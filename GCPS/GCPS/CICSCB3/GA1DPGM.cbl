000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID. GA1DPGM.                                             00000200
000300**** THIS IS A COBOL/2 PROGRAM ***                                00000300
000400 AUTHOR. T RAAK.                                                  00000400
000500 DATE-WRITTEN.   07/10/85.                                        00000500
000600 DATE-COMPILED.                                                   00000600
000700     SKIP3                                                        00000700
000800******************************************************************00000800
000900*   GA1DPGM         ALL LEVEL TABULAR MAINTENANCE PROGRAM        *00000900
001000*        DECUCTIBLE LIMITS ACCUMULATOR DESCRIPTION TABLAR - #ADL *00001000
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
003300*   FUNC CODE: GA1D                                              *00003300
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
006200*NUM-* *-DATE-* *WHO* *-----------DESCRIPTION--------------------*00006200
006300*                                                                *00006300
006400*        06/25/03 DAF USE COPYBOOK GCTIPGPC INSTEAD OF GCTIPGTC  *00006400
006500*                                                                *00006500
006600*  D368  06/04/02  JP ADD SELECTIVE ADDITIONAL BENEFIT           *00006600
006700*                         DETERMINATION (SABD)                   *00006700
006800*                                                                *00006800
006900* D365B 06/03/02  JP   ADD COMBINATION APPLIED IND (CAPI)        *00006900
007000*                                                                *00007000
007100*      11/15/01  AKK    ADD SUPPORT FOR 4 NEW BITS, TWO FOR      *00007100
007200*                       EMER AND TWO FOR SERIOUS MENTAL ILLNESS  *00007200
007300*                                                                *00007300
007400* D352 09/19/00  GDM   ADD ACCUMULATOR IDENTIFIER                *00007400
007500*                                                                *00007500
007600* P????  07/10/00 GSP   ADDED LOGIC FOR NEW #IPGS INTERNAL       *00007600
007700*                       TABULAR.                                 *00007700
007800*                                                                *00007800
007900* P????  11/19/99 FRY   ADD LENGTH PARAMETER TO THE RETURN       *00007900
008000*                       COMMAND WHEN DFHCOMMAREA IS SPECIFIED.   *00008000
008100*                                                                *00008100
008200* D341   10/07/98  GDM  HIDE TIME/DOLLAR FIELD FROM SCREEN       *00008200
008300*                                                                *00008300
008400* 14726/ 04/29/98  AB   MODIFIED TO BECOME MILLENNIUM COMPLIANT  *00008400
008500* 15057                 AND TO ADD PACKAGE CODE, PLAN CODE, AND  *00008500
008600*                       INCREASE GROUP AND SECTION NUMBERS.      *00008600
008700*                                                                *00008700
008800* 14726/ 11/10/97 DAU ADDED CODE TO SUPPORT THE YEAR 2000 AND    *00008800
008900* 15057               THE EXPANSION OF THE GROUP SPECIFIC AND    *00008900
009000*                     CONTRACT KEY TO SUPPORT THE TEXAS MERGER.  *00009000
009100*                                                                *00009100
009200*  D303  02/03/97 DAU ADD FEAK INDICATOR                         *00009200
009300*                                                                *00009300
009400* 12262  02/28/92 TPM ADD NEW COND-BIT LIF  (LIFE-THREATING)     *00009400
009500*                     COND-LIFE-THREAT-BIT                       *00009500
009600*                                                                *00009600
009700*                                                                *00009700
009800*D12009 08/28/91  TPM   INCREASED THE FAMILY-RELATION FIELD      *00009800
009900*                           FROM ONE POSITION TO TWO POSITIONS.  *00009900
010000*                                                                *00010000
010100* 11836 07/09/91  ENW  INCLUDED THE FYI FIELD IN THE COMPARE     *00010100
010200*                      AREA.                                     *00010200
010300*                                                                *00010300
010400* 11154 02/19/91  NGE  REDUCE OCCURS MAX NUM FROM 46 TO 44.      *00010400
010500*                                                                 00010500
010600* 11154 01/17/91  NGE 1. ADD AGE-QUAL-IND-FROM AND AGE-QUAL-TO   *00010600
010700*                        TO ALL ACCUM TABULARS, AS CDE FIELDS.   *00010700
010800*                       2. REMOVE RELATIONSHIP-IND FROM CDE LOGIC*00010800
010900*                                                                *00010900
011000* 11154   10/23/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *00011000
011100* D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *00011100
011200* D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *00011200
011300* D270                  4. ADD AGE-LIMIT-FROM AND AGE-LIMIT-TO.  *00011300
011400*                       5. ADD RELATIONSHIP-IND (CDE ELEMENT).   *00011400
011500*                       6. ADD NEW INTERNAL TABS #IDGD AND #IPGP.*00011500
011600*                       7. >>> CONVERT TO COBOL/2 <<<.           *00011600
011700*                                                                *00011700
011800* PG008 07/18/89 NGE  CDE MESSAGE SHOULD BE DISPLAYED WHEN ADDING*00011800
011900*                     OCCURS IN ADD/DEL MODE.                    *00011900
012000*                                                                *00012000
012100* D200 05/18/89  NGE   ADD TWO NEW COND-BITS TMJ AND INF         *00012100
012200*                      TEMPROMAND-JOINT AND INFERTILITY-COND.    *00012200
012300*                                                                *00012300
012400* ???? 03/13/89  ENW   CHANGED 'DFHBMASK' TO 'DFHBMASF' IN       *00012400
012500*                      7900-000-RESET-ATTRIBUTES SECTION BECAUSE *00012500
012600*                      FIELDS THAT WEREN'T BEING RETURNED WERE   *00012600
012700*                      CAUSING EDIT PROBLEMS.                    *00012700
012800*                                                                *00012800
012900* D201 01/12/89  ENW   ADDED LOGIC FOR BISCENDING INDICATOR.      00012900
013000*                                                                *00013000
013100*                                                                *00013100
013200* ????    09/14/88  NGE  FIX INTERNAL TABS DELETE LOGIC FOR      *00013200
013300*                        UPDATING CDE COUNTERS DEPENDING ON THE  *00013300
013400*                        INTRNL TAB RECORD NOT THE CDE STATUS    *00013400
013500*                        IN THE ACCUM RECORD ATTACHED. ALLOW     *00013500
013600*                        +CDE+ DISPLAY RETURNING FROM INTRNL PGM.*00013600
013700*                                                                *00013700
013800* ????    08/03/88  NGE  FIX ADDING ACCURS LOGIC WILL FLAG THE   *00013800
013900*                            ACCUMS AS A CDE & GAS3PGM INTERNAL  *00013900
014000*                            TAB LOGIC TO FLAG ITS ACCUM RECORD  *00014000
014100*                            WHEN THE INTERNL FLAGED CDE.        *00014100
014200*                                                                *00014200
014300*                                                                *00014300
014400*  D126      02/24/88  JLA  1. CHANGE OPTION FILE SELECTION 'S'  *00014400
014500*                              TO 'A'.                           *00014500
014600*                                                                *00014600
014700* D143 01/29/88  DES  ADD CDE/NON-CDE CHANGES USING JERRY'S      *00014700
014800*                     SCHEME WHERE THE SPLIT IS PERFORMED        *00014800
014900*                     IN BATCH AND THEN MERGED BACK ONTO W/F     *00014900
015000*                                                                *00015000
015100*  ????      09/29/87  JLA  FIX EXISTING CDE PROBLEM IN THE      *00015100
015200*                             4600- SECTION THAT CAUSED THE CDE  *00015200
015300*                             MODIFIED STATUS TO BE SET.         *00015300
015400*                                                                *00015400
015500* N126 08/28/87  JLA  ADD LOGIC FOR SUICIDE BIT.                 *00015500
015600*                                                                *00015600
015700* N121 08/09/87  NE   ADD A NEW FIELD -DEFINITION-               *00015700
015800*                                                                *00015800
015900* D200 04/24/87  JLA  BREAK INTO MULTIPLE MODULES.               *00015900
016000*                                                                *00016000
016100*D0120 03/16/87  JLA  CHANGES FOR SINGLE TABULAR SUPPORT THAT    *00016100
016200*                     ARE EXECUTED FROM TRANSACTION GTM1:        *00016200
016300*                     1. WHEN CHECKING ENTRY TRANSACTION CODE    *00016300
016400*                        TREAT GTM1 THE SAME AS GC4A.            *00016400
016500*                     2. PF1/PF13 - TREAT THE SAME AS IF GC4A    *00016500
016600*                        HAD CALLED, XCTL TO ADD SCREEN PROGRAM  *00016600
016700*                     3. PF3/PF15 - CONSTRUCT COMMAREA AS IF     *00016700
016800*                        GC4A HAD CALLED, XCTL TO GTM1PGM.       *00016800
016900*                     4. ALLOW ATTACHMENT (MAP FROM) OF PROD-    *00016900
017000*                        UCTION TABULARS, BUT PROHIBIT ATTACH-   *00017000
017100*                        ING SINGLE TABULARS UNDER SINGLE TAB-   *00017100
017200*                        ULAR SUPPORT.  DON'T CONSTRUCT C3       *00017200
017300*                        WORKFILE RECORD.  DON'T PASS CONTROL    *00017300
017400*                        TO INTERNAL TABULAR MAINTENANCE PGM.    *00017400
017500*                        DON'T CHANGE INTERNAL SLOT# ON SCREEN   *00017500
017600*                        TO ALL 9K NUMBER.                       *00017600
017700*                     5. PROHIBIT INTERNAL TAB CHANGES UNDER     *00017700
017800*                        STS.                                    *00017800
017900*                     6. PROHIBIT MAPPING FROM SKELETON UNDER    *00017900
018000*                        STS.                                    *00018000
018100*                                                                *00018100
018200*N106    02/26/87 RKH   ADDED LOGIC FOR THE FYI FIELD WHICH IS   *00018200
018300*N118                      TO BE VALIDATED & THE LOGIC TO ONLY   *00018300
018400*                          DISPLAY THE TABULAR OCCURANCE NUMBER. *00018400
018500*                                                                *00018500
018600*CDEL502 9/29/86 JLA    1. CHANGE COPY-SORTABLE-FLDS FROM X(128  *00018600
018700*                          TO X(125) AND REPLACE LAST THREE      *00018700
018800*                          BYTES WITH COPY-SORT-FYI X(3) NOT     *00018800
018900*                          INCLUDED IN TABULAR ENTRY SORT.       *00018900
019000*                       2. INITIAL THE CDE STATUS IN ANY         *00019000
019100*                          WORKFILE RECORDS CREATED TO \
019200*                       3. IF PRODUCTION TABULAR RECORD IS       *00019200
019300*                          BEING CHANGED AND CONTAINS CRITICAL   *00019300
019400*                          DATA ELEMENTS:                        *00019400
019500*                          A. INITIAL SCREEN :                   *00019500
019600*                             1) CDE STATUS(\
019700*                                 - HIGH-LIGHT CDE LABELS,       *00019700
019800*                                   SHOW +CDE+ INDICATOR.        *00019800
019900*                             2) CDE STATUS NOT (\
020000*                                 - HIGH-LIGHT CDE LABELS,       *00020000
020100*                                   HIGH-LIGHT AND PROTECT CDE   *00020100
020200*                                   ELEMENTS,                    *00020200
020300*                                   SHOW +CDE+ INDICATOR.        *00020300
020400*                          B. IF CDE ELEMENTS ARE CHANGED, SET   *00020400
020500*                             WORKFILE TABULAR CDE STATUS CODE   *00020500
020600*                             TO \
020700*                             OR GROUP SPECIFIC CONTROL          *00020700
020800*                             RECORD CDE STATUS APPROPRIATELY,   *00020800
020900*                             ISSUE CDE CHANGE MESSAGE AND       *00020900
021000*                             POSITION CURSOR ON +CDE+.  THE     *00021000
021100*                             OPERATOR THEN ADVANCES TO NEXT     *00021100
021200*                             SCREEN BY PRESSING ENTER A SECOND  *00021200
021300*                             TIME.                              *00021300
021400*                                                                *00021400
021500*CDEL501 8/22/86 JLA    DETERMINE IF POTENTIALLY CRITICAL DATA   *00021500
021600*                       ELEMENTS ARE CRITICAL BASED ON THE TRANS *00021600
021700*                       ROUTING FILE (PGM=GCTRSRT).  IF THEY     *00021700
021800*                       ARE CRITICAL AND THE USER IS DOING A     *00021800
021900*                       CHANGE TO A WORKFILE GROUP SPECIFIC      *00021900
022000*                       RECORD, PROTECT THE CRITICAL DATA ELE-   *00022000
022100*                       MENT ON THE SCREEN.                      *00022100
022200*  ?   08/12/86  AHL/DF MODIFIED 1100- ROUTINE SO THAT IT        *00022200
022300*                       FINISHES VALIDATING EACH DATA ELEMENT    *00022300
022400*                       IN THE CORRECT SEQUENCE ACCORDING TO     *00022400
022500*                       THE SCREEN LAYOUT.                       *00022500
022600*                                                                *00022600
022700*D094  08/04/86  AHL  REVISED 'NEG' LOGIC TO LET OPERATOR USE    *00022700
022800*                     EITHER 'NEG' OR DOLLARS & CENTS WITH       *00022800
022900*                     DECIMAL POINT FOR VALUE LIMIT FIELD WHEN   *00022900
023000*                     VALUE QUALIFIER = '5'.                     *00023000
023100*                                                                *00023100
023200*N112  08/01/86  AMJ  ADDED DAY FACTOR INDICATOR                 *00023200
023300*                                                                *00023300
023400*      07/30/86  AMJ  FIXED ERROR MESSAGES                       *00023400
023500*                                                                *00023500
023600*N108  07/28/86  RKH  ADDED TWO NEW CONDITION BITS               *00023600
023700*                     PRE-EXISTING CONDITIONS                    *00023700
023800*                     NON-EMERGENCY CONDITION.                   *00023800
023900*                                                                *00023900
024000*D094  07/23/86  AKM  ALLOWED 10 POSITIONS FOR VALUE LIMIT       *00024000
024100*                     FIELD SO THAT OPERATORS CAN ENTER          *00024100
024200*                     1 MILLION AS '1000000.00'.                 *00024200
024300*                                                                *00024300
024400*      06/26/86  JTC  ADDED LOGICAL EDITS                        *00024400
024500*                                                                *00024500
024600*      06/19/86  JTC  MOVED PF4/PF16 LOGIC TO AFTER VALIDATION   *00024600
024700*                     SO THAT INCORRECT RECORDS WOULD NOT BE     *00024700
024800*                     ADDED TO THE FILE.  ADDED AN INVALID       *00024800
024900*                     PF MESSAGE TO COVER THE ABOVE CASE.        *00024900
025000*                                                                *00025000
025100*                     ADDED A CHANGE TO ALLOW SCROLLING FORWARD  *00025100
025200*                     IF THE ONLY THING 'WRONG' IS AN EMPTY      *00025200
025300*                     TABLE.                                     *00025300
025400*                                                                *00025400
025500*                     FIXED THE SCREEN NOT BEING REFRESHED       *00025500
025600*                     PROPERLY FOLLOWING AN ADD WITH PF4/PF16    *00025600
025700*                                                                *00025700
025800*P495  05/30/86  AMJ  CHANGED TO ALLOW IPGT AND IPGN AT THE      *00025800
025900*                     SAME TIME                                  *00025900
026000*                                                                *00026000
026100*M106  05/30/86  AMJ  FIXED ATTRIBUTE ON ADD TO/OVERLAY FIELD    *00026100
026200*                                                                *00026200
026300*      05/14/86  MDD  CHANGED THE SEQUENCE OF THE EDITS TO BE    *00026300
026400*                      IN SYNC WITH THE SCREEN.                  *00026400
026500*                                                                *00026500
026600*      02/21/86  MDD  ADDED VALIDATION FOR FOLLOWING FIELDS:     *00026600
026700*                     'ADD-TO-OVERLAY INDICATOR',                *00026700
026800*                     'BENEFIT PERIOD',                          *00026800
026900*                     'FAMILY OR INDIVIDUAL INDICATOR',          *00026900
027000*                     'LINE OF BUSINESS',                        *00027000
027100*                     'INTERNAL DESCRIPTOR',                     *00027100
027200*                     'SERVICE GROUP',                           *00027200
027300*                     'CO-PAY INDICATOR',                        *00027300
027400*                     'COST CONTAINMENT INDICATOR',              *00027400
027500*                     'REINSTATEMENT INDICATOR',                 *00027500
027600*                     'BENEFIT PERIOD TIME QUALIFIER',           *00027600
027700*                     'BAMA BENEFIT PERIOD OVERRIDE',            *00027700
027800*                     'INTERVAL TYPE',                           *00027800
027900*                     'INTERVAL OVERRIDE INDICATOR',             *00027900
028000*                     'PLACE OF TREATMENT INDICATOR',            *00028000
028100*                     'VALUE QUALIFIER'                          *00028100
028200*                                                                *00028200
028300*      01/15/86  RKH   ADDED CODE FOR THE NEG VALUE LIMIT        *00028300
028400*                                                                *00028400
028500*      11/18/85  LET   ADDED CODE FOR THE NEW CONDITION BIT      *00028500
028600*                      NAMED ACCIDENT.                           *00028600
028700*                                                                *00028700
028800*      11/08/85  ENW   REVISED LOGIC TO ACCEPT SPACES IN THE     *00028800
028900*                      INTERNAL DESCRIPTOR FIELD INSTEAD OF      *00028900
029000*                      ZEROS.  ALSO ADDED NEW COPY MEMBER        *00029000
029100*                      'GCVALTAB'.                               *00029100
029200*                                                                *00029200
029300*            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *00029300
029400*                                                                *00029400
029500* P09400     11-07-06   GF    ADD ASCEND/DESCEND AND BISCENDING  *00029500
029600*                             INDICATORS                         *00029600
029700*                                                                *00029700
029800*            05-07-07   LR    RECOMPILE FOR CHANGES IN GASEDIT1  *00029800
029900*                                                                *00029900
029800*            10-15-10   MJL   ALLOW 'UNL' VALUE.                 *00029910
029900*                                                                *00029920
      * P21595 09/19/16   HSB CHANGES FOR GCPS NEW FIELDS BENEFIT      *00029930
      *                       TYPE CODE,TIER CODE,TIER LEVEL.          *00029940
SI0724*                                                                *00029950
SI0724* P56703  05/08/24  SI  CHANGES FOR PEAQ COPYBOOK EXPANSION      *00029960
SI0724*                       COPY ABM, ACP, ACL, ADL, AOL,            *00029970
SI0724*                       GCCDRLEN.                                *00029980
030000******************************************************************00030000
030100    SKIP3                                                         00030100
030200 ENVIRONMENT DIVISION.                                            00030200
030300/    D A T A   D I V I S I O N                                    00030300
030400 DATA DIVISION.                                                   00030400
030500 WORKING-STORAGE SECTION.                                         00030500
030600 01  WS-BEGIN                    PIC X(24)  VALUE                 00030600
030700     '***GA1DPGM WS BEGINS***'.                                   00030700
030800                                                                  00030800
030900*     T I T L E   L I N E S                                       00030900
031000 01  WS-TITLE-LINES.                                              00031000
031100 COPY GCMHLINE.                                                   00031100
031200*****05  GROUP-SPECIFIC-TITLE-LINE       PIC X(42)                00031200
031300*      VALUE ' GROUP SPEC. ALL-LEVEL TABULAR MAINTENANCE'.        00031300
031400*    05  GROUP-SPECIFIC-ID-LINE.                                  00031400
031500*      10  FILLER                        PIC X(20)                00031500
031600*        VALUE 'GROUP SPECIFIC ID= '.                             00031600
031700*      10  FILLER                        PIC X(5) VALUE 'GRP= '.  00031700
031800*      10  GRP-SPEC-GROUP-NO             PIC X(6).                00031800
031900*      10  FILLER                        PIC X(6) VALUE ' SEC= '. 00031900
032000*      10  GRP-SPEC-SECTION-NO           PIC X(4).                00032000
032100*      10  FILLER                        PIC X(5) VALUE ' FR= '.  00032100
032200*      10  GRP-SPEC-FAM-REL-LVL          PIC XX.                  00032200
032300*      10  FILLER                        PIC X(7) VALUE ' EFDT= '.00032300
032400*      10  GRP-SPEC-EFF-DATE             PIC X(6).                00032400
032500*    05  CONTRACT-TITLE-LINE             PIC X(42)                00032500
032600*      VALUE '   CONTRACT ALL-LEVEL TABULAR MAINTENANCE'.         00032600
032700*    05  CONTRACT-ID-LINE.                                        00032700
032800*      10  FILLER                      PIC X(14)                  00032800
032900*        VALUE 'CONTRACT ID= '.                                   00032900
033000*      10  FILLER                      PIC X(5) VALUE 'GRP= '.    00033000
033100*      10  CONTRACT-GROUP-NO           PIC X(6).                  00033100
033200*      10  FILLER                      PIC X(6) VALUE ' SEC= '.   00033200
033300*      10  CONTRACT-SECTION-NO         PIC X(4).                  00033300
033400*      10  FILLER                      PIC X(6) VALUE ' LOB= '.   00033400
033500*      10  CONTRACT-LOB                PIC X.                     00033500
033600*      10  FILLER                      PIC X(6) VALUE ' PRV= '.   00033600
033700*      10  CONTRACT-PROV-CTL           PIC XX.                    00033700
033800*      10  FILLER                      PIC X(5) VALUE ' FR= '.    00033800
033900*      10  CONTRACT-FAM-REL-LVL        PIC XX.                    00033900
034000*      10  FILLER                      PIC X(7) VALUE ' EFDT= '.  00034000
034100*      10  CONTRACT-EFF-DATE               PIC X(6).              00034100
034200*    05  BENEFIT-PROVISION-TITLE-LINE    PIC X(42)                00034200
034300*      VALUE '   BEN. PROV ALL-LEVEL TABULAR MAINTENANCE'.        00034300
034400*    05  BENEFIT-PROVISION-ID-LINE.                               00034400
034500*      10  FILLER                      PIC X(5) VALUE 'GRP= '.    00034500
034600*      10  BEN-PROV-GROUP-NO           PIC X(6).                  00034600
034700*      10  FILLER                      PIC X(6) VALUE ' SEC= '.   00034700
034800*      10  BEN-PROV-SECTION-NO         PIC X(4).                  00034800
034900*      10  FILLER                      PIC X(6) VALUE ' LOB= '.   00034900
035000*      10  BEN-PROV-LOB                PIC X.                     00035000
035100*      10  FILLER                      PIC X(6) VALUE ' PRV= '.   00035100
035200*      10  BEN-PROV-PROV-CTL           PIC XX.                    00035200
035300*      10  FILLER                      PIC X(5) VALUE ' FR= '.    00035300
035400*      10  BEN-PROV-FAM-REL-LVL        PIC XX.                    00035400
035500*      10  FILLER                      PIC X(7) VALUE ' EFDT= '.  00035500
035600*      10  BEN-PROV-EFF-DATE           PIC X(6).                  00035600
035700*      10  FILLER                      PIC X(8) VALUE ' BPVID= '. 00035700
035800*      10  BEN-PROV-ID-NO              PIC X(6).                  00035800
035900*    05  ADL-TITLE-LINE                PIC X(26)                  00035900
036000*********VALUE '   DEDUCTIBLE LIMITS      '.                      00036000
036100/     A L T E R N A T I V E   W O R K F I L E   K E Y S           00036100
036200 01  FILLER                      PIC X(32)  VALUE                 00036200
036300     '*** ALTERNATIVE WORKFILE KEY ***'.                          00036300
036400 01  SAVE-WS-ALT-WORKFILE-KEYS.                                   00036400
036500     05 FILLER                   PIC X(63) VALUE SPACES.          00036500
036600                                                                  00036600
036700 01  WS-ALT-WORKFILE-KEYS.                                        00036700
036800 COPY GCWRKKEY.                                                   00036800
036900                                                                  00036900
037000                                                                  00037000
037100/    D A T E   F O R M A T T I N G   A R E A                      00037100
037200 01  HGADATES-COMMAREA.                                           00037200
037300 COPY HGCDAT01.                                                   00037300
037400                                                                  00037400
037500*  *** WORKFIELDS, AND SWITCHES **                                00037500
037600 01  WS-WORK-FIELDS.                                              00037600
037700                                                                  00037700
037800     05  GCTRSRT-COMMAREA-LEN      PIC S9(4)  COMP VALUE +100.    00037800
037900                                                                  00037900
038000     05  WS-HEX-00                     PIC X    VALUE LOW-VALUE.  00038000
038100     05  WS-ONE-LOW                    PIC X VALUE LOW-VALUES.    00038100
038200     05  SAVE-COPY-FROM-SLOT           PIC 9(7).                  00038200
038300                                                                  00038300
038400     05  WS-CDE-REQUEST-CODES.                                    00038400
038500         10  WS-REQUEST-4500-CDE-PROTECT    PIC X(4) VALUE '4500'.00038500
038600         10  WS-REQUEST-4600-CDE-STATUS     PIC X(4) VALUE '4600'.00038600
038700         10  WS-REQUEST-4700-CNTL-UPDATE    PIC X(4) VALUE '4700'.00038700
038800                                                                  00038800
038900*     I N T E R N A L   T A B U L A R   P R O G R A M   N A M E   00038900
039000 01  WS-INTERNAL-TABULAR-PGM-ID        PIC X(8).                  00039000
039100                                                                  00039100
039200                                                                  00039200
039300** ***ALL LEVEL TABULAR ENTRY SAVED HERE DURING SORT ***          00039300
039400 01  WS-ENTRY                          PIC X(176).                00039400
039500     SKIP3                                                        00039500
039600/    A T T R I B U T E S                                          00039600
039700 COPY DFHBMSCA.                                                   00039700
039800     02  DFHBMABF                PIC X VALUE '9'.                 00039800
039900/    A T T E N T I O N   I D E N T I F I E R S                    00039900
040000 COPY DFHAID.                                                     00040000
040100/    R E C O R D   L E N G T H S                                  00040100
040200                                                                  00040200
040300 01  WS-RECORD-LENGTHS.                                           00040300
040400*   05 WS-COMM-KEY-PNTR-LEN           PIC S9(4) COMP  VALUE +4.   00040400
040500*   05 GAS3UPD-COMMAREA-LEN           PIC S9(4) COMP  VALUE +420. 00040500
040600*   05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP  VALUE +100. 00040600
SI0724*   05 WS-COPY-TABLE-LEN              PIC S9(4) COMP  VALUE +7744.00040700
SI0724    05 WS-COPY-TABLE-LEN              PIC S9(4) COMP VALUE +30800.00040710
040800    05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP  VALUE +0.   00040800
040900    05 WS-IO-PARM-WRK-BEN-PROV-LEN    PIC S9(4) COMP  VALUE +0.   00040900
041000    05 WS-IO-PARM-WRK-CONTRACT-LEN    PIC S9(4) COMP  VALUE +0.   00041000
041100    05 WS-IO-PARM-WRK-CONTROL-LEN     PIC S9(4) COMP  VALUE +0.   00041100
041200    05 WS-IO-PARM-WRK-GRP-SPEC-LEN    PIC S9(4) COMP  VALUE +0.   00041200
041300    05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP  VALUE +0.   00041300
041400    05 WS-WRK-BEN-PROV-LEN            PIC S9(4) COMP  VALUE +0.   00041400
041500    05 WS-WRK-CONTRACT-LEN            PIC S9(4) COMP  VALUE +0.   00041500
041600    05 WS-WRK-GRP-SPEC-LEN            PIC S9(4) COMP  VALUE +0.   00041600
041700                                                                  00041700
041800******************************************************************00041800
041900** REQUIRED FOR N118 - DISPLAY OF TABULAR OCCURS (INDEX)        **00041900
042000******************************************************************00042000
042100 01  CURNT-OCURS-BIN             PIC 9(4)  COMP.                  00042100
042200 01  CURNT-OCURS-PKD             PIC 9(4).                        00042200
042300 01  CURNT-OCURS-ALH      REDEFINES   CURNT-OCURS-PKD.            00042300
NSK24 **   05  FILLER                  PIC XX.                          00042400
NSK24 **   05  CURNT-OCCURS-OUT        PIC XX.                          00042500
NSK24      05  FILLER                  PIC X.                           00042510
NSK24      05  CURNT-OCCURS-OUT        PIC XXX.                         00042520
042600                                                                  00042600
042700 01  TOTAL-OCURS-UNK             PIC 9(5).                        00042700
042800 01  TOTAL-OCURS-ALH      REDEFINES   TOTAL-OCURS-UNK.            00042800
NSK24 **   05  FILLER                  PIC XXX.                         00042900
NSK24 **   05  TOTAL-OCCURS-OUT        PIC XX.                          00043000
NSK24      05  FILLER                  PIC XX.                          00043010
NSK24      05  TOTAL-OCCURS-OUT        PIC XXX.                         00043020
043100/    G C   R E C O R D S   L E N G T H S                          00043100
043200 01  WS-GC-RECORD-LENGTHS.                                        00043200
043300     COPY GCCDRLEN.                                               00043300
043400/    A B E N D   A R E A                                          00043400
043500                                                                  00043500
043600 01  WS-01-ABEND-AREA.                                            00043600
043700     05  FILLER                   PIC X(16)  VALUE                00043700
043800         '** ABEND AREA **'.                                      00043800
043900                                                                  00043900
044000     05  WS-ABCODE-CODES-AND-MSG.                                 00044000
044100         10  WS-ABCODE                  PIC X(04)  VALUE  SPACES. 00044100
044200         10  WS-ABCODE-MSG              PIC X(79)  VALUE  SPACES. 00044200
044300                                                                  00044300
044400         10  WS-ABCODE-1DC1             PIC X(04)  VALUE  '1DC1'. 00044400
044500         10  WS-ABCODE-1DC1-MSG         PIC X(79)  VALUE          00044500
044600             '*** INVALID PARAMETER LENGTH FOUND ***              00044600
044700-            '                           '.                       00044700
044800         10  WS-ABCODE-1DC2             PIC X(04)  VALUE  '1DC2'. 00044800
044900         10  WS-ABCODE-1DC2-MSG         PIC X(79)  VALUE          00044900
045000             '*** WRONG RECORD STATUS PASSED TO THIS PGM ***      00045000
045100-            '                           '.                       00045100
045200         10  WS-ABCODE-1DC3             PIC X(04)  VALUE  '1DC3'. 00045200
045300         10  WS-ABCODE-1DC3-MSG         PIC X(79)  VALUE          00045300
045400             '*** WRONG RECORD TYPE PASSED TO THIS PGM ***        00045400
045500-            '                           '.                       00045500
045600         10  WS-ABCODE-1DF1             PIC X(04)  VALUE  '1DF1'. 00045600
045700         10  WS-ABCODE-1DF1-MSG         PIC X(79)  VALUE          00045700
045800             '*** A SKELETON CAN NOT BE FOUND FOR AN INTERNAL TABU00045800
045900-            'LAR.  CONTACT SYSTEMS ***  '.                       00045900
046000         10  WS-ABCODE-1DF2             PIC X(04)  VALUE  '1DF2'. 00046000
046100         10  WS-ABCODE-1DF2-MSG         PIC X(79)  VALUE          00046100
046200             '*** INVALID INTERNAL TABULAR SITUATION FOUND. PLEASE00046200
046300-            ' CONTACT SYSTEMS ***       '.                       00046300
046400         10  WS-ABCODE-1DF3             PIC X(04)  VALUE  '1DF3'. 00046400
046500         10  WS-ABCODE-1DF3-MSG         PIC X(79)  VALUE          00046500
046600             '*** INVALID INTERNAL TABULAR SITUATION FOUND. PLEASE00046600
046700-            ' CONTACT SYSTEMS ***       '.                       00046700
046800         10  WS-ABCODE-1DF4             PIC X(04)  VALUE  '1DF4'. 00046800
046900         10  WS-ABCODE-1DF4-MSG         PIC X(79)  VALUE          00046900
047000             '*** ERROR REWRITING ALL LEVEL TABULAR.  PLEASE CONTA00047000
047100-            'CT SYSTEMS ***             '.                       00047100
047200         10  WS-ABCODE-1DF5             PIC X(04)  VALUE  '1DF5'. 00047200
047300         10  WS-ABCODE-1DF5-MSG         PIC X(79)  VALUE          00047300
047400             'THE INTERNAL TABULAR CAN NOT BE READ FROM THE WORKFI00047400
047500-            'LE.  PLEASE CONTACT SYSTEMS'.                       00047500
047600         10  WS-ABCODE-1DF6             PIC X(04)  VALUE  '1DF6'. 00047600
047700         10  WS-ABCODE-1DF6-MSG         PIC X(79)  VALUE          00047700
047800             'THE INTERNAL TABULAR CAN NOT BE ADDED TO THE WORKFIL00047800
047900-            'E.  PLEASE CONTACT SYSTEMS '.                       00047900
048000         10  WS-ABCODE-1DF7             PIC X(04)  VALUE  '1DF7'. 00048000
048100         10  WS-ABCODE-1DF7-MSG         PIC X(79)  VALUE          00048100
048200             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00048200
048300-            ' SYSTEMS ***               '.                       00048300
048400         10  WS-ABCODE-1DF9             PIC X(04)  VALUE  '1DF9'. 00048400
048500         10  WS-ABCODE-1DF9-MSG         PIC X(79)  VALUE          00048500
048600             'THE INTERNAL TABULAR CAN NOT BE ADDED TO THE WORFILE00048600
048700-            '.  PLEASE CONTACT SYSTEMS  '.                       00048700
048800         10  WS-ABCODE-1DFA             PIC X(04)  VALUE  '1DFA'. 00048800
048900         10  WS-ABCODE-1DFA-MSG         PIC X(79)  VALUE          00048900
049000             '*** THE INTERNAL TABULAR CAN NOT BE DELETED, PLEASE 00049000
049100-            'CONTACT SYSTEMS ***        '.                       00049100
049200         10  WS-ABCODE-1DFB             PIC X(04)  VALUE  '1DFB'. 00049200
049300         10  WS-ABCODE-1DFB-MSG         PIC X(79)  VALUE          00049300
049400             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00049400
049500-            ' SYSTEMS ***               '.                       00049500
049600         10  WS-ABCODE-1DFC             PIC X(04)  VALUE  '1DFC'. 00049600
049700         10  WS-ABCODE-1DFC-MSG         PIC X(79)  VALUE          00049700
049800             '*** ERROR WHEN DELETING INTERNAL TAB.  PLEASE CONTAC00049800
049900-            'T SYSTEMS ***              '.                       00049900
050000         10  WS-ABCODE-1DFJ             PIC X(04)  VALUE  '1DFJ'. 00050000
050100         10  WS-ABCODE-1DFJ-MSG         PIC X(79)  VALUE          00050100
050200             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00050200
050300-            ' SYSTEMS ***               '.                       00050300
050400         10  WS-ABCODE-1DFK             PIC X(04)  VALUE  '1DFK'. 00050400
050500         10  WS-ABCODE-1DFK-MSG         PIC X(79)  VALUE          00050500
050600             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00050600
050700-            ' SYSTEMS ***               '.                       00050700
050800         10  WS-ABCODE-1DFL             PIC X(04)  VALUE  '1DFL'. 00050800
050900         10  WS-ABCODE-1DFL-MSG         PIC X(79)  VALUE          00050900
051000             '*** ERROR READING GROUP SPECIFIC RECORD TO RETURN TO00051000
051100-            'MENU.  CONTACT SYSTEMS *** '.                       00051100
051200         10  WS-ABCODE-1DFM             PIC X(04)  VALUE  '1DFM'. 00051200
051300         10  WS-ABCODE-1DFM-MSG         PIC X(79)  VALUE          00051300
051400             '*** ERROR READING CONTRACT MASTER TO RETURN TO THE  00051400
051500-            'MENU.  CONTACT SYSTEMS *** '.                       00051500
051600         10  WS-ABCODE-1DFN             PIC X(04)  VALUE  '1DFN'. 00051600
051700         10  WS-ABCODE-1DFN-MSG         PIC X(79)  VALUE          00051700
051800             '*** ERROR READING BENEFIT PROV RECORD TO RETURN TO M00051800
051900-            'ENU.  CONTACT SYSTEMS ***  '.                       00051900
052000         10  WS-ABCODE-1DFO             PIC X(04)  VALUE  '1DFO'. 00052000
052100         10  WS-ABCODE-1DFO-MSG         PIC X(79)  VALUE          00052100
052200             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00052200
052300-            ' SYSTEMS ***               '.                       00052300
052400         10  WS-ABCODE-1DFP             PIC X(04)  VALUE  '1DFP'. 00052400
052500         10  WS-ABCODE-1DFP-MSG         PIC X(79)  VALUE          00052500
052600             '*** ERROR READING W/F CONTROL RECORD. PLEASE CONTACT00052600
052700-            ' SYSTEMS ***               '.                       00052700
052800         10  WS-ABCODE-1DFQ             PIC X(04)  VALUE  '1DFQ'. 00052800
052900         10  WS-ABCODE-1DFQ-MSG         PIC X(79)  VALUE          00052900
053000             '*** ERROR REWRITING W/F CONTROL RECORD. PLEASE CONTA00053000
053100-            'CT SYSTEMS ***             '.                       00053100
053200         10  WS-ABCODE-1DFR             PIC X(04)  VALUE  '1DFR'. 00053200
053300         10  WS-ABCODE-1DFR-MSG         PIC X(79)  VALUE          00053300
053400             '*** ERROR READING W/F ALL LVL TAB.    PLEASE CONTACT00053400
053500-            ' SYSTEMS ***               '.                       00053500
053600         10  WS-ABCODE-1DFS             PIC X(04)  VALUE  '1DFS'. 00053600
053700         10  WS-ABCODE-1DFS-MSG         PIC X(79)  VALUE          00053700
053800             '*** ERROR REWRITING W/F ALL LVL TAB.  PLEASE CONTACT00053800
053900-            ' SYSTEMS ***               '.                       00053900
054000         10  WS-ABCODE-1DFT             PIC X(04)  VALUE  '1DFT'. 00054000
054100         10  WS-ABCODE-1DFT-MSG         PIC X(79)  VALUE          00054100
054200             '*** ERROR READING W/F CONTROL RECORD. PLEASE CONTACT00054200
054300-            ' SYSTEMS ***               '.                       00054300
054400         10  WS-ABCODE-1DFU             PIC X(04)  VALUE  '1DFU'. 00054400
054500         10  WS-ABCODE-1DFU-MSG         PIC X(79)  VALUE          00054500
054600             '*** ERROR REWRITING W/F CONTROL RECORD. PLEASE CONTA00054600
054700-            'CT SYSTEMS ***             '.                       00054700
054800         10  WS-ABCODE-1DL1             PIC X(04)  VALUE  '1DL1'. 00054800
054900         10  WS-ABCODE-1DL1-MSG         PIC X(79)  VALUE          00054900
055000             '*** THE OCCURS WE ARE TO UPDATE HAS BEEN DELETED ***00055000
055100-            '                           '.                       00055100
055200         10  WS-ABCODE-1DLX             PIC X(04)  VALUE  '1DLX'. 00055200
055300         10  WS-ABCODE-1DLX-MSG         PIC X(79)  VALUE          00055300
055400             '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTE00055400
055500-            'MS ***                     '.                       00055500
055600         10  WS-ABCODE-1DP1             PIC X(04)  VALUE  '1DP1'. 00055600
055700         10  WS-ABCODE-1DP1-MSG         PIC X(79)  VALUE          00055700
055800             '????????????????????????????????????????????????????00055800
055900-            '???????????????????????????'.                       00055900
056000                                                                  00056000
056100/    M E S S A G E   T A B L E                                    00056100
056200******************************************************************00056200
056300 01  WT-01-TABLE.                                                 00056300
056400     05  FILLER                  PIC X(16) VALUE                  00056400
056500         '* WT-01-TABLE  *'.                                      00056500
056600                                                                  00056600
056700 01  FILLER.                                                      00056700
056800     05  WT-01-MESSAGE-VALUES.                                    00056800
056900*----------------------------------------------------------------*00056900
057000         10  WT-01-ENTRY-001.                                     00057000
057100             15  FILLER              PIC X(2)  VALUE '¬>'.        00057100
057200             15  WT-01-MESSAGE-TEXT-001.                          00057200
057300                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00057300
057400                 20  FILLER          PIC X(1)  VALUE  '-'.        00057400
057500                 20  FILLER          PIC X(3)  VALUE  '001'.      00057500
057600                 20  FILLER          PIC X(1)  VALUE  ' '.        00057600
057700                 20  FILLER          PIC X(70) VALUE              00057700
057800                     '#IBGR HAS BEEN SUCCESSFULLY MAPPED          00057800
057900-                    '                         '.                 00057900
058000             15  FILLER              PIC X(2)  VALUE '<¬'.        00058000
058100*----------------------------------------------------------------*00058100
058200         10  WT-01-ENTRY-002.                                     00058200
058300             15  FILLER              PIC X(2)  VALUE '¬>'.        00058300
058400             15  WT-01-MESSAGE-TEXT-002.                          00058400
058500                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00058500
058600                 20  FILLER          PIC X(1)  VALUE  '-'.        00058600
058700                 20  FILLER          PIC X(3)  VALUE  '002'.      00058700
058800                 20  FILLER          PIC X(1)  VALUE  ' '.        00058800
058900                 20  FILLER          PIC X(70) VALUE              00058900
059000                     '#IPGN HAS BEEN SUCCESSFULLY MAPPED          00059000
059100-                    '                         '.                 00059100
059200             15  FILLER              PIC X(2)  VALUE '<¬'.        00059200
059300*----------------------------------------------------------------*00059300
059400         10  WT-01-ENTRY-003.                                     00059400
059500             15  FILLER              PIC X(2)  VALUE '¬>'.        00059500
059600             15  WT-01-MESSAGE-TEXT-003.                          00059600
059700                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00059700
059800                 20  FILLER          PIC X(1)  VALUE  '-'.        00059800
059900                 20  FILLER          PIC X(3)  VALUE  '003'.      00059900
060000                 20  FILLER          PIC X(1)  VALUE  ' '.        00060000
060100                 20  FILLER          PIC X(70) VALUE              00060100
060200                     '#IPGT HAS BEEN SUCCESSFULLY MAPPED          00060200
060300-                    '                         '.                 00060300
060400             15  FILLER              PIC X(2)  VALUE '<¬'.        00060400
060500*----------------------------------------------------------------*00060500
060600         10  WT-01-ENTRY-004.                                     00060600
060700             15  FILLER              PIC X(2)  VALUE '¬>'.        00060700
060800             15  WT-01-MESSAGE-TEXT-004.                          00060800
060900                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00060900
061000                 20  FILLER          PIC X(1)  VALUE  '-'.        00061000
061100                 20  FILLER          PIC X(3)  VALUE  '004'.      00061100
061200                 20  FILLER          PIC X(1)  VALUE  ' '.        00061200
061300                 20  FILLER          PIC X(70) VALUE              00061300
061400                     '#IDGD HAS BEEN SUCCESSFULLY MAPPED          00061400
061500-                    '                         '.                 00061500
061600             15  FILLER              PIC X(2)  VALUE '<¬'.        00061600
061700*----------------------------------------------------------------*00061700
061800         10  WT-01-ENTRY-005.                                     00061800
061900             15  FILLER              PIC X(2)  VALUE '¬>'.        00061900
062000             15  WT-01-MESSAGE-TEXT-005.                          00062000
062100                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00062100
062200                 20  FILLER          PIC X(1)  VALUE  '-'.        00062200
062300                 20  FILLER          PIC X(3)  VALUE  '005'.      00062300
062400                 20  FILLER          PIC X(1)  VALUE  ' '.        00062400
062500                 20  FILLER          PIC X(70) VALUE              00062500
062600                     '#IPGP HAS BEEN SUCCESSFULLY MAPPED          00062600
062700-                    '                         '.                 00062700
062800             15  FILLER              PIC X(2)  VALUE '<¬'.        00062800
062900*----------------------------------------------------------------*00062900
063000         10  WT-01-ENTRY-006.                                     00063000
063100             15  FILLER              PIC X(2)  VALUE '¬>'.        00063100
063200             15  WT-01-MESSAGE-TEXT-006.                          00063200
063300                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00063300
063400                 20  FILLER          PIC X(1)  VALUE  '-'.        00063400
063500                 20  FILLER          PIC X(3)  VALUE  '006'.      00063500
063600                 20  FILLER          PIC X(1)  VALUE  ' '.        00063600
063700                 20  FILLER          PIC X(70) VALUE              00063700
063800                     'DELETE OPTION MUST BE \
063900-                    'VALID                    '.                 00063900
064000             15  FILLER              PIC X(2)  VALUE '<¬'.        00064000
064100*----------------------------------------------------------------*00064100
064200         10  WT-01-ENTRY-007.                                     00064200
064300             15  FILLER              PIC X(2)  VALUE '¬>'.        00064300
064400             15  WT-01-MESSAGE-TEXT-007.                          00064400
064500                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00064500
064600                 20  FILLER          PIC X(1)  VALUE  '-'.        00064600
064700                 20  FILLER          PIC X(3)  VALUE  '007'.      00064700
064800                 20  FILLER          PIC X(1)  VALUE  ' '.        00064800
064900                 20  FILLER          PIC X(70) VALUE              00064900
065000                     'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESS00065000
065100-                    ' PF4/PF16 TO CONTINUE    '.                 00065100
065200             15  FILLER              PIC X(2)  VALUE '<¬'.        00065200
065300*----------------------------------------------------------------*00065300
065400         10  WT-01-ENTRY-008.                                     00065400
065500             15  FILLER              PIC X(2)  VALUE '¬>'.        00065500
065600             15  WT-01-MESSAGE-TEXT-008.                          00065600
065700                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00065700
065800                 20  FILLER          PIC X(1)  VALUE  '-'.        00065800
065900                 20  FILLER          PIC X(3)  VALUE  '008'.      00065900
066000                 20  FILLER          PIC X(1)  VALUE  ' '.        00066000
066100                 20  FILLER          PIC X(70) VALUE              00066100
066200                     'GROUP IN CONVERSION STATUS, CANNOT CHANGE HI00066200
066300-                    'GH-LIGHTED ELEMENTS      '.                 00066300
066400             15  FILLER              PIC X(2)  VALUE '<¬'.        00066400
066500*----------------------------------------------------------------*00066500
066600         10  WT-01-ENTRY-009.                                     00066600
066700             15  FILLER              PIC X(2)  VALUE '¬>'.        00066700
066800             15  WT-01-MESSAGE-TEXT-009.                          00066800
066900                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00066900
067000                 20  FILLER          PIC X(1)  VALUE  '-'.        00067000
067100                 20  FILLER          PIC X(3)  VALUE  '009'.      00067100
067200                 20  FILLER          PIC X(1)  VALUE  ' '.        00067200
067300                 20  FILLER          PIC X(70) VALUE              00067300
067400                     'INVALID PFKEY SELECTION                     00067400
067500-                    '                         '.                 00067500
067600             15  FILLER              PIC X(2)  VALUE '<¬'.        00067600
067700*----------------------------------------------------------------*00067700
067800         10  WT-01-ENTRY-010.                                     00067800
067900             15  FILLER              PIC X(2)  VALUE '¬>'.        00067900
068000             15  WT-01-MESSAGE-TEXT-010.                          00068000
068100                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00068100
068200                 20  FILLER          PIC X(1)  VALUE  '-'.        00068200
068300                 20  FILLER          PIC X(3)  VALUE  '010'.      00068300
068400                 20  FILLER          PIC X(1)  VALUE  ' '.        00068400
068500                 20  FILLER          PIC X(70) VALUE              00068500
068600                     'INVALID REQUEST.  THAT PF KEY HAS NO MEANING00068600
068700-                    ' TO THIS PROGRAM         '.                 00068700
068800             15  FILLER              PIC X(2)  VALUE '<¬'.        00068800
068900*----------------------------------------------------------------*00068900
069000         10  WT-01-ENTRY-011.                                     00069000
069100             15  FILLER              PIC X(2)  VALUE '¬>'.        00069100
069200             15  WT-01-MESSAGE-TEXT-011.                          00069200
069300                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00069300
069400                 20  FILLER          PIC X(1)  VALUE  '-'.        00069400
069500                 20  FILLER          PIC X(3)  VALUE  '011'.      00069500
069600                 20  FILLER          PIC X(1)  VALUE  ' '.        00069600
069700                 20  FILLER          PIC X(70) VALUE              00069700
069800                     'NO CHANGE FOUND - NO CHANGE MADE, WHAT NEXT 00069800
069900-                    '                         '.                 00069900
070000             15  FILLER              PIC X(2)  VALUE '<¬'.        00070000
070100*----------------------------------------------------------------*00070100
070200         10  WT-01-ENTRY-012.                                     00070200
070300             15  FILLER              PIC X(2)  VALUE '¬>'.        00070300
070400             15  WT-01-MESSAGE-TEXT-012.                          00070400
070500                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00070500
070600                 20  FILLER          PIC X(1)  VALUE  '-'.        00070600
070700                 20  FILLER          PIC X(3)  VALUE  '012'.      00070700
070800                 20  FILLER          PIC X(1)  VALUE  ' '.        00070800
070900                 20  FILLER          PIC X(70) VALUE              00070900
071000                     'NO ENTRIES TO DISPLAY                       00071000
071100-                    '                         '.                 00071100
071200             15  FILLER              PIC X(2)  VALUE '<¬'.        00071200
071300*----------------------------------------------------------------*00071300
071400         10  WT-01-ENTRY-013.                                     00071400
071500             15  FILLER              PIC X(2)  VALUE '¬>'.        00071500
071600             15  WT-01-MESSAGE-TEXT-013.                          00071600
071700                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00071700
071800                 20  FILLER          PIC X(1)  VALUE  '-'.        00071800
071900                 20  FILLER          PIC X(3)  VALUE  '013'.      00071900
072000                 20  FILLER          PIC X(1)  VALUE  ' '.        00072000
072100                 20  FILLER          PIC X(70) VALUE              00072100
072200                     'PFKEY INVALID WHILE ERRORS NOT CORRECTED, HI00072200
072300-                    'T ENTER FOR ERR MSG      '.                 00072300
072400             15  FILLER              PIC X(2)  VALUE '<¬'.        00072400
072500*----------------------------------------------------------------*00072500
072600         10  WT-01-ENTRY-014.                                     00072600
072700             15  FILLER              PIC X(2)  VALUE '¬>'.        00072700
072800             15  WT-01-MESSAGE-TEXT-014.                          00072800
072900                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00072900
073000                 20  FILLER          PIC X(1)  VALUE  '-'.        00073000
073100                 20  FILLER          PIC X(3)  VALUE  '014'.      00073100
073200                 20  FILLER          PIC X(1)  VALUE  ' '.        00073200
073300                 20  FILLER          PIC X(70) VALUE              00073300
073400                     'PROCESSING FROM THE TOP OF THE LIST         00073400
073500-                    '                         '.                 00073500
073600             15  FILLER              PIC X(2)  VALUE '<¬'.        00073600
073700*----------------------------------------------------------------*00073700
073800         10  WT-01-ENTRY-015.                                     00073800
073900             15  FILLER              PIC X(2)  VALUE '¬>'.        00073900
074000             15  WT-01-MESSAGE-TEXT-015.                          00074000
074100                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00074100
074200                 20  FILLER          PIC X(1)  VALUE  '-'.        00074200
074300                 20  FILLER          PIC X(3)  VALUE  '015'.      00074300
074400                 20  FILLER          PIC X(1)  VALUE  ' '.        00074400
074500                 20  FILLER          PIC X(70) VALUE              00074500
074600                     'THE MAXIMUM NUMBER OF ENTRIES HAVE BEEN ADDE00074600
074700-                    'D                        '.                 00074700
074800             15  FILLER              PIC X(2)  VALUE '<¬'.        00074800
074900*----------------------------------------------------------------*00074900
075000         10  WT-01-ENTRY-016.                                     00075000
075100             15  FILLER              PIC X(2)  VALUE '¬>'.        00075100
075200             15  WT-01-MESSAGE-TEXT-016.                          00075200
075300                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00075300
075400                 20  FILLER          PIC X(1)  VALUE  '-'.        00075400
075500                 20  FILLER          PIC X(3)  VALUE  '016'.      00075500
075600                 20  FILLER          PIC X(1)  VALUE  ' '.        00075600
075700                 20  FILLER          PIC X(70) VALUE              00075700
075800                     'THE TABULAR ALREADY CONTAINS THE MAXIMUM NUM00075800
075900-                    'BER OF OCCURANCES        '.                 00075900
076000             15  FILLER              PIC X(2)  VALUE '<¬'.        00076000
076100*----------------------------------------------------------------*00076100
076200         10  WT-01-ENTRY-017.                                     00076200
076300             15  FILLER              PIC X(2)  VALUE '¬>'.        00076300
076400             15  WT-01-MESSAGE-TEXT-017.                          00076400
076500                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00076500
076600                 20  FILLER          PIC X(1)  VALUE  '-'.        00076600
076700                 20  FILLER          PIC X(3)  VALUE  '017'.      00076700
076800                 20  FILLER          PIC X(1)  VALUE  ' '.        00076800
076900                 20  FILLER          PIC X(70) VALUE              00076900
077000                     'THE TABULAR RECORD DOES NOT EXIST, AND CANNO00077000
077100-                    'T BE CHANGED             '.                 00077100
077200             15  FILLER              PIC X(2)  VALUE '<¬'.        00077200
077300*----------------------------------------------------------------*00077300
077400         10  WT-01-ENTRY-018.                                     00077400
077500             15  FILLER              PIC X(2)  VALUE '¬>'.        00077500
077600             15  WT-01-MESSAGE-TEXT-018.                          00077600
077700                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00077700
077800                 20  FILLER          PIC X(1)  VALUE  '-'.        00077800
077900                 20  FILLER          PIC X(3)  VALUE  '018'.      00077900
078000                 20  FILLER          PIC X(1)  VALUE  ' '.        00078000
078100                 20  FILLER          PIC X(70) VALUE              00078100
078200                     'THE TABULAR RECORD DOES NOT EXIST, AND CANNO00078200
078300-                    'T BE MAPPED              '.                 00078300
078400             15  FILLER              PIC X(2)  VALUE '<¬'.        00078400
078500*----------------------------------------------------------------*00078500
078600         10  WT-01-ENTRY-019.                                     00078600
078700             15  FILLER              PIC X(2)  VALUE '¬>'.        00078700
078800             15  WT-01-MESSAGE-TEXT-019.                          00078800
078900                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00078900
079000                 20  FILLER          PIC X(1)  VALUE  '-'.        00079000
079100                 20  FILLER          PIC X(3)  VALUE  '019'.      00079100
079200                 20  FILLER          PIC X(1)  VALUE  ' '.        00079200
079300                 20  FILLER          PIC X(70) VALUE              00079300
079400                     'THERE ARE NO MORE ENTRIES TO DISPLAY        00079400
079500-                    '                         '.                 00079500
079600             15  FILLER              PIC X(2)  VALUE '<¬'.        00079600
079700*----------------------------------------------------------------*00079700
079800         10  WT-01-ENTRY-020.                                     00079800
079900             15  FILLER              PIC X(2)  VALUE '¬>'.        00079900
080000             15  WT-01-MESSAGE-TEXT-020.                          00080000
080100                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00080100
080200                 20  FILLER          PIC X(1)  VALUE  '-'.        00080200
080300                 20  FILLER          PIC X(3)  VALUE  '020'.      00080300
080400                 20  FILLER          PIC X(1)  VALUE  ' '.        00080400
080500                 20  FILLER          PIC X(70) VALUE              00080500
080600                     'THIS IS THE FIRST ON THE TABLE              00080600
080700-                    '                         '.                 00080700
080800             15  FILLER              PIC X(2)  VALUE '<¬'.        00080800
080900*----------------------------------------------------------------*00080900
081000         10  WT-01-ENTRY-021.                                     00081000
081100             15  FILLER              PIC X(2)  VALUE '¬>'.        00081100
081200             15  WT-01-MESSAGE-TEXT-021.                          00081200
081300                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00081300
081400                 20  FILLER          PIC X(1)  VALUE  '-'.        00081400
081500                 20  FILLER          PIC X(3)  VALUE  '021'.      00081500
081600                 20  FILLER          PIC X(1)  VALUE  ' '.        00081600
081700                 20  FILLER          PIC X(70) VALUE              00081700
081800                     'THIS IS THE LAST ON THE TABLE               00081800
081900-                    '                         '.                 00081900
082000             15  FILLER              PIC X(2)  VALUE '<¬'.        00082000
082100*----------------------------------------------------------------*00082100
082200         10  WT-01-ENTRY-022.                                     00082200
082300             15  FILLER              PIC X(2)  VALUE '¬>'.        00082300
082400             15  WT-01-MESSAGE-TEXT-022.                          00082400
082500                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00082500
082600                 20  FILLER          PIC X(1)  VALUE  '-'.        00082600
082700                 20  FILLER          PIC X(3)  VALUE  '022'.      00082700
082800                 20  FILLER          PIC X(1)  VALUE  ' '.        00082800
082900                 20  FILLER          PIC X(70) VALUE              00082900
083000                     'THIS PFKEY NOT VALID WHILE IN CHG/ADD MODE  00083000
083100-                    '                         '.                 00083100
083200             15  FILLER              PIC X(2)  VALUE '<¬'.        00083200
083300*----------------------------------------------------------------*00083300
083400         10  WT-01-ENTRY-023.                                     00083400
083500             15  FILLER              PIC X(2)  VALUE '¬>'.        00083500
083600             15  WT-01-MESSAGE-TEXT-003.                          00083600
083700                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00083700
083800                 20  FILLER          PIC X(1)  VALUE  '-'.        00083800
083900                 20  FILLER          PIC X(3)  VALUE  '023'.      00083900
084000                 20  FILLER          PIC X(1)  VALUE  ' '.        00084000
084100                 20  FILLER          PIC X(70) VALUE              00084100
084200                     '#IPGS HAS BEEN SUCCESSFULLY MAPPED          00084200
084300-                    '                         '.                 00084300
084400             15  FILLER              PIC X(2)  VALUE '<¬'.        00084400
084500*----------------------------------------------------------------*00084500
084600         10  WT-01-ENTRY-024.                                     00084600
084700             15  FILLER              PIC X(2)  VALUE '¬>'.        00084700
084800             15  WT-01-MESSAGE-TEXT-023.                          00084800
084900                 20  FILLER          PIC X(4)  VALUE  'GA1D'.     00084900
085000                 20  FILLER          PIC X(1)  VALUE  '-'.        00085000
085100                 20  FILLER          PIC X(3)  VALUE  '024'.      00085100
085200                 20  FILLER          PIC X(1)  VALUE  ' '.        00085200
085300                 20  FILLER          PIC X(70) VALUE              00085300
085400                     '********** F U T U R E   U S E *************00085400
085500-                    '*************************'.                 00085500
085600             15  FILLER              PIC X(2)  VALUE '<¬'.        00085600
085700*----------------------------------------------------------------*00085700
085800                                                                  00085800
085900     05  WT-01-MESSAGE-TABLE         REDEFINES                    00085900
086000         WT-01-MESSAGE-VALUES         OCCURS 024 TIMES            00086000
086100                                     INDEXED BY WT-01-INDEX.      00086100
086200         10  WT-01-ENTRY.                                         00086200
086300             15  FILLER              PIC X(02).                   00086300
086400             15  WT-01-MESSAGE-TEXT  PIC X(79).                   00086400
086500             15  FILLER              PIC X(02).                   00086500
086600                                                                  00086600
086700 01  WS-END                      PIC X(16)  VALUE                 00086700
086800     '*** W/S ENDS ***'.                                          00086800
086900/    L I N K A G E   S E C T I O N                                00086900
087000 LINKAGE SECTION.                                                 00087000
087100 01  DFHCOMMAREA.                                                 00087100
087200 COPY G2ALCKEC.                                                   00087200
087300*    05  COMMAREA-RECORD-POINTER  USAGE IS POINTER.               00087300
087400 COPY GACDACWB.                                                   00087400
087500                                                                  00087500
087600     05  GAS3UPD-PASSED-AREA-2.                                   00087600
087700         07  LVL2-B-SW-2             PIC X.                       00087700
087800         07  LVL2-F-SW-2             PIC X.                       00087800
087900         07  LVL2-G-SW-2             PIC X.                       00087900
088000         07  INTR-TAB-PGM-ID-2       PIC X(8).                    00088000
088100         07  FILLER-2                PIC X(09).                   00088100
088200     05  DELADD-OPTION-2             PIC X(7).                    00088200
088300                                                                  00088300
088400/*****************************************************************00088400
088500* W O R K F I L E   -   A L L   L E V E L   T A B   R E C O R D   00088500
088600******************************************************************00088600
088700 01  WF-IO-PARM-ALL-LVL-TAB-RECORD.                               00088700
088800 COPY GCIOPRM1.                                                   00088800
088900/                                                                 00088900
089000 COPY GCWRKDCC.                                                   00089000
089100/                                                                 00089100
089200 COPY GCTADLC.                                                    00089200
089300/    C O M M U N I C A T I O N    K E Y   A R E A                 00089300
089400*01  COMMUNICATION-KEY-AREA.                                      00089400
089500*COPY G2ALCKEC.                                                   00089500
089600                                                                  00089600
089700/    C O P Y   T A B U L A R   T A B L E   A R E A                00089700
089800 01  COPY-TABULAR-TABLE-AREA.                                     00089800
SI0724*    05  COPY-TABULAR-TABLE  OCCURS  44 TIMES INDEXED BY          00089900
SI0724     05  COPY-TABULAR-TABLE  OCCURS 175 TIMES INDEXED BY          00089910
090000         COPY-IDX, COPY-IDX2, COPY-IDX3, COPY-IDX4.               00090000
090100       10  COPY-SORTABLE-FLDS.                                    00090100
090200           15  FILLER                      PIC X(169).            00090200
090300           15  COPY-SORT-FYI               PIC X(003).            00090300
090400       10  COPY-SORT-ENTRY-CNTR            PIC S9(7) COMP-3.      00090400
090500                                                                  00090500
090600/*****************************************************************00090600
090700* W O R K F I L E   -   I N T E R N A L   T A B U L A R   R E C   00090700
090800******************************************************************00090800
090900 01  WF-IO-PARM-INTERNAL-TAB-RECORD.                              00090900
091000 COPY GCIOPRM2.                                                   00091000
091100/                                                                 00091100
091200 COPY GCWRKDC2.                                                   00091200
091300/                                                                 00091300
091400 COPY GCTIPGPC.                                                   00091400
091500/*****************************************************************00091500
091600* W O R K F I L E   -   G R O U P   S P E C I F I C   R E C       00091600
091700******************************************************************00091700
091800 01  WF-IO-PARM-WRK-GRP-SPEC-REC.                                 00091800
091900 COPY GCIOPRM3.                                                   00091900
092000/                                                                 00092000
092100 COPY GCWRKDC3.                                                   00092100
092200/                                                                 00092200
092300 COPY GCGROUPC.                                                   00092300
092400/*****************************************************************00092400
092500* W O R K F I L E   -   C O N T R A C T   R E C O R D             00092500
092600******************************************************************00092600
092700 01  WF-IO-PARM-WRK-CONTRACT-REC.                                 00092700
092800 COPY GCIOPRM4.                                                   00092800
092900/                                                                 00092900
093000 COPY GCWRKDC4.                                                   00093000
093100/                                                                 00093100
093200 COPY GCCONTRC.                                                   00093200
093300/*****************************************************************00093300
093400* W O R K F I L E   -   B E N E F I T   P R O V I S I O N   R E C 00093400
093500******************************************************************00093500
093600 01  WF-IO-PARM-WRK-BEN-PROV-REC.                                 00093600
093700 COPY GCIOPRM5.                                                   00093700
093800/                                                                 00093800
093900 COPY GCWRKDC5.                                                   00093900
094000/                                                                 00094000
094100 COPY GCBENPVC.                                                   00094100
094200/*****************************************************************00094200
094300* W O R K F I L E   -  C O N T R O L   R E C O R D                00094300
094400******************************************************************00094400
094500 01  WF-IO-PARM-WRK-CONTROL-REC.                                  00094500
094600 COPY GCIOPRM6.                                                   00094600
094700/                                                                 00094700
094800 COPY GCWRKDC6.                                                   00094800
094900/                                                                 00094900
095000 COPY GCCCRDCC.                                                   00095000
095100/*****************************************************************00095100
095200* P R O D U C T I O N   -   C O N T R A C T   R E C O R D         00095200
095300******************************************************************00095300
095400 01  PR-IO-PARM-WRK-CONTRACT-REC.                                 00095400
095500 COPY GCIOPRM7   SUPPRESS.                                        00095500
095600                                                                  00095600
095700 COPY GCWRKDC7   SUPPRESS.                                        00095700
095800                                                                  00095800
095900 COPY GCCONTR2   SUPPRESS.                                        00095900
096000******************************************************************00096000
096100* P R O D U C T I O N   -   G R O U P   S P E C I F I C   R E C   00096100
096200******************************************************************00096200
096300 01  PR-IO-PARM-WRK-GRP-SPEC-REC.                                 00096300
096400 COPY GCIOPRM8   SUPPRESS.                                        00096400
096500                                                                  00096500
096600 COPY GCWRKDC8   SUPPRESS.                                        00096600
096700                                                                  00096700
096800 COPY GCGROUP2   SUPPRESS.                                        00096800
096900******************************************************************00096900
097000* P R O D U C T I O N   -   B E N E F I T   P V S N   R E C O R D 00097000
097100******************************************************************00097100
097200 01  PR-IO-PARM-WRK-BEN-PROV-REC.                                 00097200
097300 COPY GCIOPRM9   SUPPRESS.                                        00097300
097400                                                                  00097400
097500 COPY GCWRKDC9   SUPPRESS.                                        00097500
097600                                                                  00097600
097700 COPY GCBENPV2   SUPPRESS.                                        00097700
097800******************************************************************00097800
097900* P R O D U C T I O N   -   A L L   L E V E L   T A B   R E C     00097900
098000******************************************************************00098000
098100 01  PR-IO-PARM-ALL-LVL-TAB-RECORD.                               00098100
098200 COPY GCIOPRMA   SUPPRESS.                                        00098200
098300                                                                  00098300
098400 COPY GCWRKDCA   SUPPRESS.                                        00098400
098500                                                                  00098500
098600 COPY GCTADL2    SUPPRESS.                                        00098600
098700                                                                  00098700
098800/*****************************************************************00098800
098900*    M A P S E T   A R E A                                        00098900
099000******************************************************************00099000
099100     COPY GA1XSETC.                                               00099100
099200                                                                  00099200
099300/*****************************************************************00099300
099400*     A L L   L V L   A C C U M   C O M M O N   W O R K A R E A S 00099400
099500******************************************************************00099500
099600*  *** UPDATE/DELETE MODULE GAS3UPD COMMAREA ***                  00099600
099700*  *** WILL BE THE SAME COMMON WORK AREA + GAS3UPD COMMAREA ***   00099700
099800                                                                  00099800
099900 01  COMMON-WORKAREAS.                                            00099900
100000 COPY G2ALCKE2.                                                   00100000
100100 COPY GACDACWA.                                                   00100100
100200                                                                  00100200
100300     05  GAS3UPD-PASSED-AREA.                                     00100300
100400         07  LVL2-B-SW                PIC X.                      00100400
100500         07  LVL2-F-SW                PIC X.                      00100500
100600         07  LVL2-G-SW                PIC X.                      00100600
100700         07  INTR-TAB-PGM-ID          PIC X(8).                   00100700
100800         07  FILLER                   PIC X(09).                  00100800
100900     05  DELADD-OPTION                PIC X(7).                   00100900
101000                                                                  00101000
101100/    P R O C E D U R E   D I V I S I O N                          00101100
101200 PROCEDURE DIVISION.                                              00101200
101300                                                                  00101300
101400******************************************************************00101400
101500* 0000  HOUSEKEEPING                                             *00101500
101600******************************************************************00101600
101700 0000-000-HOUSEKEEPING          SECTION.                          00101700
101800 0000-010.                                                        00101800
101900                                                                  00101900
102000     EXEC CICS GETMAIN                                            00102000
102100               SET(ADDRESS OF COMMON-WORKAREAS)                   00102100
102200               INITIMG(WS-HEX-00)                                 00102200
102300               LENGTH(LENGTH OF COMMON-WORKAREAS)                 00102300
102400               END-EXEC.                                          00102400
102500                                                                  00102500
102600     MOVE ZEROES  TO  ACWA-CDE-1U-COUNT,  ACWA-CDE-2B-COUNT.      00102600
102700                                                                  00102700
102800     EXEC CICS GETMAIN                                            00102800
102900               SET(ADDRESS OF GA1XI01I)                           00102900
103000               INITIMG(WS-HEX-00)                                 00103000
103100               LENGTH(LENGTH OF GA1XI01I)                         00103100
103200               END-EXEC.                                          00103200
103300                                                                  00103300
103400     SET ACWA-MAPSET-PNTR  TO  ADDRESS OF  GA1XI01I.              00103400
103500                                                                  00103500
103600                                                                  00103600
103700     MOVE  +19   TO  GCVI-COMMAREA-LEN.                           00103700
103800     MOVE  'N'   TO  ACWA-ERROR-SW                                00103800
103900                     ACWA-CDE-FIELD-CHANGE-IND                    00103900
104000                     ACWA-CDE-REC-CHANGE-IND                      00104000
104100                     ACWA-CDE-RESET-WF-IND.                       00104100
104200     MOVE  ZERO  TO  ACWA-FIELD-CHG-CNT.                          00104200
104300     MOVE  SPACE TO  ACWA-CDE-STATUS-CHANGE-IND                   00104300
104400                     ACWA-CDE-INTERNAL-TAB-IND.                   00104400
104500                                                                  00104500
104600     COMPUTE WS-IO-PARM-WRK-GRP-SPEC-LEN =                        00104600
104700             GC-GCIOPARM-LEN             +                        00104700
104800             GC-WORKFILE-KEY-LEN         +                        00104800
104900             GC-GCGRPSPC-FIXED-LEN       +                        00104900
105000            (GC-GCGRPSPC-VARY-LEN        *                        00105000
105100             GC-GCGRPSPC-VARY-MAX-OCUR).                          00105100
105200                                                                  00105200
105300     COMPUTE WS-WRK-GRP-SPEC-LEN         =                        00105300
105400             GC-WORKFILE-KEY-LEN         +                        00105400
105500             GC-GCGRPSPC-FIXED-LEN       +                        00105500
105600            (GC-GCGRPSPC-VARY-LEN        *                        00105600
105700             GC-GCGRPSPC-VARY-MAX-OCUR).                          00105700
105800                                                                  00105800
105900     COMPUTE WS-IO-PARM-WRK-CONTRACT-LEN =                        00105900
106000             GC-GCIOPARM-LEN             +                        00106000
106100             GC-WORKFILE-KEY-LEN         +                        00106100
106200             GC-GCCONTR-FIXED-LEN        +                        00106200
106300            (GC-GCCONTR-VARY-LEN         *                        00106300
106400             GC-GCCONTR-VARY-MAX-OCUR).                           00106400
106500                                                                  00106500
106600     COMPUTE WS-WRK-CONTRACT-LEN         =                        00106600
106700             GC-WORKFILE-KEY-LEN         +                        00106700
106800             GC-GCCONTR-FIXED-LEN        +                        00106800
106900            (GC-GCCONTR-VARY-LEN         *                        00106900
107000             GC-GCCONTR-VARY-MAX-OCUR).                           00107000
107100                                                                  00107100
107200     COMPUTE WS-IO-PARM-WRK-BEN-PROV-LEN =                        00107200
107300             GC-GCIOPARM-LEN             +                        00107300
107400             GC-WORKFILE-KEY-LEN         +                        00107400
107500             GC-GCBENPRV-FIXED-LEN       +                        00107500
107600            (GC-GCBENPRV-VARY-LEN        *                        00107600
107700             GC-GCBENPRV-VARY-MAX-OCUR).                          00107700
107800                                                                  00107800
107900     COMPUTE WS-WRK-BEN-PROV-LEN         =                        00107900
108000             GC-WORKFILE-KEY-LEN         +                        00108000
108100             GC-GCBENPRV-FIXED-LEN       +                        00108100
108200            (GC-GCBENPRV-VARY-LEN        *                        00108200
108300             GC-GCBENPRV-VARY-MAX-OCUR).                          00108300
108400                                                                  00108400
108500     COMPUTE WS-IO-PARM-WRK-CONTROL-LEN  =                        00108500
108600             GC-GCIOPARM-LEN             +                        00108600
108700             GC-WORKFILE-KEY-LEN         +                        00108700
108800             GC-WORKFILE-CONTROL-REC-LEN.                         00108800
108900                                                                  00108900
109000     IF EIBAID  =  DFHCLEAR                                       00109000
109100         EXEC CICS SEND FROM(WS-ONE-LOW)                          00109100
109200                        ERASE                                     00109200
109300         END-EXEC                                                 00109300
109400         EXEC CICS RETURN                                         00109400
109500         END-EXEC.                                                00109500
109600                                                                  00109600
109700     EXEC CICS  HANDLE  CONDITION                                 00109700
109800                MAPFAIL(6400-000-XCTL-TO-MAIN-MENU)  END-EXEC.    00109800
109900                                                                  00109900
110000 0000-900-EXIT.                                                   00110000
110100          EXIT.                                                   00110100
110200/*****************************************************************00110200
110300* 1000  MAIN LINE                                                *00110300
110400******************************************************************00110400
110500 1000-000-MAIN-LINE             SECTION.                          00110500
110600 1000-010.                                                        00110600
110700                                                                  00110700
110800     IF  EIBTRNID  NOT =  'GA1D'                                  00110800
110900         PERFORM 4000-000-DISPLAY-FIRST-SCREEN.                   00110900
111000                                                                  00111000
111100     EXEC CICS  RECEIVE   MAP('GA1XI01')  MAPSET('GA1XSET')       00111100
111200                END-EXEC.                                         00111200
111300                                                                  00111300
111400     IF  SCRNIDNI  NOT =  '001D00'                                00111400
111500         PERFORM 6400-000-XCTL-TO-MAIN-MENU.                      00111500
111600                                                                  00111600
111700     IF  FRMNUIDI = 'GS3A'                                        00111700
111800         MOVE IDLINEI   TO   GROUP-SPECIFIC-ID-LINE.              00111800
111900     IF  FRMNUIDI = 'GC4A' OR 'GTM1'                              00111900
112000         MOVE IDLINEI   TO   CONTRACT-ID-LINE.                    00112000
112100     IF  FRMNUIDI = 'GC8A'                                        00112100
112200         MOVE IDLINEI   TO   BENEFIT-PROVISION-ID-LINE.           00112200
112300                                                                  00112300
112400     IF EIBAID = DFHPF1 OR DFHPF13                                00112400
112500        PERFORM 8000-000-SWITCH-ADD-DEL-MODE.                     00112500
112600                                                                  00112600
112700     IF EIBAID = DFHPF3 OR DFHPF15                                00112700
112800        PERFORM 5000-000-XCTL-TO-PREVIOUS-MENU.                   00112800
112900                                                                  00112900
113000     IF (EIBAID  =  DFHPF7 OR  DFHPF19 OR  DFHPF8 OR  DFHPF20) AND00113000
113100        DELADDI  =  'CHG/ADD'                                     00113100
113200     THEN                                                         00113200
113300         SET  WT-01-INDEX                     TO +22              00113300
113400         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00113400
113500         MOVE -1                              TO  PERIODL         00113500
113600         GO TO 1000-900-EXIT.                                     00113600
113700                                                                  00113700
113800                                                                  00113800
113900     IF (EIBAID  =  DFHPF7 OR  DFHPF19 OR  DFHPF8 OR  DFHPF20) AND00113900
114000        DELOPTNI  =  'D'                                          00114000
114100     THEN                                                         00114100
114200         SET  WT-01-INDEX                     TO +06              00114200
114300         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00114300
114400         MOVE -1                              TO  DELOPTNL        00114400
114500         GO TO 1000-900-EXIT.                                     00114500
114600                                                                  00114600
114700     PERFORM 1100-000-VALIDATE-SCREEN.                            00114700
114800                                                                  00114800
114900     IF  EIBAID  = DFHPF4 OR DFHPF16  AND                         00114900
115000         ACWA-SCREEN-HAS-ERRORS                                   00115000
115100     THEN                                                         00115100
115200         SET  WT-01-INDEX                     TO +13              00115200
115300         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00115300
115400         GO TO 1000-900-EXIT.                                     00115400
115500                                                                  00115500
115600     IF  EIBAID  = DFHPF4 OR DFHPF16                              00115600
115700     THEN                                                         00115700
115800         IF  ACWA-SCREEN-HAS-NO-ERRORS                            00115800
115900         THEN                                                     00115900
116000             IF  GCVI-TABLE-SW = 'N'                              00116000
116100             THEN                                                 00116100
116200                 PERFORM 2000-000-PROCESS-REQUEST                 00116200
116300                 GO TO  1000-990-RETURN                           00116300
116400             ELSE                                                 00116400
116500                 SET  WT-01-INDEX                     TO +09      00116500
116600                 MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO  00116600
116700                 MOVE -1                              TO PERIODL  00116700
116800                 GO TO 1000-900-EXIT                              00116800
116900         ELSE                                                     00116900
117000             NEXT SENTENCE                                        00117000
117100     ELSE                                                         00117100
117200         NEXT SENTENCE.                                           00117200
117300                                                                  00117300
117400     IF  ACWA-SCREEN-HAS-ERRORS                                   00117400
117500         GO TO 1000-900-EXIT.                                     00117500
117600                                                                  00117600
117700     IF  EIBAID  =  DFHENTER OR                                   00117700
117800                    DFHPF7   OR  DFHPF19 OR   DFHPF8 OR  DFHPF20  00117800
117900     THEN                                                         00117900
118000         PERFORM 2000-000-PROCESS-REQUEST                         00118000
118100                 GO TO  1000-990-RETURN.                          00118100
118200                                                                  00118200
118300     PERFORM 7900-000-RESET-ATTRIBUTES.                           00118300
118400     MOVE -1                              TO PERIODL.             00118400
118500     SET  WT-01-INDEX                     TO +10                  00118500
118600     MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO.             00118600
118700                                                                  00118700
118800                                                                  00118800
118900 1000-900-EXIT.                                                   00118900
119000                                                                  00119000
119100     PERFORM 3100-000-READ-RECORD.                                00119100
119200     MOVE GAC-ENTRY-COUNT  TO  GAC-ENTRY-COUNT.                   00119200
119300     PERFORM 9010-000-SEND-DATAONLY-RETURN.                       00119300
119400 1000-990-RETURN.                                                 00119400
119500*    EXEC CICS  RETURN  END-EXEC.                                 00119500
119600     IF DELADD-OPTION = 'GAS3UPD'                                 00119600
119700         IF INTR-TAB-PGM-ID = 'GA1GPGM'                           00119700
119800             MOVE SPACES TO DELADD-OPTION                         00119800
119900             EXEC CICS RETURN TRANSID('GA1G')                     00119900
120000                       COMMAREA(COMMON-WORKAREAS)                 00120000
120100                       END-EXEC                                   00120100
120200         ELSE                                                     00120200
120300         IF INTR-TAB-PGM-ID = 'GA2GPGM'                           00120300
120400             MOVE SPACES TO DELADD-OPTION                         00120400
120500             EXEC CICS RETURN TRANSID('GA2G')                     00120500
120600                       COMMAREA(COMMON-WORKAREAS)                 00120600
120700                       END-EXEC                                   00120700
120800         ELSE                                                     00120800
120900         IF INTR-TAB-PGM-ID = 'GA1HPGM'                           00120900
121000             MOVE SPACES TO DELADD-OPTION                         00121000
121100             EXEC CICS  RETURN TRANSID('GA1H')                    00121100
121200                        COMMAREA(COMMON-WORKAREAS)                00121200
121300                        END-EXEC                                  00121300
121400         ELSE                                                     00121400
121500         IF INTR-TAB-PGM-ID = 'GA2HPGM'                           00121500
121600             MOVE SPACES TO DELADD-OPTION                         00121600
121700             EXEC CICS  RETURN TRANSID('GA2H')                    00121700
121800                        COMMAREA(COMMON-WORKAREAS)                00121800
121900                        END-EXEC                                  00121900
122000         ELSE                                                     00122000
122100         IF INTR-TAB-PGM-ID = 'GA1SPGM'                           00122100
122200             MOVE SPACES TO DELADD-OPTION                         00122200
122300             EXEC CICS  RETURN TRANSID('GA1S')                    00122300
122400                        COMMAREA(COMMON-WORKAREAS)                00122400
122500                        END-EXEC                                  00122500
122600         ELSE                                                     00122600
122700         IF INTR-TAB-PGM-ID = 'GA1IPGM'                           00122700
122800             MOVE SPACES TO DELADD-OPTION                         00122800
122900             EXEC CICS  RETURN TRANSID('GA1I')                    00122900
123000                        COMMAREA(COMMON-WORKAREAS)                00123000
123100                        END-EXEC                                  00123100
123200         ELSE                                                     00123200
123300         IF INTR-TAB-PGM-ID = 'GA2SPGM'                           00123300
123400             MOVE SPACES TO DELADD-OPTION                         00123400
123500             EXEC CICS  RETURN TRANSID('GA2S')                    00123500
123600                        COMMAREA(COMMON-WORKAREAS)                00123600
123700                        END-EXEC                                  00123700
123800         ELSE                                                     00123800
123900         IF INTR-TAB-PGM-ID = 'GA2IPGM'                           00123900
124000             MOVE SPACES TO DELADD-OPTION                         00124000
124100             EXEC CICS  RETURN TRANSID('GA2I')                    00124100
124200                        COMMAREA(COMMON-WORKAREAS)                00124200
124300                        END-EXEC                                  00124300
124400         ELSE                                                     00124400
124500         IF INTR-TAB-PGM-ID = 'GA1NPGM'                           00124500
124600             MOVE SPACES TO DELADD-OPTION                         00124600
124700             EXEC CICS  RETURN TRANSID('GA1N')                    00124700
124800                        COMMAREA(COMMON-WORKAREAS)                00124800
124900                        END-EXEC                                  00124900
125000         ELSE                                                     00125000
125100         IF INTR-TAB-PGM-ID = 'GA2NPGM'                           00125100
125200             MOVE SPACES TO DELADD-OPTION                         00125200
125300             EXEC CICS  RETURN TRANSID('GA2N')                    00125300
125400                        COMMAREA(COMMON-WORKAREAS)                00125400
125500                        END-EXEC                                  00125500
125600         ELSE                                                     00125600
125700         IF INTR-TAB-PGM-ID = 'GA1OPGM'                           00125700
125800             MOVE SPACES TO DELADD-OPTION                         00125800
125900             EXEC CICS  RETURN TRANSID('GA1O')                    00125900
126000                        COMMAREA(COMMON-WORKAREAS)                00126000
126100                        END-EXEC                                  00126100
126200         ELSE                                                     00126200
126300         IF INTR-TAB-PGM-ID = 'GA2OPGM'                           00126300
126400             MOVE SPACES TO DELADD-OPTION                         00126400
126500             EXEC CICS  RETURN TRANSID('GA2O')                    00126500
126600                        COMMAREA(COMMON-WORKAREAS)                00126600
126700                        END-EXEC                                  00126700
126800         ELSE                                                     00126800
126900         EXEC CICS  RETURN TRANSID('GA1D')                        00126900
127000                    COMMAREA(DFHCOMMAREA)                         00127000
127100                    LENGTH  (EIBCALEN)                            00127100
127200                    END-EXEC                                      00127200
127300     ELSE                                                         00127300
127400     EXEC CICS  RETURN TRANSID('GA1D')                            00127400
127500                COMMAREA(DFHCOMMAREA)                             00127500
127600                LENGTH  (EIBCALEN)                                00127600
127700                END-EXEC.                                         00127700
127800                                                                  00127800
127900     GOBACK.                                                      00127900
128000 1000-999-EXIT.                                                   00128000
128100          EXIT.                                                   00128100
128200/*****************************************************************00128200
128300* 1100  VALIDATE SCREEN                                          *00128300
128400*                                                                *00128400
128500*    THIS IS PRIMARILY A VALIDATION ROUTINE OF DATA BEING ENTERED*00128500
128600*  BY THE OPERATOR, PLUS THE ADDITION OF SOME REINITIALIZATION.  *00128600
128700*  1. REINITIALIZE ATTRIBUTES THAT THE PROGRAM MIGHT MODIFY, AND *00128700
128800*     RESET THE ERROR MESSAGE AND DELETE OPTION TO BLANKS.       *00128800
128900*  2. INSURE THE VALIDITY OF THE OPTIONS THAT CAN BE USED FOR THE*00128900
129000*     INTERNAL TABULAR.                                          *00129000
129100******************************************************************00129100
129200 1100-000-VALIDATE-SCREEN       SECTION.                          00129200
129300 1100-010.                                                        00129300
129400                                                                  00129400
129500     SET ACWA-WF-ALL-LEVEL-TAB-PNTR TO                            00129500
129600         ADDRESS OF  WF-IO-PARM-ALL-LVL-TAB-RECORD.               00129600
129700                                                                  00129700
129800     SET ACWA-COPY-TAB-PNTR         TO                            00129800
129900         ADDRESS OF  COPY-TABULAR-TABLE-AREA.                     00129900
130000                                                                  00130000
130100     SET ACWA-WF-INTERNAL-TAB-PNTR  TO                            00130100
130200         ADDRESS OF  WF-IO-PARM-INTERNAL-TAB-RECORD.              00130200
130300                                                                  00130300
130400     SET ACWA-WF-GRP-SPEC-PNTR      TO                            00130400
130500         ADDRESS OF  WF-IO-PARM-WRK-GRP-SPEC-REC.                 00130500
130600                                                                  00130600
130700     SET ACWA-WF-CONTRACT-PNTR      TO                            00130700
130800         ADDRESS OF  WF-IO-PARM-WRK-CONTRACT-REC.                 00130800
130900                                                                  00130900
131000     SET ACWA-WF-BEN-PROV-PNTR      TO                            00131000
131100         ADDRESS OF  WF-IO-PARM-WRK-BEN-PROV-REC.                 00131100
131200                                                                  00131200
131300     SET ACWA-WF-CONTROL-RECORD-PNTR    TO                        00131300
131400         ADDRESS OF  WF-IO-PARM-WRK-CONTROL-REC.                  00131400
131500                                                                  00131500
131600     SET ACWA-PR-CONTRACT-PNTR      TO                            00131600
131700         ADDRESS OF  PR-IO-PARM-WRK-CONTRACT-REC.                 00131700
131800                                                                  00131800
131900     SET ACWA-PR-GRP-SPEC-PNTR      TO                            00131900
132000         ADDRESS OF  PR-IO-PARM-WRK-GRP-SPEC-REC.                 00132000
132100                                                                  00132100
132200     SET ACWA-PR-BEN-PROV-PNTR      TO                            00132200
132300         ADDRESS OF  PR-IO-PARM-WRK-BEN-PROV-REC.                 00132300
132400                                                                  00132400
132500     SET ACWA-PR-ALL-LEVEL-TAB-PNTR   TO                          00132500
132600         ADDRESS OF  PR-IO-PARM-ALL-LVL-TAB-RECORD.               00132600
132700                                                                  00132700
132800     MOVE 'N'              TO ACWA-ERROR-SW.                      00132800
132900     MOVE 'Y'              TO GCVI-TABLE-SW.                      00132900
133000     MOVE SPACES           TO ERRMSGO.                            00133000
133100     MOVE DFHBMFSE         TO PERIODA.                            00133100
133200     MOVE DFHBMASF         TO IBGRIDA    IPGNIDA   IPGTIDA        00133200
133300                              IDGDIDA    IPGPIDA   IPGSIDA        00133300
133400                              IBGRSLTA   IPGNSLTA  IPGTSLTA       00133400
133500                              IDGDSLTA   IPGPSLTA  IPGSSLTA.      00133500
133600                                                                  00133600
133700     PERFORM 7900-000-RESET-ATTRIBUTES.                           00133700
133800                                                                  00133800
133900*------------- LINK TO SCREEN EDIT MODULE -----------------------*00133900
134000                                                                  00134000
134100     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00134100
134200                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00134200
134300     EXEC CICS  LINK  PROGRAM('GASEDIT1')                         00134300
134400                COMMAREA (COMMON-WORKAREAS)                       00134400
134500                LENGTH(LENGTH OF COMMON-WORKAREAS)  END-EXEC.     00134500
134600                                                                  00134600
134700     IF  ACWA-SCREEN-HAS-ERRORS                                   00134700
134800         GO TO 1100-900-EXIT.                                     00134800
134900                                                                  00134900
135000     IF  ACWA-FIELD-CHG-CNT > ZEROS                               00135000
135100         GO TO 1100-900-EXIT.                                     00135100
135200                                                                  00135200
135300     IF  (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE) AND           00135300
135400         (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE) AND           00135400
135500         (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE) AND           00135500
135600         (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE) AND           00135600
135700         (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE) AND           00135700
135800         (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE)               00135800
135900     THEN                                                         00135900
136000         GO TO 1100-900-EXIT.                                     00136000
136100                                                                  00136100
136200                                                                  00136200
136300     COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   00136300
136400              GC-GCIOPARM-LEN                 +                   00136400
136500              GC-WORKFILE-KEY-LEN             +                   00136500
136600              GC-GCTABULR-IPGP-FIXED-LEN      +                   00136600
136700             (GC-GCTABULR-IPGP-VARY-LEN       *                   00136700
136800              GC-GCTABULR-IPGP-VARY-MAX-OCUR)                     00136800
136900                                                                  00136900
137000        EXEC CICS GETMAIN                                         00137000
137100               SET(ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD)     00137100
137200               INITIMG(WS-HEX-00)                                 00137200
137300               LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)             00137300
137400               END-EXEC                                           00137400
137500                                                                  00137500
137600     SET ACWA-WF-INTERNAL-TAB-PNTR  TO                            00137600
137700         ADDRESS OF  WF-IO-PARM-INTERNAL-TAB-RECORD.              00137700
137800                                                                  00137800
137900                                                                  00137900
138000     IF  IBGROPTI  =  'C'                                         00138000
138100     THEN                                                         00138100
138200         IF  IBGRSLTI  >  '8999999'                               00138200
138300         THEN                                                     00138300
138400             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00138400
138500             GO TO 1100-100-GET-INTERNAL-TAB                      00138500
138600         ELSE                                                     00138600
138700             ADD 100        TO  ACWA-FIELD-CHG-CNT                00138700
138800             MOVE '#IBGR '  TO  GCIO-TAB-TABULAR-ID               00138800
138900             MOVE IBGRSLTI  TO  GCIO-TAB-SLOT-NO                  00138900
139000     ELSE                                                         00139000
139100         NEXT SENTENCE.                                           00139100
139200                                                                  00139200
139300     IF  IDGDOPTI  =  'C'                                         00139300
139400     THEN                                                         00139400
139500         IF  IDGDSLTI  >  '8999999'                               00139500
139600         THEN                                                     00139600
139700             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00139700
139800             GO TO 1100-100-GET-INTERNAL-TAB                      00139800
139900         ELSE                                                     00139900
140000             ADD 100        TO  ACWA-FIELD-CHG-CNT                00140000
140100             MOVE '#IDGD '  TO  GCIO-TAB-TABULAR-ID               00140100
140200             MOVE IDGDSLTI  TO  GCIO-TAB-SLOT-NO                  00140200
140300     ELSE                                                         00140300
140400         NEXT SENTENCE.                                           00140400
140500                                                                  00140500
140600     IF  IPGNOPTI  =  'C'                                         00140600
140700     THEN                                                         00140700
140800         IF  IPGNSLTI  > '8999999'                                00140800
140900         THEN                                                     00140900
141000             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00141000
141100             GO TO 1100-100-GET-INTERNAL-TAB                      00141100
141200         ELSE                                                     00141200
141300           ADD 100        TO  ACWA-FIELD-CHG-CNT                  00141300
141400           MOVE '#IPGN '  TO  GCIO-TAB-TABULAR-ID                 00141400
141500           MOVE IPGNSLTI  TO  GCIO-TAB-SLOT-NO                    00141500
141600     ELSE                                                         00141600
141700         NEXT SENTENCE.                                           00141700
141800                                                                  00141800
141900     IF  IPGPOPTI  =  'C'                                         00141900
142000     THEN                                                         00142000
142100         IF  IPGPSLTI  > '8999999'                                00142100
142200         THEN                                                     00142200
142300             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00142300
142400             GO TO 1100-100-GET-INTERNAL-TAB                      00142400
142500         ELSE                                                     00142500
142600           ADD 100        TO  ACWA-FIELD-CHG-CNT                  00142600
142700           MOVE '#IPGP '  TO  GCIO-TAB-TABULAR-ID                 00142700
142800           MOVE IPGPSLTI  TO  GCIO-TAB-SLOT-NO                    00142800
142900     ELSE                                                         00142900
143000         NEXT SENTENCE.                                           00143000
143100                                                                  00143100
143200     IF  IPGTOPTI  =  'C'                                         00143200
143300     THEN                                                         00143300
143400         IF  IPGTSLTI  >  '8999999'                               00143400
143500         THEN                                                     00143500
143600             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00143600
143700             GO TO 1100-100-GET-INTERNAL-TAB                      00143700
143800         ELSE                                                     00143800
143900             ADD 100        TO  ACWA-FIELD-CHG-CNT                00143900
144000             MOVE '#IPGT '  TO  GCIO-TAB-TABULAR-ID               00144000
144100             MOVE IPGTSLTI  TO  GCIO-TAB-SLOT-NO                  00144100
144200     ELSE                                                         00144200
144300         NEXT SENTENCE.                                           00144300
144400                                                                  00144400
144500                                                                  00144500
144600     IF  IPGSOPTI  =  'C'                                         00144600
144700     THEN                                                         00144700
144800         IF  IPGSSLTI  >  '8999999'                               00144800
144900         THEN                                                     00144900
145000             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00145000
145100             GO TO 1100-100-GET-INTERNAL-TAB                      00145100
145200         ELSE                                                     00145200
145300             ADD 100        TO  ACWA-FIELD-CHG-CNT                00145300
145400             MOVE '#IPGS '  TO  GCIO-TAB-TABULAR-ID               00145400
145500             MOVE IPGSSLTI  TO  GCIO-TAB-SLOT-NO                  00145500
145600     ELSE                                                         00145600
145700         NEXT SENTENCE.                                           00145700
145800                                                                  00145800
145900                                                                  00145900
146000     IF IBGROPTI  =  'MT'                                         00146000
146100        MOVE '#IBGR '  TO  GCIO-TAB-TABULAR-ID                    00146100
146200        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00146200
146300                                                                  00146300
146400     IF IBGROPTI  =  'A'                                          00146400
146500        MOVE '#IBGR '  TO  GCIO-TAB-TABULAR-ID                    00146500
146600        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00146600
146700                                                                  00146700
146800     IF IDGDOPTI  =  'MT'                                         00146800
146900        MOVE '#IDGD '  TO  GCIO-TAB-TABULAR-ID                    00146900
147000        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00147000
147100                                                                  00147100
147200     IF IDGDOPTI  =  'A'                                          00147200
147300        MOVE '#IDGD '  TO  GCIO-TAB-TABULAR-ID                    00147300
147400        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00147400
147500                                                                  00147500
147600     IF IPGNOPTI  =  'MT'                                         00147600
147700        MOVE '#IPGN '  TO  GCIO-TAB-TABULAR-ID                    00147700
147800        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00147800
147900                                                                  00147900
148000     IF IPGNOPTI  =  'A'                                          00148000
148100        MOVE '#IPGN '  TO  GCIO-TAB-TABULAR-ID                    00148100
148200        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00148200
148300                                                                  00148300
148400     IF IPGPOPTI  =  'MT'                                         00148400
148500        MOVE '#IPGP '  TO  GCIO-TAB-TABULAR-ID                    00148500
148600        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00148600
148700                                                                  00148700
148800     IF IPGPOPTI  =  'A'                                          00148800
148900        MOVE '#IPGP '  TO  GCIO-TAB-TABULAR-ID                    00148900
149000        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00149000
149100                                                                  00149100
149200     IF IPGTOPTI  =  'MT'                                         00149200
149300        MOVE '#IPGT '  TO  GCIO-TAB-TABULAR-ID                    00149300
149400        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00149400
149500                                                                  00149500
149600     IF IPGTOPTI  =  'A'                                          00149600
149700        MOVE '#IPGT '  TO  GCIO-TAB-TABULAR-ID                    00149700
149800        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00149800
149900                                                                  00149900
150000     IF IPGSOPTI  =  'MT'                                         00150000
150100        MOVE '#IPGS '  TO  GCIO-TAB-TABULAR-ID                    00150100
150200        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00150200
150300                                                                  00150300
150400     IF IPGSOPTI  =  'A'                                          00150400
150500        MOVE '#IPGS '  TO  GCIO-TAB-TABULAR-ID                    00150500
150600        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00150600
150700                                                                  00150700
150800     MOVE GCIO-TABULAR-FILE      TO GCIO2-FILE-KEY.               00150800
150900     MOVE GC-GCTABULR-DDNAME     TO GCIO2-FILE-DDNAME.            00150900
151000     MOVE GC-GCIO-AREA-2         TO GCIO2-IO-AREA-TO-USE.         00151000
151100     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO2-FILE-ACCESS-CODE.       00151100
151200     MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          00151200
151300          TO  GXA-ENTRY-COUNT.                                    00151300
151400                                                                  00151400
151500     EXEC CICS  LINK  PROGRAM('GCIOPGM')                          00151500
151600                COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          00151600
151700                LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)   END-EXEC.00151700
151800                                                                  00151800
151900     IF  NOT GCIO2-GOOD-RETURN AND  ACWA-PROD-INTERNAL-CHG        00151900
152000     THEN                                                         00152000
152100         SET  WT-01-INDEX                     TO +17              00152100
152200         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00152200
152300         MOVE 'Y'                             TO ACWA-ERROR-SW    00152300
152400         IF  IBGROPTI  =  'C'                                     00152400
152500         THEN                                                     00152500
152600             MOVE -1        TO  IBGROPTL                          00152600
152700             MOVE DFHBMUBF  TO  IBGROPTA                          00152700
152800             MOVE DFHBMASB  TO  IBGRIDA  IBGRSLTA                 00152800
152900             GO TO 1100-900-EXIT                                  00152900
153000         ELSE                                                     00153000
153100         IF  IPGNOPTI  =  'C'                                     00153100
153200         THEN                                                     00153200
153300             MOVE -1        TO  IPGNOPTL                          00153300
153400             MOVE DFHBMUBF  TO  IPGNOPTA                          00153400
153500             MOVE DFHBMASB  TO  IPGNIDA  IPGNSLTA                 00153500
153600             GO TO 1100-900-EXIT                                  00153600
153700         ELSE                                                     00153700
153800         IF  IPGTOPTI  =  'C'                                     00153800
153900         THEN                                                     00153900
154000              MOVE -1        TO  IPGTOPTL                         00154000
154100              MOVE DFHBMUBF  TO  IPGTOPTA                         00154100
154200              MOVE DFHBMASB  TO  IPGTIDA  IPGTSLTA                00154200
154300              GO TO 1100-900-EXIT                                 00154300
154400         ELSE                                                     00154400
154500         IF  IPGSOPTI  =  'C'                                     00154500
154600         THEN                                                     00154600
154700              MOVE -1        TO  IPGSOPTL                         00154700
154800              MOVE DFHBMUBF  TO  IPGSOPTA                         00154800
154900              MOVE DFHBMASB  TO  IPGSIDA  IPGSSLTA                00154900
155000              GO TO 1100-900-EXIT                                 00155000
155100         ELSE                                                     00155100
155200         IF  IDGDOPTI  =  'C'                                     00155200
155300         THEN                                                     00155300
155400             MOVE -1        TO  IDGDOPTL                          00155400
155500             MOVE DFHBMUBF  TO  IDGDOPTA                          00155500
155600             MOVE DFHBMASB  TO  IDGDIDA  IDGDSLTA                 00155600
155700             GO TO 1100-900-EXIT                                  00155700
155800         ELSE                                                     00155800
155900         IF  IPGPOPTI  =  'C'                                     00155900
156000         THEN                                                     00156000
156100             MOVE -1        TO  IPGPOPTL                          00156100
156200             MOVE DFHBMUBF  TO  IPGPOPTA                          00156200
156300             MOVE DFHBMASB  TO  IPGPIDA  IPGPSLTA                 00156300
156400             GO TO 1100-900-EXIT                                  00156400
156500         ELSE                                                     00156500
156600             NEXT SENTENCE                                        00156600
156700     ELSE                                                         00156700
156800         NEXT SENTENCE.                                           00156800
156900                                                                  00156900
157000                                                                  00157000
157100     IF  NOT GCIO2-GOOD-RETURN AND  GCIO-TAB-SLOT-NO  NOT =  1    00157100
157200     THEN                                                         00157200
157300         SET  WT-01-INDEX                     TO +18              00157300
157400         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00157400
157500         MOVE 'Y'                             TO ACWA-ERROR-SW    00157500
157600         MOVE -1                              TO MFRMSLTL         00157600
157700         MOVE DFHBMUBF                        TO MFRMSLTA         00157700
157800         GO TO 1100-900-EXIT.                                     00157800
157900                                                                  00157900
158000     IF  NOT GCIO2-GOOD-RETURN                                    00158000
158100     THEN                                                         00158100
158200         MOVE WS-ABCODE-1DF1        TO WS-ABCODE                  00158200
158300         MOVE WS-ABCODE-1DF1-MSG    TO WS-ABCODE-MSG              00158300
158400         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                   00158400
158500                                                                  00158500
158600     COMPUTE  GCIO2-RECORD-LENGTH  =                              00158600
158700              GC-WORKFILE-KEY-LEN  +                              00158700
158800              GCIO2-RECORD-LENGTH.                                00158800
158900                                                                  00158900
159000     IF IBGROPTI  =  'C' OR  'MT' OR 'A'                          00159000
159100        IF  GXA-ENTRY-COUNT  NOT >  1                             00159100
159200            MOVE 'GA2GPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00159200
159300                                INTR-TAB-PGM-ID                   00159300
159400        ELSE                                                      00159400
159500            MOVE 'GA1GPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00159500
159600                                INTR-TAB-PGM-ID.                  00159600
159700                                                                  00159700
159800     IF IPGNOPTI  =  'C' OR  'MT' OR 'A'                          00159800
159900        IF  GXA-ENTRY-COUNT  NOT >  1                             00159900
160000            MOVE 'GA2HPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00160000
160100                                INTR-TAB-PGM-ID                   00160100
160200        ELSE                                                      00160200
160300            MOVE 'GA1HPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00160300
160400                                INTR-TAB-PGM-ID.                  00160400
160500                                                                  00160500
160600     IF IPGTOPTI  =  'C' OR  'MT' OR 'A'                          00160600
160700        IF  GXA-ENTRY-COUNT  NOT >  1                             00160700
160800            MOVE 'GA2IPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00160800
160900                                INTR-TAB-PGM-ID                   00160900
161000        ELSE                                                      00161000
161100            MOVE 'GA1IPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00161100
161200                                INTR-TAB-PGM-ID.                  00161200
161300                                                                  00161300
161400     IF IPGSOPTI  =  'C' OR  'MT' OR 'A'                          00161400
161500        IF  GXA-ENTRY-COUNT  NOT >  1                             00161500
161600            MOVE 'GA2SPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00161600
161700                                INTR-TAB-PGM-ID                   00161700
161800        ELSE                                                      00161800
161900            MOVE 'GA1SPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00161900
162000                                INTR-TAB-PGM-ID.                  00162000
162100                                                                  00162100
162200     IF IDGDOPTI  =  'C' OR  'MT' OR 'A'                          00162200
162300        IF  GXA-ENTRY-COUNT  NOT >  1                             00162300
162400            MOVE 'GA2NPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00162400
162500                                INTR-TAB-PGM-ID                   00162500
162600        ELSE                                                      00162600
162700            MOVE 'GA1NPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00162700
162800                                INTR-TAB-PGM-ID.                  00162800
162900                                                                  00162900
163000     IF IPGPOPTI  =  'C' OR  'MT' OR 'A'                          00163000
163100        IF  GXA-ENTRY-COUNT  NOT >  1                             00163100
163200            MOVE 'GA2OPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00163200
163300                                INTR-TAB-PGM-ID                   00163300
163400        ELSE                                                      00163400
163500            MOVE 'GA1OPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00163500
163600                                INTR-TAB-PGM-ID.                  00163600
163700                                                                  00163700
163800     GO TO 1100-900-EXIT.                                         00163800
163900                                                                  00163900
164000/                                                                 00164000
164100 1100-100-GET-INTERNAL-TAB.                                       00164100
164200                                                                  00164200
164300                                                                  00164300
164400     IF FRMNUIDI  =  'GS3A'                                       00164400
164500        PERFORM 6000-000-BUILD-GROUP-SPEC-KEY.                    00164500
164600        MOVE 'G4'  TO  GCIO-WRK-RECORD-TYPE.                      00164600
164700                                                                  00164700
164800     IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            00164800
164900        PERFORM 6100-000-BUILD-CONTRACT-KEY                       00164900
165000        MOVE 'C3'  TO  GCIO-WRK-RECORD-TYPE.                      00165000
165100                                                                  00165100
165200     IF FRMNUIDI  =  'GC8A'                                       00165200
165300        PERFORM 6200-000-BUILD-BEN-PROV-KEY                       00165300
165400        MOVE 'C6'               TO GCIO-WRK-RECORD-TYPE           00165400
165500        MOVE TABIDI             TO GCIO-WRK-PROVISION-ID          00165500
165600        MOVE TABSLTNI           TO ACWA-DISPLAY-LEN-7             00165600
165700        MOVE ACWA-DISPLAY-LEN-7 TO GCIO-WRK-PROVISION-SLOT-NO.    00165700
165800                                                                  00165800
165900     MOVE GC-GCPSWORK-DDNAME TO GCIO2-FILE-DDNAME.                00165900
166000     MOVE GC-GCIO-AREA-1     TO GCIO2-IO-AREA-TO-USE.             00166000
166100                                                                  00166100
166200     IF IBGROPTI  =  'C'                                          00166200
166300        MOVE '#IBGR '  TO  GCIO-WRK-TAB-PROVISION-ID              00166300
166400        MOVE IBGRSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00166400
166500                                                                  00166500
166600     IF IDGDOPTI  =  'C'                                          00166600
166700        MOVE '#IDGD '  TO  GCIO-WRK-TAB-PROVISION-ID              00166700
166800        MOVE IDGDSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00166800
166900                                                                  00166900
167000     IF IPGNOPTI  =  'C'                                          00167000
167100        MOVE '#IPGN '  TO  GCIO-WRK-TAB-PROVISION-ID              00167100
167200        MOVE IPGNSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00167200
167300                                                                  00167300
167400     IF IPGPOPTI  =  'C'                                          00167400
167500        MOVE '#IPGP '  TO  GCIO-WRK-TAB-PROVISION-ID              00167500
167600        MOVE IPGPSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00167600
167700                                                                  00167700
167800     IF IPGTOPTI  =  'C'                                          00167800
167900        MOVE '#IPGT '  TO  GCIO-WRK-TAB-PROVISION-ID              00167900
168000        MOVE IPGTSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00168000
168100                                                                  00168100
168200     IF IPGSOPTI  =  'C'                                          00168200
168300        MOVE '#IPGS '  TO  GCIO-WRK-TAB-PROVISION-ID              00168300
168400        MOVE IPGSSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00168400
168500                                                                  00168500
168600     MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              00168600
168700     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO2-FILE-ACCESS-CODE.       00168700
168800     MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          00168800
168900          TO  GXA-ENTRY-COUNT.                                    00168900
169000                                                                  00169000
169100     EXEC CICS  LINK  PROGRAM('GCIOPGM')                          00169100
169200                COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          00169200
169300                LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. 00169300
169400                                                                  00169400
169500     IF NOT GCIO2-GOOD-RETURN                                     00169500
169600        MOVE WS-ABCODE-1DF2        TO WS-ABCODE                   00169600
169700        MOVE WS-ABCODE-1DF2-MSG    TO WS-ABCODE-MSG               00169700
169800        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00169800
169900                                                                  00169900
170000     IF IBGROPTI  =  'C'                                          00170000
170100        IF GXA-ENTRY-COUNT  NOT >  1                              00170100
170200           MOVE 'GA2GPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00170200
170300                                INTR-TAB-PGM-ID                   00170300
170400        ELSE                                                      00170400
170500           MOVE 'GA1GPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00170500
170600                                INTR-TAB-PGM-ID.                  00170600
170700                                                                  00170700
170800     IF IPGNOPTI  =  'C'                                          00170800
170900        IF GXA-ENTRY-COUNT  NOT >  1                              00170900
171000           MOVE 'GA2HPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00171000
171100                                INTR-TAB-PGM-ID                   00171100
171200        ELSE                                                      00171200
171300           MOVE 'GA1HPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00171300
171400                                INTR-TAB-PGM-ID.                  00171400
171500                                                                  00171500
171600     IF IPGTOPTI  =  'C'                                          00171600
171700        IF GXA-ENTRY-COUNT  NOT >  1                              00171700
171800           MOVE 'GA2IPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00171800
171900                                INTR-TAB-PGM-ID                   00171900
172000        ELSE                                                      00172000
172100           MOVE 'GA1IPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00172100
172200                                INTR-TAB-PGM-ID.                  00172200
172300                                                                  00172300
172400     IF IPGSOPTI  =  'C'                                          00172400
172500        IF GXA-ENTRY-COUNT  NOT >  1                              00172500
172600           MOVE 'GA2SPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00172600
172700                                INTR-TAB-PGM-ID                   00172700
172800        ELSE                                                      00172800
172900           MOVE 'GA1SPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00172900
173000                                INTR-TAB-PGM-ID.                  00173000
173100                                                                  00173100
173200     IF IDGDOPTI  =  'C'                                          00173200
173300        IF GXA-ENTRY-COUNT  NOT >  1                              00173300
173400           MOVE 'GA2NPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00173400
173500                                INTR-TAB-PGM-ID                   00173500
173600        ELSE                                                      00173600
173700           MOVE 'GA1NPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00173700
173800                                INTR-TAB-PGM-ID.                  00173800
173900                                                                  00173900
174000     IF IPGPOPTI  =  'C'                                          00174000
174100        IF GXA-ENTRY-COUNT  NOT >  1                              00174100
174200           MOVE 'GA2OPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00174200
174300                                INTR-TAB-PGM-ID                   00174300
174400        ELSE                                                      00174400
174500           MOVE 'GA1OPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00174500
174600                                INTR-TAB-PGM-ID.                  00174600
174700                                                                  00174700
174800     GO TO 1100-900-EXIT.                                         00174800
174900                                                                  00174900
175000 1100-900-EXIT. EXIT.                                             00175000
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
177000     MOVE SPACES TO LVL2-B-SW  LVL2-F-SW  LVL2-G-SW.              00177000
177100                                                                  00177100
177200     MOVE WS-ALT-WORKFILE-KEYS       TO ACWA-ALT-WORKFILE-KEYS.   00177200
177300***  SET  ACWA-INDEX-1               TO GAC-INDEX.                00177300
177400                                                                  00177400
177500     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00177500
177600                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00177600
177700                                                                  00177700
177800     EXEC CICS  LINK  PROGRAM ('GAS3UPD')                         00177800
177900                COMMAREA(COMMON-WORKAREAS)                        00177900
178000                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00178000
178100                                                                  00178100
178200     SET ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD  TO             00178200
178300                 ACWA-WF-ALL-LEVEL-TAB-PNTR.                      00178300
178400                                                                  00178400
178500     SET ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD TO             00178500
178600                 ACWA-WF-INTERNAL-TAB-PNTR.                       00178600
178700                                                                  00178700
178800     IF LVL2-B-SW =  'Y'                                          00178800
178900        PERFORM  7900-000-RESET-ATTRIBUTES                        00178900
179000        PERFORM  8100-000-DISPLAY-ADD-SCREEN.                     00179000
179100                                                                  00179100
179200     IF LVL2-F-SW =  'Y'                                          00179200
179300        PERFORM  3100-000-READ-RECORD                             00179300
179400        PERFORM  4300-000-DISPLAY-PREV.                           00179400
179500                                                                  00179500
179600     IF LVL2-G-SW =  'Y'                                          00179600
179700        PERFORM  3100-000-READ-RECORD                             00179700
179800        PERFORM  4200-000-DISPLAY-NEXT.                           00179800
179900                                                                  00179900
180000                                                                  00180000
180100 2000-900-EXIT. EXIT.                                             00180100
180200                                                                  00180200
180300/*****************************************************************00180300
180400* 2100  INSERT SKELETON                                          *00180400
180500*                                                                *00180500
180600*    THIS ROUTINE WILL ADD A NEW ENTRY INTO THE TABLE.  IF THE   *00180600
180700*  TABLE ALREADY CONTAINS THE MAXIMUM NUMBER OF 29 ENTRIES THE   *00180700
180800*  SORT ROUTINE MAY REDUCE THAT NUMBER AS IT WILL DELETE ALL     *00180800
180900*  DUPLICATES.  IF THE NEW ENTRY CAN BE ADDED AND THE OPERATOR   *00180900
181000*  REQUESTED THE ADDITION OF AN INTERNAL TABULAR THIS ROUTINE    *00181000
181100*  WILL WRITE THE NEW INTERNAL TABULAR TO THE WORKFILE AND THEN  *00181100
181200*  PASS IT TO THE ADD VERSION OF THE INTERNAL TABULAR PROGRAM.   *00181200
181300******************************************************************00181300
181400 2100-000-INSERT-SKELETON       SECTION.                          00181400
181500 2100-010.                                                        00181500
181600                                                                  00181600
181700     IF  EIBAID = DFHENTER         AND                            00181700
181800         ACWA-SCREEN-HAS-NO-ERRORS AND                            00181800
181900         GCVI-TABLE-SW = 'N'                                      00181900
182000     THEN                                                         00182000
182100         SET  WT-01-INDEX                     TO +07              00182100
182200         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00182200
182300         PERFORM 9010-000-SEND-DATAONLY-RETURN.                   00182300
182400                                                                  00182400
182500     IF  EIBAID  = DFHPF4 OR DFHPF16                              00182500
182600         PERFORM 7900-000-RESET-ATTRIBUTES.                       00182600
182700                                                                  00182700
182800     PERFORM 3200-000-READ-REC-FOR-UPDATE.                        00182800
182900                                                                  00182900
183000     IF NOT GCIO-GOOD-RETURN                                      00183000
183100        MOVE WS-ABCODE-1DF5        TO WS-ABCODE                   00183100
183200        MOVE WS-ABCODE-1DF5-MSG    TO WS-ABCODE-MSG               00183200
183300        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00183300
183400                                                                  00183400
183500                                                                  00183500
183600     IF GAC-OCC-ENTRY-TAB-SLOT-CNTR > 9999900                     00183600
183700        SET  WT-01-INDEX                     TO +15               00183700
183800        MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO           00183800
183900        MOVE -1                              TO PERIODL           00183900
184000        PERFORM 9010-000-SEND-DATAONLY-RETURN.                    00184000
184100                                                                  00184100
184200                                                                  00184200
184300     MOVE GAC-ENTRY-COUNT  TO  GAC-ENTRY-COUNT.                   00184300
184400                                                                  00184400
184500     IF  GAC-ENTRY-COUNT  NOT <  GC-GCTABULR-ADL-VARY-MAX-OCUR    00184500
184600     THEN                                                         00184600
184700         PERFORM 6500-000-SORT-COMPRESS-ALL-LVL                   00184700
184800         IF  ACWA-SCREEN-HAS-NO-ERRORS                            00184800
184900         THEN                                                     00184900
185000             PERFORM 2500-000-ADD-NEW-OCCURS                      00185000
185100****         PERFORM 4600-000-UPDATE-CDE-STATUS                   00185100
185200             IF  WRK-CDE-SP = '2 '      AND                       00185200
185300                 CDEINDO    = ('+CDE+'  OR '+CDE-')               00185300
185400                 ADD 1      TO   ACWA-CDE-1U-COUNT                00185400
185500                 SUBTRACT 1 FROM ACWA-CDE-2B-COUNT                00185500
185600                 MOVE '1U'  TO   WRK-CDE-SP                       00185600
185700                 PERFORM 3000-000-UPDATE-GAC-RECORD               00185700
185800             ELSE                                                 00185800
185900                 PERFORM 3000-000-UPDATE-GAC-RECORD               00185900
186000         ELSE                                                     00186000
186100             SET  WT-01-INDEX                     TO +16          00186100
186200             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      00186200
186300             MOVE -1                              TO  PERIODL     00186300
186400             PERFORM 9010-000-SEND-DATAONLY-RETURN                00186400
186500      ELSE                                                        00186500
186600          PERFORM 2500-000-ADD-NEW-OCCURS                         00186600
186700****      PERFORM 4600-000-UPDATE-CDE-STATUS                      00186700
186800             IF  WRK-CDE-SP = '2 '      AND                       00186800
186900                 CDEINDO    = ('+CDE+'  OR '+CDE-')               00186900
187000                 ADD 1      TO   ACWA-CDE-1U-COUNT                00187000
187100                 SUBTRACT 1 FROM ACWA-CDE-2B-COUNT                00187100
187200                 MOVE '1U'  TO   WRK-CDE-SP                       00187200
187300                 PERFORM 3000-000-UPDATE-GAC-RECORD               00187300
187400             ELSE                                                 00187400
187500                 PERFORM 3000-000-UPDATE-GAC-RECORD.              00187500
187600                                                                  00187600
187700     SET GAC-INDEX  DOWN BY  1.                                   00187700
187800                                                                  00187800
187900     IF  (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE) AND           00187900
188000         (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE) AND           00188000
188100         (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE) AND           00188100
188200         (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE) AND           00188200
188300         (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE) AND           00188300
188400         (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE)               00188400
188500     THEN                                                         00188500
188600         PERFORM 4700-000-UPDATE-CONTROL-RECORD                   00188600
188700         MOVE GAC-OCCURS-ENTRY-COUNTER (GAC-INDEX)                00188700
188800                                 TO  ACWA-DISPLAY-LEN-7           00188800
188900         MOVE ACWA-DISPLAY-LEN-7 TO OENTCTRO                      00188900
189000         MOVE -1                 TO  PERIODL                      00189000
189100         PERFORM 9010-000-SEND-DATAONLY-RETURN.                   00189100
189200                                                                  00189200
189300*    EXEC CICS GETMAIN                                            00189300
189400*              SET(ADDRESS OF COMMUNICATION-KEY-AREA)             00189400
189500*              INITIMG(WS-HEX-00)                                 00189500
189600*              LENGTH(WS-COMMUNICATION-KEY-LEN)                   00189600
189700*              END-EXEC.                                          00189700
189800                                                                  00189800
189900*    SET ACWA-COMM-KEY-PNTR  TO                                   00189900
190000*                      ADDRESS OF COMMUNICATION-KEY-AREA.         00190000
190100                                                                  00190100
190200     IF FRMNUIDI  =  'GS3A'                                       00190200
190300        MOVE 'G4'    TO  GCIO-WRK-RECORD-TYPE                     00190300
190400        MOVE SPACES  TO  GCA-L-O-B                                00190400
190500                         GCA-PROV-CTL                             00190500
190600                         GCA-BEN-PROV-ID.                         00190600
190700                                                                  00190700
190800     IF FRMNUIDI = 'GC4A' OR 'GTM1'                               00190800
190900        MOVE 'C3'                       TO  GCIO-WRK-RECORD-TYPE  00190900
191000        MOVE GCIO-WRK-LINE-OF-BUS       TO  GCA-L-O-B             00191000
191100        MOVE GCIO-WRK-PROVIDER-CONTROL  TO  GCA-PROV-CTL          00191100
191200        MOVE SPACES  TO  GCA-BEN-PROV-ID.                         00191200
191300                                                                  00191300
191400     IF FRMNUIDI  =  'GC8A'                                       00191400
191500        MOVE 'C6'                       TO  GCIO-WRK-RECORD-TYPE  00191500
191600        MOVE GCIO-WRK-LINE-OF-BUS       TO  GCA-L-O-B             00191600
191700        MOVE GCIO-WRK-PROVIDER-CONTROL  TO  GCA-PROV-CTL          00191700
191800        MOVE GCIO-WRK-PROVISION-ID      TO  GCA-BEN-PROV-ID       00191800
191900        MOVE GCIO-WRK-TABULAR-PROVISION TO                        00191900
192000                                     GCIO-WRK-BENEFIT-PROVISION.  00192000
192100                                                                  00192100
192200     MOVE GCIO-WRK-EFFDT-CEN       TO GCA-EFFDT-CEN.              00192200
192300     MOVE GCIO-WRK-EFFECTIVE-DATE  TO HGADATE-JULIAN1.            00192300
192400     PERFORM 9300-000-JULIAN-TO-GREGORIAN.                        00192400
192500     MOVE HGADATE-DATE2            TO GCA-EFFECTIVE-DATE.         00192500
192600     MOVE GC-GCPSWORK-DDNAME       TO GCIO2-FILE-DDNAME.          00192600
192700     MOVE GC-GCIO-AREA-1           TO GCIO2-IO-AREA-TO-USE.       00192700
192800     MOVE TABIDI                   TO GCA-ALL-LEVEL-TAB-ID.       00192800
192900     MOVE TABSLTNI                 TO ACWA-DISPLAY-LEN-7.         00192900
193000     MOVE ACWA-DISPLAY-LEN-7       TO GCA-ALL-LEVEL-TAB-SLOT.     00193000
193100     MOVE GXA-PROVISION-ID         TO GCIO-WRK-TAB-PROVISION-ID,  00193100
193200                                      GCA-INTERNAL-TAB-ID.        00193200
193300     MOVE GXA-INCLUDE-EXCLUDE-IND  TO GCA-I-E-INDC.               00193300
193400     MOVE GXA-PROVISION-SLOT-NO    TO SAVE-COPY-FROM-SLOT.        00193400
193500     MOVE GAC-OCCURS-ENTRY-COUNTER (GAC-INDEX)                    00193500
193600                                   TO GCIO-WRK-TAB-PROV-SLOT-NO   00193600
193700                                      GXA-PROVISION-SLOT-NO       00193700
193800                                      ACWA-DISPLAY-LEN-7.         00193800
193900     MOVE ACWA-DISPLAY-LEN-7       TO GCA-INTERNAL-TAB-SLOT       00193900
194000                                      GCA-OCCURS-ENTRY-COUNTER.   00194000
194100     MOVE 'A'                      TO GCA-ADD-DEL-IND.            00194100
194200     MOVE 'CHG/ADD'                TO DELADD-OPTION.              00194200
194300     MOVE FRMNUIDI                 TO GCA-FROM-MENU-ID.           00194300
194400     MOVE FUNCTONI                 TO GCA-ALL-LEVEL-TAB-FUNC-CODE.00194400
194500     MOVE GCIO-WRK-PLAN-CODE       TO GCA-PLAN-CODE.              00194500
194600     MOVE GCIO-WRK-GROUP-NUM       TO GCA-GROUP-NUM.              00194600
194700     MOVE GCIO-WRK-SECTION-NUM     TO GCA-SECTION-NUM.            00194700
194800     MOVE GCIO-WRK-PKG-CODE        TO GCA-PKG-CODE.               00194800
194900     MOVE GCIO-WRK-FAMILY-RELATION-LVL                            00194900
195000                                   TO GCA-FAM-REL-LVL.            00195000
195100     MOVE SPACES                   TO WORK-RECORD-2.              00195100
195200     MOVE GCIO-WORKFILE-KEY        TO GCIO2-FILE-KEY              00195200
195300                                      WORK-RECORD-KEY-2.          00195300
195400     MOVE SAVE-COPY-FROM-SLOT      TO WRK2-PROV-POOL-COPY-SLOT.   00195400
195500                                                                  00195500
195600     IF FRMNUIDI  =  'GC8A'                                       00195600
195700        MOVE GCIO-WRK-PROVISION-ID TO WRK2-ALL-LEV-BEN-PROV.      00195700
195800                                                                  00195800
195900     IF WRK-SIGNAL-FROM-ONLINE  =  'W'                            00195900
196000        MOVE 'W'       TO  WRK2-SIGNAL-FROM-ONLINE                00196000
196100        MOVE '1U'      TO  WRK2-CDE-SP                            00196100
196200        ADD   1        TO  ACWA-CDE-1U-COUNT                      00196200
196300     ELSE                                                         00196300
196400        IF  CDEINDO  = ('+CDE+'  OR '+CDE-')                      00196400
196500            MOVE '1U'   TO  WRK2-CDE-SP                           00196500
196600            ADD   1     TO  ACWA-CDE-1U-COUNT                     00196600
196700        ELSE                                                      00196700
196800            MOVE '2 '   TO  WRK2-CDE-SP                           00196800
196900            ADD   1     TO  ACWA-CDE-2B-COUNT.                    00196900
197000                                                                  00197000
197100** SET INDICATOR TO CAPTURE OPERATOR-ID.                          00197100
197200     MOVE '1'          TO  GCIO2-OPER-ID-IND.                     00197200
197300                                                                  00197300
197400     PERFORM 4700-000-UPDATE-CONTROL-RECORD.                      00197400
197500     MOVE GC-GCIO-ACCESS-CODE-WR   TO  GCIO2-FILE-ACCESS-CODE.    00197500
197600                                                                  00197600
197700     COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   00197700
197800              GC-GCIOPARM-LEN  +  GCIO2-RECORD-LENGTH.            00197800
197900                                                                  00197900
198000     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00198000
198100                COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          00198100
198200                LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. 00198200
198300                                                                  00198300
198400     IF NOT GCIO2-GOOD-RETURN                                     00198400
198500        MOVE WS-ABCODE-1DF6        TO WS-ABCODE                   00198500
198600        MOVE WS-ABCODE-1DF6-MSG    TO WS-ABCODE-MSG               00198600
198700        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00198700
198800                                                                  00198800
198900     SET GCA-RECORD-POINTER  TO                                   00198900
199000                    ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD.    00199000
199100                                                                  00199100
199200     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00199200
199300                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00199300
199400     EXEC CICS  XCTL  PROGRAM(WS-INTERNAL-TABULAR-PGM-ID)         00199400
199500                COMMAREA(COMMON-WORKAREAS)                        00199500
199600                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00199600
199700                                                                  00199700
199800 2100-900-EXIT.                                                   00199800
199900          EXIT.                                                   00199900
200000/*****************************************************************00200000
200100* 2500  ADD NEW OCCURS                                           *00200100
200200*                                                                *00200200
200300*     INITIALIZE ENTRY IN THE TABLE TO EITHER SPACES OR ZEROS,   *00200300
200400*  THEN IF A FIELD WAS ENTERED BY THE OPERATOR MOVE IT TO THE    *00200400
200500*  TABLE IN THE RECORD.                                          *00200500
200600******************************************************************00200600
200700 2500-000-ADD-NEW-OCCURS        SECTION.                          00200700
200800 2500-010.                                                        00200800
200900                                                                  00200900
201000     MOVE GAC-ENTRY-COUNT  TO GAC-ENTRY-COUNT.                    00201000
201100     SET GAC-INDEX         TO GAC-ENTRY-COUNT.                    00201100
201200     MOVE LOW-VALUES       TO GAC-ENTRY (GAC-INDEX).              00201200
201300                                                                  00201300
201400     MOVE ZEROS TO GAC-DEDL-INTL-TAB-SLOT-2 (GAC-INDEX),          00201400
201500                   GAC-DEDL-INTL-TAB-SLOT-3 (GAC-INDEX),          00201500
201600                   GAC-DEDL-INTL-TAB-SLOT-4 (GAC-INDEX),          00201600
201700                   GAC-DEDL-INTL-TAB-SLOT-5 (GAC-INDEX).          00201700
201800                                                                  00201800
201900     MOVE SPACES TO GAC-DEDL-INTL-TAB-TAB-ID-2 (GAC-INDEX)        00201900
202000                    GAC-DEDL-INTL-TAB-TAB-ID-3 (GAC-INDEX)        00202000
202100                    GAC-DEDL-INTL-TAB-TAB-ID-4 (GAC-INDEX)        00202100
202200                    GAC-DEDL-INTL-TAB-TAB-ID-5 (GAC-INDEX).       00202200
202300                                                                  00202300
202400     MOVE GAC-OCC-ENTRY-TAB-SLOT-CNTR                             00202400
202500                    TO GAC-OCCURS-ENTRY-COUNTER       (GAC-INDEX).00202500
202600     ADD 1          TO GAC-OCC-ENTRY-TAB-SLOT-CNTR.               00202600
202700     MOVE DAYFACII  TO GAC-DEDL-DAY-FACTOR-IND        (GAC-INDEX).00202700
202800     MOVE COPAYINI  TO GAC-DEDL-CO-PAY-IND            (GAC-INDEX).00202800
202900     MOVE BISNDINI  TO GAC-DEDL-BISCENDING-IND-RSV    (GAC-INDEX).00202900
203000     MOVE ASCDSCDI  TO GAC-DEDL-ASCEND-DESCEND-IND    (GAC-INDEX).00203000
      *P21595 CHANGES STARTS                                            00203010
           MOVE BENTYPI   TO GAC-DEDL-BEN-TYPE              (GAC-INDEX).00203020
           MOVE TIERCDI   TO GAC-DEDL-TIER-CODE             (GAC-INDEX).00203030
           MOVE TIERLVI   TO GAC-DEDL-TIER-LVL              (GAC-INDEX).00203040
      *P21595 CHANGES ENDS                                              00203050
203100     MOVE DEFINTNI  TO GAC-DEDL-DEFINITION            (GAC-INDEX).00203100
203200     MOVE MANAPLII  TO GAC-DEDL-MANDATORY-IND         (GAC-INDEX).00203200
203300     MOVE CARYOVRI  TO GAC-CARRY-OVER-CREDIT-IND      (GAC-INDEX).00203300
203400     MOVE FYIVALI   TO GAC-DEDL-FYI-VALUE             (GAC-INDEX).00203400
203500     MOVE CSTCONTI  TO GAC-DEDL-COST-CONTAIN-IND      (GAC-INDEX).00203500
203600     MOVE PERIODI   TO GAC-DEDL-BENEFIT-PERIOD        (GAC-INDEX).00203600
203700     MOVE PERTQALI  TO GAC-DEDL-BEN-PER-TIME-QUAL     (GAC-INDEX).00203700
203800     MOVE FAMINDII  TO GAC-DEDL-FAM-OR-INDIV          (GAC-INDEX).00203800
203900     MOVE PLCTRMTI  TO GAC-DEDL-PLACE-OF-TREATMENT    (GAC-INDEX).00203900
204000     MOVE SRVGRUPI  TO GAC-DEDL-SERVICE-GROUP         (GAC-INDEX).00204000
204100     MOVE AGELIMLI  TO ACWA-DISPLAY-LEN-3-X.                      00204100
204200     MOVE ACWA-DISPLAY-LEN-3                                      00204200
204300                    TO GAC-DEDL-AGE-LIMIT-FROM        (GAC-INDEX).00204300
204400     MOVE AGELIMHI  TO ACWA-DISPLAY-LEN-3-X.                      00204400
204500     MOVE ACWA-DISPLAY-LEN-3                                      00204500
204600                    TO GAC-DEDL-AGE-LIMIT-TO          (GAC-INDEX).00204600
204700     MOVE FEAKINDI  TO GAC-DEDL-FEAK-IND              (GAC-INDEX).00204700
204800     MOVE ACCUMIDI  TO GAC-DEDL-ACCUMID               (GAC-INDEX).00204800
204900     MOVE CAPINDI   TO GAC-DEDL-COMB-APPLIED-IND      (GAC-INDEX).00204900
205000     MOVE SABDINDI  TO GAC-DEDL-SEL-ADDL-BEN-DET      (GAC-INDEX).00205000
205100     MOVE AGEQLLI   TO GAC-DEDL-AGE-QUAL-IND-FROM     (GAC-INDEX).00205100
205200     MOVE AGEQLHI   TO GAC-DEDL-AGE-QUAL-IND-TO       (GAC-INDEX).00205200
205300     MOVE RELPINDI  TO GAC-DEDL-RELATIONSHIP-IND      (GAC-INDEX).00205300
205400                                                                  00205400
205500     MOVE PRTIMEFI  TO ACWA-DISPLAY-LEN-3-X.                      00205500
205600     MOVE ACWA-DISPLAY-LEN-3                                      00205600
205700                    TO GAC-DEDL-BEN-PER-TIME-FCTR     (GAC-INDEX).00205700
205800     MOVE CLMLVLII  TO GAC-DEDL-CLAIM-LVL-ACCUM-IND   (GAC-INDEX).00205800
205900     MOVE INTRVALI  TO ACWA-DISPLAY-LEN-3-X.                      00205900
206000     MOVE ACWA-DISPLAY-LEN-3                                      00206000
206100                    TO GAC-DEDL-INTERVAL-TIME-FCTR    (GAC-INDEX).00206100
206200     MOVE INTTYPEI  TO GAC-DEDL-INTERVAL-TYPE         (GAC-INDEX).00206200
206300     MOVE LOBI      TO GAC-DEDL-L-O-B                 (GAC-INDEX).00206300
206400                                                                  00206400
206500     PERFORM 2600-000-PROCESS-VAL-LIMIT.                          00206500
206600                                                                  00206600
206700     MOVE ACWA-VALUE-LIMIT-9                                      00206700
206800                    TO GAC-DEDL-VALUE-LIMIT           (GAC-INDEX).00206800
206900     MOVE BENVLQLI  TO GAC-DEDL-VALUE-QUALIFIER       (GAC-INDEX).00206900
207000     MOVE NEWVALUI  TO ACWA-DISPLAY-LEN-5-X.                      00207000
207100     MOVE ACWA-DISPLAY-LEN-5                                      00207100
207200                    TO GAC-DEDL-INTERVAL-OVRD-VALUE   (GAC-INDEX).00207200
207300     MOVE OVRDINDI  TO GAC-DEDL-INTERVAL-OVRD-IND     (GAC-INDEX).00207300
207400     MOVE INTDESKI  TO GAC-DEDL-INTERNAL-DESCRIPTOR   (GAC-INDEX).00207400
207500     MOVE CONDALLI  TO GAC-COND-ALL-BIT               (GAC-INDEX).00207500
207600     MOVE CONDEXCI  TO GAC-COND-EXCLUSION-BIT         (GAC-INDEX).00207600
207700     MOVE CONDICDI  TO GAC-COND-ICD-BIT               (GAC-INDEX).00207700
207800     MOVE CONDTABI  TO GAC-COND-TB-BIT                (GAC-INDEX).00207800
207900     MOVE CONDMENI  TO GAC-COND-MENTAL-BIT            (GAC-INDEX).00207900
208000     MOVE CONDDRGI  TO GAC-COND-DRUG-BIT              (GAC-INDEX).00208000
208100     MOVE CONDALCI  TO GAC-COND-ALCOHOL-BIT           (GAC-INDEX).00208100
208200     MOVE CONDOBCI  TO GAC-COND-OB-COMP-BIT           (GAC-INDEX).00208200
208300     MOVE CONDOBNI  TO GAC-COND-OB-NORM-BIT           (GAC-INDEX).00208300
208400     MOVE CONDMALI  TO GAC-COND-MALIGNANCY-BIT        (GAC-INDEX).00208400
208500     MOVE CONDCARI  TO GAC-COND-CARDIAC-DISEASE-BIT   (GAC-INDEX).00208500
208600     MOVE CONDOBSI  TO GAC-COND-OBESITY-BIT           (GAC-INDEX).00208600
208700     MOVE CONDKDYI  TO GAC-COND-KIDNEY-DISEASE-BIT    (GAC-INDEX).00208700
208800     MOVE CONDACCI  TO GAC-COND-ACCIDENT-BIT          (GAC-INDEX).00208800
208900     MOVE CONDPECI  TO GAC-COND-PRE-EXIST-BIT         (GAC-INDEX).00208900
209000     MOVE CONDNEMI  TO GAC-COND-NON-EMER-BIT          (GAC-INDEX).00209000
209100     MOVE CONDSUII  TO GAC-COND-SUICIDE-BIT           (GAC-INDEX).00209100
209200     MOVE CONDTMJI  TO GAC-COND-TMJ-BIT               (GAC-INDEX).00209200
209300     MOVE CONDINFI  TO GAC-COND-INF-BIT               (GAC-INDEX).00209300
209400     MOVE CONDLIFI  TO GAC-COND-LIFE-THREAT-BIT       (GAC-INDEX).00209400
209500     MOVE CONDEMCI  TO GAC-COND-EMER-MED-BIT          (GAC-INDEX).00209500
209600     MOVE CONDEACI  TO GAC-COND-EMER-ACC-BIT          (GAC-INDEX).00209600
209700     MOVE CONDSMII  TO GAC-COND-SER-MEN-ILL-BIT       (GAC-INDEX).00209700
209800     MOVE CONDNSMI  TO GAC-COND-NON-SER-MEN-ILL-BIT   (GAC-INDEX).00209800
209900                                                                  00209900
210000                                                                  00210000
210100     IF  (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE) AND           00210100
210200         (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE) AND           00210200
210300         (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE) AND           00210300
210400         (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE) AND           00210400
210500         (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE) AND           00210500
210600         (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE)               00210600
210700     THEN                                                         00210700
210800        MOVE 1               TO GAC-INTERNAL-TABULAR-COUNT        00210800
210900                                                       (GAC-INDEX)00210900
211000        MOVE HIGH-VALUES     TO GAC-DEDL-INTL-TAB-1    (GAC-INDEX)00211000
211100        SET  GAC-INDEX  UP BY  1                                  00211100
211200        MOVE HIGH-VALUES     TO GAC-ENTRY (GAC-INDEX)             00211200
211300        SET GAC-ENTRY-COUNT  TO GAC-INDEX                         00211300
211400        GO TO 2500-900-EXIT.                                      00211400
211500                                                                  00211500
211600     IF IBGROPTI  =  'MT' OR 'A'                                  00211600
211700        MOVE 2           TO GAC-INTERNAL-TABULAR-COUNT (GAC-INDEX)00211700
211800        MOVE HIGH-VALUES TO GAC-DEDL-INTL-TAB-2        (GAC-INDEX)00211800
211900        MOVE '#IBGR '    TO GAC-DEDL-INTL-TAB-TAB-ID-1 (GAC-INDEX)00211900
212000        MOVE GAC-OCCURS-ENTRY-COUNTER                  (GAC-INDEX)00212000
212100                         TO GAC-DEDL-INTL-TAB-SLOT-1  (GAC-INDEX).00212100
212200                                                                  00212200
212300     IF IDGDOPTI  =  'MT' OR 'A'                                  00212300
212400        MOVE 2           TO GAC-INTERNAL-TABULAR-COUNT (GAC-INDEX)00212400
212500        MOVE HIGH-VALUES TO GAC-DEDL-INTL-TAB-2        (GAC-INDEX)00212500
212600        MOVE '#IDGD '    TO GAC-DEDL-INTL-TAB-TAB-ID-1 (GAC-INDEX)00212600
212700        MOVE GAC-OCCURS-ENTRY-COUNTER                  (GAC-INDEX)00212700
212800                         TO GAC-DEDL-INTL-TAB-SLOT-1  (GAC-INDEX).00212800
212900                                                                  00212900
213000     IF IPGNOPTI  =  'MT' OR 'A'                                  00213000
213100        MOVE 2           TO GAC-INTERNAL-TABULAR-COUNT (GAC-INDEX)00213100
213200        MOVE HIGH-VALUES TO GAC-DEDL-INTL-TAB-2        (GAC-INDEX)00213200
213300        MOVE '#IPGN '    TO GAC-DEDL-INTL-TAB-TAB-ID-1 (GAC-INDEX)00213300
213400        MOVE GAC-OCCURS-ENTRY-COUNTER                  (GAC-INDEX)00213400
213500                         TO GAC-DEDL-INTL-TAB-SLOT-1  (GAC-INDEX).00213500
213600                                                                  00213600
213700     IF IPGPOPTI  =  'MT' OR 'A'                                  00213700
213800        MOVE 2           TO GAC-INTERNAL-TABULAR-COUNT (GAC-INDEX)00213800
213900        MOVE HIGH-VALUES TO GAC-DEDL-INTL-TAB-2        (GAC-INDEX)00213900
214000        MOVE '#IPGP '    TO GAC-DEDL-INTL-TAB-TAB-ID-1 (GAC-INDEX)00214000
214100        MOVE GAC-OCCURS-ENTRY-COUNTER                  (GAC-INDEX)00214100
214200                         TO GAC-DEDL-INTL-TAB-SLOT-1  (GAC-INDEX).00214200
214300                                                                  00214300
214400     IF IPGTOPTI  =  'MT' OR 'A'                                  00214400
214500        MOVE 2           TO GAC-INTERNAL-TABULAR-COUNT (GAC-INDEX)00214500
214600        MOVE HIGH-VALUES TO GAC-DEDL-INTL-TAB-2        (GAC-INDEX)00214600
214700        MOVE '#IPGT '    TO GAC-DEDL-INTL-TAB-TAB-ID-1 (GAC-INDEX)00214700
214800        MOVE GAC-OCCURS-ENTRY-COUNTER                  (GAC-INDEX)00214800
214900                         TO GAC-DEDL-INTL-TAB-SLOT-1  (GAC-INDEX).00214900
215000                                                                  00215000
215100     IF IPGSOPTI  =  'MT' OR 'A'                                  00215100
215200        MOVE 2           TO GAC-INTERNAL-TABULAR-COUNT (GAC-INDEX)00215200
215300        MOVE HIGH-VALUES TO GAC-DEDL-INTL-TAB-2        (GAC-INDEX)00215300
215400        MOVE '#IPGS '    TO GAC-DEDL-INTL-TAB-TAB-ID-1 (GAC-INDEX)00215400
215500        MOVE GAC-OCCURS-ENTRY-COUNTER                  (GAC-INDEX)00215500
215600                         TO GAC-DEDL-INTL-TAB-SLOT-1  (GAC-INDEX).00215600
215700                                                                  00215700
215800                                                                  00215800
215900*******                                                           00215900
216000* STS *==> MAP FROM OPTION UNDER SINGLE TABULAR SUPPORT DOES NOT  00216000
216100*     *     CREATE INTERNAL TAB ON WORKFILE, IT SIMPLY UPDATES THE00216100
216200*     *     THE ACCUM RECORD AND SCREEN, THEN REDISPLAYS SCREEN.  00216200
216300*******                                                           00216300
216400                                                                  00216400
216500     IF  FRMNUIDI =  'GTM1'  AND                                  00216500
216600         IBGROPTI =  'MT'                                         00216600
216700     THEN                                                         00216700
216800         MOVE MFRMSLTI  TO  IBGRSLTI  ACWA-DISPLAY-LEN-7          00216800
216900         SET  WT-01-INDEX                     TO  +01             00216900
217000         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        00217000
217100         MOVE -1        TO  IBGROPTL                              00217100
217200         MOVE DFHBMABF  TO  IBGRSLTA   IBGRIDA                    00217200
217300         MOVE SPACES    TO  IBGROPTI                              00217300
217400         MOVE DFHBMUNP  TO  MFRMSLTA                              00217400
217500         MOVE ZEROS     TO  MFRMSLTI   MFRMSLTL                   00217500
217600         MOVE ACWA-DISPLAY-LEN-7                                  00217600
217700                        TO  GAC-DEDL-INTL-TAB-SLOT-1 (GAC-INDEX)  00217700
217800     ELSE                                                         00217800
217900         NEXT SENTENCE.                                           00217900
218000                                                                  00218000
218100     IF  FRMNUIDI =  'GTM1'  AND                                  00218100
218200         IDGDOPTI =  'MT'                                         00218200
218300     THEN                                                         00218300
218400         MOVE MFRMSLTI  TO  IDGDSLTI  ACWA-DISPLAY-LEN-7          00218400
218500         SET  WT-01-INDEX                     TO  +04             00218500
218600         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        00218600
218700         MOVE -1        TO  IDGDOPTL                              00218700
218800         MOVE DFHBMABF  TO  IDGDSLTA   IDGDIDA                    00218800
218900         MOVE SPACES    TO  IDGDOPTI                              00218900
219000         MOVE DFHBMUNP  TO  MFRMSLTA                              00219000
219100         MOVE ZEROS     TO  MFRMSLTI   MFRMSLTL                   00219100
219200         MOVE ACWA-DISPLAY-LEN-7                                  00219200
219300                        TO  GAC-DEDL-INTL-TAB-SLOT-1 (GAC-INDEX)  00219300
219400     ELSE                                                         00219400
219500         NEXT SENTENCE.                                           00219500
219600                                                                  00219600
219700     IF  FRMNUIDI =  'GTM1'  AND                                  00219700
219800         IPGNOPTI =  'MT'                                         00219800
219900     THEN                                                         00219900
220000         MOVE MFRMSLTI  TO  IPGNSLTI  ACWA-DISPLAY-LEN-7          00220000
220100         SET  WT-01-INDEX                     TO  +02             00220100
220200         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        00220200
220300         MOVE -1        TO  IPGNOPTL                              00220300
220400         MOVE DFHBMABF  TO  IPGNSLTA  IPGNIDA                     00220400
220500         MOVE SPACES    TO  IPGNOPTI                              00220500
220600         MOVE DFHBMUNP  TO  MFRMSLTA                              00220600
220700         MOVE ZEROS     TO  MFRMSLTI  MFRMSLTL                    00220700
220800         MOVE ACWA-DISPLAY-LEN-7                                  00220800
220900                        TO  GAC-DEDL-INTL-TAB-SLOT-1 (GAC-INDEX)  00220900
221000     ELSE                                                         00221000
221100         NEXT SENTENCE.                                           00221100
221200                                                                  00221200
221300     IF  FRMNUIDI =  'GTM1'  AND                                  00221300
221400         IPGPOPTI =  'MT'                                         00221400
221500     THEN                                                         00221500
221600         MOVE MFRMSLTI  TO  IPGPSLTI  ACWA-DISPLAY-LEN-7          00221600
221700         SET  WT-01-INDEX                     TO  +05             00221700
221800         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        00221800
221900         MOVE -1        TO  IPGPOPTL                              00221900
222000         MOVE DFHBMABF  TO  IPGPSLTA   IDGDIDA                    00222000
222100         MOVE SPACES    TO  IPGPOPTI                              00222100
222200         MOVE DFHBMUNP  TO  MFRMSLTA                              00222200
222300         MOVE ZEROS     TO  MFRMSLTI   MFRMSLTL                   00222300
222400         MOVE ACWA-DISPLAY-LEN-7                                  00222400
222500                        TO  GAC-DEDL-INTL-TAB-SLOT-1 (GAC-INDEX)  00222500
222600     ELSE                                                         00222600
222700         NEXT SENTENCE.                                           00222700
222800                                                                  00222800
222900     IF  FRMNUIDI =  'GTM1'  AND                                  00222900
223000         IPGTOPTI =  'MT'                                         00223000
223100     THEN                                                         00223100
223200         MOVE MFRMSLTI  TO  IPGTSLTI  ACWA-DISPLAY-LEN-7          00223200
223300         SET  WT-01-INDEX                     TO  +03             00223300
223400         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        00223400
223500         MOVE -1        TO  IPGTOPTL                              00223500
223600         MOVE DFHBMABF  TO  IPGTSLTA  IPGTIDA                     00223600
223700         MOVE SPACES    TO  IPGTOPTI                              00223700
223800         MOVE DFHBMUNP  TO  MFRMSLTA                              00223800
223900         MOVE ZEROS     TO  MFRMSLTI  MFRMSLTL                    00223900
224000         MOVE ACWA-DISPLAY-LEN-7                                  00224000
224100                        TO  GAC-DEDL-INTL-TAB-SLOT-1 (GAC-INDEX)  00224100
224200     ELSE                                                         00224200
224300         NEXT SENTENCE.                                           00224300
224400                                                                  00224400
224500     IF  FRMNUIDI =  'GTM1'  AND                                  00224500
224600         IPGSOPTI =  'MT'                                         00224600
224700     THEN                                                         00224700
224800         MOVE MFRMSLTI  TO  IPGSSLTI  ACWA-DISPLAY-LEN-7          00224800
224900         SET  WT-01-INDEX                     TO  +23             00224900
225000         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        00225000
225100         MOVE -1        TO  IPGSOPTL                              00225100
225200         MOVE DFHBMABF  TO  IPGSSLTA  IPGSIDA                     00225200
225300         MOVE SPACES    TO  IPGSOPTI                              00225300
225400         MOVE DFHBMUNP  TO  MFRMSLTA                              00225400
225500         MOVE ZEROS     TO  MFRMSLTI  MFRMSLTL                    00225500
225600         MOVE ACWA-DISPLAY-LEN-7                                  00225600
225700                        TO  GAC-DEDL-INTL-TAB-SLOT-1 (GAC-INDEX)  00225700
225800     ELSE                                                         00225800
225900         NEXT SENTENCE.                                           00225900
226000*******                                                          |00226000
226100* STS *----------------------------------------------------------*00226100
226200*******                                                           00226200
226300                                                                  00226300
226400                                                                  00226400
226500     SET  GAC-INDEX  UP BY  1.                                    00226500
226600     MOVE HIGH-VALUES     TO  GAC-ENTRY (GAC-INDEX).              00226600
226700     SET GAC-ENTRY-COUNT  TO  GAC-INDEX.                          00226700
226800                                                                  00226800
226900 2500-900-EXIT. EXIT.                                             00226900
227000                                                                  00227000
227100/*****************************************************************00227100
227200*     P R O C E S S   V A L U E   L I M I T                       00227200
227300******************************************************************00227300
227400 2600-000-PROCESS-VAL-LIMIT     SECTION.                          00227400
227500 2600-010.                                                        00227500
227600                                                                  00227600
227700     IF (ACWA-VAL-LIM-SCREEN-NEG1-3  =  'NEG' OR                  00227700
227800         ACWA-VAL-LIM-SCREEN-NEG2-3  =  'NEG') OR                 00227800
227700        (ACWA-VAL-LIM-SCREEN-NEG1-3  =  'UNL' OR                  00227810
227800         ACWA-VAL-LIM-SCREEN-NEG2-3  =  'UNL')                    00227820
227900         GO TO 2600-900-EXIT.                                     00227900
228000                                                                  00228000
228100     IF  ACWA-BNMXVALI-N NUMERIC                                  00228100
228200     THEN                                                         00228200
228300         IF  BENVLQLI  = '5'                                      00228300
228400         THEN                                                     00228400
228500             MOVE ACWA-BNMXVALI-N  TO  ACWA-VALUE-LIMIT-7         00228500
228600             MOVE ZEROS            TO  ACWA-VALUE-LIMIT-2         00228600
228700             GO TO 2600-900-EXIT                                  00228700
228800         ELSE                                                     00228800
228900             MOVE ACWA-BNMXVALI-N  TO  ACWA-VALUE-LIMIT-9-9       00228900
229000             GO TO 2600-900-EXIT                                  00229000
229100     ELSE                                                         00229100
229200         NEXT SENTENCE.                                           00229200
229300                                                                  00229300
229400     IF  ACWA-VAL-LIM-SCREEN-1  =  '.'                            00229400
229500         MOVE ACWA-VAL-LIM-SCREEN-7  TO  ACWA-VALUE-LIMIT-7       00229500
229600         MOVE ACWA-VAL-LIM-SCREEN-2  TO  ACWA-VALUE-LIMIT-2       00229600
229700         GO TO 2600-900-EXIT.                                     00229700
229800                                                                  00229800
229900 2600-900-EXIT. EXIT.                                             00229900
230000                                                                  00230000
230100/*****************************************************************00230100
230200* 3000 UPDATE GAC RECORD                                         *00230200
230300*                                                                *00230300
230400*    THIS ROUTINE REWRITES THE UPDATED RECORD TO THE WORK FILE.  *00230400
230500******************************************************************00230500
230600 3000-000-UPDATE-GAC-RECORD     SECTION.                          00230600
230700 3000-010.                                                        00230700
230800                                                                  00230800
230900     COMPUTE  GCIO-RECORD-LENGTH   =                              00230900
231000         GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-ADL-FIXED-LEN  +     00231000
231100         (GC-GCTABULR-ADL-VARY-LEN * GAC-ENTRY-COUNT).            00231100
231200                                                                  00231200
231300     MOVE  GC-GCIO-ACCESS-CODE-WU  TO  GCIO-FILE-ACCESS-CODE.     00231300
231400                                                                  00231400
231500     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00231500
231600                COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           00231600
231700                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)  END-EXEC.     00231700
231800                                                                  00231800
231900     IF NOT GCIO-GOOD-RETURN                                      00231900
232000        MOVE WS-ABCODE-1DF4        TO WS-ABCODE                   00232000
232100        MOVE WS-ABCODE-1DF4-MSG    TO WS-ABCODE-MSG               00232100
232200        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00232200
232300                                                                  00232300
232400 3000-900-EXIT. EXIT.                                             00232400
232500                                                                  00232500
232600/*****************************************************************00232600
232700* 3100  READ RECORD                                              *00232700
232800*                                                                *00232800
232900*    THIS ROUTINE READS THE RECORD THAT CORRESPONDS TO THE KEY   *00232900
233000*  FIELDS FOUND ON THE SCREEN'S HEADING.                         *00233000
233100******************************************************************00233100
233200 3100-000-READ-RECORD           SECTION.                          00233200
233300 3100-010.                                                        00233300
233400                                                                  00233400
233500     COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =  GC-GCIOPARM-LEN  +   00233500
233600              GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-ADL-FIXED-LEN  +00233600
233700             (GC-GCTABULR-ADL-VARY-LEN  *                         00233700
233800                                   GC-GCTABULR-ADL-VARY-MAX-OCUR).00233800
233900                                                                  00233900
234000     IF ACWA-WF-ALL-LEVEL-TAB-COMP  >  ZERO                       00234000
234100        NEXT SENTENCE                                             00234100
234200     ELSE                                                         00234200
234300        EXEC CICS GETMAIN                                         00234300
234400               SET(ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD)      00234400
234500               INITIMG(WS-HEX-00)                                 00234500
234600               LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                 00234600
234700               END-EXEC                                           00234700
234800        SET ACWA-WF-ALL-LEVEL-TAB-PNTR     TO                     00234800
234900                 ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD.        00234900
235000                                                                  00235000
235100     IF FRMNUIDI  =  'GS3A'                                       00235100
235200        PERFORM 6000-000-BUILD-GROUP-SPEC-KEY.                    00235200
235300     IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            00235300
235400        PERFORM 6100-000-BUILD-CONTRACT-KEY.                      00235400
235500     IF FRMNUIDI  =  'GC8A'                                       00235500
235600        PERFORM 6200-000-BUILD-BEN-PROV-KEY.                      00235600
235700                                                                  00235700
235800     IF GCIO-WORKFILE-KEY  =  WORK-RECORD-KEY                     00235800
235900        GO TO 3100-900-EXIT.                                      00235900
236000                                                                  00236000
236100     MOVE GC-GCPSWORK-DDNAME     TO GCIO-FILE-DDNAME.             00236100
236200     MOVE GC-GCIO-AREA-1         TO GCIO-IO-AREA-TO-USE.          00236200
236300     MOVE GCIO-WORKFILE-KEY      TO GCIO-FILE-KEY.                00236300
236400     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO-FILE-ACCESS-CODE.        00236400
236500     MOVE GC-GCTABULR-ADL-VARY-MAX-OCUR  TO  GAC-ENTRY-COUNT.     00236500
236600                                                                  00236600
236700     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00236700
236800                COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           00236800
236900                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)     END-EXEC.  00236900
237000                                                                  00237000
237100     IF NOT GCIO-GOOD-RETURN                                      00237100
237200        MOVE WS-ABCODE-1DFJ        TO WS-ABCODE                   00237200
237300        MOVE WS-ABCODE-1DFJ-MSG    TO WS-ABCODE-MSG               00237300
237400        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00237400
237500                                                                  00237500
237600 3100-900-EXIT. EXIT.                                             00237600
237700                                                                  00237700
237800/*****************************************************************00237800
237900* 3200  READ REC FOR UPDATE                                      *00237900
238000*                                                                *00238000
238100*    THIS ROUTINE READS THE RECORD FOR UPDATE THAT CORRESPONDS   *00238100
238200*  TO THE KEY FIELDS FOUND ON THE SCREEN'S HEADING.              *00238200
238300******************************************************************00238300
238400 3200-000-READ-REC-FOR-UPDATE   SECTION.                          00238400
238500 3200-010.                                                        00238500
238600                                                                  00238600
238700     COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =  GC-GCIOPARM-LEN  +   00238700
238800              GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-ADL-FIXED-LEN  +00238800
238900             (GC-GCTABULR-ADL-VARY-LEN  *                         00238900
239000                                   GC-GCTABULR-ADL-VARY-MAX-OCUR).00239000
239100                                                                  00239100
239200     IF ACWA-WF-ALL-LEVEL-TAB-COMP  >  ZERO                       00239200
239300        NEXT SENTENCE                                             00239300
239400     ELSE                                                         00239400
239500        EXEC CICS GETMAIN                                         00239500
239600               SET(ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD)      00239600
239700               INITIMG(WS-HEX-00)                                 00239700
239800               LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                 00239800
239900               END-EXEC                                           00239900
240000        SET ACWA-WF-ALL-LEVEL-TAB-PNTR     TO                     00240000
240100                 ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD.        00240100
240200                                                                  00240200
240300                                                                  00240300
240400     IF FRMNUIDI  =  'GS3A'                                       00240400
240500        PERFORM 6000-000-BUILD-GROUP-SPEC-KEY.                    00240500
240600     IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            00240600
240700        PERFORM 6100-000-BUILD-CONTRACT-KEY.                      00240700
240800     IF FRMNUIDI  =  'GC8A'                                       00240800
240900        PERFORM 6200-000-BUILD-BEN-PROV-KEY.                      00240900
241000                                                                  00241000
241100     MOVE GC-GCPSWORK-DDNAME     TO GCIO-FILE-DDNAME.             00241100
241200     MOVE GC-GCIO-AREA-1         TO GCIO-IO-AREA-TO-USE.          00241200
241300     MOVE GCIO-WORKFILE-KEY      TO GCIO-FILE-KEY.                00241300
241400     MOVE GC-GCIO-ACCESS-CODE-RU TO GCIO-FILE-ACCESS-CODE.        00241400
241500     MOVE GC-GCTABULR-ADL-VARY-MAX-OCUR  TO  GAC-ENTRY-COUNT.     00241500
241600                                                                  00241600
241700     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00241700
241800                COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           00241800
241900                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)  END-EXEC.     00241900
242000                                                                  00242000
242100 3200-900-EXIT. EXIT.                                             00242100
242200                                                                  00242200
242300/*****************************************************************00242300
242400* 4000  DISPLAY FIRST SCREEN                                     *00242400
242500*                                                                *00242500
242600*    THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM ONE OF THE*00242600
242700*  TABULAR MENUS, THE MENU WILL READ THE ALL LEVEL TABULAR IF IT *00242700
242800*  EXISTS (IF IT DOESN'T EXIST THE MENU WILL ADD A NEW ONE TO THE*00242800
242900*  WORK FILE) THEN PLACE THE ADDRESS OF THE TABULAR RECORD WITHIN*00242900
243000*  A COMMON AREA PARAMETER LIST.  THE MENU THEN MOVES THE KEY    *00243000
243100*  FIELDS TO THE COMMON AREA AND PASSES THE ADDRESS OF THE       *00243100
243200*  PARAMETER LIST IN A FULLWORD TO THIS PROGRAM.                 *00243200
243300*    WE THEN SET THIS ADDRESS INTO A BLL CELL AND ACCESS THE     *00243300
243400*  INFORMATION NEEDED TO BUILD THE SCREEN IMAGE.                 *00243400
243500******************************************************************00243500
243600 4000-000-DISPLAY-FIRST-SCREEN  SECTION.                          00243600
243700 4000-010.                                                        00243700
243800                                                                  00243800
243900     IF EIBCALEN  >  0                                            00243900
244000        NEXT SENTENCE                                             00244000
244100     ELSE                                                         00244100
244200        MOVE WS-ABCODE-1DC1        TO WS-ABCODE                   00244200
244300        MOVE WS-ABCODE-1DC1-MSG    TO WS-ABCODE-MSG               00244300
244400        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00244400
244500                                                                  00244500
244600*    SET ADDRESS OF COMMUNICATION-KEY-AREA  TO                    00244600
244700*                   COMMAREA-RECORD-POINTER.                      00244700
244800                                                                  00244800
244900*    SET  ACWA-COMM-KEY-PNTR       TO                             00244900
245000*                   ADDRESS OF COMMUNICATION-KEY-AREA.            00245000
245100     MOVE LOW-VALUES               TO GA1XI01I.                   00245100
245200                                                                  00245200
245300     MOVE GCA-FROM-MENU-ID  TO  FRMNUIDO.                         00245300
245400                                                                  00245400
245500     IF GCA-FROM-MENU-ID  =  'GS3A'                               00245500
245600        MOVE GCA-PLAN-CODE             TO GRP-SPEC-PLAN-CODE      00245600
245700        MOVE GCA-GROUP-NUM             TO GRP-SPEC-GROUP-NUM      00245700
245800        MOVE GCA-SECTION-NUM           TO GRP-SPEC-SECTION-NUM    00245800
245900        MOVE GCA-PKG-CODE              TO GRP-SPEC-PKG-CODE       00245900
246000        MOVE GCA-FAM-REL-LVL           TO GRP-SPEC-FAM-REL-LVL    00246000
246100        MOVE GCA-EFFECTIVE-DATE        TO GRP-SPEC-EFF-DATE       00246100
246200        MOVE ' GROUP SPEC. ALL-LEVEL TABULAR MAINTENANCE'         00246200
246300                                       TO TTLELNEO                00246300
246400*AB*****MOVE GROUP-SPECIFIC-TITLE-LINE TO TTLELNEO                00246400
246500        MOVE GROUP-SPECIFIC-ID-LINE    TO IDLINEO.                00246500
246600                                                                  00246600
246700     IF GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                     00246700
246800        MOVE GCA-PLAN-CODE             TO CONTRACT-PLAN-CODE      00246800
246900        MOVE GCA-GROUP-NUM             TO CONTRACT-GROUP-NUM      00246900
247000        MOVE GCA-SECTION-NUM           TO CONTRACT-SECTION-NUM    00247000
247100        MOVE GCA-PKG-CODE              TO CONTRACT-PKG-CODE       00247100
247200        MOVE GCA-L-O-B                 TO CONTRACT-LOB            00247200
247300        MOVE GCA-PROV-CTL              TO CONTRACT-PROV-CTL       00247300
247400        MOVE GCA-FAM-REL-LVL           TO CONTRACT-FAM-REL-LVL    00247400
247500        MOVE GCA-EFFECTIVE-DATE        TO CONTRACT-EFF-DATE       00247500
247600        MOVE '   CONTRACT ALL-LEVEL TABULAR MAINTENANCE'          00247600
247700                                       TO TTLELNEO                00247700
247800*AB*****MOVE CONTRACT-TITLE-LINE       TO TTLELNEO                00247800
247900        MOVE CONTRACT-ID-LINE          TO IDLINEO.                00247900
248000                                                                  00248000
248100     IF GCA-FROM-MENU-ID  =  'GC8A'                               00248100
248200        MOVE GCA-PLAN-CODE             TO BEN-PROV-PLAN-CODE      00248200
248300        MOVE GCA-GROUP-NUM             TO BEN-PROV-GROUP-NO       00248300
248400        MOVE GCA-SECTION-NUM           TO BEN-PROV-SECTION-NO     00248400
248500        MOVE GCA-PKG-CODE              TO BEN-PROV-PKG-CODE       00248500
248600        MOVE GCA-L-O-B                 TO BEN-PROV-LOB            00248600
248700        MOVE GCA-PROV-CTL              TO BEN-PROV-PROV-CTL       00248700
248800        MOVE GCA-FAM-REL-LVL           TO BEN-PROV-FAM-REL-LVL    00248800
248900        MOVE GCA-EFFECTIVE-DATE        TO BEN-PROV-EFF-DATE       00248900
249000        MOVE GCA-BEN-PROV-ID           TO BEN-PROV-ID-NO          00249000
249100        MOVE '   BEN. PROV ALL-LEVEL TABULAR MAINTENANCE'         00249100
249200                                          TO  TTLELNEO            00249200
249300*AB*****MOVE BENEFIT-PROVISION-TITLE-LINE TO  TTLELNEO            00249300
249400        MOVE BENEFIT-PROVISION-ID-LINE TO IDLINEO.                00249400
249500                                                                  00249500
249600     MOVE 'GA1D'                       TO FUNCTONO.               00249600
249700     MOVE '001D00'                     TO SCRNIDNO.               00249700
249800     MOVE ADL-TITLE-LINE               TO TITLEO.                 00249800
249900                                                                  00249900
250000     MOVE GCA-RECORD-POINTER-COMP  TO ACWA-WF-ALL-LEVEL-TAB-COMP. 00250000
250100     SET ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD  TO             00250100
250200                    GCA-RECORD-POINTER.                           00250200
250300                                                                  00250300
250400     IF  NOT WRK-STAT-CONT-MAINT AND                              00250400
250500         NOT WRK-STAT-GRP-SPEC-MAINT                              00250500
250600     THEN                                                         00250600
250700         MOVE WS-ABCODE-1DC2        TO WS-ABCODE                  00250700
250800         MOVE WS-ABCODE-1DC2-MSG    TO WS-ABCODE-MSG              00250800
250900         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                   00250900
251000                                                                  00251000
251100     IF  NOT WRK-REC-GROUP-SPEC-TAB AND                           00251100
251200         NOT WRK-REC-CONT-TAB       AND                           00251200
251300         NOT WRK-REC-CONT-BEN-TAB-PROV                            00251300
251400     THEN                                                         00251400
251500         MOVE WS-ABCODE-1DC3        TO WS-ABCODE                  00251500
251600         MOVE WS-ABCODE-1DC3-MSG    TO WS-ABCODE-MSG              00251600
251700         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                   00251700
251800                                                                  00251800
251900     MOVE GAC-ENTRY-COUNT         TO  GAC-ENTRY-COUNT.            00251900
252000     MOVE GCA-ALL-LEVEL-TAB-ID    TO  TABIDO.                     00252000
252100     MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  TABSLTNO.                   00252100
252200     SET GAC-INDEX  TO  1.                                        00252200
252300                                                                  00252300
252400     IF EIBTRNID  =  'GC4A' OR  'GC3A' OR  'GC8A'  OR 'GTM1'      00252400
252500        MOVE '0000000'  TO GCA-OCCURS-ENTRY-COUNTER.              00252500
252600                                                                  00252600
252700     IF GCA-OCCURS-ENTRY-COUNTER  =  '0000000'                    00252700
252800        GO TO 4000-200-BUILD-SCREEN.                              00252800
252900                                                                  00252900
253000     MOVE GCA-OCCURS-ENTRY-COUNTER  TO  ACWA-DISPLAY-LEN-7.       00253000
253100                                                                  00253100
253200 4000-100-FIND-RIGHT-OCCURS.                                      00253200
253300     IF GAC-DEDL-BENEFIT-PERIOD(GAC-INDEX)  NOT = HIGH-VALUES AND 00253300
253400        GAC-OCCURS-ENTRY-COUNTER(GAC-INDEX) NOT =                 00253400
253500                                                ACWA-DISPLAY-LEN-700253500
253600     THEN                                                         00253600
253700         IF GAC-INDEX  <  GAC-ENTRY-COUNT                         00253700
253800            SET GAC-INDEX  UP BY  1                               00253800
253900            GO TO 4000-100-FIND-RIGHT-OCCURS                      00253900
254000         ELSE                                                     00254000
254100             MOVE WS-ABCODE-1DL1        TO WS-ABCODE              00254100
254200             MOVE WS-ABCODE-1DL1-MSG    TO WS-ABCODE-MSG          00254200
254300             MOVE -1                    TO MFRMSLTL               00254300
254400             PERFORM 9800-000-ERROR-MSG-THEN-ABEND                00254400
254500     ELSE                                                         00254500
254600         NEXT SENTENCE.                                           00254600
254700                                                                  00254700
254800                                                                  00254800
254900 4000-200-BUILD-SCREEN.                                           00254900
255000                                                                  00255000
255100     IF  GCA-ADD-DEL-IND  =  'A' OR                               00255100
255200         GAC-ENTRY-COUNT  =  1                                    00255200
255300     THEN                                                         00255300
255400         MOVE 'CHG/ADD'  TO  DELADDO                              00255400
255500         MOVE DFHBMASD   TO  DLOPTLTA,  DELOPTNA                  00255500
255600         IF  GCA-OCCURS-ENTRY-COUNTER  =  '0000000'               00255600
255700         THEN                                                     00255700
255800             PERFORM 4100-000-DISPLAY-SKELETON                    00255800
255900         ELSE                                                     00255900
256000             PERFORM 4400-000-BUILD-DISPLAY                       00256000
256100     ELSE                                                         00256100
256200         MOVE 'CHG/DEL'  TO  DELADDO                              00256200
256300         MOVE 'D'        TO  DELOLITO                             00256300
256400         PERFORM 4400-000-BUILD-DISPLAY.                          00256400
256500                                                                  00256500
256600 4000-900-EXIT. EXIT.                                             00256600
256700                                                                  00256700
256800/*****************************************************************00256800
256900* 4100  DISPLAY SKELETON                                         *00256900
257000*                                                                *00257000
257100*    THIS ROUTINE REINITIALIZES THE SCREEN FOR THE OPERATOR      *00257100
257200*  AFTER THEY HAVE REVIEWED THE ENTRY THEY JUST ADDED AND        *00257200
257300*  INDICATED THAT THEY WANTED TO ADD MORE BY KEYING 'ENTER'.     *00257300
257400******************************************************************00257400
257500 4100-000-DISPLAY-SKELETON      SECTION.                          00257500
257600 4100-010.                                                        00257600
257700                                                                  00257700
257800     MOVE SPACES TO ERRMSGO.                                      00257800
257900                                                                  00257900
258000     MOVE DFHBMFSE  TO  PERIODA.                                  00258000
258100                                                                  00258100
258200     MOVE DFHBMUNP  TO  BENVLQLA  FAMINDIA  INTDESKA  LOBA        00258200
258300                        IBGROPTA  IPGNOPTA  IPGTOPTA  MFRMSLTA    00258300
258400                        IDGDOPTA  IPGPOPTA  IPGSOPTA.             00258400
258500                                                                  00258500
258600     MOVE ALL '_'  TO  PERIODO  BENVLQLO  LOBO                    00258600
258700                       FAMINDIO  PLCTRMTO.                        00258700
258800                                                                  00258800
258900     MOVE LOW-VALUES  TO  INTDESKO  MFRMSLTO                      00258900
259000                          IBGROPTO  IPGNOPTO  IPGTOPTO            00259000
259100                          IDGDOPTO  IPGPOPTO  IPGSOPTO.           00259100
259200                                                                  00259200
259300     MOVE ZEROS                                                   00259300
259400       TO COPAYINO CSTCONTO  PERTQALO  DEFINTNO  CARYOVRO         00259400
259500          DAYFACIO SRVGRUPO  PRTIMEFO  MANAPLIO  CONDLIFO         00259500
259600                   CLMLVLIO  INTRVALO  INTTYPEO  BNMXVALO         00259600
259700          FYIVALO  OVRDINDO  NEWVALUO  PRTIMEFO INTRVALO          00259700
259800          CONDALLO CONDEXCO  CONDICDO  CONDTABO  CONDMENO         00259800
259900          CONDEMCO CONDEACO  CONDSMIO  CONDNSMO ASCDSCDO BISNDINO 00259900
260000          CONDDRGO CONDALCO  CONDOBNO  CONDOBCO  CONDMALO         00260000
260100          CONDCARO CONDOBSO  CONDKDYO  CONDACCO  CONDSUIO         00260100
260200          CONDPECO  CONDNEMO  CONDTMJO  CONDINFO AGEQLLO AGEQLHO  00260200
260300          OENTCTRO IBGRSLTO  IPGNSLTO  IPGTSLTO  TOCURANO BENTYPO 00260300
260400          IDGDSLTO IPGPSLTO  AGELIMLO  AGELIMHO  RELPINDO TIERCDO 00260400
260500          FEAKINDO IPGSSLTO  ACCUMIDO  CAPINDO   SABDINDO TIERLVO.00260500
260600                                                                  00260600
260700     MOVE '01'    TO  COCURANO.                                   00260700
260800     MOVE -1      TO  PERIODL.                                    00260800
260900                                                                  00260900
261000     PERFORM 9000-000-SEND-ERASE-RETURN.                          00261000
261100                                                                  00261100
261200 4100-900-EXIT. EXIT.                                             00261200
261300                                                                  00261300
261400/*****************************************************************00261400
261500* 4200 DISPLAY NEXT                                              *00261500
261600*                                                                *00261600
261700*    THIS ROUTINE WILL FIND THE ENTRY CORRESPONDING TO THE       *00261700
261800*  SCREEN'S DISPLAY AND THEN POSITION TO THE NEXT ENTRY, IF THE  *00261800
261900*  NEXT ENTRY IS THE LAST IN THE LIST THE CODE WILL RECOGNIZE    *00261900
262000*  THAT AND POSITION TO THE FIRST ENTRY, ALSO DISPLAYING AN      *00262000
262100*  INFORMATIONAL MESSAGE.                                        *00262100
262200******************************************************************00262200
262300 4200-000-DISPLAY-NEXT          SECTION.                          00262300
262400 4200-010.                                                        00262400
262500                                                                  00262500
262600     MOVE GAC-ENTRY-COUNT  TO  GAC-ENTRY-COUNT.                   00262600
262700     SET GAC-INDEX         TO  1.                                 00262700
262800     MOVE OENTCTRO         TO  ACWA-DISPLAY-LEN-7.                00262800
262900                                                                  00262900
263000                                                                  00263000
263100     SET  CURNT-OCURS-BIN  TO  GAC-INDEX.                         00263100
263200     MOVE CURNT-OCURS-BIN  TO  CURNT-OCURS-PKD.                   00263200
263300     MOVE CURNT-OCCURS-OUT TO  COCURANO.                          00263300
263400                                                                  00263400
263500     IF GAC-ENTRY-COUNT  >  1                                     00263500
263600        COMPUTE  TOTAL-OCURS-UNK  =  GAC-ENTRY-COUNT  -  1        00263600
263700        MOVE  TOTAL-OCCURS-OUT  TO  TOCURANO                      00263700
263800     ELSE                                                         00263800
263900        MOVE  '01'              TO  TOCURANO.                     00263900
264000                                                                  00264000
264100                                                                  00264100
264200 4200-100-FIND-RIGHT-OCCURS.                                      00264200
264300                                                                  00264300
264400     IF GAC-DEDL-BENEFIT-PERIOD(GAC-INDEX)   NOT = HIGH-VALUES AND00264400
264500        GAC-OCCURS-ENTRY-COUNTER(GAC-INDEX)  NOT =                00264500
264600                                                ACWA-DISPLAY-LEN-700264600
264700     THEN                                                         00264700
264800         IF  GAC-INDEX  <  (GAC-ENTRY-COUNT - 1)                  00264800
264900         THEN                                                     00264900
265000             SET GAC-INDEX  UP BY  1                              00265000
265100             GO TO 4200-100-FIND-RIGHT-OCCURS                     00265100
265200         ELSE                                                     00265200
265300             SET GAC-INDEX  TO  1                                 00265300
265400             SET  WT-01-INDEX                     TO +20          00265400
265500             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      00265500
265600     ELSE                                                         00265600
265700         IF  GAC-INDEX  <  (GAC-ENTRY-COUNT - 1)                  00265700
265800         THEN                                                     00265800
265900             SET GAC-INDEX  UP BY  1                              00265900
266000         ELSE                                                     00266000
266100             SET GAC-INDEX  TO  1                                 00266100
266200             SET  WT-01-INDEX                     TO +20          00266200
266300             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO.     00266300
266400                                                                  00266400
266500     PERFORM 4400-000-BUILD-DISPLAY.                              00266500
266600                                                                  00266600
266700 4200-900-EXIT. EXIT.                                             00266700
266800                                                                  00266800
266900/*****************************************************************00266900
267000* 4300 DISPLAY PREV                                              *00267000
267100*                                                                *00267100
267200*    THIS ROUTINE WILL FIND THE ENTRY CORRESPONDING TO THE       *00267200
267300*  SCREEN'S DISPLAY AND THEN POSITION TO THE NEXT PREVIOUS ENTRY,*00267300
267400*  IF THE CURRENT ENTRY IS THE FIRST IN THE LIST THE CODE WILL   *00267400
267500*  RECOGNIZE THAT AND POSITION TO THE LAST ENTRY, ALSO DISPLAYING*00267500
267600*  AN INFORMATIONAL MESSAGE.                                     *00267600
267700******************************************************************00267700
267800 4300-000-DISPLAY-PREV          SECTION.                          00267800
267900 4300-010.                                                        00267900
268000                                                                  00268000
268100     MOVE GAC-ENTRY-COUNT  TO  GAC-ENTRY-COUNT.                   00268100
268200     SET  GAC-INDEX        TO  1.                                 00268200
268300     MOVE OENTCTRO         TO  ACWA-DISPLAY-LEN-7.                00268300
268400                                                                  00268400
268500 4300-100-FIND-RIGHT-OCCURS.                                      00268500
268600                                                                  00268600
268700     IF GAC-DEDL-BENEFIT-PERIOD(GAC-INDEX)   NOT = HIGH-VALUES AND00268700
268800        GAC-OCCURS-ENTRY-COUNTER(GAC-INDEX)  NOT =                00268800
268900                                                ACWA-DISPLAY-LEN-700268900
269000     THEN                                                         00269000
269100         IF  GAC-INDEX  <  (GAC-ENTRY-COUNT - 1)                  00269100
269200         THEN                                                     00269200
269300             SET GAC-INDEX  UP BY  1                              00269300
269400             GO TO 4300-100-FIND-RIGHT-OCCURS                     00269400
269500         ELSE                                                     00269500
269600             SET GAC-INDEX  TO  1                                 00269600
269700             SET  WT-01-INDEX                     TO +20          00269700
269800             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      00269800
269900     ELSE                                                         00269900
270000         IF  GAC-INDEX  NOT =  1                                  00270000
270100         THEN                                                     00270100
270200             SET GAC-INDEX  DOWN BY  1                            00270200
270300         ELSE                                                     00270300
270400             SET GAC-INDEX  TO  GAC-ENTRY-COUNT                   00270400
270500             SET GAC-INDEX  DOWN BY  1                            00270500
270600             SET  WT-01-INDEX                     TO +21          00270600
270700             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO.     00270700
270800                                                                  00270800
270900     PERFORM 4400-000-BUILD-DISPLAY.                              00270900
271000                                                                  00271000
271100 4300-900-EXIT. EXIT.                                             00271100
271200                                                                  00271200
271300/*****************************************************************00271300
271400* 4400 BUILD DISPLAY                                             *00271400
271500*                                                                *00271500
271600*    THIS ROUTINE WILL MOVE ALL THE FIELDS FROM THE OCCURENCE    *00271600
271700*  SPECIFIED BY INDEX GAC-INDEX TO THE SCREEN.                   *00271700
271800******************************************************************00271800
271900 4400-000-BUILD-DISPLAY         SECTION.                          00271900
272000 4400-010.                                                        00272000
272100                                                                  00272100
272200     IF  DELADDI  =  'CHG/DEL'                                    00272200
272300     THEN                                                         00272300
272400         MOVE 'D'  TO  DELOLITO                                   00272400
272500     ELSE                                                         00272500
272600         MOVE SPACE  TO  DELOLITO.                                00272600
272700                                                                  00272700
272800     MOVE SPACE                                       TO DELOPTNO.00272800
272900     MOVE GAC-OCCURS-ENTRY-COUNTER       (GAC-INDEX)  TO          00272900
273000                                               ACWA-DISPLAY-LEN-7.00273000
273100     MOVE ACWA-DISPLAY-LEN-7                          TO OENTCTRO.00273100
273200     MOVE GAC-DEDL-DAY-FACTOR-IND       (GAC-INDEX)  TO  DAYFACIO.00273200
273300     MOVE GAC-DEDL-CO-PAY-IND           (GAC-INDEX)  TO  COPAYINO.00273300
273400     MOVE GAC-DEDL-BISCENDING-IND-RSV   (GAC-INDEX)  TO BISNDINO. 00273400
273500     MOVE GAC-DEDL-ASCEND-DESCEND-IND   (GAC-INDEX)  TO ASCDSCDO. 00273500
      *P21595 CHANGES STARTS                                            00273510
           MOVE GAC-DEDL-BEN-TYPE             (GAC-INDEX)  TO  BENTYPO. 00273520
           MOVE GAC-DEDL-TIER-CODE            (GAC-INDEX)  TO  TIERCDO. 00273530
           MOVE GAC-DEDL-TIER-LVL             (GAC-INDEX)  TO  TIERLVO. 00273540
      *P21595 CHANGES ENDS                                              00273550
273600     MOVE GAC-DEDL-DEFINITION           (GAC-INDEX)  TO  DEFINTNO.00273600
273700     MOVE GAC-DEDL-MANDATORY-IND        (GAC-INDEX)  TO  MANAPLIO.00273700
273800     MOVE GAC-CARRY-OVER-CREDIT-IND     (GAC-INDEX)  TO  CARYOVRO.00273800
273900     MOVE GAC-DEDL-COST-CONTAIN-IND     (GAC-INDEX)  TO  CSTCONTO.00273900
274000     MOVE GAC-DEDL-BENEFIT-PERIOD       (GAC-INDEX)  TO  PERIODO. 00274000
274100     MOVE GAC-DEDL-BEN-PER-TIME-QUAL    (GAC-INDEX)  TO  PERTQALO.00274100
274200     MOVE GAC-DEDL-FAM-OR-INDIV         (GAC-INDEX)  TO  FAMINDIO.00274200
274300     MOVE GAC-DEDL-PLACE-OF-TREATMENT   (GAC-INDEX)  TO  PLCTRMTO.00274300
274400     MOVE GAC-DEDL-SERVICE-GROUP        (GAC-INDEX)  TO  SRVGRUPO.00274400
274500     MOVE GAC-DEDL-BEN-PER-TIME-FCTR    (GAC-INDEX)  TO           00274500
274600                                               ACWA-DISPLAY-LEN-3.00274600
274700     MOVE ACWA-DISPLAY-LEN-3                         TO  PRTIMEFO.00274700
274800     MOVE GAC-DEDL-CLAIM-LVL-ACCUM-IND  (GAC-INDEX)  TO  CLMLVLIO.00274800
274900     MOVE GAC-DEDL-AGE-LIMIT-FROM        (GAC-INDEX)  TO          00274900
275000                                               ACWA-DISPLAY-LEN-3.00275000
275100     MOVE ACWA-DISPLAY-LEN-3                          TO AGELIMLO.00275100
275200     MOVE GAC-DEDL-AGE-LIMIT-TO          (GAC-INDEX)  TO          00275200
275300                                               ACWA-DISPLAY-LEN-3.00275300
275400     MOVE ACWA-DISPLAY-LEN-3                          TO AGELIMHO.00275400
275500     MOVE GAC-DEDL-FEAK-IND              (GAC-INDEX)  TO FEAKINDO.00275500
275600     MOVE GAC-DEDL-ACCUMID               (GAC-INDEX)  TO ACCUMIDO.00275600
275700     MOVE GAC-DEDL-COMB-APPLIED-IND      (GAC-INDEX)  TO CAPINDO. 00275700
275800     MOVE GAC-DEDL-SEL-ADDL-BEN-DET      (GAC-INDEX)  TO SABDINDO.00275800
275900     MOVE GAC-DEDL-AGE-QUAL-IND-FROM     (GAC-INDEX)  TO AGEQLLO. 00275900
276000     MOVE GAC-DEDL-AGE-QUAL-IND-TO       (GAC-INDEX)  TO AGEQLHO. 00276000
276100     MOVE GAC-DEDL-RELATIONSHIP-IND      (GAC-INDEX)  TO RELPINDO.00276100
276200                                                                  00276200
276300     MOVE GAC-DEDL-INTERVAL-TIME-FCTR   (GAC-INDEX)  TO           00276300
276400                                               ACWA-DISPLAY-LEN-3.00276400
276500     MOVE ACWA-DISPLAY-LEN-3                          TO INTRVALO.00276500
276600     MOVE GAC-DEDL-INTERVAL-TYPE        (GAC-INDEX)  TO  INTTYPEO.00276600
276700     MOVE GAC-DEDL-L-O-B                (GAC-INDEX)  TO  LOBO.    00276700
276800     MOVE GAC-DEDL-VALUE-LIMIT          (GAC-INDEX)  TO           00276800
276900                                               ACWA-VALUE-LIMIT-9.00276900
277000     IF  ACWA-VALUE-LIMIT-9-9 = -1                                00277000
277100     THEN                                                         00277100
277200         MOVE 'NEG' TO BNMXVALO                                   00277200
277300     ELSE                                                         00277300
277000     IF  ACWA-VALUE-LIMIT-9-9 = -2                                00277310
277100     THEN                                                         00277320
277200         MOVE 'UNL' TO BNMXVALO                                   00277330
277300     ELSE                                                         00277340
277400         IF  GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'           00277400
277500         THEN                                                     00277500
277600             MOVE ACWA-VALUE-LIMIT-9    TO ACWA-EDIT-VALUE-LIMIT  00277600
277700             MOVE ACWA-EDIT-VALUE-LIMIT TO BNMXVALO               00277700
277800         ELSE                                                     00277800
277900             MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX) TO             00277900
278000                                              ACWA-DISPLAY-LEN-9-200278000
278100             MOVE ACWA-DISPLAY-LEN-9-X     TO ACWA-DISPLAY-9      00278100
278200             MOVE SPACES                   TO ACWA-DISPLAY-1      00278200
278300             MOVE ACWA-DISPLAY-VALUE-LIMIT TO BNMXVALO.           00278300
278400                                                                  00278400
278500     MOVE GAC-DEDL-VALUE-QUALIFIER      (GAC-INDEX)  TO  BENVLQLO.00278500
278600     MOVE GAC-DEDL-INTERVAL-OVRD-VALUE  (GAC-INDEX)  TO           00278600
278700                                               ACWA-DISPLAY-LEN-5.00278700
278800     MOVE ACWA-DISPLAY-LEN-5                         TO  NEWVALUO.00278800
278900     MOVE GAC-DEDL-INTERVAL-OVRD-IND    (GAC-INDEX)  TO  OVRDINDO.00278900
279000     MOVE GAC-DEDL-INTERNAL-DESCRIPTOR  (GAC-INDEX)  TO  INTDESKO.00279000
279100     MOVE GAC-COND-ALL-BIT              (GAC-INDEX)  TO  CONDALLO.00279100
279200     MOVE GAC-COND-EXCLUSION-BIT        (GAC-INDEX)  TO  CONDEXCO.00279200
279300     MOVE GAC-COND-ICD-BIT              (GAC-INDEX)  TO  CONDICDO.00279300
279400     MOVE GAC-COND-TB-BIT               (GAC-INDEX)  TO  CONDTABO.00279400
279500     MOVE GAC-COND-MENTAL-BIT           (GAC-INDEX)  TO  CONDMENO.00279500
279600     MOVE GAC-COND-DRUG-BIT             (GAC-INDEX)  TO  CONDDRGO.00279600
279700     MOVE GAC-COND-ALCOHOL-BIT          (GAC-INDEX)  TO  CONDALCO.00279700
279800     MOVE GAC-COND-OB-COMP-BIT          (GAC-INDEX)  TO  CONDOBCO.00279800
279900     MOVE GAC-COND-OB-NORM-BIT          (GAC-INDEX)  TO  CONDOBNO.00279900
280000     MOVE GAC-COND-MALIGNANCY-BIT       (GAC-INDEX)  TO  CONDMALO.00280000
280100     MOVE GAC-COND-CARDIAC-DISEASE-BIT  (GAC-INDEX)  TO  CONDCARO.00280100
280200     MOVE GAC-COND-OBESITY-BIT          (GAC-INDEX)  TO  CONDOBSO.00280200
280300     MOVE GAC-COND-KIDNEY-DISEASE-BIT   (GAC-INDEX)  TO  CONDKDYO.00280300
280400     MOVE GAC-COND-ACCIDENT-BIT         (GAC-INDEX)  TO  CONDACCO.00280400
280500     MOVE GAC-COND-PRE-EXIST-BIT        (GAC-INDEX)  TO  CONDPECO.00280500
280600     MOVE GAC-COND-NON-EMER-BIT         (GAC-INDEX)  TO  CONDNEMO.00280600
280700     MOVE GAC-COND-SUICIDE-BIT          (GAC-INDEX)  TO  CONDSUIO.00280700
280800     MOVE GAC-COND-TMJ-BIT              (GAC-INDEX)  TO  CONDTMJO.00280800
280900     MOVE GAC-COND-INF-BIT              (GAC-INDEX)  TO  CONDINFO.00280900
281000     MOVE GAC-COND-LIFE-THREAT-BIT      (GAC-INDEX)  TO  CONDLIFO.00281000
281100     MOVE GAC-COND-EMER-MED-BIT         (GAC-INDEX)  TO  CONDEMCO.00281100
281200     MOVE GAC-COND-EMER-ACC-BIT         (GAC-INDEX)  TO  CONDEACO.00281200
281300     MOVE GAC-COND-SER-MEN-ILL-BIT      (GAC-INDEX)  TO  CONDSMIO.00281300
281400     MOVE GAC-COND-NON-SER-MEN-ILL-BIT  (GAC-INDEX)  TO  CONDNSMO.00281400
281500                                                                  00281500
281600     MOVE -1  TO PERIODL.                                         00281600
281700                                                                  00281700
281800     MOVE GAC-DEDL-FYI-VALUE (GAC-INDEX)  TO  FYIVALO.            00281800
281900     SET  CURNT-OCURS-BIN                TO GAC-INDEX.            00281900
282000     MOVE CURNT-OCURS-BIN                TO CURNT-OCURS-PKD.      00282000
282100     MOVE CURNT-OCCURS-OUT               TO COCURANO.             00282100
282200                                                                  00282200
282300     IF GAC-ENTRY-COUNT  >  1                                     00282300
282400     THEN                                                         00282400
282500         COMPUTE  TOTAL-OCURS-UNK  =  GAC-ENTRY-COUNT  -  1       00282500
282600         MOVE  TOTAL-OCCURS-OUT  TO  TOCURANO                     00282600
282700     ELSE                                                         00282700
282800         MOVE  '01'              TO  TOCURANO.                    00282800
282900                                                                  00282900
283000     MOVE ZEROS   TO  IBGRSLTO,  IPGNSLTO,  IPGTSLTO              00283000
283100                      IDGDSLTO,  IPGPSLTO,  IPGSSLTO.             00283100
283200                                                                  00283200
283300     SET GAC-INT-INDEX TO      1.                                 00283300
283400     SET GAC-INT-INDEX DOWN BY 1.                                 00283400
283500                                                                  00283500
283600 4400-300-DISPLAY-LOOP.                                           00283600
283700                                                                  00283700
283800     SET GAC-INT-INDEX  UP BY  1.                                 00283800
283900     IF  GAC-INT-INDEX  >  5                                      00283900
284000         GO TO 4400-800-SEND.                                     00284000
284100                                                                  00284100
284200     IF GAC-INT-ID(GAC-INDEX GAC-INT-INDEX)  =  HIGH-VALUES       00284200
284300         GO TO 4400-800-SEND.                                     00284300
284400                                                                  00284400
284500     IF  GAC-INT-ID (GAC-INDEX GAC-INT-INDEX)       = '#IBGR '    00284500
284600         MOVE GAC-INT-SLOT (GAC-INDEX GAC-INT-INDEX)              00284600
284700                                TO ACWA-DISPLAY-LEN-7             00284700
284800         MOVE ACWA-DISPLAY-LEN-7 TO IBGRSLTO                      00284800
284900         GO TO 4400-300-DISPLAY-LOOP.                             00284900
285000                                                                  00285000
285100     IF  GAC-INT-ID (GAC-INDEX GAC-INT-INDEX)       = '#IDGD '    00285100
285200         MOVE GAC-INT-SLOT (GAC-INDEX GAC-INT-INDEX)              00285200
285300                                TO ACWA-DISPLAY-LEN-7             00285300
285400         MOVE ACWA-DISPLAY-LEN-7 TO IDGDSLTO                      00285400
285500         GO TO 4400-300-DISPLAY-LOOP.                             00285500
285600                                                                  00285600
285700     IF  GAC-INT-ID (GAC-INDEX GAC-INT-INDEX)       = '#IPGN '    00285700
285800         MOVE GAC-INT-SLOT (GAC-INDEX GAC-INT-INDEX)              00285800
285900                                TO ACWA-DISPLAY-LEN-7             00285900
286000         MOVE ACWA-DISPLAY-LEN-7 TO IPGNSLTO                      00286000
286100         GO TO 4400-300-DISPLAY-LOOP.                             00286100
286200                                                                  00286200
286300     IF  GAC-INT-ID (GAC-INDEX GAC-INT-INDEX)       = '#IPGP '    00286300
286400         MOVE GAC-INT-SLOT (GAC-INDEX GAC-INT-INDEX)              00286400
286500                                TO ACWA-DISPLAY-LEN-7             00286500
286600         MOVE ACWA-DISPLAY-LEN-7 TO IPGPSLTO                      00286600
286700         GO TO 4400-300-DISPLAY-LOOP.                             00286700
286800                                                                  00286800
286900     IF  GAC-INT-ID (GAC-INDEX GAC-INT-INDEX)       = '#IPGT '    00286900
287000         MOVE GAC-INT-SLOT (GAC-INDEX GAC-INT-INDEX)              00287000
287100                                TO ACWA-DISPLAY-LEN-7             00287100
287200         MOVE ACWA-DISPLAY-LEN-7 TO IPGTSLTO                      00287200
287300         GO TO 4400-300-DISPLAY-LOOP.                             00287300
287400                                                                  00287400
287500     IF  GAC-INT-ID (GAC-INDEX GAC-INT-INDEX)       = '#IPGS '    00287500
287600         MOVE GAC-INT-SLOT (GAC-INDEX GAC-INT-INDEX)              00287600
287700                                TO ACWA-DISPLAY-LEN-7             00287700
287800         MOVE ACWA-DISPLAY-LEN-7 TO IPGSSLTO                      00287800
287900         GO TO 4400-300-DISPLAY-LOOP.                             00287900
288000                                                                  00288000
288100     MOVE WS-ABCODE-1DF3        TO WS-ABCODE                      00288100
288200     MOVE WS-ABCODE-1DF3-MSG    TO WS-ABCODE-MSG                  00288200
288300     PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                       00288300
288400                                                                  00288400
288500                                                                  00288500
288600 4400-800-SEND.                                                   00288600
288700                                                                  00288700
288800     PERFORM 4500-000-PROTECT-CRIT-DATA-ELE.                      00288800
288900                                                                  00288900
289000*-------RESET ATTR. 'CAUSE INTDESK & IDPROD CHANGED IN 4500- CALL 00289000
289100     PERFORM 7900-000-RESET-ATTRIBUTES.                           00289100
289200                                                                  00289200
289300     PERFORM 9000-000-SEND-ERASE-RETURN.                          00289300
289400                                                                  00289400
289500 4400-900-EXIT. EXIT.                                             00289500
289600                                                                  00289600
289700/*****************************************************************00289700
289800*  4500  -  PROTECT CRITICAL DATA ELEMENTS                       *00289800
289900*                                                                *00289900
290000*        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *00290000
290100*          1. DETERMINE IF GROUP IS CRITICAL (CALL GCTRSRT).     *00290100
290200*          2. IF GROUP IS CRITICAL:                              *00290200
290300*              - READ PRODUCTION CONTRACT, GROUP SPECIFIC, OR    *00290300
290400*                BENEFIT PROVISION.                              *00290400
290500*                - IF ON DATA BASE:                              *00290500
290600*                  - SCAN FOR #ADL TABULAR                       *00290600
290700*                    - IF TABULAR PRESENT AND ACTIVE, TABULAR IS *00290700
290800*                      CRITICAL, PROTECT CRITICAL DATA ELEMENTS  *00290800
290900*                      ON SCREEN AND ISSUE MESSAGE.              *00290900
291000******************************************************************00291000
291100 4500-000-PROTECT-CRIT-DATA-ELE SECTION.                          00291100
291200 4500-010.                                                        00291200
291300                                                                  00291300
291400     IF DELADDI  =  'CHG/DEL'   OR                                00291400
291500        DELOLITI =  SPACES                                        00291500
291600        NEXT SENTENCE                                             00291600
291700     ELSE                                                         00291700
291800        GO TO 4500-900-EXIT.                                      00291800
291900                                                                  00291900
292000     MOVE WS-REQUEST-4500-CDE-PROTECT  TO  ACWA-CDE-REQUEST-CODE. 00292000
292100     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00292100
292200                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00292200
292300                                                                  00292300
292400     EXEC CICS  LINK   PROGRAM('GACDEPGM')                        00292400
292500                COMMAREA (COMMON-WORKAREAS)                       00292500
292600                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00292600
292700                                                                  00292700
292800     GO TO 4500-900-EXIT.                                         00292800
292900                                                                  00292900
293000 4500-900-EXIT. EXIT.                                             00293000
293100                                                                  00293100
293200/*****************************************************************00293200
293300*  4600  -  UPDATE CRITICAL DATA ELEMENT STATUS                  *00293300
293400*                                                                *00293400
293500*        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *00293500
293600*           1. READ ALL LEVEL TABULAR FROM PROVISION POOL        *00293600
293700*           2. COMPARE CDE ELEMENTS ON W/F ALL LVL TAB TO THOSE  *00293700
293800*               ON THE PROVISION POOL ALL LVL TABULAR RECORD.    *00293800
293900*           3. IF CDE ELEMENTS ON W/F ALL LVL TAB HAVE BEEN      *00293900
294000*               CHANGED, ISSUE MESSAGE AND POSITION CURSOR ON    *00294000
294100*               +CDE+ INDICATOR (POSITION=8).                    *00294100
294200*              IF MESSAGE HAS BEEN ISSUED AND OPERATOR HAS HIT   *00294200
294300*               ENTER, CONTINUE PROCESSING.                      *00294300
294400******************************************************************00294400
294500 4600-000-UPDATE-CDE-STATUS     SECTION.                          00294500
294600 4600-010.                                                        00294600
294700                                                                  00294700
294800     IF CDEINDO = ('+CDE+' OR '+CDE-') AND                        00294800
294900        (DELADDI = 'CHG/DEL' OR                                   00294900
295000        (DELADDI = 'CHG/ADD' AND                                  00295000
295100        WRK-SIGNAL-FROM-ONLINE  =  'W'))                          00295100
295200        NEXT SENTENCE                                             00295200
295300     ELSE                                                         00295300
295400        IF CDEINDO = ('+CDE+' OR '+CDE-') AND                     00295400
295500           (DELADDI = 'CHG/ADD')                                  00295500
295600           NEXT SENTENCE                                          00295600
295700        ELSE                                                      00295700
295800            GO TO 4600-900-EXIT.                                  00295800
295900                                                                  00295900
296000                                                                  00296000
296100     MOVE WS-ALT-WORKFILE-KEYS       TO ACWA-ALT-WORKFILE-KEYS.   00296100
296200     SET  ACWA-INDEX-1               TO GAC-INDEX.                00296200
296300     MOVE WS-REQUEST-4600-CDE-STATUS TO ACWA-CDE-REQUEST-CODE.    00296300
296400     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00296400
296500                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00296500
296600                                                                  00296600
296700     EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                      00296700
296800                COMMAREA (COMMON-WORKAREAS)                       00296800
296900                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00296900
297000                                                                  00297000
297100     IF  ACWA-CDE-RETURN-DONT-SEND                                00297100
297200*        EXEC CICS  RETURN  END-EXEC.                             00297200
297300         EXEC CICS  RETURN TRANSID('GA1D')                        00297300
297400                    COMMAREA(DFHCOMMAREA)                         00297400
297500                    LENGTH  (EIBCALEN)                            00297500
297600                    END-EXEC.                                     00297600
297700                                                                  00297700
297800 4600-900-EXIT. EXIT.                                             00297800
297900                                                                  00297900
298000/*****************************************************************00298000
298100*  4700  -  UPDATE W/F CONTROL RECORD                            *00298100
298200*                                                                *00298200
298300*        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *00298300
298400*          1. READ W/F CONTROL RECORD, UPDATE CDE RECORD COUNTS  *00298400
298500*             WITH THE ACTION TAKEN ON THE ALL LEVEL TABULAR     *00298500
298600*             RECORD IF ITS CDE STATUS CHANGED.                  *00298600
298700*          2. REWRITE W/F CONTROL RECORD                         *00298700
298800******************************************************************00298800
298900 4700-000-UPDATE-CONTROL-RECORD SECTION.                          00298900
299000 4700-010.                                                        00299000
299100                                                                  00299100
299200     MOVE WS-ALT-WORKFILE-KEYS        TO ACWA-ALT-WORKFILE-KEYS.  00299200
299300     SET  ACWA-INDEX-1                TO GAC-INDEX.               00299300
299400     MOVE WS-REQUEST-4700-CNTL-UPDATE TO ACWA-CDE-REQUEST-CODE.   00299400
299500     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00299500
299600                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00299600
299700                                                                  00299700
299800     EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                      00299800
299900                COMMAREA (COMMON-WORKAREAS)                       00299900
300000                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00300000
300100                                                                  00300100
300200 4700-900-EXIT. EXIT.                                             00300200
300300                                                                  00300300
300400/*****************************************************************00300400
300500* 5000  XCTL TO PREVIOUS MENU                                    *00300500
300600*                                                                *00300600
300700*   THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM      *00300700
300800*  ORIGINATED FROM.  WE READ THE ALL LEVEL TABULAR RECORD AND    *00300800
300900*  INSURE THAT THE TABLE OF OCCURRENCES IS SORTED AND THAT ANY   *00300900
301000*  DUPLICATES ARE DROPPED FROM THE LIST.  WE THEN REWRITE THE    *00301000
301100*  ALL LEVEL TABULAR AND READ THE PARTICULAR RECORD THAT THE     *00301100
301200*  MENU WHICH PASSED US CONTROL WOULD REQUIRE.  FINALLY BASED    *00301200
301300*  ON THE PREVIOUS MENU FIELD CARRIED THROUGHOUT THIS PART OF    *00301300
301400*  THE SYSTEM WE RETURN TO THE PREVIOUS MENU.                    *00301400
301500******************************************************************00301500
301600 5000-000-XCTL-TO-PREVIOUS-MENU SECTION.                          00301600
301700 5000-010.                                                        00301700
301800                                                                  00301800
301900     PERFORM 3200-000-READ-REC-FOR-UPDATE.                        00301900
302000                                                                  00302000
302100     IF NOT GCIO-GOOD-RETURN                                      00302100
302200        MOVE WS-ABCODE-1DFK        TO WS-ABCODE                   00302200
302300        MOVE WS-ABCODE-1DFK-MSG    TO WS-ABCODE-MSG               00302300
302400        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00302400
302500                                                                  00302500
302600     PERFORM 6500-000-SORT-COMPRESS-ALL-LVL.                      00302600
302700     PERFORM 4600-000-UPDATE-CDE-STATUS.                          00302700
302800     PERFORM 3000-000-UPDATE-GAC-RECORD.                          00302800
302900                                                                  00302900
303000     IF ACWA-CDE-FIELD-CHANGED OR  ACWA-CDE-REC-CHANGED           00303000
303100        IF EIBCPOSN = 8                                           00303100
303200           NEXT SENTENCE                                          00303200
303300        ELSE                                                      00303300
303400*          EXEC CICS  RETURN  END-EXEC.                           00303400
303500           EXEC CICS  RETURN TRANSID('GA1D')                      00303500
303600                      COMMAREA(DFHCOMMAREA)                       00303600
303700                      LENGTH  (EIBCALEN)                          00303700
303800                      END-EXEC.                                   00303800
303900                                                                  00303900
304000     IF FRMNUIDI  =  'GS3A'                                       00304000
304100        PERFORM 5100-000-RETURN-TO-GRP-SPEC                       00304100
304200        EXEC CICS  XCTL  PROGRAM ('GS3APGM')                      00304200
304300                   COMMAREA(WORK-RECORD-3)                        00304300
304400                   LENGTH (WS-WRK-GRP-SPEC-LEN)   END-EXEC.       00304400
304500                                                                  00304500
304600     IF  FRMNUIDI  =  'GC4A'                                      00304600
304700        PERFORM 5200-000-RETURN-TO-CONTRACT                       00304700
304800        EXEC CICS  XCTL  PROGRAM ('GC4APGM')                      00304800
304900                   COMMAREA(WORK-RECORD-4)                        00304900
305000                   LENGTH (WS-WRK-CONTRACT-LEN)   END-EXEC.       00305000
305100                                                                  00305100
305200     IF FRMNUIDI  =  'GC8A'                                       00305200
305300        PERFORM 5300-000-RETURN-TO-BEN-PROV                       00305300
305400        EXEC CICS  XCTL  PROGRAM ('GC8APGM')                      00305400
305500                   COMMAREA(WORK-RECORD-5)                        00305500
305600                   LENGTH (WS-WRK-BEN-PROV-LEN)   END-EXEC.       00305600
305700                                                                  00305700
305800*******                                                           00305800
305900* STS *===> RETURN TO SINGLE TABULAR SUPPORT MENU, NO COMMAREA    00305900
306000*******                                                          |00306000
306100     IF  FRMNUIDI  =  'GTM1'                                      00306100
306200         EXEC CICS  XCTL  PROGRAM('GTM1PGM')   END-EXEC.          00306200
306300*******                                                          |00306300
306400* STS *----------------------------------------------------------*00306400
306500*******                                                           00306500
306600                                                                  00306600
306700 5000-900-EXIT. EXIT.                                             00306700
306800                                                                  00306800
306900/*****************************************************************00306900
307000* 5100  RETURN TO GRP SPEC                                       *00307000
307100*                                                                *00307100
307200*    THESE ROUTINES WILL BUILD THE IO PROGRAMS PARAMTER LIST     *00307200
307300*  AND THEN READ THE RECORD NEEDED BY THE PREVIOUS MENU.         *00307300
307400******************************************************************00307400
307500 5100-000-RETURN-TO-GRP-SPEC    SECTION.                          00307500
307600 5100-010.                                                        00307600
307700                                                                  00307700
307800        EXEC CICS GETMAIN                                         00307800
307900               SET(ADDRESS OF WF-IO-PARM-WRK-GRP-SPEC-REC)        00307900
308000               INITIMG(WS-HEX-00)                                 00308000
308100               LENGTH(WS-IO-PARM-WRK-GRP-SPEC-LEN)                00308100
308200               END-EXEC.                                          00308200
308300                                                                  00308300
308400        SET ACWA-WF-GRP-SPEC-PNTR     TO                          00308400
308500                 ADDRESS OF WF-IO-PARM-WRK-GRP-SPEC-REC.          00308500
308600                                                                  00308600
308700     MOVE SPACES               TO GCIO-WORKFILE-KEY.              00308700
308800     MOVE 'G'                  TO GCIO-WRK-STATUS-CODE.           00308800
308900     MOVE 'G2'                 TO GCIO-WRK-RECORD-TYPE.           00308900
309000     MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             00309000
309100     MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             00309100
309200     MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           00309200
309300     MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              00309300
309400     MOVE SPACES               TO GCIO-WRK-LINE-OF-BUS            00309400
309500                                  GCIO-WRK-PROVISION-ID           00309500
309600                                  GCIO-WRK-PROVIDER-CONTROL       00309600
309700                                  GCIO-WRK-TAB-PROVISION-ID.      00309700
309800     MOVE ZEROS                TO GCIO-WRK-PROVISION-SLOT-NO,     00309800
309900                                  GCIO-WRK-TAB-PROV-SLOT-NO.      00309900
310000     MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   00310000
310100     MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             00310100
310200                                                                  00310200
310300     MOVE GC-GCPSWORK-DDNAME     TO GCIO3-FILE-DDNAME.            00310300
310400     MOVE GCIO-WORKFILE-KEY      TO GCIO3-FILE-KEY.               00310400
310500     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO3-FILE-ACCESS-CODE.       00310500
310600     MOVE GC-GCIO-AREA-1         TO GCIO3-IO-AREA-TO-USE.         00310600
310700     MOVE GC-GCGRPSPC-VARY-MAX-OCUR  TO                           00310700
310800                      GCG-COUNT-TAB-PROVN-POINTERS.               00310800
310900                                                                  00310900
311000     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00311000
311100                COMMAREA(WF-IO-PARM-WRK-GRP-SPEC-REC)             00311100
311200                LENGTH (WS-IO-PARM-WRK-GRP-SPEC-LEN)  END-EXEC.   00311200
311300                                                                  00311300
311400     IF NOT GCIO3-GOOD-RETURN                                     00311400
311500        MOVE WS-ABCODE-1DFL        TO WS-ABCODE                   00311500
311600        MOVE WS-ABCODE-1DFL-MSG    TO WS-ABCODE-MSG               00311600
311700        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00311700
311800                                                                  00311800
311900 5100-900-EXIT. EXIT.                                             00311900
312000                                                                  00312000
312100/*****************************************************************00312100
312200* 5200  RETURN TO CONTRACT                                       *00312200
312300*                                                                *00312300
312400*    THESE ROUTINES WILL BUILD THE IO PROGRAMS PARAMTER LIST     *00312400
312500*  AND THEN READ THE RECORD NEEDED BY THE PREVIOUS MENU.         *00312500
312600******************************************************************00312600
312700 5200-000-RETURN-TO-CONTRACT    SECTION.                          00312700
312800 5200-010.                                                        00312800
312900                                                                  00312900
313000        EXEC CICS GETMAIN                                         00313000
313100               SET(ADDRESS OF WF-IO-PARM-WRK-CONTRACT-REC)        00313100
313200               INITIMG(WS-HEX-00)                                 00313200
313300               LENGTH(WS-IO-PARM-WRK-CONTRACT-LEN)                00313300
313400               END-EXEC.                                          00313400
313500                                                                  00313500
313600        SET ACWA-WF-CONTRACT-PNTR     TO                          00313600
313700                 ADDRESS OF WF-IO-PARM-WRK-CONTRACT-REC.          00313700
313800                                                                  00313800
313900     MOVE SPACES               TO GCIO-WORKFILE-KEY.              00313900
314000     MOVE 'C'                  TO GCIO-WRK-STATUS-CODE.           00314000
314100     MOVE 'C2'                 TO GCIO-WRK-RECORD-TYPE.           00314100
314200     MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             00314200
314300     MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             00314300
314400     MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           00314400
314500     MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              00314500
314600     MOVE GCA-L-O-B            TO GCIO-WRK-LINE-OF-BUS.           00314600
314700     MOVE GCA-PROV-CTL         TO GCIO-WRK-PROVIDER-CONTROL.      00314700
314800     MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   00314800
314900     MOVE SPACES               TO GCIO-WRK-PROVISION-ID           00314900
315000                                  GCIO-WRK-TAB-PROVISION-ID.      00315000
315100     MOVE ZEROS                TO GCIO-WRK-PROVISION-SLOT-NO      00315100
315200                                  GCIO-WRK-TAB-PROV-SLOT-NO.      00315200
315300                                                                  00315300
315400     MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             00315400
315500     MOVE GC-GCPSWORK-DDNAME     TO GCIO4-FILE-DDNAME.            00315500
315600     MOVE GCIO-WORKFILE-KEY      TO GCIO4-FILE-KEY.               00315600
315700     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO4-FILE-ACCESS-CODE.       00315700
315800     MOVE GC-GCIO-AREA-1         TO GCIO4-IO-AREA-TO-USE.         00315800
315900     MOVE GC-GCCONTR-VARY-MAX-OCUR  TO                            00315900
316000                      GCT-COUNT-BEN-PROVN-POINTERS.               00316000
316100                                                                  00316100
316200     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00316200
316300                COMMAREA(WF-IO-PARM-WRK-CONTRACT-REC)             00316300
316400                LENGTH (WS-IO-PARM-WRK-CONTRACT-LEN)   END-EXEC.  00316400
316500                                                                  00316500
316600     IF NOT GCIO4-GOOD-RETURN                                     00316600
316700        MOVE WS-ABCODE-1DFM        TO WS-ABCODE                   00316700
316800        MOVE WS-ABCODE-1DFM-MSG    TO WS-ABCODE-MSG               00316800
316900        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00316900
317000                                                                  00317000
317100 5200-900-EXIT. EXIT.                                             00317100
317200                                                                  00317200
317300/*****************************************************************00317300
317400* 5300  RETURN TO BEN PROV                                       *00317400
317500*                                                                *00317500
317600*    THESE ROUTINES WILL BUILD THE IO PROGRAMS PARAMTER LIST     *00317600
317700*  AND THEN READ THE RECORD NEEDED BY THE PREVIOUS MENU.         *00317700
317800******************************************************************00317800
317900 5300-000-RETURN-TO-BEN-PROV    SECTION.                          00317900
318000 5300-010.                                                        00318000
318100                                                                  00318100
318200        EXEC CICS GETMAIN                                         00318200
318300               SET(ADDRESS OF WF-IO-PARM-WRK-BEN-PROV-REC)        00318300
318400               INITIMG(WS-HEX-00)                                 00318400
318500               LENGTH(WS-IO-PARM-WRK-BEN-PROV-LEN)                00318500
318600               END-EXEC.                                          00318600
318700                                                                  00318700
318800        SET ACWA-WF-BEN-PROV-PNTR     TO                          00318800
318900                 ADDRESS OF WF-IO-PARM-WRK-BEN-PROV-REC.          00318900
319000                                                                  00319000
319100     MOVE SPACES                TO GCIO-WORKFILE-KEY.             00319100
319200     MOVE 'C'                   TO GCIO-WRK-STATUS-CODE.          00319200
319300     MOVE 'C4'                  TO GCIO-WRK-RECORD-TYPE.          00319300
319400     MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             00319400
319500     MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             00319500
319600     MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           00319600
319700     MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              00319700
319800     MOVE GCA-L-O-B            TO GCIO-WRK-LINE-OF-BUS.           00319800
319900     MOVE GCA-PROV-CTL         TO GCIO-WRK-PROVIDER-CONTROL.      00319900
320000     MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   00320000
320100     MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN.            00320100
320200     MOVE GCA-BEN-PROV-ID       TO GCIO-WRK-PROVISION-ID.         00320200
320300     MOVE SPACES                TO GCIO-WRK-TAB-PROVISION-ID.     00320300
320400     MOVE 9999999               TO GCIO-WRK-PROVISION-SLOT-NO.    00320400
320500     MOVE ZEROS                 TO GCIO-WRK-TAB-PROV-SLOT-NO.     00320500
320600                                                                  00320600
320700     MOVE GC-GCPSWORK-DDNAME     TO GCIO5-FILE-DDNAME.            00320700
320800     MOVE GCIO-WORKFILE-KEY      TO GCIO5-FILE-KEY.               00320800
320900     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO5-FILE-ACCESS-CODE.       00320900
321000     MOVE GC-GCIO-AREA-1         TO GCIO5-IO-AREA-TO-USE.         00321000
321100     MOVE GC-GCBENPRV-VARY-MAX-OCUR  TO                           00321100
321200                      GCP-COUNT-TAB-PROVN-POINTERS.               00321200
321300                                                                  00321300
321400     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00321400
321500                COMMAREA(WF-IO-PARM-WRK-BEN-PROV-REC)             00321500
321600                LENGTH (WS-IO-PARM-WRK-BEN-PROV-LEN)  END-EXEC.   00321600
321700                                                                  00321700
321800     IF NOT GCIO5-GOOD-RETURN                                     00321800
321900        MOVE WS-ABCODE-1DFN        TO WS-ABCODE                   00321900
322000        MOVE WS-ABCODE-1DFN-MSG    TO WS-ABCODE-MSG               00322000
322100        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00322100
322200                                                                  00322200
322300 5300-900-EXIT. EXIT.                                             00322300
322400                                                                  00322400
322500/*****************************************************************00322500
322600* 6000  BUILD GROUP SPEC KEY                                     *00322600
322700*                                                                *00322700
322800*    BUILD THE GROUP SPECIFIC KEY FOR WORKFILE READS             *00322800
322900******************************************************************00322900
323000 6000-000-BUILD-GROUP-SPEC-KEY  SECTION.                          00323000
323100 6000-010.                                                        00323100
323200                                                                  00323200
323300     MOVE SPACES                TO GCIO-WORKFILE-KEY.             00323300
323400     MOVE  'G'                  TO GCIO-WRK-STATUS-CODE.          00323400
323500     MOVE  'G3'                 TO GCIO-WRK-RECORD-TYPE.          00323500
323600     MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE.            00323600
323700     MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM.            00323700
323800     MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM.          00323800
323900     MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE.             00323900
324000     MOVE SPACES                TO GCIO-WRK-LINE-OF-BUS,          00324000
324100                                   GCIO-WRK-PROVIDER-CONTROL.     00324100
324200     MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVL.  00324200
324300     MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN.            00324300
324400     MOVE TABIDI                TO GCIO-WRK-PROVISION-ID.         00324400
324500     MOVE TABSLTNI              TO ACWA-DISPLAY-LEN-7.            00324500
324600     MOVE ACWA-DISPLAY-LEN-7    TO GCIO-WRK-PROVISION-SLOT-NO.    00324600
324700     MOVE SPACES                TO GCIO-WRK-TAB-PROVISION-ID.     00324700
324800     MOVE ZEROS                 TO GCIO-WRK-TAB-PROV-SLOT-NO.     00324800
324900                                                                  00324900
325000 6000-900-EXIT. EXIT.                                             00325000
325100                                                                  00325100
325200******************************************************************00325200
325300* 6100  BUILD CONTRACT KEY                                       *00325300
325400*                                                                *00325400
325500*    BUILD THE CONTRACT KEY FOR WORKFILE READS                   *00325500
325600******************************************************************00325600
325700 6100-000-BUILD-CONTRACT-KEY    SECTION.                          00325700
325800 6100-010.                                                        00325800
325900                                                                  00325900
326000     MOVE SPACES               TO GCIO-WORKFILE-KEY.              00326000
326100     MOVE  'C'                 TO GCIO-WRK-STATUS-CODE.           00326100
326200     MOVE  'C3'                TO GCIO-WRK-RECORD-TYPE.           00326200
326300     MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             00326300
326400     MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             00326400
326500     MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           00326500
326600     MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              00326600
326700     MOVE GCA-L-O-B            TO GCIO-WRK-LINE-OF-BUS.           00326700
326800     MOVE GCA-PROV-CTL         TO GCIO-WRK-PROVIDER-CONTROL.      00326800
326900     MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   00326900
327000     MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             00327000
327100     MOVE TABIDI               TO GCIO-WRK-PROVISION-ID.          00327100
327200     MOVE TABSLTNI             TO ACWA-DISPLAY-LEN-7.             00327200
327300     MOVE ACWA-DISPLAY-LEN-7   TO GCIO-WRK-PROVISION-SLOT-NO.     00327300
327400     MOVE SPACES               TO GCIO-WRK-TAB-PROVISION-ID.      00327400
327500     MOVE ZEROS                TO GCIO-WRK-TAB-PROV-SLOT-NO.      00327500
327600                                                                  00327600
327700 6100-900-EXIT. EXIT.                                             00327700
327800                                                                  00327800
327900/*****************************************************************00327900
328000* 6200  BUILD BEN PROV KEY                                       *00328000
328100*                                                                *00328100
328200*    BUILD THE BEN PROV KEY FOR WORKFILE READS                   *00328200
328300******************************************************************00328300
328400 6200-000-BUILD-BEN-PROV-KEY    SECTION.                          00328400
328500 6200-010.                                                        00328500
328600                                                                  00328600
328700     MOVE SPACES                TO GCIO-WORKFILE-KEY.             00328700
328800     MOVE  'C'                  TO GCIO-WRK-STATUS-CODE.          00328800
328900     MOVE  'C5'                 TO GCIO-WRK-RECORD-TYPE.          00328900
329000     MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE.            00329000
329100     MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM.            00329100
329200     MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM.          00329200
329300     MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE.             00329300
329400     MOVE GCA-L-O-B             TO GCIO-WRK-LINE-OF-BUS.          00329400
329500     MOVE GCA-PROV-CTL          TO GCIO-WRK-PROVIDER-CONTROL.     00329500
329600     MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVL.  00329600
329700     MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN.            00329700
329800     MOVE GCA-BEN-PROV-ID       TO GCIO-WRK-PROVISION-ID.         00329800
329900     MOVE +9999999              TO GCIO-WRK-PROVISION-SLOT-NO.    00329900
330000     MOVE TABIDI                TO GCIO-WRK-TAB-PROVISION-ID.     00330000
330100     MOVE TABSLTNI              TO ACWA-DISPLAY-LEN-7.            00330100
330200     MOVE ACWA-DISPLAY-LEN-7    TO GCIO-WRK-TAB-PROV-SLOT-NO.     00330200
330300                                                                  00330300
330400 6200-900-EXIT. EXIT.                                             00330400
330500                                                                  00330500
330600/*****************************************************************00330600
330700*  XCTL TO MAIN MENU                                             *00330700
330800*                                                                *00330800
330900*    THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO   *00330900
331000*  OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE*00331000
331100*  XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE MENUS00331100
331200*  TO GET TO US; WE ARE A MODULE AT THE BOTTOM OF A PYRAMID TO GET00331200
331300*  HERE YOU MUST START AT THE TOP (THE MAIN MENU).               *00331300
331400******************************************************************00331400
331500 6400-000-XCTL-TO-MAIN-MENU     SECTION.                          00331500
331600 6400-010.                                                        00331600
331700                                                                  00331700
331800     MOVE WS-ABCODE-1DP1        TO WS-ABCODE.                     00331800
331900     MOVE WS-ABCODE-1DP1-MSG    TO WS-ABCODE-MSG.                 00331900
332000                                                                  00332000
332100     EXEC CICS  XCTL  PROGRAM('GCPSPGM')   END-EXEC.              00332100
332200                                                                  00332200
332300 6400-900-EXIT. EXIT.                                             00332300
332400                                                                  00332400
332500/*****************************************************************00332500
332600* 6500  SORT COMPRESS ALL LVL                                    *00332600
332700*                                                                *00332700
332800*    THIS ROUTINE WILL COPY ALL ENTRIES FROM THE TABULAR PORTION *00332800
332900*  TO A COPY OF THE TABULAR, THEN SORT THE COPY INTO ASCENDING   *00332900
333000*  SEQUENCE, ANY DUPLICATES ARE REMOVED FROM THE TABLE.          *00333000
333100******************************************************************00333100
333200 6500-000-SORT-COMPRESS-ALL-LVL SECTION.                          00333200
333300 6500-010.                                                        00333300
333400                                                                  00333400
333500        EXEC CICS GETMAIN                                         00333500
333600               SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)            00333600
333700               INITIMG(WS-HEX-00)                                 00333700
333800               LENGTH(WS-COPY-TABLE-LEN)                          00333800
333900               END-EXEC.                                          00333900
334000                                                                  00334000
334100        SET ACWA-COPY-TAB-PNTR        TO                          00334100
334200                 ADDRESS OF COPY-TABULAR-TABLE-AREA.              00334200
334300                                                                  00334300
334400     MOVE GAC-ENTRY-COUNT  TO  GAC-ENTRY-COUNT.                   00334400
334500     SET GAC-INDEX,  COPY-IDX  TO  1.                             00334500
334600                                                                  00334600
334700 6500-100-COPY-TABLE.                                             00334700
334800                                                                  00334800
334900     IF GAC-INDEX  NOT >  GAC-ENTRY-COUNT                         00334900
335000        MOVE GAC-ENTRY(GAC-INDEX)  TO COPY-TABULAR-TABLE(COPY-IDX)00335000
335100        SET GAC-INDEX,  COPY-IDX  UP BY  1                        00335100
335200        GO TO 6500-100-COPY-TABLE.                                00335200
335300                                                                  00335300
335400     SET  COPY-IDX  TO  1.                                        00335400
335500     SET  COPY-IDX2 TO  2.                                        00335500
335600                                                                  00335600
335700 6500-200-SORT-TABLE.                                             00335700
335800                                                                  00335800
335900     IF COPY-IDX2  >  GAC-ENTRY-COUNT                             00335900
336000        GO TO 6500-400-ARE-WE-DONE-SORTING.                       00336000
336100                                                                  00336100
336200     IF  COPY-SORTABLE-FLDS (COPY-IDX)  >                         00336200
336300                                   COPY-SORTABLE-FLDS (COPY-IDX2) 00336300
336400     THEN                                                         00336400
336500         MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO  WS-ENTRY         00336500
336600         MOVE COPY-TABULAR-TABLE (COPY-IDX2)                      00336600
336700                               TO  COPY-TABULAR-TABLE (COPY-IDX)  00336700
336800         MOVE WS-ENTRY  TO  COPY-TABULAR-TABLE (COPY-IDX2)        00336800
336900         SET COPY-IDX2  UP BY  1                                  00336900
337000         GO TO 6500-200-SORT-TABLE.                               00337000
337100                                                                  00337100
337200     IF COPY-SORTABLE-FLDS (COPY-IDX)  <                          00337200
337300                                    COPY-SORTABLE-FLDS(COPY-IDX2) 00337300
337400        SET COPY-IDX2  UP BY  1                                   00337400
337500        GO TO 6500-200-SORT-TABLE.                                00337500
337600                                                                  00337600
337700     SET COPY-IDX3,  COPY-IDX4  TO  COPY-IDX2.                    00337700
337800     SET COPY-IDX4  UP BY 1.                                      00337800
337900                                                                  00337900
338000 6500-300-ELIMINATE-DUPLICATES.                                   00338000
338100                                                                  00338100
338200     IF COPY-IDX4  NOT >  GAC-ENTRY-COUNT                         00338200
338300        MOVE COPY-TABULAR-TABLE (COPY-IDX4)  TO                   00338300
338400                                 COPY-TABULAR-TABLE (COPY-IDX3)   00338400
338500        SET COPY-IDX3,  COPY-IDX4  UP BY  1                       00338500
338600        GO TO 6500-300-ELIMINATE-DUPLICATES.                      00338600
338700                                                                  00338700
338800     SUBTRACT 1  FROM  GAC-ENTRY-COUNT.                           00338800
338900     GO TO 6500-200-SORT-TABLE.                                   00338900
339000                                                                  00339000
339100 6500-400-ARE-WE-DONE-SORTING.                                    00339100
339200                                                                  00339200
339300     IF COPY-IDX  <  GAC-ENTRY-COUNT                              00339300
339400        SET COPY-IDX   UP BY  1                                   00339400
339500        SET COPY-IDX2  TO COPY-IDX                                00339500
339600        SET COPY-IDX2  UP BY 1                                    00339600
339700        GO TO 6500-200-SORT-TABLE.                                00339700
339800                                                                  00339800
339900     MOVE GAC-ENTRY-COUNT  TO  GAC-ENTRY-COUNT.                   00339900
340000     SET GAC-INDEX,  COPY-IDX  TO  1.                             00340000
340100                                                                  00340100
340200 6500-500-MOVE-COPY-BACK.                                         00340200
340300                                                                  00340300
340400     IF GAC-INDEX  NOT >  GAC-ENTRY-COUNT                         00340400
340500        MOVE COPY-TABULAR-TABLE(COPY-IDX)  TO                     00340500
340600                                           GAC-ENTRY(GAC-INDEX)   00340600
340700        SET GAC-INDEX,  COPY-IDX  UP BY  1                        00340700
340800        GO TO 6500-500-MOVE-COPY-BACK.                            00340800
340900                                                                  00340900
341000     MOVE GAC-ENTRY-COUNT  TO  GAC-ENTRY-COUNT.                   00341000
341100     IF GAC-ENTRY-COUNT  NOT <  GC-GCTABULR-ADL-VARY-MAX-OCUR     00341100
341200        MOVE 'Y'  TO  ACWA-ERROR-SW.                              00341200
341300                                                                  00341300
341400 6500-900-EXIT. EXIT.                                             00341400
341500                                                                  00341500
341600/*****************************************************************00341600
341700* 7900  RESET ATTRIBUTES                                         *00341700
341800******************************************************************00341800
341900 7900-000-RESET-ATTRIBUTES      SECTION.                          00341900
342000 7900-010.                                                        00342000
342100                                                                  00342100
342200     MOVE DFHBMUNF                                                00342200
342300       TO BENVLQLA  COPAYINA  CSTCONTA  FAMINDIA  LOBA CONDLIFA   00342300
342400          PERIODA   PLCTRMTA  SRVGRUPA  PRTIMEFA  DEFINTNA        00342400
342500          CARYOVRA  INTRVALA  INTTYPEA  CLMLVLIA  BNMXVALA        00342500
342600          DAYFACIA  OVRDINDA  NEWVALUA  INTDESKA FYIVALA ASCDSCDA 00342600
342700          CONDALLA  CONDEXCA  CONDICDA  CONDTABA CONDMENA BENTYPA 00342700
342800          CONDEMCA  CONDEACA  CONDSMIA  CONDNSMA BISNDINA TIERCDA 00342800
342900          CONDDRGA  CONDALCA  CONDOBNA  CONDOBCA CONDMALA TIERLVA 00342900
343000          CONDCARA  CONDOBSA  CONDKDYA  CONDACCA CONDPECA         00343000
343100          CONDNEMA CONDSUIA  MANAPLIA  CONDTMJA  CONDINFA AGEQLLA 00343100
343200          IBGROPTA IPGNOPTA  IPGTOPTA  MFRMSLTA  PERTQALA AGEQLHA 00343200
343300          IDGDOPTA  IPGPOPTA  AGELIMLA  AGELIMHA  RELPINDA        00343300
343400          FEAKINDA  IPGSOPTA  ACCUMIDA  CAPINDA   SABDINDA.       00343400
343500                                                                  00343500
343600                                                                  00343600
343700     IF  DELADDO  =  'CHG/DEL'                                    00343700
343800     THEN                                                         00343800
343900         NEXT SENTENCE                                            00343900
344000     ELSE                                                         00344000
344100         GO TO 7900-900-EXIT.                                     00344100
344200                                                                  00344200
344300                                                                  00344300
344400     IF  CDEINDO = '+CDE+'                                        00344400
344500     THEN                                                         00344500
344600*---------------- HIGH-LIGHT CRITICAL DATA ELEMENT LABELS         00344600
344700         MOVE DFHBMABF TO DLOPTLTA  PERIOTA  BENVLQTA  LOTA       00344700
344800              AGELIMA     PLCTRTTA  FAMINDTA SRVGRUTA  CSTCOTTA   00344800
344900              AGEQLTA     COPAYITA  INTDESTA CONDTG1A  CONDTG2A   00344900
345000              BISNDITA                                            00345000
345100         IF INTDESKO  NOT =  IDPRODO                              00345100
345200            MOVE DFHBMASB  TO  IBGRIDA,  IPGNIDA,  IPGTIDA        00345200
345300                               IDGDIDA,  IPGPIDA,  IPGSIDA        00345300
345400            MOVE DFHBMABF  TO  IBGRSLTA, IPGNSLTA, IPGTSLTA       00345400
345500                               IDGDSLTA, IPGPSLTA, IPGSSLTA       00345500
345600            MOVE DFHBMUBF  TO  IBGROPTA, IPGNOPTA, IPGTOPTA       00345600
345700                               IDGDOPTA, IPGPOPTA, IPGSOPTA       00345700
345800         ELSE                                                     00345800
345900            MOVE DFHBMASF  TO  IBGRIDA,  IPGNIDA,  IPGTIDA        00345900
346000                               IDGDIDA,  IPGPIDA,  IPGSIDA        00346000
346100            MOVE DFHBMASF  TO  IBGRSLTA, IPGNSLTA, IPGTSLTA       00346100
346200                               IDGDSLTA, IPGPSLTA, IPGSSLTA       00346200
346300            MOVE DFHBMUNF  TO  IBGROPTA, IPGNOPTA, IPGTOPTA       00346300
346400                               IDGDOPTA, IPGPOPTA, IPGSOPTA       00346400
346500     ELSE                                                         00346500
346600         NEXT SENTENCE.                                           00346600
346700                                                                  00346700
346800                                                                  00346800
346900     IF  CDEINDO = '+CDE-'                                        00346900
347000     THEN                                                         00347000
347100*---------------- HIGH-LIGHT CRITICAL DATA ELEMENT LABELS         00347100
347200         MOVE DFHBMABF TO DLOPTLTA  PERIOTA  BENVLQTA  LOTA       00347200
347300              AGELIMA     PLCTRTTA  FAMINDTA SRVGRUTA  CSTCOTTA   00347300
347400              AGEQLTA     COPAYITA  INTDESTA CONDTG1A  CONDTG2A   00347400
347500              BISNDITA    DEFINTTA                                00347500
347600*---------------- AUTOSKIP AND FSET CRITICAL DATA ELEMENTS        00347600
347700         MOVE DFHBMASF TO DELOPTNA  PERIODA  BENVLQLA  LOBA       00347700
347800        AGELIMLA AGELIMHA PLCTRMTA  FAMINDIA SRVGRUPA  CSTCONTA   00347800
347900        AGEQLLA  AGEQLHA  COPAYINA  INTDESKA CONDALLA  CONDEXCA   00347900
348000                 BISNDINA CONDICDA  CONDTABA CONDMENA  CONDDRGA   00348000
348100                          CONDEACA  CONDEMCA CONDSMIA  CONDNSMA   00348100
348200                CONDLIFA  CONDALCA  CONDOBCA CONDOBNA  CONDMALA   00348200
348300                CONDTMJA  CONDCARA  CONDOBSA CONDKDYA  CONDACCA   00348300
348400                CONDINFA  CONDPECA  CONDNEMA CONDSUIA  DEFINTNA   00348400
348500         IF INTDESKO  NOT =  IDPRODO                              00348500
348600*--------- AUTOSKIP AND FSET CRITICAL DATA ELEMENTS               00348600
348700            MOVE DFHBMASF  TO  IBGROPTA,  IPGNOPTA,  IPGTOPTA     00348700
348800                               IDGDOPTA,  IPGPOPTA,  IPGSOPTA     00348800
348900            MOVE DFHBMABF  TO  IBGRIDA,  IBGRSLTA,                00348900
349000                       IPGNIDA,  IPGNSLTA,   IPGTIDA,  IPGTSLTA   00349000
349100                       IDGDIDA,  IDGDSLTA,   IPGPIDA,  IPGPSLTA   00349100
349200                       IPGSIDA,  IPGSSLTA                         00349200
349300            IF  ERRMSGO > SPACES                                  00349300
349400            THEN                                                  00349400
349500                NEXT SENTENCE                                     00349500
349600            ELSE                                                  00349600
349700                SET  WT-01-INDEX  TO  +08                         00349700
349800                MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO 00349800
349900         ELSE                                                     00349900
350000            MOVE DFHBMASF  TO  IBGRIDA,  IBGRSLTA,                00350000
350100                       IPGNIDA,  IPGNSLTA,   IPGTIDA,  IPGTSLTA   00350100
350200                       IDGDIDA,  IDGDSLTA,   IPGPIDA,  IPGPSLTA   00350200
350300                       IPGSIDA,  IPGSSLTA                         00350300
350400            MOVE DFHBMUNF  TO  IBGROPTA, IPGNOPTA, IPGTOPTA       00350400
350500                               IDGDOPTA, IPGPOPTA, IPGSOPTA       00350500
350600            IF  ERRMSGO > SPACES                                  00350600
350700            THEN                                                  00350700
350800                NEXT SENTENCE                                     00350800
350900            ELSE                                                  00350900
351000                SET  WT-01-INDEX  TO  +08                         00351000
351100                MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO 00351100
351200     ELSE                                                         00351200
351300         NEXT SENTENCE.                                           00351300
351400                                                                  00351400
351500                                                                  00351500
351600 7900-900-EXIT. EXIT.                                             00351600
351700                                                                  00351700
351800/*****************************************************************00351800
351900* 8000  XCTL SWITCH ADD DEL MODE                                 *00351900
352000*                                                                *00352000
352100*   THE OPERATOR WANTS TO SWITCH MODES, FROM DELETING ENTRIES TO *00352100
352200*  ADDING ENTRIES.  WE READ THE ALL LEVEL INTERNAL TABULAR & PASS*00352200
352300*  THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL     *00352300
352400*  TABULAR RECORD TO THE ADD PROGRAM.  (DEPENDING ON THE MENU THE*00352400
352500*  PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE   *00352500
352600*  IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).             *00352600
352700******************************************************************00352700
352800 8000-000-SWITCH-ADD-DEL-MODE   SECTION.                          00352800
352900 8000-010.                                                        00352900
353000                                                                  00353000
353100     IF DELADDI  =  'CHG/DEL'                                     00353100
353200        PERFORM 8100-000-DISPLAY-ADD-SCREEN.                      00353200
353300                                                                  00353300
353400     PERFORM 3100-000-READ-RECORD.                                00353400
353500     MOVE GAC-ENTRY-COUNT  TO  GAC-ENTRY-COUNT.                   00353500
353600                                                                  00353600
353700     IF  GAC-ENTRY-COUNT  >  1                                    00353700
353800     THEN                                                         00353800
353900         MOVE 'CHG/DEL'  TO  DELADDO                              00353900
354000         MOVE 'D'        TO  DELOLITO                             00354000
354100         MOVE SPACES     TO  COCURANO                             00354100
354200         MOVE DFHBMASK   TO  DLOPTLTA                             00354200
354300         MOVE DFHBMUNP   TO  DELOPTNA                             00354300
354400         SET  WT-01-INDEX                     TO  +20             00354400
354500         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO  ERRMSGO         00354500
354600         SET GAC-INDEX   TO 1                                     00354600
354700         PERFORM 4400-000-BUILD-DISPLAY                           00354700
354800     ELSE                                                         00354800
354900         SET  WT-01-INDEX                     TO  +12             00354900
355000         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO  ERRMSGO.        00355000
355100                                                                  00355100
355200 8000-900-EXIT. EXIT.                                             00355200
355300                                                                  00355300
355400/*****************************************************************00355400
355500* 8100  DISPLAY ADD SCREEN                                       *00355500
355600******************************************************************00355600
355700 8100-000-DISPLAY-ADD-SCREEN    SECTION.                          00355700
355800                                                                  00355800
355900     MOVE 'CHG/ADD'  TO  DELADDO.                                 00355900
356000     MOVE SPACES     TO  COCURANO.                                00356000
356100     MOVE DFHBMASD   TO  DLOPTLTA   DELOPTNA.                     00356100
356200     PERFORM 4100-000-DISPLAY-SKELETON.                           00356200
356300                                                                  00356300
356400 8100-900-EXIT. EXIT.                                             00356400
356500                                                                  00356500
356600/*****************************************************************00356600
356700* 9000  SEND ERASE THEN RETURN                                   *00356700
356800******************************************************************00356800
356900 9000-000-SEND-ERASE-RETURN     SECTION.                          00356900
357000 9000-010.                                                        00357000
357100                                                                  00357100
357200     MOVE DFHBMASD TO                                             00357200
357300                PERLITTA  FDLRCLTA  REININTA  MAXOVRTA            00357300
357400                PERLIMTA  FDLRCLIA  REININDA  MAXOVRDA            00357400
357500                TIMEDLRA  TIMEDOLA.                               00357500
357600                                                                  00357600
357700     MOVE -1  TO  ERRMSGL.                                        00357700
357800                                                                  00357800
357900     EXEC CICS  SEND   MAP ('GA1XI01')  ERASE  CURSOR             00357900
358000                MAPSET('GA1XSET')   END-EXEC.                     00358000
358100                                                                  00358100
358200*    EXEC CICS  RETURN   END-EXEC.                                00358200
358300     EXEC CICS  RETURN TRANSID('GA1D')                            00358300
358400                COMMAREA(DFHCOMMAREA)                             00358400
358500                LENGTH  (EIBCALEN)                                00358500
358600                END-EXEC.                                         00358600
358700                                                                  00358700
358800 9000-900-EXIT. EXIT.                                             00358800
358900                                                                  00358900
359000/*****************************************************************00359000
359100* 9010  SEND DATAONLY AND RETURN                                 *00359100
359200******************************************************************00359200
359300 9010-000-SEND-DATAONLY-RETURN  SECTION.                          00359300
359400 9010-010.                                                        00359400
359500                                                                  00359500
359600     MOVE -1  TO  ERRMSGL.                                        00359600
359700                                                                  00359700
359800     EXEC CICS  SEND   MAP ('GA1XI01')  DATAONLY  CURSOR          00359800
359900                MAPSET('GA1XSET')  END-EXEC.                      00359900
360000                                                                  00360000
360100*    EXEC CICS  RETURN   END-EXEC.                                00360100
360200     EXEC CICS  RETURN TRANSID('GA1D')                            00360200
360300                COMMAREA(DFHCOMMAREA)                             00360300
360400                LENGTH  (EIBCALEN)                                00360400
360500                END-EXEC.                                         00360500
360600                                                                  00360600
360700 9010-900-EXIT. EXIT.                                             00360700
360800                                                                  00360800
360900/*****************************************************************00360900
361000* 9200  GREGORIAN TO JULIAN                                      *00361000
361100*                                                                *00361100
361200*         MMDDYY---->YYDDD                                       *00361200
361300******************************************************************00361300
361400 9200-000-GREGORIAN-TO-JULIAN   SECTION.                          00361400
361500 9200-010.                                                        00361500
361600                                                                  00361600
361700     MOVE 'CNV'  TO  HGADATE-FUNC.                                00361700
361800     MOVE 'M'    TO  HGADATE-FORM1.                               00361800
361900     MOVE 'J'    TO  HGADATE-FORM2.                               00361900
362000     MOVE ZEROS  TO HGADATE-RETURN   HGADATE-AMOUNT.              00362000
362100                                                                  00362100
362200     EXEC  CICS LINK PROGRAM ('HGADATES')                         00362200
362300                     COMMAREA(HGADATES-COMMAREA)                  00362300
362400                     LENGTH  (LENGTH OF HGADATES-COMMAREA)        00362400
362500                     END-EXEC.                                    00362500
362600                                                                  00362600
362700                                                                  00362700
362800 9200-900-EXIT. EXIT.                                             00362800
362900                                                                  00362900
363000/*****************************************************************00363000
363100* 9300  JULIAN TO GREGORIAN                                      *00363100
363200*                                                                *00363200
363300*          YYDDD---->MMDDYY                                      *00363300
363400******************************************************************00363400
363500 9300-000-JULIAN-TO-GREGORIAN   SECTION.                          00363500
363600 9300-010.                                                        00363600
363700                                                                  00363700
363800     MOVE 'CNV'  TO  HGADATE-FUNC.                                00363800
363900     MOVE 'J'    TO  HGADATE-FORM1.                               00363900
364000     MOVE 'M'    TO  HGADATE-FORM2.                               00364000
364100     MOVE ZEROS  TO HGADATE-RETURN   HGADATE-AMOUNT.              00364100
364200                                                                  00364200
364300     EXEC  CICS LINK PROGRAM ('HGADATES')                         00364300
364400                     COMMAREA(HGADATES-COMMAREA)                  00364400
364500                     LENGTH  (LENGTH OF HGADATES-COMMAREA)        00364500
364600                     END-EXEC.                                    00364600
364700                                                                  00364700
364800 9300-900-EXIT. EXIT.                                             00364800
364900                                                                  00364900
365000/*****************************************************************00365000
365100* 9800  E R R O R   M S G   T H E N   A B E N D                   00365100
365200*                                                                *00365200
365300*    THIS ROUTINE DISPLAYS THE PREVIOUSLY BUILT ERROR MESSAGE    *00365300
365400*  AND THEN ABENDS USING THE ABEND CODE EARLIER DEFINED.         *00365400
365500******************************************************************00365500
365600 9800-000-ERROR-MSG-THEN-ABEND  SECTION.                          00365600
365700 9800-010.                                                        00365700
365800                                                                  00365800
365900     MOVE -1               TO MFRMSLTL.                           00365900
366000     MOVE WS-ABCODE-MSG    TO ERRMSGO.                            00366000
366100                                                                  00366100
366200     EXEC CICS  SEND   MAP ('GA1XI01')  ERASE  CURSOR  WAIT       00366200
366300                MAPSET('GA1XSET')   END-EXEC.                     00366300
366400                                                                  00366400
366500     EXEC CICS  ABEND   ABCODE(WS-ABCODE)  END-EXEC.              00366500
366600                                                                  00366600
366700 9800-900-EXIT. EXIT.                                             00366700
