000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID. GA1CPGM.                                             00000200
000300**** THIS IS A COBOL/2 PROGRAM ***                                00000300
000400 AUTHOR. D SECOR  -  A C I.                                       00000400
000500 DATE-WRITTEN.   11/09/84.                                        00000500
000600 DATE-COMPILED.                                                   00000600
000700     SKIP3                                                        00000700
000800******************************************************************00000800
000900*   GA1CPGM         ALL LEVEL TABULAR MAINTENANCE PROGRAM        *00000900
001000*       CONINSURANCE LIMITS ACCULATOR DESCRIPTOR TABULAR - #ACL  *00001000
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
003300*   FUNC CODE: GA1C                                              *00003300
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
007000* D365B 06/03/02  JP   ADD COMBINATION APPLIED IND (CAPI)        *00007000
007100*                                                                *00007100
007200* DXXX 11/15/01  AKK   ADD SUPPORT FO TWO NEW BITS, TWO FOR      *00007200
007300*                      EMER AND TWO FOR SERIOUS MENTAL ILLNESS   *00007300
007400*                                                                *00007400
007500* D352 09/19/00  GDM   ADD ACCUMULATOR IDENTIFIER                *00007500
007600*                                                                *00007600
007700* P????  07/07/00 GSP   ADDED LOGIC FOR NEW #IPGS INTERNAL       *00007700
007800*                       TABULAR.                                 *00007800
007900*                                                                *00007900
008000* P????  11/19/99 FRY   ADD LENGTH PARAMETER TO THE RETURN       *00008000
008100*                       COMMAND WHEN DFHCOMMAREA IS SPECIFIED.   *00008100
008200*                                                                *00008200
008300* D341   10/07/98  GDM  HIDE TIME/DOLLAR FIELD FROM SCREEN       *00008300
008400*                                                                *00008400
008500*  D341  09/25/98  GDM  ADD CARRY OVER INDICATOR                 *00008500
008600*                                                                *00008600
008700* 14726/ 04/29/98  AB   MODIFIED TO BECOME MILLENNIUM COMPLIANT  *00008700
008800* 15057                 AND TO ADD PACKAGE CODE, PLAN CODE, AND  *00008800
008900*                       INCREASE GROUP AND SECTION NUMBERS.      *00008900
009000*                                                                *00009000
009100* 14726/ 11/10/97 DAU ADDED CODE TO SUPPORT THE YEAR 2000 AND    *00009100
009200* 15057               THE EXPANSION OF THE GROUP SPECIFIC AND    *00009200
009300*                     CONTRACT KEY TO SUPPORT THE TEXAS MERGER.  *00009300
009400*                                                                *00009400
009500*  D303  02/03/97 DAU ADD FEAK INDICATOR                         *00009500
009600*                                                                *00009600
009700* 12262  02/28/92 TPM ADD NEW COND-BIT LIF  (LIFE-THREATING)     *00009700
009800*                     COND-LIFE-THREAT-BIT                       *00009800
009900*                                                                *00009900
010000*                                                                *00010000
010100*D12009 08/28/91  TPM   INCREASED THE FAMILY-RELATION FIELD      *00010100
010200*                           FROM ONE POSITION TO TWO POSITIONS.  *00010200
010300*                                                                *00010300
010400* 11836 07/09/91  ENW  INCLUDED THE FYI FIELD IN THE COMPARE     *00010400
010500*                      AREA.                                     *00010500
010600*                                                                *00010600
010700* 11154 02/19/91  NGE  REDUCE OCCUR MAX NUM FROM 46 TO 44.       *00010700
010800*                                                                *00010800
010900* 11154 01/17/91  NGE 1. ADD AGE-QUAL-IND-FROM AND AGE-QUAL-TO   *00010900
011000*                        TO ALL ACCUM TABULARS, AS CDE FIELDS.   *00011000
011100*                       2. REMOVE RELATIONSHIP-IND FROM CDE LOGIC*00011100
011200*                                                                *00011200
011300* 11154   10/23/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *00011300
011400* D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *00011400
011500* D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *00011500
011600* D270                  4. ADD AGE-LIMIT-FROM AND AGE-LIMIT-TO.  *00011600
011700*                       5. ADD RELATIONSHIP-IND (CDE ELEMENT).   *00011700
011800*                       6. ADD NEW INTERNAL TABS #IDGD AND #IPGP.*00011800
011900*                       7. >>> CONVERT TO COBOL/2 <<<.           *00011900
012000*                                                                *00012000
012100* PG008 07/18/89 NGE  CDE MESSAGE SHOULD BE DISPLAYED WHEN ADDING*00012100
012200*                     OCCURS IN ADD/DEL MODE.                    *00012200
012300*                                                                *00012300
012400* P8652 06/29/89 NGE   CORRECT AN ERROR IN THE TABULAR SCREEN    *00012400
012500*                      HEADER TITLE.                             *00012500
012600*                                                                *00012600
012700* D200 05/18/89  NGE   ADD TWO NEW COND-BITS TMJ AND INF         *00012700
012800*                      TEMPROMAND-JOINT AND INFERTILITY-COND.    *00012800
012900*                                                                *00012900
013000* ???? 03/13/89  ENW   CHANGED 'DFHBMASK' TO 'DFHBMASF' IN       *00013000
013100*                      7900-000-RESET-ATTRIBUTES SECTION BECAUSE *00013100
013200*                      FIELDS THAT WEREN'T BEING RETURNED WERE   *00013200
013300*                      CAUSING EDIT PROBLEMS.                    *00013300
013400*                                                                *00013400
013500* D201 01/12/89  ENW   ADDED LOGIC FOR BISCENDING INDICATOR.      00013500
013600*                                                                *00013600
013700* ????    09/14/88  NGE  FIX INTERNAL TABS DELETE LOGIC FOR      *00013700
013800*                        UPDATING CDE COUNTERS DEPENDING ON THE  *00013800
013900*                        INTRNL TAB RECORD NOT THE CDE STATUS    *00013900
014000*                        IN THE ACCUM RECORD ATTACHED. ALLOW     *00014000
014100*                        +CDE+ DISPLAY RETURNING FROM INTRNL PGM *00014100
014200*                                                                *00014200
014300* ????    08/03/88  NGE  FIX ADDING ACCURS LOGIC TO FLAG THE     *00014300
014400*                            ACCUMS AS A CDE & GAS2PGM INTERNAL  *00014400
014500*                            TAB LOGIC TO FLAG ITS ACCUM CDE     *00014500
014600*                            WHEN THE INTERNL FLAGED CDE.        *00014600
014700*                                                                *00014700
014800*                                                                *00014800
014900*  D126      02/24/88  JLA  1. CHANGE OPTION FILE SELECTION 'S'  *00014900
015000*                              TO 'A'.                           *00015000
015100*                                                                *00015100
015200* D143 01/29/88  DES  ADD CDE/NON-CDE CHANGES USING JERRY'S      *00015200
015300*                     SCHEME WHERE THE SPLIT IS PERFORMED        *00015300
015400*                     IN BATCH AND THEN MERGED BACK ONTO W/F     *00015400
015500*                                                                *00015500
015600*  ????      09/29/87  JLA  FIX EXISTING CDE PROBLEM IN THE      *00015600
015700*                             4600- SECTION THAT CAUSED THE CDE  *00015700
015800*                             MODIFIED STATUS TO BE SET.         *00015800
015900*                                                                *00015900
016000* N126 08/28/87  JLA  ADD LOGIC FOR SUICIDE BIT.                 *00016000
016100*                                                                *00016100
016200* N121 08/09/87  NE   ADD A NEW FIELD -DEFINITION-               *00016200
016300*                                                                *00016300
016400* D200 04/24/87  JLA  BREAK INTO MULTIPLE MODULES.               *00016400
016500*                                                                *00016500
016600*D0120 03/16/87  JLA  CHANGES FOR SINGLE TABULAR SUPPORT THAT    *00016600
016700*                     ARE EXECUTED FROM TRANSACTION GTM1:        *00016700
016800*                     1. WHEN CHECKING ENTRY TRANSACTION CODE    *00016800
016900*                        TREAT GTM1 THE SAME AS GC4A.            *00016900
017000*                     2. PF1/PF13 - TREAT THE SAME AS IF GC4A    *00017000
017100*                        HAD CALLED, XCTL TO ADD SCREEN PROGRAM  *00017100
017200*                     3. PF3/PF15 - CONSTRUCT COMMAREA AS IF     *00017200
017300*                        GC4A HAD CALLED, XCTL TO GTM1PGM.       *00017300
017400*                     4. ALLOW ATTACHMENT (MAP FROM) OF PROD-    *00017400
017500*                        UCTION TABULARS, BUT PROHIBIT ATTACH-   *00017500
017600*                        ING SINGLE TABULARS UNDER SINGLE TAB-   *00017600
017700*                        ULAR SUPPORT.  DON'T CONSTRUCT C3       *00017700
017800*                        WORKFILE RECORD.  DON'T PASS CONTROL    *00017800
017900*                        TO INTERNAL TABULAR MAINTENANCE PGM.    *00017900
018000*                        DON'T CHANGE INTERNAL SLOT# ON SCREEN   *00018000
018100*                        TO ALL 9K NUMBER.                       *00018100
018200*                     5. PROHIBIT INTERNAL TAB CHANGES UNDER     *00018200
018300*                        STS.                                    *00018300
018400*                     6. PROHIBIT MAPPING FROM SKELETON UNDER    *00018400
018500*                        STS.                                    *00018500
018600*                                                                *00018600
018700*N106    02/26/87 RKH   ADDED LOGIC FOR THE FYI FIELD WHICH IS   *00018700
018800*N118                      TO BE VALIDATED & THE LOGIC TO ONLY   *00018800
018900*                          DISPLAY THE TABULAR OCCURANCE NUMBER. *00018900
019000*                                                                *00019000
019100*CDEL502 9/29/86 JLA    1. CHANGE COPY-SORTABLE-FLDS FROM X(128  *00019100
019200*                          TO X(125) AND REPLACE LAST THREE      *00019200
019300*                          BYTES WITH COPY-SORT-FYI X(3) NOT     *00019300
019400*                          INCLUDED IN TABULAR ENTRY SORT.       *00019400
019500*                       2. INITIAL THE CDE STATUS IN ANY         *00019500
019600*                          WORKFILE RECORDS CREATED TO \
019700*                       3. IF PRODUCTION TABULAR RECORD IS       *00019700
019800*                          BEING CHANGED AND CONTAINS CRITICAL   *00019800
019900*                          DATA ELEMENTS:                        *00019900
020000*                          A. INITIAL SCREEN :                   *00020000
020100*                             1) CDE STATUS(\
020200*                                 - HIGH-LIGHT CDE LABELS,       *00020200
020300*                                   SHOW +CDE+ INDICATOR.        *00020300
020400*                             2) CDE STATUS NOT (\
020500*                                 - HIGH-LIGHT CDE LABELS,       *00020500
020600*                                   HIGH-LIGHT AND PROTECT CDE   *00020600
020700*                                   ELEMENTS,                    *00020700
020800*                                   SHOW +CDE+ INDICATOR.        *00020800
020900*                          B. IF CDE ELEMENTS ARE CHANGED, SET   *00020900
021000*                             WORKFILE TABULAR CDE STATUS CODE   *00021000
021100*                             TO \
021200*                             OR GROUP SPECIFIC CONTROL          *00021200
021300*                             RECORD CDE STATUS APPROPRIATELY,   *00021300
021400*                             ISSUE CDE CHANGE MESSAGE AND       *00021400
021500*                             POSITION CURSOR ON +CDE+.  THE     *00021500
021600*                             OPERATOR THEN ADVANCES TO NEXT     *00021600
021700*                             SCREEN BY PRESSING ENTER A SECOND  *00021700
021800*                             TIME.                              *00021800
021900*                                                                *00021900
022000*CDEL501 8/22/86 JLA    DETERMINE IF POTENTIALLY CRITICAL DATA   *00022000
022100*                       ELEMENTS ARE CRITICAL BASED ON THE TRANS *00022100
022200*                       ROUTING FILE (PGM=GCTRSRT).  IF THEY     *00022200
022300*                       ARE CRITICAL AND THE USER IS DOING A     *00022300
022400*                       CHANGE TO A WORKFILE GROUP SPECIFIC      *00022400
022500*                       RECORD, PROTECT THE CRITICAL DATA ELE-   *00022500
022600*                       MENT ON THE SCREEN.                      *00022600
022700*  ?   08/12/86  AHL/DF MODIFIED 1100- ROUTINE SO THAT IT        *00022700
022800*                       FINISHES VALIDATING EACH DATA ELEMENT    *00022800
022900*                       IN THE CORRECT SEQUENCE ACCORDING TO     *00022900
023000*                       THE SCREEN LAYOUT.                       *00023000
023100*                                                                *00023100
023200*D094  08/04/86  AHL  REVISED 'NEG' LOGIC TO LET OPERATOR USE    *00023200
023300*                     EITHER 'NEG' OR DOLLARS & CENTS WITH       *00023300
023400*                     DECIMAL POINT FOR VALUE LIMIT FIELD WHEN   *00023400
023500*                     VALUE QUALIFIER = '5'.                     *00023500
023600*                                                                *00023600
023700*N112  08/01/86  AMJ  ADDED DAY FACTOR INDICATOR                 *00023700
023800*                                                                *00023800
023900*      07/30/86  AMJ  FIXED ERROR MESSAGES                       *00023900
024000*                                                                *00024000
024100*N108  07/28/86  RKH  ADDED TWO NEW CONDITION BITS               *00024100
024200*                     PRE-EXISTING CONDITIONS                    *00024200
024300*                     NON-EMERGENCY CONDITION.                   *00024300
024400*                                                                *00024400
024500*D094  07/23/86  AKM  ALLOWED 10 POSITIONS FOR VALUE LIMIT       *00024500
024600*                     FIELD SO THAT OPERATORS CAN ENTER          *00024600
024700*                     1 MILLION AS '1000000.00'.                 *00024700
024800*                                                                *00024800
024900*      06/26/86  JTC  ADDED LOGICAL EDITS                        *00024900
025000*                                                                *00025000
025100*      06/19/86  JTC  MOVED PF4/PF16 LOGIC TO AFTER VALIDATION   *00025100
025200*                     SO THAT INCORRECT RECORDS WOULD NOT BE     *00025200
025300*                     ADDED TO THE FILE.  ADDED AN INVALID       *00025300
025400*                     PF MESSAGE TO COVER THE ABOVE CASE.        *00025400
025500*                                                                *00025500
025600*                     ADDED A CHANGE TO ALLOW SCROLLING FORWARD  *00025600
025700*                     IF THE ONLY THING 'WRONG' IS AN EMPTY      *00025700
025800*                     TABLE.                                     *00025800
025900*                                                                *00025900
026000*                     FIXED THE SCREEN NOT BEING REFRESHED       *00026000
026100*                     PROPERLY FOLLOWING AN ADD WITH PF4/PF16    *00026100
026200*                                                                *00026200
026300*P495  05/30/86  AMJ  CHANGED TO ALLOW IPGT AND IPGN AT THE      *00026300
026400*                     SAME TIME                                  *00026400
026500*                                                                *00026500
026600*M106  05/30/86  AMJ  FIXED ATTRIBUTE ON ADD TO/OVERLAY FIELD    *00026600
026700*                                                                *00026700
026800*      05/14/86  MDD  CHANGED THE SEQUENCE OF THE EDITS TO BE    *00026800
026900*                      IN SYNC WITH THE SCREEN.                  *00026900
027000*                                                                *00027000
027100*      02/21/86  MDD  ADDED VALIDATION FOR FOLLOWING FIELDS:     *00027100
027200*                     'ADD-TO-OVERLAY INDICATOR',                *00027200
027300*                     'BENEFIT PERIOD',                          *00027300
027400*                     'FAMILY OR INDIVIDUAL INDICATOR',          *00027400
027500*                     'LINE OF BUSINESS',                        *00027500
027600*                     'INTERNAL DESCRIPTOR',                     *00027600
027700*                     'SERVICE GROUP',                           *00027700
027800*                     'CO-PAY INDICATOR',                        *00027800
027900*                     'COST CONTAINMENT INDICATOR',              *00027900
028000*                     'REINSTATEMENT INDICATOR',                 *00028000
028100*                     'BENEFIT PERIOD TIME QUALIFIER',           *00028100
028200*                     'BAMA BENEFIT PERIOD OVERRIDE',            *00028200
028300*                     'INTERVAL TYPE',                           *00028300
028400*                     'INTERVAL OVERRIDE INDICATOR',             *00028400
028500*                     'PLACE OF TREATMENT INDICATOR',            *00028500
028600*                     'VALUE QUALIFIER'                          *00028600
028700*                                                                *00028700
028800*      01/15/86  RKH   ADDED CODE FOR THE NEG VALUE LIMIT        *00028800
028900*                                                                *00028900
029000*      11/18/85  LET   ADDED CODE FOR THE NEW CONDITION BIT      *00029000
029100*                      NAMED ACCIDENT.                           *00029100
029200*                                                                *00029200
029300*      11/08/85  ENW   REVISED LOGIC TO ACCEPT SPACES IN THE     *00029300
029400*                      INTERNAL DESCRIPTOR FIELD INSTEAD OF      *00029400
029500*                      ZEROS.  ALSO ADDED NEW COPY MEMBER        *00029500
029600*                      'GCVALTAB'.                               *00029600
029700*                                                                *00029700
029800*            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *00029800
029900*                                                                *00029900
030000*            05-07-07   LR    RECOMPILE FOR CHANGES IN GASEDIT1  *00030000
030100*                                                                *00030100
030000*            10-15-10  MJL    ADD 'UNL' LOGID.                   *00030110
030100*                                                                *00030120
      * P21595 09/19/16   HSB CHANGES FOR GCPS NEW FIELDS BENEFIT      *00030130
      *                       TYPE CODE,TIER CODE,TIER LEVEL.          *00030140
SI0724*                                                                *00030141
SI0724* P56703  05/08/24  SI  CHANGES FOR PEAQ COPYBOOK EXPANSION      *00030150
SI0724*                       COPY ABM, ACP, ACL, ADL, AOL,            *00030160
SI0724*                       GCCDRLEN                                 *00030170
030200******************************************************************00030200
030300    SKIP3                                                         00030300
030400 ENVIRONMENT DIVISION.                                            00030400
030500/    D A T A   D I V I S I O N                                    00030500
030600 DATA DIVISION.                                                   00030600
030700 WORKING-STORAGE SECTION.                                         00030700
030800 01  WS-BEGIN                    PIC X(24)  VALUE                 00030800
030900     '***GA1CPGM WS BEGINS***'.                                   00030900
031000                                                                  00031000
031100*     T I T L E   L I N E S                                       00031100
031200 01  WS-TITLE-LINES.                                              00031200
031300 COPY GCMHLINE.                                                   00031300
031400*****05  GROUP-SPECIFIC-TITLE-LINE       PIC X(42)                00031400
031500*      VALUE ' GROUP SPEC. ALL-LEVEL TABULAR MAINTENANCE'.        00031500
031600*    05  GROUP-SPECIFIC-ID-LINE.                                  00031600
031700*      10  FILLER                        PIC X(20)                00031700
031800*        VALUE 'GROUP SPECIFIC ID= '.                             00031800
031900*      10  FILLER                        PIC X(5) VALUE 'GRP= '.  00031900
032000*      10  GRP-SPEC-GROUP-NO             PIC X(6).                00032000
032100*      10  FILLER                        PIC X(6) VALUE ' SEC= '. 00032100
032200*      10  GRP-SPEC-SECTION-NO           PIC X(4).                00032200
032300*      10  FILLER                        PIC X(5) VALUE ' FR= '.  00032300
032400*      10  GRP-SPEC-FAM-REL-LVL          PIC XX.                  00032400
032500*      10  FILLER                        PIC X(7) VALUE ' EFDT= '.00032500
032600*      10  GRP-SPEC-EFF-DATE             PIC X(6).                00032600
032700*    05  CONTRACT-TITLE-LINE             PIC X(42)                00032700
032800*      VALUE '   CONTRACT ALL-LEVEL TABULAR MAINTENANCE'.         00032800
032900*    05  CONTRACT-ID-LINE.                                        00032900
033000*      10  FILLER                      PIC X(14)                  00033000
033100*        VALUE 'CONTRACT ID= '.                                   00033100
033200*      10  FILLER                      PIC X(5) VALUE 'GRP= '.    00033200
033300*      10  CONTRACT-GROUP-NO           PIC X(6).                  00033300
033400*      10  FILLER                      PIC X(6) VALUE ' SEC= '.   00033400
033500*      10  CONTRACT-SECTION-NO         PIC X(4).                  00033500
033600*      10  FILLER                      PIC X(6) VALUE ' LOB= '.   00033600
033700*      10  CONTRACT-LOB                PIC X.                     00033700
033800*      10  FILLER                      PIC X(6) VALUE ' PRV= '.   00033800
033900*      10  CONTRACT-PROV-CTL           PIC XX.                    00033900
034000*      10  FILLER                      PIC X(5) VALUE ' FR= '.    00034000
034100*      10  CONTRACT-FAM-REL-LVL        PIC XX.                    00034100
034200*      10  FILLER                      PIC X(7) VALUE ' EFDT= '.  00034200
034300*      10  CONTRACT-EFF-DATE               PIC X(6).              00034300
034400*    05  BENEFIT-PROVISION-TITLE-LINE    PIC X(42)                00034400
034500*      VALUE '   BEN. PROV ALL-LEVEL TABULAR MAINTENANCE'.        00034500
034600*    05  BENEFIT-PROVISION-ID-LINE.                               00034600
034700*      10  FILLER                      PIC X(5) VALUE 'GRP= '.    00034700
034800*      10  BEN-PROV-GROUP-NO           PIC X(6).                  00034800
034900*      10  FILLER                      PIC X(6) VALUE ' SEC= '.   00034900
035000*      10  BEN-PROV-SECTION-NO         PIC X(4).                  00035000
035100*      10  FILLER                      PIC X(6) VALUE ' LOB= '.   00035100
035200*      10  BEN-PROV-LOB                PIC X.                     00035200
035300*      10  FILLER                      PIC X(6) VALUE ' PRV= '.   00035300
035400*      10  BEN-PROV-PROV-CTL           PIC XX.                    00035400
035500*      10  FILLER                      PIC X(5) VALUE ' FR= '.    00035500
035600*      10  BEN-PROV-FAM-REL-LVL        PIC XX.                    00035600
035700*      10  FILLER                      PIC X(7) VALUE ' EFDT= '.  00035700
035800*      10  BEN-PROV-EFF-DATE           PIC X(6).                  00035800
035900*      10  FILLER                      PIC X(8) VALUE ' BPVID= '. 00035900
036000*      10  BEN-PROV-ID-NO              PIC X(6).                  00036000
036100*    05  ACL-TITLE-LINE                PIC X(26)                  00036100
036200*********VALUE '    COINSURANCE LIMITS    '.                      00036200
036300/     A L T E R N A T I V E   W O R K F I L E   K E Y S           00036300
036400 01  FILLER                      PIC X(32)  VALUE                 00036400
036500     '*** ALTERNATIVE WORKFILE KEY ***'.                          00036500
036600 01  SAVE-WS-ALT-WORKFILE-KEYS.                                   00036600
036700     05 FILLER                   PIC X(63) VALUE SPACES.          00036700
036800                                                                  00036800
036900 01  WS-ALT-WORKFILE-KEYS.                                        00036900
037000 COPY GCWRKKEY.                                                   00037000
037100                                                                  00037100
037200                                                                  00037200
037300/    D A T E   F O R M A T T I N G   A R E A                      00037300
037400 01  HGADATES-COMMAREA.                                           00037400
037500 COPY HGCDAT01.                                                   00037500
037600                                                                  00037600
037700*  *** WORKFIELDS, AND SWITCHES **                                00037700
037800 01  WS-WORK-FIELDS.                                              00037800
037900                                                                  00037900
038000     05  GCTRSRT-COMMAREA-LEN      PIC S9(4)  COMP VALUE +100.    00038000
038100     05  WS-HEX-00                     PIC X    VALUE LOW-VALUE.  00038100
038200     05  WS-ONE-LOW                    PIC X VALUE LOW-VALUES.    00038200
038300     05  SAVE-COPY-FROM-SLOT           PIC 9(7).                  00038300
038400                                                                  00038400
038500     05  WS-CDE-REQUEST-CODES.                                    00038500
038600         10  WS-REQUEST-4500-CDE-PROTECT    PIC X(4) VALUE '4500'.00038600
038700         10  WS-REQUEST-4600-CDE-STATUS     PIC X(4) VALUE '4600'.00038700
038800         10  WS-REQUEST-4700-CNTL-UPDATE    PIC X(4) VALUE '4700'.00038800
038900                                                                  00038900
039000*     I N T E R N A L   T A B U L A R   P R O G R A M   N A M E   00039000
039100 01  WS-INTERNAL-TABULAR-PGM-ID        PIC X(8).                  00039100
039200                                                                  00039200
039300                                                                  00039300
039400** ***ALL LEVEL TABULAR ENTRY SAVED HERE DURING SORT ***          00039400
039500 01  WS-ENTRY                          PIC X(176).                00039500
039600     SKIP3                                                        00039600
039700/    A T T R I B U T E S                                          00039700
039800 COPY DFHBMSCA.                                                   00039800
039900     02  DFHBMABF                PIC X VALUE '9'.                 00039900
040000/    A T T E N T I O N   I D E N T I F I E R S                    00040000
040100 COPY DFHAID.                                                     00040100
040200/    R E C O R D   L E N G T H S                                  00040200
040300                                                                  00040300
040400 01  WS-RECORD-LENGTHS.                                           00040400
040500*   05 WS-COMM-KEY-PNTR-LEN           PIC S9(4) COMP  VALUE +4.   00040500
040600*   05 GAS2UPD-COMMAREA-LEN           PIC S9(4) COMP  VALUE +420. 00040600
040700*   05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP  VALUE +100. 00040700
SI0724*   05 WS-COPY-TABLE-LEN              PIC S9(4) COMP  VALUE +7744.00040800
SI0724    05 WS-COPY-TABLE-LEN              PIC S9(4) COMP VALUE +30800.00040810
040900    05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP  VALUE +0.   00040900
041000    05 WS-IO-PARM-WRK-BEN-PROV-LEN    PIC S9(4) COMP  VALUE +0.   00041000
041100    05 WS-IO-PARM-WRK-CONTRACT-LEN    PIC S9(4) COMP  VALUE +0.   00041100
041200    05 WS-IO-PARM-WRK-CONTROL-LEN     PIC S9(4) COMP  VALUE +0.   00041200
041300    05 WS-IO-PARM-WRK-GRP-SPEC-LEN    PIC S9(4) COMP  VALUE +0.   00041300
041400    05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP  VALUE +0.   00041400
041500    05 WS-WRK-BEN-PROV-LEN            PIC S9(4) COMP  VALUE +0.   00041500
041600    05 WS-WRK-CONTRACT-LEN            PIC S9(4) COMP  VALUE +0.   00041600
041700    05 WS-WRK-GRP-SPEC-LEN            PIC S9(4) COMP  VALUE +0.   00041700
041800                                                                  00041800
041900******************************************************************00041900
042000** REQUIRED FOR N118 - DISPLAY OF TABULAR OCCURS (INDEX)        **00042000
042100******************************************************************00042100
042200 01  CURNT-OCURS-BIN             PIC 9(4)  COMP.                  00042200
042300 01  CURNT-OCURS-PKD             PIC 9(4).                        00042300
042400 01  CURNT-OCURS-ALH      REDEFINES   CURNT-OCURS-PKD.            00042400
SI0724*    05  FILLER                  PIC XX.                          00042500
SI0724*    05  CURNT-OCCURS-OUT        PIC XX.                          00042600
SI0724     05  FILLER                  PIC X.                           00042601
SI0724     05  CURNT-OCCURS-OUT        PIC XXX.                         00042610
042700                                                                  00042700
042800 01  TOTAL-OCURS-UNK             PIC 9(5).                        00042800
042900 01  TOTAL-OCURS-ALH      REDEFINES   TOTAL-OCURS-UNK.            00042900
SI0724*    05  FILLER                  PIC XXX.                         00043000
SI0724*    05  TOTAL-OCCURS-OUT        PIC XX.                          00043100
SI0724     05  FILLER                  PIC XX.                          00043110
SI0724     05  TOTAL-OCCURS-OUT        PIC XXX.                         00043120
043200/                                                                 00043200
043300 01  WS-GC-RECORD-LENGTHS.                                        00043300
043400     COPY GCCDRLEN.                                               00043400
043500/    A B E N D   A R E A                                          00043500
043600                                                                  00043600
043700 01  WS-01-ABEND-AREA.                                            00043700
043800     05  FILLER                   PIC X(16)  VALUE                00043800
043900         '** ABEND AREA **'.                                      00043900
044000                                                                  00044000
044100     05  WS-ABCODE-CODES-AND-MSG.                                 00044100
044200         10  WS-ABCODE                  PIC X(04)  VALUE  SPACES. 00044200
044300         10  WS-ABCODE-MSG              PIC X(79)  VALUE  SPACES. 00044300
044400                                                                  00044400
044500         10  WS-ABCODE-1CC1             PIC X(04)  VALUE  '1CC1'. 00044500
044600         10  WS-ABCODE-1CC1-MSG         PIC X(79)  VALUE          00044600
044700             '*** INVALID PARAMETER LENGTH FOUND ***              00044700
044800-            '                           '.                       00044800
044900         10  WS-ABCODE-1CC2             PIC X(04)  VALUE  '1CC2'. 00044900
045000         10  WS-ABCODE-1CC2-MSG         PIC X(79)  VALUE          00045000
045100             '*** WRONG RECORD STATUS PASSED TO THIS PGM ***      00045100
045200-            '                           '.                       00045200
045300         10  WS-ABCODE-1CC3             PIC X(04)  VALUE  '1CC3'. 00045300
045400         10  WS-ABCODE-1CC3-MSG         PIC X(79)  VALUE          00045400
045500             '*** WRONG RECORD TYPE PASSED TO THIS PGM ***        00045500
045600-            '                           '.                       00045600
045700         10  WS-ABCODE-1CF1             PIC X(04)  VALUE  '1CF1'. 00045700
045800         10  WS-ABCODE-1CF1-MSG         PIC X(79)  VALUE          00045800
045900             '*** A SKELETON CAN NOT BE FOUND FOR AN INTERNAL TABU00045900
046000-            'LAR.  CONTACT SYSTEMS ***  '.                       00046000
046100         10  WS-ABCODE-1CF2             PIC X(04)  VALUE  '1CF2'. 00046100
046200         10  WS-ABCODE-1CF2-MSG         PIC X(79)  VALUE          00046200
046300             '*** INVALID INTERNAL TABULAR SITUATION FOUND. PLEASE00046300
046400-            ' CONTACT SYSTEMS ***       '.                       00046400
046500         10  WS-ABCODE-1CF3             PIC X(04)  VALUE  '1CF3'. 00046500
046600         10  WS-ABCODE-1CF3-MSG         PIC X(79)  VALUE          00046600
046700             '*** INVALID INTERNAL TABULAR SITUATION FOUND. PLEASE00046700
046800-            ' CONTACT SYSTEMS ***       '.                       00046800
046900         10  WS-ABCODE-1CF4             PIC X(04)  VALUE  '1CF4'. 00046900
047000         10  WS-ABCODE-1CF4-MSG         PIC X(79)  VALUE          00047000
047100             '*** ERROR REWRITING ALL LEVEL TABULAR.  PLEASE CONTA00047100
047200-            'CT SYSTEMS ***             '.                       00047200
047300         10  WS-ABCODE-1CF5             PIC X(04)  VALUE  '1CF5'. 00047300
047400         10  WS-ABCODE-1CF5-MSG         PIC X(79)  VALUE          00047400
047500             'THE INTERNAL TABULAR CAN NOT BE READ FROM THE WORKFI00047500
047600-            'LE.  PLEASE CONTACT SYSTEMS'.                       00047600
047700         10  WS-ABCODE-1CF6             PIC X(04)  VALUE  '1CF6'. 00047700
047800         10  WS-ABCODE-1CF6-MSG         PIC X(79)  VALUE          00047800
047900             'THE INTERNAL TABULAR CAN NOT BE ADDED TO THE WORKFIL00047900
048000-            'E.  PLEASE CONTACT SYSTEMS '.                       00048000
048100         10  WS-ABCODE-1CF7             PIC X(04)  VALUE  '1CF7'. 00048100
048200         10  WS-ABCODE-1CF7-MSG         PIC X(79)  VALUE          00048200
048300             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00048300
048400-            ' SYSTEMS ***               '.                       00048400
048500         10  WS-ABCODE-1CF9             PIC X(04)  VALUE  '1CF9'. 00048500
048600         10  WS-ABCODE-1CF9-MSG         PIC X(79)  VALUE          00048600
048700             'THE INTERNAL TABULAR CAN NOT BE ADDED TO THE WORFILE00048700
048800-            '.  PLEASE CONTACT SYSTEMS  '.                       00048800
048900         10  WS-ABCODE-1CFA             PIC X(04)  VALUE  '1CFA'. 00048900
049000         10  WS-ABCODE-1CFA-MSG         PIC X(79)  VALUE          00049000
049100             '*** THE INTERNAL TABULAR CAN NOT BE DELETED, PLEASE 00049100
049200-            'CONTACT SYSTEMS ***        '.                       00049200
049300         10  WS-ABCODE-1CFB             PIC X(04)  VALUE  '1CFB'. 00049300
049400         10  WS-ABCODE-1CFB-MSG         PIC X(79)  VALUE          00049400
049500             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00049500
049600-            ' SYSTEMS ***               '.                       00049600
049700         10  WS-ABCODE-1CFC             PIC X(04)  VALUE  '1CFC'. 00049700
049800         10  WS-ABCODE-1CFC-MSG         PIC X(79)  VALUE          00049800
049900             '*** ERROR WHEN DELETING INTERNAL TAB.  PLEASE CONTAC00049900
050000-            'T SYSTEMS ***              '.                       00050000
050100         10  WS-ABCODE-1CFJ             PIC X(04)  VALUE  '1CFJ'. 00050100
050200         10  WS-ABCODE-1CFJ-MSG         PIC X(79)  VALUE          00050200
050300             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00050300
050400-            ' SYSTEMS ***               '.                       00050400
050500         10  WS-ABCODE-1CFK             PIC X(04)  VALUE  '1CFK'. 00050500
050600         10  WS-ABCODE-1CFK-MSG         PIC X(79)  VALUE          00050600
050700             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00050700
050800-            ' SYSTEMS ***               '.                       00050800
050900         10  WS-ABCODE-1CFL             PIC X(04)  VALUE  '1CFL'. 00050900
051000         10  WS-ABCODE-1CFL-MSG         PIC X(79)  VALUE          00051000
051100             '*** ERROR READING GROUP SPECIFIC RECORD TO RETURN TO00051100
051200-            'MENU.  CONTACT SYSTEMS *** '.                       00051200
051300         10  WS-ABCODE-1CFM             PIC X(04)  VALUE  '1CFM'. 00051300
051400         10  WS-ABCODE-1CFM-MSG         PIC X(79)  VALUE          00051400
051500             '*** ERROR READING CONTRACT MASTER TO RETURN TO THE  00051500
051600-            'MENU.  CONTACT SYSTEMS *** '.                       00051600
051700         10  WS-ABCODE-1CFN             PIC X(04)  VALUE  '1CFN'. 00051700
051800         10  WS-ABCODE-1CFN-MSG         PIC X(79)  VALUE          00051800
051900             '*** ERROR READING BENEFIT PROV RECORD TO RETURN TO M00051900
052000-            'ENU.  CONTACT SYSTEMS ***  '.                       00052000
052100         10  WS-ABCODE-1CFO             PIC X(04)  VALUE  '1CFO'. 00052100
052200         10  WS-ABCODE-1CFO-MSG         PIC X(79)  VALUE          00052200
052300             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00052300
052400-            ' SYSTEMS ***               '.                       00052400
052500         10  WS-ABCODE-1CFP             PIC X(04)  VALUE  '1CFP'. 00052500
052600         10  WS-ABCODE-1CFP-MSG         PIC X(79)  VALUE          00052600
052700             '*** ERROR READING W/F CONTROL RECORD. PLEASE CONTACT00052700
052800-            ' SYSTEMS ***               '.                       00052800
052900         10  WS-ABCODE-1CFQ             PIC X(04)  VALUE  '1CFQ'. 00052900
053000         10  WS-ABCODE-1CFQ-MSG         PIC X(79)  VALUE          00053000
053100             '*** ERROR REWRITING W/F CONTROL RECORD. PLEASE CONTA00053100
053200-            'CT SYSTEMS ***             '.                       00053200
053300         10  WS-ABCODE-1CFR             PIC X(04)  VALUE  '1CFR'. 00053300
053400         10  WS-ABCODE-1CFR-MSG         PIC X(79)  VALUE          00053400
053500             '*** ERROR READING W/F ALL LVL TAB.    PLEASE CONTACT00053500
053600-            ' SYSTEMS ***               '.                       00053600
053700         10  WS-ABCODE-1CFS             PIC X(04)  VALUE  '1CFS'. 00053700
053800         10  WS-ABCODE-1CFS-MSG         PIC X(79)  VALUE          00053800
053900             '*** ERROR REWRITING W/F ALL LVL TAB.  PLEASE CONTACT00053900
054000-            ' SYSTEMS ***               '.                       00054000
054100         10  WS-ABCODE-1CFT             PIC X(04)  VALUE  '1CFT'. 00054100
054200         10  WS-ABCODE-1CFT-MSG         PIC X(79)  VALUE          00054200
054300             '*** ERROR READING W/F CONTROL RECORD. PLEASE CONTACT00054300
054400-            ' SYSTEMS ***               '.                       00054400
054500         10  WS-ABCODE-1CFU             PIC X(04)  VALUE  '1CFU'. 00054500
054600         10  WS-ABCODE-1CFU-MSG         PIC X(79)  VALUE          00054600
054700             '*** ERROR REWRITING W/F CONTROL RECORD. PLEASE CONTA00054700
054800-            'CT SYSTEMS ***             '.                       00054800
054900         10  WS-ABCODE-1CL1             PIC X(04)  VALUE  '1CL1'. 00054900
055000         10  WS-ABCODE-1CL1-MSG         PIC X(79)  VALUE          00055000
055100             '*** THE OCCURS WE ARE TO UPDATE HAS BEEN DELETED ***00055100
055200-            '                           '.                       00055200
055300         10  WS-ABCODE-1CL2             PIC X(04)  VALUE  '1CL2'. 00055300
055400         10  WS-ABCODE-1CL2-MSG         PIC X(79)  VALUE          00055400
055500             '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTE00055500
055600-            'MS ***                     '.                       00055600
055700         10  WS-ABCODE-1CL3             PIC X(04)  VALUE  '1CL3'. 00055700
055800         10  WS-ABCODE-1CL3-MSG         PIC X(79)  VALUE          00055800
055900             '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTE00055900
056000-            'EMS ***                    '.                       00056000
056100         10  WS-ABCODE-1CL4             PIC X(04)  VALUE  '1CL4'. 00056100
056200         10  WS-ABCODE-1CL4-MSG         PIC X(79)  VALUE          00056200
056300             '*** THE OCCURS WE ARE TO DISPLAY HAS BEEN DELETED   00056300
056400-            '                           '.                       00056400
056500         10  WS-ABCODE-1CLX             PIC X(04)  VALUE  '1CLX'. 00056500
056600         10  WS-ABCODE-1CLX-MSG         PIC X(79)  VALUE          00056600
056700             '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTE00056700
056800-            'MS ***                     '.                       00056800
056900         10  WS-ABCODE-1CP1             PIC X(04)  VALUE  '1CP1'. 00056900
057000         10  WS-ABCODE-1CP1-MSG         PIC X(79)  VALUE          00057000
057100             '????????????????????????????????????????????????????00057100
057200-            '???????????????????????????'.                       00057200
057300                                                                  00057300
057400/    M E S S A G E   T A B L E                                    00057400
057500******************************************************************00057500
057600 01  WT-01-TABLE.                                                 00057600
057700     05  FILLER                  PIC X(16) VALUE                  00057700
057800         '* WT-01-TABLE  *'.                                      00057800
057900                                                                  00057900
058000 01  FILLER.                                                      00058000
058100     05  WT-01-MESSAGE-VALUES.                                    00058100
058200*----------------------------------------------------------------*00058200
058300         10  WT-01-ENTRY-001.                                     00058300
058400             15  FILLER              PIC X(2)  VALUE '¬>'.        00058400
058500             15  WT-01-MESSAGE-TEXT-001.                          00058500
058600                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00058600
058700                 20  FILLER          PIC X(1)  VALUE  '-'.        00058700
058800                 20  FILLER          PIC X(3)  VALUE  '001'.      00058800
058900                 20  FILLER          PIC X(1)  VALUE  ' '.        00058900
059000                 20  FILLER          PIC X(70) VALUE              00059000
059100                     '#IBGR HAS BEEN SUCCESSFULLY MAPPED          00059100
059200-                    '                         '.                 00059200
059300             15  FILLER              PIC X(2)  VALUE '<¬'.        00059300
059400*----------------------------------------------------------------*00059400
059500         10  WT-01-ENTRY-002.                                     00059500
059600             15  FILLER              PIC X(2)  VALUE '¬>'.        00059600
059700             15  WT-01-MESSAGE-TEXT-002.                          00059700
059800                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00059800
059900                 20  FILLER          PIC X(1)  VALUE  '-'.        00059900
060000                 20  FILLER          PIC X(3)  VALUE  '002'.      00060000
060100                 20  FILLER          PIC X(1)  VALUE  ' '.        00060100
060200                 20  FILLER          PIC X(70) VALUE              00060200
060300                     '#IPGN HAS BEEN SUCCESSFULLY MAPPED          00060300
060400-                    '                         '.                 00060400
060500             15  FILLER              PIC X(2)  VALUE '<¬'.        00060500
060600*----------------------------------------------------------------*00060600
060700         10  WT-01-ENTRY-003.                                     00060700
060800             15  FILLER              PIC X(2)  VALUE '¬>'.        00060800
060900             15  WT-01-MESSAGE-TEXT-003.                          00060900
061000                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00061000
061100                 20  FILLER          PIC X(1)  VALUE  '-'.        00061100
061200                 20  FILLER          PIC X(3)  VALUE  '003'.      00061200
061300                 20  FILLER          PIC X(1)  VALUE  ' '.        00061300
061400                 20  FILLER          PIC X(70) VALUE              00061400
061500                     '#IPGT HAS BEEN SUCCESSFULLY MAPPED          00061500
061600-                    '                         '.                 00061600
061700             15  FILLER              PIC X(2)  VALUE '<¬'.        00061700
061800*----------------------------------------------------------------*00061800
061900         10  WT-01-ENTRY-004.                                     00061900
062000             15  FILLER              PIC X(2)  VALUE '¬>'.        00062000
062100             15  WT-01-MESSAGE-TEXT-004.                          00062100
062200                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00062200
062300                 20  FILLER          PIC X(1)  VALUE  '-'.        00062300
062400                 20  FILLER          PIC X(3)  VALUE  '004'.      00062400
062500                 20  FILLER          PIC X(1)  VALUE  ' '.        00062500
062600                 20  FILLER          PIC X(70) VALUE              00062600
062700                     '#IDGD HAS BEEN SUCCESSFULLY MAPPED          00062700
062800-                    '                         '.                 00062800
062900             15  FILLER              PIC X(2)  VALUE '<¬'.        00062900
063000*----------------------------------------------------------------*00063000
063100         10  WT-01-ENTRY-005.                                     00063100
063200             15  FILLER              PIC X(2)  VALUE '¬>'.        00063200
063300             15  WT-01-MESSAGE-TEXT-005.                          00063300
063400                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00063400
063500                 20  FILLER          PIC X(1)  VALUE  '-'.        00063500
063600                 20  FILLER          PIC X(3)  VALUE  '005'.      00063600
063700                 20  FILLER          PIC X(1)  VALUE  ' '.        00063700
063800                 20  FILLER          PIC X(70) VALUE              00063800
063900                     '#IPGP HAS BEEN SUCCESSFULLY MAPPED          00063900
064000-                    '                         '.                 00064000
064100             15  FILLER              PIC X(2)  VALUE '<¬'.        00064100
064200*----------------------------------------------------------------*00064200
064300         10  WT-01-ENTRY-006.                                     00064300
064400             15  FILLER              PIC X(2)  VALUE '¬>'.        00064400
064500             15  WT-01-MESSAGE-TEXT-006.                          00064500
064600                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00064600
064700                 20  FILLER          PIC X(1)  VALUE  '-'.        00064700
064800                 20  FILLER          PIC X(3)  VALUE  '006'.      00064800
064900                 20  FILLER          PIC X(1)  VALUE  ' '.        00064900
065000                 20  FILLER          PIC X(70) VALUE              00065000
065100                     'DELETE OPTION MUST BE \
065200-                    'VALID                    '.                 00065200
065300             15  FILLER              PIC X(2)  VALUE '<¬'.        00065300
065400*----------------------------------------------------------------*00065400
065500         10  WT-01-ENTRY-007.                                     00065500
065600             15  FILLER              PIC X(2)  VALUE '¬>'.        00065600
065700             15  WT-01-MESSAGE-TEXT-007.                          00065700
065800                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00065800
065900                 20  FILLER          PIC X(1)  VALUE  '-'.        00065900
066000                 20  FILLER          PIC X(3)  VALUE  '007'.      00066000
066100                 20  FILLER          PIC X(1)  VALUE  ' '.        00066100
066200                 20  FILLER          PIC X(70) VALUE              00066200
066300                     'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESS00066300
066400-                    ' PF4/PF16 TO CONTINUE    '.                 00066400
066500             15  FILLER              PIC X(2)  VALUE '<¬'.        00066500
066600*----------------------------------------------------------------*00066600
066700         10  WT-01-ENTRY-008.                                     00066700
066800             15  FILLER              PIC X(2)  VALUE '¬>'.        00066800
066900             15  WT-01-MESSAGE-TEXT-008.                          00066900
067000                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00067000
067100                 20  FILLER          PIC X(1)  VALUE  '-'.        00067100
067200                 20  FILLER          PIC X(3)  VALUE  '008'.      00067200
067300                 20  FILLER          PIC X(1)  VALUE  ' '.        00067300
067400                 20  FILLER          PIC X(70) VALUE              00067400
067500                     'GROUP IN CONVERSION STATUS, CANNOT CHANGE HI00067500
067600-                    'GH-LIGHTED ELEMENTS      '.                 00067600
067700             15  FILLER              PIC X(2)  VALUE '<¬'.        00067700
067800*----------------------------------------------------------------*00067800
067900         10  WT-01-ENTRY-009.                                     00067900
068000             15  FILLER              PIC X(2)  VALUE '¬>'.        00068000
068100             15  WT-01-MESSAGE-TEXT-009.                          00068100
068200                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00068200
068300                 20  FILLER          PIC X(1)  VALUE  '-'.        00068300
068400                 20  FILLER          PIC X(3)  VALUE  '009'.      00068400
068500                 20  FILLER          PIC X(1)  VALUE  ' '.        00068500
068600                 20  FILLER          PIC X(70) VALUE              00068600
068700                     'INVALID PFKEY SELECTION                     00068700
068800-                    '                         '.                 00068800
068900             15  FILLER              PIC X(2)  VALUE '<¬'.        00068900
069000*----------------------------------------------------------------*00069000
069100         10  WT-01-ENTRY-010.                                     00069100
069200             15  FILLER              PIC X(2)  VALUE '¬>'.        00069200
069300             15  WT-01-MESSAGE-TEXT-010.                          00069300
069400                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00069400
069500                 20  FILLER          PIC X(1)  VALUE  '-'.        00069500
069600                 20  FILLER          PIC X(3)  VALUE  '010'.      00069600
069700                 20  FILLER          PIC X(1)  VALUE  ' '.        00069700
069800                 20  FILLER          PIC X(70) VALUE              00069800
069900                     'INVALID REQUEST.  THAT PF KEY HAS NO MEANING00069900
070000-                    ' TO THIS PROGRAM         '.                 00070000
070100             15  FILLER              PIC X(2)  VALUE '<¬'.        00070100
070200*----------------------------------------------------------------*00070200
070300         10  WT-01-ENTRY-011.                                     00070300
070400             15  FILLER              PIC X(2)  VALUE '¬>'.        00070400
070500             15  WT-01-MESSAGE-TEXT-011.                          00070500
070600                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00070600
070700                 20  FILLER          PIC X(1)  VALUE  '-'.        00070700
070800                 20  FILLER          PIC X(3)  VALUE  '011'.      00070800
070900                 20  FILLER          PIC X(1)  VALUE  ' '.        00070900
071000                 20  FILLER          PIC X(70) VALUE              00071000
071100                     'NO CHANGE FOUND - NO CHANGE MADE, WHAT NEXT 00071100
071200-                    '                         '.                 00071200
071300             15  FILLER              PIC X(2)  VALUE '<¬'.        00071300
071400*----------------------------------------------------------------*00071400
071500         10  WT-01-ENTRY-012.                                     00071500
071600             15  FILLER              PIC X(2)  VALUE '¬>'.        00071600
071700             15  WT-01-MESSAGE-TEXT-012.                          00071700
071800                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00071800
071900                 20  FILLER          PIC X(1)  VALUE  '-'.        00071900
072000                 20  FILLER          PIC X(3)  VALUE  '012'.      00072000
072100                 20  FILLER          PIC X(1)  VALUE  ' '.        00072100
072200                 20  FILLER          PIC X(70) VALUE              00072200
072300                     'NO ENTRIES TO DISPLAY                       00072300
072400-                    '                         '.                 00072400
072500             15  FILLER              PIC X(2)  VALUE '<¬'.        00072500
072600*----------------------------------------------------------------*00072600
072700         10  WT-01-ENTRY-013.                                     00072700
072800             15  FILLER              PIC X(2)  VALUE '¬>'.        00072800
072900             15  WT-01-MESSAGE-TEXT-013.                          00072900
073000                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00073000
073100                 20  FILLER          PIC X(1)  VALUE  '-'.        00073100
073200                 20  FILLER          PIC X(3)  VALUE  '013'.      00073200
073300                 20  FILLER          PIC X(1)  VALUE  ' '.        00073300
073400                 20  FILLER          PIC X(70) VALUE              00073400
073500                     'PFKEY INVALID WHILE ERRORS NOT CORRECTED, HI00073500
073600-                    'T ENTER FOR ERR MSG      '.                 00073600
073700             15  FILLER              PIC X(2)  VALUE '<¬'.        00073700
073800*----------------------------------------------------------------*00073800
073900         10  WT-01-ENTRY-014.                                     00073900
074000             15  FILLER              PIC X(2)  VALUE '¬>'.        00074000
074100             15  WT-01-MESSAGE-TEXT-014.                          00074100
074200                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00074200
074300                 20  FILLER          PIC X(1)  VALUE  '-'.        00074300
074400                 20  FILLER          PIC X(3)  VALUE  '014'.      00074400
074500                 20  FILLER          PIC X(1)  VALUE  ' '.        00074500
074600                 20  FILLER          PIC X(70) VALUE              00074600
074700                     'PROCESSING FROM THE TOP OF THE LIST         00074700
074800-                    '                         '.                 00074800
074900             15  FILLER              PIC X(2)  VALUE '<¬'.        00074900
075000*----------------------------------------------------------------*00075000
075100         10  WT-01-ENTRY-015.                                     00075100
075200             15  FILLER              PIC X(2)  VALUE '¬>'.        00075200
075300             15  WT-01-MESSAGE-TEXT-015.                          00075300
075400                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00075400
075500                 20  FILLER          PIC X(1)  VALUE  '-'.        00075500
075600                 20  FILLER          PIC X(3)  VALUE  '015'.      00075600
075700                 20  FILLER          PIC X(1)  VALUE  ' '.        00075700
075800                 20  FILLER          PIC X(70) VALUE              00075800
075900                     'THE MAXIMUM NUMBER OF ENTRIES HAVE BEEN ADDE00075900
076000-                    'D                        '.                 00076000
076100             15  FILLER              PIC X(2)  VALUE '<¬'.        00076100
076200*----------------------------------------------------------------*00076200
076300         10  WT-01-ENTRY-016.                                     00076300
076400             15  FILLER              PIC X(2)  VALUE '¬>'.        00076400
076500             15  WT-01-MESSAGE-TEXT-016.                          00076500
076600                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00076600
076700                 20  FILLER          PIC X(1)  VALUE  '-'.        00076700
076800                 20  FILLER          PIC X(3)  VALUE  '016'.      00076800
076900                 20  FILLER          PIC X(1)  VALUE  ' '.        00076900
077000                 20  FILLER          PIC X(70) VALUE              00077000
077100                     'THE TABULAR ALREADY CONTAINS THE MAXIMUM NUM00077100
077200-                    'BER OF OCCURANCES        '.                 00077200
077300             15  FILLER              PIC X(2)  VALUE '<¬'.        00077300
077400*----------------------------------------------------------------*00077400
077500         10  WT-01-ENTRY-017.                                     00077500
077600             15  FILLER              PIC X(2)  VALUE '¬>'.        00077600
077700             15  WT-01-MESSAGE-TEXT-017.                          00077700
077800                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00077800
077900                 20  FILLER          PIC X(1)  VALUE  '-'.        00077900
078000                 20  FILLER          PIC X(3)  VALUE  '017'.      00078000
078100                 20  FILLER          PIC X(1)  VALUE  ' '.        00078100
078200                 20  FILLER          PIC X(70) VALUE              00078200
078300                     'THE TABULAR RECORD DOES NOT EXIST, AND CANNO00078300
078400-                    'T BE CHANGED             '.                 00078400
078500             15  FILLER              PIC X(2)  VALUE '<¬'.        00078500
078600*----------------------------------------------------------------*00078600
078700         10  WT-01-ENTRY-018.                                     00078700
078800             15  FILLER              PIC X(2)  VALUE '¬>'.        00078800
078900             15  WT-01-MESSAGE-TEXT-018.                          00078900
079000                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00079000
079100                 20  FILLER          PIC X(1)  VALUE  '-'.        00079100
079200                 20  FILLER          PIC X(3)  VALUE  '018'.      00079200
079300                 20  FILLER          PIC X(1)  VALUE  ' '.        00079300
079400                 20  FILLER          PIC X(70) VALUE              00079400
079500                     'THE TABULAR RECORD DOES NOT EXIST, AND CANNO00079500
079600-                    'T BE MAPPED              '.                 00079600
079700             15  FILLER              PIC X(2)  VALUE '<¬'.        00079700
079800*----------------------------------------------------------------*00079800
079900         10  WT-01-ENTRY-019.                                     00079900
080000             15  FILLER              PIC X(2)  VALUE '¬>'.        00080000
080100             15  WT-01-MESSAGE-TEXT-019.                          00080100
080200                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00080200
080300                 20  FILLER          PIC X(1)  VALUE  '-'.        00080300
080400                 20  FILLER          PIC X(3)  VALUE  '019'.      00080400
080500                 20  FILLER          PIC X(1)  VALUE  ' '.        00080500
080600                 20  FILLER          PIC X(70) VALUE              00080600
080700                     'THERE ARE NO MORE ENTRIES TO DISPLAY        00080700
080800-                    '                         '.                 00080800
080900             15  FILLER              PIC X(2)  VALUE '<¬'.        00080900
081000*----------------------------------------------------------------*00081000
081100         10  WT-01-ENTRY-020.                                     00081100
081200             15  FILLER              PIC X(2)  VALUE '¬>'.        00081200
081300             15  WT-01-MESSAGE-TEXT-020.                          00081300
081400                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00081400
081500                 20  FILLER          PIC X(1)  VALUE  '-'.        00081500
081600                 20  FILLER          PIC X(3)  VALUE  '020'.      00081600
081700                 20  FILLER          PIC X(1)  VALUE  ' '.        00081700
081800                 20  FILLER          PIC X(70) VALUE              00081800
081900                     'THIS IS THE FIRST ON THE TABLE              00081900
082000-                    '                         '.                 00082000
082100             15  FILLER              PIC X(2)  VALUE '<¬'.        00082100
082200*----------------------------------------------------------------*00082200
082300         10  WT-01-ENTRY-021.                                     00082300
082400             15  FILLER              PIC X(2)  VALUE '¬>'.        00082400
082500             15  WT-01-MESSAGE-TEXT-021.                          00082500
082600                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00082600
082700                 20  FILLER          PIC X(1)  VALUE  '-'.        00082700
082800                 20  FILLER          PIC X(3)  VALUE  '021'.      00082800
082900                 20  FILLER          PIC X(1)  VALUE  ' '.        00082900
083000                 20  FILLER          PIC X(70) VALUE              00083000
083100                     'THIS IS THE LAST ON THE TABLE               00083100
083200-                    '                         '.                 00083200
083300             15  FILLER              PIC X(2)  VALUE '<¬'.        00083300
083400*----------------------------------------------------------------*00083400
083500         10  WT-01-ENTRY-022.                                     00083500
083600             15  FILLER              PIC X(2)  VALUE '¬>'.        00083600
083700             15  WT-01-MESSAGE-TEXT-022.                          00083700
083800                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00083800
083900                 20  FILLER          PIC X(1)  VALUE  '-'.        00083900
084000                 20  FILLER          PIC X(3)  VALUE  '022'.      00084000
084100                 20  FILLER          PIC X(1)  VALUE  ' '.        00084100
084200                 20  FILLER          PIC X(70) VALUE              00084200
084300                     'THIS PFKEY NOT VALID WHILE IN CHG/ADD MODE  00084300
084400-                    '                         '.                 00084400
084500             15  FILLER              PIC X(2)  VALUE '<¬'.        00084500
084600*----------------------------------------------------------------*00084600
084700         10  WT-01-ENTRY-023.                                     00084700
084800             15  FILLER              PIC X(2)  VALUE '¬>'.        00084800
084900             15  WT-01-MESSAGE-TEXT-003.                          00084900
085000                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00085000
085100                 20  FILLER          PIC X(1)  VALUE  '-'.        00085100
085200                 20  FILLER          PIC X(3)  VALUE  '023'.      00085200
085300                 20  FILLER          PIC X(1)  VALUE  ' '.        00085300
085400                 20  FILLER          PIC X(70) VALUE              00085400
085500                     '#IPGS HAS BEEN SUCCESSFULLY MAPPED          00085500
085600-                    '                         '.                 00085600
085700             15  FILLER              PIC X(2)  VALUE '<¬'.        00085700
085800*----------------------------------------------------------------*00085800
085900         10  WT-01-ENTRY-024.                                     00085900
086000             15  FILLER              PIC X(2)  VALUE '¬>'.        00086000
086100             15  WT-01-MESSAGE-TEXT-023.                          00086100
086200                 20  FILLER          PIC X(4)  VALUE  'GA1C'.     00086200
086300                 20  FILLER          PIC X(1)  VALUE  '-'.        00086300
086400                 20  FILLER          PIC X(3)  VALUE  '024'.      00086400
086500                 20  FILLER          PIC X(1)  VALUE  ' '.        00086500
086600                 20  FILLER          PIC X(70) VALUE              00086600
086700                     '********** F U T U R E   U S E *************00086700
086800-                    '*************************'.                 00086800
086900             15  FILLER              PIC X(2)  VALUE '<¬'.        00086900
087000*----------------------------------------------------------------*00087000
087100                                                                  00087100
087200     05  WT-01-MESSAGE-TABLE         REDEFINES                    00087200
087300         WT-01-MESSAGE-VALUES         OCCURS 024 TIMES            00087300
087400                                     INDEXED BY WT-01-INDEX.      00087400
087500         10  WT-01-ENTRY.                                         00087500
087600             15  FILLER              PIC X(02).                   00087600
087700             15  WT-01-MESSAGE-TEXT  PIC X(79).                   00087700
087800             15  FILLER              PIC X(02).                   00087800
087900                                                                  00087900
088000 01  WS-END                      PIC X(16)  VALUE                 00088000
088100     '*** W/S ENDS ***'.                                          00088100
088200/    L I N K A G E   S E C T I O N                                00088200
088300 LINKAGE SECTION.                                                 00088300
088400 01  DFHCOMMAREA.                                                 00088400
088500 COPY G2ALCKEC.                                                   00088500
088600*    05  COMMAREA-RECORD-POINTER   USAGE IS POINTER.              00088600
088700 COPY GACDACWB.                                                   00088700
088800                                                                  00088800
088900     05  GAS2UPD-PASSED-AREA-2.                                   00088900
089000         07  LVL2-B-SW-2              PIC X.                      00089000
089100         07  LVL2-F-SW-2              PIC X.                      00089100
089200         07  LVL2-G-SW-2              PIC X.                      00089200
089300         07  INTR-TAB-PGM-ID-2        PIC X(8).                   00089300
089400         07  FILLER-2                 PIC X(9).                   00089400
089500     05  DELADD-OPTION-2              PIC X(7).                   00089500
089600                                                                  00089600
089700/*****************************************************************00089700
089800* W O R K F I L E   -   A L L   L E V E L   T A B   R E C O R D   00089800
089900******************************************************************00089900
090000 01  WF-IO-PARM-ALL-LVL-TAB-RECORD.                               00090000
090100 COPY GCIOPRM1.                                                   00090100
090200/                                                                 00090200
090300 COPY GCWRKDCC.                                                   00090300
090400/                                                                 00090400
090500 COPY GCTACLC.                                                    00090500
090600/    C O M M U N I C A T I O N    K E Y   A R E A                 00090600
090700*01  COMMUNICATION-KEY-AREA.                                      00090700
090800*COPY G2ALCKEC.                                                   00090800
090900                                                                  00090900
091000/    C O P Y   T A B U L A R   T A B L E   A R E A                00091000
091100 01  COPY-TABULAR-TABLE-AREA.                                     00091100
SI0724*    05  COPY-TABULAR-TABLE  OCCURS  44 TIMES INDEXED BY          00091200
SI0724     05  COPY-TABULAR-TABLE  OCCURS 175 TIMES INDEXED BY          00091210
091300         COPY-IDX, COPY-IDX2, COPY-IDX3, COPY-IDX4.               00091300
091400       10  COPY-SORTABLE-FLDS.                                    00091400
091500           15  FILLER                      PIC X(169).            00091500
091600           15  COPY-SORT-FYI               PIC X(003).            00091600
091700       10  COPY-SORT-ENTRY-CNTR            PIC S9(7) COMP-3.      00091700
091800                                                                  00091800
091900/*****************************************************************00091900
092000* W O R K F I L E   -   I N T E R N A L   T A B U L A R   R E C   00092000
092100******************************************************************00092100
092200 01  WF-IO-PARM-INTERNAL-TAB-RECORD.                              00092200
092300 COPY GCIOPRM2.                                                   00092300
092400/                                                                 00092400
092500 COPY GCWRKDC2.                                                   00092500
092600/                                                                 00092600
092700 COPY GCTIPGPC.                                                   00092700
092800/*****************************************************************00092800
092900* W O R K F I L E   -   G R O U P   S P E C I F I C   R E C       00092900
093000******************************************************************00093000
093100 01  WF-IO-PARM-WRK-GRP-SPEC-REC.                                 00093100
093200 COPY GCIOPRM3.                                                   00093200
093300/                                                                 00093300
093400 COPY GCWRKDC3.                                                   00093400
093500/                                                                 00093500
093600 COPY GCGROUPC.                                                   00093600
093700/*****************************************************************00093700
093800* W O R K F I L E   -   C O N T R A C T   R E C O R D             00093800
093900******************************************************************00093900
094000 01  WF-IO-PARM-WRK-CONTRACT-REC.                                 00094000
094100 COPY GCIOPRM4.                                                   00094100
094200/                                                                 00094200
094300 COPY GCWRKDC4.                                                   00094300
094400/                                                                 00094400
094500 COPY GCCONTRC.                                                   00094500
094600/*****************************************************************00094600
094700* W O R K F I L E   -   B E N E F I T   P R O V I S I O N   R E C 00094700
094800******************************************************************00094800
094900 01  WF-IO-PARM-WRK-BEN-PROV-REC.                                 00094900
095000 COPY GCIOPRM5.                                                   00095000
095100/                                                                 00095100
095200 COPY GCWRKDC5.                                                   00095200
095300/                                                                 00095300
095400 COPY GCBENPVC.                                                   00095400
095500/*****************************************************************00095500
095600* W O R K F I L E   -  C O N T R O L   R E C O R D                00095600
095700******************************************************************00095700
095800 01  WF-IO-PARM-WRK-CONTROL-REC.                                  00095800
095900 COPY GCIOPRM6.                                                   00095900
096000/                                                                 00096000
096100 COPY GCWRKDC6.                                                   00096100
096200/                                                                 00096200
096300 COPY GCCCRDCC.                                                   00096300
096400/*****************************************************************00096400
096500* P R O D U C T I O N   -   C O N T R A C T   R E C O R D         00096500
096600******************************************************************00096600
096700 01  PR-IO-PARM-WRK-CONTRACT-REC.                                 00096700
096800 COPY GCIOPRM7   SUPPRESS.                                        00096800
096900                                                                  00096900
097000 COPY GCWRKDC7   SUPPRESS.                                        00097000
097100                                                                  00097100
097200 COPY GCCONTR2   SUPPRESS.                                        00097200
097300******************************************************************00097300
097400* P R O D U C T I O N   -   G R O U P   S P E C I F I C   R E C   00097400
097500******************************************************************00097500
097600 01  PR-IO-PARM-WRK-GRP-SPEC-REC.                                 00097600
097700 COPY GCIOPRM8   SUPPRESS.                                        00097700
097800                                                                  00097800
097900 COPY GCWRKDC8   SUPPRESS.                                        00097900
098000                                                                  00098000
098100 COPY GCGROUP2   SUPPRESS.                                        00098100
098200******************************************************************00098200
098300* P R O D U C T I O N   -   B E N E F I T   P V S N   R E C O R D 00098300
098400******************************************************************00098400
098500 01  PR-IO-PARM-WRK-BEN-PROV-REC.                                 00098500
098600 COPY GCIOPRM9   SUPPRESS.                                        00098600
098700                                                                  00098700
098800 COPY GCWRKDC9   SUPPRESS.                                        00098800
098900                                                                  00098900
099000 COPY GCBENPV2   SUPPRESS.                                        00099000
099100******************************************************************00099100
099200* P R O D U C T I O N   -   A L L   L E V E L   T A B   R E C     00099200
099300******************************************************************00099300
099400 01  PR-IO-PARM-ALL-LVL-TAB-RECORD.                               00099400
099500 COPY GCIOPRMA   SUPPRESS.                                        00099500
099600                                                                  00099600
099700 COPY GCWRKDCA   SUPPRESS.                                        00099700
099800                                                                  00099800
099900 COPY GCTACL2    SUPPRESS.                                        00099900
100000                                                                  00100000
100100/*****************************************************************00100100
100200*    M A P S E T   A R E A                                        00100200
100300******************************************************************00100300
100400     COPY GA1XSETC.                                               00100400
100500                                                                  00100500
100600/*****************************************************************00100600
100700*     A L L   L V L   A C C U M   C O M M O N   W O R K A R E A S 00100700
100800******************************************************************00100800
100900 01  COMMON-WORKAREAS.                                            00100900
101000 COPY G2ALCKE2.                                                   00101000
101100 COPY GACDACWA.                                                   00101100
101200                                                                  00101200
101300     05  GAS2UPD-PASSED-AREA.                                     00101300
101400         07  LVL2-B-SW                PIC X.                      00101400
101500         07  LVL2-F-SW                PIC X.                      00101500
101600         07  LVL2-G-SW                PIC X.                      00101600
101700         07  INTR-TAB-PGM-ID          PIC X(8).                   00101700
101800         07  FILLER                   PIC X(09).                  00101800
101900     05  DELADD-OPTION                PIC X(7).                   00101900
102000                                                                  00102000
102100/    P R O C E D U R E   D I V I S I O N                          00102100
102200 PROCEDURE DIVISION.                                              00102200
102300                                                                  00102300
102400******************************************************************00102400
102500* 0000  HOUSEKEEPING                                             *00102500
102600******************************************************************00102600
102700 0000-000-HOUSEKEEPING          SECTION.                          00102700
102800 0000-010.                                                        00102800
102900                                                                  00102900
103000     EXEC CICS GETMAIN                                            00103000
103100               SET(ADDRESS OF COMMON-WORKAREAS)                   00103100
103200               INITIMG(WS-HEX-00)                                 00103200
103300               LENGTH(LENGTH OF COMMON-WORKAREAS)                 00103300
103400               END-EXEC.                                          00103400
103500                                                                  00103500
103600     MOVE ZEROES  TO  ACWA-CDE-1U-COUNT,  ACWA-CDE-2B-COUNT.      00103600
103700                                                                  00103700
103800     EXEC CICS GETMAIN                                            00103800
103900               SET(ADDRESS OF GA1XI01I)                           00103900
104000               INITIMG(WS-HEX-00)                                 00104000
104100               LENGTH(LENGTH OF GA1XI01I)                         00104100
104200               END-EXEC.                                          00104200
104300                                                                  00104300
104400     SET ACWA-MAPSET-PNTR  TO  ADDRESS OF  GA1XI01I.              00104400
104500                                                                  00104500
104600     MOVE ZEROES  TO  ACWA-CDE-1U-COUNT,  ACWA-CDE-2B-COUNT.      00104600
104700                                                                  00104700
104800                                                                  00104800
104900     MOVE  +19   TO  GCVI-COMMAREA-LEN.                           00104900
105000     MOVE  'N'   TO  ACWA-ERROR-SW                                00105000
105100                     ACWA-CDE-FIELD-CHANGE-IND                    00105100
105200                     ACWA-CDE-REC-CHANGE-IND                      00105200
105300                     ACWA-CDE-RESET-WF-IND.                       00105300
105400     MOVE  ZERO  TO  ACWA-FIELD-CHG-CNT.                          00105400
105500     MOVE  SPACE TO  ACWA-CDE-STATUS-CHANGE-IND                   00105500
105600                     ACWA-CDE-INTERNAL-TAB-IND.                   00105600
105700                                                                  00105700
105800     COMPUTE WS-IO-PARM-WRK-GRP-SPEC-LEN  =  GC-GCIOPARM-LEN  +   00105800
105900             GC-WORKFILE-KEY-LEN  +  GC-GCGRPSPC-FIXED-LEN  +     00105900
106000            (GC-GCGRPSPC-VARY-LEN  *  GC-GCGRPSPC-VARY-MAX-OCUR). 00106000
106100                                                                  00106100
106200     COMPUTE WS-WRK-GRP-SPEC-LEN         =                        00106200
106300             GC-WORKFILE-KEY-LEN  +  GC-GCGRPSPC-FIXED-LEN  +     00106300
106400            (GC-GCGRPSPC-VARY-LEN  *  GC-GCGRPSPC-VARY-MAX-OCUR). 00106400
106500                                                                  00106500
106600     COMPUTE WS-IO-PARM-WRK-CONTRACT-LEN  =  GC-GCIOPARM-LEN   +  00106600
106700             GC-WORKFILE-KEY-LEN  +  GC-GCCONTR-FIXED-LEN  +      00106700
106800            (GC-GCCONTR-VARY-LEN  *  GC-GCCONTR-VARY-MAX-OCUR).   00106800
106900                                                                  00106900
107000     COMPUTE WS-WRK-CONTRACT-LEN  =                               00107000
107100             GC-WORKFILE-KEY-LEN   +  GC-GCCONTR-FIXED-LEN  +     00107100
107200            (GC-GCCONTR-VARY-LEN  *  GC-GCCONTR-VARY-MAX-OCUR).   00107200
107300                                                                  00107300
107400     COMPUTE WS-IO-PARM-WRK-BEN-PROV-LEN  =  GC-GCIOPARM-LEN  +   00107400
107500             GC-WORKFILE-KEY-LEN  +  GC-GCBENPRV-FIXED-LEN  +     00107500
107600            (GC-GCBENPRV-VARY-LEN  *  GC-GCBENPRV-VARY-MAX-OCUR). 00107600
107700                                                                  00107700
107800     COMPUTE WS-WRK-BEN-PROV-LEN  =                               00107800
107900             GC-WORKFILE-KEY-LEN  +  GC-GCBENPRV-FIXED-LEN  +     00107900
108000            (GC-GCBENPRV-VARY-LEN  *  GC-GCBENPRV-VARY-MAX-OCUR). 00108000
108100                                                                  00108100
108200     COMPUTE WS-IO-PARM-WRK-CONTROL-LEN  =  GC-GCIOPARM-LEN  +    00108200
108300             GC-WORKFILE-KEY-LEN  +  GC-WORKFILE-CONTROL-REC-LEN. 00108300
108400                                                                  00108400
108500     IF EIBAID  =  DFHCLEAR                                       00108500
108600         EXEC CICS SEND FROM(WS-ONE-LOW)                          00108600
108700                        ERASE                                     00108700
108800         END-EXEC                                                 00108800
108900         EXEC CICS RETURN                                         00108900
109000         END-EXEC.                                                00109000
109100                                                                  00109100
109200     EXEC CICS  HANDLE  CONDITION                                 00109200
109300                MAPFAIL(6400-000-XCTL-TO-MAIN-MENU)  END-EXEC.    00109300
109400                                                                  00109400
109500 0000-900-EXIT.                                                   00109500
109600         EXIT.                                                    00109600
109700/*****************************************************************00109700
109800* 1000  MAIN LINE                                                *00109800
109900******************************************************************00109900
110000 1000-000-MAIN-LINE             SECTION.                          00110000
110100 1000-010.                                                        00110100
110200                                                                  00110200
110300     IF  EIBTRNID  NOT =  'GA1C'                                  00110300
110400         PERFORM 4000-000-DISPLAY-FIRST-SCREEN.                   00110400
110500                                                                  00110500
110600     EXEC CICS  RECEIVE   MAP('GA1XI01')  MAPSET('GA1XSET')       00110600
110700                END-EXEC.                                         00110700
110800                                                                  00110800
110900     IF  SCRNIDNI  NOT =  '001C00'                                00110900
111000         PERFORM 6400-000-XCTL-TO-MAIN-MENU.                      00111000
111100                                                                  00111100
111200     IF  FRMNUIDI  =  'GS3A'                                      00111200
111300         MOVE IDLINEI   TO   GROUP-SPECIFIC-ID-LINE.              00111300
111400     IF  FRMNUIDI  =  'GC4A' OR  'GTM1'                           00111400
111500         MOVE IDLINEI   TO   CONTRACT-ID-LINE.                    00111500
111600     IF  FRMNUIDI  =  'GC8A'                                      00111600
111700         MOVE IDLINEI   TO   BENEFIT-PROVISION-ID-LINE.           00111700
111800                                                                  00111800
111900     IF EIBAID  =  DFHPF1 OR  DFHPF13                             00111900
112000        PERFORM 8000-000-SWITCH-ADD-DEL-MODE.                     00112000
112100                                                                  00112100
112200     IF EIBAID  =  DFHPF3 OR  DFHPF15                             00112200
112300        PERFORM 5000-000-XCTL-TO-PREVIOUS-MENU.                   00112300
112400                                                                  00112400
112500     IF (EIBAID  =  DFHPF7 OR  DFHPF19 OR  DFHPF8 OR  DFHPF20) AND00112500
112600        DELADDI  =  'CHG/ADD'                                     00112600
112700     THEN                                                         00112700
112800         SET  WT-01-INDEX                     TO +22              00112800
112900         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00112900
113000         MOVE -1                              TO  PERIODL         00113000
113100         GO TO 1000-900-EXIT.                                     00113100
113200                                                                  00113200
113300                                                                  00113300
113400     IF (EIBAID  =  DFHPF7 OR  DFHPF19 OR  DFHPF8 OR  DFHPF20) AND00113400
113500        DELOPTNI  =  'D'                                          00113500
113600     THEN                                                         00113600
113700         SET  WT-01-INDEX                     TO +06              00113700
113800         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00113800
113900         MOVE -1                              TO  DELOPTNL        00113900
114000         GO TO 1000-900-EXIT.                                     00114000
114100                                                                  00114100
114200     PERFORM 1100-000-VALIDATE-SCREEN.                            00114200
114300                                                                  00114300
114400     IF  EIBAID  = DFHPF4 OR DFHPF16  AND                         00114400
114500         ACWA-SCREEN-HAS-ERRORS                                   00114500
114600     THEN                                                         00114600
114700         SET  WT-01-INDEX                     TO +13              00114700
114800         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00114800
114900         GO TO 1000-900-EXIT.                                     00114900
115000                                                                  00115000
115100     IF  EIBAID  = DFHPF4 OR DFHPF16                              00115100
115200     THEN                                                         00115200
115300         IF  ACWA-SCREEN-HAS-NO-ERRORS                            00115300
115400         THEN                                                     00115400
115500             IF  GCVI-TABLE-SW = 'N'                              00115500
115600             THEN                                                 00115600
115700                 PERFORM 2000-000-PROCESS-REQUEST                 00115700
115800                 GO TO  1000-990-RETURN                           00115800
115900             ELSE                                                 00115900
116000                 SET  WT-01-INDEX                     TO +09      00116000
116100                 MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO  00116100
116200                 MOVE -1                              TO PERIODL  00116200
116300                 GO TO 1000-900-EXIT                              00116300
116400         ELSE                                                     00116400
116500             NEXT SENTENCE                                        00116500
116600     ELSE                                                         00116600
116700         NEXT SENTENCE.                                           00116700
116800                                                                  00116800
116900     IF  ACWA-SCREEN-HAS-ERRORS                                   00116900
117000         GO TO 1000-900-EXIT.                                     00117000
117100                                                                  00117100
117200     IF  EIBAID  =  DFHENTER OR                                   00117200
117300                    DFHPF7   OR  DFHPF19 OR   DFHPF8 OR  DFHPF20  00117300
117400     THEN                                                         00117400
117500         PERFORM 2000-000-PROCESS-REQUEST                         00117500
117600                 GO TO  1000-990-RETURN.                          00117600
117700                                                                  00117700
117800     PERFORM 7900-000-RESET-ATTRIBUTES.                           00117800
117900     MOVE -1                              TO PERIODL.             00117900
118000     SET  WT-01-INDEX                     TO +10                  00118000
118100     MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO.             00118100
118200                                                                  00118200
118300                                                                  00118300
118400 1000-900-EXIT.                                                   00118400
118500                                                                  00118500
118600     PERFORM 3100-000-READ-RECORD.                                00118600
118700     MOVE GAB-ENTRY-COUNT  TO  GAB-ENTRY-COUNT.                   00118700
118800     PERFORM 9010-000-SEND-DATAONLY-RETURN.                       00118800
118900 1000-990-RETURN.                                                 00118900
119000*    EXEC CICS  RETURN  END-EXEC.                                 00119000
119100     IF DELADD-OPTION = 'GAS2UPD'                                 00119100
119200         IF INTR-TAB-PGM-ID = 'GA1GPGM'                           00119200
119300             MOVE SPACES TO DELADD-OPTION                         00119300
119400             EXEC CICS RETURN TRANSID('GA1G')                     00119400
119500                       COMMAREA(COMMON-WORKAREAS)                 00119500
119600                       END-EXEC                                   00119600
119700         ELSE                                                     00119700
119800         IF INTR-TAB-PGM-ID = 'GA2GPGM'                           00119800
119900             MOVE SPACES TO DELADD-OPTION                         00119900
120000             EXEC CICS RETURN TRANSID('GA2G')                     00120000
120100                       COMMAREA(COMMON-WORKAREAS)                 00120100
120200                       END-EXEC                                   00120200
120300         ELSE                                                     00120300
120400         IF INTR-TAB-PGM-ID = 'GA1HPGM'                           00120400
120500             MOVE SPACES TO DELADD-OPTION                         00120500
120600             EXEC CICS  RETURN TRANSID('GA1H')                    00120600
120700                        COMMAREA(COMMON-WORKAREAS)                00120700
120800                        END-EXEC                                  00120800
120900         ELSE                                                     00120900
121000         IF INTR-TAB-PGM-ID = 'GA2HPGM'                           00121000
121100             MOVE SPACES TO DELADD-OPTION                         00121100
121200             EXEC CICS  RETURN TRANSID('GA2H')                    00121200
121300                        COMMAREA(COMMON-WORKAREAS)                00121300
121400                        END-EXEC                                  00121400
121500         ELSE                                                     00121500
121600         IF INTR-TAB-PGM-ID = 'GA1SPGM'                           00121600
121700             MOVE SPACES TO DELADD-OPTION                         00121700
121800             EXEC CICS  RETURN TRANSID('GA1S')                    00121800
121900                        COMMAREA(COMMON-WORKAREAS)                00121900
122000                        END-EXEC                                  00122000
122100         ELSE                                                     00122100
122200         IF INTR-TAB-PGM-ID = 'GA1IPGM'                           00122200
122300             MOVE SPACES TO DELADD-OPTION                         00122300
122400             EXEC CICS  RETURN TRANSID('GA1I')                    00122400
122500                        COMMAREA(COMMON-WORKAREAS)                00122500
122600                        END-EXEC                                  00122600
122700         ELSE                                                     00122700
122800         IF INTR-TAB-PGM-ID = 'GA2SPGM'                           00122800
122900             MOVE SPACES TO DELADD-OPTION                         00122900
123000             EXEC CICS  RETURN TRANSID('GA2S')                    00123000
123100                        COMMAREA(COMMON-WORKAREAS)                00123100
123200                        END-EXEC                                  00123200
123300         ELSE                                                     00123300
123400         IF INTR-TAB-PGM-ID = 'GA2IPGM'                           00123400
123500             MOVE SPACES TO DELADD-OPTION                         00123500
123600             EXEC CICS  RETURN TRANSID('GA2I')                    00123600
123700                        COMMAREA(COMMON-WORKAREAS)                00123700
123800                        END-EXEC                                  00123800
123900         ELSE                                                     00123900
124000         IF INTR-TAB-PGM-ID = 'GA1NPGM'                           00124000
124100             MOVE SPACES TO DELADD-OPTION                         00124100
124200             EXEC CICS  RETURN TRANSID('GA1N')                    00124200
124300                        COMMAREA(COMMON-WORKAREAS)                00124300
124400                        END-EXEC                                  00124400
124500         ELSE                                                     00124500
124600         IF INTR-TAB-PGM-ID = 'GA2NPGM'                           00124600
124700             MOVE SPACES TO DELADD-OPTION                         00124700
124800             EXEC CICS  RETURN TRANSID('GA2N')                    00124800
124900                        COMMAREA(COMMON-WORKAREAS)                00124900
125000                        END-EXEC                                  00125000
125100         ELSE                                                     00125100
125200         IF INTR-TAB-PGM-ID = 'GA1OPGM'                           00125200
125300             MOVE SPACES TO DELADD-OPTION                         00125300
125400             EXEC CICS  RETURN TRANSID('GA1O')                    00125400
125500                        COMMAREA(COMMON-WORKAREAS)                00125500
125600                        END-EXEC                                  00125600
125700         ELSE                                                     00125700
125800         IF INTR-TAB-PGM-ID = 'GA2OPGM'                           00125800
125900             MOVE SPACES TO DELADD-OPTION                         00125900
126000             EXEC CICS  RETURN TRANSID('GA2O')                    00126000
126100                        COMMAREA(COMMON-WORKAREAS)                00126100
126200                        END-EXEC                                  00126200
126300         ELSE                                                     00126300
126400         EXEC CICS  RETURN TRANSID('GA1C')                        00126400
126500                    COMMAREA(DFHCOMMAREA)                         00126500
126600                    LENGTH  (EIBCALEN)                            00126600
126700                    END-EXEC                                      00126700
126800     ELSE                                                         00126800
126900     EXEC CICS  RETURN TRANSID('GA1C')                            00126900
127000                COMMAREA(DFHCOMMAREA)                             00127000
127100                LENGTH  (EIBCALEN)                                00127100
127200                END-EXEC.                                         00127200
127300                                                                  00127300
127400     GOBACK.                                                      00127400
127500 1000-999-EXIT.                                                   00127500
127600        EXIT.                                                     00127600
127700/*****************************************************************00127700
127800* 1100  VALIDATE SCREEN                                          *00127800
127900*                                                                *00127900
128000*    THIS IS PRIMARILY A VALIDATION ROUTINE OF DATA BEING ENTERED*00128000
128100*  BY THE OPERATOR, PLUS THE ADDITION OF SOME REINITIALIZATION.  *00128100
128200*  1. REINITIALIZE ATTRIBUTES THAT THE PROGRAM MIGHT MODIFY, AND *00128200
128300*     RESET THE ERROR MESSAGE AND DELETE OPTION TO BLANKS.       *00128300
128400*  2. INSURE THE VALIDITY OF THE OPTIONS THAT CAN BE USED FOR THE*00128400
128500*     INTERNAL TABULAR.                                          *00128500
128600******************************************************************00128600
128700 1100-000-VALIDATE-SCREEN       SECTION.                          00128700
128800 1100-010.                                                        00128800
128900                                                                  00128900
129000     SET ACWA-WF-ALL-LEVEL-TAB-PNTR TO                            00129000
129100         ADDRESS OF  WF-IO-PARM-ALL-LVL-TAB-RECORD.               00129100
129200                                                                  00129200
129300     SET ACWA-COPY-TAB-PNTR         TO                            00129300
129400         ADDRESS OF  COPY-TABULAR-TABLE-AREA.                     00129400
129500                                                                  00129500
129600     SET ACWA-WF-INTERNAL-TAB-PNTR  TO                            00129600
129700         ADDRESS OF  WF-IO-PARM-INTERNAL-TAB-RECORD.              00129700
129800                                                                  00129800
129900     SET ACWA-WF-GRP-SPEC-PNTR      TO                            00129900
130000         ADDRESS OF  WF-IO-PARM-WRK-GRP-SPEC-REC.                 00130000
130100                                                                  00130100
130200     SET ACWA-WF-CONTRACT-PNTR      TO                            00130200
130300         ADDRESS OF  WF-IO-PARM-WRK-CONTRACT-REC.                 00130300
130400                                                                  00130400
130500     SET ACWA-WF-BEN-PROV-PNTR      TO                            00130500
130600         ADDRESS OF  WF-IO-PARM-WRK-BEN-PROV-REC.                 00130600
130700                                                                  00130700
130800     SET ACWA-WF-CONTROL-RECORD-PNTR    TO                        00130800
130900         ADDRESS OF  WF-IO-PARM-WRK-CONTROL-REC.                  00130900
131000                                                                  00131000
131100     SET ACWA-PR-CONTRACT-PNTR      TO                            00131100
131200         ADDRESS OF  PR-IO-PARM-WRK-CONTRACT-REC.                 00131200
131300                                                                  00131300
131400     SET ACWA-PR-GRP-SPEC-PNTR      TO                            00131400
131500         ADDRESS OF  PR-IO-PARM-WRK-GRP-SPEC-REC.                 00131500
131600                                                                  00131600
131700     SET ACWA-PR-BEN-PROV-PNTR      TO                            00131700
131800         ADDRESS OF  PR-IO-PARM-WRK-BEN-PROV-REC.                 00131800
131900                                                                  00131900
132000     SET ACWA-PR-ALL-LEVEL-TAB-PNTR   TO                          00132000
132100         ADDRESS OF  PR-IO-PARM-ALL-LVL-TAB-RECORD.               00132100
132200                                                                  00132200
132300     MOVE 'N'              TO ACWA-ERROR-SW.                      00132300
132400     MOVE 'Y'              TO GCVI-TABLE-SW.                      00132400
132500     MOVE SPACES           TO ERRMSGO.                            00132500
132600     MOVE DFHBMFSE         TO PERIODA.                            00132600
132700     MOVE DFHBMASF         TO IBGRIDA    IPGNIDA   IPGTIDA        00132700
132800                              IDGDIDA    IPGPIDA   IPGSIDA        00132800
132900                              IBGRSLTA   IPGNSLTA  IPGTSLTA       00132900
133000                              IDGDSLTA   IPGPSLTA  IPGSSLTA.      00133000
133100                                                                  00133100
133200     PERFORM 7900-000-RESET-ATTRIBUTES.                           00133200
133300                                                                  00133300
133400*------------- LINK TO SCREEN EDIT MODULE -----------------------*00133400
133500                                                                  00133500
133600     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00133600
133700                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00133700
133800     EXEC CICS  LINK  PROGRAM('GASEDIT1')                         00133800
133900                COMMAREA (COMMON-WORKAREAS)                       00133900
134000                LENGTH(LENGTH OF COMMON-WORKAREAS)   END-EXEC.    00134000
134100                                                                  00134100
134200     IF  ACWA-SCREEN-HAS-ERRORS                                   00134200
134300         GO TO 1100-900-EXIT.                                     00134300
134400                                                                  00134400
134500     IF  ACWA-FIELD-CHG-CNT > ZEROS                               00134500
134600         GO TO 1100-900-EXIT.                                     00134600
134700                                                                  00134700
134800     IF  (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE) AND           00134800
134900         (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE) AND           00134900
135000         (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE) AND           00135000
135100         (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE) AND           00135100
135200         (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE) AND           00135200
135300         (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE)               00135300
135400     THEN                                                         00135400
135500         GO TO 1100-900-EXIT.                                     00135500
135600                                                                  00135600
135700                                                                  00135700
135800     COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   00135800
135900              GC-GCIOPARM-LEN                 +                   00135900
136000              GC-WORKFILE-KEY-LEN             +                   00136000
136100              GC-GCTABULR-IPGP-FIXED-LEN      +                   00136100
136200             (GC-GCTABULR-IPGP-VARY-LEN       *                   00136200
136300              GC-GCTABULR-IPGP-VARY-MAX-OCUR)                     00136300
136400                                                                  00136400
136500        EXEC CICS GETMAIN                                         00136500
136600               SET(ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD)     00136600
136700               INITIMG(WS-HEX-00)                                 00136700
136800               LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)             00136800
136900               END-EXEC                                           00136900
137000                                                                  00137000
137100     SET ACWA-WF-INTERNAL-TAB-PNTR  TO                            00137100
137200         ADDRESS OF  WF-IO-PARM-INTERNAL-TAB-RECORD.              00137200
137300                                                                  00137300
137400                                                                  00137400
137500     IF  IBGROPTI  =  'C'                                         00137500
137600     THEN                                                         00137600
137700         IF  IBGRSLTI  >  '8999999'                               00137700
137800         THEN                                                     00137800
137900             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00137900
138000             GO TO 1100-100-GET-INTERNAL-TAB                      00138000
138100         ELSE                                                     00138100
138200             ADD 100        TO  ACWA-FIELD-CHG-CNT                00138200
138300             MOVE '#IBGR '  TO  GCIO-TAB-TABULAR-ID               00138300
138400             MOVE IBGRSLTI  TO  GCIO-TAB-SLOT-NO                  00138400
138500     ELSE                                                         00138500
138600         NEXT SENTENCE.                                           00138600
138700                                                                  00138700
138800     IF  IDGDOPTI  =  'C'                                         00138800
138900     THEN                                                         00138900
139000         IF  IDGDSLTI  >  '8999999'                               00139000
139100         THEN                                                     00139100
139200             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00139200
139300             GO TO 1100-100-GET-INTERNAL-TAB                      00139300
139400         ELSE                                                     00139400
139500             ADD 100        TO  ACWA-FIELD-CHG-CNT                00139500
139600             MOVE '#IDGD '  TO  GCIO-TAB-TABULAR-ID               00139600
139700             MOVE IDGDSLTI  TO  GCIO-TAB-SLOT-NO                  00139700
139800     ELSE                                                         00139800
139900         NEXT SENTENCE.                                           00139900
140000                                                                  00140000
140100                                                                  00140100
140200     IF  IPGNOPTI  =  'C'                                         00140200
140300     THEN                                                         00140300
140400         IF  IPGNSLTI  > '8999999'                                00140400
140500         THEN                                                     00140500
140600             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00140600
140700             GO TO 1100-100-GET-INTERNAL-TAB                      00140700
140800         ELSE                                                     00140800
140900           ADD 100        TO  ACWA-FIELD-CHG-CNT                  00140900
141000           MOVE '#IPGN '  TO  GCIO-TAB-TABULAR-ID                 00141000
141100           MOVE IPGNSLTI  TO  GCIO-TAB-SLOT-NO                    00141100
141200     ELSE                                                         00141200
141300         NEXT SENTENCE.                                           00141300
141400                                                                  00141400
141500     IF  IPGPOPTI  =  'C'                                         00141500
141600     THEN                                                         00141600
141700         IF  IPGPSLTI  > '8999999'                                00141700
141800         THEN                                                     00141800
141900             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00141900
142000             GO TO 1100-100-GET-INTERNAL-TAB                      00142000
142100         ELSE                                                     00142100
142200           ADD 100        TO  ACWA-FIELD-CHG-CNT                  00142200
142300           MOVE '#IPGP '  TO  GCIO-TAB-TABULAR-ID                 00142300
142400           MOVE IPGPSLTI  TO  GCIO-TAB-SLOT-NO                    00142400
142500     ELSE                                                         00142500
142600         NEXT SENTENCE.                                           00142600
142700                                                                  00142700
142800                                                                  00142800
142900     IF  IPGTOPTI  =  'C'                                         00142900
143000     THEN                                                         00143000
143100         IF  IPGTSLTI  >  '8999999'                               00143100
143200         THEN                                                     00143200
143300             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00143300
143400             GO TO 1100-100-GET-INTERNAL-TAB                      00143400
143500         ELSE                                                     00143500
143600             ADD 100        TO  ACWA-FIELD-CHG-CNT                00143600
143700             MOVE '#IPGT '  TO  GCIO-TAB-TABULAR-ID               00143700
143800             MOVE IPGTSLTI  TO  GCIO-TAB-SLOT-NO                  00143800
143900     ELSE                                                         00143900
144000         NEXT SENTENCE.                                           00144000
144100                                                                  00144100
144200                                                                  00144200
144300     IF  IPGSOPTI  =  'C'                                         00144300
144400     THEN                                                         00144400
144500         IF  IPGSSLTI  >  '8999999'                               00144500
144600         THEN                                                     00144600
144700             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00144700
144800             GO TO 1100-100-GET-INTERNAL-TAB                      00144800
144900         ELSE                                                     00144900
145000             ADD 100        TO  ACWA-FIELD-CHG-CNT                00145000
145100             MOVE '#IPGS '  TO  GCIO-TAB-TABULAR-ID               00145100
145200             MOVE IPGSSLTI  TO  GCIO-TAB-SLOT-NO                  00145200
145300     ELSE                                                         00145300
145400         NEXT SENTENCE.                                           00145400
145500                                                                  00145500
145600                                                                  00145600
145700     IF IBGROPTI  =  'MT'                                         00145700
145800        MOVE '#IBGR '  TO  GCIO-TAB-TABULAR-ID                    00145800
145900        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00145900
146000                                                                  00146000
146100     IF IBGROPTI  =  'A'                                          00146100
146200        MOVE '#IBGR '  TO  GCIO-TAB-TABULAR-ID                    00146200
146300        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00146300
146400                                                                  00146400
146500     IF IDGDOPTI  =  'MT'                                         00146500
146600        MOVE '#IDGD '  TO  GCIO-TAB-TABULAR-ID                    00146600
146700        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00146700
146800                                                                  00146800
146900     IF IDGDOPTI  =  'A'                                          00146900
147000        MOVE '#IDGD '  TO  GCIO-TAB-TABULAR-ID                    00147000
147100        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00147100
147200                                                                  00147200
147300     IF IPGNOPTI  =  'MT'                                         00147300
147400        MOVE '#IPGN '  TO  GCIO-TAB-TABULAR-ID                    00147400
147500        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00147500
147600                                                                  00147600
147700     IF IPGNOPTI  =  'A'                                          00147700
147800        MOVE '#IPGN '  TO  GCIO-TAB-TABULAR-ID                    00147800
147900        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00147900
148000                                                                  00148000
148100     IF IPGPOPTI  =  'MT'                                         00148100
148200        MOVE '#IPGP '  TO  GCIO-TAB-TABULAR-ID                    00148200
148300        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00148300
148400                                                                  00148400
148500     IF IPGPOPTI  =  'A'                                          00148500
148600        MOVE '#IPGP '  TO  GCIO-TAB-TABULAR-ID                    00148600
148700        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00148700
148800                                                                  00148800
148900     IF IPGTOPTI  =  'MT'                                         00148900
149000        MOVE '#IPGT '  TO  GCIO-TAB-TABULAR-ID                    00149000
149100        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00149100
149200                                                                  00149200
149300     IF IPGTOPTI  =  'A'                                          00149300
149400        MOVE '#IPGT '  TO  GCIO-TAB-TABULAR-ID                    00149400
149500        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00149500
149600                                                                  00149600
149700     IF IPGSOPTI  =  'MT'                                         00149700
149800        MOVE '#IPGS '  TO  GCIO-TAB-TABULAR-ID                    00149800
149900        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00149900
150000                                                                  00150000
150100     IF IPGSOPTI  =  'A'                                          00150100
150200        MOVE '#IPGS '  TO  GCIO-TAB-TABULAR-ID                    00150200
150300        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00150300
150400                                                                  00150400
150500     MOVE GCIO-TABULAR-FILE      TO GCIO2-FILE-KEY.               00150500
150600     MOVE GC-GCTABULR-DDNAME     TO GCIO2-FILE-DDNAME.            00150600
150700     MOVE GC-GCIO-AREA-2         TO GCIO2-IO-AREA-TO-USE.         00150700
150800     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO2-FILE-ACCESS-CODE.       00150800
150900     MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          00150900
151000          TO  GXA-ENTRY-COUNT.                                    00151000
151100                                                                  00151100
151200     EXEC CICS  LINK  PROGRAM('GCIOPGM')                          00151200
151300                COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          00151300
151400                LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)   END-EXEC.00151400
151500                                                                  00151500
151600     MOVE GXA-ENTRY-COUNT  TO  GXA-ENTRY-COUNT.                   00151600
151700                                                                  00151700
151800     IF  NOT GCIO2-GOOD-RETURN AND  ACWA-PROD-INTERNAL-CHG        00151800
151900     THEN                                                         00151900
152000         SET  WT-01-INDEX                     TO +17              00152000
152100         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00152100
152200         MOVE 'Y'                             TO ACWA-ERROR-SW    00152200
152300         IF  IBGROPTI  =  'C'                                     00152300
152400         THEN                                                     00152400
152500             MOVE -1        TO  IBGROPTL                          00152500
152600             MOVE DFHBMUBF  TO  IBGROPTA                          00152600
152700             MOVE DFHBMASB  TO  IBGRIDA  IBGRSLTA                 00152700
152800             GO TO 1100-900-EXIT                                  00152800
152900         ELSE                                                     00152900
153000        IF  IPGNOPTI  =  'C'                                      00153000
153100        THEN                                                      00153100
153200            MOVE -1        TO  IPGNOPTL                           00153200
153300            MOVE DFHBMUBF  TO  IPGNOPTA                           00153300
153400            MOVE DFHBMASB  TO  IPGNIDA  IPGNSLTA                  00153400
153500            GO TO 1100-900-EXIT                                   00153500
153600        ELSE                                                      00153600
153700        IF  IPGTOPTI  =  'C'                                      00153700
153800        THEN                                                      00153800
153900            MOVE -1        TO  IPGTOPTL                           00153900
154000            MOVE DFHBMUBF  TO  IPGTOPTA                           00154000
154100            MOVE DFHBMASB  TO  IPGTIDA  IPGTSLTA                  00154100
154200            GO TO 1100-900-EXIT                                   00154200
154300        ELSE                                                      00154300
154400        IF  IPGSOPTI  =  'C'                                      00154400
154500        THEN                                                      00154500
154600            MOVE -1        TO  IPGSOPTL                           00154600
154700            MOVE DFHBMUBF  TO  IPGSOPTA                           00154700
154800            MOVE DFHBMASB  TO  IPGSIDA  IPGSSLTA                  00154800
154900            GO TO 1100-900-EXIT                                   00154900
155000        ELSE                                                      00155000
155100        IF  IDGDOPTI  =  'C'                                      00155100
155200        THEN                                                      00155200
155300            MOVE -1        TO  IDGDOPTL                           00155300
155400            MOVE DFHBMUBF  TO  IDGDOPTA                           00155400
155500            MOVE DFHBMASB  TO  IDGDIDA  IDGDSLTA                  00155500
155600            GO TO 1100-900-EXIT                                   00155600
155700        ELSE                                                      00155700
155800        IF  IPGPOPTI  =  'C'                                      00155800
155900        THEN                                                      00155900
156000             MOVE -1        TO  IPGPOPTL                          00156000
156100             MOVE DFHBMUBF  TO  IPGPOPTA                          00156100
156200             MOVE DFHBMASB  TO  IPGPIDA  IPGPSLTA                 00156200
156300             GO TO 1100-900-EXIT                                  00156300
156400         ELSE                                                     00156400
156500             NEXT SENTENCE                                        00156500
156600     ELSE                                                         00156600
156700         NEXT SENTENCE.                                           00156700
156800                                                                  00156800
156900                                                                  00156900
157000     IF  NOT GCIO2-GOOD-RETURN AND  GCIO-TAB-SLOT-NO  NOT =  1    00157000
157100     THEN                                                         00157100
157200         SET  WT-01-INDEX                     TO +18              00157200
157300         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00157300
157400         MOVE 'Y'                             TO ACWA-ERROR-SW    00157400
157500         MOVE -1                              TO MFRMSLTL         00157500
157600         MOVE DFHBMUBF                        TO MFRMSLTA         00157600
157700         GO TO 1100-900-EXIT.                                     00157700
157800                                                                  00157800
157900     IF  NOT GCIO2-GOOD-RETURN                                    00157900
158000     THEN                                                         00158000
158100         MOVE WS-ABCODE-1CF1        TO WS-ABCODE                  00158100
158200         MOVE WS-ABCODE-1CF1-MSG    TO WS-ABCODE-MSG              00158200
158300         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                   00158300
158400                                                                  00158400
158500     MOVE GXA-ENTRY-COUNT  TO  GXA-ENTRY-COUNT.                   00158500
158600                                                                  00158600
158700     COMPUTE  GCIO2-RECORD-LENGTH  =                              00158700
158800              GC-WORKFILE-KEY-LEN  +                              00158800
158900              GCIO2-RECORD-LENGTH.                                00158900
159000                                                                  00159000
159100     IF IBGROPTI  =  'C' OR  'MT' OR 'A'                          00159100
159200        IF  GXA-ENTRY-COUNT  NOT >  1                             00159200
159300            MOVE 'GA2GPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00159300
159400                                INTR-TAB-PGM-ID                   00159400
159500        ELSE                                                      00159500
159600            MOVE 'GA1GPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00159600
159700                                INTR-TAB-PGM-ID.                  00159700
159800                                                                  00159800
159900     IF IPGNOPTI  =  'C' OR  'MT' OR 'A'                          00159900
160000        IF  GXA-ENTRY-COUNT  NOT >  1                             00160000
160100            MOVE 'GA2HPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00160100
160200                                INTR-TAB-PGM-ID                   00160200
160300        ELSE                                                      00160300
160400            MOVE 'GA1HPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00160400
160500                                INTR-TAB-PGM-ID.                  00160500
160600                                                                  00160600
160700     IF IPGTOPTI  =  'C' OR  'MT' OR 'A'                          00160700
160800        IF  GXA-ENTRY-COUNT  NOT >  1                             00160800
160900            MOVE 'GA2IPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00160900
161000                                INTR-TAB-PGM-ID                   00161000
161100        ELSE                                                      00161100
161200            MOVE 'GA1IPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00161200
161300                                INTR-TAB-PGM-ID.                  00161300
161400                                                                  00161400
161500     IF IPGSOPTI  =  'C' OR  'MT' OR 'A'                          00161500
161600        IF  GXA-ENTRY-COUNT  NOT >  1                             00161600
161700            MOVE 'GA2SPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00161700
161800                                INTR-TAB-PGM-ID                   00161800
161900        ELSE                                                      00161900
162000            MOVE 'GA1SPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00162000
162100                                INTR-TAB-PGM-ID.                  00162100
162200                                                                  00162200
162300     IF IDGDOPTI  =  'C' OR  'MT' OR 'A'                          00162300
162400        IF  GXA-ENTRY-COUNT  NOT >  1                             00162400
162500            MOVE 'GA2NPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00162500
162600                                INTR-TAB-PGM-ID                   00162600
162700        ELSE                                                      00162700
162800            MOVE 'GA1NPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00162800
162900                                INTR-TAB-PGM-ID.                  00162900
163000                                                                  00163000
163100     IF IPGPOPTI  =  'C' OR  'MT' OR 'A'                          00163100
163200        IF  GXA-ENTRY-COUNT  NOT >  1                             00163200
163300            MOVE 'GA2OPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00163300
163400                                INTR-TAB-PGM-ID                   00163400
163500        ELSE                                                      00163500
163600            MOVE 'GA1OPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00163600
163700                                INTR-TAB-PGM-ID.                  00163700
163800                                                                  00163800
163900     GO TO 1100-900-EXIT.                                         00163900
164000                                                                  00164000
164100/                                                                 00164100
164200 1100-100-GET-INTERNAL-TAB.                                       00164200
164300                                                                  00164300
164400                                                                  00164400
164500     IF FRMNUIDI  =  'GS3A'                                       00164500
164600        PERFORM 6000-000-BUILD-GROUP-SPEC-KEY.                    00164600
164700        MOVE 'G4'  TO  GCIO-WRK-RECORD-TYPE.                      00164700
164800                                                                  00164800
164900     IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            00164900
165000        PERFORM 6100-000-BUILD-CONTRACT-KEY                       00165000
165100        MOVE 'C3'  TO  GCIO-WRK-RECORD-TYPE.                      00165100
165200                                                                  00165200
165300     IF FRMNUIDI  =  'GC8A'                                       00165300
165400        PERFORM 6200-000-BUILD-BEN-PROV-KEY                       00165400
165500        MOVE 'C6'               TO GCIO-WRK-RECORD-TYPE           00165500
165600        MOVE TABIDI             TO GCIO-WRK-PROVISION-ID          00165600
165700        MOVE TABSLTNI           TO ACWA-DISPLAY-LEN-7             00165700
165800        MOVE ACWA-DISPLAY-LEN-7 TO GCIO-WRK-PROVISION-SLOT-NO.    00165800
165900                                                                  00165900
166000     MOVE GC-GCPSWORK-DDNAME TO GCIO2-FILE-DDNAME.                00166000
166100     MOVE GC-GCIO-AREA-1     TO GCIO2-IO-AREA-TO-USE.             00166100
166200                                                                  00166200
166300     IF IBGROPTI  =  'C'                                          00166300
166400        MOVE '#IBGR '  TO  GCIO-WRK-TAB-PROVISION-ID              00166400
166500        MOVE IBGRSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00166500
166600                                                                  00166600
166700     IF IPGNOPTI  =  'C'                                          00166700
166800        MOVE '#IPGN '  TO  GCIO-WRK-TAB-PROVISION-ID              00166800
166900        MOVE IPGNSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00166900
167000                                                                  00167000
167100     IF IPGTOPTI  =  'C'                                          00167100
167200        MOVE '#IPGT '  TO  GCIO-WRK-TAB-PROVISION-ID              00167200
167300        MOVE IPGTSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00167300
167400                                                                  00167400
167500     IF IPGSOPTI  =  'C'                                          00167500
167600        MOVE '#IPGS '  TO  GCIO-WRK-TAB-PROVISION-ID              00167600
167700        MOVE IPGSSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00167700
167800                                                                  00167800
167900     IF IDGDOPTI  =  'C'                                          00167900
168000        MOVE '#IDGD '  TO  GCIO-WRK-TAB-PROVISION-ID              00168000
168100        MOVE IDGDSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00168100
168200                                                                  00168200
168300     IF IPGPOPTI  =  'C'                                          00168300
168400        MOVE '#IPGP '  TO  GCIO-WRK-TAB-PROVISION-ID              00168400
168500        MOVE IPGPSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00168500
168600                                                                  00168600
168700     MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              00168700
168800     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO2-FILE-ACCESS-CODE.       00168800
168900     MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          00168900
169000          TO  GXA-ENTRY-COUNT.                                    00169000
169100                                                                  00169100
169200     EXEC CICS  LINK  PROGRAM('GCIOPGM')                          00169200
169300                COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          00169300
169400                LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. 00169400
169500                                                                  00169500
169600     IF NOT GCIO2-GOOD-RETURN                                     00169600
169700        MOVE WS-ABCODE-1CF2        TO WS-ABCODE                   00169700
169800        MOVE WS-ABCODE-1CF2-MSG    TO WS-ABCODE-MSG               00169800
169900        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00169900
170000                                                                  00170000
170100     MOVE GXA-ENTRY-COUNT  TO  GXA-ENTRY-COUNT.                   00170100
170200                                                                  00170200
170300     IF IBGROPTI  =  'C'                                          00170300
170400        IF GXA-ENTRY-COUNT  NOT >  1                              00170400
170500           MOVE 'GA2GPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00170500
170600                                INTR-TAB-PGM-ID                   00170600
170700        ELSE                                                      00170700
170800           MOVE 'GA1GPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00170800
170900                                INTR-TAB-PGM-ID.                  00170900
171000                                                                  00171000
171100     IF IPGNOPTI  =  'C'                                          00171100
171200        IF GXA-ENTRY-COUNT  NOT >  1                              00171200
171300           MOVE 'GA2HPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00171300
171400                                INTR-TAB-PGM-ID                   00171400
171500        ELSE                                                      00171500
171600           MOVE 'GA1HPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00171600
171700                                INTR-TAB-PGM-ID.                  00171700
171800                                                                  00171800
171900     IF IPGTOPTI  =  'C'                                          00171900
172000        IF GXA-ENTRY-COUNT  NOT >  1                              00172000
172100           MOVE 'GA2IPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00172100
172200                                INTR-TAB-PGM-ID                   00172200
172300        ELSE                                                      00172300
172400           MOVE 'GA1IPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00172400
172500                                INTR-TAB-PGM-ID.                  00172500
172600                                                                  00172600
172700     IF IPGSOPTI  =  'C'                                          00172700
172800        IF GXA-ENTRY-COUNT  NOT >  1                              00172800
172900           MOVE 'GA2SPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00172900
173000                                INTR-TAB-PGM-ID                   00173000
173100        ELSE                                                      00173100
173200           MOVE 'GA1SPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00173200
173300                                INTR-TAB-PGM-ID.                  00173300
173400                                                                  00173400
173500     IF IDGDOPTI  =  'C'                                          00173500
173600        IF GXA-ENTRY-COUNT  NOT >  1                              00173600
173700           MOVE 'GA2NPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00173700
173800                                INTR-TAB-PGM-ID                   00173800
173900        ELSE                                                      00173900
174000           MOVE 'GA1NPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00174000
174100                                INTR-TAB-PGM-ID.                  00174100
174200                                                                  00174200
174300     IF IPGPOPTI  =  'C'                                          00174300
174400        IF GXA-ENTRY-COUNT  NOT >  1                              00174400
174500           MOVE 'GA2OPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00174500
174600                                INTR-TAB-PGM-ID                   00174600
174700        ELSE                                                      00174700
174800           MOVE 'GA1OPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00174800
174900                                INTR-TAB-PGM-ID.                  00174900
175000                                                                  00175000
175100     GO TO 1100-900-EXIT.                                         00175100
175200                                                                  00175200
175300 1100-900-EXIT. EXIT.                                             00175300
175400/*****************************************************************00175400
175500* 2000  PROCESS REQUEST                                          *00175500
175600*                                                                *00175600
175700*   THIS ROUTINE CHECKS THE CHARACTERISTICS OF THE INCOMING TRANS-00175700
175800* ACTION AND ROUTES THEM TO THE APPROPRIATE ROUTINE TO PROCESS   *00175800
175900* THE REQUEST.                                                   *00175900
176000******************************************************************00176000
176100 2000-000-PROCESS-REQUEST       SECTION.                          00176100
176200 2000-010.                                                        00176200
176300                                                                  00176300
176400     IF (EIBAID       =  DFHENTER OR DFHPF4 OR DFHPF16) AND       00176400
176500        DELADDI       =  'CHG/ADD'                      AND       00176500
176600        OENTCTRI      =  '0000000'                                00176600
176700        PERFORM 2100-000-INSERT-SKELETON.                         00176700
176800                                                                  00176800
176900** LINK TO THE CHANGE/DELETE MODULE TO PROCESS FIVE DIFFERENT     00176900
177000** REQUESTS DEPENDING ON THE USER RESPONSE                        00177000
177100**                                                                00177100
177200     MOVE 'CHG/DEL' TO DELADD-OPTION.                             00177200
177300     MOVE SPACES TO LVL2-B-SW  LVL2-F-SW  LVL2-G-SW.              00177300
177400                                                                  00177400
177500     MOVE WS-ALT-WORKFILE-KEYS       TO ACWA-ALT-WORKFILE-KEYS.   00177500
177600***  SET  ACWA-INDEX-1               TO GAB-INDEX.                00177600
177700                                                                  00177700
177800     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00177800
177900                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00177900
178000                                                                  00178000
178100     EXEC CICS  LINK  PROGRAM ('GAS2UPD')                         00178100
178200                COMMAREA(COMMON-WORKAREAS)                        00178200
178300                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00178300
178400                                                                  00178400
178500     SET ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD  TO             00178500
178600                 ACWA-WF-ALL-LEVEL-TAB-PNTR.                      00178600
178700                                                                  00178700
178800     SET ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD TO             00178800
178900                 ACWA-WF-INTERNAL-TAB-PNTR.                       00178900
179000                                                                  00179000
179100                                                                  00179100
179200     IF LVL2-B-SW =  'Y'                                          00179200
179300        PERFORM  7900-000-RESET-ATTRIBUTES                        00179300
179400        PERFORM  8100-000-DISPLAY-ADD-SCREEN.                     00179400
179500                                                                  00179500
179600     IF LVL2-F-SW =  'Y'                                          00179600
179700        PERFORM  3100-000-READ-RECORD                             00179700
179800        PERFORM  4300-000-DISPLAY-PREV.                           00179800
179900                                                                  00179900
180000     IF LVL2-G-SW =  'Y'                                          00180000
180100        PERFORM  3100-000-READ-RECORD                             00180100
180200        PERFORM  4200-000-DISPLAY-NEXT.                           00180200
180300                                                                  00180300
180400                                                                  00180400
180500 2000-900-EXIT. EXIT.                                             00180500
180600                                                                  00180600
180700/*****************************************************************00180700
180800* 2100  INSERT SKELETON                                          *00180800
180900*                                                                *00180900
181000*    THIS ROUTINE WILL ADD A NEW ENTRY INTO THE TABLE.  IF THE   *00181000
181100*  TABLE ALREADY CONTAINS THE MAXIMUM NUMBER OF 29 ENTRIES THE   *00181100
181200*  SORT ROUTINE MAY REDUCE THAT NUMBER AS IT WILL DELETE ALL     *00181200
181300*  DUPLICATES.  IF THE NEW ENTRY CAN BE ADDED AND THE OPERATOR   *00181300
181400*  REQUESTED THE ADDITION OF AN INTERNAL TABULAR THIS ROUTINE    *00181400
181500*  WILL WRITE THE NEW INTERNAL TABULAR TO THE WORKFILE AND THEN  *00181500
181600*  PASS IT TO THE ADD VERSION OF THE INTERNAL TABULAR PROGRAM.   *00181600
181700******************************************************************00181700
181800 2100-000-INSERT-SKELETON       SECTION.                          00181800
181900 2100-010.                                                        00181900
182000                                                                  00182000
182100     IF  EIBAID = DFHENTER         AND                            00182100
182200         ACWA-SCREEN-HAS-NO-ERRORS AND                            00182200
182300         GCVI-TABLE-SW = 'N'                                      00182300
182400     THEN                                                         00182400
182500         SET  WT-01-INDEX                     TO +07              00182500
182600         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00182600
182700         PERFORM 9010-000-SEND-DATAONLY-RETURN.                   00182700
182800                                                                  00182800
182900     IF  EIBAID  = DFHPF4 OR DFHPF16                              00182900
183000         PERFORM 7900-000-RESET-ATTRIBUTES.                       00183000
183100                                                                  00183100
183200     PERFORM 3200-000-READ-REC-FOR-UPDATE.                        00183200
183300                                                                  00183300
183400     IF NOT GCIO-GOOD-RETURN                                      00183400
183500        MOVE WS-ABCODE-1CF5        TO WS-ABCODE                   00183500
183600        MOVE WS-ABCODE-1CF5-MSG    TO WS-ABCODE-MSG               00183600
183700        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00183700
183800                                                                  00183800
183900                                                                  00183900
184000     IF GAB-OCC-ENTRY-TAB-SLOT-CNTR > 9999900                     00184000
184100        SET  WT-01-INDEX                     TO +15               00184100
184200        MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO           00184200
184300        MOVE -1                              TO PERIODL           00184300
184400        PERFORM 9010-000-SEND-DATAONLY-RETURN.                    00184400
184500                                                                  00184500
184600                                                                  00184600
184700     MOVE GAB-ENTRY-COUNT  TO  GAB-ENTRY-COUNT.                   00184700
184800                                                                  00184800
184900     IF  GAB-ENTRY-COUNT  NOT <  GC-GCTABULR-ACL-VARY-MAX-OCUR    00184900
185000     THEN                                                         00185000
185100         PERFORM 6500-000-SORT-COMPRESS-ALL-LVL                   00185100
185200         IF  ACWA-SCREEN-HAS-NO-ERRORS                            00185200
185300         THEN                                                     00185300
185400             PERFORM 2500-000-ADD-NEW-OCCURS                      00185400
185500***          PERFORM 4600-000-UPDATE-CDE-STATUS                   00185500
185600             IF  WRK-CDE-SP = '2 '      AND                       00185600
185700                 CDEINDO    = ('+CDE+'  OR '+CDE-')               00185700
185800                 ADD 1      TO   ACWA-CDE-1U-COUNT                00185800
185900                 SUBTRACT 1 FROM ACWA-CDE-2B-COUNT                00185900
186000                 MOVE '1U'  TO   WRK-CDE-SP                       00186000
186100                 PERFORM 3000-000-UPDATE-GAB-RECORD               00186100
186200             ELSE                                                 00186200
186300                 PERFORM 3000-000-UPDATE-GAB-RECORD               00186300
186400         ELSE                                                     00186400
186500             SET  WT-01-INDEX                     TO +16          00186500
186600             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      00186600
186700             MOVE -1                              TO  PERIODL     00186700
186800             PERFORM 9010-000-SEND-DATAONLY-RETURN                00186800
186900      ELSE                                                        00186900
187000          PERFORM 2500-000-ADD-NEW-OCCURS                         00187000
187100***       PERFORM 4600-000-UPDATE-CDE-STATUS                      00187100
187200             IF  WRK-CDE-SP = '2 '     AND                        00187200
187300                 CDEINDO    = ('+CDE+'  OR '+CDE-')               00187300
187400                 ADD 1      TO   ACWA-CDE-1U-COUNT                00187400
187500                 SUBTRACT 1 FROM ACWA-CDE-2B-COUNT                00187500
187600                 MOVE '1U'  TO   WRK-CDE-SP                       00187600
187700                 PERFORM 3000-000-UPDATE-GAB-RECORD               00187700
187800             ELSE                                                 00187800
187900                 PERFORM 3000-000-UPDATE-GAB-RECORD.              00187900
188000                                                                  00188000
188100     SET GAB-INDEX  DOWN BY  1.                                   00188100
188200                                                                  00188200
188300     IF  (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE) AND           00188300
188400         (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE) AND           00188400
188500         (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE) AND           00188500
188600         (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE) AND           00188600
188700         (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE) AND           00188700
188800         (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE)               00188800
188900     THEN                                                         00188900
189000         PERFORM 4700-000-UPDATE-CONTROL-RECORD                   00189000
189100         MOVE GAB-OCCURS-ENTRY-COUNTER (GAB-INDEX)                00189100
189200                                 TO  ACWA-DISPLAY-LEN-7           00189200
189300         MOVE ACWA-DISPLAY-LEN-7 TO OENTCTRO                      00189300
189400         MOVE -1                 TO  PERIODL                      00189400
189500         PERFORM 9010-000-SEND-DATAONLY-RETURN.                   00189500
189600                                                                  00189600
189700*    EXEC CICS GETMAIN                                            00189700
189800*              SET(ADDRESS OF COMMUNICATION-KEY-AREA)             00189800
189900*              INITIMG(WS-HEX-00)                                 00189900
190000*              LENGTH(WS-COMMUNICATION-KEY-LEN)                   00190000
190100*              END-EXEC.                                          00190100
190200                                                                  00190200
190300*    SET ACWA-COMM-KEY-PNTR  TO                                   00190300
190400*                      ADDRESS OF COMMUNICATION-KEY-AREA.         00190400
190500                                                                  00190500
190600     IF FRMNUIDI  =  'GS3A'                                       00190600
190700        MOVE 'G4'    TO  GCIO-WRK-RECORD-TYPE                     00190700
190800        MOVE SPACES  TO  GCA-L-O-B                                00190800
190900                         GCA-PROV-CTL                             00190900
191000                         GCA-BEN-PROV-ID.                         00191000
191100                                                                  00191100
191200     IF FRMNUIDI = 'GC4A' OR 'GTM1'                               00191200
191300        MOVE 'C3'                       TO  GCIO-WRK-RECORD-TYPE  00191300
191400        MOVE GCIO-WRK-LINE-OF-BUS       TO  GCA-L-O-B             00191400
191500        MOVE GCIO-WRK-PROVIDER-CONTROL  TO  GCA-PROV-CTL          00191500
191600        MOVE SPACES  TO  GCA-BEN-PROV-ID.                         00191600
191700                                                                  00191700
191800     IF FRMNUIDI  =  'GC8A'                                       00191800
191900        MOVE 'C6'                       TO  GCIO-WRK-RECORD-TYPE  00191900
192000        MOVE GCIO-WRK-LINE-OF-BUS       TO  GCA-L-O-B             00192000
192100        MOVE GCIO-WRK-PROVIDER-CONTROL  TO  GCA-PROV-CTL          00192100
192200        MOVE GCIO-WRK-PROVISION-ID      TO  GCA-BEN-PROV-ID       00192200
192300        MOVE GCIO-WRK-TABULAR-PROVISION TO                        00192300
192400                                     GCIO-WRK-BENEFIT-PROVISION.  00192400
192500                                                                  00192500
192600     MOVE GCIO-WRK-EFFDT-CEN       TO GCA-EFFDT-CEN.              00192600
192700     MOVE GCIO-WRK-EFFECTIVE-DATE  TO HGADATE-JULIAN1.            00192700
192800     PERFORM 9300-000-JULIAN-TO-GREGORIAN.                        00192800
192900     MOVE HGADATE-DATE2            TO GCA-EFFECTIVE-DATE.         00192900
193000     MOVE GC-GCPSWORK-DDNAME       TO GCIO2-FILE-DDNAME.          00193000
193100     MOVE GC-GCIO-AREA-1           TO GCIO2-IO-AREA-TO-USE.       00193100
193200     MOVE TABIDI                   TO GCA-ALL-LEVEL-TAB-ID.       00193200
193300     MOVE TABSLTNI                 TO ACWA-DISPLAY-LEN-7.         00193300
193400     MOVE ACWA-DISPLAY-LEN-7       TO GCA-ALL-LEVEL-TAB-SLOT.     00193400
193500     MOVE GXA-PROVISION-ID         TO GCIO-WRK-TAB-PROVISION-ID,  00193500
193600                                      GCA-INTERNAL-TAB-ID.        00193600
193700     MOVE GXA-INCLUDE-EXCLUDE-IND  TO GCA-I-E-INDC.               00193700
193800     MOVE GXA-PROVISION-SLOT-NO    TO SAVE-COPY-FROM-SLOT.        00193800
193900     MOVE GAB-OCCURS-ENTRY-COUNTER (GAB-INDEX)                    00193900
194000                                   TO GCIO-WRK-TAB-PROV-SLOT-NO   00194000
194100                                      GXA-PROVISION-SLOT-NO       00194100
194200                                      ACWA-DISPLAY-LEN-7.         00194200
194300     MOVE ACWA-DISPLAY-LEN-7       TO GCA-INTERNAL-TAB-SLOT       00194300
194400                                      GCA-OCCURS-ENTRY-COUNTER.   00194400
194500     MOVE 'A'                      TO GCA-ADD-DEL-IND.            00194500
194600     MOVE 'CHG/ADD'                TO DELADD-OPTION.              00194600
194700     MOVE FRMNUIDI                 TO GCA-FROM-MENU-ID.           00194700
194800     MOVE FUNCTONI                 TO GCA-ALL-LEVEL-TAB-FUNC-CODE.00194800
194900     MOVE GCIO-WRK-PLAN-CODE       TO GCA-PLAN-CODE.              00194900
195000     MOVE GCIO-WRK-GROUP-NUM       TO GCA-GROUP-NUM.              00195000
195100     MOVE GCIO-WRK-SECTION-NUM     TO GCA-SECTION-NUM.            00195100
195200     MOVE GCIO-WRK-PKG-CODE        TO GCA-PKG-CODE.               00195200
195300     MOVE GCIO-WRK-FAMILY-RELATION-LVL                            00195300
195400                                   TO GCA-FAM-REL-LVL.            00195400
195500     MOVE SPACES                   TO WORK-RECORD-2.              00195500
195600     MOVE GCIO-WORKFILE-KEY        TO GCIO2-FILE-KEY              00195600
195700                                      WORK-RECORD-KEY-2.          00195700
195800     MOVE SAVE-COPY-FROM-SLOT      TO WRK2-PROV-POOL-COPY-SLOT.   00195800
195900                                                                  00195900
196000     IF FRMNUIDI  =  'GC8A'                                       00196000
196100        MOVE GCIO-WRK-PROVISION-ID TO WRK2-ALL-LEV-BEN-PROV.      00196100
196200                                                                  00196200
196300                                                                  00196300
196400     IF WRK-SIGNAL-FROM-ONLINE  =  'W'                            00196400
196500        MOVE 'W'       TO  WRK2-SIGNAL-FROM-ONLINE                00196500
196600        MOVE '1U'      TO  WRK2-CDE-SP                            00196600
196700        ADD   1        TO  ACWA-CDE-1U-COUNT                      00196700
196800     ELSE                                                         00196800
196900        IF  CDEINDO  = ('+CDE+'  OR  '+CDE-')                     00196900
197000            MOVE '1U'   TO  WRK2-CDE-SP                           00197000
197100            ADD   1     TO  ACWA-CDE-1U-COUNT                     00197100
197200        ELSE                                                      00197200
197300            MOVE '2 '   TO  WRK2-CDE-SP                           00197300
197400            ADD   1     TO  ACWA-CDE-2B-COUNT.                    00197400
197500                                                                  00197500
197600** SET INDICATOR TO CAPTURE OPERATOR-ID.                          00197600
197700     MOVE '1'          TO  GCIO2-OPER-ID-IND.                     00197700
197800                                                                  00197800
197900     PERFORM 4700-000-UPDATE-CONTROL-RECORD.                      00197900
198000     MOVE GC-GCIO-ACCESS-CODE-WR   TO  GCIO2-FILE-ACCESS-CODE.    00198000
198100                                                                  00198100
198200     COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   00198200
198300              GC-GCIOPARM-LEN  +  GCIO2-RECORD-LENGTH.            00198300
198400                                                                  00198400
198500     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00198500
198600                COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          00198600
198700                LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. 00198700
198800                                                                  00198800
198900     IF NOT GCIO2-GOOD-RETURN                                     00198900
199000        MOVE WS-ABCODE-1CF6        TO WS-ABCODE                   00199000
199100        MOVE WS-ABCODE-1CF6-MSG    TO WS-ABCODE-MSG               00199100
199200        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00199200
199300                                                                  00199300
199400     SET GCA-RECORD-POINTER  TO                                   00199400
199500                    ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD.    00199500
199600                                                                  00199600
199700     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00199700
199800                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00199800
199900     EXEC CICS  XCTL  PROGRAM(WS-INTERNAL-TABULAR-PGM-ID)         00199900
200000                COMMAREA(COMMON-WORKAREAS)                        00200000
200100                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00200100
200200                                                                  00200200
200300 2100-900-EXIT.                                                   00200300
200400           EXIT.                                                  00200400
200500/*****************************************************************00200500
200600* 2500  ADD NEW OCCURS                                           *00200600
200700*                                                                *00200700
200800*     INITIALIZE ENTRY IN THE TABLE TO EITHER SPACES OR ZEROS,   *00200800
200900*  THEN IF A FIELD WAS ENTERED BY THE OPERATOR MOVE IT TO THE    *00200900
201000*  TABLE IN THE RECORD.                                          *00201000
201100******************************************************************00201100
201200 2500-000-ADD-NEW-OCCURS        SECTION.                          00201200
201300 2500-010.                                                        00201300
201400                                                                  00201400
201500     MOVE GAB-ENTRY-COUNT  TO GAB-ENTRY-COUNT.                    00201500
201600     SET GAB-INDEX         TO GAB-ENTRY-COUNT.                    00201600
201700     MOVE LOW-VALUES       TO GAB-ENTRY (GAB-INDEX).              00201700
201800                                                                  00201800
201900     MOVE ZEROS TO GAB-COINS-INTL-TAB-SLOT-2 (GAB-INDEX),         00201900
202000                   GAB-COINS-INTL-TAB-SLOT-3 (GAB-INDEX),         00202000
202100                   GAB-COINS-INTL-TAB-SLOT-4 (GAB-INDEX),         00202100
202200                   GAB-COINS-INTL-TAB-SLOT-5 (GAB-INDEX).         00202200
202300                                                                  00202300
202400                                                                  00202400
202500     MOVE SPACES TO GAB-COINS-INTL-TAB-TAB-ID-2 (GAB-INDEX)       00202500
202600                    GAB-COINS-INTL-TAB-TAB-ID-3 (GAB-INDEX)       00202600
202700                    GAB-COINS-INTL-TAB-TAB-ID-4 (GAB-INDEX)       00202700
202800                    GAB-COINS-INTL-TAB-TAB-ID-5 (GAB-INDEX).      00202800
202900                                                                  00202900
203000     MOVE GAB-OCC-ENTRY-TAB-SLOT-CNTR                             00203000
203100                    TO GAB-OCCURS-ENTRY-COUNTER       (GAB-INDEX).00203100
203200     ADD 1          TO GAB-OCC-ENTRY-TAB-SLOT-CNTR.               00203200
203300     MOVE DAYFACII  TO GAB-COINS-DAY-FACTOR-IND       (GAB-INDEX).00203300
203400     MOVE COPAYINI  TO GAB-COINS-CO-PAY-IND           (GAB-INDEX).00203400
203500     MOVE BISNDINI  TO GAB-COINS-BISCENDING-IND       (GAB-INDEX).00203500
203600     MOVE ASCDSCDI  TO GAB-COINS-ASCEND-DESCEND-IND   (GAB-INDEX).00203600
      *P21595 CHANGES STARTS                                            00203610
203600     MOVE BENTYPI   TO GAB-COINS-BEN-TYPE             (GAB-INDEX).00203611
203600     MOVE TIERCDI   TO GAB-COINS-TIER-CODE            (GAB-INDEX).00203612
203600     MOVE TIERLVI   TO GAB-COINS-TIER-LVL             (GAB-INDEX).00203613
      *P21595 CHANGES ENDS                                              00203614
203700     MOVE CARYOVRI  TO GAB-CARRY-OVER-CREDIT-IND      (GAB-INDEX).00203700
203800     MOVE FYIVALI   TO GAB-COINS-FYI-VALUE            (GAB-INDEX).00203800
203900     MOVE CSTCONTI  TO GAB-COINS-COST-CONTAIN-IND     (GAB-INDEX).00203900
204000     MOVE PERIODI   TO GAB-COINS-BENEFIT-PERIOD       (GAB-INDEX).00204000
204100     MOVE DEFINTNI  TO GAB-COINS-DEFINITION           (GAB-INDEX).00204100
204200     MOVE PERTQALI  TO GAB-COINS-BEN-PER-TIME-QUAL    (GAB-INDEX).00204200
204300     MOVE FAMINDII  TO GAB-COINS-FAM-OR-INDIV         (GAB-INDEX).00204300
204400     MOVE PLCTRMTI  TO GAB-COINS-PLACE-OF-TREATMENT   (GAB-INDEX).00204400
204500     MOVE SRVGRUPI  TO GAB-COINS-SERVICE-GROUP        (GAB-INDEX).00204500
204600     MOVE AGELIMLI  TO ACWA-DISPLAY-LEN-3-X.                      00204600
204700     MOVE ACWA-DISPLAY-LEN-3                                      00204700
204800                    TO GAB-COINS-AGE-LIMIT-FROM       (GAB-INDEX).00204800
204900     MOVE AGELIMHI  TO ACWA-DISPLAY-LEN-3-X.                      00204900
205000     MOVE ACWA-DISPLAY-LEN-3                                      00205000
205100                    TO GAB-COINS-AGE-LIMIT-TO         (GAB-INDEX).00205100
205200     MOVE FEAKINDI  TO GAB-COINS-FEAK-IND             (GAB-INDEX).00205200
205300     MOVE ACCUMIDI  TO GAB-COINS-ACCUMID              (GAB-INDEX).00205300
205400     MOVE CAPINDI   TO GAB-COINS-COMB-APPLIED-IND     (GAB-INDEX).00205400
205500     MOVE SABDINDI  TO GAB-COINS-SEL-ADDL-BEN-DET     (GAB-INDEX).00205500
205600     MOVE AGEQLLI   TO GAB-COINS-AGE-QUAL-IND-FROM    (GAB-INDEX).00205600
205700     MOVE AGEQLHI   TO GAB-COINS-AGE-QUAL-IND-TO      (GAB-INDEX).00205700
205800     MOVE RELPINDI  TO GAB-COINS-RELATIONSHIP-IND     (GAB-INDEX).00205800
205900                                                                  00205900
206000     MOVE PRTIMEFI  TO ACWA-DISPLAY-LEN-3-X.                      00206000
206100     MOVE ACWA-DISPLAY-LEN-3                                      00206100
206200                    TO GAB-COINS-BEN-PER-TIME-FCTR    (GAB-INDEX).00206200
206300     MOVE REININDI  TO GAB-COINS-REINSTATEMENT-IND    (GAB-INDEX).00206300
206400     MOVE MANAPLII  TO GAB-COINS-LMT-MANDATORY-IND    (GAB-INDEX).00206400
206500     MOVE FDLRCLII  TO GAB-COINS-1ST-DOLR-COVRGE-LMT  (GAB-INDEX).00206500
206600     MOVE CLMLVLII  TO GAB-COINS-CLAIM-LVL-ACCUM-IND  (GAB-INDEX).00206600
206700     MOVE INTRVALI  TO ACWA-DISPLAY-LEN-3-X.                      00206700
206800     MOVE ACWA-DISPLAY-LEN-3                                      00206800
206900                    TO GAB-COINS-INTERVAL-TIME-FCTR   (GAB-INDEX).00206900
207000     MOVE INTTYPEI  TO GAB-COINS-INTERVAL-TYPE        (GAB-INDEX).00207000
207100     MOVE LOBI      TO GAB-COINS-L-O-B                (GAB-INDEX).00207100
207200                                                                  00207200
207300     PERFORM 2600-000-PROCESS-VAL-LIMIT.                          00207300
207400                                                                  00207400
207500     MOVE ACWA-VALUE-LIMIT-9                                      00207500
207600                    TO GAB-COINS-VALUE-LIMIT          (GAB-INDEX).00207600
207700     MOVE BENVLQLI  TO GAB-COINS-VALUE-QUALIFIER      (GAB-INDEX).00207700
207800     MOVE PERLIMTI  TO ACWA-DISPLAY-LEN-3-X.                      00207800
207900     MOVE ACWA-DISPLAY-LEN-3                                      00207900
208000                    TO GAB-COINS-PERCENT-LEVEL        (GAB-INDEX).00208000
208100     MOVE NEWVALUI  TO ACWA-DISPLAY-LEN-5-X.                      00208100
208200     MOVE ACWA-DISPLAY-LEN-5                                      00208200
208300                    TO GAB-COINS-INTERVAL-OVRD-VALUE  (GAB-INDEX).00208300
208400     MOVE OVRDINDI  TO GAB-COINS-INTERVAL-OVRD-IND    (GAB-INDEX).00208400
208500     MOVE INTDESKI  TO GAB-COINS-INTERNAL-DESCRIPTOR  (GAB-INDEX).00208500
208600     MOVE CONDALLI  TO GAB-COND-ALL-BIT               (GAB-INDEX).00208600
208700     MOVE CONDEXCI  TO GAB-COND-EXCLUSION-BIT         (GAB-INDEX).00208700
208800     MOVE CONDICDI  TO GAB-COND-ICD-BIT               (GAB-INDEX).00208800
208900     MOVE CONDTABI  TO GAB-COND-TB-BIT                (GAB-INDEX).00208900
209000     MOVE CONDMENI  TO GAB-COND-MENTAL-BIT            (GAB-INDEX).00209000
209100     MOVE CONDDRGI  TO GAB-COND-DRUG-BIT              (GAB-INDEX).00209100
209200     MOVE CONDALCI  TO GAB-COND-ALCOHOL-BIT           (GAB-INDEX).00209200
209300     MOVE CONDOBCI  TO GAB-COND-OB-COMP-BIT           (GAB-INDEX).00209300
209400     MOVE CONDOBNI  TO GAB-COND-OB-NORM-BIT           (GAB-INDEX).00209400
209500     MOVE CONDMALI  TO GAB-COND-MALIGNANCY-BIT        (GAB-INDEX).00209500
209600     MOVE CONDCARI  TO GAB-COND-CARDIAC-DISEASE-BIT   (GAB-INDEX).00209600
209700     MOVE CONDOBSI  TO GAB-COND-OBESITY-BIT           (GAB-INDEX).00209700
209800     MOVE CONDKDYI  TO GAB-COND-KIDNEY-DISEASE-BIT    (GAB-INDEX).00209800
209900     MOVE CONDACCI  TO GAB-COND-ACCIDENT-BIT          (GAB-INDEX).00209900
210000     MOVE CONDPECI  TO GAB-COND-PRE-EXIST-BIT         (GAB-INDEX).00210000
210100     MOVE CONDNEMI  TO GAB-COND-NON-EMER-BIT          (GAB-INDEX).00210100
210200     MOVE CONDSUII  TO GAB-COND-SUICIDE-BIT           (GAB-INDEX).00210200
210300     MOVE CONDTMJI  TO GAB-COND-TMJ-BIT               (GAB-INDEX).00210300
210400     MOVE CONDINFI  TO GAB-COND-INF-BIT               (GAB-INDEX).00210400
210500     MOVE CONDLIFI  TO GAB-COND-LIFE-THREAT-BIT       (GAB-INDEX).00210500
210600     MOVE CONDEMCI  TO GAB-COND-EMER-MED-BIT          (GAB-INDEX).00210600
210700     MOVE CONDEACI  TO GAB-COND-EMER-ACC-BIT          (GAB-INDEX).00210700
210800     MOVE CONDSMII  TO GAB-COND-SER-MEN-ILL-BIT       (GAB-INDEX).00210800
210900     MOVE CONDNSMI  TO GAB-COND-NON-SER-MEN-ILL-BIT    (GAB-INDEX)00210900
211000                                                                  00211000
211100                                                                  00211100
211200     IF  (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE) AND           00211200
211300         (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE) AND           00211300
211400         (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE) AND           00211400
211500         (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE) AND           00211500
211600         (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE) AND           00211600
211700         (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE)               00211700
211800     THEN                                                         00211800
211900        MOVE 1               TO GAB-INTERNAL-TABULAR-COUNT        00211900
212000                                                       (GAB-INDEX)00212000
212100        MOVE HIGH-VALUES     TO GAB-COINS-INTL-TAB-1   (GAB-INDEX)00212100
212200        SET  GAB-INDEX  UP BY  1                                  00212200
212300        MOVE HIGH-VALUES     TO GAB-ENTRY (GAB-INDEX)             00212300
212400        SET GAB-ENTRY-COUNT  TO GAB-INDEX                         00212400
212500        GO TO 2500-900-EXIT.                                      00212500
212600                                                                  00212600
212700                                                                  00212700
212800     IF IBGROPTI  =  'MT' OR 'A'                                  00212800
212900        MOVE 2           TO GAB-INTERNAL-TABULAR-COUNT (GAB-INDEX)00212900
213000        MOVE HIGH-VALUES TO GAB-COINS-INTL-TAB-2       (GAB-INDEX)00213000
213100        MOVE '#IBGR '   TO GAB-COINS-INTL-TAB-TAB-ID-1 (GAB-INDEX)00213100
213200        MOVE GAB-OCCURS-ENTRY-COUNTER                  (GAB-INDEX)00213200
213300                         TO GAB-COINS-INTL-TAB-SLOT-1 (GAB-INDEX).00213300
213400                                                                  00213400
213500     IF IDGDOPTI  =  'MT' OR 'A'                                  00213500
213600        MOVE 2           TO GAB-INTERNAL-TABULAR-COUNT (GAB-INDEX)00213600
213700        MOVE HIGH-VALUES TO GAB-COINS-INTL-TAB-2       (GAB-INDEX)00213700
213800        MOVE '#IDGD '   TO GAB-COINS-INTL-TAB-TAB-ID-1 (GAB-INDEX)00213800
213900        MOVE GAB-OCCURS-ENTRY-COUNTER                  (GAB-INDEX)00213900
214000                         TO GAB-COINS-INTL-TAB-SLOT-1 (GAB-INDEX).00214000
214100                                                                  00214100
214200     IF IPGNOPTI  =  'MT' OR 'A'                                  00214200
214300        MOVE 2           TO GAB-INTERNAL-TABULAR-COUNT (GAB-INDEX)00214300
214400        MOVE HIGH-VALUES TO GAB-COINS-INTL-TAB-2       (GAB-INDEX)00214400
214500        MOVE '#IPGN '   TO GAB-COINS-INTL-TAB-TAB-ID-1 (GAB-INDEX)00214500
214600        MOVE GAB-OCCURS-ENTRY-COUNTER                  (GAB-INDEX)00214600
214700                         TO GAB-COINS-INTL-TAB-SLOT-1 (GAB-INDEX).00214700
214800                                                                  00214800
214900     IF IPGPOPTI  =  'MT' OR 'A'                                  00214900
215000        MOVE 2           TO GAB-INTERNAL-TABULAR-COUNT (GAB-INDEX)00215000
215100        MOVE HIGH-VALUES TO GAB-COINS-INTL-TAB-2       (GAB-INDEX)00215100
215200        MOVE '#IPGP '   TO GAB-COINS-INTL-TAB-TAB-ID-1 (GAB-INDEX)00215200
215300        MOVE GAB-OCCURS-ENTRY-COUNTER                  (GAB-INDEX)00215300
215400                         TO GAB-COINS-INTL-TAB-SLOT-1 (GAB-INDEX).00215400
215500                                                                  00215500
215600     IF IPGTOPTI  =  'MT' OR 'A'                                  00215600
215700        MOVE 2           TO GAB-INTERNAL-TABULAR-COUNT (GAB-INDEX)00215700
215800        MOVE HIGH-VALUES TO GAB-COINS-INTL-TAB-2       (GAB-INDEX)00215800
215900        MOVE '#IPGT '   TO GAB-COINS-INTL-TAB-TAB-ID-1 (GAB-INDEX)00215900
216000        MOVE GAB-OCCURS-ENTRY-COUNTER                  (GAB-INDEX)00216000
216100                         TO GAB-COINS-INTL-TAB-SLOT-1 (GAB-INDEX).00216100
216200                                                                  00216200
216300     IF IPGSOPTI  =  'MT' OR 'A'                                  00216300
216400        MOVE 2           TO GAB-INTERNAL-TABULAR-COUNT (GAB-INDEX)00216400
216500        MOVE HIGH-VALUES TO GAB-COINS-INTL-TAB-2       (GAB-INDEX)00216500
216600        MOVE '#IPGS '   TO GAB-COINS-INTL-TAB-TAB-ID-1 (GAB-INDEX)00216600
216700        MOVE GAB-OCCURS-ENTRY-COUNTER                  (GAB-INDEX)00216700
216800                         TO GAB-COINS-INTL-TAB-SLOT-1 (GAB-INDEX).00216800
216900                                                                  00216900
217000                                                                  00217000
217100*******                                                           00217100
217200*     *                                                           00217200
217300* STS *==> MAP FROM OPTION UNDER SINGLE TABULAR SUPPORT DOES NOT  00217300
217400*     *     CREATE INTERNAL TAB ON WORKFILE, IT SIMPLY UPDATES THE00217400
217500*     *     THE ACCUM RECORD AND SCREEN, THEN REDISPLAYS SCREEN.  00217500
217600*******                                                           00217600
217700                                                                  00217700
217800     IF  FRMNUIDI =  'GTM1'  AND                                  00217800
217900         IBGROPTI =  'MT'                                         00217900
218000     THEN                                                         00218000
218100         MOVE MFRMSLTI  TO  IBGRSLTI                              00218100
218200                            ACWA-DISPLAY-LEN-7                    00218200
218300         SET  WT-01-INDEX                     TO +01              00218300
218400         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00218400
218500         MOVE -1        TO  IBGROPTL                              00218500
218600         MOVE DFHBMABF  TO  IBGRSLTA                              00218600
218700                            IBGRIDA                               00218700
218800         MOVE SPACES    TO  IBGROPTI                              00218800
218900         MOVE DFHBMUNP  TO  MFRMSLTA                              00218900
219000         MOVE ZEROS     TO  MFRMSLTI                              00219000
219100                            MFRMSLTL                              00219100
219200         MOVE ACWA-DISPLAY-LEN-7                                  00219200
219300                        TO  GAB-COINS-INTL-TAB-SLOT-1 (GAB-INDEX) 00219300
219400     ELSE                                                         00219400
219500         NEXT SENTENCE.                                           00219500
219600                                                                  00219600
219700     IF  FRMNUIDI =  'GTM1'  AND                                  00219700
219800         IPGNOPTI =  'MT'                                         00219800
219900     THEN                                                         00219900
220000         MOVE MFRMSLTI  TO  IPGNSLTI                              00220000
220100                            ACWA-DISPLAY-LEN-7                    00220100
220200         SET  WT-01-INDEX                     TO +02              00220200
220300         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00220300
220400         MOVE -1        TO  IPGNOPTL                              00220400
220500         MOVE DFHBMABF  TO  IPGNSLTA                              00220500
220600                            IPGNIDA                               00220600
220700         MOVE SPACES    TO  IPGNOPTI                              00220700
220800         MOVE DFHBMUNP  TO  MFRMSLTA                              00220800
220900         MOVE ZEROS     TO  MFRMSLTI                              00220900
221000                            MFRMSLTL                              00221000
221100         MOVE ACWA-DISPLAY-LEN-7                                  00221100
221200                        TO  GAB-COINS-INTL-TAB-SLOT-1 (GAB-INDEX) 00221200
221300     ELSE                                                         00221300
221400         NEXT SENTENCE.                                           00221400
221500                                                                  00221500
221600     IF  FRMNUIDI =  'GTM1'  AND                                  00221600
221700         IPGTOPTI =  'MT'                                         00221700
221800     THEN                                                         00221800
221900         MOVE MFRMSLTI  TO  IPGTSLTI                              00221900
222000                            ACWA-DISPLAY-LEN-7                    00222000
222100         SET  WT-01-INDEX                     TO +03              00222100
222200         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00222200
222300         MOVE -1        TO  IPGTOPTL                              00222300
222400         MOVE DFHBMABF  TO  IPGTSLTA                              00222400
222500                            IPGTIDA                               00222500
222600         MOVE SPACES    TO  IPGTOPTI                              00222600
222700         MOVE DFHBMUNP  TO  MFRMSLTA                              00222700
222800         MOVE ZEROS     TO  MFRMSLTI                              00222800
222900                            MFRMSLTL                              00222900
223000         MOVE ACWA-DISPLAY-LEN-7                                  00223000
223100                        TO  GAB-COINS-INTL-TAB-SLOT-1 (GAB-INDEX) 00223100
223200     ELSE                                                         00223200
223300         NEXT SENTENCE.                                           00223300
223400                                                                  00223400
223500     IF  FRMNUIDI =  'GTM1'  AND                                  00223500
223600         IPGSOPTI =  'MT'                                         00223600
223700     THEN                                                         00223700
223800         MOVE MFRMSLTI  TO  IPGSSLTI                              00223800
223900                            ACWA-DISPLAY-LEN-7                    00223900
224000         SET  WT-01-INDEX                     TO +03              00224000
224100         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00224100
224200         MOVE -1        TO  IPGSOPTL                              00224200
224300         MOVE DFHBMABF  TO  IPGSSLTA                              00224300
224400                            IPGSIDA                               00224400
224500         MOVE SPACES    TO  IPGSOPTI                              00224500
224600         MOVE DFHBMUNP  TO  MFRMSLTA                              00224600
224700         MOVE ZEROS     TO  MFRMSLTI                              00224700
224800                            MFRMSLTL                              00224800
224900         MOVE ACWA-DISPLAY-LEN-7                                  00224900
225000                        TO  GAB-COINS-INTL-TAB-SLOT-1 (GAB-INDEX) 00225000
225100     ELSE                                                         00225100
225200         NEXT SENTENCE.                                           00225200
225300                                                                  00225300
225400     IF  FRMNUIDI =  'GTM1'  AND                                  00225400
225500         IDGDOPTI =  'MT'                                         00225500
225600     THEN                                                         00225600
225700         MOVE MFRMSLTI  TO  IDGDSLTI                              00225700
225800                            ACWA-DISPLAY-LEN-7                    00225800
225900         SET  WT-01-INDEX                     TO +04              00225900
226000         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00226000
226100         MOVE -1        TO  IDGDOPTL                              00226100
226200         MOVE DFHBMABF  TO  IDGDSLTA                              00226200
226300                            IDGDIDA                               00226300
226400         MOVE SPACES    TO  IDGDOPTI                              00226400
226500         MOVE DFHBMUNP  TO  MFRMSLTA                              00226500
226600         MOVE ZEROS     TO  MFRMSLTI                              00226600
226700                            MFRMSLTL                              00226700
226800         MOVE ACWA-DISPLAY-LEN-7                                  00226800
226900                        TO  GAB-COINS-INTL-TAB-SLOT-1 (GAB-INDEX) 00226900
227000     ELSE                                                         00227000
227100         NEXT SENTENCE.                                           00227100
227200                                                                  00227200
227300     IF  FRMNUIDI =  'GTM1'  AND                                  00227300
227400         IPGPOPTI =  'MT'                                         00227400
227500     THEN                                                         00227500
227600         MOVE MFRMSLTI  TO  IPGPSLTI                              00227600
227700                            ACWA-DISPLAY-LEN-7                    00227700
227800         SET  WT-01-INDEX                     TO +05              00227800
227900         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00227900
228000         MOVE -1        TO  IPGPOPTL                              00228000
228100         MOVE DFHBMABF  TO  IPGPSLTA                              00228100
228200                            IPGPIDA                               00228200
228300         MOVE SPACES    TO  IPGPOPTI                              00228300
228400         MOVE DFHBMUNP  TO  MFRMSLTA                              00228400
228500         MOVE ZEROS     TO  MFRMSLTI                              00228500
228600                            MFRMSLTL                              00228600
228700         MOVE ACWA-DISPLAY-LEN-7                                  00228700
228800                        TO  GAB-COINS-INTL-TAB-SLOT-1 (GAB-INDEX) 00228800
228900     ELSE                                                         00228900
229000         NEXT SENTENCE.                                           00229000
229100                                                                  00229100
229200                                                                  00229200
229300*******                                                           00229300
229400* STS *----------------------------------------------------------*00229400
229500*******                                                           00229500
229600                                                                  00229600
229700                                                                  00229700
229800     SET  GAB-INDEX  UP BY  1.                                    00229800
229900     MOVE HIGH-VALUES     TO  GAB-ENTRY (GAB-INDEX).              00229900
230000     SET GAB-ENTRY-COUNT  TO  GAB-INDEX.                          00230000
230100                                                                  00230100
230200 2500-900-EXIT. EXIT.                                             00230200
230300/                                                                 00230300
230400 2600-000-PROCESS-VAL-LIMIT     SECTION.                          00230400
230500 2600-010.                                                        00230500
230600                                                                  00230600
230700     IF (ACWA-VAL-LIM-SCREEN-NEG1-3 = 'NEG' OR                    00230700
230800         ACWA-VAL-LIM-SCREEN-NEG2-3 = 'NEG') OR                   00230800
230700        (ACWA-VAL-LIM-SCREEN-NEG1-3 = 'UNL' OR                    00230810
230800         ACWA-VAL-LIM-SCREEN-NEG2-3 = 'UNL')                      00230820
230900         GO TO 2600-900-EXIT.                                     00230900
231000                                                                  00231000
231100     IF  ACWA-BNMXVALI-N NUMERIC                                  00231100
231200     THEN                                                         00231200
231300         IF  BENVLQLI  = '5'                                      00231300
231400         THEN                                                     00231400
231500             MOVE ACWA-BNMXVALI-N TO ACWA-VALUE-LIMIT-7           00231500
231600             MOVE ZEROS           TO ACWA-VALUE-LIMIT-2           00231600
231700             GO TO 2600-900-EXIT                                  00231700
231800         ELSE                                                     00231800
231900             MOVE ACWA-BNMXVALI-N TO ACWA-VALUE-LIMIT-9-9         00231900
232000             GO TO 2600-900-EXIT                                  00232000
232100     ELSE                                                         00232100
232200         NEXT SENTENCE.                                           00232200
232300                                                                  00232300
232400     IF  ACWA-VAL-LIM-SCREEN-1 = '.'                              00232400
232500         MOVE ACWA-VAL-LIM-SCREEN-7 TO ACWA-VALUE-LIMIT-7         00232500
232600         MOVE ACWA-VAL-LIM-SCREEN-2 TO ACWA-VALUE-LIMIT-2         00232600
232700         GO TO 2600-900-EXIT.                                     00232700
232800                                                                  00232800
232900 2600-900-EXIT. EXIT.                                             00232900
233000/*****************************************************************00233000
233100* 3000 UPDATE GAB RECORD                                         *00233100
233200*                                                                *00233200
233300*    THIS ROUTINE REWRITES THE UPDATED RECORD TO THE WORK FILE.  *00233300
233400******************************************************************00233400
233500 3000-000-UPDATE-GAB-RECORD     SECTION.                          00233500
233600 3000-010.                                                        00233600
233700                                                                  00233700
233800     COMPUTE  GCIO-RECORD-LENGTH   =  GC-WORKFILE-KEY-LEN  +      00233800
233900         GC-GCTABULR-ACL-FIXED-LEN +                              00233900
234000         (GC-GCTABULR-ACL-VARY-LEN * GAB-ENTRY-COUNT).            00234000
234100                                                                  00234100
234200     MOVE  GC-GCIO-ACCESS-CODE-WU  TO  GCIO-FILE-ACCESS-CODE.     00234200
234300                                                                  00234300
234400     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00234400
234500                COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           00234500
234600                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)  END-EXEC.     00234600
234700                                                                  00234700
234800     IF NOT GCIO-GOOD-RETURN                                      00234800
234900        MOVE WS-ABCODE-1CF4        TO WS-ABCODE                   00234900
235000        MOVE WS-ABCODE-1CF4-MSG    TO WS-ABCODE-MSG               00235000
235100        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00235100
235200                                                                  00235200
235300 3000-900-EXIT. EXIT.                                             00235300
235400                                                                  00235400
235500/*****************************************************************00235500
235600* 3100  READ RECORD                                              *00235600
235700*                                                                *00235700
235800*    THIS ROUTINE READS THE RECORD THAT CORRESPONDS TO THE KEY   *00235800
235900*  FIELDS FOUND ON THE SCREEN'S HEADING.                         *00235900
236000******************************************************************00236000
236100 3100-000-READ-RECORD           SECTION.                          00236100
236200 3100-010.                                                        00236200
236300                                                                  00236300
236400     COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =  GC-GCIOPARM-LEN  +   00236400
236500              GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-ACL-FIXED-LEN  +00236500
236600             (GC-GCTABULR-ACL-VARY-LEN  *                         00236600
236700                                   GC-GCTABULR-ACL-VARY-MAX-OCUR).00236700
236800                                                                  00236800
236900     IF ACWA-WF-ALL-LEVEL-TAB-COMP  >  ZERO                       00236900
237000        NEXT SENTENCE                                             00237000
237100     ELSE                                                         00237100
237200        EXEC CICS GETMAIN                                         00237200
237300               SET(ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD)      00237300
237400               INITIMG(WS-HEX-00)                                 00237400
237500               LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                 00237500
237600               END-EXEC                                           00237600
237700        SET ACWA-WF-ALL-LEVEL-TAB-PNTR     TO                     00237700
237800                 ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD.        00237800
237900                                                                  00237900
238000                                                                  00238000
238100     IF FRMNUIDI  =  'GS3A'                                       00238100
238200        PERFORM 6000-000-BUILD-GROUP-SPEC-KEY.                    00238200
238300     IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            00238300
238400        PERFORM 6100-000-BUILD-CONTRACT-KEY.                      00238400
238500     IF FRMNUIDI  =  'GC8A'                                       00238500
238600        PERFORM 6200-000-BUILD-BEN-PROV-KEY.                      00238600
238700                                                                  00238700
238800     IF GCIO-WORKFILE-KEY  =  WORK-RECORD-KEY                     00238800
238900        GO TO 3100-900-EXIT.                                      00238900
239000                                                                  00239000
239100     MOVE GC-GCPSWORK-DDNAME     TO GCIO-FILE-DDNAME.             00239100
239200     MOVE GC-GCIO-AREA-1         TO GCIO-IO-AREA-TO-USE.          00239200
239300     MOVE GCIO-WORKFILE-KEY      TO GCIO-FILE-KEY.                00239300
239400     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO-FILE-ACCESS-CODE.        00239400
239500     MOVE GC-GCTABULR-ACL-VARY-MAX-OCUR  TO  GAB-ENTRY-COUNT.     00239500
239600                                                                  00239600
239700     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00239700
239800                COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           00239800
239900                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)     END-EXEC.  00239900
240000                                                                  00240000
240100     IF NOT GCIO-GOOD-RETURN                                      00240100
240200        MOVE WS-ABCODE-1CFJ        TO WS-ABCODE                   00240200
240300        MOVE WS-ABCODE-1CFJ-MSG    TO WS-ABCODE-MSG               00240300
240400        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00240400
240500                                                                  00240500
240600 3100-900-EXIT. EXIT.                                             00240600
240700                                                                  00240700
240800/*****************************************************************00240800
240900* 3200  READ REC FOR UPDATE                                      *00240900
241000*                                                                *00241000
241100*    THIS ROUTINE READS THE RECORD FOR UPDATE THAT CORRESPONDS   *00241100
241200*  TO THE KEY FIELDS FOUND ON THE SCREEN'S HEADING.              *00241200
241300******************************************************************00241300
241400 3200-000-READ-REC-FOR-UPDATE   SECTION.                          00241400
241500 3200-010.                                                        00241500
241600                                                                  00241600
241700     COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =  GC-GCIOPARM-LEN  +   00241700
241800              GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-ACL-FIXED-LEN  +00241800
241900             (GC-GCTABULR-ACL-VARY-LEN  *                         00241900
242000                                   GC-GCTABULR-ACL-VARY-MAX-OCUR).00242000
242100                                                                  00242100
242200                                                                  00242200
242300     IF ACWA-WF-ALL-LEVEL-TAB-COMP  >  ZERO                       00242300
242400        NEXT SENTENCE                                             00242400
242500     ELSE                                                         00242500
242600        EXEC CICS GETMAIN                                         00242600
242700               SET(ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD)      00242700
242800               INITIMG(WS-HEX-00)                                 00242800
242900               LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                 00242900
243000               END-EXEC                                           00243000
243100        SET ACWA-WF-ALL-LEVEL-TAB-PNTR     TO                     00243100
243200                 ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD.        00243200
243300                                                                  00243300
243400     IF FRMNUIDI  =  'GS3A'                                       00243400
243500        PERFORM 6000-000-BUILD-GROUP-SPEC-KEY.                    00243500
243600     IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            00243600
243700        PERFORM 6100-000-BUILD-CONTRACT-KEY.                      00243700
243800     IF FRMNUIDI  =  'GC8A'                                       00243800
243900        PERFORM 6200-000-BUILD-BEN-PROV-KEY.                      00243900
244000                                                                  00244000
244100     MOVE GC-GCPSWORK-DDNAME     TO GCIO-FILE-DDNAME.             00244100
244200     MOVE GC-GCIO-AREA-1         TO GCIO-IO-AREA-TO-USE.          00244200
244300     MOVE GCIO-WORKFILE-KEY      TO GCIO-FILE-KEY.                00244300
244400     MOVE GC-GCIO-ACCESS-CODE-RU TO GCIO-FILE-ACCESS-CODE.        00244400
244500     MOVE GC-GCTABULR-ACL-VARY-MAX-OCUR  TO  GAB-ENTRY-COUNT.     00244500
244600                                                                  00244600
244700     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00244700
244800                COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           00244800
244900                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)  END-EXEC.     00244900
245000                                                                  00245000
245100 3200-900-EXIT. EXIT.                                             00245100
245200/*****************************************************************00245200
245300* 4000  DISPLAY FIRST SCREEN                                     *00245300
245400*                                                                *00245400
245500*    THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM ONE OF THE*00245500
245600*  TABULAR MENUS, THE MENU WILL READ THE ALL LEVEL TABULAR IF IT *00245600
245700*  EXISTS (IF IT DOESN'T EXIST THE MENU WILL ADD A NEW ONE TO THE*00245700
245800*  WORK FILE) THEN PLACE THE ADDRESS OF THE TABULAR RECORD WITHIN*00245800
245900*  A COMMON AREA PARAMETER LIST.  THE MENU THEN MOVES THE KEY    *00245900
246000*  FIELDS TO THE COMMON AREA AND PASSES THE ADDRESS OF THE       *00246000
246100*  PARAMETER LIST IN A FULLWORD TO THIS PROGRAM.                 *00246100
246200*    WE THEN SET THIS ADDRESS INTO A BLL CELL AND ACCESS THE     *00246200
246300*  INFORMATION NEEDED TO BUILD THE SCREEN IMAGE.                 *00246300
246400******************************************************************00246400
246500 4000-000-DISPLAY-FIRST-SCREEN  SECTION.                          00246500
246600 4000-010.                                                        00246600
246700                                                                  00246700
246800     IF EIBCALEN  >  0                                            00246800
246900        NEXT SENTENCE                                             00246900
247000     ELSE                                                         00247000
247100        MOVE WS-ABCODE-1CC1        TO WS-ABCODE                   00247100
247200        MOVE WS-ABCODE-1CC1-MSG    TO WS-ABCODE-MSG               00247200
247300        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00247300
247400                                                                  00247400
247500*    SET ADDRESS OF COMMUNICATION-KEY-AREA  TO                    00247500
247600*                   COMMAREA-RECORD-POINTER.                      00247600
247700                                                                  00247700
247800*    SET  ACWA-COMM-KEY-PNTR       TO                             00247800
247900*                   ADDRESS OF COMMUNICATION-KEY-AREA.            00247900
248000     MOVE LOW-VALUES               TO GA1XI01I.                   00248000
248100                                                                  00248100
248200     MOVE GCA-FROM-MENU-ID  TO  FRMNUIDO.                         00248200
248300                                                                  00248300
248400     IF GCA-FROM-MENU-ID  =  'GS3A'                               00248400
248500        MOVE GCA-PLAN-CODE             TO GRP-SPEC-PLAN-CODE      00248500
248600        MOVE GCA-GROUP-NUM             TO GRP-SPEC-GROUP-NUM      00248600
248700        MOVE GCA-SECTION-NUM           TO GRP-SPEC-SECTION-NUM    00248700
248800        MOVE GCA-PKG-CODE              TO GRP-SPEC-PKG-CODE       00248800
248900        MOVE GCA-FAM-REL-LVL           TO GRP-SPEC-FAM-REL-LVL    00248900
249000        MOVE GCA-EFFECTIVE-DATE        TO GRP-SPEC-EFF-DATE       00249000
249100        MOVE ' GROUP SPEC. ALL-LEVEL TABULAR MAINTENANCE'         00249100
249200                                       TO TTLELNEO                00249200
249300*AB*****MOVE GROUP-SPECIFIC-TITLE-LINE TO TTLELNEO                00249300
249400        MOVE GROUP-SPECIFIC-ID-LINE    TO IDLINEO.                00249400
249500                                                                  00249500
249600     IF GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                     00249600
249700        MOVE GCA-PLAN-CODE             TO CONTRACT-PLAN-CODE      00249700
249800        MOVE GCA-GROUP-NUM             TO CONTRACT-GROUP-NUM      00249800
249900        MOVE GCA-SECTION-NUM           TO CONTRACT-SECTION-NUM    00249900
250000        MOVE GCA-PKG-CODE              TO CONTRACT-PKG-CODE       00250000
250100        MOVE GCA-L-O-B                 TO CONTRACT-LOB            00250100
250200        MOVE GCA-PROV-CTL              TO CONTRACT-PROV-CTL       00250200
250300        MOVE GCA-FAM-REL-LVL           TO CONTRACT-FAM-REL-LVL    00250300
250400        MOVE GCA-EFFECTIVE-DATE        TO CONTRACT-EFF-DATE       00250400
250500        MOVE '   CONTRACT ALL-LEVEL TABULAR MAINTENANCE'          00250500
250600                                       TO TTLELNEO                00250600
250700*AB*****MOVE CONTRACT-TITLE-LINE       TO TTLELNEO                00250700
250800        MOVE CONTRACT-ID-LINE          TO IDLINEO.                00250800
250900                                                                  00250900
251000     IF GCA-FROM-MENU-ID  =  'GC8A'                               00251000
251100        MOVE GCA-PLAN-CODE             TO BEN-PROV-PLAN-CODE      00251100
251200        MOVE GCA-GROUP-NUM             TO BEN-PROV-GROUP-NO       00251200
251300        MOVE GCA-SECTION-NUM           TO BEN-PROV-SECTION-NO     00251300
251400        MOVE GCA-PKG-CODE              TO BEN-PROV-PKG-CODE       00251400
251500        MOVE GCA-L-O-B                 TO BEN-PROV-LOB            00251500
251600        MOVE GCA-PROV-CTL              TO BEN-PROV-PROV-CTL       00251600
251700        MOVE GCA-FAM-REL-LVL           TO BEN-PROV-FAM-REL-LVL    00251700
251800        MOVE GCA-EFFECTIVE-DATE        TO BEN-PROV-EFF-DATE       00251800
251900        MOVE GCA-BEN-PROV-ID           TO BEN-PROV-ID-NO          00251900
252000        MOVE '   BEN. PROV ALL-LEVEL TABULAR MAINTENANCE'         00252000
252100                                          TO  TTLELNEO            00252100
252200*AB*****MOVE BENEFIT-PROVISION-TITLE-LINE TO  TTLELNEO            00252200
252300        MOVE BENEFIT-PROVISION-ID-LINE TO IDLINEO.                00252300
252400                                                                  00252400
252500     MOVE 'GA1C'                       TO FUNCTONO.               00252500
252600     MOVE '001C00'                     TO SCRNIDNO.               00252600
252700     MOVE ACL-TITLE-LINE               TO TITLEO.                 00252700
252800                                                                  00252800
252900     MOVE GCA-RECORD-POINTER-COMP  TO ACWA-WF-ALL-LEVEL-TAB-COMP. 00252900
253000     SET ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD  TO             00253000
253100                    GCA-RECORD-POINTER.                           00253100
253200                                                                  00253200
253300     IF  NOT WRK-STAT-CONT-MAINT AND                              00253300
253400         NOT WRK-STAT-GRP-SPEC-MAINT                              00253400
253500     THEN                                                         00253500
253600         MOVE WS-ABCODE-1CC2        TO WS-ABCODE                  00253600
253700         MOVE WS-ABCODE-1CC2-MSG    TO WS-ABCODE-MSG              00253700
253800         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                   00253800
253900                                                                  00253900
254000     IF  NOT WRK-REC-GROUP-SPEC-TAB AND                           00254000
254100         NOT WRK-REC-CONT-TAB       AND                           00254100
254200         NOT WRK-REC-CONT-BEN-TAB-PROV                            00254200
254300     THEN                                                         00254300
254400         MOVE WS-ABCODE-1CC3        TO WS-ABCODE                  00254400
254500         MOVE WS-ABCODE-1CC3-MSG    TO WS-ABCODE-MSG              00254500
254600         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                   00254600
254700                                                                  00254700
254800     MOVE GAB-ENTRY-COUNT         TO  GAB-ENTRY-COUNT.            00254800
254900     MOVE GCA-ALL-LEVEL-TAB-ID    TO  TABIDO.                     00254900
255000     MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  TABSLTNO.                   00255000
255100     SET GAB-INDEX  TO  1.                                        00255100
255200                                                                  00255200
255300     IF EIBTRNID  =  'GC4A' OR  'GC3A' OR  'GC8A'  OR 'GTM1'      00255300
255400        MOVE '0000000'  TO GCA-OCCURS-ENTRY-COUNTER.              00255400
255500                                                                  00255500
255600     IF GCA-OCCURS-ENTRY-COUNTER  =  '0000000'                    00255600
255700        GO TO 4000-200-BUILD-SCREEN.                              00255700
255800                                                                  00255800
255900     MOVE GCA-OCCURS-ENTRY-COUNTER  TO  ACWA-DISPLAY-LEN-7.       00255900
256000                                                                  00256000
256100 4000-100-FIND-RIGHT-OCCURS.                                      00256100
256200     IF GAB-COINS-BENEFIT-PERIOD(GAB-INDEX) NOT = HIGH-VALUES AND 00256200
256300        GAB-OCCURS-ENTRY-COUNTER(GAB-INDEX) NOT =                 00256300
256400                                                ACWA-DISPLAY-LEN-700256400
256500     THEN                                                         00256500
256600         IF GAB-INDEX  <  GAB-ENTRY-COUNT                         00256600
256700            SET GAB-INDEX  UP BY  1                               00256700
256800            GO TO 4000-100-FIND-RIGHT-OCCURS                      00256800
256900         ELSE                                                     00256900
257000             MOVE WS-ABCODE-1CL4        TO WS-ABCODE              00257000
257100             MOVE WS-ABCODE-1CL4-MSG    TO WS-ABCODE-MSG          00257100
257200             MOVE -1                    TO MFRMSLTL               00257200
257300             PERFORM 9800-000-ERROR-MSG-THEN-ABEND                00257300
257400     ELSE                                                         00257400
257500         NEXT SENTENCE.                                           00257500
257600                                                                  00257600
257700                                                                  00257700
257800 4000-200-BUILD-SCREEN.                                           00257800
257900                                                                  00257900
258000     IF  GCA-ADD-DEL-IND  =  'A' OR                               00258000
258100         GAB-ENTRY-COUNT  =  1                                    00258100
258200     THEN                                                         00258200
258300         MOVE 'CHG/ADD'  TO  DELADDO                              00258300
258400         MOVE DFHBMASD   TO  DLOPTLTA,  DELOPTNA                  00258400
258500         IF  GCA-OCCURS-ENTRY-COUNTER  =  '0000000'               00258500
258600         THEN                                                     00258600
258700             PERFORM 4100-000-DISPLAY-SKELETON                    00258700
258800         ELSE                                                     00258800
258900             PERFORM 4400-000-BUILD-DISPLAY                       00258900
259000     ELSE                                                         00259000
259100         MOVE 'CHG/DEL'  TO  DELADDO                              00259100
259200         MOVE 'D'        TO  DELOLITO                             00259200
259300         PERFORM 4400-000-BUILD-DISPLAY.                          00259300
259400                                                                  00259400
259500 4000-900-EXIT. EXIT.                                             00259500
259600/*****************************************************************00259600
259700* 4100  DISPLAY SKELETON                                         *00259700
259800*                                                                *00259800
259900*    THIS ROUTINE REINITIALIZES THE SCREEN FOR THE OPERATOR      *00259900
260000*  AFTER THEY HAVE REVIEWED THE ENTRY THEY JUST ADDED AND        *00260000
260100*  INDICATED THAT THEY WANTED TO ADD MORE BY KEYING 'ENTER'.     *00260100
260200******************************************************************00260200
260300 4100-000-DISPLAY-SKELETON      SECTION.                          00260300
260400 4100-010.                                                        00260400
260500                                                                  00260500
260600     MOVE SPACES TO ERRMSGO.                                      00260600
260700                                                                  00260700
260800     MOVE DFHBMFSE                                                00260800
260900       TO PERIODA.                                                00260900
261000                                                                  00261000
261100     MOVE DFHBMUNP                                                00261100
261200       TO BENVLQLA FAMINDIA  INTDESKA  LOBA                       00261200
261300          IBGROPTA IPGNOPTA  IPGTOPTA  MFRMSLTA                   00261300
261400          IDGDOPTA IPGPOPTA  IPGSOPTA.                            00261400
261500                                                                  00261500
261600     MOVE ALL '_'                                                 00261600
261700       TO PERIODO  BENVLQLO  LOBO      FAMINDIO  PLCTRMTO.        00261700
261800                                                                  00261800
261900     MOVE LOW-VALUES                                              00261900
262000       TO INTDESKO IBGROPTO  IPGNOPTO  IPGTOPTO  MFRMSLTO         00262000
262100          IDGDOPTO IPGPOPTO  IPGSOPTO.                            00262100
262200                                                                  00262200
262300     MOVE ZEROS                                                   00262300
262400       TO COPAYINO CSTCONTO  PERTQALO  ASCDSCDO  BISNDINO         00262400
262500          DAYFACIO SRVGRUPO  PRTIMEFO  MANAPLIO  FDLRCLIO         00262500
262600          REININDO CLMLVLIO  INTRVALO  INTTYPEO  BNMXVALO         00262600
262700          FYIVALO  OVRDINDO  NEWVALUO  DEFINTNO  PERLIMTO         00262700
262800          CONDALLO CONDEXCO  CONDICDO  CONDTABO  CONDMENO         00262800
262900          CONDEMCO CONDEACO  CONDSMIO  CONDNSMO                   00262900
263000        CONDDRGO CONDALCO  CONDOBNO  CONDOBCO  CONDMALO CONDTMJO  00263000
263100        CONDCARO CONDOBSO  CONDKDYO  CONDACCO  CONDSUIO CONDINFO  00263100
263200        PRTIMEFO INTRVALO  CONDPECO  CONDNEMO  NEWVALUO AGEQLLO   00263200
263300        OENTCTRO IBGRSLTO  IPGNSLTO  IPGTSLTO  TOCURANO AGEQLHO   00263300
263400        CONDLIFO IDGDSLTO IPGPSLTO  AGELIMLO  AGELIMHO  RELPINDO  00263400
263500        FEAKINDO CARYOVRO IPGSSLTO  ACCUMIDO  CAPINDO   SABDINDO  00263500
              BENTYPO  TIERCDO  TIERLVO.                                00263510
263600                                                                  00263600
263700     MOVE '01'    TO  COCURANO.                                   00263700
263800     MOVE -1      TO  PERIODL.                                    00263800
263900                                                                  00263900
264000     PERFORM 9000-000-SEND-ERASE-RETURN.                          00264000
264100                                                                  00264100
264200 4100-900-EXIT. EXIT.                                             00264200
264300                                                                  00264300
264400/*****************************************************************00264400
264500* 4200 DISPLAY NEXT                                              *00264500
264600*                                                                *00264600
264700*    THIS ROUTINE WILL FIND THE ENTRY CORRESPONDING TO THE       *00264700
264800*  SCREEN'S DISPLAY AND THEN POSITION TO THE NEXT ENTRY, IF THE  *00264800
264900*  NEXT ENTRY IS THE LAST IN THE LIST THE CODE WILL RECOGNIZE    *00264900
265000*  THAT AND POSITION TO THE FIRST ENTRY, ALSO DISPLAYING AN      *00265000
265100*  INFORMATIONAL MESSAGE.                                        *00265100
265200******************************************************************00265200
265300 4200-000-DISPLAY-NEXT          SECTION.                          00265300
265400 4200-010.                                                        00265400
265500                                                                  00265500
265600     MOVE GAB-ENTRY-COUNT  TO  GAB-ENTRY-COUNT.                   00265600
265700     SET GAB-INDEX         TO  1.                                 00265700
265800     MOVE OENTCTRO         TO  ACWA-DISPLAY-LEN-7.                00265800
265900                                                                  00265900
266000                                                                  00266000
266100     SET  CURNT-OCURS-BIN  TO  GAB-INDEX.                         00266100
266200     MOVE CURNT-OCURS-BIN  TO  CURNT-OCURS-PKD.                   00266200
266300     MOVE CURNT-OCCURS-OUT TO  COCURANO.                          00266300
266400                                                                  00266400
266500     IF GAB-ENTRY-COUNT > 1                                       00266500
266600        COMPUTE  TOTAL-OCURS-UNK =  GAB-ENTRY-COUNT  - 1          00266600
266700        MOVE  TOTAL-OCCURS-OUT TO TOCURANO                        00266700
266800     ELSE                                                         00266800
266900        MOVE  '01'             TO TOCURANO.                       00266900
267000                                                                  00267000
267100                                                                  00267100
267200 4200-100-FIND-RIGHT-OCCURS.                                      00267200
267300                                                                  00267300
267400     IF  GAB-COINS-BENEFIT-PERIOD(GAB-INDEX) NOT = HIGH-VALUES AND00267400
267500         GAB-OCCURS-ENTRY-COUNTER(GAB-INDEX) NOT =                00267500
267600                                                ACWA-DISPLAY-LEN-700267600
267700     THEN                                                         00267700
267800         IF  GAB-INDEX  <  (GAB-ENTRY-COUNT - 1)                  00267800
267900         THEN                                                     00267900
268000             SET GAB-INDEX  UP BY  1                              00268000
268100             GO TO 4200-100-FIND-RIGHT-OCCURS                     00268100
268200         ELSE                                                     00268200
268300             SET GAB-INDEX  TO  1                                 00268300
268400             SET  WT-01-INDEX                     TO +20          00268400
268500             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      00268500
268600     ELSE                                                         00268600
268700         IF  GAB-INDEX  <  (GAB-ENTRY-COUNT - 1)                  00268700
268800         THEN                                                     00268800
268900             SET GAB-INDEX  UP BY  1                              00268900
269000         ELSE                                                     00269000
269100             SET GAB-INDEX  TO  1                                 00269100
269200             SET  WT-01-INDEX                     TO +20          00269200
269300             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO.     00269300
269400                                                                  00269400
269500     PERFORM 4400-000-BUILD-DISPLAY.                              00269500
269600                                                                  00269600
269700 4200-900-EXIT. EXIT.                                             00269700
269800/*****************************************************************00269800
269900* 4300 DISPLAY PREV                                              *00269900
270000*                                                                *00270000
270100*    THIS ROUTINE WILL FIND THE ENTRY CORRESPONDING TO THE       *00270100
270200*  SCREEN'S DISPLAY AND THEN POSITION TO THE NEXT PREVIOUS ENTRY,*00270200
270300*  IF THE CURRENT ENTRY IS THE FIRST IN THE LIST THE CODE WILL   *00270300
270400*  RECOGNIZE THAT AND POSITION TO THE LAST ENTRY, ALSO DISPLAYING*00270400
270500*  AN INFORMATIONAL MESSAGE.                                     *00270500
270600******************************************************************00270600
270700 4300-000-DISPLAY-PREV          SECTION.                          00270700
270800 4300-010.                                                        00270800
270900                                                                  00270900
271000     MOVE GAB-ENTRY-COUNT  TO  GAB-ENTRY-COUNT.                   00271000
271100     SET  GAB-INDEX        TO  1.                                 00271100
271200     MOVE OENTCTRO         TO  ACWA-DISPLAY-LEN-7.                00271200
271300                                                                  00271300
271400 4300-100-FIND-RIGHT-OCCURS.                                      00271400
271500                                                                  00271500
271600     IF  GAB-COINS-BENEFIT-PERIOD(GAB-INDEX) NOT = HIGH-VALUES AND00271600
271700         GAB-OCCURS-ENTRY-COUNTER(GAB-INDEX) NOT =                00271700
271800                                                ACWA-DISPLAY-LEN-700271800
271900     THEN                                                         00271900
272000         IF  GAB-INDEX  <  (GAB-ENTRY-COUNT - 1)                  00272000
272100         THEN                                                     00272100
272200             SET GAB-INDEX  UP BY  1                              00272200
272300             GO TO 4300-100-FIND-RIGHT-OCCURS                     00272300
272400         ELSE                                                     00272400
272500             SET GAB-INDEX  TO  1                                 00272500
272600             SET  WT-01-INDEX                     TO +20          00272600
272700             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      00272700
272800     ELSE                                                         00272800
272900         IF  GAB-INDEX  NOT =  1                                  00272900
273000         THEN                                                     00273000
273100             SET GAB-INDEX  DOWN BY  1                            00273100
273200         ELSE                                                     00273200
273300             SET GAB-INDEX  TO  GAB-ENTRY-COUNT                   00273300
273400             SET GAB-INDEX  DOWN BY  1                            00273400
273500             SET  WT-01-INDEX                     TO +21          00273500
273600             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO.     00273600
273700                                                                  00273700
273800     PERFORM 4400-000-BUILD-DISPLAY.                              00273800
273900                                                                  00273900
274000 4300-900-EXIT. EXIT.                                             00274000
274100/*****************************************************************00274100
274200* 4400 BUILD DISPLAY                                             *00274200
274300*                                                                *00274300
274400*    THIS ROUTINE WILL MOVE ALL THE FIELDS FROM THE OCCURENCE    *00274400
274500*  SPECIFIED BY INDEX GAB-INDEX TO THE SCREEN.                   *00274500
274600******************************************************************00274600
274700 4400-000-BUILD-DISPLAY         SECTION.                          00274700
274800 4400-010.                                                        00274800
274900                                                                  00274900
275000     IF  DELADDI  =  'CHG/DEL'                                    00275000
275100     THEN                                                         00275100
275200         MOVE 'D'  TO  DELOLITO                                   00275200
275300     ELSE                                                         00275300
275400         MOVE SPACE  TO  DELOLITO.                                00275400
275500                                                                  00275500
275600     MOVE SPACE                                       TO DELOPTNO.00275600
275700     MOVE GAB-OCCURS-ENTRY-COUNTER       (GAB-INDEX)  TO          00275700
275800                                               ACWA-DISPLAY-LEN-7.00275800
275900     MOVE ACWA-DISPLAY-LEN-7                          TO OENTCTRO.00275900
276000     MOVE GAB-COINS-DAY-FACTOR-IND       (GAB-INDEX)  TO DAYFACIO.00276000
276100     MOVE GAB-COINS-CO-PAY-IND           (GAB-INDEX)  TO COPAYINO.00276100
276200     MOVE GAB-COINS-BISCENDING-IND       (GAB-INDEX)  TO BISNDINO.00276200
276300     MOVE GAB-COINS-ASCEND-DESCEND-IND   (GAB-INDEX)  TO ASCDSCDO.00276300
      *P21595 CHANGES STARTS                                            00276310
           MOVE GAB-COINS-BEN-TYPE             (GAB-INDEX)  TO BENTYPO. 00276320
           MOVE GAB-COINS-TIER-CODE            (GAB-INDEX)  TO TIERCDO. 00276330
           MOVE GAB-COINS-TIER-LVL             (GAB-INDEX)  TO TIERLVO. 00276340
      *P21595 CHANGES ENDS                                              00276341
276400     MOVE GAB-COINS-DEFINITION           (GAB-INDEX)  TO DEFINTNO.00276400
276500     MOVE GAB-COINS-COST-CONTAIN-IND     (GAB-INDEX)  TO CSTCONTO.00276500
276600     MOVE GAB-COINS-BENEFIT-PERIOD       (GAB-INDEX)  TO PERIODO. 00276600
276700     MOVE GAB-COINS-BEN-PER-TIME-QUAL    (GAB-INDEX)  TO PERTQALO.00276700
276800     MOVE GAB-COINS-FAM-OR-INDIV         (GAB-INDEX)  TO FAMINDIO.00276800
276900     MOVE GAB-COINS-PLACE-OF-TREATMENT   (GAB-INDEX)  TO PLCTRMTO.00276900
277000     MOVE GAB-COINS-SERVICE-GROUP        (GAB-INDEX)  TO SRVGRUPO.00277000
277100     MOVE GAB-COINS-BEN-PER-TIME-FCTR    (GAB-INDEX)  TO          00277100
277200                                               ACWA-DISPLAY-LEN-3.00277200
277300     MOVE ACWA-DISPLAY-LEN-3                          TO PRTIMEFO.00277300
277400     MOVE GAB-COINS-REINSTATEMENT-IND    (GAB-INDEX)  TO REININDO.00277400
277500     MOVE GAB-COINS-LMT-MANDATORY-IND    (GAB-INDEX)  TO MANAPLIO.00277500
277600     MOVE GAB-COINS-1ST-DOLR-COVRGE-LMT  (GAB-INDEX)  TO FDLRCLIO.00277600
277700     MOVE GAB-COINS-CLAIM-LVL-ACCUM-IND  (GAB-INDEX)  TO CLMLVLIO.00277700
277800     MOVE GAB-COINS-AGE-LIMIT-FROM       (GAB-INDEX)  TO          00277800
277900                                               ACWA-DISPLAY-LEN-3.00277900
278000     MOVE ACWA-DISPLAY-LEN-3                          TO AGELIMLO.00278000
278100     MOVE GAB-COINS-AGE-LIMIT-TO         (GAB-INDEX)  TO          00278100
278200                                               ACWA-DISPLAY-LEN-3.00278200
278300     MOVE ACWA-DISPLAY-LEN-3                          TO AGELIMHO.00278300
278400     MOVE GAB-COINS-FEAK-IND             (GAB-INDEX)  TO FEAKINDO.00278400
278500     MOVE GAB-COINS-ACCUMID              (GAB-INDEX)  TO ACCUMIDO.00278500
278600     MOVE GAB-COINS-COMB-APPLIED-IND     (GAB-INDEX)  TO CAPINDO. 00278600
278700     MOVE GAB-COINS-SEL-ADDL-BEN-DET     (GAB-INDEX)  TO SABDINDO.00278700
278800     MOVE GAB-COINS-AGE-QUAL-IND-FROM    (GAB-INDEX)  TO AGEQLLO. 00278800
278900     MOVE GAB-COINS-AGE-QUAL-IND-TO      (GAB-INDEX)  TO AGEQLHO. 00278900
279000     MOVE GAB-COINS-RELATIONSHIP-IND     (GAB-INDEX)  TO RELPINDO.00279000
279100                                                                  00279100
279200     MOVE GAB-COINS-INTERVAL-TIME-FCTR   (GAB-INDEX)  TO          00279200
279300                                               ACWA-DISPLAY-LEN-3.00279300
279400     MOVE ACWA-DISPLAY-LEN-3                          TO INTRVALO.00279400
279500     MOVE GAB-COINS-INTERVAL-TYPE        (GAB-INDEX)  TO INTTYPEO.00279500
279600     MOVE GAB-COINS-L-O-B                (GAB-INDEX)  TO LOBO.    00279600
279700     MOVE GAB-COINS-VALUE-LIMIT          (GAB-INDEX)  TO          00279700
279800                                               ACWA-VALUE-LIMIT-9.00279800
279900     IF  ACWA-VALUE-LIMIT-9-9 = -1                                00279900
280000     THEN                                                         00280000
280100         MOVE 'NEG' TO BNMXVALO                                   00280100
           ELSE                                                         00280110
279900     IF  ACWA-VALUE-LIMIT-9-9 = -2                                00280120
280000     THEN                                                         00280130
280100         MOVE 'UNL' TO BNMXVALO                                   00280140
280200     ELSE                                                         00280200
280300         IF  GAB-COINS-VALUE-QUALIFIER (GAB-INDEX) = '5'          00280300
280400         THEN                                                     00280400
280500             MOVE ACWA-VALUE-LIMIT-9    TO ACWA-EDIT-VALUE-LIMIT  00280500
280600             MOVE ACWA-EDIT-VALUE-LIMIT TO BNMXVALO               00280600
280700         ELSE                                                     00280700
280800             MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX) TO            00280800
280900                                              ACWA-DISPLAY-LEN-9-200280900
281000             MOVE ACWA-DISPLAY-LEN-9-X     TO ACWA-DISPLAY-9      00281000
281100             MOVE SPACES                   TO ACWA-DISPLAY-1      00281100
281200             MOVE ACWA-DISPLAY-VALUE-LIMIT TO BNMXVALO.           00281200
281300                                                                  00281300
281400     MOVE GAB-COINS-PERCENT-LEVEL        (GAB-INDEX)  TO          00281400
281500                                               ACWA-DISPLAY-LEN-3.00281500
281600     MOVE ACWA-DISPLAY-LEN-3                          TO PERLIMTO.00281600
281700     MOVE GAB-COINS-VALUE-QUALIFIER      (GAB-INDEX)  TO BENVLQLO.00281700
281800     MOVE GAB-COINS-INTERVAL-OVRD-VALUE  (GAB-INDEX)  TO          00281800
281900                                               ACWA-DISPLAY-LEN-5.00281900
282000     MOVE ACWA-DISPLAY-LEN-5                          TO NEWVALUO.00282000
282100     MOVE GAB-COINS-INTERVAL-OVRD-IND    (GAB-INDEX)  TO OVRDINDO.00282100
282200     MOVE GAB-COINS-INTERNAL-DESCRIPTOR  (GAB-INDEX)  TO INTDESKO.00282200
282300     MOVE GAB-COND-ALL-BIT               (GAB-INDEX)  TO CONDALLO.00282300
282400     MOVE GAB-COND-EXCLUSION-BIT         (GAB-INDEX)  TO CONDEXCO.00282400
282500     MOVE GAB-COND-ICD-BIT               (GAB-INDEX)  TO CONDICDO.00282500
282600     MOVE GAB-COND-TB-BIT                (GAB-INDEX)  TO CONDTABO.00282600
282700     MOVE GAB-COND-MENTAL-BIT            (GAB-INDEX)  TO CONDMENO.00282700
282800     MOVE GAB-COND-DRUG-BIT              (GAB-INDEX)  TO CONDDRGO.00282800
282900     MOVE GAB-COND-ALCOHOL-BIT           (GAB-INDEX)  TO CONDALCO.00282900
283000     MOVE GAB-COND-OB-COMP-BIT           (GAB-INDEX)  TO CONDOBCO.00283000
283100     MOVE GAB-COND-OB-NORM-BIT           (GAB-INDEX)  TO CONDOBNO.00283100
283200     MOVE GAB-COND-MALIGNANCY-BIT        (GAB-INDEX)  TO CONDMALO.00283200
283300     MOVE GAB-COND-CARDIAC-DISEASE-BIT   (GAB-INDEX)  TO CONDCARO.00283300
283400     MOVE GAB-COND-OBESITY-BIT           (GAB-INDEX)  TO CONDOBSO.00283400
283500     MOVE GAB-COND-KIDNEY-DISEASE-BIT    (GAB-INDEX)  TO CONDKDYO.00283500
283600     MOVE GAB-COND-ACCIDENT-BIT          (GAB-INDEX)  TO CONDACCO.00283600
283700     MOVE GAB-COND-PRE-EXIST-BIT         (GAB-INDEX)  TO CONDPECO.00283700
283800     MOVE GAB-COND-NON-EMER-BIT          (GAB-INDEX)  TO CONDNEMO.00283800
283900     MOVE GAB-COND-SUICIDE-BIT           (GAB-INDEX)  TO CONDSUIO.00283900
284000     MOVE GAB-COND-TMJ-BIT               (GAB-INDEX)  TO CONDTMJO.00284000
284100     MOVE GAB-COND-INF-BIT               (GAB-INDEX)  TO CONDINFO.00284100
284200     MOVE GAB-COND-LIFE-THREAT-BIT       (GAB-INDEX)  TO CONDLIFO.00284200
284300     MOVE GAB-COND-EMER-MED-BIT          (GAB-INDEX)  TO CONDEMCO.00284300
284400     MOVE GAB-COND-EMER-ACC-BIT          (GAB-INDEX)  TO CONDEACO.00284400
284500     MOVE GAB-COND-SER-MEN-ILL-BIT       (GAB-INDEX)  TO CONDSMIO.00284500
284600     MOVE GAB-COND-NON-SER-MEN-ILL-BIT   (GAB-INDEX)  TO CONDNSMO.00284600
284700     MOVE GAB-CARRY-OVER-CREDIT-IND      (GAB-INDEX)  TO CARYOVRO.00284700
284800                                                                  00284800
284900     MOVE -1  TO PERIODL.                                         00284900
285000                                                                  00285000
285100     MOVE GAB-COINS-FYI-VALUE (GAB-INDEX) TO FYIVALO.             00285100
285200     SET  CURNT-OCURS-BIN                TO GAB-INDEX.            00285200
285300     MOVE CURNT-OCURS-BIN                TO CURNT-OCURS-PKD.      00285300
285400     MOVE CURNT-OCCURS-OUT               TO COCURANO.             00285400
285500                                                                  00285500
285600     IF GAB-ENTRY-COUNT > 1                                       00285600
285700     THEN                                                         00285700
285800         COMPUTE  TOTAL-OCURS-UNK = GAB-ENTRY-COUNT  - 1          00285800
285900         MOVE  TOTAL-OCCURS-OUT TO  TOCURANO                      00285900
286000     ELSE                                                         00286000
286100         MOVE  '01'             TO  TOCURANO.                     00286100
286200                                                                  00286200
286300                                                                  00286300
286400     MOVE ZEROS   TO  IBGRSLTO,  IPGNSLTO,  IPGTSLTO              00286400
286500                      IDGDSLTO,  IPGPSLTO,  IPGSSLTO.             00286500
286600                                                                  00286600
286700     SET GAB-INT-INDEX TO      1.                                 00286700
286800     SET GAB-INT-INDEX DOWN BY 1.                                 00286800
286900                                                                  00286900
287000 4400-300-DISPLAY-LOOP.                                           00287000
287100                                                                  00287100
287200     SET GAB-INT-INDEX UP BY 1.                                   00287200
287300     IF  GAB-INT-INDEX >  5                                       00287300
287400         GO TO 4400-800-SEND.                                     00287400
287500                                                                  00287500
287600     IF  GAB-INT-ID (GAB-INDEX GAB-INT-INDEX) = HIGH-VALUES       00287600
287700         GO TO 4400-800-SEND.                                     00287700
287800                                                                  00287800
287900     IF  GAB-INT-ID (GAB-INDEX GAB-INT-INDEX)       = '#IBGR '    00287900
288000         MOVE GAB-INT-SLOT (GAB-INDEX GAB-INT-INDEX)              00288000
288100                                TO ACWA-DISPLAY-LEN-7             00288100
288200         MOVE ACWA-DISPLAY-LEN-7 TO IBGRSLTO                      00288200
288300         GO TO 4400-300-DISPLAY-LOOP.                             00288300
288400                                                                  00288400
288500     IF  GAB-INT-ID (GAB-INDEX GAB-INT-INDEX)       = '#IDGD '    00288500
288600         MOVE GAB-INT-SLOT (GAB-INDEX GAB-INT-INDEX)              00288600
288700                                TO ACWA-DISPLAY-LEN-7             00288700
288800         MOVE ACWA-DISPLAY-LEN-7 TO IDGDSLTO                      00288800
288900         GO TO 4400-300-DISPLAY-LOOP.                             00288900
289000                                                                  00289000
289100     IF  GAB-INT-ID (GAB-INDEX GAB-INT-INDEX)       = '#IPGN '    00289100
289200         MOVE GAB-INT-SLOT (GAB-INDEX GAB-INT-INDEX)              00289200
289300                                TO ACWA-DISPLAY-LEN-7             00289300
289400         MOVE ACWA-DISPLAY-LEN-7 TO IPGNSLTO                      00289400
289500         GO TO 4400-300-DISPLAY-LOOP.                             00289500
289600                                                                  00289600
289700     IF  GAB-INT-ID (GAB-INDEX GAB-INT-INDEX)       = '#IPGP '    00289700
289800         MOVE GAB-INT-SLOT (GAB-INDEX GAB-INT-INDEX)              00289800
289900                                TO ACWA-DISPLAY-LEN-7             00289900
290000         MOVE ACWA-DISPLAY-LEN-7 TO IPGPSLTO                      00290000
290100         GO TO 4400-300-DISPLAY-LOOP.                             00290100
290200                                                                  00290200
290300     IF  GAB-INT-ID (GAB-INDEX GAB-INT-INDEX)       = '#IPGT '    00290300
290400         MOVE GAB-INT-SLOT (GAB-INDEX GAB-INT-INDEX)              00290400
290500                                TO ACWA-DISPLAY-LEN-7             00290500
290600         MOVE ACWA-DISPLAY-LEN-7 TO IPGTSLTO                      00290600
290700         GO TO 4400-300-DISPLAY-LOOP.                             00290700
290800                                                                  00290800
290900     IF  GAB-INT-ID (GAB-INDEX GAB-INT-INDEX)       = '#IPGS '    00290900
291000         MOVE GAB-INT-SLOT (GAB-INDEX GAB-INT-INDEX)              00291000
291100                                TO ACWA-DISPLAY-LEN-7             00291100
291200         MOVE ACWA-DISPLAY-LEN-7 TO IPGSSLTO                      00291200
291300         GO TO 4400-300-DISPLAY-LOOP.                             00291300
291400                                                                  00291400
291500     MOVE WS-ABCODE-1CF3        TO WS-ABCODE                      00291500
291600     MOVE WS-ABCODE-1CF3-MSG    TO WS-ABCODE-MSG                  00291600
291700     PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                       00291700
291800                                                                  00291800
291900                                                                  00291900
292000 4400-800-SEND.                                                   00292000
292100                                                                  00292100
292200     PERFORM 4500-000-PROTECT-CRIT-DATA-ELE.                      00292200
292300                                                                  00292300
292400*-------RESET ATTR. 'CAUSE INTDESK & IDPROD CHANGED IN 4500- CALL 00292400
292500     PERFORM 7900-000-RESET-ATTRIBUTES.                           00292500
292600                                                                  00292600
292700     PERFORM 9000-000-SEND-ERASE-RETURN.                          00292700
292800                                                                  00292800
292900 4400-900-EXIT. EXIT.                                             00292900
293000                                                                  00293000
293100/*****************************************************************00293100
293200*  4500  -  PROTECT CRITICAL DATA ELEMENTS                       *00293200
293300*                                                                *00293300
293400*        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *00293400
293500*          1. DETERMINE IF GROUP IS CRITICAL (CALL GCTRSRT).     *00293500
293600*          2. IF GROUP IS CRITICAL:                              *00293600
293700*              - READ PRODUCTION CONTRACT, GROUP SPECIFIC, OR    *00293700
293800*                BENEFIT PROVISION.                              *00293800
293900*                - IF ON DATA BASE:                              *00293900
294000*                  - SCAN FOR #ACL TABULAR                       *00294000
294100*                    - IF TABULAR PRESENT AND ACTIVE, TABULAR IS *00294100
294200*                      CRITICAL, PROTECT CRITICAL DATA ELEMENTS  *00294200
294300*                      ON SCREEN AND ISSUE MESSAGE.              *00294300
294400******************************************************************00294400
294500 4500-000-PROTECT-CRIT-DATA-ELE SECTION.                          00294500
294600 4500-010.                                                        00294600
294700                                                                  00294700
294800     IF DELADDI  =  'CHG/DEL'  OR                                 00294800
294900        DELOLITI  =  SPACE                                        00294900
295000        NEXT SENTENCE                                             00295000
295100     ELSE                                                         00295100
295200        GO TO 4500-900-EXIT.                                      00295200
295300                                                                  00295300
295400     MOVE WS-REQUEST-4500-CDE-PROTECT TO ACWA-CDE-REQUEST-CODE.   00295400
295500     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00295500
295600                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00295600
295700                                                                  00295700
295800     EXEC CICS  LINK   PROGRAM('GACDEPGM')                        00295800
295900                COMMAREA (COMMON-WORKAREAS)                       00295900
296000                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00296000
296100                                                                  00296100
296200     GO TO 4500-900-EXIT.                                         00296200
296300                                                                  00296300
296400 4500-900-EXIT. EXIT.                                             00296400
296500                                                                  00296500
296600/*****************************************************************00296600
296700*  4600  -  UPDATE CRITICAL DATA ELEMENT STATUS                  *00296700
296800*                                                                *00296800
296900*        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *00296900
297000*           1. READ ALL LEVEL TABULAR FROM PROVISION POOL        *00297000
297100*           2. COMPARE CDE ELEMENTS ON W/F ALL LVL TAB TO THOSE  *00297100
297200*               ON THE PROVISION POOL ALL LVL TABULAR RECORD.    *00297200
297300*           3. IF CDE ELEMENTS ON W/F ALL LVL TAB HAVE BEEN      *00297300
297400*               CHANGED, ISSUE MESSAGE AND POSITION CURSOR ON    *00297400
297500*               +CDE+ INDICATOR (POSITION=8).                    *00297500
297600*              IF MESSAGE HAS BEEN ISSUED AND OPERATOR HAS HIT   *00297600
297700*               ENTER, CONTINUE PROCESSING.                      *00297700
297800******************************************************************00297800
297900 4600-000-UPDATE-CDE-STATUS     SECTION.                          00297900
298000 4600-010.                                                        00298000
298100                                                                  00298100
298200     IF CDEINDO = ('+CDE+' OR '+CDE-') AND                        00298200
298300        (DELADDI = 'CHG/DEL' OR                                   00298300
298400        (DELADDI = 'CHG/ADD' AND                                  00298400
298500        WRK-SIGNAL-FROM-ONLINE  =  'W'))                          00298500
298600        NEXT SENTENCE                                             00298600
298700     ELSE                                                         00298700
298800        IF CDEINDO = ('+CDE+' OR '+CDE-')  AND                    00298800
298900           (DELADDI = 'CHG/ADD')                                  00298900
299000           NEXT SENTENCE                                          00299000
299100        ELSE                                                      00299100
299200            GO TO 4600-900-EXIT.                                  00299200
299300                                                                  00299300
299400                                                                  00299400
299500     MOVE WS-ALT-WORKFILE-KEYS       TO ACWA-ALT-WORKFILE-KEYS.   00299500
299600     SET  ACWA-INDEX-1               TO GAB-INDEX.                00299600
299700     MOVE WS-REQUEST-4600-CDE-STATUS TO ACWA-CDE-REQUEST-CODE.    00299700
299800     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00299800
299900                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00299900
300000                                                                  00300000
300100     EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                      00300100
300200                COMMAREA (COMMON-WORKAREAS)                       00300200
300300                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00300300
300400                                                                  00300400
300500     IF  ACWA-CDE-RETURN-DONT-SEND                                00300500
300600*        EXEC CICS  RETURN  END-EXEC.                             00300600
300700         EXEC CICS  RETURN TRANSID('GA1C')                        00300700
300800                    COMMAREA(DFHCOMMAREA)                         00300800
300900                    LENGTH  (EIBCALEN)                            00300900
301000                    END-EXEC.                                     00301000
301100                                                                  00301100
301200 4600-900-EXIT. EXIT.                                             00301200
301300                                                                  00301300
301400/*****************************************************************00301400
301500*  4700  -  UPDATE W/F CONTROL RECORD                            *00301500
301600*                                                                *00301600
301700*        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *00301700
301800*          1. READ W/F CONTROL RECORD, UPDATE CDE RECORD COUNTS  *00301800
301900*             WITH THE ACTION TAKEN ON THE ALL LEVEL TABULAR     *00301900
302000*             RECORD IF ITS CDE STATUS CHANGED.                  *00302000
302100*          2. REWRITE W/F CONTROL RECORD                         *00302100
302200******************************************************************00302200
302300 4700-000-UPDATE-CONTROL-RECORD SECTION.                          00302300
302400 4700-010.                                                        00302400
302500                                                                  00302500
302600     MOVE WS-ALT-WORKFILE-KEYS        TO ACWA-ALT-WORKFILE-KEYS.  00302600
302700     SET  ACWA-INDEX-1                TO GAB-INDEX.               00302700
302800     MOVE WS-REQUEST-4700-CNTL-UPDATE TO ACWA-CDE-REQUEST-CODE.   00302800
302900     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00302900
303000                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00303000
303100                                                                  00303100
303200     EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                      00303200
303300                COMMAREA (COMMON-WORKAREAS)                       00303300
303400                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00303400
303500                                                                  00303500
303600 4700-900-EXIT. EXIT.                                             00303600
303700                                                                  00303700
303800/*****************************************************************00303800
303900* 5000  XCTL TO PREVIOUS MENU                                    *00303900
304000*                                                                *00304000
304100*   THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM      *00304100
304200*  ORIGINATED FROM.  WE READ THE ALL LEVEL TABULAR RECORD AND    *00304200
304300*  INSURE THAT THE TABLE OF OCCURRENCES IS SORTED AND THAT ANY   *00304300
304400*  DUPLICATES ARE DROPPED FROM THE LIST.  WE THEN REWRITE THE    *00304400
304500*  ALL LEVEL TABULAR AND READ THE PARTICULAR RECORD THAT THE     *00304500
304600*  MENU WHICH PASSED US CONTROL WOULD REQUIRE.  FINALLY BASED    *00304600
304700*  ON THE PREVIOUS MENU FIELD CARRIED THROUGHOUT THIS PART OF    *00304700
304800*  THE SYSTEM WE RETURN TO THE PREVIOUS MENU.                    *00304800
304900******************************************************************00304900
305000 5000-000-XCTL-TO-PREVIOUS-MENU SECTION.                          00305000
305100 5000-010.                                                        00305100
305200                                                                  00305200
305300     PERFORM 3200-000-READ-REC-FOR-UPDATE.                        00305300
305400                                                                  00305400
305500     IF NOT GCIO-GOOD-RETURN                                      00305500
305600        MOVE WS-ABCODE-1CFK        TO WS-ABCODE                   00305600
305700        MOVE WS-ABCODE-1CFK-MSG    TO WS-ABCODE-MSG               00305700
305800        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00305800
305900                                                                  00305900
306000     PERFORM 6500-000-SORT-COMPRESS-ALL-LVL.                      00306000
306100     PERFORM 4600-000-UPDATE-CDE-STATUS.                          00306100
306200     PERFORM 3000-000-UPDATE-GAB-RECORD.                          00306200
306300                                                                  00306300
306400     IF ACWA-CDE-FIELD-CHANGED OR  ACWA-CDE-REC-CHANGED           00306400
306500        IF EIBCPOSN = 8                                           00306500
306600           NEXT SENTENCE                                          00306600
306700        ELSE                                                      00306700
306800*          EXEC CICS  RETURN  END-EXEC.                           00306800
306900           EXEC CICS  RETURN TRANSID('GA1C')                      00306900
307000                      COMMAREA(DFHCOMMAREA)                       00307000
307100                      LENGTH  (EIBCALEN)                          00307100
307200                      END-EXEC.                                   00307200
307300                                                                  00307300
307400     IF FRMNUIDI  =  'GS3A'                                       00307400
307500        PERFORM 5100-000-RETURN-TO-GRP-SPEC                       00307500
307600        EXEC CICS  XCTL  PROGRAM ('GS3APGM')                      00307600
307700                   COMMAREA(WORK-RECORD-3)                        00307700
307800                   LENGTH (WS-WRK-GRP-SPEC-LEN)   END-EXEC.       00307800
307900                                                                  00307900
308000     IF  FRMNUIDI  =  'GC4A'                                      00308000
308100        PERFORM 5200-000-RETURN-TO-CONTRACT                       00308100
308200        EXEC CICS  XCTL  PROGRAM ('GC4APGM')                      00308200
308300                   COMMAREA(WORK-RECORD-4)                        00308300
308400                   LENGTH (WS-WRK-CONTRACT-LEN)   END-EXEC.       00308400
308500                                                                  00308500
308600     IF FRMNUIDI  =  'GC8A'                                       00308600
308700        PERFORM 5300-000-RETURN-TO-BEN-PROV                       00308700
308800        EXEC CICS  XCTL  PROGRAM ('GC8APGM')                      00308800
308900                   COMMAREA(WORK-RECORD-5)                        00308900
309000                   LENGTH (WS-WRK-BEN-PROV-LEN)   END-EXEC.       00309000
309100                                                                  00309100
309200*******                                                           00309200
309300* STS *===> RETURN TO SINGLE TABULAR SUPPORT MENU, NO COMMAREA    00309300
309400*******                                                          |00309400
309500     IF  FRMNUIDI  =  'GTM1'                                      00309500
309600         EXEC CICS  XCTL  PROGRAM('GTM1PGM')   END-EXEC.          00309600
309700*******                                                          |00309700
309800* STS *----------------------------------------------------------*00309800
309900*******                                                           00309900
310000                                                                  00310000
310100 5000-900-EXIT. EXIT.                                             00310100
310200                                                                  00310200
310300/*****************************************************************00310300
310400* 5100  RETURN TO GRP SPEC                                       *00310400
310500*                                                                *00310500
310600*    THESE ROUTINES WILL BUILD THE IO PROGRAMS PARAMTER LIST     *00310600
310700*  AND THEN READ THE RECORD NEEDED BY THE PREVIOUS MENU.         *00310700
310800******************************************************************00310800
310900 5100-000-RETURN-TO-GRP-SPEC    SECTION.                          00310900
311000 5100-010.                                                        00311000
311100                                                                  00311100
311200     MOVE SPACES               TO GCIO-WORKFILE-KEY.              00311200
311300     MOVE 'G'                  TO GCIO-WRK-STATUS-CODE.           00311300
311400     MOVE 'G2'                 TO GCIO-WRK-RECORD-TYPE.           00311400
311500     MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             00311500
311600     MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             00311600
311700     MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           00311700
311800     MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              00311800
311900     MOVE SPACES               TO GCIO-WRK-LINE-OF-BUS            00311900
312000                                  GCIO-WRK-PROVISION-ID           00312000
312100                                  GCIO-WRK-PROVIDER-CONTROL       00312100
312200                                  GCIO-WRK-TAB-PROVISION-ID.      00312200
312300     MOVE ZEROS                TO GCIO-WRK-PROVISION-SLOT-NO,     00312300
312400                                  GCIO-WRK-TAB-PROV-SLOT-NO.      00312400
312500     MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   00312500
312600     MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             00312600
312700                                                                  00312700
312800        EXEC CICS GETMAIN                                         00312800
312900               SET(ADDRESS OF WF-IO-PARM-WRK-GRP-SPEC-REC)        00312900
313000               INITIMG(WS-HEX-00)                                 00313000
313100               LENGTH(WS-IO-PARM-WRK-GRP-SPEC-LEN)                00313100
313200               END-EXEC.                                          00313200
313300                                                                  00313300
313400        SET ACWA-WF-GRP-SPEC-PNTR     TO                          00313400
313500                 ADDRESS OF WF-IO-PARM-WRK-GRP-SPEC-REC.          00313500
313600                                                                  00313600
313700     MOVE GC-GCPSWORK-DDNAME     TO GCIO3-FILE-DDNAME.            00313700
313800     MOVE GCIO-WORKFILE-KEY      TO GCIO3-FILE-KEY.               00313800
313900     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO3-FILE-ACCESS-CODE.       00313900
314000     MOVE GC-GCIO-AREA-1         TO GCIO3-IO-AREA-TO-USE.         00314000
314100     MOVE GC-GCGRPSPC-VARY-MAX-OCUR  TO                           00314100
314200                      GCG-COUNT-TAB-PROVN-POINTERS.               00314200
314300                                                                  00314300
314400     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00314400
314500                COMMAREA(WF-IO-PARM-WRK-GRP-SPEC-REC)             00314500
314600                LENGTH (WS-IO-PARM-WRK-GRP-SPEC-LEN)  END-EXEC.   00314600
314700                                                                  00314700
314800     IF NOT GCIO3-GOOD-RETURN                                     00314800
314900        MOVE WS-ABCODE-1CFL        TO WS-ABCODE                   00314900
315000        MOVE WS-ABCODE-1CFL-MSG    TO WS-ABCODE-MSG               00315000
315100        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00315100
315200                                                                  00315200
315300 5100-900-EXIT. EXIT.                                             00315300
315400                                                                  00315400
315500/*****************************************************************00315500
315600* 5200  RETURN TO CONTRACT                                       *00315600
315700*                                                                *00315700
315800*    THESE ROUTINES WILL BUILD THE IO PROGRAMS PARAMTER LIST     *00315800
315900*  AND THEN READ THE RECORD NEEDED BY THE PREVIOUS MENU.         *00315900
316000******************************************************************00316000
316100 5200-000-RETURN-TO-CONTRACT    SECTION.                          00316100
316200 5200-010.                                                        00316200
316300                                                                  00316300
316400     MOVE SPACES               TO GCIO-WORKFILE-KEY.              00316400
316500     MOVE 'C'                  TO GCIO-WRK-STATUS-CODE.           00316500
316600     MOVE 'C2'                 TO GCIO-WRK-RECORD-TYPE.           00316600
316700     MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             00316700
316800     MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             00316800
316900     MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           00316900
317000     MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              00317000
317100     MOVE GCA-L-O-B            TO GCIO-WRK-LINE-OF-BUS.           00317100
317200     MOVE GCA-PROV-CTL         TO GCIO-WRK-PROVIDER-CONTROL.      00317200
317300     MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   00317300
317400     MOVE SPACES               TO GCIO-WRK-PROVISION-ID           00317400
317500                                  GCIO-WRK-TAB-PROVISION-ID.      00317500
317600     MOVE ZEROS                TO GCIO-WRK-PROVISION-SLOT-NO      00317600
317700                                  GCIO-WRK-TAB-PROV-SLOT-NO.      00317700
317800                                                                  00317800
317900        EXEC CICS GETMAIN                                         00317900
318000               SET(ADDRESS OF WF-IO-PARM-WRK-CONTRACT-REC)        00318000
318100               INITIMG(WS-HEX-00)                                 00318100
318200               LENGTH(WS-IO-PARM-WRK-CONTRACT-LEN)                00318200
318300               END-EXEC.                                          00318300
318400                                                                  00318400
318500        SET ACWA-WF-CONTRACT-PNTR     TO                          00318500
318600                 ADDRESS OF WF-IO-PARM-WRK-CONTRACT-REC.          00318600
318700                                                                  00318700
318800                                                                  00318800
318900     MOVE GCA-EFFDT-CEN          TO GCIO-WRK-EFFDT-CEN.           00318900
319000     MOVE GC-GCPSWORK-DDNAME     TO GCIO4-FILE-DDNAME.            00319000
319100     MOVE GCIO-WORKFILE-KEY      TO GCIO4-FILE-KEY.               00319100
319200     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO4-FILE-ACCESS-CODE.       00319200
319300     MOVE GC-GCIO-AREA-1         TO GCIO4-IO-AREA-TO-USE.         00319300
319400     MOVE GC-GCCONTR-VARY-MAX-OCUR  TO                            00319400
319500                      GCT-COUNT-BEN-PROVN-POINTERS.               00319500
319600                                                                  00319600
319700     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00319700
319800                COMMAREA(WF-IO-PARM-WRK-CONTRACT-REC)             00319800
319900                LENGTH (WS-IO-PARM-WRK-CONTRACT-LEN)   END-EXEC.  00319900
320000                                                                  00320000
320100     IF NOT GCIO4-GOOD-RETURN                                     00320100
320200        MOVE WS-ABCODE-1CFM        TO WS-ABCODE                   00320200
320300        MOVE WS-ABCODE-1CFM-MSG    TO WS-ABCODE-MSG               00320300
320400        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00320400
320500                                                                  00320500
320600 5200-900-EXIT. EXIT.                                             00320600
320700                                                                  00320700
320800/*****************************************************************00320800
320900* 5300  RETURN TO BEN PROV                                       *00320900
321000*                                                                *00321000
321100*    THESE ROUTINES WILL BUILD THE IO PROGRAMS PARAMTER LIST     *00321100
321200*  AND THEN READ THE RECORD NEEDED BY THE PREVIOUS MENU.         *00321200
321300******************************************************************00321300
321400 5300-000-RETURN-TO-BEN-PROV    SECTION.                          00321400
321500 5300-010.                                                        00321500
321600                                                                  00321600
321700     MOVE SPACES                TO GCIO-WORKFILE-KEY.             00321700
321800     MOVE 'C'                   TO GCIO-WRK-STATUS-CODE.          00321800
321900     MOVE 'C4'                  TO GCIO-WRK-RECORD-TYPE.          00321900
322000     MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE.            00322000
322100     MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM.            00322100
322200     MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM.          00322200
322300     MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE.             00322300
322400     MOVE GCA-L-O-B             TO GCIO-WRK-LINE-OF-BUS.          00322400
322500     MOVE GCA-PROV-CTL          TO GCIO-WRK-PROVIDER-CONTROL.     00322500
322600     MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVL.  00322600
322700     MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN.            00322700
322800     MOVE GCA-BEN-PROV-ID       TO GCIO-WRK-PROVISION-ID.         00322800
322900     MOVE SPACES                TO GCIO-WRK-TAB-PROVISION-ID.     00322900
323000     MOVE 9999999               TO GCIO-WRK-PROVISION-SLOT-NO.    00323000
323100     MOVE ZEROS                 TO GCIO-WRK-TAB-PROV-SLOT-NO.     00323100
323200                                                                  00323200
323300                                                                  00323300
323400        EXEC CICS GETMAIN                                         00323400
323500               SET(ADDRESS OF WF-IO-PARM-WRK-BEN-PROV-REC)        00323500
323600               INITIMG(WS-HEX-00)                                 00323600
323700               LENGTH(WS-IO-PARM-WRK-BEN-PROV-LEN)                00323700
323800               END-EXEC.                                          00323800
323900                                                                  00323900
324000        SET ACWA-WF-BEN-PROV-PNTR     TO                          00324000
324100                 ADDRESS OF WF-IO-PARM-WRK-BEN-PROV-REC.          00324100
324200                                                                  00324200
324300     MOVE GC-GCPSWORK-DDNAME     TO GCIO5-FILE-DDNAME.            00324300
324400     MOVE GCIO-WORKFILE-KEY      TO GCIO5-FILE-KEY.               00324400
324500     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO5-FILE-ACCESS-CODE.       00324500
324600     MOVE GC-GCIO-AREA-1         TO GCIO5-IO-AREA-TO-USE.         00324600
324700     MOVE GC-GCBENPRV-VARY-MAX-OCUR  TO                           00324700
324800                      GCP-COUNT-TAB-PROVN-POINTERS.               00324800
324900                                                                  00324900
325000     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00325000
325100                COMMAREA(WF-IO-PARM-WRK-BEN-PROV-REC)             00325100
325200                LENGTH (WS-IO-PARM-WRK-BEN-PROV-LEN)  END-EXEC.   00325200
325300                                                                  00325300
325400     IF NOT GCIO5-GOOD-RETURN                                     00325400
325500        MOVE WS-ABCODE-1CFN        TO WS-ABCODE                   00325500
325600        MOVE WS-ABCODE-1CFN-MSG    TO WS-ABCODE-MSG               00325600
325700        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00325700
325800                                                                  00325800
325900 5300-900-EXIT. EXIT.                                             00325900
326000                                                                  00326000
326100/*****************************************************************00326100
326200* 6000  BUILD GROUP SPEC KEY                                     *00326200
326300*                                                                *00326300
326400*    BUILD THE GROUP SPECIFIC KEY FOR WORKFILE READS             *00326400
326500******************************************************************00326500
326600 6000-000-BUILD-GROUP-SPEC-KEY  SECTION.                          00326600
326700 6000-010.                                                        00326700
326800                                                                  00326800
326900     MOVE SPACES                TO GCIO-WORKFILE-KEY.             00326900
327000     MOVE  'G'                  TO GCIO-WRK-STATUS-CODE.          00327000
327100     MOVE  'G3'                 TO GCIO-WRK-RECORD-TYPE.          00327100
327200     MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE.            00327200
327300     MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM.            00327300
327400     MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM.          00327400
327500     MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE.             00327500
327600     MOVE SPACES                TO GCIO-WRK-LINE-OF-BUS,          00327600
327700                                   GCIO-WRK-PROVIDER-CONTROL.     00327700
327800     MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVL.  00327800
327900     MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN.            00327900
328000     MOVE TABIDI                TO GCIO-WRK-PROVISION-ID.         00328000
328100     MOVE TABSLTNI              TO ACWA-DISPLAY-LEN-7.            00328100
328200     MOVE ACWA-DISPLAY-LEN-7    TO GCIO-WRK-PROVISION-SLOT-NO.    00328200
328300     MOVE SPACES                TO GCIO-WRK-TAB-PROVISION-ID.     00328300
328400     MOVE ZEROS                 TO GCIO-WRK-TAB-PROV-SLOT-NO.     00328400
328500                                                                  00328500
328600 6000-900-EXIT. EXIT.                                             00328600
328700                                                                  00328700
328800******************************************************************00328800
328900* 6100  BUILD CONTRACT KEY                                       *00328900
329000*                                                                *00329000
329100*    BUILD THE CONTRACT KEY FOR WORKFILE READS                   *00329100
329200******************************************************************00329200
329300 6100-000-BUILD-CONTRACT-KEY    SECTION.                          00329300
329400 6100-010.                                                        00329400
329500                                                                  00329500
329600     MOVE SPACES               TO GCIO-WORKFILE-KEY.              00329600
329700     MOVE  'C'                 TO GCIO-WRK-STATUS-CODE.           00329700
329800     MOVE  'C3'                TO GCIO-WRK-RECORD-TYPE.           00329800
329900     MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             00329900
330000     MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             00330000
330100     MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           00330100
330200     MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              00330200
330300     MOVE GCA-L-O-B            TO GCIO-WRK-LINE-OF-BUS.           00330300
330400     MOVE GCA-PROV-CTL         TO GCIO-WRK-PROVIDER-CONTROL.      00330400
330500     MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   00330500
330600     MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             00330600
330700     MOVE TABIDI               TO GCIO-WRK-PROVISION-ID.          00330700
330800     MOVE TABSLTNI             TO ACWA-DISPLAY-LEN-7.             00330800
330900     MOVE ACWA-DISPLAY-LEN-7   TO GCIO-WRK-PROVISION-SLOT-NO.     00330900
331000     MOVE SPACES               TO GCIO-WRK-TAB-PROVISION-ID.      00331000
331100     MOVE ZEROS                TO GCIO-WRK-TAB-PROV-SLOT-NO.      00331100
331200                                                                  00331200
331300 6100-900-EXIT. EXIT.                                             00331300
331400                                                                  00331400
331500/*****************************************************************00331500
331600* 6200  BUILD BEN PROV KEY                                       *00331600
331700*                                                                *00331700
331800*    BUILD THE BEN PROV KEY FOR WORKFILE READS                   *00331800
331900******************************************************************00331900
332000 6200-000-BUILD-BEN-PROV-KEY    SECTION.                          00332000
332100 6200-010.                                                        00332100
332200                                                                  00332200
332300     MOVE SPACES                TO GCIO-WORKFILE-KEY.             00332300
332400     MOVE  'C'                  TO GCIO-WRK-STATUS-CODE.          00332400
332500     MOVE  'C5'                 TO GCIO-WRK-RECORD-TYPE.          00332500
332600     MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE.            00332600
332700     MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM.            00332700
332800     MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM.          00332800
332900     MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE.             00332900
333000     MOVE GCA-L-O-B             TO GCIO-WRK-LINE-OF-BUS.          00333000
333100     MOVE GCA-PROV-CTL          TO GCIO-WRK-PROVIDER-CONTROL.     00333100
333200     MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVL.  00333200
333300     MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN.            00333300
333400     MOVE GCA-BEN-PROV-ID       TO GCIO-WRK-PROVISION-ID.         00333400
333500     MOVE +9999999              TO GCIO-WRK-PROVISION-SLOT-NO.    00333500
333600     MOVE TABIDI                TO GCIO-WRK-TAB-PROVISION-ID.     00333600
333700     MOVE TABSLTNI              TO ACWA-DISPLAY-LEN-7.            00333700
333800     MOVE ACWA-DISPLAY-LEN-7    TO GCIO-WRK-TAB-PROV-SLOT-NO.     00333800
333900                                                                  00333900
334000 6200-900-EXIT. EXIT.                                             00334000
334100                                                                  00334100
334200/*****************************************************************00334200
334300*  XCTL TO MAIN MENU                                             *00334300
334400*                                                                *00334400
334500*    THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO   *00334500
334600*  OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE*00334600
334700*  XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE MENUS00334700
334800*  TO GET TO US; WE ARE A MODULE AT THE BOTTOM OF A PYRAMID TO GET00334800
334900*  HERE YOU MUST START AT THE TOP (THE MAIN MENU).               *00334900
335000******************************************************************00335000
335100 6400-000-XCTL-TO-MAIN-MENU     SECTION.                          00335100
335200 6400-010.                                                        00335200
335300                                                                  00335300
335400     MOVE WS-ABCODE-1CP1        TO WS-ABCODE.                     00335400
335500     MOVE WS-ABCODE-1CP1-MSG    TO WS-ABCODE-MSG.                 00335500
335600                                                                  00335600
335700     EXEC CICS  XCTL  PROGRAM('GCPSPGM')   END-EXEC.              00335700
335800                                                                  00335800
335900 6400-900-EXIT. EXIT.                                             00335900
336000                                                                  00336000
336100/*****************************************************************00336100
336200* 6500  SORT COMPRESS ALL LVL                                    *00336200
336300*                                                                *00336300
336400*    THIS ROUTINE WILL COPY ALL ENTRIES FROM THE TABULAR PORTION *00336400
336500*  TO A COPY OF THE TABULAR, THEN SORT THE COPY INTO ASCENDING   *00336500
336600*  SEQUENCE, ANY DUPLICATES ARE REMOVED FROM THE TABLE.          *00336600
336700******************************************************************00336700
336800 6500-000-SORT-COMPRESS-ALL-LVL SECTION.                          00336800
336900 6500-010.                                                        00336900
337000                                                                  00337000
337100        EXEC CICS GETMAIN                                         00337100
337200               SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)            00337200
337300               INITIMG(WS-HEX-00)                                 00337300
337400               LENGTH(WS-COPY-TABLE-LEN)                          00337400
337500               END-EXEC.                                          00337500
337600                                                                  00337600
337700        SET ACWA-COPY-TAB-PNTR        TO                          00337700
337800                 ADDRESS OF COPY-TABULAR-TABLE-AREA.              00337800
337900                                                                  00337900
338000     MOVE GAB-ENTRY-COUNT  TO  GAB-ENTRY-COUNT.                   00338000
338100     SET GAB-INDEX,  COPY-IDX  TO  1.                             00338100
338200                                                                  00338200
338300 6500-100-COPY-TABLE.                                             00338300
338400                                                                  00338400
338500     IF GAB-INDEX  NOT >  GAB-ENTRY-COUNT                         00338500
338600        MOVE GAB-ENTRY (GAB-INDEX)                                00338600
338700          TO COPY-TABULAR-TABLE (COPY-IDX)                        00338700
338800        SET GAB-INDEX,  COPY-IDX  UP BY  1                        00338800
338900        GO TO 6500-100-COPY-TABLE.                                00338900
339000                                                                  00339000
339100     SET  COPY-IDX  TO  1.                                        00339100
339200     SET  COPY-IDX2 TO  2.                                        00339200
339300                                                                  00339300
339400 6500-200-SORT-TABLE.                                             00339400
339500                                                                  00339500
339600     IF COPY-IDX2  >  GAB-ENTRY-COUNT                             00339600
339700        GO TO 6500-400-ARE-WE-DONE-SORTING.                       00339700
339800                                                                  00339800
339900     IF  COPY-SORTABLE-FLDS (COPY-IDX)  >                         00339900
340000         COPY-SORTABLE-FLDS (COPY-IDX2)                           00340000
340100     THEN                                                         00340100
340200         MOVE COPY-TABULAR-TABLE (COPY-IDX)                       00340200
340300           TO WS-ENTRY                                            00340300
340400         MOVE COPY-TABULAR-TABLE (COPY-IDX2)                      00340400
340500           TO COPY-TABULAR-TABLE (COPY-IDX)                       00340500
340600         MOVE WS-ENTRY                                            00340600
340700           TO COPY-TABULAR-TABLE (COPY-IDX2)                      00340700
340800         SET COPY-IDX2  UP BY  1                                  00340800
340900         GO TO 6500-200-SORT-TABLE.                               00340900
341000                                                                  00341000
341100     IF COPY-SORTABLE-FLDS (COPY-IDX)  <                          00341100
341200        COPY-SORTABLE-FLDS (COPY-IDX2)                            00341200
341300        SET COPY-IDX2  UP BY  1                                   00341300
341400        GO TO 6500-200-SORT-TABLE.                                00341400
341500                                                                  00341500
341600     SET COPY-IDX3,  COPY-IDX4  TO  COPY-IDX2.                    00341600
341700     SET COPY-IDX4  UP BY 1.                                      00341700
341800                                                                  00341800
341900 6500-300-ELIMINATE-DUPLICATES.                                   00341900
342000                                                                  00342000
342100     IF COPY-IDX4  NOT >  GAB-ENTRY-COUNT                         00342100
342200        MOVE COPY-TABULAR-TABLE (COPY-IDX4)  TO                   00342200
342300             COPY-TABULAR-TABLE (COPY-IDX3)                       00342300
342400        SET COPY-IDX3,  COPY-IDX4  UP BY  1                       00342400
342500        GO TO 6500-300-ELIMINATE-DUPLICATES.                      00342500
342600                                                                  00342600
342700     SUBTRACT 1  FROM  GAB-ENTRY-COUNT.                           00342700
342800     GO TO 6500-200-SORT-TABLE.                                   00342800
342900                                                                  00342900
343000 6500-400-ARE-WE-DONE-SORTING.                                    00343000
343100                                                                  00343100
343200     IF COPY-IDX  <  GAB-ENTRY-COUNT                              00343200
343300        SET COPY-IDX   UP BY  1                                   00343300
343400        SET COPY-IDX2  TO COPY-IDX                                00343400
343500        SET COPY-IDX2  UP BY 1                                    00343500
343600        GO TO 6500-200-SORT-TABLE.                                00343600
343700                                                                  00343700
343800     MOVE GAB-ENTRY-COUNT  TO  GAB-ENTRY-COUNT.                   00343800
343900     SET GAB-INDEX,  COPY-IDX  TO  1.                             00343900
344000                                                                  00344000
344100 6500-500-MOVE-COPY-BACK.                                         00344100
344200                                                                  00344200
344300     IF GAB-INDEX  NOT >  GAB-ENTRY-COUNT                         00344300
344400        MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                    00344400
344500             GAB-ENTRY (GAB-INDEX)                                00344500
344600        SET GAB-INDEX,  COPY-IDX  UP BY  1                        00344600
344700        GO TO 6500-500-MOVE-COPY-BACK.                            00344700
344800                                                                  00344800
344900     MOVE GAB-ENTRY-COUNT  TO  GAB-ENTRY-COUNT.                   00344900
345000     IF GAB-ENTRY-COUNT  NOT <  GC-GCTABULR-ACL-VARY-MAX-OCUR     00345000
345100        MOVE 'Y'  TO  ACWA-ERROR-SW.                              00345100
345200                                                                  00345200
345300 6500-900-EXIT. EXIT.                                             00345300
345400                                                                  00345400
345500/*****************************************************************00345500
345600* 7900  RESET ATTRIBUTES                                         *00345600
345700******************************************************************00345700
345800 7900-000-RESET-ATTRIBUTES      SECTION.                          00345800
345900 7900-010.                                                        00345900
346000                                                                  00346000
346100     MOVE DFHBMUNF                                                00346100
346200       TO BENVLQLA  COPAYINA  CSTCONTA  FAMINDIA  LOBA CONDLIFA   00346200
346300          PERIODA   PLCTRMTA  SRVGRUPA  PRTIMEFA  MANAPLIA        00346300
346400          REININDA  INTRVALA  INTTYPEA  CLMLVLIA  BNMXVALA        00346400
346500          DAYFACIA  OVRDINDA  NEWVALUA  INTDESKA  FYIVALA         00346500
346600          CONDALLA  CONDEXCA  CONDICDA  CONDTABA  CONDMENA        00346600
346700          CONDEACA  CONDEMCA  CONDSMIA  CONDNSMA                  00346700
346800          CONDDRGA  CONDALCA  CONDOBNA  CONDOBCA  CONDMALA        00346800
346900          CONDCARA  CONDOBSA  CONDKDYA  CONDACCA  CONDPECA        00346900
347000          CONDNEMA  DEFINTNA  CONDSUIA  FDLRCLIA  PERLIMTA        00347000
347100          ASCDSCDA  BISNDINA  CONDTMJA  CONDINFA AGEQLLA AGEQLHA  00347100
347200          IBGROPTA  IPGNOPTA  IPGTOPTA  MFRMSLTA  PERTQALA        00347200
347300          IDGDOPTA  IPGPOPTA  AGELIMLA  AGELIMHA  RELPINDA        00347300
347400          FEAKINDA  CARYOVRA  IPGSOPTA  ACCUMIDA  CAPINDA         00347400
347500          SABDINDA  BENTYPA   TIERCDA   TIERLVA.                  00347500
347700                                                                  00347700
347800     IF  DELADDO  =  'CHG/DEL'                                    00347800
347900     THEN                                                         00347900
348000         NEXT SENTENCE                                            00348000
348100     ELSE                                                         00348100
348200         GO TO 7900-900-EXIT.                                     00348200
348300                                                                  00348300
348400                                                                  00348400
348500     IF  CDEINDO = '+CDE+'                                        00348500
348600     THEN                                                         00348600
348700*---------------- HIGH-LIGHT CRITICAL DATA ELEMENT LABELS         00348700
348800         MOVE DFHBMABF TO DLOPTLTA  PERIOTA  BENVLQTA  LOTA       00348800
348900                          PLCTRTTA  FAMINDTA SRVGRUTA  CSTCOTTA   00348900
349000               AGELIMA    COPAYITA  INTDESTA CONDTG1A  CONDTG2A   00349000
349100               AGEQLTA    MANAPLTA  PERLITTA BISNDITA             00349100
349200         IF INTDESKO  NOT =  IDPRODO                              00349200
349300            MOVE DFHBMASB  TO  IBGRIDA,  IPGNIDA,  IPGTIDA        00349300
349400                               IDGDIDA,  IPGPIDA,  IPGSIDA        00349400
349500            MOVE DFHBMABF  TO  IBGRSLTA, IPGNSLTA, IPGTSLTA       00349500
349600                               IDGDSLTA, IPGPSLTA, IPGSSLTA       00349600
349700            MOVE DFHBMUBF  TO  IBGROPTA, IPGNOPTA, IPGTOPTA       00349700
349800                               IDGDOPTA, IPGPOPTA, IPGSOPTA       00349800
349900         ELSE                                                     00349900
350000            MOVE DFHBMASF  TO  IBGRIDA,  IPGNIDA,  IPGTIDA        00350000
350100                               IDGDIDA,  IPGPIDA,  IPGSIDA        00350100
350200            MOVE DFHBMASF  TO  IBGRSLTA, IPGNSLTA, IPGTSLTA       00350200
350300                               IDGDSLTA, IPGPSLTA, IPGSSLTA       00350300
350400            MOVE DFHBMUNF  TO  IBGROPTA, IPGNOPTA, IPGTOPTA       00350400
350500                               IDGDOPTA, IPGPOPTA, IPGSOPTA       00350500
350600     ELSE                                                         00350600
350700         NEXT SENTENCE.                                           00350700
350800                                                                  00350800
350900                                                                  00350900
351000     IF  CDEINDO = '+CDE-'                                        00351000
351100     THEN                                                         00351100
351200*---------------- HIGH-LIGHT CRITICAL DATA ELEMENT LABELS         00351200
351300         MOVE DFHBMABF TO DLOPTLTA  PERIOTA  BENVLQTA  LOTA       00351300
351400               AGELIMA    PLCTRTTA  FAMINDTA SRVGRUTA  CSTCOTTA   00351400
351500               AGEQLTA    COPAYITA  INTDESTA CONDTG1A  CONDTG2A   00351500
351600                          MANAPLTA  PERLITTA BISNDITA             00351600
351700*---------------- AUTOSKIP AND FSET CRITICAL DATA ELEMENTS        00351700
351800         MOVE DFHBMASF TO DELOPTNA  PERIODA  BENVLQLA  LOBA       00351800
351900        AGELIMLA AGELIMHA PLCTRMTA  FAMINDIA SRVGRUPA  CSTCONTA   00351900
352000        AGEQLLA  AGEQLHA  COPAYINA  INTDESKA CONDALLA  CONDEXCA   00352000
352100                 CONDLIFA CONDICDA  CONDTABA CONDMENA  CONDDRGA   00352100
352200                          CONDALCA  CONDOBCA CONDOBNA  CONDMALA   00352200
352300                          CONDEMCA  CONDEACA CONDSMIA  CONDNSMA   00352300
352400                          CONDCARA  CONDOBSA CONDKDYA  CONDACCA   00352400
352500                          CONDPECA  CONDNEMA CONDSUIA  CONDTMJA   00352500
352600                          MANAPLIA  PERLIMTA BISNDINA  CONDINFA   00352600
352700         IF INTDESKO  NOT =  IDPRODO                              00352700
352800*--------- AUTOSKIP AND FSET CRITICAL DATA ELEMENTS               00352800
352900            MOVE DFHBMASF  TO  IBGROPTA,  IPGNOPTA,  IPGTOPTA     00352900
353000                               IDGDOPTA,  IPGPOPTA,  IPGSOPTA     00353000
353100            MOVE DFHBMABF  TO  IBGRIDA,  IBGRSLTA,                00353100
353200                       IPGNIDA,  IPGNSLTA,   IPGTIDA,  IPGTSLTA   00353200
353300                       IDGDIDA,  IDGDSLTA,   IPGPIDA,  IPGPSLTA   00353300
353400                       IPGSIDA,  IPGSSLTA                         00353400
353500            IF  ERRMSGO > SPACES                                  00353500
353600            THEN                                                  00353600
353700                NEXT SENTENCE                                     00353700
353800            ELSE                                                  00353800
353900                SET  WT-01-INDEX  TO  +08                         00353900
354000                MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO 00354000
354100         ELSE                                                     00354100
354200            MOVE DFHBMASF  TO  IBGRIDA,  IBGRSLTA,                00354200
354300                       IPGNIDA,  IPGNSLTA,   IPGTIDA,  IPGTSLTA   00354300
354400                       IDGDIDA,  IDGDSLTA,   IPGPIDA,  IPGPSLTA   00354400
354500                       IPGSIDA,  IPGSSLTA                         00354500
354600            MOVE DFHBMUNF  TO  IBGROPTA, IPGNOPTA, IPGTOPTA       00354600
354700                               IDGDOPTA, IPGPOPTA, IPGSOPTA       00354700
354800            IF  ERRMSGO > SPACES                                  00354800
354900            THEN                                                  00354900
355000                NEXT SENTENCE                                     00355000
355100            ELSE                                                  00355100
355200                SET  WT-01-INDEX  TO  +08                         00355200
355300                MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO 00355300
355400     ELSE                                                         00355400
355500         NEXT SENTENCE.                                           00355500
355600                                                                  00355600
355700                                                                  00355700
355800 7900-900-EXIT. EXIT.                                             00355800
355900                                                                  00355900
356000/*****************************************************************00356000
356100* 8000  XCTL SWITCH ADD DEL MODE                                 *00356100
356200*                                                                *00356200
356300*   THE OPERATOR WANTS TO SWITCH MODES, FROM DELETING ENTRIES TO *00356300
356400*  ADDING ENTRIES.  WE READ THE ALL LEVEL INTERNAL TABULAR & PASS*00356400
356500*  THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL     *00356500
356600*  TABULAR RECORD TO THE ADD PROGRAM.  (DEPENDING ON THE MENU THE*00356600
356700*  PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE   *00356700
356800*  IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).             *00356800
356900******************************************************************00356900
357000 8000-000-SWITCH-ADD-DEL-MODE   SECTION.                          00357000
357100 8000-010.                                                        00357100
357200                                                                  00357200
357300     IF DELADDI  =  'CHG/DEL'                                     00357300
357400        PERFORM 8100-000-DISPLAY-ADD-SCREEN.                      00357400
357500                                                                  00357500
357600     PERFORM 3100-000-READ-RECORD.                                00357600
357700     MOVE GAB-ENTRY-COUNT  TO  GAB-ENTRY-COUNT.                   00357700
357800                                                                  00357800
357900     IF  GAB-ENTRY-COUNT  >  1                                    00357900
358000     THEN                                                         00358000
358100         MOVE 'CHG/DEL'  TO DELADDO                               00358100
358200         MOVE 'D'        TO DELOLITO                              00358200
358300         MOVE SPACES     TO COCURANO                              00358300
358400         MOVE DFHBMASK   TO DLOPTLTA                              00358400
358500         MOVE DFHBMUNP   TO DELOPTNA                              00358500
358600         SET  WT-01-INDEX                     TO +20              00358600
358700         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00358700
358800         SET GAB-INDEX   TO 1                                     00358800
358900         PERFORM 4400-000-BUILD-DISPLAY                           00358900
359000     ELSE                                                         00359000
359100         SET  WT-01-INDEX                     TO +12              00359100
359200         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO.         00359200
359300                                                                  00359300
359400 8000-900-EXIT. EXIT.                                             00359400
359500                                                                  00359500
359600/*****************************************************************00359600
359700* 8100  DISPLAY ADD SCREEN                                       *00359700
359800******************************************************************00359800
359900 8100-000-DISPLAY-ADD-SCREEN    SECTION.                          00359900
360000                                                                  00360000
360100     MOVE 'CHG/ADD'  TO DELADDO.                                  00360100
360200     MOVE SPACES     TO COCURANO.                                 00360200
360300     MOVE DFHBMASD   TO DLOPTLTA   DELOPTNA.                      00360300
360400     PERFORM 4100-000-DISPLAY-SKELETON.                           00360400
360500                                                                  00360500
360600 8100-900-EXIT. EXIT.                                             00360600
360700                                                                  00360700
360800/*****************************************************************00360800
360900* 9000  SEND ERASE THEN RETURN                                   *00360900
361000******************************************************************00361000
361100 9000-000-SEND-ERASE-RETURN     SECTION.                          00361100
361200 9000-010.                                                        00361200
361300                                                                  00361300
361400     MOVE DFHBMASD  TO  MAXOVRTA  MAXOVRDA TIMEDLRA TIMEDOLA.     00361400
361500                                                                  00361500
361600     MOVE -1  TO  ERRMSGL.                                        00361600
361700                                                                  00361700
361800     EXEC CICS  SEND   MAP ('GA1XI01')  ERASE  CURSOR             00361800
361900                MAPSET('GA1XSET')   END-EXEC.                     00361900
362000                                                                  00362000
362100     EXEC CICS  RETURN TRANSID('GA1C')                            00362100
362200                COMMAREA(DFHCOMMAREA)                             00362200
362300                LENGTH  (EIBCALEN)                                00362300
362400                END-EXEC.                                         00362400
362500                                                                  00362500
362600 9000-900-EXIT. EXIT.                                             00362600
362700                                                                  00362700
362800/*****************************************************************00362800
362900* 9010  SEND DATAONLY AND RETURN                                 *00362900
363000******************************************************************00363000
363100 9010-000-SEND-DATAONLY-RETURN  SECTION.                          00363100
363200 9010-010.                                                        00363200
363300                                                                  00363300
363400     MOVE -1  TO  ERRMSGL.                                        00363400
363500                                                                  00363500
363600     EXEC CICS  SEND   MAP ('GA1XI01')  DATAONLY  CURSOR          00363600
363700                MAPSET('GA1XSET')  END-EXEC.                      00363700
363800                                                                  00363800
363900*    EXEC CICS  RETURN   END-EXEC.                                00363900
364000     EXEC CICS  RETURN TRANSID('GA1C')                            00364000
364100                COMMAREA(DFHCOMMAREA)                             00364100
364200                LENGTH  (EIBCALEN)                                00364200
364300                END-EXEC.                                         00364300
364400                                                                  00364400
364500 9010-900-EXIT.                                                   00364500
364600           EXIT.                                                  00364600
364700/*****************************************************************00364700
364800* 9200  GREGORIAN TO JULIAN                                      *00364800
364900*                                                                *00364900
365000*         MMDDYY---->YYDDD                                       *00365000
365100******************************************************************00365100
365200 9200-000-GREGORIAN-TO-JULIAN   SECTION.                          00365200
365300 9200-010.                                                        00365300
365400                                                                  00365400
365500     MOVE 'CNV'  TO  HGADATE-FUNC.                                00365500
365600     MOVE 'M'    TO  HGADATE-FORM1.                               00365600
365700     MOVE 'J'    TO  HGADATE-FORM2.                               00365700
365800     MOVE ZEROS  TO HGADATE-RETURN   HGADATE-AMOUNT.              00365800
365900                                                                  00365900
366000     EXEC  CICS LINK PROGRAM ('HGADATES')                         00366000
366100                     COMMAREA(HGADATES-COMMAREA)                  00366100
366200                     LENGTH  (LENGTH OF HGADATES-COMMAREA)        00366200
366300                     END-EXEC.                                    00366300
366400                                                                  00366400
366500                                                                  00366500
366600 9200-900-EXIT.                                                   00366600
366700          EXIT.                                                   00366700
366800                                                                  00366800
366900/*****************************************************************00366900
367000* 9300  JULIAN TO GREGORIAN                                      *00367000
367100*                                                                *00367100
367200*          YYDDD---->MMDDYY                                      *00367200
367300******************************************************************00367300
367400 9300-000-JULIAN-TO-GREGORIAN   SECTION.                          00367400
367500 9300-010.                                                        00367500
367600                                                                  00367600
367700     MOVE 'CNV'  TO  HGADATE-FUNC.                                00367700
367800     MOVE 'J'    TO  HGADATE-FORM1.                               00367800
367900     MOVE 'M'    TO  HGADATE-FORM2.                               00367900
368000     MOVE ZEROS  TO HGADATE-RETURN   HGADATE-AMOUNT.              00368000
368100                                                                  00368100
368200     EXEC  CICS LINK PROGRAM ('HGADATES')                         00368200
368300                     COMMAREA(HGADATES-COMMAREA)                  00368300
368400                     LENGTH  (LENGTH OF HGADATES-COMMAREA)        00368400
368500                     END-EXEC.                                    00368500
368600                                                                  00368600
368700 9300-900-EXIT. EXIT.                                             00368700
368800                                                                  00368800
368900/*****************************************************************00368900
369000* 9800  ERROR MSG THEN ABEND                                     *00369000
369100*                                                                *00369100
369200*    THIS ROUTINE DISPLAYS THE PREVIOUSLY BUILT ERROR MESSAGE    *00369200
369300*  AND THEN ABENDS USING THE ABEND CODE EARLIER DEFINED.         *00369300
369400******************************************************************00369400
369500 9800-000-ERROR-MSG-THEN-ABEND  SECTION.                          00369500
369600 9800-010.                                                        00369600
369700                                                                  00369700
369800     MOVE -1               TO MFRMSLTL.                           00369800
369900     MOVE WS-ABCODE-MSG    TO ERRMSGO.                            00369900
370000                                                                  00370000
370100     EXEC CICS  SEND   MAP ('GA1XI01')  ERASE  CURSOR  WAIT       00370100
370200                MAPSET('GA1XSET')   END-EXEC.                     00370200
370300                                                                  00370300
370400     EXEC CICS  ABEND   ABCODE(WS-ABCODE)  END-EXEC.              00370400
370500                                                                  00370500
370600 9800-900-EXIT. EXIT.                                             00370600
