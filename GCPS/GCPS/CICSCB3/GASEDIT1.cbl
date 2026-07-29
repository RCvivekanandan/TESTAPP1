000100 ID DIVISION.                                                     00000100
000200 PROGRAM-ID.     GASEDIT1.                                        00000200
000300**** THIS IS A COBOL/2 PROGRAM ***                                00000300
000400 AUTHOR.         J.L.ARKEMA.                                      00000400
000500 DATE-WRITTEN.   04/20/87.                                        00000500
000600 DATE-COMPILED.                                                   00000600
000700***************************************************************** 00000700
000800*                                                               * 00000800
000900*       M A I N T E N A N C E     L O G                         * 00000900
001000*               WT-01-TABLE                                     * 00001000
001100*                                                               * 00001100
001200**-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* 00001200
001300*                                                               * 00001300
001400*  N126      08/28/87  JLA   ADD LOGIC FOR SUICIDE BIT          * 00001400
001500*                                                               * 00001500
001600*  M220      11/25/87  DES   ALLOWED THE CONJUNCTION OF ICD     * 00001600
001700*                            WITH EITHER PEC OR EXC COND. BITS  * 00001700
001800*                                                               * 00001800
001900*  D126      02/24/88  JLA  1. CHANGE OPTION FILE SELECTION 'S' * 00001900
002000*                              TO 'A'.                          * 00002000
002100*                                                               * 00002100
002200*  D?????    01/13/89  ENW   ADD LOGIC FOR BISCENDING IND.      * 00002200
002300*                                                               * 00002300
002400*  D200      05/18/89  NGE  ADD TWO NEW COND-BITS TMJ AND INF,  * 00002400
002500*                           TEMPROMAND-JOINT AND INFERTILITY.   * 00002500
002600*                                                               * 00002600
002700*  D242      02/22/90  NGE  ADD NEW LOGICAL EDIT TO ALL ACCUMS  * 00002700
002800*                           BENEFIT PERIOD IND, BENEFIT PERIOD  * 00002800
002900*                           TIME QUALIFIER, BENEFIT PERIOD TIME * 00002900
003000*                           FACTOR.                             * 00003000
003100*                                                               * 00003100
003200*  D246      03/13/90  NGE  ADD NEW LOGICAL EDIT TO ALL ACCUMS  * 00003200
003300*                        IF BENEFIT PERIOD = 0V THEN QUAL CAN BE* 00003300
003400*                        ONLY 6 OR 9 ...ETC                     * 00003400
003500*                                                               * 00003500
003600*                       --- EXPAND ACCUM TABULARS ----           *00003600
003700* 11154   10/15/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *00003700
003800* D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *00003800
003900* D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *00003900
004000* D270                  4. ADD AGE-LIMIT-FROM AND AGE-LIMIT-TO.  *00004000
004100*                       5. ADD RELATIONSHIP-IND (CDE ELEMENT).   *00004100
004200*                       6. VALIDATE RELATIONSHIPIND ONLY.        *00004200
004300*                       7. ADD NEW INTERNAL TABS #IDGD AND #IPGP.*00004300
004400*                       8. >>> CONVERT TO COBOL/2 <<<.           *00004400
004500*                                                                *00004500
004600* 11154   01/16/91  NGE 1. ADD AGE-QUAL-IND-FROM AND AGE-QUAL-   *00004600
004700*                          TO (CDE-ELEMENTS).                    *00004700
004800*                                                                *00004800
004900* 12262  02/28/92 TPM ADD NEW COND-BIT LIF  (LIFE-THREATING)     *00004900
005000*                     COND-LIFE-THREAT-BIT                       *00005000
005100*                                                                *00005100
005200* XXXXX  12/27/93 KJD ADD AGE LIMIT CONVERSION TO DAYS FOR CHECK-*00005200
005300*                     ING PROPER RANGES ENTERED, ADD NEW ERR MSGS*00005300
005400*                                                                *00005400
005500* XXXXX  03/02/94 GDM PROD FIX TO MOVE SPACES TO INTERNAL       -*00005500
005600*                     DESCRIPTOR ( INTDESKO )                    *00005600
005700*                                                                *00005700
005800* D323H  01/08/97 DAU DEIMPLEMENTED COPYBOOK, GCAIDTBL.          *00005800
005900*                     CHANGED VALIDATION LOGIC FOR AGE RANGE     *00005900
006000*                     INTERNAL DESCRIPTORS TO ACCESS THE FIELD   *00006000
006100*                     VALIDATION TABLE INSTEAD OF COPYBOOK,      *00006100
006200*                     GCAIDTBL.                                  *00006200
006300*                                                                *00006300
006400*  D303  01/22/97 DAU ADD LOGIC FOR FEAK INDICATOR.              *00006400
006500*                                                                *00006500
006600* 14726/ 10/27/97 DAU ADDED CODE TO SUPPORT THE YEAR 2000 AND    *00006600
006700* 15057               THE EXPANSION OF THE GROUP SPECIFIC AND    *00006700
006800*                     CONTRACT KEY TO SUPPORT THE TEXAS MERGER.  *00006800
006900*                                                                *00006900
007000*  D341  10/01/98 GDM ADD NEW ACCUM TABULAR #ACP.                *00007000
007100*                                                                *00007100
007200*  D341  10/30/98 GDM 1. ADD VALUE AACL04 FOR #ACL CARRY OVER    *00007200
007300*                        CREDIT INDICATOR.                       *00007300
007400*                     2. ADD VALUE AACP04 COPAY DEFINITION AND   *00007400
007500*                        AACP05 COPAY TIME & DOLLAR IND FOR      *00007500
007600*                        #ACP ACCUMULATOR COPAY.                 *00007600
007700*                                                                *00007700
007800*  D352  09/20/00 GDM ADD LOGIC FOR ACCUMULATOR IDENTIFIER       *00007800
007900*                                                                *00007900
008000*        11/15/00 DAF COMMENTED OUT LOGIC WHEN AGE RANGES ARE    *00008000
008100*                     CODED ONLY CERTAIN INTERNAL DESCRIPTORS    *00008100
008200*                     ARE NEEDED.  (CAUSING PROBLEMS WITH        *00008200
008300*                     NEGATIVE OCCURRENCES ON THE COPAY TABULARS *00008300
008400*                     FOR NEW MEXICO.)                           *00008400
008500*                                                                *00008500
008600*        11/28/00 GSP ADDED #IPGS.                               *00008600
008700*                                                                *00008700
008800*        11/13/01 AKK ADD SUPPORT FOR FOR NEW CONDITION BITS,    *00008800
008900*                 GSP TWO FOR SERIOUS MENTAL ILLNESS AND TWO     *00008900
009000*                     FOR EMERGENCY.                             *00009000
009100*                                                                *00009100
009200*  D365B 06/03/02  JP ADD LOGIC FOR COMBINATION APPLID IND (CAPI)*00009200
009300*                                                                *00009300
009400*  D368  06/04/02  JP ADD LOGIC FOR SELECTIVE ADDITIONAL BENEFIT *00009400
009500*                         DETERMINATION (SABD)                   *00009500
009600*                                                                *00009600
009700*            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *00009700
009800*                                                                *00009800
009900*        01/28/04  JP FIX EDITING FOR ACCUM ID                   *00009900
010000*                                                                *00010000
010100* P09400 11/07/06  GF ADD EDITING FOR ASCEND/DESCEND AND BISCEND-*00010100
010200*                         ING INDICATORS FOR #ABM, #ACP, #ADL    *00010200
010300*                                                                *00010300
010400*        05/07/07  LR FIX FOR SABD EDITING                       *00010400
010500*                                                                *00010500
010600*        05/05/14  MJL ALLOW ZERO IN PERCENT FIELD.              *00010600
010700*                                                                *00010700
      * P21595  09/19/16  HSB CHANGES FOR GCPS NEW FIELDS BENEFIT      *00010720
      *                       TYPE CODE,TIER CODE,TIER LEVEL.          *00010730
010800******************************************************************00010800
010900                                                                  00010900
011000***************************************************************** 00011000
011100*                                                               * 00011100
011200*    GASEDIT1 - THIS PROGRAM PERFORMS SCREEN EDITS FOR ALL LEVEL* 00011200
011300*               TABULAR MAINTENANCE PROGRAMS GA1BPGM, GA1CPGM,  * 00011300
011400*               GA1DPGM, GA1EPGM, AND GA1PPGM                   * 00011400
011500*                                                               * 00011500
011600*                                                               * 00011600
011700*    TRANSID: (GA1B, GA1C, GA1D, GA1E, OR GA1P)                 * 00011700
011800*                                                               * 00011800
011900*                          ********************************     * 00011900
012000*                          *                              *     * 00012000
012100*                          *   THIS MAPSET IS SHARED BY   *     * 00012100
012200*                          *   THE FOLLOWING MODULES:     *     * 00012200
012300*                          *                              *     * 00012300
012400*                          *   1. GA1BPGM                 *     * 00012400
012500*                          *   2. GA1CPGM                 *     * 00012500
012600*                          *   3. GA1DPGM                 *     * 00012600
012700*                          *   4. GA1EPGM                 *     * 00012700
012800*                          *   5. GA1PPGM                 *     * 00012800
012900*                          *   6. GASEDIT1                *     * 00012900
013000*    MAPSET:   GA1XSETC ==>*   7. GACDEPGM                *     * 00013000
013100*                          *   8. GK1BPGM                 *     * 00013100
013200*                          *   9. GK1CPGM                 *     * 00013200
013300*                          *  10. GK1DPGM                 *     * 00013300
013400*                          *  11. GK1EPGM                 *     * 00013400
013500*                          *  12. GK1PPGM                 *     * 00013500
013600*                          *                              *     * 00013600
013700*                          ********************************     * 00013700
013800*                                                               * 00013800
013900*                                                               * 00013900
014000*    VALGEN:  NONE                                              * 00014000
014100*                                                               * 00014100
014200*                                                               * 00014200
014300***************************************************************** 00014300
014400                                                                  00014400
014500 ENVIRONMENT DIVISION.                                            00014500
014600 DATA DIVISION.                                                   00014600
014700/                                                                 00014700
014800 WORKING-STORAGE SECTION.                                         00014800
014900 01  WS-BEGIN                    PIC X(58) VALUE                  00014900
015000     '*** GASEDIT1  WORKING-STORAGE BEGINS HERE ***'.             00015000
015100                                                                  00015100
015200/                                                                 00015200
015300 01  WS-ABEND-AREA.                                               00015300
015400     05  FILLER                   PIC X(16)  VALUE                00015400
015500         '** ABEND AREA **'.                                      00015500
015600                                                                  00015600
015700     05  WS-ABEND-CODES-AND-MSG.                                  00015700
015800         10  WS-ABCODE                  PIC X(04)  VALUE  SPACES. 00015800
015900         10  WS-ABCODE-MSG              PIC X(44)  VALUE  SPACES. 00015900
016000                                                                  00016000
016100         10  WS-ABCODE-XXXX             PIC X(04)  VALUE  'XXXX'. 00016100
016200         10  WS-ABCODE-XXXX-MSG         PIC X(44)  VALUE          00016200
016300            '                                         '.          00016300
016400                                                                  00016400
016500     05  WS-ACTUAL-CONDITION-BITS      PIC 9(2).                  00016500
016600     05  WS-ALL-BUT-EXC-N-ALL          PIC 9(2).                  00016600
016700     05  WS-SUM-OF-ALL-BITS            PIC 9(2).                  00016700
016800                                                                  00016800
016900     05  WS-TAB-BIT                    PIC 9.                     00016900
017000     05  WS-MEN-BIT                    PIC 9.                     00017000
017100     05  WS-DRG-BIT                    PIC 9.                     00017100
017200     05  WS-ALC-BIT                    PIC 9.                     00017200
017300     05  WS-OBN-BIT                    PIC 9.                     00017300
017400     05  WS-OBC-BIT                    PIC 9.                     00017400
017500     05  WS-MAL-BIT                    PIC 9.                     00017500
017600     05  WS-CAR-BIT                    PIC 9.                     00017600
017700     05  WS-OBS-BIT                    PIC 9.                     00017700
017800     05  WS-KDY-BIT                    PIC 9.                     00017800
017900     05  WS-ACC-BIT                    PIC 9.                     00017900
018000     05  WS-ICD-BIT                    PIC 9.                     00018000
018100     05  WS-ALL-BIT                    PIC 9.                     00018100
018200     05  WS-EXC-BIT                    PIC 9.                     00018200
018300     05  WS-PEC-BIT                    PIC 9.                     00018300
018400     05  WS-NEM-BIT                    PIC 9.                     00018400
018500     05  WS-SUI-BIT                    PIC 9.                     00018500
018600     05  WS-TMJ-BIT                    PIC 9.                     00018600
018700     05  WS-INF-BIT                    PIC 9.                     00018700
018800     05  WS-LIF-BIT                    PIC 9.                     00018800
018900     05  WS-EMC-BIT                    PIC 9.                     00018900
019000     05  WS-EAC-BIT                    PIC 9.                     00019000
019100     05  WS-SMI-BIT                    PIC 9.                     00019100
019200     05  WS-NSM-BIT                    PIC 9.                     00019200
019300     05  WS-SLOT-CNT                   PIC 9  VALUE ZEROS.        00019300
019400     05  WS-DEL-CNT                    PIC 9  VALUE ZEROS.        00019400
019500                                                                  00019500
019600                                                                  00019600
019700/                                                                 00019700
019800 01  WS-MISC.                                                     00019800
019900     05  WS-AGE-LIM-FROM          PIC S9(7) COMP-3 VALUE ZEROES.  00019900
020000     05  WS-AGE-LIM-TO            PIC S9(7) COMP-3 VALUE ZEROES.  00020000
020100     05  WS-INTDESC-NOTFOUND      PIC X VALUE SPACE.              00020100
020200                                                                  00020200
020300 01  WT-00-GASEDIT1-TABLES.                                       00020300
020400     05  FILLER                   PIC X(16)  VALUE                00020400
020500         '*GASEDIT1 TABLES'.                                      00020500
020600                                                                  00020600
020700                                                                  00020700
020800 01  WT-01-TABLE.                                                 00020800
020900     05  FILLER                  PIC X(16) VALUE                  00020900
021000         '* WT-01-TABLE  *'.                                      00021000
021100******************************************************************00021100
021200*    WT-01   MESSAGE TABLE                                       *00021200
021300******************************************************************00021300
021400 01  FILLER.                                                      00021400
021500     05  WT-01-MESSAGE-VALUES.                                    00021500
021600                                                                  00021600
021700*----------------------------------------------------------------*00021700
021800         10  WT-01-ENTRY-001.                                     00021800
021900             15  FILLER              PIC X(2)  VALUE '¬>'.        00021900
022000             15  WT-01-MESSAGE-TEXT-001.                          00022000
022100                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00022100
022200                 20  FILLER          PIC X(1)  VALUE  '-'.        00022200
022300                 20  FILLER          PIC X(3)  VALUE  '001'.      00022300
022400                 20  FILLER          PIC X(1)  VALUE  ' '.        00022400
022500                 20  FILLER          PIC X(70) VALUE              00022500
022600                     'INTERVAL OVRD IND IS REQUIRED WHEN INTERVAL 00022600
022700-                    'VALUE IS CODED           '.                 00022700
022800             15  FILLER              PIC X(2)  VALUE '<¬'.        00022800
022900                                                                  00022900
023000*----------------------------------------------------------------*00023000
023100         10  WT-01-ENTRY-002.                                     00023100
023200             15  FILLER              PIC X(2)  VALUE '¬>'.        00023200
023300             15  WT-01-MESSAGE-TEXT-002.                          00023300
023400                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00023400
023500                 20  FILLER          PIC X(1)  VALUE  '-'.        00023500
023600                 20  FILLER          PIC X(3)  VALUE  '002'.      00023600
023700                 20  FILLER          PIC X(1)  VALUE  ' '.        00023700
023800                 20  FILLER          PIC X(70) VALUE              00023800
023900                     'TO DELETE THIS ENTRY YOU MUST ENTER ''D''   00023900
024000-                    '                         '.                 00024000
024100             15  FILLER              PIC X(2)  VALUE '<¬'.        00024100
024200                                                                  00024200
024300*----------------------------------------------------------------*00024300
024400         10  WT-01-ENTRY-003.                                     00024400
024500             15  FILLER              PIC X(2)  VALUE '¬>'.        00024500
024600             15  WT-01-MESSAGE-TEXT-003.                          00024600
024700                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00024700
024800                 20  FILLER          PIC X(1)  VALUE  '-'.        00024800
024900                 20  FILLER          PIC X(3)  VALUE  '003'.      00024900
025000                 20  FILLER          PIC X(1)  VALUE  ' '.        00025000
025100                 20  FILLER          PIC X(70) VALUE              00025100
025200                     'DAY FACTOR IND IS INVALID                   00025200
025300-                    '                         '.                 00025300
025400             15  FILLER              PIC X(2)  VALUE '<¬'.        00025400
025500                                                                  00025500
025600*----------------------------------------------------------------*00025600
025700         10  WT-01-ENTRY-004.                                     00025700
025800             15  FILLER              PIC X(2)  VALUE '¬>'.        00025800
025900             15  WT-01-MESSAGE-TEXT-004.                          00025900
026000                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00026000
026100                 20  FILLER          PIC X(1)  VALUE  '-'.        00026100
026200                 20  FILLER          PIC X(3)  VALUE  '004'.      00026200
026300                 20  FILLER          PIC X(1)  VALUE  ' '.        00026300
026400                 20  FILLER          PIC X(70) VALUE              00026400
026500                     'CO-PAY IND IS INVALID                       00026500
026600-                    '                         '.                 00026600
026700             15  FILLER              PIC X(2)  VALUE '<¬'.        00026700
026800                                                                  00026800
026900*----------------------------------------------------------------*00026900
027000         10  WT-01-ENTRY-005.                                     00027000
027100             15  FILLER              PIC X(2)  VALUE '¬>'.        00027100
027200             15  WT-01-MESSAGE-TEXT-005.                          00027200
027300                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00027300
027400                 20  FILLER          PIC X(1)  VALUE  '-'.        00027400
027500                 20  FILLER          PIC X(3)  VALUE  '005'.      00027500
027600                 20  FILLER          PIC X(1)  VALUE  ' '.        00027600
027700                 20  FILLER          PIC X(70) VALUE              00027700
027800                     'BENEFIT PERIOD IS INVALID                   00027800
027900-                    '                         '.                 00027900
028000             15  FILLER              PIC X(2)  VALUE '<¬'.        00028000
028100                                                                  00028100
028200*----------------------------------------------------------------*00028200
028300         10  WT-01-ENTRY-006.                                     00028300
028400             15  FILLER              PIC X(2)  VALUE '¬>'.        00028400
028500             15  WT-01-MESSAGE-TEXT-006.                          00028500
028600                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00028600
028700                 20  FILLER          PIC X(1)  VALUE  '-'.        00028700
028800                 20  FILLER          PIC X(3)  VALUE  '006'.      00028800
028900                 20  FILLER          PIC X(1)  VALUE  ' '.        00028900
029000                 20  FILLER          PIC X(70) VALUE              00029000
029100                     'BENEFIT PERIOD IS REQUIRED                  00029100
029200-                    '                         '.                 00029200
029300             15  FILLER              PIC X(2)  VALUE '<¬'.        00029300
029400                                                                  00029400
029500*----------------------------------------------------------------*00029500
029600         10  WT-01-ENTRY-007.                                     00029600
029700             15  FILLER              PIC X(2)  VALUE '¬>'.        00029700
029800             15  WT-01-MESSAGE-TEXT-007.                          00029800
029900                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00029900
030000                 20  FILLER          PIC X(1)  VALUE  '-'.        00030000
030100                 20  FILLER          PIC X(3)  VALUE  '007'.      00030100
030200                 20  FILLER          PIC X(1)  VALUE  ' '.        00030200
030300                 20  FILLER          PIC X(70) VALUE              00030300
030400                     'PERIOD TIME QUAL IS INVALID                 00030400
030500-                    '                         '.                 00030500
030600             15  FILLER              PIC X(2)  VALUE '<¬'.        00030600
030700                                                                  00030700
030800*----------------------------------------------------------------*00030800
030900         10  WT-01-ENTRY-008.                                     00030900
031000             15  FILLER              PIC X(2)  VALUE '¬>'.        00031000
031100             15  WT-01-MESSAGE-TEXT-008.                          00031100
031200                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00031200
031300                 20  FILLER          PIC X(1)  VALUE  '-'.        00031300
031400                 20  FILLER          PIC X(3)  VALUE  '008'.      00031400
031500                 20  FILLER          PIC X(1)  VALUE  ' '.        00031500
031600                 20  FILLER          PIC X(70) VALUE              00031600
031700                     'PERIOD TIME FACTOR MUST BE NUMERIC ONLY, ANY00031700
031800-                    ' OTHER CHAR. IS INVALID  '.                 00031800
031900             15  FILLER              PIC X(2)  VALUE '<¬'.        00031900
032000                                                                  00032000
032100*----------------------------------------------------------------*00032100
032200         10  WT-01-ENTRY-009.                                     00032200
032300             15  FILLER              PIC X(2)  VALUE '¬>'.        00032300
032400             15  WT-01-MESSAGE-TEXT-009.                          00032400
032500                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00032500
032600                 20  FILLER          PIC X(1)  VALUE  '-'.        00032600
032700                 20  FILLER          PIC X(3)  VALUE  '009'.      00032700
032800                 20  FILLER          PIC X(1)  VALUE  ' '.        00032800
032900                 20  FILLER          PIC X(70) VALUE              00032900
033000                     'PERIOD TIME FACTOR IS REQUIRED WHEN PERIOD T00033000
033100-                    'IME QUALIFIER IS CODED   '.                 00033100
033200             15  FILLER              PIC X(2)  VALUE '<¬'.        00033200
033300                                                                  00033300
033400*----------------------------------------------------------------*00033400
033500         10  WT-01-ENTRY-010.                                     00033500
033600             15  FILLER              PIC X(2)  VALUE '¬>'.        00033600
033700             15  WT-01-MESSAGE-TEXT-010.                          00033700
033800                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00033800
033900                 20  FILLER          PIC X(1)  VALUE  '-'.        00033900
034000                 20  FILLER          PIC X(3)  VALUE  '010'.      00034000
034100                 20  FILLER          PIC X(1)  VALUE  ' '.        00034100
034200                 20  FILLER          PIC X(70) VALUE              00034200
034300                     'PERIOD TIME QUALIFIER IS REQUIRED WHEN PERIO00034300
034400-                    'D TIME FACTOR IS CODED   '.                 00034400
034500             15  FILLER              PIC X(2)  VALUE '<¬'.        00034500
034600                                                                  00034600
034700*----------------------------------------------------------------*00034700
034800         10  WT-01-ENTRY-011.                                     00034800
034900             15  FILLER              PIC X(2)  VALUE '¬>'.        00034900
035000             15  WT-01-MESSAGE-TEXT-011.                          00035000
035100                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00035100
035200                 20  FILLER          PIC X(1)  VALUE  '-'.        00035200
035300                 20  FILLER          PIC X(3)  VALUE  '011'.      00035300
035400                 20  FILLER          PIC X(1)  VALUE  ' '.        00035400
035500                 20  FILLER          PIC X(70) VALUE              00035500
035600                     'PERIOD MAX OVRD IND IS INVALID              00035600
035700-                    '                         '.                 00035700
035800             15  FILLER              PIC X(2)  VALUE '<¬'.        00035800
035900                                                                  00035900
036000*----------------------------------------------------------------*00036000
036100         10  WT-01-ENTRY-012.                                     00036100
036200             15  FILLER              PIC X(2)  VALUE '¬>'.        00036200
036300             15  WT-01-MESSAGE-TEXT-012.                          00036300
036400                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00036400
036500                 20  FILLER          PIC X(1)  VALUE  '-'.        00036500
036600                 20  FILLER          PIC X(3)  VALUE  '012'.      00036600
036700                 20  FILLER          PIC X(1)  VALUE  ' '.        00036700
036800                 20  FILLER          PIC X(70) VALUE              00036800
036900                     'VALUE QUALIFIER IS INVALID                  00036900
037000-                    '                         '.                 00037000
037100             15  FILLER              PIC X(2)  VALUE '<¬'.        00037100
037200                                                                  00037200
037300*----------------------------------------------------------------*00037300
037400         10  WT-01-ENTRY-013.                                     00037400
037500             15  FILLER              PIC X(2)  VALUE '¬>'.        00037500
037600             15  WT-01-MESSAGE-TEXT-013.                          00037600
037700                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00037700
037800                 20  FILLER          PIC X(1)  VALUE  '-'.        00037800
037900                 20  FILLER          PIC X(3)  VALUE  '013'.      00037900
038000                 20  FILLER          PIC X(1)  VALUE  ' '.        00038000
038100                 20  FILLER          PIC X(70) VALUE              00038100
038200                     'VALUE QUALIFIER IS REQUIRED                 00038200
038300-                    '                         '.                 00038300
038400             15  FILLER              PIC X(2)  VALUE '<¬'.        00038400
038500                                                                  00038500
038600*----------------------------------------------------------------*00038600
038700         10  WT-01-ENTRY-014.                                     00038700
038800             15  FILLER              PIC X(2)  VALUE '¬>'.        00038800
038900             15  WT-01-MESSAGE-TEXT-014.                          00038900
039000                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00039000
039100                 20  FILLER          PIC X(1)  VALUE  '-'.        00039100
039200                 20  FILLER          PIC X(3)  VALUE  '014'.      00039200
039300                 20  FILLER          PIC X(1)  VALUE  ' '.        00039300
039400                 20  FILLER          PIC X(70) VALUE              00039400
039500                     'VALUE LIMIT MUST BE NUMERIC ONLY, ANY OTHER 00039500
039600-                    ' CHARACTER IS INVALID    '.                 00039600
039700             15  FILLER              PIC X(2)  VALUE '<¬'.        00039700
039800                                                                  00039800
039900*----------------------------------------------------------------*00039900
040000         10  WT-01-ENTRY-015.                                     00040000
040100             15  FILLER              PIC X(2)  VALUE '¬>'.        00040100
040200             15  WT-01-MESSAGE-TEXT-015.                          00040200
040300                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00040300
040400                 20  FILLER          PIC X(1)  VALUE  '-'.        00040400
040500                 20  FILLER          PIC X(3)  VALUE  '015'.      00040500
040600                 20  FILLER          PIC X(1)  VALUE  ' '.        00040600
040700                 20  FILLER          PIC X(70) VALUE              00040700
040800                     'VALUE LIMIT LENGTH ERROR                    00040800
040900-                    '                         '.                 00040900
041000             15  FILLER              PIC X(2)  VALUE '<¬'.        00041000
041100                                                                  00041100
041200*----------------------------------------------------------------*00041200
041300         10  WT-01-ENTRY-016.                                     00041300
041400             15  FILLER              PIC X(2)  VALUE '¬>'.        00041400
041500             15  WT-01-MESSAGE-TEXT-016.                          00041500
041600                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00041600
041700                 20  FILLER          PIC X(1)  VALUE  '-'.        00041700
041800                 20  FILLER          PIC X(3)  VALUE  '016'.      00041800
041900                 20  FILLER          PIC X(1)  VALUE  ' '.        00041900
042000                 20  FILLER          PIC X(70) VALUE              00042000
042100                     'VALUE LIMIT FIELD MUST HAVE DECIMAL POINT   00042100
042200-                    '                         '.                 00042200
042300             15  FILLER              PIC X(2)  VALUE '<¬'.        00042300
042400                                                                  00042400
042500*----------------------------------------------------------------*00042500
042600         10  WT-01-ENTRY-017.                                     00042600
042700             15  FILLER              PIC X(2)  VALUE '¬>'.        00042700
042800             15  WT-01-MESSAGE-TEXT-017.                          00042800
042900                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00042900
043000                 20  FILLER          PIC X(1)  VALUE  '-'.        00043000
043100                 20  FILLER          PIC X(3)  VALUE  '017'.      00043100
043200                 20  FILLER          PIC X(1)  VALUE  ' '.        00043200
043300                 20  FILLER          PIC X(70) VALUE              00043300
043400                     'VALUE LIMIT IS INVALID                      00043400
043500-                    '                         '.                 00043500
043600             15  FILLER              PIC X(2)  VALUE '<¬'.        00043600
043700                                                                  00043700
043800*----------------------------------------------------------------*00043800
043900         10  WT-01-ENTRY-018.                                     00043900
044000             15  FILLER              PIC X(2)  VALUE '¬>'.        00044000
044100             15  WT-01-MESSAGE-TEXT-018.                          00044100
044200                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00044200
044300                 20  FILLER          PIC X(1)  VALUE  '-'.        00044300
044400                 20  FILLER          PIC X(3)  VALUE  '018'.      00044400
044500                 20  FILLER          PIC X(1)  VALUE  ' '.        00044500
044600                 20  FILLER          PIC X(70) VALUE              00044600
044700                     'VALUE LIMIT IS REQUIRED WHEN VALUE QUALIFIER00044700
044800-                    ' IS CODED                '.                 00044800
044900             15  FILLER              PIC X(2)  VALUE '<¬'.        00044900
045000                                                                  00045000
045100*----------------------------------------------------------------*00045100
045200         10  WT-01-ENTRY-019.                                     00045200
045300             15  FILLER              PIC X(2)  VALUE '¬>'.        00045300
045400             15  WT-01-MESSAGE-TEXT-019.                          00045400
045500                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00045500
045600                 20  FILLER          PIC X(1)  VALUE  '-'.        00045600
045700                 20  FILLER          PIC X(3)  VALUE  '019'.      00045700
045800                 20  FILLER          PIC X(1)  VALUE  ' '.        00045800
045900                 20  FILLER          PIC X(70) VALUE              00045900
046000                     'INTERVAL TYPE FIELD IS INVALID              00046000
046100-                    '                         '.                 00046100
046200             15  FILLER              PIC X(2)  VALUE '<¬'.        00046200
046300                                                                  00046300
046400*----------------------------------------------------------------*00046400
046500         10  WT-01-ENTRY-020.                                     00046500
046600             15  FILLER              PIC X(2)  VALUE '¬>'.        00046600
046700             15  WT-01-MESSAGE-TEXT-020.                          00046700
046800                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00046800
046900                 20  FILLER          PIC X(1)  VALUE  '-'.        00046900
047000                 20  FILLER          PIC X(3)  VALUE  '020'.      00047000
047100                 20  FILLER          PIC X(1)  VALUE  ' '.        00047100
047200                 20  FILLER          PIC X(70) VALUE              00047200
047300                     'INTERVAL TIME FACTOR MUST BE NUMERIC        00047300
047400-                    '                         '.                 00047400
047500             15  FILLER              PIC X(2)  VALUE '<¬'.        00047500
047600                                                                  00047600
047700*----------------------------------------------------------------*00047700
047800         10  WT-01-ENTRY-021.                                     00047800
047900             15  FILLER              PIC X(2)  VALUE '¬>'.        00047900
048000             15  WT-01-MESSAGE-TEXT-021.                          00048000
048100                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00048100
048200                 20  FILLER          PIC X(1)  VALUE  '-'.        00048200
048300                 20  FILLER          PIC X(3)  VALUE  '021'.      00048300
048400                 20  FILLER          PIC X(1)  VALUE  ' '.        00048400
048500                 20  FILLER          PIC X(70) VALUE              00048500
048600                     'INTERVAL TIME FACTOR IS REQUIRED WHEN INTERV00048600
048700-                    'AL TYPE IS CODED         '.                 00048700
048800             15  FILLER              PIC X(2)  VALUE '<¬'.        00048800
048900                                                                  00048900
049000*----------------------------------------------------------------*00049000
049100         10  WT-01-ENTRY-022.                                     00049100
049200             15  FILLER              PIC X(2)  VALUE '¬>'.        00049200
049300             15  WT-01-MESSAGE-TEXT-022.                          00049300
049400                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00049400
049500                 20  FILLER          PIC X(1)  VALUE  '-'.        00049500
049600                 20  FILLER          PIC X(3)  VALUE  '022'.      00049600
049700                 20  FILLER          PIC X(1)  VALUE  ' '.        00049700
049800                 20  FILLER          PIC X(70) VALUE              00049800
049900                     'INTERVAL TYPE IS REQUIRED WHEN INTERVAL TIME00049900
050000-                    ' FACTOR IS CODED         '.                 00050000
050100             15  FILLER              PIC X(2)  VALUE '<¬'.        00050100
050200                                                                  00050200
050300*----------------------------------------------------------------*00050300
050400         10  WT-01-ENTRY-023.                                     00050400
050500             15  FILLER              PIC X(2)  VALUE '¬>'.        00050500
050600             15  WT-01-MESSAGE-TEXT-023.                          00050600
050700                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00050700
050800                 20  FILLER          PIC X(1)  VALUE  '-'.        00050800
050900                 20  FILLER          PIC X(3)  VALUE  '023'.      00050900
051000                 20  FILLER          PIC X(1)  VALUE  ' '.        00051000
051100                 20  FILLER          PIC X(70) VALUE              00051100
051200                     'INTERVAL OVRD IND IS INVALID                00051200
051300-                    '                         '.                 00051300
051400             15  FILLER              PIC X(2)  VALUE '<¬'.        00051400
051500                                                                  00051500
051600*----------------------------------------------------------------*00051600
051700         10  WT-01-ENTRY-024.                                     00051700
051800             15  FILLER              PIC X(2)  VALUE '¬>'.        00051800
051900             15  WT-01-MESSAGE-TEXT-024.                          00051900
052000                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00052000
052100                 20  FILLER          PIC X(1)  VALUE  '-'.        00052100
052200                 20  FILLER          PIC X(3)  VALUE  '024'.      00052200
052300                 20  FILLER          PIC X(1)  VALUE  ' '.        00052300
052400                 20  FILLER          PIC X(70) VALUE              00052400
052500                     'INTERVAL OVERRIDE VALUE MUST BE NUMERIC     00052500
052600-                    '                         '.                 00052600
052700             15  FILLER              PIC X(2)  VALUE '<¬'.        00052700
052800                                                                  00052800
052900*----------------------------------------------------------------*00052900
053000         10  WT-01-ENTRY-025.                                     00053000
053100             15  FILLER              PIC X(2)  VALUE '¬>'.        00053100
053200             15  WT-01-MESSAGE-TEXT-025.                          00053200
053300                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00053300
053400                 20  FILLER          PIC X(1)  VALUE  '-'.        00053400
053500                 20  FILLER          PIC X(3)  VALUE  '025'.      00053500
053600                 20  FILLER          PIC X(1)  VALUE  ' '.        00053600
053700                 20  FILLER          PIC X(70) VALUE              00053700
053800                     'INTERVAL OVRD VALUE IS REQUIRED WHEN INTERVA00053800
053900-                    'L OVRD IND IS CODED      '.                 00053900
054000             15  FILLER              PIC X(2)  VALUE '<¬'.        00054000
054100                                                                  00054100
054200*----------------------------------------------------------------*00054200
054300         10  WT-01-ENTRY-026.                                     00054300
054400             15  FILLER              PIC X(2)  VALUE '¬>'.        00054400
054500             15  WT-01-MESSAGE-TEXT-026.                          00054500
054600                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00054600
054700                 20  FILLER          PIC X(1)  VALUE  '-'.        00054700
054800                 20  FILLER          PIC X(3)  VALUE  '026'.      00054800
054900                 20  FILLER          PIC X(1)  VALUE  ' '.        00054900
055000                 20  FILLER          PIC X(70) VALUE              00055000
055100                     'LINE OF BUSINESS IS INVALID                 00055100
055200-                    '                         '.                 00055200
055300             15  FILLER              PIC X(2)  VALUE '<¬'.        00055300
055400                                                                  00055400
055500*----------------------------------------------------------------*00055500
055600         10  WT-01-ENTRY-027.                                     00055600
055700             15  FILLER              PIC X(2)  VALUE '¬>'.        00055700
055800             15  WT-01-MESSAGE-TEXT-027.                          00055800
055900                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00055900
056000                 20  FILLER          PIC X(1)  VALUE  '-'.        00056000
056100                 20  FILLER          PIC X(3)  VALUE  '027'.      00056100
056200                 20  FILLER          PIC X(1)  VALUE  ' '.        00056200
056300                 20  FILLER          PIC X(70) VALUE              00056300
056400                     'LINE OF BUSINESS IS REQUIRED                00056400
056500-                    '                         '.                 00056500
056600             15  FILLER              PIC X(2)  VALUE '<¬'.        00056600
056700                                                                  00056700
056800*----------------------------------------------------------------*00056800
056900         10  WT-01-ENTRY-028.                                     00056900
057000             15  FILLER              PIC X(2)  VALUE '¬>'.        00057000
057100             15  WT-01-MESSAGE-TEXT-028.                          00057100
057200                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00057200
057300                 20  FILLER          PIC X(1)  VALUE  '-'.        00057300
057400                 20  FILLER          PIC X(3)  VALUE  '028'.      00057400
057500                 20  FILLER          PIC X(1)  VALUE  ' '.        00057500
057600                 20  FILLER          PIC X(70) VALUE              00057600
057700                     'PLACE OF TREATMENT IS INVALID               00057700
057800-                    '                         '.                 00057800
057900             15  FILLER              PIC X(2)  VALUE '<¬'.        00057900
058000                                                                  00058000
058100*----------------------------------------------------------------*00058100
058200         10  WT-01-ENTRY-029.                                     00058200
058300             15  FILLER              PIC X(2)  VALUE '¬>'.        00058300
058400             15  WT-01-MESSAGE-TEXT-029.                          00058400
058500                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00058500
058600                 20  FILLER          PIC X(1)  VALUE  '-'.        00058600
058700                 20  FILLER          PIC X(3)  VALUE  '029'.      00058700
058800                 20  FILLER          PIC X(1)  VALUE  ' '.        00058800
058900                 20  FILLER          PIC X(70) VALUE              00058900
059000                     'FAMILY/INDIV IS INVALID                     00059000
059100-                    '                         '.                 00059100
059200             15  FILLER              PIC X(2)  VALUE '<¬'.        00059200
059300                                                                  00059300
059400*----------------------------------------------------------------*00059400
059500         10  WT-01-ENTRY-030.                                     00059500
059600             15  FILLER              PIC X(2)  VALUE '¬>'.        00059600
059700             15  WT-01-MESSAGE-TEXT-030.                          00059700
059800                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00059800
059900                 20  FILLER          PIC X(1)  VALUE  '-'.        00059900
060000                 20  FILLER          PIC X(3)  VALUE  '030'.      00060000
060100                 20  FILLER          PIC X(1)  VALUE  ' '.        00060100
060200                 20  FILLER          PIC X(70) VALUE              00060200
060300                     'FAMILY/INDIV IS REQUIRED                    00060300
060400-                    '                         '.                 00060400
060500             15  FILLER              PIC X(2)  VALUE '<¬'.        00060500
060600                                                                  00060600
060700*----------------------------------------------------------------*00060700
060800         10  WT-01-ENTRY-031.                                     00060800
060900             15  FILLER              PIC X(2)  VALUE '¬>'.        00060900
061000             15  WT-01-MESSAGE-TEXT-031.                          00061000
061100                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00061100
061200                 20  FILLER          PIC X(1)  VALUE  '-'.        00061200
061300                 20  FILLER          PIC X(3)  VALUE  '031'.      00061300
061400                 20  FILLER          PIC X(1)  VALUE  ' '.        00061400
061500                 20  FILLER          PIC X(70) VALUE              00061500
061600                     'FAMILY/INDIV IND MUST = F WHEN VALUE QUALIFI00061600
061700-                    'ER = 7                   '.                 00061700
061800             15  FILLER              PIC X(2)  VALUE '<¬'.        00061800
061900                                                                  00061900
062000*----------------------------------------------------------------*00062000
062100         10  WT-01-ENTRY-032.                                     00062100
062200             15  FILLER              PIC X(2)  VALUE '¬>'.        00062200
062300             15  WT-01-MESSAGE-TEXT-032.                          00062300
062400                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00062400
062500                 20  FILLER          PIC X(1)  VALUE  '-'.        00062500
062600                 20  FILLER          PIC X(3)  VALUE  '032'.      00062600
062700                 20  FILLER          PIC X(1)  VALUE  ' '.        00062700
062800                 20  FILLER          PIC X(70) VALUE              00062800
062900                     'SERVICE GROUP IS INVALID                    00062900
063000-                    '                         '.                 00063000
063100             15  FILLER              PIC X(2)  VALUE '<¬'.        00063100
063200                                                                  00063200
063300*----------------------------------------------------------------*00063300
063400         10  WT-01-ENTRY-033.                                     00063400
063500             15  FILLER              PIC X(2)  VALUE '¬>'.        00063500
063600             15  WT-01-MESSAGE-TEXT-033.                          00063600
063700                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00063700
063800                 20  FILLER          PIC X(1)  VALUE  '-'.        00063800
063900                 20  FILLER          PIC X(3)  VALUE  '033'.      00063900
064000                 20  FILLER          PIC X(1)  VALUE  ' '.        00064000
064100                 20  FILLER          PIC X(70) VALUE              00064100
064200                     'REINSTATEMENT IS INVALID                    00064200
064300-                    '                         '.                 00064300
064400             15  FILLER              PIC X(2)  VALUE '<¬'.        00064400
064500                                                                  00064500
064600*----------------------------------------------------------------*00064600
064700         10  WT-01-ENTRY-034.                                     00064700
064800             15  FILLER              PIC X(2)  VALUE '¬>'.        00064800
064900             15  WT-01-MESSAGE-TEXT-034.                          00064900
065000                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00065000
065100                 20  FILLER          PIC X(1)  VALUE  '-'.        00065100
065200                 20  FILLER          PIC X(3)  VALUE  '034'.      00065200
065300                 20  FILLER          PIC X(1)  VALUE  ' '.        00065300
065400                 20  FILLER          PIC X(70) VALUE              00065400
065500                     'CLAIM LVL IS INVALID                        00065500
065600-                    '                         '.                 00065600
065700             15  FILLER              PIC X(2)  VALUE '<¬'.        00065700
065800                                                                  00065800
065900*----------------------------------------------------------------*00065900
066000         10  WT-01-ENTRY-035.                                     00066000
066100             15  FILLER              PIC X(2)  VALUE '¬>'.        00066100
066200             15  WT-01-MESSAGE-TEXT-035.                          00066200
066300                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00066300
066400                 20  FILLER          PIC X(1)  VALUE  '-'.        00066400
066500                 20  FILLER          PIC X(3)  VALUE  '035'.      00066500
066600                 20  FILLER          PIC X(1)  VALUE  ' '.        00066600
066700                 20  FILLER          PIC X(70) VALUE              00066700
066800                     'MAP FROM SLOT# IS INVALID                   00066800
066900-                    '                         '.                 00066900
067000             15  FILLER              PIC X(2)  VALUE '<¬'.        00067000
067100                                                                  00067100
067200*----------------------------------------------------------------*00067200
067300         10  WT-01-ENTRY-036.                                     00067300
067400             15  FILLER              PIC X(2)  VALUE '¬>'.        00067400
067500             15  WT-01-MESSAGE-TEXT-036.                          00067500
067600                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00067600
067700                 20  FILLER          PIC X(1)  VALUE  '-'.        00067700
067800                 20  FILLER          PIC X(3)  VALUE  '036'.      00067800
067900                 20  FILLER          PIC X(1)  VALUE  ' '.        00067900
068000                 20  FILLER          PIC X(70) VALUE              00068000
068100                     'COST CONTAINMENT IS INVALID                 00068100
068200-                    '                         '.                 00068200
068300             15  FILLER              PIC X(2)  VALUE '<¬'.        00068300
068400                                                                  00068400
068500*----------------------------------------------------------------*00068500
068600         10  WT-01-ENTRY-037.                                     00068600
068700             15  FILLER              PIC X(2)  VALUE '¬>'.        00068700
068800             15  WT-01-MESSAGE-TEXT-037.                          00068800
068900                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00068900
069000                 20  FILLER          PIC X(1)  VALUE  '-'.        00069000
069100                 20  FILLER          PIC X(3)  VALUE  '037'.      00069100
069200                 20  FILLER          PIC X(1)  VALUE  ' '.        00069200
069300                 20  FILLER          PIC X(70) VALUE              00069300
069400                     'FYI IS INVALID                              00069400
069500-                    '                         '.                 00069500
069600             15  FILLER              PIC X(2)  VALUE '<¬'.        00069600
069700                                                                  00069700
069800*----------------------------------------------------------------*00069800
069900         10  WT-01-ENTRY-038.                                     00069900
070000             15  FILLER              PIC X(2)  VALUE '¬>'.        00070000
070100             15  WT-01-MESSAGE-TEXT-038.                          00070100
070200                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00070200
070300                 20  FILLER          PIC X(1)  VALUE  '-'.        00070300
070400                 20  FILLER          PIC X(3)  VALUE  '038'.      00070400
070500                 20  FILLER          PIC X(1)  VALUE  ' '.        00070500
070600                 20  FILLER          PIC X(70) VALUE              00070600
070700                     'INTERNAL DESCRIPTOR IS INVALID              00070700
070800-                    '                         '.                 00070800
070900             15  FILLER              PIC X(2)  VALUE '<¬'.        00070900
071000                                                                  00071000
071100*----------------------------------------------------------------*00071100
071200         10  WT-01-ENTRY-039.                                     00071200
071300             15  FILLER              PIC X(2)  VALUE '¬>'.        00071300
071400             15  WT-01-MESSAGE-TEXT-039.                          00071400
071500                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00071500
071600                 20  FILLER          PIC X(1)  VALUE  '-'.        00071600
071700                 20  FILLER          PIC X(3)  VALUE  '039'.      00071700
071800                 20  FILLER          PIC X(1)  VALUE  ' '.        00071800
071900                 20  FILLER          PIC X(70) VALUE              00071900
072000                     'INTERNAL DESCRIPTOR IS INVALID WHEN INTERNAL00072000
072100-                    ' TABULAR IS NOT CODED.   '.                 00072100
072200             15  FILLER              PIC X(2)  VALUE '<¬'.        00072200
072300                                                                  00072300
072400*----------------------------------------------------------------*00072400
072500         10  WT-01-ENTRY-040.                                     00072500
072600             15  FILLER              PIC X(2)  VALUE '¬>'.        00072600
072700             15  WT-01-MESSAGE-TEXT-040.                          00072700
072800                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00072800
072900                 20  FILLER          PIC X(1)  VALUE  '-'.        00072900
073000                 20  FILLER          PIC X(3)  VALUE  '040'.      00073000
073100                 20  FILLER          PIC X(1)  VALUE  ' '.        00073100
073200                 20  FILLER          PIC X(70) VALUE              00073200
073300                     'INTERNAL TABULAR IS REQUIRED WHEN INTERNAL D00073300
073400-                    'ESCRIPTOR IS CODED.      '.                 00073400
073500             15  FILLER              PIC X(2)  VALUE '<¬'.        00073500
073600                                                                  00073600
073700*----------------------------------------------------------------*00073700
073800         10  WT-01-ENTRY-041.                                     00073800
073900             15  FILLER              PIC X(2)  VALUE '¬>'.        00073900
074000             15  WT-01-MESSAGE-TEXT-041.                          00074000
074100                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00074100
074200                 20  FILLER          PIC X(1)  VALUE  '-'.        00074200
074300                 20  FILLER          PIC X(3)  VALUE  '041'.      00074300
074400                 20  FILLER          PIC X(1)  VALUE  ' '.        00074400
074500                 20  FILLER          PIC X(70) VALUE              00074500
074600                     'CONDITION IS INVALID                        00074600
074700-                    '                         '.                 00074700
074800             15  FILLER              PIC X(2)  VALUE '<¬'.        00074800
074900                                                                  00074900
075000*----------------------------------------------------------------*00075000
075100         10  WT-01-ENTRY-042.                                     00075100
075200             15  FILLER              PIC X(2)  VALUE '¬>'.        00075200
075300             15  WT-01-MESSAGE-TEXT-042.                          00075300
075400                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00075400
075500                 20  FILLER          PIC X(1)  VALUE  '-'.        00075500
075600                 20  FILLER          PIC X(3)  VALUE  '042'.      00075600
075700                 20  FILLER          PIC X(1)  VALUE  ' '.        00075700
075800                 20  FILLER          PIC X(70) VALUE              00075800
075900                     'CONDITION BITS COMBINATION IS INVALID       00075900
076000-                    '                         '.                 00076000
076100             15  FILLER              PIC X(2)  VALUE '<¬'.        00076100
076200                                                                  00076200
076300*----------------------------------------------------------------*00076300
076400         10  WT-01-ENTRY-043.                                     00076400
076500             15  FILLER              PIC X(2)  VALUE '¬>'.        00076500
076600             15  WT-01-MESSAGE-TEXT-043.                          00076600
076700                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00076700
076800                 20  FILLER          PIC X(1)  VALUE  '-'.        00076800
076900                 20  FILLER          PIC X(3)  VALUE  '043'.      00076900
077000                 20  FILLER          PIC X(1)  VALUE  ' '.        00077000
077100                 20  FILLER          PIC X(70) VALUE              00077100
077200                     'SELECT ONE OF THE TAB IDS TO BE MAPPED TO   00077200
077300-                    '                         '.                 00077300
077400             15  FILLER              PIC X(2)  VALUE '<¬'.        00077400
077500                                                                  00077500
077600*----------------------------------------------------------------*00077600
077700         10  WT-01-ENTRY-044.                                     00077700
077800             15  FILLER              PIC X(2)  VALUE '¬>'.        00077800
077900             15  WT-01-MESSAGE-TEXT-044.                          00077900
078000                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00078000
078100                 20  FILLER          PIC X(1)  VALUE  '-'.        00078100
078200                 20  FILLER          PIC X(3)  VALUE  '044'.      00078200
078300                 20  FILLER          PIC X(1)  VALUE  ' '.        00078300
078400                 20  FILLER          PIC X(70) VALUE              00078400
078500                     'YOU CAN WORK ON ONLY ONE TABULAR AT A TIME  00078500
078600-                    '                         '.                 00078600
078700             15  FILLER              PIC X(2)  VALUE '<¬'.        00078700
078800                                                                  00078800
078900*----------------------------------------------------------------*00078900
079000         10  WT-01-ENTRY-045.                                     00079000
079100             15  FILLER              PIC X(2)  VALUE '¬>'.        00079100
079200             15  WT-01-MESSAGE-TEXT-045.                          00079200
079300                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00079300
079400                 20  FILLER          PIC X(1)  VALUE  '-'.        00079400
079500                 20  FILLER          PIC X(3)  VALUE  '045'.      00079500
079600                 20  FILLER          PIC X(1)  VALUE  ' '.        00079600
079700                 20  FILLER          PIC X(70) VALUE              00079700
079800                     'OPTION IS INVALID                           00079800
079900-                    '                         '.                 00079900
080000             15  FILLER              PIC X(2)  VALUE '<¬'.        00080000
080100                                                                  00080100
080200*----------------------------------------------------------------*00080200
080300         10  WT-01-ENTRY-046.                                     00080300
080400             15  FILLER              PIC X(2)  VALUE '¬>'.        00080400
080500             15  WT-01-MESSAGE-TEXT-046.                          00080500
080600                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00080600
080700                 20  FILLER          PIC X(1)  VALUE  '-'.        00080700
080800                 20  FILLER          PIC X(3)  VALUE  '046'.      00080800
080900                 20  FILLER          PIC X(1)  VALUE  ' '.        00080900
081000                 20  FILLER          PIC X(70) VALUE              00081000
081100                     'SLOT NUMBER REQUIRED FOR MAPPING            00081100
081200-                    '                         '.                 00081200
081300             15  FILLER              PIC X(2)  VALUE '<¬'.        00081300
081400                                                                  00081400
081500*----------------------------------------------------------------*00081500
081600         10  WT-01-ENTRY-047.                                     00081600
081700             15  FILLER              PIC X(2)  VALUE '¬>'.        00081700
081800             15  WT-01-MESSAGE-TEXT-047.                          00081800
081900                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00081900
082000                 20  FILLER          PIC X(1)  VALUE  '-'.        00082000
082100                 20  FILLER          PIC X(3)  VALUE  '047'.      00082100
082200                 20  FILLER          PIC X(1)  VALUE  ' '.        00082200
082300                 20  FILLER          PIC X(70) VALUE              00082300
082400                     'OPTION INVALID FOR SINGLE TABULAR SUPPORT   00082400
082500-                    '                         '.                 00082500
082600             15  FILLER              PIC X(2)  VALUE '<¬'.        00082600
082700                                                                  00082700
082800*----------------------------------------------------------------*00082800
082900         10  WT-01-ENTRY-048.                                     00082900
083000             15  FILLER              PIC X(2)  VALUE '¬>'.        00083000
083100             15  WT-01-MESSAGE-TEXT-048.                          00083100
083200                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00083200
083300                 20  FILLER          PIC X(1)  VALUE  '-'.        00083300
083400                 20  FILLER          PIC X(3)  VALUE  '048'.      00083400
083500                 20  FILLER          PIC X(1)  VALUE  ' '.        00083500
083600                 20  FILLER          PIC X(70) VALUE              00083600
083700                     'ONLY PRODUCTION TABS CAN BE MAPPED DURING SI00083700
083800-                    'NGLE TABULAR SUPPORT     '.                 00083800
083900             15  FILLER              PIC X(2)  VALUE '<¬'.        00083900
084000                                                                  00084000
084100*----------------------------------------------------------------*00084100
084200         10  WT-01-ENTRY-049.                                     00084200
084300             15  FILLER              PIC X(2)  VALUE '¬>'.        00084300
084400             15  WT-01-MESSAGE-TEXT-049.                          00084400
084500                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00084500
084600                 20  FILLER          PIC X(1)  VALUE  '-'.        00084600
084700                 20  FILLER          PIC X(3)  VALUE  '049'.      00084700
084800                 20  FILLER          PIC X(1)  VALUE  ' '.        00084800
084900                 20  FILLER          PIC X(70) VALUE              00084900
085000                     'GROUP IN CONVERSION STATUS, CANNOT CHANGE HI00085000
085100-                    'GH-LIGHTED ELEMENTS      '.                 00085100
085200             15  FILLER              PIC X(2)  VALUE '<¬'.        00085200
085300                                                                  00085300
085400*----------------------------------------------------------------*00085400
085500         10  WT-01-ENTRY-050.                                     00085500
085600             15  FILLER              PIC X(2)  VALUE '¬>'.        00085600
085700             15  WT-01-MESSAGE-TEXT-050.                          00085700
085800                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00085800
085900                 20  FILLER          PIC X(1)  VALUE  '-'.        00085900
086000                 20  FILLER          PIC X(3)  VALUE  '050'.      00086000
086100                 20  FILLER          PIC X(1)  VALUE  ' '.        00086100
086200                 20  FILLER          PIC X(70) VALUE              00086200
086300                     'INTERNAL TABULAR IS REQUIRED WHEN INTERNAL D00086300
086400-                    'ESCRIPTOR IS CODED.      '.                 00086400
086500             15  FILLER              PIC X(2)  VALUE '<¬'.        00086500
086600                                                                  00086600
086700*----------------------------------------------------------------*00086700
086800         10  WT-01-ENTRY-051.                                     00086800
086900             15  FILLER              PIC X(2)  VALUE '¬>'.        00086900
087000             15  WT-01-MESSAGE-TEXT-051.                          00087000
087100                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00087100
087200                 20  FILLER          PIC X(1)  VALUE  '-'.        00087200
087300                 20  FILLER          PIC X(3)  VALUE  '051'.      00087300
087400                 20  FILLER          PIC X(1)  VALUE  ' '.        00087400
087500                 20  FILLER          PIC X(70) VALUE              00087500
087600                     'DEFINITION IS INVALID                       00087600
087700-                    '                         '.                 00087700
087800             15  FILLER              PIC X(2)  VALUE '<¬'.        00087800
087900                                                                  00087900
088000*----------------------------------------------------------------*00088000
088100         10  WT-01-ENTRY-052.                                     00088100
088200             15  FILLER              PIC X(2)  VALUE '¬>'.        00088200
088300             15  WT-01-MESSAGE-TEXT-052.                          00088300
088400                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00088400
088500                 20  FILLER          PIC X(1)  VALUE  '-'.        00088500
088600                 20  FILLER          PIC X(3)  VALUE  '052'.      00088600
088700                 20  FILLER          PIC X(1)  VALUE  ' '.        00088700
088800                 20  FILLER          PIC X(70) VALUE              00088800
088900                     'DEFINITION FIELD IS REQUIRED                00088900
089000-                    '                         '.                 00089000
089100             15  FILLER              PIC X(2)  VALUE '<¬'.        00089100
089200                                                                  00089200
089300*----------------------------------------------------------------*00089300
089400         10  WT-01-ENTRY-053.                                     00089400
089500             15  FILLER              PIC X(2)  VALUE '¬>'.        00089500
089600             15  WT-01-MESSAGE-TEXT-053.                          00089600
089700                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00089700
089800                 20  FILLER          PIC X(1)  VALUE  '-'.        00089800
089900                 20  FILLER          PIC X(3)  VALUE  '053'.      00089900
090000                 20  FILLER          PIC X(1)  VALUE  ' '.        00090000
090100                 20  FILLER          PIC X(70) VALUE              00090100
090200                     '********** F U T U R E   U S E *************00090200
090300-                    '*************************'.                 00090300
090400             15  FILLER              PIC X(2)  VALUE '<¬'.        00090400
090500                                                                  00090500
090600*----------------------------------------------------------------*00090600
090700         10  WT-01-ENTRY-054.                                     00090700
090800             15  FILLER              PIC X(2)  VALUE '¬>'.        00090800
090900             15  WT-01-MESSAGE-TEXT-054.                          00090900
091000                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00091000
091100                 20  FILLER          PIC X(1)  VALUE  '-'.        00091100
091200                 20  FILLER          PIC X(3)  VALUE  '054'.      00091200
091300                 20  FILLER          PIC X(1)  VALUE  ' '.        00091300
091400                 20  FILLER          PIC X(70) VALUE              00091400
091500                     'PERCENT REQUIRED (MUST BE GREATER THAN ZERO)00091500
091600-                    '                         '.                 00091600
091700             15  FILLER              PIC X(2)  VALUE '<¬'.        00091700
091800                                                                  00091800
091900*----------------------------------------------------------------*00091900
092000         10  WT-01-ENTRY-055.                                     00092000
092100             15  FILLER              PIC X(2)  VALUE '¬>'.        00092100
092200             15  WT-01-MESSAGE-TEXT-055.                          00092200
092300                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00092300
092400                 20  FILLER          PIC X(1)  VALUE  '-'.        00092400
092500                 20  FILLER          PIC X(3)  VALUE  '055'.      00092500
092600                 20  FILLER          PIC X(1)  VALUE  ' '.        00092600
092700                 20  FILLER          PIC X(70) VALUE              00092700
092800                     'ASCENDING/DESCENDING IS INVALID             00092800
092900-                    '                         '.                 00092900
093000             15  FILLER              PIC X(2)  VALUE '<¬'.        00093000
093100                                                                  00093100
093200*----------------------------------------------------------------*00093200
093300         10  WT-01-ENTRY-056.                                     00093300
093400             15  FILLER              PIC X(2)  VALUE '¬>'.        00093400
093500             15  WT-01-MESSAGE-TEXT-056.                          00093500
093600                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00093600
093700                 20  FILLER          PIC X(1)  VALUE  '-'.        00093700
093800                 20  FILLER          PIC X(3)  VALUE  '056'.      00093800
093900                 20  FILLER          PIC X(1)  VALUE  ' '.        00093900
094000                 20  FILLER          PIC X(70) VALUE              00094000
094100                     'MANDATORY INDICATOR IS INVALID              00094100
094200-                    '                         '.                 00094200
094300             15  FILLER              PIC X(2)  VALUE '<¬'.        00094300
094400                                                                  00094400
094500*----------------------------------------------------------------*00094500
094600         10  WT-01-ENTRY-057.                                     00094600
094700             15  FILLER              PIC X(2)  VALUE '¬>'.        00094700
094800             15  WT-01-MESSAGE-TEXT-057.                          00094800
094900                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00094900
095000                 20  FILLER          PIC X(1)  VALUE  '-'.        00095000
095100                 20  FILLER          PIC X(3)  VALUE  '057'.      00095100
095200                 20  FILLER          PIC X(1)  VALUE  ' '.        00095200
095300                 20  FILLER          PIC X(70) VALUE              00095300
095400                     'MANDATORY INDICATOR IS REQUIRED             00095400
095500-                    '                         '.                 00095500
095600             15  FILLER              PIC X(2)  VALUE '<¬'.        00095600
095700                                                                  00095700
095800*----------------------------------------------------------------*00095800
095900         10  WT-01-ENTRY-058.                                     00095900
096000             15  FILLER              PIC X(2)  VALUE '¬>'.        00096000
096100             15  WT-01-MESSAGE-TEXT-058.                          00096100
096200                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00096200
096300                 20  FILLER          PIC X(1)  VALUE  '-'.        00096300
096400                 20  FILLER          PIC X(3)  VALUE  '058'.      00096400
096500                 20  FILLER          PIC X(1)  VALUE  ' '.        00096500
096600                 20  FILLER          PIC X(70) VALUE              00096600
096700                     'CARRYOVER CREDIT INDICATOR IS INVALID       00096700
096800-                    '                         '.                 00096800
096900             15  FILLER              PIC X(2)  VALUE '<¬'.        00096900
097000                                                                  00097000
097100*----------------------------------------------------------------*00097100
097200         10  WT-01-ENTRY-059.                                     00097200
097300             15  FILLER              PIC X(2)  VALUE '¬>'.        00097300
097400             15  WT-01-MESSAGE-TEXT-059.                          00097400
097500                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00097500
097600                 20  FILLER          PIC X(1)  VALUE  '-'.        00097600
097700                 20  FILLER          PIC X(3)  VALUE  '059'.      00097700
097800                 20  FILLER          PIC X(1)  VALUE  ' '.        00097800
097900                 20  FILLER          PIC X(70) VALUE              00097900
098000                     'FIRST DOLLAR COVERAGE LIMIT INDICATOR IS INV00098000
098100-                    'ALID                     '.                 00098100
098200             15  FILLER              PIC X(2)  VALUE '<¬'.        00098200
098300*----------------------------------------------------------------*00098300
098400         10  WT-01-ENTRY-060.                                     00098400
098500             15  FILLER              PIC X(2)  VALUE '¬>'.        00098500
098600             15  WT-01-MESSAGE-TEXT-060.                          00098600
098700                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00098700
098800                 20  FILLER          PIC X(1)  VALUE  '-'.        00098800
098900                 20  FILLER          PIC X(3)  VALUE  '060'.      00098900
099000                 20  FILLER          PIC X(1)  VALUE  ' '.        00099000
099100                 20  FILLER          PIC X(70) VALUE              00099100
099200                     'PLACE OF TREATMENT IS REQUIRED              00099200
099300-                    '                         '.                 00099300
099400             15  FILLER              PIC X(2)  VALUE '<¬'.        00099400
099500*----------------------------------------------------------------*00099500
099600         10  WT-01-ENTRY-061.                                     00099600
099700             15  FILLER              PIC X(2)  VALUE '¬>'.        00099700
099800             15  WT-01-MESSAGE-TEXT-061.                          00099800
099900                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00099900
100000                 20  FILLER          PIC X(1)  VALUE  '-'.        00100000
100100                 20  FILLER          PIC X(3)  VALUE  '061'.      00100100
100200                 20  FILLER          PIC X(1)  VALUE  ' '.        00100200
100300                 20  FILLER          PIC X(70) VALUE              00100300
100400                     'BISCENDING INDICATOR INVALID--FIELD VALIDATI00100400
100500-                    'ONS SYSTEM.              '.                 00100500
100600             15  FILLER              PIC X(2)  VALUE '<¬'.        00100600
100700*----------------------------------------------------------------*00100700
100800         10  WT-01-ENTRY-062.                                     00100800
100900             15  FILLER              PIC X(2)  VALUE '¬>'.        00100900
101000             15  WT-01-MESSAGE-TEXT-062.                          00101000
101100                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00101100
101200                 20  FILLER          PIC X(1)  VALUE  '-'.        00101200
101300                 20  FILLER          PIC X(3)  VALUE  '062'.      00101300
101400                 20  FILLER          PIC X(1)  VALUE  ' '.        00101400
101500                 20  FILLER          PIC X(70) VALUE              00101500
101600                     'IF ASCEND/DESCEND IND = 3, THEN BISCENDING I00101600
101700-                    'NDICATOR MUST BE CODED.  '.                 00101700
101800             15  FILLER              PIC X(2)  VALUE '<¬'.        00101800
101900*----------------------------------------------------------------*00101900
102000         10  WT-01-ENTRY-063.                                     00102000
102100             15  FILLER              PIC X(2)  VALUE '¬>'.        00102100
102200             15  WT-01-MESSAGE-TEXT-063.                          00102200
102300                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00102300
102400                 20  FILLER          PIC X(1)  VALUE  '-'.        00102400
102500                 20  FILLER          PIC X(3)  VALUE  '063'.      00102500
102600                 20  FILLER          PIC X(1)  VALUE  ' '.        00102600
102700                 20  FILLER          PIC X(70) VALUE              00102700
102800                     'IF BISCENDING INDICATOR IS CODED, THEN ASCEN00102800
102900-                    'D/DESCEND IND MUST = 3.  '.                 00102900
103000             15  FILLER              PIC X(2)  VALUE '<¬'.        00103000
103100*----------------------------------------------------------------*00103100
103200         10  WT-01-ENTRY-064.                                     00103200
103300             15  FILLER              PIC X(2)  VALUE '¬>'.        00103300
103400             15  WT-01-MESSAGE-TEXT-064.                          00103400
103500                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00103500
103600                 20  FILLER          PIC X(1)  VALUE  '-'.        00103600
103700                 20  FILLER          PIC X(3)  VALUE  '064'.      00103700
103800                 20  FILLER          PIC X(1)  VALUE  ' '.        00103800
103900                 20  FILLER          PIC X(70) VALUE              00103900
104000                     'BENFT PERIOD IND = 0C, BEN-QUALIFIER AND BEN00104000
104100-                    '-FACTOR CAN NOT BE ZEROS  '.                00104100
104200             15  FILLER              PIC X(2)  VALUE '<¬'.        00104200
104300                                                                  00104300
104400*----------------------------------------------------------------*00104400
104500         10  WT-01-ENTRY-065.                                     00104500
104600             15  FILLER              PIC X(2)  VALUE '¬>'.        00104600
104700             15  WT-01-MESSAGE-TEXT-065.                          00104700
104800                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00104800
104900                 20  FILLER          PIC X(1)  VALUE  '-'.        00104900
105000                 20  FILLER          PIC X(3)  VALUE  '065'.      00105000
105100                 20  FILLER          PIC X(1)  VALUE  ' '.        00105100
105200                 20  FILLER          PIC X(70) VALUE              00105200
105300                     'IF BENFT-PERIOD-IND = 0V OR 0W, VALUE-QUALIF00105300
105400-                    'IER CAN ONLY BE = 6 OR 9  '.                00105400
105500             15  FILLER              PIC X(2)  VALUE '<¬'.        00105500
105600                                                                  00105600
105700*----------------------------------------------------------------*00105700
105800         10  WT-01-ENTRY-066.                                     00105800
105900             15  FILLER              PIC X(2)  VALUE '¬>'.        00105900
106000             15  WT-01-MESSAGE-TEXT-066.                          00106000
106100                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00106100
106200                 20  FILLER          PIC X(1)  VALUE  '-'.        00106200
106300                 20  FILLER          PIC X(3)  VALUE  '066'.      00106300
106400                 20  FILLER          PIC X(1)  VALUE  ' '.        00106400
106500                 20  FILLER          PIC X(70) VALUE              00106500
106600                     'IF VALUE-QUALIFIER = 6 OR 9, BENEFIT-PERIOD 00106600
106700-                    'CAN ONLY BE = 0V OR 0W    '.                00106700
106800             15  FILLER              PIC X(2)  VALUE '<¬'.        00106800
106900                                                                  00106900
107000*----------------------------------------------------------------*00107000
107100         10  WT-01-ENTRY-067.                                     00107100
107200             15  FILLER              PIC X(2)  VALUE '¬>'.        00107200
107300             15  WT-01-MESSAGE-TEXT-067.                          00107300
107400                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00107400
107500                 20  FILLER          PIC X(1)  VALUE  '-'.        00107500
107600                 20  FILLER          PIC X(3)  VALUE  '067'.      00107600
107700                 20  FILLER          PIC X(1)  VALUE  ' '.        00107700
107800                 20  FILLER          PIC X(70) VALUE              00107800
107900                     'RELATIONSHIP INDICATOR IS INVALID           00107900
108000-                    '                         '.                 00108000
108100             15  FILLER              PIC X(2)  VALUE '<¬'.        00108100
108200                                                                  00108200
108300*----------------------------------------------------------------*00108300
108400         10  WT-01-ENTRY-068.                                     00108400
108500             15  FILLER              PIC X(2)  VALUE '¬>'.        00108500
108600             15  WT-01-MESSAGE-TEXT-068.                          00108600
108700                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00108700
108800                 20  FILLER          PIC X(1)  VALUE  '-'.        00108800
108900                 20  FILLER          PIC X(3)  VALUE  '068'.      00108900
109000                 20  FILLER          PIC X(1)  VALUE  ' '.        00109000
109100                 20  FILLER          PIC X(70) VALUE              00109100
109200                     'AGE LIMIT FIELD MUST BE NUMERIC ONLY, ANY OT00109200
109300-                    'HER CHAR. IS INVALID     '.                 00109300
109400             15  FILLER              PIC X(2)  VALUE '<¬'.        00109400
109500                                                                  00109500
109600*----------------------------------------------------------------*00109600
109700         10  WT-01-ENTRY-069.                                     00109700
109800             15  FILLER              PIC X(2)  VALUE '¬>'.        00109800
109900             15  WT-01-MESSAGE-TEXT-069.                          00109900
110000                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00110000
110100                 20  FILLER          PIC X(1)  VALUE  '-'.        00110100
110200                 20  FILLER          PIC X(3)  VALUE  '069'.      00110200
110300                 20  FILLER          PIC X(1)  VALUE  ' '.        00110300
110400                 20  FILLER          PIC X(70) VALUE              00110400
110500                     'AGE LIMIT-FROM MUST BE LESS THAN AGE LIMIT-T00110500
110600-                    'O, OR QUALI - REENTER    '.                 00110600
110700             15  FILLER              PIC X(2)  VALUE '<¬'.        00110700
110800                                                                  00110800
110900*----------------------------------------------------------------*00110900
111000         10  WT-01-ENTRY-070.                                     00111000
111100             15  FILLER              PIC X(2)  VALUE '¬>'.        00111100
111200             15  WT-01-MESSAGE-TEXT-070.                          00111200
111300                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00111300
111400                 20  FILLER          PIC X(1)  VALUE  '-'.        00111400
111500                 20  FILLER          PIC X(3)  VALUE  '070'.      00111500
111600                 20  FILLER          PIC X(1)  VALUE  ' '.        00111600
111700                 20  FILLER          PIC X(70) VALUE              00111700
111800                     'AGE LIMIT FIELDS ERROR, IF ONE IS CODED THE 00111800
111900-                    'OTHER MUST BE CODED      '.                 00111900
112000             15  FILLER              PIC X(2)  VALUE '<¬'.        00112000
112100                                                                  00112100
112200*----------------------------------------------------------------*00112200
112300         10  WT-01-ENTRY-071.                                     00112300
112400             15  FILLER              PIC X(2)  VALUE '¬>'.        00112400
112500             15  WT-01-MESSAGE-TEXT-071.                          00112500
112600                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00112600
112700                 20  FILLER          PIC X(1)  VALUE  '-'.        00112700
112800                 20  FILLER          PIC X(3)  VALUE  '071'.      00112800
112900                 20  FILLER          PIC X(1)  VALUE  ' '.        00112900
113000                 20  FILLER          PIC X(70) VALUE              00113000
113100                     ' AGE QUALIFIER INDICATOR IS INVALID         00113100
113200-                    '                         '.                 00113200
113300             15  FILLER              PIC X(2)  VALUE '<¬'.        00113300
113400                                                                  00113400
113500*----------------------------------------------------------------*00113500
113600         10  WT-01-ENTRY-072.                                     00113600
113700             15  FILLER              PIC X(2)  VALUE '¬>'.        00113700
113800             15  WT-01-MESSAGE-TEXT-072.                          00113800
113900                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00113900
114000                 20  FILLER          PIC X(1)  VALUE  '-'.        00114000
114100                 20  FILLER          PIC X(3)  VALUE  '072'.      00114100
114200                 20  FILLER          PIC X(1)  VALUE  ' '.        00114200
114300                 20  FILLER          PIC X(70) VALUE              00114300
114400                     ' IF AGE LIMIT FIELDS ARE CODED, AGE QUALIFIE00114400
114500-                    'R FIELDS MUST BE CODED   '.                 00114500
114600             15  FILLER              PIC X(2)  VALUE '<¬'.        00114600
114700                                                                  00114700
114800*----------------------------------------------------------------*00114800
114900         10  WT-01-ENTRY-073.                                     00114900
115000             15  FILLER              PIC X(2)  VALUE '¬>'.        00115000
115100             15  WT-01-MESSAGE-TEXT-073.                          00115100
115200                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00115200
115300                 20  FILLER          PIC X(1)  VALUE  '-'.        00115300
115400                 20  FILLER          PIC X(3)  VALUE  '073'.      00115400
115500                 20  FILLER          PIC X(1)  VALUE  ' '.        00115500
115600                 20  FILLER          PIC X(70) VALUE              00115600
115700                     ' IF AGE QUALIFIERS ARE CODED, AGE LIMIT FIEL00115700
115800-                    'DS MUST BE CODED         '.                 00115800
115900             15  FILLER              PIC X(2)  VALUE '<¬'.        00115900
116000                                                                  00116000
116100*----------------------------------------------------------------*00116100
116200         10  WT-01-ENTRY-074.                                     00116200
116300             15  FILLER              PIC X(2)  VALUE '¬>'.        00116300
116400             15  WT-01-MESSAGE-TEXT-074.                          00116400
116500                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00116500
116600                 20  FILLER          PIC X(1)  VALUE  '-'.        00116600
116700                 20  FILLER          PIC X(3)  VALUE  '074'.      00116700
116800                 20  FILLER          PIC X(1)  VALUE  ' '.        00116800
116900                 20  FILLER          PIC X(70) VALUE              00116900
117000                     ' IF AGE QUALIFIER-IND CODED BOTH INDICATORS 00117000
117100-                    ' FIELDS MUST BE CODED    '.                 00117100
117200             15  FILLER              PIC X(2)  VALUE '<¬'.        00117200
117300                                                                  00117300
117400*----------------------------------------------------------------*00117400
117500         10  WT-01-ENTRY-075.                                     00117500
117600             15  FILLER              PIC X(2)  VALUE '¬>'.        00117600
117700             15  WT-01-MESSAGE-TEXT-075.                          00117700
117800                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00117800
117900                 20  FILLER          PIC X(1)  VALUE  '-'.        00117900
118000                 20  FILLER          PIC X(3)  VALUE  '075'.      00118000
118100                 20  FILLER          PIC X(1)  VALUE  ' '.        00118100
118200                 20  FILLER          PIC X(70) VALUE              00118200
118300                     ' THIS INTERNAL DESCRIPTOR CANNOT BE USED WIT00118300
118400-                    'H AGE RANGES             '.                 00118400
118500             15  FILLER              PIC X(2)  VALUE '<¬'.        00118500
118600                                                                  00118600
118700*----------------------------------------------------------------*00118700
118800         10  WT-01-ENTRY-076.                                     00118800
118900             15  FILLER              PIC X(2)  VALUE '¬>'.        00118900
119000             15  WT-01-MESSAGE-TEXT-076.                          00119000
119100                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00119100
119200                 20  FILLER          PIC X(1)  VALUE  '-'.        00119200
119300                 20  FILLER          PIC X(3)  VALUE  '076'.      00119300
119400                 20  FILLER          PIC X(1)  VALUE  ' '.        00119400
119500                 20  FILLER          PIC X(70) VALUE              00119500
119600                     ' THIS INTERNAL DESCRIPTOR IS INVALID WITHOUT00119600
119700-                    ' AGE RANGES CODED        '.                 00119700
119800             15  FILLER              PIC X(2)  VALUE '<¬'.        00119800
119900                                                                  00119900
120000*----------------------------------------------------------------*00120000
120100         10  WT-01-ENTRY-077.                                     00120100
120200             15  FILLER              PIC X(2)  VALUE '¬>'.        00120200
120300             15  WT-01-MESSAGE-TEXT-077.                          00120300
120400                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00120400
120500                 20  FILLER          PIC X(1)  VALUE  '-'.        00120500
120600                 20  FILLER          PIC X(3)  VALUE  '077'.      00120600
120700                 20  FILLER          PIC X(1)  VALUE  ' '.        00120700
120800                 20  FILLER          PIC X(70) VALUE              00120800
120900                     ' INTERNAL DESCRIPTOR MUST BE CODED WHEN AGE 00120900
121000-                    'RANGES ARE CODED         '.                 00121000
121100             15  FILLER              PIC X(2)  VALUE '<¬'.        00121100
121200                                                                  00121200
121300*----------------------------------------------------------------*00121300
121400         10  WT-01-ENTRY-078.                                     00121400
121500             15  FILLER              PIC X(2)  VALUE '¬>'.        00121500
121600             15  WT-01-MESSAGE-TEXT-078.                          00121600
121700                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00121700
121800                 20  FILLER          PIC X(1)  VALUE  '-'.        00121800
121900                 20  FILLER          PIC X(3)  VALUE  '078'.      00121900
122000                 20  FILLER          PIC X(1)  VALUE  ' '.        00122000
122100                 20  FILLER          PIC X(70) VALUE              00122100
122200                     'FEAK INDICATOR IS INVALID                   00122200
122300-                    '                         '.                 00122300
122400             15  FILLER              PIC X(2)  VALUE '<¬'.        00122400
122500                                                                  00122500
122600*----------------------------------------------------------------*00122600
122700         10  WT-01-ENTRY-079.                                     00122700
122800             15  FILLER              PIC X(2)  VALUE '¬>'.        00122800
122900             15  WT-01-MESSAGE-TEXT-079.                          00122900
123000                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00123000
123100                 20  FILLER          PIC X(1)  VALUE  '-'.        00123100
123200                 20  FILLER          PIC X(3)  VALUE  '079'.      00123200
123300                 20  FILLER          PIC X(1)  VALUE  ' '.        00123300
123400                 20  FILLER          PIC X(70) VALUE              00123400
123500                     'TIME & DOLLAR FIELD IS INVALID              00123500
123600-                    '                         '.                 00123600
123700             15  FILLER              PIC X(2)  VALUE '<¬'.        00123700
123800                                                                  00123800
123900*----------------------------------------------------------------*00123900
124000         10  WT-01-ENTRY-080.                                     00124000
124100             15  FILLER              PIC X(2)  VALUE '¬>'.        00124100
124200             15  WT-01-MESSAGE-TEXT-080.                          00124200
124300                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00124300
124400                 20  FILLER          PIC X(1)  VALUE  '-'.        00124400
124500                 20  FILLER          PIC X(3)  VALUE  '080'.      00124500
124600                 20  FILLER          PIC X(1)  VALUE  ' '.        00124600
124700                 20  FILLER          PIC X(70) VALUE              00124700
124800                     'TIME & DOLLAR FIELD IS REQUIRED             00124800
124900-                    '                         '.                 00124900
125000             15  FILLER              PIC X(2)  VALUE '<¬'.        00125000
125100                                                                  00125100
125200*----------------------------------------------------------------*00125200
125300         10  WT-01-ENTRY-081.                                     00125300
125400             15  FILLER              PIC X(2)  VALUE '¬>'.        00125400
125500             15  WT-01-MESSAGE-TEXT-081.                          00125500
125600                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00125600
125700                 20  FILLER          PIC X(1)  VALUE  '-'.        00125700
125800                 20  FILLER          PIC X(3)  VALUE  '081'.      00125800
125900                 20  FILLER          PIC X(1)  VALUE  ' '.        00125900
126000                 20  FILLER          PIC X(70) VALUE              00126000
126100                     'ACCUMULATOR IDENTIFIER IS INVALID           00126100
126200-                    '                         '.                 00126200
126300             15  FILLER              PIC X(2)  VALUE '<¬'.        00126300
126400                                                                  00126400
126500*----------------------------------------------------------------*00126500
126600         10  WT-01-ENTRY-082.                                     00126600
126700             15  FILLER              PIC X(2)  VALUE '¬>'.        00126700
126800             15  WT-01-MESSAGE-TEXT-082.                          00126800
126900                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00126900
127000                 20  FILLER          PIC X(1)  VALUE  '-'.        00127000
127100                 20  FILLER          PIC X(3)  VALUE  '082'.      00127100
127200                 20  FILLER          PIC X(1)  VALUE  ' '.        00127200
127300                 20  FILLER          PIC X(70) VALUE              00127300
127400                     'COMBINATION APPLIED INDICATOR IS INVALID    00127400
127500-                    '                         '.                 00127500
127600             15  FILLER              PIC X(2)  VALUE '<¬'.        00127600
127700                                                                  00127700
127800*----------------------------------------------------------------*00127800
127900         10  WT-01-ENTRY-083.                                     00127900
128000             15  FILLER              PIC X(2)  VALUE '¬>'.        00128000
128100             15  WT-01-MESSAGE-TEXT-083.                          00128100
128200                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00128200
128300                 20  FILLER          PIC X(1)  VALUE  '-'.        00128300
128400                 20  FILLER          PIC X(3)  VALUE  '083'.      00128400
128500                 20  FILLER          PIC X(1)  VALUE  ' '.        00128500
128600                 20  FILLER          PIC X(70) VALUE              00128600
128700                     '1ST DOLLAR COVERAGE IND MUST = A IF CAPI IS 00128700
128800-                    'CODED                    '.                 00128800
128900             15  FILLER              PIC X(2)  VALUE '<¬'.        00128900
129000                                                                  00129000
129100*----------------------------------------------------------------*00129100
129200         10  WT-01-ENTRY-084.                                     00129200
129300             15  FILLER              PIC X(2)  VALUE '¬>'.        00129300
129400             15  WT-01-MESSAGE-TEXT-084.                          00129400
129500                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00129500
129600                 20  FILLER          PIC X(1)  VALUE  '-'.        00129600
129700                 20  FILLER          PIC X(3)  VALUE  '084'.      00129700
129800                 20  FILLER          PIC X(1)  VALUE  ' '.        00129800
129900                 20  FILLER          PIC X(70) VALUE              00129900
130000                     'SELECTIVE ADDITIONAL BENEFIT DETERMINATION  00130000
130100-                    'IS INVALID               '.                 00130100
130200             15  FILLER              PIC X(2)  VALUE '<¬'.        00130200
130300                                                                  00130300
130400*----------------------------------------------------------------*00130400
130500         10  WT-01-ENTRY-085.                                     00130500
130600             15  FILLER              PIC X(2)  VALUE '¬>'.        00130600
130700             15  WT-01-MESSAGE-TEXT-085.                          00130700
130800                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00130800
130900                 20  FILLER          PIC X(1)  VALUE  '-'.        00130900
131000                 20  FILLER          PIC X(3)  VALUE  '085'.      00131000
131100                 20  FILLER          PIC X(1)  VALUE  ' '.        00131100
131200                 20  FILLER          PIC X(70) VALUE              00131200
131300                     'BENEFIT PERIOD IS INCONSISTENT WITH SABD -  00131300
131400-                    'BENEFIT PERIOD MUST = CA '.                 00131400
131500             15  FILLER              PIC X(2)  VALUE '<¬'.        00131500
131600                                                                  00131600
131700*----------------------------------------------------------------*00131700
131800         10  WT-01-ENTRY-086.                                     00131800
131900             15  FILLER              PIC X(2)  VALUE '¬>'.        00131900
132000             15  WT-01-MESSAGE-TEXT-086.                          00132000
132100                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00132100
132200                 20  FILLER          PIC X(1)  VALUE  '-'.        00132200
132300                 20  FILLER          PIC X(3)  VALUE  '086'.      00132300
132400                 20  FILLER          PIC X(1)  VALUE  ' '.        00132400
132500                 20  FILLER          PIC X(70) VALUE              00132500
132600                     'SABD VALID ONLY ON #ACP TAB                 00132600
132700-                    '                         '.                 00132700
132800             15  FILLER              PIC X(2)  VALUE '<¬'.        00132800
132900                                                                  00132900
133000*----------------------------------------------------------------*00133000
121400         10  WT-01-ENTRY-087.                                     00133010
121500             15  FILLER              PIC X(2)  VALUE '¬>'.        00133020
121600             15  WT-01-MESSAGE-TEXT-087.                          00133030
121700                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00133040
121800                 20  FILLER          PIC X(1)  VALUE  '-'.        00133050
121900                 20  FILLER          PIC X(3)  VALUE  '087'.      00133060
122000                 20  FILLER          PIC X(1)  VALUE  ' '.        00133070
122100                 20  FILLER          PIC X(70) VALUE              00133080
122200                     'TIER CODE IS INVALID                        00133090
122300-                    '                         '.                 00133091
122400             15  FILLER              PIC X(2)  VALUE '<¬'.        00133092
122500                                                                  00133093
133000*----------------------------------------------------------------*00133094
121400         10  WT-01-ENTRY-088.                                     00133095
121500             15  FILLER              PIC X(2)  VALUE '¬>'.        00133096
121600             15  WT-01-MESSAGE-TEXT-088.                          00133097
121700                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00133098
121800                 20  FILLER          PIC X(1)  VALUE  '-'.        00133099
121900                 20  FILLER          PIC X(3)  VALUE  '088'.      00133100
122000                 20  FILLER          PIC X(1)  VALUE  ' '.        00133101
122100                 20  FILLER          PIC X(70) VALUE              00133102
122200                     'TIER LEVEL IS INVALID                       00133103
122300-                    '                         '.                 00133104
122400             15  FILLER              PIC X(2)  VALUE '<¬'.        00133105
133100                                                                  00133110
086700*----------------------------------------------------------------*00133120
086800         10  WT-01-ENTRY-089.                                     00133130
086900             15  FILLER              PIC X(2)  VALUE '¬>'.        00133140
087000             15  WT-01-MESSAGE-TEXT-089.                          00133150
087100                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00133160
087200                 20  FILLER          PIC X(1)  VALUE  '-'.        00133170
087300                 20  FILLER          PIC X(3)  VALUE  '089'.      00133180
087400                 20  FILLER          PIC X(1)  VALUE  ' '.        00133190
087500                 20  FILLER          PIC X(70) VALUE              00133191
087600                     'BENEFIT TYPE IS INVALID                     00133192
087700-                    '                         '.                 00133193
087800             15  FILLER              PIC X(2)  VALUE '<¬'.        00133194
087900                                                                  00133195
088000*----------------------------------------------------------------*00133196
088100         10  WT-01-ENTRY-090.                                     00133197
088200             15  FILLER              PIC X(2)  VALUE '¬>'.        00133198
088300             15  WT-01-MESSAGE-TEXT-090.                          00133199
088400                 20  FILLER          PIC X(4)  VALUE  'GASE'.     00133200
088500                 20  FILLER          PIC X(1)  VALUE  '-'.        00133201
088600                 20  FILLER          PIC X(3)  VALUE  '090'.      00133202
088700                 20  FILLER          PIC X(1)  VALUE  ' '.        00133203
088800                 20  FILLER          PIC X(70) VALUE              00133204
088900                     'BENEFIT TYPE FIELD IS REQUIRED              00133205
089000-                    '                         '.                 00133206
089100             15  FILLER              PIC X(2)  VALUE '<¬'.        00133207
089200                                                                  00133208
133200     05  WT-01-MESSAGE-TABLE         REDEFINES                    00133210
133300         WT-01-MESSAGE-VALUES         OCCURS 090 TIMES            00133300
133400                                     INDEXED BY WT-01-INDEX.      00133400
133500         10  WT-01-ENTRY.                                         00133500
133600             15  FILLER              PIC X(02).                   00133600
133700             15  WT-01-MESSAGE-TEXT  PIC X(79).                   00133700
133800             15  FILLER              PIC X(02).                   00133800
133900                                                                  00133900
134000                                                                  00134000
134100/*** MAP FIELD ATTRIBUTES                                         00134100
134200 COPY DFHBMSCA.                                                   00134200
134300*                         AUTOSKIP, BRIGHT, FSET                  00134300
134400     02  DFHBMABF         PIC X  VALUE '9'.                       00134400
134500                                                                  00134500
134600/*** ATTENTION KEYS                                               00134600
134700 COPY DFHAID.                                                     00134700
134800                                                                  00134800
134900/*** ALTERNATIVE WORKFILE KEYS                                    00134900
135000 01  FILLER.                                                      00135000
135100     COPY GCWRKKEY.                                               00135100
135200                                                                  00135200
135300/*** GENERIC CONTRACT GLOBALLY DEFINED LENGTHS                    00135300
135400 01  FILLER.                                                      00135400
135500     COPY GCCDRLEN.                                               00135500
135600                                                                  00135600
135700                                                                  00135700
135800 01  WS-END                       PIC X(58) VALUE                 00135800
135900     '*** GASEDIT1  WORKING-STORAGE ENDS HERE ***'.               00135900
136000/                                                                 00136000
136100 LINKAGE SECTION.                                                 00136100
136200/                                                                 00136200
136300 01  DFHCOMMAREA.                                                 00136300
136400                                                                  00136400
136500**** COMMON WORKAREAS PASSED TO AND FROM THIS PROGRAM  ***        00136500
136600     COPY G2ALCKEC.                                               00136600
136700     COPY GACDACWA.                                               00136700
136800         05  GAS1UPD-PASSED-AREA.                                 00136800
136900             07  LVL2-B-SW         PIC X.                         00136900
137000             07  LVL2-F-SW         PIC X.                         00137000
137100             07  LVL2-G-SW         PIC X.                         00137100
137200             07  INTR-TAB-PGM-ID   PIC X(8).                      00137200
137300             07  FILLER            PIC X(9).                      00137300
137400         05  DELADD-OPTION         PIC X(7).                      00137400
137500                                                                  00137500
137600/*** MAPSET PASSED TO THIS PROGRAM ***                            00137600
137700                                                                  00137700
137800     COPY GA1XSETC.                                               00137800
137900/                                                                 00137900
138000 PROCEDURE DIVISION.                                              00138000
138100                                                                  00138100
138200****************************************************************  00138200
138300*                                                              *  00138300
138400*           P R O C E S S     C O N T R O L                    *  00138400
138500*                                                              *  00138500
138600****************************************************************  00138600
138700 0000-000-PROCESS-CONTROL       SECTION.                          00138700
138800 0000-010.                                                        00138800
138900                                                                  00138900
139000     SET ADDRESS OF  GA1XI01I  TO  ACWA-MAPSET-PNTR.              00139000
139100                                                                  00139100
139200     PERFORM  1100-000-VALIDATE-SCREEN.                           00139200
139300                                                                  00139300
139400     EXEC CICS RETURN END-EXEC.                                   00139400
139500                                                                  00139500
139600     GOBACK.                                                      00139600
139700                                                                  00139700
139800                                                                  00139800
139900 0000-900-EXIT.                                                   00139900
140000     EXIT.                                                        00140000
140100/*****************************************************************00140100
140200*                                                                *00140200
140300* 1100  VALIDATE SCREEN                                          *00140300
140400*                                                                *00140400
140500*    THIS IS PRIMARILY A VALIDATION ROUTINE OF DATA BEING ENTERED*00140500
140600*  BY THE OPERATOR, PLUS THE ADDITION OF SOME REINITIALIZATION.  *00140600
140700*  1. REINITIALIZE ATTRIBUTES THAT THE PROGRAM MIGHT MODIFY, AND *00140700
140800*     RESET THE ERROR MESSAGE AND DELETE OPTION TO BLANKS.       *00140800
140900*  2. INSURE THE VALIDITY OF THE OPTIONS THAT CAN BE USED FOR THE*00140900
141000*     INTERNAL TABULAR.                                          *00141000
141100*                                                                *00141100
141200******************************************************************00141200
141300 1100-000-VALIDATE-SCREEN       SECTION.                          00141300
141400 1100-010.                                                        00141400
141500                                                                  00141500
141600                                                                  00141600
141700*------------------ DELETE OPTION -------------------------------*00141700
141800                                                                  00141800
141900     IF   DELOPTNL > ZERO             AND                         00141900
142000         (DELOPTNI  NOT =  SPACE AND  'D')                        00142000
142100     THEN                                                         00142100
142200         MOVE -1        TO  DELOPTNL                              00142200
142300         MOVE DFHBMUBF  TO  DELOPTNA                              00142300
142400         IF  ACWA-SCREEN-HAS-ERRORS                               00142400
142500         THEN                                                     00142500
142600             MOVE 'Y'        TO ACWA-ERROR-SW                     00142600
142700             SET WT-01-INDEX TO +02                               00142700
142800         ELSE                                                     00142800
142900             NEXT SENTENCE                                        00142900
143000     ELSE                                                         00143000
143100         NEXT SENTENCE.                                           00143100
143200                                                                  00143200
143300                                                                  00143300
143400                                                                  00143400
143500*------------------ BENEFIT PERIOD ------------------------------*00143500
143600                                                                  00143600
143700     IF  PERIODL > ZERO                                           00143700
143800     THEN                                                         00143800
143900         MOVE 'ACCM02'  TO GCVI-FIELDS-KEY-ID                     00143900
144000         MOVE PERIODI   TO GCVI-VALUE-LEN-2                       00144000
144100         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00144100
144200         IF  GCVI-VALUE-NOT-FOUND                                 00144200
144300         THEN                                                     00144300
144400             MOVE DFHBMUBF TO PERIODA                             00144400
144500             MOVE -1       TO PERIODL                             00144500
144600             IF  ACWA-SCREEN-HAS-ERRORS                           00144600
144700             THEN                                                 00144700
144800                 NEXT SENTENCE                                    00144800
144900             ELSE                                                 00144900
145000                 MOVE 'Y'        TO ACWA-ERROR-SW                 00145000
145100                 SET WT-01-INDEX TO +05                           00145100
145200         ELSE                                                     00145200
145300             IF  GCVI-VALUE-NOT-LOADED                            00145300
145400             THEN                                                 00145400
145500                 MOVE DFHBMUBF TO PERIODA                         00145500
145600                 IF  GCVI-TABLE-SW = 'Y'                          00145600
145700                 THEN                                             00145700
145800                     MOVE 'N'      TO GCVI-TABLE-SW               00145800
145900                     MOVE -1       TO PERIODL                     00145900
146000                 ELSE                                             00146000
146100                     NEXT SENTENCE                                00146100
146200             ELSE                                                 00146200
146300                 NEXT SENTENCE                                    00146300
146400     ELSE                                                         00146400
146500         MOVE DFHBMUBF   TO  PERIODA                              00146500
146600         MOVE -1         TO  PERIODL                              00146600
146700         IF  ACWA-SCREEN-HAS-ERRORS                               00146700
146800         THEN                                                     00146800
146900             NEXT SENTENCE                                        00146900
147000         ELSE                                                     00147000
147100             MOVE 'Y'        TO ACWA-ERROR-SW                     00147100
147200             SET WT-01-INDEX TO +06.                              00147200
147300                                                                  00147300
147400                                                                  00147400
147500*------------------ PERIOD TIME QUALIFIER -----------------------*00147500
147600                                                                  00147600
147700     IF  PERTQALL NOT > ZERO                                      00147700
147800         MOVE ZEROS TO PERTQALO.                                  00147800
147900                                                                  00147900
148000     IF  PERTQALL > ZERO                                          00148000
148100     THEN                                                         00148100
148200         MOVE 'ACCM06'  TO GCVI-FIELDS-KEY-ID                     00148200
148300         MOVE PERTQALI  TO GCVI-VALUE-LEN-1                       00148300
148400         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00148400
148500         IF  GCVI-VALUE-NOT-FOUND                                 00148500
148600         THEN                                                     00148600
148700             MOVE DFHBMUBF TO PERTQALA                            00148700
148800             MOVE -1       TO PERTQALL                            00148800
148900             IF  ACWA-SCREEN-HAS-ERRORS                           00148900
149000             THEN                                                 00149000
149100                 NEXT SENTENCE                                    00149100
149200             ELSE                                                 00149200
149300                 MOVE 'Y'        TO ACWA-ERROR-SW                 00149300
149400                 SET WT-01-INDEX TO +07                           00149400
149500         ELSE                                                     00149500
149600             IF  GCVI-VALUE-NOT-LOADED                            00149600
149700             THEN                                                 00149700
149800                 MOVE DFHBMUBF TO PERTQALA                        00149800
149900                 IF  GCVI-TABLE-SW = 'Y'                          00149900
150000                 THEN                                             00150000
150100                     MOVE 'N'    TO GCVI-TABLE-SW                 00150100
150200                     MOVE -1     TO PERTQALL                      00150200
150300                 ELSE                                             00150300
150400                     NEXT SENTENCE                                00150400
150500             ELSE                                                 00150500
150600                 NEXT SENTENCE                                    00150600
150700     ELSE                                                         00150700
150800         MOVE ZEROS TO PERTQALO.                                  00150800
150900                                                                  00150900
151000                                                                  00151000
151100*------------------ PERIOD TIME FACTOR --------------------------*00151100
151200                                                                  00151200
151300     IF  PRTIMEFL  > ZERO                                         00151300
151400     THEN                                                         00151400
151500         IF  PRTIMEFI  NOT  NUMERIC                               00151500
151600         THEN                                                     00151600
151700             MOVE -1        TO  PRTIMEFL                          00151700
151800             MOVE DFHBMUBF  TO  PRTIMEFA                          00151800
151900             IF  ACWA-SCREEN-HAS-ERRORS                           00151900
152000             THEN                                                 00152000
152100                 NEXT SENTENCE                                    00152100
152200             ELSE                                                 00152200
152300                 SET WT-01-INDEX TO +08                           00152300
152400                 MOVE 'Y'        TO ACWA-ERROR-SW                 00152400
152500         ELSE                                                     00152500
152600             NEXT SENTENCE                                        00152600
152700     ELSE                                                         00152700
152800         MOVE ZEROS TO PRTIMEFO.                                  00152800
152900                                                                  00152900
153000*------------------- LOGICAL EDIT ------------------------------* 00153000
153100*                                                               * 00153100
153200* LOGICAL EDIT COMPARING BENEFIT PERIOD TIME QUALIFIER AND      * 00153200
153300*   AND BENEFIT PERIOD TIME FACTOR                              * 00153300
153400*                                                               * 00153400
153500*---------------------------------------------------------------* 00153500
153600                                                                  00153600
153700     IF  PERTQALI > ZEROS                                         00153700
153800     THEN                                                         00153800
153900         IF  PRTIMEFI > ZEROS                                     00153900
154000         THEN                                                     00154000
154100             NEXT SENTENCE                                        00154100
154200         ELSE                                                     00154200
154300             MOVE DFHBMUBF TO PRTIMEFA                            00154300
154400             MOVE -1       TO PRTIMEFL                            00154400
154500             IF  ACWA-SCREEN-HAS-ERRORS                           00154500
154600             THEN                                                 00154600
154700                 NEXT SENTENCE                                    00154700
154800             ELSE                                                 00154800
154900                 MOVE 'Y'        TO ACWA-ERROR-SW                 00154900
155000                 SET WT-01-INDEX TO +09                           00155000
155100     ELSE                                                         00155100
155200         NEXT SENTENCE.                                           00155200
155300                                                                  00155300
155400     IF  PRTIMEFI > ZEROS                                         00155400
155500     THEN                                                         00155500
155600         IF  PERTQALI = ZEROS                                     00155600
155700         THEN                                                     00155700
155800             MOVE DFHBMUBF TO PERTQALA                            00155800
155900             MOVE -1       TO PERTQALL                            00155900
156000             IF  ACWA-SCREEN-HAS-ERRORS                           00156000
156100             THEN                                                 00156100
156200                 NEXT SENTENCE                                    00156200
156300             ELSE                                                 00156300
156400                 MOVE 'Y'        TO ACWA-ERROR-SW                 00156400
156500                 SET WT-01-INDEX TO +10                           00156500
156600         ELSE                                                     00156600
156700             NEXT SENTENCE                                        00156700
156800     ELSE                                                         00156800
156900         NEXT SENTENCE.                                           00156900
157000                                                                  00157000
157100                                                                  00157100
157200*------------------- LOGICAL EDIT D242--------------------------* 00157200
157300*                                                               * 00157300
157400* LOGICAL EDIT COMPARING BENEFIT PERIOD INDICATOR, TIME         * 00157400
157500*   QUALIFIER AND BENEFIT PERIOD TIME FACTOR.    02/22/90       * 00157500
157600*                                                               * 00157600
157700*---------------------------------------------------------------* 00157700
157800                                                                  00157800
157900     IF  PERIODI  = '0C'                                          00157900
158000     THEN                                                         00158000
158100        IF  PERTQALI = '0'                                        00158100
158200            MOVE DFHBMUBF TO PERTQALA                             00158200
158300            MOVE -1       TO PERTQALL                             00158300
158400            IF  ACWA-SCREEN-HAS-ERRORS                            00158400
158500            THEN                                                  00158500
158600                NEXT SENTENCE                                     00158600
158700            ELSE                                                  00158700
158800                MOVE 'Y'        TO ACWA-ERROR-SW                  00158800
158900                SET WT-01-INDEX TO +64                            00158900
159000        ELSE                                                      00159000
159100           IF  PRTIMEFI = '000'                                   00159100
159200               MOVE DFHBMUBF TO PRTIMEFA                          00159200
159300               MOVE -1       TO PRTIMEFL                          00159300
159400               IF  ACWA-SCREEN-HAS-ERRORS                         00159400
159500               THEN                                               00159500
159600                   NEXT SENTENCE                                  00159600
159700               ELSE                                               00159700
159800                   MOVE 'Y'        TO ACWA-ERROR-SW               00159800
159900                   SET WT-01-INDEX TO +64.                        00159900
160000                                                                  00160000
160100                                                                  00160100
160200*------------------ DAY FACTOR INDICATOR ------------------------*00160200
160300                                                                  00160300
160400     IF  DAYFACIL > ZERO                                          00160400
160500     THEN                                                         00160500
160600         MOVE 'ACCM15'  TO GCVI-FIELDS-KEY-ID                     00160600
160700         MOVE DAYFACII  TO GCVI-VALUE-LEN-1                       00160700
160800         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00160800
160900         IF  GCVI-VALUE-NOT-FOUND                                 00160900
161000         THEN                                                     00161000
161100             MOVE DFHBMUBF TO DAYFACIA                            00161100
161200             MOVE -1       TO DAYFACIL                            00161200
161300             IF ACWA-SCREEN-HAS-ERRORS                            00161300
161400             THEN                                                 00161400
161500                 NEXT SENTENCE                                    00161500
161600             ELSE                                                 00161600
161700                 MOVE 'Y'        TO ACWA-ERROR-SW                 00161700
161800                 SET WT-01-INDEX TO +03                           00161800
161900         ELSE                                                     00161900
162000             IF  GCVI-VALUE-NOT-LOADED                            00162000
162100             THEN                                                 00162100
162200                 MOVE DFHBMUBF TO DAYFACIA                        00162200
162300                 IF  GCVI-TABLE-SW = 'Y'                          00162300
162400                 THEN                                             00162400
162500                     MOVE 'N' TO GCVI-TABLE-SW                    00162500
162600                     MOVE -1  TO DAYFACIL                         00162600
162700                 ELSE                                             00162700
162800                     NEXT SENTENCE                                00162800
162900             ELSE                                                 00162900
163000                 NEXT SENTENCE                                    00163000
163100     ELSE                                                         00163100
163200         MOVE ZEROS TO DAYFACIO.                                  00163200
163300                                                                  00163300
163400                                                                  00163400
163500*------------------ LINE OF BUSINESS ----------------------------*00163500
163600                                                                  00163600
163700     IF  LOBL > ZERO                                              00163700
163800     THEN                                                         00163800
163900         MOVE 'ACCM03'  TO GCVI-FIELDS-KEY-ID                     00163900
164000         MOVE LOBI      TO GCVI-VALUE-LEN-1                       00164000
164100         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00164100
164200         IF  GCVI-VALUE-NOT-FOUND                                 00164200
164300         THEN                                                     00164300
164400             MOVE DFHBMUBF TO LOBA                                00164400
164500             IF  ACWA-SCREEN-HAS-ERRORS                           00164500
164600             THEN                                                 00164600
164700                 NEXT SENTENCE                                    00164700
164800             ELSE                                                 00164800
164900                 MOVE 'Y'      TO ACWA-ERROR-SW                   00164900
165000                 MOVE DFHBMUBF TO LOBA                            00165000
165100                 MOVE -1       TO LOBL                            00165100
165200                 SET WT-01-INDEX TO +26                           00165200
165300         ELSE                                                     00165300
165400             IF  GCVI-VALUE-NOT-LOADED                            00165400
165500             THEN                                                 00165500
165600                 MOVE DFHBMUBF     TO LOBA                        00165600
165700                 IF GCVI-TABLE-SW = 'Y'                           00165700
165800                     MOVE 'N'      TO GCVI-TABLE-SW               00165800
165900                     MOVE -1       TO LOBL                        00165900
166000                 ELSE                                             00166000
166100                     NEXT SENTENCE                                00166100
166200             ELSE                                                 00166200
166300                 NEXT SENTENCE                                    00166300
166400     ELSE                                                         00166400
166500         MOVE DFHBMUBF TO LOBA                                    00166500
166600         IF ACWA-SCREEN-HAS-ERRORS                                00166600
166700         THEN                                                     00166700
166800             NEXT SENTENCE                                        00166800
166900         ELSE                                                     00166900
167000             MOVE 'Y'        TO  ACWA-ERROR-SW                    00167000
167100             MOVE -1         TO  LOBL                             00167100
167200             SET WT-01-INDEX TO +27.                              00167200
167300                                                                  00167300
167400                                                                  00167400
167500*------------------ PLACE OF TREATMENT --------------------------*00167500
167600                                                                  00167600
167700     IF  PLCTRMTL > ZERO                                          00167700
167800     THEN                                                         00167800
167900         MOVE 'BPFB01'  TO GCVI-FIELDS-KEY-ID                     00167900
168000         MOVE PLCTRMTI  TO GCVI-VALUE-LEN-2                       00168000
168100         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00168100
168200         IF  GCVI-VALUE-NOT-FOUND                                 00168200
168300         THEN                                                     00168300
168400             MOVE DFHBMUBF TO PLCTRMTA                            00168400
168500             IF  ACWA-SCREEN-HAS-ERRORS                           00168500
168600             THEN                                                 00168600
168700                 NEXT SENTENCE                                    00168700
168800             ELSE                                                 00168800
168900                 MOVE 'Y'        TO ACWA-ERROR-SW                 00168900
169000                 MOVE -1         TO PLCTRMTL                      00169000
169100                 SET WT-01-INDEX TO +28                           00169100
169200         ELSE                                                     00169200
169300             IF  GCVI-VALUE-NOT-LOADED                            00169300
169400             THEN                                                 00169400
169500                 MOVE DFHBMUBF TO PLCTRMTA                        00169500
169600                 IF  GCVI-TABLE-SW = 'Y'                          00169600
169700                 THEN                                             00169700
169800                     MOVE 'N'      TO GCVI-TABLE-SW               00169800
169900                     MOVE -1       TO PLCTRMTL                    00169900
170000                 ELSE                                             00170000
170100                     NEXT SENTENCE                                00170100
170200             ELSE                                                 00170200
170300                 NEXT SENTENCE                                    00170300
170400     ELSE                                                         00170400
170500         MOVE DFHBMUBF TO PLCTRMTA                                00170500
170600         IF ACWA-SCREEN-HAS-ERRORS                                00170600
170700         THEN                                                     00170700
170800             NEXT SENTENCE                                        00170800
170900         ELSE                                                     00170900
171000             MOVE 'Y'        TO  ACWA-ERROR-SW                    00171000
171100             MOVE -1         TO  PLCTRMTL                         00171100
171200             SET WT-01-INDEX TO +60.                              00171200
171300                                                                  00171300
171400                                                                  00171400
171500*------------------ SERVICE GROUP -------------------------------*00171500
171600                                                                  00171600
171700     IF  SRVGRUPL > ZERO                                          00171700
171800     THEN                                                         00171800
171900         MOVE 'ACCM04'  TO GCVI-FIELDS-KEY-ID                     00171900
172000         MOVE SRVGRUPI  TO GCVI-VALUE-LEN-2                       00172000
172100         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00172100
172200         IF  GCVI-VALUE-NOT-FOUND                                 00172200
172300         THEN                                                     00172300
172400             MOVE DFHBMUBF TO SRVGRUPA                            00172400
172500             IF  ACWA-SCREEN-HAS-ERRORS                           00172500
172600             THEN                                                 00172600
172700                 NEXT SENTENCE                                    00172700
172800             ELSE                                                 00172800
172900                 MOVE 'Y'        TO ACWA-ERROR-SW                 00172900
173000                 MOVE -1         TO SRVGRUPL                      00173000
173100                 SET WT-01-INDEX TO +32                           00173100
173200         ELSE                                                     00173200
173300             IF  GCVI-VALUE-NOT-LOADED                            00173300
173400             THEN                                                 00173400
173500                 MOVE DFHBMUBF TO SRVGRUPA                        00173500
173600                 IF  GCVI-TABLE-SW = 'Y'                          00173600
173700                 THEN                                             00173700
173800                     MOVE 'N'      TO GCVI-TABLE-SW               00173800
173900                     MOVE -1       TO SRVGRUPL                    00173900
174000                 ELSE                                             00174000
174100                     NEXT SENTENCE                                00174100
174200             ELSE                                                 00174200
174300                 NEXT SENTENCE                                    00174300
174400     ELSE                                                         00174400
174500         MOVE ZEROS TO SRVGRUPO.                                  00174500
174600                                                                  00174600
174700                                                                  00174700
174800*------------------ DEFINITION ----------------------------------*00174800
174900                                                                  00174900
175000     IF  FUNCTONI = 'GA1C'                                        00175000
175100         MOVE 'AACL02'  TO GCVI-FIELDS-KEY-ID.                    00175100
175200                                                                  00175200
175300     IF  FUNCTONI = 'GA1D'                                        00175300
175400         MOVE 'AADL05'  TO GCVI-FIELDS-KEY-ID.                    00175400
175500                                                                  00175500
175600     IF  FUNCTONI = 'GA1E'                                        00175600
175700         MOVE 'MDDL01'  TO GCVI-FIELDS-KEY-ID.                    00175700
175800                                                                  00175800
175900     IF  FUNCTONI = 'GA1B'                                        00175900
176000         MOVE 'AABM21'  TO GCVI-FIELDS-KEY-ID.                    00176000
176100                                                                  00176100
176200     IF  FUNCTONI = 'GA1P'                                        00176200
176300         MOVE 'AACP04'  TO GCVI-FIELDS-KEY-ID.                    00176300
176400                                                                  00176400
176500         IF  DEFINTNL > ZERO                                      00176500
176600         THEN                                                     00176600
176700             MOVE DEFINTNI  TO GCVI-VALUE-LEN-2                   00176700
176800             PERFORM 1200-000-LINK-TO-GCVIOPGM                    00176800
176900             IF  GCVI-VALUE-NOT-FOUND                             00176900
177000             THEN                                                 00177000
177100                 MOVE DFHBMUBF TO DEFINTNA                        00177100
177200                 IF  ACWA-SCREEN-HAS-ERRORS                       00177200
177300                 THEN                                             00177300
177400                     NEXT SENTENCE                                00177400
177500                 ELSE                                             00177500
177600                     MOVE 'Y'      TO ACWA-ERROR-SW               00177600
177700                     MOVE DFHBMUBF TO DEFINTNA                    00177700
177800                     MOVE -1       TO DEFINTNL                    00177800
177900                     SET WT-01-INDEX TO +51                       00177900
178000             ELSE                                                 00178000
178100                 IF  GCVI-VALUE-NOT-LOADED                        00178100
178200                 THEN                                             00178200
178300                     MOVE DFHBMUBF     TO DEFINTNA                00178300
178400                     IF GCVI-TABLE-SW = 'Y'                       00178400
178500                         MOVE 'N'      TO GCVI-TABLE-SW           00178500
178600                         MOVE -1       TO DEFINTNL                00178600
178700                     ELSE                                         00178700
178800                         NEXT SENTENCE                            00178800
178900                 ELSE                                             00178900
179000                     NEXT SENTENCE                                00179000
179100         ELSE                                                     00179100
179200             IF  FUNCTONI = 'GA1D' OR 'GA1P'                      00179200
179300             THEN                                                 00179300
179400                 MOVE ZEROS    TO DEFINTNO                        00179400
179500             ELSE                                                 00179500
179600                 MOVE DFHBMUBF TO DEFINTNA                        00179600
179700                 IF ACWA-SCREEN-HAS-ERRORS                        00179700
179800                 THEN                                             00179800
179900                     NEXT SENTENCE                                00179900
180000                 ELSE                                             00180000
180100                     MOVE 'Y'        TO  ACWA-ERROR-SW            00180100
180200                     MOVE -1         TO  DEFINTNL                 00180200
180300                     SET WT-01-INDEX TO +52.                      00180300
180400                                                                  00180400
180500                                                                  00180500
180600*------------------ FAMILY OR INDIVIDUAL INDICATOR --------------*00180600
180700                                                                  00180700
180800     IF  FAMINDIL > ZERO                                          00180800
180900     THEN                                                         00180900
181000         MOVE 'ACCM12'  TO GCVI-FIELDS-KEY-ID                     00181000
181100         MOVE FAMINDII  TO GCVI-VALUE-LEN-1                       00181100
181200         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00181200
181300         IF  GCVI-VALUE-NOT-FOUND                                 00181300
181400         THEN                                                     00181400
181500             MOVE DFHBMUBF TO FAMINDIA                            00181500
181600             IF  ACWA-SCREEN-HAS-ERRORS                           00181600
181700             THEN                                                 00181700
181800                 NEXT SENTENCE                                    00181800
181900             ELSE                                                 00181900
182000                 MOVE 'Y'        TO ACWA-ERROR-SW                 00182000
182100                 MOVE -1         TO FAMINDIL                      00182100
182200                 SET WT-01-INDEX TO +29                           00182200
182300         ELSE                                                     00182300
182400             IF  GCVI-VALUE-NOT-LOADED                            00182400
182500             THEN                                                 00182500
182600                 MOVE DFHBMUBF TO FAMINDIA                        00182600
182700                 IF  GCVI-TABLE-SW = 'Y'                          00182700
182800                 THEN                                             00182800
182900                     MOVE 'N'      TO GCVI-TABLE-SW               00182900
183000                     MOVE -1       TO FAMINDIL                    00183000
183100                 ELSE                                             00183100
183200                     NEXT SENTENCE                                00183200
183300             ELSE                                                 00183300
183400                 NEXT SENTENCE                                    00183400
183500     ELSE                                                         00183500
183600         MOVE DFHBMUBF  TO  FAMINDIA                              00183600
183700         IF  ACWA-SCREEN-HAS-ERRORS                               00183700
183800         THEN                                                     00183800
183900             NEXT SENTENCE                                        00183900
184000         ELSE                                                     00184000
184100             MOVE 'Y'        TO  ACWA-ERROR-SW                    00184100
184200             MOVE -1         TO  FAMINDIL                         00184200
184300             SET WT-01-INDEX TO +30.                              00184300
184400                                                                  00184400
184500                                                                  00184500
184600*------------------- LOGICAL EDIT ------------------------------* 00184600
184700*                                                               * 00184700
184800* LOGICAL EDIT COMPARING VALUE QUALIFIER AND FAMILY INDIVIDUAL  * 00184800
184900* INDICATOR                                                     * 00184900
185000*                                                               * 00185000
185100*---------------------------------------------------------------* 00185100
185200                                                                  00185200
185300     IF  BENVLQLI = 7                                             00185300
185400     THEN                                                         00185400
185500         IF  FAMINDII = 'F'                                       00185500
185600         THEN                                                     00185600
185700             NEXT SENTENCE                                        00185700
185800         ELSE                                                     00185800
185900             MOVE DFHBMUBF TO FAMINDIA                            00185900
186000             IF  ACWA-SCREEN-HAS-ERRORS                           00186000
186100             THEN                                                 00186100
186200                 NEXT SENTENCE                                    00186200
186300             ELSE                                                 00186300
186400                 MOVE 'Y'        TO ACWA-ERROR-SW                 00186400
186500                 MOVE -1         TO FAMINDIL                      00186500
186600                 SET WT-01-INDEX TO +31                           00186600
186700     ELSE                                                         00186700
186800         NEXT SENTENCE.                                           00186800
186900                                                                  00186900
187000                                                                  00187000
187100*------------------- LOGICAL EDIT REQUEST D246 -----------------* 00187100
187200*                                                               * 00187200
187300* LOGICAL EDIT COMPARING BENEFIT PERIOD INDICATOR AND VALUE     * 00187300
187400*   QUALIFIER.    03/13/90                                      * 00187400
187500*                                                               * 00187500
187600*---------------------------------------------------------------* 00187600
187700                                                                  00187700
187800     IF  PERIODI  = '0V'  OR '0W'                                 00187800
187900     THEN                                                         00187900
188000        IF  BENVLQLI  =  '6'  OR '9'                              00188000
188100            NEXT SENTENCE                                         00188100
188200        ELSE                                                      00188200
188300            MOVE DFHBMUBF TO BENVLQLA                             00188300
188400            MOVE -1       TO BENVLQLL                             00188400
188500            IF  ACWA-SCREEN-HAS-ERRORS                            00188500
188600            THEN                                                  00188600
188700                NEXT SENTENCE                                     00188700
188800            ELSE                                                  00188800
188900                MOVE 'Y'        TO ACWA-ERROR-SW                  00188900
189000                SET WT-01-INDEX TO +65.                           00189000
189100                                                                  00189100
189200     IF  BENVLQLI = '6'   OR '9'                                  00189200
189300     THEN                                                         00189300
189400        IF  PERIODI   =  '0V' OR '0W'                             00189400
189500            NEXT SENTENCE                                         00189500
189600        ELSE                                                      00189600
189700            MOVE DFHBMUBF TO PERIODA                              00189700
189800            MOVE -1       TO PERIODL                              00189800
189900            IF  ACWA-SCREEN-HAS-ERRORS                            00189900
190000            THEN                                                  00190000
190100                NEXT SENTENCE                                     00190100
190200            ELSE                                                  00190200
190300                MOVE 'Y'        TO ACWA-ERROR-SW                  00190300
190400                SET WT-01-INDEX TO +66.                           00190400
190500                                                                  00190500
190600                                                                  00190600
190700*------------------ VALUE QUALIFIER -----------------------------*00190700
190800                                                                  00190800
190900     IF  BENVLQLL > ZERO                                          00190900
191000     THEN                                                         00191000
191100         MOVE 'ACCM09'  TO GCVI-FIELDS-KEY-ID                     00191100
191200         MOVE BENVLQLI  TO GCVI-VALUE-LEN-1                       00191200
191300         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00191300
191400         IF  GCVI-VALUE-NOT-FOUND                                 00191400
191500         THEN                                                     00191500
191600             MOVE DFHBMUBF TO BENVLQLA                            00191600
191700             MOVE -1       TO BENVLQLL                            00191700
191800             IF ACWA-SCREEN-HAS-ERRORS                            00191800
191900             THEN                                                 00191900
192000                 NEXT SENTENCE                                    00192000
192100             ELSE                                                 00192100
192200                 MOVE 'Y'        TO ACWA-ERROR-SW                 00192200
192300                 SET WT-01-INDEX TO +12                           00192300
192400         ELSE                                                     00192400
192500             IF  GCVI-VALUE-NOT-LOADED                            00192500
192600             THEN                                                 00192600
192700                 MOVE DFHBMUBF TO MAXOVRDA                        00192700
192800                 IF  GCVI-TABLE-SW = 'Y'                          00192800
192900                 THEN                                             00192900
193000                     MOVE 'N'      TO GCVI-TABLE-SW               00193000
193100                     MOVE -1       TO MAXOVRDL                    00193100
193200                 ELSE                                             00193200
193300                     NEXT SENTENCE                                00193300
193400             ELSE                                                 00193400
193500                 NEXT SENTENCE                                    00193500
193600     ELSE                                                         00193600
193700         MOVE DFHBMUBF  TO  BENVLQLA                              00193700
193800         MOVE -1        TO  BENVLQLL                              00193800
193900         IF  ACWA-SCREEN-HAS-ERRORS                               00193900
194000         THEN                                                     00194000
194100             NEXT SENTENCE                                        00194100
194200         ELSE                                                     00194200
194300             SET WT-01-INDEX TO +13                               00194300
194400             MOVE 'Y'        TO ACWA-ERROR-SW.                    00194400
194500                                                                  00194500
194600                                                                  00194600
194700*------------------ VALUE LIMIT ---------------------------------*00194700
194800                                                                  00194800
194900 1100-120-VALIDATE-VALUE-LIMIT.                                   00194900
195000                                                                  00195000
195100     MOVE BNMXVALI TO ACWA-DISPLAY-VALUE-LIMIT.                   00195100
195200                                                                  00195200
195300     IF  BENVLQLI = '5'                                           00195300
195400     THEN                                                         00195400
195500         NEXT SENTENCE                                            00195500
195600     ELSE                                                         00195600
195700         IF  ACWA-DISPLAY-1 = SPACE OR LOW-VALUES                 00195700
195800         THEN                                                     00195800
195900             NEXT SENTENCE                                        00195900
196000         ELSE                                                     00196000
196100             MOVE BNMXVALI TO ACWA-VAL-LIM-SCREEN                 00196100
196200             IF (ACWA-VAL-LIM-SCREEN-NEG1-3 = 'NEG' OR            00196200
196300                 ACWA-VAL-LIM-SCREEN-NEG2-3 = 'NEG') OR           00196300
196400                (ACWA-VAL-LIM-SCREEN-NEG1-3 = 'UNL' OR            00196400
196500                 ACWA-VAL-LIM-SCREEN-NEG2-3 = 'UNL') OR           00196500
196600                 BNMXVALI NOT NUMERIC                             00196600
196700             THEN                                                 00196700
196800                 MOVE DFHBMUBF  TO BNMXVALA                       00196800
196900                 MOVE -1        TO BNMXVALL                       00196900
197000                 IF  ACWA-SCREEN-HAS-ERRORS                       00197000
197100                 THEN                                             00197100
197200                     GO TO 1100-122-EXIT                          00197200
197300                 ELSE                                             00197300
197400                     SET WT-01-INDEX TO +14                       00197400
197500                     MOVE 'Y'        TO ACWA-ERROR-SW             00197500
197600                     GO TO 1100-122-EXIT                          00197600
197700             ELSE                                                 00197700
197800                 MOVE BNMXVALI  TO  ACWA-VAL-LIM-SCREEN           00197800
197900                 IF  ACWA-VAL-LIM-SCREEN-0-1 = '0'                00197900
198000                 THEN                                             00198000
198100                     NEXT SENTENCE                                00198100
198200                 ELSE                                             00198200
198300                     MOVE DFHBMUBF  TO   BNMXVALA                 00198300
198400                     IF  ACWA-SCREEN-HAS-ERRORS                   00198400
198500                     THEN                                         00198500
198600                         GO TO 1100-122-EXIT                      00198600
198700                     ELSE                                         00198700
198800                         SET WT-01-INDEX TO +15                   00198800
198900                         MOVE -1         TO BNMXVALL              00198900
199000                         MOVE 'Y'        TO ACWA-ERROR-SW         00199000
199100                         GO TO 1100-122-EXIT.                     00199100
199200                                                                  00199200
199300     IF  BENVLQLI = '5'                                           00199300
199400     THEN                                                         00199400
199500         IF  BNMXVALI NUMERIC                                     00199500
199600         THEN                                                     00199600
199700             MOVE DFHBMUBF  TO   BNMXVALA                         00199700
199800             IF  ACWA-SCREEN-HAS-ERRORS                           00199800
199900             THEN                                                 00199900
200000                 GO TO 1100-122-EXIT                              00200000
200100             ELSE                                                 00200100
200200                 SET WT-01-INDEX TO +16                           00200200
200300                 MOVE -1         TO BNMXVALL                      00200300
200400                 MOVE 'Y'        TO ACWA-ERROR-SW                 00200400
200500                 GO TO 1100-122-EXIT                              00200500
200600         ELSE                                                     00200600
200700             MOVE BNMXVALI TO ACWA-VAL-LIM-SCREEN                 00200700
200800             IF  ACWA-VAL-LIM-SCREEN-NEG1-3 = 'NEG' OR            00200800
200900                 ACWA-VAL-LIM-SCREEN-NEG2-3 = 'NEG'               00200900
201000             THEN                                                 00201000
201100                 MOVE -1 TO ACWA-VALUE-LIMIT-9-9                  00201100
201200                 GO TO 1100-122-VALUE-LIMIT                       00201200
201300             ELSE                                                 00201300
201400             IF  ACWA-VAL-LIM-SCREEN-NEG1-3 = 'UNL' OR            00201400
201500                 ACWA-VAL-LIM-SCREEN-NEG2-3 = 'UNL'               00201500
201600             THEN                                                 00201600
201700                 MOVE -2 TO ACWA-VALUE-LIMIT-9-9                  00201700
201800                 GO TO 1100-122-VALUE-LIMIT                       00201800
201900             ELSE                                                 00201900
202000                 GO TO 1100-121-VALUE-LIMIT.                      00202000
202100                                                                  00202100
202200     MOVE BNMXVALI TO ACWA-DISPLAY-VALUE-LIMIT.                   00202200
202300                                                                  00202300
202400     IF ACWA-DISPLAY-9 NUMERIC                                    00202400
202500        GO TO 1100-121-VALUE-LIMIT.                               00202500
202600                                                                  00202600
202700     MOVE BNMXVALI TO ACWA-VAL-LIM-SCREEN.                        00202700
202800                                                                  00202800
202900     IF (ACWA-VAL-LIM-SCREEN-NEG1-3 = 'NEG' OR                    00202900
203000         ACWA-VAL-LIM-SCREEN-NEG2-3 = 'NEG') OR                   00203000
203100        (ACWA-VAL-LIM-SCREEN-NEG1-3 = 'UNL' OR                    00203100
203200         ACWA-VAL-LIM-SCREEN-NEG2-3 = 'UNL')                      00203200
203300     THEN                                                         00203300
203400         MOVE DFHBMUBF  TO  BNMXVALA                              00203400
203500         IF  ACWA-SCREEN-HAS-ERRORS                               00203500
203600         THEN                                                     00203600
203700             GO TO 1100-122-EXIT                                  00203700
203800         ELSE                                                     00203800
203900             SET WT-01-INDEX TO +14                               00203900
204000             MOVE -1         TO  BNMXVALL                         00204000
204100             MOVE 'Y'        TO  ACWA-ERROR-SW                    00204100
204200             GO TO 1100-122-EXIT                                  00204200
204300     ELSE                                                         00204300
204400         GO TO 1100-121-VALUE-LIMIT.                              00204400
204500                                                                  00204500
204600                                                                  00204600
204700 1100-121-VALUE-LIMIT.                                            00204700
204800                                                                  00204800
204900                                                                  00204900
205000     MOVE BNMXVALI TO ACWA-VAL-LIM-SCREEN.                        00205000
205100                                                                  00205100
205200     IF  BNMXVALI  NOT  NUMERIC                                   00205200
205300     THEN                                                         00205300
205400         IF  ACWA-VAL-LIM-SCREEN-NEG1-3 = 'NEG' OR                00205400
205500             ACWA-VAL-LIM-SCREEN-NEG2-3 = 'NEG'                   00205500
205600         THEN                                                     00205600
205700             MOVE -1  TO  ACWA-VALUE-LIMIT-9-9                    00205700
205800             GO TO 1100-122-VALUE-LIMIT                           00205800
205900         ELSE                                                     00205900
206000         IF  ACWA-VAL-LIM-SCREEN-NEG1-3 = 'UNL' OR                00206000
206100             ACWA-VAL-LIM-SCREEN-NEG2-3 = 'UNL'                   00206100
206200         THEN                                                     00206200
206300             MOVE -2  TO  ACWA-VALUE-LIMIT-9-9                    00206300
206400             GO TO 1100-122-VALUE-LIMIT                           00206400
206500         ELSE                                                     00206500
206600             NEXT SENTENCE                                        00206600
206700     ELSE                                                         00206700
206800         NEXT SENTENCE.                                           00206800
206900                                                                  00206900
207000                                                                  00207000
207100     IF  BENVLQLI = '5'                                           00207100
207200     THEN                                                         00207200
207300         NEXT SENTENCE                                            00207300
207400     ELSE                                                         00207400
207500         GO TO 1100-122-VALUE-LIMIT.                              00207500
207600                                                                  00207600
207700     IF  ACWA-VAL-LIM-SCREEN-1 NOT = '.'   OR                     00207700
207800         ACWA-VAL-LIM-SCREEN-2 NOT NUMERIC OR                     00207800
207900         ACWA-VAL-LIM-SCREEN-7 NOT NUMERIC                        00207900
208000     THEN                                                         00208000
208100         MOVE DFHBMUBF  TO  BNMXVALA                              00208100
208200         IF  ACWA-SCREEN-HAS-ERRORS                               00208200
208300         THEN                                                     00208300
208400             GO TO 1100-122-EXIT                                  00208400
208500         ELSE                                                     00208500
208600             SET WT-01-INDEX TO +17                               00208600
208700             MOVE -1         TO BNMXVALL                          00208700
208800             MOVE 'Y'        TO ACWA-ERROR-SW                     00208800
208900             GO TO 1100-122-EXIT                                  00208900
209000     ELSE                                                         00209000
209100         NEXT SENTENCE.                                           00209100
209200                                                                  00209200
209300                                                                  00209300
209400 1100-122-VALUE-LIMIT.                                            00209400
209500                                                                  00209500
209600     IF BENVLQLI = '5'                                            00209600
209700        MOVE BNMXVALI TO ACWA-BNMXVALI-N                          00209700
209800        MOVE BNMXVALI TO ACWA-VAL-LIM-SCREEN                      00209800
209900        GO TO 1100-122-EXIT.                                      00209900
210000                                                                  00210000
210100     MOVE BNMXVALI TO ACWA-VAL-LIM-SCREEN.                        00210100
210200                                                                  00210200
210300     IF  BNMXVALI  NOT  NUMERIC                                   00210300
210400     THEN                                                         00210400
210500         IF (ACWA-VAL-LIM-SCREEN-NEG1-3 = 'NEG' OR                00210500
210600             ACWA-VAL-LIM-SCREEN-NEG2-3 = 'NEG') OR               00210600
210700            (ACWA-VAL-LIM-SCREEN-NEG1-3 = 'UNL' OR                00210700
210800             ACWA-VAL-LIM-SCREEN-NEG2-3 = 'UNL')                  00210800
210900         THEN                                                     00210900
211000             MOVE BNMXVALI TO ACWA-VAL-LIM-SCREEN                 00211000
211100             MOVE BNMXVALI TO ACWA-BNMXVALI-A                     00211100
211200             GO TO 1100-122-EXIT                                  00211200
211300         ELSE                                                     00211300
211400             NEXT SENTENCE                                        00211400
211500     ELSE                                                         00211500
211600         NEXT SENTENCE.                                           00211600
211700                                                                  00211700
211800     IF  ACWA-DISPLAY-1 = SPACE OR LOW-VALUES                     00211800
211900     THEN                                                         00211900
212000         MOVE BNMXVALI       TO ACWA-DISPLAY-VALUE-LIMIT          00212000
212100         MOVE ACWA-DISPLAY-9 TO ACWA-BNMXVALI-N                   00212100
212200     ELSE                                                         00212200
212300         MOVE BNMXVALI       TO ACWA-BNMXVALI-N.                  00212300
212400                                                                  00212400
212500     MOVE ACWA-BNMXVALI-N    TO ACWA-VAL-LIM-SCREEN.              00212500
212600                                                                  00212600
212700 1100-122-EXIT.                                                   00212700
212800                                                                  00212800
212900                                                                  00212900
213000*------------------- LOGICAL EDIT ------------------------------* 00213000
213100*                                                               * 00213100
213200* LOGICAL EDIT COMPARING VALUE QUALIFIER AND VALUE QUALIFIER    * 00213200
213300* LIMIT                                                         * 00213300
213400*                                                               * 00213400
213500*---------------------------------------------------------------* 00213500
213600                                                                  00213600
213700     IF  BENVLQLI NOT = 5                                         00213700
213800     THEN                                                         00213800
213900         IF  BNMXVALI > ZEROS                                     00213900
214000         THEN                                                     00214000
214100             NEXT SENTENCE                                        00214100
214200         ELSE                                                     00214200
214300             MOVE DFHBMUBF TO BNMXVALA                            00214300
214400             IF  ACWA-SCREEN-HAS-ERRORS                           00214400
214500             THEN                                                 00214500
214600                 NEXT SENTENCE                                    00214600
214700             ELSE                                                 00214700
214800                 MOVE 'Y'        TO ACWA-ERROR-SW                 00214800
214900                 MOVE -1         TO BNMXVALL                      00214900
215000                 SET WT-01-INDEX TO +18                           00215000
215100     ELSE                                                         00215100
215200         NEXT SENTENCE.                                           00215200
215300                                                                  00215300
215400                                                                  00215400
215500*------------------ PERCENT -------------------------------------*00215500
215600                                                                  00215600
215700     IF  FUNCTONI = 'GA1C' OR 'GA1E'                              00215700
215800     THEN                                                         00215800
215900         IF  PERLIMTL  > ZERO    AND                              00215900
216000             PERLIMTI IS NUMERIC AND                              00216000
216100*            PERLIMTI  > ZEROS                                    00216100
216200             PERLIMTI  >= ZEROS                                   00216200
216300         THEN                                                     00216300
216400             NEXT SENTENCE                                        00216400
216500         ELSE                                                     00216500
216600             MOVE -1        TO  PERLIMTL                          00216600
216700             MOVE DFHBMUBF  TO  PERLIMTA                          00216700
216800             IF  ACWA-SCREEN-HAS-ERRORS                           00216800
216900             THEN                                                 00216900
217000                 NEXT SENTENCE                                    00217000
217100             ELSE                                                 00217100
217200                 SET WT-01-INDEX TO +54                           00217200
217300                 MOVE 'Y'        TO ACWA-ERROR-SW                 00217300
217400     ELSE                                                         00217400
217500         NEXT SENTENCE.                                           00217500
217600                                                                  00217600
217700                                                                  00217700
217800*------------------ COST CONTAINMENT INDICATOR ------------------*00217800
217900                                                                  00217900
218000     IF  CSTCONTL > ZERO                                          00218000
218100     THEN                                                         00218100
218200         MOVE 'ACCM05'  TO GCVI-FIELDS-KEY-ID                     00218200
218300         MOVE CSTCONTI  TO GCVI-VALUE-LEN-2                       00218300
218400         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00218400
218500         IF  GCVI-VALUE-NOT-FOUND                                 00218500
218600         THEN                                                     00218600
218700             MOVE DFHBMUBF TO CSTCONTA                            00218700
218800             IF  ACWA-SCREEN-HAS-ERRORS                           00218800
218900             THEN                                                 00218900
219000                 NEXT SENTENCE                                    00219000
219100             ELSE                                                 00219100
219200                 MOVE 'Y'        TO ACWA-ERROR-SW                 00219200
219300                 MOVE -1         TO CSTCONTL                      00219300
219400                 SET WT-01-INDEX TO +36                           00219400
219500         ELSE                                                     00219500
219600             IF  GCVI-VALUE-NOT-LOADED                            00219600
219700             THEN                                                 00219700
219800                 MOVE DFHBMUBF TO CSTCONTA                        00219800
219900                 IF  GCVI-TABLE-SW = 'Y'                          00219900
220000                 THEN                                             00220000
220100                     MOVE 'N'      TO GCVI-TABLE-SW               00220100
220200                     MOVE -1       TO CSTCONTL                    00220200
220300                 ELSE                                             00220300
220400                     NEXT SENTENCE                                00220400
220500             ELSE                                                 00220500
220600                 NEXT SENTENCE                                    00220600
220700     ELSE                                                         00220700
220800         MOVE ZEROS TO CSTCONTO.                                  00220800
220900                                                                  00220900
221000                                                                  00221000
221100*------------------ CLAIM LEVEL ACCUM IND -----------------------*00221100
221200                                                                  00221200
221300     IF  CLMLVLIL > ZERO                                          00221300
221400     THEN                                                         00221400
221500         MOVE 'AABM20'  TO GCVI-FIELDS-KEY-ID                     00221500
221600         MOVE CLMLVLII  TO GCVI-VALUE-LEN-1                       00221600
221700         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00221700
221800         IF  GCVI-VALUE-NOT-FOUND                                 00221800
221900         THEN                                                     00221900
222000             MOVE DFHBMUBF TO CLMLVLIA                            00222000
222100             IF  ACWA-SCREEN-HAS-ERRORS                           00222100
222200             THEN                                                 00222200
222300                 NEXT SENTENCE                                    00222300
222400             ELSE                                                 00222400
222500                 MOVE 'Y'        TO ACWA-ERROR-SW                 00222500
222600                 MOVE -1         TO CLMLVLIL                      00222600
222700                 SET WT-01-INDEX TO +34                           00222700
222800         ELSE                                                     00222800
222900             IF  GCVI-VALUE-NOT-LOADED                            00222900
223000             THEN                                                 00223000
223100                 MOVE DFHBMUBF TO CLMLVLIA                        00223100
223200                 IF  GCVI-TABLE-SW = 'Y'                          00223200
223300                 THEN                                             00223300
223400                     MOVE 'N'      TO GCVI-TABLE-SW               00223400
223500                     MOVE -1       TO CLMLVLIL                    00223500
223600                 ELSE                                             00223600
223700                     NEXT SENTENCE                                00223700
223800             ELSE                                                 00223800
223900                 NEXT SENTENCE                                    00223900
224000     ELSE                                                         00224000
224100         MOVE ZEROS  TO CLMLVLIO.                                 00224100
224200                                                                  00224200
224300                                                                  00224300
224400                                                                  00224400
224500*------------------ CO-PAY INDICATOR ----------------------------*00224500
224600                                                                  00224600
224700     IF  COPAYINL > ZERO                                          00224700
224800     THEN                                                         00224800
224900         MOVE 'ACCM14'  TO GCVI-FIELDS-KEY-ID                     00224900
225000         MOVE COPAYINI  TO GCVI-VALUE-LEN-1                       00225000
225100         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00225100
225200         IF  GCVI-VALUE-NOT-FOUND                                 00225200
225300         THEN                                                     00225300
225400             MOVE DFHBMUBF TO COPAYINA                            00225400
225500             IF  ACWA-SCREEN-HAS-ERRORS                           00225500
225600             THEN                                                 00225600
225700                 NEXT SENTENCE                                    00225700
225800             ELSE                                                 00225800
225900                 MOVE 'Y'        TO ACWA-ERROR-SW                 00225900
226000                 MOVE -1         TO COPAYINL                      00226000
226100                 SET WT-01-INDEX TO +04                           00226100
226200         ELSE                                                     00226200
226300             IF  GCVI-VALUE-NOT-LOADED                            00226300
226400             THEN                                                 00226400
226500                 MOVE DFHBMUBF TO COPAYINA                        00226500
226600                 IF  GCVI-TABLE-SW = 'Y'                          00226600
226700                 THEN                                             00226700
226800                     MOVE 'N'      TO GCVI-TABLE-SW               00226800
226900                     MOVE -1       TO COPAYINL                    00226900
227000                 ELSE                                             00227000
227100                     NEXT SENTENCE                                00227100
227200             ELSE                                                 00227200
227300                 NEXT SENTENCE                                    00227300
227400     ELSE                                                         00227400
227500         MOVE ZEROS TO COPAYINO.                                  00227500
227600                                                                  00227600
227700                                                                  00227700
227800*--11154----------- RELATIONSHIP INDICATOR ----------------------*00227800
227900                                                                  00227900
228000     IF  RELPINDL > ZERO                                          00228000
228100     THEN                                                         00228100
228200         MOVE 'ACCM17'  TO GCVI-FIELDS-KEY-ID                     00228200
228300         MOVE RELPINDI  TO GCVI-VALUE-LEN-2                       00228300
228400         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00228400
228500         IF  GCVI-VALUE-NOT-FOUND                                 00228500
228600         THEN                                                     00228600
228700             MOVE DFHBMUBF TO RELPINDA                            00228700
228800             IF  ACWA-SCREEN-HAS-ERRORS                           00228800
228900             THEN                                                 00228900
229000                 NEXT SENTENCE                                    00229000
229100             ELSE                                                 00229100
229200                 MOVE 'Y'        TO ACWA-ERROR-SW                 00229200
229300                 MOVE -1         TO RELPINDL                      00229300
229400                 SET WT-01-INDEX TO +67                           00229400
229500         ELSE                                                     00229500
229600             IF  GCVI-VALUE-NOT-LOADED                            00229600
229700             THEN                                                 00229700
229800                 MOVE DFHBMUBF TO RELPINDA                        00229800
229900                 IF  GCVI-TABLE-SW = 'Y'                          00229900
230000                 THEN                                             00230000
230100                     MOVE 'N'      TO GCVI-TABLE-SW               00230100
230200                     MOVE -1       TO RELPINDL                    00230200
230300                 ELSE                                             00230300
230400                     NEXT SENTENCE                                00230400
230500             ELSE                                                 00230500
230600                 NEXT SENTENCE                                    00230600
230700     ELSE                                                         00230700
230800         MOVE ZEROS TO RELPINDO.                                  00230800
230900                                                                  00230900
231000*------------------ TIME AND DOLLAR -----------------------------*00231000
231100                                                                  00231100
231200     IF  FUNCTONI = 'GA1P'                                        00231200
231300     THEN                                                         00231300
231400         MOVE 'AACP05'  TO GCVI-FIELDS-KEY-ID                     00231400
231500         IF  TIMEDOLL > ZERO                                      00231500
231600         THEN                                                     00231600
231700             MOVE TIMEDOLI  TO GCVI-VALUE-LEN-2                   00231700
231800             PERFORM 1200-000-LINK-TO-GCVIOPGM                    00231800
231900             IF  GCVI-VALUE-NOT-FOUND                             00231900
232000             THEN                                                 00232000
232100                 MOVE DFHBMUBF TO TIMEDOLA                        00232100
232200                 IF  ACWA-SCREEN-HAS-ERRORS                       00232200
232300                 THEN                                             00232300
232400                     NEXT SENTENCE                                00232400
232500                 ELSE                                             00232500
232600                     MOVE 'Y'      TO ACWA-ERROR-SW               00232600
232700                     MOVE DFHBMUBF TO TIMEDOLA                    00232700
232800                     MOVE -1       TO TIMEDOLL                    00232800
232900                     SET WT-01-INDEX TO +79                       00232900
233000             ELSE                                                 00233000
233100                 IF  GCVI-VALUE-NOT-LOADED                        00233100
233200                 THEN                                             00233200
233300                     MOVE DFHBMUBF     TO TIMEDOLA                00233300
233400                     IF GCVI-TABLE-SW = 'Y'                       00233400
233500                         MOVE 'N'      TO GCVI-TABLE-SW           00233500
233600                         MOVE -1       TO TIMEDOLL                00233600
233700                     ELSE                                         00233700
233800                         NEXT SENTENCE                            00233800
233900                 ELSE                                             00233900
234000                     NEXT SENTENCE                                00234000
234100         ELSE                                                     00234100
234200             IF  FUNCTONI = 'GA1P'                                00234200
234300             THEN                                                 00234300
234400                 MOVE ZEROS    TO TIMEDOLO                        00234400
234500             ELSE                                                 00234500
234600                 MOVE DFHBMUBF TO TIMEDOLA                        00234600
234700                 IF ACWA-SCREEN-HAS-ERRORS                        00234700
234800                 THEN                                             00234800
234900                     NEXT SENTENCE                                00234900
235000                 ELSE                                             00235000
235100                     MOVE 'Y'        TO  ACWA-ERROR-SW            00235100
235200                     MOVE -1         TO  TIMEDOLL                 00235200
235300                     SET WT-01-INDEX TO +80.                      00235300
235400                                                                  00235400
235500*--11154--- IF AGE-LIMITS ARE CODED, THE INTERNAL DESCRIPTOR-----*00235500
235600*---------- MUST BE IN THE LIST OF VALID VALUES------------------*00235600
235700*       MOVE 'N' TO WS-INTDESC-NOTFOUND.                          00235700
235800*       IF INTDESKL > ZERO                                        00235800
235900*           MOVE 'ACCM19' TO GCVI-FIELDS-KEY-ID                   00235900
236000*           MOVE INTDESKI TO GCVI-VALUE-LEN-9                     00236000
236100*           PERFORM 1200-000-LINK-TO-GCVIOPGM                     00236100
236200*           IF GCVI-VALUE-NOT-FOUND                               00236200
236300*               MOVE DFHBMUBF TO INTDESKA                         00236300
236400*               IF ACWA-SCREEN-HAS-ERRORS                         00236400
236500*                   NEXT SENTENCE                                 00236500
236600*               ELSE                                              00236600
236700*                   MOVE 'Y' TO WS-INTDESC-NOTFOUND               00236700
236800*           ELSE                                                  00236800
236900*               IF GCVI-VALUE-NOT-LOADED                          00236900
237000*                   MOVE 'Y' TO WS-INTDESC-NOTFOUND               00237000
237100*                   MOVE DFHBMUBF TO INTDESKA                     00237100
237200*                   IF GCVI-TABLE-SW = 'Y'                        00237200
237300*                       MOVE 'N' TO GCVI-TABLE-SW.                00237300
237400*       IF AGELIMHI  >  ZERO                                      00237400
237500*       OR AGELIMLI  >  ZERO                                      00237500
237600*       OR AGEQLLI  IS  ALPHABETIC                                00237600
237700*       OR AGEQLHI  IS  ALPHABETIC                                00237700
237800*           IF WS-INTDESC-NOTFOUND = 'Y'                          00237800
237900*               MOVE DFHBMUBF TO INTDESKA                         00237900
238000*               IF ACWA-SCREEN-HAS-ERRORS                         00238000
238100*                   NEXT SENTENCE                                 00238100
238200*               ELSE                                              00238200
238300*                   MOVE 'Y' TO ACWA-ERROR-SW                     00238300
238400*                   MOVE -1 TO INTDESKL                           00238400
238500*                   IF INTDESKI = SPACES OR LOW-VALUES            00238500
238600*                       SET WT-01-INDEX TO +77                    00238600
238700*                   ELSE                                          00238700
238800*                       SET WT-01-INDEX TO +75                    00238800
238900*           ELSE                                                  00238900
239000*               NEXT SENTENCE                                     00239000
239100*       ELSE                                                      00239100
239200*           IF WS-INTDESC-NOTFOUND = 'N'                          00239200
239300*               IF INTDESKI = SPACES OR LOW-VALUES                00239300
239400*                   NEXT SENTENCE                                 00239400
239500*               ELSE                                              00239500
239600*                   MOVE DFHBMUBF TO INTDESKA                     00239600
239700*                   SET WT-01-INDEX TO +76                        00239700
239800*                   MOVE -1 TO INTDESKL                           00239800
239900*                   MOVE 'Y' TO ACWA-ERROR-SW.                    00239900
240000                                                                  00240000
240100*--11154-- AGE QUALIFIER INDICATORS MUST BE CODED TOGETHER ------*00240100
240200                                                                  00240200
240300     IF  (AGEQLLI  IS     ALPHABETIC  AND                         00240300
240400          AGEQLHI  IS NOT ALPHABETIC)        OR                   00240400
240500         (AGEQLHI  IS     ALPHABETIC  AND                         00240500
240600          AGEQLLI  IS NOT ALPHABETIC)                             00240600
240700     THEN                                                         00240700
240800             MOVE DFHBMUBF  TO  AGEQLLA                           00240800
240900             MOVE DFHBMUBF  TO  AGEQLHA                           00240900
241000             IF  ACWA-SCREEN-HAS-ERRORS                           00241000
241100                 NEXT SENTENCE                                    00241100
241200             ELSE                                                 00241200
241300                 SET WT-01-INDEX TO +74                           00241300
241400                 MOVE -1         TO  AGEQLLL                      00241400
241500                 MOVE 'Y'        TO ACWA-ERROR-SW                 00241500
241600     ELSE                                                         00241600
241700         NEXT SENTENCE.                                           00241700
241800                                                                  00241800
241900*--11154----------- AGE QUALIFIER INDICATOR FROM ----------------*00241900
242000*--------------ZERO VALUE MEANS UNCODED QUALIFIER ---------------*00242000
242100                                                                  00242100
242200     IF  AGEQLLL  > ZERO                                          00242200
242300         MOVE 'ACCM18'  TO GCVI-FIELDS-KEY-ID                     00242300
242400         MOVE AGEQLLI   TO GCVI-VALUE-LEN-1                       00242400
242500         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00242500
242600         IF  GCVI-VALUE-NOT-FOUND                                 00242600
242700             MOVE DFHBMUBF TO AGEQLLA                             00242700
242800             IF  ACWA-SCREEN-HAS-ERRORS                           00242800
242900                 CONTINUE                                         00242900
243000             ELSE                                                 00243000
243100                 MOVE 'Y'        TO ACWA-ERROR-SW                 00243100
243200                 MOVE -1         TO AGEQLLL                       00243200
243300                 SET WT-01-INDEX TO +71                           00243300
243400         ELSE                                                     00243400
243500             IF  GCVI-VALUE-NOT-LOADED                            00243500
243600                 MOVE DFHBMUBF TO AGEQLLA                         00243600
243700                 IF  GCVI-TABLE-SW = 'Y'                          00243700
243800                     MOVE 'N'      TO GCVI-TABLE-SW               00243800
243900                     MOVE -1       TO AGEQLLL.                    00243900
244000                                                                  00244000
244100*--11154----------- AGE QUALIFIER INDICATOR TO ------------------*00244100
244200*--------------ZERO VALUE MEANS UNCODED QUALIFIER ---------------*00244200
244300                                                                  00244300
244400     IF  AGEQLHL  > ZERO                                          00244400
244500         MOVE 'ACCM18'  TO GCVI-FIELDS-KEY-ID                     00244500
244600         MOVE AGEQLHI   TO GCVI-VALUE-LEN-1                       00244600
244700         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00244700
244800         IF  GCVI-VALUE-NOT-FOUND                                 00244800
244900             MOVE DFHBMUBF TO AGEQLHA                             00244900
245000             IF  ACWA-SCREEN-HAS-ERRORS                           00245000
245100                 CONTINUE                                         00245100
245200             ELSE                                                 00245200
245300                 MOVE 'Y'        TO ACWA-ERROR-SW                 00245300
245400                 MOVE -1         TO AGEQLHL                       00245400
245500                 SET WT-01-INDEX TO +71                           00245500
245600         ELSE                                                     00245600
245700             IF  GCVI-VALUE-NOT-LOADED                            00245700
245800                 MOVE DFHBMUBF TO AGEQLHA                         00245800
245900                 IF  GCVI-TABLE-SW = 'Y'                          00245900
246000                     MOVE 'N'      TO GCVI-TABLE-SW               00246000
246100                     MOVE -1       TO AGEQLHL.                    00246100
246200                                                                  00246200
246300*--11154--- BOTH FIELDS MUST BE CODED NUMERIC -------------------*00246300
246400*-----------ZERO VALUE MEANS UNCODED AGE LIMITS -----------------*00246400
246500                                                                  00246500
246600     IF  AGELIMLL  > ZERO                                         00246600
246700         IF  AGELIMLI  NOT  NUMERIC                               00246700
246800             MOVE DFHBMUBF  TO  AGELIMLA                          00246800
246900             IF  ACWA-SCREEN-HAS-ERRORS                           00246900
247000                 CONTINUE                                         00247000
247100             ELSE                                                 00247100
247200                 SET WT-01-INDEX TO +68                           00247200
247300                 MOVE -1        TO  AGELIMLL                      00247300
247400                 MOVE 'Y'        TO ACWA-ERROR-SW.                00247400
247500                                                                  00247500
247600     IF  AGELIMHL  > ZERO                                         00247600
247700         IF  AGELIMHI  NOT  NUMERIC                               00247700
247800             MOVE DFHBMUBF  TO  AGELIMHA                          00247800
247900             IF  ACWA-SCREEN-HAS-ERRORS                           00247900
248000                 NEXT SENTENCE                                    00248000
248100             ELSE                                                 00248100
248200                 SET WT-01-INDEX TO +68                           00248200
248300                 MOVE -1        TO  AGELIMHL                      00248300
248400                 MOVE 'Y'        TO ACWA-ERROR-SW.                00248400
248500                                                                  00248500
248600*--11154----------- AGE LIMIT FIELDS + LOGICAL EDIT -------------*00248600
248700*---------- IF ONE FIELD CODED THE OTHER FIELD MUST BE CODED ----*00248700
248800                                                                  00248800
248900     IF  AGELIMLL     >  ZERO    AND                              00248900
249000         AGELIMLI     >  ZERO                                     00249000
249100        IF   AGELIMHI NOT >  ZERO                                 00249100
249200             MOVE DFHBMUBF  TO  AGELIMLA                          00249200
249300             MOVE DFHBMUBF  TO  AGELIMHA                          00249300
249400             IF  ACWA-SCREEN-HAS-ERRORS                           00249400
249500                 NEXT SENTENCE                                    00249500
249600             ELSE                                                 00249600
249700                 SET WT-01-INDEX TO +70                           00249700
249800                 MOVE -1        TO  AGELIMLL                      00249800
249900                 MOVE 'Y'        TO ACWA-ERROR-SW.                00249900
250000                                                                  00250000
250100*--11154--- AGE LIMIT FROM MUST BE LESS THAN AGE LIMIT TO -------*00250100
250200*----------                                               -------*00250200
250300                                                                  00250300
250400* CONVERT TO DAYS FROM WHATEVER UNITS OF TIME                     00250400
250500     IF AGELIMLI NUMERIC                                          00250500
250600        IF AGELIMLI > 0                                           00250600
250700           MOVE AGELIMLI TO WS-AGE-LIM-FROM                       00250700
250800           EVALUATE AGEQLLI                                       00250800
250900             WHEN  'W'                                            00250900
251000              COMPUTE WS-AGE-LIM-FROM = WS-AGE-LIM-FROM * 7       00251000
251100             WHEN  'M'                                            00251100
251200              COMPUTE WS-AGE-LIM-FROM = WS-AGE-LIM-FROM * 30      00251200
251300             WHEN  'Y'                                            00251300
251400              COMPUTE WS-AGE-LIM-FROM = WS-AGE-LIM-FROM * 365     00251400
251500           END-EVALUATE                                           00251500
251600        ELSE                                                      00251600
251700           MOVE ZEROES TO WS-AGE-LIM-FROM.                        00251700
251800                                                                  00251800
251900     IF AGELIMHI NUMERIC                                          00251900
252000        IF AGELIMHI > 0                                           00252000
252100           MOVE AGELIMHI TO WS-AGE-LIM-TO                         00252100
252200           EVALUATE AGEQLHI                                       00252200
252300             WHEN  'W'                                            00252300
252400              COMPUTE WS-AGE-LIM-TO   = WS-AGE-LIM-TO * 7         00252400
252500             WHEN  'M'                                            00252500
252600              COMPUTE WS-AGE-LIM-TO   = WS-AGE-LIM-TO * 30        00252600
252700             WHEN  'Y'                                            00252700
252800              COMPUTE WS-AGE-LIM-TO   = WS-AGE-LIM-TO * 365       00252800
252900           END-EVALUATE                                           00252900
253000        ELSE                                                      00253000
253100           MOVE ZEROES TO WS-AGE-LIM-TO.                          00253100
253200                                                                  00253200
253300     IF  AGELIMHL > ZERO    AND                                   00253300
253400         AGELIMHI > ZERO                                          00253400
253500         IF  WS-AGE-LIM-TO NOT > WS-AGE-LIM-FROM                  00253500
253600             MOVE DFHBMUBF  TO  AGELIMLA                          00253600
253700             MOVE DFHBMUBF  TO  AGELIMHA                          00253700
253800             IF  ACWA-SCREEN-HAS-ERRORS                           00253800
253900                 CONTINUE                                         00253900
254000             ELSE                                                 00254000
254100                 SET WT-01-INDEX TO +69                           00254100
254200                 MOVE -1        TO  AGELIMLL                      00254200
254300                 MOVE 'Y'        TO ACWA-ERROR-SW.                00254300
254400                                                                  00254400
254500*--11154--- IF AGE-LIMIT FIELDS ARE CODED, AGE-QUALIFIER --------*00254500
254600*---------- FIELDS MUST BE CODED --------------------------------*00254600
254700                                                                  00254700
254800     IF  AGELIMHI >  ZERO                                         00254800
254900         IF  AGEQLLI  =  ZERO    OR                               00254900
255000             AGEQLHI  =  ZERO                                     00255000
255100             MOVE DFHBMUBF  TO  AGEQLLA                           00255100
255200             MOVE DFHBMUBF  TO  AGEQLHA                           00255200
255300             IF  ACWA-SCREEN-HAS-ERRORS                           00255300
255400                 CONTINUE                                         00255400
255500             ELSE                                                 00255500
255600                 SET WT-01-INDEX TO +72                           00255600
255700                 MOVE -1        TO  AGEQLLL                       00255700
255800                 MOVE 'Y'        TO ACWA-ERROR-SW.                00255800
255900                                                                  00255900
256000*--11154--- IF AGE-QUALIFIERS FIELDS ARE CODED, AGE-LIMIT -------*00256000
256100*---------- FIELDS MUST BE CODED --------------------------------*00256100
256200                                                                  00256200
256300                                                                  00256300
256400     IF AGEQLHI NOT = ZERO                                        00256400
256500        IF AGEQLHI  IS     ALPHABETIC                             00256500
256600           IF AGELIMHI NOT  >  ZEROS                              00256600
256700              MOVE DFHBMUBF  TO  AGELIMHA                         00256700
256800              IF ACWA-SCREEN-HAS-ERRORS                           00256800
256900                 CONTINUE                                         00256900
257000              ELSE                                                00257000
257100                 SET WT-01-INDEX TO +73                           00257100
257200                 MOVE -1        TO  AGELIMHL                      00257200
257300                 MOVE 'Y'        TO ACWA-ERROR-SW                 00257300
257400              END-IF                                              00257400
257500           END-IF                                                 00257500
257600        ELSE                                                      00257600
257700           MOVE DFHBMUBF  TO  AGEQLHA                             00257700
257800           SET WT-01-INDEX TO +71                                 00257800
257900           MOVE -1         TO AGEQLHL                             00257900
258000           MOVE 'Y'        TO ACWA-ERROR-SW.                      00258000
258100                                                                  00258100
258200     IF AGEQLLI NOT = ZERO                                        00258200
258300        IF AGEQLLI  IS     ALPHABETIC                             00258300
258400           IF AGELIMLI NOT  >  ZEROS                              00258400
258500              MOVE DFHBMUBF  TO  AGELIMLA                         00258500
258600              IF ACWA-SCREEN-HAS-ERRORS                           00258600
258700                 CONTINUE                                         00258700
258800              ELSE                                                00258800
258900                 SET WT-01-INDEX TO +73                           00258900
259000                 MOVE -1        TO  AGELIMLL                      00259000
259100                 MOVE 'Y'        TO ACWA-ERROR-SW                 00259100
259200              END-IF                                              00259200
259300           END-IF                                                 00259300
259400        ELSE                                                      00259400
259500           MOVE DFHBMUBF  TO  AGEQLLA                             00259500
259600           SET WT-01-INDEX TO +71                                 00259600
259700           MOVE -1         TO AGEQLLL                             00259700
259800           MOVE 'Y'        TO ACWA-ERROR-SW.                      00259800
259900                                                                  00259900
260000                                                                  00260000
260100*------------------ FEAK INDICATOR ------------------------------*00260100
260200                                                                  00260200
260300     IF FEAKINDL > ZERO                                           00260300
260400         MOVE 'ACCM20'  TO GCVI-FIELDS-KEY-ID                     00260400
260500         MOVE FEAKINDI  TO GCVI-VALUE-LEN-1                       00260500
260600         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00260600
260700         IF GCVI-VALUE-NOT-FOUND                                  00260700
260800             MOVE DFHBMUBF TO FEAKINDA                            00260800
260900             IF ACWA-SCREEN-HAS-ERRORS                            00260900
261000                 NEXT SENTENCE                                    00261000
261100             ELSE                                                 00261100
261200                 MOVE 'Y'        TO ACWA-ERROR-SW                 00261200
261300                 MOVE -1         TO FEAKINDL                      00261300
261400                 SET WT-01-INDEX TO +78                           00261400
261500         ELSE                                                     00261500
261600             IF GCVI-VALUE-NOT-LOADED                             00261600
261700                 MOVE DFHBMUBF TO FEAKINDA                        00261700
261800                 IF GCVI-TABLE-SW = 'Y'                           00261800
261900                     MOVE 'N'      TO GCVI-TABLE-SW               00261900
262000                     MOVE -1       TO FEAKINDL                    00262000
262100                 ELSE                                             00262100
262200                     NEXT SENTENCE                                00262200
262300             ELSE                                                 00262300
262400                 NEXT SENTENCE                                    00262400
262500     ELSE                                                         00262500
262600         MOVE ZEROS     TO  FEAKINDO.                             00262600
      *------------------------- BEN TYPE -----------------------------*00262734
                                                                        00262735
      *P21595 CHANGES STARTS                                            00262736
      **** THE  FIELD VALIDATION  FIELDS  FOR BEN TYPE                  00262737
      **** ABM  =  'AABM23'                                             00262738
      **** ACL  =  'AACL21'                                             00262739
      **** ACP  =  'AACP23'                                             00262740
      **** ADL  =  'AADL23'                                             00262741
      **** AOL  =  'AAOL06'                                             00262742
      ****                                                              00262743
      **** EVERY  ACCUM  HAS  UNIQUE VALUES                             00262744
      **** THAT IS  THE  REASON EACH ACCUM  HAS ITS  OWN                00262745
      **** VALIDATION LINE IN  THE  FIELD  VALIDATION  RECORD           00262746
174900                                                                  00262750
175000     IF  FUNCTONI = 'GA1C'                                        00262751
175100         MOVE 'AACL21'  TO GCVI-FIELDS-KEY-ID.                    00262752
175200                                                                  00262753
175300     IF  FUNCTONI = 'GA1D'                                        00262754
175400         MOVE 'AADL23'  TO GCVI-FIELDS-KEY-ID.                    00262755
175500                                                                  00262756
175600     IF  FUNCTONI = 'GA1E'                                        00262757
175700         MOVE 'AAOL06'  TO GCVI-FIELDS-KEY-ID.                    00262758
175800                                                                  00262759
175900     IF  FUNCTONI = 'GA1B'                                        00262760
176000         MOVE 'AABM23'  TO GCVI-FIELDS-KEY-ID.                    00262761
176100                                                                  00262762
176200     IF  FUNCTONI = 'GA1P'                                        00262763
176300         MOVE 'AACP23'  TO GCVI-FIELDS-KEY-ID.                    00262764
176400                                                                  00262765
176500         IF  BENTYPL  > ZERO                                      00262766
176600         THEN                                                     00262767
176700             MOVE BENTYPI   TO GCVI-VALUE                         00262769
176800             PERFORM 1200-000-LINK-TO-GCVIOPGM                    00262770
176900             IF  GCVI-VALUE-NOT-FOUND                             00262771
177000             THEN                                                 00262772
177100                 MOVE DFHBMUBF TO BENTYPA                         00262773
177200                 IF  ACWA-SCREEN-HAS-ERRORS                       00262774
177300                 THEN                                             00262775
177400                     NEXT SENTENCE                                00262776
177500                 ELSE                                             00262777
177600                     MOVE 'Y'      TO ACWA-ERROR-SW               00262778
177700                     MOVE DFHBMUBF TO BENTYPA                     00262779
177800                     MOVE -1       TO BENTYPL                     00262780
177900                     SET WT-01-INDEX TO +89                       00262781
178000             ELSE                                                 00262782
178100                 IF  GCVI-VALUE-NOT-LOADED                        00262783
178200                 THEN                                             00262784
178300                     MOVE DFHBMUBF     TO BENTYPA                 00262785
178400                     IF GCVI-TABLE-SW = 'Y'                       00262786
178500                         MOVE 'N'      TO GCVI-TABLE-SW           00262787
178600                         MOVE -1       TO BENTYPL                 00262788
178700                     ELSE                                         00262789
178800                         NEXT SENTENCE                            00262790
178900                 ELSE                                             00262791
179000                     NEXT SENTENCE                                00262792
179100         ELSE                                                     00262793
179200*            IF  FUNCTONI = 'GA1D' OR 'GA1P'                      00262794
179200             IF  FUNCTONI = 'GA1B' OR 'GA1C' OR 'GA1D' OR 'GA1E'  00262795
                                                             OR 'GA1P'  00262796
179300             THEN                                                 00262797
179400                 MOVE ZEROS    TO BENTYPO                         00262798
179500             ELSE                                                 00262799
179600                 MOVE DFHBMUBF TO BENTYPA                         00262800
179700                 IF ACWA-SCREEN-HAS-ERRORS                        00262801
179800                 THEN                                             00262802
179900                     NEXT SENTENCE                                00262803
180000                 ELSE                                             00262804
180100                     MOVE 'Y'        TO  ACWA-ERROR-SW            00262805
180200                     MOVE -1         TO  BENTYPL                  00262806
180300                     SET WT-01-INDEX TO +90.                      00262807
                                                                        00262808
      *------------------------ TIER CODE -----------------------------*00262809
      *P21595 CHANGES STARTS                                            00262810
      **** THE  FIELD VALIDATION  FIELD  IS  'ACCM25'                   00262811
      **** FOR ALL  5 ACCUMS,  BECAUSE                                  00262812
      **** THE VALUES ARE   COMMON TO ALL 5 ACCUMS                      00262813
260200                                                                  00262814
260300     IF TIERCDL  > ZERO                                           00262815
260400         MOVE 'ACCM25'  TO GCVI-FIELDS-KEY-ID                     00262816
260500         MOVE TIERCDI   TO GCVI-VALUE                             00262818
260600         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00262819
260700         IF GCVI-VALUE-NOT-FOUND                                  00262820
260800             MOVE DFHBMUBF TO TIERCDA                             00262821
260900             IF ACWA-SCREEN-HAS-ERRORS                            00262822
261000                 NEXT SENTENCE                                    00262823
261100             ELSE                                                 00262824
261200                 MOVE 'Y'        TO ACWA-ERROR-SW                 00262825
261300                 MOVE -1         TO TIERCDL                       00262826
261400                 SET WT-01-INDEX TO +87                           00262827
261500         ELSE                                                     00262828
261600             IF GCVI-VALUE-NOT-LOADED                             00262829
261700                 MOVE DFHBMUBF TO TIERCDA                         00262830
261800                 IF GCVI-TABLE-SW = 'Y'                           00262831
261900                     MOVE 'N'      TO GCVI-TABLE-SW               00262832
262000                     MOVE -1       TO TIERCDL                     00262833
262100                 ELSE                                             00262834
262200                     NEXT SENTENCE                                00262835
262300             ELSE                                                 00262836
262400                 NEXT SENTENCE                                    00262837
262500     ELSE                                                         00262838
262600         MOVE ZEROS     TO  TIERCDO.                              00262839
                                                                        00262840
      *------------------------- TIER LVL -----------------------------*00262841
      *P21595 CHANGES STARTS                                            00262842
      **** THE  FIELD VALIDATION  FIELD  IS  'ACCM26'                   00262843
      **** FOR ALL  5 ACCUMS,  BECAUSE                                  00262844
      **** THE VALUES ARE   COMMON TO ALL 5 ACCUMS                      00262845
                                                                        00262847
260300     IF TIERLVL  > ZERO                                           00262848
260400         MOVE 'ACCM26'  TO GCVI-FIELDS-KEY-ID                     00262849
260500         MOVE TIERLVI   TO GCVI-VALUE                             00262850
260600         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00262851
260700         IF GCVI-VALUE-NOT-FOUND                                  00262852
260800             MOVE DFHBMUBF TO TIERLVA                             00262853
260900             IF ACWA-SCREEN-HAS-ERRORS                            00262854
261000                 NEXT SENTENCE                                    00262855
261100             ELSE                                                 00262856
261200                 MOVE 'Y'        TO ACWA-ERROR-SW                 00262857
261300                 MOVE -1         TO TIERLVL                       00262858
261400                 SET WT-01-INDEX TO +88                           00262859
261500         ELSE                                                     00262860
261600             IF GCVI-VALUE-NOT-LOADED                             00262861
261700                 MOVE DFHBMUBF TO TIERLVA                         00262862
261800                 IF GCVI-TABLE-SW = 'Y'                           00262863
261900                     MOVE 'N'      TO GCVI-TABLE-SW               00262864
262000                     MOVE -1       TO TIERLVL                     00262865
262100                 ELSE                                             00262866
262200                     NEXT SENTENCE                                00262867
262300             ELSE                                                 00262868
262400                 NEXT SENTENCE                                    00262869
262500     ELSE                                                         00262870
262600         MOVE ZEROS     TO  TIERLVO.                              00262871
                                                                        00262872
262800*------------------ ACCUMULATOR IDENTIFIER ----------------------*00262880
262900* JP-1/28/04 CORRECTED FIELDVAL KEY FOR ACCUM ID                  00262900
263000                                                                  00263000
263100     IF ACCUMIDL > ZERO                                           00263100
263200         MOVE 'ACCM22'  TO GCVI-FIELDS-KEY-ID                     00263200
263300         MOVE ACCUMIDI  TO GCVI-VALUE                             00263300
263400         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00263400
263500         IF GCVI-VALUE-NOT-FOUND                                  00263500
263600             MOVE DFHBMUBF TO ACCUMIDA                            00263600
263700             IF ACWA-SCREEN-HAS-ERRORS                            00263700
263800                 NEXT SENTENCE                                    00263800
263900             ELSE                                                 00263900
264000                 MOVE 'Y'        TO ACWA-ERROR-SW                 00264000
264100                 MOVE -1         TO ACCUMIDL                      00264100
264200                 SET WT-01-INDEX TO +81                           00264200
264300         ELSE                                                     00264300
264400             IF GCVI-VALUE-NOT-LOADED                             00264400
264500                 MOVE DFHBMUBF TO ACCUMIDA                        00264500
264600                 IF GCVI-TABLE-SW = 'Y'                           00264600
264700                     MOVE 'N'      TO GCVI-TABLE-SW               00264700
264800                     MOVE -1       TO ACCUMIDL                    00264800
264900                 ELSE                                             00264900
265000                     NEXT SENTENCE                                00265000
265100             ELSE                                                 00265100
265200                 NEXT SENTENCE                                    00265200
265300     ELSE                                                         00265300
265400         MOVE ZEROS     TO  ACCUMIDO.                             00265400
265500                                                                  00265500
265600                                                                  00265600
265700*--D365B----------- COMBINATION APPLIED INDICATOR ---------------*00265700
265800                                                                  00265800
265900     IF  CAPINDL  > ZERO                                          00265900
266000     THEN                                                         00266000
266100         MOVE 'ACCM23'  TO GCVI-FIELDS-KEY-ID                     00266100
266200         MOVE CAPINDI   TO GCVI-VALUE-LEN-2                       00266200
266300         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00266300
266400         IF  GCVI-VALUE-NOT-FOUND                                 00266400
266500         THEN                                                     00266500
266600             MOVE DFHBMUBF TO CAPINDA                             00266600
266700             IF  ACWA-SCREEN-HAS-ERRORS                           00266700
266800             THEN                                                 00266800
266900                 NEXT SENTENCE                                    00266900
267000             ELSE                                                 00267000
267100                 MOVE 'Y'        TO ACWA-ERROR-SW                 00267100
267200                 MOVE -1         TO CAPINDL                       00267200
267300                 SET WT-01-INDEX TO +82                           00267300
267400         ELSE                                                     00267400
267500             IF  GCVI-VALUE-NOT-LOADED                            00267500
267600             THEN                                                 00267600
267700                 MOVE DFHBMUBF TO CAPINDA                         00267700
267800                 IF  GCVI-TABLE-SW = 'Y'                          00267800
267900                 THEN                                             00267900
268000                     MOVE 'N'      TO GCVI-TABLE-SW               00268000
268100                     MOVE -1       TO CAPINDL                     00268100
268200                 ELSE                                             00268200
268300                     NEXT SENTENCE                                00268300
268400             ELSE                                                 00268400
268500                 NEXT SENTENCE                                    00268500
268600     ELSE                                                         00268600
268700         MOVE ZEROS TO CAPINDO.                                   00268700
268800                                                                  00268800
268900     IF  FUNCTONI = 'GA1C' AND CAPINDI  > ZERO                    00268900
269000         IF  FDLRCLII NOT = 'A'                                   00269000
269100             MOVE DFHBMUBF TO CAPINDA                             00269100
269200             IF  ACWA-SCREEN-HAS-ERRORS                           00269200
269300                 NEXT SENTENCE                                    00269300
269400             ELSE                                                 00269400
269500                 MOVE 'Y'        TO ACWA-ERROR-SW                 00269500
269600                 MOVE -1         TO CAPINDL                       00269600
269700                 SET WT-01-INDEX TO +83                           00269700
269800             END-IF                                               00269800
269900         END-IF                                                   00269900
270000     ELSE                                                         00270000
270100         NEXT SENTENCE                                            00270100
270200     END-IF.                                                      00270200
270300                                                                  00270300
270400                                                                  00270400
270500*--D368------- SELECTIVE ADDITIONAL BENEFIT DETERMINATION -------*00270500
270600                                                                  00270600
270700     IF  SABDINDL > ZERO                                          00270700
270800     THEN                                                         00270800
270900         MOVE 'ACCM24'  TO GCVI-FIELDS-KEY-ID                     00270900
271000         MOVE SABDINDI  TO GCVI-VALUE-LEN-1                       00271000
271100         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00271100
271200         IF  GCVI-VALUE-NOT-FOUND                                 00271200
271300         THEN                                                     00271300
271400             MOVE DFHBMUBF TO SABDINDA                            00271400
271500             IF  ACWA-SCREEN-HAS-ERRORS                           00271500
271600             THEN                                                 00271600
271700                 NEXT SENTENCE                                    00271700
271800             ELSE                                                 00271800
271900                 MOVE 'Y'        TO ACWA-ERROR-SW                 00271900
272000                 MOVE -1         TO SABDINDL                      00272000
272100                 SET WT-01-INDEX TO +84                           00272100
272200         ELSE                                                     00272200
272300             IF  GCVI-VALUE-NOT-LOADED                            00272300
272400             THEN                                                 00272400
272500                 MOVE DFHBMUBF TO SABDINDA                        00272500
272600                 IF  GCVI-TABLE-SW = 'Y'                          00272600
272700                 THEN                                             00272700
272800                     MOVE 'N'      TO GCVI-TABLE-SW               00272800
272900                     MOVE -1       TO SABDINDL                    00272900
273000                 ELSE                                             00273000
273100                     NEXT SENTENCE                                00273100
273200             ELSE                                                 00273200
273300                 NEXT SENTENCE                                    00273300
273400     ELSE                                                         00273400
273500         MOVE ZERO  TO SABDINDO.                                  00273500
273600                                                                  00273600
273700**--SABD LOGICAL EDITING: CAN ONLY BE CODED ON ACP W/BEN PER=CA **00273700
273800                                                                  00273800
273900     IF SABDINDI = '1'                                            00273900
274000        IF FUNCTONI = 'GA1P'                                      00274000
274100           IF PERIODI = 'CA'                                      00274100
274200              NEXT SENTENCE                                       00274200
274300           ELSE                                                   00274300
274400              MOVE DFHBMUBF TO SABDINDA                           00274400
274500*             IF  ACWA-SCREEN-HAS-ERRORS                          00274500
274600*                 NEXT SENTENCE                                   00274600
274700*             ELSE                                                00274700
274800*                 MOVE 'Y'        TO ACWA-ERROR-SW                00274800
274900*                 MOVE -1         TO SABDINDL                     00274900
275000*                 SET WT-01-INDEX TO +85                          00275000
275100*             END-IF                                              00275100
275200           END-IF                                                 00275200
275300        ELSE                                                      00275300
275400           MOVE DFHBMUBF TO SABDINDA                              00275400
275500           IF  ACWA-SCREEN-HAS-ERRORS                             00275500
275600               NEXT SENTENCE                                      00275600
275700           ELSE                                                   00275700
275800               MOVE 'Y'        TO ACWA-ERROR-SW                   00275800
275900               MOVE -1         TO SABDINDL                        00275900
276000               SET WT-01-INDEX TO +86                             00276000
276100           END-IF                                                 00276100
276200        END-IF                                                    00276200
276300     END-IF.                                                      00276300
276400                                                                  00276400
276500                                                                  00276500
276600*------------------ ASCENDING/DESCENDING INDICATOR --------------*00276600
276700                                                                  00276700
276800     IF  FUNCTONI = 'GA1C' OR 'GA1E' OR 'GA1B'                    00276800
276900                 OR 'GA1D' OR 'GA1P'                              00276900
277000         IF  ASCDSCDL > ZERO                                      00277000
277100             MOVE 'ACCM11'  TO GCVI-FIELDS-KEY-ID                 00277100
277200             MOVE ASCDSCDI  TO GCVI-VALUE-LEN-1                   00277200
277300             PERFORM 1200-000-LINK-TO-GCVIOPGM                    00277300
277400             IF  GCVI-VALUE-NOT-FOUND                             00277400
277500                 MOVE DFHBMUBF TO ASCDSCDA                        00277500
277600                 IF  ACWA-SCREEN-HAS-ERRORS                       00277600
277700                     NEXT SENTENCE                                00277700
277800                 ELSE                                             00277800
277900                     MOVE 'Y'        TO ACWA-ERROR-SW             00277900
278000                     MOVE -1         TO ASCDSCDL                  00278000
278100                     SET WT-01-INDEX TO +55                       00278100
278200                 END-IF                                           00278200
278300             ELSE                                                 00278300
278400                 IF  GCVI-VALUE-NOT-LOADED                        00278400
278500                     MOVE DFHBMUBF TO ASCDSCDA                    00278500
278600                     IF  GCVI-TABLE-SW = 'Y'                      00278600
278700                         MOVE 'N'      TO GCVI-TABLE-SW           00278700
278800                         MOVE -1       TO ASCDSCDL                00278800
278900                     END-IF                                       00278900
279000                 END-IF                                           00279000
279100             END-IF                                               00279100
279200         ELSE                                                     00279200
279300             MOVE ZEROS TO ASCDSCDO                               00279300
279400         END-IF                                                   00279400
279500     END-IF.                                                      00279500
279600                                                                  00279600
      *---------------BISCENDING INDICATOR ----------------------------*00279833
                                                                        00279840
279900* THIS FIELD IS FOUND ON ACL AND AOL RECORDS. IT HAS DIFFERENT   *00279900
280000* FIELD VALIDATION VALUES AND LOGICAL EDITS.                     *00280000
280100* AS OF 01/2007 THE BISCENDING INDICATOR WILL BE ON #ABM, #ACP,   00280100
280200* AND #ADL TABULARS. THEY WILL HAVE THE SAME FIELD VALIDATION     00280200
280300* VALUES AND LOGICAL EDITS.                                       00280300
280400     IF  FUNCTONI = 'GA1C'                                        00280400
280500         IF  BISNDINL > ZERO                                      00280500
280600             MOVE 'AACL03'  TO GCVI-FIELDS-KEY-ID                 00280600
280700             MOVE BISNDINI  TO GCVI-VALUE-LEN-1                   00280700
280800             PERFORM 1200-000-LINK-TO-GCVIOPGM                    00280800
280900             IF  GCVI-VALUE-NOT-FOUND                             00280900
281000                 MOVE DFHBMUBF TO BISNDINA                        00281000
281100                 IF  ACWA-SCREEN-HAS-ERRORS                       00281100
281200                     NEXT SENTENCE                                00281200
281300                 ELSE                                             00281300
281400                     MOVE 'Y'        TO ACWA-ERROR-SW             00281400
281500                     MOVE -1         TO BISNDINL                  00281500
281600                     SET WT-01-INDEX TO +61                       00281600
281700             ELSE                                                 00281700
281800                 IF  GCVI-VALUE-NOT-LOADED                        00281800
281900                     MOVE DFHBMUBF TO BISNDINA                    00281900
282000                     IF  GCVI-TABLE-SW = 'Y'                      00282000
282100                         MOVE 'N'      TO GCVI-TABLE-SW           00282100
282200                         MOVE -1       TO BISNDINL                00282200
282300                     END-IF                                       00282300
282400                 END-IF                                           00282400
282500             END-IF                                               00282500
282600         ELSE                                                     00282600
282700             MOVE ZEROS TO BISNDINO                               00282700
282800         END-IF                                                   00282800
282900     ELSE                                                         00282900
283000     IF  FUNCTONI = 'GA1E'                                        00283000
283100         IF  BISNDINL > ZERO                                      00283100
283200             MOVE 'AAOL05'  TO GCVI-FIELDS-KEY-ID                 00283200
283300             MOVE BISNDINI  TO GCVI-VALUE-LEN-1                   00283300
283400             PERFORM 1200-000-LINK-TO-GCVIOPGM                    00283400
283500             IF  GCVI-VALUE-NOT-FOUND                             00283500
283600                 MOVE DFHBMUBF TO BISNDINA                        00283600
283700                 IF  ACWA-SCREEN-HAS-ERRORS                       00283700
283800                     NEXT SENTENCE                                00283800
283900                 ELSE                                             00283900
284000                     MOVE 'Y'        TO ACWA-ERROR-SW             00284000
284100                     MOVE -1         TO BISNDINL                  00284100
284200                     SET WT-01-INDEX TO +61                       00284200
284300             ELSE                                                 00284300
284400                 IF  GCVI-VALUE-NOT-LOADED                        00284400
284500                     MOVE DFHBMUBF TO BISNDINA                    00284500
284600                     IF  GCVI-TABLE-SW = 'Y'                      00284600
284700                         MOVE 'N'      TO GCVI-TABLE-SW           00284700
284800                         MOVE -1       TO BISNDINL                00284800
284900                     END-IF                                       00284900
285000                 END-IF                                           00285000
285100             END-IF                                               00285100
285200         ELSE                                                     00285200
285300             MOVE ZEROS TO BISNDINO.                              00285300
285400                                                                  00285400
285500                                                                  00285500
285600     IF  FUNCTONI = 'GA1B'                                        00285600
285700         IF  BISNDINL > ZERO                                      00285700
285800             MOVE 'AABM22'  TO GCVI-FIELDS-KEY-ID                 00285800
285900             MOVE BISNDINI  TO GCVI-VALUE-LEN-1                   00285900
286000             PERFORM 1200-000-LINK-TO-GCVIOPGM                    00286000
286100             IF  GCVI-VALUE-NOT-FOUND                             00286100
286200                 MOVE DFHBMUBF TO BISNDINA                        00286200
286300                 IF  ACWA-SCREEN-HAS-ERRORS                       00286300
286400                     NEXT SENTENCE                                00286400
286500                 ELSE                                             00286500
286600                     MOVE 'Y'        TO ACWA-ERROR-SW             00286600
286700                     MOVE -1         TO BISNDINL                  00286700
286800                     SET WT-01-INDEX TO +61                       00286800
286900             ELSE                                                 00286900
287000                 IF  GCVI-VALUE-NOT-LOADED                        00287000
287100                     MOVE DFHBMUBF TO BISNDINA                    00287100
287200                     IF  GCVI-TABLE-SW = 'Y'                      00287200
287300                         MOVE 'N'      TO GCVI-TABLE-SW           00287300
287400                         MOVE -1       TO BISNDINL                00287400
287500                     END-IF                                       00287500
287600                 END-IF                                           00287600
287700             END-IF                                               00287700
287800         ELSE                                                     00287800
287900             MOVE ZEROS TO BISNDINO.                              00287900
288000                                                                  00288000
288100     IF  FUNCTONI = 'GA1D'                                        00288100
288200         IF  BISNDINL > ZERO                                      00288200
288300             MOVE 'AADL22'  TO GCVI-FIELDS-KEY-ID                 00288300
288400             MOVE BISNDINI  TO GCVI-VALUE-LEN-1                   00288400
288500             PERFORM 1200-000-LINK-TO-GCVIOPGM                    00288500
288600             IF  GCVI-VALUE-NOT-FOUND                             00288600
288700                 MOVE DFHBMUBF TO BISNDINA                        00288700
288800                 IF  ACWA-SCREEN-HAS-ERRORS                       00288800
288900                     NEXT SENTENCE                                00288900
289000                 ELSE                                             00289000
289100                     MOVE 'Y'        TO ACWA-ERROR-SW             00289100
289200                     MOVE -1         TO BISNDINL                  00289200
289300                     SET WT-01-INDEX TO +61                       00289300
289400             ELSE                                                 00289400
289500                 IF  GCVI-VALUE-NOT-LOADED                        00289500
289600                     MOVE DFHBMUBF TO BISNDINA                    00289600
289700                     IF  GCVI-TABLE-SW = 'Y'                      00289700
289800                         MOVE 'N'      TO GCVI-TABLE-SW           00289800
289900                         MOVE -1       TO BISNDINL                00289900
290000                     END-IF                                       00290000
290100                 END-IF                                           00290100
290200             END-IF                                               00290200
290300         ELSE                                                     00290300
290400             MOVE ZEROS TO BISNDINO.                              00290400
290500                                                                  00290500
290600     IF  FUNCTONI = 'GA1P'                                        00290600
290700         IF  BISNDINL > ZERO                                      00290700
290800             MOVE 'AACP22'  TO GCVI-FIELDS-KEY-ID                 00290800
290900             MOVE BISNDINI  TO GCVI-VALUE-LEN-1                   00290900
291000             PERFORM 1200-000-LINK-TO-GCVIOPGM                    00291000
291100             IF  GCVI-VALUE-NOT-FOUND                             00291100
291200                 MOVE DFHBMUBF TO BISNDINA                        00291200
291300                 IF  ACWA-SCREEN-HAS-ERRORS                       00291300
291400                     NEXT SENTENCE                                00291400
291500                 ELSE                                             00291500
291600                     MOVE 'Y'        TO ACWA-ERROR-SW             00291600
291700                     MOVE -1         TO BISNDINL                  00291700
291800                     SET WT-01-INDEX TO +61                       00291800
291900             ELSE                                                 00291900
292000                 IF  GCVI-VALUE-NOT-LOADED                        00292000
292100                     MOVE DFHBMUBF TO BISNDINA                    00292100
292200                     IF  GCVI-TABLE-SW = 'Y'                      00292200
292300                         MOVE 'N'      TO GCVI-TABLE-SW           00292300
292400                         MOVE -1       TO BISNDINL                00292400
292500                     END-IF                                       00292500
292600                 END-IF                                           00292600
292700             END-IF                                               00292700
292800         ELSE                                                     00292800
292900             MOVE ZEROS TO BISNDINO.                              00292900
293000                                                                  00293000
293100*------------------- LOGICAL EDIT ------------------------------* 00293100
293200*                                                               * 00293200
293300* LOGICAL EDIT COMPARING BISCENDING INDICATOR WITH              * 00293300
293400* ASCEND/DESCEND INDICATOR.                                     * 00293400
293500*                                                               * 00293500
293600*      IF BISCENDING INDICATOR IS CODED                         * 00293600
293700*          THEN THE ASCEND/DESCEND INDICATOR MUST BE = '3'      * 00293700
293800*      ELSE                                                     * 00293800
293900*      IF BISCENDING INDICATOR IS NOT CODED                     * 00293900
294000*          THEN THE ASCEND/DESCEND INDICATOR MUST NOT BE = '3'. * 00294000
294100*---------------------------------------------------------------* 00294100
294200                                                                  00294200
294300     IF  FUNCTONI = 'GA1C'                                        00294300
294400         IF  BISNDINI NOT = ZERO                                  00294400
294500             IF ASCDSCDI NOT = '3'                                00294500
294600                 MOVE DFHBMUBF TO BISNDINA, ASCDSCDA              00294600
294700                 IF  ACWA-SCREEN-HAS-ERRORS                       00294700
294800                     NEXT SENTENCE                                00294800
294900                 ELSE                                             00294900
295000                     MOVE 'Y'        TO ACWA-ERROR-SW             00295000
295100                     MOVE -1         TO ASCDSCDL                  00295100
295200                     SET WT-01-INDEX TO +63                       00295200
295300             ELSE                                                 00295300
295400                 NEXT SENTENCE                                    00295400
295500         ELSE                                                     00295500
295600             IF ASCDSCDI  = '3'                                   00295600
295700                 MOVE DFHBMUBF TO BISNDINA, ASCDSCDA              00295700
295800                 IF  ACWA-SCREEN-HAS-ERRORS                       00295800
295900                     NEXT SENTENCE                                00295900
296000                 ELSE                                             00296000
296100                     MOVE 'Y'        TO ACWA-ERROR-SW             00296100
296200                     MOVE -1         TO BISNDINL                  00296200
296300                     SET WT-01-INDEX TO +62.                      00296300
296400                                                                  00296400
296500     IF  FUNCTONI = 'GA1E'                                        00296500
296600         IF  BISNDINI NOT = ZERO                                  00296600
296700             IF ASCDSCDI NOT = '3'                                00296700
296800                 MOVE DFHBMUBF TO BISNDINA, ASCDSCDA              00296800
296900                 IF  ACWA-SCREEN-HAS-ERRORS                       00296900
297000                     NEXT SENTENCE                                00297000
297100                 ELSE                                             00297100
297200                     MOVE 'Y'        TO ACWA-ERROR-SW             00297200
297300                     MOVE -1         TO ASCDSCDL                  00297300
297400                     SET WT-01-INDEX TO +63                       00297400
297500             ELSE                                                 00297500
297600                 NEXT SENTENCE                                    00297600
297700         ELSE                                                     00297700
297800             IF ASCDSCDI = '3'                                    00297800
297900                 MOVE DFHBMUBF TO BISNDINA, ASCDSCDA              00297900
298000                 IF  ACWA-SCREEN-HAS-ERRORS                       00298000
298100                     NEXT SENTENCE                                00298100
298200                 ELSE                                             00298200
298300                     MOVE 'Y'        TO ACWA-ERROR-SW             00298300
298400                     MOVE -1         TO BISNDINL                  00298400
298500                     SET WT-01-INDEX TO +62.                      00298500
298600                                                                  00298600
298700     IF  FUNCTONI = 'GA1B'                                        00298700
298800         IF  BISNDINI NOT = ZERO                                  00298800
298900             IF ASCDSCDI NOT = '3'                                00298900
299000                 MOVE DFHBMUBF TO BISNDINA, ASCDSCDA              00299000
299100                 IF  ACWA-SCREEN-HAS-ERRORS                       00299100
299200                     NEXT SENTENCE                                00299200
299300                 ELSE                                             00299300
299400                     MOVE 'Y'        TO ACWA-ERROR-SW             00299400
299500                     MOVE -1         TO ASCDSCDL                  00299500
299600                     SET WT-01-INDEX TO +63                       00299600
299700             ELSE                                                 00299700
299800                 NEXT SENTENCE                                    00299800
299900         ELSE                                                     00299900
300000             IF ASCDSCDI  = '3'                                   00300000
300100                 MOVE DFHBMUBF TO BISNDINA, ASCDSCDA              00300100
300200                 IF  ACWA-SCREEN-HAS-ERRORS                       00300200
300300                     NEXT SENTENCE                                00300300
300400                 ELSE                                             00300400
300500                     MOVE 'Y'        TO ACWA-ERROR-SW             00300500
300600                     MOVE -1         TO BISNDINL                  00300600
300700                     SET WT-01-INDEX TO +62.                      00300700
300800                                                                  00300800
300900     IF  FUNCTONI = 'GA1D'                                        00300900
301000         IF  BISNDINI NOT = ZERO                                  00301000
301100             IF ASCDSCDI NOT = '3'                                00301100
301200                 MOVE DFHBMUBF TO BISNDINA, ASCDSCDA              00301200
301300                 IF  ACWA-SCREEN-HAS-ERRORS                       00301300
301400                     NEXT SENTENCE                                00301400
301500                 ELSE                                             00301500
301600                     MOVE 'Y'        TO ACWA-ERROR-SW             00301600
301700                     MOVE -1         TO ASCDSCDL                  00301700
301800                     SET WT-01-INDEX TO +63                       00301800
301900             ELSE                                                 00301900
302000                 NEXT SENTENCE                                    00302000
302100         ELSE                                                     00302100
302200             IF ASCDSCDI = '3'                                    00302200
302300                 MOVE DFHBMUBF TO BISNDINA, ASCDSCDA              00302300
302400                 IF  ACWA-SCREEN-HAS-ERRORS                       00302400
302500                     NEXT SENTENCE                                00302500
302600                 ELSE                                             00302600
302700                     MOVE 'Y'        TO ACWA-ERROR-SW             00302700
302800                     MOVE -1         TO BISNDINL                  00302800
302900                     SET WT-01-INDEX TO +62.                      00302900
303000                                                                  00303000
303100     IF  FUNCTONI = 'GA1P'                                        00303100
303200         IF  BISNDINI NOT = ZERO                                  00303200
303300             IF ASCDSCDI NOT = '3'                                00303300
303400                 MOVE DFHBMUBF TO BISNDINA, ASCDSCDA              00303400
303500                 IF  ACWA-SCREEN-HAS-ERRORS                       00303500
303600                     NEXT SENTENCE                                00303600
303700                 ELSE                                             00303700
303800                     MOVE 'Y'        TO ACWA-ERROR-SW             00303800
303900                     MOVE -1         TO ASCDSCDL                  00303900
304000                     SET WT-01-INDEX TO +63                       00304000
304100             ELSE                                                 00304100
304200                 NEXT SENTENCE                                    00304200
304300         ELSE                                                     00304300
304400             IF ASCDSCDI = '3'                                    00304400
304500                 MOVE DFHBMUBF TO BISNDINA, ASCDSCDA              00304500
304600                 IF  ACWA-SCREEN-HAS-ERRORS                       00304600
304700                     NEXT SENTENCE                                00304700
304800                 ELSE                                             00304800
304900                     MOVE 'Y'        TO ACWA-ERROR-SW             00304900
305000                     MOVE -1         TO BISNDINL                  00305000
305100                     SET WT-01-INDEX TO +62.                      00305100
305200                                                                  00305200
305300*------------------ INTERVAL TYPE -------------------------------*00305300
305400                                                                  00305400
305500     IF  INTTYPEL > ZERO                                          00305500
305600     THEN                                                         00305600
305700         MOVE 'ACCM07'  TO GCVI-FIELDS-KEY-ID                     00305700
305800         MOVE INTTYPEI  TO GCVI-VALUE-LEN-2                       00305800
305900         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00305900
306000         IF  GCVI-VALUE-NOT-FOUND                                 00306000
306100         THEN                                                     00306100
306200             MOVE DFHBMUBF TO INTTYPEA                            00306200
306300             IF  ACWA-SCREEN-HAS-ERRORS                           00306300
306400             THEN                                                 00306400
306500                 NEXT SENTENCE                                    00306500
306600             ELSE                                                 00306600
306700                 MOVE 'Y'      TO ACWA-ERROR-SW                   00306700
306800                 MOVE -1       TO INTTYPEL                        00306800
306900                 SET WT-01-INDEX TO +19                           00306900
307000         ELSE                                                     00307000
307100             IF  GCVI-VALUE-NOT-LOADED                            00307100
307200             THEN                                                 00307200
307300                 MOVE DFHBMUBF TO INTTYPEA                        00307300
307400                 IF  GCVI-TABLE-SW = 'Y'                          00307400
307500                 THEN                                             00307500
307600                     MOVE 'N'      TO GCVI-TABLE-SW               00307600
307700                     MOVE -1       TO INTTYPEL                    00307700
307800                 ELSE                                             00307800
307900                     NEXT SENTENCE                                00307900
308000             ELSE                                                 00308000
308100                 NEXT SENTENCE                                    00308100
308200     ELSE                                                         00308200
308300         MOVE ZEROS TO INTTYPEO.                                  00308300
308400                                                                  00308400
308500                                                                  00308500
308600*------------------ INTERVAL TIME FACTOR ------------------------*00308600
308700                                                                  00308700
308800     IF  INTRVALL  > ZERO                                         00308800
308900     THEN                                                         00308900
309000         IF INTRVALI  NOT  NUMERIC                                00309000
309100         THEN                                                     00309100
309200             MOVE DFHBMUBF  TO  INTRVALA                          00309200
309300             IF ACWA-SCREEN-HAS-ERRORS                            00309300
309400             THEN                                                 00309400
309500                 NEXT SENTENCE                                    00309500
309600             ELSE                                                 00309600
309700                 SET WT-01-INDEX TO +20                           00309700
309800                 MOVE -1         TO INTRVALL                      00309800
309900                 MOVE 'Y'        TO ACWA-ERROR-SW                 00309900
310000        ELSE                                                      00310000
310100            NEXT SENTENCE                                         00310100
310200     ELSE                                                         00310200
310300         MOVE ZEROS TO INTRVALO.                                  00310300
310400                                                                  00310400
310500                                                                  00310500
310600*------------------- LOGICAL EDIT ------------------------------* 00310600
310700*                                                               * 00310700
310800* LOGICAL EDIT COMPARING INTERVAL TYPE AND INTERVAL TIME FACTOR * 00310800
310900*                                                               * 00310900
311000*---------------------------------------------------------------* 00311000
311100                                                                  00311100
311200     IF  INTTYPEI > ZEROS                                         00311200
311300     THEN                                                         00311300
311400         IF  INTRVALI > ZEROS                                     00311400
311500         THEN                                                     00311500
311600             NEXT SENTENCE                                        00311600
311700         ELSE                                                     00311700
311800             MOVE DFHBMUBF TO INTRVALA                            00311800
311900             IF  ACWA-SCREEN-HAS-ERRORS                           00311900
312000             THEN                                                 00312000
312100                 NEXT SENTENCE                                    00312100
312200             ELSE                                                 00312200
312300                 MOVE 'Y'        TO ACWA-ERROR-SW                 00312300
312400                 MOVE -1         TO INTRVALL                      00312400
312500                 SET WT-01-INDEX TO +21                           00312500
312600     ELSE                                                         00312600
312700         NEXT SENTENCE.                                           00312700
312800                                                                  00312800
312900     IF  INTRVALI > ZEROS                                         00312900
313000     THEN                                                         00313000
313100         IF  INTTYPEI = ZEROS                                     00313100
313200         THEN                                                     00313200
313300             MOVE DFHBMUBF TO INTTYPEA                            00313300
313400             IF  ACWA-SCREEN-HAS-ERRORS                           00313400
313500             THEN                                                 00313500
313600                 NEXT SENTENCE                                    00313600
313700             ELSE                                                 00313700
313800                 MOVE 'Y'        TO ACWA-ERROR-SW                 00313800
313900                 MOVE -1         TO INTTYPEL                      00313900
314000                 SET WT-01-INDEX TO +22                           00314000
314100         ELSE                                                     00314100
314200             NEXT SENTENCE                                        00314200
314300     ELSE                                                         00314300
314400         NEXT SENTENCE.                                           00314400
314500                                                                  00314500
314600                                                                  00314600
314700*------------------ INTERVAL OVERRIDE INDICATOR -----------------*00314700
314800                                                                  00314800
314900     IF  OVRDINDL > ZERO                                          00314900
315000     THEN                                                         00315000
315100         MOVE 'ACCM08'  TO GCVI-FIELDS-KEY-ID                     00315100
315200         MOVE OVRDINDI  TO GCVI-VALUE-LEN-1                       00315200
315300         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00315300
315400         IF  GCVI-VALUE-NOT-FOUND                                 00315400
315500         THEN                                                     00315500
315600             MOVE DFHBMUBF TO OVRDINDA                            00315600
315700             IF  ACWA-SCREEN-HAS-ERRORS                           00315700
315800             THEN                                                 00315800
315900                 NEXT SENTENCE                                    00315900
316000             ELSE                                                 00316000
316100                 MOVE 'Y'        TO ACWA-ERROR-SW                 00316100
316200                 MOVE -1         TO OVRDINDL                      00316200
316300                 SET WT-01-INDEX TO +23                           00316300
316400         ELSE                                                     00316400
316500             IF  GCVI-VALUE-NOT-LOADED                            00316500
316600             THEN                                                 00316600
316700                 MOVE DFHBMUBF TO INTTYPEA                        00316700
316800                 IF  GCVI-TABLE-SW = 'Y'                          00316800
316900                 THEN                                             00316900
317000                     MOVE 'N'      TO GCVI-TABLE-SW               00317000
317100                     MOVE -1       TO INTTYPEL                    00317100
317200                 ELSE                                             00317200
317300                     NEXT SENTENCE                                00317300
317400             ELSE                                                 00317400
317500                 NEXT SENTENCE                                    00317500
317600     ELSE                                                         00317600
317700         MOVE ZEROS TO OVRDINDO.                                  00317700
317800                                                                  00317800
317900                                                                  00317900
318000*------------------ INTERVAL OVERRIDE VALUE ---------------------*00318000
318100                                                                  00318100
318200     IF  NEWVALUL  > ZERO                                         00318200
318300     THEN                                                         00318300
318400         IF  NEWVALUI  NOT  NUMERIC                               00318400
318500         THEN                                                     00318500
318600             MOVE DFHBMUBF  TO  NEWVALUA                          00318600
318700             IF  ACWA-SCREEN-HAS-ERRORS                           00318700
318800             THEN                                                 00318800
318900                 NEXT SENTENCE                                    00318900
319000             ELSE                                                 00319000
319100                 SET WT-01-INDEX TO +24                           00319100
319200                 MOVE -1         TO NEWVALUL                      00319200
319300                 MOVE 'Y'        TO ACWA-ERROR-SW                 00319300
319400         ELSE                                                     00319400
319500             NEXT SENTENCE                                        00319500
319600     ELSE                                                         00319600
319700         MOVE ZEROS TO NEWVALUO.                                  00319700
319800                                                                  00319800
319900                                                                  00319900
320000*------------------- LOGICAL EDIT ------------------------------* 00320000
320100*                                                               * 00320100
320200* LOGICAL EDIT COMPARING INTERVAL OVERIDE INDICATOR AND INTERVAL* 00320200
320300* OVERRIDE VALUE                                                * 00320300
320400*                                                               * 00320400
320500*---------------------------------------------------------------* 00320500
320600                                                                  00320600
320700     IF  OVRDINDI NOT = ZEROS                                     00320700
320800     THEN                                                         00320800
320900         IF  NEWVALUI > ZEROS                                     00320900
321000         THEN                                                     00321000
321100             NEXT SENTENCE                                        00321100
321200         ELSE                                                     00321200
321300             MOVE DFHBMUBF TO NEWVALUA                            00321300
321400             IF  ACWA-SCREEN-HAS-ERRORS                           00321400
321500             THEN                                                 00321500
321600                 NEXT SENTENCE                                    00321600
321700             ELSE                                                 00321700
321800                 MOVE 'Y'        TO ACWA-ERROR-SW                 00321800
321900                 MOVE -1         TO NEWVALUL                      00321900
322000                 SET WT-01-INDEX TO +25                           00322000
322100     ELSE                                                         00322100
322200         NEXT SENTENCE.                                           00322200
322300                                                                  00322300
322400     IF  NEWVALUI > ZEROS                                         00322400
322500     THEN                                                         00322500
322600         IF  OVRDINDI NOT = ZEROS                                 00322600
322700         THEN                                                     00322700
322800             NEXT SENTENCE                                        00322800
322900         ELSE                                                     00322900
323000             MOVE DFHBMUBF TO OVRDINDA                            00323000
323100             IF  ACWA-SCREEN-HAS-ERRORS                           00323100
323200             THEN                                                 00323200
323300                 NEXT SENTENCE                                    00323300
323400             ELSE                                                 00323400
323500                 MOVE 'Y'        TO ACWA-ERROR-SW                 00323500
323600                 MOVE -1         TO OVRDINDL                      00323600
323700                 SET WT-01-INDEX TO +01                           00323700
323800     ELSE                                                         00323800
323900         NEXT SENTENCE.                                           00323900
324000                                                                  00324000
324100                                                                  00324100
324200*------------------ REINSTATEMENT INDICATOR ---------------------*00324200
324300                                                                  00324300
324400     IF  REININDL > ZERO                                          00324400
324500     THEN                                                         00324500
324600         MOVE 'AABM04'  TO GCVI-FIELDS-KEY-ID                     00324600
324700         MOVE REININDI  TO GCVI-VALUE-LEN-1                       00324700
324800         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00324800
324900         IF  GCVI-VALUE-NOT-FOUND                                 00324900
325000         THEN                                                     00325000
325100             MOVE DFHBMUBF TO REININDA                            00325100
325200             IF  ACWA-SCREEN-HAS-ERRORS                           00325200
325300             THEN                                                 00325300
325400                 NEXT SENTENCE                                    00325400
325500             ELSE                                                 00325500
325600                 MOVE 'Y'        TO ACWA-ERROR-SW                 00325600
325700                 MOVE -1         TO REININDL                      00325700
325800                 SET WT-01-INDEX TO +33                           00325800
325900         ELSE                                                     00325900
326000             IF  GCVI-VALUE-NOT-LOADED                            00326000
326100             THEN                                                 00326100
326200                 MOVE DFHBMUBF TO REININDA                        00326200
326300                 IF  GCVI-TABLE-SW = 'Y'                          00326300
326400                 THEN                                             00326400
326500                     MOVE 'N'      TO GCVI-TABLE-SW               00326500
326600                     MOVE -1       TO REININDL                    00326600
326700                 ELSE                                             00326700
326800                     NEXT SENTENCE                                00326800
326900             ELSE                                                 00326900
327000                 NEXT SENTENCE                                    00327000
327100     ELSE                                                         00327100
327200         MOVE ZEROS TO REININDO.                                  00327200
327300                                                                  00327300
327400                                                                  00327400
327500*------------------ MANDATORY INDICATOR -------------------------*00327500
327600                                                                  00327600
327700     IF  FUNCTONI = 'GA1C' OR 'GA1D' OR 'GA1P'                    00327700
327800     THEN                                                         00327800
327900         IF  MANAPLIL > ZERO                                      00327900
328000         THEN                                                     00328000
328100             MOVE 'ACCM10'  TO GCVI-FIELDS-KEY-ID                 00328100
328200             MOVE MANAPLII  TO GCVI-VALUE-LEN-1                   00328200
328300             PERFORM 1200-000-LINK-TO-GCVIOPGM                    00328300
328400             IF  GCVI-VALUE-NOT-FOUND                             00328400
328500             THEN                                                 00328500
328600                 MOVE DFHBMUBF TO MANAPLIA                        00328600
328700                 IF  ACWA-SCREEN-HAS-ERRORS                       00328700
328800                 THEN                                             00328800
328900                     NEXT SENTENCE                                00328900
329000                 ELSE                                             00329000
329100                     MOVE 'Y'      TO ACWA-ERROR-SW               00329100
329200                     MOVE DFHBMUBF TO MANAPLIA                    00329200
329300                     MOVE -1       TO MANAPLIL                    00329300
329400                     SET WT-01-INDEX TO +56                       00329400
329500             ELSE                                                 00329500
329600                 IF  GCVI-VALUE-NOT-LOADED                        00329600
329700                 THEN                                             00329700
329800                     MOVE DFHBMUBF     TO MANAPLIA                00329800
329900                     IF GCVI-TABLE-SW = 'Y'                       00329900
330000                         MOVE 'N'      TO GCVI-TABLE-SW           00330000
330100                         MOVE -1       TO MANAPLIL                00330100
330200                     ELSE                                         00330200
330300                         NEXT SENTENCE                            00330300
330400                 ELSE                                             00330400
330500                     NEXT SENTENCE                                00330500
330600         ELSE                                                     00330600
330700             MOVE DFHBMUBF TO MANAPLIA                            00330700
330800             IF ACWA-SCREEN-HAS-ERRORS                            00330800
330900             THEN                                                 00330900
331000                 NEXT SENTENCE                                    00331000
331100             ELSE                                                 00331100
331200                 MOVE 'Y'        TO  ACWA-ERROR-SW                00331200
331300                 MOVE -1         TO  MANAPLIL                     00331300
331400                 SET WT-01-INDEX TO +57                           00331400
331500     ELSE                                                         00331500
331600         NEXT SENTENCE.                                           00331600
331700                                                                  00331700
331800                                                                  00331800
331900*------------------ BENEFIT PERIOD MAX OVERRIDE INDICATOR -------*00331900
332000                                                                  00332000
332100     IF  MAXOVRDL > ZERO                                          00332100
332200     THEN                                                         00332200
332300         MOVE 'AABM05'  TO GCVI-FIELDS-KEY-ID                     00332300
332400         MOVE MAXOVRDI  TO GCVI-VALUE-LEN-1                       00332400
332500         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00332500
332600         IF  GCVI-VALUE-NOT-FOUND                                 00332600
332700         THEN                                                     00332700
332800             MOVE DFHBMUBF TO MAXOVRDA                            00332800
332900             MOVE -1       TO MAXOVRDL                            00332900
333000             IF  ACWA-SCREEN-HAS-ERRORS                           00333000
333100             THEN                                                 00333100
333200                 NEXT SENTENCE                                    00333200
333300             ELSE                                                 00333300
333400                 MOVE 'Y'        TO ACWA-ERROR-SW                 00333400
333500                 SET WT-01-INDEX TO +11                           00333500
333600         ELSE                                                     00333600
333700             IF  GCVI-VALUE-NOT-LOADED                            00333700
333800             THEN                                                 00333800
333900                 MOVE DFHBMUBF TO MAXOVRDA                        00333900
334000                 IF GCVI-TABLE-SW = 'Y'                           00334000
334100                 THEN                                             00334100
334200                     MOVE 'N'      TO GCVI-TABLE-SW               00334200
334300                     MOVE -1       TO MAXOVRDL                    00334300
334400                 ELSE                                             00334400
334500                     NEXT SENTENCE                                00334500
334600             ELSE                                                 00334600
334700                 NEXT SENTENCE                                    00334700
334800     ELSE                                                         00334800
334900         MOVE ZEROS TO MAXOVRDO.                                  00334900
335000                                                                  00335000
335100                                                                  00335100
335200*------------------ CARRYOVER CREDIT INDICATOR ------------------*00335200
335300                                                                  00335300
335400     IF  FUNCTONI = 'GA1C'                                        00335400
335500         MOVE 'AACL04'  TO GCVI-FIELDS-KEY-ID.                    00335500
335600                                                                  00335600
335700     IF  FUNCTONI = 'GA1D'                                        00335700
335800         MOVE 'AADL04'  TO GCVI-FIELDS-KEY-ID.                    00335800
335900                                                                  00335900
336000     IF  FUNCTONI = 'GA1E'                                        00336000
336100         MOVE 'AAOL04'  TO GCVI-FIELDS-KEY-ID.                    00336100
336200                                                                  00336200
336300     IF  FUNCTONI = 'GA1P'                                        00336300
336400         MOVE 'AADL04'  TO GCVI-FIELDS-KEY-ID.                    00336400
336500                                                                  00336500
336600     IF  FUNCTONI = 'GA1D' OR 'GA1E' OR 'GA1P' OR 'GA1C'          00336600
336700     THEN                                                         00336700
336800         IF  CARYOVRL > ZERO                                      00336800
336900         THEN                                                     00336900
337000             MOVE CARYOVRI  TO GCVI-VALUE-LEN-1                   00337000
337100             PERFORM 1200-000-LINK-TO-GCVIOPGM                    00337100
337200             IF  GCVI-VALUE-NOT-FOUND                             00337200
337300             THEN                                                 00337300
337400                 MOVE DFHBMUBF TO CARYOVRA                        00337400
337500                 MOVE -1       TO CARYOVRL                        00337500
337600                 IF  ACWA-SCREEN-HAS-ERRORS                       00337600
337700                 THEN                                             00337700
337800                     NEXT SENTENCE                                00337800
337900                 ELSE                                             00337900
338000                     MOVE 'Y'        TO ACWA-ERROR-SW             00338000
338100                     SET WT-01-INDEX TO +58                       00338100
338200             ELSE                                                 00338200
338300                 IF  GCVI-VALUE-NOT-LOADED                        00338300
338400                 THEN                                             00338400
338500                     MOVE DFHBMUBF TO CARYOVRA                    00338500
338600                     IF GCVI-TABLE-SW = 'Y'                       00338600
338700                     THEN                                         00338700
338800                         MOVE 'N'      TO GCVI-TABLE-SW           00338800
338900                         MOVE -1       TO CARYOVRL                00338900
339000                     ELSE                                         00339000
339100                         NEXT SENTENCE                            00339100
339200                 ELSE                                             00339200
339300                     NEXT SENTENCE                                00339300
339400         ELSE                                                     00339400
339500             MOVE ZEROS TO CARYOVRO                               00339500
339600     ELSE                                                         00339600
339700         NEXT SENTENCE.                                           00339700
339800                                                                  00339800
339900                                                                  00339900
340000*------------------ FOR YOUR INFORMATION (FYI) ------------------*00340000
340100                                                                  00340100
340200     IF  FYIVALL > ZEROES                                         00340200
340300     THEN                                                         00340300
340400         MOVE 'ACCM16'   TO GCVI-FIELDS-KEY-ID                    00340400
340500         MOVE FYIVALI    TO GCVI-VALUE-LEN-3                      00340500
340600         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00340600
340700         IF  GCVI-VALUE-NOT-FOUND                                 00340700
340800         THEN                                                     00340800
340900             MOVE DFHBMUBF TO FYIVALA                             00340900
341000             IF  ACWA-SCREEN-HAS-ERRORS                           00341000
341100             THEN                                                 00341100
341200                 NEXT SENTENCE                                    00341200
341300             ELSE                                                 00341300
341400                 MOVE 'Y'        TO ACWA-ERROR-SW                 00341400
341500                 MOVE -1         TO FYIVALL                       00341500
341600                 SET WT-01-INDEX TO +37                           00341600
341700         ELSE                                                     00341700
341800             IF  GCVI-VALUE-NOT-LOADED                            00341800
341900             THEN                                                 00341900
342000                 MOVE DFHBMUBF     TO FYIVALA                     00342000
342100                 IF  ACWA-SCREEN-HAS-ERRORS                       00342100
342200                 THEN                                             00342200
342300                     NEXT SENTENCE                                00342300
342400                 ELSE                                             00342400
342500                     MOVE 'N'      TO GCVI-TABLE-SW               00342500
342600                     MOVE -1       TO FYIVALL                     00342600
342700             ELSE                                                 00342700
342800                 NEXT SENTENCE                                    00342800
342900     ELSE                                                         00342900
343000         MOVE ZEROES TO FYIVALO.                                  00343000
343100                                                                  00343100
343200                                                                  00343200
343300*------------------ FIRST DOLLAR COVERAGE LIMIT INDICATOR -------*00343300
343400                                                                  00343400
343500     IF  FUNCTONI = 'GA1C'                                        00343500
343600     THEN                                                         00343600
343700         IF  FDLRCLIL > ZERO                                      00343700
343800         THEN                                                     00343800
343900             MOVE 'AACL20'  TO GCVI-FIELDS-KEY-ID                 00343900
344000             MOVE FDLRCLII  TO GCVI-VALUE-LEN-1                   00344000
344100             PERFORM 1200-000-LINK-TO-GCVIOPGM                    00344100
344200             IF  GCVI-VALUE-NOT-FOUND                             00344200
344300             THEN                                                 00344300
344400                 MOVE DFHBMUBF TO FDLRCLIA                        00344400
344500                 MOVE -1       TO FDLRCLIL                        00344500
344600                 IF  ACWA-SCREEN-HAS-ERRORS                       00344600
344700                 THEN                                             00344700
344800                     NEXT SENTENCE                                00344800
344900                 ELSE                                             00344900
345000                     MOVE 'Y'        TO ACWA-ERROR-SW             00345000
345100                     SET WT-01-INDEX TO +59                       00345100
345200             ELSE                                                 00345200
345300                 IF  GCVI-VALUE-NOT-LOADED                        00345300
345400                 THEN                                             00345400
345500                     MOVE DFHBMUBF TO FDLRCLIA                    00345500
345600                     IF GCVI-TABLE-SW = 'Y'                       00345600
345700                     THEN                                         00345700
345800                         MOVE 'N'      TO GCVI-TABLE-SW           00345800
345900                         MOVE -1       TO FDLRCLIL                00345900
346000                     ELSE                                         00346000
346100                         NEXT SENTENCE                            00346100
346200                 ELSE                                             00346200
346300                     NEXT SENTENCE                                00346300
346400         ELSE                                                     00346400
346500             MOVE ZEROS TO FDLRCLIO                               00346500
346600     ELSE                                                         00346600
346700         NEXT SENTENCE.                                           00346700
346800                                                                  00346800
346900                                                                  00346900
347000                                                                  00347000
347100*------------------ EDIT CONDITION BITS -------------------------*00347100
347200                                                                  00347200
347300 1100-050-EDIT-CONDALL.                                           00347300
347400                                                                  00347400
347500     IF  CONDALLL < 1                                             00347500
347600     THEN                                                         00347600
347700         MOVE '0' TO CONDALLO                                     00347700
347800     ELSE                                                         00347800
347900         IF  CONDALLI = '0' OR '1'                                00347900
348000         THEN                                                     00348000
348100             NEXT SENTENCE                                        00348100
348200         ELSE                                                     00348200
348300             MOVE DFHBMUBF  TO  CONDALLA                          00348300
348400             IF  ACWA-SCREEN-HAS-ERRORS                           00348400
348500             THEN                                                 00348500
348600                 NEXT SENTENCE                                    00348600
348700             ELSE                                                 00348700
348800                 MOVE 'Y'        TO ACWA-ERROR-SW                 00348800
348900                 MOVE -1         TO CONDALLL                      00348900
349000                 SET WT-01-INDEX TO +41.                          00349000
349100                                                                  00349100
349200     IF  CONDEXCL < 1                                             00349200
349300     THEN                                                         00349300
349400         MOVE '0' TO CONDEXCO                                     00349400
349500     ELSE                                                         00349500
349600         IF  CONDEXCI = '0' OR '1'                                00349600
349700         THEN                                                     00349700
349800             NEXT SENTENCE                                        00349800
349900         ELSE                                                     00349900
350000             MOVE DFHBMUBF  TO  CONDEXCA                          00350000
350100             IF  ACWA-SCREEN-HAS-ERRORS                           00350100
350200             THEN                                                 00350200
350300                 NEXT SENTENCE                                    00350300
350400             ELSE                                                 00350400
350500                 MOVE 'Y'        TO ACWA-ERROR-SW                 00350500
350600                 MOVE -1         TO CONDEXCL                      00350600
350700                 SET WT-01-INDEX TO +41.                          00350700
350800                                                                  00350800
350900     IF  CONDICDL < 1                                             00350900
351000     THEN                                                         00351000
351100         MOVE '0' TO CONDICDO                                     00351100
351200     ELSE                                                         00351200
351300         IF  CONDICDI = '0' OR '1'                                00351300
351400         THEN                                                     00351400
351500             NEXT SENTENCE                                        00351500
351600         ELSE                                                     00351600
351700             MOVE DFHBMUBF  TO  CONDICDA                          00351700
351800             IF  ACWA-SCREEN-HAS-ERRORS                           00351800
351900             THEN                                                 00351900
352000                 NEXT SENTENCE                                    00352000
352100             ELSE                                                 00352100
352200                 MOVE 'Y'        TO ACWA-ERROR-SW                 00352200
352300                 MOVE -1         TO CONDICDL                      00352300
352400                 SET WT-01-INDEX TO +41.                          00352400
352500                                                                  00352500
352600     IF  CONDTABL < 1                                             00352600
352700     THEN                                                         00352700
352800         MOVE '0' TO CONDTABO                                     00352800
352900     ELSE                                                         00352900
353000         IF  CONDTABI = '0' OR '1'                                00353000
353100         THEN                                                     00353100
353200             NEXT SENTENCE                                        00353200
353300         ELSE                                                     00353300
353400             MOVE DFHBMUBF  TO  CONDTABA                          00353400
353500             IF  ACWA-SCREEN-HAS-ERRORS                           00353500
353600             THEN                                                 00353600
353700                 NEXT SENTENCE                                    00353700
353800             ELSE                                                 00353800
353900                 MOVE 'Y'        TO ACWA-ERROR-SW                 00353900
354000                 MOVE -1         TO CONDTABL                      00354000
354100                 SET WT-01-INDEX TO +41.                          00354100
354200                                                                  00354200
354300     IF  CONDMENL < 1                                             00354300
354400     THEN                                                         00354400
354500         MOVE '0' TO CONDMENO                                     00354500
354600     ELSE                                                         00354600
354700         IF  CONDMENI = '0' OR '1'                                00354700
354800         THEN                                                     00354800
354900             NEXT SENTENCE                                        00354900
355000         ELSE                                                     00355000
355100             MOVE DFHBMUBF  TO  CONDMENA                          00355100
355200             IF  ACWA-SCREEN-HAS-ERRORS                           00355200
355300             THEN                                                 00355300
355400                 NEXT SENTENCE                                    00355400
355500             ELSE                                                 00355500
355600                 MOVE 'Y'        TO ACWA-ERROR-SW                 00355600
355700                 MOVE -1         TO CONDMENL                      00355700
355800                 SET WT-01-INDEX TO +41.                          00355800
355900                                                                  00355900
356000     IF  CONDDRGL < 1                                             00356000
356100     THEN                                                         00356100
356200         MOVE '0' TO CONDDRGO                                     00356200
356300     ELSE                                                         00356300
356400         IF  CONDDRGI = '0' OR '1'                                00356400
356500         THEN                                                     00356500
356600             NEXT SENTENCE                                        00356600
356700         ELSE                                                     00356700
356800             MOVE DFHBMUBF  TO  CONDDRGA                          00356800
356900             IF  ACWA-SCREEN-HAS-ERRORS                           00356900
357000             THEN                                                 00357000
357100                 NEXT SENTENCE                                    00357100
357200             ELSE                                                 00357200
357300                 MOVE 'Y'        TO ACWA-ERROR-SW                 00357300
357400                 MOVE -1         TO CONDDRGL                      00357400
357500                 SET WT-01-INDEX TO +41.                          00357500
357600                                                                  00357600
357700     IF  CONDALCL < 1                                             00357700
357800     THEN                                                         00357800
357900         MOVE '0' TO CONDALCO                                     00357900
358000     ELSE                                                         00358000
358100         IF  CONDALCI = '0' OR '1'                                00358100
358200         THEN                                                     00358200
358300             NEXT SENTENCE                                        00358300
358400         ELSE                                                     00358400
358500             MOVE DFHBMUBF  TO  CONDALCA                          00358500
358600             IF  ACWA-SCREEN-HAS-ERRORS                           00358600
358700             THEN                                                 00358700
358800                 NEXT SENTENCE                                    00358800
358900             ELSE                                                 00358900
359000                 MOVE 'Y'        TO ACWA-ERROR-SW                 00359000
359100                 MOVE -1         TO CONDALCL                      00359100
359200                 SET WT-01-INDEX TO +41.                          00359200
359300                                                                  00359300
359400     IF  CONDOBCL < 1                                             00359400
359500     THEN                                                         00359500
359600         MOVE '0' TO CONDOBCO                                     00359600
359700     ELSE                                                         00359700
359800         IF  CONDOBCI = '0' OR '1'                                00359800
359900         THEN                                                     00359900
360000             NEXT SENTENCE                                        00360000
360100         ELSE                                                     00360100
360200             MOVE DFHBMUBF  TO  CONDOBCA                          00360200
360300             IF  ACWA-SCREEN-HAS-ERRORS                           00360300
360400             THEN                                                 00360400
360500                 NEXT SENTENCE                                    00360500
360600             ELSE                                                 00360600
360700                 MOVE 'Y'        TO ACWA-ERROR-SW                 00360700
360800                 MOVE -1         TO CONDOBCL                      00360800
360900                 SET WT-01-INDEX TO +41.                          00360900
361000                                                                  00361000
361100     IF  CONDOBNL < 1                                             00361100
361200     THEN                                                         00361200
361300         MOVE '0' TO CONDOBNO                                     00361300
361400     ELSE                                                         00361400
361500         IF  CONDOBNI = '0' OR '1'                                00361500
361600         THEN                                                     00361600
361700             NEXT SENTENCE                                        00361700
361800         ELSE                                                     00361800
361900             MOVE DFHBMUBF  TO  CONDOBNA                          00361900
362000             IF  ACWA-SCREEN-HAS-ERRORS                           00362000
362100             THEN                                                 00362100
362200                 NEXT SENTENCE                                    00362200
362300             ELSE                                                 00362300
362400                 MOVE 'Y'        TO ACWA-ERROR-SW                 00362400
362500                 MOVE -1         TO CONDOBNL                      00362500
362600                 SET WT-01-INDEX TO +41.                          00362600
362700                                                                  00362700
362800     IF  CONDMALL < 1                                             00362800
362900     THEN                                                         00362900
363000         MOVE '0' TO CONDMALO                                     00363000
363100     ELSE                                                         00363100
363200         IF  CONDMALI = '0' OR '1'                                00363200
363300         THEN                                                     00363300
363400             NEXT SENTENCE                                        00363400
363500         ELSE                                                     00363500
363600             MOVE DFHBMUBF  TO  CONDMALA                          00363600
363700             IF  ACWA-SCREEN-HAS-ERRORS                           00363700
363800             THEN                                                 00363800
363900                 NEXT SENTENCE                                    00363900
364000             ELSE                                                 00364000
364100                 MOVE 'Y'        TO ACWA-ERROR-SW                 00364100
364200                 MOVE -1         TO CONDMALL                      00364200
364300                 SET WT-01-INDEX TO +41.                          00364300
364400                                                                  00364400
364500     IF  CONDCARL < 1                                             00364500
364600     THEN                                                         00364600
364700         MOVE '0' TO CONDCARO                                     00364700
364800     ELSE                                                         00364800
364900         IF  CONDCARI = '0' OR '1'                                00364900
365000         THEN                                                     00365000
365100             NEXT SENTENCE                                        00365100
365200         ELSE                                                     00365200
365300             MOVE DFHBMUBF  TO  CONDCARA                          00365300
365400             IF  ACWA-SCREEN-HAS-ERRORS                           00365400
365500             THEN                                                 00365500
365600                 NEXT SENTENCE                                    00365600
365700             ELSE                                                 00365700
365800                 MOVE 'Y'        TO ACWA-ERROR-SW                 00365800
365900                 MOVE -1         TO CONDCARL                      00365900
366000                 SET WT-01-INDEX TO +41.                          00366000
366100                                                                  00366100
366200     IF  CONDOBSL < 1                                             00366200
366300     THEN                                                         00366300
366400         MOVE '0' TO CONDOBSO                                     00366400
366500     ELSE                                                         00366500
366600         IF  CONDOBSI = '0' OR '1'                                00366600
366700         THEN                                                     00366700
366800             NEXT SENTENCE                                        00366800
366900         ELSE                                                     00366900
367000             MOVE DFHBMUBF  TO  CONDOBSA                          00367000
367100             IF  ACWA-SCREEN-HAS-ERRORS                           00367100
367200             THEN                                                 00367200
367300                 NEXT SENTENCE                                    00367300
367400             ELSE                                                 00367400
367500                 MOVE 'Y'        TO ACWA-ERROR-SW                 00367500
367600                 MOVE -1         TO CONDOBSL                      00367600
367700                 SET WT-01-INDEX TO +41.                          00367700
367800                                                                  00367800
367900     IF  CONDKDYL < 1                                             00367900
368000     THEN                                                         00368000
368100         MOVE '0' TO CONDKDYO                                     00368100
368200     ELSE                                                         00368200
368300         IF  CONDKDYI = '0' OR '1'                                00368300
368400         THEN                                                     00368400
368500             NEXT SENTENCE                                        00368500
368600         ELSE                                                     00368600
368700             MOVE DFHBMUBF  TO  CONDKDYA                          00368700
368800             IF  ACWA-SCREEN-HAS-ERRORS                           00368800
368900             THEN                                                 00368900
369000                 NEXT SENTENCE                                    00369000
369100             ELSE                                                 00369100
369200                 MOVE 'Y'        TO ACWA-ERROR-SW                 00369200
369300                 MOVE -1         TO CONDKDYL                      00369300
369400                 SET WT-01-INDEX TO +41.                          00369400
369500                                                                  00369500
369600     IF  CONDACCL < 1                                             00369600
369700     THEN                                                         00369700
369800         MOVE '0' TO CONDACCO                                     00369800
369900     ELSE                                                         00369900
370000         IF  CONDACCI = '0' OR '1'                                00370000
370100         THEN                                                     00370100
370200             NEXT SENTENCE                                        00370200
370300         ELSE                                                     00370300
370400             MOVE DFHBMUBF  TO  CONDACCA                          00370400
370500             IF  ACWA-SCREEN-HAS-ERRORS                           00370500
370600             THEN                                                 00370600
370700                 NEXT SENTENCE                                    00370700
370800             ELSE                                                 00370800
370900                 MOVE 'Y'        TO ACWA-ERROR-SW                 00370900
371000                 MOVE -1         TO CONDACCL                      00371000
371100                 SET WT-01-INDEX TO +41.                          00371100
371200                                                                  00371200
371300     IF  CONDPECL < 1                                             00371300
371400     THEN                                                         00371400
371500         MOVE '0' TO CONDPECO                                     00371500
371600     ELSE                                                         00371600
371700         IF  CONDPECI = '0' OR '1'                                00371700
371800         THEN                                                     00371800
371900             NEXT SENTENCE                                        00371900
372000         ELSE                                                     00372000
372100             MOVE DFHBMUBF  TO  CONDPECA                          00372100
372200             IF  ACWA-SCREEN-HAS-ERRORS                           00372200
372300             THEN                                                 00372300
372400                 NEXT SENTENCE                                    00372400
372500             ELSE                                                 00372500
372600                 MOVE 'Y'        TO ACWA-ERROR-SW                 00372600
372700                 MOVE -1         TO CONDPECL                      00372700
372800                 SET WT-01-INDEX TO +41.                          00372800
372900                                                                  00372900
373000     IF  CONDNEML < 1                                             00373000
373100     THEN                                                         00373100
373200         MOVE '0' TO CONDNEMO                                     00373200
373300     ELSE                                                         00373300
373400         IF  CONDNEMI = '0' OR '1'                                00373400
373500         THEN                                                     00373500
373600             NEXT SENTENCE                                        00373600
373700         ELSE                                                     00373700
373800             MOVE DFHBMUBF  TO  CONDNEMA                          00373800
373900             IF  ACWA-SCREEN-HAS-ERRORS                           00373900
374000             THEN                                                 00374000
374100                 NEXT SENTENCE                                    00374100
374200             ELSE                                                 00374200
374300                 MOVE 'Y'        TO ACWA-ERROR-SW                 00374300
374400                 MOVE -1         TO CONDNEML                      00374400
374500                 SET WT-01-INDEX TO +41.                          00374500
374600                                                                  00374600
374700     IF  CONDSUIL < 1                                             00374700
374800     THEN                                                         00374800
374900         MOVE '0' TO CONDSUIO                                     00374900
375000     ELSE                                                         00375000
375100         IF  CONDSUII = '0' OR '1'                                00375100
375200         THEN                                                     00375200
375300             NEXT SENTENCE                                        00375300
375400         ELSE                                                     00375400
375500             MOVE DFHBMUBF  TO  CONDSUIA                          00375500
375600             IF  ACWA-SCREEN-HAS-ERRORS                           00375600
375700             THEN                                                 00375700
375800                 NEXT SENTENCE                                    00375800
375900             ELSE                                                 00375900
376000                 MOVE 'Y'        TO ACWA-ERROR-SW                 00376000
376100                 MOVE -1         TO CONDSUIL                      00376100
376200                 SET WT-01-INDEX TO +41.                          00376200
376300                                                                  00376300
376400     IF  CONDTMJL < 1                                             00376400
376500     THEN                                                         00376500
376600         MOVE '0' TO CONDTMJO                                     00376600
376700     ELSE                                                         00376700
376800         IF  CONDTMJI = '0' OR '1'                                00376800
376900         THEN                                                     00376900
377000             NEXT SENTENCE                                        00377000
377100         ELSE                                                     00377100
377200             MOVE DFHBMUBF  TO  CONDTMJA                          00377200
377300             IF  ACWA-SCREEN-HAS-ERRORS                           00377300
377400             THEN                                                 00377400
377500                 NEXT SENTENCE                                    00377500
377600             ELSE                                                 00377600
377700                 MOVE 'Y'        TO ACWA-ERROR-SW                 00377700
377800                 MOVE -1         TO CONDTMJL                      00377800
377900                 SET WT-01-INDEX TO +41.                          00377900
378000                                                                  00378000
378100     IF  CONDINFL < 1                                             00378100
378200     THEN                                                         00378200
378300         MOVE '0' TO CONDINFO                                     00378300
378400     ELSE                                                         00378400
378500         IF  CONDINFI = '0' OR '1'                                00378500
378600         THEN                                                     00378600
378700             NEXT SENTENCE                                        00378700
378800         ELSE                                                     00378800
378900             MOVE DFHBMUBF  TO  CONDINFA                          00378900
379000             IF  ACWA-SCREEN-HAS-ERRORS                           00379000
379100             THEN                                                 00379100
379200                 NEXT SENTENCE                                    00379200
379300             ELSE                                                 00379300
379400                 MOVE 'Y'        TO ACWA-ERROR-SW                 00379400
379500                 MOVE -1         TO CONDINFL                      00379500
379600                 SET WT-01-INDEX TO +41.                          00379600
379700                                                                  00379700
379800                                                                  00379800
379900     IF  CONDLIFL < 1                                             00379900
380000     THEN                                                         00380000
380100         MOVE '0' TO CONDLIFO                                     00380100
380200     ELSE                                                         00380200
380300         IF  CONDLIFI = '0' OR '1'                                00380300
380400         THEN                                                     00380400
380500             NEXT SENTENCE                                        00380500
380600         ELSE                                                     00380600
380700             MOVE DFHBMUBF  TO  CONDLIFA                          00380700
380800             IF  ACWA-SCREEN-HAS-ERRORS                           00380800
380900             THEN                                                 00380900
381000                 NEXT SENTENCE                                    00381000
381100             ELSE                                                 00381100
381200                 MOVE 'Y'        TO ACWA-ERROR-SW                 00381200
381300                 MOVE -1         TO CONDLIFL                      00381300
381400                 SET WT-01-INDEX TO +41.                          00381400
381500                                                                  00381500
381600     IF  CONDEMCL < 1                                             00381600
381700     THEN                                                         00381700
381800         MOVE '0' TO CONDEMCO                                     00381800
381900     ELSE                                                         00381900
382000         IF  CONDEMCI = '0' OR '1'                                00382000
382100         THEN                                                     00382100
382200             NEXT SENTENCE                                        00382200
382300         ELSE                                                     00382300
382400             MOVE DFHBMUBF  TO  CONDEMCA                          00382400
382500             IF  ACWA-SCREEN-HAS-ERRORS                           00382500
382600             THEN                                                 00382600
382700                 NEXT SENTENCE                                    00382700
382800             ELSE                                                 00382800
382900                 MOVE 'Y'        TO ACWA-ERROR-SW                 00382900
383000                 MOVE -1         TO CONDEMCL                      00383000
383100                 SET WT-01-INDEX TO +41.                          00383100
383200                                                                  00383200
383300     IF  CONDEACL < 1                                             00383300
383400     THEN                                                         00383400
383500         MOVE '0' TO CONDEACO                                     00383500
383600     ELSE                                                         00383600
383700         IF  CONDEACI = '0' OR '1'                                00383700
383800         THEN                                                     00383800
383900             NEXT SENTENCE                                        00383900
384000         ELSE                                                     00384000
384100             MOVE DFHBMUBF  TO  CONDEACA                          00384100
384200             IF  ACWA-SCREEN-HAS-ERRORS                           00384200
384300             THEN                                                 00384300
384400                 NEXT SENTENCE                                    00384400
384500             ELSE                                                 00384500
384600                 MOVE 'Y'        TO ACWA-ERROR-SW                 00384600
384700                 MOVE -1         TO CONDEACL                      00384700
384800                 SET WT-01-INDEX TO +41.                          00384800
384900                                                                  00384900
385000     IF  CONDSMIL < 1                                             00385000
385100     THEN                                                         00385100
385200         MOVE '0' TO CONDSMIO                                     00385200
385300     ELSE                                                         00385300
385400         IF  CONDSMII = '0' OR '1'                                00385400
385500         THEN                                                     00385500
385600             NEXT SENTENCE                                        00385600
385700         ELSE                                                     00385700
385800             MOVE DFHBMUBF  TO  CONDSMIA                          00385800
385900             IF  ACWA-SCREEN-HAS-ERRORS                           00385900
386000             THEN                                                 00386000
386100                 NEXT SENTENCE                                    00386100
386200             ELSE                                                 00386200
386300                 MOVE 'Y'        TO ACWA-ERROR-SW                 00386300
386400                 MOVE -1         TO CONDSMIL                      00386400
386500                 SET WT-01-INDEX TO +41.                          00386500
386600                                                                  00386600
386700     IF  CONDNSML < 1                                             00386700
386800     THEN                                                         00386800
386900         MOVE '0' TO CONDNSMO                                     00386900
387000     ELSE                                                         00387000
387100         IF  CONDNSMI = '0' OR '1'                                00387100
387200         THEN                                                     00387200
387300             NEXT SENTENCE                                        00387300
387400         ELSE                                                     00387400
387500             MOVE DFHBMUBF  TO  CONDNSMA                          00387500
387600             IF  ACWA-SCREEN-HAS-ERRORS                           00387600
387700             THEN                                                 00387700
387800                 NEXT SENTENCE                                    00387800
387900             ELSE                                                 00387900
388000                 MOVE 'Y'        TO ACWA-ERROR-SW                 00388000
388100                 MOVE -1         TO CONDNSML                      00388100
388200                 SET WT-01-INDEX TO +41.                          00388200
388300                                                                  00388300
388400                                                                  00388400
388500     IF ACWA-SCREEN-HAS-ERRORS                                    00388500
388600         GO TO 1100-500-EDIT-DESCRIPTOR.                          00388600
388700                                                                  00388700
388800                                                                  00388800
388900                                                                  00388900
389000     MOVE CONDTABI      TO    WS-TAB-BIT.                         00389000
389100     MOVE CONDMENI      TO    WS-MEN-BIT.                         00389100
389200     MOVE CONDDRGI      TO    WS-DRG-BIT.                         00389200
389300     MOVE CONDALCI      TO    WS-ALC-BIT.                         00389300
389400     MOVE CONDOBNI      TO    WS-OBN-BIT.                         00389400
389500     MOVE CONDOBCI      TO    WS-OBC-BIT.                         00389500
389600     MOVE CONDMALI      TO    WS-MAL-BIT.                         00389600
389700     MOVE CONDCARI      TO    WS-CAR-BIT.                         00389700
389800     MOVE CONDOBSI      TO    WS-OBS-BIT.                         00389800
389900     MOVE CONDKDYI      TO    WS-KDY-BIT.                         00389900
390000     MOVE CONDACCI      TO    WS-ACC-BIT.                         00390000
390100     MOVE CONDICDI      TO    WS-ICD-BIT.                         00390100
390200     MOVE CONDALLI      TO    WS-ALL-BIT.                         00390200
390300     MOVE CONDEXCI      TO    WS-EXC-BIT.                         00390300
390400     MOVE CONDPECI      TO    WS-PEC-BIT.                         00390400
390500     MOVE CONDNEMI      TO    WS-NEM-BIT.                         00390500
390600     MOVE CONDSUII      TO    WS-SUI-BIT.                         00390600
390700     MOVE CONDTMJI      TO    WS-TMJ-BIT.                         00390700
390800     MOVE CONDINFI      TO    WS-INF-BIT.                         00390800
390900     MOVE CONDLIFI      TO    WS-LIF-BIT.                         00390900
391000     MOVE CONDEMCI      TO    WS-EMC-BIT.                         00391000
391100     MOVE CONDEACI      TO    WS-EAC-BIT.                         00391100
391200     MOVE CONDSMII      TO    WS-SMI-BIT.                         00391200
391300     MOVE CONDNSMI      TO    WS-NSM-BIT.                         00391300
391400                                                                  00391400
391500     COMPUTE WS-ACTUAL-CONDITION-BITS  =  WS-TAB-BIT  +           00391500
391600                                       WS-MEN-BIT  +              00391600
391700                                       WS-DRG-BIT  +              00391700
391800                                       WS-ALC-BIT  +              00391800
391900                                       WS-OBN-BIT  +              00391900
392000                                       WS-OBC-BIT  +              00392000
392100                                       WS-MAL-BIT  +              00392100
392200                                       WS-CAR-BIT  +              00392200
392300                                       WS-OBS-BIT  +              00392300
392400                                       WS-KDY-BIT  +              00392400
392500                                       WS-ACC-BIT  +              00392500
392600                                       WS-PEC-BIT  +              00392600
392700                                       WS-NEM-BIT  +              00392700
392800                                       WS-SUI-BIT  +              00392800
392900                                       WS-TMJ-BIT  +              00392900
393000                                       WS-LIF-BIT  +              00393000
393100                                       WS-INF-BIT  +              00393100
393200                                       WS-EMC-BIT  +              00393200
393300                                       WS-EAC-BIT  +              00393300
393400                                       WS-SMI-BIT  +              00393400
393500                                       WS-NSM-BIT.                00393500
393600                                                                  00393600
393700     COMPUTE WS-ALL-BUT-EXC-N-ALL  =  WS-ACTUAL-CONDITION-BITS  + 00393700
393800                                      WS-ICD-BIT.                 00393800
393900                                                                  00393900
394000     COMPUTE WS-SUM-OF-ALL-BITS  =  WS-ACTUAL-CONDITION-BITS  +   00394000
394100                     WS-ICD-BIT  +  WS-ALL-BIT  +  WS-EXC-BIT.    00394100
394200                                                                  00394200
394300                                                                  00394300
394400     IF WS-SUM-OF-ALL-BITS  =  0                                  00394400
394500        SET WT-01-INDEX  TO  +42                                  00394500
394600        MOVE -1          TO  CONDALLL                             00394600
394700        MOVE 'Y'         TO  ACWA-ERROR-SW                        00394700
394800        GO TO 1100-500-EDIT-DESCRIPTOR.                           00394800
394900                                                                  00394900
395000     IF  (WS-EXC-BIT  =  1                                        00395000
395100          AND                                                     00395100
395200        ((WS-ICD-BIT  =  0 AND  WS-ALL-BIT  =  0 ) OR             00395200
395300         (WS-ICD-BIT  =  1 AND  WS-ALL-BIT  =  1 )))              00395300
395400     THEN                                                         00395400
395500         SET WT-01-INDEX  TO  +42                                 00395500
395600         MOVE -1          TO  CONDALLL                            00395600
395700         MOVE 'Y'         TO  ACWA-ERROR-SW                       00395700
395800         GO TO 1100-500-EDIT-DESCRIPTOR.                          00395800
395900                                                                  00395900
396000     IF (WS-ALL-BIT  =  1     AND                                 00396000
396100         WS-EXC-BIT  =  ZERO  AND                                 00396100
396200         WS-ALL-BUT-EXC-N-ALL  NOT =  0 )                         00396200
396300     THEN                                                         00396300
396400         SET WT-01-INDEX TO +42                                   00396400
396500         MOVE -1         TO CONDALLL                              00396500
396600         MOVE 'Y'        TO ACWA-ERROR-SW                         00396600
396700         GO TO 1100-500-EDIT-DESCRIPTOR.                          00396700
396800                                                                  00396800
396900                                                                  00396900
397000     IF (WS-ALL-BIT  =  1     AND  WS-EXC-BIT  =  1 )  AND        00397000
397100        (WS-ICD-BIT  NOT =  0 OR   WS-ACTUAL-CONDITION-BITS  =  0)00397100
397200     THEN                                                         00397200
397300         SET WT-01-INDEX TO +42                                   00397300
397400         MOVE -1         TO CONDALLL                              00397400
397500         MOVE 'Y'        TO ACWA-ERROR-SW                         00397500
397600         GO TO 1100-500-EDIT-DESCRIPTOR.                          00397600
397700                                                                  00397700
397800                                                                  00397800
397900     IF (WS-ICD-BIT  =  1 AND  WS-EXC-BIT  =  0 AND               00397900
398000        WS-ALL-BIT  =  0) AND                                     00398000
398100        (WS-ACTUAL-CONDITION-BITS  >  1 OR                        00398100
398200        (WS-ACTUAL-CONDITION-BITS  =  1 AND                       00398200
398300        WS-PEC-BIT  NOT =  1 AND                                  00398300
398400        WS-SUI-BIT  NOT =  1))                                    00398400
398500        SET WT-01-INDEX  TO  +42                                  00398500
398600        MOVE   -1        TO  CONDALLL                             00398600
398700        MOVE   'Y'       TO  ACWA-ERROR-SW                        00398700
398800        GO TO 1100-500-EDIT-DESCRIPTOR.                           00398800
398900                                                                  00398900
399000                                                                  00399000
399100     IF (WS-ICD-BIT  =  1 AND  WS-EXC-BIT  =  0 AND               00399100
399200        (WS-PEC-BIT  =  1  OR  WS-SUI-BIT  =  1) AND              00399200
399300        WS-SUM-OF-ALL-BITS  >  2)                                 00399300
399400        SET WT-01-INDEX  TO  +42                                  00399400
399500        MOVE   -1        TO  CONDALLL                             00399500
399600        MOVE   'Y'       TO  ACWA-ERROR-SW                        00399600
399700        GO TO 1100-500-EDIT-DESCRIPTOR.                           00399700
399800                                                                  00399800
399900     IF (WS-ICD-BIT  = 1      AND  WS-EXC-BIT  =  1 ) AND         00399900
400000       ((WS-ALL-BIT  NOT =  0 OR   WS-ACTUAL-CONDITION-BITS  = 0))00400000
400100     THEN                                                         00400100
400200         SET WT-01-INDEX TO +42                                   00400200
400300         MOVE -1         TO CONDALLL                              00400300
400400         MOVE 'Y'        TO ACWA-ERROR-SW                         00400400
400500         GO TO 1100-500-EDIT-DESCRIPTOR.                          00400500
400600                                                                  00400600
400700*GP***************************************************************00400700
400800*GP*** 11/27/01 - WAITING TO HEAR FROM SSD IF THEY WANT THIS EDIT*00400800
400900*GP***************************************************************00400900
401000*GP*                                                              00401000
401100*GP* IF      WS-MEN-BIT  =   1                                    00401100
401200*GP*         IF  WS-SMI-BIT = 1 OR                                00401200
401300*GP*             WS-NSM-BIT = 1                                   00401300
401400*GP*             SET WT-01-INDEX TO +42                           00401400
401500*GP*             MOVE -1         TO CONDMENL                      00401500
401600*GP*             MOVE 'Y'        TO ACWA-ERROR-SW                 00401600
401700*GP*         END-IF                                               00401700
401800*GP* ELSE                                                         00401800
401900*GP* IF      WS-SMI-BIT  =   1                                    00401900
402000*GP*         IF  WS-NSM-BIT = 1                                   00402000
402100*GP*             SET WT-01-INDEX TO +42                           00402100
402200*GP*             MOVE -1         TO CONDSMIL                      00402200
402300*GP*             MOVE 'Y'        TO ACWA-ERROR-SW.                00402300
402400*GP***************************************************************00402400
402500                                                                  00402500
402600*------------------ INTERNAL DESCRIPTOR -------------------------*00402600
402700                                                                  00402700
402800 1100-500-EDIT-DESCRIPTOR.                                        00402800
402900                                                                  00402900
403000     IF  INTDESKL > ZERO                                          00403000
403100     THEN                                                         00403100
403200         MOVE 'ACCM13'  TO GCVI-FIELDS-KEY-ID                     00403200
403300         MOVE INTDESKI  TO GCVI-VALUE-LEN-9                       00403300
403400         PERFORM 1200-000-LINK-TO-GCVIOPGM                        00403400
403500         IF  GCVI-VALUE-NOT-FOUND                                 00403500
403600         THEN                                                     00403600
403700             MOVE DFHBMUBF TO INTDESKA                            00403700
403800             IF  ACWA-SCREEN-HAS-ERRORS                           00403800
403900             THEN                                                 00403900
404000                 NEXT SENTENCE                                    00404000
404100             ELSE                                                 00404100
404200                 MOVE 'Y'        TO ACWA-ERROR-SW                 00404200
404300                 MOVE -1         TO INTDESKL                      00404300
404400                 SET WT-01-INDEX TO +38                           00404400
404500         ELSE                                                     00404500
404600             IF  GCVI-VALUE-NOT-LOADED                            00404600
404700             THEN                                                 00404700
404800                 MOVE DFHBMUBF TO INTDESKA                        00404800
404900                 IF  GCVI-TABLE-SW = 'Y'                          00404900
405000                 THEN                                             00405000
405100                     MOVE 'N'      TO GCVI-TABLE-SW               00405100
405200                     MOVE -1       TO INTDESKL                    00405200
405300                 ELSE                                             00405300
405400                     NEXT SENTENCE                                00405400
405500             ELSE                                                 00405500
405600                 NEXT SENTENCE                                    00405600
405700     ELSE                                                         00405700
405800         MOVE SPACES TO INTDESKO.                                 00405800
405900                                                                  00405900
406000                                                                  00406000
406100*------------------- LOGICAL EDIT ------------------------------* 00406100
406200*                                                               * 00406200
406300* LOGICAL EDIT COMPARING INTERNAL DESCRIPTOR AND IBGR, IPGN,    * 00406300
406400* IPGT, IDGD, IPGP AND IPGS                                     * 00406400
406500*                                                               * 00406500
406600*---------------------------------------------------------------* 00406600
406700                                                                  00406700
406800     IF  INTDESKI > SPACES                                        00406800
406900         IF  (IBGROPTI = 'C' OR 'A' OR 'MT') OR                   00406900
407000             (IPGNOPTI = 'C' OR 'A' OR 'MT') OR                   00407000
407100             (IPGPOPTI = 'C' OR 'A' OR 'MT') OR                   00407100
407200             (IDGDOPTI = 'C' OR 'A' OR 'MT') OR                   00407200
407300             (IPGSOPTI = 'C' OR 'A' OR 'MT') OR                   00407300
407400             (IPGTOPTI = 'C' OR 'A' OR 'MT')                      00407400
407500             GO TO 1100-500-EXIT.                                 00407500
407600                                                                  00407600
407700                                                                  00407700
407800     IF  INTDESKI > SPACES                                        00407800
407900         IF     ((IBGROPTI NOT > SPACE) AND (IBGRSLTI > ZEROS))   00407900
408000             OR ((IPGNOPTI NOT > SPACE) AND (IPGNSLTI > ZEROS))   00408000
408100             OR ((IPGSOPTI NOT > SPACE) AND (IPGSSLTI > ZEROS))   00408100
408200             OR ((IPGTOPTI NOT > SPACE) AND (IPGTSLTI > ZEROS))   00408200
408300             OR ((IPGPOPTI NOT > SPACE) AND (IPGPSLTI > ZEROS))   00408300
408400             OR ((IDGDOPTI NOT > SPACE) AND (IDGDSLTI > ZEROS))   00408400
408500             GO TO 1100-500-EXIT.                                 00408500
408600                                                                  00408600
408700                                                                  00408700
408800     IF  INTDESKI > SPACES                                        00408800
408900         IF    ((IBGROPTI NOT > SPACE) AND (IBGRSLTI NOT > ZEROS))00408900
409000           AND ((IPGNOPTI NOT > SPACE) AND (IPGNSLTI NOT > ZEROS))00409000
409100           AND ((IPGSOPTI NOT > SPACE) AND (IPGSSLTI NOT > ZEROS))00409100
409200           AND ((IPGTOPTI NOT > SPACE) AND (IPGTSLTI NOT > ZEROS))00409200
409300           AND ((IPGPOPTI NOT > SPACE) AND (IPGPSLTI NOT > ZEROS))00409300
409400           AND ((IDGDOPTI NOT > SPACE) AND (IDGDSLTI NOT > ZEROS))00409400
409500             MOVE DFHBMUBF TO INTDESKA                            00409500
409600             IF  ACWA-SCREEN-HAS-ERRORS                           00409600
409700                 NEXT SENTENCE                                    00409700
409800             ELSE                                                 00409800
409900                 MOVE 'Y'        TO ACWA-ERROR-SW                 00409900
410000                 MOVE -1         TO INTDESKL                      00410000
410100                 SET WT-01-INDEX TO +39                           00410100
410200                 GO TO 1100-800-MOVE-MESSAGE.                     00410200
410300                                                                  00410300
410400                                                                  00410400
410500     IF  INTDESKI NOT > SPACES                                    00410500
410600         IF  (IBGROPTI = 'C' OR 'A' OR 'MT') OR                   00410600
410700             (IPGNOPTI = 'C' OR 'A' OR 'MT') OR                   00410700
410800             (IDGDOPTI = 'C' OR 'A' OR 'MT') OR                   00410800
410900             (IPGPOPTI = 'C' OR 'A' OR 'MT') OR                   00410900
411000             (IPGSOPTI = 'C' OR 'A' OR 'MT') OR                   00411000
411100             (IPGTOPTI = 'C' OR 'A' OR 'MT')                      00411100
411200             MOVE DFHBMUBF TO INTDESKA                            00411200
411300             IF ACWA-SCREEN-HAS-ERRORS                            00411300
411400                 NEXT SENTENCE                                    00411400
411500             ELSE                                                 00411500
411600                 MOVE 'Y'        TO ACWA-ERROR-SW                 00411600
411700                 MOVE -1         TO INTDESKL                      00411700
411800                 SET WT-01-INDEX TO +50                           00411800
411900                 GO TO 1100-800-MOVE-MESSAGE.                     00411900
412000                                                                  00412000
412100                                                                  00412100
412200     IF  INTDESKI NOT > SPACES                                    00412200
412300         IF  ((IBGROPTI NOT > SPACE) AND (IBGRSLTI > ZEROS)) OR   00412300
412400             ((IPGNOPTI NOT > SPACE) AND (IPGNSLTI > ZEROS)) OR   00412400
412500             ((IPGPOPTI NOT > SPACE) AND (IPGPSLTI > ZEROS)) OR   00412500
412600             ((IDGDOPTI NOT > SPACE) AND (IDGDSLTI > ZEROS)) OR   00412600
412700             ((IPGSOPTI NOT > SPACE) AND (IPGSSLTI > ZEROS)) OR   00412700
412800             ((IPGTOPTI NOT > SPACE) AND (IPGTSLTI > ZEROS))      00412800
412900             MOVE DFHBMUBF TO INTDESKA                            00412900
413000             IF  ACWA-SCREEN-HAS-ERRORS                           00413000
413100                 NEXT SENTENCE                                    00413100
413200             ELSE                                                 00413200
413300                 MOVE 'Y'        TO ACWA-ERROR-SW                 00413300
413400                 MOVE -1         TO INTDESKL                      00413400
413500                 SET WT-01-INDEX TO +50                           00413500
413600                 GO TO 1100-800-MOVE-MESSAGE.                     00413600
413700                                                                  00413700
413800                                                                  00413800
413900     MOVE ZEROS TO WS-SLOT-CNT                                    00413900
414000                   WS-DEL-CNT.                                    00414000
414100                                                                  00414100
414200     IF IBGROPTI = 'D'      ADD 1 TO WS-DEL-CNT.                  00414200
414300     IF IPGNOPTI = 'D'      ADD 1 TO WS-DEL-CNT.                  00414300
414400     IF IPGSOPTI = 'D'      ADD 1 TO WS-DEL-CNT.                  00414400
414500     IF IPGTOPTI = 'D'      ADD 1 TO WS-DEL-CNT.                  00414500
414600     IF IDGDOPTI = 'D'      ADD 1 TO WS-DEL-CNT.                  00414600
414700     IF IPGPOPTI = 'D'      ADD 1 TO WS-DEL-CNT.                  00414700
414800                                                                  00414800
414900     IF IBGRSLTI > ZEROS    ADD 1 TO WS-SLOT-CNT.                 00414900
415000     IF IPGNSLTI > ZEROS    ADD 1 TO WS-SLOT-CNT.                 00415000
415100     IF IPGSSLTI > ZEROS    ADD 1 TO WS-SLOT-CNT.                 00415100
415200     IF IPGTSLTI > ZEROS    ADD 1 TO WS-SLOT-CNT.                 00415200
415300     IF IDGDSLTI > ZEROS    ADD 1 TO WS-SLOT-CNT.                 00415300
415400     IF IPGPSLTI > ZEROS    ADD 1 TO WS-SLOT-CNT.                 00415400
415500                                                                  00415500
415600                                                                  00415600
415700     IF  WS-SLOT-CNT > ZERO                                       00415700
415800         IF  WS-SLOT-CNT = WS-DEL-CNT                             00415800
415900             IF  INTDESKI > SPACES                                00415900
416000                 MOVE DFHBMUBF TO INTDESKA                        00416000
416100                 IF  ACWA-SCREEN-HAS-ERRORS                       00416100
416200                     CONTINUE                                     00416200
416300                 ELSE                                             00416300
416400                     MOVE 'Y'        TO ACWA-ERROR-SW             00416400
416500                     MOVE -1         TO INTDESKL                  00416500
416600                     SET WT-01-INDEX TO +39                       00416600
416700                     GO TO 1100-800-MOVE-MESSAGE.                 00416700
416800                                                                  00416800
416900                                                                  00416900
417000     IF  INTDESKI > SPACES                                        00417000
417100         IF  IBGRSLTI > ZEROS OR                                  00417100
417200             IPGNSLTI > ZEROS OR                                  00417200
417300             IPGPSLTI > ZEROS OR                                  00417300
417400             IDGDSLTI > ZEROS OR                                  00417400
417500             IPGSSLTI > ZEROS OR                                  00417500
417600             IPGTSLTI > ZEROS                                     00417600
417700             NEXT SENTENCE                                        00417700
417800         ELSE                                                     00417800
417900             MOVE DFHBMUBF TO INTDESKA                            00417900
418000             IF  ACWA-SCREEN-HAS-ERRORS                           00418000
418100                 NEXT SENTENCE                                    00418100
418200             ELSE                                                 00418200
418300                 MOVE 'Y'        TO ACWA-ERROR-SW                 00418300
418400                 MOVE -1         TO INTDESKL                      00418400
418500                 SET WT-01-INDEX TO +40                           00418500
418600     ELSE                                                         00418600
418700         NEXT SENTENCE.                                           00418700
418800                                                                  00418800
418900                                                                  00418900
419000     IF  INTDESKI NOT > SPACES                                    00419000
419100         IF (IBGRSLTI > ZEROS OR                                  00419100
419200             IPGNSLTI > ZEROS OR                                  00419200
419300             IPGPSLTI > ZEROS OR                                  00419300
419400             IDGDSLTI > ZEROS OR                                  00419400
419500             IPGSSLTI > ZEROS OR                                  00419500
419600             IPGTSLTI > ZEROS)    AND                             00419600
419700             WS-SLOT-CNT NOT = WS-DEL-CNT                         00419700
419800             MOVE DFHBMUBF TO INTDESKA                            00419800
419900             IF  ACWA-SCREEN-HAS-ERRORS                           00419900
420000             THEN                                                 00420000
420100                 NEXT SENTENCE                                    00420100
420200             ELSE                                                 00420200
420300                 MOVE 'Y'        TO ACWA-ERROR-SW                 00420300
420400                 MOVE -1         TO INTDESKL                      00420400
420500                 SET WT-01-INDEX TO +50                           00420500
420600         ELSE                                                     00420600
420700             NEXT SENTENCE                                        00420700
420800     ELSE                                                         00420800
420900         NEXT SENTENCE.                                           00420900
421000                                                                  00421000
421100 1100-500-EXIT.                                                   00421100
421200                                                                  00421200
421300     IF ACWA-SCREEN-HAS-ERRORS                                    00421300
421400         GO TO 1100-800-MOVE-MESSAGE.                             00421400
421500                                                                  00421500
421600                                                                  00421600
421700     IF  MFRMSLTL  > ZERO                                         00421700
421800     THEN                                                         00421800
421900         IF  MFRMSLTI IS NUMERIC OR                               00421900
422000             MFRMSLTI = SPACES                                    00422000
422100         THEN                                                     00422100
422200             NEXT SENTENCE                                        00422200
422300         ELSE                                                     00422300
422400             MOVE 'Y'        TO ACWA-ERROR-SW                     00422400
422500             SET WT-01-INDEX TO +35                               00422500
422600             MOVE -1         TO MFRMSLTL                          00422600
422700             MOVE DFHBMUBF   TO MFRMSLTA                          00422700
422800             GO TO 1100-800-MOVE-MESSAGE                          00422800
422900     ELSE                                                         00422900
423000         NEXT SENTENCE.                                           00423000
423100                                                                  00423100
423200                                                                  00423200
423300                                                                  00423300
423400     IF  (IBGROPTL      = ZERO OR  IBGROPTI  =  SPACE) AND        00423400
423500         (IPGNOPTL      = ZERO OR  IPGNOPTI  =  SPACE) AND        00423500
423600         (IPGPOPTL      = ZERO OR  IPGPOPTI  =  SPACE) AND        00423600
423700         (IDGDOPTL      = ZERO OR  IDGDOPTI  =  SPACE) AND        00423700
423800         (IPGSOPTL      = ZERO OR  IPGSOPTI  =  SPACE) AND        00423800
423900         (IPGTOPTL      = ZERO OR  IPGTOPTI  =  SPACE) AND        00423900
424000          MFRMSLTL  NOT = ZERO                         AND        00424000
424100         (MFRMSLTI  NOT = SPACES AND ZEROS)                       00424100
424200     THEN                                                         00424200
424300         MOVE 'Y'        TO ACWA-ERROR-SW                         00424300
424400         SET WT-01-INDEX TO +43                                   00424400
424500         MOVE -1         TO IBGROPTL                              00424500
424600         MOVE DFHBMUBF   TO MFRMSLTA                              00424600
424700         MOVE DFHBMASB   TO IBGRIDA  IPGNIDA  IPGTIDA             00424700
424800                            IDGDIDA  IPGPIDA  IPGSIDA             00424800
424900         GO TO 1100-800-MOVE-MESSAGE.                             00424900
425000                                                                  00425000
425100                                                                  00425100
425200     IF  (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE) AND           00425200
425300         (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE) AND           00425300
425400         (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE) AND           00425400
425500         (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE) AND           00425500
425600         (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE) AND           00425600
425700         (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE)               00425700
425800     THEN                                                         00425800
425900         GO TO 1100-800-MOVE-MESSAGE.                             00425900
426000                                                                  00426000
426100                                                                  00426100
426200     IF ACWA-SCREEN-HAS-ERRORS                                    00426200
426300        GO TO 1100-800-MOVE-MESSAGE.                              00426300
426400                                                                  00426400
426500                                                                  00426500
426600     IF   IBGROPTL NOT = ZERO                              AND    00426600
426700         (IBGROPTI     = 'C' OR  'MT' OR 'A' OR  'D')      AND    00426700
426800         (IPGNOPTL NOT = ZERO AND  IPGNOPTI NOT =  SPACE)         00426800
426900     THEN                                                         00426900
427000         MOVE 'Y'        TO ACWA-ERROR-SW                         00427000
427100         SET WT-01-INDEX TO +44                                   00427100
427200         MOVE -1         TO IBGROPTL                              00427200
427300         MOVE DFHBMUBF   TO IBGROPTA   IPGNOPTA                   00427300
427400         MOVE DFHBMASB   TO IBGRIDA    IPGNIDA                    00427400
427500         MOVE DFHBMABF   TO IBGRSLTA   IPGNSLTA.                  00427500
427600                                                                  00427600
427700                                                                  00427700
427800     IF   IBGROPTL NOT =  ZERO                               AND  00427800
427900         (IBGROPTI     =  'C' OR  'MT' OR 'A' OR  'D')       AND  00427900
428000         (IPGSOPTL NOT =  ZERO AND  IPGSOPTI  NOT =  SPACE)       00428000
428100     THEN                                                         00428100
428200         MOVE 'Y'        TO ACWA-ERROR-SW                         00428200
428300         SET WT-01-INDEX TO +44                                   00428300
428400         MOVE -1         TO IBGROPTL                              00428400
428500         MOVE DFHBMUBF   TO IBGROPTA   IPGSOPTA                   00428500
428600         MOVE DFHBMASB   TO IBGRIDA    IPGSIDA                    00428600
428700         MOVE DFHBMABF   TO IBGRSLTA   IPGSSLTA.                  00428700
428800                                                                  00428800
428900                                                                  00428900
429000     IF   IBGROPTL NOT =  ZERO                               AND  00429000
429100         (IBGROPTI     =  'C' OR  'MT' OR 'A' OR  'D')       AND  00429100
429200         (IPGTOPTL NOT =  ZERO AND  IPGTOPTI  NOT =  SPACE)       00429200
429300     THEN                                                         00429300
429400         MOVE 'Y'        TO ACWA-ERROR-SW                         00429400
429500         SET WT-01-INDEX TO +44                                   00429500
429600         MOVE -1         TO IBGROPTL                              00429600
429700         MOVE DFHBMUBF   TO IBGROPTA   IPGTOPTA                   00429700
429800         MOVE DFHBMASB   TO IBGRIDA    IPGTIDA                    00429800
429900         MOVE DFHBMABF   TO IBGRSLTA   IPGTSLTA.                  00429900
430000                                                                  00430000
430100                                                                  00430100
430200     IF   IBGROPTL NOT =  ZERO                               AND  00430200
430300         (IBGROPTI     =  'C' OR  'MT' OR 'A' OR  'D')       AND  00430300
430400         (IDGDOPTL NOT =  ZERO AND  IDGDOPTI  NOT =  SPACE)       00430400
430500     THEN                                                         00430500
430600         MOVE 'Y'        TO ACWA-ERROR-SW                         00430600
430700         SET WT-01-INDEX TO +44                                   00430700
430800         MOVE -1         TO IBGROPTL                              00430800
430900         MOVE DFHBMUBF   TO IBGROPTA   IDGDOPTA                   00430900
431000         MOVE DFHBMASB   TO IBGRIDA    IDGDIDA                    00431000
431100         MOVE DFHBMABF   TO IBGRSLTA   IDGDSLTA.                  00431100
431200                                                                  00431200
431300                                                                  00431300
431400     IF   IBGROPTL NOT =  ZERO                               AND  00431400
431500         (IBGROPTI     =  'C' OR  'MT' OR 'A' OR  'D')       AND  00431500
431600         (IPGPOPTL NOT =  ZERO AND  IPGPOPTI  NOT =  SPACE)       00431600
431700     THEN                                                         00431700
431800         MOVE 'Y'        TO ACWA-ERROR-SW                         00431800
431900         SET WT-01-INDEX TO +44                                   00431900
432000         MOVE -1         TO IBGROPTL                              00432000
432100         MOVE DFHBMUBF   TO IBGROPTA   IPGPOPTA                   00432100
432200         MOVE DFHBMASB   TO IBGRIDA    IPGPIDA                    00432200
432300         MOVE DFHBMABF   TO IBGRSLTA   IPGPSLTA.                  00432300
432400                                                                  00432400
432500                                                                  00432500
432600     IF   IPGNOPTL NOT =  ZERO                               AND  00432600
432700         (IPGNOPTI     =  'C' OR  'MT' OR 'A' OR  'D')       AND  00432700
432800         (IPGSOPTL NOT =  ZERO AND  IPGSOPTI  NOT =  SPACE)       00432800
432900     THEN                                                         00432900
433000         MOVE 'Y'        TO ACWA-ERROR-SW                         00433000
433100         SET WT-01-INDEX TO +44                                   00433100
433200         MOVE -1         TO IPGNOPTL                              00433200
433300         MOVE DFHBMUBF   TO IPGNOPTA   IPGSOPTA                   00433300
433400         MOVE DFHBMASB   TO IPGNIDA    IPGSIDA                    00433400
433500         MOVE DFHBMABF   TO IPGNSLTA   IPGSSLTA.                  00433500
433600                                                                  00433600
433700                                                                  00433700
433800     IF   IPGNOPTL NOT =  ZERO                               AND  00433800
433900         (IPGNOPTI     =  'C' OR  'MT' OR 'A' OR  'D')       AND  00433900
434000         (IPGTOPTL NOT =  ZERO AND  IPGTOPTI  NOT =  SPACE)       00434000
434100     THEN                                                         00434100
434200         MOVE 'Y'        TO ACWA-ERROR-SW                         00434200
434300         SET WT-01-INDEX TO +44                                   00434300
434400         MOVE -1         TO IPGNOPTL                              00434400
434500         MOVE DFHBMUBF   TO IPGNOPTA   IPGTOPTA                   00434500
434600         MOVE DFHBMASB   TO IPGNIDA    IPGTIDA                    00434600
434700         MOVE DFHBMABF   TO IPGNSLTA   IPGTSLTA.                  00434700
434800                                                                  00434800
434900                                                                  00434900
435000     IF   IDGDOPTL NOT =  ZERO                               AND  00435000
435100         (IDGDOPTI     =  'C' OR  'MT' OR 'A' OR  'D')       AND  00435100
435200         (IPGNOPTL NOT =  ZERO AND  IPGNOPTI  NOT =  SPACE)       00435200
435300     THEN                                                         00435300
435400         MOVE 'Y'        TO ACWA-ERROR-SW                         00435400
435500         SET WT-01-INDEX TO +44                                   00435500
435600         MOVE -1         TO IDGDOPTL                              00435600
435700         MOVE DFHBMUBF   TO IDGDOPTA   IPGNOPTA                   00435700
435800         MOVE DFHBMASB   TO IDGDIDA    IPGNIDA                    00435800
435900         MOVE DFHBMABF   TO IDGDSLTA   IPGNSLTA.                  00435900
436000                                                                  00436000
436100                                                                  00436100
436200     IF   IDGDOPTL NOT =  ZERO                               AND  00436200
436300         (IDGDOPTI     =  'C' OR  'MT' OR 'A' OR  'D')       AND  00436300
436400         (IPGSOPTL NOT =  ZERO AND  IPGSOPTI  NOT =  SPACE)       00436400
436500     THEN                                                         00436500
436600         MOVE 'Y'        TO ACWA-ERROR-SW                         00436600
436700         SET WT-01-INDEX TO +44                                   00436700
436800         MOVE -1         TO IDGDOPTL                              00436800
436900         MOVE DFHBMUBF   TO IDGDOPTA   IPGSOPTA                   00436900
437000         MOVE DFHBMASB   TO IDGDIDA    IPGSIDA                    00437000
437100         MOVE DFHBMABF   TO IDGDSLTA   IPGSSLTA.                  00437100
437200                                                                  00437200
437300                                                                  00437300
437400     IF   IDGDOPTL NOT =  ZERO                               AND  00437400
437500         (IDGDOPTI     =  'C' OR  'MT' OR 'A' OR  'D')       AND  00437500
437600         (IPGTOPTL NOT =  ZERO AND  IPGTOPTI  NOT =  SPACE)       00437600
437700     THEN                                                         00437700
437800         MOVE 'Y'        TO ACWA-ERROR-SW                         00437800
437900         SET WT-01-INDEX TO +44                                   00437900
438000         MOVE -1         TO IDGDOPTL                              00438000
438100         MOVE DFHBMUBF   TO IDGDOPTA   IPGTOPTA                   00438100
438200         MOVE DFHBMASB   TO IDGDIDA    IPGTIDA                    00438200
438300         MOVE DFHBMABF   TO IDGDSLTA   IPGTSLTA.                  00438300
438400                                                                  00438400
438500                                                                  00438500
438600     IF   IPGPOPTL NOT =  ZERO                               AND  00438600
438700         (IPGPOPTI     =  'C' OR  'MT' OR 'A' OR  'D')       AND  00438700
438800         (IPGSOPTL NOT =  ZERO AND  IPGSOPTI  NOT =  SPACE)       00438800
438900     THEN                                                         00438900
439000         MOVE 'Y'        TO ACWA-ERROR-SW                         00439000
439100         SET WT-01-INDEX TO +44                                   00439100
439200         MOVE -1         TO IPGPOPTL                              00439200
439300         MOVE DFHBMUBF   TO IPGPOPTA   IPGSOPTA                   00439300
439400         MOVE DFHBMASB   TO IPGPIDA    IPGSIDA                    00439400
439500         MOVE DFHBMABF   TO IPGPSLTA   IPGSSLTA.                  00439500
439600                                                                  00439600
439700                                                                  00439700
439800     IF   IPGPOPTL NOT =  ZERO                               AND  00439800
439900         (IPGPOPTI     =  'C' OR  'MT' OR 'A' OR  'D')       AND  00439900
440000         (IPGTOPTL NOT =  ZERO AND  IPGTOPTI  NOT =  SPACE)       00440000
440100     THEN                                                         00440100
440200         MOVE 'Y'        TO ACWA-ERROR-SW                         00440200
440300         SET WT-01-INDEX TO +44                                   00440300
440400         MOVE -1         TO IPGPOPTL                              00440400
440500         MOVE DFHBMUBF   TO IPGPOPTA   IPGTOPTA                   00440500
440600         MOVE DFHBMASB   TO IPGPIDA    IPGTIDA                    00440600
440700         MOVE DFHBMABF   TO IPGPSLTA   IPGTSLTA.                  00440700
440800                                                                  00440800
440900                                                                  00440900
441000     IF   IPGPOPTL NOT =  ZERO                               AND  00441000
441100         (IPGPOPTI     =  'C' OR  'MT' OR 'A' OR  'D')       AND  00441100
441200         (IPGNOPTL NOT =  ZERO AND  IPGNOPTI  NOT =  SPACE)       00441200
441300     THEN                                                         00441300
441400         MOVE 'Y'        TO ACWA-ERROR-SW                         00441400
441500         SET WT-01-INDEX TO +44                                   00441500
441600         MOVE -1         TO IPGPOPTL                              00441600
441700         MOVE DFHBMUBF   TO IPGPOPTA   IPGNOPTA                   00441700
441800         MOVE DFHBMASB   TO IPGPIDA    IPGNIDA                    00441800
441900         MOVE DFHBMABF   TO IPGPSLTA   IPGNSLTA.                  00441900
442000                                                                  00442000
442100                                                                  00442100
442200     IF   IPGPOPTL NOT =  ZERO                               AND  00442200
442300         (IPGPOPTI     =  'C' OR  'MT' OR 'A' OR  'D')       AND  00442300
442400         (IDGDOPTL NOT =  ZERO AND  IDGDOPTI  NOT =  SPACE)       00442400
442500     THEN                                                         00442500
442600         MOVE 'Y'        TO ACWA-ERROR-SW                         00442600
442700         SET WT-01-INDEX TO +44                                   00442700
442800         MOVE -1         TO IPGPOPTL                              00442800
442900         MOVE DFHBMUBF   TO IPGPOPTA   IDGDOPTA                   00442900
443000         MOVE DFHBMASB   TO IPGPIDA    IDGDIDA                    00443000
443100         MOVE DFHBMABF   TO IPGPSLTA   IDGDSLTA.                  00443100
443200                                                                  00443200
443300                                                                  00443300
443400     IF   IPGTOPTL NOT =  ZERO                               AND  00443400
443500         (IPGTOPTI     =  'C' OR  'MT' OR 'A' OR  'D')       AND  00443500
443600         (IPGSOPTL NOT =  ZERO AND  IPGSOPTI  NOT =  SPACE)       00443600
443700     THEN                                                         00443700
443800         MOVE 'Y'        TO ACWA-ERROR-SW                         00443800
443900         SET WT-01-INDEX TO +44                                   00443900
444000         MOVE -1         TO IPGTOPTL                              00444000
444100         MOVE DFHBMUBF   TO IPGTOPTA   IPGSOPTA                   00444100
444200         MOVE DFHBMASB   TO IPGTIDA    IPGSIDA                    00444200
444300         MOVE DFHBMABF   TO IPGTSLTA   IPGSSLTA.                  00444300
444400                                                                  00444400
444500                                                                  00444500
444600     IF ACWA-SCREEN-HAS-ERRORS                                    00444600
444700        GO TO 1100-800-MOVE-MESSAGE.                              00444700
444800                                                                  00444800
444900                                                                  00444900
445000     IF IBGROPTL  NOT =  ZERO AND  IBGROPTI  NOT =  SPACE AND     00445000
445100        ((DELADDI  =  'CHG/DEL' AND                               00445100
445200        (IBGROPTI  NOT =  'C' AND  'MT' AND 'A' AND  'D') OR      00445200
445300        (IBGROPTI  =  'C' OR  'D') AND                            00445300
445400        IBGRSLTI   =  '0000000') OR                               00445400
445500        (OENTCTRI  =  '0000000' AND                               00445500
445600        IBGROPTI  NOT =  'MT' AND 'A') OR                         00445600
445700        (IBGRSLTI  >  '8999999' AND                               00445700
445800        (IBGROPTI  =  'MT' OR 'A'))    OR                         00445800
445900        (IBGRSLTI  >  '0000000' AND                               00445900
446000        (IBGROPTI  =  'A' )))                                     00446000
446100     THEN                                                         00446100
446200         MOVE DFHBMUBF   TO IBGROPTA                              00446200
446300         MOVE DFHBMASB   TO IBGRIDA                               00446300
446400         MOVE DFHBMABF   TO IBGRSLTA                              00446400
446500         MOVE 'Y'        TO ACWA-ERROR-SW                         00446500
446600         SET WT-01-INDEX TO +45                                   00446600
446700         MOVE -1         TO IBGROPTL.                             00446700
446800                                                                  00446800
446900     IF IPGNOPTL  NOT =  ZERO AND  IPGNOPTI  NOT =  SPACE AND     00446900
447000        ((DELADDI  =  'CHG/DEL' AND                               00447000
447100        (IPGNOPTI  NOT =  'C' AND  'MT' AND 'A' AND  'D') OR      00447100
447200        (IPGNOPTI  =  'C' OR  'D') AND                            00447200
447300        IPGNSLTI   =  '0000000') OR                               00447300
447400        (OENTCTRI  =  '0000000' AND                               00447400
447500        IPGNOPTI  NOT =  'MT' AND 'A') OR                         00447500
447600        (IPGNSLTI  >  '8999999' AND                               00447600
447700        (IPGNOPTI  =  'MT' OR 'A'))    OR                         00447700
447800        (IPGNSLTI  >  '0000000' AND                               00447800
447900        (IPGNOPTI  =  'A')))                                      00447900
448000     THEN                                                         00448000
448100         MOVE DFHBMUBF   TO IPGNOPTA                              00448100
448200         MOVE DFHBMASB   TO IPGNIDA                               00448200
448300         MOVE DFHBMABF   TO IPGNSLTA                              00448300
448400         MOVE 'Y'        TO ACWA-ERROR-SW                         00448400
448500         SET WT-01-INDEX TO +45                                   00448500
448600         MOVE -1         TO IPGNOPTL.                             00448600
448700                                                                  00448700
448800     IF IPGSOPTL  NOT =  ZERO AND  IPGSOPTI  NOT =  SPACE AND     00448800
448900        ((DELADDI  =  'CHG/DEL' AND                               00448900
449000        (IPGSOPTI  NOT =  'C' AND  'MT' AND 'A' AND  'D') OR      00449000
449100        (IPGSOPTI  =  'C' OR  'D') AND                            00449100
449200        IPGSSLTI  =  '0000000') OR                                00449200
449300        (OENTCTRI  =  '0000000' AND                               00449300
449400        IPGSOPTI  NOT =  'MT' AND 'A') OR                         00449400
449500        (IPGSSLTI  >  '8999999' AND                               00449500
449600        (IPGSOPTI  =  'MT' OR 'A')))                              00449600
449700     THEN                                                         00449700
449800         MOVE DFHBMUBF   TO IPGSOPTA                              00449800
449900         MOVE DFHBMASB   TO IPGSIDA                               00449900
450000         MOVE DFHBMABF   TO IPGSSLTA                              00450000
450100         MOVE 'Y'        TO ACWA-ERROR-SW                         00450100
450200         SET WT-01-INDEX TO +45                                   00450200
450300         MOVE -1         TO IPGSOPTL.                             00450300
450400                                                                  00450400
450500                                                                  00450500
450600     IF IPGTOPTL  NOT =  ZERO AND  IPGTOPTI  NOT =  SPACE AND     00450600
450700        ((DELADDI  =  'CHG/DEL' AND                               00450700
450800        (IPGTOPTI  NOT =  'C' AND  'MT' AND 'A' AND  'D') OR      00450800
450900        (IPGTOPTI  =  'C' OR  'D') AND                            00450900
451000        IPGTSLTI  =  '0000000') OR                                00451000
451100        (OENTCTRI  =  '0000000' AND                               00451100
451200        IPGTOPTI  NOT =  'MT' AND 'A') OR                         00451200
451300        (IPGTSLTI  >  '8999999' AND                               00451300
451400        (IPGTOPTI  =  'MT' OR 'A')))                              00451400
451500     THEN                                                         00451500
451600         MOVE DFHBMUBF   TO IPGTOPTA                              00451600
451700         MOVE DFHBMASB   TO IPGTIDA                               00451700
451800         MOVE DFHBMABF   TO IPGTSLTA                              00451800
451900         MOVE 'Y'        TO ACWA-ERROR-SW                         00451900
452000         SET WT-01-INDEX TO +45                                   00452000
452100         MOVE -1         TO IPGTOPTL.                             00452100
452200                                                                  00452200
452300                                                                  00452300
452400     IF IDGDOPTL  NOT =  ZERO AND  IDGDOPTI  NOT =  SPACE AND     00452400
452500        ((DELADDI  =  'CHG/DEL' AND                               00452500
452600        (IDGDOPTI  NOT =  'C' AND  'MT' AND 'A' AND  'D') OR      00452600
452700        (IDGDOPTI  =  'C' OR  'D') AND                            00452700
452800        IDGDSLTI  =  '0000000') OR                                00452800
452900        (OENTCTRI  =  '0000000' AND                               00452900
453000        IDGDOPTI  NOT =  'MT' AND 'A') OR                         00453000
453100        (IDGDSLTI  >  '8999999' AND                               00453100
453200        (IDGDOPTI  =  'MT' OR 'A')))                              00453200
453300     THEN                                                         00453300
453400         MOVE DFHBMUBF   TO IDGDOPTA                              00453400
453500         MOVE DFHBMASB   TO IDGDIDA                               00453500
453600         MOVE DFHBMABF   TO IDGDSLTA                              00453600
453700         MOVE 'Y'        TO ACWA-ERROR-SW                         00453700
453800         SET WT-01-INDEX TO +45                                   00453800
453900         MOVE -1         TO IDGDOPTL.                             00453900
454000                                                                  00454000
454100     IF IPGPOPTL  NOT =  ZERO AND  IPGPOPTI  NOT =  SPACE AND     00454100
454200        ((DELADDI  =  'CHG/DEL' AND                               00454200
454300        (IPGPOPTI  NOT =  'C' AND  'MT' AND 'A' AND  'D') OR      00454300
454400        (IPGPOPTI  =  'C' OR  'D') AND                            00454400
454500        IPGPSLTI  =  '0000000') OR                                00454500
454600        (OENTCTRI  =  '0000000' AND                               00454600
454700        IPGPOPTI  NOT =  'MT' AND 'A') OR                         00454700
454800        (IPGPSLTI  >  '8999999' AND                               00454800
454900        (IPGPOPTI  =  'MT' OR 'A')))                              00454900
455000     THEN                                                         00455000
455100         MOVE DFHBMUBF   TO IPGPOPTA                              00455100
455200         MOVE DFHBMASB   TO IPGPIDA                               00455200
455300         MOVE DFHBMABF   TO IPGPSLTA                              00455300
455400         MOVE 'Y'        TO ACWA-ERROR-SW                         00455400
455500         SET WT-01-INDEX TO +45                                   00455500
455600         MOVE -1         TO IPGPOPTL.                             00455600
455700                                                                  00455700
455800     IF ACWA-SCREEN-HAS-ERRORS                                    00455800
455900        GO TO 1100-800-MOVE-MESSAGE.                              00455900
456000                                                                  00456000
456100                                                                  00456100
456200     IF  IBGROPTI  =  'D' OR                                      00456200
456300         IDGDOPTI  =  'D' OR                                      00456300
456400         IPGNOPTI  =  'D' OR                                      00456400
456500         IPGPOPTI  =  'D' OR                                      00456500
456600         IPGSOPTI  =  'D' OR                                      00456600
456700         IPGTOPTI  =  'D'                                         00456700
456800     THEN                                                         00456800
456900         ADD 1 TO ACWA-FIELD-CHG-CNT                              00456900
457000         GO TO 1100-800-MOVE-MESSAGE.                             00457000
457100                                                                  00457100
457200                                                                  00457200
457300     IF  IBGROPTI = 'MT' AND   MFRMSLTL = ZERO                    00457300
457400     THEN                                                         00457400
457500         MOVE DFHBMUBF   TO IBGROPTA                              00457500
457600         MOVE DFHBMASB   TO IBGRIDA                               00457600
457700         MOVE DFHBMABF   TO IBGRSLTA                              00457700
457800         MOVE 'Y'        TO ACWA-ERROR-SW                         00457800
457900         SET WT-01-INDEX TO +46                                   00457900
458000         MOVE -1         TO MFRMSLTL                              00458000
458100         GO TO 1100-800-MOVE-MESSAGE.                             00458100
458200                                                                  00458200
458300                                                                  00458300
458400     IF  IPGNOPTI = 'MT' AND   MFRMSLTL = ZERO                    00458400
458500     THEN                                                         00458500
458600         MOVE DFHBMUBF   TO IPGNOPTA                              00458600
458700         MOVE DFHBMASB   TO IPGNIDA                               00458700
458800         MOVE DFHBMABF   TO IPGNSLTA                              00458800
458900         MOVE 'Y'        TO ACWA-ERROR-SW                         00458900
459000         SET WT-01-INDEX TO +46                                   00459000
459100         MOVE -1         TO MFRMSLTL                              00459100
459200         GO TO 1100-800-MOVE-MESSAGE.                             00459200
459300                                                                  00459300
459400                                                                  00459400
459500     IF  IPGSOPTI = 'MT' AND   MFRMSLTL = ZERO                    00459500
459600     THEN                                                         00459600
459700         MOVE DFHBMUBF   TO IPGSOPTA                              00459700
459800         MOVE DFHBMASB   TO IPGSIDA                               00459800
459900         MOVE DFHBMABF   TO IPGSSLTA                              00459900
460000         MOVE 'Y'        TO ACWA-ERROR-SW                         00460000
460100         SET WT-01-INDEX TO +46                                   00460100
460200         MOVE -1         TO MFRMSLTL                              00460200
460300         GO TO 1100-800-MOVE-MESSAGE.                             00460300
460400                                                                  00460400
460500                                                                  00460500
460600     IF  IPGTOPTI = 'MT' AND   MFRMSLTL = ZERO                    00460600
460700     THEN                                                         00460700
460800         MOVE DFHBMUBF   TO IPGTOPTA                              00460800
460900         MOVE DFHBMASB   TO IPGTIDA                               00460900
461000         MOVE DFHBMABF   TO IPGTSLTA                              00461000
461100         MOVE 'Y'        TO ACWA-ERROR-SW                         00461100
461200         SET WT-01-INDEX TO +46                                   00461200
461300         MOVE -1         TO MFRMSLTL                              00461300
461400         GO TO 1100-800-MOVE-MESSAGE.                             00461400
461500                                                                  00461500
461600                                                                  00461600
461700     IF  IDGDOPTI = 'MT' AND   MFRMSLTL = ZERO                    00461700
461800     THEN                                                         00461800
461900         MOVE DFHBMUBF   TO IDGDOPTA                              00461900
462000         MOVE DFHBMASB   TO IDGDIDA                               00462000
462100         MOVE DFHBMABF   TO IDGDSLTA                              00462100
462200         MOVE 'Y'        TO ACWA-ERROR-SW                         00462200
462300         SET WT-01-INDEX TO +46                                   00462300
462400         MOVE -1         TO MFRMSLTL                              00462400
462500         GO TO 1100-800-MOVE-MESSAGE.                             00462500
462600                                                                  00462600
462700                                                                  00462700
462800     IF  IPGPOPTI = 'MT' AND   MFRMSLTL = ZERO                    00462800
462900     THEN                                                         00462900
463000         MOVE DFHBMUBF   TO IPGPOPTA                              00463000
463100         MOVE DFHBMASB   TO IPGPIDA                               00463100
463200         MOVE DFHBMABF   TO IPGPSLTA                              00463200
463300         MOVE 'Y'        TO ACWA-ERROR-SW                         00463300
463400         SET WT-01-INDEX TO +46                                   00463400
463500         MOVE -1         TO MFRMSLTL                              00463500
463600         GO TO 1100-800-MOVE-MESSAGE.                             00463600
463700                                                                  00463700
463800                                                                  00463800
463900                                                                  00463900
464000*******                                                           00464000
464100* STS *==> CHANGE/SKELETON OPTIONS INVALID FOR SINGLE TAB SUPPORT 00464100
464200*******                                                           00464200
464300                                                                  00464300
464400     IF  FRMNUIDI =  'GTM1'      AND                              00464400
464500        (IBGROPTI =  'C' OR 'A')                                  00464500
464600     THEN                                                         00464600
464700         MOVE 'Y'        TO ACWA-ERROR-SW                         00464700
464800         SET WT-01-INDEX TO +47                                   00464800
464900         MOVE -1         TO IBGROPTL                              00464900
465000         MOVE DFHBMUBF   TO IBGROPTA                              00465000
465100         MOVE DFHBMASB   TO IBGRIDA                               00465100
465200     ELSE                                                         00465200
465300         NEXT SENTENCE.                                           00465300
465400                                                                  00465400
465500                                                                  00465500
465600     IF  FRMNUIDI =  'GTM1'      AND                              00465600
465700        (IPGNOPTI =  'C' OR 'A')                                  00465700
465800     THEN                                                         00465800
465900         MOVE 'Y'        TO ACWA-ERROR-SW                         00465900
466000         SET WT-01-INDEX TO +47                                   00466000
466100         MOVE -1         TO IPGNOPTL                              00466100
466200         MOVE DFHBMUBF   TO IPGNOPTA                              00466200
466300         MOVE DFHBMASB   TO IPGNIDA                               00466300
466400     ELSE                                                         00466400
466500         NEXT SENTENCE.                                           00466500
466600                                                                  00466600
466700                                                                  00466700
466800     IF  FRMNUIDI =  'GTM1'      AND                              00466800
466900        (IPGSOPTI =  'C' OR 'A')                                  00466900
467000     THEN                                                         00467000
467100         MOVE 'Y'        TO ACWA-ERROR-SW                         00467100
467200         SET WT-01-INDEX TO +47                                   00467200
467300         MOVE -1         TO IPGSOPTL                              00467300
467400         MOVE DFHBMUBF   TO IPGSOPTA                              00467400
467500         MOVE DFHBMASB   TO IPGSIDA                               00467500
467600     ELSE                                                         00467600
467700         NEXT SENTENCE.                                           00467700
467800                                                                  00467800
467900                                                                  00467900
468000     IF  FRMNUIDI =  'GTM1'      AND                              00468000
468100        (IPGTOPTI =  'C' OR 'A')                                  00468100
468200     THEN                                                         00468200
468300         MOVE 'Y'        TO ACWA-ERROR-SW                         00468300
468400         SET WT-01-INDEX TO +47                                   00468400
468500         MOVE -1         TO IPGTOPTL                              00468500
468600         MOVE DFHBMUBF   TO IPGTOPTA                              00468600
468700         MOVE DFHBMASB   TO IPGTIDA                               00468700
468800     ELSE                                                         00468800
468900         NEXT SENTENCE.                                           00468900
469000                                                                  00469000
469100                                                                  00469100
469200     IF  FRMNUIDI =  'GTM1'      AND                              00469200
469300        (IDGDOPTI =  'C' OR 'A')                                  00469300
469400     THEN                                                         00469400
469500         MOVE 'Y'        TO ACWA-ERROR-SW                         00469500
469600         SET WT-01-INDEX TO +47                                   00469600
469700         MOVE -1         TO IDGDOPTL                              00469700
469800         MOVE DFHBMUBF   TO IDGDOPTA                              00469800
469900         MOVE DFHBMASB   TO IDGDIDA                               00469900
470000     ELSE                                                         00470000
470100         NEXT SENTENCE.                                           00470100
470200                                                                  00470200
470300                                                                  00470300
470400     IF  FRMNUIDI =  'GTM1'      AND                              00470400
470500        (IPGPOPTI =  'C' OR 'A')                                  00470500
470600     THEN                                                         00470600
470700         MOVE 'Y'        TO ACWA-ERROR-SW                         00470700
470800         SET WT-01-INDEX TO +47                                   00470800
470900         MOVE -1         TO IPGPOPTL                              00470900
471000         MOVE DFHBMUBF   TO IPGPOPTA                              00471000
471100         MOVE DFHBMASB   TO IPGPIDA                               00471100
471200     ELSE                                                         00471200
471300         NEXT SENTENCE.                                           00471300
471400                                                                  00471400
471500                                                                  00471500
471600     IF ACWA-SCREEN-HAS-ERRORS                                    00471600
471700        GO TO 1100-800-MOVE-MESSAGE.                              00471700
471800                                                                  00471800
471900                                                                  00471900
472000*******                                                           00472000
472100*     *                                                           00472100
472200* STS *==> ONLY PRODUCTION TABULARS CAN BE MAPPED IN DURING       00472200
472300*     *     SINGLE TABULAR SUPPORT                                00472300
472400*******                                                           00472400
472500                                                                  00472500
472600     IF  FRMNUIDI =  'GTM1'  AND                                  00472600
472700         IBGROPTI =  'MT'                                         00472700
472800     THEN                                                         00472800
472900         MOVE MFRMSLTI  TO  ACWA-STS-SLOT-NO                      00472900
473000         IF  ACWA-STS-SLOT-NO-RANGE                               00473000
473100         THEN                                                     00473100
473200             SET WT-01-INDEX TO +48                               00473200
473300             MOVE 'Y'        TO ACWA-ERROR-SW                     00473300
473400             MOVE -1         TO IBGROPTL                          00473400
473500             MOVE DFHBMUBF   TO IBGROPTA                          00473500
473600                                MFRMSLTA                          00473600
473700             MOVE DFHBMASB   TO IBGRIDA                           00473700
473800         ELSE                                                     00473800
473900             NEXT SENTENCE                                        00473900
474000     ELSE                                                         00474000
474100         NEXT SENTENCE.                                           00474100
474200                                                                  00474200
474300                                                                  00474300
474400     IF  FRMNUIDI =  'GTM1'  AND                                  00474400
474500         IPGNOPTI =  'MT'                                         00474500
474600     THEN                                                         00474600
474700         MOVE MFRMSLTI  TO  ACWA-STS-SLOT-NO                      00474700
474800         IF  ACWA-STS-SLOT-NO-RANGE                               00474800
474900         THEN                                                     00474900
475000             MOVE 'Y'        TO ACWA-ERROR-SW                     00475000
475100             SET WT-01-INDEX TO +48                               00475100
475200             MOVE -1         TO IPGNOPTL                          00475200
475300             MOVE DFHBMUBF   TO IPGNOPTA                          00475300
475400                                MFRMSLTA                          00475400
475500             MOVE DFHBMASB   TO IPGNIDA                           00475500
475600         ELSE                                                     00475600
475700             NEXT SENTENCE                                        00475700
475800     ELSE                                                         00475800
475900         NEXT SENTENCE.                                           00475900
476000                                                                  00476000
476100                                                                  00476100
476200     IF  FRMNUIDI =  'GTM1'  AND                                  00476200
476300         IPGSOPTI =  'MT'                                         00476300
476400     THEN                                                         00476400
476500         MOVE MFRMSLTI  TO  ACWA-STS-SLOT-NO                      00476500
476600         IF  ACWA-STS-SLOT-NO-RANGE                               00476600
476700         THEN                                                     00476700
476800             MOVE 'Y'        TO ACWA-ERROR-SW                     00476800
476900             SET WT-01-INDEX TO +48                               00476900
477000             MOVE -1         TO IPGSOPTL                          00477000
477100             MOVE DFHBMUBF   TO IPGSOPTA                          00477100
477200                                MFRMSLTA                          00477200
477300             MOVE DFHBMASB   TO IPGSIDA                           00477300
477400         ELSE                                                     00477400
477500             NEXT SENTENCE                                        00477500
477600     ELSE                                                         00477600
477700         NEXT SENTENCE.                                           00477700
477800                                                                  00477800
477900                                                                  00477900
478000     IF  FRMNUIDI =  'GTM1'  AND                                  00478000
478100         IPGTOPTI =  'MT'                                         00478100
478200     THEN                                                         00478200
478300         MOVE MFRMSLTI  TO  ACWA-STS-SLOT-NO                      00478300
478400         IF  ACWA-STS-SLOT-NO-RANGE                               00478400
478500         THEN                                                     00478500
478600             MOVE 'Y'        TO ACWA-ERROR-SW                     00478600
478700             SET WT-01-INDEX TO +48                               00478700
478800             MOVE -1         TO IPGTOPTL                          00478800
478900             MOVE DFHBMUBF   TO IPGTOPTA                          00478900
479000                                MFRMSLTA                          00479000
479100             MOVE DFHBMASB   TO IPGTIDA                           00479100
479200         ELSE                                                     00479200
479300             NEXT SENTENCE                                        00479300
479400     ELSE                                                         00479400
479500         NEXT SENTENCE.                                           00479500
479600                                                                  00479600
479700                                                                  00479700
479800     IF  FRMNUIDI =  'GTM1'  AND                                  00479800
479900         IDGDOPTI =  'MT'                                         00479900
480000     THEN                                                         00480000
480100         MOVE MFRMSLTI  TO  ACWA-STS-SLOT-NO                      00480100
480200         IF  ACWA-STS-SLOT-NO-RANGE                               00480200
480300         THEN                                                     00480300
480400             MOVE 'Y'        TO ACWA-ERROR-SW                     00480400
480500             SET WT-01-INDEX TO +48                               00480500
480600             MOVE -1         TO IDGDOPTL                          00480600
480700             MOVE DFHBMUBF   TO IDGDOPTA                          00480700
480800                                MFRMSLTA                          00480800
480900             MOVE DFHBMASB   TO IDGDIDA                           00480900
481000         ELSE                                                     00481000
481100             NEXT SENTENCE                                        00481100
481200     ELSE                                                         00481200
481300         NEXT SENTENCE.                                           00481300
481400                                                                  00481400
481500                                                                  00481500
481600     IF  FRMNUIDI =  'GTM1'  AND                                  00481600
481700         IPGPOPTI =  'MT'                                         00481700
481800     THEN                                                         00481800
481900         MOVE MFRMSLTI  TO  ACWA-STS-SLOT-NO                      00481900
482000         IF  ACWA-STS-SLOT-NO-RANGE                               00482000
482100         THEN                                                     00482100
482200             MOVE 'Y'        TO ACWA-ERROR-SW                     00482200
482300             SET WT-01-INDEX TO +48                               00482300
482400             MOVE -1         TO IPGPOPTL                          00482400
482500             MOVE DFHBMUBF   TO IPGPOPTA                          00482500
482600                                MFRMSLTA                          00482600
482700             MOVE DFHBMASB   TO IPGPIDA                           00482700
482800         ELSE                                                     00482800
482900             NEXT SENTENCE                                        00482900
483000     ELSE                                                         00483000
483100         NEXT SENTENCE.                                           00483100
483200                                                                  00483200
483300                                                                  00483300
483400     IF ACWA-SCREEN-HAS-ERRORS                                    00483400
483500        GO TO 1100-800-MOVE-MESSAGE.                              00483500
483600                                                                  00483600
483700                                                                  00483700
483800*******                                                           00483800
483900* STS *----------------------------------------------------------*00483900
484000*******                                                           00484000
484100                                                                  00484100
484200                                                                  00484200
484300 1100-800-MOVE-MESSAGE.                                           00484300
484400                                                                  00484400
484500     IF ACWA-SCREEN-HAS-ERRORS                                    00484500
484600        MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO.          00484600
484700                                                                  00484700
484800                                                                  00484800
484900                                                                  00484900
485000 1100-900-EXIT.                                                   00485000
485100     EXIT.                                                        00485100
485200/*****************************************************************00485200
485300*                                                                *00485300
485400* 1200  LINK TO GCVIOPGM                                         *00485400
485500*                                                                *00485500
485600******************************************************************00485600
485700 1200-000-LINK-TO-GCVIOPGM      SECTION.                          00485700
485800 1200-010.                                                        00485800
485900                                                                  00485900
486000     EXEC CICS LINK PROGRAM ('GCVIOPGM')                          00486000
486100                    COMMAREA(GCV-FIELD-EDIT-INTERFACE-PARMS)      00486100
486200                    LENGTH  (GCVI-COMMAREA-LEN)                   00486200
486300                    END-EXEC.                                     00486300
486400                                                                  00486400
486500 1200-900-EXIT.                                                   00486500
486600     EXIT.                                                        00486600
