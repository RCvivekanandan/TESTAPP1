000100 IDENTIFICATION DIVISION.                                         12/09/02
000200 PROGRAM-ID.    GC024015.                                         GC024015
000300 AUTHOR.        ED WITKUS.                                           LV002
000400 INSTALLATION.  HCSC.                                             GC024015
000500 DATE-WRITTEN.  JUNE 28, 1989.                                    GC024015
000600 DATE-COMPILED.                                                   GC024015
000700******************************************************************GC024015
000800*                                                                *GC024015
000900*   THIS PROGRAM PERFORMS THE LOGICAL EDITS FOR THE ACCUMULATOR  *GC024015
001000*   TABULARS.                                                    *GC024015
001100*                                                                *GC024015
001200*   IT IS DYNAMICALLY CALLED BY GC024010, GC024020, GC024025.    *GC024015
001300*                                                                *GC024015
001400******************************************************************GC024015
001500******************************************************************GC024015
001600*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC024015
001700*       *-*         U P D A T E   H I S T O R Y         *-*      *GC024015
001800*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC024015
001900*                                                                *GC024015
002000* CHG NUM    DATE    BY   *---------DESCRIPTION------------------*GC024015
002100*  _____   ________  ___  _______________________________________*GC024015
002200*  D222    06/28/89  ENW  ORIGINAL MODULE                         GC024015
002300*                                                                 GC024015
002400*                       ----ACCUM TABULAR RECORD MODIFICATION--- *GC024015
002500* 11154   10/02/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *GC024015
002600* D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *GC024015
002700* D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *GC024015
002800* D270                  4. ADD AGE-LIMIT-FROM AND AGE-LIMIT-TO.  *GC024015
002900*                       5. ADD RELATIONSHIP-IND (CDE ELEMENT).   *GC024015
003000*                       6. ADD AGE LIMIT LOGICAL EDIT ROUTINE.   *GC024015
003100*                       7. INCREASE MAX REC LENGTH FOR ACCUM REC *GC024015
003200*                          TO 8157, AND W/F RECORD TO 8192, AS   *GC024015
003300*                          (8157 + 64 = 8221) BUT USING 8192 AS  *GC024015
003400*                          RECORDS CREATED IN ONLINE WILL NOT    *GC024015
003500*                          EXCEED TWO POINTERS.                  *GC024015
003600*                                                                 GC024015
003700* 11154   01/16/91  NGE 1. ADD AGE-QUAL-IND-FROM AND AGE-QUAL-   *GC024015
003800*                          IND-TO LOGICAL ERROR.                 *GC024015
003900                                                                  GC024015
004000* 11154     2/22/91  FRY  -DECREASE MAX OCCURS FROM 46 TO 44.    *GC024015
004100*                         -DECREASE MAX RECORD LENGTHS FOR:      *GC024015
004200*                           ACCUM RECORD FROM 8157 TO 7805.      *GC024015
004300*                           WORK RECORD  FROM 8221 TO 7869.      *GC024015
004400*            WORKING STORAGE SECTION CHANGED:                    *GC024015
004500*             -WS-TABULAR-REC-AREA  PIC X(8157) CHANGED TO 7805  *GC024015
004600*             -FILLER               PIC X(8147) CHANGED TO 7795  *GC024015
004700*             -WS-AGE-LIMIT-OCCURS OCCURS 46 TIMES CHANGED       *GC024015
004800*                 TO  WS-AGE-LIMIT-OCCURS OCCURS 44 TIMES        *GC024015
004900*             -CHANGED  REPLACING  ==  1 TO 46  ==               *GC024015
005000*                 TO  REPLACING  ==  1 TO 44  ==                 *GC024015
005100*            LINKAGE SECTION CHANGED:                            *GC024015
005200*             -GC024015-CALL-AREA2  PIC X(8157) CHANGED TO 7805  *GC024015
005300*             -LS-TABULAR-REC-AREA                               *GC024015
005400*                 FILLER  PIC X(8147) CHANGED TO 7795            *GC024015
005500*             -CHANGED  REPLACING  ==  1 TO 46  ==               *GC024015
005600*                   TO  REPLACING  ==  1 TO 44  ==               *GC024015
005700*                                                                *GC024015
005800*  11154  03/21/91  NE COMMENT OUT THE NEW EDIT ROUTINE FOR AGE  *GC024015
005900*                   LIMITS AND AGE QUALIFIERS PER ED WITKUS.     *GC024015
006000*                                                                *GC024015
006100*  PXXX   12/03/93   KJD UNCOMMENT AGE LIMIT EDITS, TEST & FIX   *GC024015
006200*                                                                *GC024015
006300*  14726/                                                         GC024015
006400*  15057     09/11/97  AB   RECOMPILED TO SUPPORT THE YEAR        GC024015
006500*                           2000 AND THE EXPANSION OF THE         GC024015
006600*                           CONTRACT KEY TO SUPPORT THE TX        GC024015
006700*                           MERGER.                               GC024015
006800*                                                                *GC024015
006900*  D15182  11/24/98   GDM   1. ADD LOGIC TO SUPPORT NEW ACCUM    *GC024015
007000*                              TABULAR #ACP                      *GC024015
007100*                                                                *GC024015
007200*          10/29/01   AKK   CHANGING TEST ON A25 DISCREP TO BE   *GC024015
007300*                              > ZEROES                          *GC024015
007400*                                                                *GC024015
007500*  D-360   11/16/01   GSP   ADDED FOUR NEW ACCUM CONDITION BITS: *GC024015
007600*                             -  EMERGENCY MEDICAL BIT           *GC024015
007700*                                EMERGENCY ACCIDENT BIT          *GC024015
007800*                                SERIOUS MENTAL ILLNESS BIT      *GC024015
007900*                                NON-SERIOUS MENTAL ILLNESS BIT  *GC024015
008000*                                                                *GC024015
008100*            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION. ADD 3*GC024015
008200*                             TO LS-DFILE-AREA FOR OPID.         *GC024015
008300*                                                                *GC024015
008400*  P09400    11-08-06   GF    ADD ASC/DESC INDICATOR EDIT FOR    *GC024015
008500*                             ABM ACP ADL                        *GC024015
008510*                                                                *GC024015
008520*  DM9441    09-21-09   DNK   CHANGED 7805 TO 31370 AND 7795 TO  *GC024015
008530*                             31360 FOR TABULAR FILE EXPANSION.  *GC024015
      *                                                                *GC024015
      *            06/14/16   KIKI  RECOMPILE ONLY CHANGED PGM GC24020 *GC024015
008510*                                                                *GC024015
008400*  P21595    09/19/16   HSB   RECOMPILE FOR GCPS NEW FIELDS BEN  *GC024015
008500*                             TYPE CODE,TIER CODE,TIER LEVEL.    *GC024015
      *                                                                *GC024015
ED0624* BBDA-58217 06/04/24   ED    PEAQ COPYBOOK EXPANSION:           *        
ED0624*                                COPYBKS - GCTABM*, GCTACL*,     *        
ED0624*                                GCTACP*,  GCTADL*, GCTADL*      *        
008600******************************************************************GC024015
008700 ENVIRONMENT DIVISION.                                            GC024015
008800 CONFIGURATION SECTION.                                           GC024015
008900 SOURCE-COMPUTER. IBM-370.                                        GC024015
009000 OBJECT-COMPUTER. IBM-370.                                        GC024015
009100 INPUT-OUTPUT SECTION.                                            GC024015
009200 FILE-CONTROL.                                                    GC024015
009300 DATA DIVISION.                                                   GC024015
009400 FILE SECTION.                                                    GC024015
009500/                                                                 GC024015
009600 WORKING-STORAGE SECTION.                                         GC024015
009700 01  WORK-AREA-X.                                                 GC024015
009800     05  FILLER                    PIC X(32) VALUE                GC024015
009900     'GC024015 WORKING STORAGE'.                                  GC024015
010000                                                                  GC024015
010100     05  WS-SUBSCRIPTS.                                           GC024015
010200         10  X                   PIC S9(4) VALUE +0 COMP.         GC024015
010300         10  Y                   PIC S9(4) VALUE +0 COMP.         GC024015
010400         10  AGE-SAVEX           PIC S9(4) VALUE +0 COMP.         GC024015
010500                                                                  GC024015
010600     05  WS-SORT-EXCHANGE-IND    PIC  X(1) VALUE 'Y'.             GC024015
010700         88  WS-EXCHANGE-MADE              VALUE 'Y'.             GC024015
010800         88  WS-NO-EXCHANGE-MADE           VALUE 'N'.             GC024015
010900                                                                  GC024015
011000     05  WS-OCCURS-ENTRIES            PIC S9(5) COMP-3 VALUE +0.  GC024015
011100     05  WS-OCCURANCE-COUNT           PIC S9(5) COMP-3.           GC024015
011200* LENGTH OF WS-OCCURANCE-ENTRY MUST MATCH AGE-AL-ENTRY LENGTH     GC024015
011300     05  WS-OCCURANCE-ENTRY.                                      GC024015
011400         10  WS-OCCURANCE-KEY    PIC  X(13)  VALUE HIGH-VALUES.   GC024015
011500         10  WS-OCCURANCE-NONKEY PIC  X(05)  VALUE HIGH-VALUES.   GC024015
011600                                                                  GC024015
011700     05  WS-AGE-LIMIT-AREA.                                       GC024015
ED0624         07  WS-AGE-LIMIT-OCCURS  OCCURS 175  TIMES               GC024015
011900                          INDEXED BY WS-A-INDEX  WS-B-INDEX.      GC024015
012000            10 AGE-AL-ENTRY.                                      GC024015
012100               15  AGE-OCCUR-KEY.                                 GC024015
012200                   17  WS-INT-DESC             PIC X(09).         GC024015
012300                   17  WS-AGE-LMT-F            PIC S9(7) COMP-3.  GC024015
012400               15  WS-AGE-LMT-T            PIC S9(7) COMP-3.      GC024015
012500               15  WS-ASC-DES-IND              PIC X.             GC024015
012600                                                                  GC024015
012700 01  ABEND-CODE                PIC 9(4)  COMP.                    GC024015
012800                                                                  GC024015
012900 01  WS-SEQUENCE-AREA.                                            GC024015
ED0624     05  WS-SEQUENCE-ENTRIES  OCCURS 1 TO 175 TIMES               GC024015
013100         DEPENDING ON GAF-ENTRY-COUNT.                            GC024015
013200         10  WS-SEQUENCE-ENTRY.                                   GC024015
013300             15  WS-SEQ-1                      PIC X(02).         GC024015
013400             15  WS-SEQ-2                      PIC X(02).         GC024015
013500                                                                  GC024015
013600     05  WS-SEQ-SWITCH.                                           GC024015
013700         10  WS-SEQ-A1         PIC  X(1).                         GC024015
013800             88  A1                       VALUE 'Y'.              GC024015
013900         10  WS-SEQ-A2         PIC  X(1).                         GC024015
014000             88  A2                       VALUE 'Y'.              GC024015
014100         10  WS-SEQ-B1         PIC  X(1).                         GC024015
014200             88  B1                       VALUE 'Y'.              GC024015
014300         10  WS-SEQ-B2         PIC  X(1).                         GC024015
014400             88  B2                       VALUE 'Y'.              GC024015
014500         10  WS-SEQ-C1         PIC  X(1).                         GC024015
014600             88  C1                       VALUE 'Y'.              GC024015
014700         10  WS-SEQ-C2         PIC  X(1).                         GC024015
014800             88  C2                       VALUE 'Y'.              GC024015
014900         10  WS-SEQ-D1         PIC  X(1).                         GC024015
015000             88  D1                       VALUE 'Y'.              GC024015
015100         10  WS-SEQ-D2         PIC  X(1).                         GC024015
015200             88  D2                       VALUE 'Y'.              GC024015
015300         10  WS-SEQ-E1         PIC  X(1).                         GC024015
015400             88  E1                       VALUE 'Y'.              GC024015
015500         10  WS-SEQ-E2         PIC  X(1).                         GC024015
015600             88  E2                       VALUE 'Y'.              GC024015
015700                                                                  GC024015
015800     05  WS-CDE-FIELDS.                                           GC024015
ED0624       07  WS-CDE-AREA-OCCURS  OCCURS 175  TIMES.                 GC024015
016000         10  WS-COPAY-MANDATORY-IND                PIC X(01).     GC024015
016100         10  WS-COPAY-BENEFIT-PERIOD               PIC X(02).     GC024015
016200         10  WS-COPAY-FAM-OR-INDIV                 PIC X(01).     GC024015
016300         10  WS-COPAY-L-O-B                        PIC X(01).     GC024015
016400         10  WS-COPAY-INTERNAL-DESCRIPTOR          PIC X(09).     GC024015
016500         10  WS-COPAY-SERVICE-GROUP                PIC X(02).     GC024015
016600         10  WS-COPAY-PLACE-OF-TREATMENT           PIC X(02).     GC024015
016700         10  WS-COPAY-CONDITION.                                  GC024015
016800                 20  WS-COND-ALL-BIT               PIC X.         GC024015
016900                 20  WS-COND-EXCLUSION-BIT         PIC X.         GC024015
017000                 20  WS-COND-ICD-BIT               PIC X.         GC024015
017100                 20  WS-COND-TB-BIT                PIC X.         GC024015
017200                 20  WS-COND-MENTAL-BIT            PIC X.         GC024015
017300                 20  WS-COND-DRUG-BIT              PIC X.         GC024015
017400                 20  WS-COND-ALCOHOL-BIT           PIC X.         GC024015
017500                 20  WS-COND-OB-COMP-BIT           PIC X.         GC024015
017600                 20  WS-COND-OB-NORM-BIT           PIC X.         GC024015
017700                 20  WS-COND-MALIGNANCY-BIT        PIC X.         GC024015
017800                 20  WS-COND-CARDIAC-DISEASE-BIT   PIC X.         GC024015
017900                 20  WS-COND-OBESITY-BIT           PIC X.         GC024015
018000                 20  WS-COND-KIDNEY-DISEASE-BIT    PIC X.         GC024015
018100                 20  WS-COND-ACCIDENT-BIT          PIC X.         GC024015
018200                 20  WS-COND-PRE-EXIST-BIT         PIC X.         GC024015
018300                 20  WS-COND-NON-EMER-BIT          PIC X.         GC024015
018400                 20  WS-COND-SUICIDE-BIT           PIC X.         GC024015
018500                 20  WS-COND-TMJ-BIT               PIC X.         GC024015
018600                 20  WS-COND-INF-BIT               PIC X.         GC024015
018700                 20  WS-COND-LIFE-THREAT-BIT       PIC X.         GC024015
018800                 20  WS-COND-EMER-MED-BIT          PIC X.         GC024015
018900                 20  WS-COND-EMER-ACC-BIT          PIC X.         GC024015
019000                 20  WS-COND-SER-MEN-ILL-BIT       PIC X.         GC024015
019100                 20  WS-COND-NON-SER-MEN-ILL-BIT   PIC X.         GC024015
019200                 20  WS-COND-FILLER-BIT            PIC X(06).     GC024015
019300         10  WS-COPAY-CO-PAY-IND                   PIC X(01).     GC024015
019400         10  WS-COPAY-COST-CONTAIN-IND             PIC X(02).     GC024015
019500         10  WS-COPAY-AGE-LIMIT-FROM   COMP-3      PIC S9(3).     GC024015
019600         10  WS-COPAY-AGE-QUAL-IND-FROM            PIC X(01).     GC024015
019700         10  WS-COPAY-VALUE-QUALIFIER              PIC  X(01).    GC024015
019800         10  WS-COPAY-FEAK-IND                     PIC  X(01).    GC024015
019900                                                                  GC024015
ED0624     05  WS-DEFINITION-ENTRIES OCCURS 1 TO 175 TIMES              GC024015
020100         DEPENDING ON GAF-ENTRY-COUNT.                            GC024015
020200         10  WS-DEFINITION-ENTRY.                                 GC024015
020300             15  WS-DEF-1                      PIC X(02).         GC024015
020400             15  WS-DEF-2                      PIC X(02).         GC024015
020500                                                                  GC024015
020600     05  WS-DEF-SWITCH.                                           GC024015
020700         10  WS-DEF-0A         PIC  X(1).                         GC024015
020800             88  0A                       VALUE 'Y'.              GC024015
020900         10  WS-DEF-AA         PIC  X(1).                         GC024015
021000             88  AA                       VALUE 'Y'.              GC024015
021100         10  WS-DEF-0B         PIC  X(1).                         GC024015
021200             88  0B                       VALUE 'Y'.              GC024015
021300         10  WS-DEF-BB         PIC  X(1).                         GC024015
021400             88  BB                       VALUE 'Y'.              GC024015
021500         10  WS-DEF-0F         PIC  X(1).                         GC024015
021600             88  0F                       VALUE 'Y'.              GC024015
021700         10  WS-DEF-FF         PIC  X(1).                         GC024015
021800             88  FF                       VALUE 'Y'.              GC024015
021900                                                                  GC024015
022000 01  WS-COPAY-AREA.                                               GC024015
022100     05  WS-COPAY-TD-IND            PIC X(02)  VALUE SPACE.       GC024015
022200         88 SELECTED-TD-IND         VALUE 'A1' 'A2' 'B1' 'B2'     GC024015
022300                                          'C1' 'C2' 'D1' 'D2'     GC024015
022400                                          'E1' 'E2'.              GC024015
022500                                                                  GC024015
022600     05  WS-COPAY-DEF-IND           PIC X(02)  VALUE SPACE.       GC024015
022700         88 SELECTED-DEF-IND        VALUE '0A' 'AA' '0B' 'BB'     GC024015
022800                                          '0F' 'FF'.              GC024015
022900                                                                  GC024015
023000 01  WS-BENEFIT-PERIOD.                                           GC024015
023100     05  WS-BEN-PRD                 PIC X(02)  VALUE SPACE.       GC024015
023200         88 SELECTED-VALUE          VALUE 'CA' THRU 'CF'.         GC024015
023300                                                                  GC024015
023400 01  WS-MISC.                                                     GC024015
023500     05  WS-PCT-LVL            PIC S9(3) COMP-3 VALUE ZEROS.      GC024015
023600     05  WS-DISP-CNT           PIC  9(3)        VALUE ZEROS.      GC024015
023700                                                                  GC024015
023800 01  WS-SWITCHES.                                                 GC024015
023900     05  WS-FINISH-SW          PIC X            VALUE 'N'.        GC024015
024000     05  WS-MATCH1-SW          PIC X            VALUE 'N'.        GC024015
024100     05  WS-MATCH2-SW          PIC X            VALUE 'N'.        GC024015
024200     05  WS-CHAIN-SW           PIC X            VALUE 'N'.        GC024015
024300                                                                  GC024015
024400 01  WS-TABULAR-REC-AREA         PIC X(31370).                    GC024015
024500 01  WS-TABULAR-REC-AREA-SPLIT REDEFINES                          GC024015
024600     WS-TABULAR-REC-AREA.                                         GC024015
024700     04  WS-TABULAR-REC.                                          GC024015
024800         05  WS-TAB-KEY.                                          GC024015
024900             10  WS-TAB-ID    PIC X(6).                           GC024015
025000             10  WS-TAB-SLOT  PIC S9(7) COMP-3.                   GC024015
025100         05  FILLER           PIC X(31360).                       GC024015
025200     04  WS-ABM-REC-AREA REDEFINES WS-TABULAR-REC.                GC024015
025300           COPY GCTABM2                                           GC024015
ED0624           REPLACING  ==  1 TO 175  ==                            GC024015
ED0624                  BY  ==       175  ==                            GC024015
025600                      ==  DEPENDING ON GAA2-ENTRY-COUNT  ==       GC024015
025700                  BY  ==                                 ==.      GC024015
025800     04  WS-ACL-REC-AREA REDEFINES WS-TABULAR-REC.                GC024015
025900           COPY GCTACL2                                           GC024015
ED0624           REPLACING  ==  1 TO 175  ==                            GC024015
ED0624                  BY  ==       175  ==                            GC024015
026200                      ==  DEPENDING ON GAB2-ENTRY-COUNT  ==       GC024015
026300                  BY  ==                                 ==.      GC024015
026400     04  WS-ACP-REC-AREA REDEFINES WS-TABULAR-REC.                GC024015
026500           COPY GCTACP2                                           GC024015
ED0624           REPLACING  ==  1 TO 175  ==                            GC024015
ED0624                  BY  ==       175  ==                            GC024015
026800                      ==  DEPENDING ON GAF2-ENTRY-COUNT  ==       GC024015
026900                  BY  ==                                 ==.      GC024015
027000     04  WS-ADL-REC-AREA REDEFINES WS-TABULAR-REC.                GC024015
027100           COPY GCTADL2                                           GC024015
ED0624           REPLACING  ==  1 TO 175  ==                            GC024015
ED0624                  BY  ==       175  ==                            GC024015
027400                      ==  DEPENDING ON GAC2-ENTRY-COUNT  ==       GC024015
027500                  BY  ==                                 ==.      GC024015
027600     04  WS-AOL-REC-AREA REDEFINES WS-TABULAR-REC.                GC024015
027700           COPY GCTAOL2                                           GC024015
ED0624           REPLACING  ==  1 TO 175  ==                            GC024015
ED0624                  BY  ==       175  ==                            GC024015
028000                      ==  DEPENDING ON GAD2-ENTRY-COUNT  ==       GC024015
028100                  BY  ==                                 ==.      GC024015
028200/                                                                 GC024015
028300 LINKAGE SECTION.                                                 GC024015
028400 01  GC024015-CALL-AREA1         PIC X(658).                      GC024015
028500 01  GC024015-CALL-AREA1-SPLIT REDEFINES                          GC024015
028600     GC024015-CALL-AREA1.                                         GC024015
028700     04  FILLER PIC X.                                            GC024015
028800*- ADD 3 TO LS-DFILE AREA FOR OPID EXPANSION. GTF                 GC024015
028900     04  LS-DFILE-REC-AREA PIC X(660).                            GC024015
029000     04  LS-DFILE-AREA REDEFINES LS-DFILE-REC-AREA.               GC024015
029100         COPY GCDFILEC                                            GC024015
029200           REPLACING  ==  1 TO 200 ==                             GC024015
029300                  BY  ==       200 ==                             GC024015
029400                      ==  DEPENDING ON DF-ERROR-COUNT    ==       GC024015
029500                  BY  ==                                 ==.      GC024015
029600 01  GC024015-CALL-AREA2         PIC X(31370).                    GC024015
029700 01  GC024015-CALL-AREA2-SPLIT REDEFINES                          GC024015
029800     GC024015-CALL-AREA2.                                         GC024015
029900     04  LS-TABULAR-REC-AREA.                                     GC024015
030000         05  LS-TAB-KEY.                                          GC024015
030100             10  LS-TAB-ID    PIC X(6).                           GC024015
030200             10  LS-TAB-SLOT  PIC S9(7) COMP-3.                   GC024015
030300         05  FILLER           PIC X(31360).                       GC024015
030400     04  LS-ABM-REC-AREA REDEFINES LS-TABULAR-REC-AREA.           GC024015
030500         COPY GCTABMC                                             GC024015
ED0624           REPLACING  ==  1 TO 175  ==                            GC024015
ED0624                  BY  ==       175  ==                            GC024015
030800                      ==  DEPENDING ON GAA-ENTRY-COUNT   ==       GC024015
030900                  BY  ==                                 ==.      GC024015
031000                                                                  GC024015
031100     04  LS-ACL-REC-AREA REDEFINES LS-TABULAR-REC-AREA.           GC024015
031200         COPY GCTACLC                                             GC024015
ED0624           REPLACING  ==  1 TO 175  ==                            GC024015
ED0624                  BY  ==       175  ==                            GC024015
031500                      ==  DEPENDING ON GAB-ENTRY-COUNT   ==       GC024015
031600                  BY  ==                                 ==.      GC024015
031700                                                                  GC024015
031800     04  LS-ACP-REC-AREA REDEFINES LS-TABULAR-REC-AREA.           GC024015
031900         COPY GCTACPC                                             GC024015
ED0624           REPLACING  ==  1 TO 175  ==                            GC024015
ED0624                  BY  ==       175  ==                            GC024015
032200                      ==  DEPENDING ON GAF-ENTRY-COUNT   ==       GC024015
032300                  BY  ==                                 ==.      GC024015
032400                                                                  GC024015
032500     04  LS-ADL-REC-AREA REDEFINES LS-TABULAR-REC-AREA.           GC024015
032600         COPY GCTADLC                                             GC024015
ED0624           REPLACING  ==  1 TO 175  ==                            GC024015
ED0624                  BY  ==       175  ==                            GC024015
032900                      ==  DEPENDING ON GAC-ENTRY-COUNT   ==       GC024015
033000                  BY  ==                                 ==.      GC024015
033100                                                                  GC024015
033200     04  LS-AOL-REC-AREA REDEFINES LS-TABULAR-REC-AREA.           GC024015
033300           COPY GCTAOLC                                           GC024015
ED0624           REPLACING  ==  1 TO 175  ==                            GC024015
ED0624                  BY  ==       175  ==                            GC024015
033600                      ==  DEPENDING ON GAD-ENTRY-COUNT   ==       GC024015
033700                  BY  ==                                 ==.      GC024015
033800/                                                                 GC024015
033900 PROCEDURE DIVISION USING GC024015-CALL-AREA1                     GC024015
034000                          GC024015-CALL-AREA2.                    GC024015
034100                                                                  GC024015
034200 0000-MAINLINE.                                                   GC024015
034300     SET DF-ERROR-INDEX TO DF-ERROR-COUNT.                        GC024015
034400     SET DF-ERROR-INDEX UP BY 1.                                  GC024015
034500                                                                  GC024015
034600     MOVE HIGH-VALUES TO WS-AGE-LIMIT-AREA.                       GC024015
034700     IF LS-TAB-ID = '#ABM  '                                      GC024015
034800         PERFORM 1000-ABM-EDITS THRU 1000-EXIT                    GC024015
034900     ELSE                                                         GC024015
035000     IF LS-TAB-ID = '#ACL  '                                      GC024015
035100         PERFORM 2000-ACL-EDITS THRU 2000-EXIT                    GC024015
035200     ELSE                                                         GC024015
035300     IF LS-TAB-ID = '#ACP  '                                      GC024015
035400         PERFORM 2500-ACP-EDITS THRU 2500-EXIT                    GC024015
035500     ELSE                                                         GC024015
035600     IF LS-TAB-ID = '#ADL  '                                      GC024015
035700         PERFORM 3000-ADL-EDITS THRU 3000-EXIT                    GC024015
035800     ELSE                                                         GC024015
035900     IF LS-TAB-ID = '#AOL  '                                      GC024015
036000         PERFORM 4000-AOL-EDITS THRU 4000-EXIT                    GC024015
036100     ELSE                                                         GC024015
036200         DISPLAY 'INVALID LS-TAB-ID RECEIVED'                     GC024015
036300         DISPLAY 'LS-TAB-ID = ' LS-TAB-ID.                        GC024015
036400                                                                  GC024015
036500     GOBACK.                                                      GC024015
036600                                                                  GC024015
036700 0000-EXIT.                                                       GC024015
036800     EXIT.                                                        GC024015
036900/                                                                 GC024015
037000***************************************************************** GC024015
037100 1000-ABM-EDITS.                                                  GC024015
037200                                                                          
037300     PERFORM 1010-000-ABM-EDIT1 THRU 1010-000-EXIT.               GC024015
037400*--- START OF ISSR 11154                                          GC024015
037500*--- EDIT ALL OCCURRENCES FOR AGE LIMITS AND INTERNAL DESCRIPTOR  GC024015
037600                                                                  GC024015
037700     COMPUTE WS-OCCURS-ENTRIES = GAA-ENTRY-COUNT - 1.             GC024015
037800                                                                  GC024015
037900     IF WS-OCCURS-ENTRIES    <   +2                               GC024015
038000        GO TO   1000-EXIT.                                        GC024015
038100                                                                  GC024015
038200     SET  WS-A-INDEX  TO +1.                                      GC024015
038300     PERFORM   1150-LOAD-AGE-LIMITS-ABM THRU 1150-EXIT            GC024015
038400         VARYING  GAA-INDEX  FROM +1  BY +1                       GC024015
038500           UNTIL  GAA-INDEX  >  WS-OCCURS-ENTRIES.                GC024015
038600                                                                  GC024015
038700     MOVE  'Y'  TO  WS-SORT-EXCHANGE-IND.                         GC024015
038800     PERFORM 7000-OCCURANCE-SORT THRU 7000-EXIT                   GC024015
038900       UNTIL WS-NO-EXCHANGE-MADE.                                 GC024015
039000                                                                  GC024015
039100     SET  WS-A-INDEX  WS-B-INDEX  TO +1.                          GC024015
039200     PERFORM 1160-EDIT-AGE-LIMITS-ABM THRU 1160-EXIT              GC024015
039300       UNTIL WS-INT-DESC (WS-A-INDEX) = HIGH-VALUES               GC024015
039400          OR WS-A-INDEX  >  WS-OCCURS-ENTRIES.                    GC024015
039500                                                                  GC024015
039600*--- END OF 11154                                                 GC024015
039700*                                                                 GC024015
039800 1000-EXIT.                                                       GC024015
039900     EXIT.                                                        GC024015
040000***************************************************************** GC024015
040100*     P09400 - 11/08/06 ASC/DESC EDIT FOR #ABM                    GC024015
040200*                                                                         
040300*     FOR ALL OCCURS, UNLESS AN ERROR CONDITION IS MET,           GC024015
040400*     IF THE ASCEND/DESCEND IND. = '1' OR '2' OR '3'              GC024015
040500*         CHECK THE OTHER OCCURS FOR AN ASCEND/DESCEND IND WITH   GC024015
040600*         THE SAME VALUE (THERE MAY BE SEVERAL OCCURS)            GC024015
040700*         IF NONE ARE FOUND,                                      GC024015
040800*             ASSIGN AN ERROR CODE                                GC024015
040900*         ELSE                                                    GC024015
041000*             COMPARE THE FIELDS FROM THE ONE OCCURS WITH THE     GC024015
041100*             FIELDS FROM THE OTHER OCCURS (SEE CODE FOR THE      GC024015
041200*             SPECIFIC FIELD NAMES)                               GC024015
041300*             IF THE VALUES IN THE FIELDS CHECKED ARE ALL EQUAL   GC024015
041400*             TO EACH OTHER,                                      GC024015
041500*                 EXIT                                            GC024015
041600*             ELSE                                                GC024015
041700*                 ASSIGN AN ERROR CODE                            GC024015
041800*                 EXIT.                                           GC024015
041900***************************************************************** GC024015
042000 1010-000-ABM-EDIT1.                                              GC024015
042100                                                                  GC024015
042200     IF GAA-ENTRY-COUNT < 3                                       GC024015
042300         GO TO 1010-000-EXIT.                                     GC024015
042400     MOVE 'N' TO WS-FINISH-SW.                                    GC024015
042500     MOVE LOW-VALUES TO WS-TABULAR-REC-AREA.                      GC024015
042600     MOVE LS-TABULAR-REC-AREA TO WS-TABULAR-REC-AREA.             GC024015
042700     SET GAA-INDEX  TO +1.                                        GC024015
042800     PERFORM 1010-010-LOOP1 THRU 1010-010-EXIT                    GC024015
042900       VARYING GAA-INDEX FROM 1 BY 1                              GC024015
043000       UNTIL   GAA-INDEX = GAA-ENTRY-COUNT                        GC024015
043100          OR   WS-FINISH-SW = 'Y'.                                GC024015
043200                                                                  GC024015
043300 1010-000-EXIT.                                                   GC024015
043400     EXIT.                                                        GC024015
043500                                                                  GC024015
043600***************************************************************** GC024015
043700 1010-010-LOOP1.                                                  GC024015
043800     IF GAA-BAMA-ASCEND-DESCEND-IND (GAA-INDEX) =  '1' OR         GC024015
043900                                                   '2' OR '3'     GC024015
044000         SET GAA2-INDEX TO 1                                      GC024015
044100         MOVE 'N' TO WS-MATCH1-SW                                 GC024015
044200                     WS-MATCH2-SW                                 GC024015
044300         PERFORM 1010-020-LOOP2 THRU 1010-020-EXIT                GC024015
044400           VARYING GAA2-INDEX FROM 1 BY 1                         GC024015
044500           UNTIL   GAA2-INDEX = GAA2-ENTRY-COUNT                  GC024015
044600     ELSE                                                         GC024015
044700         GO TO 1010-010-EXIT.                                     GC024015
044800     IF WS-MATCH1-SW = 'N'                                        GC024015
044900         ADD 1 TO DF-ERROR-COUNT                                  GC024015
045000         MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                    GC024015
045100         MOVE 'A27' TO DF-ERR-CODE (DF-ERROR-INDEX)               GC024015
045200         SET DF-ERROR-INDEX UP BY 1                               GC024015
045300         MOVE 'Y' TO WS-FINISH-SW                                 GC024015
045400         GO TO  1010-010-EXIT.                                    GC024015
045500     IF WS-MATCH2-SW = 'N'                                        GC024015
045600         ADD 1 TO DF-ERROR-COUNT                                  GC024015
045700         MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                    GC024015
045800         MOVE 'A28' TO DF-ERR-CODE (DF-ERROR-INDEX)               GC024015
045900         SET DF-ERROR-INDEX UP BY 1                               GC024015
046000         MOVE 'Y' TO WS-FINISH-SW                                 GC024015
046100         GO TO  1010-010-EXIT.                                    GC024015
046200                                                                  GC024015
046300 1010-010-EXIT.                                                   GC024015
046400     EXIT.                                                        GC024015
046500                                                                  GC024015
046600***************************************************************** GC024015
046700 1010-020-LOOP2.                                                  GC024015
046800                                                                  GC024015
046900     IF GAA2-INDEX = GAA-INDEX                                    GC024015
047000         GO TO 1010-020-EXIT.                                     GC024015
047100     IF GAA-BAMA-ASCEND-DESCEND-IND (GAA-INDEX)                   GC024015
047200       =                                                          GC024015
047300        GAA2-BAMA-ASCEND-DESCEND-IND (GAA2-INDEX)                 GC024015
047400         MOVE 'Y' TO WS-MATCH1-SW                                 GC024015
047500     ELSE                                                         GC024015
047600         GO TO 1010-020-EXIT.                                     GC024015
047700*******                                                           GC024015
047800* THE CONDITION BIT FILLER BIT IS PRIMED JUST IN CASE A MUTANT    GC024015
047900* VALUE EVER ENDS UP THERE.                                       GC024015
048000*                                                                 GC024015
048100     MOVE GAA-COND-FILLER-BIT (GAA-INDEX)                         GC024015
048200       TO GAA2-COND-FILLER-BIT (GAA2-INDEX).                      GC024015
048300*                                                                 GC024015
048400     IF ((GAA-BAMA-BENEFIT-PERIOD (GAA-INDEX)                     GC024015
048500       = GAA2-BAMA-BENEFIT-PERIOD (GAA2-INDEX))                   GC024015
048600       AND                                                        GC024015
048700        (GAA-BAMA-L-O-B (GAA-INDEX)                               GC024015
048800       = GAA2-BAMA-L-O-B (GAA2-INDEX))                            GC024015
048900       AND                                                        GC024015
049000        (GAA-BAMA-FAM-OR-INDIV (GAA-INDEX)                        GC024015
049100       = GAA2-BAMA-FAM-OR-INDIV (GAA2-INDEX))                     GC024015
049200       AND                                                        GC024015
049300        (GAA-BAMA-COST-CONTAIN-IND (GAA-INDEX)                    GC024015
049400       = GAA2-BAMA-COST-CONTAIN-IND (GAA2-INDEX))                 GC024015
049500       AND                                                        GC024015
049600        (GAA-BAMA-PLACE-OF-TREATMENT (GAA-INDEX)                  GC024015
049700       = GAA2-BAMA-PLACE-OF-TREATMENT (GAA2-INDEX))               GC024015
049800       AND                                                        GC024015
049900        (GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX)                     GC024015
050000       = GAA2-BAMA-VALUE-QUALIFIER (GAA2-INDEX))                  GC024015
050100       AND                                                        GC024015
050200        (GAA-BAMA-SERVICE-GROUP (GAA-INDEX)                       GC024015
050300       = GAA2-BAMA-SERVICE-GROUP (GAA2-INDEX))                    GC024015
050400       AND                                                        GC024015
050500        (GAA-BAMA-CO-PAY-IND (GAA-INDEX)                          GC024015
050600       = GAA2-BAMA-CO-PAY-IND (GAA2-INDEX))                       GC024015
050700       AND                                                        GC024015
050800        (GAA-BAMA-CONDITION (GAA-INDEX)                           GC024015
050900       = GAA2-BAMA-CONDITION (GAA2-INDEX))                        GC024015
051000       AND                                                        GC024015
051100        (GAA-BAMA-INTERNAL-DESCRIPTOR (GAA-INDEX)                 GC024015
051200       = GAA2-BAMA-INTERNAL-DESCRIPTOR (GAA2-INDEX))              GC024015
051300       AND                                                        GC024015
051400        (GAA-BAMA-AGE-QUAL-IND-FROM (GAA-INDEX)                   GC024015
051500       = GAA2-BAMA-AGE-QUAL-IND-FROM (GAA2-INDEX))                GC024015
051600       AND                                                        GC024015
051700         (GAA-BAMA-AGE-QUAL-IND-TO (GAA-INDEX)                    GC024015
051800       =  GAA2-BAMA-AGE-QUAL-IND-TO (GAA2-INDEX))                 GC024015
051900       AND                                                        GC024015
052000         (GAA-BAMA-AGE-LIMIT-FROM (GAA-INDEX)                     GC024015
052100       =  GAA2-BAMA-AGE-LIMIT-FROM (GAA2-INDEX))                  GC024015
052200       AND                                                        GC024015
052300         (GAA-BAMA-AGE-QUAL-IND-TO (GAA-INDEX)                    GC024015
052400       =  GAA2-BAMA-AGE-QUAL-IND-TO (GAA2-INDEX)))                GC024015
052500         MOVE 'Y' TO WS-MATCH2-SW.                                GC024015
052600                                                                  GC024015
052700                                                                  GC024015
052800 1010-020-EXIT.                                                   GC024015
052900     EXIT.                                                        GC024015
053000                                                                  GC024015
053100/*****************************************************************GC024015
053200*    CHECK AGE LIMITS AMD INTERNAL DESCRIPTORS                    GC024015
053300******************************************************************GC024015
053400 1150-LOAD-AGE-LIMITS-ABM.                                        GC024015
053500                                                                  GC024015
053600*--- INTERNAL DESCRIPTOR IS SPACES AND AGE-LIMITS IS ZEROS, SKIP. GC024015
053700     IF GAA-BAMA-INTERNAL-DESCRIPTOR (GAA-INDEX) = SPACES  AND    GC024015
053800        GAA-BAMA-AGE-LIMIT-FROM (GAA-INDEX)      = ZEROS   AND    GC024015
053900        GAA-BAMA-AGE-LIMIT-TO   (GAA-INDEX)      = ZEROS          GC024015
054000        GO TO   1150-EXIT.                                        GC024015
054100                                                                  GC024015
054200*--- IF AGE LIMIT FIELDS ARE  N O T  CODED THEN INTERNAL          GC024015
054300*--- DESCRIPTOR HAS NO EFFECT ON THE LOGIC, U N L E S S  THE SAME GC024015
054400*--- DESCRIPTOR IS ON DIFFERENT OCCURS AND THE AGE-LIMITS ARE     GC024015
054500*--- CODED, THIS WILL BE DETERMINED IN THE NEXT ROUTINE, BLANK    GC024015
054600*--- DESCRIPTOR WILL BE LOADED IN THE EDIT TABLE.                 GC024015
054700                                                                  GC024015
054800     MOVE GAA-BAMA-AGE-LIMIT-FROM (GAA-INDEX)                     GC024015
054900        TO WS-AGE-LMT-F (WS-A-INDEX).                             GC024015
055000     IF GAA-BAMA-AGE-QUAL-IND-FROM (GAA-INDEX) = 'W'              GC024015
055100        COMPUTE WS-AGE-LMT-F (WS-A-INDEX) =                       GC024015
055200                WS-AGE-LMT-F (WS-A-INDEX) * 7                     GC024015
055300     ELSE                                                         GC024015
055400     IF GAA-BAMA-AGE-QUAL-IND-FROM (GAA-INDEX) = 'M'              GC024015
055500        COMPUTE WS-AGE-LMT-F (WS-A-INDEX) =                       GC024015
055600                WS-AGE-LMT-F (WS-A-INDEX) * 30                    GC024015
055700     ELSE                                                         GC024015
055800     IF GAA-BAMA-AGE-QUAL-IND-FROM (GAA-INDEX) = 'Y'              GC024015
055900        COMPUTE WS-AGE-LMT-F (WS-A-INDEX) =                       GC024015
056000                WS-AGE-LMT-F (WS-A-INDEX) * 365.                  GC024015
056100                                                                  GC024015
056200     MOVE GAA-BAMA-AGE-LIMIT-TO (GAA-INDEX)                       GC024015
056300       TO WS-AGE-LMT-T (WS-A-INDEX).                              GC024015
056400     IF GAA-BAMA-AGE-QUAL-IND-TO (GAA-INDEX)   = 'W'              GC024015
056500          COMPUTE WS-AGE-LMT-T (WS-A-INDEX) =                     GC024015
056600                  WS-AGE-LMT-T (WS-A-INDEX) * 7                   GC024015
056700     ELSE                                                         GC024015
056800     IF GAA-BAMA-AGE-QUAL-IND-TO (GAA-INDEX)   = 'M'              GC024015
056900          COMPUTE WS-AGE-LMT-T (WS-A-INDEX) =                     GC024015
057000                  WS-AGE-LMT-T (WS-A-INDEX)  * 30                 GC024015
057100     ELSE                                                         GC024015
057200     IF GAA-BAMA-AGE-QUAL-IND-TO (GAA-INDEX)   = 'Y'              GC024015
057300          COMPUTE WS-AGE-LMT-T (WS-A-INDEX) =                     GC024015
057400                  WS-AGE-LMT-T (WS-A-INDEX)  * 365.               GC024015
057500                                                                  GC024015
057600     MOVE GAA-BAMA-INTERNAL-DESCRIPTOR (GAA-INDEX)  TO            GC024015
057700          WS-INT-DESC (WS-A-INDEX).                               GC024015
057800     MOVE GAA-BAMA-ASCEND-DESCEND-IND (GAA-INDEX)  TO             GC024015
057900          WS-ASC-DES-IND (WS-A-INDEX).                            GC024015
058000     SET WS-A-INDEX  UP BY 1.                                     GC024015
058100*                                                                 GC024015
058200 1150-EXIT.                                                       GC024015
058300     EXIT.                                                        GC024015
058400******************************************************************GC024015
058500*    EDIT AGE-LIMITS AMD INTERNAL DESCRIPTORS FOR OCCURENCES      GC024015
058600*--- ARE AGE LIMITS ZEROS? YES, SEARCH FOR MATCHED DESCRIPTOR.    GC024015
058700*--- NO MATCH FOUND, THEN AGE LIMITS NOT TO BE CODED,             GC024015
058800*--- YES MATCH FOUND, AND AGE LIMITS ARE CODED, THEN AGE LIMITS   GC024015
058900*--- FROM TABLE MUST BE CODED AS OTHER OCCUR WITH SAME DESCRIPTOR.GC024015
059000*--- VALUE OF SPACES IN THE DESCRIPTOR WILL MATCH WITH SAME.      GC024015
059100******************************************************************GC024015
059200 1160-EDIT-AGE-LIMITS-ABM.                                        GC024015
059300                                                                  GC024015
059400     IF WS-AGE-LMT-T (WS-A-INDEX)  =  ZEROS                       GC024015
059500        NEXT SENTENCE                                             GC024015
059600     ELSE                                                         GC024015
059700        IF WS-INT-DESC (WS-A-INDEX)  =                            GC024015
059800                        WS-INT-DESC (WS-A-INDEX + 1)              GC024015
059900           PERFORM  1170-EDIT-BOTH-OCCURS THRU 1170-EXIT.         GC024015
060000                                                                  GC024015
060100     SET WS-A-INDEX  WS-B-INDEX  UP BY +1.                        GC024015
060200                                                                  GC024015
060300 1160-EXIT.                                                       GC024015
060400     EXIT.                                                        GC024015
060500                                                                  GC024015
060600******************************************************************GC024015
060700* IF INTERNAL DESCRIPTION MATCHS THEN AGE LIMIT SHOULD BE CODED,  GC024015
060800* RANGE OF AGE-LIMIT YEARS MUST BE IN ASCENDING SEQUENCE AND NOT  GC024015
060900* OVERLAPPING OR GAPPED           OK      | OVERLAP   | GAPS      GC024015
061000*                      1ST  |  005 - 010  | 005 - 010 | 005 - 010 GC024015
061100*           EXMP:      2ND  |  011 - 020  | 010 - 020 | 015 - 020 GC024015
061200*                      3RD  |  021 - 040  | 019 - 040 | 022 - 040 GC024015
061300******************************************************************GC024015
061400 1170-EDIT-BOTH-OCCURS.                                           GC024015
061500                                                                  GC024015
061600     SET  WS-B-INDEX  UP BY +1.                                   GC024015
061700                                                                  GC024015
061800     IF WS-ASC-DES-IND (WS-A-INDEX) < HIGH-VALUES  AND            GC024015
061900        WS-ASC-DES-IND (WS-A-INDEX) > SPACE        AND            GC024015
062000        WS-ASC-DES-IND (WS-A-INDEX) NOT = '0'      AND            GC024015
062100        WS-ASC-DES-IND (WS-A-INDEX) = WS-ASC-DES-IND (WS-B-INDEX) GC024015
062200        MOVE 'Y' TO WS-CHAIN-SW                                   GC024015
062300     ELSE                                                         GC024015
062400        MOVE 'N' TO WS-CHAIN-SW.                                  GC024015
062500                                                                  GC024015
062600* OVERLAPPED; OR IF CHAINED, MUST MATCH                           GC024015
062700     IF WS-CHAIN-SW = 'N'                                         GC024015
062800       IF WS-AGE-LMT-T (WS-A-INDEX) NOT  <                        GC024015
062900          WS-AGE-LMT-F (WS-B-INDEX)                               GC024015
063000          ADD 1 TO DF-ERROR-COUNT                                 GC024015
063100          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
063200          MOVE 'A29' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
063300          SET DF-ERROR-INDEX UP BY +1                             GC024015
063400        ELSE                                                      GC024015
063500           NEXT SENTENCE                                          GC024015
063600     ELSE                                                         GC024015
063700        IF WS-AGE-LMT-T (WS-A-INDEX) NOT  =                       GC024015
063800           WS-AGE-LMT-T (WS-B-INDEX)                              GC024015
063900        OR WS-AGE-LMT-F (WS-A-INDEX) NOT  =                       GC024015
064000           WS-AGE-LMT-F (WS-B-INDEX)                              GC024015
064100           ADD 1 TO DF-ERROR-COUNT                                GC024015
064200           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                  GC024015
064300           MOVE 'A30' TO DF-ERR-CODE (DF-ERROR-INDEX)             GC024015
064400           SET DF-ERROR-INDEX UP BY +1.                           GC024015
064500                                                                  GC024015
064600     SET  WS-B-INDEX  DOWN BY +1.                                 GC024015
064700                                                                  GC024015
064800 1170-EXIT.                                                       GC024015
064900     EXIT.                                                        GC024015
065000/                                                                 GC024015
065100*************************************************************     GC024015
065200 2000-ACL-EDITS.                                                  GC024015
065300                                                                  GC024015
065400     PERFORM 2010-000-ACL-EDIT1 THRU 2010-000-EXIT.               GC024015
065500*                                                                 GC024015
065600*--- START OF ISSR 11154                                          GC024015
065700*--- EDIT ALL OCCURRENCES FOR AGE LIMITS AND INTERNAL DESCRIPTOR  GC024015
065800                                                                  GC024015
065900     COMPUTE WS-OCCURS-ENTRIES = GAB-ENTRY-COUNT - 1.             GC024015
066000                                                                  GC024015
066100     IF  WS-OCCURS-ENTRIES     <   +2                             GC024015
066200         GO TO   2000-EXIT.                                       GC024015
066300                                                                  GC024015
066400     SET  WS-A-INDEX  TO +1.                                      GC024015
066500     PERFORM   2150-LOAD-AGE-LIMITS-ACL THRU 2150-EXIT            GC024015
066600         VARYING  GAB-INDEX  FROM +1  BY +1                       GC024015
066700           UNTIL  GAB-INDEX  >  WS-OCCURS-ENTRIES.                GC024015
066800                                                                  GC024015
066900     MOVE  'Y'  TO  WS-SORT-EXCHANGE-IND.                         GC024015
067000     PERFORM 7000-OCCURANCE-SORT THRU 7000-EXIT                   GC024015
067100        UNTIL WS-NO-EXCHANGE-MADE.                                GC024015
067200                                                                  GC024015
067300     SET  WS-A-INDEX  WS-B-INDEX  TO +1.                          GC024015
067400     PERFORM   2160-EDIT-AGE-LIMITS-ACL THRU 2160-EXIT UNTIL      GC024015
067500        WS-INT-DESC (WS-A-INDEX)  =  HIGH-VALUES  OR              GC024015
067600           WS-A-INDEX   >  WS-OCCURS-ENTRIES.                     GC024015
067700                                                                  GC024015
067800*--- END OF 11154                                                 GC024015
067900*                                                                 GC024015
068000 2000-EXIT.                                                       GC024015
068100     EXIT.                                                        GC024015
068200                                                                  GC024015
068300***************************************************************** GC024015
068400* USER REQUIREMENTS FOR EDIT D222. FOR ACL AND AOL RECORDS.       GC024015
068500*                                                                 GC024015
068600*     FOR ALL OCCURS, UNLESS AN ERROR CONDITION IS MET,           GC024015
068700*     IF THE ASCEND/DESCEND IND. = '1' OR '2' OR '3'              GC024015
068800*         CHECK THE OTHER OCCURS FOR AN ASCEND/DESCEND IND WITH   GC024015
068900*         THE SAME VALUE (THERE MAY BE SEVERAL OCCURS)            GC024015
069000*         IF NONE ARE FOUND,                                      GC024015
069100*             ASSIGN AN ERROR CODE                                GC024015
069200*         ELSE                                                    GC024015
069300*             COMPARE THE FIELDS FROM THE ONE OCCURS WITH THE     GC024015
069400*             FIELDS FROM THE OTHER OCCURS (SEE CODE FOR THE      GC024015
069500*             SPECIFIC FIELD NAMES)                               GC024015
069600*             IF THE VALUES IN THE FIELDS CHECKED ARE ALL EQUAL   GC024015
069700*             TO EACH OTHER,                                      GC024015
069800*                 EXIT                                            GC024015
069900*             ELSE                                                GC024015
070000*                 ASSIGN AN ERROR CODE                            GC024015
070100*                 EXIT.                                           GC024015
070200***************************************************************** GC024015
070300 2010-000-ACL-EDIT1.                                              GC024015
070400                                                                  GC024015
070500     IF GAB-ENTRY-COUNT < 3                                       GC024015
070600         GO TO 2010-000-EXIT.                                     GC024015
070700     MOVE 'N' TO WS-FINISH-SW.                                    GC024015
070800     MOVE LOW-VALUES TO WS-TABULAR-REC-AREA.                      GC024015
070900     MOVE LS-TABULAR-REC-AREA TO WS-TABULAR-REC-AREA.             GC024015
071000     SET GAB-INDEX  TO +1.                                        GC024015
071100     PERFORM 2010-010-LOOP1 THRU 2010-010-EXIT                    GC024015
071200       VARYING GAB-INDEX FROM 1 BY 1                              GC024015
071300       UNTIL   GAB-INDEX = GAB-ENTRY-COUNT                        GC024015
071400          OR   WS-FINISH-SW = 'Y'.                                GC024015
071500                                                                  GC024015
071600 2010-000-EXIT.                                                   GC024015
071700     EXIT.                                                        GC024015
071800                                                                  GC024015
071900***************************************************************** GC024015
072000 2010-010-LOOP1.                                                  GC024015
072100     IF GAB-COINS-ASCEND-DESCEND-IND (GAB-INDEX) = '1' OR         GC024015
072200                                                   '2' OR '3'     GC024015
072300         SET GAB2-INDEX TO 1                                      GC024015
072400         MOVE 'N' TO WS-MATCH1-SW                                 GC024015
072500                     WS-MATCH2-SW                                 GC024015
072600         PERFORM 2010-020-LOOP2 THRU 2010-020-EXIT                GC024015
072700           VARYING GAB2-INDEX FROM 1 BY 1                         GC024015
072800           UNTIL   GAB2-INDEX = GAB2-ENTRY-COUNT                  GC024015
072900     ELSE                                                         GC024015
073000         GO TO 2010-010-EXIT.                                     GC024015
073100     IF WS-MATCH1-SW = 'N'                                        GC024015
073200         ADD 1 TO DF-ERROR-COUNT                                  GC024015
073300         MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                    GC024015
073400         MOVE 'A02' TO DF-ERR-CODE (DF-ERROR-INDEX)               GC024015
073500         SET DF-ERROR-INDEX UP BY 1                               GC024015
073600         MOVE 'Y' TO WS-FINISH-SW                                 GC024015
073700         GO TO  2010-010-EXIT.                                    GC024015
073800     IF WS-MATCH2-SW = 'N'                                        GC024015
073900         ADD 1 TO DF-ERROR-COUNT                                  GC024015
074000         MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                    GC024015
074100         MOVE 'A03' TO DF-ERR-CODE (DF-ERROR-INDEX)               GC024015
074200         SET DF-ERROR-INDEX UP BY 1                               GC024015
074300         MOVE 'Y' TO WS-FINISH-SW                                 GC024015
074400         GO TO  2010-010-EXIT.                                    GC024015
074500                                                                  GC024015
074600 2010-010-EXIT.                                                   GC024015
074700     EXIT.                                                        GC024015
074800                                                                  GC024015
074900***************************************************************** GC024015
075000 2010-020-LOOP2.                                                  GC024015
075100                                                                  GC024015
075200     IF GAB2-INDEX = GAB-INDEX                                    GC024015
075300         GO TO 2010-020-EXIT.                                     GC024015
075400     IF GAB-COINS-ASCEND-DESCEND-IND (GAB-INDEX)                  GC024015
075500       =                                                          GC024015
075600        GAB2-COINS-ASCEND-DESCEND-IND (GAB2-INDEX)                GC024015
075700         MOVE 'Y' TO WS-MATCH1-SW                                 GC024015
075800     ELSE                                                         GC024015
075900         GO TO 2010-020-EXIT.                                     GC024015
076000*******                                                           GC024015
076100* THE CONDITION BIT FILLER BIT IS PRIMED JUST IN CASE A MUTANT    GC024015
076200* VALUE EVER ENDS UP THERE.                                       GC024015
076300*                                                                 GC024015
076400     MOVE GAB-COND-FILLER-BIT (GAB-INDEX)                         GC024015
076500       TO GAB2-COND-FILLER-BIT (GAB2-INDEX).                      GC024015
076600*                                                                 GC024015
076700     IF ((GAB-COINS-BENEFIT-PERIOD (GAB-INDEX)                    GC024015
076800       = GAB2-COINS-BENEFIT-PERIOD (GAB2-INDEX))                  GC024015
076900       AND                                                        GC024015
077000        (GAB-COINS-L-O-B (GAB-INDEX)                              GC024015
077100       = GAB2-COINS-L-O-B (GAB2-INDEX))                           GC024015
077200       AND                                                        GC024015
077300        (GAB-COINS-FAM-OR-INDIV (GAB-INDEX)                       GC024015
077400       = GAB2-COINS-FAM-OR-INDIV (GAB2-INDEX))                    GC024015
077500       AND                                                        GC024015
077600        (GAB-COINS-COST-CONTAIN-IND (GAB-INDEX)                   GC024015
077700       = GAB2-COINS-COST-CONTAIN-IND (GAB2-INDEX))                GC024015
077800       AND                                                        GC024015
077900        (GAB-COINS-PLACE-OF-TREATMENT (GAB-INDEX)                 GC024015
078000       = GAB2-COINS-PLACE-OF-TREATMENT (GAB2-INDEX))              GC024015
078100       AND                                                        GC024015
078200        (GAB-COINS-VALUE-QUALIFIER (GAB-INDEX)                    GC024015
078300       = GAB2-COINS-VALUE-QUALIFIER (GAB2-INDEX))                 GC024015
078400       AND                                                        GC024015
078500        (GAB-COINS-LMT-MANDATORY-IND (GAB-INDEX)                  GC024015
078600       = GAB2-COINS-LMT-MANDATORY-IND (GAB2-INDEX))               GC024015
078700       AND                                                        GC024015
078800        (GAB-COINS-SERVICE-GROUP (GAB-INDEX)                      GC024015
078900       = GAB2-COINS-SERVICE-GROUP (GAB2-INDEX))                   GC024015
079000       AND                                                        GC024015
079100        (GAB-COINS-CO-PAY-IND (GAB-INDEX)                         GC024015
079200       = GAB2-COINS-CO-PAY-IND (GAB2-INDEX))                      GC024015
079300       AND                                                        GC024015
079400        (GAB-COINS-CONDITION (GAB-INDEX)                          GC024015
079500       = GAB2-COINS-CONDITION (GAB2-INDEX))                       GC024015
079600       AND                                                        GC024015
079700        (GAB-COINS-INTERNAL-DESCRIPTOR (GAB-INDEX)                GC024015
079800       = GAB2-COINS-INTERNAL-DESCRIPTOR (GAB2-INDEX))             GC024015
079900       AND                                                        GC024015
080000        (GAB-COINS-AGE-QUAL-IND-FROM (GAB-INDEX)                  GC024015
080100       = GAB2-COINS-AGE-QUAL-IND-FROM (GAB2-INDEX))               GC024015
080200       AND                                                        GC024015
080300         (GAB-COINS-AGE-QUAL-IND-TO (GAB-INDEX)                   GC024015
080400       =  GAB2-COINS-AGE-QUAL-IND-TO (GAB2-INDEX))                GC024015
080500       AND                                                        GC024015
080600         (GAB-COINS-AGE-LIMIT-FROM (GAB-INDEX)                    GC024015
080700       =  GAB2-COINS-AGE-LIMIT-FROM (GAB2-INDEX))                 GC024015
080800       AND                                                        GC024015
080900         (GAB-COINS-AGE-QUAL-IND-TO (GAB-INDEX)                   GC024015
081000       =  GAB2-COINS-AGE-QUAL-IND-TO (GAB2-INDEX)))               GC024015
081100         MOVE 'Y' TO WS-MATCH2-SW.                                GC024015
081200                                                                  GC024015
081300                                                                  GC024015
081400 2010-020-EXIT.                                                   GC024015
081500     EXIT.                                                        GC024015
081600                                                                  GC024015
081700***************************************************************** GC024015
081800 2150-LOAD-AGE-LIMITS-ACL.                                        GC024015
081900                                                                  GC024015
082000*--- INTERNAL DESCRIPTOR IS SPACES AND AGE-LIMITS IS ZEROS, SKIP. GC024015
082100     IF GAB-COINS-INTERNAL-DESCRIPTOR (GAB-INDEX) = SPACES AND    GC024015
082200        GAB-COINS-AGE-LIMIT-FROM (GAB-INDEX)     = ZEROS   AND    GC024015
082300        GAB-COINS-AGE-LIMIT-TO  (GAB-INDEX)      = ZEROS          GC024015
082400        GO TO   2150-EXIT.                                        GC024015
082500                                                                  GC024015
082600*--- IF AGE LIMIT FIELDS ARE  N O T  CODED THEN INTERNAL          GC024015
082700*--- DESCRIPTOR HAS NO EFFECT ON THE LOGIC, U N L E S S  THE SAME GC024015
082800*--- DESCRIPTOR IS ON DIFFERENT OCCURS AND THE AGE-LIMITS ARE     GC024015
082900*--- CODED, THIS WILL BE DETERMINED IN THE NEXT ROUTINE, BLANK    GC024015
083000*--- DESCRIPTOR WILL BE LOADED IN THE EDIT TABLE.                 GC024015
083100                                                                  GC024015
083200     MOVE GAB-COINS-AGE-LIMIT-FROM (GAB-INDEX)                    GC024015
083300        TO WS-AGE-LMT-F (WS-A-INDEX).                             GC024015
083400     IF GAB-COINS-AGE-QUAL-IND-FROM (GAB-INDEX) = 'W'             GC024015
083500        COMPUTE WS-AGE-LMT-F (WS-A-INDEX) =                       GC024015
083600                WS-AGE-LMT-F (WS-A-INDEX) * 7                     GC024015
083700     ELSE                                                         GC024015
083800     IF GAB-COINS-AGE-QUAL-IND-FROM (GAB-INDEX) = 'M'             GC024015
083900        COMPUTE WS-AGE-LMT-F (WS-A-INDEX) =                       GC024015
084000                WS-AGE-LMT-F (WS-A-INDEX) * 30                    GC024015
084100     ELSE                                                         GC024015
084200     IF GAB-COINS-AGE-QUAL-IND-FROM (GAB-INDEX) = 'Y'             GC024015
084300        COMPUTE WS-AGE-LMT-F (WS-A-INDEX) =                       GC024015
084400                WS-AGE-LMT-F (WS-A-INDEX) * 365.                  GC024015
084500                                                                  GC024015
084600     MOVE  GAB-COINS-AGE-LIMIT-TO (GAB-INDEX)                     GC024015
084700       TO  WS-AGE-LMT-T (WS-A-INDEX).                             GC024015
084800     IF GAB-COINS-AGE-QUAL-IND-TO (GAB-INDEX) = 'W'               GC024015
084900        COMPUTE WS-AGE-LMT-T (WS-A-INDEX) =                       GC024015
085000                WS-AGE-LMT-T (WS-A-INDEX) * 7                     GC024015
085100     ELSE                                                         GC024015
085200     IF GAB-COINS-AGE-QUAL-IND-TO (GAB-INDEX) = 'M'               GC024015
085300        COMPUTE WS-AGE-LMT-T (WS-A-INDEX) =                       GC024015
085400                WS-AGE-LMT-T (WS-A-INDEX) * 30                    GC024015
085500     ELSE                                                         GC024015
085600     IF GAB-COINS-AGE-QUAL-IND-TO (GAB-INDEX) = 'Y'               GC024015
085700        COMPUTE WS-AGE-LMT-T (WS-A-INDEX) =                       GC024015
085800                WS-AGE-LMT-T (WS-A-INDEX) * 365.                  GC024015
085900                                                                  GC024015
086000     MOVE GAB-COINS-INTERNAL-DESCRIPTOR (GAB-INDEX) TO            GC024015
086100          WS-INT-DESC (WS-A-INDEX).                               GC024015
086200     MOVE GAB-COINS-ASCEND-DESCEND-IND (GAB-INDEX)  TO            GC024015
086300          WS-ASC-DES-IND (WS-A-INDEX).                            GC024015
086400     SET WS-A-INDEX  UP BY 1.                                     GC024015
086500*                                                                 GC024015
086600 2150-EXIT.                                                       GC024015
086700     EXIT.                                                        GC024015
086800******************************************************************GC024015
086900*    EDIT AGE-LIMITS AMD INTERNAL DESCRIPTORS FOR OCCURENCES      GC024015
087000*--- ARE AGE LIMITS ZEROS? YES, SEARCH FOR MATCHED DESCRIPTOR.    GC024015
087100*--- NO MATCH FOUND, THEN AGE LIMITS NOT TO BE CODED,             GC024015
087200*--- YES MATCH FOUND, AND AGE LIMITS ARE CODED, THEN AGE LIMITS   GC024015
087300*--- FROM TABLE MUST BE CODED AS OTHER OCCUR WITH SAME DESCRIPTOR.GC024015
087400*--- VALUE OF SPACES IN THE DESCRIPTOR WILL MATCH WITH SAME.      GC024015
087500******************************************************************GC024015
087600 2160-EDIT-AGE-LIMITS-ACL.                                        GC024015
087700                                                                  GC024015
087800     IF WS-AGE-LMT-T(WS-A-INDEX)  =  ZEROS                        GC024015
087900        NEXT SENTENCE                                             GC024015
088000     ELSE                                                         GC024015
088100        IF WS-INT-DESC (WS-A-INDEX)  =                            GC024015
088200                       WS-INT-DESC (WS-A-INDEX + 1)               GC024015
088300           PERFORM  2170-EDIT-BOTH-OCCURS THRU 2170-EXIT.         GC024015
088400                                                                  GC024015
088500     SET WS-A-INDEX  WS-B-INDEX  UP BY +1.                        GC024015
088600                                                                  GC024015
088700 2160-EXIT.                                                       GC024015
088800     EXIT.                                                        GC024015
088900                                                                  GC024015
089000******************************************************************GC024015
089100* IF INTERNAL DESCRIPTION MATCHS THEN AGE LIMIT SHOULD BE CODED,  GC024015
089200* RANGE OF AGE-LIMIT YEARS MUST BE IN ASCENDING SEQUENCE AND NOT  GC024015
089300* OVERLAPING OR LAPSED,           OK        OVERLAPED   LAPSED    GC024015
089400*                              005 - 010    005 010     005 010   GC024015
089500*           EXMP:              011 - 020    010 020     015 020   GC024015
089600*                              021 - 040    020 040     025 040   GC024015
089700******************************************************************GC024015
089800 2170-EDIT-BOTH-OCCURS.                                           GC024015
089900                                                                  GC024015
090000     SET  WS-B-INDEX  UP BY +1.                                   GC024015
090100                                                                  GC024015
090200     IF WS-ASC-DES-IND (WS-A-INDEX) < HIGH-VALUES  AND            GC024015
090300        WS-ASC-DES-IND (WS-A-INDEX) > SPACE        AND            GC024015
090400        WS-ASC-DES-IND (WS-A-INDEX) NOT = '0'      AND            GC024015
090500        WS-ASC-DES-IND (WS-A-INDEX) = WS-ASC-DES-IND (WS-B-INDEX) GC024015
090600        MOVE 'Y' TO WS-CHAIN-SW                                   GC024015
090700     ELSE                                                         GC024015
090800        MOVE 'N' TO WS-CHAIN-SW.                                  GC024015
090900                                                                  GC024015
091000* OVERLAPPED                                                      GC024015
091100     IF WS-CHAIN-SW = 'N'                                         GC024015
091200        IF WS-AGE-LMT-T (WS-A-INDEX) NOT  <                       GC024015
091300           WS-AGE-LMT-F (WS-B-INDEX)                              GC024015
091400           ADD 1 TO DF-ERROR-COUNT                                GC024015
091500           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                  GC024015
091600           MOVE 'A07' TO DF-ERR-CODE (DF-ERROR-INDEX)             GC024015
091700           SET DF-ERROR-INDEX UP BY +1                            GC024015
091800        ELSE                                                      GC024015
091900           NEXT SENTENCE                                          GC024015
092000     ELSE                                                         GC024015
092100        IF WS-AGE-LMT-T (WS-A-INDEX) NOT  =                       GC024015
092200           WS-AGE-LMT-T (WS-B-INDEX)                              GC024015
092300        OR WS-AGE-LMT-F (WS-A-INDEX) NOT  =                       GC024015
092400           WS-AGE-LMT-F (WS-B-INDEX)                              GC024015
092500           ADD 1 TO DF-ERROR-COUNT                                GC024015
092600           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                  GC024015
092700           MOVE 'A10' TO DF-ERR-CODE (DF-ERROR-INDEX)             GC024015
092800           SET DF-ERROR-INDEX UP BY +1.                           GC024015
092900                                                                  GC024015
093000     SET  WS-B-INDEX  DOWN BY +1.                                 GC024015
093100                                                                  GC024015
093200 2170-EXIT.                                                       GC024015
093300     EXIT.                                                        GC024015
093400/                                                                 GC024015
093500***************************************************************** GC024015
093600 2500-ACP-EDITS.                                                  GC024015
093700*                                                                 GC024015
093800     PERFORM 2510-000-ACP-EDIT1 THRU 2510-000-EXIT.               GC024015
093900*--- START OF ISSR 11154                                          GC024015
094000*--- EDIT ALL OCCURRENCES FOR AGE LIMITS AND INTERNAL DESCRIPTOR  GC024015
094100                                                                  GC024015
094200*>>>> COMMENT OUT THE AGE LIMIT ROUTINES PER ED WITKUS 03/21/91   GC024015
094300                                                                  GC024015
094400     COMPUTE WS-OCCURS-ENTRIES = GAF-ENTRY-COUNT - 1.             GC024015
094500                                                                  GC024015
094600     IF WS-OCCURS-ENTRIES     <   +2                              GC024015
094700        PERFORM 2540-PROCESS-ACP THRU 2540-EXIT                   GC024015
094800        GO TO   2500-EXIT.                                        GC024015
094900                                                                  GC024015
095000     SET  WS-A-INDEX  TO +1.                                      GC024015
095100     PERFORM   2510-LOAD-AGE-LIMITS-ACP THRU 2510-EXIT            GC024015
095200         VARYING  GAF-INDEX  FROM +1  BY +1                       GC024015
095300           UNTIL  GAF-INDEX  >  WS-OCCURS-ENTRIES.                GC024015
095400                                                                  GC024015
095500     MOVE  'Y'  TO  WS-SORT-EXCHANGE-IND.                         GC024015
095600     PERFORM 7000-OCCURANCE-SORT THRU 7000-EXIT                   GC024015
095700        UNTIL WS-NO-EXCHANGE-MADE.                                GC024015
095800                                                                  GC024015
095900     SET  WS-A-INDEX  WS-B-INDEX  TO +1.                          GC024015
096000     PERFORM   2520-EDIT-AGE-LIMITS-ACP THRU 2520-EXIT  UNTIL     GC024015
096100        WS-INT-DESC (WS-A-INDEX)  =  HIGH-VALUES   OR             GC024015
096200           WS-A-INDEX   >  WS-OCCURS-ENTRIES.                     GC024015
096300                                                                  GC024015
096400*--- END OF 11154                                                 GC024015
096500*                                                                 GC024015
096600                                                                  GC024015
096700*--- D15182                                                       GC024015
096800     PERFORM 2540-PROCESS-ACP THRU 2540-EXIT.                     GC024015
096900                                                                  GC024015
097000 2500-EXIT.                                                       GC024015
097100     EXIT.                                                        GC024015
097200***************************************************************** GC024015
097300* P09400 11/08/06 ASC/DESC EDIT FOR #ACP                          GC024015
097400*                                                                 GC024015
097500*     FOR ALL OCCURS, UNLESS AN ERROR CONDITION IS MET,           GC024015
097600*     IF THE ASCEND/DESCEND IND. = '1' OR '2' OR '3'              GC024015
097700*         CHECK THE OTHER OCCURS FOR AN ASCEND/DESCEND IND WITH   GC024015
097800*         THE SAME VALUE (THERE MAY BE SEVERAL OCCURS)            GC024015
097900*         IF NONE ARE FOUND,                                      GC024015
098000*             ASSIGN AN ERROR CODE                                GC024015
098100*         ELSE                                                    GC024015
098200*             COMPARE THE FIELDS FROM THE ONE OCCURS WITH THE     GC024015
098300*             FIELDS FROM THE OTHER OCCURS (SEE CODE FOR THE      GC024015
098400*             SPECIFIC FIELD NAMES)                               GC024015
098500*             IF THE VALUES IN THE FIELDS CHECKED ARE ALL EQUAL   GC024015
098600*             TO EACH OTHER,                                      GC024015
098700*                 EXIT                                            GC024015
098800*             ELSE                                                GC024015
098900*                 ASSIGN AN ERROR CODE                            GC024015
099000*                 EXIT.                                           GC024015
099100***************************************************************** GC024015
099200 2510-000-ACP-EDIT1.                                              GC024015
099300                                                                  GC024015
099400     IF GAF-ENTRY-COUNT < 3                                       GC024015
099500         GO TO 2510-000-EXIT.                                     GC024015
099600     MOVE 'N' TO WS-FINISH-SW.                                    GC024015
099700     MOVE LOW-VALUES TO WS-TABULAR-REC-AREA.                      GC024015
099800     MOVE LS-TABULAR-REC-AREA TO WS-TABULAR-REC-AREA.             GC024015
099900     SET GAF-INDEX  TO +1.                                        GC024015
100000     PERFORM 2510-010-LOOP1 THRU 2510-010-EXIT                    GC024015
100100       VARYING GAF-INDEX FROM 1 BY 1                              GC024015
100200       UNTIL   GAF-INDEX = GAF-ENTRY-COUNT                        GC024015
100300          OR   WS-FINISH-SW = 'Y'.                                GC024015
100400                                                                  GC024015
100500 2510-000-EXIT.                                                   GC024015
100600     EXIT.                                                        GC024015
100700                                                                  GC024015
100800***************************************************************** GC024015
100900 2510-010-LOOP1.                                                  GC024015
101000     IF GAF-COPAY-ASCEND-DESCEND-IND (GAF-INDEX) = '1' OR         GC024015
101100                                                   '2' OR '3'     GC024015
101200         SET GAF2-INDEX TO 1                                      GC024015
101300         MOVE 'N' TO WS-MATCH1-SW                                 GC024015
101400                     WS-MATCH2-SW                                 GC024015
101500         PERFORM 2510-020-LOOP2 THRU 2510-020-EXIT                GC024015
101600           VARYING GAF2-INDEX FROM 1 BY 1                         GC024015
101700           UNTIL   GAF2-INDEX = GAF2-ENTRY-COUNT                  GC024015
101800     ELSE                                                         GC024015
101900         GO TO 2510-010-EXIT.                                     GC024015
102000     IF WS-MATCH1-SW = 'N'                                        GC024015
102100         ADD 1 TO DF-ERROR-COUNT                                  GC024015
102200         MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                    GC024015
102300         MOVE 'A31' TO DF-ERR-CODE (DF-ERROR-INDEX)               GC024015
102400         SET DF-ERROR-INDEX UP BY 1                               GC024015
102500         MOVE 'Y' TO WS-FINISH-SW                                 GC024015
102600         GO TO  2510-010-EXIT.                                    GC024015
102700     IF WS-MATCH2-SW = 'N'                                        GC024015
102800         ADD 1 TO DF-ERROR-COUNT                                  GC024015
102900         MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                    GC024015
103000         MOVE 'A32' TO DF-ERR-CODE (DF-ERROR-INDEX)               GC024015
103100         SET DF-ERROR-INDEX UP BY 1                               GC024015
103200         MOVE 'Y' TO WS-FINISH-SW                                 GC024015
103300         GO TO  2510-010-EXIT.                                    GC024015
103400                                                                  GC024015
103500 2510-010-EXIT.                                                   GC024015
103600     EXIT.                                                        GC024015
103700                                                                  GC024015
103800***************************************************************** GC024015
103900 2510-020-LOOP2.                                                  GC024015
104000                                                                  GC024015
104100     IF GAF2-INDEX = GAF-INDEX                                    GC024015
104200         GO TO 2510-020-EXIT.                                     GC024015
104300     IF GAF-COPAY-ASCEND-DESCEND-IND (GAF-INDEX)                  GC024015
104400       =                                                          GC024015
104500        GAF2-COPAY-ASCEND-DESCEND-IND (GAF2-INDEX)                GC024015
104600         MOVE 'Y' TO WS-MATCH1-SW                                 GC024015
104700     ELSE                                                         GC024015
104800         GO TO 2510-020-EXIT.                                     GC024015
104900*******                                                           GC024015
105000* THE CONDITION BIT FILLER BIT IS PRIMED JUST IN CASE A MUTANT    GC024015
105100* VALUE EVER ENDS UP THERE.                                       GC024015
105200*                                                                 GC024015
105300     MOVE GAF-COND-FILLER-BIT (GAF-INDEX)                         GC024015
105400       TO GAF2-COND-FILLER-BIT (GAF2-INDEX).                      GC024015
105500*                                                                 GC024015
105600     IF ((GAF-COPAY-BENEFIT-PERIOD (GAF-INDEX)                    GC024015
105700       = GAF2-COPAY-BENEFIT-PERIOD (GAF2-INDEX))                  GC024015
105800       AND                                                        GC024015
105900        (GAF-COPAY-L-O-B (GAF-INDEX)                              GC024015
106000       = GAF2-COPAY-L-O-B (GAF2-INDEX))                           GC024015
106100       AND                                                        GC024015
106200        (GAF-COPAY-FAM-OR-INDIV (GAF-INDEX)                       GC024015
106300       = GAF2-COPAY-FAM-OR-INDIV (GAF2-INDEX))                    GC024015
106400       AND                                                        GC024015
106500        (GAF-COPAY-COST-CONTAIN-IND (GAF-INDEX)                   GC024015
106600       = GAF2-COPAY-COST-CONTAIN-IND (GAF2-INDEX))                GC024015
106700       AND                                                        GC024015
106800        (GAF-COPAY-PLACE-OF-TREATMENT (GAF-INDEX)                 GC024015
106900       = GAF2-COPAY-PLACE-OF-TREATMENT (GAF2-INDEX))              GC024015
107000       AND                                                        GC024015
107100        (GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX)                    GC024015
107200       = GAF2-COPAY-VALUE-QUALIFIER (GAF2-INDEX))                 GC024015
107300       AND                                                        GC024015
107400        (GAF-COPAY-MANDATORY-IND (GAF-INDEX)                      GC024015
107500       = GAF2-COPAY-MANDATORY-IND (GAF2-INDEX))                   GC024015
107600       AND                                                        GC024015
107700        (GAF-COPAY-SERVICE-GROUP (GAF-INDEX)                      GC024015
107800       = GAF2-COPAY-SERVICE-GROUP (GAF2-INDEX))                   GC024015
107900       AND                                                        GC024015
108000        (GAF-COPAY-CO-PAY-IND (GAF-INDEX)                         GC024015
108100       = GAF2-COPAY-CO-PAY-IND (GAF2-INDEX))                      GC024015
108200       AND                                                        GC024015
108300        (GAF-COPAY-CONDITION (GAF-INDEX)                          GC024015
108400       = GAF2-COPAY-CONDITION (GAF2-INDEX))                       GC024015
108500       AND                                                        GC024015
108600        (GAF-COPAY-INTERNAL-DESCRIPTOR (GAF-INDEX)                GC024015
108700       = GAF2-COPAY-INTERNAL-DESCRIPTOR (GAF2-INDEX))             GC024015
108800       AND                                                        GC024015
108900        (GAF-COPAY-AGE-QUAL-IND-FROM (GAF-INDEX)                  GC024015
109000       = GAF2-COPAY-AGE-QUAL-IND-FROM (GAF2-INDEX))               GC024015
109100       AND                                                        GC024015
109200         (GAF-COPAY-AGE-QUAL-IND-TO (GAF-INDEX)                   GC024015
109300       =  GAF2-COPAY-AGE-QUAL-IND-TO (GAF2-INDEX))                GC024015
109400       AND                                                        GC024015
109500         (GAF-COPAY-AGE-LIMIT-FROM (GAF-INDEX)                    GC024015
109600       =  GAF2-COPAY-AGE-LIMIT-FROM (GAF2-INDEX))                 GC024015
109700       AND                                                        GC024015
109800         (GAF-COPAY-AGE-QUAL-IND-TO (GAF-INDEX)                   GC024015
109900       =  GAF2-COPAY-AGE-QUAL-IND-TO (GAF2-INDEX)))               GC024015
110000         MOVE 'Y' TO WS-MATCH2-SW.                                GC024015
110100                                                                  GC024015
110200                                                                  GC024015
110300 2510-020-EXIT.                                                   GC024015
110400     EXIT.                                                        GC024015
110500                                                                  GC024015
110600****************************************************************  GC024015
110700 2510-LOAD-AGE-LIMITS-ACP.                                        GC024015
110800                                                                  GC024015
110900*--- INTERNAL DESCRIPTOR IS SPACES AND AGE-LIMITS IS ZEROS, SKIP. GC024015
111000     IF GAF-COPAY-INTERNAL-DESCRIPTOR (GAF-INDEX) = SPACES AND    GC024015
111100        GAF-COPAY-AGE-LIMIT-FROM (GAF-INDEX)     = ZEROS   AND    GC024015
111200        GAF-COPAY-AGE-LIMIT-TO  (GAF-INDEX)      = ZEROS          GC024015
111300        GO TO   2510-EXIT.                                        GC024015
111400                                                                  GC024015
111500*--- IF AGE LIMIT FIELDS ARE  N O T  CODED THEN INTERNAL          GC024015
111600*--- DESCRIPTOR HAS NO EFFECT ON THE LOGIC, U N L E S S  THE SAME GC024015
111700*--- DESCRIPTOR IS ON DIFFERENT OCCURS AND THE AGE-LIMITS ARE     GC024015
111800*--- CODED, THIS WILL BE DETERMINED IN THE NEXT ROUTINE, BLANK    GC024015
111900*--- DESCRIPTOR WILL BE LOADED IN THE EDIT TABLE.                 GC024015
112000                                                                  GC024015
112100     MOVE GAF-COPAY-AGE-LIMIT-FROM (GAF-INDEX)                    GC024015
112200       TO WS-AGE-LMT-F (WS-A-INDEX).                              GC024015
112300     IF GAF-COPAY-AGE-QUAL-IND-FROM (GAF-INDEX) = 'W'             GC024015
112400        COMPUTE WS-AGE-LMT-F (WS-A-INDEX) =                       GC024015
112500                WS-AGE-LMT-F (WS-A-INDEX) * 7                     GC024015
112600     ELSE                                                         GC024015
112700     IF GAF-COPAY-AGE-QUAL-IND-FROM (GAF-INDEX) = 'M'             GC024015
112800        COMPUTE WS-AGE-LMT-F (WS-A-INDEX) =                       GC024015
112900                WS-AGE-LMT-F (WS-A-INDEX) * 30                    GC024015
113000     ELSE                                                         GC024015
113100     IF GAF-COPAY-AGE-QUAL-IND-FROM (GAF-INDEX) = 'Y'             GC024015
113200        COMPUTE WS-AGE-LMT-F (WS-A-INDEX) =                       GC024015
113300                WS-AGE-LMT-F (WS-A-INDEX) * 365.                  GC024015
113400                                                                  GC024015
113500     MOVE GAF-COPAY-AGE-LIMIT-TO (GAF-INDEX)                      GC024015
113600       TO WS-AGE-LMT-T (WS-A-INDEX).                              GC024015
113700     IF GAF-COPAY-AGE-QUAL-IND-TO (GAF-INDEX) = 'W'               GC024015
113800        COMPUTE WS-AGE-LMT-T (WS-A-INDEX) =                       GC024015
113900                WS-AGE-LMT-T (WS-A-INDEX) * 7                     GC024015
114000     ELSE                                                         GC024015
114100     IF GAF-COPAY-AGE-QUAL-IND-TO (GAF-INDEX) = 'M'               GC024015
114200        COMPUTE WS-AGE-LMT-T (WS-A-INDEX) =                       GC024015
114300                WS-AGE-LMT-T (WS-A-INDEX) * 30                    GC024015
114400     ELSE                                                         GC024015
114500     IF GAF-COPAY-AGE-QUAL-IND-TO (GAF-INDEX) = 'Y'               GC024015
114600        COMPUTE WS-AGE-LMT-T (WS-A-INDEX) =                       GC024015
114700                WS-AGE-LMT-T (WS-A-INDEX) * 365.                  GC024015
114800                                                                  GC024015
114900     MOVE GAF-COPAY-INTERNAL-DESCRIPTOR (GAF-INDEX) TO            GC024015
115000          WS-INT-DESC (WS-A-INDEX).                               GC024015
115100     MOVE GAF-COPAY-ASCEND-DESCEND-IND (GAF-INDEX)  TO            GC024015
115200          WS-ASC-DES-IND (WS-A-INDEX).                            GC024015
115300     SET WS-A-INDEX  UP BY 1.                                     GC024015
115400*                                                                 GC024015
115500 2510-EXIT.                                                       GC024015
115600     EXIT.                                                        GC024015
115700******************************************************************GC024015
115800*    EDIT AGE-LIMITS AMD INTERNAL DESCRIPTORS FOR OCCURENCES      GC024015
115900*--- ARE AGE LIMITS ZEROS? YES, SEARCH FOR MATCHED DESCRIPTOR.    GC024015
116000*--- NO MATCH FOUND, THEN AGE LIMITS NOT TO BE CODED,             GC024015
116100*--- YES MATCH FOUND, AND AGE LIMITS ARE CODED, THEN AGE LIMITS   GC024015
116200*--- FROM TABLE MUST BE CODED AS OTHER OCCUR WITH SAME DESCRIPTOR.GC024015
116300*--- VALUE OF SPACES IN THE DESCRIPTOR WILL MATCH WITH SAME.      GC024015
116400******************************************************************GC024015
116500 2520-EDIT-AGE-LIMITS-ACP.                                        GC024015
116600                                                                  GC024015
116700     IF WS-AGE-LMT-T(WS-A-INDEX)  =  ZEROS                        GC024015
116800        NEXT SENTENCE                                             GC024015
116900     ELSE                                                         GC024015
117000        IF WS-INT-DESC (WS-A-INDEX)  =                            GC024015
117100                      WS-INT-DESC (WS-A-INDEX + 1)                GC024015
117200           PERFORM  2530-EDIT-BOTH-OCCURS THRU 2530-EXIT.         GC024015
117300                                                                  GC024015
117400     SET WS-A-INDEX  WS-B-INDEX  UP BY +1.                        GC024015
117500                                                                  GC024015
117600 2520-EXIT.                                                       GC024015
117700     EXIT.                                                        GC024015
117800                                                                  GC024015
117900******************************************************************GC024015
118000* IF INTERNAL DESCRIPTION MATCHS THEN AGE LIMIT SHOULD BE CODED,  GC024015
118100* RANGE OF AGE-LIMIT YEARS MUST BE IN ASCENDING SEQUENCE AND NOT  GC024015
118200* OVERLAPING OR LAPSED,           OK        OVERLAPED   LAPSED    GC024015
118300*                              005 - 010    005 010     005 010   GC024015
118400*           EXMP:              011 - 020    010 020     015 020   GC024015
118500*                              021 - 040    020 040     025 040   GC024015
118600******************************************************************GC024015
118700 2530-EDIT-BOTH-OCCURS.                                           GC024015
118800*                                                                 GC024015
118900     SET  WS-B-INDEX  UP BY +1.                                   GC024015
119000                                                                  GC024015
119100     IF WS-ASC-DES-IND (WS-A-INDEX) < HIGH-VALUES  AND            GC024015
119200        WS-ASC-DES-IND (WS-A-INDEX) > SPACE        AND            GC024015
119300        WS-ASC-DES-IND (WS-A-INDEX) NOT = '0'      AND            GC024015
119400        WS-ASC-DES-IND (WS-A-INDEX) = WS-ASC-DES-IND (WS-B-INDEX) GC024015
119500        MOVE 'Y' TO WS-CHAIN-SW                                   GC024015
119600     ELSE                                                         GC024015
119700        MOVE 'N' TO WS-CHAIN-SW.                                  GC024015
119800                                                                  GC024015
119900* OVERLAPPED                                                      GC024015
120000     IF WS-CHAIN-SW = 'N'                                         GC024015
120100       IF WS-AGE-LMT-T (WS-A-INDEX) NOT  <                        GC024015
120200          WS-AGE-LMT-F (WS-B-INDEX)                               GC024015
120300          ADD 1 TO DF-ERROR-COUNT                                 GC024015
120400          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
120500          MOVE 'A33' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
120600          SET DF-ERROR-INDEX UP BY +1                             GC024015
120700        ELSE                                                      GC024015
120800           NEXT SENTENCE                                          GC024015
120900     ELSE                                                         GC024015
121000        IF WS-AGE-LMT-T (WS-A-INDEX) NOT  =                       GC024015
121100           WS-AGE-LMT-T (WS-B-INDEX)                              GC024015
121200        OR WS-AGE-LMT-F (WS-A-INDEX) NOT  =                       GC024015
121300           WS-AGE-LMT-F (WS-B-INDEX)                              GC024015
121400           ADD 1 TO DF-ERROR-COUNT                                GC024015
121500           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                  GC024015
121600           MOVE 'A34' TO DF-ERR-CODE (DF-ERROR-INDEX)             GC024015
121700           SET DF-ERROR-INDEX UP BY +1.                           GC024015
121800                                                                  GC024015
121900*                                                                 GC024015
122000     SET  WS-B-INDEX  DOWN BY +1.                                 GC024015
122100 2530-EXIT.                                                       GC024015
122200     EXIT.                                                        GC024015
122300/                                                                 GC024015
122400 2540-PROCESS-ACP.                                                GC024015
122500                                                                  GC024015
122600*--- D15182  -  SEQUENCE                                          GC024015
122700     MOVE SPACES  TO  WS-SEQ-SWITCH.                              GC024015
122800     PERFORM 2600-LOAD-SEQUENCE THRU 2600-EXIT                    GC024015
122900             VARYING GAF-INDEX FROM 1 BY 1                        GC024015
123000             UNTIL   GAF-INDEX > GAF-ENTRY-COUNT.                 GC024015
123100     PERFORM 2610-SEQUENCE-CHECK  THRU 2610-EXIT.                 GC024015
123200                                                                  GC024015
123300*--- D15182  -  +CDE                                              GC024015
123400     PERFORM 2700-MOVE-CDE-FIELDS THRU 2700-EXIT                  GC024015
123500             VARYING GAF-INDEX  FROM 1 BY 1                       GC024015
123600             UNTIL   GAF-INDEX  = GAF-ENTRY-COUNT.                GC024015
123700     PERFORM 2710-CDE-CHECK     THRU 2710-EXIT                    GC024015
123800             VARYING GAF-INDEX  FROM 1 BY 1                       GC024015
123900             UNTIL   GAF-INDEX  = GAF-ENTRY-COUNT.                GC024015
124000                                                                  GC024015
124100*--- D15182  -  DEFINITION                                        GC024015
124200     MOVE SPACES  TO  WS-DEF-SWITCH.                              GC024015
124300     PERFORM 2800-LOAD-DEFINITION THRU 2800-EXIT                  GC024015
124400             VARYING GAF-INDEX FROM 1 BY 1                        GC024015
124500             UNTIL   GAF-INDEX = GAF-ENTRY-COUNT.                 GC024015
124600     PERFORM 2810-DEFINITION-CHECK  THRU 2810-EXIT.               GC024015
124700                                                                  GC024015
124800*--- D15182  -  BENEFIT PERIOD                                    GC024015
124900     PERFORM 2900-CHECK-BEN-PRD   THRU 2900-EXIT                  GC024015
125000             VARYING GAF-INDEX FROM 1 BY 1                        GC024015
125100             UNTIL   GAF-INDEX = GAF-ENTRY-COUNT.                 GC024015
125200                                                                  GC024015
125300 2540-EXIT.                                                       GC024015
125400     EXIT.                                                        GC024015
125500/                                                                 GC024015
125600 2600-LOAD-SEQUENCE.                                              GC024015
125700                                                                  GC024015
125800     IF GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)  =  'A1'            GC024015
125900        MOVE 'Y'  TO  WS-SEQ-A1                                   GC024015
126000        MOVE GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)                GC024015
126100        TO   WS-SEQ-1 (GAF-INDEX)                                 GC024015
126200        GO TO 2600-EXIT.                                          GC024015
126300                                                                  GC024015
126400     IF GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)  =  'A2'            GC024015
126500        IF A1                                                     GC024015
126600           MOVE 'Y'  TO  WS-SEQ-A2                                GC024015
126700           MOVE GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)             GC024015
126800           TO   WS-SEQ-2 (GAF-INDEX)                              GC024015
126900           GO TO 2600-EXIT                                        GC024015
127000        ELSE                                                      GC024015
127100           ADD 1 TO DF-ERROR-COUNT                                GC024015
127200           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                  GC024015
127300           MOVE 'A13' TO DF-ERR-CODE (DF-ERROR-INDEX)             GC024015
127400           SET DF-ERROR-INDEX UP BY +1                            GC024015
127500           GO TO 2600-EXIT                                        GC024015
127600     ELSE                                                         GC024015
127700        IF A1                                                     GC024015
127800           ADD 1 TO DF-ERROR-COUNT                                GC024015
127900           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                  GC024015
128000           MOVE 'A13' TO DF-ERR-CODE (DF-ERROR-INDEX)             GC024015
128100           SET DF-ERROR-INDEX UP BY +1                            GC024015
128200           GO TO 2600-EXIT.                                       GC024015
128300                                                                  GC024015
128400     IF GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)  =  'B1'            GC024015
128500        MOVE 'Y'  TO  WS-SEQ-B1                                   GC024015
128600        MOVE GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)                GC024015
128700        TO   WS-SEQ-1 (GAF-INDEX)                                 GC024015
128800        GO TO 2600-EXIT.                                          GC024015
128900                                                                  GC024015
129000     IF GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)  =  'B2'            GC024015
129100        IF B1                                                     GC024015
129200           MOVE 'Y'  TO  WS-SEQ-B2                                GC024015
129300           MOVE GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)             GC024015
129400           TO   WS-SEQ-2 (GAF-INDEX)                              GC024015
129500        ELSE                                                      GC024015
129600           ADD 1 TO DF-ERROR-COUNT                                GC024015
129700           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                  GC024015
129800           MOVE 'A14' TO DF-ERR-CODE (DF-ERROR-INDEX)             GC024015
129900           SET DF-ERROR-INDEX UP BY +1                            GC024015
130000           GO TO 2600-EXIT                                        GC024015
130100     ELSE                                                         GC024015
130200        IF B1                                                     GC024015
130300           ADD 1 TO DF-ERROR-COUNT                                GC024015
130400           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                  GC024015
130500           MOVE 'A14' TO DF-ERR-CODE (DF-ERROR-INDEX)             GC024015
130600           SET DF-ERROR-INDEX UP BY +1                            GC024015
130700           GO TO 2600-EXIT.                                       GC024015
130800                                                                  GC024015
130900     IF GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)  =  'C1'            GC024015
131000        MOVE 'Y'  TO  WS-SEQ-C1                                   GC024015
131100        MOVE GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)                GC024015
131200        TO   WS-SEQ-1 (GAF-INDEX)                                 GC024015
131300        GO TO 2600-EXIT.                                          GC024015
131400                                                                  GC024015
131500     IF GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)  =  'C2'            GC024015
131600        IF C1                                                     GC024015
131700           MOVE 'Y'  TO  WS-SEQ-C2                                GC024015
131800           MOVE GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)             GC024015
131900           TO   WS-SEQ-2 (GAF-INDEX)                              GC024015
132000        ELSE                                                      GC024015
132100           ADD 1 TO DF-ERROR-COUNT                                GC024015
132200           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                  GC024015
132300           MOVE 'A15' TO DF-ERR-CODE (DF-ERROR-INDEX)             GC024015
132400           SET DF-ERROR-INDEX UP BY +1                            GC024015
132500           GO TO 2600-EXIT                                        GC024015
132600     ELSE                                                         GC024015
132700        IF C1                                                     GC024015
132800           ADD 1 TO DF-ERROR-COUNT                                GC024015
132900           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                  GC024015
133000           MOVE 'A15' TO DF-ERR-CODE (DF-ERROR-INDEX)             GC024015
133100           SET DF-ERROR-INDEX UP BY +1                            GC024015
133200           GO TO 2600-EXIT.                                       GC024015
133300                                                                  GC024015
133400     IF GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)  =  'D1'            GC024015
133500        MOVE 'Y'  TO  WS-SEQ-D1                                   GC024015
133600        MOVE GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)                GC024015
133700        TO   WS-SEQ-1 (GAF-INDEX)                                 GC024015
133800        GO TO 2600-EXIT.                                          GC024015
133900                                                                  GC024015
134000     IF GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)  =  'D2'            GC024015
134100        IF D1                                                     GC024015
134200           MOVE 'Y'  TO  WS-SEQ-D2                                GC024015
134300           MOVE GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)             GC024015
134400           TO   WS-SEQ-2 (GAF-INDEX)                              GC024015
134500        ELSE                                                      GC024015
134600           ADD 1 TO DF-ERROR-COUNT                                GC024015
134700           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                  GC024015
134800           MOVE 'A16' TO DF-ERR-CODE (DF-ERROR-INDEX)             GC024015
134900           SET DF-ERROR-INDEX UP BY +1                            GC024015
135000           GO TO 2600-EXIT                                        GC024015
135100     ELSE                                                         GC024015
135200        IF D1                                                     GC024015
135300           ADD 1 TO DF-ERROR-COUNT                                GC024015
135400           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                  GC024015
135500           MOVE 'A16' TO DF-ERR-CODE (DF-ERROR-INDEX)             GC024015
135600           SET DF-ERROR-INDEX UP BY +1                            GC024015
135700           GO TO 2600-EXIT.                                       GC024015
135800                                                                  GC024015
135900     IF GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)  =  'E1'            GC024015
136000        MOVE 'Y'  TO  WS-SEQ-E1                                   GC024015
136100        MOVE GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)                GC024015
136200        TO   WS-SEQ-1 (GAF-INDEX)                                 GC024015
136300        GO TO 2600-EXIT.                                          GC024015
136400                                                                  GC024015
136500     IF GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)  =  'E2'            GC024015
136600        IF E1                                                     GC024015
136700           MOVE 'Y'  TO  WS-SEQ-E2                                GC024015
136800           MOVE GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)             GC024015
136900           TO   WS-SEQ-2 (GAF-INDEX)                              GC024015
137000        ELSE                                                      GC024015
137100           ADD 1 TO DF-ERROR-COUNT                                GC024015
137200           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                  GC024015
137300           MOVE 'A17' TO DF-ERR-CODE (DF-ERROR-INDEX)             GC024015
137400           SET DF-ERROR-INDEX UP BY +1                            GC024015
137500           GO TO 2600-EXIT                                        GC024015
137600     ELSE                                                         GC024015
137700        IF E1                                                     GC024015
137800           ADD 1 TO DF-ERROR-COUNT                                GC024015
137900           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                  GC024015
138000           MOVE 'A17' TO DF-ERR-CODE (DF-ERROR-INDEX)             GC024015
138100           SET DF-ERROR-INDEX UP BY +1                            GC024015
138200           GO TO 2600-EXIT.                                       GC024015
138300                                                                  GC024015
138400 2600-EXIT.                                                       GC024015
138500     EXIT.                                                        GC024015
138600/                                                                 GC024015
138700 2610-SEQUENCE-CHECK.                                             GC024015
138800                                                                  GC024015
138900     IF ( A1 AND A2 )  AND NOT                                    GC024015
139000        ( B1 AND B2 )                                             GC024015
139100        ADD 1 TO DF-ERROR-COUNT                                   GC024015
139200        MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                     GC024015
139300        MOVE 'A12' TO DF-ERR-CODE (DF-ERROR-INDEX)                GC024015
139400        SET DF-ERROR-INDEX UP BY +1.                              GC024015
139500                                                                  GC024015
139600     IF ( B1 AND B2 )                                             GC024015
139700        IF GAF-ENTRY-COUNT  >  +3                                 GC024015
139800           IF NOT ( C1 AND C2 )                                   GC024015
139900              ADD 1 TO DF-ERROR-COUNT                             GC024015
140000              MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT               GC024015
140100              MOVE 'A18' TO DF-ERR-CODE (DF-ERROR-INDEX)          GC024015
140200              SET DF-ERROR-INDEX UP BY +1.                        GC024015
140300                                                                  GC024015
140400     IF ( C1 AND C2 )                                             GC024015
140500        IF GAF-ENTRY-COUNT  >  +3                                 GC024015
140600           IF NOT ( D1 AND D2 )                                   GC024015
140700              ADD 1 TO DF-ERROR-COUNT                             GC024015
140800              MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT               GC024015
140900              MOVE 'A19' TO DF-ERR-CODE (DF-ERROR-INDEX)          GC024015
141000              SET DF-ERROR-INDEX UP BY +1.                        GC024015
141100                                                                  GC024015
141200     IF ( D1 AND D2 )                                             GC024015
141300        IF GAF-ENTRY-COUNT  >  +3                                 GC024015
141400           IF NOT ( E1 AND E2 )                                   GC024015
141500              ADD 1 TO DF-ERROR-COUNT                             GC024015
141600              MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT               GC024015
141700              MOVE 'A20' TO DF-ERR-CODE (DF-ERROR-INDEX)          GC024015
141800              SET DF-ERROR-INDEX UP BY +1.                        GC024015
141900                                                                  GC024015
142000 2610-EXIT.                                                       GC024015
142100     EXIT.                                                        GC024015
142200/                                                                 GC024015
142300 2700-MOVE-CDE-FIELDS.                                            GC024015
142400                                                                  GC024015
142500     MOVE GAF-COPAY-MANDATORY-IND (GAF-INDEX)                     GC024015
142600     TO   WS-COPAY-MANDATORY-IND  (GAF-INDEX).                    GC024015
142700                                                                  GC024015
142800     MOVE GAF-COPAY-BENEFIT-PERIOD (GAF-INDEX)                    GC024015
142900     TO   WS-COPAY-BENEFIT-PERIOD  (GAF-INDEX).                   GC024015
143000                                                                  GC024015
143100     MOVE GAF-COPAY-FAM-OR-INDIV (GAF-INDEX)                      GC024015
143200     TO   WS-COPAY-FAM-OR-INDIV  (GAF-INDEX).                     GC024015
143300                                                                  GC024015
143400     MOVE GAF-COPAY-L-O-B (GAF-INDEX)                             GC024015
143500     TO   WS-COPAY-L-O-B  (GAF-INDEX).                            GC024015
143600                                                                  GC024015
143700     MOVE GAF-COPAY-INTERNAL-DESCRIPTOR (GAF-INDEX)               GC024015
143800     TO   WS-COPAY-INTERNAL-DESCRIPTOR  (GAF-INDEX).              GC024015
143900                                                                  GC024015
144000     MOVE GAF-COPAY-SERVICE-GROUP (GAF-INDEX)                     GC024015
144100     TO   WS-COPAY-SERVICE-GROUP  (GAF-INDEX).                    GC024015
144200                                                                  GC024015
144300     MOVE GAF-COPAY-PLACE-OF-TREATMENT (GAF-INDEX)                GC024015
144400     TO   WS-COPAY-PLACE-OF-TREATMENT  (GAF-INDEX).               GC024015
144500                                                                  GC024015
144600     MOVE GAF-COPAY-CONDITION (GAF-INDEX)                         GC024015
144700     TO   WS-COPAY-CONDITION  (GAF-INDEX).                        GC024015
144800                                                                  GC024015
144900     MOVE GAF-COND-ALL-BIT (GAF-INDEX)                            GC024015
145000     TO   WS-COND-ALL-BIT  (GAF-INDEX).                           GC024015
145100                                                                  GC024015
145200     MOVE GAF-COND-EXCLUSION-BIT (GAF-INDEX)                      GC024015
145300     TO   WS-COND-EXCLUSION-BIT  (GAF-INDEX).                     GC024015
145400                                                                  GC024015
145500     MOVE GAF-COND-ICD-BIT (GAF-INDEX)                            GC024015
145600     TO   WS-COND-ICD-BIT  (GAF-INDEX).                           GC024015
145700                                                                  GC024015
145800     MOVE GAF-COND-TB-BIT (GAF-INDEX)                             GC024015
145900     TO   WS-COND-TB-BIT  (GAF-INDEX).                            GC024015
146000                                                                  GC024015
146100     MOVE GAF-COND-MENTAL-BIT (GAF-INDEX)                         GC024015
146200     TO   WS-COND-MENTAL-BIT  (GAF-INDEX).                        GC024015
146300                                                                  GC024015
146400     MOVE GAF-COND-DRUG-BIT (GAF-INDEX)                           GC024015
146500     TO   WS-COND-DRUG-BIT  (GAF-INDEX).                          GC024015
146600                                                                  GC024015
146700     MOVE GAF-COND-ALCOHOL-BIT (GAF-INDEX)                        GC024015
146800     TO   WS-COND-ALCOHOL-BIT  (GAF-INDEX).                       GC024015
146900                                                                  GC024015
147000     MOVE GAF-COND-OB-COMP-BIT (GAF-INDEX)                        GC024015
147100     TO   WS-COND-OB-COMP-BIT  (GAF-INDEX).                       GC024015
147200                                                                  GC024015
147300     MOVE GAF-COND-OB-NORM-BIT (GAF-INDEX)                        GC024015
147400     TO   WS-COND-OB-NORM-BIT  (GAF-INDEX).                       GC024015
147500                                                                  GC024015
147600     MOVE GAF-COND-MALIGNANCY-BIT (GAF-INDEX)                     GC024015
147700     TO   WS-COND-MALIGNANCY-BIT  (GAF-INDEX).                    GC024015
147800                                                                  GC024015
147900     MOVE GAF-COND-CARDIAC-DISEASE-BIT (GAF-INDEX)                GC024015
148000     TO   WS-COND-CARDIAC-DISEASE-BIT  (GAF-INDEX).               GC024015
148100                                                                  GC024015
148200     MOVE GAF-COND-OBESITY-BIT (GAF-INDEX)                        GC024015
148300     TO   WS-COND-OBESITY-BIT  (GAF-INDEX).                       GC024015
148400                                                                  GC024015
148500     MOVE GAF-COND-KIDNEY-DISEASE-BIT (GAF-INDEX)                 GC024015
148600     TO   WS-COND-KIDNEY-DISEASE-BIT  (GAF-INDEX).                GC024015
148700                                                                  GC024015
148800     MOVE GAF-COND-ACCIDENT-BIT (GAF-INDEX)                       GC024015
148900     TO   WS-COND-ACCIDENT-BIT  (GAF-INDEX).                      GC024015
149000                                                                  GC024015
149100     MOVE GAF-COND-PRE-EXIST-BIT (GAF-INDEX)                      GC024015
149200     TO   WS-COND-PRE-EXIST-BIT  (GAF-INDEX).                     GC024015
149300                                                                  GC024015
149400     MOVE GAF-COND-NON-EMER-BIT (GAF-INDEX)                       GC024015
149500     TO   WS-COND-NON-EMER-BIT  (GAF-INDEX).                      GC024015
149600                                                                  GC024015
149700     MOVE GAF-COND-SUICIDE-BIT (GAF-INDEX)                        GC024015
149800     TO   WS-COND-SUICIDE-BIT  (GAF-INDEX).                       GC024015
149900                                                                  GC024015
150000     MOVE GAF-COND-TMJ-BIT (GAF-INDEX)                            GC024015
150100     TO   WS-COND-TMJ-BIT  (GAF-INDEX).                           GC024015
150200                                                                  GC024015
150300     MOVE GAF-COND-INF-BIT (GAF-INDEX)                            GC024015
150400     TO   WS-COND-INF-BIT  (GAF-INDEX).                           GC024015
150500                                                                  GC024015
150600     MOVE GAF-COND-LIFE-THREAT-BIT (GAF-INDEX)                    GC024015
150700     TO   WS-COND-LIFE-THREAT-BIT  (GAF-INDEX).                   GC024015
150800                                                                  GC024015
150900     MOVE GAF-COND-EMER-MED-BIT (GAF-INDEX)                       GC024015
151000     TO   WS-COND-EMER-MED-BIT  (GAF-INDEX).                      GC024015
151100                                                                  GC024015
151200     MOVE GAF-COND-EMER-ACC-BIT (GAF-INDEX)                       GC024015
151300     TO   WS-COND-EMER-ACC-BIT  (GAF-INDEX).                      GC024015
151400                                                                  GC024015
151500     MOVE GAF-COND-SER-MEN-ILL-BIT (GAF-INDEX)                    GC024015
151600     TO   WS-COND-SER-MEN-ILL-BIT  (GAF-INDEX).                   GC024015
151700                                                                  GC024015
151800     MOVE GAF-COND-NON-SER-MEN-ILL-BIT (GAF-INDEX)                GC024015
151900     TO   WS-COND-NON-SER-MEN-ILL-BIT  (GAF-INDEX).               GC024015
152000                                                                  GC024015
152100     MOVE GAF-COND-FILLER-BIT (GAF-INDEX)                         GC024015
152200     TO   WS-COND-FILLER-BIT  (GAF-INDEX).                        GC024015
152300                                                                  GC024015
152400     MOVE GAF-COPAY-CO-PAY-IND (GAF-INDEX)                        GC024015
152500     TO   WS-COPAY-CO-PAY-IND  (GAF-INDEX).                       GC024015
152600                                                                  GC024015
152700     MOVE GAF-COPAY-COST-CONTAIN-IND (GAF-INDEX)                  GC024015
152800     TO   WS-COPAY-COST-CONTAIN-IND  (GAF-INDEX).                 GC024015
152900                                                                  GC024015
153000     MOVE GAF-COPAY-AGE-LIMIT-FROM (GAF-INDEX)                    GC024015
153100     TO   WS-COPAY-AGE-LIMIT-FROM  (GAF-INDEX).                   GC024015
153200                                                                  GC024015
153300     MOVE GAF-COPAY-AGE-QUAL-IND-FROM (GAF-INDEX)                 GC024015
153400     TO   WS-COPAY-AGE-QUAL-IND-FROM  (GAF-INDEX).                GC024015
153500                                                                  GC024015
153600     MOVE GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX)                   GC024015
153700     TO   WS-COPAY-VALUE-QUALIFIER  (GAF-INDEX).                  GC024015
153800                                                                  GC024015
153900     MOVE GAF-COPAY-FEAK-IND (GAF-INDEX)                          GC024015
154000     TO   WS-COPAY-FEAK-IND  (GAF-INDEX).                         GC024015
154100                                                                  GC024015
154200 2700-EXIT.                                                       GC024015
154300     EXIT.                                                        GC024015
154400/                                                                 GC024015
154500 2710-CDE-CHECK.                                                  GC024015
154600                                                                  GC024015
154700     IF  GAF-COPAY-MANDATORY-IND (GAF-INDEX) =                    GC024015
154800          WS-COPAY-MANDATORY-IND (GAF-INDEX)                      GC024015
154900         NEXT SENTENCE                                            GC024015
155000     ELSE                                                         GC024015
155100          ADD 1 TO DF-ERROR-COUNT                                 GC024015
155200          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
155300          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
155400          SET DF-ERROR-INDEX UP BY +1.                            GC024015
155500                                                                  GC024015
155600     IF  GAF-COPAY-BENEFIT-PERIOD (GAF-INDEX) =                   GC024015
155700          WS-COPAY-BENEFIT-PERIOD (GAF-INDEX)                     GC024015
155800         NEXT SENTENCE                                            GC024015
155900     ELSE                                                         GC024015
156000          ADD 1 TO DF-ERROR-COUNT                                 GC024015
156100          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
156200          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
156300          SET DF-ERROR-INDEX UP BY +1.                            GC024015
156400                                                                  GC024015
156500     IF  GAF-COPAY-FAM-OR-INDIV (GAF-INDEX) =                     GC024015
156600          WS-COPAY-FAM-OR-INDIV (GAF-INDEX)                       GC024015
156700         NEXT SENTENCE                                            GC024015
156800     ELSE                                                         GC024015
156900          ADD 1 TO DF-ERROR-COUNT                                 GC024015
157000          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
157100          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
157200          SET DF-ERROR-INDEX UP BY +1.                            GC024015
157300                                                                  GC024015
157400     IF  GAF-COPAY-L-O-B (GAF-INDEX) =                            GC024015
157500          WS-COPAY-L-O-B (GAF-INDEX)                              GC024015
157600         NEXT SENTENCE                                            GC024015
157700     ELSE                                                         GC024015
157800          ADD 1 TO DF-ERROR-COUNT                                 GC024015
157900          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
158000          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
158100          SET DF-ERROR-INDEX UP BY +1.                            GC024015
158200                                                                  GC024015
158300     IF  GAF-COPAY-INTERNAL-DESCRIPTOR (GAF-INDEX) =              GC024015
158400          WS-COPAY-INTERNAL-DESCRIPTOR (GAF-INDEX)                GC024015
158500         NEXT SENTENCE                                            GC024015
158600     ELSE                                                         GC024015
158700          ADD 1 TO DF-ERROR-COUNT                                 GC024015
158800          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
158900          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
159000          SET DF-ERROR-INDEX UP BY +1.                            GC024015
159100                                                                  GC024015
159200     IF  GAF-COPAY-SERVICE-GROUP (GAF-INDEX) =                    GC024015
159300          WS-COPAY-SERVICE-GROUP (GAF-INDEX)                      GC024015
159400         NEXT SENTENCE                                            GC024015
159500     ELSE                                                         GC024015
159600          ADD 1 TO DF-ERROR-COUNT                                 GC024015
159700          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
159800          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
159900          SET DF-ERROR-INDEX UP BY +1.                            GC024015
160000                                                                  GC024015
160100     IF  GAF-COPAY-PLACE-OF-TREATMENT (GAF-INDEX) =               GC024015
160200          WS-COPAY-PLACE-OF-TREATMENT (GAF-INDEX)                 GC024015
160300         NEXT SENTENCE                                            GC024015
160400     ELSE                                                         GC024015
160500          ADD 1 TO DF-ERROR-COUNT                                 GC024015
160600          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
160700          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
160800          SET DF-ERROR-INDEX UP BY +1.                            GC024015
160900                                                                  GC024015
161000     IF  GAF-COPAY-CONDITION (GAF-INDEX) =                        GC024015
161100          WS-COPAY-CONDITION (GAF-INDEX)                          GC024015
161200         NEXT SENTENCE                                            GC024015
161300     ELSE                                                         GC024015
161400          ADD 1 TO DF-ERROR-COUNT                                 GC024015
161500          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
161600          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
161700          SET DF-ERROR-INDEX UP BY +1.                            GC024015
161800                                                                  GC024015
161900     IF  GAF-COND-ALL-BIT (GAF-INDEX) =                           GC024015
162000          WS-COND-ALL-BIT (GAF-INDEX)                             GC024015
162100         NEXT SENTENCE                                            GC024015
162200     ELSE                                                         GC024015
162300          ADD 1 TO DF-ERROR-COUNT                                 GC024015
162400          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
162500          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
162600          SET DF-ERROR-INDEX UP BY +1.                            GC024015
162700                                                                  GC024015
162800     IF  GAF-COND-EXCLUSION-BIT (GAF-INDEX) =                     GC024015
162900          WS-COND-EXCLUSION-BIT (GAF-INDEX)                       GC024015
163000         NEXT SENTENCE                                            GC024015
163100     ELSE                                                         GC024015
163200          ADD 1 TO DF-ERROR-COUNT                                 GC024015
163300          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
163400          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
163500          SET DF-ERROR-INDEX UP BY +1.                            GC024015
163600                                                                  GC024015
163700     IF  GAF-COND-ICD-BIT (GAF-INDEX) =                           GC024015
163800          WS-COND-ICD-BIT (GAF-INDEX)                             GC024015
163900         NEXT SENTENCE                                            GC024015
164000     ELSE                                                         GC024015
164100          ADD 1 TO DF-ERROR-COUNT                                 GC024015
164200          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
164300          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
164400          SET DF-ERROR-INDEX UP BY +1.                            GC024015
164500                                                                  GC024015
164600     IF  GAF-COND-TB-BIT (GAF-INDEX) =                            GC024015
164700          WS-COND-TB-BIT (GAF-INDEX)                              GC024015
164800         NEXT SENTENCE                                            GC024015
164900     ELSE                                                         GC024015
165000          ADD 1 TO DF-ERROR-COUNT                                 GC024015
165100          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
165200          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
165300          SET DF-ERROR-INDEX UP BY +1.                            GC024015
165400                                                                  GC024015
165500     IF  GAF-COND-MENTAL-BIT (GAF-INDEX) =                        GC024015
165600          WS-COND-MENTAL-BIT (GAF-INDEX)                          GC024015
165700         NEXT SENTENCE                                            GC024015
165800     ELSE                                                         GC024015
165900          ADD 1 TO DF-ERROR-COUNT                                 GC024015
166000          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
166100          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
166200          SET DF-ERROR-INDEX UP BY +1.                            GC024015
166300                                                                  GC024015
166400     IF  GAF-COND-DRUG-BIT (GAF-INDEX) =                          GC024015
166500          WS-COND-DRUG-BIT (GAF-INDEX)                            GC024015
166600         NEXT SENTENCE                                            GC024015
166700     ELSE                                                         GC024015
166800          ADD 1 TO DF-ERROR-COUNT                                 GC024015
166900          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
167000          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
167100          SET DF-ERROR-INDEX UP BY +1.                            GC024015
167200                                                                  GC024015
167300     IF  GAF-COND-ALCOHOL-BIT (GAF-INDEX) =                       GC024015
167400          WS-COND-ALCOHOL-BIT (GAF-INDEX)                         GC024015
167500         NEXT SENTENCE                                            GC024015
167600     ELSE                                                         GC024015
167700          ADD 1 TO DF-ERROR-COUNT                                 GC024015
167800          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
167900          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
168000          SET DF-ERROR-INDEX UP BY +1.                            GC024015
168100                                                                  GC024015
168200     IF  GAF-COND-OB-COMP-BIT (GAF-INDEX) =                       GC024015
168300          WS-COND-OB-COMP-BIT (GAF-INDEX)                         GC024015
168400         NEXT SENTENCE                                            GC024015
168500     ELSE                                                         GC024015
168600          ADD 1 TO DF-ERROR-COUNT                                 GC024015
168700          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
168800          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
168900          SET DF-ERROR-INDEX UP BY +1.                            GC024015
169000                                                                  GC024015
169100     IF  GAF-COND-OB-NORM-BIT (GAF-INDEX) =                       GC024015
169200          WS-COND-OB-NORM-BIT (GAF-INDEX)                         GC024015
169300         NEXT SENTENCE                                            GC024015
169400     ELSE                                                         GC024015
169500          ADD 1 TO DF-ERROR-COUNT                                 GC024015
169600          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
169700          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
169800          SET DF-ERROR-INDEX UP BY +1.                            GC024015
169900                                                                  GC024015
170000     IF  GAF-COND-MALIGNANCY-BIT (GAF-INDEX) =                    GC024015
170100          WS-COND-MALIGNANCY-BIT (GAF-INDEX)                      GC024015
170200         NEXT SENTENCE                                            GC024015
170300     ELSE                                                         GC024015
170400          ADD 1 TO DF-ERROR-COUNT                                 GC024015
170500          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
170600          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
170700          SET DF-ERROR-INDEX UP BY +1.                            GC024015
170800                                                                  GC024015
170900     IF  GAF-COND-CARDIAC-DISEASE-BIT (GAF-INDEX) =               GC024015
171000          WS-COND-CARDIAC-DISEASE-BIT (GAF-INDEX)                 GC024015
171100         NEXT SENTENCE                                            GC024015
171200     ELSE                                                         GC024015
171300          ADD 1 TO DF-ERROR-COUNT                                 GC024015
171400          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
171500          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
171600          SET DF-ERROR-INDEX UP BY +1.                            GC024015
171700                                                                  GC024015
171800     IF  GAF-COND-OBESITY-BIT (GAF-INDEX) =                       GC024015
171900          WS-COND-OBESITY-BIT (GAF-INDEX)                         GC024015
172000         NEXT SENTENCE                                            GC024015
172100     ELSE                                                         GC024015
172200          ADD 1 TO DF-ERROR-COUNT                                 GC024015
172300          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
172400          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
172500          SET DF-ERROR-INDEX UP BY +1.                            GC024015
172600                                                                  GC024015
172700     IF  GAF-COND-KIDNEY-DISEASE-BIT (GAF-INDEX) =                GC024015
172800          WS-COND-KIDNEY-DISEASE-BIT (GAF-INDEX)                  GC024015
172900         NEXT SENTENCE                                            GC024015
173000     ELSE                                                         GC024015
173100          ADD 1 TO DF-ERROR-COUNT                                 GC024015
173200          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
173300          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
173400          SET DF-ERROR-INDEX UP BY +1.                            GC024015
173500                                                                  GC024015
173600     IF  GAF-COND-ACCIDENT-BIT (GAF-INDEX) =                      GC024015
173700          WS-COND-ACCIDENT-BIT (GAF-INDEX)                        GC024015
173800         NEXT SENTENCE                                            GC024015
173900     ELSE                                                         GC024015
174000          ADD 1 TO DF-ERROR-COUNT                                 GC024015
174100          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
174200          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
174300          SET DF-ERROR-INDEX UP BY +1.                            GC024015
174400                                                                  GC024015
174500     IF  GAF-COND-PRE-EXIST-BIT (GAF-INDEX) =                     GC024015
174600          WS-COND-PRE-EXIST-BIT (GAF-INDEX)                       GC024015
174700         NEXT SENTENCE                                            GC024015
174800     ELSE                                                         GC024015
174900          ADD 1 TO DF-ERROR-COUNT                                 GC024015
175000          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
175100          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
175200          SET DF-ERROR-INDEX UP BY +1.                            GC024015
175300                                                                  GC024015
175400     IF  GAF-COND-NON-EMER-BIT (GAF-INDEX) =                      GC024015
175500          WS-COND-NON-EMER-BIT (GAF-INDEX)                        GC024015
175600         NEXT SENTENCE                                            GC024015
175700     ELSE                                                         GC024015
175800          ADD 1 TO DF-ERROR-COUNT                                 GC024015
175900          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
176000          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
176100          SET DF-ERROR-INDEX UP BY +1.                            GC024015
176200                                                                  GC024015
176300     IF  GAF-COND-SUICIDE-BIT (GAF-INDEX) =                       GC024015
176400          WS-COND-SUICIDE-BIT (GAF-INDEX)                         GC024015
176500         NEXT SENTENCE                                            GC024015
176600     ELSE                                                         GC024015
176700          ADD 1 TO DF-ERROR-COUNT                                 GC024015
176800          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
176900          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
177000          SET DF-ERROR-INDEX UP BY +1.                            GC024015
177100                                                                  GC024015
177200     IF  GAF-COND-TMJ-BIT (GAF-INDEX) =                           GC024015
177300          WS-COND-TMJ-BIT (GAF-INDEX)                             GC024015
177400         NEXT SENTENCE                                            GC024015
177500     ELSE                                                         GC024015
177600          ADD 1 TO DF-ERROR-COUNT                                 GC024015
177700          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
177800          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
177900          SET DF-ERROR-INDEX UP BY +1.                            GC024015
178000                                                                  GC024015
178100     IF  GAF-COND-INF-BIT (GAF-INDEX) =                           GC024015
178200          WS-COND-INF-BIT (GAF-INDEX)                             GC024015
178300         NEXT SENTENCE                                            GC024015
178400     ELSE                                                         GC024015
178500          ADD 1 TO DF-ERROR-COUNT                                 GC024015
178600          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
178700          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
178800          SET DF-ERROR-INDEX UP BY +1.                            GC024015
178900                                                                  GC024015
179000     IF  GAF-COND-LIFE-THREAT-BIT (GAF-INDEX) =                   GC024015
179100          WS-COND-LIFE-THREAT-BIT (GAF-INDEX)                     GC024015
179200         NEXT SENTENCE                                            GC024015
179300     ELSE                                                         GC024015
179400          ADD 1 TO DF-ERROR-COUNT                                 GC024015
179500          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
179600          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
179700          SET DF-ERROR-INDEX UP BY +1.                            GC024015
179800                                                                  GC024015
179900     IF  GAF-COND-EMER-MED-BIT (GAF-INDEX) =                      GC024015
180000          WS-COND-EMER-MED-BIT (GAF-INDEX)                        GC024015
180100         NEXT SENTENCE                                            GC024015
180200     ELSE                                                         GC024015
180300          ADD 1 TO DF-ERROR-COUNT                                 GC024015
180400          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
180500          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
180600          SET DF-ERROR-INDEX UP BY +1.                            GC024015
180700                                                                  GC024015
180800     IF  GAF-COND-EMER-ACC-BIT (GAF-INDEX) =                      GC024015
180900          WS-COND-EMER-ACC-BIT (GAF-INDEX)                        GC024015
181000         NEXT SENTENCE                                            GC024015
181100     ELSE                                                         GC024015
181200          ADD 1 TO DF-ERROR-COUNT                                 GC024015
181300          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
181400          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
181500          SET DF-ERROR-INDEX UP BY +1.                            GC024015
181600                                                                  GC024015
181700     IF  GAF-COND-SER-MEN-ILL-BIT (GAF-INDEX) =                   GC024015
181800          WS-COND-SER-MEN-ILL-BIT (GAF-INDEX)                     GC024015
181900         NEXT SENTENCE                                            GC024015
182000     ELSE                                                         GC024015
182100          ADD 1 TO DF-ERROR-COUNT                                 GC024015
182200          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
182300          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
182400          SET DF-ERROR-INDEX UP BY +1.                            GC024015
182500                                                                  GC024015
182600     IF  GAF-COND-NON-SER-MEN-ILL-BIT (GAF-INDEX) =               GC024015
182700          WS-COND-NON-SER-MEN-ILL-BIT (GAF-INDEX)                 GC024015
182800         NEXT SENTENCE                                            GC024015
182900     ELSE                                                         GC024015
183000          ADD 1 TO DF-ERROR-COUNT                                 GC024015
183100          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
183200          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
183300          SET DF-ERROR-INDEX UP BY +1.                            GC024015
183400                                                                  GC024015
183500     IF  GAF-COND-FILLER-BIT (GAF-INDEX) =                        GC024015
183600          WS-COND-FILLER-BIT (GAF-INDEX)                          GC024015
183700         NEXT SENTENCE                                            GC024015
183800     ELSE                                                         GC024015
183900          ADD 1 TO DF-ERROR-COUNT                                 GC024015
184000          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
184100          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
184200          SET DF-ERROR-INDEX UP BY +1.                            GC024015
184300                                                                  GC024015
184400     IF  GAF-COPAY-CO-PAY-IND (GAF-INDEX) =                       GC024015
184500          WS-COPAY-CO-PAY-IND (GAF-INDEX)                         GC024015
184600         NEXT SENTENCE                                            GC024015
184700     ELSE                                                         GC024015
184800          ADD 1 TO DF-ERROR-COUNT                                 GC024015
184900          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
185000          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
185100          SET DF-ERROR-INDEX UP BY +1.                            GC024015
185200                                                                  GC024015
185300     IF  GAF-COPAY-COST-CONTAIN-IND (GAF-INDEX) =                 GC024015
185400          WS-COPAY-COST-CONTAIN-IND (GAF-INDEX)                   GC024015
185500         NEXT SENTENCE                                            GC024015
185600     ELSE                                                         GC024015
185700          ADD 1 TO DF-ERROR-COUNT                                 GC024015
185800          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
185900          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
186000          SET DF-ERROR-INDEX UP BY +1.                            GC024015
186100                                                                  GC024015
186200     IF  GAF-COPAY-AGE-LIMIT-FROM (GAF-INDEX) =                   GC024015
186300          WS-COPAY-AGE-LIMIT-FROM (GAF-INDEX)                     GC024015
186400         NEXT SENTENCE                                            GC024015
186500     ELSE                                                         GC024015
186600          ADD 1 TO DF-ERROR-COUNT                                 GC024015
186700          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
186800          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
186900          SET DF-ERROR-INDEX UP BY +1.                            GC024015
187000                                                                  GC024015
187100     IF  GAF-COPAY-AGE-QUAL-IND-FROM (GAF-INDEX) =                GC024015
187200          WS-COPAY-AGE-QUAL-IND-FROM (GAF-INDEX)                  GC024015
187300         NEXT SENTENCE                                            GC024015
187400     ELSE                                                         GC024015
187500          ADD 1 TO DF-ERROR-COUNT                                 GC024015
187600          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
187700          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
187800          SET DF-ERROR-INDEX UP BY +1.                            GC024015
187900                                                                  GC024015
188000     IF  GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) =                  GC024015
188100          WS-COPAY-VALUE-QUALIFIER (GAF-INDEX)                    GC024015
188200         NEXT SENTENCE                                            GC024015
188300     ELSE                                                         GC024015
188400          ADD 1 TO DF-ERROR-COUNT                                 GC024015
188500          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
188600          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
188700          SET DF-ERROR-INDEX UP BY +1.                            GC024015
188800                                                                  GC024015
188900     IF  GAF-COPAY-FEAK-IND (GAF-INDEX) =                         GC024015
189000          WS-COPAY-FEAK-IND (GAF-INDEX)                           GC024015
189100         NEXT SENTENCE                                            GC024015
189200     ELSE                                                         GC024015
189300          ADD 1 TO DF-ERROR-COUNT                                 GC024015
189400          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
189500          MOVE 'A21' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
189600          SET DF-ERROR-INDEX UP BY +1.                            GC024015
189700                                                                  GC024015
189800 2710-EXIT.                                                       GC024015
189900     EXIT.                                                        GC024015
190000/                                                                 GC024015
190100 2800-LOAD-DEFINITION.                                            GC024015
190200                                                                  GC024015
190300     IF GAF-COPAY-DEFINITION  (GAF-INDEX)  =  '0A'                GC024015
190400        MOVE 'Y'  TO  WS-DEF-0A                                   GC024015
190500        MOVE GAF-COPAY-DEFINITION      (GAF-INDEX)                GC024015
190600        TO   WS-DEF-1 (GAF-INDEX)                                 GC024015
190700        GO TO 2800-EXIT.                                          GC024015
190800                                                                  GC024015
190900     IF GAF-COPAY-DEFINITION  (GAF-INDEX)  =  'AA'                GC024015
191000        MOVE 'Y'  TO  WS-DEF-AA                                   GC024015
191100        MOVE GAF-COPAY-DEFINITION      (GAF-INDEX)                GC024015
191200        TO   WS-DEF-2 (GAF-INDEX)                                 GC024015
191300     ELSE                                                         GC024015
191400        IF 0A                                                     GC024015
191500           ADD 1 TO DF-ERROR-COUNT                                GC024015
191600           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                  GC024015
191700           MOVE 'A22' TO DF-ERR-CODE (DF-ERROR-INDEX)             GC024015
191800           SET DF-ERROR-INDEX UP BY +1                            GC024015
191900           GO TO 2800-EXIT.                                       GC024015
192000                                                                  GC024015
192100     IF GAF-COPAY-DEFINITION  (GAF-INDEX)  =  '0B'                GC024015
192200        MOVE 'Y'  TO  WS-DEF-0B                                   GC024015
192300        MOVE GAF-COPAY-DEFINITION      (GAF-INDEX)                GC024015
192400        TO   WS-DEF-1 (GAF-INDEX)                                 GC024015
192500        GO TO 2800-EXIT.                                          GC024015
192600                                                                  GC024015
192700     IF GAF-COPAY-DEFINITION  (GAF-INDEX)  =  'BB'                GC024015
192800        MOVE 'Y'  TO  WS-DEF-BB                                   GC024015
192900        MOVE GAF-COPAY-DEFINITION      (GAF-INDEX)                GC024015
193000        TO   WS-DEF-2 (GAF-INDEX)                                 GC024015
193100     ELSE                                                         GC024015
193200        IF 0B                                                     GC024015
193300           ADD 1 TO DF-ERROR-COUNT                                GC024015
193400           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                  GC024015
193500           MOVE 'A23' TO DF-ERR-CODE (DF-ERROR-INDEX)             GC024015
193600           SET DF-ERROR-INDEX UP BY +1                            GC024015
193700           GO TO 2800-EXIT.                                       GC024015
193800                                                                  GC024015
193900     IF GAF-COPAY-DEFINITION  (GAF-INDEX)  =  '0F'                GC024015
194000        MOVE 'Y'  TO  WS-DEF-0F                                   GC024015
194100        MOVE GAF-COPAY-DEFINITION      (GAF-INDEX)                GC024015
194200        TO   WS-DEF-1 (GAF-INDEX)                                 GC024015
194300        GO TO 2800-EXIT.                                          GC024015
194400                                                                  GC024015
194500     IF GAF-COPAY-DEFINITION  (GAF-INDEX)  =  'FF'                GC024015
194600        MOVE 'Y'  TO  WS-DEF-FF                                   GC024015
194700        MOVE GAF-COPAY-DEFINITION      (GAF-INDEX)                GC024015
194800        TO   WS-DEF-2 (GAF-INDEX)                                 GC024015
194900     ELSE                                                         GC024015
195000        IF 0F                                                     GC024015
195100           ADD 1 TO DF-ERROR-COUNT                                GC024015
195200           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                  GC024015
195300           MOVE 'A24' TO DF-ERR-CODE (DF-ERROR-INDEX)             GC024015
195400           SET DF-ERROR-INDEX UP BY +1                            GC024015
195500           GO TO 2800-EXIT.                                       GC024015
195600                                                                  GC024015
195700*    IF GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)  >  SPACES  AND     GC024015
195800     IF GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)  > ZEROES   AND     GC024015
195900        ( 0A OR AA OR 0B OR BB OR 0F OR FF )                      GC024015
196000        ADD 1 TO DF-ERROR-COUNT                                   GC024015
196100        MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                     GC024015
196200        MOVE 'A25' TO DF-ERR-CODE (DF-ERROR-INDEX)                GC024015
196300        SET DF-ERROR-INDEX UP BY +1                               GC024015
196400        GO TO 2800-EXIT.                                          GC024015
196500                                                                  GC024015
196600 2800-EXIT.                                                       GC024015
196700     EXIT.                                                        GC024015
196800/                                                                 GC024015
196900 2810-DEFINITION-CHECK.                                           GC024015
197000                                                                  GC024015
197100     IF 0A                                                        GC024015
197200        IF GAF-ENTRY-COUNT  >  +3                                 GC024015
197300           IF NOT AA                                              GC024015
197400              ADD 1 TO DF-ERROR-COUNT                             GC024015
197500              MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT               GC024015
197600              MOVE 'A22' TO DF-ERR-CODE (DF-ERROR-INDEX)          GC024015
197700              SET DF-ERROR-INDEX UP BY +1.                        GC024015
197800                                                                  GC024015
197900     IF 0B                                                        GC024015
198000        IF GAF-ENTRY-COUNT  >  +3                                 GC024015
198100           IF NOT BB                                              GC024015
198200              ADD 1 TO DF-ERROR-COUNT                             GC024015
198300              MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT               GC024015
198400              MOVE 'A23' TO DF-ERR-CODE (DF-ERROR-INDEX)          GC024015
198500              SET DF-ERROR-INDEX UP BY +1.                        GC024015
198600                                                                  GC024015
198700     IF 0F                                                        GC024015
198800        IF GAF-ENTRY-COUNT  >  +3                                 GC024015
198900           IF NOT FF                                              GC024015
199000              ADD 1 TO DF-ERROR-COUNT                             GC024015
199100              MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT               GC024015
199200              MOVE 'A24' TO DF-ERR-CODE (DF-ERROR-INDEX)          GC024015
199300              SET DF-ERROR-INDEX UP BY +1.                        GC024015
199400                                                                  GC024015
199500 2810-EXIT.                                                       GC024015
199600     EXIT.                                                        GC024015
199700/                                                                 GC024015
199800 2900-CHECK-BEN-PRD.                                              GC024015
199900                                                                  GC024015
200000     MOVE GAF-COPAY-BENEFIT-PERIOD (GAF-INDEX)                    GC024015
200100     TO   WS-BEN-PRD.                                             GC024015
200200                                                                  GC024015
200300     IF   SELECTED-VALUE                                          GC024015
200400          NEXT SENTENCE                                           GC024015
200500     ELSE                                                         GC024015
200600          ADD 1 TO DF-ERROR-COUNT                                 GC024015
200700          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
200800          MOVE 'A26' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
200900          SET DF-ERROR-INDEX UP BY +1.                            GC024015
201000                                                                  GC024015
201100 2900-EXIT.                                                       GC024015
201200     EXIT.                                                        GC024015
201300/                                                                 GC024015
201400***************************************************************** GC024015
201500 3000-ADL-EDITS.                                                  GC024015
201600*                                                                 GC024015
201700     PERFORM 3010-000-ADL-EDIT1 THRU 3010-000-EXIT.               GC024015
201800*--- START OF ISSR 11154                                          GC024015
201900*--- EDIT ALL OCCURRENCES FOR AGE LIMITS AND INTERNAL DESCRIPTOR  GC024015
202000                                                                  GC024015
202100*>>>> COMMENT OUT THE AGE LIMIT ROUTINES PER ED WITKUS 03/21/91   GC024015
202200                                                                  GC024015
202300     COMPUTE WS-OCCURS-ENTRIES = GAC-ENTRY-COUNT - 1.             GC024015
202400                                                                  GC024015
202500     IF WS-OCCURS-ENTRIES     <   +2                              GC024015
202600        GO TO   3000-EXIT.                                        GC024015
202700                                                                  GC024015
202800     SET  WS-A-INDEX  TO +1.                                      GC024015
202900     PERFORM   3150-LOAD-AGE-LIMITS-ADL THRU 3150-EXIT            GC024015
203000         VARYING  GAC-INDEX  FROM +1  BY +1                       GC024015
203100           UNTIL  GAC-INDEX  >  WS-OCCURS-ENTRIES.                GC024015
203200                                                                  GC024015
203300     MOVE 'Y'   TO  WS-SORT-EXCHANGE-IND.                         GC024015
203400     PERFORM 7000-OCCURANCE-SORT THRU 7000-EXIT                   GC024015
203500        UNTIL WS-NO-EXCHANGE-MADE.                                GC024015
203600                                                                  GC024015
203700     SET  WS-A-INDEX  WS-B-INDEX  TO +1.                          GC024015
203800     PERFORM   3160-EDIT-AGE-LIMITS-ADL THRU 3160-EXIT  UNTIL     GC024015
203900        WS-INT-DESC (WS-A-INDEX)  =  HIGH-VALUES   OR             GC024015
204000           WS-A-INDEX   >  WS-OCCURS-ENTRIES.                     GC024015
204100                                                                  GC024015
204200*--- END OF 11154                                                 GC024015
204300*                                                                 GC024015
204400 3000-EXIT.                                                       GC024015
204500     EXIT.                                                        GC024015
204600***************************************************************** GC024015
204700*     P09400 - 11/08/06 ASC/DESC EDIT FOR #ADL                    GC024015
204800*                                                                         
204900*     FOR ALL OCCURS, UNLESS AN ERROR CONDITION IS MET,           GC024015
205000*     IF THE ASCEND/DESCEND IND. = '1' OR '2' OR '3'              GC024015
205100*         CHECK THE OTHER OCCURS FOR AN ASCEND/DESCEND IND WITH   GC024015
205200*         THE SAME VALUE (THERE MAY BE SEVERAL OCCURS)            GC024015
205300*         IF NONE ARE FOUND,                                      GC024015
205400*             ASSIGN AN ERROR CODE                                GC024015
205500*         ELSE                                                    GC024015
205600*             COMPARE THE FIELDS FROM THE ONE OCCURS WITH THE     GC024015
205700*             FIELDS FROM THE OTHER OCCURS (SEE CODE FOR THE      GC024015
205800*             SPECIFIC FIELD NAMES)                               GC024015
205900*             IF THE VALUES IN THE FIELDS CHECKED ARE ALL EQUAL   GC024015
206000*             TO EACH OTHER,                                      GC024015
206100*                 EXIT                                            GC024015
206200*             ELSE                                                GC024015
206300*                 ASSIGN AN ERROR CODE                            GC024015
206400*                 EXIT.                                           GC024015
206500***************************************************************** GC024015
206600 3010-000-ADL-EDIT1.                                              GC024015
206700                                                                  GC024015
206800     IF GAC-ENTRY-COUNT < 3                                       GC024015
206900         GO TO 3010-000-EXIT.                                     GC024015
207000     MOVE 'N' TO WS-FINISH-SW.                                    GC024015
207100     MOVE LOW-VALUES TO WS-TABULAR-REC-AREA.                      GC024015
207200     MOVE LS-TABULAR-REC-AREA TO WS-TABULAR-REC-AREA.             GC024015
207300     SET GAC-INDEX  TO +1.                                        GC024015
207400     PERFORM 3010-010-LOOP1 THRU 3010-010-EXIT                    GC024015
207500       VARYING GAC-INDEX FROM 1 BY 1                              GC024015
207600       UNTIL   GAC-INDEX = GAC-ENTRY-COUNT                        GC024015
207700          OR   WS-FINISH-SW = 'Y'.                                GC024015
207800                                                                  GC024015
207900 3010-000-EXIT.                                                   GC024015
208000     EXIT.                                                        GC024015
208100                                                                  GC024015
208200***************************************************************** GC024015
208300 3010-010-LOOP1.                                                  GC024015
208400     IF GAC-DEDL-ASCEND-DESCEND-IND (GAC-INDEX) =  '1' OR         GC024015
208500                                                   '2' OR '3'     GC024015
208600         SET GAC2-INDEX TO 1                                      GC024015
208700         MOVE 'N' TO WS-MATCH1-SW                                 GC024015
208800                     WS-MATCH2-SW                                 GC024015
208900         PERFORM 3010-020-LOOP2 THRU 3010-020-EXIT                GC024015
209000           VARYING GAC2-INDEX FROM 1 BY 1                         GC024015
209100           UNTIL   GAC2-INDEX = GAC2-ENTRY-COUNT                  GC024015
209200     ELSE                                                         GC024015
209300         GO TO 3010-010-EXIT.                                     GC024015
209400     IF WS-MATCH1-SW = 'N'                                        GC024015
209500         ADD 1 TO DF-ERROR-COUNT                                  GC024015
209600         MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                    GC024015
209700         MOVE 'A35' TO DF-ERR-CODE (DF-ERROR-INDEX)               GC024015
209800         SET DF-ERROR-INDEX UP BY 1                               GC024015
209900         MOVE 'Y' TO WS-FINISH-SW                                 GC024015
210000         GO TO  3010-010-EXIT.                                    GC024015
210100     IF WS-MATCH2-SW = 'N'                                        GC024015
210200         ADD 1 TO DF-ERROR-COUNT                                  GC024015
210300         MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                    GC024015
210400         MOVE 'A36' TO DF-ERR-CODE (DF-ERROR-INDEX)               GC024015
210500         SET DF-ERROR-INDEX UP BY 1                               GC024015
210600         MOVE 'Y' TO WS-FINISH-SW                                 GC024015
210700         GO TO  3010-010-EXIT.                                    GC024015
210800                                                                  GC024015
210900 3010-010-EXIT.                                                   GC024015
211000     EXIT.                                                        GC024015
211100                                                                  GC024015
211200***************************************************************** GC024015
211300 3010-020-LOOP2.                                                  GC024015
211400                                                                  GC024015
211500     IF GAC2-INDEX = GAC-INDEX                                    GC024015
211600         GO TO 3010-020-EXIT.                                     GC024015
211700     IF GAC-DEDL-ASCEND-DESCEND-IND (GAC-INDEX)                   GC024015
211800       =                                                          GC024015
211900        GAC2-DEDL-ASCEND-DESCEND-IND (GAC2-INDEX)                 GC024015
212000         MOVE 'Y' TO WS-MATCH1-SW                                 GC024015
212100     ELSE                                                         GC024015
212200         GO TO 3010-020-EXIT.                                     GC024015
212300*******                                                           GC024015
212400* THE CONDITION BIT FILLER BIT IS PRIMED JUST IN CASE A MUTANT    GC024015
212500* VALUE EVER ENDS UP THERE.                                       GC024015
212600*                                                                 GC024015
212700     MOVE GAC-COND-FILLER-BIT (GAC-INDEX)                         GC024015
212800       TO GAC2-COND-FILLER-BIT (GAC2-INDEX).                      GC024015
212900*                                                                 GC024015
213000     IF ((GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX)                     GC024015
213100       = GAC2-DEDL-BENEFIT-PERIOD (GAC2-INDEX))                   GC024015
213200       AND                                                        GC024015
213300        (GAC-DEDL-L-O-B (GAC-INDEX)                               GC024015
213400       = GAC2-DEDL-L-O-B (GAC2-INDEX))                            GC024015
213500       AND                                                        GC024015
213600        (GAC-DEDL-FAM-OR-INDIV (GAC-INDEX)                        GC024015
213700       = GAC2-DEDL-FAM-OR-INDIV (GAC2-INDEX))                     GC024015
213800       AND                                                        GC024015
213900        (GAC-DEDL-COST-CONTAIN-IND (GAC-INDEX)                    GC024015
214000       = GAC2-DEDL-COST-CONTAIN-IND (GAC2-INDEX))                 GC024015
214100       AND                                                        GC024015
214200        (GAC-DEDL-PLACE-OF-TREATMENT (GAC-INDEX)                  GC024015
214300       = GAC2-DEDL-PLACE-OF-TREATMENT (GAC2-INDEX))               GC024015
214400       AND                                                        GC024015
214500        (GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)                     GC024015
214600       = GAC2-DEDL-VALUE-QUALIFIER (GAC2-INDEX))                  GC024015
214700       AND                                                        GC024015
214800        (GAC-DEDL-MANDATORY-IND (GAC-INDEX)                       GC024015
214900       = GAC2-DEDL-MANDATORY-IND (GAC2-INDEX))                    GC024015
215000       AND                                                        GC024015
215100        (GAC-DEDL-SERVICE-GROUP (GAC-INDEX)                       GC024015
215200       = GAC2-DEDL-SERVICE-GROUP (GAC2-INDEX))                    GC024015
215300       AND                                                        GC024015
215400        (GAC-DEDL-CO-PAY-IND (GAC-INDEX)                          GC024015
215500       = GAC2-DEDL-CO-PAY-IND (GAC2-INDEX))                       GC024015
215600       AND                                                        GC024015
215700        (GAC-DEDL-CONDITION (GAC-INDEX)                           GC024015
215800       = GAC2-DEDL-CONDITION (GAC2-INDEX))                        GC024015
215900       AND                                                        GC024015
216000        (GAC-DEDL-INTERNAL-DESCRIPTOR (GAC-INDEX)                 GC024015
216100       = GAC2-DEDL-INTERNAL-DESCRIPTOR (GAC2-INDEX))              GC024015
216200       AND                                                        GC024015
216300        (GAC-DEDL-AGE-QUAL-IND-FROM (GAC-INDEX)                   GC024015
216400       = GAC2-DEDL-AGE-QUAL-IND-FROM (GAC2-INDEX))                GC024015
216500       AND                                                        GC024015
216600         (GAC-DEDL-AGE-QUAL-IND-TO (GAC-INDEX)                    GC024015
216700       =  GAC2-DEDL-AGE-QUAL-IND-TO (GAC2-INDEX))                 GC024015
216800       AND                                                        GC024015
216900         (GAC-DEDL-AGE-LIMIT-FROM (GAC-INDEX)                     GC024015
217000       =  GAC2-DEDL-AGE-LIMIT-FROM (GAC2-INDEX))                  GC024015
217100       AND                                                        GC024015
217200         (GAC-DEDL-AGE-QUAL-IND-TO (GAC-INDEX)                    GC024015
217300       =  GAC2-DEDL-AGE-QUAL-IND-TO (GAC2-INDEX)))                GC024015
217400         MOVE 'Y' TO WS-MATCH2-SW.                                GC024015
217500                                                                  GC024015
217600                                                                  GC024015
217700 3010-020-EXIT.                                                   GC024015
217800     EXIT.                                                        GC024015
217900                                                                  GC024015
218000****************************************************************  GC024015
218100 3150-LOAD-AGE-LIMITS-ADL.                                        GC024015
218200                                                                  GC024015
218300*--- INTERNAL DESCRIPTOR IS SPACES AND AGE-LIMITS IS ZEROS, SKIP. GC024015
218400     IF GAC-DEDL-INTERNAL-DESCRIPTOR (GAC-INDEX) = SPACES AND     GC024015
218500        GAC-DEDL-AGE-LIMIT-FROM (GAC-INDEX)      = ZEROS   AND    GC024015
218600        GAC-DEDL-AGE-LIMIT-TO   (GAC-INDEX)      = ZEROS          GC024015
218700        GO TO   3150-EXIT.                                        GC024015
218800                                                                  GC024015
218900*--- IF AGE LIMIT FIELDS ARE  N O T  CODED THEN INTERNAL          GC024015
219000*--- DESCRIPTOR HAS NO EFFECT ON THE LOGIC, U N L E S S  THE SAME GC024015
219100*--- DESCRIPTOR IS ON DIFFERENT OCCURS AND THE AGE-LIMITS ARE     GC024015
219200*--- CODED, THIS WILL BE DETERMINED IN THE NEXT ROUTINE, BLANK    GC024015
219300*--- DESCRIPTOR WILL BE LOADED IN THE EDIT TABLE.                 GC024015
219400                                                                  GC024015
219500     MOVE GAC-DEDL-AGE-LIMIT-FROM (GAC-INDEX)                     GC024015
219600       TO WS-AGE-LMT-F (WS-A-INDEX).                              GC024015
219700     IF GAC-DEDL-AGE-QUAL-IND-FROM (GAC-INDEX) = 'W'              GC024015
219800        COMPUTE WS-AGE-LMT-F (WS-A-INDEX) =                       GC024015
219900                WS-AGE-LMT-F (WS-A-INDEX) * 7                     GC024015
220000     ELSE                                                         GC024015
220100     IF GAC-DEDL-AGE-QUAL-IND-FROM (GAC-INDEX) = 'M'              GC024015
220200        COMPUTE WS-AGE-LMT-F (WS-A-INDEX) =                       GC024015
220300                WS-AGE-LMT-F (WS-A-INDEX) * 30                    GC024015
220400     ELSE                                                         GC024015
220500     IF GAC-DEDL-AGE-QUAL-IND-FROM (GAC-INDEX) = 'Y'              GC024015
220600        COMPUTE WS-AGE-LMT-F (WS-A-INDEX) =                       GC024015
220700                WS-AGE-LMT-F (WS-A-INDEX) * 365.                  GC024015
220800                                                                  GC024015
220900     MOVE GAC-DEDL-AGE-LIMIT-TO (GAC-INDEX)                       GC024015
221000       TO WS-AGE-LMT-T (WS-A-INDEX).                              GC024015
221100     IF GAC-DEDL-AGE-QUAL-IND-TO (GAC-INDEX) = 'W'                GC024015
221200        COMPUTE WS-AGE-LMT-T (WS-A-INDEX) =                       GC024015
221300                WS-AGE-LMT-T (WS-A-INDEX) * 7                     GC024015
221400     ELSE                                                         GC024015
221500     IF GAC-DEDL-AGE-QUAL-IND-TO (GAC-INDEX) = 'M'                GC024015
221600        COMPUTE WS-AGE-LMT-T (WS-A-INDEX) =                       GC024015
221700                WS-AGE-LMT-T (WS-A-INDEX) * 30                    GC024015
221800     ELSE                                                         GC024015
221900     IF GAC-DEDL-AGE-QUAL-IND-TO (GAC-INDEX) = 'Y'                GC024015
222000        COMPUTE WS-AGE-LMT-T (WS-A-INDEX) =                       GC024015
222100                WS-AGE-LMT-T (WS-A-INDEX) * 365.                  GC024015
222200                                                                  GC024015
222300     MOVE GAC-DEDL-INTERNAL-DESCRIPTOR (GAC-INDEX) TO             GC024015
222400          WS-INT-DESC (WS-A-INDEX).                               GC024015
222500     MOVE GAC-DEDL-ASCEND-DESCEND-IND (GAC-INDEX)  TO             GC024015
222600          WS-ASC-DES-IND (WS-A-INDEX).                            GC024015
222700     SET WS-A-INDEX  UP BY 1.                                     GC024015
222800*                                                                 GC024015
222900 3150-EXIT.                                                       GC024015
223000     EXIT.                                                        GC024015
223100******************************************************************GC024015
223200*    EDIT AGE-LIMITS AMD INTERNAL DESCRIPTORS FOR OCCURENCES      GC024015
223300*--- ARE AGE LIMITS ZEROS? YES, SEARCH FOR MATCHED DESCRIPTOR.    GC024015
223400*--- NO MATCH FOUND, THEN AGE LIMITS NOT TO BE CODED,             GC024015
223500*--- YES MATCH FOUND, AND AGE LIMITS ARE CODED, THEN AGE LIMITS   GC024015
223600*--- FROM TABLE MUST BE CODED AS OTHER OCCUR WITH SAME DESCRIPTOR.GC024015
223700*--- VALUE OF SPACES IN THE DESCRIPTOR WILL MATCH WITH SAME.      GC024015
223800******************************************************************GC024015
223900 3160-EDIT-AGE-LIMITS-ADL.                                        GC024015
224000                                                                  GC024015
224100     IF WS-AGE-LMT-T(WS-A-INDEX)  =  ZEROS                        GC024015
224200        NEXT SENTENCE                                             GC024015
224300     ELSE                                                         GC024015
224400        IF WS-INT-DESC (WS-A-INDEX)  =                            GC024015
224500                      WS-INT-DESC (WS-A-INDEX + 1)                GC024015
224600           PERFORM  3170-EDIT-BOTH-OCCURS THRU 3170-EXIT.         GC024015
224700                                                                  GC024015
224800     SET WS-A-INDEX  WS-B-INDEX  UP BY +1.                        GC024015
224900                                                                  GC024015
225000 3160-EXIT.                                                       GC024015
225100     EXIT.                                                        GC024015
225200                                                                  GC024015
225300******************************************************************GC024015
225400* IF INTERNAL DESCRIPTION MATCHS THEN AGE LIMIT SHOULD BE CODED,  GC024015
225500* RANGE OF AGE-LIMIT YEARS MUST BE IN ASCENDING SEQUENCE AND NOT  GC024015
225600* OVERLAPING OR LAPSED,           OK        OVERLAPED   LAPSED    GC024015
225700*                              005 - 010    005 010     005 010   GC024015
225800*           EXMP:              011 - 020    010 020     015 020   GC024015
225900*                              021 - 040    020 040     025 040   GC024015
226000******************************************************************GC024015
226100 3170-EDIT-BOTH-OCCURS.                                           GC024015
226200*                                                                 GC024015
226300     SET  WS-B-INDEX  UP BY +1.                                   GC024015
226400                                                                  GC024015
226500     IF WS-ASC-DES-IND (WS-A-INDEX) < HIGH-VALUES  AND            GC024015
226600        WS-ASC-DES-IND (WS-A-INDEX) > SPACE        AND            GC024015
226700        WS-ASC-DES-IND (WS-A-INDEX) NOT = '0'      AND            GC024015
226800        WS-ASC-DES-IND (WS-A-INDEX) = WS-ASC-DES-IND (WS-B-INDEX) GC024015
226900        MOVE 'Y' TO WS-CHAIN-SW                                   GC024015
227000     ELSE                                                         GC024015
227100        MOVE 'N' TO WS-CHAIN-SW.                                  GC024015
227200                                                                  GC024015
227300* OVERLAPPED                                                      GC024015
227400     IF WS-CHAIN-SW = 'N'                                         GC024015
227500       IF WS-AGE-LMT-T (WS-A-INDEX) NOT  <                        GC024015
227600          WS-AGE-LMT-F (WS-B-INDEX)                               GC024015
227700          ADD 1 TO DF-ERROR-COUNT                                 GC024015
227800          MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                   GC024015
227900          MOVE 'A36' TO DF-ERR-CODE (DF-ERROR-INDEX)              GC024015
228000          SET DF-ERROR-INDEX UP BY +1                             GC024015
228100        ELSE                                                      GC024015
228200           NEXT SENTENCE                                          GC024015
228300     ELSE                                                         GC024015
228400        IF WS-AGE-LMT-T (WS-A-INDEX) NOT  =                       GC024015
228500           WS-AGE-LMT-T (WS-B-INDEX)                              GC024015
228600        OR WS-AGE-LMT-F (WS-A-INDEX) NOT  =                       GC024015
228700           WS-AGE-LMT-F (WS-B-INDEX)                              GC024015
228800           ADD 1 TO DF-ERROR-COUNT                                GC024015
228900           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                  GC024015
229000           MOVE 'A37' TO DF-ERR-CODE (DF-ERROR-INDEX)             GC024015
229100           SET DF-ERROR-INDEX UP BY +1.                           GC024015
229200                                                                  GC024015
229300*                                                                 GC024015
229400     SET  WS-B-INDEX  DOWN BY +1.                                 GC024015
229500 3170-EXIT.                                                       GC024015
229600     EXIT.                                                        GC024015
229700/                                                                 GC024015
229800******************************************************************GC024015
229900 4000-AOL-EDITS.                                                  GC024015
230000                                                                  GC024015
230100     PERFORM 4010-000-AOL-EDIT1 THRU 4010-000-EXIT.               GC024015
230200     PERFORM 4020-000-AOL-EDIT2 THRU 4020-000-EXIT.               GC024015
230300*                                                                 GC024015
230400*--- START OF ISSR 11154                                          GC024015
230500*--- EDIT ALL OCCURRENCES FOR AGE LIMITS AND INTERNAL DESCRIPTOR  GC024015
230600                                                                  GC024015
230700     COMPUTE WS-OCCURS-ENTRIES = GAD-ENTRY-COUNT - 1.             GC024015
230800                                                                  GC024015
230900     IF  WS-OCCURS-ENTRIES     <   +2                             GC024015
231000         GO TO   4000-EXIT.                                       GC024015
231100                                                                  GC024015
231200     SET  WS-A-INDEX  TO +1.                                      GC024015
231300     PERFORM   4150-LOAD-AGE-LIMITS-AOL THRU 4150-EXIT            GC024015
231400         VARYING  GAD-INDEX  FROM +1  BY +1                       GC024015
231500           UNTIL  GAD-INDEX  >  WS-OCCURS-ENTRIES.                GC024015
231600                                                                  GC024015
231700     MOVE  'Y'  TO  WS-SORT-EXCHANGE-IND.                         GC024015
231800     PERFORM 7000-OCCURANCE-SORT THRU 7000-EXIT                   GC024015
231900       UNTIL  WS-NO-EXCHANGE-MADE.                                GC024015
232000                                                                  GC024015
232100     SET  WS-A-INDEX  WS-B-INDEX  TO +1.                          GC024015
232200     PERFORM  4160-EDIT-AGE-LIMITS-AOL THRU 4160-EXIT UNTIL       GC024015
232300        WS-INT-DESC (WS-A-INDEX)  =  HIGH-VALUES   OR             GC024015
232400           WS-A-INDEX   >  WS-OCCURS-ENTRIES.                     GC024015
232500                                                                  GC024015
232600*--- END OF 11154                                                 GC024015
232700*                                                                 GC024015
232800 4000-EXIT.                                                       GC024015
232900     EXIT.                                                        GC024015
233000                                                                  GC024015
233100*************************************************************     GC024015
233200* THE FOLLOWING EDIT CHECKS THE AOL PERCENT LEVEL.                GC024015
233300* IF THE PERCENT LEVEL IS NOT EQUAL TO 100 PERCENT,               GC024015
233400*     THEN ALL OTHER PERCENTS MUST BE THE SAME.                   GC024015
233500*************************************************************     GC024015
233600 4010-000-AOL-EDIT1.                                              GC024015
233700     IF GAD-ENTRY-COUNT < +3                                      GC024015
233800         GO TO 4010-000-EXIT.                                     GC024015
233900     MOVE +999 TO WS-PCT-LVL.                                     GC024015
234000     SET GAD-INDEX  TO +1.                                        GC024015
234100     PERFORM 4010-010-AOL-LOOP1 THRU 4010-010-EXIT                GC024015
234200       VARYING GAD-INDEX FROM 1 BY 1                              GC024015
234300       UNTIL   GAD-INDEX = GAD-ENTRY-COUNT.                       GC024015
234400 4010-000-EXIT.                                                   GC024015
234500     EXIT.                                                        GC024015
234600                                                                  GC024015
234700 4010-010-AOL-LOOP1.                                              GC024015
234800     IF GAD-O-P-X-PERCENT-LEVEL (GAD-INDEX)  = +100               GC024015
234900         GO TO 4010-010-EXIT.                                     GC024015
235000     IF GAD-O-P-X-PERCENT-LEVEL (GAD-INDEX)  NOT = WS-PCT-LVL     GC024015
235100         IF WS-PCT-LVL = +999                                     GC024015
235200             MOVE GAD-O-P-X-PERCENT-LEVEL (GAD-INDEX)             GC024015
235300               TO WS-PCT-LVL                                      GC024015
235400             GO TO 4010-010-EXIT                                  GC024015
235500         ELSE                                                     GC024015
235600             ADD 1 TO DF-ERROR-COUNT                              GC024015
235700             MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                GC024015
235800             MOVE 'A01' TO DF-ERR-CODE (DF-ERROR-INDEX)           GC024015
235900             SET DF-ERROR-INDEX UP BY 1                           GC024015
236000             SET GAD-INDEX TO GAD-ENTRY-COUNT                     GC024015
236100             SET GAD-INDEX DOWN BY 1.                             GC024015
236200 4010-010-EXIT.                                                   GC024015
236300     EXIT.                                                        GC024015
236400/                                                                 GC024015
236500***************************************************************** GC024015
236600* USER REQUIREMENTS FOR EDIT D222. FOR #ACL AND #AOL RECORDS.     GC024015
236700*                                                                 GC024015
236800*     FOR ALL OCCURS, UNLESS AN ERROR CONDITION IS MET,           GC024015
236900*     IF THE ASCEND/DESCEND IND. = '1' OR '2' OR '3'              GC024015
237000*         CHECK THE OTHER OCCURS FOR AN ASCEND/DESCEND IND WITH   GC024015
237100*         THE SAME VALUE (THERE MAY BE SEVERAL OCCURS)            GC024015
237200*         IF NONE ARE FOUND,                                      GC024015
237300*             ASSIGN AN ERROR CODE                                GC024015
237400*         ELSE                                                    GC024015
237500*             COMPARE THE FIELDS FROM THE ONE OCCURS WITH THE     GC024015
237600*             FIELDS FROM THE OTHER OCCURS (SEE CODE FOR THE      GC024015
237700*             SPECIFIC FIELD NAMES)                               GC024015
237800*             IF THE VALUES IN THE FIELDS CHECKED ARE ALL EQUAL   GC024015
237900*             TO EACH OTHER,                                      GC024015
238000*                 EXIT                                            GC024015
238100*             ELSE                                                GC024015
238200*                 ASSIGN AN ERROR CODE                            GC024015
238300*                 EXIT.                                           GC024015
238400***************************************************************** GC024015
238500 4020-000-AOL-EDIT2.                                              GC024015
238600     IF GAD-ENTRY-COUNT < 3                                       GC024015
238700         GO TO 4020-000-EXIT.                                     GC024015
238800     MOVE 'N' TO WS-FINISH-SW.                                    GC024015
238900     MOVE LOW-VALUES TO WS-TABULAR-REC-AREA.                      GC024015
239000     MOVE LS-TABULAR-REC-AREA TO WS-TABULAR-REC-AREA.             GC024015
239100     SET GAD-INDEX  TO +1.                                        GC024015
239200     PERFORM 4020-010-LOOP1 THRU 4020-010-EXIT                    GC024015
239300       VARYING GAD-INDEX FROM 1 BY 1                              GC024015
239400       UNTIL   GAD-INDEX = GAD-ENTRY-COUNT                        GC024015
239500          OR   WS-FINISH-SW = 'Y'.                                GC024015
239600 4020-000-EXIT.                                                   GC024015
239700     EXIT.                                                        GC024015
239800                                                                  GC024015
239900****************************************************************  GC024015
240000 4020-010-LOOP1.                                                  GC024015
240100     IF GAD-O-P-X-ASCEND-DESCEND-IND (GAD-INDEX) = '1' OR         GC024015
240200                                                   '2' OR '3'     GC024015
240300         SET GAD2-INDEX TO 1                                      GC024015
240400         MOVE 'N' TO WS-MATCH1-SW                                 GC024015
240500                     WS-MATCH2-SW                                 GC024015
240600         PERFORM 4020-020-LOOP2 THRU 4020-020-EXIT                GC024015
240700           VARYING GAD2-INDEX FROM 1 BY 1                         GC024015
240800           UNTIL   GAD2-INDEX = GAD2-ENTRY-COUNT                  GC024015
240900     ELSE                                                         GC024015
241000         GO TO 4020-010-EXIT.                                     GC024015
241100     IF WS-MATCH1-SW = 'N'                                        GC024015
241200         ADD 1 TO DF-ERROR-COUNT                                  GC024015
241300         MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                    GC024015
241400         MOVE 'A04' TO DF-ERR-CODE (DF-ERROR-INDEX)               GC024015
241500         SET DF-ERROR-INDEX UP BY 1                               GC024015
241600         MOVE 'Y' TO WS-FINISH-SW                                 GC024015
241700         GO TO  4020-010-EXIT.                                    GC024015
241800     IF WS-MATCH2-SW = 'N'                                        GC024015
241900         ADD 1 TO DF-ERROR-COUNT                                  GC024015
242000         MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                    GC024015
242100         MOVE 'A05' TO DF-ERR-CODE (DF-ERROR-INDEX)               GC024015
242200         SET DF-ERROR-INDEX UP BY 1                               GC024015
242300         MOVE 'Y' TO WS-FINISH-SW                                 GC024015
242400         GO TO  4020-010-EXIT.                                    GC024015
242500 4020-010-EXIT.                                                   GC024015
242600     EXIT.                                                        GC024015
242700                                                                  GC024015
242800***************************************************************** GC024015
242900 4020-020-LOOP2.                                                  GC024015
243000                                                                  GC024015
243100     IF GAD2-INDEX = GAD-INDEX                                    GC024015
243200         GO TO 4020-020-EXIT.                                     GC024015
243300                                                                  GC024015
243400     IF GAD-O-P-X-ASCEND-DESCEND-IND (GAD-INDEX)                  GC024015
243500       = GAD2-O-P-X-ASCEND-DESCEND-IND (GAD2-INDEX)               GC024015
243600         MOVE 'Y' TO WS-MATCH1-SW                                 GC024015
243700     ELSE                                                         GC024015
243800         GO TO 4020-020-EXIT.                                     GC024015
243900*******                                                           GC024015
244000* THE CONDITION BIT FILLER BIT IS PRIMED JUST IN CASE A MUTANT    GC024015
244100* VALUE EVER ENDS UP THERE.                                       GC024015
244200*                                                                 GC024015
244300     MOVE GAD-COND-FILLER-BIT (GAD-INDEX)                         GC024015
244400       TO GAD2-COND-FILLER-BIT (GAD2-INDEX).                      GC024015
244500*                                                                 GC024015
244600     IF (GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX)                     GC024015
244700       = GAD2-O-P-X-BENEFIT-PERIOD (GAD2-INDEX))                  GC024015
244800       AND                                                        GC024015
244900        (GAD-O-P-X-L-O-B (GAD-INDEX)                              GC024015
245000       = GAD2-O-P-X-L-O-B (GAD2-INDEX))                           GC024015
245100       AND                                                        GC024015
245200        (GAD-O-P-X-FAM-OR-INDIV (GAD-INDEX)                       GC024015
245300       = GAD2-O-P-X-FAM-OR-INDIV (GAD2-INDEX))                    GC024015
245400       AND                                                        GC024015
245500        (GAD-O-P-X-COST-CONTAIN-IND (GAD-INDEX)                   GC024015
245600       = GAD2-O-P-X-COST-CONTAIN-IND (GAD2-INDEX))                GC024015
245700       AND                                                        GC024015
245800        (GAD-O-P-X-PLACE-OF-TREATMENT (GAD-INDEX)                 GC024015
245900       = GAD2-O-P-X-PLACE-OF-TREATMENT (GAD2-INDEX))              GC024015
246000       AND                                                        GC024015
246100        (GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX)                    GC024015
246200       = GAD2-O-P-X-VALUE-QUALIFIER (GAD2-INDEX))                 GC024015
246300       AND                                                        GC024015
246400        (GAD-O-P-X-SERVICE-GROUP (GAD-INDEX)                      GC024015
246500       = GAD2-O-P-X-SERVICE-GROUP (GAD2-INDEX))                   GC024015
246600       AND                                                        GC024015
246700        (GAD-O-P-X-CO-PAY-IND (GAD-INDEX)                         GC024015
246800       = GAD2-O-P-X-CO-PAY-IND (GAD2-INDEX))                      GC024015
246900       AND                                                        GC024015
247000        (GAD-O-P-X-CONDITION (GAD-INDEX)                          GC024015
247100       = GAD2-O-P-X-CONDITION (GAD2-INDEX))                       GC024015
247200       AND                                                        GC024015
247300        (GAD-O-P-X-INTERNAL-DESCRIPTOR (GAD-INDEX)                GC024015
247400       = GAD2-O-P-X-INTERNAL-DESCRIPTOR (GAD2-INDEX))             GC024015
247500       AND                                                        GC024015
247600        (GAD-O-P-X-AGE-QUAL-IND-FROM (GAD-INDEX)                  GC024015
247700       = GAD2-O-P-X-AGE-QUAL-IND-FROM (GAD2-INDEX))               GC024015
247800       AND                                                        GC024015
247900         (GAD-O-P-X-AGE-QUAL-IND-TO (GAD-INDEX)                   GC024015
248000       =  GAD2-O-P-X-AGE-QUAL-IND-TO (GAD2-INDEX))                GC024015
248100       AND                                                        GC024015
248200         (GAD-O-P-X-AGE-LIMIT-FROM (GAD-INDEX)                    GC024015
248300       =  GAD2-O-P-X-AGE-LIMIT-FROM (GAD2-INDEX))                 GC024015
248400       AND                                                        GC024015
248500         (GAD-O-P-X-AGE-QUAL-IND-TO (GAD-INDEX)                   GC024015
248600       =  GAD2-O-P-X-AGE-QUAL-IND-TO (GAD2-INDEX))                GC024015
248700         MOVE 'Y' TO WS-MATCH2-SW.                                GC024015
248800                                                                  GC024015
248900 4020-020-EXIT.                                                   GC024015
249000     EXIT.                                                        GC024015
249100***************************************************************   GC024015
249200 4150-LOAD-AGE-LIMITS-AOL.                                        GC024015
249300                                                                  GC024015
249400*--- INTERNAL DESCRIPTOR IS SPACES AND AGE-LIMITS IS ZEROS, SKIP. GC024015
249500     IF GAD-O-P-X-INTERNAL-DESCRIPTOR (GAD-INDEX) = SPACES AND    GC024015
249600        GAD-O-P-X-AGE-LIMIT-FROM (GAD-INDEX)     = ZEROS   AND    GC024015
249700        GAD-O-P-X-AGE-LIMIT-TO  (GAD-INDEX)      = ZEROS          GC024015
249800        GO TO   4150-EXIT.                                        GC024015
249900                                                                  GC024015
250000*--- IF AGE LIMIT FIELDS ARE  N O T  CODED THEN INTERNAL          GC024015
250100*--- DESCRIPTOR HAS NO EFFECT ON THE LOGIC, U N L E S S  THE SAME GC024015
250200*--- DESCRIPTOR IS ON DIFFERENT OCCURS AND THE AGE-LIMITS ARE     GC024015
250300*--- CODED, THIS WILL BE DETERMINED IN THE NEXT ROUTINE, BLANK    GC024015
250400*--- DESCRIPTOR WILL BE LOADED IN THE EDIT TABLE.                 GC024015
250500                                                                  GC024015
250600     MOVE GAD-O-P-X-AGE-LIMIT-FROM (GAD-INDEX)                    GC024015
250700       TO WS-AGE-LMT-F (WS-A-INDEX).                              GC024015
250800     IF GAD-O-P-X-AGE-QUAL-IND-FROM (GAD-INDEX) = 'W'             GC024015
250900        COMPUTE WS-AGE-LMT-F (WS-A-INDEX) =                       GC024015
251000                WS-AGE-LMT-F (WS-A-INDEX) * 7                     GC024015
251100     ELSE                                                         GC024015
251200     IF GAD-O-P-X-AGE-QUAL-IND-FROM (GAD-INDEX) = 'M'             GC024015
251300        COMPUTE WS-AGE-LMT-F (WS-A-INDEX) =                       GC024015
251400                WS-AGE-LMT-F (WS-A-INDEX) * 30                    GC024015
251500     ELSE                                                         GC024015
251600     IF GAD-O-P-X-AGE-QUAL-IND-FROM (GAD-INDEX) = 'Y'             GC024015
251700        COMPUTE WS-AGE-LMT-F (WS-A-INDEX) =                       GC024015
251800                WS-AGE-LMT-F (WS-A-INDEX) * 365.                  GC024015
251900                                                                  GC024015
252000     MOVE GAD-O-P-X-AGE-LIMIT-TO (GAD-INDEX)                      GC024015
252100       TO WS-AGE-LMT-T (WS-A-INDEX).                              GC024015
252200     IF GAD-O-P-X-AGE-QUAL-IND-TO (GAD-INDEX) = 'W'               GC024015
252300        COMPUTE WS-AGE-LMT-T (WS-A-INDEX) =                       GC024015
252400                WS-AGE-LMT-T (WS-A-INDEX) * 7                     GC024015
252500     ELSE                                                         GC024015
252600     IF GAD-O-P-X-AGE-QUAL-IND-TO (GAD-INDEX) = 'M'               GC024015
252700        COMPUTE WS-AGE-LMT-T (WS-A-INDEX) =                       GC024015
252800                WS-AGE-LMT-T (WS-A-INDEX) * 30                    GC024015
252900     ELSE                                                         GC024015
253000     IF GAD-O-P-X-AGE-QUAL-IND-TO (GAD-INDEX) = 'Y'               GC024015
253100        COMPUTE WS-AGE-LMT-T (WS-A-INDEX) =                       GC024015
253200                WS-AGE-LMT-T (WS-A-INDEX) * 365.                  GC024015
253300                                                                  GC024015
253400     MOVE GAD-O-P-X-INTERNAL-DESCRIPTOR (GAD-INDEX) TO            GC024015
253500          WS-INT-DESC (WS-A-INDEX).                               GC024015
253600     MOVE GAD-O-P-X-ASCEND-DESCEND-IND (GAD-INDEX)  TO            GC024015
253700          WS-ASC-DES-IND (WS-A-INDEX).                            GC024015
253800     SET WS-A-INDEX  UP BY 1.                                     GC024015
253900*                                                                 GC024015
254000 4150-EXIT.                                                       GC024015
254100     EXIT.                                                        GC024015
254200******************************************************************GC024015
254300*    EDIT AGE-LIMITS AMD INTERNAL DESCRIPTORS FOR OCCURENCES      GC024015
254400*--- ARE AGE LIMITS ZEROS? YES, SEARCH FOR MATCHED DESCRIPTOR.    GC024015
254500*--- NO MATCH FOUND, THEN AGE LIMITS NOT TO BE CODED,             GC024015
254600*--- YES MATCH FOUND, AND AGE LIMITS ARE CODED, THEN AGE LIMITS   GC024015
254700*--- FROM TABLE MUST BE CODED AS OTHER OCCUR WITH SAME DESCRIPTOR.GC024015
254800*--- VALUE OF SPACES IN THE DESCRIPTOR WILL MATCH WITH SAME.      GC024015
254900******************************************************************GC024015
255000 4160-EDIT-AGE-LIMITS-AOL.                                        GC024015
255100                                                                  GC024015
255200     IF WS-AGE-LMT-T(WS-A-INDEX)  =  ZEROS                        GC024015
255300        NEXT SENTENCE                                             GC024015
255400     ELSE                                                         GC024015
255500        IF WS-INT-DESC (WS-A-INDEX)  =                            GC024015
255600           WS-INT-DESC (WS-A-INDEX + 1)                           GC024015
255700              PERFORM  4170-EDIT-BOTH-OCCURS THRU 4170-EXIT.      GC024015
255800                                                                  GC024015
255900     SET WS-A-INDEX  WS-B-INDEX  UP BY +1.                        GC024015
256000                                                                  GC024015
256100 4160-EXIT.                                                       GC024015
256200     EXIT.                                                        GC024015
256300******************************************************************GC024015
256400* IF INTERNAL DESCRIPTION MATCHS THEN AGE LIMIT SHOULD BE CODED,  GC024015
256500* RANGE OF AGE-LIMIT YEARS MUST BE IN ASCENDING SEQUENCE AND NOT  GC024015
256600* OVERLAPING OR LAPSED,           OK        OVERLAPED   LAPSED    GC024015
256700*                              005 - 010    005 010     005 010   GC024015
256800*           EXMP:              011 - 020    010 020     015 020   GC024015
256900*                              021 - 040    020 040     025 040   GC024015
257000******************************************************************GC024015
257100 4170-EDIT-BOTH-OCCURS.                                           GC024015
257200*                                                                 GC024015
257300     SET  WS-B-INDEX  UP BY +1.                                   GC024015
257400                                                                  GC024015
257500     IF WS-ASC-DES-IND (WS-A-INDEX) < HIGH-VALUES  AND            GC024015
257600        WS-ASC-DES-IND (WS-A-INDEX) > SPACE        AND            GC024015
257700        WS-ASC-DES-IND (WS-A-INDEX) NOT = '0'      AND            GC024015
257800        WS-ASC-DES-IND (WS-A-INDEX) = WS-ASC-DES-IND (WS-B-INDEX) GC024015
257900        MOVE 'Y' TO WS-CHAIN-SW                                   GC024015
258000     ELSE                                                         GC024015
258100        MOVE 'N' TO WS-CHAIN-SW.                                  GC024015
258200                                                                  GC024015
258300* OVERLAPPED                                                      GC024015
258400     IF WS-CHAIN-SW = 'N'                                         GC024015
258500        IF WS-AGE-LMT-T (WS-A-INDEX) NOT  <                       GC024015
258600           WS-AGE-LMT-F (WS-B-INDEX)                              GC024015
258700           ADD 1 TO DF-ERROR-COUNT                                GC024015
258800           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                  GC024015
258900           MOVE 'A09' TO DF-ERR-CODE (DF-ERROR-INDEX)             GC024015
259000           SET DF-ERROR-INDEX UP BY +1                            GC024015
259100        ELSE                                                      GC024015
259200           NEXT SENTENCE                                          GC024015
259300     ELSE                                                         GC024015
259400        IF WS-AGE-LMT-T (WS-A-INDEX) NOT  =                       GC024015
259500           WS-AGE-LMT-T (WS-B-INDEX)                              GC024015
259600        OR WS-AGE-LMT-F (WS-A-INDEX) NOT  =                       GC024015
259700           WS-AGE-LMT-F (WS-B-INDEX)                              GC024015
259800           ADD 1 TO DF-ERROR-COUNT                                GC024015
259900           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                  GC024015
260000           MOVE 'A11' TO DF-ERR-CODE (DF-ERROR-INDEX)             GC024015
260100           SET DF-ERROR-INDEX UP BY +1.                           GC024015
260200*                                                                 GC024015
260300     SET  WS-B-INDEX  DOWN BY +1.                                 GC024015
260400                                                                  GC024015
260500 4170-EXIT.                                                       GC024015
260600     EXIT.                                                        GC024015
260700/***************************************************************  GC024015
260800*    SORT ALL OCCURS AGE LIMT FIELDS IN ASCENDING SEQUENCE     *  GC024015
260900****************************************************************  GC024015
261000 7000-OCCURANCE-SORT.                                             GC024015
261100                                                                  GC024015
261200     COMPUTE Y = X + 1.                                           GC024015
261300     MOVE 'N' TO WS-SORT-EXCHANGE-IND.                            GC024015
261400     PERFORM 7100-OCCURANCE-EXCHANGE THRU 7100-EXIT               GC024015
261500        VARYING   X FROM 1 BY 1                                   GC024015
261600          UNTIL   X >  WS-OCCURS-ENTRIES       OR                 GC024015
261700          AGE-OCCUR-KEY(Y)   =  HIGH-VALUES.                      GC024015
261800     MOVE 1 TO X.                                                 GC024015
261900 7000-EXIT.                                                       GC024015
262000     EXIT.                                                        GC024015
262100/***************************************************************  GC024015
262200* 7100   EXCHANGE AGE LIMIT FIELDS ENTRIES                     *  GC024015
262300****************************************************************  GC024015
262400 7100-OCCURANCE-EXCHANGE.                                         GC024015
262500                                                                  GC024015
262600     COMPUTE Y = X + 1.                                           GC024015
262700                                                                  GC024015
262800     IF Y >  WS-OCCURS-ENTRIES                                    GC024015
262900        GO TO 7100-EXIT.                                          GC024015
263000                                                                  GC024015
263100     IF AGE-OCCUR-KEY(Y)              < AGE-OCCUR-KEY(X)          GC024015
263200        MOVE 'Y'                     TO WS-SORT-EXCHANGE-IND      GC024015
263300        MOVE AGE-AL-ENTRY(Y)         TO WS-OCCURANCE-ENTRY        GC024015
263400        MOVE AGE-AL-ENTRY(X)         TO AGE-AL-ENTRY(Y)           GC024015
263500        MOVE WS-OCCURANCE-ENTRY      TO AGE-AL-ENTRY(X).          GC024015
263600                                                                  GC024015
263700 7100-EXIT.                                                       GC024015
263800     EXIT.                                                        GC024015
263900                                                                  GC024015
