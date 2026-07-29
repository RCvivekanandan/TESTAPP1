000100 IDENTIFICATION DIVISION.                                         12/09/02
000200 PROGRAM-ID.    GC024010.                                         GC024010
000300*THIS IS A COBOL/2 PROGRAM                                           LV002
000400 AUTHOR.        ED WITKUS.                                        GC024010
000500 INSTALLATION.  HCSC.                                             GC024010
000600 DATE-WRITTEN.  MAY, 1986.                                        GC024010
000700 DATE-COMPILED.                                                   GC024010
000800******************************************************************GC024010
000900*                                                                *GC024010
001000*   THIS PROGRAM PERFORMS THE LOGICAL EDITS FOR THE CONTRACT FILE*GC024010
001100*   IT THEN CALLS 'GC024030' WHICH UPDATES THE DISCREPANCY FILE. *GC024010
001200*                                                                *GC024010
001300*    THIS PROGRAM MODULE IS CALLED BY PROGRAM GC0240             *GC024010
001400*    AND IS NOT TO BE EXECUTED AS A STAND ALONE PROGRAM.         *GC024010
001500*                                                                *GC024010
001600*    TSGVSAM1 IS THE ONLINE CONTRACT FILE - (I/O).               *GC024010
001700*    TSGVSAM4 IS THE ONLINE TABULAR FILE - (I/O).                *GC024010
001800*                                                                *GC024010
001900******************************************************************GC024010
002000******************************************************************GC024010
002100*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC024010
002200*       *-*         U P D A T E   H I S T O R Y         *-*      *GC024010
002300*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC024010
002400*                                                                *GC024010
002500* CHG NUM    DATE    BY   *---------DESCRIPTION------------------*GC024010
002600*  _____   ________  ___  _______________________________________*GC024010
002700*  XXXX    05/01/86  ENW  ORIGINAL MODULE                         GC024010
002800*  XXXX    07/24/86  AMJ  CORRECTED LINKAGE SECTION MISMATCH      GC024010
002900*  XXXX    11/14/86  FRY  MODIFIED CONTRACT EDITS:  1A, 2A, 3A,   GC024010
003000*                         4A, 5A.                                 GC024010
003100*                         ADDED CONTRACT EDIT 7A.                 GC024010
003200*  R0602   03/18/87  ENW  MODIFIED CONTRACT EDITS:                GC024010
003300*                         REMOVED EDIT 6A AND 7A.                 GC024010
003400*  D116    09/09/87  FRY  CAPTURE OPERATOR-ID                     GC024010
003500*  D214    04/13/89  ENW  ADDED LOGIC TO CHECK ACCUMS TO SEE IF   GC024010
003600*                         ALL LOBS MATCH THE CONTRACT LOB.        GC024010
003700*  D215    05/19/89  ENW  ADDED EDITS FOR C19, C20, C21, AND C22. GC024010
003800*  D210                                                           GC024010
003900*  D203                                                           GC024010
004000*  D203    06/21/89  ENW  REVISED EDIT FOR C22, PER LOUANA HALL.  GC024010
004100*  D222    07/11/89  ENW  ADDED CALL TO GC024015 FOR ACCUM-EDITS. GC024010
004200*                                                                 GC024010
004300*                       ----ACCUM TABULAR RECORD MODIFICATION--- *GC024010
004400* 11154   10/02/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *GC024010
004500* D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *GC024010
004600* D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *GC024010
004700* D270                  4. ADD AGE-LIMIT-FROM AND AGE-LIMIT-TO.  *GC024010
004800*                       5. ADD RELATIONSHIP-IND (CDE ELEMENT).   *GC024010
004900*                    ** 6. ADD AGE LIMIT LOGICAL EDIT ROUTINE BY *GC024010
005000*                          CALLING GC024015 FOR ALL ACUMM TABS.  *GC024010
005100*                                                                 GC024010
005200* 11154     2/22/91  FRY  -DECREASE MAX OCCURS FROM 46 TO 46.    *GC024010
005300*                         -DECREASE MAX RECORD LENGTH FOR ACCUM  *GC024010
005400*                           RECORD FROM 8157 TO 7805.            *GC024010
005500*                      WORKING STORAGE CHANGED FOR TABULAR       *GC024010
005600*                      RECORD AREAS.                             *GC024010
005700*                       -TWOA-REC-AREA  PIC X(8157) TO 7805      *GC024010
005800*                       -CHANGED  REPLACING  ==  1 TO 46  ==     *GC024010
005900*                             TO  REPLACING  ==  1 TO 44  ==     *GC024010
006000*                                                                *GC024010
006100* 11698     4/08/91  PFH  ADDED LOGICAL EDITS FOR PRODUCT TYPE.  *GC024010
006200*                                                                *GC024010
006300* D12009   09/10/91  GDM  INCREASE LC-FAM-REL-LVL TO 2 POSITIONS *GC024010
006400*                                                                *GC024010
006500* D12009   09/19/91  TPM                                         *GC024010
006600*                       EXPANSION OF THE FAMILY-RELATION FIELD.  *GC024010
006700*                       CHANGED THE RECORD LENGTH FROM 21 TO 22  *GC024010
006800*                       WHEN CALLING THE TSGVSAM ROUTINE.        *GC024010
006900*                                                                *GC024010
007000*          01/18/95  GDM  CONVERT TO COBOL II                    *GC024010
007100*                                                                *GC024010
007200* D329      5/08/97  FRY  ADDED CONTRACT EDIT FOR FIELD,         *GC024010
007300*                         EXTEND-BENEFIT-PERIOD-YR.              *GC024010
007400*                                                                *GC024010
007500*  14726/                                                         GC024010
007600*  15057     09/11/97  AB   CODE ADDED TO SUPPORT THE YEAR        GC024010
007700*                           2000 AND THE EXPANSION OF THE         GC024010
007800*                           CONTRACT KEY TO SUPPORT THE TX        GC024010
007900*                           MERGER.                               GC024010
008000*                                                                *GC024010
008100* D342     10/20/98  FRY  CHANGE REFERENCE TO POSITIONAL         *GC024010
008200*                          TABULARS LOCATED IN THE COMMON AREA   *GC024010
008300*                          OF THE CONTRACT RECORD.               *GC024010
008400*                           FROM:                                *GC024010
008500*                          #ADL   MOVE GCT-CON-TAB-ID-SLOT (7)   *GC024010
008600*                          #AOL   MOVE GCT-CON-TAB-ID-SLOT (9)   *GC024010
008700*                             TO:                                *GC024010
008800*                          #ADL   MOVE GCT-CON-TAB-ID-SLOT (8)   *GC024010
008900*                          #AOL   MOVE GCT-CON-TAB-ID-SLOT (10)  *GC024010
009000*                                                                *GC024010
009100* D15182   11/24/98  GDM  ADD LOGIC TO SUPPORT NEW #ACP ACCUM    *GC024010
009200*                         TABULAR                                *GC024010
009300*                                                                *GC024010
009400*          02/04/99  KJD  ADD EXTRA DISPLAY INFO, FIX BAD GOTO   *GC024010
009500*                         IN 1410-COMPARE-ACP-OCCURS PARAGRAPH   *GC024010
009600*                                                                *GC024010
009700* D-353    09/25/00  GSP  ADDED EDITS C31 & C32 TO SUPPORT THE   *GC024010
009800*                         NEW COPAY FIELDS:                      *GC024010
009900*                         - GCT-COPAY-PERC-FOR-COPAY-IND         *GC024010
010000*                         - GCT-COPAY-PERC-FOR-COPAY             *GC024010
010100*                                                                *GC024010
010200* D-365B   06/19/02   JP  ADDED EDITS C33 & C34 TO SUPPORT THE   *GC024010
010300*                         NEW CAPI ACCUM FIELDS.                 *GC024010
010400*                                                                *GC024010
010500* P01760   09/24/02  AKK  ADD CHANGES FOR OPID                   *GC024010
010600*                                                                *GC024010
010610* DM9441   09/21/09  DNK  EXPANDED TABULAR READ AREA FROM 7805   *GC024010
010620*                         TO 31370.                              *GC024010
ED0624* BBDA-58217 06/04/24 ED  PEAQ COPYBOOK EXPANSION:               *        
ED0624*                                COPYBKS - GCTABM*, GCTACL*,     *        
ED0624*                                GCTACP*,  GCTADL*, GCTADL*      *        
010630*                                                                *GC024010
010700******************************************************************GC024010
010800******************************************************************GC024010
010900 ENVIRONMENT DIVISION.                                            GC024010
011000 CONFIGURATION SECTION.                                           GC024010
011100 SOURCE-COMPUTER. IBM-370.                                        GC024010
011200 OBJECT-COMPUTER. IBM-370.                                        GC024010
011300 INPUT-OUTPUT SECTION.                                            GC024010
011400 FILE-CONTROL.                                                    GC024010
011500 DATA DIVISION.                                                   GC024010
011600 FILE SECTION.                                                    GC024010
011700/                                                                 GC024010
011800 WORKING-STORAGE SECTION.                                         GC024010
011900 01  FILLER                    PIC X(32) VALUE                    GC024010
012000     'GC024010 WORKING STORAGE'.                                  GC024010
012100                                                                  GC024010
012200 01  VSAM-WS-ERR-MSG           PIC X(32) VALUE SPACES.            GC024010
012300                                                                  GC024010
012400 01  ABEND-CODE                PIC 9(4)  COMP.                    GC024010
012500                                                                  GC024010
012600 01  WS-MISC-AREA.                                                GC024010
012700     05   WS-PCT-LVL           PIC S9(3) COMP-3 VALUE ZEROS.      GC024010
012800     05   WS-DISP-CNT          PIC  9(3)        VALUE ZEROS.      GC024010
012900     05   WS-1230-EDIT-CNT     PIC S9(3) COMP-3 VALUE ZEROS.      GC024010
013000     05   WS-DF-ERROR-COUNT    PIC S9(3) COMP-3 VALUE ZEROS.      GC024010
013100     05   WS-GC024015          PIC X(8)         VALUE 'GC024015'. GC024010
013200                                                                  GC024010
013300 01  SWITCH-AREA.                                                 GC024010
013400     05  ERROR-SW              PIC XXX   VALUE 'NO '.             GC024010
013500         88  ERROR-FOUND                 VALUE 'YES'.             GC024010
013600     05  WS-GCT-EXTEND-BENPER-YR-ER-SW   PIC X(03) VALUE 'NO '.   GC024010
013700         88  WS-GCT-EXTEND-BENPER-YR-ERROR         VALUE 'YES'.   GC024010
013800     05  WS-ABM-AF-SWITCH               PIC X(03) VALUE 'NO '.    GC024010
013900         88 WS-ABM-AF-CODED                       VALUE 'YES'.    GC024010
014000     05  WS-ACL-AF-SWITCH               PIC X(03) VALUE 'NO '.    GC024010
014100         88 WS-ACL-AF-CODED                       VALUE 'YES'.    GC024010
014200     05  WS-ACP-AF-SWITCH               PIC X(03) VALUE 'NO '.    GC024010
014300         88 WS-ACP-AF-CODED                       VALUE 'YES'.    GC024010
014400     05  WS-ADL-AF-SWITCH               PIC X(03) VALUE 'NO '.    GC024010
014500         88 WS-ADL-AF-CODED                       VALUE 'YES'.    GC024010
014600     05  WS-AOL-AF-SWITCH               PIC X(03) VALUE 'NO '.    GC024010
014700         88 WS-AOL-AF-CODED                       VALUE 'YES'.    GC024010
014800     05  WS-ABM-L-O-B-SWITCH            PIC X(03) VALUE 'NO '.    GC024010
014900         88 WS-ABM-L-O-B-ERROR                    VALUE 'YES'.    GC024010
015000     05  WS-ACL-L-O-B-SWITCH            PIC X(03) VALUE 'NO '.    GC024010
015100         88 WS-ACL-L-O-B-ERROR                    VALUE 'YES'.    GC024010
015200     05  WS-ACP-L-O-B-SWITCH            PIC X(03) VALUE 'NO '.    GC024010
015300         88 WS-ACP-L-O-B-ERROR                    VALUE 'YES'.    GC024010
015400     05  WS-ADL-L-O-B-SWITCH            PIC X(03) VALUE 'NO '.    GC024010
015500         88 WS-ADL-L-O-B-ERROR                    VALUE 'YES'.    GC024010
015600     05  WS-AOL-L-O-B-SWITCH            PIC X(03) VALUE 'NO '.    GC024010
015700         88 WS-AOL-L-O-B-ERROR                    VALUE 'YES'.    GC024010
015800     05  WS-ACL-CAPI-FOUND-SW           PIC X(01) VALUE 'N'.      GC024010
015900         88 ACL-CAPI-FOUND                        VALUE 'Y'.      GC024010
016000         88 ACL-CAPI-NOT-FD                       VALUE 'N'.      GC024010
016100     05  WS-ACP-CAPI-FOUND-SW           PIC X(01) VALUE 'N'.      GC024010
016200         88 ACP-CAPI-FOUND                        VALUE 'Y'.      GC024010
016300         88 ACP-CAPI-NOT-FD                       VALUE 'N'.      GC024010
016400                                                                  GC024010
016500 01  WS-DEDUCT-DEF.                                               GC024010
016600     05  WS-DED-DEF                 PIC X(02)  VALUE SPACE.       GC024010
016700         88 SELECTED-VALUE          VALUE '10' THRU '13'.         GC024010
016800                                                                  GC024010
016900 01  WS-COINS-LIMIT.                                              GC024010
017000     05  WS-COINS-LIM               PIC X(02)  VALUE SPACE.       GC024010
017100         88 SELECTED-COINS          VALUE 'C1' THRU 'C5'.         GC024010
017200                                                                  GC024010
017300 01  WS-OUT-OF-POCKET.                                            GC024010
017400     05  WS-OPX                     PIC X(02)  VALUE SPACE.       GC024010
017500         88 SELECTED-OPX            VALUE '10' THRU '20'.         GC024010
017600                                                                  GC024010
017700 01  DATE-CHECK.                                                  GC024010
017800     05  DATE-CHK            PIC 9(5).                            GC024010
017900     05  D-CHK REDEFINES DATE-CHK.                                GC024010
018000         10  D-CHK-YY        PIC 99.                              GC024010
018100         10  D-CHK-DD        PIC 999.                             GC024010
018200                                                                  GC024010
018300 01  DATE-CHECK-CALC-AREA.                                        GC024010
018400     05  DATE-CHK-QUOTIENT   PIC 9(4).                            GC024010
018500     05  DATE-CHK-REMAINDER  PIC 9.                               GC024010
018600                                                                  GC024010
018700 01  DATE-WORK-AREA.                                              GC024010
018800     05  JUL-DATE.                                                GC024010
018900         10  JUL-YY          PIC 99.                              GC024010
019000         10  JUL-DD          PIC 999.                             GC024010
019100     05  JUL-DTE REDEFINES JUL-DATE PIC 9(5).                     GC024010
019200                                                                  GC024010
019300     COPY HSCDATES.                                               GC024010
019400/                                                                 GC024010
019500 01  PARM-SET.                                                    GC024010
019600     05  SET-RDW.                                                 GC024010
019700         10  SET-REC-LENG    PIC 9(4)    VALUE ZEROS     COMP.    GC024010
019800         10  SET-FEEDBACK    PIC 9(4)    VALUE ZEROS     COMP.    GC024010
019900     05  SET-VALUE           PIC 9(8)                    COMP.    GC024010
020000                                                                  GC024010
020100 01  PARM-ONE.                                                    GC024010
020200     05  RESERVED-FLDS-1     PIC 9(8)    VALUE ZEROS     COMP.    GC024010
020300     05  RESERVED-X-1 REDEFINES RESERVED-FLDS-1.                  GC024010
020400         10  REQUEST-TYPE-1  PIC X.                               GC024010
020500         10  FILLER          PIC X(3).                            GC024010
020600                                                                  GC024010
020700 01  PARM-ONEA.                                                   GC024010
020800     02  ONEA-RDW.                                                GC024010
020900         05  ONEA-REC-LENG   PIC 9(4)    VALUE ZEROS     COMP.    GC024010
021000         05  ONEA-FEEDBACK   PIC 9(4)    VALUE ZEROS     COMP.    GC024010
021100     02  ONEA-REC-AREA.                                           GC024010
021200         COPY GCCONTRC.                                           GC024010
021300/                                                                 GC024010
021400 01  PARM-TWO.                                                    GC024010
021500     05  RESERVED-FLDS-2     PIC 9(8)    VALUE ZEROS     COMP.    GC024010
021600     05  RESERVED-X-2 REDEFINES RESERVED-FLDS-2.                  GC024010
021700         10  REQUEST-TYPE-2  PIC X.                               GC024010
021800         10  FILLER          PIC X(3).                            GC024010
021900                                                                  GC024010
022000 01  PARM-TWOA.                                                   GC024010
022100     02  TWOA-RDW.                                                GC024010
022200         05  TWOA-REC-LENG   PIC 9(4)    VALUE ZEROS     COMP.    GC024010
022300         05  TWOA-FEEDBACK   PIC 9(4)    VALUE ZEROS     COMP.    GC024010
022400     02  TWOA-REC-AREA       PIC X(31370).                        GC024010
022500     02  TWOA-ABM-REC-AREA REDEFINES TWOA-REC-AREA.               GC024010
022600         COPY GCTABMC                                             GC024010
ED0624         REPLACING  ==  1 TO 175  ==                              GC024010
ED0624                BY  ==       175  ==                              GC024010
022900                    ==  DEPENDING ON GAA-ENTRY-COUNT   ==         GC024010
023000                BY  ==                                 ==.        GC024010
023100*                                                                 GC024010
023200     02  TWOA-ACL-REC-AREA REDEFINES TWOA-REC-AREA.               GC024010
023300         COPY GCTACLC                                             GC024010
ED0624         REPLACING  ==  1 TO 175  ==                              GC024010
ED0624                BY  ==       175  ==                              GC024010
023600                    ==  DEPENDING ON GAB-ENTRY-COUNT   ==         GC024010
023700                BY  ==                                 ==.        GC024010
023800*                                                                 GC024010
023900     02  TWOA-ACP-REC-AREA REDEFINES TWOA-REC-AREA.               GC024010
024000         COPY GCTACPC                                             GC024010
ED0624         REPLACING  ==  1 TO 175  ==                              GC024010
ED0624                BY  ==       175  ==                              GC024010
024300                    ==  DEPENDING ON GAF-ENTRY-COUNT   ==         GC024010
024400                BY  ==                                 ==.        GC024010
024500*                                                                 GC024010
024600     02  TWOA-ADL-REC-AREA REDEFINES TWOA-REC-AREA.               GC024010
024700         COPY GCTADLC                                             GC024010
ED0624         REPLACING  ==  1 TO 175  ==                              GC024010
ED0624                BY  ==       175  ==                              GC024010
025000                    ==  DEPENDING ON GAC-ENTRY-COUNT   ==         GC024010
025100                BY  ==                                 ==.        GC024010
025200*                                                                 GC024010
025300     02  TWOA-AOL-REC-AREA REDEFINES TWOA-REC-AREA.               GC024010
025400         COPY GCTAOLC                                             GC024010
ED0624         REPLACING  ==  1 TO 175  ==                              GC024010
ED0624                BY  ==       175  ==                              GC024010
025700                    ==  DEPENDING ON GAD-ENTRY-COUNT   ==         GC024010
025800                BY  ==                                 ==.        GC024010
025900/                                                                 GC024010
026000 01  GC024030-CALL-AREA.                                          GC024010
026100     04  GC024030-IND          PIC X.                             GC024010
026200     04  GCDFILE-REC-FIXED     PIC X(660).                        GC024010
026300     04  GCDFILE-REC REDEFINES GCDFILE-REC-FIXED.                 GC024010
026400         COPY GCDFILEC                                            GC024010
026500           REPLACING  ==  1 TO 200 ==                             GC024010
026600                  BY  ==       200 ==                             GC024010
026700                      ==  DEPENDING ON DF-ERROR-COUNT    ==       GC024010
026800                  BY  ==                                 ==.      GC024010
026900/                                                                 GC024010
027000 LINKAGE SECTION.                                                 GC024010
027100                                                                  GC024010
027200 01  GC024010-IND              PIC X.                             GC024010
027300                                                                  GC024010
027400 01  LINK-CONTRACT-KEY.                                           GC024010
027500     10  L-CON-KEY.                                               GC024010
027600******************************************************************GC024010
027700** GROUP AND SECTION WILL NEED TO BE EXPANDED WHEN CONTRACT     **GC024010
027800** FILE RDW IS CHANGED.                                         **GC024010
027900******************************************************************GC024010
028000         15  LC-KEY-X-TYPE.                                       GC024010
028100             20  LC-PLAN-CODE         PIC X(3).                   GC024010
028200             20  LC-GROUP-NUM.                                    GC024010
028300                 25 LC-GRP-NO-1-3     PIC X(3).                   GC024010
028400                 25  LC-GRP-NO        PIC X(6).                   GC024010
028500             20  LC-SECTION-NUM.                                  GC024010
028600                 25 LC-SEC-NO-1       PIC X(1).                   GC024010
028700                 25 LC-SECTN-NO       PIC X(4).                   GC024010
028800             20  LC-PKG-CODE          PIC X(3).                   GC024010
028900             20  LC-L-O-B             PIC X.                      GC024010
029000             20  LC-PROVDR-CONTROL    PIC XX.                     GC024010
029100             20  LC-FAM-REL-LVL       PIC XX.                     GC024010
029200         15  LC-EFF-DATE.                                         GC024010
029300             20 LC-EFFDT-CC           PIC X.                      GC024010
029400             20  LC-EFF-DT            PIC S9(5)  COMP-3.          GC024010
029500         15  LC-EFFDT-CEN REDEFINES                               GC024010
029600              LC-EFF-DATE             PIC S9(7)  COMP-3.          GC024010
029700 01  LINK-OPERATOR-ID                 PIC X(08).                  GC024010
029800                                                                  GC024010
029900/                                                                 GC024010
030000 PROCEDURE DIVISION USING GC024010-IND LINK-CONTRACT-KEY          GC024010
030100                          LINK-OPERATOR-ID.                       GC024010
030200                                                                  GC024010
030300 0000-MAINLINE.                                                   GC024010
030400                                                                  GC024010
030500     IF  GC024010-IND = SPACES                                    GC024010
030600         PERFORM 0010-PROCESS THRU 0010-EXIT                      GC024010
030700         PERFORM 0200-PROCESS-DFILE THRU 0200-EXIT                GC024010
030800         GOBACK.                                                  GC024010
030900                                                                  GC024010
031000     IF  GC024010-IND = 'C'                                       GC024010
031100         PERFORM 9010-CLOSE-FILES THRU 9010-EXIT.                 GC024010
031200                                                                  GC024010
031300     GOBACK.                                                      GC024010
031400                                                                  GC024010
031500 0000-EXIT.                                                       GC024010
031600     EXIT.                                                        GC024010
031700/                                                                 GC024010
031800 0010-PROCESS.                                                    GC024010
031900*    CALL 'TCDTES' USING HSCDATES.                                GC024010
032000*    MOVE JYR                TO JUL-YY.                           GC024010
032100*    MOVE JDA                TO JUL-DD.                           GC024010
032200     MOVE LOW-VALUES TO GCDFILE-REC.                              GC024010
032300     MOVE ZEROS TO DF-ERROR-COUNT.                                GC024010
032400     MOVE 'NO ' TO ERROR-SW.                                      GC024010
032500     PERFORM 0140-READ-CONTRACT THRU 0140-EXIT.                   GC024010
032600     MOVE 'C' TO DF-RECORD-TYPE.                                  GC024010
032700     MOVE GCT-PLAN-CODE      TO DF-REC-PLAN-CODE.                 GC024010
032800     MOVE GCT-GROUP-NUM      TO DF-REC-GROUP.                     GC024010
032900     MOVE GCT-SECTION-NUM    TO DF-REC-SECTION.                   GC024010
033000     MOVE GCT-PKG-CODE       TO DF-REC-PKG-CODE.                  GC024010
033100     MOVE GCT-L-O-B          TO DF-REC-LOB.                       GC024010
033200     MOVE GCT-PROVDR-CONTROL TO DF-REC-PROV-CTL.                  GC024010
033300     MOVE GCT-FAM-REL-LVL    TO DF-REC-FAM-REL.                   GC024010
033400     MOVE GCT-EFFECTIVE-DATE TO DF-REC-EFFECTIVE-DATE.            GC024010
033500     MOVE LOW-VALUES         TO DF-REC-BEN-PROV-ID.               GC024010
033600     MOVE LINK-OPERATOR-ID   TO DF-OPERATOR-ID.                   GC024010
033700     SET DF-ERROR-INDEX TO 1.                                     GC024010
033800****                                                              GC024010
033900*  CONTRACT EDIT   1A.                                            GC024010
034000     IF GCT-COPAY-PERC-FOR-COPAY-IND = ZERO                       GC024010
034100         IF (GCT-COPAY-PERC-FOR-COPAY NUMERIC                     GC024010
034200           AND                                                    GC024010
034300            GCT-COPAY-PERC-FOR-COPAY = ZERO)                      GC024010
034400             NEXT SENTENCE                                        GC024010
034500         ELSE                                                     GC024010
034600             ADD 1 TO DF-ERROR-COUNT                              GC024010
034700             MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                GC024010
034800             MOVE 'C32' TO DF-ERR-CODE (DF-ERROR-INDEX)           GC024010
034900             SET DF-ERROR-INDEX UP BY 1                           GC024010
035000             MOVE 'YES' TO ERROR-SW                               GC024010
035100     ELSE                                                         GC024010
035200         IF (GCT-COPAY-PERC-FOR-COPAY NUMERIC                     GC024010
035300           AND                                                    GC024010
035400            GCT-COPAY-PERC-FOR-COPAY = ZERO)                      GC024010
035500              ADD 1 TO DF-ERROR-COUNT                             GC024010
035600              MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT               GC024010
035700              MOVE 'C31' TO DF-ERR-CODE (DF-ERROR-INDEX)          GC024010
035800              SET DF-ERROR-INDEX UP BY 1                          GC024010
035900              MOVE 'YES' TO ERROR-SW.                             GC024010
036000                                                                  GC024010
036100*  CONTRACT EDIT   1B.                                            GC024010
036200     IF GCT-DED-PERC-FOR-DED-IND = ZERO                           GC024010
036300         IF (GCT-DED-PERC-FOR-DED NUMERIC                         GC024010
036400           AND                                                    GC024010
036500            GCT-DED-PERC-FOR-DED = ZERO) AND                      GC024010
036600            GCT-DED-PRCT-FOR-FAM-DED = ZERO                       GC024010
036700             NEXT SENTENCE                                        GC024010
036800         ELSE                                                     GC024010
036900             ADD 1 TO DF-ERROR-COUNT                              GC024010
037000             MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                GC024010
037100             MOVE 'C07' TO DF-ERR-CODE (DF-ERROR-INDEX)           GC024010
037200             SET DF-ERROR-INDEX UP BY 1                           GC024010
037300             MOVE 'YES' TO ERROR-SW                               GC024010
037400     ELSE                                                         GC024010
037500         IF (GCT-DED-PERC-FOR-DED NUMERIC                         GC024010
037600           AND                                                    GC024010
037700            GCT-DED-PERC-FOR-DED = ZERO) AND                      GC024010
037800             GCT-DED-PRCT-FOR-FAM-DED = ZERO                      GC024010
037900              ADD 1 TO DF-ERROR-COUNT                             GC024010
038000              MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT               GC024010
038100              MOVE 'C01' TO DF-ERR-CODE (DF-ERROR-INDEX)          GC024010
038200              SET DF-ERROR-INDEX UP BY 1                          GC024010
038300              MOVE 'YES' TO ERROR-SW.                             GC024010
038400                                                                  GC024010
038500*  CONTRACT EDIT   2A.                                            GC024010
038600     IF GCT-FAM-DED-MULT-OF-INDIV-IND = ZERO                      GC024010
038700         IF GCT-FAM-DED-MULT-OF-INDIV = ZERO                      GC024010
038800             NEXT SENTENCE                                        GC024010
038900         ELSE                                                     GC024010
039000             ADD 1 TO DF-ERROR-COUNT                              GC024010
039100             MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                GC024010
039200             MOVE 'C08' TO DF-ERR-CODE (DF-ERROR-INDEX)           GC024010
039300             SET DF-ERROR-INDEX UP BY 1                           GC024010
039400             MOVE 'YES' TO ERROR-SW                               GC024010
039500     ELSE                                                         GC024010
039600         IF GCT-FAM-DED-MULT-OF-INDIV = ZERO                      GC024010
039700             ADD 1 TO DF-ERROR-COUNT                              GC024010
039800             MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                GC024010
039900             MOVE 'C02' TO DF-ERR-CODE (DF-ERROR-INDEX)           GC024010
040000             SET DF-ERROR-INDEX UP BY 1                           GC024010
040100             MOVE 'YES' TO ERROR-SW.                              GC024010
040200                                                                  GC024010
040300*  CONTRACT EDIT   3A.                                            GC024010
040400     IF GCT-OPX-PERC-IND = ZERO                                   GC024010
040500         IF (GCT-OPX-PERC-FOR-INDIV  = ZERO) AND                  GC024010
040600            (GCT-OPX-PERC-FOR-FAM    = ZERO)                      GC024010
040700             NEXT SENTENCE                                        GC024010
040800         ELSE                                                     GC024010
040900             ADD 1 TO DF-ERROR-COUNT                              GC024010
041000             MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                GC024010
041100             MOVE 'C09' TO DF-ERR-CODE (DF-ERROR-INDEX)           GC024010
041200             SET DF-ERROR-INDEX UP BY 1                           GC024010
041300             MOVE 'YES' TO ERROR-SW                               GC024010
041400     ELSE                                                         GC024010
041500         IF (GCT-OPX-PERC-FOR-INDIV  = ZERO) AND                  GC024010
041600            (GCT-OPX-PERC-FOR-FAM    = ZERO)                      GC024010
041700             ADD 1 TO DF-ERROR-COUNT                              GC024010
041800             MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                GC024010
041900             MOVE 'C03' TO DF-ERR-CODE (DF-ERROR-INDEX)           GC024010
042000             SET DF-ERROR-INDEX UP BY 1                           GC024010
042100             MOVE 'YES' TO ERROR-SW.                              GC024010
042200                                                                  GC024010
042300*  CONTRACT EDIT   4A.                                            GC024010
042400     IF GCT-OPX-OVR-PERC-IND = ZERO                               GC024010
042500         IF (GCT-OPX-OVR-PERC-FOR-INDIV = ZERO) AND               GC024010
042600            (GCT-OPX-OVR-PERC-FOR-FAM   = ZERO)                   GC024010
042700             NEXT SENTENCE                                        GC024010
042800         ELSE                                                     GC024010
042900             ADD 1 TO DF-ERROR-COUNT                              GC024010
043000             MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                GC024010
043100             MOVE 'C10' TO DF-ERR-CODE (DF-ERROR-INDEX)           GC024010
043200             SET DF-ERROR-INDEX UP BY 1                           GC024010
043300             MOVE 'YES' TO ERROR-SW                               GC024010
043400     ELSE                                                         GC024010
043500         IF (GCT-OPX-OVR-PERC-FOR-INDIV = ZERO) AND               GC024010
043600            (GCT-OPX-OVR-PERC-FOR-FAM   = ZERO)                   GC024010
043700             ADD 1 TO DF-ERROR-COUNT                              GC024010
043800             MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                GC024010
043900             MOVE 'C04' TO DF-ERR-CODE (DF-ERROR-INDEX)           GC024010
044000             SET DF-ERROR-INDEX UP BY 1                           GC024010
044100             MOVE 'YES' TO ERROR-SW.                              GC024010
044200                                                                  GC024010
044300*  CONTRACT EDIT   5A.                                            GC024010
044400     IF GCT-OPX-MULT-INDIV-OTH-SC-IND = ZERO                      GC024010
044500         IF GCT-OPX-MULT-OF-INDIV     = ZERO                      GC024010
044600             NEXT SENTENCE                                        GC024010
044700         ELSE                                                     GC024010
044800             ADD 1 TO DF-ERROR-COUNT                              GC024010
044900             MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                GC024010
045000             MOVE 'C11' TO DF-ERR-CODE (DF-ERROR-INDEX)           GC024010
045100             SET DF-ERROR-INDEX UP BY 1                           GC024010
045200             MOVE 'YES' TO ERROR-SW                               GC024010
045300     ELSE                                                         GC024010
045400         IF GCT-OPX-MULT-OF-INDIV     = ZERO                      GC024010
045500             ADD 1 TO DF-ERROR-COUNT                              GC024010
045600             MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                GC024010
045700             MOVE 'C05' TO DF-ERR-CODE (DF-ERROR-INDEX)           GC024010
045800             SET DF-ERROR-INDEX UP BY 1                           GC024010
045900             MOVE 'YES' TO ERROR-SW.                              GC024010
046000                                                                  GC024010
046100*******                                                           GC024010
046200*COMMENTED OUT FOR R0602.                                         GC024010
046300*  CONTRACT EDIT   6A.                                            GC024010
046400*    IF GCT-L-O-B NOT = '3'                                       GC024010
046500*        IF GCT-MM-TYPE-ADM-IND NOT = ZERO                        GC024010
046600*            ADD 1 TO DF-ERROR-COUNT                              GC024010
046700*            MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                GC024010
046800*            MOVE 'C06' TO DF-ERR-CODE (DF-ERROR-INDEX)           GC024010
046900*            SET DF-ERROR-INDEX UP BY 1                           GC024010
047000*            MOVE 'YES' TO ERROR-SW.                              GC024010
047100                                                                  GC024010
047200*******                                                           GC024010
047300*COMMENTED OUT FOR R0602.                                         GC024010
047400*  CONTRACT EDIT   7A.                                            GC024010
047500*    IF GCT-L-O-B = '3'                                           GC024010
047600*        IF GCT-MM-TYPE-ADM-IND    NOT EQUAL    ZERO              GC024010
047700*            NEXT SENTENCE                                        GC024010
047800*        ELSE                                                     GC024010
047900*            ADD 1 TO DF-ERROR-COUNT                              GC024010
048000*            MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                GC024010
048100*            MOVE 'C12' TO DF-ERR-CODE (DF-ERROR-INDEX)           GC024010
048200*            SET DF-ERROR-INDEX UP BY 1                           GC024010
048300*            MOVE 'YES' TO ERROR-SW.                              GC024010
048400*******                                                           GC024010
048500                                                                  GC024010
048600*******                                                           GC024010
048700*ADDED FOR D214.                                                  GC024010
048800*                                                                 GC024010
048900     IF GCT-CON-TAB-ID (2) = '#ABM  '                             GC024010
049000       AND                                                        GC024010
049100        GCT-CON-TAB-SLOT (2) > ZEROS                              GC024010
049200         PERFORM 1000-CHECK-ABM THRU 1000-EXIT.                   GC024010
049300     IF GCT-CON-TAB-ID (3) = '#ACL  '                             GC024010
049400       AND                                                        GC024010
049500        GCT-CON-TAB-SLOT (3) > ZEROS                              GC024010
049600         PERFORM 1100-CHECK-ACL THRU 1100-EXIT.                   GC024010
049700     IF GCT-CON-TAB-ID (6) = '#ACP  '                             GC024010
049800       AND                                                        GC024010
049900        GCT-CON-TAB-SLOT (6) > ZEROS                              GC024010
050000         PERFORM 1400-CHECK-ACP THRU 1400-EXIT.                   GC024010
050100     IF GCT-CON-TAB-ID (8) = '#ADL  '                             GC024010
050200       AND                                                        GC024010
050300        GCT-CON-TAB-SLOT (8) > ZEROS                              GC024010
050400         PERFORM 1200-CHECK-ADL THRU 1200-EXIT.                   GC024010
050500     IF GCT-CON-TAB-ID (10) = '#AOL  '                            GC024010
050600       AND                                                        GC024010
050700        GCT-CON-TAB-SLOT (10) > ZEROS                             GC024010
050800         PERFORM 1300-CHECK-AOL THRU 1300-EXIT.                   GC024010
050900                                                                  GC024010
051000*******                                                           GC024010
051100*ADDED FOR D365B.                                                 GC024010
051200*                                                                 GC024010
051300     IF (ACL-CAPI-FOUND AND ACP-CAPI-NOT-FD)  OR                  GC024010
051400        (ACP-CAPI-FOUND AND ACL-CAPI-NOT-FD)                      GC024010
051500          MOVE 'YES'          TO  ERROR-SW                        GC024010
051600          ADD 1               TO  DF-ERROR-COUNT                  GC024010
051700          MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT                  GC024010
051800          MOVE 'C34'          TO  DF-ERR-CODE (DF-ERROR-INDEX)    GC024010
051900          SET DF-ERROR-INDEX UP BY 1                              GC024010
052000     END-IF.                                                      GC024010
052100                                                                  GC024010
052200*                                                                 GC024010
052300*END OF EDITS FOR D365B.                                          GC024010
052400*******                                                           GC024010
052500                                                                  GC024010
052600*                                                                 GC024010
052700*ADDED FOR D329                  FRY 5/14/97                      GC024010
052800*                                                                 GC024010
052900     IF GCT-EXTEND-BENEFIT-PERIOD-YR    =  'NONE'                 GC024010
053000       IF WS-ABM-AF-CODED                                         GC024010
053100          MOVE 'NO '          TO  WS-ABM-AF-SWITCH                GC024010
053200          MOVE 'YES'          TO  ERROR-SW                        GC024010
053300          ADD 1               TO  DF-ERROR-COUNT                  GC024010
053400          MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT                  GC024010
053500          MOVE 'C24'          TO  DF-ERR-CODE (DF-ERROR-INDEX)    GC024010
053600          SET DF-ERROR-INDEX UP BY 1                              GC024010
053700       ELSE                                                       GC024010
053800       IF WS-ACL-AF-CODED                                         GC024010
053900          MOVE 'NO '          TO  WS-ACL-AF-SWITCH                GC024010
054000          MOVE 'YES'          TO  ERROR-SW                        GC024010
054100          ADD 1               TO  DF-ERROR-COUNT                  GC024010
054200          MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT                  GC024010
054300          MOVE 'C25'          TO  DF-ERR-CODE (DF-ERROR-INDEX)    GC024010
054400          SET DF-ERROR-INDEX UP BY 1                              GC024010
054500       ELSE                                                       GC024010
054600       IF WS-ACP-AF-CODED                                         GC024010
054700          MOVE 'NO '          TO  WS-ACP-AF-SWITCH                GC024010
054800          MOVE 'YES'          TO  ERROR-SW                        GC024010
054900          ADD 1               TO  DF-ERROR-COUNT                  GC024010
055000          MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT                  GC024010
055100          MOVE 'C28'          TO  DF-ERR-CODE (DF-ERROR-INDEX)    GC024010
055200          SET DF-ERROR-INDEX UP BY 1                              GC024010
055300       ELSE                                                       GC024010
055400       IF WS-ADL-AF-CODED                                         GC024010
055500          MOVE 'NO '          TO  WS-ADL-AF-SWITCH                GC024010
055600          MOVE 'YES'          TO  ERROR-SW                        GC024010
055700          ADD 1               TO  DF-ERROR-COUNT                  GC024010
055800          MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT                  GC024010
055900          MOVE 'C26'          TO  DF-ERR-CODE (DF-ERROR-INDEX)    GC024010
056000          SET DF-ERROR-INDEX UP BY 1                              GC024010
056100       ELSE                                                       GC024010
056200       IF WS-AOL-AF-CODED                                         GC024010
056300          MOVE 'NO '          TO  WS-AOL-AF-SWITCH                GC024010
056400          MOVE 'YES'          TO  ERROR-SW                        GC024010
056500          ADD 1               TO  DF-ERROR-COUNT                  GC024010
056600          MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT                  GC024010
056700          MOVE 'C27'          TO  DF-ERR-CODE (DF-ERROR-INDEX)    GC024010
056800          SET DF-ERROR-INDEX UP BY 1.                             GC024010
056900                                                                  GC024010
057000*******                                                           GC024010
057100*ADDED FOR D215.                                                  GC024010
057200*                                                                 GC024010
057300     IF GCT-INTER-REL-CD = ZEROS                                  GC024010
057400         ADD 1 TO DF-ERROR-COUNT                                  GC024010
057500         MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                    GC024010
057600         MOVE 'C19' TO DF-ERR-CODE (DF-ERROR-INDEX)               GC024010
057700         SET DF-ERROR-INDEX UP BY 1                               GC024010
057800         MOVE 'YES' TO ERROR-SW.                                  GC024010
057900                                                                  GC024010
058000     IF GCT-EFFDT-CEN > GCT-TERMDT-CEN                            GC024010
058100         ADD 1 TO DF-ERROR-COUNT                                  GC024010
058200         MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                    GC024010
058300         MOVE 'C20' TO DF-ERR-CODE (DF-ERROR-INDEX)               GC024010
058400         SET DF-ERROR-INDEX UP BY 1                               GC024010
058500         MOVE 'YES' TO ERROR-SW.                                  GC024010
058600                                                                  GC024010
058700*  ADDED 04/08/91 PFH                                             GC024010
058800     IF GCT-PRODUCT-TYPE = ZEROS                                  GC024010
058900         IF GCT-PRODUCT-TYPE-IND = ZERO                           GC024010
059000             NEXT SENTENCE                                        GC024010
059100         ELSE                                                     GC024010
059200             ADD 1 TO DF-ERROR-COUNT                              GC024010
059300             MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                GC024010
059400             MOVE 'C23' TO DF-ERR-CODE (DF-ERROR-INDEX)           GC024010
059500             SET DF-ERROR-INDEX UP BY 1                           GC024010
059600             MOVE 'YES' TO ERROR-SW.                              GC024010
059700                                                                  GC024010
059800 0010-EXIT.                                                       GC024010
059900     EXIT.                                                        GC024010
060000/                                                                 GC024010
060100 0140-READ-CONTRACT.                                              GC024010
060200     MOVE LINK-CONTRACT-KEY TO GCT-CONTRACT-ID.                   GC024010
060300     MOVE 33  TO ONEA-REC-LENG.                                   GC024010
060400     MOVE 'R' TO REQUEST-TYPE-1.                                  GC024010
060500     CALL 'TSGVSAM1' USING PARM-ONE PARM-ONEA.                    GC024010
060600     IF REQUEST-TYPE-1 = '3'                                      GC024010
060700         DISPLAY 'GC024010--CONTRACT RECORD NOT FOUND'            GC024010
060800         DISPLAY 'KEY = ' LC-KEY-X-TYPE LC-EFF-DT                 GC024010
060900         GO TO 0140-EXIT.                                         GC024010
061000     IF  REQUEST-TYPE-1 NOT = 'R'                                 GC024010
061100         MOVE '0140--BAD READ             ' TO VSAM-WS-ERR-MSG    GC024010
061200         MOVE ONEA-FEEDBACK  TO ABEND-CODE                        GC024010
061300         DISPLAY 'GC024010-BAD READ TSGVSAM1 ' LINK-CONTRACT-KEY  GC024010
061400         GO TO 9999-ERROR-RTN.                                    GC024010
061500 0140-EXIT.                                                       GC024010
061600     EXIT.                                                        GC024010
061700/                                                                 GC024010
061800 0200-PROCESS-DFILE.                                              GC024010
061900     ADD 1 TO DF-ERROR-COUNT.                                     GC024010
062000     MOVE DF-ERROR-COUNT  TO DF-ERROR-COUNT.                      GC024010
062100     MOVE HIGH-VALUES TO DF-ERR-CODE (DF-ERROR-INDEX).            GC024010
062200     IF ERROR-FOUND                                               GC024010
062300         MOVE 'E' TO GC024030-IND                                 GC024010
062400     ELSE                                                         GC024010
062500         MOVE 'G' TO GC024030-IND.                                GC024010
062600     CALL 'GC024030' USING GC024030-CALL-AREA.                    GC024010
062700 0200-EXIT.                                                       GC024010
062800     EXIT.                                                        GC024010
062900/                                                                 GC024010
063000 0300-READ-TABULAR.                                               GC024010
063100     MOVE 14  TO TWOA-REC-LENG.                                   GC024010
063200     MOVE 'R' TO REQUEST-TYPE-2.                                  GC024010
063300     CALL 'TSGVSAM4' USING PARM-TWO PARM-TWOA.                    GC024010
063400     IF REQUEST-TYPE-2 NOT = 'R'                                  GC024010
063500         DISPLAY 'GC024010 ABENDING-- '                           GC024010
063600                 'BAD READ TABULAR FILE    '                      GC024010
063700         DISPLAY GAA-TABULAR-PROVISION-ID ' FOR CONTRCT '         GC024010
063800                      GCT-CONTRACT-ID                             GC024010
063900         MOVE TWOA-FEEDBACK  TO ABEND-CODE                        GC024010
064000         GO TO 9999-ERROR-RTN.                                    GC024010
064100 0300-EXIT.                                                       GC024010
064200     EXIT.                                                        GC024010
064300/------------------ E D I T  A B M  T A B L E ------------------- GC024010
064400 1000-CHECK-ABM.                                                  GC024010
064500                                                                  GC024010
064600     MOVE GCT-CON-TAB-ID-SLOT (2) TO GAA-TABULAR-PROVISION-ID.    GC024010
064700     PERFORM 0300-READ-TABULAR THRU 0300-EXIT.                    GC024010
064800     PERFORM 1010-COMPARE-ABM-OCCURS THRU 1010-EXIT               GC024010
064900       VARYING GAA-INDEX FROM 1 BY 1                              GC024010
065000       UNTIL   GAA-INDEX = GAA-ENTRY-COUNT.                       GC024010
065100                                                                  GC024010
065200*--- ISSR 11154, ALL ACCUM TABULAR MUST GO TO THE LOGICAL ------- GC024010
065300*--- EDIT MODULE GC024015 FOR AGE-LIMIT FIELDS EDIT.      ------- GC024010
065400*********************************************************         GC024010
065500* THE DF-ERROR-COUNT IS STORED TO DETERMINE WHETHER THERE         GC024010
065600* WERE ANY ERRORS ENCOUNTERED IN THE ACCUM EDIT MODULE.           GC024010
065700**                                                                GC024010
065800     MOVE DF-ERROR-COUNT TO WS-DF-ERROR-COUNT.                    GC024010
065900                                                                  GC024010
066000     CALL WS-GC024015 USING GC024030-CALL-AREA                    GC024010
066100                            TWOA-REC-AREA.                        GC024010
066200                                                                  GC024010
066300*********************************************************         GC024010
066400* THE DF-ERROR-INDEX MUST BE RESET UPON RETURNING FROM            GC024010
066500* THE ACCUM EDIT MODULE BECAUSE IT ISN'T PASSED BETWEEN           GC024010
066600* PROGRAMS.                                                       GC024010
066700**                                                                GC024010
066800     SET DF-ERROR-INDEX TO DF-ERROR-COUNT.                        GC024010
066900     SET DF-ERROR-INDEX UP BY 1.                                  GC024010
067000                                                                  GC024010
067100     IF DF-ERROR-COUNT > WS-DF-ERROR-COUNT                        GC024010
067200         MOVE 'YES' TO ERROR-SW.                                  GC024010
067300                                                                  GC024010
067400 1000-EXIT.                                                       GC024010
067500     EXIT.                                                        GC024010
067600                                                                  GC024010
067700 1010-COMPARE-ABM-OCCURS.                                         GC024010
067800                                                                  GC024010
067900     IF WS-ABM-L-O-B-ERROR                                        GC024010
068000        NEXT SENTENCE                                             GC024010
068100     ELSE                                                         GC024010
068200       IF GAA-BAMA-L-O-B (GAA-INDEX) NOT = GCT-L-O-B              GC024010
068300           MOVE 'YES'          TO  WS-ABM-L-O-B-SWITCH            GC024010
068400                                   ERROR-SW                       GC024010
068500           ADD 1               TO  DF-ERROR-COUNT                 GC024010
068600           MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT                 GC024010
068700           MOVE 'C13'          TO  DF-ERR-CODE (DF-ERROR-INDEX)   GC024010
068800           SET DF-ERROR-INDEX UP BY 1.                            GC024010
068900                                                                  GC024010
069000     IF WS-ABM-AF-CODED                                           GC024010
069100        NEXT SENTENCE                                             GC024010
069200     ELSE                                                         GC024010
069300     IF GAA-BAMA-BENEFIT-PERIOD (GAA-INDEX)    =   'AF'           GC024010
069400       IF GCT-EXTEND-BENEFIT-PERIOD-YR         =  'NONE'          GC024010
069500          MOVE 'YES'   TO  WS-ABM-AF-SWITCH                       GC024010
069600                           WS-GCT-EXTEND-BENPER-YR-ER-SW.         GC024010
069700                                                                  GC024010
069800                                                                  GC024010
069900     IF WS-ABM-L-O-B-ERROR                                        GC024010
070000       IF WS-ABM-AF-CODED                                         GC024010
070100          SET GAA-INDEX TO GAA-ENTRY-COUNT                        GC024010
070200          SET GAA-INDEX DOWN BY 1.                                GC024010
070300                                                                  GC024010
070400 1010-EXIT.                                                       GC024010
070500     EXIT.                                                        GC024010
070600/------------------ E D I T  A C L  T A B L E ------------------- GC024010
070700 1100-CHECK-ACL.                                                  GC024010
070800     MOVE GCT-CON-TAB-ID-SLOT (3) TO GAB-TABULAR-PROVISION-ID.    GC024010
070900     PERFORM 0300-READ-TABULAR THRU 0300-EXIT.                    GC024010
071000     PERFORM 1110-COMPARE-ACL-OCCURS THRU 1110-EXIT               GC024010
071100       VARYING GAB-INDEX FROM 1 BY 1                              GC024010
071200       UNTIL   GAB-INDEX = GAB-ENTRY-COUNT.                       GC024010
071300                                                                  GC024010
071400*--- D15182 CHECK COINSURANCE                                     GC024010
071500     PERFORM 1120-CHECK-COINSURANCE    THRU 1120-EXIT             GC024010
071600       VARYING GAB-INDEX FROM 1 BY 1                              GC024010
071700       UNTIL   GAB-INDEX = GAB-ENTRY-COUNT.                       GC024010
071800                                                                  GC024010
071900                                                                  GC024010
072000*--- D365B  CHECK ACL CAPI                                        GC024010
072100     MOVE 'N' TO WS-ACL-CAPI-FOUND-SW                             GC024010
072200     PERFORM 1130-CHECK-ACL-CAPI       THRU 1130-EXIT             GC024010
072300       VARYING GAB-INDEX FROM 1 BY 1                              GC024010
072400       UNTIL   GAB-INDEX = GAB-ENTRY-COUNT.                       GC024010
072500                                                                  GC024010
072600*--- ISSR 11154, ALL ACCUM TABULAR MUST GO TO THE LOGICAL ------- GC024010
072700*--- EDIT MODULE GC024015 FOR AGE-LIMIT FIELDS EDIT.      ------- GC024010
072800*********************************************************         GC024010
072900* THE DF-ERROR-COUNT IS STORED TO DETERMINE WHETHER THERE         GC024010
073000* WERE ANY ERRORS ENCOUNTERED IN THE ACCUM EDIT MODULE.           GC024010
073100**                                                                GC024010
073200     MOVE DF-ERROR-COUNT TO WS-DF-ERROR-COUNT.                    GC024010
073300                                                                  GC024010
073400     CALL WS-GC024015 USING GC024030-CALL-AREA                    GC024010
073500                            TWOA-REC-AREA.                        GC024010
073600                                                                  GC024010
073700*********************************************************         GC024010
073800* THE DF-ERROR-INDEX MUST BE RESET UPON RETURNING FROM            GC024010
073900* THE ACCUM EDIT MODULE BECAUSE IT ISN'T PASSED BETWEEN           GC024010
074000* PROGRAMS.                                                       GC024010
074100**                                                                GC024010
074200     SET DF-ERROR-INDEX TO DF-ERROR-COUNT.                        GC024010
074300     SET DF-ERROR-INDEX UP BY 1.                                  GC024010
074400                                                                  GC024010
074500     IF DF-ERROR-COUNT > WS-DF-ERROR-COUNT                        GC024010
074600         MOVE 'YES' TO ERROR-SW.                                  GC024010
074700                                                                  GC024010
074800 1100-EXIT.                                                       GC024010
074900     EXIT.                                                        GC024010
075000                                                                  GC024010
075100 1110-COMPARE-ACL-OCCURS.                                         GC024010
075200                                                                  GC024010
075300     IF WS-ACL-L-O-B-ERROR                                        GC024010
075400        NEXT SENTENCE                                             GC024010
075500     ELSE                                                         GC024010
075600       IF GAB-COINS-L-O-B (GAB-INDEX) NOT = GCT-L-O-B             GC024010
075700           MOVE 'YES'          TO  WS-ACL-L-O-B-SWITCH            GC024010
075800                                   ERROR-SW                       GC024010
075900           ADD 1               TO  DF-ERROR-COUNT                 GC024010
076000           MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT                 GC024010
076100           MOVE 'C14'          TO  DF-ERR-CODE (DF-ERROR-INDEX)   GC024010
076200           SET DF-ERROR-INDEX UP BY 1.                            GC024010
076300                                                                  GC024010
076400     IF WS-ACL-L-O-B-ERROR                                        GC024010
076500       IF WS-GCT-EXTEND-BENPER-YR-ERROR                           GC024010
076600          SET GAB-INDEX TO GAB-ENTRY-COUNT                        GC024010
076700          SET GAB-INDEX DOWN BY 1                                 GC024010
076800          GO TO 1110-EXIT.                                        GC024010
076900                                                                  GC024010
077000     IF WS-GCT-EXTEND-BENPER-YR-ERROR                             GC024010
077100          GO TO 1110-EXIT.                                        GC024010
077200                                                                  GC024010
077300     IF GAB-COINS-BENEFIT-PERIOD (GAB-INDEX)   =   'AF'           GC024010
077400       IF GCT-EXTEND-BENEFIT-PERIOD-YR         =  'NONE'          GC024010
077500          MOVE 'YES'   TO  WS-ACL-AF-SWITCH                       GC024010
077600                           WS-GCT-EXTEND-BENPER-YR-ER-SW.         GC024010
077700                                                                  GC024010
077800                                                                  GC024010
077900     IF WS-ACL-L-O-B-ERROR                                        GC024010
078000       IF WS-ACL-AF-CODED                                         GC024010
078100          SET GAB-INDEX TO GAB-ENTRY-COUNT                        GC024010
078200          SET GAB-INDEX DOWN BY 1.                                GC024010
078300                                                                  GC024010
078400 1110-EXIT.                                                       GC024010
078500     EXIT.                                                        GC024010
078600                                                                  GC024010
078700 1120-CHECK-COINSURANCE.                                          GC024010
078800*-- COINS LIMIT VALUES C1 THRU C5 VALID ONLY IF #ACP EXIST.       GC024010
078900*-- FIRST $ COVERAGE VALUES A VALID ONLY IF #ACP EXIST.           GC024010
079000                                                                  GC024010
079100     IF GAB-COINS-1ST-DOLR-COVRGE-LMT (GAB-INDEX)  = 'A'          GC024010
079200        IF GCT-CON-TAB-ID (6) = '#ACP  '                          GC024010
079300          AND                                                     GC024010
079400           GCT-CON-TAB-SLOT (6) > ZEROS                           GC024010
079500             NEXT SENTENCE                                        GC024010
079600        ELSE                                                      GC024010
079700             MOVE 'YES'          TO  ERROR-SW                     GC024010
079800             ADD 1               TO  DF-ERROR-COUNT               GC024010
079900             MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT               GC024010
080000             MOVE 'A28'          TO  DF-ERR-CODE (DF-ERROR-INDEX) GC024010
080100             SET DF-ERROR-INDEX UP BY 1.                          GC024010
080200*                                                                 GC024010
080300     MOVE GAB-COINS-DEFINITION   (GAB-INDEX)                      GC024010
080400     TO   WS-COINS-LIM.                                           GC024010
080500                                                                  GC024010
080600     IF SELECTED-COINS                                            GC024010
080700        IF GCT-CON-TAB-ID (6) = '#ACP  '                          GC024010
080800          AND                                                     GC024010
080900           GCT-CON-TAB-SLOT (6) > ZEROS                           GC024010
081000             NEXT SENTENCE                                        GC024010
081100        ELSE                                                      GC024010
081200             MOVE 'YES'          TO  ERROR-SW                     GC024010
081300             ADD 1               TO  DF-ERROR-COUNT               GC024010
081400             MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT               GC024010
081500             MOVE 'A29'          TO  DF-ERR-CODE (DF-ERROR-INDEX) GC024010
081600             SET DF-ERROR-INDEX UP BY 1.                          GC024010
081700                                                                  GC024010
081800 1120-EXIT.                                                       GC024010
081900     EXIT.                                                        GC024010
082000                                                                  GC024010
082100 1130-CHECK-ACL-CAPI.                                             GC024010
082200                                                                  GC024010
082300*-- IF CAPI CODED, SET SWITCH TO Y (ACP CAPI MUST BE CODED TOO)   GC024010
082400*-- IF CAPI IS CODED, 1ST DOLLAR COVERAGE MUST = A                GC024010
082500                                                                  GC024010
082600     IF GAB-COINS-COMB-APPLIED-IND (GAB-INDEX)  = '01'            GC024010
082700        MOVE 'Y' TO WS-ACL-CAPI-FOUND-SW                          GC024010
082800        IF GAB-COINS-1ST-DOLR-COVRGE-LMT (GAB-INDEX) NOT = 'A'    GC024010
082900             MOVE 'YES'          TO  ERROR-SW                     GC024010
083000             ADD 1               TO  DF-ERROR-COUNT               GC024010
083100             MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT               GC024010
083200             MOVE 'C33'          TO  DF-ERR-CODE (DF-ERROR-INDEX) GC024010
083300             SET DF-ERROR-INDEX UP BY 1                           GC024010
083400        END-IF                                                    GC024010
083500     END-IF.                                                      GC024010
083600                                                                  GC024010
083700                                                                  GC024010
083800 1130-EXIT.                                                       GC024010
083900     EXIT.                                                        GC024010
084000/------------------ E D I T  A D L  T A B L E ------------------- GC024010
084100 1200-CHECK-ADL.                                                  GC024010
084200     MOVE GCT-CON-TAB-ID-SLOT (8) TO GAC-TABULAR-PROVISION-ID.    GC024010
084300     PERFORM 0300-READ-TABULAR THRU 0300-EXIT.                    GC024010
084400     PERFORM 1210-COMPARE-ADL-OCCURS THRU 1210-EXIT               GC024010
084500       VARYING GAC-INDEX FROM 1 BY 1                              GC024010
084600       UNTIL   GAC-INDEX = GAC-ENTRY-COUNT.                       GC024010
084700                                                                  GC024010
084800     IF GCT-CARRY-OVER-CREDIT-IND = '0'                           GC024010
084900         PERFORM 1220-COMPARE-ADL-OCCURS-2 THRU 1220-EXIT         GC024010
085000           VARYING GAC-INDEX FROM 1 BY 1                          GC024010
085100           UNTIL   GAC-INDEX = GAC-ENTRY-COUNT                    GC024010
085200     ELSE                                                         GC024010
085300         MOVE +0    TO WS-1230-EDIT-CNT                           GC024010
085400         PERFORM 1230-COMPARE-ADL-OCCURS-3 THRU 1230-EXIT         GC024010
085500           VARYING GAC-INDEX FROM 1 BY 1                          GC024010
085600           UNTIL   GAC-INDEX = GAC-ENTRY-COUNT                    GC024010
085700         IF WS-1230-EDIT-CNT = +0                                 GC024010
085800             ADD 1 TO DF-ERROR-COUNT                              GC024010
085900             MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                GC024010
086000             MOVE 'C22' TO DF-ERR-CODE (DF-ERROR-INDEX)           GC024010
086100             SET DF-ERROR-INDEX UP BY 1                           GC024010
086200             MOVE 'YES' TO ERROR-SW.                              GC024010
086300                                                                  GC024010
086400*--- D15182 CHECK DEDUCTIBLE DEFINITION                           GC024010
086500     PERFORM 1240-CHECK-DEFINITION     THRU 1240-EXIT             GC024010
086600       VARYING GAC-INDEX FROM 1 BY 1                              GC024010
086700       UNTIL   GAC-INDEX = GAC-ENTRY-COUNT.                       GC024010
086800                                                                  GC024010
086900*--- ISSR 11154, ALL ACCUM TABULAR MUST GO TO THE LOGICAL ------- GC024010
087000*--- EDIT MODULE GC024015 FOR AGE-LIMIT FIELDS EDIT.      ------- GC024010
087100*********************************************************         GC024010
087200* THE DF-ERROR-COUNT IS STORED TO DETERMINE WHETHER THERE         GC024010
087300* WERE ANY ERRORS ENCOUNTERED IN THE ACCUM EDIT MODULE.           GC024010
087400**                                                                GC024010
087500     MOVE DF-ERROR-COUNT TO WS-DF-ERROR-COUNT.                    GC024010
087600                                                                  GC024010
087700     CALL WS-GC024015 USING GC024030-CALL-AREA                    GC024010
087800                            TWOA-REC-AREA.                        GC024010
087900                                                                  GC024010
088000*********************************************************         GC024010
088100* THE DF-ERROR-INDEX MUST BE RESET UPON RETURNING FROM            GC024010
088200* THE ACCUM EDIT MODULE BECAUSE IT ISN'T PASSED BETWEEN           GC024010
088300* PROGRAMS.                                                       GC024010
088400**                                                                GC024010
088500     SET DF-ERROR-INDEX TO DF-ERROR-COUNT.                        GC024010
088600     SET DF-ERROR-INDEX UP BY 1.                                  GC024010
088700                                                                  GC024010
088800     IF DF-ERROR-COUNT > WS-DF-ERROR-COUNT                        GC024010
088900         MOVE 'YES' TO ERROR-SW.                                  GC024010
089000                                                                  GC024010
089100 1200-EXIT.                                                       GC024010
089200     EXIT.                                                        GC024010
089300 1210-COMPARE-ADL-OCCURS.                                         GC024010
089400                                                                  GC024010
089500     IF WS-ADL-L-O-B-ERROR                                        GC024010
089600        NEXT SENTENCE                                             GC024010
089700     ELSE                                                         GC024010
089800       IF GAC-DEDL-L-O-B (GAC-INDEX) NOT = GCT-L-O-B              GC024010
089900           MOVE 'YES'          TO  WS-ADL-L-O-B-SWITCH            GC024010
090000                                   ERROR-SW                       GC024010
090100           ADD 1               TO  DF-ERROR-COUNT                 GC024010
090200           MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT                 GC024010
090300           MOVE 'C15'          TO  DF-ERR-CODE (DF-ERROR-INDEX)   GC024010
090400           SET DF-ERROR-INDEX UP BY 1.                            GC024010
090500                                                                  GC024010
090600     IF WS-ADL-L-O-B-ERROR                                        GC024010
090700       IF WS-GCT-EXTEND-BENPER-YR-ERROR                           GC024010
090800          SET GAC-INDEX TO GAC-ENTRY-COUNT                        GC024010
090900          SET GAC-INDEX DOWN BY 1                                 GC024010
091000          GO TO 1210-EXIT.                                        GC024010
091100                                                                  GC024010
091200     IF WS-GCT-EXTEND-BENPER-YR-ERROR                             GC024010
091300          GO TO 1210-EXIT.                                        GC024010
091400                                                                  GC024010
091500     IF GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX)  =   'AF'             GC024010
091600       IF GCT-EXTEND-BENEFIT-PERIOD-YR       =  'NONE'            GC024010
091700          MOVE 'YES'   TO  WS-ADL-AF-SWITCH                       GC024010
091800                           WS-GCT-EXTEND-BENPER-YR-ER-SW.         GC024010
091900                                                                  GC024010
092000                                                                  GC024010
092100     IF WS-ADL-L-O-B-ERROR                                        GC024010
092200       IF WS-ADL-AF-CODED                                         GC024010
092300          SET GAC-INDEX TO GAC-ENTRY-COUNT                        GC024010
092400          SET GAC-INDEX DOWN BY 1.                                GC024010
092500                                                                  GC024010
092600 1210-EXIT.                                                       GC024010
092700     EXIT.                                                        GC024010
092800                                                                  GC024010
092900 1220-COMPARE-ADL-OCCURS-2.                                       GC024010
093000* IF THE CONTRACT CARRY OVER CREDIT IND = ZERO                    GC024010
093100*   ALL GAC-CARRY-OVER-CREDIT-IND MUST  = ZERO.                   GC024010
093200     IF GAC-CARRY-OVER-CREDIT-IND (GAC-INDEX) NOT = '0'           GC024010
093300         ADD 1 TO DF-ERROR-COUNT                                  GC024010
093400         MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                    GC024010
093500         MOVE 'C22' TO DF-ERR-CODE (DF-ERROR-INDEX)               GC024010
093600         SET DF-ERROR-INDEX UP BY 1                               GC024010
093700         MOVE 'YES' TO ERROR-SW                                   GC024010
093800         SET GAC-INDEX TO GAC-ENTRY-COUNT                         GC024010
093900         SET GAC-INDEX DOWN BY 1.                                 GC024010
094000 1220-EXIT.                                                       GC024010
094100     EXIT.                                                        GC024010
094200/                                                                 GC024010
094300 1230-COMPARE-ADL-OCCURS-3.                                       GC024010
094400* IF THE CONTRACT CARRY OVER CREDIT IND IS NOT = ZERO             GC024010
094500*   AT LEAST ONE GAC-CARRY-OVER-CREDIT-IND MUST NOT BE ZERO.      GC024010
094600     IF GAC-CARRY-OVER-CREDIT-IND (GAC-INDEX) NOT = '0'           GC024010
094700         MOVE +1 TO WS-1230-EDIT-CNT                              GC024010
094800         SET GAC-INDEX TO GAC-ENTRY-COUNT                         GC024010
094900         SET GAC-INDEX DOWN BY 1.                                 GC024010
095000 1230-EXIT.                                                       GC024010
095100     EXIT.                                                        GC024010
095200                                                                  GC024010
095300 1240-CHECK-DEFINITION.                                           GC024010
095400*-- VALUES 10 THRU 13 VALID ONLY IF #ACP EXIST.                   GC024010
095500                                                                  GC024010
095600     MOVE GAF-COPAY-DEFINITION (GAC-INDEX)                        GC024010
095700     TO   WS-DED-DEF.                                             GC024010
095800                                                                  GC024010
095900     IF SELECTED-VALUE                                            GC024010
096000        IF GCT-CON-TAB-ID (6) = '#ACP  '                          GC024010
096100          AND                                                     GC024010
096200           GCT-CON-TAB-SLOT (6) > ZEROS                           GC024010
096300             NEXT SENTENCE                                        GC024010
096400        ELSE                                                      GC024010
096500             MOVE 'YES'          TO  ERROR-SW                     GC024010
096600             ADD 1               TO  DF-ERROR-COUNT               GC024010
096700             MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT               GC024010
096800             MOVE 'A27'          TO  DF-ERR-CODE (DF-ERROR-INDEX) GC024010
096900              SET DF-ERROR-INDEX UP BY 1.                         GC024010
097000                                                                  GC024010
097100 1240-EXIT.                                                       GC024010
097200     EXIT.                                                        GC024010
097300/------------------ E D I T  A O L  T A B L E ------------------- GC024010
097400 1300-CHECK-AOL.                                                  GC024010
097500                                                                  GC024010
097600     MOVE GCT-CON-TAB-ID-SLOT (10) TO GAD-TABULAR-PROVISION-ID.   GC024010
097700     PERFORM 0300-READ-TABULAR THRU 0300-EXIT.                    GC024010
097800     PERFORM 1310-COMPARE-AOL-OCCURS THRU 1310-EXIT               GC024010
097900       VARYING GAD-INDEX FROM 1 BY 1                              GC024010
098000       UNTIL   GAD-INDEX = GAD-ENTRY-COUNT.                       GC024010
098100                                                                  GC024010
098200*--- D15182 OUT OF POCKET                                         GC024010
098300     PERFORM 1320-CHECK-OPX    THRU 1320-EXIT                     GC024010
098400       VARYING GAD-INDEX FROM 1 BY 1                              GC024010
098500       UNTIL   GAD-INDEX = GAB-ENTRY-COUNT.                       GC024010
098600                                                                  GC024010
098700*--- ISSR 11154, ALL ACCUM TABULAR MUST GO TO THE LOGICAL ------- GC024010
098800*--- EDIT MODULE GC024015 FOR AGE-LIMIT FIELDS EDIT.      ------- GC024010
098900*********************************************************         GC024010
099000* THE DF-ERROR-COUNT IS STORED TO DETERMINE WHETHER THERE         GC024010
099100* WERE ANY ERRORS ENCOUNTERED IN THE ACCUM EDIT MODULE.           GC024010
099200**                                                                GC024010
099300     MOVE DF-ERROR-COUNT TO WS-DF-ERROR-COUNT.                    GC024010
099400                                                                  GC024010
099500     CALL WS-GC024015 USING GC024030-CALL-AREA                    GC024010
099600                            TWOA-REC-AREA.                        GC024010
099700                                                                  GC024010
099800*********************************************************         GC024010
099900* THE DF-ERROR-INDEX MUST BE RESET UPON RETURNING FROM            GC024010
100000* THE ACCUM EDIT MODULE BECAUSE IT ISN'T PASSED BETWEEN           GC024010
100100* PROGRAMS.                                                       GC024010
100200**                                                                GC024010
100300     SET DF-ERROR-INDEX TO DF-ERROR-COUNT.                        GC024010
100400     SET DF-ERROR-INDEX UP BY 1.                                  GC024010
100500                                                                  GC024010
100600     IF DF-ERROR-COUNT > WS-DF-ERROR-COUNT                        GC024010
100700         MOVE 'YES' TO ERROR-SW.                                  GC024010
100800                                                                  GC024010
100900 1300-EXIT.                                                       GC024010
101000     EXIT.                                                        GC024010
101100                                                                  GC024010
101200 1310-COMPARE-AOL-OCCURS.                                         GC024010
101300                                                                  GC024010
101400     IF WS-AOL-L-O-B-ERROR                                        GC024010
101500        NEXT SENTENCE                                             GC024010
101600     ELSE                                                         GC024010
101700       IF GAD-O-P-X-L-O-B (GAD-INDEX) NOT = GCT-L-O-B             GC024010
101800           MOVE 'YES'          TO  WS-AOL-L-O-B-SWITCH            GC024010
101900                                   ERROR-SW                       GC024010
102000           ADD 1               TO  DF-ERROR-COUNT                 GC024010
102100           MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT                 GC024010
102200           MOVE 'C16'          TO  DF-ERR-CODE (DF-ERROR-INDEX)   GC024010
102300           SET DF-ERROR-INDEX UP BY 1.                            GC024010
102400                                                                  GC024010
102500     IF WS-AOL-L-O-B-ERROR                                        GC024010
102600       IF WS-GCT-EXTEND-BENPER-YR-ERROR                           GC024010
102700          SET GAD-INDEX TO GAD-ENTRY-COUNT                        GC024010
102800          SET GAD-INDEX DOWN BY 1                                 GC024010
102900          GO TO 1310-EXIT.                                        GC024010
103000                                                                  GC024010
103100     IF WS-GCT-EXTEND-BENPER-YR-ERROR                             GC024010
103200          GO TO 1310-EXIT.                                        GC024010
103300                                                                  GC024010
103400     IF GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX)   =   'AF'           GC024010
103500       IF GCT-EXTEND-BENEFIT-PERIOD-YR         =  'NONE'          GC024010
103600          MOVE 'YES'   TO  WS-AOL-AF-SWITCH                       GC024010
103700                           WS-GCT-EXTEND-BENPER-YR-ER-SW.         GC024010
103800                                                                  GC024010
103900                                                                  GC024010
104000     IF WS-AOL-L-O-B-ERROR                                        GC024010
104100       IF WS-AOL-AF-CODED                                         GC024010
104200          SET GAD-INDEX TO GAD-ENTRY-COUNT                        GC024010
104300          SET GAD-INDEX DOWN BY 1.                                GC024010
104400                                                                  GC024010
104500 1310-EXIT.                                                       GC024010
104600     EXIT.                                                        GC024010
104700                                                                  GC024010
104800 1320-CHECK-OPX.                                                  GC024010
104900*-- VALUES 10 THRU 20 VALID ONLY IF #ACP EXIST.                   GC024010
105000                                                                  GC024010
105100     MOVE GAD-O-P-X-DEFINITION (GAD-INDEX)                        GC024010
105200     TO   WS-OPX.                                                 GC024010
105300                                                                  GC024010
105400     IF SELECTED-OPX                                              GC024010
105500        IF GCT-CON-TAB-ID (6) = '#ACP  '                          GC024010
105600          AND                                                     GC024010
105700           GCT-CON-TAB-SLOT (6) > ZEROS                           GC024010
105800             NEXT SENTENCE                                        GC024010
105900        ELSE                                                      GC024010
106000             MOVE 'YES'          TO  ERROR-SW                     GC024010
106100             ADD 1               TO  DF-ERROR-COUNT               GC024010
106200             MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT               GC024010
106300             MOVE 'A30'          TO  DF-ERR-CODE (DF-ERROR-INDEX) GC024010
106400             SET DF-ERROR-INDEX UP BY 1.                          GC024010
106500                                                                  GC024010
106600 1320-EXIT.                                                       GC024010
106700     EXIT.                                                        GC024010
106800/------------------ E D I T  A C P  T A B L E ------------------- GC024010
106900 1400-CHECK-ACP.                                                  GC024010
107000     MOVE GCT-CON-TAB-ID-SLOT (6) TO GAF-TABULAR-PROVISION-ID.    GC024010
107100     PERFORM 0300-READ-TABULAR THRU 0300-EXIT.                    GC024010
107200     PERFORM 1410-COMPARE-ACP-OCCURS THRU 1410-EXIT               GC024010
107300       VARYING GAF-INDEX FROM 1 BY 1                              GC024010
107400       UNTIL   GAF-INDEX = GAF-ENTRY-COUNT.                       GC024010
107500                                                                  GC024010
107600*--- D365B  CHECK ACP CAPI                                        GC024010
107700     MOVE 'N' TO WS-ACP-CAPI-FOUND-SW                             GC024010
107800     PERFORM 1420-CHECK-ACP-CAPI       THRU 1420-EXIT             GC024010
107900       VARYING GAF-INDEX FROM 1 BY 1                              GC024010
108000       UNTIL   GAF-INDEX = GAF-ENTRY-COUNT.                       GC024010
108100                                                                  GC024010
108200*--- ISSR 11154, ALL ACCUM TABULAR MUST GO TO THE LOGICAL ------- GC024010
108300*--- EDIT MODULE GC024015 FOR AGE-LIMIT FIELDS EDIT.      ------- GC024010
108400*********************************************************         GC024010
108500* THE DF-ERROR-COUNT IS STORED TO DETERMINE WHETHER THERE         GC024010
108600* WERE ANY ERRORS ENCOUNTERED IN THE ACCUM EDIT MODULE.           GC024010
108700**                                                                GC024010
108800     MOVE DF-ERROR-COUNT TO WS-DF-ERROR-COUNT.                    GC024010
108900                                                                  GC024010
109000     CALL WS-GC024015 USING GC024030-CALL-AREA                    GC024010
109100                            TWOA-REC-AREA.                        GC024010
109200                                                                  GC024010
109300*********************************************************         GC024010
109400* THE DF-ERROR-INDEX MUST BE RESET UPON RETURNING FROM            GC024010
109500* THE ACCUM EDIT MODULE BECAUSE IT ISN'T PASSED BETWEEN           GC024010
109600* PROGRAMS.                                                       GC024010
109700**                                                                GC024010
109800     SET DF-ERROR-INDEX TO DF-ERROR-COUNT.                        GC024010
109900     SET DF-ERROR-INDEX UP BY 1.                                  GC024010
110000                                                                  GC024010
110100     IF DF-ERROR-COUNT > WS-DF-ERROR-COUNT                        GC024010
110200         MOVE 'YES' TO ERROR-SW.                                  GC024010
110300                                                                  GC024010
110400 1400-EXIT.                                                       GC024010
110500     EXIT.                                                        GC024010
110600 1410-COMPARE-ACP-OCCURS.                                         GC024010
110700                                                                  GC024010
110800     IF WS-ACP-L-O-B-ERROR                                        GC024010
110900        NEXT SENTENCE                                             GC024010
111000     ELSE                                                         GC024010
111100       IF GAF-COPAY-L-O-B (GAF-INDEX) NOT = GCT-L-O-B             GC024010
111200           MOVE 'YES'          TO  WS-ACP-L-O-B-SWITCH            GC024010
111300                                   ERROR-SW                       GC024010
111400           ADD 1               TO  DF-ERROR-COUNT                 GC024010
111500           MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT                 GC024010
111600           MOVE 'C30'          TO  DF-ERR-CODE (DF-ERROR-INDEX)   GC024010
111700           SET DF-ERROR-INDEX UP BY 1.                            GC024010
111800                                                                  GC024010
111900     IF WS-ACP-L-O-B-ERROR                                        GC024010
112000       IF WS-GCT-EXTEND-BENPER-YR-ERROR                           GC024010
112100          SET GAF-INDEX TO GAF-ENTRY-COUNT                        GC024010
112200          SET GAF-INDEX DOWN BY 1                                 GC024010
112300          GO TO 1410-EXIT.                                        GC024010
112400                                                                  GC024010
112500     IF WS-GCT-EXTEND-BENPER-YR-ERROR                             GC024010
112600          GO TO 1410-EXIT.                                        GC024010
112700                                                                  GC024010
112800     IF GAF-COPAY-BENEFIT-PERIOD (GAF-INDEX) =   'AF'             GC024010
112900       IF GCT-EXTEND-BENEFIT-PERIOD-YR       =  'NONE'            GC024010
113000          MOVE 'YES'   TO  WS-ACP-AF-SWITCH                       GC024010
113100                           WS-GCT-EXTEND-BENPER-YR-ER-SW.         GC024010
113200                                                                  GC024010
113300     IF WS-ACP-L-O-B-ERROR                                        GC024010
113400       IF WS-ACP-AF-CODED                                         GC024010
113500          SET GAF-INDEX TO GAF-ENTRY-COUNT                        GC024010
113600          SET GAF-INDEX DOWN BY 1.                                GC024010
113700                                                                  GC024010
113800 1410-EXIT.                                                       GC024010
113900     EXIT.                                                        GC024010
114000                                                                  GC024010
114100                                                                  GC024010
114200 1420-CHECK-ACP-CAPI.                                             GC024010
114300                                                                  GC024010
114400*-- IF CAPI CODED, SET SWITCH TO Y (ACL CAPI MUST BE CODED TOO)   GC024010
114500                                                                  GC024010
114600     IF GAF-COPAY-COMB-APPLIED-IND (GAF-INDEX)  = '01'            GC024010
114700        MOVE 'Y' TO WS-ACP-CAPI-FOUND-SW                          GC024010
114800     END-IF.                                                      GC024010
114900                                                                  GC024010
115000                                                                  GC024010
115100 1420-EXIT.                                                       GC024010
115200     EXIT.                                                        GC024010
115300/                                                                 GC024010
115400 9010-CLOSE-FILES.                                                GC024010
115500     MOVE 'C' TO REQUEST-TYPE-1.                                  GC024010
115600     CALL 'TSGVSAM1' USING PARM-ONE PARM-ONEA.                    GC024010
115700     IF  REQUEST-TYPE-1 NOT = 'C'                                 GC024010
115800         MOVE '9010--BAD CLOSE CONTRACT   ' TO VSAM-WS-ERR-MSG    GC024010
115900         MOVE ONEA-FEEDBACK  TO ABEND-CODE                        GC024010
116000         GO TO 9999-ERROR-RTN.                                    GC024010
116100                                                                  GC024010
116200     MOVE 'C' TO REQUEST-TYPE-2.                                  GC024010
116300     CALL 'TSGVSAM4' USING PARM-TWO PARM-TWOA.                    GC024010
116400     IF  REQUEST-TYPE-2 NOT = 'C'                                 GC024010
116500         MOVE '9010--BAD CLOSE TABULAR FILE' TO VSAM-WS-ERR-MSG   GC024010
116600         MOVE TWOA-FEEDBACK  TO ABEND-CODE                        GC024010
116700         GO TO 9999-ERROR-RTN.                                    GC024010
116800                                                                  GC024010
116900 9010-EXIT.                                                       GC024010
117000     EXIT.                                                        GC024010
117100/                                                                 GC024010
117200 9999-ERROR-RTN.                                                  GC024010
117300                                                                  GC024010
117400     CALL 'TSGEND' USING ABEND-CODE.                              GC024010
117500                                                                  GC024010
117600 9999-EXIT.                                                       GC024010
117700     EXIT.                                                        GC024010
