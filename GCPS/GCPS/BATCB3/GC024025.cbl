000100 IDENTIFICATION DIVISION.                                         12/09/02
000200 PROGRAM-ID.    GC024025.                                         GC024025
000300*THIS IS A COBOL/2 PROGRAM                                           LV002
000400 AUTHOR.        ED WITKUS.                                        GC024025
000500 INSTALLATION.  HCSC.                                             GC024025
000600 DATE-WRITTEN.  MAY, 1986.                                        GC024025
000700 DATE-COMPILED.                                                   GC024025
000800******************************************************************GC024025
000900******************************************************************GC024025
001000*                                                                 GC024025
001100*   THIS PROGRAM PERFORMS THE LOGICAL EDITS FOR THE BENEFIT       GC024025
001200*   PROVISION FILE.                                               GC024025
001300*   IT THEN CALLS 'GC024030' WHICH UPDATES THE DISCREPANCY FILE.  GC024025
001400*                                                                 GC024025
001500*   THIS PROGRAM MODULE CALLED BY PROGRAM GC0240                  GC024025
001600*   AND IS NOT TO BE EXECUTED AS A STAND ALONE PROGRAM.           GC024025
001700*                                                                 GC024025
001800*   TSGVSAM4 IS THE ONLINE TABULAR FILE           - (I/O).        GC024025
001900*   TSGVSAM5 IS THE ONLINE BENEFIT PROVISION FILE - (I/O).        GC024025
002000*                                                                 GC024025
002100******************************************************************GC024025
002200******************************************************************GC024025
002300*  NUM     DATE     PGMER    MAINTENANCE REQUIRED                *GC024025
002400* ---------------------------------------------------------------*GC024025
002500*        11-12-86    FRY     MODIFIED AND ADDED LOGICAL EDITS    *GC024025
002600*                                                                *GC024025
002700*  D116   9-09-87    FRY     CAPTURE OPERATOR-ID.                *GC024025
002800*                                                                *GC024025
002900*  M215  11-12-87    ENW     ADD NEW EDIT. IF PRICING METHOD =   *GC024025
003000*                            '06' '07' '33' BEN-SCOPE-ID CANNOT  *GC024025
003100*                            = ZEROS.                             GC024025
003200*                                                                *GC024025
003300*  M0007  3-01-88    DES     ADD NEW EDITS FOR: PRICING-METHOD,  *GC024025
003400*                            HOSP-ADM-RESTRN-IND & DAYS, STAY-   *GC024025
003500*                            CODE & IND, TREAT-TIME-FACTOR,       GC024025
003600*                            MULT-UNRELATED-PROC-1 & 2, MULT-     GC024025
003700*                            RELATED-PROC-1 & 2, MULT-INJURY-PROC-GC024025
003800*                            1 & 2, MULT-PODIATRY-LVL-1 2 3 & 4,  GC024025
003900*                            AND BEN-MAX-VISIT; FOR ALL BEN TYPES GC024025
004000*  D217  04/03/89    ENW     ADD VALUE '40' TO PRICIING METHOD   *GC024025
004100*                            EDIT.                               *GC024025
004200* D203   05/31/89  ENW  ADDED EDIT TO CHECK THE #AOL PERCENTS.   *GC024025
004300* D222   07/11/89  ENW  ADDED CALL TO GC024015 TO PERFORM THE    *GC024025
004400*                       ACCUM EDITS.                             *GC024025
004500* D223   09/11/89  ENW  ADDED VALUE '42' TO PRICING METHOD EDIT. *GC024025
004600*                                                                *GC024025
004700* D217.1 04/03/89  AHL  REMOVE VALUE '40'  FROM PRICING METHOD   *GC024025
004800*                       EDIT IN 0300-CHECK-EDITS.                *GC024025
004900*                                                                *GC024025
005000* D12009 09/10/91  GDM  INCREASE L-FAM-REL TO 2 POSITIONS        *GC024025
005100*                                                                *GC024025
005200* P052   05/27/93  ENW  ADDED VALUE '51' TO PRICING METHOD EDIT. *GC024025
005300*                                                                *GC024025
005400*        01/18/95  GDM  CONVERT TO COBOL II                      *GC024025
005500*                                                                *GC024025
005600*        01/31/97  DAU  MODIFIED PROGRAM SO IF ADDITIONAL        *GC024025
005700*                       PRICING OR VARIABLE INDEMNITY PRICING    *GC024025
005800*                       ARE CODED, THE PROVISION PRICING METHOD  *GC024025
005900*                       MUST BE > 0.  ALSO, TOOK OUT EDITS THAT  *GC024025
006000*                       ONLY ALLOW CERTAIN PROVISION PRICING     *GC024025
006100*                       METHODS TO BE USED WITH ADDITIONAL       *GC024025
006200*                       PRICING.                                 *GC024025
006300*  14726/                                                         GC024025
006400*  15057     10/22/97  AB   ADDED CODE TO SUPPORT THE YEAR        GC024025
006500*                           2000 AND THE EXPANSION OF THE         GC024025
006600*                           CONTRACT KEY TO SUPPORT THE TX        GC024025
006700*                           MERGER.                               GC024025
006800*                                                                 GC024025
006900*            02/04/99  KJD  ADDED EXTRA DISPLAY INFO, COMMENT     GC024025
007000*                           OUT DF-ERROR-COUNT MOVE TO ITSELF     GC024025
007100*                                                                *GC024025
007200*  08-14-02     GTF       EXPAND OPERATOR ID FROM 5 TO 8 CHARS.  *GC024025
007300*                                                                *GC024025
007320*  09-21-09     DNK       DM9441 - EXPANDED 7795 TO 31360 FOR    *GC024025
007330*                         THE TABULAR FILE EXPANSION.            *GC024025
007400*                                                                *GC024025
ED0624* BBDA-58217 06/04/24 ED  PEAQ COPYBOOK EXPANSION:               *        
ED0624*                                COPYBKS - GCTABM*, GCTACL*,     *        
ED0624*                                GCTACP*,  GCTADL*, GCTADL*      *        
007500******************************************************************GC024025
007600 ENVIRONMENT DIVISION.                                            GC024025
007700 CONFIGURATION SECTION.                                           GC024025
007800 SOURCE-COMPUTER. IBM-370.                                        GC024025
007900 OBJECT-COMPUTER. IBM-370.                                        GC024025
008000 INPUT-OUTPUT SECTION.                                            GC024025
008100 FILE-CONTROL.                                                    GC024025
008200                                                                  GC024025
008300 DATA DIVISION.                                                   GC024025
008400 FILE SECTION.                                                    GC024025
008500 WORKING-STORAGE SECTION.                                         GC024025
008600 01  FILLER                    PIC X(32) VALUE                    GC024025
008700     'GC024025 WORKING STORAGE'.                                  GC024025
008800                                                                  GC024025
008900 01  VSAM-WS-ERR-MSG           PIC X(32) VALUE SPACES.            GC024025
009000                                                                  GC024025
009100 01  ABEND-CODE                PIC 9(4)  COMP.                    GC024025
009200                                                                  GC024025
009300 01  WS-MISC-AREA.                                                GC024025
009400     05   WS-PCT-LVL           PIC S9(3) COMP-3 VALUE ZEROS.      GC024025
009500     05   WS-DF-ERROR-COUNT    PIC S9(3) COMP-3 VALUE ZEROS.      GC024025
009600     05   WS-GC024015          PIC X(8)         VALUE 'GC024015'. GC024025
009700     05   WS-HOLD-ACL-KEY.                                        GC024025
009800          10  WS-HOLD-ACL-ID   PIC X(6)         VALUE SPACES.     GC024025
009900          10  WS-HOLD-ACL-SLOT PIC S9(7) COMP-3 VALUE ZEROS.      GC024025
010000     05   WS-HOLD-AOL-KEY.                                        GC024025
010100          10  WS-HOLD-AOL-ID   PIC X(6)         VALUE SPACES.     GC024025
010200          10  WS-HOLD-AOL-SLOT PIC S9(7) COMP-3 VALUE ZEROS.      GC024025
010300     05   WS-DISP-CNT          PIC  9(3)        VALUE ZEROS.      GC024025
010400                                                                  GC024025
010500 01  WS-SWITCH-AREA.                                              GC024025
010600     05  WS-ERROR-SW           PIC X(03) VALUE 'NO '.             GC024025
010700         88  WS-ERROR-FOUND              VALUE 'YES'.             GC024025
010800     05  WS-PVE-SWITCH         PIC X(03) VALUE 'NO '.             GC024025
010900         88  WS-PVE-ENTRY-FOUND          VALUE 'YES'.             GC024025
011000     05  WS-PPF-SWITCH         PIC X(03) VALUE 'NO '.             GC024025
011100         88  WS-PPF-ENTRY-FOUND          VALUE 'YES'.             GC024025
011200     05  WS-AAR-SWITCH         PIC X(03) VALUE 'NO '.             GC024025
011300         88  WS-AAR-ENTRY-FOUND          VALUE 'YES'.             GC024025
011400     05  WS-ACL-SWITCH         PIC X(03) VALUE 'NO '.             GC024025
011500         88  WS-ACL-ENTRY-FOUND          VALUE 'YES'.             GC024025
011600     05  WS-AOL-SWITCH         PIC X(03) VALUE 'NO '.             GC024025
011700         88  WS-AOL-ENTRY-FOUND          VALUE 'YES'.             GC024025
011800                                                                  GC024025
011900 01  PARM-SET.                                                    GC024025
012000     05  SET-RDW.                                                 GC024025
012100         10  SET-REC-LENG    PIC 9(4)    VALUE ZEROS     COMP.    GC024025
012200         10  SET-FEEDBACK    PIC 9(4)    VALUE ZEROS     COMP.    GC024025
012300     05  SET-VALUE           PIC 9(8)                    COMP.    GC024025
012400                                                                  GC024025
012500 01  PARM-ONE.                                                    GC024025
012600     05  RESERVED-FLDS-1     PIC 9(8)    VALUE ZEROS     COMP.    GC024025
012700     05  RESERVED-X-1 REDEFINES RESERVED-FLDS-1.                  GC024025
012800         10  REQUEST-TYPE-1  PIC X.                               GC024025
012900         10  FILLER          PIC X(3).                            GC024025
013000                                                                  GC024025
013100 01  PARM-ONEA.                                                   GC024025
013200     02  ONEA-RDW.                                                GC024025
013300         05  ONEA-REC-LENG   PIC 9(4)    VALUE ZEROS     COMP.    GC024025
013400         05  ONEA-FEEDBACK   PIC 9(4)    VALUE ZEROS     COMP.    GC024025
013500     02  ONEA-REC-AREA.                                           GC024025
013600         COPY GCBENPVC.                                           GC024025
013700                                                                  GC024025
013800 01  PARM-TWO.                                                    GC024025
013900     05  RESERVED-FLDS-2     PIC 9(8)    VALUE ZEROS     COMP.    GC024025
014000     05  RESERVED-X-2 REDEFINES RESERVED-FLDS-2.                  GC024025
014100         10  REQUEST-TYPE-2  PIC X.                               GC024025
014200         10  FILLER          PIC X(3).                            GC024025
014300                                                                  GC024025
014400 01  PARM-TWOA.                                                   GC024025
014500     02  TWOA-RDW.                                                GC024025
014600         05  TWOA-REC-LENG   PIC 9(4)    VALUE ZEROS     COMP.    GC024025
014700         05  TWOA-FEEDBACK   PIC 9(4)    VALUE ZEROS     COMP.    GC024025
014800     02  TWOA-REC-AREA.                                           GC024025
014900         05  TWOA-TAB-KEY.                                        GC024025
015000             10  TWOA-TAB-ID   PIC X(6)         VALUE SPACES.     GC024025
015100             10  TWOA-TAB-SLOT PIC S9(7) COMP-3 VALUE ZEROS.      GC024025
015200         05  FILLER            PIC X(31360).                      GC024025
015300     02  TWOA-ACL-REC-AREA REDEFINES TWOA-REC-AREA.               GC024025
015400         COPY GCTACLC                                             GC024025
ED0624         REPLACING  ==  1 TO 175  ==                              GC024025
ED0624                BY  ==       175  ==                              GC024025
015700                    ==  DEPENDING ON GAB-ENTRY-COUNT   ==         GC024025
015800                BY  ==                                 ==.        GC024025
015900     02  TWOA-AOL-REC-AREA REDEFINES TWOA-REC-AREA.               GC024025
016000         COPY GCTAOLC                                             GC024025
ED0624         REPLACING  ==  1 TO 175  ==                              GC024025
ED0624                BY  ==       175  ==                              GC024025
016300                    ==  DEPENDING ON GAD-ENTRY-COUNT   ==         GC024025
016400                BY  ==                                 ==.        GC024025
016500                                                                  GC024025
016600 01  GC024030-CALL-AREA.                                          GC024025
016700     05  GC024030-IND          PIC X.                             GC024025
016800*****    88  OPEN-FILE                VALUE 'O'.                  GC024025
016900*****    88  CLOSE-FILE               VALUE 'C'.                  GC024025
017000*****    88  BAD-EDIT                 VALUE 'E'.                  GC024025
017100*****    88  GOOD-EDIT                VALUE 'G'.                  GC024025
017200                                                                  GC024025
017300     COPY GCDFILEC.                                               GC024025
017400                                                                  GC024025
017500 LINKAGE SECTION.                                                 GC024025
017600 01  LINK-AREA.                                                   GC024025
017700     05 GC024025-IND           PIC X.                             GC024025
017800     05 LINK-CONTRACT-KEY.                                        GC024025
017900        10 L-PLAN-CODE         PIC X(3).                          GC024025
018000        10 L-GRP-NBR           PIC X(9).                          GC024025
018100        10 L-SECTN-NO          PIC X(5).                          GC024025
018200        10 L-PKG-CODE          PIC X(3).                          GC024025
018300        10 L-LOB               PIC X.                             GC024025
018400        10 L-PRV-CTL           PIC XX.                            GC024025
018500        10 L-FAM-REL           PIC XX.                            GC024025
018600        10  L-EFF-DATE.                                           GC024025
018700            15 L-EFFDT-CC           PIC X.                        GC024025
018800            15 L-EFF-DT             PIC S9(5)  COMP-3.            GC024025
018900        10  L-EFFDT-CEN REDEFINES                                 GC024025
019000             L-EFF-DATE             PIC S9(7)  COMP-3.            GC024025
019100     05 LINK-BEN-PROV-ID.                                         GC024025
019200        10 L-BEN-PROV-ID       PIC X(6).                          GC024025
019300        10 L-BEN-PROV-SLOT     PIC S9(7) COMP-3.                  GC024025
019400                                                                  GC024025
019500* 8/14/02 EXPAND OPID BY 3 TO 8 BYTES. GTF                        GC024025
019600 01  LINK-OPERATOR-ID          PIC X(08).                         GC024025
019700                                                                  GC024025
019800 PROCEDURE DIVISION USING LINK-AREA LINK-OPERATOR-ID.             GC024025
019900                                                                  GC024025
020000 0000-MAINLINE.                                                   GC024025
020100                                                                  GC024025
020200     IF  GC024025-IND = 'R'                                       GC024025
020300         PERFORM 0010-PROCESS THRU 0010-EXIT                      GC024025
020400         PERFORM 2000-PROCESS-DFILE THRU 2000-EXIT                GC024025
020500         GOBACK.                                                  GC024025
020600                                                                  GC024025
020700     IF  GC024025-IND = 'C'                                       GC024025
020800         PERFORM 9010-CLOSE-FILES THRU 9010-EXIT.                 GC024025
020900                                                                  GC024025
021000     GOBACK.                                                      GC024025
021100                                                                  GC024025
021200 0000-EXIT.                                                       GC024025
021300     EXIT.                                                        GC024025
021400                                                                  GC024025
021500 0010-PROCESS.                                                    GC024025
021600     MOVE ZEROS              TO DF-ERROR-COUNT.                   GC024025
021700     MOVE 'NO '              TO WS-ERROR-SW,     WS-PVE-SWITCH,   GC024025
021800                                WS-PPF-SWITCH,   WS-AAR-SWITCH,   GC024025
021900                                WS-ACL-SWITCH,                    GC024025
022000                                WS-AOL-SWITCH.                    GC024025
022100     MOVE 'B'                TO DF-RECORD-TYPE.                   GC024025
022200     MOVE L-PLAN-CODE        TO DF-REC-PLAN-CODE.                 GC024025
022300     MOVE L-GRP-NBR          TO DF-REC-GROUP.                     GC024025
022400     MOVE L-SECTN-NO         TO DF-REC-SECTION.                   GC024025
022500     MOVE L-PKG-CODE         TO DF-REC-PKG-CODE.                  GC024025
022600     MOVE L-LOB              TO DF-REC-LOB.                       GC024025
022700     MOVE L-PRV-CTL          TO DF-REC-PROV-CTL.                  GC024025
022800     MOVE L-FAM-REL          TO DF-REC-FAM-REL.                   GC024025
022900     MOVE L-EFFDT-CEN        TO DF-REC-EFFDT-CEN.                 GC024025
023000     MOVE L-BEN-PROV-ID      TO DF-REC-BEN-PROV-ID.               GC024025
023100     MOVE LINK-OPERATOR-ID   TO DF-OPERATOR-ID.                   GC024025
023200     SET DF-ERROR-INDEX      TO 1.                                GC024025
023300                                                                  GC024025
023400     PERFORM 0150-READ-BEN-PROVISION-FILE THRU 0150-EXIT.         GC024025
023500                                                                  GC024025
023600     PERFORM 0200-CHECK-BP-IDS  THRU 0200-EXIT                    GC024025
023700       VARYING GCP-INDEX  FROM  1  BY  1                          GC024025
023800         UNTIL  GCP-INDEX  EQUAL  GCP-COUNT-TAB-PROVN-POINTERS.   GC024025
023900                                                                  GC024025
024000     PERFORM 0300-CHECK-EDITS  THRU 0300-EXIT.                    GC024025
024100                                                                  GC024025
024200 0010-EXIT.                                                       GC024025
024300     EXIT.                                                        GC024025
024400                                                                  GC024025
024500 0150-READ-BEN-PROVISION-FILE.                                    GC024025
024600                                                                  GC024025
024700     MOVE L-BEN-PROV-ID     TO  GCP-PROVN-ID.                     GC024025
024800     MOVE L-BEN-PROV-SLOT   TO  GCP-PROVN-SLOT-NO.                GC024025
024900     MOVE 14                TO  ONEA-REC-LENG.                    GC024025
025000     MOVE 'R'               TO  REQUEST-TYPE-1.                   GC024025
025100                                                                  GC024025
025200     CALL 'TSGVSAM5' USING PARM-ONE PARM-ONEA.                    GC024025
025300                                                                  GC024025
025400     IF  REQUEST-TYPE-1 NOT = 'R'                                 GC024025
025500         MOVE '0100--BAD READ BEN-PROV    ' TO VSAM-WS-ERR-MSG    GC024025
025600         MOVE ONEA-FEEDBACK                 TO ABEND-CODE         GC024025
025700         GO  TO  9999-ERROR-RTN.                                  GC024025
025800                                                                  GC024025
025900 0150-EXIT.                                                       GC024025
026000     EXIT.                                                        GC024025
026100                                                                  GC024025
026200 0175-READ-TABULAR.                                               GC024025
026300                                                                  GC024025
026400     MOVE 14 TO TWOA-REC-LENG.                                    GC024025
026500     MOVE 'R' TO REQUEST-TYPE-2.                                  GC024025
026600                                                                  GC024025
026700     CALL 'TSGVSAM4' USING PARM-TWO PARM-TWOA.                    GC024025
026800                                                                  GC024025
026900     IF  REQUEST-TYPE-2 NOT = 'R'                                 GC024025
027000         MOVE '0175--BAD READ TABULAR     ' TO VSAM-WS-ERR-MSG    GC024025
027100         MOVE TWOA-FEEDBACK                 TO ABEND-CODE         GC024025
027200         DISPLAY 'GC024025--BAD TABULAR READ: '                   GC024025
027300             TWOA-TAB-KEY                                         GC024025
027400         DISPLAY VSAM-WS-ERR-MSG                                  GC024025
027500         GO TO 9999-ERROR-RTN.                                    GC024025
027600                                                                  GC024025
027700 0175-EXIT.                                                       GC024025
027800     EXIT.                                                        GC024025
027900                                                                  GC024025
028000 0200-CHECK-BP-IDS.                                               GC024025
028100                                                                  GC024025
028200     IF GCP-BP-ID (GCP-INDEX)    EQUAL  '#PVE  '                  GC024025
028300         MOVE 'YES' TO WS-PVE-SWITCH                              GC024025
028400         GO TO 0200-EXIT.                                         GC024025
028500                                                                  GC024025
028600     IF GCP-BP-ID (GCP-INDEX)    EQUAL  '#PPF  '                  GC024025
028700         MOVE 'YES' TO WS-PPF-SWITCH                              GC024025
028800         GO TO 0200-EXIT.                                         GC024025
028900                                                                  GC024025
029000     IF GCP-BP-ID (GCP-INDEX)    EQUAL  '#AAR  '                  GC024025
029100         MOVE 'YES' TO WS-AAR-SWITCH.                             GC024025
029200                                                                  GC024025
029300     IF GCP-BP-ID (GCP-INDEX)    EQUAL  '#ACL  '                  GC024025
029400         MOVE GCP-BEN-TAB-PROVN-ID (GCP-INDEX)                    GC024025
029500           TO WS-HOLD-ACL-KEY                                     GC024025
029600         MOVE 'YES' TO WS-ACL-SWITCH.                             GC024025
029700                                                                  GC024025
029800     IF GCP-BP-ID (GCP-INDEX)    EQUAL  '#AOL  '                  GC024025
029900         MOVE GCP-BEN-TAB-PROVN-ID (GCP-INDEX)                    GC024025
030000           TO WS-HOLD-AOL-KEY                                     GC024025
030100         MOVE 'YES' TO WS-AOL-SWITCH.                             GC024025
030200                                                                  GC024025
030300 0200-EXIT.                                                       GC024025
030400     EXIT.                                                        GC024025
030500                                                                  GC024025
030600 0300-CHECK-EDITS.                                                GC024025
030700                                                                  GC024025
030800     IF WS-PVE-ENTRY-FOUND                                        GC024025
030900         NEXT SENTENCE                                            GC024025
031000     ELSE                                                         GC024025
031100         ADD 1               TO  DF-ERROR-COUNT                   GC024025
031200*        MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT                   GC024025
031300         MOVE 'B14'          TO  DF-ERR-CODE (DF-ERROR-INDEX)     GC024025
031400         SET DF-ERROR-INDEX  UP  BY 1                             GC024025
031500         MOVE 'YES'          TO  WS-ERROR-SW.                     GC024025
031600                                                                  GC024025
031700                                                                  GC024025
031800****                                                              GC024025
031900     IF GCP-PROVN-PRICING-METHD    EQUAL   ZEROS                  GC024025
032000       AND                                                        GC024025
032100        GCP-PLACE-TREAT-ELIG-IND   EQUAL   ZEROS                  GC024025
032200          ADD 1                 TO  DF-ERROR-COUNT                GC024025
032300*         MOVE DF-ERROR-COUNT   TO  DF-ERROR-COUNT                GC024025
032400          MOVE 'B17'            TO  DF-ERR-CODE (DF-ERROR-INDEX)  GC024025
032500          SET DF-ERROR-INDEX    UP  BY 1                          GC024025
032600          MOVE 'YES'            TO  WS-ERROR-SW.                  GC024025
032700****                                                              GC024025
032800     IF GCP-PROVN-PRICING-METHD  =  '03' OR  '12' OR  '13' OR     GC024025
032900        '14' OR  '16' OR  '17' OR  '21' OR  '22' OR  '33' OR      GC024025
033000        '42' OR '51'                                              GC024025
033100        IF GCP-ADDITIONAL-PRICING-PRCNT   =   ZEROS               GC024025
033200           ADD 1                 TO  DF-ERROR-COUNT               GC024025
033300*          MOVE DF-ERROR-COUNT   TO  DF-ERROR-COUNT               GC024025
033400           MOVE 'B01'            TO  DF-ERR-CODE (DF-ERROR-INDEX) GC024025
033500           SET DF-ERROR-INDEX    UP  BY 1                         GC024025
033600           MOVE 'YES'            TO  WS-ERROR-SW                  GC024025
033700        ELSE                                                      GC024025
033800           NEXT SENTENCE                                          GC024025
033900     ELSE                                                         GC024025
034000        IF (GCP-ADDITIONAL-PRICING-PRCNT > 0) AND                 GC024025
034100           (GCP-PROVN-PRICING-METHD NOT > '00')                   GC024025
034200           ADD 1                 TO  DF-ERROR-COUNT               GC024025
034300*          MOVE DF-ERROR-COUNT   TO  DF-ERROR-COUNT               GC024025
034400           MOVE 'B20'            TO  DF-ERR-CODE (DF-ERROR-INDEX) GC024025
034500           SET DF-ERROR-INDEX    UP  BY 1                         GC024025
034600           MOVE 'YES'            TO  WS-ERROR-SW.                 GC024025
034700****                                                              GC024025
034800     IF GCP-PROVN-PRICING-METHD  =  '06' OR  '33'                 GC024025
034900        IF GCP-VARIABLE-INDEMNITY-PRCNT   =   ZEROS               GC024025
035000           ADD 1                 TO  DF-ERROR-COUNT               GC024025
035100*          MOVE DF-ERROR-COUNT   TO  DF-ERROR-COUNT               GC024025
035200           MOVE 'B02'            TO  DF-ERR-CODE (DF-ERROR-INDEX) GC024025
035300           SET DF-ERROR-INDEX    UP  BY 1                         GC024025
035400           MOVE 'YES'            TO  WS-ERROR-SW                  GC024025
035500        ELSE                                                      GC024025
035600           NEXT SENTENCE                                          GC024025
035700     ELSE                                                         GC024025
035800        IF (GCP-VARIABLE-INDEMNITY-PRCNT > 0) AND                 GC024025
035900           (GCP-PROVN-PRICING-METHD NOT > '00')                   GC024025
036000           ADD 1                 TO  DF-ERROR-COUNT               GC024025
036100*          MOVE DF-ERROR-COUNT   TO  DF-ERROR-COUNT               GC024025
036200           MOVE 'B21'            TO  DF-ERR-CODE (DF-ERROR-INDEX) GC024025
036300           SET DF-ERROR-INDEX    UP  BY 1                         GC024025
036400           MOVE 'YES'            TO  WS-ERROR-SW.                 GC024025
036500                                                                  GC024025
036600****                                                              GC024025
036700     IF GCP-BEN-PROV-FORMAT-A                                     GC024025
036800        IF GPA-HOSP-ADM-RESTRN-IND  NOT =  '0'                    GC024025
036900           IF GPA-HSP-ADM-RESTRN-DAYS  =  ZEROS                   GC024025
037000              ADD 1                TO  DF-ERROR-COUNT             GC024025
037100*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
037200              MOVE 'B03'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
037300              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
037400              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
037500           ELSE                                                   GC024025
037600              NEXT SENTENCE                                       GC024025
037700        ELSE                                                      GC024025
037800           IF GPA-HSP-ADM-RESTRN-DAYS  NOT =  ZEROS               GC024025
037900              ADD 1                TO  DF-ERROR-COUNT             GC024025
038000*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
038100              MOVE 'B23'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
038200              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
038300              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
038400****                                                              GC024025
038500     IF GCP-BEN-PROV-FORMAT-A                                     GC024025
038600        IF GPA-STAY-CODE-IND  NOT =  '0'                          GC024025
038700           IF GPA-STAY-CD  =  ZEROS                               GC024025
038800              ADD 1                TO  DF-ERROR-COUNT             GC024025
038900*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
039000              MOVE 'B04'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
039100              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
039200              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
039300           ELSE                                                   GC024025
039400              NEXT SENTENCE                                       GC024025
039500        ELSE                                                      GC024025
039600           IF GPA-STAY-CD  NOT =  ZEROS                           GC024025
039700              ADD 1                TO  DF-ERROR-COUNT             GC024025
039800*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
039900              MOVE 'B24'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
040000              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
040100              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
040200                                                                  GC024025
040300****                                                              GC024025
040400     IF GCP-BEN-PROV-FORMAT-B                                     GC024025
040500        IF GPB-HOSP-ADM-RESTRN-IND  NOT =  '0'                    GC024025
040600           IF GPB-HSP-ADM-RESTRN-DAYS  =  ZEROS                   GC024025
040700              ADD 1                TO  DF-ERROR-COUNT             GC024025
040800*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
040900              MOVE 'B03'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
041000              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
041100              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
041200           ELSE                                                   GC024025
041300              NEXT SENTENCE                                       GC024025
041400        ELSE                                                      GC024025
041500           IF GPB-HSP-ADM-RESTRN-DAYS  NOT =  ZEROS               GC024025
041600              ADD 1                TO  DF-ERROR-COUNT             GC024025
041700*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
041800              MOVE 'B23'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
041900              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
042000              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
042100****                                                              GC024025
042200     IF GCP-BEN-PROV-FORMAT-B                                     GC024025
042300        IF GPB-STAY-CODE-IND  NOT =  '0'                          GC024025
042400           IF GPB-STAY-CD  =  ZEROS                               GC024025
042500              ADD 1                TO  DF-ERROR-COUNT             GC024025
042600*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
042700              MOVE 'B04'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
042800              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
042900              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
043000           ELSE                                                   GC024025
043100              NEXT SENTENCE                                       GC024025
043200        ELSE                                                      GC024025
043300           IF GPB-STAY-CD  NOT =  ZEROS                           GC024025
043400              ADD 1                TO  DF-ERROR-COUNT             GC024025
043500*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
043600              MOVE 'B24'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
043700              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
043800              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
043900****                                                              GC024025
044000     IF GCP-BEN-PROV-FORMAT-B                                     GC024025
044100        IF GPB-TREAT-TIME-FACTOR-IND  NOT =  '0'                  GC024025
044200           IF GPB-TREAT-TIME-FACTOR  =  ZEROS                     GC024025
044300              ADD 1                TO  DF-ERROR-COUNT             GC024025
044400*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
044500              MOVE 'B12'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
044600              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
044700              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
044800           ELSE                                                   GC024025
044900              NEXT SENTENCE                                       GC024025
045000        ELSE                                                      GC024025
045100           IF GPB-TREAT-TIME-FACTOR  NOT =  ZEROS                 GC024025
045200              ADD 1                TO  DF-ERROR-COUNT             GC024025
045300*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
045400              MOVE 'B28'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
045500              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
045600              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
045700                                                                  GC024025
045800****                                                              GC024025
045900     IF GCP-BEN-PROV-FORMAT-C                                     GC024025
046000        IF GPC-MULT-UNRL-PROC-1-PCT  NOT =  ZEROS                 GC024025
046100           IF GPC-MULT-UNRL-1-NO-OCCUR  =  ZEROS                  GC024025
046200              ADD 1                TO  DF-ERROR-COUNT             GC024025
046300*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
046400              MOVE 'B05'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
046500              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
046600              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
046700           ELSE                                                   GC024025
046800              NEXT SENTENCE                                       GC024025
046900        ELSE                                                      GC024025
047000           IF GPC-MULT-UNRL-1-NO-OCCUR  NOT =  ZEROS              GC024025
047100              ADD 1                TO  DF-ERROR-COUNT             GC024025
047200*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
047300              MOVE 'B30'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
047400              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
047500              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
047600****                                                              GC024025
047700     IF GCP-BEN-PROV-FORMAT-C                                     GC024025
047800        IF GPC-MULT-UNRL-PROC-2-PCT  NOT =  ZEROS                 GC024025
047900           IF GPC-MULT-UNRL-1-NO-OCCUR  =  ZEROS                  GC024025
048000              ADD 1                TO  DF-ERROR-COUNT             GC024025
048100*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
048200              MOVE 'B05'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
048300              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
048400              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
048500           ELSE                                                   GC024025
048600              NEXT SENTENCE                                       GC024025
048700        ELSE                                                      GC024025
048800           IF GPC-MULT-UNRL-2-NO-OCCUR  NOT =  ZEROS              GC024025
048900              ADD 1                TO  DF-ERROR-COUNT             GC024025
049000*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
049100              MOVE 'B30'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
049200              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
049300              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
049400****                                                              GC024025
049500     IF GCP-BEN-PROV-FORMAT-C                                     GC024025
049600        IF GPC-MULT-RL-PROC-1-PCT  NOT =  ZEROS                   GC024025
049700           IF GPC-MULT-RL-1-NO-OCCUR  =  '0'                      GC024025
049800              ADD 1                TO  DF-ERROR-COUNT             GC024025
049900*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
050000              MOVE 'B05'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
050100              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
050200              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
050300           ELSE                                                   GC024025
050400              NEXT SENTENCE                                       GC024025
050500        ELSE                                                      GC024025
050600           IF GPC-MULT-RL-1-NO-OCCUR  NOT =  '0'                  GC024025
050700              ADD 1                TO  DF-ERROR-COUNT             GC024025
050800*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
050900              MOVE 'B30'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
051000              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
051100              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
051200****                                                              GC024025
051300     IF GCP-BEN-PROV-FORMAT-C                                     GC024025
051400        IF GPC-MULT-RL-PROC-2-PCT  NOT =  ZEROS                   GC024025
051500           IF GPC-MULT-RL-2-NO-OCCUR  =  '0'                      GC024025
051600              ADD 1                TO  DF-ERROR-COUNT             GC024025
051700*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
051800              MOVE 'B05'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
051900              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
052000              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
052100           ELSE                                                   GC024025
052200              NEXT SENTENCE                                       GC024025
052300        ELSE                                                      GC024025
052400           IF GPC-MULT-RL-2-NO-OCCUR  NOT =  '0'                  GC024025
052500              ADD 1                TO  DF-ERROR-COUNT             GC024025
052600*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
052700              MOVE 'B30'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
052800              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
052900              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
053000****                                                              GC024025
053100     IF GCP-BEN-PROV-FORMAT-C                                     GC024025
053200        IF GPC-MULT-INJ-LVL-1-PCT  NOT =  ZEROS                   GC024025
053300           IF GPC-MULT-INJ-1-NO-OCCUR  =  '0'                     GC024025
053400              ADD 1                TO  DF-ERROR-COUNT             GC024025
053500*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
053600              MOVE 'B05'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
053700              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
053800              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
053900           ELSE                                                   GC024025
054000              NEXT SENTENCE                                       GC024025
054100        ELSE                                                      GC024025
054200           IF GPC-MULT-INJ-1-NO-OCCUR  NOT =  '0'                 GC024025
054300              ADD 1                TO  DF-ERROR-COUNT             GC024025
054400*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
054500              MOVE 'B30'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
054600              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
054700              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
054800****                                                              GC024025
054900     IF GCP-BEN-PROV-FORMAT-C                                     GC024025
055000        IF GPC-MULT-INJ-LVL-2-PCT  NOT =  ZEROS                   GC024025
055100           IF GPC-MULT-INJ-2-NO-OCCUR  =  '0'                     GC024025
055200              ADD 1                TO  DF-ERROR-COUNT             GC024025
055300*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
055400              MOVE 'B05'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
055500              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
055600              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
055700           ELSE                                                   GC024025
055800              NEXT SENTENCE                                       GC024025
055900        ELSE                                                      GC024025
056000           IF GPC-MULT-INJ-2-NO-OCCUR  NOT =  '0'                 GC024025
056100              ADD 1                TO  DF-ERROR-COUNT             GC024025
056200*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
056300              MOVE 'B30'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
056400              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
056500              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
056600****                                                              GC024025
056700     IF GCP-BEN-PROV-FORMAT-C                                     GC024025
056800        IF GPC-MULT-POD-PROC-1-PCT  NOT =  ZEROS                  GC024025
056900           IF GPC-MULT-POD-1-NO-OCCUR  =  '0'                     GC024025
057000              ADD 1                TO  DF-ERROR-COUNT             GC024025
057100*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
057200              MOVE 'B05'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
057300              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
057400              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
057500           ELSE                                                   GC024025
057600              NEXT SENTENCE                                       GC024025
057700        ELSE                                                      GC024025
057800           IF GPC-MULT-POD-1-NO-OCCUR  NOT =  '0'                 GC024025
057900              ADD 1                TO  DF-ERROR-COUNT             GC024025
058000*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
058100              MOVE 'B30'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
058200              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
058300              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
058400****                                                              GC024025
058500     IF GCP-BEN-PROV-FORMAT-C                                     GC024025
058600        IF GPC-MULT-POD-PROC-2-PCT  NOT =  ZEROS                  GC024025
058700           IF GPC-MULT-POD-2-NO-OCCUR  =  '0'                     GC024025
058800              ADD 1                TO  DF-ERROR-COUNT             GC024025
058900*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
059000              MOVE 'B05'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
059100              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
059200              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
059300           ELSE                                                   GC024025
059400              NEXT SENTENCE                                       GC024025
059500        ELSE                                                      GC024025
059600           IF GPC-MULT-POD-2-NO-OCCUR  NOT =  '0'                 GC024025
059700              ADD 1                TO  DF-ERROR-COUNT             GC024025
059800*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
059900              MOVE 'B30'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
060000              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
060100              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
060200****                                                              GC024025
060300     IF GCP-BEN-PROV-FORMAT-C                                     GC024025
060400        IF GPC-MULT-POD-PROC-3-PCT  NOT =  ZEROS                  GC024025
060500           IF GPC-MULT-POD-3-NO-OCCUR  =  '0'                     GC024025
060600              ADD 1                TO  DF-ERROR-COUNT             GC024025
060700*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
060800              MOVE 'B05'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
060900              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
061000              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
061100           ELSE                                                   GC024025
061200              NEXT SENTENCE                                       GC024025
061300        ELSE                                                      GC024025
061400           IF GPC-MULT-POD-3-NO-OCCUR  NOT =  '0'                 GC024025
061500              ADD 1                TO  DF-ERROR-COUNT             GC024025
061600*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
061700              MOVE 'B30'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
061800              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
061900              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
062000****                                                              GC024025
062100     IF GCP-BEN-PROV-FORMAT-C                                     GC024025
062200        IF GPC-MULT-POD-PROC-4-PCT  NOT =  ZEROS                  GC024025
062300           IF GPC-MULT-POD-4-NO-OCCUR  =  '0'                     GC024025
062400              ADD 1                TO  DF-ERROR-COUNT             GC024025
062500*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
062600              MOVE 'B05'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
062700              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
062800              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
062900           ELSE                                                   GC024025
063000              NEXT SENTENCE                                       GC024025
063100        ELSE                                                      GC024025
063200           IF GPC-MULT-POD-4-NO-OCCUR  NOT =  '0'                 GC024025
063300              ADD 1                TO  DF-ERROR-COUNT             GC024025
063400*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
063500              MOVE 'B30'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
063600              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
063700              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
063800****                                                              GC024025
063900     IF GCP-BEN-PROV-FORMAT-C                                     GC024025
064000        IF GPC-MULT-POD-PROC-PRICE-IND  NOT =  '0'                GC024025
064100           IF GPC-MULT-POD-PROC-1-PCT  =  ZEROS AND               GC024025
064200              GPC-MULT-POD-1-NO-OCCUR  =  '0'                     GC024025
064300              ADD 1                TO  DF-ERROR-COUNT             GC024025
064400*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
064500              MOVE 'B09'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
064600              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
064700              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
064800           ELSE                                                   GC024025
064900              NEXT SENTENCE                                       GC024025
065000        ELSE                                                      GC024025
065100           IF GPC-MULT-POD-PROC-1-PCT  NOT =  ZEROS OR            GC024025
065200              GPC-MULT-POD-PROC-2-PCT  NOT =  ZEROS OR            GC024025
065300              GPC-MULT-POD-PROC-3-PCT  NOT =  ZEROS OR            GC024025
065400              GPC-MULT-POD-PROC-4-PCT  NOT =  ZEROS               GC024025
065500              ADD 1                TO  DF-ERROR-COUNT             GC024025
065600*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
065700              MOVE 'B06'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
065800              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
065900              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
066000****                                                              GC024025
066100     IF GCP-BEN-PROV-FORMAT-C                                     GC024025
066200        IF GPC-MULT-POD-PROC-2-PCT  NOT =  ZEROS AND              GC024025
066300           GPC-MULT-POD-2-NO-OCCUR  NOT =  '0'                    GC024025
066400           IF GPC-MULT-POD-PROC-1-PCT  =  ZEROS AND               GC024025
066500              GPC-MULT-POD-1-NO-OCCUR  =  '0'                     GC024025
066600              ADD 1                TO  DF-ERROR-COUNT             GC024025
066700*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
066800              MOVE 'B07'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
066900              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
067000              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
067100****                                                              GC024025
067200     IF GCP-BEN-PROV-FORMAT-C                                     GC024025
067300        IF GPC-MULT-POD-PROC-3-PCT  NOT =  ZEROS AND              GC024025
067400           GPC-MULT-POD-3-NO-OCCUR  NOT =  '0'                    GC024025
067500           IF (GPC-MULT-POD-PROC-1-PCT  =  ZEROS AND              GC024025
067600              GPC-MULT-POD-1-NO-OCCUR  =  '0') OR                 GC024025
067700              (GPC-MULT-POD-PROC-2-PCT  =  ZEROS AND              GC024025
067800              GPC-MULT-POD-2-NO-OCCUR  =  '0')                    GC024025
067900              ADD 1                TO  DF-ERROR-COUNT             GC024025
068000*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
068100              MOVE 'B08'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
068200              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
068300              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
068400****                                                              GC024025
068500     IF GCP-BEN-PROV-FORMAT-C                                     GC024025
068600        IF GPC-MULT-POD-PROC-4-PCT  NOT =  ZEROS AND              GC024025
068700           GPC-MULT-POD-4-NO-OCCUR  NOT =  '0'                    GC024025
068800           IF (GPC-MULT-POD-PROC-1-PCT  =  ZEROS AND              GC024025
068900              GPC-MULT-POD-1-NO-OCCUR  =  '0') OR                 GC024025
069000              (GPC-MULT-POD-PROC-2-PCT  =  ZEROS AND              GC024025
069100              GPC-MULT-POD-2-NO-OCCUR  =  '0') OR                 GC024025
069200              (GPC-MULT-POD-PROC-3-PCT  =  ZEROS AND              GC024025
069300              GPC-MULT-POD-3-NO-OCCUR  =  '0')                    GC024025
069400              ADD 1                TO  DF-ERROR-COUNT             GC024025
069500*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
069600              MOVE 'B10'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
069700              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
069800              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
069900****                                                              GC024025
070000     IF GCP-BEN-PROV-FORMAT-C                                     GC024025
070100        IF GPC-MULT-POD-PROC-2-PCT  =  ZEROS AND                  GC024025
070200           GPC-MULT-POD-2-NO-OCCUR  =  '0'                        GC024025
070300           IF (GPC-MULT-POD-PROC-3-PCT  NOT =  ZEROS AND          GC024025
070400              GPC-MULT-POD-3-NO-OCCUR  NOT =  '0') OR             GC024025
070500              (GPC-MULT-POD-PROC-4-PCT  NOT =  ZEROS AND          GC024025
070600              GPC-MULT-POD-4-NO-OCCUR  NOT =  '0')                GC024025
070700              ADD 1                TO  DF-ERROR-COUNT             GC024025
070800*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
070900              MOVE 'B18'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
071000              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
071100              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
071200****                                                              GC024025
071300     IF GCP-BEN-PROV-FORMAT-C                                     GC024025
071400        IF GPC-MULT-POD-PROC-3-PCT  =  ZEROS AND                  GC024025
071500           GPC-MULT-POD-3-NO-OCCUR  =  '0'                        GC024025
071600           IF GPC-MULT-POD-PROC-4-PCT  NOT =  ZEROS AND           GC024025
071700              GPC-MULT-POD-4-NO-OCCUR  NOT =  '0'                 GC024025
071800              ADD 1                TO  DF-ERROR-COUNT             GC024025
071900*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
072000              MOVE 'B19'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
072100              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
072200              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
072300                                                                  GC024025
072400****                                                              GC024025
072500     IF GCP-BEN-PROV-FORMAT-D                                     GC024025
072600        IF GPD-STAY-CODE-IND  NOT =  '0'                          GC024025
072700           IF GPD-STAY-CD  =  ZEROS                               GC024025
072800              ADD 1                TO  DF-ERROR-COUNT             GC024025
072900*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
073000              MOVE 'B04'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
073100              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
073200              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
073300           ELSE                                                   GC024025
073400              NEXT SENTENCE                                       GC024025
073500        ELSE                                                      GC024025
073600           IF GPD-STAY-CD  NOT =  ZEROS                           GC024025
073700              ADD 1                TO  DF-ERROR-COUNT             GC024025
073800*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
073900              MOVE 'B24'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
074000              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
074100              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
074200****                                                              GC024025
074300     IF GCP-BEN-PROV-FORMAT-D                                     GC024025
074400        IF GPD-BEN-MAX-VISIT-IND  NOT =  '0'                      GC024025
074500           IF GPD-BEN-MAX-VISIT-DAYS  =  ZEROS                    GC024025
074600              ADD 1                TO  DF-ERROR-COUNT             GC024025
074700*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
074800              MOVE 'B11'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
074900              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
075000              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
075100           ELSE                                                   GC024025
075200              NEXT SENTENCE                                       GC024025
075300        ELSE                                                      GC024025
075400           IF GPD-BEN-MAX-VISIT-DAYS  NOT =  ZEROS                GC024025
075500              ADD 1                TO  DF-ERROR-COUNT             GC024025
075600*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
075700              MOVE 'B29'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
075800              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
075900              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
076000****                                                              GC024025
076100     IF GCP-BEN-PROV-FORMAT-D                                     GC024025
076200        IF GPD-TREAT-TIME-FACTOR-IND  NOT =  '0'                  GC024025
076300           IF GPD-TREAT-TIME-FACTOR  =  ZEROS                     GC024025
076400              ADD 1                TO  DF-ERROR-COUNT             GC024025
076500*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
076600              MOVE 'B12'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
076700              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
076800              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
076900           ELSE                                                   GC024025
077000              NEXT SENTENCE                                       GC024025
077100        ELSE                                                      GC024025
077200           IF GPD-TREAT-TIME-FACTOR  NOT =  ZEROS                 GC024025
077300              ADD 1                TO  DF-ERROR-COUNT             GC024025
077400*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
077500              MOVE 'B28'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
077600              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
077700              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
077800                                                                  GC024025
077900****                                                              GC024025
078000     IF GCP-BEN-PROV-FORMAT-E                                     GC024025
078100        IF GPE-HOSP-ADM-RESTRN-IND  NOT =  '0'                    GC024025
078200           IF GPE-HSP-ADM-RESTRN-DAYS  =  ZEROS                   GC024025
078300              ADD 1                TO  DF-ERROR-COUNT             GC024025
078400*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
078500              MOVE 'B03'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
078600              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
078700              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
078800           ELSE                                                   GC024025
078900              NEXT SENTENCE                                       GC024025
079000        ELSE                                                      GC024025
079100           IF GPE-HSP-ADM-RESTRN-DAYS  NOT =  ZEROS               GC024025
079200              ADD 1                TO  DF-ERROR-COUNT             GC024025
079300*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
079400              MOVE 'B23'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
079500              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
079600              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
079700****                                                              GC024025
079800     IF GCP-BEN-PROV-FORMAT-E                                     GC024025
079900        IF GPE-BEN-MAX-VISITS-IND  NOT =  '0'                     GC024025
080000           IF GPE-BEN-MAX-VISITS-DAYS  =  ZEROS                   GC024025
080100              ADD 1                TO  DF-ERROR-COUNT             GC024025
080200*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
080300              MOVE 'B11'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
080400              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
080500              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
080600           ELSE                                                   GC024025
080700              NEXT SENTENCE                                       GC024025
080800        ELSE                                                      GC024025
080900           IF GPE-BEN-MAX-VISITS-DAYS  NOT =  ZEROS               GC024025
081000              ADD 1                TO  DF-ERROR-COUNT             GC024025
081100*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
081200              MOVE 'B29'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
081300              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
081400              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
081500****                                                              GC024025
081600     IF GCP-BEN-PROV-FORMAT-E                                     GC024025
081700        IF GPE-TREAT-TIME-FACTOR-IND  NOT =  '0'                  GC024025
081800           IF GPE-TREAT-TIME-FACTOR  =  ZEROS                     GC024025
081900              ADD 1                TO  DF-ERROR-COUNT             GC024025
082000*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
082100              MOVE 'B12'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
082200              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
082300              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
082400           ELSE                                                   GC024025
082500              NEXT SENTENCE                                       GC024025
082600        ELSE                                                      GC024025
082700           IF GPE-TREAT-TIME-FACTOR  NOT =  ZEROS                 GC024025
082800              ADD 1                TO  DF-ERROR-COUNT             GC024025
082900*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
083000              MOVE 'B28'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
083100              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
083200              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
083300                                                                  GC024025
083400****                                                              GC024025
083500     IF GCP-BEN-PROV-FORMAT-W                                     GC024025
083600        IF GPW-STAY-CODE-IND  NOT =  '0'                          GC024025
083700           IF GPW-STAY-CD  =  ZEROS                               GC024025
083800              ADD 1                TO  DF-ERROR-COUNT             GC024025
083900*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
084000              MOVE 'B04'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
084100              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
084200              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
084300           ELSE                                                   GC024025
084400              NEXT SENTENCE                                       GC024025
084500        ELSE                                                      GC024025
084600           IF GPW-STAY-CD  NOT =  ZEROS                           GC024025
084700              ADD 1                TO  DF-ERROR-COUNT             GC024025
084800*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
084900              MOVE 'B24'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
085000              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
085100              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
085200****                                                              GC024025
085300     IF GCP-BEN-PROV-FORMAT-W                                     GC024025
085400        IF GPW-TREAT-TIME-FACTOR-IND  NOT =  '0'                  GC024025
085500           IF GPW-TREAT-TIME-FACTOR  =  ZEROS                     GC024025
085600              ADD 1                TO  DF-ERROR-COUNT             GC024025
085700*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
085800              MOVE 'B12'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
085900              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
086000              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
086100           ELSE                                                   GC024025
086200              NEXT SENTENCE                                       GC024025
086300        ELSE                                                      GC024025
086400           IF GPW-TREAT-TIME-FACTOR  NOT =  ZEROS                 GC024025
086500              ADD 1                TO  DF-ERROR-COUNT             GC024025
086600*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
086700              MOVE 'B28'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
086800              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
086900              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
087000****                                                              GC024025
087100     IF GCP-BEN-PROV-FORMAT-W                                     GC024025
087200        IF GPW-HOSP-ADM-RESTRN-IND  NOT =  '0'                    GC024025
087300           IF GPW-HSP-ADM-RESTRN-DAYS  =  ZEROS                   GC024025
087400              ADD 1                TO  DF-ERROR-COUNT             GC024025
087500*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
087600              MOVE 'B03'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
087700              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
087800              MOVE 'YES'           TO  WS-ERROR-SW                GC024025
087900           ELSE                                                   GC024025
088000              NEXT SENTENCE                                       GC024025
088100        ELSE                                                      GC024025
088200           IF GPW-HSP-ADM-RESTRN-DAYS  NOT =  ZEROS               GC024025
088300              ADD 1                TO  DF-ERROR-COUNT             GC024025
088400*             MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT             GC024025
088500              MOVE 'B23'           TO  DF-ERR-CODE(DF-ERROR-INDEX)GC024025
088600              SET DF-ERROR-INDEX   UP  BY 1                       GC024025
088700              MOVE 'YES'           TO  WS-ERROR-SW.               GC024025
088800                                                                  GC024025
088900****                                                              GC024025
089000     IF GCP-PROVN-PRICING-METHD = '04' OR '14' OR '21' OR '22'    GC024025
089100       IF GCP-BEN-PROV-FORMAT-A                                   GC024025
089200         IF GPA-FLAT-RATE-PDM-AMT   NOT EQUAL    ZEROS            GC024025
089300            NEXT SENTENCE                                         GC024025
089400         ELSE                                                     GC024025
089500            ADD 1               TO DF-ERROR-COUNT                 GC024025
089600*           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                 GC024025
089700            MOVE 'B25'          TO DF-ERR-CODE (DF-ERROR-INDEX)   GC024025
089800            SET DF-ERROR-INDEX  UP BY 1                           GC024025
089900            MOVE 'YES'          TO WS-ERROR-SW                    GC024025
090000       ELSE                                                       GC024025
090100         IF GCP-BEN-PROV-FORMAT-D                                 GC024025
090200           IF GPD-FLAT-RATE-PDM-AMT   NOT EQUAL    ZEROS          GC024025
090300              NEXT SENTENCE                                       GC024025
090400           ELSE                                                   GC024025
090500              ADD 1               TO DF-ERROR-COUNT               GC024025
090600*             MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT               GC024025
090700              MOVE 'B25'          TO DF-ERR-CODE (DF-ERROR-INDEX) GC024025
090800              SET DF-ERROR-INDEX  UP BY 1                         GC024025
090900              MOVE 'YES'          TO WS-ERROR-SW                  GC024025
091000         ELSE                                                     GC024025
091100           IF GCP-BEN-PROV-FORMAT-W                               GC024025
091200             IF GPW-FLAT-RATE-PDM-AMT   NOT EQUAL    ZEROS        GC024025
091300              NEXT SENTENCE                                       GC024025
091400             ELSE                                                 GC024025
091500              ADD 1               TO DF-ERROR-COUNT               GC024025
091600*             MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT               GC024025
091700              MOVE 'B25'         TO DF-ERR-CODE (DF-ERROR-INDEX)  GC024025
091800              SET DF-ERROR-INDEX  UP BY 1                         GC024025
091900              MOVE 'YES'          TO WS-ERROR-SW                  GC024025
092000           ELSE                                                   GC024025
092100             NEXT SENTENCE                                        GC024025
092200     ELSE                                                         GC024025
092300       IF GCP-BEN-PROV-FORMAT-A                                   GC024025
092400         IF GPA-FLAT-RATE-PDM-AMT  GREATER THAN  ZEROS            GC024025
092500            ADD 1               TO DF-ERROR-COUNT                 GC024025
092600*           MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT                 GC024025
092700            MOVE 'B26'          TO DF-ERR-CODE (DF-ERROR-INDEX)   GC024025
092800            SET DF-ERROR-INDEX  UP BY 1                           GC024025
092900            MOVE 'YES'          TO WS-ERROR-SW                    GC024025
093000         ELSE                                                     GC024025
093100            NEXT SENTENCE                                         GC024025
093200       ELSE                                                       GC024025
093300         IF GCP-BEN-PROV-FORMAT-D                                 GC024025
093400           IF GPD-FLAT-RATE-PDM-AMT  GREATER THAN  ZEROS          GC024025
093500              ADD 1               TO DF-ERROR-COUNT               GC024025
093600*             MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT               GC024025
093700              MOVE 'B26'         TO DF-ERR-CODE (DF-ERROR-INDEX)  GC024025
093800              SET DF-ERROR-INDEX  UP BY 1                         GC024025
093900              MOVE 'YES'          TO WS-ERROR-SW                  GC024025
094000           ELSE                                                   GC024025
094100              NEXT SENTENCE                                       GC024025
094200         ELSE                                                     GC024025
094300           IF GCP-BEN-PROV-FORMAT-W                               GC024025
094400             IF GPW-FLAT-RATE-PDM-AMT  GREATER THAN  ZEROS        GC024025
094500              ADD 1               TO DF-ERROR-COUNT               GC024025
094600*             MOVE DF-ERROR-COUNT TO DF-ERROR-COUNT               GC024025
094700              MOVE 'B26'         TO DF-ERR-CODE (DF-ERROR-INDEX)  GC024025
094800              SET DF-ERROR-INDEX  UP BY 1                         GC024025
094900              MOVE 'YES'          TO WS-ERROR-SW                  GC024025
095000             ELSE                                                 GC024025
095100              NEXT SENTENCE                                       GC024025
095200           ELSE                                                   GC024025
095300             NEXT SENTENCE.                                       GC024025
095400                                                                  GC024025
095500                                                                  GC024025
095600*****                                                             GC024025
095700     IF GCP-PROVN-PRICING-METHD = '04' OR '14' OR '21' OR '22'    GC024025
095800       IF GCP-BEN-PROV-FORMAT-A                                   GC024025
095900         IF GPA-ECF-F-RAT-PER-DIEM-AMT   NOT EQUAL   ZEROS        GC024025
096000            NEXT SENTENCE                                         GC024025
096100         ELSE                                                     GC024025
096200            ADD 1               TO  DF-ERROR-COUNT                GC024025
096300*           MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT                GC024025
096400            MOVE 'B16'          TO  DF-ERR-CODE (DF-ERROR-INDEX)  GC024025
096500            SET DF-ERROR-INDEX  UP  BY 1                          GC024025
096600            MOVE 'YES'          TO  WS-ERROR-SW                   GC024025
096700       ELSE                                                       GC024025
096800          NEXT SENTENCE                                           GC024025
096900     ELSE                                                         GC024025
097000       IF GCP-BEN-PROV-FORMAT-A                                   GC024025
097100         IF GPA-ECF-F-RAT-PER-DIEM-AMT   NOT EQUAL   ZEROS        GC024025
097200            ADD 1               TO  DF-ERROR-COUNT                GC024025
097300*           MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT                GC024025
097400            MOVE 'B27'          TO  DF-ERR-CODE (DF-ERROR-INDEX)  GC024025
097500            SET DF-ERROR-INDEX  UP  BY 1                          GC024025
097600            MOVE 'YES'          TO  WS-ERROR-SW                   GC024025
097700         ELSE                                                     GC024025
097800           NEXT SENTENCE                                          GC024025
097900       ELSE                                                       GC024025
098000         NEXT SENTENCE.                                           GC024025
098100                                                                  GC024025
098200                                                                  GC024025
098300*****                                                             GC024025
098400     IF GCP-PROVN-PRICING-METHD = '09'                            GC024025
098500       IF WS-PPF-ENTRY-FOUND                                      GC024025
098600         NEXT SENTENCE                                            GC024025
098700       ELSE                                                       GC024025
098800         ADD 1                TO  DF-ERROR-COUNT                  GC024025
098900*        MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT                  GC024025
099000         MOVE 'B13'           TO  DF-ERR-CODE (DF-ERROR-INDEX)    GC024025
099100         SET DF-ERROR-INDEX   UP  BY 1                            GC024025
099200         MOVE 'YES'           TO  WS-ERROR-SW                     GC024025
099300     ELSE                                                         GC024025
099400       IF WS-PPF-ENTRY-FOUND                                      GC024025
099500         ADD 1                TO  DF-ERROR-COUNT                  GC024025
099600*        MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT                  GC024025
099700         MOVE 'B22'           TO  DF-ERR-CODE (DF-ERROR-INDEX)    GC024025
099800         SET DF-ERROR-INDEX   UP  BY 1                            GC024025
099900         MOVE 'YES'           TO  WS-ERROR-SW                     GC024025
100000       ELSE                                                       GC024025
100100         NEXT SENTENCE.                                           GC024025
100200                                                                  GC024025
100300*******                                                           GC024025
100400     IF GCP-PROVN-PRICING-METHD = '19'                            GC024025
100500       IF WS-AAR-ENTRY-FOUND                                      GC024025
100600          ADD 1                TO  DF-ERROR-COUNT                 GC024025
100700*         MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT                 GC024025
100800          MOVE 'B15'           TO  DF-ERR-CODE (DF-ERROR-INDEX)   GC024025
100900          SET DF-ERROR-INDEX   UP  BY 1                           GC024025
101000          MOVE 'YES'           TO  WS-ERROR-SW                    GC024025
101100       ELSE                                                       GC024025
101200          NEXT SENTENCE                                           GC024025
101300     ELSE                                                         GC024025
101400        NEXT SENTENCE.                                            GC024025
101500                                                                  GC024025
101600                                                                  GC024025
101700*******                                                           GC024025
101800*******  NEW EDIT ADDED 11/12/87.                                 GC024025
101900*******                                                           GC024025
102000     IF GCP-PROVN-PRICING-METHD = '06' OR '07' OR '33'            GC024025
102100         IF GCP-BEN-PROV-FORMAT-C                                 GC024025
102200             IF GPC-BEN-SCOPE-ID = '0000'                         GC024025
102300                 ADD 1                TO  DF-ERROR-COUNT          GC024025
102400*                MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT          GC024025
102500                 MOVE 'B31'      TO  DF-ERR-CODE (DF-ERROR-INDEX) GC024025
102600                 SET DF-ERROR-INDEX   UP  BY 1                    GC024025
102700                 MOVE 'YES'           TO  WS-ERROR-SW             GC024025
102800             ELSE                                                 GC024025
102900                 NEXT SENTENCE                                    GC024025
103000         ELSE                                                     GC024025
103100         IF GCP-BEN-PROV-FORMAT-D                                 GC024025
103200             IF GPD-BEN-SCOPE-ID = '0000'                         GC024025
103300                 ADD 1                TO  DF-ERROR-COUNT          GC024025
103400*                MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT          GC024025
103500                 MOVE 'B31'      TO  DF-ERR-CODE (DF-ERROR-INDEX) GC024025
103600                 SET DF-ERROR-INDEX   UP  BY 1                    GC024025
103700                 MOVE 'YES'           TO  WS-ERROR-SW             GC024025
103800             ELSE                                                 GC024025
103900                 NEXT SENTENCE                                    GC024025
104000         ELSE                                                     GC024025
104100         IF GCP-BEN-PROV-FORMAT-E                                 GC024025
104200             IF GPE-BEN-SCOPE-ID = '0000'                         GC024025
104300                 ADD 1                TO  DF-ERROR-COUNT          GC024025
104400*                MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT          GC024025
104500                 MOVE 'B31'      TO  DF-ERR-CODE (DF-ERROR-INDEX) GC024025
104600                 SET DF-ERROR-INDEX   UP  BY 1                    GC024025
104700                 MOVE 'YES'           TO  WS-ERROR-SW             GC024025
104800             ELSE                                                 GC024025
104900                 NEXT SENTENCE.                                   GC024025
105000                                                                  GC024025
105100     IF WS-ACL-ENTRY-FOUND                                        GC024025
105200         MOVE WS-HOLD-ACL-KEY TO TWOA-TAB-KEY                     GC024025
105300         PERFORM 0175-READ-TABULAR THRU 0175-EXIT                 GC024025
105400         MOVE DF-ERROR-COUNT TO WS-DF-ERROR-COUNT                 GC024025
105500         CALL WS-GC024015 USING GC024030-CALL-AREA                GC024025
105600                                TWOA-REC-AREA                     GC024025
105700         SET DF-ERROR-INDEX TO DF-ERROR-COUNT                     GC024025
105800         SET DF-ERROR-INDEX UP BY 1                               GC024025
105900         IF DF-ERROR-COUNT > WS-DF-ERROR-COUNT                    GC024025
106000             MOVE 'YES' TO WS-ERROR-SW.                           GC024025
106100                                                                  GC024025
106200     IF WS-AOL-ENTRY-FOUND                                        GC024025
106300         MOVE WS-HOLD-AOL-KEY TO TWOA-TAB-KEY                     GC024025
106400         PERFORM 0175-READ-TABULAR THRU 0175-EXIT                 GC024025
106500         MOVE DF-ERROR-COUNT TO WS-DF-ERROR-COUNT                 GC024025
106600         CALL WS-GC024015 USING GC024030-CALL-AREA                GC024025
106700                                TWOA-REC-AREA                     GC024025
106800         SET DF-ERROR-INDEX TO DF-ERROR-COUNT                     GC024025
106900         SET DF-ERROR-INDEX UP BY 1                               GC024025
107000         IF DF-ERROR-COUNT > WS-DF-ERROR-COUNT                    GC024025
107100             MOVE 'YES' TO WS-ERROR-SW.                           GC024025
107200                                                                  GC024025
107300 0300-EXIT.                                                       GC024025
107400     EXIT.                                                        GC024025
107500                                                                  GC024025
107600 2000-PROCESS-DFILE.                                              GC024025
107700                                                                  GC024025
107800     ADD 1                TO  DF-ERROR-COUNT.                     GC024025
107900*    MOVE DF-ERROR-COUNT  TO  DF-ERROR-COUNT.                     GC024025
108000     MOVE HIGH-VALUES     TO  DF-ERR-CODE (DF-ERROR-INDEX).       GC024025
108100                                                                  GC024025
108200     IF WS-ERROR-FOUND                                            GC024025
108300         MOVE 'E' TO GC024030-IND                                 GC024025
108400     ELSE                                                         GC024025
108500         MOVE 'G' TO GC024030-IND.                                GC024025
108600                                                                  GC024025
108700     CALL 'GC024030' USING GC024030-CALL-AREA.                    GC024025
108800                                                                  GC024025
108900 2000-EXIT.                                                       GC024025
109000     EXIT.                                                        GC024025
109100                                                                  GC024025
109200 9010-CLOSE-FILES.                                                GC024025
109300                                                                  GC024025
109400     MOVE 'C' TO REQUEST-TYPE-1.                                  GC024025
109500     CALL 'TSGVSAM5' USING PARM-ONE PARM-ONEA.                    GC024025
109600                                                                  GC024025
109700     IF  REQUEST-TYPE-1 NOT = 'C'                                 GC024025
109800         MOVE '9010--BAD CLOSE  BEN-PROV  ' TO VSAM-WS-ERR-MSG    GC024025
109900         MOVE ONEA-FEEDBACK                 TO ABEND-CODE         GC024025
110000         GO TO 9999-ERROR-RTN.                                    GC024025
110100                                                                  GC024025
110200     MOVE 'C' TO REQUEST-TYPE-2.                                  GC024025
110300     CALL 'TSGVSAM4' USING PARM-TWO PARM-TWOA.                    GC024025
110400                                                                  GC024025
110500     IF  REQUEST-TYPE-2 NOT = 'C'                                 GC024025
110600         MOVE '9010--BAD CLOSE  TABULAR   ' TO VSAM-WS-ERR-MSG    GC024025
110700         MOVE TWOA-FEEDBACK                 TO ABEND-CODE         GC024025
110800         DISPLAY VSAM-WS-ERR-MSG                                  GC024025
110900         GO TO 9999-ERROR-RTN.                                    GC024025
111000                                                                  GC024025
111100 9010-EXIT.                                                       GC024025
111200     EXIT.                                                        GC024025
111300                                                                  GC024025
111400 9999-ERROR-RTN.                                                  GC024025
111500                                                                  GC024025
111600     CALL 'TSGEND' USING ABEND-CODE.                              GC024025
111700                                                                  GC024025
111800 9999-EXIT.                                                       GC024025
111900     EXIT.                                                        GC024025
