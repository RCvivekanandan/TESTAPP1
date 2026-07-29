000100 IDENTIFICATION DIVISION.                                         12/09/02
000200 PROGRAM-ID. GC0015.                                              GC0015  
000300 AUTHOR. NINA CERVANTES.                                             LV002
000400 INSTALLATION. HCSC.                                              GC0015  
000500 DATE-WRITTEN. MAY, 1985.                                        GC0015   
000600 DATE-COMPILED.                                                   GC0015  
000700                                                                  GC0015  
000800******************************************************************GC0015  
000900*                                                                 GC0015  
001000*    THIS PROGRAM MODIFIED BY FRANK GARRETT - AUGUST 1985.        GC0015  
001100*    THIS PROGRAM MODIFIED BY FRANK GARRETT - OCTOBER 1985.       GC0015  
001200*      (CHECK FOR DELETES - WRK-SIGNAL-FROM-ONLINE = 'D')         GC0015  
001300*                                                                 GC0015  
001400*  PD00X  07/30/90  ENW  COMMENTED OUT THE ABENDS FOR PRODUCTION  GC0015  
001500*                        RECORDS NOT FOUND.                       GC0015  
001600*                                                                 GC0015  
001700******************************************************************GC0015  
001800*                                                                 GC0015  
001900*    THIS PROGRAM READS THE RELEASE CONTROL RECORD/CANCEL RECORD  GC0015  
002000*    FILE CREATED IN GC0010.  THIS FILE WILL BE READ SEQUENTIALY  GC0015  
002100*    BYPASSING THOSE RECORDS THAT ARE IN SKELETON FORMAT (NO      GC0015  
002200*    UPDATES WERE PROCESSED AGAINST FOR THAT DAY) OR ARE IN       GC0015  
002300*    CANCELLED STATE.  FOR EACH TYPE, 'C1' CONTRACT CONTROL AND   GC0015  
002400*    'G1' GROUP SPECIFIC CONTROL, A BEFORE IMAGE WILL BE COPIED   GC0015  
002500*    FROM THEIR PRODUCTION COUNTERPART AND WRITTEN OUT TO THEIR   GC0015  
002600*    INDIVIDUAL FILE.                                             GC0015  
002700*                                                                 GC0015  
002800*    INPUT:    TSGVSAM1 IS ONLINE CONTRACT                        GC0015  
002900*              TSGVSAM2 IS ONLINE GROUP SPECIFIC CONTROL          GC0015  
003000*                                                                 GC0015  
003100*              RELEASE CONTROL/CANCEL RECORD                      GC0015  
003200*                  READ USING BALX.                               GC0015  
003300*                                                                 GC0015  
003400*                                                                 GC0015  
003500*    OUTPUT:   CONTRACT CONTROL BEFORE IMAGE                      GC0015  
003600*              GROUP SPECIFIC CONTROL BEFORE IMAGE                GC0015  
003700*                                                                 GC0015  
003800******************************************************************GC0015  
003900/*****************************************************************GC0015  
004000* LOG #    DATE    WHO               DESCRIPTION                 *GC0015  
004100* -----  --------  ---  ---------------------------------------- *GC0015  
004200* 11161   11/05/90 GDM  1. CHANGED THE RECORD CONTAIN CLAUSE IN  *GC0015  
004300*                          GCG-FILE FD TO 964.                   *GC0015  
004400*                       2. CHANGED THE GCG-MAX-RECORD TO 964.    *GC0015  
004500* D12009  09/10/91 GDM     INCREASE WS-DISP-FRL TO 3 POSITIONS   *GC0015  
004600*                                                                *GC0015  
004700* D12009  09/19/91 TPM                                           *GC0015  
004800*                       EXPANSION OF THE FAMILY-RELATION FIELD.  *GC0015  
004900*                       CHANGED THE RECORD LENGTH FROM 21 TO 22  *GC0015  
005000*                       CHANGED THE RECORD LENGTH FROM 18 TO 19  *GC0015  
005100*                       WHEN CALLING THE TSGVSAM ROUTINE.        *GC0015  
005200*                                                                *GC0015  
005300*          1/5/95  EMS/GDM  CHANGE TO COBOL II.                  *GC0015  
005400*                                                                *GC0015  
005500* D14045  03/08/95 KJD  RECOMPILE, CHANGE \
005600*                       \
005700*                                                                *GC0015  
005800* 14726/15057                                                    *GC0015  
005900*         09/11/97 DAU  ADDED CODE TO SUPPORT THE YEAR 2000 AND  *GC0015  
006000*                       THE EXPANSION OF THE GROUP SPECIFIC AND  *GC0015  
006100*                       CONTRACT KEY TO SUPPORT THE TEXAS MERGER.*GC0015  
006200*                                                                *GC0015  
006300*            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GC0015  
006400*                                                                *GC0015  
006410*            02-11-10   DNK   ABEND FIX FOR CRS TABULAR EXPANSION*GC0015  
006420*                                                                *GC0015  
006500/*****************************************************************GC0015  
006600                                                                  GC0015  
006700 ENVIRONMENT DIVISION.                                            GC0015  
006800 CONFIGURATION SECTION.                                           GC0015  
006900 SOURCE-COMPUTER. IBM-370.                                        GC0015  
007000 OBJECT-COMPUTER. IBM-370.                                        GC0015  
007100 INPUT-OUTPUT SECTION.                                            GC0015  
007200 FILE-CONTROL.                                                    GC0015  
007300     SELECT GCC-FILE                                              GC0015  
007400         ASSIGN TO UT-S-GC0015B.                                  GC0015  
007500     SELECT GCG-FILE                                              GC0015  
007600         ASSIGN TO UT-S-GC0015C.                                  GC0015  
007700/                                                                 GC0015  
007800 DATA DIVISION.                                                   GC0015  
007900 FILE SECTION.                                                    GC0015  
008000 FD  GCC-FILE                                                     GC0015  
008100     LABEL RECORD STANDARD                                        GC0015  
008200     RECORDING MODE V                                             GC0015  
008300     RECORD CONTAINS 573 TO 8213 CHARACTERS                       GC0015  
008400     BLOCK CONTAINS 0 RECORDS.                                    GC0015  
008500                                                                  GC0015  
008600 01  GCC-MIN-RECORD          PIC X(573).                          GC0015  
008700                                                                  GC0015  
008800 01  GCC-MAX-RECORD          PIC X(8213).                         GC0015  
008900                                                                  GC0015  
009000 01  GCC-RECORD.                                                  GC0015  
009100     03  GCC-KEY             PIC X(100).                          GC0015  
009200     03  GCC-REC.                                                 GC0015  
009300     COPY GCCONTRC.                                               GC0015  
009400                                                                  GC0015  
009500 FD  GCG-FILE                                                     GC0015  
009600     LABEL RECORD STANDARD                                        GC0015  
009700     RECORDING MODE V                                             GC0015  
009800     RECORD CONTAINS 610 TO 1300 CHARACTERS                       GC0015  
009900     BLOCK CONTAINS 0 RECORDS.                                    GC0015  
010000                                                                  GC0015  
010100 01  GCG-MIN-RECORD          PIC X(610).                          GC0015  
010200                                                                  GC0015  
010300 01  GCG-MAX-RECORD          PIC X(1300).                         GC0015  
010400                                                                  GC0015  
010500 01  GCG-RECORD.                                                  GC0015  
010600     03  GCG-KEY             PIC X(100).                          GC0015  
010700     03  GCG-REC.                                                 GC0015  
010800     COPY GCGROUPC.                                               GC0015  
010900                                                                  GC0015  
011000/                                                                 GC0015  
011100 WORKING-STORAGE SECTION.                                         GC0015  
011200                                                                  GC0015  
011300 01  FILLER                      PIC X(40)   VALUE                GC0015  
011400     '***GC0015 WORKING STORAGE BEGINS HERE***'.                  GC0015  
011500 01  ABEND-CODE                  PIC 9(4)    COMP.                GC0015  
011600 01  WS-BEN-COUNT                PIC 9(5).                        GC0015  
011700                                                                  GC0015  
011800 01  WS-DISP-KEY.                                                 GC0015  
011900     05  WS-DISP-PLAN-CODE       PIC X(4)  VALUE SPACES.          GC0015  
012000     05  WS-DISP-GROUP           PIC X(10) VALUE SPACES.          GC0015  
012100     05  WS-DISP-SECTN           PIC X(6)  VALUE SPACES.          GC0015  
012200     05  WS-DISP-PKG-CODE        PIC X(4)  VALUE SPACES.          GC0015  
012300     05  WS-DISP-LOB             PIC X(2)  VALUE SPACES.          GC0015  
012400     05  WS-DISP-PRV             PIC X(3)  VALUE SPACES.          GC0015  
012500     05  WS-DISP-FRL             PIC X(3)  VALUE SPACES.          GC0015  
012600     05  WS-DISP-EFFDT           PIC 9(7)  VALUE ZEROS.           GC0015  
012700 01  PARM-1.                                                      GC0015  
012800     03  1-RESERVED-FLDS         PIC 9(8)    VALUE ZEROS COMP.    GC0015  
012900     03  1-RESERV-FLDS REDEFINES 1-RESERVED-FLDS.                 GC0015  
013000         05  1-REQUEST-TYPE      PIC X.                           GC0015  
013100         05  FILLER              PIC X(3).                        GC0015  
013200                                                                  GC0015  
013300 01  PARM-1A.                                                     GC0015  
013400     03  1A-RDW.                                                  GC0015  
013500         05  1A-REC-LENG         PIC 9(4)    VALUE ZEROS COMP.    GC0015  
013600         05  1A-FEEDBACK         PIC 9(4)    VALUE ZEROS COMP.    GC0015  
013700     03  1A-REC-AREA.                                             GC0015  
013800     COPY GCCONTR2.                                               GC0015  
013900                                                                  GC0015  
014000 01  PARM-2.                                                      GC0015  
014100     03  2-RESERVED-FLDS         PIC 9(8)    VALUE ZEROS COMP.    GC0015  
014200     03  2-RESERV-FLDS REDEFINES 2-RESERVED-FLDS.                 GC0015  
014300         05  2-REQUEST-TYPE      PIC X.                           GC0015  
014400         05  FILLER              PIC X(3).                        GC0015  
014500                                                                  GC0015  
014600 01  PARM-2A.                                                     GC0015  
014700     03  2A-RDW.                                                  GC0015  
014800         05  2A-REC-LENG         PIC 9(4)    VALUE ZEROS COMP.    GC0015  
014900         05  2A-FEEDBACK         PIC 9(4)    VALUE ZEROS COMP.    GC0015  
015000     03  2A-REC-AREA.                                             GC0015  
015100     COPY GCGROUP2.                                               GC0015  
015200                                                                  GC0015  
015300 01  PARM-SET.                                                    GC0015  
015400     03  SET-RDW.                                                 GC0015  
015500         05  SET-REC-LENG        PIC 9(4)    VALUE ZEROS COMP.    GC0015  
015600         05  SET-FEEDBACK        PIC 9(4)    VALUE ZEROS COMP.    GC0015  
015700     03  SET-VALUE               PIC 9(8)    VALUE ZEROS COMP.    GC0015  
015800                                                                  GC0015  
015900                                                                  GC0015  
016000                                                                  GC0015  
016100 01  REQUEST-TYPE    PIC  X     VALUE '0'.                        GC0015  
016200     88  END-OF-FILE      VALUE '5'.                              GC0015  
016300 01  FILE-INDEX      PIC  9(4)   COMP SYNC  VALUE ZEROES.         GC0015  
016400 01  AREA-I-FIXED    PIC  X(31474).                               GC0015  
016410*01  AREA-I-FIXED    PIC  X(8000).                                GC0015  
016500 01  AREA-I REDEFINES AREA-I-FIXED.                               GC0015  
016600     COPY GCWRKDCC.                                               GC0015  
016700     COPY GCCCRDCC.                                               GC0015  
016800 01  AREA-I-LENG     PIC 9(4)    COMP SYNC  VALUE ZEROES.         GC0015  
016900 01  AREA-O          PIC X.                                       GC0015  
017000 01  AREA-O-LENG     PIC 9(4)    COMP SYNC  VALUE ZEROES.         GC0015  
017100 01  INPDDNAME       PIC X(8)               VALUE 'GC0015A '.     GC0015  
017200                                                                  GC0015  
017300                                                                  GC0015  
017400 LINKAGE SECTION.                                                 GC0015  
017500/                                                                 GC0015  
017600 PROCEDURE DIVISION.                                              GC0015  
017700                                                                  GC0015  
017800 000-HOUSEKEEPING.                                                GC0015  
017900     OPEN OUTPUT GCC-FILE                                         GC0015  
018000                 GCG-FILE.                                        GC0015  
018100                                                                  GC0015  
018200                                                                  GC0015  
018300     MOVE 'S'                    TO 1-REQUEST-TYPE.               GC0015  
018400     MOVE 8                      TO SET-REC-LENG.                 GC0015  
018500     MOVE 3                      TO SET-VALUE.                    GC0015  
018600     CALL 'TSGVSAM1' USING PARM-1 PARM-SET.                       GC0015  
018700     IF  1-REQUEST-TYPE NOT EQUAL 'S'                             GC0015  
018800         MOVE SET-FEEDBACK       TO ABEND-CODE                    GC0015  
018900         GO TO 0900-ERROR-RTN.                                    GC0015  
019000                                                                  GC0015  
019100     MOVE 'S'                    TO 2-REQUEST-TYPE.               GC0015  
019200     MOVE 8                      TO SET-REC-LENG.                 GC0015  
019300     MOVE 3                      TO SET-VALUE.                    GC0015  
019400     CALL 'TSGVSAM2' USING PARM-2 PARM-SET.                       GC0015  
019500     IF  2-REQUEST-TYPE NOT EQUAL 'S'                             GC0015  
019600         MOVE SET-FEEDBACK       TO ABEND-CODE                    GC0015  
019700         GO TO 0900-ERROR-RTN.                                    GC0015  
019800************************************************************      GC0015  
019900*** 07/30/90  ENW  COMMENTED OUT.                                 GC0015  
020000*    MOVE 'O'                    TO 1-REQUEST-TYPE.               GC0015  
020100*    CALL 'TSGVSAM1' USING PARM-1 PARM-1A.                        GC0015  
020200*    IF  1-REQUEST-TYPE NOT EQUAL 'O'                             GC0015  
020300*        MOVE 1A-FEEDBACK        TO ABEND-CODE                    GC0015  
020400*        GO TO 0900-ERROR-RTN.                                    GC0015  
020500*                                                                 GC0015  
020600*    MOVE 'O'                    TO 2-REQUEST-TYPE.               GC0015  
020700*    CALL 'TSGVSAM2' USING PARM-2 PARM-2A.                        GC0015  
020800*    IF  2-REQUEST-TYPE NOT EQUAL 'O'                             GC0015  
020900*        MOVE 2A-FEEDBACK        TO ABEND-CODE                    GC0015  
021000*        GO TO 0900-ERROR-RTN.                                    GC0015  
021100************************************************************      GC0015  
021200                                                                  GC0015  
021300     MOVE +1  TO GCT-COUNT-BEN-PROVN-POINTERS.                    GC0015  
021400                                                                  GC0015  
021500     MOVE +1  TO GCG-COUNT-TAB-PROVN-POINTERS.                    GC0015  
021600                                                                  GC0015  
021700     PERFORM 0100-MAINLINE THRU 0100-EXIT                         GC0015  
021800         UNTIL END-OF-FILE.                                       GC0015  
021900                                                                  GC0015  
022000 000-CLOSE-VS1.                                                   GC0015  
022100                                                                  GC0015  
022200     MOVE 'C'                    TO 1-REQUEST-TYPE.               GC0015  
022300     CALL 'TSGVSAM1' USING PARM-1 PARM-1A.                        GC0015  
022400     IF  1-REQUEST-TYPE NOT EQUAL 'C'                             GC0015  
022500         MOVE 1A-FEEDBACK        TO ABEND-CODE                    GC0015  
022600         GO TO 0900-ERROR-RTN.                                    GC0015  
022700                                                                  GC0015  
022800 000-CLOSE-VS2.                                                   GC0015  
022900     MOVE 'C'                    TO 2-REQUEST-TYPE.               GC0015  
023000     CALL 'TSGVSAM2' USING PARM-2 PARM-2A.                        GC0015  
023100     IF  2-REQUEST-TYPE NOT EQUAL 'C'                             GC0015  
023200         MOVE 2A-FEEDBACK        TO ABEND-CODE                    GC0015  
023300         GO TO 0900-ERROR-RTN.                                    GC0015  
023400                                                                  GC0015  
023500 000-CLOSE-BALX.                                                  GC0015  
023600     MOVE '4'                    TO REQUEST-TYPE.                 GC0015  
023700     PERFORM 0200-ISSUE-BALX-CALL THRU 0200-EXIT.                 GC0015  
023800                                                                  GC0015  
023900 000-CLOSE-GCG.                                                   GC0015  
024000     CLOSE GCG-FILE.                                              GC0015  
024100                                                                  GC0015  
024200 000-CLOSE-GCC.                                                   GC0015  
024300     CLOSE GCC-FILE.                                              GC0015  
024400                                                                  GC0015  
024500 000-ALL-DONE.                                                    GC0015  
024600     STOP RUN.                                                    GC0015  
024700 000-EXIT.     EXIT.                                              GC0015  
024800                                                                  GC0015  
024900/                                                                 GC0015  
025000*************************************************************     GC0015  
025100*     READ THE RELEASE CONTROL/CANCEL FILE CREATED FROM           GC0015  
025200*     GC0010 AND PROCESS THOSE RECORDS THAT ARE NOT IN A          GC0015  
025300*     CANCELLED STATE AND DETERMINE WHETHER THE RECORD            GC0015  
025400*     IS CONTRACT OR GROUP SPECIFIC AND PROCESS ACCORDINGLY.      GC0015  
025500*************************************************************     GC0015  
025600                                                                  GC0015  
025700                                                                  GC0015  
025800 0100-MAINLINE.                                                   GC0015  
025900     MOVE '1'                    TO REQUEST-TYPE.                 GC0015  
026000                                                                  GC0015  
026100     PERFORM 0200-ISSUE-BALX-CALL THRU 0200-EXIT.                 GC0015  
026200                                                                  GC0015  
026300     IF END-OF-FILE                                               GC0015  
026400         GO TO 0100-EXIT.                                         GC0015  
026500                                                                  GC0015  
026600*    DISPLAY 'WRK-CANCEL-REQUEST = ' WRK-SIGNAL-FROM-ONLINE.      GC0015  
026700*    DISPLAY 'WRK-REC-CONT-CON   = ' WRK-REC-TYPE.                GC0015  
026800*    DISPLAY 'CCR-CON-SKELETON   = ' CCR-MAPFROM-CON-TYPE.        GC0015  
026900*    DISPLAY 'WRK-REC-GROUP-SPEC-CTL = ' WRK-REC-TYPE.            GC0015  
027000*    DISPLAY 'CCR-GRPSPEC-SKELETON   = ' CCR-MAPFROM-GRP-SPEC-TYPEGC0015  
027100*    DISPLAY 'AREA-I                 = ' AREA-I.                  GC0015  
027200     IF (WRK-ADD-REQUEST OR WRK-CHANGE-REQUEST)                   GC0015  
027300         IF  WRK-REC-CONT-CON  AND  NOT CCR-CON-SKELETON          GC0015  
027400             PERFORM 0300-PROCESS-CON THRU 0300-EXIT              GC0015  
027500         ELSE                                                     GC0015  
027600             IF WRK-REC-GROUP-SPEC-CTL  AND                       GC0015  
027700                            NOT CCR-GRPSPEC-SKELETON              GC0015  
027800             PERFORM 0400-PROCESS-GRP-SPEC THRU 0400-EXIT.        GC0015  
027900                                                                  GC0015  
028000                                                                  GC0015  
028100 0100-EXIT.                                                       GC0015  
028200     EXIT.                                                        GC0015  
028300/                                                                 GC0015  
028400 0200-ISSUE-BALX-CALL.                                            GC0015  
028500                                                                  GC0015  
028600     CALL 'BALX' USING REQUEST-TYPE                               GC0015  
028700                       FILE-INDEX                                 GC0015  
028800                       AREA-I                                     GC0015  
028900                       AREA-I-LENG                                GC0015  
029000                       AREA-O                                     GC0015  
029100                       AREA-O-LENG                                GC0015  
029200                       INPDDNAME.                                 GC0015  
029300                                                                  GC0015  
029400 0200-EXIT.  EXIT.                                                GC0015  
029500/                                                                 GC0015  
029600***********************************************************       GC0015  
029700*   GIVEN THE PRODUCTION KEY FROM THE WORK FILE, RETRIEVE         GC0015  
029800*   THE PRODUCTION CONTRACT AND MOVE THE COMPLETE WORK KEY        GC0015  
029900*   FROM INPUT ALONG WITH THE PRODUCTION RECORD AND WRITE         GC0015  
030000*   IT OUT.  THIS WILL CAPTURE THE BEFORE IMAGE.                  GC0015  
030100***********************************************************       GC0015  
030200                                                                  GC0015  
030300                                                                  GC0015  
030400 0300-PROCESS-CON.                                                GC0015  
030500     MOVE 33 TO 1A-REC-LENG.                                      GC0015  
030600     MOVE 'R'                    TO 1-REQUEST-TYPE.               GC0015  
030700                                                                  GC0015  
030800     MOVE CCR-MAPFROM-CON-NO     TO GCT2-CONTRACT-ID.             GC0015  
030900     MOVE ZEROES                 TO GCT2-PLAN-CODE.               GC0015  
031000     CALL 'TSGVSAM1' USING PARM-1 PARM-1A.                        GC0015  
031100     IF  1-REQUEST-TYPE EQUAL '3'                                 GC0015  
031200* 07/30/90 ENW REPLACED ABEND WITH DISPLAY                        GC0015  
031300*        MOVE 0333               TO ABEND-CODE                    GC0015  
031400*        GO TO 0900-ERROR-RTN.                                    GC0015  
031500         MOVE GCT2-PLAN-CODE      TO WS-DISP-PLAN-CODE            GC0015  
031600         MOVE GCT2-GROUP-NUM      TO WS-DISP-GROUP                GC0015  
031700         MOVE GCT2-SECTION-NUM    TO WS-DISP-SECTN                GC0015  
031800         MOVE GCT2-PKG-CODE       TO WS-DISP-PKG-CODE             GC0015  
031900         MOVE GCT2-L-O-B          TO WS-DISP-LOB                  GC0015  
032000         MOVE GCT2-PROVDR-CONTROL TO WS-DISP-PRV                  GC0015  
032100         MOVE GCT2-FAM-REL-LVL    TO WS-DISP-FRL                  GC0015  
032200         MOVE GCT2-EFFDT-CEN      TO WS-DISP-EFFDT                GC0015  
032300         DISPLAY ' '                                              GC0015  
032400         DISPLAY 'CONTRACT MAP FROM KEY NOT FOUND '               GC0015  
032500         DISPLAY 'MAP FROM KEY = ' WS-DISP-KEY                    GC0015  
032600         DISPLAY ' '                                              GC0015  
032700         GO TO 0300-EXIT.                                         GC0015  
032800     IF  1-REQUEST-TYPE NOT EQUAL 'R'                             GC0015  
032900         MOVE 1A-FEEDBACK        TO ABEND-CODE                    GC0015  
033000         GO TO 0900-ERROR-RTN.                                    GC0015  
033100                                                                  GC0015  
033200*    COMPUTE WRK-RECORD-LENGTH = 1A-REC-LENG - 4.                 GC0015  
033300     MOVE WORK-RECORD         TO GCC-KEY.                         GC0015  
033400     MOVE GCT2-COUNT-BEN-PROVN-POINTERS                           GC0015  
033500       TO GCT-COUNT-BEN-PROVN-POINTERS.                           GC0015  
033600     MOVE 1A-REC-AREA         TO GCC-REC.                         GC0015  
033700     MOVE GCT-COUNT-BEN-PROVN-POINTERS TO WS-BEN-COUNT.           GC0015  
033800     WRITE GCC-RECORD.                                            GC0015  
033900 0300-EXIT.                                                       GC0015  
034000     EXIT.                                                        GC0015  
034100/                                                                 GC0015  
034200***********************************************************       GC0015  
034300*   GIVEN THE PRODUCTION KEY FROM THE WORK FILE, RETRIEVE         GC0015  
034400*   THE PRODUCTION GROUP SPECIFIC AND MOVE THE COMPLETE           GC0015  
034500*   COMPLETE WORK KEY FROM INPUT ALONG WITH THE PRODUCTION        GC0015  
034600*   RECORD AND WRITE IT OUT.  THIS WILL CAPTURE THE BEFORE        GC0015  
034700*   IMAGE.                                                        GC0015  
034800***********************************************************       GC0015  
034900                                                                  GC0015  
035000                                                                  GC0015  
035100 0400-PROCESS-GRP-SPEC.                                           GC0015  
035200     MOVE 30 TO 2A-REC-LENG.                                      GC0015  
035300     MOVE 'R'                    TO 2-REQUEST-TYPE.               GC0015  
035400                                                                  GC0015  
035500     MOVE CCR-MAPFROM-GRP-SPEC-NO TO GCG2-GRP-SPECIF-ID.          GC0015  
035600     MOVE ZEROES                  TO GCG2-PLAN-CODE.              GC0015  
035700     CALL 'TSGVSAM2' USING PARM-2 PARM-2A.                        GC0015  
035800     IF  2-REQUEST-TYPE EQUAL '3'                                 GC0015  
035900* 07/30/90 ENW REPLACED ABEND WITH DISPLAY                        GC0015  
036000*        MOVE 0444               TO ABEND-CODE                    GC0015  
036100*        GO TO 0900-ERROR-RTN.                                    GC0015  
036200         MOVE GCG2-PLAN-CODE      TO WS-DISP-PLAN-CODE            GC0015  
036300         MOVE GCG2-GROUP-NUM      TO WS-DISP-GROUP                GC0015  
036400         MOVE GCG2-SECTION-NUM    TO WS-DISP-SECTN                GC0015  
036500         MOVE GCG2-PKG-CODE       TO WS-DISP-PKG-CODE             GC0015  
036600         MOVE SPACES              TO WS-DISP-LOB                  GC0015  
036700         MOVE SPACES              TO WS-DISP-PRV                  GC0015  
036800         MOVE GCG2-FAM-REL-LVL    TO WS-DISP-FRL                  GC0015  
036900         MOVE GCG2-EFFDT-CEN      TO WS-DISP-EFFDT                GC0015  
037000         DISPLAY ' '                                              GC0015  
037100         DISPLAY 'GRPSPEC MAP FROM KEY NOT FOUND '                GC0015  
037200         DISPLAY 'MAP FROM KEY = ' WS-DISP-KEY                    GC0015  
037300         DISPLAY ' '                                              GC0015  
037400         GO TO 0400-EXIT.                                         GC0015  
037500     IF  2-REQUEST-TYPE NOT EQUAL 'R'                             GC0015  
037600         MOVE 2A-FEEDBACK        TO ABEND-CODE                    GC0015  
037700         GO TO 0900-ERROR-RTN.                                    GC0015  
037800                                                                  GC0015  
037900*    COMPUTE WRK-RECORD-LENGTH = 1A-REC-LENG - 4.                 GC0015  
038000     MOVE WORK-RECORD         TO GCG-KEY.                         GC0015  
038100     MOVE GCG2-COUNT-TAB-PROVN-POINTERS                           GC0015  
038200       TO GCG-COUNT-TAB-PROVN-POINTERS.                           GC0015  
038300     MOVE 2A-REC-AREA         TO GCG-REC.                         GC0015  
038400     WRITE GCG-RECORD.                                            GC0015  
038500                                                                  GC0015  
038600 0400-EXIT.                                                       GC0015  
038700     EXIT.                                                        GC0015  
038800/                                                                 GC0015  
038900 0900-ERROR-RTN.                                                  GC0015  
039000                                                                  GC0015  
039100     CALL 'TSGEND' USING ABEND-CODE.                              GC0015  
039200                                                                  GC0015  
039300 0900-EXIT.                                                       GC0015  
039400     EXIT.                                                        GC0015  
