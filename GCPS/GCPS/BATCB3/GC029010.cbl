000100*      LAST MAINTENANCE TIME: 11.44.50  DATE: 11/13/84            12/09/02
000200 IDENTIFICATION DIVISION.                                         GC029010
000300 PROGRAM-ID. GC029010.                                               LV002
000400 AUTHOR. ROBERT MANN - DECISION CONSULTANTS INC.                  GC029010
000500 INSTALLATION. HCSC.                                              GC029010
000600 DATE-WRITTEN.  JUL 06,1984                                       GC029010
000700 DATE-COMPILED.                                                   GC029010
000800***********************************************************       GC029010
000900*    THIS PROGRAM IS A SUB MODULE TO PROGRAM GC029000             GC029010
001000*    IT UPDATES THE SLOT NUMBERS ON THE RELEASED                  GC029010
001100*    GRP-SPEC RECORD FILE FOR TABULAR RECORDS.                    GC029010
001200******************************************************************GC029010
001300*                                                                *GC029010
001400*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC029010
001500*       *-*         U P D A T E   H I S T O R Y         *-*      *GC029010
001600*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC029010
001700*                                                                *GC029010
001800**-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*GC029010
001900*                                                                *GC029010
002000*   11154     3/06/91  FRY   INCREASE RECORD AREA IN FILE        *GC029010
002100*                            SECTION:                            *GC029010
002200*                      TAB-REC-DATA.                             *GC029010
002300*                       FILLER    PIC X(3990)  CHANGED TO  7795. *GC029010
002400*                                                                *GC029010
002500*D12009 09/19/91  TPM   ADJUSTED THE CRGT-REC-KEY TO ACCOMODATE  *GC029010
002600*                       FOR THE EXPANSION OF THE FAMILY RELATION *GC029010
002700*                       FIELD                                    *GC029010
002800*                                                                *GC029010
002900*            01/17/95  GDM   CONVERT TO COBOL II                 *GC029010
003000*                                                                *GC029010
003100*14726/ 10/08/97  DAU   ADDED CODE TO SUPPORT THE YEAR 2000 AND  *GC029010
003200*                       THE EXPANSION OF THE GROUP SPECIFIC AND  *GC029010
003300*                       CONTRACT KEY TO SUPPORT THE TEXAS MERGER.*GC029010
003400*            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GC029010
003500*                                                                *GC029010
003510*  DM09441   09-17-09   DNK   ADJUSTED FILLER AREA FOR THE FILE  *GC029010
003520*                             EXPANSION FROM 7795 TO 31360.      *GC029010
      *                                                                *GC029000
      * P20368     09/11/15  KIKI COMPILE ONLY - BD / TC               *GC029000
      *                           EXTEND - GCGROUPC                    *GC029000
      *                                                                *GC029000
      * P22147     05/03/17  TROY COMPILE ONLY - CE                    *GC029000
      *                           EXTEND - GCGROUPC                    *GC029000
      *                                                                *GC029000
      * P21681     09/28/17  SRI  COMPILE ONLY - UTIL-MANAGEMENT       *GC029000
      *                           IND    - GCGROUPC                    *GC029000
      *                                                                *GC029000
      ******************************************************************GC029010
003700                                                                  GC029010
003800 ENVIRONMENT DIVISION.                                            GC029010
003900 CONFIGURATION SECTION.                                           GC029010
004000 SOURCE-COMPUTER. IBM-370.                                        GC029010
004100 OBJECT-COMPUTER. IBM-370.                                        GC029010
004200 INPUT-OUTPUT SECTION.                                            GC029010
004300 FILE-CONTROL.                                                    GC029010
004400     SELECT COMP-RLSE-GRP-TAB-FILE                                GC029010
004500                             ASSIGN TO UT-S-GC0290B.              GC029010
004600     EJECT                                                        GC029010
004700 DATA DIVISION.                                                   GC029010
004800 FILE SECTION.                                                    GC029010
004900                                                                  GC029010
005000 FD  COMP-RLSE-GRP-TAB-FILE                                       GC029010
005100     LABEL RECORDS ARE STANDARD                                   GC029010
005200     RECORDING MODE IS V                                          GC029010
005300     BLOCK CONTAINS 0 RECORDS.                                    GC029010
005400 01  COMP-RLSE-GRP-TAB-REC.                                       GC029010
005500     05  CRGT-REC-KEY.                                            GC029010
005600         10  CRGT-MATCH-FLDS.                                     GC029010
005700             15  CRGT-MATCH              PIC X(30).               GC029010
005800             15  CRGT-REC-TYPE           PIC X(2).                GC029010
005900             15  CRGT-BEN-ID-SLOT.                                GC029010
006000                 20  CRGT-BEN-ID         PIC X(6).                GC029010
006100                 20  CRGT-BEN-SLOT       PIC S9(7)   COMP-3.      GC029010
006200             15  CRGT-TAB-ID-SLOT.                                GC029010
006300                 20  CRGT-TAB-ID         PIC X(6).                GC029010
006400                 20  CRGT-TAB-SLOT       PIC S9(7) COMP-3.        GC029010
006500             15  FILLER                  PIC X(5).                GC029010
006600         10  FILLER                      PIC X(43).               GC029010
006700     05  TAB-REC-DATA.                                            GC029010
006800         10  TAB-ID-SLOT.                                         GC029010
006900             15  TAB-ID      PIC X(6).                            GC029010
007000             15  TAB-SLOT-NO PIC S9(7)       COMP-3.              GC029010
007100         10  FILLER          PIC X(31360).                        GC029010
007200     EJECT                                                        GC029010
007300 WORKING-STORAGE SECTION.                                         GC029010
007400                                                                  GC029010
007500 01  FILLER          PIC X(24)   VALUE                            GC029010
007600                     'GC029010 WORKING STORAGE'.                  GC029010
007700                                                                  GC029010
007800 01  SWITCH-AREA.                                                 GC029010
007900     05  CRGT-SW     PIC X   VALUE SPACES.                        GC029010
008000         88  EOF-CRGT        VALUE HIGH-VALUES.                   GC029010
008100                                                                  GC029010
008200 01  ABEND-CODE      PIC 9(4)    COMP.                            GC029010
008300                                                                  GC029010
008400 01  TAB-ID-TEST.                                                 GC029010
008500     05  TAB-POS-1   PIC X       VALUE SPACES.                    GC029010
008600         88  TAB-ID-VALID        VALUE '#'.                       GC029010
008700     05  FILLER      PIC X(5)    VALUE SPACES.                    GC029010
008800     EJECT                                                        GC029010
008900 LINKAGE SECTION.                                                 GC029010
009000                                                                  GC029010
009100 01  GC029010-IND        PIC X.                                   GC029010
009200                                                                  GC029010
009300 01  RLSE-GRP-REC.                                                GC029010
009400     COPY GCWRKDCC.                                               GC029010
009500     COPY GCGROUPC.                                               GC029010
009600     EJECT                                                        GC029010
009700 PROCEDURE DIVISION USING GC029010-IND  RLSE-GRP-REC .            GC029010
009800                                                                  GC029010
009900 0000-MAINLINE.                                                   GC029010
010000                                                                  GC029010
010100     IF  GC029010-IND EQUAL 'O'                                   GC029010
010200         MOVE SPACES             TO GC029010-IND                  GC029010
010300         OPEN INPUT COMP-RLSE-GRP-TAB-FILE                        GC029010
010400         PERFORM 0010-READ-CRGT-REC THRU 0010-EXIT.               GC029010
010500                                                                  GC029010
010600     PERFORM 0020-MATCH THRU 0020-EXIT                            GC029010
010700         UNTIL EOF-CRGT OR                                        GC029010
010800         (CRGT-MATCH GREATER THAN WRK-MATCH-CONT).                GC029010
010900                                                                  GC029010
011000     PERFORM 0030-VALID-TAB-ID THRU 0030-EXIT                     GC029010
011100         VARYING GCG-INDEX FROM 1 BY 1 UNTIL                      GC029010
011200         GCG-INDEX GREATER THAN GCG-COUNT-TAB-PROVN-POINTERS.     GC029010
011300                                                                  GC029010
011400     IF  EOF-CRGT                                                 GC029010
011500         MOVE HIGH-VALUES        TO GC029010-IND                  GC029010
011600         CLOSE COMP-RLSE-GRP-TAB-FILE.                            GC029010
011700                                                                  GC029010
011800     GOBACK.                                                      GC029010
011900     EJECT                                                        GC029010
012000 0010-READ-CRGT-REC.                                              GC029010
012100                                                                  GC029010
012200     READ COMP-RLSE-GRP-TAB-FILE                                  GC029010
012300         AT END                                                   GC029010
012400             MOVE HIGH-VALUES    TO CRGT-SW.                      GC029010
012500                                                                  GC029010
012600 0010-EXIT.                                                       GC029010
012700     EXIT.                                                        GC029010
012800     EJECT                                                        GC029010
012900 0020-MATCH.                                                      GC029010
013000                                                                  GC029010
013100     IF  WRK-MATCH-CONT GREATER THAN CRGT-MATCH                   GC029010
013200         MOVE 1120           TO ABEND-CODE                        GC029010
013300         DISPLAY '*** ABEND OCCURRED ***'                         GC029010
013400         DISPLAY 'ABEND CODE  =' ABEND-CODE                       GC029010
013500         PERFORM 1000-DISP-FIELDS THRU 1000-EXIT                  GC029010
013600         GO TO 9999-ERROR-RTN.                                    GC029010
013700                                                                  GC029010
013800     SET GCG-INDEX TO 1.                                          GC029010
013900     SEARCH GCG-GRP-SPEC-TAB-ID                                   GC029010
014000         AT END                                                   GC029010
014100             MOVE 1125       TO ABEND-CODE                        GC029010
014200             DISPLAY '*** ABEND OCCURRED ***'                     GC029010
014300             DISPLAY 'ABEND CODE  =' ABEND-CODE                   GC029010
014400             DISPLAY 'TABULAR NOT FOUND IN GROUP'                 GC029010
014500             DISPLAY 'TABULAR ID  =' TAB-ID                       GC029010
014600             PERFORM 1000-DISP-FIELDS THRU 1000-EXIT              GC029010
014700             GO TO 9999-ERROR-RTN                                 GC029010
014800         WHEN                                                     GC029010
014900             GCG-TAB-ID (GCG-INDEX) EQUAL TAB-ID                  GC029010
015000                 MOVE TAB-SLOT-NO                                 GC029010
015100                             TO GCG-TAB-SLOT-NO (GCG-INDEX).      GC029010
015200                                                                  GC029010
015300     PERFORM 0010-READ-CRGT-REC THRU 0010-EXIT.                   GC029010
015400                                                                  GC029010
015500 0020-EXIT.                                                       GC029010
015600     EXIT.                                                        GC029010
015700     EJECT                                                        GC029010
015800 0030-VALID-TAB-ID.                                               GC029010
015900                                                                  GC029010
016000     MOVE GCG-TAB-ID (GCG-INDEX)                                  GC029010
016100                                 TO TAB-ID-TEST.                  GC029010
016200                                                                  GC029010
016300     IF  TAB-ID-VALID AND                                         GC029010
016400         GCG-TAB-SLOT-NO (GCG-INDEX) GREATER THAN 8999999         GC029010
016500             MOVE 1130           TO ABEND-CODE                    GC029010
016600             DISPLAY '*** ABEND OCCURRED ***'                     GC029010
016700             DISPLAY 'ABEND CODE  =' ABEND-CODE                   GC029010
016800             DISPLAY 'TABULAR WORK RECORD NOT FOUND'              GC029010
016900             DISPLAY 'TABULAR ID  =' GCG-TAB-ID (GCG-INDEX)       GC029010
017000             PERFORM 1000-DISP-FIELDS THRU 1000-EXIT              GC029010
017100             GO TO 9999-ERROR-RTN.                                GC029010
017200                                                                  GC029010
017300 0030-EXIT.                                                       GC029010
017400     EXIT.                                                        GC029010
017500     EJECT                                                        GC029010
017600 1000-DISP-FIELDS.                                                GC029010
017700     DISPLAY 'PLAN CODE            =' GCG-PLAN-CODE.              GC029010
017800     DISPLAY 'GROUP NUMBER         =' GCG-GROUP-NUM.              GC029010
017900     DISPLAY 'SECTION NUMBER       =' GCG-SECTION-NUM.            GC029010
018000     DISPLAY 'PACKAGE CODE         =' GCG-PKG-CODE.               GC029010
018100     DISPLAY 'FAMILY RELATION      =' GCG-FAM-REL-LVL.            GC029010
018200     DISPLAY 'EFFECTIVE DATE       =' GCG-EFFDT-CEN.              GC029010
018300     DISPLAY '*** YOU ARE IN PROGRAM GC029010TS ***'.             GC029010
018400 1000-EXIT.                                                       GC029010
018500     EXIT.                                                        GC029010
018600 9999-ERROR-RTN.                                                  GC029010
018700                                                                  GC029010
018800     CALL 'TSGEND' USING ABEND-CODE.                              GC029010
018900                                                                  GC029010
019000 9999-EXIT.                                                       GC029010
019100     EXIT.                                                        GC029010
