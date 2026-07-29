000100 IDENTIFICATION DIVISION.                                         00000100
000200                                                                  00000200
000300 PROGRAM-ID.     GC03330.                                         00000300
000400 AUTHOR.         NABIL ELBAZ.                                     00000400
000500 INSTALLATION.   HCSC-HCMS.                                       00000500
000600 DATE-WRITTEN.                                                    00000600
000700 DATE-COMPILED.                                                   00000700
000800***************************************************************** 00000800
000900*    THIS PROGRAM WILL ARCHIVE KEY FIELD CHANGES FOR GROUP      * 00000900
001000*    SPECIFIC KEYS AND CONTRACT KEYS. BY READING THE DELETED    * 00001000
001100*    WORKFILE RECODS AFTER THE SPLIT IN GC0010 PROGRAM.         * 00001100
001200*    IF THE KEY IN THE WORK AREA NOT EQUAL THE KEY IN THE       * 00001200
001300*    CONTRACT/GROUP SPC RECORD AREA.                            * 00001300
001400*                                                               * 00001400
001500*    RCRC-FILE IS DEL-CONTRL-REC-CANCEL-REC AFTER THE SPLIT(IN) * 00001500
001600*    TSGVSAM3 IS GCPS ARCHIVED FILE (OUTPUT).                   * 00001600
001700*                                                               * 00001700
001800***************************************************************** 00001800
001900*    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        * 00001900
002000*    *-*         U P D A T E   H I S T O R Y         *-*        * 00002000
002100*    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        * 00002100
002200*                                                               * 00002200
002300**-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* 00002300
002400*                                                               * 00002400
002500*  D0168     05/10/88  NE   INITIAL PROGRAM BUILD               * 00002500
002600*                                                               * 00002600
002700*  D0226     08/29/89  NE  CREATE AN ARCHIVE RECORD FOR CURRENT * 00002700
002800*                          KEY VALUE LIKE THE OLD VALUE RECORD  * 00002800
002900*                          AND PRIM THE ARCH-ATB INDICATOR.     * 00002900
003000*  D0227     09/01/89  NE  CREATE AN ARCHIVE RECORD FOR DELETED * 00003000
003100*                          CONTRACT OR GROUP SPECIFIC.          * 00003100
003200*  P????     09/30/89  ENW FIXED CHECK OF REQUEST-TYPE-A IN     * 00003200
003300*                          0095-DELETE.                         * 00003300
003400*                                                               * 00003400
003500*  11154      3-07-91  FRY  INCREASE RECORD AREA IN THE FILE    * 00003500
003600*                           SECTION:                            * 00003600
003700*                     CONT-DATA  PIC X(5746)  CHANGED TO  7788. * 00003700
003800*                                                               * 00003800
003900*                                                                 00003900
004000*  09/19/91    TPM    EXPANSION OF THE FAMILY-RELATION FIELD.    *00004000
004100*  D12009             REDUCE THE DATA-AREA-A BY ONE BYTE TO      *00004100
004200*                     TO ACCOMODATE  FOR THE ABOVE CHANGE.       *00004200
004300*                     REMOVE HARD CODED RECORD LENGTHS FOR       *00004300
004400*                     ARCHIVE AND INSERTED COPYBOOK MEMBER       *00004400
004500*                     GCCDRLEN  TO BE USED TO INCLUDE NEW RECORD *00004500
004600*                     LENGTHS TO HANDLE THE EXPANSION IN THE     *00004600
004700*                     FAMILY RELATION FIELD.                     *00004700
004800*                                                                *00004800
004900*                     CHANGED THE RECORD LENGTH FROM 30 TO 31    *00004900
005000*                     WHEN CALLING THE TSGVSAM ROUTINE.          *00005000
005100*                                                                *00005100
005200*                     DATA-AREA-A ALSO CORRECTED                 *00005200
005300*                     TO MATCH THE REDEFINE-AREA.                *00005300
005400*                                                                *00005400
005500*  P????     11/21/91  FRY  CHANGED DATA NAME IN COMPUTE TO      *00005500
005600*                           UPDATE ARCHIVE RECORD.               *00005600
005700*          FROM:  + (GC-ARCHIVE-VARY-LEN * ARKY-POINTERS-COUNT)  *00005700
005800*            TO:  + (GC-ARCH-KEY-VARY-LEN * ARKY-POINTERS-COUNT) *00005800
005900*                                                                *00005900
006000*  D00336    02/21/92  GDM  PROGRAM MODIFICATION TO BYPASS       *00006000
006100*                           FLIP-FLOP WRITING OF RECORDS TO      *00006100
006200*                           THE ARCHIVE FILE WHEN THE WORK KEY   *00006200
006300*                           IS A MAP-FROM                        *00006300
006400*                                                                *00006400
006500*             1/17/95  EMS  CONVERTED TO COBOL II.               *00006500
006600*            01/25/95  JGR  CONTRACT RECORD EXPANSION.           *00006600
006700*            11/18/97  PHF2 MILLENIUM EXPANSION                  *00006700
006800*                                                                *00006800
006900* 14726/     01/08/98  GSP  CHANGED RECORD LENGTH FROM 31 TO 42  *00006900
007000* 15057                     FOR USING TSGVSAM FOR ARCHIVE FILE.  *00007000
007100*                                                                *00007100
007200*  PROD      05/17/02  DAF  CHANGED CONT-REC-AREA IN FD TO BE    *00007200
007300*                           8113 INSTEAD OF 10 LEVELS PIC X(18)  *00007300
007400*                           AND PIC X(8100).  THIS ADDED UP TO   *00007400
007500*                           8118 WHICH WAS WRONG.                *00007500
007600*                           CORRECTED CONTRACT KEY COMPARISON TO *00007600
007700*                           BE GROUP TO GROUP INSTEAD OF GROUP   *00007700
007800*                           TO SECTION.  (0030-PROCESS-ARCHIVE)  *00007800
007900*                           ADDED THE COMPARISON OF THE FAMILY   *00007900
008000*                           RELATION CODE TO BE WITHIN THE       *00008000
008100*                           CONTRACT COMPARISON.  (0030-PROCESS- *00008100
008200*                           ARCHIVE)                             *00008200
008300*                                                                *00008300
008400*  08-14-02     GTF       EXPAND OPERATOR ID FROM 5 TO 8 CHARS   *00008400
008500*                                                                *00008500
008600*  10-11-02     AKK       REGEN AFTER COPYMEMBER CHANGE          *00008600
008700*                                                                *00008700
008800*  02-28-07     DAF       CHANGE REC-AREA-A LENGTH TO MATCH THE  *00008800
008900*                         CORRECT LENGTH OF THE 2002 CONVERSION  *00008900
009000*                                                                *00009000
009100*  04-20-10     LR        CHANGE RCRC-FILE LENGTH TO MATCH NEW   *00009100
009200*                         FILE LENGTH AFTER CRS TBLR EXPANSION   *00009200
009300*                                                                *00009300
009400******************************************************************00009400
009500/                                                                 00009500
009600 ENVIRONMENT DIVISION.                                            00009600
009700                                                                  00009700
009800 CONFIGURATION SECTION.                                           00009800
009900 SOURCE-COMPUTER. IBM-370.                                        00009900
010000 OBJECT-COMPUTER. IBM-370.                                        00010000
010100                                                                  00010100
010200                                                                  00010200
010300 INPUT-OUTPUT SECTION.                                            00010300
010400 FILE-CONTROL.                                                    00010400
010500                                                                  00010500
010600     SELECT RCRC-FILE  ASSIGN TO UT-S-GC03330A.                   00010600
010700     SELECT QCF-FILE   ASSIGN TO UT-S-GC03330B.                   00010700
010800                                                                  00010800
010900/                                                                 00010900
011000 DATA DIVISION.                                                   00011000
011100 FILE SECTION.                                                    00011100
011200 FD  RCRC-FILE                                                    00011200
011300     LABEL RECORDS ARE STANDARD                                   00011300
011400     RECORDING MODE IS V                                          00011400
011500     BLOCK CONTAINS 0 RECORDS.                                    00011500
011600 01  RCRC-RECORD.                                                 00011600
011700     COPY GCWRKDCC.                                               00011700
011800*    05  CONT-REC-AREA            PIC X(8113).                    00011800
011900     05  CONT-REC-AREA            PIC X(31370).                   00011900
012000/                                                                 00012000
012100*PHF G&R - ADDED FD FOR QCF                                       00012100
012200 FD  QCF-FILE                                                     00012200
012300     LABEL RECORDS ARE STANDARD                                   00012300
012400     RECORDING MODE IS F                                          00012400
012500     BLOCK CONTAINS 0 RECORDS.                                    00012500
012600 01  QCF-RECORD.                                                  00012600
012700     COPY GCQCF.                                                  00012700
012800/                                                                 00012800
012900 WORKING-STORAGE SECTION.                                         00012900
013000 01  FILLER                         PIC X(42)   VALUE             00013000
013100     '***GC03330 WORKING STORAGE BEGINS HERE***'.                 00013100
013200                                                                  00013200
013300 01  ABEND-CODE                     PIC 9(4)    COMP.             00013300
013400* 8/14/02 EXPAND OPID BY 3 TO 8 BYTES. GTF                        00013400
013500 01  WS-OPER-ID                     PIC X(08) VALUE SPACES.       00013500
013600 01  PRINT-AREA.                                                  00013600
013700     05  ARCH-PRINT-1               PIC ZZZZZ99.                  00013700
013800     05  QCF-PRINT-1                PIC ZZZZZ99.                  00013800
013900                                                                  00013900
014000/                                                                 00014000
014100 01  CNTL-RECORD-AREA.                                            00014100
014200     COPY GCCCRDCC.                                               00014200
014300                                                                  00014300
014400/                                                                 00014400
014500 01  WS-REC-LEN-AREA.                                             00014500
014600     COPY GCCDRLEN.                                               00014600
014700/                                                                 00014700
014800     COPY MLDATE01.                                               00014800
014900 01  JUL-DATE.                                                    00014900
015000     05  JUL-DT.                                                  00015000
015100         10  JUL-CC                 PIC 99.                       00015100
015200         10  JUL-YY                 PIC 99.                       00015200
015300         10  JUL-DD                 PIC 999.                      00015300
015400     05  TODAYS-DATE REDEFINES  JUL-DT PIC 9(7).                  00015400
015500                                                                  00015500
015600 01  DATE-AREA.                                                   00015600
015700     05  GREG-DATE                  PIC 9(8) VALUE ZEROS.         00015700
015800     05  JULIAN-DATE                PIC 9(7) VALUE ZEROS.         00015800
015900     05  WS-CHNG-DATE               PIC 9(7) VALUE ZEROS.         00015900
016000                                                                  00016000
016100 01  EOF-RCRC-SW                    PIC XXX  VALUE SPACES.        00016100
016200     88  END-OF-RCRC                         VALUE 'END'.         00016200
016300                                                                  00016300
016400 01  READ-IND                       PIC XXX VALUE SPACES.         00016400
016500     88  AFTER-FOUND                         VALUE 'YES'.         00016500
016600 01  REC-ID-AREA.                                                 00016600
016700     05  REC-STA              PIC X.                              00016700
016800     05  REC-KEY-VALUE        PIC X(18).                          00016800
016900                                                                  00016900
017000                                                                  00017000
017100                                                                  00017100
017200*** THE FOLLOWING AREA IS THE ARCHIVING FILE I/O ****             00017200
017300 01  PARM-ONE-A.                                                  00017300
017400     05  RESERVED-FLDS-A         PIC 9(8)    VALUE ZEROS COMP.    00017400
017500     05  RESERVED-ONE-A  REDEFINES RESERVED-FLDS-A.               00017500
017600         10  REQUEST-TYPE-A      PIC X.                           00017600
017700         10  FILLER              PIC X(3).                        00017700
017800                                                                  00017800
017900 01  PARM-TWO-A.                                                  00017900
018000     02  RDW-A.                                                   00018000
018100         05  RECORD-LENGTH-A     PIC 9(4)    VALUE ZEROS COMP.    00018100
018200         05  FEEDBACK-CODE-A     PIC 9(4)    VALUE ZEROS COMP.    00018200
018300     02  REC-AREA-A              PIC X(9547).                     00018300
018400     02  RECORD-A REDEFINES REC-AREA-A.                           00018400
018500         10  KEY-FIELD-A.                                         00018500
018600             15  KEY-TYPE-A      PIC X.                           00018600
018700             15  KEY-ID-A        PIC X(29).                       00018700
018800             15  KEY-NO-A        PIC X(8).                        00018800
018900         10  PNTRS-COUNT-A       PIC S9(5) COMP-3.                00018900
019000         10  DATA-AREA-A         PIC X(9506).                     00019000
019100                                                                  00019100
019200                                                                  00019200
019300 01  PARM-SET.                                                    00019300
019400     05  SET-RDW.                                                 00019400
019500         10  SET-RECORD-LENGTH      PIC 9(4) VALUE ZEROS  COMP.   00019500
019600         10  SET-FEEDBACK           PIC 9(4) VALUE ZEROS  COMP.   00019600
019700     05  SET-VALUE                  PIC 9(8) VALUE ZEROS  COMP.   00019700
019800                                                                  00019800
019900                                                                  00019900
020000 01  COUNTER-AREA.                                                00020000
020100     05  BEFORE-TAB-COUNT             PIC 9(2)    COMP.           00020100
020200     05  AFTER-TAB-COUNT              PIC 9(2)    COMP.           00020200
020300     05  ARCH-RECS-BUILD              PIC S9(7)  VALUE ZEROS.     00020300
020400     05  WS-QCF-COUNT                 PIC S9(7)  VALUE ZEROS.     00020400
020500/                                                                 00020500
020600 01  KEY-FLD-ARCH-AREA.                                           00020600
020700 COPY GCARCHKY.                                                   00020700
020800/                                                                 00020800
020900*PHF G&R - ADDED LINKAGE SECTION                                  00020900
021000 LINKAGE SECTION.                                                 00021000
021100 01  PARM-AREA.                                                   00021100
021200     05  PARM-LENGTH      PIC S9(04) COMP.                        00021200
021300     05  PARM-LOCATION    PIC X(03).                              00021300
021400/                                                                 00021400
021500*PHF G&R - ADDED USING PARM-AREA                                  00021500
021600 PROCEDURE DIVISION USING PARM-AREA.                              00021600
021700 0000-MAINLINE.                                                   00021700
021800     OPEN   INPUT      RCRC-FILE.                                 00021800
021900     OPEN   OUTPUT     QCF-FILE.                                  00021900
022000                                                                  00022000
022100*** TSGVSAM3 IS THE GCPS ARCHIVED FILE                            00022100
022200     MOVE   'S'                  TO REQUEST-TYPE-A.               00022200
022300     MOVE    8                   TO SET-RECORD-LENGTH.            00022300
022400     MOVE    3                   TO SET-VALUE.                    00022400
022500     CALL   'TSGVSAM3'  USING PARM-ONE-A PARM-SET.                00022500
022600     IF  REQUEST-TYPE-A NOT EQUAL 'S'                             00022600
022700         MOVE SET-FEEDBACK       TO ABEND-CODE                    00022700
022800         GO TO 9999-ERROR-RTN.                                    00022800
022900                                                                  00022900
023000     MOVE   'O'                  TO REQUEST-TYPE-A.               00023000
023100     CALL   'TSGVSAM3'  USING PARM-ONE-A PARM-TWO-A.              00023100
023200     IF  REQUEST-TYPE-A NOT EQUAL 'O'                             00023200
023300         MOVE SET-FEEDBACK       TO ABEND-CODE                    00023300
023400         GO TO 9999-ERROR-RTN.                                    00023400
023500                                                                  00023500
023600     MOVE 'TDY' TO MLDATE-FUNC.                                   00023600
023700     MOVE 'J' TO MLDATE-FORM1.                                    00023700
023800     CALL 'MLDATE' USING MLDATE01.                                00023800
023900     MOVE MLDATE-JUL1  TO JUL-DATE.                               00023900
024000                                                                  00024000
024100     MOVE '   '     TO EOF-RCRC-SW.                               00024100
024200                                                                  00024200
024300*PHF G&R - ADDED PARM-LENGTH CHECK                                00024300
024400     IF PARM-LENGTH NOT = 3                                       00024400
024500         DISPLAY 'ABENDED ON PARM-LENGTH: ' PARM-LENGTH           00024500
024600         GO TO 9999-ERROR-RTN.                                    00024600
024700                                                                  00024700
024800     PERFORM 0010-PROCESS-RTN  THRU 0010-EXIT                     00024800
024900         UNTIL END-OF-RCRC.                                       00024900
025000                                                                  00025000
025100     MOVE   'C'                  TO REQUEST-TYPE-A.               00025100
025200     CALL   'TSGVSAM3'  USING PARM-ONE-A PARM-TWO-A.              00025200
025300     IF  REQUEST-TYPE-A NOT EQUAL 'C'                             00025300
025400         MOVE FEEDBACK-CODE-A    TO ABEND-CODE                    00025400
025500         GO TO 9999-ERROR-RTN.                                    00025500
025600                                                                  00025600
025700     CLOSE  RCRC-FILE                                             00025700
025800            QCF-FILE.                                             00025800
025900                                                                  00025900
026000     DISPLAY '*** KEY FIELD ARCHIVED AUDIT TRAIL ***'.            00026000
026100     DISPLAY '    -------------------------------        '.       00026100
026200     MOVE ARCH-RECS-BUILD  TO ARCH-PRINT-1.                       00026200
026300     DISPLAY 'TOTAL RECORDS CREATED  = ' ARCH-PRINT-1.            00026300
026400     DISPLAY '*** KEY FIELD QUALITY ASSURANCE    ***'.            00026400
026500     DISPLAY '    -------------------------------        '.       00026500
026600     MOVE WS-QCF-COUNT     TO QCF-PRINT-1                         00026600
026700     DISPLAY 'TOTAL RECORDS CREATED  = ' QCF-PRINT-1.             00026700
026800     GOBACK.                                                      00026800
026900                                                                  00026900
027000 0000-EXIT.                                                       00027000
027100      EXIT.                                                       00027100
027200/                                                                 00027200
027300 0010-PROCESS-RTN.                                                00027300
027400                                                                  00027400
027500*** READ DELETE-CANCEL WORK FILE RECORDS ***                      00027500
027600     READ   RCRC-FILE                                             00027600
027700                AT END                                            00027700
027800                  MOVE 'END'     TO EOF-RCRC-SW                   00027800
027900                                    GO TO 0010-EXIT.              00027900
028000                                                                  00028000
028100     IF WRK-STATUS-CODE  =  'C'  OR 'G'                           00028100
028200        NEXT SENTENCE                                             00028200
028300     ELSE               GO  TO  0010-EXIT.                        00028300
028400                                                                  00028400
028500     IF WRK-REC-CONT-CON  OR                                      00028500
028600        WRK-REC-GROUP-SPEC-CTL   THEN                             00028600
028700        IF  WRK-KEY-FLD-DEL-REQ     OR                            00028700
028800            WRK-ADD-REQUEST         OR                            00028800
028900            WRK-CANCEL-REQUEST      OR                            00028900
029000            WRK-DELETE-REQUEST                                    00029000
029100            MOVE  CONT-REC-AREA   TO  CONTRACT-CONTROL-RECORD     00029100
029200            PERFORM  0030-PROCESS-ARCHIVE  THRU 0030-EXIT.        00029200
029300                                                                  00029300
029400 0010-EXIT.                                                       00029400
029500      EXIT.                                                       00029500
029600/                                                                 00029600
029700 0030-PROCESS-ARCHIVE.                                            00029700
029800                                                                  00029800
029900      IF WRK-REC-CONT-CON   THEN                                  00029900
030000         IF WRK-CANCEL-REQUEST  OR                                00030000
030100            WRK-DELETE-REQUEST                                    00030100
030200            PERFORM 0060-PUT-DEL-RECORD-CNT THRU 0060-EXIT        00030200
030300         ELSE                                                     00030300
030400*--PHF2 MIL                                                       00030400
030500            IF WRK-PLAN-CODE = CCR-MAPFROM-CON-PLAN-CODE AND      00030500
030600               WRK-GROUP-NUM = CCR-MAPFROM-CON-GROUP-NUM AND      00030600
030700               WRK-SECTION-NUM = CCR-MAPFROM-CON-SECTION-NUM AND  00030700
030800               WRK-PKG-CODE  = CCR-MAPFROM-CON-PKG-CODE AND       00030800
030900               WRK-L-O-B    = CCR-MAPFROM-CON-L-O-B    AND        00030900
031000               WRK-PROV-CTL = CCR-MAPFROM-CON-PROV-CTL AND        00031000
031100               WRK-FAM-REL-LEVEL = CCR-MAPFROM-CON-FAM-REL-LV AND 00031100
031200               WRK-EFFDT-CEN = CCR-MAPFROM-CON-EFFDT-CEN          00031200
031300*--                                                               00031300
031400               GO TO  0030-EXIT                                   00031400
031500             ELSE                                                 00031500
031600               PERFORM 0040-ADD-ARCH-RECORD-CNT  THRU 0040-EXIT   00031600
031700               IF ARKY-ID-CD = 'CNTKY000'                         00031700
031800                  PERFORM 0045-ADD-ARCH-CURR-CNT THRU 0045-EXIT.  00031800
031900                                                                  00031900
032000      IF WRK-REC-GROUP-SPEC-CTL   THEN                            00032000
032100         IF WRK-CANCEL-REQUEST  OR                                00032100
032200            WRK-DELETE-REQUEST                                    00032200
032300            PERFORM 0070-PUT-DEL-RECORD-GRP THRU 0070-EXIT        00032300
032400         ELSE                                                     00032400
032500*--PHF2  MIL                                                      00032500
032600          IF WRK-PLAN-CODE = CCR-MAPFROM-GS-PLAN-CODE AND         00032600
032700             WRK-GROUP-NUM = CCR-MAPFROM-GS-GROUP-NUM AND         00032700
032800             WRK-SECTION-NUM = CCR-MAPFROM-GS-SECTION-NUM AND     00032800
032900             WRK-PKG-CODE  = CCR-MAPFROM-GS-PKG-CODE AND          00032900
033000             WRK-FAM-REL-LEVEL = CCR-MAPFROM-GS-F-R-LVL AND       00033000
033100             WRK-EFFDT-CEN = CCR-MAPFROM-GS-EFFDT-CEN             00033100
033200*--                                                               00033200
033300               GO TO  0030-EXIT                                   00033300
033400            ELSE                                                  00033400
033500               PERFORM 0050-ADD-ARCH-RECORD-GRPS THRU 0050-EXIT   00033500
033600               IF ARKY-ID-CD = 'GPSKY000'                         00033600
033700                  PERFORM 0055-ADD-ARCH-CURR-GRPS THRU 0055-EXIT  00033700
033800            END-IF                                                00033800
033900         END-IF                                                   00033900
034000      END-IF.                                                     00034000
034100                                                                  00034100
034200 0030-EXIT.                                                       00034200
034300      EXIT.                                                       00034300
034400/                                                                 00034400
034500 0040-ADD-ARCH-RECORD-CNT.                                        00034500
034600                                                                  00034600
034700      MOVE SPACES TO ARKY-GC-KEY.                                 00034700
034800      MOVE ZEROS TO ARKY-EFF-DT  ARKY-POINTERS-COUNT.             00034800
034900      MOVE 'C'    TO ARKY-STA-CD.                                 00034900
035000*--PHF2  MIL                                                      00035000
035100      MOVE WRK-PLAN-CODE                 TO ARKY-PLAN-CODE.       00035100
035200      MOVE WRK-GROUP-NUM                 TO ARKY-GROUP-NUM.       00035200
035300      MOVE WRK-SECTION-NUM               TO ARKY-SECTION-NUM.     00035300
035400      MOVE WRK-PKG-CODE                  TO ARKY-PKG-CODE.        00035400
035500      MOVE WRK-L-O-B                     TO ARKY-LOB.             00035500
035600      MOVE WRK-PROV-CTL                  TO ARKY-PRV-CTL.         00035600
035700      MOVE WRK-FAM-REL-LEVEL             TO ARKY-FAM-RL.          00035700
035800      MOVE WRK-EFFDT-CEN                 TO ARKY-EFFDT-CEN.       00035800
035900*--                                                               00035900
036000      IF  WRK-ADD-REQUEST                                         00036000
036100          MOVE 'CNTKY001'                TO ARKY-ID-CD            00036100
036200      ELSE                                                        00036200
036300          MOVE 'CNTKY000'                TO ARKY-ID-CD.           00036300
036400      MOVE ARKY-GC-KEY      TO KEY-FIELD-A.                       00036400
036500                                                                  00036500
036600        MOVE  'R'    TO REQUEST-TYPE-A.                           00036600
036700        MOVE  42     TO RECORD-LENGTH-A.                          00036700
036800     CALL   'TSGVSAM3'  USING PARM-ONE-A PARM-TWO-A.              00036800
036900                                                                  00036900
037000***  RECORD NOT FOUND OR EOF THEN CREATE A NEW ARCHIVE RECORD     00037000
037100***  FROM MAPFROM KEY AND OLD KEY IS WORKFILE KEY                 00037100
037200                                                                  00037200
037300     IF  REQUEST-TYPE-A  = '2'  OR  '3'                           00037300
037400         NEXT SENTENCE                                            00037400
037500     ELSE                                                         00037500
037600        IF REQUEST-TYPE-A  = 'R'                                  00037600
037700           GO TO 0040-ARCH-KEY                                    00037700
037800        ELSE                                                      00037800
037900           MOVE FEEDBACK-CODE-A   TO ABEND-CODE                   00037900
038000           GO TO  9999-ERROR-RTN.                                 00038000
038100                                                                  00038100
038200      MOVE SPACES  TO ARKY-GC-KEY.                                00038200
038300*--PHF2  MIL                                                      00038300
038400      MOVE ZEROS TO ARKY-EFFDT-CEN   ARKY-POINTERS-COUNT.         00038400
038500      MOVE 'C'    TO ARKY-STA-CD.                                 00038500
038600      MOVE WRK-PLAN-CODE                 TO ARKY-PLAN-CODE.       00038600
038700      MOVE WRK-GROUP-NUM                 TO ARKY-GROUP-NUM.       00038700
038800      MOVE WRK-SECTION-NUM               TO ARKY-SECTION-NUM.     00038800
038900      MOVE WRK-PKG-CODE                  TO ARKY-PKG-CODE.        00038900
039000      MOVE WRK-L-O-B                     TO ARKY-LOB.             00039000
039100      MOVE WRK-PROV-CTL                  TO ARKY-PRV-CTL.         00039100
039200      MOVE WRK-FAM-REL-LEVEL             TO ARKY-FAM-RL.          00039200
039300      MOVE WRK-EFFDT-CEN                 TO ARKY-EFFDT-CEN.       00039300
039400*--                                                               00039400
039500      IF  WRK-ADD-REQUEST                                         00039500
039600          MOVE 'CNTKY001'                TO ARKY-ID-CD            00039600
039700      ELSE                                                        00039700
039800          MOVE 'CNTKY000'                TO ARKY-ID-CD.           00039800
039900      MOVE ARKY-GC-KEY      TO KEY-FIELD-A.                       00039900
040000                                                                  00040000
040100 0040-ARCH-KEY.                                                   00040100
040200                                                                  00040200
040300      ADD +1  TO ARKY-POINTERS-COUNT.                             00040300
040400      IF ARKY-POINTERS-COUNT > +2                                 00040400
040500         GO TO  0040-EXIT.                                        00040500
040600      SET ARKY-INDEX  TO ARKY-POINTERS-COUNT.                     00040600
040700*--PHF2  MIL                                                      00040700
040800      MOVE CCR-MAPFROM-CON-PLAN-CODE                              00040800
040900                        TO ARKY-PLAN-CODE-O (ARKY-INDEX).         00040900
041000      MOVE CCR-MAPFROM-CON-GROUP-NUM                              00041000
041100                        TO ARKY-GROUP-NUM-O (ARKY-INDEX).         00041100
041200      MOVE CCR-MAPFROM-CON-SECTION-NUM                            00041200
041300                        TO ARKY-SECTION-NUM-O (ARKY-INDEX).       00041300
041400      MOVE CCR-MAPFROM-CON-PKG-CODE                               00041400
041500                        TO ARKY-PKG-CODE-O (ARKY-INDEX).          00041500
041600      MOVE CCR-MAPFROM-CON-L-O-B                                  00041600
041700                        TO ARKY-LOB-O (ARKY-INDEX).               00041700
041800      MOVE CCR-MAPFROM-CON-PROV-CTL                               00041800
041900                        TO ARKY-PRV-CTL-O (ARKY-INDEX).           00041900
042000      MOVE CCR-MAPFROM-CON-FAM-REL-LV                             00042000
042100                             TO ARKY-FAM-RL-O (ARKY-INDEX).       00042100
042200      MOVE CCR-MAPFROM-CON-EFFDT-CEN                              00042200
042300                        TO ARKY-EFFDT-CEN-O (ARKY-INDEX).         00042300
042400*--                                                               00042400
042500       MOVE WRK-OPERATOR-ID  TO ARKY-OPER-ID (ARKY-INDEX).        00042500
042600       IF WRK-CDE-SP NOT = '2 '                                   00042600
042700          MOVE 'X'  TO ARKY-CDE-IND (ARKY-INDEX)                  00042700
042800       ELSE  MOVE ' '  TO ARKY-CDE-IND (ARKY-INDEX).              00042800
042900                                                                  00042900
043000        MOVE TODAYS-DATE         TO ARKY-CHNG-DT-CEN (ARKY-INDEX).00043000
043100        MOVE SPACES              TO ARKY-ANLS-CD (ARKY-INDEX).    00043100
043200        IF WRK-ATB3-REQUEST                                       00043200
043300           MOVE WRK-SIGNAL-FROM-ONLINE TO                         00043300
043400                                    ARKY-ATB-IND (ARKY-INDEX)     00043400
043500        ELSE                                                      00043500
043600           MOVE WRK-TYPE-MAINT-IND TO                             00043600
043700                                    ARKY-ATB-IND (ARKY-INDEX).    00043700
043800                                                                  00043800
043900        PERFORM 0100-UPDATE-QCF-CON-FILE-ADD THRU 0100-EXIT.      00043900
044000        PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.         00044000
044100        ADD +1  TO ARCH-RECS-BUILD.                               00044100
044200                                                                  00044200
044300 0040-EXIT.                                                       00044300
044400      EXIT.                                                       00044400
044500/                                                                 00044500
044600 0045-ADD-ARCH-CURR-CNT.                                          00044600
044700                                                                  00044700
044800      MOVE SPACES TO ARKY-GC-KEY.                                 00044800
044900*--PHF2  MIL                                                      00044900
045000      MOVE ZEROS TO ARKY-EFFDT-CEN ARKY-POINTERS-COUNT.           00045000
045100      MOVE 'C'                     TO ARKY-STA-CD.                00045100
045200      MOVE CCR-MAPFROM-CON-PLAN-CODE    TO ARKY-PLAN-CODE.        00045200
045300      MOVE CCR-MAPFROM-CON-GROUP-NUM    TO ARKY-GROUP-NUM.        00045300
045400      MOVE CCR-MAPFROM-CON-SECTION-NUM  TO ARKY-SECTION-NUM.      00045400
045500      MOVE CCR-MAPFROM-CON-PKG-CODE     TO ARKY-PKG-CODE.         00045500
045600      MOVE CCR-MAPFROM-CON-L-O-B        TO ARKY-LOB.              00045600
045700      MOVE CCR-MAPFROM-CON-PROV-CTL     TO ARKY-PRV-CTL.          00045700
045800      MOVE CCR-MAPFROM-CON-FAM-REL-LV   TO ARKY-FAM-RL.           00045800
045900      MOVE CCR-MAPFROM-CON-EFFDT-CEN    TO ARKY-EFFDT-CEN.        00045900
046000      MOVE 'CNTKY001'                   TO ARKY-ID-CD             00046000
046100*--                                                               00046100
046200      MOVE ARKY-GC-KEY      TO KEY-FIELD-A.                       00046200
046300                                                                  00046300
046400        MOVE  'R'    TO REQUEST-TYPE-A.                           00046400
046500        MOVE  42     TO RECORD-LENGTH-A.                          00046500
046600     CALL   'TSGVSAM3'  USING PARM-ONE-A PARM-TWO-A.              00046600
046700                                                                  00046700
046800***  RECORD NOT FOUND OR EOF THEN CREATE A NEW ARCHIVE RECORD     00046800
046900***  FROM MAPFROM KEY AND OLD KEY IS WORKFILE KEY                 00046900
047000                                                                  00047000
047100     IF  REQUEST-TYPE-A  = '2'  OR  '3'                           00047100
047200         NEXT SENTENCE                                            00047200
047300     ELSE                                                         00047300
047400        IF REQUEST-TYPE-A  = 'R'                                  00047400
047500           GO TO 0045-ARCH-KEY                                    00047500
047600        ELSE                                                      00047600
047700           MOVE FEEDBACK-CODE-A   TO ABEND-CODE                   00047700
047800           GO TO  9999-ERROR-RTN.                                 00047800
047900                                                                  00047900
048000      MOVE SPACES  TO ARKY-GC-KEY.                                00048000
048100*--PHF2  MIL                                                      00048100
048200      MOVE ZEROS TO ARKY-EFFDT-CEN ARKY-POINTERS-COUNT.           00048200
048300      MOVE 'C'    TO ARKY-STA-CD.                                 00048300
048400      MOVE CCR-MAPFROM-CON-PLAN-CODE    TO ARKY-PLAN-CODE.        00048400
048500      MOVE CCR-MAPFROM-CON-GROUP-NUM    TO ARKY-GROUP-NUM.        00048500
048600      MOVE CCR-MAPFROM-CON-SECTION-NUM  TO ARKY-SECTION-NUM.      00048600
048700      MOVE CCR-MAPFROM-CON-PKG-CODE     TO ARKY-PKG-CODE.         00048700
048800      MOVE CCR-MAPFROM-CON-L-O-B        TO ARKY-LOB.              00048800
048900      MOVE CCR-MAPFROM-CON-PROV-CTL     TO ARKY-PRV-CTL.          00048900
049000      MOVE CCR-MAPFROM-CON-FAM-REL-LV   TO ARKY-FAM-RL.           00049000
049100      MOVE CCR-MAPFROM-CON-EFFDT-CEN    TO ARKY-EFFDT-CEN.        00049100
049200      MOVE 'CNTKY001'                    TO ARKY-ID-CD            00049200
049300*--                                                               00049300
049400      MOVE ARKY-GC-KEY      TO KEY-FIELD-A.                       00049400
049500                                                                  00049500
049600 0045-ARCH-KEY.                                                   00049600
049700                                                                  00049700
049800      ADD +1  TO ARKY-POINTERS-COUNT.                             00049800
049900      IF ARKY-POINTERS-COUNT > +2                                 00049900
050000         GO TO  0045-EXIT.                                        00050000
050100      SET ARKY-INDEX  TO ARKY-POINTERS-COUNT.                     00050100
050200*--PHF2  MIL                                                      00050200
050300      MOVE WRK-PLAN-CODE      TO ARKY-PLAN-CODE-O (ARKY-INDEX).   00050300
050400      MOVE WRK-GROUP-NUM      TO ARKY-GROUP-NUM-O (ARKY-INDEX).   00050400
050500      MOVE WRK-SECTION-NUM    TO ARKY-SECTION-NUM-O (ARKY-INDEX). 00050500
050600      MOVE WRK-PKG-CODE       TO ARKY-PKG-CODE-O (ARKY-INDEX).    00050600
050700      MOVE WRK-L-O-B          TO ARKY-LOB-O (ARKY-INDEX).         00050700
050800      MOVE WRK-PROV-CTL       TO ARKY-PRV-CTL-O (ARKY-INDEX).     00050800
050900      MOVE WRK-FAM-REL-LEVEL  TO ARKY-FAM-RL-O (ARKY-INDEX).      00050900
051000      MOVE WRK-EFFDT-CEN      TO ARKY-EFFDT-CEN-O (ARKY-INDEX).   00051000
051100*--                                                               00051100
051200       MOVE WRK-OPERATOR-ID  TO ARKY-OPER-ID (ARKY-INDEX).        00051200
051300       IF WRK-CDE-SP NOT = '2 '                                   00051300
051400          MOVE 'X'  TO ARKY-CDE-IND (ARKY-INDEX)                  00051400
051500       ELSE  MOVE ' '  TO ARKY-CDE-IND (ARKY-INDEX).              00051500
051600                                                                  00051600
051700        MOVE TODAYS-DATE         TO ARKY-CHNG-DT-CEN (ARKY-INDEX).00051700
051800        MOVE SPACES              TO ARKY-ANLS-CD (ARKY-INDEX).    00051800
051900        IF WRK-ATB3-REQUEST                                       00051900
052000           MOVE WRK-SIGNAL-FROM-ONLINE TO                         00052000
052100                                    ARKY-ATB-IND (ARKY-INDEX)     00052100
052200        ELSE                                                      00052200
052300           MOVE WRK-TYPE-MAINT-IND TO                             00052300
052400                                    ARKY-ATB-IND (ARKY-INDEX).    00052400
052500                                                                  00052500
052600        PERFORM 0200-QCF-CON-FILE-CHANGE THRU 0200-EXIT.          00052600
052700        PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.         00052700
052800        ADD +1  TO ARCH-RECS-BUILD.                               00052800
052900                                                                  00052900
053000 0045-EXIT.                                                       00053000
053100      EXIT.                                                       00053100
053200/                                                                 00053200
053300 0050-ADD-ARCH-RECORD-GRPS.                                       00053300
053400                                                                  00053400
053500      MOVE SPACES TO ARKY-GC-KEY.                                 00053500
053600*--PHF2  MIL                                                      00053600
053700      MOVE ZEROS TO ARKY-EFFDT-CEN  ARKY-POINTERS-COUNT.          00053700
053800      MOVE 'G'    TO ARKY-STA-CD.                                 00053800
053900      MOVE WRK-PLAN-CODE                 TO ARKY-PLAN-CODE.       00053900
054000      MOVE WRK-GROUP-NUM                 TO ARKY-GROUP-NUM.       00054000
054100      MOVE WRK-SECTION-NUM               TO ARKY-SECTION-NUM.     00054100
054200      MOVE WRK-PKG-CODE                  TO ARKY-PKG-CODE.        00054200
054300      MOVE WRK-FAM-REL-LEVEL             TO ARKY-FAM-RL.          00054300
054400      MOVE WRK-EFFDT-CEN                 TO ARKY-EFFDT-CEN.       00054400
054500*--                                                               00054500
054600      IF  WRK-ADD-REQUEST                                         00054600
054700          MOVE 'GPSKY001'                TO ARKY-ID-CD            00054700
054800      ELSE                                                        00054800
054900          MOVE 'GPSKY000'                TO ARKY-ID-CD.           00054900
055000      MOVE ARKY-GC-KEY      TO KEY-FIELD-A.                       00055000
055100                                                                  00055100
055200        MOVE  'R'    TO REQUEST-TYPE-A.                           00055200
055300        MOVE  42     TO RECORD-LENGTH-A.                          00055300
055400     CALL   'TSGVSAM3'  USING PARM-ONE-A PARM-TWO-A.              00055400
055500                                                                  00055500
055600***  RECORD NOT FOUND OR EOF THEN CREATE A NEW ARCHIVE RECORD     00055600
055700***  FROM MAPFROM KEY AND OLD KEY IS WORKFILE KEY                 00055700
055800                                                                  00055800
055900     IF  REQUEST-TYPE-A  = '2'  OR  '3'                           00055900
056000         NEXT SENTENCE                                            00056000
056100     ELSE                                                         00056100
056200        IF REQUEST-TYPE-A  = 'R'                                  00056200
056300           GO TO  0050-ARCH-KEY                                   00056300
056400        ELSE                                                      00056400
056500           MOVE FEEDBACK-CODE-A   TO ABEND-CODE                   00056500
056600           GO TO  9999-ERROR-RTN.                                 00056600
056700                                                                  00056700
056800      MOVE SPACES  TO ARKY-GC-KEY.                                00056800
056900*--PHF2  MIL                                                      00056900
057000      MOVE ZEROS TO ARKY-EFFDT-CEN  ARKY-POINTERS-COUNT.          00057000
057100      MOVE 'G'    TO ARKY-STA-CD.                                 00057100
057200      MOVE WRK-PLAN-CODE                 TO ARKY-PLAN-CODE.       00057200
057300      MOVE WRK-GROUP-NUM                 TO ARKY-GROUP-NUM.       00057300
057400      MOVE WRK-SECTION-NUM               TO ARKY-SECTION-NUM.     00057400
057500      MOVE WRK-PKG-CODE                  TO ARKY-PKG-CODE.        00057500
057600      MOVE WRK-FAM-REL-LEVEL             TO ARKY-FAM-RL.          00057600
057700      MOVE WRK-EFFDT-CEN                 TO ARKY-EFFDT-CEN        00057700
057800*--                                                               00057800
057900      IF  WRK-ADD-REQUEST                                         00057900
058000          MOVE 'GPSKY001'                TO ARKY-ID-CD            00058000
058100      ELSE                                                        00058100
058200          MOVE 'GPSKY000'                TO ARKY-ID-CD.           00058200
058300      MOVE ARKY-GC-KEY      TO KEY-FIELD-A.                       00058300
058400                                                                  00058400
058500 0050-ARCH-KEY.                                                   00058500
058600                                                                  00058600
058700      ADD +1  TO ARKY-POINTERS-COUNT.                             00058700
058800      IF ARKY-POINTERS-COUNT >  +2                                00058800
058900         GO TO  0050-EXIT.                                        00058900
059000      SET ARKY-INDEX  TO ARKY-POINTERS-COUNT.                     00059000
059100      MOVE SPACES     TO ARKY-LOB-O (ARKY-INDEX)                  00059100
059200                         ARKY-PRV-CTL-O (ARKY-INDEX).             00059200
059300*--PHF2  MIL                                                      00059300
059400      MOVE CCR-MAPFROM-GS-PLAN-CODE                               00059400
059500                        TO ARKY-PLAN-CODE-O (ARKY-INDEX).         00059500
059600      MOVE CCR-MAPFROM-GS-GROUP-NUM                               00059600
059700                        TO ARKY-GROUP-NUM-O (ARKY-INDEX).         00059700
059800      MOVE CCR-MAPFROM-GS-SECTION-NUM                             00059800
059900                        TO ARKY-SECTION-NUM-O (ARKY-INDEX).       00059900
060000      MOVE CCR-MAPFROM-GS-PKG-CODE                                00060000
060100                        TO ARKY-PKG-CODE-O (ARKY-INDEX).          00060100
060200      MOVE CCR-MAPFROM-GS-F-R-LVL                                 00060200
060300                        TO ARKY-FAM-RL-O (ARKY-INDEX).            00060300
060400      MOVE CCR-MAPFROM-GS-EFFDT-CEN                               00060400
060500                        TO ARKY-EFFDT-CEN-O (ARKY-INDEX).         00060500
060600*--                                                               00060600
060700       MOVE WRK-OPERATOR-ID  TO ARKY-OPER-ID (ARKY-INDEX).        00060700
060800       IF WRK-CDE-SP NOT = '2 '                                   00060800
060900          MOVE 'X'  TO ARKY-CDE-IND (ARKY-INDEX)                  00060900
061000       ELSE  MOVE ' '  TO ARKY-CDE-IND (ARKY-INDEX).              00061000
061100                                                                  00061100
061200        MOVE TODAYS-DATE         TO ARKY-CHNG-DT-CEN (ARKY-INDEX).00061200
061300        MOVE SPACES              TO ARKY-ANLS-CD (ARKY-INDEX).    00061300
061400        IF WRK-ATB3-REQUEST                                       00061400
061500           MOVE WRK-SIGNAL-FROM-ONLINE TO                         00061500
061600                                    ARKY-ATB-IND (ARKY-INDEX)     00061600
061700        ELSE                                                      00061700
061800           MOVE WRK-TYPE-MAINT-IND TO                             00061800
061900                                    ARKY-ATB-IND (ARKY-INDEX).    00061900
062000                                                                  00062000
062100        PERFORM 0300-UPDATE-QCF-GRP-FILE-ADD THRU 0300-EXIT.      00062100
062200        PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.         00062200
062300        ADD +1  TO ARCH-RECS-BUILD.                               00062300
062400                                                                  00062400
062500 0050-EXIT.                                                       00062500
062600      EXIT.                                                       00062600
062700/                                                                 00062700
062800 0055-ADD-ARCH-CURR-GRPS.                                         00062800
062900                                                                  00062900
063000      MOVE SPACES TO ARKY-GC-KEY.                                 00063000
063100*--PHF2  MIL                                                      00063100
063200      MOVE ZEROS TO ARKY-EFFDT-CEN  ARKY-POINTERS-COUNT.          00063200
063300      MOVE 'G'    TO ARKY-STA-CD.                                 00063300
063400      MOVE CCR-MAPFROM-GS-PLAN-CODE       TO ARKY-PLAN-CODE.      00063400
063500      MOVE CCR-MAPFROM-GS-GROUP-NUM       TO ARKY-GROUP-NUM.      00063500
063600      MOVE CCR-MAPFROM-GS-SECTION-NUM     TO ARKY-SECTION-NUM.    00063600
063700      MOVE CCR-MAPFROM-GS-PKG-CODE        TO ARKY-PKG-CODE.       00063700
063800      MOVE CCR-MAPFROM-GS-F-R-LVL         TO ARKY-FAM-RL.         00063800
063900      MOVE CCR-MAPFROM-GS-EFFDT-CEN       TO ARKY-EFFDT-CEN.      00063900
064000      MOVE 'GPSKY001'                     TO ARKY-ID-CD           00064000
064100*--                                                               00064100
064200      MOVE ARKY-GC-KEY      TO KEY-FIELD-A.                       00064200
064300                                                                  00064300
064400        MOVE  'R'    TO REQUEST-TYPE-A.                           00064400
064500        MOVE  42     TO RECORD-LENGTH-A.                          00064500
064600     CALL   'TSGVSAM3'  USING PARM-ONE-A PARM-TWO-A.              00064600
064700                                                                  00064700
064800***  RECORD NOT FOUND OR EOF THEN CREATE A NEW ARCHIVE RECORD     00064800
064900***  FROM MAPFROM KEY AND OLD KEY IS WORKFILE KEY                 00064900
065000                                                                  00065000
065100     IF  REQUEST-TYPE-A  = '2'  OR  '3'                           00065100
065200         NEXT SENTENCE                                            00065200
065300     ELSE                                                         00065300
065400        IF REQUEST-TYPE-A  = 'R'                                  00065400
065500           GO TO  0055-ARCH-KEY                                   00065500
065600        ELSE                                                      00065600
065700           MOVE FEEDBACK-CODE-A   TO ABEND-CODE                   00065700
065800           GO TO  9999-ERROR-RTN.                                 00065800
065900                                                                  00065900
066000      MOVE SPACES  TO ARKY-GC-KEY.                                00066000
066100*--PHF2  MIL                                                      00066100
066200      MOVE ZEROS TO ARKY-EFFDT-CEN   ARKY-POINTERS-COUNT.         00066200
066300      MOVE 'G'    TO ARKY-STA-CD.                                 00066300
066400      MOVE CCR-MAPFROM-GS-PLAN-CODE       TO ARKY-PLAN-CODE.      00066400
066500      MOVE CCR-MAPFROM-GS-GROUP-NUM       TO ARKY-GROUP-NUM.      00066500
066600      MOVE CCR-MAPFROM-GS-SECTION-NUM     TO ARKY-SECTION-NUM.    00066600
066700      MOVE CCR-MAPFROM-GS-PKG-CODE        TO ARKY-PKG-CODE.       00066700
066800      MOVE CCR-MAPFROM-GS-F-R-LVL         TO ARKY-FAM-RL.         00066800
066900      MOVE CCR-MAPFROM-GS-EFFDT-CEN       TO ARKY-EFFDT-CEN.      00066900
067000      MOVE 'GPSKY001'                    TO ARKY-ID-CD            00067000
067100*--                                                               00067100
067200      MOVE ARKY-GC-KEY      TO KEY-FIELD-A.                       00067200
067300                                                                  00067300
067400 0055-ARCH-KEY.                                                   00067400
067500                                                                  00067500
067600      ADD +1  TO ARKY-POINTERS-COUNT.                             00067600
067700      IF ARKY-POINTERS-COUNT >  +2                                00067700
067800         GO TO  0055-EXIT.                                        00067800
067900      SET ARKY-INDEX  TO ARKY-POINTERS-COUNT.                     00067900
068000      MOVE SPACES     TO ARKY-LOB-O (ARKY-INDEX)                  00068000
068100                         ARKY-PRV-CTL-O (ARKY-INDEX).             00068100
068200*--PHF2  MIL                                                      00068200
068300      MOVE WRK-PLAN-CODE      TO ARKY-PLAN-CODE-O (ARKY-INDEX).   00068300
068400      MOVE WRK-GROUP-NUM      TO ARKY-GROUP-NUM-O (ARKY-INDEX).   00068400
068500      MOVE WRK-SECTION-NUM    TO ARKY-SECTION-NUM-O (ARKY-INDEX). 00068500
068600      MOVE WRK-PKG-CODE       TO ARKY-PKG-CODE-O (ARKY-INDEX).    00068600
068700      MOVE WRK-L-O-B          TO ARKY-LOB-O (ARKY-INDEX).         00068700
068800      MOVE WRK-PROV-CTL       TO ARKY-PRV-CTL-O (ARKY-INDEX).     00068800
068900      MOVE WRK-FAM-REL-LEVEL  TO ARKY-FAM-RL-O (ARKY-INDEX).      00068900
069000      MOVE WRK-EFFDT-CEN      TO ARKY-EFFDT-CEN-O (ARKY-INDEX).   00069000
069100*--                                                               00069100
069200       MOVE WRK-OPERATOR-ID  TO ARKY-OPER-ID (ARKY-INDEX).        00069200
069300       IF WRK-CDE-SP NOT = '2 '                                   00069300
069400          MOVE 'X'  TO ARKY-CDE-IND (ARKY-INDEX)                  00069400
069500       ELSE  MOVE ' '  TO ARKY-CDE-IND (ARKY-INDEX).              00069500
069600                                                                  00069600
069700        MOVE TODAYS-DATE         TO ARKY-CHNG-DT-CEN (ARKY-INDEX).00069700
069800        MOVE SPACES              TO ARKY-ANLS-CD (ARKY-INDEX).    00069800
069900        IF WRK-ATB3-REQUEST                                       00069900
070000           MOVE WRK-SIGNAL-FROM-ONLINE TO                         00070000
070100                                    ARKY-ATB-IND (ARKY-INDEX)     00070100
070200        ELSE                                                      00070200
070300           MOVE WRK-TYPE-MAINT-IND TO                             00070300
070400                                    ARKY-ATB-IND (ARKY-INDEX).    00070400
070500                                                                  00070500
070600        PERFORM 0400-QCF-GRP-FILE-CHANGE THRU 0400-EXIT.          00070600
070700        PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.         00070700
070800        ADD +1  TO ARCH-RECS-BUILD.                               00070800
070900                                                                  00070900
071000 0055-EXIT.                                                       00071000
071100      EXIT.                                                       00071100
071200/                                                                 00071200
071300 0060-PUT-DEL-RECORD-CNT.                                         00071300
071400                                                                  00071400
071500      MOVE SPACES TO ARKY-GC-KEY.                                 00071500
071600*--PHF2  MIL                                                      00071600
071700      MOVE ZEROS TO ARKY-EFFDT-CEN  ARKY-POINTERS-COUNT.          00071700
071800      MOVE 'C'    TO ARKY-STA-CD.                                 00071800
071900      MOVE WRK-PLAN-CODE                 TO ARKY-PLAN-CODE.       00071900
072000      MOVE WRK-GROUP-NUM                 TO ARKY-GROUP-NUM.       00072000
072100      MOVE WRK-SECTION-NUM               TO ARKY-SECTION-NUM.     00072100
072200      MOVE WRK-PKG-CODE                  TO ARKY-PKG-CODE.        00072200
072300      MOVE WRK-L-O-B                     TO ARKY-LOB.             00072300
072400      MOVE WRK-PROV-CTL                  TO ARKY-PRV-CTL.         00072400
072500      MOVE WRK-FAM-REL-LEVEL             TO ARKY-FAM-RL.          00072500
072600      MOVE WRK-EFFDT-CEN                 TO ARKY-EFFDT-CEN.       00072600
072700*--                                                               00072700
072800      MOVE 'CNTDL000'                    TO ARKY-ID-CD            00072800
072900      MOVE ARKY-GC-KEY      TO KEY-FIELD-A.                       00072900
073000                                                                  00073000
073100        MOVE  'R'    TO REQUEST-TYPE-A.                           00073100
073200        MOVE  42     TO RECORD-LENGTH-A.                          00073200
073300     CALL   'TSGVSAM3'  USING PARM-ONE-A PARM-TWO-A.              00073300
073400                                                                  00073400
073500***  RECORD NOT FOUND OR EOF THEN CREATE A NEW ARCHIVE RECORD     00073500
073600***  FROM THE DELETED RECORD ELSE DELETE THE RECORD FOUND AND     00073600
073700***  CREATE ANOTHER ARCHIVED RECORD.  NE 09/01/89                 00073700
073800                                                                  00073800
073900     IF  REQUEST-TYPE-A  = '2'  OR  '3'                           00073900
074000         NEXT SENTENCE                                            00074000
074100     ELSE                                                         00074100
074200        IF REQUEST-TYPE-A  = 'R'                                  00074200
074300           PERFORM  0095-DELETE-RECORD THRU 0095-EXIT             00074300
074400        ELSE                                                      00074400
074500           MOVE FEEDBACK-CODE-A   TO ABEND-CODE                   00074500
074600           GO TO  9999-ERROR-RTN.                                 00074600
074700                                                                  00074700
074800      ADD +1  TO ARKY-POINTERS-COUNT.                             00074800
074900      IF ARKY-POINTERS-COUNT > +2                                 00074900
075000         GO TO  0060-EXIT.                                        00075000
075100      SET ARKY-INDEX  TO ARKY-POINTERS-COUNT.                     00075100
075200                                                                  00075200
075300                                                                  00075300
075400*--PHF2  MIL                                                      00075400
075500      MOVE WRK-PLAN-CODE      TO ARKY-PLAN-CODE-O (ARKY-INDEX).   00075500
075600      MOVE WRK-GROUP-NUM      TO ARKY-GROUP-NUM-O (ARKY-INDEX).   00075600
075700      MOVE WRK-SECTION-NUM    TO ARKY-SECTION-NUM-O (ARKY-INDEX). 00075700
075800      MOVE WRK-PKG-CODE       TO ARKY-PKG-CODE-O (ARKY-INDEX).    00075800
075900      MOVE WRK-L-O-B          TO ARKY-LOB-O (ARKY-INDEX).         00075900
076000      MOVE WRK-PROV-CTL       TO ARKY-PRV-CTL-O (ARKY-INDEX).     00076000
076100      MOVE WRK-FAM-REL-LEVEL  TO ARKY-FAM-RL-O (ARKY-INDEX).      00076100
076200      MOVE WRK-EFFDT-CEN      TO ARKY-EFFDT-CEN-O (ARKY-INDEX).   00076200
076300*--                                                               00076300
076400       MOVE WRK-OPERATOR-ID  TO ARKY-OPER-ID (ARKY-INDEX).        00076400
076500       IF WRK-CDE-SP NOT = '2 '                                   00076500
076600          MOVE 'X'  TO ARKY-CDE-IND (ARKY-INDEX)                  00076600
076700       ELSE  MOVE ' '  TO ARKY-CDE-IND (ARKY-INDEX).              00076700
076800                                                                  00076800
076900        MOVE TODAYS-DATE         TO ARKY-CHNG-DT-CEN (ARKY-INDEX).00076900
077000        MOVE SPACES              TO ARKY-ANLS-CD (ARKY-INDEX).    00077000
077100        IF WRK-ATB3-REQUEST                                       00077100
077200           MOVE WRK-SIGNAL-FROM-ONLINE TO                         00077200
077300                                    ARKY-ATB-IND (ARKY-INDEX)     00077300
077400        ELSE                                                      00077400
077500           MOVE WRK-TYPE-MAINT-IND TO                             00077500
077600                                    ARKY-ATB-IND (ARKY-INDEX).    00077600
077700                                                                  00077700
077800        PERFORM 0100-UPDATE-QCF-CON-FILE-ADD THRU 0100-EXIT.      00077800
077900        PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.         00077900
078000        ADD +1  TO ARCH-RECS-BUILD.                               00078000
078100                                                                  00078100
078200 0060-EXIT.                                                       00078200
078300      EXIT.                                                       00078300
078400/                                                                 00078400
078500 0070-PUT-DEL-RECORD-GRP.                                         00078500
078600                                                                  00078600
078700      MOVE SPACES TO ARKY-GC-KEY.                                 00078700
078800*--PHF2  MIL                                                      00078800
078900      MOVE ZEROS TO ARKY-EFFDT-CEN ARKY-POINTERS-COUNT.           00078900
079000      MOVE 'G'    TO ARKY-STA-CD.                                 00079000
079100      MOVE WRK-PLAN-CODE                 TO ARKY-PLAN-CODE.       00079100
079200      MOVE WRK-GROUP-NUM                 TO ARKY-GROUP-NUM.       00079200
079300      MOVE WRK-SECTION-NUM               TO ARKY-SECTION-NUM.     00079300
079400      MOVE WRK-PKG-CODE                  TO ARKY-PKG-CODE.        00079400
079500*     MOVE WRK-L-O-B                     TO ARKY-LOB.             00079500
079600*     MOVE WRK-PROV-CTL                  TO ARKY-PRV-CTL.         00079600
079700      MOVE WRK-FAM-REL-LEVEL             TO ARKY-FAM-RL.          00079700
079800      MOVE WRK-EFFDT-CEN                 TO ARKY-EFFDT-CEN.       00079800
079900*--                                                               00079900
080000      MOVE 'GPSDL000'                TO ARKY-ID-CD.               00080000
080100      MOVE ARKY-GC-KEY      TO KEY-FIELD-A.                       00080100
080200                                                                  00080200
080300        MOVE  'R'    TO REQUEST-TYPE-A.                           00080300
080400        MOVE  42     TO RECORD-LENGTH-A.                          00080400
080500     CALL   'TSGVSAM3'  USING PARM-ONE-A PARM-TWO-A.              00080500
080600                                                                  00080600
080700***  RECORD NOT FOUND OR EOF THEN CREATE A NEW ARCHIVE RECORD     00080700
080800***  FROM THE DELETED RECORD ELSE DELETE THE RECORD FOUND AND     00080800
080900***  CREATE ANOTHER ARCHIVED RECORD.  NE 09/01/89                 00080900
081000                                                                  00081000
081100     IF  REQUEST-TYPE-A  = '2'  OR  '3'                           00081100
081200         NEXT SENTENCE                                            00081200
081300     ELSE                                                         00081300
081400        IF REQUEST-TYPE-A  = 'R'                                  00081400
081500           PERFORM 0095-DELETE-RECORD THRU 0095-EXIT              00081500
081600        ELSE                                                      00081600
081700           MOVE FEEDBACK-CODE-A   TO ABEND-CODE                   00081700
081800           GO TO  9999-ERROR-RTN.                                 00081800
081900                                                                  00081900
082000      ADD +1  TO ARKY-POINTERS-COUNT.                             00082000
082100      IF ARKY-POINTERS-COUNT >  +2                                00082100
082200         GO TO  0070-EXIT.                                        00082200
082300      SET ARKY-INDEX  TO ARKY-POINTERS-COUNT.                     00082300
082400                                                                  00082400
082500*--PHF2  MIL                                                      00082500
082600      MOVE WRK-PLAN-CODE      TO ARKY-PLAN-CODE-O (ARKY-INDEX).   00082600
082700      MOVE WRK-GROUP-NUM      TO ARKY-GROUP-NUM-O (ARKY-INDEX).   00082700
082800      MOVE WRK-SECTION-NUM    TO ARKY-SECTION-NUM-O (ARKY-INDEX). 00082800
082900      MOVE WRK-PKG-CODE       TO ARKY-PKG-CODE-O (ARKY-INDEX).    00082900
083000*     MOVE WRK-L-O-B          TO ARKY-LOB-O (ARKY-INDEX).         00083000
083100*     MOVE WRK-PROV-CTL       TO ARKY-PRV-CTL-O (ARKY-INDEX).     00083100
083200      MOVE WRK-FAM-REL-LEVEL  TO ARKY-FAM-RL-O (ARKY-INDEX).      00083200
083300      MOVE WRK-EFFDT-CEN      TO ARKY-EFFDT-CEN-O (ARKY-INDEX).   00083300
083400*--                                                               00083400
083500       MOVE WRK-OPERATOR-ID  TO ARKY-OPER-ID (ARKY-INDEX).        00083500
083600       IF WRK-CDE-SP NOT = '2 '                                   00083600
083700          MOVE 'X'  TO ARKY-CDE-IND (ARKY-INDEX)                  00083700
083800       ELSE  MOVE ' '  TO ARKY-CDE-IND (ARKY-INDEX).              00083800
083900                                                                  00083900
084000        MOVE TODAYS-DATE         TO ARKY-CHNG-DT-CEN (ARKY-INDEX).00084000
084100        MOVE SPACES              TO ARKY-ANLS-CD (ARKY-INDEX).    00084100
084200        IF WRK-ATB3-REQUEST                                       00084200
084300           MOVE WRK-SIGNAL-FROM-ONLINE TO                         00084300
084400                                    ARKY-ATB-IND (ARKY-INDEX)     00084400
084500        ELSE                                                      00084500
084600           MOVE WRK-TYPE-MAINT-IND TO                             00084600
084700                                    ARKY-ATB-IND (ARKY-INDEX).    00084700
084800                                                                  00084800
084900        PERFORM 0300-UPDATE-QCF-GRP-FILE-ADD THRU 0300-EXIT.      00084900
085000        PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.         00085000
085100        ADD +1  TO ARCH-RECS-BUILD.                               00085100
085200                                                                  00085200
085300 0070-EXIT.                                                       00085300
085400      EXIT.                                                       00085400
085500/                                                                 00085500
085600 0085-UPDATE-ARCHIVED-FILE.                                       00085600
085700                                                                  00085700
085800***  REWRITE/WRITE THE ARCHIVED RECORD WITH THE NEW ENTRY         00085800
085900        MOVE ARKY-POINTERS-COUNT TO PNTRS-COUNT-A.                00085900
086000        MOVE ARKYIVED-GCPS-RECORD TO REC-AREA-A.                  00086000
086100        COMPUTE RECORD-LENGTH-A =  GC-ARCHIVE-FIXED-LEN + 4       00086100
086200           + (GC-ARCH-KEY-VARY-LEN * ARKY-POINTERS-COUNT).        00086200
086300                                                                  00086300
086400     MOVE 'W'    TO REQUEST-TYPE-A.                               00086400
086500     CALL   'TSGVSAM3'  USING PARM-ONE-A PARM-TWO-A.              00086500
086600     IF  REQUEST-TYPE-A NOT EQUAL 'W'                             00086600
086700         MOVE FEEDBACK-CODE-A    TO ABEND-CODE                    00086700
086800         GO TO 9999-ERROR-RTN.                                    00086800
086900                                                                  00086900
087000 0085-EXIT.                                                       00087000
087100      EXIT.                                                       00087100
087200/                                                                 00087200
087300 0095-DELETE-RECORD.                                              00087300
087400                                                                  00087400
087500     MOVE 'D'    TO REQUEST-TYPE-A.                               00087500
087600     CALL   'TSGVSAM3'  USING PARM-ONE-A PARM-TWO-A.              00087600
087700     IF  REQUEST-TYPE-A NOT EQUAL 'D'                             00087700
087800         DISPLAY 'PROGRAM GC03330 ABENDING,'                      00087800
087900         DISPLAY 'BAD DELETE ATTEMPT IN 0095-DELETE-RECORD'       00087900
088000         MOVE FEEDBACK-CODE-A    TO ABEND-CODE                    00088000
088100         GO TO 9999-ERROR-RTN.                                    00088100
088200                                                                  00088200
088300 0095-EXIT.                                                       00088300
088400      EXIT.                                                       00088400
088500 0100-UPDATE-QCF-CON-FILE-ADD.                                    00088500
088600                                                                  00088600
088700     PERFORM 0500-INITIALIZE-QCF-RECORD THRU 0500-EXIT.           00088700
088800                                                                  00088800
088900*--PHF2  MIL                                                      00088900
089000     MOVE WRK-PLAN-CODE                 TO QCF-PLAN-CODE.         00089000
089100     MOVE WRK-GROUP-NUM                 TO QCF-GROUP-NO.          00089100
089200     MOVE WRK-SECTION-NUM               TO QCF-SECTION-NO.        00089200
089300     MOVE WRK-PKG-CODE                  TO QCF-PKG-CODE.          00089300
089400     MOVE WRK-L-O-B                     TO QCF-L-O-B.             00089400
089500     MOVE WRK-PROV-CTL                  TO QCF-PROV-CTRL.         00089500
089600     MOVE WRK-FAM-REL-LEVEL             TO QCF-FAM-REL-LEVEL.     00089600
089700     MOVE WRK-EFFDT-CEN                 TO QCF-EFF-DATE.          00089700
089800*--                                                               00089800
089900                                                                  00089900
090000*    IF WRK-EFFDT-CEN < 9999999                                   00090000
090100*       MOVE WRK-EFFDT-CEN    TO QCF-EFF-CEN.                     00090100
090200                                                                  00090200
090300*--IF A DELETION IS OCCURRING NO VALUES FOR IMAGE FIELD           00090300
090400     IF ARKY-ID-CD = 'CNTKY000' OR 'CNTKY001'                     00090400
090500*--PHF2  MIL                                                      00090500
090600        MOVE CCR-MAPFROM-CON-GROUP-NUM                            00090600
090700                             TO QCF-BIM-CON-GRP-NUMBER            00090700
090800        MOVE CCR-MAPFROM-CON-SECTION-NUM                          00090800
090900                             TO QCF-BIM-CON-SECTN-NUMBER          00090900
091000        MOVE CCR-MAPFROM-CON-PKG-CODE                             00091000
091100                             TO QCF-BIM-CON-PKG-CODE              00091100
091200        MOVE CCR-MAPFROM-CON-L-O-B                                00091200
091300                             TO QCF-BIM-CON-L-O-B                 00091300
091400        MOVE CCR-MAPFROM-CON-PROV-CTL                             00091400
091500                             TO QCF-BIM-CON-PROV-CTL              00091500
091600        MOVE CCR-MAPFROM-CON-FAM-REL-LV                           00091600
091700                             TO QCF-BIM-CON-FAM-REL-LV            00091700
091800        MOVE CCR-MAPFROM-CON-EFF-DT                               00091800
091900                             TO QCF-BIM-CON-EFF-DT                00091900
092000*--                                                               00092000
092100     ELSE                                                         00092100
092200        MOVE SPACES                     TO QCF-CON-IMAGE-FIELD    00092200
092300     END-IF.                                                      00092300
092400                                                                  00092400
092500     WRITE QCF-RECORD.                                            00092500
092600     MOVE SPACES        TO QCF-IMAGE-FIELD.                       00092600
092700                                                                  00092700
092800     ADD +1 TO WS-QCF-COUNT.                                      00092800
092900                                                                  00092900
093000 0100-EXIT.  EXIT.                                                00093000
093100/                                                                 00093100
093200 0200-QCF-CON-FILE-CHANGE.                                        00093200
093300                                                                  00093300
093400     PERFORM 0500-INITIALIZE-QCF-RECORD THRU 0500-EXIT.           00093400
093500                                                                  00093500
093600*--PHF2  MIL                                                      00093600
093700     MOVE CCR-MAPFROM-CON-PLAN-CODE  TO QCF-PLAN-CODE.            00093700
093800     MOVE CCR-MAPFROM-CON-GROUP-NUM  TO QCF-GROUP-NO.             00093800
093900     MOVE CCR-MAPFROM-CON-SECTION-NUM TO QCF-SECTION-NO.          00093900
094000     MOVE CCR-MAPFROM-CON-PKG-CODE   TO QCF-PKG-CODE.             00094000
094100     MOVE CCR-MAPFROM-CON-L-O-B      TO QCF-L-O-B.                00094100
094200     MOVE CCR-MAPFROM-CON-PROV-CTL   TO QCF-PROV-CTRL.            00094200
094300     MOVE CCR-MAPFROM-CON-FAM-REL-LV TO QCF-FAM-REL-LEVEL.        00094300
094400     MOVE CCR-MAPFROM-CON-EFFDT-CEN  TO QCF-EFF-DATE.             00094400
094500*--                                                               00094500
094600*    IF CCR-MAPFROM-CON-EFF-DT < 99999                            00094600
094700*       COMPUTE                                                   00094700
094800*         QCF-EFF-DATE = +1900000 + CCR-MAPFROM-CON-EFF-DT.       00094800
094900                                                                  00094900
095000                                                                  00095000
095100*--PHF2  MIL                                                      00095100
095200     MOVE WRK-GROUP-NUM              TO QCF-BIM-CON-GRP-NUMBER.   00095200
095300     MOVE WRK-SECT-NO                TO QCF-BIM-CON-SECTN-NUMBER. 00095300
095400     MOVE WRK-PKG-CODE               TO QCF-BIM-CON-PKG-CODE.     00095400
095500     MOVE WRK-L-O-B                  TO QCF-BIM-CON-L-O-B.        00095500
095600     MOVE WRK-PROV-CTL               TO QCF-BIM-CON-PROV-CTL.     00095600
095700     MOVE WRK-FAM-REL-LEVEL          TO QCF-BIM-CON-FAM-REL-LV.   00095700
095800     MOVE WRK-EFF-DATE               TO QCF-BIM-CON-EFF-DT.       00095800
095900*--                                                               00095900
096000     WRITE QCF-RECORD.                                            00096000
096100     MOVE SPACES        TO QCF-IMAGE-FIELD.                       00096100
096200     ADD +1 TO WS-QCF-COUNT.                                      00096200
096300                                                                  00096300
096400 0200-EXIT.  EXIT.                                                00096400
096500/                                                                 00096500
096600 0300-UPDATE-QCF-GRP-FILE-ADD.                                    00096600
096700                                                                  00096700
096800     PERFORM 0500-INITIALIZE-QCF-RECORD THRU 0500-EXIT.           00096800
096900*--PHF2  MIL                                                      00096900
097000     MOVE WRK-PLAN-CODE                 TO QCF-PLAN-CODE.         00097000
097100     MOVE WRK-GROUP-NUM                 TO QCF-GROUP-NO.          00097100
097200     MOVE WRK-SECTION-NUM               TO QCF-SECTION-NO.        00097200
097300     MOVE WRK-PKG-CODE                  TO QCF-PKG-CODE.          00097300
097400     MOVE WRK-L-O-B                     TO QCF-L-O-B.             00097400
097500     MOVE WRK-PROV-CTL                  TO QCF-PROV-CTRL.         00097500
097600     MOVE WRK-FAM-REL-LEVEL             TO QCF-FAM-REL-LEVEL.     00097600
097700     MOVE WRK-EFFDT-CEN                 TO QCF-EFF-DATE.          00097700
097800*--                                                               00097800
097900*    IF WRK-EFF-DATE < 99999                                      00097900
098000*       COMPUTE                                                   00098000
098100*         QCF-EFF-DATE = +1900000 + WRK-EFF-DATE.                 00098100
098200                                                                  00098200
098300*--IF A DELETION IS OCCURRING NO VALUES FOR IMAGE FIELD           00098300
098400     IF ARKY-ID-CD = 'GPSKY000' OR 'GPSKY001'                     00098400
098500*--PHF2  MIL                                                      00098500
098600        MOVE CCR-MAPFROM-GS-GROUP-NUM                             00098600
098700                               TO QCF-BIM-GSP-GRP-NUMBER          00098700
098800        MOVE CCR-MAPFROM-GS-SECTION-NUM                           00098800
098900                               TO QCF-BIM-GSP-SECTN-NUMBER        00098900
099000        MOVE CCR-MAPFROM-GS-PKG-CODE                              00099000
099100                               TO QCF-BIM-GRP-SPEC-PKG-CODE       00099100
099200        MOVE CCR-MAPFROM-GS-F-R-LVL                               00099200
099300                               TO QCF-BIM-GRP-SPEC-F-R-LVL        00099300
099400        MOVE CCR-MAPFROM-GS-EFF-DT                                00099400
099500                               TO QCF-BIM-GRP-SPEC-EFF-DT         00099500
099600*--                                                               00099600
099700     ELSE                                                         00099700
099800        MOVE SPACES                     TO QCF-GRP-IMAGE-FIELD    00099800
099900     END-IF.                                                      00099900
100000                                                                  00100000
100100     WRITE QCF-RECORD.                                            00100100
100200     MOVE SPACES        TO QCF-IMAGE-FIELD.                       00100200
100300     ADD +1 TO WS-QCF-COUNT.                                      00100300
100400                                                                  00100400
100500 0300-EXIT.  EXIT.                                                00100500
100600/                                                                 00100600
100700 0400-QCF-GRP-FILE-CHANGE.                                        00100700
100800                                                                  00100800
100900     PERFORM 0500-INITIALIZE-QCF-RECORD THRU 0500-EXIT.           00100900
101000                                                                  00101000
101100*--PHF2  MIL                                                      00101100
101200     MOVE CCR-MAPFROM-GS-PLAN-CODE     TO QCF-PLAN-CODE.          00101200
101300     MOVE CCR-MAPFROM-GS-GROUP-NUM     TO QCF-GROUP-NO.           00101300
101400     MOVE CCR-MAPFROM-GS-SECTION-NUM   TO QCF-SECTION-NO.         00101400
101500     MOVE CCR-MAPFROM-GS-PKG-CODE      TO QCF-PKG-CODE.           00101500
101600     MOVE CCR-MAPFROM-GS-F-R-LVL       TO QCF-FAM-REL-LEVEL.      00101600
101700     MOVE CCR-MAPFROM-GS-EFFDT-CEN     TO QCF-EFF-DATE.           00101700
101800*--                                                               00101800
101900*    IF CCR-MAPFROM-GRP-SPEC-EFF-DT < 99999                       00101900
102000*       COMPUTE                                                   00102000
102100*         QCF-EFF-DATE = +1900000 + CCR-MAPFROM-GRP-SPEC-EFF-DT.  00102100
102200                                                                  00102200
102300*--PHF2  MIL                                                      00102300
102400     MOVE WRK-GROUP-NUM               TO QCF-BIM-CON-GRP-NUMBER.  00102400
102500     MOVE WRK-SECTION-NUM             TO QCF-BIM-CON-SECTN-NUMBER.00102500
102600     MOVE WRK-PKG-CODE                TO QCF-BIM-CON-PKG-CODE.    00102600
102700     MOVE WRK-L-O-B                   TO QCF-BIM-CON-L-O-B        00102700
102800     MOVE WRK-PROV-CTL                TO QCF-BIM-CON-PROV-CTL     00102800
102900     MOVE WRK-FAM-REL-LEVEL           TO QCF-BIM-CON-FAM-REL-LV   00102900
103000     MOVE WRK-EFFDT-CEN               TO QCF-BIM-CON-EFF-DT.      00103000
103100*--                                                               00103100
103200     WRITE QCF-RECORD.                                            00103200
103300     MOVE SPACES        TO QCF-IMAGE-FIELD.                       00103300
103400     ADD +1 TO WS-QCF-COUNT.                                      00103400
103500                                                                  00103500
103600                                                                  00103600
103700                                                                  00103700
103800 0400-EXIT.  EXIT.                                                00103800
103900/                                                                 00103900
104000 0500-INITIALIZE-QCF-RECORD.                                      00104000
104100                                                                  00104100
104200     INITIALIZE QCF-RECORD.                                       00104200
104300*    MOVE ZEROS            TO QCF-PLAN-CODE                       00104300
104400*                             QCF-GROUP-NO                        00104400
104500*                             QCF-SECTION-NO                      00104500
104600*                             QCF-BIM-CON-GRP-NO-1-3              00104600
104700*                             QCF-BIM-CON-SECTN-NO-1              00104700
104800*                             QCF-BIM-GSP-GRP-NO-1-3              00104800
104900*                             QCF-BIM-GSP-SECTN-NO-1.             00104900
105000     MOVE SPACES           TO QCF-FILLER.                         00105000
105100                                                                  00105100
105200     MOVE WRK-STATUS-CODE  TO QCF-CONTRACT-GROUP-SP-IND.          00105200
105300     MOVE WRK-OPERATOR-ID  TO QCF-OPERATOR-ID.                    00105300
105400     MOVE PARM-LOCATION    TO QCF-PLAN-CODE.                      00105400
105500                                                                  00105500
105600     IF TODAYS-DATE < 9999999                                     00105600
105700        MOVE TODAYS-DATE TO QCF-FUNC-DATE.                        00105700
105800                                                                  00105800
105900     MOVE ARKY-ID-CD       TO QCF-FUNC-FIELD.                     00105900
106000                                                                  00106000
106100 0500-EXIT.  EXIT.                                                00106100
106200 9999-ERROR-RTN.                                                  00106200
106300      CALL 'TSGEND' USING ABEND-CODE.                             00106300
106400 9999-EXIT.                                                       00106400
106500      EXIT.                                                       00106500
