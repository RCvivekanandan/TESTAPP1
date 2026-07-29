000100 IDENTIFICATION DIVISION.                                         00000100
000200                                                                  00000200
000300 PROGRAM-ID. GC03310.                                             00000300
000400 AUTHOR. NABIL ELBAZ.                                             00000400
000500                                                                  00000500
000600 INSTALLATION. HCSC-HCMS.                                         00000600
000700 DATE-WRITTEN.                                                    00000700
000800 DATE-COMPILED.                                                   00000800
000900******************************************************************00000900
001000*                                                                *00001000
001100*     THIS PROGRAM WILL ARCHIVE CHANGES TO CONTRACT DATA         *00001100
001200*     ELEMENTS TO THE ARCHIVING FILE. THE LOGIC WILL COMPARE     *00001200
001300*     THE BEFORE IMAGE FILE TO THE AFTER IMAGE FILE AND          *00001300
001400*     ARCHIVE THE CHANGED ELEMENT FROM THE BEFOR IMAGE FILE      *00001400
001500*     TO THE ARCHIVING FILE.                                     *00001500
001600*                                                                *00001600
001700*     RCL-FILE IS RLSE-CONTRACT FILE (AFTER-IMAGE)               *00001700
001800*     BIM-FILE IS BEFORE-IMAGE FILE                              *00001800
001900*     AUD-FILE IS AUDIT RECORD C9              01/25/89          *00001900
002000*     TSGVSAM3 IS GCPS ARCHIVED FILE (OUTPUT)                    *00002000
002100*                                                                *00002100
002200******************************************************************00002200
002300*                                                                *00002300
002400*    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         *00002400
002500*    *-*         U P D A T E   H I S T O R Y         *-*         *00002500
002600*    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         *00002600
002700*                                                                *00002700
002800* CHG #    DATE    BY              DESCRIPTION                   *00002800
002900* _____  ________  ___  ___________________________________      *00002900
003000*                                                                *00003000
003100*  D183  04/27/88  NGE  INITIOAL PROGRAM BUILD                   *00003100
003200*  D170  01/25/89  NGE  ADD A NEW LOGIC TO ARCHIVE THE AUDIT     *00003200
003300*                       INFORMATION IN C9 W/F RECORD             *00003300
003400*  D???  07/26/89  NGE  MOVE ATB INDICATOR VALUE FOR ATB-3       *00003400
003500*  11161 10/12/90  PFH  EXPANDED MCARE-TYPE FIELDS TO 2 BYTES.   *00003500
003600*                       REPLACED HARD CODED BEFORE & AFTER IMAGES*00003600
003700*                       WITH COPY BOOKS.                         *00003700
003800*  11698 04/08/91  PFH  1. CHANGED MOVE CORR TO 05 LEVEL.        *00003800
003900*                       2. REPLACED HARDCODED UNTIL VALUE WITH   *00003900
004000*                          05 LEVEL COUNTER.                     *00004000
004100*                       3. INCREASED TABLED OCCURS FROM PIC X(7) *00004100
004200*                          TO PIC X(10).                         *00004200
004300*                                                                *00004300
004400*  D12009 9/10/91  GDM  INCREASE K-F-R TO 2 POSITIONS            *00004400
004500*                                                                *00004500
004600*  09/19/91    TPM    EXPANSION OF THE FAMILY-RELATION FIELD.    *00004600
004700*  D12009             REDUCE THE DATA-AREA-A BY ONE BYTE TO      *00004700
004800*                     TO ACCOMODATE  FOR THE ABOVE CHANGE.       *00004800
004900*                     REMOVE HARD CODED RECORD LENGTHS FOR       *00004900
005000*                     ARCHIVE AND INSERTED COPYBOOK MEMBER       *00005000
005100*                     GCCDRLEN  TO BE USED TO INCLUDE NEW RECORD *00005100
005200*                     LENGTHS TO HANDLE THE EXPANSION IN THE     *00005200
005300*                     FAMILY RELATION FIELD.                     *00005300
005400*                                                                *00005400
005500*                     CHANGED THE RECORD LENGTH FROM 30 TO 31    *00005500
005600*                     WHEN CALLING THE TSGVSAM ROUTINE.          *00005600
005700*                                                                *00005700
005800* D12009  10-07-91  FRY   MODIFIED:                         .    *00005800
005900*                         FROM:    REC-AREA-A    PIC X(8099).    *00005900
006000*                           TO:    REC-AREA-A    PIC X(8076).    *00006000
006100*                         FROM:    DATA-AREA-A    PIC X(8069).   *00006100
006200*                           TO:    DATA-AREA-A    PIC X(8046).   *00006200
006300*                                                                *00006300
006400* P039    11-07-91  FRY   MODIFIED:                         .    *00006400
006500*                          FROM:   A-CONTRACT-ID    PIC X(17).   *00006500
006600*                            TO:   A-CONTRACT-ID    PIC X(18).   *00006600
006700*                          FROM:   B-CONTRACT-ID    PIC X(17).   *00006700
006800*                            TO:   B-CONTRACT-ID    PIC X(18).   *00006800
006900*                                                                *00006900
007000*          1/17/95  EMS   CONVERTED TO COBOL II.                 *00007000
007100*                                                                *00007100
007200*         11/06/96  MAM   (G&R) ADDED THE QUALITY CONTROL FILE   *00007200
007300*                         AND THE LOGIC THAT ENABLES IT TO BE    *00007300
007400*                         WRITTEN TO.                            *00007400
007500*                                                                *00007500
007600*         11/15/97  PHF   MILLENIUM CONVERSION                   *00007600
007700*                                                                *00007700
007800* 14726/  01/08/98  GSP  CHANGED RECORD LENGTH FROM 31 TO 42     *00007800
007900* 15057                  FOR USING TSGVSAM FOR ARCHIVE FILE.     *00007900
008000*                        CORRECTED INITIALIZATION OF QCF RECORD. *00008000
008100*                        (ZEROS WERE COMING OUT IN THE KEY OF    *00008100
008200*                        THE OUTPUT FILE.)                       *00008200
008300*                                                                *00008300
008400* P?????  03-17-98  FRY   MODIFIED:                              *00008400
008500*          REC-AREA-A      FROM:  PIC X(8076) TO  PIC X(8500)    *00008500
008600*          DATA-AREA-A     FROM:  PIC X(8046) TO  PIC X(8376)    *00008600
008700*          ADDED TO DATA-AREA-A     10  FILLER    PIC X(83)      *00008700
008800*          A-CONTRACT-ID   FROM:  PIC X(18)   TO  PIC X(29)      *00008800
008900*          B-CONTRACT-ID   FROM:  PIC X(18)   TO  PIC X(29)      *00008900
009000*          MOVE GCT-EFFDT-CEN  TO  ARCH-EFFDT-CEN                *00009000
009100*                    RATHER THAN   ARCH-EFF-DT                   *00009100
009200*                                                                *00009200
009300*                                                                *00009300
009400*            PER MARTY:    USE PARM-LOCATION RATHER THAN         *00009400
009500*                          GCT-PLAN-CODE OR GCT2-PLAN-CODE TO    *00009500
009600*                          UPDATE QCF-PLAN-CODE.                 *00009600
009700*                                                                *00009700
009800* ??????  11-16-99  GDM   MODIFY 0080-ADD-ARCH-CONT-RECORD       *00009800
009900*                         ROUTINE TO POPULATE ARCH-PLAN-CODE     *00009900
010000*                         FIELD.                                 *00010000
010100*                                                                *00010100
010200*  08-14-02     GTF       EXPAND OPERATOR ID FROM 5 TO 8 CHARS.  *00010200
010300*                                                                *00010300
010400*                                                                *00010400
010500*  10-11-02     AKK       REGEN AFTER GCCDRELN CHANGE            *00010500
010600*                                                                *00010600
010700*  02-28-07     DAF       CHANGE REC-AREA-A LENGTH TO MATCH THE  *00010700
010800*                         CORRECT LENGTH OF THE 2002 CONVERSION  *00010800
010900*                         ADDED CHECK IF GOING PAST THE LIMIT    *00010900
011000*                         OF OCCURRENCES                         *00011000
011100*                                                                *00011100
011200*  05-16-07     LR        RECOMPILE FOR CHANGES IN GCAUDITC      *00011200
      *                                                                *00011210
      *            P00028140 - 2022 ERS ACCOUNT UPDATES                *00011220
      *            06/21/22  TB   RECOMPILE FOR COPYBOOK GCARCHCT CHG  *00011230
      *                                                                *00011240
      *            P00027450 - NSA ASO OPT IN-OUT FIELD               * 00011250
      *            08/03/22  TB   RECOMPILE FOR COPYBOOK GCYCNVTC CHG * 00011260
      *                                                               * 00011270
      *            P00027450 - CORRECT NSA ASO OPT IN-OUT FIELD       * 00011280
      *            11/30/22  TB   RECOMPILE FOR COPYBOOK GCYCNVTC CHG * 00011290
      *                                                               * 00011291
      *            P00029516 - BALANCE BILLING PROTECTION OPTION FIELD* 00011292
      *            02/22/24  TB   RECOMPILE FOR COPYBOOK GCYCNVTC CHG * 00011293
      *                                                               * 00011294
      *            P00029711 - EXTERNAL FINANCE INDICATOR FIELD       * 00011295
      *            03/12/24  SI   RECOMPILE FOR COPYBOOK GCYCNVTC CHG * 00011296
      *                                                               * 00011297
      *            P00028776 - BALANCE BILLING PROTECTION OPTION FIELD* 00011298
      *            07/22/24  BB   RECOMPILE FOR COPYBOOK GCARCHCT CHG * 00011299
      *                                                                 00011300
      *            P00030333 - MULTI TIER INDICATOR FIELD             * 00011301
      *            07/14/25  CB   RECOMPILE FOR COPYBOOK GCYCNVTC CHG * 00011302
      *                                                               * 00011303
PG1125*            P000XXXXX - RX COUPON PROGRAM INDICATOR             *00011304
PG1125*            11/17/25  PG   RECOMPILE FOR COPYBOOK GCYCNVTC CHG  *00011305
PG1125*                                                                *00011306
TM0526*BBDA-66049  05/1/2026 RECOMPILE FOR A COPYBOOK CHANGE           *00011307
TM0526*                                  GCAUDITC                      *00011308
011300******************************************************************00011310
011400******************************************************************00011400
011500/                                                                 00011500
011600 ENVIRONMENT DIVISION.                                            00011600
011700                                                                  00011700
011800 CONFIGURATION SECTION.                                           00011800
011900 SOURCE-COMPUTER. IBM-370.                                        00011900
012000 OBJECT-COMPUTER. IBM-370.                                        00012000
012100                                                                  00012100
012200                                                                  00012200
012300 INPUT-OUTPUT SECTION.                                            00012300
012400 FILE-CONTROL.                                                    00012400
012500*MAM G&R - ADDED QCF-FILE                                         00012500
012600     SELECT RCL-FILE   ASSIGN TO UT-S-GC03310A.                   00012600
012700     SELECT BIM-FILE   ASSIGN TO UT-S-GC03310B.                   00012700
012800     SELECT AUD-FILE   ASSIGN TO UT-S-GC03310C.                   00012800
012900     SELECT QCF-FILE   ASSIGN TO UT-S-GC03310D.                   00012900
013000/                                                                 00013000
013100 DATA DIVISION.                                                   00013100
013200 FILE SECTION.                                                    00013200
013300 FD  BIM-FILE                                                     00013300
013400     LABEL RECORDS ARE STANDARD                                   00013400
013500     RECORDING MODE IS V                                          00013500
013600     BLOCK CONTAINS 0 RECORDS.                                    00013600
013700 01  BIM-RECORD.                                                  00013700
013800     COPY GCWRKDCC.                                               00013800
013900     COPY GCCONTRC.                                               00013900
014000/                                                                 00014000
014100 FD  RCL-FILE                                                     00014100
014200     LABEL RECORDS ARE STANDARD                                   00014200
014300     RECORDING MODE IS V                                          00014300
014400     BLOCK CONTAINS 0 RECORDS.                                    00014400
014500 01  RCL-RECORD.                                                  00014500
014600     COPY GCWRKDC2.                                               00014600
014700     COPY GCCONTR2.                                               00014700
014800/                                                                 00014800
014900 FD  AUD-FILE                                                     00014900
015000     LABEL RECORDS ARE STANDARD                                   00015000
015100     RECORDING MODE IS V                                          00015100
015200     BLOCK CONTAINS 0 RECORDS.                                    00015200
015300 01  AUD-RECORD.                                                  00015300
015400     COPY GCWRKDC3.                                               00015400
015500     COPY GCAUDITC.                                               00015500
015600/                                                                 00015600
015700*MAM G&R - ADDED FD FOR QCF                                       00015700
015800 FD  QCF-FILE                                                     00015800
015900     LABEL RECORDS ARE STANDARD                                   00015900
016000     RECORDING MODE IS F                                          00016000
016100     BLOCK CONTAINS 0 RECORDS.                                    00016100
016200 01  QCF-RECORD.                                                  00016200
016300     COPY GCQCF.                                                  00016300
016400/                                                                 00016400
016500 WORKING-STORAGE SECTION.                                         00016500
016600 01  FILLER                         PIC X(24)   VALUE             00016600
016700     'GC03310 WORKING-STORAGE'.                                   00016700
016800                                                                  00016800
016900 01  ABEND-CODE                       PIC 9(4)    COMP.           00016900
017000* 8/14/02 EXPAND OPID BY 3 TO 8 BYTES. GTF                        00017000
017100 01  WS-OPER-ID               PIC X(08) VALUE SPACES.             00017100
017200 01  RPT-IND    VALUE '3310'  PIC X(4).                           00017200
017300                                                                  00017300
017400     COPY MLDATE01.                                               00017400
017500 01  JUL-DATE.                                                    00017500
017600     05  JUL-DT.                                                  00017600
017700         10  JUL-CC                 PIC 99.                       00017700
017800         10  JUL-YY                 PIC 99.                       00017800
017900         10  JUL-DD                 PIC 999.                      00017900
018000     05  TODAYS-DATE REDEFINES  JUL-DT PIC 9(7).                  00018000
018100                                                                  00018100
018200 01  END-OF-FILE-B-SW               PIC XXX  VALUE SPACES.        00018200
018300     88  END-OF-BIM-FILE                     VALUE 'END'.         00018300
018400                                                                  00018400
018500 01  END-OF-FILE-A-SW               PIC XXX  VALUE SPACES.        00018500
018600     88  END-OF-RCL-FILE                     VALUE 'END'.         00018600
018700                                                                  00018700
018800 01  END-OF-FILE-C-SW               PIC XXX  VALUE SPACES.        00018800
018900     88  END-OF-AUD-FILE                     VALUE 'END'.         00018900
019000                                                                  00019000
019100 01  READ-IND                       PIC XXX VALUE SPACES.         00019100
019200     88  AFTER-FOUND                         VALUE 'YES'.         00019200
019300                                                                  00019300
019400 01  TAB-POINTER-SW                 PIC XXX  VALUE SPACES.        00019400
019500     88  TAB-POINTERS-END                    VALUE 'END'.         00019500
019600                                                                  00019600
019700 01  BEN-POINTER-SW                 PIC XXX  VALUE SPACES.        00019700
019800     88  BEN-POINTERS-END                    VALUE 'END'.         00019800
019900                                                                  00019900
020000*MAM G&R - ADDED A SWITCH TO CHECK INTER REL CD                   00020000
020100 01  WS-INTER-REL-SW                PIC X    VALUE 'Y'.           00020100
020200     88  INTER-REL-CD-FND                    VALUE 'Y'.           00020200
020300     88  INTER-REL-CD-NTFND                  VALUE 'N'.           00020300
020400                                                                  00020400
020500 01  WS-AUD-ID.                                                   00020500
020600     05  WS-AUD-PRE                 PIC XX  VALUE 'AU'.           00020600
020700     05  WS-AUD-FUNC                PIC X(6).                     00020700
020800                                                                  00020800
020900 01  AREA-A.                                                      00020900
021000     05  AFTER-ID                   PIC X(6).                     00021000
021100     05  AFTER-SLOT     COMP-3      PIC S9(7).                    00021100
021200                                                                  00021200
021300 01  AREA-B.                                                      00021300
021400     05  BEFORE-ID                  PIC X(6).                     00021400
021500     05  BEFORE-SLOT    COMP-3      PIC S9(7).                    00021500
021600                                                                  00021600
021700                                                                  00021700
021800 01  PARM-SET.                                                    00021800
021900     05  SET-RDW.                                                 00021900
022000         10  SET-RECORD-LENGTH      PIC 9(4) VALUE ZEROS  COMP.   00022000
022100         10  SET-FEEDBACK           PIC 9(4) VALUE ZEROS  COMP.   00022100
022200     05  SET-VALUE                  PIC 9(8) VALUE ZEROS  COMP.   00022200
022300                                                                  00022300
022400***  THE FOLLOWING IS THE ARCHIVED RECORD I/O AREA ***            00022400
022500 01  PARM-ONE-A.                                                  00022500
022600     05  RESERVED-FLDS-A            PIC 9(8)    VALUE ZEROS COMP. 00022600
022700     05  RESERVED-ONE-A REDEFINES RESERVED-FLDS-A.                00022700
022800         10  REQUEST-TYPE-A         PIC X.                        00022800
022900         10  FILLER                 PIC X(3).                     00022900
023000                                                                  00023000
023100 01  PARM-TWO-A.                                                  00023100
023200     05  RDW-A.                                                   00023200
023300         10  RECORD-LENGTH-A        PIC 9(4)  COMP.               00023300
023400         10  FEEDBACK-CODE-A        PIC 9(4)  COMP.               00023400
023500     05  REC-AREA-A                 PIC X(9547).                  00023500
023600     05  RECORD-A REDEFINES REC-AREA-A.                           00023600
023700         10  KEY-FIELD-A.                                         00023700
023800             15  KEY-TYPE-A         PIC X.                        00023800
023900             15  KEY-ID-A.                                        00023900
024000                 17  K-PLAN-CODE    PIC X(03).                    00024000
024100                 17  K-G-N          PIC X(09).                    00024100
024200                 17  K-S-N          PIC X(05).                    00024200
024300                 17  K-PKG-CODE     PIC X(03).                    00024300
024400                 17  K-LOB          PIC X.                        00024400
024500                 17  K-P-C          PIC XX.                       00024500
024600                 17  K-F-R          PIC XX.                       00024600
024700                 17  K-E-DT         PIC S9(07) COMP-3.            00024700
024800             15  KEY-NO-A           PIC X(08).                    00024800
024900         10  PNTRS-COUNT-A          PIC S9(5) COMP-3.             00024900
025000         10  FILLER                 PIC X(83).                    00025000
025100         10  DATA-AREA-A            PIC X(9423).                  00025100
025200                                                                  00025200
025300***  ARCHIVED DATA ELEMENT CODE FOR TABULAR PROVISIONS ***        00025300
025400 01  TAB-ARCH-DE-CD.                                              00025400
025500     05  TB-ARCH-DE-1                PIC XX  VALUE 'TT'.          00025500
025600     05  TB-ARCH-DE-2                PIC X(6).                    00025600
025700                                                                  00025700
025800***  ARCHIVED DATA ELEMENT CODE FOR BENEFIT PROVISIONS ***        00025800
025900 01  BEN-ARCH-DE-CD.                                              00025900
026000     05  BN-ARCH-DE-1                PIC XX  VALUE 'BB'.          00026000
026100     05  BN-ARCH-DE-2                PIC X(6).                    00026100
026200                                                                  00026200
026300 01  PRINT-TOTALS.                                                00026300
026400     05  ARCH-PRINT-1                PIC ZZZZZZ999.               00026400
026500     05  ARCH-PRINT-2                PIC ZZZZZZ999.               00026500
026600     05  QCF-PRINT                   PIC ZZZZZZ999.               00026600
026700     05  BYPASS-PRINT                PIC ZZZZZZ999.               00026700
026800                                                                  00026800
026900 01  COUNTER-AREA.                                                00026900
027000     05  WS-QCF-COUNT                 PIC S9(9)  VALUE ZEROS.     00027000
027100     05  WS-BYPASS-CNT                PIC S9(9)  VALUE ZEROS.     00027100
027200     05  ARCH-RECS-BUILD              PIC S9(9)  VALUE ZEROS.     00027200
027300     05  ARCH-ENTRS-INSERT            PIC S9(9)  VALUE ZEROS.     00027300
027400                                                                  00027400
027500/                                                                 00027500
027600 01  WS-REC-LEN-AREA.                                             00027600
027700 COPY GCCDRLEN.                                                   00027700
027800/                                                                 00027800
027900 01  A-CONTRACT-ID                    PIC X(29).                  00027900
028000                                                                  00028000
028100 01  B-CONTRACT-ID                    PIC X(29).                  00028100
028200/                                                                 00028200
028300 01  CONT-ARCH-AREA.                                              00028300
028400 COPY GCARCHCC.                                                   00028400
028500/                                                                 00028500
028600 COPY GCARCHCT.                                                   00028600
028700/                                                                 00028700
028800 01  COMP-CONTRACT-BEFORE-AREA.                                   00028800
028900     COPY GCAR310B.                                               00028900
029000                                                                  00029000
029100 01  TOMP-CONTRACT-AFTER-AREA.                                    00029100
029200     COPY GCAR310A.                                               00029200
029300/                                                                 00029300
029400*MAM G&R - ADDED LINKAGE SECTION                                  00029400
029500 LINKAGE SECTION.                                                 00029500
029600 01  PARM-AREA.                                                   00029600
029700     05  PARM-LENGTH      PIC S9(04) COMP.                        00029700
029800     05  PARM-LOCATION    PIC X(03).                              00029800
029900/                                                                 00029900
030000*MAM G&R - ADDED USING PARM-AREA                                  00030000
030100 PROCEDURE DIVISION USING PARM-AREA.                              00030100
030200 0000-MAINLINE.                                                   00030200
030300     OPEN   INPUT      RCL-FILE                                   00030300
030400                       AUD-FILE                                   00030400
030500                       BIM-FILE.                                  00030500
030600                                                                  00030600
030700*MAM G&R - ADDED OPEN FOR QCF-FILE                                00030700
030800     OPEN   OUTPUT     QCF-FILE.                                  00030800
030900     MOVE '3310' TO RPT-IND.                                      00030900
031000                                                                  00031000
031100***   TSGVSAM3 IS GCPS ARCHIVED FILE (INPUT/OUTPUT)               00031100
031200     MOVE   'S'                  TO REQUEST-TYPE-A.               00031200
031300     MOVE    8                   TO SET-RECORD-LENGTH.            00031300
031400     MOVE    3                   TO SET-VALUE.                    00031400
031500     CALL   'TSGVSAM3'  USING PARM-ONE-A PARM-SET.                00031500
031600     IF  REQUEST-TYPE-A NOT EQUAL 'S'                             00031600
031700         MOVE SET-FEEDBACK       TO ABEND-CODE                    00031700
031800         GO TO 9999-ERROR-RTN.                                    00031800
031900                                                                  00031900
032000                                                                  00032000
032100     MOVE   'O'                  TO REQUEST-TYPE-A.               00032100
032200     CALL   'TSGVSAM3'  USING PARM-ONE-A PARM-TWO-A.              00032200
032300     IF  REQUEST-TYPE-A NOT EQUAL 'O'                             00032300
032400         MOVE SET-FEEDBACK       TO ABEND-CODE                    00032400
032500         GO TO 9999-ERROR-RTN.                                    00032500
032600                                                                  00032600
032700                                                                  00032700
032800*MAM G&R - ADDED PARM-LENGTH CHECK                                00032800
032900     IF PARM-LENGTH NOT = 3                                       00032900
033000         DISPLAY 'ABENDED ON PARM-LENGTH: ' PARM-LENGTH           00033000
033100         GO TO 9999-ERROR-RTN.                                    00033100
033200                                                                  00033200
033300     MOVE 'TDY' TO MLDATE-FUNC.                                   00033300
033400     MOVE 'J' TO MLDATE-FORM1.                                    00033400
033500     CALL 'MLDATE' USING MLDATE01.                                00033500
033600     MOVE MLDATE-JUL1  TO JUL-DATE.                               00033600
033700                                                                  00033700
033800     PERFORM 0010-PROCESS-RTN  THRU 0010-EXIT                     00033800
033900         UNTIL END-OF-BIM-FILE.                                   00033900
034000                                                                  00034000
034100     PERFORM 0012-PROCESS-AUD  THRU 0012-EXIT                     00034100
034200         UNTIL END-OF-AUD-FILE.                                   00034200
034300                                                                  00034300
034400     MOVE   'C'                  TO REQUEST-TYPE-A.               00034400
034500     CALL   'TSGVSAM3'  USING PARM-ONE-A PARM-TWO-A.              00034500
034600     IF  REQUEST-TYPE-A NOT EQUAL 'C'                             00034600
034700         MOVE FEEDBACK-CODE-A    TO ABEND-CODE                    00034700
034800         GO TO 9999-ERROR-RTN.                                    00034800
034900                                                                  00034900
035000                                                                  00035000
035100*MAM G&R - ADDED CLOSE FOR QCF-FILE                               00035100
035200     CLOSE  RCL-FILE                                              00035200
035300            AUD-FILE                                              00035300
035400            BIM-FILE                                              00035400
035500            QCF-FILE.                                             00035500
035600                                                                  00035600
035700     DISPLAY '       ----------------------------      '.         00035700
035800     DISPLAY '  *** CONTRACT ARCHIVED AUDIT TRAIL ***  '.         00035800
035900     DISPLAY '       ----------------------------      '.         00035900
036000     MOVE ARCH-RECS-BUILD    TO ARCH-PRINT-1.                     00036000
036100     MOVE ARCH-ENTRS-INSERT  TO ARCH-PRINT-2.                     00036100
036200     MOVE WS-QCF-COUNT       TO QCF-PRINT.                        00036200
036300     MOVE WS-BYPASS-CNT      TO BYPASS-PRINT.                     00036300
036400     DISPLAY 'TOTAL RECORDS CREATED  = '  ARCH-PRINT-1.           00036400
036500     DISPLAY 'TOTAL ENTRIES INSERTED = '  ARCH-PRINT-2.           00036500
036600     DISPLAY 'TOTAL QCF RECORDS      = '  QCF-PRINT.              00036600
036700     DISPLAY 'TOTAL BYPASSED QCF REC = '  BYPASS-PRINT.           00036700
036800     GOBACK.                                                      00036800
036900                                                                  00036900
037000 0000-EXIT.                                                       00037000
037100      EXIT.                                                       00037100
037200/                                                                 00037200
037300 0010-PROCESS-RTN.                                                00037300
037400                                                                  00037400
037500*** CLEAR BEFORE-IMAGE AND AFTER-IMAGE AREAS FIRST ***            00037500
037600     MOVE   SPACES  TO  GCT-CONTRACT-BEFORE-AREA.                 00037600
037700     MOVE   SPACES  TO  GCT2-CONTRACT-AFTER-AREA.                 00037700
037800                                                                  00037800
037900*** READ BEFORE IMAGE CONTRACT WORK FILE ***                      00037900
038000                                                                  00038000
038100     READ  BIM-FILE                                               00038100
038200           AT END  MOVE 'END'  TO END-OF-FILE-B-SW                00038200
038300                   GO TO 0010-EXIT.                               00038300
038400***                                                               00038400
038500*** IF BIM RECORD ONLINE-SIGNAL-INDICATOR IS AN ADD (MAP FROM)    00038500
038600*** OR KEY FIELD CHANGES THEN  BYPASS THE ARCHIVING LOGIC         00038600
038700***                                                               00038700
038800     IF  WRK-ADD-REQUEST                                          00038800
038900         GO TO 0010-EXIT.                                         00038900
039000                                                                  00039000
039100                                                                  00039100
039200     MOVE   CORR     GCT-CONTRACT-RECORD OF BIM-RECORD  TO        00039200
039300                         GCT-CONTRACT-BEFORE-AREA.                00039300
039400                                                                  00039400
039500     MOVE   CORR     GCT-CONTRACT-RECORD OF BIM-RECORD  TO        00039500
039600                         GCT-ARCHIVED-COMMON-TABLE.               00039600
039700                                                                  00039700
039800     MOVE GCT-TERMDT-CEN OF GCT-CONTRACT-RECORD TO                00039800
039900          GCT-TERMDT-CEN OF GCT-ARCHIVED-COMMON-TABLE             00039900
040000          GCT-TERMN-T    OF GCT-CONTRACT-BEFORE-AREA.             00040000
040100                                                                  00040100
040200     MOVE GCT-CONTRACT-ID OF  BIM-RECORD                          00040200
040300                           TO  B-CONTRACT-ID.                     00040300
040400                                                                  00040400
040500*** READ AFTER-IMAGE CONTRACT RELEASED WORK FILE ***              00040500
040600                                                                  00040600
040700     PERFORM 0015-READ-AFTER-RTN  THRU 0015-EXIT.                 00040700
040800                                                                  00040800
040900     MOVE   CORR  GCT2-CONTRACT-RECORD OF RCL-RECORD  TO          00040900
041000                         GCT2-CONTRACT-AFTER-AREA.                00041000
041100                                                                  00041100
041200     MOVE GCT2-TERMDT-CEN OF GCT2-CONTRACT-RECORD TO              00041200
041300          GCT2-TERMN-DT  OF GCT2-CONTRACT-AFTER-AREA.             00041300
041400                                                                  00041400
041500     MOVE GCT2-CONTRACT-ID OF  RCL-RECORD                         00041500
041600                           TO  A-CONTRACT-ID.                     00041600
041700     MOVE SPACES  TO READ-IND.                                    00041700
041800*MAM                                                              00041800
041900     MOVE 'Y'     TO WS-INTER-REL-SW.                             00041900
042000                                                                  00042000
042100*** COMPARE BEFORE-IMAGE AND AFTER-IMAGE RECORDS, CONTRACT FIELDS 00042100
042200     PERFORM 0020-CONT-LVL-COMP-RTN  THRU 0020-EXIT.              00042200
042300                                                                  00042300
042400                                                                  00042400
042500*** COMPARE BIM AND AIM TABULAR IDS ATTACHED TO EACH RECORD.      00042500
042600     SET GCT2-TAB-INDEX   TO 1.                                   00042600
042700     SET GCT-TAB-INDEX    TO 1.                                   00042700
042800     PERFORM 0030-TAB-POINTERS-RTN  THRU 0030-EXIT                00042800
042900                  UNTIL TAB-POINTERS-END.                         00042900
043000     MOVE SPACES  TO TAB-POINTER-SW.                              00043000
043100                                                                  00043100
043200*** COMPARE BIM AND AIM BENEFIT PROVISIONS IDS ATTACHED TO EACH.  00043200
043300     IF (GCT-COUNT-BEN-PROVN-POINTERS GREATER THAN 1)    OR       00043300
043400        (GCT2-COUNT-BEN-PROVN-POINTERS GREATER THAN 1)            00043400
043500        SET GCT2-INDEX  GCT-INDEX  TO 1                           00043500
043600        PERFORM 0040-BEN-POINTERS-RTN  THRU 0040-EXIT             00043600
043700           VARYING GCT2-INDEX FROM 1 BY 1                         00043700
043800                     UNTIL BEN-POINTERS-END.                      00043800
043900     MOVE SPACES  TO BEN-POINTER-SW.                              00043900
044000                                                                  00044000
044100                                                                  00044100
044200 0010-EXIT.                                                       00044200
044300      EXIT.                                                       00044300
044400/                                                                 00044400
044500 0012-PROCESS-AUD.                                                00044500
044600                                                                  00044600
044700     READ   AUD-FILE                                              00044700
044800                  AT END                                          00044800
044900                  MOVE 'END'     TO END-OF-FILE-C-SW              00044900
045000                                    GO TO 0012-EXIT.              00045000
045100     IF WRK3-REC-TYPE =  'C9'                                     00045100
045200        NEXT SENTENCE                                             00045200
045300     ELSE                                                         00045300
045400        GO TO  0012-EXIT.                                         00045400
045500                                                                  00045500
045600     PERFORM  0071-PROCESS-ARCH-AUD  THRU 0071-EXIT.              00045600
045700                                                                  00045700
045800 0012-EXIT.                                                       00045800
045900      EXIT.                                                       00045900
046000/                                                                 00046000
046100 0015-READ-AFTER-RTN.                                             00046100
046200                                                                  00046200
046300     PERFORM 001510-READ-RCL-RTN  THRU 001510-EXIT                00046300
046400        UNTIL (AFTER-FOUND OR END-OF-RCL-FILE).                   00046400
046500                                                                  00046500
046600     IF END-OF-RCL-FILE                                           00046600
046700        MOVE '1099' TO ABEND-CODE                                 00046700
046800        GO TO 9999-ERROR-RTN.                                     00046800
046900                                                                  00046900
047000 0015-EXIT.                                                       00047000
047100     EXIT.                                                        00047100
047200/                                                                 00047200
047300 001510-READ-RCL-RTN.                                             00047300
047400                                                                  00047400
047500     READ  RCL-FILE                                               00047500
047600           AT END  MOVE 'END'  TO END-OF-FILE-A-SW                00047600
047700                   GO TO 001510-EXIT.                             00047700
047800                                                                  00047800
047900     IF WRK2-SIG-B-SKELETON-C2-G2                                 00047900
048000         GO TO 001510-EXIT.                                       00048000
048100                                                                  00048100
048200     IF WRK2-KEY-FLD-DEL-REQ  OR                                  00048200
048300        WRK2-KEY-FLD-ADD-REQ                                      00048300
048400         GO TO 001510-EXIT.                                       00048400
048500                                                                  00048500
048600     IF WRK2-MATCH-CONT EQUAL WRK-MATCH-CONT AND                  00048600
048700        WRK2-REC-CONT                                             00048700
048800         MOVE 'YES'  TO READ-IND                                  00048800
048900         GO TO 001510-EXIT.                                       00048900
049000                                                                  00049000
049100 001510-EXIT.                                                     00049100
049200     EXIT.                                                        00049200
049300/                                                                 00049300
049400 0020-CONT-LVL-COMP-RTN.                                          00049400
049500                                                                  00049500
049600***(GCT) REFER TO BEFORE-IMAGE REC, (GCT2) TO THE AFTER-IMAGE REC 00049600
049700                                                                  00049700
049800*** COMPARE CONTRACT DATA ELEMENTS FIELDS ON THE BEFORE           00049800
049900*** AND AFTER IMAGE FILES.                                        00049900
050000                                                                  00050000
050100     SET CON-A-INDX   TO 1.                                       00050100
050200     PERFORM 002020-COMP-RTN THRU 002020-EXIT                     00050200
050300       VARYING CON-A-INDX FROM 1 BY 1 UNTIL                       00050300
050400         CON-A-INDX > GCT-CONTRACT-BEFORE-OCCURS-CNT.             00050400
050500                                                                  00050500
050600 0020-EXIT.                                                       00050600
050700      EXIT.                                                       00050700
050800/                                                                 00050800
050900 002020-COMP-RTN.                                                 00050900
051000                                                                  00051000
051100     SET CON-B-INDX   TO  CON-A-INDX.                             00051100
051200                                                                  00051200
051300     IF BEFORE-FIELD (CON-B-INDX)  EQUAL                          00051300
051400        AFTER-FIELD  (CON-A-INDX)                                 00051400
051500          GO TO 002020-EXIT                                       00051500
051600     ELSE                                                         00051600
051700          PERFORM 0070-PROCESS-ARCH-CONT  THRU 0070-EXIT.         00051700
051800                                                                  00051800
051900 002020-EXIT.                                                     00051900
052000     EXIT.                                                        00052000
052100/                                                                 00052100
052200 0030-TAB-POINTERS-RTN.                                           00052200
052300                                                                  00052300
052400*** AT THE END OF TABULARS IDS SEARCH, PUT THE END SWITCH ON      00052400
052500                                                                  00052500
052600     IF GCT2-CON-TAB-ID (GCT2-TAB-INDEX) EQUAL HIGH-VALUES AND    00052600
052700        GCT-CON-TAB-ID (GCT-TAB-INDEX) EQUAL HIGH-VALUES          00052700
052800          MOVE 'END'   TO TAB-POINTER-SW                          00052800
052900          GO TO 0030-EXIT.                                        00052900
053000                                                                  00053000
053100*** IF AFTER-IMAGE TAB ID IS GREATER THAN BEFORE-IMAGE TAB ID,    00053100
053200*** THAT TAB ID HAS BEEN DELETED FROM THE CONT RECORD. A RECORD   00053200
053300*** ON THE ARCHIVE FILE (OR AN ENTRY) WILL BE CREATED FROM BEFORE 00053300
053400*** IMAGE FILE (AIM DOES NOT HAVE THE TAB ID ANYMORE).            00053400
053500                                                                  00053500
053600     IF GCT2-CON-TAB-ID (GCT2-TAB-INDEX) GREATER THAN             00053600
053700        GCT-CON-TAB-ID (GCT-TAB-INDEX)                            00053700
053800          PERFORM 003020-LOAD-DEL-RTN THRU 003020-EXIT            00053800
053900          SET GCT-TAB-INDEX UP BY 1                               00053900
054000          GO TO 0030-EXIT.                                        00054000
054100                                                                  00054100
054200*** IF AFTER-IMAGE TAB ID IS LESS THAN BEFORE-IMAGE TAB ID, THEN  00054200
054300*** THAT TAB ID HAS BEEN ADDED TO THE CONTRACT RECORD. A RECORD   00054300
054400*** ON THE ARCHIVED FILE (OR AN ENTRY) WILL BE CREATED FROM AFTER 00054400
054500*** IMAGE FILE (WHERE THE TAB ID JUST HAS BEEN ADDED) BUT THE SLOT00054500
054600*** NUMBER WILL BE 'ADDED' (DE VALUE) INDICATING THAT THE TABULAR 00054600
054700*** HAS JUST BEEN ADDED.                                          00054700
054800                                                                  00054800
054900     IF GCT2-CON-TAB-ID (GCT2-TAB-INDEX) LESS THAN                00054900
055000        GCT-CON-TAB-ID (GCT-TAB-INDEX)                            00055000
055100          PERFORM 003030-LOAD-ADD-RTN THRU 003030-EXIT            00055100
055200          SET GCT2-TAB-INDEX UP BY 1                              00055200
055300          GO TO 0030-EXIT.                                        00055300
055400                                                                  00055400
055500*** IF AFTER IMAGE TAB ID EQUAL BEFORE IMAGE TAB ID THEN, IF SLOT 00055500
055600*** NUMBER NOT EQUAL ON BOTH TABULARS, THEN CREATE AN ARCHIVED REC00055600
055700*** (OR ENTRY) ON THE ARCHIVED FILE.                              00055700
055800                                                                  00055800
055900     IF GCT2-CON-TAB-ID (GCT2-TAB-INDEX) EQUAL                    00055900
056000        GCT-CON-TAB-ID (GCT-TAB-INDEX)                            00056000
056100          PERFORM 003040-LOAD-EQ-RTN THRU 003040-EXIT             00056100
056200          SET GCT-TAB-INDEX UP BY 1                               00056200
056300          SET GCT2-TAB-INDEX UP BY 1                              00056300
056400          GO TO 0030-EXIT.                                        00056400
056500                                                                  00056500
056600 0030-EXIT.                                                       00056600
056700     EXIT.                                                        00056700
056800/                                                                 00056800
056900 003020-LOAD-DEL-RTN.                                             00056900
057000                                                                  00057000
057100     IF GCT-CON-TAB-ID (GCT-TAB-INDEX) = HIGH-VALUES              00057100
057200        GO TO 003020-EXIT.                                        00057200
057300                                                                  00057300
057400     MOVE 'C'              TO KEY-TYPE-A.                         00057400
057500     MOVE B-CONTRACT-ID    TO KEY-ID-A.                           00057500
057600     MOVE GCT-CON-TAB-ID (GCT-TAB-INDEX)                          00057600
057700                           TO TB-ARCH-DE-2.                       00057700
057800     MOVE TAB-ARCH-DE-CD   TO KEY-NO-A.                           00057800
057900                                                                  00057900
058000     MOVE  'R'             TO REQUEST-TYPE-A.                     00058000
058100     MOVE  42              TO RECORD-LENGTH-A.                    00058100
058200     CALL   'TSGVSAM3'  USING PARM-ONE-A                          00058200
058300                              PARM-TWO-A.                         00058300
058400                                                                  00058400
058500***  RECORD NOT FOUND OR EOF THEN CREATE A NEW ARCHIVE RECORD     00058500
058600***  FROM BEFORE-IMAGE RECORD                                     00058600
058700     IF  REQUEST-TYPE-A  = '2'  OR  '3'                           00058700
058800         PERFORM 0082-ADD-ARCH-TAB-RECORD THRU 0082-EXIT          00058800
058900         ADD +1  TO ARCH-RECS-BUILD                               00058900
059000         GO TO 003020-EXIT.                                       00059000
059100                                                                  00059100
059200     IF  REQUEST-TYPE-A NOT EQUAL 'R'                             00059200
059300         MOVE FEEDBACK-CODE-A    TO ABEND-CODE                    00059300
059400         GO TO 9999-ERROR-RTN.                                    00059400
059500                                                                  00059500
059600***  RECORD IS FOUND, THEN ADD ANOTHER ARCHIVING ENTRY            00059600
059700***  FROM BEFORE-IMAGE RECORD                                     00059700
059800                                                                  00059800
059900*MAM G&R - ADDED QUAL CTRL LOGIC                                  00059900
060000     INITIALIZE QCF-RECORD.                                       00060000
060100     MOVE ZEROS            TO QCF-PLAN-CODE                       00060100
060200                              QCF-GROUP-NO                        00060200
060300                              QCF-SECTION-NO                      00060300
060400                              QCF-PKG-CODE.                       00060400
060500     MOVE SPACES           TO QCF-FILLER.                         00060500
060600     MOVE 'C'              TO QCF-CONTRACT-GROUP-SP-IND.          00060600
060700     MOVE PARM-LOCATION    TO QCF-PLAN-CODE.                      00060700
060800     MOVE TAB-ARCH-DE-CD   TO QCF-FUNC-FIELD.                     00060800
060900     MOVE GCT-CON-TAB-ID (GCT-TAB-INDEX) TO QCF-BIM-FIELD         00060900
061000     MOVE 'DELETED'        TO QCF-AIM-FIELD                       00061000
061100                                                                  00061100
061200     MOVE GCT-GROUP-NUM OF BIM-RECORD TO QCF-GROUP-NO.            00061200
061300     MOVE GCT-SECTION-NUM OF BIM-RECORD TO QCF-SECTION-NO.        00061300
061400     MOVE GCT-PKG-CODE OF BIM-RECORD TO QCF-PKG-CODE              00061400
061500     MOVE GCT-L-O-B    OF BIM-RECORD TO QCF-L-O-B.                00061500
061600     MOVE GCT-PROVDR-CONTROL OF BIM-RECORD TO QCF-PROV-CTRL.      00061600
061700     MOVE GCT-FAM-REL-LVL    OF BIM-RECORD TO QCF-FAM-REL-LEVEL.  00061700
061800     MOVE WRK-OPERATOR-ID                  TO QCF-OPERATOR-ID.    00061800
061900                                                                  00061900
062000                                                                  00062000
062100     IF TODAYS-DATE < 9999999                                     00062100
062200        MOVE TODAYS-DATE   TO QCF-FUNC-DATE.                      00062200
062300*         QCF-FUNC-DATE = +1900000 + TODAYS-DATE.                 00062300
062400                                                                  00062400
062500*    IF GCT-EFF-DT OF BIM-RECORD  < 99999                         00062500
062600     IF GCT-EFFDT-CEN OF BIM-RECORD < 9999999                     00062600
062700        MOVE GCT-EFFDT-CEN OF BIM-RECORD TO QCF-EFF-DATE.         00062700
062800*       COMPUTE                                                   00062800
062900*         QCF-EFF-DATE = +1900000 + GCT-EFF-DT OF BIM-RECORD.     00062900
063000                                                                  00063000
063100     IF GCT2-INTER-REL-CD = ZEROS                                 00063100
063200        MOVE 'N' TO WS-INTER-REL-SW                               00063200
063300        ADD +1   TO WS-BYPASS-CNT                                 00063300
063400     ELSE                                                         00063400
063500        MOVE 'Y' TO WS-INTER-REL-SW.                              00063500
063600                                                                  00063600
063700     IF WRK-OPERATOR-ID > SPACES AND                              00063700
063800        INTER-REL-CD-FND                                          00063800
063900        PERFORM 0084-WRITE-QUAL-CTRL THRU 0084-EXIT.              00063900
064000                                                                  00064000
064100     MOVE PNTRS-COUNT-A  TO ARCH-POINTERS-COUNT.                  00064100
064200     MOVE REC-AREA-A     TO ARCHIVED-GCPS-RECORD.                 00064200
064300                                                                  00064300
064400     ADD +1                TO ARCH-POINTERS-COUNT.                00064400
064500     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00064500
064600         GO TO 003020-EXIT.                                       00064600
064700     SET ARCH-INDEX        TO ARCH-POINTERS-COUNT.                00064700
064800     MOVE GCT-CON-TAB-SLOT (GCT-TAB-INDEX)                        00064800
064900                           TO ARCH-FLD-VALUE (ARCH-INDEX).        00064900
065000     MOVE WRK-OPERATOR-ID  TO ARCH-OPER-ID (ARCH-INDEX).          00065000
065100                                                                  00065100
065200     IF WRK-CDE-SP NOT = '2 '                                     00065200
065300        MOVE 'X'  TO ARCH-CDE-IND (ARCH-INDEX)                    00065300
065400     ELSE                                                         00065400
065500        MOVE ' '  TO ARCH-CDE-IND (ARCH-INDEX).                   00065500
065600                                                                  00065600
065700     MOVE TODAYS-DATE         TO ARCH-CHNG-DT-CEN (ARCH-INDEX).   00065700
065800     MOVE SPACES              TO ARCH-ANLS-CD (ARCH-INDEX).       00065800
065900                                                                  00065900
066000     IF WRK-ATB3-REQUEST                                          00066000
066100        MOVE WRK-SIGNAL-FROM-ONLINE TO ARCH-ATB-IND (ARCH-INDEX)  00066100
066200     ELSE                                                         00066200
066300        MOVE WRK-TYPE-MAINT-IND     TO ARCH-ATB-IND (ARCH-INDEX). 00066300
066400                                                                  00066400
066500     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00066500
066600     ADD +1  TO ARCH-ENTRS-INSERT.                                00066600
066700                                                                  00066700
066800 003020-EXIT.                                                     00066800
066900     EXIT.                                                        00066900
067000/                                                                 00067000
067100 003030-LOAD-ADD-RTN.                                             00067100
067200                                                                  00067200
067300     IF GCT2-CON-TAB-ID (GCT2-TAB-INDEX) = HIGH-VALUES            00067300
067400        GO TO  003030-EXIT.                                       00067400
067500                                                                  00067500
067600     MOVE 'C'             TO KEY-TYPE-A.                          00067600
067700     MOVE B-CONTRACT-ID   TO KEY-ID-A.                            00067700
067800     MOVE GCT2-CON-TAB-ID (GCT2-TAB-INDEX)                        00067800
067900                          TO  TB-ARCH-DE-2.                       00067900
068000     MOVE TAB-ARCH-DE-CD  TO  KEY-NO-A.                           00068000
068100                                                                  00068100
068200     MOVE  'R'            TO REQUEST-TYPE-A.                      00068200
068300     MOVE  42             TO RECORD-LENGTH-A.                     00068300
068400     CALL   'TSGVSAM3'    USING PARM-ONE-A                        00068400
068500                                PARM-TWO-A.                       00068500
068600                                                                  00068600
068700***  RECORD NOT FOUND OR EOF THEN CREATE A NEW ARCHIVE RECORD     00068700
068800***  FROM AFTER-IMAGE RECORD                                      00068800
068900                                                                  00068900
069000     IF  REQUEST-TYPE-A  = '2'  OR  '3'                           00069000
069100         PERFORM 0081-ADD-ARCH-TAB-RECORD THRU 0081-EXIT          00069100
069200         ADD +1  TO ARCH-RECS-BUILD                               00069200
069300         GO TO 003030-EXIT.                                       00069300
069400                                                                  00069400
069500     IF  REQUEST-TYPE-A NOT EQUAL 'R'                             00069500
069600         MOVE FEEDBACK-CODE-A    TO ABEND-CODE                    00069600
069700         GO TO 9999-ERROR-RTN.                                    00069700
069800                                                                  00069800
069900***  RECORD IS FOUND, THEN ADD ANOTHER ARCHIVING ENTRY            00069900
070000***  FROM AFTER-IMAGE RECORD                                      00070000
070100                                                                  00070100
070200*MAM G&R - ADDED QUAL CTRL LOGIC                                  00070200
070300     INITIALIZE QCF-RECORD.                                       00070300
070400     MOVE ZEROS            TO QCF-PLAN-CODE                       00070400
070500                              QCF-GROUP-NO                        00070500
070600                              QCF-SECTION-NO                      00070600
070700                              QCF-PKG-CODE.                       00070700
070800     MOVE SPACES           TO QCF-FILLER.                         00070800
070900     MOVE 'C'              TO QCF-CONTRACT-GROUP-SP-IND.          00070900
071000     MOVE PARM-LOCATION    TO QCF-PLAN-CODE.                      00071000
071100     MOVE TAB-ARCH-DE-CD   TO QCF-FUNC-FIELD.                     00071100
071200     MOVE 'ADDED'          TO QCF-BIM-FIELD.                      00071200
071300     MOVE GCT2-CON-TAB-ID (GCT2-TAB-INDEX) TO QCF-AIM-FIELD.      00071300
071400                                                                  00071400
071500     MOVE GCT-GROUP-NUM OF BIM-RECORD TO QCF-GROUP-NO.            00071500
071600     MOVE GCT-SECTION-NUM OF BIM-RECORD TO QCF-SECTION-NO.        00071600
071700     MOVE GCT-PKG-CODE OF BIM-RECORD TO QCF-PKG-CODE              00071700
071800     MOVE GCT-L-O-B    OF BIM-RECORD TO QCF-L-O-B.                00071800
071900     MOVE GCT-PROVDR-CONTROL OF BIM-RECORD TO QCF-PROV-CTRL.      00071900
072000     MOVE GCT-FAM-REL-LVL    OF BIM-RECORD TO QCF-FAM-REL-LEVEL.  00072000
072100     MOVE WRK-OPERATOR-ID                  TO QCF-OPERATOR-ID.    00072100
072200                                                                  00072200
072300     IF TODAYS-DATE < 9999999                                     00072300
072400        MOVE TODAYS-DATE   TO QCF-FUNC-DATE.                      00072400
072500                                                                  00072500
072600     IF GCT-EFFDT-CEN OF BIM-RECORD < 9999999                     00072600
072700        MOVE GCT-EFFDT-CEN OF BIM-RECORD TO QCF-EFF-DATE.         00072700
072800                                                                  00072800
072900*    MOVE GCT2-GRP-NO   OF RCL-RECORD TO QCF-SCND-PRT-GROUP-NO.   00072900
073000*    MOVE GCT2-SECTN-NO OF RCL-RECORD TO QCF-SCND-PRT-SECTION-NO. 00073000
073100*    MOVE GCT2-L-O-B    OF RCL-RECORD TO QCF-L-O-B.               00073100
073200*    MOVE GCT2-PROVDR-CONTROL OF RCL-RECORD TO QCF-PROV-CTRL.     00073200
073300*    MOVE GCT2-FAM-REL-LVL    OF RCL-RECORD TO QCF-FAM-REL-LEVEL. 00073300
073400*    MOVE WRK2-OPERATOR-ID    OF RCL-RECORD TO QCF-OPERATOR-ID.   00073400
073500                                                                  00073500
073600*    IF TODAYS-DATE < 99999                                       00073600
073700*       COMPUTE                                                   00073700
073800*         QCF-FUNC-DATE = +1900000 + TODAYS-DATE.                 00073800
073900                                                                  00073900
074000*    IF GCT2-EFF-DT OF RCL-RECORD  < 99999                        00074000
074100*       COMPUTE                                                   00074100
074200*         QCF-EFF-DATE = +1900000 + GCT2-EFF-DT OF RCL-RECORD.    00074200
074300                                                                  00074300
074400     IF GCT2-INTER-REL-CD = ZEROS                                 00074400
074500        MOVE 'N' TO WS-INTER-REL-SW                               00074500
074600        ADD +1   TO WS-BYPASS-CNT                                 00074600
074700     ELSE                                                         00074700
074800        MOVE 'Y' TO WS-INTER-REL-SW.                              00074800
074900                                                                  00074900
075000     IF WRK-OPERATOR-ID > SPACES AND                              00075000
075100        INTER-REL-CD-FND                                          00075100
075200        PERFORM 0084-WRITE-QUAL-CTRL THRU 0084-EXIT.              00075200
075300                                                                  00075300
075400     MOVE PNTRS-COUNT-A  TO ARCH-POINTERS-COUNT.                  00075400
075500     MOVE REC-AREA-A     TO ARCHIVED-GCPS-RECORD.                 00075500
075600                                                                  00075600
075700     ADD +1                   TO ARCH-POINTERS-COUNT.             00075700
075800     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00075800
075900         GO TO 003030-EXIT.                                       00075900
076000     SET ARCH-INDEX           TO ARCH-POINTERS-COUNT.             00076000
076100     MOVE  '++ADDED++'        TO ARCH-FLD-VALUE (ARCH-INDEX).     00076100
076200     MOVE WRK2-OPERATOR-ID    TO ARCH-OPER-ID (ARCH-INDEX).       00076200
076300                                                                  00076300
076400     IF WRK2-CDE-SP NOT = '2 '                                    00076400
076500        MOVE 'X'   TO ARCH-CDE-IND (ARCH-INDEX)                   00076500
076600     ELSE                                                         00076600
076700        MOVE ' '   TO ARCH-CDE-IND (ARCH-INDEX).                  00076700
076800                                                                  00076800
076900     MOVE TODAYS-DATE         TO ARCH-CHNG-DT-CEN (ARCH-INDEX).   00076900
077000     MOVE SPACES              TO ARCH-ANLS-CD (ARCH-INDEX).       00077000
077100                                                                  00077100
077200     IF WRK2-ATB3-REQUEST                                         00077200
077300        MOVE WRK2-SIGNAL-FROM-ONLINE TO ARCH-ATB-IND (ARCH-INDEX) 00077300
077400     ELSE                                                         00077400
077500        MOVE WRK2-TYPE-MAINT-IND     TO ARCH-ATB-IND (ARCH-INDEX).00077500
077600                                                                  00077600
077700                                                                  00077700
077800     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00077800
077900     ADD +1  TO ARCH-ENTRS-INSERT.                                00077900
078000                                                                  00078000
078100 003030-EXIT.                                                     00078100
078200     EXIT.                                                        00078200
078300/                                                                 00078300
078400 003040-LOAD-EQ-RTN.                                              00078400
078500                                                                  00078500
078600*** IF BIM TABULAR SLOT NOT EQUAL AIM TABULAR SLOT THEN CREATE    00078600
078700*** AN ARCHIVED RECORD (OR ENTRY IF RECORD ALREADY EXISTED) FROM  00078700
078800*** THE BIM RECORD.                                               00078800
078900                                                                  00078900
079000     IF GCT-CON-TAB-SLOT (GCT-TAB-INDEX)  EQUAL                   00079000
079100        GCT2-CON-TAB-SLOT (GCT2-TAB-INDEX)                        00079100
079200          GO TO  003040-EXIT.                                     00079200
079300                                                                  00079300
079400     MOVE 'C'             TO KEY-TYPE-A.                          00079400
079500     MOVE B-CONTRACT-ID   TO KEY-ID-A.                            00079500
079600     MOVE GCT-CON-TAB-ID (GCT-TAB-INDEX)                          00079600
079700                          TO  TB-ARCH-DE-2.                       00079700
079800     MOVE TAB-ARCH-DE-CD  TO  KEY-NO-A.                           00079800
079900                                                                  00079900
080000     MOVE  'R'            TO REQUEST-TYPE-A.                      00080000
080100     MOVE  42             TO RECORD-LENGTH-A.                     00080100
080200     CALL   'TSGVSAM3'    USING PARM-ONE-A                        00080200
080300                                PARM-TWO-A.                       00080300
080400                                                                  00080400
080500***  RECORD NOT FOUND OR EOF THEN CREATE A NEW ARCHIVE RECORD     00080500
080600***  FROM BEFORE-IMAGE RECORD                                     00080600
080700                                                                  00080700
080800     IF  REQUEST-TYPE-A  = '2'  OR  '3'                           00080800
080900         PERFORM 0082-ADD-ARCH-TAB-RECORD THRU 0082-EXIT          00080900
081000         ADD +1  TO ARCH-RECS-BUILD                               00081000
081100         GO TO 003040-EXIT.                                       00081100
081200                                                                  00081200
081300***  RECORD IS FOUND, THEN ADD ANOTHER ARCHIVING ENTRY            00081300
081400***  FROM BEFORE-IMAGE RECORD                                     00081400
081500                                                                  00081500
081600*MAM G&R - ADDED QUAL CTRL LOGIC                                  00081600
081700     INITIALIZE QCF-RECORD.                                       00081700
081800     MOVE ZEROS            TO QCF-PLAN-CODE                       00081800
081900                              QCF-GROUP-NO                        00081900
082000                              QCF-SECTION-NO                      00082000
082100                              QCF-PKG-CODE.                       00082100
082200     MOVE SPACES           TO QCF-FILLER.                         00082200
082300     MOVE 'C'              TO QCF-CONTRACT-GROUP-SP-IND.          00082300
082400     MOVE PARM-LOCATION    TO QCF-PLAN-CODE.                      00082400
082500     MOVE TAB-ARCH-DE-CD   TO QCF-FUNC-FIELD.                     00082500
082600     MOVE GCT-CON-TAB-SLOT (GCT-TAB-INDEX)   TO QCF-BIM-FIELD.    00082600
082700     MOVE GCT2-CON-TAB-SLOT (GCT2-TAB-INDEX) TO QCF-AIM-FIELD.    00082700
082800                                                                  00082800
082900     MOVE GCT-GROUP-NUM OF BIM-RECORD TO QCF-GROUP-NO.            00082900
083000     MOVE GCT-SECTION-NUM OF BIM-RECORD TO QCF-SECTION-NO.        00083000
083100     MOVE GCT-PKG-CODE OF BIM-RECORD TO QCF-PKG-CODE              00083100
083200     MOVE GCT-L-O-B    OF BIM-RECORD TO QCF-L-O-B.                00083200
083300     MOVE GCT-PROVDR-CONTROL OF BIM-RECORD TO QCF-PROV-CTRL.      00083300
083400     MOVE GCT-FAM-REL-LVL    OF BIM-RECORD TO QCF-FAM-REL-LEVEL.  00083400
083500     MOVE WRK-OPERATOR-ID                  TO QCF-OPERATOR-ID.    00083500
083600                                                                  00083600
083700     IF TODAYS-DATE < 9999999                                     00083700
083800        MOVE TODAYS-DATE   TO QCF-FUNC-DATE.                      00083800
083900                                                                  00083900
084000     IF GCT-EFFDT-CEN OF BIM-RECORD  < 9999999                    00084000
084100        MOVE GCT-EFFDT-CEN OF BIM-RECORD TO QCF-EFF-DATE.         00084100
084200                                                                  00084200
084300*    MOVE GCT-GRP-NO   OF BIM-RECORD TO QCF-SCND-PRT-GROUP-NO.    00084300
084400*    MOVE GCT-SECTN-NO OF BIM-RECORD TO QCF-SCND-PRT-SECTION-NO.  00084400
084500*    MOVE GCT-L-O-B    OF BIM-RECORD TO QCF-L-O-B.                00084500
084600*    MOVE GCT-PROVDR-CONTROL OF BIM-RECORD TO QCF-PROV-CTRL.      00084600
084700*    MOVE GCT-FAM-REL-LVL    OF BIM-RECORD TO QCF-FAM-REL-LEVEL.  00084700
084800*    MOVE WRK-OPERATOR-ID                  TO QCF-OPERATOR-ID.    00084800
084900                                                                  00084900
085000*    IF TODAYS-DATE < 99999                                       00085000
085100*       COMPUTE                                                   00085100
085200*         QCF-FUNC-DATE = +1900000 + TODAYS-DATE.                 00085200
085300                                                                  00085300
085400*    IF GCT-EFF-DT OF BIM-RECORD  < 99999                         00085400
085500*       COMPUTE                                                   00085500
085600*         QCF-EFF-DATE = +1900000 + GCT-EFF-DT OF BIM-RECORD.     00085600
085700                                                                  00085700
085800     IF GCT2-INTER-REL-CD = ZEROS                                 00085800
085900        MOVE 'N' TO WS-INTER-REL-SW                               00085900
086000        ADD +1   TO WS-BYPASS-CNT                                 00086000
086100     ELSE                                                         00086100
086200        MOVE 'Y' TO WS-INTER-REL-SW.                              00086200
086300                                                                  00086300
086400     IF WRK-OPERATOR-ID > SPACES AND                              00086400
086500        INTER-REL-CD-FND                                          00086500
086600        PERFORM 0084-WRITE-QUAL-CTRL THRU 0084-EXIT.              00086600
086700                                                                  00086700
086800     MOVE PNTRS-COUNT-A  TO ARCH-POINTERS-COUNT.                  00086800
086900     MOVE REC-AREA-A     TO ARCHIVED-GCPS-RECORD.                 00086900
087000                                                                  00087000
087100     ADD +1                  TO ARCH-POINTERS-COUNT.              00087100
087200     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00087200
087300         GO TO 003040-EXIT.                                       00087300
087400     SET ARCH-INDEX          TO ARCH-POINTERS-COUNT.              00087400
087500     MOVE GCT-CON-TAB-SLOT (GCT-TAB-INDEX)                        00087500
087600                             TO ARCH-FLD-VALUE (ARCH-INDEX).      00087600
087700     MOVE WRK-OPERATOR-ID    TO ARCH-OPER-ID (ARCH-INDEX).        00087700
087800                                                                  00087800
087900     IF WRK-CDE-SP NOT = '2 '                                     00087900
088000         MOVE 'X'  TO ARCH-CDE-IND (ARCH-INDEX)                   00088000
088100     ELSE                                                         00088100
088200         MOVE ' '  TO ARCH-CDE-IND (ARCH-INDEX).                  00088200
088300                                                                  00088300
088400     MOVE TODAYS-DATE         TO ARCH-CHNG-DT-CEN (ARCH-INDEX).   00088400
088500     MOVE SPACES              TO ARCH-ANLS-CD (ARCH-INDEX).       00088500
088600                                                                  00088600
088700     IF WRK-ATB3-REQUEST                                          00088700
088800        MOVE WRK-SIGNAL-FROM-ONLINE TO ARCH-ATB-IND (ARCH-INDEX)  00088800
088900     ELSE                                                         00088900
089000        MOVE WRK-TYPE-MAINT-IND     TO ARCH-ATB-IND (ARCH-INDEX). 00089000
089100                                                                  00089100
089200     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00089200
089300     ADD +1  TO ARCH-ENTRS-INSERT.                                00089300
089400                                                                  00089400
089500 003040-EXIT.                                                     00089500
089600     EXIT.                                                        00089600
089700/                                                                 00089700
089800 0040-BEN-POINTERS-RTN.                                           00089800
089900                                                                  00089900
090000*** AT THE END OF BENEFITS IDS SEARCH, PUT THE END SWITCH ON      00090000
090100                                                                  00090100
090200     IF GCT2-BEN-PROVN-ID (GCT2-INDEX) EQUAL HIGH-VALUES AND      00090200
090300        GCT-BEN-PROVN-ID (GCT-INDEX) EQUAL HIGH-VALUES            00090300
090400          MOVE 'END'   TO BEN-POINTER-SW                          00090400
090500          GO TO 0040-EXIT.                                        00090500
090600                                                                  00090600
090700                                                                  00090700
090800*** IF AFTER-IMAGE BEN ID IS GREATER THAN BEFORE-IMAGE BEN ID, THE00090800
090900*** THAT BEN ID HAS BEEN DELETED FROM THE CONT RECORD. A RECORD   00090900
091000*** ON THE ARCHIVED FILE (OR AN ENTRY) WILL BE CREATED FROM BEFORE00091000
091100*** IMAGE FILE (AIM DOES NOT HAVE THE BEN ID ANYMORE).            00091100
091200                                                                  00091200
091300     IF GCT2-BEN-PROVN-ID (GCT2-INDEX) GREATER THAN               00091300
091400        GCT-BEN-PROVN-ID (GCT-INDEX)                              00091400
091500          PERFORM 004020-LOAD-DEL-RTN THRU 004020-EXIT            00091500
091600          SET GCT-INDEX UP BY 1                                   00091600
091700          SET GCT2-INDEX DOWN BY 1                                00091700
091800          GO TO 0040-EXIT.                                        00091800
091900                                                                  00091900
092000*** IF AFTER-IMAGE BEN ID IS LESS THAN BEFORE-IMAGE BEN ID, THEN  00092000
092100*** THAT BEN ID HAS BEEN ADDED TO THE CONTRACT RECORD. A RECORD   00092100
092200*** ON THE ARCHIVED FILE (OR AN ENTRY) WILL BE CREATED FROM AFTER 00092200
092300*** IMAGE FILE (WHERE THE BEN ID JUST BEEN ADDED) BUT THE SLOT    00092300
092400*** NUMBER WILL BE 'ADDED' (DE VALUE) INDICATING THAT THE BENEFIT 00092400
092500*** HAS JUST BEEN ADDED.                                          00092500
092600                                                                  00092600
092700     IF GCT2-BEN-PROVN-ID (GCT2-INDEX) LESS THAN                  00092700
092800        GCT-BEN-PROVN-ID (GCT-INDEX)                              00092800
092900          PERFORM 004030-LOAD-ADD-RTN THRU 004030-EXIT            00092900
093000          GO TO 0040-EXIT.                                        00093000
093100                                                                  00093100
093200*** IF AFTER-IMAGE BEN ID EQUAL BEFORE-IMAGE BEN ID THEN, IF SLOT 00093200
093300*** NUMBER NOT EQUAL ON BOTH BENEFITS, THEN CREATE AN ARCHIVED REC00093300
093400*** (OR ENTRY) ON THE ARCHIVED FILE.                              00093400
093500                                                                  00093500
093600     IF GCT2-BEN-PROVN-ID (GCT2-INDEX) EQUAL                      00093600
093700        GCT-BEN-PROVN-ID (GCT-INDEX)                              00093700
093800          PERFORM 004040-LOAD-EQ-RTN THRU 004040-EXIT             00093800
093900          SET GCT-INDEX UP BY 1                                   00093900
094000          GO TO 0040-EXIT.                                        00094000
094100                                                                  00094100
094200 0040-EXIT.                                                       00094200
094300     EXIT.                                                        00094300
094400/                                                                 00094400
094500 004020-LOAD-DEL-RTN.                                             00094500
094600                                                                  00094600
094700     IF GCT-BEN-PROVN-ID (GCT-INDEX) = HIGH-VALUES                00094700
094800         GO TO  004020-EXIT.                                      00094800
094900                                                                  00094900
095000     MOVE 'C'              TO KEY-TYPE-A.                         00095000
095100     MOVE B-CONTRACT-ID    TO KEY-ID-A.                           00095100
095200     MOVE GCT-BEN-PROVN-ID (GCT-INDEX)                            00095200
095300                           TO BN-ARCH-DE-2.                       00095300
095400     MOVE BEN-ARCH-DE-CD   TO KEY-NO-A.                           00095400
095500                                                                  00095500
095600     MOVE  'R'             TO REQUEST-TYPE-A.                     00095600
095700     MOVE  42              TO RECORD-LENGTH-A.                    00095700
095800     CALL 'TSGVSAM3'       USING PARM-ONE-A                       00095800
095900                                 PARM-TWO-A.                      00095900
096000                                                                  00096000
096100***  RECORD NOT FOUND OR EOF THEN CREATE A NEW ARCHIVE RECORD     00096100
096200***  FROM BEFORE-IMAGE RECORD                                     00096200
096300                                                                  00096300
096400     IF  REQUEST-TYPE-A  = '2'  OR  '3'                           00096400
096500         PERFORM 0087-ADD-ARCH-BEN-RECORD THRU 0087-EXIT          00096500
096600         ADD +1  TO ARCH-RECS-BUILD                               00096600
096700         GO TO 004020-EXIT.                                       00096700
096800                                                                  00096800
096900     IF  REQUEST-TYPE-A NOT EQUAL 'R'                             00096900
097000         MOVE FEEDBACK-CODE-A    TO ABEND-CODE                    00097000
097100         GO TO 9999-ERROR-RTN.                                    00097100
097200                                                                  00097200
097300***  RECORD IS FOUND, THEN ADD ANOTHER ARCHIVING ENTRY            00097300
097400***  FROM BEFORE-IMAGE RECORD                                     00097400
097500                                                                  00097500
097600*MAM G&R - ADDED QUAL CTRL LOGIC                                  00097600
097700     INITIALIZE QCF-RECORD.                                       00097700
097800     MOVE ZEROS            TO QCF-PLAN-CODE                       00097800
097900                              QCF-GROUP-NO                        00097900
098000                              QCF-SECTION-NO                      00098000
098100                              QCF-PKG-CODE.                       00098100
098200     MOVE SPACES           TO QCF-FILLER.                         00098200
098300     MOVE 'C'              TO QCF-CONTRACT-GROUP-SP-IND.          00098300
098400     MOVE PARM-LOCATION    TO QCF-PLAN-CODE.                      00098400
098500     MOVE BEN-ARCH-DE-CD   TO QCF-FUNC-FIELD.                     00098500
098600     MOVE GCT-BEN-PROVN-ID (GCT-INDEX)                            00098600
098700                           TO QCF-BIM-FIELD                       00098700
098800     MOVE 'DELETED'        TO QCF-AIM-FIELD                       00098800
098900                                                                  00098900
099000     MOVE GCT-GROUP-NUM OF BIM-RECORD TO QCF-GROUP-NO.            00099000
099100     MOVE GCT-SECTION-NUM OF BIM-RECORD TO QCF-SECTION-NO.        00099100
099200     MOVE GCT-PKG-CODE OF BIM-RECORD TO QCF-PKG-CODE              00099200
099300     MOVE GCT-L-O-B    OF BIM-RECORD TO QCF-L-O-B.                00099300
099400     MOVE GCT-PROVDR-CONTROL OF BIM-RECORD TO QCF-PROV-CTRL.      00099400
099500     MOVE GCT-FAM-REL-LVL    OF BIM-RECORD TO QCF-FAM-REL-LEVEL.  00099500
099600     MOVE WRK-OPERATOR-ID                  TO QCF-OPERATOR-ID.    00099600
099700                                                                  00099700
099800     IF TODAYS-DATE < 9999999                                     00099800
099900        MOVE TODAYS-DATE   TO QCF-FUNC-DATE.                      00099900
100000                                                                  00100000
100100     IF GCT-EFFDT-CEN OF BIM-RECORD  < 9999999                    00100100
100200        MOVE GCT-EFFDT-CEN OF BIM-RECORD TO QCF-EFF-DATE.         00100200
100300                                                                  00100300
100400*    MOVE GCT-GRP-NO   OF BIM-RECORD TO QCF-SCND-PRT-GROUP-NO.    00100400
100500*    MOVE GCT-SECTN-NO OF BIM-RECORD TO QCF-SCND-PRT-SECTION-NO.  00100500
100600*    MOVE GCT-L-O-B    OF BIM-RECORD TO QCF-L-O-B.                00100600
100700*    MOVE GCT-PROVDR-CONTROL OF BIM-RECORD TO QCF-PROV-CTRL.      00100700
100800*    MOVE GCT-FAM-REL-LVL    OF BIM-RECORD TO QCF-FAM-REL-LEVEL.  00100800
100900*    MOVE WRK-OPERATOR-ID                  TO QCF-OPERATOR-ID.    00100900
101000                                                                  00101000
101100*    IF TODAYS-DATE < 99999                                       00101100
101200*       COMPUTE                                                   00101200
101300*         QCF-FUNC-DATE = +1900000 + TODAYS-DATE.                 00101300
101400                                                                  00101400
101500*    IF GCT-EFF-DT OF BIM-RECORD  < 99999                         00101500
101600*       COMPUTE                                                   00101600
101700*         QCF-EFF-DATE = +1900000 + GCT-EFF-DT OF BIM-RECORD.     00101700
101800                                                                  00101800
101900     IF GCT2-INTER-REL-CD = ZEROS                                 00101900
102000        MOVE 'N' TO WS-INTER-REL-SW                               00102000
102100        ADD +1   TO WS-BYPASS-CNT                                 00102100
102200     ELSE                                                         00102200
102300        MOVE 'Y' TO WS-INTER-REL-SW.                              00102300
102400                                                                  00102400
102500     IF WRK-OPERATOR-ID > SPACES AND                              00102500
102600        INTER-REL-CD-FND                                          00102600
102700        PERFORM 0084-WRITE-QUAL-CTRL THRU 0084-EXIT.              00102700
102800                                                                  00102800
102900     MOVE PNTRS-COUNT-A  TO ARCH-POINTERS-COUNT.                  00102900
103000     MOVE REC-AREA-A     TO ARCHIVED-GCPS-RECORD.                 00103000
103100                                                                  00103100
103200     ADD +1                TO ARCH-POINTERS-COUNT.                00103200
103300     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00103300
103400         GO TO 004020-EXIT.                                       00103400
103500     SET ARCH-INDEX        TO ARCH-POINTERS-COUNT.                00103500
103600     MOVE GCT-BEN-PROVN-SLOT-NO (GCT-INDEX)                       00103600
103700                           TO ARCH-FLD-VALUE (ARCH-INDEX).        00103700
103800     MOVE WRK-OPERATOR-ID  TO ARCH-OPER-ID (ARCH-INDEX).          00103800
103900                                                                  00103900
104000     IF WRK-CDE-SP NOT = '2 '                                     00104000
104100        MOVE 'X'  TO ARCH-CDE-IND (ARCH-INDEX)                    00104100
104200     ELSE                                                         00104200
104300        MOVE ' '  TO ARCH-CDE-IND (ARCH-INDEX).                   00104300
104400                                                                  00104400
104500     MOVE TODAYS-DATE      TO ARCH-CHNG-DT-CEN (ARCH-INDEX).      00104500
104600     MOVE SPACES           TO ARCH-ANLS-CD (ARCH-INDEX).          00104600
104700                                                                  00104700
104800     IF WRK-ATB3-REQUEST                                          00104800
104900        MOVE WRK-SIGNAL-FROM-ONLINE TO ARCH-ATB-IND (ARCH-INDEX)  00104900
105000     ELSE                                                         00105000
105100        MOVE WRK-TYPE-MAINT-IND     TO ARCH-ATB-IND (ARCH-INDEX). 00105100
105200                                                                  00105200
105300     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00105300
105400     ADD +1  TO ARCH-ENTRS-INSERT.                                00105400
105500                                                                  00105500
105600 004020-EXIT.                                                     00105600
105700     EXIT.                                                        00105700
105800/                                                                 00105800
105900 004030-LOAD-ADD-RTN.                                             00105900
106000                                                                  00106000
106100     IF GCT2-BEN-PROVN-ID (GCT2-INDEX) = HIGH-VALUES              00106100
106200                          GO TO  004030-EXIT.                     00106200
106300                                                                  00106300
106400     MOVE 'C'             TO KEY-TYPE-A.                          00106400
106500     MOVE B-CONTRACT-ID   TO KEY-ID-A.                            00106500
106600     MOVE GCT2-BEN-PROVN-ID (GCT2-INDEX)                          00106600
106700                          TO BN-ARCH-DE-2.                        00106700
106800     MOVE BEN-ARCH-DE-CD  TO  KEY-NO-A.                           00106800
106900                                                                  00106900
107000     MOVE  'R'    TO REQUEST-TYPE-A.                              00107000
107100     MOVE  42     TO RECORD-LENGTH-A.                             00107100
107200     CALL   'TSGVSAM3'  USING PARM-ONE-A PARM-TWO-A.              00107200
107300                                                                  00107300
107400***  RECORD NOT FOUND OR EOF THEN CREATE A NEW ARCHIVE RECORD     00107400
107500***  FROM THE AFTER-IMAGE RECORD                                  00107500
107600     IF  REQUEST-TYPE-A  = '2'  OR  '3'                           00107600
107700         PERFORM 0086-ADD-ARCH-BEN-RECORD THRU 0086-EXIT          00107700
107800         ADD +1  TO ARCH-RECS-BUILD                               00107800
107900         GO TO 004030-EXIT.                                       00107900
108000                                                                  00108000
108100     IF  REQUEST-TYPE-A NOT EQUAL 'R'                             00108100
108200         MOVE FEEDBACK-CODE-A    TO ABEND-CODE                    00108200
108300         GO TO 9999-ERROR-RTN.                                    00108300
108400                                                                  00108400
108500***  RECORD IS FOUND, THEN ADD ANOTHER ARCHIVING ENTRY            00108500
108600***  FROM THE AFTER-IMAGE RECORD                                  00108600
108700                                                                  00108700
108800*MAM G&R - ADDED QUAL CTRL LOGIC                                  00108800
108900     INITIALIZE QCF-RECORD.                                       00108900
109000     MOVE ZEROS            TO QCF-PLAN-CODE                       00109000
109100                              QCF-GROUP-NO                        00109100
109200                              QCF-SECTION-NO                      00109200
109300                              QCF-PKG-CODE.                       00109300
109400     MOVE SPACES           TO QCF-FILLER.                         00109400
109500     MOVE 'C'              TO QCF-CONTRACT-GROUP-SP-IND.          00109500
109600     MOVE PARM-LOCATION    TO QCF-PLAN-CODE.                      00109600
109700     MOVE BEN-ARCH-DE-CD   TO QCF-FUNC-FIELD.                     00109700
109800     MOVE GCT2-BEN-PROVN-ID (GCT2-INDEX) TO QCF-AIM-FIELD.        00109800
109900     MOVE 'ADDED'          TO QCF-BIM-FIELD.                      00109900
110000                                                                  00110000
110100     MOVE GCT-GROUP-NUM OF BIM-RECORD TO QCF-GROUP-NO.            00110100
110200     MOVE GCT-SECTION-NUM OF BIM-RECORD TO QCF-SECTION-NO.        00110200
110300     MOVE GCT-PKG-CODE OF BIM-RECORD TO QCF-PKG-CODE              00110300
110400     MOVE GCT-L-O-B    OF BIM-RECORD TO QCF-L-O-B.                00110400
110500     MOVE GCT-PROVDR-CONTROL OF BIM-RECORD TO QCF-PROV-CTRL.      00110500
110600     MOVE GCT-FAM-REL-LVL    OF BIM-RECORD TO QCF-FAM-REL-LEVEL.  00110600
110700     MOVE WRK-OPERATOR-ID                  TO QCF-OPERATOR-ID.    00110700
110800                                                                  00110800
110900     IF TODAYS-DATE < 9999999                                     00110900
111000        MOVE TODAYS-DATE   TO QCF-FUNC-DATE.                      00111000
111100                                                                  00111100
111200     IF GCT-EFFDT-CEN OF BIM-RECORD  < 9999999                    00111200
111300        MOVE GCT-EFFDT-CEN OF BIM-RECORD TO QCF-EFF-DATE.         00111300
111400                                                                  00111400
111500*    MOVE GCT2-GRP-NO   OF RCL-RECORD TO QCF-SCND-PRT-GROUP-NO.   00111500
111600*    MOVE GCT2-SECTN-NO OF RCL-RECORD TO QCF-SCND-PRT-SECTION-NO. 00111600
111700*    MOVE GCT2-L-O-B    OF RCL-RECORD TO QCF-L-O-B.               00111700
111800*    MOVE GCT2-PROVDR-CONTROL OF RCL-RECORD TO QCF-PROV-CTRL.     00111800
111900*    MOVE GCT2-FAM-REL-LVL    OF RCL-RECORD TO QCF-FAM-REL-LEVEL. 00111900
112000*    MOVE WRK2-OPERATOR-ID    OF RCL-RECORD TO QCF-OPERATOR-ID.   00112000
112100                                                                  00112100
112200*    IF TODAYS-DATE < 99999                                       00112200
112300*       COMPUTE                                                   00112300
112400*         QCF-FUNC-DATE = +1900000 + TODAYS-DATE.                 00112400
112500                                                                  00112500
112600*    IF GCT2-EFF-DT OF RCL-RECORD  < 99999                        00112600
112700*       COMPUTE                                                   00112700
112800*        QCF-EFF-DATE = +1900000 + GCT2-EFF-DT OF RCL-RECORD.     00112800
112900                                                                  00112900
113000     IF GCT2-INTER-REL-CD = ZEROS                                 00113000
113100        MOVE 'N' TO WS-INTER-REL-SW                               00113100
113200        ADD +1   TO WS-BYPASS-CNT                                 00113200
113300     ELSE                                                         00113300
113400        MOVE 'Y' TO WS-INTER-REL-SW.                              00113400
113500                                                                  00113500
113600     IF WRK-OPERATOR-ID > SPACES AND                              00113600
113700        INTER-REL-CD-FND                                          00113700
113800        PERFORM 0084-WRITE-QUAL-CTRL THRU 0084-EXIT.              00113800
113900                                                                  00113900
114000     MOVE PNTRS-COUNT-A  TO ARCH-POINTERS-COUNT.                  00114000
114100     MOVE REC-AREA-A     TO ARCHIVED-GCPS-RECORD.                 00114100
114200                                                                  00114200
114300     ADD +1                 TO ARCH-POINTERS-COUNT.               00114300
114400     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00114400
114500         GO TO 004030-EXIT.                                       00114500
114600     SET ARCH-INDEX         TO ARCH-POINTERS-COUNT.               00114600
114700     MOVE  '++ADDED++'      TO  ARCH-FLD-VALUE (ARCH-INDEX).      00114700
114800     MOVE WRK2-OPERATOR-ID  TO ARCH-OPER-ID (ARCH-INDEX).         00114800
114900                                                                  00114900
115000     IF WRK2-CDE-SP NOT = '2 '                                    00115000
115100        MOVE 'X'  TO ARCH-CDE-IND (ARCH-INDEX)                    00115100
115200     ELSE                                                         00115200
115300        MOVE ' '  TO ARCH-CDE-IND (ARCH-INDEX).                   00115300
115400                                                                  00115400
115500     MOVE TODAYS-DATE         TO ARCH-CHNG-DT-CEN (ARCH-INDEX).   00115500
115600     MOVE SPACES              TO ARCH-ANLS-CD (ARCH-INDEX).       00115600
115700                                                                  00115700
115800     IF WRK2-ATB3-REQUEST                                         00115800
115900        MOVE WRK2-SIGNAL-FROM-ONLINE TO ARCH-ATB-IND (ARCH-INDEX) 00115900
116000     ELSE                                                         00116000
116100        MOVE WRK2-TYPE-MAINT-IND     TO ARCH-ATB-IND (ARCH-INDEX).00116100
116200                                                                  00116200
116300     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00116300
116400     ADD +1  TO ARCH-ENTRS-INSERT.                                00116400
116500                                                                  00116500
116600 004030-EXIT.                                                     00116600
116700     EXIT.                                                        00116700
116800/                                                                 00116800
116900 004040-LOAD-EQ-RTN.                                              00116900
117000                                                                  00117000
117100*** IF BIM BEN PRV SLOT NOT EQUAL AIM BEN PRV SLOT THEN CREATE    00117100
117200*** AN ARCHIVED RECORD (OR ENTRY IF RECORD ALREADY EXISTED).      00117200
117300                                                                  00117300
117400     IF GCT-BEN-PROVN-SLOT-NO (GCT-INDEX)  EQUAL                  00117400
117500        GCT2-BEN-PROVN-SLOT-NO (GCT2-INDEX)                       00117500
117600         GO TO  004040-EXIT.                                      00117600
117700                                                                  00117700
117800     MOVE 'C'             TO KEY-TYPE-A.                          00117800
117900     MOVE B-CONTRACT-ID   TO KEY-ID-A.                            00117900
118000     MOVE GCT-BEN-PROVN-ID (GCT-INDEX)                            00118000
118100                          TO BN-ARCH-DE-2.                        00118100
118200     MOVE BEN-ARCH-DE-CD  TO KEY-NO-A.                            00118200
118300                                                                  00118300
118400     MOVE  'R'            TO REQUEST-TYPE-A.                      00118400
118500     MOVE  42             TO RECORD-LENGTH-A.                     00118500
118600     CALL 'TSGVSAM3'      USING PARM-ONE-A PARM-TWO-A.            00118600
118700                                                                  00118700
118800***  RECORD NOT FOUND OR EOF THEN CREATE A NEW ARCHIVE RECORD     00118800
118900***  FROM BEFORE-IMAGE RECORD                                     00118900
119000                                                                  00119000
119100     IF  REQUEST-TYPE-A  = '2'  OR  '3'                           00119100
119200         PERFORM 0087-ADD-ARCH-BEN-RECORD THRU 0087-EXIT          00119200
119300         ADD +1  TO ARCH-RECS-BUILD                               00119300
119400         GO TO 004040-EXIT.                                       00119400
119500                                                                  00119500
119600***  RECORD IS FOUND, THEN ADD ANOTHER ARCHIVING ENTRY            00119600
119700***  FROM BEFORE-IMAGE RECORD                                     00119700
119800                                                                  00119800
119900*MAM G&R - ADDED QUAL CTRL LOGIC                                  00119900
120000     INITIALIZE QCF-RECORD.                                       00120000
120100     MOVE ZEROS            TO QCF-PLAN-CODE                       00120100
120200                              QCF-GROUP-NO                        00120200
120300                              QCF-SECTION-NO                      00120300
120400                              QCF-PKG-CODE.                       00120400
120500     MOVE SPACES           TO QCF-FILLER.                         00120500
120600     MOVE 'C'              TO QCF-CONTRACT-GROUP-SP-IND.          00120600
120700     MOVE PARM-LOCATION    TO QCF-PLAN-CODE.                      00120700
120800     MOVE BEN-ARCH-DE-CD   TO QCF-FUNC-FIELD.                     00120800
120900     MOVE GCT-BEN-PROVN-SLOT-NO (GCT-INDEX)   TO QCF-BIM-FIELD.   00120900
121000     MOVE GCT2-BEN-PROVN-SLOT-NO (GCT2-INDEX) TO QCF-AIM-FIELD.   00121000
121100                                                                  00121100
121200     MOVE GCT-GROUP-NUM OF BIM-RECORD TO QCF-GROUP-NO.            00121200
121300     MOVE GCT-SECTION-NUM OF BIM-RECORD TO QCF-SECTION-NO.        00121300
121400     MOVE GCT-PKG-CODE OF BIM-RECORD TO QCF-PKG-CODE              00121400
121500     MOVE GCT-L-O-B    OF BIM-RECORD TO QCF-L-O-B.                00121500
121600     MOVE GCT-PROVDR-CONTROL OF BIM-RECORD TO QCF-PROV-CTRL.      00121600
121700     MOVE GCT-FAM-REL-LVL    OF BIM-RECORD TO QCF-FAM-REL-LEVEL.  00121700
121800     MOVE WRK-OPERATOR-ID                  TO QCF-OPERATOR-ID.    00121800
121900                                                                  00121900
122000     IF TODAYS-DATE < 9999999                                     00122000
122100        MOVE TODAYS-DATE   TO QCF-FUNC-DATE.                      00122100
122200                                                                  00122200
122300     IF GCT-EFFDT-CEN OF BIM-RECORD  < 9999999                    00122300
122400        MOVE GCT-EFFDT-CEN OF BIM-RECORD TO QCF-EFF-DATE.         00122400
122500                                                                  00122500
122600*    MOVE GCT-GRP-NO   OF BIM-RECORD TO QCF-SCND-PRT-GROUP-NO.    00122600
122700*    MOVE GCT-SECTN-NO OF BIM-RECORD TO QCF-SCND-PRT-SECTION-NO.  00122700
122800*    MOVE GCT-L-O-B    OF BIM-RECORD TO QCF-L-O-B.                00122800
122900*    MOVE GCT-PROVDR-CONTROL OF BIM-RECORD TO QCF-PROV-CTRL.      00122900
123000*    MOVE GCT-FAM-REL-LVL    OF BIM-RECORD TO QCF-FAM-REL-LEVEL.  00123000
123100*    MOVE WRK-OPERATOR-ID                  TO QCF-OPERATOR-ID.    00123100
123200                                                                  00123200
123300*    IF TODAYS-DATE < 99999                                       00123300
123400*       COMPUTE                                                   00123400
123500*         QCF-FUNC-DATE = +1900000 + TODAYS-DATE.                 00123500
123600                                                                  00123600
123700*    IF GCT-EFF-DT OF BIM-RECORD  < 99999                         00123700
123800*       COMPUTE                                                   00123800
123900*        QCF-EFF-DATE = +1900000 + GCT-EFF-DT OF BIM-RECORD.      00123900
124000                                                                  00124000
124100     IF GCT2-INTER-REL-CD = ZEROS                                 00124100
124200        MOVE 'N' TO WS-INTER-REL-SW                               00124200
124300        ADD +1   TO WS-BYPASS-CNT                                 00124300
124400     ELSE                                                         00124400
124500        MOVE 'Y' TO WS-INTER-REL-SW.                              00124500
124600                                                                  00124600
124700     IF WRK-OPERATOR-ID > SPACES AND                              00124700
124800        INTER-REL-CD-FND                                          00124800
124900        PERFORM 0084-WRITE-QUAL-CTRL THRU 0084-EXIT.              00124900
125000                                                                  00125000
125100     MOVE PNTRS-COUNT-A    TO ARCH-POINTERS-COUNT.                00125100
125200     MOVE REC-AREA-A       TO ARCHIVED-GCPS-RECORD.               00125200
125300                                                                  00125300
125400     ADD +1                TO ARCH-POINTERS-COUNT.                00125400
125500     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00125500
125600         GO TO 004040-EXIT.                                       00125600
125700     SET ARCH-INDEX        TO ARCH-POINTERS-COUNT.                00125700
125800     MOVE GCT-BEN-PROVN-SLOT-NO (GCT-INDEX)                       00125800
125900                           TO ARCH-FLD-VALUE (ARCH-INDEX).        00125900
126000     MOVE WRK-OPERATOR-ID  TO ARCH-OPER-ID (ARCH-INDEX).          00126000
126100                                                                  00126100
126200     IF WRK-CDE-SP NOT = '2 '                                     00126200
126300        MOVE 'X'  TO ARCH-CDE-IND (ARCH-INDEX)                    00126300
126400     ELSE                                                         00126400
126500        MOVE ' '  TO ARCH-CDE-IND (ARCH-INDEX).                   00126500
126600                                                                  00126600
126700     MOVE TODAYS-DATE      TO ARCH-CHNG-DT-CEN (ARCH-INDEX).      00126700
126800     MOVE SPACES           TO ARCH-ANLS-CD (ARCH-INDEX).          00126800
126900                                                                  00126900
127000     IF WRK-ATB3-REQUEST                                          00127000
127100        MOVE WRK-SIGNAL-FROM-ONLINE TO ARCH-ATB-IND (ARCH-INDEX)  00127100
127200     ELSE                                                         00127200
127300        MOVE WRK-TYPE-MAINT-IND     TO ARCH-ATB-IND (ARCH-INDEX). 00127300
127400                                                                  00127400
127500     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00127500
127600     ADD  +1  TO ARCH-ENTRS-INSERT.                               00127600
127700                                                                  00127700
127800 004040-EXIT.                                                     00127800
127900     EXIT.                                                        00127900
128000/                                                                 00128000
128100 0070-PROCESS-ARCH-CONT.                                          00128100
128200**** RECORD-LEN = SEARCH-KEY-LEN = 38 + 4  ***                    00128200
128300                                                                  00128300
128400     MOVE 'C'                       TO KEY-TYPE-A.                00128400
128500     MOVE B-CONTRACT-ID             TO KEY-ID-A.                  00128500
128600     SET  GCT-A-INDEX               TO CON-A-INDX.                00128600
128700     MOVE GCT-A-DE-ID (GCT-A-INDEX) TO KEY-NO-A.                  00128700
128800     MOVE 'R'                       TO REQUEST-TYPE-A.            00128800
128900     MOVE 42                        TO RECORD-LENGTH-A.           00128900
129000     CALL 'TSGVSAM3'  USING PARM-ONE-A                            00129000
129100                            PARM-TWO-A.                           00129100
129200                                                                  00129200
129300***  RECORD NOT FOUND OR EOF THEN CREATE A NEW ARCHIVE RECORD     00129300
129400***  FROM BEFORE-IMAGE RECORD                                     00129400
129500                                                                  00129500
129600     IF  REQUEST-TYPE-A  = '2'  OR  '3'                           00129600
129700         PERFORM 0080-ADD-ARCH-CONT-RECORD THRU 0080-EXIT         00129700
129800         ADD  +1 TO ARCH-RECS-BUILD                               00129800
129900         GO TO 0070-EXIT.                                         00129900
130000                                                                  00130000
130100     IF  REQUEST-TYPE-A NOT EQUAL 'R'                             00130100
130200         MOVE FEEDBACK-CODE-A    TO ABEND-CODE                    00130200
130300         GO TO 9999-ERROR-RTN.                                    00130300
130400                                                                  00130400
130500***  RECORD IS FOUND, THEN ADD ANOTHER ARCHIVING ENTRY            00130500
130600                                                                  00130600
130700*MAM G&R - ADDED QUAL CTRL LOGIC                                  00130700
130800     INITIALIZE QCF-RECORD.                                       00130800
130900     MOVE ZEROS            TO QCF-PLAN-CODE                       00130900
131000                              QCF-GROUP-NO                        00131000
131100                              QCF-SECTION-NO                      00131100
131200                              QCF-PKG-CODE.                       00131200
131300     MOVE SPACES           TO QCF-FILLER.                         00131300
131400     MOVE 'C'              TO QCF-CONTRACT-GROUP-SP-IND.          00131400
131500     MOVE PARM-LOCATION    TO QCF-PLAN-CODE.                      00131500
131600                                                                  00131600
131700     MOVE GCT-GROUP-NUM OF BIM-RECORD TO QCF-GROUP-NO.            00131700
131800     MOVE GCT-SECTION-NUM OF BIM-RECORD TO QCF-SECTION-NO.        00131800
131900     MOVE GCT-PKG-CODE OF BIM-RECORD TO QCF-PKG-CODE              00131900
132000     MOVE GCT-L-O-B    OF BIM-RECORD TO QCF-L-O-B.                00132000
132100     MOVE GCT-PROVDR-CONTROL OF BIM-RECORD TO QCF-PROV-CTRL.      00132100
132200     MOVE GCT-FAM-REL-LVL    OF BIM-RECORD TO QCF-FAM-REL-LEVEL.  00132200
132300     MOVE WRK-OPERATOR-ID                  TO QCF-OPERATOR-ID.    00132300
132400                                                                  00132400
132500     IF TODAYS-DATE < 9999999                                     00132500
132600        MOVE TODAYS-DATE   TO QCF-FUNC-DATE.                      00132600
132700                                                                  00132700
132800     IF GCT-EFFDT-CEN OF BIM-RECORD  < 9999999                    00132800
132900        MOVE GCT-EFFDT-CEN OF BIM-RECORD TO QCF-EFF-DATE.         00132900
133000                                                                  00133000
133100*    MOVE GCT-GRP-NO   OF BIM-RECORD TO QCF-SCND-PRT-GROUP-NO.    00133100
133200*    MOVE GCT-SECTN-NO OF BIM-RECORD TO QCF-SCND-PRT-SECTION-NO.  00133200
133300*    MOVE GCT-L-O-B    OF BIM-RECORD TO QCF-L-O-B.                00133300
133400*    MOVE GCT-PROVDR-CONTROL OF BIM-RECORD TO QCF-PROV-CTRL.      00133400
133500*    MOVE GCT-FAM-REL-LVL    OF BIM-RECORD TO QCF-FAM-REL-LEVEL.  00133500
133600*    MOVE WRK-OPERATOR-ID                  TO QCF-OPERATOR-ID.    00133600
133700                                                                  00133700
133800     SET  GCT-A-INDEX                 TO CON-A-INDX.              00133800
133900     MOVE GCT-A-DE-ID (GCT-A-INDEX)   TO QCF-FUNC-FIELD.          00133900
134000     MOVE BEFORE-FIELD (CON-B-INDX)   TO QCF-BIM-FIELD.           00134000
134100     MOVE AFTER-FIELD (CON-A-INDX)    TO QCF-AIM-FIELD.           00134100
134200                                                                  00134200
134300*    IF TODAYS-DATE < 99999                                       00134300
134400*       COMPUTE                                                   00134400
134500*        QCF-FUNC-DATE = +1900000 + TODAYS-DATE.                  00134500
134600                                                                  00134600
134700*    IF GCT-EFF-DT OF BIM-RECORD  < 99999                         00134700
134800*       COMPUTE                                                   00134800
134900*        QCF-EFF-DATE = +1900000 + GCT-EFF-DT OF BIM-RECORD.      00134900
135000                                                                  00135000
135100     IF GCT2-INTER-REL-CD = ZEROS                                 00135100
135200        MOVE 'N' TO WS-INTER-REL-SW                               00135200
135300        ADD +1   TO WS-BYPASS-CNT                                 00135300
135400     ELSE                                                         00135400
135500        MOVE 'Y' TO WS-INTER-REL-SW.                              00135500
135600                                                                  00135600
135700     IF WRK-OPERATOR-ID > SPACES AND                              00135700
135800        INTER-REL-CD-FND                                          00135800
135900        PERFORM 0084-WRITE-QUAL-CTRL THRU 0084-EXIT.              00135900
136000                                                                  00136000
136100     MOVE PNTRS-COUNT-A    TO ARCH-POINTERS-COUNT.                00136100
136200     MOVE REC-AREA-A       TO ARCHIVED-GCPS-RECORD.               00136200
136300                                                                  00136300
136400     ADD +1                TO ARCH-POINTERS-COUNT.                00136400
136500     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00136500
136600         GO TO 0070-EXIT.                                         00136600
136700     SET ARCH-INDEX        TO ARCH-POINTERS-COUNT.                00136700
136800     MOVE GCT-A-DE-VALUE (GCT-A-INDEX)                            00136800
136900                           TO ARCH-FLD-VALUE (ARCH-INDEX).        00136900
137000     MOVE WRK-OPERATOR-ID  TO ARCH-OPER-ID (ARCH-INDEX).          00137000
137100                                                                  00137100
137200     IF WRK-CDE-SP NOT = '2 '                                     00137200
137300        MOVE 'X'  TO ARCH-CDE-IND (ARCH-INDEX)                    00137300
137400     ELSE                                                         00137400
137500        MOVE ' '  TO ARCH-CDE-IND (ARCH-INDEX).                   00137500
137600                                                                  00137600
137700     MOVE TODAYS-DATE         TO ARCH-CHNG-DT-CEN (ARCH-INDEX).   00137700
137800     MOVE SPACES              TO ARCH-ANLS-CD (ARCH-INDEX).       00137800
137900                                                                  00137900
138000     IF WRK-ATB3-REQUEST                                          00138000
138100        MOVE WRK-SIGNAL-FROM-ONLINE TO ARCH-ATB-IND (ARCH-INDEX)  00138100
138200     ELSE                                                         00138200
138300        MOVE WRK-TYPE-MAINT-IND     TO ARCH-ATB-IND (ARCH-INDEX). 00138300
138400                                                                  00138400
138500     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00138500
138600     ADD +1  TO ARCH-ENTRS-INSERT.                                00138600
138700                                                                  00138700
138800 0070-EXIT.                                                       00138800
138900      EXIT.                                                       00138900
139000/                                                                 00139000
139100 0071-PROCESS-ARCH-AUD.                                           00139100
139200**** RECORD-LEN = SEARCH-KEY-LEN = 38 + 4  ***                    00139200
139300*--- IS RECORD ON THE ARCHIVING FILE ?                            00139300
139400                                                                  00139400
139500     MOVE 'C'                 TO KEY-TYPE-A.                      00139500
139600     MOVE WRK3-PLAN-CODE      TO K-PLAN-CODE.                     00139600
139700     MOVE WRK3-GROUP-NUM      TO K-G-N.                           00139700
139800     MOVE WRK3-SECTION-NUM    TO K-S-N.                           00139800
139900     MOVE WRK3-PKG-CODE       TO K-PKG-CODE.                      00139900
140000     MOVE WRK3-L-O-B          TO K-LOB.                           00140000
140100     MOVE WRK3-PROV-CTL       TO K-P-C.                           00140100
140200     MOVE WRK3-FAM-REL-LEVEL  TO K-F-R.                           00140200
140300     MOVE WRK3-EFFDT-CEN      TO K-E-DT.                          00140300
140400                                                                  00140400
140500     MOVE  'R'    TO REQUEST-TYPE-A.                              00140500
140600     MOVE  42     TO RECORD-LENGTH-A.                             00140600
140700                                                                  00140700
140800     SET  GCAUD-TBL-INDEX  TO 1.                                  00140800
140900     SET  GCAUD-TBL-INDEX  DOWN  BY 1.                            00140900
141000                                                                  00141000
141100 0071-GET-FIRST-FUNC.                                             00141100
141200 0071-GET-NEXT-OCCUR.                                             00141200
141300     SET GCAUD-TBL-INDEX UP BY 1.                                 00141300
141400     IF  GCAUD-TBL-INDEX NOT >  GCAUD-TABLE-FLDS-OCCURS-CNT       00141400
141500         MOVE  'R'    TO REQUEST-TYPE-A                           00141500
141600         MOVE  42     TO RECORD-LENGTH-A                          00141600
141700     ELSE                                                         00141700
141800         GO  TO 0071-EXIT.                                        00141800
141900                                                                  00141900
142000     MOVE GCAUD-FUNC-TYPE (GCAUD-TBL-INDEX) TO  WS-AUD-FUNC.      00142000
142100     MOVE WS-AUD-ID  TO KEY-NO-A.                                 00142100
142200     CALL   'TSGVSAM3'  USING PARM-ONE-A PARM-TWO-A.              00142200
142300                                                                  00142300
142400***  RECORD NOT FOUND OR EOF THEN CREATE A NEW ARCHIVE RECORD     00142400
142500***  FROM THE FIRST FUNC TYPE, SEARCH FOR ANOTHER AUDIT OCCUR     00142500
142600***  WITH THE SAME FUNC TYPE AND ADDED TO THE ARCHIVE RECORD      00142600
142700***  JUST BEEN CREATED.                                           00142700
142800                                                                  00142800
142900     IF  REQUEST-TYPE-A  = '2'  OR  '3'                           00142900
143000         PERFORM 0079-ADD-ARCH-AUD-REC  THRU 0079-EXIT            00143000
143100         ADD +1  TO ARCH-RECS-BUILD                               00143100
143200         GO TO 0071-GET-NEXT-OCCUR.                               00143200
143300                                                                  00143300
143400***  RECORD IS FOUND, THEN ADD AN ARCHIVING ENTERY FOR EACH       00143400
143500***  OCCUR FOR THAT AUDIT FUNCTION TYPE                           00143500
143600                                                                  00143600
143700     IF  REQUEST-TYPE-A NOT EQUAL 'R'                             00143700
143800         MOVE FEEDBACK-CODE-A    TO ABEND-CODE                    00143800
143900         GO TO 9999-ERROR-RTN.                                    00143900
144000                                                                  00144000
144100     PERFORM 0078-ADD-ONE-ARCH-AUD   THRU  0078-EXIT.             00144100
144200     ADD +1  TO ARCH-ENTRS-INSERT.                                00144200
144300     GO TO 0071-GET-NEXT-OCCUR.                                   00144300
144400                                                                  00144400
144500 0071-EXIT.                                                       00144500
144600     EXIT.                                                        00144600
144700/                                                                 00144700
144800 0078-ADD-ONE-ARCH-AUD.                                           00144800
144900                                                                  00144900
145000*MAM G&R - ADDED PERFORM STATEMENT                                00145000
145100*          FORMATTED EXISTING STATEMENTS                          00145100
145200*    PERFORM 007910-FRMT-QUAL-CTRL-FILE  THRU 007910-EXIT.        00145200
145300                                                                  00145300
145400     MOVE PNTRS-COUNT-A    TO ARCH-POINTERS-COUNT.                00145400
145500     MOVE REC-AREA-A       TO ARCHIVED-GCPS-RECORD.               00145500
145600                                                                  00145600
145700     ADD +1  TO ARCH-POINTERS-COUNT.                              00145700
145800     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00145800
145900         GO TO 0078-EXIT.                                         00145900
146000     SET ARCH-INDEX  TO ARCH-POINTERS-COUNT.                      00146000
146100     MOVE GCAUD-DEPT-NO (GCAUD-TBL-INDEX) TO                      00146100
146200           ARCH-FLD-VALUE (ARCH-INDEX).                           00146200
146300     MOVE GCAUD-OPER-ID (GCAUD-TBL-INDEX) TO                      00146300
146400           ARCH-OPER-ID (ARCH-INDEX).                             00146400
146500     MOVE GCAUD-NONCDE-CDE-IND (GCAUD-TBL-INDEX) TO               00146500
146600           ARCH-CDE-IND (ARCH-INDEX).                             00146600
146700     MOVE GCAUD-ANLST-INIT (GCAUD-TBL-INDEX) TO                   00146700
146800           ARCH-ANLS-CD (ARCH-INDEX).                             00146800
146900     MOVE GCAUD-FUNCDT-CEN (GCAUD-TBL-INDEX) TO                   00146900
147000           ARCH-CHNG-DT-CEN (ARCH-INDEX).                         00147000
147100     MOVE GCAUD-ATB-IND (GCAUD-TBL-INDEX) TO                      00147100
147200           ARCH-ATB-IND (ARCH-INDEX).                             00147200
147300                                                                  00147300
147400     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00147400
147500                                                                  00147500
147600 0078-EXIT.                                                       00147600
147700     EXIT.                                                        00147700
147800/                                                                 00147800
147900 0079-ADD-ARCH-AUD-REC.                                           00147900
148000                                                                  00148000
148100*MAM G&R - ADDED PERFORM STATEMENT                                00148100
148200*          FORMATTED EXISTING STATEMENTS                          00148200
148300*    PERFORM 007910-FRMT-QUAL-CTRL-FILE  THRU 007910-EXIT.        00148300
148400                                                                  00148400
148500     MOVE ZEROS               TO ARCH-EFFDT-CEN                   00148500
148600                                 ARCH-POINTERS-COUNT.             00148600
148700     MOVE  'C'                TO ARCH-STA-CD.                     00148700
148800     MOVE WRK3-PLAN-CODE      TO ARCH-PLAN-CODE.                  00148800
148900     MOVE WRK3-GROUP-NUM      TO ARCH-GROUP-NUM.                  00148900
149000     MOVE WRK3-SECTION-NUM    TO ARCH-SECTION-NUM.                00149000
149100     MOVE WRK3-PKG-CODE       TO ARCH-PKG-CODE.                   00149100
149200     MOVE WRK3-L-O-B          TO ARCH-LOB.                        00149200
149300     MOVE WRK3-PROV-CTL       TO ARCH-PRV-CTL.                    00149300
149400     MOVE WRK3-FAM-REL-LEVEL  TO ARCH-FAM-RL.                     00149400
149500     MOVE WRK3-EFFDT-CEN      TO ARCH-EFFDT-CEN.                  00149500
149600     MOVE GCAUD-FUNC-TYPE (GCAUD-TBL-INDEX)                       00149600
149700                              TO  WS-AUD-FUNC.                    00149700
149800     MOVE WS-AUD-ID           TO ARCH-ID-CD.                      00149800
149900                                                                  00149900
150000     ADD +1  TO ARCH-POINTERS-COUNT.                              00150000
150100     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00150100
150200         GO TO 0079-EXIT.                                         00150200
150300     SET ARCH-INDEX  TO ARCH-POINTERS-COUNT.                      00150300
150400     MOVE GCAUD-DEPT-NO (GCAUD-TBL-INDEX)    TO                   00150400
150500                               ARCH-FLD-VALUE (ARCH-INDEX).       00150500
150600     MOVE GCAUD-OPER-ID (GCAUD-TBL-INDEX)    TO                   00150600
150700                               ARCH-OPER-ID (ARCH-INDEX).         00150700
150800     MOVE GCAUD-NONCDE-CDE-IND (GCAUD-TBL-INDEX)  TO              00150800
150900                               ARCH-CDE-IND (ARCH-INDEX).         00150900
151000     MOVE GCAUD-ANLST-INIT (GCAUD-TBL-INDEX) TO                   00151000
151100                               ARCH-ANLS-CD (ARCH-INDEX).         00151100
151200     MOVE GCAUD-FUNCDT-CEN (GCAUD-TBL-INDEX)  TO                  00151200
151300                               ARCH-CHNG-DT-CEN (ARCH-INDEX).     00151300
151400     MOVE GCAUD-ATB-IND (GCAUD-TBL-INDEX)    TO                   00151400
151500                               ARCH-ATB-IND (ARCH-INDEX).         00151500
151600                                                                  00151600
151700     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00151700
151800 0079-EXIT.                                                       00151800
151900     EXIT.                                                        00151900
152000/                                                                 00152000
152100*MAM G&R - ADDED PARAGRAPH TO FORMAT QUAL CTRL FILE               00152100
152200*007910-FRMT-QUAL-CTRL-FILE.                                      00152200
152300*                                                                 00152300
152400*    INITIALIZE QCF-RECORD.                                       00152400
152500*    MOVE ZEROS               TO QCF-FRST-PRT-GROUP-NO            00152500
152600*                                QCF-FRST-PRT-SECTION-NO.         00152600
152700*    MOVE SPACES              TO QCF-FILLER.                      00152700
152800*                                QCF-ANLS-INIT                    00152800
152900*                                QCF-DEPT-NUM.                    00152900
153000*    MOVE 'C'                 TO QCF-CONTRACT-GROUP-SP-IND.       00153000
153100*    MOVE WRK3-GROUP-NO       TO QCF-SCND-PRT-GROUP-NO.           00153100
153200*    MOVE WRK3-SECT-NO        TO QCF-SCND-PRT-SECTION-NO.         00153200
153300*    MOVE WRK3-L-O-B          TO QCF-L-O-B.                       00153300
153400*    MOVE WRK3-PROV-CTL       TO QCF-PROV-CTRL.                   00153400
153500*    MOVE WRK3-FAM-REL-LEVEL  TO QCF-FAM-REL-LEVEL.               00153500
153600*    MOVE PARM-LOCATION       TO QCF-PLAN-CODE.                   00153600
153700*                                                                 00153700
153800*    IF WRK3-EFF-DATE < 99999                                     00153800
153900*       COMPUTE QCF-EFF-DATE = +1900000 + WRK3-EFF-DATE.          00153900
154000*                                                                 00154000
154100*    MOVE GCAUD-FUNC-TYPE (GCAUD-TBL-INDEX)  TO QCF-FUNC-FIELD.   00154100
154200*    MOVE GCAUD-DEPT-NO (GCAUD-TBL-INDEX)    TO QCF-DEPT-NUM.     00154200
154300*    MOVE GCAUD-OPER-ID (GCAUD-TBL-INDEX)    TO QCF-OPERATOR-ID.  00154300
154400*    MOVE GCAUD-ANLST-INIT (GCAUD-TBL-INDEX) TO QCF-ANLS-INIT.    00154400
154500*                                                                 00154500
154600*    IF GCAUD-FUNC-DATE (GCAUD-TBL-INDEX) < 99999                 00154600
154700*       COMPUTE QCF-FUNC-DATE =                                   00154700
154800*              +1900000 + GCAUD-FUNC-DATE (GCAUD-TBL-INDEX).      00154800
154900*                                                                 00154900
155000*    PERFORM 0084-WRITE-QUAL-CTRL THRU 0084-EXIT.                 00155000
155100*007910-EXIT.                                                     00155100
155200*    EXIT.                                                        00155200
155300                                                                  00155300
155400 0080-ADD-ARCH-CONT-RECORD.                                       00155400
155500*MAM G&R - ADDED QUAL CNTRL LOGIC                                 00155500
155600                                                                  00155600
155700     INITIALIZE QCF-RECORD.                                       00155700
155800     MOVE ZEROS           TO ARCH-EFFDT-CEN                       00155800
155900                             ARCH-POINTERS-COUNT                  00155900
156000                             QCF-PLAN-CODE                        00156000
156100                             QCF-GROUP-NO                         00156100
156200                             QCF-SECTION-NO                       00156200
156300                             QCF-PKG-CODE.                        00156300
156400     MOVE SPACES          TO QCF-FILLER.                          00156400
156500     MOVE 'C'             TO ARCH-STA-CD                          00156500
156600                             QCF-CONTRACT-GROUP-SP-IND.           00156600
156700     MOVE PARM-LOCATION   TO QCF-PLAN-CODE.                       00156700
156800                                                                  00156800
156900     MOVE GCT-PLAN-CODE OF BIM-RECORD TO ARCH-PLAN-CODE.          00156900
157000     MOVE GCT-GROUP-NUM OF BIM-RECORD TO QCF-GROUP-NO             00157000
157100                                         ARCH-GROUP-NUM.          00157100
157200     MOVE GCT-SECTION-NUM OF BIM-RECORD TO QCF-SECTION-NO         00157200
157300                                         ARCH-SECTION-NUM.        00157300
157400     MOVE GCT-PKG-CODE OF BIM-RECORD TO QCF-PKG-CODE              00157400
157500                                        ARCH-PKG-CODE.            00157500
157600     MOVE GCT-L-O-B    OF BIM-RECORD TO QCF-L-O-B                 00157600
157700                                        ARCH-LOB.                 00157700
157800     MOVE GCT-PROVDR-CONTROL OF BIM-RECORD TO QCF-PROV-CTRL       00157800
157900                                        ARCH-PRV-CTL.             00157900
158000     MOVE GCT-FAM-REL-LVL    OF BIM-RECORD TO QCF-FAM-REL-LEVEL   00158000
158100                                              ARCH-FAM-RL.        00158100
158200     MOVE GCT-EFFDT-CEN      OF BIM-RECORD  TO ARCH-EFFDT-CEN.    00158200
158300*    MOVE WRK-OPERATOR-ID                  TO QCF-OPERATOR-ID.    00158300
158400                                                                  00158400
158500     IF TODAYS-DATE < 9999999                                     00158500
158600        MOVE TODAYS-DATE   TO QCF-FUNC-DATE.                      00158600
158700                                                                  00158700
158800     IF GCT-EFFDT-CEN OF BIM-RECORD  < 9999999                    00158800
158900        MOVE GCT-EFFDT-CEN OF BIM-RECORD TO QCF-EFF-DATE.         00158900
159000                                                                  00159000
159100*    MOVE GCT-GRP-NO   OF BIM-RECORD  TO ARCH-GRP-NO              00159100
159200*                                        QCF-SCND-PRT-GROUP-NO.   00159200
159300*    MOVE GCT-SECTN-NO OF BIM-RECORD  TO ARCH-SEC-NO              00159300
159400*                                        QCF-SCND-PRT-SECTION-NO. 00159400
159500*    MOVE GCT-L-O-B    OF BIM-RECORD  TO ARCH-LOB                 00159500
159600*                                        QCF-L-O-B.               00159600
159700*    MOVE GCT-PROVDR-CONTROL OF BIM-RECORD  TO ARCH-PRV-CTL       00159700
159800*                                              QCF-PROV-CTRL.     00159800
159900*    MOVE GCT-FAM-REL-LVL    OF BIM-RECORD  TO ARCH-FAM-RL        00159900
160000*                                              QCF-FAM-REL-LEVEL. 00160000
160100*    MOVE GCT-EFF-DT         OF BIM-RECORD  TO ARCH-EFF-DT.       00160100
160200                                                                  00160200
160300*    IF GCT-EFF-DT OF BIM-RECORD  < 99999                         00160300
160400*       COMPUTE                                                   00160400
160500*         QCF-EFF-DATE = +1900000 + GCT-EFF-DT OF BIM-RECORD.     00160500
160600                                                                  00160600
160700*    IF TODAYS-DATE < 99999                                       00160700
160800*       COMPUTE                                                   00160800
160900*         QCF-FUNC-DATE = +1900000 + TODAYS-DATE.                 00160900
161000                                                                  00161000
161100     SET  GCT-A-INDEX                 TO CON-A-INDX.              00161100
161200     MOVE GCT-A-DE-ID (GCT-A-INDEX)   TO ARCH-ID-CD               00161200
161300                                         QCF-FUNC-FIELD.          00161300
161400     MOVE BEFORE-FIELD (CON-B-INDX)   TO QCF-BIM-FIELD.           00161400
161500     MOVE AFTER-FIELD (CON-A-INDX)    TO QCF-AIM-FIELD.           00161500
161600                                                                  00161600
161700     ADD +1                TO ARCH-POINTERS-COUNT.                00161700
161800     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00161800
161900         GO TO 0080-EXIT.                                         00161900
162000     SET ARCH-INDEX        TO ARCH-POINTERS-COUNT.                00162000
162100     MOVE GCT-A-DE-VALUE (GCT-A-INDEX)                            00162100
162200                           TO ARCH-FLD-VALUE (ARCH-INDEX).        00162200
162300     MOVE WRK-OPERATOR-ID  TO ARCH-OPER-ID (ARCH-INDEX)           00162300
162400                              QCF-OPERATOR-ID.                    00162400
162500                                                                  00162500
162600     IF GCT2-INTER-REL-CD = ZEROS                                 00162600
162700        MOVE 'N' TO WS-INTER-REL-SW                               00162700
162800        ADD +1   TO WS-BYPASS-CNT                                 00162800
162900     ELSE                                                         00162900
163000        MOVE 'Y' TO WS-INTER-REL-SW.                              00163000
163100                                                                  00163100
163200     IF WRK-OPERATOR-ID > SPACES AND                              00163200
163300        INTER-REL-CD-FND                                          00163300
163400        PERFORM 0084-WRITE-QUAL-CTRL THRU 0084-EXIT.              00163400
163500                                                                  00163500
163600     IF WRK-CDE-SP NOT = '2 '                                     00163600
163700        MOVE 'X'  TO ARCH-CDE-IND (ARCH-INDEX)                    00163700
163800     ELSE                                                         00163800
163900        MOVE ' '  TO ARCH-CDE-IND (ARCH-INDEX).                   00163900
164000                                                                  00164000
164100     MOVE TODAYS-DATE         TO ARCH-CHNG-DT-CEN (ARCH-INDEX).   00164100
164200     MOVE SPACES              TO ARCH-ANLS-CD (ARCH-INDEX).       00164200
164300                                                                  00164300
164400     IF WRK-ATB3-REQUEST                                          00164400
164500        MOVE WRK-SIGNAL-FROM-ONLINE TO ARCH-ATB-IND (ARCH-INDEX)  00164500
164600     ELSE                                                         00164600
164700        MOVE WRK-TYPE-MAINT-IND     TO ARCH-ATB-IND (ARCH-INDEX). 00164700
164800                                                                  00164800
164900     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00164900
165000                                                                  00165000
165100 0080-EXIT.                                                       00165100
165200     EXIT.                                                        00165200
165300/                                                                 00165300
165400 0081-ADD-ARCH-TAB-RECORD.                                        00165400
165500*** ADD ARCHIVED RECORD FROM AFTER-IMAGE CONTRACT RECORD ***      00165500
165600                                                                  00165600
165700*MAM G&R - ADDED QUAL CNTRL LOGIC                                 00165700
165800     INITIALIZE QCF-RECORD.                                       00165800
165900     MOVE ZEROS           TO ARCH-EFFDT-CEN                       00165900
166000                             ARCH-POINTERS-COUNT                  00166000
166100                             QCF-PLAN-CODE                        00166100
166200                             QCF-GROUP-NO                         00166200
166300                             QCF-SECTION-NO                       00166300
166400                             QCF-PKG-CODE.                        00166400
166500     MOVE SPACES          TO QCF-FILLER.                          00166500
166600     MOVE  'C'            TO ARCH-STA-CD                          00166600
166700                             QCF-CONTRACT-GROUP-SP-IND.           00166700
166800     MOVE PARM-LOCATION   TO QCF-PLAN-CODE.                       00166800
166900                                                                  00166900
167000     MOVE GCT2-PLAN-CODE OF RCL-RECORD TO ARCH-PLAN-CODE.         00167000
167100     MOVE GCT2-GROUP-NUM OF RCL-RECORD TO QCF-GROUP-NO            00167100
167200                                         ARCH-GROUP-NUM.          00167200
167300     MOVE GCT2-SECTION-NUM OF RCL-RECORD TO QCF-SECTION-NO        00167300
167400                                         ARCH-SECTION-NUM.        00167400
167500     MOVE GCT2-PKG-CODE OF RCL-RECORD TO QCF-PKG-CODE             00167500
167600                                        ARCH-PKG-CODE.            00167600
167700     MOVE GCT2-L-O-B    OF RCL-RECORD TO QCF-L-O-B                00167700
167800                                        ARCH-LOB.                 00167800
167900     MOVE GCT2-PROVDR-CONTROL OF RCL-RECORD TO QCF-PROV-CTRL      00167900
168000                                        ARCH-PRV-CTL.             00168000
168100     MOVE GCT2-FAM-REL-LVL    OF RCL-RECORD TO QCF-FAM-REL-LEVEL  00168100
168200                                              ARCH-FAM-RL.        00168200
168300     MOVE GCT2-EFFDT-CEN      OF RCL-RECORD  TO ARCH-EFFDT-CEN.   00168300
168400*    MOVE WRK-OPERATOR-ID                  TO QCF-OPERATOR-ID.    00168400
168500                                                                  00168500
168600     IF GCT2-EFFDT-CEN OF RCL-RECORD < 9999999                    00168600
168700        MOVE GCT2-EFFDT-CEN OF RCL-RECORD TO QCF-EFF-DATE.        00168700
168800                                                                  00168800
168900     IF TODAYS-DATE < 9999999                                     00168900
169000        MOVE TODAYS-DATE   TO QCF-FUNC-DATE.                      00169000
169100                                                                  00169100
169200*    MOVE GCT2-GRP-NO   OF RCL-RECORD TO ARCH-GRP-NO              00169200
169300*                                        QCF-SCND-PRT-GROUP-NO.   00169300
169400*    MOVE GCT2-SECTN-NO OF RCL-RECORD TO ARCH-SEC-NO              00169400
169500*                                        QCF-SCND-PRT-SECTION-NO. 00169500
169600*    MOVE GCT2-L-O-B    OF RCL-RECORD TO ARCH-LOB                 00169600
169700*                                        QCF-L-O-B.               00169700
169800*    MOVE GCT2-PROVDR-CONTROL OF RCL-RECORD TO ARCH-PRV-CTL       00169800
169900*                                              QCF-PROV-CTRL.     00169900
170000*    MOVE GCT2-FAM-REL-LVL    OF RCL-RECORD TO ARCH-FAM-RL        00170000
170100*                                              QCF-FAM-REL-LEVEL. 00170100
170200*    MOVE GCT2-EFF-DT         OF RCL-RECORD TO ARCH-EFF-DT.       00170200
170300                                                                  00170300
170400*    IF GCT2-EFF-DT OF RCL-RECORD  < 99999                        00170400
170500*       COMPUTE                                                   00170500
170600*         QCF-EFF-DATE = +1900000 + GCT2-EFF-DT OF RCL-RECORD.    00170600
170700                                                                  00170700
170800*    IF TODAYS-DATE < 99999                                       00170800
170900*       COMPUTE                                                   00170900
171000*         QCF-FUNC-DATE = +1900000 + TODAYS-DATE.                 00171000
171100                                                                  00171100
171200     MOVE GCT2-CON-TAB-ID (GCT2-TAB-INDEX)  TO TB-ARCH-DE-2.      00171200
171300     MOVE TAB-ARCH-DE-CD                    TO ARCH-ID-CD         00171300
171400                                               QCF-FUNC-FIELD.    00171400
171500     MOVE GCT2-CON-TAB-ID (GCT2-TAB-INDEX)  TO QCF-AIM-FIELD.     00171500
171600     MOVE 'ADDED'                           TO QCF-BIM-FIELD.     00171600
171700                                                                  00171700
171800     ADD +1                 TO ARCH-POINTERS-COUNT.               00171800
171900     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00171900
172000         GO TO 0081-EXIT.                                         00172000
172100     SET ARCH-INDEX         TO ARCH-POINTERS-COUNT.               00172100
172200     MOVE '++ADDED++'       TO ARCH-FLD-VALUE (ARCH-INDEX).       00172200
172300     MOVE WRK2-OPERATOR-ID  TO ARCH-OPER-ID (ARCH-INDEX)          00172300
172400                                QCF-OPERATOR-ID.                  00172400
172500                                                                  00172500
172600     IF GCT2-INTER-REL-CD = ZEROS                                 00172600
172700        MOVE 'N' TO WS-INTER-REL-SW                               00172700
172800        ADD +1   TO WS-BYPASS-CNT                                 00172800
172900     ELSE                                                         00172900
173000        MOVE 'Y' TO WS-INTER-REL-SW.                              00173000
173100                                                                  00173100
173200     IF WRK-OPERATOR-ID > SPACES AND                              00173200
173300        INTER-REL-CD-FND                                          00173300
173400        PERFORM 0084-WRITE-QUAL-CTRL THRU 0084-EXIT.              00173400
173500                                                                  00173500
173600     IF WRK2-CDE-SP NOT = '2 '                                    00173600
173700        MOVE 'X'  TO ARCH-CDE-IND (ARCH-INDEX)                    00173700
173800     ELSE                                                         00173800
173900        MOVE ' '  TO ARCH-CDE-IND (ARCH-INDEX).                   00173900
174000                                                                  00174000
174100     MOVE TODAYS-DATE       TO ARCH-CHNG-DT-CEN (ARCH-INDEX).     00174100
174200     MOVE SPACES            TO ARCH-ANLS-CD (ARCH-INDEX).         00174200
174300                                                                  00174300
174400     IF WRK2-ATB3-REQUEST                                         00174400
174500        MOVE WRK2-SIGNAL-FROM-ONLINE TO ARCH-ATB-IND (ARCH-INDEX) 00174500
174600     ELSE                                                         00174600
174700        MOVE WRK2-TYPE-MAINT-IND TO ARCH-ATB-IND (ARCH-INDEX).    00174700
174800                                                                  00174800
174900     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00174900
175000 0081-EXIT.                                                       00175000
175100     EXIT.                                                        00175100
175200/                                                                 00175200
175300 0082-ADD-ARCH-TAB-RECORD.                                        00175300
175400*** ADD ARCHIVED RECORD FROM BEFORE-IMAGE CONTRACT RECORD ***     00175400
175500                                                                  00175500
175600*MAM G&R - ADDED QUAL CNTRL LOGIC                                 00175600
175700     INITIALIZE QCF-RECORD.                                       00175700
175800     MOVE ZEROS           TO ARCH-EFFDT-CEN                       00175800
175900                             ARCH-POINTERS-COUNT                  00175900
176000                             QCF-PLAN-CODE                        00176000
176100                             QCF-GROUP-NO                         00176100
176200                             QCF-SECTION-NO                       00176200
176300                             QCF-PKG-CODE.                        00176300
176400     MOVE SPACES          TO QCF-FILLER.                          00176400
176500     MOVE 'C'             TO ARCH-STA-CD                          00176500
176600                             QCF-CONTRACT-GROUP-SP-IND.           00176600
176700     MOVE PARM-LOCATION   TO QCF-PLAN-CODE.                       00176700
176800                                                                  00176800
176900     MOVE GCT-PLAN-CODE OF BIM-RECORD TO ARCH-PLAN-CODE.          00176900
177000     MOVE GCT-GROUP-NUM OF BIM-RECORD TO QCF-GROUP-NO             00177000
177100                                         ARCH-GROUP-NUM.          00177100
177200     MOVE GCT-SECTION-NUM OF BIM-RECORD TO QCF-SECTION-NO         00177200
177300                                         ARCH-SECTION-NUM.        00177300
177400     MOVE GCT-PKG-CODE OF BIM-RECORD TO QCF-PKG-CODE              00177400
177500                                        ARCH-PKG-CODE.            00177500
177600     MOVE GCT-L-O-B    OF BIM-RECORD TO QCF-L-O-B                 00177600
177700                                        ARCH-LOB.                 00177700
177800     MOVE GCT-PROVDR-CONTROL OF BIM-RECORD TO QCF-PROV-CTRL       00177800
177900                                        ARCH-PRV-CTL.             00177900
178000     MOVE GCT-FAM-REL-LVL    OF BIM-RECORD TO QCF-FAM-REL-LEVEL   00178000
178100                                              ARCH-FAM-RL.        00178100
178200     MOVE GCT-EFFDT-CEN      OF BIM-RECORD  TO ARCH-EFFDT-CEN.    00178200
178300                                                                  00178300
178400     IF TODAYS-DATE < 9999999                                     00178400
178500        MOVE TODAYS-DATE   TO QCF-FUNC-DATE.                      00178500
178600                                                                  00178600
178700     IF GCT-EFFDT-CEN OF BIM-RECORD  < 9999999                    00178700
178800        MOVE GCT-EFFDT-CEN OF BIM-RECORD TO QCF-EFF-DATE.         00178800
178900                                                                  00178900
179000*    MOVE GCT-GRP-NO   OF BIM-RECORD   TO ARCH-GRP-NO             00179000
179100*                                         QCF-SCND-PRT-GROUP-NO.  00179100
179200*    MOVE GCT-SECTN-NO OF BIM-RECORD   TO ARCH-SEC-NO             00179200
179300*                                         QCF-SCND-PRT-SECTION-NO.00179300
179400*    MOVE GCT-L-O-B    OF BIM-RECORD   TO ARCH-LOB                00179400
179500*                                         QCF-L-O-B.              00179500
179600*    MOVE GCT-PROVDR-CONTROL OF BIM-RECORD  TO ARCH-PRV-CTL       00179600
179700*                                              QCF-PROV-CTRL.     00179700
179800*    MOVE GCT-FAM-REL-LVL    OF BIM-RECORD  TO ARCH-FAM-RL        00179800
179900*                                              QCF-FAM-REL-LEVEL. 00179900
180000*    MOVE GCT-EFF-DT         OF BIM-RECORD  TO ARCH-EFF-DT.       00180000
180100                                                                  00180100
180200*    IF GCT-EFF-DT OF BIM-RECORD  < 99999                         00180200
180300*       COMPUTE                                                   00180300
180400*         QCF-EFF-DATE = +1900000 + GCT-EFF-DT OF BIM-RECORD.     00180400
180500                                                                  00180500
180600*    IF TODAYS-DATE < 99999                                       00180600
180700*       COMPUTE                                                   00180700
180800*         QCF-FUNC-DATE = +1900000 + TODAYS-DATE.                 00180800
180900                                                                  00180900
181000     MOVE TAB-ARCH-DE-CD    TO ARCH-ID-CD                         00181000
181100                               QCF-FUNC-FIELD.                    00181100
181200                                                                  00181200
181300     IF GCT2-CON-TAB-ID (GCT2-TAB-INDEX) GREATER THAN             00181300
181400        GCT-CON-TAB-ID (GCT-TAB-INDEX)                            00181400
181500         MOVE GCT-CON-TAB-ID (GCT-TAB-INDEX) TO QCF-BIM-FIELD     00181500
181600         MOVE 'DELETED'                      TO QCF-AIM-FIELD     00181600
181700     ELSE                                                         00181700
181800        IF GCT2-CON-TAB-ID (GCT2-TAB-INDEX) EQUAL                 00181800
181900           GCT-CON-TAB-ID (GCT-TAB-INDEX)                         00181900
182000            MOVE GCT-CON-TAB-SLOT (GCT-TAB-INDEX)   TO            00182000
182100                                                  QCF-BIM-FIELD   00182100
182200            MOVE GCT2-CON-TAB-SLOT (GCT2-TAB-INDEX) TO            00182200
182300                                                  QCF-AIM-FIELD   00182300
182400        END-IF                                                    00182400
182500     END-IF.                                                      00182500
182600                                                                  00182600
182700     ADD +1                 TO ARCH-POINTERS-COUNT.               00182700
182800     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00182800
182900         GO TO 0082-EXIT.                                         00182900
183000     SET ARCH-INDEX         TO ARCH-POINTERS-COUNT.               00183000
183100     MOVE GCT-CON-TAB-SLOT (GCT-TAB-INDEX)                        00183100
183200                            TO ARCH-FLD-VALUE (ARCH-INDEX).       00183200
183300     MOVE WRK-OPERATOR-ID   TO ARCH-OPER-ID (ARCH-INDEX)          00183300
183400                               QCF-OPERATOR-ID.                   00183400
183500                                                                  00183500
183600     IF GCT2-INTER-REL-CD = ZEROS                                 00183600
183700        MOVE 'N' TO WS-INTER-REL-SW                               00183700
183800        ADD +1   TO WS-BYPASS-CNT                                 00183800
183900     ELSE                                                         00183900
184000        MOVE 'Y' TO WS-INTER-REL-SW.                              00184000
184100                                                                  00184100
184200     IF WRK-OPERATOR-ID > SPACES AND                              00184200
184300        INTER-REL-CD-FND                                          00184300
184400        PERFORM 0084-WRITE-QUAL-CTRL THRU 0084-EXIT.              00184400
184500                                                                  00184500
184600     IF WRK-CDE-SP NOT = '2 '                                     00184600
184700        MOVE 'X'   TO ARCH-CDE-IND (ARCH-INDEX)                   00184700
184800     ELSE                                                         00184800
184900        MOVE ' '   TO ARCH-CDE-IND (ARCH-INDEX).                  00184900
185000                                                                  00185000
185100     MOVE TODAYS-DATE       TO ARCH-CHNG-DT-CEN (ARCH-INDEX).     00185100
185200     MOVE SPACES            TO ARCH-ANLS-CD (ARCH-INDEX).         00185200
185300                                                                  00185300
185400     IF WRK-ATB3-REQUEST                                          00185400
185500        MOVE WRK-SIGNAL-FROM-ONLINE TO ARCH-ATB-IND (ARCH-INDEX)  00185500
185600     ELSE                                                         00185600
185700        MOVE WRK-TYPE-MAINT-IND  TO ARCH-ATB-IND (ARCH-INDEX).    00185700
185800                                                                  00185800
185900     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00185900
186000 0082-EXIT.                                                       00186000
186100     EXIT.                                                        00186100
186200/                                                                 00186200
186300*MAM G&R - ADDED PARAGRAPH TO WRITE QUAL CTRL FILE                00186300
186400 0084-WRITE-QUAL-CTRL.                                            00186400
186500                                                                  00186500
186600     WRITE QCF-RECORD.                                            00186600
186700     ADD +1 TO WS-QCF-COUNT.                                      00186700
186800                                                                  00186800
186900 0084-EXIT.                                                       00186900
187000     EXIT.                                                        00187000
187100/                                                                 00187100
187200 0085-UPDATE-ARCHIVED-FILE.                                       00187200
187300***  REWRITE/WRITE THE ARCHIVED RECORD WITH THE NEW ENTRY         00187300
187400                                                                  00187400
187500     MOVE ARCH-POINTERS-COUNT    TO PNTRS-COUNT-A.                00187500
187600     MOVE ARCHIVED-GCPS-RECORD   TO REC-AREA-A.                   00187600
187700                                                                  00187700
187800     COMPUTE RECORD-LENGTH-A =  GC-ARCHIVE-FIXED-LEN + 4          00187800
187900                + (GC-ARCHIVE-VARY-LEN * ARCH-POINTERS-COUNT).    00187900
188000                                                                  00188000
188100     MOVE 'W'         TO REQUEST-TYPE-A.                          00188100
188200     CALL 'TSGVSAM3'  USING PARM-ONE-A PARM-TWO-A.                00188200
188300                                                                  00188300
188400     IF  REQUEST-TYPE-A NOT EQUAL 'W'                             00188400
188500         MOVE FEEDBACK-CODE-A    TO ABEND-CODE                    00188500
188600         GO TO 9999-ERROR-RTN.                                    00188600
188700                                                                  00188700
188800 0085-EXIT.                                                       00188800
188900     EXIT.                                                        00188900
189000/                                                                 00189000
189100 0086-ADD-ARCH-BEN-RECORD.                                        00189100
189200*** ADD ARCHIVED RECORD FROM AFTER-IMAGE CONTRACT RECORD ***      00189200
189300                                                                  00189300
189400*MAM G&R - ADDED QUAL CNTRL LOGIC                                 00189400
189500     INITIALIZE QCF-RECORD.                                       00189500
189600     MOVE ZEROS           TO ARCH-EFFDT-CEN                       00189600
189700                             ARCH-POINTERS-COUNT                  00189700
189800                             QCF-PLAN-CODE                        00189800
189900                             QCF-GROUP-NO                         00189900
190000                             QCF-SECTION-NO                       00190000
190100                             QCF-PKG-CODE.                        00190100
190200     MOVE SPACES          TO QCF-FILLER.                          00190200
190300     MOVE 'C'             TO ARCH-STA-CD                          00190300
190400                             QCF-CONTRACT-GROUP-SP-IND.           00190400
190500     MOVE PARM-LOCATION   TO QCF-PLAN-CODE.                       00190500
190600                                                                  00190600
190700     MOVE GCT2-PLAN-CODE OF RCL-RECORD TO ARCH-PLAN-CODE.         00190700
190800     MOVE GCT2-GROUP-NUM OF RCL-RECORD TO QCF-GROUP-NO            00190800
190900                                         ARCH-GROUP-NUM.          00190900
191000     MOVE GCT2-SECTION-NUM OF RCL-RECORD TO QCF-SECTION-NO        00191000
191100                                         ARCH-SECTION-NUM.        00191100
191200     MOVE GCT2-PKG-CODE OF RCL-RECORD TO QCF-PKG-CODE             00191200
191300                                        ARCH-PKG-CODE.            00191300
191400     MOVE GCT2-L-O-B    OF RCL-RECORD TO QCF-L-O-B                00191400
191500                                        ARCH-LOB.                 00191500
191600     MOVE GCT2-PROVDR-CONTROL OF RCL-RECORD TO QCF-PROV-CTRL      00191600
191700                                        ARCH-PRV-CTL.             00191700
191800     MOVE GCT2-FAM-REL-LVL    OF RCL-RECORD TO QCF-FAM-REL-LEVEL  00191800
191900                                              ARCH-FAM-RL.        00191900
192000     MOVE GCT2-EFFDT-CEN      OF RCL-RECORD  TO ARCH-EFFDT-CEN.   00192000
192100                                                                  00192100
192200     IF GCT2-EFFDT-CEN OF RCL-RECORD < 9999999                    00192200
192300        MOVE GCT2-EFFDT-CEN OF RCL-RECORD TO QCF-EFF-DATE.        00192300
192400                                                                  00192400
192500     IF TODAYS-DATE < 9999999                                     00192500
192600        MOVE TODAYS-DATE   TO QCF-FUNC-DATE.                      00192600
192700                                                                  00192700
192800*    MOVE GCT2-GRP-NO   OF RCL-RECORD TO ARCH-GRP-NO              00192800
192900*                                        QCF-SCND-PRT-GROUP-NO.   00192900
193000*    MOVE GCT2-SECTN-NO OF RCL-RECORD TO ARCH-SEC-NO              00193000
193100*                                        QCF-SCND-PRT-SECTION-NO. 00193100
193200*    MOVE GCT2-L-O-B    OF RCL-RECORD TO ARCH-LOB                 00193200
193300*                                        QCF-L-O-B.               00193300
193400                                                                  00193400
193500*    MOVE GCT2-PROVDR-CONTROL OF RCL-RECORD TO ARCH-PRV-CTL       00193500
193600*                                              QCF-PROV-CTRL.     00193600
193700*    MOVE GCT2-FAM-REL-LVL    OF RCL-RECORD TO ARCH-FAM-RL        00193700
193800*                                              QCF-FAM-REL-LEVEL. 00193800
193900*    MOVE GCT2-EFF-DT         OF RCL-RECORD TO ARCH-EFF-DT.       00193900
194000                                                                  00194000
194100*    IF GCT2-EFF-DT OF RCL-RECORD  < 99999                        00194100
194200*       COMPUTE                                                   00194200
194300*         QCF-EFF-DATE = +1900000 + GCT2-EFF-DT OF RCL-RECORD.    00194300
194400                                                                  00194400
194500*    IF TODAYS-DATE < 99999                                       00194500
194600*       COMPUTE                                                   00194600
194700*         QCF-FUNC-DATE = +1900000 + TODAYS-DATE.                 00194700
194800                                                                  00194800
194900     MOVE GCT2-BEN-PROVN-ID (GCT2-INDEX)    TO BN-ARCH-DE-2.      00194900
195000     MOVE BEN-ARCH-DE-CD                    TO ARCH-ID-CD         00195000
195100                                               QCF-FUNC-FIELD.    00195100
195200     MOVE GCT2-BEN-PROVN-ID (GCT2-INDEX)    TO QCF-AIM-FIELD.     00195200
195300     MOVE 'ADDED'                           TO QCF-BIM-FIELD.     00195300
195400                                                                  00195400
195500     ADD +1                 TO ARCH-POINTERS-COUNT.               00195500
195600     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00195600
195700         GO TO 0086-EXIT.                                         00195700
195800     SET ARCH-INDEX         TO ARCH-POINTERS-COUNT.               00195800
195900                                                                  00195900
196000     MOVE '++ADDED++'       TO ARCH-FLD-VALUE (ARCH-INDEX).       00196000
196100     MOVE WRK2-OPERATOR-ID  TO ARCH-OPER-ID (ARCH-INDEX)          00196100
196200                               QCF-OPERATOR-ID.                   00196200
196300                                                                  00196300
196400     IF GCT2-INTER-REL-CD = ZEROS                                 00196400
196500        MOVE 'N' TO WS-INTER-REL-SW                               00196500
196600        ADD +1   TO WS-BYPASS-CNT                                 00196600
196700     ELSE                                                         00196700
196800        MOVE 'Y' TO WS-INTER-REL-SW.                              00196800
196900                                                                  00196900
197000     IF WRK-OPERATOR-ID > SPACES AND                              00197000
197100        INTER-REL-CD-FND                                          00197100
197200        PERFORM 0084-WRITE-QUAL-CTRL THRU 0084-EXIT.              00197200
197300                                                                  00197300
197400     IF WRK2-CDE-SP NOT = '2 '                                    00197400
197500        MOVE 'X'  TO ARCH-CDE-IND (ARCH-INDEX)                    00197500
197600     ELSE                                                         00197600
197700        MOVE ' '  TO ARCH-CDE-IND (ARCH-INDEX).                   00197700
197800                                                                  00197800
197900     MOVE TODAYS-DATE         TO ARCH-CHNG-DT-CEN (ARCH-INDEX).   00197900
198000     MOVE SPACES              TO ARCH-ANLS-CD (ARCH-INDEX).       00198000
198100                                                                  00198100
198200     IF WRK2-ATB3-REQUEST                                         00198200
198300        MOVE WRK2-SIGNAL-FROM-ONLINE TO ARCH-ATB-IND (ARCH-INDEX) 00198300
198400     ELSE                                                         00198400
198500        MOVE WRK2-TYPE-MAINT-IND  TO ARCH-ATB-IND (ARCH-INDEX).   00198500
198600                                                                  00198600
198700     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00198700
198800 0086-EXIT.                                                       00198800
198900     EXIT.                                                        00198900
199000/                                                                 00199000
199100 0087-ADD-ARCH-BEN-RECORD.                                        00199100
199200*** ADD ARCHIVED RECORD FROM BEFORE-IMAGE CONTRACT RECORD ***     00199200
199300                                                                  00199300
199400*MAM G&R - ADDED QUAL CNTRL LOGIC   PHF                           00199400
199500     INITIALIZE QCF-RECORD.                                       00199500
199600     MOVE ZEROS           TO ARCH-EFFDT-CEN                       00199600
199700                             ARCH-POINTERS-COUNT                  00199700
199800                             QCF-PLAN-CODE                        00199800
199900                             QCF-GROUP-NO                         00199900
200000                             QCF-SECTION-NO                       00200000
200100                             QCF-PKG-CODE.                        00200100
200200     MOVE SPACES          TO QCF-FILLER.                          00200200
200300     MOVE 'C'             TO ARCH-STA-CD                          00200300
200400                             QCF-CONTRACT-GROUP-SP-IND.           00200400
200500     MOVE PARM-LOCATION   TO QCF-PLAN-CODE.                       00200500
200600                                                                  00200600
200700     MOVE GCT-PLAN-CODE OF BIM-RECORD TO ARCH-PLAN-CODE.          00200700
200800     MOVE GCT-GROUP-NUM OF BIM-RECORD TO QCF-GROUP-NO             00200800
200900                                         ARCH-GROUP-NUM.          00200900
201000     MOVE GCT-SECTION-NUM OF BIM-RECORD TO QCF-SECTION-NO         00201000
201100                                         ARCH-SECTION-NUM.        00201100
201200     MOVE GCT-PKG-CODE OF BIM-RECORD TO QCF-PKG-CODE              00201200
201300                                        ARCH-PKG-CODE.            00201300
201400     MOVE GCT-L-O-B    OF BIM-RECORD TO QCF-L-O-B                 00201400
201500                                        ARCH-LOB.                 00201500
201600     MOVE GCT-PROVDR-CONTROL OF BIM-RECORD TO QCF-PROV-CTRL       00201600
201700                                        ARCH-PRV-CTL.             00201700
201800     MOVE GCT-FAM-REL-LVL    OF BIM-RECORD TO QCF-FAM-REL-LEVEL   00201800
201900                                              ARCH-FAM-RL.        00201900
202000     MOVE GCT-EFFDT-CEN      OF BIM-RECORD  TO ARCH-EFFDT-CEN.    00202000
202100                                                                  00202100
202200     IF TODAYS-DATE < 9999999                                     00202200
202300        MOVE TODAYS-DATE   TO QCF-FUNC-DATE.                      00202300
202400                                                                  00202400
202500     IF GCT-EFFDT-CEN OF BIM-RECORD  < 9999999                    00202500
202600        MOVE GCT-EFFDT-CEN OF BIM-RECORD TO QCF-EFF-DATE.         00202600
202700                                                                  00202700
202800*    MOVE GCT-GRP-NO   OF BIM-RECORD TO ARCH-GRP-NO               00202800
202900*                                       QCF-SCND-PRT-GROUP-NO.    00202900
203000*    MOVE GCT-SECTN-NO OF BIM-RECORD TO ARCH-SEC-NO               00203000
203100*                                       QCF-SCND-PRT-SECTION-NO.  00203100
203200*    MOVE GCT-L-O-B    OF BIM-RECORD TO ARCH-LOB                  00203200
203300*                                       QCF-L-O-B.                00203300
203400*    MOVE GCT-PROVDR-CONTROL OF BIM-RECORD TO ARCH-PRV-CTL        00203400
203500*                                             QCF-PROV-CTRL.      00203500
203600*    MOVE GCT-FAM-REL-LVL    OF BIM-RECORD TO ARCH-FAM-RL         00203600
203700*                                             QCF-FAM-REL-LEVEL.  00203700
203800*    MOVE GCT-EFF-DT         OF BIM-RECORD TO ARCH-EFF-DT.        00203800
203900                                                                  00203900
204000*    IF GCT-EFF-DT OF BIM-RECORD  < 99999                         00204000
204100*       COMPUTE                                                   00204100
204200*         QCF-EFF-DATE = +1900000 + GCT-EFF-DT OF BIM-RECORD.     00204200
204300                                                                  00204300
204400*    IF TODAYS-DATE < 99999                                       00204400
204500*       COMPUTE                                                   00204500
204600*         QCF-FUNC-DATE = +1900000 + TODAYS-DATE.                 00204600
204700                                                                  00204700
204800     MOVE BEN-ARCH-DE-CD     TO ARCH-ID-CD                        00204800
204900                                QCF-FUNC-FIELD.                   00204900
205000                                                                  00205000
205100     IF GCT2-BEN-PROVN-ID (GCT2-INDEX) GREATER THAN               00205100
205200        GCT-BEN-PROVN-ID (GCT-INDEX)                              00205200
205300         MOVE GCT-BEN-PROVN-ID (GCT-INDEX)  TO QCF-BIM-FIELD      00205300
205400         MOVE 'DELETED'                     TO QCF-AIM-FIELD      00205400
205500     ELSE                                                         00205500
205600        IF GCT2-BEN-PROVN-ID (GCT2-INDEX) EQUAL                   00205600
205700           GCT-BEN-PROVN-ID (GCT-INDEX)                           00205700
205800            MOVE GCT-BEN-PROVN-SLOT-NO (GCT-INDEX)                00205800
205900                                            TO QCF-BIM-FIELD      00205900
206000            MOVE GCT2-BEN-PROVN-SLOT-NO (GCT2-INDEX)              00206000
206100                                            TO QCF-AIM-FIELD      00206100
206200        END-IF                                                    00206200
206300     END-IF.                                                      00206300
206400                                                                  00206400
206500     ADD +1          TO ARCH-POINTERS-COUNT.                      00206500
206600     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00206600
206700         GO TO 0087-EXIT.                                         00206700
206800     SET ARCH-INDEX  TO ARCH-POINTERS-COUNT.                      00206800
206900                                                                  00206900
207000     MOVE GCT-BEN-PROVN-SLOT-NO (GCT-INDEX) TO                    00207000
207100                                   ARCH-FLD-VALUE (ARCH-INDEX).   00207100
207200     MOVE WRK-OPERATOR-ID       TO ARCH-OPER-ID (ARCH-INDEX)      00207200
207300                                   QCF-OPERATOR-ID.               00207300
207400                                                                  00207400
207500     IF GCT2-INTER-REL-CD = ZEROS                                 00207500
207600        MOVE 'N' TO WS-INTER-REL-SW                               00207600
207700        ADD +1   TO WS-BYPASS-CNT                                 00207700
207800     ELSE                                                         00207800
207900        MOVE 'Y' TO WS-INTER-REL-SW.                              00207900
208000                                                                  00208000
208100     IF WRK-OPERATOR-ID > SPACES AND                              00208100
208200        INTER-REL-CD-FND                                          00208200
208300        PERFORM 0084-WRITE-QUAL-CTRL THRU 0084-EXIT.              00208300
208400                                                                  00208400
208500     IF WRK-CDE-SP NOT = '2 '                                     00208500
208600        MOVE 'X'  TO ARCH-CDE-IND (ARCH-INDEX)                    00208600
208700     ELSE                                                         00208700
208800        MOVE ' '  TO ARCH-CDE-IND (ARCH-INDEX).                   00208800
208900                                                                  00208900
209000     MOVE TODAYS-DATE         TO ARCH-CHNG-DT-CEN (ARCH-INDEX).   00209000
209100     MOVE SPACES              TO ARCH-ANLS-CD (ARCH-INDEX).       00209100
209200                                                                  00209200
209300     IF WRK-ATB3-REQUEST                                          00209300
209400        MOVE WRK-SIGNAL-FROM-ONLINE TO ARCH-ATB-IND (ARCH-INDEX)  00209400
209500     ELSE                                                         00209500
209600        MOVE WRK-TYPE-MAINT-IND     TO ARCH-ATB-IND (ARCH-INDEX). 00209600
209700                                                                  00209700
209800     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00209800
209900 0087-EXIT.                                                       00209900
210000     EXIT.                                                        00210000
210100/                                                                 00210100
210200 9999-ERROR-RTN.                                                  00210200
210300     CALL 'TSGEND' USING ABEND-CODE.                              00210300
210400 9999-EXIT.                                                       00210400
210500     EXIT.                                                        00210500
