000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID. GC0300.                                              00000200
000300 AUTHOR. ROBERT MANN - DECISION CONSULTANTS INC.                  00000300
000400 INSTALLATION.  HCSC.                                             00000400
000500 DATE-WRITTEN.  APRIL 1984.                                       00000500
000600 DATE-COMPILED.                                                   00000600
000700                                                                  00000700
000800******************************************************************00000800
000900*    THIS PROGRAM DELETES RELEASED RECORDS FROM THE               00000900
001000*    ONLINE WORK FILE USING THE UPDATED DAILY RELEASE             00001000
001100*    ONLINE WORK FILE AS INPUT.                                   00001100
001200*                                                                 00001200
001300*    TSGVSAM1 IS ONLINE WORK FILE -OWF (I/O).                     00001300
001400*    UDR-FILE IS UPDATED DAILY RELEASE FILE (IN).                 00001400
001500*    C9-G9-FILES ARE TWO FILES, CONCATENATED, USED AS INPUT.    **00001500
001600**      C9-G9-FILES:  HCMS.GCPSG.GC0010L.C9.AUDITWK.RELEASE     **00001600
001700**                    HCMS.GCPSG.GC0010M.G9.AUDITWK.RELEASE.    **00001700
001800**                                                              **00001800
001900*    1/17/95  EMS  CONVERTED TO COBOL II.                         00001900
002000*                                                                 00002000
002100******************************************************************00002100
002200                                                                  00002200
002300******************************************************************00002300
002400**                                                              **00002400
002500**                   U P D A T E   H I S T O R Y                **00002500
002600**                                                              **00002600
002700**   NUM     DATE     WHO    DESCRIPTION                        **00002700
002800**  ____   _______    ___    _________________________________  **00002800
002900**  D170   3/13/89    FRY    ADD LOGIC TO:                      **00002900
003000**                           -DELETE 'C9' AND 'G9' AUDIT WORK   **00003000
003100**                            RECORDS FROM THE GCPS WORKFILE,   **00003100
003200**                           -DISPLAY KEY OF RECORD WHEN THERE  **00003200
003300**                            IS AN ABEND.                      **00003300
003400**                                                              **00003400
003500** 11154   3/06/91    FRY    INCREASE RECORD AREA IN FILE       **00003500
003600**                           SECTION:                           **00003600
003700**                    UDR-DATA   PIC X(5763)  CHANGED TO  7805. **00003700
003800**                                                              **00003800
003900** D12009 09/10/91    GDM    INCREASE WS-FAM                    **00003900
004000**                                    WS-CON-FAM                **00004000
004100**                                    WS-GRP-FAM TO 2 POSITIONS **00004100
004200**                                                              **00004200
004300** D12009 09/19/91    TPM     ADJUSTED 1A-REC-KEY TO ACCOMODATE **00004300
004400**                            FOR THE EXPANSION OF FAMILY       **00004400
004500**                            RELATION FIELD.                   **00004500
004600**                            CHANGE THE RECORD LENGTH FROM 44  **00004600
004700**                            TO 45 WHEN CALLING 'TSGVSAM1'.    **00004700
004800** XXXXXX 01/25/95    JGR     CONTRACT RECORD EXPANSION.        **00004800
004900**                                                              **00004900
005000** 14726/ 12/04/97    DAU     ADDED CODE TO SUPPORT THE YEAR    **00005000
005100** 15057                      2000 AND THE EXPANSION OF THE     **00005100
005200**                            GROUP SPECIFIC AND CONTRACT KEY   **00005200
005300**                            TO SUPPORT THE TEXAS MERGER.      **00005300
005400**        04/30/02    AKK     CHANGED UDR-FILE TO 8213 TO FIX   **00005400
005500**                            FILE MISMATCH.                    **00005500
005600*            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *00005600
005700*                                                                *00005700
005800*            09-19-02   GTF   INCREASE INPUT FILE SIZE TO 6252   *00005800
005900*                             FROM 5760 FOR OPID EXPANSION.      *00005900
006000*  DM9400    06-30-07   LR    INCREASE C9-G9 FILE SIZE TO 6556   *00006000
006100*                             FROM 6252 FOR COPYBOOK EXPANSION.  *00006100
006110*                                                                *00006110
006120*  DM9441    09-09-09   JH    CHANGED UDR-DATA FROM 8113 TO      *00006120
006130*                             31370.                             *00006130
006140*                                                                *00006140
006200******************************************************************00006200
006300 ENVIRONMENT DIVISION.                                            00006300
006400 CONFIGURATION SECTION.                                           00006400
006500 SOURCE-COMPUTER. IBM-370.                                        00006500
006600 OBJECT-COMPUTER. IBM-370.                                        00006600
006700 INPUT-OUTPUT SECTION.                                            00006700
006800 FILE-CONTROL.                                                    00006800
006900     SELECT UDR-FILE                                              00006900
007000                         ASSIGN TO  UT-S-GC0300A.                 00007000
007100     SELECT C9-G9-FILES  ASSIGN TO  UT-S-GC0300B.                 00007100
007200                                                                  00007200
007300 DATA DIVISION.                                                   00007300
007400 FILE SECTION.                                                    00007400
007500                                                                  00007500
007600 FD  UDR-FILE                                                     00007600
007700     LABEL RECORDS ARE STANDARD                                   00007700
007800     RECORDING MODE IS V                                          00007800
007900     BLOCK CONTAINS 0 RECORDS.                                    00007900
008000 01  UDR-REC.                                                     00008000
008100     COPY GCWRKDCC.                                               00008100
008200*    05  UDR-DATA            PIC X(8118).                         00008200
008300     05  UDR-DATA            PIC X(31370).                        00008300
008400                                                                  00008400
008500 FD  C9-G9-FILES                                                  00008500
008600     LABEL RECORDS ARE STANDARD                                   00008600
008700     RECORDING MODE IS V                                          00008700
008800     BLOCK CONTAINS 0 RECORDS.                                    00008800
008900 01  C9-G9-RECS.                                                  00008900
009000     COPY GCWRKDC2.                                               00009000
009100     05  C9-G9-DATA          PIC X(6594).                         00009100
009200/                                                                 00009200
009300 WORKING-STORAGE SECTION.                                         00009300
009400                                                                  00009400
009500 01  FILLER                      PIC X(22)   VALUE                00009500
009600                                 'GC0300 WORKING STORAGE'.        00009600
009700                                                                  00009700
009800 01  ABEND-CODE                  PIC 9(4)    COMP.                00009800
009900                                                                  00009900
010000 01  PARM-1.                                                      00010000
010100     05  1-RESERVED-FLDS         PIC 9(8)    VALUE ZEROS COMP.    00010100
010200     05  1-RESERV-FLDS REDEFINES 1-RESERVED-FLDS.                 00010200
010300         10  1-REQUEST-TYPE      PIC X.                           00010300
010400         10  FILLER              PIC X(3).                        00010400
010500                                                                  00010500
010600 01  PARM-1A.                                                     00010600
010700     02  1A-RDW.                                                  00010700
010800         05  1A-REC-LENG         PIC 9(4)    VALUE ZEROS COMP.    00010800
010900         05  1A-FEEDBACK         PIC 9(4)    VALUE ZEROS COMP.    00010900
011000     02  1A-REC-AREA             PIC X(100).                      00011000
011100     02  1A-REC-A REDEFINES 1A-REC-AREA.                          00011100
011200         05  1A-REC-KEY.                                          00011200
011300             10  1A-KEY          PIC X(57).                       00011300
011400             10  FILLER          PIC X(43).                       00011400
011500                                                                  00011500
011600 01  PARM-SET.                                                    00011600
011700     05  SET-RDW.                                                 00011700
011800         10  SET-REC-LENG        PIC 9(4)    VALUE ZEROS COMP.    00011800
011900         10  SET-FEEDBACK        PIC 9(4)    VALUE ZEROS COMP.    00011900
012000     05  SET-VALUE               PIC 9(8)    VALUE ZEROS COMP.    00012000
012100                                                                  00012100
012200 01  WS-SWITCH-AREA.                                              00012200
012300     05  UDR-EOF-SW              PIC X       VALUE SPACES.        00012300
012400         88  EOF-UDR                         VALUE HIGH-VALUES.   00012400
012500     05  WS-C9-G9-EOF-SW         PIC X(01)   VALUE '0'.           00012500
012600         88  WS-C9-G9-EOF-SW-ON              VALUE '1'.           00012600
012700                                                                  00012700
012800 01  WS-HOLD-KEY.                                                 00012800
012900     05  WS-STATUS-CODE              PIC X(01)  VALUE SPACES.     00012900
013000     05  FILLER                      PIC X(02)  VALUE SPACES.     00013000
013100     05  WS-PLAN-CODE                PIC X(03)  VALUE SPACES.     00013100
013200     05  FILLER                      PIC X(01)  VALUE SPACES.     00013200
013300     05  WS-GROUP-NUM                PIC X(09)  VALUE SPACES.     00013300
013400     05  FILLER                      PIC X(01)  VALUE SPACES.     00013400
013500     05  WS-SECTION-NUM              PIC X(05)  VALUE SPACES.     00013500
013600     05  FILLER                      PIC X(01)  VALUE SPACES.     00013600
013700     05  WS-PKG-CODE                 PIC X(03)  VALUE SPACES.     00013700
013800     05  FILLER                      PIC X(01)  VALUE SPACES.     00013800
013900     05  WS-LOB                      PIC X(01)  VALUE SPACES.     00013900
014000     05  FILLER                      PIC X(01)  VALUE SPACES.     00014000
014100     05  WS-PRV                      PIC X(02)  VALUE SPACES.     00014100
014200     05  FILLER                      PIC X(01)  VALUE SPACES.     00014200
014300     05  WS-FAM                      PIC X(02)  VALUE SPACES.     00014300
014400     05  FILLER                      PIC X(01)  VALUE SPACES.     00014400
014500     05  WS-EFFDT-CEN                PIC 9(07)  VALUE ZEROES.     00014500
014600     05  FILLER                      PIC X(02)  VALUE SPACES.     00014600
014700     05  WS-REC-TYPE                 PIC X(02)  VALUE SPACES.     00014700
014800                                                                  00014800
014900 01  WS-HOLD-CON-KEY.                                             00014900
015000     05  WS-CON-STATUS-CODE          PIC X(01)  VALUE SPACES.     00015000
015100     05  FILLER                      PIC X(02)  VALUE SPACES.     00015100
015200     05  WS-CON-PLAN-CODE            PIC X(03)  VALUE SPACES.     00015200
015300     05  FILLER                      PIC X(01)  VALUE SPACES.     00015300
015400     05  WS-CON-GROUP-NUM            PIC X(09)  VALUE SPACES.     00015400
015500     05  FILLER                      PIC X(01)  VALUE SPACES.     00015500
015600     05  WS-CON-SECTION-NUM          PIC X(05)  VALUE SPACES.     00015600
015700     05  FILLER                      PIC X(01)  VALUE SPACES.     00015700
015800     05  WS-CON-PKG-CODE             PIC X(03)  VALUE SPACES.     00015800
015900     05  FILLER                      PIC X(01)  VALUE SPACES.     00015900
016000     05  WS-CON-LOB                  PIC X(01)  VALUE SPACES.     00016000
016100     05  FILLER                      PIC X(01)  VALUE SPACES.     00016100
016200     05  WS-CON-PRV                  PIC X(02)  VALUE SPACES.     00016200
016300     05  FILLER                      PIC X(01)  VALUE SPACES.     00016300
016400     05  WS-CON-FAM                  PIC X(02)  VALUE SPACES.     00016400
016500     05  FILLER                      PIC X(01)  VALUE SPACES.     00016500
016600     05  WS-CON-EFFDT-CEN            PIC 9(07)  VALUE ZEROES.     00016600
016700     05  FILLER                      PIC X(02)  VALUE SPACES.     00016700
016800     05  WS-CON-REC-TYPE             PIC X(02)  VALUE SPACES.     00016800
016900                                                                  00016900
017000 01  WS-HOLD-GRPSPEC-KEY.                                         00017000
017100     05  WS-GRP-STATUS-CODE          PIC X(01)  VALUE SPACES.     00017100
017200     05  FILLER                      PIC X(02)  VALUE SPACES.     00017200
017300     05  WS-GRP-PLAN-CODE            PIC X(03)  VALUE SPACES.     00017300
017400     05  FILLER                      PIC X(01)  VALUE SPACES.     00017400
017500     05  WS-GRP-GROUP-NUM            PIC X(09)  VALUE SPACES.     00017500
017600     05  FILLER                      PIC X(01)  VALUE SPACES.     00017600
017700     05  WS-GRP-SECTION-NUM          PIC X(05)  VALUE SPACES.     00017700
017800     05  FILLER                      PIC X(01)  VALUE SPACES.     00017800
017900     05  WS-GRP-PKG-CODE             PIC X(03)  VALUE SPACES.     00017900
018000     05  FILLER                      PIC X(01)  VALUE SPACES.     00018000
018100     05  WS-GRP-FAM                  PIC X(02)  VALUE SPACES.     00018100
018200     05  FILLER                      PIC X(01)  VALUE SPACES.     00018200
018300     05  WS-GRP-EFFDT-CEN            PIC 9(07)  VALUE ZEROES.     00018300
018400     05  FILLER                      PIC X(02)  VALUE SPACES.     00018400
018500     05  WS-GRP-REC-TYPE             PIC X(02)  VALUE SPACES.     00018500
018600/                                                                 00018600
018700 LINKAGE SECTION.                                                 00018700
018800                                                                  00018800
018900 PROCEDURE DIVISION.                                              00018900
019000 0000-MAINLINE.                                                   00019000
019100                                                                  00019100
019200     OPEN INPUT  UDR-FILE                                         00019200
019300          INPUT  C9-G9-FILES.                                     00019300
019400                                                                  00019400
019500     MOVE 'S'                    TO 1-REQUEST-TYPE.               00019500
019600     MOVE 8                      TO SET-REC-LENG.                 00019600
019700     MOVE 3                      TO SET-VALUE.                    00019700
019800     CALL 'TSGVSAM1' USING PARM-1 PARM-SET.                       00019800
019900     IF  1-REQUEST-TYPE NOT EQUAL 'S'                             00019900
020000         MOVE SET-FEEDBACK       TO ABEND-CODE                    00020000
020100         GO TO 0900-ERROR-RTN.                                    00020100
020200                                                                  00020200
020300     MOVE 'O'                    TO 1-REQUEST-TYPE.               00020300
020400     CALL 'TSGVSAM1' USING PARM-1 PARM-1A.                        00020400
020500     IF  1-REQUEST-TYPE NOT EQUAL 'O'                             00020500
020600         MOVE 1A-FEEDBACK        TO ABEND-CODE                    00020600
020700         GO TO 0900-ERROR-RTN.                                    00020700
020800                                                                  00020800
020900                                                                  00020900
021000     PERFORM 0010-PROCESS THRU 0010-EXIT                          00021000
021100         UNTIL EOF-UDR.                                           00021100
021200                                                                  00021200
021300     PERFORM 0050-PROCESS-C9-G9-RECORDS THRU 0050-EXIT            00021300
021400       UNTIL WS-C9-G9-EOF-SW-ON.                                  00021400
021500                                                                  00021500
021600                                                                  00021600
021700     CLOSE    UDR-FILE                                            00021700
021800              C9-G9-FILES.                                        00021800
021900                                                                  00021900
022000     MOVE 'C'                    TO 1-REQUEST-TYPE.               00022000
022100     CALL 'TSGVSAM1' USING PARM-1 PARM-1A.                        00022100
022200     IF  1-REQUEST-TYPE NOT EQUAL 'C'                             00022200
022300         MOVE 1A-FEEDBACK        TO ABEND-CODE                    00022300
022400         GO TO 0900-ERROR-RTN.                                    00022400
022500                                                                  00022500
022600     GOBACK.                                                      00022600
022700/                                                                 00022700
022800 0010-PROCESS.                                                    00022800
022900                                                                  00022900
023000     READ UDR-FILE                                                00023000
023100         AT END                                                   00023100
023200             MOVE HIGH-VALUES    TO UDR-EOF-SW                    00023200
023300             GO TO 0010-EXIT.                                     00023300
023400                                                                  00023400
023500     IF WRK-STATUS-CODE   =  'C'                                  00023500
023600        MOVE WRK-STATUS-CODE   TO  WS-CON-STATUS-CODE             00023600
023700        MOVE WRK-PLAN-CODE     TO  WS-CON-PLAN-CODE               00023700
023800        MOVE WRK-GROUP-NUM     TO  WS-CON-GROUP-NUM               00023800
023900        MOVE WRK-SECTION-NUM   TO  WS-CON-SECTION-NUM             00023900
024000        MOVE WRK-PKG-CODE      TO  WS-CON-PKG-CODE                00024000
024100        MOVE WRK-L-O-B         TO  WS-CON-LOB                     00024100
024200        MOVE WRK-PROV-CTL      TO  WS-CON-PRV                     00024200
024300        MOVE WRK-FAM-REL-LEVEL TO  WS-CON-FAM                     00024300
024400        MOVE WRK-EFFDT-CEN     TO  WS-CON-EFFDT-CEN               00024400
024500        MOVE WRK-REC-TYPE      TO  WS-CON-REC-TYPE                00024500
024600     ELSE                                                         00024600
024700     IF WRK-STATUS-CODE   =  'G'                                  00024700
024800        MOVE WRK-STATUS-CODE   TO  WS-GRP-STATUS-CODE             00024800
024900        MOVE WRK-PLAN-CODE     TO  WS-GRP-PLAN-CODE               00024900
025000        MOVE WRK-GROUP-NUM     TO  WS-GRP-GROUP-NUM               00025000
025100        MOVE WRK-SECTION-NUM   TO  WS-GRP-SECTION-NUM             00025100
025200        MOVE WRK-PKG-CODE      TO  WS-GRP-PKG-CODE                00025200
025300        MOVE WRK-FAM-REL-LEVEL TO  WS-GRP-FAM                     00025300
025400        MOVE WRK-EFFDT-CEN     TO  WS-GRP-EFFDT-CEN               00025400
025500        MOVE WRK-REC-TYPE      TO  WS-GRP-REC-TYPE                00025500
025600     ELSE                                                         00025600
025700     MOVE WRK-STATUS-CODE   TO  WS-STATUS-CODE                    00025700
025800     MOVE WRK-PLAN-CODE     TO  WS-PLAN-CODE                      00025800
025900     MOVE WRK-GROUP-NUM     TO  WS-GROUP-NUM                      00025900
026000     MOVE WRK-SECTION-NUM   TO  WS-SECTION-NUM                    00026000
026100     MOVE WRK-PKG-CODE      TO  WS-PKG-CODE                       00026100
026200     MOVE WRK-L-O-B         TO  WS-LOB                            00026200
026300     MOVE WRK-PROV-CTL      TO  WS-PRV                            00026300
026400     MOVE WRK-FAM-REL-LEVEL TO  WS-FAM                            00026400
026500     MOVE WRK-EFFDT-CEN     TO  WS-EFFDT-CEN                      00026500
026600     MOVE WRK-REC-TYPE      TO  WS-REC-TYPE.                      00026600
026700                                                                  00026700
026800                                                                  00026800
026900     MOVE 'E'                    TO 1-REQUEST-TYPE.               00026900
027000     MOVE 61                     TO 1A-REC-LENG.                  00027000
027100     MOVE WORK-RECORD-KEY        TO 1A-KEY.                       00027100
027200                                                                  00027200
027300     CALL 'TSGVSAM1' USING PARM-1 PARM-1A.                        00027300
027400                                                                  00027400
027500     IF 1-REQUEST-TYPE   EQUAL   '3'                              00027500
027600        MOVE 1010   TO  ABEND-CODE                                00027600
027700        IF WRK-STATUS-CODE   =  'C'                               00027700
027800           DISPLAY ' '                                            00027800
027900           DISPLAY ' THIS WORK RECORD NOT ON THE WORKFILE '       00027900
028000           DISPLAY ' CONTRACT WORK RECORD   =  '                  00028000
028100                                                 WS-HOLD-CON-KEY  00028100
028200           GO TO 0900-ERROR-RTN                                   00028200
028300        ELSE                                                      00028300
028400        IF WRK-STATUS-CODE   =  'G'                               00028400
028500           DISPLAY ' '                                            00028500
028600           DISPLAY ' THIS WORK RECORD NOT ON THE WORKFILE '       00028600
028700           DISPLAY ' GROUP SPECIFIC WORK RECORD  =  '             00028700
028800                                             WS-HOLD-GRPSPEC-KEY  00028800
028900           GO TO 0900-ERROR-RTN                                   00028900
029000        ELSE                                                      00029000
029100        DISPLAY ' '                                               00029100
029200        DISPLAY ' THIS WORK RECORD NOT ON THE WORKFILE '          00029200
029300        DISPLAY ' WORK RECORD KEY    =  '  WS-HOLD-KEY            00029300
029400        GO TO 0900-ERROR-RTN.                                     00029400
029500                                                                  00029500
029600                                                                  00029600
029700     IF 1-REQUEST-TYPE NOT EQUAL 'E'                              00029700
029800        MOVE 1A-FEEDBACK   TO ABEND-CODE                          00029800
029900        IF WRK-STATUS-CODE   =  'C'                               00029900
030000           DISPLAY ' '                                            00030000
030100           DISPLAY ' THIS WORK RECORD NOT ON THE WORKFILE '       00030100
030200           DISPLAY ' CONTRACT WORK RECORD   =  '                  00030200
030300                                                 WS-HOLD-CON-KEY  00030300
030400           GO TO 0900-ERROR-RTN                                   00030400
030500        ELSE                                                      00030500
030600        IF WRK-STATUS-CODE   =  'G'                               00030600
030700           DISPLAY ' '                                            00030700
030800           DISPLAY ' THIS WORK RECORD NOT ON THE WORKFILE '       00030800
030900           DISPLAY ' GROUP SPECIFIC WORK RECORD  =  '             00030900
031000                                             WS-HOLD-GRPSPEC-KEY  00031000
031100           GO TO 0900-ERROR-RTN                                   00031100
031200        ELSE                                                      00031200
031300        DISPLAY ' '                                               00031300
031400        DISPLAY ' THIS WORK RECORD NOT ON THE WORKFILE '          00031400
031500        DISPLAY ' WORK RECORD KEY    =  '  WS-HOLD-KEY            00031500
031600        GO TO 0900-ERROR-RTN.                                     00031600
031700                                                                  00031700
031800 0010-EXIT.                                                       00031800
031900     EXIT.                                                        00031900
032000/                                                                 00032000
032100 0050-PROCESS-C9-G9-RECORDS.                                      00032100
032200                                                                  00032200
032300     READ C9-G9-FILES                                             00032300
032400       AT END                                                     00032400
032500         MOVE '1'  TO  WS-C9-G9-EOF-SW                            00032500
032600         GO TO 0050-EXIT.                                         00032600
032700                                                                  00032700
032800     IF WRK2-STATUS-CODE   =  'C'                                 00032800
032900        MOVE WRK2-STATUS-CODE   TO  WS-CON-STATUS-CODE            00032900
033000        MOVE WRK2-PLAN-CODE     TO  WS-CON-PLAN-CODE              00033000
033100        MOVE WRK2-GROUP-NUM     TO  WS-CON-GROUP-NUM              00033100
033200        MOVE WRK2-SECTION-NUM   TO  WS-CON-SECTION-NUM            00033200
033300        MOVE WRK2-PKG-CODE      TO  WS-CON-PKG-CODE               00033300
033400        MOVE WRK2-L-O-B         TO  WS-CON-LOB                    00033400
033500        MOVE WRK2-PROV-CTL      TO  WS-CON-PRV                    00033500
033600        MOVE WRK2-FAM-REL-LEVEL TO  WS-CON-FAM                    00033600
033700        MOVE WRK2-EFFDT-CEN     TO  WS-CON-EFFDT-CEN              00033700
033800        MOVE WRK2-REC-TYPE      TO  WS-CON-REC-TYPE               00033800
033900     ELSE                                                         00033900
034000     IF WRK2-STATUS-CODE   =  'G'                                 00034000
034100        MOVE WRK2-STATUS-CODE   TO  WS-GRP-STATUS-CODE            00034100
034200        MOVE WRK2-PLAN-CODE     TO  WS-GRP-PLAN-CODE              00034200
034300        MOVE WRK2-GROUP-NUM     TO  WS-GRP-GROUP-NUM              00034300
034400        MOVE WRK2-SECTION-NUM   TO  WS-GRP-SECTION-NUM            00034400
034500        MOVE WRK2-PKG-CODE      TO  WS-GRP-PKG-CODE               00034500
034600        MOVE WRK2-FAM-REL-LEVEL TO  WS-GRP-FAM                    00034600
034700        MOVE WRK2-EFFDT-CEN     TO  WS-GRP-EFFDT-CEN              00034700
034800        MOVE WRK2-REC-TYPE      TO  WS-GRP-REC-TYPE               00034800
034900     ELSE                                                         00034900
035000     MOVE WRK2-STATUS-CODE   TO  WS-STATUS-CODE                   00035000
035100     MOVE WRK2-PLAN-CODE     TO  WS-PLAN-CODE                     00035100
035200     MOVE WRK2-GROUP-NUM     TO  WS-GROUP-NUM                     00035200
035300     MOVE WRK2-SECTION-NUM   TO  WS-SECTION-NUM                   00035300
035400     MOVE WRK2-PKG-CODE      TO  WS-PKG-CODE                      00035400
035500     MOVE WRK2-L-O-B         TO  WS-LOB                           00035500
035600     MOVE WRK2-PROV-CTL      TO  WS-PRV                           00035600
035700     MOVE WRK2-FAM-REL-LEVEL TO  WS-FAM                           00035700
035800     MOVE WRK2-EFFDT-CEN     TO  WS-EFFDT-CEN                     00035800
035900     MOVE WRK2-REC-TYPE      TO  WS-REC-TYPE.                     00035900
036000                                                                  00036000
036100                                                                  00036100
036200     MOVE 'E'                    TO  1-REQUEST-TYPE.              00036200
036300     MOVE 61                     TO  1A-REC-LENG.                 00036300
036400     MOVE WORK-RECORD-KEY-2      TO  1A-KEY.                      00036400
036500                                                                  00036500
036600     CALL 'TSGVSAM1' USING   PARM-1   PARM-1A.                    00036600
036700                                                                  00036700
036800     IF 1-REQUEST-TYPE   EQUAL    '3'                             00036800
036900        MOVE  0050  TO  ABEND-CODE                                00036900
037000        IF WRK2-STATUS-CODE   =  'C'                              00037000
037100           DISPLAY ' '                                            00037100
037200           DISPLAY ' THIS C9 WORK RECORD NOT ON THE WORKFILE '    00037200
037300           DISPLAY ' CONTRACT AUDIT WORK RECORD   =  '            00037300
037400                                                 WS-HOLD-CON-KEY  00037400
037500           GO TO 0900-ERROR-RTN                                   00037500
037600        ELSE                                                      00037600
037700        IF WRK2-STATUS-CODE   =  'G'                              00037700
037800           DISPLAY ' '                                            00037800
037900           DISPLAY ' THIS G9 WORK RECORD NOT ON THE WORKFILE '    00037900
038000           DISPLAY ' GROUP SPECIFIC AUDIT WORK RECORD  =  '       00038000
038100                                             WS-HOLD-GRPSPEC-KEY  00038100
038200           GO TO 0900-ERROR-RTN                                   00038200
038300        ELSE                                                      00038300
038400        DISPLAY ' '                                               00038400
038500        DISPLAY ' THIS WORK RECORD NOT ON THE WORKFILE '          00038500
038600        DISPLAY ' WORK RECORD KEY    =  '  WS-HOLD-KEY            00038600
038700        GO TO 0900-ERROR-RTN.                                     00038700
038800                                                                  00038800
038900                                                                  00038900
039000     IF  1-REQUEST-TYPE   NOT EQUAL  'E'                          00039000
039100         MOVE 1A-FEEDBACK   TO  ABEND-CODE                        00039100
039200         IF WRK2-REC-TYPE   =  'C9'                               00039200
039300            DISPLAY ' '                                           00039300
039400            DISPLAY ' CONTRACT AUDIT WORK RECORD   =  '           00039400
039500                                                  WS-HOLD-CON-KEY 00039500
039600            GO TO 0900-ERROR-RTN                                  00039600
039700         ELSE                                                     00039700
039800         IF WRK2-REC-TYPE   =  'G9'                               00039800
039900            DISPLAY ' '                                           00039900
040000            DISPLAY ' GROUP SPECIFIC AUDIT WORK RECORD  =  '      00040000
040100                                              WS-HOLD-GRPSPEC-KEY 00040100
040200            GO TO 0900-ERROR-RTN                                  00040200
040300        ELSE                                                      00040300
040400        DISPLAY ' '                                               00040400
040500        DISPLAY ' THIS WORK RECORD NOT ON THE WORKFILE '          00040500
040600        DISPLAY ' WORK RECORD KEY    =  '  WS-HOLD-KEY            00040600
040700        GO TO 0900-ERROR-RTN.                                     00040700
040800                                                                  00040800
040900 0050-EXIT.                                                       00040900
041000     EXIT.                                                        00041000
041100/                                                                 00041100
041200 0900-ERROR-RTN.                                                  00041200
041300                                                                  00041300
041400     CALL 'TSGEND' USING ABEND-CODE.                              00041400
041500                                                                  00041500
041600 0900-EXIT.                                                       00041600
041700     EXIT.                                                        00041700
