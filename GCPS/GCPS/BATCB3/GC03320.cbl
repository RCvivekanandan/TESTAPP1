000100 IDENTIFICATION DIVISION.                                         00000100
000200                                                                  00000200
000300 PROGRAM-ID.     GC03320.                                         00000300
000400 AUTHOR.         NABIL ELBAZ.                                     00000400
000500 INSTALLATION.   HCSC-HCMS.                                       00000500
000600 DATE-WRITTEN.                                                    00000600
000700 DATE-COMPILED.                                                   00000700
000800***************************************************************** 00000800
000900*    THIS PROGRAM WILL ARCHIVE CHANGES TO GROUP SPECIFIC        * 00000900
001000*    DATA ELEMENTS TO THE ARCHIVING FILE. THE LOGIC WILL        * 00001000
001100*    COMPARE THE BEFOR IMAGE FILE TO THE AFTER IMAGE FILE AND   * 00001100
001200*    THE CHANGED ELEMENT FROM THE BEFOR IMAGE FILE TO THE       * 00001200
001300*    ARCHIVING FILE.                                            * 00001300
001400*                                                               * 00001400
001500*    RGS-FILE IS RLSE GROUP SPECIFIC FILE (AFTER IMAGE)         * 00001500
001600*    BIM-FILE IS BEFORE IMAGE FILE                              * 00001600
001700*    TSGVSAM1 IS PROVISION TABULAR FILE >>>>>>>>> NOT USED      * 00001700
001800*    TSGVSAM3 IS GCPS ARCHIVED FILE (OUTPUT).                   * 00001800
001900*                                                               * 00001900
002000***************************************************************** 00002000
002100*    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        * 00002100
002200*    *-*         U P D A T E   H I S T O R Y         *-*        * 00002200
002300*    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        * 00002300
002400*                                                               * 00002400
002500**-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* 00002500
002600*                                                               * 00002600
002700*  D0168     05/10/88  NE   INITIAL PROGRAM BUILD               * 00002700
002800*  D170      01/27/89  NE   ADD A NEW LOGIC TO ARCHIVE THE      * 00002800
002900*                           AUDIT INFO IN G9 W/E RECORD         * 00002900
003000*  D???      07/26/89  NE   MOVE ATB INDICATOR VALUE FOR ATB-3  * 00003000
003100*  D247    05/18/90  NE  ADD TWO NEW FIELDS, GCG-POS-PARTICP-IND* 00003100
003200*                        AND GCG-POS-PENALTY-DATE FOR THE NEW   * 00003200
003300*                       POINT OF SERVICE #GCCP TABULAR. INCREASE* 00003300
003400*                        TOTAL OCCURS FROM 172 TO 174.          * 00003400
003500*  D258    06/28/90 ENW  ADDED GCG-COMB-COST-CONTAIN-PGM-IND.   * 00003500
003600*                        INCREASED OCCURS FROM 174 TO 175.      * 00003600
003700*                                                               * 00003700
003800*  D249.01  09/25/90 APH    1. MOVE GCG-POS-PARTICP-IND AND     * 00003800
003900*                              GCG-POS-PENALTY-DATE TO THE END  * 00003900
004000*                              OF THE TABLE (COMP-GRP-SPEC-     * 00004000
004100*                              BEFORE-AREA).                    * 00004100
004200*                           2. MOVE GCG2-POS-PARTICP-IND AND    * 00004200
004300*                              GCG2-POS-PENALTY-DATE TO THE END * 00004300
004400*                              OF THE TABLE (COMP-GRP-SPEC-     * 00004400
004500*                              AFTER-AREA).                     * 00004500
004600*                           3. ADD BACK 4 FIELDS FOR INTER RELA-* 00004600
004700*                              TIONAL CODES TO BOTH TABLES.     * 00004700
004800*                           4. CHANGE ENTRY COUNTER FROM 175 TO * 00004800
004900*                              179 FOR BOTH TABLES.             * 00004900
005000* !!!!!!!  ATTENTION  !!!!  5. ALL NEW DATA ELEMENTS MUST \
005100*                              BE ADDED TO THE ENDED OF THE     * 00005100
005200*                              TABLE.  IF NOT, IT WILL CAUSE    * 00005200
005300*                              INDEX OFF WHILE DECIDING ARCHIVING 00005300
005400*                              AFTER IMAGE.  THE PROGRAM DOES   * 00005400
005500*                              NOT LOOK FOR A MATCH ON DATA ID. * 00005500
005600*  11161   11/05/90 PFH  REPLACED BEFORE/AFTER IMAGES OF GROUP  * 00005600
005700*                        SPECIFIC RECORD WITH COPYBOOKS.        * 00005700
005800*                        INCLUDED EXPANSION OF GROUP SPECIFIC   * 00005800
005900*                        INDICATORS AND RELATED PENALTY DATES.  * 00005900
006000*  11697   03/08/91 PFH  REPLACED HARD CODED OCCURS WITH A 05   * 00006000
006100*                        LEVEL COUNTER IN THE COPYBOOKS.        * 00006100
006200*                        CHANGED ALL MOVES TO 01 LEVEL BEFORE/  * 00006200
006300*                        AFTER COPYBOOKS TO MOVES TO THEIR      * 00006300
006400*                        RESPECTIVE 05 LEVELS.                  * 00006400
006500*  D12009  09/10/91 GDM  INCREASE K-F-R TO 2 POSITIONS          * 00006500
006600*                                                                 00006600
006700*  09/19/91    TPM    EXPANSION OF THE FAMILY-RELATION FIELD.    *00006700
006800*  D12009             REDUCE THE DATA-AREA-A BY ONE BYTE TO      *00006800
006900*                     TO ACCOMODATE  FOR THE ABOVE CHANGE.       *00006900
007000*                     REMOVE HARD CODED RECORD LENGTHS FOR       *00007000
007100*                     ARCHIVE AND INSERTED COPYBOOK MEMBER       *00007100
007200*                     GCCDRLEN  TO BE USED TO INCLUDE NEW RECORD *00007200
007300*                     LENGTHS TO HANDLE THE EXPANSION IN THE     *00007300
007400*                     FAMILY RELATION FIELD.                     *00007400
007500*                                                                *00007500
007600*                     CHANGED THE RECORD LENGTH FROM 30 TO 31    *00007600
007700*                     WHEN CALLING THE TSGVSAM ROUTINE.          *00007700
007800*                                                                *00007800
007900* D12009  10-07-91  FRY   MODIFIED:                              *00007900
008000*                         FROM:   REC-AREA-A    PIC X(8099).     *00008000
008100*                           TO:   REC-AREA-A    PIC X(8076).     *00008100
008200*                         FROM:   DATA-AREA-A   PIC X(8069).     *00008200
008300*                           TO:   DATA-AREA-A   PIC X(8046).     *00008300
008400*                                                                *00008400
008500* P038    11-21-91  FRY   MODIFIED:                              *00008500
008600*                             FROM:  A-GRPSPEC-ID     PIC X(14)  *00008600
008700*                               TO:  A-GRPSPEC-ID     PIC X(15)  *00008700
008800*                             FROM:  B-GRPSPEC-ID     PIC X(14)  *00008800
008900*                               TO:  B-GRPSPEC-ID     PIC X(15)  *00008900
009000*                                                                *00009000
009100*          1/17/95  EMS   CONVERTED TO COBOL II.                 *00009100
009200*                                                                *00009200
009300*         11/06/96  MAM   (G&R) ADDED THE QUALITY CONTROL FILE   *00009300
009400*                         AND THE LOGIC THAT ENABLES IT TO BE    *00009400
009500*                         WRITTEN TO.                            *00009500
009600*                                                                *00009600
009700*         11/15/97  PHF   MILLENIUM CONVERSION                   *00009700
009800*                         THE COMPILE WILL HAVE A CC = 4 DUE TO  *00009800
009900*                         THE CORRESPONDING MOVES                *00009900
010000*                                                                *00010000
010100* 14726/  12/30/97  GSP   ADDED SEPARATE MOVES FOR PORTABILITY   *00010100
010200* 15057                   PREEXIST DATE AND MENTAL HEALTH PARITY *00010200
010300*                         DATE FOLLOWING \
010400*                                                                *00010400
010500* 14726/  01/08/98  GSP   CHANGED RECORD LENGTH FROM 31 TO 42    *00010500
010600* 15057                   FOR USING TSGVSAM FOR ARCHIVE FILE.    *00010600
010700*                         CORRECTED INITIALIZATION OF QCF RECORD.*00010700
010800*                         (ZEROS WERE COMING OUT IN THE KEY OF   *00010800
010900*                         THE OUTPUT FILE.)                      *00010900
011000*                                                                *00011000
011100*         03-11-98  GDM   MODIFIED:                              *00011100
011200*                             FROM:  A-GRPSPEC-ID     PIC X(15)  *00011200
011300*                               TO:  A-GRPSPEC-ID     PIC X(26)  *00011300
011400*                             FROM:  B-GRPSPEC-ID     PIC X(15)  *00011400
011500*                               TO:  B-GRPSPEC-ID     PIC X(26)  *00011500
011600*                                                                *00011600
011700* 15380      05/05/99  GDM  MODIFY TO INCLUDE #GBAE INQUIRY      *00011700
011800*                                                                *00011800
011900*            03/14/00  GSP  MOVED THE SETTING OF GCG-A-INDEX     *00011900
012000*                           TO THE BEGINNING OF PARAGRAPH        *00012000
012100*                           0070-PROCESS-ARCH-GRPSP TO CORRECT   *00012100
012200*                           INDEXING PROBLEM.                    *00012200
012300*                                                                *00012300
012400* D-353      10/12/00   JP  ADDED SEP MOVE OF HMO PENALTY DATE   *00012400
012500*                           (WAS NOT MOVED BY 'MOVE CORR')       *00012500
012600*                                                                *00012600
012700*                                                                *00012700
012800* D-356/357  05/24/01   JP  ADDED SEP MOVES OF BC/BS/MM ITS      *00012800
012900*                           NONPAR PRICE INDICATOR FIELDS        *00012900
013000*                           (NOT MOVED BY 'MOVE CORR')           *00013000
013100*                                                                *00013100
013200*            07/24/02   JP  FIX - ADD MOVE OF BAE PENALTY DATE   *00013200
013300*                                                                *00013300
013400* P01760     09/24/02  AKK  ADD OPID                             *00013400
013500*                                                                *00013500
013600* P01760     10/11/02  AKK  COPOOK CHANGED, REGEN                *00013600
013700*                                                                *00013700
013800* D-374      04/01/03  GTF  ADDED SEP MOVES TO COPY THE FOLLOWING*00013800
013900*                           FIELDS FROM GROUP SPEC TO ARCHIVE:   *00013900
014000*                           GCG-CONS-DRVN-PENLTY-DT-CEN          *00014000
014100*                                                                *00014100
014200*  02-28-07     DAF       CHANGE REC-AREA-A LENGTH TO MATCH THE  *00014200
014300*                         CORRECT LENGTH OF THE 2002 CONVERSION  *00014300
014400*                         ADDED CHECK IF GOING PAST THE LIMIT    *00014400
014500*                         OF OCCURRENCES                         *00014500
014600*                                                                *00014600
014700* DM9400     05/30/07  LR   RECOMPILE FOR CHANGED COPYBOOKS      *00014700
014800*                                                                *00014800
014700* P21681     10/25/17  SRI  RECOMPILE FOR CHANGED COPYBOOKS      *00014810
014800*                           GCGROUPC GCGROUP2 GCARCHGS           *00014820
014800*                           GCAR320A GCAR320B                    *00014830
014700* P22845     07/18/18  SRI  RECOMPILE FOR CHANGED COPYBOOKS      *00014840
014800*                           GCGROUPC GCGROUP2 GCARCHGS           *00014850
014800*                           GCAR320A GCAR320B                    *00014860
TM0526*BBDA-66049  05/1/2026 RECOMPILE FOR A COPYBOOK CHANGES FROM     *00014870
TM0526*                                  GCAUDITC                      *00014880
014900******************************************************************00014900
015000/                                                                 00015000
015100 ENVIRONMENT DIVISION.                                            00015100
015200                                                                  00015200
015300 CONFIGURATION SECTION.                                           00015300
015400 SOURCE-COMPUTER. IBM-370.                                        00015400
015500 OBJECT-COMPUTER. IBM-370.                                        00015500
015600                                                                  00015600
015700                                                                  00015700
015800 INPUT-OUTPUT SECTION.                                            00015800
015900 FILE-CONTROL.                                                    00015900
016000*MAM G&R - ADDED QCF-FILE                                         00016000
016100     SELECT RGS-FILE   ASSIGN TO UT-S-GC03320A.                   00016100
016200     SELECT BIM-FILE   ASSIGN TO UT-S-GC03320B.                   00016200
016300     SELECT AUD-FILE   ASSIGN TO UT-S-GC03320C.                   00016300
016400     SELECT QCF-FILE   ASSIGN TO UT-S-GC03320D.                   00016400
016500/                                                                 00016500
016600 DATA DIVISION.                                                   00016600
016700 FILE SECTION.                                                    00016700
016800 FD  BIM-FILE                                                     00016800
016900     LABEL RECORDS ARE STANDARD                                   00016900
017000     RECORDING MODE IS V                                          00017000
017100     BLOCK CONTAINS 0 RECORDS.                                    00017100
017200 01  BIM-RECORD.                                                  00017200
017300     COPY GCWRKDCC.                                               00017300
017400     COPY GCGROUPC.                                               00017400
017500/                                                                 00017500
017600 FD  RGS-FILE                                                     00017600
017700     LABEL RECORDS ARE STANDARD                                   00017700
017800     RECORDING MODE IS V                                          00017800
017900     BLOCK CONTAINS 0 RECORDS.                                    00017900
018000 01  RGS-RECORD.                                                  00018000
018100     COPY GCWRKDC2.                                               00018100
018200     COPY GCGROUP2.                                               00018200
018300                                                                  00018300
018400 FD  AUD-FILE                                                     00018400
018500     LABEL RECORDS ARE STANDARD                                   00018500
018600     RECORDING MODE IS V                                          00018600
018700     BLOCK CONTAINS 0 RECORDS.                                    00018700
018800 01  AUD-RECORD.                                                  00018800
018900     COPY GCWRKDC3.                                               00018900
019000     COPY GCAUDITC.                                               00019000
019100                                                                  00019100
019200*MAM G&R - ADDED FD FOR QCF                                       00019200
019300 FD  QCF-FILE                                                     00019300
019400     LABEL RECORDS ARE STANDARD                                   00019400
019500     RECORDING MODE IS F                                          00019500
019600     BLOCK CONTAINS 0 RECORDS.                                    00019600
019700 01  QCF-RECORD.                                                  00019700
019800     COPY GCQCF.                                                  00019800
019900/                                                                 00019900
020000 WORKING-STORAGE SECTION.                                         00020000
020100 01  FILLER                         PIC X(42)   VALUE             00020100
020200     '***GC03320 WORKING STORAGE BEGINS HERE***'.                 00020200
020300                                                                  00020300
020400 01  ABEND-CODE                     PIC 9(4)    COMP.             00020400
020500 01  WS-OPER-ID                     PIC X(08) VALUE SPACES.       00020500
020600 01  RPT-IND    VALUE '3320'  PIC X(4).                           00020600
020700                                                                  00020700
020800     COPY MLDATE01.                                               00020800
020900 01  JUL-DATE.                                                    00020900
021000     05  JUL-DT.                                                  00021000
021100         10  JUL-CC                 PIC 99.                       00021100
021200         10  JUL-YY                 PIC 99.                       00021200
021300         10  JUL-DD                 PIC 999.                      00021300
021400     05  TODAYS-DATE REDEFINES  JUL-DT PIC 9(7).                  00021400
021500                                                                  00021500
021600*    COPY HSCDATES.                                               00021600
021700*01  JUL-DATE.                                                    00021700
021800*    05  JUL-DT.                                                  00021800
021900*        10  JUL-YY                 PIC 99.                       00021900
022000*        10  JUL-DD                 PIC 999.                      00022000
022100*    05  TODAYS-DATE REDEFINES  JUL-DT PIC 9(5).                  00022100
022200                                                                  00022200
022300 01  DATE-AREA.                                                   00022300
022400*    05  GREG-DATE                  PIC 9(6) VALUE ZEROS.         00022400
022500     05  GREG-DATE                  PIC 9(8) VALUE ZEROS.         00022500
022600*    05  JULIAN-DATE                PIC 9(5) VALUE ZEROS.         00022600
022700     05  JULIAN-DATE                PIC 9(7) VALUE ZEROS.         00022700
022800*    05  WS-CHNG-DATE               PIC 9(5) VALUE ZEROS.         00022800
022900     05  WS-CHNG-DATE               PIC 9(7) VALUE ZEROS.         00022900
023000                                                                  00023000
023100 01  END-OF-FILE-C-SW               PIC XXX  VALUE SPACES.        00023100
023200     88  END-OF-AUD-FILE                     VALUE 'END'.         00023200
023300                                                                  00023300
023400 01  END-OF-FILE-B-SW               PIC XXX  VALUE SPACES.        00023400
023500     88  END-OF-BIM-FILE                     VALUE 'END'.         00023500
023600                                                                  00023600
023700 01  END-OF-FILE-A-SW               PIC XXX  VALUE SPACES.        00023700
023800     88  END-OF-RGS-FILE                     VALUE 'END'.         00023800
023900                                                                  00023900
024000 01  READ-IND                       PIC XXX VALUE SPACES.         00024000
024100     88  AFTER-FOUND                         VALUE 'YES'.         00024100
024200                                                                  00024200
024300 01  TAB-POINTER-SW                 PIC XXX  VALUE SPACES.        00024300
024400     88  TAB-POINTERS-END                    VALUE 'END'.         00024400
024500                                                                  00024500
024600*MAM G&R - ADDED A SWITCH TO CHECK INTER REL CD                   00024600
024700 01  WS-INTER-REL-SW                PIC X    VALUE 'Y'.           00024700
024800     88  INTER-REL-CD-FND                    VALUE 'Y'.           00024800
024900     88  INTER-REL-CD-NTFND                  VALUE 'N'.           00024900
025000                                                                  00025000
025100 01  WS-AUD-ID.                                                   00025100
025200     05  WS-AUD-PRE                 PIC XX   VALUE 'AU'.          00025200
025300     05  WS-AUD-FUNC                PIC X(6).                     00025300
025400                                                                  00025400
025500 01  AREA-A.                                                      00025500
025600     05  AFTER-ID                   PIC X(6).                     00025600
025700     05  AFTER-SLOT     COMP-3      PIC S9(7).                    00025700
025800                                                                  00025800
025900 01  AREA-B.                                                      00025900
026000     05  BEFORE-ID                  PIC X(6).                     00026000
026100     05  BEFORE-SLOT    COMP-3      PIC S9(7).                    00026100
026200                                                                  00026200
026300                                                                  00026300
026400*** THE FOLLOWING AREA IS THE ARCHIVING FILE I/O ****             00026400
026500 01  PARM-ONE-A.                                                  00026500
026600     05  RESERVED-FLDS-A         PIC 9(8)    VALUE ZEROS COMP.    00026600
026700     05  RESERVED-ONE-A  REDEFINES RESERVED-FLDS-A.               00026700
026800         10  REQUEST-TYPE-A      PIC X.                           00026800
026900         10  FILLER              PIC X(3).                        00026900
027000                                                                  00027000
027100 01  PARM-TWO-A.                                                  00027100
027200     02  RDW-A.                                                   00027200
027300         05  RECORD-LENGTH-A     PIC 9(4)    VALUE ZEROS COMP.    00027300
027400         05  FEEDBACK-CODE-A     PIC 9(4)    VALUE ZEROS COMP.    00027400
027500     02  REC-AREA-A              PIC X(9547).                     00027500
027600     02  RECORD-A REDEFINES REC-AREA-A.                           00027600
027700         10  KEY-FIELD-A.                                         00027700
027800             15  KEY-TYPE-A      PIC X.                           00027800
027900             15  KEY-ID-A.                                        00027900
028000                 20  K-P-N       PIC X(03).                       00028000
028100                 20  K-G-N       PIC X(09).                       00028100
028200                 20  K-S-N       PIC X(05).                       00028200
028300                 20  K-PKG       PIC X(03).                       00028300
028400                 20  K-LOB       PIC X.                           00028400
028500                 20  K-P-C       PIC XX.                          00028500
028600                 20  K-F-R       PIC XX.                          00028600
028700                 20  K-E-DT      PIC S9(7) COMP-3.                00028700
028800             15  KEY-NO-A        PIC X(8).                        00028800
028900         10  PNTRS-COUNT-A       PIC S9(5) COMP-3.                00028900
029000         10  DATA-AREA-A         PIC X(9506).                     00029000
029100                                                                  00029100
029200 01  PARM-SET.                                                    00029200
029300     05  SET-RDW.                                                 00029300
029400         10  SET-RECORD-LENGTH      PIC 9(4) VALUE ZEROS  COMP.   00029400
029500         10  SET-FEEDBACK           PIC 9(4) VALUE ZEROS  COMP.   00029500
029600     05  SET-VALUE                  PIC 9(8) VALUE ZEROS  COMP.   00029600
029700                                                                  00029700
029800                                                                  00029800
029900 01  TAB-ARCH-DE-CD.                                              00029900
030000     05  TB-ARCH-DE-1                PIC XX  VALUE 'TT'.          00030000
030100     05  TB-ARCH-DE-2                PIC X(6).                    00030100
030200                                                                  00030200
030300 01  PRINT-TOTALS.                                                00030300
030400     05  ARCH-PRINT-1                PIC ZZZZZZZ99.               00030400
030500     05  ARCH-PRINT-2                PIC ZZZZZZZ99.               00030500
030600     05  QCF-PRINT                   PIC ZZZZZZZ99.               00030600
030700     05  BYPASS-PRINT                PIC ZZZZZZZ99.               00030700
030800                                                                  00030800
030900 01  COUNTER-AREA.                                                00030900
031000     05  WS-QCF-COUNT                 PIC S9(9)  VALUE ZEROS.     00031000
031100     05  WS-BYPASS-CNT                PIC S9(9)  VALUE ZEROS.     00031100
031200     05  ARCH-RECS-BUILD              PIC S9(9)  VALUE ZEROS.     00031200
031300     05  ARCH-ENTRS-INSERT            PIC S9(9)  VALUE ZEROS.     00031300
031400     05  ARCH-NINES                   PIC S9(7)  VALUE +9999999.  00031400
031500                                                                  00031500
031600 01  A-GRPSPEC-ID                   PIC X(26).                    00031600
031700/                                                                 00031700
031800 01  WS-REC-LEN-AREA.                                             00031800
031900 COPY GCCDRLEN.                                                   00031900
032000/                                                                 00032000
032100 01  B-GRPSPEC-ID                   PIC X(26).                    00032100
032200/                                                                 00032200
032300 01  GRPS-ARCH-AREA.                                              00032300
032400 COPY GCARCHCC.                                                   00032400
032500/                                                                 00032500
032600 COPY GCARCHGS.                                                   00032600
032700/                                                                 00032700
032800 01  COMP-GRP-SPEC-BEFORE-AREA.                                   00032800
032900     COPY GCAR320B.                                               00032900
033000                                                                  00033000
033100 01  COMP-GRP-SPEC-AFTER-AREA.                                    00033100
033200     COPY GCAR320A.                                               00033200
033300/                                                                 00033300
033400*MAM G&R - ADDED LINKAGE SECTION                                  00033400
033500 LINKAGE SECTION.                                                 00033500
033600 01  PARM-AREA.                                                   00033600
033700     05  PARM-LENGTH      PIC S9(04) COMP.                        00033700
033800     05  PARM-LOCATION    PIC X(03).                              00033800
033900/                                                                 00033900
034000*MAM G&R - ADDED USING PARM-AREA                                  00034000
034100 PROCEDURE DIVISION USING PARM-AREA.                              00034100
034200 0000-MAINLINE.                                                   00034200
034300                                                                  00034300
034400     OPEN   INPUT      RGS-FILE                                   00034400
034500                       AUD-FILE                                   00034500
034600                       BIM-FILE.                                  00034600
034700                                                                  00034700
034800*MAM G&R - ADDED OPEN FOR QCF-FILE                                00034800
034900     OPEN   OUTPUT     QCF-FILE.                                  00034900
035000     MOVE   '3320'               TO RPT-IND.                      00035000
035100                                                                  00035100
035200***  TSGVSAM1 IS THE PROVISION TABULAR FILE                       00035200
035300*    MOVE   'S'                  TO REQUEST-TYPE-D.               00035300
035400*    MOVE    8                   TO SET-RECORD-LENGTH.            00035400
035500*    MOVE    3                   TO SET-VALUE.                    00035500
035600*    CALL   'TSGVSAM1'  USING PARM-ONE-D PARM-SET.                00035600
035700*    IF  REQUEST-TYPE-D NOT EQUAL 'S'                             00035700
035800*        MOVE SET-FEEDBACK       TO ABEND-CODE                    00035800
035900*        GO TO 9999-ERROR-RTN.                                    00035900
036000                                                                  00036000
036100***  TSGVSAM3 IS THE GCPS ARCHIVED FILE                           00036100
036200     MOVE   'S'                  TO REQUEST-TYPE-A.               00036200
036300     MOVE    8                   TO SET-RECORD-LENGTH.            00036300
036400     MOVE    3                   TO SET-VALUE.                    00036400
036500     CALL   'TSGVSAM3'  USING PARM-ONE-A PARM-SET.                00036500
036600     IF  REQUEST-TYPE-A NOT EQUAL 'S'                             00036600
036700         MOVE SET-FEEDBACK       TO ABEND-CODE                    00036700
036800         GO TO 9999-ERROR-RTN.                                    00036800
036900                                                                  00036900
037000**   MOVE   'O'                  TO REQUEST-TYPE-D.               00037000
037100*    CALL   'TSGVSAM1'  USING PARM-ONE-D PARM-TWO-D.              00037100
037200*    IF  REQUEST-TYPE-D NOT EQUAL 'O'                             00037200
037300*        MOVE SET-FEEDBACK       TO ABEND-CODE                    00037300
037400*        GO TO 9999-ERROR-RTN.                                    00037400
037500                                                                  00037500
037600     MOVE   'O'                  TO REQUEST-TYPE-A.               00037600
037700     CALL   'TSGVSAM3'  USING PARM-ONE-A PARM-TWO-A.              00037700
037800     IF  REQUEST-TYPE-A NOT EQUAL 'O'                             00037800
037900         MOVE SET-FEEDBACK       TO ABEND-CODE                    00037900
038000         GO TO 9999-ERROR-RTN.                                    00038000
038100                                                                  00038100
038200*MAM G&R - ADDED PARM-LENGTH CHECK                                00038200
038300     IF PARM-LENGTH NOT = 3                                       00038300
038400         DISPLAY 'ABENDED ON PARM-LENGTH: ' PARM-LENGTH           00038400
038500         GO TO 9999-ERROR-RTN.                                    00038500
038600                                                                  00038600
038700     MOVE 'TDY' TO MLDATE-FUNC.                                   00038700
038800     MOVE 'J' TO MLDATE-FORM1.                                    00038800
038900     CALL 'MLDATE' USING MLDATE01.                                00038900
039000     MOVE MLDATE-JUL1  TO JUL-DATE.                               00039000
039100                                                                  00039100
039200*    CALL 'TCDTES'    USING HSCDATES.                             00039200
039300                                                                  00039300
039400*    MOVE JYR                    TO JUL-YY.                       00039400
039500*    MOVE JDA                    TO JUL-DD.                       00039500
039600                                                                  00039600
039700                                                                  00039700
039800     PERFORM 0010-PROCESS-RTN  THRU 0010-EXIT                     00039800
039900         UNTIL END-OF-BIM-FILE.                                   00039900
040000                                                                  00040000
040100     PERFORM 0012-PROCESS-AUD  THRU 0012-EXIT                     00040100
040200         UNTIL END-OF-AUD-FILE.                                   00040200
040300                                                                  00040300
040400**   MOVE   'C'                  TO REQUEST-TYPE-D.               00040400
040500*    CALL   'TSGVSAM1'  USING PARM-ONE-D PARM-TWO-D.              00040500
040600*    IF  REQUEST-TYPE-D NOT EQUAL 'C'                             00040600
040700*        MOVE FEEDBACK-CODE-D    TO ABEND-CODE                    00040700
040800*        GO TO 9999-ERROR-RTN.                                    00040800
040900                                                                  00040900
041000     MOVE   'C'                  TO REQUEST-TYPE-A.               00041000
041100     CALL   'TSGVSAM3'  USING PARM-ONE-A PARM-TWO-A.              00041100
041200     IF  REQUEST-TYPE-A NOT EQUAL 'C'                             00041200
041300         MOVE FEEDBACK-CODE-A    TO ABEND-CODE                    00041300
041400         GO TO 9999-ERROR-RTN.                                    00041400
041500                                                                  00041500
041600*MAM G&R - ADDED CLOSE FOR QCF-FILE                               00041600
041700     CLOSE  RGS-FILE                                              00041700
041800            AUD-FILE                                              00041800
041900            BIM-FILE                                              00041900
042000            QCF-FILE.                                             00042000
042100                                                                  00042100
042200     DISPLAY '*** GROUP SPECIFIC ARCHIVED AUDIT TRAIL ***'.       00042200
042300     DISPLAY '    -----------------------------------    '.       00042300
042400     MOVE ARCH-RECS-BUILD   TO ARCH-PRINT-1.                      00042400
042500     MOVE ARCH-ENTRS-INSERT TO ARCH-PRINT-2.                      00042500
042600     MOVE WS-QCF-COUNT      TO QCF-PRINT.                         00042600
042700     MOVE WS-BYPASS-CNT     TO BYPASS-PRINT.                      00042700
042800     DISPLAY 'TOTAL RECORDS CREATED  = ' ARCH-PRINT-1.            00042800
042900     DISPLAY 'TOTAL ENTRIES INSERTED = ' ARCH-PRINT-2.            00042900
043000     DISPLAY 'TOTAL QCF RECORDS      = ' QCF-PRINT.               00043000
043100     DISPLAY 'TOTAL BYPASSED QCF REC = ' BYPASS-PRINT.            00043100
043200     GOBACK.                                                      00043200
043300                                                                  00043300
043400 0000-EXIT.                                                       00043400
043500     EXIT.                                                        00043500
043600/                                                                 00043600
043700 0010-PROCESS-RTN.                                                00043700
043800                                                                  00043800
043900***  CLEAR BEFORE AND AFTER IMAGE AREAS FIRST ***                 00043900
044000     MOVE SPACES TO GCG-GRP-SPEC-BEFORE-RECORD                    00044000
044100                    GCG2-GRP-SPEC-AFTER-RECORD.                   00044100
044200                                                                  00044200
044300***  READ BEFORE IMAGE GROUP SPECIFIC WORKFILE ***                00044300
044400     READ  BIM-FILE                                               00044400
044500           AT END MOVE 'END'  TO END-OF-FILE-B-SW                 00044500
044600                  GO TO 0010-EXIT.                                00044600
044700***                                                               00044700
044800***  IF THE BIM RECORD ONLINE-SIGNAL-INDICATOR IS AN ADD (MAP FROM00044800
044900***  THEN BYPASS THE ARCHIVE LOGIC                                00044900
045000***                                                               00045000
045100     IF WRK-ADD-REQUEST                                           00045100
045200        GO TO  0010-EXIT.                                         00045200
045300                                                                  00045300
045400                                                                  00045400
045500     MOVE   CORR     GCG-GRP-SPEC-RECORD OF BIM-RECORD  TO        00045500
045600                         GCG-GRP-SPEC-BEFORE-RECORD.              00045600
045700     MOVE   CORR     GCG-GRP-SPEC-RECORD OF BIM-RECORD  TO        00045700
045800                         GCG-ARCHIVED-COMMON-TABLE.               00045800
045900                                                                  00045900
046000     MOVE GCG-TERMDT-CEN OF GCG-GRP-SPEC-RECORD TO                00046000
046100          GCG-TERMDT-CEN OF GCG-ARCHIVED-COMMON-TABLE             00046100
046200          GCG-TERMN-DT OF GCG-GRP-SPEC-BEFORE-RECORD.             00046200
046300     MOVE GCG-POS-PENLTY-DT-CEN OF GCG-GRP-SPEC-RECORD TO         00046300
046400          GCG-POS-PENLTY-DT-CEN OF GCG-ARCHIVED-COMMON-TABLE      00046400
046500          GCG-NEW-POS-PENALTY-DATE OF GCG-GRP-SPEC-BEFORE-RECORD. 00046500
046600     MOVE GCG-ATCP-PENLTY-DT-CEN OF GCG-GRP-SPEC-RECORD TO        00046600
046700          GCG-ATCP-PENLTY-DT-CEN OF GCG-ARCHIVED-COMMON-TABLE     00046700
046800          GCG-ATCP-PENALTY-DATE OF GCG-GRP-SPEC-BEFORE-RECORD.    00046800
046900     MOVE GCG-WKND-PENLTY-DT-CEN OF GCG-GRP-SPEC-RECORD TO        00046900
047000          GCG-WKND-PENLTY-DT-CEN OF GCG-ARCHIVED-COMMON-TABLE     00047000
047100          GCG-WKND-PENALTY-DATE OF GCG-GRP-SPEC-BEFORE-RECORD.    00047100
047200     MOVE GCG-HOSP-PENLTY-DT-CEN OF GCG-GRP-SPEC-RECORD TO        00047200
047300          GCG-HOSP-PENLTY-DT-CEN OF GCG-ARCHIVED-COMMON-TABLE     00047300
047400          GCG-HOSP-PENALTY-DATE OF GCG-GRP-SPEC-BEFORE-RECORD.    00047400
047500     MOVE GCG-INOB-PENLTY-DT-CEN OF GCG-GRP-SPEC-RECORD TO        00047500
047600          GCG-INOB-PENLTY-DT-CEN OF GCG-ARCHIVED-COMMON-TABLE     00047600
047700          GCG-INOB-PENALTY-DATE OF GCG-GRP-SPEC-BEFORE-RECORD.    00047700
047800     MOVE GCG-MASOP-PENLTY-DT-CEN OF GCG-GRP-SPEC-RECORD TO       00047800
047900          GCG-MASOP-PENLTY-DT-CEN OF GCG-ARCHIVED-COMMON-TABLE    00047900
048000          GCG-MASOP-PENALTY-DATE OF GCG-GRP-SPEC-BEFORE-RECORD.   00048000
048100     MOVE GCG-MED-NEC-PENLTY-DT-CEN OF GCG-GRP-SPEC-RECORD TO     00048100
048200          GCG-MED-NEC-PENLTY-DT-CEN OF GCG-ARCHIVED-COMMON-TABLE  00048200
048300          GCG-MED-NEC-PENALTY-DATE OF GCG-GRP-SPEC-BEFORE-RECORD. 00048300
048400     MOVE GCG-PPO-PENLTY-DT-CEN OF GCG-GRP-SPEC-RECORD TO         00048400
048500          GCG-PPO-PENLTY-DT-CEN OF GCG-ARCHIVED-COMMON-TABLE      00048500
048600          GCG-PPO-PENALTY-DATE OF GCG-GRP-SPEC-BEFORE-RECORD.     00048600
048700     MOVE GCG-MOPS-PENLTY-DT-CEN OF GCG-GRP-SPEC-RECORD TO        00048700
048800          GCG-MOPS-PENLTY-DT-CEN OF GCG-ARCHIVED-COMMON-TABLE     00048800
048900          GCG-MOPS-PENALTY-DATE OF GCG-GRP-SPEC-BEFORE-RECORD.    00048900
049000     MOVE GCG-MSA-PENLTY-DT-CEN OF GCG-GRP-SPEC-RECORD TO         00049000
049100          GCG-MSA-PENLTY-DT-CEN OF GCG-ARCHIVED-COMMON-TABLE      00049100
049200          GCG-MSA-PENALTY-DATE OF GCG-GRP-SPEC-BEFORE-RECORD.     00049200
049300     MOVE GCG-PAR-PENLTY-DT-CEN OF GCG-GRP-SPEC-RECORD TO         00049300
049400          GCG-PAR-PENLTY-DT-CEN OF GCG-ARCHIVED-COMMON-TABLE      00049400
049500          GCG-PAR-PENALTY-DATE OF GCG-GRP-SPEC-BEFORE-RECORD.     00049500
049600     MOVE GCG-PAT-PENLTY-DT-CEN OF GCG-GRP-SPEC-RECORD TO         00049600
049700          GCG-PAT-PENLTY-DT-CEN OF GCG-ARCHIVED-COMMON-TABLE      00049700
049800          GCG-PAT-PENALTY-DATE OF GCG-GRP-SPEC-BEFORE-RECORD.     00049800
049900     MOVE GCG-REIM-PENLTY-DT-CEN OF GCG-GRP-SPEC-RECORD TO        00049900
050000          GCG-REIM-PENLTY-DT-CEN OF GCG-ARCHIVED-COMMON-TABLE     00050000
050100          GCG-REIM-PENALTY-DATE OF GCG-GRP-SPEC-BEFORE-RECORD.    00050100
050200     MOVE GCG-MON-DISCH-PENLTY-DT-CEN OF GCG-GRP-SPEC-RECORD TO   00050200
050300        GCG-MON-DISCH-PENLTY-DT-CEN OF GCG-ARCHIVED-COMMON-TABLE  00050300
050400        GCG-MON-DISCH-PENALTY-DATE OF GCG-GRP-SPEC-BEFORE-RECORD. 00050400
050500     MOVE GCG-SUB-ABUSE-PENLTY-DT-CEN OF GCG-GRP-SPEC-RECORD TO   00050500
050600        GCG-SUB-ABUSE-PENLTY-DT-CEN OF GCG-ARCHIVED-COMMON-TABLE  00050600
050700        GCG-SUB-ABUSE-PENALTY-DATE OF GCG-GRP-SPEC-BEFORE-RECORD. 00050700
050800     MOVE GCG-NEW-POS-PENLTY-DT-CEN OF GCG-GRP-SPEC-RECORD TO     00050800
050900          GCG-NEW-POS-PENLTY-DT-CEN OF GCG-ARCHIVED-COMMON-TABLE  00050900
051000          GCG-NEW-POS-PENALTY-DATE OF GCG-GRP-SPEC-BEFORE-RECORD. 00051000
051100     MOVE GCG-NEW-MEN-SUB-AB-PEN-DT-CEN OF GCG-GRP-SPEC-RECORD TO 00051100
051200          GCG-NEW-MEN-SUB-AB-PEN-DT-CEN OF                        00051200
051300                            GCG-ARCHIVED-COMMON-TABLE             00051300
051400          GCG-NEW-MEN-SUB-ABUSE-PEN-DT                            00051400
051500                            OF GCG-GRP-SPEC-BEFORE-RECORD.        00051500
051600     MOVE GCG-BAE-PENLTY-DT-CEN OF GCG-GRP-SPEC-RECORD TO         00051600
051700          GCG-BAE-PENLTY-DT-CEN OF GCG-ARCHIVED-COMMON-TABLE      00051700
051800          GCG-BAE-PENALTY-DATE OF GCG-GRP-SPEC-BEFORE-RECORD.     00051800
051900     MOVE GCG-RPO-PENLTY-DT-CEN OF GCG-GRP-SPEC-RECORD TO         00051900
052000          GCG-RPO-PENLTY-DT-CEN OF GCG-ARCHIVED-COMMON-TABLE      00052000
052100          GCG-RPO-PENALTY-DATE OF GCG-GRP-SPEC-BEFORE-RECORD.     00052100
052200     MOVE GCG-CPO-PENLTY-DT-CEN OF GCG-GRP-SPEC-RECORD TO         00052200
052300          GCG-CPO-PENLTY-DT-CEN OF GCG-ARCHIVED-COMMON-TABLE      00052300
052400          GCG-CPO-PENALTY-DATE OF GCG-GRP-SPEC-BEFORE-RECORD.     00052400
052500     MOVE GCG-CBL-PENLTY-DT-CEN OF GCG-GRP-SPEC-RECORD TO         00052500
052600          GCG-CBL-PENLTY-DT-CEN OF GCG-ARCHIVED-COMMON-TABLE      00052600
052700          GCG-CBL-PENALTY-DATE OF GCG-GRP-SPEC-BEFORE-RECORD.     00052700
052800     MOVE GCG-PAN-PENLTY-DT-CEN OF GCG-GRP-SPEC-RECORD TO         00052800
052900          GCG-PAN-PENLTY-DT-CEN OF GCG-ARCHIVED-COMMON-TABLE      00052900
053000          GCG-PAN-PENALTY-DATE OF GCG-GRP-SPEC-BEFORE-RECORD.     00053000
053100     MOVE GCG-POR-PREEXIST-DT-CEN OF GCG-GRP-SPEC-RECORD TO       00053100
053200          GCG-PORTABILITY-PREEXIST-DATE                           00053200
053300                               OF GCG-ARCHIVED-COMMON-TABLE       00053300
053400          GCG-PORTABILITY-PREEXIST-DATE                           00053400
053500                               OF GCG-GRP-SPEC-BEFORE-RECORD.     00053500
053600     MOVE GCG-MEN-HEALTH-PARITY-DT-CEN OF GCG-GRP-SPEC-RECORD TO  00053600
053700          GCG-MENTAL-HEALTH-PARITY-DATE                           00053700
053800                               OF GCG-ARCHIVED-COMMON-TABLE       00053800
053900          GCG-MENTAL-HEALTH-PARITY-DATE                           00053900
054000                               OF GCG-GRP-SPEC-BEFORE-RECORD.     00054000
054100     MOVE GCG-INTER-RELATIONAL-CODE-1 OF GCG-GRP-SPEC-RECORD TO   00054100
054200         GCG-INTER-RELATIONAL-CODE-1 OF GCG-ARCHIVED-COMMON-TABLE 00054200
054300         GCG-INTER-RELATIONAL-CODE-1 OF GCG-GRP-SPEC-BEFORE-RECORD00054300
054400     MOVE GCG-INTER-RELATIONAL-CODE-2 OF GCG-GRP-SPEC-RECORD TO   00054400
054500         GCG-INTER-RELATIONAL-CODE-2 OF GCG-ARCHIVED-COMMON-TABLE 00054500
054600         GCG-INTER-RELATIONAL-CODE-2 OF GCG-GRP-SPEC-BEFORE-RECORD00054600
054700     MOVE GCG-INTER-RELATIONAL-CODE-3 OF GCG-GRP-SPEC-RECORD TO   00054700
054800         GCG-INTER-RELATIONAL-CODE-3 OF GCG-ARCHIVED-COMMON-TABLE 00054800
054900         GCG-INTER-RELATIONAL-CODE-3 OF GCG-GRP-SPEC-BEFORE-RECORD00054900
055000     MOVE GCG-INTER-RELATIONAL-CODE-4 OF GCG-GRP-SPEC-RECORD TO   00055000
055100         GCG-INTER-RELATIONAL-CODE-4 OF GCG-ARCHIVED-COMMON-TABLE 00055100
055200         GCG-INTER-RELATIONAL-CODE-4 OF GCG-GRP-SPEC-BEFORE-RECORD00055200
055300                                                                  00055300
055400     MOVE GCG-HMO-MC-PENLTY-DT-CEN     OF GCG-GRP-SPEC-RECORD TO  00055400
055500          GCG-HMO-MC-PENLTY-DT-CEN                                00055500
055600                               OF GCG-ARCHIVED-COMMON-TABLE       00055600
055700          GCG-HMO-MC-PENLTY-DT-CEN                                00055700
055800                               OF GCG-GRP-SPEC-BEFORE-RECORD.     00055800
055900                                                                  00055900
056000     MOVE GCG-CONS-DRVN-PENLTY-DT-CEN  OF GCG-GRP-SPEC-RECORD TO  00056000
056100          GCG-CONS-DRVN-PENLTY-DT-CEN                             00056100
056200                               OF GCG-ARCHIVED-COMMON-TABLE       00056200
056300          GCG-CONS-DRVN-PENLTY-DT-CEN                             00056300
056400                               OF GCG-GRP-SPEC-BEFORE-RECORD.     00056400
056500                                                                  00056500
056600*    MOVE GCG-INTER-RELATIONAL-CODE-X  TO                         00056600
056700*                               GCG-INTER-RELATIONAL-CODE-1       00056700
056800*                               GCG-INTER-RELATIONAL-CODE-2       00056800
056900*                               GCG-INTER-RELATIONAL-CODE-3       00056900
057000*                               GCG-INTER-RELATIONAL-CODE-4       00057000
057100                                                                  00057100
057200     MOVE GCG-BC-ITS-NONPAR-PRICE-IND OF GCG-GRP-SPEC-RECORD TO   00057200
057300       GCG-BC-ITS-NONPAR-PRICE-IND OF GCG-ARCHIVED-COMMON-TABLE   00057300
057400       GCG-BC-ITS-NONPAR-PRICE-IND OF GCG-GRP-SPEC-BEFORE-RECORD  00057400
057500     MOVE GCG-BS-ITS-NONPAR-PRICE-IND OF GCG-GRP-SPEC-RECORD TO   00057500
057600       GCG-BS-ITS-NONPAR-PRICE-IND OF GCG-ARCHIVED-COMMON-TABLE   00057600
057700       GCG-BS-ITS-NONPAR-PRICE-IND OF GCG-GRP-SPEC-BEFORE-RECORD  00057700
057800     MOVE GCG-MM-ITS-NONPAR-PRICE-IND OF GCG-GRP-SPEC-RECORD TO   00057800
057900       GCG-MM-ITS-NONPAR-PRICE-IND OF GCG-ARCHIVED-COMMON-TABLE   00057900
058000       GCG-MM-ITS-NONPAR-PRICE-IND OF GCG-GRP-SPEC-BEFORE-RECORD  00058000
058100                                                                  00058100
058200     MOVE GCG-GRP-SPECIF-ID OF BIM-RECORD TO B-GRPSPEC-ID.        00058200
058300                                                                  00058300
058400***  READ AFTER-IMAGE GROUP SPECIFIC WORKFILE ***                 00058400
058500     PERFORM 0015-READ-AFTER-RTN  THRU 0015-EXIT.                 00058500
058600                                                                  00058600
058700     MOVE   CORR     GCG2-GRP-SPEC-RECORD OF RGS-RECORD  TO       00058700
058800                         GCG2-GRP-SPEC-AFTER-RECORD.              00058800
058900                                                                  00058900
059000     MOVE GCG2-TERMDT-CEN OF GCG2-GRP-SPEC-RECORD TO              00059000
059100          GCG2-TERMN-DT OF GCG2-GRP-SPEC-AFTER-RECORD.            00059100
059200     MOVE GCG2-NEW-POS-PENLTY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO   00059200
059300          GCG2-NEW-POS-PENALTY-DATE OF GCG2-GRP-SPEC-AFTER-RECORD.00059300
059400     MOVE GCG2-ATCP-PENLTY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO      00059400
059500          GCG2-ATCP-PENALTY-DATE OF GCG2-GRP-SPEC-AFTER-RECORD.   00059500
059600     MOVE GCG2-WKND-PENLTY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO      00059600
059700          GCG2-WKND-PENALTY-DATE OF GCG2-GRP-SPEC-AFTER-RECORD.   00059700
059800     MOVE GCG2-HOSP-PENLTY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO      00059800
059900          GCG2-HOSP-PENALTY-DATE OF GCG2-GRP-SPEC-AFTER-RECORD.   00059900
060000     MOVE GCG2-INOB-PENLTY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO      00060000
060100          GCG2-INOB-PENALTY-DATE OF GCG2-GRP-SPEC-AFTER-RECORD.   00060100
060200     MOVE GCG2-MASOP-PENLTY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO     00060200
060300          GCG2-MASOP-PENALTY-DATE OF GCG2-GRP-SPEC-AFTER-RECORD.  00060300
060400     MOVE GCG2-MED-NEC-PENLTY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO   00060400
060500          GCG2-MED-NEC-PENALTY-DATE OF GCG2-GRP-SPEC-AFTER-RECORD.00060500
060600     MOVE GCG2-PPO-PENLTY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO       00060600
060700          GCG2-PPO-PENALTY-DATE OF GCG2-GRP-SPEC-AFTER-RECORD.    00060700
060800     MOVE GCG2-MOPS-PENLTY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO      00060800
060900          GCG2-MOPS-PENALTY-DATE OF GCG2-GRP-SPEC-AFTER-RECORD.   00060900
061000     MOVE GCG2-MSA-PENLTY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO       00061000
061100          GCG2-MSA-PENALTY-DATE OF GCG2-GRP-SPEC-AFTER-RECORD.    00061100
061200     MOVE GCG2-PAR-PENLTY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO       00061200
061300          GCG2-PAR-PENALTY-DATE OF GCG2-GRP-SPEC-AFTER-RECORD.    00061300
061400     MOVE GCG2-PAT-PENLTY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO       00061400
061500          GCG2-PAT-PENALTY-DATE OF GCG2-GRP-SPEC-AFTER-RECORD.    00061500
061600     MOVE GCG2-REIM-PENLTY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO      00061600
061700          GCG2-REIM-PENALTY-DATE OF GCG2-GRP-SPEC-AFTER-RECORD.   00061700
061800     MOVE GCG2-MON-DISCH-PENLTY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO 00061800
061900       GCG2-MON-DISCH-PENALTY-DATE OF GCG2-GRP-SPEC-AFTER-RECORD. 00061900
062000     MOVE GCG2-SUB-ABUSE-PENLTY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO 00062000
062100       GCG2-SUB-ABUSE-PENALTY-DATE OF GCG2-GRP-SPEC-AFTER-RECORD. 00062100
062200     MOVE GCG2-NEW-POS-PENLTY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO   00062200
062300       GCG2-NEW-POS-PENALTY-DATE OF GCG2-GRP-SPEC-AFTER-RECORD.   00062300
062400     MOVE GCG2-NEW-MEN-SUB-AB-PEN-DT-CEN OF GCG2-GRP-SPEC-RECORD  00062400
062500       TO                                                         00062500
062600       GCG2-NEW-MEN-SUB-ABUSE-PEN-DT OF GCG2-GRP-SPEC-AFTER-RECORD00062600
062700*==> JP - ADDED BAE PENALTY DATE 7/24/02                          00062700
062800     MOVE GCG2-BAE-PENLTY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO       00062800
062900          GCG2-BAE-PENALTY-DATE OF GCG2-GRP-SPEC-AFTER-RECORD.    00062900
063000     MOVE GCG2-RPO-PENLTY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO       00063000
063100          GCG2-RPO-PENALTY-DATE OF GCG2-GRP-SPEC-AFTER-RECORD.    00063100
063200     MOVE GCG2-CPO-PENLTY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO       00063200
063300          GCG2-CPO-PENALTY-DATE OF GCG2-GRP-SPEC-AFTER-RECORD.    00063300
063400     MOVE GCG2-CBL-PENLTY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO       00063400
063500          GCG2-CBL-PENALTY-DATE OF GCG2-GRP-SPEC-AFTER-RECORD.    00063500
063600     MOVE GCG2-PAN-PENLTY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO       00063600
063700          GCG2-PAN-PENALTY-DATE OF GCG2-GRP-SPEC-AFTER-RECORD.    00063700
063800     MOVE GCG2-POR-PREEXIST-DT-CEN OF GCG2-GRP-SPEC-RECORD TO     00063800
063900          GCG2-PORTABILITY-PREEXIST-DATE                          00063900
064000                               OF GCG2-GRP-SPEC-AFTER-RECORD.     00064000
064100     MOVE GCG2-MEN-HEALTH-PARITY-DT-CEN OF GCG2-GRP-SPEC-RECORD TO00064100
064200          GCG2-MENTAL-HEALTH-PARITY-DATE                          00064200
064300                               OF GCG2-GRP-SPEC-AFTER-RECORD.     00064300
064400     MOVE GCG2-HMO-MC-PENLTY-DT-CEN     OF GCG2-GRP-SPEC-RECORD TO00064400
064500          GCG2-HMO-MC-PENLTY-DT-CEN                               00064500
064600                               OF GCG2-GRP-SPEC-AFTER-RECORD.     00064600
064700     MOVE GCG2-CONS-DRVN-PENLTY-DT-CEN  OF GCG2-GRP-SPEC-RECORD TO00064700
064800          GCG2-CONS-DRVN-PENLTY-DT-CEN                            00064800
064900                               OF GCG2-GRP-SPEC-AFTER-RECORD.     00064900
065000     MOVE GCG2-INTER-RELATIONAL-CODE-1 OF GCG2-GRP-SPEC-RECORD TO 00065000
065100      GCG2-INTER-RELATIONAL-CODE-1 OF GCG2-GRP-SPEC-AFTER-RECORD. 00065100
065200     MOVE GCG2-INTER-RELATIONAL-CODE-2 OF GCG2-GRP-SPEC-RECORD TO 00065200
065300      GCG2-INTER-RELATIONAL-CODE-2 OF GCG2-GRP-SPEC-AFTER-RECORD. 00065300
065400     MOVE GCG2-INTER-RELATIONAL-CODE-3 OF GCG2-GRP-SPEC-RECORD TO 00065400
065500      GCG2-INTER-RELATIONAL-CODE-3 OF GCG2-GRP-SPEC-AFTER-RECORD. 00065500
065600     MOVE GCG2-INTER-RELATIONAL-CODE-4 OF GCG2-GRP-SPEC-RECORD TO 00065600
065700      GCG2-INTER-RELATIONAL-CODE-4 OF GCG2-GRP-SPEC-AFTER-RECORD. 00065700
065800                                                                  00065800
065900     MOVE GCG2-BC-ITS-NONPAR-PRICE-IND OF GCG2-GRP-SPEC-RECORD TO 00065900
066000      GCG2-BC-ITS-NONPAR-PRICE-IND OF GCG2-GRP-SPEC-AFTER-RECORD. 00066000
066100     MOVE GCG2-BS-ITS-NONPAR-PRICE-IND OF GCG2-GRP-SPEC-RECORD TO 00066100
066200      GCG2-BS-ITS-NONPAR-PRICE-IND OF GCG2-GRP-SPEC-AFTER-RECORD. 00066200
066300     MOVE GCG2-MM-ITS-NONPAR-PRICE-IND OF GCG2-GRP-SPEC-RECORD TO 00066300
066400      GCG2-MM-ITS-NONPAR-PRICE-IND OF GCG2-GRP-SPEC-AFTER-RECORD. 00066400
066500                                                                  00066500
066600                                                                  00066600
066700     MOVE GCG2-GRP-SPECIF-ID OF RGS-RECORD TO A-GRPSPEC-ID.       00066700
066800                                                                  00066800
066900     MOVE SPACES TO READ-IND.                                     00066900
067000*MAM                                                              00067000
067100     MOVE 'Y'     TO WS-INTER-REL-SW.                             00067100
067200                                                                  00067200
067300***  COMPARE BEFORE-IMAGE AND AFTER-IMAGE RECORDS, GROUP SPECIFIC 00067300
067400     PERFORM 0020-GRSP-LVL-COMP-RTN  THRU 0020-EXIT.              00067400
067500                                                                  00067500
067600***  COMPARE BIM AND AIM TABULAR IDS ATTACHED TO EACH RECORD      00067600
067700     SET GCG2-INDEX     TO 1                                      00067700
067800     SET GCG-INDEX      TO 1.                                     00067800
067900                                                                  00067900
068000     PERFORM 0030-TAB-POINTERS-RTN  THRU 0030-EXIT                00068000
068100               UNTIL TAB-POINTERS-END.                            00068100
068200                                                                  00068200
068300     MOVE SPACES  TO TAB-POINTER-SW.                              00068300
068400                                                                  00068400
068500***  COMPARE DATA ELEMENTS WITHIN A TABULAR TO THE OTHER DATA     00068500
068600***  ELEMENTS WITHIN ANOTHER TABULAR ( BIM TAB TO AIM TAB)        00068600
068700***  THIS LOGIC IS NOT A REQUERMENT IN THE ARCHIVED SYSTEM        00068700
068800                                                                  00068800
068900**   SET GCG2-INDEX     TO 1                                      00068900
069000**   SET GCG-INDEX      TO 1                                      00069000
069100**                                                                00069100
069200**   PERFORM 0050-GRSP-TAB-RTN THRU 0050-EXIT                     00069200
069300**     VARYING GCG2-INDEX FROM 1 BY 1 UNTIL                       00069300
069400**       GCG-TAB-ID (GCG-INDEX)   EQUAL HIGH-VALUES AND           00069400
069500**       GCG2-TAB-ID (GCG2-INDEX) EQUAL HIGH-VALUES.              00069500
069600**                                                                00069600
069700                                                                  00069700
069800 0010-EXIT.                                                       00069800
069900     EXIT.                                                        00069900
070000/                                                                 00070000
070100 0012-PROCESS-AUD.                                                00070100
070200                                                                  00070200
070300     READ  AUD-FILE                                               00070300
070400           AT END MOVE 'END' TO END-OF-FILE-C-SW                  00070400
070500                  GO TO 0012-EXIT.                                00070500
070600                                                                  00070600
070700     IF WRK3-REC-TYPE =  'G9'                                     00070700
070800        NEXT SENTENCE                                             00070800
070900     ELSE                                                         00070900
071000        GO TO  0012-EXIT.                                         00071000
071100                                                                  00071100
071200     PERFORM  0071-PROCESS-ARCH-AUD  THRU 0071-EXIT.              00071200
071300                                                                  00071300
071400 0012-EXIT.                                                       00071400
071500     EXIT.                                                        00071500
071600/                                                                 00071600
071700 0015-READ-AFTER-RTN.                                             00071700
071800                                                                  00071800
071900     PERFORM 001510-READ-RGS-RTN  THRU 001510-EXIT                00071900
072000        UNTIL (AFTER-FOUND OR END-OF-RGS-FILE).                   00072000
072100                                                                  00072100
072200     IF END-OF-RGS-FILE                                           00072200
072300        MOVE '2099' TO ABEND-CODE                                 00072300
072400        GO TO 9999-ERROR-RTN.                                     00072400
072500                                                                  00072500
072600 0015-EXIT.                                                       00072600
072700     EXIT.                                                        00072700
072800                                                                  00072800
072900 001510-READ-RGS-RTN.                                             00072900
073000                                                                  00073000
073100     READ  RGS-FILE                                               00073100
073200           AT END MOVE 'END'  TO END-OF-FILE-A-SW                 00073200
073300                  GO TO 001510-EXIT.                              00073300
073400                                                                  00073400
073500     IF WRK2-SIG-B-SKELETON-C2-G2                                 00073500
073600         GO TO 001510-EXIT.                                       00073600
073700                                                                  00073700
073800     IF WRK2-KEY-FLD-DEL-REQ   OR                                 00073800
073900        WRK2-KEY-FLD-ADD-REQ                                      00073900
074000         GO TO 001510-EXIT.                                       00074000
074100                                                                  00074100
074200     IF WRK2-PLAN-CODE      EQUAL  WRK-PLAN-CODE      AND         00074200
074300        WRK2-GROUP-NUM      EQUAL  WRK-GROUP-NUM      AND         00074300
074400        WRK2-SECTION-NUM    EQUAL  WRK-SECTION-NUM    AND         00074400
074500        WRK2-PKG-CODE       EQUAL  WRK-PKG-CODE       AND         00074500
074600        WRK2-FAM-REL-LEVEL  EQUAL  WRK-FAM-REL-LEVEL  AND         00074600
074700        WRK2-EFFDT-CEN      EQUAL  WRK-EFFDT-CEN      AND         00074700
074800        WRK2-REC-GROUP-SPEC                                       00074800
074900         MOVE 'YES'     TO READ-IND.                              00074900
075000         GO TO 001510-EXIT.                                       00075000
075100                                                                  00075100
075200 001510-EXIT.                                                     00075200
075300     EXIT.                                                        00075300
075400/                                                                 00075400
075500 0020-GRSP-LVL-COMP-RTN.                                          00075500
075600*** COMPARE GROUP SPECIFIC DATA ELEMENTS (193) FIELDS ON THE      00075600
075700*** BEFORE AND AFTER IMAGE FILES                                  00075700
075800                                                                  00075800
075900     PERFORM 002020-COMP-RTN THRU 002020-EXIT                     00075900
076000       VARYING GCG-A-INDX FROM 1 BY 1 UNTIL                       00076000
076100               GCG-A-INDX > GCG2-GRP-SPEC-AFTER-OCCURS-CNT.       00076100
076200                                                                  00076200
076300 0020-EXIT.                                                       00076300
076400     EXIT.                                                        00076400
076500/                                                                 00076500
076600 002020-COMP-RTN.                                                 00076600
076700                                                                  00076700
076800     SET GCG-B-INDX   TO  GCG-A-INDX.                             00076800
076900                                                                  00076900
077000     IF  FIELDS-BEFORE (GCG-B-INDX) EQUAL                         00077000
077100         FIELDS-AFTER (GCG-A-INDX)  GO TO 002020-EXIT             00077100
077200     ELSE                                                         00077200
077300         PERFORM 0070-PROCESS-ARCH-GRPSP THRU 0070-EXIT.          00077300
077400                                                                  00077400
077500 002020-EXIT.                                                     00077500
077600     EXIT.                                                        00077600
077700/                                                                 00077700
077800 0030-TAB-POINTERS-RTN.                                           00077800
077900*** AT THE END OF TABULARS IDS SEARCH, PUT THE END SWITCH ON      00077900
078000                                                                  00078000
078100     IF GCG2-TAB-ID (GCG2-INDEX) EQUAL HIGH-VALUES AND            00078100
078200        GCG-TAB-ID (GCG-INDEX)   EQUAL HIGH-VALUES                00078200
078300          MOVE 'END'  TO TAB-POINTER-SW                           00078300
078400          GO TO 0030-EXIT.                                        00078400
078500                                                                  00078500
078600*** IF AFTER-IMAGE TAB ID IS GREATER THAN BEFORE-IMAGE TAB ID,    00078600
078700*** THEN TAB ID HAS BEEN DELETED FROM THE GRPSPC-RECORD. A RECORD 00078700
078800*** ON THE ARCHIVED FILE (OR AN ENTRY) WILL BE CREATED FROM BEFORE00078800
078900*** -IMAGE FILE (AIM DOES NOT HAVE THE TAB ID ANYMORE).           00078900
079000                                                                  00079000
079100     IF GCG2-TAB-ID (GCG2-INDEX) GREATER THAN                     00079100
079200        GCG-TAB-ID (GCG-INDEX)                                    00079200
079300          PERFORM 003020-LOAD-DEL-RTN THRU 003020-EXIT            00079300
079400          SET GCG-INDEX UP BY 1                                   00079400
079500          GO TO 0030-EXIT.                                        00079500
079600                                                                  00079600
079700*** IF AFTER-IMAGE TAB ID IS LESS THAN BEFORE-IMAGE TAB ID, THEN  00079700
079800*** THAT TAB ID HAS BEEN ADDED TO THE GRPSPC RECORD. A RECORD ON  00079800
079900*** THE ARCHIVED FILE (OR AN ENTRY) WILL BE CREATED FROM AFTER-   00079900
080000*** IMAGE FILE (WHERE THE TAB ID JUST BEEN ADDED) BUT THE SLOT    00080000
080100*** NUMBER WILL BE 'ADDED'  (DE VALUE) INDICATING THAT THE TABULAR00080100
080200*** HAS JUST BEEN ADDED.                                          00080200
080300                                                                  00080300
080400     IF GCG2-TAB-ID (GCG2-INDEX) LESS THAN                        00080400
080500        GCG-TAB-ID (GCG-INDEX)                                    00080500
080600          PERFORM 003030-LOAD-ADD-RTN THRU 003030-EXIT            00080600
080700          SET GCG2-INDEX UP BY 1                                  00080700
080800          GO TO 0030-EXIT.                                        00080800
080900                                                                  00080900
081000*** IF AFTER-IMAGE TAB ID EQUAL BEFOR-IMAGE TAB ID THEN, IF SLOT  00081000
081100*** NUMBER NOT EQUAL ON BOTH TABULARS, THEN CREATE AN ARCHIVED REC00081100
081200*** (OR ENTRY) ON THE ARCHIVED FILE.                              00081200
081300                                                                  00081300
081400     IF GCG2-TAB-ID (GCG2-INDEX) EQUAL                            00081400
081500        GCG-TAB-ID (GCG-INDEX)                                    00081500
081600          PERFORM 003040-LOAD-EQ-RTN THRU 003040-EXIT             00081600
081700          SET GCG-INDEX UP BY 1                                   00081700
081800          SET GCG2-INDEX UP BY 1                                  00081800
081900          GO TO 0030-EXIT.                                        00081900
082000                                                                  00082000
082100 0030-EXIT.                                                       00082100
082200     EXIT.                                                        00082200
082300/                                                                 00082300
082400 003020-LOAD-DEL-RTN.                                             00082400
082500*** RECORD LENGTH = KEY LENGTH + 4 BIYTS FOR VSAM                 00082500
082600                                                                  00082600
082700     IF GCG-TAB-ID (GCG-INDEX) =  HIGH-VALUES                     00082700
082800        GO TO  003020-EXIT.                                       00082800
082900                                                                  00082900
083000     MOVE SPACES                 TO KEY-FIELD-A.                  00083000
083100     MOVE 'G'                    TO KEY-TYPE-A.                   00083100
083200     MOVE GCG-PLAN-CODE   OF BIM-RECORD TO K-P-N.                 00083200
083300     MOVE GCG-GROUP-NUM   OF BIM-RECORD TO K-G-N.                 00083300
083400     MOVE GCG-SECTION-NUM OF BIM-RECORD TO K-S-N.                 00083400
083500     MOVE GCG-PKG-CODE    OF BIM-RECORD TO K-PKG.                 00083500
083600     MOVE GCG-FAM-REL-LVL OF BIM-RECORD TO K-F-R.                 00083600
083700     MOVE GCG-EFFDT-CEN   OF BIM-RECORD TO K-E-DT.                00083700
083800     MOVE GCG-TAB-ID (GCG-INDEX) TO  TB-ARCH-DE-2.                00083800
083900     MOVE TAB-ARCH-DE-CD         TO  KEY-NO-A.                    00083900
084000     MOVE 'R'                    TO REQUEST-TYPE-A.               00084000
084100     MOVE 42                     TO RECORD-LENGTH-A.              00084100
084200     CALL   'TSGVSAM3'  USING PARM-ONE-A                          00084200
084300                              PARM-TWO-A.                         00084300
084400                                                                  00084400
084500***  RECORD NOT FOUND OR EOF THEN CREATE A NEW ARCHIVE RECORD     00084500
084600***  FROM BEFORE-IMAGE RECORD                                     00084600
084700     IF  REQUEST-TYPE-A  = '2'  OR  '3'                           00084700
084800         PERFORM 0082-ADD-ARCH-TAB-RECORD THRU 0082-EXIT          00084800
084900         ADD +1  TO ARCH-RECS-BUILD                               00084900
085000         GO TO 003020-EXIT.                                       00085000
085100                                                                  00085100
085200     IF  REQUEST-TYPE-A NOT EQUAL 'R'                             00085200
085300         MOVE FEEDBACK-CODE-A    TO ABEND-CODE                    00085300
085400         GO TO 9999-ERROR-RTN.                                    00085400
085500                                                                  00085500
085600***  RECORD IS FOUND, THEN ADD ANOTHER ARCHIVING ENTRY            00085600
085700***  FROM BEFORE-IMAGE RECORD                                     00085700
085800                                                                  00085800
085900*MAM G&R - ADDED QUAL CTRL LOGIC                                  00085900
086000     INITIALIZE QCF-RECORD.                                       00086000
086100     MOVE ZEROS                  TO QCF-PLAN-CODE                 00086100
086200                                    QCF-GROUP-NO                  00086200
086300                                    QCF-SECTION-NO                00086300
086400                                    QCF-PKG-CODE.                 00086400
086500     MOVE SPACES                 TO QCF-FILLER                    00086500
086600                                    QCF-L-O-B                     00086600
086700                                    QCF-PROV-CTRL.                00086700
086800*                                   QCF-ANLS-INIT                 00086800
086900*                                   QCF-DEPT-NUM                  00086900
087000     MOVE PARM-LOCATION          TO QCF-PLAN-CODE.                00087000
087100     MOVE 'G'                    TO QCF-CONTRACT-GROUP-SP-IND     00087100
087200     MOVE TAB-ARCH-DE-CD         TO QCF-FUNC-FIELD.               00087200
087300     MOVE GCG-TAB-ID (GCG-INDEX) TO QCF-BIM-FIELD.                00087300
087400     MOVE 'DELETED'              TO QCF-AIM-FIELD.                00087400
087500     MOVE WRK-OPERATOR-ID        TO QCF-OPERATOR-ID.              00087500
087600                                                                  00087600
087700     MOVE GCG-PLAN-CODE  OF BIM-RECORD TO QCF-PLAN-CODE.          00087700
087800     MOVE GCG-GROUP-NUM  OF BIM-RECORD TO QCF-GROUP-NO.           00087800
087900     MOVE GCG-SECTION-NUM OF BIM-RECORD TO QCF-SECTION-NO.        00087900
088000     MOVE GCG-PKG-CODE   OF BIM-RECORD TO QCF-PKG-CODE.           00088000
088100     MOVE GCG-FAM-REL-LVL OF BIM-RECORD TO QCF-FAM-REL-LEVEL.     00088100
088200                                                                  00088200
088300     IF GCG-EFFDT-CEN OF BIM-RECORD  < 9999999                    00088300
088400        MOVE GCG-EFFDT-CEN OF BIM-RECORD TO QCF-EFF-DATE.         00088400
088500                                                                  00088500
088600     IF TODAYS-DATE < 9999999                                     00088600
088700        MOVE TODAYS-DATE TO QCF-FUNC-DATE.                        00088700
088800                                                                  00088800
088900     IF GCG2-INTER-RELATIONAL-CODE = ZEROS                        00088900
089000        MOVE 'N' TO WS-INTER-REL-SW                               00089000
089100        ADD +1   TO WS-BYPASS-CNT                                 00089100
089200     ELSE                                                         00089200
089300        MOVE 'Y' TO WS-INTER-REL-SW.                              00089300
089400                                                                  00089400
089500     IF WRK-OPERATOR-ID > SPACES AND                              00089500
089600        INTER-REL-CD-FND                                          00089600
089700        PERFORM 0084-WRITE-QUAL-CTRL THRU 0084-EXIT.              00089700
089800                                                                  00089800
089900     MOVE PNTRS-COUNT-A  TO ARCH-POINTERS-COUNT.                  00089900
090000     MOVE REC-AREA-A     TO ARCHIVED-GCPS-RECORD.                 00090000
090100                                                                  00090100
090200     ADD +1           TO ARCH-POINTERS-COUNT.                     00090200
090300     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00090300
090400         GO TO 003020-EXIT.                                       00090400
090500     SET ARCH-INDEX   TO ARCH-POINTERS-COUNT.                     00090500
090600                                                                  00090600
090700     MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                          00090700
090800                              ARCH-FLD-VALUE (ARCH-INDEX).        00090800
090900     MOVE WRK-OPERATOR-ID  TO ARCH-OPER-ID (ARCH-INDEX).          00090900
091000                                                                  00091000
091100     IF WRK-CDE-SP NOT = '2 '                                     00091100
091200         MOVE 'X'  TO ARCH-CDE-IND (ARCH-INDEX)                   00091200
091300     ELSE                                                         00091300
091400         MOVE ' '  TO ARCH-CDE-IND (ARCH-INDEX).                  00091400
091500                                                                  00091500
091600     MOVE TODAYS-DATE         TO ARCH-CHNG-DT-CEN (ARCH-INDEX).   00091600
091700     MOVE SPACES              TO ARCH-ANLS-CD (ARCH-INDEX).       00091700
091800                                                                  00091800
091900     IF WRK-ATB3-REQUEST                                          00091900
092000        MOVE WRK-SIGNAL-FROM-ONLINE TO ARCH-ATB-IND (ARCH-INDEX)  00092000
092100     ELSE                                                         00092100
092200        MOVE WRK-TYPE-MAINT-IND TO ARCH-ATB-IND (ARCH-INDEX).     00092200
092300                                                                  00092300
092400     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00092400
092500     ADD +1  TO ARCH-ENTRS-INSERT.                                00092500
092600                                                                  00092600
092700 003020-EXIT.                                                     00092700
092800     EXIT.                                                        00092800
092900/                                                                 00092900
093000 003030-LOAD-ADD-RTN.                                             00093000
093100                                                                  00093100
093200     IF GCG2-TAB-ID (GCG2-INDEX) =  HIGH-VALUES                   00093200
093300        GO TO  003030-EXIT.                                       00093300
093400                                                                  00093400
093500     MOVE 'G'                            TO KEY-TYPE-A.           00093500
093600     MOVE GCG2-PLAN-CODE   OF RGS-RECORD TO K-P-N.                00093600
093700     MOVE GCG2-GROUP-NUM   OF RGS-RECORD TO K-G-N.                00093700
093800     MOVE GCG2-SECTION-NUM OF RGS-RECORD TO K-S-N.                00093800
093900     MOVE GCG2-PKG-CODE    OF RGS-RECORD TO K-PKG.                00093900
094000     MOVE GCG2-FAM-REL-LVL OF RGS-RECORD TO K-F-R.                00094000
094100     MOVE GCG2-EFFDT-CEN   OF RGS-RECORD TO K-E-DT.               00094100
094200     MOVE GCG2-TAB-ID (GCG2-INDEX)       TO  TB-ARCH-DE-2.        00094200
094300     MOVE TAB-ARCH-DE-CD                 TO  KEY-NO-A.            00094300
094400                                                                  00094400
094500     MOVE 'R'    TO REQUEST-TYPE-A.                               00094500
094600     MOVE 42     TO RECORD-LENGTH-A.                              00094600
094700     CALL 'TSGVSAM3'  USING PARM-ONE-A                            00094700
094800                            PARM-TWO-A.                           00094800
094900                                                                  00094900
095000***  RECORD NOT FOUND OR EOF THEN CREATE A NEW ARCHIVE RECORD     00095000
095100***  FROM AFTER-IMAGE RECORD                                      00095100
095200     IF  REQUEST-TYPE-A  = '2'  OR  '3'                           00095200
095300         PERFORM 0081-ADD-ARCH-TAB-RECORD THRU 0081-EXIT          00095300
095400         ADD +1  TO ARCH-RECS-BUILD                               00095400
095500         GO TO 003030-EXIT.                                       00095500
095600                                                                  00095600
095700     IF  REQUEST-TYPE-A NOT EQUAL 'R'                             00095700
095800         MOVE FEEDBACK-CODE-A    TO ABEND-CODE                    00095800
095900         GO TO 9999-ERROR-RTN.                                    00095900
096000                                                                  00096000
096100***  RECORD IS FOUND, THEN ADD ANOTHER ARCHIVING ENTRY            00096100
096200***  FROM AFTER-IMAGE RECORD                                      00096200
096300                                                                  00096300
096400*MAM G&R - ADDED QUAL CTRL LOGIC                                  00096400
096500     INITIALIZE QCF-RECORD.                                       00096500
096600     MOVE ZEROS                  TO QCF-PLAN-CODE                 00096600
096700                                    QCF-GROUP-NO                  00096700
096800                                    QCF-SECTION-NO                00096800
096900                                    QCF-PKG-CODE.                 00096900
097000     MOVE SPACES                 TO QCF-FILLER                    00097000
097100                                    QCF-L-O-B                     00097100
097200                                    QCF-PROV-CTRL.                00097200
097300*                                   QCF-ANLS-INIT                 00097300
097400*                                   QCF-DEPT-NUM                  00097400
097500     MOVE PARM-LOCATION          TO QCF-PLAN-CODE.                00097500
097600     MOVE 'G'                    TO QCF-CONTRACT-GROUP-SP-IND     00097600
097700     MOVE TAB-ARCH-DE-CD         TO QCF-FUNC-FIELD.               00097700
097800     MOVE 'ADDED'                TO QCF-BIM-FIELD.                00097800
097900     MOVE GCG2-TAB-ID (GCG2-INDEX) TO QCF-AIM-FIELD.              00097900
098000     MOVE WRK2-OPERATOR-ID       TO QCF-OPERATOR-ID.              00098000
098100                                                                  00098100
098200     MOVE GCG2-PLAN-CODE   OF RGS-RECORD TO QCF-PLAN-CODE.        00098200
098300     MOVE GCG2-GROUP-NUM   OF RGS-RECORD TO QCF-GROUP-NO.         00098300
098400     MOVE GCG2-SECTION-NUM OF RGS-RECORD TO QCF-SECTION-NO.       00098400
098500     MOVE GCG2-PKG-CODE    OF RGS-RECORD TO QCF-PKG-CODE.         00098500
098600     MOVE GCG2-FAM-REL-LVL OF RGS-RECORD TO QCF-FAM-REL-LEVEL.    00098600
098700                                                                  00098700
098800     IF GCG2-EFFDT-CEN OF RGS-RECORD < 9999999                    00098800
098900        MOVE GCG2-EFFDT-CEN OF RGS-RECORD TO QCF-EFF-DATE.        00098900
099000                                                                  00099000
099100     IF TODAYS-DATE < 9999999                                     00099100
099200        MOVE TODAYS-DATE TO QCF-FUNC-DATE.                        00099200
099300                                                                  00099300
099400     IF GCG2-INTER-RELATIONAL-CODE = ZEROS                        00099400
099500        MOVE 'N' TO WS-INTER-REL-SW                               00099500
099600        ADD +1   TO WS-BYPASS-CNT                                 00099600
099700     ELSE                                                         00099700
099800        MOVE 'Y' TO WS-INTER-REL-SW.                              00099800
099900                                                                  00099900
100000     IF WRK-OPERATOR-ID > SPACES AND                              00100000
100100        INTER-REL-CD-FND                                          00100100
100200        PERFORM 0084-WRITE-QUAL-CTRL THRU 0084-EXIT.              00100200
100300                                                                  00100300
100400     MOVE PNTRS-COUNT-A  TO ARCH-POINTERS-COUNT.                  00100400
100500     MOVE REC-AREA-A     TO ARCHIVED-GCPS-RECORD.                 00100500
100600                                                                  00100600
100700     ADD +1          TO ARCH-POINTERS-COUNT.                      00100700
100800     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00100800
100900         GO TO 003030-EXIT.                                       00100900
101000     SET ARCH-INDEX  TO ARCH-POINTERS-COUNT.                      00101000
101100                                                                  00101100
101200     MOVE  '++ADDED++'      TO ARCH-FLD-VALUE (ARCH-INDEX).       00101200
101300     MOVE WRK2-OPERATOR-ID  TO ARCH-OPER-ID (ARCH-INDEX).         00101300
101400                                                                  00101400
101500     IF WRK2-CDE-SP NOT = '2 '                                    00101500
101600        MOVE 'X'  TO ARCH-CDE-IND (ARCH-INDEX)                    00101600
101700     ELSE                                                         00101700
101800        MOVE ' '  TO ARCH-CDE-IND (ARCH-INDEX).                   00101800
101900                                                                  00101900
102000     MOVE TODAYS-DATE         TO ARCH-CHNG-DT-CEN (ARCH-INDEX).   00102000
102100     MOVE SPACES              TO ARCH-ANLS-CD (ARCH-INDEX).       00102100
102200                                                                  00102200
102300     IF WRK2-ATB3-REQUEST                                         00102300
102400        MOVE WRK2-SIGNAL-FROM-ONLINE TO ARCH-ATB-IND (ARCH-INDEX) 00102400
102500     ELSE                                                         00102500
102600        MOVE WRK2-TYPE-MAINT-IND     TO ARCH-ATB-IND (ARCH-INDEX).00102600
102700                                                                  00102700
102800     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00102800
102900     ADD  +1  TO ARCH-ENTRS-INSERT.                               00102900
103000                                                                  00103000
103100 003030-EXIT.                                                     00103100
103200     EXIT.                                                        00103200
103300/                                                                 00103300
103400 003040-LOAD-EQ-RTN.                                              00103400
103500*** IF BIM TABULAR SLOT NOT EQUAL AIM TABULAR SLOT THEN CREATE    00103500
103600*** AN ARCHIVED RECORD (OR ENTRY IF RECORD ALREADY EXISTED).      00103600
103700                                                                  00103700
103800     IF GCG-TAB-SLOT-NO (GCG-INDEX) EQUAL                         00103800
103900        GCG2-TAB-SLOT-NO (GCG2-INDEX)                             00103900
104000         GO TO  003040-EXIT.                                      00104000
104100                                                                  00104100
104200     MOVE 'G'                    TO KEY-TYPE-A.                   00104200
104300     MOVE GCG-PLAN-CODE   OF BIM-RECORD TO K-P-N.                 00104300
104400     MOVE GCG-GROUP-NUM   OF BIM-RECORD TO K-G-N.                 00104400
104500     MOVE GCG-SECTION-NUM OF BIM-RECORD TO K-S-N.                 00104500
104600     MOVE GCG-PKG-CODE    OF BIM-RECORD TO K-PKG.                 00104600
104700     MOVE GCG-FAM-REL-LVL OF BIM-RECORD TO K-F-R.                 00104700
104800     MOVE GCG-EFFDT-CEN   OF BIM-RECORD TO K-E-DT.                00104800
104900     MOVE GCG-TAB-ID (GCG-INDEX) TO  TB-ARCH-DE-2.                00104900
105000     MOVE TAB-ARCH-DE-CD         TO  KEY-NO-A.                    00105000
105100     MOVE 'R'                    TO REQUEST-TYPE-A.               00105100
105200     MOVE 42                     TO RECORD-LENGTH-A.              00105200
105300                                                                  00105300
105400     CALL 'TSGVSAM3'  USING PARM-ONE-A                            00105400
105500                            PARM-TWO-A.                           00105500
105600                                                                  00105600
105700***  RECORD NOT FOUND OR EOF THEN CREATE A NEW ARCHIVE RECORD     00105700
105800***  FROM BEFORE-IMAGE RECORD                                     00105800
105900     IF  REQUEST-TYPE-A  = '2'  OR  '3'                           00105900
106000         PERFORM 0082-ADD-ARCH-TAB-RECORD THRU 0082-EXIT          00106000
106100         ADD +1  TO ARCH-RECS-BUILD                               00106100
106200         GO TO 003040-EXIT.                                       00106200
106300                                                                  00106300
106400***  RECORD IS FOUND, THEN ADD ANOTHER ARCHIVING ENTRY            00106400
106500***  FROM BEFORE-IMAGE RECORD                                     00106500
106600                                                                  00106600
106700*MAM G&R - ADDED QUAL CTRL LOGIC                                  00106700
106800     INITIALIZE QCF-RECORD.                                       00106800
106900     MOVE ZEROS                  TO QCF-PLAN-CODE                 00106900
107000                                    QCF-GROUP-NO                  00107000
107100                                    QCF-SECTION-NO                00107100
107200                                    QCF-PKG-CODE.                 00107200
107300     MOVE SPACES                 TO QCF-FILLER                    00107300
107400                                    QCF-L-O-B                     00107400
107500                                    QCF-PROV-CTRL.                00107500
107600*                                   QCF-ANLS-INIT                 00107600
107700*                                   QCF-DEPT-NUM                  00107700
107800     MOVE PARM-LOCATION          TO QCF-PLAN-CODE.                00107800
107900     MOVE 'G'                    TO QCF-CONTRACT-GROUP-SP-IND     00107900
108000     MOVE TAB-ARCH-DE-CD         TO QCF-FUNC-FIELD.               00108000
108100     MOVE GCG-TAB-SLOT-NO (GCG-INDEX)   TO QCF-BIM-FIELD.         00108100
108200     MOVE GCG2-TAB-SLOT-NO (GCG2-INDEX) TO QCF-AIM-FIELD.         00108200
108300     MOVE WRK-OPERATOR-ID        TO QCF-OPERATOR-ID.              00108300
108400                                                                  00108400
108500     MOVE GCG-PLAN-CODE    OF BIM-RECORD TO QCF-PLAN-CODE.        00108500
108600     MOVE GCG-GROUP-NUM    OF BIM-RECORD TO QCF-GROUP-NO.         00108600
108700     MOVE GCG-SECTION-NUM  OF BIM-RECORD TO QCF-SECTION-NO.       00108700
108800     MOVE GCG-PKG-CODE     OF BIM-RECORD TO QCF-PKG-CODE.         00108800
108900     MOVE GCG-FAM-REL-LVL  OF BIM-RECORD TO QCF-FAM-REL-LEVEL.    00108900
109000                                                                  00109000
109100     IF GCG-EFFDT-CEN OF BIM-RECORD < 9999999                     00109100
109200        MOVE GCG-EFFDT-CEN OF BIM-RECORD TO QCF-EFF-DATE.         00109200
109300                                                                  00109300
109400     IF TODAYS-DATE < 9999999                                     00109400
109500        MOVE TODAYS-DATE TO QCF-FUNC-DATE.                        00109500
109600                                                                  00109600
109700     IF GCG2-INTER-RELATIONAL-CODE = ZEROS                        00109700
109800        MOVE 'N' TO WS-INTER-REL-SW                               00109800
109900        ADD +1   TO WS-BYPASS-CNT                                 00109900
110000     ELSE                                                         00110000
110100        MOVE 'Y' TO WS-INTER-REL-SW.                              00110100
110200                                                                  00110200
110300     IF WRK-OPERATOR-ID > SPACES AND                              00110300
110400        INTER-REL-CD-FND                                          00110400
110500        PERFORM 0084-WRITE-QUAL-CTRL THRU 0084-EXIT.              00110500
110600                                                                  00110600
110700     MOVE PNTRS-COUNT-A  TO ARCH-POINTERS-COUNT.                  00110700
110800     MOVE REC-AREA-A     TO ARCHIVED-GCPS-RECORD.                 00110800
110900                                                                  00110900
111000     ADD +1           TO ARCH-POINTERS-COUNT.                     00111000
111100     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00111100
111200         GO TO 003040-EXIT.                                       00111200
111300     SET ARCH-INDEX   TO ARCH-POINTERS-COUNT.                     00111300
111400     MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                          00111400
111500                              ARCH-FLD-VALUE (ARCH-INDEX).        00111500
111600     MOVE WRK-OPERATOR-ID  TO ARCH-OPER-ID (ARCH-INDEX).          00111600
111700                                                                  00111700
111800     IF WRK-CDE-SP NOT = '2 '                                     00111800
111900        MOVE 'X'  TO ARCH-CDE-IND (ARCH-INDEX)                    00111900
112000     ELSE                                                         00112000
112100        MOVE ' '  TO ARCH-CDE-IND (ARCH-INDEX).                   00112100
112200                                                                  00112200
112300     MOVE TODAYS-DATE         TO ARCH-CHNG-DT-CEN (ARCH-INDEX).   00112300
112400     MOVE SPACES              TO ARCH-ANLS-CD (ARCH-INDEX).       00112400
112500                                                                  00112500
112600     IF WRK-ATB3-REQUEST                                          00112600
112700        MOVE WRK-SIGNAL-FROM-ONLINE TO ARCH-ATB-IND (ARCH-INDEX)  00112700
112800     ELSE                                                         00112800
112900        MOVE WRK-TYPE-MAINT-IND     TO ARCH-ATB-IND (ARCH-INDEX). 00112900
113000                                                                  00113000
113100     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00113100
113200     ADD +1  TO ARCH-ENTRS-INSERT.                                00113200
113300                                                                  00113300
113400 003040-EXIT.                                                     00113400
113500     EXIT.                                                        00113500
113600/                                                                 00113600
113700 0070-PROCESS-ARCH-GRPSP.                                         00113700
113800                                                                  00113800
113900     SET  GCG-A-INDEX TO  GCG-A-INDX.                             00113900
114000     MOVE SPACES                 TO KEY-FIELD-A.                  00114000
114100     MOVE 'G'                    TO KEY-TYPE-A.                   00114100
114200     MOVE GCG-PLAN-CODE   OF BIM-RECORD TO K-P-N.                 00114200
114300     MOVE GCG-GROUP-NUM   OF BIM-RECORD TO K-G-N.                 00114300
114400     MOVE GCG-SECTION-NUM OF BIM-RECORD TO K-S-N.                 00114400
114500     MOVE GCG-PKG-CODE    OF BIM-RECORD TO K-PKG.                 00114500
114600     MOVE GCG-FAM-REL-LVL OF BIM-RECORD TO K-F-R.                 00114600
114700     MOVE GCG-EFFDT-CEN   OF BIM-RECORD TO K-E-DT.                00114700
114800     MOVE GCG-TAB-ID (GCG-INDEX) TO  TB-ARCH-DE-2.                00114800
114900     MOVE GCG-A-DE-ID (GCG-A-INDEX) TO KEY-NO-A.                  00114900
115000     MOVE 'R'                    TO REQUEST-TYPE-A.               00115000
115100     MOVE 42                     TO RECORD-LENGTH-A.              00115100
115200                                                                  00115200
115300     CALL 'TSGVSAM3'  USING PARM-ONE-A                            00115300
115400                            PARM-TWO-A.                           00115400
115500                                                                  00115500
115600***  RECORD NOT FOUND OR EOF THEN CREATE A NEW ARCHIVE RECORD     00115600
115700***  FROM BEFORE-IMAGE RECORD                                     00115700
115800     IF  REQUEST-TYPE-A  = '2'  OR  '3'                           00115800
115900         PERFORM 0080-ADD-ARCH-GRPS-RECORD THRU 0080-EXIT         00115900
116000         ADD +1  TO ARCH-RECS-BUILD                               00116000
116100         GO TO 0070-EXIT.                                         00116100
116200                                                                  00116200
116300     IF  REQUEST-TYPE-A NOT EQUAL 'R'                             00116300
116400         MOVE FEEDBACK-CODE-A    TO ABEND-CODE                    00116400
116500         GO TO 9999-ERROR-RTN.                                    00116500
116600                                                                  00116600
116700***  RECORD IS FOUND, THEN ADD ANOTHER ARCHIVING ENTRY            00116700
116800*MAM G&R - ADDED QUAL CTRL LOGIC                                  00116800
116900                                                                  00116900
117000     INITIALIZE QCF-RECORD.                                       00117000
117100     MOVE ZEROS                  TO QCF-PLAN-CODE                 00117100
117200                                    QCF-GROUP-NO                  00117200
117300                                    QCF-SECTION-NO                00117300
117400                                    QCF-PKG-CODE.                 00117400
117500     MOVE SPACES                 TO QCF-FILLER                    00117500
117600                                    QCF-L-O-B                     00117600
117700                                    QCF-PROV-CTRL.                00117700
117800*                                   QCF-ANLS-INIT                 00117800
117900*                                   QCF-DEPT-NUM                  00117900
118000     MOVE PARM-LOCATION          TO QCF-PLAN-CODE.                00118000
118100     MOVE 'G'                    TO QCF-CONTRACT-GROUP-SP-IND     00118100
118200     MOVE WRK-OPERATOR-ID        TO QCF-OPERATOR-ID.              00118200
118300                                                                  00118300
118400     MOVE GCG-PLAN-CODE    OF BIM-RECORD TO QCF-PLAN-CODE.        00118400
118500     MOVE GCG-GROUP-NUM    OF BIM-RECORD TO QCF-GROUP-NO.         00118500
118600     MOVE GCG-SECTION-NUM  OF BIM-RECORD TO QCF-SECTION-NO.       00118600
118700     MOVE GCG-PKG-CODE     OF BIM-RECORD TO QCF-PKG-CODE.         00118700
118800     MOVE GCG-FAM-REL-LVL  OF BIM-RECORD TO QCF-FAM-REL-LEVEL.    00118800
118900                                                                  00118900
119000     IF GCG-EFFDT-CEN OF BIM-RECORD  < 9999999                    00119000
119100        MOVE GCG-EFFDT-CEN OF BIM-RECORD TO QCF-EFF-DATE.         00119100
119200                                                                  00119200
119300     IF TODAYS-DATE < 9999999                                     00119300
119400        MOVE TODAYS-DATE TO QCF-FUNC-DATE.                        00119400
119500                                                                  00119500
119600     MOVE PNTRS-COUNT-A  TO ARCH-POINTERS-COUNT.                  00119600
119700     MOVE REC-AREA-A     TO ARCHIVED-GCPS-RECORD.                 00119700
119800                                                                  00119800
119900     ADD +1          TO ARCH-POINTERS-COUNT.                      00119900
120000     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00120000
120100         GO TO 0070-EXIT.                                         00120100
120200     SET ARCH-INDEX  TO ARCH-POINTERS-COUNT.                      00120200
120300                                                                  00120300
120400     MOVE GCG-A-DE-VALUE (GCG-A-INDEX) TO                         00120400
120500                              ARCH-FLD-VALUE (ARCH-INDEX).        00120500
120600     MOVE GCG-A-DE-ID (GCG-A-INDEX)  TO QCF-FUNC-FIELD.           00120600
120700     MOVE WRK-OPERATOR-ID  TO ARCH-OPER-ID (ARCH-INDEX).          00120700
120800     MOVE FIELDS-BEFORE (GCG-B-INDX) TO QCF-BIM-FIELD.            00120800
120900     MOVE FIELDS-AFTER (GCG-A-INDX)  TO QCF-AIM-FIELD.            00120900
121000                                                                  00121000
121100     IF GCG2-INTER-RELATIONAL-CODE = ZEROS                        00121100
121200        MOVE 'N' TO WS-INTER-REL-SW                               00121200
121300        ADD +1   TO WS-BYPASS-CNT                                 00121300
121400     ELSE                                                         00121400
121500        MOVE 'Y' TO WS-INTER-REL-SW.                              00121500
121600                                                                  00121600
121700     IF WRK-OPERATOR-ID > SPACES AND                              00121700
121800        INTER-REL-CD-FND                                          00121800
121900        PERFORM 0084-WRITE-QUAL-CTRL THRU 0084-EXIT.              00121900
122000                                                                  00122000
122100     IF WRK-CDE-SP NOT = '2 '                                     00122100
122200        MOVE 'X'  TO ARCH-CDE-IND (ARCH-INDEX)                    00122200
122300     ELSE                                                         00122300
122400        MOVE ' '  TO ARCH-CDE-IND (ARCH-INDEX).                   00122400
122500                                                                  00122500
122600     MOVE TODAYS-DATE         TO ARCH-CHNG-DT-CEN (ARCH-INDEX).   00122600
122700     MOVE SPACES              TO ARCH-ANLS-CD (ARCH-INDEX).       00122700
122800                                                                  00122800
122900     IF WRK-ATB3-REQUEST                                          00122900
123000        MOVE WRK-SIGNAL-FROM-ONLINE TO ARCH-ATB-IND (ARCH-INDEX)  00123000
123100     ELSE                                                         00123100
123200        MOVE WRK-TYPE-MAINT-IND     TO ARCH-ATB-IND (ARCH-INDEX). 00123200
123300                                                                  00123300
123400     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00123400
123500     ADD +1  TO ARCH-ENTRS-INSERT.                                00123500
123600                                                                  00123600
123700 0070-EXIT.                                                       00123700
123800     EXIT.                                                        00123800
123900/                                                                 00123900
124000 0071-PROCESS-ARCH-AUD.                                           00124000
124100**** RECORD-LEN = SEARCH-KEY-LEN = 27 + 4  ***                    00124100
124200*--- IS RECORD ON THE ARCHIVING FILE ?                            00124200
124300                                                                  00124300
124400     MOVE 'G'                TO KEY-TYPE-A.                       00124400
124500     MOVE WRK3-PLAN-CODE     TO K-P-N.                            00124500
124600     MOVE WRK3-GROUP-NUM     TO K-G-N.                            00124600
124700     MOVE WRK3-SECTION-NUM   TO K-S-N.                            00124700
124800     MOVE WRK3-PKG-CODE      TO K-PKG.                            00124800
124900     MOVE WRK3-FAM-REL-LEVEL TO K-F-R.                            00124900
125000     MOVE SPACE              TO K-LOB.                            00125000
125100     MOVE SPACE              TO K-P-C.                            00125100
125200     MOVE WRK3-EFFDT-CEN     TO K-E-DT.                           00125200
125300                                                                  00125300
125400     MOVE 'R'    TO REQUEST-TYPE-A.                               00125400
125500     MOVE 42     TO RECORD-LENGTH-A.                              00125500
125600                                                                  00125600
125700     SET GCAUD-TBL-INDEX  TO 1.                                   00125700
125800     SET GCAUD-TBL-INDEX  DOWN  BY 1.                             00125800
125900                                                                  00125900
126000 0071-GET-FIRST-FUNC.                                             00126000
126100 0071-GET-NEXT-OCCUR.                                             00126100
126200     SET GCAUD-TBL-INDEX UP BY 1.                                 00126200
126300     IF  GCAUD-TBL-INDEX NOT >  GCAUD-TABLE-FLDS-OCCURS-CNT       00126300
126400         MOVE  'R'    TO REQUEST-TYPE-A                           00126400
126500         MOVE  42     TO RECORD-LENGTH-A                          00126500
126600     ELSE                                                         00126600
126700         GO  TO 0071-EXIT.                                        00126700
126800                                                                  00126800
126900     MOVE GCAUD-FUNC-TYPE (GCAUD-TBL-INDEX) TO  WS-AUD-FUNC.      00126900
127000     MOVE WS-AUD-ID     TO KEY-NO-A.                              00127000
127100     CALL 'TSGVSAM3'    USING PARM-ONE-A                          00127100
127200                              PARM-TWO-A.                         00127200
127300                                                                  00127300
127400***  RECORD NOT FOUND OR EOF THEN CREATE A NEW ARCHIVE RECORD     00127400
127500***  FROM THE FIRST FUNC TYPE, SEARCH FOR ANOTHER AUDIT OCCUR     00127500
127600***  WITH THE SAME FUNC TYPE AND ADDED TO THE ARCHIVE RECORD      00127600
127700***  JUST BEEN CREATED.                                           00127700
127800                                                                  00127800
127900     IF  REQUEST-TYPE-A  = '2'  OR  '3'                           00127900
128000         PERFORM 0079-ADD-ARCH-AUD-REC  THRU 0079-EXIT            00128000
128100         ADD  +1 TO ARCH-RECS-BUILD                               00128100
128200         GO TO 0071-GET-NEXT-OCCUR.                               00128200
128300                                                                  00128300
128400***  RECORD IS FOUND, THEN ADD AN ARCHIVING ENTERY FOR EACH       00128400
128500***  OCCUR FOR THAT AUDIT FUNCTION TYPE                           00128500
128600                                                                  00128600
128700     IF  REQUEST-TYPE-A NOT EQUAL 'R'                             00128700
128800         MOVE FEEDBACK-CODE-A    TO ABEND-CODE                    00128800
128900         GO TO 9999-ERROR-RTN.                                    00128900
129000                                                                  00129000
129100     PERFORM 0078-ADD-ONE-ARCH-AUD   THRU  0078-EXIT.             00129100
129200     ADD +1  TO ARCH-ENTRS-INSERT.                                00129200
129300     GO TO 0071-GET-NEXT-OCCUR.                                   00129300
129400                                                                  00129400
129500 0071-EXIT.                                                       00129500
129600     EXIT.                                                        00129600
129700/                                                                 00129700
129800 0078-ADD-ONE-ARCH-AUD.                                           00129800
129900                                                                  00129900
130000*MAM G&R - ADDED PERFORM STATEMENT                                00130000
130100*          FORMATTED EXISTING STATEMENTS                          00130100
130200*    PERFORM 007910-FRMT-QUAL-CTRL-FILE  THRU 007910-EXIT.        00130200
130300                                                                  00130300
130400     MOVE PNTRS-COUNT-A    TO ARCH-POINTERS-COUNT.                00130400
130500     MOVE REC-AREA-A       TO ARCHIVED-GCPS-RECORD.               00130500
130600                                                                  00130600
130700     ADD +1          TO ARCH-POINTERS-COUNT.                      00130700
130800     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00130800
130900         GO TO 0078-EXIT.                                         00130900
131000     SET ARCH-INDEX  TO ARCH-POINTERS-COUNT.                      00131000
131100                                                                  00131100
131200     MOVE GCAUD-DEPT-NO (GCAUD-TBL-INDEX)        TO               00131200
131300                                 ARCH-FLD-VALUE (ARCH-INDEX).     00131300
131400     MOVE GCAUD-OPER-ID (GCAUD-TBL-INDEX)        TO               00131400
131500                                 ARCH-OPER-ID (ARCH-INDEX).       00131500
131600     MOVE GCAUD-NONCDE-CDE-IND (GCAUD-TBL-INDEX) TO               00131600
131700                                 ARCH-CDE-IND (ARCH-INDEX).       00131700
131800     MOVE GCAUD-ANLST-INIT (GCAUD-TBL-INDEX)     TO               00131800
131900                                 ARCH-ANLS-CD (ARCH-INDEX).       00131900
132000     MOVE GCAUD-FUNCDT-CEN (GCAUD-TBL-INDEX)     TO               00132000
132100                                 ARCH-CHNG-DT-CEN (ARCH-INDEX).   00132100
132200     MOVE GCAUD-ATB-IND (GCAUD-TBL-INDEX)        TO               00132200
132300                                 ARCH-ATB-IND (ARCH-INDEX).       00132300
132400                                                                  00132400
132500     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00132500
132600                                                                  00132600
132700 0078-EXIT.                                                       00132700
132800     EXIT.                                                        00132800
132900/                                                                 00132900
133000 0079-ADD-ARCH-AUD-REC.                                           00133000
133100                                                                  00133100
133200*MAM G&R - ADDED PERFORM STATEMENT                                00133200
133300*          FORMATTED EXISTING STATEMENTS                          00133300
133400*    PERFORM 007910-FRMT-QUAL-CTRL-FILE  THRU 007910-EXIT.        00133400
133500                                                                  00133500
133600     MOVE ZEROS               TO ARCH-EFFDT-CEN                   00133600
133700                                 ARCH-POINTERS-COUNT.             00133700
133800     MOVE  'G'                TO ARCH-STA-CD.                     00133800
133900     MOVE WRK3-PLAN-CODE      TO ARCH-PLAN-CODE.                  00133900
134000     MOVE WRK3-GROUP-NUM      TO ARCH-GROUP-NUM.                  00134000
134100     MOVE WRK3-SECTION-NUM    TO ARCH-SECTION-NUM.                00134100
134200     MOVE WRK3-PKG-CODE       TO ARCH-PKG-CODE.                   00134200
134300     MOVE SPACES              TO ARCH-LOB.                        00134300
134400     MOVE SPACES              TO ARCH-PRV-CTL.                    00134400
134500     MOVE WRK3-FAM-REL-LEVEL  TO ARCH-FAM-RL.                     00134500
134600     MOVE WRK3-EFFDT-CEN      TO ARCH-EFFDT-CEN.                  00134600
134700     MOVE GCAUD-FUNC-TYPE (GCAUD-TBL-INDEX)                       00134700
134800                              TO  WS-AUD-FUNC.                    00134800
134900     MOVE WS-AUD-ID           TO ARCH-ID-CD.                      00134900
135000                                                                  00135000
135100     ADD +1  TO ARCH-POINTERS-COUNT.                              00135100
135200     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00135200
135300         GO TO 0079-EXIT.                                         00135300
135400     SET ARCH-INDEX  TO ARCH-POINTERS-COUNT.                      00135400
135500     MOVE GCAUD-DEPT-NO (GCAUD-TBL-INDEX) TO                      00135500
135600          ARCH-FLD-VALUE (ARCH-INDEX).                            00135600
135700     MOVE GCAUD-OPER-ID (GCAUD-TBL-INDEX) TO                      00135700
135800          ARCH-OPER-ID (ARCH-INDEX).                              00135800
135900     MOVE GCAUD-NONCDE-CDE-IND (GCAUD-TBL-INDEX) TO               00135900
136000          ARCH-CDE-IND (ARCH-INDEX).                              00136000
136100     MOVE GCAUD-ANLST-INIT (GCAUD-TBL-INDEX) TO                   00136100
136200          ARCH-ANLS-CD (ARCH-INDEX).                              00136200
136300     MOVE GCAUD-FUNCDT-CEN (GCAUD-TBL-INDEX) TO                   00136300
136400          ARCH-CHNG-DT-CEN (ARCH-INDEX).                          00136400
136500     MOVE GCAUD-ATB-IND (GCAUD-TBL-INDEX) TO                      00136500
136600          ARCH-ATB-IND (ARCH-INDEX).                              00136600
136700                                                                  00136700
136800     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00136800
136900                                                                  00136900
137000 0079-EXIT.                                                       00137000
137100     EXIT.                                                        00137100
137200/                                                                 00137200
137300*MAM G&R - ADDED PARAGRAPH TO FORMAT QUAL CTRL FILE               00137300
137400*007910-FRMT-QUAL-CTRL-FILE.                                      00137400
137500*                                                                 00137500
137600*    INITIALIZE QCF-RECORD.                                       00137600
137700*    MOVE ZEROS               TO QCF-FRST-PRT-GROUP-NO            00137700
137800*                                QCF-FRST-PRT-SECTION-NO.         00137800
137900*    MOVE SPACES              TO QCF-L-O-B                        00137900
138000*                                QCF-PROV-CTRL.                   00138000
138100*    MOVE 'G'                 TO QCF-CONTRACT-GROUP-SP-IND.       00138100
138200*    MOVE WRK3-GROUP-NO       TO QCF-SCND-PRT-GROUP-NO.           00138200
138300*    MOVE WRK3-SECT-NO        TO QCF-SCND-PRT-SECTION-NO.         00138300
138400*    MOVE WRK3-FAM-REL-LEVEL  TO QCF-FAM-REL-LEVEL.               00138400
138500*    MOVE PARM-LOCATION       TO QCF-PLAN-CODE.                   00138500
138600*                                                                 00138600
138700*    IF WRK3-EFF-DATE < 99999                                     00138700
138800*       COMPUTE QCF-EFF-DATE = +1900000 + WRK3-EFF-DATE.          00138800
138900*                                                                 00138900
139000*    MOVE GCAUD-FUNC-TYPE (GCAUD-TBL-INDEX)  TO QCF-FUNC-FIELD.   00139000
139100*    MOVE GCAUD-DEPT-NO (GCAUD-TBL-INDEX)    TO QCF-DEPT-NUM.     00139100
139200*    MOVE GCAUD-OPER-ID (GCAUD-TBL-INDEX)    TO QCF-OPERATOR-ID.  00139200
139300*    MOVE GCAUD-ANLST-INIT (GCAUD-TBL-INDEX) TO QCF-ANLS-INIT.    00139300
139400*                                                                 00139400
139500*    IF GCAUD-FUNC-DATE (GCAUD-TBL-INDEX) < 99999                 00139500
139600*       COMPUTE QCF-FUNC-DATE =                                   00139600
139700*              +1900000 + GCAUD-FUNC-DATE (GCAUD-TBL-INDEX).      00139700
139800*                                                                 00139800
139900*    PERFORM 0084-WRITE-QUAL-CTRL THRU 0084-EXIT.                 00139900
140000*                                                                 00140000
140100*007910-EXIT.                                                     00140100
140200*    EXIT.                                                        00140200
140300                                                                  00140300
140400 0080-ADD-ARCH-GRPS-RECORD.                                       00140400
140500***  ADD ARCHIVED RECORD FROM BEFORE-IMAGE RECORD ***             00140500
140600                                                                  00140600
140700*MAM G&R - ADDED QUAL CTRL LOGIC                                  00140700
140800     INITIALIZE QCF-RECORD.                                       00140800
140900     MOVE SPACES    TO ARCH-GC-KEY                                00140900
141000                       QCF-FILLER                                 00141000
141100                       QCF-L-O-B                                  00141100
141200                       QCF-PROV-CTRL.                             00141200
141300*                      QCF-ANLS-INIT                              00141300
141400*                      QCF-DEPT-NUM                               00141400
141500     MOVE ZEROS     TO ARCH-EFFDT-CEN                             00141500
141600                       ARCH-POINTERS-COUNT                        00141600
141700                       QCF-PLAN-CODE                              00141700
141800                       QCF-GROUP-NO                               00141800
141900                       QCF-SECTION-NO                             00141900
142000                       QCF-PKG-CODE.                              00142000
142100     MOVE 'G'       TO ARCH-STA-CD                                00142100
142200                       QCF-CONTRACT-GROUP-SP-IND.                 00142200
142300     MOVE PARM-LOCATION  TO QCF-PLAN-CODE.                        00142300
142400                                                                  00142400
142500     MOVE GCG-PLAN-CODE  OF BIM-RECORD TO ARCH-PLAN-CODE          00142500
142600                                          QCF-PLAN-CODE.          00142600
142700     MOVE GCG-GROUP-NUM  OF BIM-RECORD TO ARCH-GROUP-NUM          00142700
142800                                          QCF-GROUP-NO.           00142800
142900     MOVE GCG-SECTION-NUM OF BIM-RECORD TO ARCH-SECTION-NUM       00142900
143000                                          QCF-SECTION-NO.         00143000
143100     MOVE GCG-PKG-CODE    OF BIM-RECORD TO ARCH-PKG-CODE          00143100
143200                                           QCF-PKG-CODE.          00143200
143300     MOVE GCG-FAM-REL-LVL OF BIM-RECORD TO ARCH-FAM-RL            00143300
143400                                           QCF-FAM-REL-LEVEL.     00143400
143500     MOVE GCG-EFFDT-CEN   OF BIM-RECORD TO ARCH-EFFDT-CEN.        00143500
143600                                                                  00143600
143700                                                                  00143700
143800     IF GCG-EFFDT-CEN OF BIM-RECORD  < 9999999                    00143800
143900        MOVE GCG-EFFDT-CEN OF BIM-RECORD TO QCF-EFF-DATE.         00143900
144000                                                                  00144000
144100     IF TODAYS-DATE < 9999999                                     00144100
144200        MOVE TODAYS-DATE TO QCF-FUNC-DATE.                        00144200
144300                                                                  00144300
144400     SET  GCG-A-INDEX   TO GCG-A-INDX.                            00144400
144500     MOVE GCG-A-DE-ID (GCG-A-INDEX)     TO ARCH-ID-CD             00144500
144600                                           QCF-FUNC-FIELD.        00144600
144700     MOVE FIELDS-BEFORE (GCG-B-INDX)    TO QCF-BIM-FIELD.         00144700
144800     MOVE FIELDS-AFTER (GCG-A-INDX)     TO QCF-AIM-FIELD.         00144800
144900                                                                  00144900
145000                                                                  00145000
145100     ADD +1         TO ARCH-POINTERS-COUNT.                       00145100
145200     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00145200
145300         GO TO 0080-EXIT.                                         00145300
145400     SET ARCH-INDEX TO ARCH-POINTERS-COUNT.                       00145400
145500                                                                  00145500
145600     MOVE GCG-A-DE-VALUE (GCG-A-INDEX) TO                         00145600
145700                  ARCH-FLD-VALUE (ARCH-INDEX).                    00145700
145800     MOVE WRK-OPERATOR-ID  TO ARCH-OPER-ID (ARCH-INDEX)           00145800
145900                              QCF-OPERATOR-ID.                    00145900
146000                                                                  00146000
146100     IF GCG2-INTER-RELATIONAL-CODE = ZEROS                        00146100
146200        MOVE 'N' TO WS-INTER-REL-SW                               00146200
146300        ADD +1   TO WS-BYPASS-CNT                                 00146300
146400     ELSE                                                         00146400
146500        MOVE 'Y' TO WS-INTER-REL-SW.                              00146500
146600                                                                  00146600
146700     IF WRK-OPERATOR-ID > SPACES AND                              00146700
146800        INTER-REL-CD-FND                                          00146800
146900        PERFORM 0084-WRITE-QUAL-CTRL THRU 0084-EXIT.              00146900
147000                                                                  00147000
147100     IF WRK-CDE-SP NOT = '2 '                                     00147100
147200        MOVE 'X'  TO ARCH-CDE-IND (ARCH-INDEX)                    00147200
147300     ELSE                                                         00147300
147400        MOVE ' '  TO ARCH-CDE-IND (ARCH-INDEX).                   00147400
147500                                                                  00147500
147600     MOVE TODAYS-DATE         TO ARCH-CHNG-DT-CEN (ARCH-INDEX).   00147600
147700     MOVE SPACES              TO ARCH-ANLS-CD (ARCH-INDEX).       00147700
147800                                                                  00147800
147900     IF WRK-ATB3-REQUEST                                          00147900
148000        MOVE WRK-SIGNAL-FROM-ONLINE TO ARCH-ATB-IND (ARCH-INDEX)  00148000
148100     ELSE                                                         00148100
148200        MOVE WRK-TYPE-MAINT-IND     TO ARCH-ATB-IND (ARCH-INDEX). 00148200
148300                                                                  00148300
148400     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00148400
148500                                                                  00148500
148600 0080-EXIT.                                                       00148600
148700     EXIT.                                                        00148700
148800/                                                                 00148800
148900 0081-ADD-ARCH-TAB-RECORD.                                        00148900
149000*** ADD ARCHIVED RECORD FROM AFTER-IMAGE RECORD ***               00149000
149100                                                                  00149100
149200*MAM G&R - ADDED QUAL CTRL LOGIC                                  00149200
149300     INITIALIZE QCF-RECORD.                                       00149300
149400     MOVE SPACES    TO ARCH-GC-KEY                                00149400
149500                       QCF-FILLER                                 00149500
149600                       QCF-L-O-B                                  00149600
149700                       QCF-PROV-CTRL.                             00149700
149800*                      QCF-ANLS-INIT                              00149800
149900*                      QCF-DEPT-NUM                               00149900
150000     MOVE ZEROS     TO ARCH-EFFDT-CEN                             00150000
150100                       ARCH-POINTERS-COUNT                        00150100
150200                       QCF-PLAN-CODE                              00150200
150300                       QCF-GROUP-NO                               00150300
150400                       QCF-SECTION-NO                             00150400
150500                       QCF-PKG-CODE.                              00150500
150600     MOVE 'G'       TO ARCH-STA-CD                                00150600
150700                       QCF-CONTRACT-GROUP-SP-IND.                 00150700
150800                                                                  00150800
150900     MOVE WRK2-OPERATOR-ID         TO QCF-OPERATOR-ID.            00150900
151000     MOVE PARM-LOCATION            TO QCF-PLAN-CODE.              00151000
151100     MOVE TAB-ARCH-DE-CD           TO QCF-FUNC-FIELD.             00151100
151200     MOVE 'ADDED'                  TO QCF-BIM-FIELD.              00151200
151300     MOVE GCG2-TAB-ID (GCG2-INDEX) TO QCF-AIM-FIELD.              00151300
151400                                                                  00151400
151500     MOVE GCG2-PLAN-CODE OF RGS-RECORD TO ARCH-PLAN-CODE          00151500
151600                                          QCF-PLAN-CODE.          00151600
151700     MOVE GCG2-GROUP-NUM OF RGS-RECORD TO ARCH-GROUP-NUM          00151700
151800                                          QCF-GROUP-NO.           00151800
151900     MOVE GCG2-SECTION-NUM OF RGS-RECORD TO ARCH-SECTION-NUM      00151900
152000                                          QCF-SECTION-NO.         00152000
152100     MOVE GCG2-PKG-CODE    OF RGS-RECORD TO ARCH-PKG-CODE         00152100
152200                                            QCF-PKG-CODE.         00152200
152300     MOVE GCG2-FAM-REL-LVL OF RGS-RECORD TO ARCH-FAM-RL           00152300
152400                                            QCF-FAM-REL-LEVEL.    00152400
152500     MOVE GCG2-EFFDT-CEN OF RGS-RECORD TO ARCH-EFFDT-CEN.         00152500
152600                                                                  00152600
152700     IF GCG2-EFFDT-CEN OF RGS-RECORD < 9999999                    00152700
152800        MOVE GCG2-EFFDT-CEN OF RGS-RECORD TO QCF-EFF-DATE.        00152800
152900                                                                  00152900
153000     IF TODAYS-DATE < 9999999                                     00153000
153100        MOVE TODAYS-DATE TO QCF-FUNC-DATE.                        00153100
153200                                                                  00153200
153300     IF GCG2-INTER-RELATIONAL-CODE = ZEROS                        00153300
153400        MOVE 'N' TO WS-INTER-REL-SW                               00153400
153500        ADD +1   TO WS-BYPASS-CNT                                 00153500
153600     ELSE                                                         00153600
153700        MOVE 'Y' TO WS-INTER-REL-SW.                              00153700
153800                                                                  00153800
153900     IF WRK-OPERATOR-ID > SPACES AND                              00153900
154000        INTER-REL-CD-FND                                          00154000
154100        PERFORM 0084-WRITE-QUAL-CTRL THRU 0084-EXIT.              00154100
154200                                                                  00154200
154300     MOVE TAB-ARCH-DE-CD  TO  ARCH-ID-CD.                         00154300
154400                                                                  00154400
154500     ADD +1         TO ARCH-POINTERS-COUNT.                       00154500
154600     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00154600
154700         GO TO 0081-EXIT.                                         00154700
154800     SET ARCH-INDEX TO ARCH-POINTERS-COUNT.                       00154800
154900                                                                  00154900
155000     MOVE '++ADDED++'      TO ARCH-FLD-VALUE (ARCH-INDEX).        00155000
155100     MOVE WRK2-OPERATOR-ID TO ARCH-OPER-ID (ARCH-INDEX).          00155100
155200                                                                  00155200
155300     IF WRK2-CDE-SP NOT = '2 '                                    00155300
155400        MOVE 'X'  TO ARCH-CDE-IND (ARCH-INDEX)                    00155400
155500     ELSE                                                         00155500
155600        MOVE ' '  TO ARCH-CDE-IND (ARCH-INDEX).                   00155600
155700                                                                  00155700
155800     MOVE TODAYS-DATE      TO ARCH-CHNG-DT-CEN (ARCH-INDEX).      00155800
155900     MOVE SPACES           TO ARCH-ANLS-CD (ARCH-INDEX).          00155900
156000                                                                  00156000
156100     IF WRK2-ATB3-REQUEST                                         00156100
156200        MOVE WRK2-SIGNAL-FROM-ONLINE TO ARCH-ATB-IND (ARCH-INDEX) 00156200
156300     ELSE                                                         00156300
156400        MOVE WRK2-TYPE-MAINT-IND     TO ARCH-ATB-IND (ARCH-INDEX).00156400
156500                                                                  00156500
156600     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00156600
156700                                                                  00156700
156800 0081-EXIT.                                                       00156800
156900     EXIT.                                                        00156900
157000/                                                                 00157000
157100 0082-ADD-ARCH-TAB-RECORD.                                        00157100
157200***  ADD ARCHIVED RECORD FROM BEFORE IMAGE CONTRACT RECORD ***    00157200
157300                                                                  00157300
157400*MAM G&R - ADDED QUAL CTRL LOGIC                                  00157400
157500                                                                  00157500
157600     INITIALIZE QCF-RECORD.                                       00157600
157700     MOVE SPACES   TO ARCH-GC-KEY                                 00157700
157800                      QCF-FILLER                                  00157800
157900                      QCF-L-O-B                                   00157900
158000                      QCF-PROV-CTRL.                              00158000
158100*                     QCF-ANLS-INIT                               00158100
158200*                     QCF-DEPT-NUM                                00158200
158300     MOVE ZEROS    TO ARCH-EFFDT-CEN                              00158300
158400                      ARCH-POINTERS-COUNT                         00158400
158500                      QCF-PLAN-CODE                               00158500
158600                      QCF-GROUP-NO                                00158600
158700                      QCF-SECTION-NO                              00158700
158800                      QCF-PKG-CODE.                               00158800
158900     MOVE 'G'      TO ARCH-STA-CD                                 00158900
159000                      QCF-CONTRACT-GROUP-SP-IND.                  00159000
159100                                                                  00159100
159200     MOVE GCG-PLAN-CODE  OF BIM-RECORD TO ARCH-PLAN-CODE          00159200
159300                                          QCF-PLAN-CODE.          00159300
159400     MOVE GCG-GROUP-NUM  OF BIM-RECORD TO ARCH-GROUP-NUM          00159400
159500                                          QCF-GROUP-NO.           00159500
159600     MOVE GCG-SECTION-NUM OF BIM-RECORD TO ARCH-SECTION-NUM       00159600
159700                                          QCF-SECTION-NO.         00159700
159800     MOVE GCG-PKG-CODE    OF BIM-RECORD TO ARCH-PKG-CODE          00159800
159900                                           QCF-PKG-CODE.          00159900
160000     MOVE GCG-FAM-REL-LVL OF BIM-RECORD TO ARCH-FAM-RL            00160000
160100                                           QCF-FAM-REL-LEVEL.     00160100
160200     MOVE GCG-EFFDT-CEN  OF BIM-RECORD TO ARCH-EFFDT-CEN.         00160200
160300                                                                  00160300
160400     IF GCG-EFFDT-CEN OF BIM-RECORD  < 9999999                    00160400
160500        MOVE GCG-EFFDT-CEN OF BIM-RECORD TO QCF-EFF-DATE.         00160500
160600                                                                  00160600
160700     MOVE PARM-LOCATION    TO QCF-PLAN-CODE.                      00160700
160800     MOVE WRK-OPERATOR-ID  TO QCF-OPERATOR-ID.                    00160800
160900     MOVE TAB-ARCH-DE-CD   TO ARCH-ID-CD                          00160900
161000                              QCF-FUNC-FIELD.                     00161000
161100                                                                  00161100
161200     IF GCG2-TAB-ID (GCG2-INDEX) GREATER THAN                     00161200
161300        GCG-TAB-ID (GCG-INDEX)                                    00161300
161400         MOVE GCG-TAB-ID (GCG-INDEX) TO QCF-BIM-FIELD             00161400
161500         MOVE 'DELETED'              TO QCF-AIM-FIELD             00161500
161600     ELSE                                                         00161600
161700        IF GCG2-TAB-ID (GCG2-INDEX) EQUAL                         00161700
161800           GCG-TAB-ID (GCG-INDEX)                                 00161800
161900            MOVE GCG-TAB-SLOT-NO (GCG-INDEX)   TO QCF-BIM-FIELD   00161900
162000            MOVE GCG2-TAB-SLOT-NO (GCG2-INDEX) TO QCF-AIM-FIELD   00162000
162100        END-IF                                                    00162100
162200     END-IF.                                                      00162200
162300                                                                  00162300
162400     IF TODAYS-DATE < 9999999                                     00162400
162500        MOVE TODAYS-DATE TO QCF-FUNC-DATE.                        00162500
162600                                                                  00162600
162700     IF GCG2-INTER-RELATIONAL-CODE = ZEROS                        00162700
162800        MOVE 'N' TO WS-INTER-REL-SW                               00162800
162900        ADD +1   TO WS-BYPASS-CNT                                 00162900
163000     ELSE                                                         00163000
163100        MOVE 'Y' TO WS-INTER-REL-SW.                              00163100
163200                                                                  00163200
163300     IF WRK-OPERATOR-ID > SPACES AND                              00163300
163400        INTER-REL-CD-FND                                          00163400
163500        PERFORM 0084-WRITE-QUAL-CTRL THRU 0084-EXIT.              00163500
163600                                                                  00163600
163700     ADD +1         TO ARCH-POINTERS-COUNT.                       00163700
163800     IF ARCH-POINTERS-COUNT > GC-ARCHIVE-VARY-MAX-OCUR            00163800
163900         GO TO 0082-EXIT.                                         00163900
164000     SET ARCH-INDEX TO ARCH-POINTERS-COUNT.                       00164000
164100                                                                  00164100
164200     MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                          00164200
164300                              ARCH-FLD-VALUE (ARCH-INDEX).        00164300
164400     MOVE WRK-OPERATOR-ID  TO ARCH-OPER-ID (ARCH-INDEX).          00164400
164500                                                                  00164500
164600     IF WRK-CDE-SP NOT = '2 '                                     00164600
164700        MOVE 'X'  TO ARCH-CDE-IND (ARCH-INDEX)                    00164700
164800     ELSE                                                         00164800
164900        MOVE ' '  TO ARCH-CDE-IND (ARCH-INDEX).                   00164900
165000                                                                  00165000
165100     MOVE TODAYS-DATE         TO ARCH-CHNG-DT-CEN (ARCH-INDEX).   00165100
165200     MOVE SPACES              TO ARCH-ANLS-CD (ARCH-INDEX).       00165200
165300                                                                  00165300
165400     IF WRK-ATB3-REQUEST                                          00165400
165500        MOVE WRK-SIGNAL-FROM-ONLINE TO ARCH-ATB-IND (ARCH-INDEX)  00165500
165600     ELSE                                                         00165600
165700        MOVE WRK-TYPE-MAINT-IND     TO ARCH-ATB-IND (ARCH-INDEX). 00165700
165800                                                                  00165800
165900     PERFORM 0085-UPDATE-ARCHIVED-FILE THRU 0085-EXIT.            00165900
166000                                                                  00166000
166100 0082-EXIT.                                                       00166100
166200     EXIT.                                                        00166200
166300/                                                                 00166300
166400*MAM G&R - ADDED PARAGRAPH TO WRITE QUAL CTRL FILE                00166400
166500 0084-WRITE-QUAL-CTRL.                                            00166500
166600                                                                  00166600
166700     WRITE QCF-RECORD.                                            00166700
166800     ADD +1 TO WS-QCF-COUNT.                                      00166800
166900                                                                  00166900
167000 0084-EXIT.                                                       00167000
167100     EXIT.                                                        00167100
167200/                                                                 00167200
167300 0085-UPDATE-ARCHIVED-FILE.                                       00167300
167400***  REWRITE/WRITE THE ARCHIVED RECORD WITH THE NEW ENTRY         00167400
167500                                                                  00167500
167600     MOVE ARCH-POINTERS-COUNT   TO PNTRS-COUNT-A.                 00167600
167700     MOVE ARCHIVED-GCPS-RECORD  TO REC-AREA-A.                    00167700
167800     COMPUTE RECORD-LENGTH-A =  GC-ARCHIVE-FIXED-LEN + 4          00167800
167900             + (GC-ARCHIVE-VARY-LEN * ARCH-POINTERS-COUNT).       00167900
168000                                                                  00168000
168100     MOVE 'W'         TO REQUEST-TYPE-A.                          00168100
168200     CALL 'TSGVSAM3'  USING PARM-ONE-A                            00168200
168300                            PARM-TWO-A.                           00168300
168400                                                                  00168400
168500     IF  REQUEST-TYPE-A NOT EQUAL 'W'                             00168500
168600         MOVE FEEDBACK-CODE-A    TO ABEND-CODE                    00168600
168700         GO TO 9999-ERROR-RTN.                                    00168700
168800                                                                  00168800
168900 0085-EXIT.                                                       00168900
169000     EXIT.                                                        00169000
169100/                                                                 00169100
169200 9999-ERROR-RTN.                                                  00169200
169300     CALL 'TSGEND' USING ABEND-CODE.                              00169300
169400 9999-EXIT.                                                       00169400
169500     EXIT.                                                        00169500
169600/                                                                 00169600
