000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.  GC024020.                                           00000200
000300*THIS IS A COBOL/2 PROGRAM                                        00000300
000400 AUTHOR. ALIDA JATICH OF T. M. FLOYD, INC.                        00000400
000500 INSTALLATION.  BLUE CROSS / BLUE SHIELD.                         00000500
000600 DATE-WRITTEN.  JUNE 30, 1986.                                    00000600
       DATE-COMPILED.                                                   00000700
                                                                        00000710
      ******************************************************************00000800
000900*    THIS PROGRAM EDITS THE GROUP SPECIFIC RECORDS USING THE     *00000900
001000*    LOGICAL EDITS DEFINED BY THE USERS.                         *00001000
001100*                                                                *00001100
001200*    THIS PROGRAM MODULE CALLED BY PROGRAM GC024000              *00001200
001300*    AND IS NOT TO BE EXECUTED AS A STAND-ALONE PROGRAM.         *00001300
001400*                                                                *00001400
001500*    TSGVSAM3 IS THE GROUP-SPECIFIC FILE.                        *00001500
001600*    TSGVSAM4 IS THE TABULAR FILE.                               *00001600
001700*                                                                *00001700
001800******************************************************************00001800
001900*                                                                *00001900
002000*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *00002000
002100*       *-*         U P D A T E   H I S T O R Y         *-*      *00002100
002200*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *00002200
002300*                                                                *00002300
002400* CHG-NUM  *-DATE-* *WHO* *-----------DESCRIPTION----------------*00002400
002500*                                                                *00002500
002600* D332      8/12/97  FRY   CHANGED THE NUMBER OF DAYS FROM       *00002600
002700*                          180 TO 546 PER AUGGIE MCADOO ON       *00002700
002800*                          THE FOLLOWING GROUP SPECIFIC FIELDS   *00002800
002900*                          FOR LOGICAL EDITS:                    *00002900
003000*                            GCG-BC-LATE-ENROLL-MEM-DAYS   'GB1' *00003000
003100*                            GCG-BS-LATE-ENROLL-MEM-DAYS   'GB2' *00003100
003200*                            GCG-MM-LATE-ENROLL-MEM-DAYS   'GB3' *00003200
003300*                            GCG-BC-LATE-ENROLL-SPS-DAYS   'GB4' *00003300
003400*                            GCG-BS-LATE-ENROLL-SPS-DAYS   'GB5' *00003400
003500*                            GCG-MM-LATE-ENROLL-SPS-DAYS   'GB6' *00003500
003600*                            GCG-BC-LATE-ENROLL-DEP-DAYS   'GB7' *00003600
003700*                            GCG-BS-LATE-ENROLL-DEP-DAYS   'GB8' *00003700
003800*                            GCG-MM-LATE-ENROLL-DEP-DAYS   'GB9' *00003800
003900*                                                                *00003900
004000* D332      6/11/97  FRY   ADD GROUP SPECIFIC LOGICAL EDITS:     *00004000
004100*                           G.S. FIELDS AND ERROR CODES:         *00004100
004200*                            GCG-BC-LATE-ENROLL-MEM-DAYS   'GB1' *00004200
004300*                            GCG-BS-LATE-ENROLL-MEM-DAYS   'GB2' *00004300
004400*                            GCG-MM-LATE-ENROLL-MEM-DAYS   'GB3' *00004400
004500*                            GCG-BC-LATE-ENROLL-SPS-DAYS   'GB4' *00004500
004600*                            GCG-BS-LATE-ENROLL-SPS-DAYS   'GB5' *00004600
004700*                            GCG-MM-LATE-ENROLL-SPS-DAYS   'GB6' *00004700
004800*                            GCG-BC-LATE-ENROLL-DEP-DAYS   'GB7' *00004800
004900*                            GCG-BS-LATE-ENROLL-DEP-DAYS   'GB8' *00004900
005000*                            GCG-MM-LATE-ENROLL-DEP-DAYS   'GB9' *00005000
005100*                                                                *00005100
005200* 14631     3/18/96  FRY   ADD LOGICAL EDITS FOR                 *00005200
005300*                          -#GCCP TABULAR                        *00005300
005400*                            -COMMUNITY BLUE (CBL)               *00005400
005500*                             (CP-ENTRY-FOUND) 'P60', P61', P62' *00005500
005600*                            -PREFERRED ANCILLARY NETWORK (PAN)  *00005600
005700*                             (PA-ENTRY-FOUND) 'P63', P64', P65' *00005700
005800*                          -#GCBL TABULAR                        *00005800
005900*                           'GAR'   GCG-CBL-PARTICIPATION-IND    *00005900
006000*                          -#GPAN TABULAR                        *00006000
006100*                           'GAS'   GCG-PAN-PARTICIPATION-IND    *00006100
006200*                                                                *00006200
006300* D11530    7/13/95  FRY   MODIFY REIMBURSEMENT/SUBROGATION      *00006300
006400*                          (R/S) EDITS.                          *00006400
006500*                                                                *00006500
006600* ?????     3/25/95  FRY   ACTIVATE RPO EDITS, PER AUGGIE        *00006600
006700*                          -RESTRICTED PROVIDER OPTION (RPO)     *00006700
006800*                           (RP-ENTRY-FOUND) 'P48', P49', P50'   *00006800
006900*                                                                *00006900
007000*           3/10/95  RMK   COMMENT OUT EDITS FOR GA5 IN ISSR     *00007000
007100*                          13272. NO LONGER NEEDED.              *00007100
007200*                                                                *00007200
007300* 14045     2/22/95  RMK   ADD LOGICAL EDITS FOR:                *00007300
007400*                          -COMMUNITY PREFERRED OPTION (CPO)     *00007400
007500*                           (CP-ENTRY-FOUND) 'P51', P52', P53'   *00007500
007600*                                                                *00007600
007700* 14045   02/14/95  KJD  ADDED LOGICAL EDITS FOR #GCPO TABULAR   *00007700
007800*                        GAO, GAP                                *00007800
007900*                                                                *00007900
008000*            01/18/95  GDM  CONVERT TO COBOL II                  *00008000
008100*                                                                *00008100
008200* 12465   09/22/94  KJD    COMMENT OUT THE 'G98' EDIT FOR        *00008200
008300*                          DOC HIGHLIGHTS                        *00008300
008400*                                                                *00008400
008500* 13272   10/20/93  KJD    THE FOLLOWING EDITS WERE ADDED:       *00008500
008600*                          GA5, GA6, GA7, GA8, GA9, GAA, GAB,    *00008600
008700*                          GAC, GAD, GAE, GAF, GAG, GAH, GAI,    *00008700
008800*                          GAJ, GAK, GAL, GAM, GAN               *00008800
008900*                                                                *00008900
009000* P-043    01/28/93  MLG   CORRECT TABULAR RECORD LENGTH.        *00009000
009100*                                                                *00009100
009200* 12730    12/07/92  FRY   RELAX EDITS UNTIL FURTHER NOTICE FOR  *00009200
009300*                          -RESTRICTED PROVIDER OPTION (RPO)     *00009300
009400*                           PER AUGGIE MCADOO                    *00009400
009500*                           (RP-ENTRY-FOUND) 'P48', P49', P50'   *00009500
009600*                           GROUP SPECIFIC ===> 'GA3'            *00009600
009700*                                                                *00009700
009800* 12730    12/06/92  FRY   ADD LOGICAL EDITS FOR:                *00009800
009900*                          -RESTRICTED PROVIDER OPTION (RPO)     *00009900
010000*                           (RP-ENTRY-FOUND) 'P48', P49', P50'   *00010000
010100*                                                                *00010100
010200* 12262  12/05/92  ENW  ADDED LOGICAL EDITS FOR #GRPO TABULAR    *00010200
010300*                                                                *00010300
010400* 12262  06/06/91  FRY  ADDED LOGICAL EDITS FOR #GMCS TABULAR    *00010400
010500*                                                                *00010500
010600* 12009  09/19/91  TPM  EXPANSION OF THE FAMILY-RELATION FIELD.  *00010600
010700*                       CHANGED THE RECORD LENGTH FROM 18 TO 19  *00010700
010800*                       WHEN CALLING THE TSGVSAM ROUTINE.        *00010800
010900*                                                                *00010900
011000* 12009  09/10/91  GDM  INCREASE H-FAM-REL-LVL                   *00011000
011100*                                LG-FAM-REL-LVL TO 2 POSITION    *00011100
011200*                                                                *00011200
011300* D11836      6/06/91  FRY  ADDED LOGICAL EDITS FOR:             *00011300
011400*                     -POINT OF SERVICE                          *00011400
011500*                        (P1-ENTRY-FOUND)    'P45', P46', P47'   *00011500
011600*                     -MENTAL HEALTH SUBSTANCE ABUSE CARE (MHSC) *00011600
011700*                        (S1-ENTRY-FOUND)    'P42', P43', P44'   *00011700
011800*                                                                *00011800
011900* D11836      6/01/91  FRY  ADDED LOGICAL EDITS FOR:             *00011900
012000*                            GCG-NEW-MEN-SUB-ABUSE-IND  'GA0'    *00012000
012100*                            GCG-NEW-POS-IND            'GA1'    *00012100
012200*                                                                *00012200
012300* D00608      3/11/91  FRY  MODIFY LOGIC FOR ERROR CODE 'P35'.   *00012300
012400*                                                                *00012400
012500* D272/273    2/21/91  PFH    ADDED LOGICAL EDITS FOR THE        *00012500
012600*  275                        FOLLOWING FIELDS:                  *00012600
012700*                               GCG-MEDCR-ACCM-BEN-PERD-IND      *00012700
012800*                               GCG-MEDCR-ACCM-L-O-B             *00012800
012900*                               GCG-ST-PRGM-ACCUM-BEN-PRD-IND    *00012900
013000*                               GCG-ST-PRGM-ACCUM-L-O-B-IND      *00013000
013100*                               GCG-PRODUCT-TYPE                 *00013100
013200*                               GCG-PRODUCT-TYPE-IND             *00013200
013300*                                                                *00013300
013400*  11161     11/15/90  FRY  EXPANDED GROUP SPECIFIC INDICATOR    *00013400
013500*                           FIELDS FROM 1 POSITION TO 2          *00013500
013600*                           POSITIONS FIELDS THAT WERE AFFECTED: *00013600
013700*                            GCG-PARTICIPAT-PROV-OPTION          *00013700
013800*                            GCG-PRE-ADM-TESTING-PROGRAM         *00013800
013900*                            GCG-SUBS-ABUSE-MENTAL-IND           *00013900
014000*                                                                *00014000
014100*  11161     11/05/90  GDM  EXPANDED GROUP SPECIFIC INDICATOR    *00014100
014200*                           FIELDS FROM 1 POSITION TO 2          *00014200
014300*                           POSITIONS FIELDS THAT WERE AFFECTED  *00014300
014400*                            GCG-ADDL-TRNSPLNT-COVRG-IND         *00014400
014500*                            GCG-FRI-SAT-ADM-IND                 *00014500
014600*                            GCG-HOSPICE-IND                     *00014600
014700*                            GCG-INCENTIVE-IND                   *00014700
014800*                            GCG-MAND-ADDL-SURG-OPN-IND          *00014800
014900*                            GCG-MAND-OP-SURG-PROG-IND           *00014900
015000*                            GCG-MED-SERV-ADV-PROG-IND           *00015000
015100*                            GCG-PRE-ADM-REVIEW-IND              *00015100
015200*                            GCG-REIMBUR-SUBROG-IND              *00015200
015300*                            GCG-MONDAY-DISCHARGE-IND            *00015300
015400*                                                                *00015400
015500*   11161    10/19/90  GDM    THE FOLLOWING FIELDS WERE          *00015500
015600*                             EXPANDED FROM 1 TO 2 POSITIONS     *00015600
015700*                               GSS-AT-BC-CALC-METHOD            *00015700
015800*                               GSS-AT-BS-CALC-METHOD            *00015800
015900*                               GSS-PR-BC-IND                    *00015900
016000*                               GSS-PR-BS-IND                    *00016000
016100*                               GSS-PS-BS-IND                    *00016100
016200*                               GSS-PR-MM-IND                    *00016200
016300*                                                                *00016300
016400*  11161     10/18/90  PFH  CHANGED HARDCODED GCCP (39) OCCURANCE*00016400
016500*                           TO REFERENCE GCCDRLEN GCCP MAX OCCURS*00016500
016600*                           FIELD.                               *00016600
016700*                                                                *00016700
016800*  D249.01   09/05/90  APH  1. MODIFIED #GMCD/#GMCR W/ #GCCP     *00016800
016900*                              EDITS PER LRH:                    *00016900
017000*                              WHEN POS BC/BS/MM IND HAVE VALUE  *00017000
017100*                              OF '00', THEY ARE CONSIDERED      *00017100
017200*                              'NOT LOADED'.                     *00017200
017300*                                                                *00017300
017400*  D249.01   08/24/90  APH  1. ADDED LOGIC FOR NEW TABULARS:     *00017400
017500*                              #GMCD AND #GMCR.                  *00017500
017600*                           2. CORRECTED ENTRY COUNT FOR #GCCP   *00017600
017700*                              FROM +14 TO +39 IN 11000-READ-TABS*00017700
017800*                                                                *00017800
017900*  P????     08/13/90  APH  1. CHANGED LOGICAL EDITS FOR #GCCP   *00017900
018000*                              RS OCCURS ON BC AND BS INVESTIGA- *00018000
018100*                              TION INDICATORS.                  *00018100
018200*                           2. RE-WORDED ERROR MESSAGES IN       *00018200
018300*                              \
018400*                                                                *00018400
018500*  P????     07/25/90  APH  1. REMOVED DISPLAYS FOR #GVL'S       *00018500
018600*                           2. CORRECTED INDEX NAME FOR #GCCP    *00018600
018700*                              RS OCCURS ENTRY                   *00018700
018800*                                                                *00018800
018900*  D249      06/28/90  GDM  ADDED EDITS G91                      *00018900
019000*                                                                *00019000
019100*  D199    12/18/89  AHL  ADDED EDITS FOR #GVLF/#GVLG/#GVLH AND  *00019100
019200*                         #GVLP/#GVLQ/#GVLR IN GROUP SPEC RECORD *00019200
019300*                                                                *00019300
019400*  D234    12/04/89  AHL  ADDED EDITS FOR IO AND RS OCCURS FOR   *00019400
019500*                         #GCCP TABULAR                          *00019500
019600*                                                                *00019600
019700*  D222    07/11/89  ENW  ADDED CALL TO GC024015 FOR ACCUM-EDITS. 00019700
019800*                         THIS REPLACED THE 22000-D210-EDIT       00019800
019900*                                                                *00019900
020000*   D215     05/19/89  ENW  ADDED EDITS G79, G80.                 00020000
020100*   D214     04/13/89  ENW  ADDED EDITS G74, G75, G76, G77.      *00020100
020200*                                                                *00020200
020300*   M422     02/24/89  ENW  ADDED EDIT G68.                       00020300
020400*                                                                 00020400
020500*   M426     02/24/89  ENW  ADDED EDIT G69, G70, G71, G72, G73.   00020500
020600*                                                                *00020600
020700*   D196     11/16/88  ENW   ADDED EDITS FOR NEW SUBSTANCE ABUSE/*00020700
020800*                            MENTAL OCCURS FOR #GCCP.            *00020800
020900*                                                                *00020900
021000*   D177      2/08/88  FCG    THE FOLLOWING (9) FIELDS WERE      *00021000
021100*  AKA FEBRUARY GROUP SPEC.   EXPANDED FROM 1 TO 2 POSITIONS     *00021100
021200*      FILE CONVERSION.         GCG-BC-TERMN-BEN-EXTEN-IND       *00021200
021300*                               GCG-BS-TERMN-BEN-EXTEN-IND       *00021300
021400*                               GCG-MM-TERMN-BEN-EXTEN-IND       *00021400
021500*                               GCG-BC-WAIVR-IND                 *00021500
021600*                               GCG-BS-WAIVR-IND                 *00021600
021700*                               GCG-MM-WAIVR-IND                 *00021700
021800*                               GCG-BC-WAITG-PERD-IND            *00021800
021900*                               GCG-BS-WAITG-PERD-IND            *00021900
022000*                               GCG-MM-WAITG-PERD-IND.           *00022000
022100*                                                                *00022100
022200*  14726      1/12/98  GSP    INCREASED HOLD-PSEUDO-SEC-NBR TO   *00022200
022300*                             PIC X(5) AND CHANGED NAME TO       *00022300
022400*                             HOLD-PSEUDO-SECTION-NBR.           *00022400
022500*                             CHANGED GCG-ACCUM-PSEUDO-SEC-NBR   *00022500
022600*                             TO GCG-ACCUM-PSEUDO-SECTION-NBR.   *00022600
022700*                                                                *00022700
022800*  D338      04/28/98  GSP  ADDED EDITS GC5 AND GC6.             *00022800
022900*                                                                *00022900
023000*  D15182    11/24/98  GDM  ADD LOGIC TO SUPPORT NEW #ACP ACCUM  *00023000
023100*                           TABULAR                              *00023100
023200*                                                                *00023200
023300*            02/04/99  KJD  ADDED EXTRA DISPLAY INFO AND COMMENT *00023300
023400*                           OUT DF-ERROR-COUNT MOVE TO ITSELF    *00023400
023500*                                                                *00023500
023600* D15380   03/24/99  GDM   ADDED LOGICAL EDITS FOR #GBAE TABULAR *00023600
023700*                                                                *00023700
023800* PROD     03/30/99  KJD   FIX 20040-AOL EDITS TO USE THE CORRECT*00023800
023900*                          FIELD FOR ENTRY COUNT                 *00023900
024000*                                                                *00024000
024100* P???     12/22/99  FRY   CORRECTED THE FOLLOWING COMPARE.      *00024100
024200*                          CHANGED LOGIC TO REFERENCE CENTURY    *00024200
024300*                          DATE FIELDS.                          *00024300
024400*                     FROM:  IF GCG-EFF-DT > GCG-TERMN-DT        *00024400
024500*                       TO:  IF GCG-EFFDT-CEN > GCG-TERMDT-CEN   *00024500
024600*                                                                *00024600
024700* D-351     9/14/00  GSP   ADDED EDITS FOR HMO MANAGED CARE      *00024700
024800*                          (HM OCCURRANCE ON THE GCCP RECORD).   *00024800
024900*                          ERROR CODES: GAT, GAU, P66, P67 &     *00024900
025000*                          P68.                                  *00025000
025100*                                                                *00025100
025200* D-351    12/20/00  GSP   COMBINED EDITS FOR 'P1' AND 'HM'      *00025200
025300*                          OCCURANCES ON THE #GCCP TABULAR AS    *00025300
025400*                          COMPARED WITH THE #GMCS TABULAR.      *00025400
025500*                          ADDED NEW 'GAV' ERROR MESSAGE.        *00025500
025600*                                                                *00025600
025700* D365A/B   6/18/02   JP   ADDED EDITS FOR NEW FIELDS -          *00025700
025800*                          GCG-ACCM-REL-IND AND ACCUM COMB       *00025800
025900*                          APPLIED IND (CAPI).  ADDED ERROR      *00025900
026000*                          CODES 'GAW' & 'GAX'.                  *00026000
026100*                                                                *00026100
026200* P01760   10/01/02  GTF   EXPANDED OPERATOR ID FROM 5 TO 8 BYTES*00026200
026300*                                                                *00026300
026200* 9-19-2016   RECOMPLIE PGM EXPANDED OCCURS VALUE IN GCTGPPO2    *00026310
026300*                                                                *00026320
026400******************************************************************00026400
026500**** THE FOLLOWING LOGIC CHANGES WERE OVERLOOKED WHEN THE WAITING*00026500
026600*    PERIOD DAYS WERE SPLIT INTO MEMBER, SPOUSE AND DEPENDANT    *00026600
026700*    WAITING PERIOD DAYS.                                        *00026700
026800*      DELETED LOGIC FOR FIELD GCG-MM-WAITG-PERD-DAYS THAT HAS   *00026800
026900*      BEEN DELETED FROM THE GROUP SPECIFIC RDW.                 *00026900
027000*      ADDED LOGIC FOR NEW FIELDS GCG-MM-WAITG-PERD-MEM-DAYS,    *00027000
027100*      GCG-MM-WAITG-PERD-SPS-DAYS AND GCG-MM-WAITG-PERD-DEP-DAYS *00027100
027200*      IN G35 LOGIC COMPARE.    F.C.G. 02/15/88.                 *00027200
027300*                                                                *00027300
027400*  THE FOLLOWING EDITS WERE REMOVED BY ENW ON 2/16/88 BECAUSE THE*00027400
027500*  WRONG VERSION OF THE PROGRAM WAS MOVED TO AND WORKED ON IN    *00027500
027600*  RELEASE FOR WORK ORDER D177.                                   00027600
027700*                                                                 00027700
027800*   M217     11/30/87  ENW  REMOVED EDITS FOR MOPS BS/BC/MM IND   00027800
027900*                           AND #GMPB. ALSO, MASOP BS/BC/MM IND & 00027900
028000*                           #GMOB AND #GMOR.                      00028000
028100*                                                                 00028100
028200*   D108     10/20/87  ENW  ADDED NEW CHECK FOR PSEUDO-GRP NBR.   00028200
028300*                           IF 1ST POSITION FOR INTER-RELATIONAL *00028300
028400*                           CODE = 9, THEN SKIP EDIT.            *00028400
028500*                                                                *00028500
028600*   P4129    09/18/87  ENW  REVISED G47.                          00028600
028700*                                                                *00028700
028800*    D116    09/09/87  FRY  CAPTURE OPERATOR-ID.                 *00028800
028900*                                                                *00028900
029000*    M189    08/31/87  ENW  ADDED NEW PSEUDO GROUP NBR EDIT. SEE  00029000
029100*                           COMMENTS FOR MORE INFO.               00029100
029200*                                                                 00029200
029300*    M189    08/18/87  ENW  COMMENTED OUT EDITS FOR WAITING PERIOD00029300
029400*                           IND. AND WAIVER IND.                  00029400
029500*                                                                 00029500
029600*    M184    07/20/87  AHL  ADDED VALUE '4' TO EDIT BETWEEN       00029600
029700*                           #GMDN AND MM PAYMENT INDICATOR        00029700
029800*    M183    07/07/87  ENW  MADE THE FOLLOWING CHANGES:           00029800
029900*                        1. REVISED EDIT BETWEEN MOPS(BC,BS,MM)   00029900
030000*                           AND THE #GMPR TABULAR. CHANGED ERROR  00030000
030100*                           CODE FROM G23 AND G24 TO G59.         00030100
030200*                        2. REVISED EDIT BETWEEN MASOP(BC,BS,MM)  00030200
030300*                           AND THE #GMOR TABULAR. CHANGED ERROR  00030300
030400*                           CODE FROM G32 AND G33 TO G61.         00030400
030500*    D164    06/18/87  ENW  REVISED EDITS --                      00030500
030600*                           ADDED VALUE 'B' FOR G39 AND G40.      00030600
030700*    XXXX    06/04/87  ENW  REVISED EDITS --                      00030700
030800*                           ADDED NEW VALUES FOR #GMPR EDIT.      00030800
030900*                           REMOVED CODE FOR BC EDIT IN #GMPB.    00030900
031000*    XXXX    05/01/87  ENW  REVISED EDITS -- G23, G24, G25.       00031000
031100*                           ADD NEW EDIT  -- G62.                 00031100
031200*    D156    04/23/87  ENW  REVISED EDITS -- G53 FIXED.           00031200
031300*                           ENHANCED G39, G40, G41                00031300
031400*    XXXX    03/25/87  ENW  REVISED EDITS -- G60 REMOVED.        *00031400
031500*                                  G36, G37, G38 REVISED.        *00031500
031600*    XXXX    11/14/86  FRY  REVISED EDITS.                       *00031600
031700*    XXXX    10/06/86  ENW  REVISED INCORRECT EDITS.             *00031700
031800*    XXXX    07/23/86  AMJ  ADDED MORE ERROR CODES               *00031800
031900*                           (DISTINGUISH BC/BS/MM)               *00031900
032000*    XXXX    06/30/86  AMJ  ORIGINAL MODULE WRITTEN              *00032000
032100*                                                                *00032100
032200*  14726/                                                         00032200
032300*  15057     09/11/97  AB   ADDED CODE TO SUPPORT THE YEAR        00032300
032400*                           2000 AND THE EXPANSION OF THE         00032400
032500*                           CONTRACT KEY TO SUPPORT THE TX        00032500
032600*                           MERGER.                               00032600
032700* D-356A   5/08/03  GTF  RECOMPILE FOR COPYBK CHANGES.            00032700
032800*                                                                *00032800
032900* P02384   09/15/05  GDM   ADD LOGIC FOR TABULARS: #GFHC, #GFSA  *00032900
033000*                          #GHCA, #GHSA, #GLPF, #GLPH, #GWHC     *00033000
033100*                                                                *00033100
033200* DM9400   05/15/07  LR    ADD LOGIC FOR TABULAR #GMFH           *00033200
033300*                                                                *00033300
033310* DM9441   09/21/09  DNK   EXPANDED 7805 TO 31370 FOR THE        *00033310
033311*                          TABULAR FILE EXPANSION.               *00033311
033320*                                                                *00033312
AT0718* P00893   07/18/11  ART   OCTOBER RELEASE - BLUE DISTINCTION    *00033313
      *                                                                *00033314
      *                                                                *00033315
      * P00018542 01/16/14 KIKI  ADDED PROVIDER OF EXCELLENCE PLUS     *00033316
      *                          TO BLUE DISTINCTION TABULAR - 'BD'    *00033317
      *                                                                *00033318
      *           08/03/15 KIKI  ADDED MATERNITY CARE                  *00033319
      *                          TO BLUE DISTINCTION TABULAR - 'BD'    *00033320
      *                                                                *00033321
      *           09/02/15 KIKI  ADDED TOTAL CARE - #GCCP / TC  EDITS  *00033322
      *                                                                *00033324
      *           06/14/16 KIKI  CORR MODULE 20090-TOTAL-CARE-EDITS    *00033325
      *                                                                *00033326
      *           04/27/17 TROY  ADDED CENTER OF EXCELLENCE -          *00033327
      *                          #GCCP / CE  EDITS - ERS PROJECT       *00033328
      *                                                                *00033329
      *           07/18/18 SRI   ADDED PREAUTHORIZATION SERVICES       *00033330
      *                          #GCCP / PC  EDITS - HAS PROJECT       *00033331
      *                          ADDED CALL ENGAGEMENT SERVICES        *00033332
      *                          #GCCP / EC  EDITS - HAS PROJECT       *00033333
      *                          ADDED GSS2-TC-TOTAL-CARE-PLUS-IND  TO *00033334
      *                          #GCCP / TC  EDITS - HAS PROJECT       *00033335
      *           06/25/19 SRI   ADDED CUSTOM PREAUTH MED NEC          *00033336
      *                          #GCCP / CU  EDITS - PSMNR PROJECT     *00033337
      *           06/25/19 SRI   ADDED PREAUTHORIZATION 6 NEW FILEDS   *00033338
      *                          #GCCP / PC  EDITS - CONCEPT12 PROJECT *00033339
      *           08/27/21 GSD   ADDED CODE FOR NEW CANCERS FACILITY   *00033340
      *                          #GCCP / BD SEARCH \
ED0624*BBDA-58217 06/04/24 ED    RECOMPILE FOR PEAQ COPYBOOK           *00033342
ED0624*                          EXPANSION:                            *00033343
ED0624*                                COPYBKS - GCTABM*, GCTACL*,     *00033344
ED0624*                                GCTACP*,  GCTADL*, GCTADL*      *00033345
033200*BBDA-66049 04/10/206 TM   ADD LOGIC FOR TABULAR #GHPA           *00033346
033200*                                                                *00033347
      ******************************************************************00033350
                                                                        00033400
033500 ENVIRONMENT DIVISION.                                            00033500
033600 CONFIGURATION SECTION.                                           00033600
033700 SOURCE-COMPUTER. IBM-370.                                        00033700
033800 OBJECT-COMPUTER. IBM-370.                                        00033800
033900 INPUT-OUTPUT SECTION.                                            00033900
034000 FILE-CONTROL.                                                    00034000
034100 DATA DIVISION.                                                   00034100
034200 FILE SECTION.                                                    00034200
034300                                                                  00034300
034400 WORKING-STORAGE SECTION.                                         00034400
034500                                                                  00034500
034600 01  FILLER            PIC X(24) VALUE                            00034600
034700     'GC024020 WORKING STORAGE'.                                  00034700
034800                                                                  00034800
034900 01  MISC-WORK.                                                   00034900
035000     05  ABEND-CODE           PIC S9999 COMP VALUE ZERO.          00035000
035100     05  WS-PCT-LVL           PIC S9(3) COMP-3 VALUE ZEROS.       00035100
035200     05  WS-DF-ERROR-COUNT    PIC S9(3) COMP-3 VALUE ZEROS.       00035200
035300     05  WS-GC024015          PIC X(8)         VALUE 'GC024015'.  00035300
035400     05  HOLD-GROUP-KEY.                                          00035400
035500         10  H-PLAN-CODE      PIC X(3)  VALUE SPACES.             00035500
035600         10  H-GROUP-NUM.                                         00035600
035700             15 H-GRP-NO-1-3  PIC X(3)  VALUE SPACES.             00035700
035800             15 H-GRP-NO      PIC X(6)  VALUE SPACES.             00035800
035900         10  H-SECTION-NUM.                                       00035900
036000             15 H-SECTN-NO-1  PIC X(1)  VALUE SPACES.             00036000
036100             15 H-SECTN-NO    PIC X(4)  VALUE SPACES.             00036100
036200         10  H-PKG-CODE       PIC X(3)  VALUE SPACES.             00036200
036300         10  H-FAM-REL-LVL    PIC X(2)  VALUE SPACES.             00036300
036400         10  H-EFFECTIVE-DT.                                      00036400
036500             15 H-EFFDT-CC    PIC X     VALUE ZEROS.              00036500
036600             15 H-EFF-DATE    PIC S9(5) COMP-3 VALUE +0.          00036600
036700         10  H-EFFDT-CEN REDEFINES                                00036700
036800              H-EFFECTIVE-DT  PIC S9(7) COMP-3.                   00036800
036900     05  HOLD-PSEUDO-GRP-NBR  PIC X(9)  VALUE SPACES.             00036900
037000     05  HOLD-PSEUDO-SECTION-NBR PIC X(5) VALUE SPACES.           00037000
037100     05  HOLD-INT-REL-CODE.                                       00037100
037200         10  H-INT-REL-CODE-POS1  PIC X(01).                      00037200
037300             88  EXCLUDE-PSEUDO-GRP-EDIT    VALUE '9'.            00037300
037400         10  FILLER               PIC X(30).                      00037400
037500     05  WS-JUL-DATE          PIC X(5).                           00037500
037600         88  JUL-CONV-ERR               VALUE '00000'.            00037600
037700     05  WS-CURRENT-ERROR     PIC XXX   VALUE SPACES.             00037700
037800     05  WS-ERROR-COUNT       PIC S999  COMP-3 VALUE +0.          00037800
037900     05  WS-ERROR-SW          PIC X     VALUE 'N'.                00037900
038000         88  ERROR-FOUND                VALUE 'Y'.                00038000
038100     05 WS-ABM-SLOT           PIC S9(7) COMP-3 VALUE +0.          00038100
038200     05 WS-ABM-SW             PIC X     VALUE 'N'.                00038200
038300         88  ABM-REC-FOUND              VALUE 'Y'.                00038300
038400         88  ABM-REC-EMPTY              VALUE 'E'.                00038400
038500     05 WS-ACL-SLOT           PIC S9(7) COMP-3 VALUE +0.          00038500
038600     05 WS-ACL-SW             PIC X     VALUE 'N'.                00038600
038700         88  ACL-REC-FOUND              VALUE 'Y'.                00038700
038800         88  ACL-REC-EMPTY              VALUE 'E'.                00038800
038900     05 WS-ACP-SLOT           PIC S9(7) COMP-3 VALUE +0.          00038900
039000     05 WS-ACP-SW             PIC X     VALUE 'N'.                00039000
039100         88  ACP-REC-FOUND              VALUE 'Y'.                00039100
039200         88  ACP-REC-EMPTY              VALUE 'E'.                00039200
039300     05 WS-ADL-SLOT           PIC S9(7) COMP-3 VALUE +0.          00039300
039400     05 WS-ADL-SW             PIC X     VALUE 'N'.                00039400
039500         88  ADL-REC-FOUND              VALUE 'Y'.                00039500
039600         88  ADL-REC-EMPTY              VALUE 'E'.                00039600
039700     05 WS-AOL-SLOT           PIC S9(7) COMP-3 VALUE +0.          00039700
039800     05 WS-AOL-SW             PIC X     VALUE 'N'.                00039800
039900         88  AOL-REC-FOUND              VALUE 'Y'.                00039900
040000         88  AOL-REC-EMPTY              VALUE 'E'.                00040000
040100     05 WS-GCBL-SLOT          PIC S9(7) COMP-3 VALUE +0.          00040100
040200     05 WS-GCBL-SW            PIC X     VALUE 'N'.                00040200
040300         88  GCBL-REC-FOUND             VALUE 'Y'.                00040300
040400         88  GCBL-REC-EMPTY             VALUE 'E'.                00040400
040500     05 WS-GCCP-SLOT          PIC S9(7) COMP-3 VALUE +0.          00040500
040600     05 WS-GCCP-SW            PIC X     VALUE 'N'.                00040600
040700         88  GCCP-REC-FOUND             VALUE 'Y'.                00040700
040800         88  GCCP-REC-EMPTY             VALUE 'E'.                00040800
040900     05 WS-GCPO-SLOT          PIC S9(7) COMP-3 VALUE +0.          00040900
041000     05 WS-GCPO-SW            PIC X     VALUE 'N'.                00041000
041100         88  GCPO-REC-FOUND             VALUE 'Y'.                00041100
041200         88  GCPO-REC-EMPTY             VALUE 'E'.                00041200
041300     05 WS-GPPO-SLOT          PIC S9(7) COMP-3 VALUE +0.          00041300
041400     05 WS-GPPO-SW            PIC X     VALUE 'N'.                00041400
041500         88  GPPO-REC-FOUND             VALUE 'Y'.                00041500
041600         88  GPPO-REC-EMPTY             VALUE 'E'.                00041600
041700     05 WS-GRPO-SLOT          PIC S9(7) COMP-3 VALUE +0.          00041700
041800     05 WS-GRPO-SW            PIC X     VALUE 'N'.                00041800
041900         88  GRPO-REC-FOUND             VALUE 'Y'.                00041900
042000         88  GRPO-REC-EMPTY             VALUE 'E'.                00042000
042100                                                                  00042100
042200     05 WS-GBAE-SLOT          PIC S9(7) COMP-3 VALUE +0.          00042200
042300     05 WS-GBAE-SW            PIC X     VALUE 'N'.                00042300
042400         88  GBAE-REC-FOUND             VALUE 'Y'.                00042400
042500         88  GBAE-REC-EMPTY             VALUE 'E'.                00042500
042600                                                                  00042600
042700     05 WS-GHOR-SLOT          PIC S9(7) COMP-3 VALUE +0.          00042700
042800     05 WS-GHOR-SW            PIC X     VALUE 'N'.                00042800
042900         88  GHOR-REC-FOUND             VALUE 'Y'.                00042900
043000         88  GHOR-REC-EMPTY             VALUE 'E'.                00043000
043100                                                                  00043100
043200     05 WS-GMCD-SLOT          PIC S9(7) COMP-3 VALUE +0.          00043200
043300     05 WS-GMCD-SW            PIC X     VALUE 'N'.                00043300
043400         88  GMCD-REC-FOUND             VALUE 'Y'.                00043400
043500         88  GMCD-REC-EMPTY             VALUE 'E'.                00043500
043600                                                                  00043600
043700     05 WS-GMCG-SLOT          PIC S9(7) COMP-3 VALUE +0.          00043700
043800     05 WS-GMCG-SW            PIC X     VALUE 'N'.                00043800
043900         88  GMCG-REC-FOUND             VALUE 'Y'.                00043900
044000         88  GMCG-REC-EMPTY             VALUE 'E'.                00044000
044100                                                                  00044100
044200     05 WS-GMCR-SLOT          PIC S9(7) COMP-3 VALUE +0.          00044200
044300     05 WS-GMCR-SW            PIC X     VALUE 'N'.                00044300
044400         88  GMCR-REC-FOUND             VALUE 'Y'.                00044400
044500         88  GMCR-REC-EMPTY             VALUE 'E'.                00044500
044600                                                                  00044600
044700     05 WS-GMCS-SLOT          PIC S9(7) COMP-3 VALUE +0.          00044700
044800     05 WS-GMCS-SW            PIC X     VALUE 'N'.                00044800
044900         88  GMCS-REC-FOUND             VALUE 'Y'.                00044900
045000         88  GMCS-REC-EMPTY             VALUE 'E'.                00045000
045100                                                                  00045100
045200     05 WS-GHOB-SLOT          PIC S9(7) COMP-3 VALUE +0.          00045200
045300     05 WS-GHOB-SW            PIC X     VALUE 'N'.                00045300
045400         88  GHOB-REC-FOUND             VALUE 'Y'.                00045400
045500         88  GHOB-REC-EMPTY             VALUE 'E'.                00045500
045600     05 WS-GMDN-SLOT          PIC S9(7) COMP-3 VALUE +0.          00045600
045700     05 WS-GMDN-SW            PIC X     VALUE 'N'.                00045700
045800         88  GMDN-REC-FOUND             VALUE 'Y'.                00045800
045900         88  GMDN-REC-EMPTY             VALUE 'E'.                00045900
046000     05 WS-GMPR-SLOT          PIC S9(7) COMP-3 VALUE +0.          00046000
046100     05 WS-GMPR-SW            PIC X     VALUE 'N'.                00046100
046200         88  GMPR-REC-FOUND             VALUE 'Y'.                00046200
046300         88  GMPR-REC-EMPTY             VALUE 'E'.                00046300
046400     05 WS-GMPB-SLOT          PIC S9(7) COMP-3 VALUE +0.          00046400
046500     05 WS-GMPB-SW            PIC X     VALUE 'N'.                00046500
046600         88  GMPB-REC-FOUND             VALUE 'Y'.                00046600
046700         88  GMPB-REC-EMPTY             VALUE 'E'.                00046700
046800     05 WS-GPAN-SLOT          PIC S9(7) COMP-3 VALUE +0.          00046800
046900     05 WS-GPAN-SW            PIC X     VALUE 'N'.                00046900
047000         88  GPAN-REC-FOUND             VALUE 'Y'.                00047000
047100         88  GPAN-REC-EMPTY             VALUE 'E'.                00047100
047200     05 WS-GPAR-SLOT          PIC S9(7) COMP-3 VALUE +0.          00047200
047300     05 WS-GPAR-SW            PIC X     VALUE 'N'.                00047300
047400         88  GPAR-REC-FOUND             VALUE 'Y'.                00047400
047500         88  GPAR-REC-EMPTY             VALUE 'E'.                00047500
047600     05 WS-GPAD-SLOT          PIC S9(7) COMP-3 VALUE +0.          00047600
047700     05 WS-GPAD-SW            PIC X     VALUE 'N'.                00047700
047800         88  GPAD-REC-FOUND             VALUE 'Y'.                00047800
047900         88  GPAD-REC-EMPTY             VALUE 'E'.                00047900
048000     05 WS-GPAB-SLOT          PIC S9(7) COMP-3 VALUE +0.          00048000
048100     05 WS-GPAB-SW            PIC X     VALUE 'N'.                00048100
048200         88  GPAB-REC-FOUND             VALUE 'Y'.                00048200
048300         88  GPAB-REC-EMPTY             VALUE 'E'.                00048300
048400     05 WS-GMOR-SLOT          PIC S9(7) COMP-3 VALUE +0.          00048400
048500     05 WS-GMOR-SW            PIC X     VALUE 'N'.                00048500
048600         88  GMOR-REC-FOUND             VALUE 'Y'.                00048600
048700         88  GMOR-REC-EMPTY             VALUE 'E'.                00048700
048800     05 WS-GMOB-SLOT          PIC S9(7) COMP-3 VALUE +0.          00048800
048900     05 WS-GMOB-SW            PIC X     VALUE 'N'.                00048900
049000         88  GMOB-REC-FOUND             VALUE 'Y'.                00049000
049100         88  GMOB-REC-EMPTY             VALUE 'E'.                00049100
049200                                                                  00049200
049300     05 WS-GVLF-SLOT          PIC S9(7) COMP-3 VALUE +0.          00049300
049400     05 WS-GVLF-SW            PIC X     VALUE 'N'.                00049400
049500         88  GVLF-POINTER-FOUND         VALUE 'Y'.                00049500
049600         88  GVLF-POINTER-NOT-FOUND     VALUE 'N'.                00049600
049700                                                                  00049700
049800     05 WS-GVLG-SLOT          PIC S9(7) COMP-3 VALUE +0.          00049800
049900     05 WS-GVLG-SW            PIC X     VALUE 'N'.                00049900
050000         88  GVLG-POINTER-FOUND         VALUE 'Y'.                00050000
050100         88  GVLG-POINTER-NOT-FOUND     VALUE 'N'.                00050100
050200                                                                  00050200
050300     05 WS-GVLH-SLOT          PIC S9(7) COMP-3 VALUE +0.          00050300
050400     05 WS-GVLH-SW            PIC X     VALUE 'N'.                00050400
050500         88  GVLH-POINTER-FOUND         VALUE 'Y'.                00050500
050600         88  GVLH-POINTER-NOT-FOUND     VALUE 'N'.                00050600
050700                                                                  00050700
050800     05 WS-GVLP-SLOT          PIC S9(7) COMP-3 VALUE +0.          00050800
050900     05 WS-GVLP-SW            PIC X     VALUE 'N'.                00050900
051000         88  GVLP-POINTER-FOUND         VALUE 'Y'.                00051000
051100         88  GVLP-POINTER-NOT-FOUND     VALUE 'N'.                00051100
051200                                                                  00051200
051300     05 WS-GVLQ-SLOT          PIC S9(7) COMP-3 VALUE +0.          00051300
051400     05 WS-GVLQ-SW            PIC X     VALUE 'N'.                00051400
051500         88  GVLQ-POINTER-FOUND         VALUE 'Y'.                00051500
051600         88  GVLQ-POINTER-NOT-FOUND     VALUE 'N'.                00051600
051700                                                                  00051700
051800     05 WS-GVLR-SLOT          PIC S9(7) COMP-3 VALUE +0.          00051800
051900     05 WS-GVLR-SW            PIC X     VALUE 'N'.                00051900
052000         88  GVLR-POINTER-FOUND         VALUE 'Y'.                00052000
052100         88  GVLR-POINTER-NOT-FOUND     VALUE 'N'.                00052100
052200                                                                  00052200
052300     05 WS-GFHC-SLOT          PIC S9(7) COMP-3 VALUE +0.          00052300
052400     05 WS-GFHC-SW            PIC X     VALUE 'N'.                00052400
052500         88  GFHC-REC-FOUND             VALUE 'Y'.                00052500
052600         88  GFHC-REC-EMPTY             VALUE 'E'.                00052600
052700                                                                  00052700
052800     05 WS-GFSA-SLOT          PIC S9(7) COMP-3 VALUE +0.          00052800
052900     05 WS-GFSA-SW            PIC X     VALUE 'N'.                00052900
053000         88  GFSA-REC-FOUND             VALUE 'Y'.                00053000
053100         88  GFSA-REC-EMPTY             VALUE 'E'.                00053100
053200                                                                  00053200
053300     05 WS-GHCA-SLOT          PIC S9(7) COMP-3 VALUE +0.          00053300
053400     05 WS-GHCA-SW            PIC X     VALUE 'N'.                00053400
053500         88  GHCA-REC-FOUND             VALUE 'Y'.                00053500
053600         88  GHCA-REC-EMPTY             VALUE 'E'.                00053600
053700                                                                  00053700
053800     05 WS-GHSA-SLOT          PIC S9(7) COMP-3 VALUE +0.          00053800
053900     05 WS-GHSA-SW            PIC X     VALUE 'N'.                00053900
054000         88  GHSA-REC-FOUND             VALUE 'Y'.                00054000
054100         88  GHSA-REC-EMPTY             VALUE 'E'.                00054100
054200                                                                  00054200
054300     05 WS-GLPF-SLOT          PIC S9(7) COMP-3 VALUE +0.          00054300
054400     05 WS-GLPF-SW            PIC X     VALUE 'N'.                00054400
054500         88  GLPF-REC-FOUND             VALUE 'Y'.                00054500
054600         88  GLPF-REC-EMPTY             VALUE 'E'.                00054600
054700                                                                  00054700
054800     05 WS-GLPH-SLOT          PIC S9(7) COMP-3 VALUE +0.          00054800
054900     05 WS-GLPH-SW            PIC X     VALUE 'N'.                00054900
055000         88  GLPH-REC-FOUND             VALUE 'Y'.                00055000
055100         88  GLPH-REC-EMPTY             VALUE 'E'.                00055100
055200                                                                  00055200
055300     05 WS-GWHC-SLOT          PIC S9(7) COMP-3 VALUE +0.          00055300
055400     05 WS-GWHC-SW            PIC X     VALUE 'N'.                00055400
055500         88  GWHC-REC-FOUND             VALUE 'Y'.                00055500
055600         88  GWHC-REC-EMPTY             VALUE 'E'.                00055600
055700                                                                  00055700
055800     05 WS-GMFH-SLOT          PIC S9(7) COMP-3 VALUE +0.          00055800
055900     05 WS-GMFH-SW            PIC X     VALUE 'N'.                00055900
056000         88  GMFH-REC-FOUND             VALUE 'Y'.                00056000
056100         88  GMFH-REC-EMPTY             VALUE 'E'.                00056100
056200                                                                  00056200
TM0526     05 WS-GHPA-SLOT          PIC S9(7) COMP-3 VALUE +0.          00056210
TM0526     05 WS-GHPA-SW            PIC X     VALUE 'N'.                00056220
TM0526         88  GHPA-REC-FOUND             VALUE 'Y'.                00056230
TM0526         88  GHPA-REC-EMPTY             VALUE 'E'.                00056240
TM0526                                                                  00056250
056300     05 WS-TAB-KEY.                                               00056300
056400         10  WS-TAB-ID        PIC X(6)  VALUE SPACES.             00056400
056500         10  WS-TAB-SLOT      PIC S9(7) COMP-3 VALUE +0.          00056500
056600******************************************************************00056600
056700** THESE SWITCHES WILL REMAIN SET TO 'N' IF THE #GCCP TABULAR   **00056700
056800** IS EMPTY OR DOES NOT EXIST.                                  **00056800
056900******************************************************************00056900
057000     05                       PIC X(13) VALUE 'GCCP SWITCHES'.    00057000
057100     05 WS-AT-FOUND-SW        PIC X     VALUE 'N'.                00057100
057200         88  AT-ENTRY-FOUND             VALUE 'Y'.                00057200
057300     05 WS-BA-FOUND-SW        PIC X     VALUE 'N'.                00057300
057400         88  BA-ENTRY-FOUND             VALUE 'Y'.                00057400
           05 WS-BD-FOUND-SW        PIC X     VALUE 'N'.                00057410
AT0718         88  BD-ENTRY-FOUND             VALUE 'Y'.                00057420
057500     05 WS-CB-FOUND-SW        PIC X     VALUE 'N'.                00057500
057600         88  CB-ENTRY-FOUND             VALUE 'Y'.                00057600
      *--  TROY - ERS PROJECT                                           00057601
057500     05 WS-CE-FOUND-SW        PIC X     VALUE 'N'.                00057610
057600         88  CE-ENTRY-FOUND             VALUE 'Y'.                00057620
      *--  TROY - ERS PROJECT                                           00057630
057700     05 WS-CP-FOUND-SW        PIC X     VALUE 'N'.                00057700
057800         88  CP-ENTRY-FOUND             VALUE 'Y'.                00057800
      *--  SRI  - PSMNR PROJECT                                         00057810
057500     05 WS-CU-FOUND-SW        PIC X     VALUE 'N'.                00057820
057600         88  CU-ENTRY-FOUND             VALUE 'Y'.                00057830
      *--  SRI  - PSMNR PROJECT                                         00057840
      *--  SRI  - HAS PROJECT                                           00057850
057500     05 WS-EC-FOUND-SW        PIC X     VALUE 'N'.                00057860
057600         88  EC-ENTRY-FOUND             VALUE 'Y'.                00057870
      *--  SRI  - HAS PROJECT                                           00057880
057900     05 WS-FS-FOUND-SW        PIC X     VALUE 'N'.                00057900
058000         88  FS-ENTRY-FOUND             VALUE 'Y'.                00058000
058100     05 WS-HO-FOUND-SW        PIC X     VALUE 'N'.                00058100
058200         88  HO-ENTRY-FOUND             VALUE 'Y'.                00058200
058300     05 WS-IO-FOUND-SW        PIC X     VALUE 'N'.                00058300
058400         88  IO-ENTRY-FOUND             VALUE 'Y'.                00058400
058500     05 WS-MA-FOUND-SW        PIC X     VALUE 'N'.                00058500
058600         88  MA-ENTRY-FOUND             VALUE 'Y'.                00058600
058700     05 WS-MD-FOUND-SW        PIC X     VALUE 'N'.                00058700
058800         88  MD-ENTRY-FOUND             VALUE 'Y'.                00058800
058900     05 WS-MN-FOUND-SW        PIC X     VALUE 'N'.                00058900
059000         88  MN-ENTRY-FOUND             VALUE 'Y'.                00059000
059100     05 WS-MO-FOUND-SW        PIC X     VALUE 'N'.                00059100
059200         88  MO-ENTRY-FOUND             VALUE 'Y'.                00059200
059300     05 WS-MS-FOUND-SW        PIC X     VALUE 'N'.                00059300
059400         88  MS-ENTRY-FOUND             VALUE 'Y'.                00059400
059500     05 WS-PA-FOUND-SW        PIC X     VALUE 'N'.                00059500
059600         88  PA-ENTRY-FOUND             VALUE 'Y'.                00059600
      *--  SRI  - HAS PROJECT                                           00059610
057500     05 WS-PC-FOUND-SW        PIC X     VALUE 'N'.                00059620
057600         88  PC-ENTRY-FOUND             VALUE 'Y'.                00059630
      *--  SRI  - HAS PROJECT                                           00059640
059700     05 WS-PP-FOUND-SW        PIC X     VALUE 'N'.                00059700
059800         88  PP-ENTRY-FOUND             VALUE 'Y'.                00059800
059900     05 WS-PR-FOUND-SW        PIC X     VALUE 'N'.                00059900
060000         88  PR-ENTRY-FOUND             VALUE 'Y'.                00060000
060100     05 WS-PS-FOUND-SW        PIC X     VALUE 'N'.                00060100
060200         88  PS-ENTRY-FOUND             VALUE 'Y'.                00060200
060300     05 WS-PT-FOUND-SW        PIC X     VALUE 'N'.                00060300
060400         88  PT-ENTRY-FOUND             VALUE 'Y'.                00060400
060500     05 WS-P1-FOUND-SW        PIC X     VALUE 'N'.                00060500
060600         88  P1-ENTRY-FOUND             VALUE 'Y'.                00060600
060700     05 WS-HM-FOUND-SW        PIC X     VALUE 'N'.                00060700
060800         88  HM-ENTRY-FOUND             VALUE 'Y'.                00060800
060900     05 WS-RP-FOUND-SW        PIC X     VALUE 'N'.                00060900
061000         88  RP-ENTRY-FOUND             VALUE 'Y'.                00061000
061100     05 WS-RS-FOUND-SW        PIC X     VALUE 'N'.                00061100
061200         88  RS-ENTRY-FOUND             VALUE 'Y'.                00061200
061300     05 WS-SA-FOUND-SW        PIC X     VALUE 'N'.                00061300
061400         88  SA-ENTRY-FOUND             VALUE 'Y'.                00061400
061500     05 WS-S1-FOUND-SW        PIC X     VALUE 'N'.                00061500
061600         88  S1-ENTRY-FOUND             VALUE 'Y'.                00061600
061700     05 WS-CAPI-FOUND-SW      PIC X     VALUE 'N'.                00061700
               88  CAPI-FOUND                 VALUE 'Y'.                00061800
      *--  KIKI                                                         00061801
           05 WS-TC-FOUND-SW        PIC X     VALUE 'N'.                00061810
               88  TC-ENTRY-FOUND             VALUE 'Y'.                00061820
                                                                        00061830
061900     05 WS-AT-GSS2-INDEX      USAGE IS INDEX.                     00061900
062000     05 WS-BA-GSS2-INDEX      USAGE IS INDEX.                     00062000
AT0718     05 WS-BD-GSS2-INDEX      USAGE IS INDEX.                     00062010
062100     05 WS-CB-GSS2-INDEX      USAGE IS INDEX.                     00062100
      *--  TROY - ERS PROJECT                                           00062101
062100     05 WS-CE-GSS2-INDEX      USAGE IS INDEX.                     00062110
      *--  TROY - ERS PROJECT                                           00062120
062200     05 WS-CP-GSS2-INDEX      USAGE IS INDEX.                     00062200
      *--  SRI  - PSMNR PROJECT                                         00062210
062100     05 WS-CU-GSS2-INDEX      USAGE IS INDEX.                     00062220
      *--  SRI  - PSMNR PROJECT                                         00062230
      *--  SRI  - HAS PROJECT                                           00062240
062100     05 WS-EC-GSS2-INDEX      USAGE IS INDEX.                     00062250
      *--  SRI  - HAS PROJECT                                           00062260
062300     05 WS-FS-GSS2-INDEX      USAGE IS INDEX.                     00062300
062400     05 WS-HO-GSS2-INDEX      USAGE IS INDEX.                     00062400
062500     05 WS-IO-GSS2-INDEX      USAGE IS INDEX.                     00062500
062600     05 WS-MA-GSS2-INDEX      USAGE IS INDEX.                     00062600
062700     05 WS-MD-GSS2-INDEX      USAGE IS INDEX.                     00062700
062800     05 WS-MN-GSS2-INDEX      USAGE IS INDEX.                     00062800
062900     05 WS-MO-GSS2-INDEX      USAGE IS INDEX.                     00062900
063000     05 WS-MS-GSS2-INDEX      USAGE IS INDEX.                     00063000
063100     05 WS-PA-GSS2-INDEX      USAGE IS INDEX.                     00063100
      *--  SRI  - HAS PROJECT                                           00063110
062100     05 WS-PC-GSS2-INDEX      USAGE IS INDEX.                     00063120
      *--  SRI  - HAS PROJECT                                           00063130
063200     05 WS-PP-GSS2-INDEX      USAGE IS INDEX.                     00063200
063300     05 WS-PR-GSS2-INDEX      USAGE IS INDEX.                     00063300
063400     05 WS-RP-GSS2-INDEX      USAGE IS INDEX.                     00063400
063500     05 WS-PS-GSS2-INDEX      USAGE IS INDEX.                     00063500
063600     05 WS-PT-GSS2-INDEX      USAGE IS INDEX.                     00063600
063700     05 WS-P1-GSS2-INDEX      USAGE IS INDEX.                     00063700
063800     05 WS-RS-GSS2-INDEX      USAGE IS INDEX.                     00063800
063900     05 WS-SA-GSS2-INDEX      USAGE IS INDEX.                     00063900
064000     05 WS-S1-GSS2-INDEX      USAGE IS INDEX.                     00064000
064100     05 WS-HM-GSS2-INDEX      USAGE IS INDEX.                     00064100
      *--  KIKI                                                         00064110
           05 WS-TC-GSS2-INDEX      USAGE IS INDEX.                     00064120
064200                                                                  00064200
064300 01  WS-DEDUCT-DEF.                                               00064300
064400     05  WS-DED-DEF                 PIC X(02)  VALUE SPACE.       00064400
064500         88 SELECTED-VALUE          VALUE '10' THRU '13'.         00064500
064600                                                                  00064600
064700 01  WS-COINS-LIMIT.                                              00064700
064800     05  WS-COINS-LIM               PIC X(02)  VALUE SPACE.       00064800
064900         88 SELECTED-COINS          VALUE 'C1' THRU 'C5'.         00064900
065000                                                                  00065000
065100 01  WS-OUT-OF-POCKET.                                            00065100
065200     05  WS-OPX                     PIC X(02)  VALUE SPACE.       00065200
065300         88 SELECTED-OPX            VALUE '10' THRU '20'.         00065300
065400                                                                  00065400
065500/                                                                 00065500
065600 01                           PIC X(12)  VALUE 'WS-SAVE-AREA'.    00065600
065700 01  WS-SAVE-AREA.                                                00065700
065800     05  WS-GCG-ENTRY-COUNT      COMP-3 PIC S9(03).               00065800
065900     05  WS-GCG-ENTRIES.                                          00065900
066000         10  WS-GCG-GRP-TAB-ID    OCCURS 1 TO 30 TIMES            00066000
066100               DEPENDING ON WS-GCG-ENTRY-COUNT                    00066100
066200               ASCENDING KEY IS WS-GCG-TAB-ID                     00066200
066300               INDEXED BY WS-GCG-INDEX.                           00066300
066400             15  WS-GCG-TAB-ID        PIC X(06).                  00066400
066500             15  WS-GCG-TAB-SLOT-NO   COMP-3 PIC S9(07).          00066500
066600/                                                                 00066600
066700 01  PARM-SET.                                                    00066700
066800     05  SET-RDW.                                                 00066800
066900         10  SET-REC-LENG    PIC 9(4)    VALUE ZEROS     COMP.    00066900
067000         10  SET-FEEDBACK    PIC 9(4)    VALUE ZEROS     COMP.    00067000
067100     05  SET-VALUE           PIC 9(8)                    COMP.    00067100
067200                                                                  00067200
067300 01  PARM-ONE.                                                    00067300
067400     05  RESERVED-FLDS-1     PIC 9(8)    VALUE ZEROS     COMP.    00067400
067500     05  RESERVED-X-1 REDEFINES RESERVED-FLDS-1.                  00067500
067600         10  REQUEST-TYPE-1  PIC X.                               00067600
067700         10  FILLER          PIC X(3).                            00067700
067800                                                                  00067800
067900 01                          PIC X(9)    VALUE 'PARM-ONEA'.       00067900
068000 01  PARM-ONEA.                                                   00068000
068100     02  ONEA-RDW.                                                00068100
068200         05  ONEA-REC-LENG   PIC 9(4)    VALUE ZEROS     COMP.    00068200
068300         05  ONEA-FEEDBACK   PIC 9(4)    VALUE ZEROS     COMP.    00068300
068400     02  ONEA-REC-AREA.                                           00068400
068500         COPY GCGROUPC.                                           00068500
068600/                                                                 00068600
068700 01  PARM-TWO.                                                    00068700
068800     05  RESERVED-FLDS-2     PIC 9(8)    VALUE ZEROS     COMP.    00068800
068900     05  RESERVED-X-2 REDEFINES RESERVED-FLDS-2.                  00068900
069000         10  REQUEST-TYPE-2  PIC X.                               00069000
069100         10  FILLER          PIC X(3).                            00069100
069200                                                                  00069200
069300 01                          PIC X(9)    VALUE 'PARM-TWOA'.       00069300
069400 01  PARM-TWOA.                                                   00069400
069500     02  TWOA-RDW.                                                00069500
069600         05  TWOA-REC-LENG   PIC 9(4)    VALUE ZEROS     COMP.    00069600
069700         05  TWOA-FEEDBACK   PIC 9(4)    VALUE ZEROS     COMP.    00069700
069800     02  TWOA-REC-AREA00     PIC X(31370) VALUE LOW-VALUES.       00069800
069900                                                                  00069900
070000/                                                                 00070000
070100 01                          PIC X(7)    VALUE 'ABM-REC'.         00070100
070200 01  ABM-REC.                                                     00070200
070300     COPY GCTABMC.                                                00070300
070400/                                                                 00070400
070500 01                          PIC X(7)    VALUE 'ACL-REC'.         00070500
070600 01  ACL-REC.                                                     00070600
070700     COPY GCTACLC.                                                00070700
070800/                                                                 00070800
070900 01                          PIC X(7)    VALUE 'ACP-REC'.         00070900
071000 01  ACP-REC.                                                     00071000
071100     COPY GCTACPC.                                                00071100
071200/                                                                 00071200
071300 01                          PIC X(7)    VALUE 'ADL-REC'.         00071300
071400 01  ADL-REC.                                                     00071400
071500     COPY GCTADLC.                                                00071500
071600/                                                                 00071600
071700 01                          PIC X(7)    VALUE 'AOL-REC'.         00071700
071800 01  AOL-REC.                                                     00071800
071900     COPY GCTAOLC.                                                00071900
072000/                                                                 00072000
072100 01                          PIC X(8)    VALUE 'GCBL-REC'.        00072100
072200 01  GCBL-REC.                                                    00072200
072300     COPY GCTGCBL2.                                               00072300
072400/                                                                 00072400
072500 01                          PIC X(8)    VALUE 'GCCP-REC'.        00072500
072600 01  GCCP-REC.                                                    00072600
072700     COPY GCTGCCP2.                                               00072700
072800/                                                                 00072800
072900 01                          PIC X(8)    VALUE 'GCPO-REC'.        00072900
073000 01  GCPO-REC.                                                    00073000
073100     COPY GCTGCPO2.                                               00073100
073200/                                                                 00073200
073300 01                          PIC X(8)    VALUE 'GPAN-REC'.        00073300
073400 01  GPAN-REC.                                                    00073400
073500     COPY GCTGPAN2.                                               00073500
073600/                                                                 00073600
073700 01                          PIC X(8)    VALUE 'GPPO-REC'.        00073700
073800 01  GPPO-REC.                                                    00073800
073900     COPY GCTGPPO2.                                               00073900
074000/                                                                 00074000
074100 01                          PIC X(8)    VALUE 'GRPO-REC'.        00074100
074200 01  GRPO-REC.                                                    00074200
074300     COPY GCTGRPO2.                                               00074300
074400/                                                                 00074400
074500 01                          PIC X(8)    VALUE 'GBAE-REC'.        00074500
074600 01  GBAE-REC.                                                    00074600
074700     COPY GCTGBAE2.                                               00074700
074800/                                                                 00074800
074900 01                          PIC X(8)    VALUE 'GHOR-REC'.        00074900
075000 01  GHOR-REC.                                                    00075000
075100     COPY GCTGHOR2.                                               00075100
075200/                                                                 00075200
075300 01                          PIC X(8)    VALUE 'GHOB-REC'.        00075300
075400 01  GHOB-REC.                                                    00075400
075500     COPY GCTGHOB2.                                               00075500
075600/                                                                 00075600
075700 01                          PIC X(8)    VALUE 'GMCD-REC'.        00075700
075800 01  GMCD-REC.                                                    00075800
075900     COPY GCTGMCD2.                                               00075900
076000/                                                                 00076000
076100 01                          PIC X(8)    VALUE 'GMCG-REC'.        00076100
076200 01  GMCG-REC.                                                    00076200
076300     COPY GCTGMCG2.                                               00076300
076400/                                                                 00076400
076500 01                          PIC X(8)    VALUE 'GMCR-REC'.        00076500
076600 01  GMCR-REC.                                                    00076600
076700     COPY GCTGMCR2.                                               00076700
076800/                                                                 00076800
076900 01                          PIC X(8)    VALUE 'GMCS-REC'.        00076900
077000 01  GMCS-REC.                                                    00077000
077100     COPY GCTGMCS2.                                               00077100
077200/                                                                 00077200
077300 01                          PIC X(8)    VALUE 'GMDN-REC'.        00077300
077400 01  GMDN-REC.                                                    00077400
077500     COPY GCTGMDN2.                                               00077500
077600/                                                                 00077600
077700 01                          PIC X(8)    VALUE 'GMPR-REC'.        00077700
077800 01  GMPR-REC.                                                    00077800
077900     COPY GCTGMPR2.                                               00077900
078000/                                                                 00078000
078100 01                          PIC X(8)    VALUE 'GMPB-REC'.        00078100
078200 01  GMPB-REC.                                                    00078200
078300     COPY GCTGMPB2.                                               00078300
078400/                                                                 00078400
078500 01                          PIC X(8)    VALUE 'GPAR-REC'.        00078500
078600 01  GPAR-REC.                                                    00078600
078700     COPY GCTGPAR2.                                               00078700
078800/                                                                 00078800
078900 01                          PIC X(8)    VALUE 'GPAD-REC'.        00078900
079000 01  GPAD-REC.                                                    00079000
079100     COPY GCTGPAD2.                                               00079100
079200/                                                                 00079200
079300 01                          PIC X(8)    VALUE 'GPAB-REC'.        00079300
079400 01  GPAB-REC.                                                    00079400
079500     COPY GCTGPAB2.                                               00079500
079600/                                                                 00079600
079700 01                          PIC X(8)    VALUE 'GMOR-REC'.        00079700
079800 01  GMOR-REC.                                                    00079800
079900     COPY GCTGMOR2.                                               00079900
080000/                                                                 00080000
080100 01                          PIC X(8)    VALUE 'GMOB-REC'.        00080100
080200 01  GMOB-REC.                                                    00080200
080300     COPY GCTGMOB2.                                               00080300
080400/                                                                 00080400
080500 01                          PIC X(8)    VALUE 'GFHC-REC'.        00080500
080600 01  GFHC-REC.                                                    00080600
080700     COPY GCTGFHC2.                                               00080700
080800/                                                                 00080800
080900 01                          PIC X(8)    VALUE 'GFSA-REC'.        00080900
081000 01  GFSA-REC.                                                    00081000
081100     COPY GCTGFSA2.                                               00081100
081200/                                                                 00081200
081300 01                          PIC X(8)    VALUE 'GHCA-REC'.        00081300
081400 01  GHCA-REC.                                                    00081400
081500     COPY GCTGHCA2.                                               00081500
081600/                                                                 00081600
081700 01                          PIC X(8)    VALUE 'GHSA-REC'.        00081700
081800 01  GHSA-REC.                                                    00081800
081900     COPY GCTGHSA2.                                               00081900
082000/                                                                 00082000
082100 01                          PIC X(8)    VALUE 'GLPF-REC'.        00082100
082200 01  GLPF-REC.                                                    00082200
082300     COPY GCTGLPF2.                                               00082300
082400/                                                                 00082400
082500 01                          PIC X(8)    VALUE 'GLPH-REC'.        00082500
082600 01  GLPH-REC.                                                    00082600
082700     COPY GCTGLPH2.                                               00082700
082800/                                                                 00082800
082900 01                          PIC X(8)    VALUE 'GWHC-REC'.        00082900
083000 01  GWHC-REC.                                                    00083000
083100     COPY GCTGWHC2.                                               00083100
083200/                                                                 00083200
083300 01                          PIC X(8)    VALUE 'GMFH-REC'.        00083300
083400 01  GMFH-REC.                                                    00083400
083500     COPY GCTGMFH2.                                               00083500
083600/                                                                 00083600
TM0526 01                          PIC X(8)    VALUE 'GHPA-REC'.        00083610
TM0526 01  GHPA-REC.                                                    00083620
TM0526     COPY GCTGHPA2.                                               00083630
TM0526/                                                                 00083640
083700*01                          PIC X(13)   VALUE 'GC024030 AREA'.   00083700
083800 01  GC024030-CALL-AREA.                                          00083800
083900     05  GC024030-IND          PIC X.                             00083900
084000         88  OPEN-FILE                 VALUE 'O'.                 00084000
084100         88  CLOSE-FILE                VALUE 'C'.                 00084100
084200         88  BAD-EDIT                  VALUE 'E'.                 00084200
084300         88  GOOD-EDIT                 VALUE 'G'.                 00084300
084400/                                                                 00084400
084500     COPY GCDFILEC.                                               00084500
084600/                                                                 00084600
084700 01                          PIC X(13)   VALUE 'GCCDRLEN AREA'.   00084700
084800 01  WS-GCPS-LENGTHS.                                             00084800
084900     COPY GCCDRLEN.                                               00084900
085000 01                 PIC X(20)   VALUE 'END OF WRKNG STORAGE'.     00085000
085100/                                                                 00085100
085200 LINKAGE SECTION.                                                 00085200
085300                                                                  00085300
085400 01  GC024020-IND.                                                00085400
085500     05  FILLER              PIC X.                               00085500
085600                                                                  00085600
085700 01  LINK-GROUP-KEY.                                              00085700
085800     05  LG-PLAN-CODE        PIC X(3).                            00085800
085900     05  LG-GROUP-NUM.                                            00085900
086000         10 LG-GRP-NO-1-3    PIC X(3).                            00086000
086100         10 LG-GRP-NO        PIC X(6).                            00086100
086200     05  LG-SECTION-NUM.                                          00086200
086300         10 LG-SECTN-NO-1    PIC X(1).                            00086300
086400         10 LG-SECTN-NO      PIC X(4).                            00086400
086500     05  LG-PKG-CODE         PIC X(3).                            00086500
086600     05  LG-FAM-REL-LVL      PIC XX.                              00086600
086700     05  LG-EFFECTIVE-DT.                                         00086700
086800         10 LG-EFFDT-CC      PIC X.                               00086800
086900         10 LG-EFF-DATE      PIC S9(5) COMP-3.                    00086900
087000     05  LG-EFFDT-CEN REDEFINES                                   00087000
087100           LG-EFFECTIVE-DT   PIC S9(7) COMP-3.                    00087100
087200*10/1/02 EXPAND OPERATOR ID TO 8 BYTES GTF.                       00087200
087300 01  LINK-OPERATOR-ID        PIC X(08).                           00087300
087400/                                                                 00087400
087500 PROCEDURE DIVISION USING GC024020-IND LINK-GROUP-KEY             00087500
087600                          LINK-OPERATOR-ID.                       00087600
087700                                                                  00087700
087800 0000-MAINLINE.                                                   00087800
087900     IF GC024020-IND = 'R'                                        00087900
088000         IF LG-GROUP-NUM > SPACES                                 00088000
088100             PERFORM 10000-EDIT-GS-RECORD THRU 10000-EXIT         00088100
088200             GOBACK                                               00088200
088300         ELSE                                                     00088300
088400             DISPLAY 'GC024020 DUMMY KEY BYPASSED'                00088400
088500             GOBACK.                                              00088500
088600                                                                  00088600
088700     IF GC024020-IND = 'C'                                        00088700
088800         PERFORM 99300-CLOSE-FILES THRU 99300-EXIT                00088800
088900         GOBACK.                                                  00088900
089000                                                                  00089000
089100     DISPLAY 'GC024020 BAD PARM ' GC024020-IND.                   00089100
089200     MOVE +0002 TO ABEND-CODE.                                    00089200
089300     GO TO 99999-ERROR-RTN.                                       00089300
089400                                                                  00089400
089500 00000-EXIT. EXIT.                                                00089500
089600                                                                  00089600
089700/                                                                 00089700
089800 10000-EDIT-GS-RECORD.                                            00089800
089900******************************************************************00089900
090000** RESET ALL SWITCHES AND WORK FIELDS BEFORE EDITING EACH       **00090000
090100** NEW GROUP SPECIFIC RECORD.                                   **00090100
090200******************************************************************00090200
090300     MOVE SPACES              TO WS-CURRENT-ERROR.                00090300
090400     MOVE ZERO                TO WS-ERROR-COUNT.                  00090400
090500     MOVE 'N'                 TO WS-ERROR-SW                      00090500
090600                                 WS-ABM-SW                        00090600
090700                                 WS-ACL-SW                        00090700
090800                                 WS-ACP-SW                        00090800
090900                                 WS-ADL-SW                        00090900
091000                                 WS-AOL-SW                        00091000
091100                                 WS-GCBL-SW                       00091100
091200                                 WS-GCCP-SW                       00091200
091300                                 WS-GCPO-SW                       00091300
091400                                 WS-GPPO-SW                       00091400
091500                                 WS-GRPO-SW                       00091500
091600                                 WS-GBAE-SW                       00091600
091700                                 WS-GFHC-SW                       00091700
091800                                 WS-GFSA-SW                       00091800
091900                                 WS-GHCA-SW                       00091900
092000                                 WS-GHSA-SW                       00092000
092100                                 WS-GLPF-SW                       00092100
092200                                 WS-GLPH-SW                       00092200
092300                                 WS-GWHC-SW                       00092300
092400                                 WS-GHOR-SW                       00092400
092500                                 WS-GHOB-SW                       00092500
092600                                 WS-GMCD-SW                       00092600
092700                                 WS-GMCG-SW                       00092700
092800                                 WS-GMCR-SW                       00092800
092900                                 WS-GMCS-SW                       00092900
093000                                 WS-GMDN-SW                       00093000
000100                                 WS-GMFH-SW                       00093100
TM0526                                 WS-GHPA-SW                       00093110
093200                                 WS-GMPR-SW                       00093200
093300                                 WS-GMPB-SW                       00093300
093400                                 WS-GPAR-SW                       00093400
093500                                 WS-GPAD-SW                       00093500
093600                                 WS-GPAN-SW                       00093600
093700                                 WS-GPAB-SW                       00093700
093800                                 WS-GMOR-SW                       00093800
093900                                 WS-GMOB-SW                       00093900
094000                                 WS-GVLF-SW                       00094000
094100                                 WS-GVLG-SW                       00094100
094200                                 WS-GVLH-SW                       00094200
094300                                 WS-GVLP-SW                       00094300
094400                                 WS-GVLQ-SW                       00094400
094500                                 WS-GVLR-SW.                      00094500
094600                                                                  00094600
094700     MOVE ZERO                TO WS-ABM-SLOT                      00094700
094800                                 WS-ACL-SLOT                      00094800
094900                                 WS-ACP-SLOT                      00094900
095000                                 WS-ADL-SLOT                      00095000
095100                                 WS-AOL-SLOT                      00095100
095200                                 WS-GCBL-SLOT                     00095200
095300                                 WS-GCCP-SLOT                     00095300
095400                                 WS-GCPO-SLOT                     00095400
095500                                 WS-GPPO-SLOT                     00095500
095600                                 WS-GRPO-SLOT                     00095600
095700                                 WS-GBAE-SLOT                     00095700
095800                                 WS-GFHC-SLOT                     00095800
095900                                 WS-GFSA-SLOT                     00095900
096000                                 WS-GHCA-SLOT                     00096000
096100                                 WS-GHSA-SLOT                     00096100
096200                                 WS-GLPF-SLOT                     00096200
096300                                 WS-GLPH-SLOT                     00096300
096400                                 WS-GWHC-SLOT                     00096400
096500                                 WS-GHOR-SLOT                     00096500
096600                                 WS-GHOB-SLOT                     00096600
096700                                 WS-GMCD-SLOT                     00096700
096800                                 WS-GMCG-SLOT                     00096800
096900                                 WS-GMCR-SLOT                     00096900
097000                                 WS-GMCS-SLOT                     00097000
097100                                 WS-GMDN-SLOT                     00097100
097200                                 WS-GMFH-SLOT                     00097200
TM0526                                 WS-GHPA-SLOT                     00097210
097300                                 WS-GMPR-SLOT                     00097300
097400                                 WS-GMPB-SLOT                     00097400
097500                                 WS-GPAN-SLOT                     00097500
097600                                 WS-GPAR-SLOT                     00097600
097700                                 WS-GPAD-SLOT                     00097700
097800                                 WS-GPAB-SLOT                     00097800
097900                                 WS-GMOR-SLOT                     00097900
098000                                 WS-GMOB-SLOT                     00098000
098100                                 WS-GVLF-SLOT                     00098100
098200                                 WS-GVLG-SLOT                     00098200
098300                                 WS-GVLH-SLOT                     00098300
098400                                 WS-GVLP-SLOT                     00098400
098500                                 WS-GVLQ-SLOT                     00098500
098600                                 WS-GVLR-SLOT.                    00098600
098700                                                                  00098700
098800     MOVE SPACES              TO WS-TAB-ID.                       00098800
098900     MOVE ZERO                TO WS-TAB-SLOT.                     00098900
099000     MOVE 'N'                 TO WS-AT-FOUND-SW                   00099000
099100                                 WS-BA-FOUND-SW                   00099100
AT0718                                 WS-BD-FOUND-SW                   00099110
099200                                 WS-CB-FOUND-SW                   00099200
      *--  TROY - ERS PROJECT                                           00099210
099200                                 WS-CE-FOUND-SW                   00099211
      *--  TROY - ERS PROJECT                                           00099220
099300                                 WS-CP-FOUND-SW                   00099300
      *--  SRI  - PSMNR PROJECT                                         00099301
099200                                 WS-CU-FOUND-SW                   00099302
      *--  SRI  - PSMNR PROJECT                                         00099303
      *--  SRI  - HAS PROJECT                                           00099310
099200                                 WS-EC-FOUND-SW                   00099320
      *--  SRI  - HAS PROJECT                                           00099330
099400                                 WS-FS-FOUND-SW                   00099400
099500                                 WS-HM-FOUND-SW                   00099500
099600                                 WS-HO-FOUND-SW                   00099600
099700                                 WS-IO-FOUND-SW                   00099700
099800                                 WS-MA-FOUND-SW                   00099800
099900                                 WS-MD-FOUND-SW                   00099900
100000                                 WS-MN-FOUND-SW                   00100000
100100                                 WS-MO-FOUND-SW                   00100100
100200                                 WS-MS-FOUND-SW                   00100200
100300                                 WS-PA-FOUND-SW                   00100300
      *--  SRI  - HAS PROJECT                                           00100310
099200                                 WS-PC-FOUND-SW                   00100320
      *--  SRI  - HAS PROJECT                                           00100330
100400                                 WS-PP-FOUND-SW                   00100400
100500                                 WS-RP-FOUND-SW                   00100500
100600                                 WS-PR-FOUND-SW                   00100600
100700                                 WS-PS-FOUND-SW                   00100700
100800                                 WS-PT-FOUND-SW                   00100800
100900                                 WS-P1-FOUND-SW                   00100900
101000                                 WS-RS-FOUND-SW                   00101000
101100                                 WS-SA-FOUND-SW                   00101100
      *--  KIKI                                                         00101110
101200                                 WS-S1-FOUND-SW                   00101200
                                       WS-TC-FOUND-SW.                  00101220
                                                                        00101230
101300******************************************************************00101300
101400** READ GROUP SPECIFIC FILE RECORD                              **00101400
101500******************************************************************00101500
101600     MOVE 'N' TO WS-ERROR-SW.                                     00101600
101700     PERFORM 91000-READ-GROUP THRU 91000-EXIT.                    00101700
101800                                                                  00101710
      *  MOO1                                                           00101800
101900******************************************************************00101900
102000** SET UP DISCREPANCY FILE OUTPUT RECORD.                       **00102000
102100** ERROR DATE AND ACTION FLAG ARE SUPPLIED BY GC024030.         **00102100
102200** OPERATOR ID AND TERMINAL ID WILL EVENTUALLY BE SUPPLIED      **00102200
102300** BY THIS PROGRAM ONCE THEY ARE ADDED TO THE GROUP SPECIFIC    **00102300
102400** RECORD AND ASSOCIATED TABULARS.                              **00102400
102500******************************************************************00102500
102600* COBOL II NEEDS THE \
102700* MOVING LOW-VALUES.                                              00102700
102800     MOVE +200             TO  DF-ERROR-COUNT.                    00102800
102900     MOVE LOW-VALUES       TO  GC024030-CALL-AREA.                00102900
103000     MOVE 'G'              TO  DF-RECORD-TYPE.                    00103000
103100     MOVE GCG-PLAN-CODE    TO  DF-REC-PLAN-CODE.                  00103100
103200     MOVE GCG-GROUP-NUM    TO  DF-REC-GROUP.                      00103200
103300     MOVE GCG-SECTION-NUM  TO  DF-REC-SECTION.                    00103300
103400     MOVE GCG-PKG-CODE     TO  DF-REC-PKG-CODE.                   00103400
103500     MOVE GCG-FAM-REL-LVL  TO  DF-REC-FAM-REL.                    00103500
103600     MOVE GCG-EFFDT-CEN    TO  DF-REC-EFFDT-CEN.                  00103600
103700*    MOVE +200             TO  DF-ERROR-COUNT.                    00103700
103800     MOVE +0               TO  DF-ERROR-COUNT.                    00103800
103900     MOVE LINK-OPERATOR-ID TO  DF-OPERATOR-ID.                    00103900
104000     SET DF-ERROR-INDEX TO 1.                                     00104000
104100                                                                  00104100
104200*--- D199 ADD EDITS FOR #GVLF/#GVLG/#GVLH AND #GVLP/#GVLQ/#GVLR   00104200
104300                                                                  00104300
104400     PERFORM 10090-SEARCH-GRP-SPEC-TAB THRU 10090-EXIT            00104400
104500             VARYING GCG-INDEX FROM +1 BY +1                      00104500
104600             UNTIL   GCG-INDEX > GCG-COUNT-TAB-PROVN-POINTERS.    00104600
104700                                                                  00104700
104800                                                                  00104800
104900***********************************************************       00104900
105000*                                                                 00105000
105100*   APPLY THE FIRST SET OF EDITS TO #GVLF/#GVLG/#GVLH ONLY        00105100
105200*                                                                 00105200
105300***********************************************************       00105300
105400 10000-100-FIRST-SET-OF-EDITS.                                    00105400
105500*--- ONLY #GLVG EXISTS                                            00105500
105600     IF GVLG-POINTER-FOUND AND                                    00105600
105700        GVLF-POINTER-NOT-FOUND AND GVLH-POINTER-NOT-FOUND         00105700
105800           MOVE 'G82' TO WS-CURRENT-ERROR                         00105800
105900           PERFORM 90000-POST-ERROR THRU 90000-EXIT               00105900
106000           GO TO 10000-200-SECOND-SET-OF-EDITS.                   00106000
106100                                                                  00106100
106200*--- ONLY #GLVH EXISTS                                            00106200
106300     IF GVLH-POINTER-FOUND AND                                    00106300
106400        GVLF-POINTER-NOT-FOUND AND GVLG-POINTER-NOT-FOUND         00106400
106500           MOVE 'G83' TO WS-CURRENT-ERROR                         00106500
106600           PERFORM 90000-POST-ERROR THRU 90000-EXIT               00106600
106700           GO TO 10000-200-SECOND-SET-OF-EDITS.                   00106700
106800                                                                  00106800
106900*--- #GVLF AND #GVLH EXIST, BUT NOT #GVLG                         00106900
107000     IF GVLF-POINTER-FOUND AND GVLH-POINTER-FOUND AND             00107000
107100        GVLG-POINTER-NOT-FOUND                                    00107100
107200           MOVE 'G84' TO WS-CURRENT-ERROR                         00107200
107300           PERFORM 90000-POST-ERROR THRU 90000-EXIT               00107300
107400           GO TO 10000-200-SECOND-SET-OF-EDITS.                   00107400
107500                                                                  00107500
107600*--- #GVLG AND #GVLH EXIST, BUT NOT #GVLF                         00107600
107700     IF GVLG-POINTER-FOUND AND GVLH-POINTER-FOUND AND             00107700
107800        GVLF-POINTER-NOT-FOUND                                    00107800
107900           MOVE 'G85' TO WS-CURRENT-ERROR                         00107900
108000           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00108000
108100                                                                  00108100
108200***********************************************************       00108200
108300*                                                                 00108300
108400*   APPLY THE SECOND SET OF EDITS TO #GVLF/#GVLG/#GVLH ONLY       00108400
108500*                                                                 00108500
108600***********************************************************       00108600
108700 10000-200-SECOND-SET-OF-EDITS.                                   00108700
108800*--- ONLY #GLVQ EXISTS                                            00108800
108900     IF GVLQ-POINTER-FOUND AND                                    00108900
109000        GVLP-POINTER-NOT-FOUND AND GVLR-POINTER-NOT-FOUND         00109000
109100           MOVE 'G86' TO WS-CURRENT-ERROR                         00109100
109200           PERFORM 90000-POST-ERROR THRU 90000-EXIT               00109200
109300           GO TO 10000-300-EDIT-TABS.                             00109300
109400                                                                  00109400
109500*--- ONLY #GLVR EXISTS                                            00109500
109600     IF GVLR-POINTER-FOUND AND                                    00109600
109700        GVLP-POINTER-NOT-FOUND AND GVLQ-POINTER-NOT-FOUND         00109700
109800           MOVE 'G87' TO WS-CURRENT-ERROR                         00109800
109900           PERFORM 90000-POST-ERROR THRU 90000-EXIT               00109900
110000           GO TO 10000-300-EDIT-TABS.                             00110000
110100                                                                  00110100
110200*--- #GVLP AND #GVLR EXIST, BUT NOT #GVLQ                         00110200
110300     IF GVLP-POINTER-FOUND AND GVLR-POINTER-FOUND AND             00110300
110400        GVLQ-POINTER-NOT-FOUND                                    00110400
110500           MOVE 'G88' TO WS-CURRENT-ERROR                         00110500
110600           PERFORM 90000-POST-ERROR THRU 90000-EXIT               00110600
110700           GO TO 10000-300-EDIT-TABS.                             00110700
110800                                                                  00110800
110900*--- #GVLQ AND #GVLR EXIST, BUT NOT #GVLP                         00110900
111000     IF GVLQ-POINTER-FOUND AND GVLR-POINTER-FOUND AND             00111000
111100        GVLP-POINTER-NOT-FOUND                                    00111100
111200           MOVE 'G89' TO WS-CURRENT-ERROR                         00111200
111300           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00111300
111400                                                                  00111400
111500 10000-300-EDIT-TABS.                                             00111500
111600******************************************************************00111600
111700** PICK UP SLOT NUMBERS FOR TABULARS USED IN THIS PROGRAM.      **00111700
111800** READ THE ONES WE NEED INTO WORKING STORAGE.                  **00111800
111900******************************************************************00111900
112000     PERFORM 11000-READ-TABS THRU 11000-EXIT                      00112000
112100         VARYING GCG-INDEX FROM 1 BY 1                            00112100
112200         UNTIL GCG-INDEX = GCG-COUNT-TAB-PROVN-POINTERS           00112200
112300         OR GCG-GRP-SPEC-TAB-ID (GCG-INDEX) = HIGH-VALUES.        00112300
112400                                                                  00112400
112500     PERFORM 20000-EDITS THRU 20000-EXIT.                         00112500
                                                                        00112600
           PERFORM 20010-OTHER-EDITS THRU 20010-EXIT.                   00112700
                                                                        00112701
      **   MOO2                                                         00112710
112800******************************************************************00112800
112900** IF ERRORS WERE FOUND, THEN EITHER UPDATE THE EXISTING        **00112900
113000** DISCREPANCY RECORD, IF ANY, OR WRITE A NEW ONE.  IF NO       **00113000
113100** ERRORS WERE FOUND, FLAG THE EXISTING DISCREPANCY RECORD,     **00113100
113200** IF ANY, FOR DELETION.                                        **00113200
113300******************************************************************00113300
113400     ADD +1 TO DF-ERROR-COUNT.                                    00113400
113500     MOVE HIGH-VALUES TO DF-ERR-CODE (DF-ERROR-INDEX).            00113500
113600     IF ERROR-FOUND                                               00113600
113700         MOVE 'E' TO GC024030-IND                                 00113700
113800     ELSE                                                         00113800
113900         MOVE 'G' TO GC024030-IND.                                00113900
114000                                                                  00114000
114100     CALL 'GC024030' USING GC024030-IND.                          00114100
114200                                                                  00114200
114300 10000-EXIT.                                                      00114300
114400     EXIT.                                                        00114400
114500                                                                  00114500
114600/                                                                 00114600
114700 10090-SEARCH-GRP-SPEC-TAB.                                       00114700
114800                                                                  00114800
114900     IF GCG-TAB-ID (GCG-INDEX) = '#GVLF '                         00114900
115000        MOVE 'Y' TO WS-GVLF-SW.                                   00115000
115100                                                                  00115100
115200     IF GCG-TAB-ID (GCG-INDEX) = '#GVLG '                         00115200
115300        MOVE 'Y' TO WS-GVLG-SW.                                   00115300
115400                                                                  00115400
115500     IF GCG-TAB-ID (GCG-INDEX) = '#GVLH '                         00115500
115600        MOVE 'Y' TO WS-GVLH-SW.                                   00115600
115700                                                                  00115700
115800     IF GCG-TAB-ID (GCG-INDEX) = '#GVLP '                         00115800
115900        MOVE 'Y' TO WS-GVLP-SW.                                   00115900
116000                                                                  00116000
116100     IF GCG-TAB-ID (GCG-INDEX) = '#GVLQ '                         00116100
116200        MOVE 'Y' TO WS-GVLQ-SW.                                   00116200
116300                                                                  00116300
116400     IF GCG-TAB-ID (GCG-INDEX) = '#GVLR '                         00116400
116500        MOVE 'Y' TO WS-GVLR-SW.                                   00116500
116600                                                                  00116600
116700                                                                  00116700
116800 10090-EXIT.                                                      00116800
116900      EXIT.                                                       00116900
117000                                                                  00117000
117100                                                                  00117100
117200/*****************************************************************00117200
117300** PICK UP ALL TABULAR RECORDS THAT, IF PRESENT, NEED TO BE     **00117300
117400** EDITED.  REMEMBER THAT THE EDITS ARE 'VICE-VERSA'.           **00117400
117500** IF A PARTICULAR TABULAR IS PRESENT, THEN CERTAIN FIELD       **00117500
117600** VALUES ARE REQUIRED IN THE GROUP SPECIFIC RECORD, AND        **00117600
117700** VICE-VERSA.  WE NEED TO READ THEM TO MAKE SURE THEY REALLY   **00117700
117800** ARE ON FILE.  ALSO, SOME TABULARS MUST HAVE CERTAIN VALUES   **00117800
117900** IN CERTAIN FIELDS.                                           **00117900
118000******************************************************************00118000
118100 11000-READ-TABS.                                                 00118100
118200                                                                  00118200
118300     IF GCG-TAB-ID (GCG-INDEX) = '#ABM  '                         00118300
118400             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00118400
118500         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00118500
118600         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00118600
118700             WS-ABM-SLOT WS-TAB-SLOT                              00118700
118800         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00118800
118900         IF REQUEST-TYPE-2 = 'R'                                  00118900
119000             MOVE GC-GCTABULR-ABM-VARY-MAX-OCUR                   00119000
119100               TO GAA-ENTRY-COUNT                                 00119100
119200*            MOVE +29 TO GAA-ENTRY-COUNT                          00119200
119300             MOVE TWOA-REC-AREA00 TO ABM-REC                      00119300
119400             IF GAA-ENTRY-COUNT < 2                               00119400
119500                 MOVE 'E' TO WS-ABM-SW                            00119500
119600             ELSE                                                 00119600
119700                 MOVE 'Y' TO WS-ABM-SW.                           00119700
119800                                                                  00119800
119900     IF GCG-TAB-ID (GCG-INDEX) = '#ACL  '                         00119900
120000             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00120000
120100         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00120100
120200         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00120200
120300             WS-ACL-SLOT WS-TAB-SLOT                              00120300
120400         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00120400
120500         IF REQUEST-TYPE-2 = 'R'                                  00120500
120600             MOVE GC-GCTABULR-ACL-VARY-MAX-OCUR                   00120600
120700               TO GAB-ENTRY-COUNT                                 00120700
120800*            MOVE +29 TO GAB-ENTRY-COUNT                          00120800
120900             MOVE TWOA-REC-AREA00 TO ACL-REC                      00120900
121000             IF GAB-ENTRY-COUNT < 2                               00121000
121100                 MOVE 'E' TO WS-ACL-SW                            00121100
121200             ELSE                                                 00121200
121300                 MOVE 'Y' TO WS-ACL-SW.                           00121300
121400                                                                  00121400
121500     IF GCG-TAB-ID (GCG-INDEX) = '#ACP  '                         00121500
121600             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00121600
121700         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00121700
121800         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00121800
121900             WS-ACP-SLOT WS-TAB-SLOT                              00121900
122000         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00122000
122100         IF REQUEST-TYPE-2 = 'R'                                  00122100
122200             MOVE GC-GCTABULR-ACP-VARY-MAX-OCUR                   00122200
122300               TO GAF-ENTRY-COUNT                                 00122300
122400*            MOVE +29 TO GAC-ENTRY-COUNT                          00122400
122500             MOVE TWOA-REC-AREA00 TO ACP-REC                      00122500
122600             IF GAF-ENTRY-COUNT < 2                               00122600
122700                 MOVE 'E' TO WS-ACP-SW                            00122700
122800             ELSE                                                 00122800
122900                 MOVE 'Y' TO WS-ACP-SW.                           00122900
123000                                                                  00123000
123100     IF GCG-TAB-ID (GCG-INDEX) = '#ADL  '                         00123100
123200             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00123200
123300         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00123300
123400         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00123400
123500             WS-ADL-SLOT WS-TAB-SLOT                              00123500
123600         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00123600
123700         IF REQUEST-TYPE-2 = 'R'                                  00123700
123800             MOVE GC-GCTABULR-ADL-VARY-MAX-OCUR                   00123800
123900               TO GAC-ENTRY-COUNT                                 00123900
124000*            MOVE +29 TO GAC-ENTRY-COUNT                          00124000
124100             MOVE TWOA-REC-AREA00 TO ADL-REC                      00124100
124200             IF GAC-ENTRY-COUNT < 2                               00124200
124300                 MOVE 'E' TO WS-ADL-SW                            00124300
124400             ELSE                                                 00124400
124500                 MOVE 'Y' TO WS-ADL-SW.                           00124500
124600                                                                  00124600
124700     IF GCG-TAB-ID (GCG-INDEX) = '#AOL  '                         00124700
124800             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00124800
124900         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00124900
125000         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00125000
125100             WS-AOL-SLOT WS-TAB-SLOT                              00125100
125200         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00125200
125300         IF REQUEST-TYPE-2 = 'R'                                  00125300
125400             MOVE GC-GCTABULR-AOL-VARY-MAX-OCUR                   00125400
125500               TO GAD-ENTRY-COUNT                                 00125500
125600*            MOVE +29 TO GAD-ENTRY-COUNT                          00125600
125700             MOVE TWOA-REC-AREA00 TO AOL-REC                      00125700
125800             IF GAD-ENTRY-COUNT < 2                               00125800
125900                 MOVE 'E' TO WS-AOL-SW                            00125900
126000             ELSE                                                 00126000
126100                 MOVE 'Y' TO WS-AOL-SW.                           00126100
126200                                                                  00126200
126300     IF GCG-TAB-ID (GCG-INDEX)   =  '#GCBL '                      00126300
126400      AND GCG-TAB-SLOT-NO (GCG-INDEX)   NOT =   ZEROES            00126400
126500          MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                00126500
126600          MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                        00126600
126700            TO WS-GCBL-SLOT WS-TAB-SLOT                           00126700
126800          PERFORM 92000-READ-TABULAR THRU 92000-EXIT              00126800
126900          IF REQUEST-TYPE-2 = 'R'                                 00126900
127000             MOVE GC-GCTABULR-GCBL-VARY-MAX-OCUR                  00127000
127100               TO GS92-ENTRY-COUNT                                00127100
127200             MOVE TWOA-REC-AREA00 TO GCBL-REC                     00127200
127300             IF GS92-ENTRY-COUNT < 2                              00127300
127400                 MOVE 'E' TO WS-GCBL-SW                           00127400
127500             ELSE                                                 00127500
127600                 MOVE 'Y' TO WS-GCBL-SW.                          00127600
127700                                                                  00127700
127800     IF GCG-TAB-ID (GCG-INDEX) = '#GCCP '                         00127800
127900             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00127900
128000         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00128000
128100         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00128100
128200             WS-GCCP-SLOT WS-TAB-SLOT                             00128200
128300         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00128300
128400         IF REQUEST-TYPE-2 = 'R'                                  00128400
128500             MOVE GC-GCTABULR-GCCP-VARY-MAX-OCUR                  00128500
128600                 TO GSS2-ENTRY-COUNT                              00128600
128700             MOVE TWOA-REC-AREA00 TO GCCP-REC                     00128700
128800             PERFORM 12000-COST-CONTAIN-BREAKDOWN                 00128800
128900                 THRU 12000-EXIT                                  00128900
129000                 VARYING GSS2-INDEX FROM 1 BY 1                   00129000
129100                 UNTIL GSS2-INDEX = GSS2-ENTRY-COUNT              00129100
129200                 OR GSS2-REC-TAB-ENTRY (GSS2-INDEX)               00129200
129300                 = HIGH-VALUES                                    00129300
129400             IF GSS2-ENTRY-COUNT < 2                              00129400
129500                 MOVE 'E' TO WS-GCCP-SW                           00129500
129600             ELSE                                                 00129600
129700                 MOVE 'Y' TO WS-GCCP-SW.                          00129700
129800                                                                  00129800
129900     IF GCG-TAB-ID (GCG-INDEX) = '#GMCG '                         00129900
130000             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00130000
130100         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00130100
130200         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00130200
130300             WS-GMCG-SLOT WS-TAB-SLOT                             00130300
130400         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00130400
130500         IF REQUEST-TYPE-2 = 'R'                                  00130500
130600             MOVE GC-GCTABULR-GMCG-VARY-MAX-OCUR                  00130600
130700               TO GSS2-ENTRY-COUNT                                00130700
130800*            MOVE +304 TO GSS2-ENTRY-COUNT                        00130800
130900             MOVE TWOA-REC-AREA00 TO GMCG-REC                     00130900
131000             IF GS12-ENTRY-COUNT < 2                              00131000
131100                 MOVE 'E' TO WS-GMCG-SW                           00131100
131200             ELSE                                                 00131200
131300                 MOVE 'Y' TO WS-GMCG-SW.                          00131300
131400                                                                  00131400
131500     IF GCG-TAB-ID (GCG-INDEX)  = '#GPAN '                        00131500
131600      AND GCG-TAB-SLOT-NO (GCG-INDEX)   NOT =  ZEROES             00131600
131700          MOVE  GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID               00131700
131800          MOVE  GCG-TAB-SLOT-NO (GCG-INDEX)                       00131800
131900            TO  WS-GPAN-SLOT WS-TAB-SLOT                          00131900
132000          PERFORM 92000-READ-TABULAR THRU 92000-EXIT              00132000
132100          IF REQUEST-TYPE-2 = 'R'                                 00132100
132200             MOVE GC-GCTABULR-GPAN-VARY-MAX-OCUR                  00132200
132300               TO GS102-ENTRY-COUNT                               00132300
132400             MOVE TWOA-REC-AREA00 TO GPAN-REC                     00132400
132500             IF GS102-ENTRY-COUNT < 2                             00132500
132600                 MOVE 'E' TO WS-GPAN-SW                           00132600
132700             ELSE                                                 00132700
132800                 MOVE 'Y' TO WS-GPAN-SW.                          00132800
132900                                                                  00132900
133000     IF GCG-TAB-ID (GCG-INDEX) = '#GPPO '                         00133000
133100             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00133100
133200         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00133200
133300         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00133300
133400             WS-GPPO-SLOT WS-TAB-SLOT                             00133400
133500         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00133500
133600         IF REQUEST-TYPE-2 = 'R'                                  00133600
133700             MOVE GC-GCTABULR-GPPO-VARY-MAX-OCUR                  00133700
133800               TO GSW2-ENTRY-COUNT                                00133800
133900*            MOVE +565 TO GSW2-ENTRY-COUNT                        00133900
134000             MOVE TWOA-REC-AREA00 TO GPPO-REC                     00134000
134100             IF GSW2-ENTRY-COUNT < 2                              00134100
134200                 MOVE 'E' TO WS-GPPO-SW                           00134200
134300             ELSE                                                 00134300
134400                 MOVE 'Y' TO WS-GPPO-SW.                          00134400
134500     IF GCG-TAB-ID (GCG-INDEX) = '#GCPO '                         00134500
134600             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00134600
134700         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00134700
134800         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00134800
134900             WS-GCPO-SLOT WS-TAB-SLOT                             00134900
135000         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00135000
135100         IF REQUEST-TYPE-2 = 'R'                                  00135100
135200             MOVE GC-GCTABULR-GCPO-VARY-MAX-OCUR                  00135200
135300               TO GS82-ENTRY-COUNT                                00135300
135400*            MOVE +565 TO GS82-ENTRY-COUNT                        00135400
135500             MOVE TWOA-REC-AREA00 TO GCPO-REC                     00135500
135600             IF GS82-ENTRY-COUNT < 2                              00135600
135700                 MOVE 'E' TO WS-GCPO-SW                           00135700
135800             ELSE                                                 00135800
135900                 MOVE 'Y' TO WS-GCPO-SW.                          00135900
136000                                                                  00136000
136100     IF GCG-TAB-ID (GCG-INDEX) = '#GRPO '                         00136100
136200             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00136200
136300         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00136300
136400         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00136400
136500             WS-GRPO-SLOT WS-TAB-SLOT                             00136500
136600         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00136600
136700         IF REQUEST-TYPE-2 = 'R'                                  00136700
136800             MOVE GC-GCTABULR-GRPO-VARY-MAX-OCUR                  00136800
136900               TO GS72-ENTRY-COUNT                                00136900
137000*            MOVE +565 TO GS72-ENTRY-COUNT                        00137000
137100             MOVE TWOA-REC-AREA00 TO GRPO-REC                     00137100
137200             IF GS72-ENTRY-COUNT < 2                              00137200
137300                 MOVE 'E' TO WS-GRPO-SW                           00137300
137400             ELSE                                                 00137400
137500                 MOVE 'Y' TO WS-GRPO-SW.                          00137500
137600                                                                  00137600
137700     IF GCG-TAB-ID (GCG-INDEX) = '#GBAE '                         00137700
137800             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00137800
137900         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00137900
138000         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00138000
138100             WS-GBAE-SLOT WS-TAB-SLOT                             00138100
138200         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00138200
138300         IF REQUEST-TYPE-2 = 'R'                                  00138300
138400             MOVE GC-GCTABULR-GBAE-VARY-MAX-OCUR                  00138400
138500               TO GS162-ENTRY-COUNT                               00138500
138600*            MOVE +565 TO GS162-ENTRY-COUNT                       00138600
138700             MOVE TWOA-REC-AREA00 TO GBAE-REC                     00138700
138800             IF GS162-ENTRY-COUNT < 2                             00138800
138900                 MOVE 'E' TO WS-GBAE-SW                           00138900
139000             ELSE                                                 00139000
139100                 MOVE 'Y' TO WS-GBAE-SW.                          00139100
139200                                                                  00139200
139300     IF GCG-TAB-ID (GCG-INDEX) = '#GHOR '                         00139300
139400             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00139400
139500         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00139500
139600         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00139600
139700             WS-GHOR-SLOT WS-TAB-SLOT                             00139700
139800         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00139800
139900         IF REQUEST-TYPE-2 = 'R'                                  00139900
140000             MOVE GC-GCTABULR-GHOR-VARY-MAX-OCUR                  00140000
140100               TO GSO2-ENTRY-COUNT                                00140100
140200*            MOVE +565 TO GSO2-ENTRY-COUNT                        00140200
140300             MOVE TWOA-REC-AREA00 TO GHOR-REC                     00140300
140400             IF GSO2-ENTRY-COUNT < 2                              00140400
140500                 MOVE 'E' TO WS-GHOR-SW                           00140500
140600             ELSE                                                 00140600
140700                 MOVE 'Y' TO WS-GHOR-SW.                          00140700
140800                                                                  00140800
140900     IF GCG-TAB-ID (GCG-INDEX) = '#GHOB '                         00140900
141000             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00141000
141100         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00141100
141200         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00141200
141300             WS-GHOB-SLOT WS-TAB-SLOT                             00141300
141400         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00141400
141500         IF REQUEST-TYPE-2 = 'R'                                  00141500
141600             MOVE GC-GCTABULR-GHOB-VARY-MAX-OCUR                  00141600
141700               TO GSA2-ENTRY-COUNT                                00141700
141800*            MOVE +565 TO GSA2-ENTRY-COUNT                        00141800
141900             MOVE TWOA-REC-AREA00 TO GHOB-REC                     00141900
142000             IF GSA2-ENTRY-COUNT < 2                              00142000
142100                 MOVE 'E' TO WS-GHOB-SW                           00142100
142200             ELSE                                                 00142200
142300                 MOVE 'Y' TO WS-GHOB-SW.                          00142300
142400                                                                  00142400
142500     IF GCG-TAB-ID (GCG-INDEX) = '#GMCD '                         00142500
142600             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00142600
142700         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00142700
142800         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00142800
142900             WS-GMCD-SLOT WS-TAB-SLOT                             00142900
143000         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00143000
143100         IF REQUEST-TYPE-2 = 'R'                                  00143100
143200             MOVE GC-GCTABULR-GMCD-VARY-MAX-OCUR                  00143200
143300               TO GS22-ENTRY-COUNT                                00143300
143400*            MOVE +565 TO GS22-ENTRY-COUNT                        00143400
143500             MOVE TWOA-REC-AREA00 TO GMCD-REC                     00143500
143600             IF GS22-ENTRY-COUNT < 2                              00143600
143700                 MOVE 'E' TO WS-GMCD-SW                           00143700
143800             ELSE                                                 00143800
143900                 MOVE 'Y' TO WS-GMCD-SW.                          00143900
144000                                                                  00144000
144100     IF GCG-TAB-ID (GCG-INDEX) = '#GMCR '                         00144100
144200             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00144200
144300         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00144300
144400         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00144400
144500             WS-GMCR-SLOT WS-TAB-SLOT                             00144500
144600         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00144600
144700         IF REQUEST-TYPE-2 = 'R'                                  00144700
144800             MOVE GC-GCTABULR-GMCR-VARY-MAX-OCUR                  00144800
144900               TO GS42-ENTRY-COUNT                                00144900
145000*            MOVE +565 TO GS42-ENTRY-COUNT                        00145000
145100             MOVE TWOA-REC-AREA00 TO GMCR-REC                     00145100
145200             IF GS42-ENTRY-COUNT < 2                              00145200
145300                 MOVE 'E' TO WS-GMCR-SW                           00145300
145400             ELSE                                                 00145400
145500                 MOVE 'Y' TO WS-GMCR-SW.                          00145500
145600                                                                  00145600
145700     IF GCG-TAB-ID (GCG-INDEX) = '#GMDN '                         00145700
145800             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00145800
145900         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00145900
146000         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00146000
146100             WS-GMDN-SLOT WS-TAB-SLOT                             00146100
146200         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00146200
146300         IF REQUEST-TYPE-2 = 'R'                                  00146300
146400             MOVE GC-GCTABULR-GMDN-VARY-MAX-OCUR                  00146400
146500               TO GS32-ENTRY-COUNT                                00146500
146600*            MOVE +565 TO GS32-ENTRY-COUNT                        00146600
146700             MOVE TWOA-REC-AREA00 TO GMDN-REC                     00146700
146800             IF GS32-ENTRY-COUNT < 2                              00146800
146900                 MOVE 'E' TO WS-GMDN-SW                           00146900
147000             ELSE                                                 00147000
147100                 MOVE 'Y' TO WS-GMDN-SW.                          00147100
147200                                                                  00147200
147300* ADDED 2/29/92  FRY   D12262                                     00147300
147400     IF GCG-TAB-ID (GCG-INDEX) = '#GMCS '                         00147400
147500       AND  GCG-TAB-SLOT-NO (GCG-INDEX)  NOT =  ZERO              00147500
147600         MOVE  GCG-TAB-ID (GCG-INDEX)      TO  WS-TAB-ID          00147600
147700         MOVE  GCG-TAB-SLOT-NO (GCG-INDEX)                        00147700
147800           TO  WS-GMCS-SLOT WS-TAB-SLOT                           00147800
147900         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00147900
148000         IF REQUEST-TYPE-2 = 'R'                                  00148000
148100             MOVE GC-GCTABULR-GMCS-VARY-MAX-OCUR                  00148100
148200               TO GS62-ENTRY-COUNT                                00148200
148300*            MOVE +565 TO GS62-ENTRY-COUNT                        00148300
148400             MOVE TWOA-REC-AREA00 TO GMCS-REC                     00148400
148500             IF GS62-ENTRY-COUNT < 2                              00148500
148600                 MOVE 'E' TO WS-GMCS-SW                           00148600
148700             ELSE                                                 00148700
148800                 MOVE 'Y' TO WS-GMCS-SW.                          00148800
148900                                                                  00148900
149000     IF GCG-TAB-ID (GCG-INDEX) = '#GMPR '                         00149000
149100             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00149100
149200         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00149200
149300         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00149300
149400             WS-GMPR-SLOT WS-TAB-SLOT                             00149400
149500         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00149500
149600         IF REQUEST-TYPE-2 = 'R'                                  00149600
149700             MOVE GC-GCTABULR-GMPR-VARY-MAX-OCUR                  00149700
149800               TO GSR2-ENTRY-COUNT                                00149800
149900*            MOVE +565 TO GSR2-ENTRY-COUNT                        00149900
150000             MOVE TWOA-REC-AREA00 TO GMPR-REC                     00150000
150100             IF GSR2-ENTRY-COUNT < 2                              00150100
150200                 MOVE 'E' TO WS-GMPR-SW                           00150200
150300             ELSE                                                 00150300
150400                 MOVE 'Y' TO WS-GMPR-SW.                          00150400
150500                                                                  00150500
150600     IF GCG-TAB-ID (GCG-INDEX) = '#GMPB '                         00150600
150700             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00150700
150800         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00150800
150900         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00150900
151000             WS-GMPB-SLOT WS-TAB-SLOT                             00151000
151100         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00151100
151200         IF REQUEST-TYPE-2 = 'R'                                  00151200
151300             MOVE GC-GCTABULR-GMPB-VARY-MAX-OCUR                  00151300
151400               TO GSH2-ENTRY-COUNT                                00151400
151500*            MOVE +565 TO GSH2-ENTRY-COUNT                        00151500
151600             MOVE TWOA-REC-AREA00 TO GMPB-REC                     00151600
151700             IF GSH2-ENTRY-COUNT < 2                              00151700
151800                 MOVE 'E' TO WS-GMPB-SW                           00151800
151900             ELSE                                                 00151900
152000                 MOVE 'Y' TO WS-GMPB-SW.                          00152000
152100                                                                  00152100
152200                                                                  00152200
152300     IF GCG-TAB-ID (GCG-INDEX) = '#GPAR '                         00152300
152400             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00152400
152500         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00152500
152600         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00152600
152700             WS-GPAR-SLOT WS-TAB-SLOT                             00152700
152800         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00152800
152900         IF REQUEST-TYPE-2 = 'R'                                  00152900
153000             MOVE GC-GCTABULR-GPAR-VARY-MAX-OCUR                  00153000
153100               TO GSK2-ENTRY-COUNT                                00153100
153200*            MOVE +565 TO GSK2-ENTRY-COUNT                        00153200
153300             MOVE TWOA-REC-AREA00 TO GPAR-REC                     00153300
153400             IF GSK2-ENTRY-COUNT < 2                              00153400
153500                 MOVE 'E' TO WS-GPAR-SW                           00153500
153600             ELSE                                                 00153600
153700                 MOVE 'Y' TO WS-GPAR-SW.                          00153700
153800                                                                  00153800
153900     IF GCG-TAB-ID (GCG-INDEX) = '#GPAD '                         00153900
154000             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00154000
154100         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00154100
154200         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00154200
154300             WS-GPAD-SLOT WS-TAB-SLOT                             00154300
154400         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00154400
154500         IF REQUEST-TYPE-2 = 'R'                                  00154500
154600             MOVE GC-GCTABULR-GPAD-VARY-MAX-OCUR                  00154600
154700               TO GSC2-ENTRY-COUNT                                00154700
154800*            MOVE +565 TO GSC2-ENTRY-COUNT                        00154800
154900             MOVE TWOA-REC-AREA00 TO GPAD-REC                     00154900
155000             IF GSC2-ENTRY-COUNT < 2                              00155000
155100                 MOVE 'E' TO WS-GPAD-SW                           00155100
155200             ELSE                                                 00155200
155300                 MOVE 'Y' TO WS-GPAD-SW.                          00155300
155400                                                                  00155400
155500     IF GCG-TAB-ID (GCG-INDEX) = '#GPAB '                         00155500
155600             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00155600
155700         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00155700
155800         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00155800
155900             WS-GPAB-SLOT WS-TAB-SLOT                             00155900
156000         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00156000
156100         IF REQUEST-TYPE-2 = 'R'                                  00156100
156200             MOVE GC-GCTABULR-GPAB-VARY-MAX-OCUR                  00156200
156300               TO GST2-ENTRY-COUNT                                00156300
156400*            MOVE +565 TO GST2-ENTRY-COUNT                        00156400
156500             MOVE TWOA-REC-AREA00 TO GPAB-REC                     00156500
156600             IF GST2-ENTRY-COUNT < 2                              00156600
156700                 MOVE 'E' TO WS-GPAB-SW                           00156700
156800             ELSE                                                 00156800
156900                 MOVE 'Y' TO WS-GPAB-SW.                          00156900
157000                                                                  00157000
157100     IF GCG-TAB-ID (GCG-INDEX) = '#GMOR '                         00157100
157200             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00157200
157300         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00157300
157400         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00157400
157500             WS-GMOR-SLOT WS-TAB-SLOT                             00157500
157600         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00157600
157700         IF REQUEST-TYPE-2 = 'R'                                  00157700
157800             MOVE GC-GCTABULR-GMOR-VARY-MAX-OCUR                  00157800
157900               TO GSN2-ENTRY-COUNT                                00157900
158000*            MOVE +565 TO GSN2-ENTRY-COUNT                        00158000
158100             MOVE TWOA-REC-AREA00 TO GMOR-REC                     00158100
158200             IF GSN2-ENTRY-COUNT < 2                              00158200
158300                 MOVE 'E' TO WS-GMOR-SW                           00158300
158400             ELSE                                                 00158400
158500                 MOVE 'Y' TO WS-GMOR-SW.                          00158500
158600                                                                  00158600
158700     IF GCG-TAB-ID (GCG-INDEX) = '#GMOB '                         00158700
158800             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00158800
158900         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00158900
159000         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00159000
159100             WS-GMOB-SLOT WS-TAB-SLOT                             00159100
159200         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00159200
159300         IF REQUEST-TYPE-2 = 'R'                                  00159300
159400             MOVE GC-GCTABULR-GMOB-VARY-MAX-OCUR                  00159400
159500               TO GSG2-ENTRY-COUNT                                00159500
159600*            MOVE +565 TO GSG2-ENTRY-COUNT                        00159600
159700             MOVE TWOA-REC-AREA00 TO GMOB-REC                     00159700
159800             IF GSG2-ENTRY-COUNT < 2                              00159800
159900                 MOVE 'E' TO WS-GMOB-SW                           00159900
160000             ELSE                                                 00160000
160100                 MOVE 'Y' TO WS-GMOB-SW.                          00160100
160200                                                                  00160200
160300     IF GCG-TAB-ID (GCG-INDEX) = '#GFHC '                         00160300
160400             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00160400
160500         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00160500
160600         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00160600
160700             WS-GFHC-SLOT WS-TAB-SLOT                             00160700
160800         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00160800
160900         IF REQUEST-TYPE-2 = 'R'                                  00160900
161000             MOVE GC-GCTABULR-GFHC-VARY-MAX-OCUR                  00161000
161100               TO GFHC2-ENTRY-COUNT                               00161100
161200*            MOVE +565 TO GFHC2-ENTRY-COUNT                       00161200
161300             MOVE TWOA-REC-AREA00 TO GFHC-REC                     00161300
161400             IF GFHC2-ENTRY-COUNT < 2                             00161400
161500                 MOVE 'E' TO WS-GFHC-SW                           00161500
161600             ELSE                                                 00161600
161700                 MOVE 'Y' TO WS-GFHC-SW.                          00161700
161800                                                                  00161800
161900     IF GCG-TAB-ID (GCG-INDEX) = '#GFSA '                         00161900
162000             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00162000
162100         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00162100
162200         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00162200
162300             WS-GFSA-SLOT WS-TAB-SLOT                             00162300
162400         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00162400
162500         IF REQUEST-TYPE-2 = 'R'                                  00162500
162600             MOVE GC-GCTABULR-GFSA-VARY-MAX-OCUR                  00162600
162700               TO GFSA2-ENTRY-COUNT                               00162700
162800*            MOVE +565 TO GFSA2-ENTRY-COUNT                       00162800
162900             MOVE TWOA-REC-AREA00 TO GFSA-REC                     00162900
163000             IF GFSA2-ENTRY-COUNT < 2                             00163000
163100                 MOVE 'E' TO WS-GFSA-SW                           00163100
163200             ELSE                                                 00163200
163300                 MOVE 'Y' TO WS-GFSA-SW.                          00163300
163400                                                                  00163400
163500     IF GCG-TAB-ID (GCG-INDEX) = '#GHCA '                         00163500
163600             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00163600
163700         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00163700
163800         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00163800
163900             WS-GHCA-SLOT WS-TAB-SLOT                             00163900
164000         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00164000
164100         IF REQUEST-TYPE-2 = 'R'                                  00164100
164200             MOVE GC-GCTABULR-GHCA-VARY-MAX-OCUR                  00164200
164300               TO GHCA2-ENTRY-COUNT                               00164300
164400*            MOVE +565 TO GHCA2-ENTRY-COUNT                       00164400
164500             MOVE TWOA-REC-AREA00 TO GHCA-REC                     00164500
164600             IF GHCA2-ENTRY-COUNT < 2                             00164600
164700                 MOVE 'E' TO WS-GHCA-SW                           00164700
164800             ELSE                                                 00164800
164900                 MOVE 'Y' TO WS-GHCA-SW.                          00164900
165000                                                                  00165000
165100     IF GCG-TAB-ID (GCG-INDEX) = '#GHSA '                         00165100
165200             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00165200
165300         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00165300
165400         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00165400
165500             WS-GHSA-SLOT WS-TAB-SLOT                             00165500
165600         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00165600
165700         IF REQUEST-TYPE-2 = 'R'                                  00165700
165800             MOVE GC-GCTABULR-GHSA-VARY-MAX-OCUR                  00165800
165900               TO GHSA2-ENTRY-COUNT                               00165900
166000*            MOVE +565 TO GHSA2-ENTRY-COUNT                       00166000
166100             MOVE TWOA-REC-AREA00 TO GHSA-REC                     00166100
166200             IF GHSA2-ENTRY-COUNT < 2                             00166200
166300                 MOVE 'E' TO WS-GHSA-SW                           00166300
166400             ELSE                                                 00166400
166500                 MOVE 'Y' TO WS-GHSA-SW.                          00166500
166600                                                                  00166600
166700     IF GCG-TAB-ID (GCG-INDEX) = '#GLPF '                         00166700
166800             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00166800
166900         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00166900
167000         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00167000
167100             WS-GLPF-SLOT WS-TAB-SLOT                             00167100
167200         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00167200
167300         IF REQUEST-TYPE-2 = 'R'                                  00167300
167400             MOVE GC-GCTABULR-GLPF-VARY-MAX-OCUR                  00167400
167500               TO GLPF2-ENTRY-COUNT                               00167500
167600*            MOVE +565 TO GLPF2-ENTRY-COUNT                       00167600
167700             MOVE TWOA-REC-AREA00 TO GLPF-REC                     00167700
167800             IF GLPF2-ENTRY-COUNT < 2                             00167800
167900                 MOVE 'E' TO WS-GLPF-SW                           00167900
168000             ELSE                                                 00168000
168100                 MOVE 'Y' TO WS-GLPF-SW.                          00168100
168200                                                                  00168200
168300     IF GCG-TAB-ID (GCG-INDEX) = '#GLPH '                         00168300
168400             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00168400
168500         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00168500
168600         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00168600
168700             WS-GLPH-SLOT WS-TAB-SLOT                             00168700
168800         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00168800
168900         IF REQUEST-TYPE-2 = 'R'                                  00168900
169000             MOVE GC-GCTABULR-GLPH-VARY-MAX-OCUR                  00169000
169100               TO GLPH2-ENTRY-COUNT                               00169100
169200*            MOVE +565 TO GLPH2-ENTRY-COUNT                       00169200
169300             MOVE TWOA-REC-AREA00 TO GLPH-REC                     00169300
169400             IF GLPH2-ENTRY-COUNT < 2                             00169400
169500                 MOVE 'E' TO WS-GLPH-SW                           00169500
169600             ELSE                                                 00169600
169700                 MOVE 'Y' TO WS-GLPH-SW.                          00169700
169800                                                                  00169800
169900     IF GCG-TAB-ID (GCG-INDEX) = '#GWHC '                         00169900
170000             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00170000
170100         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00170100
170200         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00170200
170300             WS-GWHC-SLOT WS-TAB-SLOT                             00170300
170400         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00170400
170500         IF REQUEST-TYPE-2 = 'R'                                  00170500
170600             MOVE GC-GCTABULR-GWHC-VARY-MAX-OCUR                  00170600
170700               TO GWHC2-ENTRY-COUNT                               00170700
170800*            MOVE +565 TO GWHC2-ENTRY-COUNT                       00170800
170900             MOVE TWOA-REC-AREA00 TO GWHC-REC                     00170900
171000             IF GWHC2-ENTRY-COUNT < 2                             00171000
171100                 MOVE 'E' TO WS-GWHC-SW                           00171100
171200             ELSE                                                 00171200
171300                 MOVE 'Y' TO WS-GWHC-SW.                          00171300
171400                                                                  00171400
171500     IF GCG-TAB-ID (GCG-INDEX) = '#GMFH '                         00171500
171600             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00171600
171700         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00171700
171800         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00171800
171900             WS-GMFH-SLOT WS-TAB-SLOT                             00171900
172000         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00172000
172100         IF REQUEST-TYPE-2 = 'R'                                  00172100
172200             MOVE GC-GCTABULR-GMFH-VARY-MAX-OCUR                  00172200
172300               TO GMFH2-ENTRY-COUNT                               00172300
172400             MOVE TWOA-REC-AREA00 TO GMFH-REC                     00172400
172500             IF GMFH2-ENTRY-COUNT < 2                             00172500
172600                 MOVE 'E' TO WS-GMFH-SW                           00172600
172700             ELSE                                                 00172700
172800                 MOVE 'Y' TO WS-GMFH-SW.                          00172800
172900                                                                  00172900
TM0526     IF GCG-TAB-ID (GCG-INDEX) = '#GHPA '                         00172910
TM0526             AND GCG-TAB-SLOT-NO (GCG-INDEX) NOT = ZERO           00172920
TM0526         MOVE GCG-TAB-ID (GCG-INDEX) TO WS-TAB-ID                 00172930
TM0526         MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                      00172940
TM0526             WS-GHPA-SLOT WS-TAB-SLOT                             00172950
TM0526         PERFORM 92000-READ-TABULAR THRU 92000-EXIT               00172960
TM0526         IF REQUEST-TYPE-2 = 'R'                                  00172970
TM0526             MOVE GC-GCTABULR-GHPA-VARY-MAX-OCUR                  00172980
TM0526               TO GHPA2-ENTRY-COUNT                               00172990
TM0526             MOVE TWOA-REC-AREA00 TO GHPA-REC                     00172991
TM0526             IF GHPA2-ENTRY-COUNT < 2                             00172992
TM0526                 MOVE 'E' TO WS-GHPA-SW                           00172993
TM0526             ELSE                                                 00172994
TM0526                 MOVE 'Y' TO WS-GHPA-SW.                          00172995
TM0526                                                                  00172996
173000 11000-EXIT.                                                      00173000
173100     EXIT.                                                        00173100
173200                                                                  00173200
173300******************************************************************00173300
173400** THE COST CONTAINMENT TABULAR CAN CONTAIN UP TO 25 DIFFERENT  **00173400
173500** ENTRIES.  BEFORE DOING THE EDITS, WE NEED TO SEE WHICH ONES  **00173500
173600** ARE PRESENT.  IF A GIVEN ENTRY EXISTS, THEN CERTAIN          **00173600
173700** GROUP SPECIFIC FIELD CONTENTS MAY BE REQUIRED.               **00173700
173800******************************************************************00173800
173900 12000-COST-CONTAIN-BREAKDOWN.                                    00173900
174000                                                                  00174000
174100     IF GSS2-AT-PROG-CODE-CHR (GSS2-INDEX)                        00174100
174200         SET WS-AT-GSS2-INDEX TO GSS2-INDEX                       00174200
174300         MOVE 'Y' TO WS-AT-FOUND-SW.                              00174300
174400                                                                  00174400
174500     IF GSS2-BA-PROG-CODE-CHR (GSS2-INDEX)                        00174500
174600         SET WS-BA-GSS2-INDEX TO GSS2-INDEX                       00174600
174700         MOVE 'Y' TO WS-BA-FOUND-SW.                              00174700
174800                                                                  00174800
AT0718     IF GSS2-BD-PROG-CODE-CHR (GSS2-INDEX)                        00174810
AT0718         SET WS-BD-GSS2-INDEX TO GSS2-INDEX                       00174820
AT0718         MOVE 'Y' TO WS-BD-FOUND-SW.                              00174830
174800                                                                  00174840
174900     IF GSS2-CB-PROG-CODE-CHR (GSS2-INDEX)                        00174900
175000         SET WS-CB-GSS2-INDEX TO GSS2-INDEX                       00175000
175100         MOVE 'Y' TO WS-CB-FOUND-SW.                              00175100
175200                                                                  00175200
      *--  TROY - ERS PROJECT                                           00175210
174900     IF GSS2-CE-PROG-CODE-CHR (GSS2-INDEX)                        00175211
175000         SET WS-CE-GSS2-INDEX TO GSS2-INDEX                       00175212
175100         MOVE 'Y' TO WS-CE-FOUND-SW.                              00175213
      *--  TROY - ERS PROJECT                                           00175220
175200                                                                  00175230
175300     IF GSS2-CP-PROG-CODE-CHR (GSS2-INDEX)                        00175300
175400         SET WS-CP-GSS2-INDEX TO GSS2-INDEX                       00175400
175500         MOVE 'Y' TO WS-CP-FOUND-SW.                              00175500
175600                                                                  00175600
      *--  SRI  - PSMNR PROJECT                                         00175610
174900     IF GSS2-CU-PROG-CODE-CHR (GSS2-INDEX)                        00175620
175000         SET WS-CU-GSS2-INDEX TO GSS2-INDEX                       00175630
175100         MOVE 'Y' TO WS-CU-FOUND-SW.                              00175640
      *--  SRI  - PSMNR PROJECT                                         00175650
      *--  SRI  - HAS PROJECT                                           00175660
174900     IF GSS2-EC-PROG-CODE-CHR (GSS2-INDEX)                        00175670
175000         SET WS-EC-GSS2-INDEX TO GSS2-INDEX                       00175680
175100         MOVE 'Y' TO WS-EC-FOUND-SW.                              00175690
      *--  SRI  - HAS PROJECT                                           00175691
175700     IF GSS2-FS-PROG-CODE-CHR (GSS2-INDEX)                        00175700
175800         SET WS-FS-GSS2-INDEX TO GSS2-INDEX                       00175800
175900         MOVE 'Y' TO WS-FS-FOUND-SW.                              00175900
176000                                                                  00176000
176100     IF GSS2-HM-PROG-CODE-CHR (GSS2-INDEX)                        00176100
176200         SET WS-HM-GSS2-INDEX TO GSS2-INDEX                       00176200
176300         MOVE 'Y' TO WS-HM-FOUND-SW.                              00176300
176400                                                                  00176400
176500     IF GSS2-HO-PROG-CODE-CHR (GSS2-INDEX)                        00176500
176600         SET WS-HO-GSS2-INDEX TO GSS2-INDEX                       00176600
176700         MOVE 'Y' TO WS-HO-FOUND-SW.                              00176700
176800                                                                  00176800
176900     IF GSS2-IO-PROG-CODE-CHR (GSS2-INDEX)                        00176900
177000         SET WS-IO-GSS2-INDEX TO GSS2-INDEX                       00177000
177100         MOVE 'Y' TO WS-IO-FOUND-SW.                              00177100
177200                                                                  00177200
177300     IF GSS2-MA-PROG-CODE-CHR (GSS2-INDEX)                        00177300
177400         SET WS-MA-GSS2-INDEX TO GSS2-INDEX                       00177400
177500         MOVE 'Y' TO WS-MA-FOUND-SW.                              00177500
177600                                                                  00177600
177700     IF GSS2-MD-PROG-CODE-CHR (GSS2-INDEX)                        00177700
177800         SET WS-MD-GSS2-INDEX TO GSS2-INDEX                       00177800
177900         MOVE 'Y' TO WS-MD-FOUND-SW.                              00177900
178000                                                                  00178000
178100     IF GSS2-MN-PROG-CODE-CHR (GSS2-INDEX)                        00178100
178200         SET WS-MN-GSS2-INDEX TO GSS2-INDEX                       00178200
178300         MOVE 'Y' TO WS-MN-FOUND-SW.                              00178300
178400                                                                  00178400
178500     IF GSS2-MO-PROG-CODE-CHR (GSS2-INDEX)                        00178500
178600         SET WS-MO-GSS2-INDEX TO GSS2-INDEX                       00178600
178700         MOVE 'Y' TO WS-MO-FOUND-SW.                              00178700
178800                                                                  00178800
178900     IF GSS2-MS-PROG-CODE-CHR (GSS2-INDEX)                        00178900
179000         SET WS-MS-GSS2-INDEX TO GSS2-INDEX                       00179000
179100         MOVE 'Y' TO WS-MS-FOUND-SW.                              00179100
179200                                                                  00179200
179300     IF GSS2-PA-PROG-CODE-CHR (GSS2-INDEX)                        00179300
179400         SET WS-PA-GSS2-INDEX TO GSS2-INDEX                       00179400
179500         MOVE 'Y' TO WS-PA-FOUND-SW.                              00179500
179600                                                                  00179600
      *--  SRI  - HAS PROJECT                                           00179610
174900     IF GSS2-PC-PROG-CODE-CHR (GSS2-INDEX)                        00179620
175000         SET WS-PC-GSS2-INDEX TO GSS2-INDEX                       00179630
SSDEC          MOVE 'Y' TO WS-PC-FOUND-SW.                              00179640
      *--  SRI  - HAS PROJECT                                           00179650
179700     IF GSS2-PP-PROG-CODE-CHR (GSS2-INDEX)                        00179700
179800         SET WS-PP-GSS2-INDEX TO GSS2-INDEX                       00179800
179900         MOVE 'Y' TO WS-PP-FOUND-SW.                              00179900
180000                                                                  00180000
180100     IF GSS2-PR-PROG-CODE-CHR (GSS2-INDEX)                        00180100
180200         SET WS-PR-GSS2-INDEX TO GSS2-INDEX                       00180200
180300         MOVE 'Y' TO WS-PR-FOUND-SW.                              00180300
180400                                                                  00180400
180500     IF GSS2-PS-PROG-CODE-CHR (GSS2-INDEX)                        00180500
180600         SET WS-PS-GSS2-INDEX TO GSS2-INDEX                       00180600
180700         MOVE 'Y' TO WS-PS-FOUND-SW.                              00180700
180800                                                                  00180800
180900     IF GSS2-PT-PROG-CODE-CHR (GSS2-INDEX)                        00180900
181000         SET WS-PT-GSS2-INDEX TO GSS2-INDEX                       00181000
181100         MOVE 'Y' TO WS-PT-FOUND-SW.                              00181100
181200                                                                  00181200
181300     IF GSS2-P1-PROG-CODE-CHR (GSS2-INDEX)                        00181300
181400         SET WS-P1-GSS2-INDEX TO GSS2-INDEX                       00181400
181500         MOVE 'Y' TO WS-P1-FOUND-SW.                              00181500
181600                                                                  00181600
181700     IF GSS2-RP-PROG-CODE-CHR (GSS2-INDEX)                        00181700
181800         SET WS-RP-GSS2-INDEX TO GSS2-INDEX                       00181800
181900         MOVE 'Y' TO WS-RP-FOUND-SW.                              00181900
182000                                                                  00182000
182100     IF GSS2-RS-PROG-CODE-CHR (GSS2-INDEX)                        00182100
182200         SET WS-RS-GSS2-INDEX TO GSS2-INDEX                       00182200
182300         MOVE 'Y' TO WS-RS-FOUND-SW.                              00182300
182400                                                                  00182400
182500     IF GSS2-SA-PROG-CODE-CHR (GSS2-INDEX)                        00182500
182600         SET WS-SA-GSS2-INDEX TO GSS2-INDEX                       00182600
182700         MOVE 'Y' TO WS-SA-FOUND-SW.                              00182700
182800                                                                  00182800
182900     IF GSS2-S1-PROG-CODE-CHR (GSS2-INDEX)                        00182900
183000         SET WS-S1-GSS2-INDEX TO GSS2-INDEX                       00183000
183100         MOVE 'Y' TO WS-S1-FOUND-SW.                              00183100
      *--  KIKI                                                         00183110
           IF GSS2-TC-PROG-CODE-CHR (GSS2-INDEX)                        00183130
               SET WS-TC-GSS2-INDEX TO GSS2-INDEX                       00183140
               MOVE 'Y' TO WS-TC-FOUND-SW.                              00183150
183200                                                                  00183200
183300 12000-EXIT.                                                      00183300
183400     EXIT.                                                        00183400
183500/                                                                 00183500
183600******************************************************************00183600
183700** PERFORM THE ACTUAL EDIT TESTS.  IF AN ERROR IS FOUND, PUT    **00183700
183800** ITS CODE IN THE RECORD, AND SKIP THE CORRESPONDING           **00183800
183900** 'VICE VERSA' EDIT.                                           **00183900
184000******************************************************************00184000
184100 20000-EDITS.                                                     00184100
184200                                                                  00184200
184300     IF GCG-ADDL-TRNSPLNT-COVRG-IND = '01' OR '02' OR '03' OR     00184300
184400                                      '04' OR '05' OR '06' OR     00184400
184500                                      '07'                        00184500
184600         IF AT-ENTRY-FOUND                                        00184600
184700             SET GSS2-INDEX TO WS-AT-GSS2-INDEX                   00184700
184800             IF GSS2-AT-PROG-IND (GSS2-INDEX) NOT = ZERO          00184800
184900                 NEXT SENTENCE                                    00184900
185000             ELSE                                                 00185000
185100                 MOVE 'G01' TO WS-CURRENT-ERROR                   00185100
185200                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00185200
185300         ELSE                                                     00185300
185400             MOVE 'G01' TO WS-CURRENT-ERROR                       00185400
185500             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00185500
185600     ELSE                                                         00185600
185700     IF GCG-ADDL-TRNSPLNT-COVRG-IND = '08'                        00185700
185800         NEXT SENTENCE                                            00185800
185900     ELSE                                                         00185900
186000     IF AT-ENTRY-FOUND                                            00186000
186100         MOVE 'G01' TO WS-CURRENT-ERROR                           00186100
186200         PERFORM 90000-POST-ERROR THRU 90000-EXIT.                00186200
186300*    IF GCG-ADDL-TRNSPLNT-COVRG-IND = ZERO                        00186300
186400*        IF AT-ENTRY-FOUND                                        00186400
186500*            SET GSS2-INDEX TO WS-AT-GSS2-INDEX                   00186500
186600*            IF GSS2-AT-PROG-IND (GSS2-INDEX) = ZERO              00186600
186700*                NEXT SENTENCE                                    00186700
186800*            ELSE                                                 00186800
186900*                MOVE 'G01' TO WS-CURRENT-ERROR                   00186900
187000*                PERFORM 90000-POST-ERROR THRU 90000-EXIT         00187000
187100*        ELSE                                                     00187100
187200*            NEXT SENTENCE                                        00187200
187300*    ELSE                                                         00187300
187400*        IF AT-ENTRY-FOUND                                        00187400
187500*            SET GSS2-INDEX TO WS-AT-GSS2-INDEX                   00187500
187600*            IF GSS2-AT-PROG-IND (GSS2-INDEX) = ZERO              00187600
187700*                MOVE 'G01' TO WS-CURRENT-ERROR                   00187700
187800*                PERFORM 90000-POST-ERROR THRU 90000-EXIT         00187800
187900*            ELSE                                                 00187900
188000*                NEXT SENTENCE                                    00188000
188100*        ELSE                                                     00188100
188200*            MOVE 'G01' TO WS-CURRENT-ERROR                       00188200
188300*            PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00188300
188400                                                                  00188400
188500                                                                  00188500
188600     IF GCG-BAE-INDICATOR NOT = ZERO AND SPACES                   00188600
188700         IF BA-ENTRY-FOUND                                        00188700
188800             SET GSS2-INDEX TO WS-BA-GSS2-INDEX                   00188800
188900             IF (GSS2-BA-BC-IND (GSS2-INDEX) NOT = ZERO)          00188900
189000                     OR (GSS2-BA-BS-IND (GSS2-INDEX) NOT = ZERO)  00189000
189100                     OR (GSS2-BA-MM-IND (GSS2-INDEX) NOT = ZERO)  00189100
189200                 NEXT SENTENCE                                    00189200
189300             ELSE                                                 00189300
189400                 MOVE 'GA4' TO WS-CURRENT-ERROR                   00189400
189500                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00189500
189600         ELSE                                                     00189600
189700             MOVE 'GA4' TO WS-CURRENT-ERROR                       00189700
189800             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00189800
189900     ELSE                                                         00189900
190000         IF BA-ENTRY-FOUND                                        00190000
190100             MOVE 'GA4' TO WS-CURRENT-ERROR                       00190100
190200             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00190200
                                                                        00190300
AT0718     EVALUATE  TRUE                                               00190310
AT0718        WHEN BD-ENTRY-FOUND                                       00190320
AT0718         IF GCG-PROVIDR-OF-EXCELLNCE-IND  =  ZEROS  OR  SPACES    00190321
AT0718            MOVE 'GA4'  TO  WS-CURRENT-ERROR                      00190322
AT0718            PERFORM 90000-POST-ERROR  THRU  90000-EXIT            00190323
AT0718         ELSE                                                     00190324
AT0718            PERFORM 20080-BLUE-DISTINCTION-EDITS  THRU  20080-EXIT00190325
AT0718         END-IF                                                   00190326
                                                                        00190327
AT0718        WHEN OTHER                                                00190328
AT0718         IF GCG-PROVIDR-OF-EXCELLNCE-IND  NOT =  ZEROS  AND SPACES00190329
AT0718            MOVE 'GA4'  TO  WS-CURRENT-ERROR                      00190331
AT0718            PERFORM 90000-POST-ERROR  THRU  90000-EXIT            00190332
AT0718         END-IF                                                   00190333
                                                                        00190334
AT0718     END-EVALUATE.                                                00190350
                                                                        00190400
      *    KIKI                                                         00190402
           EVALUATE  TRUE                                               00190420
              WHEN TC-ENTRY-FOUND                                       00190430
               IF GCG-TOTAL-CARE-IND      =  ZEROS  OR  SPACES          00190440
                  MOVE 'GA4'  TO  WS-CURRENT-ERROR                      00190450
                  PERFORM 90000-POST-ERROR        THRU  90000-EXIT      00190460
               ELSE                                                     00190470
                  PERFORM 20090-TOTAL-CARE-EDITS  THRU  20090-EXIT      00190480
               END-IF                                                   00190490
                                                                        00190491
              WHEN OTHER                                                00190492
               IF GCG-TOTAL-CARE-IND  NOT =  ZEROS  AND  SPACES         00190494
                  MOVE 'GA4'  TO  WS-CURRENT-ERROR                      00190495
                  PERFORM 90000-POST-ERROR        THRU  90000-EXIT      00190496
               END-IF                                                   00190497
                                                                        00190498
           END-EVALUATE.                                                00190499
                                                                        00190500
      *--  TROY - ERS PROJECT                                           00190501
           EVALUATE  TRUE                                               00190502
              WHEN CE-ENTRY-FOUND                                       00190503
               IF GCG-CENTER-OF-EXCELLNCE-IND =  ZEROS  OR  SPACES      00190504
                  MOVE 'GA4'  TO  WS-CURRENT-ERROR                      00190505
                  PERFORM 90000-POST-ERROR        THRU  90000-EXIT      00190506
               ELSE                                                     00190507
                  PERFORM 20100-CE-EDITS  THRU  20100-EXIT              00190508
               END-IF                                                   00190509
                                                                        00190510
              WHEN OTHER                                                00190511
               IF GCG-CENTER-OF-EXCELLNCE-IND NOT =  ZEROS  AND  SPACES 00190512
                  MOVE 'GA4'  TO  WS-CURRENT-ERROR                      00190513
                  PERFORM 90000-POST-ERROR        THRU  90000-EXIT      00190514
               END-IF                                                   00190515
                                                                        00190516
           END-EVALUATE.                                                00190517
      *--  TROY - ERS PROJECT                                           00190518
                                                                        00190519
190500     IF GCG-CBL-PARTICIPATION-IND  NOT =  ZEROES AND SPACES       00190540
190600         IF CB-ENTRY-FOUND                                        00190600
190700            SET GSS2-INDEX TO WS-CB-GSS2-INDEX                    00190700
190800            IF (GSS2-CB-BC-IND  (GSS2-INDEX)  NOT =  ZEROES)      00190800
190900             OR (GSS2-CB-BS-IND (GSS2-INDEX)  NOT =  ZEROES)      00190900
191000             OR (GSS2-CB-MM-IND (GSS2-INDEX)  NOT =  ZEROES)      00191000
191100                 NEXT SENTENCE                                    00191100
191200            ELSE                                                  00191200
191300                 MOVE 'GAR' TO WS-CURRENT-ERROR                   00191300
191400                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00191400
191500         ELSE                                                     00191500
191600             MOVE 'GAR' TO WS-CURRENT-ERROR                       00191600
191700             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00191700
191800     ELSE                                                         00191800
191900         IF CB-ENTRY-FOUND                                        00191900
192000             MOVE 'GAR' TO WS-CURRENT-ERROR                       00192000
192100             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00192100
192200                                                                  00192200
192300                                                                  00192300
      *--  SRI  - PSMNR PROJECT                                         00192310
           EVALUATE  TRUE                                               00192320
              WHEN CU-ENTRY-FOUND                                       00192330
               IF GCG-UTILIZE-MANAGE-IND =  ZEROS  OR  SPACES           00192340
                  MOVE 'GA4'  TO  WS-CURRENT-ERROR                      00192350
                  PERFORM 90000-POST-ERROR        THRU  90000-EXIT      00192360
               ELSE                                                     00192370
                  PERFORM 20130-CU-EDITS  THRU  20130-EXIT              00192380
               END-IF                                                   00192390
                                                                        00192391
              WHEN OTHER                                                00192392
               IF GCG-UTILIZE-MANAGE-IND NOT =  ZEROS  AND  SPACES      00192393
                  MOVE 'GA4'  TO  WS-CURRENT-ERROR                      00192394
                  PERFORM 90000-POST-ERROR        THRU  90000-EXIT      00192395
               END-IF                                                   00192396
                                                                        00192397
           END-EVALUATE.                                                00192398
      *--  SRI  - PSMNR PROJECT                                         00192399
      *--  SRI  - HAS PROJECT                                           00192400
           EVALUATE  TRUE                                               00192401
              WHEN EC-ENTRY-FOUND                                       00192402
               IF GCG-UTILIZE-MANAGE-IND =  ZEROS  OR  SPACES           00192403
                  MOVE 'GA4'  TO  WS-CURRENT-ERROR                      00192404
                  PERFORM 90000-POST-ERROR        THRU  90000-EXIT      00192405
               ELSE                                                     00192406
SSDEC             PERFORM 20110-EC-EDITS  THRU  20110-EXIT              00192407
               END-IF                                                   00192408
                                                                        00192409
              WHEN OTHER                                                00192410
               IF GCG-UTILIZE-MANAGE-IND NOT =  ZEROS  AND  SPACES      00192411
                  MOVE 'GA4'  TO  WS-CURRENT-ERROR                      00192412
                  PERFORM 90000-POST-ERROR        THRU  90000-EXIT      00192413
               END-IF                                                   00192414
                                                                        00192415
           END-EVALUATE.                                                00192416
      *--  SRI  - HAS PROJECT                                           00192417
192400     IF GCG-FRI-SAT-ADM-IND = '01' OR '02' OR '03' OR '04'        00192420
192500                           OR '05' OR '06' OR '07'                00192500
192600         IF FS-ENTRY-FOUND                                        00192600
192700             SET GSS2-INDEX TO WS-FS-GSS2-INDEX                   00192700
192800             IF (GSS2-FS-BC-IND (GSS2-INDEX) NOT = ZERO)          00192800
192900                     OR (GSS2-FS-BS-IND (GSS2-INDEX) NOT = ZERO)  00192900
193000                     OR (GSS2-FS-MM-IND (GSS2-INDEX) NOT = ZERO)  00193000
193100                 NEXT SENTENCE                                    00193100
193200             ELSE                                                 00193200
193300                 MOVE 'G02' TO WS-CURRENT-ERROR                   00193300
193400                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00193400
193500         ELSE                                                     00193500
193600             MOVE 'G02' TO WS-CURRENT-ERROR                       00193600
193700             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00193700
193800     ELSE                                                         00193800
193900         IF FS-ENTRY-FOUND                                        00193900
194000             MOVE 'G02' TO WS-CURRENT-ERROR                       00194000
194100             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00194100
194400                                                                  00194400
194500     IF GCG-HOSPICE-IND = '01' OR '02' OR '03' OR '04' OR         00194500
194600                          '05' OR '06' OR '07'                    00194600
194700         IF HO-ENTRY-FOUND                                        00194700
194800             SET GSS2-INDEX TO WS-HO-GSS2-INDEX                   00194800
194900             IF (GSS2-HO-BC-IND (GSS2-INDEX) NOT = ZERO)          00194900
195000                     OR (GSS2-HO-BS-IND (GSS2-INDEX) NOT = ZERO)  00195000
195100                     OR (GSS2-HO-MM-IND (GSS2-INDEX) NOT = ZERO)  00195100
195200                 NEXT SENTENCE                                    00195200
195300             ELSE                                                 00195300
195400                 MOVE 'G04' TO WS-CURRENT-ERROR                   00195400
195500                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00195500
195600         ELSE                                                     00195600
195700             MOVE 'G04' TO WS-CURRENT-ERROR                       00195700
195800             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00195800
195900     ELSE                                                         00195900
196000         IF HO-ENTRY-FOUND                                        00196000
196100             MOVE 'G04' TO WS-CURRENT-ERROR                       00196100
196200             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00196200
196300                                                                  00196300
196400                                                                  00196400
196500                                                                  00196500
196600     IF GCG-INCENTIVE-OB-IND = '01' OR '02' OR '03' OR '04' OR    00196600
196700                               '05' OR '06' OR '07'               00196700
196800         IF IO-ENTRY-FOUND                                        00196800
196900             SET GSS2-INDEX TO WS-IO-GSS2-INDEX                   00196900
197000             IF (GSS2-IO-BC-IND (GSS2-INDEX) NOT = ZERO)          00197000
197100                     OR (GSS2-IO-BS-IND (GSS2-INDEX) NOT = ZERO)  00197100
197200                     OR (GSS2-IO-MM-IND (GSS2-INDEX) NOT = ZERO)  00197200
197300                 NEXT SENTENCE                                    00197300
197400             ELSE                                                 00197400
197500                 MOVE 'G03' TO WS-CURRENT-ERROR                   00197500
197600                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00197600
197700         ELSE                                                     00197700
197800             MOVE 'G03' TO WS-CURRENT-ERROR                       00197800
197900             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00197900
198000     ELSE                                                         00198000
198100         IF IO-ENTRY-FOUND                                        00198100
198200             MOVE 'G03' TO WS-CURRENT-ERROR                       00198200
198300             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00198300
198400                                                                  00198400
198500                                                                  00198500
198600                                                                  00198600
198700     IF GCG-MAND-ADDL-SURG-OPN-IND = '01' OR '02' OR '03' OR      00198700
198800                                     '04' OR '05' OR '06' OR      00198800
198900                                     '07'                         00198900
199000         IF MA-ENTRY-FOUND                                        00199000
199100             SET GSS2-INDEX TO WS-MA-GSS2-INDEX                   00199100
199200             IF (GSS2-MA-BC-IND (GSS2-INDEX) NOT = ZERO)          00199200
199300                     OR (GSS2-MA-BS-IND (GSS2-INDEX) NOT = ZERO)  00199300
199400                     OR (GSS2-MA-MM-IND (GSS2-INDEX) NOT = ZERO)  00199400
199500                 NEXT SENTENCE                                    00199500
199600             ELSE                                                 00199600
199700                 MOVE 'G05' TO WS-CURRENT-ERROR                   00199700
199800                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00199800
199900         ELSE                                                     00199900
200000             MOVE 'G05' TO WS-CURRENT-ERROR                       00200000
200100             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00200100
200200     ELSE                                                         00200200
200300         IF MA-ENTRY-FOUND                                        00200300
200400             MOVE 'G05' TO WS-CURRENT-ERROR                       00200400
200500             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00200500
200600                                                                  00200600
200700                                                                  00200700
200800                                                                  00200800
200900     IF GCG-MED-NECESSITY-HCNR-IPS-IN NOT = ZERO AND SPACES       00200900
201000         IF MN-ENTRY-FOUND                                        00201000
201100             SET GSS2-INDEX TO WS-MN-GSS2-INDEX                   00201100
201200             IF (GSS2-MN-BC-IND (GSS2-INDEX) NOT = ZERO)          00201200
201300                     OR (GSS2-MN-BS-IND (GSS2-INDEX) NOT = ZERO)  00201300
201400                     OR (GSS2-MN-MM-IND (GSS2-INDEX) NOT = ZERO)  00201400
201500                 NEXT SENTENCE                                    00201500
201600             ELSE                                                 00201600
201700                 MOVE 'G06' TO WS-CURRENT-ERROR                   00201700
201800                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00201800
201900         ELSE                                                     00201900
202000             MOVE 'G06' TO WS-CURRENT-ERROR                       00202000
202100             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00202100
202200     ELSE                                                         00202200
202300         IF MN-ENTRY-FOUND                                        00202300
202400             MOVE 'G06' TO WS-CURRENT-ERROR                       00202400
202500             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00202500
202600                                                                  00202600
202700                                                                  00202700
202800     IF GCG-PAN-PARTICIPATION-IND  NOT =  ZEROES AND SPACES       00202800
202900         IF PA-ENTRY-FOUND                                        00202900
203000            SET GSS2-INDEX TO WS-PA-GSS2-INDEX                    00203000
203100            IF (GSS2-PA-BC-IND  (GSS2-INDEX)  NOT =  ZEROES)      00203100
203200             OR (GSS2-PA-BS-IND (GSS2-INDEX)  NOT =  ZEROES)      00203200
203300             OR (GSS2-PA-MM-IND (GSS2-INDEX)  NOT =  ZEROES)      00203300
203400                 NEXT SENTENCE                                    00203400
203500            ELSE                                                  00203500
203600                 MOVE 'GAS' TO WS-CURRENT-ERROR                   00203600
203700                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00203700
203800         ELSE                                                     00203800
203900             MOVE 'GAS' TO WS-CURRENT-ERROR                       00203900
204000             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00204000
204100     ELSE                                                         00204100
204200         IF PA-ENTRY-FOUND                                        00204200
204300             MOVE 'GAS' TO WS-CURRENT-ERROR                       00204300
204400             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00204400
204500                                                                  00204500
204600                                                                  00204600
204700                                                                  00204700
      *--  SRI  - HAS PROJECT                                           00204710
           EVALUATE  TRUE                                               00204720
              WHEN PC-ENTRY-FOUND                                       00204730
               IF GCG-UTILIZE-MANAGE-IND =  ZEROS  OR  SPACES           00204740
                  MOVE 'GA4'  TO  WS-CURRENT-ERROR                      00204750
                  PERFORM 90000-POST-ERROR        THRU  90000-EXIT      00204760
               ELSE                                                     00204770
SSDEC             PERFORM 20120-PC-EDITS  THRU  20120-EXIT              00204780
               END-IF                                                   00204790
                                                                        00204791
              WHEN OTHER                                                00204792
               IF GCG-UTILIZE-MANAGE-IND NOT =  ZEROS  AND  SPACES      00204793
                  MOVE 'GA4'  TO  WS-CURRENT-ERROR                      00204794
                  PERFORM 90000-POST-ERROR        THRU  90000-EXIT      00204795
               END-IF                                                   00204796
                                                                        00204797
           END-EVALUATE.                                                00204798
      *--  SRI  - ERS PROJECT                                           00204799
204800     IF GCG-PARTICIPAT-PROV-OPTION NOT = ZERO AND SPACES          00204800
204900         IF PP-ENTRY-FOUND                                        00204900
205000             SET GSS2-INDEX TO WS-PP-GSS2-INDEX                   00205000
205100             IF (GSS2-PP-BC-IND (GSS2-INDEX) NOT = ZERO)          00205100
205200                     OR (GSS2-PP-BS-IND (GSS2-INDEX) NOT = ZERO)  00205200
205300                     OR (GSS2-PP-MM-IND (GSS2-INDEX) NOT = ZERO)  00205300
205400                 NEXT SENTENCE                                    00205400
205500             ELSE                                                 00205500
205600                 MOVE 'G07' TO WS-CURRENT-ERROR                   00205600
205700                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00205700
205800         ELSE                                                     00205800
205900             MOVE 'G07' TO WS-CURRENT-ERROR                       00205900
206000             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00206000
206100     ELSE                                                         00206100
206200         IF PP-ENTRY-FOUND                                        00206200
206300             MOVE 'G07' TO WS-CURRENT-ERROR                       00206300
206400             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00206400
206500                                                                  00206500
206600     IF GCG-CPO-PARTICIPATION-IND NOT = ZERO AND SPACES           00206600
206700         IF CP-ENTRY-FOUND                                        00206700
206800             SET GSS2-INDEX TO WS-CP-GSS2-INDEX                   00206800
206900             IF (GSS2-CP-BC-IND (GSS2-INDEX) NOT = ZERO)          00206900
207000                     OR (GSS2-CP-BS-IND (GSS2-INDEX) NOT = ZERO)  00207000
207100                     OR (GSS2-CP-MM-IND (GSS2-INDEX) NOT = ZERO)  00207100
207200                 NEXT SENTENCE                                    00207200
207300             ELSE                                                 00207300
207400                 MOVE 'GAO' TO WS-CURRENT-ERROR                   00207400
207500                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00207500
207600         ELSE                                                     00207600
207700             MOVE 'GAO' TO WS-CURRENT-ERROR                       00207700
207800             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00207800
207900     ELSE                                                         00207900
208000         IF CP-ENTRY-FOUND                                        00208000
208100             MOVE 'GAO' TO WS-CURRENT-ERROR                       00208100
208200             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00208200
208300                                                                  00208300
208400     IF GCG-RPO-INDICATOR NOT = ZERO AND SPACES                   00208400
208500         IF RP-ENTRY-FOUND                                        00208500
208600             SET GSS2-INDEX TO WS-RP-GSS2-INDEX                   00208600
208700             IF (GSS2-RP-BC-IND (GSS2-INDEX) NOT = ZERO)          00208700
208800                     OR (GSS2-RP-BS-IND (GSS2-INDEX) NOT = ZERO)  00208800
208900                     OR (GSS2-RP-MM-IND (GSS2-INDEX) NOT = ZERO)  00208900
209000                 NEXT SENTENCE                                    00209000
209100             ELSE                                                 00209100
209200                 MOVE 'GA4' TO WS-CURRENT-ERROR                   00209200
209300                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00209300
209400         ELSE                                                     00209400
209500             MOVE 'GA4' TO WS-CURRENT-ERROR                       00209500
209600             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00209600
209700     ELSE                                                         00209700
209800         IF RP-ENTRY-FOUND                                        00209800
209900             MOVE 'GA4' TO WS-CURRENT-ERROR                       00209900
210000             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00210000
210100                                                                  00210100
210200                                                                  00210200
210300                                                                  00210300
210400     IF GCG-PARTICIPAT-PROV-OPTION = '08' OR '09' OR '0A'         00210400
210500                                      OR '0B' OR '0C' OR          00210500
210600                                     '0D' OR '0E'                 00210600
210700         IF GPPO-REC-FOUND                                        00210700
210800             NEXT SENTENCE                                        00210800
210900         ELSE                                                     00210900
211000             MOVE 'G08' TO WS-CURRENT-ERROR                       00211000
211100             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00211100
211200     ELSE                                                         00211200
211300         IF GPPO-REC-FOUND                                        00211300
211400           OR                                                     00211400
211500            GPPO-REC-EMPTY                                        00211500
211600             MOVE 'G08' TO WS-CURRENT-ERROR                       00211600
211700             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00211700
211800                                                                  00211800
211900                                                                  00211900
212000     IF GCG-MAND-OP-SURG-PROG-IND = '01' OR '02' OR '03' OR       00212000
212100                                    '04' OR '05' OR '06' OR       00212100
212200                                    '07'                          00212200
212300         IF MO-ENTRY-FOUND                                        00212300
212400             SET GSS2-INDEX TO WS-MO-GSS2-INDEX                   00212400
212500             IF (GSS2-MO-BC-IND (GSS2-INDEX) NOT = ZERO)          00212500
212600                     OR (GSS2-MO-BS-IND (GSS2-INDEX) NOT = ZERO)  00212600
212700                     OR (GSS2-MO-MM-IND (GSS2-INDEX) NOT = ZERO)  00212700
212800                 NEXT SENTENCE                                    00212800
212900             ELSE                                                 00212900
213000                 MOVE 'G09' TO WS-CURRENT-ERROR                   00213000
213100                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00213100
213200         ELSE                                                     00213200
213300             MOVE 'G09' TO WS-CURRENT-ERROR                       00213300
213400             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00213400
213500     ELSE                                                         00213500
213600         IF MO-ENTRY-FOUND                                        00213600
213700             MOVE 'G09' TO WS-CURRENT-ERROR                       00213700
213800             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00213800
213900                                                                  00213900
214000                                                                  00214000
214100                                                                  00214100
214200     IF GCG-MED-SERV-ADV-PROG-IND =                               00214200
214300        '01' OR '02' OR '03' OR '04' OR '05' OR '06' OR '07'      00214300
214400         IF MS-ENTRY-FOUND                                        00214400
214500             SET GSS2-INDEX TO WS-MS-GSS2-INDEX                   00214500
214600             IF (GSS2-MS-BC-IND (GSS2-INDEX) NOT = ZERO)          00214600
214700                     OR (GSS2-MS-BS-IND (GSS2-INDEX) NOT = ZERO)  00214700
214800                     OR (GSS2-MS-MM-IND (GSS2-INDEX) NOT = ZERO)  00214800
214900                 NEXT SENTENCE                                    00214900
215000             ELSE                                                 00215000
215100                 MOVE 'G10' TO WS-CURRENT-ERROR                   00215100
215200                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00215200
215300         ELSE                                                     00215300
215400             MOVE 'G10' TO WS-CURRENT-ERROR                       00215400
215500             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00215500
215600     ELSE                                                         00215600
215700         IF MS-ENTRY-FOUND                                        00215700
215800             MOVE 'G10' TO WS-CURRENT-ERROR                       00215800
215900             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00215900
216000                                                                  00216000
216100                                                                  00216100
216200                                                                  00216200
216300     IF GCG-PRE-ADM-REVIEW-IND =                                  00216300
216400        '01' OR '02' OR '03' OR '04' OR '05' OR '06' OR '07'      00216400
216500         IF PR-ENTRY-FOUND                                        00216500
216600             SET GSS2-INDEX TO WS-PR-GSS2-INDEX                   00216600
216700             IF (GSS2-PR-BC-IND (GSS2-INDEX) NOT = ZERO)          00216700
216800                     OR (GSS2-PR-BS-IND (GSS2-INDEX) NOT = ZERO)  00216800
216900                     OR (GSS2-PR-MM-IND (GSS2-INDEX) NOT = ZERO)  00216900
217000                 NEXT SENTENCE                                    00217000
217100             ELSE                                                 00217100
217200                 MOVE 'G11' TO WS-CURRENT-ERROR                   00217200
217300                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00217300
217400         ELSE                                                     00217400
217500             MOVE 'G11' TO WS-CURRENT-ERROR                       00217500
217600             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00217600
217700     ELSE                                                         00217700
217800         IF PR-ENTRY-FOUND                                        00217800
217900             MOVE 'G11' TO WS-CURRENT-ERROR                       00217900
218000             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00218000
218100                                                                  00218100
218200                                                                  00218200
218300                                                                  00218300
218400     IF GCG-PRE-ADM-TESTING-PROGRAM    =                          00218400
218500        '01' OR '02' OR '03' OR '04' OR '05' OR '06' OR '07'      00218500
218600         IF PT-ENTRY-FOUND                                        00218600
218700             SET GSS2-INDEX TO WS-PT-GSS2-INDEX                   00218700
218800             IF (GSS2-PT-BC-IND (GSS2-INDEX) NOT = ZERO)          00218800
218900                     OR (GSS2-PT-BS-IND (GSS2-INDEX) NOT = ZERO)  00218900
219000                     OR (GSS2-PT-MM-IND (GSS2-INDEX) NOT = ZERO)  00219000
219100                 NEXT SENTENCE                                    00219100
219200             ELSE                                                 00219200
219300                 MOVE 'G12' TO WS-CURRENT-ERROR                   00219300
219400                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00219400
219500         ELSE                                                     00219500
219600             MOVE 'G12' TO WS-CURRENT-ERROR                       00219600
219700             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00219700
219800     ELSE                                                         00219800
219900         IF PT-ENTRY-FOUND                                        00219900
220000             MOVE 'G12' TO WS-CURRENT-ERROR                       00220000
220100             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00220100
220200                                                                  00220200
220300                                                                  00220300
220400**D11530  (RS)  REIMBURSEMENT/SUBROGATION      FRY 7/13/95        00220400
220500***                                                               00220500
220600     IF GCG-REIMBUR-SUBROG-IND   =   '01'                         00220600
220700        IF RS-ENTRY-FOUND                                         00220700
220800           SET GSS2-INDEX TO WS-RS-GSS2-INDEX                     00220800
220900           IF GSS2-RS-RESPONSIBILITY-IND (GSS2-INDEX) = ZERO      00220900
221000              MOVE 'G13' TO WS-CURRENT-ERROR                      00221000
221100              PERFORM 90000-POST-ERROR THRU 90000-EXIT            00221100
221200           ELSE                                                   00221200
221300              NEXT SENTENCE                                       00221300
221400        ELSE                                                      00221400
221500           MOVE 'G13' TO WS-CURRENT-ERROR                         00221500
221600           PERFORM 90000-POST-ERROR THRU 90000-EXIT               00221600
221700     ELSE                                                         00221700
221800        IF RS-ENTRY-FOUND                                         00221800
221900           MOVE 'G13' TO WS-CURRENT-ERROR                         00221900
222000           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00222000
222100                                                                  00222100
222200                                                                  00222200
222300                                                                  00222300
222400     IF GCG-MONDAY-DISCHARGE-IND =                                00222400
222500         '01' OR '02' OR '03' OR '04' OR '05' OR '06' OR '07'     00222500
222600         IF MD-ENTRY-FOUND                                        00222600
222700             SET GSS2-INDEX TO WS-MD-GSS2-INDEX                   00222700
222800             IF (GSS2-MD-BC-IND (GSS2-INDEX) NOT = ZERO)          00222800
222900                     OR (GSS2-MD-BS-IND (GSS2-INDEX) NOT = ZERO)  00222900
223000                     OR (GSS2-MD-MM-IND (GSS2-INDEX) NOT = ZERO)  00223000
223100                 NEXT SENTENCE                                    00223100
223200             ELSE                                                 00223200
223300                 MOVE 'G15' TO WS-CURRENT-ERROR                   00223300
223400                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00223400
223500         ELSE                                                     00223500
223600             MOVE 'G15' TO WS-CURRENT-ERROR                       00223600
223700             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00223700
223800     ELSE                                                         00223800
223900         IF MD-ENTRY-FOUND                                        00223900
224000             MOVE 'G15' TO WS-CURRENT-ERROR                       00224000
224100             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00224100
224200                                                                  00224200
224300                                                                  00224300
224400** (EMH)  EXTENED MENTAL HEALTH                                   00224400
224500     IF GCG-SUBS-ABUSE-MENTAL-IND =                               00224500
224600        '01' OR '02' OR '03' OR '04' OR '05' OR '06' OR '07'      00224600
224700         IF SA-ENTRY-FOUND                                        00224700
224800             SET GSS2-INDEX TO WS-SA-GSS2-INDEX                   00224800
224900             IF (GSS2-SA-BC-IND (GSS2-INDEX) NOT = ZERO)          00224900
225000                     OR (GSS2-SA-BS-IND (GSS2-INDEX) NOT = ZERO)  00225000
225100                     OR (GSS2-SA-MM-IND (GSS2-INDEX) NOT = ZERO)  00225100
225200                 NEXT SENTENCE                                    00225200
225300             ELSE                                                 00225300
225400                 MOVE 'G67' TO WS-CURRENT-ERROR                   00225400
225500                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00225500
225600         ELSE                                                     00225600
225700             MOVE 'G67' TO WS-CURRENT-ERROR                       00225700
225800             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00225800
225900     ELSE                                                         00225900
226000         IF SA-ENTRY-FOUND                                        00226000
226100             MOVE 'G67' TO WS-CURRENT-ERROR                       00226100
226200             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00226200
226300                                                                  00226300
226400                                                                  00226400
226500                                                                  00226500
226600**D11836  (MHSC)  MENTAL HEALTH SUBSTANCE ABUSE CARE    FRY 6/1/9100226600
226700     IF GCG-NEW-MEN-SUB-ABUSE-IND   >   ZEROES                    00226700
226800        IF S1-ENTRY-FOUND                                         00226800
226900            SET GSS2-INDEX TO WS-S1-GSS2-INDEX                    00226900
227000            IF (GSS2-S1-BC-IND (GSS2-INDEX)      NOT =   ZEROES)  00227000
227100              OR  (GSS2-S1-BS-IND (GSS2-INDEX)   NOT =   ZEROES)  00227100
227200              OR  (GSS2-S1-MM-IND (GSS2-INDEX)   NOT =   ZEROES)  00227200
227300                NEXT SENTENCE                                     00227300
227400            ELSE                                                  00227400
227500                MOVE 'GA0' TO WS-CURRENT-ERROR                    00227500
227600                PERFORM 90000-POST-ERROR THRU 90000-EXIT          00227600
227700        ELSE                                                      00227700
227800            MOVE 'GA0' TO WS-CURRENT-ERROR                        00227800
227900            PERFORM 90000-POST-ERROR THRU 90000-EXIT              00227900
228000     ELSE                                                         00228000
228100         IF S1-ENTRY-FOUND                                        00228100
228200             MOVE 'GA0' TO WS-CURRENT-ERROR                       00228200
228300             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00228300
228400                                                                  00228400
228500                                                                  00228500
228600**D11836  (POS)  POINT OF SERVICE    FRY 6/1/91                   00228600
228700     IF GCG-NEW-POS-IND   >   ZEROES                              00228700
228800        IF P1-ENTRY-FOUND                                         00228800
228900            SET GSS2-INDEX TO WS-P1-GSS2-INDEX                    00228900
229000            IF (GSS2-P1-BC-IND (GSS2-INDEX)     NOT =  ZEROES)    00229000
229100              OR  (GSS2-P1-BS-IND (GSS2-INDEX)  NOT =  ZEROES)    00229100
229200              OR  (GSS2-P1-MM-IND (GSS2-INDEX)  NOT =  ZEROES)    00229200
229300                NEXT SENTENCE                                     00229300
229400            ELSE                                                  00229400
229500                MOVE 'GA1' TO WS-CURRENT-ERROR                    00229500
229600                PERFORM 90000-POST-ERROR THRU 90000-EXIT          00229600
229700        ELSE                                                      00229700
229800            MOVE 'GA1' TO WS-CURRENT-ERROR                        00229800
229900            PERFORM 90000-POST-ERROR THRU 90000-EXIT              00229900
230000     ELSE                                                         00230000
230100         IF P1-ENTRY-FOUND                                        00230100
230200             MOVE 'GA1' TO WS-CURRENT-ERROR                       00230200
230300             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00230300
230400                                                                  00230400
230500                                                                  00230500
230600**D-351   (HM) HMO MANAGED CARE      GSP 9/11/2000                00230600
230700     IF GCG-HMO-MC-INDICATOR  > ZEROES                            00230700
230800        IF HM-ENTRY-FOUND                                         00230800
230900            SET GSS2-INDEX TO WS-HM-GSS2-INDEX                    00230900
231000            IF (GSS2-HM-BC-IND (GSS2-INDEX)     NOT =  ZEROES)    00231000
231100              OR  (GSS2-HM-BS-IND (GSS2-INDEX)  NOT =  ZEROES)    00231100
231200              OR  (GSS2-HM-MM-IND (GSS2-INDEX)  NOT =  ZEROES)    00231200
231300                NEXT SENTENCE                                     00231300
231400            ELSE                                                  00231400
231500                MOVE 'GAT' TO WS-CURRENT-ERROR                    00231500
231600                PERFORM 90000-POST-ERROR THRU 90000-EXIT          00231600
231700        ELSE                                                      00231700
231800            MOVE 'GAT' TO WS-CURRENT-ERROR                        00231800
231900            PERFORM 90000-POST-ERROR THRU 90000-EXIT              00231900
232000     ELSE                                                         00232000
232100         IF HM-ENTRY-FOUND                                        00232100
232200             MOVE 'GAT' TO WS-CURRENT-ERROR                       00232200
232300             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00232300
232400                                                                  00232400
232500                                                                  00232500
232600     IF GHOR-REC-FOUND                                            00232600
232700         IF AT-ENTRY-FOUND                                        00232700
232800             SET GSS2-INDEX TO WS-AT-GSS2-INDEX                   00232800
232900             IF GSS2-AT-PROG-IND (GSS2-INDEX) = ZERO              00232900
233000                 MOVE 'G16' TO WS-CURRENT-ERROR                   00233000
233100                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00233100
233200             ELSE                                                 00233200
233300                 NEXT SENTENCE                                    00233300
233400         ELSE                                                     00233400
233500             MOVE 'G16' TO WS-CURRENT-ERROR                       00233500
233600             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00233600
233700     ELSE                                                         00233700
233800         IF AT-ENTRY-FOUND                                        00233800
233900             MOVE 'G16' TO WS-CURRENT-ERROR                       00233900
234000             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00234000
234100                                                                  00234100
234200                                                                  00234200
234300                                                                  00234300
234400     IF GHOB-REC-FOUND                                            00234400
234500         IF AT-ENTRY-FOUND                                        00234500
234600             SET GSS2-INDEX TO WS-AT-GSS2-INDEX                   00234600
234700             IF GSS2-AT-PROG-IND (GSS2-INDEX)    EQUAL   '03'     00234700
234800                 NEXT SENTENCE                                    00234800
234900             ELSE                                                 00234900
235000                 MOVE 'G17' TO WS-CURRENT-ERROR                   00235000
235100                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00235100
235200         ELSE                                                     00235200
235300             MOVE 'G17' TO WS-CURRENT-ERROR                       00235300
235400             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00235400
235500     ELSE                                                         00235500
235600         IF AT-ENTRY-FOUND                                        00235600
235700            SET GSS2-INDEX TO WS-AT-GSS2-INDEX                    00235700
235800            IF GSS2-AT-PROG-IND (GSS2-INDEX)    EQUAL    '03'     00235800
235900            MOVE 'G17' TO WS-CURRENT-ERROR                        00235900
236000            PERFORM 90000-POST-ERROR THRU 90000-EXIT.             00236000
236100                                                                  00236100
236200                                                                  00236200
236300                                                                  00236300
236400     IF AT-ENTRY-FOUND                                            00236400
236500         SET GSS2-INDEX TO WS-AT-GSS2-INDEX                       00236500
236600         IF GSS2-AT-PAYMENT-METHOD (GSS2-INDEX) = '2'             00236600
236700             IF GSS2-AT-BC-CALC-METHOD (GSS2-INDEX) NOT = ZERO    00236700
236800              AND '02'                                            00236800
236900                 MOVE 'G18' TO WS-CURRENT-ERROR                   00236900
237000                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00237000
237100             ELSE                                                 00237100
237200             IF GSS2-AT-BS-CALC-METHOD (GSS2-INDEX) NOT = ZERO    00237200
237300              AND '02'                                            00237300
237400                 MOVE 'G18' TO WS-CURRENT-ERROR                   00237400
237500                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00237500
237600             ELSE                                                 00237600
237700             IF   GSS2-AT-BC-CALC-METHOD (GSS2-INDEX) = ZERO      00237700
237800              AND GSS2-AT-BS-CALC-METHOD (GSS2-INDEX) = ZERO      00237800
237900                 MOVE 'G18' TO WS-CURRENT-ERROR                   00237900
238000                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00238000
238100             ELSE                                                 00238100
238200                 NEXT SENTENCE                                    00238200
238300         ELSE                                                     00238300
238400             IF   GSS2-AT-BC-CALC-METHOD (GSS2-INDEX) = '02'      00238400
238500               OR GSS2-AT-BS-CALC-METHOD (GSS2-INDEX) = '02'      00238500
238600                 MOVE 'G18' TO WS-CURRENT-ERROR                   00238600
238700                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00238700
238800                                                                  00238800
238900                                                                  00238900
239000                                                                  00239000
239100     IF AT-ENTRY-FOUND                                            00239100
239200         SET GSS2-INDEX TO WS-AT-GSS2-INDEX                       00239200
239300         IF GSS2-AT-PAYMENT-METHOD (GSS2-INDEX) = '4'             00239300
239400             IF GSS2-AT-BC-CALC-METHOD (GSS2-INDEX) NOT = ZERO    00239400
239500              AND '04'                                            00239500
239600                 MOVE 'G19' TO WS-CURRENT-ERROR                   00239600
239700                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00239700
239800             ELSE                                                 00239800
239900             IF GSS2-AT-BS-CALC-METHOD (GSS2-INDEX) NOT = ZERO    00239900
240000              AND '04'                                            00240000
240100                 MOVE 'G19' TO WS-CURRENT-ERROR                   00240100
240200                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00240200
240300             ELSE                                                 00240300
240400             IF   GSS2-AT-BC-CALC-METHOD (GSS2-INDEX) = ZERO      00240400
240500              AND GSS2-AT-BS-CALC-METHOD (GSS2-INDEX) = ZERO      00240500
240600                 MOVE 'G19' TO WS-CURRENT-ERROR                   00240600
240700                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00240700
240800             ELSE                                                 00240800
240900                 NEXT SENTENCE                                    00240900
241000         ELSE                                                     00241000
241100             IF   GSS2-AT-BC-CALC-METHOD (GSS2-INDEX) = '04'      00241100
241200               OR GSS2-AT-BS-CALC-METHOD (GSS2-INDEX) = '04'      00241200
241300                 MOVE 'G19' TO WS-CURRENT-ERROR                   00241300
241400                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00241400
241500                                                                  00241500
241600                                                                  00241600
241700                                                                  00241700
241800     IF AT-ENTRY-FOUND                                            00241800
241900        SET GSS2-INDEX TO WS-AT-GSS2-INDEX                        00241900
242000       IF GSS2-AT-PAYMENT-METHOD (GSS2-INDEX) = '3' OR '4'        00242000
242100         IF (GSS2-AT-BC-ALT-PRICING-METH (GSS2-INDEX)  >  ZERO)   00242100
242200           OR (GSS2-AT-BS-ALT-PRICING-METH (GSS2-INDEX) > ZERO)   00242200
242300              NEXT SENTENCE                                       00242300
242400         ELSE                                                     00242400
242500              MOVE 'G20' TO WS-CURRENT-ERROR                      00242500
242600              PERFORM 90000-POST-ERROR THRU 90000-EXIT            00242600
242700       ELSE                                                       00242700
242800         IF (GSS2-AT-BC-ALT-PRICING-METH (GSS2-INDEX)  >  ZERO)   00242800
242900           OR (GSS2-AT-BS-ALT-PRICING-METH (GSS2-INDEX) > ZERO)   00242900
243000               MOVE 'G20' TO WS-CURRENT-ERROR                     00243000
243100               PERFORM 90000-POST-ERROR THRU 90000-EXIT           00243100
243200         ELSE                                                     00243200
243300              NEXT SENTENCE                                       00243300
243400     ELSE                                                         00243400
243500         NEXT SENTENCE.                                           00243500
243600                                                                  00243600
243700                                                                  00243700
243800                                                                  00243800
243900     IF AT-ENTRY-FOUND                                            00243900
244000        SET GSS2-INDEX TO WS-AT-GSS2-INDEX                        00244000
244100       IF GSS2-AT-PAYMENT-METHOD (GSS2-INDEX) = '5' OR '6'        00244100
244200         IF GSS2-AT-BC-CALC-METHOD (GSS2-INDEX) NOT = ZERO        00244200
244300           AND '06'                                               00244300
244400               MOVE 'G21' TO WS-CURRENT-ERROR                     00244400
244500               PERFORM 90000-POST-ERROR THRU 90000-EXIT           00244500
244600         ELSE                                                     00244600
244700           IF GSS2-AT-BS-CALC-METHOD (GSS2-INDEX) NOT = ZERO      00244700
244800             AND '06'                                             00244800
244900                 MOVE 'G21' TO WS-CURRENT-ERROR                   00244900
245000                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00245000
245100           ELSE                                                   00245100
245200             IF (GSS2-AT-BC-CALC-METHOD (GSS2-INDEX) = ZERO)      00245200
245300               AND (GSS2-AT-BS-CALC-METHOD (GSS2-INDEX) = ZERO)   00245300
245400                    MOVE 'G21' TO WS-CURRENT-ERROR                00245400
245500                    PERFORM 90000-POST-ERROR THRU 90000-EXIT      00245500
245600             ELSE                                                 00245600
245700               NEXT SENTENCE                                      00245700
245800       ELSE                                                       00245800
245900         IF (GSS2-AT-BC-CALC-METHOD (GSS2-INDEX) = '06')          00245900
246000           OR (GSS2-AT-BS-CALC-METHOD (GSS2-INDEX) = '06')        00246000
246100               MOVE 'G21' TO WS-CURRENT-ERROR                     00246100
246200               PERFORM 90000-POST-ERROR THRU 90000-EXIT.          00246200
246300                                                                  00246300
246400                                                                  00246400
246500     IF GMCS-REC-FOUND                                            00246500
246600       IF P1-ENTRY-FOUND                                          00246600
246700          SET GSS2-INDEX TO WS-P1-GSS2-INDEX                      00246700
246800         IF (GSS2-P1-BC-PAYMENT-LEVEL-IND (GSS2-INDEX)            00246800
246900                                            >  ZERO)              00246900
247000           OR (GSS2-P1-BS-PAYMENT-LEVEL-IND (GSS2-INDEX)          00247000
247100                                            >  ZERO)              00247100
247200             OR (GSS2-P1-MM-PAYMENT-LEVEL-IND (GSS2-INDEX)        00247200
247300                                            >  ZERO)              00247300
247400                 NEXT SENTENCE                                    00247400
247500         ELSE                                                     00247500
247600             MOVE 'GA2' TO WS-CURRENT-ERROR                       00247600
247700             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00247700
247800         END-IF                                                   00247800
247900       ELSE                                                       00247900
248000       IF HM-ENTRY-FOUND                                          00248000
248100          SET GSS2-INDEX TO WS-HM-GSS2-INDEX                      00248100
248200         IF (GSS2-HM-BC-PAYMENT-LEVEL-IND (GSS2-INDEX)            00248200
248300                                            >  ZERO)              00248300
248400           OR (GSS2-HM-BS-PAYMENT-LEVEL-IND (GSS2-INDEX)          00248400
248500                                            >  ZERO)              00248500
248600             OR (GSS2-HM-MM-PAYMENT-LEVEL-IND (GSS2-INDEX)        00248600
248700                                            >  ZERO)              00248700
248800                 NEXT SENTENCE                                    00248800
248900         ELSE                                                     00248900
249000             MOVE 'GAU' TO WS-CURRENT-ERROR                       00249000
249100             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00249100
249200         END-IF                                                   00249200
249300       ELSE                                                       00249300
249400           MOVE 'GAV' TO WS-CURRENT-ERROR                         00249400
249500           PERFORM 90000-POST-ERROR THRU 90000-EXIT               00249500
249600       END-IF.                                                    00249600
249700                                                                  00249700
249800                                                                  00249800
249900     IF GMDN-REC-FOUND                                            00249900
250000       IF MN-ENTRY-FOUND                                          00250000
250100          SET GSS2-INDEX TO WS-MN-GSS2-INDEX                      00250100
250200         IF (GSS2-MN-BC-PAYMENT-IND (GSS2-INDEX)                  00250200
250300                                            = '2' OR '3' OR '4')  00250300
250400           OR (GSS2-MN-BS-PAYMENT-IND (GSS2-INDEX)                00250400
250500                                            = '2' OR '3' OR '4')  00250500
250600             OR (GSS2-MN-MM-PAYMENT-IND (GSS2-INDEX)              00250600
250700                                            = '2' OR '3' OR '4')  00250700
250800                 NEXT SENTENCE                                    00250800
250900         ELSE                                                     00250900
251000             MOVE 'G22' TO WS-CURRENT-ERROR                       00251000
251100             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00251100
251200       ELSE                                                       00251200
251300           MOVE 'G22' TO WS-CURRENT-ERROR                         00251300
251400           PERFORM 90000-POST-ERROR THRU 90000-EXIT               00251400
251500     ELSE                                                         00251500
251600         IF MN-ENTRY-FOUND                                        00251600
251700             SET GSS2-INDEX TO WS-MN-GSS2-INDEX                   00251700
251800             IF (GSS2-MN-BC-PAYMENT-IND (GSS2-INDEX)              00251800
251900                                            = '2' OR '3' OR '4')  00251900
252000               OR (GSS2-MN-BS-PAYMENT-IND (GSS2-INDEX)            00252000
252100                                            = '2' OR '3' OR '4')  00252100
252200                 OR (GSS2-MN-MM-PAYMENT-IND (GSS2-INDEX)          00252200
252300                                            = '2' OR '3' OR '4')  00252300
252400                     MOVE 'G22' TO WS-CURRENT-ERROR               00252400
252500                     PERFORM 90000-POST-ERROR THRU 90000-EXIT     00252500
252600             ELSE                                                 00252600
252700                 NEXT SENTENCE                                    00252700
252800         ELSE                                                     00252800
252900             NEXT SENTENCE.                                       00252900
253000                                                                  00253000
253100     IF GMCG-REC-FOUND                                            00253100
253200        IF PS-ENTRY-FOUND                                         00253200
253300           SET GSS2-INDEX TO WS-PS-GSS2-INDEX                     00253300
253400           IF GSS2-PS-BS-IND (GSS2-INDEX)     >  '00' OR '01'     00253400
253500              NEXT SENTENCE                                       00253500
253600           ELSE                                                   00253600
253700              MOVE 'G91' TO WS-CURRENT-ERROR                      00253700
253800              PERFORM 90000-POST-ERROR THRU 90000-EXIT            00253800
253900        ELSE                                                      00253900
254000           MOVE 'G91' TO WS-CURRENT-ERROR                         00254000
254100           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00254100
254200                                                                  00254200
254300                                                                  00254300
254400*--- IF THE MCN (POS) OCCURS ENTRY IN THE #GCCP RECORD HAS AT     00254400
254500*--- LEAST ONE OF THE FOLLOWING FIELDS CODED, THEN PASS THE EDIT  00254500
254600*---     1) MCN (POS) BLUE CROSS INDICATOR                        00254600
254700*---     2) MCN (POS) BLUE SHIELD INDICATOR                       00254700
254800*---     3) MCN (POS) MAJOR MEDICAL INDICATOR                     00254800
254900*--- IF THE MCN (POS) OCCURS ENTRY IS NOT FOUND IN THE #GCCP      00254900
255000*--- RECORD, IT IS AN ERROR.                                      00255000
255100                                                                  00255100
255200     IF GMCD-REC-FOUND                                            00255200
255300        IF PS-ENTRY-FOUND                                         00255300
255400           SET GSS2-INDEX TO WS-PS-GSS2-INDEX                     00255400
255500           IF (GSS2-PS-BC-IND (GSS2-INDEX) NOT = '00')            00255500
255600           OR (GSS2-PS-BS-IND (GSS2-INDEX) NOT = '00')            00255600
255700           OR (GSS2-PS-MM-IND (GSS2-INDEX) NOT = '00')            00255700
255800              NEXT SENTENCE                                       00255800
255900           ELSE                                                   00255900
256000              MOVE 'G92' TO WS-CURRENT-ERROR                      00256000
256100              PERFORM 90000-POST-ERROR THRU 90000-EXIT            00256100
256200        ELSE                                                      00256200
256300           MOVE 'G94' TO WS-CURRENT-ERROR                         00256300
256400           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00256400
256500                                                                  00256500
256600     IF GMCR-REC-FOUND                                            00256600
256700        IF PS-ENTRY-FOUND                                         00256700
256800           SET GSS2-INDEX TO WS-PS-GSS2-INDEX                     00256800
256900           IF (GSS2-PS-BC-IND (GSS2-INDEX) NOT = '00')            00256900
257000           OR (GSS2-PS-BS-IND (GSS2-INDEX) NOT = '00')            00257000
257100           OR (GSS2-PS-MM-IND (GSS2-INDEX) NOT = '00')            00257100
257200              NEXT SENTENCE                                       00257200
257300           ELSE                                                   00257300
257400              MOVE 'G93' TO WS-CURRENT-ERROR                      00257400
257500              PERFORM 90000-POST-ERROR THRU 90000-EXIT            00257500
257600        ELSE                                                      00257600
257700           MOVE 'G95' TO WS-CURRENT-ERROR                         00257700
257800           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00257800
257900                                                                  00257900
258000                                                                  00258000
258100     IF GMPR-REC-FOUND                                            00258100
258200         IF MO-ENTRY-FOUND                                        00258200
258300             NEXT SENTENCE                                        00258300
258400         ELSE                                                     00258400
258500             MOVE 'G59' TO WS-CURRENT-ERROR                       00258500
258600             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00258600
258700                                                                  00258700
258800                                                                  00258800
258900                                                                  00258900
259000     IF GMPB-REC-FOUND                                            00259000
259100       IF MO-ENTRY-FOUND                                          00259100
259200            NEXT SENTENCE                                         00259200
259300          ELSE                                                    00259300
259400            MOVE 'G63' TO WS-CURRENT-ERROR                        00259400
259500            PERFORM 90000-POST-ERROR THRU 90000-EXIT.             00259500
259600                                                                  00259600
259700     IF GPAR-REC-FOUND OR GPAD-REC-FOUND OR GPAB-REC-FOUND        00259700
259800         IF PR-ENTRY-FOUND                                        00259800
259900             NEXT SENTENCE                                        00259900
260000         ELSE                                                     00260000
260100             MOVE 'G26' TO WS-CURRENT-ERROR                       00260100
260200             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00260200
260300     IF PR-ENTRY-FOUND                                            00260300
260400         SET GSS2-INDEX TO WS-PR-GSS2-INDEX                       00260400
260500         IF GSS2-PR-BC-IND (GSS2-INDEX) = ZERO                    00260500
260600             NEXT SENTENCE                                        00260600
260700         ELSE                                                     00260700
260800         IF GSS2-PR-BC-IND (GSS2-INDEX) = '08' OR '09'            00260800
260900             IF GPAR-REC-FOUND                                    00260900
261000                 NEXT SENTENCE                                    00261000
261100             ELSE                                                 00261100
261200                 MOVE 'G26' TO WS-CURRENT-ERROR                   00261200
261300                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00261300
261400         ELSE                                                     00261400
261500         IF GSS2-PR-BC-IND (GSS2-INDEX) = '05'                    00261500
261600             IF GPAR-REC-FOUND                                    00261600
261700                 NEXT SENTENCE                                    00261700
261800             ELSE                                                 00261800
261900               IF GPAD-REC-FOUND                                  00261900
262000                  NEXT SENTENCE                                   00262000
262100               ELSE                                               00262100
262200                 MOVE 'G27' TO WS-CURRENT-ERROR                   00262200
262300                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00262300
262400                                                                  00262400
262500                                                                  00262500
262600     IF PR-ENTRY-FOUND                                            00262600
262700         SET GSS2-INDEX TO WS-PR-GSS2-INDEX                       00262700
262800         IF GSS2-PR-BS-IND (GSS2-INDEX) = ZERO                    00262800
262900             NEXT SENTENCE                                        00262900
263000         ELSE                                                     00263000
263100         IF GSS2-PR-BS-IND (GSS2-INDEX) = '08' OR '09'            00263100
263200             IF GPAR-REC-FOUND                                    00263200
263300                 NEXT SENTENCE                                    00263300
263400             ELSE                                                 00263400
263500                 MOVE 'G26' TO WS-CURRENT-ERROR                   00263500
263600                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00263600
263700         ELSE                                                     00263700
263800         IF GSS2-PR-BS-IND (GSS2-INDEX) = '05'                    00263800
263900             IF GPAR-REC-FOUND                                    00263900
264000                 NEXT SENTENCE                                    00264000
264100             ELSE                                                 00264100
264200               IF GPAD-REC-FOUND                                  00264200
264300                 NEXT SENTENCE                                    00264300
264400               ELSE                                               00264400
264500                 MOVE 'G28' TO WS-CURRENT-ERROR                   00264500
264600                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00264600
264700         ELSE                                                     00264700
264800         IF GSS2-PR-BS-IND (GSS2-INDEX) = '0B'                    00264800
264900             IF GPAB-REC-FOUND                                    00264900
265000                 NEXT SENTENCE                                    00265000
265100             ELSE                                                 00265100
265200                 MOVE 'G29' TO WS-CURRENT-ERROR                   00265200
265300                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00265300
265400                                                                  00265400
265500                                                                  00265500
265600                                                                  00265600
265700     IF PR-ENTRY-FOUND                                            00265700
265800         SET GSS2-INDEX TO WS-PR-GSS2-INDEX                       00265800
265900         IF GSS2-PR-MM-IND (GSS2-INDEX) = ZERO                    00265900
266000             NEXT SENTENCE                                        00266000
266100         ELSE                                                     00266100
266200         IF GSS2-PR-MM-IND (GSS2-INDEX) = '08' OR '09' OR         00266200
266300                                          '0I' OR '0J'            00266300
266400             IF GPAR-REC-FOUND                                    00266400
266500                 NEXT SENTENCE                                    00266500
266600             ELSE                                                 00266600
266700                 MOVE 'G31' TO WS-CURRENT-ERROR                   00266700
266800                 PERFORM 90000-POST-ERROR THRU 90000-EXIT         00266800
266900         ELSE                                                     00266900
267000         IF GSS2-PR-MM-IND (GSS2-INDEX) = '05' OR '0F'            00267000
267100             IF GPAR-REC-FOUND                                    00267100
267200                 NEXT SENTENCE                                    00267200
267300             ELSE                                                 00267300
267400               IF GPAD-REC-FOUND                                  00267400
267500                   NEXT SENTENCE                                  00267500
267600               ELSE                                               00267600
267700                 MOVE 'G31' TO WS-CURRENT-ERROR                   00267700
267800                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00267800
267900                                                                  00267900
268000     IF GMOR-REC-FOUND                                            00268000
268100         IF MA-ENTRY-FOUND                                        00268100
268200             NEXT SENTENCE                                        00268200
268300         ELSE                                                     00268300
268400             MOVE 'G61' TO WS-CURRENT-ERROR                       00268400
268500             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00268500
268600                                                                  00268600
268700                                                                  00268700
268800     IF GMOB-REC-FOUND                                            00268800
268900         IF MA-ENTRY-FOUND                                        00268900
269000             NEXT SENTENCE                                        00269000
269100         ELSE                                                     00269100
269200             MOVE 'G66' TO WS-CURRENT-ERROR                       00269200
269300             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00269300
269400                                                                  00269400
269500******************************************************************00269500
269600** EDIT G35 IS DEFINITELY NOT 'VICE-VERSA'.                     **00269600
269700******************************************************************00269700
269800     IF GCG-L-O-B-CONTRACT-LEVEL-IND                 = '07'       00269800
269900         IF GCG-MM-TYPE-BEN-PERD-IND                 = ZERO       00269900
270000                 AND GCG-MM-START-AGRMS              = ZERO       00270000
270100                 AND GCG-MM-TERMN-BEN-EXTEN-IND      = ZERO       00270100
270200                 AND GCG-MM-WAIVR-IND                = ZERO       00270200
270300                 AND GCG-MM-WAITG-PERD-IND           = ZERO       00270300
270400                 AND GCG-MM-WAITG-PERD-MEM-DAYS      IS NUMERIC   00270400
270500                 AND GCG-MM-WAITG-PERD-MEM-DAYS      = ZERO       00270500
270600                 AND GCG-MM-WAITG-PERD-SPS-DAYS      IS NUMERIC   00270600
270700                 AND GCG-MM-WAITG-PERD-SPS-DAYS      = ZERO       00270700
270800                 AND GCG-MM-WAITG-PERD-DEP-DAYS      IS NUMERIC   00270800
270900                 AND GCG-MM-WAITG-PERD-DEP-DAYS      = ZERO       00270900
271000                 AND GCG-MM-EXPENSE-FREE-IND         = ZERO       00271000
271100                 AND GCG-MM-EXPENSE-FREE-DAYS        IS NUMERIC   00271100
271200                 AND GCG-MM-EXPENSE-FREE-DAYS        = ZERO       00271200
271300                 AND GCG-MM-FORGN-CLM-IND            = ZERO       00271300
271400                 AND GCG-MM-OB-WAITG-PERD-IND        = ZERO       00271400
271500                 AND GCG-MM-OB-WAITG-PERD-MEM-DAYS   IS NUMERIC   00271500
271600                 AND GCG-MM-OB-WAITG-PERD-MEM-DAYS   = ZERO       00271600
271700                 AND GCG-MM-OB-WAITG-PERD-SPS-DAYS   IS NUMERIC   00271700
271800                 AND GCG-MM-OB-WAITG-PERD-SPS-DAYS   = ZERO       00271800
271900                 AND GCG-MM-OB-WAITG-PERD-DEP-DAYS   IS NUMERIC   00271900
272000                 AND GCG-MM-OB-WAITG-PERD-DEP-DAYS   = ZERO       00272000
272100                 AND GCG-MM-NRM-NWBRN-ELIG-IND       = ZERO       00272100
272200                 AND GCG-MM-SPCL-NWBRN-COVERAGE      = ZERO       00272200
272300                 AND GCG-MM-CARD-REHAB-BIT-IND       = ZERO       00272300
272400                 AND GCG-MM-CARD-REH-PRIOR-ADM       = ZERO       00272400
272500                 AND GCG-MM-CARD-REH-APPRD-SVCS-CD   = ZERO       00272500
272600                 AND GCG-MM-TYPE-ADM-IND             = ZERO       00272600
272700                 AND GCG-MM-TRANSSXL-PMT-RESTR-IND   = ZERO       00272700
272800                 AND GCG-PROV-CONTROL-CONT-MM-IND    = ZERO       00272800
272900                 AND GCG-FAM-REL-CONTROL-CONT-MM     = ZERO       00272900
273000             NEXT SENTENCE                                        00273000
273100         ELSE                                                     00273100
273200             MOVE 'G35' TO WS-CURRENT-ERROR                       00273200
273300             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00273300
273400                                                                  00273400
273500******************************************************************00273500
273600** EDITS PERTAINING TO FIELD RELATIONSHIPS WITHIN THE GROUP     **00273600
273700** SPECIFIC RECORD ITSELF:                                      **00273700
273800** SOME OF THE FOLLOWING EDITS ARE ALSO BEING DONE IN THE       **00273800
273900** GCPS ONLINE SYSTEM.                                          **00273900
274000******************************************************************00274000
274100                                                                  00274100
274200*********************************                                 00274200
274300*** CONDITIONS FOR THE NEXT 3 EDITS, G36, G37, AND G38.           00274300
274400*** 1. IF WAIVER-IND IS CODED (NON-ZERO),                         00274400
274500***        THEN WAITING-PERIOD-IND MUST BE CODED (NON-ZERO).      00274500
274600*** 2. IF WAITING-PERIOD-IND IS NOT CODED (ZERO)                  00274600
274700***        THEN WAIVER-IND MUST NOT BE CODED (ZERO).              00274700
274800*********************************                                 00274800
274900****** COMMENTED OUT PER M189                                     00274900
275000*    IF GCG-BC-WAIVR-IND NOT = ZERO                               00275000
275100*       IF GCG-BC-WAITG-PERD-IND = ZERO                           00275100
275200*            MOVE 'G36' TO WS-CURRENT-ERROR                       00275200
275300*            PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00275300
275400*                                                                 00275400
275500*    IF GCG-BS-WAIVR-IND NOT = ZERO                               00275500
275600*        IF GCG-BS-WAITG-PERD-IND = ZERO                          00275600
275700*            MOVE 'G37' TO WS-CURRENT-ERROR                       00275700
275800*            PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00275800
275900*                                                                 00275900
276000*    IF GCG-MM-WAIVR-IND NOT = ZERO                               00276000
276100*        IF GCG-MM-WAITG-PERD-IND = ZERO                          00276100
276200*            MOVE 'G38' TO WS-CURRENT-ERROR                       00276200
276300*            PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00276300
276400*                                                                 00276400
276500*    IF GCG-BC-WAITG-PERD-IND = ZERO OR 04 OR 09 OR '0B'          00276500
276600*        IF GCG-BC-WAITG-PERD-DAYS IS NUMERIC                     00276600
276700*                AND GCG-BC-WAITG-PERD-DAYS = ZERO                00276700
276800*            NEXT SENTENCE                                        00276800
276900*        ELSE                                                     00276900
277000*            MOVE 'G39' TO WS-CURRENT-ERROR                       00277000
277100*            PERFORM 90000-POST-ERROR THRU 90000-EXIT             00277100
277200*    ELSE                                                         00277200
277300*        IF GCG-BC-WAITG-PERD-DAYS IS NUMERIC                     00277300
277400*                AND GCG-BC-WAITG-PERD-DAYS = ZERO                00277400
277500*            MOVE 'G39' TO WS-CURRENT-ERROR                       00277500
277600*            PERFORM 90000-POST-ERROR THRU 90000-EXIT             00277600
277700*        ELSE                                                     00277700
277800*            NEXT SENTENCE.                                       00277800
277900*                                                                 00277900
278000*    IF GCG-BS-WAITG-PERD-IND = ZERO OR 04 OR 09 OR '0B'          00278000
278100*        IF GCG-BS-WAITG-PERD-DAYS IS NUMERIC                     00278100
278200*                AND GCG-BS-WAITG-PERD-DAYS = ZERO                00278200
278300*            NEXT SENTENCE                                        00278300
278400*        ELSE                                                     00278400
278500*            MOVE 'G40' TO WS-CURRENT-ERROR                       00278500
278600*            PERFORM 90000-POST-ERROR THRU 90000-EXIT             00278600
278700*    ELSE                                                         00278700
278800*        IF GCG-BS-WAITG-PERD-DAYS IS NUMERIC                     00278800
278900*                AND GCG-BS-WAITG-PERD-DAYS = ZERO                00278900
279000*            MOVE 'G40' TO WS-CURRENT-ERROR                       00279000
279100*            PERFORM 90000-POST-ERROR THRU 90000-EXIT             00279100
279200*        ELSE                                                     00279200
279300*            NEXT SENTENCE.                                       00279300
279400*                                                                 00279400
279500*    IF GCG-MM-WAITG-PERD-IND = ZERO OR 04 OR 09                  00279500
279600*        IF GCG-MM-WAITG-PERD-DAYS IS NUMERIC                     00279600
279700*                AND GCG-MM-WAITG-PERD-DAYS = ZERO                00279700
279800*            NEXT SENTENCE                                        00279800
279900*        ELSE                                                     00279900
280000*            MOVE 'G41' TO WS-CURRENT-ERROR                       00280000
280100*            PERFORM 90000-POST-ERROR THRU 90000-EXIT             00280100
280200*    ELSE                                                         00280200
280300*        IF GCG-MM-WAITG-PERD-DAYS IS NUMERIC                     00280300
280400*                AND GCG-MM-WAITG-PERD-DAYS = ZERO                00280400
280500*            MOVE 'G41' TO WS-CURRENT-ERROR                       00280500
280600*            PERFORM 90000-POST-ERROR THRU 90000-EXIT             00280600
280700*        ELSE                                                     00280700
280800*            NEXT SENTENCE.                                       00280800
280900*                                                                 00280900
281000*    IF GCG-BC-WAITG-PERD-IND = '04' OR '09'                      00281000
281100*        IF GCG-BC-EXPENSE-FREE-IND = ZERO                        00281100
281200*                AND GCG-BC-EXPENSE-FREE-DAYS IS NUMERIC          00281200
281300*                AND GCG-BC-EXPENSE-FREE-DAYS = ZERO              00281300
281400*            MOVE 'G42' TO WS-CURRENT-ERROR                       00281400
281500*            PERFORM 90000-POST-ERROR THRU 90000-EXIT             00281500
281600*        ELSE                                                     00281600
281700*            NEXT SENTENCE                                        00281700
281800*    ELSE                                                         00281800
281900*        IF GCG-BC-EXPENSE-FREE-IND = ZERO                        00281900
282000*                AND GCG-BC-EXPENSE-FREE-DAYS IS NUMERIC          00282000
282100*                AND GCG-BC-EXPENSE-FREE-DAYS = ZERO              00282100
282200*            NEXT SENTENCE                                        00282200
282300*        ELSE                                                     00282300
282400*            MOVE 'G42' TO WS-CURRENT-ERROR                       00282400
282500*            PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00282500
282600*                                                                 00282600
282700*    IF GCG-BS-WAITG-PERD-IND = '04' OR '09'                      00282700
282800*        IF GCG-BS-EXPENSE-FREE-IND = ZERO                        00282800
282900*                AND GCG-BS-EXPENSE-FREE-DAYS IS NUMERIC          00282900
283000*                AND GCG-BS-EXPENSE-FREE-DAYS = ZERO              00283000
283100*            MOVE 'G43' TO WS-CURRENT-ERROR                       00283100
283200*            PERFORM 90000-POST-ERROR THRU 90000-EXIT             00283200
283300*        ELSE                                                     00283300
283400*            NEXT SENTENCE                                        00283400
283500*    ELSE                                                         00283500
283600*        IF GCG-BS-EXPENSE-FREE-IND = ZERO                        00283600
283700*                AND GCG-BS-EXPENSE-FREE-DAYS IS NUMERIC          00283700
283800*                AND GCG-BS-EXPENSE-FREE-DAYS = ZERO              00283800
283900*            NEXT SENTENCE                                        00283900
284000*        ELSE                                                     00284000
284100*            MOVE 'G43' TO WS-CURRENT-ERROR                       00284100
284200*            PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00284200
284300*                                                                 00284300
284400*    IF GCG-MM-WAITG-PERD-IND = '04' OR '09'                      00284400
284500*        IF GCG-MM-EXPENSE-FREE-IND = ZERO                        00284500
284600*                AND GCG-MM-EXPENSE-FREE-DAYS IS NUMERIC          00284600
284700*                AND GCG-MM-EXPENSE-FREE-DAYS = ZERO              00284700
284800*            MOVE 'G44' TO WS-CURRENT-ERROR                       00284800
284900*            PERFORM 90000-POST-ERROR THRU 90000-EXIT             00284900
285000*        ELSE                                                     00285000
285100*            NEXT SENTENCE                                        00285100
285200*    ELSE                                                         00285200
285300*        IF GCG-MM-EXPENSE-FREE-IND = ZERO                        00285300
285400*                AND GCG-MM-EXPENSE-FREE-DAYS IS NUMERIC          00285400
285500*                AND GCG-MM-EXPENSE-FREE-DAYS = ZERO              00285500
285600*            NEXT SENTENCE                                        00285600
285700*        ELSE                                                     00285700
285800*            MOVE 'G44' TO WS-CURRENT-ERROR                       00285800
285900*            PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00285900
286000                                                                  00286000
286100     IF GCG-EOMB-REQRD-IND = ZERO                                 00286100
286200         IF GCG-COORD-MCARE-IND = ZERO                            00286200
286300             NEXT SENTENCE                                        00286300
286400         ELSE                                                     00286400
286500             MOVE 'G45' TO WS-CURRENT-ERROR                       00286500
286600             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00286600
286700     ELSE                                                         00286700
286800         IF GCG-COORD-MCARE-IND = ZERO                            00286800
286900             MOVE 'G45' TO WS-CURRENT-ERROR                       00286900
287000             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00287000
287100         ELSE                                                     00287100
287200             NEXT SENTENCE.                                       00287200
287300                                                                  00287300
287400     IF GCG-ANNL-REINST-IND = ZERO                                00287400
287500         IF GCG-ANNL-REINST-AMT IS NUMERIC                        00287500
287600                 AND GCG-ANNL-REINST-AMT = ZERO                   00287600
287700             NEXT SENTENCE                                        00287700
287800         ELSE                                                     00287800
287900             MOVE 'G46' TO WS-CURRENT-ERROR                       00287900
288000             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00288000
288100     ELSE                                                         00288100
288200         IF GCG-ANNL-REINST-AMT IS NUMERIC                        00288200
288300           AND                                                    00288300
288400            GCG-ANNL-REINST-AMT = ZERO                            00288400
288500             MOVE 'G46' TO WS-CURRENT-ERROR                       00288500
288600             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00288600
288700                                                                  00288700
288800     IF GCG-GOOD-HLTH-REINST-IND NOT = ZERO                       00288800
288900         IF GCG-GOOD-HLTH-REINST-AFTR-BEN IS NUMERIC              00288900
289000           AND                                                    00289000
289100            GCG-GOOD-HLTH-REINST-AFTR-BEN NOT = ZERO              00289100
289200             NEXT SENTENCE                                        00289200
289300         ELSE                                                     00289300
289400             MOVE 'G47' TO WS-CURRENT-ERROR                       00289400
289500             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00289500
289600     ELSE                                                         00289600
289700         IF GCG-GOOD-HLTH-REINST-AFTR-BEN IS NUMERIC              00289700
289800           AND                                                    00289800
289900            GCG-GOOD-HLTH-REINST-AFTR-BEN NOT = ZERO              00289900
290000             MOVE 'G47' TO WS-CURRENT-ERROR                       00290000
290100             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00290100
290200                                                                  00290200
290300     IF GCG-DEP-TERMN-IND = ZERO                                  00290300
290400         IF GCG-DEP-MAX-AGE = ZERO                                00290400
290500                 AND GCG-STU-MAX-AGE = ZERO                       00290500
290600             NEXT SENTENCE                                        00290600
290700         ELSE                                                     00290700
290800             MOVE 'G48' TO WS-CURRENT-ERROR                       00290800
290900             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00290900
291000     ELSE                                                         00291000
291100         IF GCG-DEP-MAX-AGE = ZERO                                00291100
291200                 AND GCG-STU-MAX-AGE = ZERO                       00291200
291300             MOVE 'G48' TO WS-CURRENT-ERROR                       00291300
291400             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00291400
291500                                                                  00291500
291600     IF GCG-DEP-MAX-AGE = GCG-STU-MAX-AGE                         00291600
291700         IF GCG-STU-CERTN-REQRD-IND = ZERO                        00291700
291800             NEXT SENTENCE                                        00291800
291900         ELSE                                                     00291900
292000             MOVE 'G50' TO WS-CURRENT-ERROR                       00292000
292100             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00292100
292200     ELSE                                                         00292200
292300         IF GCG-STU-CERTN-REQRD-IND = ZERO                        00292300
292400             MOVE 'G49' TO WS-CURRENT-ERROR                       00292400
292500             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00292500
292600         ELSE                                                     00292600
292700             NEXT SENTENCE.                                       00292700
292800                                                                  00292800
292900     IF GCG-BC-OB-WAITG-PERD-IND = ZERO                           00292900
293000         IF (GCG-BC-OB-WAITG-PERD-MEM-DAYS IS NUMERIC             00293000
293100                 AND GCG-BC-OB-WAITG-PERD-MEM-DAYS = ZERO)        00293100
293200                AND (GCG-BC-OB-WAITG-PERD-SPS-DAYS IS NUMERIC     00293200
293300                 AND GCG-BC-OB-WAITG-PERD-SPS-DAYS = ZERO)        00293300
293400                AND (GCG-BC-OB-WAITG-PERD-DEP-DAYS IS NUMERIC     00293400
293500                 AND GCG-BC-OB-WAITG-PERD-DEP-DAYS = ZERO)        00293500
293600             NEXT SENTENCE                                        00293600
293700         ELSE                                                     00293700
293800             MOVE 'G51' TO WS-CURRENT-ERROR                       00293800
293900             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00293900
294000     ELSE                                                         00294000
294100         IF (GCG-BC-OB-WAITG-PERD-MEM-DAYS IS NUMERIC             00294100
294200                 AND GCG-BC-OB-WAITG-PERD-MEM-DAYS > ZERO)        00294200
294300                 OR (GCG-BC-OB-WAITG-PERD-SPS-DAYS IS NUMERIC     00294300
294400                 AND GCG-BC-OB-WAITG-PERD-SPS-DAYS > ZERO)        00294400
294500                 OR (GCG-BC-OB-WAITG-PERD-DEP-DAYS IS NUMERIC     00294500
294600                 AND GCG-BC-OB-WAITG-PERD-DEP-DAYS > ZERO)        00294600
294700             NEXT SENTENCE                                        00294700
294800         ELSE                                                     00294800
294900             MOVE 'G51' TO WS-CURRENT-ERROR                       00294900
295000             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00295000
295100                                                                  00295100
295200     IF GCG-BS-OB-WAITG-PERD-IND = ZERO                           00295200
295300         IF (GCG-BS-OB-WAITG-PERD-MEM-DAYS IS NUMERIC             00295300
295400                 AND GCG-BS-OB-WAITG-PERD-MEM-DAYS = ZERO)        00295400
295500                 AND (GCG-BS-OB-WAITG-PERD-SPS-DAYS IS NUMERIC    00295500
295600                 AND GCG-BS-OB-WAITG-PERD-SPS-DAYS = ZERO)        00295600
295700                 AND (GCG-BS-OB-WAITG-PERD-DEP-DAYS IS NUMERIC    00295700
295800                 AND GCG-BS-OB-WAITG-PERD-DEP-DAYS = ZERO)        00295800
295900             NEXT SENTENCE                                        00295900
296000         ELSE                                                     00296000
296100             MOVE 'G52' TO WS-CURRENT-ERROR                       00296100
296200             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00296200
296300     ELSE                                                         00296300
296400         IF (GCG-BS-OB-WAITG-PERD-MEM-DAYS IS NUMERIC             00296400
296500                 AND GCG-BS-OB-WAITG-PERD-MEM-DAYS > ZERO)        00296500
296600                 OR (GCG-BS-OB-WAITG-PERD-SPS-DAYS IS NUMERIC     00296600
296700                 AND GCG-BS-OB-WAITG-PERD-SPS-DAYS > ZERO)        00296700
296800                 OR (GCG-BS-OB-WAITG-PERD-DEP-DAYS IS NUMERIC     00296800
296900                 AND GCG-BS-OB-WAITG-PERD-DEP-DAYS > ZERO)        00296900
297000             NEXT SENTENCE                                        00297000
297100         ELSE                                                     00297100
297200             MOVE 'G52' TO WS-CURRENT-ERROR                       00297200
297300             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00297300
297400                                                                  00297400
297500     IF GCG-MM-OB-WAITG-PERD-IND = ZERO                           00297500
297600         IF (GCG-MM-OB-WAITG-PERD-MEM-DAYS IS NUMERIC             00297600
297700                 AND GCG-MM-OB-WAITG-PERD-MEM-DAYS = ZERO)        00297700
297800                 AND (GCG-MM-OB-WAITG-PERD-SPS-DAYS IS NUMERIC    00297800
297900                 AND GCG-MM-OB-WAITG-PERD-SPS-DAYS = ZERO)        00297900
298000                 AND (GCG-MM-OB-WAITG-PERD-DEP-DAYS IS NUMERIC    00298000
298100                 AND GCG-MM-OB-WAITG-PERD-DEP-DAYS = ZERO)        00298100
298200             NEXT SENTENCE                                        00298200
298300         ELSE                                                     00298300
298400             MOVE 'G53' TO WS-CURRENT-ERROR                       00298400
298500             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00298500
298600     ELSE                                                         00298600
298700         IF (GCG-MM-OB-WAITG-PERD-MEM-DAYS IS NUMERIC             00298700
298800                 AND GCG-MM-OB-WAITG-PERD-MEM-DAYS > ZERO)        00298800
298900                 OR (GCG-MM-OB-WAITG-PERD-SPS-DAYS IS NUMERIC     00298900
299000                 AND GCG-MM-OB-WAITG-PERD-SPS-DAYS > ZERO)        00299000
299100                 OR (GCG-MM-OB-WAITG-PERD-DEP-DAYS IS NUMERIC     00299100
299200                 AND GCG-MM-OB-WAITG-PERD-DEP-DAYS > ZERO)        00299200
299300             NEXT SENTENCE                                        00299300
299400         ELSE                                                     00299400
299500             MOVE 'G53' TO WS-CURRENT-ERROR                       00299500
299600             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00299600
299700                                                                  00299700
299800     IF GCG-WC-MAR-STAT-IND = ZERO                                00299800
299900             AND GCG-WC-DIAGNOSIS-BIT-IND = ZERO                  00299900
300000             AND GCG-WC-INVESN-MODE = ZERO                        00300000
300100         IF GCG-WC-MIN-AGE = ZERO                                 00300100
300200                 AND GCG-WC-MAX-AGE = ZERO                        00300200
300300             NEXT SENTENCE                                        00300300
300400         ELSE                                                     00300400
300500             MOVE 'G55' TO WS-CURRENT-ERROR                       00300500
300600             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00300600
300700     ELSE                                                         00300700
300800         IF GCG-WC-MIN-AGE = ZERO                                 00300800
300900                 OR GCG-WC-MAX-AGE = ZERO                         00300900
301000             MOVE 'G54' TO WS-CURRENT-ERROR                       00301000
301100             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00301100
301200         ELSE                                                     00301200
301300             NEXT SENTENCE.                                       00301300
301400                                                                  00301400
301500     IF GCG-ANNL-REINST-IND = ZERO                                00301500
301600         IF GCG-FIRST-YR-REINST-EFF-DT = 'NONE  '                 00301600
301700             NEXT SENTENCE                                        00301700
301800         ELSE                                                     00301800
301900             MOVE 'G57' TO WS-CURRENT-ERROR                       00301900
302000             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00302000
302100     ELSE                                                         00302100
302200         CALL 'TSGJULN' USING WS-JUL-DATE                         00302200
302300                              GCG-FIRST-YR-REINST-EFF-DT          00302300
302400         IF JUL-CONV-ERR                                          00302400
302500             MOVE 'G56' TO WS-CURRENT-ERROR                       00302500
302600             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00302600
302700                                                                  00302700
302800****************************************************************  00302800
302900* EDIT FOR M422 ADDED.                                            00302900
303000                                                                  00303000
303100     IF GCG-REIMBUR-SUBROG-IND NOT = '00'                         00303100
303200         IF GCG-REIMBUR-SUBROF-ACCM-L-O-B NOT = '0'               00303200
303300             NEXT SENTENCE                                        00303300
303400         ELSE                                                     00303400
303500             MOVE 'G68' TO WS-CURRENT-ERROR                       00303500
303600             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00303600
303700     ELSE                                                         00303700
303800         IF GCG-REIMBUR-SUBROF-ACCM-L-O-B NOT = '0'               00303800
303900             MOVE 'G68' TO WS-CURRENT-ERROR                       00303900
304000             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00304000
304100                                                                  00304100
304200****************************************************************  00304200
304300* EDITS FOR M426 ADDED.                                           00304300
304400                                                                  00304400
304500     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '07'                       00304500
304600         IF (GCG-MEDCR-ACCM-L-O-B = '0' OR 'C')                   00304600
304700             NEXT SENTENCE                                        00304700
304800         ELSE                                                     00304800
304900             MOVE 'G69' TO WS-CURRENT-ERROR                       00304900
305000             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00305000
305100     ELSE                                                         00305100
305200         IF GCG-MEDCR-ACCM-L-O-B = 'C'                            00305200
305300             MOVE 'G69' TO WS-CURRENT-ERROR                       00305300
305400             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00305400
305500                                                                  00305500
305600     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '07'                       00305600
305700         IF (GCG-MSPS-ACCM-L-O-B-IND = '0' OR 'C')                00305700
305800             NEXT SENTENCE                                        00305800
305900         ELSE                                                     00305900
306000             MOVE 'GC6' TO WS-CURRENT-ERROR                       00306000
306100             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00306100
306200     ELSE                                                         00306200
306300         IF GCG-MSPS-ACCM-L-O-B-IND = 'C'                         00306300
306400             MOVE 'GC6' TO WS-CURRENT-ERROR                       00306400
306500             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00306500
306600                                                                  00306600
306700     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '07'                       00306700
306800         IF (GCG-WKR-COMP-L-O-B = '0' OR 'C')                     00306800
306900             NEXT SENTENCE                                        00306900
307000         ELSE                                                     00307000
307100             MOVE 'G70' TO WS-CURRENT-ERROR                       00307100
307200             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00307200
307300     ELSE                                                         00307300
307400         IF GCG-WKR-COMP-L-O-B = 'C'                              00307400
307500             MOVE 'G70' TO WS-CURRENT-ERROR                       00307500
307600             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00307600
307700                                                                  00307700
307800     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '07'                       00307800
307900         IF (GCG-COB-ACCM-L-O-B-IND = '0' OR 'C')                 00307900
308000             NEXT SENTENCE                                        00308000
308100         ELSE                                                     00308100
308200             MOVE 'G71' TO WS-CURRENT-ERROR                       00308200
308300             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00308300
308400     ELSE                                                         00308400
308500         IF GCG-COB-ACCM-L-O-B-IND = 'C'                          00308500
308600             MOVE 'G71' TO WS-CURRENT-ERROR                       00308600
308700             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00308700
308800                                                                  00308800
308900     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '07'                       00308900
309000         IF (GCG-REIMBUR-SUBROF-ACCM-L-O-B = '0' OR 'C')          00309000
309100             NEXT SENTENCE                                        00309100
309200         ELSE                                                     00309200
309300             MOVE 'G72' TO WS-CURRENT-ERROR                       00309300
309400             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00309400
309500     ELSE                                                         00309500
309600         IF GCG-REIMBUR-SUBROF-ACCM-L-O-B = 'C'                   00309600
309700             MOVE 'G72' TO WS-CURRENT-ERROR                       00309700
309800             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00309800
309900                                                                  00309900
310000     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '07'                       00310000
310100         IF (GCG-UNSOLICIT-RFND-L-O-B = '0' OR 'C')               00310100
310200             NEXT SENTENCE                                        00310200
310300         ELSE                                                     00310300
310400             MOVE 'G73' TO WS-CURRENT-ERROR                       00310400
310500             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00310500
310600     ELSE                                                         00310600
310700         IF GCG-UNSOLICIT-RFND-L-O-B = 'C'                        00310700
310800             MOVE 'G73' TO WS-CURRENT-ERROR                       00310800
310900             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00310900
311000                                                                  00311000
311100     IF  GCG-MEDCR-ACCM-BEN-PERD-IND = ZEROS                      00311100
311200         IF  GCG-MEDCR-ACCM-L-O-B = ZEROS                         00311200
311300             NEXT SENTENCE                                        00311300
311400         ELSE                                                     00311400
311500             MOVE  'G97'  TO WS-CURRENT-ERROR                     00311500
311600             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00311600
311700     ELSE                                                         00311700
311800         IF  GCG-MEDCR-ACCM-L-O-B = ZEROS                         00311800
311900             MOVE  'G97'  TO WS-CURRENT-ERROR                     00311900
312000             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00312000
312100                                                                  00312100
312200                                                                  00312200
312300     IF  GCG-MSPS-ACCM-BEN-PERD-IND = ZEROS                       00312300
312400         IF  GCG-MSPS-ACCM-L-O-B-IND = ZEROS                      00312400
312500             NEXT SENTENCE                                        00312500
312600         ELSE                                                     00312600
312700             MOVE  'GC5'  TO WS-CURRENT-ERROR                     00312700
312800             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00312800
312900     ELSE                                                         00312900
313000         IF  GCG-MSPS-ACCM-L-O-B-IND = ZEROS                      00313000
313100             MOVE  'GC5'  TO WS-CURRENT-ERROR                     00313100
313200             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00313200
313300                                                                  00313300
313400*    IF  GCG-PRODUCT-TYPE = ZEROS                                 00313400
313500*        IF  GCG-PRODUCT-TYPE-IND = ZEROS                         00313500
313600*            NEXT SENTENCE                                        00313600
313700*        ELSE                                                     00313700
313800*            MOVE 'G98' TO WS-CURRENT-ERROR                       00313800
313900*            PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00313900
314000                                                                  00314000
314100     IF  GCG-ST-PRGM-ACCUM-BEN-PRD-IND = ZEROS                    00314100
314200         IF  GCG-ST-PRGM-ACCUM-L-O-B-IND = ZEROS                  00314200
314300             NEXT SENTENCE                                        00314300
314400         ELSE                                                     00314400
314500             MOVE 'G99' TO WS-CURRENT-ERROR                       00314500
314600             PERFORM 90000-POST-ERROR THRU 90000-EXIT             00314600
314700     ELSE                                                         00314700
314800         IF  GCG-ST-PRGM-ACCUM-L-O-B-IND = ZEROS                  00314800
314900             MOVE 'G99' TO WS-CURRENT-ERROR                       00314900
315000             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00315000
315100                                                                  00315100
315200                                                                  00315200
315300**                                                                00315300
315400* CREATED EDITS FOR D332          FRY  6/11/97                    00315400
315500* CHANGED EDITS FOR D332          FRY  8/12/97                    00315500
315600**   CHANGED DAYS                                                 00315600
315700**      FROM:  CAN NOT BE GREATER THAN 180 DAYS (SIX MONTHS)      00315700
315800**        TO:  CAN NOT BE GREATER THAN 546 DAYS.                  00315800
315900**                                                                00315900
316000                                                                  00316000
316100     IF  GCG-BC-LATE-ENROLL-MEM-DAYS    <  547                    00316100
316200         NEXT SENTENCE                                            00316200
316300     ELSE                                                         00316300
316400         MOVE 'GB1' TO WS-CURRENT-ERROR                           00316400
316500         PERFORM 90000-POST-ERROR THRU 90000-EXIT.                00316500
316600                                                                  00316600
316700     IF  GCG-BS-LATE-ENROLL-MEM-DAYS    <  547                    00316700
316800         NEXT SENTENCE                                            00316800
316900     ELSE                                                         00316900
317000         MOVE 'GB2' TO WS-CURRENT-ERROR                           00317000
317100         PERFORM 90000-POST-ERROR THRU 90000-EXIT.                00317100
317200                                                                  00317200
317300     IF  GCG-MM-LATE-ENROLL-MEM-DAYS    <  547                    00317300
317400         NEXT SENTENCE                                            00317400
317500     ELSE                                                         00317500
317600         MOVE 'GB3' TO WS-CURRENT-ERROR                           00317600
317700         PERFORM 90000-POST-ERROR THRU 90000-EXIT.                00317700
317800                                                                  00317800
317900     IF  GCG-BC-LATE-ENROLL-SPS-DAYS    <  547                    00317900
318000         NEXT SENTENCE                                            00318000
318100     ELSE                                                         00318100
318200         MOVE 'GB4' TO WS-CURRENT-ERROR                           00318200
318300         PERFORM 90000-POST-ERROR THRU 90000-EXIT.                00318300
318400                                                                  00318400
318500     IF  GCG-BS-LATE-ENROLL-SPS-DAYS    <  547                    00318500
318600         NEXT SENTENCE                                            00318600
318700     ELSE                                                         00318700
318800         MOVE 'GB5' TO WS-CURRENT-ERROR                           00318800
318900         PERFORM 90000-POST-ERROR THRU 90000-EXIT.                00318900
319000                                                                  00319000
319100     IF  GCG-MM-LATE-ENROLL-SPS-DAYS    <  547                    00319100
319200         NEXT SENTENCE                                            00319200
319300     ELSE                                                         00319300
319400         MOVE 'GB6' TO WS-CURRENT-ERROR                           00319400
319500         PERFORM 90000-POST-ERROR THRU 90000-EXIT.                00319500
319600                                                                  00319600
319700     IF  GCG-BC-LATE-ENROLL-DEP-DAYS    <  547                    00319700
319800         NEXT SENTENCE                                            00319800
319900     ELSE                                                         00319900
320000         MOVE 'GB7' TO WS-CURRENT-ERROR                           00320000
320100         PERFORM 90000-POST-ERROR THRU 90000-EXIT.                00320100
320200                                                                  00320200
320300     IF  GCG-BS-LATE-ENROLL-DEP-DAYS    <  547                    00320300
320400         NEXT SENTENCE                                            00320400
320500     ELSE                                                         00320500
320600         MOVE 'GB8' TO WS-CURRENT-ERROR                           00320600
320700         PERFORM 90000-POST-ERROR THRU 90000-EXIT.                00320700
320800                                                                  00320800
320900     IF  GCG-MM-LATE-ENROLL-DEP-DAYS    <  547                    00320900
321000         NEXT SENTENCE                                            00321000
321100     ELSE                                                         00321100
321200         MOVE 'GB9' TO WS-CURRENT-ERROR                           00321200
321300         PERFORM 90000-POST-ERROR THRU 90000-EXIT.                00321300
321400                                                                  00321400
321500                                                                  00321500
321600                                                                  00321600
321700******************************************************************00321700
321800** EDITS PERTAINING TO FIELD RELATIONSHIPS WITHIN THE #GCCP     **00321800
321900** TABULAR RECORD ITSELF:                                       **00321900
322000** SOME OF THE FOLLOWING EDITS ARE ALSO BEING DONE IN THE       **00322000
322100** GCPS ONLINE SYSTEM.                                          **00322100
322200******************************************************************00322200
322300                                                                  00322300
322400     IF FS-ENTRY-FOUND                                            00322400
322500         SET GSS2-INDEX TO WS-FS-GSS2-INDEX                       00322500
322600         IF GSS2-FS-BC-IND (GSS2-INDEX) = ZERO                    00322600
322700             IF GSS2-FS-BC-OPEX-OVERRIDE-IND                      00322700
322800                         (GSS2-INDEX) = ZERO                      00322800
322900                     AND GSS2-FS-BC-OPEX-APPLIC-IND               00322900
323000                         (GSS2-INDEX) = ZERO                      00323000
323100                     AND GSS2-FS-BC-DEDU-APPLIC-IND               00323100
323200                         (GSS2-INDEX) = ZERO                      00323200
323300                     AND GSS2-FS-BC-CALC-METHOD                   00323300
323400                         (GSS2-INDEX) = ZERO                      00323400
323500                     AND GSS2-FS-BC-ALT-PRICING-METH              00323500
323600                         (GSS2-INDEX) = ZERO                      00323600
323700                 NEXT SENTENCE                                    00323700
323800             ELSE                                                 00323800
323900                 MOVE 'P01' TO WS-CURRENT-ERROR                   00323900
324000                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00324000
324100                                                                  00324100
324200     IF FS-ENTRY-FOUND                                            00324200
324300         SET GSS2-INDEX TO WS-FS-GSS2-INDEX                       00324300
324400         IF GSS2-FS-BS-IND (GSS2-INDEX) = ZERO                    00324400
324500             IF GSS2-FS-BS-OPEX-OVERRIDE-IND                      00324500
324600                         (GSS2-INDEX) = ZERO                      00324600
324700                     AND GSS2-FS-BS-OPEX-APPLIC-IND               00324700
324800                         (GSS2-INDEX) = ZERO                      00324800
324900                     AND GSS2-FS-BS-DEDU-APPLIC-IND               00324900
325000                         (GSS2-INDEX) = ZERO                      00325000
325100                     AND GSS2-FS-BS-CALC-METHOD                   00325100
325200                         (GSS2-INDEX) = ZERO                      00325200
325300                     AND GSS2-FS-BS-ALT-PRICING-METH              00325300
325400                         (GSS2-INDEX) = ZERO                      00325400
325500                 NEXT SENTENCE                                    00325500
325600             ELSE                                                 00325600
325700                 MOVE 'P11' TO WS-CURRENT-ERROR                   00325700
325800                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00325800
325900                                                                  00325900
326000     IF FS-ENTRY-FOUND                                            00326000
326100         SET GSS2-INDEX TO WS-FS-GSS2-INDEX                       00326100
326200         IF GSS2-FS-MM-IND (GSS2-INDEX) = ZERO                    00326200
326300             IF GSS2-FS-MM-OPEX-OVERRIDE-IND                      00326300
326400                         (GSS2-INDEX) = ZERO                      00326400
326500                     AND GSS2-FS-MM-OPEX-APPLIC-IND               00326500
326600                         (GSS2-INDEX) = ZERO                      00326600
326700                     AND GSS2-FS-MM-DEDU-APPLIC-IND               00326700
326800                         (GSS2-INDEX) = ZERO                      00326800
326900                     AND GSS2-FS-MM-CALC-METHOD                   00326900
327000                         (GSS2-INDEX) = ZERO                      00327000
327100                     AND GSS2-FS-MM-ALT-PRICING-METH              00327100
327200                         (GSS2-INDEX) = ZERO                      00327200
327300                 NEXT SENTENCE                                    00327300
327400             ELSE                                                 00327400
327500                 MOVE 'P21' TO WS-CURRENT-ERROR                   00327500
327600                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00327600
327700                                                                  00327700
327800                                                                  00327800
327900*** 14631    3/18/96   FRY   ADD COMMUNITY BLUE (CB)              00327900
328000                                                                  00328000
328100     IF CB-ENTRY-FOUND                                            00328100
328200        SET GSS2-INDEX TO WS-CB-GSS2-INDEX                        00328200
328300        IF GSS2-CB-BC-IND (GSS2-INDEX)                   =  ZEROES00328300
328400         IF GSS2-CB-BC-CALC-METHOD (GSS2-INDEX)          =  ZEROES00328400
328500          AND GSS2-CB-BC-PAYMENT-LEVEL-IND (GSS2-INDEX)  =  ZEROES00328500
328600          AND GSS2-CB-BC-ALT-PRICING-METHOD (GSS2-INDEX) =  ZEROES00328600
328700              NEXT SENTENCE                                       00328700
328800         ELSE                                                     00328800
328900              MOVE 'P60' TO WS-CURRENT-ERROR                      00328900
329000              PERFORM 90000-POST-ERROR THRU 90000-EXIT.           00329000
329100                                                                  00329100
329200     IF CB-ENTRY-FOUND                                            00329200
329300        SET GSS2-INDEX TO WS-CB-GSS2-INDEX                        00329300
329400        IF GSS2-CB-BS-IND (GSS2-INDEX)                   =  ZEROES00329400
329500         IF GSS2-CB-BS-CALC-METHOD (GSS2-INDEX)          =  ZEROES00329500
329600          AND GSS2-CB-BS-PAYMENT-LEVEL-IND (GSS2-INDEX)  =  ZEROES00329600
329700          AND GSS2-CB-BS-ALT-PRICING-METHOD (GSS2-INDEX) =  ZEROES00329700
329800              NEXT SENTENCE                                       00329800
329900         ELSE                                                     00329900
330000              MOVE 'P61' TO WS-CURRENT-ERROR                      00330000
330100              PERFORM 90000-POST-ERROR THRU 90000-EXIT.           00330100
330200                                                                  00330200
330300     IF CB-ENTRY-FOUND                                            00330300
330400        SET GSS2-INDEX TO WS-CB-GSS2-INDEX                        00330400
330500        IF GSS2-CB-MM-IND (GSS2-INDEX)                   =  ZEROES00330500
330600         IF GSS2-CB-MM-CALC-METHOD (GSS2-INDEX)          =  ZEROES00330600
330700          AND GSS2-CB-MM-PAYMENT-LEVEL-IND (GSS2-INDEX)  =  ZEROES00330700
330800          AND GSS2-CB-MM-ALT-PRICING-METHOD (GSS2-INDEX) =  ZEROES00330800
330900              NEXT SENTENCE                                       00330900
331000         ELSE                                                     00331000
331100              MOVE 'P62' TO WS-CURRENT-ERROR                      00331100
331200              PERFORM 90000-POST-ERROR THRU 90000-EXIT.           00331200
331300                                                                  00331300
331400                                                                  00331400
331500**** 14045    2/22/95  RMK   ADD COMMUNITY PREFERRED OPTION (CP)  00331500
331600                                                                  00331600
331700     IF CP-ENTRY-FOUND                                            00331700
331800         SET GSS2-INDEX TO WS-CP-GSS2-INDEX                       00331800
331900         IF GSS2-CP-BC-IND (GSS2-INDEX) = ZEROES                  00331900
332000             IF GSS2-CP-BC-CALC-METHOD                            00332000
332100                         (GSS2-INDEX) = ZEROES                    00332100
332200                     AND GSS2-CP-BC-PAYMENT-LEVEL-IND             00332200
332300                         (GSS2-INDEX) = ZEROES                    00332300
332400                     AND GSS2-CP-BC-ALT-PRICING-METHOD            00332400
332500                         (GSS2-INDEX) = ZEROES                    00332500
332600                 NEXT SENTENCE                                    00332600
332700             ELSE                                                 00332700
332800                 MOVE 'P51' TO WS-CURRENT-ERROR                   00332800
332900                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00332900
333000                                                                  00333000
333100     IF CP-ENTRY-FOUND                                            00333100
333200         SET GSS2-INDEX TO WS-CP-GSS2-INDEX                       00333200
333300         IF GSS2-CP-BS-IND (GSS2-INDEX) = ZEROES                  00333300
333400            IF GSS2-CP-BS-CALC-METHOD                             00333400
333500                         (GSS2-INDEX)   = ZEROES                  00333500
333600                     AND GSS2-CP-BS-PAYMENT-LEVEL-IND             00333600
333700                         (GSS2-INDEX)   = ZEROES                  00333700
333800                     AND GSS2-CP-BS-ALT-PRICING-METHOD            00333800
333900                         (GSS2-INDEX)   = ZEROES                  00333900
334000                 NEXT SENTENCE                                    00334000
334100             ELSE                                                 00334100
334200                 MOVE 'P52' TO WS-CURRENT-ERROR                   00334200
334300                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00334300
334400                                                                  00334400
334500     IF CP-ENTRY-FOUND                                            00334500
334600         SET GSS2-INDEX TO WS-CP-GSS2-INDEX                       00334600
334700         IF GSS2-CP-MM-IND (GSS2-INDEX) = ZEROES                  00334700
334800             IF GSS2-CP-MM-CALC-METHOD                            00334800
334900                         (GSS2-INDEX)   = ZEROES                  00334900
335000                     AND GSS2-CP-MM-PAYMENT-LEVEL-IND             00335000
335100                         (GSS2-INDEX)   = ZEROES                  00335100
335200                     AND GSS2-CP-MM-ALT-PRICING-METHOD            00335200
335300                         (GSS2-INDEX)   = ZEROES                  00335300
335400                 NEXT SENTENCE                                    00335400
335500             ELSE                                                 00335500
335600                 MOVE 'P53' TO WS-CURRENT-ERROR                   00335600
335700                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00335700
335800                                                                  00335800
335900     IF HO-ENTRY-FOUND                                            00335900
336000         SET GSS2-INDEX TO WS-HO-GSS2-INDEX                       00336000
336100         IF GSS2-HO-BC-IND (GSS2-INDEX) = ZERO                    00336100
336200             IF GSS2-HO-BC-OPEX-OVERRIDE-IND                      00336200
336300                         (GSS2-INDEX) = ZERO                      00336300
336400                     AND GSS2-HO-BC-OPEX-APPLIC-IND               00336400
336500                         (GSS2-INDEX) = ZERO                      00336500
336600                     AND GSS2-HO-BC-DEDU-APPLIC-IND               00336600
336700                         (GSS2-INDEX) = ZERO                      00336700
336800                     AND GSS2-HO-BC-IP-ALT-PRICING-METH           00336800
336900                         (GSS2-INDEX) = ZERO                      00336900
337000                     AND GSS2-HO-BC-OP-ALT-PRICING-METH           00337000
337100                         (GSS2-INDEX) = ZERO                      00337100
337200                 NEXT SENTENCE                                    00337200
337300             ELSE                                                 00337300
337400                 MOVE 'P02' TO WS-CURRENT-ERROR                   00337400
337500                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00337500
337600                                                                  00337600
337700     IF HO-ENTRY-FOUND                                            00337700
337800         SET GSS2-INDEX TO WS-HO-GSS2-INDEX                       00337800
337900         IF GSS2-HO-BS-IND (GSS2-INDEX) = ZERO                    00337900
338000             IF GSS2-HO-BS-OPEX-OVERRIDE-IND                      00338000
338100                         (GSS2-INDEX) = ZERO                      00338100
338200                     AND GSS2-HO-BS-OPEX-APPLIC-IND               00338200
338300                         (GSS2-INDEX) = ZERO                      00338300
338400                     AND GSS2-HO-BS-DEDU-APPLIC-IND               00338400
338500                         (GSS2-INDEX) = ZERO                      00338500
338600                     AND GSS2-HO-BS-IP-ALT-PRICING-METH           00338600
338700                         (GSS2-INDEX) = ZERO                      00338700
338800                     AND GSS2-HO-BS-OP-ALT-PRICING-METH           00338800
338900                         (GSS2-INDEX) = ZERO                      00338900
339000                 NEXT SENTENCE                                    00339000
339100             ELSE                                                 00339100
339200                 MOVE 'P12' TO WS-CURRENT-ERROR                   00339200
339300                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00339300
339400                                                                  00339400
339500     IF HO-ENTRY-FOUND                                            00339500
339600         SET GSS2-INDEX TO WS-HO-GSS2-INDEX                       00339600
339700         IF GSS2-HO-MM-IND (GSS2-INDEX) = ZERO                    00339700
339800             IF GSS2-HO-MM-OPEX-OVERRIDE-IND                      00339800
339900                         (GSS2-INDEX) = ZERO                      00339900
340000                     AND GSS2-HO-MM-OPEX-APPLIC-IND               00340000
340100                         (GSS2-INDEX) = ZERO                      00340100
340200                     AND GSS2-HO-MM-DEDU-APPLIC-IND               00340200
340300                         (GSS2-INDEX) = ZERO                      00340300
340400                     AND GSS2-HO-MM-IP-ALT-PRICING-METH           00340400
340500                         (GSS2-INDEX) = ZERO                      00340500
340600                     AND GSS2-HO-MM-OP-ALT-PRICING-METH           00340600
340700                         (GSS2-INDEX) = ZERO                      00340700
340800                 NEXT SENTENCE                                    00340800
340900             ELSE                                                 00340900
341000                 MOVE 'P22' TO WS-CURRENT-ERROR                   00341000
341100                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00341100
341200                                                                  00341200
341300                                                                  00341300
341400*** 14631    3/18/96   FRY   ADD PREFERRED ANCILLARY NETWORK (PA) 00341400
341500                                                                  00341500
341600     IF PA-ENTRY-FOUND                                            00341600
341700        SET GSS2-INDEX TO WS-PA-GSS2-INDEX                        00341700
341800        IF GSS2-PA-BC-IND (GSS2-INDEX)                   =  ZEROES00341800
341900         IF GSS2-PA-BC-CALC-METHOD (GSS2-INDEX)          =  ZEROES00341900
342000          AND GSS2-PA-BC-PAYMENT-LEVEL-IND (GSS2-INDEX)  =  ZEROES00342000
342100          AND GSS2-PA-BC-ALT-PRICING-METHOD (GSS2-INDEX) =  ZEROES00342100
342200              NEXT SENTENCE                                       00342200
342300         ELSE                                                     00342300
342400              MOVE 'P63' TO WS-CURRENT-ERROR                      00342400
342500              PERFORM 90000-POST-ERROR THRU 90000-EXIT.           00342500
342600                                                                  00342600
342700     IF PA-ENTRY-FOUND                                            00342700
342800        SET GSS2-INDEX TO WS-PA-GSS2-INDEX                        00342800
342900        IF GSS2-PA-BS-IND (GSS2-INDEX)                   =  ZEROES00342900
343000         IF GSS2-PA-BS-CALC-METHOD (GSS2-INDEX)          =  ZEROES00343000
343100          AND GSS2-PA-BS-PAYMENT-LEVEL-IND (GSS2-INDEX)  =  ZEROES00343100
343200          AND GSS2-PA-BS-ALT-PRICING-METHOD (GSS2-INDEX) =  ZEROES00343200
343300              NEXT SENTENCE                                       00343300
343400         ELSE                                                     00343400
343500              MOVE 'P64' TO WS-CURRENT-ERROR                      00343500
343600              PERFORM 90000-POST-ERROR THRU 90000-EXIT.           00343600
343700                                                                  00343700
343800     IF PA-ENTRY-FOUND                                            00343800
343900        SET GSS2-INDEX TO WS-PA-GSS2-INDEX                        00343900
344000        IF GSS2-PA-MM-IND (GSS2-INDEX)                   =  ZEROES00344000
344100         IF GSS2-PA-MM-CALC-METHOD (GSS2-INDEX)          =  ZEROES00344100
344200          AND GSS2-PA-MM-PAYMENT-LEVEL-IND (GSS2-INDEX)  =  ZEROES00344200
344300          AND GSS2-PA-MM-ALT-PRICING-METHOD (GSS2-INDEX) =  ZEROES00344300
344400              NEXT SENTENCE                                       00344400
344500         ELSE                                                     00344500
344600              MOVE 'P65' TO WS-CURRENT-ERROR                      00344600
344700              PERFORM 90000-POST-ERROR THRU 90000-EXIT.           00344700
344800                                                                  00344800
344900                                                                  00344900
345000     IF PR-ENTRY-FOUND                                            00345000
345100         SET GSS2-INDEX TO WS-PR-GSS2-INDEX                       00345100
345200         IF GSS2-PR-BC-IND (GSS2-INDEX) = ZERO                    00345200
345300             IF GSS2-PR-BC-OPEX-OVERRIDE-IND                      00345300
345400                         (GSS2-INDEX) = ZERO                      00345400
345500                     AND GSS2-PR-BC-OPEX-APPLIC-IND               00345500
345600                         (GSS2-INDEX) = ZERO                      00345600
345700                     AND GSS2-PR-BC-DEDU-APPLIC-IND               00345700
345800                         (GSS2-INDEX) = ZERO                      00345800
345900                     AND GSS2-PR-BC-CALC-METHOD                   00345900
346000                         (GSS2-INDEX) = ZERO                      00346000
346100                     AND GSS2-PR-BC-ALT-PRICING-METH              00346100
346200                         (GSS2-INDEX) = ZERO                      00346200
346300                     AND GSS2-PR-BC-PAYMENT-LEVEL-IND             00346300
346400                         (GSS2-INDEX) = ZERO                      00346400
346500                 NEXT SENTENCE                                    00346500
346600             ELSE                                                 00346600
346700                 MOVE 'P03' TO WS-CURRENT-ERROR                   00346700
346800                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00346800
346900                                                                  00346900
347000     IF PR-ENTRY-FOUND                                            00347000
347100         SET GSS2-INDEX TO WS-PR-GSS2-INDEX                       00347100
347200         IF GSS2-PR-BS-IND (GSS2-INDEX) = ZERO                    00347200
347300             IF GSS2-PR-BS-OPEX-OVERRIDE-IND                      00347300
347400                         (GSS2-INDEX) = ZERO                      00347400
347500                     AND GSS2-PR-BS-OPEX-APPLIC-IND               00347500
347600                         (GSS2-INDEX) = ZERO                      00347600
347700                     AND GSS2-PR-BS-DEDU-APPLIC-IND               00347700
347800                         (GSS2-INDEX) = ZERO                      00347800
347900                     AND GSS2-PR-BS-CALC-METHOD                   00347900
348000                         (GSS2-INDEX) = ZERO                      00348000
348100                     AND GSS2-PR-BS-ALT-PRICING-METH              00348100
348200                         (GSS2-INDEX) = ZERO                      00348200
348300                     AND GSS2-PR-BS-PAYMENT-LEVEL-IND             00348300
348400                         (GSS2-INDEX) = ZERO                      00348400
348500                 NEXT SENTENCE                                    00348500
348600             ELSE                                                 00348600
348700                 MOVE 'P13' TO WS-CURRENT-ERROR                   00348700
348800                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00348800
348900                                                                  00348900
349000     IF PR-ENTRY-FOUND                                            00349000
349100         SET GSS2-INDEX TO WS-PR-GSS2-INDEX                       00349100
349200         IF GSS2-PR-MM-IND (GSS2-INDEX) = ZERO                    00349200
349300             IF GSS2-PR-MM-OPEX-OVERRIDE-IND                      00349300
349400                         (GSS2-INDEX) = ZERO                      00349400
349500                     AND GSS2-PR-MM-OPEX-APPLIC-IND               00349500
349600                         (GSS2-INDEX) = ZERO                      00349600
349700                     AND GSS2-PR-MM-DEDU-APPLIC-IND               00349700
349800                         (GSS2-INDEX) = ZERO                      00349800
349900                     AND GSS2-PR-MM-CALC-METHOD                   00349900
350000                         (GSS2-INDEX) = ZERO                      00350000
350100                     AND GSS2-PR-MM-ALT-PRICING-METH              00350100
350200                         (GSS2-INDEX) = ZERO                      00350200
350300                     AND GSS2-PR-MM-PAYMENT-LEVEL-IND             00350300
350400                         (GSS2-INDEX) = ZERO                      00350400
350500                 NEXT SENTENCE                                    00350500
350600             ELSE                                                 00350600
350700                 MOVE 'P23' TO WS-CURRENT-ERROR                   00350700
350800                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00350800
350900                                                                  00350900
351000     IF MO-ENTRY-FOUND                                            00351000
351100         SET GSS2-INDEX TO WS-MO-GSS2-INDEX                       00351100
351200         IF GSS2-MO-BC-IND (GSS2-INDEX) = ZERO                    00351200
351300             IF GSS2-MO-BC-OPEX-OVERRIDE-IND                      00351300
351400                         (GSS2-INDEX) = ZERO                      00351400
351500                     AND GSS2-MO-BC-OPEX-APPLIC-IND               00351500
351600                         (GSS2-INDEX) = ZERO                      00351600
351700                     AND GSS2-MO-BC-DEDU-APPLIC-IND               00351700
351800                         (GSS2-INDEX) = ZERO                      00351800
351900                     AND GSS2-MO-BC-CALC-METHOD                   00351900
352000                         (GSS2-INDEX) = ZERO                      00352000
352100                     AND GSS2-MO-BC-IP-ALT-PRICING-METH           00352100
352200                         (GSS2-INDEX) = ZERO                      00352200
352300                     AND GSS2-MO-BC-OP-ALT-PRICING-METH           00352300
352400                         (GSS2-INDEX) = ZERO                      00352400
352500                 NEXT SENTENCE                                    00352500
352600             ELSE                                                 00352600
352700                 MOVE 'P04' TO WS-CURRENT-ERROR                   00352700
352800                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00352800
352900                                                                  00352900
353000     IF MO-ENTRY-FOUND                                            00353000
353100         SET GSS2-INDEX TO WS-MO-GSS2-INDEX                       00353100
353200         IF GSS2-MO-BS-IND (GSS2-INDEX) = ZERO                    00353200
353300             IF GSS2-MO-BS-OPEX-OVERRIDE-IND                      00353300
353400                         (GSS2-INDEX) = ZERO                      00353400
353500                     AND GSS2-MO-BS-OPEX-APPLIC-IND               00353500
353600                         (GSS2-INDEX) = ZERO                      00353600
353700                     AND GSS2-MO-BS-DEDU-APPLIC-IND               00353700
353800                         (GSS2-INDEX) = ZERO                      00353800
353900                     AND GSS2-MO-BS-CALC-METHOD                   00353900
354000                         (GSS2-INDEX) = ZERO                      00354000
354100                     AND GSS2-MO-BS-IP-ALT-PRICING-METH           00354100
354200                         (GSS2-INDEX) = ZERO                      00354200
354300                     AND GSS2-MO-BS-OP-ALT-PRICING-METH           00354300
354400                         (GSS2-INDEX) = ZERO                      00354400
354500                 NEXT SENTENCE                                    00354500
354600             ELSE                                                 00354600
354700                 MOVE 'P14' TO WS-CURRENT-ERROR                   00354700
354800                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00354800
354900                                                                  00354900
355000     IF MO-ENTRY-FOUND                                            00355000
355100         SET GSS2-INDEX TO WS-MO-GSS2-INDEX                       00355100
355200         IF GSS2-MO-MM-IND (GSS2-INDEX) = ZERO                    00355200
355300             IF GSS2-MO-MM-OPEX-OVERRIDE-IND                      00355300
355400                         (GSS2-INDEX) = ZERO                      00355400
355500                     AND GSS2-MO-MM-OPEX-APPLIC-IND               00355500
355600                         (GSS2-INDEX) = ZERO                      00355600
355700                     AND GSS2-MO-MM-DEDU-APPLIC-IND               00355700
355800                         (GSS2-INDEX) = ZERO                      00355800
355900                     AND GSS2-MO-MM-CALC-METHOD                   00355900
356000                         (GSS2-INDEX) = ZERO                      00356000
356100                     AND GSS2-MO-MM-IP-ALT-PRICING-METH           00356100
356200                         (GSS2-INDEX) = ZERO                      00356200
356300                     AND GSS2-MO-MM-OP-ALT-PRICING-METH           00356300
356400                         (GSS2-INDEX) = ZERO                      00356400
356500                 NEXT SENTENCE                                    00356500
356600             ELSE                                                 00356600
356700                 MOVE 'P24' TO WS-CURRENT-ERROR                   00356700
356800                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00356800
356900                                                                  00356900
357000     IF PP-ENTRY-FOUND                                            00357000
357100         SET GSS2-INDEX TO WS-PP-GSS2-INDEX                       00357100
357200         IF GSS2-PP-BC-IND (GSS2-INDEX) = ZERO                    00357200
357300             IF GSS2-PP-BC-OPEX-OVERRIDE-IND                      00357300
357400                         (GSS2-INDEX) = ZERO                      00357400
357500                     AND GSS2-PP-BC-OPEX-APPLIC-IND               00357500
357600                         (GSS2-INDEX) = ZERO                      00357600
357700                     AND GSS2-PP-BC-DEDUCT-APPLIC-IND             00357700
357800                         (GSS2-INDEX) = ZERO                      00357800
357900                     AND GSS2-PP-BC-CALC-METHOD                   00357900
358000                         (GSS2-INDEX) = ZERO                      00358000
358100                     AND GSS2-PP-BC-ALT-PRICING-METHOD            00358100
358200                         (GSS2-INDEX) = ZERO                      00358200
358300                 NEXT SENTENCE                                    00358300
358400             ELSE                                                 00358400
358500                 MOVE 'P05' TO WS-CURRENT-ERROR                   00358500
358600                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00358600
358700                                                                  00358700
358800     IF PP-ENTRY-FOUND                                            00358800
358900         SET GSS2-INDEX TO WS-PP-GSS2-INDEX                       00358900
359000         IF GSS2-PP-BS-IND (GSS2-INDEX) = ZERO                    00359000
359100             IF GSS2-PP-BS-OPEX-OVERRIDE-IND                      00359100
359200                         (GSS2-INDEX) = ZERO                      00359200
359300                     AND GSS2-PP-BS-OPEX-APPLIC-IND               00359300
359400                         (GSS2-INDEX) = ZERO                      00359400
359500                     AND GSS2-PP-BS-DEDUCT-APPLIC-IND             00359500
359600                         (GSS2-INDEX) = ZERO                      00359600
359700                     AND GSS2-PP-BS-CALC-METHOD                   00359700
359800                         (GSS2-INDEX) = ZERO                      00359800
359900                     AND GSS2-PP-BS-ALT-PRICING-METHOD            00359900
360000                         (GSS2-INDEX) = ZERO                      00360000
360100                 NEXT SENTENCE                                    00360100
360200             ELSE                                                 00360200
360300                 MOVE 'P15' TO WS-CURRENT-ERROR                   00360300
360400                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00360400
360500                                                                  00360500
360600     IF PP-ENTRY-FOUND                                            00360600
360700         SET GSS2-INDEX TO WS-PP-GSS2-INDEX                       00360700
360800         IF GSS2-PP-MM-IND (GSS2-INDEX) = ZERO                    00360800
360900             IF GSS2-PP-MM-OPEX-OVERRIDE-IND                      00360900
361000                         (GSS2-INDEX) = ZERO                      00361000
361100                     AND GSS2-PP-MM-OPEX-APPLIC-IND               00361100
361200                         (GSS2-INDEX) = ZERO                      00361200
361300                     AND GSS2-PP-MM-DEDUCT-APPLIC-IND             00361300
361400                         (GSS2-INDEX) = ZERO                      00361400
361500                     AND GSS2-PP-MM-CALC-METHOD                   00361500
361600                         (GSS2-INDEX) = ZERO                      00361600
361700                     AND GSS2-PP-MM-ALT-PRICING-METHOD            00361700
361800                         (GSS2-INDEX) = ZERO                      00361800
361900                 NEXT SENTENCE                                    00361900
362000             ELSE                                                 00362000
362100                 MOVE 'P25' TO WS-CURRENT-ERROR                   00362100
362200                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00362200
362300                                                                  00362300
362400     IF MA-ENTRY-FOUND                                            00362400
362500         SET GSS2-INDEX TO WS-MA-GSS2-INDEX                       00362500
362600         IF GSS2-MA-BC-IND (GSS2-INDEX) = ZERO                    00362600
362700             IF GSS2-MA-BC-OPEX-OVERRIDE-IND                      00362700
362800                         (GSS2-INDEX) = ZERO                      00362800
362900                     AND GSS2-MA-BC-OPEX-APPLIC-IND               00362900
363000                         (GSS2-INDEX) = ZERO                      00363000
363100                     AND GSS2-MA-BC-DEDU-APPLIC-IND               00363100
363200                         (GSS2-INDEX) = ZERO                      00363200
363300                     AND GSS2-MA-BC-CALC-METHOD                   00363300
363400                         (GSS2-INDEX) = ZERO                      00363400
363500                     AND GSS2-MA-BC-IP-ALT-PRICING-METH           00363500
363600                         (GSS2-INDEX) = ZERO                      00363600
363700                     AND GSS2-MA-BC-OP-ALT-PRICING-METH           00363700
363800                         (GSS2-INDEX) = ZERO                      00363800
363900                 NEXT SENTENCE                                    00363900
364000             ELSE                                                 00364000
364100                 MOVE 'P06' TO WS-CURRENT-ERROR                   00364100
364200                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00364200
364300                                                                  00364300
364400     IF MA-ENTRY-FOUND                                            00364400
364500         SET GSS2-INDEX TO WS-MA-GSS2-INDEX                       00364500
364600         IF GSS2-MA-BS-IND (GSS2-INDEX) = ZERO                    00364600
364700             IF GSS2-MA-BS-OPEX-OVERRIDE-IND                      00364700
364800                         (GSS2-INDEX) = ZERO                      00364800
364900                     AND GSS2-MA-BS-OPEX-APPLIC-IND               00364900
365000                         (GSS2-INDEX) = ZERO                      00365000
365100                     AND GSS2-MA-BS-DEDU-APPLIC-IND               00365100
365200                         (GSS2-INDEX) = ZERO                      00365200
365300                     AND GSS2-MA-BS-CALC-METHOD                   00365300
365400                         (GSS2-INDEX) = ZERO                      00365400
365500                     AND GSS2-MA-BS-IP-ALT-PRICING-METH           00365500
365600                         (GSS2-INDEX) = ZERO                      00365600
365700                     AND GSS2-MA-BS-OP-ALT-PRICING-METH           00365700
365800                         (GSS2-INDEX) = ZERO                      00365800
365900                 NEXT SENTENCE                                    00365900
366000             ELSE                                                 00366000
366100                 MOVE 'P16' TO WS-CURRENT-ERROR                   00366100
366200                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00366200
366300                                                                  00366300
366400     IF MA-ENTRY-FOUND                                            00366400
366500         SET GSS2-INDEX TO WS-MA-GSS2-INDEX                       00366500
366600         IF GSS2-MA-MM-IND (GSS2-INDEX) = ZERO                    00366600
366700             IF GSS2-MA-MM-OPEX-OVERRIDE-IND                      00366700
366800                         (GSS2-INDEX) = ZERO                      00366800
366900                     AND GSS2-MA-MM-OPEX-APPLIC-IND               00366900
367000                         (GSS2-INDEX) = ZERO                      00367000
367100                     AND GSS2-MA-MM-DEDU-APPLIC-IND               00367100
367200                         (GSS2-INDEX) = ZERO                      00367200
367300                     AND GSS2-MA-MM-CALC-METHOD                   00367300
367400                         (GSS2-INDEX) = ZERO                      00367400
367500                     AND GSS2-MA-MM-IP-ALT-PRICING-METH           00367500
367600                         (GSS2-INDEX) = ZERO                      00367600
367700                     AND GSS2-MA-MM-OP-ALT-PRICING-METH           00367700
367800                         (GSS2-INDEX) = ZERO                      00367800
367900                 NEXT SENTENCE                                    00367900
368000             ELSE                                                 00368000
368100                 MOVE 'P26' TO WS-CURRENT-ERROR                   00368100
368200                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00368200
368300                                                                  00368300
368400     IF PT-ENTRY-FOUND                                            00368400
368500         SET GSS2-INDEX TO WS-PT-GSS2-INDEX                       00368500
368600         IF GSS2-PT-BC-IND (GSS2-INDEX) = ZERO                    00368600
368700             IF GSS2-PT-BC-OPEX-OVERRIDE-IND                      00368700
368800                         (GSS2-INDEX) = ZERO                      00368800
368900                     AND GSS2-PT-BC-OPEX-APPLIC-IND               00368900
369000                         (GSS2-INDEX) = ZERO                      00369000
369100                     AND GSS2-PT-BC-DEDU-APPLIC-IND               00369100
369200                         (GSS2-INDEX) = ZERO                      00369200
369300                     AND GSS2-PT-BC-IP-ALT-PRIC-METH              00369300
369400                         (GSS2-INDEX) = ZERO                      00369400
369500                     AND GSS2-PT-BC-OP-ALT-PRIC-METH              00369500
369600                         (GSS2-INDEX) = ZERO                      00369600
369700                 NEXT SENTENCE                                    00369700
369800             ELSE                                                 00369800
369900                 MOVE 'P07' TO WS-CURRENT-ERROR                   00369900
370000                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00370000
370100                                                                  00370100
370200     IF PT-ENTRY-FOUND                                            00370200
370300         SET GSS2-INDEX TO WS-PT-GSS2-INDEX                       00370300
370400         IF GSS2-PT-BS-IND (GSS2-INDEX) = ZERO                    00370400
370500             IF GSS2-PT-BS-OPEX-OVERRIDE-IND                      00370500
370600                         (GSS2-INDEX) = ZERO                      00370600
370700                     AND GSS2-PT-BS-OPEX-APPLIC-IND               00370700
370800                         (GSS2-INDEX) = ZERO                      00370800
370900                     AND GSS2-PT-BS-DEDU-APPLIC-IND               00370900
371000                         (GSS2-INDEX) = ZERO                      00371000
371100                     AND GSS2-PT-BS-IP-ALT-PRIC-METH              00371100
371200                         (GSS2-INDEX) = ZERO                      00371200
371300                     AND GSS2-PT-BS-OP-ALT-PRIC-METH              00371300
371400                         (GSS2-INDEX) = ZERO                      00371400
371500                 NEXT SENTENCE                                    00371500
371600             ELSE                                                 00371600
371700                 MOVE 'P17' TO WS-CURRENT-ERROR                   00371700
371800                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00371800
371900                                                                  00371900
372000     IF PT-ENTRY-FOUND                                            00372000
372100         SET GSS2-INDEX TO WS-PT-GSS2-INDEX                       00372100
372200         IF GSS2-PT-MM-IND (GSS2-INDEX) = ZERO                    00372200
372300             IF GSS2-PT-MM-OPEX-OVERRIDE-IND                      00372300
372400                         (GSS2-INDEX) = ZERO                      00372400
372500                     AND GSS2-PT-MM-OPEX-APPLIC-IND               00372500
372600                         (GSS2-INDEX) = ZERO                      00372600
372700                     AND GSS2-PT-MM-DEDU-APPLIC-IND               00372700
372800                         (GSS2-INDEX) = ZERO                      00372800
372900                     AND GSS2-PT-MM-IP-ALT-PRIC-METH              00372900
373000                         (GSS2-INDEX) = ZERO                      00373000
373100                     AND GSS2-PT-MM-OP-ALT-PRIC-METH              00373100
373200                         (GSS2-INDEX) = ZERO                      00373200
373300                 NEXT SENTENCE                                    00373300
373400             ELSE                                                 00373400
373500                 MOVE 'P27' TO WS-CURRENT-ERROR                   00373500
373600                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00373600
373700                                                                  00373700
373800                                                                  00373800
373900* POINT OF SERVICE (NEW POS)                                      00373900
374000* D11836   FRY  6/6/91    MESSAGES:  'P45', 'P46', 'P47'          00374000
374100                                                                  00374100
374200     IF P1-ENTRY-FOUND                                            00374200
374300         SET GSS2-INDEX TO WS-P1-GSS2-INDEX                       00374300
374400         IF GSS2-P1-BC-IND (GSS2-INDEX) = ZERO                    00374400
374500             IF GSS2-P1-BC-PAYMENT-LEVEL-IND                      00374500
374600                         (GSS2-INDEX) = ZERO                      00374600
374700                     AND GSS2-P1-BC-CALC-METHOD                   00374700
374800                         (GSS2-INDEX) = ZERO                      00374800
374900                 NEXT SENTENCE                                    00374900
375000             ELSE                                                 00375000
375100                 MOVE 'P45' TO WS-CURRENT-ERROR                   00375100
375200                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00375200
375300                                                                  00375300
375400     IF P1-ENTRY-FOUND                                            00375400
375500         SET GSS2-INDEX TO WS-P1-GSS2-INDEX                       00375500
375600         IF GSS2-P1-BS-IND (GSS2-INDEX) = ZERO                    00375600
375700             IF GSS2-P1-BS-PAYMENT-LEVEL-IND                      00375700
375800                         (GSS2-INDEX) = ZERO                      00375800
375900                     AND GSS2-P1-BS-ALT-PRIC-METH                 00375900
376000                         (GSS2-INDEX) = ZERO                      00376000
376100                     AND GSS2-P1-BS-CALC-METHOD                   00376100
376200                         (GSS2-INDEX) = ZERO                      00376200
376300                 NEXT SENTENCE                                    00376300
376400             ELSE                                                 00376400
376500                 MOVE 'P46' TO WS-CURRENT-ERROR                   00376500
376600                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00376600
376700                                                                  00376700
376800     IF P1-ENTRY-FOUND                                            00376800
376900         SET GSS2-INDEX TO WS-P1-GSS2-INDEX                       00376900
377000         IF GSS2-P1-MM-IND (GSS2-INDEX) = ZERO                    00377000
377100             IF GSS2-P1-MM-PAYMENT-LEVEL-IND                      00377100
377200                         (GSS2-INDEX) = ZERO                      00377200
377300                     AND GSS2-P1-MM-CALC-METHOD                   00377300
377400                         (GSS2-INDEX) = ZERO                      00377400
377500                 NEXT SENTENCE                                    00377500
377600             ELSE                                                 00377600
377700                 MOVE 'P47' TO WS-CURRENT-ERROR                   00377700
377800                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00377800
377900                                                                  00377900
378000                                                                  00378000
378100* HMO MANAGED CARE (HM)                                           00378100
378200* D-351  GST  9/11/2000   MESSAGES:  'P66', 'P67', 'P68'          00378200
378300                                                                  00378300
378400     IF HM-ENTRY-FOUND                                            00378400
378500         SET GSS2-INDEX TO WS-HM-GSS2-INDEX                       00378500
378600         IF GSS2-HM-BC-IND (GSS2-INDEX) = ZERO                    00378600
378700             IF GSS2-HM-BC-PAYMENT-LEVEL-IND                      00378700
378800                         (GSS2-INDEX) = ZERO                      00378800
378900                     AND GSS2-HM-BC-CALC-METHOD                   00378900
379000                         (GSS2-INDEX) = ZERO                      00379000
379100                 NEXT SENTENCE                                    00379100
379200             ELSE                                                 00379200
379300                 MOVE 'P66' TO WS-CURRENT-ERROR                   00379300
379400                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00379400
379500                                                                  00379500
379600     IF HM-ENTRY-FOUND                                            00379600
379700         SET GSS2-INDEX TO WS-HM-GSS2-INDEX                       00379700
379800         IF GSS2-HM-BS-IND (GSS2-INDEX) = ZERO                    00379800
379900             IF GSS2-HM-BS-PAYMENT-LEVEL-IND                      00379900
380000                         (GSS2-INDEX) = ZERO                      00380000
380100                     AND GSS2-HM-BS-ALT-PRIC-METH                 00380100
380200                         (GSS2-INDEX) = ZERO                      00380200
380300                     AND GSS2-HM-BS-CALC-METHOD                   00380300
380400                         (GSS2-INDEX) = ZERO                      00380400
380500                 NEXT SENTENCE                                    00380500
380600             ELSE                                                 00380600
380700                 MOVE 'P67' TO WS-CURRENT-ERROR                   00380700
380800                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00380800
380900                                                                  00380900
381000     IF HM-ENTRY-FOUND                                            00381000
381100         SET GSS2-INDEX TO WS-HM-GSS2-INDEX                       00381100
381200         IF GSS2-HM-MM-IND (GSS2-INDEX) = ZERO                    00381200
381300             IF GSS2-HM-MM-PAYMENT-LEVEL-IND                      00381300
381400                         (GSS2-INDEX) = ZERO                      00381400
381500                     AND GSS2-HM-MM-CALC-METHOD                   00381500
381600                         (GSS2-INDEX) = ZERO                      00381600
381700                 NEXT SENTENCE                                    00381700
381800             ELSE                                                 00381800
381900                 MOVE 'P68' TO WS-CURRENT-ERROR                   00381900
382000                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00382000
382100                                                                  00382100
382200                                                                  00382200
382300     IF MD-ENTRY-FOUND                                            00382300
382400         SET GSS2-INDEX TO WS-MD-GSS2-INDEX                       00382400
382500         IF GSS2-MD-BC-IND (GSS2-INDEX) = ZERO                    00382500
382600             IF GSS2-MD-BC-OPEX-OVERRIDE-IND                      00382600
382700                         (GSS2-INDEX) = ZERO                      00382700
382800                     AND GSS2-MD-BC-OPEX-APPLIC-IND               00382800
382900                         (GSS2-INDEX) = ZERO                      00382900
383000                     AND GSS2-MD-BC-DEDU-APPLIC-IND               00383000
383100                         (GSS2-INDEX) = ZERO                      00383100
383200                     AND GSS2-MD-BC-CALC-METHOD                   00383200
383300                         (GSS2-INDEX) = ZERO                      00383300
383400                 NEXT SENTENCE                                    00383400
383500             ELSE                                                 00383500
383600                 MOVE 'P08' TO WS-CURRENT-ERROR                   00383600
383700                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00383700
383800                                                                  00383800
383900     IF MD-ENTRY-FOUND                                            00383900
384000         SET GSS2-INDEX TO WS-MD-GSS2-INDEX                       00384000
384100         IF GSS2-MD-BS-IND (GSS2-INDEX) = ZERO                    00384100
384200             IF GSS2-MD-BS-OPEX-OVERRIDE-IND                      00384200
384300                         (GSS2-INDEX) = ZERO                      00384300
384400                     AND GSS2-MD-BS-OPEX-APPLIC-IND               00384400
384500                         (GSS2-INDEX) = ZERO                      00384500
384600                     AND GSS2-MD-BS-DEDU-APPLIC-IND               00384600
384700                         (GSS2-INDEX) = ZERO                      00384700
384800                     AND GSS2-MD-BS-CALC-METHOD                   00384800
384900                         (GSS2-INDEX) = ZERO                      00384900
385000                 NEXT SENTENCE                                    00385000
385100             ELSE                                                 00385100
385200                 MOVE 'P18' TO WS-CURRENT-ERROR                   00385200
385300                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00385300
385400                                                                  00385400
385500     IF MD-ENTRY-FOUND                                            00385500
385600         SET GSS2-INDEX TO WS-MD-GSS2-INDEX                       00385600
385700         IF GSS2-MD-MM-IND (GSS2-INDEX) = ZERO                    00385700
385800             IF GSS2-MD-MM-OPEX-OVERRIDE-IND                      00385800
385900                         (GSS2-INDEX) = ZERO                      00385900
386000                     AND GSS2-MD-MM-OPEX-APPLIC-IND               00386000
386100                         (GSS2-INDEX) = ZERO                      00386100
386200                     AND GSS2-MD-MM-DEDU-APPLIC-IND               00386200
386300                         (GSS2-INDEX) = ZERO                      00386300
386400                     AND GSS2-MD-MM-CALC-METHOD                   00386400
386500                         (GSS2-INDEX) = ZERO                      00386500
386600                 NEXT SENTENCE                                    00386600
386700             ELSE                                                 00386700
386800                 MOVE 'P28' TO WS-CURRENT-ERROR                   00386800
386900                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00386900
387000                                                                  00387000
387100     IF MS-ENTRY-FOUND                                            00387100
387200         SET GSS2-INDEX TO WS-MS-GSS2-INDEX                       00387200
387300         IF GSS2-MS-BC-IND (GSS2-INDEX) = ZERO                    00387300
387400             IF GSS2-MS-BC-OPEX-OVERRIDE-IND                      00387400
387500                         (GSS2-INDEX) = ZERO                      00387500
387600                     AND GSS2-MS-BC-OPEX-APPLIC-IND               00387600
387700                         (GSS2-INDEX) = ZERO                      00387700
387800                     AND GSS2-MS-BC-DEDU-APPLIC-IND               00387800
387900                         (GSS2-INDEX) = ZERO                      00387900
388000                 NEXT SENTENCE                                    00388000
388100             ELSE                                                 00388100
388200                 MOVE 'P09' TO WS-CURRENT-ERROR                   00388200
388300                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00388300
388400                                                                  00388400
388500     IF MS-ENTRY-FOUND                                            00388500
388600         SET GSS2-INDEX TO WS-MS-GSS2-INDEX                       00388600
388700         IF GSS2-MS-BS-IND (GSS2-INDEX) = ZERO                    00388700
388800             IF GSS2-MS-BS-OPEX-OVERRIDE-IND                      00388800
388900                         (GSS2-INDEX) = ZERO                      00388900
389000                     AND GSS2-MS-BS-OPEX-APPLIC-IND               00389000
389100                         (GSS2-INDEX) = ZERO                      00389100
389200                     AND GSS2-MS-BS-DEDU-APPLIC-IND               00389200
389300                         (GSS2-INDEX) = ZERO                      00389300
389400                 NEXT SENTENCE                                    00389400
389500             ELSE                                                 00389500
389600                 MOVE 'P19' TO WS-CURRENT-ERROR                   00389600
389700                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00389700
389800                                                                  00389800
389900     IF MS-ENTRY-FOUND                                            00389900
390000         SET GSS2-INDEX TO WS-MS-GSS2-INDEX                       00390000
390100         IF GSS2-MS-MM-IND (GSS2-INDEX) = ZERO                    00390100
390200             IF GSS2-MS-MM-OPEX-OVERRIDE-IND                      00390200
390300                         (GSS2-INDEX) = ZERO                      00390300
390400                     AND GSS2-MS-MM-OPEX-APPLIC-IND               00390400
390500                         (GSS2-INDEX) = ZERO                      00390500
390600                     AND GSS2-MS-MM-DEDU-APPLIC-IND               00390600
390700                         (GSS2-INDEX) = ZERO                      00390700
390800                 NEXT SENTENCE                                    00390800
390900             ELSE                                                 00390900
391000                 MOVE 'P29' TO WS-CURRENT-ERROR                   00391000
391100                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00391100
391200                                                                  00391200
391300     IF MN-ENTRY-FOUND                                            00391300
391400         SET GSS2-INDEX TO WS-MN-GSS2-INDEX                       00391400
391500         IF GSS2-MN-BC-IND (GSS2-INDEX) = ZERO                    00391500
391600             IF GSS2-MN-BC-PAYMENT-IND (GSS2-INDEX) = ZERO        00391600
391700                 NEXT SENTENCE                                    00391700
391800             ELSE                                                 00391800
391900                 MOVE 'P10' TO WS-CURRENT-ERROR                   00391900
392000                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00392000
392100                                                                  00392100
392200     IF MN-ENTRY-FOUND                                            00392200
392300         SET GSS2-INDEX TO WS-MN-GSS2-INDEX                       00392300
392400         IF GSS2-MN-BS-IND (GSS2-INDEX) = ZERO                    00392400
392500             IF GSS2-MN-BS-PAYMENT-IND (GSS2-INDEX) = ZERO        00392500
392600                 NEXT SENTENCE                                    00392600
392700             ELSE                                                 00392700
392800                 MOVE 'P20' TO WS-CURRENT-ERROR                   00392800
392900                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00392900
393000                                                                  00393000
393100     IF MN-ENTRY-FOUND                                            00393100
393200         SET GSS2-INDEX TO WS-MN-GSS2-INDEX                       00393200
393300         IF GSS2-MN-MM-IND (GSS2-INDEX) = ZERO                    00393300
393400             IF GSS2-MN-MM-PAYMENT-IND (GSS2-INDEX) = ZERO        00393400
393500                 NEXT SENTENCE                                    00393500
393600             ELSE                                                 00393600
393700                 MOVE 'P30' TO WS-CURRENT-ERROR                   00393700
393800                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00393800
393900                                                                  00393900
394000                                                                  00394000
394100*--- 12730   12/06/92  FRY   ADD RESTRICTED PROVIDER OPTION (RP)  00394100
394200                                                                  00394200
394300     IF RP-ENTRY-FOUND                                            00394300
394400         SET GSS2-INDEX TO WS-RP-GSS2-INDEX                       00394400
394500         IF GSS2-RP-BC-IND (GSS2-INDEX) = ZEROES                  00394500
394600             IF GSS2-RP-BC-CALC-METHOD                            00394600
394700                         (GSS2-INDEX) = ZEROES                    00394700
394800                     AND GSS2-RP-BC-PAYMENT-LEVEL-IND             00394800
394900                         (GSS2-INDEX) = ZEROES                    00394900
395000                 NEXT SENTENCE                                    00395000
395100             ELSE                                                 00395100
395200                 MOVE 'P48' TO WS-CURRENT-ERROR                   00395200
395300                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00395300
395400                                                                  00395400
395500     IF RP-ENTRY-FOUND                                            00395500
395600         SET GSS2-INDEX TO WS-RP-GSS2-INDEX                       00395600
395700         IF GSS2-RP-BS-IND (GSS2-INDEX) = ZEROES                  00395700
395800            IF GSS2-RP-BS-CALC-METHOD                             00395800
395900                         (GSS2-INDEX)   = ZEROES                  00395900
396000                     AND GSS2-RP-BS-PAYMENT-LEVEL-IND             00396000
396100                         (GSS2-INDEX)   = ZEROES                  00396100
396200                     AND GSS2-RP-BS-ALT-PRICING-METHOD            00396200
396300                         (GSS2-INDEX)   = ZEROES                  00396300
396400                 NEXT SENTENCE                                    00396400
396500             ELSE                                                 00396500
396600                 MOVE 'P49' TO WS-CURRENT-ERROR                   00396600
396700                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00396700
396800                                                                  00396800
396900     IF RP-ENTRY-FOUND                                            00396900
397000         SET GSS2-INDEX TO WS-RP-GSS2-INDEX                       00397000
397100         IF GSS2-RP-MM-IND (GSS2-INDEX) = ZEROES                  00397100
397200             IF GSS2-RP-MM-CALC-METHOD                            00397200
397300                         (GSS2-INDEX)   = ZEROES                  00397300
397400                     AND GSS2-RP-MM-PAYMENT-LEVEL-IND             00397400
397500                         (GSS2-INDEX)   = ZEROES                  00397500
397600                     AND GSS2-RP-MM-ALT-PRICING-METHOD            00397600
397700                         (GSS2-INDEX)   = ZEROES                  00397700
397800                 NEXT SENTENCE                                    00397800
397900             ELSE                                                 00397900
398000                 MOVE 'P50' TO WS-CURRENT-ERROR                   00398000
398100                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00398100
398200                                                                  00398200
398300                                                                  00398300
398400*--- D15380  03/24/99  GDM   ADD BLUE ADVANTAGE ENTREPRENEUR (BA) 00398400
398500                                                                  00398500
398600     IF BA-ENTRY-FOUND                                            00398600
398700         SET GSS2-INDEX TO WS-BA-GSS2-INDEX                       00398700
398800         IF GSS2-BA-BC-IND (GSS2-INDEX) = ZEROES                  00398800
398900             IF GSS2-BA-BC-CALC-METHOD                            00398900
399000                         (GSS2-INDEX) = ZEROES                    00399000
399100                     AND GSS2-BA-BC-PAYMENT-LEVEL-IND             00399100
399200                         (GSS2-INDEX) = ZEROES                    00399200
399300                 NEXT SENTENCE                                    00399300
399400             ELSE                                                 00399400
399500                 MOVE 'P48' TO WS-CURRENT-ERROR                   00399500
399600                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00399600
399700                                                                  00399700
399800     IF BA-ENTRY-FOUND                                            00399800
399900         SET GSS2-INDEX TO WS-BA-GSS2-INDEX                       00399900
400000         IF GSS2-BA-BS-IND (GSS2-INDEX) = ZEROES                  00400000
400100            IF GSS2-BA-BS-CALC-METHOD                             00400100
400200                         (GSS2-INDEX)   = ZEROES                  00400200
400300                     AND GSS2-BA-BS-PAYMENT-LEVEL-IND             00400300
400400                         (GSS2-INDEX)   = ZEROES                  00400400
400500                     AND GSS2-BA-BS-ALT-PRICING-METHOD            00400500
400600                         (GSS2-INDEX)   = ZEROES                  00400600
400700                 NEXT SENTENCE                                    00400700
400800             ELSE                                                 00400800
400900                 MOVE 'P49' TO WS-CURRENT-ERROR                   00400900
401000                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00401000
401100                                                                  00401100
401200     IF BA-ENTRY-FOUND                                            00401200
401300         SET GSS2-INDEX TO WS-BA-GSS2-INDEX                       00401300
401400         IF GSS2-BA-MM-IND (GSS2-INDEX) = ZEROES                  00401400
401500             IF GSS2-BA-MM-CALC-METHOD                            00401500
401600                         (GSS2-INDEX)   = ZEROES                  00401600
401700                     AND GSS2-BA-MM-PAYMENT-LEVEL-IND             00401700
401800                         (GSS2-INDEX)   = ZEROES                  00401800
401900                     AND GSS2-BA-MM-ALT-PRICING-METHOD            00401900
402000                         (GSS2-INDEX)   = ZEROES                  00402000
402100                 NEXT SENTENCE                                    00402100
402200             ELSE                                                 00402200
402300                 MOVE 'P50' TO WS-CURRENT-ERROR                   00402300
402400                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00402400
402500                                                                  00402500
402600**D11530  (RS)  REIMBURSEMENT/SUBROGATION      FRY 7/13/95        00402600
402700***                                                               00402700
402800     IF RS-ENTRY-FOUND                                            00402800
402900        SET GSS2-INDEX TO WS-RS-GSS2-INDEX                        00402900
403000        IF  GSS2-RS-BC-IND (GSS2-INDEX)  NOT =  ZEROES            00403000
403100          IF GSS2-RS-BC-INVEST-METHOD (GSS2-INDEX)  = '1'         00403100
403200            IF GSS2-RS-BC-DENIAL-PARAMETER (GSS2-INDEX)  = ZEROES 00403200
403300               NEXT SENTENCE                                      00403300
403400          ELSE                                                    00403400
403500             MOVE 'P34' TO WS-CURRENT-ERROR                       00403500
403600             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00403600
403700                                                                  00403700
403800     IF RS-ENTRY-FOUND                                            00403800
403900        SET GSS2-INDEX TO WS-RS-GSS2-INDEX                        00403900
404000        IF  GSS2-RS-BS-IND (GSS2-INDEX)  NOT =  ZEROES            00404000
404100          IF GSS2-RS-BS-INVEST-METHOD (GSS2-INDEX)  = '1'         00404100
404200            IF GSS2-RS-BS-DENIAL-PARAMETER (GSS2-INDEX)  = ZEROES 00404200
404300               NEXT SENTENCE                                      00404300
404400          ELSE                                                    00404400
404500             MOVE 'P34' TO WS-CURRENT-ERROR                       00404500
404600             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00404600
404700                                                                  00404700
404800                                                                  00404800
404900     IF RS-ENTRY-FOUND                                            00404900
405000        SET GSS2-INDEX TO WS-RS-GSS2-INDEX                        00405000
405100        IF  GSS2-RS-BS-IND (GSS2-INDEX)  NOT =  ZEROES            00405100
405200          OR  GSS2-RS-BS-IND (GSS2-INDEX)  NOT =  ZEROES          00405200
405300        IF (GSS2-RS-RESPONSIBILITY-IND (GSS2-INDEX)      NOT =  0 00405300
405400        AND  GSS2-RS-MEMB-RELATIONSHIP-IND (GSS2-INDEX)  NOT =  0 00405400
405500        AND  GSS2-RS-BIT-IND (GSS2-INDEX)                NOT =  0 00405500
405600        AND  GSS2-RS-DOC-FREQUENCY-IND (GSS2-INDEX)      NOT =  0 00405600
405700        AND  GSS2-RS-DOC-TEXT-IND (GSS2-INDEX)           NOT =  0)00405700
405800             NEXT SENTENCE                                        00405800
405900          ELSE                                                    00405900
406000             MOVE 'P35' TO WS-CURRENT-ERROR                       00406000
406100             PERFORM 90000-POST-ERROR THRU 90000-EXIT.            00406100
406200                                                                  00406200
406300                                                                  00406300
406400     IF RS-ENTRY-FOUND                                            00406400
406500        SET GSS2-INDEX TO WS-RS-GSS2-INDEX                        00406500
406600        IF  GSS2-RS-BS-IND (GSS2-INDEX)  NOT =  ZEROES            00406600
406700          IF  GSS2-RS-BC-IND (GSS2-INDEX)  NOT =  ZEROES          00406700
406800            IF GSS2-RS-BS-INI-DOL-QUAL (GSS2-INDEX)  =            00406800
406900                          GSS2-RS-BC-INI-DOL-QUAL (GSS2-INDEX)    00406900
407000               NEXT SENTENCE                                      00407000
407100             ELSE                                                 00407100
407200               MOVE 'P54' TO WS-CURRENT-ERROR                     00407200
407300               PERFORM 90000-POST-ERROR THRU 90000-EXIT.          00407300
407400                                                                  00407400
407500                                                                  00407500
407600     IF RS-ENTRY-FOUND                                            00407600
407700        SET GSS2-INDEX TO WS-RS-GSS2-INDEX                        00407700
407800        IF  GSS2-RS-BS-IND (GSS2-INDEX)  NOT =  ZEROES            00407800
407900          IF  GSS2-RS-BC-IND (GSS2-INDEX)  NOT =  ZEROES          00407900
408000            IF GSS2-RS-BS-SUB-DOL-QUAL (GSS2-INDEX)  =            00408000
408100                          GSS2-RS-BC-SUB-DOL-QUAL (GSS2-INDEX)    00408100
408200               NEXT SENTENCE                                      00408200
408300             ELSE                                                 00408300
408400               MOVE 'P55' TO WS-CURRENT-ERROR                     00408400
408500               PERFORM 90000-POST-ERROR THRU 90000-EXIT.          00408500
408600                                                                  00408600
408700                                                                  00408700
408800     IF RS-ENTRY-FOUND                                            00408800
408900        SET GSS2-INDEX TO WS-RS-GSS2-INDEX                        00408900
409000        IF GSS2-RS-ADMIN-FEE-PERCENT (GSS2-INDEX)   >   +999      00409000
409100           MOVE 'P56' TO WS-CURRENT-ERROR                         00409100
409200           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00409200
409300                                                                  00409300
409400     IF RS-ENTRY-FOUND                                            00409400
409500        SET GSS2-INDEX TO WS-RS-GSS2-INDEX                        00409500
409600        IF  GSS2-RS-RESPONSIBILITY-IND (GSS2-INDEX)               00409600
409700                                                  = '1' OR '2'    00409700
409800          IF  GSS2-RS-DOC-FREQUENCY-IND (GSS2-INDEX)  > ZEROES    00409800
409900            IF  GSS2-RS-DOC-TEXT-IND (GSS2-INDEX)     > ZEROES    00409900
410000               NEXT SENTENCE                                      00410000
410100             ELSE                                                 00410100
410200               MOVE 'P57' TO WS-CURRENT-ERROR                     00410200
410300               PERFORM 90000-POST-ERROR THRU 90000-EXIT.          00410300
410400                                                                  00410400
410500                                                                  00410500
410600     IF RS-ENTRY-FOUND                                            00410600
410700        SET GSS2-INDEX TO WS-RS-GSS2-INDEX                        00410700
410800        IF  GSS2-RS-RESPONSIBILITY-IND (GSS2-INDEX)               00410800
410900                                             NOT =   '1' OR '2'   00410900
411000          IF  GSS2-RS-DOC-FREQUENCY-IND (GSS2-INDEX)  =  ZEROES   00411000
411100            IF  GSS2-RS-DOC-TEXT-IND (GSS2-INDEX)     =  ZEROES   00411100
411200               NEXT SENTENCE                                      00411200
411300             ELSE                                                 00411300
411400               MOVE 'P58' TO WS-CURRENT-ERROR                     00411400
411500               PERFORM 90000-POST-ERROR THRU 90000-EXIT.          00411500
411600                                                                  00411600
411700                                                                  00411700
411800     IF IO-ENTRY-FOUND                                            00411800
411900         SET GSS2-INDEX TO WS-IO-GSS2-INDEX                       00411900
412000         IF GSS2-IO-BC-CALC-METHOD (GSS2-INDEX) NOT = 0           00412000
412100            IF GSS2-IO-BC-IND (GSS2-INDEX) NOT = 0                00412100
412200               NEXT SENTENCE                                      00412200
412300            ELSE                                                  00412300
412400               MOVE 'P36' TO WS-CURRENT-ERROR                     00412400
412500               PERFORM 90000-POST-ERROR THRU 90000-EXIT.          00412500
412600                                                                  00412600
412700     IF IO-ENTRY-FOUND                                            00412700
412800         SET GSS2-INDEX TO WS-IO-GSS2-INDEX                       00412800
412900         IF GSS2-IO-BS-CALC-METHOD (GSS2-INDEX) NOT = 0           00412900
413000            IF GSS2-IO-BS-IND (GSS2-INDEX) NOT = 0                00413000
413100               NEXT SENTENCE                                      00413100
413200            ELSE                                                  00413200
413300               MOVE 'P37' TO WS-CURRENT-ERROR                     00413300
413400               PERFORM 90000-POST-ERROR THRU 90000-EXIT.          00413400
413500                                                                  00413500
413600     IF IO-ENTRY-FOUND                                            00413600
413700         SET GSS2-INDEX TO WS-IO-GSS2-INDEX                       00413700
413800         IF GSS2-IO-MM-CALC-METHOD (GSS2-INDEX) NOT = 0           00413800
413900            IF GSS2-IO-MM-IND (GSS2-INDEX) NOT = 0                00413900
414000               NEXT SENTENCE                                      00414000
414100            ELSE                                                  00414100
414200               MOVE 'P38' TO WS-CURRENT-ERROR                     00414200
414300               PERFORM 90000-POST-ERROR THRU 90000-EXIT.          00414300
414400                                                                  00414400
414500     IF SA-ENTRY-FOUND                                            00414500
414600         SET GSS2-INDEX TO WS-SA-GSS2-INDEX                       00414600
414700         IF GSS2-SA-BC-IND (GSS2-INDEX) = ZERO                    00414700
414800             IF GSS2-SA-BC-OPEX-OVERRIDE-IND                      00414800
414900                         (GSS2-INDEX) = ZERO                      00414900
415000                     AND GSS2-SA-BC-OPEX-APPLIC-IND               00415000
415100                         (GSS2-INDEX) = ZERO                      00415100
415200                     AND GSS2-SA-BC-DEDU-APPLIC-IND               00415200
415300                         (GSS2-INDEX) = ZERO                      00415300
415400                 NEXT SENTENCE                                    00415400
415500             ELSE                                                 00415500
415600                 MOVE 'P31' TO WS-CURRENT-ERROR                   00415600
415700                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00415700
415800                                                                  00415800
415900     IF SA-ENTRY-FOUND                                            00415900
416000         SET GSS2-INDEX TO WS-SA-GSS2-INDEX                       00416000
416100         IF GSS2-SA-BS-IND (GSS2-INDEX) = ZERO                    00416100
416200             IF GSS2-SA-BS-OPEX-OVERRIDE-IND                      00416200
416300                         (GSS2-INDEX) = ZERO                      00416300
416400                     AND GSS2-SA-BS-OPEX-APPLIC-IND               00416400
416500                         (GSS2-INDEX) = ZERO                      00416500
416600                     AND GSS2-SA-BS-DEDU-APPLIC-IND               00416600
416700                         (GSS2-INDEX) = ZERO                      00416700
416800                 NEXT SENTENCE                                    00416800
416900             ELSE                                                 00416900
417000                 MOVE 'P32' TO WS-CURRENT-ERROR                   00417000
417100                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00417100
417200                                                                  00417200
417300     IF SA-ENTRY-FOUND                                            00417300
417400         SET GSS2-INDEX TO WS-SA-GSS2-INDEX                       00417400
417500         IF GSS2-SA-MM-IND (GSS2-INDEX) = ZERO                    00417500
417600             IF GSS2-SA-MM-OPEX-OVERRIDE-IND                      00417600
417700                         (GSS2-INDEX) = ZERO                      00417700
417800                     AND GSS2-SA-MM-OPEX-APPLIC-IND               00417800
417900                         (GSS2-INDEX) = ZERO                      00417900
418000                     AND GSS2-SA-MM-DEDU-APPLIC-IND               00418000
418100                         (GSS2-INDEX) = ZERO                      00418100
418200                 NEXT SENTENCE                                    00418200
418300             ELSE                                                 00418300
418400                 MOVE 'P33' TO WS-CURRENT-ERROR                   00418400
418500                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00418500
418600                                                                  00418600
418700                                                                  00418700
418800* MENTAL HEALTH SUBSTANCE ABUSE CARE (MHSC)                       00418800
418900* D11836   FRY  6/6/91    MESSAGES:  'P42', 'P43', 'P44'          00418900
419000                                                                  00419000
419100     IF S1-ENTRY-FOUND                                            00419100
419200         SET GSS2-INDEX TO WS-S1-GSS2-INDEX                       00419200
419300         IF GSS2-S1-BC-IND (GSS2-INDEX) = ZERO                    00419300
419400             IF GSS2-S1-BC-PAYMENT-LEVEL-IND                      00419400
419500                         (GSS2-INDEX) = ZERO                      00419500
419600                     AND GSS2-S1-BC-CALC-METHOD                   00419600
419700                         (GSS2-INDEX) = ZERO                      00419700
419800                 NEXT SENTENCE                                    00419800
419900             ELSE                                                 00419900
420000                 MOVE 'P42' TO WS-CURRENT-ERROR                   00420000
420100                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00420100
420200                                                                  00420200
420300     IF S1-ENTRY-FOUND                                            00420300
420400         SET GSS2-INDEX TO WS-S1-GSS2-INDEX                       00420400
420500         IF GSS2-S1-BS-IND (GSS2-INDEX) = ZERO                    00420500
420600             IF GSS2-S1-BS-PAYMENT-LEVEL-IND                      00420600
420700                         (GSS2-INDEX) = ZERO                      00420700
420800                     AND GSS2-S1-BS-CALC-METHOD                   00420800
420900                         (GSS2-INDEX) = ZERO                      00420900
421000                     AND GSS2-S1-BS-ALT-PRICING-METHOD            00421000
421100                         (GSS2-INDEX) = ZERO                      00421100
421200                 NEXT SENTENCE                                    00421200
421300             ELSE                                                 00421300
421400                 MOVE 'P43' TO WS-CURRENT-ERROR                   00421400
421500                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00421500
421600                                                                  00421600
421700     IF S1-ENTRY-FOUND                                            00421700
421800         SET GSS2-INDEX TO WS-S1-GSS2-INDEX                       00421800
421900         IF GSS2-S1-MM-IND (GSS2-INDEX) = ZERO                    00421900
422000             IF GSS2-S1-MM-PAYMENT-LEVEL-IND                      00422000
422100                         (GSS2-INDEX) = ZERO                      00422100
422200                     AND GSS2-S1-MM-CALC-METHOD                   00422200
422300                         (GSS2-INDEX) = ZERO                      00422300
422400                 NEXT SENTENCE                                    00422400
422500             ELSE                                                 00422500
422600                 MOVE 'P44' TO WS-CURRENT-ERROR                   00422600
422700                 PERFORM 90000-POST-ERROR THRU 90000-EXIT.        00422700
422800                                                                  00422800
422900****************************************************************  00422900
423000***** D214. ADDED 04/13/89.                                       00423000
423100     IF ABM-REC-FOUND                                             00423100
423200         PERFORM 25000-SCAN-ABM-REC THRU 25000-EXIT               00423200
423300           VARYING GAA-INDEX FROM 1 BY 1                          00423300
423400           UNTIL   GAA-INDEX > GAA-ENTRY-COUNT.                   00423400
423500                                                                  00423500
423600     IF ACL-REC-FOUND                                             00423600
423700         PERFORM 25100-SCAN-ACL-REC THRU 25100-EXIT               00423700
423800           VARYING GAB-INDEX FROM 1 BY 1                          00423800
423900           UNTIL   GAB-INDEX > GAB-ENTRY-COUNT                    00423900
424000         MOVE DF-ERROR-COUNT TO WS-DF-ERROR-COUNT                 00424000
424100         CALL WS-GC024015 USING GC024030-CALL-AREA                00424100
424200                                ACL-REC                           00424200
424300         SET DF-ERROR-INDEX TO DF-ERROR-COUNT                     00424300
424400         SET DF-ERROR-INDEX UP BY 1                               00424400
424500         IF DF-ERROR-COUNT > WS-DF-ERROR-COUNT                    00424500
424600             MOVE 'Y' TO WS-ERROR-SW.                             00424600
424700                                                                  00424700
424800     IF ACP-REC-FOUND                                             00424800
424900         PERFORM 25400-SCAN-ACP-REC THRU 25400-EXIT               00424900
425000           VARYING GAF-INDEX FROM 1 BY 1                          00425000
425100           UNTIL   GAF-INDEX > GAF-ENTRY-COUNT                    00425100
425200         MOVE DF-ERROR-COUNT TO WS-DF-ERROR-COUNT                 00425200
425300         CALL WS-GC024015 USING GC024030-CALL-AREA                00425300
425400                                ACP-REC                           00425400
425500         SET DF-ERROR-INDEX TO DF-ERROR-COUNT                     00425500
425600         SET DF-ERROR-INDEX UP BY 1                               00425600
425700         IF DF-ERROR-COUNT > WS-DF-ERROR-COUNT                    00425700
425800             MOVE 'Y' TO WS-ERROR-SW.                             00425800
425900                                                                  00425900
426000     IF ADL-REC-FOUND                                             00426000
426100         PERFORM 25200-SCAN-ADL-REC THRU 25200-EXIT               00426100
426200           VARYING GAC-INDEX FROM 1 BY 1                          00426200
426300           UNTIL   GAC-INDEX > GAC-ENTRY-COUNT.                   00426300
426400                                                                  00426400
426500     IF AOL-REC-FOUND                                             00426500
426600         PERFORM 25300-SCAN-AOL-REC THRU 25300-EXIT               00426600
426700           VARYING GAD-INDEX FROM 1 BY 1                          00426700
426800           UNTIL   GAD-INDEX > GAD-ENTRY-COUNT                    00426800
426900         MOVE DF-ERROR-COUNT TO WS-DF-ERROR-COUNT                 00426900
427000         CALL WS-GC024015 USING GC024030-CALL-AREA                00427000
427100                                AOL-REC                           00427100
427200         SET DF-ERROR-INDEX TO DF-ERROR-COUNT                     00427200
427300         SET DF-ERROR-INDEX UP BY 1                               00427300
427400         IF DF-ERROR-COUNT > WS-DF-ERROR-COUNT                    00427400
427500             MOVE 'Y' TO WS-ERROR-SW.                             00427500
427600                                                                  00427600
427700***** END OF D214 EDITS.                                          00427700
427800****************************************************************  00427800
427900*KJD 10/20/93 ISSR 13272                                          00427900
428000*BOB  3/10/95 GA5 EDITS NO LONGER NEEDED.                         00428000
428100*    IF GCG-IPAR-PLAN-PROCESS-REQ = '00'                          00428100
428200*       NEXT SENTENCE                                             00428200
428300*    ELSE                                                         00428300
428400*       IF GCG-IPAR-PLAN-TRANS-RULE = '0'                         00428400
428500*          MOVE 'GA5' TO WS-CURRENT-ERROR                         00428500
428600*          PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00428600
428700*                                                                 00428700
428800*    IF GCG-IPAR-PLAN-TRANS-RULE = '0'                            00428800
428900*       NEXT SENTENCE                                             00428900
429000*    ELSE                                                         00429000
429100*       IF GCG-IPAR-PLAN-PROCESS-REQ = '00'                       00429100
429200*          MOVE 'GA5' TO WS-CURRENT-ERROR                         00429200
429300*          PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00429300
429400                                                                  00429400
429500     IF GCG-ADDL-TRNSPLNT-COVRG-IND = '0R'                        00429500
429600        IF GCG-IPAR-PLAN-TRANS-RULE = '2' OR '3' OR '4'           00429600
429700           NEXT SENTENCE                                          00429700
429800        ELSE                                                      00429800
429900           MOVE 'GA6' TO WS-CURRENT-ERROR                         00429900
430000           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00430000
430100                                                                  00430100
430200     IF GCG-FRI-SAT-ADM-IND = '0R'                                00430200
430300        IF GCG-IPAR-PLAN-TRANS-RULE = '2' OR '3' OR '4'           00430300
430400           NEXT SENTENCE                                          00430400
430500        ELSE                                                      00430500
430600           MOVE 'GA7' TO WS-CURRENT-ERROR                         00430600
430700           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00430700
430800                                                                  00430800
430900     IF GCG-HOSPICE-IND = '0R'                                    00430900
431000        IF GCG-IPAR-PLAN-TRANS-RULE = '2' OR '3' OR '4'           00431000
431100           NEXT SENTENCE                                          00431100
431200        ELSE                                                      00431200
431300           MOVE 'GA8' TO WS-CURRENT-ERROR                         00431300
431400           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00431400
431500                                                                  00431500
431600     IF GCG-INCENTIVE-OB-IND = '0R'                               00431600
431700        IF GCG-IPAR-PLAN-TRANS-RULE = '2' OR '3' OR '4'           00431700
431800           NEXT SENTENCE                                          00431800
431900        ELSE                                                      00431900
432000           MOVE 'GA9' TO WS-CURRENT-ERROR                         00432000
432100           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00432100
432200                                                                  00432200
432300     IF GCG-MAND-ADDL-SURG-OPN-IND = '0R'                         00432300
432400        IF GCG-IPAR-PLAN-TRANS-RULE = '2' OR '3' OR '4'           00432400
432500           NEXT SENTENCE                                          00432500
432600        ELSE                                                      00432600
432700           MOVE 'GAA' TO WS-CURRENT-ERROR                         00432700
432800           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00432800
432900                                                                  00432900
433000     IF GCG-MED-NECESSITY-HCNR-IPS-IN = '0R'                      00433000
433100        IF GCG-IPAR-PLAN-TRANS-RULE = '2' OR '3' OR '4'           00433100
433200           NEXT SENTENCE                                          00433200
433300        ELSE                                                      00433300
433400           MOVE 'GAB' TO WS-CURRENT-ERROR                         00433400
433500           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00433500
433600                                                                  00433600
433700     IF GCG-PARTICIPAT-PROV-OPTION = '0R'                         00433700
433800        IF GCG-IPAR-PLAN-TRANS-RULE = '2' OR '3' OR '4'           00433800
433900           NEXT SENTENCE                                          00433900
434000        ELSE                                                      00434000
434100           MOVE 'GAC' TO WS-CURRENT-ERROR                         00434100
434200           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00434200
434300                                                                  00434300
434400     IF GCG-MAND-OP-SURG-PROG-IND = '0R'                          00434400
434500        IF GCG-IPAR-PLAN-TRANS-RULE = '2' OR '3' OR '4'           00434500
434600           NEXT SENTENCE                                          00434600
434700        ELSE                                                      00434700
434800           MOVE 'GAD' TO WS-CURRENT-ERROR                         00434800
434900           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00434900
435000                                                                  00435000
435100     IF GCG-MED-SERV-ADV-PROG-IND = '0R'                          00435100
435200        IF GCG-IPAR-PLAN-TRANS-RULE = '2' OR '3' OR '4'           00435200
435300           NEXT SENTENCE                                          00435300
435400        ELSE                                                      00435400
435500           MOVE 'GAE' TO WS-CURRENT-ERROR                         00435500
435600           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00435600
435700                                                                  00435700
435800     IF GCG-PRE-ADM-REVIEW-IND = '0R'                             00435800
435900        IF GCG-IPAR-PLAN-TRANS-RULE = '2' OR '3' OR '4'           00435900
436000           NEXT SENTENCE                                          00436000
436100        ELSE                                                      00436100
436200           MOVE 'GAF' TO WS-CURRENT-ERROR                         00436200
436300           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00436300
436400                                                                  00436400
436500     IF GCG-PRE-ADM-TESTING-PROGRAM = '0R'                        00436500
436600        IF GCG-IPAR-PLAN-TRANS-RULE = '2' OR '3' OR '4'           00436600
436700           NEXT SENTENCE                                          00436700
436800        ELSE                                                      00436800
436900           MOVE 'GAG' TO WS-CURRENT-ERROR                         00436900
437000           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00437000
437100                                                                  00437100
437200     IF GCG-REIMBUR-SUBROG-IND = '0R'                             00437200
437300        IF GCG-IPAR-PLAN-TRANS-RULE = '2' OR '3' OR '4'           00437300
437400           NEXT SENTENCE                                          00437400
437500        ELSE                                                      00437500
437600           MOVE 'GAH' TO WS-CURRENT-ERROR                         00437600
437700           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00437700
437800                                                                  00437800
437900     IF GCG-MONDAY-DISCHARGE-IND = '0R'                           00437900
438000        IF GCG-IPAR-PLAN-TRANS-RULE = '2' OR '3' OR '4'           00438000
438100           NEXT SENTENCE                                          00438100
438200        ELSE                                                      00438200
438300           MOVE 'GAI' TO WS-CURRENT-ERROR                         00438300
438400           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00438400
438500                                                                  00438500
438600     IF GCG-SUBS-ABUSE-MENTAL-IND = '0R'                          00438600
438700        IF GCG-IPAR-PLAN-TRANS-RULE = '2' OR '3' OR '4'           00438700
438800           NEXT SENTENCE                                          00438800
438900        ELSE                                                      00438900
439000           MOVE 'GAJ' TO WS-CURRENT-ERROR                         00439000
439100           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00439100
439200                                                                  00439200
439300     IF GCG-POS-PARTICP-IND = '0R'                                00439300
439400        IF GCG-IPAR-PLAN-TRANS-RULE = '2' OR '3' OR '4'           00439400
439500           NEXT SENTENCE                                          00439500
439600        ELSE                                                      00439600
439700           MOVE 'GAK' TO WS-CURRENT-ERROR                         00439700
439800           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00439800
439900                                                                  00439900
440000     IF GCG-NEW-MEN-SUB-ABUSE-IND = '0R'                          00440000
440100        IF GCG-IPAR-PLAN-TRANS-RULE = '2' OR '3' OR '4'           00440100
440200           NEXT SENTENCE                                          00440200
440300        ELSE                                                      00440300
440400           MOVE 'GAL' TO WS-CURRENT-ERROR                         00440400
440500           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00440500
440600                                                                  00440600
440700     IF GCG-NEW-POS-IND = '0R'                                    00440700
440800        IF GCG-IPAR-PLAN-TRANS-RULE = '2' OR '3' OR '4'           00440800
440900           NEXT SENTENCE                                          00440900
441000        ELSE                                                      00441000
441100           MOVE 'GAM' TO WS-CURRENT-ERROR                         00441100
441200           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00441200
441300                                                                  00441300
441400     IF GCG-RPO-INDICATOR = '0R'                                  00441400
441500        IF GCG-IPAR-PLAN-TRANS-RULE = '2' OR '3' OR '4'           00441500
441600           NEXT SENTENCE                                          00441600
441700        ELSE                                                      00441700
441800           MOVE 'GAN' TO WS-CURRENT-ERROR                         00441800
441900           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00441900
442000                                                                  00442000
442100                                                                  00442100
442200     IF GCG-BAE-INDICATOR = '0R'                                  00442200
442300        IF GCG-IPAR-PLAN-TRANS-RULE = '2' OR '3' OR '4'           00442300
442400           NEXT SENTENCE                                          00442400
442500        ELSE                                                      00442500
442600           MOVE 'GAN' TO WS-CURRENT-ERROR                         00442600
442700           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00442700
442800                                                                  00442800
442900*END OF 13272 EDITS                                               00442900
443000*14045**********************************************************  00443000
443100*  FRY  PER AUGGIE ON 3/24/95, SHE IS UNAWARE OF THE '0R' VALUE.  00443100
443200*       HOWEVER, LEAVE THIS EDIT, JUST INCASE, IN THE FUTURE,     00443200
443300*       THIS VALUE MAY BE ADDED AND WE WILL BE PREPARED.          00443300
443400*                                                                 00443400
443500     IF GCG-CPO-PARTICIPATION-IND = '0R'                          00443500
443600        IF GCG-IPAR-PLAN-TRANS-RULE = '2' OR '3' OR '4'           00443600
443700           NEXT SENTENCE                                          00443700
443800        ELSE                                                      00443800
443900           MOVE 'GAP' TO WS-CURRENT-ERROR                         00443900
444000           PERFORM 90000-POST-ERROR THRU 90000-EXIT.              00444000
444100                                                                  00444100
444200                                                                  00444200
444300****************************************************************  00444300
444400* ADDED FOR D210 AND D215.                                        00444400
444500     IF GCG-INTER-RELATIONAL-CODE = ZEROS                         00444500
444600         MOVE 'G79' TO WS-CURRENT-ERROR                           00444600
444700         PERFORM 90000-POST-ERROR THRU 90000-EXIT.                00444700
444800     IF GCG-EFFDT-CEN  >  GCG-TERMDT-CEN                          00444800
444900         MOVE 'G80' TO WS-CURRENT-ERROR                           00444900
445000         PERFORM 90000-POST-ERROR THRU 90000-EXIT.                00445000
445100* END OF D210 AND D215 EDITS.                                     00445100
445200* ---------------------------                                     00445200
445300                                                                  00445300
445400                                                                  00445400
445500* ----------------------------                                    00445500
445600* ADDED FOR D365A AND D365B  -                                    00445600
445700* ----------------------------                                    00445700
445800                                                                  00445800
445900     MOVE 'N' TO WS-CAPI-FOUND-SW                                 00445900
446000     IF ABM-REC-FOUND                                             00446000
446100        PERFORM VARYING GAA-INDEX FROM 1 BY 1                     00446100
446200          UNTIL (GAA-INDEX > GAA-ENTRY-COUNT)                     00446200
446300                 OR CAPI-FOUND                                    00446300
446400            IF  GAA-BAMA-COMB-APPLIED-IND (GAA-INDEX) = '01'      00446400
446500                MOVE 'Y' TO WS-CAPI-FOUND-SW                      00446500
446600            END-IF                                                00446600
446700        END-PERFORM                                               00446700
446800     END-IF                                                       00446800
446900     IF ACL-REC-FOUND                                             00446900
447000        PERFORM VARYING GAB-INDEX FROM 1 BY 1                     00447000
447100          UNTIL (GAB-INDEX > GAB-ENTRY-COUNT)                     00447100
447200            IF  GAB-COINS-COMB-APPLIED-IND (GAB-INDEX) = '01'     00447200
447300                MOVE 'Y' TO WS-CAPI-FOUND-SW                      00447300
447400                IF GAB-COINS-1ST-DOLR-COVRGE-LMT (GAB-INDEX)      00447400
447500                       NOT = 'A'                                  00447500
447600                   MOVE 'GAW' TO WS-CURRENT-ERROR                 00447600
447700                   PERFORM 90000-POST-ERROR                       00447700
447800                END-IF                                            00447800
447900            END-IF                                                00447900
448000        END-PERFORM                                               00448000
448100     END-IF                                                       00448100
448200     IF ACP-REC-FOUND                                             00448200
448300        PERFORM VARYING GAF-INDEX FROM 1 BY 1                     00448300
448400          UNTIL (GAF-INDEX > GAF-ENTRY-COUNT)                     00448400
448500                 OR CAPI-FOUND                                    00448500
448600            IF  GAF-COPAY-COMB-APPLIED-IND (GAF-INDEX) = '01'     00448600
448700                MOVE 'Y' TO WS-CAPI-FOUND-SW                      00448700
448800            END-IF                                                00448800
448900        END-PERFORM                                               00448900
449000     END-IF                                                       00449000
449100     IF ADL-REC-FOUND                                             00449100
449200        PERFORM VARYING GAC-INDEX FROM 1 BY 1                     00449200
449300          UNTIL (GAC-INDEX > GAC-ENTRY-COUNT)                     00449300
449400                 OR CAPI-FOUND                                    00449400
449500            IF  GAC-DEDL-COMB-APPLIED-IND (GAC-INDEX) = '01'      00449500
449600                MOVE 'Y' TO WS-CAPI-FOUND-SW                      00449600
449700            END-IF                                                00449700
449800        END-PERFORM                                               00449800
449900     END-IF                                                       00449900
450000     IF AOL-REC-FOUND                                             00450000
450100        PERFORM VARYING GAD-INDEX FROM 1 BY 1                     00450100
450200          UNTIL (GAD-INDEX > GAD-ENTRY-COUNT)                     00450200
450300                 OR CAPI-FOUND                                    00450300
450400            IF  GAD-O-P-X-COMB-APPLIED-IND (GAD-INDEX) = '01'     00450400
450500                MOVE 'Y' TO WS-CAPI-FOUND-SW                      00450500
450600            END-IF                                                00450600
450700        END-PERFORM                                               00450700
450800     END-IF                                                       00450800
450900     IF CAPI-FOUND                                                00450900
451000        IF GCG-ACCM-REL-IND NOT = '01'                            00451000
451100           MOVE 'GAX' TO WS-CURRENT-ERROR                         00451100
451200           PERFORM 90000-POST-ERROR                               00451200
451300        END-IF                                                    00451300
451400     END-IF.                                                      00451400
451500                                                                  00451500
451600                                                                  00451600
451700* -----------------------------------                             00451700
451800* END OF EDITS FOR D365A AND D365B  -                             00451800
451900* -----------------------------------                             00451900
452000                                                                  00452000
452100                                                                  00452100
452200                                                                  00452200
452300****************************************************************  00452300
452400*** THE NEXT EDIT WILL CHECK THE CURRENT RECORD'S PSEUDO-GRP-NBR  00452400
452500*** AND PSEUDO-SECTION-NBR WITH ALL THE GROUP SPECIFIC RECORDS WIT00452500
452600*** THE SAME GROUP NUMBER. IF ANY OF THE PSEUDO-GRP-NBRS OR       00452600
452700*** PSEUDO-SECTION-NBRS DO NOT MATCH THEN THE CURRENT RECORD WILL 00452700
452800*** FLAGGED. NOTE: THIS IS THE LAST EDIT. THE CURRENT GROUP       00452800
452900*** SPECIFIC RECORD'S FIELDS WILL BE OVERLAYED WITH THE FOLLOWING 00452900
453000*** READS. FUTURE EDITS WILL HAVE TO BE PERFORMED PRIOR TO THIS   00453000
453100*** OR THE GROUP SPECIFIC FILE WILL HAVE TO BE READ AGAIN WITH    00453100
453200*** THE ORIGINAL KEY.                                             00453200
453300                                                                  00453300
453400***** THE FOLLOWING CHECK WAS ADDED BECAUSE THERE ARE SOME        00453400
453500***** GROUPS WHICH CAN HAVE DIFFERENT PSEUDO GROUP NBRS           00453500
453600***** WITHIN SECTIONS. A '9' WAS ADDED TO THE FIRST               00453600
453700***** POSITION OF THE INTER-RELATIONAL-CODE IN ORDER TO           00453700
453800***** ALLOW THE USER TO MAINTAIN THEIR OWN GROUP EXEMPT           00453800
453900***** NUMBERS.                                                    00453900
454000     MOVE GCG-INTER-RELATIONAL-CODE TO HOLD-INT-REL-CODE.         00454000
454100     IF H-INT-REL-CODE-POS1 = '9'                                 00454100
454200         GO TO 20000-EXIT.                                        00454200
454300     MOVE GCG-GRP-SPECIF-ID        TO HOLD-GROUP-KEY.             00454300
454400     MOVE GCG-ACCUM-PSEUDO-GRP-NBR TO HOLD-PSEUDO-GRP-NBR.        00454400
454500     MOVE GCG-ACCUM-PSEUDO-SECTION-NBR TO HOLD-PSEUDO-SECTION-NBR.00454500
454600     PERFORM 21010-POINT        THRU 21010-EXIT.                  00454600
454700     PERFORM 21020-CHECK-PSEUDO THRU 21020-EXIT.                  00454700
454800 20000-EXIT.                                                      00454800
454900     EXIT.                                                        00454900
455000                                                                  00455000
455100 20010-OTHER-EDITS.                                               00455100
455200                                                                  00455200
455300     IF ACL-REC-FOUND                                             00455300
455400        PERFORM 20020-ACL-EDITS  THRU 20020-EXIT.                 00455400
455500                                                                  00455500
455600     IF ADL-REC-FOUND                                             00455600
455700        PERFORM 20030-ADL-EDITS  THRU 20030-EXIT.                 00455700
455800                                                                  00455800
455900     IF AOL-REC-FOUND                                             00455900
456000        PERFORM 20040-AOL-EDITS  THRU 20040-EXIT.                 00456000
456100                                                                  00456100
456200 20010-EXIT.                                                      00456200
456300     EXIT.                                                        00456300
456400                                                                  00456400
456500/                                                                 00456500
456600 20020-ACL-EDITS.                                                 00456600
456700                                                                  00456700
456800*--- D15182 CHECK COINSURANCE                                     00456800
456900     PERFORM 20021-CHECK-COINSURANCE    THRU 20021-EXIT           00456900
457000       VARYING GAB-INDEX FROM 1 BY 1                              00457000
457100       UNTIL   GAB-INDEX = GAB-ENTRY-COUNT.                       00457100
457200                                                                  00457200
457300 20020-EXIT.                                                      00457300
457400     EXIT.                                                        00457400
457500/                                                                 00457500
457600 20021-CHECK-COINSURANCE.                                         00457600
457700*-- COINS LIMIT VALUES C1 THRU C5 VALID ONLY IF #ACP EXIST.       00457700
457800*-- FIRST $ COVERAGE VALUES A VALID ONLY IF #ACP EXIST.           00457800
457900                                                                  00457900
458000     IF GAB-COINS-1ST-DOLR-COVRGE-LMT (GAB-INDEX)  = 'A'          00458000
458100        IF ACP-REC-FOUND                                          00458100
458200             NEXT SENTENCE                                        00458200
458300        ELSE                                                      00458300
458400             MOVE 'YES'          TO  WS-ERROR-SW                  00458400
458500             ADD 1               TO  DF-ERROR-COUNT               00458500
458600*            MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT               00458600
458700             MOVE 'A28'          TO  DF-ERR-CODE (DF-ERROR-INDEX) 00458700
458800             SET DF-ERROR-INDEX UP BY 1.                          00458800
458900*                                                                 00458900
459000     MOVE GAB-COINS-DEFINITION   (GAB-INDEX)                      00459000
459100     TO   WS-COINS-LIM.                                           00459100
459200                                                                  00459200
459300     IF SELECTED-COINS                                            00459300
459400        IF ACP-REC-FOUND                                          00459400
459500             NEXT SENTENCE                                        00459500
459600        ELSE                                                      00459600
459700             MOVE 'YES'          TO  WS-ERROR-SW                  00459700
459800             ADD 1               TO  DF-ERROR-COUNT               00459800
459900*            MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT               00459900
460000             MOVE 'A29'          TO  DF-ERR-CODE (DF-ERROR-INDEX) 00460000
460100             SET DF-ERROR-INDEX UP BY 1.                          00460100
460200                                                                  00460200
460300 20021-EXIT.                                                      00460300
460400     EXIT.                                                        00460400
460500/                                                                 00460500
460600 20030-ADL-EDITS.                                                 00460600
460700                                                                  00460700
460800*--- D15182 CHECK DEDUCTIBLE DEFINITION                           00460800
460900     PERFORM 20031-CHECK-DEFINITION     THRU 20031-EXIT           00460900
461000       VARYING GAC-INDEX FROM 1 BY 1                              00461000
461100       UNTIL   GAC-INDEX = GAC-ENTRY-COUNT.                       00461100
461200                                                                  00461200
461300 20030-EXIT.                                                      00461300
461400     EXIT.                                                        00461400
461500/                                                                 00461500
461600 20031-CHECK-DEFINITION.                                          00461600
461700*-- VALUES 10 THRU 13 VALID ONLY IF #ACP EXIST.                   00461700
461800                                                                  00461800
461900     MOVE GAF-COPAY-DEFINITION (GAC-INDEX)                        00461900
462000     TO   WS-DED-DEF.                                             00462000
462100                                                                  00462100
462200     IF SELECTED-VALUE                                            00462200
462300        IF ACP-REC-FOUND                                          00462300
462400             NEXT SENTENCE                                        00462400
462500        ELSE                                                      00462500
462600             MOVE 'YES'          TO  WS-ERROR-SW                  00462600
462700             ADD 1               TO  DF-ERROR-COUNT               00462700
462800*            MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT               00462800
462900             MOVE 'A27'          TO  DF-ERR-CODE (DF-ERROR-INDEX) 00462900
463000              SET DF-ERROR-INDEX UP BY 1.                         00463000
463100                                                                  00463100
463200 20031-EXIT.                                                      00463200
463300     EXIT.                                                        00463300
463400/                                                                 00463400
463500 20040-AOL-EDITS.                                                 00463500
463600                                                                  00463600
463700*--- D15182 OUT OF POCKET                                         00463700
463800     PERFORM 20041-CHECK-OPX    THRU 20041-EXIT                   00463800
463900       VARYING GAD-INDEX FROM 1 BY 1                              00463900
464000       UNTIL   GAD-INDEX = GAD-ENTRY-COUNT.                       00464000
464100                                                                  00464100
464200 20040-EXIT.                                                      00464200
464300     EXIT.                                                        00464300
464400/                                                                 00464400
464500 20041-CHECK-OPX.                                                 00464500
464600*-- VALUES 10 THRU 20 VALID ONLY IF #ACP EXIST.                   00464600
464700                                                                  00464700
464800     MOVE GAD-O-P-X-DEFINITION (GAD-INDEX)                        00464800
464900     TO   WS-OPX.                                                 00464900
465000                                                                  00465000
465100     IF SELECTED-OPX                                              00465100
465200        IF ACP-REC-FOUND                                          00465200
465300             NEXT SENTENCE                                        00465300
465400        ELSE                                                      00465400
465500             MOVE 'YES'          TO  WS-ERROR-SW                  00465500
465600             ADD 1               TO  DF-ERROR-COUNT               00465600
465700*            MOVE DF-ERROR-COUNT TO  DF-ERROR-COUNT               00465700
465800             MOVE 'A30'          TO  DF-ERR-CODE (DF-ERROR-INDEX) 00465800
465900             SET DF-ERROR-INDEX UP BY 1.                          00465900
466000                                                                  00466000
466100 20041-EXIT.                                                      00466100
466200     EXIT.                                                        00466200
       EJECT                                                            00466300
                                                                        00466301
       20080-BLUE-DISTINCTION-EDITS.                                    00466310
                                                                        00466315
           SET  GSS2-INDEX  TO  WS-BD-GSS2-INDEX.                       00466316
                                                                        00466317
           IF  GSS2-BD-CALC-METHOD           (GSS2-INDEX)  =  ZEROS     00466318
                                                                        00466319
           AND GSS2-BD-BARIATRIC-POEPLS-IND  (GSS2-INDEX)  =  ZEROES    00466320
           AND GSS2-BD-BARIATRIC-POE-IND     (GSS2-INDEX)  =  ZEROS     00466321
           AND GSS2-BD-BARIATRIC-NET-IND     (GSS2-INDEX)  =  ZEROS     00466322
           AND GSS2-BD-BARIATRIC-NONNET-IND  (GSS2-INDEX)  =  ZEROS     00466323
                                                                        00466324
           AND GSS2-BD-MATERNITY-POEPLS-IND  (GSS2-INDEX)  =  ZEROES    00466325
           AND GSS2-BD-MATERNITY-POE-IND     (GSS2-INDEX)  =  ZEROS     00466326
           AND GSS2-BD-MATERNITY-NET-IND     (GSS2-INDEX)  =  ZEROS     00466327
           AND GSS2-BD-MATERNITY-NONNET-IND  (GSS2-INDEX)  =  ZEROS     00466328
                                                                        00466329
           AND GSS2-BD-TRANSPLANT-POEPLS-IND (GSS2-INDEX)  =  ZEROES    00466330
           AND GSS2-BD-TRANSPLANT-POE-IND    (GSS2-INDEX)  =  ZEROS     00466331
           AND GSS2-BD-TRANSPLANT-NET-IND    (GSS2-INDEX)  =  ZEROS     00466332
           AND GSS2-BD-TRANSPLANT-NONNET-IND (GSS2-INDEX)  =  ZEROS     00466333
                                                                        00466334
           AND GSS2-BD-CARDIAC-POEPLS-IND    (GSS2-INDEX)  =  ZEROES    00466335
           AND GSS2-BD-CARDIAC-POE-IND       (GSS2-INDEX)  =  ZEROS     00466336
           AND GSS2-BD-CARDIAC-NET-IND       (GSS2-INDEX)  =  ZEROS     00466337
           AND GSS2-BD-CARDIAC-NONNET-IND    (GSS2-INDEX)  =  ZEROS     00466338
                                                                        00466339
           AND GSS2-BD-CANCERS-POEPLS-IND    (GSS2-INDEX)  =  ZEROES    00466340
           AND GSS2-BD-CANCERS-POE-IND       (GSS2-INDEX)  =  ZEROS     00466341
           AND GSS2-BD-CANCERS-NET-IND       (GSS2-INDEX)  =  ZEROS     00466342
           AND GSS2-BD-CANCERS-NONNET-IND    (GSS2-INDEX)  =  ZEROS     00466343
      *NOV2021 BEG                                                      00466344
           AND GSS2-BD-CANCERSP-POEPLS-IND   (GSS2-INDEX)  =  ZEROS     00466346
           AND GSS2-BD-CANCERSP-POE-IND      (GSS2-INDEX)  =  ZEROS     00466347
           AND GSS2-BD-CANCERSP-NET-IND      (GSS2-INDEX)  =  ZEROS     00466348
           AND GSS2-BD-CANCERSP-NONNET-IND   (GSS2-INDEX)  =  ZEROS     00466349
      *NOV2021 END                                                      00466350
           AND GSS2-BD-SPINE-POEPLS-IND      (GSS2-INDEX)  =  ZEROS     00466351
           AND GSS2-BD-SPINE-POE-IND         (GSS2-INDEX)  =  ZEROS     00466352
           AND GSS2-BD-SPINE-NET-IND         (GSS2-INDEX)  =  ZEROS     00466353
           AND GSS2-BD-SPINE-NONNET-IND      (GSS2-INDEX)  =  ZEROS     00466354
                                                                        00466355
           AND GSS2-BD-REPLACE-POEPLS-IND    (GSS2-INDEX)  =  ZEROS     00466356
           AND GSS2-BD-REPLACE-POE-IND       (GSS2-INDEX)  =  ZEROS     00466357
           AND GSS2-BD-REPLACE-NET-IND       (GSS2-INDEX)  =  ZEROS     00466358
           AND GSS2-BD-REPLACE-NONNET-IND    (GSS2-INDEX)  =  ZEROS     00466359
                                                                        00466360
                                                                        00466361
               MOVE 'GA4'  TO  WS-CURRENT-ERROR                         00466362
               PERFORM 90000-POST-ERROR  THRU  90000-EXIT               00466363
           END-IF.                                                      00466364
                                                                        00466365
       20080-EXIT.                                                      00466366
           EXIT.                                                        00466367
       EJECT                                                            00466368
                                                                        00466369
      *  KIKI  LAST                                                     00466371
       20090-TOTAL-CARE-EDITS.                                          00466380
                                                                        00466391
           SET  GSS2-INDEX  TO  WS-TC-GSS2-INDEX.                       00466392
                                                                        00466393
           IF  GSS2-TC-CALC-METHOD    (GSS2-INDEX)  =  ZEROS            00466394
           AND GSS2-TC-TOTAL-CARE-IND (GSS2-INDEX)  =  ZEROS            00466396
      *--  SRI - HAS PROJECT                                            00466397
           AND GSS2-TC-TOTAL-CARE-PLUS-IND(GSS2-INDEX) =  ZEROS         00466398
      *--  SRI - HAS PROJECT                                            00466399
                                                                        00466430
                                                                        00466431
               MOVE 'GA4'  TO  WS-CURRENT-ERROR                         00466432
               PERFORM 90000-POST-ERROR  THRU  90000-EXIT               00466433
           END-IF.                                                      00466434
                                                                        00466435
       20090-EXIT.                                                      00466436
           EXIT.                                                        00466437
       EJECT                                                            00466439
                                                                        00466440
       20100-CE-EDITS.                                                  00466442
                                                                        00466443
           SET  GSS2-INDEX  TO  WS-CE-GSS2-INDEX.                       00466444
                                                                        00466445
           IF  GSS2-CE-CALC-METHOD          (GSS2-INDEX)  =  ZEROS      00466446
           AND GSS2-CE-BARIATRIC-COE-IND    (GSS2-INDEX)  =  ZEROS      00466447
           AND GSS2-CE-BARIATRIC-NET-IND    (GSS2-INDEX)  =  ZEROS      00466448
           AND GSS2-CE-BARIATRIC-NONNET-IND (GSS2-INDEX)  =  ZEROS      00466449
                                                                        00466450
                                                                        00466451
               MOVE 'GA4'  TO  WS-CURRENT-ERROR                         00466452
               PERFORM 90000-POST-ERROR  THRU  90000-EXIT               00466453
           END-IF.                                                      00466454
                                                                        00466455
       20100-EXIT.                                                      00466456
           EXIT.                                                        00466457
       EJECT                                                            00466458
      *--  TROY - ERS PROJECT                                           00466459
                                                                        00466460
      *--  SRI  - HAS PROJECT                                           00466484
       20110-EC-EDITS.                                                  00466485
                                                                        00466486
           SET  GSS2-INDEX  TO  WS-EC-GSS2-INDEX.                       00466487
                                                                        00466488
           IF  GSS2-EC-BIT-MRI              (GSS2-INDEX)  =  ZEROS      00466489
           AND GSS2-EC-BIT-CTSCAN           (GSS2-INDEX)  =  ZEROS      00466490
           AND GSS2-EC-BIT-DIAGNOSTIC-RADIO (GSS2-INDEX)  =  ZEROS      00466491
           AND GSS2-EC-BIT-JOINT-REPLACEMENT(GSS2-INDEX)  =  ZEROS      00466492
           AND GSS2-EC-BIT-BARIATRIC        (GSS2-INDEX)  =  ZEROS      00466493
           AND GSS2-EC-BIT-MUSC-SKELETAL-IP (GSS2-INDEX)  =  ZEROS      00466494
           AND GSS2-EC-BIT-MUSC-SKELETAL-OP (GSS2-INDEX)  =  ZEROS      00466495
           AND GSS2-EC-BIT-REDUCTION-MAMMO  (GSS2-INDEX)  =  ZEROS      00466496
                                                                        00466497
                                                                        00466498
               MOVE 'GA4'  TO  WS-CURRENT-ERROR                         00466499
               PERFORM 90000-POST-ERROR  THRU  90000-EXIT               00466500
           END-IF.                                                      00466501
                                                                        00466502
       20110-EXIT.                                                      00466503
           EXIT.                                                        00466504
       EJECT                                                            00466505
      *--  SRI  - HAS PROJECT                                           00466506
      *--  SRI  - HAS PROJECT                                           00466507
       20120-PC-EDITS.                                                  00466508
                                                                        00466509
           SET  GSS2-INDEX  TO  WS-PC-GSS2-INDEX.                       00466510
                                                                        00466511
           IF  GSS2-PC-BIT-RADIATION-TH     (GSS2-INDEX)  =  ZEROS      00466512
           AND GSS2-PC-BIT-GENETIC-TEST     (GSS2-INDEX)  =  ZEROS      00466513
           AND GSS2-PC-BIT-ADV-IMAGING      (GSS2-INDEX)  =  ZEROS      00466514
           AND GSS2-PC-BIT-SLEEP-STUDY      (GSS2-INDEX)  =  ZEROS      00466515
           AND GSS2-PC-BIT-CARDIOLOGY       (GSS2-INDEX)  =  ZEROS      00466516
           AND GSS2-PC-BIT-PAIN-MGMT        (GSS2-INDEX)  =  ZEROS      00466517
           AND GSS2-PC-BIT-JOINT-SPINE      (GSS2-INDEX)  =  ZEROS      00466518
           AND GSS2-PC-BIT-OTH-OP-PROC      (GSS2-INDEX)  =  ZEROS      00466519
           AND GSS2-PC-BIT-SPECIAL-RX       (GSS2-INDEX)  =  ZEROS      00466520
      *--  SRI  - CONCEPT12 PROJECT                                     00466521
                                                                        00466522
           AND GSS2-PC-BIT-BARIATRIC-SUR    (GSS2-INDEX)  =  ZEROS      00466523
           AND GSS2-PC-BIT-INFERT-PROC      (GSS2-INDEX)  =  ZEROS      00466524
           AND GSS2-PC-BIT-RECONS-OTHER     (GSS2-INDEX)  =  ZEROS      00466525
           AND GSS2-PC-BIT-SPECIALTY-INF    (GSS2-INDEX)  =  ZEROS      00466526
           AND GSS2-PC-BIT-ORGAN-TRANS      (GSS2-INDEX)  =  ZEROS      00466527
           AND GSS2-PC-BIT-GENE-THERAPY     (GSS2-INDEX)  =  ZEROS      00466528
      *--  SRI  - CONCEPT12 PROJECT                                     00466529
                                                                        00466530
               MOVE 'GA4'  TO  WS-CURRENT-ERROR                         00466531
               PERFORM 90000-POST-ERROR  THRU  90000-EXIT               00466532
           END-IF.                                                      00466533
                                                                        00466534
       20120-EXIT.                                                      00466535
           EXIT.                                                        00466536
       EJECT                                                            00466537
      *--  SRI  - HAS PROJECT                                           00466538
      *--  SRI  - PSMNR PROJECT                                         00466539
       20130-CU-EDITS.                                                  00466540
                                                                        00466541
           SET  GSS2-INDEX  TO  WS-CU-GSS2-INDEX.                       00466542
                                                                        00466543
           IF  GSS2-CU-CUSTOM-ACCOUNT-CODE      (GSS2-INDEX)  =  SPACES 00466544
SRIAUG     AND GSS2-CU-CUSTOM-ACCOUNT-IPBIT     (GSS2-INDEX)  =  SPACES 00466545
SRIAUG     AND GSS2-CU-CUSTOM-ACCOUNT-IMBIT     (GSS2-INDEX)  =  SPACES 00466546
SRIAUG     AND GSS2-CU-CUSTOM-ACCOUNT-VPBIT     (GSS2-INDEX)  =  SPACES 00466547
SRIAUG     AND GSS2-CU-CUSTOM-ACCOUNT-VMBIT     (GSS2-INDEX)  =  SPACES 00466548
           AND GSS2-CU-BIT-RADIATION-IPA        (GSS2-INDEX)  =  ZEROS  00466549
           AND GSS2-CU-BIT-RADIATION-IMN        (GSS2-INDEX)  =  ZEROS  00466550
           AND GSS2-CU-BIT-RADIATION-EPA        (GSS2-INDEX)  =  ZEROS  00466551
           AND GSS2-CU-BIT-RADIATION-EMN        (GSS2-INDEX)  =  ZEROS  00466552
           AND GSS2-CU-BIT-RADIATION-PEN        (GSS2-INDEX)  =  ZEROS  00466553
           AND GSS2-CU-BIT-GENETIC-IPA          (GSS2-INDEX)  =  ZEROS  00466554
           AND GSS2-CU-BIT-GENETIC-IMN          (GSS2-INDEX)  =  ZEROS  00466555
           AND GSS2-CU-BIT-GENETIC-EPA          (GSS2-INDEX)  =  ZEROS  00466556
           AND GSS2-CU-BIT-GENETIC-EMN          (GSS2-INDEX)  =  ZEROS  00466557
           AND GSS2-CU-BIT-GENETIC-PEN          (GSS2-INDEX)  =  ZEROS  00466558
           AND GSS2-CU-BIT-ADV-IMAG-IPA         (GSS2-INDEX)  =  ZEROS  00466559
           AND GSS2-CU-BIT-ADV-IMAG-IMN         (GSS2-INDEX)  =  ZEROS  00466560
           AND GSS2-CU-BIT-ADV-IMAG-EPA         (GSS2-INDEX)  =  ZEROS  00466561
           AND GSS2-CU-BIT-ADV-IMAG-EMN         (GSS2-INDEX)  =  ZEROS  00466562
           AND GSS2-CU-BIT-ADV-IMAG-PEN         (GSS2-INDEX)  =  ZEROS  00466563
           AND GSS2-CU-BIT-SLEEP-IPA            (GSS2-INDEX)  =  ZEROS  00466564
           AND GSS2-CU-BIT-SLEEP-IMN            (GSS2-INDEX)  =  ZEROS  00466565
           AND GSS2-CU-BIT-SLEEP-EPA            (GSS2-INDEX)  =  ZEROS  00466566
           AND GSS2-CU-BIT-SLEEP-EMN            (GSS2-INDEX)  =  ZEROS  00466567
           AND GSS2-CU-BIT-SLEEP-PEN            (GSS2-INDEX)  =  ZEROS  00466568
           AND GSS2-CU-BIT-CARDIO-IPA           (GSS2-INDEX)  =  ZEROS  00466569
           AND GSS2-CU-BIT-CARDIO-IMN           (GSS2-INDEX)  =  ZEROS  00466570
           AND GSS2-CU-BIT-CARDIO-EPA           (GSS2-INDEX)  =  ZEROS  00466571
           AND GSS2-CU-BIT-CARDIO-EMN           (GSS2-INDEX)  =  ZEROS  00466572
           AND GSS2-CU-BIT-CARDIO-PEN           (GSS2-INDEX)  =  ZEROS  00466573
           AND GSS2-CU-BIT-PAIN-MGMT-IPA        (GSS2-INDEX)  =  ZEROS  00466574
           AND GSS2-CU-BIT-PAIN-MGMT-IMN        (GSS2-INDEX)  =  ZEROS  00466575
           AND GSS2-CU-BIT-PAIN-MGMT-EPA        (GSS2-INDEX)  =  ZEROS  00466576
           AND GSS2-CU-BIT-PAIN-MGMT-EMN        (GSS2-INDEX)  =  ZEROS  00466577
           AND GSS2-CU-BIT-PAIN-MGMT-PEN        (GSS2-INDEX)  =  ZEROS  00466578
           AND GSS2-CU-BIT-JOINT-SPINE-IPA      (GSS2-INDEX)  =  ZEROS  00466579
           AND GSS2-CU-BIT-JOINT-SPINE-IMN      (GSS2-INDEX)  =  ZEROS  00466580
           AND GSS2-CU-BIT-JOINT-SPINE-EPA      (GSS2-INDEX)  =  ZEROS  00466581
           AND GSS2-CU-BIT-JOINT-SPINE-EMN      (GSS2-INDEX)  =  ZEROS  00466582
           AND GSS2-CU-BIT-JOINT-SPINE-PEN      (GSS2-INDEX)  =  ZEROS  00466583
           AND GSS2-CU-BIT-OP-PROC-IPA          (GSS2-INDEX)  =  ZEROS  00466584
           AND GSS2-CU-BIT-OP-PROC-IMN          (GSS2-INDEX)  =  ZEROS  00466585
           AND GSS2-CU-BIT-OP-PROC-EPA          (GSS2-INDEX)  =  ZEROS  00466586
           AND GSS2-CU-BIT-OP-PROC-EMN          (GSS2-INDEX)  =  ZEROS  00466587
           AND GSS2-CU-BIT-OP-PROC-PEN          (GSS2-INDEX)  =  ZEROS  00466588
           AND GSS2-CU-BIT-SPECIAL-RX-IPA       (GSS2-INDEX)  =  ZEROS  00466589
           AND GSS2-CU-BIT-SPECIAL-RX-IMN       (GSS2-INDEX)  =  ZEROS  00466590
           AND GSS2-CU-BIT-SPECIAL-RX-EPA       (GSS2-INDEX)  =  ZEROS  00466591
           AND GSS2-CU-BIT-SPECIAL-RX-EMN       (GSS2-INDEX)  =  ZEROS  00466592
           AND GSS2-CU-BIT-SPECIAL-RX-PEN       (GSS2-INDEX)  =  ZEROS  00466593
           AND GSS2-CU-BIT-OTHER-PRE-IPA        (GSS2-INDEX)  =  ZEROS  00466594
           AND GSS2-CU-BIT-OTHER-PRE-IMN        (GSS2-INDEX)  =  ZEROS  00466595
           AND GSS2-CU-BIT-OTHER-PRE-EPA        (GSS2-INDEX)  =  ZEROS  00466596
           AND GSS2-CU-BIT-OTHER-PRE-EMN        (GSS2-INDEX)  =  ZEROS  00466597
           AND GSS2-CU-BIT-OTHER-PRE-PEN        (GSS2-INDEX)  =  ZEROS  00466598
                                                                        00466599
                                                                        00466600
               MOVE 'GA4'  TO  WS-CURRENT-ERROR                         00466601
               PERFORM 90000-POST-ERROR  THRU  90000-EXIT               00466602
           END-IF.                                                      00466603
                                                                        00466604
       20130-EXIT.                                                      00466605
           EXIT.                                                        00466606
       EJECT                                                            00466607
      *--  SRI  - PSMNR PROJECT                                         00466608
466400 21010-POINT.                                                     00466609
466500     MOVE 'P' TO REQUEST-TYPE-1.                                  00466610
466600     MOVE +16  TO ONEA-REC-LENG.                                  00466620
466700     CALL 'TSGVSAM3' USING PARM-ONE PARM-ONEA.                    00466700
466800     IF REQUEST-TYPE-1 NOT = 'P'                                  00466800
466900         DISPLAY 'GC024020 BAD POINT '                            00466900
467000              H-GROUP-NUM   ' '                                   00467000
467100              H-SECTION-NUM ' '                                   00467100
467200              H-FAM-REL-LVL ' '                                   00467200
467300              H-EFF-DATE                                          00467300
467400             MOVE SET-FEEDBACK          TO ABEND-CODE             00467400
467500             GO TO 99999-ERROR-RTN.                               00467500
467600 21010-EXIT.                                                      00467600
467700     EXIT.                                                        00467700
467800                                                                  00467800
467900 21020-CHECK-PSEUDO.                                              00467900
468000                                                                  00468000
468100     MOVE 'G' TO REQUEST-TYPE-1.                                  00468100
468200     CALL 'TSGVSAM3' USING PARM-ONE PARM-ONEA.                    00468200
468300     IF REQUEST-TYPE-1 = '2'                                      00468300
468400         GO TO 21020-EXIT.                                        00468400
468500     IF REQUEST-TYPE-1 NOT = 'G'                                  00468500
468600         DISPLAY 'GC024020 BAD GET-PARA 21020-CHECK-PSEUDO '      00468600
468700              H-GROUP-NUM   ' '                                   00468700
468800              H-SECTION-NUM ' '                                   00468800
468900              H-FAM-REL-LVL ' '                                   00468900
469000              H-EFF-DATE                                          00469000
469100             MOVE SET-FEEDBACK          TO ABEND-CODE             00469100
469200             GO TO 99999-ERROR-RTN.                               00469200
469300     IF GCG-GROUP-NUM = H-GROUP-NUM                               00469300
469400         NEXT SENTENCE                                            00469400
469500     ELSE                                                         00469500
469600         GO TO 21020-EXIT.                                        00469600
469700     IF GCG-ACCUM-PSEUDO-GRP-NBR NOT = HOLD-PSEUDO-GRP-NBR        00469700
469800       OR                                                         00469800
469900        GCG-ACCUM-PSEUDO-SECTION-NBR NOT = HOLD-PSEUDO-SECTION-NBR00469900
470000         PERFORM 21030-MOVE-PSEUDO-ERR-MSG THRU 21030-EXIT        00470000
470100         GO TO 21020-EXIT.                                        00470100
470200                                                                  00470200
470300     GO TO 21020-CHECK-PSEUDO.                                    00470300
470400 21020-EXIT.                                                      00470400
470500     EXIT.                                                        00470500
470600                                                                  00470600
470700 21030-MOVE-PSEUDO-ERR-MSG.                                       00470700
470800     IF GCG-ACCUM-PSEUDO-GRP-NBR NOT = HOLD-PSEUDO-GRP-NBR        00470800
470900         MOVE 'G64' TO WS-CURRENT-ERROR                           00470900
471000         PERFORM 90000-POST-ERROR THRU 90000-EXIT.                00471000
471100     IF GCG-ACCUM-PSEUDO-SECTION-NBR NOT = HOLD-PSEUDO-SECTION-NBR00471100
471200         MOVE 'G65' TO WS-CURRENT-ERROR                           00471200
471300         PERFORM 90000-POST-ERROR THRU 90000-EXIT.                00471300
471400 21030-EXIT.                                                      00471400
471500     EXIT.                                                        00471500
471600/                                                                 00471600
471700*22000-D210-EDIT.                                                 00471700
471800*    IF GAD-ENTRY-COUNT > +2                                      00471800
471900*        NEXT SENTENCE                                            00471900
472000*    ELSE                                                         00472000
472100*        GO TO 22000-EXIT.                                        00472100
472200*    MOVE +999 TO WS-PCT-LVL.                                     00472200
472300*    PERFORM 22100-D210-COMPARE THRU 22100-EXIT                   00472300
472400*      VARYING GAD-INDEX FROM 1 BY 1                              00472400
472500*      UNTIL   GAD-INDEX = GAD-ENTRY-COUNT.                       00472500
472600*22000-EXIT.                                                      00472600
472700*    EXIT.                                                        00472700
472800*22100-D210-COMPARE.                                              00472800
472900*    IF GAD-O-P-X-PERCENT-LEVEL (GAD-INDEX)  = +100               00472900
473000*        GO TO 22100-EXIT.                                        00473000
473100*    IF GAD-O-P-X-PERCENT-LEVEL (GAD-INDEX)  NOT = WS-PCT-LVL     00473100
473200*        IF WS-PCT-LVL = +999                                     00473200
473300*            MOVE GAD-O-P-X-PERCENT-LEVEL (GAD-INDEX)             00473300
473400*              TO WS-PCT-LVL                                      00473400
473500*            GO TO 22100-EXIT                                     00473500
473600*        ELSE                                                     00473600
473700*            MOVE 'G81' TO WS-CURRENT-ERROR                       00473700
473800*            PERFORM 90000-POST-ERROR THRU 90000-EXIT             00473800
473900*            SET GAD-INDEX TO GAD-ENTRY-COUNT                     00473900
474000*            SET GAD-INDEX DOWN BY 1.                             00474000
474100*22100-EXIT.                                                      00474100
474200*    EXIT.                                                        00474200
474300/                                                                 00474300
474400 25000-SCAN-ABM-REC.                                              00474400
474500     IF GAA-INDEX = GAA-ENTRY-COUNT                               00474500
474600         GO TO 25000-EXIT.                                        00474600
474700     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '01'                       00474700
474800         IF GAA-BAMA-L-O-B (GAA-INDEX) = '1'                      00474800
474900             GO TO 25000-EXIT                                     00474900
475000         ELSE                                                     00475000
475100             PERFORM 25010-POST-ABM-ERROR THRU 25010-EXIT         00475100
475200             SET GAA-INDEX TO GAA-ENTRY-COUNT                     00475200
475300             GO TO 25000-EXIT.                                    00475300
475400     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '02'                       00475400
475500         IF GAA-BAMA-L-O-B (GAA-INDEX) = '1' OR '2' OR '5'        00475500
475600             GO TO 25000-EXIT                                     00475600
475700         ELSE                                                     00475700
475800             PERFORM 25010-POST-ABM-ERROR THRU 25010-EXIT         00475800
475900             SET GAA-INDEX TO GAA-ENTRY-COUNT                     00475900
476000             GO TO 25000-EXIT.                                    00476000
476100     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '03'                       00476100
476200         IF GAA-BAMA-L-O-B (GAA-INDEX) = '1' OR '2' OR '3' OR     00476200
476300                                  '5' OR '6' OR '7' OR '8'        00476300
476400             GO TO 25000-EXIT                                     00476400
476500         ELSE                                                     00476500
476600             PERFORM 25010-POST-ABM-ERROR THRU 25010-EXIT         00476600
476700             SET GAA-INDEX TO GAA-ENTRY-COUNT                     00476700
476800             GO TO 25000-EXIT.                                    00476800
476900     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '04'                       00476900
477000         IF GAA-BAMA-L-O-B (GAA-INDEX) = '1' OR '3' OR '6'        00477000
477100             GO TO 25000-EXIT                                     00477100
477200         ELSE                                                     00477200
477300             PERFORM 25010-POST-ABM-ERROR THRU 25010-EXIT         00477300
477400             SET GAA-INDEX TO GAA-ENTRY-COUNT                     00477400
477500             GO TO 25000-EXIT.                                    00477500
477600     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '05'                       00477600
477700         IF GAA-BAMA-L-O-B (GAA-INDEX) = '2'                      00477700
477800             GO TO 25000-EXIT                                     00477800
477900         ELSE                                                     00477900
478000             PERFORM 25010-POST-ABM-ERROR THRU 25010-EXIT         00478000
478100             SET GAA-INDEX TO GAA-ENTRY-COUNT                     00478100
478200             GO TO 25000-EXIT.                                    00478200
478300     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '06'                       00478300
478400         IF GAA-BAMA-L-O-B (GAA-INDEX) = '2' OR '3' OR '7'        00478400
478500             GO TO 25000-EXIT                                     00478500
478600         ELSE                                                     00478600
478700             PERFORM 25010-POST-ABM-ERROR THRU 25010-EXIT         00478700
478800             SET GAA-INDEX TO GAA-ENTRY-COUNT                     00478800
478900             GO TO 25000-EXIT.                                    00478900
479000     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '07'                       00479000
479100         IF GAA-BAMA-L-O-B (GAA-INDEX) = '4'                      00479100
479200             GO TO 25000-EXIT                                     00479200
479300         ELSE                                                     00479300
479400             PERFORM 25010-POST-ABM-ERROR THRU 25010-EXIT         00479400
479500             SET GAA-INDEX TO GAA-ENTRY-COUNT                     00479500
479600             GO TO 25000-EXIT.                                    00479600
479700     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '08'                       00479700
479800         IF GAA-BAMA-L-O-B (GAA-INDEX) = '3'                      00479800
479900             GO TO 25000-EXIT                                     00479900
480000         ELSE                                                     00480000
480100             PERFORM 25010-POST-ABM-ERROR THRU 25010-EXIT         00480100
480200             SET GAA-INDEX TO GAA-ENTRY-COUNT                     00480200
480300             GO TO 25000-EXIT.                                    00480300
480400 25000-EXIT.                                                      00480400
480500     EXIT.                                                        00480500
480600 25010-POST-ABM-ERROR.                                            00480600
480700     MOVE 'G74' TO WS-CURRENT-ERROR.                              00480700
480800     PERFORM 90000-POST-ERROR THRU 90000-EXIT.                    00480800
480900 25010-EXIT.                                                      00480900
481000     EXIT.                                                        00481000
481100/                                                                 00481100
481200 25100-SCAN-ACL-REC.                                              00481200
481300     IF GAB-INDEX = GAB-ENTRY-COUNT                               00481300
481400         GO TO 25100-EXIT.                                        00481400
481500     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '01'                       00481500
481600         IF GAB-COINS-L-O-B (GAB-INDEX) = '1'                     00481600
481700             GO TO 25100-EXIT                                     00481700
481800         ELSE                                                     00481800
481900             PERFORM 25110-POST-ACL-ERROR THRU 25110-EXIT         00481900
482000             SET GAB-INDEX TO GAB-ENTRY-COUNT                     00482000
482100             GO TO 25100-EXIT.                                    00482100
482200     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '02'                       00482200
482300         IF GAB-COINS-L-O-B (GAB-INDEX) = '1' OR '2' OR '5'       00482300
482400             GO TO 25100-EXIT                                     00482400
482500         ELSE                                                     00482500
482600             PERFORM 25110-POST-ACL-ERROR THRU 25110-EXIT         00482600
482700             SET GAB-INDEX TO GAB-ENTRY-COUNT                     00482700
482800             GO TO 25100-EXIT.                                    00482800
482900     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '03'                       00482900
483000         IF GAB-COINS-L-O-B (GAB-INDEX) = '1' OR '2' OR '3' OR    00483000
483100                                  '5' OR '6' OR '7' OR '8'        00483100
483200             GO TO 25100-EXIT                                     00483200
483300         ELSE                                                     00483300
483400             PERFORM 25110-POST-ACL-ERROR THRU 25110-EXIT         00483400
483500             SET GAB-INDEX TO GAB-ENTRY-COUNT                     00483500
483600             GO TO 25100-EXIT.                                    00483600
483700     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '04'                       00483700
483800         IF GAB-COINS-L-O-B (GAB-INDEX) = '1' OR '3' OR '6'       00483800
483900             GO TO 25100-EXIT                                     00483900
484000         ELSE                                                     00484000
484100             PERFORM 25110-POST-ACL-ERROR THRU 25110-EXIT         00484100
484200             SET GAB-INDEX TO GAB-ENTRY-COUNT                     00484200
484300             GO TO 25100-EXIT.                                    00484300
484400     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '05'                       00484400
484500         IF GAB-COINS-L-O-B (GAB-INDEX) = '2'                     00484500
484600             GO TO 25100-EXIT                                     00484600
484700         ELSE                                                     00484700
484800             PERFORM 25110-POST-ACL-ERROR THRU 25110-EXIT         00484800
484900             SET GAB-INDEX TO GAB-ENTRY-COUNT                     00484900
485000             GO TO 25100-EXIT.                                    00485000
485100     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '06'                       00485100
485200         IF GAB-COINS-L-O-B (GAB-INDEX) = '2' OR '3' OR '7'       00485200
485300             GO TO 25100-EXIT                                     00485300
485400         ELSE                                                     00485400
485500             PERFORM 25110-POST-ACL-ERROR THRU 25110-EXIT         00485500
485600             SET GAB-INDEX TO GAB-ENTRY-COUNT                     00485600
485700             GO TO 25100-EXIT.                                    00485700
485800     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '07'                       00485800
485900         IF GAB-COINS-L-O-B (GAB-INDEX) = '4'                     00485900
486000             GO TO 25100-EXIT                                     00486000
486100         ELSE                                                     00486100
486200             PERFORM 25110-POST-ACL-ERROR THRU 25110-EXIT         00486200
486300             SET GAB-INDEX TO GAB-ENTRY-COUNT                     00486300
486400             GO TO 25100-EXIT.                                    00486400
486500     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '08'                       00486500
486600         IF GAB-COINS-L-O-B (GAB-INDEX) = '3'                     00486600
486700             GO TO 25100-EXIT                                     00486700
486800         ELSE                                                     00486800
486900             PERFORM 25110-POST-ACL-ERROR THRU 25110-EXIT         00486900
487000             SET GAB-INDEX TO GAB-ENTRY-COUNT                     00487000
487100             GO TO 25100-EXIT.                                    00487100
487200 25100-EXIT.                                                      00487200
487300     EXIT.                                                        00487300
487400 25110-POST-ACL-ERROR.                                            00487400
487500     MOVE 'G75' TO WS-CURRENT-ERROR.                              00487500
487600     PERFORM 90000-POST-ERROR THRU 90000-EXIT.                    00487600
487700 25110-EXIT.                                                      00487700
487800     EXIT.                                                        00487800
487900/                                                                 00487900
488000 25200-SCAN-ADL-REC.                                              00488000
488100     IF GAC-INDEX = GAC-ENTRY-COUNT                               00488100
488200         GO TO 25200-EXIT.                                        00488200
488300     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '01'                       00488300
488400         IF GAC-DEDL-L-O-B (GAC-INDEX) = '1'                      00488400
488500             GO TO 25200-EXIT                                     00488500
488600         ELSE                                                     00488600
488700             PERFORM 25210-POST-ADL-ERROR THRU 25210-EXIT         00488700
488800             SET GAC-INDEX TO GAC-ENTRY-COUNT                     00488800
488900             GO TO 25200-EXIT.                                    00488900
489000     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '02'                       00489000
489100         IF GAC-DEDL-L-O-B (GAC-INDEX) = '1' OR '2' OR '5'        00489100
489200             GO TO 25200-EXIT                                     00489200
489300         ELSE                                                     00489300
489400             PERFORM 25210-POST-ADL-ERROR THRU 25210-EXIT         00489400
489500             SET GAC-INDEX TO GAC-ENTRY-COUNT                     00489500
489600             GO TO 25200-EXIT.                                    00489600
489700     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '03'                       00489700
489800         IF GAC-DEDL-L-O-B (GAC-INDEX) = '1' OR '2' OR '3' OR     00489800
489900                                  '5' OR '6' OR '7' OR '8'        00489900
490000             GO TO 25200-EXIT                                     00490000
490100         ELSE                                                     00490100
490200             PERFORM 25210-POST-ADL-ERROR THRU 25210-EXIT         00490200
490300             SET GAC-INDEX TO GAC-ENTRY-COUNT                     00490300
490400             GO TO 25200-EXIT.                                    00490400
490500     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '04'                       00490500
490600         IF GAC-DEDL-L-O-B (GAC-INDEX) = '1' OR '3' OR '6'        00490600
490700             GO TO 25200-EXIT                                     00490700
490800         ELSE                                                     00490800
490900             PERFORM 25210-POST-ADL-ERROR THRU 25210-EXIT         00490900
491000             SET GAC-INDEX TO GAC-ENTRY-COUNT                     00491000
491100             GO TO 25200-EXIT.                                    00491100
491200     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '05'                       00491200
491300         IF GAC-DEDL-L-O-B (GAC-INDEX) = '2'                      00491300
491400             GO TO 25200-EXIT                                     00491400
491500         ELSE                                                     00491500
491600             PERFORM 25210-POST-ADL-ERROR THRU 25210-EXIT         00491600
491700             SET GAC-INDEX TO GAC-ENTRY-COUNT                     00491700
491800             GO TO 25200-EXIT.                                    00491800
491900     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '06'                       00491900
492000         IF GAC-DEDL-L-O-B (GAC-INDEX) = '2' OR '3' OR '7'        00492000
492100             GO TO 25200-EXIT                                     00492100
492200         ELSE                                                     00492200
492300             PERFORM 25210-POST-ADL-ERROR THRU 25210-EXIT         00492300
492400             SET GAC-INDEX TO GAC-ENTRY-COUNT                     00492400
492500             GO TO 25200-EXIT.                                    00492500
492600     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '07'                       00492600
492700         IF GAC-DEDL-L-O-B (GAC-INDEX) = '4'                      00492700
492800             GO TO 25200-EXIT                                     00492800
492900         ELSE                                                     00492900
493000             PERFORM 25210-POST-ADL-ERROR THRU 25210-EXIT         00493000
493100             SET GAC-INDEX TO GAC-ENTRY-COUNT                     00493100
493200             GO TO 25200-EXIT.                                    00493200
493300     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '08'                       00493300
493400         IF GAC-DEDL-L-O-B (GAC-INDEX) = '3'                      00493400
493500             GO TO 25200-EXIT                                     00493500
493600         ELSE                                                     00493600
493700             PERFORM 25210-POST-ADL-ERROR THRU 25210-EXIT         00493700
493800             SET GAC-INDEX TO GAC-ENTRY-COUNT                     00493800
493900             GO TO 25200-EXIT.                                    00493900
494000 25200-EXIT.                                                      00494000
494100     EXIT.                                                        00494100
494200 25210-POST-ADL-ERROR.                                            00494200
494300     MOVE 'G76' TO WS-CURRENT-ERROR.                              00494300
494400     PERFORM 90000-POST-ERROR THRU 90000-EXIT.                    00494400
494500 25210-EXIT.                                                      00494500
494600     EXIT.                                                        00494600
494700/                                                                 00494700
494800 25300-SCAN-AOL-REC.                                              00494800
494900     IF GAD-INDEX = GAD-ENTRY-COUNT                               00494900
495000         GO TO 25300-EXIT.                                        00495000
495100     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '01'                       00495100
495200         IF GAD-O-P-X-L-O-B (GAD-INDEX) = '1'                     00495200
495300             GO TO 25300-EXIT                                     00495300
495400         ELSE                                                     00495400
495500             PERFORM 25310-POST-AOL-ERROR THRU 25310-EXIT         00495500
495600             SET GAD-INDEX TO GAD-ENTRY-COUNT                     00495600
495700             GO TO 25300-EXIT.                                    00495700
495800     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '02'                       00495800
495900         IF GAD-O-P-X-L-O-B (GAD-INDEX) = '1' OR '2' OR '5'       00495900
496000             GO TO 25300-EXIT                                     00496000
496100         ELSE                                                     00496100
496200             PERFORM 25310-POST-AOL-ERROR THRU 25310-EXIT         00496200
496300             SET GAD-INDEX TO GAD-ENTRY-COUNT                     00496300
496400             GO TO 25300-EXIT.                                    00496400
496500     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '03'                       00496500
496600         IF GAD-O-P-X-L-O-B (GAD-INDEX) = '1' OR '2' OR '3' OR    00496600
496700                                  '5' OR '6' OR '7' OR '8'        00496700
496800             GO TO 25300-EXIT                                     00496800
496900         ELSE                                                     00496900
497000             PERFORM 25310-POST-AOL-ERROR THRU 25310-EXIT         00497000
497100             SET GAD-INDEX TO GAD-ENTRY-COUNT                     00497100
497200             GO TO 25300-EXIT.                                    00497200
497300     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '04'                       00497300
497400         IF GAD-O-P-X-L-O-B (GAD-INDEX) = '1' OR '3' OR '6'       00497400
497500             GO TO 25300-EXIT                                     00497500
497600         ELSE                                                     00497600
497700             PERFORM 25310-POST-AOL-ERROR THRU 25310-EXIT         00497700
497800             SET GAD-INDEX TO GAD-ENTRY-COUNT                     00497800
497900             GO TO 25300-EXIT.                                    00497900
498000     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '05'                       00498000
498100         IF GAD-O-P-X-L-O-B (GAD-INDEX) = '2'                     00498100
498200             GO TO 25300-EXIT                                     00498200
498300         ELSE                                                     00498300
498400             PERFORM 25310-POST-AOL-ERROR THRU 25310-EXIT         00498400
498500             SET GAD-INDEX TO GAD-ENTRY-COUNT                     00498500
498600             GO TO 25300-EXIT.                                    00498600
498700     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '06'                       00498700
498800         IF GAD-O-P-X-L-O-B (GAD-INDEX) = '2' OR '3' OR '7'       00498800
498900             GO TO 25300-EXIT                                     00498900
499000         ELSE                                                     00499000
499100             PERFORM 25310-POST-AOL-ERROR THRU 25310-EXIT         00499100
499200             SET GAD-INDEX TO GAD-ENTRY-COUNT                     00499200
499300             GO TO 25300-EXIT.                                    00499300
499400     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '07'                       00499400
499500         IF GAD-O-P-X-L-O-B (GAD-INDEX) = '4'                     00499500
499600             GO TO 25300-EXIT                                     00499600
499700         ELSE                                                     00499700
499800             PERFORM 25310-POST-AOL-ERROR THRU 25310-EXIT         00499800
499900             SET GAD-INDEX TO GAD-ENTRY-COUNT                     00499900
500000             GO TO 25300-EXIT.                                    00500000
500100     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '08'                       00500100
500200         IF GAD-O-P-X-L-O-B (GAD-INDEX) = '3'                     00500200
500300             GO TO 25300-EXIT                                     00500300
500400         ELSE                                                     00500400
500500             PERFORM 25310-POST-AOL-ERROR THRU 25310-EXIT         00500500
500600             SET GAD-INDEX TO GAD-ENTRY-COUNT                     00500600
500700             GO TO 25300-EXIT.                                    00500700
500800 25300-EXIT.                                                      00500800
500900     EXIT.                                                        00500900
501000 25310-POST-AOL-ERROR.                                            00501000
501100     MOVE 'G77' TO WS-CURRENT-ERROR.                              00501100
501200     PERFORM 90000-POST-ERROR THRU 90000-EXIT.                    00501200
501300 25310-EXIT.                                                      00501300
501400     EXIT.                                                        00501400
501500/                                                                 00501500
501600 25400-SCAN-ACP-REC.                                              00501600
501700     IF GAF-INDEX = GAF-ENTRY-COUNT                               00501700
501800         GO TO 25400-EXIT.                                        00501800
501900     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '01'                       00501900
502000         IF GAF-COPAY-L-O-B (GAF-INDEX) = '1'                     00502000
502100             GO TO 25400-EXIT                                     00502100
502200         ELSE                                                     00502200
502300             PERFORM 25410-POST-ACP-ERROR THRU 25410-EXIT         00502300
502400             SET GAF-INDEX TO GAF-ENTRY-COUNT                     00502400
502500             GO TO 25400-EXIT.                                    00502500
502600     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '02'                       00502600
502700         IF GAF-COPAY-L-O-B (GAF-INDEX) = '1' OR '2' OR '5'       00502700
502800             GO TO 25400-EXIT                                     00502800
502900         ELSE                                                     00502900
503000             PERFORM 25410-POST-ACP-ERROR THRU 25410-EXIT         00503000
503100             SET GAF-INDEX TO GAF-ENTRY-COUNT                     00503100
503200             GO TO 25400-EXIT.                                    00503200
503300     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '03'                       00503300
503400         IF GAF-COPAY-L-O-B (GAF-INDEX) = '1' OR '2' OR '3' OR    00503400
503500                                  '5' OR '6' OR '7' OR '8'        00503500
503600             GO TO 25400-EXIT                                     00503600
503700         ELSE                                                     00503700
503800             PERFORM 25410-POST-ACP-ERROR THRU 25410-EXIT         00503800
503900             SET GAF-INDEX TO GAF-ENTRY-COUNT                     00503900
504000             GO TO 25400-EXIT.                                    00504000
504100     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '04'                       00504100
504200         IF GAF-COPAY-L-O-B (GAF-INDEX) = '1' OR '3' OR '6'       00504200
504300             GO TO 25400-EXIT                                     00504300
504400         ELSE                                                     00504400
504500             PERFORM 25410-POST-ACP-ERROR THRU 25410-EXIT         00504500
504600             SET GAF-INDEX TO GAF-ENTRY-COUNT                     00504600
504700             GO TO 25400-EXIT.                                    00504700
504800     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '05'                       00504800
504900         IF GAF-COPAY-L-O-B (GAF-INDEX) = '2'                     00504900
505000             GO TO 25400-EXIT                                     00505000
505100         ELSE                                                     00505100
505200             PERFORM 25410-POST-ACP-ERROR THRU 25410-EXIT         00505200
505300             SET GAF-INDEX TO GAF-ENTRY-COUNT                     00505300
505400             GO TO 25400-EXIT.                                    00505400
505500     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '06'                       00505500
505600         IF GAF-COPAY-L-O-B (GAF-INDEX) = '2' OR '3' OR '7'       00505600
505700             GO TO 25400-EXIT                                     00505700
505800         ELSE                                                     00505800
505900             PERFORM 25410-POST-ACP-ERROR THRU 25410-EXIT         00505900
506000             SET GAF-INDEX TO GAF-ENTRY-COUNT                     00506000
506100             GO TO 25400-EXIT.                                    00506100
506200     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '07'                       00506200
506300         IF GAF-COPAY-L-O-B (GAF-INDEX) = '4'                     00506300
506400             GO TO 25400-EXIT                                     00506400
506500         ELSE                                                     00506500
506600             PERFORM 25410-POST-ACP-ERROR THRU 25410-EXIT         00506600
506700             SET GAF-INDEX TO GAF-ENTRY-COUNT                     00506700
506800             GO TO 25400-EXIT.                                    00506800
506900     IF GCG-L-O-B-CONTRACT-LEVEL-IND = '08'                       00506900
507000         IF GAF-COPAY-L-O-B (GAF-INDEX) = '3'                     00507000
507100             GO TO 25400-EXIT                                     00507100
507200         ELSE                                                     00507200
507300             PERFORM 25410-POST-ACP-ERROR THRU 25410-EXIT         00507300
507400             SET GAF-INDEX TO GAF-ENTRY-COUNT                     00507400
507500             GO TO 25400-EXIT.                                    00507500
507600 25400-EXIT.                                                      00507600
507700     EXIT.                                                        00507700
507800 25410-POST-ACP-ERROR.                                            00507800
507900     MOVE 'G76' TO WS-CURRENT-ERROR.                              00507900
508000     PERFORM 90000-POST-ERROR THRU 90000-EXIT.                    00508000
508100 25410-EXIT.                                                      00508100
508200     EXIT.                                                        00508200
508300/                                                                 00508300
      *  MOO3                                                           00508310
508400******************************************************************00508400
508500** HERE WE ARE ADDING ANOTHER ERROR TABLE ENTRY TO THE          **00508500
508600** DISCREPANCY FILE RECORD WHICH WE ARE BUILDING.               **00508600
508700******************************************************************00508700
508800 90000-POST-ERROR.                                                00508800
508900                                                                  00508900
509000     IF DF-ERROR-INDEX = 200                                      00509000
509100         DISPLAY 'GC024020 WARNING - DISCREPANCY RECORD OVERFLOW' 00509100
509200         ' ON KEY=' DF-RECORD-KEY                                 00509200
509300         GO TO 90000-EXIT.                                        00509300
509400                                                                  00509400
509500     ADD 1 TO DF-ERROR-COUNT.                                     00509500
509600     MOVE WS-CURRENT-ERROR TO DF-ERR-CODE (DF-ERROR-INDEX).       00509600
509700     SET DF-ERROR-INDEX UP BY 1.                                  00509700
509800     MOVE 'Y' TO WS-ERROR-SW.                                     00509800
509900                                                                  00509900
510000 90000-EXIT. EXIT.                                                00510000
510100                                                                  00510100
510200 91000-READ-GROUP.                                                00510200
510300                                                                  00510300
510400     MOVE LINK-GROUP-KEY TO GCG-GRP-SPECIF-ID.                    00510400
510500     MOVE 'R'     TO REQUEST-TYPE-1.                              00510500
510600     MOVE +30     TO ONEA-REC-LENG.                               00510600
510700     CALL 'TSGVSAM3' USING PARM-ONE PARM-ONEA.                    00510700
510800     IF  REQUEST-TYPE-1 = '3'                                     00510800
510900         DISPLAY 'GC024020 GROUP SPECIFIC NOT FOUND '             00510900
511000             LG-GROUP-NUM   ' '                                   00511000
511100             LG-SECTION-NUM ' '                                   00511100
511200             LG-FAM-REL-LVL ' '                                   00511200
511300             LG-EFF-DATE                                          00511300
511400     ELSE                                                         00511400
511500         IF  REQUEST-TYPE-1 NOT = 'R'                             00511500
511600             DISPLAY 'GC024020 - BAD READ - GRPSPC FILE '         00511600
511700                         LINK-GROUP-KEY                           00511700
511800             MOVE SET-FEEDBACK          TO ABEND-CODE             00511800
511900             GO TO 99999-ERROR-RTN.                               00511900
512000                                                                  00512000
512100 91000-EXIT. EXIT.                                                00512100
512200                                                                  00512200
512300 92000-READ-TABULAR.                                              00512300
512400                                                                  00512400
512500     MOVE WS-TAB-KEY TO TWOA-REC-AREA00.                          00512500
512600     MOVE 14  TO TWOA-REC-LENG.                                   00512600
512700     MOVE 'R' TO REQUEST-TYPE-2.                                  00512700
512800     CALL 'TSGVSAM4' USING PARM-TWO PARM-TWOA.                    00512800
512900     IF  REQUEST-TYPE-2 = '3'                                     00512900
513000         DISPLAY 'GC024020 TABULAR NOT FOUND '                    00513000
513100             WS-TAB-ID ' ' WS-TAB-SLOT                            00513100
513200     ELSE                                                         00513200
513300         IF  REQUEST-TYPE-2 NOT = 'R'                             00513300
513400             DISPLAY 'GC024020--BAD READ TABULAR '                00513400
513500             MOVE TWOA-FEEDBACK  TO ABEND-CODE                    00513500
513600             GO TO 99999-ERROR-RTN.                               00513600
513700                                                                  00513700
513800 92000-EXIT. EXIT.                                                00513800
513900                                                                  00513900
514000                                                                  00514000
514100 99300-CLOSE-FILES.                                               00514100
514200                                                                  00514200
514300******************************************************************00514300
514400** GROUP SPECIFIC FILE - CLOSE                                  **00514400
514500******************************************************************00514500
514600     MOVE 'C' TO REQUEST-TYPE-1.                                  00514600
514700     CALL 'TSGVSAM3' USING PARM-ONE PARM-ONEA.                    00514700
514800     IF  REQUEST-TYPE-1 NOT = 'C'                                 00514800
514900         DISPLAY 'GC024020--BAD CLOSE GRPSPC '                    00514900
515000         MOVE ONEA-FEEDBACK  TO ABEND-CODE                        00515000
515100         GO TO 99999-ERROR-RTN.                                   00515100
515200                                                                  00515200
515300******************************************************************00515300
515400** TABULAR FILE - CLOSE                                         **00515400
515500******************************************************************00515500
515600     MOVE 'C' TO REQUEST-TYPE-2.                                  00515600
515700     CALL 'TSGVSAM4' USING PARM-TWO PARM-TWOA.                    00515700
515800     IF  REQUEST-TYPE-2 NOT = 'C'                                 00515800
515900         DISPLAY 'GC024020--BAD CLOSE TABULAR'                    00515900
516000         MOVE TWOA-FEEDBACK  TO ABEND-CODE                        00516000
516100         GO TO 99999-ERROR-RTN.                                   00516100
516200                                                                  00516200
516300 99300-EXIT. EXIT.                                                00516300
516400                                                                  00516400
516500 99998-FALL-THROUGH-TRAP.                                         00516500
516600                                                                  00516600
516700     DISPLAY 'GC024020 - FALL-THROUGH LOGIC ERROR'.               00516700
516800     MOVE +0001 TO ABEND-CODE.                                    00516800
516900                                                                  00516900
517000 99999-ERROR-RTN.                                                 00517000
517100                                                                  00517100
517200     CALL 'TSGEND' USING ABEND-CODE.                              00517200
517300                                                                  00517300
517400 99999-EXIT. EXIT.                                                00517400
