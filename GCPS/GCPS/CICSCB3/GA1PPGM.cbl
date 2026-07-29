000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID. GA1PPGM.                                             00000200
000300 AUTHOR. GARY D MULLINGS.                                         00000300
000400 DATE-WRITTEN. SEPT 1998.                                         00000400
000500 DATE-COMPILED.                                                   00000500
000600                                                                  00000600
000700******************************************************************00000700
000800*   GA1PPGM         ALL LEVEL TABULAR MAINTENANCE PROGRAM        *00000800
000900*                       ACCUMULATOR COPAY   - #ACP               *00000900
001000*                                                                *00001000
001100*     THIS PROGRAM WILL PERFORM ADD/CHANGE/DELETE MAINTENANCE TO *00001100
001200*   ENTRIES ON THE ALL LEVEL TABULAR RECORD.  THE TABULAR RECORD *00001200
001300*   CAN CONTAIN UP TO 29 ENTRIES IN A TABLE, EACH ENTRY HAS A    *00001300
001400*   NUMBER OF FIELDS AND ANOTHER SMALL TABLE, THIS 2NDARY TABLE  *00001400
001500*   IS A POINTER TO AN INTERNAL TABULAR RECORD.  THE PROGRAM     *00001500
001600*   OPERATES IN TWO MODES AN ADD/CHANGE AND A CHANGE/DELETE MODE.*00001600
001700*                                                                *00001700
001800*     THE CHG/DEL SCREEN WILL DISPLAY AN ENTRY CURRENTLY ON THE  *00001800
001900*   ALL LEVEL TABULAR RECORD.  THE OPERATOR WILL THEN CHANGE ANY *00001900
002000*   FIELD OR ADD, CHANGE, OR DELETE AN INTERNAL TABULAR; THERE IS*00002000
002100*   ALSO THE OPTION OF DELETING THE WHOLE ENTRY IN THE TABULAR,  *00002100
002200*   INTERNAL TABULARS INCLUDED, THIS OPTION CAN BE SELECTED BY   *00002200
002300*   PLACING A 'D' IN THE DELETE OPTION FIELD.                    *00002300
002400*                                                                *00002400
002500*    THE CHG/ADD SCREEN WILL BE SHOWN THE OPERATOR WHEN THEY WANT*00002500
002600*   TO ADD A NEW ENTRY INTO THE TABLE. FROM HERE THE OPERATOR CAN*00002600
002700*   FILL THE ENTRY, THEN REVIEW AND CHANGE THE NEW ENTRY.  AFTER *00002700
002800*   THE OPERATOR KEYS ENTER ON THE REVIEW SCREEN, THE PROGRAM    *00002800
002900*   ASSUMES THAT THEY WANT TO ADD ANOTHER ENTRY AND SO DISPLAYS  *00002900
003000*   THE SKELETON FOR THE OPERATOR TO OVERLAY.                    *00003000
003100*                                                                *00003100
003200*   FUNC CODE: GA1P                                              *00003200
003300*                          ********************************      *00003300
003400*                          *   THIS MAPSET IS SHARED BY   *      *00003400
003500*                          *   THE FOLLOWING MODULES:     *      *00003500
003600*                          *   1. GA1BPGM                 *      *00003600
003700*                          *   2. GA1CPGM                 *      *00003700
003800*                          *   3. GA1PPGM                 *      *00003800
003900*   MAPSET:    GA1XSETC ==>*   4. GA1EPGM                 *      *00003900
004000*                          *   5. GA1PPGM                 *      *00004000
004100*                          *   6. GASEDIT1                *      *00004100
004200*                          *   7. GACDEPGM                *      *00004200
004300*                          *   8. GK1BPGM                 *      *00004300
004400*                          *   9. GK1CPGM                 *      *00004400
004500*                          *  10. GK1DPGM                 *      *00004500
004600*                          *  11. GK1EPGM                 *      *00004600
004700*                          *  12. GK1PPGM                 *      *00004700
004800*                          *  13. GAS1UPD                 *      *00004800
004900*                          *  14. GAS2UPD                 *      *00004900
005000*                          *  15. GAS3UPD                 *      *00005000
005100*                          *  16. GAS4UPD                 *      *00005100
005200*                          ********************************      *00005200
005300*                                                                *00005300
005400*   FILES:     GCPSWORK     GCTABULR                             *00005400
005500*              GCCONTR      GCGRPSPC                             *00005500
005600*              GCSTABLR     GCSPROVN                             *00005600
005700*                                                                *00005700
005800******************************************************************00005800
005900******************************************************************00005900
006000*    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         *00006000
006100*    *-*         U P D A T E   H I S T O R Y         *-*         *00006100
006200*    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         *00006200
006300* *-NUM-* *-DATE-* *WHO* *-----------DESCRIPTION-----------------*00006300
006400*                                                                *00006400
006500*   D341  09/24/98  GDM  INITIAL MODULE                          *00006500
006600*                                                                *00006600
006700*  P????  11/19/99  FRY  ADD LENGTH PARAMETER TO THE RETURN      *00006700
006800*                        COMMAND WHEN DFHCOMMAREA IS SPECIFIED.  *00006800
006900*                                                                *00006900
007000*  P????  07/10/00  GSP  ADDED LOGIC FOR NEW #IPGS INTERNAL      *00007000
007100*                        TABULAR.                                *00007100
007200*                                                                *00007200
007300*   D352  09/19/00  GDM  ADD ACCUMULATOR IDENTIFIER              *00007300
007400*                                                                *00007400
007500*         11/16/01  AKK  ADD SUPPORT FOR 4 NEW BITS, TWO FOR     *00007500
007600*                        EMER AND TWO FOR SERIOUS MENTAL ILLNESS *00007600
007700*                                                                *00007700
007800*  D365B 06/03/02  JP ADD COMBINATION APPLIED IND (CAPI)         *00007800
007900*                                                                *00007900
008000*  D368  06/04/02  JP ADD SELECTIVE ADDITIONAL BENEFIT           *00008000
008100*                         DETERMINATION (SABD)                   *00008100
008200*                                                                *00008200
008300*            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *00008300
008400*                                                                *00008400
008500*            06-25-03   DAF   USE COPYBOOK GCTIPGPC INSTEAD OF   *00008500
008600*                             GCTIPGTC                           *00008600
008700*                                                                *00008700
008800* P09400     11-07-06   GF    ADD ASCEND/DESCEND AND BISCENDING  *00008800
008900*                             INDICATORS                         *00008900
009000*            05-07-07   LR    RECOMPILE FOR CHANGES IN GASEDIT1  *00009000
009100*                                                                *00009100
009000*            10-15-10   MJL   ALLOW 'UNL' VALUE.                 *00009110
009100*                                                                *00009120
      * P21595 09/19/16   HSB CHANGES FOR GCPS NEW FIELDS BENEFIT      *00009130
      *                       TYPE CODE,TIER CODE,TIER LEVEL.          *00009150
SI0724*                                                                *00009160
SI0724* P56703 05/08/24   SI  RECOMPILE - PEAQ COPYBOOK EXPANSION      *00009170
SI0724*                           COPY ABM, ACP, ACL, ADL, AOL,        *00009180
SI0724*                           GCCDRLEN                             *00009190
009200******************************************************************00009200
009300    SKIP3                                                         00009300
009400 ENVIRONMENT DIVISION.                                            00009400
009500/    D A T A   D I V I S I O N                                    00009500
009600 DATA DIVISION.                                                   00009600
009700 WORKING-STORAGE SECTION.                                         00009700
009800 01  WS-BEGIN                    PIC X(24)  VALUE                 00009800
009900     '***GA1PPGM WS BEGINS***'.                                   00009900
010000                                                                  00010000
010100*     T I T L E   L I N E S                                       00010100
010200 01  WS-TITLE-LINES.                                              00010200
010300 COPY GCMHLINE.                                                   00010300
010400/     A L T E R N A T I V E   W O R K F I L E   K E Y S           00010400
010500 01  FILLER                      PIC X(32)  VALUE                 00010500
010600     '*** ALTERNATIVE WORKFILE KEY ***'.                          00010600
010700 01  SAVE-WS-ALT-WORKFILE-KEYS.                                   00010700
010800     05 FILLER                   PIC X(63) VALUE SPACES.          00010800
010900                                                                  00010900
011000 01  WS-ALT-WORKFILE-KEYS.                                        00011000
011100 COPY GCWRKKEY.                                                   00011100
011200                                                                  00011200
011300/    D A T E   F O R M A T T I N G   A R E A                      00011300
011400 01  HGADATES-COMMAREA.                                           00011400
011500 COPY HGCDAT01.                                                   00011500
011600                                                                  00011600
011700*  *** WORKFIELDS, AND SWITCHES **                                00011700
011800 01  WS-WORK-FIELDS.                                              00011800
011900                                                                  00011900
012000     05  GCTRSRT-COMMAREA-LEN      PIC S9(4)  COMP VALUE +100.    00012000
012100                                                                  00012100
012200     05  WS-HEX-00                     PIC X    VALUE LOW-VALUE.  00012200
012300     05  WS-ONE-LOW                    PIC X VALUE LOW-VALUES.    00012300
012400     05  SAVE-COPY-FROM-SLOT           PIC 9(7).                  00012400
012500                                                                  00012500
012600     05  WS-CDE-REQUEST-CODES.                                    00012600
012700         10  WS-REQUEST-4500-CDE-PROTECT    PIC X(4) VALUE '4500'.00012700
012800         10  WS-REQUEST-4600-CDE-STATUS     PIC X(4) VALUE '4600'.00012800
012900         10  WS-REQUEST-4700-CNTL-UPDATE    PIC X(4) VALUE '4700'.00012900
013000                                                                  00013000
013100*     I N T E R N A L   T A B U L A R   P R O G R A M   N A M E   00013100
013200 01  WS-INTERNAL-TABULAR-PGM-ID        PIC X(8).                  00013200
013300                                                                  00013300
013400                                                                  00013400
013500** ***ALL LEVEL TABULAR ENTRY SAVED HERE DURING SORT ***          00013500
013600 01  WS-ENTRY                          PIC X(176).                00013600
013700     SKIP3                                                        00013700
013800/    A T T R I B U T E S                                          00013800
013900 COPY DFHBMSCA.                                                   00013900
014000     02  DFHBMABF                PIC X VALUE '9'.                 00014000
014100/    A T T E N T I O N   I D E N T I F I E R S                    00014100
014200 COPY DFHAID.                                                     00014200
014300/    R E C O R D   L E N G T H S                                  00014300
014400                                                                  00014400
014500 01  WS-RECORD-LENGTHS.                                           00014500
SI0724*   05 WS-COPY-TABLE-LEN              PIC S9(4) COMP  VALUE +7744.00014600
SI0724    05 WS-COPY-TABLE-LEN              PIC S9(4) COMP VALUE +30800.00014610
014700    05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP  VALUE +0.   00014700
014800    05 WS-IO-PARM-WRK-BEN-PROV-LEN    PIC S9(4) COMP  VALUE +0.   00014800
014900    05 WS-IO-PARM-WRK-CONTRACT-LEN    PIC S9(4) COMP  VALUE +0.   00014900
015000    05 WS-IO-PARM-WRK-CONTROL-LEN     PIC S9(4) COMP  VALUE +0.   00015000
015100    05 WS-IO-PARM-WRK-GRP-SPEC-LEN    PIC S9(4) COMP  VALUE +0.   00015100
015200    05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP  VALUE +0.   00015200
015300    05 WS-WRK-BEN-PROV-LEN            PIC S9(4) COMP  VALUE +0.   00015300
015400    05 WS-WRK-CONTRACT-LEN            PIC S9(4) COMP  VALUE +0.   00015400
015500    05 WS-WRK-GRP-SPEC-LEN            PIC S9(4) COMP  VALUE +0.   00015500
015600                                                                  00015600
015700******************************************************************00015700
015800** REQUIRED FOR N118 - DISPLAY OF TABULAR OCCURS (INDEX)        **00015800
015900******************************************************************00015900
016000 01  CURNT-OCURS-BIN             PIC 9(4)  COMP.                  00016000
016100 01  CURNT-OCURS-PKD             PIC 9(4).                        00016100
016200 01  CURNT-OCURS-ALH      REDEFINES   CURNT-OCURS-PKD.            00016200
NSK24 *    05  FILLER                  PIC XX.                          00016300
NSK24 *    05  CURNT-OCCURS-OUT        PIC XX.                          00016400
NSK24      05  FILLER                  PIC X.                           00016410
NSK24      05  CURNT-OCCURS-OUT        PIC XXX.                         00016420
016500                                                                  00016500
016600 01  TOTAL-OCURS-UNK             PIC 9(5).                        00016600
016700 01  TOTAL-OCURS-ALH      REDEFINES   TOTAL-OCURS-UNK.            00016700
NSK24 *    05  FILLER                  PIC XXX.                         00016800
NSK24 *    05  TOTAL-OCCURS-OUT        PIC XX.                          00016900
NSK24      05  FILLER                  PIC XX.                          00016910
NSK24      05  TOTAL-OCCURS-OUT        PIC XXX.                         00016920
017000/    G C   R E C O R D S   L E N G T H S                          00017000
017100 01  WS-GC-RECORD-LENGTHS.                                        00017100
017200     COPY GCCDRLEN.                                               00017200
017300/    A B E N D   A R E A                                          00017300
017400                                                                  00017400
017500 01  WS-01-ABEND-AREA.                                            00017500
017600     05  FILLER                   PIC X(16)  VALUE                00017600
017700         '** ABEND AREA **'.                                      00017700
017800                                                                  00017800
017900     05  WS-ABCODE-CODES-AND-MSG.                                 00017900
018000         10  WS-ABCODE                  PIC X(04)  VALUE  SPACES. 00018000
018100         10  WS-ABCODE-MSG              PIC X(79)  VALUE  SPACES. 00018100
018200                                                                  00018200
018300         10  WS-ABCODE-1PC1             PIC X(04)  VALUE  '1PC1'. 00018300
018400         10  WS-ABCODE-1PC1-MSG         PIC X(79)  VALUE          00018400
018500             '*** INVALID PARAMETER LENGTH FOUND ***              00018500
018600-            '                           '.                       00018600
018700         10  WS-ABCODE-1PC2             PIC X(04)  VALUE  '1PC2'. 00018700
018800         10  WS-ABCODE-1PC2-MSG         PIC X(79)  VALUE          00018800
018900             '*** WRONG RECORD STATUS PASSED TO THIS PGM ***      00018900
019000-            '                           '.                       00019000
019100         10  WS-ABCODE-1PC3             PIC X(04)  VALUE  '1PC3'. 00019100
019200         10  WS-ABCODE-1PC3-MSG         PIC X(79)  VALUE          00019200
019300             '*** WRONG RECORD TYPE PASSED TO THIS PGM ***        00019300
019400-            '                           '.                       00019400
019500         10  WS-ABCODE-1PF1             PIC X(04)  VALUE  '1PF1'. 00019500
019600         10  WS-ABCODE-1PF1-MSG         PIC X(79)  VALUE          00019600
019700             '*** A SKELETON CAN NOT BE FOUND FOR AN INTERNAL TABU00019700
019800-            'LAR.  CONTACT SYSTEMS ***  '.                       00019800
019900         10  WS-ABCODE-1PF2             PIC X(04)  VALUE  '1PF2'. 00019900
020000         10  WS-ABCODE-1PF2-MSG         PIC X(79)  VALUE          00020000
020100             '*** INVALID INTERNAL TABULAR SITUATION FOUND. PLEASE00020100
020200-            ' CONTACT SYSTEMS ***       '.                       00020200
020300         10  WS-ABCODE-1PF3             PIC X(04)  VALUE  '1PF3'. 00020300
020400         10  WS-ABCODE-1PF3-MSG         PIC X(79)  VALUE          00020400
020500             '*** INVALID INTERNAL TABULAR SITUATION FOUND. PLEASE00020500
020600-            ' CONTACT SYSTEMS ***       '.                       00020600
020700         10  WS-ABCODE-1PF4             PIC X(04)  VALUE  '1PF4'. 00020700
020800         10  WS-ABCODE-1PF4-MSG         PIC X(79)  VALUE          00020800
020900             '*** ERROR REWRITING ALL LEVEL TABULAR.  PLEASE CONTA00020900
021000-            'CT SYSTEMS ***             '.                       00021000
021100         10  WS-ABCODE-1PF5             PIC X(04)  VALUE  '1PF5'. 00021100
021200         10  WS-ABCODE-1PF5-MSG         PIC X(79)  VALUE          00021200
021300             'THE INTERNAL TABULAR CAN NOT BE READ FROM THE WORKFI00021300
021400-            'LE.  PLEASE CONTACT SYSTEMS'.                       00021400
021500         10  WS-ABCODE-1PF6             PIC X(04)  VALUE  '1PF6'. 00021500
021600         10  WS-ABCODE-1PF6-MSG         PIC X(79)  VALUE          00021600
021700             'THE INTERNAL TABULAR CAN NOT BE ADDED TO THE WORKFIL00021700
021800-            'E.  PLEASE CONTACT SYSTEMS '.                       00021800
021900         10  WS-ABCODE-1PF7             PIC X(04)  VALUE  '1PF7'. 00021900
022000         10  WS-ABCODE-1PF7-MSG         PIC X(79)  VALUE          00022000
022100             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00022100
022200-            ' SYSTEMS ***               '.                       00022200
022300         10  WS-ABCODE-1PF9             PIC X(04)  VALUE  '1PF9'. 00022300
022400         10  WS-ABCODE-1PF9-MSG         PIC X(79)  VALUE          00022400
022500             'THE INTERNAL TABULAR CAN NOT BE ADDED TO THE WORFILE00022500
022600-            '.  PLEASE CONTACT SYSTEMS  '.                       00022600
022700         10  WS-ABCODE-1PFA             PIC X(04)  VALUE  '1PFA'. 00022700
022800         10  WS-ABCODE-1PFA-MSG         PIC X(79)  VALUE          00022800
022900             '*** THE INTERNAL TABULAR CAN NOT BE DELETED, PLEASE 00022900
023000-            'CONTACT SYSTEMS ***        '.                       00023000
023100         10  WS-ABCODE-1PFB             PIC X(04)  VALUE  '1PFB'. 00023100
023200         10  WS-ABCODE-1PFB-MSG         PIC X(79)  VALUE          00023200
023300             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00023300
023400-            ' SYSTEMS ***               '.                       00023400
023500         10  WS-ABCODE-1PFC             PIC X(04)  VALUE  '1PFC'. 00023500
023600         10  WS-ABCODE-1PFC-MSG         PIC X(79)  VALUE          00023600
023700             '*** ERROR WHEN DELETING INTERNAL TAB.  PLEASE CONTAC00023700
023800-            'T SYSTEMS ***              '.                       00023800
023900         10  WS-ABCODE-1PFJ             PIC X(04)  VALUE  '1PFJ'. 00023900
024000         10  WS-ABCODE-1PFJ-MSG         PIC X(79)  VALUE          00024000
024100             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00024100
024200-            ' SYSTEMS ***               '.                       00024200
024300         10  WS-ABCODE-1PFK             PIC X(04)  VALUE  '1PFK'. 00024300
024400         10  WS-ABCODE-1PFK-MSG         PIC X(79)  VALUE          00024400
024500             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00024500
024600-            ' SYSTEMS ***               '.                       00024600
024700         10  WS-ABCODE-1PFL             PIC X(04)  VALUE  '1PFL'. 00024700
024800         10  WS-ABCODE-1PFL-MSG         PIC X(79)  VALUE          00024800
024900             '*** ERROR READING GROUP SPECIFIC RECORD TO RETURN TO00024900
025000-            'MENU.  CONTACT SYSTEMS *** '.                       00025000
025100         10  WS-ABCODE-1PFM             PIC X(04)  VALUE  '1PFM'. 00025100
025200         10  WS-ABCODE-1PFM-MSG         PIC X(79)  VALUE          00025200
025300             '*** ERROR READING CONTRACT MASTER TO RETURN TO THE  00025300
025400-            'MENU.  CONTACT SYSTEMS *** '.                       00025400
025500         10  WS-ABCODE-1PFN             PIC X(04)  VALUE  '1PFN'. 00025500
025600         10  WS-ABCODE-1PFN-MSG         PIC X(79)  VALUE          00025600
025700             '*** ERROR READING BENEFIT PROV RECORD TO RETURN TO M00025700
025800-            'ENU.  CONTACT SYSTEMS ***  '.                       00025800
025900         10  WS-ABCODE-1PFO             PIC X(04)  VALUE  '1PFO'. 00025900
026000         10  WS-ABCODE-1PFO-MSG         PIC X(79)  VALUE          00026000
026100             '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACT00026100
026200-            ' SYSTEMS ***               '.                       00026200
026300         10  WS-ABCODE-1PFP             PIC X(04)  VALUE  '1PFP'. 00026300
026400         10  WS-ABCODE-1PFP-MSG         PIC X(79)  VALUE          00026400
026500             '*** ERROR READING W/F CONTROL RECORD. PLEASE CONTACT00026500
026600-            ' SYSTEMS ***               '.                       00026600
026700         10  WS-ABCODE-1PFQ             PIC X(04)  VALUE  '1PFQ'. 00026700
026800         10  WS-ABCODE-1PFQ-MSG         PIC X(79)  VALUE          00026800
026900             '*** ERROR REWRITING W/F CONTROL RECORD. PLEASE CONTA00026900
027000-            'CT SYSTEMS ***             '.                       00027000
027100         10  WS-ABCODE-1PFR             PIC X(04)  VALUE  '1PFR'. 00027100
027200         10  WS-ABCODE-1PFR-MSG         PIC X(79)  VALUE          00027200
027300             '*** ERROR READING W/F ALL LVL TAB.    PLEASE CONTACT00027300
027400-            ' SYSTEMS ***               '.                       00027400
027500         10  WS-ABCODE-1PFS             PIC X(04)  VALUE  '1PFS'. 00027500
027600         10  WS-ABCODE-1PFS-MSG         PIC X(79)  VALUE          00027600
027700             '*** ERROR REWRITING W/F ALL LVL TAB.  PLEASE CONTACT00027700
027800-            ' SYSTEMS ***               '.                       00027800
027900         10  WS-ABCODE-1PFT             PIC X(04)  VALUE  '1PFT'. 00027900
028000         10  WS-ABCODE-1PFT-MSG         PIC X(79)  VALUE          00028000
028100             '*** ERROR READING W/F CONTROL RECORD. PLEASE CONTACT00028100
028200-            ' SYSTEMS ***               '.                       00028200
028300         10  WS-ABCODE-1PFU             PIC X(04)  VALUE  '1PFU'. 00028300
028400         10  WS-ABCODE-1PFU-MSG         PIC X(79)  VALUE          00028400
028500             '*** ERROR REWRITING W/F CONTROL RECORD. PLEASE CONTA00028500
028600-            'CT SYSTEMS ***             '.                       00028600
028700         10  WS-ABCODE-1PL1             PIC X(04)  VALUE  '1PL1'. 00028700
028800         10  WS-ABCODE-1PL1-MSG         PIC X(79)  VALUE          00028800
028900             '*** THE OCCURS WE ARE TO UPDATE HAS BEEN DELETED ***00028900
029000-            '                           '.                       00029000
029100         10  WS-ABCODE-1PLX             PIC X(04)  VALUE  '1PLX'. 00029100
029200         10  WS-ABCODE-1PLX-MSG         PIC X(79)  VALUE          00029200
029300             '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTE00029300
029400-            'MS ***                     '.                       00029400
029500         10  WS-ABCODE-1PP1             PIC X(04)  VALUE  '1PP1'. 00029500
029600         10  WS-ABCODE-1PP1-MSG         PIC X(79)  VALUE          00029600
029700             '????????????????????????????????????????????????????00029700
029800-            '???????????????????????????'.                       00029800
029900                                                                  00029900
030000/    M E S S A G E   T A B L E                                    00030000
030100******************************************************************00030100
030200 01  WT-01-TABLE.                                                 00030200
030300     05  FILLER                  PIC X(16) VALUE                  00030300
030400         '* WT-01-TABLE  *'.                                      00030400
030500                                                                  00030500
030600 01  FILLER.                                                      00030600
030700     05  WT-01-MESSAGE-VALUES.                                    00030700
030800*----------------------------------------------------------------*00030800
030900         10  WT-01-ENTRY-001.                                     00030900
031000             15  FILLER              PIC X(2)  VALUE '¬>'.        00031000
031100             15  WT-01-MESSAGE-TEXT-001.                          00031100
031200                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00031200
031300                 20  FILLER          PIC X(1)  VALUE  '-'.        00031300
031400                 20  FILLER          PIC X(3)  VALUE  '001'.      00031400
031500                 20  FILLER          PIC X(1)  VALUE  ' '.        00031500
031600                 20  FILLER          PIC X(70) VALUE              00031600
031700                     '#IBGR HAS BEEN SUCCESSFULLY MAPPED          00031700
031800-                    '                         '.                 00031800
031900             15  FILLER              PIC X(2)  VALUE '<¬'.        00031900
032000*----------------------------------------------------------------*00032000
032100         10  WT-01-ENTRY-002.                                     00032100
032200             15  FILLER              PIC X(2)  VALUE '¬>'.        00032200
032300             15  WT-01-MESSAGE-TEXT-002.                          00032300
032400                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00032400
032500                 20  FILLER          PIC X(1)  VALUE  '-'.        00032500
032600                 20  FILLER          PIC X(3)  VALUE  '002'.      00032600
032700                 20  FILLER          PIC X(1)  VALUE  ' '.        00032700
032800                 20  FILLER          PIC X(70) VALUE              00032800
032900                     '#IPGN HAS BEEN SUCCESSFULLY MAPPED          00032900
033000-                    '                         '.                 00033000
033100             15  FILLER              PIC X(2)  VALUE '<¬'.        00033100
033200*----------------------------------------------------------------*00033200
033300         10  WT-01-ENTRY-003.                                     00033300
033400             15  FILLER              PIC X(2)  VALUE '¬>'.        00033400
033500             15  WT-01-MESSAGE-TEXT-003.                          00033500
033600                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00033600
033700                 20  FILLER          PIC X(1)  VALUE  '-'.        00033700
033800                 20  FILLER          PIC X(3)  VALUE  '003'.      00033800
033900                 20  FILLER          PIC X(1)  VALUE  ' '.        00033900
034000                 20  FILLER          PIC X(70) VALUE              00034000
034100                     '#IPGT HAS BEEN SUCCESSFULLY MAPPED          00034100
034200-                    '                         '.                 00034200
034300             15  FILLER              PIC X(2)  VALUE '<¬'.        00034300
034400*----------------------------------------------------------------*00034400
034500         10  WT-01-ENTRY-004.                                     00034500
034600             15  FILLER              PIC X(2)  VALUE '¬>'.        00034600
034700             15  WT-01-MESSAGE-TEXT-004.                          00034700
034800                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00034800
034900                 20  FILLER          PIC X(1)  VALUE  '-'.        00034900
035000                 20  FILLER          PIC X(3)  VALUE  '004'.      00035000
035100                 20  FILLER          PIC X(1)  VALUE  ' '.        00035100
035200                 20  FILLER          PIC X(70) VALUE              00035200
035300                     '#IDGD HAS BEEN SUCCESSFULLY MAPPED          00035300
035400-                    '                         '.                 00035400
035500             15  FILLER              PIC X(2)  VALUE '<¬'.        00035500
035600*----------------------------------------------------------------*00035600
035700         10  WT-01-ENTRY-005.                                     00035700
035800             15  FILLER              PIC X(2)  VALUE '¬>'.        00035800
035900             15  WT-01-MESSAGE-TEXT-005.                          00035900
036000                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00036000
036100                 20  FILLER          PIC X(1)  VALUE  '-'.        00036100
036200                 20  FILLER          PIC X(3)  VALUE  '005'.      00036200
036300                 20  FILLER          PIC X(1)  VALUE  ' '.        00036300
036400                 20  FILLER          PIC X(70) VALUE              00036400
036500                     '#IPGP HAS BEEN SUCCESSFULLY MAPPED          00036500
036600-                    '                         '.                 00036600
036700             15  FILLER              PIC X(2)  VALUE '<¬'.        00036700
036800*----------------------------------------------------------------*00036800
036900         10  WT-01-ENTRY-006.                                     00036900
037000             15  FILLER              PIC X(2)  VALUE '¬>'.        00037000
037100             15  WT-01-MESSAGE-TEXT-006.                          00037100
037200                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00037200
037300                 20  FILLER          PIC X(1)  VALUE  '-'.        00037300
037400                 20  FILLER          PIC X(3)  VALUE  '006'.      00037400
037500                 20  FILLER          PIC X(1)  VALUE  ' '.        00037500
037600                 20  FILLER          PIC X(70) VALUE              00037600
037700                     'DELETE OPTION MUST BE \
037800-                    'VALID                    '.                 00037800
037900             15  FILLER              PIC X(2)  VALUE '<¬'.        00037900
038000*----------------------------------------------------------------*00038000
038100         10  WT-01-ENTRY-007.                                     00038100
038200             15  FILLER              PIC X(2)  VALUE '¬>'.        00038200
038300             15  WT-01-MESSAGE-TEXT-007.                          00038300
038400                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00038400
038500                 20  FILLER          PIC X(1)  VALUE  '-'.        00038500
038600                 20  FILLER          PIC X(3)  VALUE  '007'.      00038600
038700                 20  FILLER          PIC X(1)  VALUE  ' '.        00038700
038800                 20  FILLER          PIC X(70) VALUE              00038800
038900                     'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESS00038900
039000-                    ' PF4/PF16 TO CONTINUE    '.                 00039000
039100             15  FILLER              PIC X(2)  VALUE '<¬'.        00039100
039200*----------------------------------------------------------------*00039200
039300         10  WT-01-ENTRY-008.                                     00039300
039400             15  FILLER              PIC X(2)  VALUE '¬>'.        00039400
039500             15  WT-01-MESSAGE-TEXT-008.                          00039500
039600                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00039600
039700                 20  FILLER          PIC X(1)  VALUE  '-'.        00039700
039800                 20  FILLER          PIC X(3)  VALUE  '008'.      00039800
039900                 20  FILLER          PIC X(1)  VALUE  ' '.        00039900
040000                 20  FILLER          PIC X(70) VALUE              00040000
040100                     'GROUP IN CONVERSION STATUS, CANNOT CHANGE HI00040100
040200-                    'GH-LIGHTED ELEMENTS      '.                 00040200
040300             15  FILLER              PIC X(2)  VALUE '<¬'.        00040300
040400*----------------------------------------------------------------*00040400
040500         10  WT-01-ENTRY-009.                                     00040500
040600             15  FILLER              PIC X(2)  VALUE '¬>'.        00040600
040700             15  WT-01-MESSAGE-TEXT-009.                          00040700
040800                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00040800
040900                 20  FILLER          PIC X(1)  VALUE  '-'.        00040900
041000                 20  FILLER          PIC X(3)  VALUE  '009'.      00041000
041100                 20  FILLER          PIC X(1)  VALUE  ' '.        00041100
041200                 20  FILLER          PIC X(70) VALUE              00041200
041300                     'INVALID PFKEY SELECTION                     00041300
041400-                    '                         '.                 00041400
041500             15  FILLER              PIC X(2)  VALUE '<¬'.        00041500
041600*----------------------------------------------------------------*00041600
041700         10  WT-01-ENTRY-010.                                     00041700
041800             15  FILLER              PIC X(2)  VALUE '¬>'.        00041800
041900             15  WT-01-MESSAGE-TEXT-010.                          00041900
042000                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00042000
042100                 20  FILLER          PIC X(1)  VALUE  '-'.        00042100
042200                 20  FILLER          PIC X(3)  VALUE  '010'.      00042200
042300                 20  FILLER          PIC X(1)  VALUE  ' '.        00042300
042400                 20  FILLER          PIC X(70) VALUE              00042400
042500                     'INVALID REQUEST.  THAT PF KEY HAS NO MEANING00042500
042600-                    ' TO THIS PROGRAM         '.                 00042600
042700             15  FILLER              PIC X(2)  VALUE '<¬'.        00042700
042800*----------------------------------------------------------------*00042800
042900         10  WT-01-ENTRY-011.                                     00042900
043000             15  FILLER              PIC X(2)  VALUE '¬>'.        00043000
043100             15  WT-01-MESSAGE-TEXT-011.                          00043100
043200                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00043200
043300                 20  FILLER          PIC X(1)  VALUE  '-'.        00043300
043400                 20  FILLER          PIC X(3)  VALUE  '011'.      00043400
043500                 20  FILLER          PIC X(1)  VALUE  ' '.        00043500
043600                 20  FILLER          PIC X(70) VALUE              00043600
043700                     'NO CHANGE FOUND - NO CHANGE MADE, WHAT NEXT 00043700
043800-                    '                         '.                 00043800
043900             15  FILLER              PIC X(2)  VALUE '<¬'.        00043900
044000*----------------------------------------------------------------*00044000
044100         10  WT-01-ENTRY-012.                                     00044100
044200             15  FILLER              PIC X(2)  VALUE '¬>'.        00044200
044300             15  WT-01-MESSAGE-TEXT-012.                          00044300
044400                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00044400
044500                 20  FILLER          PIC X(1)  VALUE  '-'.        00044500
044600                 20  FILLER          PIC X(3)  VALUE  '012'.      00044600
044700                 20  FILLER          PIC X(1)  VALUE  ' '.        00044700
044800                 20  FILLER          PIC X(70) VALUE              00044800
044900                     'NO ENTRIES TO DISPLAY                       00044900
045000-                    '                         '.                 00045000
045100             15  FILLER              PIC X(2)  VALUE '<¬'.        00045100
045200*----------------------------------------------------------------*00045200
045300         10  WT-01-ENTRY-013.                                     00045300
045400             15  FILLER              PIC X(2)  VALUE '¬>'.        00045400
045500             15  WT-01-MESSAGE-TEXT-013.                          00045500
045600                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00045600
045700                 20  FILLER          PIC X(1)  VALUE  '-'.        00045700
045800                 20  FILLER          PIC X(3)  VALUE  '013'.      00045800
045900                 20  FILLER          PIC X(1)  VALUE  ' '.        00045900
046000                 20  FILLER          PIC X(70) VALUE              00046000
046100                     'PFKEY INVALID WHILE ERRORS NOT CORRECTED, HI00046100
046200-                    'T ENTER FOR ERR MSG      '.                 00046200
046300             15  FILLER              PIC X(2)  VALUE '<¬'.        00046300
046400*----------------------------------------------------------------*00046400
046500         10  WT-01-ENTRY-014.                                     00046500
046600             15  FILLER              PIC X(2)  VALUE '¬>'.        00046600
046700             15  WT-01-MESSAGE-TEXT-014.                          00046700
046800                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00046800
046900                 20  FILLER          PIC X(1)  VALUE  '-'.        00046900
047000                 20  FILLER          PIC X(3)  VALUE  '014'.      00047000
047100                 20  FILLER          PIC X(1)  VALUE  ' '.        00047100
047200                 20  FILLER          PIC X(70) VALUE              00047200
047300                     'PROCESSING FROM THE TOP OF THE LIST         00047300
047400-                    '                         '.                 00047400
047500             15  FILLER              PIC X(2)  VALUE '<¬'.        00047500
047600*----------------------------------------------------------------*00047600
047700         10  WT-01-ENTRY-015.                                     00047700
047800             15  FILLER              PIC X(2)  VALUE '¬>'.        00047800
047900             15  WT-01-MESSAGE-TEXT-015.                          00047900
048000                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00048000
048100                 20  FILLER          PIC X(1)  VALUE  '-'.        00048100
048200                 20  FILLER          PIC X(3)  VALUE  '015'.      00048200
048300                 20  FILLER          PIC X(1)  VALUE  ' '.        00048300
048400                 20  FILLER          PIC X(70) VALUE              00048400
048500                     'THE MAXIMUM NUMBER OF ENTRIES HAVE BEEN ADDE00048500
048600-                    'D                        '.                 00048600
048700             15  FILLER              PIC X(2)  VALUE '<¬'.        00048700
048800*----------------------------------------------------------------*00048800
048900         10  WT-01-ENTRY-016.                                     00048900
049000             15  FILLER              PIC X(2)  VALUE '¬>'.        00049000
049100             15  WT-01-MESSAGE-TEXT-016.                          00049100
049200                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00049200
049300                 20  FILLER          PIC X(1)  VALUE  '-'.        00049300
049400                 20  FILLER          PIC X(3)  VALUE  '016'.      00049400
049500                 20  FILLER          PIC X(1)  VALUE  ' '.        00049500
049600                 20  FILLER          PIC X(70) VALUE              00049600
049700                     'THE TABULAR ALREADY CONTAINS THE MAXIMUM NUM00049700
049800-                    'BER OF OCCURANCES        '.                 00049800
049900             15  FILLER              PIC X(2)  VALUE '<¬'.        00049900
050000*----------------------------------------------------------------*00050000
050100         10  WT-01-ENTRY-017.                                     00050100
050200             15  FILLER              PIC X(2)  VALUE '¬>'.        00050200
050300             15  WT-01-MESSAGE-TEXT-017.                          00050300
050400                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00050400
050500                 20  FILLER          PIC X(1)  VALUE  '-'.        00050500
050600                 20  FILLER          PIC X(3)  VALUE  '017'.      00050600
050700                 20  FILLER          PIC X(1)  VALUE  ' '.        00050700
050800                 20  FILLER          PIC X(70) VALUE              00050800
050900                     'THE TABULAR RECORD DOES NOT EXIST, AND CANNO00050900
051000-                    'T BE CHANGED             '.                 00051000
051100             15  FILLER              PIC X(2)  VALUE '<¬'.        00051100
051200*----------------------------------------------------------------*00051200
051300         10  WT-01-ENTRY-018.                                     00051300
051400             15  FILLER              PIC X(2)  VALUE '¬>'.        00051400
051500             15  WT-01-MESSAGE-TEXT-018.                          00051500
051600                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00051600
051700                 20  FILLER          PIC X(1)  VALUE  '-'.        00051700
051800                 20  FILLER          PIC X(3)  VALUE  '018'.      00051800
051900                 20  FILLER          PIC X(1)  VALUE  ' '.        00051900
052000                 20  FILLER          PIC X(70) VALUE              00052000
052100                     'THE TABULAR RECORD DOES NOT EXIST, AND CANNO00052100
052200-                    'T BE MAPPED              '.                 00052200
052300             15  FILLER              PIC X(2)  VALUE '<¬'.        00052300
052400*----------------------------------------------------------------*00052400
052500         10  WT-01-ENTRY-019.                                     00052500
052600             15  FILLER              PIC X(2)  VALUE '¬>'.        00052600
052700             15  WT-01-MESSAGE-TEXT-019.                          00052700
052800                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00052800
052900                 20  FILLER          PIC X(1)  VALUE  '-'.        00052900
053000                 20  FILLER          PIC X(3)  VALUE  '019'.      00053000
053100                 20  FILLER          PIC X(1)  VALUE  ' '.        00053100
053200                 20  FILLER          PIC X(70) VALUE              00053200
053300                     'THERE ARE NO MORE ENTRIES TO DISPLAY        00053300
053400-                    '                         '.                 00053400
053500             15  FILLER              PIC X(2)  VALUE '<¬'.        00053500
053600*----------------------------------------------------------------*00053600
053700         10  WT-01-ENTRY-020.                                     00053700
053800             15  FILLER              PIC X(2)  VALUE '¬>'.        00053800
053900             15  WT-01-MESSAGE-TEXT-020.                          00053900
054000                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00054000
054100                 20  FILLER          PIC X(1)  VALUE  '-'.        00054100
054200                 20  FILLER          PIC X(3)  VALUE  '020'.      00054200
054300                 20  FILLER          PIC X(1)  VALUE  ' '.        00054300
054400                 20  FILLER          PIC X(70) VALUE              00054400
054500                     'THIS IS THE FIRST ON THE TABLE              00054500
054600-                    '                         '.                 00054600
054700             15  FILLER              PIC X(2)  VALUE '<¬'.        00054700
054800*----------------------------------------------------------------*00054800
054900         10  WT-01-ENTRY-021.                                     00054900
055000             15  FILLER              PIC X(2)  VALUE '¬>'.        00055000
055100             15  WT-01-MESSAGE-TEXT-021.                          00055100
055200                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00055200
055300                 20  FILLER          PIC X(1)  VALUE  '-'.        00055300
055400                 20  FILLER          PIC X(3)  VALUE  '021'.      00055400
055500                 20  FILLER          PIC X(1)  VALUE  ' '.        00055500
055600                 20  FILLER          PIC X(70) VALUE              00055600
055700                     'THIS IS THE LAST ON THE TABLE               00055700
055800-                    '                         '.                 00055800
055900             15  FILLER              PIC X(2)  VALUE '<¬'.        00055900
056000*----------------------------------------------------------------*00056000
056100         10  WT-01-ENTRY-022.                                     00056100
056200             15  FILLER              PIC X(2)  VALUE '¬>'.        00056200
056300             15  WT-01-MESSAGE-TEXT-022.                          00056300
056400                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00056400
056500                 20  FILLER          PIC X(1)  VALUE  '-'.        00056500
056600                 20  FILLER          PIC X(3)  VALUE  '022'.      00056600
056700                 20  FILLER          PIC X(1)  VALUE  ' '.        00056700
056800                 20  FILLER          PIC X(70) VALUE              00056800
056900                     'THIS PFKEY NOT VALID WHILE IN CHG/ADD MODE  00056900
057000-                    '                         '.                 00057000
057100             15  FILLER              PIC X(2)  VALUE '<¬'.        00057100
057200*----------------------------------------------------------------*00057200
057300         10  WT-01-ENTRY-023.                                     00057300
057400             15  FILLER              PIC X(2)  VALUE '¬>'.        00057400
057500             15  WT-01-MESSAGE-TEXT-003.                          00057500
057600                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00057600
057700                 20  FILLER          PIC X(1)  VALUE  '-'.        00057700
057800                 20  FILLER          PIC X(3)  VALUE  '023'.      00057800
057900                 20  FILLER          PIC X(1)  VALUE  ' '.        00057900
058000                 20  FILLER          PIC X(70) VALUE              00058000
058100                     '#IPGS HAS BEEN SUCCESSFULLY MAPPED          00058100
058200-                    '                         '.                 00058200
058300             15  FILLER              PIC X(2)  VALUE '<¬'.        00058300
058400*----------------------------------------------------------------*00058400
058500         10  WT-01-ENTRY-024.                                     00058500
058600             15  FILLER              PIC X(2)  VALUE '¬>'.        00058600
058700             15  WT-01-MESSAGE-TEXT-023.                          00058700
058800                 20  FILLER          PIC X(4)  VALUE  'GA1P'.     00058800
058900                 20  FILLER          PIC X(1)  VALUE  '-'.        00058900
059000                 20  FILLER          PIC X(3)  VALUE  '024'.      00059000
059100                 20  FILLER          PIC X(1)  VALUE  ' '.        00059100
059200                 20  FILLER          PIC X(70) VALUE              00059200
059300                     '********** F U T U R E   U S E *************00059300
059400-                    '*************************'.                 00059400
059500             15  FILLER              PIC X(2)  VALUE '<¬'.        00059500
059600*----------------------------------------------------------------*00059600
059700                                                                  00059700
059800     05  WT-01-MESSAGE-TABLE         REDEFINES                    00059800
059900         WT-01-MESSAGE-VALUES         OCCURS 024 TIMES            00059900
060000                                     INDEXED BY WT-01-INDEX.      00060000
060100         10  WT-01-ENTRY.                                         00060100
060200             15  FILLER              PIC X(02).                   00060200
060300             15  WT-01-MESSAGE-TEXT  PIC X(79).                   00060300
060400             15  FILLER              PIC X(02).                   00060400
060500                                                                  00060500
060600 01  WS-END                      PIC X(16)  VALUE                 00060600
060700     '*** W/S ENDS ***'.                                          00060700
060800/    L I N K A G E   S E C T I O N                                00060800
060900 LINKAGE SECTION.                                                 00060900
061000 01  DFHCOMMAREA.                                                 00061000
061100 COPY G2ALCKEC.                                                   00061100
061200 COPY GACDACWB.                                                   00061200
061300                                                                  00061300
061400     05  GAS5UPD-PASSED-AREA-2.                                   00061400
061500         07  LVL2-B-SW-2             PIC X.                       00061500
061600         07  LVL2-F-SW-2             PIC X.                       00061600
061700         07  LVL2-G-SW-2             PIC X.                       00061700
061800         07  INTR-TAB-PGM-ID-2       PIC X(8).                    00061800
061900         07  FILLER-2                PIC X(09).                   00061900
062000     05  DELADD-OPTION-2             PIC X(7).                    00062000
062100                                                                  00062100
062200/*****************************************************************00062200
062300* W O R K F I L E   -   A L L   L E V E L   T A B   R E C O R D   00062300
062400******************************************************************00062400
062500 01  WF-IO-PARM-ALL-LVL-TAB-RECORD.                               00062500
062600 COPY GCIOPRM1.                                                   00062600
062700 COPY GCWRKDCC.                                                   00062700
062800 COPY GCTACPC.                                                    00062800
062900                                                                  00062900
063000/    C O P Y   T A B U L A R   T A B L E   A R E A                00063000
063100 01  COPY-TABULAR-TABLE-AREA.                                     00063100
SI0724*    05  COPY-TABULAR-TABLE  OCCURS  44 TIMES INDEXED BY          00063200
SI0724     05  COPY-TABULAR-TABLE  OCCURS 175 TIMES INDEXED BY          00063210
063300         COPY-IDX, COPY-IDX2, COPY-IDX3, COPY-IDX4.               00063300
063400       10  COPY-SORTABLE-FLDS.                                    00063400
063500           15  FILLER                      PIC X(169).            00063500
063600           15  COPY-SORT-FYI               PIC X(003).            00063600
063700       10  COPY-SORT-ENTRY-CNTR            PIC S9(7) COMP-3.      00063700
063800                                                                  00063800
063900/*****************************************************************00063900
064000* W O R K F I L E   -   I N T E R N A L   T A B U L A R   R E C   00064000
064100******************************************************************00064100
064200 01  WF-IO-PARM-INTERNAL-TAB-RECORD.                              00064200
064300 COPY GCIOPRM2.                                                   00064300
064400 COPY GCWRKDC2.                                                   00064400
064500 COPY GCTIPGPC.                                                   00064500
064600                                                                  00064600
064700/*****************************************************************00064700
064800* W O R K F I L E   -   G R O U P   S P E C I F I C   R E C       00064800
064900******************************************************************00064900
065000 01  WF-IO-PARM-WRK-GRP-SPEC-REC.                                 00065000
065100 COPY GCIOPRM3.                                                   00065100
065200 COPY GCWRKDC3.                                                   00065200
065300 COPY GCGROUPC.                                                   00065300
065400                                                                  00065400
065500/*****************************************************************00065500
065600* W O R K F I L E   -   C O N T R A C T   R E C O R D             00065600
065700******************************************************************00065700
065800 01  WF-IO-PARM-WRK-CONTRACT-REC.                                 00065800
065900 COPY GCIOPRM4.                                                   00065900
066000 COPY GCWRKDC4.                                                   00066000
066100 COPY GCCONTRC.                                                   00066100
066200                                                                  00066200
066300/*****************************************************************00066300
066400* W O R K F I L E   -   B E N E F I T   P R O V I S I O N   R E C 00066400
066500******************************************************************00066500
066600 01  WF-IO-PARM-WRK-BEN-PROV-REC.                                 00066600
066700 COPY GCIOPRM5.                                                   00066700
066800 COPY GCWRKDC5.                                                   00066800
066900 COPY GCBENPVC.                                                   00066900
067000                                                                  00067000
067100/*****************************************************************00067100
067200* W O R K F I L E   -  C O N T R O L   R E C O R D                00067200
067300******************************************************************00067300
067400 01  WF-IO-PARM-WRK-CONTROL-REC.                                  00067400
067500 COPY GCIOPRM6.                                                   00067500
067600 COPY GCWRKDC6.                                                   00067600
067700 COPY GCCCRDCC.                                                   00067700
067800                                                                  00067800
067900/*****************************************************************00067900
068000* P R O D U C T I O N   -   C O N T R A C T   R E C O R D         00068000
068100******************************************************************00068100
068200 01  PR-IO-PARM-WRK-CONTRACT-REC.                                 00068200
068300 COPY GCIOPRM7   SUPPRESS.                                        00068300
068400 COPY GCWRKDC7   SUPPRESS.                                        00068400
068500 COPY GCCONTR2   SUPPRESS.                                        00068500
068600                                                                  00068600
068700******************************************************************00068700
068800* P R O D U C T I O N   -   G R O U P   S P E C I F I C   R E C   00068800
068900******************************************************************00068900
069000 01  PR-IO-PARM-WRK-GRP-SPEC-REC.                                 00069000
069100 COPY GCIOPRM8   SUPPRESS.                                        00069100
069200 COPY GCWRKDC8   SUPPRESS.                                        00069200
069300 COPY GCGROUP2   SUPPRESS.                                        00069300
069400                                                                  00069400
069500******************************************************************00069500
069600* P R O D U C T I O N   -   B E N E F I T   P V S N   R E C O R D 00069600
069700******************************************************************00069700
069800 01  PR-IO-PARM-WRK-BEN-PROV-REC.                                 00069800
069900 COPY GCIOPRM9   SUPPRESS.                                        00069900
070000 COPY GCWRKDC9   SUPPRESS.                                        00070000
070100 COPY GCBENPV2   SUPPRESS.                                        00070100
070200                                                                  00070200
070300******************************************************************00070300
070400* P R O D U C T I O N   -   A L L   L E V E L   T A B   R E C     00070400
070500******************************************************************00070500
070600 01  PR-IO-PARM-ALL-LVL-TAB-RECORD.                               00070600
070700 COPY GCIOPRMA   SUPPRESS.                                        00070700
070800 COPY GCWRKDCA   SUPPRESS.                                        00070800
070900 COPY GCTACP2    SUPPRESS.                                        00070900
071000                                                                  00071000
071100/*****************************************************************00071100
071200*    M A P S E T   A R E A                                        00071200
071300******************************************************************00071300
071400     COPY GA1XSETC.                                               00071400
071500                                                                  00071500
071600/*****************************************************************00071600
071700*     A L L   L V L   A C C U M   C O M M O N   W O R K A R E A S 00071700
071800******************************************************************00071800
071900*  *** UPDATE/DELETE MODULE GAS5UPD COMMAREA ***                  00071900
072000*  *** WILL BE THE SAME COMMON WORK AREA + GAS5UPD COMMAREA ***   00072000
072100                                                                  00072100
072200 01  COMMON-WORKAREAS.                                            00072200
072300 COPY G2ALCKE2.                                                   00072300
072400 COPY GACDACWA.                                                   00072400
072500                                                                  00072500
072600     05  GAS5UPD-PASSED-AREA.                                     00072600
072700         07  LVL2-B-SW                PIC X.                      00072700
072800         07  LVL2-F-SW                PIC X.                      00072800
072900         07  LVL2-G-SW                PIC X.                      00072900
073000         07  INTR-TAB-PGM-ID          PIC X(8).                   00073000
073100         07  FILLER                   PIC X(09).                  00073100
073200     05  DELADD-OPTION                PIC X(7).                   00073200
073300                                                                  00073300
073400/    P R O C E D U R E   D I V I S I O N                          00073400
073500 PROCEDURE DIVISION.                                              00073500
073600                                                                  00073600
073700******************************************************************00073700
073800* 0000  HOUSEKEEPING                                             *00073800
073900******************************************************************00073900
074000 0000-000-HOUSEKEEPING          SECTION.                          00074000
074100 0000-010.                                                        00074100
074200                                                                  00074200
074300     EXEC CICS GETMAIN                                            00074300
074400               SET(ADDRESS OF COMMON-WORKAREAS)                   00074400
074500               INITIMG(WS-HEX-00)                                 00074500
074600               LENGTH(LENGTH OF COMMON-WORKAREAS)                 00074600
074700               END-EXEC.                                          00074700
074800                                                                  00074800
074900     MOVE ZEROES  TO  ACWA-CDE-1U-COUNT,  ACWA-CDE-2B-COUNT.      00074900
075000                                                                  00075000
075100     EXEC CICS GETMAIN                                            00075100
075200               SET(ADDRESS OF GA1XI01I)                           00075200
075300               INITIMG(WS-HEX-00)                                 00075300
075400               LENGTH(LENGTH OF GA1XI01I)                         00075400
075500               END-EXEC.                                          00075500
075600                                                                  00075600
075700     SET ACWA-MAPSET-PNTR  TO  ADDRESS OF  GA1XI01I.              00075700
075800                                                                  00075800
075900                                                                  00075900
076000     MOVE  +19   TO  GCVI-COMMAREA-LEN.                           00076000
076100     MOVE  'N'   TO  ACWA-ERROR-SW                                00076100
076200                     ACWA-CDE-FIELD-CHANGE-IND                    00076200
076300                     ACWA-CDE-REC-CHANGE-IND                      00076300
076400                     ACWA-CDE-RESET-WF-IND.                       00076400
076500     MOVE  ZERO  TO  ACWA-FIELD-CHG-CNT.                          00076500
076600     MOVE  SPACE TO  ACWA-CDE-STATUS-CHANGE-IND                   00076600
076700                     ACWA-CDE-INTERNAL-TAB-IND.                   00076700
076800                                                                  00076800
076900     COMPUTE WS-IO-PARM-WRK-GRP-SPEC-LEN =                        00076900
077000             GC-GCIOPARM-LEN             +                        00077000
077100             GC-WORKFILE-KEY-LEN         +                        00077100
077200             GC-GCGRPSPC-FIXED-LEN       +                        00077200
077300            (GC-GCGRPSPC-VARY-LEN        *                        00077300
077400             GC-GCGRPSPC-VARY-MAX-OCUR).                          00077400
077500                                                                  00077500
077600     COMPUTE WS-WRK-GRP-SPEC-LEN         =                        00077600
077700             GC-WORKFILE-KEY-LEN         +                        00077700
077800             GC-GCGRPSPC-FIXED-LEN       +                        00077800
077900            (GC-GCGRPSPC-VARY-LEN        *                        00077900
078000             GC-GCGRPSPC-VARY-MAX-OCUR).                          00078000
078100                                                                  00078100
078200     COMPUTE WS-IO-PARM-WRK-CONTRACT-LEN =                        00078200
078300             GC-GCIOPARM-LEN             +                        00078300
078400             GC-WORKFILE-KEY-LEN         +                        00078400
078500             GC-GCCONTR-FIXED-LEN        +                        00078500
078600            (GC-GCCONTR-VARY-LEN         *                        00078600
078700             GC-GCCONTR-VARY-MAX-OCUR).                           00078700
078800                                                                  00078800
078900     COMPUTE WS-WRK-CONTRACT-LEN         =                        00078900
079000             GC-WORKFILE-KEY-LEN         +                        00079000
079100             GC-GCCONTR-FIXED-LEN        +                        00079100
079200            (GC-GCCONTR-VARY-LEN         *                        00079200
079300             GC-GCCONTR-VARY-MAX-OCUR).                           00079300
079400                                                                  00079400
079500     COMPUTE WS-IO-PARM-WRK-BEN-PROV-LEN =                        00079500
079600             GC-GCIOPARM-LEN             +                        00079600
079700             GC-WORKFILE-KEY-LEN         +                        00079700
079800             GC-GCBENPRV-FIXED-LEN       +                        00079800
079900            (GC-GCBENPRV-VARY-LEN        *                        00079900
080000             GC-GCBENPRV-VARY-MAX-OCUR).                          00080000
080100                                                                  00080100
080200     COMPUTE WS-WRK-BEN-PROV-LEN         =                        00080200
080300             GC-WORKFILE-KEY-LEN         +                        00080300
080400             GC-GCBENPRV-FIXED-LEN       +                        00080400
080500            (GC-GCBENPRV-VARY-LEN        *                        00080500
080600             GC-GCBENPRV-VARY-MAX-OCUR).                          00080600
080700                                                                  00080700
080800     COMPUTE WS-IO-PARM-WRK-CONTROL-LEN  =                        00080800
080900             GC-GCIOPARM-LEN             +                        00080900
081000             GC-WORKFILE-KEY-LEN         +                        00081000
081100             GC-WORKFILE-CONTROL-REC-LEN.                         00081100
081200                                                                  00081200
081300     IF EIBAID  =  DFHCLEAR                                       00081300
081400         EXEC CICS SEND FROM(WS-ONE-LOW)                          00081400
081500                        ERASE                                     00081500
081600         END-EXEC                                                 00081600
081700         EXEC CICS RETURN                                         00081700
081800         END-EXEC.                                                00081800
081900                                                                  00081900
082000     EXEC CICS  HANDLE  CONDITION                                 00082000
082100                MAPFAIL(6400-000-XCTL-TO-MAIN-MENU)  END-EXEC.    00082100
082200                                                                  00082200
082300 0000-900-EXIT.                                                   00082300
082400          EXIT.                                                   00082400
082500/*****************************************************************00082500
082600* 1000  MAIN LINE                                                *00082600
082700******************************************************************00082700
082800 1000-000-MAIN-LINE             SECTION.                          00082800
082900 1000-010.                                                        00082900
083000                                                                  00083000
083100     IF  EIBTRNID  NOT =  'GA1P'                                  00083100
083200         PERFORM 4000-000-DISPLAY-FIRST-SCREEN.                   00083200
083300                                                                  00083300
083400     EXEC CICS  RECEIVE   MAP('GA1XI01')  MAPSET('GA1XSET')       00083400
083500                END-EXEC.                                         00083500
083600                                                                  00083600
083700     IF  SCRNIDNI  NOT =  '001P00'                                00083700
083800         PERFORM 6400-000-XCTL-TO-MAIN-MENU.                      00083800
083900                                                                  00083900
084000     IF  FRMNUIDI = 'GS3A'                                        00084000
084100         MOVE IDLINEI   TO   GROUP-SPECIFIC-ID-LINE.              00084100
084200     IF  FRMNUIDI = 'GC4A' OR 'GTM1'                              00084200
084300         MOVE IDLINEI   TO   CONTRACT-ID-LINE.                    00084300
084400     IF  FRMNUIDI = 'GC8A'                                        00084400
084500         MOVE IDLINEI   TO   BENEFIT-PROVISION-ID-LINE.           00084500
084600                                                                  00084600
084700     IF EIBAID = DFHPF1 OR DFHPF13                                00084700
084800        PERFORM 8000-000-SWITCH-ADD-DEL-MODE.                     00084800
084900                                                                  00084900
085000     IF EIBAID = DFHPF3 OR DFHPF15                                00085000
085100        PERFORM 5000-000-XCTL-TO-PREVIOUS-MENU.                   00085100
085200                                                                  00085200
085300     IF (EIBAID  =  DFHPF7 OR  DFHPF19 OR  DFHPF8 OR  DFHPF20) AND00085300
085400        DELADDI  =  'CHG/ADD'                                     00085400
085500     THEN                                                         00085500
085600         SET  WT-01-INDEX                     TO +22              00085600
085700         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00085700
085800         MOVE -1                              TO  PERIODL         00085800
085900         GO TO 1000-900-EXIT.                                     00085900
086000                                                                  00086000
086100     IF (EIBAID  =  DFHPF7 OR  DFHPF19 OR  DFHPF8 OR  DFHPF20) AND00086100
086200        DELOPTNI  =  'D'                                          00086200
086300     THEN                                                         00086300
086400         SET  WT-01-INDEX                     TO +06              00086400
086500         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00086500
086600         MOVE -1                              TO  DELOPTNL        00086600
086700         GO TO 1000-900-EXIT.                                     00086700
086800                                                                  00086800
086900     PERFORM 1100-000-VALIDATE-SCREEN.                            00086900
087000                                                                  00087000
087100     IF  EIBAID  = DFHPF4 OR DFHPF16  AND                         00087100
087200         ACWA-SCREEN-HAS-ERRORS                                   00087200
087300     THEN                                                         00087300
087400         SET  WT-01-INDEX                     TO +13              00087400
087500         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00087500
087600         GO TO 1000-900-EXIT.                                     00087600
087700                                                                  00087700
087800     IF  EIBAID  = DFHPF4 OR DFHPF16                              00087800
087900     THEN                                                         00087900
088000         IF  ACWA-SCREEN-HAS-NO-ERRORS                            00088000
088100         THEN                                                     00088100
088200             IF  GCVI-TABLE-SW = 'N'                              00088200
088300             THEN                                                 00088300
088400                 PERFORM 2000-000-PROCESS-REQUEST                 00088400
088500                 GO TO  1000-990-RETURN                           00088500
088600             ELSE                                                 00088600
088700                 SET  WT-01-INDEX                     TO +09      00088700
088800                 MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO  00088800
088900                 MOVE -1                              TO PERIODL  00088900
089000                 GO TO 1000-900-EXIT                              00089000
089100         ELSE                                                     00089100
089200             NEXT SENTENCE                                        00089200
089300     ELSE                                                         00089300
089400         NEXT SENTENCE.                                           00089400
089500                                                                  00089500
089600     IF  ACWA-SCREEN-HAS-ERRORS                                   00089600
089700         GO TO 1000-900-EXIT.                                     00089700
089800                                                                  00089800
089900     IF  EIBAID  =  DFHENTER OR                                   00089900
090000                    DFHPF7   OR  DFHPF19 OR   DFHPF8 OR  DFHPF20  00090000
090100     THEN                                                         00090100
090200         PERFORM 2000-000-PROCESS-REQUEST                         00090200
090300                 GO TO  1000-990-RETURN.                          00090300
090400                                                                  00090400
090500     PERFORM 7900-000-RESET-ATTRIBUTES.                           00090500
090600     MOVE -1                              TO PERIODL.             00090600
090700     SET  WT-01-INDEX                     TO +10                  00090700
090800     MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO.             00090800
090900                                                                  00090900
091000                                                                  00091000
091100 1000-900-EXIT.                                                   00091100
091200                                                                  00091200
091300     PERFORM 3100-000-READ-RECORD.                                00091300
091400     MOVE GAF-ENTRY-COUNT  TO  GAF-ENTRY-COUNT.                   00091400
091500     PERFORM 9010-000-SEND-DATAONLY-RETURN.                       00091500
091600 1000-990-RETURN.                                                 00091600
091700                                                                  00091700
091800     IF DELADD-OPTION = 'GAS5UPD'                                 00091800
091900         IF INTR-TAB-PGM-ID = 'GA1GPGM'                           00091900
092000             MOVE SPACES TO DELADD-OPTION                         00092000
092100             EXEC CICS RETURN TRANSID('GA1G')                     00092100
092200                       COMMAREA(COMMON-WORKAREAS)                 00092200
092300                       END-EXEC                                   00092300
092400         ELSE                                                     00092400
092500         IF INTR-TAB-PGM-ID = 'GA2GPGM'                           00092500
092600             MOVE SPACES TO DELADD-OPTION                         00092600
092700             EXEC CICS RETURN TRANSID('GA2G')                     00092700
092800                       COMMAREA(COMMON-WORKAREAS)                 00092800
092900                       END-EXEC                                   00092900
093000         ELSE                                                     00093000
093100         IF INTR-TAB-PGM-ID = 'GA1HPGM'                           00093100
093200             MOVE SPACES TO DELADD-OPTION                         00093200
093300             EXEC CICS  RETURN TRANSID('GA1H')                    00093300
093400                        COMMAREA(COMMON-WORKAREAS)                00093400
093500                        END-EXEC                                  00093500
093600         ELSE                                                     00093600
093700         IF INTR-TAB-PGM-ID = 'GA2HPGM'                           00093700
093800             MOVE SPACES TO DELADD-OPTION                         00093800
093900             EXEC CICS  RETURN TRANSID('GA2H')                    00093900
094000                        COMMAREA(COMMON-WORKAREAS)                00094000
094100                        END-EXEC                                  00094100
094200         ELSE                                                     00094200
094300         IF INTR-TAB-PGM-ID = 'GA1SPGM'                           00094300
094400             MOVE SPACES TO DELADD-OPTION                         00094400
094500             EXEC CICS  RETURN TRANSID('GA1S')                    00094500
094600                        COMMAREA(COMMON-WORKAREAS)                00094600
094700                        END-EXEC                                  00094700
094800         ELSE                                                     00094800
094900         IF INTR-TAB-PGM-ID = 'GA1IPGM'                           00094900
095000             MOVE SPACES TO DELADD-OPTION                         00095000
095100             EXEC CICS  RETURN TRANSID('GA1I')                    00095100
095200                        COMMAREA(COMMON-WORKAREAS)                00095200
095300                        END-EXEC                                  00095300
095400         ELSE                                                     00095400
095500         IF INTR-TAB-PGM-ID = 'GA2SPGM'                           00095500
095600             MOVE SPACES TO DELADD-OPTION                         00095600
095700             EXEC CICS  RETURN TRANSID('GA2S')                    00095700
095800                        COMMAREA(COMMON-WORKAREAS)                00095800
095900                        END-EXEC                                  00095900
096000         ELSE                                                     00096000
096100         IF INTR-TAB-PGM-ID = 'GA2IPGM'                           00096100
096200             MOVE SPACES TO DELADD-OPTION                         00096200
096300             EXEC CICS  RETURN TRANSID('GA2I')                    00096300
096400                        COMMAREA(COMMON-WORKAREAS)                00096400
096500                        END-EXEC                                  00096500
096600         ELSE                                                     00096600
096700         IF INTR-TAB-PGM-ID = 'GA1NPGM'                           00096700
096800             MOVE SPACES TO DELADD-OPTION                         00096800
096900             EXEC CICS  RETURN TRANSID('GA1N')                    00096900
097000                        COMMAREA(COMMON-WORKAREAS)                00097000
097100                        END-EXEC                                  00097100
097200         ELSE                                                     00097200
097300         IF INTR-TAB-PGM-ID = 'GA2NPGM'                           00097300
097400             MOVE SPACES TO DELADD-OPTION                         00097400
097500             EXEC CICS  RETURN TRANSID('GA2N')                    00097500
097600                        COMMAREA(COMMON-WORKAREAS)                00097600
097700                        END-EXEC                                  00097700
097800         ELSE                                                     00097800
097900         IF INTR-TAB-PGM-ID = 'GA1OPGM'                           00097900
098000             MOVE SPACES TO DELADD-OPTION                         00098000
098100             EXEC CICS  RETURN TRANSID('GA1O')                    00098100
098200                        COMMAREA(COMMON-WORKAREAS)                00098200
098300                        END-EXEC                                  00098300
098400         ELSE                                                     00098400
098500         IF INTR-TAB-PGM-ID = 'GA2OPGM'                           00098500
098600             MOVE SPACES TO DELADD-OPTION                         00098600
098700             EXEC CICS  RETURN TRANSID('GA2O')                    00098700
098800                        COMMAREA(COMMON-WORKAREAS)                00098800
098900                        END-EXEC                                  00098900
099000         ELSE                                                     00099000
099100         EXEC CICS  RETURN TRANSID('GA1P')                        00099100
099200                    COMMAREA(DFHCOMMAREA)                         00099200
099300                    LENGTH  (EIBCALEN)                            00099300
099400                    END-EXEC                                      00099400
099500     ELSE                                                         00099500
099600     EXEC CICS  RETURN TRANSID('GA1P')                            00099600
099700                COMMAREA(DFHCOMMAREA)                             00099700
099800                LENGTH  (EIBCALEN)                                00099800
099900                END-EXEC.                                         00099900
100000                                                                  00100000
100100     GOBACK.                                                      00100100
100200 1000-999-EXIT.                                                   00100200
100300          EXIT.                                                   00100300
100400/*****************************************************************00100400
100500* 1100  VALIDATE SCREEN                                          *00100500
100600*                                                                *00100600
100700*    THIS IS PRIMARILY A VALIDATION ROUTINE OF DATA BEING ENTERED*00100700
100800*  BY THE OPERATOR, PLUS THE ADDITION OF SOME REINITIALIZATION.  *00100800
100900*  1. REINITIALIZE ATTRIBUTES THAT THE PROGRAM MIGHT MODIFY, AND *00100900
101000*     RESET THE ERROR MESSAGE AND DELETE OPTION TO BLANKS.       *00101000
101100*  2. INSURE THE VALIDITY OF THE OPTIONS THAT CAN BE USED FOR THE*00101100
101200*     INTERNAL TABULAR.                                          *00101200
101300******************************************************************00101300
101400 1100-000-VALIDATE-SCREEN       SECTION.                          00101400
101500 1100-010.                                                        00101500
101600                                                                  00101600
101700     SET ACWA-WF-ALL-LEVEL-TAB-PNTR TO                            00101700
101800         ADDRESS OF  WF-IO-PARM-ALL-LVL-TAB-RECORD.               00101800
101900                                                                  00101900
102000     SET ACWA-COPY-TAB-PNTR         TO                            00102000
102100         ADDRESS OF  COPY-TABULAR-TABLE-AREA.                     00102100
102200                                                                  00102200
102300     SET ACWA-WF-INTERNAL-TAB-PNTR  TO                            00102300
102400         ADDRESS OF  WF-IO-PARM-INTERNAL-TAB-RECORD.              00102400
102500                                                                  00102500
102600     SET ACWA-WF-GRP-SPEC-PNTR      TO                            00102600
102700         ADDRESS OF  WF-IO-PARM-WRK-GRP-SPEC-REC.                 00102700
102800                                                                  00102800
102900     SET ACWA-WF-CONTRACT-PNTR      TO                            00102900
103000         ADDRESS OF  WF-IO-PARM-WRK-CONTRACT-REC.                 00103000
103100                                                                  00103100
103200     SET ACWA-WF-BEN-PROV-PNTR      TO                            00103200
103300         ADDRESS OF  WF-IO-PARM-WRK-BEN-PROV-REC.                 00103300
103400                                                                  00103400
103500     SET ACWA-WF-CONTROL-RECORD-PNTR    TO                        00103500
103600         ADDRESS OF  WF-IO-PARM-WRK-CONTROL-REC.                  00103600
103700                                                                  00103700
103800     SET ACWA-PR-CONTRACT-PNTR      TO                            00103800
103900         ADDRESS OF  PR-IO-PARM-WRK-CONTRACT-REC.                 00103900
104000                                                                  00104000
104100     SET ACWA-PR-GRP-SPEC-PNTR      TO                            00104100
104200         ADDRESS OF  PR-IO-PARM-WRK-GRP-SPEC-REC.                 00104200
104300                                                                  00104300
104400     SET ACWA-PR-BEN-PROV-PNTR      TO                            00104400
104500         ADDRESS OF  PR-IO-PARM-WRK-BEN-PROV-REC.                 00104500
104600                                                                  00104600
104700     SET ACWA-PR-ALL-LEVEL-TAB-PNTR   TO                          00104700
104800         ADDRESS OF  PR-IO-PARM-ALL-LVL-TAB-RECORD.               00104800
104900                                                                  00104900
105000     MOVE 'N'              TO ACWA-ERROR-SW.                      00105000
105100     MOVE 'Y'              TO GCVI-TABLE-SW.                      00105100
105200     MOVE SPACES           TO ERRMSGO.                            00105200
105300     MOVE DFHBMFSE         TO PERIODA.                            00105300
105400     MOVE DFHBMASF         TO IBGRIDA    IPGNIDA   IPGTIDA        00105400
105500                              IDGDIDA    IPGPIDA   IPGSIDA        00105500
105600                              IBGRSLTA   IPGNSLTA  IPGTSLTA       00105600
105700                              IDGDSLTA   IPGPSLTA  IPGSSLTA.      00105700
105800                                                                  00105800
105900     PERFORM 7900-000-RESET-ATTRIBUTES.                           00105900
106000                                                                  00106000
106100*------------- LINK TO SCREEN EDIT MODULE -----------------------*00106100
106200                                                                  00106200
106300     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00106300
106400                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00106400
106500     EXEC CICS  LINK  PROGRAM('GASEDIT1')                         00106500
106600                COMMAREA (COMMON-WORKAREAS)                       00106600
106700                LENGTH(LENGTH OF COMMON-WORKAREAS)  END-EXEC.     00106700
106800                                                                  00106800
106900     IF  ACWA-SCREEN-HAS-ERRORS                                   00106900
107000         GO TO 1100-900-EXIT.                                     00107000
107100                                                                  00107100
107200     IF  ACWA-FIELD-CHG-CNT > ZEROS                               00107200
107300         GO TO 1100-900-EXIT.                                     00107300
107400                                                                  00107400
107500     IF  (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE) AND           00107500
107600         (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE) AND           00107600
107700         (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE) AND           00107700
107800         (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE) AND           00107800
107900         (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE) AND           00107900
108000         (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE)               00108000
108100     THEN                                                         00108100
108200         GO TO 1100-900-EXIT.                                     00108200
108300                                                                  00108300
108400     COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   00108400
108500              GC-GCIOPARM-LEN                 +                   00108500
108600              GC-WORKFILE-KEY-LEN             +                   00108600
108700              GC-GCTABULR-IPGP-FIXED-LEN      +                   00108700
108800             (GC-GCTABULR-IPGP-VARY-LEN       *                   00108800
108900              GC-GCTABULR-IPGP-VARY-MAX-OCUR)                     00108900
109000                                                                  00109000
109100        EXEC CICS GETMAIN                                         00109100
109200               SET(ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD)     00109200
109300               INITIMG(WS-HEX-00)                                 00109300
109400               LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)             00109400
109500               END-EXEC                                           00109500
109600                                                                  00109600
109700     SET ACWA-WF-INTERNAL-TAB-PNTR  TO                            00109700
109800         ADDRESS OF  WF-IO-PARM-INTERNAL-TAB-RECORD.              00109800
109900                                                                  00109900
110000     IF  IBGROPTI  =  'C'                                         00110000
110100     THEN                                                         00110100
110200         IF  IBGRSLTI  >  '8999999'                               00110200
110300         THEN                                                     00110300
110400             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00110400
110500             GO TO 1100-100-GET-INTERNAL-TAB                      00110500
110600         ELSE                                                     00110600
110700             ADD 100        TO  ACWA-FIELD-CHG-CNT                00110700
110800             MOVE '#IBGR '  TO  GCIO-TAB-TABULAR-ID               00110800
110900             MOVE IBGRSLTI  TO  GCIO-TAB-SLOT-NO                  00110900
111000     ELSE                                                         00111000
111100         NEXT SENTENCE.                                           00111100
111200                                                                  00111200
111300     IF  IDGDOPTI  =  'C'                                         00111300
111400     THEN                                                         00111400
111500         IF  IDGDSLTI  >  '8999999'                               00111500
111600         THEN                                                     00111600
111700             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00111700
111800             GO TO 1100-100-GET-INTERNAL-TAB                      00111800
111900         ELSE                                                     00111900
112000             ADD 100        TO  ACWA-FIELD-CHG-CNT                00112000
112100             MOVE '#IDGD '  TO  GCIO-TAB-TABULAR-ID               00112100
112200             MOVE IDGDSLTI  TO  GCIO-TAB-SLOT-NO                  00112200
112300     ELSE                                                         00112300
112400         NEXT SENTENCE.                                           00112400
112500                                                                  00112500
112600     IF  IPGNOPTI  =  'C'                                         00112600
112700     THEN                                                         00112700
112800         IF  IPGNSLTI  > '8999999'                                00112800
112900         THEN                                                     00112900
113000             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00113000
113100             GO TO 1100-100-GET-INTERNAL-TAB                      00113100
113200         ELSE                                                     00113200
113300           ADD 100        TO  ACWA-FIELD-CHG-CNT                  00113300
113400           MOVE '#IPGN '  TO  GCIO-TAB-TABULAR-ID                 00113400
113500           MOVE IPGNSLTI  TO  GCIO-TAB-SLOT-NO                    00113500
113600     ELSE                                                         00113600
113700         NEXT SENTENCE.                                           00113700
113800                                                                  00113800
113900     IF  IPGPOPTI  =  'C'                                         00113900
114000     THEN                                                         00114000
114100         IF  IPGPSLTI  > '8999999'                                00114100
114200         THEN                                                     00114200
114300             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00114300
114400             GO TO 1100-100-GET-INTERNAL-TAB                      00114400
114500         ELSE                                                     00114500
114600           ADD 100        TO  ACWA-FIELD-CHG-CNT                  00114600
114700           MOVE '#IPGP '  TO  GCIO-TAB-TABULAR-ID                 00114700
114800           MOVE IPGPSLTI  TO  GCIO-TAB-SLOT-NO                    00114800
114900     ELSE                                                         00114900
115000         NEXT SENTENCE.                                           00115000
115100                                                                  00115100
115200     IF  IPGTOPTI  =  'C'                                         00115200
115300     THEN                                                         00115300
115400         IF  IPGTSLTI  >  '8999999'                               00115400
115500         THEN                                                     00115500
115600             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00115600
115700             GO TO 1100-100-GET-INTERNAL-TAB                      00115700
115800         ELSE                                                     00115800
115900             ADD 100        TO  ACWA-FIELD-CHG-CNT                00115900
116000             MOVE '#IPGT '  TO  GCIO-TAB-TABULAR-ID               00116000
116100             MOVE IPGTSLTI  TO  GCIO-TAB-SLOT-NO                  00116100
116200     ELSE                                                         00116200
116300         NEXT SENTENCE.                                           00116300
116400                                                                  00116400
116500                                                                  00116500
116600     IF  IPGSOPTI  =  'C'                                         00116600
116700     THEN                                                         00116700
116800         IF  IPGSSLTI  >  '8999999'                               00116800
116900         THEN                                                     00116900
117000             ADD 900  TO  ACWA-FIELD-CHG-CNT                      00117000
117100             GO TO 1100-100-GET-INTERNAL-TAB                      00117100
117200         ELSE                                                     00117200
117300             ADD 100        TO  ACWA-FIELD-CHG-CNT                00117300
117400             MOVE '#IPGS '  TO  GCIO-TAB-TABULAR-ID               00117400
117500             MOVE IPGSSLTI  TO  GCIO-TAB-SLOT-NO                  00117500
117600     ELSE                                                         00117600
117700         NEXT SENTENCE.                                           00117700
117800                                                                  00117800
117900                                                                  00117900
118000     IF IBGROPTI  =  'MT'                                         00118000
118100        MOVE '#IBGR '  TO  GCIO-TAB-TABULAR-ID                    00118100
118200        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00118200
118300                                                                  00118300
118400     IF IBGROPTI  =  'A'                                          00118400
118500        MOVE '#IBGR '  TO  GCIO-TAB-TABULAR-ID                    00118500
118600        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00118600
118700                                                                  00118700
118800     IF IDGDOPTI  =  'MT'                                         00118800
118900        MOVE '#IDGD '  TO  GCIO-TAB-TABULAR-ID                    00118900
119000        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00119000
119100                                                                  00119100
119200     IF IDGDOPTI  =  'A'                                          00119200
119300        MOVE '#IDGD '  TO  GCIO-TAB-TABULAR-ID                    00119300
119400        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00119400
119500                                                                  00119500
119600     IF IPGNOPTI  =  'MT'                                         00119600
119700        MOVE '#IPGN '  TO  GCIO-TAB-TABULAR-ID                    00119700
119800        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00119800
119900                                                                  00119900
120000     IF IPGNOPTI  =  'A'                                          00120000
120100        MOVE '#IPGN '  TO  GCIO-TAB-TABULAR-ID                    00120100
120200        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00120200
120300                                                                  00120300
120400     IF IPGPOPTI  =  'MT'                                         00120400
120500        MOVE '#IPGP '  TO  GCIO-TAB-TABULAR-ID                    00120500
120600        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00120600
120700                                                                  00120700
120800     IF IPGPOPTI  =  'A'                                          00120800
120900        MOVE '#IPGP '  TO  GCIO-TAB-TABULAR-ID                    00120900
121000        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00121000
121100                                                                  00121100
121200     IF IPGTOPTI  =  'MT'                                         00121200
121300        MOVE '#IPGT '  TO  GCIO-TAB-TABULAR-ID                    00121300
121400        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00121400
121500                                                                  00121500
121600     IF IPGTOPTI  =  'A'                                          00121600
121700        MOVE '#IPGT '  TO  GCIO-TAB-TABULAR-ID                    00121700
121800        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00121800
121900                                                                  00121900
122000     IF IPGSOPTI  =  'MT'                                         00122000
122100        MOVE '#IPGS '  TO  GCIO-TAB-TABULAR-ID                    00122100
122200        MOVE MFRMSLTI  TO  GCIO-TAB-SLOT-NO.                      00122200
122300                                                                  00122300
122400     IF IPGSOPTI  =  'A'                                          00122400
122500        MOVE '#IPGS '  TO  GCIO-TAB-TABULAR-ID                    00122500
122600        MOVE 1         TO  GCIO-TAB-SLOT-NO.                      00122600
122700                                                                  00122700
122800     MOVE GCIO-TABULAR-FILE      TO GCIO2-FILE-KEY.               00122800
122900     MOVE GC-GCTABULR-DDNAME     TO GCIO2-FILE-DDNAME.            00122900
123000     MOVE GC-GCIO-AREA-2         TO GCIO2-IO-AREA-TO-USE.         00123000
123100     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO2-FILE-ACCESS-CODE.       00123100
123200     MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          00123200
123300          TO  GXA-ENTRY-COUNT.                                    00123300
123400                                                                  00123400
123500     EXEC CICS  LINK  PROGRAM('GCIOPGM')                          00123500
123600                COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          00123600
123700                LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)   END-EXEC.00123700
123800                                                                  00123800
123900     IF  NOT GCIO2-GOOD-RETURN AND  ACWA-PROD-INTERNAL-CHG        00123900
124000     THEN                                                         00124000
124100         SET  WT-01-INDEX                     TO +17              00124100
124200         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00124200
124300         MOVE 'Y'                             TO ACWA-ERROR-SW    00124300
124400         IF  IBGROPTI  =  'C'                                     00124400
124500         THEN                                                     00124500
124600             MOVE -1        TO  IBGROPTL                          00124600
124700             MOVE DFHBMUBF  TO  IBGROPTA                          00124700
124800             MOVE DFHBMASB  TO  IBGRIDA  IBGRSLTA                 00124800
124900             GO TO 1100-900-EXIT                                  00124900
125000         ELSE                                                     00125000
125100         IF  IPGNOPTI  =  'C'                                     00125100
125200         THEN                                                     00125200
125300             MOVE -1        TO  IPGNOPTL                          00125300
125400             MOVE DFHBMUBF  TO  IPGNOPTA                          00125400
125500             MOVE DFHBMASB  TO  IPGNIDA  IPGNSLTA                 00125500
125600             GO TO 1100-900-EXIT                                  00125600
125700         ELSE                                                     00125700
125800         IF  IPGTOPTI  =  'C'                                     00125800
125900         THEN                                                     00125900
126000              MOVE -1        TO  IPGTOPTL                         00126000
126100              MOVE DFHBMUBF  TO  IPGTOPTA                         00126100
126200              MOVE DFHBMASB  TO  IPGTIDA  IPGTSLTA                00126200
126300              GO TO 1100-900-EXIT                                 00126300
126400         ELSE                                                     00126400
126500         IF  IPGSOPTI  =  'C'                                     00126500
126600         THEN                                                     00126600
126700              MOVE -1        TO  IPGSOPTL                         00126700
126800              MOVE DFHBMUBF  TO  IPGSOPTA                         00126800
126900              MOVE DFHBMASB  TO  IPGSIDA  IPGSSLTA                00126900
127000              GO TO 1100-900-EXIT                                 00127000
127100         ELSE                                                     00127100
127200         IF  IDGDOPTI  =  'C'                                     00127200
127300         THEN                                                     00127300
127400             MOVE -1        TO  IDGDOPTL                          00127400
127500             MOVE DFHBMUBF  TO  IDGDOPTA                          00127500
127600             MOVE DFHBMASB  TO  IDGDIDA  IDGDSLTA                 00127600
127700             GO TO 1100-900-EXIT                                  00127700
127800         ELSE                                                     00127800
127900         IF  IPGPOPTI  =  'C'                                     00127900
128000         THEN                                                     00128000
128100             MOVE -1        TO  IPGPOPTL                          00128100
128200             MOVE DFHBMUBF  TO  IPGPOPTA                          00128200
128300             MOVE DFHBMASB  TO  IPGPIDA  IPGPSLTA                 00128300
128400             GO TO 1100-900-EXIT                                  00128400
128500         ELSE                                                     00128500
128600             NEXT SENTENCE                                        00128600
128700     ELSE                                                         00128700
128800         NEXT SENTENCE.                                           00128800
128900                                                                  00128900
129000     IF  NOT GCIO2-GOOD-RETURN AND  GCIO-TAB-SLOT-NO  NOT =  1    00129000
129100     THEN                                                         00129100
129200         SET  WT-01-INDEX                     TO +18              00129200
129300         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00129300
129400         MOVE 'Y'                             TO ACWA-ERROR-SW    00129400
129500         MOVE -1                              TO MFRMSLTL         00129500
129600         MOVE DFHBMUBF                        TO MFRMSLTA         00129600
129700         GO TO 1100-900-EXIT.                                     00129700
129800                                                                  00129800
129900     IF  NOT GCIO2-GOOD-RETURN                                    00129900
130000     THEN                                                         00130000
130100         MOVE WS-ABCODE-1PF1        TO WS-ABCODE                  00130100
130200         MOVE WS-ABCODE-1PF1-MSG    TO WS-ABCODE-MSG              00130200
130300         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                   00130300
130400                                                                  00130400
130500     COMPUTE  GCIO2-RECORD-LENGTH  =                              00130500
130600              GC-WORKFILE-KEY-LEN  +                              00130600
130700              GCIO2-RECORD-LENGTH.                                00130700
130800                                                                  00130800
130900     IF IBGROPTI  =  'C' OR  'MT' OR 'A'                          00130900
131000        IF  GXA-ENTRY-COUNT  NOT >  1                             00131000
131100            MOVE 'GA2GPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00131100
131200                                INTR-TAB-PGM-ID                   00131200
131300        ELSE                                                      00131300
131400            MOVE 'GA1GPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00131400
131500                                INTR-TAB-PGM-ID.                  00131500
131600                                                                  00131600
131700     IF IPGNOPTI  =  'C' OR  'MT' OR 'A'                          00131700
131800        IF  GXA-ENTRY-COUNT  NOT >  1                             00131800
131900            MOVE 'GA2HPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00131900
132000                                INTR-TAB-PGM-ID                   00132000
132100        ELSE                                                      00132100
132200            MOVE 'GA1HPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00132200
132300                                INTR-TAB-PGM-ID.                  00132300
132400                                                                  00132400
132500     IF IPGTOPTI  =  'C' OR  'MT' OR 'A'                          00132500
132600        IF  GXA-ENTRY-COUNT  NOT >  1                             00132600
132700            MOVE 'GA2IPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00132700
132800                                INTR-TAB-PGM-ID                   00132800
132900        ELSE                                                      00132900
133000            MOVE 'GA1IPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00133000
133100                                INTR-TAB-PGM-ID.                  00133100
133200                                                                  00133200
133300     IF IPGSOPTI  =  'C' OR  'MT' OR 'A'                          00133300
133400        IF  GXA-ENTRY-COUNT  NOT >  1                             00133400
133500            MOVE 'GA2SPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00133500
133600                                INTR-TAB-PGM-ID                   00133600
133700        ELSE                                                      00133700
133800            MOVE 'GA1SPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00133800
133900                                INTR-TAB-PGM-ID.                  00133900
134000                                                                  00134000
134100     IF IDGDOPTI  =  'C' OR  'MT' OR 'A'                          00134100
134200        IF  GXA-ENTRY-COUNT  NOT >  1                             00134200
134300            MOVE 'GA2NPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00134300
134400                                INTR-TAB-PGM-ID                   00134400
134500        ELSE                                                      00134500
134600            MOVE 'GA1NPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00134600
134700                                INTR-TAB-PGM-ID.                  00134700
134800                                                                  00134800
134900     IF IPGPOPTI  =  'C' OR  'MT' OR 'A'                          00134900
135000        IF  GXA-ENTRY-COUNT  NOT >  1                             00135000
135100            MOVE 'GA2OPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00135100
135200                                INTR-TAB-PGM-ID                   00135200
135300        ELSE                                                      00135300
135400            MOVE 'GA1OPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID        00135400
135500                                INTR-TAB-PGM-ID.                  00135500
135600                                                                  00135600
135700     GO TO 1100-900-EXIT.                                         00135700
135800                                                                  00135800
135900/                                                                 00135900
136000 1100-100-GET-INTERNAL-TAB.                                       00136000
136100                                                                  00136100
136200     IF FRMNUIDI  =  'GS3A'                                       00136200
136300        PERFORM 6000-000-BUILD-GROUP-SPEC-KEY.                    00136300
136400        MOVE 'G4'  TO  GCIO-WRK-RECORD-TYPE.                      00136400
136500                                                                  00136500
136600     IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            00136600
136700        PERFORM 6100-000-BUILD-CONTRACT-KEY                       00136700
136800        MOVE 'C3'  TO  GCIO-WRK-RECORD-TYPE.                      00136800
136900                                                                  00136900
137000     IF FRMNUIDI  =  'GC8A'                                       00137000
137100        PERFORM 6200-000-BUILD-BEN-PROV-KEY                       00137100
137200        MOVE 'C6'               TO GCIO-WRK-RECORD-TYPE           00137200
137300        MOVE TABIDI             TO GCIO-WRK-PROVISION-ID          00137300
137400        MOVE TABSLTNI           TO ACWA-DISPLAY-LEN-7             00137400
137500        MOVE ACWA-DISPLAY-LEN-7 TO GCIO-WRK-PROVISION-SLOT-NO.    00137500
137600                                                                  00137600
137700     MOVE GC-GCPSWORK-DDNAME TO GCIO2-FILE-DDNAME.                00137700
137800     MOVE GC-GCIO-AREA-1     TO GCIO2-IO-AREA-TO-USE.             00137800
137900                                                                  00137900
138000     IF IBGROPTI  =  'C'                                          00138000
138100        MOVE '#IBGR '  TO  GCIO-WRK-TAB-PROVISION-ID              00138100
138200        MOVE IBGRSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00138200
138300                                                                  00138300
138400     IF IDGDOPTI  =  'C'                                          00138400
138500        MOVE '#IDGD '  TO  GCIO-WRK-TAB-PROVISION-ID              00138500
138600        MOVE IDGDSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00138600
138700                                                                  00138700
138800     IF IPGNOPTI  =  'C'                                          00138800
138900        MOVE '#IPGN '  TO  GCIO-WRK-TAB-PROVISION-ID              00138900
139000        MOVE IPGNSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00139000
139100                                                                  00139100
139200     IF IPGPOPTI  =  'C'                                          00139200
139300        MOVE '#IPGP '  TO  GCIO-WRK-TAB-PROVISION-ID              00139300
139400        MOVE IPGPSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00139400
139500                                                                  00139500
139600     IF IPGTOPTI  =  'C'                                          00139600
139700        MOVE '#IPGT '  TO  GCIO-WRK-TAB-PROVISION-ID              00139700
139800        MOVE IPGTSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00139800
139900                                                                  00139900
140000     IF IPGSOPTI  =  'C'                                          00140000
140100        MOVE '#IPGS '  TO  GCIO-WRK-TAB-PROVISION-ID              00140100
140200        MOVE IPGSSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.             00140200
140300                                                                  00140300
140400     MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              00140400
140500     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO2-FILE-ACCESS-CODE.       00140500
140600     MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          00140600
140700          TO  GXA-ENTRY-COUNT.                                    00140700
140800                                                                  00140800
140900     EXEC CICS  LINK  PROGRAM('GCIOPGM')                          00140900
141000                COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          00141000
141100                LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. 00141100
141200                                                                  00141200
141300     IF NOT GCIO2-GOOD-RETURN                                     00141300
141400        MOVE WS-ABCODE-1PF2        TO WS-ABCODE                   00141400
141500        MOVE WS-ABCODE-1PF2-MSG    TO WS-ABCODE-MSG               00141500
141600        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00141600
141700                                                                  00141700
141800     IF IBGROPTI  =  'C'                                          00141800
141900        IF GXA-ENTRY-COUNT  NOT >  1                              00141900
142000           MOVE 'GA2GPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00142000
142100                                INTR-TAB-PGM-ID                   00142100
142200        ELSE                                                      00142200
142300           MOVE 'GA1GPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00142300
142400                                INTR-TAB-PGM-ID.                  00142400
142500                                                                  00142500
142600     IF IPGNOPTI  =  'C'                                          00142600
142700        IF GXA-ENTRY-COUNT  NOT >  1                              00142700
142800           MOVE 'GA2HPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00142800
142900                                INTR-TAB-PGM-ID                   00142900
143000        ELSE                                                      00143000
143100           MOVE 'GA1HPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00143100
143200                                INTR-TAB-PGM-ID.                  00143200
143300                                                                  00143300
143400     IF IPGTOPTI  =  'C'                                          00143400
143500        IF GXA-ENTRY-COUNT  NOT >  1                              00143500
143600           MOVE 'GA2IPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00143600
143700                                INTR-TAB-PGM-ID                   00143700
143800        ELSE                                                      00143800
143900           MOVE 'GA1IPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00143900
144000                                INTR-TAB-PGM-ID.                  00144000
144100                                                                  00144100
144200     IF IPGSOPTI  =  'C'                                          00144200
144300        IF GXA-ENTRY-COUNT  NOT >  1                              00144300
144400           MOVE 'GA2SPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00144400
144500                                INTR-TAB-PGM-ID                   00144500
144600        ELSE                                                      00144600
144700           MOVE 'GA1SPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00144700
144800                                INTR-TAB-PGM-ID.                  00144800
144900                                                                  00144900
145000     IF IDGDOPTI  =  'C'                                          00145000
145100        IF GXA-ENTRY-COUNT  NOT >  1                              00145100
145200           MOVE 'GA2NPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00145200
145300                                INTR-TAB-PGM-ID                   00145300
145400        ELSE                                                      00145400
145500           MOVE 'GA1NPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00145500
145600                                INTR-TAB-PGM-ID.                  00145600
145700                                                                  00145700
145800     IF IPGPOPTI  =  'C'                                          00145800
145900        IF GXA-ENTRY-COUNT  NOT >  1                              00145900
146000           MOVE 'GA2OPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00146000
146100                                INTR-TAB-PGM-ID                   00146100
146200        ELSE                                                      00146200
146300           MOVE 'GA1OPGM'  TO  WS-INTERNAL-TABULAR-PGM-ID         00146300
146400                                INTR-TAB-PGM-ID.                  00146400
146500                                                                  00146500
146600     GO TO 1100-900-EXIT.                                         00146600
146700                                                                  00146700
146800 1100-900-EXIT. EXIT.                                             00146800
146900/*****************************************************************00146900
147000* 2000  PROCESS REQUEST                                          *00147000
147100*                                                                *00147100
147200*   THIS ROUTINE CHECKS THE CHARACTERISTICS OF THE INCOMING TRANS-00147200
147300* ACTION AND ROUTES THEM TO THE APPROPRIATE ROUTINE TO PROCESS   *00147300
147400* THE REQUEST.                                                   *00147400
147500******************************************************************00147500
147600 2000-000-PROCESS-REQUEST       SECTION.                          00147600
147700 2000-010.                                                        00147700
147800                                                                  00147800
147900     IF (EIBAID       =  DFHENTER OR DFHPF4 OR DFHPF16) AND       00147900
148000        DELADDI       =  'CHG/ADD'                      AND       00148000
148100        OENTCTRI      =  '0000000'                                00148100
148200        PERFORM 2100-000-INSERT-SKELETON.                         00148200
148300                                                                  00148300
148400** LINK TO THE CHANGE/DELETE MODULE TO PROCESS FIVE DIFFERENT     00148400
148500** REQUESTS DEPENDING ON THE USER RESPONSE                        00148500
148600**                                                                00148600
148700     MOVE 'CHG/DEL' TO DELADD-OPTION.                             00148700
148800     MOVE SPACES TO LVL2-B-SW  LVL2-F-SW  LVL2-G-SW.              00148800
148900                                                                  00148900
149000     MOVE WS-ALT-WORKFILE-KEYS       TO ACWA-ALT-WORKFILE-KEYS.   00149000
149100                                                                  00149100
149200     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00149200
149300                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00149300
149400                                                                  00149400
149500     EXEC CICS  LINK  PROGRAM ('GAS5UPD')                         00149500
149600                COMMAREA(COMMON-WORKAREAS)                        00149600
149700                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00149700
149800                                                                  00149800
149900     SET ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD  TO             00149900
150000                 ACWA-WF-ALL-LEVEL-TAB-PNTR.                      00150000
150100                                                                  00150100
150200     SET ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD TO             00150200
150300                 ACWA-WF-INTERNAL-TAB-PNTR.                       00150300
150400                                                                  00150400
150500     IF LVL2-B-SW =  'Y'                                          00150500
150600        PERFORM  7900-000-RESET-ATTRIBUTES                        00150600
150700        PERFORM  8100-000-DISPLAY-ADD-SCREEN.                     00150700
150800                                                                  00150800
150900     IF LVL2-F-SW =  'Y'                                          00150900
151000        PERFORM  3100-000-READ-RECORD                             00151000
151100        PERFORM  4300-000-DISPLAY-PREV.                           00151100
151200                                                                  00151200
151300     IF LVL2-G-SW =  'Y'                                          00151300
151400        PERFORM  3100-000-READ-RECORD                             00151400
151500        PERFORM  4200-000-DISPLAY-NEXT.                           00151500
151600                                                                  00151600
151700                                                                  00151700
151800 2000-900-EXIT. EXIT.                                             00151800
151900                                                                  00151900
152000/*****************************************************************00152000
152100* 2100  INSERT SKELETON                                          *00152100
152200*                                                                *00152200
152300*    THIS ROUTINE WILL ADD A NEW ENTRY INTO THE TABLE.  IF THE   *00152300
152400*  TABLE ALREADY CONTAINS THE MAXIMUM NUMBER OF 29 ENTRIES THE   *00152400
152500*  SORT ROUTINE MAY REDUCE THAT NUMBER AS IT WILL DELETE ALL     *00152500
152600*  DUPLICATES.  IF THE NEW ENTRY CAN BE ADDED AND THE OPERATOR   *00152600
152700*  REQUESTED THE ADDITION OF AN INTERNAL TABULAR THIS ROUTINE    *00152700
152800*  WILL WRITE THE NEW INTERNAL TABULAR TO THE WORKFILE AND THEN  *00152800
152900*  PASS IT TO THE ADD VERSION OF THE INTERNAL TABULAR PROGRAM.   *00152900
153000******************************************************************00153000
153100 2100-000-INSERT-SKELETON       SECTION.                          00153100
153200 2100-010.                                                        00153200
153300                                                                  00153300
153400     IF  EIBAID = DFHENTER         AND                            00153400
153500         ACWA-SCREEN-HAS-NO-ERRORS AND                            00153500
153600         GCVI-TABLE-SW = 'N'                                      00153600
153700     THEN                                                         00153700
153800         SET  WT-01-INDEX                     TO +07              00153800
153900         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          00153900
154000         PERFORM 9010-000-SEND-DATAONLY-RETURN.                   00154000
154100                                                                  00154100
154200     IF  EIBAID  = DFHPF4 OR DFHPF16                              00154200
154300         PERFORM 7900-000-RESET-ATTRIBUTES.                       00154300
154400                                                                  00154400
154500     PERFORM 3200-000-READ-REC-FOR-UPDATE.                        00154500
154600                                                                  00154600
154700     IF NOT GCIO-GOOD-RETURN                                      00154700
154800        MOVE WS-ABCODE-1PF5        TO WS-ABCODE                   00154800
154900        MOVE WS-ABCODE-1PF5-MSG    TO WS-ABCODE-MSG               00154900
155000        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00155000
155100                                                                  00155100
155200     IF GAF-OCC-ENTRY-TAB-SLOT-CNTR > 9999900                     00155200
155300        SET  WT-01-INDEX                     TO +15               00155300
155400        MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO           00155400
155500        MOVE -1                              TO PERIODL           00155500
155600        PERFORM 9010-000-SEND-DATAONLY-RETURN.                    00155600
155700                                                                  00155700
155800     MOVE GAF-ENTRY-COUNT  TO  GAF-ENTRY-COUNT.                   00155800
155900                                                                  00155900
156000     IF  GAF-ENTRY-COUNT  NOT <  GC-GCTABULR-ACP-VARY-MAX-OCUR    00156000
156100     THEN                                                         00156100
156200         PERFORM 6500-000-SORT-COMPRESS-ALL-LVL                   00156200
156300         IF  ACWA-SCREEN-HAS-NO-ERRORS                            00156300
156400         THEN                                                     00156400
156500             PERFORM 2500-000-ADD-NEW-OCCURS                      00156500
156600             IF  WRK-CDE-SP = '2 '      AND                       00156600
156700                 CDEINDO    = ('+CDE+'  OR '+CDE-')               00156700
156800                 ADD 1      TO   ACWA-CDE-1U-COUNT                00156800
156900                 SUBTRACT 1 FROM ACWA-CDE-2B-COUNT                00156900
157000                 MOVE '1U'  TO   WRK-CDE-SP                       00157000
157100                 PERFORM 3000-000-UPDATE-GAF-RECORD               00157100
157200             ELSE                                                 00157200
157300                 PERFORM 3000-000-UPDATE-GAF-RECORD               00157300
157400         ELSE                                                     00157400
157500             SET  WT-01-INDEX                     TO +16          00157500
157600             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      00157600
157700             MOVE -1                              TO  PERIODL     00157700
157800             PERFORM 9010-000-SEND-DATAONLY-RETURN                00157800
157900      ELSE                                                        00157900
158000          PERFORM 2500-000-ADD-NEW-OCCURS                         00158000
158100             IF  WRK-CDE-SP = '2 '      AND                       00158100
158200                 CDEINDO    = ('+CDE+'  OR '+CDE-')               00158200
158300                 ADD 1      TO   ACWA-CDE-1U-COUNT                00158300
158400                 SUBTRACT 1 FROM ACWA-CDE-2B-COUNT                00158400
158500                 MOVE '1U'  TO   WRK-CDE-SP                       00158500
158600                 PERFORM 3000-000-UPDATE-GAF-RECORD               00158600
158700             ELSE                                                 00158700
158800                 PERFORM 3000-000-UPDATE-GAF-RECORD.              00158800
158900                                                                  00158900
159000     SET GAF-INDEX  DOWN BY  1.                                   00159000
159100                                                                  00159100
159200     IF  (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE) AND           00159200
159300         (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE) AND           00159300
159400         (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE) AND           00159400
159500         (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE) AND           00159500
159600         (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE) AND           00159600
159700         (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE)               00159700
159800     THEN                                                         00159800
159900         PERFORM 4700-000-UPDATE-CONTROL-RECORD                   00159900
160000         MOVE GAF-OCCURS-ENTRY-COUNTER (GAF-INDEX)                00160000
160100                                 TO  ACWA-DISPLAY-LEN-7           00160100
160200         MOVE ACWA-DISPLAY-LEN-7 TO OENTCTRO                      00160200
160300         MOVE -1                 TO  PERIODL                      00160300
160400         PERFORM 9010-000-SEND-DATAONLY-RETURN.                   00160400
160500                                                                  00160500
160600     IF FRMNUIDI  =  'GS3A'                                       00160600
160700        MOVE 'G4'    TO  GCIO-WRK-RECORD-TYPE                     00160700
160800        MOVE SPACES  TO  GCA-L-O-B                                00160800
160900                         GCA-PROV-CTL                             00160900
161000                         GCA-BEN-PROV-ID.                         00161000
161100                                                                  00161100
161200     IF FRMNUIDI = 'GC4A' OR 'GTM1'                               00161200
161300        MOVE 'C3'                       TO  GCIO-WRK-RECORD-TYPE  00161300
161400        MOVE GCIO-WRK-LINE-OF-BUS       TO  GCA-L-O-B             00161400
161500        MOVE GCIO-WRK-PROVIDER-CONTROL  TO  GCA-PROV-CTL          00161500
161600        MOVE SPACES  TO  GCA-BEN-PROV-ID.                         00161600
161700                                                                  00161700
161800     IF FRMNUIDI  =  'GC8A'                                       00161800
161900        MOVE 'C6'                       TO  GCIO-WRK-RECORD-TYPE  00161900
162000        MOVE GCIO-WRK-LINE-OF-BUS       TO  GCA-L-O-B             00162000
162100        MOVE GCIO-WRK-PROVIDER-CONTROL  TO  GCA-PROV-CTL          00162100
162200        MOVE GCIO-WRK-PROVISION-ID      TO  GCA-BEN-PROV-ID       00162200
162300        MOVE GCIO-WRK-TABULAR-PROVISION TO                        00162300
162400                                     GCIO-WRK-BENEFIT-PROVISION.  00162400
162500                                                                  00162500
162600     MOVE GCIO-WRK-EFFDT-CEN       TO GCA-EFFDT-CEN.              00162600
162700     MOVE GCIO-WRK-EFFECTIVE-DATE  TO HGADATE-JULIAN1.            00162700
162800     PERFORM 9300-000-JULIAN-TO-GREGORIAN.                        00162800
162900     MOVE HGADATE-DATE2            TO GCA-EFFECTIVE-DATE.         00162900
163000     MOVE GC-GCPSWORK-DDNAME       TO GCIO2-FILE-DDNAME.          00163000
163100     MOVE GC-GCIO-AREA-1           TO GCIO2-IO-AREA-TO-USE.       00163100
163200     MOVE TABIDI                   TO GCA-ALL-LEVEL-TAB-ID.       00163200
163300     MOVE TABSLTNI                 TO ACWA-DISPLAY-LEN-7.         00163300
163400     MOVE ACWA-DISPLAY-LEN-7       TO GCA-ALL-LEVEL-TAB-SLOT.     00163400
163500     MOVE GXA-PROVISION-ID         TO GCIO-WRK-TAB-PROVISION-ID,  00163500
163600                                      GCA-INTERNAL-TAB-ID.        00163600
163700     MOVE GXA-INCLUDE-EXCLUDE-IND  TO GCA-I-E-INDC.               00163700
163800     MOVE GXA-PROVISION-SLOT-NO    TO SAVE-COPY-FROM-SLOT.        00163800
163900     MOVE GAF-OCCURS-ENTRY-COUNTER (GAF-INDEX)                    00163900
164000                                   TO GCIO-WRK-TAB-PROV-SLOT-NO   00164000
164100                                      GXA-PROVISION-SLOT-NO       00164100
164200                                      ACWA-DISPLAY-LEN-7.         00164200
164300     MOVE ACWA-DISPLAY-LEN-7       TO GCA-INTERNAL-TAB-SLOT       00164300
164400                                      GCA-OCCURS-ENTRY-COUNTER.   00164400
164500     MOVE 'A'                      TO GCA-ADD-DEL-IND.            00164500
164600     MOVE 'CHG/ADD'                TO DELADD-OPTION.              00164600
164700     MOVE FRMNUIDI                 TO GCA-FROM-MENU-ID.           00164700
164800     MOVE FUNCTONI                 TO GCA-ALL-LEVEL-TAB-FUNC-CODE.00164800
164900     MOVE GCIO-WRK-PLAN-CODE       TO GCA-PLAN-CODE.              00164900
165000     MOVE GCIO-WRK-GROUP-NUM       TO GCA-GROUP-NUM.              00165000
165100     MOVE GCIO-WRK-SECTION-NUM     TO GCA-SECTION-NUM.            00165100
165200     MOVE GCIO-WRK-PKG-CODE        TO GCA-PKG-CODE.               00165200
165300     MOVE GCIO-WRK-FAMILY-RELATION-LVL                            00165300
165400                                   TO GCA-FAM-REL-LVL.            00165400
165500     MOVE SPACES                   TO WORK-RECORD-2.              00165500
165600     MOVE GCIO-WORKFILE-KEY        TO GCIO2-FILE-KEY              00165600
165700                                      WORK-RECORD-KEY-2.          00165700
165800     MOVE SAVE-COPY-FROM-SLOT      TO WRK2-PROV-POOL-COPY-SLOT.   00165800
165900                                                                  00165900
166000     IF FRMNUIDI  =  'GC8A'                                       00166000
166100        MOVE GCIO-WRK-PROVISION-ID TO WRK2-ALL-LEV-BEN-PROV.      00166100
166200                                                                  00166200
166300     IF WRK-SIGNAL-FROM-ONLINE  =  'W'                            00166300
166400        MOVE 'W'       TO  WRK2-SIGNAL-FROM-ONLINE                00166400
166500        MOVE '1U'      TO  WRK2-CDE-SP                            00166500
166600        ADD   1        TO  ACWA-CDE-1U-COUNT                      00166600
166700     ELSE                                                         00166700
166800        IF  CDEINDO  = ('+CDE+'  OR '+CDE-')                      00166800
166900            MOVE '1U'   TO  WRK2-CDE-SP                           00166900
167000            ADD   1     TO  ACWA-CDE-1U-COUNT                     00167000
167100        ELSE                                                      00167100
167200            MOVE '2 '   TO  WRK2-CDE-SP                           00167200
167300            ADD   1     TO  ACWA-CDE-2B-COUNT.                    00167300
167400                                                                  00167400
167500** SET INDICATOR TO CAPTURE OPERATOR-ID.                          00167500
167600     MOVE '1'          TO  GCIO2-OPER-ID-IND.                     00167600
167700                                                                  00167700
167800     PERFORM 4700-000-UPDATE-CONTROL-RECORD.                      00167800
167900     MOVE GC-GCIO-ACCESS-CODE-WR   TO  GCIO2-FILE-ACCESS-CODE.    00167900
168000                                                                  00168000
168100     COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   00168100
168200              GC-GCIOPARM-LEN  +  GCIO2-RECORD-LENGTH.            00168200
168300                                                                  00168300
168400     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00168400
168500                COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          00168500
168600                LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. 00168600
168700                                                                  00168700
168800     IF NOT GCIO2-GOOD-RETURN                                     00168800
168900        MOVE WS-ABCODE-1PF6        TO WS-ABCODE                   00168900
169000        MOVE WS-ABCODE-1PF6-MSG    TO WS-ABCODE-MSG               00169000
169100        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00169100
169200                                                                  00169200
169300     SET GCA-RECORD-POINTER  TO                                   00169300
169400                    ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD.    00169400
169500                                                                  00169500
169600     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00169600
169700                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00169700
169800     EXEC CICS  XCTL  PROGRAM(WS-INTERNAL-TABULAR-PGM-ID)         00169800
169900                COMMAREA(COMMON-WORKAREAS)                        00169900
170000                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00170000
170100                                                                  00170100
170200 2100-900-EXIT.                                                   00170200
170300          EXIT.                                                   00170300
170400/*****************************************************************00170400
170500* 2500  ADD NEW OCCURS                                           *00170500
170600*                                                                *00170600
170700*     INITIALIZE ENTRY IN THE TABLE TO EITHER SPACES OR ZEROS,   *00170700
170800*  THEN IF A FIELD WAS ENTERED BY THE OPERATOR MOVE IT TO THE    *00170800
170900*  TABLE IN THE RECORD.                                          *00170900
171000******************************************************************00171000
171100 2500-000-ADD-NEW-OCCURS        SECTION.                          00171100
171200 2500-010.                                                        00171200
171300                                                                  00171300
171400     MOVE GAF-ENTRY-COUNT  TO GAF-ENTRY-COUNT.                    00171400
171500     SET GAF-INDEX         TO GAF-ENTRY-COUNT.                    00171500
171600     MOVE LOW-VALUES       TO GAF-ENTRY (GAF-INDEX).              00171600
171700                                                                  00171700
171800     MOVE ZEROS TO GAF-COPAY-INTL-TAB-SLOT-2 (GAF-INDEX),         00171800
171900                   GAF-COPAY-INTL-TAB-SLOT-3 (GAF-INDEX),         00171900
172000                   GAF-COPAY-INTL-TAB-SLOT-4 (GAF-INDEX),         00172000
172100                   GAF-COPAY-INTL-TAB-SLOT-5 (GAF-INDEX).         00172100
172200                                                                  00172200
172300     MOVE SPACES TO GAF-COPAY-INTL-TAB-TAB-ID-2 (GAF-INDEX)       00172300
172400                    GAF-COPAY-INTL-TAB-TAB-ID-3 (GAF-INDEX)       00172400
172500                    GAF-COPAY-INTL-TAB-TAB-ID-4 (GAF-INDEX)       00172500
172600                    GAF-COPAY-INTL-TAB-TAB-ID-5 (GAF-INDEX).      00172600
172700                                                                  00172700
172800     MOVE GAF-OCC-ENTRY-TAB-SLOT-CNTR                             00172800
172900                    TO GAF-OCCURS-ENTRY-COUNTER       (GAF-INDEX).00172900
173000     ADD 1          TO GAF-OCC-ENTRY-TAB-SLOT-CNTR.               00173000
173100     MOVE DAYFACII  TO GAF-COPAY-DAY-FACTOR-IND       (GAF-INDEX).00173100
173200     MOVE COPAYINI  TO GAF-COPAY-CO-PAY-IND           (GAF-INDEX).00173200
173300     MOVE BISNDINI  TO GAF-COPAY-BISCENDING-IND       (GAF-INDEX).00173300
173400     MOVE ASCDSCDI  TO GAF-COPAY-ASCEND-DESCEND-IND   (GAF-INDEX).00173400
      *P21595 CHANGES STARTS                                            00173410
173400     MOVE BENTYPI   TO GAF-COPAY-BEN-TYPE             (GAF-INDEX).00173411
173400     MOVE TIERCDI   TO GAF-COPAY-TIER-CODE            (GAF-INDEX).00173412
173400     MOVE TIERLVI   TO GAF-COPAY-TIER-LVL             (GAF-INDEX).00173413
      *P21595 CHANGES ENDS                                              00173420
173500     MOVE DEFINTNI  TO GAF-COPAY-DEFINITION           (GAF-INDEX).00173500
173600     MOVE MANAPLII  TO GAF-COPAY-MANDATORY-IND        (GAF-INDEX).00173600
173700     MOVE TIMEDOLI  TO GAF-COPAY-TIME-DOLLAR-IND      (GAF-INDEX).00173700
173800     MOVE FYIVALI   TO GAF-COPAY-FYI-VALUE            (GAF-INDEX).00173800
173900     MOVE CSTCONTI  TO GAF-COPAY-COST-CONTAIN-IND     (GAF-INDEX).00173900
174000     MOVE PERIODI   TO GAF-COPAY-BENEFIT-PERIOD       (GAF-INDEX).00174000
174100     MOVE PERTQALI  TO GAF-COPAY-BEN-PER-TIME-QUAL    (GAF-INDEX).00174100
174200     MOVE FAMINDII  TO GAF-COPAY-FAM-OR-INDIV         (GAF-INDEX).00174200
174300     MOVE PLCTRMTI  TO GAF-COPAY-PLACE-OF-TREATMENT   (GAF-INDEX).00174300
174400     MOVE SRVGRUPI  TO GAF-COPAY-SERVICE-GROUP        (GAF-INDEX).00174400
174500     MOVE AGELIMLI  TO ACWA-DISPLAY-LEN-3-X.                      00174500
174600     MOVE ACWA-DISPLAY-LEN-3                                      00174600
174700                    TO GAF-COPAY-AGE-LIMIT-FROM       (GAF-INDEX).00174700
174800     MOVE AGELIMHI  TO ACWA-DISPLAY-LEN-3-X.                      00174800
174900     MOVE ACWA-DISPLAY-LEN-3                                      00174900
175000                    TO GAF-COPAY-AGE-LIMIT-TO         (GAF-INDEX).00175000
175100     MOVE FEAKINDI  TO GAF-COPAY-FEAK-IND             (GAF-INDEX).00175100
175200     MOVE ACCUMIDI  TO GAF-COPAY-ACCUMID              (GAF-INDEX).00175200
175300     MOVE CAPINDI   TO GAF-COPAY-COMB-APPLIED-IND     (GAF-INDEX).00175300
175400     MOVE SABDINDI  TO GAF-COPAY-SEL-ADDL-BEN-DET     (GAF-INDEX).00175400
175500     MOVE AGEQLLI   TO GAF-COPAY-AGE-QUAL-IND-FROM    (GAF-INDEX).00175500
175600     MOVE AGEQLHI   TO GAF-COPAY-AGE-QUAL-IND-TO      (GAF-INDEX).00175600
175700     MOVE RELPINDI  TO GAF-COPAY-RELATIONSHIP-IND     (GAF-INDEX).00175700
175800                                                                  00175800
175900     MOVE PRTIMEFI  TO ACWA-DISPLAY-LEN-3-X.                      00175900
176000     MOVE ACWA-DISPLAY-LEN-3                                      00176000
176100                    TO GAF-COPAY-BEN-PER-TIME-FCTR    (GAF-INDEX).00176100
176200     MOVE CLMLVLII  TO GAF-COPAY-CLAIM-LVL-ACCUM-IND  (GAF-INDEX).00176200
176300     MOVE INTRVALI  TO ACWA-DISPLAY-LEN-3-X.                      00176300
176400     MOVE ACWA-DISPLAY-LEN-3                                      00176400
176500                    TO GAF-COPAY-INTERVAL-TIME-FCTR   (GAF-INDEX).00176500
176600     MOVE INTTYPEI  TO GAF-COPAY-INTERVAL-TYPE        (GAF-INDEX).00176600
176700     MOVE LOBI      TO GAF-COPAY-L-O-B                (GAF-INDEX).00176700
176800                                                                  00176800
176900     PERFORM 2600-000-PROCESS-VAL-LIMIT.                          00176900
177000                                                                  00177000
177100     MOVE ACWA-VALUE-LIMIT-9                                      00177100
177200                    TO GAF-COPAY-VALUE-LIMIT          (GAF-INDEX).00177200
177300     MOVE BENVLQLI  TO GAF-COPAY-VALUE-QUALIFIER      (GAF-INDEX).00177300
177400     MOVE NEWVALUI  TO ACWA-DISPLAY-LEN-5-X.                      00177400
177500     MOVE ACWA-DISPLAY-LEN-5                                      00177500
177600                    TO GAF-COPAY-INTERVAL-OVRD-VALUE  (GAF-INDEX).00177600
177700     MOVE OVRDINDI  TO GAF-COPAY-INTERVAL-OVRD-IND    (GAF-INDEX).00177700
177800     MOVE INTDESKI  TO GAF-COPAY-INTERNAL-DESCRIPTOR  (GAF-INDEX).00177800
177900     MOVE CONDALLI  TO GAF-COND-ALL-BIT               (GAF-INDEX).00177900
178000     MOVE CONDEXCI  TO GAF-COND-EXCLUSION-BIT         (GAF-INDEX).00178000
178100     MOVE CONDICDI  TO GAF-COND-ICD-BIT               (GAF-INDEX).00178100
178200     MOVE CONDTABI  TO GAF-COND-TB-BIT                (GAF-INDEX).00178200
178300     MOVE CONDMENI  TO GAF-COND-MENTAL-BIT            (GAF-INDEX).00178300
178400     MOVE CONDDRGI  TO GAF-COND-DRUG-BIT              (GAF-INDEX).00178400
178500     MOVE CONDALCI  TO GAF-COND-ALCOHOL-BIT           (GAF-INDEX).00178500
178600     MOVE CONDOBCI  TO GAF-COND-OB-COMP-BIT           (GAF-INDEX).00178600
178700     MOVE CONDOBNI  TO GAF-COND-OB-NORM-BIT           (GAF-INDEX).00178700
178800     MOVE CONDMALI  TO GAF-COND-MALIGNANCY-BIT        (GAF-INDEX).00178800
178900     MOVE CONDCARI  TO GAF-COND-CARDIAC-DISEASE-BIT   (GAF-INDEX).00178900
179000     MOVE CONDOBSI  TO GAF-COND-OBESITY-BIT           (GAF-INDEX).00179000
179100     MOVE CONDKDYI  TO GAF-COND-KIDNEY-DISEASE-BIT    (GAF-INDEX).00179100
179200     MOVE CONDACCI  TO GAF-COND-ACCIDENT-BIT          (GAF-INDEX).00179200
179300     MOVE CONDPECI  TO GAF-COND-PRE-EXIST-BIT         (GAF-INDEX).00179300
179400     MOVE CONDNEMI  TO GAF-COND-NON-EMER-BIT          (GAF-INDEX).00179400
179500     MOVE CONDSUII  TO GAF-COND-SUICIDE-BIT           (GAF-INDEX).00179500
179600     MOVE CONDTMJI  TO GAF-COND-TMJ-BIT               (GAF-INDEX).00179600
179700     MOVE CONDINFI  TO GAF-COND-INF-BIT               (GAF-INDEX).00179700
179800     MOVE CONDLIFI  TO GAF-COND-LIFE-THREAT-BIT       (GAF-INDEX).00179800
179900     MOVE CONDEMCI  TO GAF-COND-EMER-MED-BIT          (GAF-INDEX).00179900
180000     MOVE CONDEACI  TO GAF-COND-EMER-ACC-BIT          (GAF-INDEX).00180000
180100     MOVE CONDSMII  TO GAF-COND-SER-MEN-ILL-BIT       (GAF-INDEX).00180100
180200     MOVE CONDNSMI  TO GAF-COND-NON-SER-MEN-ILL-BIT   (GAF-INDEX).00180200
180300                                                                  00180300
180400     IF  (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE) AND           00180400
180500         (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE) AND           00180500
180600         (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE) AND           00180600
180700         (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE) AND           00180700
180800         (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE) AND           00180800
180900         (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE)               00180900
181000     THEN                                                         00181000
181100        MOVE 1               TO GAF-INTERNAL-TABULAR-COUNT        00181100
181200                                                       (GAF-INDEX)00181200
181300        MOVE HIGH-VALUES     TO GAF-COPAY-INTL-TAB-1   (GAF-INDEX)00181300
181400        SET  GAF-INDEX  UP BY  1                                  00181400
181500        MOVE HIGH-VALUES     TO GAF-ENTRY (GAF-INDEX)             00181500
181600        SET GAF-ENTRY-COUNT  TO GAF-INDEX                         00181600
181700        GO TO 2500-900-EXIT.                                      00181700
181800                                                                  00181800
181900     IF IBGROPTI  =  'MT' OR 'A'                                  00181900
182000        MOVE 2           TO GAF-INTERNAL-TABULAR-COUNT (GAF-INDEX)00182000
182100        MOVE HIGH-VALUES TO GAF-COPAY-INTL-TAB-2       (GAF-INDEX)00182100
182200        MOVE '#IBGR '    TO GAF-COPAY-INTL-TAB-TAB-ID-1(GAF-INDEX)00182200
182300        MOVE GAF-OCCURS-ENTRY-COUNTER                  (GAF-INDEX)00182300
182400                         TO GAF-COPAY-INTL-TAB-SLOT-1 (GAF-INDEX).00182400
182500                                                                  00182500
182600     IF IDGDOPTI  =  'MT' OR 'A'                                  00182600
182700        MOVE 2           TO GAF-INTERNAL-TABULAR-COUNT (GAF-INDEX)00182700
182800        MOVE HIGH-VALUES TO GAF-COPAY-INTL-TAB-2       (GAF-INDEX)00182800
182900        MOVE '#IDGD '    TO GAF-COPAY-INTL-TAB-TAB-ID-1(GAF-INDEX)00182900
183000        MOVE GAF-OCCURS-ENTRY-COUNTER                  (GAF-INDEX)00183000
183100                         TO GAF-COPAY-INTL-TAB-SLOT-1 (GAF-INDEX).00183100
183200                                                                  00183200
183300     IF IPGNOPTI  =  'MT' OR 'A'                                  00183300
183400        MOVE 2           TO GAF-INTERNAL-TABULAR-COUNT (GAF-INDEX)00183400
183500        MOVE HIGH-VALUES TO GAF-COPAY-INTL-TAB-2       (GAF-INDEX)00183500
183600        MOVE '#IPGN '    TO GAF-COPAY-INTL-TAB-TAB-ID-1(GAF-INDEX)00183600
183700        MOVE GAF-OCCURS-ENTRY-COUNTER                  (GAF-INDEX)00183700
183800                         TO GAF-COPAY-INTL-TAB-SLOT-1 (GAF-INDEX).00183800
183900                                                                  00183900
184000     IF IPGPOPTI  =  'MT' OR 'A'                                  00184000
184100        MOVE 2           TO GAF-INTERNAL-TABULAR-COUNT (GAF-INDEX)00184100
184200        MOVE HIGH-VALUES TO GAF-COPAY-INTL-TAB-2       (GAF-INDEX)00184200
184300        MOVE '#IPGP '    TO GAF-COPAY-INTL-TAB-TAB-ID-1(GAF-INDEX)00184300
184400        MOVE GAF-OCCURS-ENTRY-COUNTER                  (GAF-INDEX)00184400
184500                         TO GAF-COPAY-INTL-TAB-SLOT-1 (GAF-INDEX).00184500
184600                                                                  00184600
184700     IF IPGTOPTI  =  'MT' OR 'A'                                  00184700
184800        MOVE 2           TO GAF-INTERNAL-TABULAR-COUNT (GAF-INDEX)00184800
184900        MOVE HIGH-VALUES TO GAF-COPAY-INTL-TAB-2       (GAF-INDEX)00184900
185000        MOVE '#IPGT '    TO GAF-COPAY-INTL-TAB-TAB-ID-1(GAF-INDEX)00185000
185100        MOVE GAF-OCCURS-ENTRY-COUNTER                  (GAF-INDEX)00185100
185200                         TO GAF-COPAY-INTL-TAB-SLOT-1 (GAF-INDEX).00185200
185300                                                                  00185300
185400     IF IPGSOPTI  =  'MT' OR 'A'                                  00185400
185500        MOVE 2           TO GAF-INTERNAL-TABULAR-COUNT (GAF-INDEX)00185500
185600        MOVE HIGH-VALUES TO GAF-COPAY-INTL-TAB-2       (GAF-INDEX)00185600
185700        MOVE '#IPGS '    TO GAF-COPAY-INTL-TAB-TAB-ID-1(GAF-INDEX)00185700
185800        MOVE GAF-OCCURS-ENTRY-COUNTER                  (GAF-INDEX)00185800
185900                         TO GAF-COPAY-INTL-TAB-SLOT-1 (GAF-INDEX).00185900
186000                                                                  00186000
186100*******                                                           00186100
186200* STS *==> MAP FROM OPTION UNDER SINGLE TABULAR SUPPORT DOES NOT  00186200
186300*     *     CREATE INTERNAL TAB ON WORKFILE, IT SIMPLY UPDATES THE00186300
186400*     *     THE ACCUM RECORD AND SCREEN, THEN REDISPLAYS SCREEN.  00186400
186500*******                                                           00186500
186600                                                                  00186600
186700     IF  FRMNUIDI =  'GTM1'  AND                                  00186700
186800         IBGROPTI =  'MT'                                         00186800
186900     THEN                                                         00186900
187000         MOVE MFRMSLTI  TO  IBGRSLTI  ACWA-DISPLAY-LEN-7          00187000
187100         SET  WT-01-INDEX                     TO  +01             00187100
187200         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        00187200
187300         MOVE -1        TO  IBGROPTL                              00187300
187400         MOVE DFHBMABF  TO  IBGRSLTA   IBGRIDA                    00187400
187500         MOVE SPACES    TO  IBGROPTI                              00187500
187600         MOVE DFHBMUNP  TO  MFRMSLTA                              00187600
187700         MOVE ZEROS     TO  MFRMSLTI   MFRMSLTL                   00187700
187800         MOVE ACWA-DISPLAY-LEN-7                                  00187800
187900                        TO  GAF-COPAY-INTL-TAB-SLOT-1 (GAF-INDEX) 00187900
188000     ELSE                                                         00188000
188100         NEXT SENTENCE.                                           00188100
188200                                                                  00188200
188300     IF  FRMNUIDI =  'GTM1'  AND                                  00188300
188400         IDGDOPTI =  'MT'                                         00188400
188500     THEN                                                         00188500
188600         MOVE MFRMSLTI  TO  IDGDSLTI  ACWA-DISPLAY-LEN-7          00188600
188700         SET  WT-01-INDEX                     TO  +04             00188700
188800         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        00188800
188900         MOVE -1        TO  IDGDOPTL                              00188900
189000         MOVE DFHBMABF  TO  IDGDSLTA   IDGDIDA                    00189000
189100         MOVE SPACES    TO  IDGDOPTI                              00189100
189200         MOVE DFHBMUNP  TO  MFRMSLTA                              00189200
189300         MOVE ZEROS     TO  MFRMSLTI   MFRMSLTL                   00189300
189400         MOVE ACWA-DISPLAY-LEN-7                                  00189400
189500                        TO  GAF-COPAY-INTL-TAB-SLOT-1 (GAF-INDEX) 00189500
189600     ELSE                                                         00189600
189700         NEXT SENTENCE.                                           00189700
189800                                                                  00189800
189900     IF  FRMNUIDI =  'GTM1'  AND                                  00189900
190000         IPGNOPTI =  'MT'                                         00190000
190100     THEN                                                         00190100
190200         MOVE MFRMSLTI  TO  IPGNSLTI  ACWA-DISPLAY-LEN-7          00190200
190300         SET  WT-01-INDEX                     TO  +02             00190300
190400         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        00190400
190500         MOVE -1        TO  IPGNOPTL                              00190500
190600         MOVE DFHBMABF  TO  IPGNSLTA  IPGNIDA                     00190600
190700         MOVE SPACES    TO  IPGNOPTI                              00190700
190800         MOVE DFHBMUNP  TO  MFRMSLTA                              00190800
190900         MOVE ZEROS     TO  MFRMSLTI  MFRMSLTL                    00190900
191000         MOVE ACWA-DISPLAY-LEN-7                                  00191000
191100                        TO  GAF-COPAY-INTL-TAB-SLOT-1 (GAF-INDEX) 00191100
191200     ELSE                                                         00191200
191300         NEXT SENTENCE.                                           00191300
191400                                                                  00191400
191500     IF  FRMNUIDI =  'GTM1'  AND                                  00191500
191600         IPGPOPTI =  'MT'                                         00191600
191700     THEN                                                         00191700
191800         MOVE MFRMSLTI  TO  IPGPSLTI  ACWA-DISPLAY-LEN-7          00191800
191900         SET  WT-01-INDEX                     TO  +05             00191900
192000         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        00192000
192100         MOVE -1        TO  IPGPOPTL                              00192100
192200         MOVE DFHBMABF  TO  IPGPSLTA   IDGDIDA                    00192200
192300         MOVE SPACES    TO  IPGPOPTI                              00192300
192400         MOVE DFHBMUNP  TO  MFRMSLTA                              00192400
192500         MOVE ZEROS     TO  MFRMSLTI   MFRMSLTL                   00192500
192600         MOVE ACWA-DISPLAY-LEN-7                                  00192600
192700                        TO  GAF-COPAY-INTL-TAB-SLOT-1 (GAF-INDEX) 00192700
192800     ELSE                                                         00192800
192900         NEXT SENTENCE.                                           00192900
193000                                                                  00193000
193100     IF  FRMNUIDI =  'GTM1'  AND                                  00193100
193200         IPGTOPTI =  'MT'                                         00193200
193300     THEN                                                         00193300
193400         MOVE MFRMSLTI  TO  IPGTSLTI  ACWA-DISPLAY-LEN-7          00193400
193500         SET  WT-01-INDEX                     TO  +03             00193500
193600         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        00193600
193700         MOVE -1        TO  IPGTOPTL                              00193700
193800         MOVE DFHBMABF  TO  IPGTSLTA  IPGTIDA                     00193800
193900         MOVE SPACES    TO  IPGTOPTI                              00193900
194000         MOVE DFHBMUNP  TO  MFRMSLTA                              00194000
194100         MOVE ZEROS     TO  MFRMSLTI  MFRMSLTL                    00194100
194200         MOVE ACWA-DISPLAY-LEN-7                                  00194200
194300                        TO  GAF-COPAY-INTL-TAB-SLOT-1 (GAF-INDEX) 00194300
194400     ELSE                                                         00194400
194500         NEXT SENTENCE.                                           00194500
194600                                                                  00194600
194700     IF  FRMNUIDI =  'GTM1'  AND                                  00194700
194800         IPGSOPTI =  'MT'                                         00194800
194900     THEN                                                         00194900
195000         MOVE MFRMSLTI  TO  IPGSSLTI  ACWA-DISPLAY-LEN-7          00195000
195100         SET  WT-01-INDEX                     TO  +03             00195100
195200         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        00195200
195300         MOVE -1        TO  IPGSOPTL                              00195300
195400         MOVE DFHBMABF  TO  IPGSSLTA  IPGSIDA                     00195400
195500         MOVE SPACES    TO  IPGSOPTI                              00195500
195600         MOVE DFHBMUNP  TO  MFRMSLTA                              00195600
195700         MOVE ZEROS     TO  MFRMSLTI  MFRMSLTL                    00195700
195800         MOVE ACWA-DISPLAY-LEN-7                                  00195800
195900                        TO  GAF-COPAY-INTL-TAB-SLOT-1 (GAF-INDEX) 00195900
196000     ELSE                                                         00196000
196100         NEXT SENTENCE.                                           00196100
196200*******                                                          |00196200
196300* STS *----------------------------------------------------------*00196300
196400*******                                                           00196400
196500                                                                  00196500
196600     SET  GAF-INDEX  UP BY  1.                                    00196600
196700     MOVE HIGH-VALUES     TO  GAF-ENTRY (GAF-INDEX).              00196700
196800     SET GAF-ENTRY-COUNT  TO  GAF-INDEX.                          00196800
196900                                                                  00196900
197000 2500-900-EXIT. EXIT.                                             00197000
197100                                                                  00197100
197200/*****************************************************************00197200
197300*     P R O C E S S   V A L U E   L I M I T                       00197300
197400******************************************************************00197400
197500 2600-000-PROCESS-VAL-LIMIT     SECTION.                          00197500
197600 2600-010.                                                        00197600
197700                                                                  00197700
197800     IF (ACWA-VAL-LIM-SCREEN-NEG1-3  =  'NEG' OR                  00197800
197900         ACWA-VAL-LIM-SCREEN-NEG2-3  =  'NEG') OR                 00197900
197800        (ACWA-VAL-LIM-SCREEN-NEG1-3  =  'UNL' OR                  00197910
197900         ACWA-VAL-LIM-SCREEN-NEG2-3  =  'UNL')                    00197920
198000         GO TO 2600-900-EXIT.                                     00198000
198100                                                                  00198100
198200     IF  ACWA-BNMXVALI-N NUMERIC                                  00198200
198300     THEN                                                         00198300
198400         IF  BENVLQLI  = '5'                                      00198400
198500         THEN                                                     00198500
198600             MOVE ACWA-BNMXVALI-N  TO  ACWA-VALUE-LIMIT-7         00198600
198700             MOVE ZEROS            TO  ACWA-VALUE-LIMIT-2         00198700
198800             GO TO 2600-900-EXIT                                  00198800
198900         ELSE                                                     00198900
199000             MOVE ACWA-BNMXVALI-N  TO  ACWA-VALUE-LIMIT-9-9       00199000
199100             GO TO 2600-900-EXIT                                  00199100
199200     ELSE                                                         00199200
199300         NEXT SENTENCE.                                           00199300
199400                                                                  00199400
199500     IF  ACWA-VAL-LIM-SCREEN-1  =  '.'                            00199500
199600         MOVE ACWA-VAL-LIM-SCREEN-7  TO  ACWA-VALUE-LIMIT-7       00199600
199700         MOVE ACWA-VAL-LIM-SCREEN-2  TO  ACWA-VALUE-LIMIT-2       00199700
199800         GO TO 2600-900-EXIT.                                     00199800
199900                                                                  00199900
200000 2600-900-EXIT. EXIT.                                             00200000
200100                                                                  00200100
200200/*****************************************************************00200200
200300* 3000 UPDATE GAC RECORD                                         *00200300
200400*                                                                *00200400
200500*    THIS ROUTINE REWRITES THE UPDATED RECORD TO THE WORK FILE.  *00200500
200600******************************************************************00200600
200700 3000-000-UPDATE-GAF-RECORD     SECTION.                          00200700
200800 3000-010.                                                        00200800
200900                                                                  00200900
201000     COMPUTE  GCIO-RECORD-LENGTH   =                              00201000
201100         GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-ACP-FIXED-LEN  +     00201100
201200         (GC-GCTABULR-ACP-VARY-LEN * GAF-ENTRY-COUNT).            00201200
201300                                                                  00201300
201400     MOVE  GC-GCIO-ACCESS-CODE-WU  TO  GCIO-FILE-ACCESS-CODE.     00201400
201500                                                                  00201500
201600     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00201600
201700                COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           00201700
201800                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)  END-EXEC.     00201800
201900                                                                  00201900
202000     IF NOT GCIO-GOOD-RETURN                                      00202000
202100        MOVE WS-ABCODE-1PF4        TO WS-ABCODE                   00202100
202200        MOVE WS-ABCODE-1PF4-MSG    TO WS-ABCODE-MSG               00202200
202300        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00202300
202400                                                                  00202400
202500 3000-900-EXIT. EXIT.                                             00202500
202600                                                                  00202600
202700/*****************************************************************00202700
202800* 3100  READ RECORD                                              *00202800
202900*                                                                *00202900
203000*    THIS ROUTINE READS THE RECORD THAT CORRESPONDS TO THE KEY   *00203000
203100*  FIELDS FOUND ON THE SCREEN'S HEADING.                         *00203100
203200******************************************************************00203200
203300 3100-000-READ-RECORD           SECTION.                          00203300
203400 3100-010.                                                        00203400
203500                                                                  00203500
203600     COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =  GC-GCIOPARM-LEN  +   00203600
203700              GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-ACP-FIXED-LEN  +00203700
203800             (GC-GCTABULR-ACP-VARY-LEN  *                         00203800
203900                                   GC-GCTABULR-ACP-VARY-MAX-OCUR).00203900
204000                                                                  00204000
204100     IF ACWA-WF-ALL-LEVEL-TAB-COMP  >  ZERO                       00204100
204200        NEXT SENTENCE                                             00204200
204300     ELSE                                                         00204300
204400        EXEC CICS GETMAIN                                         00204400
204500               SET(ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD)      00204500
204600               INITIMG(WS-HEX-00)                                 00204600
204700               LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                 00204700
204800               END-EXEC                                           00204800
204900        SET ACWA-WF-ALL-LEVEL-TAB-PNTR     TO                     00204900
205000                 ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD.        00205000
205100                                                                  00205100
205200     IF FRMNUIDI  =  'GS3A'                                       00205200
205300        PERFORM 6000-000-BUILD-GROUP-SPEC-KEY.                    00205300
205400     IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            00205400
205500        PERFORM 6100-000-BUILD-CONTRACT-KEY.                      00205500
205600     IF FRMNUIDI  =  'GC8A'                                       00205600
205700        PERFORM 6200-000-BUILD-BEN-PROV-KEY.                      00205700
205800                                                                  00205800
205900     IF GCIO-WORKFILE-KEY  =  WORK-RECORD-KEY                     00205900
206000        GO TO 3100-900-EXIT.                                      00206000
206100                                                                  00206100
206200     MOVE GC-GCPSWORK-DDNAME     TO GCIO-FILE-DDNAME.             00206200
206300     MOVE GC-GCIO-AREA-1         TO GCIO-IO-AREA-TO-USE.          00206300
206400     MOVE GCIO-WORKFILE-KEY      TO GCIO-FILE-KEY.                00206400
206500     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO-FILE-ACCESS-CODE.        00206500
206600     MOVE GC-GCTABULR-ACP-VARY-MAX-OCUR  TO  GAF-ENTRY-COUNT.     00206600
206700                                                                  00206700
206800     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00206800
206900                COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           00206900
207000                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)     END-EXEC.  00207000
207100                                                                  00207100
207200     IF NOT GCIO-GOOD-RETURN                                      00207200
207300        MOVE WS-ABCODE-1PFJ        TO WS-ABCODE                   00207300
207400        MOVE WS-ABCODE-1PFJ-MSG    TO WS-ABCODE-MSG               00207400
207500        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00207500
207600                                                                  00207600
207700 3100-900-EXIT. EXIT.                                             00207700
207800                                                                  00207800
207900/*****************************************************************00207900
208000* 3200  READ REC FOR UPDATE                                      *00208000
208100*                                                                *00208100
208200*    THIS ROUTINE READS THE RECORD FOR UPDATE THAT CORRESPONDS   *00208200
208300*  TO THE KEY FIELDS FOUND ON THE SCREEN'S HEADING.              *00208300
208400******************************************************************00208400
208500 3200-000-READ-REC-FOR-UPDATE   SECTION.                          00208500
208600 3200-010.                                                        00208600
208700                                                                  00208700
208800     COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =  GC-GCIOPARM-LEN  +   00208800
208900              GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-ACP-FIXED-LEN  +00208900
209000             (GC-GCTABULR-ACP-VARY-LEN  *                         00209000
209100                                   GC-GCTABULR-ACP-VARY-MAX-OCUR).00209100
209200                                                                  00209200
209300     IF ACWA-WF-ALL-LEVEL-TAB-COMP  >  ZERO                       00209300
209400        NEXT SENTENCE                                             00209400
209500     ELSE                                                         00209500
209600        EXEC CICS GETMAIN                                         00209600
209700               SET(ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD)      00209700
209800               INITIMG(WS-HEX-00)                                 00209800
209900               LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                 00209900
210000               END-EXEC                                           00210000
210100        SET ACWA-WF-ALL-LEVEL-TAB-PNTR     TO                     00210100
210200                 ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD.        00210200
210300                                                                  00210300
210400                                                                  00210400
210500     IF FRMNUIDI  =  'GS3A'                                       00210500
210600        PERFORM 6000-000-BUILD-GROUP-SPEC-KEY.                    00210600
210700     IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            00210700
210800        PERFORM 6100-000-BUILD-CONTRACT-KEY.                      00210800
210900     IF FRMNUIDI  =  'GC8A'                                       00210900
211000        PERFORM 6200-000-BUILD-BEN-PROV-KEY.                      00211000
211100                                                                  00211100
211200     MOVE GC-GCPSWORK-DDNAME     TO GCIO-FILE-DDNAME.             00211200
211300     MOVE GC-GCIO-AREA-1         TO GCIO-IO-AREA-TO-USE.          00211300
211400     MOVE GCIO-WORKFILE-KEY      TO GCIO-FILE-KEY.                00211400
211500     MOVE GC-GCIO-ACCESS-CODE-RU TO GCIO-FILE-ACCESS-CODE.        00211500
211600     MOVE GC-GCTABULR-ACP-VARY-MAX-OCUR  TO  GAF-ENTRY-COUNT.     00211600
211700                                                                  00211700
211800     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00211800
211900                COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           00211900
212000                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)  END-EXEC.     00212000
212100                                                                  00212100
212200 3200-900-EXIT. EXIT.                                             00212200
212300                                                                  00212300
212400/*****************************************************************00212400
212500* 4000  DISPLAY FIRST SCREEN                                     *00212500
212600*                                                                *00212600
212700*    THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM ONE OF THE*00212700
212800*  TABULAR MENUS, THE MENU WILL READ THE ALL LEVEL TABULAR IF IT *00212800
212900*  EXISTS (IF IT DOESN'T EXIST THE MENU WILL ADD A NEW ONE TO THE*00212900
213000*  WORK FILE) THEN PLACE THE ADDRESS OF THE TABULAR RECORD WITHIN*00213000
213100*  A COMMON AREA PARAMETER LIST.  THE MENU THEN MOVES THE KEY    *00213100
213200*  FIELDS TO THE COMMON AREA AND PASSES THE ADDRESS OF THE       *00213200
213300*  PARAMETER LIST IN A FULLWORD TO THIS PROGRAM.                 *00213300
213400*    WE THEN SET THIS ADDRESS INTO A BLL CELL AND ACCESS THE     *00213400
213500*  INFORMATION NEEDED TO BUILD THE SCREEN IMAGE.                 *00213500
213600******************************************************************00213600
213700 4000-000-DISPLAY-FIRST-SCREEN  SECTION.                          00213700
213800 4000-010.                                                        00213800
213900                                                                  00213900
214000     IF EIBCALEN  >  0                                            00214000
214100        NEXT SENTENCE                                             00214100
214200     ELSE                                                         00214200
214300        MOVE WS-ABCODE-1PC1        TO WS-ABCODE                   00214300
214400        MOVE WS-ABCODE-1PC1-MSG    TO WS-ABCODE-MSG               00214400
214500        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00214500
214600                                                                  00214600
214700     MOVE LOW-VALUES               TO GA1XI01I.                   00214700
214800                                                                  00214800
214900     MOVE GCA-FROM-MENU-ID  TO  FRMNUIDO.                         00214900
215000                                                                  00215000
215100     IF GCA-FROM-MENU-ID  =  'GS3A'                               00215100
215200        MOVE GCA-PLAN-CODE             TO GRP-SPEC-PLAN-CODE      00215200
215300        MOVE GCA-GROUP-NUM             TO GRP-SPEC-GROUP-NUM      00215300
215400        MOVE GCA-SECTION-NUM           TO GRP-SPEC-SECTION-NUM    00215400
215500        MOVE GCA-PKG-CODE              TO GRP-SPEC-PKG-CODE       00215500
215600        MOVE GCA-FAM-REL-LVL           TO GRP-SPEC-FAM-REL-LVL    00215600
215700        MOVE GCA-EFFECTIVE-DATE        TO GRP-SPEC-EFF-DATE       00215700
215800        MOVE ' GROUP SPEC. ALL-LEVEL TABULAR MAINTENANCE'         00215800
215900                                       TO TTLELNEO                00215900
216000        MOVE GROUP-SPECIFIC-ID-LINE    TO IDLINEO.                00216000
216100                                                                  00216100
216200     IF GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                     00216200
216300        MOVE GCA-PLAN-CODE             TO CONTRACT-PLAN-CODE      00216300
216400        MOVE GCA-GROUP-NUM             TO CONTRACT-GROUP-NUM      00216400
216500        MOVE GCA-SECTION-NUM           TO CONTRACT-SECTION-NUM    00216500
216600        MOVE GCA-PKG-CODE              TO CONTRACT-PKG-CODE       00216600
216700        MOVE GCA-L-O-B                 TO CONTRACT-LOB            00216700
216800        MOVE GCA-PROV-CTL              TO CONTRACT-PROV-CTL       00216800
216900        MOVE GCA-FAM-REL-LVL           TO CONTRACT-FAM-REL-LVL    00216900
217000        MOVE GCA-EFFECTIVE-DATE        TO CONTRACT-EFF-DATE       00217000
217100        MOVE '   CONTRACT ALL-LEVEL TABULAR MAINTENANCE'          00217100
217200                                       TO TTLELNEO                00217200
217300        MOVE CONTRACT-ID-LINE          TO IDLINEO.                00217300
217400                                                                  00217400
217500     IF GCA-FROM-MENU-ID  =  'GC8A'                               00217500
217600        MOVE GCA-PLAN-CODE             TO BEN-PROV-PLAN-CODE      00217600
217700        MOVE GCA-GROUP-NUM             TO BEN-PROV-GROUP-NO       00217700
217800        MOVE GCA-SECTION-NUM           TO BEN-PROV-SECTION-NO     00217800
217900        MOVE GCA-PKG-CODE              TO BEN-PROV-PKG-CODE       00217900
218000        MOVE GCA-L-O-B                 TO BEN-PROV-LOB            00218000
218100        MOVE GCA-PROV-CTL              TO BEN-PROV-PROV-CTL       00218100
218200        MOVE GCA-FAM-REL-LVL           TO BEN-PROV-FAM-REL-LVL    00218200
218300        MOVE GCA-EFFECTIVE-DATE        TO BEN-PROV-EFF-DATE       00218300
218400        MOVE GCA-BEN-PROV-ID           TO BEN-PROV-ID-NO          00218400
218500        MOVE '   BEN. PROV ALL-LEVEL TABULAR MAINTENANCE'         00218500
218600                                          TO  TTLELNEO            00218600
218700        MOVE BENEFIT-PROVISION-ID-LINE TO IDLINEO.                00218700
218800                                                                  00218800
218900     MOVE 'GA1P'                       TO FUNCTONO.               00218900
219000     MOVE '001P00'                     TO SCRNIDNO.               00219000
219100     MOVE ACP-TITLE-LINE               TO TITLEO.                 00219100
219200                                                                  00219200
219300     MOVE GCA-RECORD-POINTER-COMP  TO ACWA-WF-ALL-LEVEL-TAB-COMP. 00219300
219400     SET ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD  TO             00219400
219500                    GCA-RECORD-POINTER.                           00219500
219600                                                                  00219600
219700     IF  NOT WRK-STAT-CONT-MAINT AND                              00219700
219800         NOT WRK-STAT-GRP-SPEC-MAINT                              00219800
219900     THEN                                                         00219900
220000         MOVE WS-ABCODE-1PC2        TO WS-ABCODE                  00220000
220100         MOVE WS-ABCODE-1PC2-MSG    TO WS-ABCODE-MSG              00220100
220200         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                   00220200
220300                                                                  00220300
220400     IF  NOT WRK-REC-GROUP-SPEC-TAB AND                           00220400
220500         NOT WRK-REC-CONT-TAB       AND                           00220500
220600         NOT WRK-REC-CONT-BEN-TAB-PROV                            00220600
220700     THEN                                                         00220700
220800         MOVE WS-ABCODE-1PC3        TO WS-ABCODE                  00220800
220900         MOVE WS-ABCODE-1PC3-MSG    TO WS-ABCODE-MSG              00220900
221000         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                   00221000
221100                                                                  00221100
221200     MOVE GAF-ENTRY-COUNT         TO  GAF-ENTRY-COUNT.            00221200
221300     MOVE GCA-ALL-LEVEL-TAB-ID    TO  TABIDO.                     00221300
221400     MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  TABSLTNO.                   00221400
221500     SET GAF-INDEX  TO  1.                                        00221500
221600                                                                  00221600
221700     IF EIBTRNID  =  'GC4A' OR  'GC3A' OR  'GC8A'  OR 'GTM1'      00221700
221800        MOVE '0000000'  TO GCA-OCCURS-ENTRY-COUNTER.              00221800
221900                                                                  00221900
222000     IF GCA-OCCURS-ENTRY-COUNTER  =  '0000000'                    00222000
222100        GO TO 4000-200-BUILD-SCREEN.                              00222100
222200                                                                  00222200
222300     MOVE GCA-OCCURS-ENTRY-COUNTER  TO  ACWA-DISPLAY-LEN-7.       00222300
222400                                                                  00222400
222500 4000-100-FIND-RIGHT-OCCURS.                                      00222500
222600     IF GAF-COPAY-BENEFIT-PERIOD(GAF-INDEX) NOT = HIGH-VALUES AND 00222600
222700        GAF-OCCURS-ENTRY-COUNTER(GAF-INDEX) NOT =                 00222700
222800                                                ACWA-DISPLAY-LEN-700222800
222900     THEN                                                         00222900
223000         IF GAF-INDEX  <  GAF-ENTRY-COUNT                         00223000
223100            SET GAF-INDEX  UP BY  1                               00223100
223200            GO TO 4000-100-FIND-RIGHT-OCCURS                      00223200
223300         ELSE                                                     00223300
223400             MOVE WS-ABCODE-1PL1        TO WS-ABCODE              00223400
223500             MOVE WS-ABCODE-1PL1-MSG    TO WS-ABCODE-MSG          00223500
223600             MOVE -1                    TO MFRMSLTL               00223600
223700             PERFORM 9800-000-ERROR-MSG-THEN-ABEND                00223700
223800     ELSE                                                         00223800
223900         NEXT SENTENCE.                                           00223900
224000                                                                  00224000
224100                                                                  00224100
224200 4000-200-BUILD-SCREEN.                                           00224200
224300                                                                  00224300
224400     IF  GCA-ADD-DEL-IND  =  'A' OR                               00224400
224500         GAF-ENTRY-COUNT  =  1                                    00224500
224600     THEN                                                         00224600
224700         MOVE 'CHG/ADD'  TO  DELADDO                              00224700
224800         MOVE DFHBMASD   TO  DLOPTLTA,  DELOPTNA                  00224800
224900         IF  GCA-OCCURS-ENTRY-COUNTER  =  '0000000'               00224900
225000         THEN                                                     00225000
225100             PERFORM 4100-000-DISPLAY-SKELETON                    00225100
225200         ELSE                                                     00225200
225300             PERFORM 4400-000-BUILD-DISPLAY                       00225300
225400     ELSE                                                         00225400
225500         MOVE 'CHG/DEL'  TO  DELADDO                              00225500
225600         MOVE 'D'        TO  DELOLITO                             00225600
225700         PERFORM 4400-000-BUILD-DISPLAY.                          00225700
225800                                                                  00225800
225900 4000-900-EXIT. EXIT.                                             00225900
226000                                                                  00226000
226100/*****************************************************************00226100
226200* 4100  DISPLAY SKELETON                                         *00226200
226300*                                                                *00226300
226400*    THIS ROUTINE REINITIALIZES THE SCREEN FOR THE OPERATOR      *00226400
226500*  AFTER THEY HAVE REVIEWED THE ENTRY THEY JUST ADDED AND        *00226500
226600*  INDICATED THAT THEY WANTED TO ADD MORE BY KEYING 'ENTER'.     *00226600
226700******************************************************************00226700
226800 4100-000-DISPLAY-SKELETON      SECTION.                          00226800
226900 4100-010.                                                        00226900
227000                                                                  00227000
227100     MOVE SPACES TO ERRMSGO.                                      00227100
227200                                                                  00227200
227300     MOVE DFHBMFSE  TO  PERIODA.                                  00227300
227400                                                                  00227400
227500     MOVE DFHBMUNP  TO  BENVLQLA  FAMINDIA  INTDESKA  LOBA        00227500
227600                        IBGROPTA  IPGNOPTA  IPGTOPTA  MFRMSLTA    00227600
227700                        IDGDOPTA  IPGPOPTA  IPGSOPTA.             00227700
227800                                                                  00227800
227900     MOVE ALL '_'  TO  PERIODO  BENVLQLO  LOBO                    00227900
228000                       FAMINDIO  PLCTRMTO.                        00228000
228100                                                                  00228100
228200     MOVE LOW-VALUES  TO  INTDESKO  MFRMSLTO                      00228200
228300                          IBGROPTO  IPGNOPTO  IPGTOPTO            00228300
228400                          IDGDOPTO  IPGPOPTO  IPGSOPTO.           00228400
228500                                                                  00228500
228600     MOVE ZEROS                                                   00228600
228700       TO COPAYINO CSTCONTO  PERTQALO  DEFINTNO                   00228700
228800          DAYFACIO SRVGRUPO  PRTIMEFO  MANAPLIO  CONDLIFO         00228800
228900          ASCDSCDO CLMLVLIO  INTRVALO  INTTYPEO  BNMXVALO         00228900
229000          FYIVALO  OVRDINDO  NEWVALUO  PRTIMEFO INTRVALO          00229000
229100          CONDALLO CONDEXCO  CONDICDO  CONDTABO  CONDMENO         00229100
229200          CONDEACO CONDEMCO  CONDSMIO  CONDNSMO  BISNDINO         00229200
229300          CONDDRGO CONDALCO  CONDOBNO  CONDOBCO  CONDMALO         00229300
229400          CONDCARO CONDOBSO  CONDKDYO  CONDACCO  CONDSUIO         00229400
229500          CONDPECO  CONDNEMO  CONDTMJO  CONDINFO AGEQLLO AGEQLHO  00229500
229600          OENTCTRO IBGRSLTO  IPGNSLTO  IPGTSLTO  TOCURANO         00229600
229700          IDGDSLTO IPGPSLTO  AGELIMLO  AGELIMHO  RELPINDO         00229700
229800          FEAKINDO TIMEDOLO  IPGSSLTO  ACCUMIDO  CAPINDO          00229800
229900          SABDINDO BENTYPO   TIERCDO   TIERLVO.                   00229900
230000                                                                  00230000
230100     MOVE '01'    TO  COCURANO.                                   00230100
230200     MOVE -1      TO  PERIODL.                                    00230200
230300                                                                  00230300
230400     PERFORM 9000-000-SEND-ERASE-RETURN.                          00230400
230500                                                                  00230500
230600 4100-900-EXIT. EXIT.                                             00230600
230700                                                                  00230700
230800/*****************************************************************00230800
230900* 4200 DISPLAY NEXT                                              *00230900
231000*                                                                *00231000
231100*    THIS ROUTINE WILL FIND THE ENTRY CORRESPONDING TO THE       *00231100
231200*  SCREEN'S DISPLAY AND THEN POSITION TO THE NEXT ENTRY, IF THE  *00231200
231300*  NEXT ENTRY IS THE LAST IN THE LIST THE CODE WILL RECOGNIZE    *00231300
231400*  THAT AND POSITION TO THE FIRST ENTRY, ALSO DISPLAYING AN      *00231400
231500*  INFORMATIONAL MESSAGE.                                        *00231500
231600******************************************************************00231600
231700 4200-000-DISPLAY-NEXT          SECTION.                          00231700
231800 4200-010.                                                        00231800
231900                                                                  00231900
232000     MOVE GAF-ENTRY-COUNT  TO  GAF-ENTRY-COUNT.                   00232000
232100     SET GAF-INDEX         TO  1.                                 00232100
232200     MOVE OENTCTRO         TO  ACWA-DISPLAY-LEN-7.                00232200
232300                                                                  00232300
232400                                                                  00232400
232500     SET  CURNT-OCURS-BIN  TO  GAF-INDEX.                         00232500
232600     MOVE CURNT-OCURS-BIN  TO  CURNT-OCURS-PKD.                   00232600
232700     MOVE CURNT-OCCURS-OUT TO  COCURANO.                          00232700
232800                                                                  00232800
232900     IF GAF-ENTRY-COUNT  >  1                                     00232900
233000        COMPUTE  TOTAL-OCURS-UNK  =  GAF-ENTRY-COUNT  -  1        00233000
233100        MOVE  TOTAL-OCCURS-OUT  TO  TOCURANO                      00233100
233200     ELSE                                                         00233200
233300        MOVE  '01'              TO  TOCURANO.                     00233300
233400                                                                  00233400
233500                                                                  00233500
233600 4200-100-FIND-RIGHT-OCCURS.                                      00233600
233700                                                                  00233700
233800     IF GAF-COPAY-BENEFIT-PERIOD(GAF-INDEX)  NOT = HIGH-VALUES AND00233800
233900        GAF-OCCURS-ENTRY-COUNTER(GAF-INDEX)  NOT =                00233900
234000                                                ACWA-DISPLAY-LEN-700234000
234100     THEN                                                         00234100
234200         IF  GAF-INDEX  <  (GAF-ENTRY-COUNT - 1)                  00234200
234300         THEN                                                     00234300
234400             SET GAF-INDEX  UP BY  1                              00234400
234500             GO TO 4200-100-FIND-RIGHT-OCCURS                     00234500
234600         ELSE                                                     00234600
234700             SET GAF-INDEX  TO  1                                 00234700
234800             SET  WT-01-INDEX                     TO +20          00234800
234900             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      00234900
235000     ELSE                                                         00235000
235100         IF  GAF-INDEX  <  (GAF-ENTRY-COUNT - 1)                  00235100
235200         THEN                                                     00235200
235300             SET GAF-INDEX  UP BY  1                              00235300
235400         ELSE                                                     00235400
235500             SET GAF-INDEX  TO  1                                 00235500
235600             SET  WT-01-INDEX                     TO +20          00235600
235700             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO.     00235700
235800                                                                  00235800
235900     PERFORM 4400-000-BUILD-DISPLAY.                              00235900
236000                                                                  00236000
236100 4200-900-EXIT. EXIT.                                             00236100
236200                                                                  00236200
236300/*****************************************************************00236300
236400* 4300 DISPLAY PREV                                              *00236400
236500*                                                                *00236500
236600*    THIS ROUTINE WILL FIND THE ENTRY CORRESPONDING TO THE       *00236600
236700*  SCREEN'S DISPLAY AND THEN POSITION TO THE NEXT PREVIOUS ENTRY,*00236700
236800*  IF THE CURRENT ENTRY IS THE FIRST IN THE LIST THE CODE WILL   *00236800
236900*  RECOGNIZE THAT AND POSITION TO THE LAST ENTRY, ALSO DISPLAYING*00236900
237000*  AN INFORMATIONAL MESSAGE.                                     *00237000
237100******************************************************************00237100
237200 4300-000-DISPLAY-PREV          SECTION.                          00237200
237300 4300-010.                                                        00237300
237400                                                                  00237400
237500     MOVE GAF-ENTRY-COUNT  TO  GAF-ENTRY-COUNT.                   00237500
237600     SET  GAF-INDEX        TO  1.                                 00237600
237700     MOVE OENTCTRO         TO  ACWA-DISPLAY-LEN-7.                00237700
237800                                                                  00237800
237900 4300-100-FIND-RIGHT-OCCURS.                                      00237900
238000                                                                  00238000
238100     IF GAF-COPAY-BENEFIT-PERIOD(GAF-INDEX)  NOT = HIGH-VALUES AND00238100
238200        GAF-OCCURS-ENTRY-COUNTER(GAF-INDEX)  NOT =                00238200
238300                                                ACWA-DISPLAY-LEN-700238300
238400     THEN                                                         00238400
238500         IF  GAF-INDEX  <  (GAF-ENTRY-COUNT - 1)                  00238500
238600         THEN                                                     00238600
238700             SET GAF-INDEX  UP BY  1                              00238700
238800             GO TO 4300-100-FIND-RIGHT-OCCURS                     00238800
238900         ELSE                                                     00238900
239000             SET GAF-INDEX  TO  1                                 00239000
239100             SET  WT-01-INDEX                     TO +20          00239100
239200             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      00239200
239300     ELSE                                                         00239300
239400         IF  GAF-INDEX  NOT =  1                                  00239400
239500         THEN                                                     00239500
239600             SET GAF-INDEX  DOWN BY  1                            00239600
239700         ELSE                                                     00239700
239800             SET GAF-INDEX  TO  GAF-ENTRY-COUNT                   00239800
239900             SET GAF-INDEX  DOWN BY  1                            00239900
240000             SET  WT-01-INDEX                     TO +21          00240000
240100             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO.     00240100
240200                                                                  00240200
240300     PERFORM 4400-000-BUILD-DISPLAY.                              00240300
240400                                                                  00240400
240500 4300-900-EXIT. EXIT.                                             00240500
240600                                                                  00240600
240700/*****************************************************************00240700
240800* 4400 BUILD DISPLAY                                             *00240800
240900*                                                                *00240900
241000*    THIS ROUTINE WILL MOVE ALL THE FIELDS FROM THE OCCURENCE    *00241000
241100*  SPECIFIED BY INDEX GAF-INDEX TO THE SCREEN.                   *00241100
241200******************************************************************00241200
241300 4400-000-BUILD-DISPLAY         SECTION.                          00241300
241400 4400-010.                                                        00241400
241500                                                                  00241500
241600     IF  DELADDI  =  'CHG/DEL'                                    00241600
241700     THEN                                                         00241700
241800         MOVE 'D'  TO  DELOLITO                                   00241800
241900     ELSE                                                         00241900
242000         MOVE SPACE  TO  DELOLITO.                                00242000
242100                                                                  00242100
242200     MOVE SPACE                                       TO DELOPTNO.00242200
242300     MOVE GAF-OCCURS-ENTRY-COUNTER       (GAF-INDEX)  TO          00242300
242400                                               ACWA-DISPLAY-LEN-7.00242400
242500     MOVE ACWA-DISPLAY-LEN-7                          TO OENTCTRO.00242500
242600     MOVE GAF-COPAY-DAY-FACTOR-IND      (GAF-INDEX)  TO  DAYFACIO.00242600
242700     MOVE GAF-COPAY-CO-PAY-IND          (GAF-INDEX)  TO  COPAYINO.00242700
242800     MOVE GAF-COPAY-BISCENDING-IND       (GAF-INDEX)  TO BISNDINO.00242800
242900     MOVE GAF-COPAY-ASCEND-DESCEND-IND   (GAF-INDEX)  TO ASCDSCDO.00242900
      *P21595 CHANGES STARTS                                            00242910
242900     MOVE GAF-COPAY-BEN-TYPE             (GAF-INDEX)  TO BENTYPO. 00242911
242900     MOVE GAF-COPAY-TIER-CODE            (GAF-INDEX)  TO TIERCDO. 00242912
242900     MOVE GAF-COPAY-TIER-LVL             (GAF-INDEX)  TO TIERLVO. 00242913
      *P21595 CHANGES ENDS                                              00242920
243000     MOVE GAF-COPAY-DEFINITION          (GAF-INDEX)  TO  DEFINTNO.00243000
243100     MOVE GAF-COPAY-MANDATORY-IND       (GAF-INDEX)  TO  MANAPLIO.00243100
243200     MOVE GAF-COPAY-TIME-DOLLAR-IND     (GAF-INDEX)  TO  TIMEDOLO.00243200
243300     MOVE GAF-COPAY-COST-CONTAIN-IND    (GAF-INDEX)  TO  CSTCONTO.00243300
243400     MOVE GAF-COPAY-BENEFIT-PERIOD      (GAF-INDEX)  TO  PERIODO. 00243400
243500     MOVE GAF-COPAY-BEN-PER-TIME-QUAL   (GAF-INDEX)  TO  PERTQALO.00243500
243600     MOVE GAF-COPAY-FAM-OR-INDIV        (GAF-INDEX)  TO  FAMINDIO.00243600
243700     MOVE GAF-COPAY-PLACE-OF-TREATMENT  (GAF-INDEX)  TO  PLCTRMTO.00243700
243800     MOVE GAF-COPAY-SERVICE-GROUP       (GAF-INDEX)  TO  SRVGRUPO.00243800
243900     MOVE GAF-COPAY-BEN-PER-TIME-FCTR   (GAF-INDEX)  TO           00243900
244000                                               ACWA-DISPLAY-LEN-3.00244000
244100     MOVE ACWA-DISPLAY-LEN-3                         TO  PRTIMEFO.00244100
244200     MOVE GAF-COPAY-CLAIM-LVL-ACCUM-IND (GAF-INDEX)  TO  CLMLVLIO.00244200
244300     MOVE GAF-COPAY-AGE-LIMIT-FROM       (GAF-INDEX)  TO          00244300
244400                                               ACWA-DISPLAY-LEN-3.00244400
244500     MOVE ACWA-DISPLAY-LEN-3                          TO AGELIMLO.00244500
244600     MOVE GAF-COPAY-AGE-LIMIT-TO         (GAF-INDEX)  TO          00244600
244700                                               ACWA-DISPLAY-LEN-3.00244700
244800     MOVE ACWA-DISPLAY-LEN-3                          TO AGELIMHO.00244800
244900     MOVE GAF-COPAY-FEAK-IND             (GAF-INDEX)  TO FEAKINDO.00244900
245000     MOVE GAF-COPAY-ACCUMID              (GAF-INDEX)  TO ACCUMIDO.00245000
245100     MOVE GAF-COPAY-COMB-APPLIED-IND     (GAF-INDEX)  TO CAPINDO. 00245100
245200     MOVE GAF-COPAY-SEL-ADDL-BEN-DET     (GAF-INDEX)  TO SABDINDO.00245200
245300     MOVE GAF-COPAY-AGE-QUAL-IND-FROM    (GAF-INDEX)  TO AGEQLLO. 00245300
245400     MOVE GAF-COPAY-AGE-QUAL-IND-TO      (GAF-INDEX)  TO AGEQLHO. 00245400
245500     MOVE GAF-COPAY-RELATIONSHIP-IND     (GAF-INDEX)  TO RELPINDO.00245500
245600                                                                  00245600
245700     MOVE GAF-COPAY-INTERVAL-TIME-FCTR  (GAF-INDEX)  TO           00245700
245800                                               ACWA-DISPLAY-LEN-3.00245800
245900     MOVE ACWA-DISPLAY-LEN-3                          TO INTRVALO.00245900
246000     MOVE GAF-COPAY-INTERVAL-TYPE       (GAF-INDEX)  TO  INTTYPEO.00246000
246100     MOVE GAF-COPAY-L-O-B               (GAF-INDEX)  TO  LOBO.    00246100
246200     MOVE GAF-COPAY-VALUE-LIMIT         (GAF-INDEX)  TO           00246200
246300                                               ACWA-VALUE-LIMIT-9.00246300
246400     IF  ACWA-VALUE-LIMIT-9-9 = -1                                00246400
246500     THEN                                                         00246500
246600         MOVE 'NEG' TO BNMXVALO                                   00246600
246700     ELSE                                                         00246700
246400     IF  ACWA-VALUE-LIMIT-9-9 = -2                                00246710
246500     THEN                                                         00246720
246600         MOVE 'UNL' TO BNMXVALO                                   00246730
246700     ELSE                                                         00246740
246800         IF  GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'          00246800
246900         THEN                                                     00246900
247000             MOVE ACWA-VALUE-LIMIT-9    TO ACWA-EDIT-VALUE-LIMIT  00247000
247100             MOVE ACWA-EDIT-VALUE-LIMIT TO BNMXVALO               00247100
247200         ELSE                                                     00247200
247300             MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX) TO            00247300
247400                                              ACWA-DISPLAY-LEN-9-200247400
247500             MOVE ACWA-DISPLAY-LEN-9-X     TO ACWA-DISPLAY-9      00247500
247600             MOVE SPACES                   TO ACWA-DISPLAY-1      00247600
247700             MOVE ACWA-DISPLAY-VALUE-LIMIT TO BNMXVALO.           00247700
247800                                                                  00247800
247900     MOVE GAF-COPAY-VALUE-QUALIFIER     (GAF-INDEX)  TO  BENVLQLO.00247900
248000     MOVE GAF-COPAY-INTERVAL-OVRD-VALUE (GAF-INDEX)  TO           00248000
248100                                               ACWA-DISPLAY-LEN-5.00248100
248200     MOVE ACWA-DISPLAY-LEN-5                         TO  NEWVALUO.00248200
248300     MOVE GAF-COPAY-INTERVAL-OVRD-IND   (GAF-INDEX)  TO  OVRDINDO.00248300
248400     MOVE GAF-COPAY-INTERNAL-DESCRIPTOR (GAF-INDEX)  TO  INTDESKO.00248400
248500     MOVE GAF-COND-ALL-BIT              (GAF-INDEX)  TO  CONDALLO.00248500
248600     MOVE GAF-COND-EXCLUSION-BIT        (GAF-INDEX)  TO  CONDEXCO.00248600
248700     MOVE GAF-COND-ICD-BIT              (GAF-INDEX)  TO  CONDICDO.00248700
248800     MOVE GAF-COND-TB-BIT               (GAF-INDEX)  TO  CONDTABO.00248800
248900     MOVE GAF-COND-MENTAL-BIT           (GAF-INDEX)  TO  CONDMENO.00248900
249000     MOVE GAF-COND-DRUG-BIT             (GAF-INDEX)  TO  CONDDRGO.00249000
249100     MOVE GAF-COND-ALCOHOL-BIT          (GAF-INDEX)  TO  CONDALCO.00249100
249200     MOVE GAF-COND-OB-COMP-BIT          (GAF-INDEX)  TO  CONDOBCO.00249200
249300     MOVE GAF-COND-OB-NORM-BIT          (GAF-INDEX)  TO  CONDOBNO.00249300
249400     MOVE GAF-COND-MALIGNANCY-BIT       (GAF-INDEX)  TO  CONDMALO.00249400
249500     MOVE GAF-COND-CARDIAC-DISEASE-BIT  (GAF-INDEX)  TO  CONDCARO.00249500
249600     MOVE GAF-COND-OBESITY-BIT          (GAF-INDEX)  TO  CONDOBSO.00249600
249700     MOVE GAF-COND-KIDNEY-DISEASE-BIT   (GAF-INDEX)  TO  CONDKDYO.00249700
249800     MOVE GAF-COND-ACCIDENT-BIT         (GAF-INDEX)  TO  CONDACCO.00249800
249900     MOVE GAF-COND-PRE-EXIST-BIT        (GAF-INDEX)  TO  CONDPECO.00249900
250000     MOVE GAF-COND-NON-EMER-BIT         (GAF-INDEX)  TO  CONDNEMO.00250000
250100     MOVE GAF-COND-SUICIDE-BIT          (GAF-INDEX)  TO  CONDSUIO.00250100
250200     MOVE GAF-COND-TMJ-BIT              (GAF-INDEX)  TO  CONDTMJO.00250200
250300     MOVE GAF-COND-INF-BIT              (GAF-INDEX)  TO  CONDINFO.00250300
250400     MOVE GAF-COND-LIFE-THREAT-BIT      (GAF-INDEX)  TO  CONDLIFO.00250400
250500     MOVE GAF-COND-EMER-MED-BIT         (GAF-INDEX)  TO  CONDEMCO.00250500
250600     MOVE GAF-COND-EMER-ACC-BIT         (GAF-INDEX)  TO  CONDEACO.00250600
250700     MOVE GAF-COND-SER-MEN-ILL-BIT      (GAF-INDEX)  TO  CONDSMIO.00250700
250800     MOVE GAF-COND-NON-SER-MEN-ILL-BIT  (GAF-INDEX)  TO  CONDNSMO.00250800
250900                                                                  00250900
251000     MOVE -1  TO PERIODL.                                         00251000
251100                                                                  00251100
251200     MOVE GAF-COPAY-FYI-VALUE (GAF-INDEX) TO  FYIVALO.            00251200
251300     SET  CURNT-OCURS-BIN                TO GAF-INDEX.            00251300
251400     MOVE CURNT-OCURS-BIN                TO CURNT-OCURS-PKD.      00251400
251500     MOVE CURNT-OCCURS-OUT               TO COCURANO.             00251500
251600                                                                  00251600
251700     IF GAF-ENTRY-COUNT  >  1                                     00251700
251800     THEN                                                         00251800
251900         COMPUTE  TOTAL-OCURS-UNK  =  GAF-ENTRY-COUNT  -  1       00251900
252000         MOVE  TOTAL-OCCURS-OUT  TO  TOCURANO                     00252000
252100     ELSE                                                         00252100
252200         MOVE  '01'              TO  TOCURANO.                    00252200
252300                                                                  00252300
252400     MOVE ZEROS   TO  IBGRSLTO,  IPGNSLTO,  IPGTSLTO              00252400
252500                      IDGDSLTO,  IPGPSLTO,  IPGSSLTO.             00252500
252600                                                                  00252600
252700     SET GAF-INT-INDEX TO      1.                                 00252700
252800     SET GAF-INT-INDEX DOWN BY 1.                                 00252800
252900                                                                  00252900
253000 4400-300-DISPLAY-LOOP.                                           00253000
253100                                                                  00253100
253200     SET GAF-INT-INDEX  UP BY  1.                                 00253200
253300     IF  GAF-INT-INDEX  >  5                                      00253300
253400         GO TO 4400-800-SEND.                                     00253400
253500                                                                  00253500
253600     IF GAF-INT-ID(GAF-INDEX GAF-INT-INDEX)  =  HIGH-VALUES       00253600
253700         GO TO 4400-800-SEND.                                     00253700
253800                                                                  00253800
253900     IF  GAF-INT-ID (GAF-INDEX GAF-INT-INDEX)       = '#IBGR '    00253900
254000         MOVE GAF-INT-SLOT (GAF-INDEX GAF-INT-INDEX)              00254000
254100                                TO ACWA-DISPLAY-LEN-7             00254100
254200         MOVE ACWA-DISPLAY-LEN-7 TO IBGRSLTO                      00254200
254300         GO TO 4400-300-DISPLAY-LOOP.                             00254300
254400                                                                  00254400
254500     IF  GAF-INT-ID (GAF-INDEX GAF-INT-INDEX)       = '#IDGD '    00254500
254600         MOVE GAF-INT-SLOT (GAF-INDEX GAF-INT-INDEX)              00254600
254700                                TO ACWA-DISPLAY-LEN-7             00254700
254800         MOVE ACWA-DISPLAY-LEN-7 TO IDGDSLTO                      00254800
254900         GO TO 4400-300-DISPLAY-LOOP.                             00254900
255000                                                                  00255000
255100     IF  GAF-INT-ID (GAF-INDEX GAF-INT-INDEX)       = '#IPGN '    00255100
255200         MOVE GAF-INT-SLOT (GAF-INDEX GAF-INT-INDEX)              00255200
255300                                TO ACWA-DISPLAY-LEN-7             00255300
255400         MOVE ACWA-DISPLAY-LEN-7 TO IPGNSLTO                      00255400
255500         GO TO 4400-300-DISPLAY-LOOP.                             00255500
255600                                                                  00255600
255700     IF  GAF-INT-ID (GAF-INDEX GAF-INT-INDEX)       = '#IPGP '    00255700
255800         MOVE GAF-INT-SLOT (GAF-INDEX GAF-INT-INDEX)              00255800
255900                                TO ACWA-DISPLAY-LEN-7             00255900
256000         MOVE ACWA-DISPLAY-LEN-7 TO IPGPSLTO                      00256000
256100         GO TO 4400-300-DISPLAY-LOOP.                             00256100
256200                                                                  00256200
256300     IF  GAF-INT-ID (GAF-INDEX GAF-INT-INDEX)       = '#IPGT '    00256300
256400         MOVE GAF-INT-SLOT (GAF-INDEX GAF-INT-INDEX)              00256400
256500                                TO ACWA-DISPLAY-LEN-7             00256500
256600         MOVE ACWA-DISPLAY-LEN-7 TO IPGTSLTO                      00256600
256700         GO TO 4400-300-DISPLAY-LOOP.                             00256700
256800                                                                  00256800
256900     IF  GAF-INT-ID (GAF-INDEX GAF-INT-INDEX)       = '#IPGS '    00256900
257000         MOVE GAF-INT-SLOT (GAF-INDEX GAF-INT-INDEX)              00257000
257100                                TO ACWA-DISPLAY-LEN-7             00257100
257200         MOVE ACWA-DISPLAY-LEN-7 TO IPGSSLTO                      00257200
257300         GO TO 4400-300-DISPLAY-LOOP.                             00257300
257400                                                                  00257400
257500     MOVE WS-ABCODE-1PF3        TO WS-ABCODE                      00257500
257600     MOVE WS-ABCODE-1PF3-MSG    TO WS-ABCODE-MSG                  00257600
257700     PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                       00257700
257800                                                                  00257800
257900                                                                  00257900
258000 4400-800-SEND.                                                   00258000
258100                                                                  00258100
258200     PERFORM 4500-000-PROTECT-CRIT-DATA-ELE.                      00258200
258300                                                                  00258300
258400*-------RESET ATTR. 'CAUSE INTDESK & IDPROD CHANGED IN 4500- CALL 00258400
258500     PERFORM 7900-000-RESET-ATTRIBUTES.                           00258500
258600                                                                  00258600
258700     PERFORM 9000-000-SEND-ERASE-RETURN.                          00258700
258800                                                                  00258800
258900 4400-900-EXIT. EXIT.                                             00258900
259000                                                                  00259000
259100/*****************************************************************00259100
259200*  4500  -  PROTECT CRITICAL DATA ELEMENTS                       *00259200
259300*                                                                *00259300
259400*        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *00259400
259500*          1. DETERMINE IF GROUP IS CRITICAL (CALL GCTRSRT).     *00259500
259600*          2. IF GROUP IS CRITICAL:                              *00259600
259700*              - READ PRODUCTION CONTRACT, GROUP SPECIFIC, OR    *00259700
259800*                BENEFIT PROVISION.                              *00259800
259900*                - IF ON DATA BASE:                              *00259900
260000*                  - SCAN FOR #ACP TABULAR                       *00260000
260100*                    - IF TABULAR PRESENT AND ACTIVE, TABULAR IS *00260100
260200*                      CRITICAL, PROTECT CRITICAL DATA ELEMENTS  *00260200
260300*                      ON SCREEN AND ISSUE MESSAGE.              *00260300
260400******************************************************************00260400
260500 4500-000-PROTECT-CRIT-DATA-ELE SECTION.                          00260500
260600 4500-010.                                                        00260600
260700                                                                  00260700
260800     IF DELADDI  =  'CHG/DEL'   OR                                00260800
260900        DELOLITI =  SPACES                                        00260900
261000        NEXT SENTENCE                                             00261000
261100     ELSE                                                         00261100
261200        GO TO 4500-900-EXIT.                                      00261200
261300                                                                  00261300
261400     MOVE WS-REQUEST-4500-CDE-PROTECT  TO  ACWA-CDE-REQUEST-CODE. 00261400
261500     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00261500
261600                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00261600
261700                                                                  00261700
261800     EXEC CICS  LINK   PROGRAM('GACDEPGM')                        00261800
261900                COMMAREA (COMMON-WORKAREAS)                       00261900
262000                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00262000
262100                                                                  00262100
262200     GO TO 4500-900-EXIT.                                         00262200
262300                                                                  00262300
262400 4500-900-EXIT. EXIT.                                             00262400
262500                                                                  00262500
262600/*****************************************************************00262600
262700*  4600  -  UPDATE CRITICAL DATA ELEMENT STATUS                  *00262700
262800*                                                                *00262800
262900*        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *00262900
263000*           1. READ ALL LEVEL TABULAR FROM PROVISION POOL        *00263000
263100*           2. COMPARE CDE ELEMENTS ON W/F ALL LVL TAB TO THOSE  *00263100
263200*               ON THE PROVISION POOL ALL LVL TABULAR RECORD.    *00263200
263300*           3. IF CDE ELEMENTS ON W/F ALL LVL TAB HAVE BEEN      *00263300
263400*               CHANGED, ISSUE MESSAGE AND POSITION CURSOR ON    *00263400
263500*               +CDE+ INDICATOR (POSITION=8).                    *00263500
263600*              IF MESSAGE HAS BEEN ISSUED AND OPERATOR HAS HIT   *00263600
263700*               ENTER, CONTINUE PROCESSING.                      *00263700
263800******************************************************************00263800
263900 4600-000-UPDATE-CDE-STATUS     SECTION.                          00263900
264000 4600-010.                                                        00264000
264100                                                                  00264100
264200     IF CDEINDO = ('+CDE+' OR '+CDE-') AND                        00264200
264300        (DELADDI = 'CHG/DEL' OR                                   00264300
264400        (DELADDI = 'CHG/ADD' AND                                  00264400
264500        WRK-SIGNAL-FROM-ONLINE  =  'W'))                          00264500
264600        NEXT SENTENCE                                             00264600
264700     ELSE                                                         00264700
264800        IF CDEINDO = ('+CDE+' OR '+CDE-') AND                     00264800
264900           (DELADDI = 'CHG/ADD')                                  00264900
265000           NEXT SENTENCE                                          00265000
265100        ELSE                                                      00265100
265200            GO TO 4600-900-EXIT.                                  00265200
265300                                                                  00265300
265400                                                                  00265400
265500     MOVE WS-ALT-WORKFILE-KEYS       TO ACWA-ALT-WORKFILE-KEYS.   00265500
265600     SET  ACWA-INDEX-1               TO GAF-INDEX.                00265600
265700     MOVE WS-REQUEST-4600-CDE-STATUS TO ACWA-CDE-REQUEST-CODE.    00265700
265800     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00265800
265900                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00265900
266000                                                                  00266000
266100     EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                      00266100
266200                COMMAREA (COMMON-WORKAREAS)                       00266200
266300                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00266300
266400                                                                  00266400
266500     IF  ACWA-CDE-RETURN-DONT-SEND                                00266500
266600         EXEC CICS  RETURN TRANSID('GA1P')                        00266600
266700                    COMMAREA(DFHCOMMAREA)                         00266700
266800                    LENGTH  (EIBCALEN)                            00266800
266900                    END-EXEC.                                     00266900
267000                                                                  00267000
267100 4600-900-EXIT. EXIT.                                             00267100
267200                                                                  00267200
267300/*****************************************************************00267300
267400*  4700  -  UPDATE W/F CONTROL RECORD                            *00267400
267500*                                                                *00267500
267600*        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *00267600
267700*          1. READ W/F CONTROL RECORD, UPDATE CDE RECORD COUNTS  *00267700
267800*             WITH THE ACTION TAKEN ON THE ALL LEVEL TABULAR     *00267800
267900*             RECORD IF ITS CDE STATUS CHANGED.                  *00267900
268000*          2. REWRITE W/F CONTROL RECORD                         *00268000
268100******************************************************************00268100
268200 4700-000-UPDATE-CONTROL-RECORD SECTION.                          00268200
268300 4700-010.                                                        00268300
268400                                                                  00268400
268500     MOVE WS-ALT-WORKFILE-KEYS        TO ACWA-ALT-WORKFILE-KEYS.  00268500
268600     SET  ACWA-INDEX-1                TO GAF-INDEX.               00268600
268700     MOVE WS-REQUEST-4700-CNTL-UPDATE TO ACWA-CDE-REQUEST-CODE.   00268700
268800     MOVE COMMAREA-ALL-LEV-TAB-RECORD                             00268800
268900                            TO COMMAREA2-ALL-LEV-TAB-RECORD.      00268900
269000                                                                  00269000
269100     EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                      00269100
269200                COMMAREA (COMMON-WORKAREAS)                       00269200
269300                LENGTH (LENGTH OF COMMON-WORKAREAS)  END-EXEC.    00269300
269400                                                                  00269400
269500 4700-900-EXIT. EXIT.                                             00269500
269600                                                                  00269600
269700/*****************************************************************00269700
269800* 5000  XCTL TO PREVIOUS MENU                                    *00269800
269900*                                                                *00269900
270000*   THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM      *00270000
270100*  ORIGINATED FROM.  WE READ THE ALL LEVEL TABULAR RECORD AND    *00270100
270200*  INSURE THAT THE TABLE OF OCCURRENCES IS SORTED AND THAT ANY   *00270200
270300*  DUPLICATES ARE DROPPED FROM THE LIST.  WE THEN REWRITE THE    *00270300
270400*  ALL LEVEL TABULAR AND READ THE PARTICULAR RECORD THAT THE     *00270400
270500*  MENU WHICH PASSED US CONTROL WOULD REQUIRE.  FINALLY BASED    *00270500
270600*  ON THE PREVIOUS MENU FIELD CARRIED THROUGHOUT THIS PART OF    *00270600
270700*  THE SYSTEM WE RETURN TO THE PREVIOUS MENU.                    *00270700
270800******************************************************************00270800
270900 5000-000-XCTL-TO-PREVIOUS-MENU SECTION.                          00270900
271000 5000-010.                                                        00271000
271100                                                                  00271100
271200     PERFORM 3200-000-READ-REC-FOR-UPDATE.                        00271200
271300                                                                  00271300
271400     IF NOT GCIO-GOOD-RETURN                                      00271400
271500        MOVE WS-ABCODE-1PFK        TO WS-ABCODE                   00271500
271600        MOVE WS-ABCODE-1PFK-MSG    TO WS-ABCODE-MSG               00271600
271700        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00271700
271800                                                                  00271800
271900     PERFORM 6500-000-SORT-COMPRESS-ALL-LVL.                      00271900
272000     PERFORM 4600-000-UPDATE-CDE-STATUS.                          00272000
272100     PERFORM 3000-000-UPDATE-GAF-RECORD.                          00272100
272200                                                                  00272200
272300     IF ACWA-CDE-FIELD-CHANGED OR  ACWA-CDE-REC-CHANGED           00272300
272400        IF EIBCPOSN = 8                                           00272400
272500           NEXT SENTENCE                                          00272500
272600        ELSE                                                      00272600
272700           EXEC CICS  RETURN TRANSID('GA1P')                      00272700
272800                      COMMAREA(DFHCOMMAREA)                       00272800
272900                      LENGTH  (EIBCALEN)                          00272900
273000                      END-EXEC.                                   00273000
273100                                                                  00273100
273200     IF FRMNUIDI  =  'GS3A'                                       00273200
273300        PERFORM 5100-000-RETURN-TO-GRP-SPEC                       00273300
273400        EXEC CICS  XCTL  PROGRAM ('GS3APGM')                      00273400
273500                   COMMAREA(WORK-RECORD-3)                        00273500
273600                   LENGTH (WS-WRK-GRP-SPEC-LEN)   END-EXEC.       00273600
273700                                                                  00273700
273800     IF  FRMNUIDI  =  'GC4A'                                      00273800
273900        PERFORM 5200-000-RETURN-TO-CONTRACT                       00273900
274000        EXEC CICS  XCTL  PROGRAM ('GC4APGM')                      00274000
274100                   COMMAREA(WORK-RECORD-4)                        00274100
274200                   LENGTH (WS-WRK-CONTRACT-LEN)   END-EXEC.       00274200
274300                                                                  00274300
274400     IF FRMNUIDI  =  'GC8A'                                       00274400
274500        PERFORM 5300-000-RETURN-TO-BEN-PROV                       00274500
274600        EXEC CICS  XCTL  PROGRAM ('GC8APGM')                      00274600
274700                   COMMAREA(WORK-RECORD-5)                        00274700
274800                   LENGTH (WS-WRK-BEN-PROV-LEN)   END-EXEC.       00274800
274900                                                                  00274900
275000*******                                                           00275000
275100* STS *===> RETURN TO SINGLE TABULAR SUPPORT MENU, NO COMMAREA    00275100
275200*******                                                          |00275200
275300     IF  FRMNUIDI  =  'GTM1'                                      00275300
275400         EXEC CICS  XCTL  PROGRAM('GTM1PGM')   END-EXEC.          00275400
275500*******                                                          |00275500
275600* STS *----------------------------------------------------------*00275600
275700*******                                                           00275700
275800                                                                  00275800
275900 5000-900-EXIT. EXIT.                                             00275900
276000                                                                  00276000
276100/*****************************************************************00276100
276200* 5100  RETURN TO GRP SPEC                                       *00276200
276300*                                                                *00276300
276400*    THESE ROUTINES WILL BUILD THE IO PROGRAMS PARAMTER LIST     *00276400
276500*  AND THEN READ THE RECORD NEEDED BY THE PREVIOUS MENU.         *00276500
276600******************************************************************00276600
276700 5100-000-RETURN-TO-GRP-SPEC    SECTION.                          00276700
276800 5100-010.                                                        00276800
276900                                                                  00276900
277000        EXEC CICS GETMAIN                                         00277000
277100               SET(ADDRESS OF WF-IO-PARM-WRK-GRP-SPEC-REC)        00277100
277200               INITIMG(WS-HEX-00)                                 00277200
277300               LENGTH(WS-IO-PARM-WRK-GRP-SPEC-LEN)                00277300
277400               END-EXEC.                                          00277400
277500                                                                  00277500
277600        SET ACWA-WF-GRP-SPEC-PNTR     TO                          00277600
277700                 ADDRESS OF WF-IO-PARM-WRK-GRP-SPEC-REC.          00277700
277800                                                                  00277800
277900     MOVE SPACES               TO GCIO-WORKFILE-KEY.              00277900
278000     MOVE 'G'                  TO GCIO-WRK-STATUS-CODE.           00278000
278100     MOVE 'G2'                 TO GCIO-WRK-RECORD-TYPE.           00278100
278200     MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             00278200
278300     MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             00278300
278400     MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           00278400
278500     MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              00278500
278600     MOVE SPACES               TO GCIO-WRK-LINE-OF-BUS            00278600
278700                                  GCIO-WRK-PROVISION-ID           00278700
278800                                  GCIO-WRK-PROVIDER-CONTROL       00278800
278900                                  GCIO-WRK-TAB-PROVISION-ID.      00278900
279000     MOVE ZEROS                TO GCIO-WRK-PROVISION-SLOT-NO,     00279000
279100                                  GCIO-WRK-TAB-PROV-SLOT-NO.      00279100
279200     MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   00279200
279300     MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             00279300
279400                                                                  00279400
279500     MOVE GC-GCPSWORK-DDNAME     TO GCIO3-FILE-DDNAME.            00279500
279600     MOVE GCIO-WORKFILE-KEY      TO GCIO3-FILE-KEY.               00279600
279700     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO3-FILE-ACCESS-CODE.       00279700
279800     MOVE GC-GCIO-AREA-1         TO GCIO3-IO-AREA-TO-USE.         00279800
279900     MOVE GC-GCGRPSPC-VARY-MAX-OCUR  TO                           00279900
280000                      GCG-COUNT-TAB-PROVN-POINTERS.               00280000
280100                                                                  00280100
280200     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00280200
280300                COMMAREA(WF-IO-PARM-WRK-GRP-SPEC-REC)             00280300
280400                LENGTH (WS-IO-PARM-WRK-GRP-SPEC-LEN)  END-EXEC.   00280400
280500                                                                  00280500
280600     IF NOT GCIO3-GOOD-RETURN                                     00280600
280700        MOVE WS-ABCODE-1PFL        TO WS-ABCODE                   00280700
280800        MOVE WS-ABCODE-1PFL-MSG    TO WS-ABCODE-MSG               00280800
280900        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00280900
281000                                                                  00281000
281100 5100-900-EXIT. EXIT.                                             00281100
281200                                                                  00281200
281300/*****************************************************************00281300
281400* 5200  RETURN TO CONTRACT                                       *00281400
281500*                                                                *00281500
281600*    THESE ROUTINES WILL BUILD THE IO PROGRAMS PARAMTER LIST     *00281600
281700*  AND THEN READ THE RECORD NEEDED BY THE PREVIOUS MENU.         *00281700
281800******************************************************************00281800
281900 5200-000-RETURN-TO-CONTRACT    SECTION.                          00281900
282000 5200-010.                                                        00282000
282100                                                                  00282100
282200        EXEC CICS GETMAIN                                         00282200
282300               SET(ADDRESS OF WF-IO-PARM-WRK-CONTRACT-REC)        00282300
282400               INITIMG(WS-HEX-00)                                 00282400
282500               LENGTH(WS-IO-PARM-WRK-CONTRACT-LEN)                00282500
282600               END-EXEC.                                          00282600
282700                                                                  00282700
282800        SET ACWA-WF-CONTRACT-PNTR     TO                          00282800
282900                 ADDRESS OF WF-IO-PARM-WRK-CONTRACT-REC.          00282900
283000                                                                  00283000
283100     MOVE SPACES               TO GCIO-WORKFILE-KEY.              00283100
283200     MOVE 'C'                  TO GCIO-WRK-STATUS-CODE.           00283200
283300     MOVE 'C2'                 TO GCIO-WRK-RECORD-TYPE.           00283300
283400     MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             00283400
283500     MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             00283500
283600     MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           00283600
283700     MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              00283700
283800     MOVE GCA-L-O-B            TO GCIO-WRK-LINE-OF-BUS.           00283800
283900     MOVE GCA-PROV-CTL         TO GCIO-WRK-PROVIDER-CONTROL.      00283900
284000     MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   00284000
284100     MOVE SPACES               TO GCIO-WRK-PROVISION-ID           00284100
284200                                  GCIO-WRK-TAB-PROVISION-ID.      00284200
284300     MOVE ZEROS                TO GCIO-WRK-PROVISION-SLOT-NO      00284300
284400                                  GCIO-WRK-TAB-PROV-SLOT-NO.      00284400
284500                                                                  00284500
284600     MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             00284600
284700     MOVE GC-GCPSWORK-DDNAME     TO GCIO4-FILE-DDNAME.            00284700
284800     MOVE GCIO-WORKFILE-KEY      TO GCIO4-FILE-KEY.               00284800
284900     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO4-FILE-ACCESS-CODE.       00284900
285000     MOVE GC-GCIO-AREA-1         TO GCIO4-IO-AREA-TO-USE.         00285000
285100     MOVE GC-GCCONTR-VARY-MAX-OCUR  TO                            00285100
285200                      GCT-COUNT-BEN-PROVN-POINTERS.               00285200
285300                                                                  00285300
285400     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00285400
285500                COMMAREA(WF-IO-PARM-WRK-CONTRACT-REC)             00285500
285600                LENGTH (WS-IO-PARM-WRK-CONTRACT-LEN)   END-EXEC.  00285600
285700                                                                  00285700
285800     IF NOT GCIO4-GOOD-RETURN                                     00285800
285900        MOVE WS-ABCODE-1PFM        TO WS-ABCODE                   00285900
286000        MOVE WS-ABCODE-1PFM-MSG    TO WS-ABCODE-MSG               00286000
286100        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00286100
286200                                                                  00286200
286300 5200-900-EXIT. EXIT.                                             00286300
286400                                                                  00286400
286500/*****************************************************************00286500
286600* 5300  RETURN TO BEN PROV                                       *00286600
286700*                                                                *00286700
286800*    THESE ROUTINES WILL BUILD THE IO PROGRAMS PARAMTER LIST     *00286800
286900*  AND THEN READ THE RECORD NEEDED BY THE PREVIOUS MENU.         *00286900
287000******************************************************************00287000
287100 5300-000-RETURN-TO-BEN-PROV    SECTION.                          00287100
287200 5300-010.                                                        00287200
287300                                                                  00287300
287400        EXEC CICS GETMAIN                                         00287400
287500               SET(ADDRESS OF WF-IO-PARM-WRK-BEN-PROV-REC)        00287500
287600               INITIMG(WS-HEX-00)                                 00287600
287700               LENGTH(WS-IO-PARM-WRK-BEN-PROV-LEN)                00287700
287800               END-EXEC.                                          00287800
287900                                                                  00287900
288000        SET ACWA-WF-BEN-PROV-PNTR     TO                          00288000
288100                 ADDRESS OF WF-IO-PARM-WRK-BEN-PROV-REC.          00288100
288200                                                                  00288200
288300     MOVE SPACES                TO GCIO-WORKFILE-KEY.             00288300
288400     MOVE 'C'                   TO GCIO-WRK-STATUS-CODE.          00288400
288500     MOVE 'C4'                  TO GCIO-WRK-RECORD-TYPE.          00288500
288600     MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             00288600
288700     MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             00288700
288800     MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           00288800
288900     MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              00288900
289000     MOVE GCA-L-O-B            TO GCIO-WRK-LINE-OF-BUS.           00289000
289100     MOVE GCA-PROV-CTL         TO GCIO-WRK-PROVIDER-CONTROL.      00289100
289200     MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   00289200
289300     MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN.            00289300
289400     MOVE GCA-BEN-PROV-ID       TO GCIO-WRK-PROVISION-ID.         00289400
289500     MOVE SPACES                TO GCIO-WRK-TAB-PROVISION-ID.     00289500
289600     MOVE 9999999               TO GCIO-WRK-PROVISION-SLOT-NO.    00289600
289700     MOVE ZEROS                 TO GCIO-WRK-TAB-PROV-SLOT-NO.     00289700
289800                                                                  00289800
289900     MOVE GC-GCPSWORK-DDNAME     TO GCIO5-FILE-DDNAME.            00289900
290000     MOVE GCIO-WORKFILE-KEY      TO GCIO5-FILE-KEY.               00290000
290100     MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO5-FILE-ACCESS-CODE.       00290100
290200     MOVE GC-GCIO-AREA-1         TO GCIO5-IO-AREA-TO-USE.         00290200
290300     MOVE GC-GCBENPRV-VARY-MAX-OCUR  TO                           00290300
290400                      GCP-COUNT-TAB-PROVN-POINTERS.               00290400
290500                                                                  00290500
290600     EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         00290600
290700                COMMAREA(WF-IO-PARM-WRK-BEN-PROV-REC)             00290700
290800                LENGTH (WS-IO-PARM-WRK-BEN-PROV-LEN)  END-EXEC.   00290800
290900                                                                  00290900
291000     IF NOT GCIO5-GOOD-RETURN                                     00291000
291100        MOVE WS-ABCODE-1PFN        TO WS-ABCODE                   00291100
291200        MOVE WS-ABCODE-1PFN-MSG    TO WS-ABCODE-MSG               00291200
291300        PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    00291300
291400                                                                  00291400
291500 5300-900-EXIT. EXIT.                                             00291500
291600                                                                  00291600
291700/*****************************************************************00291700
291800* 6000  BUILD GROUP SPEC KEY                                     *00291800
291900*                                                                *00291900
292000*    BUILD THE GROUP SPECIFIC KEY FOR WORKFILE READS             *00292000
292100******************************************************************00292100
292200 6000-000-BUILD-GROUP-SPEC-KEY  SECTION.                          00292200
292300 6000-010.                                                        00292300
292400                                                                  00292400
292500     MOVE SPACES                TO GCIO-WORKFILE-KEY.             00292500
292600     MOVE  'G'                  TO GCIO-WRK-STATUS-CODE.          00292600
292700     MOVE  'G3'                 TO GCIO-WRK-RECORD-TYPE.          00292700
292800     MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE.            00292800
292900     MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM.            00292900
293000     MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM.          00293000
293100     MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE.             00293100
293200     MOVE SPACES                TO GCIO-WRK-LINE-OF-BUS,          00293200
293300                                   GCIO-WRK-PROVIDER-CONTROL.     00293300
293400     MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVL.  00293400
293500     MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN.            00293500
293600     MOVE TABIDI                TO GCIO-WRK-PROVISION-ID.         00293600
293700     MOVE TABSLTNI              TO ACWA-DISPLAY-LEN-7.            00293700
293800     MOVE ACWA-DISPLAY-LEN-7    TO GCIO-WRK-PROVISION-SLOT-NO.    00293800
293900     MOVE SPACES                TO GCIO-WRK-TAB-PROVISION-ID.     00293900
294000     MOVE ZEROS                 TO GCIO-WRK-TAB-PROV-SLOT-NO.     00294000
294100                                                                  00294100
294200 6000-900-EXIT. EXIT.                                             00294200
294300                                                                  00294300
294400******************************************************************00294400
294500* 6100  BUILD CONTRACT KEY                                       *00294500
294600*                                                                *00294600
294700*    BUILD THE CONTRACT KEY FOR WORKFILE READS                   *00294700
294800******************************************************************00294800
294900 6100-000-BUILD-CONTRACT-KEY    SECTION.                          00294900
295000 6100-010.                                                        00295000
295100                                                                  00295100
295200     MOVE SPACES               TO GCIO-WORKFILE-KEY.              00295200
295300     MOVE  'C'                 TO GCIO-WRK-STATUS-CODE.           00295300
295400     MOVE  'C3'                TO GCIO-WRK-RECORD-TYPE.           00295400
295500     MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             00295500
295600     MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             00295600
295700     MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           00295700
295800     MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              00295800
295900     MOVE GCA-L-O-B            TO GCIO-WRK-LINE-OF-BUS.           00295900
296000     MOVE GCA-PROV-CTL         TO GCIO-WRK-PROVIDER-CONTROL.      00296000
296100     MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   00296100
296200     MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             00296200
296300     MOVE TABIDI               TO GCIO-WRK-PROVISION-ID.          00296300
296400     MOVE TABSLTNI             TO ACWA-DISPLAY-LEN-7.             00296400
296500     MOVE ACWA-DISPLAY-LEN-7   TO GCIO-WRK-PROVISION-SLOT-NO.     00296500
296600     MOVE SPACES               TO GCIO-WRK-TAB-PROVISION-ID.      00296600
296700     MOVE ZEROS                TO GCIO-WRK-TAB-PROV-SLOT-NO.      00296700
296800                                                                  00296800
296900 6100-900-EXIT. EXIT.                                             00296900
297000                                                                  00297000
297100/*****************************************************************00297100
297200* 6200  BUILD BEN PROV KEY                                       *00297200
297300*                                                                *00297300
297400*    BUILD THE BEN PROV KEY FOR WORKFILE READS                   *00297400
297500******************************************************************00297500
297600 6200-000-BUILD-BEN-PROV-KEY    SECTION.                          00297600
297700 6200-010.                                                        00297700
297800                                                                  00297800
297900     MOVE SPACES                TO GCIO-WORKFILE-KEY.             00297900
298000     MOVE  'C'                  TO GCIO-WRK-STATUS-CODE.          00298000
298100     MOVE  'C5'                 TO GCIO-WRK-RECORD-TYPE.          00298100
298200     MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE.            00298200
298300     MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM.            00298300
298400     MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM.          00298400
298500     MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE.             00298500
298600     MOVE GCA-L-O-B             TO GCIO-WRK-LINE-OF-BUS.          00298600
298700     MOVE GCA-PROV-CTL          TO GCIO-WRK-PROVIDER-CONTROL.     00298700
298800     MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVL.  00298800
298900     MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN.            00298900
299000     MOVE GCA-BEN-PROV-ID       TO GCIO-WRK-PROVISION-ID.         00299000
299100     MOVE +9999999              TO GCIO-WRK-PROVISION-SLOT-NO.    00299100
299200     MOVE TABIDI                TO GCIO-WRK-TAB-PROVISION-ID.     00299200
299300     MOVE TABSLTNI              TO ACWA-DISPLAY-LEN-7.            00299300
299400     MOVE ACWA-DISPLAY-LEN-7    TO GCIO-WRK-TAB-PROV-SLOT-NO.     00299400
299500                                                                  00299500
299600 6200-900-EXIT. EXIT.                                             00299600
299700                                                                  00299700
299800/*****************************************************************00299800
299900*  XCTL TO MAIN MENU                                             *00299900
300000*                                                                *00300000
300100*    THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO   *00300100
300200*  OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE*00300200
300300*  XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE MENUS00300300
300400*  TO GET TO US; WE ARE A MODULE AT THE BOTTOM OF A PYRAMID TO GET00300400
300500*  HERE YOU MUST START AT THE TOP (THE MAIN MENU).               *00300500
300600******************************************************************00300600
300700 6400-000-XCTL-TO-MAIN-MENU     SECTION.                          00300700
300800 6400-010.                                                        00300800
300900                                                                  00300900
301000     MOVE WS-ABCODE-1PP1        TO WS-ABCODE.                     00301000
301100     MOVE WS-ABCODE-1PP1-MSG    TO WS-ABCODE-MSG.                 00301100
301200                                                                  00301200
301300     EXEC CICS  XCTL  PROGRAM('GCPSPGM')   END-EXEC.              00301300
301400                                                                  00301400
301500 6400-900-EXIT. EXIT.                                             00301500
301600                                                                  00301600
301700/*****************************************************************00301700
301800* 6500  SORT COMPRESS ALL LVL                                    *00301800
301900*                                                                *00301900
302000*    THIS ROUTINE WILL COPY ALL ENTRIES FROM THE TABULAR PORTION *00302000
302100*  TO A COPY OF THE TABULAR, THEN SORT THE COPY INTO ASCENDING   *00302100
302200*  SEQUENCE, ANY DUPLICATES ARE REMOVED FROM THE TABLE.          *00302200
302300******************************************************************00302300
302400 6500-000-SORT-COMPRESS-ALL-LVL SECTION.                          00302400
302500 6500-010.                                                        00302500
302600                                                                  00302600
302700        EXEC CICS GETMAIN                                         00302700
302800               SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)            00302800
302900               INITIMG(WS-HEX-00)                                 00302900
303000               LENGTH(WS-COPY-TABLE-LEN)                          00303000
303100               END-EXEC.                                          00303100
303200                                                                  00303200
303300        SET ACWA-COPY-TAB-PNTR        TO                          00303300
303400                 ADDRESS OF COPY-TABULAR-TABLE-AREA.              00303400
303500                                                                  00303500
303600     MOVE GAF-ENTRY-COUNT  TO  GAF-ENTRY-COUNT.                   00303600
303700     SET GAF-INDEX,  COPY-IDX  TO  1.                             00303700
303800                                                                  00303800
303900 6500-100-COPY-TABLE.                                             00303900
304000                                                                  00304000
304100     IF GAF-INDEX  NOT >  GAF-ENTRY-COUNT                         00304100
304200        MOVE GAF-ENTRY(GAF-INDEX)  TO COPY-TABULAR-TABLE(COPY-IDX)00304200
304300        SET GAF-INDEX,  COPY-IDX  UP BY  1                        00304300
304400        GO TO 6500-100-COPY-TABLE.                                00304400
304500                                                                  00304500
304600     SET  COPY-IDX  TO  1.                                        00304600
304700     SET  COPY-IDX2 TO  2.                                        00304700
304800                                                                  00304800
304900 6500-200-SORT-TABLE.                                             00304900
305000                                                                  00305000
305100     IF COPY-IDX2  >  GAF-ENTRY-COUNT                             00305100
305200        GO TO 6500-400-ARE-WE-DONE-SORTING.                       00305200
305300                                                                  00305300
305400     IF  COPY-SORTABLE-FLDS (COPY-IDX)  >                         00305400
305500                                   COPY-SORTABLE-FLDS (COPY-IDX2) 00305500
305600     THEN                                                         00305600
305700         MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO  WS-ENTRY         00305700
305800         MOVE COPY-TABULAR-TABLE (COPY-IDX2)                      00305800
305900                               TO  COPY-TABULAR-TABLE (COPY-IDX)  00305900
306000         MOVE WS-ENTRY  TO  COPY-TABULAR-TABLE (COPY-IDX2)        00306000
306100         SET COPY-IDX2  UP BY  1                                  00306100
306200         GO TO 6500-200-SORT-TABLE.                               00306200
306300                                                                  00306300
306400     IF COPY-SORTABLE-FLDS (COPY-IDX)  <                          00306400
306500                                    COPY-SORTABLE-FLDS(COPY-IDX2) 00306500
306600        SET COPY-IDX2  UP BY  1                                   00306600
306700        GO TO 6500-200-SORT-TABLE.                                00306700
306800                                                                  00306800
306900     SET COPY-IDX3,  COPY-IDX4  TO  COPY-IDX2.                    00306900
307000     SET COPY-IDX4  UP BY 1.                                      00307000
307100                                                                  00307100
307200 6500-300-ELIMINATE-DUPLICATES.                                   00307200
307300                                                                  00307300
307400     IF COPY-IDX4  NOT >  GAF-ENTRY-COUNT                         00307400
307500        MOVE COPY-TABULAR-TABLE (COPY-IDX4)  TO                   00307500
307600                                 COPY-TABULAR-TABLE (COPY-IDX3)   00307600
307700        SET COPY-IDX3,  COPY-IDX4  UP BY  1                       00307700
307800        GO TO 6500-300-ELIMINATE-DUPLICATES.                      00307800
307900                                                                  00307900
308000     SUBTRACT 1  FROM  GAF-ENTRY-COUNT.                           00308000
308100     GO TO 6500-200-SORT-TABLE.                                   00308100
308200                                                                  00308200
308300 6500-400-ARE-WE-DONE-SORTING.                                    00308300
308400                                                                  00308400
308500     IF COPY-IDX  <  GAF-ENTRY-COUNT                              00308500
308600        SET COPY-IDX   UP BY  1                                   00308600
308700        SET COPY-IDX2  TO COPY-IDX                                00308700
308800        SET COPY-IDX2  UP BY 1                                    00308800
308900        GO TO 6500-200-SORT-TABLE.                                00308900
309000                                                                  00309000
309100     MOVE GAF-ENTRY-COUNT  TO  GAF-ENTRY-COUNT.                   00309100
309200     SET GAF-INDEX,  COPY-IDX  TO  1.                             00309200
309300                                                                  00309300
309400 6500-500-MOVE-COPY-BACK.                                         00309400
309500                                                                  00309500
309600     IF GAF-INDEX  NOT >  GAF-ENTRY-COUNT                         00309600
309700        MOVE COPY-TABULAR-TABLE(COPY-IDX)  TO                     00309700
309800                                           GAF-ENTRY(GAF-INDEX)   00309800
309900        SET GAF-INDEX,  COPY-IDX  UP BY  1                        00309900
310000        GO TO 6500-500-MOVE-COPY-BACK.                            00310000
310100                                                                  00310100
310200     MOVE GAF-ENTRY-COUNT  TO  GAF-ENTRY-COUNT.                   00310200
310300     IF GAF-ENTRY-COUNT  NOT <  GC-GCTABULR-ACP-VARY-MAX-OCUR     00310300
310400        MOVE 'Y'  TO  ACWA-ERROR-SW.                              00310400
310500                                                                  00310500
310600 6500-900-EXIT. EXIT.                                             00310600
310700                                                                  00310700
310800/*****************************************************************00310800
310900* 7900  RESET ATTRIBUTES                                         *00310900
311000******************************************************************00311000
311100 7900-000-RESET-ATTRIBUTES      SECTION.                          00311100
311200 7900-010.                                                        00311200
311300                                                                  00311300
311400     MOVE DFHBMUNF                                                00311400
311500       TO BENVLQLA  COPAYINA  CSTCONTA  FAMINDIA  LOBA CONDLIFA   00311500
311600          PERIODA   PLCTRMTA  SRVGRUPA  PRTIMEFA  DEFINTNA        00311600
311700          ASCDSCDA  INTRVALA  INTTYPEA  CLMLVLIA  BNMXVALA        00311700
311800          DAYFACIA  OVRDINDA  NEWVALUA  INTDESKA  FYIVALA         00311800
311900          CONDALLA  CONDEXCA  CONDICDA  CONDTABA  CONDMENA        00311900
312000          CONDEMCA  CONDEACA  CONDSMIA  CONDNSMA  BISNDINA        00312000
312100          CONDDRGA  CONDALCA  CONDOBNA  CONDOBCA  CONDMALA        00312100
312200          CONDCARA  CONDOBSA  CONDKDYA  CONDACCA  CONDPECA        00312200
312300          CONDNEMA CONDSUIA  MANAPLIA  CONDTMJA  CONDINFA AGEQLLA 00312300
312400          IBGROPTA IPGNOPTA  IPGTOPTA  MFRMSLTA  PERTQALA AGEQLHA 00312400
312500          IDGDOPTA  IPGPOPTA  AGELIMLA  AGELIMHA  RELPINDA        00312500
312600          FEAKINDA  TIMEDOLA  IPGSOPTA  ACCUMIDA  CAPINDA         00312600
312700          SABDINDA  BENTYPA   TIERCDA   TIERLVA.                  00312700
312800                                                                  00312800
312900                                                                  00312900
313000     IF  DELADDO  =  'CHG/DEL'                                    00313000
313100     THEN                                                         00313100
313200         NEXT SENTENCE                                            00313200
313300     ELSE                                                         00313300
313400         GO TO 7900-900-EXIT.                                     00313400
313500                                                                  00313500
313600                                                                  00313600
313700     IF  CDEINDO = '+CDE+'                                        00313700
313800     THEN                                                         00313800
313900*---------------- HIGH-LIGHT CRITICAL DATA ELEMENT LABELS         00313900
314000         MOVE DFHBMABF TO DLOPTLTA  PERIOTA  BENVLQTA  LOTA       00314000
314100              AGELIMA     PLCTRTTA  FAMINDTA SRVGRUTA  CSTCOTTA   00314100
314200              AGEQLTA     COPAYITA  INTDESTA CONDTG1A  CONDTG2A   00314200
314300              BISNDITA                                            00314300
314400         IF INTDESKO  NOT =  IDPRODO                              00314400
314500            MOVE DFHBMASB  TO  IBGRIDA,  IPGNIDA,  IPGTIDA        00314500
314600                               IDGDIDA,  IPGPIDA,  IPGSIDA        00314600
314700            MOVE DFHBMABF  TO  IBGRSLTA, IPGNSLTA, IPGTSLTA       00314700
314800                               IDGDSLTA, IPGPSLTA, IPGSSLTA       00314800
314900            MOVE DFHBMUBF  TO  IBGROPTA, IPGNOPTA, IPGTOPTA       00314900
315000                               IDGDOPTA, IPGPOPTA, IPGSOPTA       00315000
315100         ELSE                                                     00315100
315200            MOVE DFHBMASF  TO  IBGRIDA,  IPGNIDA,  IPGTIDA        00315200
315300                               IDGDIDA,  IPGPIDA,  IPGSIDA        00315300
315400            MOVE DFHBMASF  TO  IBGRSLTA, IPGNSLTA, IPGTSLTA       00315400
315500                               IDGDSLTA, IPGPSLTA, IPGSSLTA       00315500
315600            MOVE DFHBMUNF  TO  IBGROPTA, IPGNOPTA, IPGTOPTA       00315600
315700                               IDGDOPTA, IPGPOPTA, IPGSOPTA       00315700
315800     ELSE                                                         00315800
315900         NEXT SENTENCE.                                           00315900
316000                                                                  00316000
316100     IF  CDEINDO = '+CDE-'                                        00316100
316200     THEN                                                         00316200
316300*---------------- HIGH-LIGHT CRITICAL DATA ELEMENT LABELS         00316300
316400         MOVE DFHBMABF TO DLOPTLTA  PERIOTA  BENVLQTA  LOTA       00316400
316500              AGELIMA     PLCTRTTA  FAMINDTA SRVGRUTA  CSTCOTTA   00316500
316600              AGEQLTA     COPAYITA  INTDESTA CONDTG1A  CONDTG2A   00316600
316700                          DEFINTTA  BISNDITA                      00316700
316800*---------------- AUTOSKIP AND FSET CRITICAL DATA ELEMENTS        00316800
316900         MOVE DFHBMASF TO DELOPTNA  PERIODA  BENVLQLA  LOBA       00316900
317000        AGELIMLA AGELIMHA PLCTRMTA  FAMINDIA SRVGRUPA  CSTCONTA   00317000
317100        AGEQLLA  AGEQLHA  COPAYINA  INTDESKA CONDALLA  CONDEXCA   00317100
317200                BISNDINA  CONDICDA  CONDTABA CONDMENA  CONDDRGA   00317200
317300                CONDLIFA  CONDALCA  CONDOBCA CONDOBNA  CONDMALA   00317300
317400                CONDEMCA  CONDEACA  CONDSMIA CONDNSMA             00317400
317500                CONDTMJA  CONDCARA  CONDOBSA CONDKDYA  CONDACCA   00317500
317600                CONDINFA  CONDPECA  CONDNEMA CONDSUIA  DEFINTNA   00317600
317700         IF INTDESKO  NOT =  IDPRODO                              00317700
317800*--------- AUTOSKIP AND FSET CRITICAL DATA ELEMENTS               00317800
317900            MOVE DFHBMASF  TO  IBGROPTA,  IPGNOPTA,  IPGTOPTA     00317900
318000                               IDGDOPTA,  IPGPOPTA,  IPGSOPTA     00318000
318100            MOVE DFHBMABF  TO  IBGRIDA,  IBGRSLTA,                00318100
318200                       IPGNIDA,  IPGNSLTA,   IPGTIDA,  IPGTSLTA   00318200
318300                       IDGDIDA,  IDGDSLTA,   IPGPIDA,  IPGPSLTA   00318300
318400                       IPGSIDA,  IPGSSLTA                         00318400
318500            IF  ERRMSGO > SPACES                                  00318500
318600            THEN                                                  00318600
318700                NEXT SENTENCE                                     00318700
318800            ELSE                                                  00318800
318900                SET  WT-01-INDEX  TO  +08                         00318900
319000                MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO 00319000
319100         ELSE                                                     00319100
319200            MOVE DFHBMASF  TO  IBGRIDA,  IBGRSLTA,                00319200
319300                       IPGNIDA,  IPGNSLTA,   IPGTIDA,  IPGTSLTA   00319300
319400                       IDGDIDA,  IDGDSLTA,   IPGPIDA,  IPGPSLTA   00319400
319500                       IPGSIDA,  IPGSSLTA                         00319500
319600            MOVE DFHBMUNF  TO  IBGROPTA, IPGNOPTA, IPGTOPTA       00319600
319700                               IDGDOPTA, IPGPOPTA, IPGSOPTA       00319700
319800            IF  ERRMSGO > SPACES                                  00319800
319900            THEN                                                  00319900
320000                NEXT SENTENCE                                     00320000
320100            ELSE                                                  00320100
320200                SET  WT-01-INDEX  TO  +08                         00320200
320300                MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO 00320300
320400     ELSE                                                         00320400
320500         NEXT SENTENCE.                                           00320500
320600                                                                  00320600
320700 7900-900-EXIT. EXIT.                                             00320700
320800                                                                  00320800
320900/*****************************************************************00320900
321000* 8000  XCTL SWITCH ADD DEL MODE                                 *00321000
321100*                                                                *00321100
321200*   THE OPERATOR WANTS TO SWITCH MODES, FROM DELETING ENTRIES TO *00321200
321300*  ADDING ENTRIES.  WE READ THE ALL LEVEL INTERNAL TABULAR & PASS*00321300
321400*  THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL     *00321400
321500*  TABULAR RECORD TO THE ADD PROGRAM.  (DEPENDING ON THE MENU THE*00321500
321600*  PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE   *00321600
321700*  IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).             *00321700
321800******************************************************************00321800
321900 8000-000-SWITCH-ADD-DEL-MODE   SECTION.                          00321900
322000 8000-010.                                                        00322000
322100                                                                  00322100
322200     IF DELADDI  =  'CHG/DEL'                                     00322200
322300        PERFORM 8100-000-DISPLAY-ADD-SCREEN.                      00322300
322400                                                                  00322400
322500     PERFORM 3100-000-READ-RECORD.                                00322500
322600     MOVE GAF-ENTRY-COUNT  TO  GAF-ENTRY-COUNT.                   00322600
322700                                                                  00322700
322800     IF  GAF-ENTRY-COUNT  >  1                                    00322800
322900     THEN                                                         00322900
323000         MOVE 'CHG/DEL'  TO  DELADDO                              00323000
323100         MOVE 'D'        TO  DELOLITO                             00323100
323200         MOVE SPACES     TO  COCURANO                             00323200
323300         MOVE DFHBMASK   TO  DLOPTLTA                             00323300
323400         MOVE DFHBMUNP   TO  DELOPTNA                             00323400
323500         SET  WT-01-INDEX                     TO  +20             00323500
323600         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO  ERRMSGO         00323600
323700         SET GAF-INDEX   TO 1                                     00323700
323800         PERFORM 4400-000-BUILD-DISPLAY                           00323800
323900     ELSE                                                         00323900
324000         SET  WT-01-INDEX                     TO  +12             00324000
324100         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO  ERRMSGO.        00324100
324200                                                                  00324200
324300 8000-900-EXIT. EXIT.                                             00324300
324400                                                                  00324400
324500/*****************************************************************00324500
324600* 8100  DISPLAY ADD SCREEN                                       *00324600
324700******************************************************************00324700
324800 8100-000-DISPLAY-ADD-SCREEN    SECTION.                          00324800
324900                                                                  00324900
325000     MOVE 'CHG/ADD'  TO  DELADDO.                                 00325000
325100     MOVE SPACES     TO  COCURANO.                                00325100
325200     MOVE DFHBMASD   TO  DLOPTLTA   DELOPTNA.                     00325200
325300     PERFORM 4100-000-DISPLAY-SKELETON.                           00325300
325400                                                                  00325400
325500 8100-900-EXIT. EXIT.                                             00325500
325600                                                                  00325600
325700/*****************************************************************00325700
325800* 9000  SEND ERASE THEN RETURN                                   *00325800
325900******************************************************************00325900
326000 9000-000-SEND-ERASE-RETURN     SECTION.                          00326000
326100 9000-010.                                                        00326100
326200                                                                  00326200
326300     MOVE DFHBMASD                                                00326300
326400       TO       PERLITTA  FDLRCLTA  REININTA  MAXOVRTA            00326400
326500                PERLIMTA  FDLRCLIA  REININDA  MAXOVRDA            00326500
326600                CARYOVRA  CARYOVTA.                               00326600
326700                                                                  00326700
326800     MOVE -1  TO  ERRMSGL.                                        00326800
326900                                                                  00326900
327000     EXEC CICS  SEND   MAP ('GA1XI01')  ERASE  CURSOR             00327000
327100                MAPSET('GA1XSET')   END-EXEC.                     00327100
327200                                                                  00327200
327300     EXEC CICS  RETURN TRANSID('GA1P')                            00327300
327400                COMMAREA(DFHCOMMAREA)                             00327400
327500                LENGTH  (EIBCALEN)                                00327500
327600                END-EXEC.                                         00327600
327700                                                                  00327700
327800 9000-900-EXIT. EXIT.                                             00327800
327900                                                                  00327900
328000/*****************************************************************00328000
328100* 9010  SEND DATAONLY AND RETURN                                 *00328100
328200******************************************************************00328200
328300 9010-000-SEND-DATAONLY-RETURN  SECTION.                          00328300
328400 9010-010.                                                        00328400
328500                                                                  00328500
328600     MOVE -1  TO  ERRMSGL.                                        00328600
328700                                                                  00328700
328800     EXEC CICS  SEND   MAP ('GA1XI01')  DATAONLY  CURSOR          00328800
328900                MAPSET('GA1XSET')  END-EXEC.                      00328900
329000                                                                  00329000
329100     EXEC CICS  RETURN TRANSID('GA1P')                            00329100
329200                COMMAREA(DFHCOMMAREA)                             00329200
329300                LENGTH  (EIBCALEN)                                00329300
329400                END-EXEC.                                         00329400
329500                                                                  00329500
329600 9010-900-EXIT. EXIT.                                             00329600
329700                                                                  00329700
329800/*****************************************************************00329800
329900* 9200  GREGORIAN TO JULIAN                                      *00329900
330000*                                                                *00330000
330100*         MMDDYY---->YYDDD                                       *00330100
330200******************************************************************00330200
330300 9200-000-GREGORIAN-TO-JULIAN   SECTION.                          00330300
330400 9200-010.                                                        00330400
330500                                                                  00330500
330600     MOVE 'CNV'  TO  HGADATE-FUNC.                                00330600
330700     MOVE 'M'    TO  HGADATE-FORM1.                               00330700
330800     MOVE 'J'    TO  HGADATE-FORM2.                               00330800
330900     MOVE ZEROS  TO HGADATE-RETURN   HGADATE-AMOUNT.              00330900
331000                                                                  00331000
331100     EXEC  CICS LINK PROGRAM ('HGADATES')                         00331100
331200                     COMMAREA(HGADATES-COMMAREA)                  00331200
331300                     LENGTH  (LENGTH OF HGADATES-COMMAREA)        00331300
331400                     END-EXEC.                                    00331400
331500                                                                  00331500
331600                                                                  00331600
331700 9200-900-EXIT. EXIT.                                             00331700
331800                                                                  00331800
331900/*****************************************************************00331900
332000* 9300  JULIAN TO GREGORIAN                                      *00332000
332100*                                                                *00332100
332200*          YYDDD---->MMDDYY                                      *00332200
332300******************************************************************00332300
332400 9300-000-JULIAN-TO-GREGORIAN   SECTION.                          00332400
332500 9300-010.                                                        00332500
332600                                                                  00332600
332700     MOVE 'CNV'  TO  HGADATE-FUNC.                                00332700
332800     MOVE 'J'    TO  HGADATE-FORM1.                               00332800
332900     MOVE 'M'    TO  HGADATE-FORM2.                               00332900
333000     MOVE ZEROS  TO HGADATE-RETURN   HGADATE-AMOUNT.              00333000
333100                                                                  00333100
333200     EXEC  CICS LINK PROGRAM ('HGADATES')                         00333200
333300                     COMMAREA(HGADATES-COMMAREA)                  00333300
333400                     LENGTH  (LENGTH OF HGADATES-COMMAREA)        00333400
333500                     END-EXEC.                                    00333500
333600                                                                  00333600
333700 9300-900-EXIT. EXIT.                                             00333700
333800                                                                  00333800
333900/*****************************************************************00333900
334000* 9800  E R R O R   M S G   T H E N   A B E N D                   00334000
334100*                                                                *00334100
334200*    THIS ROUTINE DISPLAYS THE PREVIOUSLY BUILT ERROR MESSAGE    *00334200
334300*  AND THEN ABENDS USING THE ABEND CODE EARLIER DEFINED.         *00334300
334400******************************************************************00334400
334500 9800-000-ERROR-MSG-THEN-ABEND  SECTION.                          00334500
334600 9800-010.                                                        00334600
334700                                                                  00334700
334800     MOVE -1               TO MFRMSLTL.                           00334800
334900     MOVE WS-ABCODE-MSG    TO ERRMSGO.                            00334900
335000                                                                  00335000
335100     EXEC CICS  SEND   MAP ('GA1XI01')  ERASE  CURSOR  WAIT       00335100
335200                MAPSET('GA1XSET')   END-EXEC.                     00335200
335300                                                                  00335300
335400     EXEC CICS  ABEND   ABCODE(WS-ABCODE)  END-EXEC.              00335400
335500                                                                  00335500
335600 9800-900-EXIT. EXIT.                                             00335600
