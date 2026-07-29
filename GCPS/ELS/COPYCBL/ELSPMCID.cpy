000100******************************************************************00000100
000200*    COPYBOOK:   ELSPMCID                                        *00000200
000300*    AUTHOR:     ANNE KEFFER-KING.                               *00000300
000400*    DATE:       16-JUL-1992                                     *00000400
000500*    FUNCTION:   LIST OF BENEFIT PROVISIONS BEING USED BY        *00000500
000600*                THE NOTICE OF ADMISSION/ ELIGIBILITY SUMMARY    *00000600
000700*                                                                *00000700
000800******************************************************************00000800
000900*                      MAINTENANCE HISTORY                       *00000900
001000*                                                                *00001000
001100*  MOD     DATE     BY  DRPT                ACTION               *00001100
001200* ----- ----------- --- ----- ---------------------------------- *00001200
001300* 1.00  16-JUL-1992 AKK       CREATED                            *00001300
001400* 1.01  12-AUG-1992 BAK       ADD TABULAR READ CONTROL DATA      *00001400
001500* 1.02  26-AUG-1992 BAK       CHANGED TABULAR READ CONTROL DATA  *00001500
001500* 1.03  31-AUG-1992 BAK       REMOVED TABULAR TABLE ENTRIES      *00001600
001500* 1.04  23-FEB-1993 BAK       ADD SUPPORT FOR THE FOLLOWING PROV.*00001700
001500*                             DMEI B, DMEO B, DMEI E, DMEO E.    *00001800
001500* 1.05  05-MAR-1993 CGL       ADD SUPPORT FOR CHC  W             *00001900
001500* 1.06  18-MAR-1993 BAK       CORRECT ENTRY COUNT ON INST-OUT.   *00002000
001500* 2.00  19-MAY-1993 BAK       REORGANIZE TABLE TO ALLOW MAJOR    *00002010
001500*                             MEDICAL SUPPORT FOR ALL BENEFITS.  *00002020
001500* 2.01  02-AUG-1993 BAK       ADD SELECTED DATE FOR GVL READ     *00002010
001500* 3.00  20-SEP-1993 BAK       CHANGED TABS-IND1 AND IND2 TO 2    *00002010
001500*                             BYTES EACH FOR NEW TABULAR FILE.   *00002010
001500* 4.00  16-FEB-1995 RGO       SUPPLEMENTAL MEDICARE PROJECT, ADD:*00002010
001500*                             MEDICAL SURGICAL SUPPLIES:         *00002010
001500*                             MSSI  B (II) , MSSO B  (IO)        *00002010
001500*                             MSSI  E (PI) , MSSO E  (PO)        *00002010
001500*                             PROSTHETICS:                       *00002010
001500*                             PRAI  B (II) , PRAO B  (IO)        *00002010
001500*                             PRAI  E (PI) , PRAO E  (PO)        *00002010
001500*                             HOME VISITS:                       *00002010
001500*                             HVIS  E (PO)                       *00002010
001500* 5.00  15-NOV-1997 AKK       ADDED CENTURY COMPLIANT DATE       *00002010
001500* 6.00  06-AUG-1998 AKK       CHANGED DUPLICATE NAME TO CENTURY  *00002010
001500*                             DATE.                                       
001500*                                                                *00002010
001600******************************************************************00002100
001700                                                                  00002200
001800*  IF PRV-TYPE = 'P' - TABULARS GVLP, GVLQ AND GVLR ARE READ      00002300
001900*  IF PRV-TYPE = 'I' - TABULARS GVLF, GVLG AND GVLH ARE READ      00002400
001800*  IF PRV-TYPE = 'M' - TABULARS GVLP, GVLQ AND GVLR ARE READ      00002410
002000 01  NAES-INTERMEDIATE-DATA.                                      00002500
002100     02 NAES-TABULAR-READ-CONTROL-DATA.                           00002600
002200        03 NAES-PRV-TYPE         PIC X(01)         VALUE SPACE.   00002700
002300        03 NAES-PROVIDER-NBR     PIC X(10)         VALUE SPACE.   00002800
007400        03 NAES-TABS-IND1        PIC X(02)         VALUE SPACE.   00002900
007500        03 NAES-TABS-IND2        PIC X(02)         VALUE SPACE.   00003000
007500        03 NAES-TABS-IND3        PIC X(02)         VALUE SPACE.   00003010
007600        03 NAES-TABS-INCL-EXCL   PIC X(01)         VALUE SPACE.   00003100
007700        03 NAES-TABS-RETN-CODE   PIC X(01)         VALUE SPACE.   00003200
007700        03 NAES-PRV-SELECT-DATE.                                          
007700           05 NAES-PRV-SELECT-DATE-CC  PIC X(01).                         
007700           05 NAES-PRV-SELECT-DATE     PIC S9(05)                         
007700                                 COMP-3 VALUE ZEROS.                      
              03 NAES-PRV-SELECT-DATE-CEN REDEFINES NAES-PRV-SELECT-DATE        
                                    PIC S9(07) COMP-3.                          
007600        03 FILLER                PIC X(14)         VALUE SPACE.   00003300
002400        03 NAES-TABS-CONTROL.                                     00003400
002500           04 FILLER.                                             00003500
002600              05 FILLER          PIC  X(01)      VALUE 'N'.       00003600
002700              05 FILLER          PIC  X(06)      VALUE '#GVLF '.  00003700
003200           04 FILLER.                                             00003800
003300              05 FILLER          PIC  X(01)      VALUE 'N'.       00003900
003400              05 FILLER          PIC  X(06)      VALUE '#GVLG '.  00004000
003900           04 FILLER.                                             00004100
004000              05 FILLER          PIC  X(01)      VALUE 'N'.       00004200
004100              05 FILLER          PIC  X(06)      VALUE '#GVLH '.  00004300
004600           04 FILLER.                                             00004400
004700              05 FILLER          PIC  X(01)      VALUE 'N'.       00004500
004800              05 FILLER          PIC  X(06)      VALUE '#GVLP '.  00004600
005300           04 FILLER.                                             00004700
005400              05 FILLER          PIC  X(01)      VALUE 'N'.       00004800
005500              05 FILLER          PIC  X(06)      VALUE '#GVLQ '.  00004900
006000           04 FILLER.                                             00005000
006100              05 FILLER          PIC  X(01)      VALUE 'N'.       00005100
006200              05 FILLER          PIC  X(06)      VALUE '#GVLR '.  00005200
006700                                                                  00005300
006800        03 NAES-TABS             REDEFINES NAES-TABS-CONTROL.     00005400
006900           04  NAES-TABS-CNTL    OCCURS 6 TIMES                   00005500
007000                                 INDEXED BY NAES-TAB-INDX         00005600
007100                                            NAES-MAX-INDX.        00005700
007200             05 NAES-TABS-READ-IND           PIC X(01).           00005800
007300             05 NAES-TABS-ID                 PIC X(06).           00005900
007800                                                                  00006000
007900                                                                  00006100
008000     02 NAES-INSTITUTIONAL-IP-BEN-PROV.                           00006200
008100        03 NAES-II-TABLE-MAX     PIC S9(04) COMP VALUE 26.        00006300
008200                                                                  00006400
008300        03 NAES-II-AMB           PIC  X(06)      VALUE 'AMB  B'.  00006500
008400        03 NAES-II-AMB-DRVD-IND  PIC S9(04) COMP VALUE 0.         00006600
008400        03 NAES-II-AMB-DRVD-CHR  PIC X(01) VALUE SPACE.           00006600
008500        03                       PIC  X(01).                      00006700
008600           88 NAES-II-AMB-YES                    VALUE 'Y'.       00006800
008700           88 NAES-II-AMB-NO                     VALUE 'N'.       00006900
008800                                                                  00007000
008900        03 NAES-II-ARPI          PIC  X(06)      VALUE 'ARPI W'.  00007100
009000        03 NAES-II-ARPI-DRVD-IND PIC S9(04) COMP VALUE 0.         00007200
009000        03 NAES-II-ARPI-DRVD-CHR PIC X(01) VALUE SPACE.           00007200
009100        03                       PIC  X(01).                      00007300
009200           88 NAES-II-ARPI-YES                   VALUE 'Y'.       00007400
009300           88 NAES-II-ARPI-NO                    VALUE 'N'.       00007500
009400                                                                  00007600
009500        03 NAES-II-CHCW          PIC  X(06)      VALUE 'CHC  W'.  00007700
009600        03 NAES-II-CHC-DRVD-IND  PIC S9(04) COMP VALUE 0.         00007800
009600        03 NAES-II-CHC-DRVD-CHR  PIC X(01) VALUE SPACE.           00007800
009700        03                       PIC  X(01).                      00007900
009800           88 NAES-II-CHC-YES                    VALUE 'Y'.       00008000
009900           88 NAES-II-CHC-NO                     VALUE 'N'.       00008100
010000                                                                  00008200
010100        03 NAES-II-DMEI          PIC  X(06)      VALUE 'DMEI B'.  00008300
010200        03 NAES-II-DMEI-DRVD-IND PIC S9(04) COMP VALUE 0.         00008400
010200        03 NAES-II-DMEI-DRVD-CHR PIC X(01) VALUE SPACE.           00008400
010300        03                       PIC  X(01).                      00008500
010400           88 NAES-II-DMEI-YES                   VALUE 'Y'.       00008600
010500           88 NAES-II-DMEI-NO                    VALUE 'N'.       00008700
010600                                                                  00008800
010100        03 NAES-II-DMRI          PIC  X(06)      VALUE 'DMRI B'.  00008900
010200        03 NAES-II-DMRI-DRVD-IND PIC S9(04) COMP VALUE 0.         00009000
010200        03 NAES-II-DMRI-DRVD-CHR PIC X(01) VALUE SPACE.           00009000
010300        03                       PIC  X(01).                      00009100
010400           88 NAES-II-DMRI-YES                   VALUE 'Y'.       00009200
010500           88 NAES-II-DMRI-NO                    VALUE 'N'.       00009300
010600                                                                  00009400
010700        03 NAES-II-DPSY          PIC  X(06)      VALUE 'DPSY A'.  00009500
010800        03 NAES-II-DPSY-DRVD-IND PIC S9(04) COMP VALUE 0.         00009600
010800        03 NAES-II-DPSY-DRVD-CHR PIC X(01) VALUE SPACE.           00009600
010900        03                       PIC  X(01).                      00009700
011000           88 NAES-II-DPSY-YES                   VALUE 'Y'.       00009800
011100           88 NAES-II-DPSY-NO                    VALUE 'N'.       00009900
011200                                                                  00010000
011300        03 NAES-II-DRB           PIC  X(06)      VALUE 'DRB  A'.  00010100
011400        03 NAES-II-DRB-DRVD-IND  PIC S9(04) COMP VALUE 0.         00010200
011400        03 NAES-II-DRB-DRVD-CHR  PIC X(01) VALUE SPACE.           00010200
011500        03                       PIC  X(01).                      00010300
011600           88 NAES-II-DRB-YES                    VALUE 'Y'.       00010400
011700           88 NAES-II-DRB-NO                     VALUE 'N'.       00010500
011800                                                                  00010600
011900        03 NAES-II-DRPI          PIC  X(06)      VALUE 'DRPI W'.  00010700
012000        03 NAES-II-DRPI-DRVD-IND PIC S9(04) COMP VALUE 0.         00010800
012000        03 NAES-II-DRPI-DRVD-CHR PIC X(01) VALUE SPACE.           00010800
012100        03                       PIC  X(01).                      00010900
012200           88 NAES-II-DRPI-YES                   VALUE 'Y'.       00011000
012300           88 NAES-II-DRPI-NO                    VALUE 'N'.       00011100
012400                                                                  00011200
012500        03 NAES-II-FOTI          PIC  X(06)      VALUE 'FOTI B'.  00011300
012600        03 NAES-II-FOTI-DRVD-IND PIC S9(04) COMP VALUE 0.         00011400
012600        03 NAES-II-FOTI-DRVD-CHR PIC X(01) VALUE SPACE.           00011400
012700        03                       PIC  X(01).                      00011500
012800           88 NAES-II-FOTI-YES                   VALUE 'Y'.       00011600
012900           88 NAES-II-FOTI-NO                    VALUE 'N'.       00011700
013000                                                                  00011800
013100        03 NAES-II-LABI          PIC  X(06)      VALUE 'LABI B'.  00011900
013200        03 NAES-II-LABI-DRVD-IND PIC S9(04) COMP VALUE 0.         00012000
013200        03 NAES-II-LABI-DRVD-CHR PIC X(01) VALUE SPACE.           00012000
013300        03                       PIC  X(01).                      00012100
013400           88 NAES-II-LABI-YES                   VALUE 'Y'.       00012200
013500           88 NAES-II-LABI-NO                    VALUE 'N'.       00012300
013000                                                                  00011800
013100        03 NAES-II-MSPI          PIC  X(06)      VALUE 'MSSI B'.  00011900
013200        03 NAES-II-MSPI-DRVD-IND PIC S9(04) COMP VALUE 0.         00012000
013200        03 NAES-II-MSPI-DRVD-CHR PIC X(01) VALUE SPACE.           00012000
013300        03                       PIC  X(01).                      00012100
013400           88 NAES-II-MSPI-YES                   VALUE 'Y'.       00012200
013500           88 NAES-II-MSPI-NO                    VALUE 'N'.       00012300
013600                                                                  00012400
013700        03 NAES-II-NPSY          PIC  X(06)      VALUE 'NPSY A'.  00012500
013800        03 NAES-II-NPSY-DRVD-IND PIC S9(04) COMP VALUE 0.         00012600
013800        03 NAES-II-NPSY-DRVD-CHR PIC X(01) VALUE SPACE.           00012600
013900        03                       PIC  X(01).                      00012700
014000           88 NAES-II-NPSY-YES                   VALUE 'Y'.       00012800
014100           88 NAES-II-NPSY-NO                    VALUE 'N'.       00012900
014200                                                                  00013000
014300        03 NAES-II-NRSI          PIC  X(06)      VALUE 'NRSI B'.  00013100
014400        03 NAES-II-NRSI-DRVD-IND PIC S9(04) COMP VALUE 0.         00013200
014400        03 NAES-II-NRSI-DRVD-CHR PIC X(01) VALUE SPACE.           00013200
014500        03                       PIC  X(01).                      00013300
014600           88 NAES-II-NRSI-YES                   VALUE 'Y'.       00013400
014700           88 NAES-II-NRSI-NO                    VALUE 'N'.       00013500
014800                                                                  00013600
014900        03 NAES-II-OBCD          PIC  X(06)      VALUE 'OBCD W'.  00013700
015000        03 NAES-II-OBCD-DRVD-IND PIC S9(04) COMP VALUE 0.         00013800
015000        03 NAES-II-OBCD-DRVD-CHR PIC X(01) VALUE SPACE.           00013800
015100        03                       PIC  X(01).                      00013900
015200           88 NAES-II-OBCD-YES                   VALUE 'Y'.       00014000
015300           88 NAES-II-OBCD-NO                    VALUE 'N'.       00014100
015400                                                                  00014200
015500        03 NAES-II-OBCM          PIC  X(06)      VALUE 'OBCM W'.  00014300
015600        03 NAES-II-OBCM-DRVD-IND PIC S9(04) COMP VALUE 0.         00014400
015600        03 NAES-II-OBCM-DRVD-CHR PIC X(01) VALUE SPACE.           00014400
015700        03                       PIC  X(01).                      00014500
015800           88 NAES-II-OBCM-YES                   VALUE 'Y'.       00014600
015900           88 NAES-II-OBCM-NO                    VALUE 'N'.       00014700
016000                                                                  00014800
016100        03 NAES-II-OBCS          PIC  X(06)      VALUE 'OBCS W'.  00014900
016200        03 NAES-II-OBCS-DRVD-IND PIC S9(04) COMP VALUE 0.         00015000
016200        03 NAES-II-OBCS-DRVD-CHR PIC X(01) VALUE SPACE.           00015000
016300        03                       PIC  X(01).                      00015100
016400           88 NAES-II-OBCS-YES                   VALUE 'Y'.       00015200
016500           88 NAES-II-OBCS-NO                    VALUE 'N'.       00015300
016600                                                                  00015400
016700        03 NAES-II-OBND          PIC  X(06)      VALUE 'OBND W'.  00015500
016800        03 NAES-II-OBND-DRVD-IND PIC S9(04) COMP VALUE 0.         00015600
016800        03 NAES-II-OBND-DRVD-CHR PIC X(01) VALUE SPACE.           00015600
016900        03                       PIC  X(01).                      00015700
017000           88 NAES-II-OBND-YES                   VALUE 'Y'.       00015800
017100           88 NAES-II-OBND-NO                    VALUE 'N'.       00015900
017200                                                                  00016000
017300        03 NAES-II-OBNM          PIC  X(06)      VALUE 'OBNM W'.  00016100
017400        03 NAES-II-OBNM-DRVD-IND PIC S9(04) COMP VALUE 0.         00016200
017400        03 NAES-II-OBNM-DRVD-CHR PIC X(01) VALUE SPACE.           00016200
017500        03                       PIC  X(01).                      00016300
017600           88 NAES-II-OBNM-YES                   VALUE 'Y'.       00016400
017700           88 NAES-II-OBNM-NO                    VALUE 'N'.       00016500
017800                                                                  00016600
017900        03 NAES-II-OBNS          PIC  X(06)      VALUE 'OBNS W'.  00016700
018000        03 NAES-II-OBNS-DRVD-IND PIC S9(04) COMP VALUE 0.         00016800
018000        03 NAES-II-OBNS-DRVD-CHR PIC X(01) VALUE SPACE.           00016800
018100        03                       PIC  X(01).                      00016900
018200           88 NAES-II-OBNS-YES                   VALUE 'Y'.       00017000
018300           88 NAES-II-OBNS-NO                    VALUE 'N'.       00017100
018400                                                                  00017200
018500        03 NAES-II-PMTI          PIC  X(06)      VALUE 'PMTI B'.  00017300
018600        03 NAES-II-PMTI-DRVD-IND PIC S9(04) COMP VALUE 0.         00017400
018600        03 NAES-II-PMTI-DRVD-CHR PIC X(01) VALUE SPACE.           00017400
018700        03                       PIC  X(01).                      00017500
018800           88 NAES-II-PMTI-YES                   VALUE 'Y'.       00017600
018900           88 NAES-II-PMTI-NO                    VALUE 'N'.       00017700
019000                                                                  00017800
019100        03 NAES-II-PSYI          PIC  X(06)      VALUE 'PSYI W'.  00017900
019200        03 NAES-II-PSYI-DRVD-IND PIC S9(04) COMP VALUE 0.         00018000
019200        03 NAES-II-PSYI-DRVD-CHR PIC X(01) VALUE SPACE.           00018000
019300        03                       PIC  X(01).                      00018100
019400           88 NAES-II-PSYI-YES                   VALUE 'Y'.       00018200
019500           88 NAES-II-PSYI-NO                    VALUE 'N'.       00018300
019600                                                                  00018400
019100        03 NAES-II-PRSI          PIC  X(06)      VALUE 'PRAI B'.  00017900
019200        03 NAES-II-PRSI-DRVD-IND PIC S9(04) COMP VALUE 0.         00018000
019200        03 NAES-II-PRSI-DRVD-CHR PIC X(01) VALUE SPACE.           00018000
019300        03                       PIC  X(01).                      00018100
019400           88 NAES-II-PRSI-YES                   VALUE 'Y'.       00018200
019500           88 NAES-II-PRSI-NO                    VALUE 'N'.       00018300
019600                                                                  00018400
019700        03 NAES-II-PVTA          PIC  X(06)      VALUE 'PVTA A'.  00018500
019800        03 NAES-II-PVTA-DRVD-IND PIC S9(04) COMP VALUE 0.         00018600
019800        03 NAES-II-PVTA-DRVD-CHR PIC X(01) VALUE SPACE.           00018600
019900        03                       PIC  X(01).                      00018700
020000           88 NAES-II-PVTA-YES                   VALUE 'Y'.       00018800
020100           88 NAES-II-PVTA-NO                    VALUE 'N'.       00018900
020200                                                                  00019000
020300        03 NAES-II-PVTR          PIC  X(06)      VALUE 'PVTR A'.  00019100
020400        03 NAES-II-PVTR-DRVD-IND PIC S9(04) COMP VALUE 0.         00019200
020400        03 NAES-II-PVTR-DRVD-CHR PIC X(01) VALUE SPACE.           00019200
020500        03                       PIC  X(01).                      00019300
020600           88 NAES-II-PVTR-YES                   VALUE 'Y'.       00019400
020700           88 NAES-II-PVTR-NO                    VALUE 'N'.       00019500
020800                                                                  00019600
020900        03 NAES-II-SPTI          PIC  X(06)      VALUE 'SPTI B'.  00019700
021000        03 NAES-II-SPTI-DRVD-IND PIC S9(04) COMP VALUE 0.         00019800
021000        03 NAES-II-SPTI-DRVD-CHR PIC X(01) VALUE SPACE.           00019800
021100        03                       PIC  X(01).                      00019900
021200           88 NAES-II-SPTI-YES                   VALUE 'Y'.       00020000
021300           88 NAES-II-SPTI-NO                    VALUE 'N'.       00020100
021400                                                                  00020200
021500        03 NAES-II-XRYI          PIC  X(06)      VALUE 'XRYI B'.  00020300
021600        03 NAES-II-XRYI-DRVD-IND PIC S9(04) COMP VALUE 0.         00020400
021600        03 NAES-II-XRYI-DRVD-CHR PIC X(01) VALUE SPACE.           00020400
021700        03                       PIC  X(01).                      00020500
021800           88 NAES-II-XRYI-YES                   VALUE 'Y'.       00020600
021900           88 NAES-II-XRYI-NO                    VALUE 'N'.       00020700
022000                                                                  00020800
023300     02 NAES-PROFESSIONAL-IP-BEN-PROV.                            00020900
023400        03 NAES-PI-TABLE-MAX     PIC S9(04) COMP VALUE 24.        00021000
023500                                                                  00021100
023600        03 NAES-PI-AHI            PIC  X(06)     VALUE 'AHI  D'.  00021200
023700        03 NAES-PI-AHI-DRVD-IND   PIC S9(04) COMP VALUE 0.        00021300
023700        03 NAES-PI-AHI-DRVD-CHR   PIC X(01) VALUE SPACE.          00021300
023800        03                        PIC  X(01).                     00021400
023900           88 NAES-PI-AHI-YES                    VALUE 'Y'.       00021500
024000           88 NAES-PI-AHI-NO                     VALUE 'N'.       00021600
024100                                                                  00021700
024110        03 NAES-PI-AMB            PIC  X(06)     VALUE 'AMB  E'.  00021800
024120        03 NAES-PI-AMB-DRVD-IND   PIC S9(04) COMP VALUE 0.        00021900
024120        03 NAES-PI-AMB-DRVD-CHR   PIC X(01) VALUE SPACE.          00021900
024130        03                        PIC  X(01).                     00022000
024140           88 NAES-PI-AMB-YES                    VALUE 'Y'.       00022100
024150           88 NAES-PI-AMB-NO                     VALUE 'N'.       00022200
024160                                                                  00022300
024200        03 NAES-PI-ASOP           PIC  X(06)     VALUE 'ASOP E'.  00022400
024300        03 NAES-PI-ASOP-DRVD-IND  PIC S9(04) COMP VALUE 0.        00022500
024300        03 NAES-PI-ASOP-DRVD-CHR  PIC X(01) VALUE SPACE.          00022500
024400        03                        PIC  X(01).                     00022600
024500           88 NAES-PI-ASOP-YES                   VALUE 'Y'.       00022700
024600           88 NAES-PI-ASOP-NO                    VALUE 'N'.       00022800
024700                                                                  00022900
024800        03 NAES-PI-CHCV           PIC  X(06)     VALUE 'CHCV D'.  00023000
024900        03 NAES-PI-CHCV-DRVD-IND  PIC S9(04) COMP VALUE 0.        00023100
024900        03 NAES-PI-CHCV-DRVD-CHR  PIC X(01) VALUE SPACE.          00023100
025000        03                        PIC  X(01).                     00023200
025100           88 NAES-PI-CHCV-YES                   VALUE 'Y'.       00023300
025200           88 NAES-PI-CHCV-NO                    VALUE 'N'.       00023400
025300                                                                  00023500
025400        03 NAES-PI-DMEI           PIC  X(06)     VALUE 'DMEI E'.  00023600
025500        03 NAES-PI-DMEI-DRVD-IND  PIC S9(04) COMP VALUE 0.        00023700
025500        03 NAES-PI-DMEI-DRVD-CHR  PIC X(01) VALUE SPACE.          00023700
025600        03                        PIC  X(01).                     00023800
025700           88 NAES-PI-DMEI-YES                   VALUE 'Y'.       00023900
025800           88 NAES-PI-DMEI-NO                    VALUE 'N'.       00024000
025900                                                                  00024100
025400        03 NAES-PI-DMRI           PIC  X(06)     VALUE 'DMRI E'.  00024200
025500        03 NAES-PI-DMRI-DRVD-IND  PIC S9(04) COMP VALUE 0.        00024300
025500        03 NAES-PI-DMRI-DRVD-CHR  PIC X(01) VALUE SPACE.          00024300
025600        03                        PIC  X(01).                     00024400
025700           88 NAES-PI-DMRI-YES                   VALUE 'Y'.       00024500
025800           88 NAES-PI-DMRI-NO                    VALUE 'N'.       00024600
025900                                                                  00024700
026000        03 NAES-PI-DPV            PIC  X(06)     VALUE 'DPV  D'.  00024800
026100        03 NAES-PI-DPV-DRVD-IND   PIC S9(04) COMP VALUE 0.        00024900
026100        03 NAES-PI-DPV-DRVD-CHR   PIC X(01) VALUE SPACE.          00024900
026200        03                        PIC  X(01).                     00025000
026300           88 NAES-PI-DPV-YES                    VALUE 'Y'.       00025100
026400           88 NAES-PI-DPV-NO                     VALUE 'N'.       00025200
026500                                                                  00025300
026510        03 NAES-PI-DRI            PIC  X(06)     VALUE 'DRI  D'.  00025400
026520        03 NAES-PI-DRI-DRVD-IND   PIC S9(04) COMP VALUE 0.        00025500
026520        03 NAES-PI-DRI-DRVD-CHR   PIC X(01) VALUE SPACE.          00025500
026530        03                        PIC  X(01).                     00025600
026540           88 NAES-PI-DRI-YES                    VALUE 'Y'.       00025700
026550           88 NAES-PI-DRI-NO                     VALUE 'N'.       00025800
026560                                                                  00025900
026600        03 NAES-PI-FOTI           PIC  X(06)     VALUE 'FOTI E'.  00026000
026700        03 NAES-PI-FOTI-DRVD-IND  PIC S9(04) COMP VALUE 0.        00026100
026700        03 NAES-PI-FOTI-DRVD-CHR  PIC X(01) VALUE SPACE.          00026100
026800        03                        PIC  X(01).                     00026200
026900           88 NAES-PI-FOTI-YES                   VALUE 'Y'.       00026300
027000           88 NAES-PI-FOTI-NO                    VALUE 'N'.       00026400
027100                                                                  00026500
027200        03 NAES-PI-LABI           PIC  X(06)     VALUE 'LABI E'.  00026600
027300        03 NAES-PI-LABI-DRVD-IND  PIC S9(04) COMP VALUE 0.        00026700
027300        03 NAES-PI-LABI-DRVD-CHR  PIC X(01) VALUE SPACE.          00026700
027400        03                        PIC  X(01).                     00026800
027500           88 NAES-PI-LABI-YES                   VALUE 'Y'.       00026900
027600           88 NAES-PI-LABI-NO                    VALUE 'N'.       00027000
027700                                                                  00027100
027800        03 NAES-PI-MNI            PIC  X(06)     VALUE 'MNI  D'.  00027200
027900        03 NAES-PI-MNI-DRVD-IND   PIC S9(04) COMP VALUE 0.        00027300
027900        03 NAES-PI-MNI-DRVD-CHR   PIC X(01) VALUE SPACE.          00027300
028000        03                        PIC  X(01).                     00027400
028100           88 NAES-PI-MNI-YES                    VALUE 'Y'.       00027500
028200           88 NAES-PI-MNI-NO                     VALUE 'N'.       00027600
027700                                                                  00027100
027800        03 NAES-PI-MSPI           PIC  X(06)     VALUE 'MSSI E'.  00027200
027900        03 NAES-PI-MSPI-DRVD-IND  PIC S9(04) COMP VALUE 0.        00027300
027900        03 NAES-PI-MSPI-DRVD-CHR  PIC X(01) VALUE SPACE.          00027300
028000        03                        PIC  X(01).                     00027400
028100           88 NAES-PI-MSPI-YES                   VALUE 'Y'.       00027500
028200           88 NAES-PI-MSPI-NO                    VALUE 'N'.       00027600
028300                                                                  00027700
028400        03 NAES-PI-NPV            PIC  X(06)     VALUE 'NPV  D'.  00027800
028500        03 NAES-PI-NPV-DRVD-IND   PIC S9(04) COMP VALUE 0.        00027900
028500        03 NAES-PI-NPV-DRVD-CHR   PIC X(01) VALUE SPACE.          00027900
028600        03                        PIC  X(01).                     00028000
028700           88 NAES-PI-NPV-YES                    VALUE 'Y'.       00028100
028800           88 NAES-PI-NPV-NO                     VALUE 'N'.       00028200
028900                                                                  00028300
029000        03 NAES-PI-NRSI           PIC  X(06)     VALUE 'NRSI E'.  00028400
029100        03 NAES-PI-NRSI-DRVD-IND  PIC S9(04) COMP VALUE 0.        00028500
029100        03 NAES-PI-NRSI-DRVD-CHR  PIC X(01) VALUE SPACE.          00028500
029200        03                        PIC  X(01).                     00028600
029300           88 NAES-PI-NRSI-YES                   VALUE 'Y'.       00028700
029400           88 NAES-PI-NRSI-NO                    VALUE 'N'.       00028800
029500                                                                  00028900
029600        03 NAES-PI-OBCD           PIC  X(06)     VALUE 'OBCD C'.  00029000
029700        03 NAES-PI-OBCD-DRVD-IND  PIC S9(04) COMP VALUE 0.        00029100
029700        03 NAES-PI-OBCD-DRVD-CHR  PIC X(01) VALUE SPACE.          00029100
029800        03                        PIC  X(01).                     00029200
029900            88 NAES-PI-OBCD-YES                  VALUE 'Y'.       00029300
030000            88 NAES-PI-OBCD-NO                   VALUE 'N'.       00029400
030100                                                                  00029500
030200        03 NAES-PI-OBCM           PIC  X(06)     VALUE 'OBCM C'.  00029600
030300        03 NAES-PI-OBCM-DRVD-IND  PIC S9(04) COMP VALUE 0.        00029700
030300        03 NAES-PI-OBCM-DRVD-CHR  PIC X(01) VALUE SPACE.          00029700
030400        03                        PIC  X(01).                     00029800
030500           88 NAES-PI-OBCM-YES                   VALUE 'Y'.       00029900
030600           88 NAES-PI-OBCM-NO                    VALUE 'N'.       00030000
030700                                                                  00030100
030800        03 NAES-PI-OBCS           PIC  X(06)     VALUE 'OBCS C'.  00030200
030900        03 NAES-PI-OBCS-DRVD-IND  PIC S9(04) COMP VALUE 0.        00030300
030900        03 NAES-PI-OBCS-DRVD-CHR  PIC X(01) VALUE SPACE.          00030300
031000        03                        PIC  X(01).                     00030400
031100           88 NAES-PI-OBCS-YES                   VALUE 'Y'.       00030500
031200           88 NAES-PI-OBCS-NO                    VALUE 'N'.       00030600
031300                                                                  00030700
031400        03 NAES-PI-OBND           PIC  X(06)     VALUE 'OBND C'.  00030800
031500        03 NAES-PI-OBND-DRVD-IND  PIC S9(04) COMP VALUE 0.        00030900
031500        03 NAES-PI-OBND-DRVD-CHR  PIC X(01) VALUE SPACE.          00030900
031600        03                        PIC  X(01).                     00031000
031700           88 NAES-PI-OBND-YES                   VALUE 'Y'.       00031100
031800           88 NAES-PI-OBND-NO                    VALUE 'N'.       00031200
031900                                                                  00031300
032000        03 NAES-PI-OBNM           PIC  X(06)     VALUE 'OBNM C'.  00031400
032100        03 NAES-PI-OBNM-DRVD-IND  PIC S9(04) COMP VALUE 0.        00031500
032100        03 NAES-PI-OBNM-DRVD-CHR  PIC X(01) VALUE SPACE.          00031500
032200        03                        PIC  X(01).                     00031600
032300           88 NAES-PI-OBNM-YES                   VALUE 'Y'.       00031700
032400           88 NAES-PI-OBNM-NO                    VALUE 'N'.       00031800
032500                                                                  00031900
032600        03 NAES-PI-OBNS           PIC  X(06)     VALUE 'OBNS C'.  00032000
032700        03 NAES-PI-OBNS-DRVD-IND  PIC S9(04) COMP VALUE 0.        00032100
032700        03 NAES-PI-OBNS-DRVD-CHR  PIC X(01) VALUE SPACE.          00032100
032800        03                        PIC  X(01).                     00032200
032900           88 NAES-PI-OBNS-YES                   VALUE 'Y'.       00032300
033000           88 NAES-PI-OBNS-NO                    VALUE 'N'.       00032400
033100                                                                  00032500
033200        03 NAES-PI-PMTI           PIC  X(06)     VALUE 'PMTI E'.  00032600
033300        03 NAES-PI-PMTI-DRVD-IND  PIC S9(04) COMP VALUE 0.        00032700
033300        03 NAES-PI-PMTI-DRVD-CHR  PIC X(01) VALUE SPACE.          00032700
033400        03                        PIC  X(01).                     00032800
033500           88 NAES-PI-PMTI-YES                   VALUE 'Y'.       00032900
033600           88 NAES-PI-PMTI-NO                    VALUE 'N'.       00033000
033100                                                                  00032500
033200        03 NAES-PI-PRSI           PIC  X(06)     VALUE 'PRAI E'.  00032600
033300        03 NAES-PI-PRSI-DRVD-IND  PIC S9(04) COMP VALUE 0.        00032700
033300        03 NAES-PI-PRSI-DRVD-CHR  PIC X(01) VALUE SPACE.          00032700
033400        03                        PIC  X(01).                     00032800
033500           88 NAES-PI-PRSI-YES                   VALUE 'Y'.       00032900
033600           88 NAES-PI-PRSI-NO                    VALUE 'N'.       00033000
033700                                                                  00033100
033800        03 NAES-PI-SPTI           PIC  X(06)     VALUE 'SPTI E'.  00033200
033900        03 NAES-PI-SPTI-DRVD-IND  PIC S9(04) COMP VALUE 0.        00033300
033900        03 NAES-PI-SPTI-DRVD-CHR  PIC X(01) VALUE SPACE.          00033300
034000        03                        PIC  X(01).                     00033400
034100           88 NAES-PI-SPTI-YES                   VALUE 'Y'.       00033500
034200           88 NAES-PI-SPTI-NO                    VALUE 'N'.       00033600
034300                                                                  00033700
034400        03 NAES-PI-XRYI           PIC  X(06)     VALUE 'XRYI E'.  00033800
034500        03 NAES-PI-XRYI-DRVD-IND  PIC S9(04) COMP VALUE 0.        00033900
034500        03 NAES-PI-XRYI-DRVD-CHR  PIC X(01) VALUE SPACE.          00033900
034600        03                        PIC  X(01).                     00034000
034700           88 NAES-PI-XRYI-YES                   VALUE 'Y'.       00034100
034800           88 NAES-PI-XRYI-NO                    VALUE 'N'.       00034200
034900                                                                  00034300
036200     02 NAES-INSTITUTIONAL-OP-BEN-PROV.                           00034400
036300        03 NAES-IO-TABLE-MAX     PIC S9(04) COMP VALUE 23.        00034500
036400                                                                  00034600
036500        03 NAES-IO-AMB            PIC  X(06)     VALUE 'AMB  B'.  00034700
036600        03 NAES-IO-AMB-DRVD-IND   PIC S9(04) COMP VALUE 0.        00034800
036600        03 NAES-IO-AMB-DRVD-CHR   PIC X(01) VALUE SPACE.          00034800
036700        03                        PIC  X(01).                     00034900
036800           88 NAES-IO-AMB-YES                    VALUE 'Y'.       00035000
036900           88 NAES-IO-AMB-NO                     VALUE 'N'.       00035100
037000                                                                  00035200
037100        03 NAES-IO-ARPO           PIC  X(06)     VALUE 'ARPO W'.  00035300
037200        03 NAES-IO-ARPO-DRVD-IND  PIC S9(04) COMP VALUE 0.        00035400
037200        03 NAES-IO-ARPO-DRVD-CHR  PIC X(01) VALUE SPACE.          00035400
037300        03                        PIC  X(01).                     00035500
037400           88 NAES-IO-ARPO-YES                   VALUE 'Y'.       00035600
037500           88 NAES-IO-ARPO-NO                    VALUE 'N'.       00035700
037600                                                                  00035800
037100        03 NAES-IO-CHCW           PIC  X(06)     VALUE 'CHC  W'.  00035900
037200        03 NAES-IO-CHC-DRVD-IND   PIC S9(04) COMP VALUE 0.        00036000
037200        03 NAES-IO-CHC-DRVD-CHR   PIC X(01) VALUE SPACE.          00036000
037300        03                        PIC  X(01).                     00036100
037400           88 NAES-IO-CHC-YES                    VALUE 'Y'.       00036200
037500           88 NAES-IO-CHC-NO                     VALUE 'N'.       00036300
037600                                                                  00036400
037700        03 NAES-IO-DMEO           PIC  X(06)     VALUE 'DMEO B'.  00036500
037800        03 NAES-IO-DMEO-DRVD-IND  PIC S9(04) COMP VALUE 0.        00036600
037800        03 NAES-IO-DMEO-DRVD-CHR  PIC X(01) VALUE SPACE.          00036600
037900        03                        PIC  X(01).                     00036700
038000           88 NAES-IO-DMEO-YES                   VALUE 'Y'.       00036800
038100           88 NAES-IO-DMEO-NO                    VALUE 'N'.       00036900
038200                                                                  00037000
037700        03 NAES-IO-DMRO           PIC  X(06)     VALUE 'DMRO B'.  00037100
037800        03 NAES-IO-DMRO-DRVD-IND  PIC S9(04) COMP VALUE 0.        00037200
037800        03 NAES-IO-DMRO-DRVD-CHR  PIC X(01) VALUE SPACE.          00037200
037900        03                        PIC  X(01).                     00037300
038000           88 NAES-IO-DMRO-YES                   VALUE 'Y'.       00037400
038100           88 NAES-IO-DMRO-NO                    VALUE 'N'.       00037500
038200                                                                  00037600
038300        03 NAES-IO-DRPO           PIC  X(06)     VALUE 'DRPO W'.  00037700
038400        03 NAES-IO-DRPO-DRVD-IND  PIC S9(04) COMP VALUE 0.        00037800
038400        03 NAES-IO-DRPO-DRVD-CHR  PIC X(01) VALUE SPACE.          00037800
038500        03                        PIC  X(01).                     00037900
038600           88 NAES-IO-DRPO-YES                   VALUE 'Y'.       00038000
038700           88 NAES-IO-DRPO-NO                    VALUE 'N'.       00038100
038800                                                                  00038200
038900        03 NAES-IO-EAER           PIC  X(06)     VALUE 'EAER B'.  00038300
039000        03 NAES-IO-EAER-DRVD-IND  PIC S9(04) COMP VALUE 0.        00038400
039000        03 NAES-IO-EAER-DRVD-CHR  PIC X(01) VALUE SPACE.          00038400
039100        03                        PIC  X(01).                     00038500
039200           88 NAES-IO-EAER-YES                   VALUE 'Y'.       00038600
039300           88 NAES-IO-EAER-NO                    VALUE 'N'.       00038700
039400                                                                  00038800
039500        03 NAES-IO-EMER           PIC  X(06)     VALUE 'EMER B'.  00038900
039600        03 NAES-IO-EMER-DRVD-IND  PIC S9(04) COMP VALUE 0.        00039000
039600        03 NAES-IO-EMER-DRVD-CHR  PIC X(01) VALUE SPACE.          00039000
039700        03                        PIC  X(01).                     00039100
039800           88 NAES-IO-EMER-YES                   VALUE 'Y'.       00039200
039900           88 NAES-IO-EMER-NO                    VALUE 'N'.       00039300
040000                                                                  00039400
040100        03 NAES-IO-FOTO           PIC  X(06)     VALUE 'FOTO B'.  00039500
040200        03 NAES-IO-FOTO-DRVD-IND  PIC S9(04) COMP VALUE 0.        00039600
040200        03 NAES-IO-FOTO-DRVD-CHR  PIC X(01) VALUE SPACE.          00039600
040300        03                        PIC  X(01).                     00039700
040400           88 NAES-IO-FOTO-YES                   VALUE 'Y'.       00039800
040500           88 NAES-IO-FOTO-NO                    VALUE 'N'.       00039900
040600                                                                  00040000
040700        03 NAES-IO-LABO           PIC  X(06)     VALUE 'LABO B'.  00040100
040800        03 NAES-IO-LABO-DRVD-IND  PIC S9(04) COMP VALUE 0.        00040200
040800        03 NAES-IO-LABO-DRVD-CHR  PIC X(01) VALUE SPACE.          00040200
040900        03                        PIC  X(01).                     00040300
041000           88 NAES-IO-LABO-YES                   VALUE 'Y'.       00040400
041100           88 NAES-IO-LABO-NO                    VALUE 'N'.       00040500
040600                                                                  00040000
040700        03 NAES-IO-MSPO           PIC  X(06)     VALUE 'MSSO B'.  00040100
040800        03 NAES-IO-MSPO-DRVD-IND  PIC S9(04) COMP VALUE 0.        00040200
040800        03 NAES-IO-MSPO-DRVD-CHR  PIC X(01) VALUE SPACE.          00040200
040900        03                        PIC  X(01).                     00040300
041000           88 NAES-IO-MSPO-YES                   VALUE 'Y'.       00040400
041100           88 NAES-IO-MSPO-NO                    VALUE 'N'.       00040500
041200                                                                  00040600
041300        03 NAES-IO-NRSO           PIC  X(06)     VALUE 'NRSO B'.  00040700
041400        03 NAES-IO-NRSO-DRVD-IND  PIC S9(04) COMP VALUE 0.        00040800
041400        03 NAES-IO-NRSO-DRVD-CHR  PIC X(01) VALUE SPACE.          00040800
041500        03                        PIC  X(01).                     00040900
041600           88 NAES-IO-NRSO-YES                   VALUE 'Y'.       00041000
041700           88 NAES-IO-NRSO-NO                    VALUE 'N'.       00041100
041800                                                                  00041200
041900        03 NAES-IO-OBCD           PIC  X(06)     VALUE 'OBCD W'.  00041300
042000        03 NAES-IO-OBCD-DRVD-IND  PIC S9(04) COMP VALUE 0.        00041400
042000        03 NAES-IO-OBCD-DRVD-CHR  PIC X(01) VALUE SPACE.          00041400
042100        03                        PIC  X(01).                     00041500
042200           88 NAES-IO-OBCD-YES                   VALUE 'Y'.       00041600
042300           88 NAES-IO-OBCD-NO                    VALUE 'N'.       00041700
042400                                                                  00041800
042500        03 NAES-IO-OBCM           PIC  X(06)     VALUE 'OBCM W'.  00041900
042600        03 NAES-IO-OBCM-DRVD-IND  PIC S9(04) COMP VALUE 0.        00042000
042600        03 NAES-IO-OBCM-DRVD-CHR  PIC X(01) VALUE SPACE.          00042000
042700        03                        PIC  X(01).                     00042100
042800           88 NAES-IO-OBCM-YES                   VALUE 'Y'.       00042200
042900           88 NAES-IO-OBCM-NO                    VALUE 'N'.       00042300
043000                                                                  00042400
043100        03 NAES-IO-OBCS           PIC  X(06)     VALUE 'OBCS W'.  00042500
043200        03 NAES-IO-OBCS-DRVD-IND  PIC S9(04) COMP VALUE 0.        00042600
043200        03 NAES-IO-OBCS-DRVD-CHR  PIC X(01) VALUE SPACE.          00042600
043300        03                        PIC  X(01).                     00042700
043400           88 NAES-IO-OBCS-YES                   VALUE 'Y'.       00042800
043500           88 NAES-IO-OBCS-NO                    VALUE 'N'.       00042900
043600                                                                  00043000
043700        03 NAES-IO-OBND           PIC  X(06)     VALUE 'OBND W'.  00043100
043800        03 NAES-IO-OBND-DRVD-IND  PIC S9(04) COMP VALUE 0.        00043200
043800        03 NAES-IO-OBND-DRVD-CHR  PIC X(01) VALUE SPACE.          00043200
043900        03                        PIC  X(01).                     00043300
044000           88 NAES-IO-OBND-YES                   VALUE 'Y'.       00043400
044100           88 NAES-IO-OBND-NO                    VALUE 'N'.       00043500
044200                                                                  00043600
044300        03 NAES-IO-OBNM           PIC  X(06)     VALUE 'OBNM W'.  00043700
044400        03 NAES-IO-OBNM-DRVD-IND  PIC S9(04) COMP VALUE 0.        00043800
044400        03 NAES-IO-OBNM-DRVD-CHR  PIC X(01) VALUE SPACE.          00043800
044500        03                        PIC  X(01).                     00043900
044600           88 NAES-IO-OBNM-YES                   VALUE 'Y'.       00044000
044700           88 NAES-IO-OBNM-NO                    VALUE 'N'.       00044100
044800                                                                  00044200
044900        03 NAES-IO-OBNS           PIC  X(06)     VALUE 'OBNS W'.  00044300
045000        03 NAES-IO-OBNS-DRVD-IND  PIC S9(04) COMP VALUE 0.        00044400
045000        03 NAES-IO-OBNS-DRVD-CHR  PIC X(01) VALUE SPACE.          00044400
045100        03                        PIC  X(01).                     00044500
045200           88 NAES-IO-OBNS-YES                   VALUE 'Y'.       00044600
045300           88 NAES-IO-OBNS-NO                    VALUE 'N'.       00044700
045400                                                                  00044800
046100        03 NAES-IO-PMTO           PIC  X(06)     VALUE 'PMTO B'.  00044900
046200        03 NAES-IO-PMTO-DRVD-IND  PIC S9(04) COMP VALUE 0.        00045000
046200        03 NAES-IO-PMTO-DRVD-CHR  PIC X(01) VALUE SPACE.          00045000
046300        03                        PIC  X(01).                     00045100
046400           88 NAES-IO-PMTO-YES                   VALUE 'Y'.       00045200
046500           88 NAES-IO-PMTO-NO                    VALUE 'N'.       00045300
045400                                                                  00044800
046100        03 NAES-IO-PRSO           PIC  X(06)     VALUE 'PRAO B'.  00044900
046200        03 NAES-IO-PRSO-DRVD-IND  PIC S9(04) COMP VALUE 0.        00045000
046200        03 NAES-IO-PRSO-DRVD-CHR  PIC X(01) VALUE SPACE.          00045000
046300        03                        PIC  X(01).                     00045100
046400           88 NAES-IO-PRSO-YES                   VALUE 'Y'.       00045200
046500           88 NAES-IO-PRSO-NO                    VALUE 'N'.       00045300
046600                                                                  00045400
045500        03 NAES-IO-PSYO           PIC  X(06)     VALUE 'PSYO W'.  00045500
045600        03 NAES-IO-PSYO-DRVD-IND  PIC S9(04) COMP VALUE 0.        00045600
045600        03 NAES-IO-PSYO-DRVD-CHR  PIC X(01) VALUE SPACE.          00045600
045700        03                        PIC  X(01).                     00045700
045800           88 NAES-IO-PSYO-YES                   VALUE 'Y'.       00045800
045900           88 NAES-IO-PSYO-NO                    VALUE 'N'.       00045900
046000                                                                  00046000
046700        03 NAES-IO-SPTO           PIC  X(06)     VALUE 'SPTO B'.  00046100
046800        03 NAES-IO-SPTO-DRVD-IND  PIC S9(04) COMP VALUE 0.        00046200
046800        03 NAES-IO-SPTO-DRVD-CHR  PIC X(01) VALUE SPACE.          00046200
046900        03                        PIC  X(01).                     00046300
047000           88 NAES-IO-SPTO-YES                   VALUE 'Y'.       00046400
047100           88 NAES-IO-SPTO-NO                    VALUE 'N'.       00046500
047200                                                                  00046600
047300        03 NAES-IO-XRYO           PIC  X(06)     VALUE 'XRYO B'.  00046700
047400        03 NAES-IO-XRYO-DRVD-IND  PIC S9(04) COMP VALUE 0.        00046800
047400        03 NAES-IO-XRYO-DRVD-CHR  PIC X(01) VALUE SPACE.          00046800
047500        03                        PIC  X(01).                     00046900
047600           88 NAES-IO-XRYO-YES                   VALUE 'Y'.       00047000
047700           88 NAES-IO-XRYO-NO                    VALUE 'N'.       00047100
048900                                                                  00047200
049000     02 NAES-PROFESSIONAL-OP-BEN-PROV.                            00047300
049100        03 NAES-PO-TABLE-MAX     PIC S9(04) COMP  VALUE 25.       00047400
049200                                                                  00047500
049300        03 NAES-PO-AMB            PIC  X(06)     VALUE 'AMB  E'.  00047600
049400        03 NAES-PO-AMB-DRVD-IND   PIC S9(04) COMP VALUE 0.        00047700
049400        03 NAES-PO-AMB-DRVD-CHR   PIC X(01) VALUE SPACE.          00047700
049500        03                        PIC  X(01).                     00047800
049600           88 NAES-PO-AMB-YES                    VALUE 'Y'.       00047900
049700           88 NAES-PO-AMB-NO                     VALUE 'N'.       00048000
049800                                                                  00048100
049900        03 NAES-PO-ASOP           PIC  X(06)     VALUE 'ASOP E'.  00048200
050000        03 NAES-PO-ASOP-DRVD-IND  PIC S9(04) COMP VALUE 0.        00048300
050000        03 NAES-PO-ASOP-DRVD-CHR  PIC X(01) VALUE SPACE.          00048300
050100        03                        PIC  X(01).                     00048400
050200           88 NAES-PO-ASOP-YES                   VALUE 'Y'.       00048500
050300           88 NAES-PO-ASOP-NO                    VALUE 'N'.       00048600
050400                                                                  00048700
050500        03 NAES-PO-CHCVD         PIC  X(06)      VALUE 'CHCV D'.  00048800
050600        03 NAES-PO-CHCV-DRVD-IND PIC S9(04) COMP VALUE 0.         00048900
050600        03 NAES-PO-CHCV-DRVD-CHR PIC X(01) VALUE SPACE.           00048900
050700        03                       PIC  X(01).                      00049000
050800           88 NAES-PO-CHCV-YES                   VALUE 'Y'.       00049100
050900           88 NAES-PO-CHCV-NO                    VALUE 'N'.       00049200
051000                                                                  00049300
051100        03 NAES-PO-DMEO          PIC  X(06)      VALUE 'DMEO E'.  00049400
051200        03 NAES-PO-DMEO-DRVD-IND PIC S9(04) COMP VALUE 0.         00049500
051200        03 NAES-PO-DMEO-DRVD-CHR PIC X(01) VALUE SPACE.           00049500
051300        03                       PIC  X(01).                      00049600
051400           88 NAES-PO-DMEO-YES                   VALUE 'Y'.       00049700
051500           88 NAES-PO-DMEO-NO                    VALUE 'N'.       00049800
051600                                                                  00049900
051100        03 NAES-PO-DMRO          PIC  X(06)      VALUE 'DMRO E'.  00050000
051200        03 NAES-PO-DMRO-DRVD-IND PIC S9(04) COMP VALUE 0.         00050100
051200        03 NAES-PO-DMRO-DRVD-CHR PIC X(01) VALUE SPACE.           00050100
051300        03                       PIC  X(01).                      00050200
051400           88 NAES-PO-DMRO-YES                   VALUE 'Y'.       00050300
051500           88 NAES-PO-DMRO-NO                    VALUE 'N'.       00050400
051600                                                                  00050500
051700        03 NAES-PO-EAC           PIC  X(06)      VALUE 'EAC  E'.  00050600
051800        03 NAES-PO-EAC-DRVD-IND  PIC S9(04) COMP VALUE 0.         00050700
051800        03 NAES-PO-EAC-DRVD-CHR  PIC X(01) VALUE SPACE.           00050700
051900        03                       PIC  X(01).                      00050800
052000           88 NAES-PO-EAC-YES                    VALUE 'Y'.       00050900
052100           88 NAES-PO-EAC-NO                     VALUE 'N'.       00051000
052200                                                                  00051100
052300        03 NAES-PO-EMC           PIC  X(06)      VALUE 'EMC  E'.  00051200
052400        03 NAES-PO-EMC-DRVD-IND  PIC S9(04) COMP VALUE 0.         00051300
052400        03 NAES-PO-EMC-DRVD-CHR  PIC X(01) VALUE SPACE.           00051300
052500        03                       PIC  X(01).                      00051400
052600           88 NAES-PO-EMC-YES                    VALUE 'Y'.       00051500
052700           88 NAES-PO-EMC-NO                     VALUE 'N'.       00051600
052800                                                                  00051700
052900        03 NAES-PO-FOTO          PIC  X(06)      VALUE 'FOTO E'.  00051800
053000        03 NAES-PO-FOTO-DRVD-IND PIC S9(04) COMP VALUE 0.         00051900
053000        03 NAES-PO-FOTO-DRVD-CHR PIC X(01) VALUE SPACE.           00051900
053100        03                       PIC  X(01).                      00052000
053200           88 NAES-PO-FOTO-YES                   VALUE 'Y'.       00052100
053300           88 NAES-PO-FOTO-NO                    VALUE 'N'.       00052200
053400                                                                  00052300
053410        03 NAES-PO-GPO            PIC  X(06)     VALUE 'GPO  E'.  00052400
053420        03 NAES-PO-GPO-DRVD-IND   PIC S9(04) COMP VALUE 0.        00052500
053420        03 NAES-PO-GPO-DRVD-CHR   PIC X(01) VALUE SPACE.          00052500
053430        03                        PIC  X(01).                     00052600
053440           88 NAES-PO-GPO-YES                    VALUE 'Y'.       00052700
053450           88 NAES-PO-GPO-NO                     VALUE 'N'.       00052800
053400                                                                  00052300
053410        03 NAES-PO-HVI            PIC  X(06)     VALUE 'HVIS E'.  00052400
053420        03 NAES-PO-HVI-DRVD-IND   PIC S9(04) COMP VALUE 0.        00052500
053420        03 NAES-PO-HVI-DRVD-CHR   PIC X(01) VALUE SPACE.          00052500
053430        03                        PIC  X(01).                     00052600
053440           88 NAES-PO-HVIO-YES                   VALUE 'Y'.       00052700
053450           88 NAES-PO-HVIO-NO                    VALUE 'N'.       00052800
053460                                                                  00052900
053461        03 NAES-PO-IPO            PIC  X(06)     VALUE 'IPO  E'.  00053000
053462        03 NAES-PO-IPO-DRVD-IND   PIC S9(04) COMP VALUE 0.        00053100
053462        03 NAES-PO-IPO-DRVD-CHR   PIC X(01) VALUE SPACE.          00053100
053463        03                        PIC  X(01).                     00053200
053464           88 NAES-PO-IPO-YES                    VALUE 'Y'.       00053300
053465           88 NAES-PO-IPO-NO                     VALUE 'N'.       00053400
053466                                                                  00053500
053500        03 NAES-PO-LABO          PIC  X(06)      VALUE 'LABO E'.  00053600
053600        03 NAES-PO-LABO-DRVD-IND PIC S9(04) COMP VALUE 0.         00053700
053600        03 NAES-PO-LABO-DRVD-CHR PIC X(01) VALUE SPACE.           00053700
053700        03                       PIC  X(01).                      00053800
053800           88 NAES-PO-LABO-YES                   VALUE 'Y'.       00053900
053900           88 NAES-PO-LABO-NO                    VALUE 'N'.       00054000
053466                                                                  00053500
053500        03 NAES-PO-MSPO          PIC  X(06)      VALUE 'MSSO E'.  00053600
053600        03 NAES-PO-MSPO-DRVD-IND PIC S9(04) COMP VALUE 0.         00053700
053600        03 NAES-PO-MSPO-DRVD-CHR PIC X(01) VALUE SPACE.           00053700
053700        03                       PIC  X(01).                      00053800
053800           88 NAES-PO-MSPO-YES                   VALUE 'Y'.       00053900
053900           88 NAES-PO-MSPO-NO                    VALUE 'N'.       00054000
054000                                                                  00054100
054100        03 NAES-PO-NRSO          PIC  X(06)      VALUE 'NRSO E'.  00054200
054200        03 NAES-PO-NRSO-DRVD-IND PIC S9(04) COMP VALUE 0.         00054300
054200        03 NAES-PO-NRSO-DRVD-CHR PIC X(01) VALUE SPACE.           00054300
054300        03                       PIC  X(01).                      00054400
054400           88 NAES-PO-NRSO-YES                   VALUE 'Y'.       00054500
054500           88 NAES-PO-NRSO-NO                    VALUE 'N'.       00054600
054600                                                                  00054700
054700        03 NAES-PO-OBCD          PIC  X(06)      VALUE 'OBCD C'.  00054800
054800        03 NAES-PO-OBCD-DRVD-IND PIC S9(04) COMP VALUE 0.         00054900
054800        03 NAES-PO-OBCD-DRVD-CHR PIC X(01) VALUE SPACE.           00054900
054900        03                       PIC  X(01).                      00055000
055000           88 NAES-PO-OBCD-YES                   VALUE 'Y'.       00055100
055100           88 NAES-PO-OBCD-NO                    VALUE 'N'.       00055200
055200                                                                  00055300
055300        03 NAES-PO-OBCM          PIC  X(06)      VALUE 'OBCM C'.  00055400
055400        03 NAES-PO-OBCM-DRVD-IND PIC S9(04) COMP VALUE 0.         00055500
055400        03 NAES-PO-OBCM-DRVD-CHR PIC X(01) VALUE SPACE.           00055500
055500        03                       PIC  X(01).                      00055600
055600           88 NAES-PO-OBCM-YES                   VALUE 'Y'.       00055700
055700           88 NAES-PO-OBCM-NO                    VALUE 'N'.       00055800
055800                                                                  00055900
055900        03 NAES-PO-OBCS          PIC  X(06)      VALUE 'OBCS C'.  00056000
056000        03 NAES-PO-OBCS-DRVD-IND PIC S9(04) COMP VALUE 0.         00056100
056000        03 NAES-PO-OBCS-DRVD-CHR PIC X(01) VALUE SPACE.           00056100
056100        03                       PIC  X(01).                      00056200
056200           88 NAES-PO-OBCS-YES                   VALUE 'Y'.       00056300
056300           88 NAES-PO-OBCS-NO                    VALUE 'N'.       00056400
056400                                                                  00056500
056500        03 NAES-PO-OBND          PIC  X(06)      VALUE 'OBND C'.  00056600
056600        03 NAES-PO-OBND-DRVD-IND PIC S9(04) COMP VALUE 0.         00056700
056600        03 NAES-PO-OBND-DRVD-CHR PIC X(01) VALUE SPACE.           00056700
056700        03                       PIC  X(01).                      00056800
056800           88 NAES-PO-OBND-YES                   VALUE 'Y'.       00056900
056900           88 NAES-PO-OBND-NO                    VALUE 'N'.       00057000
057000                                                                  00057100
057100        03 NAES-PO-OBNM          PIC  X(06)      VALUE 'OBNM C'.  00057200
057200        03 NAES-PO-OBNM-DRVD-IND PIC S9(04) COMP VALUE 0.         00057300
057200        03 NAES-PO-OBNM-DRVD-CHR PIC X(01) VALUE SPACE.           00057300
057300        03                       PIC  X(01).                      00057400
057400           88 NAES-PO-OBNM-YES                   VALUE 'Y'.       00057500
057500           88 NAES-PO-OBNM-NO                    VALUE 'N'.       00057600
057600                                                                  00057700
057700        03 NAES-PO-OBNS          PIC  X(06)      VALUE 'OBNS C'.  00057800
057800        03 NAES-PO-OBNS-DRVD-IND PIC S9(04) COMP VALUE 0.         00057900
057800        03 NAES-PO-OBNS-DRVD-CHR PIC X(01) VALUE SPACE.           00057900
057900        03                       PIC  X(01).                      00058000
058000           88 NAES-PO-OBNS-YES                   VALUE 'Y'.       00058100
058100           88 NAES-PO-OBNS-NO                    VALUE 'N'.       00058200
058200                                                                  00058300
058300        03 NAES-PO-OVIS          PIC  X(06)      VALUE 'OVIS E'.  00058400
058400        03 NAES-PO-OVIS-DRVD-IND PIC S9(04) COMP VALUE 0.         00058500
058400        03 NAES-PO-OVIS-DRVD-CHR PIC X(01) VALUE SPACE.           00058500
058500        03                       PIC  X(01).                      00058600
058600           88 NAES-PO-OVIS-YES                   VALUE 'Y'.       00058700
058700           88 NAES-PO-OVIS-NO                    VALUE 'N'.       00058800
058800                                                                  00058900
058900        03 NAES-PO-PMTO          PIC  X(06)      VALUE 'PMTO E'.  00059000
059000        03 NAES-PO-PMTO-DRVD-IND PIC S9(04) COMP VALUE 0.         00059100
059000        03 NAES-PO-PMTO-DRVD-CHR PIC X(01) VALUE SPACE.           00059100
059100        03                       PIC  X(01).                      00059200
059200           88 NAES-PO-PMTO-YES                   VALUE 'Y'.       00059300
059300           88 NAES-PO-PMTO-NO                    VALUE 'N'.       00059400
058800                                                                  00058900
058900        03 NAES-PO-PRSO          PIC  X(06)      VALUE 'PRAO E'.  00059000
059000        03 NAES-PO-PRSO-DRVD-IND PIC S9(04) COMP VALUE 0.         00059100
059000        03 NAES-PO-PRSO-DRVD-CHR PIC X(01) VALUE SPACE.           00059100
059100        03                       PIC  X(01).                      00059200
059200           88 NAES-PO-PRSO-YES                   VALUE 'Y'.       00059300
059300           88 NAES-PO-PRSO-NO                    VALUE 'N'.       00059400
059400                                                                  00059500
059500        03 NAES-PO-SPTO          PIC  X(06)      VALUE 'SPTO E'.  00059600
059600        03 NAES-PO-SPTO-DRVD-IND PIC S9(04) COMP VALUE 0.         00059700
059600        03 NAES-PO-SPTO-DRVD-CHR PIC X(01) VALUE SPACE.           00059700
059700        03                       PIC  X(01).                      00059800
059800           88 NAES-PO-SPTO-YES                   VALUE 'Y'.       00059900
059900           88 NAES-PO-SPTO-NO                    VALUE 'N'.       00060000
060000                                                                  00060100
060100        03 NAES-PO-XRYO          PIC  X(06)      VALUE 'XRYO E'.  00060200
060200        03 NAES-PO-XRYO-DRVD-IND PIC S9(04) COMP VALUE 0.         00060300
060200        03 NAES-PO-XRYO-DRVD-CHR PIC X(01) VALUE SPACE.           00060300
060300        03                       PIC  X(01).                      00060400
060400           88 NAES-PO-XRYO-YES                   VALUE 'Y'.       00060500
060500           88 NAES-PO-XRYO-NO                    VALUE 'N'.       00060600
