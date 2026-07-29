000100******************************************************************00010003
000200*                                                                *00020003
000300*    COPYBOOK:   ELSATBLC                                        *00030003
000400*    DATE:       21-JUN-1988                                     *00040003
000500*    AUTHOR:     NINA A. CERVANTES                               *00050003
000510*    FUNCTION:   COPYBOOK USED WITHIN THE OVERALL ACCUM          *00060003
000520*                DETERMINATION SUBSYSTEM FOR ELIQ.               *00070003
000530*                EACH ACCUM WILL ITS OWN AREA CONTAINING THIS    *00080003
000540*                SAME LAYOUT.                                    *00090003
000550*                                                                *00100003
000560******************************************************************00110003
000570*                                                                *00120003
000580*                      MAINTENANCE HISTORY                       *00130003
000590*                                                                *00140003
000600*  MOD     DATE     BY  DRPT                ACTION               *00150003
000610* ----- ----------- --- ----- ---------------------------------- *00160003
000620* 01.00 21-JUN-1988 NAC       CREATED                            *00170003
000630*                                                                *00180003
000640* 01.03 01-JUL-1992 BAK       CHANGE IPGD TO IDGD, INCREASE      *00190004
000650*                             CONDITION CODE BITS TO 30 BYTES.   *00200003
000660*                             ADD LIFE THREATENING BIT.          *00210003
000670*                             ADD ATBL-CF-OV-DIAG-CODE FOR       *00220003
000680*                             DIAGNOSIS GRP BY DIAG CODE &       *00230003
000690*                             ADD ATBL-CF-OV-PROC-GRP FOR        *00240003
000700*                             PROCEDURE GROUP TABULARS.          *00250003
000710*                                                                *00260003
000720* 01.04 23-NOV-1992 RJL       ADDED CONFIDENCE FACTORS NEEDED BY *00270012
000730*                             PROCESSES REVISED/CREATED FOR      *00280004
000740*                             NA/ES PROCESS (TO BE INCORPORATED  *00290004
000750*                             INTO CONTRACT SUMMARY LATER).      *00300004
000760*                             REVISED LAYOUT FOR EASIER READING. *00310004
000770*                                                                *00320004
000720* 01.05 15-DEC-1992 BAK       CORRECTED SEQUENCE BETWEEN CF AND  *00320005
000770*                             SP CONFIDENCE FACTORS.             *00320006
000770*                                                                *00320007
000720* 01.06 07-SEP-2000 AKK       ADDED CONFIDENC FACTOR ENTRY FOR   *00320005
000770*                             PROVIDER SPECIALTY IN SUPPORT OF   *00320006
000770*                             #IPGS TABULAR.                     *00320007
000770*                                                                *00320007
000720* 01.07 22-SEP-2000  JP       ADDED CDE FIELDS NEEDED TO         *00320005
000770*                             SUPPORT REALMED PROCESSING.        *00320006
000770*                                                                *00320007
000780******************************************************************00330003
000790 01  ATBL-ACCUMULATOR-TABLE.                                      00340003
000800     02 ATBL-TBL-CNT                           PIC S9(04) COMP.   00350007
000810        88  ATBL-TBL-FULL                          VALUE +0150.   00360008
000820                                                                  00370008
000830     02 ATBL-ACCUMULATOR                    OCCURS 1 TO 150 TIMES 00380008
000840                                               DEPENDING ON       00390008
000850                                                  ATBL-TBL-CNT    00400008
000860                                               INDEXED BY         00410008
000870                                                  ATBL-IDX        00411010
000880                                                  ATBL-MAX-IDX    00411110
000890                                                  ATBL-X-IDX.     00412010
000900                                                                  00420007
000910        03 ATBL-SLOT-NUMBER                    PIC S9(07) COMP-3. 00430007
000920        03 ATBL-INTERNAL-TABULARS.                                00440003
000930           04 ATBL-IBGR-SLOT-NUMBER            PIC S9(07) COMP-3. 00450007
000940           04 ATBL-IDGD-SLOT-NUMBER            PIC S9(07) COMP-3. 00460007
000950           04 ATBL-IPGN-SLOT-NUMBER            PIC S9(07) COMP-3. 00470007
000960           04 ATBL-IPGP-SLOT-NUMBER            PIC S9(07) COMP-3. 00480007
000970           04 ATBL-IPGT-SLOT-NUMBER            PIC S9(07) COMP-3. 00490007
000970           04 ATBL-IPGS-SLOT-NUMBER            PIC S9(07) COMP-3. 00490007
000980                                                                  00500007
000990        03 ATBL-DATA-ELEMENTS.                                    00510003
001000           04 ATBL-1ST-DOLR-COVRGE-LMT         PIC X(01).         00520007
001010           04 ATBL-ASCEND-DESCEND-IND          PIC X(01).         00530007
001020           04 ATBL-BEN-PER-MAX-OVRD-IND        PIC X(01).         00540007
001030           04 ATBL-BEN-PER-TIME-FCTR           PIC S9(03) COMP-3. 00550007
001040           04 ATBL-BEN-PER-TIME-QUAL           PIC X(01).         00560007
001050           04 ATBL-BENEFIT-PERIOD              PIC X(02).         00570007
001060           04 ATBL-BISCEND-IND                 PIC X(01).         00580007
001070           04 ATBL-CARRY-OVER-CREDIT-IND       PIC X(01).         00590007
001080           04 ATBL-CLAIM-LVL-ACCUM-IND         PIC X(01).         00600007
001090           04 ATBL-CO-PAY-IND                  PIC X(01).         00610007
001030           04 ATBL-AGE-LIMIT-FROM              PIC S9(03) COMP-3. 00550007
001030           04 ATBL-AGE-LIMIT-TO                PIC S9(03) COMP-3. 00550007
001090           04 ATBL-AGE-QUAL-FROM               PIC X(01).         00610007
001090           04 ATBL-AGE-QUAL-TO                 PIC X(01).         00610007
001100                                                                  00620007
001110           04 ATBL-CONDITION.                                     00630003
001120              05 ATBL-COND-ALL-BIT             PIC X(01).         00640007
001130              05 ATBL-COND-EXCLUSION-BIT       PIC X(01).         00650007
001140              05 ATBL-COND-ICD-BIT             PIC X(01).         00660007
001150              05 ATBL-COND-TB-BIT              PIC X(01).         00670007
001160              05 ATBL-COND-MENTAL-BIT          PIC X(01).         00680007
001170              05 ATBL-COND-DRUG-BIT            PIC X(01).         00690008
001180              05 ATBL-COND-ALCOHOL-BIT         PIC X(01).         00700007
001190              05 ATBL-COND-OB-COMP-BIT         PIC X(01).         00710007
001200              05 ATBL-COND-OB-NORM-BIT         PIC X(01).         00720007
001210              05 ATBL-COND-MALIGNANCY-BIT      PIC X(01).         00730007
001220              05 ATBL-COND-CARDIAC-DISEASE-BIT PIC X(01).         00740007
001230              05 ATBL-COND-OBESITY-BIT         PIC X(01).         00750007
001240              05 ATBL-COND-KIDNEY-DISEASE-BIT  PIC X(01).         00760007
001250              05 ATBL-COND-ACCIDENT-BIT        PIC X(01).         00770007
001260              05 ATBL-COND-PRE-EXIST-BIT       PIC X(01).         00780007
001270              05 ATBL-COND-NON-EMER-BIT        PIC X(01).         00790007
001280              05 ATBL-COND-SUICIDE-BIT         PIC X(01).         00800007
001290              05 ATBL-COND-TMJ-BIT             PIC X(01).         00810007
001300              05 ATBL-COND-INF-BIT             PIC X(01).         00820007
001310              05 ATBL-COND-LIFE-THREAT-BIT     PIC X(01).         00830007
001320              05 FILLER                        PIC X(10).         00840007
001330                                                                  00850007
001340           04 ATBL-COST-CONTAIN-IND            PIC X(02).         00860007
001350           04 ATBL-DAY-FACTOR-IND              PIC X(01).         00870007
001360           04 ATBL-DEFINITION                  PIC X(01).         00880007
001370           04 ATBL-FAM-OR-INDIV                PIC X(01).         00890007
001380           04 ATBL-FYI-VALUE                   PIC X(03).         00900007
001390           04 ATBL-INTERNAL-DESCRIPTOR         PIC X(09).         00910007
001400           04 ATBL-INTERVAL-OVRD-IND           PIC X(01).         00920007
001410           04 ATBL-INTERVAL-OVRD-VALUE         PIC S9(05) COMP-3. 00930007
001420           04 ATBL-INTERVAL-TIME-FCTR          PIC S9(03) COMP-3. 00940007
001430           04 ATBL-INTERVAL-TYPE               PIC X(02).         00950007
001440           04 ATBL-L-O-B                       PIC X(01).         00960007
001450           04 ATBL-MANDATORY-IND               PIC X(01).         00970007
001460           04 ATBL-PERCENT-LEVEL               PIC S9(03) COMP-3. 00980007
001470           04 ATBL-PLACE-OF-TREATMENT          PIC X(02).         00990007
001480           04 ATBL-REINSTATEMENT-IND           PIC X(01).         01000007
001490           04 ATBL-SERVICE-GROUP               PIC X(02).         01010007
001500           04 ATBL-VALUE-LIMIT                 PIC S9(07)V9(02)   01020007
001510                                                          COMP-3. 01030008
001520           04 ATBL-VALUE-LIMIT-INTGR   REDEFINES ATBL-VALUE-LIMIT 01031013
001530                                               PIC S9(09) COMP-3. 01033013
001540           04 ATBL-VALUE-QUALIFIER             PIC X(01).         01040007
001550                                                                  01041008
001560        03 ATBL-ATTR-CONFIDENCE-FACTORS.                          01050003
001570           04 ATBL-CF-BNFT-PRD                   COMP-1.          01060012
001580           04 ATBL-CF-BENPERD                                     01070007
001590                REDEFINES ATBL-CF-BNFT-PRD       COMP-1.          01080012
001600           04 ATBL-CF-ANL                        COMP-1.          01090012
001610           04 ATBL-CF-LFTM                       COMP-1.          01100012
001620           04 ATBL-CF-FMLY                       COMP-1.          01110012
001630           04 ATBL-CF-FAM                                         01120007
001640                REDEFINES ATBL-CF-FMLY           COMP-1.          01130012
001650           04 ATBL-CF-INDVDL                     COMP-1.          01140012
001660           04 ATBL-CF-INDIV                                       01150007
001670                REDEFINES ATBL-CF-INDVDL         COMP-1.          01160012
001680           04 ATBL-CF-INST-BAS                   COMP-1.          01170012
001690           04 ATBL-CF-INST                                        01180007
001700                REDEFINES ATBL-CF-INST-BAS       COMP-1.          01190012
001710           04 ATBL-CF-INST-SUP                   COMP-1.          01200012
001720           04 ATBL-CF-BAS                                         01210007
001730                REDEFINES ATBL-CF-INST-SUP       COMP-1.          01220012
001740           04 ATBL-CF-PROF-BAS                   COMP-1.          01230012
001750           04 ATBL-CF-PROF                                        01240007
001760                REDEFINES ATBL-CF-PROF-BAS       COMP-1.          01250012
001770           04 ATBL-CF-PROF-SUP                   COMP-1.          01260012
001780           04 ATBL-CF-SUP                                         01270007
001790                REDEFINES ATBL-CF-PROF-SUP       COMP-1.          01280012
001800           04 ATBL-CF-IP                         COMP-1.          01290012
001810           04 ATBL-CF-OP                         COMP-1.          01300012
001820           04 ATBL-CF-PLAN                       COMP-1.          01310012
001830           04 ATBL-CF-NON-PLAN                   COMP-1.          01320012
001840                                                                  01320112
001850           04 ATBL-CF-OV-FCTRS.                                   01320212
001860             05 ATBL-CF-OV                       COMP-1.          01320312
001870             05 ATBL-CF-OV-BNFT-PRVSN            COMP-1.          01321012
001880             05 ATBL-CF-OV-BEN-PROVN                              01330012
001890                REDEFINES ATBL-CF-OV-BNFT-PRVSN  COMP-1.          01331012
001900             05 ATBL-CF-OV-CNDTN-BTS             COMP-1.          01340012
001910             05 ATBL-CF-OV-CST-CNTNMT            COMP-1.          01350012
001920             05 ATBL-CF-OV-DGNSS                 COMP-1.          01351012
001930             05 ATBL-CF-OV-INTRNL-DSCRPTR        COMP-1.          01360012
001940             05 ATBL-CF-OV-PLC-TRTMNT            COMP-1.          01370012
001950             05 ATBL-CF-OV-PRCDR                 COMP-1.          01380012
001960             05 ATBL-CF-OV-PRVDR-NBR             COMP-1.          01430012
001970             05 ATBL-CF-OV-PROV-NBR                               01440012
001980                REDEFINES ATBL-CF-OV-PRVDR-NBR COMP-1.            01450007
001990             05 ATBL-CF-OV-PRVDR-TYP             COMP-1.          01460012
002000             05 ATBL-CF-OV-PROV-TYPE                              01470012
002010                REDEFINES ATBL-CF-OV-PRVDR-TYP COMP-1.            01480007
001990             05 ATBL-CF-OV-PRVDR-SPC             COMP-1.          01460012
002000             05 ATBL-CF-OV-PROV-SPEC                              01470012
002010                REDEFINES ATBL-CF-OV-PRVDR-SPC COMP-1.            01480007
002020             05 ATBL-CF-OV-SRVC-GRP              COMP-1.          01500112
002030             05 ATBL-CF-OV-VL-QLFR               COMP-1.          01500212
002040                                                                  01500312
002050           04 ATBL-CF-SP-FCTRS.                                   01500412
002170             05 ATBL-CF-SP                       COMP-1.          01500413
002060             05 ATBL-CF-SP-BNFT-PRVSN            COMP-1.          01511012
002070             05 ATBL-CF-SP-CNDTN-BTS             COMP-1.          01520012
002080             05 ATBL-CF-SP-CST-CNTNMT            COMP-1.          01530012
002090             05 ATBL-CF-SP-DGNSS                 COMP-1.          01531012
002100             05 ATBL-CF-SP-INTRNL-DSCRPTR        COMP-1.          01540012
002110             05 ATBL-CF-SP-PLC-TRTMNT            COMP-1.          01541012
002120             05 ATBL-CF-SP-PRCDR                 COMP-1.          01541112
002130             05 ATBL-CF-SP-PRVDR-NBR             COMP-1.          01542012
002140             05 ATBL-CF-SP-PRVDR-TYP             COMP-1.          01543012
002140             05 ATBL-CF-SP-PRVDR-SPC             COMP-1.          01543012
002150             05 ATBL-CF-SP-SRVC-GRP              COMP-1.          01560012
002160             05 ATBL-CF-SP-VL-QLFR               COMP-1.          01570012
002180                                                                  01621008
002190        03 ATBL-CF-WORK-ENTRY                    COMP-1.          01630012
002200        03 ATBL-KEY-LINKAGE.                                      01640003
002210           04 ATBL-ASCEND-DESCEND.                                01650003
002220              05 ATBL-NEXT-A-D                 PIC S9(04) COMP.   01660008
002230              05 ATBL-PREV-A-D                 PIC S9(04) COMP.   01670008
002240                                                                  01671008
017900           04 ATBL-CONDITION-RELATED.                             01680008
018000              05 ATBL-NEXT-CONDITION           PIC S9(04) COMP.   01690008
018200              05 ATBL-PREV-CONDITION           PIC S9(04) COMP.   01710008
018400              05 ATBL-CONDITION-BITS           PIC X(30).         01730008
002240                                                                  01671008
000800        03 ATBL-RLMED-COMMON-FIELDS.                              00350007
002200           04 ATBL-ACCUM-DESC             PIC X(06).              01640003
002200           04 ATBL-GRP-NBR                PIC X(09).              01640003
002200           04 ATBL-SECT-NBR               PIC X(05).              01640003
002200           04 ATBL-PSEU-NBR-USING-IND     PIC X(01).              01640003
002200           04 ATBL-PSEUDO-GRP-NBR         PIC X(09).              01640003
002200           04 ATBL-PSEUDO-SECT-NBR        PIC X(05).              01640003
002200           04 ATBL-CON-FEAK-IND           PIC X(01).              01640003
002200           04 ATBL-CON-BGN-DT-MMDD        PIC X(04).              01640003
