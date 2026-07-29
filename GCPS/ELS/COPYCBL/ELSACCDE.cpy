000100******************************************************************00010001
000200*                                                                *00020001
000300*    COPYBOOK:   ELSACCDE                                        *00030006
000400*    DATE:       22-SEP-2000                                     *00040001
000500*    AUTHOR:     JUNE PON                                        *00050001
000510*    FUNCTION:   COPYBOOK USED TO PROCESS ACCUMULATOR            *00060006
000520*                CDE DATA.  THIS IS A CLONE OF                   *00070006
000530*                THE TABULAR PORTION OF COPYBOOK MEMBER          *00080001
000540*                PMCACCUM.                                       *00090001
000550*                                                                *00100001
000560******************************************************************00110001
000570*                                                                *00120001
000580*                      MAINTENANCE HISTORY                       *00130001
000590*                                                                *00140001
000600*  MOD     DATE     BY  DRPT                ACTION               *00150001
000610* ----- ----------- --- ----- ---------------------------------- *00160001
000620* 01.00 22-SEP-2000 JP        CREATED                            *00170001
000630*                                                                *00180001
000640*                                                                *00190001
000650*                                                                *00200001
000660*                                                                *00210001
000670*                                                                *00220001
000680*                                                                *00230001
000780******************************************************************00240001
000790 01  ACCDE-ATBL-ACCUMULATOR-TABLE.                                00250006
000800     10 AC-ATBL-TBL-CNT                    PIC S9(04) COMP.       00260006
000810        88  AC-ATBL-TBL-FULL                  VALUE +0099.        00270006
000820                                                                  00280001
000830     10 AC-ATBL-ACCUMULATOR              OCCURS 1 TO  99 TIMES    00290006
000840                                             DEPENDING ON         00300001
000850                                             AC-ATBL-TBL-CNT      00310006
000860                                             INDEXED BY           00320001
000870                                             AC-ATBL-IDX          00330006
000880                                             AC-ATBL-MAX-IDX      00340006
000890                                             AC-ATBL-X-IDX.       00350006
000900                                                                  00360001
004400        20  AC-ATBL-ACCUM-DESC             PIC X(6).              00370006
004500        20  AC-ATBL-PSEUDO-GRP-NO          PIC X(9).              00380006
004600        20  AC-ATBL-PSEUDO-SEC-NO          PIC X(5).              00390006
004700        20  AC-ATBL-CON-FEAK-IND           PIC X(1).              00400006
004800        20  AC-ATBL-CON-BGN-DT-MMDD        PIC X(4).              00410006
004900        20  AC-ATBL-MANDATORY-IND          PIC X(1).              00420006
005000        20  AC-ATBL-BENEFIT-PERIOD         PIC X(2).              00430006
005100        20  AC-ATBL-FAM-OR-INDIV           PIC X(1).              00440006
005200        20  AC-ATBL-L-O-B                  PIC X(1).              00450006
005300        20  AC-ATBL-INTERNAL-DESC          PIC X(9).              00460006
005400        20  AC-ATBL-SERVICE-GROUP          PIC X(2).              00470006
005500        20  AC-ATBL-P-O-T                  PIC X(2).              00480006
005600        20  AC-ATBL-CONDITION.                                    00490006
005700            25  AC-ATBL-COND-ALL-BIT        PIC X(1).             00500006
005900            25  AC-ATBL-COND-EXCLUSION-BIT  PIC X(1).             00510006
006100            25  AC-ATBL-COND-ICD-BIT        PIC X(1).             00520006
006300            25  AC-ATBL-COND-TB-BIT         PIC X(1).             00530006
006400            25  AC-ATBL-COND-MENTAL-BIT     PIC X(1).             00540006
006600            25  AC-ATBL-COND-DRUG-BIT       PIC X(1).             00550006
006800            25  AC-ATBL-COND-ALCOHOL-BIT    PIC X(1).             00560006
007000            25  AC-ATBL-COND-OB-COMP-BIT    PIC X(1).             00570006
007200            25  AC-ATBL-COND-OB-NOAC-BIT    PIC X(1).             00580006
007400            25  AC-ATBL-COND-MALIGNANCY-BIT PIC X(1).             00590006
007600            25  AC-ATBL-COND-CARDIAC-DIS-BIT  PIC X(1).           00600006
007800            25  AC-ATBL-COND-OBESITY-BIT    PIC X(1).             00610006
008000            25  AC-ATBL-COND-KIDNEY-DIS-BIT   PIC X(1).           00620006
008200            25  AC-ATBL-COND-ACCIDENT-BIT    PIC X(1).            00630006
008400            25  AC-ATBL-COND-PRE-EXIST-BIT   PIC X(1).            00640006
008600            25  AC-ATBL-COND-NON-EMER-BIT    PIC X(1).            00650006
008800            25  AC-ATBL-COND-SUICIDE-BIT     PIC X(1).            00660006
009000            25  AC-ATBL-COND-TMJ-BIT         PIC X(1).            00670006
009200            25  AC-ATBL-COND-INF-BIT         PIC X(1).            00680006
009400            25  AC-ATBL-COND-LIFE-THREAT-BIT PIC X(1).            00690006
009600            25  AC-ATBL-COND-FILLER-BIT      PIC X(10).           00700006
009700                                                                  00710001
009800        20  AC-ATBL-BISCENDING-IND           PIC X(1).            00720006
009900        20  AC-ATBL-CO-PAY-IND               PIC X(1).            00730006
010000        20  AC-ATBL-COST-CONT-IND            PIC X(2).            00740006
010100        20  AC-ATBL-AGE-LIMIT-FROM   PIC S9(3) COMP-3.            00750006
010200        20  AC-ATBL-AGE-LIMIT-TO     PIC S9(3) COMP-3.            00760006
010300        20  AC-ATBL-AGE-QUAL-FROM            PIC X(1).            00770006
010400        20  AC-ATBL-AGE-QUAL-TO              PIC X(1).            00780006
010500        20  AC-ATBL-PERCENT-LEVEL    PIC S9(3) COMP-3.            00790006
010600        20  AC-ATBL-VALUE-QUALIFIER          PIC X(1).            00800006
010700*                                                                 00810001
010800        20  AC-ATBL-FILLER        PIC S9(7)V99 COMP-3.            00820006
010700*                                                                 00830001
010700*                                                                 00840001
010900************* END OF ELSACCDE *********************************** 00850006
