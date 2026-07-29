000100******************************************************************00010000
000200*                                                                *00020000
000300*    COPYBOOK:   ELSCSBPC                                        *00030000
000400*    DATE:       30-DEC-1987                                     *00040000
000500*    AUTHOR:     NINA A. CERVANTES                               *00050000
000600*    FUNCTION:   BENEFIT PROVISION MATRIX FOR CONTRACT SUMMARY   *00060000
000700*                WHICH CONTAINS PAYMENT LEVEL INFORMATION.       *00070000
000800*                                                                *00080000
000900******************************************************************00090000
001000*                                                                *00100000
001100*                      MAINTENANCE HISTORY                       *00110000
001200*                                                                *00120000
001300*  MOD     DATE     BY  DRPT                ACTION               *00130000
001400* ----- ----------- --- ----- ---------------------------------- *00140000
004700* 07.00 01-JUL-1992 BAK        ADD CSBP-DAYS-BTWN-LIFE-THREAT    *00470011
004800*                              FOR LIFE THREATENING CONDITION    *00480010
004900******************************************************************00490000
005000 01  CSBP-BENEFIT-PROVISION-TABLE.                                00500000
005100     03  CSBP-FIXED-PORTION.                                      00510000
005200         05  CSBP-SUBTOPIC                  PIC X(03).            00520000
005300             88  CSBP-HOSPITAL-ST      VALUE 'IHS'.               00530000
005400             88  CSBP-PHYSICIAN-ST     VALUE 'IPS'.               00540000
005500             88  CSBP-OUTPATIENT-ST    VALUE 'OPS'.               00550000
005600             88  CSBP-OB-STERILIZE-ST  VALUE 'OBS'.               00560000
005700             88  CSBP-PSYCHIATRIC-ST   VALUE 'PSY'.               00570000
005800         05  CSBP-GROUP-CNT                 PIC S9(04) COMP.      00580000
005900         05  CSBP-TBL-CNT                   PIC S9(04) COMP.      00590000
006000             88  CSBP-TBL-FULL                 VALUE +0050.       00600000
006100     03  CSBP-BENEFIT-PROVISION-ENTRY   OCCURS 1 TO 50 TIMES      00610000
006200                                        DEPENDING ON CSBP-TBL-CNT 00620000
006300                                        INDEXED BY CSBP-X-IDX.    00630000
006400         05  CSBP-REQUEST-PARAMETERS.                             00640000
006500             10  CSBP-BP-KEY.                                     00650000
006600                 15  CSBP-BP-ID              PIC X(05).           00660000
006700                 15  CSBP-BP-ID-FORMAT       PIC X(01).           00670000
006800                     88  CSBP-FORMAT-A         VALUE 'A'.         00680000
006900                     88  CSBP-FORMAT-B         VALUE 'B'.         00690000
007000                     88  CSBP-FORMAT-C         VALUE 'C'.         00700000
007100                     88  CSBP-FORMAT-D         VALUE 'D'.         00710000
007200                     88  CSBP-FORMAT-E         VALUE 'E'.         00720000
007300                     88  CSBP-FORMAT-W         VALUE 'W'.         00730000
007400             10  CSBP-L-O-B                  PIC X(01).           00740000
007500             10  CSBP-BP-ACCUM-SLOTS.                             00750000
007600                 15  CSBP-BP-ABM-SLOT        PIC S9(07) COMP-3.   00760000
007700                 15  CSBP-BP-ACL-SLOT        PIC S9(07) COMP-3.   00770000
007800                 15  CSBP-BP-ADL-SLOT        PIC S9(07) COMP-3.   00780000
007900                 15  CSBP-BP-PPF-SLOT        PIC S9(07) COMP-3.   00790000
008000             10  CSBP-PAYMENT-REQ            PIC X(01).           00800000
008100                 88  CSBP-PAYMENT-REQUESTED    VALUE 'Y'.         00810000
008200                 88  CSBP-NO-PAYMENT-REQUESTED VALUE 'N'.         00820000
008300                 88  CSBP-USE-SUPP-INFO        VALUE 'M'.         00830000
008400             10  CSBP-PAYMENT-LVL            PIC S9(04) COMP.     00840000
008500             10  CSBP-SENTENCE-PTR       POINTER.                 00850000
008600             10  CSBP-ABM-ADDN-TEXT          PIC X(01).           00860000
008700                 88  CSBP-ADDITIONAL-ABM-TEXT   VALUE 'Y'.        00870000
008800             10  CSBP-ACL-ADDN-TEXT          PIC X(01).           00880000
008900                 88  CSBP-ADDITIONAL-ACL-TEXT   VALUE 'Y'.        00890000
009000             10  CSBP-ADL-ADDN-TEXT          PIC X(01).           00900000
009100                 88  CSBP-ADDITIONAL-ADL-TEXT   VALUE 'Y'.        00910000
009200             10  CSBP-AOL-ADDN-TEXT          PIC X(01).           00920000
009300                 88  CSBP-ADDITIONAL-AOL-TEXT   VALUE 'Y'.        00930000
009400         05  CSBP-PAYMENT-LEVEL-DATA.                             00940000
009500           07  CSBP-PAYMENT-LEVEL-KEY.                            00950000
009600             10  CSBP-PROVISION-GRP          PIC S9(04) COMP.     00960000
009700             10  CSBP-PROVIDER-CLASS         PIC X(01).           00970000
009800                 88  CSBP-INSTITUTIONAL        VALUE 'I'.         00980000
009900                 88  CSBP-PROFESSIONAL         VALUE 'P'.         00990000
010000             10  CSBP-SERVICE-CLASS          PIC X(01).           01000000
010100                 88  CSBP-INPATIENT            VALUE 'I'.         01010000
010200                 88  CSBP-OUTPATIENT           VALUE 'O'.         01020000
010300                 88  CSBP-BOTH                 VALUE 'B'.         01030000
010400           07  CSBP-RESULTS.                                      01040000
010500             10  CSBP-COVERAGE-IND           PIC X(01).           01050000
010600                 88  CSBP-COVERED              VALUE 'Y'.         01060000
010700                 88  CSBP-NOT-COVERED          VALUE 'N'.         01070000
010800                 88  CSBP-COVERED-ON-SUPP      VALUE 'M'.         01080000
010900             10  CSBP-BP-COMMON.                                  01090000
011000                 15  CSBP-CERTFN-REQRM-IND   PIC X(02).           01100010
011100                 15  CSBP-PROVN-PRICING-METHD                     01110000
011200                                             PIC X(02).           01120000
011300                 15  CSBP-ADDITIONAL-PRICING-PRCNT                01130000
011400                                             PIC S999 COMP-3.     01140000
011500                 15  CSBP-VARIABLE-INDEMNITY-PRCNT                01150000
011600                                             PIC S999 COMP-3.     01160000
011700             10  CSBP-BP-FORMAT.                                  01170000
011800                 15  CSBP-ADDN-ALLOW-AMT-PER-DAY                  01180000
011900                                             PIC S9(03)V99        01190000
012000                                                          COMP-3. 01200000
012100                 15  CSBP-FLAT-RATE-PDM-AMT  PIC S9(5)V99 COMP-3. 01210000
012200                 15  CSBP-MAX-AMT-PER-VISIT  PIC S999V99 COMP-3.  01220000
012300                 15  CSBP-BEN-SCOPE-ID       PIC X(04).           01230000
012400             10  CSBP-ABM.                                        01240000
012500                 15  CSBP-BAMA-OVERALL-SW    PIC X.               01250010
012600                     88  CSBP-BAMA-OVERALL-COVERAGE  VALUE 'Y'.   01260010
012700                 15  CSBP-BAMA-IBGR-SLOT     PIC S9(07)   COMP-3. 01270000
012800                 15  CSBP-BAMA-INFO   OCCURS 5 TIMES              01280000
012900                                      INDEXED BY CSBP-Y-IDX.      01290000
013000                     20  CSBP-BAMA-BENEFIT-PERIOD                 01300000
013100                                             PIC X(02).           01310000
013200                     20  CSBP-BAMA-L-O-B     PIC X(01).           01320000
013300                     20  CSBP-BAMA-PLACE-OF-TREATMENT             01330000
013400                                             PIC X(02).           01340000
013500                     20  CSBP-BAMA-VALUE-LIMIT                    01350000
013600                                             PIC S9(7)V99 COMP-3. 01360000
013700                     20  CSBP-BAMA-VALUE-QUALIFIER                01370000
013800                                             PIC X(01).           01380000
013900             10  CSBP-ACL.                                        01390000
014000                 15  CSBP-COINS-OVERALL-SW   PIC X.               01400010
014100                     88  CSBP-COINS-OVERALL-COVERAGE VALUE 'Y'.   01410010
014200                     88  CSBP-COINS-UNWANTED-SLOT    VALUE 'U'.   01420010
014300                 15  CSBP-COINS-BENEFIT-PERIOD                    01430000
014400                                             PIC X(02).           01440000
014500                 15  CSBP-COINS-L-O-B        PIC X(01).           01450000
014600                 15  CSBP-COINS-PLACE-OF-TREATMENT                01460000
014700                                             PIC X(02).           01470000
014800                 15  CSBP-COINS-PERCENT-LEVEL                     01480000
014900                                             PIC S999 COMP-3.     01490000
015000                 15  CSBP-COINS-VALUE-LIMIT  PIC S9(7)V99 COMP-3. 01500000
015100                 15  CSBP-COINS-VALUE-QUALIFIER                   01510000
015200                                             PIC X(01).           01520000
015300                 15  CSBP-COINS-IBGR-SLOT     PIC S9(07)  COMP-3. 01530000
015400                 15  CSBP-COINS-AD-SUB       PIC S9(04) COMP SYNC.01540000
015500             10  CSBP-ADL.                                        01550000
015600                 15  CSBP-DEDL-OVERALL-SW    PIC X.               01560010
015700                     88  CSBP-DEDL-OVERALL-COVERAGE VALUE 'Y'.    01570010
015800                 15  CSBP-DEDL-BENEFIT-PERIOD                     01580000
015900                                             PIC X(02).           01590000
016000                 15  CSBP-DEDL-L-O-B         PIC X(01).           01600000
016100                 15  CSBP-DEDL-PLACE-OF-TREATMENT                 01610000
016200                                             PIC X(02).           01620000
016300                 15  CSBP-DEDL-VALUE-LIMIT   PIC S9(7)V99 COMP-3. 01630000
016400                 15  CSBP-DEDL-VALUE-QUALIFIER                    01640000
016500                                             PIC X(01).           01650000
016600                 15  CSBP-DEDL-IBGR-SLOT     PIC S9(07)  COMP-3.  01660000
016700             10  CSBP-CONTRACT.                                   01670000
016800                 15  CSBP-DAYS-BTWN-MED-EMRG-TREAT                01680000
016900                                             PIC S999     COMP-3. 01690000
017000                 15  CSBP-DAYS-BTWN-ACCD-EMRG-TREAT               01700000
017100                                             PIC S999     COMP-3. 01710000
017200                 15  CSBP-DAYS-BTWN-LIFE-THREAT                   01720000
017300                                             PIC S999     COMP-3. 01730000
