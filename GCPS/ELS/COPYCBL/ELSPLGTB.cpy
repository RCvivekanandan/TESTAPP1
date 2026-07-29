000100******************************************************************00010000
000200*                                                                *00020000
000300*    COPYBOOK:   ELCDPLT                                         *00030000
000400*    DATE:       29-SEP-1986                                     *00040000
000500*    AUTHOR:     JERRY L. ARKEMA                                 *00050000
000600*    FUNCTION:   PAYMENT LEVE TABLE RETURNED BY ELUPLGRP TO THE  *00060000
000700*                CALLING BENEFIT PROVISION TOPIC PROGRAM.        *00070000
000800*                                                                *00080000
000900******************************************************************00090000
001000*                                                                *00100000
001100*                      MAINTENANCE HISTORY                       *00110000
001200*                                                                *00120000
001300*  MOD     DATE     BY  DRPT                ACTION               *00130000
001400* ----- ----------- --- ----- ---------------------------------- *00140000
001500* 01.00 29-SEP-1986 RJL       RECREATED                          *00150000
001600* 01.01 17-OCT-1986 RJL       ADDED NEW DATA ELEMENT             *00160000
001700* 01.02 24-NOV-1986 NAC       DELETE CERTIFICATION REQUIREMENT   *00170000
001800*                             INDICATOR FROM ALL FORMATS AND ADD *00180000
001900*                             IT TO THE COMMON PORTION.          *00190000
002000* 01.03 30-MAY-1989 RJL       CONVERTED TREATMENT RESTRICTION    *00200000
002100*                             INDICATOR FROM ONE BYTE TO TWO     *00210000
002200*                             BYTES.                             *00220000
002210* 01.03 30-MAY-1989 RJL       CONVERTED TREATMENT RESTRICTION    *00221001
002220*                             INDICATOR FROM ONE BYTE TO TWO     *00222001
002230*                             BYTES.                             *00223001
002240* 01.04 17-APR-1990 GEM       DELETE PLE-COST-CONT-PYMT-ELIG-IND *00224001
002250*                             FROM FORMAT E.                     *00225001
002251*                             ADDED  PLP-COST-CONT-PYMT-ELIG-IND *00225101
002260*                             TO COMMON PORTION.                 *00226001
002300*                                                                *00230000
002400******************************************************************00240000
002500                                                                  00250000
002600 01  PLT-PAYMENT-LEVEL-TABLE.                                     00260000
002700     02 PLT-ENTRY-COUNT          PICTURE S9(04)          COMP.    00270000
002800     02 PLT-ENTRY                OCCURS 1 TO 25 TIMES             00280000
002900                                 DEPENDING ON PLT-ENTRY-COUNT     00290000
003000                                 INDEXED BY PLT-INDEX1.           00300000
003100                                                                  00310000
003200        03 PLT-ENTRY-NUMBER      PICTURE S9(04)          COMP.    00320000
003300        03 PLT-SAME-AS-ENTRY     PICTURE S9(04)          COMP.    00330000
003400        03 PLT-ENTRY-STATUS      PICTURE  X(01).                  00340000
003500           88 PLT-ENTRY-IS-ACTIVE                                 00350000
003600                                 VALUE 'Y'.                       00360000
003700           88 PLT-ENTRY-IS-INACTIVE                               00370000
003800                                 VALUE 'N'.                       00380000
003900        03 PLT-PROVISION-ID.                                      00390000
004000           04 PLT-BEN-PROV-ID    PICTURE  X(05).                  00400000
004100           04 PLT-BEN-PROV-FORMAT                                 00410000
004200                                 PICTURE  X(01).                  00420000
004300                                                                  00430000
004400        03 PLP-LOB-AREA          OCCURS 2 TIMES                   00440000
004500                                 INDEXED BY PLT-INDEX2.           00450000
004600           04 PLP-BP-COMMON-AREA.                                 00460000
004700              05 PLP-USE-COUNT                 PICTURE S9(05)     00470000
004800                                               COMP-3.            00480000
004900              05 PLP-PROVN-PRICING-METHD       PICTURE  X(02).    00490000
005000              05 PLP-COV-QUALIF                PICTURE  X(03).    00500000
005100              05 PLP-AGE-LVL-1                 PICTURE S9(03)     00510000
005200                                               COMP-3.            00520000
005300              05 PLP-DAY-LVL-1                 PICTURE S9(03)     00530000
005400                                               COMP-3.            00540000
005500              05 PLP-AGE-LVL-2                 PICTURE S9(03)     00550000
005600                                               COMP-3.            00560000
005700              05 PLP-DAY-LVL-2                 PICTURE S9(03)     00570000
005800                                               COMP-3.            00580000
005900              05 PLP-TREAT-RESTRN-IND          PICTURE  X(02).    00590000
006000              05 PLP-AGE-COV-TERMN-IND         PICTURE  X(01).    00600000
006100              05 PLP-AGE-COV-TERMN             PICTURE S9(03)     00610000
006200                                               COMP-3.            00620000
006300              05 PLP-AGE-EFF-DT-COMPRSN-IND    PICTURE  X(01).    00630000
006400              05 PLP-TRANSF-OTHER-RESP-IND     PICTURE  X(02).    00640002
006500              05 PLP-TRAUM-INJ-EFF-DT-COMP-IND PICTURE  X(01).    00650000
006600              05 PLP-COORD-MCARE-IND           PICTURE  X(01).    00660000
006700              05 PLP-SPILL-OVER-COINS-APL-IND  PICTURE  X(01).    00670000
006800              05 PLP-SPILL-OVER-DED-APL-IND    PICTURE  X(01).    00680000
006900              05 PLP-PROV-FILE-ELIG-IND        PICTURE  X(02).    00690000
007000              05 PLP-CRDT-CARD-PROCS-IND       PICTURE  X(01).    00700000
007100              05 PLP-DENT-SURG-PMT-ELG-IP-IND  PICTURE  X(02).    00710000
007200              05 PLP-DENT-SURG-PMT-ELG-OP-IND  PICTURE  X(02).    00720000
007300              05 PLP-ADDITIONAL-PRICING-PRCNT  PICTURE S9(03)     00730000
007400                                               COMP-3.            00740000
007500              05 PLP-VARIABLE-INDEMNITY-PRCNT  PICTURE S9(03)     00750000
007600                                               COMP-3.            00760000
007700              05 PLP-SPILL-OVR-RM-F-RT-APL-IND PICTURE  X(01).    00770000
007800              05 PLP-PLACE-TREAT-ELIG-IND      PICTURE  X(02).    00780000
007900              05 PLP-COSM-SURG-PAYMT-IND       PICTURE  X(02).    00790000
008000              05 PLP-CONG-DFCT-SURG-PMT-ELG    PICTURE  X(02).    00800000
008100              05 PLP-SERV-NECESRY-CORP-BIT-IND PICTURE  X(01).    00810000
008200              05 PLP-CERTFN-REQRM-IND          PICTURE  X(02).    00820000
008210              05 PLP-COST-CONT-PYMT-ELIG-IND   PICTURE  X(02).    00821001
008300                                                                  00830000
008400           04 PLP-BP-TABULARS.                                    00840000
008500              05 PLP-BEN-TAB-PROVN-ID-AAR      PICTURE  X(10).    00850000
008600              05 PLP-BEN-TAB-PROVN-ID-ABM      PICTURE  X(10).    00860000
008700              05 PLP-BEN-TAB-PROVN-ID-ACL      PICTURE  X(10).    00870000
008800              05 PLP-BEN-TAB-PROVN-ID-ADL      PICTURE  X(10).    00880000
008900              05 PLP-BEN-TAB-PROVN-ID-AOL      PICTURE  X(10).    00890000
009000              05 PLP-BEN-TAB-PROVN-ID-PPF      PICTURE  X(10).    00900000
009100              05 PLP-BEN-TAB-PROVN-ID-PVE      PICTURE  X(10).    00910000
009200                                                                  00920000
009300           04 PLA-BP-FORMAT-A.                                    00930000
009400              05 PLA-HOSP-ADM-RESTRN-IND       PICTURE  X(01).    00940000
009500              05 PLA-STAY-CD                   PICTURE S9(03)     00950000
009600                                               COMP-3.            00960000
009700              05 PLA-HOSP-COND-RELATSP-IND     PICTURE  X(01).    00970000
009800              05 PLA-REHAB-ADM-RESTRN-IND      PICTURE  X(01).    00980000
009900              05 PLA-DAYS-RDCN-RAT-IND         PICTURE  X(01).    00990000
010000              05 PLA-DAYS-RDCN-RAT-BASIC-APL   PICTURE S9(02)V9   01000000
010100                                               COMP-3.            01010000
010200              05 PLA-DAYS-RDCN-RAT-BASIC-BASE  PICTURE S9(02)V9   01020000
010300                                               COMP-3.            01030000
010400              05 PLA-DAYS-RDCN-RAT-SEC-APL     PICTURE S9(02)V9   01040000
010500                                               COMP-3.            01050000
010600              05 PLA-DAYS-RDCN-RAT-SEC-BASE    PICTURE S9(02)V9   01060000
010700                                               COMP-3.            01070000
010800              05 PLA-FLAT-RATE-PDM-AMT         PICTURE S9(05)V99  01080000
010900                                               COMP-3.            01090000
011000              05 PLA-CERTFN-REPETN-REQRM-IND   PICTURE  X(01).    01100000
011100              05 PLA-ADDN-ALLOW-AMT-PER-DAY    PICTURE S9(03)V99  01110000
011200                                               COMP-3.            01120000
011300              05 PLA-HSP-ADM-RESTRN-DAYS       PICTURE S9(03)     01130000
011400                                               COMP-3.            01140000
011500              05 PLA-NORM-NWBORN-OVRD-IND      PICTURE  X(01).    01150000
011600              05 PLA-DRUG-ELIG-MEMB-CLS-OVRD   PICTURE  X(01).    01160000
011700              05 PLA-ALCO-ELIG-MEMB-CLS-OVRD   PICTURE  X(01).    01170000
011800              05 PLA-ECF-SNF-OVRD-IND          PICTURE  X(01).    01180000
011900              05 PLA-TRANSSXL-PMT-RESTR-OVRD   PICTURE  X(01).    01190000
012000              05 PLA-ECF-F-RAT-PER-DIEM-AMT    PICTURE S9(05)V99  01200000
012100                                               COMP-3.            01210000
012200              05 PLA-STAY-CODE-IND             PICTURE  X(01).    01220000
012300              05 PLA-PHYS-EXAM-IND             PICTURE  X(01).    01230000
012400                                                                  01240000
012500           04 PLB-BP-FORMAT-B.                                    01250000
012600              05 PLB-HOSP-ADM-RESTRN-IND       PICTURE  X(01).    01260000
012700              05 PLB-STAY-CD                   PICTURE S9(03)     01270000
012800                                               COMP-3.            01280000
012900              05 PLB-HOSP-COND-RELATSP-IND     PICTURE  X(01).    01290000
013000              05 PLB-REHAB-ADM-RESTRN-IND      PICTURE  X(01).    01300000
013100              05 PLB-TREAT-TIME-FACTOR-IND     PICTURE  X(01).    01310000
013200              05 PLB-TREAT-TIME-FACTOR         PICTURE S9(03)     01320000
013300                                               COMP-3.            01330000
013400              05 PLB-ELIG-METHD-OF-TREAT-IND   PICTURE  X(01).    01340000
013500              05 PLB-REPR-REPLAC-RESTRN-IND    PICTURE  X(01).    01350000
013600              05 PLB-CERTN-REPETN-REQRD-IND    PICTURE  X(01).    01360000
013700              05 PLB-PHYS-EXAM-IND             PICTURE  X(01).    01370000
013800              05 PLB-HSP-ADM-RESTRN-DAYS       PICTURE S9(03)     01380000
013900                                               COMP-3.            01390000
014000              05 PLB-PROF-CHRG-HSP-CLM         PICTURE  X(01).    01400000
014100              05 PLB-AMBULANCE-ELIG-IND        PICTURE  X(02).    01410000
014200              05 PLB-NORM-NWBORN-OVRD-IND      PICTURE  X(01).    01420000
014300              05 PLB-DRUG-ELIG-MEMB-CLS-OVRD   PICTURE  X(01).    01430000
014400              05 PLB-ALCO-ELIG-MEMB-CLS-OVRD   PICTURE  X(01).    01440000
014500              05 PLB-ECF-SNF-OVRD-IND          PICTURE  X(01).    01450000
014600              05 PLB-TRANSSXL-PMT-RESTR-OVRD   PICTURE  X(01).    01460000
014700              05 PLB-STAY-CODE-IND             PICTURE  X(01).    01470000
014800              05 PLB-MAX-AMT-PER-VISIT         PICTURE S9(03)V99  01480000
014900                                               COMP-3.            01490000
015000                                                                  01500000
015100           04 PLC-BP-FORMAT-C.                                    01510000
015200              05 PLC-BEN-SCOPE-ID              PICTURE  X(04).    01520000
015300              05 PLC-EXCP-SCHED-ID             PICTURE  X(04).    01530000
015400              05 PLC-ELIG-METHD-OF-TREAT-IND   PICTURE  X(01).    01540000
015500              05 PLC-MULT-REL-PROC-IND         PICTURE  X(02).    01550000
015600              05 PLC-PRIM-SURG-DEPEND-IND      PICTURE  X(01).    01560000
015700              05 PLC-HOSP-STAFF-PROV-IND       PICTURE  X(01).    01570000
015800              05 PLC-REPEAT-PROC-IND           PICTURE  X(01).    01580000
015900              05 PLC-MULT-INJ-PRICING-MOD-IND  PICTURE  X(01).    01590000
016000              05 PLC-MIN-ELIG-AMT              PICTURE S9(03)V99  01600000
016100                                               COMP-3.            01610000
016200              05 PLC-SURG-MULT-PROC-PRICE-IND  PICTURE  X(01).    01620000
016300              05 PLC-PRIM-SURG-DPD-PAY-PCT     PICTURE S9(03)V99  01630000
016400                                               COMP-3.            01640000
016500              05 PLC-MULT-UNRL-PROC-1-PCT      PICTURE S9(03)V99  01650000
016600                                               COMP-3.            01660000
016700              05 PLC-MULT-UNRL-1-NO-OCCUR      PICTURE  X(01).    01670000
016800              05 PLC-MULT-UNRL-PROC-2-PCT      PICTURE S9(03)V99  01680000
016900                                               COMP-3.            01690000
017000              05 PLC-MULT-UNRL-2-NO-OCCUR      PICTURE  X(01).    01700000
017100              05 PLC-MULT-RL-PROC-1-PCT        PICTURE S9(03)V99  01710000
017200                                               COMP-3.            01720000
017300              05 PLC-MULT-RL-1-NO-OCCUR        PICTURE  X(01).    01730000
017400              05 PLC-MULT-RL-PROC-2-PCT        PICTURE S9(03)V99  01740000
017500                                               COMP-3.            01750000
017600              05 PLC-MULT-RL-2-NO-OCCUR        PICTURE  X(01).    01760000
017700              05 PLC-MULT-INJ-LVL-1-PCT        PICTURE S9(03)V99  01770000
017800                                               COMP-3.            01780000
017900              05 PLC-MULT-INJ-1-NO-OCCUR       PICTURE  X(01).    01790000
018000              05 PLC-MULT-INJ-LVL-2-PCT        PICTURE S9(03)V99  01800000
018100                                               COMP-3.            01810000
018200              05 PLC-MULT-INJ-2-NO-OCCUR       PICTURE  X(01).    01820000
018300              05 PLC-MULT-POD-PROC-PRICE-IND   PICTURE  X(01).    01830000
018400              05 PLC-MULT-POD-PROC-1-PCT       PICTURE S9(03)V99  01840000
018500                                               COMP-3.            01850000
018600              05 PLC-MULT-POD-1-NO-OCCUR       PICTURE  X(01).    01860000
018700              05 PLC-MULT-POD-PROC-2-PCT       PICTURE S9(03)V99  01870000
018800                                               COMP-3.            01880000
018900              05 PLC-MULT-POD-2-NO-OCCUR       PICTURE  X(01).    01890000
019000              05 PLC-MULT-POD-PROC-3-PCT       PICTURE S9(03)V99  01900000
019100                                               COMP-3.            01910000
019200              05 PLC-MULT-POD-3-NO-OCCUR       PICTURE  X(01).    01920000
019300              05 PLC-MULT-POD-PROC-4-PCT       PICTURE S9(03)V99  01930000
019400                                               COMP-3.            01940000
019500              05 PLC-MULT-POD-4-NO-OCCUR       PICTURE  X(01).    01950000
019600              05 PLC-CORRIDOR-OVERRIDE         PICTURE  X(01).    01960000
019700                                                                  01970000
019800           04 PLD-BP-FORMAT-D.                                    01980000
019900              05 PLD-BEN-SCOPE-ID              PICTURE  X(04).    01990000
020000              05 PLD-EXCP-SCHED-ID             PICTURE  X(04).    02000000
020100              05 PLD-HOSP-ADM-RESTRN-IND       PICTURE  X(01).    02010000
020200              05 PLD-STAY-CD                   PICTURE S9(03)     02020000
020300                                               COMP-3.            02030000
020400              05 PLD-HOSP-COND-RELATSP-IND     PICTURE  X(01).    02040000
020500              05 PLD-DAYS-RDCN-RAT-IND         PICTURE  X(01).    02050000
020600              05 PLD-DAYS-RDCN-RAT-BASIC-APL   PICTURE S9(02)V9   02060000
020700                                               COMP-3.            02070000
020800              05 PLD-DAYS-RDCN-RAT-BASIC-BASE  PICTURE S9(02)V9   02080000
020900                                               COMP-3.            02090000
021000              05 PLD-DAYS-RDCN-RAT-SEC-APL     PICTURE S9(02)V9   02100000
021100                                               COMP-3.            02110000
021200              05 PLD-DAYS-RDCN-RAT-SEC-BASE    PICTURE S9(02)V9   02120000
021300                                               COMP-3.            02130000
021400              05 PLD-MAX-AMT-PER-VISIT         PICTURE S9(03)V99  02140000
021500                                               COMP-3.            02150000
021600              05 PLD-FLAT-RATE-PDM-AMT         PICTURE S9(05)V99  02160000
021700                                               COMP-3.            02170000
021800              05 PLD-CERT-REPT-REQ-IND         PICTURE  X(01).    02180000
021900              05 PLD-HSP-ADM-RESTRN-DAYS       PICTURE S9(03)     02190000
022000                                               COMP-3.            02200000
022100              05 PLD-STAY-CODE-IND             PICTURE  X(01).    02210000
022200              05 PLD-BEN-MAX-VISIT-IND         PICTURE  X(01).    02220000
022300              05 PLD-BEN-MAX-VISIT-DAYS        PICTURE S9(03)     02230000
022400                                               COMP-3.            02240000
022500              05 PLD-TREAT-TIME-FACTOR-IND     PICTURE  X(01).    02250000
022600              05 PLD-TREAT-TIME-FACTOR         PICTURE S9(03)     02260000
022700                                               COMP-3.            02270000
022800              05 PLD-CORRIDOR-OVERRIDE         PICTURE  X(01).    02280000
022900                                                                  02290000
023000           04 PLE-BP-FORMAT-E.                                    02300000
023100              05 PLE-BEN-SCOPE-ID              PICTURE  X(04).    02310000
023200              05 PLE-EXCP-SCHED-ID             PICTURE  X(04).    02320000
023300              05 PLE-HOSP-ADM-RESTRN-IND       PICTURE  X(01).    02330000
023400              05 PLE-TREAT-TIME-FACTOR-IND     PICTURE  X(01).    02340000
023500              05 PLE-TREAT-TIME-FACTOR         PICTURE S9(03)     02350000
023600                                               COMP-3.            02360000
023700              05 PLE-REPR-REPLAC-RESTRN-IND    PICTURE  X(01).    02370000
023800              05 PLE-CERTN-REPETN-REQRD-IND    PICTURE  X(01).    02380000
023900              05 PLE-PHYS-EXAM-IND             PICTURE  X(01).    02390000
024000              05 PLE-MAX-AMT-PER-VISIT         PICTURE S9(03)V99  02400000
024100                                               COMP-3.            02410000
024200              05 PLE-HSP-ADM-RESTRN-DAYS       PICTURE S9(03)     02420000
024300                                               COMP-3.            02430000
024400              05 PLE-AMBULANCE-ELIG-IND        PICTURE  X(02).    02440000
024500              05 PLE-BEN-MAX-VISITS-IND        PICTURE  X(01).    02450000
024600              05 PLE-BEN-MAX-VISITS-DAYS       PICTURE S9(03)     02460000
024700                                               COMP-3.            02470000
024900              05 PLE-CORRIDOR-OVERRIDE         PICTURE  X(01).    02490000
025000                                                                  02500000
025100           04 PLW-BP-FORMAT-W.                                    02510000
025200              05 PLW-HOSP-ADM-RESTRN-IND       PICTURE  X(01).    02520000
025300              05 PLW-STAY-CD                   PICTURE S9(03)     02530000
025400                                               COMP-3.            02540000
025500              05 PLW-HOSP-COND-RELATSP-IND     PICTURE  X(01).    02550000
025600              05 PLW-DAYS-RDCN-RAT-IND         PICTURE  X(01).    02560000
025700              05 PLW-DAYS-RDCN-RAT-BASIC-APL   PICTURE S9(02)V9   02570000
025800                                               COMP-3.            02580000
025900              05 PLW-DAYS-RDCN-RAT-BASIC-BASE  PICTURE S9(02)V9   02590000
026000                                               COMP-3.            02600000
026100              05 PLW-DAYS-RDCN-RAT-SEC-APL     PICTURE S9(02)V9   02610000
026200                                               COMP-3.            02620000
026300              05 PLW-DAYS-RDCN-RAT-SEC-BASE    PICTURE S9(02)V9   02630000
026400                                               COMP-3.            02640000
026500              05 PLW-FLAT-RATE-PDM-AMT         PICTURE S9(05)V99  02650000
026600                                               COMP-3.            02660000
026700              05 PLW-ADDN-ALLOW-AMT-PER-DAY    PICTURE S9(03)V99  02670000
026800                                               COMP-3.            02680000
026900              05 PLW-CERTFN-REPETN-REQRM-IND   PICTURE  X(01).    02690000
027000              05 PLW-TREAT-TIME-FACTOR-IND     PICTURE  X(01).    02700000
027100              05 PLW-TREAT-TIME-FACTOR         PICTURE S9(03)     02710000
027200                                               COMP-3.            02720000
027300              05 PLW-ELIG-METHD-OF-TREAT-IND   PICTURE  X(01).    02730000
027400              05 PLW-PHYS-EXAM-IND             PICTURE  X(01).    02740000
027500              05 PLW-REHAB-ADM-RESTRN-IND      PICTURE  X(01).    02750000
027600              05 PLW-HSP-ADM-RESTRN-DAYS       PICTURE S9(03)     02760000
027700                                               COMP-3.            02770000
027800              05 PLW-NORM-NWBORN-OVRD-IND      PICTURE  X(01).    02780000
027900              05 PLW-DRUG-ELIG-MEMB-CLS-OVRD   PICTURE  X(01).    02790000
028000              05 PLW-ALCO-ELIG-MEMB-CLS-OVRD   PICTURE  X(01).    02800000
028100              05 PLW-STAY-CODE-IND             PICTURE  X(01).    02810000
028200              05 PLW-TRNS-SEX-REST-OVRD-IND    PICTURE  X(01).    02820000
