000100******************************************************************00010000
000200*                                                                *00020000
000300*    COPYBOOK:   ELSACUMC                                        *00030000
000400*    DATE:       19-JUN-1987                                     *00040000
000500*    AUTHOR:     LUCY E. TORRES                                  *00050000
000600*    FUNCTION:   ACUMULATOR SUMMARY RECORD LAYOUT                *00060000
000700*                                                                *00070000
000800*                THIS COPY MEMBER WILL BE USED TO SUMMARIZE      *00080000
000900*                AN ACCUMULATOR'S OCCURENCE ONE AT A TIME.       *00090000
001000*                ALL ACCUMULATOR SETUP PROGRAM WILL WRITE THIS   *00100000
001100*                RECORD LAYOUT TO A TEMPORARY QUEUE AND THE      *00110000
001200*                ACCUMULATOR GENERATORS WILL READ IT IN.  THIS   *00120000
001300*                IS USED TO DISPLAY ACCUMULATOR INFORMATION.     *00130000
001400*                                                                *00140000
001401******************************************************************00140100
001402*                                                                *00140200
001403*                      MAINTENANCE HISTORY                       *00140300
001404*                                                                *00140400
001405*  MOD     DATE     BY  DRPT                ACTION               *00140500
001406* ----- ----------- --- ----- ---------------------------------- *00140600
001407* 01.00 19-JUN-1987 LET       CREATED                            *00140700
001408* 01.01 20-JUL-1987 LET       ADD TWO 88 LEVELS TO ASCEND DESCEND*00140800
001409*                             INDICATOR ELEMENT.                 *00140900
001410* 01.02 24-SEP-1987 LET       DELETED SOME USELESS ENTRIES.      *00141000
001411*                                                                *00141100
001412* 01.03 02-NOV-1987 REB       CHANGED INTERNAL SLOT NBRS INTO    *00141200
001413*                             TABLES. ADDED 88 LEVEL TO ASCEND   *00141300
001414*                             DESCEND INDICATOR ELEMENT. MOVED   *00141400
001415*                             THE VARIABLE AREA TO THE END.      *00141500
001416*                                                                *00141600
001417* 01.04 19-NOV-1987 LET       ADDED PERCENT LEVEL FIELDS TO      *00141700
001418*                             INTERNAL TABULAR AREA.  REQ:REB    *00141800
001419*                                                                *00141900
001420* 01.05 20-DEC-1990 AKK       ADDED 88 LEVEL BISCEND IND AND AGE *00142000
001421*                             ORDER; RELATIONSHIP IND; AGE       *00142100
001422*                             LIMIT TO AND FROM IND; ADD NEW     *00142200
001423*                             TABULARS; COPY BENEFIT PER IND,    *00142300
001424*                             BENEFIT-PERIOD TIME FACTOR AND,    *00142400
001425*                             BENEFIT-PERIOD QUAL INTO NON-,     *00142500
001426*                             FIXED PERTION AND INCREASED,       *00142600
001427*                             OCCURENCES TO 46 FROM 29.          *00142700
001428*                                                                *00142800
001429* 01.06 22-APR-1991 JPB       CHANGED ACCUM-DEFINITION FROM 1 TO *00142900
001430*                             2 BYTES.                           *00143000
001431*                                                                *00143100
001432* 01.07 16-JUL-1991 AKK       ADDED OPX BASE AMOUNT SOURCE IND   *00143200
001433*                             AND 12 BYTES OF FILLER.            *00143300
001434*                                                                *00143400
001435* 01.08 16-AUG-1991 RJL       RESTRUCTURED TO ASSOCIATE INTERNAL *00143500
001436*                             TABULAR SLOT NUMBERS WITH THEIR    *00143600
001437*                             RESPECTIVE PAYMENT LEVELS.  ALSO   *00143700
001438*                             MOVED PATIENT AGE INFORMATION TO   *00143800
001439*                             THE FIXED PORTION OF THE RECORD.   *00143900
001440*                                                                *00144000
001441* 01.09 27-MAR-1992 BAK       ADD FIELDS TO SUPPORT RECORDS--    *00144100
001442*                             GCTABMC, GCTACLC, GCTADLC & GCTAOLC*00144200
001443*                                                                *00144300
001441* 01.10 10-OCT-1998 AKK       ADD FILES TO SUPPORT COPAY TABULAR *00144401
001443*                                                                *00144601
001441* 01.11 21-AUG-2000 AKK       ADD SUPPORT FOR #IPGS TABULAR      *00144711
001443*                                                                *00144810
NSK24 * 01.12 13-JUN-2024 NSK P28776-INCREASE MAX OCCURS NUM FROM 44   *00144820
NSK24 *                              TO 175.                           *00144821
001443*                                                                *00144830
001444******************************************************************00144910
001445                                                                  00145010
001446 01  ACCUM-OCCURENCE-SUMMARY.                                     00145110
001447     02 ACCUM-FIXED-AREA.                                         00145210
001448        03 ACCUM-TYPE                      PICTURE  X(03).        00145310
001449           88 ACCUM-ABM                    VALUE 'ABM'.           00145410
001450           88 ACCUM-ACL                    VALUE 'ACL'.           00145510
001450           88 ACCUM-ACP                    VALUE 'ACP'.           00145610
001451           88 ACCUM-ADL                    VALUE 'ADL'.           00145710
001452           88 ACCUM-AOL                    VALUE 'AOL'.           00145810
001453        03 ACCUM-FYI-VALUE                 PICTURE  X(03).        00145910
001454           88 FYI-VALUE-NA                 VALUE ZEROES.          00146010
001455        03 ACCUM-COST-CONTAIN-IND          PICTURE  X(02).        00146110
001456           88 COST-CONTAIN-IND-NA          VALUE ZEROES.          00146210
001457        03 ACCUM-PLACE-OF-TREATMENT        PICTURE  X(02).        00146310
001458           88 PLACE-OF-TREATMENT-NA        VALUE ZEROES.          00146410
001459        03 ACCUM-BENEFIT-PERIOD            PICTURE  X(02).        00146510
001460           88 BENEFIT-PERIOD-NA            VALUE ZEROES.          00146610
001461        03 ACCUM-BEN-PER-TIME-FCTR         PICTURE S9(03) COMP-3. 00146710
001462        03 ACCUM-BEN-PER-TIME-QUAL         PICTURE  X(01).        00146810
001463           88 BEN-PER-TIME-QUAL-NA         VALUE ZEROS.           00146910
001464        03 ACCUM-INTERVAL-TIME-FCTR        PICTURE S9(03) COMP-3. 00147010
001465        03 ACCUM-INTERVAL-TYPE             PICTURE  X(02).        00147110
001466           88 INTERVAL-TYPE-NA             VALUE ZEROS.           00147210
001467        03 ACCUM-INTERVAL-OVRD-IND         PICTURE  X(01).        00147310
001468           88 INTERVAL-OVRD-IND-NA         VALUE ZEROES.          00147410
001469        03 ACCUM-INTERVAL-OVRD-VALUE       PICTURE S9(05) COMP-3. 00147510
001470        03 ACCUM-L-O-B                     PICTURE  X(01).        00147610
001471           88 L-O-B-NA                     VALUE ZEROES.          00147710
001472        03 ACCUM-PRVDR-CLS                 PICTURE  X(01).        00147810
001473           88 ACCUM-PRVDR-CLS-ALL            VALUE 'A'.           00147910
001474           88 ACCUM-PRVDR-CLS-INST           VALUE 'I'.           00148010
001475           88 ACCUM-PRVDR-CLS-PROF           VALUE 'P'.           00148110
001472        03 ACCUM-PRVDR-SPC                 PICTURE  X(01).        00148211
001473           88 ACCUM-PRVDR-SPC-ALL            VALUE 'A'.           00148311
001474           88 ACCUM-PRVDR-SPC-INST           VALUE 'I'.           00148411
001475           88 ACCUM-PRVDR-SPC-PROF           VALUE 'P'.           00148511
001476        03 ACCUM-REINSTATEMENT-IND         PICTURE  X(01).        00148611
001477           88 REINSTATEMENT-IND-NA         VALUE ZEROES.          00148711
001478        03 ACCUM-DEFINITION                PICTURE  X(02).        00148811
001479           88 DEFINITION-NA                VALUE ZEROES.          00148911
001480        03 ACCUM-CARRY-OVER-CREDIT-IND     PICTURE  X(01).        00149011
001481           88 CARRY-OVER-CREDIT-IND-NA     VALUE ZEROES.          00149111
001482        03 ACCUM-ASCEND-DESCEND-IND        PICTURE  X(01).        00149211
001483           88 ASCEND-DESCEND-IND-NA        VALUE ZEROES.          00149311
001484           88 ACCUM-ASCEND-ORDER           VALUE '1'.             00149411
001485           88 ACCUM-DESCEND-ORDER          VALUE '2'.             00149511
001486           88 ACCUM-BISCEND-ORDER          VALUE '3'.             00149611
001487           88 ACCUM-VARIABLE-TYPE          VALUE '1', '2', '3'.   00149711
001488        03 ACCUM-CONDITION.                                       00149811
001489           04 ACCUM-COND-ALL-BIT           PICTURE  X(01).        00149911
001490           04 ACCUM-COND-EXCLUSION-BIT     PICTURE  X(01).        00150011
001491           04 ACCUM-COND-ICD-BIT           PICTURE  X(01).        00150111
001492           04 ACCUM-COND-TB-BIT            PICTURE  X(01).        00150211
001493           04 ACCUM-COND-MENTAL-BIT        PICTURE  X(01).        00150311
001494           04 ACCUM-COND-DRUG-BIT          PICTURE  X(01).        00150411
001495           04 ACCUM-COND-ALCOHOL-BIT       PICTURE  X(01).        00150511
001496           04 ACCUM-COND-OB-COMP-BIT       PICTURE  X(01).        00150611
001497           04 ACCUM-COND-OB-NORM-BIT       PICTURE  X(01).        00150711
001498           04 ACCUM-COND-MALIGNANCY-BIT    PICTURE  X(01).        00150811
001499           04 ACCUM-COND-CARDIAC-DISEASE-BIT                      00150911
001500                                           PICTURE  X(01).        00151011
001501           04 ACCUM-COND-OBESITY-BIT       PICTURE  X(01).        00151111
001502           04 ACCUM-COND-KIDNEY-DISEASE-BIT                       00151211
001503                                           PICTURE  X(01).        00151311
001504           04 ACCUM-COND-ACCIDENT-BIT      PICTURE  X(01).        00151411
001505           04 ACCUM-COND-PRE-EXIST-BIT     PICTURE  X(01).        00151511
001506           04 ACCUM-COND-NON-EMER-BIT      PICTURE  X(01).        00151611
001507           04 ACCUM-COND-SUICIDE-BIT       PICTURE  X(01).        00151711
001508           04 ACCUM-COND-TMJ-BIT           PICTURE  X(01).        00151811
001509           04 ACCUM-COND-INF-BIT           PICTURE  X(01).        00151911
001510           04 ACCUM-COND-LIFE-THREAT-BIT   PICTURE  X(01).        00152011
001511           04                              PICTURE  X(10).        00152111
001512        03 ACCUM-FAM-OR-INDIV              PICTURE  X(01).        00152211
001513           88 FAM-OR-INDIV-NA              VALUE ZEROS.           00152311
001514        03 ACCUM-DED-BASE-AMT-SOURCE-IND   PICTURE  X(01).        00152411
001515           88 DED-BASE-AMT-SOURCE-IND-NA   VALUE ZEROS.           00152511
001516        03 ACCUM-OPX-BASE-AMT-SOURCE-IND   PICTURE  X(01).        00152611
001517           88 OPX-BASE-AMT-SOURCE-IND-NA   VALUE ZEROS.           00152711
001518        03 ACCUM-MAX-BASE-AMT-SOURCE-IND   PICTURE  X(01).        00152811
001519           88 MAX-BASE-AMT-SOURCE-IND-NA   VALUE ZEROS.           00152911
001520        03 FILLER                          PICTURE  X(01).        00153011
001521        03 ACCUM-VALUE-QUALIFIER           PICTURE  X(01).        00153111
001522           88 VALUE-QUALIFIER-NA           VALUE ZEROS.           00153211
001523        03 ACCUM-RELATIONSHIP-IND          PICTURE  X(02).        00153311
001524           88 RELATIONSHIP-IND-NA          VALUE ZEROES.          00153411
001525        03 ACCUM-AGE-LIMIT-FROM-VAL        PICTURE S9(03) COMP-3. 00153511
001526           88 ACCUM-AGE-LIMIT-FROM-UNLIM   VALUE +999.            00153611
001527        03 ACCUM-AGE-LIMIT-FROM-IND        PICTURE  X(01).        00153711
001528           88 AGE-LMT-FROM-IND-NA          VALUE ZEROES.          00153811
001529        03 ACCUM-AGE-LIMIT-TO-VAL          PICTURE S9(03) COMP-3. 00153911
001530           88 ACCUM-AGE-LIMIT-TO-UNLIM     VALUE +999.            00154011
001531        03 ACCUM-AGE-LIMIT-TO-IND          PICTURE  X(01).        00154111
001532           88 AGE-LMT-TO-IND-NA            VALUE ZEROES.          00154211
001533        03 ACCUM-LMT-MANDATORY-IND         PICTURE  X(01).        00154311
001534           88 LMT-MANDATORY-IND-NA         VALUE ZEROS.           00154411
001537        03 ACCUM-SERVICE-GROUP             PICTURE  X(02).        00154511
001538           88 SERVICE-GROUP-NA             VALUE ZEROS.           00154611
001539        03 ACCUM-INTERNAL-DESCRIPTOR       PICTURE  X(09).        00154711
001540           88 INTERNAL-DESCRIPTOR-NA       VALUE ZEROS.           00154811
001541        03 ACCUM-DAY-FACTOR-IND            PICTURE  X(01).        00154911
001542           88 DAY-FACTOR-IND-NA            VALUE ZEROES.          00155011
001543        03 ACCUM-CLAIM-LVL-ACCUM-IND       PICTURE  X(01).        00155111
001544           88 CLAIM-LVL-ACCUM-IND-NA       VALUE ZEROES.          00155211
001545        03 ACCUM-BEN-PER-MAX-OVRD-IND      PICTURE  X(01).        00155311
001546           88 BEN-PER-MAX-OVRD-IND-NA      VALUE ZEROES.          00155411
001547        03 ACCUM-1ST-DOLR-COVRGE-LMT       PICTURE  X(01).        00155511
001548           88 1ST-DOLR-COVRGE-LMT-NA       VALUE ZEROES.          00155611
001549        03 FILLER                          PICTURE  X(4).         00155711
001550     02 ACCUM-VARIABLE-AREA.                                      00155811
001551        03 ACCUM-COPAY-COUNT               PICTURE S9(03) COMP.   00155911
001551        03 ACCUM-ASCEND-DESCEND-COUNT      PICTURE S9(03) COMP.   00156011
001552        03 ACCUM-ASCEND-DESCEND-ENTRY                             00156111
001553                       OCCURS 46 TIMES                            00156211
001555                       INDEXED BY ASC-DES-INDEX                   00156306
001556                                  ASC-DES-MAX-INDEX.              00156406
001557           04 ACCUM-BISCEND-IND            PICTURE  X(01).        00156506
001558              88 BISCEND-IND-NA            VALUE ZEROS.           00156606
001559           04 ACCUM-PERCENT-LEVEL          PICTURE S9(03) COMP-3. 00156706
001560           04 ACCUM-VALUE-LIMIT            PICTURE S9(07)V99      00156806
001561                                           COMP-3.                00156906
001562           04 ACCUM-VALUE-LIMIT-NON-DOLLAR                        00157006
001563                                 REDEFINES ACCUM-VALUE-LIMIT      00157106
001564                                           PICTURE S9(09)         00157206
001565                                           COMP-3.                00157306
001566              88 ACCUM-VAL-UNLIM           VALUE +999999999       00157406
001567                                                 +999999900.      00157506
001568           04 ACCUM-IBGR-SLOT-NBR          PICTURE S9(07) COMP-3. 00157606
001569              88 NO-IBGR-SLOT-NBR          VALUE ZEROS.           00157706
001570           04 ACCUM-IPGN-SLOT-NBR          PICTURE S9(07) COMP-3. 00157806
001571              88 NO-IPGN-SLOT-NBR          VALUE ZEROS.           00157906
001580           04 ACCUM-IPGT-SLOT-NBR          PICTURE S9(07) COMP-3. 00158000
001590              88 NO-IPGT-SLOT-NBR          VALUE ZEROS.           00159000
001600           04 ACCUM-IDGD-SLOT-NBR          PICTURE S9(07) COMP-3. 00160000
001700              88 NO-IDGD-SLOT-NBR          VALUE ZEROS.           00170000
001800           04 ACCUM-IPGP-SLOT-NBR          PICTURE S9(07) COMP-3. 00180000
001900              88 NO-IPGP-SLOT-NBR          VALUE ZEROS.           00190000
001800           04 ACCUM-IPGS-SLOT-NBR          PICTURE S9(07) COMP-3. 00191010
001900              88 NO-IPGS-SLOT-NBR          VALUE ZEROS.           00192010
001550     02 ACCUM-COPAY-VARIABLE-AREA.                                00200003
001552        03 ACCUM-COPAY-ENTRY                                      00220003
NSK24 *****               OCCURS  44 TIMES                              00230007
NSK24                     OCCURS  175 TIMES                             00230008
001555                    INDEXED BY COPAY-INDEX.                       00250003
001557           04 ACCUM-COPAY-TIME-DOL-IND     PICTURE  X(01).        00270003
001558              88 ACCUM-COPAY-TAD-NA      VALUE ZEROS.             00280009
                 04 ACCUM-CO-PAY-IND           PICTURE  X(01).          00281009
                    88 CO-PAY-IND-NA           VALUE ZEROS.             00282009
001559           04 ACCUM-COPAY-BEN-PER          PICTURE X(02).         00290003
001559           04 ACCUM-COPAY-TAD-IND          PICTURE X(02).         00290103
001559           04 ACCUM-COPAY-DEFINITION       PICTURE X(02).         00290203
001559           04 ACCUM-COPAY-VALUE-QUALIFIER  PICTURE X.             00291003
001560           04 ACCUM-COPAY-VALUE-LIMIT      PICTURE S9(07)V99      00300003
001561                                           COMP-3.                00310003
001562           04 ACCUM-COPAY-VAL-LIM-NO-DOL                          00320005
001563                     REDEFINES ACCUM-COPAY-VALUE-LIMIT            00330006
001564                                           PICTURE S9(09)         00340003
001565                                           COMP-3.                00350003
001566              88 ACCUM-COPAY-VAL-UNLIM     VALUE +999999999       00360003
                                                       +999999900.      00370008
