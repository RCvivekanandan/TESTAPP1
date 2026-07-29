000100******************************************************************00010002
000200*                                                                *00020002
000300*    COPYBOOK:   ELXPVITC                                        *00030002
000400*    DATE:       06-AUG-1992                                     *00040002
000500*    AUTHOR:     BARBARA KEIB *                                   00050002
000600*    FUNCTION:   TABLE TO DERIVE PROVIDER CONTROL FOR INSTITUTION*00060002
000200*                RGO 4/95. IT APPEARS THAT THE PURPOSE OF THESE  *00060003
000200*                4 TABLES IS TO DETERMINE WHICH CONTRACT RECORD  *00060003
000200*                TO CHOOSE.                                      *00060003
000200*                                                                *00060003
000800******************************************************************00060005
000200*                                                                *00060006
000200*    THE TABLE CONSISTS OF THE FOLLOWING FIELDS:                 *00060007
      *                                                                *00060008
000200* 1. TAB-KEY    = GROUP SPECIFIC BC PROVIDER CONTROL CONT IND    *00060009
000200*                 PROV-CONTROL-CONT-BS-IND FIELD                 *00060009
000200* 2. CON-KEY    = CONTRACT FILE PROVIDER CODE DESIRED            *00060010
      *                 THESE ARE ON THE DESCRIPTION, IN THE CODES     *00060008
      *                 MANUAL, OF THE PROV-CONTROL-CONT-BS-IND.       *00060008
      *                 'CONTRACT PRV EQUALS __ AND __ AND ETC'        *00060008
000200* 3. TAB1-IND1  = GVLF/GVLP/GVLQ PROVIDER CONTROL INDICATOR 1    *00060030
000200* 4. TAB1-IND2  = GVLF/GVLP/GVLQ PROVIDER CONTROL INDICATOR 2    *00060031
000200* 5. PLAN-IND   = PLAN PROVIDER INDICATOR                        *00060071
000200* 6. EMPL-IND   = EMPLOYER PROVIDER INDICATOR                    *00060072
000200*                                                                *00060006
      *                                                                *00060080
      ******************************************************************00060080
      * HOW THE OCCURANCES IN THE TABLE ARE CREATED.               RGO *00060080
      *                                                                *00060080
      *   1) LOOK IN THE CODES MANUAL AT THE DESCRIPTION FOR ELEMENT   *00060008
      *       'PROV-CONTROL-CONT-BC-IND'.  AFTER THE PHRASE            *00060008
      *       'CONTRACT PRV =' WILL BE 1 OR MORE 2 DIGIT CODES.        *00060008
      *   2) THESE ARE THE ONLY VALID VALUES THAT CAN BE ON THE        *00060008
      *      PROVIDER-CNTL FROM THE GCDATES FILE OF THE CONTRACT,      *00060008
      *      FOR THIS PARTICULAR PROV-CONTROL-CONT-BC-IND VALUE.       *00060080
      *   3) YOU MUST HAVE AN ENTRY IN THE TABLE FOR EACH COMBINATION  *00060008
      *      OF PROV-CONTROL-CONT-BC-IND AND ALL PROVIDER-CNTL VALUES. *00060008
      *                                                                *00060008
000200* NOTES ON PLAN-IND AND EMPL-IND:                                *00060006
000200*   1. THE VALUE IS DETERMINED BY WHAT THE DESCRIPTION IN THE    *00060006
000200*      CODES MANUAL FOR PROV-CONTROL-CONT-BS-IND FIELD STATES.   *00060006
000200*                                                                *00060006
000200*   2. THE DESCRIPTION IS IN THE SAME ORDER/CORRESPONDS TO, THE  *00060006
000200*      ORDER OF THE VALUES AFTER 'CONTRACT PRV EQUALS'.          *00060006
000200*                                                                *00060006
000200*    VALID VALUES FOR PLAN-IND AND EMPL-IND:                     *00060006
000200*    '+'  MEANS IT WAS NOT SPECIFIED IN THE DESCRIPTION.         *00060006
000200*    '1'  MEANS IT WAS STATED. IE, 'PLAN'.                       *00060006
000200*    '0'  MEANS IT WAS STATED AS 'NON-______'. IE, 'NON-PLAN'.   *00060006
      *                                                                *00060080
000200*                                                                *00060006
000800******************************************************************00080002
000900*                                                                *00090002
001000*                      MAINTENANCE HISTORY                       *00100002
001100*                                                                *00110002
001200*  MOD     DATE     BY  DRPT                ACTION               *00120002
001300* ----- ----------- --- ----- ---------------------------------- *00130002
001400* 01.00 06-AUG-1992 BAK       CREATED                            *00140002
001400* 01.01 01-SEP-1992 BAK       ELIMINATED ALL BUT 1 SET OF INDIC. *00140003
001400* 02.00 21-SEP-1993 BAK       CHANGE INDICATORS TO 2 DIGITS      *00140003
002920* 03.00 20-JUL-1994 RGO       FIX 08 1F . SHOULD BE 08 2F        *00292002
002920* 04.00 07-APR-1995 RGO       ADDED TAB-KEY / CON-KEY '15  10'   *00292002
002920*                                                                *00292002
003000******************************************************************00300002
003200 01  ELS-IPRV-TABLE.                                              00300003
001100                                                                  00300004
001000    02  IPRV-VALUES.                                              00301300
                                                                        00301400
001300      03  FILLER.                                                 00302400
001300          04  FILLER              PIC X(02)  VALUE '00'.          00302410
001300          04  FILLER              PIC X(02)  VALUE '00'.          00302411
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302420
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302421
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302426
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302429
                                                                        00302450
001300      03  FILLER.                                                 00302451
001300          04  FILLER              PIC X(02)  VALUE '01'.          00302452
001300          04  FILLER              PIC X(02)  VALUE '0C'.          00302453
001300          04  FILLER              PIC X(02)  VALUE '0C'.          00302454
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302455
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302460
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302461
                                                                        00302462
001300      03  FILLER.                                                 00302463
001300          04  FILLER              PIC X(02)  VALUE '01'.          00302464
001300          04  FILLER              PIC X(02)  VALUE '0F'.          00302465
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302466
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302467
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302472
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302473
                                                                        00302474
001300      03  FILLER.                                                 00302475
001300          04  FILLER              PIC X(02)  VALUE '02'.          00302476
001300          04  FILLER              PIC X(02)  VALUE '1C'.          00302477
001300          04  FILLER              PIC X(02)  VALUE '1C'.          00302478
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302479
001300          04  FILLER              PIC X(01)  VALUE '1'.           00302484
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302485
                                                                        00302486
001300      03  FILLER.                                                 00302487
001300          04  FILLER              PIC X(02)  VALUE '02'.          00302488
001300          04  FILLER              PIC X(02)  VALUE '1F'.          00302489
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302490
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302491
001300          04  FILLER              PIC X(01)  VALUE '1'.           00302496
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302497
                                                                        00302498
001300      03  FILLER.                                                 00302499
001300          04  FILLER              PIC X(02)  VALUE '02'.          00302500
001300          04  FILLER              PIC X(02)  VALUE '20'.          00302501
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302502
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302503
001300          04  FILLER              PIC X(01)  VALUE '0'.           00302508
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302509
                                                                        00302510
001300      03  FILLER.                                                 00302511
001300          04  FILLER              PIC X(02)  VALUE '03'.          00302512
001300          04  FILLER              PIC X(02)  VALUE '10'.          00302513
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302514
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302515
001300          04  FILLER              PIC X(01)  VALUE '1'.           00302520
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302521
                                                                        00302522
001300      03  FILLER.                                                 00302523
001300          04  FILLER              PIC X(02)  VALUE '03'.          00302524
001300          04  FILLER              PIC X(02)  VALUE '20'.          00302525
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302526
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302527
001300          04  FILLER              PIC X(01)  VALUE '0'.           00302532
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302533
                                                                        00302534
001300      03  FILLER.                                                 00302535
001300          04  FILLER              PIC X(02)  VALUE '04'.          00302536
001300          04  FILLER              PIC X(02)  VALUE '1A'.          00302537
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302538
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302539
001300          04  FILLER              PIC X(01)  VALUE '1'.           00302544
001300          04  FILLER              PIC X(01)  VALUE '1'.           00302545
                                                                        00302546
001300      03  FILLER.                                                 00302547
001300          04  FILLER              PIC X(02)  VALUE '04'.          00302548
001300          04  FILLER              PIC X(02)  VALUE '1B'.          00302549
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302550
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302551
001300          04  FILLER              PIC X(01)  VALUE '1'.           00302556
001300          04  FILLER              PIC X(01)  VALUE '0'.           00302557
                                                                        00302558
001300      03  FILLER.                                                 00302571
001300          04  FILLER              PIC X(02)  VALUE '04'.          00302572
001300          04  FILLER              PIC X(02)  VALUE '20'.          00302573
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302574
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302575
001300          04  FILLER              PIC X(01)  VALUE '0'.           00302580
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302581
                                                                        00302582
001300      03  FILLER.                                                 00302583
001300          04  FILLER              PIC X(02)  VALUE '05'.          00302584
001300          04  FILLER              PIC X(02)  VALUE '0A'.          00302585
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302586
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302587
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302592
001300          04  FILLER              PIC X(01)  VALUE '1'.           00302593
                                                                        00302594
001300      03  FILLER.                                                 00302595
001300          04  FILLER              PIC X(02)  VALUE '05'.          00302596
001300          04  FILLER              PIC X(02)  VALUE '0B'.          00302597
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302598
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302599
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302604
001300          04  FILLER              PIC X(01)  VALUE '0'.           00302605
                                                                        00302606
001300      03  FILLER.                                                 00302607
001300          04  FILLER              PIC X(02)  VALUE '06'.          00302608
001300          04  FILLER              PIC X(02)  VALUE '1D'.          00302609
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302610
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302611
001300          04  FILLER              PIC X(01)  VALUE '1'.           00302616
001300          04  FILLER              PIC X(01)  VALUE '1'.           00302617
                                                                        00302618
001300      03  FILLER.                                                 00302619
001300          04  FILLER              PIC X(02)  VALUE '06'.          00302620
001300          04  FILLER              PIC X(02)  VALUE '1E'.          00302621
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302622
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302623
001300          04  FILLER              PIC X(01)  VALUE '1'.           00302628
001300          04  FILLER              PIC X(01)  VALUE '0'.           00302629
                                                                        00302630
001300      03  FILLER.                                                 00302631
001300          04  FILLER              PIC X(02)  VALUE '06'.          00302632
001300          04  FILLER              PIC X(02)  VALUE '2F'.          00302633
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302634
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302635
001300          04  FILLER              PIC X(01)  VALUE '0'.           00302640
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302641
                                                                        00302642
001300      03  FILLER.                                                 00302643
001300          04  FILLER              PIC X(02)  VALUE '07'.          00302644
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00302645
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302646
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302647
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302652
001300          04  FILLER              PIC X(01)  VALUE '1'.           00302653
                                                                        00302654
001300      03  FILLER.                                                 00302655
001300          04  FILLER              PIC X(02)  VALUE '07'.          00302656
001300          04  FILLER              PIC X(02)  VALUE '0E'.          00302657
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302658
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302659
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302664
001300          04  FILLER              PIC X(01)  VALUE '0'.           00302665
                                                                        00302666
001300      03  FILLER.                                                 00302667
001300          04  FILLER              PIC X(02)  VALUE '08'.          00302668
001300          04  FILLER              PIC X(02)  VALUE '10'.          00302669
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302670
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302671
001300          04  FILLER              PIC X(01)  VALUE '1'.           00303000
001300          04  FILLER              PIC X(01)  VALUE '+'.           00303100
                                                                        00303200
                                                                                
000100*     *** OCCURS # 20.  FIXED, RGO.  ***                          00010002
001300      03  FILLER.                                                 00303300
001300          04  FILLER              PIC X(02)  VALUE '08'.          00303400
001300          04  FILLER              PIC X(02)  VALUE '2F'.          00303500
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00303600
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00303700
001300          04  FILLER              PIC X(01)  VALUE '0'.           00304200
001300          04  FILLER              PIC X(01)  VALUE '+'.           00304300
                                                                        00304400
001300      03  FILLER.                                                 00304500
001300          04  FILLER              PIC X(02)  VALUE '08'.          00304600
001300          04  FILLER              PIC X(02)  VALUE '2C'.          00304700
001300          04  FILLER              PIC X(02)  VALUE '2C'.          00304710
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00304720
001300          04  FILLER              PIC X(01)  VALUE '0'.           00304770
001300          04  FILLER              PIC X(01)  VALUE '+'.           00304780
                                                                        00304790
001300      03  FILLER.                                                 00304791
001300          04  FILLER              PIC X(02)  VALUE '09'.          00304792
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00304793
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00304794
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00304795
001300          04  FILLER              PIC X(01)  VALUE '+'.           00304800
001300          04  FILLER              PIC X(01)  VALUE '1'.           00304810
                                                                        00304820
001300      03  FILLER.                                                 00304830
001300          04  FILLER              PIC X(02)  VALUE '09'.          00304840
001300          04  FILLER              PIC X(02)  VALUE '0H'.          00304850
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00304860
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00304870
001300          04  FILLER              PIC X(01)  VALUE '+'.           00304893
001300          04  FILLER              PIC X(01)  VALUE '0'.           00304894
                                                                        00304895
001300      03  FILLER.                                                 00304896
001300          04  FILLER              PIC X(02)  VALUE '11'.          00304897
001300          04  FILLER              PIC X(02)  VALUE '1J'.          00304898
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00304899
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00304900
001300          04  FILLER              PIC X(01)  VALUE '1'.           00304950
001300          04  FILLER              PIC X(01)  VALUE '1'.           00304960
                                                                        00304970
001300      03  FILLER.                                                 00304980
001300          04  FILLER              PIC X(02)  VALUE '11'.          00304990
001300          04  FILLER              PIC X(02)  VALUE '1K'.          00304991
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00304992
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00304993
001300          04  FILLER              PIC X(01)  VALUE '1'.           00304998
001300          04  FILLER              PIC X(01)  VALUE '0'.           00304999
                                                                        00305000
001300      03  FILLER.                                                 00305010
001300          04  FILLER              PIC X(02)  VALUE '11'.          00305020
001300          04  FILLER              PIC X(02)  VALUE '20'.          00305030
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305040
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305050
001300          04  FILLER              PIC X(01)  VALUE '0'.           00305091
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305092
                                                                        00305093
001300      03  FILLER.                                                 00305094
001300          04  FILLER              PIC X(02)  VALUE '12'.          00305095
001300          04  FILLER              PIC X(02)  VALUE '4P'.          00305096
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305097
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305098
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305130
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305140
                                                                        00305150
001300      03  FILLER.                                                 00305160
001300          04  FILLER              PIC X(02)  VALUE '13'.          00305170
001300          04  FILLER              PIC X(02)  VALUE '4V'.          00305180
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305190
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305191
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305196
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305197
                                                                        00305198
001300      03  FILLER.                                                 00305199
001300          04  FILLER              PIC X(02)  VALUE '14'.          00305200
001300          04  FILLER              PIC X(02)  VALUE '0F'.          00305210
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305220
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305230
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305280
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305290
                                                                        00305198
001300      03  FILLER.                                                 00305199
001300          04  FILLER              PIC X(02)  VALUE '15'.          00305200
001300          04  FILLER              PIC X(02)  VALUE '10'.          00305210
001300          04  FILLER              PIC X(02)  VALUE '0C'.          00305220
001300          04  FILLER              PIC X(02)  VALUE 'ZZ'.          00305230
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305280
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305290
                                                                        00305291
                                                                        00305300
001100                                                                  00306500
058900    02    IPRV-TABLE                 REDEFINES  IPRV-VALUES.      00307200
001300      03  IPRV-TBL                   OCCURS 30 TIMES              00307210
059200                                     INDEXED BY IPRV-INDX         00307500
059200                                               IPRV-MAX-INDX.     00307501
001300          04  IPRV-KEY.                                           00307502
001300              08  IPRV-TAB-PRV     PIC X(02).                     00307503
001300              08  IPRV-CON-PRV     PIC X(02).                     00307504
001300          04  IPRV-TABS-INDS.                                     00307505
001300              08  IPRV-TAB-IND1    PIC X(02).                     00307506
001300              08  IPRV-TAB-IND2    PIC X(02).                     00307507
001300          04  IPRV-PLAN-IND        PIC X(01).                     00307517
001300          04  IPRV-EMPL-IND        PIC X(01).                     00307518
001100                                                                  00307519
058900    02    IPRV-COUNTS.                                            00307520
   300      03  IPRV-ENTRY-CNT     PIC S9(04) COMP  VALUE 29.           00307530
 03100                                                                  00310002
