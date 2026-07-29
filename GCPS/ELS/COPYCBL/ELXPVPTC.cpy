000100******************************************************************00010002
   200*                                                                *00020002
000300*    COPYB00K:   ELXPVIPC                                        *00030002
000400*    DATE:       06-AUG-1992                                     *00040002
000500*    AUTHOR:     BARBARA KEIB * *                                 00050002
000600*    FUNCTION:   TABLE TO DERIVE PROVIDER CONTROL FOR PROFESSION *00060002
000200*                                                                *00060003
000800******************************************************************00060005
000200*                                                                *00060006
000200*    THE TABLE CONSISTS OF THE FOLLOWING FIELDS:                 *00060007
      *                                                                *00060008
 00200* 1. TAB-KEY    = GROUP SPECIFIC BS PROVIDER CONTROL CONT IND    *00060009
 00200* 2. CON-KEY    = CONTRACT FILE PROVIDER CODE DESIRED            *00060010
000200* 3. TAB1-IND1  = GVLP/GVLQ/GVLR PROVIDER INDICATOR 1            *00060030
000200* 4. TAB1-IND2  = GVLP/GVLQ/GVLR PROVIDER INDICATOR 2            *00060031
000200* 5. MPP-PRVD   = PMCI-MPP-PROVIDER                              *00060071
000200* 6. EMPL-IND   = EMPLOYER INDICATOR                             *00060072
000200* 7. PART-PHAR  = PARTICIPATING PHARMACY INDICATOR               *00060073
000200* 8. VISN-CARE  = VISION CARE PROVIDER INDICATOR                 *00060074
      *                                                                *00060080
000800******************************************************************00080002
000900*                                                                *00090002
001000*                      MAINTENANCE HISTORY                       *00100002
001100*                                                                *00110002
001200*  MOD     DATE     BY  DRPT                ACTION               *00120002
001300* ----- ----------- --- ----- ---------------------------------- *00130002
001400* 01.00 06-AUG-1992 BAK       CREATED                            *00140002
001400* 01.01 01-SEP-1992 BAK       ELIMINATED ALL BUT 1 SET OF INDICA.*00140003
002920* 02.00 21-SEP-1993 BAK       CHANGE INDICATOR TO 2 DIGITS.      *00292002
C02920* 02.01 12/16/93    RGO       FIX BUG IN 19TH OCCURANCE.         *00292002
C02920* 03.00 07/20/94    RGO       FIX BUG IN 35TH AND 36TH OCCURANCE.*00292002
C02920*                             INVALID VALUES IN CON-PRV.         *00292002
C02920* 04.00 07/04/95    RGO  -ADDED VALUE 20 0F.                     *00292002
C02920*                        -FIXED 05 0F. IT SHOULD BE 05 2F.       *00292002
C02920*                        -ADDED 07 0D 0DZZ. ZZ IS THE SAME AS 0Z *00292002
C02920*                                                                *00292002
C02920*                                                                *00292002
003000******************************************************************00300002
C02920* FOR A DESCRIPTION OF HOW THE TABLE ENTRIES WERE CREATED,       *00292002
C02920*    SEE DOCUMENTATION IN THE ELXPVITC MEMBER.  RGO.             *00292002
C02920* AS OF 4/95, I THINK THE VISION AND PHARMACY CHECKS ARE USELESS,*00292002
C02920*    AND MAYBE EVEN IN ERROR.                                    *00292002
003000******************************************************************00300002
003200 01  ELS-PPRV-TABLE.                                              00300003
001100                                                                  00300004
001000    02  PPRV-VALUES.                                              00301300
                                                                        00301400
001300      03  FILLER.                                                 00302400
001300          04  FILLER              PIC X(02)  VALUE '00'.          00302410
001300          04  FILLER              PIC X(02)  VALUE '00'.          00302411
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302420
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302421
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302426
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302429
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302430
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302440
                                                                        00302450
001300      03  FILLER.                                                 00302451
001300          04  FILLER              PIC X(02)  VALUE '01'.          00302452
001300          04  FILLER              PIC X(02)  VALUE '0C'.          00302453
001300          04  FILLER              PIC X(02)  VALUE '0C'.          00302454
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302455
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302460
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302461
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302462
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302463
                                                                        00302464
001300      03  FILLER.                                                 00302465
001300          04  FILLER              PIC X(02)  VALUE '01'.          00302466
001300          04  FILLER              PIC X(02)  VALUE '0F'.          00302467
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302468
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302469
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302474
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302475
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302476
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302477
                                                                        00302478
001300      03  FILLER.                                                 00302479
001300          04  FILLER              PIC X(02)  VALUE '02'.          00302480
001300          04  FILLER              PIC X(02)  VALUE '10'.          00302481
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302482
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302483
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302488
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302489
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302490
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302491
                                                                        00302492
001300      03  FILLER.                                                 00302493
001300          04  FILLER              PIC X(02)  VALUE '03'.          00302494
001300          04  FILLER              PIC X(02)  VALUE '3V'.          00302495
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302496
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302497
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302502
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302503
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302504
001300          04  FILLER              PIC X(01)  VALUE '1'.           00302505
                                                                        00302506
001300      03  FILLER.                                                 00302507
001300          04  FILLER              PIC X(02)  VALUE '03'.          00302508
001300          04  FILLER              PIC X(02)  VALUE '3V'.          00302509
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302510
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302511
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302516
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302517
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302518
001300          04  FILLER              PIC X(01)  VALUE 'Z'.           00302519
                                                                        00302520
001300      03  FILLER.                                                 00302521
001300          04  FILLER              PIC X(02)  VALUE '03'.          00302522
001300          04  FILLER              PIC X(02)  VALUE '4V'.          00302523
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302524
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302525
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302530
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302531
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302532
001300          04  FILLER              PIC X(01)  VALUE '0'.           00302533
                                                                        00302534
001300      03  FILLER.                                                 00302535
001300          04  FILLER              PIC X(02)  VALUE '04'.          00302536
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00302537
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00302538
001300          04  FILLER              PIC X(02)  VALUE '0E'.          00302539
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302544
001300          04  FILLER              PIC X(01)  VALUE '1'.           00302545
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302546
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302547
                                                                        00302550
001300      03  FILLER.                                                 00302551
001300          04  FILLER              PIC X(02)  VALUE '04'.          00302552
001300          04  FILLER              PIC X(02)  VALUE '0E'.          00302553
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00302554
001300          04  FILLER              PIC X(02)  VALUE '0E'.          00302555
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302560
001300          04  FILLER              PIC X(01)  VALUE '0'.           00302561
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302562
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302563
                                                                        00302564
001300      03  FILLER.                                                 00302565
001300          04  FILLER              PIC X(02)  VALUE '04'.          00302566
001300          04  FILLER              PIC X(02)  VALUE '0F'.          00302567
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302568
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302569
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302574
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302575
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302576
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302577
                                                                        00302578
001300      03  FILLER.                                                 00302579
001300          04  FILLER              PIC X(02)  VALUE '05'.          00302580
001300          04  FILLER              PIC X(02)  VALUE '2F'.          00302581
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302582
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302583
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302588
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302589
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302590
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302591
                                                                        00302592
001300      03  FILLER.                                                 00302593
001300          04  FILLER              PIC X(02)  VALUE '05'.          00302594
001300          04  FILLER              PIC X(02)  VALUE '1D'.          00302595
001300          04  FILLER              PIC X(02)  VALUE '1D'.          00302596
001300          04  FILLER              PIC X(02)  VALUE '1E'.          00302597
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302602
001300          04  FILLER              PIC X(01)  VALUE '1'.           00302603
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302604
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302605
                                                                        00302606
001300      03  FILLER.                                                 00302607
001300          04  FILLER              PIC X(02)  VALUE '05'.          00302608
001300          04  FILLER              PIC X(02)  VALUE '1E'.          00302609
001300          04  FILLER              PIC X(02)  VALUE '1D'.          00302610
001300          04  FILLER              PIC X(02)  VALUE '1E'.          00302611
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302616
001300          04  FILLER              PIC X(01)  VALUE '0'.           00302617
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302618
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302619
                                                                        00302620
001300      03  FILLER.                                                 00302621
001300          04  FILLER              PIC X(02)  VALUE '06'.          00302622
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00302623
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00302624
001300          04  FILLER              PIC X(02)  VALUE '0E'.          00302625
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302630
001300          04  FILLER              PIC X(01)  VALUE '1'.           00302631
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302632
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302633
                                                                        00302634
001300      03  FILLER.                                                 00302635
001300          04  FILLER              PIC X(02)  VALUE '06'.          00302636
001300          04  FILLER              PIC X(02)  VALUE '0E'.          00302637
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00302638
001300          04  FILLER              PIC X(02)  VALUE '0E'.          00302639
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302644
001300          04  FILLER              PIC X(01)  VALUE '0'.           00302645
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302646
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302647
                                                                        00302648
001300      03  FILLER.                                                 00302649
001300          04  FILLER              PIC X(02)  VALUE '06'.          00302650
001300          04  FILLER              PIC X(02)  VALUE '0F'.          00302651
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302652
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302653
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302658
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302659
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302660
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302661
                                                                        00302662
001300      03  FILLER.                                                 00302663
001300          04  FILLER              PIC X(02)  VALUE '07'.          00302664
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00302665
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00302666
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302667
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302672
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302673
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302674
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302675
001300      03  FILLER.                                                 00302663
001300          04  FILLER              PIC X(02)  VALUE '07'.          00302664
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00302665
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00302666
001300          04  FILLER              PIC X(02)  VALUE 'ZZ'.          00302667
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302672
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302673
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302674
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302675
                                                                        00302676
001300      03  FILLER.                                                 00302677
001300          04  FILLER              PIC X(02)  VALUE '07'.          00302678
001300          04  FILLER              PIC X(02)  VALUE '0H'.          00302679
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302680
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302681
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302686
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302687
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302688
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302689
                                                                        00302690
001300      03  FILLER.                                                 00302691
001300          04  FILLER              PIC X(02)  VALUE '08'.          00302692
001300          04  FILLER              PIC X(02)  VALUE '1A'.          00302693
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302694
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302695
001300          04  FILLER              PIC X(01)  VALUE '+'.           00303000
001300          04  FILLER              PIC X(01)  VALUE '1'.           00303100
001300          04  FILLER              PIC X(01)  VALUE '+'.           00303110
001300          04  FILLER              PIC X(01)  VALUE '+'.           00303120
                                                                        00303200
001300      03  FILLER.                                                 00303300
001300          04  FILLER              PIC X(02)  VALUE '08'.          00303400
001300          04  FILLER              PIC X(02)  VALUE '1B'.          00303500
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00303600
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00303700
001300          04  FILLER              PIC X(01)  VALUE '+'.           00304300
001300          04  FILLER              PIC X(01)  VALUE '0'.           00304310
001300          04  FILLER              PIC X(01)  VALUE '+'.           00304330
001300          04  FILLER              PIC X(01)  VALUE '+'.           00304340
                                                                        00304400
001300      03  FILLER.                                                 00304500
001300          04  FILLER              PIC X(02)  VALUE '09'.          00304600
001300          04  FILLER              PIC X(02)  VALUE '0F'.          00304700
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00304710
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00304720
001300          04  FILLER              PIC X(01)  VALUE '+'.           00304781
001300          04  FILLER              PIC X(01)  VALUE '+'.           00304782
001300          04  FILLER              PIC X(01)  VALUE '+'.           00304783
001300          04  FILLER              PIC X(01)  VALUE '+'.           00304784
                                                                        00304790
001300      03  FILLER.                                                 00304791
001300          04  FILLER              PIC X(02)  VALUE '10'.          00304792
001300          04  FILLER              PIC X(02)  VALUE '0C'.          00304793
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00304794
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00304795
001300          04  FILLER              PIC X(01)  VALUE '1'.           00304800
001300          04  FILLER              PIC X(01)  VALUE '+'.           00304810
001300          04  FILLER              PIC X(01)  VALUE '+'.           00304811
001300          04  FILLER              PIC X(01)  VALUE '+'.           00304812
                                                                        00304820
001300      03  FILLER.                                                 00304830
001300          04  FILLER              PIC X(02)  VALUE '10'.          00304840
001300          04  FILLER              PIC X(02)  VALUE '0F'.          00304850
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00304860
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00304870
001300          04  FILLER              PIC X(01)  VALUE '0'.           00304893
001300          04  FILLER              PIC X(01)  VALUE '+'.           00304894
001300          04  FILLER              PIC X(01)  VALUE '+'.           00304895
001300          04  FILLER              PIC X(01)  VALUE '+'.           00304896
                                                                        00304897
001300      03  FILLER.                                                 00304898
001300          04  FILLER              PIC X(02)  VALUE '12'.          00304899
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00304900
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00304901
001300          04  FILLER              PIC X(02)  VALUE '0E'.          00304902
001300          04  FILLER              PIC X(01)  VALUE '+'.           00304950
001300          04  FILLER              PIC X(01)  VALUE '1'.           00304960
001300          04  FILLER              PIC X(01)  VALUE '+'.           00304961
001300          04  FILLER              PIC X(01)  VALUE '+'.           00304962
                                                                        00304970
001300      03  FILLER.                                                 00304980
001300          04  FILLER              PIC X(02)  VALUE '12'.          00304990
001300          04  FILLER              PIC X(02)  VALUE '0E'.          00304991
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00304992
001300          04  FILLER              PIC X(02)  VALUE '0E'.          00304993
001300          04  FILLER              PIC X(01)  VALUE '+'.           00304998
001300          04  FILLER              PIC X(01)  VALUE '0'.           00304999
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305000
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305001
                                                                        00305002
001300      03  FILLER.                                                 00305010
001300          04  FILLER              PIC X(02)  VALUE '12'.          00305020
001300          04  FILLER              PIC X(02)  VALUE '0F'.          00305030
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305040
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305050
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305091
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305092
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305093
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305094
                                                                        00305095
001300      03  FILLER.                                                 00305096
001300          04  FILLER              PIC X(02)  VALUE '12'.          00305097
001300          04  FILLER              PIC X(02)  VALUE '0S'.          00305098
001300          04  FILLER              PIC X(02)  VALUE '0S'.          00305101
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305102
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305130
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305140
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305141
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305142
                                                                        00305150
001300      03  FILLER.                                                 00305160
001300          04  FILLER              PIC X(02)  VALUE '13'.          00305170
001300          04  FILLER              PIC X(02)  VALUE '1B'.          00305180
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305190
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305191
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305196
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305197
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305198
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305199
                                                                        00305200
001300      03  FILLER.                                                 00305201
001300          04  FILLER              PIC X(02)  VALUE '14'.          00305202
001300          04  FILLER              PIC X(02)  VALUE '0B'.          00305210
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305220
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305230
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305280
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305290
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305291
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305292
                                                                        00305293
001300      03  FILLER.                                                 00305294
001300          04  FILLER              PIC X(02)  VALUE '15'.          00305295
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00305296
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00305297
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305298
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305330
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305340
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305350
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305360
                                                                        00305370
001300      03  FILLER.                                                 00305371
001300          04  FILLER              PIC X(02)  VALUE '15'.          00305372
001300          04  FILLER              PIC X(02)  VALUE '0H'.          00305373
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305374
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305375
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305380
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305381
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305382
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305383
                                                                        00305384
001300      03  FILLER.                                                 00305385
001300          04  FILLER              PIC X(02)  VALUE '15'.          00305386
001300          04  FILLER              PIC X(02)  VALUE '0S'.          00305387
001300          04  FILLER              PIC X(02)  VALUE '0S'.          00305390
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305391
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305394
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305395
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305396
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305397
                                                                        00305398
001300      03  FILLER.                                                 00305399
001300          04  FILLER              PIC X(02)  VALUE '16'.          00305400
001300          04  FILLER              PIC X(02)  VALUE '0A'.          00305401
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305402
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305403
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305408
001300          04  FILLER              PIC X(01)  VALUE '1'.           00305409
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305410
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305411
                                                                        00305412
001300      03  FILLER.                                                 00305413
001300          04  FILLER              PIC X(02)  VALUE '16'.          00305414
001300          04  FILLER              PIC X(02)  VALUE '0B'.          00305415
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305416
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305417
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305422
001300          04  FILLER              PIC X(01)  VALUE '0'.           00305423
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305424
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305425
                                                                        00305426
000100*  *** THE 35TH OCCURANCE. RGO           *************************00010002
001300      03  FILLER.                                                 00305427
001300          04  FILLER              PIC X(02)  VALUE '17'.          00305428
001300          04  FILLER              PIC X(02)  VALUE '1J'.          00305429
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00305430
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305431
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305436
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305437
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305438
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305439
                                                                        00305440
001300      03  FILLER.                                                 00305441
001300          04  FILLER              PIC X(02)  VALUE '17'.          00305442
001300          04  FILLER              PIC X(02)  VALUE '1K'.          00305450
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305460
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305470
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305493
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305494
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305495
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305496
                                                                        00305497
001300      03  FILLER.                                                 00305498
001300          04  FILLER              PIC X(02)  VALUE '18'.          00305499
001300          04  FILLER              PIC X(02)  VALUE '3P'.          00305500
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305510
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305520
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305570
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305580
001300          04  FILLER              PIC X(01)  VALUE '1'.           00305590
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305591
                                                                        00305592
001300      03  FILLER.                                                 00305593
001300          04  FILLER              PIC X(02)  VALUE '18'.          00305594
001300          04  FILLER              PIC X(02)  VALUE '4P'.          00305595
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305596
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305597
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305620
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305630
001300          04  FILLER              PIC X(01)  VALUE 'Z'.           00305640
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305650
                                                                        00305660
001300      03  FILLER.                                                 00305670
001300          04  FILLER              PIC X(02)  VALUE '18'.          00305680
001300          04  FILLER              PIC X(02)  VALUE '4P'.          00305690
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305691
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305692
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305697
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305698
001300          04  FILLER              PIC X(01)  VALUE '0'.           00305699
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305700
                                                                        00305710
001300      03  FILLER.                                                 00305720
001300          04  FILLER              PIC X(02)  VALUE '19'.          00305730
001300          04  FILLER              PIC X(02)  VALUE '3V'.          00305740
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305750
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305760
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305792
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305793
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305794
001300          04  FILLER              PIC X(01)  VALUE 'Z'.           00305795
                                                                        00305796
001300      03  FILLER.                                                 00305797
001300          04  FILLER              PIC X(02)  VALUE '19'.          00305798
001300          04  FILLER              PIC X(02)  VALUE '3V'.          00305799
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305800
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305810
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305860
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305870
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305880
001300          04  FILLER              PIC X(01)  VALUE '1'.           00305890
                                                                        00305891
001300      03  FILLER.                                                 00305892
001300          04  FILLER              PIC X(02)  VALUE '19'.          00305893
001300          04  FILLER              PIC X(02)  VALUE '4V'.          00305894
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305895
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305896
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305910
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305920
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305930
001300          04  FILLER              PIC X(01)  VALUE '0'.           00305940
                                                                        00305950
001300      03  FILLER.                                                 00305892
001300          04  FILLER              PIC X(02)  VALUE '20'.          00305893
001300          04  FILLER              PIC X(02)  VALUE '0F'.          00305894
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305895
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00305896
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305910
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305920
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305930
001300          04  FILLER              PIC X(01)  VALUE '+'.           00305940
                                                                        00306000
001100                                                                  00306500
058900    02    PPRV-TABLE                 REDEFINES  PPRV-VALUES.      00307200
001300      03  PPRV-TBL                   OCCURS 44 TIMES              00307210
059200                                     INDEXED BY PPRV-INDX         00307500
059200                                               PPRV-MAX-INDX.     00307501
001300          04  PPRV-KEY.                                           00307502
001300              08  PPRV-TAB-PRV     PIC X(02).                     00307503
001300              08  PPRV-C0N-PRV     PIC X(02).                     00307504
001300          04  PPRV-TABS-INDS.                                     00307505
001300              08  PPRV-TAB-IND1    PIC X(02).                     00307506
001300              08  PPRV-TAB-IND2    PIC X(02).                     00307507
001300          04  PPRV-MPP-IND         PIC X(01).                     00307517
001300          04  PPRV-EMPL-IND        PIC X(01).                     00307518
001300          04  PPRV-PHAR-IND        PIC X(01).                     00307519
001300          04  PPRV-VISN-IND        PIC X(01).                     00307520
001100                                                                  00307521
058900    02    PPRV-C0UNTS.                                            00307522
   300      03  PPRV-ENTRY-CNT     PIC S9(04) COMP  VALUE 44.           00307530
 03100                                                                  00310002
