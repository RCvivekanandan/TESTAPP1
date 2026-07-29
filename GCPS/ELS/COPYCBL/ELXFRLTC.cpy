000100******************************************************************00010002
000200*                                                                *00020002
000300*    COPYBOOK:   ELXFRLTC                                        *00030002
000400*    DATE:       21-JUL-1992                                     *00040002
000500*    AUTHOR:     BARBARA KEIB                                     00050002
000600*    FUNCTION:   FAMILY RELATIONSHIP TABLE                       *00060002
000200*                                                                *00060003
000800******************************************************************00080002
000900*                                                                *00090002
001000*                      MAINTENANCE HISTORY                       *00100002
001100*                                                                *00110002
001200*  MOD     DATE     BY  DRPT                ACTION               *00120002
001300* ----- ----------- --- ----- ---------------------------------- *00130002
001400* 01.00 21-JUL-1992 BAK       CREATED                            *00140002
001400* 01.01 03-FEB-1993 BAK       ADD 10-13 FAMILY RELATIONSHIPS     *00140003
001400* 02.00 11-OCT-1993 BAK       ADD 14-25 FAMILY RELATIONSHIPS     *00140003
001400* 02.01 01-NOV-1993 BAK       EXPAND TABLE TO ADD INDICATOR FOR  *00140003
002920*                             MEDICARE USE ONLY-IND-MED          *00292002
001400* 02.02 12-NOV-1993 BAK       ADDITIONAL CHANGES TO MEDICARE     *00140003
001400* 03.00 10-APR-1995 RGO       FIXED THE OCCURS VALUE AND         *00140003
001400*                             THE ENTRY-COUNT.                   *00140003
003000******************************************************************00300002
003200 01  ELS-FML-TABLE.                                               00300003
001100                                                                  00300004
001000    02  FML-VALUES.                                               00301300
                                                                        00301400
001300      03  FILLER.                                                 00302400
001300          04  FILLER              PIC X(02)  VALUE '0A'.          00302410
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00302420
001300          04  FILLER              PIC X(03)  VALUE '000'.         00302421
001300          04  FILLER              PIC X(03)  VALUE '039'.         00302430
                04  FILLER              PIC X(01)  VALUE ' '.           00302440
                04  FILLER              PIC X(01)  VALUE ' '.           00302440
                                                                        00302450
001300      03  FILLER.                                                 00302460
001300          04  FILLER              PIC X(02)  VALUE '0B'.          00302470
001300          04  FILLER              PIC X(01)  VALUE 'M'.           00302480
001300          04  FILLER              PIC X(03)  VALUE '000'.         00302490
001300          04  FILLER              PIC X(03)  VALUE '999'.         00302491
                04  FILLER              PIC X(01)  VALUE ' '.           00302492
                04  FILLER              PIC X(01)  VALUE ' '.           00302492
                                                                        00302493
001300      03  FILLER.                                                 00302494
001300          04  FILLER              PIC X(02)  VALUE '0B'.          00302495
001300          04  FILLER              PIC X(01)  VALUE 'S'.           00302496
001300          04  FILLER              PIC X(03)  VALUE '000'.         00302497
001300          04  FILLER              PIC X(03)  VALUE '999'.         00302498
                04  FILLER              PIC X(01)  VALUE ' '.           00302499
                04  FILLER              PIC X(01)  VALUE ' '.           00302499
                                                                        00302500
001300      03  FILLER.                                                 00302501
001300          04  FILLER              PIC X(02)  VALUE '0B'.          00302502
001300          04  FILLER              PIC X(01)  VALUE 'D'.           00302503
001300          04  FILLER              PIC X(03)  VALUE '000'.         00302504
001300          04  FILLER              PIC X(03)  VALUE '012'.         00302505
                04  FILLER              PIC X(01)  VALUE 'Y'.           00302506
                04  FILLER              PIC X(01)  VALUE ' '.           00302506
                                                                        00302507
001300      03  FILLER.                                                 00302508
001300          04  FILLER              PIC X(02)  VALUE '0B'.          00302509
001300          04  FILLER              PIC X(01)  VALUE 'D'.           00302510
001300          04  FILLER              PIC X(03)  VALUE '019'.         00302511
001300          04  FILLER              PIC X(03)  VALUE '999'.         00302512
                04  FILLER              PIC X(01)  VALUE ' '.           00302513
                04  FILLER              PIC X(01)  VALUE ' '.           00302513
                                                                        00302514
001300      03  FILLER.                                                 00302515
001300          04  FILLER              PIC X(02)  VALUE '0C'.          00302516
001300          04  FILLER              PIC X(01)  VALUE 'D'.           00302517
001300          04  FILLER              PIC X(03)  VALUE '013'.         00302518
001300          04  FILLER              PIC X(03)  VALUE '018'.         00302519
                04  FILLER              PIC X(01)  VALUE ' '.           00302520
                04  FILLER              PIC X(01)  VALUE ' '.           00302520
                                                                        00302521
001300      03  FILLER.                                                 00302522
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00302523
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00302530
001300          04  FILLER              PIC X(03)  VALUE '016'.         00302540
001300          04  FILLER              PIC X(03)  VALUE '999'.         00302550
                04  FILLER              PIC X(01)  VALUE ' '.           00302560
                04  FILLER              PIC X(01)  VALUE ' '.           00302560
                                                                        00302570
001300      03  FILLER.                                                 00302580
001300          04  FILLER              PIC X(02)  VALUE '0E'.          00302590
001300          04  FILLER              PIC X(01)  VALUE 'D'.           00302591
001300          04  FILLER              PIC X(03)  VALUE '000'.         00302592
001300          04  FILLER              PIC X(03)  VALUE '015'.         00302593
                04  FILLER              PIC X(01)  VALUE ' '.           00302594
                04  FILLER              PIC X(01)  VALUE ' '.           00302594
                                                                        00302595
001300      03  FILLER.                                                 00302596
001300          04  FILLER              PIC X(02)  VALUE '0F'.          00302597
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00302598
001300          04  FILLER              PIC X(03)  VALUE '003'.         00302599
001300          04  FILLER              PIC X(03)  VALUE '999'.         00302600
                04  FILLER              PIC X(01)  VALUE ' '.           00302610
                04  FILLER              PIC X(01)  VALUE ' '.           00302610
                                                                        00302620
001300      03  FILLER.                                                 00302630
001300          04  FILLER              PIC X(02)  VALUE '0G'.          00302640
001300          04  FILLER              PIC X(01)  VALUE 'D'.           00302650
001300          04  FILLER              PIC X(03)  VALUE '000'.         00302660
001300          04  FILLER              PIC X(03)  VALUE '000'.         00302670
                04  FILLER              PIC X(01)  VALUE ' '.           00302680
                04  FILLER              PIC X(01)  VALUE ' '.           00302680
                                                                        00302690
001300      03  FILLER.                                                 00302691
001300          04  FILLER              PIC X(02)  VALUE '0H'.          00302692
001300          04  FILLER              PIC X(01)  VALUE 'D'.           00302693
001300          04  FILLER              PIC X(03)  VALUE '001'.         00302694
001300          04  FILLER              PIC X(03)  VALUE '002'.         00302695
                04  FILLER              PIC X(01)  VALUE ' '.           00302696
                04  FILLER              PIC X(01)  VALUE ' '.           00302696
                                                                        00302697
001300      03  FILLER.                                                 00302698
001300          04  FILLER              PIC X(02)  VALUE '0I'.          00302699
001300          04  FILLER              PIC X(01)  VALUE 'M'.           00302700
001300          04  FILLER              PIC X(03)  VALUE '000'.         00302710
001300          04  FILLER              PIC X(03)  VALUE '039'.         00302720
                04  FILLER              PIC X(01)  VALUE ' '.           00302730
                04  FILLER              PIC X(01)  VALUE ' '.           00302730
                                                                        00302740
001300      03  FILLER.                                                 00302750
001300          04  FILLER              PIC X(02)  VALUE '0J'.          00302760
001300          04  FILLER              PIC X(01)  VALUE 'M'.           00302770
001300          04  FILLER              PIC X(03)  VALUE '040'.         00302780
001300          04  FILLER              PIC X(03)  VALUE '999'.         00302790
                04  FILLER              PIC X(01)  VALUE ' '.           00302791
                04  FILLER              PIC X(01)  VALUE ' '.           00302791
                                                                        00302792
001300      03  FILLER.                                                 00302793
001300          04  FILLER              PIC X(02)  VALUE '0K'.          00302794
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00302795
001300          04  FILLER              PIC X(03)  VALUE '000'.         00302796
001300          04  FILLER              PIC X(03)  VALUE '064'.         00302797
                04  FILLER              PIC X(01)  VALUE ' '.           00302798
                04  FILLER              PIC X(01)  VALUE ' '.           00302798
                                                                        00302799
001300      03  FILLER.                                                 00302800
001300          04  FILLER              PIC X(02)  VALUE '0L'.          00302810
001300          04  FILLER              PIC X(01)  VALUE 'D'.           00302820
001300          04  FILLER              PIC X(03)  VALUE '001'.         00302830
001300          04  FILLER              PIC X(03)  VALUE '016'.         00302840
                04  FILLER              PIC X(01)  VALUE ' '.           00302850
                04  FILLER              PIC X(01)  VALUE ' '.           00302850
                                                                        00302860
001300      03  FILLER.                                                 00302870
001300          04  FILLER              PIC X(02)  VALUE '0M'.          00302880
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00302890
001300          04  FILLER              PIC X(03)  VALUE '000'.         00302891
001300          04  FILLER              PIC X(03)  VALUE '999'.         00302892
                04  FILLER              PIC X(01)  VALUE ' '.           00302893
                04  FILLER              PIC X(01)  VALUE ' '.           00302893
                                                                        00302894
001300      03  FILLER.                                                 00302895
001300          04  FILLER              PIC X(02)  VALUE '0P'.          00302896
001300          04  FILLER              PIC X(01)  VALUE 'D'.           00302897
001300          04  FILLER              PIC X(03)  VALUE '000'.         00302898
001300          04  FILLER              PIC X(03)  VALUE '012'.         00302899
                04  FILLER              PIC X(01)  VALUE ' '.           00302900
                04  FILLER              PIC X(01)  VALUE ' '.           00302900
                                                                        00302910
001300      03  FILLER.                                                 00302920
001300          04  FILLER              PIC X(02)  VALUE '0R'.          00302930
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00302940
001300          04  FILLER              PIC X(03)  VALUE '019'.         00302950
001300          04  FILLER              PIC X(03)  VALUE '999'.         00302960
                04  FILLER              PIC X(01)  VALUE ' '.           00302970
                04  FILLER              PIC X(01)  VALUE ' '.           00302970
                                                                        00302980
001300      03  FILLER.                                                 00302990
001300          04  FILLER              PIC X(02)  VALUE '0S'.          00302991
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00302992
001300          04  FILLER              PIC X(03)  VALUE '000'.         00302993
001300          04  FILLER              PIC X(03)  VALUE '002'.         00302994
                04  FILLER              PIC X(01)  VALUE ' '.           00302995
                04  FILLER              PIC X(01)  VALUE ' '.           00302995
                                                                        00302996
001300      03  FILLER.                                                 00302997
001300          04  FILLER              PIC X(02)  VALUE '0T'.          00302998
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00302999
001300          04  FILLER              PIC X(03)  VALUE '003'.         00303000
001300          04  FILLER              PIC X(03)  VALUE '018'.         00303010
                04  FILLER              PIC X(01)  VALUE ' '.           00303020
                04  FILLER              PIC X(01)  VALUE ' '.           00303020
                                                                        00303030
001300      03  FILLER.                                                 00303040
001300          04  FILLER              PIC X(02)  VALUE '0U'.          00303050
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303060
001300          04  FILLER              PIC X(03)  VALUE '019'.         00303070
001300          04  FILLER              PIC X(03)  VALUE '040'.         00303080
                04  FILLER              PIC X(01)  VALUE ' '.           00303090
                04  FILLER              PIC X(01)  VALUE ' '.           00303090
                                                                        00303091
001300      03  FILLER.                                                 00303092
001300          04  FILLER              PIC X(02)  VALUE '0V'.          00303093
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303094
001300          04  FILLER              PIC X(03)  VALUE '041'.         00303095
001300          04  FILLER              PIC X(03)  VALUE '999'.         00303096
                04  FILLER              PIC X(01)  VALUE ' '.           00303097
                04  FILLER              PIC X(01)  VALUE ' '.           00303097
                                                                        00303098
001300      03  FILLER.                                                 00303099
001300          04  FILLER              PIC X(02)  VALUE '0W'.          00303100
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303110
001300          04  FILLER              PIC X(03)  VALUE '000'.         00303120
001300          04  FILLER              PIC X(03)  VALUE '024'.         00303130
                04  FILLER              PIC X(01)  VALUE ' '.           00303140
                04  FILLER              PIC X(01)  VALUE ' '.           00303140
                                                                        00303150
001300      03  FILLER.                                                 00303160
001300          04  FILLER              PIC X(02)  VALUE '0X'.          00303170
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303180
001300          04  FILLER              PIC X(03)  VALUE '025'.         00303190
001300          04  FILLER              PIC X(03)  VALUE '044'.         00303191
                04  FILLER              PIC X(01)  VALUE ' '.           00303192
                04  FILLER              PIC X(01)  VALUE ' '.           00303192
                                                                        00303193
001300      03  FILLER.                                                 00303194
001300          04  FILLER              PIC X(02)  VALUE '0Y'.          00303195
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303196
001300          04  FILLER              PIC X(03)  VALUE '045'.         00303197
001300          04  FILLER              PIC X(03)  VALUE '054'.         00303198
                04  FILLER              PIC X(01)  VALUE ' '.           00303199
                04  FILLER              PIC X(01)  VALUE ' '.           00303199
                                                                        00303200
001300      03  FILLER.                                                 00303210
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00303220
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303230
001300          04  FILLER              PIC X(03)  VALUE '055'.         00303240
001300          04  FILLER              PIC X(03)  VALUE '999'.         00303250
                04  FILLER              PIC X(01)  VALUE ' '.           00303260
                04  FILLER              PIC X(01)  VALUE ' '.           00303260
                                                                        00303270
001300      03  FILLER.                                                 00303280
001300          04  FILLER              PIC X(02)  VALUE '00'.          00303290
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303291
001300          04  FILLER              PIC X(03)  VALUE '000'.         00303292
001300          04  FILLER              PIC X(03)  VALUE '999'.         00303293
                04  FILLER              PIC X(01)  VALUE ' '.           00303294
                04  FILLER              PIC X(01)  VALUE ' '.           00303294
                                                                        00303295
001300      03  FILLER.                                                 00303296
001300          04  FILLER              PIC X(02)  VALUE '01'.          00303297
001300          04  FILLER              PIC X(01)  VALUE 'M'.           00303298
001300          04  FILLER              PIC X(03)  VALUE '000'.         00303299
001300          04  FILLER              PIC X(03)  VALUE '999'.         00303300
                04  FILLER              PIC X(01)  VALUE ' '.           00303310
                04  FILLER              PIC X(01)  VALUE ' '.           00303310
                                                                        00303320
001300      03  FILLER.                                                 00303330
001300          04  FILLER              PIC X(02)  VALUE '02'.          00303340
001300          04  FILLER              PIC X(01)  VALUE 'S'.           00303350
001300          04  FILLER              PIC X(03)  VALUE '000'.         00303360
001300          04  FILLER              PIC X(03)  VALUE '999'.         00303370
                04  FILLER              PIC X(01)  VALUE ' '.           00303380
                04  FILLER              PIC X(01)  VALUE ' '.           00303380
                                                                        00303390
001300      03  FILLER.                                                 00303391
001300          04  FILLER              PIC X(02)  VALUE '03'.          00303392
001300          04  FILLER              PIC X(01)  VALUE 'M'.           00303393
001300          04  FILLER              PIC X(03)  VALUE '000'.         00303394
001300          04  FILLER              PIC X(03)  VALUE '999'.         00303395
                04  FILLER              PIC X(01)  VALUE ' '.           00303396
                04  FILLER              PIC X(01)  VALUE ' '.           00303396
                                                                        00303397
001300      03  FILLER.                                                 00303398
001300          04  FILLER              PIC X(02)  VALUE '03'.          00303399
001300          04  FILLER              PIC X(01)  VALUE 'S'.           00303400
001300          04  FILLER              PIC X(03)  VALUE '000'.         00303410
001300          04  FILLER              PIC X(03)  VALUE '999'.         00303420
                04  FILLER              PIC X(01)  VALUE ' '.           00303430
                04  FILLER              PIC X(01)  VALUE ' '.           00303430
                                                                        00303440
001300      03  FILLER.                                                 00303450
001300          04  FILLER              PIC X(02)  VALUE '04'.          00303460
001300          04  FILLER              PIC X(01)  VALUE 'D'.           00303470
001300          04  FILLER              PIC X(03)  VALUE '000'.         00303480
001300          04  FILLER              PIC X(03)  VALUE '999'.         00303490
                04  FILLER              PIC X(01)  VALUE ' '.           00303491
                04  FILLER              PIC X(01)  VALUE ' '.           00303491
                                                                        00303492
001300      03  FILLER.                                                 00303493
001300          04  FILLER              PIC X(02)  VALUE '05'.          00303494
001300          04  FILLER              PIC X(01)  VALUE 'S'.           00303495
001300          04  FILLER              PIC X(03)  VALUE '000'.         00303496
001300          04  FILLER              PIC X(03)  VALUE '999'.         00303497
                04  FILLER              PIC X(01)  VALUE ' '.           00303498
                04  FILLER              PIC X(01)  VALUE ' '.           00303498
                                                                        00303499
001300      03  FILLER.                                                 00303500
001300          04  FILLER              PIC X(02)  VALUE '05'.          00303510
001300          04  FILLER              PIC X(01)  VALUE 'D'.           00303520
001300          04  FILLER              PIC X(03)  VALUE '000'.         00303530
001300          04  FILLER              PIC X(03)  VALUE '999'.         00303540
                04  FILLER              PIC X(01)  VALUE ' '.           00303550
                04  FILLER              PIC X(01)  VALUE ' '.           00303550
                                                                        00303560
001300      03  FILLER.                                                 00303570
001300          04  FILLER              PIC X(02)  VALUE '06'.          00303580
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303590
001300          04  FILLER              PIC X(03)  VALUE '065'.         00303591
001300          04  FILLER              PIC X(03)  VALUE '999'.         00303592
                04  FILLER              PIC X(01)  VALUE ' '.           00303593
                04  FILLER              PIC X(01)  VALUE ' '.           00303593
                                                                        00303594
001300      03  FILLER.                                                 00303595
001300          04  FILLER              PIC X(02)  VALUE '07'.          00303596
001300          04  FILLER              PIC X(01)  VALUE 'M'.           00303597
001300          04  FILLER              PIC X(03)  VALUE '000'.         00303598
001300          04  FILLER              PIC X(03)  VALUE '064'.         00303599
                04  FILLER              PIC X(01)  VALUE ' '.           00303600
                04  FILLER              PIC X(01)  VALUE ' '.           00303600
                                                                        00303610
001300      03  FILLER.                                                 00303620
001300          04  FILLER              PIC X(02)  VALUE '07'.          00303630
001300          04  FILLER              PIC X(01)  VALUE 'S'.           00303640
001300          04  FILLER              PIC X(03)  VALUE '023'.         00303650
001300          04  FILLER              PIC X(03)  VALUE '064'.         00303660
                04  FILLER              PIC X(01)  VALUE ' '.           00303670
                04  FILLER              PIC X(01)  VALUE ' '.           00303670
                                                                        00303680
001300      03  FILLER.                                                 00303690
001300          04  FILLER              PIC X(02)  VALUE '07'.          00303691
001300          04  FILLER              PIC X(01)  VALUE 'D'.           00303692
001300          04  FILLER              PIC X(03)  VALUE '023'.         00303693
001300          04  FILLER              PIC X(03)  VALUE '064'.         00303694
                04  FILLER              PIC X(01)  VALUE ' '.           00303695
                04  FILLER              PIC X(01)  VALUE ' '.           00303695
                                                                        00303696
001300      03  FILLER.                                                 00303697
001300          04  FILLER              PIC X(02)  VALUE '08'.          00303698
001300          04  FILLER              PIC X(01)  VALUE 'S'.           00303699
001300          04  FILLER              PIC X(03)  VALUE '000'.         00303700
001300          04  FILLER              PIC X(03)  VALUE '022'.         00303710
                04  FILLER              PIC X(01)  VALUE ' '.           00303720
                04  FILLER              PIC X(01)  VALUE ' '.           00303720
                                                                        00303730
001300      03  FILLER.                                                 00303740
001300          04  FILLER              PIC X(02)  VALUE '08'.          00303750
001300          04  FILLER              PIC X(01)  VALUE 'D'.           00303760
001300          04  FILLER              PIC X(03)  VALUE '000'.         00303770
001300          04  FILLER              PIC X(03)  VALUE '022'.         00303780
                04  FILLER              PIC X(01)  VALUE ' '.           00303790
                04  FILLER              PIC X(01)  VALUE ' '.           00303790
                                                                        00303791
001300      03  FILLER.                                                 00303792
001300          04  FILLER              PIC X(02)  VALUE '09'.          00303793
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303794
001300          04  FILLER              PIC X(03)  VALUE '040'.         00303795
001300          04  FILLER              PIC X(03)  VALUE '999'.         00303796
                04  FILLER              PIC X(01)  VALUE ' '.           00303797
                04  FILLER              PIC X(01)  VALUE ' '.           00303797
                                                                        00303798
001300      03  FILLER.                                                 00303799
001300          04  FILLER              PIC X(02)  VALUE '10'.          00303800
001300          04  FILLER              PIC X(01)  VALUE 'M'.           00303810
001300          04  FILLER              PIC X(03)  VALUE '000'.         00303820
001300          04  FILLER              PIC X(03)  VALUE '018'.         00303830
                04  FILLER              PIC X(01)  VALUE ' '.           00303840
                04  FILLER              PIC X(01)  VALUE ' '.           00303840
                                                                        00303850
001300      03  FILLER.                                                 00303860
001300          04  FILLER              PIC X(02)  VALUE '11'.          00303870
001300          04  FILLER              PIC X(01)  VALUE 'M'.           00303880
001300          04  FILLER              PIC X(03)  VALUE '019'.         00303890
001300          04  FILLER              PIC X(03)  VALUE '999'.         00303891
                04  FILLER              PIC X(01)  VALUE ' '.           00303892
                04  FILLER              PIC X(01)  VALUE ' '.           00303892
                                                                        00303893
001300      03  FILLER.                                                 00303894
001300          04  FILLER              PIC X(02)  VALUE '12'.          00303895
001300          04  FILLER              PIC X(01)  VALUE 'S'.           00303896
001300          04  FILLER              PIC X(03)  VALUE '000'.         00303897
001300          04  FILLER              PIC X(03)  VALUE '018'.         00303898
                04  FILLER              PIC X(01)  VALUE ' '.           00303899
                04  FILLER              PIC X(01)  VALUE ' '.           00303899
                                                                        00303900
001300      03  FILLER.                                                 00303910
001300          04  FILLER              PIC X(02)  VALUE '12'.          00303920
001300          04  FILLER              PIC X(01)  VALUE 'D'.           00303930
001300          04  FILLER              PIC X(03)  VALUE '000'.         00303940
001300          04  FILLER              PIC X(03)  VALUE '018'.         00303950
                04  FILLER              PIC X(01)  VALUE ' '.           00303960
                04  FILLER              PIC X(01)  VALUE ' '.           00303960
                                                                        00303970
001300      03  FILLER.                                                 00303980
001300          04  FILLER              PIC X(02)  VALUE '13'.          00303990
001300          04  FILLER              PIC X(01)  VALUE 'S'.           00303991
001300          04  FILLER              PIC X(03)  VALUE '019'.         00303992
001300          04  FILLER              PIC X(03)  VALUE '999'.         00303993
                04  FILLER              PIC X(01)  VALUE ' '.           00303994
                04  FILLER              PIC X(01)  VALUE ' '.           00303994
                                                                        00303995
001300      03  FILLER.                                                 00303996
001300          04  FILLER              PIC X(02)  VALUE '13'.          00303997
001300          04  FILLER              PIC X(01)  VALUE 'D'.           00303998
001300          04  FILLER              PIC X(03)  VALUE '019'.         00303999
001300          04  FILLER              PIC X(03)  VALUE '999'.         00304000
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                                                                        00303995
001300      03  FILLER.                                                 00303996
001300          04  FILLER              PIC X(02)  VALUE '14'.          00303997
001300          04  FILLER              PIC X(01)  VALUE 'M'.           00303998
001300          04  FILLER              PIC X(03)  VALUE '000'.         00303999
001300          04  FILLER              PIC X(03)  VALUE '999'.         00304000
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                                                                        00303995
001300      03  FILLER.                                                 00303996
001300          04  FILLER              PIC X(02)  VALUE '14'.          00303997
001300          04  FILLER              PIC X(01)  VALUE 'S'.           00303998
001300          04  FILLER              PIC X(03)  VALUE '000'.         00303999
001300          04  FILLER              PIC X(03)  VALUE '999'.         00304000
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                                                                        00303995
001300      03  FILLER.                                                 00303996
001300          04  FILLER              PIC X(02)  VALUE '14'.          00303997
001300          04  FILLER              PIC X(01)  VALUE 'D'.           00303998
001300          04  FILLER              PIC X(03)  VALUE '001'.         00303999
001300          04  FILLER              PIC X(03)  VALUE '999'.         00304000
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                                                                        00303995
001300      03  FILLER.                                                 00303996
001300          04  FILLER              PIC X(02)  VALUE '15'.          00303997
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303998
001300          04  FILLER              PIC X(03)  VALUE '000'.         00303999
001300          04  FILLER              PIC X(03)  VALUE '018'.         00304000
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                                                                        00303995
001300      03  FILLER.                                                 00303996
001300          04  FILLER              PIC X(02)  VALUE '16'.          00303997
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303998
001300          04  FILLER              PIC X(03)  VALUE '003'.         00303999
001300          04  FILLER              PIC X(03)  VALUE '003'.         00304000
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                                                                        00303995
001300      03  FILLER.                                                 00303996
001300          04  FILLER              PIC X(02)  VALUE '17'.          00303997
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303998
001300          04  FILLER              PIC X(03)  VALUE '006'.         00303999
001300          04  FILLER              PIC X(03)  VALUE '019'.         00304000
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                                                                        00303995
001300      03  FILLER.                                                 00303996
001300          04  FILLER              PIC X(02)  VALUE '18'.          00303997
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303998
001300          04  FILLER              PIC X(03)  VALUE '020'.         00303999
001300          04  FILLER              PIC X(03)  VALUE '040'.         00304000
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                                                                        00303995
001300      03  FILLER.                                                 00303996
001300          04  FILLER              PIC X(02)  VALUE '19'.          00303997
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303998
001300          04  FILLER              PIC X(03)  VALUE '041'.         00303999
001300          04  FILLER              PIC X(03)  VALUE '049'.         00304000
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                                                                        00303995
001300      03  FILLER.                                                 00303996
001300          04  FILLER              PIC X(02)  VALUE '20'.          00303997
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303998
001300          04  FILLER              PIC X(03)  VALUE '050'.         00303999
001300          04  FILLER              PIC X(03)  VALUE '999'.         00304000
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                                                                        00303995
001300      03  FILLER.                                                 00303996
001300          04  FILLER              PIC X(02)  VALUE '21'.          00303997
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303998
001300          04  FILLER              PIC X(03)  VALUE '019'.         00303999
001300          04  FILLER              PIC X(03)  VALUE '999'.         00304000
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                                                                        00303995
001300      03  FILLER.                                                 00303996
001300          04  FILLER              PIC X(02)  VALUE '22'.          00303997
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303998
001300          04  FILLER              PIC X(03)  VALUE '000'.         00303999
001300          04  FILLER              PIC X(03)  VALUE '003'.         00304000
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                04  FILLER              PIC X(01)  VALUE 'Y'.           00304010
                                                                        00303995
001300      03  FILLER.                                                 00303996
001300          04  FILLER              PIC X(02)  VALUE '23'.          00303997
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303998
001300          04  FILLER              PIC X(03)  VALUE '005'.         00303999
001300          04  FILLER              PIC X(03)  VALUE '034'.         00304000
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                04  FILLER              PIC X(01)  VALUE 'Y'.           00304010
                                                                        00303995
001300      03  FILLER.                                                 00303996
001300          04  FILLER              PIC X(02)  VALUE '24'.          00303997
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303998
001300          04  FILLER              PIC X(03)  VALUE '035'.         00303999
001300          04  FILLER              PIC X(03)  VALUE '039'.         00304000
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                04  FILLER              PIC X(01)  VALUE 'Y'.           00304010
                                                                        00303995
001300      03  FILLER.                                                 00303996
001300          04  FILLER              PIC X(02)  VALUE '25'.          00303997
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303998
001300          04  FILLER              PIC X(03)  VALUE '040'.         00303999
001300          04  FILLER              PIC X(03)  VALUE '049'.         00304000
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                04  FILLER              PIC X(01)  VALUE 'Y'.           00304010
                                                                        00303995
001300      03  FILLER.                                                 00303996
001300          04  FILLER              PIC X(02)  VALUE '26'.          00303997
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303998
001300          04  FILLER              PIC X(03)  VALUE '050'.         00303999
001300          04  FILLER              PIC X(03)  VALUE '999'.         00304000
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                04  FILLER              PIC X(01)  VALUE 'Y'.           00304010
                                                                        00303995
001300      03  FILLER.                                                 00303996
001300          04  FILLER              PIC X(02)  VALUE '27'.          00303997
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303998
001300          04  FILLER              PIC X(03)  VALUE '004'.         00303999
001300          04  FILLER              PIC X(03)  VALUE '005'.         00304000
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                                                                        00303995
001300      03  FILLER.                                                 00303996
001300          04  FILLER              PIC X(02)  VALUE '28'.          00303997
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303998
001300          04  FILLER              PIC X(03)  VALUE '004'.         00303999
001300          04  FILLER              PIC X(03)  VALUE '005'.         00304000
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                04  FILLER              PIC X(01)  VALUE 'Y'.           00304010
                                                                        00304100
001300      03  FILLER.                                                 00303996
001300          04  FILLER              PIC X(02)  VALUE '29'.          00303997
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303998
001300          04  FILLER              PIC X(03)  VALUE '000'.         00303999
001300          04  FILLER              PIC X(03)  VALUE '000'.         00304000
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                                                                        00304100
001300      03  FILLER.                                                 00303996
001300          04  FILLER              PIC X(02)  VALUE '30'.          00303997
001300          04  FILLER              PIC X(01)  VALUE 'A'.           00303998
001300          04  FILLER              PIC X(03)  VALUE '001'.         00303999
001300          04  FILLER              PIC X(03)  VALUE '002'.         00304000
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
                04  FILLER              PIC X(01)  VALUE ' '.           00304010
001100                                                                  00306500
058900    02    FML-TABLE               REDEFINES  FML-VALUES.          00307200
001300      03  FML-TBL                 OCCURS 66 TIMES                 00307210
059200                                  INDEXED BY FML-INDX             00307500
059200                                             FML-MAX-INDX.        00307501
001300          04  FML-KEY             PIC X(02).                      00307502
001300          04  FML-TYPE            PIC X(01).                      00307503
001300          04  FML-LOW-AGE         PIC X(03).                      00307504
001300          04  FML-HIGH-AGE        PIC X(03).                      00307505
001300          04  FML-IND             PIC X(01).                      00307506
001300          04  FML-IND-MED         PIC X(01).                      00307506
001100                                                                  00307507
058900    02    FML-COUNTS.                                             00307508
   300      03  FML-ENTRY-CNT      PIC S9(04) COMP  VALUE 66.           00307509
 03100                                                                  00310002
