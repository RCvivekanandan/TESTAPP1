000100******************************************************************00010002
   200*                                                                *00020002
000300*    COPYB00K:   ELXPMIPC                                        *00030002
000400*    DATE:       21-MAY-1993                                     *00040002
000500*    AUTHOR:     BARBARA KEIB * *                                 00050002
000600*    FUNCTION:   TABLE TO DERIVE PROVIDER CONTROL FOR PROFESSION *00060002
000200*                FOR SUPPLEMENTAL MAJOR MEDICAL ONLY             *00060003
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
001400* 01.00 21-MAY-1993 BAK       CREATED                            *00140002
001400* 02.00 21-SEP-1993 BAK       CHANGED INDICATORS TO 2 DIGITS     *00140002
002920*                                                                *00292002
003000******************************************************************00300002
003200 01  ELS-PMPRV-TABLE.                                             00300003
001100                                                                  00300004
001000    02  PMPRV-VALUES.                                             00301300
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
                                                                        00302620
001300      03  FILLER.                                                 00302621
001300          04  FILLER              PIC X(02)  VALUE '06'.          00302622
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00302623
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00302624
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302625
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302630
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302631
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302632
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302633
                                                                        00302634
001300      03  FILLER.                                                 00302635
001300          04  FILLER              PIC X(02)  VALUE '06'.          00302636
001300          04  FILLER              PIC X(02)  VALUE '0D'.          00302637
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302638
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302639
001300          04  FILLER              PIC X(01)  VALUE '1'.           00302644
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302645
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302646
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302647
                                                                        00302648
001300      03  FILLER.                                                 00302649
001300          04  FILLER              PIC X(02)  VALUE '06'.          00302650
001300          04  FILLER              PIC X(02)  VALUE '0H'.          00302651
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302652
001300          04  FILLER              PIC X(02)  VALUE '0Z'.          00302653
001300          04  FILLER              PIC X(01)  VALUE '0'.           00302658
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302659
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302660
001300          04  FILLER              PIC X(01)  VALUE '+'.           00302661
                                                                        00302662
                                                                        00306000
001100                                                                  00306500
058900    02    PMPRV-TABLE                REDEFINES  PMPRV-VALUES.     00307200
001300      03  PMPRV-TBL                  OCCURS 4 TIMES               00307210
059200                                     INDEXED BY PMPRV-INDX        00307500
059200                                               PMPRV-MAX-INDX.    00307501
001300          04  PMPRV-KEY.                                          00307502
001300              08  PMPRV-TAB-PRV    PIC X(02).                     00307503
001300              08  PMPRV-C0N-PRV    PIC X(02).                     00307504
001300          04  PMPRV-TABS-INDS.                                    00307505
001300              08  PMPRV-TAB-IND1   PIC X(02).                     00307506
001300              08  PMPRV-TAB-IND2   PIC X(02).                     00307507
001300          04  PMPRV-MPP-IND        PIC X(01).                     00307517
001300          04  PMPRV-EMPL-IND       PIC X(01).                     00307518
001300          04  PMPRV-PHAR-IND       PIC X(01).                     00307519
001300          04  PMPRV-VISN-IND       PIC X(01).                     00307520
001100                                                                  00307521
058900    02    PMPRV-C0UNTS.                                           00307522
   300      03  PMPRV-ENTRY-CNT    PIC S9(04) COMP  VALUE 42.           00307530
 03100                                                                  00310002
