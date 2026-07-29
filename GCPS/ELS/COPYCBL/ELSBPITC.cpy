      ******************************************************************00010000
      *                                                                *00020000
      *    COPYBOOK:   ELSBPITB                                        *00030000
      *    DATE:       02-AUG-1989                                     *00040000
      *    AUTHOR:     GEORGE E MOORE                                  *00050000
      *    FUNCTION:   ELS BENEFIT PERIOD INDICATOR TABLES USED        *00060000
      *                FOR BENEFIT PROVISION LEVEL SELECTIONS.         *00070000
      *                                                                *00080000
      ******************************************************************00090000
      *                                                                *00100000
      *                      MAINTENANCE HISTORY                       *00110000
      *                                                                *00120000
      *  MOD     DATE     BY  DRPT                ACTION               *00130000
      * ----- ----------- --- ----- ---------------------------------- *00140000
      * 01.00 02-AUG-1989 GEM       CREATED                            *00150000
      *                                                                *00160000
      ******************************************************************00170000
                                                                        00180000
      *    BENEFIT PERIOD INDICATOR TABLE ONE   *                       00181000
                                                                        00182000
       01  WS-BEN-PER-IND-TBL1.                                         00190001
           05  WS-BPI-T1-01              PIC X(02) VALUE '0A'.          00200000
           05  WS-BPI-T1-02              PIC X(02) VALUE '0D'.          00210000
           05  WS-BPI-T1-03              PIC X(02) VALUE '0P'.          00220000
           05  WS-BPI-T1-04              PIC X(02) VALUE '0E'.          00230000
           05  WS-BPI-T1-05              PIC X(02) VALUE '0B'.          00240000
           05  WS-BPI-T1-06              PIC X(02) VALUE '0C'.          00250000
           05  WS-BPI-T1-07              PIC X(02) VALUE '0F'.          00260000
           05  WS-BPI-T1-08              PIC X(02) VALUE '0G'.          00270000
           05  WS-BPI-T1-09              PIC X(02) VALUE '0H'.          00280000
           05  WS-BPI-T1-10              PIC X(02) VALUE '0I'.          00290000
           05  WS-BPI-T1-11              PIC X(02) VALUE '0J'.          00300000
           05  WS-BPI-T1-12              PIC X(02) VALUE '0K'.          00310000
           05  WS-BPI-T1-13              PIC X(02) VALUE '0L'.          00320000
           05  WS-BPI-T1-14              PIC X(02) VALUE '0M'.          00330000
           05  WS-BPI-T1-15              PIC X(02) VALUE '0N'.          00340000
           05  WS-BPI-T1-16              PIC X(02) VALUE '0Q'.          00350000
           05  WS-BPI-T1-17              PIC X(02) VALUE '0R'.          00360000
           05  WS-BPI-T1-18              PIC X(02) VALUE '0S'.          00370000
           05  WS-BPI-T1-19              PIC X(02) VALUE '0T'.          00380000
           05  WS-BPI-T1-20              PIC X(02) VALUE '0U'.          00390000
           05  WS-BPI-T1-21              PIC X(02) VALUE '0V'.          00400000
           05  WS-BPI-T1-22              PIC X(02) VALUE '0W'.          00410000
           05  WS-BPI-T1-23              PIC X(02) VALUE '0X'.          00420000
           05  WS-BPI-T1-24              PIC X(02) VALUE '0Y'.          00430000
           05  WS-BPI-T1-25              PIC X(02) VALUE '0Z'.          00440000
                                                                        00450000
      *    BENEFIT PERIOD INDICATOR TABLE TWO   *                       00450100
                                                                        00451000
       01  WS-BEN-PER-IND-TBL2.                                         00460001
           05  WS-BPI-T2-01              PIC X(02) VALUE '0I'.          00470000
           05  WS-BPI-T2-02              PIC X(02) VALUE '0J'.          00480000
           05  WS-BPI-T2-03              PIC X(02) VALUE '0D'.          00490000
           05  WS-BPI-T2-04              PIC X(02) VALUE '0P'.          00500000
           05  WS-BPI-T2-05              PIC X(02) VALUE '0E'.          00510000
           05  WS-BPI-T2-06              PIC X(02) VALUE '0B'.          00520000
           05  WS-BPI-T2-07              PIC X(02) VALUE '0A'.          00530000
           05  WS-BPI-T2-08              PIC X(02) VALUE '0C'.          00540000
           05  WS-BPI-T2-09              PIC X(02) VALUE '0F'.          00550000
           05  WS-BPI-T2-10              PIC X(02) VALUE '0G'.          00560000
           05  WS-BPI-T2-11              PIC X(02) VALUE '0H'.          00570000
           05  WS-BPI-T2-12              PIC X(02) VALUE '0K'.          00580000
           05  WS-BPI-T2-13              PIC X(02) VALUE '0L'.          00590000
           05  WS-BPI-T2-14              PIC X(02) VALUE '0M'.          00600000
           05  WS-BPI-T2-15              PIC X(02) VALUE '0N'.          00610000
           05  WS-BPI-T2-16              PIC X(02) VALUE '0Q'.          00620000
           05  WS-BPI-T2-17              PIC X(02) VALUE '0R'.          00630000
           05  WS-BPI-T2-18              PIC X(02) VALUE '0S'.          00640000
           05  WS-BPI-T2-19              PIC X(02) VALUE '0T'.          00650000
           05  WS-BPI-T2-20              PIC X(02) VALUE '0U'.          00660000
           05  WS-BPI-T2-21              PIC X(02) VALUE '0V'.          00670000
           05  WS-BPI-T2-22              PIC X(02) VALUE '0W'.          00680000
           05  WS-BPI-T2-23              PIC X(02) VALUE '0X'.          00690000
           05  WS-BPI-T2-24              PIC X(02) VALUE '0Y'.          00700000
           05  WS-BPI-T2-25              PIC X(02) VALUE '0Z'.          00710000
                                                                        00710100
      *    BENEFIT PERIOD INDICATOR TABLE THREE   *                     00711000
                                                                        00712000
       01  WS-BEN-PER-IND-TBL3.                                         00713001
           05  WS-BPI-T3-01              PIC X(02) VALUE '0A'.          00714000
           05  WS-BPI-T3-02              PIC X(02) VALUE '0I'.          00715000
           05  WS-BPI-T3-03              PIC X(02) VALUE '0J'.          00716000
           05  WS-BPI-T3-04              PIC X(02) VALUE '0D'.          00717000
           05  WS-BPI-T3-05              PIC X(02) VALUE '0P'.          00718000
           05  WS-BPI-T3-06              PIC X(02) VALUE '0E'.          00719000
           05  WS-BPI-T3-07              PIC X(02) VALUE '0B'.          00719100
           05  WS-BPI-T3-08              PIC X(02) VALUE '0C'.          00719200
           05  WS-BPI-T3-09              PIC X(02) VALUE '0F'.          00719300
           05  WS-BPI-T3-10              PIC X(02) VALUE '0G'.          00719400
           05  WS-BPI-T3-11              PIC X(02) VALUE '0H'.          00719500
           05  WS-BPI-T3-12              PIC X(02) VALUE '0K'.          00719600
           05  WS-BPI-T3-13              PIC X(02) VALUE '0L'.          00719700
           05  WS-BPI-T3-14              PIC X(02) VALUE '0M'.          00719800
           05  WS-BPI-T3-15              PIC X(02) VALUE '0N'.          00719900
           05  WS-BPI-T3-16              PIC X(02) VALUE '0Q'.          00720000
           05  WS-BPI-T3-17              PIC X(02) VALUE '0R'.          00721000
           05  WS-BPI-T3-18              PIC X(02) VALUE '0S'.          00722000
           05  WS-BPI-T3-19              PIC X(02) VALUE '0T'.          00723000
           05  WS-BPI-T3-20              PIC X(02) VALUE '0U'.          00724000
           05  WS-BPI-T3-21              PIC X(02) VALUE '0V'.          00725000
           05  WS-BPI-T3-22              PIC X(02) VALUE '0W'.          00726000
           05  WS-BPI-T3-23              PIC X(02) VALUE '0X'.          00727000
           05  WS-BPI-T3-24              PIC X(02) VALUE '0Y'.          00728000
           05  WS-BPI-T3-25              PIC X(02) VALUE '0Z'.          00729000
                                                                        00730000
      *    BENEFIT PERIOD INDICATOR TABLE FOUR   *                      00740000
                                                                        00750000
       01  WS-BEN-PER-IND-TBL4.                                         00760001
           05  WS-BPI-T4-01              PIC X(02) VALUE '0I'.          00770000
           05  WS-BPI-T4-02              PIC X(02) VALUE '0J'.          00780000
           05  WS-BPI-T4-03              PIC X(02) VALUE '0D'.          00790000
           05  WS-BPI-T4-04              PIC X(02) VALUE '0P'.          00800000
           05  WS-BPI-T4-05              PIC X(02) VALUE '0E'.          00810000
           05  WS-BPI-T4-06              PIC X(02) VALUE '0B'.          00820000
           05  WS-BPI-T4-07              PIC X(02) VALUE '0A'.          00830000
           05  WS-BPI-T4-08              PIC X(02) VALUE '0C'.          00840000
           05  WS-BPI-T4-09              PIC X(02) VALUE '0F'.          00850000
           05  WS-BPI-T4-10              PIC X(02) VALUE '0G'.          00860000
           05  WS-BPI-T4-11              PIC X(02) VALUE '0H'.          00870000
           05  WS-BPI-T4-12              PIC X(02) VALUE '0K'.          00880000
           05  WS-BPI-T4-13              PIC X(02) VALUE '0L'.          00890000
           05  WS-BPI-T4-14              PIC X(02) VALUE '0M'.          00900000
           05  WS-BPI-T4-15              PIC X(02) VALUE '0N'.          00910000
           05  WS-BPI-T4-16              PIC X(02) VALUE '0Q'.          00920000
           05  WS-BPI-T4-17              PIC X(02) VALUE '0R'.          00930000
           05  WS-BPI-T4-18              PIC X(02) VALUE '0S'.          00940000
           05  WS-BPI-T4-19              PIC X(02) VALUE '0T'.          00950000
           05  WS-BPI-T4-20              PIC X(02) VALUE '0U'.          00960000
           05  WS-BPI-T4-21              PIC X(02) VALUE '0V'.          00970000
           05  WS-BPI-T4-22              PIC X(02) VALUE '0W'.          00980000
           05  WS-BPI-T4-23              PIC X(02) VALUE '0X'.          00990000
           05  WS-BPI-T4-24              PIC X(02) VALUE '0Y'.          01000000
           05  WS-BPI-T4-25              PIC X(02) VALUE '0Z'.          01010000
                                                                        01020000
      *    BENEFIT PERIOD INDICATOR TABLE FIVE   *                      01030000
                                                                        01040000
       01  WS-BEN-PER-IND-TBL5.                                         01050001
           05  WS-BPI-T5-01              PIC X(02) VALUE '0I'.          01060000
           05  WS-BPI-T5-02              PIC X(02) VALUE '0J'.          01070000
           05  WS-BPI-T5-03              PIC X(02) VALUE '0D'.          01080000
           05  WS-BPI-T5-04              PIC X(02) VALUE '0P'.          01090000
           05  WS-BPI-T5-05              PIC X(02) VALUE '0E'.          01100000
           05  WS-BPI-T5-06              PIC X(02) VALUE '0B'.          01110000
           05  WS-BPI-T5-07              PIC X(02) VALUE '0C'.          01120000
           05  WS-BPI-T5-08              PIC X(02) VALUE '0U'.          01130000
           05  WS-BPI-T5-09              PIC X(02) VALUE '0F'.          01140000
           05  WS-BPI-T5-10              PIC X(02) VALUE '0A'.          01150000
           05  WS-BPI-T5-11              PIC X(02) VALUE '0G'.          01160000
           05  WS-BPI-T5-12              PIC X(02) VALUE '0H'.          01170000
           05  WS-BPI-T5-13              PIC X(02) VALUE '0K'.          01180000
           05  WS-BPI-T5-14              PIC X(02) VALUE '0L'.          01190000
           05  WS-BPI-T5-15              PIC X(02) VALUE '0M'.          01200000
           05  WS-BPI-T5-16              PIC X(02) VALUE '0N'.          01210000
           05  WS-BPI-T5-17              PIC X(02) VALUE '0Q'.          01220000
           05  WS-BPI-T5-18              PIC X(02) VALUE '0R'.          01230000
           05  WS-BPI-T5-19              PIC X(02) VALUE '0S'.          01240000
           05  WS-BPI-T5-20              PIC X(02) VALUE '0T'.          01250000
           05  WS-BPI-T5-21              PIC X(02) VALUE '0V'.          01260000
           05  WS-BPI-T5-22              PIC X(02) VALUE '0W'.          01270000
           05  WS-BPI-T5-23              PIC X(02) VALUE '0X'.          01280000
           05  WS-BPI-T5-24              PIC X(02) VALUE '0Y'.          01290000
           05  WS-BPI-T5-25              PIC X(02) VALUE '0Z'.          01300000
                                                                        01310000
