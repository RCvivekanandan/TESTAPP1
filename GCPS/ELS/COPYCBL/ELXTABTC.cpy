000100******************************************************************00010002
000200*                                                                *00020002
000300*    COPYBOOK:   ELXTABTC                                        *00030002
000400*    DATE:       06-AUG-1992                                     *00040002
000500*    AUTHOR:     BARBARA KEIB                                     00050002
000600*    FUNCTION:   TABULAR READ INDICATOR TABLE FOR GROUP SPECIFIC *00060002
      *                TABULAR RECORDS.                                *00070002
      *                                                                *00070003
000100******************************************************************00070004
      *                                                                *00070005
      *    THIS TABLE IS USED TO DETERMINE IF TABULAR RECORDS NEED     *00070006
      *    TO BE READ TO DETERMINE PROVIDER CONTROL NUMBER IN ORDER    *00070007
      *    TO BUILD THE KEY TO READ THE CONTRACT RECORD.               *00070008
      *                                                                *00070009
      *    THE TABLE KEY IS PROVIDER NUMBER XX FOLLOWED BY AN I FOR    *00070010
      *    INSTITUTIONAL AND A P TO INDICATE PROFESSIONAL PROVIDERS.   *00070020
      *    THE 3 FIELDS FOLLOWING INDICATE IF THE PROPER TABULAR IS    *00070030
      *    TO BE READ OR NOT Y OR N.                                   *00070040
      *                                                                *00070041
      *    THE INSTITUTIONAL TABULARS ARE #GVLF,#GVLG AND #GVLH        *00070050
      *    THE Y/N INDICATORS FOLLOW THAT ORDER FOR TABULAR TO BE READ.*00070060
      *                                                                *00070061
      *    THE PROFESSIONAL TABULARS ARE #GVLP,#GVLQ AND #GVLR         *00070070
      *    THE Y/N INDICATORS FOLLOW THAT ORDER FOR TABULAR TO BE READ.*00070080
      *                                                                *00070090
      * NOTE:  10I AND 11P HAVE BEEN OMITTED SINCE THEY ARE CURRENTLY\ *00070100
      *        NOT IN USE--SO AN ERROR CAN BE GENERATED.               *00070200
      *                                                                *00070300
000800******************************************************************00080002
      * NOTE:  FOR MAJOR MEDICAL THE '06' ENTRY IS BEING SHARED WITH   *00080003
      *        REGULAR CONTRACTS SINCE THE ANSWER IS THE SAME -- THIS  *00080004
      *        MAY NEED TO BE REVIEWED IN THE FUTURE **********        *00080005
000800******************************************************************00080006
000900*                                                                *00090002
001000*                      MAINTENANCE HISTORY                       *00100002
001100*                                                                *00110002
001200*  MOD     DATE     BY  DRPT                ACTION               *00120002
001300* ----- ----------- --- ----- ---------------------------------- *00130002
001400* 01.00 06-AUG-1992 BAK       CREATED                            *00140002
001400* 02.00 21-MAY-1993 BAK       ADDED MAJOR MEDICAL NOTE ABOVE     *00140003
002920*                                                                *00292002
003000******************************************************************00300002
003200 01  ELS-TAB-READ-TABLE.                                          00300003
001100                                                                  00300004
 01000    02  TAB-INDICATOR-VALUES.                                     00301300
001100                                                                  00301400
001300      03  FILLER.                                                 00302400
001300          04  FILLER              PIC X(03)  VALUE '00I'.         00302410
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00302420
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00302421
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00302422
001100                                                                  00302500
001300      03  FILLER.                                                 00302600
001300          04  FILLER              PIC X(03)  VALUE '00P'.         00302700
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00302800
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00302900
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303000
001100                                                                  00303100
001300      03  FILLER.                                                 00303110
001300          04  FILLER              PIC X(03)  VALUE '01I'.         00303120
001300          04  FILLER              PIC X(01)  VALUE 'Y'.           00303130
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303140
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303150
001100                                                                  00303160
001300      03  FILLER.                                                 00303170
001300          04  FILLER              PIC X(03)  VALUE '01P'.         00303180
001300          04  FILLER              PIC X(01)  VALUE 'Y'.           00303190
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303191
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303192
001100                                                                  00303193
001300      03  FILLER.                                                 00303194
001300          04  FILLER              PIC X(03)  VALUE '02I'.         00303195
001300          04  FILLER              PIC X(01)  VALUE 'Y'.           00303196
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303197
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303198
001100                                                                  00303199
001300      03  FILLER.                                                 00303200
001300          04  FILLER              PIC X(03)  VALUE '02P'.         00303201
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303202
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303203
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303204
001100                                                                  00303205
001300      03  FILLER.                                                 00303206
001300          04  FILLER              PIC X(03)  VALUE '03I'.         00303207
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303208
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303209
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303210
001100                                                                  00303211
001300      03  FILLER.                                                 00303212
001300          04  FILLER              PIC X(03)  VALUE '03P'.         00303213
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303214
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303215
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303216
001100                                                                  00303217
001300      03  FILLER.                                                 00303218
001300          04  FILLER              PIC X(03)  VALUE '04I'.         00303219
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303220
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303221
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303222
001100                                                                  00303223
001300      03  FILLER.                                                 00303224
001300          04  FILLER              PIC X(03)  VALUE '04P'.         00303225
001300          04  FILLER              PIC X(01)  VALUE 'Y'.           00303226
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303227
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303228
001100                                                                  00303229
001300      03  FILLER.                                                 00303230
001300          04  FILLER              PIC X(03)  VALUE '05I'.         00303231
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303232
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303233
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303234
001100                                                                  00303235
001300      03  FILLER.                                                 00303240
001300          04  FILLER              PIC X(03)  VALUE '05P'.         00303300
001300          04  FILLER              PIC X(01)  VALUE 'Y'.           00303400
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303500
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00303600
001100                                                                  00303700
001300      03  FILLER.                                                 00303800
001300          04  FILLER              PIC X(03)  VALUE '06I'.         00303900
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00304000
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00304100
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00304200
001100                                                                  00304300
001300      03  FILLER.                                                 00304400
001300          04  FILLER              PIC X(03)  VALUE '06P'.         00304500
001300          04  FILLER              PIC X(01)  VALUE 'Y'.           00304600
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00304700
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00304800
001100                                                                  00304900
001300      03  FILLER.                                                 00305000
001300          04  FILLER              PIC X(03)  VALUE '07I'.         00305100
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00305200
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00305300
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00305400
001100                                                                  00305500
001300      03  FILLER.                                                 00305600
001300          04  FILLER              PIC X(03)  VALUE '07P'.         00305700
001300          04  FILLER              PIC X(01)  VALUE 'Y'.           00305800
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00305900
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306000
001100                                                                  00306100
001300      03  FILLER.                                                 00306200
001300          04  FILLER              PIC X(03)  VALUE '08I'.         00306300
001300          04  FILLER              PIC X(01)  VALUE 'Y'.           00306400
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306500
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306600
001100                                                                  00306700
001300      03  FILLER.                                                 00306710
001300          04  FILLER              PIC X(03)  VALUE '08P'.         00306720
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306730
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306740
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306750
001100                                                                  00306760
001300      03  FILLER.                                                 00306770
001300          04  FILLER              PIC X(03)  VALUE '09I'.         00306780
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306790
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306791
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306792
001100                                                                  00306793
001300      03  FILLER.                                                 00306794
001300          04  FILLER              PIC X(03)  VALUE '09P'.         00306795
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306796
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306797
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306798
001100                                                                  00306799
001300      03  FILLER.                                                 00306806
001300          04  FILLER              PIC X(03)  VALUE '1OP'.         00306807
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306808
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306809
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306810
001100                                                                  00306811
001300      03  FILLER.                                                 00306812
001300          04  FILLER              PIC X(03)  VALUE '11I'.         00306813
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306814
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306815
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306816
001100                                                                  00306817
001300      03  FILLER.                                                 00306824
001300          04  FILLER              PIC X(03)  VALUE '12I'.         00306825
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306826
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306827
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306828
001100                                                                  00306829
001300      03  FILLER.                                                 00306830
001300          04  FILLER              PIC X(03)  VALUE '12P'.         00306831
001300          04  FILLER              PIC X(01)  VALUE 'Y'.           00306832
001300          04  FILLER              PIC X(01)  VALUE 'Y'.           00306833
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306834
001100                                                                  00306835
001300      03  FILLER.                                                 00306836
001300          04  FILLER              PIC X(03)  VALUE '13I'.         00306837
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306838
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306839
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306840
001100                                                                  00306841
001300      03  FILLER.                                                 00306842
001300          04  FILLER              PIC X(03)  VALUE '13P'.         00306843
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306844
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306845
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306846
001100                                                                  00306847
001300      03  FILLER.                                                 00306848
001300          04  FILLER              PIC X(03)  VALUE '14I'.         00306849
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306850
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306851
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306852
001100                                                                  00306853
001300      03  FILLER.                                                 00306854
001300          04  FILLER              PIC X(03)  VALUE '14P'.         00306855
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306856
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306857
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306858
001100                                                                  00306859
001300      03  FILLER.                                                 00306893
001300          04  FILLER              PIC X(03)  VALUE '15P'.         00306894
001300          04  FILLER              PIC X(01)  VALUE 'Y'.           00306895
001300          04  FILLER              PIC X(01)  VALUE 'Y'.           00306896
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306897
001100                                                                  00306940
001300      03  FILLER.                                                 00306950
001300          04  FILLER              PIC X(03)  VALUE '16P'.         00306960
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306970
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306980
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00306990
001100                                                                  00306991
001300      03  FILLER.                                                 00306998
001300          04  FILLER              PIC X(03)  VALUE '17P'.         00306999
001300          04  FILLER              PIC X(01)  VALUE 'Y'.           00307000
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00307010
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00307020
001100                                                                  00307030
001300      03  FILLER.                                                 00307091
001300          04  FILLER              PIC X(03)  VALUE '18P'.         00307092
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00307093
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00307094
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00307095
001100                                                                  00307102
001300      03  FILLER.                                                 00307103
001300          04  FILLER              PIC X(03)  VALUE '19P'.         00307104
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00307105
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00307106
001300          04  FILLER              PIC X(01)  VALUE 'N'.           00307107
001100                                                                  00307108
001100                                                                  00307110
058900    02    TAB-TABLE             REDEFINES  TAB-INDICATOR-VALUES.  00307200
001300      03  TAB-TBL               OCCURS 33 TIMES                   00307210
059200                                INDEXED BY TAB-INDX               00307500
059200                                           TAB-MAX-INDX.          00307501
001300          04  TAB-KEY             PIC X(03).                      00307502
001300          04  TAB-READ-INDICATORS.                                00307503
001300              08  TAB-IND1            PIC X(01).                  00307504
001300              08  TAB-IND2            PIC X(01).                  00307505
001300              08  TAB-IND3            PIC X(01).                  00307506
001100                                                                  00307507
058900    02    TAB-COUNTS.                                             00307508
   300      03  TAB-ENTRY-CNT      PIC S9(04) COMP  VALUE 33.           00307509
 03100                                                                  00310002
