00001  IDENTIFICATION DIVISION.                                         00010000
00002  PROGRAM-ID. GC0080.                                              00020000
00003  AUTHOR. ROBERT MANN - DECISION CONSULTANTS INC.                  00030000
00004  INSTALLATION. HCSC.                                              00040000
00005  DATE-WRITTEN.  JUL 03,1984.                                      00050000
00006  DATE-COMPLIED.                                                   00060000
00007 ****************************************************************  00070000
00008 *                                                                 00080000
00009 *    THIS PROGRAM NOW MERELY SPLITS THE ALL LEVEL TABULARS INTO   00090000
00010 *    THREE FILES ACCORDING TO TYPE OF HIGHER LEVEL.               00100000
00011 *    TABULAR VSAM FILE UPDATE REMOVED 1/21/88.                    00110000
00012 *                                                                 00120000
00013 *    SEQ.FILE IS SLOT UPDATED REL ALL TAB PROV'S -SURATP (IN).    00130000
00014 *    SEQ.FILE IS SLOT UPDT REL ALL CON TAB PROV'S -SURACTP (OUT). 00140000
00015 *    SEQ.FILE IS SLOT UPDT REL ALL BEN TAB PROV'S -SURABTP (OUT). 00150000
00016 *    SEQ.FILE IS SLOT UPDT REL ALL GRS TAB PROV'S -SURAGTP (OUT). 00160000
00017 *                                                                 00170000
00018 ******************************************************************00180000
00019 *                     U P D A T E  L O G                          00190000
00020 ******************************************************************00200000
00021 *  NUM      DATE    WHO           DESCRIPTION                     00210000
00022 * -----   --------  ----   ---------------------------------------00220000
00023 *         03/16/87  ENW    REMOVED CODE FOR XREF-FILE UPDATE.     00230000
00024 *         01/21/88  ENW    REMOVED TABULAR VSAM FILE UDPATE.      00240000
00025 * 11154    2/21/91  NE     -CHANGE ACCUM TABULAR RECORD LEN FROM  00250000
00026 *                           4000 TO 7805.                         00260000
00027 *                                                                 00270000
00028 *                                                                 00280000
00029 *     REVISED MARCH 1985 TO INCLUDE:                              00290000
00030 *        GCTACRD   GCTACOS   GCTADIP   GCTADOP   - ALL LVL TABS   00300000
00031 *                                                3/12/85  RKH     00310000
00032 *                                                                 00320000
00033 * 1/10/95   EMS  CONVERTED TO COBOL II.                           00330000
00034 *                                                                 00340000
00035 * 14726/  11/14/97  DAU  ADDED CODE TO SUPPORT THE YEAR 2000      00350000
00036 * 15057                  AND THE EXPANSION OF THE GROUP SPECIFIC  00360000
00037 *                        AND CONTRACT KEY TO SUPPORT THE TEXAS    00370000
00038 *                        MERGER.                                  00380000
00039 *                                                                *00390000
00040 * D15182  12/12/98  FRY    ADD #ACP ACCUM TABULAR                *00400000
00041 *                                                                *00410000
00042 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *00420000
00043 *                                                                *00430000
00044 * D-356A   5/08/03  GTF  RECOMPILE FOR COPYBK CHANGES #ACON,     *00440000
00045 *                        #ACOS, #ADIP, #ADOP.                    *00450000
00044 *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION         *00451001
ED0624*BBDA-58217 06/04/24  ED   RECOMPILE FOR PEAQ COPYBOOK           *00451002
ED0624*                          EXPANSION:                            *00451003
ED0624*                                COPYBKS - GCTABM*, GCTACL*,     *00451004
ED0624*                                GCTACP*,  GCTADL*, GCTADL*      *00451005
00046 ******************************************************************00460000
00047                                                                   00470000
00048  ENVIRONMENT DIVISION.                                            00480000
00049  CONFIGURATION SECTION.                                           00490000
00050  SOURCE-COMPUTER. IBM-370.                                        00500000
00051  OBJECT-COMPUTER. IBM-370.                                        00510000
00052  INPUT-OUTPUT SECTION.                                            00520000
00053  FILE-CONTROL.                                                    00530000
00054      SELECT SURATP  ASSIGN TO UT-S-GC0080A.                       00540000
00055      SELECT SURACTP ASSIGN TO UT-S-GC0080B.                       00550000
00056      SELECT SURABTP ASSIGN TO UT-S-GC0080C.                       00560000
00057      SELECT SURAGTP ASSIGN TO UT-S-GC0080D.                       00570000
00058 /                                                                 00580000
00059  DATA DIVISION.                                                   00590000
00060  FILE SECTION.                                                    00600000
00061                                                                   00610000
00062  FD  SURATP                                                       00620000
00063      LABEL RECORDS ARE STANDARD                                   00630000
00064      RECORDING MODE IS V                                          00640000
00065      BLOCK CONTAINS 0 RECORDS.                                    00650000
00066  01  SURATP-REC.                                                  00660000
00067      COPY GCWRKDCC.                                               00670000
00068      05  SURATP-TAB-REC.                                          00680000
00069          10  SURATP-TAB-ID-SLOT.                                  00690000
00070              15  SURATP-TAB-ID   PIC X(6).                        00700000
00071              15  SURATP-TAB-SLOT PIC S9(7)   COMP-3.              00710000
00072          10  FILLER              PIC X(27).                       00720000
00073          10  SURATP-ENT-COUNT    PIC S9(5)   COMP-3.              00730000
00044 *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION         *00731001
00074          10  FILLER              PIC X(31330).                    00740001
00075 /                                                                 00750000
00076  FD  SURACTP                                                      00760000
00077      LABEL RECORDS ARE STANDARD                                   00770000
00078      RECORDING MODE IS V                                          00780000
00079      BLOCK CONTAINS 0 RECORDS.                                    00790000
00080  01  SURACTP-REC.                                                 00800000
00081      05  SURACTP-KEY                    PIC X(100).               00810000
00082      05  SURACTP-PROV-ID-SLOT.                                    00820000
00083          10  SURACTP-PROV-ID            PIC X(6).                 00830000
00084          10  SURACTP-PROV-SLOT          PIC S9(7)     COMP-3.     00840000
00044 *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION         *00841001
00085      05  FILLER                         PIC X(31360).             00850001
00086                                                                   00860000
00087  01  SURACTP-REC-A.                                               00870000
00088      05  FILLER          PIC X(100).                              00880000
00089      COPY GCTABMC.                                                00890000
00090                                                                   00900000
00091  01  SURACTP-REC-B.                                               00910000
00092      05  FILLER          PIC X(100).                              00920000
00093      COPY GCTACLC.                                                00930000
00094                                                                   00940000
00095  01  SURACTP-REC-J.                                               00950000
00096      05  FILLER          PIC X(100).                              00960000
00097      COPY GCTACPC.                                                00970000
00098                                                                   00980000
00099  01  SURACTP-REC-C.                                               00990000
00100      05  FILLER          PIC X(100).                              01000000
00101      COPY GCTADLC.                                                01010000
00102                                                                   01020000
00103  01  SURACTP-REC-D.                                               01030000
00104      05  FILLER          PIC X(100).                              01040000
00105      COPY GCTAOLC.                                                01050000
00106                                                                   01060000
00107  01  SURACTP-REC-E.                                               01070000
00108      05  FILLER          PIC X(100).                              01080000
00109      COPY GCTAARC.                                                01090000
00110                                                                   01100000
00111  01  SURACTP-REC-F.                                               01110000
00112      05  FILLER          PIC X(100).                              01120000
00113      COPY GCTACONC.                                               01130000
00114                                                                   01140000
00115  01  SURACTP-REC-G.                                               01150000
00116      05  FILLER          PIC X(100).                              01160000
00117      COPY GCTACOSC.                                               01170000
00118                                                                   01180000
00119  01  SURACTP-REC-H.                                               01190000
00120      05  FILLER          PIC X(100).                              01200000
00121      COPY GCTADIPC.                                               01210000
00122                                                                   01220000
00123  01  SURACTP-REC-I.                                               01230000
00124      05  FILLER          PIC X(100).                              01240000
00125      COPY GCTADOPC.                                               01250000
00126 /                                                                 01260000
00127  FD  SURABTP                                                      01270000
00128      LABEL RECORDS ARE STANDARD                                   01280000
00129      RECORDING MODE IS V                                          01290000
00130      BLOCK CONTAINS 0 RECORDS.                                    01300000
00131  01  SURABTP-REC.                                                 01310000
00132      05  SURABTP-KEY        PIC X(100).                           01320000
00133      05  SURABTP-PROV-ID    PIC X(6).                             01330000
00044 *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION         *01331001
00134      05  FILLER             PIC X(31364).                         01340001
00135                                                                   01350000
00136  01  SURABTP-REC-A.                                               01360000
00137      05  FILLER          PIC X(100).                              01370000
00138      COPY GCTABM2.                                                01380000
00139                                                                   01390000
00140  01  SURABTP-REC-B.                                               01400000
00141      05  FILLER          PIC X(100).                              01410000
00142      COPY GCTACL2.                                                01420000
00143                                                                   01430000
00144  01  SURABTP-REC-J.                                               01440000
00145      05  FILLER          PIC X(100).                              01450000
00146      COPY GCTACP2.                                                01460000
00147                                                                   01470000
00148  01  SURABTP-REC-C.                                               01480000
00149      05  FILLER          PIC X(100).                              01490000
00150      COPY GCTADL2.                                                01500000
00151                                                                   01510000
00152  01  SURABTP-REC-D.                                               01520000
00153      05  FILLER          PIC X(100).                              01530000
00154      COPY GCTAOL2.                                                01540000
00155                                                                   01550000
00156  01  SURABTP-REC-E.                                               01560000
00157      05  FILLER          PIC X(100).                              01570000
00158      COPY GCTAAR2.                                                01580000
00159                                                                   01590000
00160  01  SURABTP-REC-F.                                               01600000
00161      05  FILLER          PIC X(100).                              01610000
00162      COPY GCTACON2.                                               01620000
00163                                                                   01630000
00164  01  SURABTP-REC-G.                                               01640000
00165      05  FILLER          PIC X(100).                              01650000
00166      COPY GCTACOS2.                                               01660000
00167                                                                   01670000
00168  01  SURABTP-REC-H.                                               01680000
00169      05  FILLER          PIC X(100).                              01690000
00170      COPY GCTADIP2.                                               01700000
00171                                                                   01710000
00172  01  SURABTP-REC-I.                                               01720000
00173      05  FILLER          PIC X(100).                              01730000
00174      COPY GCTADOP2.                                               01740000
00175 /                                                                 01750000
00176  FD  SURAGTP                                                      01760000
00177      LABEL RECORDS ARE STANDARD                                   01770000
00178      RECORDING MODE IS V                                          01780000
00179      BLOCK CONTAINS 0 RECORDS.                                    01790000
00180  01  SURAGTP-REC.                                                 01800000
00181      05  SURAGTP-KEY        PIC X(100).                           01810000
00182      05  SURAGTP-PROV-ID    PIC X(6).                             01820000
00044 *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION         *01821001
00183      05  FILLER             PIC X(31364).                         01830001
00184                                                                   01840000
00185  01  SURAGTP-REC-A.                                               01850000
00186      05  FILLER          PIC X(100).                              01860000
00187      COPY GCTABM3.                                                01870000
00188                                                                   01880000
00189  01  SURAGTP-REC-B.                                               01890000
00190      05  FILLER          PIC X(100).                              01900000
00191      COPY GCTACL3.                                                01910000
00192                                                                   01920000
00193  01  SURAGTP-REC-J.                                               01930000
00194      05  FILLER          PIC X(100).                              01940000
00195      COPY GCTACP3.                                                01950000
00196                                                                   01960000
00197  01  SURAGTP-REC-C.                                               01970000
00198      05  FILLER          PIC X(100).                              01980000
00199      COPY GCTADL3.                                                01990000
00200                                                                   02000000
00201  01  SURAGTP-REC-D.                                               02010000
00202      05  FILLER          PIC X(100).                              02020000
00203      COPY GCTAOL3.                                                02030000
00204                                                                   02040000
00205  01  SURAGTP-REC-E.                                               02050000
00206      05  FILLER          PIC X(100).                              02060000
00207      COPY GCTAAR3.                                                02070000
00208                                                                   02080000
00209  01  SURAGTP-REC-F.                                               02090000
00210      05  FILLER          PIC X(100).                              02100000
00211      COPY GCTACON3.                                               02110000
00212                                                                   02120000
00213  01  SURAGTP-REC-G.                                               02130000
00214      05  FILLER          PIC X(100).                              02140000
00215      COPY GCTACOS3.                                               02150000
00216                                                                   02160000
00217  01  SURAGTP-REC-H.                                               02170000
00218      05  FILLER          PIC X(100).                              02180000
00219      COPY GCTADIP3.                                               02190000
00220                                                                   02200000
00221  01  SURAGTP-REC-I.                                               02210000
00222      05  FILLER          PIC X(100).                              02220000
00223      COPY GCTADOP3.                                               02230000
00224 /                                                                 02240000
00225  WORKING-STORAGE SECTION.                                         02250000
00226                                                                   02260000
00227  01  FILLER                      PIC X(22)   VALUE                02270000
00228                                  'GC0080 WORKING STORAGE'.        02280000
00229  01  SWITCH-AREA.                                                 02290000
00230      05  SURATP-SW               PIC X       VALUE SPACES.        02300000
00231          88  EOF-SURATP          VALUE HIGH-VALUES.               02310000
00232                                                                   02320000
00233                                                                   02330000
00234 /                                                                 02340000
00235  PROCEDURE DIVISION.                                              02350000
00236 *    READY TRACE.                                                 02360000
00237  0000-MAINLINE.                                                   02370000
00238                                                                   02380000
00239      OPEN INPUT  SURATP                                           02390000
00240           OUTPUT SURACTP                                          02400000
00241                  SURABTP                                          02410000
00242                  SURAGTP.                                         02420000
00243                                                                   02430000
00244      PERFORM 0010-READ-SURATP THRU 0010-EXIT                      02440000
00245          UNTIL EOF-SURATP.                                        02450000
00246                                                                   02460000
00247      CLOSE       SURATP                                           02470000
00248                  SURACTP                                          02480000
00249                  SURABTP                                          02490000
00250                  SURAGTP.                                         02500000
00251                                                                   02510000
00252      GOBACK.                                                      02520000
00253 /                                                                 02530000
00254  0010-READ-SURATP.                                                02540000
00255                                                                   02550000
00256      READ SURATP                                                  02560000
00257          AT END                                                   02570000
00258              MOVE HIGH-VALUES    TO SURATP-SW                     02580000
00259              GO TO 0010-EXIT.                                     02590000
00260                                                                   02600000
00261 *    DISPLAY WORK-RECORD.                                         02610000
00262      PERFORM 0020-SPLIT-RTN THRU 0020-EXIT.                       02620000
00263                                                                   02630000
00264  0010-EXIT.                                                       02640000
00265      EXIT.                                                        02650000
00266 /                                                                 02660000
00267  0020-SPLIT-RTN.                                                  02670000
00268                                                                   02680000
00269      IF  WRK-REC-CONT-TAB                                         02690000
00270          PERFORM 0030-WRITE-SURACTP THRU 0030-EXIT.               02700000
00271                                                                   02710000
00272      IF  WRK-REC-CONT-BEN-TAB-PROV                                02720000
00273          PERFORM 0040-WRITE-SURABTP THRU 0040-EXIT.               02730000
00274                                                                   02740000
00275      IF  WRK-REC-GROUP-SPEC-TAB                                   02750000
00276          PERFORM 0050-WRITE-SURAGTP THRU 0050-EXIT.               02760000
00277                                                                   02770000
00278  0020-EXIT.                                                       02780000
00279      EXIT.                                                        02790000
00280 /                                                                 02800000
00281  0030-WRITE-SURACTP.                                              02810000
00282                                                                   02820000
00283      IF  SURATP-TAB-ID EQUAL '#ABM  '                             02830000
00284          MOVE SURATP-ENT-COUNT   TO GAA-ENTRY-COUNT               02840000
00285          MOVE SURATP-REC         TO SURACTP-REC-A                 02850000
00286          WRITE SURACTP-REC-A                                      02860000
00287          GO TO 0030-EXIT.                                         02870000
00288                                                                   02880000
00289      IF  SURATP-TAB-ID EQUAL '#ACL  '                             02890000
00290          MOVE SURATP-ENT-COUNT   TO GAB-ENTRY-COUNT               02900000
00291          MOVE SURATP-REC         TO SURACTP-REC-B                 02910000
00292          WRITE SURACTP-REC-B                                      02920000
00293          GO TO 0030-EXIT.                                         02930000
00294                                                                   02940000
00295      IF  SURATP-TAB-ID EQUAL '#ACP  '                             02950000
00296          MOVE SURATP-ENT-COUNT   TO GAF-ENTRY-COUNT               02960000
00297          MOVE SURATP-REC         TO SURACTP-REC-J                 02970000
00298          WRITE SURACTP-REC-J                                      02980000
00299          GO TO 0030-EXIT.                                         02990000
00300                                                                   03000000
00301      IF  SURATP-TAB-ID EQUAL '#ADL  '                             03010000
00302          MOVE SURATP-ENT-COUNT   TO GAC-ENTRY-COUNT               03020000
00303          MOVE SURATP-REC         TO SURACTP-REC-C                 03030000
00304          WRITE SURACTP-REC-C                                      03040000
00305          GO TO 0030-EXIT.                                         03050000
00306                                                                   03060000
00307      IF  SURATP-TAB-ID EQUAL '#AOL  '                             03070000
00308          MOVE SURATP-ENT-COUNT   TO GAD-ENTRY-COUNT               03080000
00309          MOVE SURATP-REC         TO SURACTP-REC-D                 03090000
00310          WRITE SURACTP-REC-D                                      03100000
00311          GO TO 0030-EXIT.                                         03110000
00312                                                                   03120000
00313      IF  SURATP-TAB-ID EQUAL '#AAR  '                             03130000
00314          MOVE SURATP-ENT-COUNT   TO GAE-ENTRY-COUNT               03140000
00315          MOVE SURATP-REC         TO SURACTP-REC-E                 03150000
00316          WRITE SURACTP-REC-E                                      03160000
00317          GO TO 0030-EXIT.                                         03170000
00318                                                                   03180000
00319      IF  SURATP-TAB-ID EQUAL '#ACON '                             03190000
00320          MOVE SURATP-ENT-COUNT   TO GAI-ENTRY-COUNT               03200000
00321          MOVE SURATP-REC         TO SURACTP-REC-F                 03210000
00322          WRITE SURACTP-REC-F                                      03220000
00323          GO TO 0030-EXIT.                                         03230000
00324                                                                   03240000
00325      IF  SURATP-TAB-ID EQUAL '#ACOS '                             03250000
00326          MOVE SURATP-ENT-COUNT   TO GAJ-ENTRY-COUNT               03260000
00327          MOVE SURATP-REC         TO SURACTP-REC-G                 03270000
00328          WRITE SURACTP-REC-G                                      03280000
00329          GO TO 0030-EXIT.                                         03290000
00330                                                                   03300000
00331      IF  SURATP-TAB-ID EQUAL '#ADIP '                             03310000
00332          MOVE SURATP-ENT-COUNT   TO GAG-ENTRY-COUNT               03320000
00333          MOVE SURATP-REC         TO SURACTP-REC-H                 03330000
00334          WRITE SURACTP-REC-H                                      03340000
00335          GO TO 0030-EXIT.                                         03350000
00336                                                                   03360000
00337      IF  SURATP-TAB-ID EQUAL '#ADOP '                             03370000
00338          MOVE SURATP-ENT-COUNT   TO GAH-ENTRY-COUNT               03380000
00339          MOVE SURATP-REC         TO SURACTP-REC-I                 03390000
00340          WRITE SURACTP-REC-I                                      03400000
00341          GO TO 0030-EXIT.                                         03410000
00342                                                                   03420000
00343  0030-EXIT.                                                       03430000
00344      EXIT.                                                        03440000
00345 /                                                                 03450000
00346  0040-WRITE-SURABTP.                                              03460000
00347                                                                   03470000
00348      IF  SURATP-TAB-ID EQUAL '#ABM  '                             03480000
00349          MOVE SURATP-ENT-COUNT   TO GAA2-ENTRY-COUNT              03490000
00350          MOVE SURATP-REC         TO SURABTP-REC-A                 03500000
00351          WRITE SURABTP-REC-A                                      03510000
00352          GO TO 0040-EXIT.                                         03520000
00353                                                                   03530000
00354      IF  SURATP-TAB-ID EQUAL '#ACL  '                             03540000
00355          MOVE SURATP-ENT-COUNT   TO GAB2-ENTRY-COUNT              03550000
00356          MOVE SURATP-REC         TO SURABTP-REC-B                 03560000
00357          WRITE SURABTP-REC-B                                      03570000
00358          GO TO 0040-EXIT.                                         03580000
00359                                                                   03590000
00360      IF  SURATP-TAB-ID EQUAL '#ACP  '                             03600000
00361          MOVE SURATP-ENT-COUNT   TO GAF2-ENTRY-COUNT              03610000
00362          MOVE SURATP-REC         TO SURABTP-REC-J                 03620000
00363          WRITE SURABTP-REC-J                                      03630000
00364          GO TO 0040-EXIT.                                         03640000
00365                                                                   03650000
00366      IF  SURATP-TAB-ID EQUAL '#ADL  '                             03660000
00367          MOVE SURATP-ENT-COUNT   TO GAC2-ENTRY-COUNT              03670000
00368          MOVE SURATP-REC         TO SURABTP-REC-C                 03680000
00369          WRITE SURABTP-REC-C                                      03690000
00370          GO TO 0040-EXIT.                                         03700000
00371                                                                   03710000
00372      IF  SURATP-TAB-ID EQUAL '#AOL  '                             03720000
00373          MOVE SURATP-ENT-COUNT   TO GAD2-ENTRY-COUNT              03730000
00374          MOVE SURATP-REC         TO SURABTP-REC-D                 03740000
00375          WRITE SURABTP-REC-D                                      03750000
00376          GO TO 0040-EXIT.                                         03760000
00377                                                                   03770000
00378      IF  SURATP-TAB-ID EQUAL '#AAR  '                             03780000
00379          MOVE SURATP-ENT-COUNT   TO GAE2-ENTRY-COUNT              03790000
00380          MOVE SURATP-REC         TO SURABTP-REC-E                 03800000
00381          WRITE SURABTP-REC-E                                      03810000
00382          GO TO 0040-EXIT.                                         03820000
00383                                                                   03830000
00384      IF  SURATP-TAB-ID EQUAL '#ACON '                             03840000
00385          MOVE SURATP-ENT-COUNT   TO GAI2-ENTRY-COUNT              03850000
00386          MOVE SURATP-REC         TO SURABTP-REC-F                 03860000
00387          WRITE SURABTP-REC-F                                      03870000
00388          GO TO 0040-EXIT.                                         03880000
00389                                                                   03890000
00390      IF  SURATP-TAB-ID EQUAL '#ACOS '                             03900000
00391          MOVE SURATP-ENT-COUNT   TO GAJ2-ENTRY-COUNT              03910000
00392          MOVE SURATP-REC         TO SURABTP-REC-G                 03920000
00393          WRITE SURABTP-REC-G                                      03930000
00394          GO TO 0040-EXIT.                                         03940000
00395                                                                   03950000
00396      IF  SURATP-TAB-ID EQUAL '#ADIP '                             03960000
00397          MOVE SURATP-ENT-COUNT   TO GAG2-ENTRY-COUNT              03970000
00398          MOVE SURATP-REC         TO SURABTP-REC-H                 03980000
00399          WRITE SURABTP-REC-H                                      03990000
00400          GO TO 0040-EXIT.                                         04000000
00401                                                                   04010000
00402      IF  SURATP-TAB-ID EQUAL '#ADOP '                             04020000
00403          MOVE SURATP-ENT-COUNT   TO GAH2-ENTRY-COUNT              04030000
00404          MOVE SURATP-REC         TO SURABTP-REC-I                 04040000
00405          WRITE SURABTP-REC-I                                      04050000
00406          GO TO 0040-EXIT.                                         04060000
00407                                                                   04070000
00408  0040-EXIT.                                                       04080000
00409      EXIT.                                                        04090000
00410 /                                                                 04100000
00411  0050-WRITE-SURAGTP.                                              04110000
00412                                                                   04120000
00413      IF  SURATP-TAB-ID EQUAL '#ABM  '                             04130000
00414          MOVE SURATP-ENT-COUNT   TO GAA3-ENTRY-COUNT              04140000
00415          MOVE SURATP-REC         TO SURAGTP-REC-A                 04150000
00416          WRITE SURAGTP-REC-A                                      04160000
00417          GO TO 0050-EXIT.                                         04170000
00418                                                                   04180000
00419      IF  SURATP-TAB-ID EQUAL '#ACL  '                             04190000
00420          MOVE SURATP-ENT-COUNT   TO GAB3-ENTRY-COUNT              04200000
00421          MOVE SURATP-REC         TO SURAGTP-REC-B                 04210000
00422          WRITE SURAGTP-REC-B                                      04220000
00423          GO TO 0050-EXIT.                                         04230000
00424                                                                   04240000
00425      IF  SURATP-TAB-ID EQUAL '#ACP  '                             04250000
00426          MOVE SURATP-ENT-COUNT   TO GAF3-ENTRY-COUNT              04260000
00427          MOVE SURATP-REC         TO SURAGTP-REC-J                 04270000
00428          WRITE SURAGTP-REC-J                                      04280000
00429          GO TO 0050-EXIT.                                         04290000
00430                                                                   04300000
00431      IF  SURATP-TAB-ID EQUAL '#ADL  '                             04310000
00432          MOVE SURATP-ENT-COUNT   TO GAC3-ENTRY-COUNT              04320000
00433          MOVE SURATP-REC         TO SURAGTP-REC-C                 04330000
00434          WRITE SURAGTP-REC-C                                      04340000
00435          GO TO 0050-EXIT.                                         04350000
00436                                                                   04360000
00437      IF  SURATP-TAB-ID EQUAL '#AOL  '                             04370000
00438          MOVE SURATP-ENT-COUNT   TO GAD3-ENTRY-COUNT              04380000
00439          MOVE SURATP-REC         TO SURAGTP-REC-D                 04390000
00440          WRITE SURAGTP-REC-D                                      04400000
00441          GO TO 0050-EXIT.                                         04410000
00442                                                                   04420000
00443      IF  SURATP-TAB-ID EQUAL '#AAR  '                             04430000
00444          MOVE SURATP-ENT-COUNT   TO GAE3-ENTRY-COUNT              04440000
00445          MOVE SURATP-REC         TO SURAGTP-REC-E                 04450000
00446          WRITE SURAGTP-REC-E                                      04460000
00447          GO TO 0050-EXIT.                                         04470000
00448                                                                   04480000
00449      IF  SURATP-TAB-ID EQUAL '#ACON '                             04490000
00450          MOVE SURATP-ENT-COUNT   TO GAI3-ENTRY-COUNT              04500000
00451          MOVE SURATP-REC         TO SURAGTP-REC-F                 04510000
00452          WRITE SURAGTP-REC-F                                      04520000
00453          GO TO 0050-EXIT.                                         04530000
00454                                                                   04540000
00455      IF  SURATP-TAB-ID EQUAL '#ACOS '                             04550000
00456          MOVE SURATP-ENT-COUNT   TO GAJ3-ENTRY-COUNT              04560000
00457          MOVE SURATP-REC         TO SURAGTP-REC-G                 04570000
00458          WRITE SURAGTP-REC-G                                      04580000
00459          GO TO 0050-EXIT.                                         04590000
00460                                                                   04600000
00461      IF  SURATP-TAB-ID EQUAL '#ADIP '                             04610000
00462          MOVE SURATP-ENT-COUNT   TO GAG3-ENTRY-COUNT              04620000
00463          MOVE SURATP-REC         TO SURAGTP-REC-H                 04630000
00464          WRITE SURAGTP-REC-H                                      04640000
00465          GO TO 0050-EXIT.                                         04650000
00466                                                                   04660000
00467      IF  SURATP-TAB-ID EQUAL '#ADOP '                             04670000
00468          MOVE SURATP-ENT-COUNT   TO GAH3-ENTRY-COUNT              04680000
00469          MOVE SURATP-REC         TO SURAGTP-REC-I                 04690000
00470          WRITE SURAGTP-REC-I                                      04700000
00471          GO TO 0050-EXIT.                                         04710000
00472                                                                   04720000
00473  0050-EXIT.                                                       04730000
00474      EXIT.                                                        04740000
