00001  IDENTIFICATION DIVISION.                                         00010000
00002  PROGRAM-ID. GC0064.                                              00020000
00003  AUTHOR. ANNE KING.                                               00030000
00004  INSTALLATION. HCSC.                                              00040000
00005  DATE-WRITTEN.  JUL 24,2001.                                      00050000
00006  DATE-COMPILED.                                                   00060000
00007 ******************************************************************00070000
00008 *                                                                 00080000
00009 *    THIS PROGRAM CREATES A NEW RELEASE ALL LEVEL TAB FILE        00090000
00010 *    FILE WITH UPDATED SLOT NUMBERS FOR ALL LEVEL INTERNALS.      00100000
00011 *                                                                *00110000
00012 ******************************************************************00120000
00013 *                  U P D A T E   H I S T O R Y                   *00130000
00014 *                                                                *00140000
00015 **-NUM-* *-DATE-* *WHO* *----------  DESCRIPTION  ---------------*00150000
00016 *                                                                *00160000
00017 * D-358   07/24/01 AKK   CREATED PROGRAM AS CLONE OF GC0060 TO   *00170000
00018 *                         PROCESS #CDRS TABULAR THE SAME WAY     *00180000
00019 *                         THE ALL-LEVEL TABULARS ARE PROCESSED,  *00190000
00020 *                         BUT SEPERATELY.                         00200000
00021 *                                                                *00210000
00022 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *00220000
00023 *                                                                *00220102
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION          00221002
00023 *                                                                *00230000
00024 ******************************************************************00240000
00025  ENVIRONMENT DIVISION.                                            00250000
00026  CONFIGURATION SECTION.                                           00260000
00027  SOURCE-COMPUTER. IBM-370.                                        00270000
00028  OBJECT-COMPUTER. IBM-370.                                        00280000
00029  INPUT-OUTPUT SECTION.                                            00290000
00030  FILE-CONTROL.                                                    00300000
00031      SELECT RLSE-CDRS-TAB-FILE                                    00310000
00032                              ASSIGN TO UT-S-GC0064A.              00320000
00033      SELECT COMP-RLSE-CDRS-INT-TAB-FILE                           00330000
00034                              ASSIGN TO UT-S-GC0064B.              00340000
00035      SELECT UPDT-RLSE-CDRS-TAB-FILE                               00350000
00036                              ASSIGN TO UT-S-GC0064C.              00360000
00037 /                                                                 00370000
00038  DATA DIVISION.                                                   00380000
00039  FILE SECTION.                                                    00390000
00040                                                                   00400000
00041  FD  RLSE-CDRS-TAB-FILE                                           00410000
00042      LABEL RECORDS ARE STANDARD                                   00420000
00043      RECORDING MODE IS V                                          00430000
00044      BLOCK CONTAINS 0 RECORDS.                                    00440000
00045  01  RLSE-ALL-LEVEL-TAB-REC.                                      00450000
00046      COPY GCWRKDCC.                                               00460000
00047      05  RALT-REC.                                                00470000
00048          10  RALT-ID             PIC X(6).                        00480000
00049          10  FILLER              PIC X(31364).                    00490004
00050  01  RALT-CDRS-REC.                                               00500000
00051      05  FILLER                  PIC X(100).                      00510000
00052      COPY GCTCDRSC.                                               00520000
00053 /                                                                 00530000
00054  FD  COMP-RLSE-CDRS-INT-TAB-FILE                                  00540000
00055      LABEL RECORDS ARE STANDARD                                   00550000
00056      RECORDING MODE IS V                                          00560000
00057      BLOCK CONTAINS 0 RECORDS.                                    00570000
00058  01  COMP-RLSE-INT-TAB-REC.                                       00580000
00059      05  CRIT-REC-KEY.                                            00590000
00060          10  CRIT-KEY-FLDS.                                       00600000
00061              15  CRIT-MATCH              PIC X(30).               00610000
00062              15  CRIT-REC-TYPE           PIC X(2).                00620000
00063                  88 CRIT-CONT-BEN-TAB-TAB VALUE 'C6' 'T6'.        00630000
00064              15  CRIT-BEN-ID-SLOT.                                00640000
00065                  20  CRIT-BEN-ID         PIC X(6).                00650000
00066                  20  CRIT-BEN-SLOT       PIC S9(7)   COMP-3.      00660000
00067              15  CRIT-TAB-ID-SLOT.                                00670000
00068                  20  CRIT-TAB-ID         PIC X(6).                00680000
00069                  20  CRIT-TAB-SLOT       PIC S9(7)   COMP-3.      00690000
00070              15  FILLER                  PIC X(5).                00700000
00071          10  FILLER                      PIC X(6).                00710000
00072          10  CRIT-INT-BEN-ID             PIC X(6).                00720000
00073          10  FILLER                      PIC X(31).               00730000
00074      05  CRIT-INT-REC-DATA.                                       00740000
00075          10  CRIT-INT-ID-SLOT.                                    00750000
00076              15  CRIT-INT-ID             PIC X(6).                00760000
00077              15  CRIT-INT-SLOT-NO        PIC S9(7)   COMP-3.      00770000
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION          00771002
00078          10  FILLER                      PIC X(31360).            00780001
00079 /                                                                 00790000
00080  FD  UPDT-RLSE-CDRS-TAB-FILE                                      00800000
00081      LABEL RECORDS ARE STANDARD                                   00810000
00082      RECORDING MODE IS V                                          00820000
00083      BLOCK CONTAINS 0 RECORDS.                                    00830000
00084  01  UPDT-RLSE-ALL-LEVEL-TAB-REC.                                 00840000
00085      05  URALT-REC-KEY.                                           00850000
00086          10  URALT-KEY           PIC X(57).                       00860000
00087          10  FILLER              PIC X(43).                       00870000
      *DM9441 09/04/09 JJS  CHANGED        FOR FILE CONVERSION          00871003
00088      05  URALT-REC               PIC X(31370).                    00880002
00089  01  URALT-CDRS-REC.                                              00890000
00090      05  FILLER                  PIC X(100).                      00900000
00091      COPY GCTCDRS2.                                               00910000
00092 /                                                                 00920000
00093  WORKING-STORAGE SECTION.                                         00930000
00094                                                                   00940000
00095  01  FILLER                  PIC X(22)   VALUE                    00950000
00096                              'GC0060 WORKING STORAGE'.            00960000
00097                                                                   00970000
00098  01  SWITCH-AREA.                                                 00980000
00099      05  RALT-SW             PIC X       VALUE SPACES.            00990000
00100          88  EOF-RALT                VALUE HIGH-VALUES.           01000000
00101      05  DRIVER-SW           PIC X       VALUE SPACES.            01010000
00102          88  DRIVER-OK               VALUE HIGH-VALUES.           01020000
00103      05  ADDN-DRIVER-SW      PIC X       VALUE SPACES.            01030000
00104          88  ADDN-DRIVER-OK          VALUE HIGH-VALUES.           01040000
00105      05  CRIT-SW             PIC X       VALUE SPACES.            01050000
00106          88  EOF-CRIT                VALUE HIGH-VALUES.           01060000
00107      05  INT-ID-FOUND-SW     PIC X       VALUE SPACES.            01070000
00108          88  INT-ID-FOUND            VALUE HIGH-VALUES.           01080000
00109                                                                   01090000
00110  01  ABEND-CODE              PIC 9(4)    COMP.                    01100000
00111                                                                   01110000
00112  01  TAB-ID-TEST.                                                 01120000
00113      05  TAB-POS-1           PIC X       VALUE SPACES.            01130000
00114          88  TAB-ID-VALID            VALUE '#'.                   01140000
00115      05  FILLER              PIC X(5)    VALUE SPACES.            01150000
00116                                                                   01160000
00117  01  RALT-KEY.                                                    01170000
00118      05  RALT-KEY-FLD-1      PIC X(30)   VALUE SPACES.            01180000
00119      05  RALT-KEY-FLD-2      PIC X(6)    VALUE SPACES.            01190000
00120      05  RALT-KEY-FLD-3      PIC X(6)    VALUE SPACES.            01200000
00121                                                                   01210000
00122  01  CRIT-KEY.                                                    01220000
00123      05  CRIT-KEY-FLD-1      PIC X(30)   VALUE SPACES.            01230000
00124      05  CRIT-KEY-FLD-2      PIC X(6)    VALUE SPACES.            01240000
00125      05  CRIT-KEY-FLD-3      PIC X(6)    VALUE SPACES.            01250000
00126                                                                   01260000
00127  01  TS-TAB-COUNT            PIC S9      COMP-3.                  01270000
00128  01  WS-SLOT-DISPLAY         PIC X(7).                            01280000
00129                                                                   01290000
00130 *01  TAB-ID-SLOT-HOLD-AREA.                                       01300000
00131 *    05  TAB-ID-SLOT-HOLD      OCCURS 5 TIMES                     01310000
00132 *                              INDEXED BY TS-INDEX.               01320000
00133 *        10  TAB-ID-HOLD         PIC X(6).                        01330000
00134 *        10  TAB-SLOT-HOLD       PIC S9(7)   COMP-3.              01340000
00135 *                                                                 01350000
00136  01  TAB-ID-SLOT-HOLD-AREA.                                       01360000
00137    02 TAB-PRIMARY-AREA.                                           01370000
00138      05  TAB-ENTRY-COUNT  PIC S9(05) COMP-3.                      01380000
00139      05  TAB-SEQ-NO       PIC 9(02).                              01390000
00140      05  TAB-ADDN-SEQ-NO       PIC 9(02).                         01400000
00141      05  TAB-DRIVER-SLOT.                                         01410000
00142          10 TAB-DRIVER    PIC X(06).                              01420000
00143          10 TAB-SLOT-NO   PIC S9(07) COMP-3.                      01430000
00144                                                                   01440000
00145  01  GCCDRLEN-REC-WORK-AREA.                                      01450000
00146      COPY GCCDRLEN.                                               01460000
00147                                                                   01470000
00148 /                                                                 01480000
00149  LINKAGE SECTION.                                                 01490000
00150 /                                                                 01500000
00151  PROCEDURE DIVISION.                                              01510000
00152                                                                   01520000
00153  0000-MAINLINE.                                                   01530000
00154                                                                   01540000
00155      OPEN INPUT  RLSE-CDRS-TAB-FILE                               01550000
00156                  COMP-RLSE-CDRS-INT-TAB-FILE                      01560000
00157           OUTPUT UPDT-RLSE-CDRS-TAB-FILE.                         01570000
00158                                                                   01580000
00159      PERFORM 0010-READ-RALT-REC THRU 0010-EXIT.                   01590000
00160      PERFORM 0020-READ-CRIT-REC THRU 0020-EXIT.                   01600000
00161      PERFORM 0060-MATCH THRU 0060-EXIT                            01610000
00162          UNTIL EOF-RALT OR EOF-CRIT.                              01620000
00163                                                                   01630000
00164      IF  EOF-RALT AND EOF-CRIT                                    01640000
00165          NEXT SENTENCE                                            01650000
00166      ELSE                                                         01660000
00167          IF  EOF-RALT                                             01670000
00168              MOVE 1000           TO ABEND-CODE                    01680000
00169              GO TO 9999-ERROR-RTN                                 01690000
00170          ELSE                                                     01700000
00171              PERFORM 0050-END-RALT-FILE THRU 0050-EXIT            01710000
00172                  UNTIL EOF-RALT.                                  01720000
00173                                                                   01730000
00174      CLOSE RLSE-CDRS-TAB-FILE                                     01740000
00175            COMP-RLSE-CDRS-INT-TAB-FILE                            01750000
00176            UPDT-RLSE-CDRS-TAB-FILE.                               01760000
00177                                                                   01770000
00178      GOBACK.                                                      01780000
00179 /                                                                 01790000
00180  0010-READ-RALT-REC.                                              01800000
00181                                                                   01810000
00182      READ RLSE-CDRS-TAB-FILE                                      01820000
00183          AT END                                                   01830000
00184              MOVE HIGH-VALUES    TO RALT-SW                       01840000
00185              GO TO 0010-EXIT.                                     01850000
00186                                                                   01860000
00187      MOVE WRK-MATCH-CONT         TO RALT-KEY-FLD-1.               01870000
00188      MOVE RALT-ID                TO RALT-KEY-FLD-3.               01880000
00189                                                                   01890000
00190      IF  WRK-REC-CONT-BEN-TAB-PROV                                01900000
00191          MOVE WRK-BN-PROV-ID     TO RALT-KEY-FLD-2                01910000
00192      ELSE                                                         01920000
00193          MOVE SPACES             TO RALT-KEY-FLD-2.               01930000
00194                                                                   01940000
00195  0010-EXIT.                                                       01950000
00196      EXIT.                                                        01960000
00197 /                                                                 01970000
00198  0020-READ-CRIT-REC.                                              01980000
00199                                                                   01990000
00200      READ COMP-RLSE-CDRS-INT-TAB-FILE                             02000000
00201          AT END                                                   02010000
00202              MOVE HIGH-VALUES    TO CRIT-SW.                      02020000
00203                                                                   02030000
00204      MOVE CRIT-MATCH             TO CRIT-KEY-FLD-1.               02040000
00205      MOVE CRIT-BEN-ID            TO CRIT-KEY-FLD-3.               02050000
00206                                                                   02060000
00207      IF  CRIT-CONT-BEN-TAB-TAB                                    02070000
00208          MOVE CRIT-INT-BEN-ID    TO CRIT-KEY-FLD-2                02080000
00209      ELSE                                                         02090000
00210          MOVE SPACES             TO CRIT-KEY-FLD-2.               02100000
00211                                                                   02110000
00212  0020-EXIT.                                                       02120000
00213      EXIT.                                                        02130000
00214 /                                                                 02140000
00215  0030-WRITE-URALT-REC.                                            02150000
00216                                                                   02160000
00217      IF  RALT-ID EQUAL '#CDRS '                                   02170000
00218          MOVE GTE-ENTRY-COUNT    TO GTE2-ENTRY-COUNT              02180000
00219          MOVE RALT-CDRS-REC       TO URALT-CDRS-REC               02190000
00220          WRITE URALT-CDRS-REC.                                    02200000
00221                                                                   02210000
00222  0030-EXIT.                                                       02220000
00223      EXIT.                                                        02230000
00224 /                                                                 02240000
00225 *0048-VALID-SLOT.                                                 02250000
00226 *                                                                 02260000
00227 *    MOVE TAB-ID-HOLD (TS-INDEX) TO TAB-ID-TEST.                  02270000
00228 *    IF  TAB-ID-VALID                                             02280000
00229 *        AND TAB-SLOT-HOLD (TS-INDEX) GREATER THAN 8999999        02290000
00230 *            MOVE 1048           TO ABEND-CODE                    02300000
00231 *            GO TO 9999-ERROR-RTN.                                02310000
00232 *                                                                 02320000
00233 *0048-EXIT.                                                       02330000
00234 *    EXIT.                                                        02340000
00235 /                                                                 02350000
00236  0050-END-RALT-FILE.                                              02360000
00237                                                                   02370000
00238      PERFORM 0030-WRITE-URALT-REC THRU 0030-EXIT.                 02380000
00239      PERFORM 0010-READ-RALT-REC THRU 0010-EXIT.                   02390000
00240                                                                   02400000
00241  0050-EXIT.                                                       02410000
00242      EXIT.                                                        02420000
00243 /                                                                 02430000
00244  0060-MATCH.                                                      02440000
00245                                                                   02450000
00246      IF  RALT-KEY GREATER THAN CRIT-KEY                           02460000
00247          MOVE 1060           TO ABEND-CODE                        02470000
00248          GO TO 9999-ERROR-RTN.                                    02480000
00249                                                                   02490000
00250      IF  RALT-KEY LESS THAN CRIT-KEY                              02500000
00251          PERFORM 0030-WRITE-URALT-REC THRU 0030-EXIT              02510000
00252          PERFORM 0010-READ-RALT-REC THRU 0010-EXIT                02520000
00253          GO TO 0060-EXIT.                                         02530000
00254                                                                   02540000
00255      IF  RALT-ID EQUAL '#CDRS '                                   02550000
00256          SET GTE-INDEX TO 1                                       02560000
00257          SET GTE-ADDL-INDEX TO 1                                  02570000
00258          PERFORM 0070-CDRS-MATCH THRU 0070-EXIT                   02580000
00259              VARYING GTE-INDEX FROM 1 BY 1 UNTIL                  02590000
00260              GTE-INDEX GREATER THAN GTE-ENTRY-COUNT.              02600000
00261                                                                   02610000
00262                                                                   02620000
00263      PERFORM 0020-READ-CRIT-REC THRU 0020-EXIT.                   02630000
00264      INITIALIZE INT-ID-FOUND-SW.                                  02640000
00265                                                                   02650000
00266  0060-EXIT.                                                       02660000
00267      EXIT.                                                        02670000
00268 /                                                                 02680000
00269  0070-CDRS-MATCH.                                                 02690000
00270                                                                   02700000
00271      INITIALIZE INT-ID-FOUND-SW.                                  02710000
00272      IF  GTE-PRIMARY-ENTRY (GTE-INDEX) EQUAL HIGH-VALUES          02720000
00273          MOVE 1070               TO ABEND-CODE                    02730000
00274          GO TO 9999-ERROR-RTN.                                    02740000
00275      PERFORM 0071-VERIFY-DRIVER-TYPE THRU 0071-EXIT               02750000
00276      IF DRIVER-OK                                                 02760000
00277         PERFORM 0075-HOLD-PRIMARY-ITEMS THRU 0075-EXIT            02770000
00278         PERFORM 0110-SEARCH-PRIMARY-ID THRU 0110-EXIT             02780000
00279         IF  INT-ID-FOUND                                          02790000
00280 *           MOVE SPACES             TO INT-ID-FOUND-SW            02800000
00281             MOVE TAB-SLOT-NO                                      02810000
00282                  TO GTE-PRIMARY-SLOT-NO (GTE-INDEX)               02820000
00283             SET GTE-INDEX           TO GTE-ENTRY-COUNT            02830000
00284         ELSE                                                      02840000
00285           PERFORM 0071-CHECK-FOR-ADDN-INFO THRU 0071A-EXIT        02850000
00286         END-IF                                                    02860000
00287      ELSE                                                         02870000
00288          PERFORM 0071-CHECK-FOR-ADDN-INFO THRU 0071A-EXIT         02880000
00289      END-IF.                                                      02890000
00290                                                                   02900000
00291  0070-EXIT.                                                       02910000
00292      EXIT.                                                        02920000
00293 /                                                                 02930000
00294  0071-CHECK-FOR-ADDN-INFO.                                        02940000
00295      SET GTE-ADDL-INDEX TO 1.                                     02950000
00296      PERFORM 0072-VERIFY-DRIVER-ADDN THRU 0072-EXIT.              02960000
00297      IF ADDN-DRIVER-OK                                            02970000
00298        PERFORM 0077-SEARCH-ADDN-ITEMS THRU 0077-EXIT              02980000
00299      END-IF.                                                      02990000
00300  0071A-EXIT.                                                      03000000
00301      EXIT.                                                        03010000
00302                                                                   03020000
00303  0072-VERIFY-DRIVER-ADDN.                                         03030000
00304      INITIALIZE ADDN-DRIVER-SW.                                   03040000
00305      IF GTE-ADDL-IRDX-DRIVER (GTE-INDEX GTE-ADDL-INDEX) OR        03050000
00306         GTE-ADDL-IRIC-DRIVER (GTE-INDEX GTE-ADDL-INDEX) OR        03060000
00307         GTE-ADDL-IRPR-DRIVER (GTE-INDEX GTE-ADDL-INDEX) OR        03070000
00308         GTE-ADDL-IRPV-DRIVER (GTE-INDEX GTE-ADDL-INDEX)           03080000
00309            SET ADDN-DRIVER-OK TO TRUE                             03090000
00310      END-IF.                                                      03100000
00311                                                                   03110000
00312  0072-EXIT.                                                       03120000
00313      EXIT.                                                        03130000
00314                                                                   03140000
00315  0071-VERIFY-DRIVER-TYPE.                                         03150000
00316      INITIALIZE DRIVER-SW.                                        03160000
00317      IF GTE-PRIM-IRDX-DRIVER (GTE-INDEX) OR                       03170000
00318         GTE-PRIM-IRIC-DRIVER (GTE-INDEX) OR                       03180000
00319         GTE-PRIM-IRPR-DRIVER (GTE-INDEX) OR                       03190000
00320         GTE-PRIM-IRPV-DRIVER (GTE-INDEX)                          03200000
00321            SET DRIVER-OK TO TRUE                                  03210000
00322      END-IF.                                                      03220000
00323  0071-EXIT.                                                       03230000
00324      EXIT.                                                        03240000
00325                                                                   03250000
00326  0075-HOLD-PRIMARY-ITEMS.                                         03260000
00327      MOVE GTE-ENTRY-COUNT TO TAB-ENTRY-COUNT.                     03270000
00328      MOVE GTE-PRIM-SEQ-NO (GTE-INDEX) TO TAB-SEQ-NO.              03280000
00329      MOVE GTE-PRIMARY-DRIVER (GTE-INDEX) TO TAB-DRIVER.           03290000
00330      MOVE GTE-PRIMARY-SLOT-NO (GTE-INDEX) TO TAB-SLOT-NO.         03300000
00331  0075-EXIT.                                                       03310000
00332      EXIT.                                                        03320000
00333                                                                   03330000
00334  0077-SEARCH-ADDN-ITEMS.                                          03340000
00335      MOVE GTE-ADDL-ENTRY-COUNT (GTE-INDEX)                        03350000
00336                 TO TAB-ENTRY-COUNT.                               03360000
00337      PERFORM VARYING GTE-ADDL-INDEX FROM 1 BY 1                   03370000
00338        UNTIL GTE-ADDITIONAL-DRIVER (GTE-INDEX GTE-ADDL-INDEX)     03380000
00339            = HIGH-VALUES                                          03390000
00340         PERFORM 0078-HOLD-ADDN-FIELDS THRU 0078-EXIT              03400000
00341         PERFORM 0085-RESEARCH-ADDN-DRIVER THRU 0085-EXIT          03410000
00342      END-PERFORM.                                                 03420000
00343  0077-EXIT.                                                       03430000
00344      EXIT.                                                        03440000
00345                                                                   03450000
00346  0078-HOLD-ADDN-FIELDS.                                           03460000
00347      MOVE GTE-ADDL-SEQ-NO (GTE-INDEX GTE-ADDL-INDEX)              03470000
00348              TO TAB-SEQ-NO.                                       03480000
00349      MOVE GTE-ADDITIONAL-DRIVER (GTE-INDEX GTE-ADDL-INDEX)        03490000
00350                 TO TAB-DRIVER.                                    03500000
00351      MOVE GTE-ADDITIONAL-SLOT-NO (GTE-INDEX GTE-ADDL-INDEX)       03510000
00352                TO TAB-SLOT-NO.                                    03520000
00353  0078-EXIT.                                                       03530000
00354      EXIT.                                                        03540000
00355                                                                   03550000
00356  0085-RESEARCH-ADDN-DRIVER.                                       03560000
00357      PERFORM 0111-SEARCH-ADDN-ID THRU 0111-EXIT                   03570000
00358      IF  INT-ID-FOUND                                             03580000
00359          MOVE SPACES             TO INT-ID-FOUND-SW               03590000
00360          MOVE TAB-SLOT-NO                                         03600000
00361          TO GTE-ADDITIONAL-SLOT-NO (GTE-INDEX GTE-ADDL-INDEX)     03610000
00362          SET GTE-ADDL-INDEX TO GTE-ADDL-ENTRY-COUNT (GTE-INDEX)   03620000
00363          SET GTE-INDEX TO GTE-ENTRY-COUNT                         03630000
00364      END-IF.                                                      03640000
00365                                                                   03650000
00366  0085-EXIT.                                                       03660000
00367      EXIT.                                                        03670000
00368                                                                   03680000
00369  0110-SEARCH-PRIMARY-ID.                                          03690000
00370                                                                   03700000
00371      IF  TAB-PRIMARY-AREA EQUAL HIGH-VALUES                       03710000
00372 *        SET TS-INDEX            TO TS-TAB-COUNT                  03720000
00373          GO TO 0110-EXIT.                                         03730000
00374                                                                   03740000
00375      IF  (TAB-SLOT-NO EQUAL CRIT-TAB-SLOT AND                     03750000
00376          TAB-DRIVER EQUAL CRIT-TAB-ID)                            03760000
00377      AND (TAB-SEQ-NO  =  GTE-PRIM-SEQ-NO (GTE-INDEX))             03770000
00378          MOVE CRIT-INT-SLOT-NO   TO TAB-SLOT-NO                   03780000
00379          MOVE HIGH-VALUES        TO INT-ID-FOUND-SW.              03790000
00380 *        SET TS-INDEX            TO TS-TAB-COUNT.                 03800000
00381                                                                   03810000
00382  0110-EXIT.                                                       03820000
00383      EXIT.                                                        03830000
00384                                                                   03840000
00385  0111-SEARCH-ADDN-ID.                                             03850000
00386                                                                   03860000
00387      IF  TAB-PRIMARY-AREA EQUAL HIGH-VALUES                       03870000
00388 *        SET TS-INDEX            TO TS-TAB-COUNT                  03880000
00389          GO TO 0111-EXIT.                                         03890000
00390                                                                   03900000
00391      IF  TAB-SLOT-NO EQUAL CRIT-TAB-SLOT                          03910000
00392      AND (TAB-SEQ-NO  =                                           03920000
00393              GTE-ADDL-SEQ-NO (GTE-INDEX GTE-ADDL-INDEX))          03930000
00394 *    IF  TAB-ID-SLOT-HOLD (TS-INDEX) EQUAL CRIT-TAB-ID-SLOT       03940000
00395          MOVE CRIT-INT-SLOT-NO   TO TAB-SLOT-NO                   03950000
00396          MOVE HIGH-VALUES        TO INT-ID-FOUND-SW.              03960000
00397 *        SET TS-INDEX            TO TS-TAB-COUNT.                 03970000
00398                                                                   03980000
00399  0111-EXIT.                                                       03990000
00400      EXIT.                                                        04000000
00401 /                                                                 04010000
00402  9999-ERROR-RTN.                                                  04020000
00403                                                                   04030000
00404      CALL 'TSGEND' USING ABEND-CODE.                              04040000
00405                                                                   04050000
00406  9999-EXIT.                                                       04060000
00407      EXIT.                                                        04070000
