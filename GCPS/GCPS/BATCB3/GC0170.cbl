00001  IDENTIFICATION DIVISION.                                         00010000
00002  PROGRAM-ID.         GC0170.                                      00020000
00003  AUTHOR.             DELORES FRY.                                 00030000
00004  INSTALLATION.       HCSC.                                        00040000
00005  DATE-WRITTEN.       DECEMBER 1987.                               00050000
00006  DATE-COMPILED.                                                   00060000
00007 ******************************************************************00070000
00008 *                                                                *00080000
00009 *    CONTRACT TABULAR RECORD MATCH AND SLOT UPDATE PROGRAM       *00090000
00010 *                                                                *00100000
00011 ******************************************************************00110000
00012 ******************************************************************00120000
00013 ******************************************************************00130000
00014 *                                                                *00140000
00015 *       ***-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *00150000
00016 *       *-*         U P D A T E   H I S T O R Y         *-*      *00160000
00017 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *00170000
00018 *                                                                *00180000
00019 *                                                                *00190000
00020 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION--------------- *00200000
00021 *                                                                *00210000
00022 *   11154     3/06/91  FRY   INCREASE RECORD AREAS IN FILE       *00220000
00023 *                            SECTION:                            *00230000
00024 *             INPUT-WRK-COMPARE.                                 *00240000
00025 *             INPUT-WRK-ENTRIES  PIC X(3960)  CHANGED TO  7765.  *00250000
00026 *             OUTPUT-WRK-RECORD  PIC X(4064)  CHANGED TO  7869   *00260000
00027 *                                                                *00270000
00028 *    1293    01/05/93  JGR   REPLACE CURRENT PROCESSING WITH     *00280000
00029 *                            MATCHES AGAINST EXISTING TABULARS   *00290000
00030 *                            USING THE TABULAR HASHING PROGRAM.  *00300000
00031 *                                                                *00310000
00032 *             1/17/95  EMS   CONVERTED TO COBOL II.              *00320000
00033 *                                                                *00330000
00034 * 14726/     11/11/97  GSP  MODIFIED TO BECOME MILLENNIUM        *00340000
00035 * 15057                     COMPLIANT. INCREASED FILLERS FROM    *00350000
00036 *                           64 TO 100 TO REFLECT INCREASE IN     *00360000
00037 *                           INPUT WORK KEY (COPYBOOK GCWRKDCC).  *00370000
00038 *                                                                *00380000
00039 *  D15446    10/12/98  FRY  ADD #CRS CONTRACT TABULAR.           *00390000
00040 *                                                                *00400000
00041 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *00410000
00042 *                                                                *00420000
00043 * D-356A   5/08/03  GTF  RECOMPILE FOR COPYBK CHANGES #CLDR,     *00430000
00043 * D-356A   5/08/03  GTF  RECOMPILE FOR COPYBK CHANGES #CLDR,     *00431001
00044 *                        #CRR.                                   *00440000
00043 * DM9441   9/08/09  JJS  RECOMPILE FOR COPYBK CHANGES            *00441001
00043 * DM9441   COPYBOOK GCTCRS2 - RECOMPILE                          *00442001
00043 * DM9441   COPYBOOK GCTCRS3 - RECOMPILE                          *00443001
00043 * DM9441   9/08/09  JJS  CHANGED FOR FILE CONVERSION             *00444002
00045 ******************************************************************00450000
00046 ******************************************************************00460000
00047  ENVIRONMENT DIVISION.                                            00470000
00048                                                                   00480000
00049  CONFIGURATION SECTION.                                           00490000
00050  SOURCE-COMPUTER.  IBM-370.                                       00500000
00051  OBJECT-COMPUTER.  IBM-370.                                       00510000
00052                                                                   00520000
00053  INPUT-OUTPUT SECTION.                                            00530000
00054                                                                   00540000
00055  FILE-CONTROL.                                                    00550000
00056      SELECT   INPUT-WRK-FILE     ASSIGN  TO   UT-S-GC0170A.       00560000
00057      SELECT   OUTPUT-WRK-FILE    ASSIGN  TO   UT-S-GC0170B.       00570000
00058                                                                   00580000
00059                                                                   00590000
00060  DATA DIVISION.                                                   00600000
00061  FILE SECTION.                                                    00610000
00062                                                                   00620000
00063  FD  INPUT-WRK-FILE                                               00630000
00064      LABEL RECORDS ARE STANDARD                                   00640000
00065      RECORDING MODE IS V                                          00650000
00066      BLOCK CONTAINS  0  RECORDS.                                  00660000
00067                                                                   00670000
00068  01  INPUT-WRK-RECORD.                                            00680000
00069 *    05  INPUT-WRK-KEY                     PIC X(100).            00690000
00070          COPY GCWRKDCC.                                           00700000
00071      05  INPUT-WRK-TABULAR-RECORD.                                00710000
00072          10  INPUT-WRK-TAB-REC-KEY.                               00720000
00073              15  INPUT-WRK-TAB-REC-ID      PIC X(6).              00730000
00074              15  INPUT-WRK-TAB-REC-SLOT    PIC S9(7) COMP-3.      00740000
00075          10  FILLER                        PIC X(27).             00750000
00076          10  INPUT-WRK-COMPARE.                                   00760000
00077              15  INPUT-WRK-TAB-REC-CNT     PIC S9(5) COMP-3.      00770000
00043 * DM9441   9/08/09  JJS  CHANGED FOR FILE CONVERSION             *00771002
00078              15  INPUT-WRK-ENTRIES         PIC X(31330).          00780002
00079 /                                                                 00790000
00080                                                                   00800000
00081  01  INPUT-CLDR-RECORD.                                           00810000
00082      05  FILLER                            PIC X(100).            00820000
00083 /                                                                 00830000
00084      COPY GCTCLDR3.                                               00840000
00085 /                                                                 00850000
00086  01  INPUT-CRR-RECORD.                                            00860000
00087      05  FILLER                            PIC X(100).            00870000
00088 /                                                                 00880000
00089      COPY GCTCRR3.                                                00890000
00090 /                                                                 00900000
00091                                                                   00910000
00092  01  INPUT-CRS-RECORD.                                            00920000
00093      05  FILLER                            PIC X(100).            00930000
00094      COPY GCTCRS3.                                                00940000
00095 /                                                                 00950000
00096                                                                   00960000
00097                                                                   00970000
00098                                                                   00980000
00099  FD  OUTPUT-WRK-FILE                                              00990000
00100      LABEL RECORDS ARE STANDARD                                   01000000
00101      RECORDING MODE IS V                                          01010000
00102      BLOCK CONTAINS  0  RECORDS.                                  01020000
00103                                                                   01030000
00104                                                                   01040000
00105  01  OUTPUT-WRK-RECORD                PIC X(31470).               01050002
00106 /                                                                 01060000
00107                                                                   01070000
00108  01  OUTPUT-CLDR-RECORD.                                          01080000
00109      05 FILLER                        PIC X(100).                 01090000
00110 /                                                                 01100000
00111      COPY  GCTCLDRC.                                              01110000
00112 /                                                                 01120000
00113  01  OUTPUT-CRR-RECORD.                                           01130000
00114      05 FILLER                        PIC X(100).                 01140000
00115 /                                                                 01150000
00116      COPY  GCTCRRC.                                               01160000
00117 /                                                                 01170000
00118  01  OUTPUT-CRS-RECORD.                                           01180000
00119      05 FILLER                        PIC X(100).                 01190000
00120                                                                   01200000
00121      COPY  GCTCRSC.                                               01210000
00122 /                                                                 01220000
00123                                                                   01230000
00124  WORKING-STORAGE SECTION.                                         01240000
00125                                                                   01250000
00126  01  WS-PROGRAM-ID                 PIC  X(27)  VALUE              01260000
00127                                   '* GC0170 WORKING STORAGE *'.   01270000
00128                                                                   01280000
00129  01  WORK-AREAS.                                                  01290000
00130      05  HASH-PROGRAM              PIC X(17) VALUE 'GHS1BAT'.     01300000
00131                                                                   01310000
00132  01  WS-GHS1BAT-CALL-AREA.                                        01320000
00133    03  WS-GHS1BAT-PROCESS-IND      PIC X.                         01330000
00134      88  CALL-FOR-OPEN                       VALUE 'O'.           01340000
00135      88  CALL-FOR-CLOSE                      VALUE 'C'.           01350000
00136      88  CALL-FOR-PROCESS                    VALUE 'P'.           01360000
00137                                                                   01370000
00138  01  ACCUM-SLOT-AREA.                                             01380000
00139      05  HASH-RETURN-CODE          PIC X(02).                     01390000
00140      05  ASUR-REC-AREA.                                           01400000
00141          10  ASUR-TAB-ID           PIC X(0006).                   01410000
00142          10  ASUR-SLOT             PIC S9(7) COMP-3.              01420000
00043 * DM9441   9/08/09  JJS  CHANGED FILLER FOR FILE CONVERSION      *01421002
00143          10  FILLER                PIC X(31360).                  01430002
00144                                                                   01440000
00145 ****                                                              01450000
00146                                                                   01460000
00147  01  WS-CLDR-RECORD.                                              01470000
00148      05  FILLER                            PIC X(100).            01480000
00149 /                                                                 01490000
00150      COPY GCTCLDR2.                                               01500000
00151 /                                                                 01510000
00152  01  WS-CRR-RECORD.                                               01520000
00153      05  FILLER                            PIC X(100).            01530000
00154 /                                                                 01540000
00155      COPY GCTCRR2.                                                01550000
00156 /                                                                 01560000
00157  01  WS-CRS-RECORD.                                               01570000
00158      05  FILLER                            PIC X(100).            01580000
00159 /                                                                 01590000
00160      COPY GCTCRS2.                                                01600000
00161 /                                                                 01610000
00162                                                                   01620000
00163  01  WS-SWITCHES.                                                 01630000
00164      05  FILLER                    PIC  X(16)  VALUE              01640000
00165                                    '*** SWITCHES ***'.            01650000
00166      05  WS-END-OF-FILE-SW         PIC  X(01)  VALUE '0'.         01660000
00167          88  WS-END-OF-FILE-ON                 VALUE '1'.         01670000
00168                                                                   01680000
00169  01  WS-WORK-AREA.                                                01690000
00170      05  FILLER                    PIC  X(17)  VALUE              01700000
00171                                    '*** WORK AREA ***'.           01710000
00172      05  WS-HOLD-LAST-SLOT         PIC S9(07)  VALUE +0    COMP-3.01720000
00173 /                                                                 01730000
00174  PROCEDURE DIVISION.                                              01740000
00175                                                                   01750000
00176 ******************************************************************01760000
00177 **                                                                01770000
00178 **               P R O C E S S    C O N T R O L                   01780000
00179 **                                                                01790000
00180 ******************************************************************01800000
00181  0000-MAINLINE.                                                   01810000
00182                                                                   01820000
00183      PERFORM 1000-OPEN-THE-FILES  THRU  1000-EXIT.                01830000
00184                                                                   01840000
00185      IF WS-END-OF-FILE-ON                                         01850000
00186          DISPLAY 'GC0170--NO INPUT RECORDS RECEIVED'              01860000
00187      ELSE                                                         01870000
00188          PERFORM 3000-PROCESS-ALL-INPUT-RECORDS THRU 3000-EXIT    01880000
00189             UNTIL  WS-END-OF-FILE-ON.                             01890000
00190                                                                   01900000
00191      PERFORM 9000-CLOSE-THE-FILES  THRU  9000-EXIT.               01910000
00192                                                                   01920000
00193      STOP RUN.                                                    01930000
00194                                                                   01940000
00195  0000-EXIT.                                                       01950000
00196      EXIT.                                                        01960000
00197 /                                                                 01970000
00198 ******************************************************************01980000
00199 ***                                                               01990000
00200 **       OPEN SEQUENTIAL FILES AND READ THE FIRST RECORD          02000000
00201 ***                                                               02010000
00202 ******************************************************************02020000
00203  1000-OPEN-THE-FILES.                                             02030000
00204                                                                   02040000
00205      OPEN INPUT    INPUT-WRK-FILE,                                02050000
00206           OUTPUT   OUTPUT-WRK-FILE.                               02060000
00207                                                                   02070000
00208      MOVE 'O'      TO WS-GHS1BAT-PROCESS-IND.                     02080000
00209      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA.                02090000
00210                                                                   02100000
00211 **--- READ THE FIRST RECORD.                                      02110000
00212 **                                                                02120000
00213      PERFORM 2000-READ-INPUT-WRK-FILE THRU 2000-EXIT.             02130000
00214                                                                   02140000
00215  1000-EXIT.                                                       02150000
00216      EXIT.                                                        02160000
00217 /                                                                 02170000
00218 ******************************************************************02180000
00219 **            SEQUENTIAL ALL LEVEL TABULAR FILE                   02190000
00220 ******************************************************************02200000
00221  2000-READ-INPUT-WRK-FILE.                                        02210000
00222                                                                   02220000
00223      READ INPUT-WRK-FILE                                          02230000
00224          AT END                                                   02240000
00225              MOVE  '1'   TO   WS-END-OF-FILE-SW.                  02250000
00226                                                                   02260000
00227  2000-EXIT.                                                       02270000
00228      EXIT.                                                        02280000
00229 /                                                                 02290000
00230 ******************************************************************02300000
00231 ******************************************************************02310000
00232  3000-PROCESS-ALL-INPUT-RECORDS.                                  02320000
00233                                                                   02330000
00234                                                                   02340000
00235      IF INPUT-WRK-TAB-REC-ID = '#CLDR '                           02350000
00236          PERFORM 3100-PROCESS-CLDR-TABULAR THRU 3100-EXIT         02360000
00237          PERFORM 2000-READ-INPUT-WRK-FILE THRU 2000-EXIT          02370000
00238          GO TO 3000-EXIT.                                         02380000
00239                                                                   02390000
00240                                                                   02400000
00241      IF INPUT-WRK-TAB-REC-ID = '#CRR '                            02410000
00242          PERFORM 3200-PROCESS-CRR-TABULAR THRU 3200-EXIT          02420000
00243          PERFORM 2000-READ-INPUT-WRK-FILE THRU 2000-EXIT          02430000
00244          GO TO 3000-EXIT.                                         02440000
00245                                                                   02450000
00246                                                                   02460000
00247      IF INPUT-WRK-TAB-REC-ID = '#CRS '                            02470000
00248          PERFORM 3300-PROCESS-CRS-TABULAR THRU 3300-EXIT          02480000
00249          PERFORM 2000-READ-INPUT-WRK-FILE THRU 2000-EXIT          02490000
00250          GO TO 3000-EXIT.                                         02500000
00251                                                                   02510000
00252      PERFORM 2000-READ-INPUT-WRK-FILE THRU 2000-EXIT.             02520000
00253                                                                   02530000
00254  3000-EXIT.                                                       02540000
00255      EXIT.                                                        02550000
00256 /                                                                 02560000
00257 ******************************************************************02570000
00258 ****               CONTRACT CLDR TABULAR                          02580000
00259 ******************************************************************02590000
00260  3100-PROCESS-CLDR-TABULAR.                                       02600000
00261                                                                   02610000
00262 ***************************************************************   02620000
00263 ***** IF SLOT IS LESS THAN 9,000,000                      *****   02630000
00264 ***** BYPASS THIS RECORD.                                 *****   02640000
00265 ***************************************************************   02650000
00266                                                                   02660000
00267      IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                02670000
00268          GO TO 3100-EXIT.                                         02680000
00269                                                                   02690000
00270 ***************************************************************   02700000
00271 ***** CALL TABULAR HASHING PROGRAM                        *****   02710000
00272 ***** TO RETRIEVE 'EXISTING' OR 'NEW' SLOT NUMBERS.       *****   02720000
00273 ***************************************************************   02730000
00274                                                                   02740000
00275      MOVE LOW-VALUES TO ASUR-REC-AREA.                            02750000
00276      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              02760000
00277      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          02770000
00278      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 02780000
00279                              ACCUM-SLOT-AREA.                     02790000
00280                                                                   02800000
00281 ***************************************************************   02810000
00282 ***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   02820000
00283 ***************************************************************   02830000
00284                                                                   02840000
00285      IF HASH-RETURN-CODE = '00'                                   02850000
00286         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  02860000
00287      ELSE                                                         02870000
00288         IF HASH-RETURN-CODE = '01'                                02880000
00289            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  02890000
00290         ELSE                                                      02900000
00291            GO TO 3100-EXIT.                                       02910000
00292                                                                   02920000
00293                                                                   02930000
00294 ***************************************************************   02940000
00295 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   02950000
00296 ***** THE SEQUENTIAL FILE.               .                *****   02960000
00297 ***************************************************************   02970000
00298                                                                   02980000
00299      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         02990000
00300      MOVE INPUT-WRK-TAB-REC-CNT     TO GTB-ENTRY-COUNT.           03000000
00301      MOVE INPUT-CLDR-RECORD         TO OUTPUT-WRK-RECORD.         03010000
00302      MOVE ASUR-SLOT                 TO GTB-PROVISION-SLOT-NO.     03020000
00303      WRITE OUTPUT-CLDR-RECORD.                                    03030000
00304                                                                   03040000
00305  3100-EXIT.                                                       03050000
00306      EXIT.                                                        03060000
00307 /                                                                 03070000
00308 ******************************************************************03080000
00309 ****               CONTRACT CRR TABULAR                           03090000
00310 ******************************************************************03100000
00311  3200-PROCESS-CRR-TABULAR.                                        03110000
00312                                                                   03120000
00313 ***************************************************************   03130000
00314 ***** IF SLOT IS LESS THAN 9,000,000                      *****   03140000
00315 ***** BYPASS THIS RECORD.                                 *****   03150000
00316 ***************************************************************   03160000
00317                                                                   03170000
00318      IF INPUT-WRK-TAB-REC-SLOT    NOT  >     +8999999             03180000
00319          GO TO 3200-EXIT.                                         03190000
00320                                                                   03200000
00321 ***************************************************************   03210000
00322 ***** CALL TABULAR HASHING PROGRAM                        *****   03220000
00323 ***** TO RETRIEVE 'EXISTING' OR 'NEW' SLOT NUMBERS.       *****   03230000
00324 ***************************************************************   03240000
00325                                                                   03250000
00326      MOVE LOW-VALUES TO ASUR-REC-AREA.                            03260000
00327      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              03270000
00328      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          03280000
00329      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 03290000
00330                              ACCUM-SLOT-AREA.                     03300000
00331                                                                   03310000
00332 ***************************************************************   03320000
00333 ***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   03330000
00334 ***************************************************************   03340000
00335                                                                   03350000
00336      IF HASH-RETURN-CODE = '00'                                   03360000
00337         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  03370000
00338      ELSE                                                         03380000
00339         IF HASH-RETURN-CODE = '01'                                03390000
00340            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  03400000
00341         ELSE                                                      03410000
00342            GO TO 3200-EXIT.                                       03420000
00343                                                                   03430000
00344 ***************************************************************   03440000
00345 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   03450000
00346 ***** THE SEQUENTIAL FILE.               .                *****   03460000
00347 ***************************************************************   03470000
00348                                                                   03480000
00349      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         03490000
00350      MOVE INPUT-WRK-TAB-REC-CNT     TO GTC-ENTRY-COUNT.           03500000
00351      MOVE INPUT-CRR-RECORD          TO OUTPUT-WRK-RECORD.         03510000
00352      MOVE ASUR-SLOT                 TO GTC-PROVISION-SLOT-NO.     03520000
00353      WRITE OUTPUT-CRR-RECORD.                                     03530000
00354                                                                   03540000
00355  3200-EXIT.                                                       03550000
00356      EXIT.                                                        03560000
00357 /                                                                 03570000
00358 ******************************************************************03580000
00359 *                  CONTRACT CRS TABULAR                          *03590000
00360 ******************************************************************03600000
00361  3300-PROCESS-CRS-TABULAR.                                        03610000
00362                                                                   03620000
00363 ***                                                               03630000
00364 *  IF SLOT IS LESS THAN 9,000,000, BYPASS THIS RECORD             03640000
00365 ***                                                               03650000
00366                                                                   03660000
00367      IF INPUT-WRK-TAB-REC-SLOT    NOT  >     +8999999             03670000
00368          GO TO 3300-EXIT.                                         03680000
00369                                                                   03690000
00370 ***                                                               03700000
00371 *  CALL TABULAR HASHING PROGRAM TO RETRIEVE                       03710000
00372 *  'EXISTING'  OR  'NEW'  SLOT NUMBERS.                           03720000
00373 ***                                                               03730000
00374                                                                   03740000
00375      MOVE LOW-VALUES TO ASUR-REC-AREA.                            03750000
00376      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              03760000
00377      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          03770000
00378      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 03780000
00379                              ACCUM-SLOT-AREA.                     03790000
00380                                                                   03800000
00381 ***                                                               03810000
00382 *  MARK RECORDS AS 'EXISTING' OR 'NEW'.                           03820000
00383 ***                                                               03830000
00384                                                                   03840000
00385      IF HASH-RETURN-CODE = '00'                                   03850000
00386         MOVE 'E'                 TO WRK-SIGNAL-BATCH-INTERNAL     03860000
00387      ELSE                                                         03870000
00388      IF HASH-RETURN-CODE = '01'                                   03880000
00389         MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL     03890000
00390      ELSE                                                         03900000
00391         GO TO 3300-EXIT.                                          03910000
00392                                                                   03920000
00393 ***                                                               03930000
00394 *  MOVE IN TABULAR SLOT AND WRITE RECORD TO THE SEQUENTIAL FILE   03940000
00395 ***                                                               03950000
00396                                                                   03960000
00397      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         03970000
00398      MOVE INPUT-WRK-TAB-REC-CNT     TO GTD-ENTRY-COUNT.           03980000
00399      MOVE INPUT-CRS-RECORD          TO OUTPUT-WRK-RECORD.         03990000
00400      MOVE ASUR-SLOT                 TO GTD-PROVISION-SLOT-NO.     04000000
00401      WRITE OUTPUT-CRS-RECORD.                                     04010000
00402                                                                   04020000
00403  3300-EXIT.                                                       04030000
00404      EXIT.                                                        04040000
00405 /                                                                 04050000
00406 ******************************************************************04060000
00407 **                                                                04070000
00408 **                  CLOSE SEQUENTIAL FILES                        04080000
00409 **                                                                04090000
00410 ******************************************************************04100000
00411  9000-CLOSE-THE-FILES.                                            04110000
00412                                                                   04120000
00413      CLOSE INPUT-WRK-FILE,                                        04130000
00414            OUTPUT-WRK-FILE.                                       04140000
00415                                                                   04150000
00416      MOVE 'C'      TO WS-GHS1BAT-PROCESS-IND.                     04160000
00417      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA.                04170000
00418                                                                   04180000
00419  9000-EXIT.                                                       04190000
00420      EXIT.                                                        04200000
00421 /                                                                 04210000
