00001  IDENTIFICATION DIVISION.                                         00010000
00002 *THIS IS A COBOL/2 PROGRAM                                        00020000
00003  PROGRAM-ID.         GC0068.                                      00030000
00004  AUTHOR.             DELORES FRY.                                 00040000
00005  INSTALLATION.       HCSC.                                        00050000
00006  DATE-WRITTEN.       DECEMBER 1987.                               00060000
00007  DATE-COMPILED.                                                   00070000
00008 ******************************************************************00080000
00009 *                                                                *00090000
00010 *               ALL LEVEL TABULAR RECORD                         *00100000
00011 *             MATCH AND SLOT UPDATE PROGRAM                      *00110000
00012 *                                                                *00120000
00013 ******************************************************************00130000
00014 ******************************************************************00140000
00015 ******************************************************************00150000
00016 *                                                                *00160000
00017 *       ***-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *00170000
00018 *       *-*         U P D A T E   H I S T O R Y         *-*      *00180000
00019 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *00190000
00020 *                                                                *00200000
00021 *                                                                *00210000
00022 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION--------------- *00220000
00023 *                                                                *00230000
00024 *   D1009    12/10/87  FRY    CREATED THIS PROGRAM......         *00240000
00025 *                                                                *00250000
00026 *   11154     3/06/91  FRY    INCREASE RECORD AREAS IN FILE      *00260000
00027 *                             SECTION:                           *00270000
00028 *             INPUT-WRK-ENTRIES   PIC X(3960)  CHANGED TO  7765  *00280000
00029 *             OUTPUT-WRK-RECORD   PIC X(4064)  CHANGED TO  7869  *00290000
00030 *                                                                *00300000
00031 *    1293    02/03/93  JGR    REPLACE CURRENT PROCESSING WITH    *00310000
00032 *                             MATCHES AGAINST EXISTING TABULARS  *00320000
00033 *                             USING THE TABULAR HASHING PROGRAM. *00330000
00034 *                                                                *00340000
00035 *             1/10/95  EMS    CONVERTED TO COBOL II.             *00350000
00036 *                                                                *00360000
00037 *  14726/    11/14/97  DAU    ADDED CODE TO SUPPORT THE YEAR 2000*00370000
00038 *  15057                      AND THE EXPANSION OF THE GROUP     *00380000
00039 *                             SPECIFIC AND CONTRACT KEY TO       *00390000
00040 *                             SUPPORT THE TEXAS MERGER.          *00400000
00041 *                                                                *00410000
00042 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *00420000
00043 *                                                                *00430000
00044 * D-356A   5/08/03  GTF  RECOMPILE FOR COPYBK CHANGES #ACON,     *00440000
00045 *                        #ACOS, #ADIP, #ADOP.                    *00450000
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION          00451001
00046 ******************************************************************00460000
00047 ******************************************************************00470000
00048  ENVIRONMENT DIVISION.                                            00480000
00049                                                                   00490000
00050  CONFIGURATION SECTION.                                           00500000
00051  SOURCE-COMPUTER.  IBM-370.                                       00510000
00052  OBJECT-COMPUTER.  IBM-370.                                       00520000
00053                                                                   00530000
00054  INPUT-OUTPUT SECTION.                                            00540000
00055                                                                   00550000
00056  FILE-CONTROL.                                                    00560000
00057      SELECT   INPUT-WRK-FILE     ASSIGN  TO   UT-S-GC0068A.       00570000
00058      SELECT   OUTPUT-WRK-FILE    ASSIGN  TO   UT-S-GC0068B.       00580000
00059                                                                   00590000
00060                                                                   00600000
00061  DATA DIVISION.                                                   00610000
00062  FILE SECTION.                                                    00620000
00063                                                                   00630000
00064  FD  INPUT-WRK-FILE                                               00640000
00065      LABEL RECORDS ARE STANDARD                                   00650000
00066      RECORDING MODE IS V                                          00660000
00067      BLOCK CONTAINS  0  RECORDS.                                  00670000
00068                                                                   00680000
00069  01  INPUT-WRK-RECORD.                                            00690000
00070 *    05  INPUT-WRK-KEY                     PIC X(100).            00700000
00071          COPY GCWRKDCC.                                           00710000
00072      05  INPUT-WRK-TABULAR-RECORD.                                00720000
00073          10  INPUT-WRK-TAB-REC-KEY.                               00730000
00074              15  INPUT-WRK-TAB-REC-ID      PIC X(6).              00740000
00075              15  INPUT-WRK-TAB-REC-SLOT    PIC S9(7) COMP-3.      00750000
00076          10  FILLER                        PIC X(27).             00760000
00077          10  INPUT-WRK-COMPARE.                                   00770000
00078              15  INPUT-WRK-TAB-REC-CNT     PIC S9(5) COMP-3.      00780000
      *DM9441 09/04/09 JJS  CHANGED        FOR FILE CONVERSION          00781001
00079              15  INPUT-WRK-ENTRIES         PIC X(31330).          00790001
00080 /                                                                 00800000
00081                                                                   00810000
00082  01  INPUT-AAR-RECORD.                                            00820000
00083      05  FILLER                            PIC X(100).            00830000
00084 /                                                                 00840000
00085      COPY GCTAAR3.                                                00850000
00086 /                                                                 00860000
00087  01  INPUT-ACON-RECORD.                                           00870000
00088      05  FILLER                            PIC X(100).            00880000
00089 /                                                                 00890000
00090      COPY GCTACON3.                                               00900000
00091 /                                                                 00910000
00092  01  INPUT-ACOS-RECORD.                                           00920000
00093      05  FILLER                            PIC X(100).            00930000
00094 /                                                                 00940000
00095      COPY GCTACOS3.                                               00950000
00096 /                                                                 00960000
00097  01  INPUT-ADIP-RECORD.                                           00970000
00098      05  FILLER                            PIC X(100).            00980000
00099 /                                                                 00990000
00100      COPY GCTADIP3.                                               01000000
00101 /                                                                 01010000
00102  01  INPUT-ADOP-RECORD.                                           01020000
00103      05  FILLER                            PIC X(100).            01030000
00104      COPY GCTADOP3.                                               01040000
00105 /                                                                 01050000
00106                                                                   01060000
00107                                                                   01070000
00108                                                                   01080000
00109  FD  OUTPUT-WRK-FILE                                              01090000
00110      LABEL RECORDS ARE STANDARD                                   01100000
00111      RECORDING MODE IS V                                          01110000
00112      BLOCK CONTAINS  0  RECORDS.                                  01120000
00113                                                                   01130000
00114                                                                   01140000
      *DM9441 09/04/09 JJS  CHANGED        FOR FILE CONVERSION          01141002
00115  01  OUTPUT-WRK-RECORD                PIC X(31470).               01150001
00116 /                                                                 01160000
00117                                                                   01170000
00118  01  OUTPUT-AAR-RECORD.                                           01180000
00119      05 FILLER                        PIC X(100).                 01190000
00120 /                                                                 01200000
00121      COPY  GCTAARC.                                               01210000
00122 /                                                                 01220000
00123  01  OUTPUT-ACON-RECORD.                                          01230000
00124      05 FILLER                        PIC X(100).                 01240000
00125 /                                                                 01250000
00126      COPY  GCTACONC.                                              01260000
00127 /                                                                 01270000
00128  01  OUTPUT-ACOS-RECORD.                                          01280000
00129      05 FILLER                        PIC X(100).                 01290000
00130 /                                                                 01300000
00131      COPY  GCTACOSC.                                              01310000
00132 /                                                                 01320000
00133  01  OUTPUT-ADIP-RECORD.                                          01330000
00134      05 FILLER                        PIC X(100).                 01340000
00135 /                                                                 01350000
00136      COPY  GCTADIPC.                                              01360000
00137 /                                                                 01370000
00138  01  OUTPUT-ADOP-RECORD.                                          01380000
00139      05 FILLER                        PIC X(100).                 01390000
00140 /                                                                 01400000
00141      COPY  GCTADOPC.                                              01410000
00142 /                                                                 01420000
00143                                                                   01430000
00144  WORKING-STORAGE SECTION.                                         01440000
00145                                                                   01450000
00146  01  WS-PROGRAM-ID                 PIC  X(26)  VALUE              01460000
00147                                      '* GC0068 WORKING STORAGE *'.01470000
00148                                                                   01480000
00149  01  WORK-AREAS.                                                  01490000
00150      05  HASH-PROGRAM            PIC X(17) VALUE 'GHS1BAT'.       01500000
00151                                                                   01510000
00152 **** GHS1BAT LINKAGE AREA                                         01520000
00153                                                                   01530000
00154  01  WS-GHS1BAT-CALL-AREA.                                        01540000
00155    03  WS-GHS1BAT-PROCESS-IND    PIC X.                           01550000
00156        88  CALL-FOR-OPEN                     VALUE 'O'.           01560000
00157        88  CALL-FOR-CLOSE                    VALUE 'C'.           01570000
00158        88  CALL-FOR-PROCESS                  VALUE 'P'.           01580000
00159                                                                   01590000
00160  01  ACCUM-SLOT-AREA.                                             01600000
00161      05  HASH-RETURN-CODE        PIC X(02).                       01610000
00162      05  ASUR-REC-AREA.                                           01620000
00163          10  ASUR-TAB-ID         PIC X(0006).                     01630000
00164          10  ASUR-SLOT           PIC S9(7)  COMP-3.               01640000
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION          01641003
00165          10  FILLER              PIC X(31360).                    01650001
00166                                                                   01660000
00167 ****                                                              01670000
00168                                                                   01680000
00169  01  WS-AAR-RECORD.                                               01690000
00170      05  FILLER                            PIC X(100).            01700000
00171 /                                                                 01710000
00172      COPY GCTAAR2.                                                01720000
00173 /                                                                 01730000
00174  01  WS-ACON-RECORD.                                              01740000
00175      05  FILLER                            PIC X(100).            01750000
00176 /                                                                 01760000
00177      COPY GCTACON2.                                               01770000
00178 /                                                                 01780000
00179  01  WS-ACOS-RECORD.                                              01790000
00180      05  FILLER                            PIC X(100).            01800000
00181 /                                                                 01810000
00182      COPY GCTACOS2.                                               01820000
00183 /                                                                 01830000
00184  01  WS-ADIP-RECORD.                                              01840000
00185      05  FILLER                            PIC X(100).            01850000
00186 /                                                                 01860000
00187      COPY GCTADIP2.                                               01870000
00188 /                                                                 01880000
00189  01  WS-ADOP-RECORD.                                              01890000
00190      05  FILLER                            PIC X(100).            01900000
00191 /                                                                 01910000
00192      COPY GCTADOP2.                                               01920000
00193 /                                                                 01930000
00194                                                                   01940000
00195  01  WS-SWITCHES.                                                 01950000
00196      05  FILLER                    PIC  X(16)  VALUE              01960000
00197                                    '*** SWITCHES ***'.            01970000
00198      05  WS-END-OF-FILE-SW         PIC  X(01)  VALUE '0'.         01980000
00199          88  WS-END-OF-FILE-ON                 VALUE '1'.         01990000
00200                                                                   02000000
00201  01  WS-WORK-AREA.                                                02010000
00202      05  FILLER                    PIC  X(17)  VALUE              02020000
00203                                    '*** WORK AREA ***'.           02030000
00204      05  WS-HOLD-LAST-SLOT         PIC S9(07)  VALUE +0    COMP-3.02040000
00205 /                                                                 02050000
00206  PROCEDURE DIVISION.                                              02060000
00207                                                                   02070000
00208 ******************************************************************02080000
00209 **                                                                02090000
00210 **               P R O C E S S    C O N T R O L                   02100000
00211 **                                                                02110000
00212 ******************************************************************02120000
00213  0000-MAINLINE.                                                   02130000
00214                                                                   02140000
00215      PERFORM 1000-OPEN-THE-FILES  THRU  1000-EXIT.                02150000
00216                                                                   02160000
00217      IF WS-END-OF-FILE-ON                                         02170000
00218          DISPLAY 'GC0068--NO INPUT RECORDS RECEIVED'              02180000
00219      ELSE                                                         02190000
00220          PERFORM 3000-PROCESS-ALL-INPUT-RECORDS THRU 3000-EXIT    02200000
00221             UNTIL  WS-END-OF-FILE-ON.                             02210000
00222                                                                   02220000
00223      PERFORM 9000-CLOSE-THE-FILES  THRU  9000-EXIT.               02230000
00224                                                                   02240000
00225      STOP RUN.                                                    02250000
00226                                                                   02260000
00227  0000-EXIT.                                                       02270000
00228      EXIT.                                                        02280000
00229 /                                                                 02290000
00230 ******************************************************************02300000
00231 ***                                                               02310000
00232 **       OPEN SEQUENTIAL FILES AND READ THE FIRST RECORD          02320000
00233 ***                                                               02330000
00234 ******************************************************************02340000
00235  1000-OPEN-THE-FILES.                                             02350000
00236                                                                   02360000
00237      OPEN INPUT    INPUT-WRK-FILE,                                02370000
00238           OUTPUT   OUTPUT-WRK-FILE.                               02380000
00239                                                                   02390000
00240      MOVE 'O'      TO WS-GHS1BAT-PROCESS-IND.                     02400000
00241      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA.                02410000
00242                                                                   02420000
00243 **--- READ THE FIRST RECORD.                                      02430000
00244 **                                                                02440000
00245      PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT.                 02450000
00246                                                                   02460000
00247  1000-EXIT.                                                       02470000
00248      EXIT.                                                        02480000
00249 /                                                                 02490000
00250 ******************************************************************02500000
00251 **            SEQUENTIAL ALL LEVEL TABULAR FILE                   02510000
00252 ******************************************************************02520000
00253  2000-READ-INPUT-FILE.                                            02530000
00254                                                                   02540000
00255      READ INPUT-WRK-FILE                                          02550000
00256          AT END                                                   02560000
00257              MOVE  '1'   TO   WS-END-OF-FILE-SW.                  02570000
00258                                                                   02580000
00259  2000-EXIT.                                                       02590000
00260      EXIT.                                                        02600000
00261 /                                                                 02610000
00262 ******************************************************************02620000
00263 ******************************************************************02630000
00264  3000-PROCESS-ALL-INPUT-RECORDS.                                  02640000
00265                                                                   02650000
00266                                                                   02660000
00267      IF INPUT-WRK-RECORD    EQUAL   LOW-VALUES                    02670000
00268          GO TO 3000-EXIT.                                         02680000
00269                                                                   02690000
00270                                                                   02700000
00271      IF INPUT-WRK-TAB-REC-ID    EQUAL   '#AAR '                   02710000
00272          PERFORM 3100-PROCESS-AAR-TABULAR THRU 3100-EXIT          02720000
00273          PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              02730000
00274          GO TO 3000-EXIT.                                         02740000
00275                                                                   02750000
00276                                                                   02760000
00277      IF INPUT-WRK-TAB-REC-ID    EQUAL   '#ACON '                  02770000
00278          PERFORM 3200-PROCESS-ACON-TABULAR THRU 3200-EXIT         02780000
00279          PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              02790000
00280          GO TO 3000-EXIT.                                         02800000
00281                                                                   02810000
00282                                                                   02820000
00283      IF INPUT-WRK-TAB-REC-ID    EQUAL   '#ACOS '                  02830000
00284          PERFORM 3300-PROCESS-ACOS-TABULAR THRU 3300-EXIT         02840000
00285          PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              02850000
00286          GO TO 3000-EXIT.                                         02860000
00287                                                                   02870000
00288                                                                   02880000
00289      IF INPUT-WRK-TAB-REC-ID    EQUAL   '#ADIP '                  02890000
00290          PERFORM 3400-PROCESS-ADIP-TABULAR THRU 3400-EXIT         02900000
00291          PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              02910000
00292          GO TO 3000-EXIT.                                         02920000
00293                                                                   02930000
00294                                                                   02940000
00295      IF INPUT-WRK-TAB-REC-ID    EQUAL   '#ADOP '                  02950000
00296          PERFORM 3500-PROCESS-ADOP-TABULAR THRU 3500-EXIT         02960000
00297          PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              02970000
00298          GO TO 3000-EXIT.                                         02980000
00299                                                                   02990000
00300      PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT.                 03000000
00301                                                                   03010000
00302  3000-EXIT.                                                       03020000
00303      EXIT.                                                        03030000
00304 /                                                                 03040000
00305 ******************************************************************03050000
00306 ****               ALL LEVEL AAR TABULAR                          03060000
00307 ******************************************************************03070000
00308  3100-PROCESS-AAR-TABULAR.                                        03080000
00309                                                                   03090000
00310 ***************************************************************   03100000
00311 ***** IF SLOT IS LESS THAN 9,000,000                      *****   03110000
00312 ***** BYPASS THIS RECORD.                                 *****   03120000
00313 ***************************************************************   03130000
00314                                                                   03140000
00315      IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                03150000
00316          GO TO 3100-EXIT.                                         03160000
00317                                                                   03170000
00318 ***************************************************************   03180000
00319 ***** CALL TABULAR HASHING PROGRAM                        *****   03190000
00320 ***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   03200000
00321 ***************************************************************   03210000
00322                                                                   03220000
00323      MOVE LOW-VALUES TO ASUR-REC-AREA.                            03230000
00324      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              03240000
00325      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          03250000
00326      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 03260000
00327                              ACCUM-SLOT-AREA.                     03270000
00328                                                                   03280000
00329 ***************************************************************   03290000
00330 ***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   03300000
00331 ***************************************************************   03310000
00332                                                                   03320000
00333      IF HASH-RETURN-CODE = '00'                                   03330000
00334         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  03340000
00335      ELSE                                                         03350000
00336         IF HASH-RETURN-CODE = '01'                                03360000
00337            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  03370000
00338         ELSE                                                      03380000
00339            GO TO 3100-EXIT.                                       03390000
00340                                                                   03400000
00341                                                                   03410000
00342 ***************************************************************   03420000
00343 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   03430000
00344 ***** THE SEQUENTIAL FILE.                                *****   03440000
00345 ***************************************************************   03450000
00346                                                                   03460000
00347      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         03470000
00348      MOVE INPUT-WRK-TAB-REC-CNT     TO GAE-ENTRY-COUNT.           03480000
00349      MOVE INPUT-AAR-RECORD          TO OUTPUT-WRK-RECORD.         03490000
00350      MOVE ASUR-SLOT                 TO GAE-PROVISION-SLOT-NO.     03500000
00351      WRITE OUTPUT-AAR-RECORD.                                     03510000
00352                                                                   03520000
00353  3100-EXIT.                                                       03530000
00354      EXIT.                                                        03540000
00355 /                                                                 03550000
00356 ******************************************************************03560000
00357 ****               ALL LEVEL ACON TABULAR                         03570000
00358 ******************************************************************03580000
00359  3200-PROCESS-ACON-TABULAR.                                       03590000
00360                                                                   03600000
00361 ***************************************************************   03610000
00362 ***** IF SLOT IS LESS THAN 9,000,000                      *****   03620000
00363 ***** BYPASS THIS RECORD.                                 *****   03630000
00364 ***************************************************************   03640000
00365                                                                   03650000
00366      IF INPUT-WRK-TAB-REC-SLOT    NOT  >     +8999999             03660000
00367          GO TO 3200-EXIT.                                         03670000
00368                                                                   03680000
00369 ***************************************************************   03690000
00370 ***** CALL TABULAR HASHING PROGRAM                        *****   03700000
00371 ***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   03710000
00372 ***************************************************************   03720000
00373                                                                   03730000
00374      MOVE LOW-VALUES TO ASUR-REC-AREA.                            03740000
00375      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              03750000
00376      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          03760000
00377      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 03770000
00378                              ACCUM-SLOT-AREA.                     03780000
00379                                                                   03790000
00380 ***************************************************************   03800000
00381 ***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   03810000
00382 ***************************************************************   03820000
00383                                                                   03830000
00384      IF HASH-RETURN-CODE = '00'                                   03840000
00385         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  03850000
00386      ELSE                                                         03860000
00387         IF HASH-RETURN-CODE = '01'                                03870000
00388            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  03880000
00389         ELSE                                                      03890000
00390            GO TO 3200-EXIT.                                       03900000
00391                                                                   03910000
00392                                                                   03920000
00393 ***************************************************************   03930000
00394 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   03940000
00395 ***** THE SEQUENTIAL FILE.                                *****   03950000
00396 ***************************************************************   03960000
00397                                                                   03970000
00398      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         03980000
00399      MOVE INPUT-WRK-TAB-REC-CNT     TO GAI-ENTRY-COUNT.           03990000
00400      MOVE INPUT-ACON-RECORD         TO OUTPUT-WRK-RECORD.         04000000
00401      MOVE ASUR-SLOT                 TO GAI-PROVISION-SLOT-NO.     04010000
00402      WRITE OUTPUT-ACON-RECORD.                                    04020000
00403                                                                   04030000
00404  3200-EXIT.                                                       04040000
00405      EXIT.                                                        04050000
00406 /                                                                 04060000
00407 ******************************************************************04070000
00408 ****               ALL LEVEL ACOS TABULAR                         04080000
00409 ******************************************************************04090000
00410  3300-PROCESS-ACOS-TABULAR.                                       04100000
00411                                                                   04110000
00412 ***************************************************************   04120000
00413 ***** IF SLOT IS LESS THAN 9,000,000                      *****   04130000
00414 ***** BYPASS THIS RECORD.                                 *****   04140000
00415 ***************************************************************   04150000
00416                                                                   04160000
00417      IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                04170000
00418          GO TO 3300-EXIT.                                         04180000
00419                                                                   04190000
00420 ***************************************************************   04200000
00421 ***** CALL TABULAR HASHING PROGRAM                        *****   04210000
00422 ***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   04220000
00423 ***************************************************************   04230000
00424                                                                   04240000
00425      MOVE LOW-VALUES TO ASUR-REC-AREA.                            04250000
00426      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              04260000
00427      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          04270000
00428      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 04280000
00429                              ACCUM-SLOT-AREA.                     04290000
00430                                                                   04300000
00431 ***************************************************************   04310000
00432 ***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   04320000
00433 ***************************************************************   04330000
00434                                                                   04340000
00435      IF HASH-RETURN-CODE = '00'                                   04350000
00436         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  04360000
00437      ELSE                                                         04370000
00438         IF HASH-RETURN-CODE = '01'                                04380000
00439            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  04390000
00440         ELSE                                                      04400000
00441            GO TO 3300-EXIT.                                       04410000
00442                                                                   04420000
00443                                                                   04430000
00444 ***************************************************************   04440000
00445 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   04450000
00446 ***** THE SEQUENTIAL FILE.                                *****   04460000
00447 ***************************************************************   04470000
00448                                                                   04480000
00449      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         04490000
00450      MOVE INPUT-WRK-TAB-REC-CNT     TO GAJ-ENTRY-COUNT.           04500000
00451      MOVE INPUT-ACOS-RECORD         TO OUTPUT-WRK-RECORD.         04510000
00452      MOVE ASUR-SLOT                 TO GAJ-PROVISION-SLOT-NO.     04520000
00453      WRITE OUTPUT-ACOS-RECORD.                                    04530000
00454                                                                   04540000
00455  3300-EXIT.                                                       04550000
00456      EXIT.                                                        04560000
00457 /                                                                 04570000
00458 ******************************************************************04580000
00459 ****               ALL LEVEL ADIP TABULAR                         04590000
00460 ******************************************************************04600000
00461  3400-PROCESS-ADIP-TABULAR.                                       04610000
00462                                                                   04620000
00463 ***************************************************************   04630000
00464 ***** IF SLOT IS LESS THAN 9,000,000                      *****   04640000
00465 ***** BYPASS THIS RECORD.                                 *****   04650000
00466 ***************************************************************   04660000
00467                                                                   04670000
00468      IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                04680000
00469          GO TO 3400-EXIT.                                         04690000
00470                                                                   04700000
00471 ***************************************************************   04710000
00472 ***** CALL TABULAR HASHING PROGRAM                        *****   04720000
00473 ***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   04730000
00474 ***************************************************************   04740000
00475                                                                   04750000
00476      MOVE LOW-VALUES TO ASUR-REC-AREA.                            04760000
00477      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              04770000
00478      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          04780000
00479      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 04790000
00480                              ACCUM-SLOT-AREA.                     04800000
00481                                                                   04810000
00482 ***************************************************************   04820000
00483 ***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   04830000
00484 ***************************************************************   04840000
00485                                                                   04850000
00486      IF HASH-RETURN-CODE = '00'                                   04860000
00487         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  04870000
00488      ELSE                                                         04880000
00489         IF HASH-RETURN-CODE = '01'                                04890000
00490            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  04900000
00491         ELSE                                                      04910000
00492            GO TO 3400-EXIT.                                       04920000
00493                                                                   04930000
00494                                                                   04940000
00495 ***************************************************************   04950000
00496 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   04960000
00497 ***** THE SEQUENTIAL FILE.                                *****   04970000
00498 ***************************************************************   04980000
00499                                                                   04990000
00500      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         05000000
00501      MOVE INPUT-WRK-TAB-REC-CNT     TO GAG-ENTRY-COUNT.           05010000
00502      MOVE INPUT-ADIP-RECORD         TO OUTPUT-WRK-RECORD.         05020000
00503      MOVE ASUR-SLOT                 TO GAG-PROVISION-SLOT-NO.     05030000
00504      WRITE OUTPUT-ADIP-RECORD.                                    05040000
00505                                                                   05050000
00506  3400-EXIT.                                                       05060000
00507      EXIT.                                                        05070000
00508 /                                                                 05080000
00509 ******************************************************************05090000
00510 ****               ALL LEVEL ADOP TABULAR                         05100000
00511 ******************************************************************05110000
00512  3500-PROCESS-ADOP-TABULAR.                                       05120000
00513                                                                   05130000
00514 ***************************************************************   05140000
00515 ***** IF SLOT IS LESS THAN 9,000,000                      *****   05150000
00516 ***** BYPASS THIS RECORD.                                 *****   05160000
00517 ***************************************************************   05170000
00518                                                                   05180000
00519      IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                05190000
00520          GO TO 3500-EXIT.                                         05200000
00521                                                                   05210000
00522 ***************************************************************   05220000
00523 ***** CALL TABULAR HASHING PROGRAM                        *****   05230000
00524 ***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   05240000
00525 ***************************************************************   05250000
00526                                                                   05260000
00527      MOVE LOW-VALUES TO ASUR-REC-AREA.                            05270000
00528      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              05280000
00529      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          05290000
00530      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 05300000
00531                              ACCUM-SLOT-AREA.                     05310000
00532                                                                   05320000
00533 ***************************************************************   05330000
00534 ***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   05340000
00535 ***************************************************************   05350000
00536                                                                   05360000
00537      IF HASH-RETURN-CODE = '00'                                   05370000
00538         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  05380000
00539      ELSE                                                         05390000
00540         IF HASH-RETURN-CODE = '01'                                05400000
00541            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  05410000
00542         ELSE                                                      05420000
00543            GO TO 3500-EXIT.                                       05430000
00544                                                                   05440000
00545                                                                   05450000
00546 ***************************************************************   05460000
00547 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   05470000
00548 ***** THE SEQUENTIAL FILE.                                *****   05480000
00549 ***************************************************************   05490000
00550                                                                   05500000
00551      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         05510000
00552      MOVE INPUT-WRK-TAB-REC-CNT     TO GAH-ENTRY-COUNT.           05520000
00553      MOVE INPUT-ADOP-RECORD         TO OUTPUT-WRK-RECORD.         05530000
00554      MOVE ASUR-SLOT                 TO GAH-PROVISION-SLOT-NO.     05540000
00555      WRITE OUTPUT-ADOP-RECORD.                                    05550000
00556                                                                   05560000
00557  3500-EXIT.                                                       05570000
00558      EXIT.                                                        05580000
00559 /                                                                 05590000
00560 ******************************************************************05600000
00561 **                                                                05610000
00562 **                  CLOSE SEQUENTIAL FILES                        05620000
00563 **                                                                05630000
00564 ******************************************************************05640000
00565  9000-CLOSE-THE-FILES.                                            05650000
00566                                                                   05660000
00567      CLOSE INPUT-WRK-FILE,                                        05670000
00568            OUTPUT-WRK-FILE.                                       05680000
00569                                                                   05690000
00570      MOVE 'C'    TO WS-GHS1BAT-PROCESS-IND.                       05700000
00571      CALL HASH-PROGRAM  USING WS-GHS1BAT-CALL-AREA.               05710000
00572                                                                   05720000
00573  9000-EXIT.                                                       05730000
00574      EXIT.                                                        05740000
00575 /                                                                 05750000
