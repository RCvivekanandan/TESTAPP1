00001  IDENTIFICATION DIVISION.                                         00010000
00002  PROGRAM-ID.         GC0110.                                      00020000
00003  AUTHOR.             DELORES FRY.                                 00030000
00004  INSTALLATION.       HCSC.                                        00040000
00005  DATE-WRITTEN.       DECEMBER 1987.                               00050000
00006  DATE-COMPILED.                                                   00060000
00007 ******************************************************************00070000
00008 *                                                                *00080000
00009 *             BENEFIT PROVISION TABULAR RECORD                   *00090000
00010 *             MATCH AND SLOT UPDATE PROGRAM                      *00100000
00011 *                                                                *00110000
00012 ******************************************************************00120000
00013 ******************************************************************00130000
00014 ******************************************************************00140000
00015 *                                                                *00150000
00016 *       ***-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *00160000
00017 *       *-*         U P D A T E   H I S T O R Y         *-*      *00170000
00018 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *00180000
00019 *                                                                *00190000
00020 *                                                                *00200000
00021 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION--------------- *00210000
00022 *                                                                *00220000
00023 *            MM/DD/YY  XXX  1. XXXXXXXXXXX..............         *00230000
00024 *                                                                *00240000
00025 *   11154     3/06/91  FRY  INCREASE RECORD AREAS IN FILE        *00250000
00026 *                           SECTION:                             *00260000
00027 *             INPUT-WRK-ENTRIES   PIC X(3960)  CHANGED TO  7765  *00270000
00028 *             OUTPUT-WRK-RECORD   PIC X(4064)  CHANGED TO  7869  *00280000
00029 *                                                                *00290000
00030 *    1293    02/05/93  JGR  REPLACE CURRENT PROCESSING WITH      *00300000
00031 *                           MATCHES AGAINST EXISTING TABULARS    *00310000
00032 *                           USING THE TABULAR HASHING PROGRAM.   *00320000
00033 *                                                                *00330000
00034 *             1/17/95  EMS  CONVERTED TO COBOL II.               *00340000
00035 *             6/24/97  AB   CHANGED TO AVOID 0C4.                *00350000
00036 *                                                                *00360000
00037 * 14726/     11/11/97  GSP  MODIFIED TO BECOME MILLENNIUM        *00370000
00038 * 15057                     COMPLIANT. RECORD LENGTHS CHANGED.   *00380000
00039 *                                                                *00390000
00040 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *00400000
00041 *                                                                *00410000
00042 * D-356A   5/08/03  GTF  RECOMPILE FOR COPYBK CHANGES #PAQ, #PDR,*00420000
00043 *                        #PRR, #PRV.                             *00430000
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION          00431002
00044 ******************************************************************00440000
00045 ******************************************************************00450000
00046  ENVIRONMENT DIVISION.                                            00460000
00047                                                                   00470000
00048  CONFIGURATION SECTION.                                           00480000
00049  SOURCE-COMPUTER.  IBM-370.                                       00490000
00050  OBJECT-COMPUTER.  IBM-370.                                       00500000
00051                                                                   00510000
00052  INPUT-OUTPUT SECTION.                                            00520000
00053                                                                   00530000
00054  FILE-CONTROL.                                                    00540000
00055      SELECT   INPUT-WRK-FILE     ASSIGN  TO   UT-S-GC0110A.       00550000
00056      SELECT   OUTPUT-WRK-FILE    ASSIGN  TO   UT-S-GC0110B.       00560000
00057                                                                   00570000
00058                                                                   00580000
00059  DATA DIVISION.                                                   00590000
00060  FILE SECTION.                                                    00600000
00061                                                                   00610000
00062  FD  INPUT-WRK-FILE                                               00620000
00063      LABEL RECORDS ARE STANDARD                                   00630000
00064      RECORDING MODE IS V                                          00640000
00065      BLOCK CONTAINS  0  RECORDS.                                  00650000
00066                                                                   00660000
00067  01  INPUT-WRK-RECORD.                                            00670000
00068 *    05  INPUT-WRK-KEY                     PIC X(100).            00680000
00069          COPY GCWRKDCC.                                           00690000
00070      05  INPUT-WRK-TABULAR-RECORD.                                00700000
00071          10  INPUT-WRK-TAB-REC-KEY.                               00710000
00072              15  INPUT-WRK-TAB-REC-ID      PIC X(6).              00720000
00073              15  INPUT-WRK-TAB-REC-SLOT    PIC S9(7) COMP-3.      00730000
00074          10  FILLER                        PIC X(27).             00740000
00075          10  INPUT-WRK-COMPARE.                                   00750000
00076              15  INPUT-WRK-TAB-REC-CNT     PIC S9(5) COMP-3.      00760000
      *DM9441 09/04/09 JJS  CHANGED        FOR FILE CONVERSION          00761002
00077              15  INPUT-WRK-ENTRIES         PIC X(31330).          00770001
00078 /                                                                 00780000
00079                                                                   00790000
00080  01  INPUT-PAQ-RECORD.                                            00800000
00081      05  FILLER                            PIC X(100).            00810000
00082      COPY GCTPAQ3.                                                00820000
00083 /                                                                 00830000
00084  01  INPUT-PCX-RECORD.                                            00840000
00085      05  FILLER                            PIC X(100).            00850000
00086      COPY GCTPCX3.                                                00860000
00087 /                                                                 00870000
00088  01  INPUT-PDR-RECORD.                                            00880000
00089      05  FILLER                            PIC X(100).            00890000
00090      COPY GCTPDR3.                                                00900000
00091 /                                                                 00910000
00092  01  INPUT-PPF-RECORD.                                            00920000
00093      05  FILLER                            PIC X(100).            00930000
00094      COPY GCTPPF3.                                                00940000
00095 /                                                                 00950000
00096  01  INPUT-PRR-RECORD.                                            00960000
00097      05  FILLER                            PIC X(100).            00970000
00098      COPY GCTPRR3.                                                00980000
00099 /                                                                 00990000
00100                                                                   01000000
00101  01  INPUT-PRV-RECORD.                                            01010000
00102      05  FILLER                            PIC X(100).            01020000
00103      COPY GCTPRV3.                                                01030000
00104 /                                                                 01040000
00105                                                                   01050000
00106  01  INPUT-PSC-RECORD.                                            01060000
00107      05  FILLER                            PIC X(100).            01070000
00108      COPY GCTPSC3.                                                01080000
00109 /                                                                 01090000
00110                                                                   01100000
00111  01  INPUT-PVE-RECORD.                                            01110000
00112      05  FILLER                            PIC X(100).            01120000
00113      COPY GCTPVE3.                                                01130000
00114 /                                                                 01140000
00115                                                                   01150000
00116                                                                   01160000
00117  FD  OUTPUT-WRK-FILE                                              01170000
00118      LABEL RECORDS ARE STANDARD                                   01180000
00119      RECORDING MODE IS V                                          01190000
00120      BLOCK CONTAINS  0  RECORDS.                                  01200000
00121                                                                   01210000
00122                                                                   01220000
      *DM9441 09/04/09 JJS  CHANGED        FOR FILE CONVERSION          01221002
00123  01  OUTPUT-WRK-RECORD                PIC X(31470).               01230001
00124 /                                                                 01240000
00125                                                                   01250000
00126  01  OUTPUT-PAQ-RECORD.                                           01260000
00127      05 FILLER                        PIC X(100).                 01270000
00128      COPY  GCTPAQC.                                               01280000
00129 /                                                                 01290000
00130  01  OUTPUT-PCX-RECORD.                                           01300000
00131      05 FILLER                        PIC X(100).                 01310000
00132      COPY  GCTPCXC.                                               01320000
00133 /                                                                 01330000
00134  01  OUTPUT-PDR-RECORD.                                           01340000
00135      05 FILLER                        PIC X(100).                 01350000
00136      COPY  GCTPDRC.                                               01360000
00137 /                                                                 01370000
00138  01  OUTPUT-PPF-RECORD.                                           01380000
00139      05 FILLER                        PIC X(100).                 01390000
00140      COPY  GCTPPFC.                                               01400000
00141 /                                                                 01410000
00142  01  OUTPUT-PRR-RECORD.                                           01420000
00143      05 FILLER                        PIC X(100).                 01430000
00144      COPY  GCTPRRC.                                               01440000
00145 /                                                                 01450000
00146  01  OUTPUT-PRV-RECORD.                                           01460000
00147      05 FILLER                        PIC X(100).                 01470000
00148      COPY  GCTPRVC.                                               01480000
00149 /                                                                 01490000
00150  01  OUTPUT-PSC-RECORD.                                           01500000
00151      05 FILLER                        PIC X(100).                 01510000
00152      COPY  GCTPSCC.                                               01520000
00153 /                                                                 01530000
00154  01  OUTPUT-PVE-RECORD.                                           01540000
00155      05 FILLER                        PIC X(100).                 01550000
00156      COPY  GCTPVEC.                                               01560000
00157 /                                                                 01570000
00158                                                                   01580000
00159  WORKING-STORAGE SECTION.                                         01590000
00160                                                                   01600000
00161  01  WS-PROGRAM-ID                 PIC  X(27)  VALUE              01610000
00162                                     '* GC0110 WORKING STORAGE *'. 01620000
00163                                                                   01630000
00164  01  WORK-AREAS.                                                  01640000
00165      05  HASH-PROGRAM              PIC X(17) VALUE 'GHS1BAT'.     01650000
00166                                                                   01660000
00167 **** GHS1BAT LINKAGE AREA                                         01670000
00168                                                                   01680000
00169  01  WS-GHS1BAT-CALL-AREA.                                        01690000
00170    03  WS-GHS1BAT-PROCESS-IND      PIC X.                         01700000
00171        88  CALL-FOR-OPEN                     VALUE 'O'.           01710000
00172        88  CALL-FOR-CLOSE                    VALUE 'C'.           01720000
00173        88  CALL-FOR-PROCESS                  VALUE 'P'.           01730000
00174                                                                   01740000
00175  01  ACCUM-SLOT-AREA.                                             01750000
00176      05  HASH-RETURN-CODE          PIC X(02).                     01760000
00177      05  ASUR-REC-AREA.                                           01770000
00178          10  ASUR-TAB-ID           PIC X(0006).                   01780000
00179          10  ASUR-SLOT             PIC S9(7) COMP-3.              01790000
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION          01791002
00180          10  FILLER                PIC X(31360).                  01800001
00181                                                                   01810000
00182 ****                                                              01820000
00183                                                                   01830000
00184  01  ACCUM-PAQ-SLOT-AREA.                                         01840000
00185      05  PAQ-HASH-RETURN-CODE           PIC X(02).                01850000
00186      COPY GCTPAQ2.                                                01860000
00187 /                                                                 01870000
00188  01  ACCUM-PCX-SLOT-AREA.                                         01880000
00189      05  PCX-HASH-RETURN-CODE           PIC X(02).                01890000
00190      COPY GCTPCX2.                                                01900000
00191 /                                                                 01910000
00192  01  ACCUM-PDR-SLOT-AREA.                                         01920000
00193      05  PDR-HASH-RETURN-CODE           PIC X(02).                01930000
00194      COPY GCTPDR2.                                                01940000
00195 /                                                                 01950000
00196  01  ACCUM-PPF-SLOT-AREA.                                         01960000
00197      05  PPF-HASH-RETURN-CODE           PIC X(02).                01970000
00198      COPY GCTPPF2.                                                01980000
00199 /                                                                 01990000
00200  01  ACCUM-PRR-SLOT-AREA.                                         02000000
00201      05  PRR-HASH-RETURN-CODE              PIC X(02).             02010000
00202      COPY GCTPRR2.                                                02020000
00203 /                                                                 02030000
00204  01  ACCUM-PRV-SLOT-AREA.                                         02040000
00205      05  PRV-HASH-RETURN-CODE              PIC X(02).             02050000
00206      COPY GCTPRV2.                                                02060000
00207 /                                                                 02070000
00208  01  ACCUM-PSC-SLOT-AREA.                                         02080000
00209      05  PSC-HASH-RETURN-CODE              PIC X(02).             02090000
00210      COPY GCTPSC2.                                                02100000
00211 /                                                                 02110000
00212  01  ACCUM-PVE-SLOT-AREA.                                         02120000
00213      05  PVE-HASH-RETURN-CODE              PIC X(02).             02130000
00214      COPY GCTPVE2.                                                02140000
00215 /                                                                 02150000
00216                                                                   02160000
00217  01  WS-SWITCHES.                                                 02170000
00218      05  FILLER                    PIC  X(16)  VALUE              02180000
00219                                    '*** SWITCHES ***'.            02190000
00220      05  WS-END-OF-FILE-SW         PIC  X(01)  VALUE '0'.         02200000
00221          88  WS-END-OF-FILE-ON                 VALUE '1'.         02210000
00222                                                                   02220000
00223  01  WS-WORK-AREA.                                                02230000
00224      05  FILLER                    PIC  X(17)  VALUE              02240000
00225                                    '*** WORK AREA ***'.           02250000
00226      05  WS-HOLD-LAST-SLOT         PIC S9(07)  VALUE +0    COMP-3.02260000
00227 /                                                                 02270000
00228  PROCEDURE DIVISION.                                              02280000
00229                                                                   02290000
00230 ******************************************************************02300000
00231 **                                                                02310000
00232 **               P R O C E S S    C O N T R O L                   02320000
00233 **                                                                02330000
00234 ******************************************************************02340000
00235  0000-MAINLINE.                                                   02350000
00236                                                                   02360000
00237      PERFORM 1000-OPEN-THE-FILES  THRU  1000-EXIT.                02370000
00238                                                                   02380000
00239      IF WS-END-OF-FILE-ON                                         02390000
00240          DISPLAY 'GC0110--NO INPUT RECORDS RECEIVED'              02400000
00241      ELSE                                                         02410000
00242         PERFORM 3000-PROCESS-ALL-INPUT-RECORDS THRU 3000-EXIT     02420000
00243            UNTIL  WS-END-OF-FILE-ON.                              02430000
00244                                                                   02440000
00245      PERFORM 9000-CLOSE-THE-FILES  THRU  9000-EXIT.               02450000
00246                                                                   02460000
00247      STOP RUN.                                                    02470000
00248                                                                   02480000
00249  0000-EXIT.                                                       02490000
00250      EXIT.                                                        02500000
00251 /                                                                 02510000
00252 ******************************************************************02520000
00253 ***                                                               02530000
00254 **       OPEN SEQUENTIAL FILES AND READ THE FIRST RECORD          02540000
00255 ***                                                               02550000
00256 ******************************************************************02560000
00257  1000-OPEN-THE-FILES.                                             02570000
00258                                                                   02580000
00259      OPEN INPUT    INPUT-WRK-FILE,                                02590000
00260           OUTPUT   OUTPUT-WRK-FILE.                               02600000
00261                                                                   02610000
00262      MOVE 'O'      TO WS-GHS1BAT-PROCESS-IND.                     02620000
00263      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA.                02630000
00264                                                                   02640000
00265 **--- READ THE FIRST RECORD.                                      02650000
00266 **                                                                02660000
00267      PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT.                 02670000
00268                                                                   02680000
00269  1000-EXIT.                                                       02690000
00270      EXIT.                                                        02700000
00271 /                                                                 02710000
00272 ******************************************************************02720000
00273 **         SEQUENTIAL BENEFIT PROVISION TABULAR FILE              02730000
00274 ******************************************************************02740000
00275  2000-READ-INPUT-FILE.                                            02750000
00276                                                                   02760000
00277      READ INPUT-WRK-FILE                                          02770000
00278          AT END                                                   02780000
00279              MOVE  '1'   TO   WS-END-OF-FILE-SW.                  02790000
00280                                                                   02800000
00281  2000-EXIT.                                                       02810000
00282      EXIT.                                                        02820000
00283 /                                                                 02830000
00284 ******************************************************************02840000
00285 ******************************************************************02850000
00286  3000-PROCESS-ALL-INPUT-RECORDS.                                  02860000
00287                                                                   02870000
00288                                                                   02880000
00289      IF INPUT-WRK-TAB-REC-ID    EQUAL   '#PAQ '                   02890000
00290          PERFORM 3100-PROCESS-PAQ-TABULAR THRU 3100-EXIT          02900000
00291          PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              02910000
00292          GO TO 3000-EXIT.                                         02920000
00293                                                                   02930000
00294      IF INPUT-WRK-TAB-REC-ID    EQUAL   '#PCX '                   02940000
00295          PERFORM 3200-PROCESS-PCX-TABULAR THRU 3200-EXIT          02950000
00296          PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              02960000
00297          GO TO 3000-EXIT.                                         02970000
00298                                                                   02980000
00299                                                                   02990000
00300      IF INPUT-WRK-TAB-REC-ID    EQUAL   '#PDR '                   03000000
00301          PERFORM 3300-PROCESS-PDR-TABULAR THRU 3300-EXIT          03010000
00302          PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              03020000
00303          GO TO 3000-EXIT.                                         03030000
00304                                                                   03040000
00305                                                                   03050000
00306      IF INPUT-WRK-TAB-REC-ID    EQUAL   '#PPF '                   03060000
00307          PERFORM 3400-PROCESS-PPF-TABULAR THRU 3400-EXIT          03070000
00308          PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              03080000
00309          GO TO 3000-EXIT.                                         03090000
00310                                                                   03100000
00311                                                                   03110000
00312      IF INPUT-WRK-TAB-REC-ID    EQUAL   '#PRR '                   03120000
00313          PERFORM 3500-PROCESS-PRR-TABULAR THRU 3500-EXIT          03130000
00314          PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              03140000
00315          GO TO 3000-EXIT.                                         03150000
00316                                                                   03160000
00317                                                                   03170000
00318      IF INPUT-WRK-TAB-REC-ID    EQUAL   '#PRV '                   03180000
00319          PERFORM 3600-PROCESS-PRV-TABULAR THRU 3600-EXIT          03190000
00320          PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              03200000
00321          GO TO 3000-EXIT.                                         03210000
00322                                                                   03220000
00323                                                                   03230000
00324      IF INPUT-WRK-TAB-REC-ID    EQUAL   '#PSC '                   03240000
00325          PERFORM 3700-PROCESS-PSC-TABULAR THRU 3700-EXIT          03250000
00326          PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              03260000
00327          GO TO 3000-EXIT.                                         03270000
00328                                                                   03280000
00329                                                                   03290000
00330      IF INPUT-WRK-TAB-REC-ID    EQUAL   '#PVE '                   03300000
00331          PERFORM 3800-PROCESS-PVE-TABULAR THRU 3800-EXIT          03310000
00332          PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT              03320000
00333          GO TO 3000-EXIT.                                         03330000
00334                                                                   03340000
00335                                                                   03350000
00336      PERFORM 2000-READ-INPUT-FILE THRU 2000-EXIT.                 03360000
00337                                                                   03370000
00338  3000-EXIT.                                                       03380000
00339      EXIT.                                                        03390000
00340 /                                                                 03400000
00341 ******************************************************************03410000
00342 ****         BENEFIT PROVISION 'PAQ' TABULAR                      03420000
00343 ******************************************************************03430000
00344  3100-PROCESS-PAQ-TABULAR.                                        03440000
00345                                                                   03450000
00346 ***************************************************************   03460000
00347 ***** IF SLOT IS LESS THAN 9,000,000                      *****   03470000
00348 ***** BYPASS THIS RECORD.                                 *****   03480000
00349 ***************************************************************   03490000
00350                                                                   03500000
00351      IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                03510000
00352          GO TO 3100-EXIT.                                         03520000
00353                                                                   03530000
00354 ***************************************************************   03540000
00355 ***** CALL TABULAR HASHING PROGRAM                        *****   03550000
00356 ***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   03560000
00357 ***************************************************************   03570000
00358                                                                   03580000
00359      INITIALIZE ACCUM-SLOT-AREA.                                  03590000
00360      MOVE GBA3-RECORD              TO ASUR-REC-AREA.              03600000
00361      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          03610000
00362      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 03620000
00363                              ACCUM-SLOT-AREA.                     03630000
00364                                                                   03640000
00365 ***************************************************************   03650000
00366 ***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   03660000
00367 ***************************************************************   03670000
00368                                                                   03680000
00369      IF     HASH-RETURN-CODE = '00'                               03690000
00370         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  03700000
00371      ELSE                                                         03710000
00372         IF     HASH-RETURN-CODE = '01'                            03720000
00373            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  03730000
00374         ELSE                                                      03740000
00375            GO TO 3100-EXIT.                                       03750000
00376                                                                   03760000
00377                                                                   03770000
00378 ***************************************************************   03780000
00379 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   03790000
00380 ***** THE SEQUENTIAL FILE.                                *****   03800000
00381 ***************************************************************   03810000
00382                                                                   03820000
00383      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         03830000
00384      MOVE INPUT-WRK-TAB-REC-CNT     TO GBA-ENTRY-COUNT.           03840000
00385      MOVE INPUT-PAQ-RECORD          TO OUTPUT-WRK-RECORD.         03850000
00386      MOVE ASUR-SLOT               TO GBA-PROVISION-SLOT-NO.       03860000
00387      WRITE OUTPUT-PAQ-RECORD.                                     03870000
00388                                                                   03880000
00389  3100-EXIT.                                                       03890000
00390      EXIT.                                                        03900000
00391 /                                                                 03910000
00392 ******************************************************************03920000
00393 ****         BENEFIT PROVISION 'PCX' TABULAR                      03930000
00394 ******************************************************************03940000
00395  3200-PROCESS-PCX-TABULAR.                                        03950000
00396                                                                   03960000
00397 ***************************************************************   03970000
00398 ***** IF SLOT IS LESS THAN 9,000,000                      *****   03980000
00399 ***** BYPASS THIS RECORD.                                 *****   03990000
00400 ***************************************************************   04000000
00401                                                                   04010000
00402      IF INPUT-WRK-TAB-REC-SLOT    NOT  >     +8999999             04020000
00403          GO TO 3200-EXIT.                                         04030000
00404                                                                   04040000
00405 ***************************************************************   04050000
00406 ***** CALL TABULAR HASHING PROGRAM                        *****   04060000
00407 ***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   04070000
00408 ***************************************************************   04080000
00409                                                                   04090000
00410      INITIALIZE ACCUM-SLOT-AREA.                                  04100000
00411      MOVE GBD3-RECORD              TO ASUR-REC-AREA.              04110000
00412      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          04120000
00413      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 04130000
00414                              ACCUM-SLOT-AREA.                     04140000
00415                                                                   04150000
00416 ***************************************************************   04160000
00417 ***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   04170000
00418 ***************************************************************   04180000
00419                                                                   04190000
00420      IF     HASH-RETURN-CODE = '00'                               04200000
00421         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  04210000
00422      ELSE                                                         04220000
00423         IF     HASH-RETURN-CODE = '01'                            04230000
00424            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  04240000
00425         ELSE                                                      04250000
00426            GO TO 3200-EXIT.                                       04260000
00427                                                                   04270000
00428                                                                   04280000
00429 ***************************************************************   04290000
00430 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   04300000
00431 ***** THE SEQUENTIAL FILE.                                *****   04310000
00432 ***************************************************************   04320000
00433                                                                   04330000
00434      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         04340000
00435      MOVE INPUT-WRK-TAB-REC-CNT     TO GBD-ENTRY-COUNT.           04350000
00436      MOVE INPUT-PCX-RECORD          TO OUTPUT-WRK-RECORD.         04360000
00437      MOVE ASUR-SLOT                 TO GBD-PROVISION-SLOT-NO.     04370000
00438      WRITE OUTPUT-PCX-RECORD.                                     04380000
00439                                                                   04390000
00440  3200-EXIT.                                                       04400000
00441      EXIT.                                                        04410000
00442 /                                                                 04420000
00443 ******************************************************************04430000
00444 ****        BENEFIT PROVISION 'PDR' TABULAR                       04440000
00445 ******************************************************************04450000
00446  3300-PROCESS-PDR-TABULAR.                                        04460000
00447                                                                   04470000
00448 ***************************************************************   04480000
00449 ***** IF SLOT IS LESS THAN 9,000,000                      *****   04490000
00450 ***** BYPASS THIS RECORD.                                 *****   04500000
00451 ***************************************************************   04510000
00452                                                                   04520000
00453      IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                04530000
00454          GO TO 3300-EXIT.                                         04540000
00455                                                                   04550000
00456 ***************************************************************   04560000
00457 ***** CALL TABULAR HASHING PROGRAM                        *****   04570000
00458 ***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   04580000
00459 ***************************************************************   04590000
00460                                                                   04600000
00461      INITIALIZE ACCUM-SLOT-AREA.                                  04610000
00462      MOVE GBG3-RECORD              TO ASUR-REC-AREA.              04620000
00463      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          04630000
00464      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 04640000
00465                              ACCUM-SLOT-AREA.                     04650000
00466                                                                   04660000
00467 ***************************************************************   04670000
00468 ***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   04680000
00469 ***************************************************************   04690000
00470                                                                   04700000
00471      IF     HASH-RETURN-CODE = '00'                               04710000
00472         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  04720000
00473      ELSE                                                         04730000
00474         IF     HASH-RETURN-CODE = '01'                            04740000
00475            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  04750000
00476         ELSE                                                      04760000
00477            GO TO 3300-EXIT.                                       04770000
00478                                                                   04780000
00479                                                                   04790000
00480 ***************************************************************   04800000
00481 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   04810000
00482 ***** THE SEQUENTIAL FILE.                                *****   04820000
00483 ***************************************************************   04830000
00484                                                                   04840000
00485      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         04850000
00486      MOVE INPUT-WRK-TAB-REC-CNT     TO GBG-ENTRY-COUNT.           04860000
00487      MOVE INPUT-PDR-RECORD          TO OUTPUT-WRK-RECORD.         04870000
00488      MOVE ASUR-SLOT                 TO GBG-PROVISION-SLOT-NO.     04880000
00489      WRITE OUTPUT-PDR-RECORD.                                     04890000
00490                                                                   04900000
00491  3300-EXIT.                                                       04910000
00492      EXIT.                                                        04920000
00493 /                                                                 04930000
00494 ******************************************************************04940000
00495 ****      BENEFIT PROVISION 'PPF' TABULAR                         04950000
00496 ******************************************************************04960000
00497  3400-PROCESS-PPF-TABULAR.                                        04970000
00498                                                                   04980000
00499 ***************************************************************   04990000
00500 ***** IF SLOT IS LESS THAN 9,000,000                      *****   05000000
00501 ***** BYPASS THIS RECORD.                                 *****   05010000
00502 ***************************************************************   05020000
00503                                                                   05030000
00504      IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                05040000
00505          GO TO 3400-EXIT.                                         05050000
00506                                                                   05060000
00507 ***************************************************************   05070000
00508 ***** CALL TABULAR HASHING PROGRAM                        *****   05080000
00509 ***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   05090000
00510 ***************************************************************   05100000
00511                                                                   05110000
00512      INITIALIZE ACCUM-SLOT-AREA.                                  05120000
00513      MOVE GBB3-RECORD              TO ASUR-REC-AREA.              05130000
00514      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          05140000
00515      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 05150000
00516                              ACCUM-SLOT-AREA.                     05160000
00517                                                                   05170000
00518 ***************************************************************   05180000
00519 ***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   05190000
00520 ***************************************************************   05200000
00521                                                                   05210000
00522      IF     HASH-RETURN-CODE = '00'                               05220000
00523         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  05230000
00524      ELSE                                                         05240000
00525         IF     HASH-RETURN-CODE = '01'                            05250000
00526            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  05260000
00527         ELSE                                                      05270000
00528            GO TO 3400-EXIT.                                       05280000
00529                                                                   05290000
00530                                                                   05300000
00531 ***************************************************************   05310000
00532 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   05320000
00533 ***** THE SEQUENTIAL FILE.                                *****   05330000
00534 ***************************************************************   05340000
00535                                                                   05350000
00536      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         05360000
00537      MOVE INPUT-WRK-TAB-REC-CNT     TO GBB-ENTRY-COUNT.           05370000
00538      MOVE INPUT-PPF-RECORD          TO OUTPUT-WRK-RECORD.         05380000
00539      MOVE ASUR-SLOT                 TO GBB-PROVISION-SLOT-NO.     05390000
00540      WRITE OUTPUT-PPF-RECORD.                                     05400000
00541                                                                   05410000
00542  3400-EXIT.                                                       05420000
00543      EXIT.                                                        05430000
00544 /                                                                 05440000
00545 ******************************************************************05450000
00546 ****       BENEFIT PROVISION 'PRR' TABULAR                        05460000
00547 ******************************************************************05470000
00548  3500-PROCESS-PRR-TABULAR.                                        05480000
00549                                                                   05490000
00550 ***************************************************************   05500000
00551 ***** IF SLOT IS LESS THAN 9,000,000                      *****   05510000
00552 ***** BYPASS THIS RECORD.                                 *****   05520000
00553 ***************************************************************   05530000
00554                                                                   05540000
00555      IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                05550000
00556          GO TO 3500-EXIT.                                         05560000
00557                                                                   05570000
00558 ***************************************************************   05580000
00559 ***** CALL TABULAR HASHING PROGRAM                        *****   05590000
00560 ***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   05600000
00561 ***************************************************************   05610000
00562                                                                   05620000
00563      INITIALIZE ACCUM-SLOT-AREA.                                  05630000
00564      MOVE GBE3-ENTRY-COUNT         TO GBE2-ENTRY-COUNT.           05640000
00565      MOVE GBE3-RECORD              TO GBE2-RECORD.                05650000
00566      MOVE GBE2-RECORD              TO ASUR-REC-AREA.              05660000
00567      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          05670000
00568      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 05680000
00569                              ACCUM-SLOT-AREA.                     05690000
00570                                                                   05700000
00571 ***************************************************************   05710000
00572 ***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   05720000
00573 ***************************************************************   05730000
00574                                                                   05740000
00575      IF     HASH-RETURN-CODE = '00'                               05750000
00576         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  05760000
00577      ELSE                                                         05770000
00578         IF     HASH-RETURN-CODE = '01'                            05780000
00579            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  05790000
00580         ELSE                                                      05800000
00581            GO TO 3500-EXIT.                                       05810000
00582                                                                   05820000
00583                                                                   05830000
00584 ***************************************************************   05840000
00585 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   05850000
00586 ***** THE SEQUENTIAL FILE.                                *****   05860000
00587 ***************************************************************   05870000
00588                                                                   05880000
00589      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         05890000
00590      MOVE INPUT-WRK-TAB-REC-CNT     TO GBE-ENTRY-COUNT.           05900000
00591      MOVE INPUT-PRR-RECORD          TO OUTPUT-WRK-RECORD.         05910000
00592      MOVE ASUR-SLOT                 TO GBE-PROVISION-SLOT-NO.     05920000
00593      WRITE OUTPUT-PRR-RECORD.                                     05930000
00594                                                                   05940000
00595  3500-EXIT.                                                       05950000
00596      EXIT.                                                        05960000
00597 /                                                                 05970000
00598 ******************************************************************05980000
00599 ****       BENEFIT PROVISION 'PRV' TABULAR                        05990000
00600 ******************************************************************06000000
00601  3600-PROCESS-PRV-TABULAR.                                        06010000
00602                                                                   06020000
00603 ***************************************************************   06030000
00604 ***** IF SLOT IS LESS THAN 9,000,000                      *****   06040000
00605 ***** BYPASS THIS RECORD.                                 *****   06050000
00606 ***************************************************************   06060000
00607                                                                   06070000
00608      IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                06080000
00609          GO TO 3600-EXIT.                                         06090000
00610                                                                   06100000
00611 ***************************************************************   06110000
00612 ***** CALL TABULAR HASHING PROGRAM                        *****   06120000
00613 ***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   06130000
00614 ***************************************************************   06140000
00615                                                                   06150000
00616      INITIALIZE ACCUM-SLOT-AREA.                                  06160000
00617      MOVE GBH3-RECORD              TO ASUR-REC-AREA.              06170000
00618      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          06180000
00619      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 06190000
00620                              ACCUM-SLOT-AREA.                     06200000
00621                                                                   06210000
00622 ***************************************************************   06220000
00623 ***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   06230000
00624 ***************************************************************   06240000
00625                                                                   06250000
00626      IF     HASH-RETURN-CODE = '00'                               06260000
00627         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  06270000
00628      ELSE                                                         06280000
00629         IF     HASH-RETURN-CODE = '01'                            06290000
00630            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  06300000
00631         ELSE                                                      06310000
00632            GO TO 3600-EXIT.                                       06320000
00633                                                                   06330000
00634                                                                   06340000
00635 ***************************************************************   06350000
00636 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   06360000
00637 ***** THE SEQUENTIAL FILE.                                *****   06370000
00638 ***************************************************************   06380000
00639                                                                   06390000
00640      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         06400000
00641      MOVE INPUT-WRK-TAB-REC-CNT     TO GBH-ENTRY-COUNT.           06410000
00642      MOVE INPUT-PRV-RECORD          TO OUTPUT-WRK-RECORD.         06420000
00643      MOVE ASUR-SLOT                 TO GBH-PROVISION-SLOT-NO.     06430000
00644      WRITE OUTPUT-PRV-RECORD.                                     06440000
00645                                                                   06450000
00646  3600-EXIT.                                                       06460000
00647      EXIT.                                                        06470000
00648 /                                                                 06480000
00649 ******************************************************************06490000
00650 ****       BENEFIT PROVISION 'PSC' TABULAR                        06500000
00651 ******************************************************************06510000
00652  3700-PROCESS-PSC-TABULAR.                                        06520000
00653                                                                   06530000
00654 ***************************************************************   06540000
00655 ***** IF SLOT IS LESS THAN 9,000,000                      *****   06550000
00656 ***** BYPASS THIS RECORD.                                 *****   06560000
00657 ***************************************************************   06570000
00658                                                                   06580000
00659      IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                06590000
00660          GO TO 3700-EXIT.                                         06600000
00661                                                                   06610000
00662 ***************************************************************   06620000
00663 ***** CALL TABULAR HASHING PROGRAM                        *****   06630000
00664 ***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   06640000
00665 ***************************************************************   06650000
00666                                                                   06660000
00667      INITIALIZE ACCUM-SLOT-AREA.                                  06670000
00668      MOVE GBI3-RECORD              TO ASUR-REC-AREA.              06680000
00669      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          06690000
00670      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 06700000
00671                              ACCUM-SLOT-AREA.                     06710000
00672                                                                   06720000
00673 ***************************************************************   06730000
00674 ***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   06740000
00675 ***************************************************************   06750000
00676                                                                   06760000
00677      IF     HASH-RETURN-CODE = '00'                               06770000
00678         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  06780000
00679      ELSE                                                         06790000
00680         IF     HASH-RETURN-CODE = '01'                            06800000
00681            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  06810000
00682         ELSE                                                      06820000
00683            GO TO 3700-EXIT.                                       06830000
00684                                                                   06840000
00685                                                                   06850000
00686 ***************************************************************   06860000
00687 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   06870000
00688 ***** THE SEQUENTIAL FILE.                                *****   06880000
00689 ***************************************************************   06890000
00690                                                                   06900000
00691      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         06910000
00692      MOVE INPUT-WRK-TAB-REC-CNT     TO GBI-ENTRY-COUNT.           06920000
00693      MOVE INPUT-PSC-RECORD          TO OUTPUT-WRK-RECORD.         06930000
00694      MOVE ASUR-SLOT                 TO GBI-PROVISION-SLOT-NO.     06940000
00695      WRITE OUTPUT-PSC-RECORD.                                     06950000
00696                                                                   06960000
00697  3700-EXIT.                                                       06970000
00698      EXIT.                                                        06980000
00699 /                                                                 06990000
00700 ******************************************************************07000000
00701 ****       BENEFIT PROVISION 'PVE' TABULAR                        07010000
00702 ******************************************************************07020000
00703  3800-PROCESS-PVE-TABULAR.                                        07030000
00704                                                                   07040000
00705 ***************************************************************   07050000
00706 ***** IF SLOT IS LESS THAN 9,000,000                      *****   07060000
00707 ***** BYPASS THIS RECORD.                                 *****   07070000
00708 ***************************************************************   07080000
00709                                                                   07090000
00710      IF INPUT-WRK-TAB-REC-SLOT   NOT >    +8999999                07100000
00711          GO TO 3800-EXIT.                                         07110000
00712                                                                   07120000
00713 ***************************************************************   07130000
00714 ***** CALL TABULAR HASHING PROGRAM                        *****   07140000
00715 ***** TO RETRIEVE EXISTING OR NEW SLOT NUMBER.            *****   07150000
00716 ***************************************************************   07160000
00717                                                                   07170000
00718      INITIALIZE ACCUM-SLOT-AREA.                                  07180000
00719      MOVE GBJ3-ENTRY-COUNT         TO GBJ2-ENTRY-COUNT.           07190000
00720      MOVE GBJ3-RECORD              TO GBJ2-RECORD.                07200000
00721      MOVE GBJ2-RECORD              TO ASUR-REC-AREA.              07210000
00722      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          07220000
00723      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 07230000
00724                              ACCUM-SLOT-AREA.                     07240000
00725                                                                   07250000
00726 ***************************************************************   07260000
00727 ***** MARK RECORDS AS 'EXISTING' OR 'NEW'.                *****   07270000
00728 ***************************************************************   07280000
00729                                                                   07290000
00730      IF     HASH-RETURN-CODE = '00'                               07300000
00731         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  07310000
00732      ELSE                                                         07320000
00733         IF     HASH-RETURN-CODE = '01'                            07330000
00734            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  07340000
00735         ELSE                                                      07350000
00736            GO TO 3800-EXIT.                                       07360000
00737                                                                   07370000
00738                                                                   07380000
00739 ***************************************************************   07390000
00740 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO            *****   07400000
00741 ***** THE SEQUENTIAL FILE.                                *****   07410000
00742 ***************************************************************   07420000
00743                                                                   07430000
00744      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         07440000
00745      MOVE INPUT-WRK-TAB-REC-CNT     TO GBJ-ENTRY-COUNT.           07450000
00746      MOVE INPUT-PVE-RECORD          TO OUTPUT-WRK-RECORD.         07460000
00747      MOVE ASUR-SLOT                 TO GBJ-PROVISION-SLOT-NO.     07470000
00748      WRITE OUTPUT-PVE-RECORD.                                     07480000
00749                                                                   07490000
00750  3800-EXIT.                                                       07500000
00751      EXIT.                                                        07510000
00752 /                                                                 07520000
00753 ******************************************************************07530000
00754 **                                                                07540000
00755 **                  CLOSE SEQUENTIAL FILES                        07550000
00756 **                                                                07560000
00757 ******************************************************************07570000
00758  9000-CLOSE-THE-FILES.                                            07580000
00759      CLOSE INPUT-WRK-FILE,                                        07590000
00760            OUTPUT-WRK-FILE.                                       07600000
00761                                                                   07610000
00762      MOVE 'C'      TO WS-GHS1BAT-PROCESS-IND.                     07620000
00763      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA.                07630000
00764                                                                   07640000
00765  9000-EXIT.                                                       07650000
00766      EXIT.                                                        07660000
00767 /                                                                 07670000
