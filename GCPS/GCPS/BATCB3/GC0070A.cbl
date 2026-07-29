00001  IDENTIFICATION DIVISION.                                         00010000
00002 *THIS IS A COBOL/2 PROGRAM                                        00020000
00003  PROGRAM-ID.    GC0070A.                                          00030000
00004  AUTHOR.        KEVIN DUNN.                                       00040000
00005  DATE-WRITTEN.  OCT 1991.                                         00050000
00006  DATE-COMPILED.                                                   00060000
00007 ******************************************************************00070000
00008 ***                                                            ***00080000
00009 ***  GENERIC CONTRACT PROCESSING SYSTEM (GCPS)                 ***00090000
00010 ***  ACCUM TABULAR SLOT ASSIGNMENT                             ***00100000
00011 ***                                                            ***00110000
00012 ***   INPUT FILE:   OPER.GCPS.GC0060C.ACCUM.TAB.RLSE.UPDA      ***00120000
00013 ***  OUTPUT FILE:   OPER.GCPS.GC0070B.ACCUM.SLOTUPDA           ***00130000
00014 ***                                                            ***00140000
00015 ***  PURPOSE:                                                  ***00150000
00016 ***      ACCUM SLOT ASSIGNMENT USING HASHING PROGRAM.          ***00160000
00017 ***                                                            ***00170000
00018 ******************************************************************00180000
00019 ***              U P D A T E   H I S T O R Y                   ***00190000
00020 ******************************************************************00200000
00021 ***  CHG-NUM    DATE    WHO          DESCRIPTION               ***00210000
00022 ***                                                            ***00220000
00023 ***           02/04/93  KJD     PROGRAM WRITTEN                ***00230000
00024 ***    1293   02/08/93  JGR     MINOR MODIFICATION'S FOR       ***00240000
00025 ***                             BATCH HASHING.                 ***00250000
00026 ***                                                            ***00260000
00027 ***           1/10/95   EMS     CONVERTED TO COBOL II.         ***00270000
00028 ***                                                            ***00280000
00029 ***  15057     09/11/97  AB   ADDED CODE TO SUPPORT THE YEAR   ***00290000
00030 ***                           2000 AND THE EXPANSION OF THE    ***00300000
00031 ***                           CONTRACT KEY TO SUPPORT THE TX   ***00310000
00032 ***                           MERGER.                          ***00320000
00033 ***                                                            ***00330000
00034 ***  15182     12/12/98 FRY   ADD #ACP ACCUM TABULAR           ***00340000
00035 ***                                                            ***00350000
00036 ***          08-14-02   GTF   RECOMPILE FOR OPID EXPANSION     ***00360000
00037 ***                                                            ***00370000
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION          00371001
ED0624*BBDA-58217  06/04/24   ED    RECOMPILE FOR PEAQ COPYBOOK      ***00371002
ED0624*                             EXPANSION:                       ***00371003
ED0624*                                   COPYBKS - GCTABM*, GCTACL*,***00371004
ED0624*                                   GCTACP*,  GCTADL*, GCTADL* ***00371005
00037 ***                                                            ***00372001
00038 ******************************************************************00380000
00039  ENVIRONMENT DIVISION.                                            00390000
00040  CONFIGURATION SECTION.                                           00400000
00041  SOURCE-COMPUTER.  IBM-370.                                       00410000
00042  OBJECT-COMPUTER.  IBM-370.                                       00420000
00043                                                                   00430000
00044  INPUT-OUTPUT SECTION.                                            00440000
00045  FILE-CONTROL.                                                    00450000
00046                                                                   00460000
00047      SELECT ACCUM-SLOT-UNASSIGN-FILE ASSIGN TO GC0070A.           00470000
00048      SELECT ACCUM-SLOT-ASSIGN-FILE   ASSIGN TO GC0070B.           00480000
00049                                                                   00490000
00050  DATA DIVISION.                                                   00500000
00051  FILE SECTION.                                                    00510000
00052                                                                   00520000
00053  FD  ACCUM-SLOT-UNASSIGN-FILE                                     00530000
00054      BLOCK CONTAINS 0 RECORDS                                     00540000
00055      LABEL RECORDS ARE STANDARD                                   00550000
00056      RECORDING MODE IS V.                                         00560000
00057  01  INPUT-WRK-RECORD.                                            00570000
00058      COPY GCWRKDCC.                                               00580000
00059      05  INPUT-WRK-TABULAR-RECORD.                                00590000
00060          10  INPUT-WRK-TAB-REC-KEY.                               00600000
00061              15  INPUT-WRK-TAB-REC-ID     PIC X(6).               00610000
00062              15  INPUT-WRK-TAB-REC-SLOT   PIC S9(7) COMP-3.       00620000
00063          10  FILLER                       PIC X(27).              00630000
00064          10  INPUT-WRK-COMPARE.                                   00640000
00065              15  INPUT-WRK-TAB-REC-CNT    PIC S9(5) COMP-3.       00650000
      *DM9441 09/04/09 JJS  CHANGED        FOR FILE CONVERSION          00651001
00066              15  INPUT-WRK-ENTRIES        PIC X(31330).           00660001
00067 /                                                                 00670000
00068  01  ABM-SLOT-UNASSIGN-REC.                                       00680000
00069      05  FILLER                        PIC X(100).                00690000
00070      COPY GCTABMC.                                                00700000
00071 /                                                                 00710000
00072  01  ACL-SLOT-UNASSIGN-REC.                                       00720000
00073      05  FILLER                        PIC X(100).                00730000
00074      COPY GCTACLC.                                                00740000
00075 /                                                                 00750000
00076  01  ACP-SLOT-UNASSIGN-REC.                                       00760000
00077      05  FILLER                        PIC X(100).                00770000
00078      COPY GCTACPC.                                                00780000
00079 /                                                                 00790000
00080  01  ADL-SLOT-UNASSIGN-REC.                                       00800000
00081      05  FILLER                        PIC X(100).                00810000
00082      COPY GCTADLC.                                                00820000
00083 /                                                                 00830000
00084  01  AOL-SLOT-UNASSIGN-REC.                                       00840000
00085      05  FILLER                        PIC X(100).                00850000
00086      COPY GCTAOLC.                                                00860000
00087 /                                                                 00870000
00088  FD  ACCUM-SLOT-ASSIGN-FILE                                       00880000
00089      BLOCK CONTAINS 0 RECORDS                                     00890000
00090      LABEL RECORDS ARE STANDARD                                   00900000
00091      RECORDING MODE IS V.                                         00910000
00092                                                                   00920000
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION          00921001
00093  01  OUTPUT-WRK-RECORD                 PIC X(31470).              00930001
00094 /                                                                 00940000
00095  01  ABM-SLOT-ASSIGN-REC.                                         00950000
00096      05  FILLER                        PIC X(100).                00960000
00097      COPY GCTABM2.                                                00970000
00098 /                                                                 00980000
00099  01  ACL-SLOT-ASSIGN-REC.                                         00990000
00100      05  FILLER                        PIC X(100).                01000000
00101      COPY GCTACL2.                                                01010000
00102 /                                                                 01020000
00103  01  ACP-SLOT-ASSIGN-REC.                                         01030000
00104      05  FILLER                        PIC X(100).                01040000
00105      COPY GCTACP2.                                                01050000
00106 /                                                                 01060000
00107  01  ADL-SLOT-ASSIGN-REC.                                         01070000
00108      05  FILLER                        PIC X(100).                01080000
00109      COPY GCTADL2.                                                01090000
00110 /                                                                 01100000
00111  01  AOL-SLOT-ASSIGN-REC.                                         01110000
00112      05  FILLER                        PIC X(100).                01120000
00113      COPY GCTAOL2.                                                01130000
00114 /                                                                 01140000
00115  WORKING-STORAGE SECTION.                                         01150000
00116 ******************************************************************01160000
00117 ***        W O  R K I N G    S T O R A G E    A R E A          ***01170000
00118 ******************************************************************01180000
00119                                                                   01190000
00120  01  FILLER                            PIC X(36) VALUE            01200000
00121                           'GC0070 WORKING STORAGE STARTS HERE'.   01210000
00122                                                                   01220000
00123  01  WORK-AREAS.                                                  01230000
00124      05  FLAGS.                                                   01240000
00125          10  EOF-FLAG            PIC X(01)  VALUE 'N'.            01250000
00126      05  HASH-PROGRAM            PIC X(17)  VALUE 'GHS1BAT'.      01260000
00127                                                                   01270000
00128 **** GHS1BAT LINKAGE AREA                                         01280000
00129                                                                   01290000
00130  01  WS-GHS1BAT-CALL-AREA.                                        01300000
00131    03  WS-GHS1BAT-PROCESS-IND    PIC X.                           01310000
00132        88  CALL-FOR-OPEN                    VALUE 'O'.            01320000
00133        88  CALL-FOR-CLOSE                   VALUE 'C'.            01330000
00134        88  CALL-FOR-PROCESS                 VALUE 'P'.            01340000
00135                                                                   01350000
00136  01  ACCUM-SLOT-AREA.                                             01360000
00137      05  HASH-RETURN-CODE        PIC X(02).                       01370000
00138      05  ASUR-REC-AREA.                                           01380000
00139          10 ASUR-TAB-ID          PIC X(0006).                     01390000
00140          10 ASUR-SLOT            PIC S9(07) COMP-3.               01400000
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION          01401001
00141          10 FILLER               PIC X(31360).                    01410001
00142                                                                   01420000
00143                                                                   01430000
00144 *01  LENGTHS.                                                     01440000
00145 *    COPY GCCDRLEN SUPPRESS.                                      01450000
00146                                                                   01460000
00147  01  FILLER                         PIC X(25) VALUE               01470000
00148                                'WORKING STORAGE ENDS HERE'.       01480000
00149 /                                                                 01490000
00150  PROCEDURE DIVISION.                                              01500000
00151                                                                   01510000
00152 ******************************************************************01520000
00153 **                                                                01530000
00154 **               P R O C E S S    C O N T R O L                   01540000
00155 **                                                                01550000
00156 ******************************************************************01560000
00157  0000-MAINLINE.                                                   01570000
00158                                                                   01580000
00159      PERFORM 1000-OPEN-THE-FILES THRU 1000-EXIT.                  01590000
00160                                                                   01600000
00161      IF EOF-FLAG = 'Y'                                            01610000
00162         DISPLAY 'GC0070A--NO INPUT RECORDS RECEIVED'              01620000
00163      ELSE                                                         01630000
00164         PERFORM 2000-PROCESS-WORK-FILE THRU 2000-EXIT             01640000
00165           UNTIL EOF-FLAG = 'Y'.                                   01650000
00166                                                                   01660000
00167      PERFORM 3000-CLOSE-THE-FILES THRU 3000-EXIT.                 01670000
00168      STOP RUN.                                                    01680000
00169                                                                   01690000
00170  0000-EXIT.                                                       01700000
00171      EXIT.                                                        01710000
00172 /*****************************************************************01720000
00173 ***                                                            ***01730000
00174  1000-OPEN-THE-FILES.                                             01740000
00175                                                                   01750000
00176      OPEN INPUT  ACCUM-SLOT-UNASSIGN-FILE                         01760000
00177           OUTPUT ACCUM-SLOT-ASSIGN-FILE.                          01770000
00178                                                                   01780000
00179      SET CALL-FOR-OPEN TO TRUE.                                   01790000
00180      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA.                01800000
00181                                                                   01810000
00182 **--- READ THE FIRST RECORD.                                      01820000
00183 **                                                                01830000
00184      PERFORM 4000-READ-INPUT-FILE THRU 4000-EXIT.                 01840000
00185                                                                   01850000
00186  1000-EXIT.  EXIT.                                                01860000
00187 /*****************************************************************01870000
00188  2000-PROCESS-WORK-FILE.                                          01880000
00189                                                                   01890000
00190      IF  INPUT-WRK-RECORD = LOW-VALUES                            01900000
00191          GO TO 2000-EXIT.                                         01910000
00192                                                                   01920000
00193                                                                   01930000
00194      EVALUATE  INPUT-WRK-TAB-REC-ID                               01940000
00195        WHEN  '#ABM  '                                             01950000
00196          PERFORM 2100-PROCESS-ABM-TABULAR THRU 2100-EXIT          01960000
00197        WHEN  '#ACL  '                                             01970000
00198          PERFORM 2200-PROCESS-ACL-TABULAR THRU 2200-EXIT          01980000
00199        WHEN  '#ACP  '                                             01990000
00200          PERFORM 2500-PROCESS-ACP-TABULAR THRU 2500-EXIT          02000000
00201        WHEN  '#ADL  '                                             02010000
00202          PERFORM 2300-PROCESS-ADL-TABULAR THRU 2300-EXIT          02020000
00203        WHEN  '#AOL  '                                             02030000
00204          PERFORM 2400-PROCESS-AOL-TABULAR THRU 2400-EXIT          02040000
00205      END-EVALUATE.                                                02050000
00206                                                                   02060000
00207      PERFORM 4000-READ-INPUT-FILE THRU 4000-EXIT.                 02070000
00208                                                                   02080000
00209                                                                   02090000
00210  2000-EXIT.  EXIT.                                                02100000
00211 /                                                                 02110000
00212  2100-PROCESS-ABM-TABULAR.                                        02120000
00213                                                                   02130000
00214 *********************************************************         02140000
00215 ***** IF SLOT IS LESS THAN 9,000,000                *****         02150000
00216 ***** BYPASS THIS RECORD.                           *****         02160000
00217 *********************************************************         02170000
00218                                                                   02180000
00219      IF GAA-PROVISION-SLOT-NO NOT > +8999999                      02190000
00220          GO TO 2100-EXIT.                                         02200000
00221                                                                   02210000
00222 *********************************************************         02220000
00223 ***** CALL TABULAR HASHING PROGRAM                  *****         02230000
00224 ***** TO RETREIVE EXISTING OR NEW SLOT NUMBER.      *****         02240000
00225 *********************************************************         02250000
00226                                                                   02260000
00227      MOVE LOW-VALUES               TO ASUR-REC-AREA.              02270000
00228      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              02280000
00229      SET CALL-FOR-PROCESS          TO TRUE.                       02290000
00230      CALL HASH-PROGRAM             USING WS-GHS1BAT-CALL-AREA     02300000
00231                                          ACCUM-SLOT-AREA.         02310000
00232                                                                   02320000
00233 *********************************************************         02330000
00234 ***** MARK RECORD AS 'EXISTING' OR 'NEW'.           *****         02340000
00235 *********************************************************         02350000
00236                                                                   02360000
00237      IF HASH-RETURN-CODE = '00'                                   02370000
00238         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  02380000
00239      ELSE                                                         02390000
00240         IF HASH-RETURN-CODE = '01'                                02400000
00241            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  02410000
00242         ELSE                                                      02420000
00243            GO TO 2100-EXIT.                                       02430000
00244                                                                   02440000
00245                                                                   02450000
00246 *********************************************************         02460000
00247 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO      *****         02470000
00248 ***** THE SEQUENTIAL FILE.                          *****         02480000
00249 *********************************************************         02490000
00250                                                                   02500000
00251      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         02510000
00252      MOVE INPUT-WRK-TAB-REC-CNT     TO GAA2-ENTRY-COUNT.          02520000
00253      MOVE ABM-SLOT-UNASSIGN-REC     TO OUTPUT-WRK-RECORD.         02530000
00254      MOVE ASUR-SLOT                 TO GAA2-PROVISION-SLOT-NO.    02540000
00255      WRITE ABM-SLOT-ASSIGN-REC.                                   02550000
00256                                                                   02560000
00257  2100-EXIT.                                                       02570000
00258      EXIT.                                                        02580000
00259 /                                                                 02590000
00260  2200-PROCESS-ACL-TABULAR.                                        02600000
00261                                                                   02610000
00262 *********************************************************         02620000
00263 ***** IF SLOT IS LESS THAN 9,000,000                *****         02630000
00264 ***** BYPASS THIS RECORD.                           *****         02640000
00265 *********************************************************         02650000
00266                                                                   02660000
00267      IF GAB-PROVISION-SLOT-NO NOT > +8999999                      02670000
00268         GO TO 2200-EXIT.                                          02680000
00269                                                                   02690000
00270 *********************************************************         02700000
00271 ***** CALL TABULAR HASHING PROGRAM                  *****         02710000
00272 ***** TO RETREIVE EXISTING OR NEW SLOT NUMBER.      *****         02720000
00273 *********************************************************         02730000
00274                                                                   02740000
00275      MOVE LOW-VALUES               TO ASUR-REC-AREA.              02750000
00276      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              02760000
00277      SET CALL-FOR-PROCESS          TO TRUE.                       02770000
00278      CALL HASH-PROGRAM             USING WS-GHS1BAT-CALL-AREA     02780000
00279                                          ACCUM-SLOT-AREA.         02790000
00280                                                                   02800000
00281 *********************************************************         02810000
00282 ***** MARK RECORD AS 'EXISTING' OR 'NEW'.           *****         02820000
00283 *********************************************************         02830000
00284                                                                   02840000
00285      IF HASH-RETURN-CODE = '00'                                   02850000
00286         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  02860000
00287      ELSE                                                         02870000
00288         IF HASH-RETURN-CODE = '01'                                02880000
00289            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  02890000
00290         ELSE                                                      02900000
00291            GO TO 2200-EXIT.                                       02910000
00292                                                                   02920000
00293                                                                   02930000
00294 *********************************************************         02940000
00295 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO      *****         02950000
00296 ***** THE SEQUENTIAL FILE.                          *****         02960000
00297 *********************************************************         02970000
00298                                                                   02980000
00299      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         02990000
00300      MOVE INPUT-WRK-TAB-REC-CNT     TO GAB2-ENTRY-COUNT.          03000000
00301      MOVE ACL-SLOT-UNASSIGN-REC     TO OUTPUT-WRK-RECORD.         03010000
00302      MOVE ASUR-SLOT                 TO GAB2-PROVISION-SLOT-NO.    03020000
00303      WRITE ACL-SLOT-ASSIGN-REC.                                   03030000
00304                                                                   03040000
00305  2200-EXIT.                                                       03050000
00306      EXIT.                                                        03060000
00307 /                                                                 03070000
00308  2300-PROCESS-ADL-TABULAR.                                        03080000
00309                                                                   03090000
00310 *********************************************************         03100000
00311 ***** IF SLOT IS LESS THAN 9,000,000                *****         03110000
00312 ***** BYPASS THIS RECORD.                           *****         03120000
00313 *********************************************************         03130000
00314                                                                   03140000
00315      IF GAC-PROVISION-SLOT-NO NOT > +8999999                      03150000
00316          GO TO 2300-EXIT.                                         03160000
00317                                                                   03170000
00318 *********************************************************         03180000
00319 ***** CALL TABULAR HASHING PROGRAM                  *****         03190000
00320 ***** TO RETREIVE EXISTING OR NEW SLOT NUMBER.      *****         03200000
00321 *********************************************************         03210000
00322                                                                   03220000
00323      MOVE LOW-VALUES               TO ASUR-REC-AREA.              03230000
00324      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              03240000
00325      SET CALL-FOR-PROCESS          TO TRUE.                       03250000
00326      CALL HASH-PROGRAM             USING WS-GHS1BAT-CALL-AREA     03260000
00327                                          ACCUM-SLOT-AREA.         03270000
00328                                                                   03280000
00329 *********************************************************         03290000
00330 ***** MARK RECORD AS 'EXISTING' OR 'NEW'.           *****         03300000
00331 *********************************************************         03310000
00332                                                                   03320000
00333      IF HASH-RETURN-CODE = '00'                                   03330000
00334         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  03340000
00335      ELSE                                                         03350000
00336         IF HASH-RETURN-CODE = '01'                                03360000
00337            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  03370000
00338         ELSE                                                      03380000
00339            GO TO 2300-EXIT.                                       03390000
00340                                                                   03400000
00341                                                                   03410000
00342 *********************************************************         03420000
00343 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO      *****         03430000
00344 ***** THE SEQUENTIAL FILE.                          *****         03440000
00345 *********************************************************         03450000
00346                                                                   03460000
00347      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         03470000
00348      MOVE INPUT-WRK-TAB-REC-CNT     TO GAC2-ENTRY-COUNT.          03480000
00349      MOVE ADL-SLOT-UNASSIGN-REC     TO OUTPUT-WRK-RECORD.         03490000
00350      MOVE ASUR-SLOT                 TO GAC2-PROVISION-SLOT-NO.    03500000
00351      WRITE ADL-SLOT-ASSIGN-REC.                                   03510000
00352                                                                   03520000
00353  2300-EXIT.                                                       03530000
00354      EXIT.                                                        03540000
00355 /                                                                 03550000
00356  2400-PROCESS-AOL-TABULAR.                                        03560000
00357                                                                   03570000
00358 *********************************************************         03580000
00359 ***** IF SLOT IS LESS THAN 9,000,000                *****         03590000
00360 ***** BYPASS THIS RECORD.                           *****         03600000
00361 *********************************************************         03610000
00362                                                                   03620000
00363      IF GAD-PROVISION-SLOT-NO NOT > +8999999                      03630000
00364          GO TO 2400-EXIT.                                         03640000
00365                                                                   03650000
00366 *********************************************************         03660000
00367 ***** CALL TABULAR HASHING PROGRAM                  *****         03670000
00368 ***** TO RETREIVE EXISTING OR NEW SLOT NUMBER.      *****         03680000
00369 *********************************************************         03690000
00370                                                                   03700000
00371      MOVE LOW-VALUES               TO ASUR-REC-AREA.              03710000
00372      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              03720000
00373      SET CALL-FOR-PROCESS          TO TRUE.                       03730000
00374      CALL HASH-PROGRAM             USING WS-GHS1BAT-CALL-AREA     03740000
00375                                          ACCUM-SLOT-AREA.         03750000
00376                                                                   03760000
00377 *********************************************************         03770000
00378 ***** MARK RECORD AS 'EXISTING' OR 'NEW'.           *****         03780000
00379 *********************************************************         03790000
00380                                                                   03800000
00381      IF HASH-RETURN-CODE = '00'                                   03810000
00382         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  03820000
00383      ELSE                                                         03830000
00384         IF HASH-RETURN-CODE = '01'                                03840000
00385            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  03850000
00386         ELSE                                                      03860000
00387            GO TO 2400-EXIT.                                       03870000
00388                                                                   03880000
00389                                                                   03890000
00390 *********************************************************         03900000
00391 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO      *****         03910000
00392 ***** THE SEQUENTIAL FILE.                          *****         03920000
00393 *********************************************************         03930000
00394                                                                   03940000
00395      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         03950000
00396      MOVE INPUT-WRK-TAB-REC-CNT     TO GAD2-ENTRY-COUNT.          03960000
00397      MOVE AOL-SLOT-UNASSIGN-REC     TO OUTPUT-WRK-RECORD.         03970000
00398      MOVE ASUR-SLOT                 TO GAD2-PROVISION-SLOT-NO.    03980000
00399      WRITE AOL-SLOT-ASSIGN-REC.                                   03990000
00400                                                                   04000000
00401  2400-EXIT.                                                       04010000
00402      EXIT.                                                        04020000
00403 /*****************************************************************04030000
00404  2500-PROCESS-ACP-TABULAR.                                        04040000
00405                                                                   04050000
00406 ***                                                               04060000
00407 * IF SLOT IS LESS THAN 9,000,000, BYPASS THIS RECORD.             04070000
00408 ***                                                               04080000
00409                                                                   04090000
00410      IF GAF-PROVISION-SLOT-NO   NOT >   +8999999                  04100000
00411         GO TO 2500-EXIT.                                          04110000
00412                                                                   04120000
00413 ***                                                               04130000
00414 * CALL TABULAR HASHING PROGRAM TO RETRIEVE EXISTING OR            04140000
00415 * NEW SLOT NUMBER.                                                04150000
00416 ***                                                               04160000
00417                                                                   04170000
00418      MOVE LOW-VALUES               TO ASUR-REC-AREA.              04180000
00419      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              04190000
00420      SET CALL-FOR-PROCESS          TO TRUE.                       04200000
00421      CALL HASH-PROGRAM             USING WS-GHS1BAT-CALL-AREA     04210000
00422                                          ACCUM-SLOT-AREA.         04220000
00423                                                                   04230000
00424 ***                                                               04240000
00425 * MARK RECORD AS 'EXISTING' OR 'NEW'.                             04250000
00426 ***                                                               04260000
00427                                                                   04270000
00428      IF HASH-RETURN-CODE = '00'                                   04280000
00429         MOVE 'E'    TO  WRK-SIGNAL-BATCH-INTERNAL                 04290000
00430      ELSE                                                         04300000
00431      IF HASH-RETURN-CODE = '01'                                   04310000
00432         MOVE 'N'    TO  WRK-SIGNAL-BATCH-INTERNAL                 04320000
00433      ELSE                                                         04330000
00434         GO TO 2500-EXIT.                                          04340000
00435                                                                   04350000
00436                                                                   04360000
00437 ***                                                               04370000
00438 * MOVE IN TABULAR SLOT AND WRITE RECORD TO THE SEQUENTIAL FILE    04380000
00439 ***                                                               04390000
00440                                                                   04400000
00441      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         04410000
00442      MOVE INPUT-WRK-TAB-REC-CNT     TO GAF2-ENTRY-COUNT.          04420000
00443      MOVE ACP-SLOT-UNASSIGN-REC     TO OUTPUT-WRK-RECORD.         04430000
00444      MOVE ASUR-SLOT                 TO GAF2-PROVISION-SLOT-NO.    04440000
00445      WRITE ACP-SLOT-ASSIGN-REC.                                   04450000
00446                                                                   04460000
00447  2500-EXIT.                                                       04470000
00448      EXIT.                                                        04480000
00449 /*****************************************************************04490000
00450  3000-CLOSE-THE-FILES.                                            04500000
00451                                                                   04510000
00452      CLOSE ACCUM-SLOT-UNASSIGN-FILE                               04520000
00453            ACCUM-SLOT-ASSIGN-FILE.                                04530000
00454                                                                   04540000
00455      SET CALL-FOR-CLOSE TO TRUE.                                  04550000
00456      CALL HASH-PROGRAM  USING WS-GHS1BAT-CALL-AREA.               04560000
00457                                                                   04570000
00458  3000-EXIT.  EXIT.                                                04580000
00459 /*****************************************************************04590000
00460  4000-READ-INPUT-FILE.                                            04600000
00461                                                                   04610000
00462      READ ACCUM-SLOT-UNASSIGN-FILE AT END MOVE                    04620000
00463        'Y' TO EOF-FLAG.                                           04630000
00464                                                                   04640000
00465  4000-EXIT.  EXIT.                                                04650000
