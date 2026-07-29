00001  IDENTIFICATION DIVISION.                                         00010000
00002  PROGRAM-ID. GC0070.                                              00020000
00003  AUTHOR.  ED WITKUS.                                              00030000
00004  INSTALLATION.  HCSC.                                             00040000
00005  DATE-WRITTEN.  12/10/87.                                         00050000
00006  DATE-COMPILED.                                                   00060000
00007 ******************************************************************00070000
00008 *                                                                 00080000
00009 *   THIS PROGRAM CREATES SORT WORK RECORDS.                       00090000
00010 *                                                                 00100000
00011 *  TSGVSAM1 IS THE TABULAR FILE                                   00110000
00012 ******************************************************************00120000
00013 ******************************************************************00130000
00014 ******************************************************************00140000
00015 *                  U P D A T E   H I S T O R Y                   *00150000
00016 *                                                                *00160000
00017 **-NUM-* *-DATE-* *WHO* *----------  DESCRIPTION  ---------------*00170000
00018 *                                                                *00180000
00019 *                       ----ACCUM TABULAR RECORD MODIFICATION--- *00190000
00020 * 11154   10/02/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *00200000
00021 * D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *00210000
00022 * D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *00220000
00023 * D270                  4. INCREASE MAX REC LENGTH FOR ACCUM REC *00230000
00024 *                          TO 8157                               *00240000
00025 *                                                                *00250000
00026 *                       ----ACCUM TABULAR RECORD MODIFICATION--- *00260000
00027 * 11154    2/21/91  FRY   -DECREASE MAX OCCURS FROM 46 TO 44.    *00270000
00028 *                         -DECREASE MAX REC LENGTH FOR ACCUM REC *00280000
00029 *                           FROM 8157 TO 7805.                   *00290000
00030 *                                                                *00300000
00031 *          1/10/95  EMS    CONVERTED TO COBOL II.                *00310000
00032 *                                                                *00320000
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION          00321002
00033 *                                                                *00330000
00034 ******************************************************************00340000
00035  ENVIRONMENT DIVISION.                                            00350000
00036                                                                   00360000
00037  CONFIGURATION SECTION.                                           00370000
00038  SOURCE-COMPUTER.  IBM-370.                                       00380000
00039  OBJECT-COMPUTER.  IBM-370.                                       00390000
00040  INPUT-OUTPUT SECTION.                                            00400000
00041                                                                   00410000
00042  FILE-CONTROL.                                                    00420000
00043      SELECT INPUT-TABULAR-FILE        ASSIGN  TO UT-S-GC0070A.    00430000
00044      SELECT OUTPUT-WORKFILE           ASSIGN  TO UT-S-GC0070B.    00440000
00045      SELECT OUTPUT-EXISTING           ASSIGN  TO UT-S-GC0070C.    00450000
00046  DATA DIVISION.                                                   00460000
00047                                                                   00470000
00048  FILE SECTION.                                                    00480000
00049                                                                   00490000
00050  FD  INPUT-TABULAR-FILE                                           00500000
00051          LABEL RECORDS ARE STANDARD                               00510000
00052          RECORDING MODE IS V                                      00520000
00053          BLOCK CONTAINS  0  RECORDS.                              00530000
00054                                                                   00540000
00055  01  INPUT-TABULAR-RECORD.                                        00550000
00056      05 INPUT-WORK-KEY                    PIC X(100).             00560000
00057      05 INPUT-FIXED-PORTION.                                      00570000
00058          10 IN-TAB-ID                     PIC X(6).               00580000
00059          10 IN-TAB-SLOT                   PIC S9(7) COMP-3.       00590000
00060          10 FILLER                        PIC X(27).              00600000
00061          10 INPUT-COUNT                   PIC S9(5) COMP-3.       00610000
00062          10 FILLER                        PIC X(21).              00620000
00063      05 INPUT-OCCURS-TABLE.                                       00630000
00064          10  INPUT-OCCURS-TAB OCCURS 1 TO 44 TIMES                00640000
00065                               DEPENDING ON INPUT-COUNT            00650000
00066                               INDEXED BY INPUT-INDEX.             00660000
00067              15 INPUT-ENTRY               PIC X(176).             00670000
00068                                                                   00680000
00069 /                                                                 00690000
00070  FD  OUTPUT-WORKFILE                                              00700000
00071          LABEL RECORDS ARE STANDARD                               00710000
00072          RECORDING MODE IS F                                      00720000
00073          BLOCK CONTAINS  0  RECORDS.                              00730000
00074                                                                   00740000
00075  01  OUTPUT-WORKFILE-REC.                                         00750000
00076      05 OUTPUT-WORK-KEY               PIC X(100).                 00760000
00077      05 OUTPUT-WORK-FIXED             PIC X(61).                  00770000
00078      05 OUTPUT-WORK-OCCURS            PIC X(176).                 00780000
00079                                                                   00790000
00080  FD  OUTPUT-EXISTING                                              00800000
00081          LABEL RECORDS ARE STANDARD                               00810000
00082          RECORDING MODE IS F                                      00820000
00083          BLOCK CONTAINS  0  RECORDS.                              00830000
00084                                                                   00840000
00085  01  OUTPUT-EXISTING-REC.                                         00850000
00086      05 OUTPUT-EXIST-WORK-KEY        PIC X(100).                  00860000
00087      05 OUTPUT-EXIST-FIXED           PIC X(61).                   00870000
00088      05 OUTPUT-EXIST-FIXED-SPLIT REDEFINES OUTPUT-EXIST-FIXED.    00880000
00089         10  OUTPUT-EXIST-TAB-ID      PIC X(6).                    00890000
00090         10  OUTPUT-EXIST-TAB-SLOT    PIC S9(7) COMP-3.            00900000
00091         10  FILLER                   PIC X(51).                   00910000
00092      05 OUTPUT-EXIST-OCCURS          PIC X(176).                  00920000
00093                                                                   00930000
00094 /                                                                 00940000
00095  WORKING-STORAGE SECTION.                                         00950000
00096                                                                   00960000
00097  01  FILLER                        PIC  X(24)                     00970000
00098              VALUE 'GC0070 WORKING STORAGE'.                      00980000
00099  01  ABEND-CODE                    PIC 9(4)       COMP.           00990000
00100                                                                   01000000
00101  01  WS-COUNTERS.                                                 01010000
00102      05  WS-READ-COUNT             PIC 9(8)       VALUE ZEROS.    01020000
00103      05  WS-WRITE-WORK-COUNT       PIC 9(8)       VALUE ZEROS.    01030000
00104      05  WS-WRITE-EXIST-COUNT      PIC 9(8)       VALUE ZEROS.    01040000
00105                                                                   01050000
00106  01  WS-SWITCHES.                                                 01060000
00107      05 WS-EOF-SW                  PIC X          VALUE SPACES.   01070000
00108          88 EOF                                   VALUE '1'.      01080000
00109      05 WS-VSAM-EOF-SW             PIC X          VALUE SPACES.   01090000
00110          88 VSAM-EOF                              VALUE '1'.      01100000
00111      05 WS-ABM-SW                  PIC X          VALUE '0'.      01110000
00112      05 WS-ACL-SW                  PIC X          VALUE '0'.      01120000
00113      05 WS-ADL-SW                  PIC X          VALUE '0'.      01130000
00114      05 WS-AOL-SW                  PIC X          VALUE '0'.      01140000
00115                                                                   01150000
00116  01  MISC-AREA.                                                   01160000
00117      05  H-TAB-ID                  PIC X(6).                      01170000
00118      05  H-LAST-SLOT               PIC S9(7) COMP-3 VALUE ZEROS.  01180000
00119 /                                                                 01190000
00120  01  PARM-SET.                                                    01200000
00121      05 SET-RDW.                                                  01210000
00122          10 SET-RECORD-LENGTH      PIC 9(4)       COMP.           01220000
00123          10 SET-FEEDBACK-CODE      PIC 9(4)       COMP.           01230000
00124      05  SET-VALUE                 PIC 9(8)       COMP.           01240000
00125                                                                   01250000
00126  01  PARM-ONE.                                                    01260000
00127      05 RESERVED-FLDS              PIC 9(8)   VALUE ZEROS COMP.   01270000
00128      05 RESERVED-ONE  REDEFINES  RESERVED-FLDS.                   01280000
00129          10 REQUEST-TYPE           PIC X.                         01290000
00130          10 FILLER                 PIC X(3).                      01300000
00131                                                                   01310000
00132  01  PARM-TWO.                                                    01320000
00133      03 RDW.                                                      01330000
00134          10 RECORD-LENGTH          PIC 9(4)   COMP.               01340000
00135          10 FEEDBACK-CODE          PIC 9(4)   COMP.               01350000
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION          01351002
00136      03  VSAM-REC-AREA             PIC X(31370).                  01360001
00137      03  VSAM-REC-AREA-SPLIT REDEFINES VSAM-REC-AREA.             01370000
00138          05 VSAM-FIXED-PORTION.                                   01380000
00139              10 VSAM-TAB-ID                   PIC X(6).           01390000
00140              10 VSAM-TAB-SLOT                 PIC S9(7) COMP-3.   01400000
00141              10 FILLER                        PIC X(27).          01410000
00142              10 VSAM-COUNT                    PIC S9(5) COMP-3.   01420000
00143              10 FILLER                        PIC X(21).          01430000
00144          05 VSAM-OCCURS-TABLE.                                    01440000
00145              10  VSAM-OCCURS-TAB OCCURS 44 TIMES                  01450000
00146                                   INDEXED BY VSAM-INDEX.          01460000
00147                  15 VSAM-ENTRY                PIC X(176).         01470000
00148                                                                   01480000
00149                                                                   01490000
00150 /                                                                 01500000
00151  PROCEDURE DIVISION.                                              01510000
00152  0000-MAINLINE.                                                   01520000
00153      PERFORM R1000-OPEN THRU R1000-EXIT.                          01530000
00154                                                                   01540000
00155      PERFORM  R1100-READ-INPUT-FILE THROUGH R1100-EXIT.           01550000
00156                                                                   01560000
00157      PERFORM  0100-PROCESS  THROUGH  0100-EXIT                    01570000
00158          UNTIL EOF.                                               01580000
00159                                                                   01590000
00160      PERFORM R1200-CLOSE THRU R1200-EXIT.                         01600000
00161                                                                   01610000
00162      STOP RUN.                                                    01620000
00163                                                                   01630000
00164  0000-EXIT. EXIT.                                                 01640000
00165 /                                                                 01650000
00166  0100-PROCESS.                                                    01660000
00167      IF IN-TAB-ID = '#ABM  ' OR '#ACL  ' OR '#ADL  ' OR '#AOL  '  01670000
00168          NEXT SENTENCE                                            01680000
00169      ELSE                                                         01690000
00170          PERFORM R1100-READ-INPUT-FILE THRU R1100-EXIT            01700000
00171          GO TO 0100-EXIT.                                         01710000
00172                                                                   01720000
00173                                                                   01730000
00174      PERFORM 0200-WRITE-WORK THRU 0200-EXIT                       01740000
00175          VARYING INPUT-INDEX FROM 1 BY 1                          01750000
00176          UNTIL INPUT-INDEX > INPUT-COUNT.                         01760000
00177      IF IN-TAB-ID = '#ABM  '                                      01770000
00178          IF WS-ABM-SW = '0'                                       01780000
00179              MOVE '1' TO WS-ABM-SW                                01790000
00180              PERFORM 0300-PROCESS-ALL THRU 0300-EXIT.             01800000
00181      IF IN-TAB-ID = '#ACL  '                                      01810000
00182          IF WS-ACL-SW = '0'                                       01820000
00183              MOVE '1' TO WS-ACL-SW                                01830000
00184              PERFORM 0300-PROCESS-ALL THRU 0300-EXIT.             01840000
00185      IF IN-TAB-ID = '#ADL  '                                      01850000
00186          IF WS-ADL-SW = '0'                                       01860000
00187              MOVE '1' TO WS-ADL-SW                                01870000
00188              PERFORM 0300-PROCESS-ALL THRU 0300-EXIT.             01880000
00189      IF IN-TAB-ID = '#AOL  '                                      01890000
00190          IF WS-AOL-SW = '0'                                       01900000
00191              MOVE '1' TO WS-AOL-SW                                01910000
00192              PERFORM 0300-PROCESS-ALL THRU 0300-EXIT.             01920000
00193      PERFORM  R1100-READ-INPUT-FILE THROUGH R1100-EXIT.           01930000
00194  0100-EXIT.                                                       01940000
00195      EXIT.                                                        01950000
00196 /                                                                 01960000
00197  0200-WRITE-WORK.                                                 01970000
00198      MOVE INPUT-WORK-KEY            TO OUTPUT-WORK-KEY.           01980000
00199      MOVE INPUT-FIXED-PORTION       TO OUTPUT-WORK-FIXED.         01990000
00200      MOVE INPUT-ENTRY (INPUT-INDEX) TO OUTPUT-WORK-OCCURS.        02000000
00201      WRITE OUTPUT-WORKFILE-REC.                                   02010000
00202      ADD 1 TO WS-WRITE-WORK-COUNT.                                02020000
00203  0200-EXIT.                                                       02030000
00204      EXIT.                                                        02040000
00205 /                                                                 02050000
00206  0300-PROCESS-ALL.                                                02060000
00207 ******                                                            02070000
00208 ** WE START THE FILE AT 11. WE DON'T WANT TO MATCH THE SKELETON   02080000
00209 ** AND ASSIGN A 1.                                                02090000
00210 ******                                                            02100000
00211      MOVE IN-TAB-ID TO VSAM-TAB-ID,                               02110000
00212                        H-TAB-ID.                                  02120000
00213      MOVE 11        TO VSAM-TAB-SLOT.                             02130000
00214      MOVE ZEROS     TO WS-VSAM-EOF-SW.                            02140000
00215      PERFORM R1400-POINT THRU R1400-EXIT.                         02150000
00216      PERFORM R1500-READNEXT THRU R1500-EXIT.                      02160000
00217 *******                                                           02170000
00218 **  AN EOF AT THIS POINT IS UNLIKELY; HOWEVER, JUST IN            02180000
00219 **  CASE, A LAST SLOT OF 10 IS WRITTEN, AND DURING THE MATCH      02190000
00220 **  PROCESS AN 11 WILL BE ASSIGNED.                               02200000
00221 *******                                                           02210000
00222      IF VSAM-EOF                                                  02220000
00223          MOVE LOW-VALUES TO OUTPUT-EXISTING-REC                   02230000
00224          MOVE H-TAB-ID TO OUTPUT-EXIST-TAB-ID                     02240000
00225          MOVE +10      TO OUTPUT-EXIST-TAB-SLOT                   02250000
00226          WRITE OUTPUT-EXISTING-REC                                02260000
00227          ADD 1 TO WS-WRITE-EXIST-COUNT                            02270000
00228          GO TO 0300-EXIT.                                         02280000
00229      PERFORM 0400-READ THRU 0400-EXIT                             02290000
00230          UNTIL VSAM-EOF.                                          02300000
00231      MOVE LOW-VALUES   TO OUTPUT-EXISTING-REC.                    02310000
00232      MOVE H-TAB-ID     TO OUTPUT-EXIST-TAB-ID.                    02320000
00233      MOVE H-LAST-SLOT  TO OUTPUT-EXIST-TAB-SLOT.                  02330000
00234      WRITE OUTPUT-EXISTING-REC.                                   02340000
00235      ADD 1 TO WS-WRITE-EXIST-COUNT.                               02350000
00236  0300-EXIT.                                                       02360000
00237      EXIT.                                                        02370000
00238 /                                                                 02380000
00239  0400-READ.                                                       02390000
00240      MOVE VSAM-TAB-SLOT TO H-LAST-SLOT.                           02400000
00241      PERFORM 0500-MOVE THRU 0500-EXIT                             02410000
00242          VARYING VSAM-INDEX FROM 1 BY 1                           02420000
00243          UNTIL VSAM-INDEX > VSAM-COUNT.                           02430000
00244      PERFORM R1500-READNEXT THRU R1500-EXIT.                      02440000
00245  0400-EXIT.                                                       02450000
00246      EXIT.                                                        02460000
00247 /                                                                 02470000
00248  0500-MOVE.                                                       02480000
00249      MOVE LOW-VALUES TO OUTPUT-EXISTING-REC.                      02490000
00250      MOVE VSAM-FIXED-PORTION TO OUTPUT-EXIST-FIXED.               02500000
00251      MOVE VSAM-ENTRY (VSAM-INDEX) TO OUTPUT-EXIST-OCCURS.         02510000
00252      WRITE OUTPUT-EXISTING-REC.                                   02520000
00253      ADD 1 TO WS-WRITE-EXIST-COUNT.                               02530000
00254  0500-EXIT.                                                       02540000
00255      EXIT.                                                        02550000
00256 /                                                                 02560000
00257  R1000-OPEN.                                                      02570000
00258                                                                   02580000
00259      OPEN INPUT  INPUT-TABULAR-FILE                               02590000
00260           OUTPUT OUTPUT-WORKFILE                                  02600000
00261                  OUTPUT-EXISTING.                                 02610000
00262                                                                   02620000
00263      MOVE  'S'          TO  REQUEST-TYPE.                         02630000
00264      MOVE   8           TO  SET-RECORD-LENGTH.                    02640000
00265      MOVE   3           TO  SET-VALUE.                            02650000
00266      CALL  'TSGVSAM1'  USING  PARM-ONE  PARM-SET.                 02660000
00267                                                                   02670000
00268      IF  REQUEST-TYPE  NOT =      'S'                             02680000
00269          DISPLAY 'SET ERROR'                                      02690000
00270          MOVE  FEEDBACK-CODE  TO  ABEND-CODE                      02700000
00271          GO TO  9999-ERROR-RTN.                                   02710000
00272  R1000-EXIT.                                                      02720000
00273      EXIT.                                                        02730000
00274 /                                                                 02740000
00275  R1100-READ-INPUT-FILE.                                           02750000
00276                                                                   02760000
00277      READ INPUT-TABULAR-FILE AT END                               02770000
00278         MOVE '1'  TO   WS-EOF-SW.                                 02780000
00279                                                                   02790000
00280  R1100-EXIT. EXIT.                                                02800000
00281                                                                   02810000
00282  R1200-CLOSE.                                                     02820000
00283       CLOSE INPUT-TABULAR-FILE,                                   02830000
00284             OUTPUT-WORKFILE,                                      02840000
00285             OUTPUT-EXISTING.                                      02850000
00286                                                                   02860000
00287      MOVE     'C'           TO   REQUEST-TYPE.                    02870000
00288      CALL  'TSGVSAM1'  USING  PARM-ONE  PARM-TWO.                 02880000
00289                                                                   02890000
00290      IF  REQUEST-TYPE  NOT =      'C'                             02900000
00291          DISPLAY 'CLOSE ERROR'                                    02910000
00292          MOVE  FEEDBACK-CODE  TO  ABEND-CODE                      02920000
00293          GO TO  9999-ERROR-RTN.                                   02930000
00294                                                                   02940000
00295  R1200-EXIT.                                                      02950000
00296      EXIT.                                                        02960000
00297                                                                   02970000
00298  R1400-POINT.                                                     02980000
00299      MOVE  'P'          TO  REQUEST-TYPE.                         02990000
00300      MOVE  14           TO  RECORD-LENGTH.                        03000000
00301      CALL  'TSGVSAM1'  USING  PARM-ONE  PARM-TWO.                 03010000
00302      IF  REQUEST-TYPE  NOT = 'P'                                  03020000
00303          DISPLAY 'BAD POINT - GCEW70  R1400-POINT'                03030000
00304          MOVE  FEEDBACK-CODE  TO  ABEND-CODE                      03040000
00305          GO TO  9999-ERROR-RTN.                                   03050000
00306  R1400-EXIT.                                                      03060000
00307      EXIT.                                                        03070000
00308                                                                   03080000
00309  R1500-READNEXT.                                                  03090000
00310                                                                   03100000
00311      MOVE  'G'          TO  REQUEST-TYPE.                         03110000
00312      CALL  'TSGVSAM1'  USING  PARM-ONE  PARM-TWO.                 03120000
00313                                                                   03130000
00314      IF  REQUEST-TYPE  = '2'                                      03140000
00315          MOVE '1' TO WS-VSAM-EOF-SW                               03150000
00316          GO TO  R1500-EXIT.                                       03160000
00317                                                                   03170000
00318      IF  REQUEST-TYPE  NOT =      'G'                             03180000
00319          DISPLAY 'BAD GET -- GCEW70 1500-READNEXT'                03190000
00320          MOVE  FEEDBACK-CODE  TO  ABEND-CODE                      03200000
00321          GO TO  9999-ERROR-RTN.                                   03210000
00322                                                                   03220000
00323      IF  VSAM-TAB-ID NOT = H-TAB-ID                               03230000
00324          MOVE '1' TO WS-VSAM-EOF-SW                               03240000
00325          GO TO  R1500-EXIT.                                       03250000
00326                                                                   03260000
00327  R1500-EXIT.                                                      03270000
00328      EXIT.                                                        03280000
00329 /                                                                 03290000
00330  9999-ERROR-RTN.                                                  03300000
00331                                                                   03310000
00332      CALL  'TSGEND' USING  ABEND-CODE.                            03320000
00333                                                                   03330000
00334  9999-ERROR-RTN-EXIT. EXIT.                                       03340000
