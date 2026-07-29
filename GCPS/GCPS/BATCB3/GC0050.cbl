00001  IDENTIFICATION DIVISION.                                         00010000
00002 *THIS IS A COBOL/2 PROGRAM                                        00020000
00003  PROGRAM-ID.     GC0050.                                          00030000
00004  AUTHOR.         ED WITKUS.                                       00040000
00005  INSTALLATION.   HCSC.                                            00050000
00006  DATE-WRITTEN.   OCTOBER, 1987.                                   00060000
00007  DATE-COMPILED.                                                   00070000
00008 ******************************************************************00080000
00009 ******************************************************************00090000
00010 *   INTERNAL TABULAR RECORD MATCH AND SLOT UPDATE PROGRAM.        00100000
00011 ******************************************************************00110000
00012 ***************************************************************** 00120000
00013 *                                                               * 00130000
00014 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        * 00140000
00015 *    *-*         U P D A T E   H I S T O R Y         *-*        * 00150000
00016 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        * 00160000
00017 *                                                               * 00170000
00018 * CHG #    DATE    BY              DESCRIPTION                  * 00180000
00019 * _____  ________  ___  ________________________________________* 00190000
00020 *                                                               * 00200000
00021 * ?????  12/27/88  ENW  FIXED PROBLEM WITH SLOT ASSIGNMENT      * 00210000
00022 *                       FOR #IPGN RECORDS. COUNT WASN'T BEING   * 00220000
00023 *                       MOVED PRIOR TO MOVING INPUT-AREA TO WS. * 00230000
00024 *                                                               * 00240000
00025 * D184/  12/13/89  ENW  ADDED NEW LOGIC FOR NEW INTERNAL TABS,  * 00250000
00026 * D185                  #IDGD AND #IPGP.                        * 00260000
00027 *                                                               * 00270000
00028 * 11154   3/06/91  FRY  INCREASE RECORD AREAS IN FILE SECTION:  * 00280000
00029 *           INPUT-WRK-ENTRIES   PIC X(3960)  CHANGED TO  7765   * 00290000
00030 *           OUTPUT-WRK-RECORD   PIC X(4064)  CHANGED TO  7869   * 00300000
00031 *                                                               * 00310000
00032 * ?????   2/11/92  KJD  ALLOW CHAINED TABS TO BE TREATED AS     * 00320000
00033 *                       EXISTING.                               * 00330000
00034 *                                                               * 00340000
00035 *         6/18/92  KJD  CORRECT CHAIN LOGIC                     * 00350000
00036 *                                                               * 00360000
00037 *  1293   2/02/93  JGR  REPLACE CURRENT PROCESSING WITH MATCHES * 00370000
00038 *                       AGAINST EXISTING TABULARS USING THE     * 00380000
00039 *                       TABULAR HASHING PROGRAM.                * 00390000
00040 *                                                               * 00400000
00041 *         1/10/95  EMS  CONVERTED TO COBOL II.                  * 00410000
00042 *                                                               * 00420000
00043 * 14726/ 10/01/97  AB   ADDED CODE TO SUPPORT THE YEAR 2000     * 00430000
00044 * 15057                 AND THE EXPANSION OF THE GROUP SPECIFI  * 00440000
00045 *                       AND CONTRACT KEY TO SUPPORT THE TEXAS   * 00450000
00046 *                       MERGER.                                 * 00460000
00047 *                                                               * 00470000
00048 *        06/28/00  GSP  ADDED LOGIC FOR NEW INTERNAL TABULAR    * 00480000
00049 *                       #IPGS.                                  * 00490000
00050 *                                                               * 00500000
00051 * D-358  06/19/01  GSP  ADDED LOGIC FOR NEW CONTRACT INTERNAL   * 00510000
00052 *                       TABULARS #IRDX, #IRIC, #IRPR & #IRPV.   * 00520000
00053 *                                                               * 00530000
00054 * D-358  07/24/01  AKK  ADDED LOGIC TO WRITE THE #CDRS RECORDS  * 00540000
00055 *                       WITH #IRDX, #IRIC, #IRPR & #IRPV INTERNAL 00550000
00056 *                       TABS ATTCHED TO NEW OUTPUT FILE GC0050C.* 00560000
00057 *                                                               * 00570000
00058 *        08-14-02   GTF RECOMPILE FOR OPID EXPANSION            * 00580000
00059 *                                                               * 00590000
00060 * D-356A   5/08/03  GTF RECOMPILE FOR COPYBK CHANGES #IPGP,     * 00600000
00061 *                       #IDGD, #IRDX, #IRIC, #IRPR.             * 00610000
00062 *                                                               * 00620000
00063 * NO LOG   4/08/04 KIKI FIX PROGRAM TO ACCEPT THE 9 MILLION     * 00630000
00064 *                       SLOT NOS. FOR #CDRS TABULAR PRIMARY     * 00640000
00065 *                       AND ADDITIONAL INTERNALS.               * 00650000
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION        * 00651004
00066 *                                                               * 00660000
00067 ***************************************************************** 00670000
00068  ENVIRONMENT DIVISION.                                            00680000
00069  CONFIGURATION SECTION.                                           00690000
00070  SOURCE-COMPUTER.  IBM-370.                                       00700000
00071  OBJECT-COMPUTER.  IBM-370.                                       00710000
00072  INPUT-OUTPUT SECTION.                                            00720000
00073                                                                   00730000
00074  FILE-CONTROL.                                                    00740000
00075      SELECT INPUT-WRK-FILE        ASSIGN  TO UT-S-GC0050A.        00750000
00076      SELECT OUTPUT-WRK-FILE       ASSIGN  TO UT-S-GC0050B.        00760000
00077      SELECT OUTPUT-CDRS-WRK-FILE  ASSIGN  TO UT-S-GC0050C.        00770000
00078  DATA DIVISION.                                                   00780000
00079  FILE SECTION.                                                    00790000
00080                                                                   00800000
00081  FD  INPUT-WRK-FILE                                               00810000
00082      LABEL RECORDS ARE STANDARD                                   00820000
00083      RECORDING MODE IS V                                          00830000
00084      BLOCK CONTAINS  0  RECORDS.                                  00840000
00085  01  INPUT-WRK-RECORD.                                            00850000
00086      COPY GCWRKDCC.                                               00860000
00087      05  INPUT-WRK-TABULAR-RECORD.                                00870000
00088          10  INPUT-WRK-TAB-REC-KEY.                               00880000
00089              15  INPUT-WRK-TAB-REC-ID      PIC X(6).              00890000
00090              15  INPUT-WRK-TAB-REC-SLOT    PIC S9(7) COMP-3.      00900000
00091          10  FILLER                        PIC X(27).             00910000
00092          10  INPUT-WRK-COMPARE.                                   00920000
00093              15  INPUT-WRK-TAB-REC-CNT     PIC S9(5) COMP-3.      00930000
      *DM9441 09/04/09 JJS  CHANGED        FOR FILE CONVERSION         *00931003
00094              15  INPUT-WRK-ENTRIES         PIC X(31330).          00940002
00095 /                                                                 00950000
00096  01  INPUT-IBGR-RECORD.                                           00960000
00097      05  FILLER                            PIC X(100).            00970000
00098      COPY GCTIBGR3.                                               00980000
00099 /                                                                 00990000
00100  01  INPUT-IPGN-RECORD.                                           01000000
00101      05  FILLER                            PIC X(100).            01010000
00102      COPY GCTIPGN3.                                               01020000
00103 /                                                                 01030000
00104  01  INPUT-IPGT-RECORD.                                           01040000
00105      05  FILLER                            PIC X(100).            01050000
00106      COPY GCTIPGT3.                                               01060000
00107 /                                                                 01070000
00108  01  INPUT-IPGS-RECORD.                                           01080000
00109      05  FILLER                            PIC X(100).            01090000
00110      COPY GCTIPGS3.                                               01100000
00111 /                                                                 01110000
00112  01  INPUT-IDGD-RECORD.                                           01120000
00113      05  FILLER                            PIC X(100).            01130000
00114      COPY GCTIDGD3.                                               01140000
00115 /                                                                 01150000
00116  01  INPUT-IPGP-RECORD.                                           01160000
00117      05  FILLER                            PIC X(100).            01170000
00118      COPY GCTIPGP3.                                               01180000
00119 /                                                                 01190000
00120  01  INPUT-IRDX-RECORD.                                           01200000
00121      05  FILLER                            PIC X(100).            01210000
00122      COPY GCTIRDX3.                                               01220000
00123 /                                                                 01230000
00124  01  INPUT-IRIC-RECORD.                                           01240000
00125      05  FILLER                            PIC X(100).            01250000
00126      COPY GCTIRIC3.                                               01260000
00127 /                                                                 01270000
00128  01  INPUT-IRPR-RECORD.                                           01280000
00129      05  FILLER                            PIC X(100).            01290000
00130      COPY GCTIRPR3.                                               01300000
00131 /                                                                 01310000
00132  01  INPUT-IRPV-RECORD.                                           01320000
00133      05  FILLER                            PIC X(100).            01330000
00134      COPY GCTIRPV3.                                               01340000
00135 /                                                                 01350000
00136  FD  OUTPUT-WRK-FILE                                              01360000
00137      LABEL RECORDS ARE STANDARD                                   01370000
00138      RECORDING MODE IS V                                          01380000
00139      BLOCK CONTAINS  0  RECORDS.                                  01390000
00140                                                                   01400000
      *DM9441 09/04/09 JJS  CHANGED        FOR FILE CONVERSION        * 01401006
00141  01  OUTPUT-WRK-RECORD                PIC X(31470).               01410005
00142                                                                   01420000
00143  01  GX1-REC.                                                     01430000
00144      05 FILLER                        PIC X(100).                 01440000
00145      COPY  GCTIBGRC.                                              01450000
00146 /                                                                 01460000
00147  01  GX2-REC.                                                     01470000
00148      05 FILLER                        PIC X(100).                 01480000
00149      COPY  GCTIPGNC.                                              01490000
00150 /                                                                 01500000
00151  01  GX3-REC.                                                     01510000
00152      05 FILLER                        PIC X(100).                 01520000
00153      COPY  GCTIPGTC.                                              01530000
00154 /                                                                 01540000
00155  01  GXS-REC.                                                     01550000
00156      05 FILLER                        PIC X(100).                 01560000
00157      COPY  GCTIPGSC.                                              01570000
00158 /                                                                 01580000
00159  01  GX9-REC.                                                     01590000
00160      05 FILLER                        PIC X(100).                 01600000
00161      COPY  GCTIDGDC.                                              01610000
00162 /                                                                 01620000
00163  01  GXA-REC.                                                     01630000
00164      05 FILLER                        PIC X(100).                 01640000
00165      COPY  GCTIPGPC.                                              01650000
00166 /                                                                 01660000
00167  FD  OUTPUT-CDRS-WRK-FILE                                         01670000
00168      LABEL RECORDS ARE STANDARD                                   01680000
00169      RECORDING MODE IS V                                          01690000
00170      BLOCK CONTAINS  0  RECORDS.                                  01700000
00171                                                                   01710000
      *DM9441 09/04/09 JJS  CHANGED        FOR FILE CONVERSION        * 01711006
00172  01  OUTPUT-CDRS-WRK-RECORD           PIC X(31470).               01720005
00173                                                                   01730000
00174  01  GXG-REC.                                                     01740000
00175      05 FILLER                        PIC X(100).                 01750000
00176      COPY  GCTIRDXC.                                              01760000
00177 /                                                                 01770000
00178  01  GXH-REC.                                                     01780000
00179      05 FILLER                        PIC X(100).                 01790000
00180      COPY  GCTIRICC.                                              01800000
00181 /                                                                 01810000
00182  01  GXI-REC.                                                     01820000
00183      05 FILLER                        PIC X(100).                 01830000
00184      COPY  GCTIRPRC.                                              01840000
00185 /                                                                 01850000
00186  01  GXJ-REC.                                                     01860000
00187      05 FILLER                        PIC X(100).                 01870000
00188      COPY  GCTIRPVC.                                              01880000
00189 /                                                                 01890000
00190  WORKING-STORAGE SECTION.                                         01900000
00191  77  WS-PGM-ID                     PIC  X(24) VALUE               01910000
00192      'GC0050 WORKING STORAGE'.                                    01920000
00193  77  ABEND-CODE                    PIC 9(4) COMP.                 01930000
00194                                                                   01940000
00195  01  SWITCHES.                                                    01950000
00196      05  WS-EOF-SW                 PIC X      VALUE '0'.          01960000
00197          88  EOF                              VALUE '1'.          01970000
00198                                                                   01980000
00199  01  WORK-AREAS.                                                  01990000
00200      05  HASH-PROGRAM              PIC X(17) VALUE 'GHS1BAT'.     02000000
00201                                                                   02010000
00202 **** GHS1BAT LINKAGE AREA                                         02020000
00203                                                                   02030000
00204  01  WS-GHS1BAT-CALL-AREA.                                        02040000
00205    03  WS-GHS1BAT-PROCESS-IND      PIC X.                         02050000
00206        88  CALL-FOR-OPEN                       VALUE 'O'.         02060000
00207        88  CALL-FOR-CLOSE                      VALUE 'C'.         02070000
00208        88  CALL-FOR-PROCESS                    VALUE 'P'.         02080000
00209                                                                   02090000
00210  01  ACCUM-SLOT-AREA.                                             02100000
00211      05  HASH-RETURN-CODE          PIC X(02).                     02110000
00212      05  ASUR-REC-AREA.                                           02120000
00213          10  ASUR-TAB-ID           PIC X(0006).                   02130000
00214          10  ASUR-SLOT             PIC S9(7)  COMP-3.             02140000
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION         *02141002
00215          10  FILLER                PIC X(31360).                  02150001
00216                                                                   02160000
00217 ****                                                              02170000
00218 /                                                                 02180000
00219  01  WS-IBGR.                                                     02190000
00220      05  FILLER                            PIC X(100).            02200000
00221      COPY GCTIBGR2.                                               02210000
00222 /                                                                 02220000
00223  01  WS-IPGN.                                                     02230000
00224      05  FILLER                            PIC X(100).            02240000
00225      COPY GCTIPGN2.                                               02250000
00226 /                                                                 02260000
00227  01  WS-IPGT.                                                     02270000
00228      05  FILLER                            PIC X(100).            02280000
00229      COPY GCTIPGT2.                                               02290000
00230 /                                                                 02300000
00231  01  WS-IPGS.                                                     02310000
00232      05  FILLER                            PIC X(100).            02320000
00233      COPY GCTIPGS2.                                               02330000
00234 /                                                                 02340000
00235  01  WS-IDGD.                                                     02350000
00236      05  FILLER                            PIC X(100).            02360000
00237      COPY GCTIDGD2.                                               02370000
00238 /                                                                 02380000
00239  01  WS-IPGP.                                                     02390000
00240      05  FILLER                            PIC X(100).            02400000
00241      COPY GCTIPGP2.                                               02410000
00242 /                                                                 02420000
00243  01  WS-IRDX.                                                     02430000
00244      05  FILLER                            PIC X(100).            02440000
00245      COPY GCTIRDX2.                                               02450000
00246 /                                                                 02460000
00247  01  WS-IRIC.                                                     02470000
00248      05  FILLER                            PIC X(100).            02480000
00249      COPY GCTIRIC2.                                               02490000
00250 /                                                                 02500000
00251  01  WS-IRPR.                                                     02510000
00252      05  FILLER                            PIC X(100).            02520000
00253      COPY GCTIRPR2.                                               02530000
00254 /                                                                 02540000
00255  01  WS-IRPV.                                                     02550000
00256      05  FILLER                            PIC X(100).            02560000
00257      COPY GCTIRPV2.                                               02570000
00258 /                                                                 02580000
00259  01  WS-GCPS-RECORD-LENGTHS.                                      02590000
00260      COPY GCCDRLEN.                                               02600000
00261 /                                                                 02610000
00262  PROCEDURE DIVISION.                                              02620000
00263                                                                   02630000
00264  0000-BEGIN.                                                      02640000
00265                                                                   02650000
00266      PERFORM R100-OPEN THRU R100-EXIT.                            02660000
00267      PERFORM R200-READ THRU R200-EXIT.                            02670000
00268                                                                   02680000
00269      IF EOF                                                       02690000
00270          DISPLAY 'GC0050--NO INPUT RECORDS RECEIVED'              02700000
00271      ELSE                                                         02710000
00272          PERFORM 1000-PROCESS THRU 1000-EXIT                      02720000
00273             UNTIL EOF.                                            02730000
00274                                                                   02740000
00275      PERFORM R300-CLOSE   THRU R300-EXIT.                         02750000
00276      STOP RUN.                                                    02760000
00277                                                                   02770000
00278  0000-EXIT.                                                       02780000
00279      EXIT.                                                        02790000
00280 /                                                                 02800000
00281  1000-PROCESS.                                                    02810000
00282                                                                   02820000
00283                                                                   02830000
00284      IF INPUT-WRK-TAB-REC-ID = '#IBGR '                           02840000
00285          PERFORM 1100-PROCESS-IBGR THRU 1100-EXIT                 02850000
00286          PERFORM R200-READ         THRU R200-EXIT                 02860000
00287          GO TO 1000-EXIT.                                         02870000
00288                                                                   02880000
00289      IF INPUT-WRK-TAB-REC-ID = '#IPGN '                           02890000
00290          PERFORM 1200-PROCESS-IPGN THRU 1200-EXIT                 02900000
00291          PERFORM R200-READ         THRU R200-EXIT                 02910000
00292          GO TO 1000-EXIT.                                         02920000
00293                                                                   02930000
00294      IF INPUT-WRK-TAB-REC-ID = '#IPGT '                           02940000
00295          PERFORM 1300-PROCESS-IPGT THRU 1300-EXIT                 02950000
00296          PERFORM R200-READ         THRU R200-EXIT                 02960000
00297          GO TO 1000-EXIT.                                         02970000
00298                                                                   02980000
00299      IF INPUT-WRK-TAB-REC-ID = '#IDGD '                           02990000
00300          PERFORM 1400-PROCESS-IDGD THRU 1400-EXIT                 03000000
00301          PERFORM R200-READ         THRU R200-EXIT                 03010000
00302          GO TO 1000-EXIT.                                         03020000
00303                                                                   03030000
00304      IF INPUT-WRK-TAB-REC-ID = '#IPGP '                           03040000
00305          PERFORM 1500-PROCESS-IPGP THRU 1500-EXIT                 03050000
00306          PERFORM R200-READ         THRU R200-EXIT                 03060000
00307          GO TO 1000-EXIT.                                         03070000
00308                                                                   03080000
00309      IF INPUT-WRK-TAB-REC-ID = '#IPGS '                           03090000
00310          PERFORM 1600-PROCESS-IPGS THRU 1600-EXIT                 03100000
00311          PERFORM R200-READ         THRU R200-EXIT                 03110000
00312          GO TO 1000-EXIT.                                         03120000
00313                                                                   03130000
00314 *LOLO-1                                                           03140000
00315      IF INPUT-WRK-TAB-REC-ID = '#IRDX '                           03150000
00316          PERFORM 1700-PROCESS-IRDX THRU 1700-EXIT                 03160000
00317          PERFORM R200-READ         THRU R200-EXIT                 03170000
00318          GO TO 1000-EXIT.                                         03180000
00319                                                                   03190000
00320      IF INPUT-WRK-TAB-REC-ID = '#IRIC '                           03200000
00321          PERFORM 1800-PROCESS-IRIC THRU 1800-EXIT                 03210000
00322          PERFORM R200-READ         THRU R200-EXIT                 03220000
00323          GO TO 1000-EXIT.                                         03230000
00324                                                                   03240000
00325      IF INPUT-WRK-TAB-REC-ID = '#IRPR '                           03250000
00326          PERFORM 1900-PROCESS-IRPR THRU 1900-EXIT                 03260000
00327          PERFORM R200-READ         THRU R200-EXIT                 03270000
00328          GO TO 1000-EXIT.                                         03280000
00329                                                                   03290000
00330 *LOLO-2                                                           03300000
00331      IF INPUT-WRK-TAB-REC-ID = '#IRPV '                           03310000
00332          PERFORM 2000-PROCESS-IRPV THRU 2000-EXIT                 03320000
00333          PERFORM R200-READ         THRU R200-EXIT                 03330000
00334          GO TO 1000-EXIT.                                         03340000
00335                                                                   03350000
00336  1000-EXIT.                                                       03360000
00337      EXIT.                                                        03370000
00338 /                                                                 03380000
00339  1100-PROCESS-IBGR.                                               03390000
00340                                                                   03400000
00341                                                                   03410000
00342 *********************************************************         03420000
00343 ***** IF SLOT IS LESS THAN 9,000,000                *****         03430000
00344 ***** BYPASS THIS RECORD.                           *****         03440000
00345 *********************************************************         03450000
00346                                                                   03460000
00347      IF INPUT-WRK-TAB-REC-SLOT NOT > +8999999                     03470000
00348          GO TO 1100-EXIT.                                         03480000
00349                                                                   03490000
00350 *********************************************************         03500000
00351 ***** CALL TABULAR HASHING PROGRAM                  *****         03510000
00352 ***** TO RETREIVE EXISTING OR NEW SLOT NUMBER.      *****         03520000
00353 *********************************************************         03530000
00354                                                                   03540000
00355      MOVE LOW-VALUES TO ASUR-REC-AREA.                            03550000
00356      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              03560000
00357      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          03570000
00358      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 03580000
00359                              ACCUM-SLOT-AREA.                     03590000
00360                                                                   03600000
00361 *********************************************************         03610000
00362 ***** MARK RECORD AS 'EXISTING' OR 'NEW'.           *****         03620000
00363 *********************************************************         03630000
00364                                                                   03640000
00365      IF HASH-RETURN-CODE = '00'                                   03650000
00366         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  03660000
00367      ELSE                                                         03670000
00368         IF HASH-RETURN-CODE = '01'                                03680000
00369            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  03690000
00370         ELSE                                                      03700000
00371            GO TO 1100-EXIT.                                       03710000
00372                                                                   03720000
00373                                                                   03730000
00374 *********************************************************         03740000
00375 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO      *****         03750000
00376 ***** THE SEQUENTIAL FILE.                          *****         03760000
00377 *********************************************************         03770000
00378                                                                   03780000
00379      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         03790000
00380      MOVE INPUT-WRK-TAB-REC-CNT     TO GX1-ENTRY-COUNT.           03800000
00381      MOVE INPUT-IBGR-RECORD         TO OUTPUT-WRK-RECORD.         03810000
00382      MOVE ASUR-SLOT                 TO GX1-PROVISION-SLOT-NO.     03820000
00383      WRITE GX1-REC.                                               03830000
00384                                                                   03840000
00385  1100-EXIT.                                                       03850000
00386      EXIT.                                                        03860000
00387 /                                                                 03870000
00388  1200-PROCESS-IPGN.                                               03880000
00389                                                                   03890000
00390                                                                   03900000
00391 *********************************************************         03910000
00392 ***** IF SLOT IS LESS THAN 9,000,000                *****         03920000
00393 ***** BYPASS THIS RECORD.                           *****         03930000
00394 *********************************************************         03940000
00395                                                                   03950000
00396      IF INPUT-WRK-TAB-REC-SLOT NOT > +8999999                     03960000
00397         GO TO 1200-EXIT.                                          03970000
00398                                                                   03980000
00399 *********************************************************         03990000
00400 ***** IF THIS IS A CHAINED TABULAR RECORD           *****         04000000
00401 ***** MARK AND WRITE THE RECORD AS BEING EXISTING.  *****         04010000
00402 *********************************************************         04020000
00403                                                                   04030000
00404      IF  WRK-PROV-POOL-COPY-SLOT > 99999                          04040000
00405      AND WRK-PROV-POOL-COPY-SLOT < 7000000                        04050000
00406          MOVE LOW-VALUES              TO OUTPUT-WRK-RECORD        04060000
00407          MOVE INPUT-WRK-TAB-REC-CNT   TO GX2-ENTRY-COUNT          04070000
00408          MOVE 'E'                     TO WRK-SIGNAL-BATCH-INTERNAL04080000
00409          MOVE INPUT-IPGN-RECORD       TO OUTPUT-WRK-RECORD        04090000
00410          MOVE WRK-PROV-POOL-COPY-SLOT TO GX2-PROVISION-SLOT-NO    04100000
00411          WRITE GX2-REC                                            04110000
00412          GO TO 1200-EXIT.                                         04120000
00413                                                                   04130000
00414 *********************************************************         04140000
00415 ***** IF THIS IS NOT A CHAINED TABULAR RECORD       *****         04150000
00416 ***** CALL TABULAR HASHING PROGRAM                  *****         04160000
00417 ***** TO RETREIVE EXISTING OR NEW SLOT NUMBER.      *****         04170000
00418 *********************************************************         04180000
00419                                                                   04190000
00420      MOVE LOW-VALUES TO ASUR-REC-AREA.                            04200000
00421      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              04210000
00422      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          04220000
00423      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 04230000
00424                              ACCUM-SLOT-AREA.                     04240000
00425                                                                   04250000
00426 *********************************************************         04260000
00427 ***** MARK RECORD AS 'EXISTING' OR 'NEW'.           *****         04270000
00428 *********************************************************         04280000
00429                                                                   04290000
00430      IF HASH-RETURN-CODE = '00'                                   04300000
00431         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  04310000
00432      ELSE                                                         04320000
00433         IF HASH-RETURN-CODE = '01'                                04330000
00434            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  04340000
00435         ELSE                                                      04350000
00436            GO TO 1200-EXIT.                                       04360000
00437                                                                   04370000
00438                                                                   04380000
00439 *********************************************************         04390000
00440 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO      *****         04400000
00441 ***** THE SEQUENTIAL FILE.                          *****         04410000
00442 *********************************************************         04420000
00443                                                                   04430000
00444      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         04440000
00445      MOVE INPUT-WRK-TAB-REC-CNT     TO GX2-ENTRY-COUNT.           04450000
00446      MOVE INPUT-IPGN-RECORD         TO OUTPUT-WRK-RECORD.         04460000
00447      MOVE ASUR-SLOT                 TO GX2-PROVISION-SLOT-NO.     04470000
00448      WRITE GX2-REC.                                               04480000
00449                                                                   04490000
00450  1200-EXIT.                                                       04500000
00451      EXIT.                                                        04510000
00452 /                                                                 04520000
00453  1300-PROCESS-IPGT.                                               04530000
00454                                                                   04540000
00455                                                                   04550000
00456 *********************************************************         04560000
00457 ***** IF SLOT IS LESS THAN 9,000,000                *****         04570000
00458 ***** BYPASS THIS RECORD.                           *****         04580000
00459 *********************************************************         04590000
00460                                                                   04600000
00461      IF INPUT-WRK-TAB-REC-SLOT NOT > +8999999                     04610000
00462          GO TO 1300-EXIT.                                         04620000
00463                                                                   04630000
00464 *********************************************************         04640000
00465 ***** CALL TABULAR HASHING PROGRAM                  *****         04650000
00466 ***** TO RETREIVE EXISTING OR NEW SLOT NUMBER.      *****         04660000
00467 *********************************************************         04670000
00468                                                                   04680000
00469      MOVE LOW-VALUES TO ASUR-REC-AREA.                            04690000
00470      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              04700000
00471      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          04710000
00472      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 04720000
00473                              ACCUM-SLOT-AREA.                     04730000
00474                                                                   04740000
00475 *********************************************************         04750000
00476 ***** MARK RECORD AS 'EXISTING' OR 'NEW'.           *****         04760000
00477 *********************************************************         04770000
00478                                                                   04780000
00479      IF HASH-RETURN-CODE = '00'                                   04790000
00480         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  04800000
00481      ELSE                                                         04810000
00482         IF HASH-RETURN-CODE = '01'                                04820000
00483            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  04830000
00484         ELSE                                                      04840000
00485            GO TO 1300-EXIT.                                       04850000
00486                                                                   04860000
00487                                                                   04870000
00488 *********************************************************         04880000
00489 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO      *****         04890000
00490 ***** THE SEQUENTIAL FILE.                          *****         04900000
00491 *********************************************************         04910000
00492                                                                   04920000
00493      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         04930000
00494      MOVE INPUT-WRK-TAB-REC-CNT     TO GX3-ENTRY-COUNT.           04940000
00495      MOVE INPUT-IPGT-RECORD         TO OUTPUT-WRK-RECORD.         04950000
00496      MOVE ASUR-SLOT                 TO GX3-PROVISION-SLOT-NO.     04960000
00497      WRITE GX3-REC.                                               04970000
00498                                                                   04980000
00499  1300-EXIT.                                                       04990000
00500      EXIT.                                                        05000000
00501 /                                                                 05010000
00502  1400-PROCESS-IDGD.                                               05020000
00503                                                                   05030000
00504                                                                   05040000
00505 *********************************************************         05050000
00506 ***** IF SLOT IS LESS THAN 9,000,000                *****         05060000
00507 ***** BYPASS THIS RECORD.                           *****         05070000
00508 *********************************************************         05080000
00509                                                                   05090000
00510      IF INPUT-WRK-TAB-REC-SLOT NOT > +8999999                     05100000
00511          GO TO 1400-EXIT.                                         05110000
00512                                                                   05120000
00513 *********************************************************         05130000
00514 ***** CALL TABULAR HASHING PROGRAM                  *****         05140000
00515 ***** TO RETREIVE EXISTING OR NEW SLOT NUMBER.      *****         05150000
00516 *********************************************************         05160000
00517                                                                   05170000
00518      MOVE LOW-VALUES TO ASUR-REC-AREA.                            05180000
00519      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              05190000
00520      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          05200000
00521      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 05210000
00522                              ACCUM-SLOT-AREA.                     05220000
00523                                                                   05230000
00524 *********************************************************         05240000
00525 ***** MARK RECORD AS 'EXISTING' OR 'NEW'.           *****         05250000
00526 *********************************************************         05260000
00527                                                                   05270000
00528      IF HASH-RETURN-CODE = '00'                                   05280000
00529         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  05290000
00530      ELSE                                                         05300000
00531         IF HASH-RETURN-CODE = '01'                                05310000
00532            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  05320000
00533         ELSE                                                      05330000
00534            GO TO 1400-EXIT.                                       05340000
00535                                                                   05350000
00536                                                                   05360000
00537 *********************************************************         05370000
00538 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO      *****         05380000
00539 ***** THE SEQUENTIAL FILE.                          *****         05390000
00540 *********************************************************         05400000
00541                                                                   05410000
00542      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         05420000
00543      MOVE INPUT-WRK-TAB-REC-CNT     TO GX9-ENTRY-COUNT.           05430000
00544      MOVE INPUT-IDGD-RECORD         TO OUTPUT-WRK-RECORD.         05440000
00545      MOVE ASUR-SLOT                 TO GX9-PROVISION-SLOT-NO.     05450000
00546      WRITE GX9-REC.                                               05460000
00547                                                                   05470000
00548  1400-EXIT.                                                       05480000
00549      EXIT.                                                        05490000
00550 /                                                                 05500000
00551  1500-PROCESS-IPGP.                                               05510000
00552                                                                   05520000
00553                                                                   05530000
00554 *********************************************************         05540000
00555 ***** IF SLOT IS LESS THAN 9,000,000                *****         05550000
00556 ***** BYPASS THIS RECORD.                           *****         05560000
00557 *********************************************************         05570000
00558                                                                   05580000
00559      IF INPUT-WRK-TAB-REC-SLOT NOT > +8999999                     05590000
00560          GO TO 1500-EXIT.                                         05600000
00561                                                                   05610000
00562 *********************************************************         05620000
00563 ***** CALL TABULAR HASHING PROGRAM                  *****         05630000
00564 ***** TO RETREIVE EXISTING OR NEW SLOT NUMBER.      *****         05640000
00565 *********************************************************         05650000
00566                                                                   05660000
00567      MOVE LOW-VALUES TO ASUR-REC-AREA.                            05670000
00568      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              05680000
00569      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          05690000
00570      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 05700000
00571                              ACCUM-SLOT-AREA.                     05710000
00572                                                                   05720000
00573 *********************************************************         05730000
00574 ***** MARK RECORD AS 'EXISTING' OR 'NEW'.           *****         05740000
00575 *********************************************************         05750000
00576                                                                   05760000
00577      IF HASH-RETURN-CODE = '00'                                   05770000
00578         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  05780000
00579      ELSE                                                         05790000
00580         IF HASH-RETURN-CODE = '01'                                05800000
00581            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  05810000
00582         ELSE                                                      05820000
00583            GO TO 1500-EXIT.                                       05830000
00584                                                                   05840000
00585                                                                   05850000
00586 *********************************************************         05860000
00587 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO      *****         05870000
00588 ***** THE SEQUENTIAL FILE.                          *****         05880000
00589 *********************************************************         05890000
00590                                                                   05900000
00591      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         05910000
00592      MOVE INPUT-WRK-TAB-REC-CNT     TO GXA-ENTRY-COUNT.           05920000
00593      MOVE INPUT-IPGP-RECORD         TO OUTPUT-WRK-RECORD.         05930000
00594      MOVE ASUR-SLOT                 TO GXA-PROVISION-SLOT-NO.     05940000
00595      WRITE GXA-REC.                                               05950000
00596                                                                   05960000
00597  1500-EXIT.                                                       05970000
00598      EXIT.                                                        05980000
00599 /                                                                 05990000
00600  1600-PROCESS-IPGS.                                               06000000
00601                                                                   06010000
00602                                                                   06020000
00603 *********************************************************         06030000
00604 ***** IF SLOT IS LESS THAN 9,000,000                *****         06040000
00605 ***** BYPASS THIS RECORD.                           *****         06050000
00606 *********************************************************         06060000
00607                                                                   06070000
00608      IF INPUT-WRK-TAB-REC-SLOT NOT > +8999999                     06080000
00609          GO TO 1600-EXIT.                                         06090000
00610                                                                   06100000
00611 *********************************************************         06110000
00612 ***** CALL TABULAR HASHING PROGRAM                  *****         06120000
00613 ***** TO RETREIVE EXISTING OR NEW SLOT NUMBER.      *****         06130000
00614 *********************************************************         06140000
00615                                                                   06150000
00616      MOVE LOW-VALUES TO ASUR-REC-AREA.                            06160000
00617      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              06170000
00618      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          06180000
00619      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 06190000
00620                              ACCUM-SLOT-AREA.                     06200000
00621                                                                   06210000
00622 *********************************************************         06220000
00623 ***** MARK RECORD AS 'EXISTING' OR 'NEW'.           *****         06230000
00624 *********************************************************         06240000
00625                                                                   06250000
00626      IF HASH-RETURN-CODE = '00'                                   06260000
00627         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  06270000
00628      ELSE                                                         06280000
00629         IF HASH-RETURN-CODE = '01'                                06290000
00630            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  06300000
00631         ELSE                                                      06310000
00632            GO TO 1600-EXIT.                                       06320000
00633                                                                   06330000
00634                                                                   06340000
00635 *********************************************************         06350000
00636 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO      *****         06360000
00637 ***** THE SEQUENTIAL FILE.                          *****         06370000
00638 *********************************************************         06380000
00639                                                                   06390000
00640      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         06400000
00641      MOVE INPUT-WRK-TAB-REC-CNT     TO GXS-ENTRY-COUNT.           06410000
00642      MOVE INPUT-IPGS-RECORD         TO OUTPUT-WRK-RECORD.         06420000
00643      MOVE ASUR-SLOT                 TO GXS-PROVISION-SLOT-NO.     06430000
00644      WRITE GXS-REC.                                               06440000
00645                                                                   06450000
00646  1600-EXIT.                                                       06460000
00647      EXIT.                                                        06470000
00648 *LOLO-3                                                           06480000
00649  1700-PROCESS-IRDX.                                               06490000
00650                                                                   06500000
00651                                                                   06510000
00652 *********************************************************         06520000
00653 ***** IF SLOT IS LESS THAN 9,000,000                *****         06530000
00654 ***** BYPASS THIS RECORD.                           *****         06540000
00655 *********************************************************         06550000
00656 *THE NOT > 9 MILL WAS COMMENTED OUT AND THE < 11 WAS CODED        06560000
00657 *IT IS +9000001, 2, 3                                             06570000
00658                                                                   06580000
00659      IF INPUT-WRK-TAB-REC-SLOT NOT > +8999999                     06590000
00660          GO TO 1700-EXIT.                                         06600000
00661                                                                   06610000
00662 *    IF INPUT-WRK-TAB-REC-SLOT  < +11                             06620000
00663 *        CONTINUE                                                 06630000
00664 *    ELSE                                                         06640000
00665 *        GO TO 1700-EXIT.                                         06650000
00666                                                                   06660000
00667 *********************************************************         06670000
00668 ***** CALL TABULAR HASHING PROGRAM                  *****         06680000
00669 ***** TO RETREIVE EXISTING OR NEW SLOT NUMBER.      *****         06690000
00670 *********************************************************         06700000
00671 *01  ACCUM-SLOT-AREA.                                             06710000
00672 *    05  HASH-RETURN-CODE          PIC X(02).                     06720000
00673 *XXX 05  ASUR-REC-AREA.                                           06730000
00674 *        10  ASUR-TAB-ID           PIC X(0006).                   06740000
00675 *        10  ASUR-SLOT             PIC S9(7)  COMP-3.             06750000
00676 *        10  FILLER                PIC X(7795).                   06760002
00677                                                                   06770000
00678                                                                   06780000
00679      MOVE  LOW-VALUES                TO  ASUR-REC-AREA.           06790000
00680      MOVE  INPUT-WRK-TABULAR-RECORD  TO  ASUR-REC-AREA.           06800000
00681                                                                   06810000
00682      MOVE  'P'              TO  WS-GHS1BAT-PROCESS-IND.           06820000
00683      CALL  HASH-PROGRAM  USING  WS-GHS1BAT-CALL-AREA              06830000
00684                                 ACCUM-SLOT-AREA.                  06840000
00685                                                                   06850000
00686 *********************************************************         06860000
00687 ***** MARK RECORD AS 'EXISTING' OR 'NEW'.           *****         06870000
00688 *********************************************************         06880000
00689                                                                   06890000
00690      IF HASH-RETURN-CODE = '00'                                   06900000
00691         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  06910000
00692      ELSE                                                         06920000
00693         IF HASH-RETURN-CODE = '01'                                06930000
00694            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  06940000
00695         ELSE                                                      06950000
00696            GO TO 1700-EXIT.                                       06960000
00697                                                                   06970000
00698                                                                   06980000
00699 **KOO****************************************************         06990000
00700 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO      *****         07000000
00701 ***** THE SEQUENTIAL FILE.                          *****         07010000
00702 *********************************************************         07020000
00703 *01  OUTPUT-CDRS-WRK-RECORD           PIC X(7905).                07030002
00704                                                                   07040000
00705 *        10  INPUT-WRK-COMPARE.                                   07050000
00706 * XXXX       15  INPUT-WRK-TAB-REC-CNT     PIC S9(5) COMP-3.      07060000
00707 *            15  INPUT-WRK-ENTRIES         PIC X(7765).           07070002
00708                                                                   07080000
00709      MOVE LOW-VALUES                TO OUTPUT-CDRS-WRK-RECORD.    07090000
00710      MOVE INPUT-WRK-TAB-REC-CNT     TO GXG-ENTRY-COUNT.           07100000
00711      MOVE INPUT-IRDX-RECORD         TO OUTPUT-CDRS-WRK-RECORD.    07110000
00712      MOVE ASUR-SLOT                 TO GXG-PROVISION-SLOT-NO.     07120000
00713      WRITE GXG-REC.                                               07130000
00714                                                                   07140000
00715  1700-EXIT.                                                       07150000
00716      EXIT.                                                        07160000
00717 /                                                                 07170000
00718  1800-PROCESS-IRIC.                                               07180000
00719                                                                   07190000
00720 *********************************************************         07200000
00721 ***** IF SLOT IS LESS THAN 9,000,000                *****         07210000
00722 ***** BYPASS THIS RECORD.                           *****         07220000
00723 *********************************************************         07230000
00724                                                                   07240000
00725 *    IF INPUT-WRK-TAB-REC-SLOT NOT > +8999999                     07250000
00726 *        GO TO 1800-EXIT.                                         07260000
00727      IF INPUT-WRK-TAB-REC-SLOT  < +11                             07270000
00728          CONTINUE                                                 07280000
00729      ELSE                                                         07290000
00730          GO TO 1700-EXIT.                                         07300000
00731                                                                   07310000
00732 *********************************************************         07320000
00733 ***** CALL TABULAR HASHING PROGRAM                  *****         07330000
00734 ***** TO RETREIVE EXISTING OR NEW SLOT NUMBER.      *****         07340000
00735 *********************************************************         07350000
00736                                                                   07360000
00737      MOVE LOW-VALUES TO ASUR-REC-AREA.                            07370000
00738      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              07380000
00739      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          07390000
00740      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 07400000
00741                              ACCUM-SLOT-AREA.                     07410000
00742                                                                   07420000
00743 *********************************************************         07430000
00744 ***** MARK RECORD AS 'EXISTING' OR 'NEW'.           *****         07440000
00745 *********************************************************         07450000
00746                                                                   07460000
00747      IF HASH-RETURN-CODE = '00'                                   07470000
00748         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  07480000
00749      ELSE                                                         07490000
00750         IF HASH-RETURN-CODE = '01'                                07500000
00751            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  07510000
00752         ELSE                                                      07520000
00753            GO TO 1800-EXIT.                                       07530000
00754                                                                   07540000
00755                                                                   07550000
00756 *********************************************************         07560000
00757 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO      *****         07570000
00758 ***** THE SEQUENTIAL FILE.                          *****         07580000
00759 *********************************************************         07590000
00760                                                                   07600000
00761      MOVE LOW-VALUES                TO OUTPUT-CDRS-WRK-RECORD.    07610000
00762      MOVE INPUT-WRK-TAB-REC-CNT     TO GXH-ENTRY-COUNT.           07620000
00763      MOVE INPUT-IRIC-RECORD         TO OUTPUT-CDRS-WRK-RECORD.    07630000
00764      MOVE ASUR-SLOT                 TO GXH-PROVISION-SLOT-NO.     07640000
00765      WRITE GXH-REC.                                               07650000
00766                                                                   07660000
00767  1800-EXIT.                                                       07670000
00768      EXIT.                                                        07680000
00769 /                                                                 07690000
00770  1900-PROCESS-IRPR.                                               07700000
00771                                                                   07710000
00772                                                                   07720000
00773 *********************************************************         07730000
00774 ***** IF SLOT IS LESS THAN 9,000,000                *****         07740000
00775 ***** BYPASS THIS RECORD.                           *****         07750000
00776 *********************************************************         07760000
00777                                                                   07770000
00778      IF INPUT-WRK-TAB-REC-SLOT NOT > +8999999                     07780000
00779          GO TO 1900-EXIT.                                         07790000
00780      IF INPUT-WRK-TAB-REC-SLOT  < +11                             07800000
00781          CONTINUE                                                 07810000
00782      ELSE                                                         07820000
00783          GO TO 1700-EXIT.                                         07830000
00784                                                                   07840000
00785 *********************************************************         07850000
00786 ***** CALL TABULAR HASHING PROGRAM                  *****         07860000
00787 ***** TO RETREIVE EXISTING OR NEW SLOT NUMBER.      *****         07870000
00788 *********************************************************         07880000
00789                                                                   07890000
00790      MOVE LOW-VALUES TO ASUR-REC-AREA.                            07900000
00791      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              07910000
00792      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          07920000
00793      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 07930000
00794                              ACCUM-SLOT-AREA.                     07940000
00795                                                                   07950000
00796 *********************************************************         07960000
00797 ***** MARK RECORD AS 'EXISTING' OR 'NEW'.           *****         07970000
00798 *********************************************************         07980000
00799                                                                   07990000
00800      IF HASH-RETURN-CODE = '00'                                   08000000
00801         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  08010000
00802      ELSE                                                         08020000
00803         IF HASH-RETURN-CODE = '01'                                08030000
00804            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  08040000
00805         ELSE                                                      08050000
00806            GO TO 1900-EXIT.                                       08060000
00807                                                                   08070000
00808                                                                   08080000
00809 *********************************************************         08090000
00810 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO      *****         08100000
00811 ***** THE SEQUENTIAL FILE.                          *****         08110000
00812 *********************************************************         08120000
00813                                                                   08130000
00814      MOVE LOW-VALUES                TO OUTPUT-CDRS-WRK-RECORD.    08140000
00815      MOVE INPUT-WRK-TAB-REC-CNT     TO GXI-ENTRY-COUNT.           08150000
00816      MOVE INPUT-IRPR-RECORD         TO OUTPUT-CDRS-WRK-RECORD.    08160000
00817      MOVE ASUR-SLOT                 TO GXI-PROVISION-SLOT-NO.     08170000
00818      WRITE GXI-REC.                                               08180000
00819                                                                   08190000
00820  1900-EXIT.                                                       08200000
00821      EXIT.                                                        08210000
00822 /                                                                 08220000
00823 *LOLO-4                                                           08230000
00824  2000-PROCESS-IRPV.                                               08240000
00825                                                                   08250000
00826                                                                   08260000
00827 *********************************************************         08270000
00828 ***** IF SLOT IS LESS THAN 9,000,000                *****         08280000
00829 ***** BYPASS THIS RECORD.                           *****         08290000
00830 *********************************************************         08300000
00831                                                                   08310000
00832      IF INPUT-WRK-TAB-REC-SLOT NOT > +8999999                     08320000
00833          GO TO 2000-EXIT.                                         08330000
00834                                                                   08340000
00835 *    IF INPUT-WRK-TAB-REC-SLOT  < +11                             08350000
00836 *        CONTINUE                                                 08360000
00837 *    ELSE                                                         08370000
00838 *        GO TO 1700-EXIT.                                         08380000
00839                                                                   08390000
00840 *********************************************************         08400000
00841 ***** CALL TABULAR HASHING PROGRAM                  *****         08410000
00842 ***** TO RETREIVE EXISTING OR NEW SLOT NUMBER.      *****         08420000
00843 *********************************************************         08430000
00844                                                                   08440000
00845      MOVE LOW-VALUES TO ASUR-REC-AREA.                            08450000
00846      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              08460000
00847      MOVE 'P' TO WS-GHS1BAT-PROCESS-IND.                          08470000
00848      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA                 08480000
00849                              ACCUM-SLOT-AREA.                     08490000
00850                                                                   08500000
00851 *********************************************************         08510000
00852 ***** MARK RECORD AS 'EXISTING' OR 'NEW'.           *****         08520000
00853 *********************************************************         08530000
00854                                                                   08540000
00855      IF HASH-RETURN-CODE = '00'                                   08550000
00856         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  08560000
00857      ELSE                                                         08570000
00858         IF HASH-RETURN-CODE = '01'                                08580000
00859            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  08590000
00860         ELSE                                                      08600000
00861            GO TO 2000-EXIT.                                       08610000
00862                                                                   08620000
00863                                                                   08630000
00864 *********************************************************         08640000
00865 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO      *****         08650000
00866 ***** THE SEQUENTIAL FILE.                          *****         08660000
00867 *********************************************************         08670000
00868                                                                   08680000
00869      MOVE LOW-VALUES                TO OUTPUT-CDRS-WRK-RECORD.    08690000
00870      MOVE INPUT-WRK-TAB-REC-CNT     TO GXJ-ENTRY-COUNT.           08700000
00871      MOVE INPUT-IRPV-RECORD         TO OUTPUT-CDRS-WRK-RECORD.    08710000
00872      MOVE ASUR-SLOT                 TO GXJ-PROVISION-SLOT-NO.     08720000
00873      WRITE GXJ-REC.                                               08730000
00874                                                                   08740000
00875  2000-EXIT.                                                       08750000
00876      EXIT.                                                        08760000
00877 /                                                                 08770000
00878  R100-OPEN.                                                       08780000
00879                                                                   08790000
00880      OPEN INPUT  INPUT-WRK-FILE,                                  08800000
00881           OUTPUT OUTPUT-WRK-FILE                                  08810000
00882           OUTPUT OUTPUT-CDRS-WRK-FILE.                            08820000
00883                                                                   08830000
00884      MOVE 'O'    TO WS-GHS1BAT-PROCESS-IND.                       08840000
00885      CALL HASH-PROGRAM  USING WS-GHS1BAT-CALL-AREA.               08850000
00886                                                                   08860000
00887  R100-EXIT.                                                       08870000
00888      EXIT.                                                        08880000
00889                                                                   08890000
00890  R200-READ.                                                       08900000
00891                                                                   08910000
00892      READ INPUT-WRK-FILE AT END                                   08920000
00893          MOVE '1' TO WS-EOF-SW.                                   08930000
00894                                                                   08940000
00895  R200-EXIT.                                                       08950000
00896      EXIT.                                                        08960000
00897                                                                   08970000
00898  R300-CLOSE.                                                      08980000
00899                                                                   08990000
00900      CLOSE INPUT-WRK-FILE,                                        09000000
00901            OUTPUT-WRK-FILE                                        09010000
00902            OUTPUT-CDRS-WRK-FILE.                                  09020000
00903                                                                   09030000
00904      MOVE 'C'    TO WS-GHS1BAT-PROCESS-IND.                       09040000
00905      CALL HASH-PROGRAM  USING WS-GHS1BAT-CALL-AREA.               09050000
00906                                                                   09060000
00907  R300-EXIT.                                                       09070000
00908      EXIT.                                                        09080000
