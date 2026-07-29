00001  IDENTIFICATION DIVISION.                                         00010000
00002  PROGRAM-ID. GC0076.                                              00020000
00003  AUTHOR.  ED WITKUS.                                              00030000
00004  INSTALLATION.  HCSC.                                             00040000
00005  DATE-WRITTEN.  12/11/87.                                         00050000
00006  DATE-COMPILED.                                                   00060000
00007 ******************************************************************00070000
00008 ******************************************************************00080000
00009 *                                                                 00090000
00010 *   THIS PROGRAM WILL MOVE THE NON-COMPARE SECTIONS OF THE ACCUM  00100000
00011 *   RECORDS TO AN AREA OUTSIDE OF THE COMPARE AREA.               00110000
00012 *                                                                 00120000
00013 ******************************************************************00130000
00014 ******************************************************************00140000
00015 *                  U P D A T E   H I S T O R Y                   *00150000
00016 *                                                                *00160000
00017 *  NUM     DATE    BY             DESCRIPTION                    *00170000
00018 * -----  --------  ---  --------------------------------------   *00180000
00019 * ?????  06/15/88  ENW  ADDED A WRITE AFTER END OF FILE. LAST    *00190000
00020 *                       RECORD WAS NOT BEING WRITTEN; THEREFORE, *00200000
00021 *                       NO MATCH COULD OCCUR VS. THE LAST RECORD.*00210000
00022 *                                                                *00220000
00023 *                       ----ACCUM TABULAR RECORD MODIFICATION--- *00230000
00024 * 11154   10/02/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *00240000
00025 * D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *00250000
00026 * D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *00260000
00027 * D270                  4. INCREASE MAX REC LENGTH FOR ACCUM REC *00270000
00028 *                          TO 8157, AND W/F RECORD TO 8192, AS   *00280000
00029 *                          (8157 + 64 = 8221) BUT USING 8192 AS  *00290000
00030 *                          RECORDS CREATED IN ONLINE WILL NOT    *00300000
00031 *                          EXCEED TWO POINTERS.                  *00310000
00032 *                                                                *00320000
00033 * 11154    2/21/91  FRY   -DECREASE MAX OCCURS FROM 46 TO 44.    *00330000
00034 *                         -DECREASE MAX REC LENGTHS FOR:         *00340000
00035 *                            ACCUM RECORD FROM 8157 TO 7805      *00350000
00036 *                            WORK RECORD  FROM 8221 TO 7869      *00360000
00037 *                  FILE SECTION WAS CHANGED:                     *00370000
00038 *                   -OUT-REC  PIC X(8221) CHANGED TO (7869)      *00380000
00039 *                  WORKING STORAGE WAS CHANGED:                  *00390000
00040 *                   -FILLER               PIC X(8147) TO 7795    *00400000
00041 *                   -WS-OUT-COMPARE-AREA  PIC X(7728) TO 7392    *00410000
00042 *                   -CHANGED WS-OUT-NON-COMP-TAB OCCURS 46 TIMES *00420000
00043 *                         TO WS-OUT-NON-COMP-TAB OCCURS 44 TIMES *00430000
00044 *                   -CHANGED WS-OUT-COMP-TAB OCCURS 46 TIMES     *00440000
00045 *                         TO WS-OUT-COMP-TAB OCCURS 44 TIMES     *00450000
00046 ******************************************************************00460000
00047 * 11836   06/27/91  TPM    EXPAND IN-OCCURS-COMPARE AREA FROM    *00470000
00048 *                          168 TO 172 TO HANDLE \
00049 *                          ELIMINATE  FOUR BYTES FROM THE NON-   *00490000
00050 *                          COMPARE AREA TO ACCOMODATE FOR THE    *00500000
00051 *                          THE EXPANSION OF THE IN-COMPARE AREA. *00510000
00052 *                                                                *00520000
00053 *          1/10/95   EMS   CONVERTED TO COBOL II.                *00530000
00054 *                                                                *00540000
00055 *15057   09/11/97    AB   ADDED CODE TO SUPPORT THE YEAR         *00550000
00056 *                           2000 AND THE EXPANSION OF THE        *00560000
00057 *                           CONTRACT KEY TO SUPPORT THE TX       *00570000
00058 *                           MERGER.                              *00580000
00059 *                                                                *00590000
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION          00591002
00060 ******************************************************************00600000
00061 ******************************************************************00610000
00062  ENVIRONMENT DIVISION.                                            00620000
00063                                                                   00630000
00064  CONFIGURATION SECTION.                                           00640000
00065  SOURCE-COMPUTER.  IBM-370.                                       00650000
00066  OBJECT-COMPUTER.  IBM-370.                                       00660000
00067  INPUT-OUTPUT SECTION.                                            00670000
00068                                                                   00680000
00069  FILE-CONTROL.                                                    00690000
00070      SELECT IN-FILE                   ASSIGN  TO UT-S-GC0076A.    00700000
00071      SELECT OUT-FILE                  ASSIGN  TO UT-S-GC0076B.    00710000
00072  DATA DIVISION.                                                   00720000
00073                                                                   00730000
00074  FILE SECTION.                                                    00740000
00075                                                                   00750000
00076  FD  IN-FILE                                                      00760000
00077      LABEL RECORDS ARE STANDARD                                   00770000
00078      RECORDING MODE IS F                                          00780000
00079      BLOCK CONTAINS  0  RECORDS.                                  00790000
00080                                                                   00800000
00081  01  IN-REC.                                                      00810000
00082      05 IN-WORK-KEY                       PIC X(100).             00820000
00083      05 IN-FIXED-PORTION.                                         00830000
00084          10 IN-TAB-KEY.                                           00840000
00085              15 IN-TAB-ID                 PIC X(6).               00850000
00086              15 IN-TAB-SLOT               PIC S9(7) COMP-3.       00860000
00087          10 FILLER                        PIC X(27).              00870000
00088          10 IN-COUNT                      PIC S9(5) COMP-3.       00880000
00089          10 FILLER                        PIC X(21).              00890000
00090      05 IN-OCCURS.                                                00900000
00091          10  IN-OCCURS-COMPARE-AREA         PIC X(172).           00910000
00092          10  IN-OCCURS-NON-COMPARE-AREA.                          00920000
00093              15  IN-OC-NON-COMP-ENT-CNT     PIC S9(7) COMP-3.     00930000
00094                                                                   00940000
00095 /                                                                 00950000
00096  FD  OUT-FILE                                                     00960000
00097      LABEL RECORDS ARE STANDARD                                   00970000
00098      RECORDING MODE IS F                                          00980000
00099      BLOCK CONTAINS  0  RECORDS.                                  00990000
00100                                                                   01000000
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION          01001002
00101  01  OUT-REC                          PIC X(31470).               01010001
00102 /                                                                 01020000
00103  WORKING-STORAGE SECTION.                                         01030000
00104                                                                   01040000
00105  01  FILLER                        PIC  X(24)                     01050000
00106              VALUE 'GC0076 WORKING STORAGE'.                      01060000
00107                                                                   01070000
00108  01  WS-COUNTERS.                                                 01080000
00109      05  WS-READ-COUNT             PIC 9(8)       VALUE ZEROS.    01090000
00110      05  WS-WRITE-COUNT            PIC 9(8)       VALUE ZEROS.    01100000
00111                                                                   01110000
00112  01  WS-SWITCHES.                                                 01120000
00113      05 WS-EOF-SW                  PIC X          VALUE SPACES.   01130000
00114          88 EOF                                   VALUE '1'.      01140000
00115      05 1ST-TIME-SW                PIC X          VALUE SPACES.   01150000
00116          88  1ST-TIME                             VALUE SPACES.   01160000
00117                                                                   01170000
00118  01  MISC-AREA.                                                   01180000
00119      05  LAST-WORK-KEY             PIC X(100).                    01190000
00120      05  LAST-TAB-KEY.                                            01200000
00121          10  LAST-TAB-ID           PIC X(06).                     01210000
00122          10  LAST-TAB-SLOT         PIC S9(7) COMP-3 VALUE ZEROS.  01220000
00123 /                                                                 01230000
00124  01  WS-OUT-REC.                                                  01240000
00125      05  WS-OUT-WORK-KEY                PIC X(100).               01250000
00126      05  WS-OUT-FIXED-PORTION           PIC X(61).                01260000
00127      05  WS-OUT-NON-COMPARE-AREA        PIC X(176).               01270000
00128      05  WS-OUT-NON-COMPARE-TABLE                                 01280000
00129              REDEFINES WS-OUT-NON-COMPARE-AREA.                   01290000
00130          10  WS-OUT-NON-COMP-TAB OCCURS 44 TIMES                  01300000
00131                               INDEXED BY NON-COMP-INDEX.          01310000
00132              15  WS-OUT-NON-COMP-ENT-CNT PIC S9(7) COMP-3.        01320000
00133      05  WS-OUT-COMPARE-AREA            PIC X(7568).              01330000
00134      05  WS-OUT-OCCURS-TABLE REDEFINES WS-OUT-COMPARE-AREA.       01340000
00135          10  WS-OUT-COMP-TAB OCCURS 44 TIMES                      01350000
00136                               INDEXED BY COMP-INDEX.              01360000
00137              15  FILLER               PIC X(172).                 01370000
00138                                                                   01380000
00139  01  WS-LAST-SLOT-REC.                                            01390000
00140      05  FILLER                       PIC X(100).                 01400000
00141      05  WS-LAST-TAB-KEY.                                         01410000
00142          15 WS-LAST-TAB-ID            PIC X(6).                   01420000
00143          15 WS-LAST-TAB-SLOT          PIC S9(7) COMP-3.           01430000
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION          01431003
00144      05  FILLER                       PIC X(31360).               01440003
00145 /                                                                 01450000
00146  PROCEDURE DIVISION.                                              01460000
00147  0000-MAINLINE.                                                   01470000
00148      PERFORM R1000-OPEN THRU R1000-EXIT.                          01480000
00149      PERFORM R1100-READ-INPUT-FILE THRU R1100-EXIT.               01490000
00150 *    MOVE SPACES     TO WS-OUT-REC.                               01500000
00151      PERFORM 0100-PROCESS THRU 0100-EXIT                          01510000
00152          UNTIL EOF.                                               01520000
00153 ******************************************************            01530000
00154 **  THIS WILL WRITE THE LAST RECORD. IT WAS PREVIOUSLY            01540000
00155 **  BEING SKIPPED RESULTING IN NEVER BEING ABLE TO                01550000
00156 **  MATCH THE LAST RECORD.                                        01560000
00157 ******************************************************            01570000
00158      PERFORM R1400-WRITE THRU R1400-EXIT.                         01580000
00159      PERFORM R1200-CLOSE THRU R1200-EXIT.                         01590000
00160      STOP RUN.                                                    01600000
00161  0000-EXIT. EXIT.                                                 01610000
00162 /                                                                 01620000
00163  0100-PROCESS.                                                    01630000
00164      IF IN-WORK-KEY = LOW-VALUES                                  01640000
00165          PERFORM 0200-PROCESS-EXISTING THRU 0200-EXIT             01650000
00166          GO TO 0100-EXIT.                                         01660000
00167      IF IN-WORK-KEY NOT = LAST-WORK-KEY                           01670000
00168          PERFORM R1400-WRITE THRU R1400-EXIT,                     01680000
00169          SET COMP-INDEX, NON-COMP-INDEX TO 1,                     01690000
00170          MOVE IN-WORK-KEY TO LAST-WORK-KEY.                       01700000
00171      PERFORM R1300-MOVE THRU R1300-EXIT.                          01710000
00172      PERFORM  R1100-READ-INPUT-FILE THRU R1100-EXIT.              01720000
00173  0100-EXIT.                                                       01730000
00174      EXIT.                                                        01740000
00175 /                                                                 01750000
00176  0200-PROCESS-EXISTING.                                           01760000
00177      IF IN-OCCURS = LOW-VALUES                                    01770000
00178          PERFORM 0300-PROCESS-LAST-SLOT-REC THRU 0300-EXIT        01780000
00179          PERFORM  R1100-READ-INPUT-FILE THRU R1100-EXIT           01790000
00180          GO TO 0200-EXIT.                                         01800000
00181      IF IN-TAB-KEY NOT = LAST-TAB-KEY                             01810000
00182          PERFORM R1400-WRITE THRU R1400-EXIT                      01820000
00183          SET COMP-INDEX, NON-COMP-INDEX TO 1,                     01830000
00184          MOVE IN-TAB-KEY TO LAST-TAB-KEY.                         01840000
00185      PERFORM R1300-MOVE THRU R1300-EXIT.                          01850000
00186      PERFORM  R1100-READ-INPUT-FILE THRU R1100-EXIT.              01860000
00187  0200-EXIT.                                                       01870000
00188      EXIT.                                                        01880000
00189 /                                                                 01890000
00190  0300-PROCESS-LAST-SLOT-REC.                                      01900000
00191      MOVE LOW-VALUES TO WS-LAST-SLOT-REC.                         01910000
00192      MOVE IN-TAB-KEY TO WS-LAST-TAB-KEY.                          01920000
00193      WRITE OUT-REC FROM WS-LAST-SLOT-REC.                         01930000
00194  0300-EXIT.                                                       01940000
00195      EXIT.                                                        01950000
00196 /                                                                 01960000
00197  R1000-OPEN.                                                      01970000
00198      OPEN INPUT  IN-FILE                                          01980000
00199           OUTPUT OUT-FILE.                                        01990000
00200  R1000-EXIT.                                                      02000000
00201      EXIT.                                                        02010000
00202                                                                   02020000
00203  R1100-READ-INPUT-FILE.                                           02030000
00204      READ IN-FILE AT END                                          02040000
00205         MOVE '1' TO WS-EOF-SW.                                    02050000
00206  R1100-EXIT. EXIT.                                                02060000
00207                                                                   02070000
00208  R1200-CLOSE.                                                     02080000
00209       CLOSE IN-FILE,                                              02090000
00210             OUT-FILE.                                             02100000
00211  R1200-EXIT.                                                      02110000
00212      EXIT.                                                        02120000
00213                                                                   02130000
00214  R1300-MOVE.                                                      02140000
00215      MOVE IN-WORK-KEY TO WS-OUT-WORK-KEY.                         02150000
00216      MOVE IN-FIXED-PORTION TO WS-OUT-FIXED-PORTION.               02160000
00217      MOVE IN-OCCURS-NON-COMPARE-AREA TO                           02170000
00218                          WS-OUT-NON-COMP-TAB (NON-COMP-INDEX).    02180000
00219      MOVE IN-OCCURS-COMPARE-AREA TO                               02190000
00220                          WS-OUT-COMP-TAB (COMP-INDEX).            02200000
00221      SET NON-COMP-INDEX, COMP-INDEX UP BY 1.                      02210000
00222  R1300-EXIT.                                                      02220000
00223      EXIT.                                                        02230000
00224                                                                   02240000
00225  R1400-WRITE.                                                     02250000
00226      IF 1ST-TIME                                                  02260000
00227          MOVE '1' TO 1ST-TIME-SW                                  02270000
00228          GO TO R1400-EXIT.                                        02280000
00229 *    MOVE HIGH-VALUES TO WS-OUT-COMP-TAB (COMP-INDEX).            02290000
00230      WRITE OUT-REC FROM WS-OUT-REC.                               02300000
00231      MOVE LOW-VALUES TO WS-OUT-REC.                               02310000
00232 *    MOVE SPACES     TO WS-OUT-REC.                               02320000
00233  R1400-EXIT.                                                      02330000
00234      EXIT.                                                        02340000
00235                                                                   02350000
