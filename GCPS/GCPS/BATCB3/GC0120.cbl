00001  IDENTIFICATION DIVISION.                                         00010000
00002  PROGRAM-ID. GC0120.                                              00020000
00003  AUTHOR. ROBERT MANN - DECISION CONSULANTS INC.                   00030000
00004  INSTALLATION. HCSC.                                              00040000
00005  DATE-WRITTEN.  APR  6,1984                                       00050000
00006  DATE-COMPILED.                                                   00060000
00007 ******************************************************************00070000
00008 *                                                                 00080000
00009 *    THIS PROGRAM CREATES A NEW RELEASE BENEFIT PROVISION         00090000
00010 *    FILE WITH UPDATED SLOT NUMBERS.                              00100000
00011 *                                                                 00110000
00012 ******************************************************************00120000
00013 ******************************************************************00130000
00014 *                                                                *00140000
00015 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *00150000
00016 *       *-*         U P D A T E   H I S T O R Y         *-*      *00160000
00017 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *00170000
00018 *                                                                *00180000
00019 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*00190000
00020 *                                                                *00200000
00021 *   11154     3/06/91  FRY   INCREASE RECORD AREA IN FILE        *00210000
00022 *                            SECTION:                            *00220000
00023 *                        TAB-REC-DATA.                           *00230000
00024 *                        FILLER   PIC X(3990) CHANGED TO  7795   *00240000
00025 *                                                                *00250000
00026 *                                                                *00260000
00027 *D12009 09/18/91  TPM   THE FOLLOWING KEYS WERE INCREASED BY 1   *00270000
00028 *                       TO ACCOMODATE FOR THE INCREASE IN        *00280000
00029 *                       THE FAMILY RELATION FIELD:               *00290000
00030 *                           CRBTP-REC-KEY                        *00300000
00031 *                           URBP-KEY                             *00310000
00032 *                           CRBTP-KEY-FLD-1                      *00320000
00033 *                           RBP-KEY-FLD-1                        *00330000
00034 *                                                                *00340000
00035 *        1/17/95   EMS  CONVERTED TO COBOL II.                   *00350000
00036 *                                                                *00360000
00037 * 14726/     11/11/97  GSP  MODIFIED TO BECOME MILLENNIUM        *00370000
00038 * 15057                     COMPLIANT. INCREASED FILLER IN       *00380000
00039 *                           CRBTP-REC-KEY AND URBP-REC-KEY       *00390000
00040 *                           TO MAKE KEY LENGTH 100.              *00400000
00041 *                                                                *00410000
00042 *            04/30/02  AKK  CHANGED TAB-REC-DATA FILLER TO       *00420000
00043 *                           7795 FILE MISMATCH IN 0S 390         *00430000
00044 *                           CONVERSION.                          *00440000
00045 *                                                                *00450000
00046 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *00460000
00047 *                                                                *00470000
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION          00471002
00048 ******************************************************************00480000
00049  ENVIRONMENT DIVISION.                                            00490000
00050  CONFIGURATION SECTION.                                           00500000
00051  SOURCE-COMPUTER. IBM-370.                                        00510000
00052  OBJECT-COMPUTER. IBM-370.                                        00520000
00053  INPUT-OUTPUT SECTION.                                            00530000
00054  FILE-CONTROL.                                                    00540000
00055      SELECT RLSE-BEN-PROV-FILE                                    00550000
00056                              ASSIGN TO UT-S-GC0120A.              00560000
00057      SELECT COMP-RLSE-BEN-TAB-PROV-FILE                           00570000
00058                              ASSIGN TO UT-S-GC0120B.              00580000
00059      SELECT UPDT-RLSE-BEN-PROV-FILE                               00590000
00060                              ASSIGN TO UT-S-GC0120C.              00600000
00061      EJECT                                                        00610000
00062  DATA DIVISION.                                                   00620000
00063  FILE SECTION.                                                    00630000
00064                                                                   00640000
00065  FD  RLSE-BEN-PROV-FILE                                           00650000
00066      LABEL RECORDS ARE STANDARD                                   00660000
00067      RECORDING MODE IS V                                          00670000
00068      BLOCK CONTAINS 0 RECORDS.                                    00680000
00069  01  RLSE-BEN-PROV-REC.                                           00690000
00070      COPY GCWRKDCC.                                               00700000
00071      05  GCP-REC             PIC X(395).                          00710000
00072      EJECT                                                        00720000
00073  FD  COMP-RLSE-BEN-TAB-PROV-FILE                                  00730000
00074      LABEL RECORDS ARE STANDARD                                   00740000
00075      RECORDING MODE IS V                                          00750000
00076      BLOCK CONTAINS 0 RECORDS.                                    00760000
00077  01  COMP-RLSE-BEN-TAB-PROV-REC.                                  00770000
00078      05  CRBTP-REC-KEY.                                           00780000
00079          10  CRBTP-KEY-FLDS.                                      00790000
00080              15  CRBTP-MATCH             PIC X(30).               00800000
00081              15  CRBTP-REC-TYPE          PIC X(2).                00810000
00082              15  CRBTP-BEN-ID-SLOT.                               00820000
00083                  20  CRBTP-BEN-ID        PIC X(6).                00830000
00084                  20  CRBTP-BEN-SLOT      PIC S9(7)   COMP-3.      00840000
00085              15  CRBTP-TAB-ID-SLOT.                               00850000
00086                  20  CRBTP-TAB-ID        PIC X(6).                00860000
00087                  20  CRBTP-TAB-SLOT      PIC S9(7) COMP-3.        00870000
00088              15  FILLER                  PIC X(5).                00880000
00089          10  FILLER                      PIC X(43).               00890000
00090      05  TAB-REC-DATA.                                            00900000
00091          10  TAB-ID-SLOT.                                         00910000
00092              15  TAB-ID      PIC X(6).                            00920000
00093              15  TAB-SLOT-NO PIC S9(7)       COMP-3.              00930000
00094 *        10  FILLER          PIC X(3990).                         00940000
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION          00941002
00095          10  FILLER          PIC X(31360).                        00950001
00096      EJECT                                                        00960000
00097  FD  UPDT-RLSE-BEN-PROV-FILE                                      00970000
00098      LABEL RECORDS ARE STANDARD                                   00980000
00099      RECORDING MODE IS V                                          00990000
00100      BLOCK CONTAINS 0 RECORDS.                                    01000000
00101  01  UPDT-RLSE-BEN-PROV-REC.                                      01010000
00102      05  URBP-REC-KEY.                                            01020000
00103          10  URBP-KEY        PIC X(57).                           01030000
00104          10  FILLER          PIC X(43).                           01040000
00105      COPY GCBENPVC.                                               01050000
00106      EJECT                                                        01060000
00107  WORKING-STORAGE SECTION.                                         01070000
00108                                                                   01080000
00109  01  FILLER          PIC X(22)   VALUE                            01090000
00110                      'GC0120 WORKING STORAGE'.                    01100000
00111                                                                   01110000
00112  01  SWITCH-AREA.                                                 01120000
00113      05  RBP-SW      PIC X   VALUE SPACES.                        01130000
00114          88  EOF-RBP         VALUE HIGH-VALUES.                   01140000
00115      05  CRBTP-SW    PIC X   VALUE SPACES.                        01150000
00116          88  EOF-CRBTP       VALUE HIGH-VALUES.                   01160000
00117                                                                   01170000
00118  01  ABEND-CODE      PIC 9(4)    COMP.                            01180000
00119                                                                   01190000
00120  01  TAB-ID-TEST.                                                 01200000
00121      05  TAB-POS-1   PIC X       VALUE SPACES.                    01210000
00122          88  TAB-ID-VALID        VALUE '#'.                       01220000
00123      05  FILLER      PIC X(5)    VALUE SPACES.                    01230000
00124                                                                   01240000
00125  01  RBP-KEY.                                                     01250000
00126      05  RBP-KEY-FLD-1       PIC X(30)   VALUE SPACES.            01260000
00127      05  RBP-KEY-FLD-2       PIC X(6)    VALUE SPACES.            01270000
00128                                                                   01280000
00129  01  CRBTP-KEY.                                                   01290000
00130      05  CRBTP-KEY-FLD-1     PIC X(30)   VALUE SPACES.            01300000
00131      05  CRBTP-KEY-FLD-2     PIC X(6)    VALUE SPACES.            01310000
00132      EJECT                                                        01320000
00133  01  GCCDRLEN-REC-WORK-AREA.                                      01330000
00134      COPY GCCDRLEN.                                               01340000
00135                                                                   01350000
00136  LINKAGE SECTION.                                                 01360000
00137      EJECT                                                        01370000
00138  PROCEDURE DIVISION.                                              01380000
00139                                                                   01390000
00140  0000-MAINLINE.                                                   01400000
00141                                                                   01410000
00142      OPEN INPUT  RLSE-BEN-PROV-FILE                               01420000
00143                  COMP-RLSE-BEN-TAB-PROV-FILE                      01430000
00144           OUTPUT UPDT-RLSE-BEN-PROV-FILE.                         01440000
00145                                                                   01450000
00146      PERFORM 0010-READ-RBP-REC THRU 0010-EXIT.                    01460000
00147      PERFORM 0020-READ-CRBTP-REC THRU 0020-EXIT.                  01470000
00148      PERFORM 0030-MATCH THRU 0030-EXIT                            01480000
00149          UNTIL EOF-RBP OR EOF-CRBTP.                              01490000
00150                                                                   01500000
00151      IF  EOF-RBP AND EOF-CRBTP                                    01510000
00152          NEXT SENTENCE                                            01520000
00153      ELSE                                                         01530000
00154          IF  EOF-RBP                                              01540000
00155              MOVE 1000           TO ABEND-CODE                    01550000
00156              GO TO 0060-ERROR-RTN                                 01560000
00157          ELSE                                                     01570000
00158              PERFORM 0050-END-RBP-FILE THRU 0050-EXIT             01580000
00159                  UNTIL EOF-RBP.                                   01590000
00160                                                                   01600000
00161      CLOSE RLSE-BEN-PROV-FILE                                     01610000
00162            COMP-RLSE-BEN-TAB-PROV-FILE                            01620000
00163            UPDT-RLSE-BEN-PROV-FILE.                               01630000
00164                                                                   01640000
00165      GOBACK.                                                      01650000
00166      EJECT                                                        01660000
00167  0010-READ-RBP-REC.                                               01670000
00168                                                                   01680000
00169      READ RLSE-BEN-PROV-FILE                                      01690000
00170          AT END                                                   01700000
00171              MOVE HIGH-VALUES    TO RBP-SW                        01710000
00172              GO TO 0010-EXIT.                                     01720000
00173                                                                   01730000
00174      MOVE GC-GCBENPRV-VARY-MAX-OCUR TO                            01740000
00175                           GCP-COUNT-TAB-PROVN-POINTERS.           01750000
00176      MOVE RLSE-BEN-PROV-REC      TO UPDT-RLSE-BEN-PROV-REC.       01760000
00177      MOVE WRK-MATCH-CONT         TO RBP-KEY-FLD-1.                01770000
00178      MOVE WRK-BN-PROV-ID         TO RBP-KEY-FLD-2.                01780000
00179                                                                   01790000
00180  0010-EXIT.                                                       01800000
00181      EXIT.                                                        01810000
00182      EJECT                                                        01820000
00183  0020-READ-CRBTP-REC.                                             01830000
00184                                                                   01840000
00185      READ COMP-RLSE-BEN-TAB-PROV-FILE                             01850000
00186          AT END                                                   01860000
00187              MOVE HIGH-VALUES    TO CRBTP-SW.                     01870000
00188                                                                   01880000
00189      MOVE CRBTP-MATCH            TO CRBTP-KEY-FLD-1.              01890000
00190      MOVE CRBTP-BEN-ID           TO CRBTP-KEY-FLD-2.              01900000
00191                                                                   01910000
00192  0020-EXIT.                                                       01920000
00193      EXIT.                                                        01930000
00194      EJECT                                                        01940000
00195  0030-MATCH.                                                      01950000
00196                                                                   01960000
00197      IF  RBP-KEY GREATER THAN CRBTP-KEY                           01970000
00198          MOVE 1030           TO ABEND-CODE                        01980000
00199          GO TO 0060-ERROR-RTN.                                    01990000
00200                                                                   02000000
00201      IF  RBP-KEY LESS THAN CRBTP-KEY                              02010000
00202          PERFORM 0040-WRITE-URBP-REC THRU 0040-EXIT               02020000
00203          PERFORM 0010-READ-RBP-REC THRU 0010-EXIT                 02030000
00204          GO TO 0030-EXIT.                                         02040000
00205                                                                   02050000
00206      SET GCP-INDEX TO 1.                                          02060000
00207      SEARCH GCP-BEN-TAB-PROVN-ID                                  02070000
00208          AT END                                                   02080000
00209              MOVE 1035       TO ABEND-CODE                        02090000
00210              GO TO 0060-ERROR-RTN                                 02100000
00211          WHEN                                                     02110000
00212              GCP-BP-ID (GCP-INDEX) EQUAL TAB-ID                   02120000
00213                  MOVE TAB-SLOT-NO TO GCP-BP-SLOT-NO (GCP-INDEX).  02130000
00214                                                                   02140000
00215      PERFORM 0020-READ-CRBTP-REC THRU 0020-EXIT.                  02150000
00216                                                                   02160000
00217  0030-EXIT.                                                       02170000
00218      EXIT.                                                        02180000
00219      EJECT                                                        02190000
00220  0040-WRITE-URBP-REC.                                             02200000
00221                                                                   02210000
00222      PERFORM 0045-VALID-TABS THRU 0045-EXIT                       02220000
00223          VARYING GCP-INDEX FROM 1 BY 1 UNTIL                      02230000
00224          GCP-INDEX GREATER THAN GCP-COUNT-TAB-PROVN-POINTERS.     02240000
00225                                                                   02250000
00226      WRITE UPDT-RLSE-BEN-PROV-REC.                                02260000
00227                                                                   02270000
00228  0040-EXIT.                                                       02280000
00229      EXIT.                                                        02290000
00230                                                                   02300000
00231  0045-VALID-TABS.                                                 02310000
00232                                                                   02320000
00233      MOVE GCP-BP-ID (GCP-INDEX)  TO TAB-ID-TEST.                  02330000
00234                                                                   02340000
00235      IF  TAB-ID-VALID                                             02350000
00236          AND GCP-BP-SLOT-NO (GCP-INDEX) GREATER THAN 8999999      02360000
00237              MOVE 1045           TO ABEND-CODE                    02370000
00238              GO TO 0060-ERROR-RTN.                                02380000
00239                                                                   02390000
00240  0045-EXIT.                                                       02400000
00241      EXIT.                                                        02410000
00242      EJECT                                                        02420000
00243  0050-END-RBP-FILE.                                               02430000
00244                                                                   02440000
00245      PERFORM 0040-WRITE-URBP-REC THRU 0040-EXIT.                  02450000
00246      PERFORM 0010-READ-RBP-REC THRU 0010-EXIT.                    02460000
00247                                                                   02470000
00248  0050-EXIT.                                                       02480000
00249      EXIT.                                                        02490000
00250                                                                   02500000
00251  0060-ERROR-RTN.                                                  02510000
00252                                                                   02520000
00253      CALL 'TSGEND' USING ABEND-CODE.                              02530000
00254                                                                   02540000
00255  0060-EXIT.                                                       02550000
00256      EXIT.                                                        02560000
