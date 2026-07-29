00001  IDENTIFICATION DIVISION.                                         00010000
00002  PROGRAM-ID. GC0060.                                              00020000
00003  AUTHOR. ROBERT MANN - DECISION CONSULANTS INC.                   00030000
00004  INSTALLATION. HCSC.                                              00040000
00005  DATE-WRITTEN.  DEC 27,1984.                                      00050000
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
00017 * 11154    2/21/91  NE   -CHANGE ACCUM TABULAR RECORD LEN FROM   *00170000
00018 *                         4000 TO 7805.                          *00180000
00019 *                                                                *00190000
00020 * 11154    3/06/91  FRY  -INCREASE RECORD AREA IN FILE SECTION:  *00200000
00021 *                        CRIT-INT-REC-DATA.                       00210000
00022 *                        FILLER    PIC X(3990)  CHANGED TO  7795 *00220000
00023 *                                                                *00230000
00024 * 11154    3/15/91  ENW  -COMMENTED OUT THE INTERNAL TABULAR SLOT*00240000
00025 *                         VALIDATION ROUTINES.                   *00250000
00026 *                                                                *00260000
00027 *                                                                *00270000
00028 *D12009 09/17/91  TPM   CHANGED THE CRIT-REC-KEY TO ACCOMODATE   *00280000
00029 *                       FOR THE INCREASE  IN THE FAMILY RELATION *00290000
00030 *                       FIELD.                                   *00300000
00031 *                       THE FOLLOWING KEYS WERE ALSO CHANGED FOR *00310000
00032 *                       THE  FAMILY RELATION FIELD:              *00320000
00033 *                          URALT-KEY                             *00330000
00034 *                          RALT-KEY                              *00340000
00035 *                          CRIT-KEY                              *00350000
00036 *                                                                *00360000
00037 *       1/10/95   EMS  CONVERTED TO COBOL II.                    *00370000
00038 *                                                                *00380000
00039 *      11/21/96   KJD  ALLOW 'T6' VALUE FOR TEXAS DATA           *00390000
00040 *                                                                *00400000
00041 * 14726/ 10/01/97  AB   ADDED CODE TO SUPPORT THE YEAR 2000      *00410000
00042 * 15057                 AND THE EXPANSION OF THE GROUP SPECIFIC  *00420000
00043 *                       AND CONTRACT KEY TO SUPPORT THE TEXAS    *00430000
00044 *                       MERGER.                                  *00440000
00045 *                                                                *00450000
00046 * D15182 10/15/98  FRY  ADD #ACP ACCUM TABULAR                   *00460000
00047 *                                                                *00470000
00048 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *00480000
00049 *                                                                *00490000
00050 * D-356A   5/08/03  GTF  RECOMPILE FOR COPYBK CHANGES #ACON,     *00500000
00051 *                        #ACOS, #ADIP, #ADOP.                    *00510000
00052 *                                                                *00510102
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION          00511002
ED0624*BBDA-58217 06/04/24  ED  RECOMPILE FOR PEAQ COPYBOOK            *00511003
ED0624*                         EXPANSION:                             *00511004
ED0624*                               COPYBKS - GCTABM*, GCTACL*,      *00511005
ED0624*                               GCTACP*,  GCTADL*, GCTADL*       *00511006
00052 ******************************************************************00520000
00053  ENVIRONMENT DIVISION.                                            00530000
00054  CONFIGURATION SECTION.                                           00540000
00055  SOURCE-COMPUTER. IBM-370.                                        00550000
00056  OBJECT-COMPUTER. IBM-370.                                        00560000
00057  INPUT-OUTPUT SECTION.                                            00570000
00058  FILE-CONTROL.                                                    00580000
00059      SELECT RLSE-ALL-LEVEL-TAB-FILE                               00590000
00060                              ASSIGN TO UT-S-GC0060A.              00600000
00061      SELECT COMP-RLSE-INT-TAB-FILE                                00610000
00062                              ASSIGN TO UT-S-GC0060B.              00620000
00063      SELECT UPDT-RLSE-ALL-LEVEL-TAB-FILE                          00630000
00064                              ASSIGN TO UT-S-GC0060C.              00640000
00065 /                                                                 00650000
00066  DATA DIVISION.                                                   00660000
00067  FILE SECTION.                                                    00670000
00068                                                                   00680000
00069  FD  RLSE-ALL-LEVEL-TAB-FILE                                      00690000
00070      LABEL RECORDS ARE STANDARD                                   00700000
00071      RECORDING MODE IS V                                          00710000
00072      BLOCK CONTAINS 0 RECORDS.                                    00720000
00073  01  RLSE-ALL-LEVEL-TAB-REC.                                      00730000
00074      COPY GCWRKDCC.                                               00740000
00075      05  RALT-REC.                                                00750000
00076          10  RALT-ID             PIC X(6).                        00760000
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION          00761002
00077          10  FILLER              PIC X(31364).                    00770001
00078  01  RALT-ABM-REC.                                                00780000
00079      05  FILLER                  PIC X(100).                      00790000
00080      COPY GCTABMC.                                                00800000
00081  01  RALT-ACL-REC.                                                00810000
00082      05  FILLER                  PIC X(100).                      00820000
00083      COPY GCTACLC.                                                00830000
00084  01  RALT-ACP-REC.                                                00840000
00085      05  FILLER                  PIC X(100).                      00850000
00086      COPY GCTACPC.                                                00860000
00087  01  RALT-ADL-REC.                                                00870000
00088      05  FILLER                  PIC X(100).                      00880000
00089      COPY GCTADLC.                                                00890000
00090  01  RALT-AOL-REC.                                                00900000
00091      05  FILLER                  PIC X(100).                      00910000
00092      COPY GCTAOLC.                                                00920000
00093  01  RALT-AAR-REC.                                                00930000
00094      05  FILLER                  PIC X(100).                      00940000
00095      COPY GCTAARC.                                                00950000
00096  01  RALT-ACON-REC.                                               00960000
00097      05  FILLER                  PIC X(100).                      00970000
00098      COPY GCTACONC.                                               00980000
00099  01  RALT-ACOS-REC.                                               00990000
00100      05  FILLER                  PIC X(100).                      01000000
00101      COPY GCTACOSC.                                               01010000
00102  01  RALT-ADIP-REC.                                               01020000
00103      05  FILLER                  PIC X(100).                      01030000
00104      COPY GCTADIPC.                                               01040000
00105  01  RALT-ADOP-REC.                                               01050000
00106      05  FILLER                  PIC X(100).                      01060000
00107      COPY GCTADOPC.                                               01070000
00108 /                                                                 01080000
00109  FD  COMP-RLSE-INT-TAB-FILE                                       01090000
00110      LABEL RECORDS ARE STANDARD                                   01100000
00111      RECORDING MODE IS V                                          01110000
00112      BLOCK CONTAINS 0 RECORDS.                                    01120000
00113  01  COMP-RLSE-INT-TAB-REC.                                       01130000
00114      05  CRIT-REC-KEY.                                            01140000
00115          10  CRIT-KEY-FLDS.                                       01150000
00116              15  CRIT-MATCH              PIC X(30).               01160000
00117              15  CRIT-REC-TYPE           PIC X(2).                01170000
00118                  88 CRIT-CONT-BEN-TAB-TAB VALUE 'C6' 'T6'.        01180000
00119              15  CRIT-BEN-ID-SLOT.                                01190000
00120                  20  CRIT-BEN-ID         PIC X(6).                01200000
00121                  20  CRIT-BEN-SLOT       PIC S9(7)   COMP-3.      01210000
00122              15  CRIT-TAB-ID-SLOT.                                01220000
00123                  20  CRIT-TAB-ID         PIC X(6).                01230000
00124                  20  CRIT-TAB-SLOT       PIC S9(7)   COMP-3.      01240000
00125              15  FILLER                  PIC X(5).                01250000
00126          10  FILLER                      PIC X(6).                01260000
00127          10  CRIT-INT-BEN-ID             PIC X(6).                01270000
00128          10  FILLER                      PIC X(31).               01280000
00129      05  CRIT-INT-REC-DATA.                                       01290000
00130          10  CRIT-INT-ID-SLOT.                                    01300000
00131              15  CRIT-INT-ID             PIC X(6).                01310000
00132              15  CRIT-INT-SLOT-NO        PIC S9(7)   COMP-3.      01320000
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION          01321002
00133          10  FILLER                      PIC X(31360).            01330001
00134 /                                                                 01340000
00135  FD  UPDT-RLSE-ALL-LEVEL-TAB-FILE                                 01350000
00136      LABEL RECORDS ARE STANDARD                                   01360000
00137      RECORDING MODE IS V                                          01370000
00138      BLOCK CONTAINS 0 RECORDS.                                    01380000
00139  01  UPDT-RLSE-ALL-LEVEL-TAB-REC.                                 01390000
00140      05  URALT-REC-KEY.                                           01400000
00141          10  URALT-KEY           PIC X(57).                       01410000
00142          10  FILLER              PIC X(43).                       01420000
      *DM9441 09/04/09 JJS  CHANGED        FOR FILE CONVERSION          01421002
00143      05  URALT-REC               PIC X(31370).                    01430001
00144  01  URALT-ABM-REC.                                               01440000
00145      05  FILLER                  PIC X(100).                      01450000
00146      COPY GCTABM2.                                                01460000
00147  01  URALT-ACL-REC.                                               01470000
00148      05  FILLER                  PIC X(100).                      01480000
00149      COPY GCTACL2.                                                01490000
00150  01  URALT-ACP-REC.                                               01500000
00151      05  FILLER                  PIC X(100).                      01510000
00152      COPY GCTACP2.                                                01520000
00153  01  URALT-ADL-REC.                                               01530000
00154      05  FILLER                  PIC X(100).                      01540000
00155      COPY GCTADL2.                                                01550000
00156  01  URALT-AOL-REC.                                               01560000
00157      05  FILLER                  PIC X(100).                      01570000
00158      COPY GCTAOL2.                                                01580000
00159  01  URALT-AAR-REC.                                               01590000
00160      05  FILLER                  PIC X(100).                      01600000
00161      COPY GCTAAR2.                                                01610000
00162  01  URALT-ACON-REC.                                              01620000
00163      05  FILLER                  PIC X(100).                      01630000
00164      COPY GCTACON2.                                               01640000
00165  01  URALT-ACOS-REC.                                              01650000
00166      05  FILLER                  PIC X(100).                      01660000
00167      COPY GCTACOS2.                                               01670000
00168  01  URALT-ADIP-REC.                                              01680000
00169      05  FILLER                  PIC X(100).                      01690000
00170      COPY GCTADIP2.                                               01700000
00171  01  URALT-ADOP-REC.                                              01710000
00172      05  FILLER                  PIC X(100).                      01720000
00173      COPY GCTADOP2.                                               01730000
00174 /                                                                 01740000
00175  WORKING-STORAGE SECTION.                                         01750000
00176                                                                   01760000
00177  01  FILLER                  PIC X(22)   VALUE                    01770000
00178                              'GC0060 WORKING STORAGE'.            01780000
00179                                                                   01790000
00180  01  SWITCH-AREA.                                                 01800000
00181      05  RALT-SW             PIC X       VALUE SPACES.            01810000
00182          88  EOF-RALT                VALUE HIGH-VALUES.           01820000
00183      05  CRIT-SW             PIC X       VALUE SPACES.            01830000
00184          88  EOF-CRIT                VALUE HIGH-VALUES.           01840000
00185      05  INT-ID-FOUND-SW     PIC X       VALUE SPACES.            01850000
00186          88  INT-ID-FOUND            VALUE HIGH-VALUES.           01860000
00187                                                                   01870000
00188  01  ABEND-CODE              PIC 9(4)    COMP.                    01880000
00189                                                                   01890000
00190  01  TAB-ID-TEST.                                                 01900000
00191      05  TAB-POS-1           PIC X       VALUE SPACES.            01910000
00192          88  TAB-ID-VALID            VALUE '#'.                   01920000
00193      05  FILLER              PIC X(5)    VALUE SPACES.            01930000
00194                                                                   01940000
00195  01  RALT-KEY.                                                    01950000
00196      05  RALT-KEY-FLD-1      PIC X(30)   VALUE SPACES.            01960000
00197      05  RALT-KEY-FLD-2      PIC X(6)    VALUE SPACES.            01970000
00198      05  RALT-KEY-FLD-3      PIC X(6)    VALUE SPACES.            01980000
00199                                                                   01990000
00200  01  CRIT-KEY.                                                    02000000
00201      05  CRIT-KEY-FLD-1      PIC X(30)   VALUE SPACES.            02010000
00202      05  CRIT-KEY-FLD-2      PIC X(6)    VALUE SPACES.            02020000
00203      05  CRIT-KEY-FLD-3      PIC X(6)    VALUE SPACES.            02030000
00204                                                                   02040000
00205  01  TS-TAB-COUNT            PIC S9      COMP-3.                  02050000
00206  01  WS-SLOT-DISPLAY         PIC X(7).                            02060000
00207                                                                   02070000
00208  01  TAB-ID-SLOT-HOLD-AREA.                                       02080000
00209      05  TAB-ID-SLOT-HOLD      OCCURS 5 TIMES                     02090000
00210                                INDEXED BY TS-INDEX.               02100000
00211          10  TAB-ID-HOLD         PIC X(6).                        02110000
00212          10  TAB-SLOT-HOLD       PIC S9(7)   COMP-3.              02120000
00213                                                                   02130000
00214  01  GCCDRLEN-REC-WORK-AREA.                                      02140000
00215      COPY GCCDRLEN.                                               02150000
00216                                                                   02160000
00217 /                                                                 02170000
00218  LINKAGE SECTION.                                                 02180000
00219 /                                                                 02190000
00220  PROCEDURE DIVISION.                                              02200000
00221                                                                   02210000
00222  0000-MAINLINE.                                                   02220000
00223                                                                   02230000
00224      OPEN INPUT  RLSE-ALL-LEVEL-TAB-FILE                          02240000
00225                  COMP-RLSE-INT-TAB-FILE                           02250000
00226           OUTPUT UPDT-RLSE-ALL-LEVEL-TAB-FILE.                    02260000
00227                                                                   02270000
00228      PERFORM 0010-READ-RALT-REC THRU 0010-EXIT.                   02280000
00229      PERFORM 0020-READ-CRIT-REC THRU 0020-EXIT.                   02290000
00230      PERFORM 0060-MATCH THRU 0060-EXIT                            02300000
00231          UNTIL EOF-RALT OR EOF-CRIT.                              02310000
00232                                                                   02320000
00233      IF  EOF-RALT AND EOF-CRIT                                    02330000
00234          NEXT SENTENCE                                            02340000
00235      ELSE                                                         02350000
00236          IF  EOF-RALT                                             02360000
00237              MOVE 1000           TO ABEND-CODE                    02370000
00238              GO TO 9999-ERROR-RTN                                 02380000
00239          ELSE                                                     02390000
00240              PERFORM 0050-END-RALT-FILE THRU 0050-EXIT            02400000
00241                  UNTIL EOF-RALT.                                  02410000
00242                                                                   02420000
00243      CLOSE RLSE-ALL-LEVEL-TAB-FILE                                02430000
00244            COMP-RLSE-INT-TAB-FILE                                 02440000
00245            UPDT-RLSE-ALL-LEVEL-TAB-FILE.                          02450000
00246                                                                   02460000
00247      GOBACK.                                                      02470000
00248 /                                                                 02480000
00249  0010-READ-RALT-REC.                                              02490000
00250                                                                   02500000
00251      READ RLSE-ALL-LEVEL-TAB-FILE                                 02510000
00252          AT END                                                   02520000
00253              MOVE HIGH-VALUES    TO RALT-SW                       02530000
00254              GO TO 0010-EXIT.                                     02540000
00255                                                                   02550000
00256      MOVE WRK-MATCH-CONT         TO RALT-KEY-FLD-1.               02560000
00257      MOVE RALT-ID                TO RALT-KEY-FLD-3.               02570000
00258                                                                   02580000
00259      IF  WRK-REC-CONT-BEN-TAB-PROV                                02590000
00260          MOVE WRK-BN-PROV-ID     TO RALT-KEY-FLD-2                02600000
00261      ELSE                                                         02610000
00262          MOVE SPACES             TO RALT-KEY-FLD-2.               02620000
00263                                                                   02630000
00264  0010-EXIT.                                                       02640000
00265      EXIT.                                                        02650000
00266 /                                                                 02660000
00267  0020-READ-CRIT-REC.                                              02670000
00268                                                                   02680000
00269      READ COMP-RLSE-INT-TAB-FILE                                  02690000
00270          AT END                                                   02700000
00271              MOVE HIGH-VALUES    TO CRIT-SW.                      02710000
00272                                                                   02720000
00273      MOVE CRIT-MATCH             TO CRIT-KEY-FLD-1.               02730000
00274      MOVE CRIT-BEN-ID            TO CRIT-KEY-FLD-3.               02740000
00275                                                                   02750000
00276      IF  CRIT-CONT-BEN-TAB-TAB                                    02760000
00277          MOVE CRIT-INT-BEN-ID    TO CRIT-KEY-FLD-2                02770000
00278      ELSE                                                         02780000
00279          MOVE SPACES             TO CRIT-KEY-FLD-2.               02790000
00280                                                                   02800000
00281  0020-EXIT.                                                       02810000
00282      EXIT.                                                        02820000
00283 /                                                                 02830000
00284  0030-WRITE-URALT-REC.                                            02840000
00285                                                                   02850000
00286      IF  RALT-ID EQUAL '#AAR  '                                   02860000
00287          MOVE GAE-ENTRY-COUNT    TO GAE2-ENTRY-COUNT              02870000
00288          MOVE RALT-AAR-REC       TO URALT-AAR-REC                 02880000
00289          WRITE URALT-AAR-REC                                      02890000
00290          GO TO 0030-EXIT.                                         02900000
00291                                                                   02910000
00292      IF  RALT-ID EQUAL '#ABM  '                                   02920000
00293 *        PERFORM 0040-VALID-TABS-ABM THRU 0040-EXIT               02930000
00294 *            VARYING GAA-INDEX FROM 1 BY 1 UNTIL                  02940000
00295 *            GAA-INDEX GREATER THAN GAA-ENTRY-COUNT               02950000
00296          MOVE GAA-ENTRY-COUNT    TO GAA2-ENTRY-COUNT              02960000
00297          MOVE RALT-ABM-REC       TO URALT-ABM-REC                 02970000
00298          WRITE URALT-ABM-REC                                      02980000
00299          GO TO 0030-EXIT.                                         02990000
00300                                                                   03000000
00301      IF  RALT-ID EQUAL '#ACL  '                                   03010000
00302 *        PERFORM 0042-VALID-TABS-ACL THRU 0042-EXIT               03020000
00303 *            VARYING GAB-INDEX FROM 1 BY 1 UNTIL                  03030000
00304 *            GAB-INDEX GREATER THAN GAB-ENTRY-COUNT               03040000
00305          MOVE GAB-ENTRY-COUNT    TO GAB2-ENTRY-COUNT              03050000
00306          MOVE RALT-ACL-REC       TO URALT-ACL-REC                 03060000
00307          WRITE URALT-ACL-REC                                      03070000
00308          GO TO 0030-EXIT.                                         03080000
00309                                                                   03090000
00310      IF  RALT-ID EQUAL '#ACP  '                                   03100000
00311          MOVE GAF-ENTRY-COUNT    TO GAF2-ENTRY-COUNT              03110000
00312          MOVE RALT-ACP-REC       TO URALT-ACP-REC                 03120000
00313          WRITE URALT-ACP-REC                                      03130000
00314          GO TO 0030-EXIT.                                         03140000
00315                                                                   03150000
00316      IF  RALT-ID EQUAL '#ADL  '                                   03160000
00317 *        PERFORM 0044-VALID-TABS-ADL THRU 0044-EXIT               03170000
00318 *            VARYING GAC-INDEX FROM 1 BY 1 UNTIL                  03180000
00319 *            GAC-INDEX GREATER THAN GAC-ENTRY-COUNT               03190000
00320          MOVE GAC-ENTRY-COUNT    TO GAC2-ENTRY-COUNT              03200000
00321          MOVE RALT-ADL-REC       TO URALT-ADL-REC                 03210000
00322          WRITE URALT-ADL-REC                                      03220000
00323          GO TO 0030-EXIT.                                         03230000
00324                                                                   03240000
00325      IF  RALT-ID EQUAL '#AOL  '                                   03250000
00326 *        PERFORM 0046-VALID-TABS-AOL THRU 0046-EXIT               03260000
00327 *            VARYING GAD-INDEX FROM 1 BY 1 UNTIL                  03270000
00328 *            GAD-INDEX GREATER THAN GAD-ENTRY-COUNT               03280000
00329          MOVE GAD-ENTRY-COUNT    TO GAD2-ENTRY-COUNT              03290000
00330          MOVE RALT-AOL-REC       TO URALT-AOL-REC                 03300000
00331          WRITE URALT-AOL-REC                                      03310000
00332          GO TO 0030-EXIT.                                         03320000
00333                                                                   03330000
00334      IF  RALT-ID EQUAL '#ACON '                                   03340000
00335          MOVE GAI-ENTRY-COUNT    TO GAI2-ENTRY-COUNT              03350000
00336          MOVE RALT-ACON-REC      TO URALT-ACON-REC                03360000
00337          WRITE URALT-ACON-REC                                     03370000
00338          GO TO 0030-EXIT.                                         03380000
00339                                                                   03390000
00340      IF  RALT-ID EQUAL '#ACOS '                                   03400000
00341          MOVE GAJ-ENTRY-COUNT    TO GAJ2-ENTRY-COUNT              03410000
00342          MOVE RALT-ACOS-REC      TO URALT-ACOS-REC                03420000
00343          WRITE URALT-ACOS-REC                                     03430000
00344          GO TO 0030-EXIT.                                         03440000
00345                                                                   03450000
00346      IF  RALT-ID EQUAL '#ADIP '                                   03460000
00347          MOVE GAG-ENTRY-COUNT    TO GAG2-ENTRY-COUNT              03470000
00348          MOVE RALT-ADIP-REC      TO URALT-ADIP-REC                03480000
00349          WRITE URALT-ADIP-REC                                     03490000
00350          GO TO 0030-EXIT.                                         03500000
00351                                                                   03510000
00352      IF  RALT-ID EQUAL '#ADOP '                                   03520000
00353          MOVE GAH-ENTRY-COUNT    TO GAH2-ENTRY-COUNT              03530000
00354          MOVE RALT-ADOP-REC      TO URALT-ADOP-REC                03540000
00355          WRITE URALT-ADOP-REC                                     03550000
00356          GO TO 0030-EXIT.                                         03560000
00357                                                                   03570000
00358  0030-EXIT.                                                       03580000
00359      EXIT.                                                        03590000
00360 /                                                                 03600000
00361  0040-VALID-TABS-ABM.                                             03610000
00362                                                                   03620000
00363      IF  GAA-ENTRY (GAA-INDEX) EQUAL HIGH-VALUES                  03630000
00364          SET GAA-INDEX           TO GAA-ENTRY-COUNT               03640000
00365          GO TO 0040-EXIT.                                         03650000
00366                                                                   03660000
00367      MOVE GAA-INTERNAL-TAB-SLOT (GAA-INDEX)                       03670000
00368                                  TO TAB-ID-SLOT-HOLD-AREA.        03680000
00369                                                                   03690000
00370      PERFORM 0048-VALID-SLOT THRU 0048-EXIT                       03700000
00371          VARYING TS-INDEX FROM 1 BY 1 UNTIL                       03710000
00372          TAB-ID-SLOT-HOLD (TS-INDEX) EQUAL HIGH-VALUES.           03720000
00373                                                                   03730000
00374  0040-EXIT.                                                       03740000
00375      EXIT.                                                        03750000
00376 /                                                                 03760000
00377  0042-VALID-TABS-ACL.                                             03770000
00378                                                                   03780000
00379      IF  GAB-ENTRY (GAB-INDEX) EQUAL HIGH-VALUES                  03790000
00380          SET GAB-INDEX           TO GAB-ENTRY-COUNT               03800000
00381          GO TO 0042-EXIT.                                         03810000
00382                                                                   03820000
00383      MOVE GAB-INTERNAL-TAB-SLOT (GAB-INDEX)                       03830000
00384                                  TO TAB-ID-SLOT-HOLD-AREA.        03840000
00385                                                                   03850000
00386      PERFORM 0048-VALID-SLOT THRU 0048-EXIT                       03860000
00387          VARYING TS-INDEX FROM 1 BY 1 UNTIL                       03870000
00388          TAB-ID-SLOT-HOLD (TS-INDEX) EQUAL HIGH-VALUES.           03880000
00389                                                                   03890000
00390  0042-EXIT.                                                       03900000
00391      EXIT.                                                        03910000
00392 /                                                                 03920000
00393  0044-VALID-TABS-ADL.                                             03930000
00394                                                                   03940000
00395      IF  GAC-ENTRY (GAC-INDEX) EQUAL HIGH-VALUES                  03950000
00396          SET GAC-INDEX           TO GAC-ENTRY-COUNT               03960000
00397          GO TO 0044-EXIT.                                         03970000
00398                                                                   03980000
00399      MOVE GAC-INTERNAL-TAB-SLOT (GAC-INDEX)                       03990000
00400                                  TO TAB-ID-SLOT-HOLD-AREA.        04000000
00401                                                                   04010000
00402 *???                                                              04020000
00403      DISPLAY 'INT-TAB-ID ='  TAB-ID-HOLD(TS-INDEX).               04030000
00404      MOVE TAB-SLOT-HOLD(TS-INDEX)  TO  WS-SLOT-DISPLAY.           04040000
00405      DISPLAY 'INT-TAB-SLT ='  WS-SLOT-DISPLAY.                    04050000
00406                                                                   04060000
00407      PERFORM 0048-VALID-SLOT THRU 0048-EXIT                       04070000
00408          VARYING TS-INDEX FROM 1 BY 1 UNTIL                       04080000
00409          TAB-ID-SLOT-HOLD (TS-INDEX) EQUAL HIGH-VALUES.           04090000
00410                                                                   04100000
00411  0044-EXIT.                                                       04110000
00412      EXIT.                                                        04120000
00413 /                                                                 04130000
00414  0046-VALID-TABS-AOL.                                             04140000
00415                                                                   04150000
00416      IF  GAD-ENTRY (GAD-INDEX) EQUAL HIGH-VALUES                  04160000
00417          SET GAD-INDEX           TO GAD-ENTRY-COUNT               04170000
00418          GO TO 0046-EXIT.                                         04180000
00419                                                                   04190000
00420      MOVE GAD-INTERNAL-TAB-SLOT (GAD-INDEX)                       04200000
00421                                  TO TAB-ID-SLOT-HOLD-AREA.        04210000
00422                                                                   04220000
00423      PERFORM 0048-VALID-SLOT THRU 0048-EXIT                       04230000
00424          VARYING TS-INDEX FROM 1 BY 1 UNTIL                       04240000
00425          TAB-ID-SLOT-HOLD (TS-INDEX) EQUAL HIGH-VALUES.           04250000
00426                                                                   04260000
00427  0046-EXIT.                                                       04270000
00428      EXIT.                                                        04280000
00429 /                                                                 04290000
00430  0048-VALID-SLOT.                                                 04300000
00431                                                                   04310000
00432      MOVE TAB-ID-HOLD (TS-INDEX) TO TAB-ID-TEST.                  04320000
00433      IF  TAB-ID-VALID                                             04330000
00434          AND TAB-SLOT-HOLD (TS-INDEX) GREATER THAN 8999999        04340000
00435              MOVE 1048           TO ABEND-CODE                    04350000
00436              GO TO 9999-ERROR-RTN.                                04360000
00437                                                                   04370000
00438  0048-EXIT.                                                       04380000
00439      EXIT.                                                        04390000
00440 /                                                                 04400000
00441  0050-END-RALT-FILE.                                              04410000
00442                                                                   04420000
00443      PERFORM 0030-WRITE-URALT-REC THRU 0030-EXIT.                 04430000
00444      PERFORM 0010-READ-RALT-REC THRU 0010-EXIT.                   04440000
00445                                                                   04450000
00446  0050-EXIT.                                                       04460000
00447      EXIT.                                                        04470000
00448 /                                                                 04480000
00449  0060-MATCH.                                                      04490000
00450                                                                   04500000
00451      IF  RALT-KEY GREATER THAN CRIT-KEY                           04510000
00452          MOVE 1060           TO ABEND-CODE                        04520000
00453          GO TO 9999-ERROR-RTN.                                    04530000
00454                                                                   04540000
00455      IF  RALT-KEY LESS THAN CRIT-KEY                              04550000
00456          PERFORM 0030-WRITE-URALT-REC THRU 0030-EXIT              04560000
00457          PERFORM 0010-READ-RALT-REC THRU 0010-EXIT                04570000
00458          GO TO 0060-EXIT.                                         04580000
00459                                                                   04590000
00460      IF  RALT-ID EQUAL '#ABM  '                                   04600000
00461          PERFORM 0070-ABM-MATCH THRU 0070-EXIT                    04610000
00462              VARYING GAA-INDEX FROM 1 BY 1 UNTIL                  04620000
00463              GAA-INDEX GREATER THAN GAA-ENTRY-COUNT.              04630000
00464                                                                   04640000
00465      IF  RALT-ID EQUAL '#ACL  '                                   04650000
00466          PERFORM 0080-ACL-MATCH THRU 0080-EXIT                    04660000
00467              VARYING GAB-INDEX FROM 1 BY 1 UNTIL                  04670000
00468              GAB-INDEX GREATER THAN GAB-ENTRY-COUNT.              04680000
00469                                                                   04690000
00470      IF  RALT-ID   =   '#ACP  '                                   04700000
00471          PERFORM 0085-ACP-MATCH THRU 0085-EXIT                    04710000
00472            VARYING GAF-INDEX FROM 1 BY 1 UNTIL                    04720000
00473                    GAF-INDEX  >  GAF-ENTRY-COUNT.                 04730000
00474                                                                   04740000
00475      IF  RALT-ID EQUAL '#ADL  '                                   04750000
00476          PERFORM 0090-ADL-MATCH THRU 0090-EXIT                    04760000
00477              VARYING GAC-INDEX FROM 1 BY 1 UNTIL                  04770000
00478              GAC-INDEX GREATER THAN GAC-ENTRY-COUNT.              04780000
00479                                                                   04790000
00480      IF  RALT-ID EQUAL '#AOL  '                                   04800000
00481          PERFORM 0100-AOL-MATCH THRU 0100-EXIT                    04810000
00482              VARYING GAD-INDEX FROM 1 BY 1 UNTIL                  04820000
00483              GAD-INDEX GREATER THAN GAD-ENTRY-COUNT.              04830000
00484                                                                   04840000
00485                                                                   04850000
00486      PERFORM 0020-READ-CRIT-REC THRU 0020-EXIT.                   04860000
00487                                                                   04870000
00488  0060-EXIT.                                                       04880000
00489      EXIT.                                                        04890000
00490 /                                                                 04900000
00491  0070-ABM-MATCH.                                                  04910000
00492                                                                   04920000
00493      IF  GAA-ENTRY (GAA-INDEX) EQUAL HIGH-VALUES                  04930000
00494          MOVE 1070               TO ABEND-CODE                    04940000
00495          GO TO 9999-ERROR-RTN.                                    04950000
00496                                                                   04960000
00497      MOVE GAA-INTERNAL-TAB-SLOT (GAA-INDEX)                       04970000
00498                                  TO TAB-ID-SLOT-HOLD-AREA.        04980000
00499                                                                   04990000
00500      MOVE GAA-INTERNAL-TABULAR-COUNT (GAA-INDEX)                  05000000
00501                                  TO TS-TAB-COUNT.                 05010000
00502                                                                   05020000
00503      PERFORM 0110-SEARCH-ID THRU 0110-EXIT                        05030000
00504          VARYING TS-INDEX FROM 1 BY 1 UNTIL                       05040000
00505          TS-INDEX GREATER THAN TS-TAB-COUNT.                      05050000
00506                                                                   05060000
00507      IF  INT-ID-FOUND                                             05070000
00508          MOVE SPACES             TO INT-ID-FOUND-SW               05080000
00509          MOVE TAB-ID-SLOT-HOLD-AREA                               05090000
00510                              TO GAA-INTERNAL-TAB-SLOT (GAA-INDEX) 05100000
00511          SET GAA-INDEX           TO GAA-ENTRY-COUNT.              05110000
00512                                                                   05120000
00513  0070-EXIT.                                                       05130000
00514      EXIT.                                                        05140000
00515 /                                                                 05150000
00516  0080-ACL-MATCH.                                                  05160000
00517                                                                   05170000
00518      IF  GAB-ENTRY (GAB-INDEX) EQUAL HIGH-VALUES                  05180000
00519          MOVE 1080               TO ABEND-CODE                    05190000
00520          GO TO 9999-ERROR-RTN.                                    05200000
00521                                                                   05210000
00522      MOVE GAB-INTERNAL-TAB-SLOT (GAB-INDEX)                       05220000
00523                                  TO TAB-ID-SLOT-HOLD-AREA.        05230000
00524                                                                   05240000
00525      MOVE GAB-INTERNAL-TABULAR-COUNT (GAB-INDEX)                  05250000
00526                                  TO TS-TAB-COUNT.                 05260000
00527                                                                   05270000
00528      PERFORM 0110-SEARCH-ID THRU 0110-EXIT                        05280000
00529          VARYING TS-INDEX FROM 1 BY 1 UNTIL                       05290000
00530          TS-INDEX GREATER THAN TS-TAB-COUNT.                      05300000
00531                                                                   05310000
00532      IF  INT-ID-FOUND                                             05320000
00533          MOVE SPACES             TO INT-ID-FOUND-SW               05330000
00534          MOVE TAB-ID-SLOT-HOLD-AREA                               05340000
00535                              TO GAB-INTERNAL-TAB-SLOT (GAB-INDEX) 05350000
00536          SET GAB-INDEX           TO GAB-ENTRY-COUNT.              05360000
00537  0080-EXIT.                                                       05370000
00538      EXIT.                                                        05380000
00539 /                                                                 05390000
00540  0085-ACP-MATCH.                                                  05400000
00541                                                                   05410000
00542      IF  GAF-ENTRY (GAF-INDEX)   =   HIGH-VALUES                  05420000
00543          MOVE 1090  TO  ABEND-CODE                                05430000
00544          GO TO 9999-ERROR-RTN.                                    05440000
00545                                                                   05450000
00546      MOVE  GAF-INTERNAL-TAB-SLOT (GAF-INDEX)                      05460000
00547        TO  TAB-ID-SLOT-HOLD-AREA.                                 05470000
00548                                                                   05480000
00549      MOVE  GAF-INTERNAL-TABULAR-COUNT (GAF-INDEX)                 05490000
00550        TO  TS-TAB-COUNT.                                          05500000
00551                                                                   05510000
00552      PERFORM 0110-SEARCH-ID THRU 0110-EXIT                        05520000
00553        VARYING TS-INDEX FROM 1 BY 1 UNTIL                         05530000
00554                TS-INDEX  >  TS-TAB-COUNT.                         05540000
00555                                                                   05550000
00556      IF  INT-ID-FOUND                                             05560000
00557          MOVE SPACES     TO  INT-ID-FOUND-SW                      05570000
00558          MOVE  TAB-ID-SLOT-HOLD-AREA                              05580000
00559            TO  GAF-INTERNAL-TAB-SLOT (GAF-INDEX)                  05590000
00560          SET GAF-INDEX   TO  GAF-ENTRY-COUNT.                     05600000
00561                                                                   05610000
00562  0085-EXIT.                                                       05620000
00563      EXIT.                                                        05630000
00564 /                                                                 05640000
00565  0090-ADL-MATCH.                                                  05650000
00566                                                                   05660000
00567      IF  GAC-ENTRY (GAC-INDEX) EQUAL HIGH-VALUES                  05670000
00568          MOVE 1090               TO ABEND-CODE                    05680000
00569          GO TO 9999-ERROR-RTN.                                    05690000
00570                                                                   05700000
00571      MOVE GAC-INTERNAL-TAB-SLOT (GAC-INDEX)                       05710000
00572                                  TO TAB-ID-SLOT-HOLD-AREA.        05720000
00573                                                                   05730000
00574      MOVE GAC-INTERNAL-TABULAR-COUNT (GAC-INDEX)                  05740000
00575                                  TO TS-TAB-COUNT.                 05750000
00576                                                                   05760000
00577      PERFORM 0110-SEARCH-ID THRU 0110-EXIT                        05770000
00578          VARYING TS-INDEX FROM 1 BY 1 UNTIL                       05780000
00579          TS-INDEX GREATER THAN TS-TAB-COUNT.                      05790000
00580                                                                   05800000
00581      IF  INT-ID-FOUND                                             05810000
00582          MOVE SPACES             TO INT-ID-FOUND-SW               05820000
00583          MOVE TAB-ID-SLOT-HOLD-AREA                               05830000
00584                              TO GAC-INTERNAL-TAB-SLOT (GAC-INDEX) 05840000
00585          SET GAC-INDEX           TO GAC-ENTRY-COUNT.              05850000
00586  0090-EXIT.                                                       05860000
00587      EXIT.                                                        05870000
00588 /                                                                 05880000
00589  0100-AOL-MATCH.                                                  05890000
00590                                                                   05900000
00591      IF  GAD-ENTRY (GAD-INDEX) EQUAL HIGH-VALUES                  05910000
00592          MOVE 1100               TO ABEND-CODE                    05920000
00593          GO TO 9999-ERROR-RTN.                                    05930000
00594                                                                   05940000
00595      MOVE GAD-INTERNAL-TAB-SLOT (GAD-INDEX)                       05950000
00596                                  TO TAB-ID-SLOT-HOLD-AREA.        05960000
00597                                                                   05970000
00598      MOVE GAD-INTERNAL-TABULAR-COUNT (GAD-INDEX)                  05980000
00599                                  TO TS-TAB-COUNT.                 05990000
00600                                                                   06000000
00601      PERFORM 0110-SEARCH-ID THRU 0110-EXIT                        06010000
00602          VARYING TS-INDEX FROM 1 BY 1 UNTIL                       06020000
00603          TS-INDEX GREATER THAN TS-TAB-COUNT.                      06030000
00604                                                                   06040000
00605      IF  INT-ID-FOUND                                             06050000
00606          MOVE SPACES             TO INT-ID-FOUND-SW               06060000
00607          MOVE TAB-ID-SLOT-HOLD-AREA                               06070000
00608                              TO GAD-INTERNAL-TAB-SLOT (GAD-INDEX) 06080000
00609          SET GAD-INDEX           TO GAD-ENTRY-COUNT.              06090000
00610  0100-EXIT.                                                       06100000
00611      EXIT.                                                        06110000
00612 /                                                                 06120000
00613  0110-SEARCH-ID.                                                  06130000
00614                                                                   06140000
00615      IF  TAB-ID-SLOT-HOLD (TS-INDEX) EQUAL HIGH-VALUES            06150000
00616          SET TS-INDEX            TO TS-TAB-COUNT                  06160000
00617          GO TO 0110-EXIT.                                         06170000
00618                                                                   06180000
00619      IF  TAB-ID-SLOT-HOLD (TS-INDEX) EQUAL CRIT-TAB-ID-SLOT       06190000
00620          MOVE CRIT-INT-SLOT-NO   TO TAB-SLOT-HOLD (TS-INDEX)      06200000
00621          MOVE HIGH-VALUES        TO INT-ID-FOUND-SW               06210000
00622          SET TS-INDEX            TO TS-TAB-COUNT.                 06220000
00623                                                                   06230000
00624  0110-EXIT.                                                       06240000
00625      EXIT.                                                        06250000
00626 /                                                                 06260000
00627  9999-ERROR-RTN.                                                  06270000
00628                                                                   06280000
00629      CALL 'TSGEND' USING ABEND-CODE.                              06290000
00630                                                                   06300000
00631  9999-EXIT.                                                       06310000
00632      EXIT.                                                        06320000
