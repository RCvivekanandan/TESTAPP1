00001  IDENTIFICATION DIVISION.                                         08/21/03
00002                                                                   GC040000
00003  PROGRAM-ID. GC040000.                                               LV001
00004  AUTHOR. JOHN KOSTOUROS.                                          GC040000
00005                                                                   GC040000
00006  INSTALLATION.HCMS.                                               GC040000
00007  DATE-WRITTEN.                                                    GC040000
00008  DATE-COMPILED.                                                   GC040000
00009 ******************************************************************GC040000
00010 *                                                                 GC040000
00011 *     RCL-FILE IS RLSE-CONT                                       GC040000
00012 *     TSGVSAM1 IS PROVISION TABULAR FILE                          GC040000
00013 *     TSGVSAM2 IS BENENFIT PROVISION FILE (I/O).                  GC040000
00014 *                                                                 GC040000
00015 ******************************************************************GC040000
00016                                                                   GC040000
00017                                                                   GC040000
00018 ******************************************************************GC040000
00019 *   DATE     PROG   ANAL                      COMMENTS           *GC040000
00020 * --------   ----   ----    ------------------------------------ *GC040000
00021 * 01-20-86    LET    LET    REPLACED #CHOP TABULAR WITH #CLDR    *GC040000
00022 *                           FOR IMPL #9.                         *GC040000
00023 *                                                                *GC040000
00024 * 10-30-85    LET    LET    ADDED CODE SO THAT ONLY SKELETON ADDS*GC040000
00025 *                           WILL BE PRINTED ON THE REPORT.       *GC040000
00026 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GC040000
00027 *                                                                *GC040000
00028 ******************************************************************GC040000
00029 /                                                                 GC040000
00030  ENVIRONMENT DIVISION.                                            GC040000
00031                                                                   GC040000
00032  CONFIGURATION SECTION.                                           GC040000
00033  SOURCE-COMPUTER. IBM-370.                                        GC040000
00034  OBJECT-COMPUTER. IBM-370.                                        GC040000
00035                                                                   GC040000
00036                                                                   GC040000
00037  INPUT-OUTPUT SECTION.                                            GC040000
00038  FILE-CONTROL.                                                    GC040000
00039      SELECT RCL-FILE   ASSIGN TO UT-S-GC0400A.                    GC040000
00040  DATA DIVISION.                                                   GC040000
00041  FILE SECTION.                                                    GC040000
00042  FD  RCL-FILE                                                     GC040000
00043      LABEL RECORDS ARE STANDARD                                   GC040000
00044      RECORDING MODE IS V                                          GC040000
00045      BLOCK CONTAINS 0 RECORDS.                                    GC040000
00046  01  RCL-RECORD.                                                  GC040000
00047      05  RCL-KEY                    PIC X(64).                    GC040000
00048      05  REC-AREA-A                 PIC X(5763).                  GC040000
00049      05  RECORD-A REDEFINES REC-AREA-A.                           GC040000
00050          10  KEY-FIELD-A            PIC X(20).                    GC040000
00051          10  FILLER                 PIC X(68).                    GC040000
00052          10  REC-A-PNTR-COUNT       PIC S9(5)   COMP-3.           GC040000
00053          10  FILLER                 PIC X(5672).                  GC040000
00054 /                                                                 GC040000
00055  WORKING-STORAGE SECTION.                                         GC040000
00056  01  FILLER                         PIC X(24)   VALUE             GC040000
00057                                     'GC040000 WORKING-STORAGE'.   GC040000
00058                                                                   GC040000
00059                                                                   GC040000
00060  01  RPT-IND    VALUE '1172'  PIC X(4).                           GC040000
00061  01  LINE-COUNT VALUE  ZEROES PIC 99.                             GC040000
00062  01  GC040050-IND             PIC X.                              GC040000
00063      88  HEADER-TIME          VALUE 'H'.                          GC040000
00064      88  RPT-TIME             VALUE 'R'.                          GC040000
00065      88  TRAIL-TIME           VALUE 'T'.                          GC040000
00066      88  DETAIL-TIME          VALUE 'D'.                          GC040000
00067      88  LOAD-TIME            VALUE 'L'.                          GC040000
00068  01  PRINT-LINE-CC.                                               GC040000
00069      05  CONTROL-CHARACTERS         PIC X.                        GC040000
00070      05  PRINT-LINE-132             PIC X(132).                   GC040000
00071  01  COUNT-AMT                      PIC S9(5)  COMP-3.            GC040000
00072  01  COUNT-AMT2                     PIC S9(5)  COMP-3.            GC040000
00073  01  COUNT-ENTRIES                  PIC S9(5)  COMP-3.            GC040000
00074                                                                   GC040000
00075  01  SW-ON                          PIC  X(01) VALUE '1'.         GC040000
00076  01  SW-OFF                         PIC  X(01) VALUE '0'.         GC040000
00077                                                                   GC040000
00078  01  SWITCHES.                                                    GC040000
00079      05  PRT-CONTR-SW               PIC  X(01) VALUE '0'.         GC040000
00080          88  PRINT-CONTRACT                    VALUE '1'.         GC040000
00081                                                                   GC040000
00082  01  HOLD-WRK-AREA.                                               GC040000
00083      COPY GCWRKDCC.                                               GC040000
00084 /                                                                 GC040000
00085      COPY HSCDATES.                                               GC040000
00086  01  JUL-DATE.                                                    GC040000
00087      05  JUL-DT.                                                  GC040000
00088          10  JUL-YY                 PIC 99.                       GC040000
00089          10  JUL-DD                 PIC 999.                      GC040000
00090      05  TODAYS-DATE REDEFINES  JUL-DT PIC 9(5).                  GC040000
00091                                                                   GC040000
00092  01  DATE-AREA.                                                   GC040000
00093      05  GREG-DATE                  PIC 9(6).                     GC040000
00094      05  JULIAN-DATE                PIC 9(5).                     GC040000
00095                                                                   GC040000
00096  01  END-OF-FILE-SW                 PIC XXX  VALUE SPACES.        GC040000
00097      88  END-OF-RCL-FILE                     VALUE 'END'.         GC040000
00098                                                                   GC040000
00099                                                                   GC040000
00100  01  PARM-ONE-B.                                                  GC040000
00101      05  RESERVED-FLDS-B            PIC 9(8)    VALUE ZEROS COMP. GC040000
00102      05  RESERVED-ONE-B REDEFINES RESERVED-FLDS-B.                GC040000
00103          10  REQUEST-TYPE-B         PIC X.                        GC040000
00104          10  FILLER                 PIC X(3).                     GC040000
00105                                                                   GC040000
00106  01  PARM-TWO-B.                                                  GC040000
00107      05  RDW-B.                                                   GC040000
00108          10  RECORD-LENGTH-B        PIC 9(4)  COMP.               GC040000
00109          10  FEEDBACK-CODE-B        PIC 9(4)  COMP.               GC040000
00110      05  REC-AREA-B                 PIC X(4000).                  GC040000
00111      05  RECORD-B REDEFINES REC-AREA-B.                           GC040000
00112          10  KEY-FIELD-B.                                         GC040000
00113              15  KEY-ID-B           PIC X(6).                     GC040000
00114              15  KEY-SLOT-B         PIC S9(7)   COMP-3.           GC040000
00115          10  DATA-AREA-B            PIC X(3990).                  GC040000
00116                                                                   GC040000
00117  01  PARM-ONE-C.                                                  GC040000
00118      05  RESERVED-FLDS-C            PIC 9(8)    VALUE ZEROS COMP. GC040000
00119      05  RESERVED-ONE-C REDEFINES RESERVED-FLDS-C.                GC040000
00120          10  REQUEST-TYPE-C         PIC X.                        GC040000
00121          10  FILLER                 PIC X(3).                     GC040000
00122                                                                   GC040000
00123  01  PARM-TWO-C.                                                  GC040000
00124      05  RDW-C.                                                   GC040000
00125          10  RECORD-LENGTH-C        PIC 9(4)  COMP.               GC040000
00126          10  FEEDBACK-CODE-C        PIC 9(4)  COMP.               GC040000
00127      05  REC-AREA-C                 PIC X(395).                   GC040000
00128      05  RECORD-C REDEFINES REC-AREA-C.                           GC040000
00129          10  KEY-FIELD-C            PIC X(10).                    GC040000
00130          10  FILLER                 PIC X(3).                     GC040000
00131          10  REC-C-PNTR-COUNT       PIC S9(3)   COMP-3.           GC040000
00132          10  FILLER                 PIC X(380).                   GC040000
00133                                                                   GC040000
00134  01  PARM-SET.                                                    GC040000
00135      05  SET-RDW.                                                 GC040000
00136          10  SET-RECORD-LENGTH      PIC 9(4) VALUE ZEROS  COMP.   GC040000
00137          10  SET-FEEDBACK           PIC 9(4) VALUE ZEROS  COMP.   GC040000
00138      05  SET-VALUE                  PIC 9(8) VALUE ZEROS  COMP.   GC040000
00139                                                                   GC040000
00140  01  ABEND-CODE                       PIC 9(4)    COMP.           GC040000
00141                                                                   GC040000
00142  01  CONTRACT-AREA.                                               GC040000
00143      COPY GCCONTRC.                                               GC040000
00144                                                                   GC040000
00145  01  BEN-PROV-AREA.                                               GC040000
00146      COPY GCBENPVC.                                               GC040000
00147 /                                                                 GC040000
00148  PROCEDURE DIVISION.                                              GC040000
00149  0000-MAINLINE.                                                   GC040000
00150      OPEN   INPUT      RCL-FILE.                                  GC040000
00151                                                                   GC040000
00152      MOVE   'S'                  TO REQUEST-TYPE-B.               GC040000
00153      MOVE    8                   TO SET-RECORD-LENGTH.            GC040000
00154      MOVE    3                   TO SET-VALUE.                    GC040000
00155      CALL   'TSGVSAM1'  USING PARM-ONE-B PARM-SET.                GC040000
00156      IF  REQUEST-TYPE-B NOT EQUAL 'S'                             GC040000
00157          MOVE SET-FEEDBACK       TO ABEND-CODE                    GC040000
00158          GO TO 9999-ERROR-RTN.                                    GC040000
00159                                                                   GC040000
00160      MOVE   'S'                  TO REQUEST-TYPE-C.               GC040000
00161      MOVE    8                   TO SET-RECORD-LENGTH.            GC040000
00162      MOVE    3                   TO SET-VALUE.                    GC040000
00163      CALL   'TSGVSAM2'  USING PARM-ONE-C PARM-SET.                GC040000
00164      IF  REQUEST-TYPE-C NOT EQUAL 'S'                             GC040000
00165          MOVE SET-FEEDBACK       TO ABEND-CODE                    GC040000
00166          GO TO 9999-ERROR-RTN.                                    GC040000
00167                                                                   GC040000
00168                                                                   GC040000
00169      MOVE   'O'                  TO REQUEST-TYPE-B.               GC040000
00170      CALL   'TSGVSAM1'  USING PARM-ONE-B PARM-TWO-B.              GC040000
00171      IF  REQUEST-TYPE-B NOT EQUAL 'O'                             GC040000
00172          MOVE SET-FEEDBACK       TO ABEND-CODE                    GC040000
00173          GO TO 9999-ERROR-RTN.                                    GC040000
00174                                                                   GC040000
00175      MOVE   'O'                  TO REQUEST-TYPE-C.               GC040000
00176      CALL   'TSGVSAM2'  USING PARM-ONE-C PARM-TWO-C.              GC040000
00177      IF  REQUEST-TYPE-C NOT EQUAL 'O'                             GC040000
00178          MOVE SET-FEEDBACK       TO ABEND-CODE                    GC040000
00179          GO TO 9999-ERROR-RTN.                                    GC040000
00180                                                                   GC040000
00181      CALL 'TCDTES'    USING HSCDATES.                             GC040000
00182                                                                   GC040000
00183      MOVE JYR                    TO JUL-YY.                       GC040000
00184      MOVE JDA                    TO JUL-DD.                       GC040000
00185                                                                   GC040000
00186      MOVE 'H'                    TO GC040050-IND.                 GC040000
00187      CALL 'GC040050'  USING  RPT-IND, LINE-COUNT, GC040050-IND,   GC040000
00188                PRINT-LINE-CC.                                     GC040000
00189                                                                   GC040000
00190      PERFORM 0010-PROCESS-RTN  THRU 0010-EXIT                     GC040000
00191          UNTIL END-OF-RCL-FILE.                                   GC040000
00192                                                                   GC040000
00193                                                                   GC040000
00194      MOVE   'C'                  TO REQUEST-TYPE-B.               GC040000
00195      CALL   'TSGVSAM1'  USING PARM-ONE-B PARM-TWO-B.              GC040000
00196      IF  REQUEST-TYPE-B NOT EQUAL 'C'                             GC040000
00197          MOVE FEEDBACK-CODE-B    TO ABEND-CODE                    GC040000
00198          GO TO 9999-ERROR-RTN.                                    GC040000
00199                                                                   GC040000
00200      MOVE   'C'                  TO REQUEST-TYPE-C.               GC040000
00201      CALL   'TSGVSAM2'  USING PARM-ONE-C PARM-TWO-C.              GC040000
00202      IF  REQUEST-TYPE-C NOT EQUAL 'C'                             GC040000
00203          MOVE FEEDBACK-CODE-C    TO ABEND-CODE                    GC040000
00204          GO TO 9999-ERROR-RTN.                                    GC040000
00205      CLOSE  RCL-FILE.                                             GC040000
00206      MOVE 'T'                    TO GC040050-IND.                 GC040000
00207      CALL 'GC040050'  USING  RPT-IND, LINE-COUNT, GC040050-IND,   GC040000
00208           PRINT-LINE-CC.                                          GC040000
00209      GOBACK.                                                      GC040000
00210                                                                   GC040000
00211  0000-EXIT.                                                       GC040000
00212       EXIT.                                                       GC040000
00213 /                                                                 GC040000
00214  0010-PROCESS-RTN.                                                GC040000
00215                                                                   GC040000
00216      PERFORM 0015-CONT-LVL-RTN THRU 0015-EXIT.                    GC040000
00217                                                                   GC040000
00218      IF  END-OF-RCL-FILE                                          GC040000
00219          GO TO 0010-EXIT.                                         GC040000
00220                                                                   GC040000
00221      IF PRINT-CONTRACT                                            GC040000
00222          NEXT SENTENCE                                            GC040000
00223      ELSE                                                         GC040000
00224          GO TO 0010-EXIT.                                         GC040000
00225                                                                   GC040000
00226      MOVE ZEROS     TO  COUNT-ENTRIES.                            GC040000
00227      PERFORM 0060-COUNT-CONT-TAB-ENT THRU                         GC040000
00228              0060-EXIT   VARYING                                  GC040000
00229        GCT-TAB-INDEX FROM 1 BY 1 UNTIL                            GC040000
00230            GCT-CON-TAB-ID (GCT-TAB-INDEX)                         GC040000
00231          EQUAL HIGH-VALUES.                                       GC040000
00232                                                                   GC040000
00233      IF  COUNT-ENTRIES GREATER THAN ZERO                          GC040000
00234          PERFORM 0020-CONT-TAB-RTN THRU 0020-EXIT                 GC040000
00235              VARYING GCT-TAB-INDEX FROM 1 BY 1 UNTIL              GC040000
00236              GCT-CON-TAB-ID (GCT-TAB-INDEX) EQUAL HIGH-VALUES.    GC040000
00237                                                                   GC040000
00238      SUBTRACT 1 FROM GCT-COUNT-BEN-PROVN-POINTERS                 GC040000
00239          GIVING COUNT-AMT.                                        GC040000
00240      PERFORM 0030-BEN-PROVN-RTN THRU 0030-EXIT                    GC040000
00241          VARYING GCT-INDEX FROM 1 BY 1 UNTIL                      GC040000
00242          GCT-INDEX GREATER THAN COUNT-AMT.                        GC040000
00243                                                                   GC040000
00244  0010-EXIT.                                                       GC040000
00245       EXIT.                                                       GC040000
00246 /                                                                 GC040000
00247  0015-CONT-LVL-RTN.                                               GC040000
00248                                                                   GC040000
00249      MOVE SW-OFF  TO  PRT-CONTR-SW.                               GC040000
00250                                                                   GC040000
00251      READ   RCL-FILE                                              GC040000
00252                 AT END                                            GC040000
00253                   MOVE 'END'     TO END-OF-FILE-SW                GC040000
00254                                     GO TO 0015-EXIT.              GC040000
00255                                                                   GC040000
00256      MOVE RCL-KEY  TO  HOLD-WRK-AREA.                             GC040000
00257                                                                   GC040000
00258      IF WRK-SIG-B-SKELETON-C2-G2  AND WRK-ADD-REQUEST             GC040000
00259          MOVE SW-ON  TO  PRT-CONTR-SW                             GC040000
00260      ELSE                                                         GC040000
00261          GO TO 0015-EXIT.                                         GC040000
00262                                                                   GC040000
00263      MOVE REC-A-PNTR-COUNT       TO GCT-COUNT-BEN-PROVN-POINTERS. GC040000
00264      MOVE REC-AREA-A             TO CONTRACT-AREA.                GC040000
00265                                                                   GC040000
00266      CALL 'GC040001' USING REC-AREA-A, LINE-COUNT,                GC040000
00267             RPT-IND.                                              GC040000
00268                                                                   GC040000
00269      MOVE ZEROS     TO  COUNT-ENTRIES.                            GC040000
00270                                                                   GC040000
00271      PERFORM 0060-COUNT-CONT-TAB-ENT THRU                         GC040000
00272              0060-EXIT   VARYING                                  GC040000
00273        GCT-TAB-INDEX FROM 1 BY 1 UNTIL                            GC040000
00274            GCT-CON-TAB-ID (GCT-TAB-INDEX)                         GC040000
00275          EQUAL HIGH-VALUES.                                       GC040000
00276                                                                   GC040000
00277      IF  COUNT-ENTRIES GREATER THAN  ZERO                         GC040000
00278          CALL 'GC040055' USING REC-AREA-A, LINE-COUNT,            GC040000
00279                RPT-IND.                                           GC040000
00280                                                                   GC040000
00281      IF  GCT-CON-TAB-ID (1) EQUAL HIGH-VALUES                     GC040000
00282          NEXT SENTENCE                                            GC040000
00283      ELSE                                                         GC040000
00284          CALL 'GC040057' USING REC-AREA-A, LINE-COUNT,            GC040000
00285               RPT-IND.                                            GC040000
00286  0015-EXIT.                                                       GC040000
00287       EXIT.                                                       GC040000
00288 /                                                                 GC040000
00289  0020-CONT-TAB-RTN.                                               GC040000
00290                                                                   GC040000
00291      IF  GCT-CON-TAB-SLOT (GCT-TAB-INDEX) LESS THAN 11            GC040000
00292          GO TO 0020-EXIT.                                         GC040000
00293                                                                   GC040000
00294      MOVE GCT-CON-TAB-ID-SLOT (GCT-TAB-INDEX)                     GC040000
00295                                  TO KEY-FIELD-B.                  GC040000
00296                                                                   GC040000
00297      PERFORM 0050-READ-TPF-FILE THRU 0050-EXIT.                   GC040000
00298                                                                   GC040000
00299      IF  GCT-CON-TAB-ID (GCT-TAB-INDEX) EQUAL '#CLDR '            GC040000
00300          CALL 'GC040004' USING REC-AREA-B LINE-COUNT              GC040000
00301             RPT-IND.                                              GC040000
00302      IF  GCT-CON-TAB-ID (GCT-TAB-INDEX) EQUAL '#CRR  '            GC040000
00303          CALL 'GC040005' USING REC-AREA-B LINE-COUNT              GC040000
00304             RPT-IND.                                              GC040000
00305      IF  GCT-CON-TAB-ID (GCT-TAB-INDEX) EQUAL '#AAR  '            GC040000
00306          CALL 'GC040060' USING REC-AREA-B LINE-COUNT              GC040000
00307             RPT-IND.                                              GC040000
00308      IF  GCT-CON-TAB-ID (GCT-TAB-INDEX) EQUAL '#ABM  '            GC040000
00309          CALL 'GC040062' USING REC-AREA-B LINE-COUNT              GC040000
00310             RPT-IND.                                              GC040000
00311      IF  GCT-CON-TAB-ID (GCT-TAB-INDEX) EQUAL '#ACL  '            GC040000
00312          CALL 'GC040064' USING REC-AREA-B LINE-COUNT              GC040000
00313             RPT-IND.                                              GC040000
00314      IF  GCT-CON-TAB-ID (GCT-TAB-INDEX) EQUAL '#ADL  '            GC040000
00315          CALL 'GC040066' USING REC-AREA-B LINE-COUNT              GC040000
00316             RPT-IND.                                              GC040000
00317      IF  GCT-CON-TAB-ID (GCT-TAB-INDEX) EQUAL '#AOL  '            GC040000
00318          CALL 'GC040068' USING REC-AREA-B LINE-COUNT              GC040000
00319             RPT-IND.                                              GC040000
00320      IF  GCT-CON-TAB-ID (GCT-TAB-INDEX) EQUAL '#ADIP '            GC040000
00321          CALL 'GC040076' USING REC-AREA-B LINE-COUNT              GC040000
00322             RPT-IND.                                              GC040000
00323      IF  GCT-CON-TAB-ID (GCT-TAB-INDEX) EQUAL '#ADOP '            GC040000
00324          CALL 'GC040078' USING REC-AREA-B LINE-COUNT              GC040000
00325             RPT-IND.                                              GC040000
00326      IF  GCT-CON-TAB-ID (GCT-TAB-INDEX) EQUAL '#ACON '            GC040000
00327          CALL 'GC040080' USING REC-AREA-B LINE-COUNT              GC040000
00328             RPT-IND.                                              GC040000
00329      IF  GCT-CON-TAB-ID (GCT-TAB-INDEX) EQUAL '#ACOS '            GC040000
00330          CALL 'GC040082' USING REC-AREA-B LINE-COUNT              GC040000
00331             RPT-IND.                                              GC040000
00332  0020-EXIT.                                                       GC040000
00333       EXIT.                                                       GC040000
00334 /                                                                 GC040000
00335  0030-BEN-PROVN-RTN.                                              GC040000
00336                                                                   GC040000
00337      IF  GCT-BEN-PROVN-SLOT-NO (GCT-INDEX) LESS THAN 11           GC040000
00338          GO TO 0030-EXIT.                                         GC040000
00339                                                                   GC040000
00340      MOVE GCT-BEN-PROVN-ID-PNTR (GCT-INDEX)                       GC040000
00341                                  TO KEY-FIELD-C.                  GC040000
00342      MOVE 'R'                    TO REQUEST-TYPE-C.               GC040000
00343      MOVE 14                     TO RECORD-LENGTH-C.              GC040000
00344      CALL 'TSGVSAM2' USING PARM-ONE-C PARM-TWO-C.                 GC040000
00345      IF  REQUEST-TYPE-C EQUAL '3'                                 GC040000
00346          MOVE 1030               TO ABEND-CODE                    GC040000
00347          GO TO 9999-ERROR-RTN.                                    GC040000
00348      IF  REQUEST-TYPE-C NOT EQUAL 'R'                             GC040000
00349          MOVE FEEDBACK-CODE-C    TO ABEND-CODE                    GC040000
00350          GO TO 9999-ERROR-RTN.                                    GC040000
00351                                                                   GC040000
00352      MOVE REC-C-PNTR-COUNT       TO GCP-COUNT-TAB-PROVN-POINTERS. GC040000
00353      MOVE REC-AREA-C             TO BEN-PROV-AREA.                GC040000
00354                                                                   GC040000
00355      CALL 'GC040051' USING REC-AREA-C, LINE-COUNT,                GC040000
00356                RPT-IND.                                           GC040000
00357      IF  GCP-BEN-PROV-FORMAT-A                                    GC040000
00358          CALL 'GC040030' USING REC-AREA-C, LINE-COUNT,            GC040000
00359                RPT-IND.                                           GC040000
00360      IF  GCP-BEN-PROV-FORMAT-B                                    GC040000
00361          CALL 'GC040031' USING REC-AREA-C, LINE-COUNT,            GC040000
00362               RPT-IND.                                            GC040000
00363      IF  GCP-BEN-PROV-FORMAT-C                                    GC040000
00364          CALL 'GC040032' USING REC-AREA-C, LINE-COUNT,            GC040000
00365                RPT-IND.                                           GC040000
00366      IF  GCP-BEN-PROV-FORMAT-D                                    GC040000
00367          CALL 'GC040033' USING REC-AREA-C, LINE-COUNT,            GC040000
00368                RPT-IND.                                           GC040000
00369      IF  GCP-BEN-PROV-FORMAT-E                                    GC040000
00370          CALL 'GC040034' USING REC-AREA-C, LINE-COUNT,            GC040000
00371                RPT-IND.                                           GC040000
00372      IF GCP-BEN-PROV-FORMAT-W                                     GC040000
00373          CALL 'GC040036' USING REC-AREA-C, LINE-COUNT,            GC040000
00374                RPT-IND.                                           GC040000
00375      IF GCP-COUNT-TAB-PROVN-POINTERS    GREATER  THAN  1          GC040000
00376         CALL 'GC040035' USING REC-AREA-C LINE-COUNT               GC040000
00377               RPT-IND.                                            GC040000
00378      SUBTRACT 1 FROM GCP-COUNT-TAB-PROVN-POINTERS                 GC040000
00379          GIVING COUNT-AMT2.                                       GC040000
00380      PERFORM 0040-BEN-TAB-PROVN-RTN THRU 0040-EXIT VARYING        GC040000
00381          GCP-INDEX FROM 1 BY 1 UNTIL                              GC040000
00382          GCP-INDEX  GREATER THAN COUNT-AMT2.                      GC040000
00383                                                                   GC040000
00384  0030-EXIT.                                                       GC040000
00385       EXIT.                                                       GC040000
00386 /                                                                 GC040000
00387  0040-BEN-TAB-PROVN-RTN.                                          GC040000
00388                                                                   GC040000
00389      MOVE GCP-BEN-TAB-PROVN-ID (GCP-INDEX)                        GC040000
00390                                  TO KEY-FIELD-B.                  GC040000
00391                                                                   GC040000
00392      PERFORM 0050-READ-TPF-FILE THRU 0050-EXIT.                   GC040000
00393                                                                   GC040000
00394      IF  GCP-BP-ID (GCP-INDEX)  EQUAL  '#PAQ  '                   GC040000
00395        CALL 'GC040040' USING REC-AREA-B, LINE-COUNT,              GC040000
00396          RPT-IND.                                                 GC040000
00397      IF  GCP-BP-ID (GCP-INDEX)  EQUAL  '#PCR  '                   GC040000
00398        CALL 'GC040041' USING REC-AREA-B, LINE-COUNT,              GC040000
00399          RPT-IND.                                                 GC040000
00400      IF  GCP-BP-ID (GCP-INDEX)  EQUAL  '#PCX  '                   GC040000
00401        CALL 'GC040042' USING REC-AREA-B, LINE-COUNT,              GC040000
00402          RPT-IND.                                                 GC040000
00403      IF  GCP-BP-ID (GCP-INDEX)  EQUAL  '#PDP  '                   GC040000
00404        CALL 'GC040043' USING REC-AREA-B, LINE-COUNT,              GC040000
00405          RPT-IND.                                                 GC040000
00406      IF  GCP-BP-ID (GCP-INDEX)  EQUAL  '#PDR  '                   GC040000
00407        CALL 'GC040044' USING REC-AREA-B, LINE-COUNT,              GC040000
00408          RPT-IND.                                                 GC040000
00409      IF  GCP-BP-ID (GCP-INDEX)  EQUAL  '#PPF  '                   GC040000
00410        CALL 'GC040045' USING REC-AREA-B, LINE-COUNT,              GC040000
00411          RPT-IND.                                                 GC040000
00412      IF  GCP-BP-ID (GCP-INDEX)  EQUAL  '#PRR  '                   GC040000
00413        CALL 'GC040046' USING REC-AREA-B, LINE-COUNT,              GC040000
00414          RPT-IND.                                                 GC040000
00415      IF  GCP-BP-ID (GCP-INDEX)  EQUAL  '#PRV  '                   GC040000
00416        CALL 'GC040047' USING REC-AREA-B, LINE-COUNT,              GC040000
00417          RPT-IND.                                                 GC040000
00418      IF  GCP-BP-ID (GCP-INDEX)  EQUAL  '#PSC  '                   GC040000
00419        CALL 'GC040048' USING REC-AREA-B, LINE-COUNT,              GC040000
00420          RPT-IND.                                                 GC040000
00421      IF  GCP-BP-ID (GCP-INDEX)  EQUAL  '#PVE  '                   GC040000
00422        CALL 'GC040049' USING REC-AREA-B, LINE-COUNT,              GC040000
00423          RPT-IND.                                                 GC040000
00424      IF  GCP-BP-ID (GCP-INDEX)  EQUAL  '#AAR  '                   GC040000
00425        CALL 'GC040060' USING REC-AREA-B, LINE-COUNT,              GC040000
00426          RPT-IND.                                                 GC040000
00427      IF  GCP-BP-ID (GCP-INDEX)  EQUAL  '#ABM  '                   GC040000
00428        CALL 'GC040062' USING REC-AREA-B, LINE-COUNT,              GC040000
00429          RPT-IND.                                                 GC040000
00430      IF  GCP-BP-ID (GCP-INDEX)  EQUAL  '#ACL  '                   GC040000
00431        CALL 'GC040064' USING REC-AREA-B, LINE-COUNT,              GC040000
00432          RPT-IND.                                                 GC040000
00433      IF  GCP-BP-ID (GCP-INDEX)  EQUAL  '#ADL  '                   GC040000
00434        CALL 'GC040066' USING REC-AREA-B, LINE-COUNT,              GC040000
00435          RPT-IND.                                                 GC040000
00436      IF  GCP-BP-ID (GCP-INDEX)  EQUAL  '#AOL  '                   GC040000
00437        CALL 'GC040068' USING REC-AREA-B, LINE-COUNT,              GC040000
00438          RPT-IND.                                                 GC040000
00439      IF  GCP-BP-ID (GCP-INDEX) EQUAL '#ADIP '                     GC040000
00440          CALL 'GC040076' USING REC-AREA-B LINE-COUNT              GC040000
00441             RPT-IND.                                              GC040000
00442      IF  GCP-BP-ID (GCP-INDEX) EQUAL '#ADOP '                     GC040000
00443          CALL 'GC040078' USING REC-AREA-B LINE-COUNT              GC040000
00444             RPT-IND.                                              GC040000
00445      IF  GCP-BP-ID (GCP-INDEX) EQUAL '#ACON '                     GC040000
00446          CALL 'GC040080' USING REC-AREA-B LINE-COUNT              GC040000
00447             RPT-IND.                                              GC040000
00448      IF  GCP-BP-ID (GCP-INDEX) EQUAL '#ACOS '                     GC040000
00449          CALL 'GC040082' USING REC-AREA-B LINE-COUNT              GC040000
00450             RPT-IND.                                              GC040000
00451  0040-EXIT.                                                       GC040000
00452      EXIT.                                                        GC040000
00453 /                                                                 GC040000
00454  0050-READ-TPF-FILE.                                              GC040000
00455                                                                   GC040000
00456      MOVE 'R'                    TO REQUEST-TYPE-B.               GC040000
00457      MOVE 14                     TO RECORD-LENGTH-B.              GC040000
00458      CALL 'TSGVSAM1' USING PARM-ONE-B PARM-TWO-B.                 GC040000
00459      IF  REQUEST-TYPE-B EQUAL '3'                                 GC040000
00460          MOVE 1050               TO ABEND-CODE                    GC040000
00461          GO TO 9999-ERROR-RTN.                                    GC040000
00462      IF  REQUEST-TYPE-B NOT EQUAL 'R'                             GC040000
00463          MOVE FEEDBACK-CODE-B    TO ABEND-CODE                    GC040000
00464          GO TO 9999-ERROR-RTN.                                    GC040000
00465                                                                   GC040000
00466  0050-EXIT.                                                       GC040000
00467      EXIT.                                                        GC040000
00468 /                                                                 GC040000
00469  0060-COUNT-CONT-TAB-ENT.                                         GC040000
00470      IF GCT-CON-TAB-ID (GCT-TAB-INDEX)   = HIGH-VALUES  OR        GC040000
00471         GCT-CON-TAB-SLOT (GCT-TAB-INDEX) = ZEROS                  GC040000
00472         GO TO 0060-EXIT                                           GC040000
00473      ELSE                                                         GC040000
00474          ADD 1 TO COUNT-ENTRIES.                                  GC040000
00475  0060-EXIT.                                                       GC040000
00476       EXIT.                                                       GC040000
00477 /                                                                 GC040000
00478  9999-ERROR-RTN.                                                  GC040000
00479       CALL    'TSGEND'     USING      ABEND-CODE.                 GC040000
00480  9999-EXIT.                                                       GC040000
00481       EXIT.                                                       GC040000
