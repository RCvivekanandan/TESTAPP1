00001 *      LAST MAINTENANCE TIME: 16.30.40  DATE: 11/19/84            12/09/02
00002  IDENTIFICATION DIVISION.                                         GC0270  
00003  PROGRAM-ID. GC0270.                                                 LV002
00004  AUTHOR. ROBERT MANN - DECISION CONSULTANTS INC.                  GC0270  
00005  INSTALLATION. HCSC.                                              GC0270  
00006  DATE-WRITTEN.  NOV 14,1984.                                      GC0270  
00007  DATE-COMPILED.                                                   GC0270  
00008  REMARKS.                                                         GC0270  
00009                                                                   GC0270  
00010      THIS PROGRAM CREATES THE BEFORE IMAGE FOR ALL CHANGED        GC0270  
00011      CONTRACT AND GROUP SPECIFIC RECORDS USING THE CONTROL        GC0270  
00012      RECORD TO OBTAIN THE KEY OF THE MAP FROM RECORDS.            GC0270  
00013                                                                   GC0270  
00014                                                                   GC0270  
00015      BALX IS USED TO READ THE RCRC FILE.                          GC0270  
00016      BALX IS USED TO WRITE THE BEFORE-AFTER-IMAGE FILE.           GC0270  
00017                                                                   GC0270  
00018 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION        GC0270  
00019 *                                                                 GC0270  
00020  ENVIRONMENT DIVISION.                                            GC0270  
00021  CONFIGURATION SECTION.                                           GC0270  
00022  SOURCE-COMPUTER. IBM-370.                                        GC0270  
00023  OBJECT-COMPUTER. IBM-370.                                        GC0270  
00024  INPUT-OUTPUT SECTION.                                            GC0270  
00025  FILE-CONTROL.                                                    GC0270  
00026                                                                   GC0270  
00027  DATA DIVISION.                                                   GC0270  
00028  FILE SECTION.                                                    GC0270  
00029                                                                   GC0270  
00030      EJECT                                                        GC0270  
00031  WORKING-STORAGE SECTION.                                         GC0270  
00032                                                                   GC0270  
00033  01  FILLER                      PIC X(22)   VALUE                GC0270  
00034                                  'GC0270 WORKING STORAGE'.        GC0270  
00035                                                                   GC0270  
00036  01  PARM-1.                                                      GC0270  
00037      05  1-RESERVED-FLDS         PIC 9(8)    VALUE ZEROS COMP.    GC0270  
00038      05  1-RESERV-FLDS REDEFINES 1-RESERVED-FLDS.                 GC0270  
00039          10  1-REQUEST-TYPE      PIC X.                           GC0270  
00040          10  FILLER              PIC X(3).                        GC0270  
00041                                                                   GC0270  
00042  01  PARM-1A.                                                     GC0270  
00043      02  1A-RDW.                                                  GC0270  
00044          05  1A-REC-LENG         PIC 9(4)    VALUE ZEROS COMP.    GC0270  
00045          05  1A-FEEDBACK         PIC 9(4)    VALUE ZEROS COMP.    GC0270  
00046      02  1A-REC-AREA             PIC X(5763).                     GC0270  
00047      02  1A-REC-X REDEFINES 1A-REC-AREA.                          GC0270  
00048          05  1A-KEY              PIC X(17).                       GC0270  
00049          05  FILLER              PIC X(71).                       GC0270  
00050          05  1A-PTR-COUNT        PIC S9(5)    COMP-3.             GC0270  
00051          05  FILLER              PIC X(5672).                     GC0270  
00052                                                                   GC0270  
00053  01  PARM-2.                                                      GC0270  
00054      05  2-RESERVED-FLDS         PIC 9(8)    VALUE ZEROS COMP.    GC0270  
00055      05  2-RESERV-FLDS REDEFINES 2-RESERVED-FLDS.                 GC0270  
00056          10  2-REQUEST-TYPE      PIC X.                           GC0270  
00057          10  FILLER              PIC X(3).                        GC0270  
00058                                                                   GC0270  
00059  01  PARM-2A.                                                     GC0270  
00060      02  2A-RDW.                                                  GC0270  
00061          05  2A-REC-LENG         PIC 9(4)    VALUE ZEROS COMP.    GC0270  
00062          05  2A-FEEDBACK         PIC 9(4)    VALUE ZEROS COMP.    GC0270  
00063      02  2A-REC-AREA             PIC X(710).                      GC0270  
00064      02  2A-REC-X REDEFINES 2A-REC-AREA.                          GC0270  
00065          05  2A-KEY              PIC X(14).                       GC0270  
00066          05  FILLER              PIC X(6).                        GC0270  
00067          05  2A-PTR-COUNT        PIC S9(3)    COMP-3.             GC0270  
00068          05  FILLER              PIC X(688).                      GC0270  
00069                                                                   GC0270  
00070  01  PARM-SET.                                                    GC0270  
00071      05  SET-RDW.                                                 GC0270  
00072          10  SET-REC-LENG        PIC 9(4)    VALUE ZEROS COMP.    GC0270  
00073          10  SET-FEEDBACK        PIC 9(4)    VALUE ZEROS COMP.    GC0270  
00074      05  SET-VALUE               PIC 9(8)    VALUE ZEROS COMP.    GC0270  
00075                                                                   GC0270  
00076  01  IOSW            PIC  X     VALUE '0'.                        GC0270  
00077  01  IOFILE          PIC  9(4)   COMP SYNC  VALUE ZEROES.         GC0270  
00078  01  INLENGTH        PIC  9(4)   COMP SYNC  VALUE ZEROES.         GC0270  
00079  01  OUTLENGTH       PIC  9(4)   COMP SYNC  VALUE ZEROES.         GC0270  
00080  01  IAREA.                                                       GC0270  
00081      COPY GCWRKDCC.                                               GC0270  
00082      05  RCRC-REC    PIC X(5763).                                 GC0270  
00083  01  OAREA.                                                       GC0270  
00084      COPY GCHSTWKC.                                               GC0270  
00085      05  BAIF-REC    PIC X(5763).                                 GC0270  
00086  01  INPDDNAME       PIC  X(8)  VALUE 'GC0270A '.                 GC0270  
00087  01  OUTDDNAMEA      PIC  X(8)  VALUE 'GC0270B '.                 GC0270  
00088                                                                   GC0270  
00089  01  ABEND-CODE                  PIC 9(4)    COMP.                GC0270  
00090                                                                   GC0270  
00091  01  SWITCH-AREA.                                                 GC0270  
00092      05  RCRC-SW                 PIC X       VALUE SPACES.        GC0270  
00093          88  EOF-RCRC            VALUE HIGH-VALUES.               GC0270  
00094                                                                   GC0270  
00095      COPY HSCDATES.                                               GC0270  
00096                                                                   GC0270  
00097  01  DATE-WORK-AREA.                                              GC0270  
00098      05  JUL-DATE.                                                GC0270  
00099          10  JUL-YY              PIC 99.                          GC0270  
00100          10  JUL-DD              PIC 999.                         GC0270  
00101      05  JUL-DTE REDEFINES JUL-DATE PIC 9(5).                     GC0270  
00102                                                                   GC0270  
00103  01  CTL-REC.                                                     GC0270  
00104      COPY GCCCRDCC.                                               GC0270  
00105      EJECT                                                        GC0270  
00106  LINKAGE SECTION.                                                 GC0270  
00107      EJECT                                                        GC0270  
00108  PROCEDURE DIVISION.                                              GC0270  
00109                                                                   GC0270  
00110  0000-MAINLINE.                                                   GC0270  
00111                                                                   GC0270  
00112      MOVE 'S'                    TO 1-REQUEST-TYPE.               GC0270  
00113      MOVE 8                      TO SET-REC-LENG.                 GC0270  
00114      MOVE 3                      TO SET-VALUE.                    GC0270  
00115      CALL 'TSGVSAM1' USING PARM-1 PARM-SET.                       GC0270  
00116      IF  1-REQUEST-TYPE NOT EQUAL 'S'                             GC0270  
00117          MOVE SET-FEEDBACK       TO ABEND-CODE                    GC0270  
00118          GO TO 9999-ERROR-RTN.                                    GC0270  
00119                                                                   GC0270  
00120      MOVE 'S'                    TO 2-REQUEST-TYPE.               GC0270  
00121      MOVE 8                      TO SET-REC-LENG.                 GC0270  
00122      MOVE 3                      TO SET-VALUE.                    GC0270  
00123      CALL 'TSGVSAM2' USING PARM-2 PARM-SET.                       GC0270  
00124      IF  2-REQUEST-TYPE NOT EQUAL 'S'                             GC0270  
00125          MOVE SET-FEEDBACK       TO ABEND-CODE                    GC0270  
00126          GO TO 9999-ERROR-RTN.                                    GC0270  
00127                                                                   GC0270  
00128      MOVE 'O'                    TO 1-REQUEST-TYPE.               GC0270  
00129      CALL 'TSGVSAM1' USING PARM-1 PARM-1A.                        GC0270  
00130      IF  1-REQUEST-TYPE NOT EQUAL 'O'                             GC0270  
00131          MOVE 1A-FEEDBACK        TO ABEND-CODE                    GC0270  
00132          GO TO 9999-ERROR-RTN.                                    GC0270  
00133                                                                   GC0270  
00134      MOVE 'O'                    TO 2-REQUEST-TYPE.               GC0270  
00135      CALL 'TSGVSAM2' USING PARM-2 PARM-2A.                        GC0270  
00136      IF  2-REQUEST-TYPE NOT EQUAL 'O'                             GC0270  
00137          MOVE 2A-FEEDBACK        TO ABEND-CODE                    GC0270  
00138          GO TO 9999-ERROR-RTN.                                    GC0270  
00139                                                                   GC0270  
00140      CALL 'TCDTES' USING HSCDATES.                                GC0270  
00141      MOVE JYR                    TO JUL-YY.                       GC0270  
00142      MOVE JDA                    TO JUL-DD.                       GC0270  
00143                                                                   GC0270  
00144      PERFORM 0010-READ-RCRC THRU 0010-EXIT.                       GC0270  
00145      PERFORM 0060-PROCESS THRU 0060-EXIT                          GC0270  
00146          UNTIL EOF-RCRC.                                          GC0270  
00147                                                                   GC0270  
00148      MOVE '4'                    TO IOSW.                         GC0270  
00149      PERFORM 0020-CALL-BALX THRU 0020-EXIT.                       GC0270  
00150                                                                   GC0270  
00151      MOVE '4'                    TO IOSW.                         GC0270  
00152      MOVE +1                     TO IOFILE.                       GC0270  
00153      PERFORM 0020-CALL-BALX THRU 0020-EXIT.                       GC0270  
00154                                                                   GC0270  
00155      MOVE 'C'                    TO 1-REQUEST-TYPE.               GC0270  
00156      CALL 'TSGVSAM1' USING PARM-1 PARM-1A.                        GC0270  
00157      IF  1-REQUEST-TYPE NOT EQUAL 'C'                             GC0270  
00158          MOVE 1A-FEEDBACK        TO ABEND-CODE                    GC0270  
00159          GO TO 9999-ERROR-RTN.                                    GC0270  
00160                                                                   GC0270  
00161      MOVE 'C'                    TO 2-REQUEST-TYPE.               GC0270  
00162      CALL 'TSGVSAM2' USING PARM-2 PARM-2A.                        GC0270  
00163      IF  2-REQUEST-TYPE NOT EQUAL 'C'                             GC0270  
00164          MOVE 2A-FEEDBACK        TO ABEND-CODE                    GC0270  
00165          GO TO 9999-ERROR-RTN.                                    GC0270  
00166                                                                   GC0270  
00167      GOBACK.                                                      GC0270  
00168      EJECT                                                        GC0270  
00169  0010-READ-RCRC.                                                  GC0270  
00170                                                                   GC0270  
00171      MOVE '1'                    TO IOSW.                         GC0270  
00172      MOVE ZEROES                 TO IOFILE.                       GC0270  
00173                                                                   GC0270  
00174      PERFORM 0020-CALL-BALX THRU 0020-EXIT.                       GC0270  
00175                                                                   GC0270  
00176      IF  IOSW = '5'                                               GC0270  
00177          MOVE HIGH-VALUES        TO RCRC-SW.                      GC0270  
00178          GO TO 0010-EXIT.                                         GC0270  
00179                                                                   GC0270  
00180      IF  (WRK-REC-CONT-CON OR WRK-REC-GROUP-SPEC-CTL)             GC0270  
00181          AND WRK-CHANGE-REQUEST                                   GC0270  
00182              GO TO 0010-EXIT                                      GC0270  
00183      ELSE                                                         GC0270  
00184          GO TO 0010-READ-RCRC.                                    GC0270  
00185                                                                   GC0270  
00186  0010-EXIT.                                                       GC0270  
00187      EXIT.                                                        GC0270  
00188      EJECT                                                        GC0270  
00189  0020-CALL-BALX.                                                  GC0270  
00190                                                                   GC0270  
00191      CALL 'BALX' USING                                            GC0270  
00192          IOSW                                                     GC0270  
00193          IOFILE                                                   GC0270  
00194          IAREA                                                    GC0270  
00195          INLENGTH                                                 GC0270  
00196          OAREA                                                    GC0270  
00197          OUTLENGTH                                                GC0270  
00198          INPDDNAME                                                GC0270  
00199          OUTDDNAMEA.                                              GC0270  
00200                                                                   GC0270  
00201  0020-EXIT.                                                       GC0270  
00202      EXIT.                                                        GC0270  
00203      EJECT                                                        GC0270  
00204  0030-READ-CONT.                                                  GC0270  
00205                                                                   GC0270  
00206      MOVE CCR-MAPFROM-CON-NO     TO 1A-KEY.                       GC0270  
00207      MOVE 'R'                    TO 1-REQUEST-TYPE.               GC0270  
00208      MOVE 21                     TO 1A-REC-LENG.                  GC0270  
00209      CALL 'TSGVSAM1' USING PARM-1 PARM-1A.                        GC0270  
00210      IF  1-REQUEST-TYPE EQUAL '3'                                 GC0270  
00211          MOVE 1020               TO ABEND-CODE                    GC0270  
00212          GO TO 9999-ERROR-RTN.                                    GC0270  
00213      IF  1-REQUEST-TYPE NOT EQUAL 'R'                             GC0270  
00214          MOVE 1A-FEEDBACK        TO ABEND-CODE                    GC0270  
00215          GO TO 9999-ERROR-RTN.                                    GC0270  
00216                                                                   GC0270  
00217      MOVE 1A-REC-AREA            TO BAIF-REC.                     GC0270  
00218                                                                   GC0270  
00219      COMPUTE OUTLENGTH EQUAL                                      GC0270  
00220          1A-REC-LENG - 4 + 100.                                   GC0270  
00221                                                                   GC0270  
00222  0030-EXIT.                                                       GC0270  
00223      EXIT.                                                        GC0270  
00224      EJECT                                                        GC0270  
00225  0040-READ-GROUP.                                                 GC0270  
00226                                                                   GC0270  
00227      MOVE CCR-MAPFROM-GRP-SPEC-NO                                 GC0270  
00228                                  TO 2A-KEY.                       GC0270  
00229      MOVE 'R'                    TO 2-REQUEST-TYPE.               GC0270  
00230      MOVE 18                     TO 2A-REC-LENG.                  GC0270  
00231      CALL 'TSGVSAM2' USING PARM-2 PARM-2A.                        GC0270  
00232      IF  2-REQUEST-TYPE EQUAL '3'                                 GC0270  
00233          MOVE 1030               TO ABEND-CODE                    GC0270  
00234          GO TO 9999-ERROR-RTN.                                    GC0270  
00235      IF  2-REQUEST-TYPE NOT EQUAL 'R'                             GC0270  
00236          MOVE 2A-FEEDBACK        TO ABEND-CODE                    GC0270  
00237          GO TO 9999-ERROR-RTN.                                    GC0270  
00238                                                                   GC0270  
00239      MOVE 2A-REC-AREA            TO BAIF-REC.                     GC0270  
00240                                                                   GC0270  
00241      COMPUTE OUTLENGTH EQUAL                                      GC0270  
00242          2A-REC-LENG - 4 + 100.                                   GC0270  
00243                                                                   GC0270  
00244  0040-EXIT.                                                       GC0270  
00245      EXIT.                                                        GC0270  
00246      EJECT                                                        GC0270  
00247  0050-WRITE-BAIF.                                                 GC0270  
00248                                                                   GC0270  
00249      MOVE '2'                    TO IOSW.                         GC0270  
00250      MOVE +1                     TO IOFILE.                       GC0270  
00251                                                                   GC0270  
00252      PERFORM 0020-CALL-BALX THRU 0020-EXIT.                       GC0270  
00253                                                                   GC0270  
00254  0050-EXIT.                                                       GC0270  
00255      EXIT.                                                        GC0270  
00256      EJECT                                                        GC0270  
00257  0060-PROCESS.                                                    GC0270  
00258                                                                   GC0270  
00259      MOVE WORK-RECORD            TO HIST-RECORD.                  GC0270  
00260      MOVE 'B'                    TO HST-IMAGE-INDICATOR.          GC0270  
00261      MOVE JUL-DTE                TO HST-DT-OF-LAST-CHANGE.        GC0270  
00262                                                                   GC0270  
00263      MOVE RCRC-REC               TO CONTRACT-CONTROL-RECORD.      GC0270  
00264                                                                   GC0270  
00265      IF  WRK-REC-CONT-CON                                         GC0270  
00266          PERFORM 0030-READ-CONT THRU 0030-EXIT.                   GC0270  
00267                                                                   GC0270  
00268      IF  WRK-REC-GROUP-SPEC-CTL                                   GC0270  
00269          PERFORM 0040-READ-GROUP THRU 0040-EXIT.                  GC0270  
00270                                                                   GC0270  
00271      PERFORM 0050-WRITE-BAIF THRU 0050-EXIT.                      GC0270  
00272                                                                   GC0270  
00273      PERFORM 0010-READ-RCRC THRU 0010-EXIT.                       GC0270  
00274                                                                   GC0270  
00275  0060-EXIT.                                                       GC0270  
00276      EXIT.                                                        GC0270  
00277      EJECT                                                        GC0270  
00278  9999-ERROR-RTN.                                                  GC0270  
00279                                                                   GC0270  
00280      CALL 'TSGEND' USING ABEND-CODE.                              GC0270  
00281                                                                   GC0270  
00282  9999-EXIT.                                                       GC0270  
00283      EXIT.                                                        GC0270  
