00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID.    GC0030.                                           GC0030  
00003  AUTHOR.        RON HUNTLEY.                                         LV003
00004  INSTALLATION.  HCSC.                                             GC0030  
00005  DATE-WRITTEN.  03/85.                                            GC0030  
00006                                                                   GC0030  
00007 ******************************************************************GC0030  
00008 *   THIS  PROGRAM UPDATES THE VSAM SYSTEM MASTER / TABULAR        GC0030  
00009 *   RECORD FILE. IT USES THE RELEASED SYSTEM MASTER/TABULAR       GC0030  
00010 *   SEQUENTIAL FILE CREATED IN GC0020.                            GC0030  
00011 *                                                                 GC0030  
00012 *   RSMT     - RELEASED SYSTEM MASTER/TABULAR FILE    (INPUT)     GC0030  
00013 *   TSGVSAM1 - SYSTEM MASTER TABULAR FILE             (I/O)       GC0030  
00014 ******************************************************************GC0030  
00015 ******************************************************************GC0030  
00016 *               U P D A T E   H I S T O R Y                      *GC0030  
00017 *    DATE    PROGRAMMER   MAINTENANCE                            *GC0030  
00018 *  --------  ----------   ---------------------------------------*GC0030  
00019 *  07-09-87     FCG       ADDED DCC TABLE LOGIC                  *GC0030  
00020 *!!!!PLEASE NOTE IF FIXED AREA CHANGES LENGTH, COMPUTE FOR       *GC0030  
00021 *!!!!RECORD-LENGTH MUST HAVE CORRESPONDING LENGTH CHANGE  !!!!   *GC0030  
00022 *!!!!THIS IS FOR ADDS AND CHANGES FOR GCTX*** AND GCTT*** !!!!   *GC0030  
00023 *!!!!COPYMEMBERS                                          !!!!   *GC0030  
00024 *  01-10-95     EMS       CONVERTED TO COBOL II.                 *GC0030  
00025 *                                                                *GC0030  
00026 *  08-14-02   GTF   RECOMPILE FOR OPID EXPANSION                 *GC0030  
00027 *                                                                *GC0030  
00028 *  09-12-02   AKK   EXPANDED FILLERS FROM 27 TO 36 IN FILE       *GC0030  
00029 *                   LAYOUTS DUE TO EXPANSION OF OPID FROM 5 TO 8.*GC0030  
00030 *  04-22-03   AKK   CHANGE 47 IN LENGTH CALC IN 0300- TO         *GC0030  
00031 *                   56 TO ACCOMODATE 9 MORE BYTES IN RECOR.      *GC0030  
00032 *  04-24-03   AKK   CHANGED REMAINING COMPUTE STATEMENTS TO      *GC0030  
00033 *                   RFLECT ADDITIONAL 9 CHARACTERS.       .      *GC0030  
00034 *  08-06-03   AKK   CHANGES FOR FILEX EXPANSION., CHANGE COMPUTES*GC0030  
00035 *                   FOR THOSE NEEDING 11, CHANGED LENGTH TO 4012.*GC0030  
00036 ******************************************************************GC0030  
00037                                                                   GC0030  
00038  ENVIRONMENT DIVISION.                                            GC0030  
00039                                                                   GC0030  
00040  CONFIGURATION SECTION.                                           GC0030  
00041  SOURCE-COMPUTER.  IBM-370.                                       GC0030  
00042  OBJECT-COMPUTER.  IBM-370.                                       GC0030  
00043  INPUT-OUTPUT SECTION.                                            GC0030  
00044                                                                   GC0030  
00045  FILE-CONTROL.                                                    GC0030  
00046      SELECT RSMT-FILE                 ASSIGN  TO UT-S-GC0030A.    GC0030  
00047  DATA DIVISION.                                                   GC0030  
00048                                                                   GC0030  
00049  FILE SECTION.                                                    GC0030  
00050                                                                   GC0030  
00051  FD  RSMT-FILE                                                    GC0030  
00052          LABEL RECORDS ARE STANDARD                               GC0030  
00053          RECORDING MODE IS V                                      GC0030  
00054          BLOCK CONTAINS  0  RECORDS.                              GC0030  
00055  01  RSMT-RECORD.                                                 GC0030  
00056      COPY GCSYSDCC.                                               GC0030  
00057      05 RSMT-TAB-REC.                                             GC0030  
00058         10 RSMT-TAB-ID                 PIC X(6).                  GC0030  
00059         10 RSMT-TAB-SLOT               PIC S9(7)  COMP-3.         GC0030  
00060         10 FILLER                      PIC X(36).                 GC0030  
00061         10 RSMT-ENTRY-COUNT            PIC S9(5)  COMP-3.         GC0030  
00062         10 FILLER                      PIC X(3960).               GC0030  
00063      05 FILLER                         PIC X(03).                 GC0030  
00064 /                                                                 GC0030  
00065  WORKING-STORAGE SECTION.                                         GC0030  
00066  01  FILLER                        PIC  X(24)                     GC0030  
00067              VALUE 'GC0030 WORKING STORAGE'.                      GC0030  
00068  01  GCXSTMR-REC.                                                 GC0030  
00069      COPY  GCXSTMRC.                                              GC0030  
00070  01  GC-XCON-REC.                                                 GC0030  
00071      COPY  GCTXCONC.                                              GC0030  
00072  01  GC-XCOS-REC.                                                 GC0030  
00073      COPY  GCTXCOSC.                                              GC0030  
00074  01  GC-XDIP-REC.                                                 GC0030  
00075      COPY  GCTXDIPC.                                              GC0030  
00076  01  GC-XDOP-REC.                                                 GC0030  
00077      COPY  GCTXDOPC.                                              GC0030  
00078  01  GC-TCON-REC.                                                 GC0030  
00079      COPY  GCTTCONC.                                              GC0030  
00080  01  GC-TCOS-REC.                                                 GC0030  
00081      COPY  GCTTCOSC.                                              GC0030  
00082  01  GC-TDIP-REC.                                                 GC0030  
00083      COPY  GCTTDIPC.                                              GC0030  
00084  01  GC-TDOP-REC.                                                 GC0030  
00085      COPY  GCTTDOPC.                                              GC0030  
00086  01  GCXSTMR-REC2.                                                GC0030  
00087      COPY  GCXSTMR2.                                              GC0030  
00088 /                                                                 GC0030  
00089  01  PARM-ONE.                                                    GC0030  
00090      05 RESERVED-FLDS              PIC 9(8)   VALUE ZEROS COMP.   GC0030  
00091      05 RESERVED-ONE  REDEFINES  RESERVED-FLDS.                   GC0030  
00092          10 REQUEST-TYPE           PIC X.                         GC0030  
00093          10 FILLER                 PIC X(3).                      GC0030  
00094                                                                   GC0030  
00095  01  PARM-TWO.                                                    GC0030  
00096      05 RDW.                                                      GC0030  
00097          10 RECORD-LENGTH          PIC 9(4)   COMP.               GC0030  
00098          10 FEEDBACK-CODE          PIC 9(4)   COMP.               GC0030  
00099      05  REC-AREA                  PIC X(4012).                   GC0030  
00100      05  VSAM-RECORD  REDEFINES  REC-AREA.                        GC0030  
00101          10 VSAM-KEY-FIELD.                                       GC0030  
00102              15  VSAM-ID           PIC X(6).                      GC0030  
00103              15  VSAM-SLOT         PIC S9(7)  COMP-3.             GC0030  
00104          10 VSAM-DATA.                                            GC0030  
00105 *EXPANDED FILLER BY NINE BYTES DUE TO EXPANSION OF                GC0030  
00106 *OPID FROM 5 TO 8 BYTES AKK 09/12/02                              GC0030  
00107              15 VSAM-FIXED-PART    PIC X(36).                     GC0030  
00108              15 VSAM-ENTRY-COUNT   PIC S9(5)  COMP-3.             GC0030  
00109              15 VSAM-VAR-PART      PIC X(3960).                   GC0030  
00110                                                                   GC0030  
00111  01  PARM-SET.                                                    GC0030  
00112      05 SET-RDW.                                                  GC0030  
00113          10 SET-RECORD-LENGTH      PIC 9(4)       COMP.           GC0030  
00114          10 SET-FEEDBACK-CODE      PIC 9(4)       COMP.           GC0030  
00115      05  SET-VALUE                 PIC 9(8)       COMP.           GC0030  
00116                                                                   GC0030  
00117 /                                                                 GC0030  
00118  01  PROGRAM-VARIABLES.                                           GC0030  
00119      05  HOLD-TAB-ID                   PIC X(11)  VALUE SPACES.   GC0030  
00120      05  ABEND-CODE                    PIC 9(4)   COMP.           GC0030  
00121      05  SEARCH-KEY.                                              GC0030  
00122          10  SEARCH-ID                 PIC X(6).                  GC0030  
00123          10  SEARCH-SLOT               PIC S9(7)  COMP-3.         GC0030  
00124      05  EOF-SW                        PIC XXX    VALUE SPACES.   GC0030  
00125          88  RSMT-EOF                             VALUE 'END'.    GC0030  
00126      05  MST-REC-UPDT-SW               PIC XXX    VALUE 'NO '.    GC0030  
00127          88  MST-REC-NOT-UPDATED                  VALUE 'NO '.    GC0030  
00128      05  CHG-ADD-REQ-SW                PIC X      VALUE SPACES.   GC0030  
00129          88  CHANGE-REQUEST                       VALUE 'C'.      GC0030  
00130          88  ADD-REQUEST                          VALUE 'A'.      GC0030  
00131                                                                   GC0030  
00132  01  WRK-SUBSCRIPTS.                                              GC0030  
00133      05 I              VALUE +0        PIC S9(4)      COMP-3.     GC0030  
00134      05 J              VALUE +0        PIC S9(4)      COMP-3.     GC0030  
00135      05 K              VALUE +0        PIC S9(4)      COMP-3.     GC0030  
00136      05 SORT-VALUE     VALUE +0        PIC S9(4)      COMP-3.     GC0030  
00137      05 CHECK-VALUE    VALUE +0        PIC S9(4)      COMP-3.     GC0030  
00138                                                                   GC0030  
00139 /                                                                 GC0030  
00140  PROCEDURE DIVISION.                                              GC0030  
00141  1000-MAINLINE.                                                   GC0030  
00142      MOVE  'S'          TO  REQUEST-TYPE.                         GC0030  
00143      MOVE   8           TO  SET-RECORD-LENGTH.                    GC0030  
00144      MOVE   3           TO  SET-VALUE.                            GC0030  
00145      CALL  'TSGVSAM1'  USING  PARM-ONE  PARM-SET.                 GC0030  
00146                                                                   GC0030  
00147      IF  REQUEST-TYPE  NOT EQUAL  'S'                             GC0030  
00148          MOVE  SET-FEEDBACK-CODE  TO  ABEND-CODE                  GC0030  
00149          GO TO  9999-ERROR-RTN.                                   GC0030  
00150                                                                   GC0030  
00151      OPEN INPUT RSMT-FILE.                                        GC0030  
00152      MOVE  'O'          TO  REQUEST-TYPE.                         GC0030  
00153      CALL  'TSGVSAM1'  USING  PARM-ONE  PARM-TWO.                 GC0030  
00154                                                                   GC0030  
00155      IF  REQUEST-TYPE  NOT EQUAL  'O'                             GC0030  
00156          MOVE  FEEDBACK-CODE  TO  ABEND-CODE                      GC0030  
00157          GO TO  9999-ERROR-RTN.                                   GC0030  
00158                                                                   GC0030  
00159       PERFORM  0010-READ-RSMT-FILE THROUGH  0010-EXIT.            GC0030  
00160                                                                   GC0030  
00161       IF  RSMT-EOF                                                GC0030  
00162           NEXT SENTENCE                                           GC0030  
00163       ELSE                                                        GC0030  
00164           IF  XWRK-REC-SYS-TBL-MSTR-REC                           GC0030  
00165               NEXT SENTENCE                                       GC0030  
00166           ELSE                                                    GC0030  
00167               IF  XWRK-REC-SYS-TBL-DCCTABLE-REC                   GC0030  
00168                   NEXT SENTENCE                                   GC0030  
00169               ELSE                                                GC0030  
00170                   MOVE 1001 TO ABEND-CODE                         GC0030  
00171                   GO TO 9999-ERROR-RTN.                           GC0030  
00172                                                                   GC0030  
00173       PERFORM  0080-PROCESS  THROUGH  0080-EXIT                   GC0030  
00174                UNTIL RSMT-EOF.                                    GC0030  
00175                                                                   GC0030  
00176      CLOSE RSMT-FILE.                                             GC0030  
00177                                                                   GC0030  
00178      MOVE     'C'           TO   REQUEST-TYPE.                    GC0030  
00179      CALL  'TSGVSAM1'  USING  PARM-ONE  PARM-TWO.                 GC0030  
00180                                                                   GC0030  
00181      IF  REQUEST-TYPE  NOT EQUAL  'C'                             GC0030  
00182          MOVE  FEEDBACK-CODE  TO  ABEND-CODE                      GC0030  
00183          GO TO  9999-ERROR-RTN.                                   GC0030  
00184       GOBACK.                                                     GC0030  
00185  1000-EXIT. EXIT.                                                 GC0030  
00186 /                                                                 GC0030  
00187  0010-READ-RSMT-FILE.                                             GC0030  
00188      READ RSMT-FILE                                               GC0030  
00189           AT  END                                                 GC0030  
00190       MOVE 'END' TO EOF-SW.                                       GC0030  
00191  0010-EXIT. EXIT.                                                 GC0030  
00192 /                                                                 GC0030  
00193  0020-READ-SYSTEM-FILE.                                           GC0030  
00194                                                                   GC0030  
00195      MOVE  'R'          TO  REQUEST-TYPE.                         GC0030  
00196      MOVE  14           TO  RECORD-LENGTH.                        GC0030  
00197      MOVE  SEARCH-KEY   TO  VSAM-KEY-FIELD.                       GC0030  
00198      CALL  'TSGVSAM1'  USING  PARM-ONE  PARM-TWO.                 GC0030  
00199                                                                   GC0030  
00200                                                                   GC0030  
00201  0020-EXIT. EXIT.                                                 GC0030  
00202 /                                                                 GC0030  
00203  0050-WRITE-SYSTEM-RECORD.                                        GC0030  
00204                                                                   GC0030  
00205      CALL  'TSGVSAM1'  USING  PARM-ONE  PARM-TWO.                 GC0030  
00206                                                                   GC0030  
00207  0050-EXIT.                                                       GC0030  
00208       EXIT.                                                       GC0030  
00209 /                                                                 GC0030  
00210  0080-PROCESS.                                                    GC0030  
00211      IF  XWRK-REC-SYS-TBL-TAB                                     GC0030  
00212          PERFORM 0100-PROCESS-TAB-REC  THRU                       GC0030  
00213                  0100-EXIT                                        GC0030  
00214      ELSE                                                         GC0030  
00215          IF  XWRK-REC-SYS-TBL-MSTR-REC AND                        GC0030  
00216              MST-REC-NOT-UPDATED                                  GC0030  
00217              PERFORM 0200-PROCESS-MASTER-REC THRU                 GC0030  
00218                      0200-EXIT                                    GC0030  
00219          ELSE                                                     GC0030  
00220              IF  XWRK-REC-SYS-TBL-DCCTABLE-REC                    GC0030  
00221                  PERFORM 0500-PROCESS-TABLE-REC THRU              GC0030  
00222                          0500-EXIT                                GC0030  
00223              ELSE                                                 GC0030  
00224                  MOVE 0080    TO ABEND-CODE                       GC0030  
00225                  GO TO  9999-ERROR-RTN.                           GC0030  
00226                                                                   GC0030  
00227      PERFORM 0010-READ-RSMT-FILE  THRU                            GC0030  
00228              0010-EXIT.                                           GC0030  
00229                                                                   GC0030  
00230  0080-EXIT. EXIT.                                                 GC0030  
00231 /                                                                 GC0030  
00232  0100-PROCESS-TAB-REC.                                            GC0030  
00233                                                                   GC0030  
00234      IF  RSMT-TAB-ID        = '#XCON '                            GC0030  
00235          MOVE RSMT-ENTRY-COUNT   TO GX6-ENTRY-COUNT               GC0030  
00236          MOVE RSMT-TAB-REC       TO GX6-RECORD                    GC0030  
00237          PERFORM 0110-PROCESS-CDR-REC  THRU                       GC0030  
00238                  0110-EXIT                                        GC0030  
00239          GO TO  0100-EXIT.                                        GC0030  
00240                                                                   GC0030  
00241      IF  RSMT-TAB-ID        = '#XCOS '                            GC0030  
00242          MOVE RSMT-ENTRY-COUNT   TO GX7-ENTRY-COUNT               GC0030  
00243          MOVE RSMT-TAB-REC       TO GX7-RECORD                    GC0030  
00244          PERFORM 0120-PROCESS-CSR-REC  THRU                       GC0030  
00245                  0120-EXIT                                        GC0030  
00246          GO TO  0100-EXIT.                                        GC0030  
00247                                                                   GC0030  
00248      IF  RSMT-TAB-ID        = '#XDIP '                            GC0030  
00249          MOVE RSMT-ENTRY-COUNT   TO GX4-ENTRY-COUNT               GC0030  
00250          MOVE RSMT-TAB-REC       TO GX4-RECORD                    GC0030  
00251          PERFORM 0130-PROCESS-DIR-REC  THRU                       GC0030  
00252                  0130-EXIT                                        GC0030  
00253          GO TO  0100-EXIT.                                        GC0030  
00254                                                                   GC0030  
00255      IF  RSMT-TAB-ID        = '#XDOP '                            GC0030  
00256          MOVE RSMT-ENTRY-COUNT   TO GX5-ENTRY-COUNT               GC0030  
00257          MOVE RSMT-TAB-REC       TO GX5-RECORD                    GC0030  
00258          PERFORM 0140-PROCESS-DOR-REC  THRU                       GC0030  
00259                  0140-EXIT                                        GC0030  
00260          GO TO  0100-EXIT.                                        GC0030  
00261                                                                   GC0030  
00262      MOVE 0100    TO ABEND-CODE                                   GC0030  
00263      GO TO  9999-ERROR-RTN.                                       GC0030  
00264  0100-EXIT.                                                       GC0030  
00265       EXIT.                                                       GC0030  
00266  0105-DET-REQUEST-TYPE.                                           GC0030  
00267                                                                   GC0030  
00268       IF  GX8-SYS-TABULAR-ID (GX8-INDEX) = SEARCH-KEY             GC0030  
00269           MOVE GX8-PROV-STATUS-CODE (GX8-INDEX) TO  CHG-ADD-REQ-SWGC0030  
00270           GO TO 0105-EXIT.                                        GC0030  
00271                                                                   GC0030  
00272       IF  GX8-SYS-TABULAR-ID (GX8-INDEX) = HIGH-VALUES            GC0030  
00273           MOVE 0105  TO ABEND-CODE                                GC0030  
00274           GO TO 9999-ERROR-RTN.                                   GC0030  
00275                                                                   GC0030  
00276      SET GX8-INDEX  UP BY 1.                                      GC0030  
00277      GO TO 0105-DET-REQUEST-TYPE.                                 GC0030  
00278                                                                   GC0030  
00279  0105-EXIT.                                                       GC0030  
00280       EXIT.                                                       GC0030  
00281  0110-PROCESS-CDR-REC.                                            GC0030  
00282                                                                   GC0030  
00283      MOVE  GX6-TABULAR-PROVISION-ID  TO SEARCH-KEY.               GC0030  
00284      MOVE  SPACES                    TO  CHG-ADD-REQ-SW.          GC0030  
00285      SET GX8-INDEX  TO 1.                                         GC0030  
00286      PERFORM 0105-DET-REQUEST-TYPE  THRU                          GC0030  
00287              0105-EXIT.                                           GC0030  
00288                                                                   GC0030  
00289      IF  CHANGE-REQUEST                                           GC0030  
00290          PERFORM 0020-READ-SYSTEM-FILE  THRU                      GC0030  
00291                  0020-EXIT                                        GC0030  
00292          IF  REQUEST-TYPE EQUAL 'R'                               GC0030  
00293              MOVE  GX6-ENTRY-COUNT   TO  VSAM-ENTRY-COUNT         GC0030  
00294              MOVE  GC-XCON-REC       TO  VSAM-RECORD              GC0030  
00295              PERFORM 0300-UPDATE-TAB-RECORD  THRU                 GC0030  
00296                      0300-EXIT                                    GC0030  
00297              GO TO  0110-EXIT                                     GC0030  
00298          ELSE                                                     GC0030  
00299              MOVE  0110  TO ABEND-CODE                            GC0030  
00300              GO TO  9999-ERROR-RTN.                               GC0030  
00301                                                                   GC0030  
00302      IF  ADD-REQUEST                                              GC0030  
00303          MOVE  GX6-ENTRY-COUNT   TO  VSAM-ENTRY-COUNT             GC0030  
00304          MOVE  GC-XCON-REC       TO  VSAM-RECORD                  GC0030  
00305          PERFORM 0400-ADD-TAB-RECORD  THRU                        GC0030  
00306                  0400-EXIT                                        GC0030  
00307              GO TO  0110-EXIT.                                    GC0030  
00308                                                                   GC0030  
00309      MOVE 0111 TO ABEND-CODE                                      GC0030  
00310      GO TO  9999-ERROR-RTN.                                       GC0030  
00311                                                                   GC0030  
00312  0110-EXIT.                                                       GC0030  
00313       EXIT.                                                       GC0030  
00314 /                                                                 GC0030  
00315  0120-PROCESS-CSR-REC.                                            GC0030  
00316                                                                   GC0030  
00317      MOVE  GX7-TABULAR-PROVISION-ID  TO SEARCH-KEY.               GC0030  
00318      MOVE  SPACES                    TO  CHG-ADD-REQ-SW.          GC0030  
00319      SET GX8-INDEX  TO 1.                                         GC0030  
00320      PERFORM 0105-DET-REQUEST-TYPE  THRU                          GC0030  
00321              0105-EXIT.                                           GC0030  
00322                                                                   GC0030  
00323      IF  CHANGE-REQUEST                                           GC0030  
00324          PERFORM 0020-READ-SYSTEM-FILE  THRU                      GC0030  
00325                  0020-EXIT                                        GC0030  
00326          IF  REQUEST-TYPE EQUAL 'R'                               GC0030  
00327              MOVE  GX7-ENTRY-COUNT   TO  VSAM-ENTRY-COUNT         GC0030  
00328              MOVE  GC-XCOS-REC       TO  VSAM-RECORD              GC0030  
00329              PERFORM 0300-UPDATE-TAB-RECORD  THRU                 GC0030  
00330                      0300-EXIT                                    GC0030  
00331              GO TO  0120-EXIT                                     GC0030  
00332          ELSE                                                     GC0030  
00333              MOVE  0120  TO ABEND-CODE                            GC0030  
00334              GO TO  9999-ERROR-RTN.                               GC0030  
00335                                                                   GC0030  
00336      IF  ADD-REQUEST                                              GC0030  
00337          MOVE  GX7-ENTRY-COUNT   TO  VSAM-ENTRY-COUNT             GC0030  
00338          MOVE  GC-XCOS-REC       TO  VSAM-RECORD                  GC0030  
00339          PERFORM 0400-ADD-TAB-RECORD  THRU                        GC0030  
00340                  0400-EXIT                                        GC0030  
00341              GO TO  0120-EXIT.                                    GC0030  
00342                                                                   GC0030  
00343      MOVE  0121  TO ABEND-CODE                                    GC0030  
00344      GO TO  9999-ERROR-RTN.                                       GC0030  
00345                                                                   GC0030  
00346  0120-EXIT.                                                       GC0030  
00347       EXIT.                                                       GC0030  
00348 /                                                                 GC0030  
00349  0130-PROCESS-DIR-REC.                                            GC0030  
00350                                                                   GC0030  
00351      MOVE  GX4-TABULAR-PROVISION-ID  TO SEARCH-KEY.               GC0030  
00352      MOVE  SPACES                    TO  CHG-ADD-REQ-SW.          GC0030  
00353      SET GX8-INDEX  TO 1.                                         GC0030  
00354      PERFORM 0105-DET-REQUEST-TYPE  THRU                          GC0030  
00355              0105-EXIT.                                           GC0030  
00356                                                                   GC0030  
00357      IF  CHANGE-REQUEST                                           GC0030  
00358          PERFORM 0020-READ-SYSTEM-FILE  THRU                      GC0030  
00359                  0020-EXIT                                        GC0030  
00360          IF  REQUEST-TYPE EQUAL 'R'                               GC0030  
00361              MOVE  GX4-ENTRY-COUNT   TO  VSAM-ENTRY-COUNT         GC0030  
00362              MOVE  GC-XDIP-REC       TO  VSAM-RECORD              GC0030  
00363              PERFORM 0300-UPDATE-TAB-RECORD  THRU                 GC0030  
00364                      0300-EXIT                                    GC0030  
00365              GO TO  0130-EXIT                                     GC0030  
00366          ELSE                                                     GC0030  
00367              MOVE  0130  TO ABEND-CODE                            GC0030  
00368              GO TO  9999-ERROR-RTN.                               GC0030  
00369                                                                   GC0030  
00370      IF  ADD-REQUEST                                              GC0030  
00371          MOVE  GX4-ENTRY-COUNT   TO  VSAM-ENTRY-COUNT             GC0030  
00372          MOVE  GC-XDIP-REC       TO  VSAM-RECORD                  GC0030  
00373          PERFORM 0400-ADD-TAB-RECORD  THRU                        GC0030  
00374                  0400-EXIT                                        GC0030  
00375              GO TO  0130-EXIT.                                    GC0030  
00376                                                                   GC0030  
00377      MOVE 0131 TO ABEND-CODE                                      GC0030  
00378      GO TO  9999-ERROR-RTN.                                       GC0030  
00379                                                                   GC0030  
00380  0130-EXIT.                                                       GC0030  
00381       EXIT.                                                       GC0030  
00382 /                                                                 GC0030  
00383  0140-PROCESS-DOR-REC.                                            GC0030  
00384                                                                   GC0030  
00385      MOVE  GX5-TABULAR-PROVISION-ID  TO SEARCH-KEY.               GC0030  
00386      MOVE  SPACES                    TO  CHG-ADD-REQ-SW.          GC0030  
00387      SET GX8-INDEX  TO 1.                                         GC0030  
00388      PERFORM 0105-DET-REQUEST-TYPE  THRU                          GC0030  
00389              0105-EXIT.                                           GC0030  
00390                                                                   GC0030  
00391      IF  CHANGE-REQUEST                                           GC0030  
00392          PERFORM 0020-READ-SYSTEM-FILE  THRU                      GC0030  
00393                  0020-EXIT                                        GC0030  
00394          IF  REQUEST-TYPE EQUAL 'R'                               GC0030  
00395              MOVE  GX5-ENTRY-COUNT   TO  VSAM-ENTRY-COUNT         GC0030  
00396              MOVE  GC-XDOP-REC       TO  VSAM-RECORD              GC0030  
00397              PERFORM 0300-UPDATE-TAB-RECORD  THRU                 GC0030  
00398                      0300-EXIT                                    GC0030  
00399              GO TO  0140-EXIT                                     GC0030  
00400          ELSE                                                     GC0030  
00401              MOVE  0140  TO ABEND-CODE                            GC0030  
00402              GO TO  9999-ERROR-RTN.                               GC0030  
00403                                                                   GC0030  
00404      IF  ADD-REQUEST                                              GC0030  
00405          MOVE  GX5-ENTRY-COUNT   TO  VSAM-ENTRY-COUNT             GC0030  
00406          MOVE  GC-XDOP-REC       TO  VSAM-RECORD                  GC0030  
00407          PERFORM 0400-ADD-TAB-RECORD  THRU                        GC0030  
00408                  0400-EXIT                                        GC0030  
00409              GO TO  0140-EXIT.                                    GC0030  
00410                                                                   GC0030  
00411      MOVE 0141 TO ABEND-CODE                                      GC0030  
00412      GO TO  9999-ERROR-RTN.                                       GC0030  
00413                                                                   GC0030  
00414  0140-EXIT.                                                       GC0030  
00415       EXIT.                                                       GC0030  
00416 /                                                                 GC0030  
00417  0200-PROCESS-MASTER-REC.                                         GC0030  
00418                                                                   GC0030  
00419      IF  RSMT-TAB-ID          = 'XSTMR '                          GC0030  
00420          NEXT SENTENCE                                            GC0030  
00421      ELSE                                                         GC0030  
00422          MOVE 0200            TO ABEND-CODE                       GC0030  
00423          GO TO  9999-ERROR-RTN.                                   GC0030  
00424                                                                   GC0030  
00425       MOVE RSMT-TAB-ID        TO SEARCH-ID.                       GC0030  
00426       MOVE RSMT-TAB-SLOT      TO SEARCH-SLOT.                     GC0030  
00427       PERFORM 0020-READ-SYSTEM-FILE  THRU                         GC0030  
00428               0020-EXIT.                                          GC0030  
00429       IF  REQUEST-TYPE          = 'R'                             GC0030  
00430           MOVE VSAM-ENTRY-COUNT TO GX82-ENTRY-COUNT               GC0030  
00431           MOVE VSAM-RECORD      TO GCXSTMR-REC2                   GC0030  
00432       ELSE                                                        GC0030  
00433           MOVE 0201           TO ABEND-CODE                       GC0030  
00434           GO TO  9999-ERROR-RTN.                                  GC0030  
00435                                                                   GC0030  
00436       MOVE 'YES'              TO MST-REC-UPDT-SW.                 GC0030  
00437       MOVE RSMT-ENTRY-COUNT   TO GX8-ENTRY-COUNT.                 GC0030  
00438       MOVE RSMT-TAB-REC       TO GX8-RECORD.                      GC0030  
00439                                                                   GC0030  
00440       PERFORM 0210-UPDATE-MST-REC  THRU                           GC0030  
00441               0210-EXIT                                           GC0030  
00442          VARYING GX8-INDEX FROM 1 BY 1 UNTIL                      GC0030  
00443          GX8-PROVISION-ID (GX8-INDEX) = HIGH-VALUES.              GC0030  
00444                                                                   GC0030  
00445       IF  VSAM-ENTRY-COUNT = GX82-ENTRY-COUNT                     GC0030  
00446           NEXT SENTENCE                                           GC0030  
00447       ELSE                                                        GC0030  
00448           PERFORM 0250-SORT-MSTR-REC  THRU                        GC0030  
00449                   0250-EXIT.                                      GC0030  
00450 *                                                                 GC0030  
00451       PERFORM 0220-FORMAT-MSTR-REC THRU                           GC0030  
00452               0220-EXIT.                                          GC0030  
00453 *                                                                 GC0030  
00454  0200-EXIT.                                                       GC0030  
00455       EXIT.                                                       GC0030  
00456  0210-UPDATE-MST-REC.                                             GC0030  
00457                                                                   GC0030  
00458      IF GX8-PROVISION-ID (GX8-INDEX) = HIGH-VALUES                GC0030  
00459         GO TO 0210-EXIT.                                          GC0030  
00460                                                                   GC0030  
00461      MOVE GX8-SYS-TABULAR-ID (GX8-INDEX)   TO SEARCH-KEY.         GC0030  
00462      PERFORM 0020-READ-SYSTEM-FILE  THRU                          GC0030  
00463              0020-EXIT.                                           GC0030  
00464                                                                   GC0030  
00465  0210-FIND-MST-REC.                                               GC0030  
00466                                                                   GC0030  
00467      IF  GX8-STAT-CODE-ADD (GX8-INDEX) AND                        GC0030  
00468          REQUEST-TYPE EQUAL   'R'                                 GC0030  
00469          MOVE 0210           TO ABEND-CODE                        GC0030  
00470          GO TO  9999-ERROR-RTN.                                   GC0030  
00471                                                                   GC0030  
00472      IF  GX8-STAT-CODE-ADD (GX8-INDEX) AND                        GC0030  
00473          REQUEST-TYPE EQUAL   '3'                                 GC0030  
00474          ADD 1 TO GX82-ENTRY-COUNT                                GC0030  
00475          MOVE GX8-SYS-TABULAR-ID (GX8-INDEX) TO                   GC0030  
00476               GX82-SYS-TABULAR-ID (GX82-ENTRY-COUNT)              GC0030  
00477          MOVE SPACES TO  GX82-PROV-STATUS-CODE (GX82-ENTRY-COUNT).GC0030  
00478                                                                   GC0030  
00479      IF  GX8-STAT-CODE-CHG (GX8-INDEX) AND                        GC0030  
00480          REQUEST-TYPE EQUAL   '3'                                 GC0030  
00481          MOVE 0211           TO ABEND-CODE                        GC0030  
00482          GO TO  9999-ERROR-RTN.                                   GC0030  
00483  0210-EXIT.                                                       GC0030  
00484       EXIT.                                                       GC0030  
00485  0220-FORMAT-MSTR-REC.                                            GC0030  
00486                                                                   GC0030  
00487       MOVE GX8-DT-OF-LAST-CHANGE TO GX82-DT-OF-LAST-CHANGE.       GC0030  
00488       MOVE GX82-ENTRY-COUNT      TO VSAM-ENTRY-COUNT.             GC0030  
00489       MOVE GCXSTMR-REC2          TO VSAM-RECORD.                  GC0030  
00490       MOVE 'U'                   TO REQUEST-TYPE.                 GC0030  
00491 *     COMPUTE RECORD-LENGTH = 4 + 40 + (GX82-ENTRY-COUNT * 11).   GC0030  
00492       COMPUTE RECORD-LENGTH = 4 + 49 + (GX82-ENTRY-COUNT * 11).   GC0030  
00493                                                                   GC0030  
00494       PERFORM 0050-WRITE-SYSTEM-RECORD THRU                       GC0030  
00495               0050-EXIT.                                          GC0030  
00496                                                                   GC0030  
00497       IF REQUEST-TYPE = 'U'                                       GC0030  
00498          NEXT SENTENCE                                            GC0030  
00499       ELSE                                                        GC0030  
00500          MOVE 0202 TO ABEND-CODE                                  GC0030  
00501          GO TO 9999-ERROR-RTN.                                    GC0030  
00502                                                                   GC0030  
00503  0220-EXIT.                                                       GC0030  
00504       EXIT.                                                       GC0030  
00505 /                                                                 GC0030  
00506  0250-SORT-MSTR-REC.                                              GC0030  
00507       COMPUTE  SORT-VALUE  = GX82-ENTRY-COUNT  - 1.               GC0030  
00508       COMPUTE  CHECK-VALUE = SORT-VALUE + 1.                      GC0030  
00509                                                                   GC0030  
00510       PERFORM  0260-CHECK-TAB-VALUE THRU 0260-EXIT                GC0030  
00511                VARYING I FROM 1 BY 1 UNTIL                        GC0030  
00512                        I IS GREATER THAN SORT-VALUE.              GC0030  
00513                                                                   GC0030  
00514  0250-EXIT.                                                       GC0030  
00515       EXIT.                                                       GC0030  
00516  0260-CHECK-TAB-VALUE.                                            GC0030  
00517                                                                   GC0030  
00518       COMPUTE  K = I + 1.                                         GC0030  
00519                                                                   GC0030  
00520       PERFORM  0270-MOVE-TAB-VALUE THRU 0270-EXIT                 GC0030  
00521                VARYING J FROM K BY 1 UNTIL                        GC0030  
00522                        J IS GREATER THAN CHECK-VALUE.             GC0030  
00523                                                                   GC0030  
00524  0260-EXIT.                                                       GC0030  
00525       EXIT.                                                       GC0030  
00526  0270-MOVE-TAB-VALUE.                                             GC0030  
00527                                                                   GC0030  
00528       IF GX82-TABULAR-PROVISION-ID (I) GREATER THAN               GC0030  
00529               GX82-TABULAR-PROVISION-ID (J)                       GC0030  
00530          MOVE GX82-TABULAR-PROVISION-ID (I)  TO HOLD-TAB-ID       GC0030  
00531          MOVE GX82-TABULAR-PROVISION-ID (J)  TO                   GC0030  
00532               GX82-TABULAR-PROVISION-ID (I)                       GC0030  
00533          MOVE HOLD-TAB-ID TO   GX82-TABULAR-PROVISION-ID (J).     GC0030  
00534                                                                   GC0030  
00535  0270-EXIT.                                                       GC0030  
00536       EXIT.                                                       GC0030  
00537 /                                                                 GC0030  
00538  0300-UPDATE-TAB-RECORD.                                          GC0030  
00539                                                                   GC0030  
00540      MOVE 'U'                TO  REQUEST-TYPE.                    GC0030  
00541 *    COMPUTE  RECORD-LENGTH = 47 + (9 * VSAM-ENTRY-COUNT).        GC0030  
00542      COMPUTE  RECORD-LENGTH = 56 + (10 * VSAM-ENTRY-COUNT).       GC0030  
00543                                                                   GC0030  
00544      PERFORM 0050-WRITE-SYSTEM-RECORD THRU                        GC0030  
00545              0050-EXIT.                                           GC0030  
00546                                                                   GC0030  
00547      IF  REQUEST-TYPE EQUAL 'U'                                   GC0030  
00548          NEXT SENTENCE                                            GC0030  
00549      ELSE                                                         GC0030  
00550          MOVE  0300  TO ABEND-CODE                                GC0030  
00551          GO TO  9999-ERROR-RTN.                                   GC0030  
00552                                                                   GC0030  
00553  0300-EXIT. EXIT.                                                 GC0030  
00554 /                                                                 GC0030  
00555  0400-ADD-TAB-RECORD.                                             GC0030  
00556                                                                   GC0030  
00557      MOVE 'W'                TO  REQUEST-TYPE.                    GC0030  
00558 *    COMPUTE  RECORD-LENGTH = 47 + (9 * VSAM-ENTRY-COUNT).        GC0030  
00559      COMPUTE  RECORD-LENGTH = 56 + (10 * VSAM-ENTRY-COUNT).       GC0030  
00560                                                                   GC0030  
00561      PERFORM 0050-WRITE-SYSTEM-RECORD THRU                        GC0030  
00562              0050-EXIT.                                           GC0030  
00563                                                                   GC0030  
00564      IF  REQUEST-TYPE EQUAL 'W'                                   GC0030  
00565          NEXT SENTENCE                                            GC0030  
00566      ELSE                                                         GC0030  
00567          MOVE  0400  TO ABEND-CODE                                GC0030  
00568          GO TO  9999-ERROR-RTN.                                   GC0030  
00569                                                                   GC0030  
00570                                                                   GC0030  
00571  0400-EXIT. EXIT.                                                 GC0030  
00572 /                                                                 GC0030  
00573  0500-PROCESS-TABLE-REC.                                          GC0030  
00574                                                                   GC0030  
00575      IF  RSMT-TAB-ID        = '#TXCON'                            GC0030  
00576          MOVE RSMT-ENTRY-COUNT   TO GT1-ENTRY-COUNT               GC0030  
00577          MOVE RSMT-TAB-REC       TO GT1-RECORD                    GC0030  
00578          PERFORM 0510-PROCESS-TXCON-REC  THRU                     GC0030  
00579                  0510-EXIT                                        GC0030  
00580          GO TO  0500-EXIT.                                        GC0030  
00581                                                                   GC0030  
00582      IF  RSMT-TAB-ID        = '#TXCOS'                            GC0030  
00583          MOVE RSMT-ENTRY-COUNT   TO GT2-ENTRY-COUNT               GC0030  
00584          MOVE RSMT-TAB-REC       TO GT2-RECORD                    GC0030  
00585          PERFORM 0520-PROCESS-TXCOS-REC  THRU                     GC0030  
00586                  0520-EXIT                                        GC0030  
00587          GO TO  0500-EXIT.                                        GC0030  
00588                                                                   GC0030  
00589      IF  RSMT-TAB-ID        = '#TXDIP'                            GC0030  
00590          MOVE RSMT-ENTRY-COUNT   TO GT3-ENTRY-COUNT               GC0030  
00591          MOVE RSMT-TAB-REC       TO GT3-RECORD                    GC0030  
00592          PERFORM 0530-PROCESS-TXDIP-REC  THRU                     GC0030  
00593                  0530-EXIT                                        GC0030  
00594          GO TO  0500-EXIT.                                        GC0030  
00595                                                                   GC0030  
00596      IF  RSMT-TAB-ID        = '#TXDOP'                            GC0030  
00597          MOVE RSMT-ENTRY-COUNT   TO GT4-ENTRY-COUNT               GC0030  
00598          MOVE RSMT-TAB-REC       TO GT4-RECORD                    GC0030  
00599          PERFORM 0540-PROCESS-TXDOP-REC  THRU                     GC0030  
00600                  0540-EXIT                                        GC0030  
00601          GO TO  0500-EXIT.                                        GC0030  
00602                                                                   GC0030  
00603      MOVE 0500    TO ABEND-CODE                                   GC0030  
00604      GO TO  9999-ERROR-RTN.                                       GC0030  
00605  0500-EXIT.                                                       GC0030  
00606       EXIT.                                                       GC0030  
00607 /                                                                 GC0030  
00608  0510-PROCESS-TXCON-REC.                                          GC0030  
00609      MOVE  GT1-TABULAR-PROVISION-ID  TO SEARCH-KEY.               GC0030  
00610      PERFORM 0020-READ-SYSTEM-FILE  THRU                          GC0030  
00611              0020-EXIT.                                           GC0030  
00612      IF  REQUEST-TYPE EQUAL 'R'                                   GC0030  
00613          MOVE  GT1-ENTRY-COUNT   TO  VSAM-ENTRY-COUNT             GC0030  
00614          MOVE  GC-TCON-REC       TO  VSAM-RECORD                  GC0030  
00615          PERFORM 0600-UPDATE-TABLE-RECORD  THRU                   GC0030  
00616                  0600-EXIT                                        GC0030  
00617          GO TO  0510-EXIT                                         GC0030  
00618          ELSE                                                     GC0030  
00619              MOVE  0510  TO ABEND-CODE                            GC0030  
00620              GO TO  9999-ERROR-RTN.                               GC0030  
00621  0510-EXIT.                                                       GC0030  
00622       EXIT.                                                       GC0030  
00623 /                                                                 GC0030  
00624  0520-PROCESS-TXCOS-REC.                                          GC0030  
00625      MOVE  GT2-TABULAR-PROVISION-ID  TO SEARCH-KEY.               GC0030  
00626      PERFORM 0020-READ-SYSTEM-FILE  THRU                          GC0030  
00627              0020-EXIT.                                           GC0030  
00628      IF  REQUEST-TYPE EQUAL 'R'                                   GC0030  
00629          MOVE  GT2-ENTRY-COUNT   TO  VSAM-ENTRY-COUNT             GC0030  
00630          MOVE  GC-TCOS-REC       TO  VSAM-RECORD                  GC0030  
00631          PERFORM 0600-UPDATE-TABLE-RECORD  THRU                   GC0030  
00632                  0600-EXIT                                        GC0030  
00633          GO TO  0520-EXIT                                         GC0030  
00634          ELSE                                                     GC0030  
00635              MOVE  0520  TO ABEND-CODE                            GC0030  
00636              GO TO  9999-ERROR-RTN.                               GC0030  
00637  0520-EXIT.                                                       GC0030  
00638       EXIT.                                                       GC0030  
00639 /                                                                 GC0030  
00640  0530-PROCESS-TXDIP-REC.                                          GC0030  
00641      MOVE  GT3-TABULAR-PROVISION-ID  TO SEARCH-KEY.               GC0030  
00642      PERFORM 0020-READ-SYSTEM-FILE  THRU                          GC0030  
00643              0020-EXIT.                                           GC0030  
00644      IF  REQUEST-TYPE EQUAL 'R'                                   GC0030  
00645          MOVE  GT3-ENTRY-COUNT   TO  VSAM-ENTRY-COUNT             GC0030  
00646          MOVE  GC-TDIP-REC       TO  VSAM-RECORD                  GC0030  
00647          PERFORM 0600-UPDATE-TABLE-RECORD  THRU                   GC0030  
00648                  0600-EXIT                                        GC0030  
00649          GO TO  0530-EXIT                                         GC0030  
00650          ELSE                                                     GC0030  
00651              MOVE  0530  TO ABEND-CODE                            GC0030  
00652              GO TO  9999-ERROR-RTN.                               GC0030  
00653  0530-EXIT.                                                       GC0030  
00654       EXIT.                                                       GC0030  
00655 /                                                                 GC0030  
00656  0540-PROCESS-TXDOP-REC.                                          GC0030  
00657      MOVE  GT4-TABULAR-PROVISION-ID  TO SEARCH-KEY.               GC0030  
00658      PERFORM 0020-READ-SYSTEM-FILE  THRU                          GC0030  
00659              0020-EXIT.                                           GC0030  
00660      IF  REQUEST-TYPE EQUAL 'R'                                   GC0030  
00661          MOVE  GT4-ENTRY-COUNT   TO  VSAM-ENTRY-COUNT             GC0030  
00662          MOVE  GC-TDOP-REC       TO  VSAM-RECORD                  GC0030  
00663          PERFORM 0600-UPDATE-TABLE-RECORD  THRU                   GC0030  
00664                  0600-EXIT                                        GC0030  
00665          GO TO  0540-EXIT                                         GC0030  
00666          ELSE                                                     GC0030  
00667              MOVE  0540  TO ABEND-CODE                            GC0030  
00668              GO TO  9999-ERROR-RTN.                               GC0030  
00669  0540-EXIT.                                                       GC0030  
00670       EXIT.                                                       GC0030  
00671 /                                                                 GC0030  
00672  0600-UPDATE-TABLE-RECORD.                                        GC0030  
00673                                                                   GC0030  
00674      MOVE 'U'                TO  REQUEST-TYPE.                    GC0030  
00675 *    COMPUTE  RECORD-LENGTH = 44 + (6 * VSAM-ENTRY-COUNT).        GC0030  
00676      COMPUTE  RECORD-LENGTH = 53 + (6 * VSAM-ENTRY-COUNT).        GC0030  
00677                                                                   GC0030  
00678      PERFORM 0050-WRITE-SYSTEM-RECORD THRU                        GC0030  
00679              0050-EXIT.                                           GC0030  
00680                                                                   GC0030  
00681      IF  REQUEST-TYPE EQUAL 'U'                                   GC0030  
00682          NEXT SENTENCE                                            GC0030  
00683      ELSE                                                         GC0030  
00684          MOVE  0600  TO ABEND-CODE                                GC0030  
00685          GO TO  9999-ERROR-RTN.                                   GC0030  
00686                                                                   GC0030  
00687  0600-EXIT. EXIT.                                                 GC0030  
00688 /                                                                 GC0030  
00689  9999-ERROR-RTN.                                                  GC0030  
00690                                                                   GC0030  
00691      CALL  'TSGEND' USING  ABEND-CODE.                            GC0030  
00692                                                                   GC0030  
00693  9999-ERROR-RTN-EXIT. EXIT.                                       GC0030  
