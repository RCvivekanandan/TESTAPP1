00001  IDENTIFICATION DIVISION.                                         12/09/02
00002  PROGRAM-ID. GC024000.                                            GC024000
00003 *THIS IS A COBOL/2 PROGRAM                                           LV002
00004  AUTHOR. ED WITKUS.                                               GC024000
00005  INSTALLATION. HCSC.                                              GC024000
00006  DATE-WRITTEN.  MAY, 1986.                                        GC024000
00007  DATE-COMPILED.                                                   GC024000
00008 *REMARKS.                                                         GC024000
00009 *    THIS PROGRAM IS THE DRIVER FOR THE BATCH FILE DISCREPANCY    GC024000
00010 *    SYSTEM.                                                      GC024000
00011 ****** #2 FOR THE BENEFIT PROVISIONS.                             GC024000
00012 ******************************************************************GC024000
00013 ******************************************************************GC024000
00014 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC024000
00015 *       *-*         U P D A T E   H I S T O R Y         *-*      *GC024000
00016 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC024000
00017 *                                                                *GC024000
00018 * CHG NUM    DATE    BY   *---------DESCRIPTION------------------*GC024000
00019 * ______   ________  ___  _______________________________________*GC024000
00020 *  D116    09/09/87  FRY   PASS OPERATOR-ID PARAMETERS           *GC024000
00021 *  D12009  09/10/91  GDM   INCREASE L3-FAM-REL TO 2 POSITIONS    *GC024000
00022 *          01/18/95  GDM   CONVERT TO COBOL II                   *GC024000
00023 *                                                                *GC024000
00024 *  14726/                                                        *GC024000
00025 *  15057   09/11/97  AB    CODE ADDED TO SUPPORT THE YEAR        *GC024000
00026 *                          2000 AND THE EXPANSION OF THE         *GC024000
00027 *                          CONTRACT KEY TO SUPPORT THE TX        *GC024000
00028 *                          MERGER.                               *GC024000
00029 *                                                                *GC024000
00029 *          06/14/16  KIKI  RECOMPILE ONLY - CHANGED PGM GC024020 *GC024000
00029 *                                                                *GC024000
00029 *  P22147  04/27/17  TROY  RECOMPILE ONLY - CHANGED PGM GC024020 *GC024000
00029 *                                                                *GC024000
00029 *  P22845  07/18/18  SRI   RECOMPILE ONLY - CHANGED PGM GC024020 *GC024000
00029 *                                                                *GC024000
00029 *  P23672  06/25/19  SRI   RECOMPILE ONLY - CHANGED PGM GC024020 *GC024000
00029 *                                                                *GC024000
00029 *  P23805  06/25/19  SRI   RECOMPILE ONLY - CHANGED PGM GC024020 *GC024000
ED0624*                                                                *        
ED0624*  P287767 06/26/24  ED    RECOMPILE ONLY - CHANGED PGM GC024020 *        
00029 *                                                                *GC024000
ED0624*BBDA-6604904/13/26  TM    RECOMPILE ONLY - CHANGED PGM GC024020 *        
00029 *                                                                *GC024000
00030 ******************************************************************GC024000
00031  ENVIRONMENT DIVISION.                                            GC024000
00032  CONFIGURATION SECTION.                                           GC024000
00033  SOURCE-COMPUTER. IBM-370.                                        GC024000
00034  OBJECT-COMPUTER. IBM-370.                                        GC024000
00035  INPUT-OUTPUT SECTION.                                            GC024000
00036  FILE-CONTROL.                                                    GC024000
00037      SELECT RLSE-CONTRACT-FILE                                    GC024000
00038                              ASSIGN TO UT-S-GC0240A.              GC024000
00039      SELECT RLSE-GRP-SPEC-FILE                                    GC024000
00040                              ASSIGN TO UT-S-GC0240B.              GC024000
00041      SELECT RLSE-BEN-PROV-FILE                                    GC024000
00042                              ASSIGN TO UT-S-GC0240C.              GC024000
00043 /                                                                 GC024000
00044  DATA DIVISION.                                                   GC024000
00045  FILE SECTION.                                                    GC024000
00046                                                                   GC024000
00047  FD  RLSE-CONTRACT-FILE                                           GC024000
00048      LABEL RECORDS ARE STANDARD                                   GC024000
00049      RECORDING MODE IS V                                          GC024000
00050      BLOCK CONTAINS 0 RECORDS.                                    GC024000
00051  01  RLSE-CONTRACT-REC.                                           GC024000
00052      COPY GCWRKDCC.                                               GC024000
00053      COPY GCCONTRC.                                               GC024000
00054                                                                   GC024000
00055 /                                                                 GC024000
00056  FD  RLSE-GRP-SPEC-FILE                                           GC024000
00057      LABEL RECORDS ARE STANDARD                                   GC024000
00058      RECORDING MODE IS V                                          GC024000
00059      BLOCK CONTAINS 0 RECORDS.                                    GC024000
00060  01  RLSE-GRP-SPEC-REC.                                           GC024000
00061      COPY GCWRKDC2.                                               GC024000
00062      COPY GCGROUPC.                                               GC024000
00063                                                                   GC024000
00064 /                                                                 GC024000
00065  FD  RLSE-BEN-PROV-FILE                                           GC024000
00066      LABEL RECORDS ARE STANDARD                                   GC024000
00067      RECORDING MODE IS V                                          GC024000
00068      BLOCK CONTAINS 0 RECORDS.                                    GC024000
00069  01  RLSE-BEN-PROV-REC.                                           GC024000
00070      COPY GCWRKDC3.                                               GC024000
00071      COPY GCBENPVC.                                               GC024000
00072                                                                   GC024000
00073 /                                                                 GC024000
00074  WORKING-STORAGE SECTION.                                         GC024000
00075                                                                   GC024000
00076  01  FILLER                    PIC X(24) VALUE                    GC024000
00077      'GC024000 WORKING STORAGE'.                                  GC024000
00078                                                                   GC024000
00079  01  EIGHTY-EIGHTS.                                               GC024000
00080      05  CON-EOF-SW            PIC X(3) VALUE 'NO '.              GC024000
00081          88  CON-EOF                    VALUE 'YES'.              GC024000
00082      05  GRP-EOF-SW            PIC X(3) VALUE 'NO '.              GC024000
00083          88  GRP-EOF                    VALUE 'YES'.              GC024000
00084      05  BEN-PROV-EOF-SW       PIC X(3) VALUE 'NO '.              GC024000
00085          88  BEN-PROV-EOF               VALUE 'YES'.              GC024000
00086                                                                   GC024000
00087  01  COUNTERS.                                                    GC024000
00088      05  CONTRACT-READ-COUNT   PIC 9(7) VALUE ZEROS.              GC024000
00089      05  GRPSPC-READ-COUNT     PIC 9(7) VALUE ZEROS.              GC024000
00090      05  BEN-PROV-READ-COUNT   PIC 9(7) VALUE ZEROS.              GC024000
00091                                                                   GC024000
00092  01  DISP-DATE-TIME-AREA.                                         GC024000
00093      05  H-DATE.                                                  GC024000
00094          10  D-YR              PIC 99.                            GC024000
00095          10  D-MO              PIC 99.                            GC024000
00096          10  D-DA              PIC 99.                            GC024000
00097      05  D-DATE.                                                  GC024000
00098          10  D-MO              PIC Z9.                            GC024000
00099          10  FL                PIC X  VALUE '/'.                  GC024000
00100          10  D-DA              PIC 99.                            GC024000
00101          10  FL                PIC X  VALUE '/'.                  GC024000
00102          10  D-YR              PIC 99.                            GC024000
00103      05  H-TIME.                                                  GC024000
00104          10  D-HR              PIC 99.                            GC024000
00105          10  D-MIN             PIC 99.                            GC024000
00106          10  D-SEC             PIC 99.                            GC024000
00107      05  D-TIME.                                                  GC024000
00108          10  D-HR              PIC Z9.                            GC024000
00109          10  FL                PIC X  VALUE ':'.                  GC024000
00110          10  D-MIN             PIC 99.                            GC024000
00111          10  FL                PIC X  VALUE ':'.                  GC024000
00112          10  D-SEC             PIC 99.                            GC024000
00113                                                                   GC024000
00114  01  STATUS-CODES.                                                GC024000
00115      05 GC024010-IND           PIC X  VALUE SPACE.                GC024000
00116      05 GC024020-IND           PIC X  VALUE SPACE.                GC024000
00117      05 GC024030-IND           PIC X  VALUE SPACE.                GC024000
00118                                                                   GC024000
00119  01  LINK-AREA-3.                                                 GC024000
00120      05 GC024025-IND           PIC X  VALUE SPACE.                GC024000
00121      05 LINK-3-CONTRACT-KEY.                                      GC024000
00122         10 L3-PLAN-CODE        PIC X(3).                          GC024000
00123         10 L3-GRP-NBR          PIC X(9).                          GC024000
00124         10 L3-SECTN-NO         PIC X(5).                          GC024000
00125         10 L3-PKG-CODE         PIC X(3).                          GC024000
00126         10 L3-LOB              PIC X.                             GC024000
00127         10 L3-PRV-CTL          PIC XX.                            GC024000
00128         10 L3-FAM-REL          PIC XX.                            GC024000
00129         10 L3-EFFECTIVE-DT.                                       GC024000
00130            15 L3-EFFDT-CC      PIC X.                             GC024000
00131            15 L3-EFF-DT        PIC S9(5) COMP-3.                  GC024000
00132         10 L3-EFFDT-CEN REDEFINES                                 GC024000
00133             L3-EFFECTIVE-DT    PIC S9(7) COMP-3.                  GC024000
00134      05 LINK-3-BEN-PROV-ID.                                       GC024000
00135         10 L3-BEN-PROV-ID      PIC X(6).                          GC024000
00136         10 L3-BEN-PROV-SLOT    PIC S9(7) COMP-3.                  GC024000
00137                                                                   GC024000
00138  01  ABEND-CODE                PIC S9(4) COMP VALUE ZEROS.        GC024000
00139                                                                   GC024000
00140 /                                                                 GC024000
00141  LINKAGE SECTION.                                                 GC024000
00142                                                                   GC024000
00143 /                                                                 GC024000
00144  PROCEDURE DIVISION.                                              GC024000
00145                                                                   GC024000
00146  0000-MAINLINE.                                                   GC024000
00147                                                                   GC024000
00148      PERFORM 9010-ACCEPT-DATE-TIME THRU 9010-EXIT.                GC024000
00149      DISPLAY 'START UP FOR GC024000 AT ' D-TIME ' ON ' D-DATE.    GC024000
00150                                                                   GC024000
00151 *** OPEN DISCPREPANCY FILE IN GC024030.                           GC024000
00152      MOVE 'O' TO GC024030-IND.                                    GC024000
00153      CALL 'GC024030' USING GC024030-IND.                          GC024000
00154                                                                   GC024000
00155      PERFORM 1000-PROCESS-CONTRACT THRU 1000-EXIT.                GC024000
00156                                                                   GC024000
00157      PERFORM 2000-PROCESS-GRPSPC THRU 2000-EXIT.                  GC024000
00158                                                                   GC024000
00159      PERFORM 3000-PROCESS-BEN-PROV THRU 3000-EXIT.                GC024000
00160                                                                   GC024000
00161      DISPLAY 'CONTRACT RECORDS READ = ' CONTRACT-READ-COUNT.      GC024000
00162      DISPLAY 'GRPSPC RECORDS READ   = ' GRPSPC-READ-COUNT.        GC024000
00163      DISPLAY 'BENPROV RECS READ     = ' BEN-PROV-READ-COUNT.      GC024000
00164                                                                   GC024000
00165 *** CLOSE DISCPREPANCY FILE IN GC024030.                          GC024000
00166      MOVE 'C' TO GC024030-IND.                                    GC024000
00167      CALL 'GC024030' USING GC024030-IND.                          GC024000
00168                                                                   GC024000
00169      PERFORM 9010-ACCEPT-TIME THRU 9010-EXIT.                     GC024000
00170      DISPLAY 'GC024000 FINISHED AT ' D-TIME.                      GC024000
00171                                                                   GC024000
00172      GOBACK.                                                      GC024000
00173                                                                   GC024000
00174  0000-EXIT. EXIT.                                                 GC024000
00175                                                                   GC024000
00176 /                                                                 GC024000
00177  1000-PROCESS-CONTRACT.                                           GC024000
00178      OPEN INPUT RLSE-CONTRACT-FILE.                               GC024000
00179                                                                   GC024000
00180      PERFORM 1050-READ-CON-REC THRU 1050-EXIT.                    GC024000
00181      PERFORM 1100-PROCESS THRU 1100-EXIT                          GC024000
00182          UNTIL CON-EOF.                                           GC024000
00183 *** CLOSE VSAM FILES IN CONTRACT EDIT MODULE.                     GC024000
00184      MOVE 'C' TO GC024010-IND.                                    GC024000
00185      CALL 'GC024010' USING GC024010-IND.                          GC024000
00186      CLOSE RLSE-CONTRACT-FILE.                                    GC024000
00187  1000-EXIT. EXIT.                                                 GC024000
00188                                                                   GC024000
00189 /                                                                 GC024000
00190  1050-READ-CON-REC.                                               GC024000
00191      READ RLSE-CONTRACT-FILE                                      GC024000
00192          AT END                                                   GC024000
00193              MOVE 'YES' TO CON-EOF-SW                             GC024000
00194              GO TO 1050-EXIT.                                     GC024000
00195      ADD 1 TO CONTRACT-READ-COUNT.                                GC024000
00196  1050-EXIT. EXIT.                                                 GC024000
00197                                                                   GC024000
00198 /                                                                 GC024000
00199  1100-PROCESS.                                                    GC024000
00200      MOVE SPACE TO GC024010-IND.                                  GC024000
00201      CALL 'GC024010' USING GC024010-IND  GCT-CONTRACT-ID          GC024000
00202                            WRK-OPERATOR-ID.                       GC024000
00203      PERFORM 1050-READ-CON-REC THRU 1050-EXIT.                    GC024000
00204  1100-EXIT. EXIT.                                                 GC024000
00205                                                                   GC024000
00206 /                                                                 GC024000
00207  2000-PROCESS-GRPSPC.                                             GC024000
00208                                                                   GC024000
00209      OPEN INPUT RLSE-GRP-SPEC-FILE.                               GC024000
00210                                                                   GC024000
00211      PERFORM 2050-READ-GRP-REC THRU 2050-EXIT.                    GC024000
00212                                                                   GC024000
00213      PERFORM 2100-PROCESS THRU 2100-EXIT                          GC024000
00214          UNTIL GRP-EOF.                                           GC024000
00215                                                                   GC024000
00216 *** CLOSE VSAM FILES IN GRPSPC EDIT MODULE.                       GC024000
00217      MOVE 'C' TO GC024020-IND.                                    GC024000
00218      CALL 'GC024020' USING GC024020-IND.                          GC024000
00219                                                                   GC024000
00220      CLOSE RLSE-GRP-SPEC-FILE.                                    GC024000
00221                                                                   GC024000
00222  2000-EXIT. EXIT.                                                 GC024000
00223                                                                   GC024000
00224                                                                   GC024000
00225 /                                                                 GC024000
00226  2050-READ-GRP-REC.                                               GC024000
00227                                                                   GC024000
00228      READ RLSE-GRP-SPEC-FILE                                      GC024000
00229          AT END                                                   GC024000
00230              MOVE 'YES' TO GRP-EOF-SW                             GC024000
00231              GO TO 2050-EXIT.                                     GC024000
00232      ADD 1 TO GRPSPC-READ-COUNT.                                  GC024000
00233                                                                   GC024000
00234  2050-EXIT. EXIT.                                                 GC024000
00235                                                                   GC024000
00236 /                                                                 GC024000
00237  2100-PROCESS.                                                    GC024000
00238      MOVE 'R' TO GC024020-IND.                                    GC024000
00239      CALL 'GC024020' USING GC024020-IND  GCG-GRP-SPECIF-ID        GC024000
00240                            WRK2-OPERATOR-ID.                      GC024000
00241      PERFORM 2050-READ-GRP-REC THRU 2050-EXIT.                    GC024000
00242  2100-EXIT. EXIT.                                                 GC024000
00243 /                                                                 GC024000
00244  3000-PROCESS-BEN-PROV.                                           GC024000
00245                                                                   GC024000
00246      OPEN INPUT RLSE-BEN-PROV-FILE.                               GC024000
00247                                                                   GC024000
00248      PERFORM 3050-READ-BEN-PROV THRU 3050-EXIT.                   GC024000
00249                                                                   GC024000
00250      PERFORM 3100-PROCESS THRU 3100-EXIT                          GC024000
00251          UNTIL BEN-PROV-EOF.                                      GC024000
00252                                                                   GC024000
00253 *** CLOSE VSAM FILES IN BENPROV EDIT MODULE.                      GC024000
00254      MOVE 'C' TO GC024025-IND.                                    GC024000
00255      CALL 'GC024025' USING GC024025-IND.                          GC024000
00256                                                                   GC024000
00257      CLOSE RLSE-BEN-PROV-FILE.                                    GC024000
00258                                                                   GC024000
00259  3000-EXIT. EXIT.                                                 GC024000
00260                                                                   GC024000
00261                                                                   GC024000
00262 /                                                                 GC024000
00263  3050-READ-BEN-PROV.                                              GC024000
00264                                                                   GC024000
00265      READ RLSE-BEN-PROV-FILE                                      GC024000
00266          AT END                                                   GC024000
00267              MOVE 'YES' TO BEN-PROV-EOF-SW                        GC024000
00268              GO TO 3050-EXIT.                                     GC024000
00269      ADD 1 TO BEN-PROV-READ-COUNT.                                GC024000
00270                                                                   GC024000
00271  3050-EXIT. EXIT.                                                 GC024000
00272                                                                   GC024000
00273 /                                                                 GC024000
00274  3100-PROCESS.                                                    GC024000
00275      MOVE 'R' TO GC024025-IND.                                    GC024000
00276      MOVE WRK3-PLAN-CODE TO L3-PLAN-CODE.                         GC024000
00277      MOVE WRK3-GROUP-NUM TO L3-GRP-NBR.                           GC024000
00278      MOVE WRK3-SECTION-NUM  TO L3-SECTN-NO.                       GC024000
00279      MOVE WRK3-PKG-CODE  TO L3-PKG-CODE.                          GC024000
00280      MOVE WRK3-L-O-B    TO L3-LOB.                                GC024000
00281      MOVE WRK3-PROV-CTL TO L3-PRV-CTL.                            GC024000
00282      MOVE WRK3-FAM-REL-LEVEL TO L3-FAM-REL.                       GC024000
00283      MOVE WRK3-EFFDT-CEN TO L3-EFFDT-CEN.                         GC024000
00284      MOVE GCP-PROVN-ID TO L3-BEN-PROV-ID.                         GC024000
00285      MOVE GCP-PROVN-SLOT-NO TO L3-BEN-PROV-SLOT.                  GC024000
00286      CALL 'GC024025' USING LINK-AREA-3 WRK3-OPERATOR-ID.          GC024000
00287      PERFORM 3050-READ-BEN-PROV THRU 3050-EXIT.                   GC024000
00288  3100-EXIT. EXIT.                                                 GC024000
00289                                                                   GC024000
00290 /                                                                 GC024000
00291  9010-ACCEPT-DATE-TIME.                                           GC024000
00292      ACCEPT H-DATE FROM DATE.                                     GC024000
00293      MOVE CORR H-DATE TO D-DATE.                                  GC024000
00294  9010-ACCEPT-TIME.                                                GC024000
00295      ACCEPT H-TIME FROM TIME.                                     GC024000
00296      MOVE CORR H-TIME TO D-TIME.                                  GC024000
00297  9010-EXIT. EXIT.                                                 GC024000
00298                                                                   GC024000
00299 /                                                                 GC024000
00300  9999-ERROR-RTN.                                                  GC024000
00301                                                                   GC024000
00302      CALL 'TSGEND' USING ABEND-CODE.                              GC024000
00303                                                                   GC024000
00304  9999-EXIT. EXIT.                                                 GC024000
