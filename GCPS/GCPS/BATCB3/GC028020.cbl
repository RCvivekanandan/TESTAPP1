00001 *      LAST MAINTENANCE TIME: 15.06.55  DATE: 07/16/84            12/09/02
00002  IDENTIFICATION DIVISION.                                         GC028020
00003  PROGRAM-ID. GC028020.                                               LV002
00004  AUTHOR. ROBERT MANN - DECISION CONSULANTS INC.                   GC028020
00005  INSTALLATION. HCSC.                                              GC028020
00006  DATE-WRITTEN.  JUL 09,1984                                       GC028020
00007  DATE-COMPILED.                                                   GC028020
00008 ***************************************************************** GC028020
00009 *    THIS PROGRAM IS A SUB MODULE TO PROGRAM GC0280               GC028020
00010 *    IT UPDATES THE SLOT NUMBERS ON THE RELEASED                  GC028020
00011 *    CONTRACT RECORD FILE FOR BENEFIT PROVISION RECORDS.          GC028020
00012 ******************************************************************GC028020
00013 *                                                                *GC028020
00014 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         *GC028020
00015 *    *-*         U P D A T E   H I S T O R Y         *-*         *GC028020
00016 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         *GC028020
00017 *                                                                *GC028020
00018 * CHG #    DATE    BY              DESCRIPTION                   *GC028020
00019 * _____  ________  ___  ___________________________________      *GC028020
00020 *                                                                *GC028020
00021 * XXXX   03/09/91  ENW  COMMENTED OUT READY TRACE.               *GC028020
00022 *                                                                *GC028020
00023 *D12009 09/18/91  TPM   ADJUSTED THE CRBP-REC-KEY TO ACCOMODATE  *GC028020
00024 *                       FOR THE EXPANSION IN THE FAMILY RELATION *GC028020
00025 *                                                                *GC028020
00026 * 14726/15057                                                    *GC028020
00027 *         09/15/97 DAU  ADDED CODE TO SUPPORT THE YEAR 2000 AND  *GC028020
00028 *                       THE EXPANSION OF THE GROUP SPECIFIC AND  *GC028020
00029 *                       CONTRACT KEY TO SUPPORT THE TEXAS MERGER.*GC028020
00030 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GC028020
00031 *                                                                *GC028020
00032 ******************************************************************GC028020
00033  ENVIRONMENT DIVISION.                                            GC028020
00034  CONFIGURATION SECTION.                                           GC028020
00035  SOURCE-COMPUTER. IBM-370.                                        GC028020
00036  OBJECT-COMPUTER. IBM-370.                                        GC028020
00037  INPUT-OUTPUT SECTION.                                            GC028020
00038  FILE-CONTROL.                                                    GC028020
00039      SELECT COMP-RLSE-BEN-PROV-FILE                               GC028020
00040                              ASSIGN TO UT-S-GC0280C.              GC028020
00041      EJECT                                                        GC028020
00042  DATA DIVISION.                                                   GC028020
00043  FILE SECTION.                                                    GC028020
00044                                                                   GC028020
00045  FD  COMP-RLSE-BEN-PROV-FILE                                      GC028020
00046      LABEL RECORDS ARE STANDARD                                   GC028020
00047      RECORDING MODE IS V                                          GC028020
00048      BLOCK CONTAINS 0 RECORDS.                                    GC028020
00049  01  COMP-RLSE-BEN-PROV-REC.                                      GC028020
00050      05  CRBP-REC-KEY.                                            GC028020
00051          10  CRBP-MATCH-FLDS.                                     GC028020
00052              15  CRBP-MATCH              PIC X(30).               GC028020
00053              15  CRBP-REC-TYPE           PIC X(2).                GC028020
00054              15  CRBP-BEN-ID-SLOT.                                GC028020
00055                  20  CRBP-BEN-ID         PIC X(6).                GC028020
00056                  20  CRBP-BEN-SLOT       PIC S9(7)   COMP-3.      GC028020
00057              15  CRBP-TAB-ID-SLOT.                                GC028020
00058                  20  CRBP-TAB-ID         PIC X(6).                GC028020
00059                  20  CRBP-TAB-SLOT       PIC S9(7) COMP-3.        GC028020
00060              15  FILLER                  PIC X(5).                GC028020
00061          10  FILLER                      PIC X(43).               GC028020
00062      05  BEN-PROV-REC-DATA.                                       GC028020
00063          10  BEN-PROV-ID-SLOT.                                    GC028020
00064              15  BEN-PROV-ID         PIC X(6).                    GC028020
00065              15  BEN-PROV-SLOT-NO    PIC S9(7)       COMP-3.      GC028020
00066          10  FILLER                  PIC X(385).                  GC028020
00067      EJECT                                                        GC028020
00068  WORKING-STORAGE SECTION.                                         GC028020
00069                                                                   GC028020
00070  01  FILLER          PIC X(24)   VALUE                            GC028020
00071                      'GC028020 WORKING STORAGE'.                  GC028020
00072                                                                   GC028020
00073  01  SWITCH-AREA.                                                 GC028020
00074      05  CRBP-SW     PIC X   VALUE SPACES.                        GC028020
00075          88  EOF-CRBP        VALUE HIGH-VALUES.                   GC028020
00076                                                                   GC028020
00077  01  ABEND-CODE      PIC 9(4)    COMP.                            GC028020
00078                                                                   GC028020
00079      EJECT                                                        GC028020
00080  LINKAGE SECTION.                                                 GC028020
00081                                                                   GC028020
00082  01  GC028020-IND        PIC X.                                   GC028020
00083                                                                   GC028020
00084  01  RLSE-CON-REC.                                                GC028020
00085      COPY GCWRKDCC.                                               GC028020
00086      COPY GCCONTRC.                                               GC028020
00087      EJECT                                                        GC028020
00088  PROCEDURE DIVISION USING GC028020-IND RLSE-CON-REC.              GC028020
00089                                                                   GC028020
00090  0000-MAINLINE.                                                   GC028020
00091 *    READY TRACE.                                                 GC028020
00092      IF  GC028020-IND EQUAL 'O'                                   GC028020
00093          MOVE SPACES             TO GC028020-IND                  GC028020
00094          OPEN INPUT COMP-RLSE-BEN-PROV-FILE                       GC028020
00095          PERFORM 0010-READ-CRBP-REC THRU 0010-EXIT.               GC028020
00096                                                                   GC028020
00097      PERFORM 0020-MATCH THRU 0020-EXIT                            GC028020
00098          UNTIL EOF-CRBP OR                                        GC028020
00099          (CRBP-MATCH GREATER THAN WRK-MATCH-CONT).                GC028020
00100                                                                   GC028020
00101      PERFORM 0030-VALID-BEN-PROV THRU 0030-EXIT                   GC028020
00102          VARYING GCT-INDEX FROM 1 BY 1 UNTIL                      GC028020
00103          GCT-INDEX GREATER THAN GCT-COUNT-BEN-PROVN-POINTERS.     GC028020
00104                                                                   GC028020
00105      IF  EOF-CRBP                                                 GC028020
00106          MOVE HIGH-VALUES        TO GC028020-IND                  GC028020
00107          CLOSE COMP-RLSE-BEN-PROV-FILE.                           GC028020
00108                                                                   GC028020
00109      GOBACK.                                                      GC028020
00110      EJECT                                                        GC028020
00111  0010-READ-CRBP-REC.                                              GC028020
00112                                                                   GC028020
00113      READ COMP-RLSE-BEN-PROV-FILE                                 GC028020
00114          AT END                                                   GC028020
00115              MOVE HIGH-VALUES    TO CRBP-SW.                      GC028020
00116                                                                   GC028020
00117                                                                   GC028020
00118  0010-EXIT.                                                       GC028020
00119      EXIT.                                                        GC028020
00120      EJECT                                                        GC028020
00121  0020-MATCH.                                                      GC028020
00122                                                                   GC028020
00123      IF  WRK-MATCH-CONT GREATER THAN CRBP-MATCH                   GC028020
00124          MOVE 1220           TO ABEND-CODE                        GC028020
00125          DISPLAY '*** ABEND OCCURRED ***'                         GC028020
00126          DISPLAY 'ABEND CODE IS' ABEND-CODE                       GC028020
00127          PERFORM 1000-DISP-FIELDS THRU 1000-EXIT                  GC028020
00128          GO TO 9999-ERROR-RTN.                                    GC028020
00129                                                                   GC028020
00130      SET GCT-INDEX TO 1.                                          GC028020
00131      SEARCH GCT-BEN-PROVN                                         GC028020
00132          AT END                                                   GC028020
00133              MOVE 1225       TO ABEND-CODE                        GC028020
00134              DISPLAY '*** ABEND OCCURRED ***'                     GC028020
00135              DISPLAY 'ABEND CODE IS' ABEND-CODE                   GC028020
00136              DISPLAY '*** BENEFIT PROVISION SEARCH FAILED ***'    GC028020
00137              DISPLAY 'BENEFIT PROVISION ID   =' BEN-PROV-ID       GC028020
00138              PERFORM 1000-DISP-FIELDS THRU 1000-EXIT              GC028020
00139              GO TO 9999-ERROR-RTN                                 GC028020
00140          WHEN                                                     GC028020
00141              GCT-BEN-PROVN-ID (GCT-INDEX) EQUAL BEN-PROV-ID       GC028020
00142                  MOVE BEN-PROV-SLOT-NO                            GC028020
00143                              TO GCT-BEN-PROVN-SLOT-NO (GCT-INDEX).GC028020
00144                                                                   GC028020
00145      PERFORM 0010-READ-CRBP-REC THRU 0010-EXIT.                   GC028020
00146                                                                   GC028020
00147  0020-EXIT.                                                       GC028020
00148      EXIT.                                                        GC028020
00149      EJECT                                                        GC028020
00150  0030-VALID-BEN-PROV.                                             GC028020
00151                                                                   GC028020
00152      IF  GCT-BEN-PROVN-ID (GCT-INDEX) GREATER THAN SPACES AND     GC028020
00153          GCT-BEN-PROVN-SLOT-NO (GCT-INDEX) GREATER THAN 8999999   GC028020
00154              MOVE 1230           TO ABEND-CODE                    GC028020
00155              DISPLAY '*** ABEND OCCURRED ***'                     GC028020
00156              DISPLAY 'ABEND CODE IS' ABEND-CODE                   GC028020
00157              DISPLAY 'BENEFIT PROVISION WORK RECORD NOT FOUND'    GC028020
00158              DISPLAY 'PROVISION ID =' GCT-BEN-PROVN-ID (GCT-INDEX)GC028020
00159              PERFORM 1000-DISP-FIELDS THRU 1000-EXIT              GC028020
00160              GO TO 9999-ERROR-RTN.                                GC028020
00161                                                                   GC028020
00162  0030-EXIT.                                                       GC028020
00163      EXIT.                                                        GC028020
00164      EJECT                                                        GC028020
00165  1000-DISP-FIELDS.                                                GC028020
00166      DISPLAY 'PLAN CODE            =' GCT-PLAN-CODE.              GC028020
00167      DISPLAY 'GROUP NUMBER         =' GCT-GROUP-NUM.              GC028020
00168      DISPLAY 'SECTION NUMBER       =' GCT-SECTION-NUM.            GC028020
00169      DISPLAY 'PACKAGE CODE         =' GCT-PKG-CODE.               GC028020
00170      DISPLAY 'LINE OF BUISNESS     =' GCT-L-O-B.                  GC028020
00171      DISPLAY 'PROVIDER CONTROL     =' GCT-PROVDR-CONTROL.         GC028020
00172      DISPLAY 'FAMILY RELATION LEVEL=' GCT-FAM-REL-LVL.            GC028020
00173      DISPLAY 'EFFECTIVE DATE       =' GCT-EFFDT-CEN.              GC028020
00174      DISPLAY '*** YOU ARE IN PROGRAM GC028020TS ***'.             GC028020
00175  1000-EXIT.                                                       GC028020
00176      EXIT.                                                        GC028020
00177  9999-ERROR-RTN.                                                  GC028020
00178                                                                   GC028020
00179      CALL 'TSGEND' USING ABEND-CODE.                              GC028020
00180                                                                   GC028020
00181  0060-EXIT.                                                       GC028020
00182      EXIT.                                                        GC028020
