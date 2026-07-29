00001 *      LAST MAINTENANCE TIME: 15.55.24  DATE: 07/06/84            12/09/02
00002  IDENTIFICATION DIVISION.                                         GC028010
00003  PROGRAM-ID. GC028010.                                               LV002
00004  AUTHOR. ROBERT MANN - DECISION CONSULANTS INC.                   GC028010
00005  INSTALLATION. HCSC.                                              GC028010
00006  DATE-WRITTEN.  JUL 06,1984                                       GC028010
00007  DATE-COMPILED.                                                   GC028010
00008 ******************************************************************GC028010
00009 *    THIS PROGRAM IS A SUB MODULE TO PROGRAM GC0280               GC028010
00010 *    IT UPDATES THE SLOT NUMBERS ON THE RELEASED                  GC028010
00011 *    CONTRACT RECORD FILE FOR CONTRACT TABULAR RECORDS.           GC028010
00012 ******************************************************************GC028010
00013 *                                                                *GC028010
00014 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC028010
00015 *       *-*         U P D A T E   H I S T O R Y         *-*      *GC028010
00016 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC028010
00017 *                                                                *GC028010
00018 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*GC028010
00019 *                                                                *GC028010
00020 *   11154     3/06/91  FRY   INCREASE RECORD AREA IN FILE        *GC028010
00021 *                            SECTION:                            *GC028010
00022 *                     TAB-REC-DATA.                              *GC028010
00023 *                        FILLER   PIC X(3990)  CHANGED TO  7795. *GC028010
00024 *                                                                *GC028010
00025 *                                                                *GC028010
00026 *D12009 09/18/91  TPM   INCREASED THE CRCT-MATCH FIELD WITHIN    *GC028010
00027 *                       THE CRCT-REC-KEY TO ACCOMODATE FOR THE   *GC028010
00028 *                       INCREASE IN THE FAMILY RELATION FIELD    *GC028010
00029 *                       AND DECREASED THE FILLER AREA BY 1.      *GC028010
00030 *                                                                *GC028010
00031 * 14726/15057                                                    *GC028010
00032 *         09/15/97 DAU  ADDED CODE TO SUPPORT THE YEAR 2000 AND  *GC028010
00033 *                       THE EXPANSION OF THE GROUP SPECIFIC AND  *GC028010
00034 *                       CONTRACT KEY TO SUPPORT THE TEXAS MERGER.*GC028010
00035 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GC028010
00036 *                                                                *GC028010
00036 * DM9441  09/17/09 JH   CRS TABULAR EXPANSION                    *GC028010
00036 *                                                                *GC028010
00037 ******************************************************************GC028010
00038  ENVIRONMENT DIVISION.                                            GC028010
00039  CONFIGURATION SECTION.                                           GC028010
00040  SOURCE-COMPUTER. IBM-370.                                        GC028010
00041  OBJECT-COMPUTER. IBM-370.                                        GC028010
00042  INPUT-OUTPUT SECTION.                                            GC028010
00043  FILE-CONTROL.                                                    GC028010
00044      SELECT COMP-RLSE-CON-TAB-FILE                                GC028010
00045                              ASSIGN TO UT-S-GC0280B.              GC028010
00046      EJECT                                                        GC028010
00047  DATA DIVISION.                                                   GC028010
00048  FILE SECTION.                                                    GC028010
00049                                                                   GC028010
00050  FD  COMP-RLSE-CON-TAB-FILE                                       GC028010
00051      LABEL RECORDS ARE STANDARD                                   GC028010
00052      RECORDING MODE IS V                                          GC028010
00053      BLOCK CONTAINS 0 RECORDS.                                    GC028010
00054  01  COMP-RLSE-CON-TAB-REC.                                       GC028010
00055      05  CRCT-REC-KEY.                                            GC028010
00056          10  CRCT-MATCH-FLDS.                                     GC028010
00057              15  CRCT-MATCH              PIC X(30).               GC028010
00058              15  CRCT-REC-TYPE           PIC X(2).                GC028010
00059              15  CRCT-BEN-ID-SLOT.                                GC028010
00060                  20  CRCT-BEN-ID         PIC X(6).                GC028010
00061                  20  CRCT-BEN-SLOT       PIC S9(7)   COMP-3.      GC028010
00062              15  CRCT-TAB-ID-SLOT.                                GC028010
00063                  20  CRCT-TAB-ID         PIC X(6).                GC028010
00064                  20  CRCT-TAB-SLOT       PIC S9(7) COMP-3.        GC028010
00065              15  FILLER                  PIC X(5).                GC028010
00066          10  FILLER                      PIC X(43).               GC028010
00067      05  TAB-REC-DATA.                                            GC028010
00068          10  TAB-ID-SLOT.                                         GC028010
00069              15  TAB-ID      PIC X(6).                            GC028010
00070              15  TAB-SLOT-NO PIC S9(7)       COMP-3.              GC028010
00071          10  FILLER          PIC X(31360).                        GC028010
00072      EJECT                                                        GC028010
00073  WORKING-STORAGE SECTION.                                         GC028010
00074                                                                   GC028010
00075  01  FILLER          PIC X(24)   VALUE                            GC028010
00076                      'GC028010 WORKING STORAGE'.                  GC028010
00077                                                                   GC028010
00078  01  SWITCH-AREA.                                                 GC028010
00079      05  CRCT-SW     PIC X   VALUE SPACES.                        GC028010
00080          88  EOF-CRCT        VALUE HIGH-VALUES.                   GC028010
00081                                                                   GC028010
00082  01  ABEND-CODE      PIC 9(4)    COMP.                            GC028010
00083                                                                   GC028010
00084  01  TAB-ID-TEST.                                                 GC028010
00085      05  TAB-POS-1   PIC X       VALUE SPACES.                    GC028010
00086          88  TAB-ID-VALID        VALUE '#'.                       GC028010
00087      05  FILLER      PIC X(5)    VALUE SPACES.                    GC028010
00088      EJECT                                                        GC028010
00089  LINKAGE SECTION.                                                 GC028010
00090                                                                   GC028010
00091  01  GC028010-IND        PIC X.                                   GC028010
00092                                                                   GC028010
00093  01  RLSE-CON-REC.                                                GC028010
00094      COPY GCWRKDCC.                                               GC028010
00095      COPY GCCONTRC.                                               GC028010
00096      EJECT                                                        GC028010
00097  PROCEDURE DIVISION USING GC028010-IND RLSE-CON-REC.              GC028010
00098                                                                   GC028010
00099  0000-MAINLINE.                                                   GC028010
00100                                                                   GC028010
00101      IF  GC028010-IND EQUAL 'O'                                   GC028010
00102          MOVE SPACES             TO GC028010-IND                  GC028010
00103          OPEN INPUT COMP-RLSE-CON-TAB-FILE                        GC028010
00104          PERFORM 0010-READ-CRCT-REC THRU 0010-EXIT.               GC028010
00105                                                                   GC028010
00106      PERFORM 0020-MATCH THRU 0020-EXIT                            GC028010
00107          UNTIL EOF-CRCT OR                                        GC028010
00108          (CRCT-MATCH GREATER THAN WRK-MATCH-CONT).                GC028010
00109                                                                   GC028010
00110      PERFORM 0030-VALID-TAB-ID THRU 0030-EXIT                     GC028010
00111          VARYING GCT-TAB-INDEX FROM 1 BY 1 UNTIL                  GC028010
00112          GCT-TAB-INDEX GREATER THAN GCT-COUNT-TAB-PROVN-POINTERS. GC028010
00113                                                                   GC028010
00114      IF  EOF-CRCT                                                 GC028010
00115          MOVE HIGH-VALUES        TO GC028010-IND                  GC028010
00116          CLOSE COMP-RLSE-CON-TAB-FILE.                            GC028010
00117                                                                   GC028010
00118      GOBACK.                                                      GC028010
00119      EJECT                                                        GC028010
00120  0010-READ-CRCT-REC.                                              GC028010
00121                                                                   GC028010
00122      READ COMP-RLSE-CON-TAB-FILE                                  GC028010
00123          AT END                                                   GC028010
00124              MOVE HIGH-VALUES    TO CRCT-SW.                      GC028010
00125                                                                   GC028010
00126  0010-EXIT.                                                       GC028010
00127      EXIT.                                                        GC028010
00128      EJECT                                                        GC028010
00129  0020-MATCH.                                                      GC028010
00130                                                                   GC028010
00131      IF  WRK-MATCH-CONT GREATER THAN CRCT-MATCH                   GC028010
00132          MOVE 1120           TO ABEND-CODE                        GC028010
00133          DISPLAY '*** ABEND OCCURRED ***'                         GC028010
00134          DISPLAY 'ABEND CODE IS' ABEND-CODE                       GC028010
00135          PERFORM 1000-DISP-FIELDS THRU 1000-EXIT                  GC028010
00136          GO TO 9999-ERROR-RTN.                                    GC028010
00137                                                                   GC028010
00138      SET GCT-TAB-INDEX TO 1.                                      GC028010
00139      SEARCH GCT-CON-TAB-ID-SLOT                                   GC028010
00140          AT END                                                   GC028010
00141              MOVE 1125       TO ABEND-CODE                        GC028010
00142              DISPLAY '*** ABEND OCCURRED ***'                     GC028010
00143              DISPLAY 'ABEND CODE IS' ABEND-CODE                   GC028010
00144              DISPLAY '*** TABULAR NOT FOUND ON CONTRACT ***'      GC028010
00145              DISPLAY 'TABULAR ID    =' TAB-ID                     GC028010
00146              PERFORM 1000-DISP-FIELDS THRU 1000-EXIT              GC028010
00147              GO TO 9999-ERROR-RTN                                 GC028010
00148          WHEN                                                     GC028010
00149              GCT-CON-TAB-ID (GCT-TAB-INDEX) EQUAL TAB-ID          GC028010
00150                  MOVE TAB-SLOT-NO                                 GC028010
00151                              TO GCT-CON-TAB-SLOT (GCT-TAB-INDEX). GC028010
00152                                                                   GC028010
00153      PERFORM 0010-READ-CRCT-REC THRU 0010-EXIT.                   GC028010
00154                                                                   GC028010
00155  0020-EXIT.                                                       GC028010
00156      EXIT.                                                        GC028010
00157      EJECT                                                        GC028010
00158  0030-VALID-TAB-ID.                                               GC028010
00159                                                                   GC028010
00160      MOVE GCT-CON-TAB-ID (GCT-TAB-INDEX)                          GC028010
00161                                  TO TAB-ID-TEST.                  GC028010
00162                                                                   GC028010
00163      IF  TAB-ID-VALID AND                                         GC028010
00164          GCT-CON-TAB-SLOT (GCT-TAB-INDEX) GREATER THAN 8999999    GC028010
00165              MOVE 1130           TO ABEND-CODE                    GC028010
00166              DISPLAY '*** ABEND OCCURRED ***'                     GC028010
00167              DISPLAY 'ABEND CODE IS' ABEND-CODE                   GC028010
00168              DISPLAY '*** TABULAR WORK RECORD NOT FOUND ***'      GC028010
00169              DISPLAY 'TABULAR ID =' GCT-CON-TAB-ID (GCT-TAB-INDEX)GC028010
00170              PERFORM 1000-DISP-FIELDS THRU 1000-EXIT              GC028010
00171              GO TO 9999-ERROR-RTN.                                GC028010
00172                                                                   GC028010
00173  0030-EXIT.                                                       GC028010
00174      EXIT.                                                        GC028010
00175      EJECT                                                        GC028010
00176  1000-DISP-FIELDS.                                                GC028010
00177      DISPLAY 'PLAN CODE            =' GCT-PLAN-CODE.              GC028010
00178      DISPLAY 'GROUP NUMBER         =' GCT-GROUP-NUM.              GC028010
00179      DISPLAY 'SECTION NUMBER       =' GCT-SECTION-NUM.            GC028010
00180      DISPLAY 'PACKAGE CODE         =' GCT-PKG-CODE.               GC028010
00181      DISPLAY 'LINE OF BUISNESS     =' GCT-L-O-B.                  GC028010
00182      DISPLAY 'PROVIDER CONTROL     =' GCT-PROVDR-CONTROL.         GC028010
00183      DISPLAY 'FAMILY RELATION LEVEL=' GCT-FAM-REL-LVL.            GC028010
00184      DISPLAY 'EFFECTIVE DATE       =' GCT-EFFDT-CEN.              GC028010
00185      DISPLAY '*** YOU ARE IN PROGRAM GC028010TS ***'.             GC028010
00186  1000-EXIT.                                                       GC028010
00187      EXIT.                                                        GC028010
00188  9999-ERROR-RTN.                                                  GC028010
00189                                                                   GC028010
00190      CALL 'TSGEND' USING ABEND-CODE.                              GC028010
00191                                                                   GC028010
00192  9999-EXIT.                                                       GC028010
00193      EXIT.                                                        GC028010
