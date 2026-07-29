00001 *      LAST MAINTENANCE TIME: 12.07.26  DATE: 10/03/84            12/09/02
00002  IDENTIFICATION DIVISION.                                         GC028080
00003  PROGRAM-ID.  GC028080.                                              LV002
00004  AUTHOR. ROBERT MANN - DECISION CONSULTANTS INC.                  GC028080
00005  INSTALLATION.  HCSC.                                             GC028080
00006  DATE-WRITTEN.  JUL 11,1984.                                      GC028080
00007  DATE-COMPILED.                                                   GC028080
00008 ***************************************************************   GC028080
00009 *    THIS PROGRAM UPDATES THE ONLINE CONTRACT FILE AND CREATES    GC028080
00010 *    A COMPLETED RELEASE SLOT UPDATED CONTRACT FILE.              GC028080
00011 *                                                                 GC028080
00012 *    THE ENTIRE CONTRACT RECORD IS REPLACED ON THE ONLINE FILE.   GC028080
00013 *                                                                 GC028080
00014 *    THIS PROGRAM MODULE CALLED BY PROGRAM GC0280                 GC028080
00015 *    AND IS NOT TO BE EXECUTED AS A STAND ALONE PROGRAM.          GC028080
00016 *                                                                 GC028080
00017 *    TSGVSAM1 IS THE ONLINE CONTRACT FILE - (OUTPUT).             GC028080
00018 *    COMPLETED RELEASE CONTRACT FILE- CRSUC- (OUTPUT).            GC028080
00019 *                                                                 GC028080
00020 * 01/25/95   JGR  CONTRACT RECORD EXPANSION.                      GC028080
00021 *                                                                 GC028080
00022 * 14726/15057                                                     GC028080
00023 *         09/15/97 DAU  ADDED CODE TO SUPPORT THE YEAR 2000 AND   GC028080
00024 *                       THE EXPANSION OF THE GROUP SPECIFIC AND   GC028080
00025 *                       CONTRACT KEY TO SUPPORT THE TEXAS MERGER. GC028080
00026 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GC028080
00027 *                                                                *GC028080
00028 ******************************************************************GC028080
00029  ENVIRONMENT DIVISION.                                            GC028080
00030  CONFIGURATION SECTION.                                           GC028080
00031  SOURCE-COMPUTER. IBM-370.                                        GC028080
00032  OBJECT-COMPUTER. IBM-370.                                        GC028080
00033  INPUT-OUTPUT SECTION.                                            GC028080
00034  FILE-CONTROL.                                                    GC028080
00035      SELECT CRSUC            ASSIGN TO UT-S-GC0280D.              GC028080
00036      EJECT                                                        GC028080
00037  DATA DIVISION.                                                   GC028080
00038  FILE SECTION.                                                    GC028080
00039                                                                   GC028080
00040  FD  CRSUC                                                        GC028080
00041      LABEL RECORDS ARE STANDARD                                   GC028080
00042      RECORDING MODE IS V                                          GC028080
00043      BLOCK CONTAINS 0 RECORDS.                                    GC028080
00044  01  CRSUC-REC.                                                   GC028080
00045      COPY GCWRKDC2.                                               GC028080
00046      COPY GCCONTR2.                                               GC028080
00047      EJECT                                                        GC028080
00048  WORKING-STORAGE SECTION.                                         GC028080
00049                                                                   GC028080
00050  01  FILLER            PIC X(24)                                  GC028080
00051               VALUE 'GC028080 WORKING STORAGE'.                   GC028080
00052                                                                   GC028080
00053  01  ABEND-CODE        PIC 9(4)  COMP.                            GC028080
00054                                                                   GC028080
00055  01  DATE-WORK-AREA.                                              GC028080
00056      05  JUL-DATE            PIC 9(7).                            GC028080
00057                                                                   GC028080
00058      COPY MLDATE01.                                               GC028080
00059      EJECT                                                        GC028080
00060  01  PARM-SET.                                                    GC028080
00061      05  SET-RDW.                                                 GC028080
00062          10  SET-REC-LENG    PIC 9(4)    VALUE ZEROS     COMP.    GC028080
00063          10  SET-FEEDBACK    PIC 9(4)    VALUE ZEROS     COMP.    GC028080
00064      05  SET-VALUE           PIC 9(8)                    COMP.    GC028080
00065                                                                   GC028080
00066  01  PARM-ONE.                                                    GC028080
00067      05  RESERVED-FLDS-1     PIC 9(8)    VALUE ZEROS     COMP.    GC028080
00068      05  RESERVED-X-1 REDEFINES RESERVED-FLDS-1.                  GC028080
00069          10  REQUEST-TYPE-1  PIC X.                               GC028080
00070          10  FILLER          PIC X(3).                            GC028080
00071                                                                   GC028080
00072  01  PARM-ONEA.                                                   GC028080
00073      05  ONEA-RDW.                                                GC028080
00074          10  ONEA-REC-LENG   PIC 9(4)    VALUE ZEROS     COMP.    GC028080
00075          10  ONEA-FEEDBACK   PIC 9(4)    VALUE ZEROS     COMP.    GC028080
00076      05  ONEA-REC-AREA       PIC X(8113).                         GC028080
00077      05  ONEA-REC-X REDEFINES ONEA-REC-AREA.                      GC028080
00078          10  ONEA-KEY        PIC X(29).                           GC028080
00079          10  ONEA-DATA.                                           GC028080
00080              15  FILLER      PIC X(69).                           GC028080
00081              15  GCT-DT-LAST-CHG.                                 GC028080
00082                  20  GCT-DT-LAST-CHG-CC     PIC X.                GC028080
00083                  20  GCT-DT-OF-LAST-CHG     PIC S9(5)   COMP-3.   GC028080
00084              15  GCT-DT-LAST-CHG-CEN REDEFINES                    GC028080
00085                     GCT-DT-LAST-CHG         PIC S9(7)   COMP-3.   GC028080
00086              15  FILLER      PIC X(8011).                         GC028080
00087      EJECT                                                        GC028080
00088  LINKAGE SECTION.                                                 GC028080
00089                                                                   GC028080
00090  01  GC028080-IND        PIC X.                                   GC028080
00091                                                                   GC028080
00092  01  RLSE-CON-REC.                                                GC028080
00093      COPY GCWRKDCC.                                               GC028080
00094      COPY GCCONTRC.                                               GC028080
00095      EJECT                                                        GC028080
00096  PROCEDURE DIVISION USING GC028080-IND RLSE-CON-REC.              GC028080
00097                                                                   GC028080
00098  0000-MAINLINE.                                                   GC028080
00099                                                                   GC028080
00100      IF  GC028080-IND EQUAL 'O'                                   GC028080
00101          MOVE SPACES             TO GC028080-IND                  GC028080
00102          PERFORM 0020-OPEN-FILES THRU 0020-EXIT                   GC028080
00103          GO TO 0000-EXIT.                                         GC028080
00104                                                                   GC028080
00105      IF  GC028080-IND EQUAL SPACES                                GC028080
00106          PERFORM 0010-PROCESS THRU 0010-EXIT.                     GC028080
00107                                                                   GC028080
00108      IF  GC028080-IND EQUAL 'C'                                   GC028080
00109          PERFORM 0030-CLOSE-FILES THRU 0030-EXIT.                 GC028080
00110                                                                   GC028080
00111  0000-EXIT.                                                       GC028080
00112                                                                   GC028080
00113      GOBACK.                                                      GC028080
00114      EJECT                                                        GC028080
00115  0010-PROCESS.                                                    GC028080
00116                                                                   GC028080
00117      MOVE GCT-CONTRACT-RECORD                                     GC028080
00118                              TO ONEA-REC-AREA.                    GC028080
00119                                                                   GC028080
00120      MOVE JUL-DATE           TO GCT-DT-LAST-CHG-CEN.              GC028080
00121                                                                   GC028080
00122      COMPUTE ONEA-REC-LENG EQUAL                                  GC028080
00123          WRK-RECORD-LENGTH - 100 + 4.                             GC028080
00124                                                                   GC028080
00125      MOVE 'W'                TO REQUEST-TYPE-1.                   GC028080
00126      CALL 'TSGVSAM1' USING PARM-ONE PARM-ONEA.                    GC028080
00127      IF  REQUEST-TYPE-1 NOT EQUAL 'W'                             GC028080
00128          MOVE ONEA-FEEDBACK  TO ABEND-CODE                        GC028080
00129          DISPLAY '*** WRITE TO RELEASE CONTRACT FILE FAILED ***'  GC028080
00130          DISPLAY 'REQUEST TYPE    =' REQUEST-TYPE-1               GC028080
00131          PERFORM 1000-DISP-FIELDS THRU 1000-EXIT                  GC028080
00132          GO TO 9999-ERROR-RTN.                                    GC028080
00133                                                                   GC028080
00134      MOVE GCT-COUNT-BEN-PROVN-POINTERS                            GC028080
00135                                  TO GCT2-COUNT-BEN-PROVN-POINTERS.GC028080
00136      MOVE RLSE-CON-REC           TO CRSUC-REC.                    GC028080
00137                                                                   GC028080
00138      WRITE CRSUC-REC.                                             GC028080
00139                                                                   GC028080
00140  0010-EXIT.                                                       GC028080
00141      EXIT.                                                        GC028080
00142      EJECT                                                        GC028080
00143  0020-OPEN-FILES.                                                 GC028080
00144                                                                   GC028080
00145      OPEN OUTPUT CRSUC.                                           GC028080
00146                                                                   GC028080
00147      MOVE 'S'                TO REQUEST-TYPE-1.                   GC028080
00148      MOVE 8                  TO SET-REC-LENG.                     GC028080
00149      MOVE 3                  TO SET-VALUE.                        GC028080
00150      CALL 'TSGVSAM1' USING PARM-ONE PARM-SET.                     GC028080
00151      IF  REQUEST-TYPE-1 NOT EQUAL 'S'                             GC028080
00152          MOVE SET-FEEDBACK   TO ABEND-CODE                        GC028080
00153          DISPLAY '*** SET FAILED FOR CONTRACT FILE ***'           GC028080
00154          DISPLAY 'REQUEST TYPE    =' REQUEST-TYPE-1               GC028080
00155          GO TO 9999-ERROR-RTN.                                    GC028080
00156                                                                   GC028080
00157      MOVE 'O'                TO REQUEST-TYPE-1.                   GC028080
00158      CALL 'TSGVSAM1' USING PARM-ONE PARM-ONEA.                    GC028080
00159      IF  REQUEST-TYPE-1 NOT EQUAL 'O'                             GC028080
00160          MOVE ONEA-FEEDBACK  TO ABEND-CODE                        GC028080
00161          DISPLAY '*** OPEN FAILED FOR CONTRACT FILE ***'          GC028080
00162          DISPLAY 'REQUEST TYPE    =' REQUEST-TYPE-1               GC028080
00163          GO TO 9999-ERROR-RTN.                                    GC028080
00164                                                                   GC028080
00165      MOVE 'TDY' TO MLDATE-FUNC.                                   GC028080
00166      MOVE 'J' TO MLDATE-FORM1.                                    GC028080
00167      CALL 'MLDATE' USING MLDATE01.                                GC028080
00168      MOVE MLDATE-JUL1 TO JUL-DATE.                                GC028080
00169                                                                   GC028080
00170  0020-EXIT.                                                       GC028080
00171      EXIT.                                                        GC028080
00172      EJECT                                                        GC028080
00173  0030-CLOSE-FILES.                                                GC028080
00174                                                                   GC028080
00175      CLOSE CRSUC.                                                 GC028080
00176                                                                   GC028080
00177      MOVE 'C'                TO REQUEST-TYPE-1.                   GC028080
00178      CALL 'TSGVSAM1' USING PARM-ONE PARM-ONEA.                    GC028080
00179      IF  REQUEST-TYPE-1 NOT EQUAL 'C'                             GC028080
00180          MOVE ONEA-FEEDBACK  TO ABEND-CODE                        GC028080
00181          DISPLAY '*** CLOSE FAILED FOR CONTRACT FILE ***'         GC028080
00182          DISPLAY 'REQUEST TYPE    =' REQUEST-TYPE-1               GC028080
00183          GO TO 9999-ERROR-RTN.                                    GC028080
00184                                                                   GC028080
00185  0030-EXIT.                                                       GC028080
00186      EXIT.                                                        GC028080
00187      EJECT                                                        GC028080
00188  1000-DISP-FIELDS.                                                GC028080
00189      DISPLAY 'PLAN CODE            =' GCT-PLAN-CODE.              GC028080
00190      DISPLAY 'GROUP NUMBER         =' GCT-GROUP-NUM.              GC028080
00191      DISPLAY 'SECTION NUMBER       =' GCT-SECTION-NUM.            GC028080
00192      DISPLAY 'PACKAGE CODE         =' GCT-PKG-CODE.               GC028080
00193      DISPLAY 'LINE OF BUISNESS     =' GCT-L-O-B.                  GC028080
00194      DISPLAY 'PROVIDER CONTROL     =' GCT-PROVDR-CONTROL.         GC028080
00195      DISPLAY 'FAMILY RELATION LEVEL=' GCT-FAM-REL-LVL.            GC028080
00196      DISPLAY 'EFFECTIVE DATE       =' GCT-EFFDT-CEN.              GC028080
00197      DISPLAY '*** YOU ARE IN PROGRAM GC028010TS ***'.             GC028080
00198  1000-EXIT.                                                       GC028080
00199      EXIT.                                                        GC028080
00200  9999-ERROR-RTN.                                                  GC028080
00201                                                                   GC028080
00202      DISPLAY 'YOU ARE IN PROGRAM GC028080TS'.                     GC028080
00203      CALL 'TSGEND' USING ABEND-CODE.                              GC028080
00204                                                                   GC028080
00205  9999-EXIT.                                                       GC028080
00206      EXIT.                                                        GC028080
