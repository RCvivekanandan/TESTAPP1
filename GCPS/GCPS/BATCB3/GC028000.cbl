00001 *      LAST MAINTENANCE TIME: 12.58.17  DATE: 07/23/84            12/09/02
00002  IDENTIFICATION DIVISION.                                         GC028000
00003  PROGRAM-ID. GC028000.                                               LV002
00004  AUTHOR. ROBERT MANN - DECISION CONSULANTS INC.                   GC028000
00005  INSTALLATION. HCSC.                                              GC028000
00006  DATE-WRITTEN.  JUL 06,1984                                       GC028000
00007  DATE-COMPILED.                                                   GC028000
00008 ******************************************************************GC028000
00009 *    THIS PROGRAM IS THE DRIVER FOR UPDATING THE PRODUCTION      *GC028000
00010 *    CONTRACT FILE WITH UPDATED BENEFIT PROVISION SLOT NUMBERS   *GC028000
00011 *    AND UPDATED TABULAR PROVISION SLOT NUMBERS.                 *GC028000
00012 ******************************************************************GC028000
00013 ******************************************************************GC028000
00014 *  UPDATES                                                       *GC028000
00015 *    7/11/96 -RGO  PREVENT INVALID RECORDS WRITTEN TO THE DATES  *GC028000
00016 *             FILE.  CHECK THE GC028070-IND.  IF IT CONTAINS AN  *GC028000
00017 *             'E', ABEND THE PROGRAM AFTER IT FINISHES.  THIS    *GC028000
00018 *             WILL NOTIFY A PROGRAMMER, SO THAT WE CAN RESEARCH  *GC028000
00019 *             IT LATER. SET THE RETURN CODE TO 12                *GC028000
00020 *            -IMPORTANT! ABEND CODES MUST BE LESS THAN 4095!     *GC028000
00021 *                                                                *GC028000
00022 * 14726/15057                                                    *GC028000
00023 *         09/15/97 DAU  RECOMPILED PROGRAM TO SUPPORT THE YEAR   *GC028000
00024 *                       2000 AND THE EXPANSION OF THE GROUP      *GC028000
00025 *                       SPECIFIC AND CONTRACT KEY TO SUPPORT THE *GC028000
00026 *                       TEXAS MERGER.                            *GC028000
00027 *                                                                *GC028000
00028 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GC028000
00029 *                                                                *GC028000
00030 ******************************************************************GC028000
00031  ENVIRONMENT DIVISION.                                            GC028000
00032  CONFIGURATION SECTION.                                           GC028000
00033  SOURCE-COMPUTER. IBM-370.                                        GC028000
00034  OBJECT-COMPUTER. IBM-370.                                        GC028000
00035  INPUT-OUTPUT SECTION.                                            GC028000
00036  FILE-CONTROL.                                                    GC028000
00037      SELECT RLSE-CON-FILE                                         GC028000
00038                              ASSIGN TO UT-S-GC0280A.              GC028000
00039      EJECT                                                        GC028000
00040  DATA DIVISION.                                                   GC028000
00041  FILE SECTION.                                                    GC028000
00042                                                                   GC028000
00043  FD  RLSE-CON-FILE                                                GC028000
00044      LABEL RECORDS ARE STANDARD                                   GC028000
00045      RECORDING MODE IS V                                          GC028000
00046      BLOCK CONTAINS 0 RECORDS.                                    GC028000
00047  01  RLSE-CON-REC.                                                GC028000
00048      COPY GCWRKDCC.                                               GC028000
00049      COPY GCCONTRC.                                               GC028000
00050      EJECT                                                        GC028000
00051  WORKING-STORAGE SECTION.                                         GC028000
00052                                                                   GC028000
00053  01  FILLER          PIC X(24)   VALUE                            GC028000
00054                      'GC028000 WORKING STORAGE'.                  GC028000
00055                                                                   GC028000
00056  01  SWITCH-AREA.                                                 GC028000
00057      05  RC-SW       PIC X   VALUE SPACES.                        GC028000
00058          88  EOF-RC          VALUE HIGH-VALUES.                   GC028000
00059                                                                   GC028000
00060  01  GC028010-IND    PIC X   VALUE 'O'.                           GC028000
00061  01  GC028020-IND    PIC X   VALUE 'O'.                           GC028000
00062  01  GC028070-IND    PIC X   VALUE 'O'.                           GC028000
00063  01  GC028080-IND    PIC X   VALUE 'O'.                           GC028000
00064  01  ABEND-AT-END    PIC X VALUE SPACES.                          GC028000
00065                                                                   GC028000
00066  01  ABEND-CODE      PIC 9(4)   COMP    VALUE ZEROS.              GC028000
00067      EJECT                                                        GC028000
00068  LINKAGE SECTION.                                                 GC028000
00069      EJECT                                                        GC028000
00070  PROCEDURE DIVISION.                                              GC028000
00071                                                                   GC028000
00072  0000-MAINLINE.                                                   GC028000
00073      OPEN INPUT  RLSE-CON-FILE.                                   GC028000
00074                                                                   GC028000
00075      CALL 'GC028080' USING GC028080-IND.                          GC028000
00076                                                                   GC028000
00077      PERFORM 0010-READ-RC-REC THRU 0010-EXIT.                     GC028000
00078                                                                   GC028000
00079      PERFORM 0020-PROCESS THRU 0020-EXIT                          GC028000
00080          UNTIL EOF-RC.                                            GC028000
00081                                                                   GC028000
00082      MOVE 'C'                    TO GC028070-IND                  GC028000
00083                                     GC028080-IND.                 GC028000
00084                                                                   GC028000
00085                                                                   GC028000
00086      IF  GC028010-IND EQUAL SPACES                                GC028000
00087          MOVE 1000       TO ABEND-CODE                            GC028000
00088          DISPLAY '*** ABEND OCCURRED ***'                         GC028000
00089          DISPLAY '    ABEND CODE =' ABEND-CODE                    GC028000
00090          DISPLAY '    YOU ARE IN PROGRAM GC028000TS'              GC028000
00091          DISPLAY '*** EOF NOT REACHED IN PROGRAM GC028010TS ***'  GC028000
00092          GO TO 0060-ERROR-RTN.                                    GC028000
00093                                                                   GC028000
00094      IF  GC028020-IND EQUAL SPACES                                GC028000
00095          MOVE 1005       TO ABEND-CODE                            GC028000
00096          DISPLAY '*** ABEND OCCURRED ***'                         GC028000
00097          DISPLAY '    YOU ARE IN PROGRAM GC028000TS'              GC028000
00098          DISPLAY '    ABEND CODE =' ABEND-CODE                    GC028000
00099          DISPLAY '*** EOF NOT REACHED IN PROGRAM GC028020TS ***'  GC028000
00100          GO TO 0060-ERROR-RTN.                                    GC028000
00101                                                                   GC028000
00102      CALL 'GC028070' USING GC028070-IND.                          GC028000
00103                                                                   GC028000
00104      CALL 'GC028080' USING GC028080-IND.                          GC028000
00105                                                                   GC028000
00106      CLOSE RLSE-CON-FILE.                                         GC028000
00107                                                                   GC028000
00108      IF ABEND-AT-END = 'Y'                                        GC028000
00109         DISPLAY '*E* ABOUT TO ABEND GC0280 WITH CODE 7099.'       GC028000
00110         DISPLAY '*E* NOTIFY PROGRAMMER.'                          GC028000
00111         MOVE 12 TO RETURN-CODE                                    GC028000
00112      END-IF.                                                      GC028000
00113                                                                   GC028000
00114      GOBACK.                                                      GC028000
00115  0000-EXIT.                                                       GC028000
00116      EXIT.                                                        GC028000
00117      EJECT                                                        GC028000
00118  0010-READ-RC-REC.                                                GC028000
00119                                                                   GC028000
00120      READ RLSE-CON-FILE                                           GC028000
00121          AT END                                                   GC028000
00122              MOVE HIGH-VALUES    TO RC-SW                         GC028000
00123              GO TO 0010-EXIT.                                     GC028000
00124                                                                   GC028000
00125  0010-EXIT.                                                       GC028000
00126      EXIT.                                                        GC028000
00127      EJECT                                                        GC028000
00128  0020-PROCESS.                                                    GC028000
00129                                                                   GC028000
00130      IF  GC028010-IND NOT EQUAL HIGH-VALUES                       GC028000
00131          CALL 'GC028010' USING GC028010-IND RLSE-CON-REC.         GC028000
00132                                                                   GC028000
00133      IF  GC028020-IND NOT EQUAL HIGH-VALUES                       GC028000
00134          CALL 'GC028020' USING GC028020-IND RLSE-CON-REC.         GC028000
00135                                                                   GC028000
00136      CALL 'GC028070' USING GC028070-IND RLSE-CON-REC.             GC028000
00137                                                                   GC028000
00138      IF GC028070-IND = 'E'                                        GC028000
00139         MOVE 'Y' TO ABEND-AT-END                                  GC028000
00140         MOVE SPACES TO GC028070-IND                               GC028000
00141      END-IF.                                                      GC028000
00142                                                                   GC028000
00143      CALL 'GC028080' USING GC028080-IND RLSE-CON-REC.             GC028000
00144                                                                   GC028000
00145      PERFORM 0010-READ-RC-REC THRU 0010-EXIT.                     GC028000
00146                                                                   GC028000
00147  0020-EXIT.                                                       GC028000
00148      EXIT.                                                        GC028000
00149      EJECT                                                        GC028000
00150  0060-ERROR-RTN.                                                  GC028000
00151                                                                   GC028000
00152      CALL 'TSGEND' USING ABEND-CODE.                              GC028000
00153                                                                   GC028000
00154  0060-EXIT.                                                       GC028000
00155      EXIT.                                                        GC028000
