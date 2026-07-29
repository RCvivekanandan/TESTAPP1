00001 *      LAST MAINTENANCE TIME: 13.00.50  DATE: 11/13/84            12/09/02
00002  IDENTIFICATION DIVISION.                                         GC029080
00003  PROGRAM-ID.  GC029080.                                              LV002
00004  AUTHOR. ROBERT MANN - DECISION CONSULTANTS INC.                  GC029080
00005  INSTALLATION.  HCSC.                                             GC029080
00006  DATE-WRITTEN.  JUL 11,1984.                                      GC029080
00007  DATE-COMPILED.                                                   GC029080
00008 ***************************************************************** GC029080
00009 *    THIS PROGRAM UPDATES THE ONLINE GRP SPEC FILE AND CREATES    GC029080
00010 *    A COMPLETED RELEASE SLOT UPDATED GRP SPEC FILE.              GC029080
00011 *                                                                 GC029080
00012 *    THE ENTIRE GRP SPEC RECORD IS REPLACED ON THE ONLINE FILE.   GC029080
00013 *                                                                 GC029080
00014 *    THIS PROGRAM MODULE CALLED BY PROGRAM GC029000               GC029080
00015 *    AND IS NOT TO BE EXECUTED AS A STAND ALONE PROGRAM.          GC029080
00016 *                                                                 GC029080
00017 *    TSGVSAM1 IS THE ONLINE GRP SPEC FILE - (OUTPUT).             GC029080
00018 *    COMPLETED RELEASE GRP SPEC FILE- CRGUC- (OUTPUT).            GC029080
00019 ***************************************************************** GC029080
00020 *                                                               * GC029080
00021 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        * GC029080
00022 *    *-*         U P D A T E   H I S T O R Y         *-*        * GC029080
00023 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        * GC029080
00024 *                                                               * GC029080
00025 * CHG #     DATE      BY             DESCRIPTION                * GC029080
00026 * _____   ________   ___    __________________________________  * GC029080
00027 *                                                               * GC029080
00028 * 11161   11/13/90   FRY    CHANGE HARD CODED GROUP SPECIFIC    * GC029080
00029 *                           LENGTH.                             * GC029080
00030 *                                                               * GC029080
00031 * 12009   09/19/91   TPM    ADJUSTED THE ONEA-REC-AREA TO       * GC029080
00032 *                           ACCOMODATE FOR THE EXPANSION IN THE * GC029080
00033 *                           FAMILY RELATION FIELD               * GC029080
00034 *                                                               * GC029080
00035 *         01/17/95   GDM    CONVERT TO COBOL II                 * GC029080
00036 *                                                               * GC029080
00037 * 14726/  10/08/97   DAU    ADDED CODE TO SUPPORT THE YEAR 2000 * GC029080
00038 * 15057                     AND THE EXPANSION OF THE GROUP      * GC029080
00039 *                           SPECIFIC AND CONTRACT KEY TO SUPPORT* GC029080
00040 *                           THE TEXAS MERGER.                    *GC029080
00041 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GC029080
      *                                                                *GC029000
      * P20368   09/11/15  KIKI COMPILE ONLY - BD / TC                 *GC029000
      *                         EXTEND - GCGROUPC / GCGROUP2           *GC029000
      *                                                                *GC029000
      * P22147   05/03/17  TROY COMPILE ONLY - CE                      *GC029000
      *                         EXTEND - GCGROUPC / GCGROUP2           *GC029000
      *                                                                *GC029000
      * P21681     09/28/17  SRI  COMPILE ONLY - UTIL-MANAGEMENT       *GC029000
      *                           IND    - GCGROUPC                    *GC029000
      *                                                                *GC029000
      ******************************************************************GC029010
00044                                                                   GC029080
00045  ENVIRONMENT DIVISION.                                            GC029080
00046  CONFIGURATION SECTION.                                           GC029080
00047  SOURCE-COMPUTER. IBM-370.                                        GC029080
00048  OBJECT-COMPUTER. IBM-370.                                        GC029080
00049  INPUT-OUTPUT SECTION.                                            GC029080
00050  FILE-CONTROL.                                                    GC029080
00051      SELECT CRGUC            ASSIGN TO UT-S-GC0290C.              GC029080
00052      EJECT                                                        GC029080
00053  DATA DIVISION.                                                   GC029080
00054  FILE SECTION.                                                    GC029080
00055                                                                   GC029080
00056  FD  CRGUC                                                        GC029080
00057      LABEL RECORDS ARE STANDARD                                   GC029080
00058      RECORDING MODE IS V                                          GC029080
00059      BLOCK CONTAINS 0 RECORDS.                                    GC029080
00060  01  CRGUC-REC.                                                   GC029080
00061      COPY GCWRKDC2.                                               GC029080
00062      COPY GCGROUP2.                                               GC029080
00063      EJECT                                                        GC029080
00064  WORKING-STORAGE SECTION.                                         GC029080
00065                                                                   GC029080
00066  01  FILLER            PIC X(24)                                  GC029080
00067               VALUE 'GC029080 WORKING STORAGE'.                   GC029080
00068                                                                   GC029080
00069  01  ABEND-CODE        PIC 9(4)  COMP.                            GC029080
00070                                                                   GC029080
00071  01  DATE-WORK-AREA.                                              GC029080
00072      05  JUL-DATE            PIC 9(7).                            GC029080
00073                                                                   GC029080
00074      COPY MLDATE01.                                               GC029080
00075      EJECT                                                        GC029080
00076  01  PARM-SET.                                                    GC029080
00077      05  SET-RDW.                                                 GC029080
00078          10  SET-REC-LENG    PIC 9(4)    VALUE ZEROS     COMP.    GC029080
00079          10  SET-FEEDBACK    PIC 9(4)    VALUE ZEROS     COMP.    GC029080
00080      05  SET-VALUE           PIC 9(8)                    COMP.    GC029080
00081                                                                   GC029080
00082  01  PARM-ONE.                                                    GC029080
00083      05  RESERVED-FLDS-1     PIC 9(8)    VALUE ZEROS     COMP.    GC029080
00084      05  RESERVED-X-1 REDEFINES RESERVED-FLDS-1.                  GC029080
00085          10  REQUEST-TYPE-1  PIC X.                               GC029080
00086          10  FILLER          PIC X(3).                            GC029080
00087                                                                   GC029080
00088  01  PARM-ONEA.                                                   GC029080
00089      05  ONEA-RDW.                                                GC029080
00090          10  ONEA-REC-LENG   PIC 9(4)    VALUE ZEROS     COMP.    GC029080
00091          10  ONEA-FEEDBACK   PIC 9(4)    VALUE ZEROS     COMP.    GC029080
00092      05  ONEA-REC-AREA       PIC X(1200).                         GC029080
00093      05  ONEA-REC-X REDEFINES ONEA-REC-AREA.                      GC029080
00094          10  ONEA-KEY        PIC X(26).                           GC029080
00095          10  ONEA-DATA.                                           GC029080
00096              15  FILLER      PIC X(4).                            GC029080
00097              15  GCG-DT-LAST-CHG.                                 GC029080
00098                  20  GCG-DT-LAST-CHG-CC   PIC X.                  GC029080
00099                  20  GCG-DT-OF-LAST-CHG   PIC S9(5)   COMP-3.     GC029080
00100              15  GCG-DT-LAST-CHG-CEN REDEFINES                    GC029080
00101                     GCG-DT-LAST-CHG       PIC S9(7)   COMP-3.     GC029080
00102              15  FILLER      PIC X(1166).                         GC029080
00103      EJECT                                                        GC029080
00104  LINKAGE SECTION.                                                 GC029080
00105                                                                   GC029080
00106  01  GC029080-IND        PIC X.                                   GC029080
00107                                                                   GC029080
00108  01  RLSE-GRP-REC.                                                GC029080
00109      COPY GCWRKDCC.                                               GC029080
00110      COPY GCGROUPC.                                               GC029080
00111      EJECT                                                        GC029080
00112  PROCEDURE DIVISION USING GC029080-IND RLSE-GRP-REC.              GC029080
00113                                                                   GC029080
00114  0000-MAINLINE.                                                   GC029080
00115                                                                   GC029080
00116      IF  GC029080-IND EQUAL 'O'                                   GC029080
00117          MOVE SPACES             TO GC029080-IND                  GC029080
00118          PERFORM 0020-OPEN-FILES THRU 0020-EXIT                   GC029080
00119          GO TO 0000-EXIT.                                         GC029080
00120                                                                   GC029080
00121      IF  GC029080-IND EQUAL SPACES                                GC029080
00122          PERFORM 0010-PROCESS THRU 0010-EXIT.                     GC029080
00123                                                                   GC029080
00124      IF  GC029080-IND EQUAL 'C'                                   GC029080
00125          PERFORM 0030-CLOSE-FILES THRU 0030-EXIT.                 GC029080
00126                                                                   GC029080
00127  0000-EXIT.                                                       GC029080
00128                                                                   GC029080
00129      GOBACK.                                                      GC029080
00130      EJECT                                                        GC029080
00131  0010-PROCESS.                                                    GC029080
00132                                                                   GC029080
00133      MOVE GCG-GRP-SPEC-RECORD                                     GC029080
00134                              TO ONEA-REC-AREA.                    GC029080
00135                                                                   GC029080
00136      MOVE JUL-DATE           TO GCG-DT-LAST-CHG-CEN.              GC029080
00137                                                                   GC029080
00138      COMPUTE ONEA-REC-LENG EQUAL                                  GC029080
00139          WRK-RECORD-LENGTH - 100 + 4.                             GC029080
00140                                                                   GC029080
00141      MOVE 'W'                TO REQUEST-TYPE-1.                   GC029080
00142      CALL 'TSGVSAM1' USING PARM-ONE PARM-ONEA.                    GC029080
00143      IF  REQUEST-TYPE-1 NOT EQUAL 'W'                             GC029080
00144          MOVE ONEA-FEEDBACK  TO ABEND-CODE                        GC029080
00145          DISPLAY '*** ABEND OCCURRED ***'                         GC029080
00146          DISPLAY 'ABEND CODE   =' ABEND-CODE                      GC029080
00147          DISPLAY '*** WRITE TO RELEASE CONTRACT FILE FAILED ***'  GC029080
00148          DISPLAY 'REQUEST TYPE    =' REQUEST-TYPE-1               GC029080
00149          PERFORM 1000-DISP-FIELDS THRU 1000-EXIT                  GC029080
00150          GO TO 9999-ERROR-RTN.                                    GC029080
00151                                                                   GC029080
00152      MOVE GCG-COUNT-TAB-PROVN-POINTERS                            GC029080
00153                                  TO GCG2-COUNT-TAB-PROVN-POINTERS.GC029080
00154      MOVE RLSE-GRP-REC           TO CRGUC-REC.                    GC029080
00155                                                                   GC029080
00156      WRITE CRGUC-REC.                                             GC029080
00157                                                                   GC029080
00158  0010-EXIT.                                                       GC029080
00159      EXIT.                                                        GC029080
00160      EJECT                                                        GC029080
00161  0020-OPEN-FILES.                                                 GC029080
00162                                                                   GC029080
00163      OPEN OUTPUT CRGUC.                                           GC029080
00164                                                                   GC029080
00165      MOVE 'S'                TO REQUEST-TYPE-1.                   GC029080
00166      MOVE 8                  TO SET-REC-LENG.                     GC029080
00167      MOVE 3                  TO SET-VALUE.                        GC029080
00168      CALL 'TSGVSAM1' USING PARM-ONE PARM-SET.                     GC029080
00169      IF  REQUEST-TYPE-1 NOT EQUAL 'S'                             GC029080
00170          MOVE SET-FEEDBACK   TO ABEND-CODE                        GC029080
00171          DISPLAY '*** ABEND OCCURRED ***'                         GC029080
00172          DISPLAY 'ABEND CODE   =' ABEND-CODE                      GC029080
00173          DISPLAY '*** SET FAILED FOR GROUP FILE ***'              GC029080
00174          DISPLAY 'REQUEST TYPE    =' REQUEST-TYPE-1               GC029080
00175          GO TO 9999-ERROR-RTN.                                    GC029080
00176                                                                   GC029080
00177      MOVE 'O'                TO REQUEST-TYPE-1.                   GC029080
00178      CALL 'TSGVSAM1' USING PARM-ONE PARM-ONEA.                    GC029080
00179      IF  REQUEST-TYPE-1 NOT EQUAL 'O'                             GC029080
00180          MOVE ONEA-FEEDBACK  TO ABEND-CODE                        GC029080
00181          DISPLAY '*** ABEND OCCURRED ***'                         GC029080
00182          DISPLAY 'ABEND CODE   =' ABEND-CODE                      GC029080
00183          DISPLAY '*** OPEN FAILED FOR GROUP FILE ***'             GC029080
00184          DISPLAY 'REQUEST TYPE    =' REQUEST-TYPE-1               GC029080
00185          GO TO 9999-ERROR-RTN.                                    GC029080
00186                                                                   GC029080
00187      MOVE 'TDY' TO MLDATE-FUNC.                                   GC029080
00188      MOVE 'J' TO MLDATE-FORM1.                                    GC029080
00189      CALL 'MLDATE' USING MLDATE01.                                GC029080
00190      MOVE MLDATE-JUL1 TO JUL-DATE.                                GC029080
00191                                                                   GC029080
00192  0020-EXIT.                                                       GC029080
00193      EXIT.                                                        GC029080
00194      EJECT                                                        GC029080
00195  0030-CLOSE-FILES.                                                GC029080
00196                                                                   GC029080
00197      CLOSE CRGUC.                                                 GC029080
00198                                                                   GC029080
00199      MOVE 'C'                TO REQUEST-TYPE-1.                   GC029080
00200      CALL 'TSGVSAM1' USING PARM-ONE PARM-ONEA.                    GC029080
00201      IF  REQUEST-TYPE-1 NOT EQUAL 'C'                             GC029080
00202          MOVE ONEA-FEEDBACK  TO ABEND-CODE                        GC029080
00203          DISPLAY '*** ABEND OCCURRED ***'                         GC029080
00204          DISPLAY 'ABEND CODE   =' ABEND-CODE                      GC029080
00205          DISPLAY '*** CLOSE FAILED FOR GROUP FILE ***'            GC029080
00206          DISPLAY 'REQUEST TYPE    =' REQUEST-TYPE-1               GC029080
00207          GO TO 9999-ERROR-RTN.                                    GC029080
00208                                                                   GC029080
00209  0030-EXIT.                                                       GC029080
00210      EXIT.                                                        GC029080
00211      EJECT                                                        GC029080
00212  1000-DISP-FIELDS.                                                GC029080
00213      DISPLAY 'PLAN CODE            =' GCG-PLAN-CODE.              GC029080
00214      DISPLAY 'GROUP NUMBER         =' GCG-GROUP-NUM.              GC029080
00215      DISPLAY 'SECTION NUMBER       =' GCG-SECTION-NUM.            GC029080
00216      DISPLAY 'PACKAGE CODE         =' GCG-PKG-CODE.               GC029080
00217      DISPLAY 'FAMILY RELATION      =' GCG-FAM-REL-LVL.            GC029080
00218      DISPLAY 'EFFECTIVE DATE       =' GCG-EFFDT-CEN.              GC029080
00219      DISPLAY '*** YOU ARE IN PROGRAM GC029080TS ***'.             GC029080
00220  1000-EXIT.                                                       GC029080
00221      EXIT.                                                        GC029080
00222  9999-ERROR-RTN.                                                  GC029080
00223                                                                   GC029080
00224      CALL 'TSGEND' USING ABEND-CODE.                              GC029080
00225                                                                   GC029080
00226  9999-EXIT.                                                       GC029080
00227      EXIT.                                                        GC029080
