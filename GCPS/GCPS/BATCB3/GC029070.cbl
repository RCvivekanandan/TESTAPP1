       CBL DTR                                                                  
00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID.  GC029070.                                           GC029070
00003  AUTHOR. ROBERT MANN - DECISION CONSULTANTS INC.                     LV002
00004  INSTALLATION.  HCSC.                                             GC029070
00005  DATE-WRITTEN.  JUL 11,1984.                                      GC029070
00006  DATE-COMPILED.                                                   GC029070
00007 ******************************************************************GC029070
00008 *    THIS PROGRAM UPDATES THE ONLINE GRP SPEC FILE AND            GC029070
00009 *    AND THE ONLINE DATE FILE.                                    GC029070
00010 *                                                                 GC029070
00011 *    THE MAIN FUNCTION IS TO LOAD POLICY EFFECTIVE AND            GC029070
00012 *    TERMINATION DATES ON THE THE DATE FILE AND                   GC029070
00013 *    POLICY TERMINATION DATES ON THE GRP SPEC FILE.               GC029070
00014 *                                                                 GC029070
00015 *    THIS PROGRAM MODULE CALLED BY PROGRAM GC029000               GC029070
00016 *    AND IS NOT TO BE EXECUTED AS A STAND ALONE PROGRAM.          GC029070
00017 *                                                                 GC029070
00018 *    TSGVSAM1 IS THE ONLINE GRP SPEC FILE - (I/O).                GC029070
00019 *    TSGVSAM2 IS THE DATE FILE - (I/O).                           GC029070
00020 *                                                                 GC029070
00021 *    NEW DATE FIELD ADDED MARCH 1985.       RKH                   GC029070
00022 *                                                                 GC029070
00023 *                                                                 GC029070
00024 ******************************************************************GC029070
00025 *  DATE      AUTHOR            REASON FOR CHANGE                 *GC029070
00026 ******************************************************************GC029070
00027 *  MARCH 85           NEW DATE FILE ADDED                        *GC029070
00028 *  02/04/87    RKH    ADDED LOGIC FOR KEY-FIELD ADDED RECORDS.   *GC029070
00029 *                     THIS ALLOWS THE DATE RECORD TO HAVE THIS   *GC029070
00030 *                     NEW OCCURANCE ADDED ONLY / AND THE OLD     *GC029070
00031 *                     OCCURANCE ON THE DATE REC IS NOT MODIFIED. *GC029070
00032 *                                                                *GC029070
00033 *  09/10/91    GDM    INCREASE TAB-FAM-RELAT TO 2 POSITIONS      *GC029070
00034 *                     PROJECT D12009                             *GC029070
00035 *                                                                *GC029070
00036 *  09/19/91    TPM    EXPANSION OF THE FAMILY-RELATION FIELD.    *GC029070
00037 *  D12009             REDUCE THE NUMBER OF OCCURS IN THE TABLE-  *GC029070
00038 *                     HOLD-AREA FROM 400 TO 396 TO ACCOMODATE    *GC029070
00039 *                     FOR THE ABOVE CHANGE.                      *GC029070
00040 *                     REMOVE HARD CODED RECORD LENGTHS FOR       *GC029070
00041 *                     GCDATES AND INSERTED COPYBOOK MEMBER       *GC029070
00042 *                     GCCDRLEN  TO BE USED TO INCLUDE NEW RECORD *GC029070
00043 *                     LENGTHS TO HANDLE THE EXPANSION IN THE     *GC029070
00044 *                     FAMILY RELATION FIELD.                     *GC029070
00045 *                                                                *GC029070
00046 *                     CHANGED THE RECORD LENGTH FROM 18 TO 19    *GC029070
00047 *                     WHEN CALLING THE TSGVSAM ROUTINE.          *GC029070
00048 *                                                                *GC029070
00049 *                     CHANGED THE COMPARE OF GCG-FAM-REL-LVL     *GC029070
00050 *                     FROM '0'  TO '00'.                         *GC029070
00051 *                                                                *GC029070
00052 *  01/17/95    GDM    CONVERT TO COBOL II                        *GC029070
00053 *  08/07/96    RGO -ADDED CODE TO DETECT AND PREVENT ADDING A    *GC029070
00054 *                   DATE RECORD WITH THE SAME EFFECTIVE DATE AS  *GC029070
00055 *                   ANOTHER RECORD.                              *GC029070
00056 *                   WHEN THIS IS FOUND (WHICH SHOULD BE VERY     *GC029070
00057 *                   RARELY), DISPLAY INFO FOR DEBUGGING, AND     *GC029070
00058 *                   SET THE RETURN CODE TO 'E'.  THE MAIN PROGRAM*GC029070
00059 *                   WILL THEN ABEND AFTER IT FINISHES            *GC029070
00060 *                   PROCESSING.                                  *GC029070
00061 *                  -CHANGED ALL ABEND CODES TO BE LESS THAN 4095.*GC029070
00062 *                   ANY ABEND CODES GREATER WILL NOT BE CONVERTED*GC029070
00063 *                   CORRECTLY.                                   *GC029070
00064 *                                                                *GC029070
00065 *  14726/  10/08/97  DAU  ADDED CODE TO SUPPORT THE YEAR 2000    *GC029070
00066 *  15057                  AND THE EXPANSION OF THE GROUP SPECIFIC*GC029070
00067 *                         AND CONTRACT KEY TO SUPPORT THE TEXAS  *GC029070
00068 *                         MERGER.                                *GC029070
00069 *                                                                *GC029070
      *          05/17/03  AKK  REGEN TO PICK UP 8 DIGIT OPID CODE     *GC029070
      *                                                                *GC029000
      * P20368   09/11/15  KIKI COMPILE ONLY - BD / TC                 *GC029000
      *                           EXTEND - GCGROUPC / GCGROUP2         *GC029000
      *                                                                *GC029000
      * P22147   05/03/17  TROY COMPILE ONLY - CE                      *GC029000
      *                           EXTEND - GCGROUPC / GCGROUP2         *GC029000
      *                                                                *GC029000
      * P21681     09/28/17  SRI  COMPILE ONLY - UTIL-MANAGEMENT       *GC029000
      *                           IND    - GCGROUPC                    *GC029000
      *                                                                *GC029000
      *            06/XX/29  DJD  ADD MESSAGES TO TSGVSAM1 RET-CODE    *GC029000
      *                           LOOK FOR \
      *                          -COMPILE WITH COBOL 6.2                        
      *                                                                *GC029000
      ******************************************************************GC029010
00072 *                                                                 GC029070
00073  ENVIRONMENT DIVISION.                                            GC029070
00074  CONFIGURATION SECTION.                                           GC029070
00075  SOURCE-COMPUTER. IBM-370.                                        GC029070
00076  OBJECT-COMPUTER. IBM-370.                                        GC029070
00077  INPUT-OUTPUT SECTION.                                            GC029070
00078  FILE-CONTROL.                                                    GC029070
00079  DATA DIVISION.                                                   GC029070
00080  FILE SECTION.                                                    GC029070
00081 /                                                                 GC029070
00082  WORKING-STORAGE SECTION.                                         GC029070
00083                                                                   GC029070
00084  01  FILLER            PIC X(24)                                  GC029070
00085               VALUE 'GC029070 WORKING STORAGE'.                   GC029070
00086                                                                   GC029070
00087 /                                                                 GC029070
00088  01  WS-REC-LENGTHS.                                              GC029070
00089      COPY GCCDRLEN.                                               GC029070
00090 /                                                                 GC029070
00091                                                                   GC029070
00092  01  ABEND-CODE        PIC 9(4)  COMP.                            GC029070
00093                                                                   GC029070
00094  01  DATE-WORK-AREA.                                              GC029070
00095      05  JUL-DATE            PIC 9(7).                            GC029070
00096                                                                   GC029070
00097      COPY MLDATE01.                                               GC029070
00098                                                                   GC029070
00099 /                                                                 GC029070
00100  01  PARM-SET.                                                    GC029070
00101      05  SET-RDW.                                                 GC029070
00102          10  SET-REC-LENG    PIC 9(4)    VALUE ZEROS     COMP.    GC029070
00103          10  SET-FEEDBACK    PIC 9(4)    VALUE ZEROS     COMP.    GC029070
00104      05  SET-VALUE           PIC 9(8)                    COMP.    GC029070
00105                                                                   GC029070
00106  01  PARM-ONE.                                                    GC029070
00107      05  RESERVED-FLDS-1     PIC 9(8)    VALUE ZEROS     COMP.    GC029070
00108      05  RESERVED-X-1 REDEFINES RESERVED-FLDS-1.                  GC029070
00109          10  REQUEST-TYPE-1  PIC X.                               GC029070
00110          10  FILLER          PIC X(3).                            GC029070
00111                                                                   GC029070
00112  01  PARM-ONEA.                                                   GC029070
00113      02  ONEA-RDW.                                                GC029070
00114          05  ONEA-REC-LENG   PIC 9(4)    VALUE ZEROS     COMP.    GC029070
00115          05  ONEA-FEEDBACK   PIC 9(4)    VALUE ZEROS     COMP.    GC029070
00116      02  ONEA-REC-AREA.                                           GC029070
00117          COPY GCGROUP2.                                           GC029070
00118 /                                                                 GC029070
00119  01  PARM-TWO.                                                    GC029070
00120      05  RESERVED-FLDS-2     PIC 9(8)    VALUE ZEROS     COMP.    GC029070
00121      05  RESERVED-X-2 REDEFINES RESERVED-FLDS-2.                  GC029070
00122          10  REQUEST-TYPE-2  PIC X.                               GC029070
00123          10  FILLER          PIC X(3).                            GC029070
00124                                                                   GC029070
00125  01  PARM-TWOA.                                                   GC029070
00126      02  TWOA-RDW.                                                GC029070
00127          05  TWOA-REC-LENG   PIC 9(4)    VALUE ZEROS     COMP.    GC029070
00128          05  TWOA-FEEDBACK   PIC 9(4)    VALUE ZEROS     COMP.    GC029070
00129      02  TWOA-REC-AREA.                                           GC029070
00130      COPY GCDATEC5.                                               GC029070
00131 /                                                                 GC029070
00132  01  SUBSCRIPT-AREA.                                              GC029070
00133      05  CHECK-VALUE             PIC S9(4)   VALUE ZERO.          GC029070
00134      05  SORT-VALUE              PIC S9(4)   VALUE ZERO.          GC029070
00135      05  H                       PIC S9(4)   VALUE ZERO.          GC029070
00136      05  I                       PIC S9(4)   VALUE ZERO.          GC029070
00137      05  J                       PIC S9(4)   VALUE ZERO.          GC029070
00138      05  K                       PIC S9(4)   VALUE ZERO.          GC029070
00139      05  SUBX                    PIC S9(4)   VALUE ZERO.          GC029070
00140                                                                   GC029070
00141  01  SWITCH-AREA.                                                 GC029070
00142      05  WAS-OCCURANCE-FOUND     PIC  X(3)   VALUE 'NO '.         GC029070
00143          88 OCCURS-WAS-FOUND                 VALUE 'YES'.         GC029070
00144                                                                   GC029070
00145  01  TABLE-HOLD-AREA.                                             GC029070
00146      05  TAB-WORK-AREA           PIC X(12)   VALUE SPACES.        GC029070
00147      05  TAB-ENTRY-COUNT         PIC 9(3)    VALUE 1.             GC029070
00148      05  TAB-WORK-AREA-ALL-TERM.                                  GC029070
00149          10  TAB-WRK-TERM        OCCURS 1 TO 410 TIMES            GC029070
00150              DEPENDING ON TAB-ENTRY-COUNT.                        GC029070
00151              15  TAB-EFFECTIVE-DATE.                              GC029070
00152                  20  TAB-EFFDT-CC            PIC X(1).            GC029070
00153                  20  TAB-EFF         COMP-3  PIC S9(5).           GC029070
00154              15  TAB-EFFDT-CEN REDEFINES                          GC029070
00155                  TAB-EFFECTIVE-DATE  COMP-3  PIC S9(7).           GC029070
00156              15  TAB-REST.                                        GC029070
00157                  20  TAB-PROVIDER            PIC XX.              GC029070
00158                  20  TAB-FAMILY-RELAT        PIC XX.              GC029070
00159              15  TAB-TERM-DATE.                                   GC029070
00160                  20  TAB-TERMDT-CC           PIC X(1).            GC029070
00161                  20  TAB-TERM        COMP-3  PIC S9(5).           GC029070
00162              15  TAB-TERMDT-CEN REDEFINES                         GC029070
00163                  TAB-TERM-DATE       COMP-3  PIC S9(7).           GC029070
00164                                                                   GC029070
00165  01  WS-AREA.                                                     GC029070
00166      05  SAME-DATE-FOUND     PIC X(1)    VALUE SPACES.            GC029070
00167      05  REAL-COUNT          PIC S9(5)  COMP-3 VALUE ZEROS.       GC029070
00168                                                                   GC029070
00169 /                                                                 GC029070
00170  LINKAGE SECTION.                                                 GC029070
00171                                                                   GC029070
00172  01  GC029070-IND        PIC X.                                   GC029070
00173                                                                   GC029070
00174  01  RLSE-GRP-SPEC-REC.                                           GC029070
00175      COPY GCWRKDCC.                                               GC029070
00176      COPY GCGROUPC.                                               GC029070
00177 /                                                                 GC029070
00178  PROCEDURE DIVISION USING GC029070-IND RLSE-GRP-SPEC-REC.         GC029070
00179                                                                   GC029070
00180  0000-MAINLINE.                                                   GC029070
00181                                                                   GC029070
00182      IF  GC029070-IND EQUAL 'O'                                   GC029070
00183          MOVE SPACES             TO GC029070-IND                  GC029070
00184          PERFORM 0900-OPEN-FILES THRU 0900-EXIT.                  GC029070
00185                                                                   GC029070
00186      IF  GC029070-IND EQUAL SPACES                                GC029070
00187          PERFORM 0010-PROCESS THRU 0010-EXIT.                     GC029070
00188                                                                   GC029070
00189      IF  GC029070-IND EQUAL 'C'                                   GC029070
00190          PERFORM 0910-CLOSE-FILES THRU 0910-EXIT.                 GC029070
00191                                                                   GC029070
00192      GOBACK.                                                      GC029070
00193                                                                   GC029070
00194  0000-EXIT.                                                       GC029070
00195      EXIT.                                                        GC029070
00196 /                                                                 GC029070
00197  0010-PROCESS.                                                    GC029070
00198                                                                   GC029070
00199      IF  WRK-CHANGE-REQUEST                                       GC029070
00200          PERFORM 0020-PROCESS-CHANGE-REQ THRU 0020-EXIT           GC029070
00201          GO TO 0010-EXIT.                                         GC029070
00202                                                                   GC029070
00203      IF (WRK-ADD-REQUEST OR                                       GC029070
00204          WRK-KEY-FLD-ADD-REQ )                                    GC029070
00205          PERFORM 0030-PROCESS-ADD-REQ THRU 0030-EXIT              GC029070
00206          GO TO 0010-EXIT.                                         GC029070
00207                                                                   GC029070
00208  0010-EXIT.                                                       GC029070
00209      EXIT.                                                        GC029070
00210 /                                                                 GC029070
00211  0020-PROCESS-CHANGE-REQ.                                         GC029070
00212                                                                   GC029070
00213      PERFORM 0070-LOAD-DATE-KEY THRU 0070-EXIT.                   GC029070
00214                                                                   GC029070
00215      PERFORM 0080-READ-DATE THRU 0080-EXIT.                       GC029070
00216                                                                   GC029070
00217      MOVE 1 TO H.                                                 GC029070
00218      PERFORM 0150-DET-CORRECT-OCCUR THRU 0150-EXIT.               GC029070
00219                                                                   GC029070
00220      MOVE GCG-TERMDT-CEN     TO DTE-TERMDT-CEN (H).               GC029070
00221                                                                   GC029070
00222      PERFORM 0090-WRITE-DATE THRU 0090-EXIT.                      GC029070
00223                                                                   GC029070
00224  0020-EXIT.                                                       GC029070
00225      EXIT.                                                        GC029070
00226 /                                                                 GC029070
00227  0030-PROCESS-ADD-REQ.                                            GC029070
00228                                                                   GC029070
00229      PERFORM 0070-LOAD-DATE-KEY THRU 0070-EXIT.                   GC029070
00230                                                                   GC029070
00231      PERFORM 0080-READ-DATE THRU 0080-EXIT.                       GC029070
00232                                                                   GC029070
00233      IF  REQUEST-TYPE-2 EQUAL '3'                                 GC029070
00234          PERFORM 0035-ADD-NEW-RECORD THRU 0035-EXIT               GC029070
00235          GO TO 0030-EXIT.                                         GC029070
00236                                                                   GC029070
00237      IF  WRK-ADD-REQUEST                                          GC029070
00238          PERFORM 0040-UPDATE-DATE-REC-OCCURS  THRU 0040-EXIT      GC029070
00239          GO TO 0030-EXIT.                                         GC029070
00240                                                                   GC029070
00241      IF  WRK-KEY-FLD-ADD-REQ                                      GC029070
00242          PERFORM 0040-ADD-DATE-REC-OCCURS  THRU 0040-EXIT         GC029070
00243          GO TO 0030-EXIT.                                         GC029070
00244                                                                   GC029070
00245  0030-EXIT.  EXIT.                                                GC029070
00246 /                                                                 GC029070
00247  0035-ADD-NEW-RECORD.                                             GC029070
00248                                                                   GC029070
00249      MOVE GC-GCDATES-VARY-MAX-OCUR TO DTE-ENTRY-COUNT.            GC029070
00250      MOVE LOW-VALUES             TO DTE-RECORD.                   GC029070
00251      MOVE 2                      TO DTE-ENTRY-COUNT.              GC029070
00252      MOVE GCG-EFFDT-CEN          TO DTE-EFFDT-CEN (1).            GC029070
00253      MOVE GCG-FAM-REL-LVL        TO DTE-FAMILY-RELAT-LEVEL (1).   GC029070
00254      MOVE LOW-VALUES             TO DTE-PROVIDER-CNTRL (1).       GC029070
00255      MOVE GCG-TERMDT-CEN         TO DTE-TERMDT-CEN (1).           GC029070
00256 *                                                                 GC029070
00257      MOVE HIGH-VALUES            TO DTE-EFF-TERM (2).             GC029070
00258 *                                                                 GC029070
00259      PERFORM 0070-LOAD-DATE-KEY THRU 0070-EXIT.                   GC029070
00260                                                                   GC029070
00261      IF  GCG-FAM-REL-LVL  = '00'                                  GC029070
00262          MOVE '0' TO DTE-F-R-L-IN-USE-IND                         GC029070
00263      ELSE                                                         GC029070
00264          MOVE 'Y' TO DTE-F-R-L-IN-USE-IND.                        GC029070
00265                                                                   GC029070
00266      MOVE LOW-VALUES  TO DTE-PROVIDER-CNTL-IN-USE-IND.            GC029070
00267                                                                   GC029070
00268      PERFORM 0090-WRITE-DATE THRU 0090-EXIT.                      GC029070
00269                                                                   GC029070
00270  0035-EXIT.                                                       GC029070
00271      EXIT.                                                        GC029070
00272 /                                                                 GC029070
00273  0040-UPDATE-DATE-REC-OCCURS.                                     GC029070
00274                                                                   GC029070
00275      MOVE 1 TO H.                                                 GC029070
00276      PERFORM 0160-FIND-OCCUR  THRU 0160-EXIT.                     GC029070
00277                                                                   GC029070
00278      IF  OCCURS-WAS-FOUND AND                                     GC029070
00279          DTE-TERMDT-CEN (H) EQUAL +9999365                        GC029070
00280          PERFORM 0050-DATE-CHECK THRU 0050-EXIT.                  GC029070
00281                                                                   GC029070
00282  0040-ADD-DATE-REC-OCCURS.                                        GC029070
00283                                                                   GC029070
00284 *  ADD CODE TO PREVENT ADDING A RECORD WITH SAME EFF DATE. RGO    GC029070
00285                                                                   GC029070
00286      MOVE 'N' TO SAME-DATE-FOUND.                                 GC029070
00287                                                                   GC029070
00288      COMPUTE REAL-COUNT = DTE-ENTRY-COUNT - 1.                    GC029070
00289      PERFORM 115-CHECK-DUP-ENTRY                                  GC029070
00290          VARYING SUBX FROM 1 BY 1                                 GC029070
00291          UNTIL ( (SUBX > REAL-COUNT)                              GC029070
00292                 OR (SAME-DATE-FOUND = 'Y')).                      GC029070
00293      IF SAME-DATE-FOUND = 'Y'                                     GC029070
00294         MOVE 'E' TO GC029070-IND                                  GC029070
00295         GO TO 0040-EXIT                                           GC029070
00296      END-IF.                                                      GC029070
00297                                                                   GC029070
00298                                                                   GC029070
00299      SET DTE-INDEX2 TO DTE-ENTRY-COUNT.                           GC029070
00300      SET DTE-INDEX2 UP BY 1.                                      GC029070
00301                                                                   GC029070
00302      PERFORM 0060-SHIFT-DATES THRU 0060-EXIT                      GC029070
00303          VARYING DTE-INDEX FROM DTE-ENTRY-COUNT                   GC029070
00304          BY -1 UNTIL DTE-INDEX EQUAL ZEROS.                       GC029070
00305                                                                   GC029070
00306      MOVE GCG-EFFDT-CEN             TO DTE-EFFDT-CEN (1).         GC029070
00307      MOVE LOW-VALUES                TO DTE-PROVIDER-CNTRL (1).    GC029070
00308      MOVE GCG-FAM-REL-LVL           TO DTE-FAMILY-RELAT-LEVEL (1).GC029070
00309      MOVE GCG-TERMDT-CEN            TO DTE-TERMDT-CEN (1).        GC029070
00310                                                                   GC029070
00311      ADD 1 TO DTE-ENTRY-COUNT.                                    GC029070
00312                                                                   GC029070
00313      MOVE LOW-VALUES TO DTE-PROVIDER-CNTL-IN-USE-IND.             GC029070
00314                                                                   GC029070
00315      IF  DTE-F-R-L-IN-USE-IND  =  'Y'                             GC029070
00316          NEXT SENTENCE                                            GC029070
00317      ELSE                                                         GC029070
00318          IF  GCG-FAM-REL-LVL = '00'                               GC029070
00319              MOVE '0' TO DTE-F-R-L-IN-USE-IND                     GC029070
00320          ELSE                                                     GC029070
00321              MOVE 'Y' TO DTE-F-R-L-IN-USE-IND.                    GC029070
00322                                                                   GC029070
00323      PERFORM 0090-WRITE-DATE THRU 0090-EXIT.                      GC029070
00324                                                                   GC029070
00325  0040-EXIT.                                                       GC029070
00326      EXIT.                                                        GC029070
00327 /                                                                 GC029070
00328  0050-DATE-CHECK.                                                 GC029070
00329                                                                   GC029070
00330 ***                                                               GC029070
00331 **   ADDED 10/14/85 TO ALLOW FOR PREVIOUS GROUPS TO BE ADDED      GC029070
00332 **                                                                GC029070
00333      IF   GCG-EFFDT-CEN  LESS THAN DTE-EFFDT-CEN (H)              GC029070
00334           GO TO 0050-EXIT.                                        GC029070
00335                                                                   GC029070
00336      MOVE 'SUB'                     TO MLDATE-FUNC.               GC029070
00337      MOVE 'J'                       TO MLDATE-FORM1.              GC029070
00338      MOVE 1                         TO MLDATE-AMOUNT.             GC029070
00339      MOVE GCG-EFFDT-CEN             TO MLDATE-DATE1.              GC029070
00340      MOVE 'J'                       TO MLDATE-FORM2.              GC029070
00341      MOVE SPACES                    TO MLDATE-DATE2.              GC029070
00342      CALL 'MLDATE' USING MLDATE01.                                GC029070
00343      MOVE MLDATE-JUL2               TO JUL-DATE.                  GC029070
00344                                                                   GC029070
00345      MOVE JUL-DATE                  TO DTE-TERMDT-CEN (H).        GC029070
00346                                                                   GC029070
00347      MOVE GCG-GRP-SPECIF-ID        TO GCG2-GRP-SPECIF-ID.         GC029070
00348      MOVE DTE-EFFDT-CEN (H)        TO GCG2-EFFDT-CEN.             GC029070
00349                                                                   GC029070
00350      PERFORM 0100-READ-GRP-SPECIF THRU 0100-EXIT.                 GC029070
00351                                                                   GC029070
00352      MOVE JUL-DATE               TO GCG2-TERMDT-CEN.              GC029070
00353                                                                   GC029070
00354      PERFORM 0110-WRITE-GRP-SPECIF THRU 0110-EXIT.                GC029070
00355                                                                   GC029070
00356  0050-EXIT.                                                       GC029070
00357      EXIT.                                                        GC029070
00358 /                                                                 GC029070
00359  0060-SHIFT-DATES.                                                GC029070
00360                                                                   GC029070
00361      MOVE DTE-EFF-TERM (DTE-INDEX)                                GC029070
00362                                  TO DTE-EFF-TERM (DTE-INDEX2).    GC029070
00363                                                                   GC029070
00364      SET DTE-INDEX2 DOWN BY 1.                                    GC029070
00365                                                                   GC029070
00366  0060-EXIT.                                                       GC029070
00367      EXIT.                                                        GC029070
00368 /                                                                 GC029070
00369  0070-LOAD-DATE-KEY.                                              GC029070
00370                                                                   GC029070
00371      MOVE LOW-VALUES             TO DTE-RECORD-KEY.               GC029070
00372      MOVE GCG-PLAN-CODE          TO DTE-GROUP-PLAN-CODE.          GC029070
00373      MOVE GCG-GROUP-NUM          TO DTE-GROUP-GROUP-NUM.          GC029070
00374      MOVE GCG-SECTION-NUM        TO DTE-GROUP-SECTION-NUM.        GC029070
00375      MOVE GCG-PKG-CODE           TO DTE-GROUP-PKG-CODE.           GC029070
00376      MOVE 'G'                    TO DTE-FILE-REF-IND.             GC029070
00377                                                                   GC029070
00378  0070-EXIT.                                                       GC029070
00379      EXIT.                                                        GC029070
00380 /                                                                 GC029070
00381  0080-READ-DATE.                                                  GC029070
00382                                                                   GC029070
00383      MOVE 36                 TO TWOA-REC-LENG.                    GC029070
00384                                                                   GC029070
00385      MOVE 'R'                TO REQUEST-TYPE-2.                   GC029070
00386      CALL 'TSGVSAM2' USING PARM-TWO PARM-TWOA.                    GC029070
00387                                                                   GC029070
00388      IF  REQUEST-TYPE-2 EQUAL '3' AND                             GC029070
00389          WRK-CHANGE-REQUEST                                       GC029070
00390          MOVE 0780  TO ABEND-CODE                                 GC029070
00391          DISPLAY '*** ABEND OCCURRED ***'                         GC029070
00392          DISPLAY 'ABEND CODE IS ' ABEND-CODE                      GC029070
00393          DISPLAY '*** REQUEST WAS IGNORED AS A LOGICAL ERROR ***' GC029070
00394          PERFORM 1000-DISP-FIELDS THRU 1000-EXIT                  GC029070
00395          GO TO 9999-ERROR-RTN.                                    GC029070
00396                                                                   GC029070
00397      IF (WRK-ADD-REQUEST  OR                                      GC029070
00398          WRK-KEY-FLD-ADD-REQ )                                    GC029070
00399          GO TO 0080-EXIT.                                         GC029070
00400                                                                   GC029070
00401      IF  REQUEST-TYPE-2 NOT EQUAL 'R'                             GC029070
00402          MOVE 0781  TO ABEND-CODE                                 GC029070
00403          DISPLAY '*** ABEND OCCURRED ***'                         GC029070
00404          DISPLAY 'ABEND CODE IS ' ABEND-CODE                      GC029070
00405          DISPLAY '*** DATE FILE READ FAILED ***'                  GC029070
00406          DISPLAY 'DATE FILE REQUEST TYPE =' REQUEST-TYPE-2        GC029070
00407          PERFORM 1000-DISP-FIELDS THRU 1000-EXIT                  GC029070
00408          GO TO 9999-ERROR-RTN.                                    GC029070
00409                                                                   GC029070
00410  0080-EXIT.                                                       GC029070
00411      EXIT.                                                        GC029070
00412 /                                                                 GC029070
00413  0090-WRITE-DATE.                                                 GC029070
00414                                                                   GC029070
00415      PERFORM 200-SORT-TABULAR-ENTRIES THRU 200-EXIT.              GC029070
00416                                                                   GC029070
00417      COMPUTE TWOA-REC-LENG = 4 + GC-GCDATES-FIXED-LEN +           GC029070
00418             (DTE-ENTRY-COUNT * GC-GCDATES-VARY-LEN).              GC029070
00419                                                                   GC029070
00420      MOVE 'W'                TO REQUEST-TYPE-2.                   GC029070
00421      CALL 'TSGVSAM2' USING PARM-TWO PARM-TWOA.                    GC029070
00422      IF  REQUEST-TYPE-2 NOT EQUAL 'W'                             GC029070
00423          MOVE 0790  TO ABEND-CODE                                 GC029070
00424          DISPLAY '*** ABEND OCCURRED ***'                         GC029070
00425          DISPLAY 'ABEND CODE IS ' ABEND-CODE                      GC029070
00426          DISPLAY '*** DATE FILE WRITE FAILED ***'                 GC029070
00427          DISPLAY 'DATE FILE REQUEST TYPE =' REQUEST-TYPE-2        GC029070
00428          PERFORM 1000-DISP-FIELDS THRU 1000-EXIT                  GC029070
00429          GO TO 9999-ERROR-RTN.                                    GC029070
00430                                                                   GC029070
00431  0090-EXIT.                                                       GC029070
00432      EXIT.                                                        GC029070
00433 /                                                                 GC029070
00434  0100-READ-GRP-SPECIF.                                            GC029070
00435                                                                   GC029070
00436      MOVE 30                 TO ONEA-REC-LENG.                    GC029070
00437                                                                   GC029070
00438      MOVE 'R'                TO REQUEST-TYPE-1.                   GC029070
00439      CALL 'TSGVSAM1' USING PARM-ONE PARM-ONEA.                    GC029070
00440      IF  REQUEST-TYPE-1 NOT EQUAL 'R'                             GC029070
00441          MOVE 0710  TO ABEND-CODE                                 GC029070
00442          DISPLAY '*** ABEND OCCURRED ***'                         GC029070
00443          DISPLAY 'ABEND CODE IS ' ABEND-CODE                      GC029070
00444          DISPLAY '*** GROUP FILE READ FAILED ***'                 GC029070
00445          DISPLAY 'GROUP FILE REQUEST TYPE =' REQUEST-TYPE-1       GC029070
                                                                                
ERRMSG         DISPLAY '-------------------------------------------'            
ERRMSG         DISPLAY 'GC029070: ALERT, ALERT, ABEND 0710!!! *****'            
ERRMSG         DISPLAY ' REQ-TYPE-1 = ' REQUEST-TYPE-1                          
ERRMSG                 ' INSTEAD OF = R  (R IS EXPECTED)'                       
ERRMSG         DISPLAY '-------------------------------------------'            
ERRMSG         DISPLAY 'PROBABLE KEY-NOT-FOUND CONDITION!  '                    
ERRMSG         DISPLAY 'KEY INFO: '                                             
ERRMSG         DISPLAY '  -PLAN-CODE   = '  GCG2-PLAN-CODE                      
ERRMSG         DISPLAY '  -GROUP-NUM   = '  GCG2-GROUP-NUM                      
ERRMSG         DISPLAY '  -SECTION-NUM = '  GCG2-SECTION-NUM                    
ERRMSG         DISPLAY '  -PKG-CODE    = '  GCG2-PKG-CODE                       
ERRMSG         DISPLAY '  -FAM-REL-LVL = '  GCG2-FAM-REL-LVL                    
ERRMSG         DISPLAY '  -EFFDT-CEN   = '  GCG2-EFFDT-CEN                      
ERRMSG         DISPLAY 'DATA IN INPUT FILE GC0290A IS MISSING IN '              
ERRMSG                 'TSGVSAM1 FILE.'                                         
ERRMSG         DISPLAY 'CHECK WITH BENEFIT CODING, AND SEE IF THEY '            
ERRMSG                 'CAN FIX/DELETE DATA.'                                   
ERRMSG         DISPLAY 'PGM=GC029070, CHECK TSGVSAM1 FILE. '                    
ERRMSG         DISPLAY '-------------------------------------------'            
                                                                                
00446          PERFORM 1000-DISP-FIELDS THRU 1000-EXIT                  GC029070
00447          GO TO 9999-ERROR-RTN.                                    GC029070
00448                                                                   GC029070
00449  0100-EXIT.                                                       GC029070
00450      EXIT.                                                        GC029070
00451 ******************************************************************GC029070
00452 * 115-CHECK FOR DUPLICATE ENTRY.  RGO 8/96                        GC029070
00453 ******************************************************************GC029070
00454  115-CHECK-DUP-ENTRY.                                             GC029070
00455                                                                   GC029070
00456       IF  DTE-EFF-TERM (SUBX) = HIGH-VALUES                       GC029070
00457           MOVE  0700           TO ABEND-CODE                      GC029070
00458           DISPLAY '*** ABEND OCCURRED ***'                        GC029070
00459           DISPLAY 'ABEND CODE IS ' ABEND-CODE                     GC029070
00460           DISPLAY '** DATE FILE OCCURRENCE EQUALS HIGH VALUES **' GC029070
00461           DISPLAY '*****        DATE RECORD IN ERROR       *****' GC029070
00462           DISPLAY 'OCCURRENCE   =' SUBX                           GC029070
00463           PERFORM 1000-DISP-FIELDS THRU 1000-EXIT                 GC029070
00464           GO TO 9999-ERROR-RTN.                                   GC029070
00465                                                                   GC029070
00466       IF  GCG-EFFDT-CEN        =    DTE-EFFDT-CEN (SUBX)       ANDGC029070
00467           GCG-FAM-REL-LVL      =    DTE-FAMILY-RELAT-LEVEL (SUBX) GC029070
00468           MOVE 'Y' TO SAME-DATE-FOUND                             GC029070
00469           PERFORM 120-DISPLAY-INFO                                GC029070
00470       END-IF.                                                     GC029070
00471                                                                   GC029070
00472  120-DISPLAY-INFO.                                                GC029070
00473      DISPLAY '*E* ERROR DETECTED, GROUP SPECIFIC PRGRAM GC029070'.GC029070
00474      DISPLAY '*E* ATTEMPTING TO ADD AN OCCURS TO THE DATE FILE.'. GC029070
00475      DISPLAY '*E* KEY INFO FROM WRK-REC FOLLOWS:'.                GC029070
00476      DISPLAY '*E* PLAN CODE: ' WRK-PLAN-CODE                      GC029070
00477              ' GROUP: ' WRK-GROUP-NUM ' SECT: ' WRK-SECTION-NUM.  GC029070
00478      DISPLAY '*E* PACKAGE CODE: ' WRK-PKG-CODE.                   GC029070
00479                                                                   GC029070
00480      DISPLAY '*E* EFFECTIVE DATE: ' WRK-EFFDT-CEN.                GC029070
00481      DISPLAY '*E* FRL: ' WRK-FAM-REL-LEVEL.                       GC029070
00482                                                                   GC029070
00483      DISPLAY '*E* SIGNAL FROM ONLINE = ' WRK-SIGNAL-FROM-ONLINE.  GC029070
00484      DISPLAY '*E* OPERATOR-ID = ' WRK-OPERATOR-ID.                GC029070
00485                                                                   GC029070
00486 /                                                                 GC029070
00487  0110-WRITE-GRP-SPECIF.                                           GC029070
00488                                                                   GC029070
00489      MOVE 'U'                TO REQUEST-TYPE-1.                   GC029070
00490      CALL 'TSGVSAM1' USING PARM-ONE PARM-ONEA.                    GC029070
00491      IF  REQUEST-TYPE-1 NOT EQUAL 'U'                             GC029070
00492          MOVE 0711  TO ABEND-CODE                                 GC029070
00493          DISPLAY '*** ABEND OCCURRED ***'                         GC029070
00494          DISPLAY 'ABEND CODE IS ' ABEND-CODE                      GC029070
00495          DISPLAY '*** GROUP UPDATE FAILED ***'                    GC029070
00496          DISPLAY 'PLAN CODE            =' GCG-PLAN-CODE           GC029070
00497          DISPLAY 'GROUP NUMBER         =' GCG-GROUP-NUM           GC029070
00498          DISPLAY 'SECTION NUMBER       =' GCG-SECTION-NUM         GC029070
00499          DISPLAY 'PACKAGE CODE         =' GCG-PKG-CODE            GC029070
00500          DISPLAY 'FAMILY RELATION      =' GCG-FAM-REL-LVL         GC029070
00501          DISPLAY 'EFFECTIVE DATE       =' GCG-EFFDT-CEN           GC029070
00502          DISPLAY 'REQUEST TYPE         =' REQUEST-TYPE-1          GC029070
00503          PERFORM 1000-DISP-FIELDS THRU 1000-EXIT                  GC029070
00504          GO TO 9999-ERROR-RTN.                                    GC029070
00505                                                                   GC029070
00506  0110-EXIT.                                                       GC029070
00507      EXIT.                                                        GC029070
00508 /                                                                 GC029070
00509  0150-DET-CORRECT-OCCUR.                                          GC029070
00510                                                                   GC029070
00511      IF  DTE-EFF-TERM (H) = HIGH-VALUES                           GC029070
00512          MOVE 1551 TO ABEND-CODE                                  GC029070
00513          DISPLAY '*** ABEND OCCURRED ***'                         GC029070
00514          DISPLAY 'ABEND CODE IS ' ABEND-CODE                      GC029070
00515          DISPLAY '*** DATE FILE OCCURRENCE NOT FOUND ***'         GC029070
00516          PERFORM 1000-DISP-FIELDS THRU 1000-EXIT                  GC029070
00517          DISPLAY 'EFFECTIVE DATE         =' GCG-EFFDT-CEN         GC029070
00518          DISPLAY 'FAMILY RELATION LEVEL  =' GCG-FAM-REL-LVL       GC029070
00519          GO TO 9999-ERROR-RTN.                                    GC029070
00520                                                                   GC029070
00521      IF  GCG-EFFDT-CEN             =  DTE-EFFDT-CEN (H)  AND      GC029070
00522          GCG-FAM-REL-LVL           =  DTE-FAMILY-RELAT-LEVEL (H)  GC029070
00523          GO TO 0150-EXIT.                                         GC029070
00524                                                                   GC029070
00525       ADD 1 TO H.                                                 GC029070
00526       GO TO 0150-DET-CORRECT-OCCUR.                               GC029070
00527  0150-EXIT.                                                       GC029070
00528      EXIT.                                                        GC029070
00529  0160-FIND-OCCUR.                                                 GC029070
00530                                                                   GC029070
00531      MOVE 'NO ' TO WAS-OCCURANCE-FOUND.                           GC029070
00532                                                                   GC029070
00533      IF  GCG-FAM-REL-LVL =  DTE-FAMILY-RELAT-LEVEL (H)            GC029070
00534          MOVE 'YES' TO WAS-OCCURANCE-FOUND                        GC029070
00535          GO TO 0160-EXIT.                                         GC029070
00536                                                                   GC029070
00537      IF  DTE-EFF-TERM (H) = HIGH-VALUES                           GC029070
00538          GO TO 0160-EXIT.                                         GC029070
00539                                                                   GC029070
00540       ADD 1 TO H.                                                 GC029070
00541       GO TO 0160-FIND-OCCUR.                                      GC029070
00542  0160-EXIT.                                                       GC029070
00543      EXIT.                                                        GC029070
00544 /                                                                 GC029070
00545  200-SORT-TABULAR-ENTRIES.                                        GC029070
00546                                                                   GC029070
00547      MOVE  DTE-ENTRY-COUNT    TO  TAB-ENTRY-COUNT.                GC029070
00548      MOVE  DTE-EFF-TERM-AREA  TO  TAB-WORK-AREA-ALL-TERM.         GC029070
00549                                                                   GC029070
00550      COMPUTE  SORT-VALUE  = DTE-ENTRY-COUNT - 2.                  GC029070
00551      COMPUTE  CHECK-VALUE = SORT-VALUE + 1.                       GC029070
00552                                                                   GC029070
00553      PERFORM  230-CHECK-MINOR-VALUE  THRU 230-EXIT                GC029070
00554               VARYING I FROM 1 BY 1 UNTIL                         GC029070
00555               I IS GREATER THAN SORT-VALUE.                       GC029070
00556                                                                   GC029070
00557      PERFORM  270-CHECK-MAJOR-VALUE  THRU 270-EXIT                GC029070
00558               VARYING I FROM 1 BY 1 UNTIL                         GC029070
00559               I IS GREATER THAN SORT-VALUE.                       GC029070
00560                                                                   GC029070
00561      MOVE  TAB-ENTRY-COUNT          TO  DTE-ENTRY-COUNT.          GC029070
00562      MOVE  TAB-WORK-AREA-ALL-TERM   TO  DTE-EFF-TERM-AREA.        GC029070
00563                                                                   GC029070
00564  200-EXIT.                                                        GC029070
00565      EXIT.                                                        GC029070
00566 /                                                                 GC029070
00567  230-CHECK-MINOR-VALUE.                                           GC029070
00568                                                                   GC029070
00569      COMPUTE K = I + 1.                                           GC029070
00570                                                                   GC029070
00571      PERFORM  240-MINOR-SORT-RTN  THRU 240-EXIT                   GC029070
00572               VARYING  J  FROM  K BY  1 UNTIL                     GC029070
00573               J IS GREATER THAN CHECK-VALUE.                      GC029070
00574  230-EXIT.                                                        GC029070
00575      EXIT.                                                        GC029070
00576  240-MINOR-SORT-RTN.                                              GC029070
00577      IF  TAB-REST (I) GREATER THAN TAB-REST (J)                   GC029070
00578          MOVE  TAB-WRK-TERM (I) TO  TAB-WORK-AREA                 GC029070
00579          MOVE  TAB-WRK-TERM (J) TO  TAB-WRK-TERM (I)              GC029070
00580          MOVE  TAB-WORK-AREA    TO  TAB-WRK-TERM (J).             GC029070
00581  240-EXIT.                                                        GC029070
00582      EXIT.                                                        GC029070
00583 /                                                                 GC029070
00584  270-CHECK-MAJOR-VALUE.                                           GC029070
00585                                                                   GC029070
00586      COMPUTE K = I + 1.                                           GC029070
00587                                                                   GC029070
00588      PERFORM  280-MAJOR-SORT-RTN  THRU 280-EXIT                   GC029070
00589               VARYING  J  FROM  K BY  1 UNTIL                     GC029070
00590               J IS GREATER THAN CHECK-VALUE.                      GC029070
00591  270-EXIT.                                                        GC029070
00592      EXIT.                                                        GC029070
00593  280-MAJOR-SORT-RTN.                                              GC029070
00594      IF  TAB-EFFDT-CEN (I) LESS THAN TAB-EFFDT-CEN (J)            GC029070
00595          MOVE  TAB-WRK-TERM (I) TO  TAB-WORK-AREA                 GC029070
00596          MOVE  TAB-WRK-TERM (J) TO  TAB-WRK-TERM (I)              GC029070
00597          MOVE  TAB-WORK-AREA    TO  TAB-WRK-TERM (J).             GC029070
00598  280-EXIT.                                                        GC029070
00599      EXIT.                                                        GC029070
00600 /                                                                 GC029070
00601  0900-OPEN-FILES.                                                 GC029070
00602                                                                   GC029070
00603      MOVE 'S'                TO REQUEST-TYPE-2.                   GC029070
00604      MOVE 8                  TO SET-REC-LENG.                     GC029070
00605      MOVE 3                  TO SET-VALUE.                        GC029070
00606      CALL 'TSGVSAM2' USING PARM-TWO PARM-SET.                     GC029070
00607      IF  REQUEST-TYPE-2 NOT EQUAL 'S'                             GC029070
00608          MOVE 0901   TO ABEND-CODE                                GC029070
00609          DISPLAY '*** ABEND OCCURRED ***'                         GC029070
00610          DISPLAY 'ABEND CODE IS ' ABEND-CODE                      GC029070
00611          DISPLAY '*** SET FAILED FOR DATE FILE ***'               GC029070
00612          DISPLAY 'REQUEST TYPE    =' REQUEST-TYPE-2               GC029070
00613          GO TO 9999-ERROR-RTN.                                    GC029070
00614                                                                   GC029070
00615      MOVE 'O'                TO REQUEST-TYPE-2.                   GC029070
00616      CALL 'TSGVSAM2' USING PARM-TWO PARM-TWOA.                    GC029070
00617      IF  REQUEST-TYPE-2 NOT EQUAL 'O'                             GC029070
00618          MOVE 0902  TO ABEND-CODE                                 GC029070
00619          DISPLAY '*** ABEND OCCURRED ***'                         GC029070
00620          DISPLAY 'ABEND CODE IS ' ABEND-CODE                      GC029070
00621          DISPLAY '*** OPEN FAILED FOR DATE FILE ***'              GC029070
00622          DISPLAY 'REQUEST TYPE    =' REQUEST-TYPE-2               GC029070
00623          GO TO 9999-ERROR-RTN.                                    GC029070
00624                                                                   GC029070
00625  0900-EXIT.                                                       GC029070
00626      EXIT.                                                        GC029070
00627 /                                                                 GC029070
00628  0910-CLOSE-FILES.                                                GC029070
00629                                                                   GC029070
00630      MOVE 'C'                TO REQUEST-TYPE-2.                   GC029070
00631      CALL 'TSGVSAM2' USING PARM-TWO PARM-TWOA.                    GC029070
00632      IF  REQUEST-TYPE-2 NOT EQUAL 'C'                             GC029070
00633          MOVE 0911  TO ABEND-CODE                                 GC029070
00634          DISPLAY '*** ABEND OCCURRED ***'                         GC029070
00635          DISPLAY 'ABEND CODE IS ' ABEND-CODE                      GC029070
00636          DISPLAY '*** CLOSE FAILED FOR DATE FILE ***'             GC029070
00637          DISPLAY 'REQUEST TYPE    =' REQUEST-TYPE-2               GC029070
00638          GO TO 9999-ERROR-RTN.                                    GC029070
00639                                                                   GC029070
00640  0910-EXIT.                                                       GC029070
00641      EXIT.                                                        GC029070
00642 /                                                                 GC029070
00643  1000-DISP-FIELDS.                                                GC029070
00644      DISPLAY 'GROUP PLAN CODE        ='   DTE-GROUP-PLAN-CODE.    GC029070
00645      DISPLAY 'GROUP NUMBER           ='   DTE-GROUP-GROUP-NUM.    GC029070
00646      DISPLAY 'GROUP SECTION NUMBER   ='   DTE-GROUP-SECTION-NUM.  GC029070
00647      DISPLAY 'GROUP PACKAGE CODE     ='   DTE-GROUP-PKG-CODE.     GC029070
00648      DISPLAY 'FILE REFERENCE IND =' DTE-FILE-REF-IND.             GC029070
00649      DISPLAY '*** YOU ARE IN PROGRAM GC029070 ***'.               GC029070
00650  1000-EXIT.                                                       GC029070
00651      EXIT.                                                        GC029070
00652  9999-ERROR-RTN.                                                  GC029070
00653                                                                   GC029070
00654      CALL 'TSGEND' USING ABEND-CODE.                              GC029070
00655                                                                   GC029070
00656  9999-EXIT.                                                       GC029070
00657      EXIT.                                                        GC029070
