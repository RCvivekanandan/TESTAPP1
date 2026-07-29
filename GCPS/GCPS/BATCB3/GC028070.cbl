00001  IDENTIFICATION DIVISION.                                         12/09/02
00002  PROGRAM-ID.  GC028070.                                           GC028070
00003  AUTHOR. ROBERT MANN - DECISION CONSULTANTS INC.                     LV002
00004  INSTALLATION.  HCSC.                                             GC028070
00005  DATE-WRITTEN.  JUL 11,1984.                                      GC028070
00006  DATE-COMPILED.                                                   GC028070
00007 ******************************************************************GC028070
00008 *                                                                 GC028070
00009 *    THIS PROGRAM UPDATES THE ONLINE CONTRACT FILE AND            GC028070
00010 *    AND THE ONLINE DATE FILE.                                    GC028070
00011 *                                                                 GC028070
00012 *    THE MAIN FUNCTION IS TO LOAD POLICY EFFECTIVE AND            GC028070
00013 *    TERMINATION DATES ON THE THE DATE FILE AND                   GC028070
00014 *    POLICY TERMINATION DATES ON THE CONTRACT FILE.               GC028070
00015 *                                                                 GC028070
00016 *    THIS PROGRAM MODULE CALLED BY PROGRAM GC0280                 GC028070
00017 *    AND IS NOT TO BE EXECUTED AS A STAND ALONE PROGRAM.          GC028070
00018 *                                                                 GC028070
00019 *    TSGVSAM1 IS THE ONLINE CONTRACT FILE - (I/O).                GC028070
00020 *    TSGVSAM2 IS THE DATE FILE - (I/O).                           GC028070
00021 *                                                                 GC028070
00022 ******************************************************************GC028070
00023 /                                                                 GC028070
00024 ******************************************************************GC028070
00025 *            A B E N D    C O D E S                              *GC028070
00026 ******************************************************************GC028070
00027 * THE ABEND CODE CANNOT BE GREATER THAN 4095!!!                  *GC028070
00028 * ALL CODES HAVE BEEN CHANGED. RGO 8/96.                         *GC028070
00029 * CODE IS IN FORMAT 0YXX, WHERE                                  *GC028070
00030 * Y = MODULE NUMBER   (07)                                       *GC028070
00031 * Y = PARAGRAPH NUMBER                                         *  GC028070
00032 * EX  0780    MODULE IS 28070 PARAGRAPH = 0080                   *GC028070
00033 *                                                                *GC028070
00034 *                                                                *GC028070
00035 * CODE       REASON FOR ABEND                                    *GC028070
00036 * ----       ------------------------------------------------    *GC028070
00037 * 0780       READ ERROR DATE FILE - THE INPUT RECORD (CONTRACT)  *GC028070
00038 *            WAS MARKED FOR ADD, BUT NO DATE RECORD WAS FOUND.   *GC028070
00039 *                                                                *GC028070
00040 * 0781       READ ERROR DATE FILE - NO DATE RECORD WAS FOUND     *GC028070
00041 *            ON THE DATE FILE.                                   *GC028070
00042 *                                                                *GC028070
00043 * 0790       WRITE ERROR DATE FILE - DATE RECORD WAS NOT WRITTEN *GC028070
00044 *            CORRECTLY TO THE DATE FILE.                         *GC028070
00045 *                                                                *GC028070
00046 * 0700       DATE FILE UPDATE ERROR - THE OCCURANCE ON THE DATE  *GC028070
00047 *            RECORD TO BE UPDATED DID NOT EXIST.                 *GC028070
00048 *                                                                *GC028070
00049 * 0740       READ ERROR CONTRACT FILE - THE CONTRACT RECORD TO   *GC028070
00050 *            BE UPDATED WAS NOT FOUND (KEYS FOR THIS RECORD      *GC028070
00051 *            CAME FROM THE DATE RECORD).                         *GC028070
00052 *                                                                *GC028070
00053 * 0750       UPDATE ERROR CONTRACT FILE - THE CONTRACT RECORD TO *GC028070
00054 *            BE UPDATED WAS NOT UPDATED CORRECTLY.               *GC028070
00055 *                                                                *GC028070
00056 * 0700       DATE FILE OPEN ERROR FOR TSGVSAM SET.               *GC028070
00057 *                                                                *GC028070
00058 * 0701       DATE FILE OPEN ERROR FOR TSGVSAM OPEN.              *GC028070
00059 *                                                                *GC028070
00060 * 0710       DATE FILE CLOSE ERROR FOR TSGVSAM CLOSE.            *GC028070
00061 *                                                                *GC028070
00062 *                                                                *GC028070
00063 ******************************************************************GC028070
00064 /                                                                 GC028070
00065 ******************************************************************GC028070
00066 *                  UPDATE HISTORY                                *GC028070
00067 ******************************************************************GC028070
00068 *  DATE       AUTHOR  REASON                                     *GC028070
00069 ******************************************************************GC028070
00070 *  03/11/87    RKH    REVISED LOGIC FOR KEY-FIELD ADDED RECORDS. *GC028070
00071 *                     REVISED AND ADDED ERROR LOGIC TABLE.       *GC028070
00072 *                     REMOVED DISPLAYS AND READY TRACE.  .       *GC028070
00073 *                                                                *GC028070
00074 *  02/04/87    RKH    ADDED LOGIC FOR KEY-FIELD ADDED RECORDS.   *GC028070
00075 *                     THIS ALLOWS THE DATE RECORD TO HAVE THIS   *GC028070
00076 *                     NEW OCCURANCE ADDED ONLY / AND THE OLD     *GC028070
00077 *                     OCCURANCE ON THE DATE REC IS NOT MODIFIED. *GC028070
00078 *                                                                *GC028070
00079 *  MARCH 85           NEW DATE FILE ADDED                        *GC028070
00080 *                                                                *GC028070
00081 *  09/10/91    GDM    INCREASE TAB-FAM-RELAT TO 2 POSITIONS      *GC028070
00082 *                                                                *GC028070
00083 *  09/19/91    TPM    EXPANSION OF THE FAMILY-RELATION FIELD.    *GC028070
00084 *  D12009             REDUCE THE NUMBER OF OCCURS IN THE TABLE-  *GC028070
00085 *                     HOLD-AREA FROM 400 TO 396 TO ACCOMODATE    *GC028070
00086 *                     FOR THE ABOVE CHANGE.                      *GC028070
00087 *                     REMOVE HARD CODED RECORD LENGTHS FOR       *GC028070
00088 *                     GCDATES AND INSERTED COPYBOOK MEMBER       *GC028070
00089 *                     GCCDRLEN  TO BE USED TO INCLUDE NEW RECORD *GC028070
00090 *                     LENGTHS TO HANDLE THE EXPANSION IN THE     *GC028070
00091 *                     FAMILY RELATION FIELD.                     *GC028070
00092 *                                                                *GC028070
00093 *                     CHANGED THE RECORD LENGTH FROM 21 TO 22    *GC028070
00094 *                     WHEN CALLING THE TSGVSAM ROUTINE.          *GC028070
00095 *                                                                *GC028070
00096 * D12009   10-17-91  FRY  MODIFIED IN WORKING-STORAGE:           *GC028070
00097 *                          FROM:  05  TAB-WORK-AREA   PIC X(9)   *GC028070
00098 *                            TO:  05  TAB-WORK-AREA   PIC X(10)  *GC028070
00099 *                                                                *GC028070
00100 *          01-26-95  GDM  CONVERTED TO COBOL II                  *GC028070
00101 *                                                                *GC028070
00102 * 07/11/96  RGO     ADDED CODE TO DETECT AND PREVENT ADDING A    *GC028070
00103 *                   DATE RECORD WITH THE SAME EFFECTIVE DATE AS  *GC028070
00104 *                   ANOTHER ANOTHER RECORD.                      *GC028070
00105 *                   WHEN THIS IS FOUND (WHICH SHOULD BE VERY     *GC028070
00106 *                   RARELY) DISPLAY INFO FOR DEBUGGING, SET      *GC028070
00107 *                   THE RETURN CODE TO 'E'.                      *GC028070
00108 *                   THE MAIN PROGRAM WILL THEN ABEND AFTER       *GC028070
00109 *                   IT FINISHES PROCESSING.                      *GC028070
00110 *                                                                *GC028070
00111 * 14726/15057                                                    *GC028070
00112 *         09/15/97 DAU  ADDED CODE TO SUPPORT THE YEAR 2000 AND  *GC028070
00113 *                       THE EXPANSION OF THE GROUP SPECIFIC AND  *GC028070
00114 *                       CONTRACT KEY TO SUPPORT THE TEXAS MERGER.*GC028070
00115 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GC028070
00116 *                                                                *GC028070
00117 ******************************************************************GC028070
00118  ENVIRONMENT DIVISION.                                            GC028070
00119  CONFIGURATION SECTION.                                           GC028070
00120  SOURCE-COMPUTER. IBM-370.                                        GC028070
00121  OBJECT-COMPUTER. IBM-370.                                        GC028070
00122  INPUT-OUTPUT SECTION.                                            GC028070
00123  FILE-CONTROL.                                                    GC028070
00124  DATA DIVISION.                                                   GC028070
00125  FILE SECTION.                                                    GC028070
00126 /                                                                 GC028070
00127  WORKING-STORAGE SECTION.                                         GC028070
00128                                                                   GC028070
00129  01  FILLER            PIC X(24)                                  GC028070
00130               VALUE 'GC028070 WORKING STORAGE'.                   GC028070
00131                                                                   GC028070
00132  01  ABEND-CODE        PIC 9(4)  COMP.                            GC028070
00133 /                                                                 GC028070
00134  01  WS-RECORD-LENGTHS.                                           GC028070
00135      COPY GCCDRLEN.                                               GC028070
00136                                                                   GC028070
00137 /                                                                 GC028070
00138  01  DATE-WORK-AREA.                                              GC028070
00139      05  JUL-DATE            PIC 9(7).                            GC028070
00140                                                                   GC028070
00141      COPY MLDATE01.                                               GC028070
00142 /                                                                 GC028070
00143  01  PARM-SET.                                                    GC028070
00144      05  SET-RDW.                                                 GC028070
00145          10  SET-REC-LENG    PIC 9(4)    VALUE ZEROS     COMP.    GC028070
00146          10  SET-FEEDBACK    PIC 9(4)    VALUE ZEROS     COMP.    GC028070
00147      05  SET-VALUE           PIC 9(8)                    COMP.    GC028070
00148                                                                   GC028070
00149  01  PARM-ONE.                                                    GC028070
00150      05  RESERVED-FLDS-1     PIC 9(8)    VALUE ZEROS     COMP.    GC028070
00151      05  RESERVED-X-1 REDEFINES RESERVED-FLDS-1.                  GC028070
00152          10  REQUEST-TYPE-1  PIC X.                               GC028070
00153          10  FILLER          PIC X(3).                            GC028070
00154                                                                   GC028070
00155  01  PARM-ONEA.                                                   GC028070
00156      02  ONEA-RDW.                                                GC028070
00157          05  ONEA-REC-LENG   PIC 9(4)    VALUE ZEROS     COMP.    GC028070
00158          05  ONEA-FEEDBACK   PIC 9(4)    VALUE ZEROS     COMP.    GC028070
00159      02  ONEA-REC-AREA.                                           GC028070
00160          COPY GCCONTR2.                                           GC028070
00161 /                                                                 GC028070
00162  01  PARM-TWO.                                                    GC028070
00163      05  RESERVED-FLDS-2     PIC 9(8)    VALUE ZEROS     COMP.    GC028070
00164      05  RESERVED-X-2 REDEFINES RESERVED-FLDS-2.                  GC028070
00165          10  REQUEST-TYPE-2  PIC X.                               GC028070
00166          10  FILLER          PIC X(3).                            GC028070
00167                                                                   GC028070
00168  01  PARM-TWOA.                                                   GC028070
00169      02  TWOA-RDW.                                                GC028070
00170          05  TWOA-REC-LENG   PIC 9(4)    VALUE ZEROS     COMP.    GC028070
00171          05  TWOA-FEEDBACK   PIC 9(4)    VALUE ZEROS     COMP.    GC028070
00172      02  TWOA-REC-AREA.                                           GC028070
00173      COPY GCDATEC5.                                               GC028070
00174 /                                                                 GC028070
00175  01  SWITCH-AREA.                                                 GC028070
00176      05  OCCURANCE-WAS-FOUND   PIC XXX   VALUE 'NO '.             GC028070
00177          88  OCCURANCE-FOUND             VALUE 'YES'.             GC028070
00178  01  SUBSCRIPT-AREA.                                              GC028070
00179      05  CHECK-VALUE         PIC S9(4)   VALUE ZERO.              GC028070
00180      05  SORT-VALUE          PIC S9(4)   VALUE ZERO.              GC028070
00181      05  H                   PIC S9(4)   VALUE ZERO.              GC028070
00182      05  I                   PIC S9(4)   VALUE ZERO.              GC028070
00183      05  J                   PIC S9(4)   VALUE ZERO.              GC028070
00184      05  K                   PIC S9(4)   VALUE ZERO.              GC028070
00185      05  SUB1                PIC S9(4)   VALUE ZERO.              GC028070
00186      05  SUBX                PIC S9(5)   VALUE ZERO.              GC028070
00187                                                                   GC028070
00188  01  TABLES-HOLD-AREA.                                            GC028070
00189      05  TAB-WORK-AREA       PIC X(12)   VALUE SPACES.            GC028070
00190      05  TAB-ENTRY-COUNT     PIC 999     VALUE 1.                 GC028070
00191      05  TAB-WORK-AREA-ALL-TERM.                                  GC028070
00192          10  TAB-WRK-TERM    OCCURS 1 TO 410 TIMES                GC028070
00193              DEPENDING ON TAB-ENTRY-COUNT.                        GC028070
00194              15  TAB-EFFECTIVE-DATE.                              GC028070
00195                  20  TAB-EFFDT-CC            PIC X(1).            GC028070
00196                  20  TAB-EFF         COMP-3  PIC S9(5).           GC028070
00197              15  TAB-EFFDT-CEN REDEFINES                          GC028070
00198                  TAB-EFFECTIVE-DATE  COMP-3  PIC S9(7).           GC028070
00199              15  TAB-REST.                                        GC028070
00200                  20  TAB-PROVIDER            PIC XX.              GC028070
00201                  20  TAB-FAMILY-RELAT        PIC XX.              GC028070
00202              15  TAB-TERM-DATE.                                   GC028070
00203                  20  TAB-TERMDT-CC           PIC X(1).            GC028070
00204                  20  TAB-TERM        COMP-3  PIC S9(5).           GC028070
00205              15  TAB-TERMDT-CEN REDEFINES                         GC028070
00206                  TAB-TERM-DATE       COMP-3  PIC S9(7).           GC028070
00207  01  WS-AREA.                                                     GC028070
00208      05  SAME-DATE-FOUND     PIC X(1)    VALUE SPACES.            GC028070
00209      05  REAL-COUNT          PIC S9(5)  COMP-3 VALUE ZEROS.       GC028070
00210 /                                                                 GC028070
00211  LINKAGE SECTION.                                                 GC028070
00212                                                                   GC028070
00213  01  GC028070-IND        PIC X.                                   GC028070
00214                                                                   GC028070
00215  01  RLSE-CON-REC.                                                GC028070
00216      COPY GCWRKDCC.                                               GC028070
00217      COPY GCCONTRC.                                               GC028070
00218 /                                                                 GC028070
00219  PROCEDURE DIVISION USING GC028070-IND RLSE-CON-REC.              GC028070
00220                                                                   GC028070
00221  0000-MAINLINE.                                                   GC028070
00222                                                                   GC028070
00223      IF  GC028070-IND EQUAL 'O'                                   GC028070
00224          MOVE SPACES             TO GC028070-IND                  GC028070
00225          PERFORM 0900-OPEN-FILES THRU 0900-EXIT.                  GC028070
00226                                                                   GC028070
00227      IF  GC028070-IND EQUAL SPACES                                GC028070
00228          PERFORM 0010-PROCESS THRU 0010-EXIT.                     GC028070
00229                                                                   GC028070
00230      IF  GC028070-IND EQUAL 'C'                                   GC028070
00231          PERFORM 0910-CLOSE-FILES THRU 0910-EXIT.                 GC028070
00232                                                                   GC028070
00233      GOBACK.                                                      GC028070
00234                                                                   GC028070
00235  0000-EXIT.                                                       GC028070
00236      EXIT.                                                        GC028070
00237 /                                                                 GC028070
00238  0010-PROCESS.                                                    GC028070
00239                                                                   GC028070
00240      PERFORM VARYING SUB1 FROM 1 BY 1                             GC028070
00241         UNTIL SUB1 > 410                                          GC028070
00242          INITIALIZE TAB-WRK-TERM (SUB1)                           GC028070
00243      END-PERFORM.                                                 GC028070
00244                                                                   GC028070
00245      IF  WRK-CHANGE-REQUEST                                       GC028070
00246          PERFORM 0020-PROCESS-CHANGE-REQ THRU 0020-EXIT           GC028070
00247          GO TO 0010-EXIT.                                         GC028070
00248                                                                   GC028070
00249      IF (WRK-ADD-REQUEST  OR                                      GC028070
00250          WRK-KEY-FLD-ADD-REQ )                                    GC028070
00251          PERFORM 0030-PROCESS-ADD-REQ THRU 0030-EXIT              GC028070
00252          GO TO 0010-EXIT.                                         GC028070
00253                                                                   GC028070
00254  0010-EXIT.                                                       GC028070
00255      EXIT.                                                        GC028070
00256 /                                                                 GC028070
00257  0020-PROCESS-CHANGE-REQ.                                         GC028070
00258                                                                   GC028070
00259      PERFORM 0070-LOAD-DATE-KEY THRU 0070-EXIT.                   GC028070
00260                                                                   GC028070
00261      PERFORM 0080-READ-DATE THRU 0080-EXIT.                       GC028070
00262                                                                   GC028070
00263      MOVE 1 TO H.                                                 GC028070
00264      PERFORM 0100-DET-CORRECT-ADD-OCCUR THRU                      GC028070
00265              0100-EXIT.                                           GC028070
00266                                                                   GC028070
00267      MOVE GCT-TERMDT-CEN     TO DTE-TERMDT-CEN (H).               GC028070
00268                                                                   GC028070
00269      PERFORM 0090-WRITE-DATE THRU 0090-EXIT.                      GC028070
00270                                                                   GC028070
00271  0020-EXIT.                                                       GC028070
00272      EXIT.                                                        GC028070
00273 /                                                                 GC028070
00274  0030-PROCESS-ADD-REQ.                                            GC028070
00275                                                                   GC028070
00276      PERFORM 0070-LOAD-DATE-KEY THRU 0070-EXIT.                   GC028070
00277                                                                   GC028070
00278      PERFORM 0080-READ-DATE THRU 0080-EXIT.                       GC028070
00279                                                                   GC028070
00280      IF  REQUEST-TYPE-2 EQUAL '3'                                 GC028070
00281          PERFORM 0035-ADD-NEW-RECORD  THRU                        GC028070
00282                  0035-EXIT                                        GC028070
00283          GO TO 0030-EXIT.                                         GC028070
00284                                                                   GC028070
00285      IF  WRK-ADD-REQUEST                                          GC028070
00286          PERFORM 0040-UPDATE-DATE-REC-OCCURS THRU                 GC028070
00287                  0040-EXIT                                        GC028070
00288          GO TO 0030-EXIT.                                         GC028070
00289                                                                   GC028070
00290      IF  WRK-KEY-FLD-ADD-REQ                                      GC028070
00291          PERFORM 0040-ADD-DATE-REC-OCCURS THRU                    GC028070
00292                  0040-EXIT                                        GC028070
00293          GO TO 0030-EXIT.                                         GC028070
00294                                                                   GC028070
00295  0030-EXIT.     EXIT.                                             GC028070
00296                                                                   GC028070
00297  0035-ADD-NEW-RECORD.                                             GC028070
00298      MOVE GC-GCDATES-VARY-MAX-OCUR                                GC028070
00299                                  TO DTE-ENTRY-COUNT.              GC028070
00300      MOVE LOW-VALUES             TO DTE-RECORD.                   GC028070
00301      PERFORM VARYING SUB1 FROM 1 BY 1                             GC028070
00302         UNTIL SUB1 > 410                                          GC028070
00303          INITIALIZE DTE-EFF-TERM (SUB1)                           GC028070
00304      END-PERFORM.                                                 GC028070
00305      MOVE 2                      TO DTE-ENTRY-COUNT.              GC028070
00306 *                                                                 GC028070
00307      MOVE GCT-EFFDT-CEN          TO DTE-EFFDT-CEN (1).            GC028070
00308      MOVE GCT-PROVDR-CONTROL     TO DTE-PROVIDER-CNTRL (1).       GC028070
00309      MOVE GCT-FAM-REL-LVL        TO DTE-FAMILY-RELAT-LEVEL (1).   GC028070
00310      MOVE GCT-TERMDT-CEN         TO DTE-TERMDT-CEN (1).           GC028070
00311 *                                                                 GC028070
00312      MOVE HIGH-VALUES            TO DTE-EFF-TERM (2).             GC028070
00313                                                                   GC028070
00314      MOVE LOW-VALUES             TO DTE-RECORD-KEY.               GC028070
00315      MOVE GCT-PLAN-CODE          TO DTE-CONTR-PLAN-CODE.          GC028070
00316      MOVE GCT-GROUP-NUM          TO DTE-CONTR-GROUP-NUM.          GC028070
00317      MOVE GCT-SECTION-NUM        TO DTE-CONTR-SECTION-NUM.        GC028070
00318      MOVE GCT-PKG-CODE           TO DTE-CONTR-PKG-CODE.           GC028070
00319      MOVE GCT-L-O-B              TO DTE-CONTR-L-O-B.              GC028070
00320      MOVE 'C'                    TO DTE-FILE-REF-IND.             GC028070
00321                                                                   GC028070
00322      IF  GCT-PROVDR-CONTROL = '00'                                GC028070
00323          MOVE '0'  TO  DTE-PROVIDER-CNTL-IN-USE-IND               GC028070
00324      ELSE                                                         GC028070
00325          MOVE 'Y'  TO  DTE-PROVIDER-CNTL-IN-USE-IND.              GC028070
00326                                                                   GC028070
00327      IF  GCT-FAM-REL-LVL    = '00'                                GC028070
00328          MOVE '0'  TO  DTE-F-R-L-IN-USE-IND                       GC028070
00329      ELSE                                                         GC028070
00330          MOVE 'Y'  TO  DTE-F-R-L-IN-USE-IND.                      GC028070
00331                                                                   GC028070
00332      PERFORM 0090-WRITE-DATE THRU 0090-EXIT.                      GC028070
00333                                                                   GC028070
00334  0035-EXIT.      EXIT.                                            GC028070
00335 /                                                                 GC028070
00336  0040-UPDATE-DATE-REC-OCCURS.                                     GC028070
00337      MOVE 1 TO H.                                                 GC028070
00338      PERFORM 0110-FIND-OCCUR      THRU 0110-EXIT.                 GC028070
00339                                                                   GC028070
00340      IF  OCCURANCE-FOUND AND                                      GC028070
00341          DTE-TERMDT-CEN (H) EQUAL +9999365                        GC028070
00342          PERFORM 0050-DATE-CHECK THRU 0050-EXIT.                  GC028070
00343                                                                   GC028070
00344  0040-ADD-DATE-REC-OCCURS.                                        GC028070
00345                                                                   GC028070
00346 *  ADD CODE TO PREVENT ADDING A RECORD WITH SAME EFF DATE. RGO    GC028070
00347                                                                   GC028070
00348      MOVE 'N' TO SAME-DATE-FOUND.                                 GC028070
00349                                                                   GC028070
00350      COMPUTE REAL-COUNT = DTE-ENTRY-COUNT - 1.                    GC028070
00351      PERFORM 115-CHECK-DUP-ENTRY                                  GC028070
00352          VARYING SUBX FROM 1 BY 1                                 GC028070
00353          UNTIL ( (SUBX > REAL-COUNT)                              GC028070
00354                 OR (SAME-DATE-FOUND = 'Y')).                      GC028070
00355      IF SAME-DATE-FOUND = 'Y'                                     GC028070
00356         MOVE 'E' TO GC028070-IND                                  GC028070
00357         GO TO 0040-EXIT                                           GC028070
00358      END-IF.                                                      GC028070
00359                                                                   GC028070
00360      SET DTE-INDEX2 TO DTE-ENTRY-COUNT.                           GC028070
00361      SET DTE-INDEX2 UP BY 1.                                      GC028070
00362                                                                   GC028070
00363      PERFORM 0060-SHIFT-DATES THRU 0060-EXIT                      GC028070
00364          VARYING DTE-INDEX FROM DTE-ENTRY-COUNT                   GC028070
00365          BY -1 UNTIL DTE-INDEX EQUAL ZEROS.                       GC028070
00366                                                                   GC028070
00367      MOVE GCT-EFFDT-CEN          TO DTE-EFFDT-CEN (1).            GC028070
00368      MOVE GCT-PROVDR-CONTROL     TO DTE-PROVIDER-CNTRL (1).       GC028070
00369      MOVE GCT-FAM-REL-LVL        TO DTE-FAMILY-RELAT-LEVEL (1).   GC028070
00370      MOVE GCT-TERMDT-CEN         TO DTE-TERMDT-CEN (1).           GC028070
00371                                                                   GC028070
00372      ADD 1 TO DTE-ENTRY-COUNT.                                    GC028070
00373                                                                   GC028070
00374      IF  DTE-PROVIDER-CNTL-IN-USE-IND = 'Y'                       GC028070
00375          NEXT SENTENCE                                            GC028070
00376      ELSE                                                         GC028070
00377          IF  GCT-PROVDR-CONTROL = '00'                            GC028070
00378              MOVE '0'  TO  DTE-PROVIDER-CNTL-IN-USE-IND           GC028070
00379          ELSE                                                     GC028070
00380              MOVE 'Y'  TO  DTE-PROVIDER-CNTL-IN-USE-IND.          GC028070
00381                                                                   GC028070
00382      IF  DTE-F-R-L-IN-USE-IND  =  'Y'                             GC028070
00383          NEXT SENTENCE                                            GC028070
00384      ELSE                                                         GC028070
00385          IF  GCT-FAM-REL-LVL    = '00'                            GC028070
00386              MOVE '0'  TO  DTE-F-R-L-IN-USE-IND                   GC028070
00387          ELSE                                                     GC028070
00388              MOVE 'Y'  TO  DTE-F-R-L-IN-USE-IND.                  GC028070
00389                                                                   GC028070
00390                                                                   GC028070
00391      PERFORM 0090-WRITE-DATE THRU 0090-EXIT.                      GC028070
00392                                                                   GC028070
00393  0040-EXIT.                                                       GC028070
00394      EXIT.                                                        GC028070
00395 /                                                                 GC028070
00396  0050-DATE-CHECK.                                                 GC028070
00397                                                                   GC028070
00398 *                                                                 GC028070
00399 **   ADDED 10/14/85 TO ALLOW FOR PREVIOUS CONTRACTS WITHOU DTE CHGGC028070
00400 *                                                                 GC028070
00401      IF   GCT-EFFDT-CEN  IS LESS THAN  DTE-EFFDT-CEN (H)          GC028070
00402           GO TO 0050-EXIT.                                        GC028070
00403                                                                   GC028070
00404      MOVE 'SUB'                  TO MLDATE-FUNC.                  GC028070
00405      MOVE 'J'                    TO MLDATE-FORM1.                 GC028070
00406      MOVE 1                      TO MLDATE-AMOUNT.                GC028070
00407      MOVE GCT-EFFDT-CEN          TO MLDATE-DATE1.                 GC028070
00408      MOVE 'J'                    TO MLDATE-FORM2.                 GC028070
00409      MOVE SPACES                 TO MLDATE-DATE2.                 GC028070
00410      CALL 'MLDATE' USING MLDATE01.                                GC028070
00411      MOVE MLDATE-JUL2            TO JUL-DATE.                     GC028070
00412                                                                   GC028070
00413      MOVE JUL-DATE               TO DTE-TERMDT-CEN (H).           GC028070
00414                                                                   GC028070
00415      MOVE GCT-CONTRACT-ID        TO GCT2-CONTRACT-ID.             GC028070
00416      MOVE DTE-EFFDT-CEN (H)      TO GCT2-EFFDT-CEN.               GC028070
00417                                                                   GC028070
00418      PERFORM 0140-READ-CONTRACT THRU 0140-EXIT.                   GC028070
00419                                                                   GC028070
00420      MOVE JUL-DATE               TO GCT2-TERMDT-CEN.              GC028070
00421                                                                   GC028070
00422      PERFORM 0150-WRITE-CONTRACT THRU 0150-EXIT.                  GC028070
00423                                                                   GC028070
00424  0050-EXIT.                                                       GC028070
00425      EXIT.                                                        GC028070
00426 /                                                                 GC028070
00427  0060-SHIFT-DATES.                                                GC028070
00428                                                                   GC028070
00429      MOVE DTE-EFF-TERM (DTE-INDEX)                                GC028070
00430                                  TO DTE-EFF-TERM (DTE-INDEX2).    GC028070
00431                                                                   GC028070
00432      SET DTE-INDEX2 DOWN BY 1.                                    GC028070
00433                                                                   GC028070
00434  0060-EXIT.                                                       GC028070
00435      EXIT.                                                        GC028070
00436 /                                                                 GC028070
00437  0070-LOAD-DATE-KEY.                                              GC028070
00438                                                                   GC028070
00439      MOVE GC-GCDATES-VARY-MAX-OCUR                                GC028070
00440                                  TO DTE-ENTRY-COUNT.              GC028070
00441      MOVE LOW-VALUES             TO DTE-RECORD-KEY.               GC028070
00442      MOVE GCT-PLAN-CODE          TO DTE-CONTR-PLAN-CODE.          GC028070
00443      MOVE GCT-GROUP-NUM          TO DTE-CONTR-GROUP-NUM.          GC028070
00444      MOVE GCT-SECTION-NUM        TO DTE-CONTR-SECTION-NUM.        GC028070
00445      MOVE GCT-PKG-CODE           TO DTE-CONTR-PKG-CODE.           GC028070
00446      MOVE GCT-L-O-B              TO DTE-CONTR-L-O-B.              GC028070
00447      MOVE 'C'                    TO DTE-FILE-REF-IND.             GC028070
00448                                                                   GC028070
00449  0070-EXIT.                                                       GC028070
00450      EXIT.                                                        GC028070
00451 /                                                                 GC028070
00452  0080-READ-DATE.                                                  GC028070
00453                                                                   GC028070
00454      MOVE 36                 TO TWOA-REC-LENG.                    GC028070
00455                                                                   GC028070
00456      MOVE 'R'                TO REQUEST-TYPE-2.                   GC028070
00457      CALL 'TSGVSAM2' USING PARM-TWO PARM-TWOA.                    GC028070
00458                                                                   GC028070
00459      IF  REQUEST-TYPE-2 EQUAL '3' AND                             GC028070
00460          WRK-CHANGE-REQUEST                                       GC028070
00461          MOVE 0780 TO ABEND-CODE                                  GC028070
00462          DISPLAY '*** ABEND OCCURRED ***'                         GC028070
00463          DISPLAY 'ABEND CODE IS' ABEND-CODE                       GC028070
00464          DISPLAY '*** REQUEST WAS IGNORED AS A LOGICAL ERROR ***' GC028070
00465          PERFORM 1000-DISP-FIELDS THRU 1000-EXIT                  GC028070
00466          GO TO 9999-ERROR-RTN.                                    GC028070
00467                                                                   GC028070
00468      IF  (WRK-ADD-REQUEST  OR                                     GC028070
00469          WRK-KEY-FLD-ADD-REQ )                                    GC028070
00470          GO TO 0080-EXIT.                                         GC028070
00471                                                                   GC028070
00472      IF  REQUEST-TYPE-2 NOT EQUAL 'R'                             GC028070
00473          MOVE 0781 TO ABEND-CODE                                  GC028070
00474          DISPLAY '*** ABEND OCCURRED ***'                         GC028070
00475          DISPLAY 'ABEND CODE IS' ABEND-CODE                       GC028070
00476          DISPLAY '*** DATE FILE READ FAILED ***'                  GC028070
00477          DISPLAY 'DATE FILE REQUEST TYPE =' REQUEST-TYPE-2        GC028070
00478          PERFORM 1000-DISP-FIELDS THRU 1000-EXIT                  GC028070
00479          GO TO 9999-ERROR-RTN.                                    GC028070
00480                                                                   GC028070
00481                                                                   GC028070
00482  0080-EXIT.                                                       GC028070
00483      EXIT.                                                        GC028070
00484  0090-WRITE-DATE.                                                 GC028070
00485                                                                   GC028070
00486      PERFORM 200-SORT-TABULAR-ENTRIES THRU 200-EXIT.              GC028070
00487                                                                   GC028070
00488      COMPUTE TWOA-REC-LENG = 4 + GC-GCDATES-FIXED-LEN  +          GC028070
00489                  (DTE-ENTRY-COUNT * GC-GCDATES-VARY-LEN).         GC028070
00490                                                                   GC028070
00491 ****    VSAM =4, TAB FIXED PORTION = 80 , TAB VARIABLE = 12.      GC028070
00492                                                                   GC028070
00493      MOVE 'W'                TO REQUEST-TYPE-2.                   GC028070
00494      CALL 'TSGVSAM2' USING PARM-TWO PARM-TWOA.                    GC028070
00495                                                                   GC028070
00496      IF  REQUEST-TYPE-2 NOT EQUAL 'W'                             GC028070
00497          MOVE 0781 TO ABEND-CODE                                  GC028070
00498          DISPLAY '*** ABEND OCCURRED ***'                         GC028070
00499          DISPLAY 'ABEND CODE IS' ABEND-CODE                       GC028070
00500          DISPLAY '*** DATE FILE WRITE FAILED ***'                 GC028070
00501          DISPLAY 'DATE FILE REQUEST TYPE =' REQUEST-TYPE-2        GC028070
00502          PERFORM 1000-DISP-FIELDS THRU 1000-EXIT                  GC028070
00503          GO TO 9999-ERROR-RTN.                                    GC028070
00504                                                                   GC028070
00505  0090-EXIT.                                                       GC028070
00506      EXIT.                                                        GC028070
00507  0100-DET-CORRECT-ADD-OCCUR.                                      GC028070
00508                                                                   GC028070
00509       IF  DTE-EFF-TERM (H)  =  HIGH-VALUES                        GC028070
00510           MOVE  0700           TO ABEND-CODE                      GC028070
00511           DISPLAY '*** ABEND OCCURRED ***'                        GC028070
00512           DISPLAY 'ABEND CODE IS' ABEND-CODE                      GC028070
00513           DISPLAY '*** DATE FILE OCCURRENCE NOT FOUND ***'        GC028070
00514           PERFORM 1000-DISP-FIELDS THRU 1000-EXIT                 GC028070
00515           DISPLAY 'EFFECTIVE DATE         =' GCT-EFFDT-CEN        GC028070
00516           DISPLAY 'PROVIDER CONTOL        =' GCT-PROVDR-CONTROL   GC028070
00517           DISPLAY 'FAMILY RELATION LEVEL  =' GCT-FAM-REL-LVL      GC028070
00518           GO TO 9999-ERROR-RTN.                                   GC028070
00519                                                                   GC028070
00520       IF  GCT-EFFDT-CEN        =    DTE-EFFDT-CEN (H)          ANDGC028070
00521           GCT-PROVDR-CONTROL   =    DTE-PROVIDER-CNTRL (H)     ANDGC028070
00522           GCT-FAM-REL-LVL      =    DTE-FAMILY-RELAT-LEVEL (H)    GC028070
00523           GO TO 0100-EXIT.                                        GC028070
00524                                                                   GC028070
00525       ADD 1 TO H.                                                 GC028070
00526       GO TO 0100-DET-CORRECT-ADD-OCCUR.                           GC028070
00527  0100-EXIT.                                                       GC028070
00528      EXIT.                                                        GC028070
00529  0110-FIND-OCCUR.                                                 GC028070
00530                                                                   GC028070
00531       MOVE 'NO '              TO    OCCURANCE-WAS-FOUND.          GC028070
00532                                                                   GC028070
00533       IF  GCT-PROVDR-CONTROL   =    DTE-PROVIDER-CNTRL (H)     ANDGC028070
00534           GCT-FAM-REL-LVL      =    DTE-FAMILY-RELAT-LEVEL (H)    GC028070
00535           MOVE 'YES'          TO    OCCURANCE-WAS-FOUND           GC028070
00536           GO TO 0110-EXIT.                                        GC028070
00537                                                                   GC028070
00538       IF  DTE-EFF-TERM (H)  =  HIGH-VALUES                        GC028070
00539           GO TO 0110-EXIT.                                        GC028070
00540                                                                   GC028070
00541       ADD 1 TO H.                                                 GC028070
00542       GO TO 0110-FIND-OCCUR.                                      GC028070
00543  0110-EXIT.                                                       GC028070
00544      EXIT.                                                        GC028070
00545 /                                                                 GC028070
00546  115-CHECK-DUP-ENTRY.                                             GC028070
00547                                                                   GC028070
00548       IF  DTE-EFF-TERM (SUBX) = HIGH-VALUES                       GC028070
00549           MOVE  0700           TO ABEND-CODE                      GC028070
00550           DISPLAY '*** ABEND OCCURRED ***'                        GC028070
00551           DISPLAY 'ABEND CODE IS' ABEND-CODE                      GC028070
00552           DISPLAY '** DATE FILE OCCURRENCE EQUALS HIGH VALUES **' GC028070
00553           DISPLAY '*****        DATE RECORD IN ERROR *****'       GC028070
00554           DISPLAY 'OCCURRENCE   =' SUBX                           GC028070
00555           PERFORM 1000-DISP-FIELDS THRU 1000-EXIT                 GC028070
00556           GO TO 9999-ERROR-RTN.                                   GC028070
00557                                                                   GC028070
00558       IF  GCT-EFFDT-CEN        =    DTE-EFFDT-CEN (SUBX)       ANDGC028070
00559           GCT-PROVDR-CONTROL   =    DTE-PROVIDER-CNTRL (SUBX)  ANDGC028070
00560           GCT-FAM-REL-LVL      =    DTE-FAMILY-RELAT-LEVEL (SUBX) GC028070
00561           MOVE 'Y' TO SAME-DATE-FOUND                             GC028070
00562           PERFORM 120-DISPLAY-INFO                                GC028070
00563       END-IF.                                                     GC028070
00564  120-DISPLAY-INFO.                                                GC028070
00565       DISPLAY '*E* ERROR DETECTED IN CONTRACT PROGRAM GC028070,'. GC028070
00566       DISPLAY '*E* ATTEMPTING TO ADD AN OCCURS TO THE DATE FILE.'.GC028070
00567       DISPLAY '*E* KEY INFO FROM WRK-REC FOLLOWS:'.               GC028070
00568       DISPLAY '*E* PLAN CODE: ' WRK-PLAN-CODE                     GC028070
00569               ' GROUP: ' WRK-GROUP-NUM ' SECT: ' WRK-SECTION-NUM. GC028070
00570       DISPLAY '*E* PACKAGE CODE: ' WRK-PKG-CODE.                  GC028070
00571       DISPLAY '*E* L-O-B: ' WRK-L-O-B ' '                         GC028070
00572       DISPLAY '*E* PROV-CTL: ' WRK-PROV-CTL ' FRL: '              GC028070
00573               WRK-FAM-REL-LEVEL.                                  GC028070
00574       DISPLAY '*E* EFFECTIVE DATE: ' WRK-EFFDT-CEN.               GC028070
00575       DISPLAY '*E* SIGNAL FROM ONLINE = ' WRK-SIGNAL-FROM-ONLINE. GC028070
00576       DISPLAY '*E* OPERATOR-ID = ' WRK-OPERATOR-ID.               GC028070
00577                                                                   GC028070
00578  0140-READ-CONTRACT.                                              GC028070
00579                                                                   GC028070
00580      MOVE 33                 TO ONEA-REC-LENG.                    GC028070
00581      MOVE 'R'                TO REQUEST-TYPE-1.                   GC028070
00582      CALL 'TSGVSAM1' USING PARM-ONE PARM-ONEA.                    GC028070
00583                                                                   GC028070
00584      IF  REQUEST-TYPE-1 NOT EQUAL 'R'                             GC028070
00585          GO TO 9999-ERROR-RTN.                                    GC028070
00586                                                                   GC028070
00587  0140-EXIT.                                                       GC028070
00588      EXIT.                                                        GC028070
00589  0150-WRITE-CONTRACT.                                             GC028070
00590                                                                   GC028070
00591      MOVE 'U'                TO REQUEST-TYPE-1.                   GC028070
00592      CALL 'TSGVSAM1' USING PARM-ONE PARM-ONEA.                    GC028070
00593                                                                   GC028070
00594      IF  REQUEST-TYPE-1 NOT EQUAL 'U'                             GC028070
00595          MOVE  0750    TO ABEND-CODE                              GC028070
00596          DISPLAY '*** ABEND OCCURRED ***'                         GC028070
00597          DISPLAY 'ABEND CODE IS' ABEND-CODE                       GC028070
00598          DISPLAY '*** CONTRACT UPDATE FAILED ***'                 GC028070
00599          DISPLAY 'REQUEST TYPE     =' REQUEST-TYPE-1              GC028070
00600          PERFORM 1000-DISP-FIELDS THRU 1000-EXIT                  GC028070
00601          DISPLAY 'PLAN CODE            =' GCT-PLAN-CODE           GC028070
00602          DISPLAY 'GROUP NUMBER         =' GCT-GROUP-NUM           GC028070
00603          DISPLAY 'SECTION NUMBER       =' GCT-SECTION-NUM         GC028070
00604          DISPLAY 'PACKAGE CODE         =' GCT-PKG-CODE            GC028070
00605          DISPLAY 'LINE OF BUISNESS     =' GCT-L-O-B               GC028070
00606          DISPLAY 'PROVIDER CONTROL     =' GCT-PROVDR-CONTROL      GC028070
00607          DISPLAY 'FAMILY RELATION LEVEL=' GCT-FAM-REL-LVL         GC028070
00608          DISPLAY 'EFFECTIVE DATE       =' GCT-EFFDT-CEN           GC028070
00609          GO TO 9999-ERROR-RTN.                                    GC028070
00610                                                                   GC028070
00611  0150-EXIT.                                                       GC028070
00612      EXIT.                                                        GC028070
00613 /                                                                 GC028070
00614  200-SORT-TABULAR-ENTRIES.                                        GC028070
00615                                                                   GC028070
00616      MOVE DTE-ENTRY-COUNT    TO  TAB-ENTRY-COUNT.                 GC028070
00617      MOVE DTE-EFF-TERM-AREA  TO  TAB-WORK-AREA-ALL-TERM.          GC028070
00618                                                                   GC028070
00619      COMPUTE SORT-VALUE   = DTE-ENTRY-COUNT - 2.                  GC028070
00620      COMPUTE CHECK-VALUE  = SORT-VALUE + 1.                       GC028070
00621                                                                   GC028070
00622      PERFORM  230-CHECK-MINOR-VALUE THRU 230-EXIT                 GC028070
00623               VARYING I FROM 1 BY 1 UNTIL                         GC028070
00624               I IS GREATER THAN SORT-VALUE.                       GC028070
00625                                                                   GC028070
00626      PERFORM  270-CHECK-MAJOR-VALUE THRU 270-EXIT                 GC028070
00627               VARYING I FROM 1 BY 1 UNTIL                         GC028070
00628               I IS GREATER THAN SORT-VALUE.                       GC028070
00629                                                                   GC028070
00630      MOVE TAB-ENTRY-COUNT         TO  DTE-ENTRY-COUNT.            GC028070
00631      MOVE TAB-WORK-AREA-ALL-TERM  TO  DTE-EFF-TERM-AREA.          GC028070
00632                                                                   GC028070
00633  200-EXIT.                                                        GC028070
00634      EXIT.                                                        GC028070
00635  230-CHECK-MINOR-VALUE.                                           GC028070
00636                                                                   GC028070
00637      COMPUTE K = I + 1.                                           GC028070
00638                                                                   GC028070
00639      PERFORM  240-MINOR-SORT-RTN THRU 240-EXIT                    GC028070
00640               VARYING J FROM K BY 1 UNTIL                         GC028070
00641               J IS GREATER THAN CHECK-VALUE.                      GC028070
00642                                                                   GC028070
00643  230-EXIT.                                                        GC028070
00644      EXIT.                                                        GC028070
00645  240-MINOR-SORT-RTN.                                              GC028070
00646      IF  TAB-REST (I)  GREATER THAN TAB-REST (J)                  GC028070
00647          MOVE TAB-WRK-TERM (I) TO TAB-WORK-AREA                   GC028070
00648          MOVE TAB-WRK-TERM (J) TO TAB-WRK-TERM (I)                GC028070
00649          MOVE TAB-WORK-AREA    TO TAB-WRK-TERM (J).               GC028070
00650                                                                   GC028070
00651  240-EXIT.                                                        GC028070
00652      EXIT.                                                        GC028070
00653 /                                                                 GC028070
00654  270-CHECK-MAJOR-VALUE.                                           GC028070
00655                                                                   GC028070
00656      COMPUTE K = I + 1.                                           GC028070
00657                                                                   GC028070
00658      PERFORM  280-MAJOR-SORT-RTN THRU 280-EXIT                    GC028070
00659               VARYING J FROM K BY 1 UNTIL                         GC028070
00660               J IS GREATER THAN CHECK-VALUE.                      GC028070
00661                                                                   GC028070
00662  270-EXIT.                                                        GC028070
00663      EXIT.                                                        GC028070
00664  280-MAJOR-SORT-RTN.                                              GC028070
00665      IF  TAB-EFFDT-CEN (I) LESS THAN  TAB-EFFDT-CEN (J)           GC028070
00666          MOVE TAB-WRK-TERM (I) TO TAB-WORK-AREA                   GC028070
00667          MOVE TAB-WRK-TERM (J) TO TAB-WRK-TERM (I)                GC028070
00668          MOVE TAB-WORK-AREA    TO TAB-WRK-TERM (J).               GC028070
00669                                                                   GC028070
00670  280-EXIT.                                                        GC028070
00671      EXIT.                                                        GC028070
00672 /                                                                 GC028070
00673  0900-OPEN-FILES.                                                 GC028070
00674                                                                   GC028070
00675      MOVE 'S'                TO REQUEST-TYPE-2.                   GC028070
00676      MOVE 8                  TO SET-REC-LENG.                     GC028070
00677      MOVE 3                  TO SET-VALUE.                        GC028070
00678      CALL 'TSGVSAM2' USING PARM-TWO PARM-SET.                     GC028070
00679      IF  REQUEST-TYPE-2 NOT EQUAL 'S'                             GC028070
00680          MOVE 0701      TO ABEND-CODE                             GC028070
00681          DISPLAY '*** ABEND OCCURRED ***'                         GC028070
00682          DISPLAY 'ABEND CODE IS' ABEND-CODE                       GC028070
00683          DISPLAY '*** SET FAILED FOR DATE FILE ***'               GC028070
00684          DISPLAY 'REQUEST TYPE    =' REQUEST-TYPE-2               GC028070
00685          GO TO 9999-ERROR-RTN.                                    GC028070
00686                                                                   GC028070
00687      MOVE 'O'                TO REQUEST-TYPE-2.                   GC028070
00688      CALL 'TSGVSAM2' USING PARM-TWO PARM-TWOA.                    GC028070
00689      IF  REQUEST-TYPE-2 NOT EQUAL 'O'                             GC028070
00690          MOVE 7902  TO ABEND-CODE                                 GC028070
00691          DISPLAY '*** ABEND OCCURRED ***'                         GC028070
00692          DISPLAY 'ABEND CODE IS' ABEND-CODE                       GC028070
00693          DISPLAY '*** OPEN FAILED FOR DATE FILE ***'              GC028070
00694          DISPLAY 'REQUEST TYPE    =' REQUEST-TYPE-2               GC028070
00695          GO TO 9999-ERROR-RTN.                                    GC028070
00696                                                                   GC028070
00697  0900-EXIT.                                                       GC028070
00698      EXIT.                                                        GC028070
00699 /                                                                 GC028070
00700  0910-CLOSE-FILES.                                                GC028070
00701                                                                   GC028070
00702      MOVE 'C'                TO REQUEST-TYPE-2.                   GC028070
00703      CALL 'TSGVSAM2' USING PARM-TWO PARM-TWOA.                    GC028070
00704      IF  REQUEST-TYPE-2 NOT EQUAL 'C'                             GC028070
00705          MOVE 0710  TO ABEND-CODE                                 GC028070
00706          DISPLAY '*** ABEND OCCURRED ***'                         GC028070
00707          DISPLAY 'ABEND CODE IS' ABEND-CODE                       GC028070
00708          DISPLAY '*** CLOSE FAILED FOR DATE FILE ***'             GC028070
00709          DISPLAY 'REQUEST TYPE    =' REQUEST-TYPE-2               GC028070
00710          GO TO 9999-ERROR-RTN.                                    GC028070
00711                                                                   GC028070
00712  0910-EXIT.                                                       GC028070
00713      EXIT.                                                        GC028070
00714 /                                                                 GC028070
00715  1000-DISP-FIELDS.                                                GC028070
00716      DISPLAY 'CONTRACT PLAN CODE       =' DTE-CONTR-PLAN-CODE.    GC028070
00717      DISPLAY 'CONTRACT GROUP NUMBER    =' DTE-CONTR-GROUP-NUM.    GC028070
00718      DISPLAY 'CONTRACT SECTION NUMBER  =' DTE-CONTR-SECTION-NUM.  GC028070
00719      DISPLAY 'CONTRACT PACKAGE CODE    =' DTE-CONTR-PKG-CODE.     GC028070
00720      DISPLAY 'CONTRACT LINE OF BUISNESS=' DTE-CONTR-L-O-B.        GC028070
00721      DISPLAY 'FILE REFERENCE INDICATOR =' DTE-FILE-REF-IND.       GC028070
00722      DISPLAY '*** YOU ARE IN PROGRAM GC028070TS'.                 GC028070
00723  1000-EXIT.                                                       GC028070
00724      EXIT.                                                        GC028070
00725  9999-ERROR-RTN.                                                  GC028070
00726                                                                   GC028070
00727      CALL 'TSGEND' USING ABEND-CODE.                              GC028070
00728                                                                   GC028070
00729  9999-EXIT.                                                       GC028070
00730      EXIT.                                                        GC028070
