00001  IDENTIFICATION DIVISION.                                         12/09/02
00002  PROGRAM-ID.     GC0078.                                          GC0078  
00003  AUTHOR.         ED WITKUS.                                          LV002
00004  INSTALLATION.   HCSC.                                            GC0078  
00005  DATE-WRITTEN.   DECEMBER, 1987.                                  GC0078  
00006  DATE-COMPILED.                                                   GC0078  
00007 ******************************************************************GC0078  
00008 *   ACCUM TABULAR RECORD MATCH AND SLOT UPDATE PROGRAM.           GC0078  
00009 *   INPUT-FILE CONTAINS EXISTING AND WORK RECORDS SORTED BY:      GC0078  
00010 *       1. TABULAR ID.                                            GC0078  
00011 *       2. BODY OF RECORD.                                        GC0078  
00012 *       3. TABULAR SLOT.                                          GC0078  
00013 *   OUTPUT-FILE IS THE SLOTUPDATE FILE.                           GC0078  
00014 *   PROCESSING.                                                   GC0078  
00015 *       THIS PROGRAM READS THE INPUT-FILE AND MAY RECEIVE THE     GC0078  
00016 *       FOLLOWING RECORDS:                                        GC0078  
00017 *           1.  LAST SLOT RECORD, USED TO ADD A NEW RECORD.       GC0078  
00018 *           2.  EXISTING RECORD, SLOT NUMBER < 9,000,000.         GC0078  
00019 *           3.  MODIFIED RECORD, SLOT NUMBER > 8,999,999.         GC0078  
00020 *                                                                 GC0078  
00021 *       WHEN LAST SLOT RECORD (IN-ID NOT = WS-ID) IS ENCOUNTERED, GC0078  
00022 *       MOVE IN-KEY TO WS-TAB-KEY. MOVE THE SLOT TO WS-LAST-SLOT. GC0078  
00023 *       READ NEXT.                                                GC0078  
00024 *                                                                 GC0078  
00025 *       WHEN EXISTING RECORD IS ENCOUNTERED, MOVE IT WS.          GC0078  
00026 *       READ NEXT.                                                GC0078  
00027 *                                                                 GC0078  
00028 *       WHEN MODIFIED RECORD IS ENCOUNTERED, CHECK VS. RECORD IN  GC0078  
00029 *       WS. IF THE RECORD MATCHES, ASSIGN EXISTING RECORD'S SLOT  GC0078  
00030 *       NUMBER, REFORMAT NON-COMPARE AREA, WRITE OUTPUT RECORD.   GC0078  
00031 *       READ NEXT.                                                GC0078  
00032 *                                                                 GC0078  
00033 *       IF THE RECORD DOES NOT MATCH, SAME LOGIC AS ABOVE, EXCEPT GC0078  
00034 *       ADD 1 TO THE WS-LAST-SLOT AND ASSIGN THAT SLOT NUMBER.    GC0078  
00035 *       MOVE INPUT-RECORD TO WS.                                  GC0078  
00036 *       READ NEXT.                                                GC0078  
00037 ******************************************************************GC0078  
00038 ******************************************************************GC0078  
00039 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC0078  
00040 *       *-*         U P D A T E   H I S T O R Y         *-*      *GC0078  
00041 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC0078  
00042 *                                                                *GC0078  
00043 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*GC0078  
00044 *    XXXX    12/15/87  EW   ORIGINAL MODULE.                     *GC0078  
00045 *  P0012   01/08/89  NGE    TO FIX A BUG IN CDE LOGIC ADDED OCCUR*GC0078  
00046 *                           UPDATING W/F IMSGE FROM PROD TABULAR *GC0078  
00047 *                           BOTH COMPARE AND NONE COMPARE AREAS. *GC0078  
00048 *                                                                *GC0078  
00049 *                       ----ACCUM TABULAR RECORD MODIFICATION--- *GC0078  
00050 * 11154   10/02/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *GC0078  
00051 * D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *GC0078  
00052 * D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *GC0078  
00053 * D270                  4. INCREASE MAX REC LENGTH FOR ACCUM REC *GC0078  
00054 *                          TO 8157.                              *GC0078  
00055 *                                                                *GC0078  
00056 *  11154      2/21/91  FRY  -DECREASE MAX OCCURS FROM  46 TO 44. *GC0078  
00057 *                           -DECREASE MAX REC LENGTH FOR ACCUM   *GC0078  
00058 *                             RECORD FROM 8157 TO 7805.          *GC0078  
00059 *                      FILE SECTION CHANGED:                     *GC0078  
00060 *                     -IN-COMPARE-AREA  PIC X(7728) TO  7392     *GC0078  
00061 *                     -IN-NON-COMP-TAB OCCURS 46 TIMES CHANGED   *GC0078  
00062 *                       TO IN-NON-COMP-TAB OCCURS 44 TIMES.      *GC0078  
00063 *                     -IN-COMP-TAB     OCCURS 46 TIMES CHANGED   *GC0078  
00064 *                       TO IN-COMP-TAB     OCCURS 44 TIMES.      *GC0078  
00065 *                     -OUTPUT-REC       PIC X(8221) TO  7869     *GC0078  
00066 *                      WORKING SECTION CHANGED:                  *GC0078  
00067 *                     -WS-COMPARE-AREA  PIC X(7728)  TO  7392    *GC0078  
00068 *                     -WS-NON-COMP-TAB OCCURS 46 TIMES CHANGED   *GC0078  
00069 *                       TO WS-NON-COMP-TAB OCCURS 44 TIMES.      *GC0078  
00070 *                     -WS-COMP-TAB OCCURS 46 TIMES CHANGED       *GC0078  
00071 *                       TO WS-COMP-TAB OCCURS 44 TIMES.          *GC0078  
00072 *                                                                *GC0078  
00073 ******************************************************************GC0078  
00074 * 11836   06/27/91  TPM    EXPAND IN-OCCURS-COMPARE AREA FROM    *GC0078  
00075 *                          168 TO 172 TO HANDLE \
00076 *                          ELIMINATE  FOUR BYTES FROM THE NON-   *GC0078  
00077 *                          COMPARE AREA TO ACCOMODATE FOR THE    *GC0078  
00078 *                          THE EXPANSION OF THE IN-COMPARE AREA. *GC0078  
00079 *                                                                *GC0078  
00080 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GC0078  
ED0624* BBDA-58217 06/04/24   ED    RECOMPILE FOR PEAQ COPYBOOK        *        
ED0624*                             EXPANSION:                         *        
ED0624*                                   COPYBKS - GCTABM*, GCTACL*,  *        
ED0624*                                   GCTACP*,  GCTADL*, GCTADL*   *        
00081 *                                                                *GC0078  
00082 ******************************************************************GC0078  
00083 ******************************************************************GC0078  
00084  ENVIRONMENT DIVISION.                                            GC0078  
00085  CONFIGURATION SECTION.                                           GC0078  
00086  SOURCE-COMPUTER.  IBM-370.                                       GC0078  
00087  OBJECT-COMPUTER.  IBM-370.                                       GC0078  
00088  INPUT-OUTPUT SECTION.                                            GC0078  
00089                                                                   GC0078  
00090  FILE-CONTROL.                                                    GC0078  
00091      SELECT INPUT-FILE         ASSIGN  TO UT-S-GC0078A.           GC0078  
00092      SELECT OUTPUT-FILE        ASSIGN  TO UT-S-GC0078B.           GC0078  
00093  DATA DIVISION.                                                   GC0078  
00094  FILE SECTION.                                                    GC0078  
00095                                                                   GC0078  
00096  FD  INPUT-FILE                                                   GC0078  
00097      LABEL RECORDS ARE STANDARD                                   GC0078  
00098      RECORDING MODE IS F                                          GC0078  
00099      BLOCK CONTAINS  0  RECORDS.                                  GC0078  
00100  01  IN-REC.                                                      GC0078  
00101      COPY  GCWRKDCC.                                              GC0078  
00102      05  IN-FIXED-PORTION.                                        GC0078  
00103          10  IN-TAB-KEY.                                          GC0078  
00104              15  IN-TAB-ID              PIC X(6).                 GC0078  
00105              15  IN-TAB-SLOT            PIC S9(7)  COMP-3.        GC0078  
00106          10  FILLER                     PIC X(27).                GC0078  
00107          10  IN-ENTRY-COUNT             PIC S9(5)  COMP-3.        GC0078  
00108          10  FILLER                     PIC X(21).                GC0078  
00109      05  IN-NON-COMPARE-AREA            PIC X(176).               GC0078  
00110      05  IN-NON-COMPARE-TABLE                                     GC0078  
00111              REDEFINES IN-NON-COMPARE-AREA.                       GC0078  
00112          10  IN-NON-COMP-TAB OCCURS 44 TIMES                      GC0078  
00113                               INDEXED BY NON-COMP-INDEX.          GC0078  
00114              15  IN-NON-COMP-ENT-CNT PIC S9(7) COMP-3.            GC0078  
00115      05  IN-COMPARE-AREA                PIC X(7568).              GC0078  
00116      05  IN-OCCURS-TABLE REDEFINES IN-COMPARE-AREA.               GC0078  
00117          10  IN-COMP-TAB OCCURS 44 TIMES                          GC0078  
00118                               INDEXED BY COMP-INDEX.              GC0078  
00119              15  FILLER               PIC X(172).                 GC0078  
00120 /                                                                 GC0078  
00121  FD  OUTPUT-FILE                                                  GC0078  
00122      LABEL RECORDS ARE STANDARD                                   GC0078  
00123      RECORDING MODE IS V                                          GC0078  
00124      BLOCK CONTAINS  0  RECORDS.                                  GC0078  
00125                                                                   GC0078  
00126  01  OUTPUT-REC                       PIC X(7869).                GC0078  
00127                                                                   GC0078  
00128  01  OUTPUT-ABM-REC.                                              GC0078  
00129      05 ABM-WORK-KEY                  PIC X(64).                  GC0078  
00130      COPY  GCTABMC.                                               GC0078  
00131 /                                                                 GC0078  
00132  01  OUTPUT-ACL-REC.                                              GC0078  
00133      05 ACL-WORK-KEY                  PIC X(64).                  GC0078  
00134      COPY  GCTACLC.                                               GC0078  
00135 /                                                                 GC0078  
00136  01  OUTPUT-ADL-REC.                                              GC0078  
00137      05 ADL-WORK-KEY                  PIC X(64).                  GC0078  
00138      COPY  GCTADLC.                                               GC0078  
00139 /                                                                 GC0078  
00140  01  OUTPUT-AOL-REC.                                              GC0078  
00141      05 AOL-WORK-KEY                  PIC X(64).                  GC0078  
00142      COPY  GCTAOLC.                                               GC0078  
00143 /                                                                 GC0078  
00144  WORKING-STORAGE SECTION.                                         GC0078  
00145  77  WS-PGM-ID                     PIC  X(24) VALUE               GC0078  
00146      'GC0078 WORKING STORAGE'.                                    GC0078  
00147                                                                   GC0078  
00148  01  SWITCHES.                                                    GC0078  
00149      05  WS-EOF-SW                 PIC X      VALUE '0'.          GC0078  
00150          88  EOF                              VALUE '1'.          GC0078  
00151                                                                   GC0078  
00152  01  MISC-AREA.                                                   GC0078  
00153      05  WS-LAST-KEY.                                             GC0078  
00154          10 WS-LAST-ID             PIC S9(7)  VALUE +0   COMP-3.  GC0078  
00155          10 WS-LAST-SLOT           PIC S9(7)  VALUE +0   COMP-3.  GC0078  
00156 /                                                                 GC0078  
00157  01  WS-REC.                                                      GC0078  
00158      05  WS-WORK-KEY                    PIC X(64).                GC0078  
00159      05  WS-FIXED-PORTION.                                        GC0078  
00160          10  WS-TAB-KEY.                                          GC0078  
00161              15  WS-TAB-ID              PIC X(6).                 GC0078  
00162              15  WS-TAB-SLOT            PIC S9(7)  COMP-3.        GC0078  
00163          10  FILLER                     PIC X(27).                GC0078  
00164          10  WS-ENTRY-COUNT             PIC S9(5)  COMP-3.        GC0078  
00165          10  FILLER                     PIC X(21).                GC0078  
00166      05  WS-NON-COMPARE-AREA            PIC X(176).               GC0078  
00167      05  WS-NON-COMPARE-TABLE                                     GC0078  
00168              REDEFINES WS-NON-COMPARE-AREA.                       GC0078  
00169          10  WS-NON-COMP-TAB OCCURS 44 TIMES                      GC0078  
00170                               INDEXED BY WS-NON-COMP-INDEX.       GC0078  
00171              15  WS-NON-COMP-ENT-CNT PIC S9(7) COMP-3.            GC0078  
00172      05  WS-COMPARE-AREA                PIC X(7568).              GC0078  
00173      05  WS-OCCURS-TABLE REDEFINES WS-COMPARE-AREA.               GC0078  
00174          10  WS-COMP-TAB OCCURS 44 TIMES                          GC0078  
00175                               INDEXED BY WS-COMP-INDEX.           GC0078  
00176              15  FILLER               PIC X(172).                 GC0078  
00177 /                                                                 GC0078  
00178  PROCEDURE DIVISION.                                              GC0078  
00179                                                                   GC0078  
00180  0000-BEGIN.                                                      GC0078  
00181      PERFORM R100-OPEN THRU R100-EXIT.                            GC0078  
00182      PERFORM R200-READ THRU R200-EXIT.                            GC0078  
00183      IF EOF                                                       GC0078  
00184          DISPLAY 'GC0078--NO INPUT RECORDS RECEIVED'              GC0078  
00185          PERFORM R300-CLOSE THRU R300-EXIT                        GC0078  
00186          STOP RUN.                                                GC0078  
00187      PERFORM 1000-PROCESS THRU 1000-EXIT                          GC0078  
00188          UNTIL EOF.                                               GC0078  
00189      PERFORM R300-CLOSE   THRU R300-EXIT.                         GC0078  
00190      STOP RUN.                                                    GC0078  
00191                                                                   GC0078  
00192  1000-PROCESS.                                                    GC0078  
00193                                                                   GC0078  
00194      PERFORM 1100-CHECK THRU 1100-EXIT.                           GC0078  
00195      PERFORM R200-READ THRU R200-EXIT.                            GC0078  
00196                                                                   GC0078  
00197  1000-EXIT.                                                       GC0078  
00198      EXIT.                                                        GC0078  
00199 /                                                                 GC0078  
00200  1100-CHECK.                                                      GC0078  
00201 *****                                                             GC0078  
00202 **  THIS CHECKS FOR THE LAST-SLOT RECORD.                         GC0078  
00203 **  IF IT IS THE LAST-SLOT RECORD, MOVE IT TO WS.                 GC0078  
00204 *****                                                             GC0078  
00205      IF IN-TAB-ID NOT = WS-TAB-ID                                 GC0078  
00206          MOVE IN-REC         TO WS-REC,                           GC0078  
00207          MOVE IN-TAB-SLOT    TO WS-LAST-SLOT,                     GC0078  
00208          GO TO 1100-EXIT.                                         GC0078  
00209                                                                   GC0078  
00210 *****                                                             GC0078  
00211 **  THIS CHECKS FOR A MODIFIED RECORD.                            GC0078  
00212 **  IF NOT MODIFIED, JUST MOVE THE INPUT REC TO WS.               GC0078  
00213 *****                                                             GC0078  
00214      IF IN-TAB-SLOT NOT > +8999999                                GC0078  
00215          MOVE IN-REC         TO WS-REC,                           GC0078  
00216          GO TO 1100-EXIT.                                         GC0078  
00217                                                                   GC0078  
00218 *****                                                             GC0078  
00219 **  THIS CHECKS FOR A MATCH WITH THE PREVIOUS RECORD.             GC0078  
00220 **  IF IT DOES MATCH, PERFORM EXISTING SLOT ROUTINE.              GC0078  
00221 *****                                                             GC0078  
00222      IF IN-COMPARE-AREA = WS-COMPARE-AREA                         GC0078  
00223          PERFORM 1200-EXIST-RTN THRU 1200-EXIT,                   GC0078  
00224          GO TO 1100-EXIT.                                         GC0078  
00225                                                                   GC0078  
00226 *****                                                             GC0078  
00227 **  THERE IS NO MATCH FOR THIS INPUT RECORD.                      GC0078  
00228 **  PERFORM NEW-SLOT ROUTINE.                                     GC0078  
00229 *****                                                             GC0078  
00230      PERFORM 1300-NEW-SLOT-RTN THRU 1300-EXIT.                    GC0078  
00231      MOVE IN-REC TO WS-REC.                                       GC0078  
00232      MOVE WS-LAST-SLOT TO WS-TAB-SLOT.                            GC0078  
00233  1100-EXIT.                                                       GC0078  
00234      EXIT.                                                        GC0078  
00235 /                                                                 GC0078  
00236  1200-EXIST-RTN.                                                  GC0078  
00237       IF IN-TAB-ID = '#ABM  '                                     GC0078  
00238           PERFORM 1200-100-MOVE-EXISTING-ABM THRU 1200-100-EXIT   GC0078  
00239           GO TO 1200-EXIT.                                        GC0078  
00240       IF IN-TAB-ID = '#ACL  '                                     GC0078  
00241           PERFORM 1200-200-MOVE-EXISTING-ACL THRU 1200-200-EXIT   GC0078  
00242           GO TO 1200-EXIT.                                        GC0078  
00243       IF IN-TAB-ID = '#ADL  '                                     GC0078  
00244           PERFORM 1200-300-MOVE-EXISTING-ADL THRU 1200-300-EXIT   GC0078  
00245           GO TO 1200-EXIT.                                        GC0078  
00246       IF IN-TAB-ID = '#AOL  '                                     GC0078  
00247           PERFORM 1200-400-MOVE-EXISTING-AOL THRU 1200-400-EXIT.  GC0078  
00248  1200-EXIT.                                                       GC0078  
00249      EXIT.                                                        GC0078  
00250                                                                   GC0078  
00251  1200-100-MOVE-EXISTING-ABM.                                      GC0078  
00252      MOVE LOW-VALUES       TO OUTPUT-ABM-REC.                     GC0078  
00253      MOVE 'E'              TO WRK-SIGNAL-BATCH-INTERNAL.          GC0078  
00254      MOVE WORK-RECORD      TO ABM-WORK-KEY.                       GC0078  
00255      MOVE WS-ENTRY-COUNT   TO GAA-ENTRY-COUNT.                    GC0078  
00256      MOVE WS-FIXED-PORTION TO GAA-FIXED-PORTION.                  GC0078  
00257      MOVE WS-TAB-SLOT      TO GAA-PROVISION-SLOT-NO.              GC0078  
00258      SET GAA-INDEX                                                GC0078  
00259          WS-NON-COMP-INDEX TO 1.                                  GC0078  
00260      PERFORM R410-MOVE-ABM-OCCURS THRU R410-EXIT                  GC0078  
00261          VARYING WS-COMP-INDEX FROM 1 BY 1                        GC0078  
00262          UNTIL   WS-COMP-INDEX  >  WS-ENTRY-COUNT.                GC0078  
00263      PERFORM R800-WRITE-ABM THRU R800-EXIT.                       GC0078  
00264  1200-100-EXIT.                                                   GC0078  
00265      EXIT.                                                        GC0078  
00266                                                                   GC0078  
00267 /                                                                 GC0078  
00268  1200-200-MOVE-EXISTING-ACL.                                      GC0078  
00269      MOVE LOW-VALUES       TO OUTPUT-ACL-REC.                     GC0078  
00270      MOVE 'E'              TO WRK-SIGNAL-BATCH-INTERNAL.          GC0078  
00271      MOVE WORK-RECORD      TO ACL-WORK-KEY.                       GC0078  
00272      MOVE WS-ENTRY-COUNT   TO GAB-ENTRY-COUNT.                    GC0078  
00273      MOVE WS-FIXED-PORTION TO GAB-FIXED-PORTION.                  GC0078  
00274      MOVE WS-TAB-SLOT      TO GAB-PROVISION-SLOT-NO.              GC0078  
00275      SET GAB-INDEX                                                GC0078  
00276          WS-NON-COMP-INDEX TO 1.                                  GC0078  
00277      PERFORM R510-MOVE-ACL-OCCURS THRU R510-EXIT                  GC0078  
00278          VARYING WS-COMP-INDEX FROM 1 BY 1                        GC0078  
00279          UNTIL   WS-COMP-INDEX  >  WS-ENTRY-COUNT.                GC0078  
00280      PERFORM R900-WRITE-ACL THRU R900-EXIT.                       GC0078  
00281                                                                   GC0078  
00282  1200-200-EXIT.                                                   GC0078  
00283      EXIT.                                                        GC0078  
00284                                                                   GC0078  
00285 /                                                                 GC0078  
00286  1200-300-MOVE-EXISTING-ADL.                                      GC0078  
00287      MOVE LOW-VALUES       TO OUTPUT-ADL-REC.                     GC0078  
00288      MOVE 'E'              TO WRK-SIGNAL-BATCH-INTERNAL.          GC0078  
00289      MOVE WORK-RECORD      TO ADL-WORK-KEY.                       GC0078  
00290      MOVE WS-ENTRY-COUNT   TO GAC-ENTRY-COUNT.                    GC0078  
00291      MOVE WS-FIXED-PORTION TO GAC-FIXED-PORTION.                  GC0078  
00292      MOVE WS-TAB-SLOT      TO GAC-PROVISION-SLOT-NO.              GC0078  
00293      SET GAC-INDEX                                                GC0078  
00294          WS-NON-COMP-INDEX TO 1.                                  GC0078  
00295      PERFORM R610-MOVE-ADL-OCCURS THRU R610-EXIT                  GC0078  
00296          VARYING WS-COMP-INDEX FROM 1 BY 1                        GC0078  
00297          UNTIL   WS-COMP-INDEX  >  WS-ENTRY-COUNT.                GC0078  
00298      PERFORM R1000-WRITE-ADL THRU R1000-EXIT.                     GC0078  
00299                                                                   GC0078  
00300  1200-300-EXIT.                                                   GC0078  
00301      EXIT.                                                        GC0078  
00302                                                                   GC0078  
00303 /                                                                 GC0078  
00304  1200-400-MOVE-EXISTING-AOL.                                      GC0078  
00305      MOVE LOW-VALUES       TO OUTPUT-AOL-REC.                     GC0078  
00306      MOVE 'E'              TO WRK-SIGNAL-BATCH-INTERNAL.          GC0078  
00307      MOVE WORK-RECORD      TO AOL-WORK-KEY.                       GC0078  
00308      MOVE WS-ENTRY-COUNT   TO GAD-ENTRY-COUNT.                    GC0078  
00309      MOVE WS-FIXED-PORTION TO GAD-FIXED-PORTION.                  GC0078  
00310      MOVE WS-TAB-SLOT      TO GAD-PROVISION-SLOT-NO.              GC0078  
00311      SET GAD-INDEX                                                GC0078  
00312          WS-NON-COMP-INDEX TO 1.                                  GC0078  
00313      PERFORM R710-MOVE-AOL-OCCURS THRU R710-EXIT                  GC0078  
00314          VARYING WS-COMP-INDEX FROM 1 BY 1                        GC0078  
00315          UNTIL   WS-COMP-INDEX  >  WS-ENTRY-COUNT.                GC0078  
00316      PERFORM R1100-WRITE-AOL THRU R1100-EXIT.                     GC0078  
00317                                                                   GC0078  
00318  1200-400-EXIT.                                                   GC0078  
00319      EXIT.                                                        GC0078  
00320                                                                   GC0078  
00321 /                                                                 GC0078  
00322  1300-NEW-SLOT-RTN.                                               GC0078  
00323       ADD 1 TO WS-LAST-SLOT.                                      GC0078  
00324       IF IN-TAB-ID = '#ABM  '                                     GC0078  
00325           PERFORM 1300-100-MOVE-NEW-ABM THRU 1300-100-EXIT        GC0078  
00326           GO TO 1300-EXIT.                                        GC0078  
00327       IF IN-TAB-ID = '#ACL  '                                     GC0078  
00328           PERFORM 1300-200-MOVE-NEW-ACL THRU 1300-200-EXIT        GC0078  
00329           GO TO 1300-EXIT.                                        GC0078  
00330       IF IN-TAB-ID = '#ADL  '                                     GC0078  
00331           PERFORM 1300-300-MOVE-NEW-ADL THRU 1300-300-EXIT        GC0078  
00332           GO TO 1300-EXIT.                                        GC0078  
00333       IF IN-TAB-ID = '#AOL  '                                     GC0078  
00334           PERFORM 1300-400-MOVE-NEW-AOL THRU 1300-400-EXIT.       GC0078  
00335  1300-EXIT.                                                       GC0078  
00336      EXIT.                                                        GC0078  
00337                                                                   GC0078  
00338  1300-100-MOVE-NEW-ABM.                                           GC0078  
00339      MOVE LOW-VALUES       TO OUTPUT-ABM-REC.                     GC0078  
00340      MOVE 'N'              TO WRK-SIGNAL-BATCH-INTERNAL.          GC0078  
00341      MOVE WORK-RECORD      TO ABM-WORK-KEY.                       GC0078  
00342      MOVE IN-ENTRY-COUNT   TO GAA-ENTRY-COUNT.                    GC0078  
00343      MOVE IN-FIXED-PORTION TO GAA-FIXED-PORTION.                  GC0078  
00344      MOVE WS-LAST-SLOT     TO GAA-PROVISION-SLOT-NO.              GC0078  
00345      SET GAA-INDEX                                                GC0078  
00346          NON-COMP-INDEX TO 1.                                     GC0078  
00347      PERFORM R400-MOVE-ABM-OCCURS THRU R400-EXIT                  GC0078  
00348          VARYING COMP-INDEX FROM 1 BY 1                           GC0078  
00349          UNTIL   COMP-INDEX  >  IN-ENTRY-COUNT.                   GC0078  
00350      PERFORM R800-WRITE-ABM THRU R800-EXIT.                       GC0078  
00351                                                                   GC0078  
00352  1300-100-EXIT.                                                   GC0078  
00353      EXIT.                                                        GC0078  
00354                                                                   GC0078  
00355  1300-200-MOVE-NEW-ACL.                                           GC0078  
00356      MOVE LOW-VALUES       TO OUTPUT-ACL-REC.                     GC0078  
00357      MOVE 'N'              TO WRK-SIGNAL-BATCH-INTERNAL.          GC0078  
00358      MOVE WORK-RECORD      TO ACL-WORK-KEY.                       GC0078  
00359      MOVE IN-ENTRY-COUNT   TO GAB-ENTRY-COUNT.                    GC0078  
00360      MOVE IN-FIXED-PORTION TO GAB-FIXED-PORTION.                  GC0078  
00361      MOVE WS-LAST-SLOT     TO GAB-PROVISION-SLOT-NO.              GC0078  
00362      SET GAB-INDEX                                                GC0078  
00363          NON-COMP-INDEX TO 1.                                     GC0078  
00364      PERFORM R500-MOVE-ACL-OCCURS THRU R500-EXIT                  GC0078  
00365          VARYING COMP-INDEX FROM 1 BY 1                           GC0078  
00366          UNTIL   COMP-INDEX  >  IN-ENTRY-COUNT.                   GC0078  
00367      PERFORM R900-WRITE-ACL THRU R900-EXIT.                       GC0078  
00368                                                                   GC0078  
00369  1300-200-EXIT.                                                   GC0078  
00370      EXIT.                                                        GC0078  
00371                                                                   GC0078  
00372  1300-300-MOVE-NEW-ADL.                                           GC0078  
00373      MOVE LOW-VALUES       TO OUTPUT-ADL-REC.                     GC0078  
00374      MOVE 'N'              TO WRK-SIGNAL-BATCH-INTERNAL.          GC0078  
00375      MOVE WORK-RECORD      TO ADL-WORK-KEY.                       GC0078  
00376      MOVE IN-ENTRY-COUNT   TO GAC-ENTRY-COUNT.                    GC0078  
00377      MOVE IN-FIXED-PORTION TO GAC-FIXED-PORTION.                  GC0078  
00378      MOVE WS-LAST-SLOT     TO GAC-PROVISION-SLOT-NO.              GC0078  
00379      SET GAC-INDEX                                                GC0078  
00380          NON-COMP-INDEX TO 1.                                     GC0078  
00381      PERFORM R600-MOVE-ADL-OCCURS THRU R600-EXIT                  GC0078  
00382          VARYING COMP-INDEX FROM 1 BY 1                           GC0078  
00383          UNTIL   COMP-INDEX  >  IN-ENTRY-COUNT.                   GC0078  
00384      PERFORM R1000-WRITE-ADL THRU R1000-EXIT.                     GC0078  
00385                                                                   GC0078  
00386  1300-300-EXIT.                                                   GC0078  
00387      EXIT.                                                        GC0078  
00388                                                                   GC0078  
00389  1300-400-MOVE-NEW-AOL.                                           GC0078  
00390      MOVE LOW-VALUES       TO OUTPUT-AOL-REC.                     GC0078  
00391      MOVE 'N'              TO WRK-SIGNAL-BATCH-INTERNAL.          GC0078  
00392      MOVE WORK-RECORD      TO AOL-WORK-KEY.                       GC0078  
00393      MOVE IN-ENTRY-COUNT   TO GAD-ENTRY-COUNT.                    GC0078  
00394      MOVE IN-FIXED-PORTION TO GAD-FIXED-PORTION.                  GC0078  
00395      MOVE WS-LAST-SLOT     TO GAD-PROVISION-SLOT-NO.              GC0078  
00396      SET GAD-INDEX                                                GC0078  
00397          NON-COMP-INDEX TO 1.                                     GC0078  
00398      PERFORM R700-MOVE-AOL-OCCURS THRU R700-EXIT                  GC0078  
00399          VARYING COMP-INDEX FROM 1 BY 1                           GC0078  
00400          UNTIL   COMP-INDEX  >  IN-ENTRY-COUNT.                   GC0078  
00401      PERFORM R1100-WRITE-AOL THRU R1100-EXIT.                     GC0078  
00402                                                                   GC0078  
00403  1300-400-EXIT.                                                   GC0078  
00404      EXIT.                                                        GC0078  
00405                                                                   GC0078  
00406 /                                                                 GC0078  
00407  R100-OPEN.                                                       GC0078  
00408                                                                   GC0078  
00409      OPEN INPUT  INPUT-FILE,                                      GC0078  
00410           OUTPUT OUTPUT-FILE.                                     GC0078  
00411                                                                   GC0078  
00412  R100-EXIT.                                                       GC0078  
00413      EXIT.                                                        GC0078  
00414                                                                   GC0078  
00415  R200-READ.                                                       GC0078  
00416                                                                   GC0078  
00417      READ INPUT-FILE AT END                                       GC0078  
00418          MOVE '1' TO WS-EOF-SW.                                   GC0078  
00419                                                                   GC0078  
00420  R200-EXIT.                                                       GC0078  
00421      EXIT.                                                        GC0078  
00422                                                                   GC0078  
00423  R300-CLOSE.                                                      GC0078  
00424                                                                   GC0078  
00425      CLOSE INPUT-FILE,                                            GC0078  
00426            OUTPUT-FILE.                                           GC0078  
00427                                                                   GC0078  
00428  R300-EXIT.                                                       GC0078  
00429      EXIT.                                                        GC0078  
00430                                                                   GC0078  
00431  R400-MOVE-ABM-OCCURS.                                            GC0078  
00432      MOVE IN-COMP-TAB (COMP-INDEX) TO GAA-ENTRY (GAA-INDEX).      GC0078  
00433      MOVE IN-NON-COMP-TAB (NON-COMP-INDEX) TO                     GC0078  
00434                                 GAA-NON-COMPARE-AREA (GAA-INDEX). GC0078  
00435      SET GAA-INDEX                                                GC0078  
00436          NON-COMP-INDEX UP BY 1.                                  GC0078  
00437  R400-EXIT.                                                       GC0078  
00438      EXIT.                                                        GC0078  
00439                                                                   GC0078  
00440  R410-MOVE-ABM-OCCURS.                                            GC0078  
00441      MOVE WS-COMP-TAB (WS-COMP-INDEX) TO GAA-ENTRY (GAA-INDEX).   GC0078  
00442      MOVE WS-NON-COMP-TAB (WS-NON-COMP-INDEX) TO                  GC0078  
00443                                 GAA-NON-COMPARE-AREA (GAA-INDEX). GC0078  
00444      SET GAA-INDEX                                                GC0078  
00445          WS-NON-COMP-INDEX UP BY 1.                               GC0078  
00446  R410-EXIT.                                                       GC0078  
00447      EXIT.                                                        GC0078  
00448                                                                   GC0078  
00449  R500-MOVE-ACL-OCCURS.                                            GC0078  
00450      MOVE IN-COMP-TAB (COMP-INDEX) TO GAB-ENTRY (GAB-INDEX).      GC0078  
00451      MOVE IN-NON-COMP-TAB (NON-COMP-INDEX) TO                     GC0078  
00452                                 GAB-NON-COMPARE-AREA (GAB-INDEX). GC0078  
00453      SET GAB-INDEX                                                GC0078  
00454          NON-COMP-INDEX UP BY 1.                                  GC0078  
00455  R500-EXIT.                                                       GC0078  
00456      EXIT.                                                        GC0078  
00457                                                                   GC0078  
00458  R510-MOVE-ACL-OCCURS.                                            GC0078  
00459      MOVE WS-COMP-TAB (WS-COMP-INDEX) TO GAB-ENTRY (GAB-INDEX).   GC0078  
00460      MOVE WS-NON-COMP-TAB (WS-NON-COMP-INDEX) TO                  GC0078  
00461                                 GAB-NON-COMPARE-AREA (GAB-INDEX). GC0078  
00462      SET GAB-INDEX                                                GC0078  
00463          WS-NON-COMP-INDEX UP BY 1.                               GC0078  
00464  R510-EXIT.                                                       GC0078  
00465      EXIT.                                                        GC0078  
00466                                                                   GC0078  
00467  R600-MOVE-ADL-OCCURS.                                            GC0078  
00468      MOVE IN-COMP-TAB (COMP-INDEX) TO GAC-ENTRY (GAC-INDEX).      GC0078  
00469      MOVE IN-NON-COMP-TAB (NON-COMP-INDEX) TO                     GC0078  
00470                                 GAC-NON-COMPARE-AREA (GAC-INDEX). GC0078  
00471      SET GAC-INDEX                                                GC0078  
00472          NON-COMP-INDEX UP BY 1.                                  GC0078  
00473  R600-EXIT.                                                       GC0078  
00474      EXIT.                                                        GC0078  
00475                                                                   GC0078  
00476  R610-MOVE-ADL-OCCURS.                                            GC0078  
00477      MOVE WS-COMP-TAB (WS-COMP-INDEX) TO GAC-ENTRY (GAC-INDEX).   GC0078  
00478      MOVE WS-NON-COMP-TAB (WS-NON-COMP-INDEX) TO                  GC0078  
00479                                 GAC-NON-COMPARE-AREA (GAC-INDEX). GC0078  
00480      SET GAC-INDEX                                                GC0078  
00481          WS-NON-COMP-INDEX UP BY 1.                               GC0078  
00482  R610-EXIT.                                                       GC0078  
00483      EXIT.                                                        GC0078  
00484                                                                   GC0078  
00485  R700-MOVE-AOL-OCCURS.                                            GC0078  
00486      MOVE IN-COMP-TAB (COMP-INDEX) TO GAD-ENTRY (GAD-INDEX).      GC0078  
00487      MOVE IN-NON-COMP-TAB (NON-COMP-INDEX) TO                     GC0078  
00488                                 GAD-NON-COMPARE-AREA (GAD-INDEX). GC0078  
00489      SET GAD-INDEX                                                GC0078  
00490          NON-COMP-INDEX UP BY 1.                                  GC0078  
00491  R700-EXIT.                                                       GC0078  
00492      EXIT.                                                        GC0078  
00493                                                                   GC0078  
00494  R710-MOVE-AOL-OCCURS.                                            GC0078  
00495      MOVE WS-COMP-TAB (WS-COMP-INDEX) TO GAD-ENTRY (GAD-INDEX).   GC0078  
00496      MOVE WS-NON-COMP-TAB (WS-NON-COMP-INDEX) TO                  GC0078  
00497                                 GAD-NON-COMPARE-AREA (GAD-INDEX). GC0078  
00498      SET GAD-INDEX                                                GC0078  
00499          WS-NON-COMP-INDEX UP BY 1.                               GC0078  
00500  R710-EXIT.                                                       GC0078  
00501      EXIT.                                                        GC0078  
00502                                                                   GC0078  
00503  R800-WRITE-ABM.                                                  GC0078  
00504      WRITE OUTPUT-ABM-REC.                                        GC0078  
00505  R800-EXIT.                                                       GC0078  
00506      EXIT.                                                        GC0078  
00507                                                                   GC0078  
00508  R900-WRITE-ACL.                                                  GC0078  
00509      WRITE OUTPUT-ACL-REC.                                        GC0078  
00510  R900-EXIT.                                                       GC0078  
00511      EXIT.                                                        GC0078  
00512                                                                   GC0078  
00513  R1000-WRITE-ADL.                                                 GC0078  
00514      WRITE OUTPUT-ADL-REC.                                        GC0078  
00515  R1000-EXIT.                                                      GC0078  
00516      EXIT.                                                        GC0078  
00517                                                                   GC0078  
00518  R1100-WRITE-AOL.                                                 GC0078  
00519      WRITE OUTPUT-AOL-REC.                                        GC0078  
00520  R1100-EXIT.                                                      GC0078  
00521      EXIT.                                                        GC0078  
