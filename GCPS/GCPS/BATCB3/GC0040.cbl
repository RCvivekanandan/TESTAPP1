00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID. GC0040.                                              GC0040  
00003  AUTHOR.        ED WITKUS.                                           LV002
00004  INSTALLATION.  HCSC.                                             GC0040  
00005  DATE-WRITTEN.  OCTOBER, 1987.                                    GC0040  
00006                                                                   GC0040  
00007 ******************************************************************GC0040  
00008 **  THIS PROGRAM READS THE INTERNAL TABULAR FILE CREATED IN       GC0040  
00009 **  GCD01A. FOR EACH DIFFERENT TABULAR ID FOUND,                  GC0040  
00010 **  ALL EXISTING RECORDS WITH THE SAME ID ON THE TABULAR FILE WILLGC0040  
00011 **  BE WRITTEN, INCLUDING ONE THAT CONTAINS ONLY THE LAST ID AND  GC0040  
00012 **  SLOT NBR. THIS PROGRAM WILL END WHEN ALL THREE ID RECORD      GC0040  
00013 **  TYPES HAVE BEEN ENCOUNTERED, OR WHEN THERE IS AN EOF ON THE   GC0040  
00014 **  INPUT FILE. THE OUTPUT FILE WILL BE SORTED IN THE NEXT STEP   GC0040  
00015 **  AND PASSED TO THE MATCH/UPDATE PROGRAM(GC0050).               GC0040  
00016 ******************************************************************GC0040  
00017 *   IN-INTERNAL-TAB-FILE  -  SEQ.                                 GC0040  
00018 *   OUT-INTERNAL-TAB-FILE -  SEQ.                                 GC0040  
00019 *   TSGVSAM1 - TABULAR FILE - VSAM.                               GC0040  
00020 ******************************************************************GC0040  
00021 ******************************************************************GC0040  
00022 *                     U P D A T E  L O G                          GC0040  
00023 ******************************************************************GC0040  
00024 * LOG#    DATE    BY                  DESCRIPTION                 GC0040  
00025 * ----- --------  ---  -------------------------------------------GC0040  
00026 * D1009 10/01/87  ENW  CREATED.                                   GC0040  
00027 * D184/ 12/13/89  ENW  ADDED NEW INTERNAL TABULARS #IDGD AND #IPGPGC0040  
00028 * D185                                                            GC0040  
00029 *                                                                 GC0040  
00030 * 11138 05/23/90  ENW  ADDED CODE TO BYPASS RECORDS WHERE SLOT    GC0040  
00031 *                      NUMBERS ARE GREATER THAN +99999. RECORDS   GC0040  
00032 *                      WITH SLOTS GREATER THAN +99999 ARE FOR     GC0040  
00033 *                      CHAINED RECORDS AND CANNOT BE RUN THROUGH  GC0040  
00034 *                      BATCH.                                     GC0040  
00035 *                                                                 GC0040  
00036 *                       ----ACCUM TABULAR RECORD MODIFICATION--- *GC0040  
00037 * 11154   11/02/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *GC0040  
00038 * D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *GC0040  
00039 * D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *GC0040  
00040 * D270                  4. ADD AGE-LIMIT-FROM AND AGE-LIMIT-TO.  *GC0040  
00041 *                       5. ADD RELATIONSHIP-IND (CDE ELEMENT).   *GC0040  
00042 *                       6. CHANGE TABULAR RECORD MAX LENGTH FROM *GC0040  
00043 *                          4000 TO 8157. (VSAM)                  *GC0040  
00044 *                                                                *GC0040  
00045 *                       ----ACCUM TABULAR RECORD MODIFICATION--- *GC0040  
00046 * 11154    2/21/91  FRY   -DECREASE MAX OCCURS FROM 46 TO 44.    *GC0040  
00047 *                         -DECREASE MAX REC LENGTH FOR ACCUM REC *GC0040  
00048 *                           FROM 8157 TO 7805. (VSAM)            *GC0040  
00049 *                                                                *GC0040  
00050 * 11154    3/06/91  FRY   -INCREASE RECORD AREAS IN FILE SECTION:*GC0040  
00051 *               IN-INT-TAB-RECORD                                *GC0040  
00052 *                            FILLER  PIC X(3994) CHANGED TO 7799 *GC0040  
00053 *                OUT-INT-TAB-RECORD  PIC X(4064) CHANGED TO 7869 *GC0040  
00054 *                                                                *GC0040  
00055 * D-356A   5/08/03  GTF  RECOMPILE FOR COPYBK CHANGES #IPGP,     *GC0040  
00056 *                        #IDGD.                                  *GC0040  
00057 ******************************************************************GC0040  
00058                                                                   GC0040  
00059  ENVIRONMENT DIVISION.                                            GC0040  
00060                                                                   GC0040  
00061  CONFIGURATION SECTION.                                           GC0040  
00062  SOURCE-COMPUTER.  IBM-370.                                       GC0040  
00063  OBJECT-COMPUTER.  IBM-370.                                       GC0040  
00064  INPUT-OUTPUT SECTION.                                            GC0040  
00065                                                                   GC0040  
00066  FILE-CONTROL.                                                    GC0040  
00067      SELECT IN-INT-TAB-FILE           ASSIGN  TO UT-S-GC0040A.    GC0040  
00068      SELECT OUT-INT-TAB-FILE          ASSIGN  TO UT-S-GC0040B.    GC0040  
00069  DATA DIVISION.                                                   GC0040  
00070                                                                   GC0040  
00071  FILE SECTION.                                                    GC0040  
00072                                                                   GC0040  
00073  FD  IN-INT-TAB-FILE                                              GC0040  
00074          LABEL RECORDS ARE STANDARD                               GC0040  
00075          RECORDING MODE IS V                                      GC0040  
00076          BLOCK CONTAINS  0  RECORDS.                              GC0040  
00077                                                                   GC0040  
00078  01  IN-INT-TAB-RECORD.                                           GC0040  
00079      05 FILLER                            PIC X(64).              GC0040  
00080      05 WRK-BEN-TAB-PROV-DATA.                                    GC0040  
00081          10 WRK-TAB-PROV-ID               PIC X(6).               GC0040  
00082          10 FILLER                        PIC X(7799).            GC0040  
00083                                                                   GC0040  
00084  01  GX1B-REC2.                                                   GC0040  
00085      05 FILLER                         PIC X(64).                 GC0040  
00086      COPY  GCTIBGR2.                                              GC0040  
00087  01  GX2B-REC2.                                                   GC0040  
00088      05 FILLER                         PIC X(64).                 GC0040  
00089      COPY  GCTIPGN2.                                              GC0040  
00090  01  GX3B-REC2.                                                   GC0040  
00091      05 FILLER                         PIC X(64).                 GC0040  
00092      COPY  GCTIPGT2.                                              GC0040  
00093  01  GX9B-REC2.                                                   GC0040  
00094      05 FILLER                         PIC X(64).                 GC0040  
00095      COPY  GCTIDGD2.                                              GC0040  
00096  01  GXAB-REC2.                                                   GC0040  
00097      05 FILLER                         PIC X(64).                 GC0040  
00098      COPY  GCTIPGP2.                                              GC0040  
00099 /                                                                 GC0040  
00100  FD  OUT-INT-TAB-FILE                                             GC0040  
00101          LABEL RECORDS ARE STANDARD                               GC0040  
00102          RECORDING MODE IS V                                      GC0040  
00103          BLOCK CONTAINS  0  RECORDS.                              GC0040  
00104                                                                   GC0040  
00105  01  OUT-INT-TAB-RECORD               PIC X(7869).                GC0040  
00106  01  OUT-INT-TAB-LAST-KEY-REC.                                    GC0040  
00107      05 FILLER                        PIC X(64).                  GC0040  
00108      05 OUT-INT-TAB-KEY.                                          GC0040  
00109         10  OUT-INT-TAB-LAST-KEY-ID   PIC X(6).                   GC0040  
00110         10  OUT-INT-TAB-LAST-KEY-SLOT PIC S9(7) COMP-3.           GC0040  
00111                                                                   GC0040  
00112  01  GX1-REC.                                                     GC0040  
00113      05 FILLER                        PIC X(64).                  GC0040  
00114      COPY  GCTIBGRC.                                              GC0040  
00115  01  GX2-REC.                                                     GC0040  
00116      05 FILLER                        PIC X(64).                  GC0040  
00117      COPY  GCTIPGNC.                                              GC0040  
00118  01  GX3-REC.                                                     GC0040  
00119      05 FILLER                        PIC X(64).                  GC0040  
00120      COPY  GCTIPGTC.                                              GC0040  
00121  01  GX9-REC.                                                     GC0040  
00122      05 FILLER                        PIC X(64).                  GC0040  
00123      COPY  GCTIDGDC.                                              GC0040  
00124  01  GXA-REC.                                                     GC0040  
00125      05 FILLER                        PIC X(64).                  GC0040  
00126      COPY  GCTIPGPC.                                              GC0040  
00127 /                                                                 GC0040  
00128  WORKING-STORAGE SECTION.                                         GC0040  
00129  77  FILLER                      PIC  X(24) VALUE                 GC0040  
00130      'GC0040 WORKING STORAGE  '.                                  GC0040  
00131                                                                   GC0040  
00132  01  ABEND-CODE                    PIC 9(4)       COMP.           GC0040  
00133                                                                   GC0040  
00134  01  WS-MISC-AREA.                                                GC0040  
00135      05  LAST-ID                   PIC X(6)   VALUE SPACES.       GC0040  
00136      05  LAST-SLOT    COMP-3       PIC S9(7)  VALUE +0.           GC0040  
00137                                                                   GC0040  
00138  01  WS-SWITCHES.                                                 GC0040  
00139      05  WS-EOF-SW                 PIC X      VALUE '0'.          GC0040  
00140          88  EOF                              VALUE '1'.          GC0040  
00141      05  WS-READ-SW                PIC X      VALUE '0'.          GC0040  
00142      05  WS-IBGR-SW                PIC X      VALUE '0'.          GC0040  
00143      05  WS-IDGD-SW                PIC X      VALUE '0'.          GC0040  
00144      05  WS-IPGN-SW                PIC X      VALUE '0'.          GC0040  
00145      05  WS-IPGP-SW                PIC X      VALUE '0'.          GC0040  
00146      05  WS-IPGT-SW                PIC X      VALUE '0'.          GC0040  
00147                                                                   GC0040  
00148  01  PARM-ONE.                                                    GC0040  
00149      05 RESERVED-FLDS              PIC 9(8)   VALUE ZEROS COMP.   GC0040  
00150      05 RESERVED-ONE  REDEFINES  RESERVED-FLDS.                   GC0040  
00151          10 REQUEST-TYPE           PIC X.                         GC0040  
00152          10 FILLER                 PIC X(3).                      GC0040  
00153                                                                   GC0040  
00154  01  PARM-TWO.                                                    GC0040  
00155      05 RDW.                                                      GC0040  
00156          10 RECORD-LENGTH          PIC 9(4)   COMP.               GC0040  
00157          10 FEEDBACK-CODE          PIC 9(4)   COMP.               GC0040  
00158      05  REC-AREA                  PIC X(7805).                   GC0040  
00159      05  RECORD-A    REDEFINES  REC-AREA.                         GC0040  
00160          10 KEY-FIELD.                                            GC0040  
00161              15  KEY-ID            PIC X(6).                      GC0040  
00162              15  KEY-SLOT          PIC S9(7) COMP-3.              GC0040  
00163          10 KEY-DATA.                                             GC0040  
00164              15  FILLER            PIC X(27).                     GC0040  
00165              15  KEY-ENTRY-COUNT   PIC S9(5) COMP-3.              GC0040  
00166              15  FILLER            PIC X(7765).                   GC0040  
00167                                                                   GC0040  
00168  01  PARM-SET.                                                    GC0040  
00169      05 SET-RDW.                                                  GC0040  
00170          10 SET-RECORD-LENGTH      PIC 9(4)       COMP.           GC0040  
00171          10 SET-FEEDBACK-CODE      PIC 9(4)       COMP.           GC0040  
00172      05  SET-VALUE                 PIC 9(8)       COMP.           GC0040  
00173 /                                                                 GC0040  
00174  PROCEDURE DIVISION.                                              GC0040  
00175                                                                   GC0040  
00176  A000-MAINLINE.                                                   GC0040  
00177      PERFORM 0100-OPEN THRU 0100-EXIT.                            GC0040  
00178      PERFORM 1000-PROCESS THRU 1000-EXIT                          GC0040  
00179          UNTIL EOF.                                               GC0040  
00180      PERFORM 0200-CLOSE THRU 0200-EXIT.                           GC0040  
00181      STOP RUN.                                                    GC0040  
00182                                                                   GC0040  
00183  0100-OPEN.                                                       GC0040  
00184      MOVE  'S'          TO  REQUEST-TYPE.                         GC0040  
00185      MOVE   8           TO  SET-RECORD-LENGTH.                    GC0040  
00186      MOVE   3           TO  SET-VALUE.                            GC0040  
00187      CALL  'TSGVSAM1'  USING  PARM-ONE  PARM-SET.                 GC0040  
00188                                                                   GC0040  
00189      IF  REQUEST-TYPE  NOT =  'S'                                 GC0040  
00190          DISPLAY 'BAD SET IN GC0040 AT 0100-OPEN'                 GC0040  
00191          MOVE  SET-FEEDBACK-CODE  TO  ABEND-CODE                  GC0040  
00192          GO TO  9999-ERROR-RTN.                                   GC0040  
00193                                                                   GC0040  
00194      OPEN INPUT IN-INT-TAB-FILE,                                  GC0040  
00195           OUTPUT OUT-INT-TAB-FILE.                                GC0040  
00196                                                                   GC0040  
00197  0100-EXIT.                                                       GC0040  
00198      EXIT.                                                        GC0040  
00199                                                                   GC0040  
00200  0200-CLOSE.                                                      GC0040  
00201                                                                   GC0040  
00202      CLOSE IN-INT-TAB-FILE,                                       GC0040  
00203            OUT-INT-TAB-FILE.                                      GC0040  
00204                                                                   GC0040  
00205      MOVE     'C'           TO   REQUEST-TYPE.                    GC0040  
00206      CALL  'TSGVSAM1'  USING  PARM-ONE  PARM-TWO.                 GC0040  
00207                                                                   GC0040  
00208      IF  REQUEST-TYPE  NOT =  'C'                                 GC0040  
00209          DISPLAY 'BAD CLOSE IN GC0040 AT 0200-CLOSE'              GC0040  
00210          MOVE  FEEDBACK-CODE  TO  ABEND-CODE                      GC0040  
00211          GO TO  9999-ERROR-RTN.                                   GC0040  
00212                                                                   GC0040  
00213  0200-EXIT.                                                       GC0040  
00214      EXIT.                                                        GC0040  
00215 /                                                                 GC0040  
00216  1000-PROCESS.                                                    GC0040  
00217 ****                                                              GC0040  
00218 *  WRITE ALL EXISTING INTERNAL TABULAR RECORDS TO THE OUTPUT      GC0040  
00219 *  FILE FOR EACH RECORD TYPE ENCOUNTERED.                         GC0040  
00220 ****                                                              GC0040  
00221      READ IN-INT-TAB-FILE AT END                                  GC0040  
00222          MOVE '1' TO WS-EOF-SW                                    GC0040  
00223          GO TO 1000-EXIT.                                         GC0040  
00224                                                                   GC0040  
00225      IF WRK-TAB-PROV-ID = '#IBGR '                                GC0040  
00226          IF WS-IBGR-SW > '0'                                      GC0040  
00227              GO TO 1000-EXIT                                      GC0040  
00228          ELSE                                                     GC0040  
00229              MOVE 1 TO WS-IBGR-SW                                 GC0040  
00230              PERFORM 1100-WRITE-ALL-EXISTING THRU 1100-EXIT.      GC0040  
00231                                                                   GC0040  
00232      IF WRK-TAB-PROV-ID = '#IDGD '                                GC0040  
00233          IF WS-IDGD-SW > '0'                                      GC0040  
00234              GO TO 1000-EXIT                                      GC0040  
00235          ELSE                                                     GC0040  
00236              MOVE 1 TO WS-IDGD-SW                                 GC0040  
00237              PERFORM 1100-WRITE-ALL-EXISTING THRU 1100-EXIT.      GC0040  
00238                                                                   GC0040  
00239      IF WRK-TAB-PROV-ID = '#IPGN '                                GC0040  
00240          IF WS-IPGN-SW > '0'                                      GC0040  
00241              GO TO 1000-EXIT                                      GC0040  
00242          ELSE                                                     GC0040  
00243              MOVE 1 TO WS-IPGN-SW                                 GC0040  
00244              PERFORM 1100-WRITE-ALL-EXISTING THRU 1100-EXIT.      GC0040  
00245                                                                   GC0040  
00246      IF WRK-TAB-PROV-ID = '#IPGP '                                GC0040  
00247          IF WS-IPGP-SW > '0'                                      GC0040  
00248              GO TO 1000-EXIT                                      GC0040  
00249          ELSE                                                     GC0040  
00250              MOVE 1 TO WS-IPGP-SW                                 GC0040  
00251              PERFORM 1100-WRITE-ALL-EXISTING THRU 1100-EXIT.      GC0040  
00252                                                                   GC0040  
00253      IF WRK-TAB-PROV-ID = '#IPGT '                                GC0040  
00254          IF WS-IPGT-SW > '0'                                      GC0040  
00255              GO TO 1000-EXIT                                      GC0040  
00256          ELSE                                                     GC0040  
00257              MOVE 1 TO WS-IPGT-SW                                 GC0040  
00258              PERFORM 1100-WRITE-ALL-EXISTING THRU 1100-EXIT.      GC0040  
00259                                                                   GC0040  
00260 ****                                                              GC0040  
00261 *  CHECK TO SEE IF ALL RECORD TYPES WERE WRITTEN ALREADY.         GC0040  
00262 *  IF SO, SET SWITCH AND GET OUT OF ROUTINE.                      GC0040  
00263 ****                                                              GC0040  
00264      IF WS-IBGR-SW > '0'                                          GC0040  
00265        AND                                                        GC0040  
00266         WS-IDGD-SW > '0'                                          GC0040  
00267        AND                                                        GC0040  
00268         WS-IPGN-SW > '0'                                          GC0040  
00269        AND                                                        GC0040  
00270         WS-IPGP-SW > '0'                                          GC0040  
00271        AND                                                        GC0040  
00272         WS-IPGT-SW > '0'                                          GC0040  
00273          MOVE '1' TO WS-EOF-SW.                                   GC0040  
00274                                                                   GC0040  
00275  1000-EXIT.                                                       GC0040  
00276      EXIT.                                                        GC0040  
00277                                                                   GC0040  
00278  1100-WRITE-ALL-EXISTING.                                         GC0040  
00279      MOVE WRK-TAB-PROV-ID TO KEY-ID                               GC0040  
00280                              LAST-ID.                             GC0040  
00281      MOVE +10             TO LAST-SLOT.                           GC0040  
00282      MOVE +11             TO KEY-SLOT.                            GC0040  
00283      MOVE 14              TO RECORD-LENGTH.                       GC0040  
00284      MOVE 'P'             TO REQUEST-TYPE.                        GC0040  
00285      PERFORM 1200-POINT THRU 1200-EXIT.                           GC0040  
00286      MOVE '0' TO WS-READ-SW.                                      GC0040  
00287      PERFORM 1300-READNEXT THRU 1300-EXIT                         GC0040  
00288          UNTIL WS-READ-SW > '0'.                                  GC0040  
00289 *****                                                             GC0040  
00290 *  WRITE THE LAST SLOT READ. THE LOW-VALUES IN THE BODY OF        GC0040  
00291 *  THE RECORD WILL CAUSE IT TO SORT FIRST. THE MATCH/UPDATE       GC0040  
00292 *  PROGRAM WILL USE THIS TO ASSIGN NEW SLOT NUMBERS.              GC0040  
00293 *****                                                             GC0040  
00294      MOVE LOW-VALUES TO OUT-INT-TAB-RECORD.                       GC0040  
00295      MOVE WRK-TAB-PROV-ID TO OUT-INT-TAB-LAST-KEY-ID.             GC0040  
00296      MOVE LAST-SLOT       TO OUT-INT-TAB-LAST-KEY-SLOT.           GC0040  
00297      WRITE OUT-INT-TAB-LAST-KEY-REC.                              GC0040  
00298  1100-EXIT.                                                       GC0040  
00299      EXIT.                                                        GC0040  
00300                                                                   GC0040  
00301  1200-POINT.                                                      GC0040  
00302      CALL  'TSGVSAM1'  USING  PARM-ONE  PARM-TWO.                 GC0040  
00303      IF  REQUEST-TYPE  NOT =  'P'                                 GC0040  
00304          DISPLAY 'BAD POINT IN GC0040 AT 1200-POINT'              GC0040  
00305          MOVE  FEEDBACK-CODE  TO  ABEND-CODE                      GC0040  
00306          GO TO  9999-ERROR-RTN.                                   GC0040  
00307  1200-EXIT.                                                       GC0040  
00308      EXIT.                                                        GC0040  
00309                                                                   GC0040  
00310  1300-READNEXT.                                                   GC0040  
00311                                                                   GC0040  
00312      MOVE  'G'          TO  REQUEST-TYPE.                         GC0040  
00313                                                                   GC0040  
00314      CALL  'TSGVSAM1'  USING  PARM-ONE  PARM-TWO.                 GC0040  
00315                                                                   GC0040  
00316      IF  REQUEST-TYPE = '2'                                       GC0040  
00317          MOVE '1' TO WS-READ-SW                                   GC0040  
00318          GO TO 1300-EXIT.                                         GC0040  
00319                                                                   GC0040  
00320      IF  REQUEST-TYPE  NOT = 'G'                                  GC0040  
00321          DISPLAY 'BAD GET   IN GC0040 AT 1300-READNEXT'           GC0040  
00322          MOVE  FEEDBACK-CODE  TO  ABEND-CODE                      GC0040  
00323          GO TO  9999-ERROR-RTN.                                   GC0040  
00324                                                                   GC0040  
00325      IF KEY-ID NOT = WRK-TAB-PROV-ID                              GC0040  
00326          MOVE '1' TO WS-READ-SW                                   GC0040  
00327          GO TO 1300-EXIT.                                         GC0040  
00328                                                                   GC0040  
00329 ****************************************************              GC0040  
00330 **  ADDED 05/23/90 BY ENW FOR PROJ. NUM. 11138.                   GC0040  
00331 **  BYPASS SPECIAL \
00332 **  IS GREATER THAN +99999.                                       GC0040  
00333                                                                   GC0040  
00334      IF KEY-SLOT > +99999                                         GC0040  
00335          GO TO 1300-EXIT.                                         GC0040  
00336                                                                   GC0040  
00337 **  END OF MODIFICATION FOR PROJ. NUM. 11138.                     GC0040  
00338 ****************************************************              GC0040  
00339      MOVE KEY-SLOT TO LAST-SLOT.                                  GC0040  
00340                                                                   GC0040  
00341      MOVE LOW-VALUES TO OUT-INT-TAB-RECORD.                       GC0040  
00342                                                                   GC0040  
00343      IF KEY-ID = '#IBGR '                                         GC0040  
00344          MOVE KEY-ENTRY-COUNT TO GX1-ENTRY-COUNT                  GC0040  
00345          MOVE REC-AREA TO GX1-RECORD                              GC0040  
00346          WRITE GX1-REC.                                           GC0040  
00347                                                                   GC0040  
00348      IF KEY-ID = '#IPGN '                                         GC0040  
00349          MOVE KEY-ENTRY-COUNT TO GX2-ENTRY-COUNT                  GC0040  
00350          MOVE REC-AREA TO GX2-RECORD                              GC0040  
00351          WRITE GX2-REC.                                           GC0040  
00352                                                                   GC0040  
00353      IF KEY-ID = '#IPGT '                                         GC0040  
00354          MOVE KEY-ENTRY-COUNT TO GX3-ENTRY-COUNT                  GC0040  
00355          MOVE REC-AREA TO GX3-RECORD                              GC0040  
00356          WRITE GX3-REC.                                           GC0040  
00357                                                                   GC0040  
00358      IF KEY-ID = '#IDGD '                                         GC0040  
00359          MOVE KEY-ENTRY-COUNT TO GX9-ENTRY-COUNT                  GC0040  
00360          MOVE REC-AREA TO GX9-RECORD                              GC0040  
00361          WRITE GX9-REC.                                           GC0040  
00362                                                                   GC0040  
00363      IF KEY-ID = '#IPGP '                                         GC0040  
00364          MOVE KEY-ENTRY-COUNT TO GXA-ENTRY-COUNT                  GC0040  
00365          MOVE REC-AREA TO GXA-RECORD                              GC0040  
00366          WRITE GXA-REC.                                           GC0040  
00367                                                                   GC0040  
00368  1300-EXIT.                                                       GC0040  
00369      EXIT.                                                        GC0040  
00370                                                                   GC0040  
00371 /                                                                 GC0040  
00372  9999-ERROR-RTN.                                                  GC0040  
00373                                                                   GC0040  
00374      CALL  'TSGEND' USING  ABEND-CODE.                            GC0040  
00375                                                                   GC0040  
00376  9999-ERROR-RTN-EXIT. EXIT.                                       GC0040  
