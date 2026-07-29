00001  IDENTIFICATION DIVISION.                                         08/21/03
00002                                                                   GC03500 
00003  PROGRAM-ID. GC03500.                                                LV001
00004  AUTHOR. MELANIE MOSTACCIO.                                       GC03500 
00005                                                                   GC03500 
00006  INSTALLATION. HCSC-HCMS.                                         GC03500 
00007  DATE-WRITTEN.                                                    GC03500 
00008  DATE-COMPILED.                                                   GC03500 
00009 ******************************************************************GC03500 
00010 *                                                                *GC03500 
00011 *    THIS PROGRAM WILL RANDOMLY PICK THREE GROUP NUMBERS FOR     *GC03500 
00012 *    EACH OPERATOR ID, TO BE USED IN A REPORT PROGRAM.           *GC03500 
00013 *                                                                *GC03500 
00014 *    INPUT FILES:                                                *GC03500 
00015 *       QCF-FILE - QUALITY CONTROL EXTRACT FILE                  *GC03500 
00016 *                                                                *GC03500 
00017 *    OUTPUT FILES:                                               *GC03500 
00018 *       RND-FILE - RANDOMIZED VERSION OF THE QCF-FILE            *GC03500 
00019 *                                                                *GC03500 
00020 ******************************************************************GC03500 
00021 *                                                                *GC03500 
00022 *      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       *GC03500 
00023 *      *-*         U P D A T E   H I S T O R Y         *-*       *GC03500 
00024 *      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       *GC03500 
00025 *                                                                *GC03500 
00026 * CHG #    DATE    BY              DESCRIPTION                   *GC03500 
00027 * _____  ________  ___  ___________________________________      *GC03500 
00028 *        07/27/98  KJD  INCREASE TABLE SIZE TO 1000              *GC03500 
00029 *                                                                *GC03500 
00030 ******************************************************************GC03500 
00031 /                                                                 GC03500 
00032  ENVIRONMENT DIVISION.                                            GC03500 
00033  CONFIGURATION SECTION.                                           GC03500 
00034  SOURCE-COMPUTER. IBM-370.                                        GC03500 
00035  OBJECT-COMPUTER. IBM-370.                                        GC03500 
00036                                                                   GC03500 
00037  INPUT-OUTPUT SECTION.                                            GC03500 
00038  FILE-CONTROL.                                                    GC03500 
00039                                                                   GC03500 
00040      SELECT QCF-FILE   ASSIGN TO UT-S-GC03500A.                   GC03500 
00041      SELECT RND-FILE   ASSIGN TO UT-S-GC03500B.                   GC03500 
00042 /                                                                 GC03500 
00043  DATA DIVISION.                                                   GC03500 
00044  FILE SECTION.                                                    GC03500 
00045                                                                   GC03500 
00046  FD  QCF-FILE                                                     GC03500 
00047      LABEL RECORDS ARE STANDARD                                   GC03500 
00048      RECORDING MODE IS F                                          GC03500 
00049      BLOCK CONTAINS 0 RECORDS.                                    GC03500 
00050                                                                   GC03500 
00051  01  QCF-RECORD.                                                  GC03500 
00052      COPY GCQCF.                                                  GC03500 
00053                                                                   GC03500 
00054  FD  RND-FILE                                                     GC03500 
00055      LABEL RECORDS ARE STANDARD                                   GC03500 
00056      RECORDING MODE IS F                                          GC03500 
00057      BLOCK CONTAINS 0 RECORDS.                                    GC03500 
00058                                                                   GC03500 
00059  01  RND-RECORD.                                                  GC03500 
00060      05  RND-OPERATOR-ID     PIC X(08).                           GC03500 
00061      05  RND-PLAN-CODE       PIC X(03).                           GC03500 
00062      05  RND-GROUP-NO        PIC X(09).                           GC03500 
00063 /                                                                 GC03500 
00064  WORKING-STORAGE SECTION.                                         GC03500 
00065  01  FILLER                      PIC X(24)   VALUE                GC03500 
00066      'GC03500 WORKING-STORAGE'.                                   GC03500 
00067                                                                   GC03500 
00068  01  ABEND-CODE                  PIC 9(4)    COMP.                GC03500 
00069                                                                   GC03500 
00070  01  END-OF-FILE                 PIC XXX   VALUE SPACES.          GC03500 
00071      88  END-OF-QCF-FILE                   VALUE 'END'.           GC03500 
00072                                                                   GC03500 
00073  01  LOOP-FLAG                   PIC X     VALUE SPACES.          GC03500 
00074      88  ONCE-MORE                         VALUE 'Y'.             GC03500 
00075      88  EOF-LOOP                          VALUE 'N'.             GC03500 
00076                                                                   GC03500 
00077  01  TABLE-FLAG                  PIC X     VALUE 'N'.             GC03500 
00078      88  FND-IN-TBL                        VALUE 'Y'.             GC03500 
00079                                                                   GC03500 
00080  01  COUNTER-AREA.                                                GC03500 
00081      05  TABLE-ENTRIES-CNT       PIC S9(9) VALUE ZEROS.           GC03500 
00082      05  WS-QCF-COUNT            PIC S9(9) VALUE ZEROS.           GC03500 
00083      05  COUNTER1                PIC S9(9) VALUE ZEROS.           GC03500 
00084      05  COUNTER2                PIC S9(9) VALUE ZEROS.           GC03500 
00085      05  COUNTER3                PIC S9(9) VALUE ZEROS.           GC03500 
00086                                                                   GC03500 
00087  01  HOLD-AREA.                                                   GC03500 
00088      05  PREVIOUS-GROUP-NO       PIC X(09)  VALUE SPACES.         GC03500 
00089      05  PREVIOUS-OPERATOR       PIC X(08)  VALUE SPACES.         GC03500 
00090      05  MAX-TABLE-SIZE          PIC 9(4)   VALUE 1000.           GC03500 
00091      05  OPERATOR-TABLE    OCCURS 1000 TIMES                      GC03500 
00092                            INDEXED BY TBL-INDX, TBL-INDX2.        GC03500 
00093          10  HLD-PLAN-CODE       PIC X(03).                       GC03500 
00094          10  HLD-GROUP-NO        PIC X(09).                       GC03500 
00095                                                                   GC03500 
00096  01  NUMERICAL-AREA.                                              GC03500 
00097      05  NUM-TIME-TT             PIC 9(02)  VALUE ZEROS.          GC03500 
00098      05  NUM-ANS-AREA            PIC S9(04) VALUE ZEROS.          GC03500 
00099      05  NUM-SUBSCRIPT           PIC 9(04)  VALUE ZEROS.          GC03500 
00100                                                                   GC03500 
00101  01  WS-TIME.                                                     GC03500 
00102      05  WS-TIME-HH              PIC X(02)  VALUE SPACES.         GC03500 
00103      05  WS-TIME-MM              PIC X(02)  VALUE SPACES.         GC03500 
00104      05  WS-TIME-SS              PIC X(02)  VALUE SPACES.         GC03500 
00105      05  WS-TIME-TT              PIC X(02)  VALUE SPACES.         GC03500 
00106 /                                                                 GC03500 
00107  01  WS-REC-LEN-AREA.                                             GC03500 
00108  COPY GCCDRLEN.                                                   GC03500 
00109 /                                                                 GC03500 
00110  PROCEDURE DIVISION.                                              GC03500 
00111 ******************************************************************GC03500 
00112 *                       0010-MAINLINE                            *GC03500 
00113 *                                                                *GC03500 
00114 *  OPENS FILES, DOES THE INTIAL READ OF THE QUALITY CONTROL      *GC03500 
00115 *  FILE, CONTROL THE MAIN LOOP OF THE PROGRAM, AND CLOSES THE    *GC03500 
00116 *  FILES AT THE END OF PROCESSING                                *GC03500 
00117 ******************************************************************GC03500 
00118  0000-MAINLINE.                                                   GC03500 
00119                                                                   GC03500 
00120      OPEN  INPUT  QCF-FILE                                        GC03500 
00121      OPEN  OUTPUT RND-FILE.                                       GC03500 
00122                                                                   GC03500 
00123      PERFORM 0010-READ-QCF-FILE THRU 0010-EXIT.                   GC03500 
00124                                                                   GC03500 
00125      MOVE QCF-OPERATOR-ID TO PREVIOUS-OPERATOR.                   GC03500 
00126      MOVE ZEROS           TO TABLE-ENTRIES-CNT.                   GC03500 
00127                                                                   GC03500 
00128      PERFORM 0015-PROCESS-QCF-FILE THRU 0015-EXIT                 GC03500 
00129          UNTIL EOF-LOOP.                                          GC03500 
00130                                                                   GC03500 
00131      CLOSE  QCF-FILE                                              GC03500 
00132             RND-FILE.                                             GC03500 
00133      GOBACK.                                                      GC03500 
00134                                                                   GC03500 
00135  0000-EXIT.                                                       GC03500 
00136      EXIT.                                                        GC03500 
00137 /                                                                 GC03500 
00138 ******************************************************************GC03500 
00139 *                     0010-READ-QCF-FILE                         *GC03500 
00140 *                                                                *GC03500 
00141 *  READ THE QUALITY CONTROL FILE CREATED FROM GC03310 & GC03320  *GC03500 
00142 ******************************************************************GC03500 
00143  0010-READ-QCF-FILE.                                              GC03500 
00144                                                                   GC03500 
00145      INITIALIZE QCF-QUAL-CTRL-RECORD.                             GC03500 
00146                                                                   GC03500 
00147      READ  QCF-FILE                                               GC03500 
00148            AT END  MOVE 'END'  TO END-OF-FILE                     GC03500 
00149                    GO TO 0010-EXIT.                               GC03500 
00150                                                                   GC03500 
00151  0010-EXIT.                                                       GC03500 
00152       EXIT.                                                       GC03500 
00153 /                                                                 GC03500 
00154 ******************************************************************GC03500 
00155 *                   0015-PROCESS-QCF-FILE                        *GC03500 
00156 *                                                                *GC03500 
00157 *  IN A LOOP, PROCESS THE QUALITY CONTROL FILE.  IF THE GROUP    *GC03500 
00158 *  NUMBER IS DIFFERENT FROM THE PREVIOUS ONE READ IN, THEN THE   *GC03500 
00159 *  TABLE IS CHECKED TO SEE IF IT ALREADY EXISTS.  IF IT DOESN'T  *GC03500 
00160 *  ALREADY EXIST IN THE TABLE, THEN IT IS ADDED.  IF THE         *GC03500 
00161 *  OPERATOR ID IS DIFFERENT FROM THE PREVIOUS ONE, THEN THE      *GC03500 
00162 *  TABLE IS PROCESSED AND RANDOMIZED.                            *GC03500 
00163 ******************************************************************GC03500 
00164  0015-PROCESS-QCF-FILE.                                           GC03500 
00165                                                                   GC03500 
00166      IF QCF-OPERATOR-ID = PREVIOUS-OPERATOR                       GC03500 
00167         IF QCF-GROUP-NO = PREVIOUS-GROUP-NO                       GC03500 
00168            NEXT SENTENCE                                          GC03500 
00169         ELSE                                                      GC03500 
00170            MOVE 'N' TO TABLE-FLAG                                 GC03500 
00171            PERFORM VARYING COUNTER1 FROM 1 BY 1                   GC03500 
00172              UNTIL COUNTER1 > TABLE-ENTRIES-CNT                   GC03500 
00173                 OR FND-IN-TBL OR COUNTER1 > MAX-TABLE-SIZE        GC03500 
00174                SET TBL-INDX2 TO COUNTER1                          GC03500 
00175                IF QCF-GROUP-NO = HLD-GROUP-NO (TBL-INDX2)         GC03500 
00176                   SET FND-IN-TBL TO TRUE                          GC03500 
00177                END-IF                                             GC03500 
00178            END-PERFORM                                            GC03500 
00179            IF FND-IN-TBL                                          GC03500 
00180               CONTINUE                                            GC03500 
00181            ELSE                                                   GC03500 
00182               IF TABLE-ENTRIES-CNT = MAX-TABLE-SIZE               GC03500 
00183                  DISPLAY 'GC03500 ***MAX TABLE ENTRIES REACHED***'GC03500 
00184               ELSE                                                GC03500 
00185                  MOVE QCF-PLAN-CODE  TO HLD-PLAN-CODE (TBL-INDX)  GC03500 
00186                  MOVE QCF-GROUP-NO   TO HLD-GROUP-NO  (TBL-INDX)  GC03500 
00187                  COMPUTE TABLE-ENTRIES-CNT = TABLE-ENTRIES-CNT + 1GC03500 
00188                  SET TBL-INDX UP BY 1                             GC03500 
00189               END-IF                                              GC03500 
00190            END-IF                                                 GC03500 
00191         END-IF                                                    GC03500 
00192      ELSE                                                         GC03500 
00193         IF ONCE-MORE                                              GC03500 
00194            PERFORM 0020-PROCESS-TABLE THRU 0020-EXIT              GC03500 
00195            PERFORM 0025-INITIALIZE-TABLE THRU 0025-EXIT           GC03500 
00196            SET EOF-LOOP TO TRUE                                   GC03500 
00197            GO TO 0015-EXIT                                        GC03500 
00198         ELSE                                                      GC03500 
00199            IF QCF-OPERATOR-ID NOT = PREVIOUS-OPERATOR             GC03500 
00200               PERFORM 0020-PROCESS-TABLE THRU 0020-EXIT           GC03500 
00201               PERFORM 0025-INITIALIZE-TABLE THRU 0025-EXIT        GC03500 
00202               SET TBL-INDX TO 1                                   GC03500 
00203               MOVE QCF-PLAN-CODE  TO HLD-PLAN-CODE (TBL-INDX)     GC03500 
00204               MOVE QCF-GROUP-NO   TO HLD-GROUP-NO  (TBL-INDX)     GC03500 
00205               MOVE +1             TO TABLE-ENTRIES-CNT            GC03500 
00206               SET TBL-INDX UP BY 1                                GC03500 
00207            END-IF                                                 GC03500 
00208         END-IF                                                    GC03500 
00209      END-IF.                                                      GC03500 
00210                                                                   GC03500 
00211      MOVE QCF-OPERATOR-ID       TO PREVIOUS-OPERATOR.             GC03500 
00212      MOVE QCF-GROUP-NO          TO PREVIOUS-GROUP-NO.             GC03500 
00213      PERFORM 0010-READ-QCF-FILE THRU 0010-EXIT.                   GC03500 
00214                                                                   GC03500 
00215      IF END-OF-QCF-FILE                                           GC03500 
00216         SET ONCE-MORE TO TRUE.                                    GC03500 
00217                                                                   GC03500 
00218  0015-EXIT.                                                       GC03500 
00219      EXIT.                                                        GC03500 
00220 /                                                                 GC03500 
00221 ******************************************************************GC03500 
00222 *                     0020-PROCESS-TABLE                         *GC03500 
00223 *                                                                *GC03500 
00224 *  IF THE TABLE HAS LESS THAN THREE ENTRIES, THEN ALL OF THE     *GC03500 
00225 *  ENTRIES ARE WRITTEN TO THE NEW FILE.  IF THE FILE HAS MORE    *GC03500 
00226 *  THREE ENTRIES THEN THE ENTRIES ARE PROCESSED AND THREE        *GC03500 
00227 *  OF THE ENTRIES ARE RANDOMLY PICKED TO BE WRITTEN TO THE NEW   *GC03500 
00228 *  FILE.                                                         *GC03500 
00229 ******************************************************************GC03500 
00230  0020-PROCESS-TABLE.                                              GC03500 
00231                                                                   GC03500 
00232      IF TABLE-ENTRIES-CNT > 3                                     GC03500 
00233         PERFORM 0030-RANDOMIZE-GROUP-NO THRU 0030-EXIT            GC03500 
00234           VARYING COUNTER2 FROM 1 BY 1                            GC03500 
00235            UNTIL COUNTER2 > 3                                     GC03500 
00236      ELSE                                                         GC03500 
00237         PERFORM VARYING COUNTER1 FROM 1 BY 1                      GC03500 
00238          UNTIL COUNTER1 > 3 OR                                    GC03500 
00239                COUNTER1 > TABLE-ENTRIES-CNT                       GC03500 
00240             SET TBL-INDX  TO COUNTER1                             GC03500 
00241             MOVE SPACES   TO RND-RECORD                           GC03500 
00242             MOVE HLD-PLAN-CODE(TBL-INDX) TO RND-PLAN-CODE         GC03500 
00243             MOVE HLD-GROUP-NO (TBL-INDX) TO RND-GROUP-NO          GC03500 
00244             MOVE PREVIOUS-OPERATOR       TO RND-OPERATOR-ID       GC03500 
00245             PERFORM 0040-WRITE-FILE THRU 0040-EXIT                GC03500 
00246         END-PERFORM                                               GC03500 
00247      END-IF.                                                      GC03500 
00248                                                                   GC03500 
00249  0020-EXIT.                                                       GC03500 
00250      EXIT.                                                        GC03500 
00251 /                                                                 GC03500 
00252 ******************************************************************GC03500 
00253 *                    0025-INITIALIZE-TABLE                       *GC03500 
00254 *                                                                *GC03500 
00255 *  AFTER THE TABLE HAS BEEN RANDOMIZED, THEN IT MUST BE          *GC03500 
00256 *  INITIALIZED TO ACCOMODATE THE NEW OPERATOR AND GROUP NUMBERS  *GC03500 
00257 ******************************************************************GC03500 
00258  0025-INITIALIZE-TABLE.                                           GC03500 
00259                                                                   GC03500 
00260      PERFORM VARYING COUNTER3 FROM 1 BY 1                         GC03500 
00261        UNTIL COUNTER3 > TABLE-ENTRIES-CNT                         GC03500 
00262           SET TBL-INDX TO COUNTER3                                GC03500 
00263           MOVE SPACES      TO HLD-PLAN-CODE (TBL-INDX)            GC03500 
00264                               HLD-GROUP-NO (TBL-INDX)             GC03500 
00265      END-PERFORM.                                                 GC03500 
00266                                                                   GC03500 
00267  0025-EXIT.                                                       GC03500 
00268      EXIT.                                                        GC03500 
00269 /                                                                 GC03500 
00270 ******************************************************************GC03500 
00271 *                  0030-RANDOMIZE-GROUP-NO                       *GC03500 
00272 *                                                                *GC03500 
00273 *  THE RANDOM TABLE SUBSCRIPT IS CALCULATED USING A FORMULA      *GC03500 
00274 *  THAT TAKES THE HUNDREDTH SECONDS FROM THE SYSTEM TIME,        *GC03500 
00275 *  DIVIDES IT BY 100 AND MULTIPLIES IT BY NUMBER OF TABLE        *GC03500 
00276 *  ENTRIES.  WHEN THE SUBSCRIPT CALCULATED IS GREATER THAN ZERO, *GC03500 
00277 *  IT IS USED TO INDEX THE TABLE.  THE DATA AT THAT SUBSCRIPT    *GC03500 
00278 *  IS THEN WRITTEN OUT TO THE NEW FILE.  THAT AREA IS THEN       *GC03500 
00279 *  REINITIALIZED AND THE REST OF THE DATA FROM THE TABLE IS      *GC03500 
00280 *  REFORMATTED TO REMOVE THE SPACE IN THE TABLE.                 *GC03500 
00281 ******************************************************************GC03500 
00282  0030-RANDOMIZE-GROUP-NO.                                         GC03500 
00283                                                                   GC03500 
00284      MOVE ZEROS TO NUM-SUBSCRIPT.                                 GC03500 
00285                                                                   GC03500 
00286      PERFORM UNTIL NUM-SUBSCRIPT > ZERO                           GC03500 
00287        ACCEPT WS-TIME   FROM TIME                                 GC03500 
00288        MOVE WS-TIME-TT  TO NUM-TIME-TT                            GC03500 
00289        COMPUTE                                                    GC03500 
00290           NUM-SUBSCRIPT ROUNDED =                                 GC03500 
00291                    TABLE-ENTRIES-CNT * (NUM-TIME-TT / 100)        GC03500 
00292      END-PERFORM.                                                 GC03500 
00293                                                                   GC03500 
00294      SET TBL-INDX  TO NUM-SUBSCRIPT.                              GC03500 
00295      MOVE SPACES   TO RND-RECORD.                                 GC03500 
00296      MOVE HLD-PLAN-CODE(TBL-INDX) TO RND-PLAN-CODE.               GC03500 
00297      MOVE HLD-GROUP-NO (TBL-INDX) TO RND-GROUP-NO.                GC03500 
00298      MOVE PREVIOUS-OPERATOR       TO RND-OPERATOR-ID.             GC03500 
00299      PERFORM 0040-WRITE-FILE   THRU 0040-EXIT.                    GC03500 
00300                                                                   GC03500 
00301      MOVE SPACES   TO HLD-PLAN-CODE(TBL-INDX)                     GC03500 
00302                       HLD-GROUP-NO (TBL-INDX).                    GC03500 
00303                                                                   GC03500 
00304      MOVE NUM-SUBSCRIPT       TO COUNTER3.                        GC03500 
00305      SET TBL-INDX, TBL-INDX2  TO NUM-SUBSCRIPT.                   GC03500 
00306      SET TBL-INDX2 UP BY 1.                                       GC03500 
00307                                                                   GC03500 
00308      PERFORM VARYING COUNTER3 FROM COUNTER3 BY 1                  GC03500 
00309        UNTIL COUNTER3 > TABLE-ENTRIES-CNT                         GC03500 
00310           MOVE HLD-PLAN-CODE (TBL-INDX2)                          GC03500 
00311                                      TO HLD-PLAN-CODE (TBL-INDX)  GC03500 
00312           MOVE HLD-GROUP-NO (TBL-INDX2)                           GC03500 
00313                                      TO HLD-GROUP-NO (TBL-INDX)   GC03500 
00314           SET TBL-INDX, TBL-INDX2 UP BY 1                         GC03500 
00315      END-PERFORM.                                                 GC03500 
00316                                                                   GC03500 
00317      COMPUTE TABLE-ENTRIES-CNT = TABLE-ENTRIES-CNT - 1.           GC03500 
00318                                                                   GC03500 
00319  0030-EXIT.                                                       GC03500 
00320      EXIT.                                                        GC03500 
00321 /                                                                 GC03500 
00322 ******************************************************************GC03500 
00323 *                      0040-WRITE-FILE                           *GC03500 
00324 *                                                                 GC03500 
00325 *  WRITES TO THE RANDOM FILE.                                    *GC03500 
00326 ******************************************************************GC03500 
00327  0040-WRITE-FILE.                                                 GC03500 
00328                                                                   GC03500 
00329      WRITE RND-RECORD.                                            GC03500 
00330                                                                   GC03500 
00331  0040-EXIT.                                                       GC03500 
00332      EXIT.                                                        GC03500 
00333 /                                                                 GC03500 
