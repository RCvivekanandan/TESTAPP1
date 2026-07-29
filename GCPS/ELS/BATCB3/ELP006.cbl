00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELP006  
00003  PROGRAM-ID.         ELP006.                                         LV001
00004                                                                   ELP006  
00005  AUTHOR.             EDWARD G LISS.                               ELP006  
00006                                                                   ELP006  
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELP006  
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELP006  
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELP006  
00010                      233 N. MICHIGAN AVE                          ELP006  
00011                      CHICAGO, ILLINOIS 60601                      ELP006  
00012                                                                   ELP006  
00013  DATE-WRITTEN.       22-NOV-1989.                                 ELP006  
00014                      ORIGINAL WRITTEN 18-OCT-1985 BY CAT LADY.    ELP006  
00015                                                                   ELP006  
00016  DATE-COMPILED.                                                   ELP006  
00017                                                                   ELP006  
00018  SECURITY.           COPYRIGHT 1988,                              ELP006  
00019                      HEALTH CARE SERVICE CORPORATION              ELP006  
00020      TITLE 'PRODUCE CODES MANUAL INDEX FILE'.                     ELP006  
00021 ********** ORIGINAL AUTHORS COMMENTS **************************** ELP006  
00022 ***************************************************************** ELP006  
00023 * THIS PROGRAM GENERATES INDEX ENTRIES FOR THE CODES MANUAL FOR * ELP006  
00024 * THE ENGLISH LANGUAGE SUPPORT PROTOTYPE SYSTEM.  THE INDEX     * ELP006  
00025 * ENTRIES GO INTO AN ALPHA SORT AND ARE PRINTED BY ELP011.      * ELP006  
00026 *                                                               * ELP006  
00027 * ELP006 READS THE ENTIRE DATA ELEMENT FILE.  FOR EVERY RECORD  * ELP006  
00028 * ON IT NOT FLAGGED AS DELETED, ELP006 USES THE DATA ELEMENT    * ELP006  
00029 * NAME FIELD TO GENERATE ONE OR MORE INDEX ENTRY RECORDS.       * ELP006  
00030 *                                                               * ELP006  
00031 * THE DATA ELEMENT NAME CONSISTS OF A SERIES OF WORDS FOLLOWED  * ELP006  
00032 * BY TRAILING BLANKS.  EACH INDEX ENTRY CONSISTS OF THE         * ELP006  
00033 * ENTIRE DATA NAME FIELD, IN THE SAME SEQUENCE, BUT STARTING    * ELP006  
00034 * WITH A DIFFERENT WORD EACH TIME.  WE PROCEED TO BUILD THE     * ELP006  
00035 * OUTPUT RECORD STARTING AT THE BEGINNING OF A PARTICULAR       * ELP006  
00036 * WORD IN THE DATA NAME FIELD.  WE COPY THAT WORD, AND ALL      * ELP006  
00037 * WORDS FOLLOWING IT IN THE DATA NAME FIELD, TO THE OUTPUT.     * ELP006  
00038 * IF THERE ARE TRAILING BLANKS, WE COMPRESS THEM DOWN TO ONE    * ELP006  
00039 * BLANK.  WE THEN GO BACK THROUGH THE BEGINNING OF THE DATA     * ELP006  
00040 * NAME FIELD, COPYING ALL WORDS UP TO THE STARTING POINT WORD.  * ELP006  
00041 *                                                               * ELP006  
00042 * NOT EVERY WORD GETS A CHANCE TO BE A 'STARTING POINT' WORD.   * ELP006  
00043 * ENTRIES IN AN INDEX DON'T ORDINARILY START WITH ARTICLES      * ELP006  
00044 * OR PREPOSITIONS, SO WE WATCH FOR THOSE.                       * ELP006  
00045 * ALSO, CERTAIN TWO-WORD PHRASES ARE ALWAYS TREATED AS ONE      * ELP006  
00046 * WORD.  OBVIOUS EXAMPLES ARE BLUE CROSS, BLUE SHIELD,          * ELP006  
00047 * MAJOR MEDICAL, AND WAITING PERIOD.                            * ELP006  
00048 *                                                               * ELP006  
00049 ***************************************************************** ELP006  
00050 /**************************************************************** ELP006  
00051 *                                                               * ELP006  
00052 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        * ELP006  
00053 *    *-*         U P D A T E   H I S T O R Y         *-*        * ELP006  
00054 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        * ELP006  
00055 *                                                               * ELP006  
00056 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* ELP006  
00057 *                                                               * ELP006  
00058 *  01.0      11/22/89  EGL  REWROTE PROGRAM SINCE THE OLD       * ELP006  
00059 *                           VERSION WAS VERY STRANGE.           * ELP006  
00060 *                                                               * ELP006  
00061 ***************************************************************** ELP006  
00062                                                                   ELP006  
00063  ENVIRONMENT DIVISION.                                            ELP006  
00064  CONFIGURATION SECTION.                                           ELP006  
00065  SOURCE-COMPUTER.  IBM-370.                                       ELP006  
00066  OBJECT-COMPUTER.  IBM-370.                                       ELP006  
00067                                                                   ELP006  
00068  INPUT-OUTPUT SECTION.                                            ELP006  
00069  FILE-CONTROL.                                                    ELP006  
00070                                                                   ELP006  
00071      SELECT ENTRY-FILE                                            ELP006  
00072          ASSIGN TO CODINDX.                                       ELP006  
00073 /                                                                 ELP006  
00074  DATA DIVISION.                                                   ELP006  
00075                                                                   ELP006  
00076  FILE SECTION.                                                    ELP006  
00077                                                                   ELP006  
00078  FD  ENTRY-FILE                                                   ELP006  
00079      BLOCK CONTAINS  0  RECORDS                                   ELP006  
00080      LABEL RECORDS ARE STANDARD                                   ELP006  
00081      RECORDING MODE IS F                                          ELP006  
00082      DATA RECORD IS ENTRY-REC.                                    ELP006  
00083  01  ENTRY-REC.                                                   ELP006  
00084  COPY ELPIXC.                                                     ELP006  
00085 /                                                                 ELP006  
00086  WORKING-STORAGE SECTION.                                         ELP006  
00087  77  FILLER                         PIC X(30)                     ELP006  
00088        VALUE 'ELP006 WORKING STORAGE BEGINS'.                     ELP006  
00089                                                                   ELP006  
00090  01  MISC-WORK.                                                   ELP006  
00091      05  ABEND-CODE                 PIC 9(04) COMP VALUE ZEROS.   ELP006  
00092      05  WS-NUM-RAW-WORDS           PIC S9(4) COMP VALUE ZERO.    ELP006  
00093      05  WS-NUM-GOOD-WORDS          PIC S9(4) COMP VALUE ZERO.    ELP006  
00094      05  WS-WORD-SUB                PIC S9(4) COMP VALUE ZERO.    ELP006  
00095      05  WS-MOVE-SUB                PIC S9(4) COMP VALUE ZERO.    ELP006  
00096      05  WS-POINTER                 PIC S9(4) COMP VALUE ZERO.    ELP006  
00097      05  WS-DE-STATUS-IND           PIC X          VALUE 'N'.     ELP006  
00098          88  WS-DE-NOT-EOF                         VALUE 'N'.     ELP006  
00099          88  WS-DE-EOF                             VALUE 'Y'.     ELP006  
00100      05  WS-ACCEPT-REJECT-IND       PIC X          VALUE 'N'.     ELP006  
00101          88  WS-REJECT-WORD                        VALUE 'N'.     ELP006  
00102          88  WS-ACCEPT-WORD                        VALUE 'Y'.     ELP006  
00103      05  WS-PHRASE-IND              PIC X          VALUE 'N'.     ELP006  
00104          88  WS-NOT-PHRASE                         VALUE 'N'.     ELP006  
00105          88  WS-PHRASE                             VALUE 'Y'.     ELP006  
00106      05  WS-WORD-TEST               PIC X(20).                    ELP006  
00107      05  FILLER        REDEFINES    WS-WORD-TEST.                 ELP006  
00108          10  WS-WORD-TEST-5         PIC X(5).                     ELP006  
00109          10  FILLER                 PIC X(15).                    ELP006  
00110      05  WS-ENTRY-WORK              PIC X(75).                    ELP006  
00111                                                                   ELP006  
00112  01  WS-PARSE-AREA.                                               ELP006  
00113      05  WS-PARSE-WORD              PIC X(75)                     ELP006  
00114                                     OCCURS 38 TIMES               ELP006  
00115                                     INDEXED BY WS-PARSE-IDX.      ELP006  
00116                                                                   ELP006  
00117  01  WS-WORD-AREA.                                                ELP006  
00118      05  WS-WORD-LIST               PIC X(75)                     ELP006  
00119                                     OCCURS 38 TIMES               ELP006  
00120                                     INDEXED BY WS-WORD-IDX.       ELP006  
00121 /*****************************************************************ELP006  
00122 **            PARAMETERS FOR THE IO INTERFACE PROGRAM           **ELP006  
00123 ******************************************************************ELP006  
00124  01  IO-INFO.                                                     ELP006  
00125      COPY ELBHIOPM.                                               ELP006  
00126 /                                                                 ELP006  
00127  01  UNQUALIFIED-WORD-TABLE.                                      ELP006  
00128      05  UNQUALIFIED-WORDS.                                       ELP006  
00129          10  FILLER                 PIC X(4)  VALUE 'A   '.       ELP006  
00130          10  FILLER                 PIC X(4)  VALUE 'AN  '.       ELP006  
00131          10  FILLER                 PIC X(4)  VALUE 'AND '.       ELP006  
00132          10  FILLER                 PIC X(4)  VALUE 'FOR '.       ELP006  
00133          10  FILLER                 PIC X(4)  VALUE 'FROM'.       ELP006  
00134          10  FILLER                 PIC X(4)  VALUE 'IN  '.       ELP006  
00135          10  FILLER                 PIC X(4)  VALUE 'OF  '.       ELP006  
00136          10  FILLER                 PIC X(4)  VALUE 'ON  '.       ELP006  
00137          10  FILLER                 PIC X(4)  VALUE 'THE '.       ELP006  
00138          10  FILLER                 PIC X(4)  VALUE 'THIS'.       ELP006  
00139          10  FILLER                 PIC X(4)  VALUE 'TO  '.       ELP006  
00140          10  FILLER                 PIC X(4)  VALUE 'WITH'.       ELP006  
00141          10  FILLER                 PIC X(4)  VALUE '0   '.       ELP006  
00142          10  FILLER                 PIC X(4)  VALUE '1   '.       ELP006  
00143          10  FILLER                 PIC X(4)  VALUE '2   '.       ELP006  
00144          10  FILLER                 PIC X(4)  VALUE '3   '.       ELP006  
00145          10  FILLER                 PIC X(4)  VALUE '4   '.       ELP006  
00146          10  FILLER                 PIC X(4)  VALUE '5   '.       ELP006  
00147          10  FILLER                 PIC X(4)  VALUE '6   '.       ELP006  
00148          10  FILLER                 PIC X(4)  VALUE '7   '.       ELP006  
00149          10  FILLER                 PIC X(4)  VALUE '8   '.       ELP006  
00150          10  FILLER                 PIC X(4)  VALUE '9   '.       ELP006  
00151      05  UNQUALIFIED-WORDS-RDF REDEFINES UNQUALIFIED-WORDS.       ELP006  
00152          10  UNQUALIFIED-WORD       OCCURS 22 TIMES               ELP006  
00153                                     ASCENDING KEY IS UNQUAL-KEY   ELP006  
00154                                     INDEXED BY UNQUAL-IX1.        ELP006  
00155              15  UNQUAL-KEY         PIC X(4).                     ELP006  
00156                                                                   ELP006  
00157  COPY ELPPHC.                                                     ELP006  
00158 /                                                                 ELP006  
00159 ******************************************************************ELP006  
00160 **         HOLD AREA FOR THE VSAM RECORDS.                      **ELP006  
00161 ******************************************************************ELP006  
00162                                                                   ELP006  
00163  01  HOLD-DE-RECORD.                                              ELP006  
00164      COPY ELPDEC.                                                 ELP006  
00165 /                                                                 ELP006  
00166  PROCEDURE DIVISION.                                              ELP006  
00167                                                                   ELP006  
00168      PERFORM 0010-OPEN-FILES.                                     ELP006  
00169                                                                   ELP006  
00170      PERFORM 8050-READ-NEXT-ELPDE.                                ELP006  
00171                                                                   ELP006  
00172      PERFORM 1000-EXAMINE-DE-NAME                                 ELP006  
00173          UNTIL WS-DE-EOF.                                         ELP006  
00174                                                                   ELP006  
00175      PERFORM 0020-CLOSE-FILES.                                    ELP006  
00176      MOVE ZERO TO RETURN-CODE.                                    ELP006  
00177      GOBACK.                                                      ELP006  
00178                                                                   ELP006  
00179  0010-OPEN-FILES.                                                 ELP006  
00180                                                                   ELP006  
00181      OPEN OUTPUT ENTRY-FILE.                                      ELP006  
00182      SET DATA-ELEMENT-FILE TO TRUE.                               ELP006  
00183      SET ELBHIO-OPEN TO TRUE.                                     ELP006  
00184                                                                   ELP006  
00185      CALL 'ELBIOPGM'  USING IO-INFO.                              ELP006  
00186                                                                   ELP006  
00187      IF ELBHIO-GOOD-RETURN                                        ELP006  
00188          NEXT SENTENCE                                            ELP006  
00189      ELSE                                                         ELP006  
00190          MOVE ELBHIO-FEEDBACK  TO  ABEND-CODE                     ELP006  
00191          PERFORM 9999-ABEND-RTN.                                  ELP006  
00192                                                                   ELP006  
00193  0020-CLOSE-FILES.                                                ELP006  
00194                                                                   ELP006  
00195      CLOSE ENTRY-FILE.                                            ELP006  
00196      SET DATA-ELEMENT-FILE TO TRUE.                               ELP006  
00197      SET ELBHIO-CLOSE TO TRUE.                                    ELP006  
00198                                                                   ELP006  
00199      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP006  
00200                                                                   ELP006  
00201      IF ELBHIO-GOOD-RETURN                                        ELP006  
00202          NEXT SENTENCE                                            ELP006  
00203      ELSE                                                         ELP006  
00204          MOVE ELBHIO-FEEDBACK  TO  ABEND-CODE                     ELP006  
00205          PERFORM 9999-ABEND-RTN.                                  ELP006  
00206 /*****************************************************************ELP006  
00207 ** READ THE DATA ELEMENT FILE RECORD AND OBTAIN THE DATA        **ELP006  
00208 ** ELEMENT NAME.  IF THE DATA ELEMENT RECORD IS NOT FLAGGED     **ELP006  
00209 ** FOR DELETION, PRODUCE ALL OF THE INDEX ENTRY RECORDS         **ELP006  
00210 ** THAT CAN BE BUILT FROM THE DATA ELEMENT NAME IN ACCORDANCE   **ELP006  
00211 ** WITH THE ABOVE RULES.                                        **ELP006  
00212 ******************************************************************ELP006  
00213                                                                   ELP006  
00214  1000-EXAMINE-DE-NAME.                                            ELP006  
00215                                                                   ELP006  
00216      IF DE-DELETE   OR                                            ELP006  
00217         DE-ELEMENT-NAME = SPACES OR LOW-VALUE                     ELP006  
00218           NEXT SENTENCE                                           ELP006  
00219      ELSE                                                         ELP006  
00220          PERFORM 2000-CREATE-NEXT-ENTRY.                          ELP006  
00221      PERFORM 8050-READ-NEXT-ELPDE.                                ELP006  
00222                                                                   ELP006  
00223  2000-CREATE-NEXT-ENTRY.                                          ELP006  
00224                                                                   ELP006  
00225      PERFORM 2100-PARSE-INTO-WORDS.                               ELP006  
00226      MOVE 1 TO WS-WORD-SUB.                                       ELP006  
00227      PERFORM 2200-ELIMINATE-NOISE-WORDS                           ELP006  
00228              UNTIL WS-WORD-SUB > WS-NUM-RAW-WORDS.                ELP006  
00229      PERFORM 2300-WRITE-INDEX-RECORDS                             ELP006  
00230            VARYING WS-WORD-SUB FROM 1 BY 1                        ELP006  
00231              UNTIL WS-WORD-SUB > WS-NUM-GOOD-WORDS.               ELP006  
00232 /                                                                 ELP006  
00233  2100-PARSE-INTO-WORDS.                                           ELP006  
00234      INSPECT DE-ELEMENT-NAME REPLACING ALL '.' BY SPACE           ELP006  
00235                                            ',' BY SPACE           ELP006  
00236                                            '-' BY SPACE           ELP006  
00237                                            '/' BY SPACE           ELP006  
00238                                      LOW-VALUE BY SPACE.          ELP006  
00239      MOVE SPACES TO WS-PARSE-AREA.                                ELP006  
00240      MOVE ZERO   TO WS-NUM-RAW-WORDS                              ELP006  
00241                     WS-NUM-GOOD-WORDS.                            ELP006  
00242      UNSTRING DE-ELEMENT-NAME DELIMITED BY ALL SPACES INTO        ELP006  
00243               WS-PARSE-WORD (1)                                   ELP006  
00244               WS-PARSE-WORD (2)                                   ELP006  
00245               WS-PARSE-WORD (3)                                   ELP006  
00246               WS-PARSE-WORD (4)                                   ELP006  
00247               WS-PARSE-WORD (5)                                   ELP006  
00248               WS-PARSE-WORD (6)                                   ELP006  
00249               WS-PARSE-WORD (7)                                   ELP006  
00250               WS-PARSE-WORD (8)                                   ELP006  
00251               WS-PARSE-WORD (9)                                   ELP006  
00252               WS-PARSE-WORD (10)                                  ELP006  
00253               WS-PARSE-WORD (11)                                  ELP006  
00254               WS-PARSE-WORD (12)                                  ELP006  
00255               WS-PARSE-WORD (13)                                  ELP006  
00256               WS-PARSE-WORD (14)                                  ELP006  
00257               WS-PARSE-WORD (15)                                  ELP006  
00258               WS-PARSE-WORD (16)                                  ELP006  
00259               WS-PARSE-WORD (17)                                  ELP006  
00260               WS-PARSE-WORD (18)                                  ELP006  
00261               WS-PARSE-WORD (19)                                  ELP006  
00262               WS-PARSE-WORD (20)                                  ELP006  
00263               WS-PARSE-WORD (21)                                  ELP006  
00264               WS-PARSE-WORD (22)                                  ELP006  
00265               WS-PARSE-WORD (23)                                  ELP006  
00266               WS-PARSE-WORD (24)                                  ELP006  
00267               WS-PARSE-WORD (25)                                  ELP006  
00268               WS-PARSE-WORD (26)                                  ELP006  
00269               WS-PARSE-WORD (27)                                  ELP006  
00270               WS-PARSE-WORD (28)                                  ELP006  
00271               WS-PARSE-WORD (29)                                  ELP006  
00272               WS-PARSE-WORD (30)                                  ELP006  
00273               WS-PARSE-WORD (31)                                  ELP006  
00274               WS-PARSE-WORD (32)                                  ELP006  
00275               WS-PARSE-WORD (33)                                  ELP006  
00276               WS-PARSE-WORD (34)                                  ELP006  
00277               WS-PARSE-WORD (35)                                  ELP006  
00278               WS-PARSE-WORD (36)                                  ELP006  
00279               WS-PARSE-WORD (37)                                  ELP006  
00280               WS-PARSE-WORD (38)                                  ELP006  
00281          TALLYING IN WS-NUM-RAW-WORDS.                            ELP006  
00282 /                                                                 ELP006  
00283  2200-ELIMINATE-NOISE-WORDS.                                      ELP006  
00284       SET WS-PARSE-IDX TO WS-WORD-SUB.                            ELP006  
00285       MOVE WS-PARSE-WORD (WS-PARSE-IDX) TO WS-WORD-TEST.          ELP006  
00286       SEARCH ALL UNQUALIFIED-WORD                                 ELP006  
00287           AT END                                                  ELP006  
00288               SET WS-ACCEPT-WORD TO TRUE                          ELP006  
00289           WHEN UNQUAL-KEY (UNQUAL-IX1) = WS-WORD-TEST-5           ELP006  
00290               SET WS-REJECT-WORD TO TRUE                          ELP006  
00291       END-SEARCH.                                                 ELP006  
00292       IF WS-ACCEPT-WORD THEN                                      ELP006  
00293           IF WS-WORD-SUB = WS-NUM-RAW-WORDS                       ELP006  
00294                SET WS-NOT-PHRASE TO TRUE                          ELP006  
00295           ELSE                                                    ELP006  
00296                PERFORM 2210-CHECK-FOR-PHRASE                      ELP006  
00297           END-IF                                                  ELP006  
00298           ADD 1 TO WS-NUM-GOOD-WORDS                              ELP006  
00299           IF WS-PHRASE                                            ELP006  
00300               MOVE WS-WORD-TEST TO WS-WORD-LIST                   ELP006  
00301                                      (WS-NUM-GOOD-WORDS)          ELP006  
00302               ADD 2 TO WS-WORD-SUB                                ELP006  
00303           ELSE                                                    ELP006  
00304               MOVE WS-PARSE-WORD (WS-PARSE-IDX) TO                ELP006  
00305                    WS-WORD-LIST (WS-NUM-GOOD-WORDS)               ELP006  
00306               ADD 1 TO WS-WORD-SUB                                ELP006  
00307           END-IF                                                  ELP006  
00308       ELSE                                                        ELP006  
00309           ADD 1 TO WS-WORD-SUB                                    ELP006  
00310       END-IF.                                                     ELP006  
00311 /                                                                 ELP006  
00312  2210-CHECK-FOR-PHRASE.                                           ELP006  
00313      MOVE SPACES TO WS-WORD-TEST.                                 ELP006  
00314      STRING WS-PARSE-WORD (WS-PARSE-IDX)                          ELP006  
00315                                     DELIMITED BY SPACE            ELP006  
00316             ' '                     DELIMITED BY SIZE             ELP006  
00317             WS-PARSE-WORD (WS-PARSE-IDX + 1)                      ELP006  
00318                                     DELIMITED BY SPACE            ELP006  
00319        INTO WS-WORD-TEST.                                         ELP006  
00320      SEARCH ALL PHRASE                                            ELP006  
00321          AT END                                                   ELP006  
00322              SET WS-NOT-PHRASE TO TRUE                            ELP006  
00323          WHEN PHRASE-KEY (PHRASE-IX1) = WS-WORD-TEST              ELP006  
00324              SET WS-PHRASE TO TRUE                                ELP006  
00325      END-SEARCH.                                                  ELP006  
00326 ***                                                               ELP006  
00327 ***  CHANGE THE SPACE TO A LOW VALUE BETWEEN THE WORDS.  THIS     ELP006  
00328 ***  WILL RELEAVE SUBSEQUENT PROGRAMS FROM REPEATING THESE        ELP006  
00329 ***  COMPLICATED WORD/PHRASE TESTS.                               ELP006  
00330 ***                                                               ELP006  
00331      IF WS-PHRASE                                                 ELP006  
00332          INSPECT WS-WORD-TEST REPLACING                           ELP006  
00333               FIRST SPACE BY LOW-VALUE                            ELP006  
00334      END-IF.                                                      ELP006  
00335 /                                                                 ELP006  
00336  2300-WRITE-INDEX-RECORDS.                                        ELP006  
00337      MOVE SPACES TO WS-ENTRY-WORK.                                ELP006  
00338      MOVE WS-WORD-SUB TO WS-MOVE-SUB.                             ELP006  
00339      MOVE 1 TO WS-POINTER.                                        ELP006  
00340      PERFORM WITH TEST AFTER                                      ELP006  
00341             UNTIL WS-WORD-SUB = WS-MOVE-SUB                       ELP006  
00342          STRING WS-WORD-LIST (WS-MOVE-SUB) DELIMITED BY SPACE     ELP006  
00343                 ' '                        DELIMITED BY SIZE      ELP006  
00344            INTO WS-ENTRY-WORK POINTER WS-POINTER                  ELP006  
00345          END-STRING                                               ELP006  
00346          ADD 1 TO WS-MOVE-SUB                                     ELP006  
00347          IF WS-MOVE-SUB > WS-NUM-GOOD-WORDS                       ELP006  
00348               MOVE 1 TO WS-MOVE-SUB                               ELP006  
00349          END-IF                                                   ELP006  
00350      END-PERFORM.                                                 ELP006  
00351      MOVE WS-ENTRY-WORK TO ENTRY-TAB.                             ELP006  
00352      MOVE DE-RECORD-PREFIX TO ENTRY-PREFIX.                       ELP006  
00353      MOVE DE-ELEMENT-NBR TO ENTRY-ELEMENT-NBR.                    ELP006  
00354      WRITE ENTRY-REC.                                             ELP006  
00355 /*****************************************************************ELP006  
00356 ** THIS ROUTINE DOES A SEQUENTIAL READ NEXT ON THE DATA ELEMENT **ELP006  
00357 ** FILE.  IF AT END OF FILE, A STATUS CODE IS SET.              **ELP006  
00358 ** END OF FILE IS NOT NECESSARILY AN ERROR IN ELP006.           **ELP006  
00359 ** IF A SERIOUS ERROR OCCURS, A MESSAGE IS PRODUCED             **ELP006  
00360 ** AND PROCESSING IS TERMINATED.                                **ELP006  
00361 ******************************************************************ELP006  
00362                                                                   ELP006  
00363  8050-READ-NEXT-ELPDE.                                            ELP006  
00364                                                                   ELP006  
00365      SET DATA-ELEMENT-FILE TO TRUE.                               ELP006  
00366      SET ELBHIO-SEQUENTIAL-GET TO TRUE.                           ELP006  
00367                                                                   ELP006  
00368      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP006  
00369                                                                   ELP006  
00370      IF ELBHIO-GOOD-RETURN                                        ELP006  
00371          MOVE +11               TO  DE-NBR-DESC-LINES             ELP006  
00372          MOVE ELBHIO-ELPDE      TO  DATA-ELEMENT                  ELP006  
00373          SET WS-DE-NOT-EOF      TO  TRUE                          ELP006  
00374      ELSE                                                         ELP006  
00375          SET WS-DE-EOF          TO  TRUE                          ELP006  
00376          IF ELBHIO-REQUEST-TYPE  NOT  =  '2'                      ELP006  
00377              IF ELBHIO-FEEDBACK  NOT  =  16                       ELP006  
00378                  PERFORM 9100-VSAM-ABEND.                         ELP006  
00379 /*****************************************************************ELP006  
00380 ** THIS ROUTINE IS USED FOR VSAM ERRORS WHICH INDICATE SOMETHING**ELP006  
00381 ** SERIOUSLY WRONG WITH THE FILE.                               **ELP006  
00382 ******************************************************************ELP006  
00383  9100-VSAM-ABEND.                                                 ELP006  
00384                                                                   ELP006  
00385      MOVE ELBHIO-FEEDBACK       TO  ABEND-CODE.                   ELP006  
00386      PERFORM 9999-ABEND-RTN.                                      ELP006  
00387                                                                   ELP006  
00388  9999-ABEND-RTN.                                                  ELP006  
00389                                                                   ELP006  
00390      CALL 'TSGEND'  USING  ABEND-CODE.                            ELP006  
