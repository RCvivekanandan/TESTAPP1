00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID.    ELP012.                                           ELP012  
00003  AUTHOR.        NINA CERVANTES.                                      LV001
00004  INSTALLATION.  HCMS/BCBS.                                        ELP012  
00005  DATE-WRITTEN.  OCTOBER 1985.                                     ELP012  
00006  DATE-COMPILED.                                                   ELP012  
00007 ***************************************************************** ELP012  
00008 *            ELP012 - ELS: MASS DELETE OF RECORDS.              * ELP012  
00009 *                                                               * ELP012  
00010 *      1) READ THROUGH THE ENTIRE RECORD LIST FILE AND          * ELP012  
00011 *         PHYSICALLY DELETE THOSE RECORDS FLAGGED FOR           * ELP012  
00012 *         DELETION.  FOR EACH RECORD LIST DELETED, READ         * ELP012  
00013 *         THROUGH THE ENTIRE DATA ELEMENT FILE AND DELETE       * ELP012  
00014 *         THOSE DATA ELEMENT(S) ASSOCIATED WITH IT.  FOR EACH   * ELP012  
00015 *         DATA ELEMENT DELETED, READ THROUGH THE ENTIRE         * ELP012  
00016 *         ELEMENT CODE VALUE FILE AND DELETE THOSE CODE VALUE(S)* ELP012  
00017 *         ASSOCIATED WITH IT.                                   * ELP012  
00018 *                                                               * ELP012  
00019 *      2) DO THE SAME AS 1) ABOVE EXCEPT START THE PROCESS AT   * ELP012  
00020 *         THE DATA ELEMENT FILE.                                * ELP012  
00021 *                                                               * ELP012  
00022 *      3) DO THE SAME AS 1) ABOVE EXCEPT START THE PROCESS AT   * ELP012  
00023 *         THE ELEMENT CODE VALUE FILE.                          * ELP012  
00024 *                                                               * ELP012  
00025 *      4) READ THROUGH THE ENTIRE ENGLISH NAME FILE AND         * ELP012  
00026 *         PHYSICALLY DELETE THOSE RECORDS FLAGGED FOR           * ELP012  
00027 *         DELETION.                                             * ELP012  
00028 *                                                               * ELP012  
00029 ***************************************************************** ELP012  
00030                                                                   ELP012  
00031 ***************************************************************** ELP012  
00032 *   DATE    PRGMER   UPDATE HISTORY (MOST CURRENT AT TOP)       * ELP012  
00033 * --------  ------  ------------------------------------------- * ELP012  
00034 * 07/10/86   NAC    REINITIALIZE DE-AUTO-REPRINT-FLAG IF        * ELP012  
00035 *                   DE-ENG-NAME-CHG ALSO.                       * ELP012  
00036 * 03/04/86   LET    REPLACED COBLVSAM CALLS THAT FOR AN ELS     * ELP012  
00037 *                   INTERFACE MODULE THAT WILL DO THE COBLVSAM  * ELP012  
00038 *                   FOR US.                                     * ELP012  
00039 ***************************************************************** ELP012  
00040      EJECT                                                        ELP012  
00041  ENVIRONMENT DIVISION.                                            ELP012  
00042  CONFIGURATION SECTION.                                           ELP012  
00043  SOURCE-COMPUTER.  IBM-370.                                       ELP012  
00044  OBJECT-COMPUTER.  IBM-370.                                       ELP012  
00045                                                                   ELP012  
00046  DATA DIVISION.                                                   ELP012  
00047  WORKING-STORAGE SECTION.                                         ELP012  
00048  77  FILLER   PIC X(30) VALUE 'WORKING STORAGE PROGRAM ELP012'.   ELP012  
00049  77  ELPRL-STATUS            PIC XX     VALUE ZEROES.             ELP012  
00050  77  ELPDE-STATUS            PIC XX     VALUE ZEROES.             ELP012  
00051  77  ELPCV-STATUS            PIC XX     VALUE ZEROES.             ELP012  
00052  77  ELPEN-STATUS            PIC XX     VALUE ZEROES.             ELP012  
00053  77  ELPRL-EOF-SW            PIC XXX    VALUE 'NO '.              ELP012  
00054          88 ELPRL-END-OF-FILE           VALUE 'YES'.              ELP012  
00055  77  ELPDE-EOF-SW            PIC XXX    VALUE 'NO '.              ELP012  
00056          88 ELPDE-END-OF-FILE           VALUE 'YES'.              ELP012  
00057  77  ELPCV-EOF-SW            PIC XXX    VALUE 'NO '.              ELP012  
00058          88 ELPCV-END-OF-FILE           VALUE 'YES'.              ELP012  
00059  77  ELPEN-EOF-SW            PIC XXX    VALUE 'NO '.              ELP012  
00060          88 ELPEN-END-OF-FILE           VALUE 'YES'.              ELP012  
00061  77  PROCESS-SW              PIC X      VALUE ZERO.               ELP012  
00062      88  FIRST-PROCESS                  VALUE '1'.                ELP012  
00063      88  SECOND-PROCESS                 VALUE '2'.                ELP012  
00064      88  THIRD-PROCESS                  VALUE '3'.                ELP012  
00065      88  FOURTH-PROCESS                 VALUE '4'.                ELP012  
00066  01  ERR-PRIMARY-KEY.                                             ELP012  
00067      03  ERR-RECORD-PREFIX         PIC X(8).                      ELP012  
00068      03  ERR-ELEMENT-NBR           PIC 9(5).                      ELP012  
00069      03  ERR-CODE-VALUE            PIC X(10).                     ELP012  
00070  01  ABEND-CODE                    PIC 9(4)   COMP VALUE ZERO.    ELP012  
00071  01  ELPRL-DEL-COUNT PIC 9(11)  VALUE ZEROES.                     ELP012  
00072  01  ELPRL-DEL-COUNT-X  PIC Z(10)9.                               ELP012  
00073  01  ELPDE-DEL-COUNT PIC 9(11)  VALUE ZEROES.                     ELP012  
00074  01  ELPDE-DEL-COUNT-X  PIC Z(10)9.                               ELP012  
00075  01  ELPCV-DEL-COUNT PIC 9(11)  VALUE ZEROES.                     ELP012  
00076  01  ELPCV-DEL-COUNT-X  PIC Z(10)9.                               ELP012  
00077  01  ELPEN-DEL-COUNT PIC 9(11)  VALUE ZEROES.                     ELP012  
00078  01  ELPEN-DEL-COUNT-X  PIC Z(10)9.                               ELP012  
00079      EJECT                                                        ELP012  
00080  01  HOLD-RL-RECORD.                                              ELP012  
00081      COPY ELPRLC.                                                 ELP012  
00082      SKIP3                                                        ELP012  
00083  01  HOLD-DE-RECORD.                                              ELP012  
00084      COPY ELPDEC.                                                 ELP012  
00085      SKIP3                                                        ELP012  
00086  01  HOLD-CV-RECORD.                                              ELP012  
00087      COPY ELPCVC.                                                 ELP012  
00088      SKIP3                                                        ELP012  
00089  01  HOLD-EN-RECORD.                                              ELP012  
00090      COPY ELPENC.                                                 ELP012  
00091      SKIP3                                                        ELP012  
00092  01  IO-INFO.                                                     ELP012  
00093      COPY ELBHIOPM.                                               ELP012  
00094 /                                                                 ELP012  
00095  PROCEDURE DIVISION.                                              ELP012  
00096  0000-MAINLINE.                                                   ELP012  
00097                                                                   ELP012  
00098      PERFORM 0050-OPEN-FILES                                      ELP012  
00099         THRU 0050-EXIT.                                           ELP012  
00100                                                                   ELP012  
00101      MOVE '1' TO PROCESS-SW.                                      ELP012  
00102      PERFORM  0100-SEARCH-ELPRL-FILE THRU 0100-EXIT               ELP012  
00103             UNTIL ELPRL-END-OF-FILE.                              ELP012  
00104                                                                   ELP012  
00105      MOVE '2' TO PROCESS-SW.                                      ELP012  
00106      MOVE ZEROES TO ELPDE-DEL-COUNT                               ELP012  
00107                     ELPCV-DEL-COUNT.                              ELP012  
00108                                                                   ELP012  
00109      MOVE 'DE'        TO  ELBHIO-FILE-ID.                         ELP012  
00110      MOVE 'P'         TO  ELBHIO-REQUEST-TYPE.                    ELP012  
00111      MOVE 15          TO  ELBHIO-RECORD-LENGTH.                   ELP012  
00112      MOVE LOW-VALUES  TO  ELBHIO-ELPDE-KEY.                       ELP012  
00113                                                                   ELP012  
00114      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP012  
00115                                                                   ELP012  
00116      IF ELBHIO-GOOD-RETURN                                        ELP012  
00117          NEXT SENTENCE                                            ELP012  
00118      ELSE                                                         ELP012  
00119          DISPLAY ' LET - ABEND ON P1 IN MAINLINE RTN '            ELP012  
00120              '  REQUEST RETURN IS ' ELBHIO-REQUEST-TYPE           ELP012  
00121          MOVE ELBHIO-FEEDBACK  TO ABEND-CODE                      ELP012  
00122          GO TO 0900-ERROR-RTN.                                    ELP012  
00123                                                                   ELP012  
00124      MOVE 'NO ' TO ELPDE-EOF-SW.                                  ELP012  
00125      PERFORM 0200-SEARCH-ELPDE-FILE THRU 0200-EXIT                ELP012  
00126          UNTIL ELPDE-END-OF-FILE.                                 ELP012  
00127                                                                   ELP012  
00128      MOVE '3' TO PROCESS-SW.                                      ELP012  
00129      MOVE ZERO TO ELPCV-DEL-COUNT.                                ELP012  
00130                                                                   ELP012  
00131      MOVE 'CV'        TO  ELBHIO-FILE-ID.                         ELP012  
00132      MOVE 'P'         TO  ELBHIO-REQUEST-TYPE.                    ELP012  
00133      MOVE 27          TO  ELBHIO-RECORD-LENGTH.                   ELP012  
00134      MOVE LOW-VALUES  TO  ELBHIO-ELPCV-KEY.                       ELP012  
00135                                                                   ELP012  
00136      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP012  
00137                                                                   ELP012  
00138      IF ELBHIO-GOOD-RETURN                                        ELP012  
00139          NEXT SENTENCE                                            ELP012  
00140      ELSE                                                         ELP012  
00141          DISPLAY ' LET - ABEND ON P2 IN MAINLINE RTN '            ELP012  
00142              '  REQUEST RETURN IS  ' ELBHIO-REQUEST-TYPE          ELP012  
00143          MOVE ELBHIO-FEEDBACK  TO ABEND-CODE                      ELP012  
00144          GO TO 0900-ERROR-RTN.                                    ELP012  
00145                                                                   ELP012  
00146      MOVE 'NO ' TO ELPCV-EOF-SW.                                  ELP012  
00147      PERFORM  0300-SEARCH-ELPCV-FILE THRU 0300-EXIT               ELP012  
00148             UNTIL ELPCV-END-OF-FILE.                              ELP012  
00149                                                                   ELP012  
00150      PERFORM 0400-CLOSE-FILES                                     ELP012  
00151         THRU 0400-EXIT.                                           ELP012  
00152                                                                   ELP012  
00153      STOP RUN.                                                    ELP012  
00154  0000-EXIT. EXIT.                                                 ELP012  
00155 /                                                                 ELP012  
00156  0050-OPEN-FILES.                                                 ELP012  
00157 *--------------------------------------------------------------*  ELP012  
00158 *    ISSUE ONLY ONE OPEN BUT ALL OF THE FILES RL,DE,CV,EN, WILL*  ELP012  
00159 * BE OPENED BY THE IO INTERFACE MODULE 'ELBIOPGM'.             *  ELP012  
00160 *--------------------------------------------------------------*  ELP012  
00161                                                                   ELP012  
00162      MOVE 'RL'  TO  ELBHIO-FILE-ID.                               ELP012  
00163      MOVE 'O'   TO  ELBHIO-REQUEST-TYPE.                          ELP012  
00164                                                                   ELP012  
00165      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP012  
00166                                                                   ELP012  
00167      IF ELBHIO-GOOD-RETURN                                        ELP012  
00168          NEXT SENTENCE                                            ELP012  
00169      ELSE                                                         ELP012  
00170          DISPLAY ' LET - ABEND ON OPEN '                          ELP012  
00171          MOVE ELBHIO-FEEDBACK  TO ABEND-CODE                      ELP012  
00172          GO TO 0900-ERROR-RTN.                                    ELP012  
00173                                                                   ELP012  
00174  0050-EXIT.  EXIT.                                                ELP012  
00175 /                                                                 ELP012  
00176  0100-SEARCH-ELPRL-FILE.                                          ELP012  
00177                                                                   ELP012  
00178      MOVE 'RL'  TO  ELBHIO-FILE-ID.                               ELP012  
00179      MOVE 'G'   TO  ELBHIO-REQUEST-TYPE.                          ELP012  
00180                                                                   ELP012  
00181      CALL 'ELBIOPGM' USING  IO-INFO.                              ELP012  
00182                                                                   ELP012  
00183      IF ELBHIO-GOOD-RETURN                                        ELP012  
00184          NEXT SENTENCE                                            ELP012  
00185      ELSE                                                         ELP012  
00186          IF ELBHIO-REQUEST-TYPE  =  '2'                           ELP012  
00187              MOVE 'YES' TO ELPRL-EOF-SW                           ELP012  
00188              MOVE ELPRL-DEL-COUNT TO ELPRL-DEL-COUNT-X            ELP012  
00189              MOVE ELPDE-DEL-COUNT TO ELPDE-DEL-COUNT-X            ELP012  
00190              MOVE ELPCV-DEL-COUNT TO ELPCV-DEL-COUNT-X            ELP012  
00191              DISPLAY '***************************'                ELP012  
00192              DISPLAY '     RECORD LIST LEVEL'                     ELP012  
00193              DISPLAY ELPRL-DEL-COUNT-X ' RECORDS WERE DELETED WHICELP012  
00194 -              'H CONTAINED '                                     ELP012  
00195              DISPLAY ELPDE-DEL-COUNT-X ' DATA ELEMENTS AND '      ELP012  
00196              DISPLAY ELPCV-DEL-COUNT-X ' CODE VALUES.'            ELP012  
00197              GO TO 0100-EXIT                                      ELP012  
00198          ELSE                                                     ELP012  
00199              DISPLAY ' LET - ABEND ON G IN 0100-RTN '             ELP012  
00200              MOVE ELBHIO-FEEDBACK  TO  ABEND-CODE                 ELP012  
00201              GO TO 0900-ERROR-RTN.                                ELP012  
00202                                                                   ELP012  
00203      MOVE ELBHIO-ELPRL  TO  HOLD-RL-RECORD.                       ELP012  
00204                                                                   ELP012  
00205      IF RL-DELETE                                                 ELP012  
00206          NEXT SENTENCE                                            ELP012  
00207      ELSE                                                         ELP012  
00208        IF RL-REPRINT                                              ELP012  
00209          MOVE SPACE           TO RL-AUTO-REPRINT-FLAG             ELP012  
00210          MOVE HOLD-RL-RECORD  TO  ELBHIO-ELPRL                    ELP012  
00211          MOVE 'U'             TO  ELBHIO-REQUEST-TYPE             ELP012  
00212                                                                   ELP012  
00213          CALL 'ELBIOPGM' USING  IO-INFO                           ELP012  
00214                                                                   ELP012  
00215          IF ELBHIO-GOOD-RETURN                                    ELP012  
00216              NEXT SENTENCE                                        ELP012  
00217          ELSE                                                     ELP012  
00218              DISPLAY ' LET - ABEND ON U IN 0100-RTN'              ELP012  
00219              MOVE ELBHIO-FEEDBACK  TO ABEND-CODE                  ELP012  
00220              GO TO 0900-ERROR-RTN.                                ELP012  
00221                                                                   ELP012  
00222                                                                   ELP012  
00223      IF RL-DELETE                                                 ELP012  
00224          NEXT SENTENCE                                            ELP012  
00225      ELSE                                                         ELP012  
00226          GO TO 0100-EXIT.                                         ELP012  
00227                                                                   ELP012  
00228      MOVE 'D'  TO  ELBHIO-REQUEST-TYPE.                           ELP012  
00229                                                                   ELP012  
00230      CALL 'ELBIOPGM' USING  IO-INFO.                              ELP012  
00231                                                                   ELP012  
00232      IF ELBHIO-GOOD-RETURN                                        ELP012  
00233          NEXT SENTENCE                                            ELP012  
00234      ELSE                                                         ELP012  
00235          DISPLAY ' LET - ABEND ON D IN 0100-RTN'                  ELP012  
00236          MOVE ELBHIO-FEEDBACK  TO ABEND-CODE                      ELP012  
00237          GO TO 0900-ERROR-RTN.                                    ELP012  
00238                                                                   ELP012  
00239      ADD 1 TO ELPRL-DEL-COUNT.                                    ELP012  
00240                                                                   ELP012  
00241      MOVE 'DE'              TO  ELBHIO-FILE-ID.                   ELP012  
00242      MOVE 'P'               TO  ELBHIO-REQUEST-TYPE.              ELP012  
00243      MOVE 15                TO  ELBHIO-RECORD-LENGTH.             ELP012  
00244      MOVE LOW-VALUES        TO  ELBHIO-ELPDE-KEY.                 ELP012  
00245      MOVE RL-RECORD-PREFIX  TO  ELBHIO-DE-RECORD-PREFIX.          ELP012  
00246                                                                   ELP012  
00247      CALL 'ELBIOPGM' USING  IO-INFO.                              ELP012  
00248                                                                   ELP012  
00249      IF ELBHIO-GOOD-RETURN                                        ELP012  
00250          NEXT SENTENCE                                            ELP012  
00251      ELSE                                                         ELP012  
00252          DISPLAY ' LET -ABEND ON P IN 0100-RTN'                   ELP012  
00253              ' REQUEST RETURN IS  '  ELBHIO-REQUEST-TYPE          ELP012  
00254          MOVE ELBHIO-FEEDBACK  TO ABEND-CODE                      ELP012  
00255          GO TO 0900-ERROR-RTN.                                    ELP012  
00256                                                                   ELP012  
00257                                                                   ELP012  
00258        IF ELBHIO-DE-RECORD-PREFIX NOT EQUAL RL-RECORD-PREFIX      ELP012  
00259            NEXT SENTENCE                                          ELP012  
00260        ELSE                                                       ELP012  
00261            MOVE 'NO ' TO ELPDE-EOF-SW                             ELP012  
00262            PERFORM 0200-SEARCH-ELPDE-FILE THRU 0200-EXIT          ELP012  
00263                UNTIL ELPDE-END-OF-FILE.                           ELP012  
00264                                                                   ELP012  
00265  0100-EXIT.  EXIT.                                                ELP012  
00266 /                                                                 ELP012  
00267  0200-SEARCH-ELPDE-FILE.                                          ELP012  
00268                                                                   ELP012  
00269      MOVE 'DE'  TO  ELBHIO-FILE-ID.                               ELP012  
00270      MOVE 'G'   TO  ELBHIO-REQUEST-TYPE.                          ELP012  
00271                                                                   ELP012  
00272      CALL 'ELBIOPGM' USING  IO-INFO.                              ELP012  
00273                                                                   ELP012  
00274      IF ELBHIO-GOOD-RETURN                                        ELP012  
00275          NEXT SENTENCE                                            ELP012  
00276      ELSE                                                         ELP012  
00277          IF  ELBHIO-REQUEST-TYPE  =  '2'                          ELP012  
00278              MOVE 'YES' TO ELPDE-EOF-SW                           ELP012  
00279              IF SECOND-PROCESS                                    ELP012  
00280                  MOVE ELPDE-DEL-COUNT TO ELPDE-DEL-COUNT-X        ELP012  
00281                  MOVE ELPCV-DEL-COUNT TO ELPCV-DEL-COUNT-X        ELP012  
00282                  DISPLAY '***************************'            ELP012  
00283                  DISPLAY '    DATA ELEMENT LEVEL'                 ELP012  
00284                  DISPLAY ELPDE-DEL-COUNT-X ' DATA ELEMENTS WERE DEELP012  
00285 -                'LETED WHICH CONTAINED '                         ELP012  
00286                  DISPLAY ELPCV-DEL-COUNT-X ' CODE VALUES.'        ELP012  
00287                  GO TO 0200-EXIT                                  ELP012  
00288              ELSE                                                 ELP012  
00289                  GO TO 0200-EXIT                                  ELP012  
00290          ELSE                                                     ELP012  
00291              DISPLAY ' LET - ABEND ON G IN 0200-RTN'              ELP012  
00292              MOVE ELBHIO-FEEDBACK  TO  ABEND-CODE                 ELP012  
00293              GO TO 0900-ERROR-RTN.                                ELP012  
00294                                                                   ELP012  
00295      IF ELBHIO-DE-RECORD-PREFIX  =  LOW-VALUES                    ELP012  
00296          MOVE ELBHIO-ELPDE  TO  DATA-ELEMENT                      ELP012  
00297      ELSE                                                         ELP012  
00298          MOVE ELBHIO-ELPDE-NBR-DESC-LINES                         ELP012  
00299                             TO  DE-NBR-DESC-LINES                 ELP012  
00300          MOVE ELBHIO-ELPDE  TO  DATA-ELEMENT.                     ELP012  
00301                                                                   ELP012  
00302      IF FIRST-PROCESS AND                                         ELP012  
00303               DE-RECORD-PREFIX NOT EQUAL RL-RECORD-PREFIX         ELP012  
00304          MOVE 'YES' TO ELPDE-EOF-SW                               ELP012  
00305          GO TO 0200-EXIT.                                         ELP012  
00306                                                                   ELP012  
00307      IF DE-DELETE  OR  FIRST-PROCESS                              ELP012  
00308          NEXT SENTENCE                                            ELP012  
00309      ELSE                                                         ELP012  
00310        IF DE-REPRINT OR DE-ENG-NAME-CHG                           ELP012  
00311          MOVE SPACE TO DE-AUTO-REPRINT-FLAG                       ELP012  
00312          MOVE DE-NBR-DESC-LINES  TO  ELBHIO-ELPDE-NBR-DESC-LINES  ELP012  
00313          MOVE DATA-ELEMENT       TO  ELBHIO-ELPDE                 ELP012  
00314          MOVE 'U'  TO  ELBHIO-REQUEST-TYPE                        ELP012  
00315                                                                   ELP012  
00316          CALL 'ELBIOPGM' USING  IO-INFO                           ELP012  
00317                                                                   ELP012  
00318          IF ELBHIO-GOOD-RETURN                                    ELP012  
00319              NEXT SENTENCE                                        ELP012  
00320          ELSE                                                     ELP012  
00321              DISPLAY ' LET - ABEND ON U IN 0200-RTN'              ELP012  
00322              MOVE ELBHIO-FEEDBACK  TO ABEND-CODE                  ELP012  
00323              GO TO 0900-ERROR-RTN.                                ELP012  
00324                                                                   ELP012  
00325      IF DE-DELETE  OR  FIRST-PROCESS                              ELP012  
00326          NEXT SENTENCE                                            ELP012  
00327      ELSE                                                         ELP012  
00328          GO TO 0200-EXIT.                                         ELP012  
00329                                                                   ELP012  
00330      MOVE 'D'  TO  ELBHIO-REQUEST-TYPE.                           ELP012  
00331                                                                   ELP012  
00332      CALL 'ELBIOPGM' USING  IO-INFO.                              ELP012  
00333                                                                   ELP012  
00334      IF ELBHIO-GOOD-RETURN                                        ELP012  
00335          NEXT SENTENCE                                            ELP012  
00336      ELSE                                                         ELP012  
00337          DISPLAY ' LET - ABEND ON D IN 0200-RTN'                  ELP012  
00338          MOVE ELBHIO-FEEDBACK  TO ABEND-CODE                      ELP012  
00339          GO TO 0900-ERROR-RTN.                                    ELP012  
00340                                                                   ELP012  
00341      ADD 1 TO ELPDE-DEL-COUNT.                                    ELP012  
00342                                                                   ELP012  
00343      MOVE 'CV'              TO  ELBHIO-FILE-ID.                   ELP012  
00344      MOVE 'P'               TO  ELBHIO-REQUEST-TYPE.              ELP012  
00345      MOVE 27                TO  ELBHIO-RECORD-LENGTH.             ELP012  
00346      MOVE DE-RECORD-PREFIX  TO  ELBHIO-CV-RECORD-PREFIX.          ELP012  
00347      MOVE DE-ELEMENT-NBR    TO  ELBHIO-CV-ELEMENT-NBR.            ELP012  
00348      MOVE LOW-VALUES        TO  ELBHIO-CV-CODE-VALUE.             ELP012  
00349                                                                   ELP012  
00350      CALL 'ELBIOPGM' USING  IO-INFO.                              ELP012  
00351                                                                   ELP012  
00352      IF ELBHIO-GOOD-RETURN                                        ELP012  
00353          NEXT SENTENCE                                            ELP012  
00354      ELSE                                                         ELP012  
00355           DISPLAY ' LET - ABEND ON P IN 0200-RTN'                 ELP012  
00356                 '  REQUEST RETURN IS  '  ELBHIO-REQUEST-TYPE      ELP012  
00357          MOVE ELBHIO-FEEDBACK  TO ABEND-CODE                      ELP012  
00358          GO TO 0900-ERROR-RTN.                                    ELP012  
00359                                                                   ELP012  
00360                                                                   ELP012  
00361      IF ELBHIO-CV-RECORD-PREFIX NOT EQUAL DE-RECORD-PREFIX  AND   ELP012  
00362         ELBHIO-CV-ELEMENT-NBR   NOT EQUAL DE-ELEMENT-NBR          ELP012  
00363          NEXT SENTENCE                                            ELP012  
00364      ELSE                                                         ELP012  
00365          MOVE 'NO ' TO ELPCV-EOF-SW                               ELP012  
00366          PERFORM 0300-SEARCH-ELPCV-FILE THRU 0300-EXIT            ELP012  
00367              UNTIL ELPCV-END-OF-FILE.                             ELP012  
00368  0200-EXIT.  EXIT.                                                ELP012  
00369 /                                                                 ELP012  
00370  0300-SEARCH-ELPCV-FILE.                                          ELP012  
00371                                                                   ELP012  
00372      MOVE 'CV'  TO  ELBHIO-FILE-ID.                               ELP012  
00373      MOVE 'G'   TO  ELBHIO-REQUEST-TYPE.                          ELP012  
00374                                                                   ELP012  
00375      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP012  
00376                                                                   ELP012  
00377      IF ELBHIO-GOOD-RETURN                                        ELP012  
00378          NEXT SENTENCE                                            ELP012  
00379      ELSE                                                         ELP012  
00380          IF ELBHIO-REQUEST-TYPE  =  '2'                           ELP012  
00381              MOVE 'YES' TO ELPCV-EOF-SW                           ELP012  
00382              IF THIRD-PROCESS                                     ELP012  
00383                  MOVE ELPCV-DEL-COUNT TO ELPCV-DEL-COUNT-X        ELP012  
00384                  DISPLAY '***************************'            ELP012  
00385                  DISPLAY '     CODE VALUE LEVEL'                  ELP012  
00386                  DISPLAY ELPCV-DEL-COUNT-X ' CODE VALUES WERE DELEELP012  
00387 -                'TED.'                                           ELP012  
00388                  GO TO 0300-EXIT                                  ELP012  
00389              ELSE                                                 ELP012  
00390                  GO TO 0300-EXIT                                  ELP012  
00391          ELSE                                                     ELP012  
00392              DISPLAY ' LET -ABEND ON G IN 0300-RTN'               ELP012  
00393              MOVE ELBHIO-FEEDBACK  TO  ABEND-CODE                 ELP012  
00394              GO TO 0900-ERROR-RTN.                                ELP012  
00395                                                                   ELP012  
00396      IF ELBHIO-CV-RECORD-PREFIX  =  LOW-VALUES                    ELP012  
00397           MOVE ELBHIO-ELPCV  TO  ELEMENT-CODE-VALUE               ELP012  
00398      ELSE                                                         ELP012  
00399          MOVE ELBHIO-ELPCV-NBR-DESC-LINES                         ELP012  
00400                             TO  CV-NBR-VALUE-DESC-LINES           ELP012  
00401          MOVE ELBHIO-ELPCV  TO  ELEMENT-CODE-VALUE.               ELP012  
00402                                                                   ELP012  
00403      IF SECOND-PROCESS  OR  FIRST-PROCESS                         ELP012  
00404          IF CV-ELEMENT-NBR NOT EQUAL DE-ELEMENT-NBR               ELP012  
00405              MOVE 'YES' TO ELPCV-EOF-SW                           ELP012  
00406              GO TO 0300-EXIT.                                     ELP012  
00407                                                                   ELP012  
00408      IF CV-DELETE  OR  FIRST-PROCESS  OR  SECOND-PROCESS          ELP012  
00409          MOVE 'D'   TO  ELBHIO-REQUEST-TYPE                       ELP012  
00410                                                                   ELP012  
00411          CALL 'ELBIOPGM'  USING  IO-INFO                          ELP012  
00412                                                                   ELP012  
00413          IF ELBHIO-GOOD-RETURN                                    ELP012  
00414              NEXT SENTENCE                                        ELP012  
00415          ELSE                                                     ELP012  
00416              DISPLAY ' LET - ABEND ON D IN 0300-RTN'              ELP012  
00417              MOVE ELBHIO-FEEDBACK  TO ABEND-CODE                  ELP012  
00418              GO TO 0900-ERROR-RTN.                                ELP012  
00419                                                                   ELP012  
00420      IF CV-DELETE  OR  FIRST-PROCESS  OR  SECOND-PROCESS          ELP012  
00421          ADD 1 TO ELPCV-DEL-COUNT.                                ELP012  
00422  0300-EXIT. EXIT.                                                 ELP012  
00423 /                                                                 ELP012  
00424  0400-CLOSE-FILES.                                                ELP012  
00425 *--------------------------------------------------------------*  ELP012  
00426 *    ISSUE ONLY ONE CLOSE BUT ALL OF THE FILE RL, DE, CV, EN,  *  ELP012  
00427 * WILL BE CLOSED BY THE IO INTERFACE MODULE 'ELBIOPGM'.        *  ELP012  
00428 *--------------------------------------------------------------*  ELP012  
00429                                                                   ELP012  
00430      MOVE 'RL'  TO  ELBHIO-FILE-ID.                               ELP012  
00431      MOVE 'C'   TO  ELBHIO-REQUEST-TYPE.                          ELP012  
00432                                                                   ELP012  
00433      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP012  
00434                                                                   ELP012  
00435      IF ELBHIO-GOOD-RETURN                                        ELP012  
00436          NEXT SENTENCE                                            ELP012  
00437      ELSE                                                         ELP012  
00438          DISPLAY ' LET - ABEND ON CLOSE '                         ELP012  
00439          MOVE ELBHIO-FEEDBACK  TO ABEND-CODE                      ELP012  
00440          GO TO 0900-ERROR-RTN.                                    ELP012  
00441                                                                   ELP012  
00442  0400-EXIT.  EXIT.                                                ELP012  
00443 /                                                                 ELP012  
00444  0900-ERROR-RTN.                                                  ELP012  
00445                                                                   ELP012  
00446      CALL 'TSGEND' USING ABEND-CODE.                              ELP012  
00447                                                                   ELP012  
00448  0900-EXIT.                                                       ELP012  
00449      EXIT.                                                        ELP012  
