00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID.    ELBIOPGM.                                         ELBIOPGM
00003  AUTHOR.        NINA CERVANTES.                                      LV001
00004  INSTALLATION.  HCMS/BCBS.                                        ELBIOPGM
00005  DATE-WRITTEN.  MARCH, 1986.                                      ELBIOPGM
00006  DATE-COMPILED.                                                   ELBIOPGM
00007 ***************************************************************** ELBIOPGM
00008 *          ELBIOPGM: ENGLISH LANGUAGE BATCH I/O MODULE          * ELBIOPGM
00009 *  06/16/86  JTC  CHANGED THE FIXED LENGTH CONSTANTS USED       * ELBIOPGM
00010 *                 IN COMPUTING THE LENGTH OF THE DATA ELEMENT   * ELBIOPGM
00011 *                 AND CODE VALUE RECORDS.  CHANGED THE DATA     * ELBIOPGM
00012 *                 ELEMENT CONSTANT LENGTH TO 176 AND CHANGED    * ELBIOPGM
00013 *                 THE CODE VALUE CONSTANT LENGTH TO 80.         * ELBIOPGM
00014 *                 THIS WAS DONE BECAUSE THE OLD VALUES WERE     * ELBIOPGM
00015 *                 TOO SHORT.  COBOL VSAM NEED FOUR BYTES ADDED  * ELBIOPGM
00016 *                 TO THE TOTAL LENGTH.                          * ELBIOPGM
00017 *                 ALSO, CHANGED THE OUTPUT OF THE DATA ELEMENT  * ELBIOPGM
00018 *                 AND CODE VALUE RECORD SO THAT AT LEAST ONE    * ELBIOPGM
00019 *                 BLANK DESCRIPTION LINE WILL BE OUTPUT.        * ELBIOPGM
00020 *                                                               * ELBIOPGM
00021 ***************************************************************** ELBIOPGM
00022                                                                   ELBIOPGM
00023  ENVIRONMENT DIVISION.                                            ELBIOPGM
00024  CONFIGURATION SECTION.                                           ELBIOPGM
00025  SOURCE-COMPUTER.  IBM-370.                                       ELBIOPGM
00026  OBJECT-COMPUTER.  IBM-370.                                       ELBIOPGM
00027                                                                   ELBIOPGM
00028  DATA DIVISION.                                                   ELBIOPGM
00029  WORKING-STORAGE SECTION.                                         ELBIOPGM
00030  77  FILLER   PIC X(32) VALUE 'WORKING STORAGE PROGRAM ELBIOPGM'. ELBIOPGM
00031  77  ELPDE-FIXED-PORTION-LENGTH   PIC S999  VALUE +176.           ELBIOPGM
00032  77  ELPCV-FIXED-PORTION-LENGTH   PIC S999  VALUE +080.           ELBIOPGM
00033  77  DESCRIPTION-LENGTH           PIC S999  VALUE +079.           ELBIOPGM
00034  01  ABEND-CODE                   PIC 9(4)   COMP VALUE ZEROES.   ELBIOPGM
00035                                                                   ELBIOPGM
00036 **  I/O TABLE  **                                                 ELBIOPGM
00037  01  IO-TABLE-DEFINITION.                                         ELBIOPGM
00038      03  IO-OPEN                  PIC X   VALUE 'N'.              ELBIOPGM
00039          88  OPEN-PERFORMED               VALUE 'Y'.              ELBIOPGM
00040          88  OPEN-NOT-PERFORMED           VALUE 'N'.              ELBIOPGM
00041      03  IO-CLOSE                 PIC X   VALUE 'N'.              ELBIOPGM
00042          88  CLOSE-PERFORMED              VALUE 'Y'.              ELBIOPGM
00043          88  CLOSE-NOT-PERFORMED          VALUE 'N'.              ELBIOPGM
00044                                                                   ELBIOPGM
00045 **  RECORD LIST FILE  **                                          ELBIOPGM
00046  01  PARM-1.                                                      ELBIOPGM
00047      03  1-RESERVED-FLDS         PIC 9(8)    VALUE ZEROS COMP.    ELBIOPGM
00048      03  1-RESERV-FLDS REDEFINES 1-RESERVED-FLDS.                 ELBIOPGM
00049          05  1-REQUEST-TYPE      PIC X.                           ELBIOPGM
00050          05  FILLER              PIC X(3).                        ELBIOPGM
00051                                                                   ELBIOPGM
00052  01  PARM-1A.                                                     ELBIOPGM
00053      03  1A-RDW.                                                  ELBIOPGM
00054          05  1A-REC-LENG         PIC 9(4)    VALUE ZEROS COMP.    ELBIOPGM
00055          05  1A-FEEDBACK         PIC 9(4)    VALUE ZEROS COMP.    ELBIOPGM
00056      03  1A-REC-AREA.                                             ELBIOPGM
00057      COPY  ELPRLC.                                                ELBIOPGM
00058 /                                                                 ELBIOPGM
00059 **  DATA ELEMENT FILE  **                                         ELBIOPGM
00060  01  PARM-2.                                                      ELBIOPGM
00061      03  2-RESERVED-FLDS         PIC 9(8)    VALUE ZEROS COMP.    ELBIOPGM
00062      03  2-RESERV-FLDS REDEFINES 2-RESERVED-FLDS.                 ELBIOPGM
00063          05  2-REQUEST-TYPE      PIC X.                           ELBIOPGM
00064          05  FILLER              PIC X(3).                        ELBIOPGM
00065                                                                   ELBIOPGM
00066  01  PARM-2A.                                                     ELBIOPGM
00067      03  2A-RDW.                                                  ELBIOPGM
00068          05  2A-REC-LENG         PIC 9(4)    VALUE ZEROS COMP.    ELBIOPGM
00069          05  2A-FEEDBACK         PIC 9(4)    VALUE ZEROS COMP.    ELBIOPGM
00070      03  2A-REC-AREA.                                             ELBIOPGM
00071      COPY ELPDEC.                                                 ELBIOPGM
00072 /                                                                 ELBIOPGM
00073 **  CODE VALUE FILE  **                                           ELBIOPGM
00074  01  PARM-3.                                                      ELBIOPGM
00075      03  3-RESERVED-FLDS         PIC 9(8)    VALUE ZEROS COMP.    ELBIOPGM
00076      03  3-RESERV-FLDS REDEFINES 3-RESERVED-FLDS.                 ELBIOPGM
00077          05  3-REQUEST-TYPE      PIC X.                           ELBIOPGM
00078          05  FILLER              PIC X(3).                        ELBIOPGM
00079                                                                   ELBIOPGM
00080  01  PARM-3A.                                                     ELBIOPGM
00081      03  3A-RDW.                                                  ELBIOPGM
00082          05  3A-REC-LENG         PIC 9(4)    VALUE ZEROS COMP.    ELBIOPGM
00083          05  3A-FEEDBACK         PIC 9(4)    VALUE ZEROS COMP.    ELBIOPGM
00084      03  3A-REC-AREA.                                             ELBIOPGM
00085      COPY  ELPCVC.                                                ELBIOPGM
00086 /                                                                 ELBIOPGM
00087 **  ENGLISH NAME FILE  **                                         ELBIOPGM
00088  01  PARM-4.                                                      ELBIOPGM
00089      03  4-RESERVED-FLDS         PIC 9(8)    VALUE ZEROS COMP.    ELBIOPGM
00090      03  4-RESERV-FLDS REDEFINES 4-RESERVED-FLDS.                 ELBIOPGM
00091          05  4-REQUEST-TYPE      PIC X.                           ELBIOPGM
00092          05  FILLER              PIC X(3).                        ELBIOPGM
00093                                                                   ELBIOPGM
00094  01  PARM-4A.                                                     ELBIOPGM
00095      03  4A-RDW.                                                  ELBIOPGM
00096          05  4A-REC-LENG         PIC 9(4)    VALUE ZEROS COMP.    ELBIOPGM
00097          05  4A-FEEDBACK         PIC 9(4)    VALUE ZEROS COMP.    ELBIOPGM
00098      03  4A-REC-AREA.                                             ELBIOPGM
00099      COPY ELPENC.                                                 ELBIOPGM
00100                                                                   ELBIOPGM
00101 /                                                                 ELBIOPGM
00102 **  COBOL XREF FILE **                                            ELBIOPGM
00103  01  PARM-5.                                                      ELBIOPGM
00104      03  5-RESERVED-FLDS         PIC 9(8)    VALUE ZEROS COMP.    ELBIOPGM
00105      03  5-RESERV-FLDS REDEFINES 5-RESERVED-FLDS.                 ELBIOPGM
00106          05  5-REQUEST-TYPE      PIC X.                           ELBIOPGM
00107          05  FILLER              PIC X(3).                        ELBIOPGM
00108                                                                   ELBIOPGM
00109  01  PARM-5A.                                                     ELBIOPGM
00110      03  5A-RDW.                                                  ELBIOPGM
00111          05  5A-REC-LENG         PIC 9(4)    VALUE ZEROS COMP.    ELBIOPGM
00112          05  5A-FEEDBACK         PIC 9(4)    VALUE ZEROS COMP.    ELBIOPGM
00113      03  5A-REC-AREA.                                             ELBIOPGM
00114      COPY  ELPCNC.                                                ELBIOPGM
00115                                                                   ELBIOPGM
00116  01  PARM-SET.                                                    ELBIOPGM
00117      03  SET-RDW.                                                 ELBIOPGM
00118          05  SET-REC-LENG        PIC 9(4)    VALUE ZEROS COMP.    ELBIOPGM
00119          05  SET-FEEDBACK        PIC 9(4)    VALUE ZEROS COMP.    ELBIOPGM
00120      03  SET-VALUE               PIC 9(8)    VALUE ZEROS COMP.    ELBIOPGM
00121                                                                   ELBIOPGM
00122  LINKAGE SECTION.                                                 ELBIOPGM
00123  01  IO.                                                          ELBIOPGM
00124      COPY ELBHIOPM.                                               ELBIOPGM
00125 /                                                                 ELBIOPGM
00126  PROCEDURE DIVISION   USING IO.                                   ELBIOPGM
00127                                                                   ELBIOPGM
00128      MOVE '00' TO ELBHIO-RETURN-CODE.                             ELBIOPGM
00129                                                                   ELBIOPGM
00130      PERFORM 1000-MAINLINE THRU 1000-EXIT.                        ELBIOPGM
00131                                                                   ELBIOPGM
00132      GOBACK.                                                      ELBIOPGM
00133 /                                                                 ELBIOPGM
00134  1000-MAINLINE.                                                   ELBIOPGM
00135                                                                   ELBIOPGM
00136      PERFORM 2000-CHECK-ELBHIO-FILE-ID  THRU 2000-EXIT.           ELBIOPGM
00137                                                                   ELBIOPGM
00138      PERFORM 3000-CHECK-ELBHIO-REQUEST-TYPE THRU 3000-EXIT.       ELBIOPGM
00139                                                                   ELBIOPGM
00140      PERFORM 4000-DETERMINE-FUNCTION THRU 4000-EXIT.              ELBIOPGM
00141                                                                   ELBIOPGM
00142  1000-EXIT.  EXIT.                                                ELBIOPGM
00143 /                                                                 ELBIOPGM
00144  2000-CHECK-ELBHIO-FILE-ID.                                       ELBIOPGM
00145      IF RECORD-LIST-FILE            OR                            ELBIOPGM
00146         DATA-ELEMENT-FILE           OR                            ELBIOPGM
00147         CODE-VALUE-FILE                                           ELBIOPGM
00148          NEXT SENTENCE                                            ELBIOPGM
00149      ELSE                                                         ELBIOPGM
00150         IF ENGLISH-NAME-FILE           OR                         ELBIOPGM
00151            COBOL-XREF-NAME-FILE                                   ELBIOPGM
00152             MOVE '05' TO ELBHIO-RETURN-CODE                       ELBIOPGM
00153             GOBACK                                                ELBIOPGM
00154         ELSE                                                      ELBIOPGM
00155             MOVE '01' TO ELBHIO-RETURN-CODE                       ELBIOPGM
00156             GOBACK.                                               ELBIOPGM
00157                                                                   ELBIOPGM
00158  2000-EXIT.  EXIT.                                                ELBIOPGM
00159 /                                                                 ELBIOPGM
00160  3000-CHECK-ELBHIO-REQUEST-TYPE.                                  ELBIOPGM
00161                                                                   ELBIOPGM
00162      IF ELBHIO-OPEN                  OR                           ELBIOPGM
00163         ELBHIO-CLOSE                 OR                           ELBIOPGM
00164         ELBHIO-T-CLOSE               OR                           ELBIOPGM
00165         ELBHIO-SEQUENTIAL-GET        OR                           ELBIOPGM
00166         ELBHIO-SEQUENTIAL-ADD        OR                           ELBIOPGM
00167         ELBHIO-SEQUENTIAL-LOAD       OR                           ELBIOPGM
00168         ELBHIO-READ                  OR                           ELBIOPGM
00169         ELBHIO-WRITE                 OR                           ELBIOPGM
00170         ELBHIO-UPDATE                OR                           ELBIOPGM
00171         ELBHIO-DELETE                OR                           ELBIOPGM
00172         ELBHIO-ERASE                 OR                           ELBIOPGM
00173         ELBHIO-INSERT                OR                           ELBIOPGM
00174         ELBHIO-POINT                 OR                           ELBIOPGM
00175         ELBHIO-SET                                                ELBIOPGM
00176          NEXT SENTENCE                                            ELBIOPGM
00177      ELSE                                                         ELBIOPGM
00178          MOVE '02' TO ELBHIO-RETURN-CODE                          ELBIOPGM
00179          GOBACK.                                                  ELBIOPGM
00180                                                                   ELBIOPGM
00181  3000-EXIT.  EXIT.                                                ELBIOPGM
00182 /                                                                 ELBIOPGM
00183  4000-DETERMINE-FUNCTION.                                         ELBIOPGM
00184      IF ELBHIO-OPEN                                               ELBIOPGM
00185          PERFORM 4100-OPEN-ROUTINE THRU 4100-EXIT                 ELBIOPGM
00186      ELSE                                                         ELBIOPGM
00187        IF ELBHIO-CLOSE                                            ELBIOPGM
00188            PERFORM 4200-CLOSE-ROUTINE THRU 4200-EXIT              ELBIOPGM
00189        ELSE                                                       ELBIOPGM
00190            PERFORM 5000-STANDARD-ROUTINE THRU 5000-EXIT.          ELBIOPGM
00191                                                                   ELBIOPGM
00192  4000-EXIT.  EXIT.                                                ELBIOPGM
00193 /                                                                 ELBIOPGM
00194  4100-OPEN-ROUTINE.                                               ELBIOPGM
00195                                                                   ELBIOPGM
00196      IF OPEN-NOT-PERFORMED                                        ELBIOPGM
00197          MOVE 'Y' TO IO-OPEN                                      ELBIOPGM
00198      ELSE                                                         ELBIOPGM
00199          GO TO 4100-EXIT.                                         ELBIOPGM
00200                                                                   ELBIOPGM
00201 ***                                                               ELBIOPGM
00202 ** AN AUTOMATIC SET OPERATION WILL BE PERFORMED AGAINST ALL FILES ELBIOPGM
00203 ***                                                               ELBIOPGM
00204                                                                   ELBIOPGM
00205      MOVE 'S'                    TO 1-REQUEST-TYPE.               ELBIOPGM
00206      MOVE 8                      TO SET-REC-LENG.                 ELBIOPGM
00207      MOVE 3                      TO SET-VALUE.                    ELBIOPGM
00208      CALL 'TSGVSAM1' USING PARM-1 PARM-SET.                       ELBIOPGM
00209      IF  1-REQUEST-TYPE NOT EQUAL 'S'                             ELBIOPGM
00210          MOVE SET-FEEDBACK       TO ABEND-CODE                    ELBIOPGM
00211          DISPLAY 'NAC-SET1'                                       ELBIOPGM
00212          GO TO 9000-ERROR-ROUTINE.                                ELBIOPGM
00213                                                                   ELBIOPGM
00214      MOVE 'S'                    TO 2-REQUEST-TYPE.               ELBIOPGM
00215      MOVE 8                      TO SET-REC-LENG.                 ELBIOPGM
00216      MOVE 3                      TO SET-VALUE.                    ELBIOPGM
00217      CALL 'TSGVSAM2' USING PARM-2 PARM-SET.                       ELBIOPGM
00218      IF  2-REQUEST-TYPE NOT EQUAL 'S'                             ELBIOPGM
00219          MOVE SET-FEEDBACK       TO ABEND-CODE                    ELBIOPGM
00220          DISPLAY 'NAC-SET2'                                       ELBIOPGM
00221          GO TO 9000-ERROR-ROUTINE.                                ELBIOPGM
00222                                                                   ELBIOPGM
00223      MOVE 'S'                    TO 3-REQUEST-TYPE.               ELBIOPGM
00224      MOVE 8                      TO SET-REC-LENG.                 ELBIOPGM
00225      MOVE 3                      TO SET-VALUE.                    ELBIOPGM
00226      CALL 'TSGVSAM3' USING PARM-3 PARM-SET.                       ELBIOPGM
00227      IF  3-REQUEST-TYPE NOT EQUAL 'S'                             ELBIOPGM
00228          MOVE SET-FEEDBACK       TO ABEND-CODE                    ELBIOPGM
00229          DISPLAY 'NAC-SET3'                                       ELBIOPGM
00230          GO TO 9000-ERROR-ROUTINE.                                ELBIOPGM
00231                                                                   ELBIOPGM
00232      MOVE 'S'                    TO 4-REQUEST-TYPE.               ELBIOPGM
00233      MOVE 8                      TO SET-REC-LENG.                 ELBIOPGM
00234      MOVE 3                      TO SET-VALUE.                    ELBIOPGM
00235      CALL 'TSGVSAM4' USING PARM-4 PARM-SET.                       ELBIOPGM
00236      IF  4-REQUEST-TYPE NOT EQUAL 'S'                             ELBIOPGM
00237          MOVE SET-FEEDBACK       TO ABEND-CODE                    ELBIOPGM
00238          DISPLAY 'NAC-SET4'                                       ELBIOPGM
00239          GO TO 9000-ERROR-ROUTINE.                                ELBIOPGM
00240                                                                   ELBIOPGM
00241      MOVE 'S'                    TO 5-REQUEST-TYPE.               ELBIOPGM
00242      MOVE 8                      TO SET-REC-LENG.                 ELBIOPGM
00243      MOVE 3                      TO SET-VALUE.                    ELBIOPGM
00244      CALL 'TSGVSAM5' USING PARM-5 PARM-SET.                       ELBIOPGM
00245      IF  5-REQUEST-TYPE NOT EQUAL 'S'                             ELBIOPGM
00246          MOVE SET-FEEDBACK       TO ELBHIO-FEEDBACK               ELBIOPGM
00247          DISPLAY 'NAC-SET5'                                       ELBIOPGM
00248          GO TO 9000-ERROR-ROUTINE.                                ELBIOPGM
00249                                                                   ELBIOPGM
00250 ***                                                               ELBIOPGM
00251 **  AN AUTOMATIC OPEN OPERATION WILL BE PERFORMED AGAINST ALL     ELBIOPGM
00252 **  FILES.                                                        ELBIOPGM
00253 ***                                                               ELBIOPGM
00254                                                                   ELBIOPGM
00255      MOVE 'O'                    TO 1-REQUEST-TYPE.               ELBIOPGM
00256      CALL 'TSGVSAM1' USING PARM-1 PARM-1A.                        ELBIOPGM
00257      IF  1-REQUEST-TYPE NOT EQUAL 'O'                             ELBIOPGM
00258          MOVE 1A-FEEDBACK        TO ABEND-CODE                    ELBIOPGM
00259          DISPLAY 'NAC-OPEN1'                                      ELBIOPGM
00260          GO TO 9000-ERROR-ROUTINE.                                ELBIOPGM
00261                                                                   ELBIOPGM
00262      MOVE 'O'                    TO 2-REQUEST-TYPE.               ELBIOPGM
00263      CALL 'TSGVSAM2' USING PARM-2 PARM-2A.                        ELBIOPGM
00264      IF  2-REQUEST-TYPE NOT EQUAL 'O'                             ELBIOPGM
00265          MOVE 2A-FEEDBACK        TO ABEND-CODE                    ELBIOPGM
00266          DISPLAY 'NAC-OPEN2'                                      ELBIOPGM
00267          GO TO 9000-ERROR-ROUTINE.                                ELBIOPGM
00268                                                                   ELBIOPGM
00269      MOVE 'O'                    TO 3-REQUEST-TYPE.               ELBIOPGM
00270      CALL 'TSGVSAM3' USING PARM-3 PARM-3A.                        ELBIOPGM
00271      IF  3-REQUEST-TYPE NOT EQUAL 'O'                             ELBIOPGM
00272          MOVE 3A-FEEDBACK        TO ABEND-CODE                    ELBIOPGM
00273          DISPLAY 'NAC-OPEN3'                                      ELBIOPGM
00274          GO TO 9000-ERROR-ROUTINE.                                ELBIOPGM
00275                                                                   ELBIOPGM
00276      MOVE 'O'                    TO 4-REQUEST-TYPE.               ELBIOPGM
00277      CALL 'TSGVSAM4' USING PARM-4 PARM-4A.                        ELBIOPGM
00278      IF  4-REQUEST-TYPE NOT EQUAL 'O'                             ELBIOPGM
00279          MOVE 4A-FEEDBACK        TO ABEND-CODE                    ELBIOPGM
00280          DISPLAY 'NAC-OPEN4'                                      ELBIOPGM
00281          GO TO 9000-ERROR-ROUTINE.                                ELBIOPGM
00282                                                                   ELBIOPGM
00283      MOVE 'O'                    TO 5-REQUEST-TYPE.               ELBIOPGM
00284      CALL 'TSGVSAM5' USING PARM-5 PARM-5A.                        ELBIOPGM
00285      IF  5-REQUEST-TYPE NOT EQUAL 'O'                             ELBIOPGM
00286          MOVE 5A-FEEDBACK        TO ABEND-CODE                    ELBIOPGM
00287          DISPLAY 'NAC-OPEN5'                                      ELBIOPGM
00288          GO TO 9000-ERROR-ROUTINE.                                ELBIOPGM
00289                                                                   ELBIOPGM
00290  4100-EXIT.  EXIT.                                                ELBIOPGM
00291 /                                                                 ELBIOPGM
00292  4200-CLOSE-ROUTINE.                                              ELBIOPGM
00293                                                                   ELBIOPGM
00294      IF CLOSE-NOT-PERFORMED                                       ELBIOPGM
00295          MOVE 'Y' TO IO-CLOSE                                     ELBIOPGM
00296      ELSE                                                         ELBIOPGM
00297          GO TO 4200-EXIT.                                         ELBIOPGM
00298                                                                   ELBIOPGM
00299 ***                                                               ELBIOPGM
00300 **  AN AUTOMATIC CLOSE OPERATION WILL BE PERFORMED AGAINST ALL    ELBIOPGM
00301 **  FILES.                                                        ELBIOPGM
00302 ***                                                               ELBIOPGM
00303                                                                   ELBIOPGM
00304      MOVE 'C'                    TO 1-REQUEST-TYPE.               ELBIOPGM
00305      CALL 'TSGVSAM1' USING PARM-1 PARM-1A.                        ELBIOPGM
00306      IF  1-REQUEST-TYPE NOT EQUAL 'C'                             ELBIOPGM
00307          MOVE 1A-FEEDBACK        TO ABEND-CODE                    ELBIOPGM
00308          DISPLAY 'NAC-CLOSE1'                                     ELBIOPGM
00309          GO TO 9000-ERROR-ROUTINE.                                ELBIOPGM
00310                                                                   ELBIOPGM
00311      MOVE 'C'                    TO 2-REQUEST-TYPE.               ELBIOPGM
00312      CALL 'TSGVSAM2' USING PARM-2 PARM-2A.                        ELBIOPGM
00313      IF  2-REQUEST-TYPE NOT EQUAL 'C'                             ELBIOPGM
00314          MOVE 2A-FEEDBACK        TO ABEND-CODE                    ELBIOPGM
00315          DISPLAY 'NAC-CLOSE2'                                     ELBIOPGM
00316          GO TO 9000-ERROR-ROUTINE.                                ELBIOPGM
00317                                                                   ELBIOPGM
00318      MOVE 'C'                    TO 3-REQUEST-TYPE.               ELBIOPGM
00319      CALL 'TSGVSAM3' USING PARM-3 PARM-3A.                        ELBIOPGM
00320      IF  3-REQUEST-TYPE NOT EQUAL 'C'                             ELBIOPGM
00321          MOVE 3A-FEEDBACK        TO ABEND-CODE                    ELBIOPGM
00322          DISPLAY 'NAC-CLOSE3'                                     ELBIOPGM
00323          GO TO 9000-ERROR-ROUTINE.                                ELBIOPGM
00324                                                                   ELBIOPGM
00325      MOVE 'C'                    TO 4-REQUEST-TYPE.               ELBIOPGM
00326      CALL 'TSGVSAM4' USING PARM-4 PARM-4A.                        ELBIOPGM
00327      IF  4-REQUEST-TYPE NOT EQUAL 'C'                             ELBIOPGM
00328          MOVE 4A-FEEDBACK        TO ABEND-CODE                    ELBIOPGM
00329          DISPLAY 'NAC-CLOSE4'                                     ELBIOPGM
00330          GO TO 9000-ERROR-ROUTINE.                                ELBIOPGM
00331                                                                   ELBIOPGM
00332      MOVE 'C'                    TO 5-REQUEST-TYPE.               ELBIOPGM
00333      CALL 'TSGVSAM5' USING PARM-5 PARM-5A.                        ELBIOPGM
00334      IF  5-REQUEST-TYPE NOT EQUAL 'C'                             ELBIOPGM
00335          MOVE 5A-FEEDBACK        TO ABEND-CODE                    ELBIOPGM
00336          DISPLAY 'NAC-CLOSE5'                                     ELBIOPGM
00337          GO TO 9000-ERROR-ROUTINE.                                ELBIOPGM
00338                                                                   ELBIOPGM
00339  4200-EXIT.  EXIT.                                                ELBIOPGM
00340 /                                                                 ELBIOPGM
00341  5000-STANDARD-ROUTINE.                                           ELBIOPGM
00342                                                                   ELBIOPGM
00343      IF RECORD-LIST-FILE                                          ELBIOPGM
00344          PERFORM 5100-ISSUE-ELPRL-CALL THRU 5100-EXIT             ELBIOPGM
00345      ELSE                                                         ELBIOPGM
00346        IF DATA-ELEMENT-FILE                                       ELBIOPGM
00347            PERFORM 5200-ISSUE-ELPDE-CALL THRU 5200-EXIT           ELBIOPGM
00348        ELSE                                                       ELBIOPGM
00349            PERFORM 5300-ISSUE-ELPCV-CALL THRU 5300-EXIT.          ELBIOPGM
00350                                                                   ELBIOPGM
00351  5000-EXIT.  EXIT.                                                ELBIOPGM
00352 /                                                                 ELBIOPGM
00353  5100-ISSUE-ELPRL-CALL.                                           ELBIOPGM
00354                                                                   ELBIOPGM
00355      MOVE ELBHIO-REQUEST-TYPE    TO 1-REQUEST-TYPE.               ELBIOPGM
00356                                                                   ELBIOPGM
00357      IF ELBHIO-SEQUENTIAL-ADD    OR                               ELBIOPGM
00358         ELBHIO-SEQUENTIAL-LOAD   OR                               ELBIOPGM
00359         ELBHIO-READ              OR                               ELBIOPGM
00360         ELBHIO-WRITE             OR                               ELBIOPGM
00361         ELBHIO-ERASE             OR                               ELBIOPGM
00362         ELBHIO-POINT                                              ELBIOPGM
00363          MOVE ELBHIO-RECORD-LENGTH  TO 1A-REC-LENG                ELBIOPGM
00364          MOVE ELBHIO-ELPRL-KEY      TO RL-RECORD-PREFIX.          ELBIOPGM
00365                                                                   ELBIOPGM
00366      IF ELBHIO-SEQUENTIAL-ADD    OR                               ELBIOPGM
00367         ELBHIO-SEQUENTIAL-LOAD   OR                               ELBIOPGM
00368         ELBHIO-WRITE             OR                               ELBIOPGM
00369         ELBHIO-UPDATE            OR                               ELBIOPGM
00370         ELBHIO-INSERT                                             ELBIOPGM
00371          MOVE ELBHIO-ELPRL          TO 1A-REC-AREA.               ELBIOPGM
00372                                                                   ELBIOPGM
00373      CALL 'TSGVSAM1' USING PARM-1 PARM-1A.                        ELBIOPGM
00374      IF  1-REQUEST-TYPE NOT EQUAL ELBHIO-REQUEST-TYPE             ELBIOPGM
00375          MOVE '03'               TO ELBHIO-RETURN-CODE            ELBIOPGM
00376          MOVE 1-REQUEST-TYPE     TO ELBHIO-REQUEST-TYPE           ELBIOPGM
00377          MOVE 1A-FEEDBACK        TO ELBHIO-FEEDBACK               ELBIOPGM
00378      ELSE                                                         ELBIOPGM
00379          MOVE 1A-REC-AREA        TO ELBHIO-ELPRL.                 ELBIOPGM
00380                                                                   ELBIOPGM
00381                                                                   ELBIOPGM
00382  5100-EXIT.  EXIT.                                                ELBIOPGM
00383 /                                                                 ELBIOPGM
00384  5200-ISSUE-ELPDE-CALL.                                           ELBIOPGM
00385                                                                   ELBIOPGM
00386      IF ELBHIO-SEQUENTIAL-ADD    OR                               ELBIOPGM
00387         ELBHIO-SEQUENTIAL-LOAD   OR                               ELBIOPGM
00388         ELBHIO-WRITE             OR                               ELBIOPGM
00389         ELBHIO-INSERT                                             ELBIOPGM
00390          PERFORM 5210-CHECK-ELPEN-FOR-DUPS THRU 5210-EXIT         ELBIOPGM
00391          IF ELBHIO-COBOL-NAME NOT EQUAL SPACES                    ELBIOPGM
00392              PERFORM 5220-CHECK-ELPCN-FOR-DUPS THRU 5220-EXIT.    ELBIOPGM
00393                                                                   ELBIOPGM
00394      IF ELBHIO-UPDATE  OR                                         ELBIOPGM
00395         ELBHIO-DELETE  OR                                         ELBIOPGM
00396         ELBHIO-ERASE                                              ELBIOPGM
00397          PERFORM 5230-CHECK-ELPDE-KEY-CHANGES THRU 5230-EXIT.     ELBIOPGM
00398                                                                   ELBIOPGM
00399                                                                   ELBIOPGM
00400      IF ELBHIO-READ              OR                               ELBIOPGM
00401         ELBHIO-ERASE             OR                               ELBIOPGM
00402         ELBHIO-POINT                                              ELBIOPGM
00403          MOVE ELBHIO-RECORD-LENGTH  TO 2A-REC-LENG                ELBIOPGM
00404          MOVE ELBHIO-ELPDE-KEY      TO DE-PRIMARY-KEY.            ELBIOPGM
00405                                                                   ELBIOPGM
00406      IF ELBHIO-SEQUENTIAL-ADD    OR                               ELBIOPGM
00407         ELBHIO-SEQUENTIAL-LOAD   OR                               ELBIOPGM
00408         ELBHIO-WRITE             OR                               ELBIOPGM
00409         ELBHIO-UPDATE            OR                               ELBIOPGM
00410         ELBHIO-INSERT                                             ELBIOPGM
00411          IF ELBHIO-ELPDE-NBR-DESC-LINES GREATER THAN ZERO         ELBIOPGM
00412              COMPUTE ELBHIO-RECORD-LENGTH =                       ELBIOPGM
00413                ELPDE-FIXED-PORTION-LENGTH +                       ELBIOPGM
00414                (ELBHIO-ELPDE-NBR-DESC-LINES * DESCRIPTION-LENGTH) ELBIOPGM
00415              MOVE ELBHIO-RECORD-LENGTH TO 2A-REC-LENG             ELBIOPGM
00416              MOVE ELBHIO-ELPDE-NBR-DESC-LINES TO DE-NBR-DESC-LINESELBIOPGM
00417              MOVE ELBHIO-ELPDE         TO 2A-REC-AREA             ELBIOPGM
00418          ELSE                                                     ELBIOPGM
00419              COMPUTE 2A-REC-LENG = ELPDE-FIXED-PORTION-LENGTH +   ELBIOPGM
00420                                    DESCRIPTION-LENGTH             ELBIOPGM
00421              MOVE 1  TO DE-NBR-DESC-LINES                         ELBIOPGM
00422              MOVE SPACES TO DE-DESC-LINE(1)                       ELBIOPGM
00423              MOVE ELBHIO-ELPDE          TO 2A-REC-AREA.           ELBIOPGM
00424                                                                   ELBIOPGM
00425      MOVE ELBHIO-REQUEST-TYPE    TO 2-REQUEST-TYPE.               ELBIOPGM
00426      CALL 'TSGVSAM2' USING PARM-2 PARM-2A.                        ELBIOPGM
00427      IF  2-REQUEST-TYPE NOT EQUAL ELBHIO-REQUEST-TYPE             ELBIOPGM
00428          MOVE '03'               TO ELBHIO-RETURN-CODE            ELBIOPGM
00429          MOVE 2-REQUEST-TYPE     TO ELBHIO-REQUEST-TYPE           ELBIOPGM
00430          MOVE 2A-FEEDBACK        TO ELBHIO-FEEDBACK               ELBIOPGM
00431          GO TO 5200-EXIT                                          ELBIOPGM
00432      ELSE                                                         ELBIOPGM
00433          IF ELBHIO-SEQUENTIAL-GET                                 ELBIOPGM
00434                     OR                                            ELBIOPGM
00435             ELBHIO-READ                                           ELBIOPGM
00436              IF DE-RECORD-PREFIX EQUAL LOW-VALUES                 ELBIOPGM
00437                  MOVE 2A-REC-AREA    TO ELBHIO-ELPDE              ELBIOPGM
00438              ELSE                                                 ELBIOPGM
00439                  MOVE DE-NBR-DESC-LINES  TO                       ELBIOPGM
00440                                      ELBHIO-ELPDE-NBR-DESC-LINES  ELBIOPGM
00441                  MOVE 2A-REC-AREA    TO ELBHIO-ELPDE.             ELBIOPGM
00442                                                                   ELBIOPGM
00443      IF ELBHIO-SEQUENTIAL-ADD   OR                                ELBIOPGM
00444         ELBHIO-SEQUENTIAL-LOAD  OR                                ELBIOPGM
00445         ELBHIO-WRITE            OR                                ELBIOPGM
00446         ELBHIO-INSERT                                             ELBIOPGM
00447          PERFORM 5400-ISSUE-ELPEN-CALL  THRU  5400-EXIT           ELBIOPGM
00448          IF DE-COBOL-NAME NOT EQUAL SPACES                        ELBIOPGM
00449              PERFORM 5500-ISSUE-ELPCN-CALL  THRU  5500-EXIT.      ELBIOPGM
00450                                                                   ELBIOPGM
00451  5200-EXIT.  EXIT.                                                ELBIOPGM
00452 /                                                                 ELBIOPGM
00453  5210-CHECK-ELPEN-FOR-DUPS.                                       ELBIOPGM
00454                                                                   ELBIOPGM
00455      MOVE 'R'                  TO 4-REQUEST-TYPE.                 ELBIOPGM
00456      MOVE 87                   TO 4A-REC-LENG.                    ELBIOPGM
00457      MOVE ELBHIO-DE-RECORD-PREFIX  TO EN-RECORD-PREFIX.           ELBIOPGM
00458      MOVE ELBHIO-ELEMENT-NAME      TO EN-ELEMENT-NAME.            ELBIOPGM
00459      CALL 'TSGVSAM4' USING PARM-4 PARM-4A.                        ELBIOPGM
00460      IF  4-REQUEST-TYPE NOT EQUAL '3'                             ELBIOPGM
00461          IF  4-REQUEST-TYPE EQUAL 'R'                             ELBIOPGM
00462              MOVE '06'               TO ELBHIO-RETURN-CODE        ELBIOPGM
00463              MOVE 4-REQUEST-TYPE     TO ELBHIO-REQUEST-TYPE       ELBIOPGM
00464              MOVE 4A-FEEDBACK        TO ELBHIO-FEEDBACK           ELBIOPGM
00465              GOBACK                                               ELBIOPGM
00466          ELSE                                                     ELBIOPGM
00467              MOVE 4A-FEEDBACK        TO ABEND-CODE                ELBIOPGM
00468              DISPLAY 'NAC-READ41'                                 ELBIOPGM
00469              GO TO 9000-ERROR-ROUTINE.                            ELBIOPGM
00470  5210-EXIT.  EXIT.                                                ELBIOPGM
00471 /                                                                 ELBIOPGM
00472  5220-CHECK-ELPCN-FOR-DUPS.                                       ELBIOPGM
00473                                                                   ELBIOPGM
00474      MOVE 'R'                  TO 5-REQUEST-TYPE.                 ELBIOPGM
00475      MOVE 42                   TO 5A-REC-LENG.                    ELBIOPGM
00476      MOVE ELBHIO-DE-RECORD-PREFIX TO CN-RECORD-PREFIX.            ELBIOPGM
00477      MOVE ELBHIO-COBOL-NAME       TO CN-COBOL-NAME.               ELBIOPGM
00478      CALL 'TSGVSAM5' USING PARM-5 PARM-5A.                        ELBIOPGM
00479      IF  5-REQUEST-TYPE NOT EQUAL '3'                             ELBIOPGM
00480          IF  5-REQUEST-TYPE EQUAL 'R'                             ELBIOPGM
00481              MOVE '07'               TO ELBHIO-RETURN-CODE        ELBIOPGM
00482              MOVE 5-REQUEST-TYPE     TO ELBHIO-REQUEST-TYPE       ELBIOPGM
00483              MOVE 5A-FEEDBACK        TO ELBHIO-FEEDBACK           ELBIOPGM
00484              GOBACK                                               ELBIOPGM
00485          ELSE                                                     ELBIOPGM
00486              MOVE 5A-FEEDBACK        TO ABEND-CODE                ELBIOPGM
00487              DISPLAY 'NAC-READ51'                                 ELBIOPGM
00488              GO TO 9000-ERROR-ROUTINE.                            ELBIOPGM
00489                                                                   ELBIOPGM
00490  5220-EXIT.  EXIT.                                                ELBIOPGM
00491 /                                                                 ELBIOPGM
00492  5230-CHECK-ELPDE-KEY-CHANGES.                                    ELBIOPGM
00493                                                                   ELBIOPGM
00494      MOVE 'R' TO 2-REQUEST-TYPE.                                  ELBIOPGM
00495      MOVE 15                   TO 2A-REC-LENG.                    ELBIOPGM
00496      MOVE ELBHIO-ELPDE-KEY     TO DE-PRIMARY-KEY.                 ELBIOPGM
00497      CALL 'TSGVSAM2' USING PARM-2 PARM-2A.                        ELBIOPGM
00498      IF  2-REQUEST-TYPE NOT EQUAL 'R'                             ELBIOPGM
00499          MOVE 2A-FEEDBACK        TO ABEND-CODE                    ELBIOPGM
00500          DISPLAY 'NAC-READ21'                                     ELBIOPGM
00501          GO TO 9000-ERROR-ROUTINE.                                ELBIOPGM
00502                                                                   ELBIOPGM
00503                                                                   ELBIOPGM
00504      IF ELBHIO-ELEMENT-NAME NOT EQUAL DE-ELEMENT-NAME  OR         ELBIOPGM
00505          ELBHIO-DELETE  OR  ELBHIO-ERASE                          ELBIOPGM
00506          PERFORM 5240-DELETE-OLD-ELPEN THRU 5240-EXIT.            ELBIOPGM
00507                                                                   ELBIOPGM
00508      IF ELBHIO-COBOL-NAME NOT EQUAL DE-COBOL-NAME  OR             ELBIOPGM
00509          ELBHIO-DELETE  OR  ELBHIO-ERASE                          ELBIOPGM
00510          IF DE-COBOL-NAME EQUAL SPACES                            ELBIOPGM
00511              NEXT SENTENCE                                        ELBIOPGM
00512          ELSE                                                     ELBIOPGM
00513              PERFORM 5250-DELETE-OLD-ELPCN THRU 5250-EXIT.        ELBIOPGM
00514                                                                   ELBIOPGM
00515  5230-EXIT.  EXIT.                                                ELBIOPGM
00516 /                                                                 ELBIOPGM
00517  5240-DELETE-OLD-ELPEN.                                           ELBIOPGM
00518                                                                   ELBIOPGM
00519      MOVE 'D'                  TO 4-REQUEST-TYPE.                 ELBIOPGM
00520      MOVE 87                   TO 4A-REC-LENG.                    ELBIOPGM
00521      MOVE DE-RECORD-PREFIX     TO EN-RECORD-PREFIX.               ELBIOPGM
00522      MOVE DE-ELEMENT-NAME      TO EN-ELEMENT-NAME.                ELBIOPGM
00523      CALL 'TSGVSAM4' USING PARM-4 PARM-4A.                        ELBIOPGM
00524      IF  4-REQUEST-TYPE NOT EQUAL 'D'                             ELBIOPGM
00525          MOVE 4A-FEEDBACK        TO ABEND-CODE                    ELBIOPGM
00526          DISPLAY 'NAC-DELETE42'                                   ELBIOPGM
00527          GO TO 9000-ERROR-ROUTINE.                                ELBIOPGM
00528                                                                   ELBIOPGM
00529  5240-EXIT.  EXIT.                                                ELBIOPGM
00530 /                                                                 ELBIOPGM
00531  5250-DELETE-OLD-ELPCN.                                           ELBIOPGM
00532                                                                   ELBIOPGM
00533      MOVE 'D'                  TO 5-REQUEST-TYPE.                 ELBIOPGM
00534      MOVE 42                   TO 5A-REC-LENG.                    ELBIOPGM
00535      MOVE DE-RECORD-PREFIX     TO CN-RECORD-PREFIX.               ELBIOPGM
00536      MOVE DE-COBOL-NAME        TO CN-COBOL-NAME.                  ELBIOPGM
00537      CALL 'TSGVSAM5' USING PARM-5 PARM-5A.                        ELBIOPGM
00538      IF  5-REQUEST-TYPE NOT EQUAL 'D'                             ELBIOPGM
00539          MOVE 5A-FEEDBACK        TO ABEND-CODE                    ELBIOPGM
00540          DISPLAY 'NAC-DELETE52'                                   ELBIOPGM
00541          GO TO 9000-ERROR-ROUTINE.                                ELBIOPGM
00542                                                                   ELBIOPGM
00543  5250-EXIT.  EXIT.                                                ELBIOPGM
00544 /                                                                 ELBIOPGM
00545  5300-ISSUE-ELPCV-CALL.                                           ELBIOPGM
00546                                                                   ELBIOPGM
00547      MOVE ELBHIO-REQUEST-TYPE    TO 3-REQUEST-TYPE.               ELBIOPGM
00548                                                                   ELBIOPGM
00549      IF ELBHIO-SEQUENTIAL-ADD    OR                               ELBIOPGM
00550         ELBHIO-SEQUENTIAL-LOAD   OR                               ELBIOPGM
00551         ELBHIO-READ              OR                               ELBIOPGM
00552         ELBHIO-WRITE             OR                               ELBIOPGM
00553         ELBHIO-ERASE             OR                               ELBIOPGM
00554         ELBHIO-POINT                                              ELBIOPGM
00555          MOVE ELBHIO-RECORD-LENGTH  TO 3A-REC-LENG                ELBIOPGM
00556          MOVE ELBHIO-ELPCV-KEY      TO CV-CODE-KEY.               ELBIOPGM
00557                                                                   ELBIOPGM
00558      IF ELBHIO-SEQUENTIAL-ADD    OR                               ELBIOPGM
00559         ELBHIO-SEQUENTIAL-LOAD   OR                               ELBIOPGM
00560         ELBHIO-WRITE             OR                               ELBIOPGM
00561         ELBHIO-UPDATE            OR                               ELBIOPGM
00562         ELBHIO-INSERT                                             ELBIOPGM
00563          IF ELBHIO-ELPCV-NBR-DESC-LINES GREATER THAN ZERO         ELBIOPGM
00564              COMPUTE ELBHIO-RECORD-LENGTH =                       ELBIOPGM
00565                ELPCV-FIXED-PORTION-LENGTH +                       ELBIOPGM
00566                (ELBHIO-ELPCV-NBR-DESC-LINES * DESCRIPTION-LENGTH) ELBIOPGM
00567              MOVE ELBHIO-RECORD-LENGTH TO 3A-REC-LENG             ELBIOPGM
00568              MOVE ELBHIO-ELPCV-NBR-DESC-LINES TO                  ELBIOPGM
00569                                     CV-NBR-VALUE-DESC-LINES       ELBIOPGM
00570              MOVE ELBHIO-ELPCV         TO 3A-REC-AREA             ELBIOPGM
00571          ELSE                                                     ELBIOPGM
00572              COMPUTE 3A-REC-LENG = ELPCV-FIXED-PORTION-LENGTH +   ELBIOPGM
00573                                    DESCRIPTION-LENGTH             ELBIOPGM
00574              MOVE 1      TO CV-NBR-VALUE-DESC-LINES               ELBIOPGM
00575              MOVE SPACES TO CV-VALUE-DESC-LINE(1)                 ELBIOPGM
00576              MOVE ELBHIO-ELPCV          TO 3A-REC-AREA.           ELBIOPGM
00577                                                                   ELBIOPGM
00578      CALL 'TSGVSAM3' USING PARM-3 PARM-3A.                        ELBIOPGM
00579      IF  3-REQUEST-TYPE NOT EQUAL ELBHIO-REQUEST-TYPE             ELBIOPGM
00580          MOVE '03'               TO ELBHIO-RETURN-CODE            ELBIOPGM
00581          MOVE 3-REQUEST-TYPE     TO ELBHIO-REQUEST-TYPE           ELBIOPGM
00582          MOVE 3A-FEEDBACK        TO ELBHIO-FEEDBACK               ELBIOPGM
00583      ELSE                                                         ELBIOPGM
00584          IF ELBHIO-SEQUENTIAL-GET                                 ELBIOPGM
00585                     OR                                            ELBIOPGM
00586             ELBHIO-READ                                           ELBIOPGM
00587              IF CV-RECORD-PREFIX EQUAL LOW-VALUES                 ELBIOPGM
00588                  MOVE 3A-REC-AREA    TO ELBHIO-ELPCV              ELBIOPGM
00589              ELSE                                                 ELBIOPGM
00590                  MOVE CV-NBR-VALUE-DESC-LINES TO                  ELBIOPGM
00591                            ELBHIO-ELPCV-NBR-DESC-LINES            ELBIOPGM
00592                  MOVE 3A-REC-AREA        TO ELBHIO-ELPCV.         ELBIOPGM
00593                                                                   ELBIOPGM
00594                                                                   ELBIOPGM
00595  5300-EXIT.  EXIT.                                                ELBIOPGM
00596 /                                                                 ELBIOPGM
00597  5400-ISSUE-ELPEN-CALL.                                           ELBIOPGM
00598                                                                   ELBIOPGM
00599                                                                   ELBIOPGM
00600      IF ELBHIO-SEQUENTIAL-ADD    OR                               ELBIOPGM
00601         ELBHIO-SEQUENTIAL-LOAD   OR                               ELBIOPGM
00602         ELBHIO-WRITE             OR                               ELBIOPGM
00603         ELBHIO-INSERT                                             ELBIOPGM
00604          MOVE 'I'                  TO 4-REQUEST-TYPE              ELBIOPGM
00605          MOVE 91                   TO 4A-REC-LENG                 ELBIOPGM
00606          MOVE SPACES               TO 4A-REC-AREA                 ELBIOPGM
00607          MOVE DE-RECORD-PREFIX     TO EN-RECORD-PREFIX            ELBIOPGM
00608          MOVE DE-ELEMENT-NAME      TO EN-ELEMENT-NAME             ELBIOPGM
00609          MOVE DE-ELEMENT-NBR       TO EN-ELEMENT-NBR              ELBIOPGM
00610          CALL 'TSGVSAM4' USING PARM-4 PARM-4A                     ELBIOPGM
00611          IF  4-REQUEST-TYPE NOT EQUAL 'I'                         ELBIOPGM
00612              MOVE 4A-FEEDBACK        TO ABEND-CODE                ELBIOPGM
00613              DISPLAY 'NAC-INSERT44'                               ELBIOPGM
00614              GO TO 9000-ERROR-ROUTINE.                            ELBIOPGM
00615                                                                   ELBIOPGM
00616                                                                   ELBIOPGM
00617  5400-EXIT.  EXIT.                                                ELBIOPGM
00618 /                                                                 ELBIOPGM
00619  5500-ISSUE-ELPCN-CALL.                                           ELBIOPGM
00620                                                                   ELBIOPGM
00621                                                                   ELBIOPGM
00622      IF ELBHIO-SEQUENTIAL-ADD    OR                               ELBIOPGM
00623         ELBHIO-SEQUENTIAL-LOAD   OR                               ELBIOPGM
00624         ELBHIO-WRITE             OR                               ELBIOPGM
00625         ELBHIO-INSERT                                             ELBIOPGM
00626          MOVE 'I'                  TO 5-REQUEST-TYPE              ELBIOPGM
00627          MOVE 46                   TO 5A-REC-LENG                 ELBIOPGM
00628          MOVE SPACES               TO 5A-REC-AREA                 ELBIOPGM
00629          MOVE DE-RECORD-PREFIX     TO CN-RECORD-PREFIX            ELBIOPGM
00630          MOVE DE-COBOL-NAME        TO CN-COBOL-NAME               ELBIOPGM
00631          MOVE DE-ELEMENT-NBR       TO CN-ELEMENT-NBR              ELBIOPGM
00632          CALL 'TSGVSAM5' USING PARM-5 PARM-5A                     ELBIOPGM
00633          IF  5-REQUEST-TYPE NOT EQUAL 'I'                         ELBIOPGM
00634              MOVE 5A-FEEDBACK        TO ABEND-CODE                ELBIOPGM
00635                  DISPLAY 'NAC-INSERT54'                           ELBIOPGM
00636              GO TO 9000-ERROR-ROUTINE.                            ELBIOPGM
00637                                                                   ELBIOPGM
00638  5500-EXIT.  EXIT.                                                ELBIOPGM
00639 /                                                                 ELBIOPGM
00640  9000-ERROR-ROUTINE.                                              ELBIOPGM
00641                                                                   ELBIOPGM
00642      CALL 'TSGEND' USING ABEND-CODE.                              ELBIOPGM
00643                                                                   ELBIOPGM
00644  9000-EXIT.                                                       ELBIOPGM
00645      EXIT.                                                        ELBIOPGM
