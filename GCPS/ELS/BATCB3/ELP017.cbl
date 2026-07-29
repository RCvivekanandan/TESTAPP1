00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID.    ELP017.                                           ELP017  
00003  AUTHOR.        JOHN CURIN, KEANE, INC.                              LV001
00004  INSTALLATION.  HCMS/BCBS.                                        ELP017  
00005  DATE-WRITTEN.  FEBRUARY 1986.                                    ELP017  
00006  DATE-COMPILED.                                                   ELP017  
00007 ***************************************************************** ELP017  
00008 *            ELP017 - ELS: CREATION OF CROSSINDEX NAME FILE     * ELP017  
00009 *                                                               * ELP017  
00010 *      1) READ THROUGH THE ENTIRE DATA ELEMENT FILE AND FOR     * ELP017  
00011 *         EACH DATA ELEMENT CREATE A CROSS REFERENCE RECORD.    * ELP017  
00012 *         THE KEY FOR THE CROSS REFERENCE RECORD IS THE RECORD  * ELP017  
00013 *         PREFIX AND THE ENGLISH NAME.  THE RECORD CONSIST OF   * ELP017  
00014 *         THE RECORD PREFIX, ENGLISH NAME ELEMENT NUMBER AND A  * ELP017  
00015 *         DELETE FLAG.  THE CROSS REFERENCE FILE IS BEING       * ELP017  
00016 *         CREATED BECAUSE OF THE PROCESSING PROBLEMS ENCOUNTED  * ELP017  
00017 *         BY USING THE ENGLISH NAME AS AN ALTERNATE INDEX ON    * ELP017  
00018 *         THE DATA ELEMENT VSAM FILE.                           * ELP017  
00019 *                                                               * ELP017  
00020 ***************************************************************** ELP017  
00021                                                                   ELP017  
00022  ENVIRONMENT DIVISION.                                            ELP017  
00023  CONFIGURATION SECTION.                                           ELP017  
00024  SOURCE-COMPUTER.  IBM-370.                                       ELP017  
00025  OBJECT-COMPUTER.  IBM-370.                                       ELP017  
00026  INPUT-OUTPUT SECTION.                                            ELP017  
00027  FILE-CONTROL.                                                    ELP017  
00028                                                                   ELP017  
00029  DATA DIVISION.                                                   ELP017  
00030  FILE SECTION.                                                    ELP017  
00031                                                                   ELP017  
00032  WORKING-STORAGE SECTION.                                         ELP017  
00033  77  FILLER   PIC X(30) VALUE 'WORKING STORAGE PROGRAM ELP017'.   ELP017  
00034  77  ELPDE-STATUS            PIC XX     VALUE ZEROES.             ELP017  
00035  77  ELPEN-STATUS            PIC XX     VALUE ZEROES.             ELP017  
00036  77  ELPDE-EOF-SW            PIC XXX    VALUE 'NO '.              ELP017  
00037          88 ELPDE-END-OF-FILE           VALUE 'YES'.              ELP017  
00038  01  ERR-PRIMARY-KEY.                                             ELP017  
00039      03  ERR-RECORD-PREFIX   PIC X(8).                            ELP017  
00040      03  ERR-ELEMENT-NBR     PIC 9(5).                            ELP017  
00041      03  ERR-ENGLISH-NAME    PIC X(75).                           ELP017  
00042  01  ABEND-CODE              PIC 9(4)   COMP VALUE ZERO.          ELP017  
00043  01  ELPDE-COUNT             PIC 9(11)  VALUE ZEROES.             ELP017  
00044  01  ELPDE-COUNT-X           PIC Z(10)9.                          ELP017  
00045  01  ELPEN-COUNT             PIC 9(11)  VALUE ZEROES.             ELP017  
00046  01  ELPEN-COUNT-X           PIC Z(10)9.                          ELP017  
00047  01  ELPCN-COUNT             PIC 9(11)  VALUE ZEROES.             ELP017  
00048  01  ELPCN-COUNT-X           PIC Z(10)9.                          ELP017  
00049                                                                   ELP017  
00050 ******** PARAMETER WORK AREAS FOR COBLVSAM ****************       ELP017  
00051                                                                   ELP017  
00052  01  PARM-ONE-DE.                                                 ELP017  
00053      05  RESERVED-DE      PIC 9(5) COMP VALUE 0.                  ELP017  
00054      05  RESERVED-DE-X    REDEFINES RESERVED-DE.                  ELP017  
00055          10  REQUEST-TYPE-DE   PIC X.                             ELP017  
00056                                                                   ELP017  
00057  01  PARM-TWO-DE.                                                 ELP017  
00058      05  RDW-DE.                                                  ELP017  
00059          10  RECORD-LENGTH-DE  PIC 99 COMP.                       ELP017  
00060          10  FEEDBACK-CODE-DE  PIC 99 COMP VALUE 0.               ELP017  
00061  COPY ELPDEC.                                                     ELP017  
00062 /                                                                 ELP017  
00063  01  PARM-ONE-EN.                                                 ELP017  
00064      05  RESERVED-EN      PIC 9(5) COMP VALUE 0.                  ELP017  
00065      05  RESERVED-EN-X    REDEFINES RESERVED-EN.                  ELP017  
00066          10  REQUEST-TYPE-EN   PIC X.                             ELP017  
00067                                                                   ELP017  
00068  01  PARM-TWO-EN.                                                 ELP017  
00069      05  RDW-EN.                                                  ELP017  
00070          10  RECORD-LENGTH-EN  PIC 99 COMP.                       ELP017  
00071          10  FEEDBACK-CODE-EN  PIC 99 COMP VALUE 0.               ELP017  
00072  COPY ELPENC.                                                     ELP017  
00073 /                                                                 ELP017  
00074  01  PARM-ONE-CN.                                                 ELP017  
00075      05  RESERVED-CN      PIC 9(5) COMP VALUE 0.                  ELP017  
00076      05  RESERVED-CN-X    REDEFINES RESERVED-CN.                  ELP017  
00077          10  REQUEST-TYPE-CN   PIC X.                             ELP017  
00078                                                                   ELP017  
00079  01  PARM-TWO-CN.                                                 ELP017  
00080      05  RDW-CN.                                                  ELP017  
00081          10  RECORD-LENGTH-CN  PIC 99 COMP.                       ELP017  
00082          10  FEEDBACK-CODE-CN  PIC 99 COMP VALUE 0.               ELP017  
00083  COPY ELPCNC.                                                     ELP017  
00084                                                                   ELP017  
00085  01  PARM-TWO-SET.                                                ELP017  
00086      05  RDW-SET.                                                 ELP017  
00087          10  RECORD-LENGTH-SET PIC 99 COMP.                       ELP017  
00088          10  FEEDBACK-CODE-SET PIC 99 COMP VALUE 0.               ELP017  
00089      05  SET-VALUE             PIC 9(5) COMP.                     ELP017  
00090 /                                                                 ELP017  
00091  PROCEDURE DIVISION.                                              ELP017  
00092  0000-MAINLINE.                                                   ELP017  
00093                                                                   ELP017  
00094      MOVE 'S' TO REQUEST-TYPE-DE.                                 ELP017  
00095      MOVE 8   TO RECORD-LENGTH-SET.                               ELP017  
00096      MOVE 3   TO SET-VALUE.                                       ELP017  
00097      CALL 'TSGVSAM1' USING PARM-ONE-DE PARM-TWO-SET.              ELP017  
00098      IF REQUEST-TYPE-DE NOT = 'S'                                 ELP017  
00099           DISPLAY 'SET FAILED DATA ELEMENT REQUEST CD = '         ELP017  
00100                   REQUEST-TYPE-DE                                 ELP017  
00101           MOVE 1000 TO ABEND-CODE                                 ELP017  
00102           CALL 'TSGEND' USING ABEND-CODE.                         ELP017  
00103      MOVE 'O' TO REQUEST-TYPE-DE.                                 ELP017  
00104      CALL 'TSGVSAM1' USING PARM-ONE-DE, PARM-TWO-DE.              ELP017  
00105      IF REQUEST-TYPE-DE NOT = 'O'                                 ELP017  
00106         DISPLAY 'OPEN FAILED ON DATA ELEMENT FILE, REQUEST CD = ' ELP017  
00107                   REQUEST-TYPE-DE                                 ELP017  
00108         MOVE 1050 TO ABEND-CODE                                   ELP017  
00109         CALL 'TSGEND' USING ABEND-CODE.                           ELP017  
00110                                                                   ELP017  
00111      MOVE 'S' TO REQUEST-TYPE-EN.                                 ELP017  
00112      MOVE 8   TO RECORD-LENGTH-SET.                               ELP017  
00113      MOVE 3   TO SET-VALUE.                                       ELP017  
00114      CALL 'TSGVSAM2' USING PARM-ONE-EN PARM-TWO-SET.              ELP017  
00115      IF REQUEST-TYPE-EN NOT = 'S'                                 ELP017  
00116           DISPLAY 'SET FAILED ENGISH NAME REQUEST CD = '          ELP017  
00117                   REQUEST-TYPE-EN                                 ELP017  
00118           MOVE 1100 TO ABEND-CODE                                 ELP017  
00119           CALL 'TSGEND' USING ABEND-CODE.                         ELP017  
00120      MOVE 'O' TO REQUEST-TYPE-EN.                                 ELP017  
00121      CALL 'TSGVSAM2' USING PARM-ONE-EN, PARM-TWO-EN.              ELP017  
00122      IF REQUEST-TYPE-EN NOT = 'O'                                 ELP017  
00123        DISPLAY 'OPEN FAILED ON ENGLISH NAME FILE, REQUEST CD = '  ELP017  
00124                  REQUEST-TYPE-EN                                  ELP017  
00125        MOVE 1150 TO ABEND-CODE                                    ELP017  
00126        CALL 'TSGEND' USING ABEND-CODE.                            ELP017  
00127                                                                   ELP017  
00128      MOVE 'S' TO REQUEST-TYPE-CN.                                 ELP017  
00129      MOVE 8   TO RECORD-LENGTH-SET.                               ELP017  
00130      MOVE 3   TO SET-VALUE.                                       ELP017  
00131      CALL 'TSGVSAM3' USING PARM-ONE-CN PARM-TWO-SET.              ELP017  
00132      IF REQUEST-TYPE-CN NOT = 'S'                                 ELP017  
00133           DISPLAY 'SET FAILED COBOL NAME REQUEST CD = '           ELP017  
00134                   REQUEST-TYPE-CN                                 ELP017  
00135           MOVE 1200 TO ABEND-CODE                                 ELP017  
00136           CALL 'TSGEND' USING ABEND-CODE.                         ELP017  
00137      MOVE 'O' TO REQUEST-TYPE-CN.                                 ELP017  
00138      CALL 'TSGVSAM3' USING PARM-ONE-CN, PARM-TWO-CN.              ELP017  
00139      IF REQUEST-TYPE-CN NOT = 'O'                                 ELP017  
00140           DISPLAY 'OPEN FAILED ON COBOL NAME FILE, REQUEST CD = ' ELP017  
00141                   REQUEST-TYPE-CN                                 ELP017  
00142           MOVE 1250 TO ABEND-CODE                                 ELP017  
00143           CALL 'TSGEND' USING ABEND-CODE.                         ELP017  
00144                                                                   ELP017  
00145      MOVE 'P'        TO REQUEST-TYPE-DE.                          ELP017  
00146      MOVE LOW-VALUES TO DE-PRIMARY-KEY.                           ELP017  
00147      MOVE 15         TO RECORD-LENGTH-DE.                         ELP017  
00148      CALL 'TSGVSAM1' USING PARM-ONE-DE PARM-TWO-DE.               ELP017  
00149      IF REQUEST-TYPE-DE NOT = 'P'                                 ELP017  
00150          DISPLAY '*********************************'              ELP017  
00151          DISPLAY '      ELP017 START ERR MSG       '              ELP017  
00152          DISPLAY '    FILE STATUS = ' FEEDBACK-CODE-DE            ELP017  
00153          MOVE DE-RECORD-PREFIX TO ERR-RECORD-PREFIX               ELP017  
00154          MOVE DE-ELEMENT-NBR   TO ERR-ELEMENT-NBR                 ELP017  
00155          MOVE SPACES           TO ERR-ENGLISH-NAME                ELP017  
00156          DISPLAY '    RECORD KEY = '  ERR-PRIMARY-KEY             ELP017  
00157          DISPLAY '*********************************'              ELP017  
00158          DISPLAY 'SET FAILED COBOL NAME REQUEST CD = '            ELP017  
00159             REQUEST-TYPE-DE                                       ELP017  
00160          MOVE 2000 TO ABEND-CODE                                  ELP017  
00161          CALL 'TSGEND' USING ABEND-CODE.                          ELP017  
00162      MOVE 'NO ' TO ELPDE-EOF-SW.                                  ELP017  
00163      PERFORM 0100-READ-ELPDE-FILE THRU 0100-EXIT                  ELP017  
00164          UNTIL ELPDE-END-OF-FILE.                                 ELP017  
00165                                                                   ELP017  
00166      MOVE 'C' TO REQUEST-TYPE-DE.                                 ELP017  
00167      CALL 'TSGVSAM1' USING PARM-ONE-DE, PARM-TWO-DE.              ELP017  
00168      IF REQUEST-TYPE-DE NOT = 'C'                                 ELP017  
00169       DISPLAY 'CLOSE FAILED ON DATA ELEMENT FILE, REQUEST CD = '  ELP017  
00170                  REQUEST-TYPE-DE                                  ELP017  
00171       MOVE 2500 TO ABEND-CODE                                     ELP017  
00172       CALL 'TSGEND' USING ABEND-CODE.                             ELP017  
00173                                                                   ELP017  
00174      MOVE 'C' TO REQUEST-TYPE-EN.                                 ELP017  
00175      CALL 'TSGVSAM2' USING PARM-ONE-EN, PARM-TWO-EN.              ELP017  
00176      IF REQUEST-TYPE-EN NOT = 'C'                                 ELP017  
00177       DISPLAY 'CLOSE FAILED ON ENGLISH NAME FILE, REQUEST CD = '  ELP017  
00178                  REQUEST-TYPE-EN                                  ELP017  
00179       MOVE 2550 TO ABEND-CODE                                     ELP017  
00180       CALL 'TSGEND' USING ABEND-CODE.                             ELP017  
00181                                                                   ELP017  
00182      MOVE 'C' TO REQUEST-TYPE-CN.                                 ELP017  
00183      CALL 'TSGVSAM1' USING PARM-ONE-CN, PARM-TWO-CN.              ELP017  
00184      IF REQUEST-TYPE-CN NOT = 'C'                                 ELP017  
00185          DISPLAY 'CLOSE FAILED ON COBOL NAME FILE, REQUEST CD = ' ELP017  
00186                  REQUEST-TYPE-DE                                  ELP017  
00187          MOVE 2600 TO ABEND-CODE                                  ELP017  
00188          CALL 'TSGEND' USING ABEND-CODE.                          ELP017  
00189                                                                   ELP017  
00190      STOP RUN.                                                    ELP017  
00191  0000-EXIT. EXIT.                                                 ELP017  
00192 /                                                                 ELP017  
00193  0100-READ-ELPDE-FILE.                                            ELP017  
00194      MOVE 'G' TO REQUEST-TYPE-DE.                                 ELP017  
00195      CALL 'TSGVSAM1' USING PARM-ONE-DE, PARM-TWO-DE.              ELP017  
00196      IF REQUEST-TYPE-DE = '2'                                     ELP017  
00197          MOVE 'YES' TO ELPDE-EOF-SW                               ELP017  
00198      ELSE                                                         ELP017  
00199       IF REQUEST-TYPE-DE NOT = 'G'                                ELP017  
00200              DISPLAY '***************************'                ELP017  
00201              DISPLAY 'READ REQUEST FAILED FOR DATA ELEMENT FILE'  ELP017  
00202              DISPLAY ' FILE STATUS IS ' FEEDBACK-CODE-DE          ELP017  
00203              DISPLAY '***************************'                ELP017  
00204              MOVE 2050 TO ABEND-CODE                              ELP017  
00205              CALL 'TSGABEND' USING ABEND-CODE.                    ELP017  
00206                                                                   ELP017  
00207      IF ELPDE-END-OF-FILE                                         ELP017  
00208              MOVE ELPDE-COUNT TO ELPDE-COUNT-X                    ELP017  
00209              MOVE ELPEN-COUNT TO ELPEN-COUNT-X                    ELP017  
00210              MOVE ELPCN-COUNT TO ELPCN-COUNT-X                    ELP017  
00211              DISPLAY '***************************'                ELP017  
00212              DISPLAY ELPEN-COUNT-X ' CROSS REFERENCE RECORDS WERE ELP017  
00213 -            'CREATED AND '                                       ELP017  
00214              DISPLAY ELPCN-COUNT-X ' COBOL CROSS REFERENCE RECORDSELP017  
00215 -            ' WERE CREATED '                                     ELP017  
00216              DISPLAY 'FOR ' ELPDE-COUNT-X ' RECORDS'              ELP017  
00217              DISPLAY '***************************'                ELP017  
00218              GO TO 0100-EXIT.                                     ELP017  
00219      IF DE-RECORD-PREFIX = LOW-VALUES                             ELP017  
00220          GO TO 0100-EXIT.                                         ELP017  
00221      ADD 1 TO ELPDE-COUNT.                                        ELP017  
00222                                                                   ELP017  
00223      MOVE DE-RECORD-PREFIX       TO EN-RECORD-PREFIX,             ELP017  
00224                                     CN-RECORD-PREFIX.             ELP017  
00225      MOVE DE-ELEMENT-NBR         TO EN-ELEMENT-NBR,               ELP017  
00226                                     CN-ELEMENT-NBR.               ELP017  
00227      MOVE DE-ELEMENT-NAME        TO EN-ELEMENT-NAME.              ELP017  
00228      MOVE DE-COBOL-NAME          TO CN-COBOL-NAME.                ELP017  
00229      MOVE DE-DELETE-ELEMENT-FLAG TO EN-DELETE-ELEMENT-FLAG,       ELP017  
00230                                     CN-DELETE-ELEMENT-FLAG.       ELP017  
00231                                                                   ELP017  
00232      MOVE 'I' TO REQUEST-TYPE-EN.                                 ELP017  
00233      MOVE 91  TO RECORD-LENGTH-EN.                                ELP017  
00234      CALL 'TSGVSAM2' USING PARM-ONE-EN, PARM-TWO-EN.              ELP017  
00235      IF REQUEST-TYPE-EN NOT = 'I'                                 ELP017  
00236           DISPLAY '*************************'                     ELP017  
00237           DISPLAY ' WRITE FAILED FOR CROSS REFERENCE FILE'        ELP017  
00238           DISPLAY 'RECORD KEY IS ' EN-KEY                         ELP017  
00239           DISPLAY 'STATUS IS ' FEEDBACK-CODE-EN                   ELP017  
00240           MOVE 3000 TO ABEND-CODE                                 ELP017  
00241           CALL 'TSGEND' USING ABEND-CODE.                         ELP017  
00242      ADD 1 TO ELPEN-COUNT.                                        ELP017  
00243                                                                   ELP017  
00244      IF CN-COBOL-NAME = SPACES                                    ELP017  
00245           GO TO 0100-EXIT.                                        ELP017  
00246      MOVE 'I' TO REQUEST-TYPE-CN.                                 ELP017  
00247      MOVE 46  TO RECORD-LENGTH-CN.                                ELP017  
00248      CALL 'TSGVSAM3' USING PARM-ONE-CN, PARM-TWO-CN.              ELP017  
00249      IF REQUEST-TYPE-CN NOT = 'I'                                 ELP017  
00250           DISPLAY '*************************'                     ELP017  
00251           DISPLAY ' WRITE FAILED FOR COBOL CROSS REFERENCE FILE'  ELP017  
00252           DISPLAY 'RECORD KEY IS ' CN-KEY                         ELP017  
00253           DISPLAY ' ELEMENT NUMBER IS ' CN-ELEMENT-NBR            ELP017  
00254           DISPLAY 'STATUS IS ' FEEDBACK-CODE-CN                   ELP017  
00255      ELSE                                                         ELP017  
00256        ADD +1  TO ELPCN-COUNT.                                    ELP017  
00257  0100-EXIT.  EXIT.                                                ELP017  
00258 /                                                                 ELP017  
