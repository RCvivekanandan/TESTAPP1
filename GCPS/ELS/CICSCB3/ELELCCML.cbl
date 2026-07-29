00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID.  ELELCCML.                                           ELELCCML
00003  AUTHOR.   JOHN CURIN --- KEANE,INC.                                 LV001
00004  DATE-WRITTEN.  09/30/85.                                         ELELCCML
00005  DATE-COMPILED.                                                   ELELCCML
00006 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ELELCCML
00007 *       CCCCCCC OOOOOOOO  BBBBBBBBB OOOOOOOO LLLL      2222222222 ELELCCML
00008 *     CCC      OOO   OOO BBB   BBB OOO   OOO LLL        222  222  ELELCCML
00009 *    CCC      OOO   OOO BBB   BBB OOO   OOO LLL        222  222   ELELCCML
00010 *   CCC      OOO   OOO BBBBBBBBB OOO   OOO LLL        222  222    ELELCCML
00011 *  CCC      OOO   OOO BBB   BBB OOO   OOO LLL        222  222     ELELCCML
00012 * CCC      OOO   OOO BBB   BBB OOO   OOO LLL        222  222   *  ELELCCML
00013 * CCCCCCCC OOOOOOOO BBBBBBBBB OOOOOOOOO LLLLLLLLL 2222222222  *   ELELCCML
00014 /*****************************************************************ELELCCML
00015 *        * * * U P D A T E   H I S T O R Y * * *                  ELELCCML
00016                                                                   ELELCCML
00017 *  DATE        BY   COMMENTS                                      ELELCCML
00018                                                                   ELELCCML
00019 * 09/30/85    JTC  CREATED                                        ELELCCML
00020                                                                   ELELCCML
00021 * 11/12/87    AKK  ADDING CODE THAT WILL ALLOW USER TO LOCATE     ELELCCML
00022 *                  THE NEXT DESIRED DATA ELEMENT BY KEYING IN     ELELCCML
00023 *                  AND 'L' AND A PARTIAL OR FULL DATA ELEMENT     ELELCCML
00024 *                  NAME.                                          ELELCCML
00025                                                                   ELELCCML
00026 * 03-21-88    AKK  CHANGED CODE UNDER 5000-NAME-SEARCH FROM       ELELCCML
00027 *                  MOVE NAME-SEARCH TO CA-SEL-ELEMENT-NBR TO      ELELCCML
00028 *                  MOVE NAME-SEARCH TO CA-SEL-ELEMENT-NAME.       ELELCCML
00029                                                                   ELELCCML
00030 * 12-19-88    NAC  CORRECT LOGIC FOR ADDITION OF NEW ELEMENT.     ELELCCML
00031                                                                   ELELCCML
00032 * 12-21-88    NAC  IT'S BEEN DISCOVERED THAT THIS IS NOT THE      ELELCCML
00033 *                  GOOD PRODUCTION SOURCE;  I HAVE TO DEBUG AND   ELELCCML
00034 *                  THROUGHLY TEST THE PROGRAM.  THE SOURCE MUST   ELELCCML
00035 *                  HAVE BEEN TRASHED.                             ELELCCML
00036                                                                   ELELCCML
00037 * 01-10-90    RKH  CONVERTED EXISTING PROGRAM TO VS COBOL II.     ELELCCML
00038                                                                   ELELCCML
00039 ******************************************************************ELELCCML
00040  ENVIRONMENT DIVISION.                                            ELELCCML
00041  DATA DIVISION.                                                   ELELCCML
00042 /                                                                 ELELCCML
00043  WORKING-STORAGE SECTION.                                         ELELCCML
00044  01  WORK-STORAGE               PIC X(60)           VALUE         ELELCCML
00045      'WORKING STORAGE STARTS HERE FOR ELELCCML'.                  ELELCCML
00046                                                                   ELELCCML
00047  01  WS-PARA-ID                 PIC X(04)           VALUE 'XXXX'. ELELCCML
00048  01  WS-ABEND-CODE              PIC X(04)           VALUE 'XXXX'. ELELCCML
00049 /                                                                 ELELCCML
00050  01  PROGRAM-MESSAGES.                                            ELELCCML
00051      05  ADD-SUCCESSFUL         PIC X(22)           VALUE         ELELCCML
00052          'ADD WAS SUCCESSFUL'.                                    ELELCCML
00053      05  BEGINNING-FILE-MSG     PIC X(65)           VALUE         ELELCCML
00054          'BEGINNING OF FILE ENCOUNTERED.  NO PRIOR DATA ELEMENT TOELELCCML
00055 -        ' DISPLAY'.                                              ELELCCML
00056      05  CANNOT-MAP-TO-SELF     PIC X(43)           VALUE         ELELCCML
00057          'A DATA ELEMENT CANNOT BE MAPPED TO ITSELF'.             ELELCCML
00058      05  COBOL-NAME-EXIST       PIC X(52)           VALUE         ELELCCML
00059          'SYSTEM NAME ALREADY ON FILE -- CHANGE NOT ALLOWED'.     ELELCCML
00060      05  COBOLNAME-NEWNBR-FAIL  PIC X(45)           VALUE         ELELCCML
00061          'MOVE OF SYSTEM NAME TO CHANGE RECORD FAILED'.           ELELCCML
00062      05  DE-ALREADY-DELETED     PIC X(29)           VALUE         ELELCCML
00063          'DATA ELEMENT ALREADY DELETED'.                          ELELCCML
00064      05  DE-DELETED-NOUPDATE    PIC X(71)           VALUE         ELELCCML
00065          'DATA ELEMENT PREVIOUSLY DELETED.  TRANSFER TO CODE VALUEELELCCML
00066 -        'S NOT ALLOWED'.                                         ELELCCML
00067      05  DELETE-CONFIRM         PIC X(57)           VALUE         ELELCCML
00068          'DELETE MUST BE CONFIRMED.  USE PF6 TO CONFIRM THIS DELETELELCCML
00069 -        'E'.                                                     ELELCCML
00070      05  DE-NOTFND              PIC X(29)           VALUE         ELELCCML
00071          'DATA ELEMENT NOT FOUND      '.                          ELELCCML
00072      05  DE-ON-FILE             PIC X(29)           VALUE         ELELCCML
00073          'DATA ELEMENT ALREADY ON FILE'.                          ELELCCML
00074      05  DELETE-SUCCESSFUL      PIC X(25)           VALUE         ELELCCML
00075          'DELETE WAS SUCCESSFUL'.                                 ELELCCML
00076      05  END-OF-FILE-MSG        PIC X(57)           VALUE         ELELCCML
00077          'END OF FILE ENCOUNTERED.  NO NEXT DATA ELEMENT TO DISPLAELELCCML
00078 -        'Y'.                                                     ELELCCML
00079      05  FROM-REC-NOTFND        PIC X(30)           VALUE         ELELCCML
00080          'THE MAP FROM RECORD NOT FOUND'.                         ELELCCML
00081      05  INITIAL-INQUIRY        PIC X(79)           VALUE         ELELCCML
00082          'FCN  (L)OCATE'.                                         ELELCCML
00083      05  INITIAL-MAINT          PIC X(79)           VALUE         ELELCCML
00084         'FCN  (A)DD (C)HANGE (D)ELETE (L)OCATE (M)AP NEW ELEMENT'.ELELCCML
00085      05  INITIAL-SUPERV         PIC X(79)           VALUE         ELELCCML
00086         'FCN  (A)DD (C)HANGE (D)ELETE (L)OCATE (M)AP NEW ELEMENT'.ELELCCML
00087      05  INITIAL-VALUE          PIC X(79)           VALUE SPACES. ELELCCML
00088      05  INITIAL-VALUE-INQUIRY  PIC X(79)           VALUE         ELELCCML
00089          '(PF3=DE SEL)(PF4=CV SEL)(PF7/PF8=BACK/FWD)(PF9=RL SEL)(CELELCCML
00090 -        'LEAR=EXIT)'.                                            ELELCCML
00091      05  INITIAL-VALUE-MAINT    PIC X(79)           VALUE         ELELCCML
00092          '(PF3=DE SEL)(PF4=CV SEL)(PF7/PF8=BACK/FWD)(PF9=RL SEL)(PELELCCML
00093 -        'F10=CLEAR)(CLEAR=EXIT)'.                                ELELCCML
00094      05  INVALID-DECIMAL        PIC X(45)           VALUE         ELELCCML
00095          'DECIMAL ENTERED NOT NUMERIC.  PLEASE RE-ENTER'.         ELELCCML
00096      05  INVALID-DE-NUMBER      PIC X(47)           VALUE         ELELCCML
00097          'DATA ELEMENT NUMBER NOT VALID.  PLEASE RE-ENTER'.       ELELCCML
00098      05  INVALID-FORMAT         PIC X(43)           VALUE         ELELCCML
00099          'FORMAT ENTERED NOT VALID.  PLEASE RE-ENTER'.            ELELCCML
00100      05  INVALID-LENGTH         PIC X(45)           VALUE         ELELCCML
00101          'LENGTH ENTERED NOT NUMERIC.  PLEASE RE-ENTER'.          ELELCCML
00102      05  INVALID-MAP-FUNC       PIC X(40)           VALUE         ELELCCML
00103          'NAME ON FILE INVALID MAP FUNCTION USAGE'.               ELELCCML
00104      05  INVALID-POS            PIC X(46)           VALUE         ELELCCML
00105          'POSTION ENTERED NOT NUMERIC.  PLEASE RE-ENTER'.         ELELCCML
00106      05  INVALID-PF4-SEL-DE     PIC X(79)           VALUE         ELELCCML
00107          'INVALID USE OF PF4. ELEMENT MUST BE ADDED BEFORE TRANSFEELELCCML
00108 -        'R TO CODE VALUE ALLOWED'.                               ELELCCML
00109      05  INVALID-STORED-FORMAT  PIC X(49)           VALUE         ELELCCML
00110          'STORED FORMAT ENTERED NOT VALID.  PLEASE RE-ENTER'.     ELELCCML
00111      05  INVALID-S-LENGTH       PIC X(51)           VALUE         ELELCCML
00112          'STORED LENGTH ENTERED NOT NUMERIC.  PLEASE RE-ENTER'.   ELELCCML
00113      05  INVALID-SELECT         PIC X(45)           VALUE         ELELCCML
00114          'FUNCTION SELECTED NOT VALID.  PLEASE RE-ENTER'.         ELELCCML
00115      05  INVALID-USE-PF6        PIC X(58)           VALUE         ELELCCML
00116          'PF6 TO BE USED ONLY AS CONFIRMATION OF MAP OR DELETE'.  ELELCCML
00117      05  KEY-NOT-ENTERED        PIC X(48)           VALUE         ELELCCML
00118          'DATA ELEMENT KEY MUST BE ENTER FOR THIS FUNCTION'.      ELELCCML
00119      05  MAP-FAIL               PIC X(37)           VALUE         ELELCCML
00120          'PLEASE ENTER DATA OR USE VALID PF KEY'.                 ELELCCML
00121      05  MAP-FROM-MSG           PIC X(51)           VALUE         ELELCCML
00122          'MAP FROM WILL TAKE A LONG TIME.  USE PF6 TO CONFIRM'.   ELELCCML
00123      05  MAP-FROM-NAME-PRE-INVALID PIC X(39)        VALUE         ELELCCML
00124          'MAP FROM NAME/PREFIX INVALID FOR AN ADD'.               ELELCCML
00125      05  MAP-NAME-REQ           PIC X(43)           VALUE         ELELCCML
00126          'DATA ELEMENT NAME REQUIRED FOR MAP FUNCTION'.           ELELCCML
00127      05  MAP-NOT-ALLOW-FROM-DELETE PIC X(42)        VALUE         ELELCCML
00128          'MAPPING NOT ALLOWED FROM A DELETED ELEMENT'.            ELELCCML
00129      05  MAP-OVERLAY-MSG        PIC X(65)           VALUE         ELELCCML
00130          'MAP FROM WILL OVERLAY EXISTING DATA ELEMENT.  USE PF6 TOELELCCML
00131 -        ' CONFIRM'.                                              ELELCCML
00132      05  MAP-SUCCESSFUL         PIC X(62)           VALUE         ELELCCML
00133          'MAPPING SUCCCESSFULLY COMPLETED - SYSTEM NAME HAS BEEN BELELCCML
00134 -        'LANKED'.                                                ELELCCML
00135      05  NAME-CHG-NOTALLOW-MAP  PIC X(43)           VALUE         ELELCCML
00136          'NAME CHANGE IS NOT ALLOWED FOR MAP FUNCITON'.           ELELCCML
00137      05  NAME-MB-ENTER-ADD      PIC X(44)           VALUE         ELELCCML
00138          'DATA ELEMENT NAME MUST BE ENTERED FOR AN ADD'.          ELELCCML
00139      05  NO-CHANGES-MADE        PIC X(16)           VALUE         ELELCCML
00140          'NO CHANGES MADE'.                                       ELELCCML
00141      05  NO-CODE-VALUES         PIC X(24)           VALUE         ELELCCML
00142          'NO CODE VALUES TO REVIEW'.                              ELELCCML
00143      05  NO-FUNCTION-SELECT     PIC X(56)           VALUE         ELELCCML
00144          'NO FUNCTION SELECTED.  RE-ENTER OR USE APPROIRATE PF KEYELELCCML
00145 -        ''.                                                      ELELCCML
00146      05  NOSPACE-MSG            PIC X(38)           VALUE         ELELCCML
00147          'FILE IS FULL, NO MORE ADDS CAN BE DONE'.                ELELCCML
00148      05  PREFIX-CHG-INVALID     PIC X(41)           VALUE         ELELCCML
00149          'CHANGE OF RECORD PREFIX NOT ALLOWED'.                   ELELCCML
00150      05  REC-SCH-FOR-DEL-NCHG   PIC X(59)           VALUE         ELELCCML
00151          'DATA ELEMENT SCHEDULED FOR DELETION. NO CHANGES ARE ALLOELELCCML
00152 -        'WED'.                                                   ELELCCML
00153      05  REORGANIZATION-MSG     PIC X(76)           VALUE         ELELCCML
00154          'UNABLE TO INSERT ELEMENT.  REORGANIZATION OF DATA ELEME ELELCCML
00155 -        'NT FILE NEEDED'.                                        ELELCCML
00156      05  UNABLE-TO-ASSIGN-NUM  PIC X(71)            VALUE         ELELCCML
00157          'UNABLE TO ASSIGN DATA ELEMENT NUMBER AT THIS TIME.  ITEMELELCCML
00158 -        'NOT ADDED.'.                                            ELELCCML
00159      05  UNAUTHOR-MSG           PIC X(79)           VALUE         ELELCCML
00160         'NO COMMON AREA RECEIVED.  UNAUTHORIZED ACCESS ATTEMPTED'.ELELCCML
00161      05  UNAUTHOR-MAINTAIN      PIC X(41)           VALUE         ELELCCML
00162          'USE CORRECT PF KEY FOR INQUIRY SELECTION '.             ELELCCML
00163      05  UNAUTHOR-OVERLAY       PIC X(54)           VALUE         ELELCCML
00164          'YOU ARE NOT AUTHORIZED TO OVERLAY AN EXISTING RECORD'.  ELELCCML
00165      05  UPDATE-SUCCESSFUL      PIC X(22)           VALUE         ELELCCML
00166          'UPDATE WAS SUCCESSFUL'.                                 ELELCCML
00167      05  UNDELETE-REQ           PIC X(43)           VALUE         ELELCCML
00168          'UNDELETE REQUIRED BEFORE CHANGE CAN BE MADE'.           ELELCCML
00169      05  USE-MAP-FUNCTION       PIC X(66)            VALUE        ELELCCML
00170          'NAME CHANGE WILL OVERLAY EXISTING DATA ELEMENT.  USE MAPELELCCML
00171 -        ' FUNCTION.'.                                            ELELCCML
00172 /                                                                 ELELCCML
00173  01  PROGRAM-REC-LENGTHS.                                         ELELCCML
00174      05  COMM-LENGTH            PIC S9(4)   COMP     VALUE +500.  ELELCCML
00175      05  ELPDE-LENGTH           PIC S9(4)   COMP     VALUE +000.  ELELCCML
00176      05  ELPRL-KEYLENGTH        PIC S9(4)   COMP     VALUE +008.  ELELCCML
00177      05  ELPDE-KEYLENGTH        PIC S9(4)   COMP     VALUE +011.  ELELCCML
00178      05  ELPEN-KEYLENGTH        PIC S9(4)   COMP     VALUE +083.  ELELCCML
00179      05  ELPCV-KEYLENGTH        PIC S9(4)   COMP     VALUE +023.  ELELCCML
00180      05  DESC-LINE-LENGTH       PIC S9(4)   COMP     VALUE +079.  ELELCCML
00181      05  ADD-DESC-LENGTH        PIC S9(4)   COMP     VALUE +000.  ELELCCML
00182      05  DE-BASE-LENGTH         PIC S9(4)   COMP     VALUE +251.  ELELCCML
00183  COPY ELCDRLEN.                                                   ELELCCML
00184 /                                                                 ELELCCML
00185  01  COBOL-NAME-KEY.                                              ELELCCML
00186      05  CB-PREFIX              PIC X(08)            VALUE SPACES.ELELCCML
00187      05  CB-NAME                PIC X(30)            VALUE SPACES.ELELCCML
00188                                                                   ELELCCML
00189  01  PROGRAM-SWITCHES.                                            ELELCCML
00190      05  PF6-SW                 PIC X.                            ELELCCML
00191      05  DELETE-SW              PIC X.                            ELELCCML
00192      05  ERROR-SW               PIC X.                            ELELCCML
00193      05  NUMBER-ERROR-SW        PIC X.                            ELELCCML
00194      05  FOUND-SW               PIC X.                            ELELCCML
00195      05  FOUND-COBOL-NAME       PIC X.                            ELELCCML
00196      05  NEW-NBR-SW             PIC X.                            ELELCCML
00197      05  REWRITE-SW             PIC X.                            ELELCCML
00198      05  SAVE-CODE-VALUE        PIC X(10).                        ELELCCML
00199      05  SAVE-CODE-DESC-SEQ     PIC 99.                           ELELCCML
00200      05  ELPEN-SWITCH           PIC X.                            ELELCCML
00201                                                                   ELELCCML
00202  01  PROGRAM-WORK-AREAS.                                          ELELCCML
00203      05  WS-TRANS-ID            PIC X(04)         VALUE 'ELCC'.   ELELCCML
00204      05  WS-LOW-VALUES          PIC X          VALUE LOW-VALUES.  ELELCCML
00205      05  FROM-PREFIX            PIC X(08).                        ELELCCML
00206      05  HOLD-EN-PREFIX         PIC X(08).                        ELELCCML
00207      05  PREFIX-SEARCH          PIC X(08).                        ELELCCML
00208      05  COBOL-NAME-SEARCH      PIC X(30).                        ELELCCML
00209      05  HOLD-COBOL-NAME        PIC X(30).                        ELELCCML
00210      05  NAME-SEARCH            PIC X(75).                        ELELCCML
00211      05  SAVE-OLD-DE-NAME       PIC X(75).                        ELELCCML
00212      05  MAX-DES-LINES          PIC S9(2)           VALUE +11.    ELELCCML
00213      05  SCREEN-CTR             PIC S9(2).                        ELELCCML
00214      05  FROM-NBR               PIC S9(3)V99.                     ELELCCML
00215      05  SAVE-DE-NBR-NEXT       PIC S9(3)V99.                     ELELCCML
00216      05  SAVE-DE-NBR-PREV       PIC S9(3)V99.                     ELELCCML
00217      05  NEW-DE-NBR             PIC S9(3)V99.                     ELELCCML
00218      05  DIFFER-ENCE            PIC S9(3)V99.                     ELELCCML
00219      05  DIVID-END              PIC S9(3)V99.                     ELELCCML
00220      05  POINT-ZERO-ONE         PIC S9(3)V99        VALUE +000.01.ELELCCML
00221      05  FORMAT-ELEMENT-NBR     PIC  9(3)V99.                     ELELCCML
00222      05  FORMAT-ELEMENT-NBR-9 REDEFINES FORMAT-ELEMENT-NBR.       ELELCCML
00223          10  DE-NBR             PIC 9(3).                         ELELCCML
00224          10  DE-INSERT-NBR      PIC 99.                           ELELCCML
00225      05  FORMAT-ELEMENT-NBR-D   PIC 999.99.                       ELELCCML
00226      05  FORMAT-ELEMENT-NBR-X REDEFINES FORMAT-ELEMENT-NBR-D.     ELELCCML
00227          10  FIRST-THREE-X      PIC XXX.                          ELELCCML
00228          10  PERIOD-X           PIC X.                            ELELCCML
00229          10  LAST-TWO-X         PIC XX.                           ELELCCML
00230                                                                   ELELCCML
00231  01  VALIDATAION-AREA.                                            ELELCCML
00232      05 FORMAT-CHECK            PIC XX.                           ELELCCML
00233         88 VALID-FORMAT         VALUES ARE 'A ', ' A', 'AN', 'DT' ELELCCML
00234                                            'N ', ' N'.            ELELCCML
00235      05 STORED-CHECK            PIC X.                            ELELCCML
00236         88 VALID-STORED         VALUES ARE 'B', 'C', 'D', 'P'     ELELCCML
00237                                            'X', ' '.              ELELCCML
00238 /                                                                 ELELCCML
00239  COPY ELCCSETC.                                                   ELELCCML
00240  01  DATA-ELEM-SCREEN REDEFINES ELCCI01I.                         ELELCCML
00241      05  FILLER               PIC X(12).                          ELELCCML
00242      05  FUNC-L               COMP  PIC S9(4).                    ELELCCML
00243      05  FUNC-A               PIC X.                              ELELCCML
00244      05  FUNC-D               PIC X(04).                          ELELCCML
00245      05  FUNC-TA              PIC X.                              ELELCCML
00246      05  TITLE-L              COMP  PIC S9(4).                    ELELCCML
00247      05  TITLE-A              PIC X.                              ELELCCML
00248      05  TITLE-D              PIC X(36).                          ELELCCML
00249      05  TITLE-TA             PIC X.                              ELELCCML
00250      05  PREF-L               COMP  PIC S9(4).                    ELELCCML
00251      05  PREF-A               PIC X.                              ELELCCML
00252      05  PREF-D               PIC X(08).                          ELELCCML
00253      05  PREF-TA              PIC X.                              ELELCCML
00254      05  RL-NAME-L            COMP  PIC S9(4).                    ELELCCML
00255      05  RL-NAME-A            PIC X.                              ELELCCML
00256      05  RL-NAME-D            PIC X(50).                          ELELCCML
00257      05  RL-NAME-TA           PIC X.                              ELELCCML
00258      05  DE-NUMBER-L          COMP  PIC S9(4).                    ELELCCML
00259      05  DE-NUMBER-A          PIC X.                              ELELCCML
00260      05  DE-NUMBER-D          PIC X(06).                          ELELCCML
00261      05  DE-NUMBER-TA         PIC X.                              ELELCCML
00262      05  FCN-L                COMP  PIC S9(4).                    ELELCCML
00263      05  FCN-A                PIC X.                              ELELCCML
00264      05  FCN-D                PIC X.                              ELELCCML
00265      05  DE-NAME-L            COMP PIC S9(4).                     ELELCCML
00266      05  DE-NAME-A            PIC X.                              ELELCCML
00267      05  DE-NAME-D            PIC X(75).                          ELELCCML
00268      05  DE-FORMAT-L          COMP  PIC S9(4).                    ELELCCML
00269      05  DE-FORMAT-A          PIC X.                              ELELCCML
00270      05  DE-FORMAT-D          PIC X(02).                          ELELCCML
00271      05  DE-FORMAT-TA         PIC X.                              ELELCCML
00272      05  DE-LENGTH-L          COMP  PIC S9(4).                    ELELCCML
00273      05  DE-LENGTH-A          PIC X.                              ELELCCML
00274      05  DE-LENGTH-D          PIC X(03).                          ELELCCML
00275      05  DE-FORMCOM-L         COMP  PIC S9(4).                    ELELCCML
00276      05  DE-FORMCOM-A         PIC X.                              ELELCCML
00277      05  DE-FORMCOM-D         PIC X(21).                          ELELCCML
00278      05  DE-POS-L             COMP  PIC S9(4).                    ELELCCML
00279      05  DE-POS-A             PIC X.                              ELELCCML
00280      05  DE-POS-D             PIC X(05).                          ELELCCML
00281      05  DE-STORED-LENGTH-L   COMP  PIC S9(4).                    ELELCCML
00282      05  DE-STORED-LENGTH-A   PIC X.                              ELELCCML
00283      05  DE-STORED-LENGTH-D   PIC X(03).                          ELELCCML
00284      05  DE-DECIMAL-L         COMP  PIC S9(4).                    ELELCCML
00285      05  DE-DECIMAL-A         PIC X.                              ELELCCML
00286      05  DE-DECIMAL-D         PIC X(03).                          ELELCCML
00287      05  DE-STORED-FORMAT-L   COMP  PIC S9(4).                    ELELCCML
00288      05  DE-STORED-FORMAT-A   PIC X.                              ELELCCML
00289      05  DE-STORED-FORMAT-D   PIC X.                              ELELCCML
00290      05  DE-COBOL-NAME-L      COMP  PIC S9(4).                    ELELCCML
00291      05  DE-COBOL-NAME-A      PIC X.                              ELELCCML
00292      05  DE-COBOL-NAME-D      PIC X(30).                          ELELCCML
00293      05  DE-COBOL-NAME-TA     PIC X.                              ELELCCML
00294      05  DE-BAL-NAME-L        COMP  PIC S9(4).                    ELELCCML
00295      05  DE-BAL-NAME-A        PIC X.                              ELELCCML
00296      05  DE-BAL-NAME-D        PIC X(08).                          ELELCCML
00297      05  DE-BAL-NAME-TA       PIC X.                              ELELCCML
00298      05  DE-DESCRIPTION-LINE OCCURS 11 TIMES.                     ELELCCML
00299            10  DE-DESCRIPTION-L COMP PIC S9(4).                   ELELCCML
00300            10  DE-DESCRIPTION-A PIC X.                            ELELCCML
00301            10  DE-DESCRIPTION-D PIC X(79).                        ELELCCML
00302      05  DE-MAP-PREFIX-L      COMP PIC S9(4).                     ELELCCML
00303      05  DE-MAP-PREFIX-A      PIC X.                              ELELCCML
00304      05  DE-MAP-PREFIX-D      PIC X(08).                          ELELCCML
00305      05  DE-MAP-PREFIX-TA     PIC X.                              ELELCCML
00306      05  DE-MAP-NAME-L        COMP PIC S9(4).                     ELELCCML
00307      05  DE-MAP-NAME-A        PIC X.                              ELELCCML
00308      05  DE-MAP-NAME-D        PIC X(75).                          ELELCCML
00309      05  MSG-L                COMP PIC S9(4).                     ELELCCML
00310      05  MSG-A                PIC X.                              ELELCCML
00311      05  MSG-D                PIC X(79).                          ELELCCML
00312      05  ERRM-L               COMP PIC S9(4).                     ELELCCML
00313      05  ERRM-A               PIC X.                              ELELCCML
00314      05  ERRM-D               PIC X(79).                          ELELCCML
00315 /                                                                 ELELCCML
00316 *    ATTRIBUTES *                                                 ELELCCML
00317  COPY DFHBMSCA.                                                   ELELCCML
00318                                                                   ELELCCML
00319  01  AID-KEYS                   PIC X(08)           VALUE         ELELCCML
00320      'AID-KEYS'.                                                  ELELCCML
00321  COPY DFHAID.                                                     ELELCCML
00322 /                                                                 ELELCCML
00323  LINKAGE SECTION.                                                 ELELCCML
00324  01  DFHCOMMAREA.                                                 ELELCCML
00325  COPY ELPCOMMC.                                                   ELELCCML
00326 /                                                                 ELELCCML
00327  01  CIA-PARMS-RECORD.                                            ELELCCML
00328  COPY ELCDCIA.                                                    ELELCCML
00329                                                                   ELELCCML
00330 / ****    DATA ELEMENT AREA   *****************                   ELELCCML
00331  01  IOPARM-DATA-ELEMENT.                                         ELELCCML
00332  COPY ELCDIOPM.                                                   ELELCCML
00333  01  EL-DATA-ELEMENT.                                             ELELCCML
00334      COPY ELPDEC                                                  ELELCCML
00335      REPLACING == OCCURS 1 TO 11 TIMES ==                         ELELCCML
00336             BY == OCCURS      11 TIMES.==                         ELELCCML
00337                == DEPENDING ON DE-NBR-DESC-LINES. ==              ELELCCML
00338             BY ==                                 ==.             ELELCCML
00339                                                                   ELELCCML
00340 / ****    CODE VALUE AREA     *****************                   ELELCCML
00341  01  IOPARM-CODE-VALUE.                                           ELELCCML
00342  COPY ELCDIOP2.                                                   ELELCCML
00343  01  EL-CODE-VALUE.                                               ELELCCML
00344  COPY ELPCVC                                                      ELELCCML
00345      REPLACING == OCCURS 1 TO 12 TIMES ==                         ELELCCML
00346             BY == OCCURS      12 TIMES. ==                        ELELCCML
00347                == DEPENDING ON    ==                              ELELCCML
00348             BY ==                 ==                              ELELCCML
00349                == CV-NBR-VALUE-DESC-LINES. ==                     ELELCCML
00350             BY ==                          ==.                    ELELCCML
00351                                                                   ELELCCML
00352 / ****    RECORD LIST AREA    *****************                   ELELCCML
00353  01  IOPARM-RECORD-LIST.                                          ELELCCML
00354  COPY ELCDIOP3.                                                   ELELCCML
00355  01  EL-RECORD-LIST.                                              ELELCCML
00356  COPY ELPRLC.                                                     ELELCCML
00357                                                                   ELELCCML
00358 / ****    ENGLISH NAME AREA   *****************                   ELELCCML
00359  01  IOPARM-ENGLISH-NAME.                                         ELELCCML
00360  COPY ELCDIOP4.                                                   ELELCCML
00361  01  EL-ENGLISH-NAME.                                             ELELCCML
00362  COPY ELPENC.                                                     ELELCCML
00363                                                                   ELELCCML
00364 / ****    PROCEDURE DIVISION  *****************                   ELELCCML
00365  PROCEDURE DIVISION.                                              ELELCCML
00366  MAIN-LINE-PROGRAM.                                               ELELCCML
00367      PERFORM 1000-BEGINNING-PROCESS THRU 1000-END.                ELELCCML
00368      PERFORM 2000-MAINLINE-PROCESS  THRU 2000-END.                ELELCCML
00369  0000-RETURN-CICS.                                                ELELCCML
00370      EXEC CICS RETURN                                             ELELCCML
00371      END-EXEC.                                                    ELELCCML
00372      GOBACK.                                                      ELELCCML
00373 /                                                                 ELELCCML
00374  1000-BEGINNING-PROCESS.                                          ELELCCML
00375 *    *-----------------------------------------------------------*ELELCCML
00376 *    * THIS IS WHERE IT STARTS.                                  *ELELCCML
00377 *    *-----------------------------------------------------------*ELELCCML
00378      MOVE '1000'  TO WS-PARA-ID.                                  ELELCCML
00379                                                                   ELELCCML
00380      IF  CA-DE-DEFINE                                             ELELCCML
00381          EXEC CICS GETMAIN                                        ELELCCML
00382              SET(ADDRESS OF CIA-PARMS-RECORD)                     ELELCCML
00383              INITIMG(WS-LOW-VALUES)                               ELELCCML
00384              LENGTH(EL-CIA-REC-REC-LEN)                           ELELCCML
00385          END-EXEC                                                 ELELCCML
00386          SET CA-CIA-POINTER TO                                    ELELCCML
00387          ADDRESS OF CIA-PARMS-RECORD                              ELELCCML
00388      ELSE                                                         ELELCCML
00389          SET ADDRESS OF CIA-PARMS-RECORD  TO                      ELELCCML
00390              CA-CIA-POINTER.                                      ELELCCML
00391                                                                   ELELCCML
00392 *-->  CHECK COMMUNICATIONS AREA                                   ELELCCML
00393      IF EIBCALEN = 0                                              ELELCCML
00394         MOVE 'EB00'  TO   WS-ABEND-CODE                           ELELCCML
00395      GO TO 9999-ABEND.                                            ELELCCML
00396                                                                   ELELCCML
00397      MOVE LOW-VALUES    TO   ELCCI01I.                            ELELCCML
00398                                                                   ELELCCML
00399      MOVE 'N'              TO ERROR-SW,                           ELELCCML
00400                               NEW-NBR-SW.                         ELELCCML
00401 *  ****   SET HANDLE CONDITIONS                                   ELELCCML
00402                                                                   ELELCCML
00403      EXEC CICS HANDLE CONDITION                                   ELELCCML
00404             MAPFAIL(9000-MAPFAIL)                                 ELELCCML
00405      END-EXEC.                                                    ELELCCML
00406                                                                   ELELCCML
00407      EXEC CICS HANDLE AID                                         ELELCCML
00408              PF3(9100-XCTL-ELCB)                                  ELELCCML
00409              PF4(9200-XCTL-ELCD)                                  ELELCCML
00410              PF6(6000-CONFIRM)                                    ELELCCML
00411              PF9(9400-XCTL-ELCA)                                  ELELCCML
00412              PF10(8000-DISP-CLR-SCREEN)                           ELELCCML
00413              CLEAR(0000-RETURN-CICS)                              ELELCCML
00414      END-EXEC.                                                    ELELCCML
00415                                                                   ELELCCML
00416    1000-END.                                                      ELELCCML
00417      EXIT.                                                        ELELCCML
00418 /                                                                 ELELCCML
00419  2000-MAINLINE-PROCESS.                                           ELELCCML
00420 *    *-----------------------------------------------------------*ELELCCML
00421 *    * DETERMINE PROGRAMS THAT PASSED RECORD THEN PROCESS        *ELELCCML
00422 *    *   THE CORRECT ROUTINES.                                   *ELELCCML
00423 *    *-----------------------------------------------------------*ELELCCML
00424      MOVE '2000'      TO WS-PARA-ID.                              ELELCCML
00425                                                                   ELELCCML
00426      MOVE SPACES      TO SAVE-OLD-DE-NAME.                        ELELCCML
00427      PERFORM 3000-CLEAR-ATTRIBUTES THRU 3000-END.                 ELELCCML
00428                                                                   ELELCCML
00429      IF CA-DE-DEFINE                                              ELELCCML
00430          PERFORM 2100-GETMAINS    THRU 2100-END                   ELELCCML
00431          PERFORM 2300-RECEIVE-MAP THRU 2300-END                   ELELCCML
00432          PERFORM 3400-EDIT-SCREEN THRU 3400-END                   ELELCCML
00433          MOVE 'C' TO CA-CURRENT-PGM                               ELELCCML
00434      ELSE                                                         ELELCCML
00435        PERFORM 2200-SET-ADDRESS     THRU 2200-END                 ELELCCML
00436        PERFORM 3100-INITIAL-HEADING THRU 3100-END                 ELELCCML
00437        PERFORM 3200-CREATE-SCREEN   THRU 3200-END.                ELELCCML
00438                                                                   ELELCCML
00439      PERFORM 9050-SEND-SCREEN THRU 9050-END.                      ELELCCML
00440      PERFORM 9300-RETURN-ELCC THRU 9300-END.                      ELELCCML
00441    2000-END.                                                      ELELCCML
00442      EXIT.                                                        ELELCCML
00443 /                                                                 ELELCCML
00444  2100-GETMAINS.                                                   ELELCCML
00445 *    *-----------------------------------------------------------*ELELCCML
00446 *    * ISSUE GETMAIN FOR IOPARMS AREA AND RECORD AREA            *ELELCCML
00447 *    *-----------------------------------------------------------*ELELCCML
00448      MOVE '2100'      TO WS-PARA-ID.                              ELELCCML
00449                                                                   ELELCCML
00450 *---->  GETMAIN FOR DATA ELEMENT IOPARM                           ELELCCML
00451                                                                   ELELCCML
00452      EXEC CICS GETMAIN                                            ELELCCML
00453                SET(ADDRESS OF IOPARM-DATA-ELEMENT)                ELELCCML
00454                LENGTH(EL-IOPARMS-REC-LEN)                         ELELCCML
00455                INITIMG(WS-LOW-VALUES)                             ELELCCML
00456      END-EXEC.                                                    ELELCCML
00457                                                                   ELELCCML
00458      SET CIA-ELPDE-IOPARM-AREA-PNTR   TO                          ELELCCML
00459          ADDRESS OF IOPARM-DATA-ELEMENT.                          ELELCCML
00460                                                                   ELELCCML
00461 *---->  GETMAIN FOR DATA ELEMENT                                  ELELCCML
00462      EXEC CICS GETMAIN                                            ELELCCML
00463                SET(ADDRESS OF EL-DATA-ELEMENT)                    ELELCCML
00464                LENGTH(EL-ELPDE-REC-LEN)                           ELELCCML
00465                INITIMG(WS-LOW-VALUES)                             ELELCCML
00466      END-EXEC.                                                    ELELCCML
00467                                                                   ELELCCML
00468      SET  CIA-ELPDE-REC-AREA-PNTR     TO                          ELELCCML
00469           ADDRESS OF EL-DATA-ELEMENT.                             ELELCCML
00470                                                                   ELELCCML
00471      MOVE EL-DSN-ELPDE TO CIA-IO-GETMAIN-DDNAME.                  ELELCCML
00472                                                                   ELELCCML
00473 *---->  GETMAIN FOR CODES VALUE IOPARM                            ELELCCML
00474                                                                   ELELCCML
00475      EXEC CICS GETMAIN                                            ELELCCML
00476                SET(ADDRESS OF IOPARM-CODE-VALUE)                  ELELCCML
00477                LENGTH(EL-IOPARMS-REC-LEN)                         ELELCCML
00478                INITIMG(WS-LOW-VALUES)                             ELELCCML
00479      END-EXEC.                                                    ELELCCML
00480                                                                   ELELCCML
00481      SET CIA-ELPCV-IOPARM-AREA-PNTR   TO                          ELELCCML
00482            ADDRESS  OF  IOPARM-CODE-VALUE.                        ELELCCML
00483                                                                   ELELCCML
00484 *---->  GETMAIN FOR CODES VALUE                                   ELELCCML
00485                                                                   ELELCCML
00486      EXEC CICS GETMAIN                                            ELELCCML
00487                SET(ADDRESS OF EL-CODE-VALUE)                      ELELCCML
00488                LENGTH(EL-ELPCV-REC-LEN)                           ELELCCML
00489                INITIMG(WS-LOW-VALUES)                             ELELCCML
00490      END-EXEC.                                                    ELELCCML
00491                                                                   ELELCCML
00492      SET  CIA-ELPCV-REC-AREA-PNTR  TO                             ELELCCML
00493          ADDRESS OF EL-CODE-VALUE.                                ELELCCML
00494                                                                   ELELCCML
00495      MOVE EL-DSN-ELPCV TO CIA-IO-GETMAIN-DDNAME.                  ELELCCML
00496                                                                   ELELCCML
00497 *---->  GETMAIN FOR RECORD LIST IOPARM                            ELELCCML
00498                                                                   ELELCCML
00499      EXEC CICS GETMAIN                                            ELELCCML
00500                SET(ADDRESS OF IOPARM-RECORD-LIST)                 ELELCCML
00501                LENGTH(EL-IOPARMS-REC-LEN)                         ELELCCML
00502                INITIMG(WS-LOW-VALUES)                             ELELCCML
00503      END-EXEC.                                                    ELELCCML
00504                                                                   ELELCCML
00505      SET  CIA-ELPRL-IOPARM-AREA-PNTR     TO                       ELELCCML
00506           ADDRESS OF IOPARM-RECORD-LIST.                          ELELCCML
00507                                                                   ELELCCML
00508 *---->  GETMAIN FOR RECORD LIST                                   ELELCCML
00509                                                                   ELELCCML
00510      EXEC CICS GETMAIN                                            ELELCCML
00511                SET(ADDRESS OF EL-RECORD-LIST)                     ELELCCML
00512                LENGTH(EL-ELPRL-REC-LEN)                           ELELCCML
00513                INITIMG(WS-LOW-VALUES)                             ELELCCML
00514      END-EXEC.                                                    ELELCCML
00515                                                                   ELELCCML
00516      SET  CIA-ELPRL-REC-AREA-PNTR    TO                           ELELCCML
00517           ADDRESS OF EL-RECORD-LIST.                              ELELCCML
00518                                                                   ELELCCML
00519      MOVE EL-DSN-ELPRL TO CIA-IO-GETMAIN-DDNAME.                  ELELCCML
00520                                                                   ELELCCML
00521 *---->  GETMAIN FOR ENGLISH NAME IOPARM                           ELELCCML
00522                                                                   ELELCCML
00523      EXEC CICS GETMAIN                                            ELELCCML
00524                SET(ADDRESS OF IOPARM-ENGLISH-NAME)                ELELCCML
00525                LENGTH(EL-IOPARMS-REC-LEN)                         ELELCCML
00526                INITIMG(WS-LOW-VALUES)                             ELELCCML
00527      END-EXEC.                                                    ELELCCML
00528                                                                   ELELCCML
00529      SET CIA-ELPEN-IOPARM-AREA-PNTR    TO                         ELELCCML
00530          ADDRESS  OF  IOPARM-ENGLISH-NAME.                        ELELCCML
00531                                                                   ELELCCML
00532 *---->  GETMAIN FOR ENGLISH NAME RECORD                           ELELCCML
00533                                                                   ELELCCML
00534      EXEC CICS GETMAIN                                            ELELCCML
00535                SET(ADDRESS OF EL-ENGLISH-NAME)                    ELELCCML
00536                LENGTH(EL-ELPEN-REC-LEN)                           ELELCCML
00537                INITIMG(WS-LOW-VALUES)                             ELELCCML
00538      END-EXEC.                                                    ELELCCML
00539                                                                   ELELCCML
00540      SET CIA-ELPEN-REC-AREA-PNTR     TO                           ELELCCML
00541          ADDRESS OF EL-ENGLISH-NAME.                              ELELCCML
00542                                                                   ELELCCML
00543      MOVE EL-DSN-ELPEN TO CIA-IO-GETMAIN-DDNAME.                  ELELCCML
00544                                                                   ELELCCML
00545  2100-END.   EXIT.                                                ELELCCML
00546 /                                                                 ELELCCML
00547  2200-SET-ADDRESS.                                                ELELCCML
00548 *    *-----------------------------------------------------------*ELELCCML
00549 *    * ISSUE SET ADDRESS FOR IOPARMS AND RECORD AREAS            *ELELCCML
00550 *    *-----------------------------------------------------------*ELELCCML
00551      MOVE '2200'      TO WS-PARA-ID.                              ELELCCML
00552                                                                   ELELCCML
00553 * ****************************************************************ELELCCML
00554 *    SET ADDRESS OF DATA ELEMENT AREAS                            ELELCCML
00555 * ****************************************************************ELELCCML
00556                                                                   ELELCCML
00557      SET ADDRESS OF IOPARM-DATA-ELEMENT TO                        ELELCCML
00558          CIA-ELPDE-IOPARM-AREA-PNTR.                              ELELCCML
00559                                                                   ELELCCML
00560      SET  CIA-IO-PARM-AREA-PNTR      TO                           ELELCCML
00561           CIA-ELPDE-IOPARM-AREA-PNTR.                             ELELCCML
00562                                                                   ELELCCML
00563      SET ADDRESS OF EL-DATA-ELEMENT TO                            ELELCCML
00564           CIA-ELPDE-REC-AREA-PNTR.                                ELELCCML
00565                                                                   ELELCCML
00566 * ****************************************************************ELELCCML
00567 *    SET ADDRESS OF CODE VALUE AREAS                              ELELCCML
00568 * ****************************************************************ELELCCML
00569                                                                   ELELCCML
00570      SET ADDRESS OF IOPARM-CODE-VALUE   TO                        ELELCCML
00571          CIA-ELPCV-IOPARM-AREA-PNTR.                              ELELCCML
00572                                                                   ELELCCML
00573      SET  CIA-IO-PARM-AREA-PNTR      TO                           ELELCCML
00574           CIA-ELPCV-IOPARM-AREA-PNTR.                             ELELCCML
00575                                                                   ELELCCML
00576      SET ADDRESS OF EL-CODE-VALUE   TO                            ELELCCML
00577           CIA-ELPCV-REC-AREA-PNTR.                                ELELCCML
00578                                                                   ELELCCML
00579 * ****************************************************************ELELCCML
00580 *    SET ADDRESS OF RECORD LIST AREAS                             ELELCCML
00581 * ****************************************************************ELELCCML
00582                                                                   ELELCCML
00583      SET ADDRESS OF IOPARM-RECORD-LIST  TO                        ELELCCML
00584          CIA-ELPRL-IOPARM-AREA-PNTR.                              ELELCCML
00585                                                                   ELELCCML
00586      SET  CIA-IO-PARM-AREA-PNTR      TO                           ELELCCML
00587           CIA-ELPRL-IOPARM-AREA-PNTR.                             ELELCCML
00588                                                                   ELELCCML
00589      SET  ADDRESS OF EL-RECORD-LIST   TO                          ELELCCML
00590           CIA-ELPRL-REC-AREA-PNTR.                                ELELCCML
00591                                                                   ELELCCML
00592 * ****************************************************************ELELCCML
00593 *    SET ADDRESS OF ENGLISH NAME AREAS                            ELELCCML
00594 * ****************************************************************ELELCCML
00595                                                                   ELELCCML
00596      SET ADDRESS OF IOPARM-ENGLISH-NAME TO                        ELELCCML
00597          CIA-ELPRL-IOPARM-AREA-PNTR.                              ELELCCML
00598                                                                   ELELCCML
00599      SET  CIA-IO-PARM-AREA-PNTR      TO                           ELELCCML
00600           CIA-ELPEN-IOPARM-AREA-PNTR.                             ELELCCML
00601                                                                   ELELCCML
00602      SET  ADDRESS OF EL-ENGLISH-NAME  TO                          ELELCCML
00603           CIA-ELPEN-REC-AREA-PNTR.                                ELELCCML
00604                                                                   ELELCCML
00605  2200-END.    EXIT.                                               ELELCCML
00606 /                                                                 ELELCCML
00607  2300-RECEIVE-MAP.                                                ELELCCML
00608 *    *-----------------------------------------------------------*ELELCCML
00609 *    * RECEIVE MAP                                               *ELELCCML
00610 *    *-----------------------------------------------------------*ELELCCML
00611      MOVE '2300'      TO WS-PARA-ID.                              ELELCCML
00612                                                                   ELELCCML
00613      EXEC CICS RECEIVE                                            ELELCCML
00614           MAP('ELCCI01')                                          ELELCCML
00615           MAPSET('ELCCSET')                                       ELELCCML
00616      END-EXEC.                                                    ELELCCML
00617  2300-END.   EXIT.                                                ELELCCML
00618 /                                                                 ELELCCML
00619  3000-CLEAR-ATTRIBUTES.                                           ELELCCML
00620 *    *-----------------------------------------------------------*ELELCCML
00621 *    * RESET SCREEN ATTRIBUTES                                   *ELELCCML
00622 *    *-----------------------------------------------------------*ELELCCML
00623      MOVE '3000'      TO WS-PARA-ID.                              ELELCCML
00624                                                                   ELELCCML
00625      MOVE LOW-VALUES TO ELCCI01I                                  ELELCCML
00626                         DATA-ELEM-SCREEN.                         ELELCCML
00627                                                                   ELELCCML
00628      MOVE WS-TRANS-ID TO FUNC-D.                                  ELELCCML
00629                                                                   ELELCCML
00630      MOVE DFHBMPRF TO FUNC-A,                                     ELELCCML
00631                       RL-NAME-A,                                  ELELCCML
00632                       DE-NUMBER-A,                                ELELCCML
00633                       PREF-A.                                     ELELCCML
00634                                                                   ELELCCML
00635      MOVE DFHBMPRO TO FUNC-TA,           RL-NAME-TA,              ELELCCML
00636                       DE-NUMBER-TA,      PREF-TA,                 ELELCCML
00637                       DE-FORMAT-TA,      DE-COBOL-NAME-TA,        ELELCCML
00638                       DE-BAL-NAME-TA,    DE-MAP-PREFIX-TA,        ELELCCML
00639                       MSG-A,             TITLE-A.                 ELELCCML
00640                                                                   ELELCCML
00641      IF  CA-INQUIRY AND CA-LOCATE                                 ELELCCML
00642          MOVE DFHBMUNF TO DE-NAME-A.                              ELELCCML
00643                                                                   ELELCCML
00644      IF CA-INQUIRY                                                ELELCCML
00645         MOVE DFHBMPRO TO DE-FORMAT-A,           DE-FORMCOM-A,     ELELCCML
00646                          DE-STORED-FORMAT-A,    DE-COBOL-NAME-A,  ELELCCML
00647                          DE-BAL-NAME-A,         DE-MAP-PREFIX-A,  ELELCCML
00648                          DE-MAP-NAME-A                            ELELCCML
00649         MOVE DFHBMASK TO DE-LENGTH-A,           DE-POS-A,         ELELCCML
00650                          DE-STORED-LENGTH-A,    DE-DECIMAL-A      ELELCCML
00651      ELSE                                                         ELELCCML
00652         MOVE DFHBMUNP TO DE-NAME-A,             DE-FORMAT-A,      ELELCCML
00653                          DE-FORMCOM-A,          DE-STORED-FORMAT-AELELCCML
00654                          DE-COBOL-NAME-A,       DE-BAL-NAME-A,    ELELCCML
00655                          DE-MAP-PREFIX-A,       DE-MAP-NAME-A     ELELCCML
00656         MOVE DFHBMUNN TO DE-LENGTH-A,           DE-POS-A,         ELELCCML
00657                          DE-STORED-LENGTH-A,    DE-DECIMAL-A.     ELELCCML
00658                                                                   ELELCCML
00659      MOVE DFHBMUNP    TO FCN-A.                                   ELELCCML
00660      MOVE DFHPROTI    TO ERRM-A.                                  ELELCCML
00661                                                                   ELELCCML
00662      PERFORM 3050-CLEAR-DESC-ATTRB THRU 3050-END                  ELELCCML
00663          VARYING SCREEN-CTR FROM 1 BY 1                           ELELCCML
00664                   UNTIL SCREEN-CTR > MAX-DES-LINES.               ELELCCML
00665                                                                   ELELCCML
00666  3000-END.  EXIT.                                                 ELELCCML
00667                                                                   ELELCCML
00668  3050-CLEAR-DESC-ATTRB.                                           ELELCCML
00669 *    *-----------------------------------------------------------*ELELCCML
00670 *    * CLEAR INDIVIDUAL SCREEN AREAS                             *ELELCCML
00671 *    *-----------------------------------------------------------*ELELCCML
00672      MOVE '3050'      TO WS-PARA-ID.                              ELELCCML
00673                                                                   ELELCCML
00674      IF CA-INQUIRY                                                ELELCCML
00675         MOVE DFHBMPRO TO   DE-DESCRIPTION-A (SCREEN-CTR)          ELELCCML
00676         MOVE SPACES   TO   DE-DESCRIPTION-D (SCREEN-CTR)          ELELCCML
00677      ELSE                                                         ELELCCML
00678        MOVE DFHBMUNP TO    DE-DESCRIPTION-A (SCREEN-CTR)          ELELCCML
00679        MOVE SPACES   TO    DE-DESCRIPTION-D (SCREEN-CTR).         ELELCCML
00680                                                                   ELELCCML
00681  3050-END.   EXIT.                                                ELELCCML
00682 /                                                                 ELELCCML
00683  3100-INITIAL-HEADING.                                            ELELCCML
00684 *    *-----------------------------------------------------------*ELELCCML
00685 *    * DISPLAY THE INITIAL HEADINGS                              *ELELCCML
00686 *    *-----------------------------------------------------------*ELELCCML
00687      MOVE '3100'      TO WS-PARA-ID.                              ELELCCML
00688                                                                   ELELCCML
00689      IF CA-INQUIRY                                                ELELCCML
00690         MOVE INITIAL-INQUIRY       TO MSG-D                       ELELCCML
00691         MOVE INITIAL-VALUE-INQUIRY TO INITIAL-VALUE               ELELCCML
00692      ELSE                                                         ELELCCML
00693         MOVE INITIAL-VALUE-MAINT   TO INITIAL-VALUE.              ELELCCML
00694                                                                   ELELCCML
00695      IF CA-MAINTENANCE                                            ELELCCML
00696         MOVE INITIAL-MAINT         TO MSG-D.                      ELELCCML
00697                                                                   ELELCCML
00698      IF CA-SUPERVISORY                                            ELELCCML
00699         MOVE INITIAL-SUPERV        TO MSG-D.                      ELELCCML
00700                                                                   ELELCCML
00701      MOVE CA-TRANS-HDR             TO TITLE-D.                    ELELCCML
00702      MOVE INITIAL-VALUE            TO ERRM-D.                     ELELCCML
00703                                                                   ELELCCML
00704  3100-END.   EXIT.                                                ELELCCML
00705 /                                                                 ELELCCML
00706  3200-CREATE-SCREEN.                                              ELELCCML
00707 *    *-----------------------------------------------------------*ELELCCML
00708 *    * LOAD SCREEN                                               *ELELCCML
00709 *    *-----------------------------------------------------------*ELELCCML
00710      MOVE '3200'      TO WS-PARA-ID.                              ELELCCML
00711                                                                   ELELCCML
00712      IF ((CA-RECORD-LIST OR CA-SELECT-DE) AND                     ELELCCML
00713           CA-ADD) OR CA-SEL-ELEMENT-NBR-X = LOW-VALUES            ELELCCML
00714              MOVE CA-SEL-RECORD-PREFIX  TO PREF-D                 ELELCCML
00715              MOVE CA-SEL-RECORD-NAME    TO RL-NAME-D              ELELCCML
00716              MOVE ZEROS                 TO CA-SEL-ELEMENT-NBR     ELELCCML
00717              MOVE 'A'                   TO FCN-D                  ELELCCML
00718              MOVE DFHBMUNF              TO FCN-A                  ELELCCML
00719              MOVE -1                    TO FCN-L                  ELELCCML
00720              GO TO 3200-END.                                      ELELCCML
00721                                                                   ELELCCML
00722      MOVE CA-SEL-RECORD-PREFIX TO DE-RECORD-PREFIX.               ELELCCML
00723      MOVE CA-SEL-ELEMENT-NBR   TO DE-ELEMENT-NBR.                 ELELCCML
00724                                                                   ELELCCML
00725 *-->   READ THE DATA ELEMENT FILE DIRECTLY                        ELELCCML
00726                                                                   ELELCCML
00727      MOVE 'RD '                TO ELCIO-FILE-ACCESS-CODE.         ELELCCML
00728      MOVE DE-PRIMARY-KEY       TO ELCIO-VSAM-KEY.                 ELELCCML
00729      MOVE EL-ELPDE-REC-LEN     TO ELCIO-MAX-REC-LEN.              ELELCCML
00730      MOVE EL-DSN-ELPDE         TO ELCIO-FILE-DDNAME,              ELELCCML
00731                                   CIA-IO-GETMAIN-DDNAME.          ELELCCML
00732      MOVE 'M'                  TO ELCIO-STORAGE.                  ELELCCML
00733                                                                   ELELCCML
00734      SET  CIA-IO-PARM-AREA-PNTR   TO  CIA-ELPDE-IOPARM-AREA-PNTR. ELELCCML
00735      SET  ELCIO-REC-AREA-ADDRESS  TO  CIA-ELPDE-REC-AREA-PNTR.    ELELCCML
00736                                                                   ELELCCML
00737      EXEC CICS LINK                                               ELELCCML
00738           PROGRAM('ELAIOPGM')                                     ELELCCML
00739           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCCML
00740           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
00741      END-EXEC.                                                    ELELCCML
00742                                                                   ELELCCML
00743      IF ELCIO-REC-NOT-FOUND                                       ELELCCML
00744         MOVE DE-NOTFND       TO ERRM-D                            ELELCCML
00745         MOVE -1              TO DE-NAME-L                         ELELCCML
00746      ELSE                                                         ELELCCML
00747         MOVE DE-NBR-DESC-LINES TO DE-NBR-DESC-LINES               ELELCCML
00748         PERFORM 3300-LOAD-SCREEN  THRU 3300-END.                  ELELCCML
00749  3200-END.    EXIT.                                               ELELCCML
00750 /                                                                 ELELCCML
00751  3300-LOAD-SCREEN.                                                ELELCCML
00752 *    *-----------------------------------------------------------*ELELCCML
00753 *    * LOAD SCREEN                                               *ELELCCML
00754 *    *-----------------------------------------------------------*ELELCCML
00755      MOVE '3300'                   TO   WS-PARA-ID.               ELELCCML
00756                                                                   ELELCCML
00757      MOVE CA-SEL-RECORD-PREFIX     TO   PREF-D.                   ELELCCML
00758      MOVE CA-SEL-RECORD-NAME       TO   RL-NAME-D.                ELELCCML
00759      MOVE DE-RECORD-PREFIX         TO   DE-NUMBER-D.              ELELCCML
00760      MOVE DE-ELEMENT-NAME          TO   DE-NAME-D,                ELELCCML
00761                                         CA-SEL-ELEMENT-NAME.      ELELCCML
00762                                                                   ELELCCML
00763      IF NOT CA-INQUIRY OR (CA-INQUIRY AND CA-LOCATE)              ELELCCML
00764          MOVE DFHBMUNF TO DE-NAME-A.                              ELELCCML
00765      PERFORM 3310-FORMAT-ELEMENT-NBR  THRU                        ELELCCML
00766              3310-END.                                            ELELCCML
00767      MOVE '3300'                   TO   WS-PARA-ID.               ELELCCML
00768                                                                   ELELCCML
00769      MOVE DE-ELEMENT-FORMAT        TO DE-FORMAT-D.                ELELCCML
00770      MOVE DE-ELEMENT-LENGTH        TO DE-LENGTH-D.                ELELCCML
00771      MOVE DE-FORMAT-COMMENT        TO DE-FORMCOM-D.               ELELCCML
00772      MOVE DE-RECORD-POS            TO DE-POS-D.                   ELELCCML
00773      MOVE DE-STORED-LENGTH         TO DE-STORED-LENGTH-D.         ELELCCML
00774      MOVE DE-STORED-DECIMALS       TO DE-DECIMAL-D.               ELELCCML
00775      MOVE DE-STORED-FORMAT         TO DE-STORED-FORMAT-D.         ELELCCML
00776      MOVE DE-COBOL-NAME            TO DE-COBOL-NAME-D.            ELELCCML
00777      MOVE DE-BAL-NAME              TO DE-BAL-NAME-D.              ELELCCML
00778      MOVE ZEROS                    TO SCREEN-CTR.                 ELELCCML
00779                                                                   ELELCCML
00780      PERFORM 3350-LOAD-DESCRIPTION THRU 3350-END                  ELELCCML
00781            DE-NBR-DESC-LINES TIMES.                               ELELCCML
00782      ADD 1 TO SCREEN-CTR.                                         ELELCCML
00783                                                                   ELELCCML
00784      PERFORM 3050-CLEAR-DESC-ATTRB THRU 3050-END                  ELELCCML
00785          VARYING SCREEN-CTR FROM SCREEN-CTR                       ELELCCML
00786            BY 1 UNTIL SCREEN-CTR > MAX-DES-LINES.                 ELELCCML
00787      MOVE -1                    TO FCN-L.                         ELELCCML
00788                                                                   ELELCCML
00789  3300-END.    EXIT.                                               ELELCCML
00790                                                                   ELELCCML
00791  3310-FORMAT-ELEMENT-NBR.                                         ELELCCML
00792      MOVE '3310'                   TO   WS-PARA-ID.               ELELCCML
00793                                                                   ELELCCML
00794      MOVE DE-ELEMENT-NBR        TO FORMAT-ELEMENT-NBR.            ELELCCML
00795      MOVE DE-NBR                TO FIRST-THREE-X.                 ELELCCML
00796      MOVE DE-INSERT-NBR         TO LAST-TWO-X.                    ELELCCML
00797      MOVE '.'                   TO PERIOD-X.                      ELELCCML
00798      MOVE FORMAT-ELEMENT-NBR-X  TO DE-NUMBER-D.                   ELELCCML
00799  3310-END.    EXIT.                                               ELELCCML
00800                                                                   ELELCCML
00801  3350-LOAD-DESCRIPTION.                                           ELELCCML
00802      MOVE '3350'      TO WS-PARA-ID.                              ELELCCML
00803                                                                   ELELCCML
00804      ADD 1 TO SCREEN-CTR.                                         ELELCCML
00805      MOVE DE-DESC-LINE (SCREEN-CTR) TO                            ELELCCML
00806                DE-DESCRIPTION-D (SCREEN-CTR).                     ELELCCML
00807                                                                   ELELCCML
00808      IF CA-INQUIRY                                                ELELCCML
00809         NEXT SENTENCE                                             ELELCCML
00810      ELSE                                                         ELELCCML
00811         MOVE DFHBMUNF TO   DE-DESCRIPTION-A (SCREEN-CTR).         ELELCCML
00812                                                                   ELELCCML
00813  3350-END.    EXIT.                                               ELELCCML
00814 /                                                                 ELELCCML
00815  3400-EDIT-SCREEN.                                                ELELCCML
00816 *    *-----------------------------------------------------------*ELELCCML
00817 *    * EDIT CURRENT SCREEN                                       *ELELCCML
00818 *    *-----------------------------------------------------------*ELELCCML
00819      MOVE '3400'      TO WS-PARA-ID.                              ELELCCML
00820                                                                   ELELCCML
00821      IF CA-INQUIRY                                                ELELCCML
00822         MOVE INITIAL-VALUE-INQUIRY   TO  INITIAL-VALUE            ELELCCML
00823      ELSE                                                         ELELCCML
00824         MOVE INITIAL-VALUE-MAINT     TO  INITIAL-VALUE.           ELELCCML
00825                                                                   ELELCCML
00826      MOVE INITIAL-VALUE              TO  ERRM-D.                  ELELCCML
00827      MOVE FCN-D                      TO  CA-CURRENT-FUNCTION.     ELELCCML
00828                                                                   ELELCCML
00829      IF EIBAID = DFHPF7                                           ELELCCML
00830          PERFORM 3500-FIND-PREV-DE  THRU 3500-END                 ELELCCML
00831          PERFORM 3200-CREATE-SCREEN THRU 3200-END                 ELELCCML
00832          GO TO 3400-END.                                          ELELCCML
00833                                                                   ELELCCML
00834      IF EIBAID = DFHPF8                                           ELELCCML
00835          PERFORM 3600-FIND-NEXT-DE  THRU 3600-END                 ELELCCML
00836          PERFORM 3200-CREATE-SCREEN THRU 3200-END                 ELELCCML
00837          GO TO 3400-END.                                          ELELCCML
00838                                                                   ELELCCML
00839      IF CA-INQUIRY AND NOT CA-LOCATE                              ELELCCML
00840        MOVE UNAUTHOR-MAINTAIN TO ERRM-D                           ELELCCML
00841        MOVE -1                TO FCN-L                            ELELCCML
00842        MOVE SPACE             TO FCN-D                            ELELCCML
00843        MOVE SPACE             TO CA-CURRENT-FUNCTION              ELELCCML
00844        GO TO 3400-END.                                            ELELCCML
00845                                                                   ELELCCML
00846      IF FCN-L > +0                                                ELELCCML
00847          NEXT SENTENCE                                            ELELCCML
00848      ELSE                                                         ELELCCML
00849        MOVE -1                 TO FCN-L                           ELELCCML
00850        MOVE NO-FUNCTION-SELECT TO ERRM-D                          ELELCCML
00851        GO TO 3400-END.                                            ELELCCML
00852                                                                   ELELCCML
00853      MOVE DFHBMUNF TO FCN-A.                                      ELELCCML
00854      IF CA-ADD                                                    ELELCCML
00855          PERFORM 4000-VALIDATE-ADD    THRU 4000-END               ELELCCML
00856          GO TO 3400-END.                                          ELELCCML
00857                                                                   ELELCCML
00858      IF CA-CHANGE                                                 ELELCCML
00859          PERFORM 4100-VALIDATE-INPUT  THRU 4100-END               ELELCCML
00860          GO TO 3400-END.                                          ELELCCML
00861                                                                   ELELCCML
00862      IF CA-DELETE                                                 ELELCCML
00863          MOVE DELETE-CONFIRM   TO   ERRM-D                        ELELCCML
00864          MOVE -1               TO   FCN-L                         ELELCCML
00865          GO TO 3400-END.                                          ELELCCML
00866                                                                   ELELCCML
00867      IF CA-MAP-FROM                                               ELELCCML
00868          PERFORM 4300-VALIDATE-MAP  THRU 4300-END                 ELELCCML
00869          GO TO 3400-END.                                          ELELCCML
00870                                                                   ELELCCML
00871      IF CA-LOCATE                                                 ELELCCML
00872          PERFORM 4600-LOCATE-ANOTHER-ELEMENT  THRU                ELELCCML
00873                  4600-END                                         ELELCCML
00874          GO TO 3400-END.                                          ELELCCML
00875                                                                   ELELCCML
00876      MOVE INVALID-SELECT  TO ERRM-D.                              ELELCCML
00877      MOVE -1              TO FCN-L.                               ELELCCML
00878  3400-END.    EXIT.                                               ELELCCML
00879 /                                                                 ELELCCML
00880  3500-FIND-PREV-DE.                                               ELELCCML
00881 *    *-----------------------------------------------------------*ELELCCML
00882 *    * GET PREVIOUS DATA ELEMENT RECORD                          *ELELCCML
00883 *    *-----------------------------------------------------------*ELELCCML
00884      MOVE '3500'      TO WS-PARA-ID.                              ELELCCML
00885                                                                   ELELCCML
00886      MOVE CA-SEL-RECORD-PREFIX TO DE-RECORD-PREFIX.               ELELCCML
00887      MOVE CA-SEL-ELEMENT-NBR   TO DE-ELEMENT-NBR.                 ELELCCML
00888      MOVE SPACES               TO FCN-D.                          ELELCCML
00889                                                                   ELELCCML
00890 *--> CODE SB = START BROWSE PREVIOUS                              ELELCCML
00891      MOVE 'SBP'             TO ELCIO-FILE-ACCESS-CODE.            ELELCCML
00892      MOVE 'EQ '             TO ELCIO-CIO-QUAL.                    ELELCCML
00893      MOVE DE-PRIMARY-KEY    TO ELCIO-VSAM-KEY.                    ELELCCML
00894      MOVE EL-ELPDE-REC-LEN  TO ELCIO-MAX-REC-LEN.                 ELELCCML
00895      MOVE EL-DSN-ELPDE      TO ELCIO-FILE-DDNAME,                 ELELCCML
00896                                CIA-IO-GETMAIN-DDNAME.             ELELCCML
00897      MOVE 'M'               TO ELCIO-STORAGE.                     ELELCCML
00898                                                                   ELELCCML
00899      SET  CIA-IO-PARM-AREA-PNTR TO CIA-ELPDE-IOPARM-AREA-PNTR.    ELELCCML
00900      SET  ELCIO-REC-AREA-ADDRESS  TO                              ELELCCML
00901                                CIA-ELPDE-REC-AREA-PNTR.           ELELCCML
00902                                                                   ELELCCML
00903      EXEC CICS LINK                                               ELELCCML
00904           PROGRAM('ELAIOPGM')                                     ELELCCML
00905           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCCML
00906           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
00907      END-EXEC.                                                    ELELCCML
00908                                                                   ELELCCML
00909 ******************************************************************ELELCCML
00910 *     ANYTHING OTHER THAN A GOOD RETURN CODE IS CONSIDERED AS   **ELELCCML
00911 *      ENCOUNTERING THE BEGINNING OF THE FILE. THE ONLY CODES   **ELELCCML
00912 *      OTHER THAN ZERO(0-GOOD RETURN) SHOULD BE 01-NOT FOUND    **ELELCCML
00913 *      OR 03-END OF FILE.                                       **ELELCCML
00914 ******************************************************************ELELCCML
00915                                                                   ELELCCML
00916      IF NOT ELCIO-GOOD-RETURN                                     ELELCCML
00917           GO TO 3500-NOTFND-ERROR.                                ELELCCML
00918                                                                   ELELCCML
00919  3500-GET-PREV.                                                   ELELCCML
00920                                                                   ELELCCML
00921      MOVE DE-NBR-DESC-LINES TO DE-NBR-DESC-LINES.                 ELELCCML
00922                                                                   ELELCCML
00923 *--> THE CALL AUTOMATICALLY WILL DO A READ PREVIOUS               ELELCCML
00924      EXEC CICS LINK                                               ELELCCML
00925           PROGRAM('ELAIOPGM')                                     ELELCCML
00926           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCCML
00927           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
00928      END-EXEC.                                                    ELELCCML
00929                                                                   ELELCCML
00930      IF NOT ELCIO-GOOD-RETURN                                     ELELCCML
00931           GO TO 3500-NOTFND-ERROR.                                ELELCCML
00932                                                                   ELELCCML
00933      IF DE-DELETE                                                 ELELCCML
00934          GO TO 3500-GET-PREV.                                     ELELCCML
00935                                                                   ELELCCML
00936      IF DE-RECORD-PREFIX = LOW-VALUES                             ELELCCML
00937          GO TO 3500-NOTFND-ERROR.                                 ELELCCML
00938                                                                   ELELCCML
00939      IF DE-RECORD-PREFIX NOT = CA-SEL-RECORD-PREFIX               ELELCCML
00940          GO TO 3500-END-BROWSE.                                   ELELCCML
00941                                                                   ELELCCML
00942      MOVE DE-RECORD-PREFIX TO CA-SEL-RECORD-PREFIX.               ELELCCML
00943      MOVE DE-ELEMENT-NBR   TO CA-SEL-ELEMENT-NBR.                 ELELCCML
00944      MOVE DE-ELEMENT-NAME  TO CA-SEL-ELEMENT-NAME.                ELELCCML
00945                                                                   ELELCCML
00946  3500-END-BROWSE.                                                 ELELCCML
00947                                                                   ELELCCML
00948 *--> CODE EB = END PREVIOUS*****                                  ELELCCML
00949      MOVE 'EB '            TO ELCIO-FILE-ACCESS-CODE.             ELELCCML
00950      EXEC CICS LINK                                               ELELCCML
00951           PROGRAM('ELAIOPGM')                                     ELELCCML
00952           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCCML
00953           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
00954      END-EXEC.                                                    ELELCCML
00955                                                                   ELELCCML
00956      GO TO 3500-END.                                              ELELCCML
00957  3500-NOTFND-ERROR.                                               ELELCCML
00958      MOVE BEGINNING-FILE-MSG TO ERRM-D.                           ELELCCML
00959      MOVE 'Y'                 TO ERROR-SW.                        ELELCCML
00960  3500-END.     EXIT.                                              ELELCCML
00961 /                                                                 ELELCCML
00962  3600-FIND-NEXT-DE.                                               ELELCCML
00963 *    *-----------------------------------------------------------*ELELCCML
00964 *    * GET THE NEXT DATA ELEMENT RECORD                          *ELELCCML
00965 *    *-----------------------------------------------------------*ELELCCML
00966      MOVE '3600'      TO WS-PARA-ID.                              ELELCCML
00967                                                                   ELELCCML
00968      MOVE CA-SEL-RECORD-PREFIX TO DE-RECORD-PREFIX.               ELELCCML
00969      MOVE CA-SEL-ELEMENT-NBR   TO DE-ELEMENT-NBR.                 ELELCCML
00970      MOVE SPACES               TO FCN-D.                          ELELCCML
00971                                                                   ELELCCML
00972 *--> CODE SB = START BROWSE *****                                 ELELCCML
00973      MOVE 'SB '                TO ELCIO-FILE-ACCESS-CODE.         ELELCCML
00974      MOVE 'EQ '                TO ELCIO-CIO-QUAL.                 ELELCCML
00975      MOVE DE-PRIMARY-KEY       TO ELCIO-VSAM-KEY.                 ELELCCML
00976      MOVE EL-ELPDE-REC-LEN     TO ELCIO-MAX-REC-LEN.              ELELCCML
00977      MOVE EL-DSN-ELPDE         TO ELCIO-FILE-DDNAME,              ELELCCML
00978                                   CIA-IO-GETMAIN-DDNAME.          ELELCCML
00979      MOVE 'M'                  TO ELCIO-STORAGE.                  ELELCCML
00980                                                                   ELELCCML
00981      SET  CIA-IO-PARM-AREA-PNTR TO CIA-ELPDE-IOPARM-AREA-PNTR.    ELELCCML
00982      SET  ELCIO-REC-AREA-ADDRESS  TO                              ELELCCML
00983                                CIA-ELPDE-REC-AREA-PNTR.           ELELCCML
00984                                                                   ELELCCML
00985      EXEC CICS LINK                                               ELELCCML
00986           PROGRAM('ELAIOPGM')                                     ELELCCML
00987           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCCML
00988           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
00989      END-EXEC.                                                    ELELCCML
00990                                                                   ELELCCML
00991 ******************************************************************ELELCCML
00992 **    ANYTHING OTHER THAN A GOOD RETURN CODE IS CONSIDERED AS   **ELELCCML
00993 **     ENCOUNTERING THE END OF THE FILE. THE ONLY CODES         **ELELCCML
00994 **     OTHER THAN ZERO(0-GOOD RETURN) SHOULD BE 01-NOT FOUND    **ELELCCML
00995 **     OR 03-END OF FILE                                        **ELELCCML
00996 ******************************************************************ELELCCML
00997                                                                   ELELCCML
00998      IF NOT ELCIO-GOOD-RETURN                                     ELELCCML
00999           GO TO 3600-NOTFND-ERROR.                                ELELCCML
01000                                                                   ELELCCML
01001  3600-GET-NEXT.                                                   ELELCCML
01002      MOVE DE-NBR-DESC-LINES TO DE-NBR-DESC-LINES.                 ELELCCML
01003                                                                   ELELCCML
01004 *--> THIS CALL WILL AUTOMATICALLY DO A READ NEXT                  ELELCCML
01005      EXEC CICS LINK                                               ELELCCML
01006           PROGRAM('ELAIOPGM')                                     ELELCCML
01007           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCCML
01008           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
01009      END-EXEC.                                                    ELELCCML
01010                                                                   ELELCCML
01011      IF NOT ELCIO-GOOD-RETURN                                     ELELCCML
01012           GO TO 3600-NOTFND-ERROR.                                ELELCCML
01013                                                                   ELELCCML
01014      IF DE-DELETE                                                 ELELCCML
01015          GO TO 3600-GET-NEXT.                                     ELELCCML
01016                                                                   ELELCCML
01017      IF DE-RECORD-PREFIX NOT = CA-SEL-RECORD-PREFIX               ELELCCML
01018          GO TO 3600-END-BROWSE.                                   ELELCCML
01019                                                                   ELELCCML
01020      MOVE DE-RECORD-PREFIX TO CA-SEL-RECORD-PREFIX.               ELELCCML
01021      MOVE DE-ELEMENT-NBR   TO CA-SEL-ELEMENT-NBR.                 ELELCCML
01022      MOVE DE-ELEMENT-NAME  TO CA-SEL-ELEMENT-NAME.                ELELCCML
01023                                                                   ELELCCML
01024  3600-END-BROWSE.                                                 ELELCCML
01025                                                                   ELELCCML
01026 *--> CODE EB = END PREVIOUS*****                                  ELELCCML
01027      MOVE 'EB '            TO ELCIO-FILE-ACCESS-CODE.             ELELCCML
01028      EXEC CICS LINK                                               ELELCCML
01029           PROGRAM('ELAIOPGM')                                     ELELCCML
01030           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCCML
01031           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
01032      END-EXEC.                                                    ELELCCML
01033      GO TO 3600-END.                                              ELELCCML
01034                                                                   ELELCCML
01035  3600-NOTFND-ERROR.                                               ELELCCML
01036      MOVE END-OF-FILE-MSG TO ERRM-D.                              ELELCCML
01037      MOVE 'Y'             TO ERROR-SW.                            ELELCCML
01038  3600-END.     EXIT.                                              ELELCCML
01039 /                                                                 ELELCCML
01040  4000-VALIDATE-ADD.                                               ELELCCML
01041 *    *-----------------------------------------------------------*ELELCCML
01042 *    * VALIDATE ADD                                              *ELELCCML
01043 *    *-----------------------------------------------------------*ELELCCML
01044      MOVE '4000'      TO WS-PARA-ID.                              ELELCCML
01045                                                                   ELELCCML
01046      IF DE-NAME-L > +0                                            ELELCCML
01047          NEXT SENTENCE                                            ELELCCML
01048      ELSE                                                         ELELCCML
01049        MOVE NAME-MB-ENTER-ADD TO ERRM-D                           ELELCCML
01050        MOVE -1                TO DE-NAME-L                        ELELCCML
01051        GO TO 4000-END.                                            ELELCCML
01052                                                                   ELELCCML
01053      IF DE-COBOL-NAME-L > +0                                      ELELCCML
01054         MOVE DE-COBOL-NAME-D TO COBOL-NAME-SEARCH                 ELELCCML
01055      ELSE                                                         ELELCCML
01056       MOVE SPACES TO COBOL-NAME-SEARCH.                           ELELCCML
01057                                                                   ELELCCML
01058      IF DE-MAP-NAME-L > +0                                        ELELCCML
01059         OR DE-MAP-PREFIX-L > +0                                   ELELCCML
01060             MOVE MAP-FROM-NAME-PRE-INVALID TO ERRM-D              ELELCCML
01061             MOVE -1                        TO DE-MAP-PREFIX-L     ELELCCML
01062             GO TO 4000-END.                                       ELELCCML
01063                                                                   ELELCCML
01064      MOVE DE-NAME-D TO NAME-SEARCH.                               ELELCCML
01065      MOVE CA-SEL-RECORD-PREFIX TO PREFIX-SEARCH.                  ELELCCML
01066      PERFORM 5000-NAME-SEARCH  THRU 5000-END.                     ELELCCML
01067                                                                   ELELCCML
01068      IF FOUND-SW = 'Y'                                            ELELCCML
01069          MOVE DE-ON-FILE TO ERRM-D                                ELELCCML
01070          MOVE -1         TO DE-NAME-L                             ELELCCML
01071          MOVE DFHBMUBF   TO DE-NAME-A                             ELELCCML
01072          GO TO 4000-END.                                          ELELCCML
01073                                                                   ELELCCML
01074      IF FOUND-COBOL-NAME = 'Y'                                    ELELCCML
01075          MOVE COBOL-NAME-EXIST TO ERRM-D                          ELELCCML
01076          MOVE -1               TO DE-COBOL-NAME-L                 ELELCCML
01077          GO TO 4000-END.                                          ELELCCML
01078                                                                   ELELCCML
01079      PERFORM 4500-FIND-NEW-INSERT-NBR THRU 4500-END.              ELELCCML
01080      IF ERROR-SW = 'Y'                                            ELELCCML
01081          GO TO 4000-END.                                          ELELCCML
01082                                                                   ELELCCML
01083      IF NEW-NBR-SW = 'N'                                          ELELCCML
01084          MOVE UNABLE-TO-ASSIGN-NUM TO ERRM-D                      ELELCCML
01085          MOVE -1                   TO DE-NAME-L                   ELELCCML
01086          GO TO 4000-END.                                          ELELCCML
01087                                                                   ELELCCML
01088      MOVE MAX-DES-LINES TO DE-NBR-DESC-LINES.                     ELELCCML
01089      MOVE SPACES        TO DATA-ELEMENT.                          ELELCCML
01090                                                                   ELELCCML
01091      IF DE-COBOL-NAME-L > +0                                      ELELCCML
01092         MOVE DE-COBOL-NAME-D  TO  DE-COBOL-NAME                   ELELCCML
01093      ELSE                                                         ELELCCML
01094       MOVE SPACES             TO  DE-COBOL-NAME.                  ELELCCML
01095                                                                   ELELCCML
01096      MOVE 1      TO DE-NBR-DESC-LINES.                            ELELCCML
01097      MOVE ZEROS  TO DE-ELEMENT-LENGTH,    DE-RECORD-POS,          ELELCCML
01098                     DE-STORED-LENGTH,     DE-STORED-DECIMALS,     ELELCCML
01099                     DE-CODE-VALUES-CT.                            ELELCCML
01100                                                                   ELELCCML
01101      MOVE CA-SEL-RECORD-PREFIX TO DE-RECORD-PREFIX,               ELELCCML
01102                                   DE-RECORD-PREFIX-N,             ELELCCML
01103      MOVE DE-NAME-D            TO DE-ELEMENT-NAME.                ELELCCML
01104      MOVE NEW-DE-NBR           TO DE-ELEMENT-NBR.                 ELELCCML
01105                                                                   ELELCCML
01106      PERFORM 3310-FORMAT-ELEMENT-NBR     THRU 3310-END.           ELELCCML
01107      PERFORM 4110-VALIDATE-REMAIN-INPUT  THRU 4110-END.           ELELCCML
01108                                                                   ELELCCML
01109  4000-END.    EXIT.                                               ELELCCML
01110 /                                                                 ELELCCML
01111  4100-VALIDATE-INPUT.                                             ELELCCML
01112 *    *-----------------------------------------------------------*ELELCCML
01113 *    * VALIDATE INPUT                                            *ELELCCML
01114 *    *-----------------------------------------------------------*ELELCCML
01115      MOVE '4100'      TO WS-PARA-ID.                              ELELCCML
01116                                                                   ELELCCML
01117      MOVE 'N' TO REWRITE-SW.                                      ELELCCML
01118      MOVE CA-SEL-RECORD-PREFIX     TO  DE-RECORD-PREFIX.          ELELCCML
01119      MOVE CA-SEL-ELEMENT-NBR       TO  DE-ELEMENT-NBR.            ELELCCML
01120                                                                   ELELCCML
01121 *--> CODE RD = READ DIRECT  *****                                 ELELCCML
01122      MOVE 'RD '                    TO  ELCIO-FILE-ACCESS-CODE.    ELELCCML
01123      MOVE DE-PRIMARY-KEY           TO  ELCIO-VSAM-KEY.            ELELCCML
01124      MOVE EL-ELPDE-REC-LEN         TO  ELCIO-MAX-REC-LEN.         ELELCCML
01125      MOVE EL-DSN-ELPDE             TO  ELCIO-FILE-DDNAME,         ELELCCML
01126                                        CIA-IO-GETMAIN-DDNAME.     ELELCCML
01127      MOVE 'M'                      TO  ELCIO-STORAGE.             ELELCCML
01128                                                                   ELELCCML
01129      SET  CIA-IO-PARM-AREA-PNTR   TO  CIA-ELPDE-IOPARM-AREA-PNTR. ELELCCML
01130      SET  ELCIO-REC-AREA-ADDRESS  TO  CIA-ELPDE-REC-AREA-PNTR.    ELELCCML
01131                                                                   ELELCCML
01132      EXEC CICS LINK                                               ELELCCML
01133           PROGRAM('ELAIOPGM')                                     ELELCCML
01134           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCCML
01135           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
01136      END-EXEC.                                                    ELELCCML
01137                                                                   ELELCCML
01138      IF ELCIO-REC-NOT-FOUND                                       ELELCCML
01139           GO TO 4100-NOTFND-ERR.                                  ELELCCML
01140                                                                   ELELCCML
01141      MOVE DE-NBR-DESC-LINES TO DE-NBR-DESC-LINES.                 ELELCCML
01142                                                                   ELELCCML
01143      IF DE-DELETE                                                 ELELCCML
01144          MOVE UNDELETE-REQ TO ERRM-D                              ELELCCML
01145          MOVE -1           TO FCN-L                               ELELCCML
01146          GO TO 4100-END.                                          ELELCCML
01147                                                                   ELELCCML
01148      MOVE DE-ELEMENT-NAME TO SAVE-OLD-DE-NAME.                    ELELCCML
01149                                                                   ELELCCML
01150 ******************************************************************ELELCCML
01151 *** SHOULD EITHER THE ELEMENT OR COBOL NAME(S) CHANGE, BYPASS     ELELCCML
01152 *** THE FOLLOWING READ FOR UPDATE FUNCTION AND CONTINUE TO CHECK  ELELCCML
01153 *** THE REMAINIG FIELDS.   NAC 01/89.                             ELELCCML
01154 ******************************************************************ELELCCML
01155                                                                   ELELCCML
01156      IF (DE-NAME-L > +0                                           ELELCCML
01157         AND DE-ELEMENT-NAME NOT = DE-NAME-D)                      ELELCCML
01158          OR (DE-COBOL-NAME-L > +0 AND                             ELELCCML
01159               DE-COBOL-NAME-D NOT = DE-COBOL-NAME)                ELELCCML
01160          OR DE-COBOL-NAME-A = DFHBMEOF                            ELELCCML
01161             PERFORM 4400-PROCESS-NAME-CHANGE     THRU 4400-END    ELELCCML
01162             PERFORM 4110-VALIDATE-REMAIN-INPUT   THRU 4110-END    ELELCCML
01163             GO TO 4100-END.                                       ELELCCML
01164                                                                   ELELCCML
01165 *--> CODE RU = READ DIRECT  *****                                 ELELCCML
01166                                                                   ELELCCML
01167      MOVE 'RU '            TO ELCIO-FILE-ACCESS-CODE.             ELELCCML
01168      MOVE DE-PRIMARY-KEY   TO ELCIO-VSAM-KEY.                     ELELCCML
01169      EXEC CICS LINK                                               ELELCCML
01170           PROGRAM('ELAIOPGM')                                     ELELCCML
01171           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCCML
01172           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
01173      END-EXEC.                                                    ELELCCML
01174                                                                   ELELCCML
01175      IF ELCIO-REC-NOT-FOUND                                       ELELCCML
01176           GO TO 4100-NOTFND-ERR.                                  ELELCCML
01177                                                                   ELELCCML
01178      PERFORM 4110-VALIDATE-REMAIN-INPUT  THRU 4110-END            ELELCCML
01179      GO TO 4100-END.                                              ELELCCML
01180                                                                   ELELCCML
01181  4100-NOTFND-ERR.                                                 ELELCCML
01182      MOVE DE-NOTFND       TO ERRM-D.                              ELELCCML
01183      MOVE -1              TO DE-NAME-L.                           ELELCCML
01184  4100-END.   EXIT.                                                ELELCCML
01185                                                                   ELELCCML
01186  4110-VALIDATE-REMAIN-INPUT.                                      ELELCCML
01187      MOVE '4110'      TO WS-PARA-ID.                              ELELCCML
01188                                                                   ELELCCML
01189      IF CFORML > +0                                               ELELCCML
01190           MOVE DE-FORMAT-D TO FORMAT-CHECK                        ELELCCML
01191           IF VALID-FORMAT                                         ELELCCML
01192               IF DE-FORMAT-D NOT = DE-ELEMENT-FORMAT              ELELCCML
01193                    MOVE 'Y' TO REWRITE-SW                         ELELCCML
01194                    MOVE DE-FORMAT-D TO DE-ELEMENT-FORMAT          ELELCCML
01195               ELSE                                                ELELCCML
01196                 NEXT SENTENCE                                     ELELCCML
01197           ELSE                                                    ELELCCML
01198             MOVE INVALID-FORMAT  TO ERRM-D                        ELELCCML
01199             MOVE 'Y'             TO ERROR-SW                      ELELCCML
01200             MOVE -1              TO DE-FORMAT-L                   ELELCCML
01201             MOVE DFHBMUBF        TO DE-FORMAT-A                   ELELCCML
01202             GO TO 4110-END.                                       ELELCCML
01203                                                                   ELELCCML
01204      IF CLENGTL > +0                                              ELELCCML
01205          IF CLENGTI NOT = DE-ELEMENT-LENGTH                       ELELCCML
01206             MOVE 'Y' TO REWRITE-SW                                ELELCCML
01207             MOVE CLENGTI TO DE-ELEMENT-LENGTH.                    ELELCCML
01208                                                                   ELELCCML
01209      IF DE-FORMCOM-L > +0                                         ELELCCML
01210         IF DE-FORMCOM-D NOT = DE-FORMAT-COMMENT                   ELELCCML
01211             MOVE 'Y' TO REWRITE-SW                                ELELCCML
01212             MOVE DE-FORMCOM-D TO DE-FORMAT-COMMENT.               ELELCCML
01213                                                                   ELELCCML
01214      IF CPOSITL > +0                                              ELELCCML
01215          IF CPOSITI NOT = DE-RECORD-POS                           ELELCCML
01216              MOVE 'Y' TO REWRITE-SW                               ELELCCML
01217              MOVE CPOSITI TO DE-RECORD-POS.                       ELELCCML
01218                                                                   ELELCCML
01219      IF CSTORLL > +0                                              ELELCCML
01220          IF CSTORLI NOT = DE-STORED-LENGTH                        ELELCCML
01221            MOVE 'Y' TO REWRITE-SW                                 ELELCCML
01222            MOVE CSTORLI TO DE-STORED-LENGTH.                      ELELCCML
01223                                                                   ELELCCML
01224      IF CDECIML > +0                                              ELELCCML
01225          IF CDECIMI NOT = DE-STORED-DECIMALS                      ELELCCML
01226            MOVE 'Y' TO REWRITE-SW                                 ELELCCML
01227            MOVE CDECIMI TO DE-STORED-DECIMALS.                    ELELCCML
01228                                                                   ELELCCML
01229      IF DE-STORED-FORMAT-L > +0                                   ELELCCML
01230           MOVE DE-STORED-FORMAT-D TO STORED-CHECK                 ELELCCML
01231           IF VALID-STORED                                         ELELCCML
01232               IF DE-STORED-FORMAT-D NOT = DE-STORED-FORMAT        ELELCCML
01233                    MOVE 'Y' TO REWRITE-SW                         ELELCCML
01234                    MOVE DE-STORED-FORMAT-D TO DE-STORED-FORMAT    ELELCCML
01235               ELSE                                                ELELCCML
01236                 NEXT SENTENCE                                     ELELCCML
01237           ELSE                                                    ELELCCML
01238             MOVE INVALID-STORED-FORMAT  TO ERRM-D                 ELELCCML
01239             MOVE 'Y'                    TO ERROR-SW               ELELCCML
01240             MOVE -1                     TO DE-STORED-FORMAT-L     ELELCCML
01241             MOVE DFHBMUBF               TO DE-STORED-FORMAT-A     ELELCCML
01242             GO TO 4110-END.                                       ELELCCML
01243                                                                   ELELCCML
01244      IF DE-BAL-NAME-L > +0                                        ELELCCML
01245         AND DE-BAL-NAME-D NOT = DE-BAL-NAME                       ELELCCML
01246             MOVE 'Y' TO REWRITE-SW                                ELELCCML
01247             MOVE DE-BAL-NAME-D TO DE-BAL-NAME.                    ELELCCML
01248                                                                   ELELCCML
01249      MOVE ZERO TO SCREEN-CTR.                                     ELELCCML
01250      PERFORM 4200-CHECK-DESCRIPTION  THRU                         ELELCCML
01251              4200-END                                             ELELCCML
01252            UNTIL SCREEN-CTR > MAX-DES-LINES.                      ELELCCML
01253                                                                   ELELCCML
01254      PERFORM 4250-DETER-LINE-CTR  THRU 4250-END.                  ELELCCML
01255      IF SCREEN-CTR = 1                                            ELELCCML
01256         MOVE DE-BASE-LENGTH TO ELPDE-LENGTH                       ELELCCML
01257      ELSE                                                         ELELCCML
01258         SUBTRACT 1 FROM SCREEN-CTR                                ELELCCML
01259         MULTIPLY SCREEN-CTR BY DESC-LINE-LENGTH                   ELELCCML
01260               GIVING ADD-DESC-LENGTH                              ELELCCML
01261         ADD ADD-DESC-LENGTH, DE-BASE-LENGTH                       ELELCCML
01262             GIVING ELPDE-LENGTH.                                  ELELCCML
01263                                                                   ELELCCML
01264      IF CA-ADD                                                    ELELCCML
01265           MOVE 'E' TO DE-AUTO-REPRINT-FLAG                        ELELCCML
01266           MOVE 'N' TO DE-CODES-FLAG                               ELELCCML
01267 *--> CODE WRITE RECORD                                            ELELCCML
01268           MOVE 'WR '                 TO ELCIO-FILE-ACCESS-CODE    ELELCCML
01269           MOVE DE-PRIMARY-KEY        TO ELCIO-VSAM-KEY            ELELCCML
01270           MOVE ELPDE-LENGTH          TO ELCIO-RECORD-LEN          ELELCCML
01271           SET  CIA-IO-PARM-AREA-PNTR   TO                         ELELCCML
01272                CIA-ELPDE-IOPARM-AREA-PNTR                         ELELCCML
01273           SET  ELCIO-REC-AREA-ADDRESS  TO                         ELELCCML
01274                             CIA-ELPDE-REC-AREA-PNTR               ELELCCML
01275           EXEC CICS LINK                                          ELELCCML
01276                PROGRAM('ELAIOPGM')                                ELELCCML
01277                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCCML
01278                LENGTH(EL-CIA-POINTER-LEN)                         ELELCCML
01279           END-EXEC                                                ELELCCML
01280           MOVE DE-ELEMENT-NBR   TO CA-SEL-ELEMENT-NBR             ELELCCML
01281           MOVE DE-ELEMENT-NAME  TO CA-SEL-ELEMENT-NAME.           ELELCCML
01282                                                                   ELELCCML
01283      IF CA-CHANGE AND REWRITE-SW = 'Y'                            ELELCCML
01284            IF DE-AUTO-REPRINT-FLAG  =  'E'                        ELELCCML
01285                NEXT SENTENCE                                      ELELCCML
01286            ELSE                                                   ELELCCML
01287               MOVE 'Y' TO DE-AUTO-REPRINT-FLAG.                   ELELCCML
01288                                                                   ELELCCML
01289      IF CA-CHANGE                                                 ELELCCML
01290         IF REWRITE-SW = 'Y'                                       ELELCCML
01291 *--> CODE WU = REWRITE      *****                                 ELELCCML
01292           MOVE 'WU '            TO ELCIO-FILE-ACCESS-CODE         ELELCCML
01293           MOVE DE-PRIMARY-KEY   TO ELCIO-VSAM-KEY                 ELELCCML
01294           MOVE ELPDE-LENGTH     TO ELCIO-RECORD-LEN               ELELCCML
01295           EXEC CICS LINK                                          ELELCCML
01296                PROGRAM('ELAIOPGM')                                ELELCCML
01297                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCCML
01298                LENGTH(EL-CIA-POINTER-LEN)                         ELELCCML
01299           END-EXEC                                                ELELCCML
01300         ELSE                                                      ELELCCML
01301 *--> CODE ULK = UNLOCK      *****                                 ELELCCML
01302           MOVE 'ULK'            TO ELCIO-FILE-ACCESS-CODE         ELELCCML
01303           EXEC CICS LINK                                          ELELCCML
01304                PROGRAM('ELAIOPGM')                                ELELCCML
01305                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCCML
01306                LENGTH(EL-CIA-POINTER-LEN)                         ELELCCML
01307           END-EXEC.                                               ELELCCML
01308                                                                   ELELCCML
01309  4110-UPDATE-SUCCESS.                                             ELELCCML
01310      IF CA-ADD                                                    ELELCCML
01311          MOVE ADD-SUCCESSFUL       TO ERRM-D                      ELELCCML
01312      ELSE                                                         ELELCCML
01313          IF REWRITE-SW = 'N'                                      ELELCCML
01314             MOVE NO-CHANGES-MADE   TO ERRM-D                      ELELCCML
01315          ELSE                                                     ELELCCML
01316             MOVE UPDATE-SUCCESSFUL TO ERRM-D.                     ELELCCML
01317                                                                   ELELCCML
01318      MOVE SPACE           TO FCN-D                                ELELCCML
01319                              CA-CURRENT-FUNCTION.                 ELELCCML
01320      MOVE -1              TO FCN-L.                               ELELCCML
01321      MOVE DFHBMUNP        TO FCN-A.                               ELELCCML
01322      GO TO 4110-END.                                              ELELCCML
01323                                                                   ELELCCML
01324  4110-DUPREC-ERR.                                                 ELELCCML
01325      MOVE DE-ON-FILE      TO ERRM-D.                              ELELCCML
01326      MOVE DFHBMUBF        TO DE-NAME-A.                           ELELCCML
01327      MOVE -1              TO DE-NAME-L.                           ELELCCML
01328      GO TO 4110-END.                                              ELELCCML
01329                                                                   ELELCCML
01330  4110-NOTFND-ERR.                                                 ELELCCML
01331      MOVE DE-NOTFND       TO ERRM-D.                              ELELCCML
01332      MOVE DFHBMUBF        TO DE-NAME-A.                           ELELCCML
01333      MOVE -1              TO DE-NAME-L.                           ELELCCML
01334                                                                   ELELCCML
01335  4110-END.    EXIT.                                               ELELCCML
01336 /                                                                 ELELCCML
01337  4200-CHECK-DESCRIPTION.                                          ELELCCML
01338 *    *-----------------------------------------------------------*ELELCCML
01339 *    * CHECK DESCRIPTION                                         *ELELCCML
01340 *    *-----------------------------------------------------------*ELELCCML
01341      MOVE '4200'      TO WS-PARA-ID.                              ELELCCML
01342                                                                   ELELCCML
01343      ADD 1 TO SCREEN-CTR.                                         ELELCCML
01344      IF SCREEN-CTR > MAX-DES-LINES                                ELELCCML
01345          GO TO 4200-END.                                          ELELCCML
01346                                                                   ELELCCML
01347      IF DE-DESCRIPTION-A (SCREEN-CTR) = DFHBMEOF                  ELELCCML
01348          MOVE SPACES TO DE-DESC-LINE (SCREEN-CTR)                 ELELCCML
01349          MOVE 'Y'    TO REWRITE-SW                                ELELCCML
01350          GO TO 4200-END.                                          ELELCCML
01351                                                                   ELELCCML
01352      IF DE-DESCRIPTION-D (SCREEN-CTR) = LOW-VALUES                ELELCCML
01353          GO TO 4200-END.                                          ELELCCML
01354                                                                   ELELCCML
01355      IF DE-DESC-LINE (SCREEN-CTR) NOT =                           ELELCCML
01356            DE-DESCRIPTION-D (SCREEN-CTR)                          ELELCCML
01357               MOVE 'Y' TO REWRITE-SW                              ELELCCML
01358               MOVE DE-DESCRIPTION-D (SCREEN-CTR) TO               ELELCCML
01359               DE-DESC-LINE (SCREEN-CTR).                          ELELCCML
01360                                                                   ELELCCML
01361  4200-END.    EXIT.                                               ELELCCML
01362                                                                   ELELCCML
01363  4250-DETER-LINE-CTR.                                             ELELCCML
01364      MOVE '4250'      TO WS-PARA-ID.                              ELELCCML
01365                                                                   ELELCCML
01366      PERFORM 4275-SEARCH-FOR-NONBLANK THRU                        ELELCCML
01367              4275-EXIT                                            ELELCCML
01368        VARYING SCREEN-CTR FROM MAX-DES-LINES BY -1                ELELCCML
01369          UNTIL SCREEN-CTR = 1 OR                                  ELELCCML
01370            DE-DESCRIPTION-D (SCREEN-CTR) NOT = SPACES             ELELCCML
01371                 AND NOT = LOW-VALUES.                             ELELCCML
01372      MOVE SCREEN-CTR TO DE-NBR-DESC-LINES.                        ELELCCML
01373  4250-END.    EXIT.                                               ELELCCML
01374                                                                   ELELCCML
01375  4275-SEARCH-FOR-NONBLANK.                                        ELELCCML
01376       CONTINUE.                                                   ELELCCML
01377  4275-EXIT.   EXIT.                                               ELELCCML
01378 /                                                                 ELELCCML
01379  4300-VALIDATE-MAP.                                               ELELCCML
01380 *    *-----------------------------------------------------------*ELELCCML
01381 *    * VALIDATE MAP                                              *ELELCCML
01382 *    *-----------------------------------------------------------*ELELCCML
01383      MOVE '4300'      TO WS-PARA-ID.                              ELELCCML
01384                                                                   ELELCCML
01385 **** CHECK FOR \
01386      IF DE-MAP-NAME-L NOT > +0                                    ELELCCML
01387           AND DE-MAP-PREFIX-L NOT > +0                            ELELCCML
01388              MOVE KEY-NOT-ENTERED TO ERRM-D                       ELELCCML
01389              MOVE -1              TO DE-MAP-PREFIX-L              ELELCCML
01390              GO TO 4300-END.                                      ELELCCML
01391                                                                   ELELCCML
01392      IF DE-MAP-PREFIX-L > +0                                      ELELCCML
01393          MOVE DE-MAP-PREFIX-D TO PREFIX-SEARCH                    ELELCCML
01394          MOVE DFHBMUNF TO DE-MAP-PREFIX-A                         ELELCCML
01395      ELSE                                                         ELELCCML
01396          MOVE CA-SEL-RECORD-PREFIX TO PREFIX-SEARCH.              ELELCCML
01397                                                                   ELELCCML
01398      IF DE-MAP-NAME-L > +0                                        ELELCCML
01399         MOVE DE-MAP-NAME-D TO NAME-SEARCH                         ELELCCML
01400      ELSE                                                         ELELCCML
01401         MOVE MAP-NAME-REQ TO ERRM-D                               ELELCCML
01402         MOVE -1           TO DE-MAP-NAME-L                        ELELCCML
01403         GO TO 4300-END.                                           ELELCCML
01404                                                                   ELELCCML
01405      IF DE-NAME-L > +0                                            ELELCCML
01406          IF DE-MAP-NAME-L > +0                                    ELELCCML
01407            IF DE-NAME-D = DE-MAP-NAME-D                           ELELCCML
01408              IF PREFIX-SEARCH = CA-SEL-RECORD-PREFIX              ELELCCML
01409                  MOVE CANNOT-MAP-TO-SELF TO ERRM-D                ELELCCML
01410                  MOVE -1                 TO DE-NAME-L             ELELCCML
01411                  GO TO 4300-END.                                  ELELCCML
01412                                                                   ELELCCML
01413      MOVE SPACES TO COBOL-NAME-SEARCH.                            ELELCCML
01414      PERFORM 5000-NAME-SEARCH  THRU 5000-END.                     ELELCCML
01415                                                                   ELELCCML
01416      IF FOUND-SW = 'N'                                            ELELCCML
01417          MOVE FROM-REC-NOTFND TO ERRM-D                           ELELCCML
01418          MOVE -1              TO DE-MAP-PREFIX-L                  ELELCCML
01419          GO TO 4300-END.                                          ELELCCML
01420                                                                   ELELCCML
01421      IF DELETE-SW = 'Y'                                           ELELCCML
01422          MOVE MAP-NOT-ALLOW-FROM-DELETE TO ERRM-D                 ELELCCML
01423          MOVE -1                        TO DE-MAP-PREFIX-L        ELELCCML
01424          GO TO 4300-END.                                          ELELCCML
01425                                                                   ELELCCML
01426      MOVE DE-ELEMENT-NBR      TO FROM-NBR,                        ELELCCML
01427                                  CA-MF-ELEMENT-NBR.               ELELCCML
01428      MOVE DE-RECORD-PREFIX    TO FROM-PREFIX,                     ELELCCML
01429                                  CA-MF-RECORD-PREFIX.             ELELCCML
01430 **** CHECK FOR OVERLAY ****                                       ELELCCML
01431      IF DE-NAME-L > +0                                            ELELCCML
01432          IF DE-NAME-D = CA-SEL-ELEMENT-NAME                       ELELCCML
01433              IF CA-SUPERVISORY                                    ELELCCML
01434                  MOVE MAP-OVERLAY-MSG TO ERRM-D                   ELELCCML
01435                  MOVE DFHBMPRF        TO DE-NAME-A                ELELCCML
01436                  MOVE -1              TO FCN-L                    ELELCCML
01437                  GO TO 4300-END                                   ELELCCML
01438              ELSE                                                 ELELCCML
01439                MOVE UNAUTHOR-OVERLAY TO ERRM-D                    ELELCCML
01440                MOVE DFHBMUNF         TO DE-NAME-A                 ELELCCML
01441                MOVE -1               TO DE-NAME-L                 ELELCCML
01442                GO TO 4300-END.                                    ELELCCML
01443                                                                   ELELCCML
01444 **** CHECK FOR NAME CHANGE ON NON BLANK SCREEN ****               ELELCCML
01445      IF DE-NAME-L > +0                                            ELELCCML
01446         IF CA-SEL-ELEMENT-NAME NOT = SPACES                       ELELCCML
01447             IF DE-NAME-D NOT = CA-SEL-ELEMENT-NAME                ELELCCML
01448                 MOVE NAME-CHG-NOTALLOW-MAP TO ERRM-D              ELELCCML
01449                 MOVE -1                    TO DE-NAME-L           ELELCCML
01450                 MOVE DFHBMUNF              TO DE-NAME-A           ELELCCML
01451                 GO TO 4300-END.                                   ELELCCML
01452                                                                   ELELCCML
01453 **** CHECK FOR NO OVERLAY WHEN NEW NAME IS ENTERED ****           ELELCCML
01454      MOVE CA-SEL-RECORD-PREFIX TO PREFIX-SEARCH.                  ELELCCML
01455      IF DE-NAME-L > +0                                            ELELCCML
01456          MOVE DE-NAME-D TO NAME-SEARCH                            ELELCCML
01457      ELSE                                                         ELELCCML
01458          MOVE DE-MAP-NAME-D TO NAME-SEARCH.                       ELELCCML
01459                                                                   ELELCCML
01460      PERFORM 5000-NAME-SEARCH    THRU 5000-END.                   ELELCCML
01461                                                                   ELELCCML
01462      IF FOUND-SW = 'Y'                                            ELELCCML
01463          MOVE INVALID-MAP-FUNC TO ERRM-D                          ELELCCML
01464          MOVE -1               TO DE-NAME-L                       ELELCCML
01465          MOVE DFHBMUNF         TO DE-NAME-A                       ELELCCML
01466          GO TO 4300-END.                                          ELELCCML
01467                                                                   ELELCCML
01468 ****          PERFORM THE MAP FUNCTION          ****              ELELCCML
01469                                                                   ELELCCML
01470      PERFORM 4500-FIND-NEW-INSERT-NBR THRU 4500-END.              ELELCCML
01471      IF ERROR-SW = 'Y'                                            ELELCCML
01472          GO TO 4300-END.                                          ELELCCML
01473                                                                   ELELCCML
01474      IF NEW-NBR-SW = 'N'                                          ELELCCML
01475          MOVE UNABLE-TO-ASSIGN-NUM TO ERRM-D                      ELELCCML
01476          MOVE -1                   TO DE-NAME-L                   ELELCCML
01477          GO TO 4300-END.                                          ELELCCML
01478                                                                   ELELCCML
01479      PERFORM 6500-READ-WRITE-DE  THRU 6500-END.                   ELELCCML
01480      IF ERROR-SW = 'Y'                                            ELELCCML
01481          GO TO 4300-END.                                          ELELCCML
01482                                                                   ELELCCML
01483      MOVE DE-ELEMENT-NBR TO CA-SEL-ELEMENT-NBR.                   ELELCCML
01484      PERFORM 6600-CODE-VALUE   THRU  6600-END.                    ELELCCML
01485      PERFORM 3300-LOAD-SCREEN  THRU  3300-END.                    ELELCCML
01486      MOVE SPACES TO FCN-D,                                        ELELCCML
01487                     DE-MAP-NAME-D,                                ELELCCML
01488                     DE-MAP-PREFIX-D.                              ELELCCML
01489      MOVE MAP-SUCCESSFUL TO ERRM-D.                               ELELCCML
01490  4300-END.    EXIT.                                               ELELCCML
01491 /                                                                 ELELCCML
01492  4400-PROCESS-NAME-CHANGE.                                        ELELCCML
01493 *    *-----------------------------------------------------------*ELELCCML
01494 *    * PROCESS NAME CHANGE                                       *ELELCCML
01495 *    *-----------------------------------------------------------*ELELCCML
01496      MOVE '4400'      TO WS-PARA-ID.                              ELELCCML
01497                                                                   ELELCCML
01498      MOVE DE-NAME-D TO NAME-SEARCH.                               ELELCCML
01499      IF DE-COBOL-NAME-A = DFHBMEOF                                ELELCCML
01500          MOVE SPACES TO DE-COBOL-NAME-D                           ELELCCML
01501          MOVE 30     TO DE-COBOL-NAME-L.                          ELELCCML
01502                                                                   ELELCCML
01503      IF DE-COBOL-NAME-L > +0                                      ELELCCML
01504         MOVE DE-COBOL-NAME-D TO COBOL-NAME-SEARCH                 ELELCCML
01505      ELSE                                                         ELELCCML
01506         MOVE DE-COBOL-NAME TO COBOL-NAME-SEARCH.                  ELELCCML
01507                                                                   ELELCCML
01508      MOVE CA-SEL-RECORD-PREFIX TO PREFIX-SEARCH,                  ELELCCML
01509                                   FROM-PREFIX.                    ELELCCML
01510      MOVE CA-SEL-ELEMENT-NBR   TO FROM-NBR.                       ELELCCML
01511      PERFORM 5000-NAME-SEARCH  THRU  5000-END.                    ELELCCML
01512                                                                   ELELCCML
01513      IF FOUND-SW = 'Y'                                            ELELCCML
01514        AND (DE-NAME-L > +0 AND DE-NAME-D NOT = DE-ELEMENT-NAME)   ELELCCML
01515          MOVE USE-MAP-FUNCTION TO ERRM-D                          ELELCCML
01516          MOVE -1               TO FCN-L                           ELELCCML
01517          GO TO 4400-END.                                          ELELCCML
01518                                                                   ELELCCML
01519      IF FOUND-COBOL-NAME = 'Y'                                    ELELCCML
01520        IF DE-COBOL-NAME-L > +0                                    ELELCCML
01521           AND DE-COBOL-NAME-D NOT = DE-COBOL-NAME                 ELELCCML
01522          MOVE COBOL-NAME-EXIST TO ERRM-D                          ELELCCML
01523          MOVE -1               TO DE-COBOL-NAME-L                 ELELCCML
01524          GO TO 4400-END.                                          ELELCCML
01525                                                                   ELELCCML
01526      IF DE-NAME-D NOT = DE-ELEMENT-NAME                           ELELCCML
01527          NEXT SENTENCE                                            ELELCCML
01528      ELSE                                                         ELELCCML
01529       IF DE-COBOL-NAME-D NOT = DE-COBOL-NAME                      ELELCCML
01530         AND DE-COBOL-NAME-L > +0                                  ELELCCML
01531                     GO TO 4400-READ-FOR-UPDATE.                   ELELCCML
01532                                                                   ELELCCML
01533      PERFORM 4500-FIND-NEW-INSERT-NBR THRU 4500-END.              ELELCCML
01534                                                                   ELELCCML
01535      IF ERROR-SW = 'Y'                                            ELELCCML
01536           MOVE -1 TO DE-NAME-L                                    ELELCCML
01537           GO TO 4400-END.                                         ELELCCML
01538                                                                   ELELCCML
01539      IF CA-CHANGE                                                 ELELCCML
01540           NEXT SENTENCE                                           ELELCCML
01541      ELSE                                                         ELELCCML
01542        GO TO 4400-END.                                            ELELCCML
01543                                                                   ELELCCML
01544      IF NEW-NBR-SW = 'Y'                                          ELELCCML
01545         PERFORM 6500-READ-WRITE-DE  THRU                          ELELCCML
01546                 6500-END                                          ELELCCML
01547         IF ERROR-SW = 'Y'                                         ELELCCML
01548             GO TO 4400-END                                        ELELCCML
01549         ELSE                                                      ELELCCML
01550           PERFORM 6600-CODE-VALUE  THRU 6600-END.                 ELELCCML
01551                                                                   ELELCCML
01552  4400-READ-FOR-UPDATE.                                            ELELCCML
01553      MOVE 'Y'                  TO REWRITE-SW.                     ELELCCML
01554      MOVE CA-SEL-RECORD-PREFIX TO DE-RECORD-PREFIX.               ELELCCML
01555      MOVE CA-SEL-ELEMENT-NBR   TO DE-ELEMENT-NBR.                 ELELCCML
01556                                                                   ELELCCML
01557 *--> CODE RU = READ DIRECT  *****                                 ELELCCML
01558      MOVE 'RU '             TO ELCIO-FILE-ACCESS-CODE.            ELELCCML
01559      MOVE DE-PRIMARY-KEY    TO ELCIO-VSAM-KEY.                    ELELCCML
01560      MOVE EL-DSN-ELPDE      TO ELCIO-FILE-DDNAME,                 ELELCCML
01561                                CIA-IO-GETMAIN-DDNAME.             ELELCCML
01562      MOVE 'M'               TO ELCIO-STORAGE.                     ELELCCML
01563      SET  CIA-IO-PARM-AREA-PNTR   TO   CIA-ELPDE-IOPARM-AREA-PNTR.ELELCCML
01564      SET  ELCIO-REC-AREA-ADDRESS  TO   CIA-ELPDE-REC-AREA-PNTR.   ELELCCML
01565      EXEC CICS LINK                                               ELELCCML
01566           PROGRAM('ELAIOPGM')                                     ELELCCML
01567           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCCML
01568           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
01569      END-EXEC.                                                    ELELCCML
01570                                                                   ELELCCML
01571      IF ELCIO-REC-NOT-FOUND                                       ELELCCML
01572         OR NOT ELCIO-GOOD-RETURN                                  ELELCCML
01573           GO TO 4400-NOTFND-ERR.                                  ELELCCML
01574                                                                   ELELCCML
01575      MOVE DE-NBR-DESC-LINES TO DE-NBR-DESC-LINES.                 ELELCCML
01576      IF NEW-NBR-SW = 'Y'                                          ELELCCML
01577          MOVE 'D'             TO DE-DELETE-ELEMENT-FLAG           ELELCCML
01578          MOVE NEW-DE-NBR      TO CA-SEL-ELEMENT-NBR               ELELCCML
01579          MOVE DE-NAME-D       TO CA-SEL-ELEMENT-NAME              ELELCCML
01580          MOVE DE-COBOL-NAME   TO HOLD-COBOL-NAME                  ELELCCML
01581          MOVE SPACES          TO DE-COBOL-NAME                    ELELCCML
01582          MOVE 'E'             TO DE-AUTO-REPRINT-FLAG             ELELCCML
01583      ELSE                                                         ELELCCML
01584          IF DE-NAME-D IS NOT EQUAL    TO DE-ELEMENT-NAME          ELELCCML
01585             MOVE DE-NAME-D            TO DE-ELEMENT-NAME          ELELCCML
01586             MOVE 'E'                  TO DE-AUTO-REPRINT-FLAG     ELELCCML
01587             IF DE-COBOL-NAME-D NOT = DE-COBOL-NAME                ELELCCML
01588                MOVE DE-COBOL-NAME-D   TO DE-COBOL-NAME            ELELCCML
01589              ELSE                                                 ELELCCML
01590                  NEXT SENTENCE                                    ELELCCML
01591          ELSE                                                     ELELCCML
01592              IF DE-COBOL-NAME-D NOT = DE-COBOL-NAME               ELELCCML
01593                 MOVE DE-COBOL-NAME-D  TO DE-COBOL-NAME            ELELCCML
01594              ELSE                                                 ELELCCML
01595                  NEXT SENTENCE.                                   ELELCCML
01596                                                                   ELELCCML
01597 *** REROUTE THE LOGIC FOR THE CHANGE FUNCTION IN CASE OTHER***    ELELCCML
01598 *** FIELDS IN THE RECORD NEED CHANGING.   NAC 01/98        ***    ELELCCML
01599                                                                   ELELCCML
01600      IF CA-CHANGE                                                 ELELCCML
01601          GO TO 4400-END.                                          ELELCCML
01602                                                                   ELELCCML
01603 *--> CODE WU = REWRITE                                            ELELCCML
01604      MOVE 'WU '            TO ELCIO-FILE-ACCESS-CODE.             ELELCCML
01605      EXEC CICS LINK                                               ELELCCML
01606           PROGRAM('ELAIOPGM')                                     ELELCCML
01607           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCCML
01608           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
01609      END-EXEC.                                                    ELELCCML
01610                                                                   ELELCCML
01611      IF NOT ELCIO-GOOD-RETURN                                     ELELCCML
01612             GO TO 4400-NOTFND-ERR.                                ELELCCML
01613                                                                   ELELCCML
01614      IF NEW-NBR-SW = 'Y'                                          ELELCCML
01615            MOVE NEW-DE-NBR TO DE-ELEMENT-NBR                      ELELCCML
01616            PERFORM 3310-FORMAT-ELEMENT-NBR   THRU                 ELELCCML
01617                    3310-END                                       ELELCCML
01618            PERFORM 4410-COMPLETE-NEW-NBR-CHANGE  THRU             ELELCCML
01619                    4410-END                                       ELELCCML
01620            IF ERROR-SW = 'Y'                                      ELELCCML
01621                 GO TO 4400-END.                                   ELELCCML
01622                                                                   ELELCCML
01623      MOVE DFHBMUNF          TO FCN-A.                             ELELCCML
01624      MOVE UPDATE-SUCCESSFUL TO ERRM-D.                            ELELCCML
01625      MOVE -1                TO FCN-L.                             ELELCCML
01626      MOVE SPACE             TO CA-CURRENT-FUNCTION.               ELELCCML
01627      GO TO 4400-END.                                              ELELCCML
01628                                                                   ELELCCML
01629  4400-NOTFND-ERR.                                                 ELELCCML
01630      MOVE DE-NOTFND       TO ERRM-D.                              ELELCCML
01631      MOVE -1              TO FCN-L.                               ELELCCML
01632  4400-END.     EXIT.                                              ELELCCML
01633 /                                                                 ELELCCML
01634  4410-COMPLETE-NEW-NBR-CHANGE.                                    ELELCCML
01635 ****** 3/14/86                                                    ELELCCML
01636 ******  NEW CODE FOR ELCDIOPM THIS CODE NEEDED BECAUSE            ELELCCML
01637 ******  COBOL NAME IS NOW AN ALTERNATE KEY                        ELELCCML
01638      MOVE '4410'      TO WS-PARA-ID.                              ELELCCML
01639                                                                   ELELCCML
01640      MOVE 'N' TO ERROR-SW.                                        ELELCCML
01641      IF COBOL-NAME-SEARCH = HOLD-COBOL-NAME                       ELELCCML
01642         IF COBOL-NAME-SEARCH NOT = SPACES                         ELELCCML
01643              NEXT SENTENCE                                        ELELCCML
01644         ELSE                                                      ELELCCML
01645           GO TO 4410-END.                                         ELELCCML
01646                                                                   ELELCCML
01647 *--> CODE RU = READ DIRECT  *****                                 ELELCCML
01648      MOVE 'RU '            TO ELCIO-FILE-ACCESS-CODE.             ELELCCML
01649      MOVE DE-PRIMARY-KEY   TO ELCIO-VSAM-KEY.                     ELELCCML
01650      EXEC CICS LINK                                               ELELCCML
01651           PROGRAM('ELAIOPGM')                                     ELELCCML
01652           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCCML
01653           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
01654      END-EXEC.                                                    ELELCCML
01655                                                                   ELELCCML
01656      IF NOT ELCIO-GOOD-RETURN                                     ELELCCML
01657           GO TO 4410-NOTFND-ERR.                                  ELELCCML
01658                                                                   ELELCCML
01659      MOVE COBOL-NAME-SEARCH TO DE-COBOL-NAME.                     ELELCCML
01660                                                                   ELELCCML
01661 *--> CODE WU = REWRITE      *****                                 ELELCCML
01662      MOVE 'WU '            TO ELCIO-FILE-ACCESS-CODE.             ELELCCML
01663      EXEC CICS LINK                                               ELELCCML
01664           PROGRAM('ELAIOPGM')                                     ELELCCML
01665           COMMAREA(ADDRESS OF  CIA-PARMS-RECORD)                  ELELCCML
01666           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
01667      END-EXEC.                                                    ELELCCML
01668                                                                   ELELCCML
01669      IF NOT ELCIO-GOOD-RETURN                                     ELELCCML
01670             GO TO 4410-NOTFND-ERR.                                ELELCCML
01671      GO TO 4410-END.                                              ELELCCML
01672                                                                   ELELCCML
01673  4410-NOTFND-ERR.                                                 ELELCCML
01674      MOVE COBOLNAME-NEWNBR-FAIL TO ERRM-D.                        ELELCCML
01675      MOVE -1                    TO DE-COBOL-NAME-D.               ELELCCML
01676      MOVE 'Y'                   TO ERROR-SW.                      ELELCCML
01677  4410-END.   EXIT.                                                ELELCCML
01678 /                                                                 ELELCCML
01679  4500-FIND-NEW-INSERT-NBR.                                        ELELCCML
01680 *    *-----------------------------------------------------------*ELELCCML
01681 *    * FIND NEW INSERTED RECORD                                  *ELELCCML
01682 *    *-----------------------------------------------------------*ELELCCML
01683      MOVE '4500'      TO WS-PARA-ID.                              ELELCCML
01684                                                                   ELELCCML
01685      MOVE ZEROS TO SAVE-DE-NBR-PREV,                              ELELCCML
01686                    SAVE-DE-NBR-NEXT.                              ELELCCML
01687      MOVE NAME-SEARCH   TO DE-ELEMENT-NAME.                       ELELCCML
01688      MOVE PREFIX-SEARCH TO DE-RECORD-PREFIX-N.                    ELELCCML
01689                                                                   ELELCCML
01690 *--> CODE SB = START BROWSE *****                                 ELELCCML
01691      MOVE 'SB '                 TO ELCIO-FILE-ACCESS-CODE.        ELELCCML
01692      MOVE 'GT '                 TO ELCIO-CIO-QUAL.                ELELCCML
01693      MOVE DE-SECONDARY-NAME-KEY TO ELCIO-VSAM-KEY.                ELELCCML
01694      MOVE EL-DSN-ELPDE          TO ELCIO-FILE-DDNAME,             ELELCCML
01695                                    CIA-IO-GETMAIN-DDNAME.         ELELCCML
01696      MOVE EL-DSN-ELPEN          TO ELCIO-ALT-INDEX-FILE-DDNAME.   ELELCCML
01697      MOVE 'M'                   TO ELCIO-STORAGE.                 ELELCCML
01698                                                                   ELELCCML
01699      SET  CIA-IO-PARM-AREA-PNTR   TO   CIA-ELPDE-IOPARM-AREA-PNTR.ELELCCML
01700      SET  ELCIO-REC-AREA-ADDRESS  TO   CIA-ELPDE-REC-AREA-PNTR.   ELELCCML
01701      EXEC CICS LINK                                               ELELCCML
01702           PROGRAM('ELAIOPGM')                                     ELELCCML
01703           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCCML
01704           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
01705      END-EXEC.                                                    ELELCCML
01706                                                                   ELELCCML
01707 **** WHEN NOT GOOO CONDITION IS RECEIVED, IT IS ASSUMED THAT      ELELCCML
01708 ****  A NOT FOUND CONDITION OR END OF FILE HAS BEEN ENCOUNTERED   ELELCCML
01709      IF NOT ELCIO-GOOD-RETURN                                     ELELCCML
01710           GO TO 4500-LAST-ELEMENT-NOSTR-BR.                       ELELCCML
01711                                                                   ELELCCML
01712      MOVE DE-NBR-DESC-LINES TO DE-NBR-DESC-LINES.                 ELELCCML
01713      IF SAVE-OLD-DE-NAME NOT = SPACES                             ELELCCML
01714           IF SAVE-OLD-DE-NAME = DE-ELEMENT-NAME                   ELELCCML
01715               GO TO 4500-END.                                     ELELCCML
01716                                                                   ELELCCML
01717      IF DE-RECORD-PREFIX = CA-SEL-RECORD-PREFIX                   ELELCCML
01718           MOVE DE-ELEMENT-NBR TO SAVE-DE-NBR-NEXT.                ELELCCML
01719                                                                   ELELCCML
01720  4500-READ-PREV.                                                  ELELCCML
01721                                                                   ELELCCML
01722 *--> CODE SBP = START BROWSE PREVIOUS ***                         ELELCCML
01723      MOVE 'SBP'                 TO ELCIO-FILE-ACCESS-CODE.        ELELCCML
01724      MOVE 'LT '                 TO ELCIO-CIO-QUAL.                ELELCCML
01725      MOVE DE-SECONDARY-NAME-KEY TO ELCIO-VSAM-KEY.                ELELCCML
01726      MOVE EL-DSN-ELPDE          TO ELCIO-FILE-DDNAME,             ELELCCML
01727                                    CIA-IO-GETMAIN-DDNAME.         ELELCCML
01728      MOVE EL-DSN-ELPEN          TO ELCIO-ALT-INDEX-FILE-DDNAME.   ELELCCML
01729      MOVE 'M'                   TO ELCIO-STORAGE.                 ELELCCML
01730                                                                   ELELCCML
01731      SET  CIA-IO-PARM-AREA-PNTR   TO   CIA-ELPDE-IOPARM-AREA-PNTR.ELELCCML
01732      SET  ELCIO-REC-AREA-ADDRESS  TO   CIA-ELPDE-REC-AREA-PNTR.   ELELCCML
01733      EXEC CICS LINK                                               ELELCCML
01734           PROGRAM('ELAIOPGM')                                     ELELCCML
01735           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCCML
01736           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
01737      END-EXEC.                                                    ELELCCML
01738                                                                   ELELCCML
01739      IF NOT ELCIO-GOOD-RETURN                                     ELELCCML
01740           GO TO 4500-NUMBER-CHECK.                                ELELCCML
01741                                                                   ELELCCML
01742      MOVE DE-NBR-DESC-LINES TO DE-NBR-DESC-LINES.                 ELELCCML
01743      IF SAVE-OLD-DE-NAME NOT = SPACES                             ELELCCML
01744           IF SAVE-OLD-DE-NAME = DE-ELEMENT-NAME                   ELELCCML
01745               GO TO 4500-END.                                     ELELCCML
01746                                                                   ELELCCML
01747      IF DE-RECORD-PREFIX = CA-SEL-RECORD-PREFIX                   ELELCCML
01748           MOVE DE-ELEMENT-NBR TO SAVE-DE-NBR-PREV.                ELELCCML
01749                                                                   ELELCCML
01750  4500-NUMBER-CHECK.                                               ELELCCML
01751      IF SAVE-DE-NBR-NEXT = ZEROS                                  ELELCCML
01752           ADD +001              TO SAVE-DE-NBR-PREV               ELELCCML
01753           MOVE SAVE-DE-NBR-PREV TO NEW-DE-NBR                     ELELCCML
01754           MOVE 'Y'              TO NEW-NBR-SW                     ELELCCML
01755           GO TO 4500-END.                                         ELELCCML
01756                                                                   ELELCCML
01757      SUBTRACT SAVE-DE-NBR-PREV FROM SAVE-DE-NBR-NEXT              ELELCCML
01758                     GIVING DIFFER-ENCE.                           ELELCCML
01759      DIVIDE DIFFER-ENCE BY 2 GIVING DIVID-END ROUNDED.            ELELCCML
01760      IF DIVID-END = ZEROS                                         ELELCCML
01761        OR DIVID-END = POINT-ZERO-ONE                              ELELCCML
01762            MOVE REORGANIZATION-MSG TO ERRM-D                      ELELCCML
01763            MOVE -1                 TO FCN-L                       ELELCCML
01764            MOVE 'Y'                TO ERROR-SW                    ELELCCML
01765            GO TO 4500-END.                                        ELELCCML
01766      ADD DIVID-END, SAVE-DE-NBR-PREV GIVING NEW-DE-NBR.           ELELCCML
01767      MOVE 'Y'       TO NEW-NBR-SW.                                ELELCCML
01768      GO TO 4500-END.                                              ELELCCML
01769                                                                   ELELCCML
01770  4500-LAST-ELEMENT-NOSTR-BR.                                      ELELCCML
01771      MOVE PREFIX-SEARCH TO DE-RECORD-PREFIX.                      ELELCCML
01772      MOVE ZEROS         TO DE-ELEMENT-NBR.                        ELELCCML
01773                                                                   ELELCCML
01774 *--> CODE SB  = START BROWSE ***                                  ELELCCML
01775      MOVE 'SB '           TO ELCIO-FILE-ACCESS-CODE.              ELELCCML
01776      MOVE 'GTE'                 TO ELCIO-CIO-QUAL.                ELELCCML
01777      MOVE DE-PRIMARY-KEY        TO ELCIO-VSAM-KEY.                ELELCCML
01778      MOVE EL-DSN-ELPDE          TO ELCIO-FILE-DDNAME,             ELELCCML
01779                                    CIA-IO-GETMAIN-DDNAME.         ELELCCML
01780      MOVE 'M'                   TO ELCIO-STORAGE.                 ELELCCML
01781      SET  CIA-IO-PARM-AREA-PNTR   TO   CIA-ELPDE-IOPARM-AREA-PNTR.ELELCCML
01782      SET  ELCIO-REC-AREA-ADDRESS  TO   CIA-ELPDE-REC-AREA-PNTR.   ELELCCML
01783      EXEC CICS LINK                                               ELELCCML
01784           PROGRAM('ELAIOPGM')                                     ELELCCML
01785           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCCML
01786           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
01787      END-EXEC.                                                    ELELCCML
01788                                                                   ELELCCML
01789      IF NOT ELCIO-GOOD-RETURN                                     ELELCCML
01790           GO TO 4500-DONE-NOSTR-BR.                               ELELCCML
01791                                                                   ELELCCML
01792      MOVE DE-NBR-DESC-LINES TO DE-NBR-DESC-LINES.                 ELELCCML
01793      IF PREFIX-SEARCH = DE-RECORD-PREFIX                          ELELCCML
01794         MOVE DE-ELEMENT-NBR TO SAVE-DE-NBR-NEXT.                  ELELCCML
01795                                                                   ELELCCML
01796  4500-LAST-ELEMENT-LOOP.                                          ELELCCML
01797 **** THIS CALL WILL AUTOMATICALLY DO A READ NEXT                  ELELCCML
01798      EXEC CICS LINK                                               ELELCCML
01799           PROGRAM('ELAIOPGM')                                     ELELCCML
01800           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCCML
01801           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
01802      END-EXEC.                                                    ELELCCML
01803                                                                   ELELCCML
01804      IF NOT ELCIO-GOOD-RETURN                                     ELELCCML
01805           GO TO 4500-DONE-NOSTR-BR.                               ELELCCML
01806                                                                   ELELCCML
01807      IF PREFIX-SEARCH = DE-RECORD-PREFIX                          ELELCCML
01808         MOVE DE-ELEMENT-NBR TO SAVE-DE-NBR-NEXT                   ELELCCML
01809      ELSE                                                         ELELCCML
01810        GO TO 4500-DONE.                                           ELELCCML
01811                                                                   ELELCCML
01812      GO TO 4500-LAST-ELEMENT-LOOP.                                ELELCCML
01813  4500-DONE.                                                       ELELCCML
01814 *--> CODE EB = END BROWSE*****                                    ELELCCML
01815      MOVE 'EB '            TO ELCIO-FILE-ACCESS-CODE.             ELELCCML
01816      EXEC CICS LINK                                               ELELCCML
01817           PROGRAM('ELAIOPGM')                                     ELELCCML
01818           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCCML
01819           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
01820      END-EXEC.                                                    ELELCCML
01821                                                                   ELELCCML
01822  4500-DONE-NOSTR-BR.                                              ELELCCML
01823      ADD 1 SAVE-DE-NBR-NEXT GIVING NEW-DE-NBR.                    ELELCCML
01824      MOVE 'Y' TO NEW-NBR-SW.                                      ELELCCML
01825  4500-END.    EXIT.                                               ELELCCML
01826 /                                                                 ELELCCML
01827  4600-LOCATE-ANOTHER-ELEMENT.                                     ELELCCML
01828      MOVE '4600'      TO WS-PARA-ID.                              ELELCCML
01829                                                                   ELELCCML
01830      MOVE DFHBMPRF TO PREF-A.                                     ELELCCML
01831      MOVE SPACES TO HOLD-EN-PREFIX                                ELELCCML
01832                     NAME-SEARCH.                                  ELELCCML
01833      MOVE PREF-D TO CA-SEL-RECORD-PREFIX                          ELELCCML
01834                     EN-RECORD-PREFIX                              ELELCCML
01835                     HOLD-EN-PREFIX.                               ELELCCML
01836      MOVE DE-NAME-D TO EN-ELEMENT-NAME                            ELELCCML
01837                        CA-SEL-ELEMENT-NAME                        ELELCCML
01838                        NAME-SEARCH.                               ELELCCML
01839                                                                   ELELCCML
01840      MOVE 'SB'             TO ELCIO-FILE-ACCESS-CODE4.            ELELCCML
01841      MOVE 'GTE'            TO ELCIO-CIO-QUAL4.                    ELELCCML
01842      MOVE ELPEN-KEYLENGTH  TO ELCIO-BROWSE-KEYLEN4.               ELELCCML
01843      MOVE EN-KEY           TO ELCIO-VSAM-KEY4.                    ELELCCML
01844      MOVE EL-ELPEN-REC-LEN TO ELCIO-MAX-REC-LEN4.                 ELELCCML
01845      MOVE EL-DSN-ELPEN TO ELCIO-FILE-DDNAME4                      ELELCCML
01846                           CIA-IO-GETMAIN-DDNAME.                  ELELCCML
01847      MOVE 'M'              TO ELCIO-STORAGE4.                     ELELCCML
01848                                                                   ELELCCML
01849      SET  CIA-IO-PARM-AREA-PNTR   TO   CIA-ELPEN-IOPARM-AREA-PNTR.ELELCCML
01850      SET  ELCIO-REC-AREA-ADDRESS4 TO                              ELELCCML
01851                                  CIA-ELPEN-REC-AREA-PNTR.         ELELCCML
01852      EXEC CICS LINK                                               ELELCCML
01853           PROGRAM('ELAIOPGM')                                     ELELCCML
01854           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCCML
01855           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
01856      END-EXEC.                                                    ELELCCML
01857                                                                   ELELCCML
01858 **************************************************************    ELELCCML
01859 **  THIS BROWSE INCLUDES BOTH A FORWARD AND A BACKWARD READ **    ELELCCML
01860 **  IN ORDER TO KEEP THE BROWSE WITHIN THE SAME PREFIX      **    ELELCCML
01861 **  AND STILL GIVE THE USER A VALUE.  THE USER MAY ENTER    **    ELELCCML
01862 **  FROM ONE CHARACTER TO A FULL KEY AND BECAUSE OF THAT    **    ELELCCML
01863 **  OFTEN THE VALUE RETURNED IS 'CLOSE' TO THE DESIRED      **    ELELCCML
01864 **  VALUE, IS THE FIRST OR THE LAST ELEMENT IN A FILE.      **    ELELCCML
01865 **   A NOT FOUND MESSAGE IS NOT APPROPRIATE BECAUSE         **    ELELCCML
01866 **   SOMETHING WILL ALWAYS BE FOUND                         **    ELELCCML
01867 **************************************************************    ELELCCML
01868                                                                   ELELCCML
01869      IF ELCIO-REC-NOT-FOUND4 OR (ELCIO-GOOD-RETURN4               ELELCCML
01870             AND EN-RECORD-PREFIX NOT = HOLD-EN-PREFIX)            ELELCCML
01871               PERFORM 4610-READ-PREV-ELPEN-RECORD THRU            ELELCCML
01872                       4610-END UNTIL                              ELELCCML
01873                EN-RECORD-PREFIX  =  HOLD-EN-PREFIX.               ELELCCML
01874                                                                   ELELCCML
01875      MOVE EN-RECORD-PREFIX    TO CA-SEL-RECORD-PREFIX.            ELELCCML
01876      MOVE EN-ELEMENT-NBR      TO CA-SEL-ELEMENT-NBR.              ELELCCML
01877      MOVE 'EB'                TO ELCIO-FILE-ACCESS-CODE4.         ELELCCML
01878      EXEC CICS LINK                                               ELELCCML
01879         PROGRAM ('ELAIOPGM')                                      ELELCCML
01880         COMMAREA (ADDRESS OF CIA-PARMS-RECORD)                    ELELCCML
01881         LENGTH (EL-CIA-POINTER-LEN)                               ELELCCML
01882      END-EXEC.                                                    ELELCCML
01883      PERFORM 3200-CREATE-SCREEN  THRU 3200-END.                   ELELCCML
01884  4600-END.    EXIT.                                               ELELCCML
01885 *                                                                 ELELCCML
01886  4610-READ-PREV-ELPEN-RECORD.                                     ELELCCML
01887      MOVE '4600'      TO WS-PARA-ID.                              ELELCCML
01888      MOVE 'RP'        TO ELCIO-FILE-ACCESS-CODE4.                 ELELCCML
01889      PERFORM 4620-SET-ELPEN-PARMS THRU                            ELELCCML
01890              4620-END.                                            ELELCCML
01891      IF ELCIO-REC-NOT-FOUND4                                      ELELCCML
01892           PERFORM 4650-READ-NEXT-ELPEN-RECORD THRU                ELELCCML
01893                   4650-END.                                       ELELCCML
01894  4610-END.    EXIT.                                               ELELCCML
01895 *                                                                 ELELCCML
01896  4620-SET-ELPEN-PARMS.                                            ELELCCML
01897      MOVE EN-RECORD-PREFIX    TO CA-SEL-RECORD-PREFIX.            ELELCCML
01898      MOVE EN-ELEMENT-NBR      TO CA-SEL-ELEMENT-NBR.              ELELCCML
01899      MOVE EN-KEY              TO ELCIO-VSAM-KEY4.                 ELELCCML
01900      MOVE EL-ELPEN-REC-LEN    TO ELCIO-BROWSE-KEYLEN4.            ELELCCML
01901      MOVE EL-DSN-ELPEN        TO ELCIO-FILE-DDNAME4               ELELCCML
01902                                  CIA-IO-GETMAIN-DDNAME.           ELELCCML
01903      MOVE 'M'                 TO ELCIO-STORAGE.                   ELELCCML
01904                                                                   ELELCCML
01905      SET  CIA-IO-PARM-AREA-PNTR   TO   CIA-ELPEN-IOPARM-AREA-PNTR.ELELCCML
01906      SET  ELCIO-REC-AREA-ADDRESS4 TO   CIA-ELPEN-REC-AREA-PNTR.   ELELCCML
01907                                                                   ELELCCML
01908      EXEC CICS LINK                                               ELELCCML
01909           PROGRAM ('ELAIOPGM')                                    ELELCCML
01910           COMMAREA (ADDRESS OF CIA-PARMS-RECORD)                  ELELCCML
01911           LENGTH (EL-CIA-POINTER-LEN)                             ELELCCML
01912      END-EXEC.                                                    ELELCCML
01913  4620-END.      EXIT.                                             ELELCCML
01914 *                                                                 ELELCCML
01915  4650-READ-NEXT-ELPEN-RECORD.                                     ELELCCML
01916      MOVE 'RN'                    TO ELCIO-FILE-ACCESS-CODE4.     ELELCCML
01917      PERFORM 4620-SET-ELPEN-PARMS THRU                            ELELCCML
01918              4620-END.                                            ELELCCML
01919  4650-END.      EXIT.                                             ELELCCML
01920 /                                                                 ELELCCML
01921  5000-NAME-SEARCH.                                                ELELCCML
01922      MOVE '5000'      TO WS-PARA-ID.                              ELELCCML
01923                                                                   ELELCCML
01924      MOVE PREFIX-SEARCH    TO DE-RECORD-PREFIX-N.                 ELELCCML
01925      MOVE NAME-SEARCH      TO DE-ELEMENT-NAME.                    ELELCCML
01926                                                                   ELELCCML
01927      MOVE 'N' TO FOUND-SW,                                        ELELCCML
01928                  DELETE-SW,                                       ELELCCML
01929                  FOUND-COBOL-NAME.                                ELELCCML
01930                                                                   ELELCCML
01931 *--> CODE RD = READ DIRECT  *****                                 ELELCCML
01932 *--> USE ALTERNATE KEY INSTEAD OF PRIMARY KEY                     ELELCCML
01933                                                                   ELELCCML
01934      MOVE 'RD '                 TO ELCIO-FILE-ACCESS-CODE.        ELELCCML
01935      MOVE DE-SECONDARY-NAME-KEY TO ELCIO-VSAM-KEY.                ELELCCML
01936      MOVE EL-ELPDE-REC-LEN      TO ELCIO-MAX-REC-LEN.             ELELCCML
01937      MOVE EL-DSN-ELPDE          TO ELCIO-FILE-DDNAME,             ELELCCML
01938                                    CIA-IO-GETMAIN-DDNAME.         ELELCCML
01939      MOVE EL-DSN-ELPEN          TO ELCIO-ALT-INDEX-FILE-DDNAME.   ELELCCML
01940      MOVE 'M'                   TO ELCIO-STORAGE.                 ELELCCML
01941                                                                   ELELCCML
01942      SET  CIA-IO-PARM-AREA-PNTR   TO   CIA-ELPDE-IOPARM-AREA-PNTR.ELELCCML
01943      SET  ELCIO-REC-AREA-ADDRESS  TO   CIA-ELPDE-REC-AREA-PNTR.   ELELCCML
01944                                                                   ELELCCML
01945      EXEC CICS LINK                                               ELELCCML
01946           PROGRAM ('ELAIOPGM')                                    ELELCCML
01947           COMMAREA (ADDRESS OF CIA-PARMS-RECORD)                  ELELCCML
01948           LENGTH (EL-CIA-POINTER-LEN)                             ELELCCML
01949      END-EXEC.                                                    ELELCCML
01950                                                                   ELELCCML
01951      IF ELCIO-REC-NOT-FOUND                                       ELELCCML
01952           GO TO 5000-CHECK-FOR-COBOL-NAME.                        ELELCCML
01953                                                                   ELELCCML
01954      MOVE DE-NBR-DESC-LINES TO DE-NBR-DESC-LINES.                 ELELCCML
01955                                                                   ELELCCML
01956      IF NOT ELCIO-GOOD-RETURN                                     ELELCCML
01957           GO TO 5000-CHECK-FOR-COBOL-NAME.                        ELELCCML
01958                                                                   ELELCCML
01959      MOVE 'Y' TO FOUND-SW.                                        ELELCCML
01960      IF DE-DELETE                                                 ELELCCML
01961          MOVE 'Y' TO DELETE-SW.                                   ELELCCML
01962 *                                                                 ELELCCML
01963  5000-CHECK-FOR-COBOL-NAME.                                       ELELCCML
01964 ******** USE COBOL NAME KEY INSTEAD OF PRIMARY KEY                ELELCCML
01965      IF COBOL-NAME-SEARCH = SPACES                                ELELCCML
01966           GO TO 5000-END.                                         ELELCCML
01967                                                                   ELELCCML
01968      MOVE PREFIX-SEARCH         TO CB-PREFIX.                     ELELCCML
01969      MOVE COBOL-NAME-SEARCH     TO CB-NAME.                       ELELCCML
01970      MOVE COBOL-NAME-KEY        TO ELCIO-VSAM-KEY.                ELELCCML
01971      MOVE EL-DSN-ELPCN          TO ELCIO-ALT-INDEX-FILE-DDNAME.   ELELCCML
01972      EXEC CICS LINK                                               ELELCCML
01973           PROGRAM('ELAIOPGM')                                     ELELCCML
01974           COMMAREA(ADDRESS OF  CIA-PARMS-RECORD)                  ELELCCML
01975           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
01976      END-EXEC.                                                    ELELCCML
01977                                                                   ELELCCML
01978      IF ELCIO-REC-NOT-FOUND                                       ELELCCML
01979           GO TO 5000-END.                                         ELELCCML
01980                                                                   ELELCCML
01981      IF NOT ELCIO-GOOD-RETURN                                     ELELCCML
01982           GO TO 5000-END.                                         ELELCCML
01983                                                                   ELELCCML
01984      MOVE 'Y' TO FOUND-COBOL-NAME.                                ELELCCML
01985      IF DE-DELETE                                                 ELELCCML
01986          MOVE 'Y' TO DELETE-SW.                                   ELELCCML
01987  5000-END.    EXIT.                                               ELELCCML
01988 /                                                                 ELELCCML
01989  6000-CONFIRM.                                                    ELELCCML
01990      MOVE '6000'      TO WS-PARA-ID.                              ELELCCML
01991                                                                   ELELCCML
01992      MOVE FCN-D TO CA-CURRENT-FUNCTION.                           ELELCCML
01993      IF CA-DELETE OR                                              ELELCCML
01994           CA-MAP-FROM                                             ELELCCML
01995                NEXT SENTENCE                                      ELELCCML
01996      ELSE                                                         ELELCCML
01997        MOVE -1 TO FCN-L                                           ELELCCML
01998        MOVE INVALID-USE-PF6 TO ERRM-D                             ELELCCML
01999        PERFORM 9050-SEND-SCREEN  THRU 9050-END                    ELELCCML
02000        PERFORM 9300-RETURN-ELCC  THRU 9300-END.                   ELELCCML
02001                                                                   ELELCCML
02002      IF FCN-D = 'D'                                               ELELCCML
02003        MOVE -1 TO FCN-L                                           ELELCCML
02004        PERFORM 6100-PROCESS-DELETE  THRU 6100-END                 ELELCCML
02005        PERFORM 9050-SEND-SCREEN     THRU 9050-END                 ELELCCML
02006        PERFORM 9300-RETURN-ELCC     THRU 9300-END.                ELELCCML
02007                                                                   ELELCCML
02008      IF FCN-D = 'M'                                               ELELCCML
02009        PERFORM 6200-PROCESS-MAP  THRU 6200-END                    ELELCCML
02010        PERFORM 9050-SEND-SCREEN  THRU 9050-END                    ELELCCML
02011        PERFORM 9300-RETURN-ELCC  THRU 9300-END.                   ELELCCML
02012                                                                   ELELCCML
02013  6000-END.     EXIT.                                              ELELCCML
02014 /                                                                 ELELCCML
02015  6100-PROCESS-DELETE.                                             ELELCCML
02016      MOVE '6100'      TO WS-PARA-ID.                              ELELCCML
02017                                                                   ELELCCML
02018      MOVE CA-SEL-RECORD-PREFIX    TO DE-RECORD-PREFIX.            ELELCCML
02019      MOVE CA-SEL-ELEMENT-NBR      TO DE-ELEMENT-NBR.              ELELCCML
02020                                                                   ELELCCML
02021 *--> CODE RU = READ DIRECT FOR UPDATE ***                         ELELCCML
02022      MOVE 'RU '                   TO ELCIO-FILE-ACCESS-CODE.      ELELCCML
02023      MOVE DE-PRIMARY-KEY          TO ELCIO-VSAM-KEY.              ELELCCML
02024      MOVE EL-ELPDE-REC-LEN        TO ELCIO-MAX-REC-LEN.           ELELCCML
02025      MOVE EL-DSN-ELPDE            TO ELCIO-FILE-DDNAME,           ELELCCML
02026                                      CIA-IO-GETMAIN-DDNAME.       ELELCCML
02027      MOVE 'M'                     TO ELCIO-STORAGE.               ELELCCML
02028                                                                   ELELCCML
02029      SET  CIA-IO-PARM-AREA-PNTR   TO   CIA-ELPDE-IOPARM-AREA-PNTR.ELELCCML
02030      SET  ELCIO-REC-AREA-ADDRESS  TO   CIA-ELPDE-REC-AREA-PNTR.   ELELCCML
02031      EXEC CICS LINK                                               ELELCCML
02032           PROGRAM ('ELAIOPGM')                                    ELELCCML
02033           COMMAREA (ADDRESS OF CIA-PARMS-RECORD)                  ELELCCML
02034           LENGTH (EL-CIA-POINTER-LEN)                             ELELCCML
02035      END-EXEC.                                                    ELELCCML
02036                                                                   ELELCCML
02037      IF ELCIO-REC-NOT-FOUND                                       ELELCCML
02038           GO TO 6100-NOTFND-ERR.                                  ELELCCML
02039                                                                   ELELCCML
02040      MOVE DE-NBR-DESC-LINES TO DE-NBR-DESC-LINES.                 ELELCCML
02041                                                                   ELELCCML
02042      IF (FCN-D = 'D' AND RL-DELETE)                               ELELCCML
02043          MOVE 'ULK'            TO ELCIO-FILE-ACCESS-CODE          ELELCCML
02044          EXEC CICS LINK                                           ELELCCML
02045               PROGRAM ('ELAIOPGM')                                ELELCCML
02046               COMMAREA (ADDRESS OF CIA-PARMS-RECORD)              ELELCCML
02047               LENGTH (EL-CIA-POINTER-LEN)                         ELELCCML
02048          END-EXEC                                                 ELELCCML
02049          GO TO 6100-END.                                          ELELCCML
02050                                                                   ELELCCML
02051      IF FCN-D = 'D'                                               ELELCCML
02052         MOVE 'D' TO DE-DELETE-ELEMENT-FLAG                        ELELCCML
02053         MOVE SPACE TO DE-AUTO-REPRINT-FLAG.                       ELELCCML
02054      MOVE 'WU '            TO ELCIO-FILE-ACCESS-CODE.             ELELCCML
02055      MOVE ELPDE-LENGTH     TO ELCIO-RECORD-LEN.                   ELELCCML
02056      EXEC CICS LINK                                               ELELCCML
02057           PROGRAM('ELAIOPGM')                                     ELELCCML
02058           COMMAREA(ADDRESS OF  CIA-PARMS-RECORD)                  ELELCCML
02059           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
02060      END-EXEC.                                                    ELELCCML
02061      MOVE DELETE-SUCCESSFUL TO ERRM-D.                            ELELCCML
02062      MOVE DFHBMUNP          TO FCN-A.                             ELELCCML
02063      MOVE SPACE             TO FCN-D,                             ELELCCML
02064                                CA-CURRENT-FUNCTION.               ELELCCML
02065      GO TO 6100-END.                                              ELELCCML
02066                                                                   ELELCCML
02067  6100-NOTFND-ERR.                                                 ELELCCML
02068      MOVE DE-NOTFND       TO ERRM-D.                              ELELCCML
02069  6100-END.     EXIT.                                              ELELCCML
02070 /                                                                 ELELCCML
02071  6200-PROCESS-MAP.                                                ELELCCML
02072      PERFORM 6400-MAP-WITH-OVERLAY  THRU                          ELELCCML
02073              6400-END.                                            ELELCCML
02074      MOVE '6200'      TO WS-PARA-ID.                              ELELCCML
02075                                                                   ELELCCML
02076      IF ERROR-SW = 'Y'                                            ELELCCML
02077           GO TO 6200-END.                                         ELELCCML
02078                                                                   ELELCCML
02079      PERFORM 6500-READ-WRITE-DE   THRU                            ELELCCML
02080              6500-END.                                            ELELCCML
02081                                                                   ELELCCML
02082      MOVE '6200'      TO WS-PARA-ID.                              ELELCCML
02083      IF ERROR-SW = 'Y'                                            ELELCCML
02084           GO TO 6200-END.                                         ELELCCML
02085                                                                   ELELCCML
02086      PERFORM 6600-CODE-VALUE   THRU                               ELELCCML
02087              6600-END.                                            ELELCCML
02088                                                                   ELELCCML
02089      PERFORM 3300-LOAD-SCREEN  THRU                               ELELCCML
02090              3300-END.                                            ELELCCML
02091                                                                   ELELCCML
02092      MOVE -1 TO FCN-L.                                            ELELCCML
02093      MOVE SPACES TO FCN-D,                                        ELELCCML
02094                     DE-MAP-NAME-D,                                ELELCCML
02095                     DE-MAP-PREFIX-D.                              ELELCCML
02096      MOVE MAP-SUCCESSFUL TO ERRM-D.                               ELELCCML
02097                                                                   ELELCCML
02098  6200-END.     EXIT.                                              ELELCCML
02099 /                                                                 ELELCCML
02100  6400-MAP-WITH-OVERLAY.                                           ELELCCML
02101      MOVE '6400'      TO WS-PARA-ID.                              ELELCCML
02102      IF DE-MAP-NAME-L NOT > +0 AND                                ELELCCML
02103          DE-MAP-PREFIX-L NOT > +0                                 ELELCCML
02104              MOVE KEY-NOT-ENTERED TO ERRM-D                       ELELCCML
02105              MOVE -1              TO DE-MAP-PREFIX-L              ELELCCML
02106              MOVE 'Y'             TO ERROR-SW                     ELELCCML
02107              GO TO 6400-END.                                      ELELCCML
02108                                                                   ELELCCML
02109      IF DE-MAP-PREFIX-L > +0                                      ELELCCML
02110           MOVE DE-MAP-PREFIX-D TO DE-RECORD-PREFIX-N              ELELCCML
02111      ELSE                                                         ELELCCML
02112        MOVE CA-SEL-RECORD-PREFIX TO DE-RECORD-PREFIX-N.           ELELCCML
02113      IF DE-MAP-NAME-L > +0                                        ELELCCML
02114          MOVE DE-MAP-NAME-D TO DE-ELEMENT-NAME                    ELELCCML
02115      ELSE                                                         ELELCCML
02116        MOVE MAP-NAME-REQ TO ERRM-D                                ELELCCML
02117        MOVE -1           TO DE-MAP-NAME-L                         ELELCCML
02118        MOVE 'Y'          TO ERROR-SW                              ELELCCML
02119        GO TO 6400-END.                                            ELELCCML
02120                                                                   ELELCCML
02121      IF DE-NAME-L > +0                                            ELELCCML
02122          IF DE-MAP-NAME-L > +0                                    ELELCCML
02123            IF DE-NAME-D = DE-MAP-NAME-D                           ELELCCML
02124              IF CA-SEL-RECORD-PREFIX = DE-RECORD-PREFIX-N         ELELCCML
02125                  MOVE CANNOT-MAP-TO-SELF TO ERRM-D                ELELCCML
02126                  MOVE -1                 TO DE-NAME-L             ELELCCML
02127                  MOVE 'Y'                TO ERROR-SW              ELELCCML
02128                  GO TO 6400-END.                                  ELELCCML
02129                                                                   ELELCCML
02130 *--> CODE RD = READ DIRECT  *****                                 ELELCCML
02131      MOVE 'RD '                   TO ELCIO-FILE-ACCESS-CODE.      ELELCCML
02132      MOVE DE-SECONDARY-NAME-KEY   TO ELCIO-VSAM-KEY.              ELELCCML
02133      MOVE EL-DSN-ELPDE            TO ELCIO-FILE-DDNAME,           ELELCCML
02134                                      CIA-IO-GETMAIN-DDNAME.       ELELCCML
02135      MOVE EL-DSN-ELPEN            TO ELCIO-ALT-INDEX-FILE-DDNAME. ELELCCML
02136      MOVE 'M'                     TO ELCIO-STORAGE.               ELELCCML
02137                                                                   ELELCCML
02138      SET  CIA-IO-PARM-AREA-PNTR   TO   CIA-ELPDE-IOPARM-AREA-PNTR.ELELCCML
02139      SET  ELCIO-REC-AREA-ADDRESS  TO  CIA-ELPDE-REC-AREA-PNTR.    ELELCCML
02140      EXEC CICS LINK                                               ELELCCML
02141           PROGRAM ('ELAIOPGM')                                    ELELCCML
02142           COMMAREA (ADDRESS OF CIA-PARMS-RECORD)                  ELELCCML
02143           LENGTH (EL-CIA-POINTER-LEN)                             ELELCCML
02144      END-EXEC.                                                    ELELCCML
02145                                                                   ELELCCML
02146      IF ELCIO-REC-NOT-FOUND                                       ELELCCML
02147           GO TO 6400-NOTFND-ERROR.                                ELELCCML
02148                                                                   ELELCCML
02149      MOVE DE-NBR-DESC-LINES TO  DE-NBR-DESC-LINES.                ELELCCML
02150      MOVE DE-ELEMENT-NBR    TO  FROM-NBR.                         ELELCCML
02151      MOVE DE-RECORD-PREFIX  TO  FROM-PREFIX.                      ELELCCML
02152 *                                                                 ELELCCML
02153 **********THIS IF STATEMEMT IS A CHECK TO SEE IF THE OPERATOR     ELELCCML
02154 ********** KEYED IN A NEW DATA ELEMENT AFTER THE PF6 MESSAGE      ELELCCML
02155 ********** WAS SENT AND THEN DEPRESSED THE PF6 KEY                ELELCCML
02156 *                                                                 ELELCCML
02157      IF FROM-NBR NOT = CA-MF-ELEMENT-NBR                          ELELCCML
02158         OR FROM-PREFIX NOT = CA-MF-RECORD-PREFIX                  ELELCCML
02159             PERFORM 4300-VALIDATE-MAP THRU 4300-END               ELELCCML
02160             PERFORM 9050-SEND-SCREEN  THRU 9050-END               ELELCCML
02161             PERFORM 9300-RETURN-ELCC  THRU 9300-END.              ELELCCML
02162      MOVE CA-SEL-RECORD-PREFIX TO DE-RECORD-PREFIX.               ELELCCML
02163      MOVE CA-SEL-ELEMENT-NBR   TO DE-ELEMENT-NBR.                 ELELCCML
02164                                                                   ELELCCML
02165      IF CA-SUPERVISORY                                            ELELCCML
02166           PERFORM 6800-DELETE-DE-STRUCT  THRU                     ELELCCML
02167                   6800-END                                        ELELCCML
02168           GO TO 6400-END                                          ELELCCML
02169      ELSE                                                         ELELCCML
02170       MOVE -1               TO FCN-L                              ELELCCML
02171       MOVE UNAUTHOR-OVERLAY TO ERRM-D                             ELELCCML
02172       MOVE 'Y'              TO ERROR-SW.                          ELELCCML
02173       GO TO 6400-END.                                             ELELCCML
02174                                                                   ELELCCML
02175  6400-NOTFND-ERROR.                                               ELELCCML
02176      MOVE FROM-REC-NOTFND TO ERRM-D.                              ELELCCML
02177      MOVE -1              TO DE-MAP-PREFIX-L.                     ELELCCML
02178      MOVE 'Y'             TO ERROR-SW.                            ELELCCML
02179  6400-END.    EXIT.                                               ELELCCML
02180 /                                                                 ELELCCML
02181  6500-READ-WRITE-DE.                                              ELELCCML
02182      MOVE '6500'      TO WS-PARA-ID.                              ELELCCML
02183      MOVE FROM-PREFIX       TO DE-RECORD-PREFIX.                  ELELCCML
02184      MOVE FROM-NBR          TO DE-ELEMENT-NBR.                    ELELCCML
02185                                                                   ELELCCML
02186 *--> CODE RD = READ DIRECT  *****                                 ELELCCML
02187      MOVE 'RD '             TO ELCIO-FILE-ACCESS-CODE.            ELELCCML
02188      MOVE DE-PRIMARY-KEY    TO ELCIO-VSAM-KEY.                    ELELCCML
02189      MOVE EL-DSN-ELPDE      TO ELCIO-FILE-DDNAME,                 ELELCCML
02190                                CIA-IO-GETMAIN-DDNAME.             ELELCCML
02191      MOVE 'M'               TO ELCIO-STORAGE.                     ELELCCML
02192                                                                   ELELCCML
02193      SET  CIA-IO-PARM-AREA-PNTR   TO   CIA-ELPDE-IOPARM-AREA-PNTR.ELELCCML
02194      SET  ELCIO-REC-AREA-ADDRESS  TO  CIA-ELPDE-REC-AREA-PNTR.    ELELCCML
02195      EXEC CICS LINK                                               ELELCCML
02196           PROGRAM ('ELAIOPGM')                                    ELELCCML
02197           COMMAREA (ADDRESS OF CIA-PARMS-RECORD)                  ELELCCML
02198           LENGTH (EL-CIA-POINTER-LEN)                             ELELCCML
02199      END-EXEC.                                                    ELELCCML
02200                                                                   ELELCCML
02201      IF ELCIO-REC-NOT-FOUND                                       ELELCCML
02202           GO TO 6500-NOTFND-ERR.                                  ELELCCML
02203                                                                   ELELCCML
02204      MOVE DE-NBR-DESC-LINES TO DE-NBR-DESC-LINES.                 ELELCCML
02205      MOVE 'Y'               TO DE-AUTO-REPRINT-FLAG.              ELELCCML
02206      IF NEW-NBR-SW = 'Y'                                          ELELCCML
02207           MOVE NEW-DE-NBR TO DE-ELEMENT-NBR                       ELELCCML
02208           MOVE SPACES     TO DE-COBOL-NAME.                       ELELCCML
02209                                                                   ELELCCML
02210      IF CA-CHANGE                                                 ELELCCML
02211           MOVE DE-NAME-D TO DE-ELEMENT-NAME                       ELELCCML
02212           MOVE 'E'       TO DE-AUTO-REPRINT-FLAG.                 ELELCCML
02213                                                                   ELELCCML
02214      IF CA-MAP-FROM                                               ELELCCML
02215             MOVE CA-SEL-RECORD-PREFIX TO DE-RECORD-PREFIX-N,      ELELCCML
02216                                          DE-RECORD-PREFIX         ELELCCML
02217             MOVE SPACES               TO DE-COBOL-NAME            ELELCCML
02218             IF NEW-NBR-SW NOT = 'Y'                               ELELCCML
02219               MOVE CA-SEL-ELEMENT-NBR TO DE-ELEMENT-NBR.          ELELCCML
02220                                                                   ELELCCML
02221      IF CA-MAP-FROM                                               ELELCCML
02222           IF DE-NAME-L > +0                                       ELELCCML
02223              MOVE DE-NAME-D     TO DE-ELEMENT-NAME                ELELCCML
02224           ELSE                                                    ELELCCML
02225              MOVE DE-MAP-NAME-D TO DE-ELEMENT-NAME.               ELELCCML
02226                                                                   ELELCCML
02227 *--> CODE WR = WRITE        *****                                 ELELCCML
02228      MOVE 'WR '                 TO ELCIO-FILE-ACCESS-CODE.        ELELCCML
02229      MOVE DE-PRIMARY-KEY        TO ELCIO-VSAM-KEY.                ELELCCML
02230      EXEC CICS LINK                                               ELELCCML
02231           PROGRAM('ELAIOPGM')                                     ELELCCML
02232           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCCML
02233           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
02234      END-EXEC.                                                    ELELCCML
02235                                                                   ELELCCML
02236      IF ELCIO-GOOD-RETURN                                         ELELCCML
02237         GO TO 6500-END.                                           ELELCCML
02238                                                                   ELELCCML
02239  6500-DUPREC-ERR.                                                 ELELCCML
02240      MOVE DE-ON-FILE      TO ERRM-D.                              ELELCCML
02241      MOVE -1              TO DE-NAME-L.                           ELELCCML
02242      MOVE 'Y'             TO ERROR-SW.                            ELELCCML
02243      GO TO 6500-END.                                              ELELCCML
02244                                                                   ELELCCML
02245  6500-NOTFND-ERR.                                                 ELELCCML
02246      MOVE DE-NOTFND       TO ERRM-D.                              ELELCCML
02247      MOVE -1              TO DE-NAME-L.                           ELELCCML
02248      MOVE 'Y'             TO ERROR-SW.                            ELELCCML
02249  6500-END.    EXIT.                                               ELELCCML
02250 /                                                                 ELELCCML
02251  6600-CODE-VALUE.                                                 ELELCCML
02252      MOVE '6600'           TO WS-PARA-ID.                         ELELCCML
02253      MOVE FROM-PREFIX      TO CV-RECORD-PREFIX.                   ELELCCML
02254      MOVE FROM-NBR         TO CV-ELEMENT-NBR.                     ELELCCML
02255      MOVE LOW-VALUES       TO CV-CODE-VALUE.                      ELELCCML
02256      MOVE ZEROS            TO CV-CODE-DESC-SEQ.                   ELELCCML
02257                                                                   ELELCCML
02258 *--> CODE SB = START BROWSE *****                                 ELELCCML
02259 *-->           *****  KEY MUST BE LESS THAN CV KEY                ELELCCML
02260      MOVE 'GB '            TO ELCIO-FILE-ACCESS-CODE2.            ELELCCML
02261      MOVE 'EQ '            TO ELCIO-CIO-QUAL2.                    ELELCCML
02262      MOVE ELPDE-KEYLENGTH  TO ELCIO-BROWSE-KEYLEN2.               ELELCCML
02263      MOVE EL-ELPCV-REC-LEN TO ELCIO-MAX-REC-LEN2.                 ELELCCML
02264      MOVE CV-CODE-KEY      TO ELCIO-VSAM-KEY2.                    ELELCCML
02265      MOVE EL-DSN-ELPCV     TO ELCIO-FILE-DDNAME2,                 ELELCCML
02266                               CIA-IO-GETMAIN-DDNAME.              ELELCCML
02267      MOVE 'M'              TO ELCIO-STORAGE2.                     ELELCCML
02268                                                                   ELELCCML
02269      SET  CIA-IO-PARM-AREA-PNTR   TO   CIA-ELPCV-IOPARM-AREA-PNTR.ELELCCML
02270      SET  ELCIO-REC-AREA-ADDRESS2 TO  CIA-ELPCV-REC-AREA-PNTR.    ELELCCML
02271      EXEC CICS LINK                                               ELELCCML
02272           PROGRAM ('ELAIOPGM')                                    ELELCCML
02273           COMMAREA (ADDRESS OF CIA-PARMS-RECORD)                  ELELCCML
02274           LENGTH (EL-CIA-POINTER-LEN)                             ELELCCML
02275      END-EXEC.                                                    ELELCCML
02276                                                                   ELELCCML
02277      IF ELCIO-REC-NOT-FOUND2                                      ELELCCML
02278           GO TO 6600-END.                                         ELELCCML
02279                                                                   ELELCCML
02280      IF NOT ELCIO-GOOD-RETURN2                                    ELELCCML
02281           GO TO 6600-END.                                         ELELCCML
02282                                                                   ELELCCML
02283  6600-CHECK-NEXT.                                                 ELELCCML
02284      MOVE CV-NBR-VALUE-DESC-LINES TO CV-NBR-VALUE-DESC-LINES.     ELELCCML
02285                                                                   ELELCCML
02286      IF FROM-PREFIX NOT = CV-RECORD-PREFIX                        ELELCCML
02287        OR FROM-NBR NOT = CV-ELEMENT-NBR                           ELELCCML
02288           GO TO 6600-END.                                         ELELCCML
02289                                                                   ELELCCML
02290      MOVE CV-CODE-VALUE    TO SAVE-CODE-VALUE.                    ELELCCML
02291      MOVE CV-CODE-DESC-SEQ TO SAVE-CODE-DESC-SEQ.                 ELELCCML
02292 *--> CODE EB = END PREVIOUS*****                                  ELELCCML
02293      MOVE 'EB '            TO ELCIO-FILE-ACCESS-CODE2.            ELELCCML
02294      EXEC CICS LINK                                               ELELCCML
02295           PROGRAM('ELAIOPGM')                                     ELELCCML
02296           COMMAREA(ADDRESS OF  CIA-PARMS-RECORD)                  ELELCCML
02297           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
02298      END-EXEC.                                                    ELELCCML
02299                                                                   ELELCCML
02300      MOVE DE-RECORD-PREFIX TO CV-RECORD-PREFIX.                   ELELCCML
02301      MOVE DE-ELEMENT-NBR   TO CV-ELEMENT-NBR.                     ELELCCML
02302                                                                   ELELCCML
02303 *--> CODE WR = WRITE        *****                                 ELELCCML
02304      MOVE 'WR '            TO ELCIO-FILE-ACCESS-CODE2.            ELELCCML
02305      MOVE CV-CODE-KEY      TO ELCIO-VSAM-KEY2.                    ELELCCML
02306      EXEC CICS  LINK                                              ELELCCML
02307           PROGRAM('ELAIOPGM')                                     ELELCCML
02308           COMMAREA(ADDRESS OF  CIA-PARMS-RECORD)                  ELELCCML
02309           LENGTH(EL-CIA-POINTER-LEN)                              ELELCCML
02310      END-EXEC.                                                    ELELCCML
02311                                                                   ELELCCML
02312      MOVE FROM-PREFIX          TO CV-RECORD-PREFIX.               ELELCCML
02313      MOVE FROM-NBR             TO CV-ELEMENT-NBR.                 ELELCCML
02314      MOVE SAVE-CODE-VALUE      TO CV-CODE-VALUE.                  ELELCCML
02315      MOVE SAVE-CODE-DESC-SEQ   TO CV-CODE-DESC-SEQ.               ELELCCML
02316                                                                   ELELCCML
02317 *--> CODE SB = START BROWSE                                       ELELCCML
02318      MOVE 'SB '            TO ELCIO-FILE-ACCESS-CODE2.            ELELCCML
02319      MOVE 'EQ '            TO ELCIO-CIO-QUAL2.                    ELELCCML
02320      MOVE ELPCV-KEYLENGTH  TO ELCIO-BROWSE-KEYLEN2.               ELELCCML
02321      MOVE CV-CODE-KEY      TO ELCIO-VSAM-KEY2.                    ELELCCML
02322      MOVE EL-ELPCV-REC-LEN TO ELCIO-MAX-REC-LEN2.                 ELELCCML
02323      MOVE EL-DSN-ELPCV     TO ELCIO-FILE-DDNAME2                  ELELCCML
02324                               CIA-IO-GETMAIN-DDNAME.              ELELCCML
02325      MOVE 'M'              TO ELCIO-STORAGE2                      ELELCCML
02326                                                                   ELELCCML
02327      SET  CIA-IO-PARM-AREA-PNTR   TO   CIA-ELPCV-IOPARM-AREA-PNTR.ELELCCML
02328      SET  ELCIO-REC-AREA-ADDRESS2 TO  CIA-ELPCV-REC-AREA-PNTR.    ELELCCML
02329                                                                   ELELCCML
02330      EXEC CICS LINK                                               ELELCCML
02331           PROGRAM ('ELAIOPGM')                                    ELELCCML
02332           COMMAREA (ADDRESS OF CIA-PARMS-RECORD)                  ELELCCML
02333           LENGTH (EL-CIA-POINTER-LEN)                             ELELCCML
02334      END-EXEC.                                                    ELELCCML
02335                                                                   ELELCCML
02336      IF ELCIO-REC-NOT-FOUND2                                      ELELCCML
02337           GO TO 6600-END.                                         ELELCCML
02338                                                                   ELELCCML
02339      IF NOT ELCIO-GOOD-RETURN2                                    ELELCCML
02340           GO TO 6600-END.                                         ELELCCML
02341                                                                   ELELCCML
02342      MOVE CV-NBR-VALUE-DESC-LINES TO CV-NBR-VALUE-DESC-LINES.     ELELCCML
02343                                                                   ELELCCML
02344 *-->  THIS CALL WILL AUTOMATICALLY DO A  READ NEXT                ELELCCML
02345                                                                   ELELCCML
02346      EXEC CICS LINK                                               ELELCCML
02347           PROGRAM ('ELAIOPGM')                                    ELELCCML
02348           COMMAREA (ADDRESS OF CIA-PARMS-RECORD)                  ELELCCML
02349           LENGTH (EL-CIA-POINTER-LEN)                             ELELCCML
02350      END-EXEC.                                                    ELELCCML
02351                                                                   ELELCCML
02352      IF ELCIO-REC-NOT-FOUND2                                      ELELCCML
02353           GO TO 6600-END.                                         ELELCCML
02354                                                                   ELELCCML
02355      IF NOT ELCIO-GOOD-RETURN2                                    ELELCCML
02356           GO TO 6600-END.                                         ELELCCML
02357                                                                   ELELCCML
02358      GO TO 6600-CHECK-NEXT.                                       ELELCCML
02359  6600-END.     EXIT.                                              ELELCCML
02360                                                                   ELELCCML
02361  6800-DELETE-DE-STRUCT.                                           ELELCCML
02362                                                                   ELELCCML
02363 *--> CODE RD = READ DIRECT                                        ELELCCML
02364      MOVE 'RD '             TO ELCIO-FILE-ACCESS-CODE.            ELELCCML
02365      MOVE DE-PRIMARY-KEY    TO ELCIO-VSAM-KEY.                    ELELCCML
02366      MOVE EL-DSN-ELPDE      TO ELCIO-FILE-DDNAME,                 ELELCCML
02367                                CIA-IO-GETMAIN-DDNAME.             ELELCCML
02368      MOVE 'M'               TO ELCIO-STORAGE.                     ELELCCML
02369      SET  CIA-IO-PARM-AREA-PNTR   TO   CIA-ELPDE-IOPARM-AREA-PNTR.ELELCCML
02370      SET  ELCIO-REC-AREA-ADDRESS  TO  CIA-ELPDE-REC-AREA-PNTR.    ELELCCML
02371      EXEC CICS LINK                                               ELELCCML
02372           PROGRAM ('ELAIOPGM')                                    ELELCCML
02373           COMMAREA (ADDRESS OF CIA-PARMS-RECORD)                  ELELCCML
02374           LENGTH (EL-CIA-POINTER-LEN)                             ELELCCML
02375      END-EXEC.                                                    ELELCCML
02376                                                                   ELELCCML
02377      IF ELCIO-REC-NOT-FOUND                                       ELELCCML
02378            GO TO 6800-NOTFND-ERR.                                 ELELCCML
02379                                                                   ELELCCML
02380      MOVE DE-NBR-DESC-LINES TO DE-NBR-DESC-LINES.                 ELELCCML
02381                                                                   ELELCCML
02382                                                                   ELELCCML
02383 **** DELETE LOWER LEVEL RECORDS FIRST ****                        ELELCCML
02384      PERFORM 6900-DELETE-CODE-VALUES  THRU                        ELELCCML
02385              6900-END.                                            ELELCCML
02386                                                                   ELELCCML
02387 **** READ AND DELETE DATA ELEMENT RECORD ****                     ELELCCML
02388 *--> CODE DL = DIRECT DELETE WHEN KEY KNOW *****                  ELELCCML
02389      MOVE 'DL '             TO ELCIO-FILE-ACCESS-CODE.            ELELCCML
02390      MOVE DE-PRIMARY-KEY    TO ELCIO-VSAM-KEY.                    ELELCCML
02391      MOVE EL-DSN-ELPDE      TO ELCIO-FILE-DDNAME,                 ELELCCML
02392                                CIA-IO-GETMAIN-DDNAME.             ELELCCML
02393      MOVE 'M'               TO ELCIO-STORAGE.                     ELELCCML
02394      SET  CIA-IO-PARM-AREA-PNTR   TO   CIA-ELPDE-IOPARM-AREA-PNTR.ELELCCML
02395      SET  ELCIO-REC-AREA-ADDRESS  TO  CIA-ELPDE-REC-AREA-PNTR.    ELELCCML
02396      EXEC CICS LINK                                               ELELCCML
02397           PROGRAM ('ELAIOPGM')                                    ELELCCML
02398           COMMAREA (ADDRESS OF CIA-PARMS-RECORD)                  ELELCCML
02399           LENGTH (EL-CIA-POINTER-LEN)                             ELELCCML
02400      END-EXEC.                                                    ELELCCML
02401                                                                   ELELCCML
02402      IF NOT ELCIO-GOOD-RETURN                                     ELELCCML
02403          GO TO 6800-NOTFND-ERR.                                   ELELCCML
02404                                                                   ELELCCML
02405      GO TO 6800-END.                                              ELELCCML
02406  6800-NOTFND-ERR.                                                 ELELCCML
02407      MOVE DE-NOTFND       TO ERRM-D.                              ELELCCML
02408      MOVE 'Y'             TO ERROR-SW.                            ELELCCML
02409      MOVE -1              TO DE-NAME-L.                           ELELCCML
02410  6800-END.   EXIT.                                                ELELCCML
02411 /                                                                 ELELCCML
02412  6900-DELETE-CODE-VALUES.                                         ELELCCML
02413      MOVE '6900'               TO WS-PARA-ID.                     ELELCCML
02414      MOVE CA-SEL-RECORD-PREFIX TO CV-RECORD-PREFIX.               ELELCCML
02415      MOVE DE-ELEMENT-NBR       TO CV-ELEMENT-NBR.                 ELELCCML
02416      MOVE LOW-VALUES           TO CV-CODE-VALUE.                  ELELCCML
02417      MOVE ZEROS                TO CV-CODE-DESC-SEQ.               ELELCCML
02418                                                                   ELELCCML
02419 *--> READ AND DELETE CODE VALUES ****                             ELELCCML
02420      MOVE 'GB '            TO ELCIO-FILE-ACCESS-CODE2.            ELELCCML
02421      MOVE 'EQ '            TO ELCIO-CIO-QUAL2.                    ELELCCML
02422      MOVE ELPDE-KEYLENGTH  TO ELCIO-BROWSE-KEYLEN2.               ELELCCML
02423      MOVE CV-CODE-KEY      TO ELCIO-VSAM-KEY2.                    ELELCCML
02424      MOVE EL-ELPCV-REC-LEN TO ELCIO-MAX-REC-LEN2.                 ELELCCML
02425      MOVE EL-DSN-ELPCV     TO ELCIO-FILE-DDNAME2                  ELELCCML
02426                               CIA-IO-GETMAIN-DDNAME.              ELELCCML
02427      MOVE 'M'              TO ELCIO-STORAGE2                      ELELCCML
02428      SET  CIA-IO-PARM-AREA-PNTR   TO   CIA-ELPCV-IOPARM-AREA-PNTR.ELELCCML
02429      SET  ELCIO-REC-AREA-ADDRESS2 TO  CIA-ELPCV-REC-AREA-PNTR.    ELELCCML
02430      EXEC CICS LINK                                               ELELCCML
02431           PROGRAM ('ELAIOPGM')                                    ELELCCML
02432           COMMAREA (ADDRESS OF CIA-PARMS-RECORD)                  ELELCCML
02433           LENGTH (EL-CIA-POINTER-LEN)                             ELELCCML
02434      END-EXEC.                                                    ELELCCML
02435                                                                   ELELCCML
02436      IF ELCIO-REC-NOT-FOUND2                                      ELELCCML
02437           GO TO 6900-END.                                         ELELCCML
02438      IF NOT ELCIO-GOOD-RETURN2                                    ELELCCML
02439           GO TO 6900-END.                                         ELELCCML
02440                                                                   ELELCCML
02441      MOVE CV-NBR-VALUE-DESC-LINES TO CV-NBR-VALUE-DESC-LINES.     ELELCCML
02442                                                                   ELELCCML
02443  6900-CV-DELETE.                                                  ELELCCML
02444      MOVE 'EB '            TO ELCIO-FILE-ACCESS-CODE2.            ELELCCML
02445      EXEC CICS LINK                                               ELELCCML
02446           PROGRAM ('ELAIOPGM')                                    ELELCCML
02447           COMMAREA (ADDRESS OF CIA-PARMS-RECORD)                  ELELCCML
02448           LENGTH (EL-CIA-POINTER-LEN)                             ELELCCML
02449      END-EXEC.                                                    ELELCCML
02450                                                                   ELELCCML
02451      IF NOT ELCIO-GOOD-RETURN2                                    ELELCCML
02452           GO TO 6900-END.                                         ELELCCML
02453                                                                   ELELCCML
02454      IF CA-SEL-RECORD-PREFIX NOT = CV-RECORD-PREFIX               ELELCCML
02455         OR DE-ELEMENT-NBR NOT = CV-ELEMENT-NBR                    ELELCCML
02456           GO TO 6900-END.                                         ELELCCML
02457                                                                   ELELCCML
02458 *--> CODE DL = DIRECT DELETE WITH KEY ***                         ELELCCML
02459      MOVE 'DL '            TO ELCIO-FILE-ACCESS-CODE2.            ELELCCML
02460      MOVE CV-CODE-KEY      TO ELCIO-VSAM-KEY2.                    ELELCCML
02461      EXEC CICS LINK                                               ELELCCML
02462           PROGRAM ('ELAIOPGM')                                    ELELCCML
02463           COMMAREA (ADDRESS OF CIA-PARMS-RECORD)                  ELELCCML
02464           LENGTH (EL-CIA-POINTER-LEN)                             ELELCCML
02465      END-EXEC.                                                    ELELCCML
02466                                                                   ELELCCML
02467      IF NOT ELCIO-GOOD-RETURN2                                    ELELCCML
02468           GO TO 6900-END.                                         ELELCCML
02469                                                                   ELELCCML
02470      MOVE 'SB '            TO ELCIO-FILE-ACCESS-CODE2.            ELELCCML
02471      MOVE 'GTE'            TO ELCIO-CIO-QUAL2.                    ELELCCML
02472      EXEC CICS LINK                                               ELELCCML
02473           PROGRAM ('ELAIOPGM')                                    ELELCCML
02474           COMMAREA (ADDRESS OF CIA-PARMS-RECORD)                  ELELCCML
02475           LENGTH (EL-CIA-POINTER-LEN)                             ELELCCML
02476      END-EXEC.                                                    ELELCCML
02477                                                                   ELELCCML
02478      IF NOT ELCIO-GOOD-RETURN2                                    ELELCCML
02479           GO TO 6900-END.                                         ELELCCML
02480                                                                   ELELCCML
02481      GO TO 6900-CV-DELETE.                                        ELELCCML
02482  6900-END.   EXIT.                                                ELELCCML
02483 /                                                                 ELELCCML
02484  8000-DISP-CLR-SCREEN.                                            ELELCCML
02485      MOVE '8000'              TO WS-PARA-ID.                      ELELCCML
02486                                                                   ELELCCML
02487      IF CA-INQUIRY                                                ELELCCML
02488        MOVE UNAUTHOR-MAINTAIN TO ERRM-D                           ELELCCML
02489        MOVE -1                TO FCN-L                            ELELCCML
02490        MOVE SPACE             TO FCN-D                            ELELCCML
02491        MOVE SPACE             TO CA-CURRENT-FUNCTION              ELELCCML
02492        PERFORM 9050-SEND-SCREEN  THRU 9050-END                    ELELCCML
02493        PERFORM 9300-RETURN-ELCC  THRU 9300-END.                   ELELCCML
02494                                                                   ELELCCML
02495      PERFORM 3000-CLEAR-ATTRIBUTES  THRU 3000-END.                ELELCCML
02496      MOVE '8000'              TO WS-PARA-ID.                      ELELCCML
02497                                                                   ELELCCML
02498      MOVE WS-TRANS-ID           TO FUNC-D.                        ELELCCML
02499      MOVE -1                    TO FCN-L.                         ELELCCML
02500      MOVE 'A'                   TO FCN-D.                         ELELCCML
02501      MOVE DFHBMUNF              TO FCN-A.                         ELELCCML
02502      MOVE CA-SEL-RECORD-PREFIX  TO PREF-D.                        ELELCCML
02503      MOVE CA-SEL-RECORD-NAME    TO RL-NAME-D.                     ELELCCML
02504      MOVE SPACES                TO CA-SEL-ELEMENT-NAME.           ELELCCML
02505      PERFORM 3100-INITIAL-HEADING  THRU                           ELELCCML
02506              3100-END.                                            ELELCCML
02507      MOVE '8000'              TO WS-PARA-ID.                      ELELCCML
02508                                                                   ELELCCML
02509      EXEC CICS                                                    ELELCCML
02510          SEND                                                     ELELCCML
02511            MAP('ELCCI01')                                         ELELCCML
02512            MAPSET('ELCCSET')                                      ELELCCML
02513            ERASE                                                  ELELCCML
02514            CURSOR                                                 ELELCCML
02515      END-EXEC.                                                    ELELCCML
02516      PERFORM 9300-RETURN-ELCC  THRU 9300-END.                     ELELCCML
02517  8000-END.     EXIT.                                              ELELCCML
02518 /                                                                 ELELCCML
02519  9000-MAPFAIL.                                                    ELELCCML
02520      MOVE '9000'          TO WS-PARA-ID.                          ELELCCML
02521      MOVE MAP-FAIL        TO ERRM-D.                              ELELCCML
02522      MOVE -1              TO FCN-L.                               ELELCCML
02523      PERFORM 9050-SEND-SCREEN  THRU 9050-END.                     ELELCCML
02524  9000-END.     EXIT.                                              ELELCCML
02525 /                                                                 ELELCCML
02526  9050-SEND-SCREEN.                                                ELELCCML
02527      MOVE '9050'          TO WS-PARA-ID.                          ELELCCML
02528      IF CA-DE-DEFINE                                              ELELCCML
02529         EXEC CICS SEND                                            ELELCCML
02530              MAP('ELCCI01')                                       ELELCCML
02531              MAPSET('ELCCSET')                                    ELELCCML
02532              DATAONLY                                             ELELCCML
02533              CURSOR                                               ELELCCML
02534         END-EXEC                                                  ELELCCML
02535      ELSE                                                         ELELCCML
02536         MOVE 'C'   TO CA-CURRENT-PGM                              ELELCCML
02537         EXEC CICS SEND                                            ELELCCML
02538              MAP('ELCCI01')                                       ELELCCML
02539              MAPSET('ELCCSET')                                    ELELCCML
02540              ERASE                                                ELELCCML
02541         END-EXEC.                                                 ELELCCML
02542  9050-END.    EXIT.                                               ELELCCML
02543 /                                                                 ELELCCML
02544  9100-XCTL-ELCB.                                                  ELELCCML
02545      MOVE '9100'          TO WS-PARA-ID.                          ELELCCML
02546      IF CA-FIRST-ELEMENT = LOW-VALUES                             ELELCCML
02547        IF CA-SEL-ELEMENT-NBR-X NOT = LOW-VALUES                   ELELCCML
02548            AND CA-SEL-ELEMENT-NBR NOT = ZEROS                     ELELCCML
02549                MOVE CA-SELECTED-DE-KEY TO CA-FIRST-ELEMENT.       ELELCCML
02550      MOVE LOW-VALUES TO CA-SEL-ELEMENT-NBR-X.                     ELELCCML
02551      MOVE 'C'        TO CA-CURRENT-PGM.                           ELELCCML
02552      MOVE SPACES     TO CA-SEL-ELEMENT-NAME,                      ELELCCML
02553                         CA-CURRENT-FUNCTION.                      ELELCCML
02554      EXEC CICS                                                    ELELCCML
02555          XCTL PROGRAM('ELELCBML')                                 ELELCCML
02556               COMMAREA(DFHCOMMAREA)                               ELELCCML
02557               LENGTH(COMM-LENGTH)                                 ELELCCML
02558      END-EXEC.                                                    ELELCCML
02559  9100-END.    EXIT.                                               ELELCCML
02560 /                                                                 ELELCCML
02561  9200-XCTL-ELCD.                                                  ELELCCML
02562      MOVE '9200'          TO WS-PARA-ID.                          ELELCCML
02563      IF CA-SEL-ELEMENT-NBR-X = LOW-VALUES                         ELELCCML
02564           MOVE -1                 TO FCN-L                        ELELCCML
02565           MOVE INVALID-PF4-SEL-DE TO ERRM-D                       ELELCCML
02566           PERFORM 9050-SEND-SCREEN THRU 9050-END                  ELELCCML
02567           PERFORM 9300-RETURN-ELCC THRU 9300-END.                 ELELCCML
02568                                                                   ELELCCML
02569      IF CA-DELETE                                                 ELELCCML
02570           MOVE -1                  TO FCN-L                       ELELCCML
02571           MOVE DE-DELETED-NOUPDATE TO ERRM-D                      ELELCCML
02572           PERFORM 9050-SEND-SCREEN  THRU 9050-END                 ELELCCML
02573           PERFORM 9300-RETURN-ELCC  THRU 9300-END.                ELELCCML
02574                                                                   ELELCCML
02575      MOVE 'C'                   TO CA-CURRENT-PGM.                ELELCCML
02576      MOVE LOW-VALUES            TO CA-SEL-CODE-VALUE,             ELELCCML
02577                                    CA-SEL-CODE-SEQ-X.             ELELCCML
02578      MOVE SPACES                TO CA-SEL-CODE-NAME.              ELELCCML
02579      MOVE CA-SEL-RECORD-PREFIX  TO CV-RECORD-PREFIX.              ELELCCML
02580      MOVE CA-SEL-ELEMENT-NBR    TO CV-ELEMENT-NBR.                ELELCCML
02581      MOVE LOW-VALUES            TO CV-CODE-VALUE.                 ELELCCML
02582      MOVE ZEROS                 TO CV-CODE-DESC-SEQ.              ELELCCML
02583                                                                   ELELCCML
02584 *--> CODE GB = GENERIC BROWSE  *********************              ELELCCML
02585      MOVE 'GB '            TO ELCIO-FILE-ACCESS-CODE2.            ELELCCML
02586      MOVE 'EQ '            TO ELCIO-CIO-QUAL2.                    ELELCCML
02587      MOVE ELPDE-KEYLENGTH  TO ELCIO-BROWSE-KEYLEN2.               ELELCCML
02588      MOVE CV-CODE-KEY      TO ELCIO-VSAM-KEY2.                    ELELCCML
02589      MOVE EL-ELPCV-REC-LEN TO ELCIO-MAX-REC-LEN2.                 ELELCCML
02590      MOVE EL-DSN-ELPCV     TO ELCIO-FILE-DDNAME2,                 ELELCCML
02591                               CIA-IO-GETMAIN-DDNAME.              ELELCCML
02592      MOVE 'M'              TO ELCIO-STORAGE2.                     ELELCCML
02593      SET  CIA-IO-PARM-AREA-PNTR   TO   CIA-ELPCV-IOPARM-AREA-PNTR.ELELCCML
02594      SET  ELCIO-REC-AREA-ADDRESS2 TO  CIA-ELPCV-REC-AREA-PNTR.    ELELCCML
02595      EXEC CICS LINK                                               ELELCCML
02596           PROGRAM ('ELAIOPGM')                                    ELELCCML
02597           COMMAREA (ADDRESS OF CIA-PARMS-RECORD)                  ELELCCML
02598           LENGTH (EL-CIA-POINTER-LEN)                             ELELCCML
02599      END-EXEC.                                                    ELELCCML
02600                                                                   ELELCCML
02601      IF ELCIO-REC-NOT-FOUND2                                      ELELCCML
02602          IF CA-INQUIRY                                            ELELCCML
02603              MOVE -1              TO  FCN-L                       ELELCCML
02604              MOVE NO-CODE-VALUES  TO  ERRM-D                      ELELCCML
02605              PERFORM 9050-SEND-SCREEN  THRU 9050-END              ELELCCML
02606              PERFORM 9300-RETURN-ELCC  THRU 9300-END              ELELCCML
02607          ELSE                                                     ELELCCML
02608              GO TO 9200-XCLT-ELCE.                                ELELCCML
02609                                                                   ELELCCML
02610      IF NOT ELCIO-GOOD-RETURN2                                    ELELCCML
02611           GO TO 9200-XCLT-ELCE.                                   ELELCCML
02612 *--> CODE EB = END PREVIOUS*****                                  ELELCCML
02613      MOVE 'EB '            TO ELCIO-FILE-ACCESS-CODE2.            ELELCCML
02614      EXEC CICS LINK                                               ELELCCML
02615           PROGRAM ('ELAIOPGM')                                    ELELCCML
02616           COMMAREA (ADDRESS OF CIA-PARMS-RECORD)                  ELELCCML
02617           LENGTH (EL-CIA-POINTER-LEN)                             ELELCCML
02618      END-EXEC.                                                    ELELCCML
02619                                                                   ELELCCML
02620      IF CA-SEL-RECORD-PREFIX = CV-RECORD-PREFIX                   ELELCCML
02621         AND CA-SEL-ELEMENT-NBR = CV-ELEMENT-NBR                   ELELCCML
02622               NEXT SENTENCE                                       ELELCCML
02623      ELSE                                                         ELELCCML
02624        GO TO 9200-XCLT-ELCE.                                      ELELCCML
02625                                                                   ELELCCML
02626      EXEC CICS                                                    ELELCCML
02627          XCTL PROGRAM('ELELCDML')                                 ELELCCML
02628               COMMAREA(DFHCOMMAREA)                               ELELCCML
02629               LENGTH(COMM-LENGTH)                                 ELELCCML
02630      END-EXEC.                                                    ELELCCML
02631  9200-XCLT-ELCE.                                                  ELELCCML
02632      EXEC CICS                                                    ELELCCML
02633          XCTL PROGRAM('ELELCEML')                                 ELELCCML
02634               COMMAREA(DFHCOMMAREA)                               ELELCCML
02635               LENGTH(COMM-LENGTH)                                 ELELCCML
02636      END-EXEC.                                                    ELELCCML
02637  9200-END.    EXIT.                                               ELELCCML
02638                                                                   ELELCCML
02639  9300-RETURN-ELCC.                                                ELELCCML
02640      EXEC CICS                                                    ELELCCML
02641           RETURN                                                  ELELCCML
02642             TRANSID('ELCC')                                       ELELCCML
02643             COMMAREA(DFHCOMMAREA)                                 ELELCCML
02644             LENGTH(COMM-LENGTH)                                   ELELCCML
02645      END-EXEC.                                                    ELELCCML
02646  9300-END.     EXIT.                                              ELELCCML
02647                                                                   ELELCCML
02648  9400-XCTL-ELCA.                                                  ELELCCML
02649      MOVE LOW-VALUES TO CA-SELECTED-KEYS.                         ELELCCML
02650      MOVE 'C'        TO CA-CURRENT-PGM.                           ELELCCML
02651      MOVE SPACES     TO CA-SEL-ELEMENT-NAME,                      ELELCCML
02652                         CA-CURRENT-FUNCTION.                      ELELCCML
02653      EXEC CICS XCTL                                               ELELCCML
02654           PROGRAM('ELELCAML')                                     ELELCCML
02655           COMMAREA(DFHCOMMAREA)                                   ELELCCML
02656           LENGTH(COMM-LENGTH)                                     ELELCCML
02657      END-EXEC.                                                    ELELCCML
02658  9400-END.     EXIT.                                              ELELCCML
02659 /                                                                 ELELCCML
02660  9999-ABEND.                                                      ELELCCML
02661      EXEC CICS ABEND                                              ELELCCML
02662                ABCODE(WS-ABEND-CODE)                              ELELCCML
02663      END-EXEC.                                                    ELELCCML
02664                                                                   ELELCCML
02665  9900-RETURN-CICS.                                                ELELCCML
02666      EXEC CICS                                                    ELELCCML
02667          RETURN                                                   ELELCCML
02668      END-EXEC.                                                    ELELCCML
02669      GOBACK.                                                      ELELCCML
02670  9900-END.     EXIT.                                              ELELCCML
