00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID.  ELP004.                                             ELP004  
00003  AUTHOR.   JOHN CURIN  --- KEANE,INC.                                LV001
00004  INSTALLATION.   HEALTH CARE SERVICE CORPORATION.                 ELP004  
00005  DATE-WRITTEN.   SEPTEMBER, 1985.                                 ELP004  
00006  DATE-COMPILED.                                                   ELP004  
00007  EJECT                                                            ELP004  
00008  REMARKS. THIS IS A ONE TIME PROGRAM THAT READS A WORK FILE       ELP004  
00009           THAT IS CREATED FROM THE DATA DICTIONARY.               ELP004  
00010           THE PROGRAM EXTRACTS THE INFORMATION NEED FOR THE       ELP004  
00011           CODES MANUAL DATA DICTIONARY WHICH BE USED TO SUPPORT   ELP004  
00012           THE ENGLISH LANGUAGE PROTYPE SYSTEM.                    ELP004  
00013                                                                   ELP004  
00014           A SORT KEY IS ALSO CONSTRUCT AT THIS TIME SO THAT       ELP004  
00015           THE CODE MANUAL FILES WILL BE IN ENGLISH NAME ORDER     ELP004  
00016           AND SO THAT THE INSERT NUMBER CAN BE DETERMINED SO THE  ELP004  
00017           NEW RECORDS CAN BE ADDED AFTER THE INITIAL LOAD IS      ELP004  
00018           COMPLETED.                                              ELP004  
00019                                                                   ELP004  
00020             THE INPUT FILE IS THE DATA DICTIONARY FILE.           ELP004  
00021             THE OUTPUT FILE IS THE TEMPORARY WORK FILE.           ELP004  
00022  ENVIRONMENT DIVISION.                                            ELP004  
00023  CONFIGURATION SECTION.                                           ELP004  
00024  SOURCE-COMPUTER.  IBM-370.                                       ELP004  
00025  OBJECT-COMPUTER.  IBM-370.                                       ELP004  
00026  SPECIAL-NAMES.                                                   ELP004  
00027         C01 IS TOP-OF-FORM.                                       ELP004  
00028  INPUT-OUTPUT SECTION.                                            ELP004  
00029  FILE-CONTROL.                                                    ELP004  
00030         SELECT   DD-WORK-FILE   ASSIGN TO DA-S-DDWKFIL.           ELP004  
00031                                                                   ELP004  
00032         SELECT   PRINT-FILE     ASSIGN TO PRINT.                  ELP004  
00033                                                                   ELP004  
00034         SELECT   NAME-FILE      ASSIGN TO DA-S-NAMEFILE.          ELP004  
00035                                                                   ELP004  
00036         SELECT   TEMP-WORK-FILE ASSIGN TO DA-S-TEMPFILE.          ELP004  
00037  DATA DIVISION.                                                   ELP004  
00038  FILE SECTION.                                                    ELP004  
00039  FD  DD-WORK-FILE                                                 ELP004  
00040      BLOCK 0                                                      ELP004  
00041      LABEL RECORD STANDARD                                        ELP004  
00042      RECORD CONTAINS 133 CHARACTERS                               ELP004  
00043      DATA RECORD IS DD-WORK-REC.                                  ELP004  
00044  01  DD-WORK-REC                PIC X(133).                       ELP004  
00045                                                                   ELP004  
00046  FD  PRINT-FILE                                                   ELP004  
00047      BLOCK 0                                                      ELP004  
00048      LABEL RECORD STANDARD                                        ELP004  
00049      RECORD CONTAINS 133 CHARACTERS                               ELP004  
00050      DATA RECORD IS PRT-REC.                                      ELP004  
00051  01  PRT-REC.                                                     ELP004  
00052      05  PO-CTL                 PIC X.                            ELP004  
00053      05  PO-REST                PIC X(132).                       ELP004  
00054                                                                   ELP004  
00055  FD  NAME-FILE                                                    ELP004  
00056      BLOCK 0                                                      ELP004  
00057      LABEL RECORD STANDARD                                        ELP004  
00058      RECORD CONTAINS 80 CHARACTERS                                ELP004  
00059      DATA RECORD IS NAME-REC.                                     ELP004  
00060  01  NAME-REC.                                                    ELP004  
00061      05  NF-DD-PREFIX           PIC X(08).                        ELP004  
00062      05  NF-EL-PREFIX           PIC X(08).                        ELP004  
00063      05  NF-FILE-IND            PIC X.                            ELP004  
00064      05  NF-REC-LIST-NAME       PIC X(50).                        ELP004  
00065      05  FILLER                 PIC X(13).                        ELP004  
00066                                                                   ELP004  
00067  FD  TEMP-WORK-FILE                                               ELP004  
00068      RECORDING V                                                  ELP004  
00069      BLOCK 0                                                      ELP004  
00070      LABEL RECORD STANDARD.                                       ELP004  
00071  COPY ELPMUC.                                                     ELP004  
00072      EJECT                                                        ELP004  
00073  WORKING-STORAGE SECTION.                                         ELP004  
00074  01  BEGIN-WS                   PIC X(40)     VALUE               ELP004  
00075      '**** WORKING STORAGE BEGINS HERE ****   '.                  ELP004  
00076  01  DD-REC-AREA                PIC X(133).                       ELP004  
00077                                                                   ELP004  
00078  01  DD-FL-TP01 REDEFINES DD-REC-AREA.                            ELP004  
00079      05  FILLER                 PIC X.                            ELP004  
00080      05  TP01-DATA-EL-ID        PIC X(16).                        ELP004  
00081          88  TP01-DATA-ELEMENT   VALUE 'DATA ELEMENT  : '.        ELP004  
00082      05  TP01-COBOL-ID          PIC X(03).                        ELP004  
00083          88  TP01-COBOL-IDENT    VALUE 'GC '.                     ELP004  
00084      05  TP01-COBOL-NAME        PIC X(30).                        ELP004  
00085      05  FILLER                 PIC X(83).                        ELP004  
00086                                                                   ELP004  
00087  01  DD-FL-TP02 REDEFINES DD-REC-AREA.                            ELP004  
00088      05  FILLER                 PIC XX.                           ELP004  
00089      05  TP02-SECOND-NAME       PIC X(16).                        ELP004  
00090          88  TP02-SECONDARY-NAME VALUE 'SECONDARY NAME:'.         ELP004  
00091      05  FILLER                 PIC X(115).                       ELP004  
00092                                                                   ELP004  
00093  01  DD-FL-TP03 REDEFINES DD-REC-AREA.                            ELP004  
00094      05  FILLER                 PIC X(18).                        ELP004  
00095      05  TP03-BAL-ID            PIC X(03).                        ELP004  
00096          88  TP03-BAL-IDENT      VALUE 'GA '.                     ELP004  
00097      05  TP03-BAL-NAME          PIC X(08).                        ELP004  
00098      05  FILLER                 PIC X(104).                       ELP004  
00099                                                                   ELP004  
00100  01  DD-FL-TP04 REDEFINES DD-REC-AREA.                            ELP004  
00101      05  FILLER                 PIC XX.                           ELP004  
00102      05  TP04-DE-ATTRIBUTES     PIC X(27).                        ELP004  
00103          88  TP04-ATTRIBUTES                                      ELP004  
00104               VALUE 'ATTRIBUTES EFFECTIVE AS OF'.                 ELP004  
00105      05  TP04-EFFECTIVE-DATE    PIC X(10).                        ELP004  
00106      05  FILLER                 PIC X(94).                        ELP004  
00107  EJECT                                                            ELP004  
00108  01  DD-FL-TP05 REDEFINES DD-REC-AREA.                            ELP004  
00109      05  FILLER                 PIC X(03).                        ELP004  
00110      05  TP05-DE-TYPE           PIC X(06).                        ELP004  
00111          88  TP05-TYPE           VALUE 'TYPE:'.                   ELP004  
00112      05  TP05-TYPE-CODE         PIC X.                            ELP004  
00113          88  TP05-ALPHA          VALUE 'A'.                       ELP004  
00114          88  TP05-CHAR           VALUE 'C'.                       ELP004  
00115          88  TP05-PACK           VALUE 'P'.                       ELP004  
00116      05  FILLER                 PIC X(123).                       ELP004  
00117                                                                   ELP004  
00118  01  DD-FL-TP06 REDEFINES DD-REC-AREA.                            ELP004  
00119      05  FILLER                 PIC X(03).                        ELP004  
00120      05  TP06-LENGTH-T          PIC X(10).                        ELP004  
00121          88  TP06-LENGTH-01      VALUE 'LENGTH :  '.              ELP004  
00122      05  TP06-DE-LENGTH         PIC 9(05).                        ELP004  
00123      05  FILLER                 PIC X(115).                       ELP004  
00124                                                                   ELP004  
00125  01  DD-FL-TP07 REDEFINES DD-REC-AREA.                            ELP004  
00126      05  FILLER                 PIC X(03).                        ELP004  
00127      05  TP07-DIGITS-ID         PIC X(09).                        ELP004  
00128          88  TP07-DIGITS         VALUE 'DIGITS:  '.               ELP004  
00129      05  TP07-NUMBER-DIGITS     PIC 9(03).                        ELP004  
00130      05  TP07-DECIMAL-ID        PIC X(15).                        ELP004  
00131          88  TP07-DECIMAL        VALUE '   DECIMALS:   '.         ELP004  
00132      05  TP07-NUMBER-DECIMALS   PIC 9(03).                        ELP004  
00133      05  FILLER                 PIC X(100).                       ELP004  
00134                                                                   ELP004  
00135  01  DD-FL-TP08 REDEFINES DD-REC-AREA.                            ELP004  
00136      05  FILLER                 PIC XX.                           ELP004  
00137      05  TP08-COBOL-ATT-T       PIC X(16).                        ELP004  
00138          88  TP08-COBOL-ATT      VALUE 'COBOL ATTRIBUTES'.        ELP004  
00139      05  FILLER                 PIC X(115).                       ELP004  
00140  EJECT                                                            ELP004  
00141  01  DD-FL-TP09 REDEFINES DD-REC-AREA.                            ELP004  
00142      05  FILLER                 PIC X(03).                        ELP004  
00143      05  TP09-PICTURE           PIC X(08).                        ELP004  
00144          88  TP09-PICTURE        VALUE 'PICTURE'.                 ELP004  
00145      05  FILLER                 PIC X(122).                       ELP004  
00146                                                                   ELP004  
00147  01  DD-FL-TP10 REDEFINES DD-REC-AREA.                            ELP004  
00148      05  FILLER                 PIC X(02).                        ELP004  
00149      05  TP10-DESCRIPT-T        PIC X(12).                        ELP004  
00150          88  TP10-DESCRIPT       VALUE 'DESCRIPTION:'.            ELP004  
00151      05  FILLER                 PIC X(115).                       ELP004  
00152                                                                   ELP004  
00153  01  DD-FL-TP11 REDEFINES DD-REC-AREA.                            ELP004  
00154      05  FILLER                 PIC X(04).                        ELP004  
00155      05  TP11-NUM-LINE          PIC X(03).                        ELP004  
00156      05  FILLER                 PIC X.                            ELP004  
00157      05  TP11-DESCRIPTION       PIC X(79).                        ELP004  
00158      05  FILLER                 PIC X(46).                        ELP004  
00159                                                                   ELP004  
00160  01  DD-FL-TP12 REDEFINES DD-REC-AREA.                            ELP004  
00161      05  FILLER                 PIC X(08).                        ELP004  
00162      05  TP12-POUND-SIGN-T      PIC X.                            ELP004  
00163          88  TP12-POUND-SIGN     VALUE '#'.                       ELP004  
00164      05  TP12-ENGL-NAME         PIC X(70).                        ELP004  
00165      05  FILLER                 PIC X(55).                        ELP004  
00166  EJECT                                                            ELP004  
00167  01  DD-FL-TP13 REDEFINES DD-REC-AREA.                            ELP004  
00168      05  FILLER                 PIC X(08).                        ELP004  
00169      05  TP13-CENT-SIGN-T       PIC X.                            ELP004  
00170          88  TP13-CENT-SIGN      VALUE 'Â¢'.                       ELP004  
00171      05  TP13-FILE-IDENTIFIERS  PIC X(70).                        ELP004  
00172      05  FILLER                 PIC X(55).                        ELP004  
00173                                                                   ELP004  
00174  01  DD-FL-TP14 REDEFINES DD-REC-AREA.                            ELP004  
00175      05  FILLER                 PIC X(04).                        ELP004  
00176      05  TP14-NUM-LN            PIC X(03).                        ELP004  
00177      05  FILLER                 PIC X.                            ELP004  
00178      05  TP14-STAR-SIGN-T       PIC X(06).                        ELP004  
00179          88  TP14-STAR-SIGN      VALUE '*VALUE'.                  ELP004  
00180      05  FILLER                 PIC X(05).                        ELP004  
00181      05  TP14-DEFINIT-T         PIC X(10).                        ELP004  
00182          88  TP14-DEFINIT        VALUE 'DEFINITION'.              ELP004  
00183      05  FILLER                 PIC X(104).                       ELP004  
00184                                                                   ELP004  
00185  01  DD-FL-TP15 REDEFINES DD-REC-AREA.                            ELP004  
00186      05  FILLER                 PIC X(04).                        ELP004  
00187      05  TP15-NUM-LN            PIC X(03).                        ELP004  
00188      05  FILLER                 PIC X(02).                        ELP004  
00189      05  TP15-VALUE             PIC X(10).                        ELP004  
00190      05  FILLER                 PIC X.                            ELP004  
00191      05  TP15-VALUE-DESCR       PIC X(79).                        ELP004  
00192      05  FILLER                 PIC X(29).                        ELP004  
00193                                                                   ELP004  
00194  01  DD-FL-TP16 REDEFINES DD-REC-AREA.                            ELP004  
00195      05  FILLER                 PIC X(12).                        ELP004  
00196      05  TP16-USER-SEG-T        PIC X(13).                        ELP004  
00197          88  TP16-USER-SEG       VALUE 'SEE USER SEGM'.           ELP004  
00198      05  FILLER                 PIC X(108).                       ELP004  
00199  EJECT                                                            ELP004  
00200  01  DD-FL-TP17 REDEFINES DD-REC-AREA.                            ELP004  
00201      05  FILLER                 PIC X(05).                        ELP004  
00202      05  TP17-USER-T            PIC X(04).                        ELP004  
00203          88  TP17-USER           VALUE 'USER'.                    ELP004  
00204      05  FILLER                 PIC X.                            ELP004  
00205      05  TP17-COLON-T           PIC X.                            ELP004  
00206          88  TP17-COLON          VALUE ':'.                       ELP004  
00207      05  FILLER                 PIC X(122).                       ELP004  
00208                                                                   ELP004  
00209  01  DD-FL-TP18 REDEFINES DD-REC-AREA.                            ELP004  
00210      05  FILLER                 PIC X(23).                        ELP004  
00211      05  TP18-END-OF-REPORT-T   PIC X(27).                        ELP004  
00212          88  TP18-END-OF-REPORT                                   ELP004  
00213                VALUE '* * *  END-OF-REPORT  * * *'.               ELP004  
00214      05  FILLER                 PIC X(104).                       ELP004  
00215                                                                   ELP004  
00216  01  DD-FL-TP19 REDEFINES DD-REC-AREA.                            ELP004  
00217      05  FILLER                 PIC X(11).                        ELP004  
00218      05  TP19-UNABLE-TO-FIND-T  PIC X(14).                        ELP004  
00219          88  TP19-UNABLE-TO-FIND                                  ELP004  
00220              VALUE 'UNABLE TO FIND'.                              ELP004  
00221      05  FILLER                 PIC X(08).                        ELP004  
00222      05  TP19-COBOL-NAME        PIC X(30).                        ELP004  
00223      05  FILLER                 PIC X(70).                        ELP004  
00224  EJECT                                                            ELP004  
00225  01  RECORD-LIST.                                                 ELP004  
00226      05  SORT-KEY-RL-WS.                                          ELP004  
00227         10  PREFIX-RL-WS          PIC X(08).                      ELP004  
00228         10  NAME-RL-WS            PIC X(75).                      ELP004  
00229         10  RECORD-TYPE-RL-WS     PIC X.                          ELP004  
00230         10  CODE-VALUE-RL-WS      PIC X(10).                      ELP004  
00231         10  CODE-DESC-SEQ-RL-WS   PIC 99.                         ELP004  
00232      05  RECORD-NAME-RL-WS        PIC X(50).                      ELP004  
00233      05  RECORD-FILE-TYPE-RL-WS   PIC X.                          ELP004  
00234     05  AUTO-REPRINT-FLAG-RL-WS  PIC X.                           ELP004  
00235     05  DELETE-RECORD-FLAG-RL-WS PIC X.                           ELP004  
00236                                                                   ELP004  
00237  01  CODE-VALUE.                                                  ELP004  
00238      05  SORT-KEY-CV-WS.                                          ELP004  
00239          10  PREFIX-CV-WS            PIC X(08).                   ELP004  
00240          10  NAME-CV-WS              PIC X(75).                   ELP004  
00241          10  RECORD-TYPE-CV-WS       PIC X.                       ELP004  
00242          10  CODE-VALUE-CV-WS        PIC X(10).                   ELP004  
00243          10  CODE-DESC-SEQ-CV-WS     PIC 99.                      ELP004  
00244      05  CODE-NAME-CV-WS             PIC X(50).                   ELP004  
00245      05  DELETE-CODE-FLAG-CV-WS      PIC X.                       ELP004  
00246      05  NBR-VALUE-DESC-LINES-CV-WS  PIC S9(03)   COMP-3.         ELP004  
00247      05  VALUE-DESC-LINE-CV-WS       PIC X(79)                    ELP004  
00248             OCCURS 1 TO 12 TIMES DEPENDING ON                     ELP004  
00249                       NBR-VALUE-DESC-LINES-CV-WS.                 ELP004  
00250                                                                   ELP004  
00251  01  DATA-ELEMENT.                                                ELP004  
00252      05  SORT-KEY-DE-WS.                                          ELP004  
00253          10  PREFIX-DE-WS          PIC X(08).                     ELP004  
00254          10  NAME-DE-WS            PIC X(75).                     ELP004  
00255          10  RECORD-TYPE-DE-WS     PIC X.                         ELP004  
00256          10  CODE-VALUE-DE-WS      PIC X(10).                     ELP004  
00257          10  CODE-DESC-SEQ-DE-WS   PIC 99.                        ELP004  
00258      05  ELEMENT-FORMAT-DE-WS      PIC XX.                        ELP004  
00259      05  ELEMENT-LENGTH-DE-WS      PIC S9(03)  COMP-3.            ELP004  
00260      05  FORMAT-COMMENT-DE-WS      PIC X(21).                     ELP004  
00261      05  DELETE-ELEMENT-FLAG-DE-WS PIC X.                         ELP004  
00262      05  AUTO-REPRINT-FLAG-DE-WS   PIC X.                         ELP004  
00263      05  RECORD-POS-DE-WS          PIC S9(03)  COMP-3.            ELP004  
00264      05  STORED-LENGTH-DE-WS       PIC S9(03)  COMP-3.            ELP004  
00265      05  STORED-DECIMALS-DE-WS     PIC S9(03)  COMP-3.            ELP004  
00266      05  STORED-FORMAT-DE-WS       PIC X.                         ELP004  
00267      05  CODES-FLAG-DE-WS          PIC X.                         ELP004  
00268      05  COBOL-NAME-DE-WS          PIC X(30).                     ELP004  
00269      05  BAL-NAME-DE-WS            PIC X(08).                     ELP004  
00270      05  NBR-DESC-LINES-DE-WS      PIC S9(03)  COMP-3.            ELP004  
00271      05  DESC-LINE-DE-WS           PIC X(79)                      ELP004  
00272        OCCURS 1 TO 11 TIMES DEPENDING ON NBR-DESC-LINES-DE-WS.    ELP004  
00273                                                                   ELP004  
00274  01  DUMMY-PREFIX.                                                ELP004  
00275      05  ALPHA-LETTER           PIC X      VALUE 'D'.             ELP004  
00276      05  DUMMY-NUMBER           PIC 9(7)   VALUE ZEROS.           ELP004  
00277                                                                   ELP004  
00278  01  TYPE-CODES.                                                  ELP004  
00279      05  RL-TYPE-CODE           PIC X      VALUE 'A'.             ELP004  
00280      05  DE-TYPE-CODE           PIC X      VALUE 'B'.             ELP004  
00281      05  CV-TYPE-CODE           PIC X      VALUE 'C'.             ELP004  
00282                                                                   ELP004  
00283  01  ERROR-MESSAGES.                                              ELP004  
00284      05  DESC-TOO-LONG.                                           ELP004  
00285          10  DESC-LONG-PART1    PIC X(43)  VALUE                  ELP004  
00286          'DESCRIPTION EXCEEDS THE MAX ALLOWABLE TEXT-'.           ELP004  
00287          10  DESC-LONG-PART2    PIC X(44)  VALUE                  ELP004  
00288          '-TEXT HAS BEEN TRUNCATED'.                              ELP004  
00289      05  NO-LENGTH              PIC X(87)  VALUE                  ELP004  
00290          'HAS NO LENGTH'.                                         ELP004  
00291      05  NO-DESCRIPTION         PIC X(87)  VALUE                  ELP004  
00292          'HAS NO DESCRIPTION'.                                    ELP004  
00293      05  NO-ENGLISH-NAME        PIC X(87)  VALUE                  ELP004  
00294          'HAS NO ENGLISH NAME'.                                   ELP004  
00295      05  NO-PREFIX              PIC X(87)  VALUE                  ELP004  
00296          'HAS NO PREFIX, DUMMY PREFIX USED'.                      ELP004  
00297      05  NO-FORMAT              PIC X(87)  VALUE                  ELP004  
00298          'HAS NO FORMAT'.                                         ELP004  
00299      05  NO-COBOL-NAME          PIC X(87)  VALUE                  ELP004  
00300          'HAS NO COBOL NAME, RECORD COUNT USED'.                  ELP004  
00301      05  NOT-FOUND-MSG          PIC X(87)  VALUE                  ELP004  
00302          'THIS ELEMENT ATTRIBUTES NOT FOUND ON DATA DICTIONARY'.  ELP004  
00303  EJECT                                                            ELP004  
00304  01  PROGRAM-SWITCHES-HERE      PIC X(40)  VALUE                  ELP004  
00305      '**** PROGRAM SWITCHES START HERE ****   '.                  ELP004  
00306                                                                   ELP004  
00307  01  PROGRAM-SWITCHES.                                            ELP004  
00308      05  DESC-SW                PIC X      VALUE 'N'.             ELP004  
00309      05  EOF-NAME-FILE          PIC X      VALUE 'N'.             ELP004  
00310      05  EOF-SW                 PIC X      VALUE 'N'.             ELP004  
00311      05  ERROR-SW               PIC X      VALUE 'N'.             ELP004  
00312      05  PREFIX-NOT-FOUND       PIC X      VALUE 'N'.             ELP004  
00313      05  HAS-CODE-VALUES        PIC X      VALUE 'N'.             ELP004  
00314      05  HAS-SECOND-NAME        PIC X      VALUE 'N'.             ELP004  
00315      05  NEW-RECORD             PIC X      VALUE 'Y'.             ELP004  
00316      05  PREV-VALUE             PIC X(10)  VALUE SPACES.          ELP004  
00317      05  VALUE-SW               PIC X      VALUE 'N'.             ELP004  
00318                                                                   ELP004  
00319  01  PROGRAM-COUNTERS-HERE      PIC X(40)  VALUE                  ELP004  
00320      '**** PROGRAM COUNTERS STARTS HERE ****  '.                  ELP004  
00321                                                                   ELP004  
00322  01  PROGRAM-COUNTERS.                                            ELP004  
00323      05  CODE-VALUE-CTR         PIC S9(6)   COMP-3  VALUE ZEROS.  ELP004  
00324      05  DATA-EL-CTR            PIC S9(6)   COMP-3  VALUE ZEROS.  ELP004  
00325      05  DUMMY-CTR              PIC S9(7)   COMP-3  VALUE ZEROS.  ELP004  
00326      05  LINE-CTR               PIC S9(3)   COMP-3  VALUE +66.    ELP004  
00327      05  PAGE-CTR               PIC S9(4)   COMP-3  VALUE ZEROS.  ELP004  
00328                                                                   ELP004  
00329                                                                   ELP004  
00330  01  PROGRAM-TABLE              PIC X(40)  VALUE                  ELP004  
00331      '****   PROGRAM TABLES STARTS HERE ****  '.                  ELP004  
00332                                                                   ELP004  
00333  01  PREFIX-TABLE.                                                ELP004  
00334      05  PRE-TBL-ENTRY OCCURS 58 TIMES INDEXED BY TBL-IDX.        ELP004  
00335          10  DD-PREFIX          PIC X(08).                        ELP004  
00336          10  EL-PREFIX          PIC X(08).                        ELP004  
00337          10  FILE-TYPE          PIC X.                            ELP004  
00338          10  REC-LIST-NAME      PIC X(50).                        ELP004  
00339          10  PRINT-REC-LIST-REC PIC X.                            ELP004  
00340                                                                   ELP004  
00341  01  PREFIX-NFND-TABLE.                                           ELP004  
00342      05  PRE-NFND-TBL-ENTRY OCCURS 50 TIMES INDEXED BY NFND-IDX.  ELP004  
00343          10  NFND-PREFIX        PIC X(08).                        ELP004  
00344                                                                   ELP004  
00345  01  UNSTRING-PREFIX-AREA.                                        ELP004  
00346      05  UNS-PREFIX1            PIC X(08).                        ELP004  
00347      05  UNS-PREFIX2            PIC X(08).                        ELP004  
00348      05  UNS-PREFIX3            PIC X(08).                        ELP004  
00349      05  UNS-PREFIX4            PIC X(08).                        ELP004  
00350      05  UNS-PREFIX5            PIC X(08).                        ELP004  
00351      05  UNS-PREFIX6            PIC X(08).                        ELP004  
00352      05  UNS-PREFIX7            PIC X(08).                        ELP004  
00353      05  UNS-PREFIX8            PIC X(08).                        ELP004  
00354      05  UNS-PREFIX9            PIC X(08).                        ELP004  
00355      05  UNS-PREFIX10           PIC X(08).                        ELP004  
00356      05  UNS-PREFIX11           PIC X(08).                        ELP004  
00357      05  UNS-PREFIX12           PIC X(08).                        ELP004  
00358      05  UNS-PREFIX13           PIC X(08).                        ELP004  
00359      05  UNS-PREFIX14           PIC X(08).                        ELP004  
00360      05  UNS-PREFIX15           PIC X(08).                        ELP004  
00361  01  UNSTRING-TABLE REDEFINES UNSTRING-PREFIX-AREA.               ELP004  
00362      05  UNSTRING-ELEMENT OCCURS 15 TIMES INDEXED BY UNS-IDX.     ELP004  
00363          10  UNSTRING-PREFIX    PIC X(08).                        ELP004  
00364                                                                   ELP004  
00365  EJECT                                                            ELP004  
00366  01  PRINT-WORK-AREAS-HERE      PIC X(40)  VALUE                  ELP004  
00367      '**** PRINT WORK AREAS START HERE  ****  '.                  ELP004  
00368                                                                   ELP004  
00369  01  HEADING1.                                                    ELP004  
00370      05  S-HEADING1-CTL         PIC X      VALUE '1'.             ELP004  
00371      05  FILLER                 PIC X(12)  VALUE 'REPORT ID.'.    ELP004  
00372      05  FILLER                 PIC X(07)  VALUE ' ELP004'.       ELP004  
00373      05  FILLER                 PIC X(33)  VALUE SPACES.          ELP004  
00374      05  FILLER                 PIC X(28)  VALUE                  ELP004  
00375          'DATA DICTIONARY ERROR REPORT'.                          ELP004  
00376      05  FILLER                 PIC X(30)  VALUE SPACES.          ELP004  
00377      05  FILLER                 PIC X(05)  VALUE 'PAGE'.          ELP004  
00378      05  S-HEADING1-PAGE        PIC ZZZ9.                         ELP004  
00379                                                                   ELP004  
00380  01  HEADING2.                                                    ELP004  
00381      05  S-HEADING2-CTL         PIC X      VALUE SPACE.           ELP004  
00382      05  FILLER                 PIC X(09)  VALUE 'RUN DATE'.      ELP004  
00383      05  S-HEADING2-RUN-DATE    PIC X(08)  VALUE SPACE.           ELP004  
00384      05  FILLER                 PIC X(115) VALUE SPACES.          ELP004  
00385                                                                   ELP004  
00386  01  HEADING3.                                                    ELP004  
00387      05  FILLER                 PIC X(10)  VALUE '0'.             ELP004  
00388      05  FILLER                 PIC X(15)  VALUE                  ELP004  
00389          'DATA ELEMENT ID'.                                       ELP004  
00390      05  FILLER                 PIC X(20)  VALUE SPACES.          ELP004  
00391      05  FILLER                 PIC X(20)  VALUE 'ERROR'.         ELP004  
00392      05  FILLER                 PIC X(68)  VALUE SPACES.          ELP004  
00393                                                                   ELP004  
00394  01  PRT-DTL.                                                     ELP004  
00395      05  PD-CTL                 PIC X.                            ELP004  
00396      05  FILLER                 PIC X(10)  VALUE SPACES.          ELP004  
00397      05  PD-DATA-EL-ID          PIC X(30).                        ELP004  
00398      05  FILLER                 PIC X(05)  VALUE SPACES.          ELP004  
00399      05  PD-ERROR-MSG           PIC X(87).                        ELP004  
00400                                                                   ELP004  
00401  01  TOTAL-LINE.                                                  ELP004  
00402      05  FILLER                 PIC X.                            ELP004  
00403      05  FILLER                 PIC X(37)  VALUE                  ELP004  
00404          'THE TOTAL NUMBER OF DATA ELEMENTS IS '.                 ELP004  
00405      05  PRT-DATA-EL-CTR        PIC Z,ZZZ,ZZ9-.                   ELP004  
00406      05  FILLER                 PIC X(40)  VALUE                  ELP004  
00407          '---- THE TOTAL NUMBER OF CODE VALUES IS '.              ELP004  
00408      05  PRT-CODE-VALUE-CTR     PIC Z,ZZZ,ZZ9-.                   ELP004  
00409                                                                   ELP004  
00410  PROCEDURE DIVISION.                                              ELP004  
00411  MAINLINE-PROCESS.                                                ELP004  
00412      PERFORM 0001-INITIALIZE THRU 0001-EXIT.                      ELP004  
00413      PERFORM 1000-READ-DD-FILE THRU 1000-EXIT                     ELP004  
00414               UNTIL EOF-SW = 'Y'.                                 ELP004  
00415      PERFORM 9990-WINDUP THRU 9990-EXIT.                          ELP004  
00416  EJECT                                                            ELP004  
00417  0001-INITIALIZE.                                                 ELP004  
00418      OPEN INPUT DD-WORK-FILE,                                     ELP004  
00419                 NAME-FILE                                         ELP004  
00420           OUTPUT PRINT-FILE,                                      ELP004  
00421                  TEMP-WORK-FILE.                                  ELP004  
00422      MOVE 'N' TO EOF-SW,                                          ELP004  
00423                  ERROR-SW.                                        ELP004  
00424      MOVE +66 TO LINE-CTR.                                        ELP004  
00425 *    MOVE CURRENT-DATE TO S-HEADING2-RUN-DATE.                    ELP004  
00426      ACCEPT S-HEADING2-RUN-DATE FROM DATE.                        ELP004  
00427      MOVE SPACES TO PREV-VALUE.                                   ELP004  
00428      PERFORM 0010-RESET-FLAGS THRU 0010-EXIT.                     ELP004  
00429      PERFORM 0015-WRITE-HEADER-RECORD THRU 0015-EXIT.             ELP004  
00430      MOVE SPACES TO PREFIX-TABLE,                                 ELP004  
00431                     PREFIX-NFND-TABLE,                            ELP004  
00432                     UNSTRING-PREFIX-AREA.                         ELP004  
00433      SET NFND-IDX TO 1.                                           ELP004  
00434      SET TBL-IDX TO 1.                                            ELP004  
00435      PERFORM 0020-LOAD-PRGM-TBL THRU 0020-EXIT                    ELP004  
00436          UNTIL EOF-NAME-FILE = 'Y' OR TBL-IDX > 58.               ELP004  
00437      PERFORM 0030-INITIAL-RL    THRU 0030-EXIT.                   ELP004  
00438      PERFORM 0031-INITIAL-DE-WK THRU 0031-EXIT.                   ELP004  
00439      PERFORM 0032-INITIAL-CV    THRU 0032-EXIT.                   ELP004  
00440  0001-EXIT.   EXIT.                                               ELP004  
00441                                                                   ELP004  
00442  0010-RESET-FLAGS.                                                ELP004  
00443      MOVE 'N'    TO DESC-SW,                                      ELP004  
00444                     EOF-NAME-FILE,                                ELP004  
00445                     HAS-CODE-VALUES,                              ELP004  
00446                     HAS-SECOND-NAME,                              ELP004  
00447                     PREFIX-NOT-FOUND,                             ELP004  
00448                     VALUE-SW,                                     ELP004  
00449                     NEW-RECORD.                                   ELP004  
00450      MOVE SPACES TO UNSTRING-PREFIX-AREA,                         ELP004  
00451                     PREV-VALUE.                                   ELP004  
00452  0010-EXIT.   EXIT.                                               ELP004  
00453                                                                   ELP004  
00454  0015-WRITE-HEADER-RECORD.                                        ELP004  
00455      MOVE SPACES TO HEADER-RECORD.                                ELP004  
00456      MOVE LOW-VALUES TO HEADER-SORT-KEY.                          ELP004  
00457      MOVE 'F'        TO HEADER-TYPE-RUN.                          ELP004  
00458      WRITE HEADER-RECORD.                                         ELP004  
00459  0015-EXIT.   EXIT.                                               ELP004  
00460                                                                   ELP004  
00461  0020-LOAD-PRGM-TBL.                                              ELP004  
00462      READ NAME-FILE                                               ELP004  
00463          AT END                                                   ELP004  
00464             MOVE 'Y' TO EOF-NAME-FILE.                            ELP004  
00465      MOVE NF-DD-PREFIX     TO DD-PREFIX (TBL-IDX).                ELP004  
00466      MOVE NF-EL-PREFIX     TO EL-PREFIX (TBL-IDX).                ELP004  
00467      MOVE NF-FILE-IND      TO FILE-TYPE (TBL-IDX).                ELP004  
00468      MOVE NF-REC-LIST-NAME TO REC-LIST-NAME (TBL-IDX).            ELP004  
00469      SET TBL-IDX UP BY 1.                                         ELP004  
00470  0020-EXIT.   EXIT.                                               ELP004  
00471                                                                   ELP004  
00472  0030-INITIAL-RL.                                                 ELP004  
00473      MOVE SPACES TO RECORD-LIST.                                  ELP004  
00474  0030-EXIT.   EXIT.                                               ELP004  
00475                                                                   ELP004  
00476  0031-INITIAL-DE-WK.                                              ELP004  
00477      MOVE SPACES TO DATA-ELEMENT.                                 ELP004  
00478      MOVE ZEROS  TO ELEMENT-LENGTH-DE-WS,                         ELP004  
00479                     RECORD-POS-DE-WS,                             ELP004  
00480                     STORED-LENGTH-DE-WS,                          ELP004  
00481                     STORED-DECIMALS-DE-WS.                        ELP004  
00482      MOVE +0     TO NBR-DESC-LINES-DE-WS.                         ELP004  
00483  0031-EXIT.    EXIT.                                              ELP004  
00484                                                                   ELP004  
00485  0032-INITIAL-CV.                                                 ELP004  
00486      MOVE SPACES TO CODE-VALUE.                                   ELP004  
00487      MOVE +0     TO NBR-VALUE-DESC-LINES-CV-WS.                   ELP004  
00488      MOVE 01     TO CODE-DESC-SEQ-CV-WS.                          ELP004  
00489  0032-EXIT.   EXIT.                                               ELP004  
00490  EJECT                                                            ELP004  
00491  1000-READ-DD-FILE.                                               ELP004  
00492      READ DD-WORK-FILE                                            ELP004  
00493           AT END                                                  ELP004  
00494               MOVE 'Y' TO EOF-SW                                  ELP004  
00495               GO TO 1000-EXIT.                                    ELP004  
00496      IF NEW-RECORD = 'Y'                                          ELP004  
00497           PERFORM 0010-RESET-FLAGS THRU 0010-EXIT.                ELP004  
00498      MOVE DD-WORK-REC TO DD-REC-AREA.                             ELP004  
00499      PERFORM 1500-DETERMINE-RECORD-TYPE THRU 1500-EXIT.           ELP004  
00500  1000-EXIT.   EXIT.                                               ELP004  
00501  EJECT                                                            ELP004  
00502  1500-DETERMINE-RECORD-TYPE.                                      ELP004  
00503 ***10/02/85 ADDED CHECK FOR RECORD NOT FOUND****                  ELP004  
00504      IF TP19-UNABLE-TO-FIND                                       ELP004  
00505            UNSTRING TP19-COBOL-NAME DELIMITED BY SPACE INTO       ELP004  
00506                  COBOL-NAME-DE-WS                                 ELP004  
00507            MOVE NOT-FOUND-MSG TO PD-ERROR-MSG                     ELP004  
00508            PERFORM 3800-WRITE-ERROR THRU 3800-EXIT                ELP004  
00509            MOVE 'Y' TO NEW-RECORD                                 ELP004  
00510            PERFORM 0031-INITIAL-DE-WK THRU 0031-EXIT              ELP004  
00511            ADD 1 TO DATA-EL-CTR                                   ELP004  
00512            GO TO 1500-EXIT.                                       ELP004  
00513      IF TP01-DATA-ELEMENT                                         ELP004  
00514         IF TP01-COBOL-IDENT                                       ELP004  
00515            UNSTRING TP01-COBOL-NAME DELIMITED BY SPACE INTO       ELP004  
00516                  COBOL-NAME-DE-WS                                 ELP004  
00517            GO TO 1500-EXIT.                                       ELP004  
00518      IF TP02-SECONDARY-NAME                                       ELP004  
00519         MOVE 'Y' TO HAS-SECOND-NAME                               ELP004  
00520         GO TO 1500-EXIT.                                          ELP004  
00521      IF HAS-SECOND-NAME = 'Y'                                     ELP004  
00522         IF TP03-BAL-IDENT                                         ELP004  
00523            UNSTRING TP03-BAL-NAME DELIMITED BY SPACE INTO         ELP004  
00524                  BAL-NAME-DE-WS                                   ELP004  
00525            MOVE 'N' TO HAS-SECOND-NAME                            ELP004  
00526            GO TO 1500-EXIT.                                       ELP004  
00527      IF TP04-ATTRIBUTES                                           ELP004  
00528            MOVE 'N' TO HAS-SECOND-NAME                            ELP004  
00529            GO TO 1500-EXIT.                                       ELP004  
00530      IF TP05-TYPE                                                 ELP004  
00531          IF TP05-CHAR                                             ELP004  
00532               MOVE 'AN' TO ELEMENT-FORMAT-DE-WS                   ELP004  
00533               GO TO 1500-EXIT                                     ELP004  
00534          ELSE                                                     ELP004  
00535            MOVE TP05-TYPE-CODE TO ELEMENT-FORMAT-DE-WS            ELP004  
00536            GO TO 1500-EXIT.                                       ELP004  
00537      IF TP06-LENGTH-01                                            ELP004  
00538            MOVE TP06-DE-LENGTH TO ELEMENT-LENGTH-DE-WS            ELP004  
00539                                   STORED-LENGTH-DE-WS             ELP004  
00540            MOVE ZEROS          TO STORED-DECIMALS-DE-WS           ELP004  
00541            GO TO 1500-EXIT.                                       ELP004  
00542      IF TP07-DIGITS                                               ELP004  
00543            MOVE TP07-NUMBER-DIGITS TO ELEMENT-LENGTH-DE-WS        ELP004  
00544            IF TP07-DECIMAL                                        ELP004  
00545               MOVE TP07-NUMBER-DECIMALS                           ELP004  
00546                          TO STORED-DECIMALS-DE-WS                 ELP004  
00547               GO TO 1500-EXIT                                     ELP004  
00548            ELSE                                                   ELP004  
00549             GO TO 1500-EXIT.                                      ELP004  
00550      IF TP10-DESCRIPT                                             ELP004  
00551           MOVE 'Y' TO DESC-SW                                     ELP004  
00552           GO TO 1500-EXIT.                                        ELP004  
00553      IF TP12-POUND-SIGN                                           ELP004  
00554           MOVE 'N' TO DESC-SW                                     ELP004  
00555           MOVE TP12-ENGL-NAME TO NAME-DE-WS                       ELP004  
00556           GO TO 1500-EXIT.                                        ELP004  
00557      IF TP13-CENT-SIGN                                            ELP004  
00558           UNSTRING TP13-FILE-IDENTIFIERS DELIMITED BY ',' OR ' '  ELP004  
00559              INTO UNS-PREFIX1, UNS-PREFIX2, UNS-PREFIX3,          ELP004  
00560                   UNS-PREFIX4, UNS-PREFIX5, UNS-PREFIX6,          ELP004  
00561                   UNS-PREFIX7, UNS-PREFIX8, UNS-PREFIX9,          ELP004  
00562                   UNS-PREFIX10, UNS-PREFIX11, UNS-PREFIX12,       ELP004  
00563                   UNS-PREFIX11, UNS-PREFIX14, UNS-PREFIX15        ELP004  
00564           GO TO 1500-EXIT.                                        ELP004  
00565      IF TP14-STAR-SIGN                                            ELP004  
00566           MOVE 'Y'  TO VALUE-SW,                                  ELP004  
00567                        HAS-CODE-VALUES                            ELP004  
00568           PERFORM 2200-CHECK-DE-RECORD THRU 2200-EXIT             ELP004  
00569           SET UNS-IDX TO 1                                        ELP004  
00570           MOVE DE-TYPE-CODE TO RECORD-TYPE-DE-WS                  ELP004  
00571           MOVE LOW-VALUES TO CODE-VALUE-DE-WS                     ELP004  
00572           MOVE ZEROS      TO CODE-DESC-SEQ-DE-WS                  ELP004  
00573           PERFORM 3000-WRITE-DE-RECORD THRU 3000-EXIT             ELP004  
00574              UNTIL UNS-IDX > 15                                   ELP004  
00575                     OR UNSTRING-ELEMENT (UNS-IDX) = SPACES        ELP004  
00576           ADD 1 TO DATA-EL-CTR                                    ELP004  
00577           GO TO 1500-EXIT.                                        ELP004  
00578      IF VALUE-SW = 'Y'                                            ELP004  
00579           PERFORM 2000-VALUE-ROUTINE THRU 2000-EXIT               ELP004  
00580           GO TO 1500-EXIT.                                        ELP004  
00581      IF TP18-END-OF-REPORT                                        ELP004  
00582           MOVE 'N' TO HAS-CODE-VALUES                             ELP004  
00583           PERFORM 2200-CHECK-DE-RECORD THRU 2200-EXIT             ELP004  
00584           SET UNS-IDX TO 1                                        ELP004  
00585           MOVE DE-TYPE-CODE TO RECORD-TYPE-DE-WS                  ELP004  
00586           MOVE LOW-VALUES TO CODE-VALUE-DE-WS                     ELP004  
00587           MOVE ZEROS      TO CODE-DESC-SEQ-DE-WS                  ELP004  
00588           PERFORM 3000-WRITE-DE-RECORD THRU 3000-EXIT             ELP004  
00589              UNTIL UNS-IDX > 15                                   ELP004  
00590                     OR UNSTRING-ELEMENT (UNS-IDX) = SPACES        ELP004  
00591           MOVE 'Y' TO NEW-RECORD                                  ELP004  
00592           PERFORM 0031-INITIAL-DE-WK THRU 0031-EXIT               ELP004  
00593           ADD 1 TO DATA-EL-CTR                                    ELP004  
00594           GO TO 1500-EXIT.                                        ELP004  
00595      IF DESC-SW = 'Y'                                             ELP004  
00596           IF TP11-DESCRIPTION NOT = SPACES                        ELP004  
00597                ADD +1 TO NBR-DESC-LINES-DE-WS                     ELP004  
00598                IF NBR-DESC-LINES-DE-WS < 12                       ELP004  
00599                   MOVE TP11-DESCRIPTION TO                        ELP004  
00600                    DESC-LINE-DE-WS (NBR-DESC-LINES-DE-WS)         ELP004  
00601                   GO TO 1500-EXIT                                 ELP004  
00602                ELSE                                               ELP004  
00603                 SUBTRACT 1 FROM NBR-DESC-LINES-DE-WS              ELP004  
00604                 MOVE DESC-TOO-LONG TO PD-ERROR-MSG                ELP004  
00605                 PERFORM 3800-WRITE-ERROR THRU 3800-EXIT.          ELP004  
00606  1500-EXIT.   EXIT.                                               ELP004  
00607  EJECT                                                            ELP004  
00608  2000-VALUE-ROUTINE.                                              ELP004  
00609      IF TP18-END-OF-REPORT                                        ELP004  
00610           MOVE 'Y' TO NEW-RECORD                                  ELP004  
00611           SET UNS-IDX TO 1                                        ELP004  
00612           MOVE CV-TYPE-CODE TO RECORD-TYPE-CV-WS                  ELP004  
00613           MOVE NAME-DE-WS   TO NAME-CV-WS                         ELP004  
00614           PERFORM 3100-WRITE-CV-RECORD THRU 3100-EXIT             ELP004  
00615              UNTIL UNS-IDX > 15 OR                                ELP004  
00616                UNSTRING-ELEMENT (UNS-IDX) = SPACES                ELP004  
00617           PERFORM 0031-INITIAL-DE-WK THRU 0031-EXIT               ELP004  
00618           PERFORM 0032-INITIAL-CV    THRU 0032-EXIT               ELP004  
00619           GO TO 2000-EXIT.                                        ELP004  
00620      IF TP11-NUM-LINE = '999'                                     ELP004  
00621           GO TO 2000-EXIT.                                        ELP004  
00622      IF TP17-USER                                                 ELP004  
00623           GO TO 2000-EXIT.                                        ELP004  
00624      IF PREV-VALUE NOT = TP15-VALUE                               ELP004  
00625           IF PREV-VALUE = SPACES                                  ELP004  
00626            MOVE TP15-VALUE TO PREV-VALUE,                         ELP004  
00627                               CODE-VALUE-CV-WS                    ELP004  
00628            ADD +1 TO NBR-VALUE-DESC-LINES-CV-WS                   ELP004  
00629            MOVE TP15-VALUE-DESCR TO                               ELP004  
00630             VALUE-DESC-LINE-CV-WS (NBR-VALUE-DESC-LINES-CV-WS)    ELP004  
00631               GO TO 2000-EXIT                                     ELP004  
00632           ELSE                                                    ELP004  
00633             IF TP15-VALUE NOT = SPACE                             ELP004  
00634              SET UNS-IDX TO 1                                     ELP004  
00635              MOVE CV-TYPE-CODE TO RECORD-TYPE-CV-WS               ELP004  
00636              MOVE NAME-DE-WS   TO NAME-CV-WS                      ELP004  
00637              PERFORM 3100-WRITE-CV-RECORD THRU 3100-EXIT          ELP004  
00638                UNTIL UNS-IDX > 15 OR                              ELP004  
00639                 UNSTRING-ELEMENT (UNS-IDX) = SPACES               ELP004  
00640              ADD 1 TO CODE-VALUE-CTR                              ELP004  
00641              PERFORM 0032-INITIAL-CV THRU 0032-EXIT               ELP004  
00642              MOVE TP15-VALUE TO PREV-VALUE,                       ELP004  
00643                                 CODE-VALUE-CV-WS                  ELP004  
00644              ADD +1 TO NBR-VALUE-DESC-LINES-CV-WS                 ELP004  
00645              MOVE TP15-VALUE-DESCR TO                             ELP004  
00646               VALUE-DESC-LINE-CV-WS (NBR-VALUE-DESC-LINES-CV-WS)  ELP004  
00647              GO TO 2000-EXIT.                                     ELP004  
00648      IF TP15-VALUE = SPACE                                        ELP004  
00649           IF TP15-VALUE-DESCR = SPACES                            ELP004  
00650                 GO TO 2000-EXIT                                   ELP004  
00651           ELSE                                                    ELP004  
00652             ADD +1 TO NBR-VALUE-DESC-LINES-CV-WS                  ELP004  
00653             IF NBR-VALUE-DESC-LINES-CV-WS < 13                    ELP004  
00654              MOVE TP15-VALUE-DESCR TO                             ELP004  
00655               VALUE-DESC-LINE-CV-WS (NBR-VALUE-DESC-LINES-CV-WS)  ELP004  
00656             ELSE                                                  ELP004  
00657               SUBTRACT 1 FROM NBR-VALUE-DESC-LINES-CV-WS          ELP004  
00658               MOVE CV-TYPE-CODE TO RECORD-TYPE-CV-WS              ELP004  
00659               MOVE NAME-DE-WS   TO NAME-CV-WS                     ELP004  
00660               SET UNS-IDX TO 1                                    ELP004  
00661               PERFORM 3100-WRITE-CV-RECORD THRU 3100-EXIT         ELP004  
00662                 UNTIL UNS-IDX > 15 OR                             ELP004  
00663                   UNSTRING-ELEMENT (UNS-IDX) = SPACES             ELP004  
00664               ADD 05 TO CODE-DESC-SEQ-CV-WS                       ELP004  
00665               MOVE 1 TO NBR-VALUE-DESC-LINES-CV-WS                ELP004  
00666 *****CHANGE TO GET 13TH LINE OF CODE VALUE DESCRIPTION******      ELP004  
00667 ***** ADDED THE FOLLOWING LINE AND CHANGE MOVE 0 TO NBR-****      ELP004  
00668 ***** VALUE-DESC-LINES-CV-WS TO MOVE 1 TO NBR-VALUE-DESC-***      ELP004  
00669 ***** LINES-CV-WS.                                     *****      ELP004  
00670               MOVE TP15-VALUE-DESCR TO                            ELP004  
00671                VALUE-DESC-LINE-CV-WS (NBR-VALUE-DESC-LINES-CV-WS).ELP004  
00672  2000-EXIT.   EXIT.                                               ELP004  
00673  EJECT                                                            ELP004  
00674  2200-CHECK-DE-RECORD.                                            ELP004  
00675      IF COBOL-NAME-DE-WS = SPACES                                 ELP004  
00676          MOVE DATA-EL-CTR TO COBOL-NAME-DE-WS                     ELP004  
00677          MOVE NO-COBOL-NAME TO PD-ERROR-MSG                       ELP004  
00678          PERFORM 3800-WRITE-ERROR THRU 3800-EXIT.                 ELP004  
00679      IF UNSTRING-PREFIX-AREA = SPACES                             ELP004  
00680          ADD +1  TO DUMMY-CTR                                     ELP004  
00681          MOVE DUMMY-CTR    TO DUMMY-NUMBER                        ELP004  
00682          MOVE DUMMY-PREFIX TO UNS-PREFIX1                         ELP004  
00683          MOVE NO-PREFIX    TO PD-ERROR-MSG                        ELP004  
00684          PERFORM 3800-WRITE-ERROR THRU 3800-EXIT.                 ELP004  
00685      IF NBR-DESC-LINES-DE-WS = +0                                 ELP004  
00686              MOVE +1 TO NBR-DESC-LINES-DE-WS                      ELP004  
00687              MOVE NO-DESCRIPTION TO PD-ERROR-MSG                  ELP004  
00688              PERFORM 3800-WRITE-ERROR THRU 3800-EXIT.             ELP004  
00689      IF NAME-DE-WS = SPACES                                       ELP004  
00690          MOVE COBOL-NAME-DE-WS TO NAME-DE-WS                      ELP004  
00691          MOVE NO-ENGLISH-NAME TO PD-ERROR-MSG                     ELP004  
00692          PERFORM 3800-WRITE-ERROR THRU 3800-EXIT.                 ELP004  
00693      IF ELEMENT-LENGTH-DE-WS = ZEROS                              ELP004  
00694          MOVE NO-LENGTH TO PD-ERROR-MSG                           ELP004  
00695          PERFORM 3800-WRITE-ERROR THRU 3800-EXIT.                 ELP004  
00696      IF ELEMENT-FORMAT-DE-WS = SPACES                             ELP004  
00697          MOVE NO-FORMAT TO PD-ERROR-MSG                           ELP004  
00698          PERFORM 3800-WRITE-ERROR THRU 3800-EXIT.                 ELP004  
00699  2200-EXIT.   EXIT.                                               ELP004  
00700  EJECT                                                            ELP004  
00701  2500-SEARCH-PRE-TABLE.                                           ELP004  
00702      SET TBL-IDX TO 1.                                            ELP004  
00703      SEARCH PRE-TBL-ENTRY                                         ELP004  
00704           AT END                                                  ELP004  
00705              MOVE 'Y' TO PREFIX-NOT-FOUND                         ELP004  
00706      WHEN UNSTRING-ELEMENT (UNS-IDX) =                            ELP004  
00707             DD-PREFIX (TBL-IDX)                                   ELP004  
00708       MOVE EL-PREFIX (TBL-IDX) TO UNSTRING-ELEMENT (UNS-IDX)      ELP004  
00709       MOVE 'Y' TO PRINT-REC-LIST-REC (TBL-IDX).                   ELP004  
00710  2500-EXIT.   EXIT.                                               ELP004  
00711  EJECT                                                            ELP004  
00712  3000-WRITE-DE-RECORD.                                            ELP004  
00713      PERFORM 2500-SEARCH-PRE-TABLE THRU 2500-EXIT.                ELP004  
00714      IF PREFIX-NOT-FOUND = 'Y'                                    ELP004  
00715         MOVE 'N' TO PREFIX-NOT-FOUND                              ELP004  
00716         IF NFND-IDX > 50                                          ELP004  
00717             MOVE UNSTRING-ELEMENT (UNS-IDX) TO PREFIX-RL-WS       ELP004  
00718             MOVE RL-TYPE-CODE TO RECORD-TYPE-RL-WS                ELP004  
00719             MOVE LOW-VALUES   TO CODE-VALUE-RL-WS,                ELP004  
00720                                  NAME-RL-WS                       ELP004  
00721             MOVE ZEROS        TO CODE-DESC-SEQ-RL-WS              ELP004  
00722             MOVE 'NOT FOUND'  TO RECORD-NAME-RL-WS                ELP004  
00723             PERFORM 3200-WRITE-RL-RECORD THRU 3300-EXIT           ELP004  
00724         ELSE                                                      ELP004  
00725           MOVE UNSTRING-ELEMENT (UNS-IDX) TO                      ELP004  
00726                     NFND-PREFIX (NFND-IDX)                        ELP004  
00727           SET NFND-IDX UP BY 1.                                   ELP004  
00728      MOVE UNSTRING-ELEMENT (UNS-IDX) TO PREFIX-DE-WS.             ELP004  
00729      WRITE DATA-ELEMENT-LOAD FROM DATA-ELEMENT.                   ELP004  
00730      SET UNS-IDX UP BY 1.                                         ELP004  
00731  3000-EXIT.   EXIT.                                               ELP004  
00732  3100-WRITE-CV-RECORD.                                            ELP004  
00733      IF NBR-VALUE-DESC-LINES-CV-WS  = +0                          ELP004  
00734           MOVE +1 TO NBR-VALUE-DESC-LINES-CV-WS.                  ELP004  
00735      MOVE VALUE-DESC-LINE-CV-WS (1) TO CODE-NAME-CV-WS.           ELP004  
00736      MOVE UNSTRING-ELEMENT (UNS-IDX) TO PREFIX-CV-WS.             ELP004  
00737      WRITE CODE-VALUE-LOAD FROM CODE-VALUE.                       ELP004  
00738      SET UNS-IDX UP BY 1.                                         ELP004  
00739  3100-EXIT.   EXIT.                                               ELP004  
00740  3200-WRITE-RL-RECORD.                                            ELP004  
00741      WRITE RECORD-LIST-LOAD FROM RECORD-LIST.                     ELP004  
00742  3200-EXIT.   EXIT.                                               ELP004  
00743  3300-WRITE-OUT-PRE-TBL.                                          ELP004  
00744      IF PRINT-REC-LIST-REC (TBL-IDX) = 'Y'                        ELP004  
00745          NEXT SENTENCE                                            ELP004  
00746      ELSE                                                         ELP004  
00747        GO TO 3300-EXIT.                                           ELP004  
00748      MOVE EL-PREFIX (TBL-IDX)     TO PREFIX-RL-WS.                ELP004  
00749      MOVE RL-TYPE-CODE            TO RECORD-TYPE-RL-WS.           ELP004  
00750      MOVE LOW-VALUES              TO CODE-VALUE-RL-WS,            ELP004  
00751                                      NAME-RL-WS.                  ELP004  
00752      MOVE ZEROS                   TO CODE-DESC-SEQ-RL-WS.         ELP004  
00753      MOVE REC-LIST-NAME (TBL-IDX) TO RECORD-NAME-RL-WS.           ELP004  
00754      MOVE FILE-TYPE (TBL-IDX)     TO RECORD-FILE-TYPE-RL-WS.      ELP004  
00755      PERFORM 3200-WRITE-RL-RECORD THRU 3200-EXIT.                 ELP004  
00756  3300-EXIT.   EXIT.                                               ELP004  
00757  EJECT                                                            ELP004  
00758  3400-WRITE-OUT-NFND-PRE-TBL.                                     ELP004  
00759      MOVE NFND-PREFIX (NFND-IDX)  TO PREFIX-RL-WS.                ELP004  
00760      MOVE RL-TYPE-CODE            TO RECORD-TYPE-RL-WS.           ELP004  
00761      MOVE LOW-VALUES              TO CODE-VALUE-RL-WS,            ELP004  
00762                                      NAME-RL-WS.                  ELP004  
00763      MOVE ZEROS                   TO CODE-DESC-SEQ-RL-WS.         ELP004  
00764      MOVE '  N O T   F O U N D  ' TO RECORD-NAME-RL-WS.           ELP004  
00765      PERFORM 3200-WRITE-RL-RECORD THRU 3200-EXIT.                 ELP004  
00766  3400-EXIT.   EXIT.                                               ELP004  
00767  EJECT                                                            ELP004  
00768  3500-ERROR-HEADING.                                              ELP004  
00769      ADD 1 TO PAGE-CTR.                                           ELP004  
00770      MOVE PAGE-CTR TO S-HEADING1-PAGE.                            ELP004  
00771      MOVE SPACES TO PRT-REC.                                      ELP004  
00772      WRITE PRT-REC FROM HEADING1 AFTER ADVANCING TOP-OF-FORM      ELP004  
00773      MOVE SPACES TO PRT-REC.                                      ELP004  
00774      WRITE PRT-REC FROM HEADING2 AFTER ADVANCING 1 LINE.          ELP004  
00775      MOVE SPACES TO PRT-REC.                                      ELP004  
00776      WRITE PRT-REC FROM HEADING3 AFTER ADVANCING 2 LINES.         ELP004  
00777      MOVE 7 TO LINE-CTR.                                          ELP004  
00778  3500-EXIT.   EXIT.                                               ELP004  
00779  3800-WRITE-ERROR.                                                ELP004  
00780      MOVE COBOL-NAME-DE-WS TO PD-DATA-EL-ID.                      ELP004  
00781      IF LINE-CTR > +60                                            ELP004  
00782         PERFORM 3500-ERROR-HEADING THRU 3500-EXIT.                ELP004  
00783      MOVE SPACE TO PRT-REC.                                       ELP004  
00784      WRITE PRT-REC FROM PRT-DTL AFTER ADVANCING 2 LINES.          ELP004  
00785      ADD +2 TO LINE-CTR.                                          ELP004  
00786      MOVE 'Y' TO ERROR-SW.                                        ELP004  
00787  3800-EXIT.   EXIT.                                               ELP004  
00788  EJECT                                                            ELP004  
00789  9990-WINDUP.                                                     ELP004  
00790      PERFORM 3300-WRITE-OUT-PRE-TBL THRU 3300-EXIT                ELP004  
00791          VARYING TBL-IDX FROM 1 BY 1 UNTIL TBL-IDX > 58.          ELP004  
00792      PERFORM 3400-WRITE-OUT-NFND-PRE-TBL THRU 3400-EXIT           ELP004  
00793         VARYING NFND-IDX FROM 1 BY 1                              ELP004  
00794          UNTIL NFND-IDX > 50 OR NFND-PREFIX (NFND-IDX) = SPACES.  ELP004  
00795      IF ERROR-SW = 'N'                                            ELP004  
00796          PERFORM 3500-ERROR-HEADING THRU 3500-EXIT                ELP004  
00797          MOVE '  N O  E R R O R S  D E T E C T E D  ' TO          ELP004  
00798             PO-REST                                               ELP004  
00799          WRITE PRT-REC AFTER ADVANCING 3 LINES.                   ELP004  
00800      IF LINE-CTR > 60                                             ELP004  
00801          PERFORM 3500-ERROR-HEADING THRU 3500-EXIT.               ELP004  
00802      MOVE ALL '*' TO PO-REST.                                     ELP004  
00803      WRITE PRT-REC AFTER ADVANCING 2 LINES.                       ELP004  
00804      MOVE SPACE TO PRT-REC.                                       ELP004  
00805      MOVE DATA-EL-CTR    TO PRT-DATA-EL-CTR.                      ELP004  
00806      MOVE CODE-VALUE-CTR TO PRT-CODE-VALUE-CTR.                   ELP004  
00807      WRITE PRT-REC FROM TOTAL-LINE AFTER ADVANCING 1 LINE.        ELP004  
00808      MOVE SPACE TO PRT-REC.                                       ELP004  
00809      MOVE ALL '*' TO PO-REST.                                     ELP004  
00810      WRITE PRT-REC AFTER ADVANCING 2 LINES.                       ELP004  
00811      CLOSE DD-WORK-FILE,                                          ELP004  
00812            NAME-FILE                                              ELP004  
00813            PRINT-FILE,                                            ELP004  
00814            TEMP-WORK-FILE.                                        ELP004  
00815      STOP RUN.                                                    ELP004  
00816  9990-EXIT.   EXIT.                                               ELP004  
