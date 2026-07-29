00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELP013  
00003  PROGRAM-ID.         ELP013.                                         LV001
00004                                                                   ELP013  
00005  AUTHOR.             RICHARD J. LUKETICH, CCP                     ELP013  
00006                                                                   ELP013  
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELP013  
00008                      233 N. MICHIGAN AVENUE                       ELP013  
00009                      CHICAGO, ILLINOIS  60601-5655                ELP013  
00010                                                                   ELP013  
00011  DATE-WRITTEN.       04-OCT-1985.                                 ELP013  
00012                      03-MAR-1986 MODIFY FOR CROSS-REFERENCE FILES.ELP013  
00013                                                                   ELP013  
00014  DATE-COMPILED.                                                   ELP013  
00015      EJECT                                                        ELP013  
00016  ENVIRONMENT DIVISION.                                            ELP013  
00017                                                                   ELP013  
00018                                                                   ELP013  
00019  CONFIGURATION SECTION.                                           ELP013  
00020                                                                   ELP013  
00021  SOURCE-COMPUTER.    IBM-3081.                                    ELP013  
00022  OBJECT-COMPUTER.    IBM-3081.                                    ELP013  
00023                                                                   ELP013  
00024                                                                   ELP013  
00025  INPUT-OUTPUT SECTION.                                            ELP013  
00026                                                                   ELP013  
00027  FILE-CONTROL.                                                    ELP013  
00028                                                                   ELP013  
00029      SELECT RECORD-LIST-FILE                                      ELP013  
00030          ASSIGN               TO ELPRL                            ELP013  
00031          ORGANIZATION         IS INDEXED                          ELP013  
00032          ACCESS MODE          IS DYNAMIC                          ELP013  
00033          RECORD KEY           IS RL-RECORD-PREFIX                 ELP013  
00034          FILE STATUS          IS RL-STATUS.                       ELP013  
00035                                                                   ELP013  
00036      SELECT DATA-ELEMENT-FILE                                     ELP013  
00037          ASSIGN               TO ELPDE                            ELP013  
00038          ORGANIZATION         IS INDEXED                          ELP013  
00039          ACCESS MODE          IS DYNAMIC                          ELP013  
00040          RECORD KEY           IS DE-PRIMARY-KEY                   ELP013  
00041          FILE STATUS          IS DE-STATUS.                       ELP013  
00042                                                                   ELP013  
00043      SELECT ENGLISH-NAME-FILE                                     ELP013  
00044          ASSIGN               TO ELPEN                            ELP013  
00045          ORGANIZATION         IS INDEXED                          ELP013  
00046          ACCESS MODE          IS DYNAMIC                          ELP013  
00047          RECORD KEY           IS EN-KEY                           ELP013  
00048          FILE STATUS          IS EN-STATUS.                       ELP013  
00049                                                                   ELP013  
00050      SELECT SYSTEM-NAME-FILE                                      ELP013  
00051          ASSIGN               TO ELPCN                            ELP013  
00052          ORGANIZATION         IS INDEXED                          ELP013  
00053          ACCESS MODE          IS DYNAMIC                          ELP013  
00054          RECORD KEY           IS CN-KEY                           ELP013  
00055          FILE STATUS          IS CN-STATUS.                       ELP013  
00056                                                                   ELP013  
00057      SELECT CODE-VALUES-FILE                                      ELP013  
00058          ASSIGN               TO ELPCV                            ELP013  
00059          ORGANIZATION         IS INDEXED                          ELP013  
00060          ACCESS MODE          IS DYNAMIC                          ELP013  
00061          RECORD KEY           IS CV-CODE-KEY                      ELP013  
00062          FILE STATUS          IS CV-STATUS.                       ELP013  
00063                                                                   ELP013  
00064      EJECT                                                        ELP013  
00065  DATA DIVISION.                                                   ELP013  
00066                                                                   ELP013  
00067  FILE SECTION.                                                    ELP013  
00068      EJECT                                                        ELP013  
00069  FD  RECORD-LIST-FILE                                             ELP013  
00070      LABEL RECORDS          ARE STANDARD                          ELP013  
00071      DATA RECORD             IS RECORD-LIST-RECORD.               ELP013  
00072                                                                   ELP013  
00073  01  RECORD-LIST-RECORD.                                          ELP013  
00074      COPY ELPRLC.                                                 ELP013  
00075                                                                   ELP013  
00076      EJECT                                                        ELP013  
00077  FD  DATA-ELEMENT-FILE                                            ELP013  
00078      LABEL RECORDS          ARE STANDARD                          ELP013  
00079      DATA RECORD             IS DATA-ELEMENT-RECORD.              ELP013  
00080                                                                   ELP013  
00081  01  DATA-ELEMENT-RECORD.                                         ELP013  
00082      COPY ELPDEC.                                                 ELP013  
00083                                                                   ELP013  
00084      EJECT                                                        ELP013  
00085  FD  ENGLISH-NAME-FILE                                            ELP013  
00086      LABEL RECORDS          ARE STANDARD                          ELP013  
00087      DATA RECORD             IS ENGLISH-NAME-RECORD.              ELP013  
00088                                                                   ELP013  
00089  01  ENGLISH-NAME-RECORD.                                         ELP013  
00090      COPY ELPENC.                                                 ELP013  
00091                                                                   ELP013  
00092      EJECT                                                        ELP013  
00093  FD  SYSTEM-NAME-FILE                                             ELP013  
00094      LABEL RECORDS          ARE STANDARD                          ELP013  
00095      DATA RECORD             IS SYSTEM-NAME-RECORD.               ELP013  
00096                                                                   ELP013  
00097  01  SYSTEM-NAME-RECORD.                                          ELP013  
00098      COPY ELPCNC.                                                 ELP013  
00099                                                                   ELP013  
00100      EJECT                                                        ELP013  
00101  FD  CODE-VALUES-FILE                                             ELP013  
00102      LABEL RECORDS          ARE STANDARD                          ELP013  
00103      DATA RECORD             IS CODE-VALUES-RECORD.               ELP013  
00104                                                                   ELP013  
00105  01  CODE-VALUES-RECORD.                                          ELP013  
00106      COPY ELPCVC.                                                 ELP013  
00107      EJECT                                                        ELP013  
00108  WORKING-STORAGE SECTION.                                         ELP013  
00109                                                                   ELP013  
00110  01  FILE-STATUS-FLAGS.                                           ELP013  
00111      02  RL-STATUS               PICTURE  X(02).                  ELP013  
00112      02  DE-STATUS               PICTURE  X(02).                  ELP013  
00113      02  EN-STATUS               PICTURE  X(02).                  ELP013  
00114      02  CN-STATUS               PICTURE  X(02).                  ELP013  
00115      02  CV-STATUS               PICTURE  X(02).                  ELP013  
00116      EJECT                                                        ELP013  
00117  PROCEDURE DIVISION.                                              ELP013  
00118                                                                   ELP013  
00119  A000-MAIN-ROUTINE.                                               ELP013  
00120                                                                   ELP013  
00121      PERFORM A100-OPEN-FILES-FOR-SEED.                            ELP013  
00122                                                                   ELP013  
00123      PERFORM A200-WRITE-SEED-RECORDS.                             ELP013  
00124                                                                   ELP013  
00125      PERFORM A500-CLOSE-FILES.                                    ELP013  
00126                                                                   ELP013  
00127      STOP RUN.                                                    ELP013  
00128      EJECT                                                        ELP013  
00129  A100-OPEN-FILES-FOR-SEED.                                        ELP013  
00130                                                                   ELP013  
00131      OPEN OUTPUT                                                  ELP013  
00132          RECORD-LIST-FILE                                         ELP013  
00133          DATA-ELEMENT-FILE                                        ELP013  
00134          ENGLISH-NAME-FILE                                        ELP013  
00135          SYSTEM-NAME-FILE                                         ELP013  
00136          CODE-VALUES-FILE.                                        ELP013  
00137                                                                   ELP013  
00138      IF RL-STATUS IS EQUAL TO '00'                                ELP013  
00139      THEN                                                         ELP013  
00140          DISPLAY 'RECORD LIST FILE OPENED FOR OUTPUT'             ELP013  
00141                                  UPON SYSOUT                      ELP013  
00142      ELSE                                                         ELP013  
00143          DISPLAY 'RECORD LIST FILE STATUS OPEN RETURN '           ELP013  
00144                  RL-STATUS                                        ELP013  
00145                                  UPON SYSOUT.                     ELP013  
00146                                                                   ELP013  
00147      IF DE-STATUS IS EQUAL TO '00'                                ELP013  
00148      THEN                                                         ELP013  
00149          DISPLAY 'DATA ELEMENT FILE OPENED FOR OUTPUT'            ELP013  
00150                                  UPON SYSOUT                      ELP013  
00151      ELSE                                                         ELP013  
00152          DISPLAY 'DATA ELEMENT FILE STATUS OPEN RETURN '          ELP013  
00153                  DE-STATUS                                        ELP013  
00154                                  UPON SYSOUT.                     ELP013  
00155                                                                   ELP013  
00156      IF EN-STATUS IS EQUAL TO '00'                                ELP013  
00157      THEN                                                         ELP013  
00158          DISPLAY 'ENGLISH NAME INDEX FILE OPENED FOR OUTPUT'      ELP013  
00159                                  UPON SYSOUT                      ELP013  
00160      ELSE                                                         ELP013  
00161          DISPLAY 'ENGLISH NAME INDEX FILE STATUS OPEN RETURN '    ELP013  
00162                  EN-STATUS                                        ELP013  
00163                                  UPON SYSOUT.                     ELP013  
00164                                                                   ELP013  
00165      IF CN-STATUS IS EQUAL TO '00'                                ELP013  
00166      THEN                                                         ELP013  
00167          DISPLAY 'SYSTEM NAME INDEX FILE OPENED FOR OUTPUT'       ELP013  
00168                                  UPON SYSOUT                      ELP013  
00169      ELSE                                                         ELP013  
00170          DISPLAY 'SYSTEM NAME INDEX FILE STATUS OPEN RETURN '     ELP013  
00171                  DE-STATUS                                        ELP013  
00172                                  UPON SYSOUT.                     ELP013  
00173                                                                   ELP013  
00174      IF CV-STATUS IS EQUAL TO '00'                                ELP013  
00175      THEN                                                         ELP013  
00176          DISPLAY 'CODE VALUES FILE OPENED FOR OUTPUT'             ELP013  
00177                                  UPON SYSOUT                      ELP013  
00178      ELSE                                                         ELP013  
00179          DISPLAY 'CODE VALUES FILE STATUS OPEN RETURN '           ELP013  
00180                  CV-STATUS                                        ELP013  
00181                                  UPON SYSOUT.                     ELP013  
00182      EJECT                                                        ELP013  
00183  A200-WRITE-SEED-RECORDS.                                         ELP013  
00184                                                                   ELP013  
00185      MOVE LOW-VALUES          TO RL-RECORD-PREFIX.                ELP013  
00186      WRITE RECORD-LIST-RECORD.                                    ELP013  
00187      IF RL-STATUS IS EQUAL TO '00'                                ELP013  
00188      THEN                                                         ELP013  
00189          DISPLAY 'RECORD LIST SEED RECORD WRITTEN'                ELP013  
00190                                  UPON SYSOUT                      ELP013  
00191      ELSE                                                         ELP013  
00192          DISPLAY 'RECORD LIST FILE STATUS WRITE RETURN '          ELP013  
00193                  RL-STATUS                                        ELP013  
00194                                  UPON SYSOUT.                     ELP013  
00195                                                                   ELP013  
00196      MOVE LOW-VALUES          TO DE-RECORD-PREFIX.                ELP013  
00197      MOVE ZERO                TO DE-ELEMENT-NBR                   ELP013  
00198      MOVE LOW-VALUES          TO DE-SECONDARY-NAME-KEY.           ELP013  
00199      MOVE 1                   TO DE-NBR-DESC-LINES.               ELP013  
00200      WRITE DATA-ELEMENT-RECORD.                                   ELP013  
00201      IF DE-STATUS IS EQUAL TO '00'                                ELP013  
00202      THEN                                                         ELP013  
00203          DISPLAY 'DATA ELEMENT SEED RECORD WRITTEN'               ELP013  
00204                                  UPON SYSOUT                      ELP013  
00205      ELSE                                                         ELP013  
00206          DISPLAY 'DATA ELEMENT FILE STATUS WRITE RETURN '         ELP013  
00207                  DE-STATUS                                        ELP013  
00208                                  UPON SYSOUT.                     ELP013  
00209                                                                   ELP013  
00210      MOVE LOW-VALUES          TO EN-RECORD-PREFIX.                ELP013  
00211      MOVE LOW-VALUES          TO EN-ELEMENT-NAME.                 ELP013  
00212      MOVE ZERO                TO EN-ELEMENT-NBR.                  ELP013  
00213      WRITE ENGLISH-NAME-RECORD.                                   ELP013  
00214      IF EN-STATUS IS EQUAL TO '00'                                ELP013  
00215      THEN                                                         ELP013  
00216          DISPLAY 'ENGLISH NAME SEED RECORD WRITTEN'               ELP013  
00217                                  UPON SYSOUT                      ELP013  
00218      ELSE                                                         ELP013  
00219          DISPLAY 'ENGLISH NAME FILE STATUS WRITE RETURN '         ELP013  
00220                  EN-STATUS                                        ELP013  
00221                                  UPON SYSOUT.                     ELP013  
00222                                                                   ELP013  
00223      MOVE LOW-VALUES          TO CN-RECORD-PREFIX.                ELP013  
00224      MOVE LOW-VALUES          TO CN-COBOL-NAME.                   ELP013  
00225      MOVE ZERO                TO CN-ELEMENT-NBR.                  ELP013  
00226      WRITE SYSTEM-NAME-RECORD.                                    ELP013  
00227      IF CN-STATUS IS EQUAL TO '00'                                ELP013  
00228      THEN                                                         ELP013  
00229          DISPLAY 'SYSTEM NAME SEED RECORD WRITTEN'                ELP013  
00230                                  UPON SYSOUT                      ELP013  
00231      ELSE                                                         ELP013  
00232          DISPLAY 'SYSTEM NAME FILE STATUS WRITE RETURN '          ELP013  
00233                  CN-STATUS                                        ELP013  
00234                                  UPON SYSOUT.                     ELP013  
00235                                                                   ELP013  
00236      MOVE LOW-VALUES          TO CV-RECORD-PREFIX.                ELP013  
00237      MOVE ZERO                TO CV-ELEMENT-NBR.                  ELP013  
00238      MOVE LOW-VALUES          TO CV-CODE-VALUE.                   ELP013  
00239      MOVE ZERO                TO CV-CODE-DESC-SEQ.                ELP013  
00240      MOVE 1                   TO CV-NBR-VALUE-DESC-LINES.         ELP013  
00241      WRITE CODE-VALUES-RECORD.                                    ELP013  
00242      IF CV-STATUS IS EQUAL TO '00'                                ELP013  
00243      THEN                                                         ELP013  
00244          DISPLAY 'CODE VALUES SEED RECORD WRITTEN'                ELP013  
00245                                  UPON SYSOUT                      ELP013  
00246      ELSE                                                         ELP013  
00247          DISPLAY 'CODE VALUES FILE STATUS WRITE RETURN '          ELP013  
00248                  CV-STATUS                                        ELP013  
00249                                  UPON SYSOUT.                     ELP013  
00250      EJECT                                                        ELP013  
00251  A500-CLOSE-FILES.                                                ELP013  
00252                                                                   ELP013  
00253      CLOSE                                                        ELP013  
00254          RECORD-LIST-FILE                                         ELP013  
00255          DATA-ELEMENT-FILE                                        ELP013  
00256          ENGLISH-NAME-FILE                                        ELP013  
00257          SYSTEM-NAME-FILE                                         ELP013  
00258          CODE-VALUES-FILE.                                        ELP013  
00259                                                                   ELP013  
00260      IF RL-STATUS IS EQUAL TO '00'                                ELP013  
00261      THEN                                                         ELP013  
00262          DISPLAY 'RECORD LIST FILE CLOSED'                        ELP013  
00263                                  UPON SYSOUT                      ELP013  
00264      ELSE                                                         ELP013  
00265          DISPLAY 'RECORD LIST FILE STATUS CLOSE RETURN '          ELP013  
00266                  RL-STATUS                                        ELP013  
00267                                  UPON SYSOUT.                     ELP013  
00268                                                                   ELP013  
00269      IF DE-STATUS IS EQUAL TO '00'                                ELP013  
00270      THEN                                                         ELP013  
00271          DISPLAY 'DATA ELEMENT FILE CLOSED'                       ELP013  
00272                                  UPON SYSOUT                      ELP013  
00273      ELSE                                                         ELP013  
00274          DISPLAY 'DATA ELEMENT FILE STATUS CLOSE RETURN '         ELP013  
00275                  DE-STATUS                                        ELP013  
00276                                  UPON SYSOUT.                     ELP013  
00277                                                                   ELP013  
00278      IF EN-STATUS IS EQUAL TO '00'                                ELP013  
00279      THEN                                                         ELP013  
00280          DISPLAY 'ENGLISH NAME FILE CLOSED'                       ELP013  
00281                                  UPON SYSOUT                      ELP013  
00282      ELSE                                                         ELP013  
00283          DISPLAY 'ENGLISH NAME FILE STATUS CLOSE RETURN '         ELP013  
00284                  EN-STATUS                                        ELP013  
00285                                  UPON SYSOUT.                     ELP013  
00286                                                                   ELP013  
00287      IF CN-STATUS IS EQUAL TO '00'                                ELP013  
00288      THEN                                                         ELP013  
00289          DISPLAY 'SYSTEM NAME FILE CLOSED'                        ELP013  
00290                                  UPON SYSOUT                      ELP013  
00291      ELSE                                                         ELP013  
00292          DISPLAY 'SYSTEM NAME FILE STATUS CLOSE RETURN '          ELP013  
00293                  CN-STATUS                                        ELP013  
00294                                  UPON SYSOUT.                     ELP013  
00295                                                                   ELP013  
00296      IF CV-STATUS IS EQUAL TO '00'                                ELP013  
00297      THEN                                                         ELP013  
00298          DISPLAY 'CODE VALUES FILE CLOSED'                        ELP013  
00299                                  UPON SYSOUT                      ELP013  
00300      ELSE                                                         ELP013  
00301          DISPLAY 'CODE VALUES FILE STATUS CLOSE RETURN '          ELP013  
00302                  CV-STATUS                                        ELP013  
00303                                  UPON SYSOUT.                     ELP013  
