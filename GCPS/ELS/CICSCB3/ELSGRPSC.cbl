00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELSGRPSC
00003  PROGRAM-ID.         ELSGRPSC.                                       LV002
00004                                                                   ELSGRPSC
00005  AUTHOR.             NINA CERVANTES.                              ELSGRPSC
00006                                                                   ELSGRPSC
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSGRPSC
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELSGRPSC
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSGRPSC
00010                      233 N. MICHIGAN AVE                          ELSGRPSC
00011                      CHICAGO, ILLINOIS 60601                      ELSGRPSC
00012                                                                   ELSGRPSC
00013  DATE-WRITTEN.       28-OCT-1986.                                 ELSGRPSC
00014                                                                   ELSGRPSC
00015  DATE-COMPILED.                                                   ELSGRPSC
00016                                                                   ELSGRPSC
00017  SECURITY.           COPYRIGHT 1986,                              ELSGRPSC
00018                      HEALTH CARE SERVICE CORPORATION              ELSGRPSC
00019 /************************************************************     ELSGRPSC
00020 *                                                           *     ELSGRPSC
00021 *        PROGRAMMING CHANGES                                *     ELSGRPSC
00022 *                                                           *     ELSGRPSC
00023 *************************************************************     ELSGRPSC
00024 *                                                           *     ELSGRPSC
00025 *  CONVERTED FROM STRUCTURES                   4/10/89  EGL *     ELSGRPSC
00026 *                                                           *     ELSGRPSC
00027 *  STORAGE MANAGEMENT CONVERSION               4/12/89  GEM *     ELSGRPSC
00028 *                                                           *     ELSGRPSC
00029 *  DELETED 'SECTION NOT VIEWABLE' MESSAGE PROCESS 2/1/90 RJL*     ELSGRPSC
00030 *                                                           *     ELSGRPSC
00031 * DISCREPANCY : D02171                           3/18/91 RKH*     ELSGRPSC
00032 * ADDED LOGIC TO READ THE DATES FILE FOR THE CURRENT GROUP  *     ELSGRPSC
00033 * SPECIFIC RECORD KEY.  THIS KEY WILL THE BE USED TO READ   *     ELSGRPSC
00034 * THE GROUP SPECIFIC FILE FOR THE SECTION NAME.             *     ELSGRPSC
00035 *                                                           *     ELSGRPSC
00036 *  ADDED SUPPORT FOR YEAR 2000 AND TEXAS MERGER.            *     ELSGRPSC
00037 *                                              10/15/97 AKK *     ELSGRPSC
00038 *                                                           *     ELSGRPSC
00039 *  GEN'D FOR TESTING ORDRER OF COMPILE                      *     ELSGRPSC
00040 *                                              08/13/03 AKK *     ELSGRPSC
00041 *                                                           *     ELSGRPSC
00042 *************************************************************     ELSGRPSC
00043      SKIP3                                                        ELSGRPSC
00044  ENVIRONMENT DIVISION.                                            ELSGRPSC
00045                                                                   ELSGRPSC
00046  CONFIGURATION SECTION.                                           ELSGRPSC
00047  SOURCE-COMPUTER.    IBM-3033.                                    ELSGRPSC
00048  OBJECT-COMPUTER.    IBM-3033.                                    ELSGRPSC
00049      EJECT                                                        ELSGRPSC
00050  DATA DIVISION.                                                   ELSGRPSC
00051                                                                   ELSGRPSC
00052  FILE SECTION.                                                    ELSGRPSC
00053                                                                   ELSGRPSC
00054  WORKING-STORAGE SECTION.                                         ELSGRPSC
00055  77  FILLER                  PIC X(29)   VALUE                    ELSGRPSC
00056      '***ELSGRPSC WS BEGINS HERE***'.                             ELSGRPSC
00057                                                                   ELSGRPSC
00058  01  WS-SECT-PKG-INFO.                                            ELSGRPSC
00059      05  WS-PKG-INFO         PIC X(03).                           ELSGRPSC
00060      05  WS-SECTION-INFO     PIC X(05).                           ELSGRPSC
00061                                                                   ELSGRPSC
00062  01  SUB1                    PIC S9(4)   COMP VALUE +0.           ELSGRPSC
00063  01  SUB2                    PIC S9(4)   COMP VALUE +0.           ELSGRPSC
00064  01  WS-MENU-TITLE           PIC X(24)   VALUE                    ELSGRPSC
00065      'SELECT SECTION FOR GROUP'.                                  ELSGRPSC
00066                                                                   ELSGRPSC
00067  01  MAX-HEADER-LNS          PIC S9(4)   COMP VALUE +8.           ELSGRPSC
00068  01  WS-HEADINGS.                                                 ELSGRPSC
00069      02  WS-HEADING-LINES.                                        ELSGRPSC
00070                                                                   ELSGRPSC
00071          03  WS-HDR-LNA      PIC X(79)   VALUE  '**** TEXAS USERS ELSGRPSC
00072 -    'MUST ENTER A PACKAGE CODE AND SECTION NUMBER. ****'.        ELSGRPSC
00073          03  WS-HDR-LNB      PIC X(79)   VALUE  SPACES.           ELSGRPSC
00074                                                                   ELSGRPSC
00075          03  WS-HDR-LN1      PIC X(79)   VALUE   'NO SECTION NUMBEELSGRPSC
00076 -    'R WAS ENTERED AND MORE THAN ONE SECTION EXISTS FOR THIS GROUELSGRPSC
00077 -    'P.'.                                                        ELSGRPSC
00078          03  WS-HDR-LN2      PIC X(79)   VALUE   'PLEASE SELECT THELSGRPSC
00079 -    'E SECTION NUMBER YOU WISH TO SEE FOR THE INQUIRY YOU ARE'.  ELSGRPSC
00080          03  WS-HDR-LN3      PIC X(79)   VALUE   'PROCESSING NOW. ELSGRPSC
00081 -    ' YOU MAY RETURN TO THIS MENU LATER VIA PF3 TO SELECT ANOTHERELSGRPSC
00082 -    ''.                                                          ELSGRPSC
00083          03  WS-HDR-LN4      PIC X(79)   VALUE 'SECTION.'.        ELSGRPSC
00084          03  WS-HDR-LN5      PIC X(79)   VALUE  SPACES.           ELSGRPSC
00085          03  WS-HDR-LN6      PIC X(79)   VALUE                    ELSGRPSC
00086      'PACKAGE CODE    SECTION DESCRIPTION'.                       ELSGRPSC
00087                                                                   ELSGRPSC
00088      02  FILLER  REDEFINES  WS-HEADING-LINES.                     ELSGRPSC
00089          03  WS-HDR-LN  OCCURS 8 TIMES  PIC X(79).                ELSGRPSC
00090                                                                   ELSGRPSC
00091  01  GROUP-TYPE              PIC X      VALUE 'G'.                ELSGRPSC
00092  01  MAX-DETAIL-LNS          PIC S9(4)   COMP  VALUE +2.          ELSGRPSC
00093  01  WS-DTL-LN.                                                   ELSGRPSC
00094      03  FILLER              PIC X(5)   VALUE SPACES.             ELSGRPSC
00095      03  WS-PKG-CODE         PIC X(3).                            ELSGRPSC
00096      03  FILLER              PIC X(6)   VALUE SPACES.             ELSGRPSC
00097      03  WS-SECTION          PIC X(5).                            ELSGRPSC
00098      03  FILLER              PIC X      VALUE SPACES.             ELSGRPSC
00099      03  WS-DESCRIPTION      PIC X(50).                           ELSGRPSC
00100                                                                   ELSGRPSC
00101 /                                                                 ELSGRPSC
00102  LINKAGE SECTION.                                                 ELSGRPSC
00103                                                                   ELSGRPSC
00104  01  DFHCOMMAREA.                                                 ELSGRPSC
00105      COPY ELSCOMMC.                                               ELSGRPSC
00106 /                                                                 ELSGRPSC
00107      COPY ELSCIA2C.                                               ELSGRPSC
00108 /                                                                 ELSGRPSC
00109      COPY ELSSSCBC.                                               ELSGRPSC
00110 /                                                                 ELSGRPSC
00111      COPY ELSIOPMC.                                               ELSGRPSC
00112 /                                                                 ELSGRPSC
00113      COPY ELSKEYSC.                                               ELSGRPSC
00114 /                                                                 ELSGRPSC
00115      COPY ELSKTBSC.                                               ELSGRPSC
00116 /                                                                 ELSGRPSC
00117      COPY ELSMHDGC.                                               ELSGRPSC
00118 /                                                                 ELSGRPSC
00119      COPY ELSMOPTC.                                               ELSGRPSC
00120 /                                                                 ELSGRPSC
00121      COPY ELSMENUC.                                               ELSGRPSC
00122 /                                                                 ELSGRPSC
00123  01  GROUP-SPECIFIC.                                              ELSGRPSC
00124      COPY GCGROUPC.                                               ELSGRPSC
00125 /                                                                 ELSGRPSC
00126  01  DATES-RECORD.                                                ELSGRPSC
00127      COPY GCDATESC.                                               ELSGRPSC
00128                                                                   ELSGRPSC
00129 /***********************************************************      ELSGRPSC
00130 *                                                          *      ELSGRPSC
00131 *                    PROCEDURE DIVISION                    *      ELSGRPSC
00132 *                                                          *      ELSGRPSC
00133 ************************************************************      ELSGRPSC
00134  PROCEDURE DIVISION.                                              ELSGRPSC
00135                                                                   ELSGRPSC
00136                                                                   ELSGRPSC
00137 ************************************************************      ELSGRPSC
00138 *                                                          *      ELSGRPSC
00139 *        GROUP SECTION SELECTOR                            *      ELSGRPSC
00140 *                                                          *      ELSGRPSC
00141 ************************************************************      ELSGRPSC
00142  GROUP-SECTION-SELECTOR.                                          ELSGRPSC
00143      PERFORM INITIALIZE-MODULE.                                   ELSGRPSC
00144      PERFORM DETERMINE-MODULE-STATUS.                             ELSGRPSC
00145 *---> RETURN-TO-CALLER                                            ELSGRPSC
00146      EXEC CICS RETURN                                             ELSGRPSC
00147                END-EXEC.                                          ELSGRPSC
00148      GOBACK.                                                      ELSGRPSC
00149                                                                   ELSGRPSC
00150 /***********************************************************      ELSGRPSC
00151 *                                                          *      ELSGRPSC
00152 *        INITIALIZE MODULE                                 *      ELSGRPSC
00153 *                                                          *      ELSGRPSC
00154 ************************************************************      ELSGRPSC
00155  INITIALIZE-MODULE.                                               ELSGRPSC
00156      PERFORM CHECK-COMMAREA-LENGTH.                               ELSGRPSC
00157      PERFORM ESTABLISH-ADDRESSING-TO-COMMON.                      ELSGRPSC
00158                                                                   ELSGRPSC
00159                                                                   ELSGRPSC
00160 ************************************************************      ELSGRPSC
00161 *                                                          *      ELSGRPSC
00162 *        CHECK COMMAREA LENGTH                             *      ELSGRPSC
00163 *                                                          *      ELSGRPSC
00164 ************************************************************      ELSGRPSC
00165  CHECK-COMMAREA-LENGTH.                                           ELSGRPSC
00166      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELSGRPSC
00167         SET CIA-AB-DFHCOMMAREA TO TRUE                            ELSGRPSC
00168         PERFORM EXEC-CICS-ABEND.                                  ELSGRPSC
00169                                                                   ELSGRPSC
00170 ************************************************************      ELSGRPSC
00171 *                                                          *      ELSGRPSC
00172 *        ESTABLISH ADDRESSING TO COMMON INTERFACE AREA     *      ELSGRPSC
00173 *                                                          *      ELSGRPSC
00174 ************************************************************      ELSGRPSC
00175  ESTABLISH-ADDRESSING-TO-COMMON.                                  ELSGRPSC
00176      CALL 'ELUINISM' USING DFHCOMMAREA                            ELSGRPSC
00177          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELSGRPSC
00178                                                                   ELSGRPSC
00179      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSGRPSC
00180      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSC
00181          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELSGRPSC
00182                                                                   ELSGRPSC
00183 /***********************************************************      ELSGRPSC
00184 *                                                          *      ELSGRPSC
00185 *        DETERMINE MODULE STATUS                           *      ELSGRPSC
00186 *                                                          *      ELSGRPSC
00187 ************************************************************      ELSGRPSC
00188  DETERMINE-MODULE-STATUS.                                         ELSGRPSC
00189      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSGRPSC
00190          PERFORM INITIAL-CALL-FOR-SECTION-MENU                    ELSGRPSC
00191      ELSE                                                         ELSGRPSC
00192          IF SSB-MENU-COMPLETE (SSB-SELECTOR-STATE)                ELSGRPSC
00193             PERFORM COMPLETE-PROCESS-FOR-SECTION-M                ELSGRPSC
00194          ELSE                                                     ELSGRPSC
00195             SET CIA-AB-UNDEF TO TRUE                              ELSGRPSC
00196             PERFORM EXEC-CICS-ABEND.                              ELSGRPSC
00197                                                                   ELSGRPSC
00198 /***********************************************************      ELSGRPSC
00199 *                                                          *      ELSGRPSC
00200 *        INITIAL CALL FOR SECTION MENU                     *      ELSGRPSC
00201 *                                                          *      ELSGRPSC
00202 ************************************************************      ELSGRPSC
00203  INITIAL-CALL-FOR-SECTION-MENU.                                   ELSGRPSC
00204      PERFORM PREPARE-STORAGE-AREAS.                               ELSGRPSC
00205      PERFORM BUILD-MENU.                                          ELSGRPSC
00206                                                                   ELSGRPSC
00207 ************************************************************      ELSGRPSC
00208 *                                                          *      ELSGRPSC
00209 *        PREPARE STORAGE AREAS                             *      ELSGRPSC
00210 *                                                          *      ELSGRPSC
00211 ************************************************************      ELSGRPSC
00212  PREPARE-STORAGE-AREAS.                                           ELSGRPSC
00213      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGRPSC
00214      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSC
00215          ADDRESS OF GROUP-SPECIFIC.                               ELSGRPSC
00216      IF CIA-RC-PTR-NULL                                           ELSGRPSC
00217          PERFORM ALLOCATE-IOPARM-BLOCK.                           ELSGRPSC
00218                                                                   ELSGRPSC
00219      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELSGRPSC
00220      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSC
00221          ADDRESS OF GROUP-SPECIFIC.                               ELSGRPSC
00222      IF CIA-RC-PTR-NULL                                           ELSGRPSC
00223          PERFORM ALLOCATE-KEYS-BLOCK.                             ELSGRPSC
00224                                                                   ELSGRPSC
00225      PERFORM DELETE-ELSMENU-AREA.                                 ELSGRPSC
00226                                                                   ELSGRPSC
00227      SET CIA-ELSKTBS-DDN TO TRUE.                                 ELSGRPSC
00228      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSC
00229          ADDRESS OF GROUP-SPECIFIC.                               ELSGRPSC
00230      IF CIA-RC-PTR-NULL                                           ELSGRPSC
00231          PERFORM RETRIEVE-SECTION-KEY-TABLE.                      ELSGRPSC
00232                                                                   ELSGRPSC
00233      SET CIA-ELSKTBS-DDN TO TRUE.                                 ELSGRPSC
00234      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSC
00235          ADDRESS OF KTS-SECTIONS-KEY-TABLE.                       ELSGRPSC
00236                                                                   ELSGRPSC
00237      MOVE WS-MENU-TITLE TO SSB-MNU-TITLE.                         ELSGRPSC
00238      PERFORM INITIALIZE-MENU-TABLES.                              ELSGRPSC
00239                                                                   ELSGRPSC
00240 /***********************************************************      ELSGRPSC
00241 *                                                          *      ELSGRPSC
00242 *        ALLOCATE IOPARM BLOCK                             *      ELSGRPSC
00243 *                                                          *      ELSGRPSC
00244 ************************************************************      ELSGRPSC
00245  ALLOCATE-IOPARM-BLOCK.                                           ELSGRPSC
00246      SET CIA-ELSMENU-DDN    TO  TRUE.                             ELSGRPSC
00247      SET CIA-STG-GETMAIN    TO  TRUE.                             ELSGRPSC
00248      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSGRPSC
00249                                                                   ELSGRPSC
00250 ************************************************************      ELSGRPSC
00251 *                                                          *      ELSGRPSC
00252 *        ALLOCATE KEYS BLOCK                               *      ELSGRPSC
00253 *                                                          *      ELSGRPSC
00254 ************************************************************      ELSGRPSC
00255  ALLOCATE-KEYS-BLOCK.                                             ELSGRPSC
00256      SET CIA-ELSKEYS-DDN    TO  TRUE.                             ELSGRPSC
00257      SET CIA-STG-GETMAIN    TO  TRUE.                             ELSGRPSC
00258      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSGRPSC
00259                                                                   ELSGRPSC
00260 ************************************************************      ELSGRPSC
00261 *                                                          *      ELSGRPSC
00262 *        DELETE ELSMENU AREA                               *      ELSGRPSC
00263 *                                                          *      ELSGRPSC
00264 ************************************************************      ELSGRPSC
00265  DELETE-ELSMENU-AREA.                                             ELSGRPSC
00266      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGRPSC
00267      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSC
00268          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSGRPSC
00269      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGRPSC
00270      SET IOP-DEL TO TRUE.                                         ELSGRPSC
00271      SET IOP-FCQ-NONE TO TRUE.                                    ELSGRPSC
00272      SET IOP-KVQ-NONE TO TRUE.                                    ELSGRPSC
00273      PERFORM LINK-TO-ELUIOPGM.                                    ELSGRPSC
00274                                                                   ELSGRPSC
00275 ************************************************************      ELSGRPSC
00276 *                                                          *      ELSGRPSC
00277 *        RETRIEVE SECTION KEY TABLE                        *      ELSGRPSC
00278 *                                                          *      ELSGRPSC
00279 ************************************************************      ELSGRPSC
00280  RETRIEVE-SECTION-KEY-TABLE.                                      ELSGRPSC
00281      SET CIA-ELSKTBS-DDN   TO  TRUE.                              ELSGRPSC
00282      SET CIA-STG-RETRIEVE  TO  TRUE.                              ELSGRPSC
00283      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSGRPSC
00284                                                                   ELSGRPSC
00285                                                                   ELSGRPSC
00286 /***********************************************************      ELSGRPSC
00287 *                                                          *      ELSGRPSC
00288 *        LINK TO ELUIOPGM                                  *      ELSGRPSC
00289 *                                                          *      ELSGRPSC
00290 ************************************************************      ELSGRPSC
00291  LINK-TO-ELUIOPGM.                                                ELSGRPSC
00292      EXEC CICS LINK                                               ELSGRPSC
00293                PROGRAM('ELUIOPGM')                                ELSGRPSC
00294                COMMAREA(DFHCOMMAREA)                              ELSGRPSC
00295                END-EXEC.                                          ELSGRPSC
00296                                                                   ELSGRPSC
00297 ************************************************************      ELSGRPSC
00298 *                                                          *      ELSGRPSC
00299 *        LINK TO STORAGE MANAGER                           *      ELSGRPSC
00300 *                                                          *      ELSGRPSC
00301 ************************************************************      ELSGRPSC
00302  LINK-TO-STORAGE-MANAGER.                                         ELSGRPSC
00303      EXEC CICS LINK                                               ELSGRPSC
00304                PROGRAM ('ELUSTGMG')                               ELSGRPSC
00305                COMMAREA(DFHCOMMAREA)                              ELSGRPSC
00306                END-EXEC.                                          ELSGRPSC
00307                                                                   ELSGRPSC
00308 /***********************************************************      ELSGRPSC
00309 *                                                          *      ELSGRPSC
00310 *        INITIALIZE MENU TABLES                            *      ELSGRPSC
00311 *                                                          *      ELSGRPSC
00312 ************************************************************      ELSGRPSC
00313  INITIALIZE-MENU-TABLES.                                          ELSGRPSC
00314      PERFORM ALLOCATE-MENU-OPTIONS-AREA.                          ELSGRPSC
00315      PERFORM ALLOCATE-MENU-HEADINGS-AREA.                         ELSGRPSC
00316      PERFORM ALLOCATE-MENU-DESCRIPTIONS-ARE.                      ELSGRPSC
00317      INITIALIZE SSB-MNU-CHOICE (1).                               ELSGRPSC
00318                                                                   ELSGRPSC
00319                                                                   ELSGRPSC
00320 ************************************************************      ELSGRPSC
00321 *                                                          *      ELSGRPSC
00322 *        ALLOCATE MENU OPTIONS AREA                        *      ELSGRPSC
00323 *                                                          *      ELSGRPSC
00324 ************************************************************      ELSGRPSC
00325  ALLOCATE-MENU-OPTIONS-AREA.                                      ELSGRPSC
00326      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSGRPSC
00327      SET CIA-STG-GETMAIN TO TRUE.                                 ELSGRPSC
00328      COMPUTE CIA-AREA-LEN =                                       ELSGRPSC
00329              LENGTH OF MSO-MENU-OPTS-HEADER  +                    ELSGRPSC
00330             (LENGTH OF MSO-MENU-OPT * KTS-NBR-KEYS).              ELSGRPSC
00331      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSGRPSC
00332                                                                   ELSGRPSC
00333      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSGRPSC
00334      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSC
00335          ADDRESS OF MSO-MENU-SELECTION-VALUES.                    ELSGRPSC
00336                                                                   ELSGRPSC
00337 ************************************************************      ELSGRPSC
00338 *                                                          *      ELSGRPSC
00339 *        ALLOCATE MENU HEADINGS AREA                       *      ELSGRPSC
00340 *                                                          *      ELSGRPSC
00341 ************************************************************      ELSGRPSC
00342  ALLOCATE-MENU-HEADINGS-AREA.                                     ELSGRPSC
00343      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSGRPSC
00344      SET CIA-STG-GETMAIN TO TRUE.                                 ELSGRPSC
00345      COMPUTE CIA-AREA-LEN =                                       ELSGRPSC
00346              LENGTH OF MHD-NBR-HDG-LINES +                        ELSGRPSC
00347             (MAX-HEADER-LNS  *  LENGTH OF MHD-HDG-LINE).          ELSGRPSC
00348      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSGRPSC
00349                                                                   ELSGRPSC
00350      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSGRPSC
00351      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSC
00352          ADDRESS OF MHD-MENU-HEADINGS.                            ELSGRPSC
00353                                                                   ELSGRPSC
00354 ************************************************************      ELSGRPSC
00355 *                                                          *      ELSGRPSC
00356 *        ALLOCATE MENU DESCRIPTIONS AREA                   *      ELSGRPSC
00357 *                                                          *      ELSGRPSC
00358 ************************************************************      ELSGRPSC
00359  ALLOCATE-MENU-DESCRIPTIONS-ARE.                                  ELSGRPSC
00360      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGRPSC
00361      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSC
00362          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSGRPSC
00363                                                                   ELSGRPSC
00364      SET CIA-ELSMENU-DDN     TO TRUE.                             ELSGRPSC
00365      SET IOP-GETMAIN-REC     TO TRUE.                             ELSGRPSC
00366      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELSGRPSC
00367      COMPUTE IOP-REC-LEN =                                        ELSGRPSC
00368              LENGTH OF MSD-NBR-DESCR-LINES +                      ELSGRPSC
00369             (MAX-DETAIL-LNS  * LENGTH OF MSD-DESCR-LINE).         ELSGRPSC
00370                                                                   ELSGRPSC
00371      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSGRPSC
00372                                                                   ELSGRPSC
00373      SET ADDRESS OF MSD-MENU-ITEM-DESCRIPTIONS                    ELSGRPSC
00374                  TO IOP-REC-PTR.                                  ELSGRPSC
00375                                                                   ELSGRPSC
00376 /************************************************************     ELSGRPSC
00377 *                                                          *      ELSGRPSC
00378 *        BUILD MENU                                        *      ELSGRPSC
00379 *                                                          *      ELSGRPSC
00380 ************************************************************      ELSGRPSC
00381  BUILD-MENU.                                                      ELSGRPSC
00382      PERFORM CREATE-SECTION-MENU-HEADER.                          ELSGRPSC
00383      PERFORM CREATE-MENU-OPTIONS-TABLE.                           ELSGRPSC
00384      PERFORM CREATE-SECTION-MENU-DETAIL.                          ELSGRPSC
00385      SET SSB-START-MENU (SSB-SELECTOR-STATE) TO TRUE.             ELSGRPSC
00386                                                                   ELSGRPSC
00387 ************************************************************      ELSGRPSC
00388 *                                                          *      ELSGRPSC
00389 *        CREATE SECTION MENU HEADER                        *      ELSGRPSC
00390 *                                                          *      ELSGRPSC
00391 ************************************************************      ELSGRPSC
00392  CREATE-SECTION-MENU-HEADER.                                      ELSGRPSC
00393      MOVE MAX-HEADER-LNS TO MHD-NBR-HDG-LINES.                    ELSGRPSC
00394      PERFORM LOAD-HEADER-TABLE                                    ELSGRPSC
00395          VARYING MHD-IDX FROM 1 BY 1                              ELSGRPSC
00396                    UNTIL MHD-IDX GREATER THAN                     ELSGRPSC
00397              MAX-HEADER-LNS.                                      ELSGRPSC
00398                                                                   ELSGRPSC
00399 ************************************************************      ELSGRPSC
00400 *                                                          *      ELSGRPSC
00401 *        LOAD HEADER TABLE                                 *      ELSGRPSC
00402 *                                                          *      ELSGRPSC
00403 ************************************************************      ELSGRPSC
00404  LOAD-HEADER-TABLE.                                               ELSGRPSC
00405      SET SUB1 TO MHD-IDX.                                         ELSGRPSC
00406      MOVE WS-HDR-LN (SUB1) TO MHD-HDG-LINE (MHD-IDX).             ELSGRPSC
00407                                                                   ELSGRPSC
00408 ************************************************************      ELSGRPSC
00409 *                                                          *      ELSGRPSC
00410 *        CREATE MENU OPTIONS TABLE                         *      ELSGRPSC
00411 *                                                          *      ELSGRPSC
00412 ************************************************************      ELSGRPSC
00413  CREATE-MENU-OPTIONS-TABLE.                                       ELSGRPSC
00414      MOVE LENGTH OF GCG-SECTION-NUM TO MSO-OPT-LEN.               ELSGRPSC
00415      SET MSO-OPT-TYP-AN          TO TRUE.                         ELSGRPSC
00416      MOVE KTS-NBR-KEYS           TO MSO-NBR-MENU-OPTS.            ELSGRPSC
00417      MOVE 1                      TO MSO-MIN-CHOICES               ELSGRPSC
00418                                     MSO-MAX-CHOICES.              ELSGRPSC
00419                                                                   ELSGRPSC
00420 /***********************************************************      ELSGRPSC
00421 *                                                          *      ELSGRPSC
00422 *        CREATE SECTION MENU DETAIL                        *      ELSGRPSC
00423 *                                                          *      ELSGRPSC
00424 ************************************************************      ELSGRPSC
00425  CREATE-SECTION-MENU-DETAIL.                                      ELSGRPSC
00426      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELSGRPSC
00427      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSC
00428          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELSGRPSC
00429                                                                   ELSGRPSC
00430      SET CIA-GCGRPSPC-DDN TO TRUE.                                ELSGRPSC
00431      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSC
00432          ADDRESS OF GROUP-SPECIFIC.                               ELSGRPSC
00433      IF CIA-RC-PTR-NULL                                           ELSGRPSC
00434          PERFORM ALLOCATE-GROUP-SPECIFIC-IOPARM.                  ELSGRPSC
00435                                                                   ELSGRPSC
00436      SET CIA-GCDATES-DDN TO TRUE.                                 ELSGRPSC
00437      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSC
00438          ADDRESS OF DATES-RECORD.                                 ELSGRPSC
00439      IF CIA-RC-PTR-NULL                                           ELSGRPSC
00440          PERFORM ALLOCATE-DATES-RECORD-IOPARM.                    ELSGRPSC
00441                                                                   ELSGRPSC
00442      PERFORM DUMP-SECTION-TABLE                                   ELSGRPSC
00443          VARYING KTS-IDX FROM 1 BY 1                              ELSGRPSC
00444                    UNTIL KTS-IDX GREATER THAN KTS-NBR-KEYS.       ELSGRPSC
00445                                                                   ELSGRPSC
00446 /***********************************************************      ELSGRPSC
00447 *                                                          *      ELSGRPSC
00448 *        ALLOCATE GROUP SPECIFIC IOPARM AREA               *      ELSGRPSC
00449 *                                                          *      ELSGRPSC
00450 ************************************************************      ELSGRPSC
00451  ALLOCATE-GROUP-SPECIFIC-IOPARM.                                  ELSGRPSC
00452      SET CIA-GCGRPSPC-DDN   TO  TRUE.                             ELSGRPSC
00453      SET CIA-STG-GETMAIN    TO  TRUE.                             ELSGRPSC
00454      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSGRPSC
00455                                                                   ELSGRPSC
00456 ************************************************************      ELSGRPSC
00457 *                                                          *      ELSGRPSC
00458 *        ALLOCATE DATE RECORD IOPARM AREA                  *      ELSGRPSC
00459 *                                                          *      ELSGRPSC
00460 ************************************************************      ELSGRPSC
00461  ALLOCATE-DATES-RECORD-IOPARM.                                    ELSGRPSC
00462      SET CIA-GCDATES-DDN    TO  TRUE.                             ELSGRPSC
00463      SET CIA-STG-GETMAIN    TO  TRUE.                             ELSGRPSC
00464      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSGRPSC
00465                                                                   ELSGRPSC
00466 /***********************************************************      ELSGRPSC
00467 *                                                          *      ELSGRPSC
00468 *        DUMP SECTION TABLE                                *      ELSGRPSC
00469 *                                                          *      ELSGRPSC
00470 ************************************************************      ELSGRPSC
00471  DUMP-SECTION-TABLE.                                              ELSGRPSC
00472                                                                   ELSGRPSC
00473      MOVE LOW-VALUES             TO KWA-GCDATES-KEY.              ELSGRPSC
00474      MOVE SSB-PLAN-CODE          TO KWA-GCDATES-PLAN-CODE.        ELSGRPSC
00475      MOVE SSB-GROUP-NUMBER   TO KWA-GCDATES-GROUP-NUMBER.         ELSGRPSC
00476      MOVE KTS-SECTION-NUMBER (KTS-IDX)                            ELSGRPSC
00477                           TO KWA-GCDATES-SECTION-NUMBER.          ELSGRPSC
00478      MOVE KTS-PKG-CODE (KTS-IDX) TO KWA-GCDATES-PKG-CODE.         ELSGRPSC
00479      MOVE GROUP-TYPE             TO KWA-GCDATES-FILE-REF.         ELSGRPSC
00480      PERFORM ISSUE-READ-DATES.                                    ELSGRPSC
00481                                                                   ELSGRPSC
00482      MOVE LOW-VALUES             TO KWA-GCGRPSPC-KEY.             ELSGRPSC
00483      PERFORM ISSUE-READ-GROUP.                                    ELSGRPSC
00484                                                                   ELSGRPSC
00485      SET SUB1 TO KTS-IDX.                                         ELSGRPSC
00486      PERFORM PUT-SECTION-NUMBER-IN-OPTION-S.                      ELSGRPSC
00487      PERFORM CREATE-ITEM-DESCRIPTION-PER-SE.                      ELSGRPSC
00488                                                                   ELSGRPSC
00489 /***********************************************************      ELSGRPSC
00490 *                                                          *      ELSGRPSC
00491 *        ISSUE READ FOR DATES RECORD                       *      ELSGRPSC
00492 *                                                          *      ELSGRPSC
00493 ************************************************************      ELSGRPSC
00494  ISSUE-READ-DATES.                                                ELSGRPSC
00495      SET CIA-GCDATES-DDN TO TRUE.                                 ELSGRPSC
00496      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSC
00497          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSGRPSC
00498                                                                   ELSGRPSC
00499      MOVE KWA-GCDATES-KEY     TO IOP-FILE-KEY.                    ELSGRPSC
00500      SET CIA-GCDATES-DDN      TO TRUE.                            ELSGRPSC
00501      SET IOP-RD               TO TRUE.                            ELSGRPSC
00502      SET IOP-FCQ-NONE         TO TRUE.                            ELSGRPSC
00503      SET IOP-KVQ-EQ           TO TRUE.                            ELSGRPSC
00504      SET IOP-STG-MODE-LOCATE  TO TRUE.                            ELSGRPSC
00505      COMPUTE IOP-KEY-LEN = LENGTH OF KWA-GCDATES-GROUP-NUMBER +   ELSGRPSC
00506                            LENGTH OF KWA-GCDATES-PLAN-CODE    +   ELSGRPSC
00507                            LENGTH OF KWA-GCDATES-SECTION-NUMBER + ELSGRPSC
00508                            LENGTH OF KWA-GCDATES-L-O-B    +       ELSGRPSC
00509                            LENGTH OF KWA-GCDATES-PKG-CODE    +    ELSGRPSC
00510                            LENGTH OF KWA-GCDATES-FILLER   +       ELSGRPSC
00511                            LENGTH OF KWA-GCDATES-FILE-REF.        ELSGRPSC
00512      PERFORM LINK-TO-ELUIOPGM.                                    ELSGRPSC
00513                                                                   ELSGRPSC
00514      IF IOP-RC-NOTFND                                             ELSGRPSC
00515          SET CIA-AB-NOTFND-GCDATES  TO TRUE                       ELSGRPSC
00516          PERFORM EXEC-CICS-ABEND                                  ELSGRPSC
00517      ELSE                                                         ELSGRPSC
00518           IF NOT IOP-RC-OK                                        ELSGRPSC
00519              SET CIA-AB-CRITIO TO TRUE                            ELSGRPSC
00520              PERFORM EXEC-CICS-ABEND.                             ELSGRPSC
00521                                                                   ELSGRPSC
00522      SET ADDRESS OF DATES-RECORD       TO IOP-REC-PTR.            ELSGRPSC
00523                                                                   ELSGRPSC
00524 /***********************************************************      ELSGRPSC
00525 *                                                          *      ELSGRPSC
00526 *        ISSUE READ FOR GROUP SPECIFIC RECORD              *      ELSGRPSC
00527 *                                                          *      ELSGRPSC
00528 ************************************************************      ELSGRPSC
00529  ISSUE-READ-GROUP.                                                ELSGRPSC
00530      SET CIA-GCGRPSPC-DDN TO TRUE.                                ELSGRPSC
00531      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSC
00532          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSGRPSC
00533 *-->                                                              ELSGRPSC
00534 *--> SETUP KEY FROM DATES RECORD                                  ELSGRPSC
00535 *-->                                                              ELSGRPSC
00536      MOVE DTE-GROUP-PLAN-CODE         TO  KWA-GCG-PLAN-CODE.      ELSGRPSC
00537      MOVE DTE-GROUP-GROUP-NUM         TO  KWA-GCG-GROUP-NUMBER.   ELSGRPSC
00538      MOVE DTE-GROUP-SECTION-NUM       TO  KWA-GCG-SECTION-NUMBER. ELSGRPSC
00539      MOVE DTE-GROUP-PKG-CODE          TO  KWA-GCG-PKG-CODE.       ELSGRPSC
00540      MOVE DTE-FAMILY-RELAT-LEVEL (1)  TO  KWA-GCG-FAM-REL-LVL.    ELSGRPSC
00541      MOVE DTE-EFFDT-CEN (1)                                       ELSGRPSC
00542                                 TO  KWA-GCG-EFF-DATE-CENTURY.     ELSGRPSC
00543      MOVE KWA-GCGRPSPC-KEY            TO  IOP-FILE-KEY.           ELSGRPSC
00544                                                                   ELSGRPSC
00545      SET CIA-GCGRPSPC-DDN             TO TRUE.                    ELSGRPSC
00546      COMPUTE IOP-KEY-LEN = LENGTH OF KWA-GCG-GROUP-NUMBER +       ELSGRPSC
00547                            LENGTH OF KWA-GCG-SECTION-NUMBER +     ELSGRPSC
00548                            LENGTH OF KWA-GCG-PLAN-CODE +          ELSGRPSC
00549                            LENGTH OF KWA-GCG-PKG-CODE +           ELSGRPSC
00550                            LENGTH OF KWA-GCG-FAM-REL-LVL +        ELSGRPSC
00551                            LENGTH OF KWA-GCG-EFF-DATE-CENTURY.    ELSGRPSC
00552      SET IOP-RD                   TO TRUE.                        ELSGRPSC
00553      SET IOP-FCQ-NONE             TO TRUE.                        ELSGRPSC
00554      SET IOP-KVQ-EQ               TO TRUE.                        ELSGRPSC
00555      SET IOP-STG-MODE-LOCATE      TO TRUE.                        ELSGRPSC
00556      PERFORM LINK-TO-ELUIOPGM.                                    ELSGRPSC
00557                                                                   ELSGRPSC
00558      IF IOP-RC-NOTFND                                             ELSGRPSC
00559         SET CIA-AB-NOTFND-GCGRPSPC TO TRUE                        ELSGRPSC
00560         PERFORM EXEC-CICS-ABEND                                   ELSGRPSC
00561      ELSE                                                         ELSGRPSC
00562          IF NOT IOP-RC-OK                                         ELSGRPSC
00563             SET CIA-AB-CRITIO TO TRUE                             ELSGRPSC
00564             PERFORM EXEC-CICS-ABEND.                              ELSGRPSC
00565                                                                   ELSGRPSC
00566      SET ADDRESS OF GROUP-SPECIFIC     TO IOP-REC-PTR.            ELSGRPSC
00567                                                                   ELSGRPSC
00568 /***********************************************************      ELSGRPSC
00569 *                                                          *      ELSGRPSC
00570 *        PUT SECTION NUMBER IN OPTION SELECTOR             *      ELSGRPSC
00571 * FOR TX WHEN SECTN NUMBER ENTERED BUT DON'T KNOW PKG CODE *      ELSGRPSC
00572 * FORCE PGM TO ONLY PICK UP SECT/PKG CODES FOR DESIRED     *      ELSGRPSC
00573 * SECTIONS.                                                *      ELSGRPSC
00574 ************************************************************      ELSGRPSC
00575  PUT-SECTION-NUMBER-IN-OPTION-S.                                  ELSGRPSC
00576 **IF SECTION SELECTED ON MAIN ELS SCREEN                          ELSGRPSC
00577 **                                                                ELSGRPSC
00578      IF NOT SSB-NO-SECTN-NO                                       ELSGRPSC
00579         IF KTS-SECTION-NUMBER  (KTS-IDX) = SSB-SECTN-NO           ELSGRPSC
00580            MOVE KTS-PKG-CODE  (KTS-IDX) TO WS-PKG-INFO            ELSGRPSC
00581            MOVE KTS-SECTION-NUMBER  (KTS-IDX) TO WS-SECTION-INFO  ELSGRPSC
00582            MOVE WS-SECT-PKG-INFO TO MSO-OPT-KWD (SUB1)            ELSGRPSC
00583            MOVE KTS-SECTION-NUMBER  (KTS-IDX)                     ELSGRPSC
00584                                  TO MSO-OPT-SEL (SUB1)            ELSGRPSC
00585         ELSE                                                      ELSGRPSC
00586            NEXT SENTENCE                                          ELSGRPSC
00587         END-IF                                                    ELSGRPSC
00588      ELSE                                                         ELSGRPSC
00589          MOVE KTS-PKG-CODE  (KTS-IDX) TO WS-PKG-INFO              ELSGRPSC
00590          MOVE KTS-SECTION-NUMBER  (KTS-IDX) TO WS-SECTION-INFO    ELSGRPSC
00591          MOVE WS-SECT-PKG-INFO TO MSO-OPT-KWD (SUB1)              ELSGRPSC
00592          MOVE KTS-SECTION-NUMBER  (KTS-IDX) TO MSO-OPT-SEL (SUB1) ELSGRPSC
00593      END-IF.                                                      ELSGRPSC
00594 *                                          MSO-OPT-KWD (SUB1)     ELSGRPSC
00595 *    MOVE KTS-PKG-CODE (KTS-IDX) TO MSO-OPT-SEL (SUB2)            ELSGRPSC
00596 *                                   MSO-OPT-KWD (SUB2).           ELSGRPSC
00597                                                                   ELSGRPSC
00598 ************************************************************      ELSGRPSC
00599 *                                                          *      ELSGRPSC
00600 *        CREATE ITEM DESCRIPTION PER SECTION FOUND         *      ELSGRPSC
00601 * FOR TX WHEN SECTN NUMBER ENTERED BUT DON'T KNOW PKG CODE *      ELSGRPSC
00602 * FORCE PGM TO ONLY PICK UP ITEM DESCRIPTIONS FOR DESIRED  *      ELSGRPSC
00603 * SECTION. I.E. SECTION CHOSEN.                            *      ELSGRPSC
00604 ************************************************************      ELSGRPSC
00605  CREATE-ITEM-DESCRIPTION-PER-SE.                                  ELSGRPSC
00606 **IF SECTION SELECTED ON MAIN ELS SCREEN                          ELSGRPSC
00607 **                                                                ELSGRPSC
00608      IF NOT SSB-NO-SECTN-NO                                       ELSGRPSC
00609         IF GCG-SECTION-NUM = SSB-SECTN-NO                         ELSGRPSC
00610            MOVE GCG-SECTION-NUM        TO WS-SECTION              ELSGRPSC
00611            MOVE GCG-PKG-CODE           TO WS-PKG-CODE             ELSGRPSC
00612            MOVE GCG-GROUP-SECTION-NAME TO WS-DESCRIPTION          ELSGRPSC
00613            SET MSD-IDX                 TO 1                       ELSGRPSC
00614            SET MSD-NBR-DESCR-LINES     TO MSD-IDX                 ELSGRPSC
00615            MOVE WS-DTL-LN              TO MSD-DESCR-LINE(MSD-IDX) ELSGRPSC
00616            PERFORM ADD-ITEM-DESCRIPTION-TO-MENU                   ELSGRPSC
00617         ELSE                                                      ELSGRPSC
00618            NEXT SENTENCE                                          ELSGRPSC
00619         END-IF                                                    ELSGRPSC
00620      ELSE                                                         ELSGRPSC
00621         MOVE GCG-SECTION-NUM        TO WS-SECTION                 ELSGRPSC
00622         MOVE GCG-PKG-CODE           TO WS-PKG-CODE                ELSGRPSC
00623         MOVE GCG-GROUP-SECTION-NAME TO WS-DESCRIPTION             ELSGRPSC
00624         SET MSD-IDX                 TO 1                          ELSGRPSC
00625         SET MSD-NBR-DESCR-LINES     TO MSD-IDX                    ELSGRPSC
00626         MOVE WS-DTL-LN              TO MSD-DESCR-LINE(MSD-IDX)    ELSGRPSC
00627         PERFORM ADD-ITEM-DESCRIPTION-TO-MENU                      ELSGRPSC
00628      END-IF.                                                      ELSGRPSC
00629                                                                   ELSGRPSC
00630 ************************************************************      ELSGRPSC
00631 *                                                          *      ELSGRPSC
00632 *        ADD ITEM DESCRIPTION TO MENU                      *      ELSGRPSC
00633 *                                                          *      ELSGRPSC
00634 ************************************************************      ELSGRPSC
00635  ADD-ITEM-DESCRIPTION-TO-MENU.                                    ELSGRPSC
00636      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGRPSC
00637      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGRPSC
00638          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSGRPSC
00639                                                                   ELSGRPSC
00640      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGRPSC
00641      SET IOP-ADD         TO TRUE.                                 ELSGRPSC
00642      SET IOP-FCQ-NONE    TO TRUE.                                 ELSGRPSC
00643      SET IOP-KVQ-NONE    TO TRUE.                                 ELSGRPSC
00644      PERFORM LINK-TO-ELUIOPGM.                                    ELSGRPSC
00645                                                                   ELSGRPSC
00646 /***********************************************************      ELSGRPSC
00647 *                                                          *      ELSGRPSC
00648 *        COMPLETE PROCESS FOR SECTION MENU                 *      ELSGRPSC
00649 *                                                          *      ELSGRPSC
00650 ************************************************************      ELSGRPSC
00651  COMPLETE-PROCESS-FOR-SECTION-M.                                  ELSGRPSC
00652 *    MOVE SSB-MNU-CHOICE (1) TO SSB-SECTN-NO.                     ELSGRPSC
00653      MOVE SSB-MNU-CHOICE (1) TO WS-SECT-PKG-INFO.                 ELSGRPSC
00654      MOVE WS-SECTION-INFO TO SSB-SECTN-NO.                        ELSGRPSC
00655      MOVE WS-PKG-INFO TO SSB-PKG-CODE.                            ELSGRPSC
00656      EXEC CICS LINK                                               ELSGRPSC
00657                PROGRAM ('ELUKYTAB')                               ELSGRPSC
00658                COMMAREA(DFHCOMMAREA)                              ELSGRPSC
00659                END-EXEC.                                          ELSGRPSC
00660                                                                   ELSGRPSC
00661      IF CIA-RC-MEMB-NO-SECTN-INFO                                 ELSGRPSC
00662          SET CIA-RC-OK TO TRUE.                                   ELSGRPSC
00663                                                                   ELSGRPSC
00664      IF CIA-RC-KTB-GRP-NOTFND  OR                                 ELSGRPSC
00665         CIA-RC-KTB-SECTN-NOTFND                                   ELSGRPSC
00666             SET CIA-AB-CRITIO TO TRUE                             ELSGRPSC
00667             PERFORM EXEC-CICS-ABEND                               ELSGRPSC
00668      ELSE                                                         ELSGRPSC
00669         IF NOT CIA-RC-OK                                          ELSGRPSC
00670            SET CIA-AB-UNDEF  TO TRUE                              ELSGRPSC
00671            PERFORM EXEC-CICS-ABEND                                ELSGRPSC
00672         ELSE                                                      ELSGRPSC
00673            SET SSB-COMPLETED (SSB-SELECTOR-STATE) TO TRUE.        ELSGRPSC
00674                                                                   ELSGRPSC
00675 /***********************************************************      ELSGRPSC
00676 *                                                          *      ELSGRPSC
00677 *        EXEC CICS ABEND                                   *      ELSGRPSC
00678 *                                                          *      ELSGRPSC
00679 ************************************************************      ELSGRPSC
00680  EXEC-CICS-ABEND.                                                 ELSGRPSC
00681      EXEC CICS ABEND                                              ELSGRPSC
00682                ABCODE(CIA-ABCODE)                                 ELSGRPSC
00683                END-EXEC.                                          ELSGRPSC
00684                                                                   ELSGRPSC
00685 ************************************************************      ELSGRPSC
00686 *                                                          *      ELSGRPSC
00687 *        RETURN TO CALLER                                  *      ELSGRPSC
00688 *                                                          *      ELSGRPSC
00689 ************************************************************      ELSGRPSC
00690  RETURN-TO-CALLER.                                                ELSGRPSC
