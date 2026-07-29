00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELSMEMSC
00003  PROGRAM-ID.         ELSMEMSC.                                       LV002
00004                                                                   ELSMEMSC
00005  AUTHOR.             NINA CERVANTES.                              ELSMEMSC
00006                                                                   ELSMEMSC
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSMEMSC
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELSMEMSC
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSMEMSC
00010                      233 N. MICHIGAN AVE                          ELSMEMSC
00011                      CHICAGO, ILLINOIS 60601                      ELSMEMSC
00012                                                                   ELSMEMSC
00013  DATE-WRITTEN.       31-OCT-1986.                                 ELSMEMSC
00014                                                                   ELSMEMSC
00015  DATE-COMPILED.                                                   ELSMEMSC
00016                                                                   ELSMEMSC
00017  SECURITY.           COPYRIGHT 1986,                              ELSMEMSC
00018                      HEALTH CARE SERVICE CORPORATION              ELSMEMSC
00019      SKIP3                                                        ELSMEMSC
00020  ENVIRONMENT DIVISION.                                            ELSMEMSC
00021                                                                   ELSMEMSC
00022  CONFIGURATION SECTION.                                           ELSMEMSC
00023  SOURCE-COMPUTER.    IBM-3033.                                    ELSMEMSC
00024  OBJECT-COMPUTER.    IBM-3033.                                    ELSMEMSC
00025      EJECT                                                        ELSMEMSC
00026 ***************************************************************   ELSMEMSC
00027 *                                                             *   ELSMEMSC
00028 *                                                             *   ELSMEMSC
00029 *                                                             *   ELSMEMSC
00030 ***************************************************************   ELSMEMSC
00031 *                                                             *   ELSMEMSC
00032 *                      MAINTENANCE HISTORY                    *   ELSMEMSC
00033 *                                                             *   ELSMEMSC
00034 *  MOD     DATE     BY  DRPT              ACTION              *   ELSMEMSC
00035 * ----- ----------- --- ----- ------------------------------  *   ELSMEMSC
00036 * 01.00 31-OCT-1986 NAC       CREATED                         *   ELSMEMSC
00037 *                                                             *   ELSMEMSC
00038 * 01.01 26-MAY-1988 REB       SPECS WERE CHANGED TO HANDLE    *   ELSMEMSC
00039 *                             ADDITIONAL SITUATIONS.          *   ELSMEMSC
00040 *                                                             *   ELSMEMSC
00041 * 01.02 31-OCT-1988 JPB       ADDED LOGIC TO DISPLAY A MESSAGE*   ELSMEMSC
00042 *                             IF THE SECTION IS NOT VIEWABLE. *   ELSMEMSC
00043 *                                                             *   ELSMEMSC
00044 * 01.03 10-APR-1989 EGL       CONVERTED FROM STRUCTURES       *   ELSMEMSC
00045 *                                                             *   ELSMEMSC
00046 * 01.04 12-APR-1989 GEM       STORAGE MANAGEMENT ENHANCEMENTS *   ELSMEMSC
00047 *                                                             *   ELSMEMSC
00048 * 01.05 09-SEP-1989 EGL       CORRECTED ERRORS IN STORAGE     *   ELSMEMSC
00049 *                             ENHANCMENT CHANGES WHICH WERE   *   ELSMEMSC
00050 *                             CAUSING EL10 AND ASRA ABENDS    *   ELSMEMSC
00051 *                             IN PRODUCTION.  AFTER THIS WAS  *   ELSMEMSC
00052 *                             FIXED, ANOTHER PROBLEM WITH A   *   ELSMEMSC
00053 *                             NO COVERAGE SITUATION WAS NOT   *   ELSMEMSC
00054 *                             DONE CORRECTLY.                 *   ELSMEMSC
00055 *                                                             *   ELSMEMSC
00056 *       13-APR-2003 AKK       REGEN TO TEST ORDER OF COMPILES *   ELSMEMSC
00057 ***************************************************************   ELSMEMSC
00058  DATA DIVISION.                                                   ELSMEMSC
00059                                                                   ELSMEMSC
00060  FILE SECTION.                                                    ELSMEMSC
00061                                                                   ELSMEMSC
00062  WORKING-STORAGE SECTION.                                         ELSMEMSC
00063  77  FILLER                  PIC X(29)   VALUE                    ELSMEMSC
00064      '***ELSMEMSC WS BEGINS HERE***'.                             ELSMEMSC
00065  77  WS-DUMMY-PTR            POINTER.                             ELSMEMSC
00066  01  SUB1                    PIC S9(4)   VALUE +0     COMP.       ELSMEMSC
00067  01  WS-DUMMY-OCCUR-CNT      PIC S9(4)   VALUE +0     COMP.       ELSMEMSC
00068  01  WS-OPT-KEYWORD.                                              ELSMEMSC
00069      05  WS-OPT-PKG          PIC X(03).                           ELSMEMSC
00070      05  WS-OPT-SECT         PIC X(05).                           ELSMEMSC
00071  01  WS-REFORMAT-VALUE.                                           ELSMEMSC
00072      03  WS-PKG-CODE         PIC X(3).                            ELSMEMSC
00073          88  DUMMY-PKG-CODE  VALUE '***'.                         ELSMEMSC
00074      03  WS-SECTION          PIC X(5).                            ELSMEMSC
00075          88  DUMMY-SECTION   VALUE '*****'.                       ELSMEMSC
00076      03  FILLER              PIC X(12).                           ELSMEMSC
00077                                                                   ELSMEMSC
00078  01  WS-MENU-TITLE           PIC X(29)   VALUE                    ELSMEMSC
00079      'SELECT SECTION FOR SUBSCRIBER'.                             ELSMEMSC
00080  01  WS-CANCEL-DATES.                                             ELSMEMSC
00081  05  WS-OLDEST-CANCEL-DATE.                                       ELSMEMSC
00082      10 WS-OLDEST-CAN-DT-CC  PIC X.                               ELSMEMSC
00083      10 WS-OLDEST-CAN-DT     PIC S9(05) COMP-3.                   ELSMEMSC
00084  05  WS-OLDEST-CAN-DT-CEN REDEFINES WS-OLDEST-CANCEL-DATE         ELSMEMSC
00085                              PIC S9(07) COMP-3.                   ELSMEMSC
00086                                                                   ELSMEMSC
00087  01  WS-HDG-LINE-CNT         PIC S9(4)   VALUE +0     COMP.       ELSMEMSC
00088  01  MAX-HEADER-LNS          PIC S9(4)   VALUE +7     COMP.       ELSMEMSC
00089  01  WS-HEADINGS.                                                 ELSMEMSC
00090      02  WS-HEADING-LINES.                                        ELSMEMSC
00091          03  WS-HDR-LN1.                                          ELSMEMSC
00092              05 FILLER       PIC X(51)   VALUE 'MORE THAN ONE SECTELSMEMSC
00093 -    'ION NUMBER APPLIES TO SUBSCRIBER '.                         ELSMEMSC
00094              05  WS-SUBSCRIBER-NBR      PIC X(12).                ELSMEMSC
00095              05  FILLER      PIC X(16)   VALUE   ' BETWEEN'.      ELSMEMSC
00096          03  WS-HDR-LN2.                                          ELSMEMSC
00097              05  WS-HDR-FROM-DT        PIC 99/99/99.              ELSMEMSC
00098              05  FILLER                PIC X(5)   VALUE           ELSMEMSC
00099                                                 ' AND '.          ELSMEMSC
00100              05  WS-HDR-TO-DT          PIC 99/99/99.              ELSMEMSC
00101              05  FILLER                PIC X(01)  VALUE '.'.      ELSMEMSC
00102              05  FILLER                PIC X(57)  VALUE SPACES.   ELSMEMSC
00103      02  FILLER REDEFINES WS-HEADING-LINES.                       ELSMEMSC
00104          03  WS-HDR-LN  OCCURS 2 TIMES PIC X(79).                 ELSMEMSC
00105                                                                   ELSMEMSC
00106  01  WS-NOT-VIEWABLE-MSG.                                         ELSMEMSC
00107      02  FILLER              PIC X(41)   VALUE                    ELSMEMSC
00108          '*** SECTION NOT AVAILABLE FOR VIEWING ***'.             ELSMEMSC
00109      02  FILLER              PIC X(9)    VALUE SPACES.            ELSMEMSC
00110                                                                   ELSMEMSC
00111  01  MAX-DETAIL-LNS          PIC S9(4)   COMP VALUE +2.           ELSMEMSC
00112  01  WS-DTL-LN.                                                   ELSMEMSC
00113      05  WS-DTL-FROM-DT      PIC 99/99/99.                        ELSMEMSC
00114      05  FILLER              PIC X    VALUE SPACE.                ELSMEMSC
00115      05  WS-DTL-TO-DT        PIC 99/99/99.                        ELSMEMSC
00116      05  FILLER              PIC XXX  VALUE SPACES.               ELSMEMSC
00117      05  WS-DTL-PKG-CODE     PIC X(03).                           ELSMEMSC
00118      05  FILLER              PIC X(03) VALUE SPACES.              ELSMEMSC
00119      05  WS-DTL-SECTION      PIC X(5).                            ELSMEMSC
00120      05  FILLER              PIC X(2) VALUE SPACES.               ELSMEMSC
00121      05  WS-DTL-DESCRIPTION  PIC X(50).                           ELSMEMSC
00122      05  FILLER              PIC X(3) VALUE SPACES.               ELSMEMSC
00123                                                                   ELSMEMSC
00124  01  WS-HEADINGS-AREA.                                            ELSMEMSC
00125      05  WS-RESTART-PHRASE-1.                                     ELSMEMSC
00126          10  FILLER          PIC X(67)  VALUE 'PLEASE PRESS THE PFELSMEMSC
00127 -          '5 KEY TO RESTART INQUIRY AGAIN. YOU MAY WANT TO '.    ELSMEMSC
00128          10  FILLER          PIC X(12)  VALUE 'CHANGE YOUR '.     ELSMEMSC
00129      05  WS-RESTART-PHRASE-2.                                     ELSMEMSC
00130          10  FILLER          PIC X(79)  VALUE 'DATE RANGE IN ORDERELSMEMSC
00131 -          ' TO SEE COVERAGE FOR THIS GROUP AND SUBSCRIBER.'.     ELSMEMSC
00132      05  WS-INFO-LINE-1.                                          ELSMEMSC
00133          10  FILLER          PIC X(52)  VALUE                     ELSMEMSC
00134            'PLEASE SELECT THE SECTION NUMBER YOU WISH TO SEE FOR'.ELSMEMSC
00135          10  FILLER          PIC X(27)  VALUE                     ELSMEMSC
00136            ' THE INQUIRY YOU ARE '.                               ELSMEMSC
00137      05  WS-INFO-LINE-2.                                          ELSMEMSC
00138          10  FILLER          PIC X(16)  VALUE 'PROCESSING NOW. '. ELSMEMSC
00139          10  WS-RETURN-MSG   PIC X(63)  VALUE                     ELSMEMSC
00140            'YOU MAY RETURN TO THIS MENU LATER VIA PF9 TO SELECT ANELSMEMSC
00141 -          'OTHER '.                                              ELSMEMSC
00142      05  WS-INFO-LINE-3      PIC X(08)  VALUE 'SECTION.'.         ELSMEMSC
00143      05  WS-COLUMN-HEADINGS  PIC X(79)  VALUE                     ELSMEMSC
00144          '  FROM      TO      SECTION DESCRIPTION'.               ELSMEMSC
00145      05  WS-PRIOR-PHRASE.                                         ELSMEMSC
00146          10  FILLER          PIC X(09)  VALUE 'PRIOR TO '.        ELSMEMSC
00147          10  WS-PRIOR-DATE   PIC XX/XX/XX.                        ELSMEMSC
00148      05  WS-AFTER-PHRASE.                                         ELSMEMSC
00149          10  FILLER          PIC X(06)  VALUE 'AFTER '.           ELSMEMSC
00150          10  WS-AFTER-DATE   PIC XX/XX/XX.                        ELSMEMSC
00151      05  WS-GRP-SUB-PHRASE.                                       ELSMEMSC
00152          10  FILLER          PIC X(06)  VALUE 'GROUP '.           ELSMEMSC
00153          10  WS-GROUP-NBR.                                        ELSMEMSC
00154          15  WS-GROUP-NBR-3  PIC X(03)  VALUE ZEROES.             ELSMEMSC
00155          15  WS-GROUP-NBR-6  PIC X(06)  VALUE SPACES.             ELSMEMSC
00156          10  FILLER          PIC X(12)  VALUE ' SUBSCRIBER '.     ELSMEMSC
00157          10  WS-SUB-NBR      PIC X(12)  VALUE SPACES.             ELSMEMSC
00158          10  FILLER          PIC X(01)  VALUE SPACE.              ELSMEMSC
00159      05  PC-COVERED-UNDER    PIC X(15)  VALUE ' COVERED UNDER '.  ELSMEMSC
00160      05  PC-NOT-COVERED      PIC X(23)  VALUE                     ELSMEMSC
00161              'NOT COVERED UNDER THIS '.                           ELSMEMSC
00162      05  PC-NO-COVERAGE      PIC X(13)  VALUE ' NO COVERAGE '.    ELSMEMSC
00163                                                                   ELSMEMSC
00164  01  WS-HGADATES-PARMS.                                           ELSMEMSC
00165      COPY HGCDAT01.                                               ELSMEMSC
00166 /                                                                 ELSMEMSC
00167                                                                   ELSMEMSC
00168  01  ERROR-MSG-1            PIC X(79)   VALUE                     ELSMEMSC
00169      'ELUKYTAB INDICATES THAT GROUP SECTION WAS NOT FOUND.'.      ELSMEMSC
00170                                                                   ELSMEMSC
00171  01  ERROR-MSG-2            PIC X(79)   VALUE                     ELSMEMSC
00172      'GROUP SECTION DOES NOT EXIST ON BLUE CHIP SYSTEM.'.         ELSMEMSC
00173  01  ERROR-MSG-3.                                                 ELSMEMSC
00174      05  FILLER             PIC X(12)   VALUE ALL '*'.            ELSMEMSC
00175      05  FILLER             PIC X(55)   VALUE                     ELSMEMSC
00176      ' PRESS PF5 TO CONTINUE WITH ELIQ OR CLEAR TO END ELIQ '.    ELSMEMSC
00177      05  FILLER             PIC X(12)   VALUE ALL '*'.            ELSMEMSC
00178 /                                                                 ELSMEMSC
00179      COPY EL00SETC.                                               ELSMEMSC
00180 /                                                                 ELSMEMSC
00181  LINKAGE SECTION.                                                 ELSMEMSC
00182                                                                   ELSMEMSC
00183  01  DFHCOMMAREA.                                                 ELSMEMSC
00184      COPY ELSCOMMC.                                               ELSMEMSC
00185 /                                                                 ELSMEMSC
00186      COPY ELSCIA2C.                                               ELSMEMSC
00187 /                                                                 ELSMEMSC
00188      COPY ELSSSCBC.                                               ELSMEMSC
00189 /                                                                 ELSMEMSC
00190      COPY ELSKEYSC.                                               ELSMEMSC
00191 /                                                                 ELSMEMSC
00192      COPY ELSIOPMC.                                               ELSMEMSC
00193 /                                                                 ELSMEMSC
00194      COPY ELSMHDGC.                                               ELSMEMSC
00195 /                                                                 ELSMEMSC
00196      COPY ELSMOPTC.                                               ELSMEMSC
00197 /                                                                 ELSMEMSC
00198      COPY ELSMENUC.                                               ELSMEMSC
00199 /                                                                 ELSMEMSC
00200      COPY ELSMEMSC.                                               ELSMEMSC
00201 /                                                                 ELSMEMSC
00202  01  GROUP-SPECIFIC.                                              ELSMEMSC
00203      COPY GCGROUPC.                                               ELSMEMSC
00204      EJECT                                                        ELSMEMSC
00205  PROCEDURE DIVISION.                                              ELSMEMSC
00206 ************************************************************      ELSMEMSC
00207 *                                                          *      ELSMEMSC
00208 *                    PROCEDURE DIVISION                    *      ELSMEMSC
00209 *                                                          *      ELSMEMSC
00210 ************************************************************      ELSMEMSC
00211                                                                   ELSMEMSC
00212                                                                   ELSMEMSC
00213 ************************************************************      ELSMEMSC
00214 *                                                          *      ELSMEMSC
00215 *        MEMBER SECTION SELECTOR                           *      ELSMEMSC
00216 *                                                          *      ELSMEMSC
00217 ************************************************************      ELSMEMSC
00218  MEMBER-SECTION-SELECTOR.                                         ELSMEMSC
00219      PERFORM INITIALIZE-MODULE.                                   ELSMEMSC
00220      PERFORM DETERMINE-MODULE-STATUS.                             ELSMEMSC
00221      PERFORM RETURN-TO-CALLER.                                    ELSMEMSC
00222                                                                   ELSMEMSC
00223                                                                   ELSMEMSC
00224 ************************************************************      ELSMEMSC
00225 *                                                          *      ELSMEMSC
00226 *        INITIALIZE MODULE                                 *      ELSMEMSC
00227 *                                                          *      ELSMEMSC
00228 ************************************************************      ELSMEMSC
00229  INITIALIZE-MODULE.                                               ELSMEMSC
00230      PERFORM CHECK-COMMAREA-LENGTH.                               ELSMEMSC
00231      PERFORM ESTABLISH-ADDRESSING-TO-COMMON.                      ELSMEMSC
00232                                                                   ELSMEMSC
00233                                                                   ELSMEMSC
00234 ************************************************************      ELSMEMSC
00235 *                                                          *      ELSMEMSC
00236 *        CHECK COMMAREA LENGTH                             *      ELSMEMSC
00237 *                                                          *      ELSMEMSC
00238 ************************************************************      ELSMEMSC
00239  CHECK-COMMAREA-LENGTH.                                           ELSMEMSC
00240      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELSMEMSC
00241          PERFORM SIGNAL-COMMAREA-LENGTH-ERROR.                    ELSMEMSC
00242                                                                   ELSMEMSC
00243                                                                   ELSMEMSC
00244 ************************************************************      ELSMEMSC
00245 *                                                          *      ELSMEMSC
00246 *        SIGNAL COMMAREA LENGTH ERROR                      *      ELSMEMSC
00247 *                                                          *      ELSMEMSC
00248 ************************************************************      ELSMEMSC
00249  SIGNAL-COMMAREA-LENGTH-ERROR.                                    ELSMEMSC
00250      EXEC CICS ABEND                                              ELSMEMSC
00251                ABCODE('EL01')                                     ELSMEMSC
00252                END-EXEC.                                          ELSMEMSC
00253                                                                   ELSMEMSC
00254                                                                   ELSMEMSC
00255 ************************************************************      ELSMEMSC
00256 *                                                          *      ELSMEMSC
00257 *        ESTABLISH ADDRESSING TO COMMON INTERFACE AREA     *      ELSMEMSC
00258 *                                                          *      ELSMEMSC
00259 ************************************************************      ELSMEMSC
00260  ESTABLISH-ADDRESSING-TO-COMMON.                                  ELSMEMSC
00261      CALL 'ELUINISM' USING DFHCOMMAREA                            ELSMEMSC
00262          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELSMEMSC
00263                                                                   ELSMEMSC
00264      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSMEMSC
00265      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMEMSC
00266          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELSMEMSC
00267      EJECT                                                        ELSMEMSC
00268                                                                   ELSMEMSC
00269                                                                   ELSMEMSC
00270 ************************************************************      ELSMEMSC
00271 *                                                          *      ELSMEMSC
00272 *        DETERMINE MODULE STATUS                           *      ELSMEMSC
00273 *                                                          *      ELSMEMSC
00274 ************************************************************      ELSMEMSC
00275  DETERMINE-MODULE-STATUS.                                         ELSMEMSC
00276      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSMEMSC
00277          PERFORM INITIAL-CALL-FOR-SECTION-MENU                    ELSMEMSC
00278      ELSE IF SSB-MENU-COMPLETE (SSB-SELECTOR-STATE)               ELSMEMSC
00279          PERFORM COMPLETE-PROCESS-FOR-SECTION-M                   ELSMEMSC
00280      ELSE                                                         ELSMEMSC
00281          PERFORM SIGNAL-UNDEFINED-MODULE-STATUS.                  ELSMEMSC
00282      EJECT                                                        ELSMEMSC
00283                                                                   ELSMEMSC
00284                                                                   ELSMEMSC
00285 ************************************************************      ELSMEMSC
00286 *                                                          *      ELSMEMSC
00287 *        SIGNAL UNDEFINED MODULE STATUS                    *      ELSMEMSC
00288 *                                                          *      ELSMEMSC
00289 ************************************************************      ELSMEMSC
00290  SIGNAL-UNDEFINED-MODULE-STATUS.                                  ELSMEMSC
00291      SET CIA-AB-UNDEF TO TRUE.                                    ELSMEMSC
00292      EXEC CICS ABEND                                              ELSMEMSC
00293                ABCODE(CIA-ABCODE)                                 ELSMEMSC
00294                END-EXEC.                                          ELSMEMSC
00295                                                                   ELSMEMSC
00296                                                                   ELSMEMSC
00297 ************************************************************      ELSMEMSC
00298 *                                                          *      ELSMEMSC
00299 *        INITIAL CALL FOR SECTION MENU                     *      ELSMEMSC
00300 *                                                          *      ELSMEMSC
00301 ************************************************************      ELSMEMSC
00302  INITIAL-CALL-FOR-SECTION-MENU.                                   ELSMEMSC
00303      PERFORM PREPARE-STORAGE-AREAS.                               ELSMEMSC
00304      PERFORM BUILD-MENU.                                          ELSMEMSC
00305      EJECT                                                        ELSMEMSC
00306                                                                   ELSMEMSC
00307                                                                   ELSMEMSC
00308 ************************************************************      ELSMEMSC
00309 *                                                          *      ELSMEMSC
00310 *        PREPARE STORAGE AREAS                             *      ELSMEMSC
00311 *                                                          *      ELSMEMSC
00312 ************************************************************      ELSMEMSC
00313  PREPARE-STORAGE-AREAS.                                           ELSMEMSC
00314      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSMEMSC
00315      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMEMSC
00316                            WS-DUMMY-PTR.                          ELSMEMSC
00317      IF CIA-RC-PTR-NULL                                           ELSMEMSC
00318          PERFORM ALLOCATE-ELSMENU-IOPARM-BLOCK.                   ELSMEMSC
00319                                                                   ELSMEMSC
00320      PERFORM PURGE-ELSMENU-AREA.                                  ELSMEMSC
00321                                                                   ELSMEMSC
00322      SET CIA-ELSMEMS-DDN TO TRUE.                                 ELSMEMSC
00323      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMEMSC
00324                            WS-DUMMY-PTR.                          ELSMEMSC
00325      IF CIA-RC-PTR-NULL                                           ELSMEMSC
00326          PERFORM RETRIEVE-MEMBERSHIP-INTERFACEX.                  ELSMEMSC
00327                                                                   ELSMEMSC
00328      SET CIA-ELSMEMS-DDN TO TRUE.                                 ELSMEMSC
00329      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMEMSC
00330          ADDRESS OF MSI-MEMBERSHIP-INTERFACE.                     ELSMEMSC
00331                                                                   ELSMEMSC
00332      MOVE WS-MENU-TITLE TO SSB-MNU-TITLE.                         ELSMEMSC
00333      PERFORM INITIALIZE-MENU-TABLES.                              ELSMEMSC
00334      EJECT                                                        ELSMEMSC
00335                                                                   ELSMEMSC
00336                                                                   ELSMEMSC
00337 ************************************************************      ELSMEMSC
00338 *                                                          *      ELSMEMSC
00339 *        RETRIEVE MEMBERSHIP INTERFACE TABLE               *      ELSMEMSC
00340 *                                                          *      ELSMEMSC
00341 ************************************************************      ELSMEMSC
00342  RETRIEVE-MEMBERSHIP-INTERFACEX.                                  ELSMEMSC
00343      SET CIA-ELSMEMS-DDN   TO  TRUE.                              ELSMEMSC
00344      SET CIA-STG-RETRIEVE  TO  TRUE.                              ELSMEMSC
00345      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSMEMSC
00346                                                                   ELSMEMSC
00347                                                                   ELSMEMSC
00348 ************************************************************      ELSMEMSC
00349 *                                                          *      ELSMEMSC
00350 *        ALLOCATE ELSMENU IOPARM BLOCK                     *      ELSMEMSC
00351 *                                                          *      ELSMEMSC
00352 ************************************************************      ELSMEMSC
00353  ALLOCATE-ELSMENU-IOPARM-BLOCK.                                   ELSMEMSC
00354      SET CIA-ELSMENU-DDN    TO  TRUE.                             ELSMEMSC
00355      SET CIA-STG-GETMAIN    TO  TRUE.                             ELSMEMSC
00356      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSMEMSC
00357      EJECT                                                        ELSMEMSC
00358                                                                   ELSMEMSC
00359                                                                   ELSMEMSC
00360 ************************************************************      ELSMEMSC
00361 *                                                          *      ELSMEMSC
00362 *        PURGE ELSMENU AREA                                *      ELSMEMSC
00363 *                                                          *      ELSMEMSC
00364 ************************************************************      ELSMEMSC
00365  PURGE-ELSMENU-AREA.                                              ELSMEMSC
00366      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSMEMSC
00367      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMEMSC
00368          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSMEMSC
00369      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSMEMSC
00370      SET IOP-DEL TO TRUE.                                         ELSMEMSC
00371      SET IOP-FCQ-NONE TO TRUE.                                    ELSMEMSC
00372      SET IOP-KVQ-NONE TO TRUE.                                    ELSMEMSC
00373      PERFORM LINK-TO-ELUIOPGM.                                    ELSMEMSC
00374                                                                   ELSMEMSC
00375                                                                   ELSMEMSC
00376 ************************************************************      ELSMEMSC
00377 *                                                          *      ELSMEMSC
00378 *        LINK TO STORAGE MANAGER                           *      ELSMEMSC
00379 *                                                          *      ELSMEMSC
00380 ************************************************************      ELSMEMSC
00381  LINK-TO-STORAGE-MANAGER.                                         ELSMEMSC
00382      EXEC CICS LINK                                               ELSMEMSC
00383                PROGRAM ('ELUSTGMG')                               ELSMEMSC
00384                COMMAREA(DFHCOMMAREA)                              ELSMEMSC
00385                END-EXEC.                                          ELSMEMSC
00386      EJECT                                                        ELSMEMSC
00387                                                                   ELSMEMSC
00388                                                                   ELSMEMSC
00389 ************************************************************      ELSMEMSC
00390 *                                                          *      ELSMEMSC
00391 *        INITIALIZE MENU TABLES                            *      ELSMEMSC
00392 *                                                          *      ELSMEMSC
00393 ************************************************************      ELSMEMSC
00394  INITIALIZE-MENU-TABLES.                                          ELSMEMSC
00395      PERFORM ALLOCATE-MENU-SELECTIONS-AREA.                       ELSMEMSC
00396      PERFORM ALLOCATE-MENU-HEADINGS-AREA.                         ELSMEMSC
00397      PERFORM ALLOCATE-MENU-DESCRIPTIONS-ARE.                      ELSMEMSC
00398      INITIALIZE SSB-MNU-CHOICE (1).                               ELSMEMSC
00399                                                                   ELSMEMSC
00400                                                                   ELSMEMSC
00401 ************************************************************      ELSMEMSC
00402 *                                                          *      ELSMEMSC
00403 *        ALLOCATE MENU SELECTIONS AREA                     *      ELSMEMSC
00404 *                                                          *      ELSMEMSC
00405 ************************************************************      ELSMEMSC
00406  ALLOCATE-MENU-SELECTIONS-AREA.                                   ELSMEMSC
00407      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSMEMSC
00408      COMPUTE CIA-AREA-LEN = LENGTH OF MSO-MENU-OPTS-HEADER        ELSMEMSC
00409          +                                                        ELSMEMSC
00410              (LENGTH OF MSO-MENU-OPT *                            ELSMEMSC
00411          MSI-NBR-MBR-SECTNS).                                     ELSMEMSC
00412      SET CIA-STG-GETMAIN TO TRUE.                                 ELSMEMSC
00413      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSMEMSC
00414      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSMEMSC
00415      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMEMSC
00416          ADDRESS OF MSO-MENU-SELECTION-VALUES.                    ELSMEMSC
00417      EJECT                                                        ELSMEMSC
00418                                                                   ELSMEMSC
00419                                                                   ELSMEMSC
00420 ************************************************************      ELSMEMSC
00421 *                                                          *      ELSMEMSC
00422 *        ALLOCATE MENU HEADINGS AREA                       *      ELSMEMSC
00423 *                                                          *      ELSMEMSC
00424 ************************************************************      ELSMEMSC
00425  ALLOCATE-MENU-HEADINGS-AREA.                                     ELSMEMSC
00426      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSMEMSC
00427      COMPUTE CIA-AREA-LEN = LENGTH OF MHD-NBR-HDG-LINES +         ELSMEMSC
00428              (MAX-HEADER-LNS  *  LENGTH OF                        ELSMEMSC
00429          MHD-HDG-LINE).                                           ELSMEMSC
00430      SET CIA-STG-GETMAIN TO TRUE.                                 ELSMEMSC
00431      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSMEMSC
00432      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSMEMSC
00433      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMEMSC
00434          ADDRESS OF MHD-MENU-HEADINGS.                            ELSMEMSC
00435      EJECT                                                        ELSMEMSC
00436                                                                   ELSMEMSC
00437                                                                   ELSMEMSC
00438 ************************************************************      ELSMEMSC
00439 *                                                          *      ELSMEMSC
00440 *        ALLOCATE MENU DESCRIPTIONS AREA                   *      ELSMEMSC
00441 *                                                          *      ELSMEMSC
00442 ************************************************************      ELSMEMSC
00443  ALLOCATE-MENU-DESCRIPTIONS-ARE.                                  ELSMEMSC
00444      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSMEMSC
00445      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMEMSC
00446          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSMEMSC
00447      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSMEMSC
00448      COMPUTE IOP-REC-LEN = LENGTH OF MSD-NBR-DESCR-LINES +        ELSMEMSC
00449              (MAX-DETAIL-LNS  * LENGTH OF                         ELSMEMSC
00450          MSD-DESCR-LINE).                                         ELSMEMSC
00451      SET IOP-GETMAIN-REC TO TRUE.                                 ELSMEMSC
00452      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELSMEMSC
00453      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSMEMSC
00454      SET ADDRESS OF MSD-MENU-ITEM-DESCRIPTIONS                    ELSMEMSC
00455                                        TO IOP-REC-PTR.            ELSMEMSC
00456      EJECT                                                        ELSMEMSC
00457                                                                   ELSMEMSC
00458                                                                   ELSMEMSC
00459 ************************************************************      ELSMEMSC
00460 *                                                          *      ELSMEMSC
00461 *        BUILD MENU                                        *      ELSMEMSC
00462 *                                                          *      ELSMEMSC
00463 ************************************************************      ELSMEMSC
00464  BUILD-MENU.                                                      ELSMEMSC
00465 ******************************************************************ELSMEMSC
00466 ** THESE ARE THE POSSIBLE SITUATIONS THAT REQUIRE UNIQUE HEADINGS ELSMEMSC
00467 **    1.) THE ENTIRE INQUIRY IS BEFORE ANY MEMBERSHIP RECORDS     ELSMEMSC
00468 **    2.) THE BEGINNING OF INQUIRY IS BEFORE ANY FIRST RECORD     ELSMEMSC
00469 **    3.) ALL MEMBERSHIP RECORDS FALL IN THE INQUIRY RANGE        ELSMEMSC
00470 **    4.) THE END OF INQUIRY IS AFTER THE LAST RECORD             ELSMEMSC
00471 **    5.) THE ENTIRE INQUIRY IS AFTER ANY MEMBERSHIP RECORDS      ELSMEMSC
00472 **    6.) BOTH THE BEGINNING AND END OF INQUIRY FALL OUTSIDE THE  ELSMEMSC
00473 **        MEMBERSHIP RECORDS PASSED.                              ELSMEMSC
00474 **                                                                ELSMEMSC
00475 ** FOR ALL OF THE ABOVE SITUATIONS MENTIONED CONSIDERATION MUST   ELSMEMSC
00476 ** BE GIVEN TO THE EVENT THAT PREVIOUS AND/OR NEXT MEMBERSHIP     ELSMEMSC
00477 ** RECORDS MIGHT HAVE CHANGED FROM WHAT WAS SELECTED IN INQUIRY.  ELSMEMSC
00478 ** IF SO SPECIAL TEXT IS DISPLAYED TO INFORM USER OF WHAT HAS     ELSMEMSC
00479 ** OCCURRED.                                                      ELSMEMSC
00480 **                                                                ELSMEMSC
00481 ******************************************************************ELSMEMSC
00482      PERFORM DETERMINE-SECTION-HEADER-THATX.                      ELSMEMSC
00483      MOVE WS-HDG-LINE-CNT                    TO                   ELSMEMSC
00484          MHD-NBR-HDG-LINES.                                       ELSMEMSC
00485      IF MSI-NBR-MBR-SECTNS > ZERO                                 ELSMEMSC
00486          PERFORM CREATE-SECTION-MENU-DETAIL.                      ELSMEMSC
00487      IF WS-DUMMY-OCCUR-CNT > ZERO AND                             ELSMEMSC
00488         WS-DUMMY-OCCUR-CNT = SUB1                                 ELSMEMSC
00489            PERFORM TERMINATE-DUMMY-MENU.                          ELSMEMSC
00490      SET SSB-START-MENU (SSB-SELECTOR-STATE) TO TRUE.             ELSMEMSC
00491  TERMINATE-DUMMY-MENU.                                            ELSMEMSC
00492      SET WS-DUMMY-PTR TO                                          ELSMEMSC
00493           ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                 ELSMEMSC
00494      MOVE 2                       TO MSD-NBR-DESCR-LINES.         ELSMEMSC
00495      MOVE SPACES                  TO MSD-DESCR-LINE (1).          ELSMEMSC
00496      MOVE ERROR-MSG-3             TO MSD-DESCR-LINE (2).          ELSMEMSC
00497      PERFORM ADD-ITEM-DESCRIPTION-TO-MENU.                        ELSMEMSC
00498      SET ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                   ELSMEMSC
00499           TO WS-DUMMY-PTR.                                        ELSMEMSC
00500      EJECT                                                        ELSMEMSC
00501                                                                   ELSMEMSC
00502                                                                   ELSMEMSC
00503 ************************************************************      ELSMEMSC
00504 *                                                          *      ELSMEMSC
00505 *        DETERMINE SECTION HEADER THAT IS APPLICABLE FOR TH*      ELSMEMSC
00506 *                                                          *      ELSMEMSC
00507 ************************************************************      ELSMEMSC
00508  DETERMINE-SECTION-HEADER-THATX.                                  ELSMEMSC
00509      IF MSI-NBR-MBR-SECTNS = ZERO                                 ELSMEMSC
00510          PERFORM CREATE-MESSAGE-THAT-INQUIRY-IS                   ELSMEMSC
00511      ELSE                                                         ELSMEMSC
00512          PERFORM CREATE-SECTION-MENU-HEADER.                      ELSMEMSC
00513                                                                   ELSMEMSC
00514                                                                   ELSMEMSC
00515 ************************************************************      ELSMEMSC
00516 *                                                          *      ELSMEMSC
00517 *        CREATE MESSAGE THAT INQUIRY IS BEFORE ANY APPLICAB*      ELSMEMSC
00518 *                                                          *      ELSMEMSC
00519 ************************************************************      ELSMEMSC
00520  CREATE-MESSAGE-THAT-INQUIRY-IS.                                  ELSMEMSC
00521      MOVE MSI-GRP-SUB-EFF-DT      TO HGADATE-JULIAN1.             ELSMEMSC
00522      PERFORM JULIAN-TO-GREG-CONVERSION.                           ELSMEMSC
00523      MOVE HGADATE-DATE2           TO WS-PRIOR-DATE.               ELSMEMSC
00524      IF MSI-PREV-GROUP-NUMBER   NOT = SPACES OR                   ELSMEMSC
00525                 MSI-PREV-MEMBR-NBR NOT = SPACES                   ELSMEMSC
00526          PERFORM DISPLAY-THE-PREVIOUS-MEMBER-IN                   ELSMEMSC
00527      ELSE                                                         ELSMEMSC
00528          PERFORM DISPLAY-NO-BEFORE-COVERAGE.                      ELSMEMSC
00529      ADD  +1                      TO WS-HDG-LINE-CNT.             ELSMEMSC
00530      MOVE WS-RESTART-PHRASE-1     TO MHD-HDG-LINE                 ELSMEMSC
00531          (WS-HDG-LINE-CNT).                                       ELSMEMSC
00532      ADD  +1                      TO WS-HDG-LINE-CNT.             ELSMEMSC
00533      MOVE WS-RESTART-PHRASE-2     TO MHD-HDG-LINE                 ELSMEMSC
00534          (WS-HDG-LINE-CNT).                                       ELSMEMSC
00535      EJECT                                                        ELSMEMSC
00536                                                                   ELSMEMSC
00537                                                                   ELSMEMSC
00538 ************************************************************      ELSMEMSC
00539 *                                                          *      ELSMEMSC
00540 *        DISPLAY THE PREVIOUS MEMBER INFO THAT CHANGED     *      ELSMEMSC
00541 *                                                          *      ELSMEMSC
00542 ************************************************************      ELSMEMSC
00543  DISPLAY-THE-PREVIOUS-MEMBER-IN.                                  ELSMEMSC
00544      IF MSI-PREV-GROUP-NUMBER   NOT = SPACES                      ELSMEMSC
00545          PERFORM INSERT-MSI-PREVIOUS-GROUP-NUMB                   ELSMEMSC
00546      ELSE                                                         ELSMEMSC
00547          PERFORM USE-GROUP-NUMBER-FROM-SSCB.                      ELSMEMSC
00548      IF MSI-PREV-MEMBR-NBR NOT = SPACES                           ELSMEMSC
00549          PERFORM INSERT-MSI-PREVIOUS-MEMBER-NUM                   ELSMEMSC
00550      ELSE                                                         ELSMEMSC
00551          PERFORM USE-MEMBER-NUMBER-FROM-SSCB.                     ELSMEMSC
00552      ADD  +1                      TO WS-HDG-LINE-CNT.             ELSMEMSC
00553      STRING WS-PRIOR-PHRASE          DELIMITED BY SIZE            ELSMEMSC
00554             PC-COVERED-UNDER         DELIMITED BY SIZE            ELSMEMSC
00555             WS-GRP-SUB-PHRASE        DELIMITED BY SIZE            ELSMEMSC
00556             '.'                      DELIMITED BY SIZE            ELSMEMSC
00557             INTO MHD-HDG-LINE (1).                                ELSMEMSC
00558                                                                   ELSMEMSC
00559                                                                   ELSMEMSC
00560 ************************************************************      ELSMEMSC
00561 *                                                          *      ELSMEMSC
00562 *        INSERT MSI PREVIOUS GROUP NUMBER                  *      ELSMEMSC
00563 *                                                          *      ELSMEMSC
00564 ************************************************************      ELSMEMSC
00565  INSERT-MSI-PREVIOUS-GROUP-NUMB.                                  ELSMEMSC
00566      MOVE MSI-PREV-GROUP-NUMBER   TO WS-GROUP-NBR.                ELSMEMSC
00567                                                                   ELSMEMSC
00568                                                                   ELSMEMSC
00569 ************************************************************      ELSMEMSC
00570 *                                                          *      ELSMEMSC
00571 *        USE GROUP NUMBER FROM SSCB                        *      ELSMEMSC
00572 *                                                          *      ELSMEMSC
00573 ************************************************************      ELSMEMSC
00574  USE-GROUP-NUMBER-FROM-SSCB.                                      ELSMEMSC
00575      MOVE SSB-GROUP-NUMBER        TO WS-GROUP-NBR.                ELSMEMSC
00576                                                                   ELSMEMSC
00577                                                                   ELSMEMSC
00578 ************************************************************      ELSMEMSC
00579 *                                                          *      ELSMEMSC
00580 *        INSERT MSI PREVIOUS MEMBER NUMBER                 *      ELSMEMSC
00581 *                                                          *      ELSMEMSC
00582 ************************************************************      ELSMEMSC
00583  INSERT-MSI-PREVIOUS-MEMBER-NUM.                                  ELSMEMSC
00584      MOVE MSI-PREV-MEMBR-NBR      TO WS-SUB-NBR.                  ELSMEMSC
00585                                                                   ELSMEMSC
00586                                                                   ELSMEMSC
00587 ************************************************************      ELSMEMSC
00588 *                                                          *      ELSMEMSC
00589 *        USE MEMBER NUMBER FROM SSCB                       *      ELSMEMSC
00590 *                                                          *      ELSMEMSC
00591 ************************************************************      ELSMEMSC
00592  USE-MEMBER-NUMBER-FROM-SSCB.                                     ELSMEMSC
00593      MOVE SSB-SUBSCRIBER-NBR      TO WS-SUB-NBR.                  ELSMEMSC
00594      EJECT                                                        ELSMEMSC
00595                                                                   ELSMEMSC
00596                                                                   ELSMEMSC
00597 ************************************************************      ELSMEMSC
00598 *                                                          *      ELSMEMSC
00599 *        CREATE SECTION MENU HEADER                        *      ELSMEMSC
00600 *                                                          *      ELSMEMSC
00601 ************************************************************      ELSMEMSC
00602  CREATE-SECTION-MENU-HEADER.                                      ELSMEMSC
00603      MOVE MSI-TERMIN-DATE-CC (1)  TO                              ELSMEMSC
00604          WS-OLDEST-CAN-DT-CEN.                                    ELSMEMSC
00605      IF MSI-NBR-MBR-SECTNS > +1                                   ELSMEMSC
00606          PERFORM DETERMINE-OLDEST-CANCEL-DATE-F.                  ELSMEMSC
00607      IF SSB-SRV-FROM-DT-CEN < MSI-EFF-DATE-CENTURY (1)            ELSMEMSC
00608          AND    SSB-SRV-TO-DT-CEN >                               ELSMEMSC
00609          WS-OLDEST-CAN-DT-CEN                                     ELSMEMSC
00610          PERFORM DISPLAY-HEADER-FOR-SITUATION-S                   ELSMEMSC
00611      ELSE IF SSB-SRV-FROM-DT-CEN < MSI-EFF-DATE-CENTURY (1)       ELSMEMSC
00612          PERFORM DISPLAY-HEADER-FOR-CASE-2                        ELSMEMSC
00613      ELSE IF SSB-SRV-TO-DT-CEN   > WS-OLDEST-CAN-DT-CEN           ELSMEMSC
00614          PERFORM DISPLAY-HEADER-FOR-EITHER-SITU                   ELSMEMSC
00615      ELSE IF MSI-NBR-MBR-SECTNS > +1                              ELSMEMSC
00616          PERFORM DISPLAY-HEADER-FOR-CASE-3.                       ELSMEMSC
00617      ADD  +1                      TO WS-HDG-LINE-CNT.             ELSMEMSC
00618      MOVE WS-INFO-LINE-1          TO MHD-HDG-LINE                 ELSMEMSC
00619          (WS-HDG-LINE-CNT).                                       ELSMEMSC
00620      IF MSI-NBR-MBR-SECTNS < +2                                   ELSMEMSC
00621          PERFORM CLEAR-MESSAGE-AREA-ABOUT-RETUR.                  ELSMEMSC
00622      ADD  +1                      TO WS-HDG-LINE-CNT.             ELSMEMSC
00623      MOVE WS-INFO-LINE-2          TO MHD-HDG-LINE                 ELSMEMSC
00624          (WS-HDG-LINE-CNT).                                       ELSMEMSC
00625      IF MSI-NBR-MBR-SECTNS > +1                                   ELSMEMSC
00626          PERFORM FINISH-MESSAGE-ABOUT-RETURN-WI.                  ELSMEMSC
00627      ADD  +1                      TO WS-HDG-LINE-CNT.             ELSMEMSC
00628      MOVE SPACES                  TO MHD-HDG-LINE                 ELSMEMSC
00629          (WS-HDG-LINE-CNT).                                       ELSMEMSC
00630      ADD  +1                      TO WS-HDG-LINE-CNT.             ELSMEMSC
00631      MOVE WS-COLUMN-HEADINGS      TO MHD-HDG-LINE                 ELSMEMSC
00632          (WS-HDG-LINE-CNT).                                       ELSMEMSC
00633      EJECT                                                        ELSMEMSC
00634                                                                   ELSMEMSC
00635                                                                   ELSMEMSC
00636 ************************************************************      ELSMEMSC
00637 *                                                          *      ELSMEMSC
00638 *        CLEAR MESSAGE AREA ABOUT RETURN WITH PF9 KEY      *      ELSMEMSC
00639 *                                                          *      ELSMEMSC
00640 ************************************************************      ELSMEMSC
00641  CLEAR-MESSAGE-AREA-ABOUT-RETUR.                                  ELSMEMSC
00642      MOVE SPACES                  TO WS-RETURN-MSG.               ELSMEMSC
00643      EJECT                                                        ELSMEMSC
00644                                                                   ELSMEMSC
00645                                                                   ELSMEMSC
00646 ************************************************************      ELSMEMSC
00647 *                                                          *      ELSMEMSC
00648 *        FINISH MESSAGE ABOUT RETURN WITH PF9 KEY          *      ELSMEMSC
00649 *                                                          *      ELSMEMSC
00650 ************************************************************      ELSMEMSC
00651  FINISH-MESSAGE-ABOUT-RETURN-WI.                                  ELSMEMSC
00652      ADD  +1                      TO WS-HDG-LINE-CNT.             ELSMEMSC
00653      MOVE WS-INFO-LINE-3          TO MHD-HDG-LINE                 ELSMEMSC
00654          (WS-HDG-LINE-CNT).                                       ELSMEMSC
00655      EJECT                                                        ELSMEMSC
00656                                                                   ELSMEMSC
00657                                                                   ELSMEMSC
00658 ************************************************************      ELSMEMSC
00659 *                                                          *      ELSMEMSC
00660 *        DETERMINE OLDEST CANCEL DATE FOR ENTIRE MSI TABLE *      ELSMEMSC
00661 *                                                          *      ELSMEMSC
00662 ************************************************************      ELSMEMSC
00663  DETERMINE-OLDEST-CANCEL-DATE-F.                                  ELSMEMSC
00664      PERFORM COMPARE-MSI-CANCEL-DATE-WITH-C                       ELSMEMSC
00665          VARYING MSI-IDX FROM 2 BY 1                              ELSMEMSC
00666                  UNTIL   MSI-IDX > MSI-NBR-MBR-SECTNS.            ELSMEMSC
00667                                                                   ELSMEMSC
00668                                                                   ELSMEMSC
00669 ************************************************************      ELSMEMSC
00670 *                                                          *      ELSMEMSC
00671 *        COMPARE MSI CANCEL DATE WITH CANCEL DATE HELD     *      ELSMEMSC
00672 *                                                          *      ELSMEMSC
00673 ************************************************************      ELSMEMSC
00674  COMPARE-MSI-CANCEL-DATE-WITH-C.                                  ELSMEMSC
00675      IF MSI-TERMIN-DATE-CC  (MSI-IDX) >                           ELSMEMSC
00676                                 WS-OLDEST-CAN-DT-CEN              ELSMEMSC
00677          PERFORM HOLD-A-NEW-CANCEL-DATE.                          ELSMEMSC
00678                                                                   ELSMEMSC
00679                                                                   ELSMEMSC
00680 ************************************************************      ELSMEMSC
00681 *                                                          *      ELSMEMSC
00682 *        HOLD A NEW CANCEL DATE                            *      ELSMEMSC
00683 *                                                          *      ELSMEMSC
00684 ************************************************************      ELSMEMSC
00685  HOLD-A-NEW-CANCEL-DATE.                                          ELSMEMSC
00686      MOVE MSI-TERMIN-DATE-CC (MSI-IDX) TO                         ELSMEMSC
00687          WS-OLDEST-CAN-DT-CEN.                                    ELSMEMSC
00688      EJECT                                                        ELSMEMSC
00689                                                                   ELSMEMSC
00690                                                                   ELSMEMSC
00691 ************************************************************      ELSMEMSC
00692 *                                                          *      ELSMEMSC
00693 *        DISPLAY HEADER FOR SITUATION SIX                  *      ELSMEMSC
00694 *                                                          *      ELSMEMSC
00695 ************************************************************      ELSMEMSC
00696  DISPLAY-HEADER-FOR-SITUATION-S.                                  ELSMEMSC
00697      MOVE MSI-EFF-DT (1)          TO HGADATE-JULIAN1.             ELSMEMSC
00698      PERFORM JULIAN-TO-GREG-CONVERSION.                           ELSMEMSC
00699      MOVE HGADATE-DATE2           TO WS-PRIOR-DATE.               ELSMEMSC
00700      MOVE WS-OLDEST-CAN-DT        TO HGADATE-JULIAN1.             ELSMEMSC
00701      PERFORM JULIAN-TO-GREG-CONVERSION.                           ELSMEMSC
00702      MOVE HGADATE-DATE2           TO WS-AFTER-DATE.               ELSMEMSC
00703      IF MSI-PREV-GRP-NBR   NOT = SPACES OR                        ELSMEMSC
00704                 MSI-PREV-MEMBR-NBR NOT = SPACES                   ELSMEMSC
00705          PERFORM DISPLAY-THE-PREVIOUS-MEMBER-IN.                  ELSMEMSC
00706      IF MSI-NEXT-GRP-NBR   NOT = SPACES OR                        ELSMEMSC
00707                 MSI-NEXT-MEMBR-NBR NOT = SPACES                   ELSMEMSC
00708          PERFORM DISPLAY-THE-NEXT-MEMBER-INFO-T.                  ELSMEMSC
00709      IF WS-HDG-LINE-CNT = ZERO                                    ELSMEMSC
00710          PERFORM DISPLAY-NO-COVERAGE-BEFORE-AND.                  ELSMEMSC
00711      EJECT                                                        ELSMEMSC
00712                                                                   ELSMEMSC
00713                                                                   ELSMEMSC
00714 ************************************************************      ELSMEMSC
00715 *                                                          *      ELSMEMSC
00716 *        DISPLAY THE NEXT MEMBER INFO THAT CHANGED         *      ELSMEMSC
00717 *                                                          *      ELSMEMSC
00718 ************************************************************      ELSMEMSC
00719  DISPLAY-THE-NEXT-MEMBER-INFO-T.                                  ELSMEMSC
00720      IF MSI-PREV-GROUP-NUMBER NOT = SPACES                        ELSMEMSC
00721          PERFORM INSERT-MSI-NEXT-GROUP-NUMBER                     ELSMEMSC
00722      ELSE                                                         ELSMEMSC
00723          PERFORM USE-GROUP-NUMBER-FROM-SSCB.                      ELSMEMSC
00724      IF MSI-PREV-MEMBR-NBR NOT = SPACES                           ELSMEMSC
00725          PERFORM INSERT-MSI-NEXT-MEMBER-NUMBER                    ELSMEMSC
00726      ELSE                                                         ELSMEMSC
00727          PERFORM USE-MEMBER-NUMBER-FROM-SSCB.                     ELSMEMSC
00728      ADD  +1                      TO WS-HDG-LINE-CNT.             ELSMEMSC
00729      STRING WS-AFTER-PHRASE          DELIMITED BY SIZE            ELSMEMSC
00730             PC-COVERED-UNDER         DELIMITED BY SIZE            ELSMEMSC
00731             WS-GRP-SUB-PHRASE        DELIMITED BY SIZE            ELSMEMSC
00732             '.'                      DELIMITED BY SIZE            ELSMEMSC
00733             INTO MHD-HDG-LINE (1).                                ELSMEMSC
00734                                                                   ELSMEMSC
00735                                                                   ELSMEMSC
00736 ************************************************************      ELSMEMSC
00737 *                                                          *      ELSMEMSC
00738 *        INSERT MSI NEXT GROUP NUMBER                      *      ELSMEMSC
00739 *                                                          *      ELSMEMSC
00740 ************************************************************      ELSMEMSC
00741  INSERT-MSI-NEXT-GROUP-NUMBER.                                    ELSMEMSC
00742      MOVE MSI-NEXT-GROUP-NUMBER   TO WS-GROUP-NBR.                ELSMEMSC
00743                                                                   ELSMEMSC
00744                                                                   ELSMEMSC
00745 ************************************************************      ELSMEMSC
00746 *                                                          *      ELSMEMSC
00747 *        INSERT MSI NEXT MEMBER NUMBER                     *      ELSMEMSC
00748 *                                                          *      ELSMEMSC
00749 ************************************************************      ELSMEMSC
00750  INSERT-MSI-NEXT-MEMBER-NUMBER.                                   ELSMEMSC
00751      MOVE MSI-NEXT-MEMBR-NBR      TO WS-SUB-NBR.                  ELSMEMSC
00752      EJECT                                                        ELSMEMSC
00753                                                                   ELSMEMSC
00754                                                                   ELSMEMSC
00755 ************************************************************      ELSMEMSC
00756 *                                                          *      ELSMEMSC
00757 *        DISPLAY NO COVERAGE BEFORE AND AFTER MEMBER INFO  *      ELSMEMSC
00758 *                                                          *      ELSMEMSC
00759 ************************************************************      ELSMEMSC
00760  DISPLAY-NO-COVERAGE-BEFORE-AND.                                  ELSMEMSC
00761      PERFORM DISPLAY-NO-BEFORE-COVERAGE.                          ELSMEMSC
00762      PERFORM DISPLAY-NO-AFTER-COVERAGE.                           ELSMEMSC
00763      EJECT                                                        ELSMEMSC
00764                                                                   ELSMEMSC
00765                                                                   ELSMEMSC
00766 ************************************************************      ELSMEMSC
00767 *                                                          *      ELSMEMSC
00768 *        DISPLAY HEADER FOR CASE 2                         *      ELSMEMSC
00769 *                                                          *      ELSMEMSC
00770 ************************************************************      ELSMEMSC
00771  DISPLAY-HEADER-FOR-CASE-2.                                       ELSMEMSC
00772      MOVE MSI-EFF-DT (1)          TO HGADATE-JULIAN1.             ELSMEMSC
00773      PERFORM JULIAN-TO-GREG-CONVERSION.                           ELSMEMSC
00774      MOVE HGADATE-DATE2           TO WS-PRIOR-DATE.               ELSMEMSC
00775      IF MSI-PREV-GRP-NBR   NOT = SPACES OR                        ELSMEMSC
00776                 MSI-PREV-MEMBR-NBR NOT = SPACES                   ELSMEMSC
00777          PERFORM DISPLAY-THE-PREVIOUS-MEMBER-IN.                  ELSMEMSC
00778      IF WS-HDG-LINE-CNT = ZERO                                    ELSMEMSC
00779          PERFORM DISPLAY-NO-BEFORE-COVERAGE.                      ELSMEMSC
00780      EJECT                                                        ELSMEMSC
00781                                                                   ELSMEMSC
00782                                                                   ELSMEMSC
00783 ************************************************************      ELSMEMSC
00784 *                                                          *      ELSMEMSC
00785 *        DISPLAY NO BEFORE COVERAGE                        *      ELSMEMSC
00786 *                                                          *      ELSMEMSC
00787 ************************************************************      ELSMEMSC
00788  DISPLAY-NO-BEFORE-COVERAGE.                                      ELSMEMSC
00789      MOVE SSB-GROUP-NUMBER        TO WS-GROUP-NBR.                ELSMEMSC
00790      MOVE SSB-SUBSCRIBER-NBR      TO WS-SUB-NBR.                  ELSMEMSC
00791      ADD  +1                      TO WS-HDG-LINE-CNT.             ELSMEMSC
00792      STRING PC-NOT-COVERED           DELIMITED BY SIZE            ELSMEMSC
00793             WS-GRP-SUB-PHRASE        DELIMITED BY SIZE            ELSMEMSC
00794             WS-PRIOR-PHRASE          DELIMITED BY SIZE            ELSMEMSC
00795             '.'                      DELIMITED BY SIZE            ELSMEMSC
00796             INTO MHD-HDG-LINE (1).                                ELSMEMSC
00797      EJECT                                                        ELSMEMSC
00798                                                                   ELSMEMSC
00799                                                                   ELSMEMSC
00800 ************************************************************      ELSMEMSC
00801 *                                                          *      ELSMEMSC
00802 *        DISPLAY HEADER FOR EITHER SITUATIONS FOUR OR FIVE *      ELSMEMSC
00803 *                                                          *      ELSMEMSC
00804 ************************************************************      ELSMEMSC
00805  DISPLAY-HEADER-FOR-EITHER-SITU.                                  ELSMEMSC
00806      MOVE WS-OLDEST-CAN-DT        TO HGADATE-JULIAN1.             ELSMEMSC
00807      PERFORM JULIAN-TO-GREG-CONVERSION.                           ELSMEMSC
00808      MOVE HGADATE-DATE2           TO WS-AFTER-DATE.               ELSMEMSC
00809      IF MSI-NEXT-GROUP-NUMBER NOT = SPACES OR                     ELSMEMSC
00810                 MSI-NEXT-MEMBR-NBR NOT = SPACES                   ELSMEMSC
00811          PERFORM DISPLAY-THE-NEXT-MEMBER-INFO-T.                  ELSMEMSC
00812      IF WS-HDG-LINE-CNT = ZERO                                    ELSMEMSC
00813          PERFORM DISPLAY-NO-AFTER-COVERAGE.                       ELSMEMSC
00814                                                                   ELSMEMSC
00815                                                                   ELSMEMSC
00816 ************************************************************      ELSMEMSC
00817 *                                                          *      ELSMEMSC
00818 *        DISPLAY NO AFTER COVERAGE                         *      ELSMEMSC
00819 *                                                          *      ELSMEMSC
00820 ************************************************************      ELSMEMSC
00821  DISPLAY-NO-AFTER-COVERAGE.                                       ELSMEMSC
00822      MOVE SSB-GROUP-NUMBER        TO WS-GROUP-NBR.                ELSMEMSC
00823      MOVE SSB-SUBSCRIBER-NBR      TO WS-SUB-NBR.                  ELSMEMSC
00824      ADD  +1                      TO WS-HDG-LINE-CNT.             ELSMEMSC
00825      STRING PC-NOT-COVERED           DELIMITED BY SIZE            ELSMEMSC
00826             WS-GRP-SUB-PHRASE        DELIMITED BY SIZE            ELSMEMSC
00827             WS-AFTER-PHRASE          DELIMITED BY SIZE            ELSMEMSC
00828             '.'                      DELIMITED BY SIZE            ELSMEMSC
00829             INTO MHD-HDG-LINE (WS-HDG-LINE-CNT).                  ELSMEMSC
00830      EJECT                                                        ELSMEMSC
00831                                                                   ELSMEMSC
00832                                                                   ELSMEMSC
00833 ************************************************************      ELSMEMSC
00834 *                                                          *      ELSMEMSC
00835 *        DISPLAY HEADER FOR CASE 3                         *      ELSMEMSC
00836 *                                                          *      ELSMEMSC
00837 ************************************************************      ELSMEMSC
00838  DISPLAY-HEADER-FOR-CASE-3.                                       ELSMEMSC
00839      MOVE SSB-SUBSCRIBER-NBR      TO WS-SUBSCRIBER-NBR.           ELSMEMSC
00840      PERFORM CONVERT-HEADER-EFFECTIVE-DATE.                       ELSMEMSC
00841      PERFORM CONVERT-HEADER-TERMINATION-DAT.                      ELSMEMSC
00842      PERFORM LOAD-ENTRIES-INTO-HEADER-TABLE                       ELSMEMSC
00843          VARYING MHD-IDX FROM 1 BY 1                              ELSMEMSC
00844                  UNTIL   MHD-IDX > 2.                             ELSMEMSC
00845                                                                   ELSMEMSC
00846                                                                   ELSMEMSC
00847 ************************************************************      ELSMEMSC
00848 *                                                          *      ELSMEMSC
00849 *        CONVERT HEADER EFFECTIVE DATE                     *      ELSMEMSC
00850 *                                                          *      ELSMEMSC
00851 ************************************************************      ELSMEMSC
00852  CONVERT-HEADER-EFFECTIVE-DATE.                                   ELSMEMSC
00853      MOVE SSB-SRV-FROM-DATE      TO HGADATE-JULIAN1.              ELSMEMSC
00854      PERFORM JULIAN-TO-GREG-CONVERSION.                           ELSMEMSC
00855      MOVE HGADATE-DATE2          TO WS-HDR-FROM-DT.               ELSMEMSC
00856                                                                   ELSMEMSC
00857                                                                   ELSMEMSC
00858 ************************************************************      ELSMEMSC
00859 *                                                          *      ELSMEMSC
00860 *        CONVERT HEADER TERMINATION DATE                   *      ELSMEMSC
00861 *                                                          *      ELSMEMSC
00862 ************************************************************      ELSMEMSC
00863  CONVERT-HEADER-TERMINATION-DAT.                                  ELSMEMSC
00864      MOVE SSB-SRV-TO-DATE        TO HGADATE-JULIAN1.              ELSMEMSC
00865      PERFORM JULIAN-TO-GREG-CONVERSION.                           ELSMEMSC
00866      MOVE HGADATE-DATE2          TO WS-HDR-TO-DT.                 ELSMEMSC
00867                                                                   ELSMEMSC
00868                                                                   ELSMEMSC
00869 ************************************************************      ELSMEMSC
00870 *                                                          *      ELSMEMSC
00871 *        LOAD ENTRIES INTO HEADER TABLE                    *      ELSMEMSC
00872 *                                                          *      ELSMEMSC
00873 ************************************************************      ELSMEMSC
00874  LOAD-ENTRIES-INTO-HEADER-TABLE.                                  ELSMEMSC
00875      ADD  +1                     TO WS-HDG-LINE-CNT.              ELSMEMSC
00876      MOVE WS-HDR-LN (MHD-IDX)    TO MHD-HDG-LINE                  ELSMEMSC
00877          (MHD-IDX).                                               ELSMEMSC
00878      EJECT                                                        ELSMEMSC
00879                                                                   ELSMEMSC
00880                                                                   ELSMEMSC
00881 ************************************************************      ELSMEMSC
00882 *                                                          *      ELSMEMSC
00883 *        CREATE SECTION MENU DETAIL                        *      ELSMEMSC
00884 *                                                          *      ELSMEMSC
00885 ************************************************************      ELSMEMSC
00886  CREATE-SECTION-MENU-DETAIL.                                      ELSMEMSC
00887      PERFORM BUILD-MENU-OPTIONS-TABLE.                            ELSMEMSC
00888 ********************************************************          ELSMEMSC
00889 ** THIS ALLOCATE KEY AREA WAS ADDED BECAUSE I NOTICED **          ELSMEMSC
00890 ** THAT THE POINTER WAS NULL AT RESELECTION TIME !    **          ELSMEMSC
00891 ** REB ==> 05/26/88.                                  **          ELSMEMSC
00892 ********************************************************          ELSMEMSC
00893      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELSMEMSC
00894      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMEMSC
00895          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELSMEMSC
00896      IF CIA-RC-PTR-NULL                                           ELSMEMSC
00897          PERFORM ALLOCATE-KEY-WORK-AREA                           ELSMEMSC
00898          SET CIA-ELSKEYS-DDN TO TRUE                              ELSMEMSC
00899          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELSMEMSC
00900              ADDRESS OF KWA-FILE-KEY-WORK-AREA.                   ELSMEMSC
00901                                                                   ELSMEMSC
00902      SET CIA-GCGRPSPC-DDN TO TRUE.                                ELSMEMSC
00903      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMEMSC
00904                            WS-DUMMY-PTR.                          ELSMEMSC
00905      IF CIA-RC-PTR-NULL                                           ELSMEMSC
00906          PERFORM ALLOCATE-GROUP-SPECIFIC-IOPARM.                  ELSMEMSC
00907      MOVE LOW-VALUES TO KWA-GCGRPSPC-KEY.                         ELSMEMSC
00908      MOVE 000 TO              KWA-GCG-PLAN-CODE.                  ELSMEMSC
00909      MOVE SSB-GROUP-NUMBER TO KWA-GCG-GROUP-NUMBER.               ELSMEMSC
00910      MOVE ZERO TO SUB1.                                           ELSMEMSC
00911      PERFORM DUMP-SECTION-TABLE                                   ELSMEMSC
00912          VARYING MSI-IDX FROM 1 BY 1                              ELSMEMSC
00913                    UNTIL MSI-IDX GREATER THAN                     ELSMEMSC
00914              MSI-NBR-MBR-SECTNS.                                  ELSMEMSC
00915      EJECT                                                        ELSMEMSC
00916                                                                   ELSMEMSC
00917                                                                   ELSMEMSC
00918 ************************************************************      ELSMEMSC
00919 *                                                          *      ELSMEMSC
00920 *        BUILD MENU OPTIONS TABLE                          *      ELSMEMSC
00921 *                                                          *      ELSMEMSC
00922 ************************************************************      ELSMEMSC
00923  BUILD-MENU-OPTIONS-TABLE.                                        ELSMEMSC
00924      MOVE LENGTH OF GCG-SECTION-NUM TO MSO-OPT-LEN.               ELSMEMSC
00925      SET MSO-OPT-TYP-AN TO TRUE.                                  ELSMEMSC
00926      MOVE MSI-NBR-MBR-SECTNS TO MSO-NBR-MENU-OPTS.                ELSMEMSC
00927      MOVE 1   TO MSO-MIN-CHOICES                                  ELSMEMSC
00928                  MSO-MAX-CHOICES.                                 ELSMEMSC
00929      EJECT                                                        ELSMEMSC
00930                                                                   ELSMEMSC
00931                                                                   ELSMEMSC
00932 ************************************************************      ELSMEMSC
00933 *                                                          *      ELSMEMSC
00934 *        ALLOCATE KEY WORK AREA                            *      ELSMEMSC
00935 *                                                          *      ELSMEMSC
00936 ************************************************************      ELSMEMSC
00937  ALLOCATE-KEY-WORK-AREA.                                          ELSMEMSC
00938      SET CIA-ELSKEYS-DDN    TO  TRUE.                             ELSMEMSC
00939      SET CIA-STG-GETMAIN    TO  TRUE.                             ELSMEMSC
00940      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSMEMSC
00941      EJECT                                                        ELSMEMSC
00942                                                                   ELSMEMSC
00943                                                                   ELSMEMSC
00944 ************************************************************      ELSMEMSC
00945 *                                                          *      ELSMEMSC
00946 *        ALLOCATE GROUP SPECIFIC IOPARM AREA               *      ELSMEMSC
00947 *                                                          *      ELSMEMSC
00948 ************************************************************      ELSMEMSC
00949  ALLOCATE-GROUP-SPECIFIC-IOPARM.                                  ELSMEMSC
00950      SET CIA-GCGRPSPC-DDN   TO  TRUE.                             ELSMEMSC
00951      SET CIA-STG-GETMAIN    TO  TRUE.                             ELSMEMSC
00952      PERFORM LINK-TO-STORAGE-MANAGER.                             ELSMEMSC
00953      EJECT                                                        ELSMEMSC
00954                                                                   ELSMEMSC
00955                                                                   ELSMEMSC
00956 ************************************************************      ELSMEMSC
00957 *                                                          *      ELSMEMSC
00958 *        DUMP SECTION TABLE                                *      ELSMEMSC
00959 *                                                          *      ELSMEMSC
00960 ************************************************************      ELSMEMSC
00961  DUMP-SECTION-TABLE.                                              ELSMEMSC
00962      MOVE MSI-PKG-CODE (MSI-IDX)   TO                             ELSMEMSC
00963                KWA-GCG-PKG-CODE.                                  ELSMEMSC
00964      MOVE MSI-MEMBER-SECTION (MSI-IDX)   TO                       ELSMEMSC
00965                KWA-GCG-SECTION-NUMBER.                            ELSMEMSC
00966      PERFORM ISSUE-READ.                                          ELSMEMSC
00967      IF IOP-RC-OK                                                 ELSMEMSC
00968          PERFORM CREATE-ITEM-DESCRIPTION-PER-SE.                  ELSMEMSC
00969      EJECT                                                        ELSMEMSC
00970                                                                   ELSMEMSC
00971                                                                   ELSMEMSC
00972 ************************************************************      ELSMEMSC
00973 *                                                          *      ELSMEMSC
00974 *        ISSUE READ                                        *      ELSMEMSC
00975 *                                                          *      ELSMEMSC
00976 ************************************************************      ELSMEMSC
00977  ISSUE-READ.                                                      ELSMEMSC
00978      SET CIA-GCGRPSPC-DDN TO TRUE.                                ELSMEMSC
00979      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMEMSC
00980          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSMEMSC
00981      MOVE KWA-GCGRPSPC-KEY TO IOP-FILE-KEY.                       ELSMEMSC
00982      SET CIA-GCGRPSPC-DDN TO TRUE.                                ELSMEMSC
00983      COMPUTE IOP-KEY-LEN =                                        ELSMEMSC
00984                   LENGTH OF KWA-GCG-GROUP-NUMBER +                ELSMEMSC
00985                   LENGTH OF KWA-GCG-SECTION-NUMBER                ELSMEMSC
00986      SET IOP-RD    TO TRUE.                                       ELSMEMSC
00987      SET IOP-FCQ-GEN  TO TRUE.                                    ELSMEMSC
00988      SET IOP-KVQ-EQ   TO TRUE.                                    ELSMEMSC
00989      SET IOP-STG-MODE-LOCATE  TO TRUE.                            ELSMEMSC
00990      PERFORM LINK-TO-ELUIOPGM.                                    ELSMEMSC
00991      IF IOP-RC-NOTFND                                             ELSMEMSC
00992          PERFORM ADD-DUMMY-OPTION-OCCURRENCE                      ELSMEMSC
00993      ELSE IF NOT IOP-RC-OK                                        ELSMEMSC
00994          PERFORM SIGNAL-CRITICAL-IO.                              ELSMEMSC
00995      EJECT                                                        ELSMEMSC
00996                                                                   ELSMEMSC
00997                                                                   ELSMEMSC
00998 ************************************************************      ELSMEMSC
00999 *                                                          *      ELSMEMSC
01000 *        CREATE ITEM DESCRIPTION PER SECTION FOUND         *      ELSMEMSC
01001 *                                                          *      ELSMEMSC
01002 ************************************************************      ELSMEMSC
01003  CREATE-ITEM-DESCRIPTION-PER-SE.                                  ELSMEMSC
01004      SET ADDRESS OF GROUP-SPECIFIC                                ELSMEMSC
01005                                        TO IOP-REC-PTR.            ELSMEMSC
01006 *    MOVE 000 TO WS-DTL-PLAN-CODE.                                ELSMEMSC
01007      MOVE MSI-PKG-CODE (MSI-IDX) TO WS-DTL-PKG-CODE.              ELSMEMSC
01008      MOVE MSI-MEMBER-SECTION (MSI-IDX) TO WS-DTL-SECTION.         ELSMEMSC
01009      MOVE GCG-GROUP-SECTION-NAME TO WS-DTL-DESCRIPTION.           ELSMEMSC
01010      PERFORM CONVERT-DETAIL-EFFECTIVE-DATE.                       ELSMEMSC
01011      PERFORM CONVERT-DETAIL-TERMINATION-DAT.                      ELSMEMSC
01012      SET MSD-IDX                 TO 1.                            ELSMEMSC
01013      SET MSD-NBR-DESCR-LINES     TO MSD-IDX.                      ELSMEMSC
01014      MOVE WS-DTL-LN              TO MSD-DESCR-LINE                ELSMEMSC
01015          (MSD-IDX).                                               ELSMEMSC
01016      IF GCG-INTER-RELATIONAL-CODE = ZEROES                        ELSMEMSC
01017          PERFORM PUT-NOT-VIEWABLE-MSG-IN-LINE.                    ELSMEMSC
01018      PERFORM ADD-ITEM-DESCRIPTION-TO-MENU.                        ELSMEMSC
01019      PERFORM ADD-MENU-OPTION-OCCURRENCE.                          ELSMEMSC
01020      EJECT                                                        ELSMEMSC
01021                                                                   ELSMEMSC
01022                                                                   ELSMEMSC
01023 ************************************************************      ELSMEMSC
01024 *                                                          *      ELSMEMSC
01025 *        PUT NOT VIEWABLE MSG IN LINE                      *      ELSMEMSC
01026 *                                                          *      ELSMEMSC
01027 ************************************************************      ELSMEMSC
01028  PUT-NOT-VIEWABLE-MSG-IN-LINE.                                    ELSMEMSC
01029      SET MSD-IDX UP BY 1.                                         ELSMEMSC
01030      SET MSD-NBR-DESCR-LINES     TO MSD-IDX.                      ELSMEMSC
01031      MOVE SPACES                  TO WS-DTL-LN.                   ELSMEMSC
01032      MOVE WS-NOT-VIEWABLE-MSG     TO                              ELSMEMSC
01033          WS-DTL-DESCRIPTION.                                      ELSMEMSC
01034      MOVE WS-DTL-LN               TO MSD-DESCR-LINE (MSD-IDX).    ELSMEMSC
01035      EJECT                                                        ELSMEMSC
01036                                                                   ELSMEMSC
01037                                                                   ELSMEMSC
01038 ************************************************************      ELSMEMSC
01039 *                                                          *      ELSMEMSC
01040 *        CONVERT DETAIL EFFECTIVE DATE                     *      ELSMEMSC
01041 *                                                          *      ELSMEMSC
01042 ************************************************************      ELSMEMSC
01043  CONVERT-DETAIL-EFFECTIVE-DATE.                                   ELSMEMSC
01044      MOVE MSI-EFF-DT    (MSI-IDX) TO HGADATE-JULIAN1.             ELSMEMSC
01045      PERFORM JULIAN-TO-GREG-CONVERSION.                           ELSMEMSC
01046      MOVE HGADATE-DATE2 TO WS-DTL-FROM-DT.                        ELSMEMSC
01047      EJECT                                                        ELSMEMSC
01048                                                                   ELSMEMSC
01049                                                                   ELSMEMSC
01050 ************************************************************      ELSMEMSC
01051 *                                                          *      ELSMEMSC
01052 *        CONVERT DETAIL TERMINATION DATE                   *      ELSMEMSC
01053 *                                                          *      ELSMEMSC
01054 ************************************************************      ELSMEMSC
01055  CONVERT-DETAIL-TERMINATION-DAT.                                  ELSMEMSC
01056      MOVE MSI-TERM-DT   (MSI-IDX) TO HGADATE-JULIAN1.             ELSMEMSC
01057      PERFORM JULIAN-TO-GREG-CONVERSION.                           ELSMEMSC
01058      MOVE HGADATE-DATE2 TO WS-DTL-TO-DT.                          ELSMEMSC
01059      EJECT                                                        ELSMEMSC
01060                                                                   ELSMEMSC
01061                                                                   ELSMEMSC
01062 ************************************************************      ELSMEMSC
01063 *                                                          *      ELSMEMSC
01064 *        ADD MENU OPTION OCCURRENCE                        *      ELSMEMSC
01065 *                                                          *      ELSMEMSC
01066 ************************************************************      ELSMEMSC
01067  ADD-MENU-OPTION-OCCURRENCE.                                      ELSMEMSC
01068      ADD 1 TO SUB1.                                               ELSMEMSC
01069      SET MSO-IDX TO SUB1.                                         ELSMEMSC
01070      IF GCG-INTER-RELATIONAL-CODE = ZEROES                        ELSMEMSC
01071          PERFORM PUT-HIGH-VALUES-IN-OPTION-SELE                   ELSMEMSC
01072      ELSE                                                         ELSMEMSC
01073          PERFORM PUT-SECTION-NUMBER-IN-OPTION-S.                  ELSMEMSC
01074                                                                   ELSMEMSC
01075                                                                   ELSMEMSC
01076 ************************************************************      ELSMEMSC
01077 *                                                          *      ELSMEMSC
01078 *        PUT HIGH VALUES IN OPTION SELECTOR                *      ELSMEMSC
01079 *                                                          *      ELSMEMSC
01080 ************************************************************      ELSMEMSC
01081  PUT-HIGH-VALUES-IN-OPTION-SELE.                                  ELSMEMSC
01082      MOVE HIGH-VALUES             TO MSO-OPT-SEL                  ELSMEMSC
01083          (MSO-IDX)                                                ELSMEMSC
01084                                      MSO-OPT-KWD                  ELSMEMSC
01085          (MSO-IDX).                                               ELSMEMSC
01086                                                                   ELSMEMSC
01087                                                                   ELSMEMSC
01088 ************************************************************      ELSMEMSC
01089 *                                                          *      ELSMEMSC
01090 *        PUT SECTION NUMBER IN OPTION SELECTOR             *      ELSMEMSC
01091 *                                                          *      ELSMEMSC
01092 ************************************************************      ELSMEMSC
01093  PUT-SECTION-NUMBER-IN-OPTION-S.                                  ELSMEMSC
01094      MOVE MSI-MEMBER-SECTION (MSI-IDX) TO                         ELSMEMSC
01095            MSO-OPT-SEL (MSO-IDX)                                  ELSMEMSC
01096      MOVE MSI-MEMBER-SECTION (MSI-IDX) TO WS-OPT-SECT.            ELSMEMSC
01097      MOVE MSI-PKG-CODE (MSI-IDX) TO WS-OPT-PKG.                   ELSMEMSC
01098      MOVE WS-OPT-KEYWORD TO MSO-OPT-KWD (MSO-IDX).                ELSMEMSC
01099      MOVE MSI-MEMBER-SECTION (MSI-IDX) TO MSO-OPT-SEL (MSO-IDX).  ELSMEMSC
01100      EJECT                                                        ELSMEMSC
01101                                                                   ELSMEMSC
01102                                                                   ELSMEMSC
01103 ************************************************************      ELSMEMSC
01104 *                                                          *      ELSMEMSC
01105 *        ADD DUMMY OPTION OCCURRENCE                       *      ELSMEMSC
01106 *                                                          *      ELSMEMSC
01107 ************************************************************      ELSMEMSC
01108  ADD-DUMMY-OPTION-OCCURRENCE.                                     ELSMEMSC
01109      ADD 1 TO SUB1.                                               ELSMEMSC
01110      SET MSO-IDX TO SUB1.                                         ELSMEMSC
01111      MOVE '****'                  TO MSO-OPT-SEL (MSO-IDX)        ELSMEMSC
01112                                      MSO-OPT-KWD (MSO-IDX).       ELSMEMSC
01113      ADD 1 TO WS-DUMMY-OCCUR-CNT.                                 ELSMEMSC
01114                                                                   ELSMEMSC
01115      EJECT                                                        ELSMEMSC
01116                                                                   ELSMEMSC
01117                                                                   ELSMEMSC
01118 ************************************************************      ELSMEMSC
01119 *                                                          *      ELSMEMSC
01120 *        SIGNAL CRITICAL IO                                *      ELSMEMSC
01121 *                                                          *      ELSMEMSC
01122 ************************************************************      ELSMEMSC
01123  SIGNAL-CRITICAL-IO.                                              ELSMEMSC
01124      SET CIA-AB-CRITIO TO TRUE.                                   ELSMEMSC
01125      EXEC CICS ABEND                                              ELSMEMSC
01126                ABCODE(CIA-ABCODE)                                 ELSMEMSC
01127                END-EXEC.                                          ELSMEMSC
01128      EJECT                                                        ELSMEMSC
01129                                                                   ELSMEMSC
01130                                                                   ELSMEMSC
01131 ************************************************************      ELSMEMSC
01132 *                                                          *      ELSMEMSC
01133 *        SIGNAL UNDEFINED                                  *      ELSMEMSC
01134 *                                                          *      ELSMEMSC
01135 ************************************************************      ELSMEMSC
01136  SIGNAL-UNDEFINED.                                                ELSMEMSC
01137      SET CIA-AB-UNDEF TO TRUE.                                    ELSMEMSC
01138      EXEC CICS ABEND                                              ELSMEMSC
01139                ABCODE(CIA-ABCODE)                                 ELSMEMSC
01140                END-EXEC.                                          ELSMEMSC
01141                                                                   ELSMEMSC
01142                                                                   ELSMEMSC
01143 ************************************************************      ELSMEMSC
01144 *                                                          *      ELSMEMSC
01145 *        LINK TO ELUIOPGM                                  *      ELSMEMSC
01146 *                                                          *      ELSMEMSC
01147 ************************************************************      ELSMEMSC
01148  LINK-TO-ELUIOPGM.                                                ELSMEMSC
01149      EXEC CICS LINK                                               ELSMEMSC
01150                PROGRAM('ELUIOPGM')                                ELSMEMSC
01151                COMMAREA(DFHCOMMAREA)                              ELSMEMSC
01152                END-EXEC.                                          ELSMEMSC
01153      EJECT                                                        ELSMEMSC
01154                                                                   ELSMEMSC
01155                                                                   ELSMEMSC
01156 ************************************************************      ELSMEMSC
01157 *                                                          *      ELSMEMSC
01158 *        ADD ITEM DESCRIPTION TO MENU                      *      ELSMEMSC
01159 *                                                          *      ELSMEMSC
01160 ************************************************************      ELSMEMSC
01161  ADD-ITEM-DESCRIPTION-TO-MENU.                                    ELSMEMSC
01162      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSMEMSC
01163      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMEMSC
01164          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSMEMSC
01165      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSMEMSC
01166      SET IOP-ADD TO TRUE.                                         ELSMEMSC
01167      SET IOP-FCQ-NONE TO TRUE.                                    ELSMEMSC
01168      SET IOP-KVQ-NONE TO TRUE.                                    ELSMEMSC
01169      PERFORM LINK-TO-ELUIOPGM.                                    ELSMEMSC
01170                                                                   ELSMEMSC
01171                                                                   ELSMEMSC
01172 ************************************************************      ELSMEMSC
01173 *                                                          *      ELSMEMSC
01174 *        COMPLETE PROCESS FOR SECTION MENU                 *      ELSMEMSC
01175 *                                                          *      ELSMEMSC
01176 ************************************************************      ELSMEMSC
01177  COMPLETE-PROCESS-FOR-SECTION-M.                                  ELSMEMSC
01178      MOVE SSB-MNU-CHOICE (1) TO WS-REFORMAT-VALUE.                ELSMEMSC
01179      IF DUMMY-SECTION                                             ELSMEMSC
01180          PERFORM SEND-MSG-2                                       ELSMEMSC
01181      ELSE                                                         ELSMEMSC
01182          PERFORM ISSUE-LINK-TO-ELUKTYTAB.                         ELSMEMSC
01183      EJECT                                                        ELSMEMSC
01184                                                                   ELSMEMSC
01185                                                                   ELSMEMSC
01186 ************************************************************      ELSMEMSC
01187 *                                                          *      ELSMEMSC
01188 *        ISSUE LINK TO ELUKTYTAB                           *      ELSMEMSC
01189 *                                                          *      ELSMEMSC
01190 ************************************************************      ELSMEMSC
01191  ISSUE-LINK-TO-ELUKTYTAB.                                         ELSMEMSC
01192      MOVE WS-SECTION      TO SSB-SECTN-NO.                        ELSMEMSC
01193      EXEC CICS LINK                                               ELSMEMSC
01194                PROGRAM ('ELUKYTAB')                               ELSMEMSC
01195                COMMAREA(DFHCOMMAREA)                              ELSMEMSC
01196                END-EXEC.                                          ELSMEMSC
01197      IF CIA-RC-MEMB-NO-SECTN-INFO                                 ELSMEMSC
01198          PERFORM SET-RETURN-CODE-TO-ZERO.                         ELSMEMSC
01199      IF CIA-RC-KTB-GRP-NOTFND  OR                                 ELSMEMSC
01200                   CIA-RC-KTB-SECTN-NOTFND                         ELSMEMSC
01201          PERFORM SEND-MSG-1                                       ELSMEMSC
01202      ELSE IF NOT CIA-RC-OK                                        ELSMEMSC
01203          PERFORM SIGNAL-UNDEFINED                                 ELSMEMSC
01204      ELSE                                                         ELSMEMSC
01205          PERFORM CONTINUE-PROCESS.                                ELSMEMSC
01206                                                                   ELSMEMSC
01207                                                                   ELSMEMSC
01208 ************************************************************      ELSMEMSC
01209 *                                                          *      ELSMEMSC
01210 *        SET RETURN CODE TO ZERO                           *      ELSMEMSC
01211 *                                                          *      ELSMEMSC
01212 ************************************************************      ELSMEMSC
01213  SET-RETURN-CODE-TO-ZERO.                                         ELSMEMSC
01214      SET CIA-RC-OK TO TRUE.                                       ELSMEMSC
01215                                                                   ELSMEMSC
01216                                                                   ELSMEMSC
01217 ************************************************************      ELSMEMSC
01218 *                                                          *      ELSMEMSC
01219 *        CONTINUE PROCESS                                  *      ELSMEMSC
01220 *                                                          *      ELSMEMSC
01221 ************************************************************      ELSMEMSC
01222  CONTINUE-PROCESS.                                                ELSMEMSC
01223      SET SSB-COMPLETED (SSB-SELECTOR-STATE) TO TRUE.              ELSMEMSC
01224      EJECT                                                        ELSMEMSC
01225                                                                   ELSMEMSC
01226                                                                   ELSMEMSC
01227 ************************************************************      ELSMEMSC
01228 *                                                          *      ELSMEMSC
01229 *        SEND MSG 1                                        *      ELSMEMSC
01230 *                                                          *      ELSMEMSC
01231 ************************************************************      ELSMEMSC
01232  SEND-MSG-1.                                                      ELSMEMSC
01233      SET SSB-IN-MENU   (SSB-SELECTOR-STATE) TO TRUE.              ELSMEMSC
01234      MOVE ERROR-MSG-1 TO ERRMSGO.                                 ELSMEMSC
01235      EXEC CICS SEND MAP('EL00MAP')                                ELSMEMSC
01236                MAPSET  ('EL00SET')                                ELSMEMSC
01237                ERASE                                              ELSMEMSC
01238                END-EXEC.                                          ELSMEMSC
01239      EJECT                                                        ELSMEMSC
01240                                                                   ELSMEMSC
01241                                                                   ELSMEMSC
01242 ************************************************************      ELSMEMSC
01243 *                                                          *      ELSMEMSC
01244 *        SEND MSG 2                                        *      ELSMEMSC
01245 *                                                          *      ELSMEMSC
01246 ************************************************************      ELSMEMSC
01247  SEND-MSG-2.                                                      ELSMEMSC
01248      SET SSB-IN-MENU   (SSB-SELECTOR-STATE) TO TRUE.              ELSMEMSC
01249      MOVE ERROR-MSG-2 TO ERRMSGO.                                 ELSMEMSC
01250      EXEC CICS SEND MAP('EL00MAP')                                ELSMEMSC
01251                MAPSET  ('EL00SET')                                ELSMEMSC
01252                ERASE                                              ELSMEMSC
01253                END-EXEC.                                          ELSMEMSC
01254      EJECT                                                        ELSMEMSC
01255                                                                   ELSMEMSC
01256                                                                   ELSMEMSC
01257 ************************************************************      ELSMEMSC
01258 *                                                          *      ELSMEMSC
01259 *        JULIAN TO GREG CONVERSION                         *      ELSMEMSC
01260 *                                                          *      ELSMEMSC
01261 ************************************************************      ELSMEMSC
01262  JULIAN-TO-GREG-CONVERSION.                                       ELSMEMSC
01263      MOVE 'CNV'     TO HGADATE-FUNC.                              ELSMEMSC
01264      MOVE 'J'       TO HGADATE-FORM1.                             ELSMEMSC
01265      MOVE 'M'       TO HGADATE-FORM2.                             ELSMEMSC
01266      MOVE ZEROES    TO HGADATE-RETURN                             ELSMEMSC
01267                        HGADATE-DATE2.                             ELSMEMSC
01268      EXEC CICS LINK PROGRAM('HGADATES')                           ELSMEMSC
01269                     COMMAREA(WS-HGADATES-PARMS)                   ELSMEMSC
01270                     END-EXEC.                                     ELSMEMSC
01271      IF HGADATE-RETURN NOT = '00'                                 ELSMEMSC
01272          PERFORM INITIALIZE-RETURNED-DATE.                        ELSMEMSC
01273                                                                   ELSMEMSC
01274                                                                   ELSMEMSC
01275 ************************************************************      ELSMEMSC
01276 *                                                          *      ELSMEMSC
01277 *        INITIALIZE RETURNED DATE                          *      ELSMEMSC
01278 *                                                          *      ELSMEMSC
01279 ************************************************************      ELSMEMSC
01280  INITIALIZE-RETURNED-DATE.                                        ELSMEMSC
01281      MOVE ZEROES TO HGADATE-DATE2.                                ELSMEMSC
01282                                                                   ELSMEMSC
01283                                                                   ELSMEMSC
01284 ************************************************************      ELSMEMSC
01285 *                                                          *      ELSMEMSC
01286 *        RETURN TO CALLER                                  *      ELSMEMSC
01287 *                                                          *      ELSMEMSC
01288 ************************************************************      ELSMEMSC
01289  RETURN-TO-CALLER.                                                ELSMEMSC
01290      EXEC CICS RETURN                                             ELSMEMSC
01291                END-EXEC.                                          ELSMEMSC
01292      GOBACK.                                                      ELSMEMSC
