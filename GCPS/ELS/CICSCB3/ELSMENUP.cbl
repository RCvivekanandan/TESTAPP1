00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELSMENUP
00003  PROGRAM-ID.         ELSMENUP.                                       LV002
00004                                                                   ELSMENUP
00005  AUTHOR.             EDWARD G LISS                                ELSMENUP
00006                                                                   ELSMENUP
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSMENUP
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELSMENUP
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSMENUP
00010                      233 N. MICHIGAN AVE                          ELSMENUP
00011                      CHICAGO, ILLINOIS 60601                      ELSMENUP
00012                                                                   ELSMENUP
00013  DATE-WRITTEN.       07-NOV-1986.                                 ELSMENUP
00014                                                                   ELSMENUP
00015  DATE-COMPILED.                                                   ELSMENUP
00016                                                                   ELSMENUP
00017  SECURITY.           COPYRIGHT 1986,                              ELSMENUP
00018                      HEALTH CARE SERVICE CORPORATION              ELSMENUP
00019      SKIP3                                                        ELSMENUP
00020  ENVIRONMENT DIVISION.                                            ELSMENUP
00021                                                                   ELSMENUP
00022  CONFIGURATION SECTION.                                           ELSMENUP
00023  SOURCE-COMPUTER.    IBM-3033.                                    ELSMENUP
00024  OBJECT-COMPUTER.    IBM-3033.                                    ELSMENUP
00025      EJECT                                                        ELSMENUP
00026 ******************************************************************ELSMENUP
00027 *                                                                *ELSMENUP
00028 *    PROGRAM:    ELSMENUP                                        *ELSMENUP
00029 *    DATE:       07-NOV-1986                                     *ELSMENUP
00030 *    AUTHOR:     EDWARD G LISS                                   *ELSMENUP
00031 *    FUNCTION:                                                   *ELSMENUP
00032 *      THIS MODULE PERFORM MENU PROCESSING.  IT FORMATS THE      *ELSMENUP
00033 *      MENU AND SCROLLS THOUGH IT.  THE USER  MAKES A CHOICE     *ELSMENUP
00034 *      FROM THE LIST PROVIDED OR MAY OPTIONALY TAKE A DEFAULT    *ELSMENUP
00035 *      VALUE.  ALL CHOICES ARE VALIDATED FOR THE REQUESTING      *ELSMENUP
00036 *      PROGRAM.                                                  *ELSMENUP
00037 *                                                                *ELSMENUP
00038 ******************************************************************ELSMENUP
00039 *                                                                *ELSMENUP
00040 *                      MAINTENANCE HISTORY                       *ELSMENUP
00041 *                                                                *ELSMENUP
00042 *  MOD     DATE     BY  DRPT                ACTION               *ELSMENUP
00043 * ----- ----------- --- ----- ---------------------------------- *ELSMENUP
00044 * 01.00 07-NOV-1986 EGL       CREATED                            *ELSMENUP
00045 * 01.10 12-FEB-1987 EGL       ADDED THE ABILITY TO SELECT ON     *ELSMENUP
00046 *                             THE PRIMARY CHOICE OR KEYWORD.     *ELSMENUP
00047 * 01.20 05-OCT-1987 EGL       ADDED THE ABILITY TO SUPPORT       *ELSMENUP
00048 *                             MULTIPLE SELECTIONS IN ONE CALL    *ELSMENUP
00049 * 01.30 02-FEB-1988 EGL       ADDED SUPPORT FOR NO RESPONSE      *ELSMENUP
00050 *                             MENUS.                             *ELSMENUP
00051 * 01.40 12-DEC-1988 EGL       ADDED CODE TO PREVENT A MENU FROM  *ELSMENUP
00052 *                             APPEARING ON THE STACK MORE THAN   *ELSMENUP
00053 *                             ONCE.  ALSO, CONVERTED TO NEW      *ELSMENUP
00054 *                             STORAGE MANAGEMENT ROUTINES.       *ELSMENUP
00055 * 01.50 21-DEC-1988 EGL       ADDED MOVE STATEMENT TO INITIALIZE *ELSMENUP
00056 *                             MAX OCCURS FOR MENU HEADING WHEN   *ELSMENUP
00057 *                             MODULE IS RE-ENTERED.  THIS COR-   *ELSMENUP
00058 *                             RECTED THE STRANGE EL10 ABENDS.    *ELSMENUP
00059 * 01.60 24-AUG-1989 EGL       DESTRUCTED PROGRAM.                *ELSMENUP
00060 * 01.70 22-MAR-1993 AKK       CHANGED PATIENT AGE HEADING TO READ*ELSMENUP
00061 *                             MEMBER, SPOUSE, OR DEPENDENT DUE   *ELSMENUP
00062 *                             TO TRUNCATIONS USING CURRENT METHOD*ELSMENUP
00063 *                                                                *ELSMENUP
00064 *       13-APR-2003 AKK       REGEN TO TEST ORDER OF COMPILES    *ELSMENUP
00065 ******************************************************************ELSMENUP
00066      TITLE 'ELS MENU PROCESSOR'.                                  ELSMENUP
00067  DATA DIVISION.                                                   ELSMENUP
00068                                                                   ELSMENUP
00069  WORKING-STORAGE SECTION.                                         ELSMENUP
00070                                                                   ELSMENUP
00071  01  MISC-WORKING-STORAGE.                                        ELSMENUP
00072      05  FILLER            PICTURE X(16) VALUE '*WS START HERE'.  ELSMENUP
00073      05  WS-COLON          PICTURE X  VALUE ':'.                  ELSMENUP
00074      05  WS-QUOTE          PICTURE X  VALUE QUOTE.                ELSMENUP
00075      05  WS-MAX-CHOICES    PICTURE S9(4) COMP SYNC VALUE 30.      ELSMENUP
00076      05  WS-LENGTH-CHECK   PIC 9(08).                             ELSMENUP
00077                                                                   ELSMENUP
00078      05  WS-EL02MAPI-LEN   PICTURE S9(4) COMP SYNC.               ELSMENUP
00079      05  WS-SELECTOR-STATE PICTURE S9(4) COMP SYNC.               ELSMENUP
00080      05  WS-DESCR-SUB      PICTURE S9(4) COMP SYNC.               ELSMENUP
00081      05  WS-CLEAR-SUB      PICTURE S9(4) COMP SYNC.               ELSMENUP
00082      05  WS-SEND-SUB       PICTURE S9(4) COMP SYNC.               ELSMENUP
00083      05  WS-RECV-SUB       PICTURE S9(4) COMP SYNC.               ELSMENUP
00084      05  WS-SELECTN-LENGTH PICTURE S9(4) COMP SYNC.               ELSMENUP
00085      05  WS-MOVE-POS       PICTURE S9(4) COMP SYNC.               ELSMENUP
00086      05  WS-EXT-MOVE-POS   PICTURE S9(4) COMP SYNC.               ELSMENUP
00087      05  WS-EXT-END-POS    PICTURE S9(4) COMP SYNC.               ELSMENUP
00088      05  WS-BOTTOM-DESCR   PICTURE S9(4) COMP SYNC.               ELSMENUP
00089      05  WS-DUP-SUB        PICTURE S9(4) COMP SYNC.               ELSMENUP
00090      05  WS-ELSMHDG-MVO    PICTURE S9(4) COMP SYNC.               ELSMENUP
00091      05  WS-CHOICE-LENGTH  PICTURE S9(8) COMP SYNC.               ELSMENUP
00092      05  WS-SENDER-LENGTH  PICTURE S9(8) COMP SYNC.               ELSMENUP
00093      05  WS-RECEIVER-LENGTH PICTURE S9(8) COMP SYNC.              ELSMENUP
00094                                                                   ELSMENUP
00095      05  WS-PAD-CHAR       PICTURE X     VALUE SPACE.             ELSMENUP
00096      05  WS-EDIT-TWO       PICTURE Z9.                            ELSMENUP
00097                                                                   ELSMENUP
00098      05  WS-SCREEN-OVERFLOW-SW PICTURE X VALUE 'N'.               ELSMENUP
00099          88 WS-SCREEN-OVERFLOW           VALUE 'Y'.               ELSMENUP
00100          88 WS-NO-SCREEN-OVERFLOW        VALUE 'N'.               ELSMENUP
00101                                                                   ELSMENUP
00102      05  WS-ERROR-MESSAGE-SW PICTURE X   VALUE 'N'.               ELSMENUP
00103          88 WS-ERROR-MESSAGE-ISSUED      VALUE 'Y'.               ELSMENUP
00104          88 WS-NO-ERROR-MESSAGE          VALUE 'N'.               ELSMENUP
00105                                                                   ELSMENUP
00106      05  WS-PARSE-SW         PICTURE X   VALUE 'N'.               ELSMENUP
00107          88 WS-PARSE-COMPLETED           VALUE 'Y'.               ELSMENUP
00108          88 WS-PARSE-NOT-COMPLETED       VALUE 'N'.               ELSMENUP
00109                                                                   ELSMENUP
00110      05  WS-SELECTION-FOUND-SW PICTURE X VALUE 'N'.               ELSMENUP
00111          88 WS-SELECTION-FOUND           VALUE 'Y'.               ELSMENUP
00112          88 WS-SELECTION-NOT-FOUND       VALUE 'N'.               ELSMENUP
00113                                                                   ELSMENUP
00114      05  WS-STOW-MENU-ITEMS-SW PICTURE X VALUE 'N'.               ELSMENUP
00115          88 WS-STOW-MENU-ITEMS           VALUE 'Y'.               ELSMENUP
00116          88 WS-DONT-STOW-MENU-ITEMS      VALUE 'N'.               ELSMENUP
00117                                                                   ELSMENUP
00118      05  WS-SEND-INITIAL-SW PICTURE X    VALUE 'N'.               ELSMENUP
00119          88 WS-SEND-INITIAL-SCREEN       VALUE 'Y'.               ELSMENUP
00120          88 WS-NOT-INITIAL-SCREEN        VALUE 'N'.               ELSMENUP
00121                                                                   ELSMENUP
00122      05  WS-DUP-STACK-ITEMS PICTURE X    VALUE 'N'.               ELSMENUP
00123          88 WS-DUP-FOUND                 VALUE 'Y'.               ELSMENUP
00124          88 WS-DUP-NOT-FOUND             VALUE 'N'.               ELSMENUP
00125                                                                   ELSMENUP
00126      05  WS-SCROLL-IND     PICTURE X     VALUE SPACE.             ELSMENUP
00127          88 WS-NO-SCROLL                 VALUE SPACE.             ELSMENUP
00128          88 WS-SCROLL-DOWN               VALUE 'D'.               ELSMENUP
00129          88 WS-SCROLL-UP                 VALUE 'U'.               ELSMENUP
00130          88 WS-SCROLL-TOP                VALUE 'T'.               ELSMENUP
00131          88 WS-SCROLL-BOTTOM             VALUE 'B'.               ELSMENUP
00132          88 WS-SCROLL                    VALUE 'D', 'U',          ELSMENUP
00133                                                    'T', 'B'.      ELSMENUP
00134 * TEXAS REGIONS FOR PACKAGE CODE CHECK                            ELSMENUP
00135      05  WS-APPLID.                                               ELSMENUP
00136          10 FILLER                   PIC X(03).                   ELSMENUP
00137          10 FILLER                   PIC X(04).                   ELSMENUP
00138             88 TEXAS-REGION          VALUES                       ELSMENUP
00139               'XAI1' 'XAI2' 'XAB1' 'XAB2' 'XAB3' 'XAB4' 'XAB5'    ELSMENUP
00140               'XAB6' 'XAB7' 'XAB8' 'XAB9' 'XAS1' 'XAS2' 'XFB1'    ELSMENUP
00141               'XFB2' 'XF01'.                                      ELSMENUP
00142                                                                   ELSMENUP
00143      05  WS-GROUPS-TO-SKIP.                                       ELSMENUP
00144          10 WS-CHANGE-TX-GROUP-CHECK PIC X(06).                   ELSMENUP
00145             88  WS-CHANGE-TX-PKG-CODE-GROUP                       ELSMENUP
00146                  VALUES '0FEPTX' '051200' '051201'                ELSMENUP
00147                         '051300' '051301' '000600'                ELSMENUP
00148                         '061100' '061500' '061600'                ELSMENUP
00149                         '071100'.                                 ELSMENUP
00150                                                                   ELSMENUP
00151      05  WS-DATE-AREA.                                            ELSMENUP
00152          10 WS-DATE-EDIT   PICTURE 99/99/99.                      ELSMENUP
00153                                                                   ELSMENUP
00154      05  WS-TIME-AREA.                                            ELSMENUP
00155          10 WS-TIME-EDIT   PICTURE 99B99B99.                      ELSMENUP
00156          10 FILLER  REDEFINES WS-TIME-EDIT.                       ELSMENUP
00157              15 FILLER     PICTURE 99.                            ELSMENUP
00158              15 WS-COLON-1 PICTURE X.                             ELSMENUP
00159              15 FILLER     PICTURE 99.                            ELSMENUP
00160              15 WS-COLON-2 PICTURE X.                             ELSMENUP
00161              15 FILLER     PICTURE 99.                            ELSMENUP
00162                                                                   ELSMENUP
00163      05  WS-ONE-CHOICE-PERMITTED PICTURE X(42) VALUE              ELSMENUP
00164          'ONLY ONE CHOICE IS PERMITTED AT THIS TIME.'.            ELSMENUP
00165      05  WS-HOLD-RESPONSE PICTURE X(80) VALUE SPACES.             ELSMENUP
00166      EJECT                                                        ELSMENUP
00167  01  WS-MENU-OPT.                                                 ELSMENUP
00168      05  WS-OPT-SEL          PICTURE X(8).                        ELSMENUP
00169      05  FILLER              REDEFINES WS-OPT-SEL.                ELSMENUP
00170          10  FILLER          PICTURE X(7).                        ELSMENUP
00171          10  WS-OPT-NUM-1    PICTURE X(1).                        ELSMENUP
00172      05  FILLER              REDEFINES WS-OPT-SEL.                ELSMENUP
00173          10  FILLER          PICTURE X(6).                        ELSMENUP
00174          10  WS-OPT-NUM-2    PICTURE X(2).                        ELSMENUP
00175      05  FILLER              REDEFINES WS-OPT-SEL.                ELSMENUP
00176          10  FILLER          PICTURE X(5).                        ELSMENUP
00177          10  WS-OPT-NUM-3    PICTURE X(3).                        ELSMENUP
00178      05  FILLER              REDEFINES WS-OPT-SEL.                ELSMENUP
00179          10  FILLER          PICTURE X(4).                        ELSMENUP
00180          10  WS-OPT-NUM-4    PICTURE X(4).                        ELSMENUP
00181      05  FILLER              REDEFINES WS-OPT-SEL.                ELSMENUP
00182          10  FILLER          PICTURE X(3).                        ELSMENUP
00183          10  WS-OPT-NUM-5    PICTURE X(5).                        ELSMENUP
00184      05  WS-SEARCH-KEY       PICTURE X(8).                        ELSMENUP
00185      05  WS-KEYWORD          PICTURE X(16).                       ELSMENUP
00186                                                                   ELSMENUP
00187  01  WS-CENTER-WORK-AREA.                                         ELSMENUP
00188      05  WS-CENTER-AREA      PICTURE X(50).                       ELSMENUP
00189      05  WS-CENTER-BYTES     REDEFINES WS-CENTER-AREA.            ELSMENUP
00190          10  WS-CENTER-CHAR  OCCURS 50 TIMES                      ELSMENUP
00191                              INDEXED BY WS-FIRST-NON-BLANK-IDX    ELSMENUP
00192                                         WS-LAST-NON-BLANK-IDX     ELSMENUP
00193                              PICTURE X.                           ELSMENUP
00194      05  WS-CENTER-START-POS PICTURE S9(4) COMP SYNC.             ELSMENUP
00195      05  WS-CENTER-END-POS   PICTURE S9(4) COMP SYNC.             ELSMENUP
00196      05  WS-CENTER-LENGTH    PICTURE S9(4) COMP SYNC.             ELSMENUP
00197      05  WS-CENTER-MOVE-POS  PICTURE S9(4) COMP SYNC.             ELSMENUP
00198      05  WS-CENTER-FILL      PICTURE X     VALUE SPACE.           ELSMENUP
00199      EJECT                                                        ELSMENUP
00200  01  FILLER                  PICTURE X(16) VALUE '***HGADATES***'.ELSMENUP
00201  01  WS-HGADATES-PARMS.                                           ELSMENUP
00202      COPY HGCDAT01.                                               ELSMENUP
00203                                                                   ELSMENUP
00204  01  WS-TITLE-ITEMS.                                              ELSMENUP
00205    05  WS-ITEM-1.                                                 ELSMENUP
00206      10  FILLER                    PIC X(7)  VALUE 'GROUP: '.     ELSMENUP
00207      10  WS-HDR1-GROUP-NO          PIC X(6).                      ELSMENUP
00208    05  WS-ITEM-2.                                                 ELSMENUP
00209      10  FILLER                    PIC X(10) VALUE                ELSMENUP
00210          ' SECTION: '.                                            ELSMENUP
00211      10  WS-HDR1-SECT-NO           PIC X(5).                      ELSMENUP
00212    05  WS-ITEM-3.                                                 ELSMENUP
00213      10  WS-HDR1-FAM-REL           PIC X(22).                     ELSMENUP
00214    05  WS-ITEM-4.                                                 ELSMENUP
00215      10  FILLER                    PIC X(07) VALUE ' FROM: '.     ELSMENUP
00216      10  WS-HDR1-FROM-DATE         PIC 99/99/99.                  ELSMENUP
00217    05  WS-ITEM-5.                                                 ELSMENUP
00218      10  FILLER                    PIC X(05) VALUE                ELSMENUP
00219          ' TO: '.                                                 ELSMENUP
00220      10  WS-HDR1-TO-DATE           PIC 99/99/99.                  ELSMENUP
00221    05  WS-ITEM-6.                                                 ELSMENUP
00222      10  FILLER                    PIC X(13) VALUE                ELSMENUP
00223          ' SUBSCRIBER: '.                                         ELSMENUP
00224      10  WS-SUBSCR-NO              PIC X(12).                     ELSMENUP
00225      EJECT                                                        ELSMENUP
00226  01  FILLER                PICTURE X(16) VALUE '***SYM MAPS***'.  ELSMENUP
00227      COPY EL00SETC.                                               ELSMENUP
00228                                                                   ELSMENUP
00229      COPY EL02SETC.                                               ELSMENUP
00230      EJECT                                                        ELSMENUP
00231      COPY DFHAID.                                                 ELSMENUP
00232      EJECT                                                        ELSMENUP
00233      COPY DFHBMSCA.                                               ELSMENUP
00234      EJECT                                                        ELSMENUP
00235  01  FILLER                  PICTURE X(16) VALUE '***ELSTCWA***'. ELSMENUP
00236      COPY ELSTCWAC.                                               ELSMENUP
00237      EJECT                                                        ELSMENUP
00238  LINKAGE SECTION.                                                 ELSMENUP
00239                                                                   ELSMENUP
00240  01  DFHCOMMAREA.                                                 ELSMENUP
00241  COPY ELSCOMMC.                                                   ELSMENUP
00242      EJECT                                                        ELSMENUP
00243  COPY ELSCIA2C.                                                   ELSMENUP
00244      EJECT                                                        ELSMENUP
00245  COPY ELSSSCBC.                                                   ELSMENUP
00246      EJECT                                                        ELSMENUP
00247  COPY ELSMHDGC.                                                   ELSMENUP
00248      EJECT                                                        ELSMENUP
00249  COPY ELSMOPTC.                                                   ELSMENUP
00250      EJECT                                                        ELSMENUP
00251  COPY ELSMENUC.                                                   ELSMENUP
00252      EJECT                                                        ELSMENUP
00253  COPY ELSIOPMC.                                                   ELSMENUP
00254      EJECT                                                        ELSMENUP
00255  COPY ELSCMIFC.                                                   ELSMENUP
00256      EJECT                                                        ELSMENUP
00257  COPY ELSCMDSC.                                                   ELSMENUP
00258      EJECT                                                        ELSMENUP
00259  PROCEDURE DIVISION.                                              ELSMENUP
00260 ************************************************************      ELSMENUP
00261 *                                                          *      ELSMENUP
00262 *        MENU PROCESSING                                   *      ELSMENUP
00263 *                                                          *      ELSMENUP
00264 ************************************************************      ELSMENUP
00265  MENU-PROCESSING.                                                 ELSMENUP
00266      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELSMENUP
00267          PERFORM INVALID-COMMAREA-ABEND.                          ELSMENUP
00268      PERFORM INITIALIZATION.                                      ELSMENUP
00269      EXEC CICS ASSIGN APPLID (WS-APPLID) END-EXEC.                ELSMENUP
00270      MOVE SSB-GRP-NO TO WS-CHANGE-TX-GROUP-CHECK                  ELSMENUP
00271      PERFORM MAIN-PROCESSING.                                     ELSMENUP
00272      PERFORM TERMINATION.                                         ELSMENUP
00273                                                                   ELSMENUP
00274                                                                   ELSMENUP
00275 ************************************************************      ELSMENUP
00276 *                                                          *      ELSMENUP
00277 *        INITIALIZATION                                    *      ELSMENUP
00278 *                                                          *      ELSMENUP
00279 ************************************************************      ELSMENUP
00280  INITIALIZATION.                                                  ELSMENUP
00281      PERFORM ESTABLISH-ADDRESSABILITY.                            ELSMENUP
00282      IF SSB-START-MENU (SSB-SELECTOR-STATE)                       ELSMENUP
00283          PERFORM NEW-MENU-INITIALIZATION                          ELSMENUP
00284      ELSE IF SSB-IN-MENU (SSB-SELECTOR-STATE)                     ELSMENUP
00285          PERFORM REENTRY-INITIALIZATION                           ELSMENUP
00286      ELSE                                                         ELSMENUP
00287          PERFORM SYSTEM-LOGIC-ERROR.                              ELSMENUP
00288      MOVE SPACES TO ERRMSGO.                                      ELSMENUP
00289      SET WS-NO-ERROR-MESSAGE TO TRUE.                             ELSMENUP
00290 /***********************************************************      ELSMENUP
00291 *                                                          *      ELSMENUP
00292 *        ESTABLISH ADDRESSABILITY                          *      ELSMENUP
00293 *                                                          *      ELSMENUP
00294 ************************************************************      ELSMENUP
00295  ESTABLISH-ADDRESSABILITY.                                        ELSMENUP
00296      CALL 'ELUINISM' USING DFHCOMMAREA                            ELSMENUP
00297          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELSMENUP
00298      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSMENUP
00299      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMENUP
00300          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELSMENUP
00301      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSMENUP
00302      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMENUP
00303          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSMENUP
00304      IF CIA-RC-PTR-NULL                                           ELSMENUP
00305          PERFORM ALLOCATE-IO-BLOCK.                               ELSMENUP
00306      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELSMENUP
00307      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMENUP
00308          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELSMENUP
00309      IF CIA-RC-PTR-NULL                                           ELSMENUP
00310          PERFORM ALLOCATE-CODES-MANUAL-INTERFAC.                  ELSMENUP
00311                                                                   ELSMENUP
00312                                                                   ELSMENUP
00313                                                                   ELSMENUP
00314 ************************************************************      ELSMENUP
00315 *                                                          *      ELSMENUP
00316 *        ALLOCATE IO BLOCK                                 *      ELSMENUP
00317 *                                                          *      ELSMENUP
00318 ************************************************************      ELSMENUP
00319  ALLOCATE-IO-BLOCK.                                               ELSMENUP
00320      SET CIA-ELSMENU-DDN     TO  TRUE.                            ELSMENUP
00321      SET CIA-STG-GETMAIN     TO  TRUE.                            ELSMENUP
00322      PERFORM CALL-STORAGE-MANAGER.                                ELSMENUP
00323      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSMENUP
00324      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMENUP
00325          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSMENUP
00326 /***********************************************************      ELSMENUP
00327 *                                                          *      ELSMENUP
00328 *        ALLOCATE CODES MANUAL INTERFACE AREA              *      ELSMENUP
00329 *                                                          *      ELSMENUP
00330 ************************************************************      ELSMENUP
00331  ALLOCATE-CODES-MANUAL-INTERFAC.                                  ELSMENUP
00332      SET CIA-ELSCMIF-DDN      TO TRUE.                            ELSMENUP
00333      SET CIA-STG-GETMAIN      TO TRUE.                            ELSMENUP
00334      PERFORM CALL-STORAGE-MANAGER.                                ELSMENUP
00335      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELSMENUP
00336      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMENUP
00337          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELSMENUP
00338                                                                   ELSMENUP
00339                                                                   ELSMENUP
00340 ************************************************************      ELSMENUP
00341 *                                                          *      ELSMENUP
00342 *        NEW MENU INITIALIZATION                           *      ELSMENUP
00343 *                                                          *      ELSMENUP
00344 ************************************************************      ELSMENUP
00345  NEW-MENU-INITIALIZATION.                                         ELSMENUP
00346      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSMENUP
00347      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMENUP
00348          ADDRESS OF MHD-MENU-HEADINGS.                            ELSMENUP
00349      IF CIA-RC-PTR-NULL                                           ELSMENUP
00350          PERFORM MISSING-MENU-PARM-ABEND.                         ELSMENUP
00351      MOVE CIA-MVO TO WS-ELSMHDG-MVO.                              ELSMENUP
00352      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSMENUP
00353      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMENUP
00354          ADDRESS OF MSO-MENU-SELECTION-VALUES.                    ELSMENUP
00355      IF CIA-RC-PTR-NULL                                           ELSMENUP
00356          PERFORM MISSING-MENU-PARM-ABEND.                         ELSMENUP
00357      IF MSO-MIN-CHOICES = ZERO AND                                ELSMENUP
00358                  MSO-MAX-CHOICES = ZERO                           ELSMENUP
00359          CONTINUE                                                 ELSMENUP
00360      ELSE IF MSO-MIN-CHOICES < 1                                  ELSMENUP
00361               OR MSO-MAX-CHOICES > WS-MAX-CHOICES                 ELSMENUP
00362               OR MSO-MIN-CHOICES > MSO-MAX-CHOICES                ELSMENUP
00363          PERFORM PARAMETER-ERROR-ABEND.                           ELSMENUP
00364      SET WS-SEND-INITIAL-SCREEN TO TRUE.                          ELSMENUP
00365      SET WS-STOW-MENU-ITEMS TO TRUE.                              ELSMENUP
00366 /***********************************************************      ELSMENUP
00367 *                                                          *      ELSMENUP
00368 *        REENTRY INITIALIZATION                            *      ELSMENUP
00369 *                                                          *      ELSMENUP
00370 ************************************************************      ELSMENUP
00371  REENTRY-INITIALIZATION.                                          ELSMENUP
00372      PERFORM RETRIEVE-MENU-HEADINGS.                              ELSMENUP
00373      PERFORM RETRIEVE-MENU-OPTIONS.                               ELSMENUP
00374      SET WS-DONT-STOW-MENU-ITEMS TO TRUE.                         ELSMENUP
00375      SET WS-NOT-INITIAL-SCREEN TO TRUE.                           ELSMENUP
00376                                                                   ELSMENUP
00377                                                                   ELSMENUP
00378 ************************************************************      ELSMENUP
00379 *                                                          *      ELSMENUP
00380 *        RETRIEVE MENU HEADINGS                            *      ELSMENUP
00381 *                                                          *      ELSMENUP
00382 ************************************************************      ELSMENUP
00383  RETRIEVE-MENU-HEADINGS.                                          ELSMENUP
00384      SET CIA-ELSMHDG-DDN        TO TRUE.                          ELSMENUP
00385      SET CIA-STG-RETRIEVE       TO TRUE.                          ELSMENUP
00386      PERFORM CALL-STORAGE-MANAGER.                                ELSMENUP
00387      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSMENUP
00388      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMENUP
00389          ADDRESS OF MHD-MENU-HEADINGS.                            ELSMENUP
00390      MOVE CIA-MVO TO WS-ELSMHDG-MVO.                              ELSMENUP
00391                                                                   ELSMENUP
00392                                                                   ELSMENUP
00393 ************************************************************      ELSMENUP
00394 *                                                          *      ELSMENUP
00395 *        RETRIEVE MENU OPTIONS                             *      ELSMENUP
00396 *                                                          *      ELSMENUP
00397 ************************************************************      ELSMENUP
00398  RETRIEVE-MENU-OPTIONS.                                           ELSMENUP
00399      SET CIA-ELSMOPT-DDN        TO TRUE.                          ELSMENUP
00400      SET CIA-STG-RETRIEVE       TO TRUE.                          ELSMENUP
00401      PERFORM CALL-STORAGE-MANAGER.                                ELSMENUP
00402      SET CIA-ELSMOPT-DDN        TO TRUE.                          ELSMENUP
00403      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSMENUP
00404          ADDRESS OF MSO-MENU-SELECTION-VALUES.                    ELSMENUP
00405 /***********************************************************      ELSMENUP
00406 *                                                          *      ELSMENUP
00407 *        MAIN PROCESSING                                   *      ELSMENUP
00408 *                                                          *      ELSMENUP
00409 ************************************************************      ELSMENUP
00410  MAIN-PROCESSING.                                                 ELSMENUP
00411      IF SSB-IN-MENU (SSB-SELECTOR-STATE)                          ELSMENUP
00412          PERFORM PROCESS-INPUT.                                   ELSMENUP
00413      IF SSB-START-MENU (SSB-SELECTOR-STATE)                       ELSMENUP
00414               OR SSB-IN-MENU (SSB-SELECTOR-STATE)                 ELSMENUP
00415          PERFORM FORMAT-MENU                                      ELSMENUP
00416      ELSE IF NOT SSB-MENU-COMPLETE (SSB-SELECTOR-STATE)           ELSMENUP
00417          PERFORM SYSTEM-LOGIC-ERROR.                              ELSMENUP
00418                                                                   ELSMENUP
00419                                                                   ELSMENUP
00420 ************************************************************      ELSMENUP
00421 *                                                          *      ELSMENUP
00422 *        PROCESS INPUT                                     *      ELSMENUP
00423 *                                                          *      ELSMENUP
00424 ************************************************************      ELSMENUP
00425  PROCESS-INPUT.                                                   ELSMENUP
00426      PERFORM READ-THE-SELECTION.                                  ELSMENUP
00427      IF MSO-MIN-CHOICES = ZERO                                    ELSMENUP
00428          PERFORM PROCESS-NO-RESPONSE-MENU                         ELSMENUP
00429      ELSE IF EIBAID =  DFHENTER                                   ELSMENUP
00430          PERFORM CHECK-FOR-VALID-RESPONSE                         ELSMENUP
00431      ELSE                                                         ELSMENUP
00432          PERFORM CHECK-FOR-SCROLLING.                             ELSMENUP
00433                                                                   ELSMENUP
00434                                                                   ELSMENUP
00435 ************************************************************      ELSMENUP
00436 *                                                          *      ELSMENUP
00437 *        READ THE SELECTION                                *      ELSMENUP
00438 *                                                          *      ELSMENUP
00439 ************************************************************      ELSMENUP
00440  READ-THE-SELECTION.                                              ELSMENUP
00441      EXEC CICS RECEIVE                                            ELSMENUP
00442                MAP('EL02MAP')                                     ELSMENUP
00443                MAPSET('EL02SET')                                  ELSMENUP
00444                INTO(EL02MAPI)                                     ELSMENUP
00445                END-EXEC.                                          ELSMENUP
00446 /***********************************************************      ELSMENUP
00447 *                                                          *      ELSMENUP
00448 *        PROCESS NO RESPONSE MENU                          *      ELSMENUP
00449 *                                                          *      ELSMENUP
00450 ************************************************************      ELSMENUP
00451  PROCESS-NO-RESPONSE-MENU.                                        ELSMENUP
00452      ADD 1, SSB-MNU-CUR-BOT-ITM                                   ELSMENUP
00453           GIVING IOP-TSQ-ITEM-NBR.                                ELSMENUP
00454      IF IOP-TSQ-ITEM-NBR > MSO-NBR-MENU-OPTS                      ELSMENUP
00455          PERFORM MENU-PROCESSING-WRAP-UP                          ELSMENUP
00456      ELSE                                                         ELSMENUP
00457          SET WS-SCROLL-DOWN TO TRUE.                              ELSMENUP
00458                                                                   ELSMENUP
00459                                                                   ELSMENUP
00460 ************************************************************      ELSMENUP
00461 *                                                          *      ELSMENUP
00462 *        CHECK FOR VALID RESPONSE                          *      ELSMENUP
00463 *                                                          *      ELSMENUP
00464 ************************************************************      ELSMENUP
00465  CHECK-FOR-VALID-RESPONSE.                                        ELSMENUP
00466      IF SELECTNI = SPACES OR LOW-VALUES                           ELSMENUP
00467          SET WS-SCROLL-DOWN TO TRUE                               ELSMENUP
00468      ELSE                                                         ELSMENUP
00469          PERFORM PROCESS-SELECTION.                               ELSMENUP
00470                                                                   ELSMENUP
00471                                                                   ELSMENUP
00472 ************************************************************      ELSMENUP
00473 *                                                          *      ELSMENUP
00474 *        PROCESS SELECTION                                 *      ELSMENUP
00475 *                                                          *      ELSMENUP
00476 ************************************************************      ELSMENUP
00477  PROCESS-SELECTION.                                               ELSMENUP
00478      PERFORM PROCESS-SELECTION-INITIALIZATI.                      ELSMENUP
00479      PERFORM PROCESS-ALL-CHOICES                                  ELSMENUP
00480          UNTIL WS-PARSE-COMPLETED                                 ELSMENUP
00481                      OR WS-ERROR-MESSAGE-ISSUED.                  ELSMENUP
00482      IF WS-NO-ERROR-MESSAGE                                       ELSMENUP
00483          PERFORM MENU-PROCESSING-WRAP-UP.                         ELSMENUP
00484 /***********************************************************      ELSMENUP
00485 *                                                          *      ELSMENUP
00486 *        PROCESS SELECTION INITIALIZATION                  *      ELSMENUP
00487 *                                                          *      ELSMENUP
00488 ************************************************************      ELSMENUP
00489  PROCESS-SELECTION-INITIALIZATI.                                  ELSMENUP
00490      PERFORM COMPRESS-SELECTION.                                  ELSMENUP
00491      MOVE 1 TO WS-EXT-MOVE-POS.                                   ELSMENUP
00492      MOVE ZERO TO SSB-MNU-NUM-CHOICES.                            ELSMENUP
00493                                                                   ELSMENUP
00494                                                                   ELSMENUP
00495 ************************************************************      ELSMENUP
00496 *                                                          *      ELSMENUP
00497 *        COMPRESS SELECTION                                *      ELSMENUP
00498 *                                                          *      ELSMENUP
00499 ************************************************************      ELSMENUP
00500  COMPRESS-SELECTION.                                              ELSMENUP
00501      MOVE LENGTH OF SELECTNI TO TCAR-AREA-LENGTH.                 ELSMENUP
00502      MOVE SELECTNI TO TCAR-FROM-LINE (1)                          ELSMENUP
00503                       WS-HOLD-RESPONSE.                           ELSMENUP
00504      INSPECT TCAR-FROM-LINE (1) REPLACING ALL ',' BY              ELSMENUP
00505          SPACE.                                                   ELSMENUP
00506      PERFORM TEXT-COMPRESSION.                                    ELSMENUP
00507      SUBTRACT 1 FROM TCAR-L GIVING WS-SELECTN-LENGTH.             ELSMENUP
00508 /***********************************************************      ELSMENUP
00509 *                                                          *      ELSMENUP
00510 *        PROCESS ALL CHOICES                               *      ELSMENUP
00511 *                                                          *      ELSMENUP
00512 ************************************************************      ELSMENUP
00513  PROCESS-ALL-CHOICES.                                             ELSMENUP
00514      PERFORM EXTRACT-NEXT-WORD.                                   ELSMENUP
00515      IF WS-NO-ERROR-MESSAGE                                       ELSMENUP
00516          PERFORM VALIDATE-THE-CHOICE.                             ELSMENUP
00517      IF WS-EXT-MOVE-POS > WS-SELECTN-LENGTH                       ELSMENUP
00518          SET WS-PARSE-COMPLETED TO TRUE.                          ELSMENUP
00519                                                                   ELSMENUP
00520                                                                   ELSMENUP
00521 ************************************************************      ELSMENUP
00522 *                                                          *      ELSMENUP
00523 *        EXTRACT NEXT WORD                                 *      ELSMENUP
00524 *                                                          *      ELSMENUP
00525 ************************************************************      ELSMENUP
00526  EXTRACT-NEXT-WORD.                                               ELSMENUP
00527      PERFORM                                                      ELSMENUP
00528          VARYING WS-EXT-END-POS FROM WS-EXT-MOVE-POS BY 1         ELSMENUP
00529            UNTIL WS-EXT-END-POS > WS-SELECTN-LENGTH               ELSMENUP
00530               OR TCAR-TO-DIGIT (WS-EXT-END-POS) = SPACE           ELSMENUP
00531      END-PERFORM.                                                 ELSMENUP
00532                                                                   ELSMENUP
00533      IF SSB-MNU-NUM-CHOICES = MSO-MAX-CHOICES                     ELSMENUP
00534          PERFORM REJECT-EXCESSIVE-CHOICES                         ELSMENUP
00535      ELSE                                                         ELSMENUP
00536          PERFORM MOVE-THE-CHOICE.                                 ELSMENUP
00537      ADD 1, WS-EXT-END-POS GIVING WS-EXT-MOVE-POS.                ELSMENUP
00538 /***********************************************************      ELSMENUP
00539 *                                                          *      ELSMENUP
00540 *        REJECT EXCESSIVE CHOICES                          *      ELSMENUP
00541 *                                                          *      ELSMENUP
00542 ************************************************************      ELSMENUP
00543  REJECT-EXCESSIVE-CHOICES.                                        ELSMENUP
00544      MOVE SPACES TO ERRMSGO.                                      ELSMENUP
00545      MOVE MSO-MAX-CHOICES TO WS-EDIT-TWO.                         ELSMENUP
00546      IF MSO-MAX-CHOICES = 1                                       ELSMENUP
00547          MOVE WS-ONE-CHOICE-PERMITTED  TO ERRMSGO                 ELSMENUP
00548      ELSE                                                         ELSMENUP
00549          STRING 'TOO MANY CHOICES.  UP TO ' DELIMITED BY SIZE     ELSMENUP
00550                 WS-EDIT-TWO                 DELIMITED BY SIZE     ELSMENUP
00551                 ' CHOICE(S) ARE PERMITTED.' DELIMITED BY SIZE     ELSMENUP
00552            INTO ERRMSGO                                           ELSMENUP
00553         END-STRING                                                ELSMENUP
00554      END-IF.                                                      ELSMENUP
00555      SET WS-ERROR-MESSAGE-ISSUED TO TRUE.                         ELSMENUP
00556                                                                   ELSMENUP
00557                                                                   ELSMENUP
00558 ************************************************************      ELSMENUP
00559 *                                                          *      ELSMENUP
00560 *        MOVE THE CHOICE                                   *      ELSMENUP
00561 *                                                          *      ELSMENUP
00562 ************************************************************      ELSMENUP
00563  MOVE-THE-CHOICE.                                                 ELSMENUP
00564      ADD 1 TO SSB-MNU-NUM-CHOICES.                                ELSMENUP
00565      SET SSB-MNU-IDX TO SSB-MNU-NUM-CHOICES.                      ELSMENUP
00566      COMPUTE WS-CHOICE-LENGTH = WS-EXT-END-POS -                  ELSMENUP
00567          WS-EXT-MOVE-POS.                                         ELSMENUP
00568      MOVE LENGTH OF SSB-MNU-CHOICE                                ELSMENUP
00569           TO WS-RECEIVER-LENGTH.                                  ELSMENUP
00570      CALL 'ELUMVCL' USING TCAR-TO-DIGIT (WS-EXT-MOVE-POS)         ELSMENUP
00571                           WS-CHOICE-LENGTH                        ELSMENUP
00572                           SSB-MNU-CHOICE (SSB-MNU-IDX)            ELSMENUP
00573                           WS-RECEIVER-LENGTH                      ELSMENUP
00574                           WS-PAD-CHAR.                            ELSMENUP
00575      IF WS-CHOICE-LENGTH > WS-RECEIVER-LENGTH                     ELSMENUP
00576          PERFORM INDICATE-INVALID-CHOICE-LENGTH.                  ELSMENUP
00577 /***********************************************************      ELSMENUP
00578 *                                                          *      ELSMENUP
00579 *        INDICATE INVALID CHOICE LENGTH                    *      ELSMENUP
00580 *                                                          *      ELSMENUP
00581 ************************************************************      ELSMENUP
00582  INDICATE-INVALID-CHOICE-LENGTH.                                  ELSMENUP
00583      MOVE SPACES TO ERRMSGO.                                      ELSMENUP
00584      STRING 'THE CHOICE BEGINING WITH '  DELIMITED BY SIZE        ELSMENUP
00585             WS-QUOTE                     DELIMITED BY SIZE        ELSMENUP
00586             SSB-MNU-CHOICE (SSB-MNU-IDX) DELIMITED BY SIZE        ELSMENUP
00587             WS-QUOTE                     DELIMITED BY SIZE        ELSMENUP
00588             ' IS TOO LONG.'              DELIMITED BY SIZE        ELSMENUP
00589         INTO ERRMSGO                                              ELSMENUP
00590      END-STRING.                                                  ELSMENUP
00591      SET WS-ERROR-MESSAGE-ISSUED TO TRUE.                         ELSMENUP
00592                                                                   ELSMENUP
00593                                                                   ELSMENUP
00594 ************************************************************      ELSMENUP
00595 *                                                          *      ELSMENUP
00596 *        VALIDATE THE CHOICE                               *      ELSMENUP
00597 *                                                          *      ELSMENUP
00598 ************************************************************      ELSMENUP
00599  VALIDATE-THE-CHOICE.                                             ELSMENUP
00600      IF MSO-OPT-TYP-NUM                                           ELSMENUP
00601          PERFORM NUMERIC-SELECTION                                ELSMENUP
00602      ELSE IF MSO-OPT-TYP-AN                                       ELSMENUP
00603          PERFORM ALPHANUMERIC-SELECTION                           ELSMENUP
00604      ELSE                                                         ELSMENUP
00605          PERFORM PARAMETER-ERROR-ABEND.                           ELSMENUP
00606                                                                   ELSMENUP
00607                                                                   ELSMENUP
00608 ************************************************************      ELSMENUP
00609 *                                                          *      ELSMENUP
00610 *        NUMERIC SELECTION                                 *      ELSMENUP
00611 *                                                          *      ELSMENUP
00612 ************************************************************      ELSMENUP
00613  NUMERIC-SELECTION.                                               ELSMENUP
00614 * IF THE SELECTION IS TOO LONG, IT CANNOT BE A                    ELSMENUP
00615 * VALID NUMERIC SELECTION                                         ELSMENUP
00616      IF WS-CHOICE-LENGTH > MSO-OPT-LEN                            ELSMENUP
00617          PERFORM ALPHANUMERIC-SELECTION                           ELSMENUP
00618      ELSE                                                         ELSMENUP
00619          PERFORM PROCESS-NUMERIC-SELECTION.                       ELSMENUP
00620 /***********************************************************      ELSMENUP
00621 *                                                          *      ELSMENUP
00622 *        PROCESS NUMERIC SELECTION                         *      ELSMENUP
00623 *                                                          *      ELSMENUP
00624 ************************************************************      ELSMENUP
00625  PROCESS-NUMERIC-SELECTION.                                       ELSMENUP
00626      PERFORM RIGHT-JUSTIFY-NUMBER.                                ELSMENUP
00627      PERFORM NUMERIC-VALIDATION.                                  ELSMENUP
00628                                                                   ELSMENUP
00629                                                                   ELSMENUP
00630 ************************************************************      ELSMENUP
00631 *                                                          *      ELSMENUP
00632 *        RIGHT JUSTIFY NUMBER                              *      ELSMENUP
00633 *                                                          *      ELSMENUP
00634 ************************************************************      ELSMENUP
00635  RIGHT-JUSTIFY-NUMBER.                                            ELSMENUP
00636      MOVE ZEROS  TO  WS-OPT-SEL.                                  ELSMENUP
00637      COMPUTE WS-MOVE-POS = LENGTH OF WS-OPT-SEL -                 ELSMENUP
00638                            WS-CHOICE-LENGTH + 1.                  ELSMENUP
00639      STRING SSB-MNU-CHOICE (SSB-MNU-IDX)  DELIMITED BY SIZE       ELSMENUP
00640          INTO WS-OPT-SEL POINTER WS-MOVE-POS.                     ELSMENUP
00641                                                                   ELSMENUP
00642                                                                   ELSMENUP
00643 ************************************************************      ELSMENUP
00644 *                                                          *      ELSMENUP
00645 *        NUMERIC VALIDATION                                *      ELSMENUP
00646 *                                                          *      ELSMENUP
00647 ************************************************************      ELSMENUP
00648  NUMERIC-VALIDATION.                                              ELSMENUP
00649      IF WS-OPT-SEL NOT NUMERIC                                    ELSMENUP
00650          PERFORM INVALID-NUMERIC-CHARACTERS                       ELSMENUP
00651      ELSE                                                         ELSMENUP
00652          PERFORM VALIDATE-NUMERIC-SELECTION.                      ELSMENUP
00653                                                                   ELSMENUP
00654                                                                   ELSMENUP
00655 ************************************************************      ELSMENUP
00656 *                                                          *      ELSMENUP
00657 *        VALIDATE NUMERIC SELECTION                        *      ELSMENUP
00658 *                                                          *      ELSMENUP
00659 ************************************************************      ELSMENUP
00660  VALIDATE-NUMERIC-SELECTION.                                      ELSMENUP
00661      PERFORM BUILD-SEARCH-KEY.                                    ELSMENUP
00662      PERFORM SEARCH-FOR-SELECTION.                                ELSMENUP
00663      IF WS-SELECTION-FOUND                                        ELSMENUP
00664          PERFORM ACCEPT-THE-SELECTION                             ELSMENUP
00665      ELSE                                                         ELSMENUP
00666          PERFORM INVALID-NUMERIC-CHARACTERS.                      ELSMENUP
00667 /***********************************************************      ELSMENUP
00668 *                                                          *      ELSMENUP
00669 *        INVALID NUMERIC CHARACTERS                        *      ELSMENUP
00670 *                                                          *      ELSMENUP
00671 ************************************************************      ELSMENUP
00672  INVALID-NUMERIC-CHARACTERS.                                      ELSMENUP
00673      PERFORM KEYWORD-SEARCH.                                      ELSMENUP
00674      IF WS-SELECTION-FOUND                                        ELSMENUP
00675          PERFORM ACCEPT-THE-SELECTION                             ELSMENUP
00676      ELSE                                                         ELSMENUP
00677          PERFORM SELECTION-NOT-FOUND.                             ELSMENUP
00678                                                                   ELSMENUP
00679                                                                   ELSMENUP
00680 ************************************************************      ELSMENUP
00681 *                                                          *      ELSMENUP
00682 *        BUILD SEARCH KEY                                  *      ELSMENUP
00683 *                                                          *      ELSMENUP
00684 ************************************************************      ELSMENUP
00685  BUILD-SEARCH-KEY.                                                ELSMENUP
00686      EVALUATE MSO-OPT-LEN                                         ELSMENUP
00687      WHEN 1                                                       ELSMENUP
00688          MOVE WS-OPT-NUM-1  TO  WS-SEARCH-KEY                     ELSMENUP
00689      WHEN 2                                                       ELSMENUP
00690          MOVE WS-OPT-NUM-2  TO  WS-SEARCH-KEY                     ELSMENUP
00691      WHEN 3                                                       ELSMENUP
00692          MOVE WS-OPT-NUM-3  TO  WS-SEARCH-KEY                     ELSMENUP
00693      WHEN 4                                                       ELSMENUP
00694          MOVE WS-OPT-NUM-4  TO  WS-SEARCH-KEY                     ELSMENUP
00695      WHEN 5                                                       ELSMENUP
00696          MOVE WS-OPT-NUM-5  TO  WS-SEARCH-KEY                     ELSMENUP
00697      WHEN OTHER                                                   ELSMENUP
00698          PERFORM PARAMETER-ERROR-ABEND                            ELSMENUP
00699      END-EVALUATE.                                                ELSMENUP
00700 /***********************************************************      ELSMENUP
00701 *                                                          *      ELSMENUP
00702 *        ALPHANUMERIC SELECTION                            *      ELSMENUP
00703 *                                                          *      ELSMENUP
00704 ************************************************************      ELSMENUP
00705  ALPHANUMERIC-SELECTION.                                          ELSMENUP
00706      MOVE SSB-MNU-CHOICE (SSB-MNU-IDX) TO                         ELSMENUP
00707          WS-SEARCH-KEY.                                           ELSMENUP
00708      IF (SSB-MNU-TITLE = 'SELECT SECTION FOR GROUP')              ELSMENUP
00709          AND (TEXAS-REGION) AND  (WS-SELECTN-LENGTH               ELSMENUP
00710                NOT = 8) AND NOT WS-CHANGE-TX-PKG-CODE-GROUP       ELSMENUP
00711         CONTINUE                                                  ELSMENUP
00712      ELSE                                                         ELSMENUP
00713          PERFORM SEARCH-FOR-SELECTION                             ELSMENUP
00714          IF WS-SELECTION-NOT-FOUND                                ELSMENUP
00715             PERFORM KEYWORD-SEARCH                                ELSMENUP
00716          END-IF                                                   ELSMENUP
00717      END-IF.                                                      ELSMENUP
00718      IF WS-SELECTION-FOUND                                        ELSMENUP
00719          PERFORM ACCEPT-THE-SELECTION                             ELSMENUP
00720      ELSE                                                         ELSMENUP
00721          PERFORM SELECTION-NOT-FOUND.                             ELSMENUP
00722                                                                   ELSMENUP
00723                                                                   ELSMENUP
00724 ************************************************************      ELSMENUP
00725 *                                                          *      ELSMENUP
00726 *        SEARCH FOR SELECTION                              *      ELSMENUP
00727 *                                                          *      ELSMENUP
00728 ************************************************************      ELSMENUP
00729  SEARCH-FOR-SELECTION.                                            ELSMENUP
00730      SET MSO-IDX TO 1.                                            ELSMENUP
00731      SEARCH MSO-MENU-OPT VARYING MSO-IDX                          ELSMENUP
00732           AT END                                                  ELSMENUP
00733               SET WS-SELECTION-NOT-FOUND TO TRUE                  ELSMENUP
00734           WHEN WS-SEARCH-KEY = MSO-OPT-SEL (MSO-IDX)              ELSMENUP
00735               SET WS-SELECTION-FOUND TO TRUE.                     ELSMENUP
00736 /***********************************************************      ELSMENUP
00737 *                                                          *      ELSMENUP
00738 *        KEYWORD SEARCH                                    *      ELSMENUP
00739 *                                                          *      ELSMENUP
00740 ************************************************************      ELSMENUP
00741  KEYWORD-SEARCH.                                                  ELSMENUP
00742      MOVE SSB-MNU-CHOICE (SSB-MNU-IDX) TO  WS-KEYWORD.            ELSMENUP
00743      SET MSO-IDX TO 1.                                            ELSMENUP
00744      SEARCH MSO-MENU-OPT VARYING MSO-IDX                          ELSMENUP
00745           AT END                                                  ELSMENUP
00746               SET WS-SELECTION-NOT-FOUND TO TRUE                  ELSMENUP
00747           WHEN WS-KEYWORD = MSO-OPT-KWD (MSO-IDX)                 ELSMENUP
00748               SET WS-SELECTION-FOUND TO TRUE.                     ELSMENUP
00749                                                                   ELSMENUP
00750                                                                   ELSMENUP
00751 ************************************************************      ELSMENUP
00752 *                                                          *      ELSMENUP
00753 *        ACCEPT THE SELECTION                              *      ELSMENUP
00754 *                                                          *      ELSMENUP
00755 ************************************************************      ELSMENUP
00756  ACCEPT-THE-SELECTION.                                            ELSMENUP
00757      MOVE MSO-OPT-KWD (MSO-IDX) TO SSB-MNU-CHOICE                 ELSMENUP
00758          (SSB-MNU-IDX).                                           ELSMENUP
00759                                                                   ELSMENUP
00760                                                                   ELSMENUP
00761 ************************************************************      ELSMENUP
00762 *                                                          *      ELSMENUP
00763 *        SELECTION NOT FOUND                               *      ELSMENUP
00764 *                                                          *      ELSMENUP
00765 ************************************************************      ELSMENUP
00766  SELECTION-NOT-FOUND.                                             ELSMENUP
00767      MOVE SPACES  TO ERRMSGO.                                     ELSMENUP
00768      STRING 'THE SELECTION '             DELIMITED BY SIZE        ELSMENUP
00769             WS-QUOTE                     DELIMITED BY SIZE        ELSMENUP
00770             SSB-MNU-CHOICE (SSB-MNU-IDX) DELIMITED BY SPACE       ELSMENUP
00771             WS-QUOTE                     DELIMITED BY SIZE        ELSMENUP
00772             ' IS NOT AVAILABLE.'         DELIMITED BY SIZE        ELSMENUP
00773          INTO ERRMSGO                                             ELSMENUP
00774      END-STRING.                                                  ELSMENUP
00775      SET WS-ERROR-MESSAGE-ISSUED TO TRUE.                         ELSMENUP
00776 /***********************************************************      ELSMENUP
00777 *                                                          *      ELSMENUP
00778 *        MENU PROCESSING WRAP UP                           *      ELSMENUP
00779 *                                                          *      ELSMENUP
00780 ************************************************************      ELSMENUP
00781  MENU-PROCESSING-WRAP-UP.                                         ELSMENUP
00782      SET SSB-MENU-COMPLETE (SSB-SELECTOR-STATE)                   ELSMENUP
00783          TO TRUE.                                                 ELSMENUP
00784      IF SSB-SS-GET-TOPIC                                          ELSMENUP
00785          PERFORM EMPTY-THE-STACK.                                 ELSMENUP
00786      IF SSB-SS-GET-TOPIC                                          ELSMENUP
00787               OR SSB-SS-GET-SUBTOPIC                              ELSMENUP
00788               OR SSB-PUSH-MENU                                    ELSMENUP
00789          PERFORM PUSH-THE-MENU-TO-THE-STACK.                      ELSMENUP
00790                                                                   ELSMENUP
00791                                                                   ELSMENUP
00792 ************************************************************      ELSMENUP
00793 *                                                          *      ELSMENUP
00794 *        EMPTY THE STACK                                   *      ELSMENUP
00795 *                                                          *      ELSMENUP
00796 ************************************************************      ELSMENUP
00797  EMPTY-THE-STACK.                                                 ELSMENUP
00798      SET SSB-STACK-EMPTY TO TRUE.                                 ELSMENUP
00799                                                                   ELSMENUP
00800                                                                   ELSMENUP
00801 ************************************************************      ELSMENUP
00802 *                                                          *      ELSMENUP
00803 *        PUSH THE MENU TO THE STACK                        *      ELSMENUP
00804 *                                                          *      ELSMENUP
00805 ************************************************************      ELSMENUP
00806  PUSH-THE-MENU-TO-THE-STACK.                                      ELSMENUP
00807      IF SSB-STACK-EMPTY                                           ELSMENUP
00808          PERFORM APPEND-THE-MENU-TO-THE-STACK                     ELSMENUP
00809      ELSE                                                         ELSMENUP
00810          PERFORM CHECK-FOR-DUPLICATE-MENUS.                       ELSMENUP
00811 /***********************************************************      ELSMENUP
00812 *                                                          *      ELSMENUP
00813 *        CHECK FOR DUPLICATE MENUS                         *      ELSMENUP
00814 *                                                          *      ELSMENUP
00815 ************************************************************      ELSMENUP
00816  CHECK-FOR-DUPLICATE-MENUS.                                       ELSMENUP
00817      SET WS-DUP-NOT-FOUND TO TRUE.                                ELSMENUP
00818      MOVE 1 TO WS-DUP-SUB.                                        ELSMENUP
00819      PERFORM WITH TEST BEFORE                                     ELSMENUP
00820          UNTIL WS-DUP-SUB > SSB-STACK-CURRENT-ITEM                ELSMENUP
00821             OR WS-DUP-FOUND                                       ELSMENUP
00822             IF SSB-SELECTOR-STATE =                               ELSMENUP
00823                SSB-STACK-STATE (WS-DUP-SUB)                       ELSMENUP
00824                   SET WS-DUP-FOUND TO TRUE                        ELSMENUP
00825             ELSE                                                  ELSMENUP
00826                ADD 1 TO WS-DUP-SUB                                ELSMENUP
00827             END-IF                                                ELSMENUP
00828      END-PERFORM.                                                 ELSMENUP
00829      IF WS-DUP-NOT-FOUND                                          ELSMENUP
00830          PERFORM APPEND-THE-MENU-TO-THE-STACK.                    ELSMENUP
00831                                                                   ELSMENUP
00832                                                                   ELSMENUP
00833 ************************************************************      ELSMENUP
00834 *                                                          *      ELSMENUP
00835 *        APPEND THE MENU TO THE STACK                      *      ELSMENUP
00836 *                                                          *      ELSMENUP
00837 ************************************************************      ELSMENUP
00838  APPEND-THE-MENU-TO-THE-STACK.                                    ELSMENUP
00839      ADD 1 TO SSB-STACK-CURRENT-ITEM.                             ELSMENUP
00840      IF NOT SSB-VALID-STACK-ITEM                                  ELSMENUP
00841          PERFORM PROGRAM-LOGIC-ERROR.                             ELSMENUP
00842      MOVE SSB-SELECTOR-STATE TO                                   ELSMENUP
00843          SSB-STACK-STATE (SSB-STACK-CURRENT-ITEM).                ELSMENUP
00844 /***********************************************************      ELSMENUP
00845 *                                                          *      ELSMENUP
00846 *        CHECK FOR SCROLLING                               *      ELSMENUP
00847 *                                                          *      ELSMENUP
00848 ************************************************************      ELSMENUP
00849  CHECK-FOR-SCROLLING.                                             ELSMENUP
00850      EVALUATE EIBAID                                              ELSMENUP
00851      WHEN DFHPF8                                                  ELSMENUP
00852          SET WS-SCROLL-DOWN TO TRUE                               ELSMENUP
00853      WHEN DFHPF20                                                 ELSMENUP
00854          SET WS-SCROLL-DOWN TO TRUE                               ELSMENUP
00855      WHEN DFHPF7                                                  ELSMENUP
00856          SET WS-SCROLL-UP TO TRUE                                 ELSMENUP
00857      WHEN DFHPF19                                                 ELSMENUP
00858          SET WS-SCROLL-UP TO TRUE                                 ELSMENUP
00859      WHEN DFHPF10                                                 ELSMENUP
00860          SET WS-SCROLL-TOP TO TRUE                                ELSMENUP
00861      WHEN DFHPF22                                                 ELSMENUP
00862          SET WS-SCROLL-TOP TO TRUE                                ELSMENUP
00863      WHEN DFHPF11                                                 ELSMENUP
00864          SET WS-SCROLL-BOTTOM TO TRUE                             ELSMENUP
00865      WHEN DFHPF23                                                 ELSMENUP
00866          SET WS-SCROLL-BOTTOM TO TRUE                             ELSMENUP
00867      WHEN OTHER                                                   ELSMENUP
00868          PERFORM INVALID-SCROLL-REQUEST.                          ELSMENUP
00869 /***********************************************************      ELSMENUP
00870 *                                                          *      ELSMENUP
00871 *        FORMAT MENU                                       *      ELSMENUP
00872 *                                                          *      ELSMENUP
00873 ************************************************************      ELSMENUP
00874  FORMAT-MENU.                                                     ELSMENUP
00875      PERFORM INITIALIZE-SYMBOLIC-MAP.                             ELSMENUP
00876      PERFORM BUILD-FIXED-HEADING.                                 ELSMENUP
00877      PERFORM BUILD-CALLER-HEADING.                                ELSMENUP
00878      IF MHD-NBR-HDG-LINES =                                       ELSMENUP
00879                  WS-ELSMHDG-MVO                                   ELSMENUP
00880          PERFORM HEADING-ONLY-MENU                                ELSMENUP
00881      ELSE IF SSB-START-MENU (SSB-SELECTOR-STATE)                  ELSMENUP
00882          PERFORM BUILD-FIRST-OPTIONS-PAGE                         ELSMENUP
00883      ELSE                                                         ELSMENUP
00884          PERFORM BUILD-OPTIONS-PAGE.                              ELSMENUP
00885                                                                   ELSMENUP
00886                                                                   ELSMENUP
00887 ************************************************************      ELSMENUP
00888 *                                                          *      ELSMENUP
00889 *        INITIALIZE SYMBOLIC MAP                           *      ELSMENUP
00890 *                                                          *      ELSMENUP
00891 ************************************************************      ELSMENUP
00892  INITIALIZE-SYMBOLIC-MAP.                                         ELSMENUP
00893      MOVE SPACES   TO  EL02MAPO.                                  ELSMENUP
00894      MOVE DFHBMASB TO  MAPID2A                                    ELSMENUP
00895                        DATE2A                                     ELSMENUP
00896                        MENUID2A                                   ELSMENUP
00897                        TIME2A                                     ELSMENUP
00898                        HEADA (1)                                  ELSMENUP
00899                        HEADA (2)                                  ELSMENUP
00900                        HEADA (3)                                  ELSMENUP
00901                        ERRMSGA.                                   ELSMENUP
00902 /***********************************************************      ELSMENUP
00903 *                                                          *      ELSMENUP
00904 *        BUILD FIXED HEADING                               *      ELSMENUP
00905 *                                                          *      ELSMENUP
00906 ************************************************************      ELSMENUP
00907  BUILD-FIXED-HEADING.                                             ELSMENUP
00908      PERFORM BUILD-SCREEN-HEADINGS.                               ELSMENUP
00909      PERFORM CENTER-SCREEN-TITLE.                                 ELSMENUP
00910      PERFORM BUILD-VARIABLE-HEADING.                              ELSMENUP
00911                                                                   ELSMENUP
00912                                                                   ELSMENUP
00913 ************************************************************      ELSMENUP
00914 *                                                          *      ELSMENUP
00915 *        BUILD SCREEN HEADINGS                             *      ELSMENUP
00916 *                                                          *      ELSMENUP
00917 ************************************************************      ELSMENUP
00918  BUILD-SCREEN-HEADINGS.                                           ELSMENUP
00919      MOVE 'EL02MAP'     TO  MAPID2O.                              ELSMENUP
00920      PERFORM MOVE-CURRENT-DATE-TO-SCREEN.                         ELSMENUP
00921      PERFORM MOVE-CURRENT-TIME-TO-SCREEN.                         ELSMENUP
00922                                                                   ELSMENUP
00923                                                                   ELSMENUP
00924 ************************************************************      ELSMENUP
00925 *                                                          *      ELSMENUP
00926 *        MOVE CURRENT DATE TO SCREEN                       *      ELSMENUP
00927 *                                                          *      ELSMENUP
00928 ************************************************************      ELSMENUP
00929  MOVE-CURRENT-DATE-TO-SCREEN.                                     ELSMENUP
00930      MOVE EIBDATE       TO  HGADATE-JULIAN1.                      ELSMENUP
00931      PERFORM JULIAN-TO-GREG-CONVERSION.                           ELSMENUP
00932      MOVE HGADATE-DATE2 TO  WS-DATE-EDIT.                         ELSMENUP
00933      MOVE WS-DATE-AREA  TO  DATE2O.                               ELSMENUP
00934                                                                   ELSMENUP
00935                                                                   ELSMENUP
00936 ************************************************************      ELSMENUP
00937 *                                                          *      ELSMENUP
00938 *        MOVE CURRENT TIME TO SCREEN                       *      ELSMENUP
00939 *                                                          *      ELSMENUP
00940 ************************************************************      ELSMENUP
00941  MOVE-CURRENT-TIME-TO-SCREEN.                                     ELSMENUP
00942      MOVE EIBTIME       TO  WS-TIME-EDIT.                         ELSMENUP
00943      MOVE WS-COLON      TO  WS-COLON-1                            ELSMENUP
00944                             WS-COLON-2.                           ELSMENUP
00945      MOVE WS-TIME-AREA  TO  TIME2O.                               ELSMENUP
00946 /***********************************************************      ELSMENUP
00947 *                                                          *      ELSMENUP
00948 *        CENTER SCREEN TITLE                               *      ELSMENUP
00949 *                                                          *      ELSMENUP
00950 ************************************************************      ELSMENUP
00951  CENTER-SCREEN-TITLE.                                             ELSMENUP
00952      PERFORM DETERMINE-TITLE-LENGTH.                              ELSMENUP
00953      IF WS-CENTER-START-POS > 1                                   ELSMENUP
00954              AND WS-CENTER-LENGTH < LENGTH OF MENUID2O            ELSMENUP
00955          PERFORM LEFT-JUSTIFY-TITLE.                              ELSMENUP
00956      PERFORM MOVE-TITLE-TO-SCREEN.                                ELSMENUP
00957                                                                   ELSMENUP
00958                                                                   ELSMENUP
00959 ************************************************************      ELSMENUP
00960 *                                                          *      ELSMENUP
00961 *        DETERMINE TITLE LENGTH                            *      ELSMENUP
00962 *                                                          *      ELSMENUP
00963 ************************************************************      ELSMENUP
00964  DETERMINE-TITLE-LENGTH.                                          ELSMENUP
00965      MOVE SSB-MNU-TITLE          TO WS-CENTER-AREA.               ELSMENUP
00966      PERFORM                                                      ELSMENUP
00967          VARYING WS-FIRST-NON-BLANK-IDX FROM 1 BY 1               ELSMENUP
00968            UNTIL WS-FIRST-NON-BLANK-IDX > LENGTH OF               ELSMENUP
00969                                           WS-CENTER-BYTES         ELSMENUP
00970               OR WS-CENTER-CHAR                                   ELSMENUP
00971                  (WS-FIRST-NON-BLANK-IDX) NOT = SPACE             ELSMENUP
00972      END-PERFORM.                                                 ELSMENUP
00973      PERFORM                                                      ELSMENUP
00974          VARYING WS-LAST-NON-BLANK-IDX                            ELSMENUP
00975             FROM LENGTH OF WS-CENTER-BYTES BY -1                  ELSMENUP
00976            UNTIL WS-LAST-NON-BLANK-IDX NOT >                      ELSMENUP
00977                  WS-FIRST-NON-BLANK-IDX                           ELSMENUP
00978               OR WS-CENTER-CHAR (WS-LAST-NON-BLANK-IDX)           ELSMENUP
00979                      NOT = SPACE                                  ELSMENUP
00980      END-PERFORM.                                                 ELSMENUP
00981      SET WS-CENTER-START-POS  TO  WS-FIRST-NON-BLANK-IDX.         ELSMENUP
00982      SET WS-CENTER-END-POS    TO  WS-LAST-NON-BLANK-IDX.          ELSMENUP
00983      COMPUTE WS-CENTER-LENGTH = WS-CENTER-END-POS -               ELSMENUP
00984                                 WS-CENTER-START-POS + 1.          ELSMENUP
00985 /***********************************************************      ELSMENUP
00986 *                                                          *      ELSMENUP
00987 *        LEFT JUSTIFY TITLE                                *      ELSMENUP
00988 *                                                          *      ELSMENUP
00989 ************************************************************      ELSMENUP
00990  LEFT-JUSTIFY-TITLE.                                              ELSMENUP
00991      MOVE WS-CENTER-LENGTH TO WS-SENDER-LENGTH.                   ELSMENUP
00992      MOVE LENGTH OF WS-CENTER-BYTES                               ELSMENUP
00993          TO WS-RECEIVER-LENGTH.                                   ELSMENUP
00994      CALL 'ELUMVCL'  USING WS-CENTER-CHAR (WS-CENTER-START-POS)   ELSMENUP
00995                            WS-SENDER-LENGTH                       ELSMENUP
00996                            WS-CENTER-CHAR (1)                     ELSMENUP
00997                            WS-RECEIVER-LENGTH                     ELSMENUP
00998                            WS-CENTER-FILL.                        ELSMENUP
00999                                                                   ELSMENUP
01000                                                                   ELSMENUP
01001 ************************************************************      ELSMENUP
01002 *                                                          *      ELSMENUP
01003 *        MOVE TITLE TO SCREEN                              *      ELSMENUP
01004 *                                                          *      ELSMENUP
01005 ************************************************************      ELSMENUP
01006  MOVE-TITLE-TO-SCREEN.                                            ELSMENUP
01007      COMPUTE WS-CENTER-MOVE-POS = (LENGTH OF MENUID2O / 2)        ELSMENUP
01008                                 - (WS-CENTER-LENGTH  / 2)         ELSMENUP
01009                                 + 1.                              ELSMENUP
01010      MOVE SPACES TO MENUID2O.                                     ELSMENUP
01011      STRING  WS-CENTER-BYTES   DELIMITED BY SIZE                  ELSMENUP
01012          INTO MENUID2O POINTER WS-CENTER-MOVE-POS.                ELSMENUP
01013 /***********************************************************      ELSMENUP
01014 *                                                          *      ELSMENUP
01015 *        BUILD VARIABLE HEADING                            *      ELSMENUP
01016 *                                                          *      ELSMENUP
01017 ************************************************************      ELSMENUP
01018  BUILD-VARIABLE-HEADING.                                          ELSMENUP
01019      PERFORM INITIALIZE-BUILD-VARIABLE-HEAD.                      ELSMENUP
01020      PERFORM CONSTRUCT-COMMON-HEADER.                             ELSMENUP
01021      PERFORM TEXT-FORMAT-HEADING.                                 ELSMENUP
01022      PERFORM MOVE-HEADINGS-TO-SCREEN.                             ELSMENUP
01023                                                                   ELSMENUP
01024                                                                   ELSMENUP
01025 ************************************************************      ELSMENUP
01026 *                                                          *      ELSMENUP
01027 *        INITIALIZE BUILD VARIABLE HEADING                 *      ELSMENUP
01028 *                                                          *      ELSMENUP
01029 ************************************************************      ELSMENUP
01030  INITIALIZE-BUILD-VARIABLE-HEAD.                                  ELSMENUP
01031      MOVE SPACES TO TCAR-FROM-AREA.                               ELSMENUP
01032      MOVE 1 TO TCAR-AREA-LENGTH.                                  ELSMENUP
01033                                                                   ELSMENUP
01034                                                                   ELSMENUP
01035 ************************************************************      ELSMENUP
01036 *                                                          *      ELSMENUP
01037 *        CONSTRUCT COMMON HEADER                           *      ELSMENUP
01038 *                                                          *      ELSMENUP
01039 ************************************************************      ELSMENUP
01040  CONSTRUCT-COMMON-HEADER.                                         ELSMENUP
01041      MOVE SSB-SELECTOR-STATE TO WS-SELECTOR-STATE.                ELSMENUP
01042      IF NOT SSB-NO-GRP-NO                                         ELSMENUP
01043          PERFORM CONSTRUCT-GROUP-NUMBER.                          ELSMENUP
01044      IF NOT SSB-NO-SECTN-NO                                       ELSMENUP
01045          PERFORM CONSTRUCT-SECTION-NO.                            ELSMENUP
01046      IF NOT SSB-NO-SUBSCRIBER-NBR                                 ELSMENUP
01047          PERFORM CONSTRUCT-SUBSCRIBER-NO.                         ELSMENUP
01048      SET SSB-SS-GET-PT-AGE   TO TRUE.                             ELSMENUP
01049      PERFORM CONSTRUCT-FAMILY-RELATION-HEAD.                      ELSMENUP
01050      IF NOT SSB-NO-GRP-NO                                         ELSMENUP
01051          PERFORM CONSTRUCT-SERVICE-FROM-DATE.                     ELSMENUP
01052      IF NOT SSB-NO-GRP-NO                                         ELSMENUP
01053          PERFORM CONSTRUCT-SERVICE-TO-DATE.                       ELSMENUP
01054      MOVE WS-SELECTOR-STATE TO SSB-SELECTOR-STATE.                ELSMENUP
01055 /***********************************************************      ELSMENUP
01056 *                                                          *      ELSMENUP
01057 *        CONSTRUCT GROUP NUMBER                            *      ELSMENUP
01058 *                                                          *      ELSMENUP
01059 ************************************************************      ELSMENUP
01060  CONSTRUCT-GROUP-NUMBER.                                          ELSMENUP
01061      MOVE SSB-GRP-NO   TO WS-HDR1-GROUP-NO.                       ELSMENUP
01062      STRING WS-ITEM-1   DELIMITED BY SIZE                         ELSMENUP
01063          INTO TCAR-FROM-AREA POINTER TCAR-AREA-LENGTH.            ELSMENUP
01064      ADD 1 TO TCAR-AREA-LENGTH.                                   ELSMENUP
01065                                                                   ELSMENUP
01066                                                                   ELSMENUP
01067 ************************************************************      ELSMENUP
01068 *                                                          *      ELSMENUP
01069 *        CONSTRUCT SECTION NO                              *      ELSMENUP
01070 *                                                          *      ELSMENUP
01071 ************************************************************      ELSMENUP
01072  CONSTRUCT-SECTION-NO.                                            ELSMENUP
01073      MOVE SSB-SECTN-NO TO WS-HDR1-SECT-NO.                        ELSMENUP
01074      STRING WS-ITEM-2   DELIMITED BY SIZE                         ELSMENUP
01075          INTO TCAR-FROM-AREA POINTER TCAR-AREA-LENGTH.            ELSMENUP
01076      ADD 1 TO TCAR-AREA-LENGTH.                                   ELSMENUP
01077                                                                   ELSMENUP
01078                                                                   ELSMENUP
01079 ************************************************************      ELSMENUP
01080 *                                                          *      ELSMENUP
01081 *        CONSTRUCT SUBSCRIBER NO                           *      ELSMENUP
01082 *                                                          *      ELSMENUP
01083 ************************************************************      ELSMENUP
01084  CONSTRUCT-SUBSCRIBER-NO.                                         ELSMENUP
01085      MOVE SSB-SUBSCRIBER-NBR TO WS-SUBSCR-NO.                     ELSMENUP
01086      STRING WS-ITEM-6   DELIMITED BY SIZE                         ELSMENUP
01087          INTO TCAR-FROM-AREA POINTER TCAR-AREA-LENGTH.            ELSMENUP
01088      ADD 1 TO TCAR-AREA-LENGTH.                                   ELSMENUP
01089                                                                   ELSMENUP
01090                                                                   ELSMENUP
01091 ************************************************************      ELSMENUP
01092 *                                                          *      ELSMENUP
01093 *        CONSTRUCT FAMILY RELATION HEADER                  *      ELSMENUP
01094 *                                                          *      ELSMENUP
01095 ************************************************************      ELSMENUP
01096  CONSTRUCT-FAMILY-RELATION-HEAD.                                  ELSMENUP
01097      PERFORM FAMILY-RELATION-AGE-HEADER.                          ELSMENUP
01098      STRING WS-ITEM-3   DELIMITED BY SIZE                         ELSMENUP
01099          INTO TCAR-FROM-AREA POINTER TCAR-AREA-LENGTH.            ELSMENUP
01100      ADD 1 TO TCAR-AREA-LENGTH.                                   ELSMENUP
01101 /***********************************************************      ELSMENUP
01102 *                                                          *      ELSMENUP
01103 *        CONSTRUCT SERVICE FROM DATE                       *      ELSMENUP
01104 *                                                          *      ELSMENUP
01105 ************************************************************      ELSMENUP
01106  CONSTRUCT-SERVICE-FROM-DATE.                                     ELSMENUP
01107      PERFORM CONVERT-SERVICE-FROM-DATE.                           ELSMENUP
01108      STRING WS-ITEM-4   DELIMITED BY SIZE                         ELSMENUP
01109          INTO TCAR-FROM-AREA POINTER TCAR-AREA-LENGTH.            ELSMENUP
01110      ADD 1 TO TCAR-AREA-LENGTH.                                   ELSMENUP
01111                                                                   ELSMENUP
01112                                                                   ELSMENUP
01113 ************************************************************      ELSMENUP
01114 *                                                          *      ELSMENUP
01115 *        CONSTRUCT SERVICE TO DATE                         *      ELSMENUP
01116 *                                                          *      ELSMENUP
01117 ************************************************************      ELSMENUP
01118  CONSTRUCT-SERVICE-TO-DATE.                                       ELSMENUP
01119      PERFORM CONVERT-SERVICE-TO-DATE.                             ELSMENUP
01120      STRING WS-ITEM-5   DELIMITED BY SIZE                         ELSMENUP
01121          INTO TCAR-FROM-AREA POINTER TCAR-AREA-LENGTH.            ELSMENUP
01122      ADD 1 TO TCAR-AREA-LENGTH.                                   ELSMENUP
01123 /***********************************************************      ELSMENUP
01124 *                                                          *      ELSMENUP
01125 *        FAMILY RELATION AGE HEADER                        *      ELSMENUP
01126 *                                                          *      ELSMENUP
01127 ************************************************************      ELSMENUP
01128  FAMILY-RELATION-AGE-HEADER.                                      ELSMENUP
01129      IF SSB-MEDCA-UNDEF AND                                       ELSMENUP
01130                 SSB-FR-UNDEF    AND                               ELSMENUP
01131                 SSB-PT-AGE-UNDEF                                  ELSMENUP
01132          PERFORM ALL-MEMBER-PHRASE                                ELSMENUP
01133      ELSE IF SSB-MEDCA-ELIG                                       ELSMENUP
01134          PERFORM ALL-MEDICARE-PHRASE                              ELSMENUP
01135      ELSE IF SSB-MEDCA-INELIG AND                                 ELSMENUP
01136                 SSB-FR-UNDEF     AND                              ELSMENUP
01137                 SSB-PT-AGE-UNDEF                                  ELSMENUP
01138          PERFORM ALL-NON-MEDICARE-PHRASE                          ELSMENUP
01139      ELSE                                                         ELSMENUP
01140          PERFORM SELECTED-FAMILY-MEMBER-PHRASE.                   ELSMENUP
01141                                                                   ELSMENUP
01142                                                                   ELSMENUP
01143 ************************************************************      ELSMENUP
01144 *                                                          *      ELSMENUP
01145 *        ALL MEMBER PHRASE                                 *      ELSMENUP
01146 *                                                          *      ELSMENUP
01147 ************************************************************      ELSMENUP
01148  ALL-MEMBER-PHRASE.                                               ELSMENUP
01149      MOVE ' ALL FAMILY MEMBERS ' TO WS-HDR1-FAM-REL.              ELSMENUP
01150                                                                   ELSMENUP
01151                                                                   ELSMENUP
01152 ************************************************************      ELSMENUP
01153 *                                                          *      ELSMENUP
01154 *        ALL MEDICARE PHRASE                               *      ELSMENUP
01155 *                                                          *      ELSMENUP
01156 ************************************************************      ELSMENUP
01157  ALL-MEDICARE-PHRASE.                                             ELSMENUP
01158      MOVE ' ALL MEDICARE ' TO WS-HDR1-FAM-REL.                    ELSMENUP
01159                                                                   ELSMENUP
01160                                                                   ELSMENUP
01161 ************************************************************      ELSMENUP
01162 *                                                          *      ELSMENUP
01163 *        ALL NON MEDICARE PHRASE                           *      ELSMENUP
01164 *                                                          *      ELSMENUP
01165 ************************************************************      ELSMENUP
01166  ALL-NON-MEDICARE-PHRASE.                                         ELSMENUP
01167      MOVE ' ALL NON-MEDICARE ' TO WS-HDR1-FAM-REL.                ELSMENUP
01168 /***********************************************************      ELSMENUP
01169 *                                                          *      ELSMENUP
01170 *        SELECTED FAMILY MEMBER PHRASE                     *      ELSMENUP
01171 *                                                          *      ELSMENUP
01172 ************************************************************      ELSMENUP
01173  SELECTED-FAMILY-MEMBER-PHRASE.                                   ELSMENUP
01174      PERFORM SIMPLE-SELECTED-FAMILY-PHRASE.                       ELSMENUP
01175                                                                   ELSMENUP
01176 ************************************************************      ELSMENUP
01177 *                                                          *      ELSMENUP
01178 *        SIMPLE SELECTED FAMILY PHRASE                     *      ELSMENUP
01179 *                                                          *      ELSMENUP
01180 ************************************************************      ELSMENUP
01181  SIMPLE-SELECTED-FAMILY-PHRASE.                                   ELSMENUP
01182      IF SSB-MEMBER                                                ELSMENUP
01183          PERFORM MEMBER-PHRASE                                    ELSMENUP
01184      ELSE IF SSB-SPOUSE                                           ELSMENUP
01185          PERFORM SPOUSE-PHRASE                                    ELSMENUP
01186      ELSE IF SSB-DEPENDENT                                        ELSMENUP
01187          PERFORM DEPENDENT-PHRASE                                 ELSMENUP
01188      ELSE                                                         ELSMENUP
01189          PERFORM UNKNOWN-FAMILY-SELECTION.                        ELSMENUP
01190                                                                   ELSMENUP
01191                                                                   ELSMENUP
01192 ************************************************************      ELSMENUP
01193 *                                                          *      ELSMENUP
01194 *        MEMBER PHRASE                                     *      ELSMENUP
01195 *                                                          *      ELSMENUP
01196 ************************************************************      ELSMENUP
01197  MEMBER-PHRASE.                                                   ELSMENUP
01198      MOVE ' MEMBER ' TO WS-HDR1-FAM-REL.                          ELSMENUP
01199                                                                   ELSMENUP
01200                                                                   ELSMENUP
01201 ************************************************************      ELSMENUP
01202 *                                                          *      ELSMENUP
01203 *        SPOUSE PHRASE                                     *      ELSMENUP
01204 *                                                          *      ELSMENUP
01205 ************************************************************      ELSMENUP
01206  SPOUSE-PHRASE.                                                   ELSMENUP
01207      MOVE ' SPOUSE ' TO WS-HDR1-FAM-REL.                          ELSMENUP
01208                                                                   ELSMENUP
01209                                                                   ELSMENUP
01210 ************************************************************      ELSMENUP
01211 *                                                          *      ELSMENUP
01212 *        DEPENDENT PHRASE                                  *      ELSMENUP
01213 *                                                          *      ELSMENUP
01214 ************************************************************      ELSMENUP
01215  DEPENDENT-PHRASE.                                                ELSMENUP
01216      MOVE ' DEPENDENT ' TO WS-HDR1-FAM-REL.                       ELSMENUP
01217                                                                   ELSMENUP
01218 ************************************************************      ELSMENUP
01219 *                                                          *      ELSMENUP
01220 *        STRING PATIENT AGE DESCRIPTION                    *      ELSMENUP
01221 *                                                          *      ELSMENUP
01222 ************************************************************      ELSMENUP
01223 *STRING-PATIENT-AGE-DESCRIPTION.                                  ELSMENUP
01224 *    STRING WS-HDR1-FAM-REL DELIMITED BY '  '                     ELSMENUP
01225 *           ' ' CMF-DESCR-LINE (1) DELIMITED BY SIZE              ELSMENUP
01226 *           INTO WS-HDR1-FAM-REL.                                 ELSMENUP
01227                                                                   ELSMENUP
01228                                                                   ELSMENUP
01229 /***********************************************************      ELSMENUP
01230 *                                                          *      ELSMENUP
01231 *        CONVERT SERVICE FROM DATE                         *      ELSMENUP
01232 *                                                          *      ELSMENUP
01233 ************************************************************      ELSMENUP
01234  CONVERT-SERVICE-FROM-DATE.                                       ELSMENUP
01235      MOVE SSB-SRV-FROM-DATE TO HGADATE-JULIAN1.                   ELSMENUP
01236      PERFORM JULIAN-TO-GREG-CONVERSION.                           ELSMENUP
01237      IF HGADATE-RETURN NOT = '00'                                 ELSMENUP
01238          PERFORM MOVE-ZEROES-TO-DATE-FROM                         ELSMENUP
01239      ELSE                                                         ELSMENUP
01240          PERFORM MOVE-RETURNED-DATE-TO-DATE-FRO.                  ELSMENUP
01241                                                                   ELSMENUP
01242                                                                   ELSMENUP
01243 ************************************************************      ELSMENUP
01244 *                                                          *      ELSMENUP
01245 *        CONVERT SERVICE TO DATE                           *      ELSMENUP
01246 *                                                          *      ELSMENUP
01247 ************************************************************      ELSMENUP
01248  CONVERT-SERVICE-TO-DATE.                                         ELSMENUP
01249      MOVE SSB-SRV-TO-DATE TO HGADATE-JULIAN1.                     ELSMENUP
01250      PERFORM JULIAN-TO-GREG-CONVERSION.                           ELSMENUP
01251      IF HGADATE-RETURN NOT = '00'                                 ELSMENUP
01252          PERFORM MOVE-ZEROES-TO-DATE-TO                           ELSMENUP
01253      ELSE                                                         ELSMENUP
01254          PERFORM MOVE-RETURNED-DATE-TO-DATE-TO.                   ELSMENUP
01255                                                                   ELSMENUP
01256                                                                   ELSMENUP
01257 ************************************************************      ELSMENUP
01258 *                                                          *      ELSMENUP
01259 *        MOVE RETURNED DATE TO DATE FROM                   *      ELSMENUP
01260 *                                                          *      ELSMENUP
01261 ************************************************************      ELSMENUP
01262  MOVE-RETURNED-DATE-TO-DATE-FRO.                                  ELSMENUP
01263      MOVE HGADATE-DATE2 TO WS-HDR1-FROM-DATE.                     ELSMENUP
01264                                                                   ELSMENUP
01265                                                                   ELSMENUP
01266 ************************************************************      ELSMENUP
01267 *                                                          *      ELSMENUP
01268 *        MOVE RETURNED DATE TO DATE TO                     *      ELSMENUP
01269 *                                                          *      ELSMENUP
01270 ************************************************************      ELSMENUP
01271  MOVE-RETURNED-DATE-TO-DATE-TO.                                   ELSMENUP
01272      MOVE HGADATE-DATE2 TO WS-HDR1-TO-DATE.                       ELSMENUP
01273 /***********************************************************      ELSMENUP
01274 *                                                          *      ELSMENUP
01275 *        MOVE ZEROES TO DATE FROM                          *      ELSMENUP
01276 *                                                          *      ELSMENUP
01277 ************************************************************      ELSMENUP
01278  MOVE-ZEROES-TO-DATE-FROM.                                        ELSMENUP
01279      MOVE ZEROES TO WS-HDR1-FROM-DATE.                            ELSMENUP
01280                                                                   ELSMENUP
01281                                                                   ELSMENUP
01282 ************************************************************      ELSMENUP
01283 *                                                          *      ELSMENUP
01284 *        MOVE ZEROES TO DATE TO                            *      ELSMENUP
01285 *                                                          *      ELSMENUP
01286 ************************************************************      ELSMENUP
01287  MOVE-ZEROES-TO-DATE-TO.                                          ELSMENUP
01288      MOVE ZEROES TO WS-HDR1-TO-DATE.                              ELSMENUP
01289                                                                   ELSMENUP
01290                                                                   ELSMENUP
01291 ************************************************************      ELSMENUP
01292 *                                                          *      ELSMENUP
01293 *        JULIAN TO GREG CONVERSION                         *      ELSMENUP
01294 *                                                          *      ELSMENUP
01295 ************************************************************      ELSMENUP
01296  JULIAN-TO-GREG-CONVERSION.                                       ELSMENUP
01297      MOVE 'CNV'  TO  HGADATE-FUNC.                                ELSMENUP
01298      MOVE 'J'  TO  HGADATE-FORM1.                                 ELSMENUP
01299      MOVE 'M'  TO  HGADATE-FORM2.                                 ELSMENUP
01300      MOVE ZEROS  TO  HGADATE-RETURN,   HGADATE-AMOUNT             ELSMENUP
01301                      HGADATE-DATE2.                               ELSMENUP
01302      EXEC CICS  LINK PROGRAM('HGADATES')                          ELSMENUP
01303          COMMAREA(WS-HGADATES-PARMS)                              ELSMENUP
01304          END-EXEC.                                                ELSMENUP
01305 /***********************************************************      ELSMENUP
01306 *                                                          *      ELSMENUP
01307 *        UNKNOWN FAMILY SELECTION                          *      ELSMENUP
01308 *                                                          *      ELSMENUP
01309 ************************************************************      ELSMENUP
01310  UNKNOWN-FAMILY-SELECTION.                                        ELSMENUP
01311      MOVE SPACES TO WS-HDR1-FAM-REL.                              ELSMENUP
01312                                                                   ELSMENUP
01313                                                                   ELSMENUP
01314 ************************************************************      ELSMENUP
01315 *                                                          *      ELSMENUP
01316 *        TEXT FORMAT HEADING                               *      ELSMENUP
01317 *                                                          *      ELSMENUP
01318 ************************************************************      ELSMENUP
01319  TEXT-FORMAT-HEADING.                                             ELSMENUP
01320      MOVE 3  TO  TCAR-OUTPUT-FIELD-COUNT.                         ELSMENUP
01321      MOVE LENGTH OF HEADO TO                                      ELSMENUP
01322          TCAR-OUTPUT-FIELD-1-LEN                                  ELSMENUP
01323          TCAR-OUTPUT-FIELD-2-LEN                                  ELSMENUP
01324          TCAR-OUTPUT-FIELD-3-LEN.                                 ELSMENUP
01325      PERFORM TEXT-FORMAT.                                         ELSMENUP
01326                                                                   ELSMENUP
01327                                                                   ELSMENUP
01328 ************************************************************      ELSMENUP
01329 *                                                          *      ELSMENUP
01330 *        MOVE HEADINGS TO SCREEN                           *      ELSMENUP
01331 *                                                          *      ELSMENUP
01332 ************************************************************      ELSMENUP
01333  MOVE-HEADINGS-TO-SCREEN.                                         ELSMENUP
01334      MOVE TCAR-OPF-DATA (1)   TO  HEADO (1).                      ELSMENUP
01335      MOVE TCAR-OPF-DATA (2)   TO  HEADO (2).                      ELSMENUP
01336      MOVE TCAR-OPF-DATA (3)   TO  HEADO (3).                      ELSMENUP
01337                                                                   ELSMENUP
01338                                                                   ELSMENUP
01339 ************************************************************      ELSMENUP
01340 *                                                          *      ELSMENUP
01341 *        BUILD CALLER HEADING                              *      ELSMENUP
01342 *                                                          *      ELSMENUP
01343 ************************************************************      ELSMENUP
01344  BUILD-CALLER-HEADING.                                            ELSMENUP
01345      PERFORM                                                      ELSMENUP
01346          VARYING WS-DESCR-SUB FROM 1 BY 1                         ELSMENUP
01347                  UNTIL WS-DESCR-SUB > MHD-NBR-HDG-LINES           ELSMENUP
01348          MOVE MHD-HDG-LINE (WS-DESCR-SUB)                         ELSMENUP
01349              TO DESCRO (WS-DESCR-SUB)                             ELSMENUP
01350      END-PERFORM.                                                 ELSMENUP
01351 /***********************************************************      ELSMENUP
01352 *                                                          *      ELSMENUP
01353 *        HEADING ONLY MENU                                 *      ELSMENUP
01354 *                                                          *      ELSMENUP
01355 ************************************************************      ELSMENUP
01356  HEADING-ONLY-MENU.                                               ELSMENUP
01357      PERFORM SET-UP-DEFAULT-RESPONSE.                             ELSMENUP
01358      SET SSB-IN-MENU (SSB-SELECTOR-STATE) TO TRUE.                ELSMENUP
01359                                                                   ELSMENUP
01360                                                                   ELSMENUP
01361 ************************************************************      ELSMENUP
01362 *                                                          *      ELSMENUP
01363 *        BUILD FIRST OPTIONS PAGE                          *      ELSMENUP
01364 *                                                          *      ELSMENUP
01365 ************************************************************      ELSMENUP
01366  BUILD-FIRST-OPTIONS-PAGE.                                        ELSMENUP
01367      PERFORM SCROLL-TOP.                                          ELSMENUP
01368      IF WS-SCREEN-OVERFLOW                                        ELSMENUP
01369              AND MHD-NBR-HDG-LINES = WS-ELSMHDG-MVO               ELSMENUP
01370          PERFORM PARAMETER-ERROR-ABEND.                           ELSMENUP
01371      PERFORM SET-UP-DEFAULT-RESPONSE.                             ELSMENUP
01372      SET SSB-IN-MENU (SSB-SELECTOR-STATE)                         ELSMENUP
01373          TO TRUE.                                                 ELSMENUP
01374                                                                   ELSMENUP
01375                                                                   ELSMENUP
01376 ************************************************************      ELSMENUP
01377 *                                                          *      ELSMENUP
01378 *        BUILD OPTIONS PAGE                                *      ELSMENUP
01379 *                                                          *      ELSMENUP
01380 ************************************************************      ELSMENUP
01381  BUILD-OPTIONS-PAGE.                                              ELSMENUP
01382      PERFORM SET-UP-DEFAULT-RESPONSE.                             ELSMENUP
01383      IF WS-NO-SCROLL                                              ELSMENUP
01384               OR WS-ERROR-MESSAGE-ISSUED                          ELSMENUP
01385          PERFORM NO-SCROLL                                        ELSMENUP
01386      ELSE IF WS-SCROLL-DOWN                                       ELSMENUP
01387          PERFORM SCROLL-DOWN                                      ELSMENUP
01388      ELSE IF WS-SCROLL-UP                                         ELSMENUP
01389          PERFORM SCROLL-UP                                        ELSMENUP
01390      ELSE IF WS-SCROLL-TOP                                        ELSMENUP
01391          PERFORM SCROLL-TOP                                       ELSMENUP
01392      ELSE IF WS-SCROLL-BOTTOM                                     ELSMENUP
01393          PERFORM SCROLL-BOTTOM                                    ELSMENUP
01394      ELSE                                                         ELSMENUP
01395          PERFORM INVALID-SCROLL-REQUEST.                          ELSMENUP
01396                                                                   ELSMENUP
01397                                                                   ELSMENUP
01398 ************************************************************      ELSMENUP
01399 *                                                          *      ELSMENUP
01400 *        SET UP DEFAULT RESPONSE                           *      ELSMENUP
01401 *                                                          *      ELSMENUP
01402 ************************************************************      ELSMENUP
01403  SET-UP-DEFAULT-RESPONSE.                                         ELSMENUP
01404      IF WS-SEND-INITIAL-SCREEN                                    ELSMENUP
01405          PERFORM SET-THE-DEFAULT                                  ELSMENUP
01406      ELSE                                                         ELSMENUP
01407          PERFORM SEND-RESPONSE-AGAIN.                             ELSMENUP
01408                                                                   ELSMENUP
01409                                                                   ELSMENUP
01410 ************************************************************      ELSMENUP
01411 *                                                          *      ELSMENUP
01412 *        SET THE DEFAULT                                   *      ELSMENUP
01413 *                                                          *      ELSMENUP
01414 ************************************************************      ELSMENUP
01415  SET-THE-DEFAULT.                                                 ELSMENUP
01416      IF SSB-MNU-CHOICE (1) = SPACES OR LOW-VALUES                 ELSMENUP
01417          PERFORM BLANK-SELECTION                                  ELSMENUP
01418      ELSE                                                         ELSMENUP
01419          PERFORM USE-THE-DEFAULT.                                 ELSMENUP
01420 /***********************************************************      ELSMENUP
01421 *                                                          *      ELSMENUP
01422 *        USE THE DEFAULT                                   *      ELSMENUP
01423 *                                                          *      ELSMENUP
01424 ************************************************************      ELSMENUP
01425  USE-THE-DEFAULT.                                                 ELSMENUP
01426      MOVE SSB-MNU-CHOICE-TABLE TO                                 ELSMENUP
01427           TCAR-FROM-AREA.                                         ELSMENUP
01428      COMPUTE TCAR-AREA-LENGTH = MSO-MAX-CHOICES *                 ELSMENUP
01429           LENGTH OF SSB-MNU-CHOICE.                               ELSMENUP
01430      PERFORM TEXT-COMPRESSION.                                    ELSMENUP
01431      IF TCAR-L NOT < LENGTH OF SELECTNO                           ELSMENUP
01432          PERFORM BLANK-SELECTION                                  ELSMENUP
01433      ELSE                                                         ELSMENUP
01434          PERFORM MOVE-THE-COMPRESSED-RESPONSE.                    ELSMENUP
01435                                                                   ELSMENUP
01436                                                                   ELSMENUP
01437 ************************************************************      ELSMENUP
01438 *                                                          *      ELSMENUP
01439 *        BLANK SELECTION                                   *      ELSMENUP
01440 *                                                          *      ELSMENUP
01441 ************************************************************      ELSMENUP
01442  BLANK-SELECTION.                                                 ELSMENUP
01443      MOVE SPACES         TO SELECTNO                              ELSMENUP
01444                             SSB-MNU-CHOICE-TABLE.                 ELSMENUP
01445                                                                   ELSMENUP
01446                                                                   ELSMENUP
01447 ************************************************************      ELSMENUP
01448 *                                                          *      ELSMENUP
01449 *        MOVE THE COMPRESSED RESPONSE                      *      ELSMENUP
01450 *                                                          *      ELSMENUP
01451 ************************************************************      ELSMENUP
01452  MOVE-THE-COMPRESSED-RESPONSE.                                    ELSMENUP
01453      MOVE TCAR-TO-AREA  TO  SELECTNO                              ELSMENUP
01454                             SSB-MNU-CHOICE-TABLE.                 ELSMENUP
01455 /***********************************************************      ELSMENUP
01456 *                                                          *      ELSMENUP
01457 *        SEND RESPONSE AGAIN                               *      ELSMENUP
01458 *                                                          *      ELSMENUP
01459 ************************************************************      ELSMENUP
01460  SEND-RESPONSE-AGAIN.                                             ELSMENUP
01461      MOVE WS-HOLD-RESPONSE TO SELECTNO.                           ELSMENUP
01462                                                                   ELSMENUP
01463                                                                   ELSMENUP
01464 ************************************************************      ELSMENUP
01465 *                                                          *      ELSMENUP
01466 *        NO SCROLL                                         *      ELSMENUP
01467 *                                                          *      ELSMENUP
01468 ************************************************************      ELSMENUP
01469  NO-SCROLL.                                                       ELSMENUP
01470      MOVE SSB-MNU-CUR-TOP-ITM                                     ELSMENUP
01471           TO IOP-TSQ-ITEM-NBR.                                    ELSMENUP
01472      PERFORM BUILD-MENU-OPTIONS-FORWARD.                          ELSMENUP
01473                                                                   ELSMENUP
01474                                                                   ELSMENUP
01475 ************************************************************      ELSMENUP
01476 *                                                          *      ELSMENUP
01477 *        SCROLL DOWN                                       *      ELSMENUP
01478 *                                                          *      ELSMENUP
01479 ************************************************************      ELSMENUP
01480  SCROLL-DOWN.                                                     ELSMENUP
01481      ADD 1, SSB-MNU-CUR-BOT-ITM                                   ELSMENUP
01482           GIVING IOP-TSQ-ITEM-NBR.                                ELSMENUP
01483      IF IOP-TSQ-ITEM-NBR > MSO-NBR-MENU-OPTS                      ELSMENUP
01484          PERFORM SCROLL-TOP                                       ELSMENUP
01485      ELSE                                                         ELSMENUP
01486          PERFORM BUILD-MENU-OPTIONS-FORWARD.                      ELSMENUP
01487                                                                   ELSMENUP
01488                                                                   ELSMENUP
01489 ************************************************************      ELSMENUP
01490 *                                                          *      ELSMENUP
01491 *        SCROLL UP                                         *      ELSMENUP
01492 *                                                          *      ELSMENUP
01493 ************************************************************      ELSMENUP
01494  SCROLL-UP.                                                       ELSMENUP
01495      SUBTRACT 1 FROM SSB-MNU-CUR-TOP-ITM                          ELSMENUP
01496           GIVING IOP-TSQ-ITEM-NBR.                                ELSMENUP
01497      IF IOP-TSQ-ITEM-NBR < 1                                      ELSMENUP
01498          PERFORM SCROLL-BOTTOM                                    ELSMENUP
01499      ELSE                                                         ELSMENUP
01500          PERFORM BUILD-MENU-OPTIONS-BACKWARD.                     ELSMENUP
01501 /***********************************************************      ELSMENUP
01502 *                                                          *      ELSMENUP
01503 *        SCROLL TOP                                        *      ELSMENUP
01504 *                                                          *      ELSMENUP
01505 ************************************************************      ELSMENUP
01506  SCROLL-TOP.                                                      ELSMENUP
01507      MOVE 1 TO IOP-TSQ-ITEM-NBR.                                  ELSMENUP
01508      PERFORM BUILD-MENU-OPTIONS-FORWARD.                          ELSMENUP
01509                                                                   ELSMENUP
01510                                                                   ELSMENUP
01511 ************************************************************      ELSMENUP
01512 *                                                          *      ELSMENUP
01513 *        SCROLL BOTTOM                                     *      ELSMENUP
01514 *                                                          *      ELSMENUP
01515 ************************************************************      ELSMENUP
01516  SCROLL-BOTTOM.                                                   ELSMENUP
01517      MOVE MSO-NBR-MENU-OPTS  TO IOP-TSQ-ITEM-NBR.                 ELSMENUP
01518      PERFORM BUILD-MENU-OPTIONS-BACKWARD.                         ELSMENUP
01519                                                                   ELSMENUP
01520                                                                   ELSMENUP
01521 ************************************************************      ELSMENUP
01522 *                                                          *      ELSMENUP
01523 *        BUILD MENU OPTIONS BACKWARD                       *      ELSMENUP
01524 *                                                          *      ELSMENUP
01525 ************************************************************      ELSMENUP
01526  BUILD-MENU-OPTIONS-BACKWARD.                                     ELSMENUP
01527      SET WS-NO-SCREEN-OVERFLOW TO TRUE.                           ELSMENUP
01528      MOVE IOP-TSQ-ITEM-NBR TO SSB-MNU-CUR-BOT-ITM.                ELSMENUP
01529      MOVE 15 TO WS-BOTTOM-DESCR.                                  ELSMENUP
01530      PERFORM BUILD-MENU-BACKWARD                                  ELSMENUP
01531          UNTIL WS-SCREEN-OVERFLOW                                 ELSMENUP
01532                  OR IOP-TSQ-ITEM-NBR < 1.                         ELSMENUP
01533      IF WS-BOTTOM-DESCR > WS-DESCR-SUB                            ELSMENUP
01534          PERFORM MOVE-SCREEN-UP.                                  ELSMENUP
01535 /***********************************************************      ELSMENUP
01536 *                                                          *      ELSMENUP
01537 *        BUILD MENU BACKWARD                               *      ELSMENUP
01538 *                                                          *      ELSMENUP
01539 ************************************************************      ELSMENUP
01540  BUILD-MENU-BACKWARD.                                             ELSMENUP
01541      PERFORM READ-DESCRIPTION-LINE.                               ELSMENUP
01542      IF (WS-BOTTOM-DESCR - MSD-NBR-DESCR-LINES + 1)               ELSMENUP
01543              NOT < WS-DESCR-SUB                                   ELSMENUP
01544          PERFORM MOVE-DESCRIPTION-LINES-TO-UPPE                   ELSMENUP
01545      ELSE                                                         ELSMENUP
01546          PERFORM SCREEN-OVERFLOW.                                 ELSMENUP
01547      SUBTRACT 1 FROM IOP-TSQ-ITEM-NBR.                            ELSMENUP
01548                                                                   ELSMENUP
01549                                                                   ELSMENUP
01550 ************************************************************      ELSMENUP
01551 *                                                          *      ELSMENUP
01552 *        MOVE DESCRIPTION LINES TO UPPER SCREEN            *      ELSMENUP
01553 *                                                          *      ELSMENUP
01554 ************************************************************      ELSMENUP
01555  MOVE-DESCRIPTION-LINES-TO-UPPE.                                  ELSMENUP
01556      PERFORM MOVE-LINE-TO-UPPER-SCREEN                            ELSMENUP
01557          VARYING WS-MOVE-POS                                      ELSMENUP
01558                  FROM MSD-NBR-DESCR-LINES BY -1                   ELSMENUP
01559                  UNTIL WS-MOVE-POS < 1.                           ELSMENUP
01560      MOVE IOP-TSQ-ITEM-NBR TO SSB-MNU-CUR-TOP-ITM.                ELSMENUP
01561                                                                   ELSMENUP
01562                                                                   ELSMENUP
01563 ************************************************************      ELSMENUP
01564 *                                                          *      ELSMENUP
01565 *        MOVE LINE TO UPPER SCREEN                         *      ELSMENUP
01566 *                                                          *      ELSMENUP
01567 ************************************************************      ELSMENUP
01568  MOVE-LINE-TO-UPPER-SCREEN.                                       ELSMENUP
01569      MOVE MSD-DESCR-LINE (WS-MOVE-POS)                            ELSMENUP
01570          TO DESCRO (WS-BOTTOM-DESCR).                             ELSMENUP
01571      SUBTRACT 1 FROM WS-BOTTOM-DESCR.                             ELSMENUP
01572 /***********************************************************      ELSMENUP
01573 *                                                          *      ELSMENUP
01574 *        MOVE SCREEN UP                                    *      ELSMENUP
01575 *                                                          *      ELSMENUP
01576 ************************************************************      ELSMENUP
01577  MOVE-SCREEN-UP.                                                  ELSMENUP
01578      MOVE 1 TO IOP-TSQ-ITEM-NBR.                                  ELSMENUP
01579      PERFORM CLEAR-DESCRIPTION-LINE                               ELSMENUP
01580          VARYING WS-CLEAR-SUB FROM WS-DESCR-SUB BY 1              ELSMENUP
01581                  UNTIL WS-CLEAR-SUB > 15.                         ELSMENUP
01582      PERFORM BUILD-MENU-OPTIONS-FORWARD.                          ELSMENUP
01583                                                                   ELSMENUP
01584                                                                   ELSMENUP
01585 ************************************************************      ELSMENUP
01586 *                                                          *      ELSMENUP
01587 *        CLEAR DESCRIPTION LINE                            *      ELSMENUP
01588 *                                                          *      ELSMENUP
01589 ************************************************************      ELSMENUP
01590  CLEAR-DESCRIPTION-LINE.                                          ELSMENUP
01591      MOVE SPACES TO DESCRO (WS-CLEAR-SUB).                        ELSMENUP
01592                                                                   ELSMENUP
01593                                                                   ELSMENUP
01594 ************************************************************      ELSMENUP
01595 *                                                          *      ELSMENUP
01596 *        BUILD MENU OPTIONS FORWARD                        *      ELSMENUP
01597 *                                                          *      ELSMENUP
01598 ************************************************************      ELSMENUP
01599  BUILD-MENU-OPTIONS-FORWARD.                                      ELSMENUP
01600      SET WS-NO-SCREEN-OVERFLOW TO TRUE.                           ELSMENUP
01601      MOVE IOP-TSQ-ITEM-NBR TO SSB-MNU-CUR-TOP-ITM.                ELSMENUP
01602      PERFORM BUILD-MENU-FORWARD                                   ELSMENUP
01603          UNTIL WS-SCREEN-OVERFLOW                                 ELSMENUP
01604                  OR IOP-TSQ-ITEM-NBR > MSO-NBR-MENU-OPTS.         ELSMENUP
01605                                                                   ELSMENUP
01606                                                                   ELSMENUP
01607 ************************************************************      ELSMENUP
01608 *                                                          *      ELSMENUP
01609 *        BUILD MENU FORWARD                                *      ELSMENUP
01610 *                                                          *      ELSMENUP
01611 ************************************************************      ELSMENUP
01612  BUILD-MENU-FORWARD.                                              ELSMENUP
01613      PERFORM READ-DESCRIPTION-LINE.                               ELSMENUP
01614      IF (MSD-NBR-DESCR-LINES + WS-DESCR-SUB - 1)                  ELSMENUP
01615                NOT > 15                                           ELSMENUP
01616          PERFORM MOVE-DESCRIPTION-LINES-TO-LOWE                   ELSMENUP
01617      ELSE                                                         ELSMENUP
01618          PERFORM SCREEN-OVERFLOW.                                 ELSMENUP
01619      ADD 1 TO IOP-TSQ-ITEM-NBR.                                   ELSMENUP
01620 /***********************************************************      ELSMENUP
01621 *                                                          *      ELSMENUP
01622 *        MOVE DESCRIPTION LINES TO LOWER SCREEN            *      ELSMENUP
01623 *                                                          *      ELSMENUP
01624 ************************************************************      ELSMENUP
01625  MOVE-DESCRIPTION-LINES-TO-LOWE.                                  ELSMENUP
01626      PERFORM MOVE-LINE-TO-LOWER-SCREEN                            ELSMENUP
01627          VARYING WS-MOVE-POS FROM 1 BY 1                          ELSMENUP
01628                  UNTIL WS-MOVE-POS > MSD-NBR-DESCR-LINES.         ELSMENUP
01629      MOVE IOP-TSQ-ITEM-NBR TO SSB-MNU-CUR-BOT-ITM.                ELSMENUP
01630                                                                   ELSMENUP
01631                                                                   ELSMENUP
01632 ************************************************************      ELSMENUP
01633 *                                                          *      ELSMENUP
01634 *        MOVE LINE TO LOWER SCREEN                         *      ELSMENUP
01635 *                                                          *      ELSMENUP
01636 ************************************************************      ELSMENUP
01637  MOVE-LINE-TO-LOWER-SCREEN.                                       ELSMENUP
01638      MOVE MSD-DESCR-LINE (WS-MOVE-POS)                            ELSMENUP
01639          TO DESCRO (WS-DESCR-SUB).                                ELSMENUP
01640      ADD 1 TO WS-DESCR-SUB.                                       ELSMENUP
01641                                                                   ELSMENUP
01642                                                                   ELSMENUP
01643 ************************************************************      ELSMENUP
01644 *                                                          *      ELSMENUP
01645 *        SCREEN OVERFLOW                                   *      ELSMENUP
01646 *                                                          *      ELSMENUP
01647 ************************************************************      ELSMENUP
01648  SCREEN-OVERFLOW.                                                 ELSMENUP
01649      SET WS-SCREEN-OVERFLOW TO TRUE.                              ELSMENUP
01650                                                                   ELSMENUP
01651                                                                   ELSMENUP
01652 ************************************************************      ELSMENUP
01653 *                                                          *      ELSMENUP
01654 *        READ DESCRIPTION LINE                             *      ELSMENUP
01655 *                                                          *      ELSMENUP
01656 ************************************************************      ELSMENUP
01657  READ-DESCRIPTION-LINE.                                           ELSMENUP
01658      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSMENUP
01659      SET IOP-RD TO TRUE.                                          ELSMENUP
01660      SET IOP-FCQ-NONE TO TRUE.                                    ELSMENUP
01661      SET IOP-KVQ-NONE TO TRUE.                                    ELSMENUP
01662      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELSMENUP
01663      EXEC CICS LINK                                               ELSMENUP
01664                PROGRAM('ELUIOPGM')                                ELSMENUP
01665                COMMAREA(DFHCOMMAREA)                              ELSMENUP
01666                END-EXEC.                                          ELSMENUP
01667      SET ADDRESS OF MSD-MENU-ITEM-DESCRIPTIONS                    ELSMENUP
01668            TO IOP-REC-PTR.                                        ELSMENUP
01669 /***********************************************************      ELSMENUP
01670 *                                                          *      ELSMENUP
01671 *        INVALID SCROLL REQUEST                            *      ELSMENUP
01672 *                                                          *      ELSMENUP
01673 ************************************************************      ELSMENUP
01674  INVALID-SCROLL-REQUEST.                                          ELSMENUP
01675      MOVE 'NOTHING TO SCROLL TO' TO ERRMSGO.                      ELSMENUP
01676      SET WS-ERROR-MESSAGE-ISSUED TO TRUE.                         ELSMENUP
01677                                                                   ELSMENUP
01678                                                                   ELSMENUP
01679 ************************************************************      ELSMENUP
01680 *                                                          *      ELSMENUP
01681 *        TERMINATION                                       *      ELSMENUP
01682 *                                                          *      ELSMENUP
01683 ************************************************************      ELSMENUP
01684  TERMINATION.                                                     ELSMENUP
01685      IF SSB-IN-MENU (SSB-SELECTOR-STATE)                          ELSMENUP
01686          PERFORM PREPARE-FOR-REENTRY.                             ELSMENUP
01687      EXEC CICS RETURN                                             ELSMENUP
01688                END-EXEC.                                          ELSMENUP
01689                                                                   ELSMENUP
01690                                                                   ELSMENUP
01691 ************************************************************      ELSMENUP
01692 *                                                          *      ELSMENUP
01693 *        PREPARE FOR REENTRY                               *      ELSMENUP
01694 *                                                          *      ELSMENUP
01695 ************************************************************      ELSMENUP
01696  PREPARE-FOR-REENTRY.                                             ELSMENUP
01697      IF WS-STOW-MENU-ITEMS                                        ELSMENUP
01698          PERFORM STOW-THE-MENU-HEADINGS.                          ELSMENUP
01699      IF WS-STOW-MENU-ITEMS                                        ELSMENUP
01700          PERFORM STOW-THE-MENU-OPTIONS.                           ELSMENUP
01701      PERFORM WRITE-THE-ERROR-MSG.                                 ELSMENUP
01702      PERFORM WRITE-THE-MENU-SCREEN.                               ELSMENUP
01703                                                                   ELSMENUP
01704                                                                   ELSMENUP
01705 ************************************************************      ELSMENUP
01706 *                                                          *      ELSMENUP
01707 *        STOW THE MENU HEADINGS                            *      ELSMENUP
01708 *                                                          *      ELSMENUP
01709 ************************************************************      ELSMENUP
01710  STOW-THE-MENU-HEADINGS.                                          ELSMENUP
01711      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSMENUP
01712      SET CIA-STG-STOW TO TRUE.                                    ELSMENUP
01713      MOVE LENGTH OF MHD-MENU-HEADINGS                             ELSMENUP
01714          TO CIA-AREA-LEN.                                         ELSMENUP
01715      PERFORM CALL-STORAGE-MANAGER.                                ELSMENUP
01716 /***********************************************************      ELSMENUP
01717 *                                                          *      ELSMENUP
01718 *        STOW THE MENU OPTIONS                             *      ELSMENUP
01719 *                                                          *      ELSMENUP
01720 ************************************************************      ELSMENUP
01721  STOW-THE-MENU-OPTIONS.                                           ELSMENUP
01722      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSMENUP
01723      SET CIA-STG-STOW TO TRUE.                                    ELSMENUP
01724      MOVE LENGTH OF MSO-MENU-SELECTION-VALUES                     ELSMENUP
01725          TO CIA-AREA-LEN.                                         ELSMENUP
01726      PERFORM CALL-STORAGE-MANAGER.                                ELSMENUP
01727                                                                   ELSMENUP
01728                                                                   ELSMENUP
01729 ************************************************************      ELSMENUP
01730 *                                                          *      ELSMENUP
01731 *        WRITE THE ERROR MSG                               *      ELSMENUP
01732 *                                                          *      ELSMENUP
01733 ************************************************************      ELSMENUP
01734  WRITE-THE-ERROR-MSG.                                             ELSMENUP
01735      EXEC CICS SEND                                               ELSMENUP
01736                MAP('EL00MAP')                                     ELSMENUP
01737                MAPSET('EL00SET')                                  ELSMENUP
01738                FROM(EL00MAPO)                                     ELSMENUP
01739                CURSOR(EIBCPOSN)                                   ELSMENUP
01740                END-EXEC.                                          ELSMENUP
01741                                                                   ELSMENUP
01742                                                                   ELSMENUP
01743 ************************************************************      ELSMENUP
01744 *                                                          *      ELSMENUP
01745 *        WRITE THE MENU SCREEN                             *      ELSMENUP
01746 *                                                          *      ELSMENUP
01747 ************************************************************      ELSMENUP
01748  WRITE-THE-MENU-SCREEN.                                           ELSMENUP
01749      MOVE -1 TO SELECTNL.                                         ELSMENUP
01750      PERFORM SET-THE-MENU-DESCRIPTION-ATTRI.                      ELSMENUP
01751      MOVE DFHBMFSE TO SELECTNA.                                   ELSMENUP
01752      IF WS-SEND-INITIAL-SCREEN                                    ELSMENUP
01753          PERFORM SEND-INITIAL-SCREEN                              ELSMENUP
01754      ELSE                                                         ELSMENUP
01755          PERFORM SEND-SECONDARY-SCREEN.                           ELSMENUP
01756                                                                   ELSMENUP
01757                                                                   ELSMENUP
01758 ************************************************************      ELSMENUP
01759 *                                                          *      ELSMENUP
01760 *        SET THE MENU DESCRIPTION ATTRIBUTES               *      ELSMENUP
01761 *                                                          *      ELSMENUP
01762 ************************************************************      ELSMENUP
01763  SET-THE-MENU-DESCRIPTION-ATTRI.                                  ELSMENUP
01764      PERFORM                                                      ELSMENUP
01765          VARYING WS-DESCR-SUB                                     ELSMENUP
01766                   FROM 1 BY 1                                     ELSMENUP
01767                   UNTIL WS-DESCR-SUB > 15                         ELSMENUP
01768          MOVE DFHBMASK TO  DESCRA (WS-DESCR-SUB)                  ELSMENUP
01769      END-PERFORM.                                                 ELSMENUP
01770 /***********************************************************      ELSMENUP
01771 *                                                          *      ELSMENUP
01772 *        SEND INITIAL SCREEN                               *      ELSMENUP
01773 *                                                          *      ELSMENUP
01774 ************************************************************      ELSMENUP
01775  SEND-INITIAL-SCREEN.                                             ELSMENUP
01776      EXEC CICS SEND                                               ELSMENUP
01777                MAP('EL02MAP')                                     ELSMENUP
01778                MAPSET('EL02SET')                                  ELSMENUP
01779                ERASE                                              ELSMENUP
01780                FROM(EL02MAPO)                                     ELSMENUP
01781                CURSOR                                             ELSMENUP
01782                FREEKB                                             ELSMENUP
01783                END-EXEC.                                          ELSMENUP
01784                                                                   ELSMENUP
01785                                                                   ELSMENUP
01786 ************************************************************      ELSMENUP
01787 *                                                          *      ELSMENUP
01788 *        SEND SECONDARY SCREEN                             *      ELSMENUP
01789 *                                                          *      ELSMENUP
01790 ************************************************************      ELSMENUP
01791  SEND-SECONDARY-SCREEN.                                           ELSMENUP
01792      EXEC CICS SEND                                               ELSMENUP
01793                MAP('EL02MAP')                                     ELSMENUP
01794                MAPSET('EL02SET')                                  ELSMENUP
01795                DATAONLY                                           ELSMENUP
01796                FROM(EL02MAPO)                                     ELSMENUP
01797                CURSOR                                             ELSMENUP
01798                FREEKB                                             ELSMENUP
01799                END-EXEC.                                          ELSMENUP
01800                                                                   ELSMENUP
01801                                                                   ELSMENUP
01802 ************************************************************      ELSMENUP
01803 *                                                          *      ELSMENUP
01804 *        CALL STORAGE MANAGER                              *      ELSMENUP
01805 *                                                          *      ELSMENUP
01806 ************************************************************      ELSMENUP
01807  CALL-STORAGE-MANAGER.                                            ELSMENUP
01808      EXEC CICS LINK                                               ELSMENUP
01809                PROGRAM('ELUSTGMG')                                ELSMENUP
01810                COMMAREA(DFHCOMMAREA)                              ELSMENUP
01811                END-EXEC.                                          ELSMENUP
01812 /***********************************************************      ELSMENUP
01813 *                                                          *      ELSMENUP
01814 *        PARAMETER ERROR ABEND                             *      ELSMENUP
01815 *                                                          *      ELSMENUP
01816 ************************************************************      ELSMENUP
01817  PARAMETER-ERROR-ABEND.                                           ELSMENUP
01818      SET CIA-AB-PARM-ERR TO TRUE.                                 ELSMENUP
01819      EXEC CICS ABEND                                              ELSMENUP
01820                ABCODE(CIA-ABCODE)                                 ELSMENUP
01821                END-EXEC.                                          ELSMENUP
01822                                                                   ELSMENUP
01823                                                                   ELSMENUP
01824 ************************************************************      ELSMENUP
01825 *                                                          *      ELSMENUP
01826 *        MISSING MENU PARM ABEND                           *      ELSMENUP
01827 *                                                          *      ELSMENUP
01828 ************************************************************      ELSMENUP
01829  MISSING-MENU-PARM-ABEND.                                         ELSMENUP
01830      SET CIA-AB-PARM-MISSING TO TRUE.                             ELSMENUP
01831      EXEC CICS ABEND                                              ELSMENUP
01832                ABCODE(CIA-ABCODE)                                 ELSMENUP
01833                END-EXEC.                                          ELSMENUP
01834                                                                   ELSMENUP
01835                                                                   ELSMENUP
01836 ************************************************************      ELSMENUP
01837 *                                                          *      ELSMENUP
01838 *        INVALID COMMAREA ABEND                            *      ELSMENUP
01839 *                                                          *      ELSMENUP
01840 ************************************************************      ELSMENUP
01841  INVALID-COMMAREA-ABEND.                                          ELSMENUP
01842      SET CIA-AB-DFHCOMMAREA TO TRUE.                              ELSMENUP
01843      EXEC CICS ABEND                                              ELSMENUP
01844                ABCODE(CIA-ABCODE)                                 ELSMENUP
01845                END-EXEC.                                          ELSMENUP
01846                                                                   ELSMENUP
01847                                                                   ELSMENUP
01848 ************************************************************      ELSMENUP
01849 *                                                          *      ELSMENUP
01850 *        SYSTEM LOGIC ERROR                                *      ELSMENUP
01851 *                                                          *      ELSMENUP
01852 ************************************************************      ELSMENUP
01853  SYSTEM-LOGIC-ERROR.                                              ELSMENUP
01854      SET CIA-AB-UNDEF TO TRUE.                                    ELSMENUP
01855      EXEC CICS ABEND                                              ELSMENUP
01856                ABCODE(CIA-ABCODE)                                 ELSMENUP
01857                END-EXEC.                                          ELSMENUP
01858                                                                   ELSMENUP
01859                                                                   ELSMENUP
01860 ************************************************************      ELSMENUP
01861 *                                                          *      ELSMENUP
01862 *        PROGRAM LOGIC ERROR                               *      ELSMENUP
01863 *                                                          *      ELSMENUP
01864 ************************************************************      ELSMENUP
01865  PROGRAM-LOGIC-ERROR.                                             ELSMENUP
01866      SET CIA-AB-PGM-LOGIC TO TRUE.                                ELSMENUP
01867      EXEC CICS ABEND                                              ELSMENUP
01868                ABCODE(CIA-ABCODE)                                 ELSMENUP
01869                END-EXEC.                                          ELSMENUP
01870 /***********************************************************      ELSMENUP
01871 *                                                          *      ELSMENUP
01872 *        TEXT FORMAT                                       *      ELSMENUP
01873 *                                                          *      ELSMENUP
01874 ************************************************************      ELSMENUP
01875  TEXT-FORMAT.                                                     ELSMENUP
01876      PERFORM TEXT-COMPRESSION.                                    ELSMENUP
01877      PERFORM TEXT-UNSTRING.                                       ELSMENUP
01878                                                                   ELSMENUP
01879                                                                   ELSMENUP
01880 ************************************************************      ELSMENUP
01881 *                                                          *      ELSMENUP
01882 *        TEXT COMPRESSION                                  *      ELSMENUP
01883 *                                                          *      ELSMENUP
01884 ************************************************************      ELSMENUP
01885  TEXT-COMPRESSION.                                                ELSMENUP
01886      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELSMENUP
01887                                                                   ELSMENUP
01888                                                                   ELSMENUP
01889 ************************************************************      ELSMENUP
01890 *                                                          *      ELSMENUP
01891 *        TEXT UNSTRING                                     *      ELSMENUP
01892 *                                                          *      ELSMENUP
01893 ************************************************************      ELSMENUP
01894  TEXT-UNSTRING.                                                   ELSMENUP
01895      PERFORM TCPR-000-TEXT-UNSTRING.                              ELSMENUP
01896      COPY ELSTCOMP.                                               ELSMENUP
