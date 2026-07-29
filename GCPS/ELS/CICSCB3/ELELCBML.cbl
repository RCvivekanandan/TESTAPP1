00001  ID DIVISION.                                                     06/29/02
00002  PROGRAM-ID.    ELELCBML.                                         ELELCBML
00003  AUTHOR.        NINA CERVANTES.                                      LV001
00004  INSTALLATION.  BCBS/HCMS.                                        ELELCBML
00005  DATE-WRITTEN.  09/85.                                            ELELCBML
00006  DATE-COMPILED.   /  /  .                                         ELELCBML
00007 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ELELCBML
00008 *       CCCCCCC OOOOOOOO  BBBBBBBBB OOOOOOOO LLLL   2222222222  * ELELCBML
00009 *     CCC      OOO   OOO BBB   BBB OOO   OOO LLL     222  222   * ELELCBML
00010 *    CCC      OOO   OOO BBB   BBB OOO   OOO LLL      222  222   * ELELCBML
00011 *   CCC      OOO   OOO BBBBBBBBB OOO   OOO LLL       222  222   * ELELCBML
00012 *  CCC      OOO   OOO BBB   BBB OOO   OOO LLL        222  222   * ELELCBML
00013 * CCC      OOO   OOO BBB   BBB OOO   OOO LLL         222  222   * ELELCBML
00014 * CCCCCCCC OOOOOOOO BBBBBBBBB OOOOOOOOO LLLLLLLLL   2222222222  * ELELCBML
00015                                                                   ELELCBML
00016 /*****************************************************************ELELCBML
00017 *         *** U P D A T E   H I S T O R Y ***                     ELELCBML
00018 *                                                                 ELELCBML
00019 *   DATE     BY    COMMENTS                                       ELELCBML
00020 *                                                                 ELELCBML
00021 *  09/85     NAC   CREATED                                        ELELCBML
00022 *                                                                 ELELCBML
00023 *  12/02/87  AKK   ADDED CODE TO ENABLE A USER TO USE ANY FORMAT  ELELCBML
00024 *                  WHEN SEEKING A DATA ELEMENT BY NUMBER.  ALSO   ELELCBML
00025 *                  ADDED CODE TO ALLOW A USER TO LOCATE AN ELEMENTELELCBML
00026 *                  BY INPUTING A PARTIAL DATA NAME, 6 OR LESS     ELELCBML
00027 *                  CHARACTERS.                                    ELELCBML
00028 *                                                                 ELELCBML
00029 *  12/90     RKH   COBOL II CONVERSION                            ELELCBML
00030 *                                                                 ELELCBML
00031 ***************************************************************** ELELCBML
00032 *            ELELCBML - ELS: SELECT DATA ELEMENT                  ELELCBML
00033 *                                                                 ELELCBML
00034 *    THIS MODULE IS RESPONSIBLE FOR LISTING ALL DATA ELEMENTS     ELELCBML
00035 *    WITHIN A GIVEN RECORD.  THE FOLLOWING FUNCTIONS COULD THEN   ELELCBML
00036 *    BE PERFORMED:                                                ELELCBML
00037 *                                                                 ELELCBML
00038 *    (1)  SELECT ACTUAL DATA ELEMENT FOR MAINTENCE BY SUBSEQUENT  ELELCBML
00039 *         SCREEN.  THIS CAN BE DONE BY 2 METHODS:                 ELELCBML
00040 *         (A)  USER MAY ENTER 'S' FOR FUNCTION CODE               ELELCBML
00041 *         (B)  USER MAY ENTER 'SELECT ELEMENT NUMBER';  THE       ELELCBML
00042 *              ELEMENT NUMBER MUST NOT BE IN DELETE STATUS AND    ELELCBML
00043 *              IT MUST EXIST ON FILE.                             ELELCBML
00044 *                                                                 ELELCBML
00045 *    (2)  UNDELETE DATA ELEMENT FROM DELETE STATUS.  THE STATUS   ELELCBML
00046 *         MUST BE IN DELETE STATUS ALREADY.  THE USER MAY ENTER   ELELCBML
00047 *         'U' FOR FUNCTION CODE.  THE MODULE WILL FIRST SEND      ELELCBML
00048 *         BACK THE SCREEN WITH A MESSAGE FOR THE USER TO HIT THE  ELELCBML
00049 *         PF6 KEY FOR CONFIRMATION AND THEN A SECOND SCREEN WILL  ELELCBML
00050 *         BE SENT TO VERIFY THAT ALL WENT WELL.                   ELELCBML
00051 *                                                                 ELELCBML
00052 *    (3)  PF3/PF9    TRANSFER CONTROL TO ELELCAML.                ELELCBML
00053 *                                                                 ELELCBML
00054 *    (4)  PF4 KEY    TRANSFER CONTROL TO ELELCCML; INDICATES A    ELELCBML
00055                      NEW ELEMENT WILL BE ADDED FOR THIS RECORD    ELELCBML
00056 *                                                                 ELELCBML
00057 *    (5)  PF7/PF8    BACKWARD/FORWARD POSITIONING.                ELELCBML
00058 *                                                                 ELELCBML
00059 *    (6)  CLEAR      RETURN TO CICS.                              ELELCBML
00060 *                                                                 ELELCBML
00061 ******************************************************************ELELCBML
00062 /                                                                 ELELCBML
00063  ENVIRONMENT DIVISION.                                            ELELCBML
00064  DATA DIVISION.                                                   ELELCBML
00065  WORKING-STORAGE SECTION.                                         ELELCBML
00066  77  FILLER                 PIC X(42)        VALUE                ELELCBML
00067      '***ELELCBML WORKING STORAGE BEGINS HERE***'.                ELELCBML
00068  01  WS-PARA-ID             PIC X(4)         VALUE 'XXXX'.        ELELCBML
00069  01  WS-ABEND-CODE          PIC X(4)         VALUE 'XXXX'.        ELELCBML
00070                                                                   ELELCBML
00071 ** FILE PARMS**                                                   ELELCBML
00072  01  COMM-LENGTH             PIC S9(4)   COMP VALUE +500.         ELELCBML
00073  01  RLEN.                                                        ELELCBML
00074      COPY ELCDRLEN.                                               ELELCBML
00075 /                                                                 ELELCBML
00076 ** SWITCHES **                                                    ELELCBML
00077  01  SCREEN-SW               PIC XXX   VALUE 'NO '.               ELELCBML
00078      88  SCREEN-BUILT                  VALUE 'YES'.               ELELCBML
00079  01  FUNC-SW                 PIC XXX   VALUE 'NO '.               ELELCBML
00080      88  FUNCTION-ENTERED              VALUE 'YES'.               ELELCBML
00081  01  ELEMENT-SW              PIC XXX   VALUE 'NO '.               ELELCBML
00082      88  ELEMENT-ENTERED               VALUE 'YES'.               ELELCBML
00083  01  NAME-SW                 PIC XXX   VALUE 'NO '.               ELELCBML
00084      88  NAME-ENTERED                  VALUE 'YES'.               ELELCBML
00085  01  REPOSITION-SW           PIC XXX   VALUE 'NO '.               ELELCBML
00086      88  REPOSITION-INDICATED          VALUE 'YES'.               ELELCBML
00087  01  ERROR-SWITCH            PIC XXX   VALUE 'NO '.               ELELCBML
00088      88  ERROR-DETECTED                VALUE 'YES'.               ELELCBML
00089                                                                   ELELCBML
00090 ** WORK AREAS**                                                   ELELCBML
00091  01  SUBA                   PIC 9.                                ELELCBML
00092  01  SUBB                   PIC 9.                                ELELCBML
00093  01  WS-REFORMAT-ELEMENT    PIC ZZ9.99.                           ELELCBML
00094  01  WS-REFORMAT-ELEMENT-X.                                       ELELCBML
00095      03  HIGH-3X.                                                 ELELCBML
00096          05  CHAR-1X        PIC X.                                ELELCBML
00097          05  CHAR-2X        PIC X.                                ELELCBML
00098          05  CHAR-3X        PIC X.                                ELELCBML
00099      03  FILLER.                                                  ELELCBML
00100          05  DECIMAL        PIC X.                                ELELCBML
00101      03  LOW-2X.                                                  ELELCBML
00102          05  CHAR-4X        PIC X.                                ELELCBML
00103          05  CHAR-5X        PIC X.                                ELELCBML
00104  01  WS-REFORMAT-ELEMENT-NAME REDEFINES WS-REFORMAT-ELEMENT-X     ELELCBML
00105                             PIC X(06).                            ELELCBML
00106  01  WS-REFORMAT-ELEMENT-N.                                       ELELCBML
00107      03  HIGH-3N.                                                 ELELCBML
00108          05  FIRST-NUM      PIC 9.                                ELELCBML
00109          05  SECOND-NUM     PIC 9.                                ELELCBML
00110          05  THIRD-NUM      PIC 9.                                ELELCBML
00111      03  LOW-2N.                                                  ELELCBML
00112          05  FOURTH-NUM     PIC 9.                                ELELCBML
00113          05  FIFTH-NUM      PIC 9.                                ELELCBML
00114  01  WS-ELEMENT-NBR  REDEFINES  WS-REFORMAT-ELEMENT-N             ELELCBML
00115                             PIC 999V99.                           ELELCBML
00116  01  WS-SAVE-ELEMENT-KEY    PIC S999V99  COMP-3 VALUE ZERO.       ELELCBML
00117  01  WS-SAVE-FUNCTION       PIC X               VALUE SPACE.      ELELCBML
00118      88  SELECT-INDICATED        VALUE 'S'.                       ELELCBML
00119      88  UNDELETE-INDICATED      VALUE 'U'.                       ELELCBML
00120  01  WS-SAVE-SUBA           PIC 9               VALUE ZERO.       ELELCBML
00121  01  WS-LOW-VALUES          PIC X               VALUE LOW-VALUES. ELELCBML
00122  01  HOLD-EN-PREFIX         PIC X(08).                            ELELCBML
00123  01  HOLD-DE-PREFIX         PIC X(08).                            ELELCBML
00124 /                                                                 ELELCBML
00125 ** MESSAGES **                                                    ELELCBML
00126  01  MAP-LITERAL1           PIC X(79)  VALUE                      ELELCBML
00127      '(PF3=RECORD LIST) (PF4=NEW ELEMENT) (PF7/8=BKWD/FWK) (PF9=REELELCBML
00128 -    'CLIST) (CLEAR=EXIT)'.                                       ELELCBML
00129  01  MAP-LITERAL2           PIC X(79)  VALUE                      ELELCBML
00130      'UNDELETE FUNCTION WAS SUCCESSFUL'.                          ELELCBML
00131                                                                   ELELCBML
00132  01  MAP-LITERAL3           PIC X(79)  VALUE                      ELELCBML
00133      'PLEASE ENTER PF6 TO CONFIRM UNDELETE'.                      ELELCBML
00134                                                                   ELELCBML
00135  01  MAP-LITERAL4           PIC X(79)  VALUE                      ELELCBML
00136      '(PF3=RECORD LIST) (PF7/8=BKWD/FWK) (PF9=RECORD LIST) (CLEAR ELELCBML
00137 -    '=EXIT)'.                                                    ELELCBML
00138                                                                   ELELCBML
00139  01  MAP-LITERAL5           PIC X(79)  VALUE                      ELELCBML
00140      'FCN IS (S)ELECT (U)NDELETE'.                                ELELCBML
00141                                                                   ELELCBML
00142  01  MAP-LITERAL6           PIC X(79)  VALUE                      ELELCBML
00143      'FCN IS (S)ELECT'.                                           ELELCBML
00144                                                                   ELELCBML
00145  01  ERR-01-MSG             PIC X(79)  VALUE                      ELELCBML
00146      'INVALID ELEMENT NUMBER/ DATA ELEMENT ENTERED'.              ELELCBML
00147                                                                   ELELCBML
00148  01  ERR-02-MSG             PIC X(79)  VALUE                      ELELCBML
00149      '                                     '.                     ELELCBML
00150                                                                   ELELCBML
00151  01  ERR-03-MSG.                                                  ELELCBML
00152      03  FILLER             PIC X(31)  VALUE SPACES.              ELELCBML
00153      03  FILLER             PIC X(16)  VALUE                      ELELCBML
00154      'END OF RETRIEVAL'.                                          ELELCBML
00155      03  FILLER             PIC X(32)  VALUE SPACES.              ELELCBML
00156                                                                   ELELCBML
00157  01  ERR-04-MSG.                                                  ELELCBML
00158      03  FILLER             PIC X(34)  VALUE SPACES.              ELELCBML
00159      03  FILLER             PIC X(10)  VALUE                      ELELCBML
00160      'FIRST PAGE'.                                                ELELCBML
00161      03  FILLER             PIC X(35)  VALUE SPACES.              ELELCBML
00162                                                                   ELELCBML
00163  01  ERR-05-MSG             PIC X(79)  VALUE                      ELELCBML
00164      'PLEASE INDICATE WHICH FUNCTION IS TO BE PERFORMED'.         ELELCBML
00165                                                                   ELELCBML
00166  01  ERR-06-MSG             PIC X(79)  VALUE                      ELELCBML
00167      'INVALID FUNCTION CODE ENTERED'.                             ELELCBML
00168                                                                   ELELCBML
00169  01  ERR-07-MSG             PIC X(79)  VALUE                      ELELCBML
00170      'SELECT NOT ALLOWED FOR ELEMENT IN DELETE STATUS'.           ELELCBML
00171                                                                   ELELCBML
00172  01  ERR-08-MSG             PIC X(79)  VALUE                      ELELCBML
00173      'UNDELETE ALLOWED FOR ELEMENT IN DELETE STATUS ONLY'.        ELELCBML
00174                                                                   ELELCBML
00175  01  ERR-09-MSG             PIC X(79)  VALUE                      ELELCBML
00176      'ENTER ONLY ONE FUNCTION CODE AT A TIME'.                    ELELCBML
00177                                                                   ELELCBML
00178  01  ERR-10-MSG             PIC X(79)  VALUE                      ELELCBML
00179      'ENTER FUNCTION CODE OR ELEMENT NUMBER - NOT BOTH'.          ELELCBML
00180                                                                   ELELCBML
00181  01  ERR-11-MSG             PIC X(79)  VALUE                      ELELCBML
00182      'DATA ELEMENT FOR FUNCTION ENTERED CAN NOT BE FOUND'.        ELELCBML
00183                                                                   ELELCBML
00184  01  ERR-12-MSG             PIC X(79)  VALUE                      ELELCBML
00185      'MAPFAIL - PLEASE NOTIFY ELS SYSTEMS GROUP'.                 ELELCBML
00186                                                                   ELELCBML
00187  01  ERR-13-MSG             PIC X(79)  VALUE                      ELELCBML
00188      'INQUIRY MODE ONLY - NO OTHER FUNCTIONS ALLOWED'.            ELELCBML
00189 /                                                                 ELELCBML
00190 ** MAP AREA **                                                    ELELCBML
00191  COPY ELCBSETC.                                                   ELELCBML
00192  01  FILLER  REDEFINES  ELCBI01I.                                 ELELCBML
00193      03  FILLER             PIC X(126).                           ELELCBML
00194      03  MAP-GRP  OCCURS  7 TIMES.                                ELELCBML
00195          05  FUNCL          PIC S9(4) COMP.                       ELELCBML
00196          05  FUNCA          PIC X.                                ELELCBML
00197          05  FUNC           PIC X.                                ELELCBML
00198          05  NAMEL          PIC S9(4) COMP.                       ELELCBML
00199          05  NAMEA          PIC X.                                ELELCBML
00200          05  NAME           PIC X(75).                            ELELCBML
00201          05  NUMBL          PIC S9(4) COMP.                       ELELCBML
00202          05  NUMBA          PIC X.                                ELELCBML
00203          05  NUMB           PIC X(6).                             ELELCBML
00204          05  FILLER         PIC X.                                ELELCBML
00205          05  STATL          PIC S9(4) COMP.                       ELELCBML
00206          05  STATA          PIC X.                                ELELCBML
00207          05  STAT           PIC X(7).                             ELELCBML
00208      03  FILLER             PIC X(174).                           ELELCBML
00209 /                                                                 ELELCBML
00210 ** ATTRIBUTES **                                                  ELELCBML
00211  COPY DFHBMSCA.                                                   ELELCBML
00212      02  DFHBMABF                PIC X VALUE 'Z'.                 ELELCBML
00213 /                                                                 ELELCBML
00214 ** ATTENTION IDENTIFIERS **                                       ELELCBML
00215  COPY DFHAID.                                                     ELELCBML
00216  01  FILLER                 PIC X(31)         VALUE               ELELCBML
00217      '***WORKING STORAGE ENDS HERE***'.                           ELELCBML
00218 /     ------------------------------------------------------------ELELCBML
00219  LINKAGE SECTION.                                                 ELELCBML
00220  01  DFHCOMMAREA.                                                 ELELCBML
00221  COPY ELPCOMMC.                                                   ELELCBML
00222                                                                   ELELCBML
00223  01  CIA-PARMS-RECORD.                                            ELELCBML
00224      COPY ELCDCIA.                                                ELELCBML
00225 /                                                                 ELELCBML
00226  01  IOPARM-RECORD-LIST.                                          ELELCBML
00227      COPY ELCDIOPM.                                               ELELCBML
00228 /                                                                 ELELCBML
00229  01  EL-RECORD-LIST.                                              ELELCBML
00230      COPY ELPRLC.                                                 ELELCBML
00231 /                                                                 ELELCBML
00232  01  IOPARM-DATA-ELEMENT.                                         ELELCBML
00233      COPY ELCDIOP2.                                               ELELCBML
00234 /                                                                 ELELCBML
00235  01  EL-DATA-ELEMENT.                                             ELELCBML
00236      COPY ELPDEC                                                  ELELCBML
00237      REPLACING == OCCURS 1 TO 11 ==                               ELELCBML
00238             BY == OCCURS      11 ==                               ELELCBML
00239                == DEPENDING ON DE-NBR-DESC-LINES ==               ELELCBML
00240             BY ==                                ==.              ELELCBML
00241 /                                                                 ELELCBML
00242  01  IOPARM-CODE-VALUE.                                           ELELCBML
00243      COPY ELCDIOP3.                                               ELELCBML
00244 /                                                                 ELELCBML
00245  01  EL-CODE-VALUE.                                               ELELCBML
00246      COPY ELPCVC                                                  ELELCBML
00247      REPLACING == OCCURS 1 TO 12 TIMES ==                         ELELCBML
00248             BY == OCCURS      12 TIMES. ==                        ELELCBML
00249                == DEPENDING ON    ==                              ELELCBML
00250             BY ==                 ==                              ELELCBML
00251                == CV-NBR-VALUE-DESC-LINES. ==                     ELELCBML
00252             BY ==                          ==.                    ELELCBML
00253 /                                                                 ELELCBML
00254  01  IOPARM-ENGLISH-NAME.                                         ELELCBML
00255      COPY ELCDIOP4.                                               ELELCBML
00256 /                                                                 ELELCBML
00257  01  EL-ENGLISH-NAME.                                             ELELCBML
00258      COPY ELPENC.                                                 ELELCBML
00259 /                                                                 ELELCBML
00260  PROCEDURE DIVISION.                                              ELELCBML
00261      PERFORM 1000-HOUSEKEEPING THRU 1000-EXIT.                    ELELCBML
00262      PERFORM 2000-MAINLINE-PROCESSING THRU 2000-EXIT.             ELELCBML
00263  0000-RETURN.                                                     ELELCBML
00264      EXEC CICS RETURN                                             ELELCBML
00265      END-EXEC.                                                    ELELCBML
00266      GOBACK.                                                      ELELCBML
00267 /                                                                 ELELCBML
00268  1000-HOUSEKEEPING.                                               ELELCBML
00269      MOVE '1000' TO WS-PARA-ID.                                   ELELCBML
00270                                                                   ELELCBML
00271      IF CA-SELECT-DE                                              ELELCBML
00272          EXEC CICS GETMAIN                                        ELELCBML
00273                    SET(ADDRESS OF CIA-PARMS-RECORD)               ELELCBML
00274                    INITIMG(WS-LOW-VALUES)                         ELELCBML
00275                    LENGTH(EL-CIA-REC-REC-LEN)                     ELELCBML
00276          END-EXEC                                                 ELELCBML
00277          SET CA-CIA-POINTER TO                                    ELELCBML
00278              ADDRESS OF  CIA-PARMS-RECORD                         ELELCBML
00279      ELSE                                                         ELELCBML
00280          SET ADDRESS OF  CIA-PARMS-RECORD TO                      ELELCBML
00281              CA-CIA-POINTER.                                      ELELCBML
00282                                                                   ELELCBML
00283      MOVE LOW-VALUES TO ELCBI01I.                                 ELELCBML
00284                                                                   ELELCBML
00285      IF EIBCALEN EQUAL ZEROES                                     ELELCBML
00286          MOVE 'EB00' TO WS-ABEND-CODE                             ELELCBML
00287          GO TO 9999-ABEND.                                        ELELCBML
00288                                                                   ELELCBML
00289      EXEC CICS HANDLE AID                                         ELELCBML
00290                CLEAR(0000-RETURN)                                 ELELCBML
00291                PF3  (9000-XCTL-ELELCAML)                          ELELCBML
00292                PF4  (9010-XCTL-ELELCCML)                          ELELCBML
00293                PF7  (5000-BACKWARD-READ)                          ELELCBML
00294                PF8  (6000-FORWARD-READ)                           ELELCBML
00295                PF9  (9000-XCTL-ELELCAML)                          ELELCBML
00296      END-EXEC.                                                    ELELCBML
00297                                                                   ELELCBML
00298      EXEC CICS HANDLE AID                                         ELELCBML
00299                PF15 (9000-XCTL-ELELCAML)                          ELELCBML
00300                PF16 (9010-XCTL-ELELCCML)                          ELELCBML
00301                PF19 (5000-BACKWARD-READ)                          ELELCBML
00302                PF20 (6000-FORWARD-READ)                           ELELCBML
00303                PF21 (9000-XCTL-ELELCAML)                          ELELCBML
00304      END-EXEC.                                                    ELELCBML
00305      EXEC CICS HANDLE CONDITION                                   ELELCBML
00306                MAPFAIL(9999-MAPFAIL)                              ELELCBML
00307      END-EXEC.                                                    ELELCBML
00308                                                                   ELELCBML
00309  1000-EXIT.  EXIT.                                                ELELCBML
00310 /                                                                 ELELCBML
00311  1100-GET-STORAGE-FOR-ELPRL.                                      ELELCBML
00312                                                                   ELELCBML
00313 *---->  GETMAIN FOR RECORD LIST IOPARM                            ELELCBML
00314      MOVE '1100' TO WS-PARA-ID.                                   ELELCBML
00315      EXEC CICS GETMAIN                                            ELELCBML
00316                SET(ADDRESS OF IOPARM-RECORD-LIST)                 ELELCBML
00317                LENGTH(EL-IOPARMS-REC-LEN)                         ELELCBML
00318                INITIMG(WS-LOW-VALUES)                             ELELCBML
00319      END-EXEC.                                                    ELELCBML
00320                                                                   ELELCBML
00321      SET  CIA-ELPRL-IOPARM-AREA-PNTR     TO                       ELELCBML
00322           ADDRESS OF IOPARM-RECORD-LIST.                          ELELCBML
00323                                                                   ELELCBML
00324 *---->  GETMAIN FOR RECORD LIST                                   ELELCBML
00325                                                                   ELELCBML
00326      EXEC CICS GETMAIN                                            ELELCBML
00327                SET(ADDRESS OF EL-RECORD-LIST)                     ELELCBML
00328                LENGTH(EL-ELPRL-REC-LEN)                           ELELCBML
00329                INITIMG(WS-LOW-VALUES)                             ELELCBML
00330      END-EXEC.                                                    ELELCBML
00331                                                                   ELELCBML
00332      SET  CIA-ELPRL-REC-AREA-PNTR    TO                           ELELCBML
00333           ADDRESS OF EL-RECORD-LIST.                              ELELCBML
00334                                                                   ELELCBML
00335      MOVE EL-DSN-ELPRL TO CIA-IO-GETMAIN-DDNAME.                  ELELCBML
00336                                                                   ELELCBML
00337  1100-EXIT.  EXIT.                                                ELELCBML
00338                                                                   ELELCBML
00339  1150-SET-ADDRESS-OF-ELPRL.                                       ELELCBML
00340                                                                   ELELCBML
00341      SET  ADDRESS OF EL-RECORD-LIST   TO                          ELELCBML
00342           ELCIO-REC-AREA-ADDRESS.                                 ELELCBML
00343                                                                   ELELCBML
00344  1150-EXIT.  EXIT.                                                ELELCBML
00345 /                                                                 ELELCBML
00346  1200-GET-STORAGE-FOR-ELPDE.                                      ELELCBML
00347                                                                   ELELCBML
00348 *---->  GETMAIN FOR DATA ELEMENT IOPARM                           ELELCBML
00349      MOVE '1200' TO WS-PARA-ID.                                   ELELCBML
00350                                                                   ELELCBML
00351      EXEC CICS GETMAIN                                            ELELCBML
00352                SET(ADDRESS OF IOPARM-DATA-ELEMENT)                ELELCBML
00353                LENGTH(EL-IOPARMS-REC-LEN)                         ELELCBML
00354                INITIMG(WS-LOW-VALUES)                             ELELCBML
00355      END-EXEC.                                                    ELELCBML
00356                                                                   ELELCBML
00357      SET CIA-ELPDE-IOPARM-AREA-PNTR   TO                          ELELCBML
00358          ADDRESS OF IOPARM-DATA-ELEMENT.                          ELELCBML
00359                                                                   ELELCBML
00360 *---->  GETMAIN FOR DATA ELEMENT                                  ELELCBML
00361      EXEC CICS GETMAIN                                            ELELCBML
00362                SET(ADDRESS OF EL-DATA-ELEMENT)                    ELELCBML
00363                LENGTH(EL-ELPDE-REC-LEN)                           ELELCBML
00364                INITIMG(WS-LOW-VALUES)                             ELELCBML
00365      END-EXEC.                                                    ELELCBML
00366                                                                   ELELCBML
00367      SET  CIA-ELPDE-REC-AREA-PNTR     TO                          ELELCBML
00368           ADDRESS OF EL-DATA-ELEMENT.                             ELELCBML
00369                                                                   ELELCBML
00370      MOVE EL-DSN-ELPDE TO CIA-IO-GETMAIN-DDNAME.                  ELELCBML
00371                                                                   ELELCBML
00372  1200-EXIT.  EXIT.                                                ELELCBML
00373                                                                   ELELCBML
00374  1250-SET-ADDRESS-OF-ELPDE.                                       ELELCBML
00375                                                                   ELELCBML
00376 *    SET ADDRESS OF IO PARM AREA FOR DATA ELEMENT                 ELELCBML
00377                                                                   ELELCBML
00378      SET ADDRESS OF IOPARM-DATA-ELEMENT TO                        ELELCBML
00379          CIA-ELPDE-IOPARM-AREA-PNTR.                              ELELCBML
00380                                                                   ELELCBML
00381      SET  CIA-IO-PARM-AREA-PNTR      TO                           ELELCBML
00382           CIA-ELPDE-IOPARM-AREA-PNTR.                             ELELCBML
00383                                                                   ELELCBML
00384 *    SET ADDRESS OF DATA ELEMENT RECORD                           ELELCBML
00385                                                                   ELELCBML
00386      SET ADDRESS OF EL-DATA-ELEMENT TO                            ELELCBML
00387           CIA-ELPDE-REC-AREA-PNTR.                                ELELCBML
00388                                                                   ELELCBML
00389  1250-EXIT.  EXIT.                                                ELELCBML
00390 /                                                                 ELELCBML
00391  1300-GET-STORAGE-FOR-ELPCV.                                      ELELCBML
00392                                                                   ELELCBML
00393 *---->  GETMAIN FOR CODES VALUE  IOPARM                           ELELCBML
00394      MOVE '1300' TO WS-PARA-ID.                                   ELELCBML
00395      EXEC CICS GETMAIN                                            ELELCBML
00396                SET(ADDRESS OF IOPARM-CODE-VALUE)                  ELELCBML
00397                LENGTH(EL-IOPARMS-REC-LEN)                         ELELCBML
00398                INITIMG(WS-LOW-VALUES)                             ELELCBML
00399      END-EXEC.                                                    ELELCBML
00400                                                                   ELELCBML
00401      SET CIA-ELPCV-IOPARM-AREA-PNTR   TO                          ELELCBML
00402            ADDRESS  OF  IOPARM-CODE-VALUE.                        ELELCBML
00403                                                                   ELELCBML
00404 *---->  GETMAIN FOR CODES VALUE                                   ELELCBML
00405                                                                   ELELCBML
00406      EXEC CICS GETMAIN                                            ELELCBML
00407                SET(ADDRESS OF EL-CODE-VALUE)                      ELELCBML
00408                LENGTH(EL-ELPCV-REC-LEN)                           ELELCBML
00409                INITIMG(WS-LOW-VALUES)                             ELELCBML
00410      END-EXEC.                                                    ELELCBML
00411                                                                   ELELCBML
00412      SET  CIA-ELPCV-REC-AREA-PNTR  TO                             ELELCBML
00413          ADDRESS OF EL-CODE-VALUE.                                ELELCBML
00414                                                                   ELELCBML
00415      MOVE EL-DSN-ELPCV TO CIA-IO-GETMAIN-DDNAME.                  ELELCBML
00416                                                                   ELELCBML
00417  1300-EXIT.  EXIT.                                                ELELCBML
00418                                                                   ELELCBML
00419  1350-SET-ADDRESS-OF-ELPCV.                                       ELELCBML
00420      MOVE '1350' TO WS-PARA-ID.                                   ELELCBML
00421                                                                   ELELCBML
00422 *    SET ADDRESS OF IO PARM AREA FOR CODES VALUE                  ELELCBML
00423                                                                   ELELCBML
00424      SET ADDRESS OF IOPARM-CODE-VALUE   TO                        ELELCBML
00425          CIA-ELPCV-IOPARM-AREA-PNTR.                              ELELCBML
00426                                                                   ELELCBML
00427      SET  CIA-IO-PARM-AREA-PNTR      TO                           ELELCBML
00428           CIA-ELPCV-IOPARM-AREA-PNTR.                             ELELCBML
00429                                                                   ELELCBML
00430 *    SET ADDRESS OF DATA ELEMENT RECORD                           ELELCBML
00431                                                                   ELELCBML
00432      SET ADDRESS OF EL-DATA-ELEMENT TO                            ELELCBML
00433           CIA-ELPCV-REC-AREA-PNTR.                                ELELCBML
00434                                                                   ELELCBML
00435  1350-EXIT.  EXIT.                                                ELELCBML
00436 /                                                                 ELELCBML
00437  1400-GET-STORAGE-FOR-ELPEN.                                      ELELCBML
00438                                                                   ELELCBML
00439 *---->  GETMAIN FOR ENGLISH NAME IOPARM                           ELELCBML
00440      MOVE '1400'       TO WS-PARA-ID.                             ELELCBML
00441      EXEC CICS GETMAIN                                            ELELCBML
00442                SET(ADDRESS OF IOPARM-ENGLISH-NAME)                ELELCBML
00443                LENGTH(EL-IOPARMS-REC-LEN)                         ELELCBML
00444                INITIMG(WS-LOW-VALUES)                             ELELCBML
00445      END-EXEC.                                                    ELELCBML
00446                                                                   ELELCBML
00447      SET CIA-ELPEN-IOPARM-AREA-PNTR    TO                         ELELCBML
00448          ADDRESS  OF  IOPARM-ENGLISH-NAME.                        ELELCBML
00449                                                                   ELELCBML
00450 *---->  GETMAIN FOR ENGLISH NAME RECORD                           ELELCBML
00451                                                                   ELELCBML
00452      EXEC CICS GETMAIN                                            ELELCBML
00453                SET(ADDRESS OF EL-ENGLISH-NAME)                    ELELCBML
00454                LENGTH(EL-ELPEN-REC-LEN)                           ELELCBML
00455                INITIMG(WS-LOW-VALUES)                             ELELCBML
00456      END-EXEC.                                                    ELELCBML
00457                                                                   ELELCBML
00458      SET CIA-ELPEN-REC-AREA-PNTR     TO                           ELELCBML
00459          ADDRESS OF EL-ENGLISH-NAME.                              ELELCBML
00460                                                                   ELELCBML
00461      MOVE EL-DSN-ELPEN TO CIA-IO-GETMAIN-DDNAME.                  ELELCBML
00462                                                                   ELELCBML
00463  1400-EXIT.  EXIT.                                                ELELCBML
00464 /                                                                 ELELCBML
00465  1450-SET-ADDRESS-OF-ELPEN.                                       ELELCBML
00466                                                                   ELELCBML
00467      MOVE '1450' TO WS-PARA-ID.                                   ELELCBML
00468 *    SET ADDRESS OF IO PARM AREA FOR ENGLISH NAME                 ELELCBML
00469                                                                   ELELCBML
00470      SET ADDRESS OF IOPARM-ENGLISH-NAME TO                        ELELCBML
00471          CIA-ELPEN-IOPARM-AREA-PNTR.                              ELELCBML
00472                                                                   ELELCBML
00473      SET  CIA-IO-PARM-AREA-PNTR      TO                           ELELCBML
00474           CIA-ELPEN-IOPARM-AREA-PNTR.                             ELELCBML
00475                                                                   ELELCBML
00476 *    SET ADDRESS OF DATA ELEMENT RECORD                           ELELCBML
00477                                                                   ELELCBML
00478      SET ADDRESS OF EL-ENGLISH-NAME  TO                           ELELCBML
00479           CIA-ELPEN-REC-AREA-PNTR.                                ELELCBML
00480                                                                   ELELCBML
00481  1450-EXIT.  EXIT.                                                ELELCBML
00482 /                                                                 ELELCBML
00483  2000-MAINLINE-PROCESSING.                                        ELELCBML
00484      MOVE '2000' TO WS-PARA-ID.                                   ELELCBML
00485                                                                   ELELCBML
00486 ** DETERMINE WHERE PROGRAM CONTROL CAME FROM; THEN PROCESS        ELELCBML
00487 ** THE APPROPRIATE ROUTINE.                                       ELELCBML
00488                                                                   ELELCBML
00489      IF CA-RECORD-LIST                                            ELELCBML
00490         PERFORM 4000-CREATE-SCREEN THRU 4000-EXIT                 ELELCBML
00491      ELSE                                                         ELELCBML
00492        IF CA-DE-DEFINE                                            ELELCBML
00493            IF CA-FIRST-ELEMENT NOT EQUAL LOW-VALUES               ELELCBML
00494                PERFORM 4000-CREATE-SCREEN THRU 4000-EXIT          ELELCBML
00495            ELSE                                                   ELELCBML
00496                GO TO 9000-XCTL-ELELCAML                           ELELCBML
00497        ELSE                                                       ELELCBML
00498          IF CA-SELECT-DE                                          ELELCBML
00499              PERFORM 3000-RECEIVE-SCREEN THRU 3000-EXIT           ELELCBML
00500          ELSE                                                     ELELCBML
00501              MOVE 'EB01' TO WS-ABEND-CODE                         ELELCBML
00502              GO TO 9999-ABEND.                                    ELELCBML
00503                                                                   ELELCBML
00504  2000-EXIT.  EXIT.                                                ELELCBML
00505 /                                                                 ELELCBML
00506  3000-RECEIVE-SCREEN.                                             ELELCBML
00507      MOVE '3000' TO WS-PARA-ID.                                   ELELCBML
00508                                                                   ELELCBML
00509      EXEC CICS RECEIVE MAP('ELCBI01')                             ELELCBML
00510                MAPSET     ('ELCBSET')                             ELELCBML
00511      END-EXEC.                                                    ELELCBML
00512                                                                   ELELCBML
00513      PERFORM 1100-GET-STORAGE-FOR-ELPRL THRU 1100-EXIT.           ELELCBML
00514      PERFORM 1200-GET-STORAGE-FOR-ELPDE THRU 1200-EXIT.           ELELCBML
00515      PERFORM 1300-GET-STORAGE-FOR-ELPCV THRU 1300-EXIT.           ELELCBML
00516      PERFORM 1400-GET-STORAGE-FOR-ELPEN THRU 1400-EXIT.           ELELCBML
00517                                                                   ELELCBML
00518      IF EIBAID EQUAL DFHENTER  OR                                 ELELCBML
00519                      DFHPF6  OR  DFHPF18                          ELELCBML
00520          NEXT SENTENCE                                            ELELCBML
00521      ELSE                                                         ELELCBML
00522          MOVE ERR-05-MSG TO BERRMO                                ELELCBML
00523          MOVE -1 TO BFCN1L                                        ELELCBML
00524          GO TO   9999-RETURN-WITH-MSG.                            ELELCBML
00525                                                                   ELELCBML
00526      MOVE SPACES TO BERRMO.                                       ELELCBML
00527                                                                   ELELCBML
00528      IF BFUNCI NOT EQUAL 'ELCB'                                   ELELCBML
00529          GO TO 9000-XCTL-ELELCAML.                                ELELCBML
00530                                                                   ELELCBML
00531      MOVE '3100' TO WS-PARA-ID.                                   ELELCBML
00532      PERFORM 3100-EDIT-SCREEN THRU 3100-EXIT                      ELELCBML
00533          VARYING SUBA FROM 1 BY 1                                 ELELCBML
00534          UNTIL SUBA GREATER THAN 7.                               ELELCBML
00535                                                                   ELELCBML
00536      MOVE '3000' TO WS-PARA-ID.                                   ELELCBML
00537                                                                   ELELCBML
00538 **  EDIT SECURITY LEVEL **                                        ELELCBML
00539                                                                   ELELCBML
00540      IF UNDELETE-INDICATED  AND  CA-INQUIRY                       ELELCBML
00541          MOVE -1 TO FUNCL (WS-SAVE-SUBA)                          ELELCBML
00542          MOVE DFHBMUBF TO FUNCA (WS-SAVE-SUBA)                    ELELCBML
00543          MOVE ERR-13-MSG    TO BERRMO                             ELELCBML
00544          GO TO 9999-RETURN-WITH-MSG.                              ELELCBML
00545                                                                   ELELCBML
00546 ** EDIT SELECT ELEMENT NUMBER  OR ELEMENT NAME             **     ELELCBML
00547 ** CODE CAN NOW ACCEPT ALPHA CHARACTERS OF ELELMENT NUMBER **     ELELCBML
00548      IF BSCODL GREATER THAN ZERO                                  ELELCBML
00549          IF BSCODI NOT NUMERIC                                    ELELCBML
00550             MOVE SPACES TO WS-REFORMAT-ELEMENT-X                  ELELCBML
00551             MOVE BSCODI TO WS-REFORMAT-ELEMENT-X                  ELELCBML
00552             IF CHAR-1X IS ALPHABETIC                              ELELCBML
00553                IF CHAR-2X IS EQUAL SPACES AND CHAR-3X IS EQUAL    ELELCBML
00554                   SPACES                                          ELELCBML
00555                       MOVE 'YES' TO NAME-SW                       ELELCBML
00556                  ELSE                                             ELELCBML
00557                      IF CHAR-3X IS EQUAL SPACE AND DECIMAL IS     ELELCBML
00558                           EQUAL SPACE                             ELELCBML
00559                           MOVE 'YES' TO NAME-SW                   ELELCBML
00560                      ELSE                                         ELELCBML
00561                         IF DECIMAL IS EQUAL SPACE AND CHAR-4X     ELELCBML
00562                            IS EQUAL SPACE                         ELELCBML
00563                               MOVE 'YES' TO NAME-SW               ELELCBML
00564                         ELSE                                      ELELCBML
00565                             IF CHAR-4X IS EQUAL SPACE AND CHAR-5X ELELCBML
00566                                IS EQUAL SPACE                     ELELCBML
00567                                  MOVE 'YES' TO NAME-SW            ELELCBML
00568                             ELSE                                  ELELCBML
00569                                 MOVE 'YES' TO NAME-SW             ELELCBML
00570             ELSE                                                  ELELCBML
00571                IF CHAR-1X IS EQUAL '.' AND CHAR-2X NUMERIC        ELELCBML
00572                   AND CHAR-3X NUMERIC                             ELELCBML
00573                   MOVE CHAR-2X TO CHAR-4X                         ELELCBML
00574                   MOVE CHAR-3X TO CHAR-5X                         ELELCBML
00575                   MOVE 0 TO HIGH-3N                               ELELCBML
00576                   MOVE LOW-2X TO LOW-2N                           ELELCBML
00577                   MOVE WS-ELEMENT-NBR TO CA-SEL-ELEMENT-NBR       ELELCBML
00578                ELSE                                               ELELCBML
00579                   IF CHAR-1X IS NUMERIC                           ELELCBML
00580                      PERFORM 3025-CONTINUE-NUMERIC-EDIT THRU      ELELCBML
00581                              3025-EXIT                            ELELCBML
00582                   ELSE                                            ELELCBML
00583                      MOVE -1 TO BSCODL                            ELELCBML
00584                      MOVE DFHBMUBF TO BSCODA                      ELELCBML
00585                      MOVE ERR-01-MSG TO BERRMO                    ELELCBML
00586                      GO TO 9999-RETURN-WITH-MSG.                  ELELCBML
00587                                                                   ELELCBML
00588      IF FUNCTION-ENTERED                                          ELELCBML
00589          MOVE WS-SAVE-ELEMENT-KEY TO CA-SEL-ELEMENT-NBR.          ELELCBML
00590                                                                   ELELCBML
00591 ** CHECK IF PF6/PF18 WAS ENTERED FOR CONFIRMATION **              ELELCBML
00592                                                                   ELELCBML
00593      IF EIBAID EQUAL DFHPF6  OR  DFHPF18                          ELELCBML
00594         IF CA-SEL-ELEMENT-NBR-X NOT EQUAL LOW-VALUES  AND         ELELCBML
00595            CA-SEL-ELEMENT-NBR EQUAL WS-SAVE-ELEMENT-KEY AND       ELELCBML
00596            CA-CURRENT-FUNCTION EQUAL WS-SAVE-FUNCTION   AND       ELELCBML
00597            UNDELETE-INDICATED                           AND       ELELCBML
00598            NOT ELEMENT-ENTERED                                    ELELCBML
00599             PERFORM 3200-UNDELETE-FUNCTION THRU 3200-EXIT         ELELCBML
00600         ELSE                                                      ELELCBML
00601             MOVE -1 TO FUNCL (1)                                  ELELCBML
00602             MOVE ERR-05-MSG TO BERRMO                             ELELCBML
00603             GO TO 9999-RETURN-WITH-MSG.                           ELELCBML
00604                                                                   ELELCBML
00605 ** SEEK ELEMENT BASED ON PARTIAL KEY ENTERED IN CODE FIELD **     ELELCBML
00606                                                                   ELELCBML
00607      IF NAME-ENTERED                                              ELELCBML
00608         PERFORM 3010-BROWSE-ELPEN-FILE THRU 3010-EXIT             ELELCBML
00609         IF ELCIO-REC-NOT-FOUND4 OR (ELCIO-GOOD-RETURN4            ELELCBML
00610            AND EN-RECORD-PREFIX NOT = HOLD-EN-PREFIX)             ELELCBML
00611            PERFORM 3075-READ-PREV-ELPEN-RECORD THRU 3075-EXIT     ELELCBML
00612                UNTIL EN-RECORD-PREFIX = HOLD-EN-PREFIX            ELELCBML
00613            MOVE EN-RECORD-PREFIX TO CA-SEL-RECORD-PREFIX          ELELCBML
00614            MOVE EN-ELEMENT-NBR TO CA-SEL-ELEMENT-NBR              ELELCBML
00615            MOVE 'EB' TO ELCIO-FILE-ACCESS-CODE4                   ELELCBML
00616            EXEC CICS LINK                                         ELELCBML
00617                PROGRAM ('ELAIOPGM')                               ELELCBML
00618                COMMAREA (ADDRESS OF CIA-PARMS-RECORD)             ELELCBML
00619                LENGTH (EL-CIA-POINTER-LEN)                        ELELCBML
00620            END-EXEC                                               ELELCBML
00621         ELSE                                                      ELELCBML
00622            MOVE EN-RECORD-PREFIX TO CA-SEL-RECORD-PREFIX          ELELCBML
00623            MOVE EN-ELEMENT-NBR TO CA-SEL-ELEMENT-NBR              ELELCBML
00624            MOVE 'EB' TO ELCIO-FILE-ACCESS-CODE4                   ELELCBML
00625            EXEC CICS LINK                                         ELELCBML
00626                PROGRAM ('ELAIOPGM')                               ELELCBML
00627                COMMAREA (ADDRESS OF CIA-PARMS-RECORD)             ELELCBML
00628                LENGTH (EL-CIA-POINTER-LEN)                        ELELCBML
00629            END-EXEC.                                              ELELCBML
00630                                                                   ELELCBML
00631                                                                   ELELCBML
00632 ** VERIFY THAT ELEMENT SELECTED EXISTS ON FILE **                 ELELCBML
00633                                                                   ELELCBML
00634      IF FUNCTION-ENTERED  OR                                      ELELCBML
00635         ELEMENT-ENTERED OR NAME-ENTERED                           ELELCBML
00636          PERFORM 1200-GET-STORAGE-FOR-ELPDE THRU 1200-EXIT        ELELCBML
00637          MOVE EL-DSN-ELPDE       TO ELCIO-FILE-DDNAME2            ELELCBML
00638          MOVE EL-ELPDE-REC-LEN   TO ELCIO-MAX-REC-LEN2            ELELCBML
00639          MOVE 'EQ '              TO ELCIO-CIO-QUAL2               ELELCBML
00640          MOVE 11                 TO ELCIO-BROWSE-KEYLEN2          ELELCBML
00641          MOVE 'M'                TO ELCIO-STORAGE2                ELELCBML
00642          MOVE 'RD '              TO ELCIO-FILE-ACCESS-CODE2       ELELCBML
00643          MOVE CA-SELECTED-DE-KEY TO ELCIO-VSAM-KEY2               ELELCBML
00644          SET   ELCIO-REC-AREA-ADDRESS2   TO                       ELELCBML
00645                CIA-ELPDE-REC-AREA-PNTR                            ELELCBML
00646          SET   CIA-IO-PARM-AREA-PNTR     TO                       ELELCBML
00647                CIA-ELPDE-IOPARM-AREA-PNTR                         ELELCBML
00648          EXEC CICS LINK                                           ELELCBML
00649                    PROGRAM('ELAIOPGM')                            ELELCBML
00650                    COMMAREA(ADDRESS OF CIA-PARMS-RECORD)          ELELCBML
00651                    LENGTH(EL-CIA-POINTER-LEN)                     ELELCBML
00652          END-EXEC                                                 ELELCBML
00653 ***                                                               ELELCBML
00654          IF ELCIO-REC-NOT-FOUND2                                  ELELCBML
00655              GO TO 9999-NOTFND                                    ELELCBML
00656          ELSE                                                     ELELCBML
00657              IF NOT ELCIO-GOOD-RETURN2                            ELELCBML
00658                  MOVE 'EB02' TO WS-ABEND-CODE                     ELELCBML
00659                  GO TO 9999-ABEND.                                ELELCBML
00660                                                                   ELELCBML
00661                                                                   ELELCBML
00662      IF FUNCTION-ENTERED                                          ELELCBML
00663          IF SELECT-INDICATED                                      ELELCBML
00664              MOVE DE-ELEMENT-NAME TO CA-SEL-ELEMENT-NAME          ELELCBML
00665              GO TO 9010-XCTL-ELELCCML                             ELELCBML
00666          ELSE                                                     ELELCBML
00667              MOVE DE-ELEMENT-NAME TO CA-SEL-ELEMENT-NAME          ELELCBML
00668              MOVE 'U' TO CA-CURRENT-FUNCTION                      ELELCBML
00669              MOVE -1 TO FUNCL (WS-SAVE-SUBA)                      ELELCBML
00670              MOVE DFHBMUBF TO FUNCA (WS-SAVE-SUBA)                ELELCBML
00671              MOVE MAP-LITERAL3  TO BERRMO                         ELELCBML
00672              GO TO 9999-RETURN-WITH-MSG.                          ELELCBML
00673                                                                   ELELCBML
00674      IF ELEMENT-ENTERED                                           ELELCBML
00675          IF DE-DELETE                                             ELELCBML
00676              MOVE DE-ELEMENT-NBR TO CA-FE-ELEMENT-NBR             ELELCBML
00677              MOVE 'YES' TO REPOSITION-SW                          ELELCBML
00678              PERFORM 4000-CREATE-SCREEN THRU 4000-EXIT            ELELCBML
00679          ELSE                                                     ELELCBML
00680              MOVE DE-ELEMENT-NAME TO CA-SEL-ELEMENT-NAME          ELELCBML
00681              GO TO 9010-XCTL-ELELCCML.                            ELELCBML
00682                                                                   ELELCBML
00683      IF NAME-ENTERED                                              ELELCBML
00684          MOVE 'B' TO CA-CURRENT-PGM                               ELELCBML
00685          PERFORM 6000-FORWARD-READ THRU 6000-EXIT.                ELELCBML
00686                                                                   ELELCBML
00687 **  AT THIS POINT NO FUNCTION WAS INDICATED, SO SCROLL FORWARD ** ELELCBML
00688       GO TO 6000-FORWARD-READ.                                    ELELCBML
00689                                                                   ELELCBML
00690  3000-EXIT.  EXIT.                                                ELELCBML
00691 /                                                                 ELELCBML
00692  3010-BROWSE-ELPEN-FILE.                                          ELELCBML
00693         MOVE SPACES TO HOLD-EN-PREFIX.                            ELELCBML
00694         MOVE BRPREXI TO CA-SEL-RECORD-PREFIX                      ELELCBML
00695                       EN-RECORD-PREFIX                            ELELCBML
00696                       HOLD-EN-PREFIX.                             ELELCBML
00697         MOVE BSCODI               TO CA-SEL-ELEMENT-NAME          ELELCBML
00698                                      EN-ELEMENT-NAME.             ELELCBML
00699         MOVE 'SB'                 TO ELCIO-FILE-ACCESS-CODE4.     ELELCBML
00700         MOVE 'GTE'                TO ELCIO-CIO-QUAL4.             ELELCBML
00701         MOVE 83                   TO ELCIO-BROWSE-KEYLEN4.        ELELCBML
00702         MOVE 'M'                  TO ELCIO-STORAGE4.              ELELCBML
00703         MOVE EN-KEY               TO ELCIO-VSAM-KEY4.             ELELCBML
00704         MOVE EL-ELPEN-REC-LEN     TO ELCIO-MAX-REC-LEN4.          ELELCBML
00705         MOVE EL-DSN-ELPEN         TO ELCIO-FILE-DDNAME4           ELELCBML
00706                                      CIA-IO-GETMAIN-DDNAME.       ELELCBML
00707         SET  ELCIO-REC-AREA-ADDRESS4  TO                          ELELCBML
00708              CIA-ELPEN-REC-AREA-PNTR.                             ELELCBML
00709                                                                   ELELCBML
00710         SET   CIA-IO-PARM-AREA-PNTR     TO                        ELELCBML
00711               CIA-ELPEN-IOPARM-AREA-PNTR.                         ELELCBML
00712                                                                   ELELCBML
00713         EXEC CICS LINK                                            ELELCBML
00714             PROGRAM ('ELAIOPGM')                                  ELELCBML
00715             COMMAREA (ADDRESS OF CIA-PARMS-RECORD)                ELELCBML
00716             LENGTH (EL-CIA-POINTER-LEN)                           ELELCBML
00717         END-EXEC.                                                 ELELCBML
00718  3010-EXIT.     EXIT.                                             ELELCBML
00719 /                                                                 ELELCBML
00720  3025-CONTINUE-NUMERIC-EDIT.                                      ELELCBML
00721      IF CHAR-2X NUMERIC AND CHAR-3X NUMERIC                       ELELCBML
00722         MOVE HIGH-3X TO HIGH-3N                                   ELELCBML
00723      ELSE                                                         ELELCBML
00724         IF CHAR-2X NUMERIC AND CHAR-3X NOT NUMERIC                ELELCBML
00725            MOVE 0 TO FIRST-NUM                                    ELELCBML
00726            MOVE CHAR-1X TO SECOND-NUM                             ELELCBML
00727            MOVE CHAR-2X TO THIRD-NUM                              ELELCBML
00728        ELSE                                                       ELELCBML
00729           IF CHAR-2X NOT NUMERIC AND CHAR-3X NOT NUMERIC          ELELCBML
00730              MOVE 0 TO FIRST-NUM                                  ELELCBML
00731                        SECOND-NUM                                 ELELCBML
00732              MOVE CHAR-1X TO THIRD-NUM                            ELELCBML
00733           ELSE                                                    ELELCBML
00734              IF CHAR-2X EQUAL '.' AND CHAR-3X IS NUMERIC          ELELCBML
00735                  MOVE 0 TO FIRST-NUM                              ELELCBML
00736                            SECOND-NUM                             ELELCBML
00737                  MOVE CHAR-1X TO THIRD-NUM                        ELELCBML
00738                  MOVE CHAR-4X TO FOURTH-NUM                       ELELCBML
00739              ELSE                                                 ELELCBML
00740                  MOVE -1 TO BSCODL                                ELELCBML
00741                  MOVE DFHBMUBF TO BSCODA                          ELELCBML
00742                  MOVE ERR-01-MSG TO BERRMO                        ELELCBML
00743                  GO TO 9999-RETURN-WITH-MSG.                      ELELCBML
00744       PERFORM 3050-FINAL-NUMERIC-EDIT THRU 3050-EXIT.             ELELCBML
00745  3025-EXIT.     EXIT.                                             ELELCBML
00746 /                                                                 ELELCBML
00747  3050-FINAL-NUMERIC-EDIT.                                         ELELCBML
00748      IF CHAR-4X NUMERIC AND CHAR-5X NUMERIC                       ELELCBML
00749          MOVE LOW-2X TO LOW-2N                                    ELELCBML
00750      ELSE                                                         ELELCBML
00751         IF CHAR-4X NUMERIC AND CHAR-5X NOT NUMERIC                ELELCBML
00752            MOVE 0 TO CHAR-5X                                      ELELCBML
00753            MOVE LOW-2X TO LOW-2N                                  ELELCBML
00754         ELSE                                                      ELELCBML
00755            IF CHAR-4X NOT NUMERIC AND CHAR-5X NOT NUMERIC         ELELCBML
00756                MOVE 0 TO CHAR-4X                                  ELELCBML
00757                          CHAR-5X                                  ELELCBML
00758                MOVE LOW-2X TO LOW-2N                              ELELCBML
00759           ELSE                                                    ELELCBML
00760                 MOVE -1 TO BSCODL                                 ELELCBML
00761                 MOVE DFHBMUBF TO BSCODA                           ELELCBML
00762                 MOVE ERR-01-MSG TO BERRMO                         ELELCBML
00763                 GO TO 9999-RETURN-WITH-MSG.                       ELELCBML
00764         MOVE 'YES' TO ELEMENT-SW.                                 ELELCBML
00765         MOVE WS-ELEMENT-NBR TO CA-SEL-ELEMENT-NBR.                ELELCBML
00766  3050-EXIT.     EXIT.                                             ELELCBML
00767 /                                                                 ELELCBML
00768  3075-READ-PREV-ELPEN-RECORD.                                     ELELCBML
00769      MOVE 'RP' TO ELCIO-FILE-ACCESS-CODE4.                        ELELCBML
00770      PERFORM 3090-SET-ELPEN-PARMS THRU 3090-EXIT.                 ELELCBML
00771      IF ELCIO-REC-NOT-FOUND4                                      ELELCBML
00772          PERFORM 3080-READ-NEXT-ELPEN-RECORD THRU 3080-EXIT.      ELELCBML
00773  3075-EXIT.     EXIT.                                             ELELCBML
00774                                                                   ELELCBML
00775  3080-READ-NEXT-ELPEN-RECORD.                                     ELELCBML
00776      MOVE 'RN' TO ELCIO-FILE-ACCESS-CODE4.                        ELELCBML
00777      PERFORM 3090-SET-ELPEN-PARMS THRU 3090-EXIT.                 ELELCBML
00778  3080-EXIT.     EXIT.                                             ELELCBML
00779 /                                                                 ELELCBML
00780  3090-SET-ELPEN-PARMS.                                            ELELCBML
00781      MOVE EN-RECORD-PREFIX        TO CA-SEL-RECORD-PREFIX.        ELELCBML
00782      MOVE EN-ELEMENT-NBR          TO CA-SEL-ELEMENT-NBR.          ELELCBML
00783      MOVE EN-KEY                  TO ELCIO-VSAM-KEY4.             ELELCBML
00784      MOVE 83                      TO ELCIO-BROWSE-KEYLEN4.        ELELCBML
00785      MOVE EL-ELPEN-REC-LEN        TO ELCIO-MAX-REC-LEN4.          ELELCBML
00786      MOVE EL-DSN-ELPEN            TO ELCIO-FILE-DDNAME4           ELELCBML
00787                                      CIA-IO-GETMAIN-DDNAME.       ELELCBML
00788      MOVE 'M'                     TO ELCIO-STORAGE4.              ELELCBML
00789                                                                   ELELCBML
00790      SET  ELCIO-REC-AREA-ADDRESS4 TO CIA-ELPEN-REC-AREA-PNTR.     ELELCBML
00791      SET  CIA-IO-PARM-AREA-PNTR   TO CIA-ELPEN-IOPARM-AREA-PNTR.  ELELCBML
00792                                                                   ELELCBML
00793      EXEC CICS LINK                                               ELELCBML
00794           PROGRAM('ELAIOPGM')                                     ELELCBML
00795           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCBML
00796           LENGTH(EL-CIA-POINTER-LEN)                              ELELCBML
00797      END-EXEC.                                                    ELELCBML
00798  3090-EXIT.     EXIT.                                             ELELCBML
00799 /                                                                 ELELCBML
00800  3100-EDIT-SCREEN.                                                ELELCBML
00801                                                                   ELELCBML
00802 **  IGNORE SPACE IF ENTERED FOR FUNCTION CODE  **                 ELELCBML
00803                                                                   ELELCBML
00804      IF FUNCL (SUBA) GREATER THAN ZERO  AND                       ELELCBML
00805         FUNC  (SUBA) EQUAL SPACE                                  ELELCBML
00806             GO TO 3100-EXIT.                                      ELELCBML
00807                                                                   ELELCBML
00808      IF FUNCL (SUBA) GREATER THAN ZERO                            ELELCBML
00809          IF FUNC (SUBA) NOT EQUAL 'S' AND 'U'                     ELELCBML
00810              MOVE -1 TO FUNCL (SUBA)                              ELELCBML
00811              MOVE DFHBMUBF TO FUNCA (SUBA)                        ELELCBML
00812              MOVE ERR-06-MSG TO BERRMO                            ELELCBML
00813              GO TO 9999-RETURN-WITH-MSG                           ELELCBML
00814          ELSE                                                     ELELCBML
00815              IF FUNC (SUBA) EQUAL 'S'                             ELELCBML
00816                  IF STAT (SUBA) EQUAL 'DELETED'                   ELELCBML
00817                      MOVE -1 TO FUNCL (SUBA)                      ELELCBML
00818                      MOVE ERR-07-MSG TO BERRMO                    ELELCBML
00819                      MOVE DFHBMUBF TO FUNCA (SUBA)                ELELCBML
00820                      GO TO 9999-RETURN-WITH-MSG                   ELELCBML
00821                  ELSE                                             ELELCBML
00822                      NEXT SENTENCE                                ELELCBML
00823              ELSE                                                 ELELCBML
00824                  IF STAT (SUBA) EQUAL SPACES                      ELELCBML
00825                      MOVE -1 TO FUNCL (SUBA)                      ELELCBML
00826                      MOVE ERR-08-MSG TO BERRMO                    ELELCBML
00827                      MOVE DFHBMUBF TO FUNCA (SUBA)                ELELCBML
00828                      GO TO 9999-RETURN-WITH-MSG.                  ELELCBML
00829                                                                   ELELCBML
00830      IF FUNCL (SUBA) GREATER THAN ZERO                            ELELCBML
00831          IF FUNCTION-ENTERED                                      ELELCBML
00832              MOVE -1 TO FUNCL (SUBA)                              ELELCBML
00833              MOVE ERR-09-MSG TO BERRMO                            ELELCBML
00834              MOVE DFHBMUBF TO FUNCA (SUBA)                        ELELCBML
00835              GO TO 9999-RETURN-WITH-MSG                           ELELCBML
00836          ELSE                                                     ELELCBML
00837              MOVE 'YES' TO FUNC-SW                                ELELCBML
00838              IF BSCODL GREATER THAN ZERO                          ELELCBML
00839                  MOVE -1 TO FUNCL (SUBA)                          ELELCBML
00840                  MOVE ERR-09-MSG TO BERRMO                        ELELCBML
00841                  MOVE DFHBMUBF TO FUNCA (SUBA)                    ELELCBML
00842                  GO TO 9999-RETURN-WITH-MSG.                      ELELCBML
00843                                                                   ELELCBML
00844      IF FUNCL (SUBA) GREATER THAN ZERO                            ELELCBML
00845          MOVE FUNC (SUBA) TO WS-SAVE-FUNCTION                     ELELCBML
00846          MOVE SUBA  TO WS-SAVE-SUBA                               ELELCBML
00847          MOVE NUMB (SUBA) TO WS-REFORMAT-ELEMENT-X                ELELCBML
00848          INSPECT HIGH-3X REPLACING ALL SPACES BY ZEROES           ELELCBML
00849          MOVE HIGH-3X     TO HIGH-3N                              ELELCBML
00850          MOVE LOW-2X      TO LOW-2N                               ELELCBML
00851          MOVE WS-ELEMENT-NBR TO WS-SAVE-ELEMENT-KEY.              ELELCBML
00852                                                                   ELELCBML
00853  3100-EXIT.  EXIT.                                                ELELCBML
00854 /                                                                 ELELCBML
00855  3200-UNDELETE-FUNCTION.                                          ELELCBML
00856      MOVE '3200' TO WS-PARA-ID.                                   ELELCBML
00857                                                                   ELELCBML
00858 **  THIS PROCESS BRINGS IN THE DATA ELEMENT RECORD, UPDATES       ELELCBML
00859 **  THE FLAG AND REWRITES IT.                                     ELELCBML
00860                                                                   ELELCBML
00861      MOVE EL-DSN-ELPDE             TO ELCIO-FILE-DDNAME2.         ELELCBML
00862      MOVE EL-DSN-ELPDE             TO CIA-IO-GETMAIN-DDNAME.      ELELCBML
00863      MOVE CA-SELECTED-DE-KEY       TO ELCIO-VSAM-KEY2.            ELELCBML
00864      MOVE 'RU '                    TO ELCIO-FILE-ACCESS-CODE2.    ELELCBML
00865      MOVE EL-ELPDE-REC-LEN         TO ELCIO-MAX-REC-LEN2.         ELELCBML
00866      MOVE 'M'                      TO ELCIO-STORAGE2.             ELELCBML
00867                                                                   ELELCBML
00868      SET  ELCIO-REC-AREA-ADDRESS2 TO                              ELELCBML
00869                                CIA-ELPDE-REC-AREA-PNTR.           ELELCBML
00870      SET  CIA-IO-PARM-AREA-PNTR   TO                              ELELCBML
00871                                CIA-ELPDE-IOPARM-AREA-PNTR.        ELELCBML
00872                                                                   ELELCBML
00873      EXEC CICS LINK                                               ELELCBML
00874                PROGRAM('ELAIOPGM')                                ELELCBML
00875                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCBML
00876                LENGTH(EL-CIA-POINTER-LEN)                         ELELCBML
00877      END-EXEC.                                                    ELELCBML
00878                                                                   ELELCBML
00879      IF NOT ELCIO-GOOD-RETURN2                                    ELELCBML
00880          MOVE 'EB03' TO WS-ABEND-CODE                             ELELCBML
00881          GO TO 9999-ABEND.                                        ELELCBML
00882                                                                   ELELCBML
00883      MOVE SPACE TO DE-DELETE-ELEMENT-FLAG.                        ELELCBML
00884                                                                   ELELCBML
00885      MOVE 'WU '            TO ELCIO-FILE-ACCESS-CODE2.            ELELCBML
00886                                                                   ELELCBML
00887      EXEC CICS LINK                                               ELELCBML
00888                PROGRAM('ELAIOPGM')                                ELELCBML
00889                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCBML
00890                LENGTH(EL-CIA-POINTER-LEN)                         ELELCBML
00891      END-EXEC.                                                    ELELCBML
00892                                                                   ELELCBML
00893      IF NOT ELCIO-GOOD-RETURN2                                    ELELCBML
00894          MOVE 'EB04' TO WS-ABEND-CODE                             ELELCBML
00895          GO TO 9999-ABEND.                                        ELELCBML
00896                                                                   ELELCBML
00897                                                                   ELELCBML
00898 **  SEND A MESSAGE BACK TO THE USER WHICH WILL INDICATE THAT      ELELCBML
00899 **  ALL WENT WELL.                                                ELELCBML
00900                                                                   ELELCBML
00901      MOVE SPACE          TO CA-CURRENT-FUNCTION.                  ELELCBML
00902      MOVE ZEROES         TO CA-SEL-ELEMENT-NBR.                   ELELCBML
00903      MOVE MAP-LITERAL2   TO BERRMO.                               ELELCBML
00904      MOVE SPACES         TO STAT (WS-SAVE-SUBA).                  ELELCBML
00905      MOVE -1             TO FUNCL (1).                            ELELCBML
00906      MOVE SPACES         TO FUNC (WS-SAVE-SUBA).                  ELELCBML
00907      MOVE DFHBMUNP       TO FUNCA (WS-SAVE-SUBA).                 ELELCBML
00908      GO TO 9999-RETURN-WITH-MSG.                                  ELELCBML
00909  3200-EXIT.  EXIT.                                                ELELCBML
00910 /                                                                 ELELCBML
00911  4000-CREATE-SCREEN.                                              ELELCBML
00912                                                                   ELELCBML
00913      MOVE '4000' TO WS-PARA-ID.                                   ELELCBML
00914                                                                   ELELCBML
00915      PERFORM 1100-GET-STORAGE-FOR-ELPRL THRU 1100-EXIT.           ELELCBML
00916      PERFORM 1200-GET-STORAGE-FOR-ELPDE THRU 1200-EXIT.           ELELCBML
00917      PERFORM 1300-GET-STORAGE-FOR-ELPCV THRU 1300-EXIT.           ELELCBML
00918                                                                   ELELCBML
00919      MOVE '4000' TO WS-PARA-ID.                                   ELELCBML
00920                                                                   ELELCBML
00921 ** MOVE COMMON PORTION **                                         ELELCBML
00922      MOVE CA-SEL-RECORD-PREFIX          TO BRPREXO.               ELELCBML
00923      MOVE CA-SEL-RECORD-NAME            TO BRNAMEO.               ELELCBML
00924      MOVE CA-TRANS-HDR                  TO BTITLEO.               ELELCBML
00925                                                                   ELELCBML
00926      PERFORM 1250-SET-ADDRESS-OF-ELPDE THRU 1250-EXIT.            ELELCBML
00927                                                                   ELELCBML
00928      MOVE 'SB '            TO ELCIO-FILE-ACCESS-CODE2.            ELELCBML
00929      PERFORM 4100-BROWSE-DATA-ELEMENT-FILE THRU 4100-EXIT.        ELELCBML
00930                                                                   ELELCBML
00931      IF ELCIO-EOF-BROWSE2                                         ELELCBML
00932          NEXT SENTENCE                                            ELELCBML
00933      ELSE                                                         ELELCBML
00934          IF NOT ELCIO-GOOD-RETURN2                                ELELCBML
00935              MOVE 'EB05' TO WS-ABEND-CODE                         ELELCBML
00936              GO TO 9999-ABEND                                     ELELCBML
00937          ELSE                                                     ELELCBML
00938              MOVE 1 TO SUBA                                       ELELCBML
00939              PERFORM 4200-READNEXT-WITHIN-FILE THRU 4200-EXIT     ELELCBML
00940                      UNTIL SCREEN-BUILT.                          ELELCBML
00941                                                                   ELELCBML
00942      IF NOT SCREEN-BUILT                                          ELELCBML
00943          MOVE 'YES' TO SCREEN-SW                                  ELELCBML
00944          MOVE HIGH-VALUES TO CA-LE-ELEMENT-NBR-X                  ELELCBML
00945      ELSE                                                         ELELCBML
00946          IF NOT ELCIO-EOF-BROWSE2                                 ELELCBML
00947              PERFORM 9999-ENDBR THRU 9999-ENDBR-EXIT.             ELELCBML
00948                                                                   ELELCBML
00949      IF REPOSITION-INDICATED                                      ELELCBML
00950          MOVE ERR-07-MSG TO BERRMO                                ELELCBML
00951      ELSE                                                         ELELCBML
00952          IF CA-INQUIRY                                            ELELCBML
00953              MOVE MAP-LITERAL6 TO BLINEO                          ELELCBML
00954              MOVE MAP-LITERAL4 TO BERRMO                          ELELCBML
00955          ELSE                                                     ELELCBML
00956              MOVE MAP-LITERAL5 TO BLINEO                          ELELCBML
00957              MOVE MAP-LITERAL1 TO BERRMO.                         ELELCBML
00958                                                                   ELELCBML
00959 **  INITIALIZE THE BEGINNING SCROLL KEY REGARDLESS OF WHAT WAS    ELELCBML
00960 **  SET IN PARA. 4200 BECAUSE THIS IS THE FIRST SCREEN.           ELELCBML
00961      IF NOT CA-DE-DEFINE                                          ELELCBML
00962          MOVE LOW-VALUES TO CA-FE-ELEMENT-NBR-X.                  ELELCBML
00963                                                                   ELELCBML
00964      MOVE LOW-VALUES   TO CA-SEL-ELEMENT-NBR-X.                   ELELCBML
00965      MOVE 'B' TO CA-CURRENT-PGM.                                  ELELCBML
00966                                                                   ELELCBML
00967      EXEC CICS SEND MAP ('ELCBI01')                               ELELCBML
00968                MAPSET   ('ELCBSET')                               ELELCBML
00969                ERASE                                              ELELCBML
00970      END-EXEC.                                                    ELELCBML
00971                                                                   ELELCBML
00972      EXEC CICS RETURN                                             ELELCBML
00973                TRANSID('ELCB')                                    ELELCBML
00974                COMMAREA(DFHCOMMAREA)                              ELELCBML
00975                LENGTH(COMM-LENGTH)                                ELELCBML
00976      END-EXEC.                                                    ELELCBML
00977                                                                   ELELCBML
00978  4000-EXIT.  EXIT.                                                ELELCBML
00979 /                                                                 ELELCBML
00980  4100-BROWSE-DATA-ELEMENT-FILE.                                   ELELCBML
00981      MOVE '4100' TO WS-PARA-ID.                                   ELELCBML
00982                                                                   ELELCBML
00983 **  DETERMINE WHERE TO START THE BROWSE, DEPENDING WHERE          ELELCBML
00984 **  CONTROL CAME FROM.                                            ELELCBML
00985                                                                   ELELCBML
00986      IF CA-RECORD-LIST                                            ELELCBML
00987          MOVE LOW-VALUES         TO CA-SEL-ELEMENT-NBR-X          ELELCBML
00988          MOVE CA-SELECTED-DE-KEY TO ELCIO-VSAM-KEY2               ELELCBML
00989      ELSE                                                         ELELCBML
00990          IF CA-DE-DEFINE  OR  REPOSITION-INDICATED                ELELCBML
00991              MOVE CA-FIRST-ELEMENT TO ELCIO-VSAM-KEY2             ELELCBML
00992          ELSE                                                     ELELCBML
00993              IF EIBAID EQUAL DFHPF8  OR  DFHPF20  OR  DFHENTER    ELELCBML
00994                 AND NOT NAME-ENTERED                              ELELCBML
00995                  MOVE CA-LAST-ELEMENT TO ELCIO-VSAM-KEY2          ELELCBML
00996              ELSE                                                 ELELCBML
00997                  IF EIBAID EQUAL DFHPF7  OR  DFHPF19              ELELCBML
00998                      MOVE CA-FIRST-ELEMENT TO ELCIO-VSAM-KEY2     ELELCBML
00999                      MOVE CA-SEL-RECORD-PREFIX TO HOLD-DE-PREFIX  ELELCBML
01000                  ELSE                                             ELELCBML
01001                     IF CA-SELECT-DE AND NAME-ENTERED              ELELCBML
01002                        MOVE SPACES TO HOLD-DE-PREFIX              ELELCBML
01003                        MOVE CA-SEL-RECORD-PREFIX TO               ELELCBML
01004                           DE-RECORD-PREFIX                        ELELCBML
01005                           HOLD-DE-PREFIX                          ELELCBML
01006                        MOVE CA-SEL-ELEMENT-NBR TO                 ELELCBML
01007                           DE-ELEMENT-NBR                          ELELCBML
01008                        MOVE DE-PRIMARY-KEY TO                     ELELCBML
01009                           ELCIO-VSAM-KEY2.                        ELELCBML
01010 **ADDED ABOVE IF 12/11/87 ****                                    ELELCBML
01011                                                                   ELELCBML
01012      MOVE EL-DSN-ELPDE             TO ELCIO-FILE-DDNAME2.         ELELCBML
01013      MOVE EL-ELPDE-REC-LEN         TO ELCIO-MAX-REC-LEN2.         ELELCBML
01014      MOVE 'GTE'                    TO ELCIO-CIO-QUAL2.            ELELCBML
01015      MOVE 11                       TO ELCIO-BROWSE-KEYLEN2.       ELELCBML
01016      MOVE 'M'                      TO ELCIO-STORAGE2.             ELELCBML
01017      SET  ELCIO-REC-AREA-ADDRESS2  TO                             ELELCBML
01018           CIA-ELPDE-REC-AREA-PNTR.                                ELELCBML
01019      SET  CIA-IO-PARM-AREA-PNTR   TO CIA-ELPDE-IOPARM-AREA-PNTR.  ELELCBML
01020                                                                   ELELCBML
01021      EXEC CICS LINK                                               ELELCBML
01022                PROGRAM('ELAIOPGM')                                ELELCBML
01023                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCBML
01024                LENGTH(EL-CIA-POINTER-LEN)                         ELELCBML
01025      END-EXEC.                                                    ELELCBML
01026                                                                   ELELCBML
01027  4100-EXIT.  EXIT.                                                ELELCBML
01028 /                                                                 ELELCBML
01029  4200-READNEXT-WITHIN-FILE.                                       ELELCBML
01030      MOVE '4200' TO WS-PARA-ID.                                   ELELCBML
01031                                                                   ELELCBML
01032 **  SUBA REPRESENTS THE OCCURRANCES OF DATA ELEMENTS ON THE       ELELCBML
01033 **  SCREEN.  THE MAXIMUM IS 7.                                    ELELCBML
01034                                                                   ELELCBML
01035      IF SUBA EQUAL 1                                              ELELCBML
01036          IF DE-RECORD-PREFIX EQUAL HIGH-VALUES OR                 ELELCBML
01037             DE-RECORD-PREFIX NOT EQUAL CA-SEL-RECORD-PREFIX       ELELCBML
01038              MOVE 'YES' TO SCREEN-SW                              ELELCBML
01039              PERFORM 9999-ENDBR THRU 9999-ENDBR-EXIT              ELELCBML
01040              GO TO 9999-ENDFILE.                                  ELELCBML
01041                                                                   ELELCBML
01042      IF DE-RECORD-PREFIX NOT EQUAL CA-SEL-RECORD-PREFIX           ELELCBML
01043          MOVE 'YES' TO SCREEN-SW                                  ELELCBML
01044          MOVE HIGH-VALUES TO CA-LE-ELEMENT-NBR-X                  ELELCBML
01045          GO TO 4200-EXIT.                                         ELELCBML
01046                                                                   ELELCBML
01047      IF SUBA EQUAL 7                                              ELELCBML
01048          MOVE 'YES' TO SCREEN-SW                                  ELELCBML
01049          MOVE DE-PRIMARY-KEY TO CA-LAST-ELEMENT                   ELELCBML
01050          PERFORM 4400-MOVE-INFO-TO-SCREEN THRU 4400-EXIT          ELELCBML
01051          GO TO 4200-EXIT                                          ELELCBML
01052      ELSE                                                         ELELCBML
01053          IF SUBA EQUAL 1                                          ELELCBML
01054              MOVE DE-PRIMARY-KEY TO CA-FIRST-ELEMENT              ELELCBML
01055              MOVE SPACES     TO BSCODO                            ELELCBML
01056              MOVE DFHBMUNP   TO BSCODA                            ELELCBML
01057              PERFORM 4500-REINITIALIZE-SCREEN THRU 4500-EXIT      ELELCBML
01058                  VARYING SUBB FROM 1 BY 1                         ELELCBML
01059                  UNTIL SUBB GREATER THAN 7.                       ELELCBML
01060                                                                   ELELCBML
01061                                                                   ELELCBML
01062      PERFORM 4400-MOVE-INFO-TO-SCREEN THRU 4400-EXIT.             ELELCBML
01063                                                                   ELELCBML
01064      EXEC CICS LINK                                               ELELCBML
01065                PROGRAM('ELAIOPGM')                                ELELCBML
01066                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCBML
01067                LENGTH(EL-CIA-POINTER-LEN)                         ELELCBML
01068      END-EXEC.                                                    ELELCBML
01069                                                                   ELELCBML
01070      IF ELCIO-EOF-BROWSE2                                         ELELCBML
01071          MOVE 'YES' TO SCREEN-SW                                  ELELCBML
01072          MOVE HIGH-VALUES TO CA-LE-ELEMENT-NBR-X                  ELELCBML
01073          GO TO 4200-EXIT                                          ELELCBML
01074      ELSE                                                         ELELCBML
01075          IF NOT ELCIO-GOOD-RETURN2                                ELELCBML
01076              MOVE 'EB06' TO WS-ABEND-CODE                         ELELCBML
01077              GO TO 9999-ABEND.                                    ELELCBML
01078                                                                   ELELCBML
01079      ADD 1 TO SUBA.                                               ELELCBML
01080                                                                   ELELCBML
01081  4200-EXIT.  EXIT.                                                ELELCBML
01082 /                                                                 ELELCBML
01083  4300-READPREV-WITHIN-FILE.                                       ELELCBML
01084      MOVE '4300' TO WS-PARA-ID.                                   ELELCBML
01085                                                                   ELELCBML
01086 **  SUBA REPRESENTS THE OCCURRANCES OF DATA ELEMENT LINES         ELELCBML
01087 **  ON THE SCREEN.  BECAUSE THE ROUTINE IS READING BACKWARDS,     ELELCBML
01088 **  SUBA WAS INITIALIZED TO 8 AND WILL BE SUBTRACTED BY 1.        ELELCBML
01089                                                                   ELELCBML
01090      EXEC CICS LINK                                               ELELCBML
01091                PROGRAM('ELAIOPGM')                                ELELCBML
01092                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCBML
01093                LENGTH(EL-CIA-POINTER-LEN)                         ELELCBML
01094      END-EXEC                                                     ELELCBML
01095                                                                   ELELCBML
01096      IF ELCIO-EOF-BROWSE2                                         ELELCBML
01097          PERFORM 9999-ENDBR THRU 9999-ENDBR-EXIT                  ELELCBML
01098          GO TO 9999-ENDFILE                                       ELELCBML
01099      ELSE                                                         ELELCBML
01100          IF NOT ELCIO-GOOD-RETURN2                                ELELCBML
01101              MOVE 'EB07' TO WS-ABEND-CODE                         ELELCBML
01102              GO TO 9999-ABEND.                                    ELELCBML
01103                                                                   ELELCBML
01104      IF SUBA EQUAL 8                                              ELELCBML
01105          IF DE-RECORD-PREFIX EQUAL LOW-VALUES  OR                 ELELCBML
01106             DE-RECORD-PREFIX NOT EQUAL CA-SEL-RECORD-PREFIX       ELELCBML
01107              MOVE 'YES' TO SCREEN-SW                              ELELCBML
01108              PERFORM 9999-ENDBR THRU 9999-ENDBR-EXIT              ELELCBML
01109              GO TO 9999-ENDFILE.                                  ELELCBML
01110                                                                   ELELCBML
01111      IF DE-RECORD-PREFIX NOT EQUAL CA-SEL-RECORD-PREFIX           ELELCBML
01112          MOVE 'YES' TO SCREEN-SW                                  ELELCBML
01113          MOVE LOW-VALUES TO CA-FE-ELEMENT-NBR-X                   ELELCBML
01114          GO TO 4300-EXIT                                          ELELCBML
01115      ELSE                                                         ELELCBML
01116          IF DE-PRIMARY-KEY EQUAL CA-FIRST-ELEMENT                 ELELCBML
01117              GO TO 4300-EXIT.                                     ELELCBML
01118                                                                   ELELCBML
01119      SUBTRACT 1 FROM SUBA.                                        ELELCBML
01120                                                                   ELELCBML
01121      IF SUBA EQUAL 1                                              ELELCBML
01122          MOVE 'YES' TO SCREEN-SW                                  ELELCBML
01123          MOVE DE-PRIMARY-KEY TO CA-FIRST-ELEMENT                  ELELCBML
01124      ELSE                                                         ELELCBML
01125          IF SUBA EQUAL 7                                          ELELCBML
01126              MOVE DE-PRIMARY-KEY TO CA-LAST-ELEMENT               ELELCBML
01127              MOVE SPACES     TO BSCODO                            ELELCBML
01128              MOVE DFHBMUNP   TO BSCODA                            ELELCBML
01129              PERFORM 4500-REINITIALIZE-SCREEN THRU 4500-EXIT      ELELCBML
01130                  VARYING SUBB FROM 1 BY 1                         ELELCBML
01131                  UNTIL SUBB GREATER THAN 7.                       ELELCBML
01132                                                                   ELELCBML
01133                                                                   ELELCBML
01134                                                                   ELELCBML
01135      PERFORM 4400-MOVE-INFO-TO-SCREEN THRU 4400-EXIT.             ELELCBML
01136                                                                   ELELCBML
01137  4300-EXIT.  EXIT.                                                ELELCBML
01138 /                                                                 ELELCBML
01139  4325-FIND-CORRECT-ELPDE-REC.                                     ELELCBML
01140      IF ELCIO-REC-NOT-FOUND2 OR (ELCIO-GOOD-RETURN2               ELELCBML
01141           AND DE-RECORD-PREFIX NOT EQUAL HOLD-DE-PREFIX)          ELELCBML
01142             PERFORM 4350-READ-NEXT-ELPDE THRU 4350-EXIT           ELELCBML
01143               UNTIL DE-RECORD-PREFIX EQUAL HOLD-DE-PREFIX         ELELCBML
01144                MOVE DE-RECORD-PREFIX TO CA-SEL-RECORD-PREFIX      ELELCBML
01145                MOVE DE-ELEMENT-NBR TO CA-SEL-ELEMENT-NBR          ELELCBML
01146                MOVE 1 TO SUBA                                     ELELCBML
01147                MOVE 'NO' TO SCREEN-SW                             ELELCBML
01148                PERFORM 4200-READNEXT-WITHIN-FILE THRU 4200-EXIT   ELELCBML
01149                   UNTIL SCREEN-BUILT.                             ELELCBML
01150  4325-EXIT.     EXIT.                                             ELELCBML
01151 /                                                                 ELELCBML
01152  4350-READ-NEXT-ELPDE.                                            ELELCBML
01153      MOVE 'NO' TO SCREEN-SW.                                      ELELCBML
01154      MOVE 1 TO SUBA.                                              ELELCBML
01155      MOVE CA-SEL-RECORD-PREFIX TO HOLD-DE-PREFIX.                 ELELCBML
01156      MOVE 'RN' TO ELCIO-FILE-ACCESS-CODE2.                        ELELCBML
01157      EXEC CICS LINK                                               ELELCBML
01158         PROGRAM ('ELAIOPGM')                                      ELELCBML
01159         COMMAREA (ADDRESS OF CIA-PARMS-RECORD)                    ELELCBML
01160         LENGTH (EL-CIA-POINTER-LEN)                               ELELCBML
01161      END-EXEC.                                                    ELELCBML
01162  4350-EXIT.     EXIT.                                             ELELCBML
01163 /                                                                 ELELCBML
01164  4400-MOVE-INFO-TO-SCREEN.                                        ELELCBML
01165      MOVE '4400' TO WS-PARA-ID.                                   ELELCBML
01166                                                                   ELELCBML
01167      MOVE DE-ELEMENT-NAME      TO NAME    (SUBA).                 ELELCBML
01168      MOVE DE-ELEMENT-NBR       TO WS-REFORMAT-ELEMENT.            ELELCBML
01169 *    MOVE WS-ELEMENT-NBR       TO WS-REFORMAT-ELEMENT.            ELELCBML
01170      MOVE WS-REFORMAT-ELEMENT  TO NUMB    (SUBA).                 ELELCBML
01171      IF DE-DELETE                                                 ELELCBML
01172          MOVE 'DELETED'        TO STAT    (SUBA)                  ELELCBML
01173      ELSE                                                         ELELCBML
01174          MOVE SPACES           TO STAT    (SUBA).                 ELELCBML
01175  4400-EXIT.  EXIT.                                                ELELCBML
01176 /                                                                 ELELCBML
01177  4500-REINITIALIZE-SCREEN.                                        ELELCBML
01178      MOVE SPACES     TO FUNC   (SUBB)                             ELELCBML
01179                         NAME   (SUBB)                             ELELCBML
01180                         NUMB   (SUBB)                             ELELCBML
01181                         STAT   (SUBB).                            ELELCBML
01182      MOVE DFHBMUNP   TO FUNCA  (SUBB).                            ELELCBML
01183  4500-EXIT.  EXIT.                                                ELELCBML
01184 /                                                                 ELELCBML
01185  4600-ENDBR.                                                      ELELCBML
01186      MOVE '4600' TO WS-PARA-ID.                                   ELELCBML
01187                                                                   ELELCBML
01188      IF NOT ELCIO-EOF-BROWSE2                                     ELELCBML
01189          PERFORM 9999-ENDBR THRU 9999-ENDBR-EXIT.                 ELELCBML
01190                                                                   ELELCBML
01191      IF CA-INQUIRY                                                ELELCBML
01192          MOVE MAP-LITERAL6 TO BLINEO                              ELELCBML
01193          MOVE MAP-LITERAL4 TO BERRMO                              ELELCBML
01194      ELSE                                                         ELELCBML
01195          MOVE MAP-LITERAL5 TO BLINEO                              ELELCBML
01196          MOVE MAP-LITERAL1 TO BERRMO.                             ELELCBML
01197                                                                   ELELCBML
01198      MOVE LOW-VALUES   TO CA-SEL-ELEMENT-NBR-X.                   ELELCBML
01199      MOVE 'B' TO CA-CURRENT-PGM.                                  ELELCBML
01200                                                                   ELELCBML
01201      EXEC CICS SEND MAP ('ELCBI01')                               ELELCBML
01202                MAPSET   ('ELCBSET')                               ELELCBML
01203                ERASE                                              ELELCBML
01204      END-EXEC.                                                    ELELCBML
01205                                                                   ELELCBML
01206      EXEC CICS RETURN                                             ELELCBML
01207                TRANSID('ELCB')                                    ELELCBML
01208                COMMAREA(DFHCOMMAREA)                              ELELCBML
01209                LENGTH(COMM-LENGTH)                                ELELCBML
01210      END-EXEC.                                                    ELELCBML
01211  4600-EXIT.  EXIT.                                                ELELCBML
01212 /                                                                 ELELCBML
01213  5000-BACKWARD-READ.                                              ELELCBML
01214      MOVE '5000' TO WS-PARA-ID.                                   ELELCBML
01215                                                                   ELELCBML
01216                                                                   ELELCBML
01217      PERFORM 1100-GET-STORAGE-FOR-ELPRL THRU 1100-EXIT.           ELELCBML
01218      PERFORM 1200-GET-STORAGE-FOR-ELPDE THRU 1200-EXIT.           ELELCBML
01219      PERFORM 1300-GET-STORAGE-FOR-ELPCV THRU 1300-EXIT.           ELELCBML
01220                                                                   ELELCBML
01221      PERFORM 1250-SET-ADDRESS-OF-ELPDE THRU 1250-EXIT.            ELELCBML
01222                                                                   ELELCBML
01223      IF CA-FE-ELEMENT-NBR-X EQUAL LOW-VALUES                      ELELCBML
01224          GO TO 9999-ENDFILE.                                      ELELCBML
01225                                                                   ELELCBML
01226                                                                   ELELCBML
01227      MOVE 'SBP'            TO ELCIO-FILE-ACCESS-CODE2.            ELELCBML
01228      PERFORM 4100-BROWSE-DATA-ELEMENT-FILE THRU 4100-EXIT.        ELELCBML
01229                                                                   ELELCBML
01230      IF ELCIO-EOF-BROWSE2                                         ELELCBML
01231          GO TO 9999-ENDFILE                                       ELELCBML
01232      ELSE                                                         ELELCBML
01233          IF NOT ELCIO-GOOD-RETURN2                                ELELCBML
01234              MOVE 'EB08' TO WS-ABEND-CODE                         ELELCBML
01235              GO TO 9999-ABEND.                                    ELELCBML
01236                                                                   ELELCBML
01237      IF DE-PRIMARY-KEY NOT EQUAL CA-FIRST-ELEMENT                 ELELCBML
01238          PERFORM 9999-ENDBR THRU 9999-ENDBR-EXIT                  ELELCBML
01239          GO TO 9999-ENDFILE.                                      ELELCBML
01240                                                                   ELELCBML
01241                                                                   ELELCBML
01242      MOVE 8 TO SUBA.                                              ELELCBML
01243      MOVE '4300' TO WS-PARA-ID.                                   ELELCBML
01244      PERFORM 4300-READPREV-WITHIN-FILE THRU 4300-EXIT             ELELCBML
01245          UNTIL SCREEN-BUILT.                                      ELELCBML
01246                                                                   ELELCBML
01247 ***THE FOLLOWING HAS BEEN ADDED TO ADJUST THE 'FIRST PAGE'.****   ELELCBML
01248 ***WHEN A NAME HAS BEEN USED TO LOCATE AN ELEMENT AND THEN ****   ELELCBML
01249 ***PF7 USED TO PAGE BACKWARD THE NUMBER OF ELEMENTS        ****   ELELCBML
01250 ***MAY NOT BE DIVISIBLE BY 7, LEAVING BLANK LINES AT THE   ****   ELELCBML
01251 ***TOP OF THE PAGE- THIS CODE CORRECTS THIS SITUTATION     ****   ELELCBML
01252                                                                   ELELCBML
01253      IF (DE-RECORD-PREFIX NOT EQUAL CA-SEL-RECORD-PREFIX)         ELELCBML
01254          AND NAME (1) EQUAL SPACES OR LOW-VALUES                  ELELCBML
01255             IF ELCIO-FILE-ACCESS-CODE2 EQUAL 'SBP'                ELELCBML
01256                PERFORM 9999-ENDBR THRU 9999-ENDBR-EXIT            ELELCBML
01257                PERFORM 5050-START-ELPDE-BROWSE THRU 5050-EXIT     ELELCBML
01258                PERFORM 4325-FIND-CORRECT-ELPDE-REC THRU 4325-EXIT ELELCBML
01259              ELSE                                                 ELELCBML
01260                PERFORM 4325-FIND-CORRECT-ELPDE-REC THRU 4325-EXIT ELELCBML
01261      ELSE                                                         ELELCBML
01262         IF NAME (1) EQUAL SPACES OR LOW-VALUES                    ELELCBML
01263             IF ELCIO-FILE-ACCESS-CODE2 EQUAL 'SBP'                ELELCBML
01264                PERFORM 9999-ENDBR THRU 9999-ENDBR-EXIT            ELELCBML
01265                PERFORM 5050-START-ELPDE-BROWSE THRU 5050-EXIT     ELELCBML
01266                MOVE 1 TO SUBA                                     ELELCBML
01267                MOVE 'NO' TO SCREEN-SW                             ELELCBML
01268                PERFORM 4200-READNEXT-WITHIN-FILE THRU 4200-EXIT   ELELCBML
01269                     UNTIL SCREEN-BUILT.                           ELELCBML
01270      GO TO 4600-ENDBR.                                            ELELCBML
01271                                                                   ELELCBML
01272  5000-EXIT.  EXIT.                                                ELELCBML
01273 /                                                                 ELELCBML
01274  5050-START-ELPDE-BROWSE.                                         ELELCBML
01275      MOVE EL-DSN-ELPDE               TO ELCIO-FILE-DDNAME2.       ELELCBML
01276      MOVE EL-ELPDE-REC-LEN           TO ELCIO-MAX-REC-LEN2.       ELELCBML
01277      MOVE 'EQ'                       TO ELCIO-CIO-QUAL2.          ELELCBML
01278      MOVE 11                         TO ELCIO-BROWSE-KEYLEN2.     ELELCBML
01279      MOVE 'M'                        TO ELCIO-STORAGE2.           ELELCBML
01280      MOVE 'SB'                       TO ELCIO-FILE-ACCESS-CODE2.  ELELCBML
01281      MOVE CA-SEL-RECORD-PREFIX       TO DE-RECORD-PREFIX.         ELELCBML
01282      MOVE CA-SEL-ELEMENT-NBR         TO DE-ELEMENT-NBR.           ELELCBML
01283      MOVE DE-PRIMARY-KEY             TO ELCIO-VSAM-KEY2.          ELELCBML
01284      SET  ELCIO-REC-AREA-ADDRESS2 TO  CIA-ELPDE-REC-AREA-PNTR.    ELELCBML
01285      SET  CIA-IO-PARM-AREA-PNTR   TO CIA-ELPDE-IOPARM-AREA-PNTR.  ELELCBML
01286                                                                   ELELCBML
01287         EXEC CICS LINK                                            ELELCBML
01288                PROGRAM('ELAIOPGM')                                ELELCBML
01289                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCBML
01290                LENGTH(EL-CIA-POINTER-LEN)                         ELELCBML
01291         END-EXEC.                                                 ELELCBML
01292  5050-EXIT.     EXIT.                                             ELELCBML
01293 /                                                                 ELELCBML
01294 ***THE FOLLOWING PARAGRAPH WAS CHANGED TO ACCOMODATE THE ****     ELELCBML
01295 ***SEARCH FOR A DATA ELEMENT USING ALPHA CHARACTERS.  A  ****     ELELCBML
01296 ***READ PREVIOUS WAS INCORPORATED HERE TO KEEP THE BROWSE****     ELELCBML
01297 ***WITHIN THE SAME FILE.                                 ****     ELELCBML
01298  6000-FORWARD-READ.                                               ELELCBML
01299      MOVE '6000' TO WS-PARA-ID.                                   ELELCBML
01300                                                                   ELELCBML
01301      PERFORM 1100-GET-STORAGE-FOR-ELPRL THRU 1100-EXIT.           ELELCBML
01302      PERFORM 1200-GET-STORAGE-FOR-ELPDE THRU 1200-EXIT.           ELELCBML
01303      PERFORM 1300-GET-STORAGE-FOR-ELPCV THRU 1300-EXIT.           ELELCBML
01304                                                                   ELELCBML
01305      PERFORM 1250-SET-ADDRESS-OF-ELPDE THRU 1250-EXIT.            ELELCBML
01306                                                                   ELELCBML
01307                                                                   ELELCBML
01308      IF CA-LE-ELEMENT-NBR-X EQUAL HIGH-VALUES                     ELELCBML
01309            AND NOT NAME-ENTERED                                   ELELCBML
01310          GO TO 9999-ENDFILE.                                      ELELCBML
01311                                                                   ELELCBML
01312      MOVE 'SB ' TO ELCIO-FILE-ACCESS-CODE2.                       ELELCBML
01313      PERFORM 4100-BROWSE-DATA-ELEMENT-FILE THRU 4100-EXIT.        ELELCBML
01314                                                                   ELELCBML
01315      IF ELCIO-EOF-BROWSE2 AND NAME-ENTERED                        ELELCBML
01316          NEXT SENTENCE                                            ELELCBML
01317      ELSE                                                         ELELCBML
01318         IF ELCIO-EOF-BROWSE2 AND NOT NAME-ENTERED                 ELELCBML
01319             GO TO 9999-ENDFILE                                    ELELCBML
01320         ELSE                                                      ELELCBML
01321             IF NOT ELCIO-GOOD-RETURN2                             ELELCBML
01322                 MOVE 'EB09' TO WS-ABEND-CODE                      ELELCBML
01323                GO TO 9999-ABEND.                                  ELELCBML
01324                                                                   ELELCBML
01325      IF NAME-ENTERED AND (DE-RECORD-PREFIX NOT EQUAL              ELELCBML
01326          HOLD-DE-PREFIX)                                          ELELCBML
01327          PERFORM 6050-FIND-REC-FOR-NAME THRU 6050-EXIT            ELELCBML
01328             UNTIL DE-RECORD-PREFIX = HOLD-DE-PREFIX               ELELCBML
01329      ELSE                                                         ELELCBML
01330         IF (DE-PRIMARY-KEY NOT EQUAL CA-LAST-ELEMENT)             ELELCBML
01331            AND NOT NAME-ENTERED                                   ELELCBML
01332             PERFORM 9999-ENDBR THRU 9999-ENDBR-EXIT               ELELCBML
01333             GO TO 9999-ENDFILE.                                   ELELCBML
01334                                                                   ELELCBML
01335      IF NOT NAME-ENTERED                                          ELELCBML
01336         EXEC CICS LINK                                            ELELCBML
01337                PROGRAM('ELAIOPGM')                                ELELCBML
01338                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCBML
01339                LENGTH(EL-CIA-POINTER-LEN)                         ELELCBML
01340         END-EXEC.                                                 ELELCBML
01341                                                                   ELELCBML
01342      IF ELCIO-EOF-BROWSE2 AND NOT NAME-ENTERED                    ELELCBML
01343          NEXT SENTENCE                                            ELELCBML
01344      ELSE                                                         ELELCBML
01345          IF NOT ELCIO-GOOD-RETURN2 AND (NOT NAME-ENTERED          ELELCBML
01346              AND ELCIO-REC-NOT-FOUND2)                            ELELCBML
01347              MOVE 'EB10' TO WS-ABEND-CODE                         ELELCBML
01348              GO TO 9999-ABEND                                     ELELCBML
01349          ELSE                                                     ELELCBML
01350              MOVE 1 TO SUBA                                       ELELCBML
01351              PERFORM 4200-READNEXT-WITHIN-FILE THRU 4200-EXIT     ELELCBML
01352                  UNTIL SCREEN-BUILT.                              ELELCBML
01353                                                                   ELELCBML
01354      GO TO 4600-ENDBR.                                            ELELCBML
01355                                                                   ELELCBML
01356  6000-EXIT.  EXIT.                                                ELELCBML
01357 /                                                                 ELELCBML
01358  6050-FIND-REC-FOR-NAME.                                          ELELCBML
01359      MOVE 'RP' TO ELCIO-FILE-ACCESS-CODE2.                        ELELCBML
01360      EXEC CICS LINK                                               ELELCBML
01361         PROGRAM ('ELAIOPGM')                                      ELELCBML
01362         COMMAREA (ADDRESS OF CIA-PARMS-RECORD)                    ELELCBML
01363         LENGTH (EL-CIA-POINTER-LEN)                               ELELCBML
01364      END-EXEC.                                                    ELELCBML
01365      IF ELCIO-REC-NOT-FOUND2                                      ELELCBML
01366         NEXT SENTENCE.                                            ELELCBML
01367  6050-EXIT.     EXIT.                                             ELELCBML
01368 /                                                                 ELELCBML
01369  9000-XCTL-ELELCAML.                                              ELELCBML
01370                                                                   ELELCBML
01371      PERFORM 1100-GET-STORAGE-FOR-ELPRL THRU 1100-EXIT.           ELELCBML
01372      PERFORM 1200-GET-STORAGE-FOR-ELPDE THRU 1200-EXIT.           ELELCBML
01373      PERFORM 1300-GET-STORAGE-FOR-ELPCV THRU 1300-EXIT.           ELELCBML
01374                                                                   ELELCBML
01375      MOVE 'B' TO CA-CURRENT-PGM.                                  ELELCBML
01376      MOVE LOW-VALUES TO CA-SEL-ELEMENT-NBR-X                      ELELCBML
01377                         CA-ELEMENT-SCROLL-KEYS.                   ELELCBML
01378      MOVE SPACES     TO CA-SEL-ELEMENT-NAME                       ELELCBML
01379                         CA-CURRENT-FUNCTION.                      ELELCBML
01380                                                                   ELELCBML
01381      EXEC CICS XCTL                                               ELELCBML
01382                PROGRAM('ELELCAML')                                ELELCBML
01383                COMMAREA(DFHCOMMAREA)                              ELELCBML
01384                LENGTH(COMM-LENGTH)                                ELELCBML
01385      END-EXEC.                                                    ELELCBML
01386 /                                                                 ELELCBML
01387  9010-XCTL-ELELCCML.                                              ELELCBML
01388                                                                   ELELCBML
01389 *    PERFORM 1100-GET-STORAGE-FOR-ELPRL THRU 1100-EXIT.           ELELCBML
01390 *    PERFORM 1200-GET-STORAGE-FOR-ELPDE THRU 1200-EXIT.           ELELCBML
01391 *    PERFORM 1300-GET-STORAGE-FOR-ELPCV THRU 1300-EXIT.           ELELCBML
01392                                                                   ELELCBML
01393      IF CA-INQUIRY                                                ELELCBML
01394          IF SELECT-INDICATED  OR  ELEMENT-ENTERED                 ELELCBML
01395              NEXT SENTENCE                                        ELELCBML
01396          ELSE                                                     ELELCBML
01397              MOVE ERR-13-MSG TO BERRMO                            ELELCBML
01398              MOVE -1 TO FUNCL (1)                                 ELELCBML
01399              GO TO 9999-RETURN-WITH-MSG.                          ELELCBML
01400                                                                   ELELCBML
01401      MOVE 'B' TO CA-CURRENT-PGM.                                  ELELCBML
01402      IF EIBAID EQUAL DFHPF4                                       ELELCBML
01403          MOVE LOW-VALUES TO CA-SEL-ELEMENT-NBR-X                  ELELCBML
01404          MOVE SPACES     TO CA-SEL-ELEMENT-NAME                   ELELCBML
01405                             CA-CURRENT-FUNCTION.                  ELELCBML
01406                                                                   ELELCBML
01407      EXEC CICS XCTL                                               ELELCBML
01408                PROGRAM('ELELCCML')                                ELELCBML
01409                COMMAREA(DFHCOMMAREA)                              ELELCBML
01410                LENGTH(COMM-LENGTH)                                ELELCBML
01411      END-EXEC.                                                    ELELCBML
01412 /                                                                 ELELCBML
01413  9999-ENDBR.                                                      ELELCBML
01414      MOVE 'EB '            TO ELCIO-FILE-ACCESS-CODE2.            ELELCBML
01415                                                                   ELELCBML
01416      EXEC CICS LINK                                               ELELCBML
01417                PROGRAM('ELAIOPGM')                                ELELCBML
01418                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCBML
01419                LENGTH(EL-CIA-POINTER-LEN)                         ELELCBML
01420      END-EXEC.                                                    ELELCBML
01421                                                                   ELELCBML
01422      IF NOT ELCIO-GOOD-RETURN2                                    ELELCBML
01423          MOVE 'EB11' TO WS-ABEND-CODE                             ELELCBML
01424          GO TO 9999-ABEND.                                        ELELCBML
01425  9999-ENDBR-EXIT.  EXIT.                                          ELELCBML
01426 /                                                                 ELELCBML
01427  9999-ENDFILE.                                                    ELELCBML
01428                                                                   ELELCBML
01429      IF EIBAID EQUAL DFHPF8  OR  DFHPF20  OR  DFHENTER            ELELCBML
01430          MOVE ERR-03-MSG TO BERRMO                                ELELCBML
01431          MOVE HIGH-VALUES TO CA-LE-ELEMENT-NBR-X                  ELELCBML
01432      ELSE                                                         ELELCBML
01433          MOVE ERR-04-MSG TO BERRMO                                ELELCBML
01434          MOVE LOW-VALUES TO CA-FE-ELEMENT-NBR-X.                  ELELCBML
01435                                                                   ELELCBML
01436      MOVE -1 TO FUNCL (1).                                        ELELCBML
01437      GO TO 9999-RETURN-WITH-MSG.                                  ELELCBML
01438 /                                                                 ELELCBML
01439                                                                   ELELCBML
01440  9999-NOTFND.                                                     ELELCBML
01441      MOVE ERR-11-MSG TO BERRMO.                                   ELELCBML
01442      IF FUNCTION-ENTERED                                          ELELCBML
01443          MOVE -1 TO FUNCL (WS-SAVE-SUBA)                          ELELCBML
01444      ELSE                                                         ELELCBML
01445          IF ELEMENT-ENTERED                                       ELELCBML
01446              MOVE -1 TO BSCODL                                    ELELCBML
01447          ELSE                                                     ELELCBML
01448              MOVE -1 TO FUNCL (1).                                ELELCBML
01449      GO TO 9999-RETURN-WITH-MSG.                                  ELELCBML
01450                                                                   ELELCBML
01451  9999-MAPFAIL.                                                    ELELCBML
01452      MOVE ERR-12-MSG TO BERRMO.                                   ELELCBML
01453      MOVE -1 TO FUNCL (1).                                        ELELCBML
01454      GO TO 9999-RETURN-WITH-MSG.                                  ELELCBML
01455                                                                   ELELCBML
01456  9999-RETURN-WITH-MSG.                                            ELELCBML
01457      MOVE 'B' TO CA-CURRENT-PGM.                                  ELELCBML
01458                                                                   ELELCBML
01459      EXEC CICS SEND MAP ('ELCBI01')                               ELELCBML
01460                MAPSET   ('ELCBSET')                               ELELCBML
01461                DATAONLY                                           ELELCBML
01462                CURSOR                                             ELELCBML
01463      END-EXEC.                                                    ELELCBML
01464      EXEC CICS RETURN                                             ELELCBML
01465                TRANSID('ELCB')                                    ELELCBML
01466                COMMAREA(DFHCOMMAREA)                              ELELCBML
01467                LENGTH(COMM-LENGTH)                                ELELCBML
01468      END-EXEC.                                                    ELELCBML
01469  9999-ABEND.                                                      ELELCBML
01470      EXEC CICS ABEND                                              ELELCBML
01471                ABCODE(WS-ABEND-CODE)                              ELELCBML
01472      END-EXEC.                                                    ELELCBML
01473                                                                   ELELCBML
