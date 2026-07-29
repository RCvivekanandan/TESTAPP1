00001  ID DIVISION.                                                     06/29/02
00002  PROGRAM-ID.    ELELCEML.                                         ELELCEML
00003 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    LV001
00004 *       CCCCCCC OOOOOOOO  BBBBBBBBB OOOOOOOO LLLL   2222222222  * ELELCEML
00005 *     CCC      OOO   OOO BBB   BBB OOO   OOO LLL     222  222   * ELELCEML
00006 *    CCC      OOO   OOO BBB   BBB OOO   OOO LLL      222  222   * ELELCEML
00007 *   CCC      OOO   OOO BBBBBBBBB OOO   OOO LLL       222  222   * ELELCEML
00008 *  CCC      OOO   OOO BBB   BBB OOO   OOO LLL        222  222   * ELELCEML
00009 * CCC      OOO   OOO BBB   BBB OOO   OOO LLL         222  222   * ELELCEML
00010 * CCCCCCCC OOOOOOOO BBBBBBBBB OOOOOOOOO LLLLLLLLL   2222222222  * ELELCEML
00011 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ELELCEML
00012  AUTHOR.        NINA CERVANTES.                                   ELELCEML
00013 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ELELCEML
00014  INSTALLATION.  BCBS/HCMS.                                        ELELCEML
00015  DATE-WRITTEN.  10/85.                                            ELELCEML
00016  DATE-COMPILED.   /  /  .                                         ELELCEML
00017      SKIP3                                                        ELELCEML
00018 ***************************************************************** ELELCEML
00019 *            ELELCEML - ELS: CODE VALUES MAINTENANCE            * ELELCEML
00020 *                                                               * ELELCEML
00021 *    THIS MODULE IS RESPONSIBLE FOR THE ADDITION, CHANGE AND    * ELELCEML
00022 *    DELETION OF A CODE VALUE WITHIN A GIVEN ELEMENT.  PROGRAM  * ELELCEML
00023 *    CONTROL MAY COME FROM ELELCCML FOR ADDITIONS OR ELELCDML   * ELELCEML
00024 *    FOR CHANGES/DELETIONS.  THE FOLLOWING FUNCTIONS COULD THEN * ELELCEML
00025 *    BE PERFORMED:                                              * ELELCEML
00026 *                                                               * ELELCEML
00027 *    (1)  ADDITIONS - MAY BE DONE BY 2 METHODS:                 * ELELCEML
00028 *         (A)  IMMEDIATE CONTROL FROM ELELCCML;                 * ELELCEML
00029 *         (B)  USER MAY HIT PF10.                               * ELELCEML
00030 *         A NEW SCREEN WILL BE DISPLAYED FOR THE USER TO ENTER  * ELELCEML
00031 *         NEW DATA.  THE FUNCTION 'A' WILL AUTOMATICALLY DIS-   * ELELCEML
00032 *         PLAY.                                                 * ELELCEML
00033 *                                                               * ELELCEML
00034 *    (2)  CHANGES - A CODE VALUE AND IT'S DATA WILL BE DISPLAYED* ELELCEML
00035 *         FOR THE USER TO VIEW.  THE USER WILL HAVE TO ENTER THE* ELELCEML
00036 *         FUNCTION 'C' AND HIT ENTER.  THE FIELDS WHICH WILL BE * ELELCEML
00037 *         ALLOWED TO CHANGE ARE NAME AND DESCRIPTION.           * ELELCEML
00038 *                                                               * ELELCEML
00039 *    (3)  DELETIONS - A CODE VALUE AND IT'S DATA WILL BE DIS-   * ELELCEML
00040 *         PLAYED FOR THE USER TO VIEW .  THE USER WILL HAVE TO  * ELELCEML
00041 *         ENTER THE FUNCTION 'D' AND HIT ENTER.   FIRST SEND    * ELELCEML
00042 *         BACK THE SCREEN WITH A MESSAGE FOR THE USER TO HIT THE* ELELCEML
00043 *         PF6 KEY FOR CONFIRMATION AND THEN A SECOND SCREEN WILL* ELELCEML
00044 *         BE SENT TO VERIFY THAT THE RECORD WAS PEGGED FOR      * ELELCEML
00045 *         DELETION.                                             * ELELCEML
00046 *                                                               * ELELCEML
00047 *    (4)  PF3 KEY    TRANSFER CONTROL TO ELELCDML.              * ELELCEML
00048 *                                                               * ELELCEML
00049 *    (5)  PF7/PF8    BACKWARD/FORWARD POSITIONING.              * ELELCEML
00050 *                                                               * ELELCEML
00051 *    (5)  PF9 KEY    TRANSFER CONTROL TO ELELCAML.              * ELELCEML
00052 *                                                               * ELELCEML
00053 *    (6)  PF10       TO SEND INITIALIZED SCREEN FOR ADDITIONS.  * ELELCEML
00054 *                                                               * ELELCEML
00055 *    (7)  CLEAR      RETURN TO CICS.                            * ELELCEML
00056 *                                                               * ELELCEML
00057 ***************************************************************** ELELCEML
00058 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          ELELCEML
00059 *    *-*         U P D A T E   H I S T O R Y         *-*          ELELCEML
00060 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          ELELCEML
00061                                                                   ELELCEML
00062 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* ELELCEML
00063                                                                   ELELCEML
00064 *    217     08/21/86  NAC  1. CORRECT MAP-FROM LOGIC; INCREASED  ELELCEML
00065 *                              WRONG KEY VALUE.                   ELELCEML
00066 *            07/13/87  NAC  1. CORRECT MAP-FROM DELETE LOGIC.     ELELCEML
00067                                                                   ELELCEML
00068 *            12/90     RKH     CONVERTED TO COBOL II              ELELCEML
00069 *                                                                 ELELCEML
00070 ***************************************************************** ELELCEML
00071 /                                                                 ELELCEML
00072  ENVIRONMENT DIVISION.                                            ELELCEML
00073  DATA DIVISION.                                                   ELELCEML
00074  WORKING-STORAGE SECTION.                                         ELELCEML
00075  01  FILLER                 PIC X(42)        VALUE                ELELCEML
00076      '***ELELCEML WORKING STORAGE BEGINS HERE***'.                ELELCEML
00077  01  WS-PARA-ID             PIC X(4)         VALUE 'XXXX'.        ELELCEML
00078  01  WS-ABEND-CODE          PIC X(4)         VALUE 'XXXX'.        ELELCEML
00079  01  WS-LOW-VALUES          PIC X            VALUE LOW-VALUES.    ELELCEML
00080                                                                   ELELCEML
00081 ** FILE LENGTHS FOR DATA ELEMENTS ****                            ELELCEML
00082  01  ELPDE-KEY-LENGTH        PIC S9(4)   COMP VALUE +011.         ELELCEML
00083  01  ELPDE-ALT-KEY-LENGTH    PIC S9(4)   COMP VALUE +083.         ELELCEML
00084                                                                   ELELCEML
00085 ** FILE LENGTHS FOR CODE VALUE ELEMENTS ***                       ELELCEML
00086  01  ELPCV-KEY-LENGTH        PIC S9(4)   COMP VALUE +023.         ELELCEML
00087  01  ELPCV-GEN-KEY-LENGTH    PIC S9(4)   COMP VALUE +021.         ELELCEML
00088  01  ELPCV-FIXED-LENGTH      PIC S9(4)   COMP VALUE +076.         ELELCEML
00089  01  ELPCV-VALUE-DESC-LENGTH PIC S9(4)   COMP VALUE +079.         ELELCEML
00090  01  ELPCV-REC-LENGTH        PIC S9(4)   COMP VALUE +1024.        ELELCEML
00091                                                                   ELELCEML
00092 ** COMM AREA LENGTH *********                                     ELELCEML
00093  01  COMM-LENGTH             PIC S9(4)   COMP VALUE +500.         ELELCEML
00094                                                                   ELELCEML
00095 ** DD NAMES AND FILE LENGTHS ******                               ELELCEML
00096  01  RLEN.                                                        ELELCEML
00097      COPY ELCDRLEN.                                               ELELCEML
00098 /                                                                 ELELCEML
00099                                                                   ELELCEML
00100 ** SWITCHES **                                                    ELELCEML
00101  01  SCREEN-SW               PIC XXX   VALUE 'NO '.               ELELCEML
00102      88  SCREEN-BUILT                  VALUE 'YES'.               ELELCEML
00103  01  FUNC-SW                 PIC X     VALUE SPACE.               ELELCEML
00104      88  ADD-INDICATED                 VALUE 'A'.                 ELELCEML
00105      88  CHANGE-INDICATED              VALUE 'C'.                 ELELCEML
00106      88  DELETE-INDICATED              VALUE 'D'.                 ELELCEML
00107      88  MAP-FROM-INDICATED            VALUE 'M'.                 ELELCEML
00108  01  CHANGE-SW               PIC XXX   VALUE 'NO '.               ELELCEML
00109      88  DATA-CHANGED                  VALUE 'YES'.               ELELCEML
00110  01  MOVE-SW                 PIC XXX   VALUE 'NO '.               ELELCEML
00111      88  DESC-LINES-MOVED              VALUE 'YES'.               ELELCEML
00112  01  COPY-SW                 PIC XXX   VALUE 'NO '.               ELELCEML
00113      88  COPIED-ALL-VALUE-SEQS         VALUE 'YES'.               ELELCEML
00114  01  DELETE-SW               PIC XXX   VALUE 'NO '.               ELELCEML
00115      88  DELETED-ALL-PRESENT-VALUES    VALUE 'YES'.               ELELCEML
00116 /                                                                 ELELCEML
00117  01  VALIDATION-MASKS.                                            ELELCEML
00118      03  MASK-A           PIC X(8)         VALUE 'RRROONNN'.      ELELCEML
00119      03  MASK-C           PIC X(8)         VALUE 'RRROONNN'.      ELELCEML
00120      03  MASK-D           PIC X(8)         VALUE 'RRROONNN'.      ELELCEML
00121      03  MASK-M           PIC X(8)         VALUE 'RRROOORO'.      ELELCEML
00122      03  MASK-I           PIC X(8)         VALUE 'NNNNNNNN'.      ELELCEML
00123                                                                   ELELCEML
00124  01  VALIDATION-VALUES.                                           ELELCEML
00125      03  FUNCTION-SW             PIC X     VALUE SPACE.           ELELCEML
00126          88  FUNCTION-REQUIRED             VALUE 'R'.             ELELCEML
00127          88  FUNCTION-NOT-REQUIRED         VALUE 'N'.             ELELCEML
00128      03  CODE-VALUE-SW           PIC X     VALUE SPACE.           ELELCEML
00129          88  CODE-VALUE-REQUIRED           VALUE 'R'.             ELELCEML
00130          88  CODE-VALUE-NOT-REQUIRED       VALUE 'N'.             ELELCEML
00131      03  SEQUENCE-SW             PIC X     VALUE SPACE.           ELELCEML
00132          88  SEQUENCE-REQUIRED             VALUE 'R'.             ELELCEML
00133          88  SEQUENCE-NOT-REQUIRED         VALUE 'N'.             ELELCEML
00134      03  NAME-SW                 PIC X     VALUE SPACE.           ELELCEML
00135          88  NAME-REQUIRED                 VALUE 'R'.             ELELCEML
00136          88  NAME-NOT-REQUIRED             VALUE 'N'.             ELELCEML
00137          88  NAME-OPTIONAL                 VALUE 'O'.             ELELCEML
00138      03  DESCRIPTION-SW          PIC X     VALUE SPACE.           ELELCEML
00139          88  DESCRIPTION-OPTIONAL          VALUE 'O'.             ELELCEML
00140          88  DESCRIPTION-NOT-REQUIRED      VALUE 'N'.             ELELCEML
00141      03  MAPF-PREFIX-SW          PIC X     VALUE SPACE.           ELELCEML
00142          88  MAPF-PREFIX-OPTIONAL          VALUE 'O'.             ELELCEML
00143          88  MAPF-PREFIX-REQUIRED          VALUE 'R'.             ELELCEML
00144          88  MAPF-PREFIX-NOT-REQUIRED      VALUE 'N'.             ELELCEML
00145      03  MAPF-CODE-SW            PIC X     VALUE SPACE.           ELELCEML
00146          88 MAPF-CODE-REQUIRED             VALUE 'R'.             ELELCEML
00147          88 MAPF-CODE-NOT-REQUIRED         VALUE 'N'.             ELELCEML
00148      03  MAPF-NAME-SW            PIC X     VALUE SPACE.           ELELCEML
00149          88 MAPF-NAME-OPTIONAL             VALUE 'O'.             ELELCEML
00150          88 MAPF-NAME-REQUIRED             VALUE 'R'.             ELELCEML
00151          88 MAPF-NAME-NOT-REQUIRED         VALUE 'N'.             ELELCEML
00152  01  VALIDATION-TABLE  REDEFINES  VALIDATION-VALUES PIC X(8).     ELELCEML
00153 /                                                                 ELELCEML
00154 ** WORK AREAS**                                                   ELELCEML
00155  01  SUBA                   PIC 99      VALUE ZERO.               ELELCEML
00156  01  SUBB                   PIC 99      VALUE ZERO.               ELELCEML
00157  01  LINE-CNT               PIC 99      VALUE ZERO.               ELELCEML
00158  01  WS-REFORMAT-ELEMENT    PIC ZZ9.99.                           ELELCEML
00159  01  WS-SAVE-SEQUENCE-N     PIC 99      VALUE ZERO.               ELELCEML
00160  01  WS-HEX-00              PIC X       VALUE LOW-VALUE.          ELELCEML
00161 /                                                                 ELELCEML
00162 ** MESSAGES **                                                    ELELCEML
00163  01  MAP-LITERAL1           PIC X(79)  VALUE                      ELELCEML
00164      '(PF3=CODE SEL)(PF7/8=BKWD/FWD)(PF9=RECORD SEL)(PF10=ERASE DAELELCEML
00165 -    'TA)(CLEAR=EXIT)'.                                           ELELCEML
00166  01  MAP-LITERAL2           PIC X(79)  VALUE                      ELELCEML
00167      'CODE VALUE HAS BEEN MARKED FOR DELETION'.                   ELELCEML
00168                                                                   ELELCEML
00169  01  MAP-LITERAL3           PIC X(79)  VALUE                      ELELCEML
00170      'PLEASE ENTER PF6 TO CONFIRM DELETE'.                        ELELCEML
00171                                                                   ELELCEML
00172  01  MAP-LITERAL4           PIC X(79)  VALUE                      ELELCEML
00173      'PLEASE ENTER PF6 TO CONFIRM MAP FROM'.                      ELELCEML
00174                                                                   ELELCEML
00175  01  MAP-LITERAL5           PIC X(79)  VALUE                      ELELCEML
00176      'CODE VALUE HAS BEEN ADDED           '.                      ELELCEML
00177                                                                   ELELCEML
00178  01  MAP-LITERAL6           PIC X(79)  VALUE                      ELELCEML
00179      'CODE VALUE HAS BEEN CHANGED         '.                      ELELCEML
00180                                                                   ELELCEML
00181  01  MAP-LITERAL7           PIC X(79)  VALUE                      ELELCEML
00182      'MAP-FROM SUCCESSFUL'.                                       ELELCEML
00183                                                                   ELELCEML
00184  01  MAP-LITERAL8           PIC X(79)  VALUE                      ELELCEML
00185      '(PF3=CODE SEL)(PF7/8=BKWD/FWD)(PF9=RECORD SEL)(CLEAR=EXIT)'.ELELCEML
00186                                                                   ELELCEML
00187  01  MAP-LITERAL9           PIC X(79)  VALUE                      ELELCEML
00188      'FCN IS (A)DD (C)HANGE (D)ELETE OR (M)AP FROM'.              ELELCEML
00189                                                                   ELELCEML
00190  01  MAP-LITERAL10          PIC X(79)  VALUE                      ELELCEML
00191      '                                            '.              ELELCEML
00192                                                                   ELELCEML
00193  01  ERR-01-MSG             PIC X(79)  VALUE                      ELELCEML
00194      'INQUIRY MODE ONLY - NO OTHER FUNCTION ALLOWED'.             ELELCEML
00195                                                                   ELELCEML
00196  01  ERR-02-MSG             PIC X(79)  VALUE                      ELELCEML
00197      'CODE VALUE MUST NOT BE ALTERED FOR CHANGE FUNCTION'.        ELELCEML
00198                                                                   ELELCEML
00199  01  ERR-03-MSG             PIC X(79)  VALUE                      ELELCEML
00200      'INVALID PFKEY ENTERED - PLEASE RESUBMIT'.                   ELELCEML
00201                                                                   ELELCEML
00202  01  ERR-04-MSG             PIC X(79)  VALUE                      ELELCEML
00203      'INVALID FUNCTION CODE ENTERED'.                             ELELCEML
00204                                                                   ELELCEML
00205  01  ERR-05-MSG             PIC X(79)  VALUE                      ELELCEML
00206      'PLEASE INDICATE WHICH FUNCTION IS TO BE PERFORMED'.         ELELCEML
00207                                                                   ELELCEML
00208  01  ERR-06-MSG             PIC X(79)  VALUE                      ELELCEML
00209      'CODE VALUE FOR FUNCTION ENTERED CAN NOT BE FOUND - PLEASE NOELELCEML
00210 -    'TIFY SYSTEMS'.                                              ELELCEML
00211                                                                   ELELCEML
00212  01  ERR-07-MSG             PIC X(79)  VALUE                      ELELCEML
00213      'CODE VALUE RECORD/SEQ EXISTS ON FILE - CHANGE SEQ NO'.      ELELCEML
00214                                                                   ELELCEML
00215  01  ERR-08-MSG             PIC X(79)  VALUE                      ELELCEML
00216      'UNAUTHORIZED USER FOR MAP-FROM FUNCTION'.                   ELELCEML
00217                                                                   ELELCEML
00218  01  ERR-09-MSG             PIC X(79)  VALUE                      ELELCEML
00219      'PLEASE ENTER CODE VALUE'.                                   ELELCEML
00220                                                                   ELELCEML
00221  01  ERR-10-MSG             PIC X(79)  VALUE                      ELELCEML
00222      'THE ORIGINAL DATA HAS NOT BEEN ALTERED - PLEASE RESUBMIT'.  ELELCEML
00223                                                                   ELELCEML
00224  01  ERR-11-MSG             PIC X(79)  VALUE                      ELELCEML
00225      'SEQUENCE NUMBER MUST BE NUMERIC'.                           ELELCEML
00226                                                                   ELELCEML
00227  01  ERR-12-MSG             PIC X(79)  VALUE                      ELELCEML
00228      'SEQUENCE NUMBER MUST BE ENTERED'.                           ELELCEML
00229                                                                   ELELCEML
00230  01  ERR-13-MSG.                                                  ELELCEML
00231      03  FILLER             PIC X(31)  VALUE SPACES.              ELELCEML
00232      03  FILLER             PIC X(16)  VALUE                      ELELCEML
00233      'END OF RETRIEVAL'.                                          ELELCEML
00234      03  FILLER             PIC X(32)  VALUE SPACES.              ELELCEML
00235                                                                   ELELCEML
00236  01  ERR-14-MSG.                                                  ELELCEML
00237      03  FILLER             PIC X(34)  VALUE SPACES.              ELELCEML
00238      03  FILLER             PIC X(10)  VALUE                      ELELCEML
00239      'FIRST PAGE'.                                                ELELCEML
00240      03  FILLER             PIC X(35)  VALUE SPACES.              ELELCEML
00241                                                                   ELELCEML
00242  01  ERR-15-MSG             PIC X(79)  VALUE                      ELELCEML
00243      'MAPFAIL--PLEASE INFORM ELS SYSTEM GROUP'.                   ELELCEML
00244                                                                   ELELCEML
00245  01  ERR-16-MSG             PIC X(79)  VALUE                      ELELCEML
00246      'PLEASE ENTER CODE VALUE NAME'.                              ELELCEML
00247                                                                   ELELCEML
00248  01  ERR-17-MSG             PIC X(79)  VALUE                      ELELCEML
00249      'PLEASE ENTER MAP-FROM RECORD PREFIX '.                      ELELCEML
00250                                                                   ELELCEML
00251  01  ERR-18-MSG             PIC X(79)  VALUE                      ELELCEML
00252      'PLEASE ENTER MAP-FROM CODE VALUE '.                         ELELCEML
00253                                                                   ELELCEML
00254  01  ERR-19-MSG             PIC X(79)  VALUE                      ELELCEML
00255      'PLEASE ENTER MAP-FROM DATA ELEMENT NAME'.                   ELELCEML
00256                                                                   ELELCEML
00257  01  ERR-20-MSG             PIC X(79)  VALUE                      ELELCEML
00258      'FIELD NOT REQUIRED'.                                        ELELCEML
00259                                                                   ELELCEML
00260  01  ERR-21-MSG             PIC X(79)  VALUE                      ELELCEML
00261      'MAP-FROM CODE VALUE DOES NOT EXIST'.                        ELELCEML
00262                                                                   ELELCEML
00263  01  ERR-22-MSG             PIC X(79)  VALUE                      ELELCEML
00264      'CODE SEQ. MUST NOT BE ALTERED FOR CHANGE FUNCTION'.         ELELCEML
00265                                                                   ELELCEML
00266  01  ERR-23-MSG             PIC X(79)  VALUE                      ELELCEML
00267      'INVALID ENTRY - IDENTICAL MAP-FROM/PRESENT CODE VALUE KEYS'.ELELCEML
00268                                                                   ELELCEML
00269 /                                                                 ELELCEML
00270 ** MAP AREA **                                                    ELELCEML
00271  COPY ELCESETC.                                                   ELELCEML
00272  01  FILLER  REDEFINES  ELCEI01I.                                 ELELCEML
00273      03  FILLER             PIC X(292).                           ELELCEML
00274      03  MAP-GRP  OCCURS  12  TIMES.                              ELELCEML
00275          05  DESCL          PIC S9(4) COMP.                       ELELCEML
00276          05  DESCA          PIC X.                                ELELCEML
00277          05  DESC           PIC X(79).                            ELELCEML
00278      03  FILLER             PIC X(186).                           ELELCEML
00279 /                                                                 ELELCEML
00280 ** ATTRIBUTES **                                                  ELELCEML
00281  COPY DFHBMSCA.                                                   ELELCEML
00282      02  DFHBMADF                PIC X VALUE 'Z'.                 ELELCEML
00283 /                                                                 ELELCEML
00284 ** ATTENTION IDENTIFIERS **                                       ELELCEML
00285  COPY DFHAID.                                                     ELELCEML
00286  01  FILLER                 PIC X(31)         VALUE               ELELCEML
00287      '***WORKING STORAGE ENDS HERE***'.                           ELELCEML
00288 /                                                                 ELELCEML
00289  LINKAGE SECTION.                                                 ELELCEML
00290 /                                                                 ELELCEML
00291  01  DFHCOMMAREA.                                                 ELELCEML
00292  COPY ELPCOMMC.                                                   ELELCEML
00293                                                                   ELELCEML
00294  01  CIA-PARMS-RECORD.                                            ELELCEML
00295      COPY ELCDCIA.                                                ELELCEML
00296 /                                                                 ELELCEML
00297  01  IOPARM-RECORD-LIST.                                          ELELCEML
00298      COPY ELCDIOPM.                                               ELELCEML
00299 /                                                                 ELELCEML
00300  01  EL-RECORD-LIST.                                              ELELCEML
00301      COPY ELPRLC.                                                 ELELCEML
00302 /                                                                 ELELCEML
00303  01  IOPARM-DATA-ELEMENT.                                         ELELCEML
00304      COPY ELCDIOP2.                                               ELELCEML
00305 /                                                                 ELELCEML
00306  01  EL-DATA-ELEMENT.                                             ELELCEML
00307      COPY ELPDEC                                                  ELELCEML
00308      REPLACING == OCCURS 1 TO 11 ==                               ELELCEML
00309             BY == OCCURS      11 ==                               ELELCEML
00310                == DEPENDING ON DE-NBR-DESC-LINES ==               ELELCEML
00311             BY ==                                ==.              ELELCEML
00312 /                                                                 ELELCEML
00313  01  IOPARM-CODE-VALUE.                                           ELELCEML
00314      COPY ELCDIOP3.                                               ELELCEML
00315 /                                                                 ELELCEML
00316  01  EL-CODE-VALUE.                                               ELELCEML
00317      COPY ELPCVC                                                  ELELCEML
00318      REPLACING == OCCURS 1 TO 12 TIMES ==                         ELELCEML
00319             BY == OCCURS      12 TIMES. ==                        ELELCEML
00320                == DEPENDING ON    ==                              ELELCEML
00321             BY ==                 ==                              ELELCEML
00322                == CV-NBR-VALUE-DESC-LINES. ==                     ELELCEML
00323             BY ==                          ==.                    ELELCEML
00324 /                                                                 ELELCEML
00325  PROCEDURE DIVISION.                                              ELELCEML
00326      PERFORM 1000-HOUSEKEEPING THRU 1000-EXIT.                    ELELCEML
00327      PERFORM 2000-MAINLINE-PROCESSING THRU 2000-EXIT.             ELELCEML
00328  0000-RETURN.                                                     ELELCEML
00329      EXEC CICS RETURN                                             ELELCEML
00330      END-EXEC.                                                    ELELCEML
00331      GOBACK.                                                      ELELCEML
00332 /                                                                 ELELCEML
00333  1000-HOUSEKEEPING.                                               ELELCEML
00334 **************************************************************    ELELCEML
00335 *    SET UP ALL NECESSARY HANDLE CONDITION AND INITIALIZE ALL*    ELELCEML
00336 *    WORKAREAS.  CHECK IF THE COMMAREA WAS PASSED  OTHERWISE *    ELELCEML
00337 *    ABEND.                                                  *    ELELCEML
00338 **************************************************************    ELELCEML
00339                                                                   ELELCEML
00340      MOVE '1000' TO WS-PARA-ID.                                   ELELCEML
00341                                                                   ELELCEML
00342      IF CA-CV-EDIT                                                ELELCEML
00343          EXEC CICS GETMAIN                                        ELELCEML
00344                    SET(ADDRESS OF CIA-PARMS-RECORD)               ELELCEML
00345                    INITIMG(WS-HEX-00)                             ELELCEML
00346                    LENGTH(EL-CIA-REC-REC-LEN)                     ELELCEML
00347          END-EXEC                                                 ELELCEML
00348          SET CA-CIA-POINTER  TO                                   ELELCEML
00349              ADDRESS OF CIA-PARMS-RECORD                          ELELCEML
00350      ELSE                                                         ELELCEML
00351          SET ADDRESS OF CIA-PARMS-RECORD TO                       ELELCEML
00352              CA-CIA-POINTER.                                      ELELCEML
00353                                                                   ELELCEML
00354      MOVE LOW-VALUES TO ELCEI01I.                                 ELELCEML
00355                                                                   ELELCEML
00356      IF EIBCALEN EQUAL ZEROES                                     ELELCEML
00357          MOVE 'EE00' TO WS-ABEND-CODE                             ELELCEML
00358          GO TO 9999-ABEND.                                        ELELCEML
00359                                                                   ELELCEML
00360      EXEC CICS HANDLE AID                                         ELELCEML
00361                CLEAR(0000-RETURN)                                 ELELCEML
00362                PF3  (9010-XCTL-ELELCDML)                          ELELCEML
00363                PF7  (5000-BACKWARD-READ)                          ELELCEML
00364                PF8  (5050-FORWARD-READ)                           ELELCEML
00365                PF9  (9000-XCTL-ELELCAML)                          ELELCEML
00366                PF10 (6000-REINITIALIZE-SCREEN)                    ELELCEML
00367      END-EXEC.                                                    ELELCEML
00368                                                                   ELELCEML
00369      EXEC CICS HANDLE AID                                         ELELCEML
00370                PF15 (9010-XCTL-ELELCDML)                          ELELCEML
00371                PF19 (5000-BACKWARD-READ)                          ELELCEML
00372                PF20 (5050-FORWARD-READ)                           ELELCEML
00373                PF21 (9000-XCTL-ELELCAML)                          ELELCEML
00374                PF22 (6000-REINITIALIZE-SCREEN)                    ELELCEML
00375      END-EXEC.                                                    ELELCEML
00376                                                                   ELELCEML
00377      EXEC CICS HANDLE CONDITION                                   ELELCEML
00378                MAPFAIL(9999-MAPFAIL)                              ELELCEML
00379      END-EXEC.                                                    ELELCEML
00380                                                                   ELELCEML
00381  1000-EXIT.  EXIT.                                                ELELCEML
00382 /                                                                 ELELCEML
00383  1100-GET-STORAGE-FOR-ELPRL.                                      ELELCEML
00384      MOVE '1100' TO WS-PARA-ID.                                   ELELCEML
00385 *                                                                 ELELCEML
00386 *   ISSUE GETMAIN FOR RECORD LIST I/O AREA                        ELELCEML
00387 *         GETMAIN FOR RECORD LIST RECORD                          ELELCEML
00388 *                                                                 ELELCEML
00389                                                                   ELELCEML
00390 *--> GETMAIN FOR RECORD LIST I/O PARM                             ELELCEML
00391                                                                   ELELCEML
00392      EXEC CICS GETMAIN                                            ELELCEML
00393             SET(ADDRESS OF IOPARM-RECORD-LIST)                    ELELCEML
00394             LENGTH(EL-IOPARMS-REC-LEN)                            ELELCEML
00395             INITIMG(WS-LOW-VALUES)                                ELELCEML
00396      END-EXEC.                                                    ELELCEML
00397                                                                   ELELCEML
00398      SET CIA-ELPRL-IOPARM-AREA-PNTR TO                            ELELCEML
00399                 ADDRESS OF IOPARM-RECORD-LIST.                    ELELCEML
00400      MOVE EL-DSN-ELPRL  TO ELCIO-FILE-DDNAME.                     ELELCEML
00401                                                                   ELELCEML
00402 *--> GETMAIN FOR RECORD LIST                                      ELELCEML
00403                                                                   ELELCEML
00404      EXEC CICS  GETMAIN                                           ELELCEML
00405                 SET(ADDRESS OF EL-RECORD-LIST)                    ELELCEML
00406                 LENGTH(EL-ELPRL-REC-LEN)                          ELELCEML
00407                 INITIMG(WS-LOW-VALUES)                            ELELCEML
00408      END-EXEC.                                                    ELELCEML
00409                                                                   ELELCEML
00410      SET CIA-ELPRL-REC-AREA-PNTR TO                               ELELCEML
00411                 ADDRESS OF EL-RECORD-LIST.                        ELELCEML
00412                                                                   ELELCEML
00413                                                                   ELELCEML
00414  1100-EXIT.  EXIT.                                                ELELCEML
00415 /                                                                 ELELCEML
00416  1200-GET-STORAGE-FOR-ELPDE.                                      ELELCEML
00417 *                                                                 ELELCEML
00418 *   ISSUE GETMAIN FOR DATA ELEMENT I/O AREA                       ELELCEML
00419 *         GETMAIN FOR DATA ELEMENT RECORD                         ELELCEML
00420 *                                                                 ELELCEML
00421      MOVE '1200' TO WS-PARA-ID.                                   ELELCEML
00422                                                                   ELELCEML
00423 *--> GETMAIN FOR DATA ELEMENT I/O AREA                            ELELCEML
00424                                                                   ELELCEML
00425      EXEC CICS GETMAIN                                            ELELCEML
00426             SET(ADDRESS OF IOPARM-DATA-ELEMENT)                   ELELCEML
00427             LENGTH(EL-IOPARMS-REC-LEN)                            ELELCEML
00428             INITIMG(WS-LOW-VALUES)                                ELELCEML
00429      END-EXEC.                                                    ELELCEML
00430                                                                   ELELCEML
00431      SET CIA-ELPDE-IOPARM-AREA-PNTR TO                            ELELCEML
00432                 ADDRESS OF IOPARM-DATA-ELEMENT.                   ELELCEML
00433      MOVE EL-DSN-ELPDE  TO ELCIO-FILE-DDNAME2.                    ELELCEML
00434                                                                   ELELCEML
00435 *--> GETMAIN FOR DATA ELEMENT RECORD                              ELELCEML
00436                                                                   ELELCEML
00437      EXEC CICS GETMAIN                                            ELELCEML
00438                 SET(ADDRESS OF EL-DATA-ELEMENT)                   ELELCEML
00439                 LENGTH(EL-ELPDE-REC-LEN)                          ELELCEML
00440                 INITIMG(WS-LOW-VALUES)                            ELELCEML
00441      END-EXEC.                                                    ELELCEML
00442                                                                   ELELCEML
00443      SET CIA-ELPDE-REC-AREA-PNTR TO                               ELELCEML
00444          ADDRESS OF EL-DATA-ELEMENT.                              ELELCEML
00445                                                                   ELELCEML
00446                                                                   ELELCEML
00447  1200-EXIT.  EXIT.                                                ELELCEML
00448 /                                                                 ELELCEML
00449  1250-SET-ADDRESS-OF-ELPDE.                                       ELELCEML
00450 *                                                                 ELELCEML
00451 *   SET ADDRESS FOR DATA ELEMENT I/O AREA                         ELELCEML
00452 *       ADDRESS FOR DATA ELEMENT RECORD                           ELELCEML
00453 *                                                                 ELELCEML
00454      MOVE '1250' TO WS-PARA-ID.                                   ELELCEML
00455                                                                   ELELCEML
00456      SET ADDRESS OF IOPARM-DATA-ELEMENT TO                        ELELCEML
00457           CIA-ELPDE-IOPARM-AREA-PNTR.                             ELELCEML
00458                                                                   ELELCEML
00459      SET  CIA-IO-PARM-AREA-PNTR         TO                        ELELCEML
00460           CIA-ELPDE-IOPARM-AREA-PNTR.                             ELELCEML
00461                                                                   ELELCEML
00462      SET ADDRESS OF EL-DATA-ELEMENT TO                            ELELCEML
00463          CIA-ELPDE-REC-AREA-PNTR.                                 ELELCEML
00464                                                                   ELELCEML
00465                                                                   ELELCEML
00466  1250-EXIT.  EXIT.                                                ELELCEML
00467 /                                                                 ELELCEML
00468  1300-GET-STORAGE-FOR-ELPCV.                                      ELELCEML
00469 *                                                                 ELELCEML
00470 *   ISSUE GETMAIN FOR CODE VALUE I/O AREA                         ELELCEML
00471 *         GETMAIN FOR CODE VALUE RECORD                           ELELCEML
00472 *                                                                 ELELCEML
00473      MOVE '1300' TO WS-PARA-ID.                                   ELELCEML
00474                                                                   ELELCEML
00475 *--> GETMAIN FOR CODE VALUE PARMS LIST                            ELELCEML
00476                                                                   ELELCEML
00477      EXEC CICS GETMAIN                                            ELELCEML
00478             SET(ADDRESS OF IOPARM-CODE-VALUE)                     ELELCEML
00479             LENGTH(EL-IOPARMS-REC-LEN)                            ELELCEML
00480             INITIMG(WS-LOW-VALUES)                                ELELCEML
00481      END-EXEC.                                                    ELELCEML
00482                                                                   ELELCEML
00483      SET CIA-ELPCV-IOPARM-AREA-PNTR TO                            ELELCEML
00484          ADDRESS OF IOPARM-CODE-VALUE.                            ELELCEML
00485                                                                   ELELCEML
00486      MOVE EL-DSN-ELPCV  TO ELCIO-FILE-DDNAME3.                    ELELCEML
00487 *--> GETMAIN FOR CODE VALUE                                       ELELCEML
00488                                                                   ELELCEML
00489      EXEC CICS GETMAIN                                            ELELCEML
00490                 SET(ADDRESS OF EL-CODE-VALUE)                     ELELCEML
00491                 LENGTH(EL-ELPCV-REC-LEN)                          ELELCEML
00492                 INITIMG(WS-LOW-VALUES)                            ELELCEML
00493      END-EXEC.                                                    ELELCEML
00494                                                                   ELELCEML
00495      SET CIA-ELPCV-REC-AREA-PNTR TO                               ELELCEML
00496          ADDRESS OF EL-CODE-VALUE.                                ELELCEML
00497                                                                   ELELCEML
00498  1300-EXIT.  EXIT.                                                ELELCEML
00499 /                                                                 ELELCEML
00500  1350-SET-ADDRESS-OF-ELPCV.                                       ELELCEML
00501 *                                                                 ELELCEML
00502 *   SET ADDRESS FOR CODE VALUE I/O AREA                           ELELCEML
00503 *       ADDRESS FOR CODE VALUE RECORD                             ELELCEML
00504 *                                                                 ELELCEML
00505      MOVE '1350' TO WS-PARA-ID.                                   ELELCEML
00506                                                                   ELELCEML
00507      SET ADDRESS OF IOPARM-CODE-VALUE TO                          ELELCEML
00508           CIA-ELPCV-IOPARM-AREA-PNTR.                             ELELCEML
00509                                                                   ELELCEML
00510      SET  CIA-IO-PARM-AREA-PNTR       TO                          ELELCEML
00511           CIA-ELPCV-IOPARM-AREA-PNTR.                             ELELCEML
00512                                                                   ELELCEML
00513      SET ADDRESS OF EL-CODE-VALUE TO                              ELELCEML
00514          CIA-ELPCV-REC-AREA-PNTR.                                 ELELCEML
00515                                                                   ELELCEML
00516  1350-EXIT.  EXIT.                                                ELELCEML
00517 /                                                                 ELELCEML
00518  2000-MAINLINE-PROCESSING.                                        ELELCEML
00519 **************************************************************    ELELCEML
00520 *    BASED UPON WHERE PROGRAM CONTROL CAME, DECIDE WHETHER TO*    ELELCEML
00521 *    CREATE AND SEND OUT THE SCREEN FOR THE INITIAL TIME OR  *    ELELCEML
00522 *    RECEIVE THE SCREEN AND EDIT.  IF CA-CURRENT-PGM EQUAL   *    ELELCEML
00523 *    'C' (ELELCCML) OR 'D' (ELELCDML) PERFORM 4000 ROUTINE;  *    ELELCEML
00524 *    IF 'E' (ELELCEML) PERFORM 3000 ROUTINE ELSE ABEND.      *    ELELCEML
00525 **************************************************************    ELELCEML
00526                                                                   ELELCEML
00527      MOVE '2000' TO WS-PARA-ID.                                   ELELCEML
00528      IF CA-DE-DEFINE  OR CA-SELECT-CV                             ELELCEML
00529          PERFORM 4000-CREATE-SCREEN THRU 4000-EXIT                ELELCEML
00530      ELSE                                                         ELELCEML
00531          IF CA-CV-EDIT                                            ELELCEML
00532              PERFORM 3000-RECEIVE-SCREEN THRU 3000-EXIT           ELELCEML
00533          ELSE                                                     ELELCEML
00534              MOVE 'EE01' TO WS-ABEND-CODE                         ELELCEML
00535              GO TO 9999-ABEND.                                    ELELCEML
00536                                                                   ELELCEML
00537  2000-EXIT.  EXIT.                                                ELELCEML
00538 /                                                                 ELELCEML
00539  3000-RECEIVE-SCREEN.                                             ELELCEML
00540      MOVE '3000' TO WS-PARA-ID.                                   ELELCEML
00541                                                                   ELELCEML
00542      EXEC CICS RECEIVE MAP('ELCEI01')                             ELELCEML
00543                MAPSET     ('ELCESET')                             ELELCEML
00544      END-EXEC.                                                    ELELCEML
00545                                                                   ELELCEML
00546      PERFORM 1100-GET-STORAGE-FOR-ELPRL THRU 1100-EXIT.           ELELCEML
00547      PERFORM 1200-GET-STORAGE-FOR-ELPDE THRU 1200-EXIT.           ELELCEML
00548      PERFORM 1300-GET-STORAGE-FOR-ELPCV THRU 1300-EXIT.           ELELCEML
00549                                                                   ELELCEML
00550                                                                   ELELCEML
00551      IF EIBAID EQUAL DFHENTER  OR                                 ELELCEML
00552                      DFHPF6    OR                                 ELELCEML
00553                      DFHPF18                                      ELELCEML
00554          NEXT SENTENCE                                            ELELCEML
00555      ELSE                                                         ELELCEML
00556          MOVE ERR-03-MSG TO EERRMO                                ELELCEML
00557          MOVE -1 TO EFCNL                                         ELELCEML
00558          GO TO   9999-RETURN-WITH-MSG.                            ELELCEML
00559                                                                   ELELCEML
00560      IF EFUNCI NOT EQUAL 'ELCE'                                   ELELCEML
00561          GO TO 9000-XCTL-ELELCAML.                                ELELCEML
00562                                                                   ELELCEML
00563      MOVE SPACES TO EERRMO.                                       ELELCEML
00564                                                                   ELELCEML
00565      PERFORM 3200-EDIT-SCREEN THRU 3200-EXIT.                     ELELCEML
00566                                                                   ELELCEML
00567      MOVE '3000' TO WS-PARA-ID.                                   ELELCEML
00568 *************************************************************     ELELCEML
00569 *    THIS CHECK IS TO ENSURE THAT THE USER WANTED TO DO A   *     ELELCEML
00570 *    DELETE OR MAP FROM OPERATION BY TESTING IF THE PF6 KEY *     ELELCEML
00571 *    WAS HIT, AND THAT THE OTHER INFORMATION REMAINED THE   *     ELELCEML
00572 *    SAME.  IT WAS ALREADY DETERMINED THAT THE USER HAS     *     ELELCEML
00573 *    SUPERVISORY LEVEL DURING  THE FIRST PASS THROUGH THIS  *     ELELCEML
00574 *    PROGRAM.                                               *     ELELCEML
00575 *************************************************************     ELELCEML
00576                                                                   ELELCEML
00577      IF EIBAID EQUAL DFHPF6  OR  DFHPF18                          ELELCEML
00578          IF CA-INQUIRY                                            ELELCEML
00579              MOVE ERR-01-MSG TO EERRMO                            ELELCEML
00580              MOVE -1 TO EFCNL                                     ELELCEML
00581              GO TO 9999-RETURN-WITH-MSG                           ELELCEML
00582          ELSE                                                     ELELCEML
00583            IF DELETE-INDICATED                                    ELELCEML
00584             PERFORM 3400-CHANGE-DELETE-CODE-VALUE THRU 3400-EXIT  ELELCEML
00585            ELSE                                                   ELELCEML
00586                IF MAP-FROM-INDICATED                              ELELCEML
00587                 PERFORM 8000-MAP-FROM-PROCESS THRU 8000-EXIT      ELELCEML
00588                ELSE                                               ELELCEML
00589                    MOVE -1 TO EFCNL                               ELELCEML
00590                    MOVE ERR-05-MSG TO EERRMO                      ELELCEML
00591                    GO TO 9999-RETURN-WITH-MSG.                    ELELCEML
00592                                                                   ELELCEML
00593 **************************************************************    ELELCEML
00594 *    DURING THE 3200 ROUTINE, THE FUNCTION CODE ENTERED HAS  *    ELELCEML
00595 *    ALREADY BEEN DETERMINED.                                *    ELELCEML
00596 *    ADD - PERFORM 3300 ROUTINE, WHICH IS RESPONSIBLE FOR THE*    ELELCEML
00597 *    ADDITION OF A NEW ELEMENT CODE VALUE RECORD.  THEN SEND *    ELELCEML
00598 *    BACK TO THE USER A CLEARED SCREEN FOR ANY ADDITIONAL    *    ELELCEML
00599 *    CODE VALUE(S).                                          *    ELELCEML
00600 *                                                            *    ELELCEML
00601 *    CHANGE - PERFORM 3400 ROUTINE, WHICH IS RESPONSIBLE FOR *    ELELCEML
00602 *    THE UPDATE OF AN EXISTING ELEMENT CODE VALUE RECORD.    *    ELELCEML
00603 *    THEN SEND BACK TO THE USER A CLEARED SCREEN FOR ANY     *    ELELCEML
00604 *    ADDITIONAL CODE VALUE(S).                               *    ELELCEML
00605 *                                                            *    ELELCEML
00606 *    DELETE - RETURN THE SCREEN WITH A MESSAGE TO THE USER   *    ELELCEML
00607 *    TO CONFIRM  THE DELETE BY ENTERING PF6.  THE DELETE     *    ELELCEML
00608 *    FLAG WILL NOT BE SET AT THIS TIME.                      *    ELELCEML
00609 *                                                            *    ELELCEML
00610 *    MAP-FROM - RETURN THE SCREEN WITH A MESSAGE TO THE USER *    ELELCEML
00611 *    TO CONFIRM THE MAP FROM BY ENTERING PF6.  THE ACTUAL    *    ELELCEML
00612 *    COPY WILL NOT BE DONE AT THIS TIME.                     *    ELELCEML
00613 **************************************************************    ELELCEML
00614                                                                   ELELCEML
00615      IF ADD-INDICATED                                             ELELCEML
00616          PERFORM 3300-ADD-NEW-CODE-VALUE THRU 3300-EXIT           ELELCEML
00617      ELSE                                                         ELELCEML
00618          IF CHANGE-INDICATED                                      ELELCEML
00619              PERFORM 3400-CHANGE-DELETE-CODE-VALUE THRU 3400-EXIT ELELCEML
00620          ELSE                                                     ELELCEML
00621              IF DELETE-INDICATED                                  ELELCEML
00622                  MOVE ECODEI         TO CA-SEL-CODE-VALUE         ELELCEML
00623                  MOVE EFCNI          TO CA-SEL-CODE-NAME          ELELCEML
00624                  MOVE MAP-LITERAL3   TO EERRMO                    ELELCEML
00625                  MOVE -1             TO EFCNL                     ELELCEML
00626                  MOVE 'D'            TO CA-CURRENT-FUNCTION       ELELCEML
00627                  GO TO 9999-RETURN-WITH-MSG                       ELELCEML
00628              ELSE                                                 ELELCEML
00629                IF MAP-FROM-INDICATED                              ELELCEML
00630                  MOVE ECODEI         TO CA-SEL-CODE-VALUE         ELELCEML
00631                  MOVE EFCNI          TO CA-SEL-CODE-NAME          ELELCEML
00632                  MOVE MFPREXI        TO CA-MF-RECORD-PREFIX       ELELCEML
00633                  MOVE MFVALI         TO CA-MF-CODE-VALUE          ELELCEML
00634                  MOVE MFNMI          TO CA-MAPFROM-ELEMENT-NAME   ELELCEML
00635                  MOVE MAP-LITERAL4   TO EERRMO                    ELELCEML
00636                  MOVE -1             TO EFCNL                     ELELCEML
00637                  MOVE 'M'            TO CA-CURRENT-FUNCTION       ELELCEML
00638                  GO TO 9999-RETURN-WITH-MSG                       ELELCEML
00639                ELSE                                               ELELCEML
00640                  GO TO 5050-FORWARD-READ.                         ELELCEML
00641                                                                   ELELCEML
00642  3000-EXIT.  EXIT.                                                ELELCEML
00643 /                                                                 ELELCEML
00644  3200-EDIT-SCREEN.                                                ELELCEML
00645      MOVE '3200' TO WS-PARA-ID.                                   ELELCEML
00646                                                                   ELELCEML
00647 ** FUNCTION CODE **                                               ELELCEML
00648                                                                   ELELCEML
00649      IF EFCNL GREATER THAN ZERO                                   ELELCEML
00650          IF CA-INQUIRY                                            ELELCEML
00651              MOVE -1 TO EFCNL                                     ELELCEML
00652              MOVE ERR-01-MSG TO EERRMO                            ELELCEML
00653              MOVE DFHBMUNP TO EFCNA                               ELELCEML
00654              GO TO 9999-RETURN-WITH-MSG                           ELELCEML
00655          ELSE                                                     ELELCEML
00656              MOVE EFCNI TO FUNC-SW                                ELELCEML
00657              IF ADD-INDICATED                                     ELELCEML
00658                 MOVE MASK-A TO VALIDATION-TABLE                   ELELCEML
00659              ELSE                                                 ELELCEML
00660                  IF CHANGE-INDICATED                              ELELCEML
00661                     MOVE MASK-C TO VALIDATION-TABLE               ELELCEML
00662                  ELSE                                             ELELCEML
00663                      IF DELETE-INDICATED                          ELELCEML
00664                         MOVE MASK-D TO VALIDATION-TABLE           ELELCEML
00665                      ELSE                                         ELELCEML
00666                          IF MAP-FROM-INDICATED                    ELELCEML
00667                              MOVE MASK-M TO VALIDATION-TABLE      ELELCEML
00668                          ELSE                                     ELELCEML
00669                              MOVE -1 TO EFCNL                     ELELCEML
00670                              MOVE ERR-04-MSG TO EERRMO            ELELCEML
00671                              MOVE DFHBMUBF TO EFCNA               ELELCEML
00672                              GO TO 9999-RETURN-WITH-MSG           ELELCEML
00673      ELSE                                                         ELELCEML
00674          IF CA-INQUIRY                                            ELELCEML
00675              MOVE MASK-I TO VALIDATION-TABLE                      ELELCEML
00676          ELSE                                                     ELELCEML
00677              MOVE -1 TO EFCNL                                     ELELCEML
00678              MOVE ERR-05-MSG TO EERRMO                            ELELCEML
00679              MOVE DFHBMUNP TO EFCNA                               ELELCEML
00680              GO TO 9999-RETURN-WITH-MSG.                          ELELCEML
00681                                                                   ELELCEML
00682                                                                   ELELCEML
00683 ** CODE VALUE **                                                  ELELCEML
00684                                                                   ELELCEML
00685      IF CODE-VALUE-REQUIRED                                       ELELCEML
00686          IF ECODEL GREATER THAN ZERO                              ELELCEML
00687              IF ECODEI NOT EQUAL CA-SEL-CODE-VALUE  AND           ELELCEML
00688                                    EFCNI EQUAL 'C'                ELELCEML
00689                  MOVE -1 TO ECODEL                                ELELCEML
00690                  MOVE ERR-02-MSG TO EERRMO                        ELELCEML
00691                  MOVE DFHBMUNP TO ECODEA                          ELELCEML
00692                  GO TO 9999-RETURN-WITH-MSG                       ELELCEML
00693              ELSE                                                 ELELCEML
00694                  MOVE ECODEI TO CA-SEL-CODE-VALUE                 ELELCEML
00695          ELSE                                                     ELELCEML
00696              MOVE -1 TO ECODEL                                    ELELCEML
00697              MOVE ERR-09-MSG TO EERRMO                            ELELCEML
00698              MOVE DFHBMUNP TO ECODEA                              ELELCEML
00699              GO TO 9999-RETURN-WITH-MSG.                          ELELCEML
00700                                                                   ELELCEML
00701                                                                   ELELCEML
00702 ** SEQUENCE **                                                    ELELCEML
00703                                                                   ELELCEML
00704      IF SEQUENCE-REQUIRED                                         ELELCEML
00705          IF ESEQL GREATER THAN ZERO                               ELELCEML
00706              IF ESEQI NUMERIC  AND ESEQI NOT EQUAL ZERO           ELELCEML
00707                  IF ESEQI NOT EQUAL CA-SEL-CODE-SEQ  AND          ELELCEML
00708                                        EFCNI EQUAL 'C'            ELELCEML
00709                      MOVE -1 TO ESEQL                             ELELCEML
00710                      MOVE ERR-22-MSG TO EERRMO                    ELELCEML
00711                      MOVE DFHBMUBF TO ESEQA                       ELELCEML
00712                      GO TO 9999-RETURN-WITH-MSG                   ELELCEML
00713                  ELSE                                             ELELCEML
00714                      MOVE ESEQI TO CA-SEL-CODE-SEQ-X              ELELCEML
00715              ELSE                                                 ELELCEML
00716                  MOVE -1 TO ESEQL                                 ELELCEML
00717                  MOVE ERR-11-MSG TO EERRMO                        ELELCEML
00718                  MOVE DFHBMUBF TO ESEQA                           ELELCEML
00719                  GO TO 9999-RETURN-WITH-MSG                       ELELCEML
00720          ELSE                                                     ELELCEML
00721              IF ADD-INDICATED                                     ELELCEML
00722                   MOVE '01' TO ESEQO                              ELELCEML
00723                                CA-SEL-CODE-SEQ-X                  ELELCEML
00724              ELSE                                                 ELELCEML
00725                  MOVE -1 TO ESEQL                                 ELELCEML
00726                  MOVE ERR-12-MSG TO EERRMO                        ELELCEML
00727                  MOVE DFHBMUNP TO ESEQA                           ELELCEML
00728                  GO TO 9999-RETURN-WITH-MSG.                      ELELCEML
00729                                                                   ELELCEML
00730 ** NAME **                                                        ELELCEML
00731                                                                   ELELCEML
00732      IF NAME-REQUIRED                                             ELELCEML
00733         IF ENAMEL GREATER THAN ZERO                               ELELCEML
00734             MOVE ENAMEI TO CA-SEL-CODE-NAME                       ELELCEML
00735         ELSE                                                      ELELCEML
00736             MOVE -1 TO ENAMEL                                     ELELCEML
00737             MOVE ERR-16-MSG TO EERRMO                             ELELCEML
00738             GO TO 9999-RETURN-WITH-MSG                            ELELCEML
00739      ELSE                                                         ELELCEML
00740          IF NAME-OPTIONAL                                         ELELCEML
00741             IF ENAMEL GREATER THAN ZERO                           ELELCEML
00742                 MOVE ENAMEI TO CA-SEL-CODE-NAME                   ELELCEML
00743             ELSE                                                  ELELCEML
00744                 MOVE SPACES TO ENAMEO.                            ELELCEML
00745                                                                   ELELCEML
00746 ** DESCRIPTION **                                                 ELELCEML
00747                                                                   ELELCEML
00748      IF DESCRIPTION-NOT-REQUIRED                                  ELELCEML
00749          NEXT SENTENCE                                            ELELCEML
00750      ELSE                                                         ELELCEML
00751          MOVE '3210' TO WS-PARA-ID                                ELELCEML
00752          PERFORM 3210-COUNT-LINES THRU 3210-EXIT                  ELELCEML
00753             VARYING SUBA FROM 1 BY 1                              ELELCEML
00754             UNTIL SUBA GREATER THAN 12                            ELELCEML
00755                                                                   ELELCEML
00756          MOVE '3200' TO WS-PARA-ID.                               ELELCEML
00757                                                                   ELELCEML
00758 ** MAP FROM PREFIX **                                             ELELCEML
00759                                                                   ELELCEML
00760      IF MAPF-PREFIX-REQUIRED                                      ELELCEML
00761          IF MFPREXL NOT GREATER THAN ZERO                         ELELCEML
00762              MOVE -1 TO MFPREXL                                   ELELCEML
00763              MOVE ERR-17-MSG TO EERRMO                            ELELCEML
00764              GO TO 9999-RETURN-WITH-MSG                           ELELCEML
00765          ELSE                                                     ELELCEML
00766              MOVE MFPREXI TO CA-MF-RECORD-PREFIX                  ELELCEML
00767                              CIA-ELPEN-RECORD-ID                  ELELCEML
00768      ELSE                                                         ELELCEML
00769          IF MAPF-PREFIX-OPTIONAL                                  ELELCEML
00770              IF MFPREXL NOT GREATER THAN ZERO                     ELELCEML
00771                  MOVE CA-SEL-RECORD-PREFIX TO MFPREXO             ELELCEML
00772                              CA-MF-RECORD-PREFIX                  ELELCEML
00773                              CIA-ELPEN-RECORD-ID                  ELELCEML
00774                  MOVE DFHBMUNF TO MFPREXA                         ELELCEML
00775              ELSE                                                 ELELCEML
00776                  MOVE MFPREXI TO CA-MF-RECORD-PREFIX              ELELCEML
00777                                  CIA-ELPEN-RECORD-ID              ELELCEML
00778          ELSE                                                     ELELCEML
00779              IF MFPREXL GREATER THAN ZERO                         ELELCEML
00780                  MOVE -1 TO MFPREXL                               ELELCEML
00781                  MOVE ERR-20-MSG TO EERRMO                        ELELCEML
00782                  GO TO 9999-RETURN-WITH-MSG.                      ELELCEML
00783                                                                   ELELCEML
00784 ** MAP FROM CODE VALUE **                                         ELELCEML
00785                                                                   ELELCEML
00786      IF MAPF-CODE-REQUIRED                                        ELELCEML
00787          IF MFVALL NOT GREATER THAN ZERO                          ELELCEML
00788              MOVE -1 TO MFVALL                                    ELELCEML
00789              MOVE ERR-18-MSG TO EERRMO                            ELELCEML
00790              GO TO 9999-RETURN-WITH-MSG                           ELELCEML
00791          ELSE                                                     ELELCEML
00792              MOVE MFVALI TO CA-MF-CODE-VALUE                      ELELCEML
00793      ELSE                                                         ELELCEML
00794          IF MFVALL GREATER THAN ZERO                              ELELCEML
00795              MOVE -1 TO MFVALL                                    ELELCEML
00796              MOVE ERR-20-MSG TO EERRMO                            ELELCEML
00797              GO TO 9999-RETURN-WITH-MSG.                          ELELCEML
00798                                                                   ELELCEML
00799 ** MAP FROM DATA ELEMENT NAME **                                  ELELCEML
00800      IF MAPF-NAME-REQUIRED                                        ELELCEML
00801          IF MFNML NOT GREATER THAN ZERO                           ELELCEML
00802              MOVE -1 TO MFNML                                     ELELCEML
00803              MOVE ERR-19-MSG TO EERRMO                            ELELCEML
00804              GO TO 9999-RETURN-WITH-MSG                           ELELCEML
00805          ELSE                                                     ELELCEML
00806              MOVE MFNMI TO CA-MAPFROM-ELEMENT-NAME                ELELCEML
00807                            CIA-ELPEN-ENG-NAME                     ELELCEML
00808      ELSE                                                         ELELCEML
00809          IF MAPF-NAME-OPTIONAL                                    ELELCEML
00810              IF MFNML NOT GREATER THAN ZERO                       ELELCEML
00811                  MOVE CA-SEL-ELEMENT-NAME TO MFNMO                ELELCEML
00812                                       CA-MAPFROM-ELEMENT-NAME     ELELCEML
00813                                       CIA-ELPEN-ENG-NAME          ELELCEML
00814                  MOVE DFHBMUNF TO MFNMA                           ELELCEML
00815              ELSE                                                 ELELCEML
00816                  MOVE MFNMI TO CA-MAPFROM-ELEMENT-NAME            ELELCEML
00817                                CIA-ELPEN-ENG-NAME                 ELELCEML
00818          ELSE                                                     ELELCEML
00819              IF MFNML GREATER THAN ZERO                           ELELCEML
00820                  MOVE -1 TO MFNML                                 ELELCEML
00821                  MOVE ERR-20-MSG TO EERRMO                        ELELCEML
00822                  GO TO 9999-RETURN-WITH-MSG.                      ELELCEML
00823                                                                   ELELCEML
00824      IF NOT MAP-FROM-INDICATED                                    ELELCEML
00825          GO TO 3200-EXIT.                                         ELELCEML
00826                                                                   ELELCEML
00827 **  VERIFTY THAT THE MAP-FROM KEYS ENTERED (OR DEFAULTED TO)  **  ELELCEML
00828 **  ARE NOT IDENTICAL TO THE PRESENT CODE VALUE KEYS.         **  ELELCEML
00829                                                                   ELELCEML
00830      IF CA-SEL-RECORD-PREFIX EQUAL CA-MF-RECORD-PREFIX  AND       ELELCEML
00831         CA-SEL-CODE-VALUE    EQUAL CA-MF-CODE-VALUE     AND       ELELCEML
00832         CA-SEL-ELEMENT-NAME  EQUAL CA-MAPFROM-ELEMENT-NAME        ELELCEML
00833           MOVE -1 TO MFPREXL                                      ELELCEML
00834           MOVE ERR-23-MSG TO EERRMO                               ELELCEML
00835           GO TO 9999-RETURN-WITH-MSG.                             ELELCEML
00836                                                                   ELELCEML
00837 **  VERIFY THAT THE MAP FROM RECORDS EXIST  **                    ELELCEML
00838                                                                   ELELCEML
00839      MOVE EL-DSN-ELPDE            TO  ELCIO-FILE-DDNAME2.         ELELCEML
00840      MOVE EL-DSN-ELPEN            TO  ELCIO-ALT-INDEX-FILE-DDNAME2ELELCEML
00841      MOVE EL-ELPDE-REC-LEN        TO  ELCIO-MAX-REC-LEN2.         ELELCEML
00842      MOVE ELPDE-KEY-LENGTH        TO  ELCIO-BROWSE-KEYLEN2.       ELELCEML
00843      MOVE 'EQ '                   TO  ELCIO-CIO-QUAL2.            ELELCEML
00844      MOVE 'M'                     TO  ELCIO-STORAGE2.             ELELCEML
00845      MOVE 'RD '                   TO  ELCIO-FILE-ACCESS-CODE2.    ELELCEML
00846      MOVE CIA-ELPEN-KEY           TO  ELCIO-VSAM-KEY2.            ELELCEML
00847                                                                   ELELCEML
00848      SET  CIA-IO-PARM-AREA-PNTR   TO                              ELELCEML
00849           CIA-ELPDE-IOPARM-AREA-PNTR.                             ELELCEML
00850                                                                   ELELCEML
00851      SET  ELCIO-REC-AREA-ADDRESS2 TO                              ELELCEML
00852           CIA-ELPDE-REC-AREA-PNTR.                                ELELCEML
00853                                                                   ELELCEML
00854      EXEC CICS LINK                                               ELELCEML
00855                PROGRAM('ELAIOPGM')                                ELELCEML
00856                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCEML
00857                LENGTH(EL-CIA-POINTER-LEN)                         ELELCEML
00858      END-EXEC.                                                    ELELCEML
00859                                                                   ELELCEML
00860      IF ELCIO-REC-NOT-FOUND2                                      ELELCEML
00861          GO TO 3200-NOTFND                                        ELELCEML
00862          ELSE                                                     ELELCEML
00863              IF NOT ELCIO-GOOD-RETURN2                            ELELCEML
00864                  MOVE 'EE02' TO WS-ABEND-CODE                     ELELCEML
00865                  GO TO 9999-ABEND.                                ELELCEML
00866                                                                   ELELCEML
00867      IF DE-DELETE                                                 ELELCEML
00868          MOVE ERR-02-MSG TO EERRMO                                ELELCEML
00869          MOVE -1 TO MFPREXL                                       ELELCEML
00870          MOVE DFHBMUBF TO MFPREXA                                 ELELCEML
00871                           MFVALA                                  ELELCEML
00872                           MFNMA                                   ELELCEML
00873          GO TO 9999-RETURN-WITH-MSG.                              ELELCEML
00874                                                                   ELELCEML
00875      MOVE DE-ELEMENT-NBR TO CA-MF-ELEMENT-NBR.                    ELELCEML
00876      MOVE 01             TO CA-MF-CODE-SEQ.                       ELELCEML
00877                                                                   ELELCEML
00878      MOVE EL-DSN-ELPCV            TO  ELCIO-FILE-DDNAME3.         ELELCEML
00879      MOVE EL-ELPCV-REC-LEN        TO  ELCIO-MAX-REC-LEN3.         ELELCEML
00880      MOVE ELPCV-KEY-LENGTH        TO  ELCIO-BROWSE-KEYLEN3.       ELELCEML
00881      MOVE 'EQ '                   TO  ELCIO-CIO-QUAL3.            ELELCEML
00882      MOVE 'M'                     TO  ELCIO-STORAGE3.             ELELCEML
00883      MOVE 'RD '                   TO  ELCIO-FILE-ACCESS-CODE3.    ELELCEML
00884      MOVE CA-MAPFROM-CV-KEY       TO  ELCIO-VSAM-KEY3.            ELELCEML
00885                                                                   ELELCEML
00886      SET  CIA-IO-PARM-AREA-PNTR   TO                              ELELCEML
00887           CIA-ELPCV-IOPARM-AREA-PNTR.                             ELELCEML
00888                                                                   ELELCEML
00889      SET  ELCIO-REC-AREA-ADDRESS3 TO                              ELELCEML
00890           CIA-ELPCV-REC-AREA-PNTR.                                ELELCEML
00891                                                                   ELELCEML
00892      EXEC CICS LINK                                               ELELCEML
00893                PROGRAM('ELAIOPGM')                                ELELCEML
00894                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCEML
00895                LENGTH(EL-CIA-POINTER-LEN)                         ELELCEML
00896      END-EXEC.                                                    ELELCEML
00897                                                                   ELELCEML
00898      IF ELCIO-REC-NOT-FOUND3                                      ELELCEML
00899          GO TO 3200-NOTFND                                        ELELCEML
00900          ELSE                                                     ELELCEML
00901              IF NOT ELCIO-GOOD-RETURN3                            ELELCEML
00902                  MOVE 'EE03' TO WS-ABEND-CODE                     ELELCEML
00903                  GO TO 9999-ABEND.                                ELELCEML
00904                                                                   ELELCEML
00905  3200-EXIT.  EXIT.                                                ELELCEML
00906      SKIP3                                                        ELELCEML
00907  3200-NOTFND.                                                     ELELCEML
00908      MOVE ERR-21-MSG TO EERRMO.                                   ELELCEML
00909      MOVE -1 TO MFPREXL.                                          ELELCEML
00910      MOVE DFHBMUBF TO MFPREXA                                     ELELCEML
00911                       MFVALA                                      ELELCEML
00912                       MFNMA                                       ELELCEML
00913      GO TO 9999-RETURN-WITH-MSG.                                  ELELCEML
00914                                                                   ELELCEML
00915      SKIP3                                                        ELELCEML
00916  3210-COUNT-LINES.                                                ELELCEML
00917      IF DESCL (SUBA) GREATER THAN ZERO                            ELELCEML
00918          ADD 1 TO LINE-CNT.                                       ELELCEML
00919  3210-EXIT. EXIT.                                                 ELELCEML
00920 /                                                                 ELELCEML
00921  3300-ADD-NEW-CODE-VALUE.                                         ELELCEML
00922      MOVE '3300' TO WS-PARA-ID.                                   ELELCEML
00923                                                                   ELELCEML
00924 ** INITIALIZE ELEMENT CODE VALUE RECORD **                        ELELCEML
00925      MOVE SPACES TO ELEMENT-CODE-VALUE.                           ELELCEML
00926      MOVE ZEROES TO CV-NBR-VALUE-DESC-LINES.                      ELELCEML
00927                                                                   ELELCEML
00928      IF LINE-CNT GREATER THAN ZERO                                ELELCEML
00929          COMPUTE ELPCV-REC-LENGTH = ELPCV-FIXED-LENGTH +          ELELCEML
00930                  (LINE-CNT * ELPCV-VALUE-DESC-LENGTH)             ELELCEML
00931          MOVE LINE-CNT TO CV-NBR-VALUE-DESC-LINES                 ELELCEML
00932          MOVE '3310' TO WS-PARA-ID                                ELELCEML
00933          PERFORM 3310-MOVE-DESCRIPTION-LINES THRU 3310-EXIT       ELELCEML
00934              VARYING SUBA FROM 1 BY 1                             ELELCEML
00935              UNTIL SUBA GREATER THAN CV-NBR-VALUE-DESC-LINES      ELELCEML
00936      ELSE                                                         ELELCEML
00937          COMPUTE ELPCV-REC-LENGTH = ELPCV-FIXED-LENGTH.           ELELCEML
00938                                                                   ELELCEML
00939      IF ENAMEL GREATER THAN ZERO                                  ELELCEML
00940          MOVE ENAMEI           TO CV-CODE-NAME.                   ELELCEML
00941                                                                   ELELCEML
00942      MOVE CA-SELECTED-CV-KEY TO CV-CODE-KEY                       ELELCEML
00943                                 ELCIO-VSAM-KEY3.                  ELELCEML
00944                                                                   ELELCEML
00945      MOVE EL-DSN-ELPCV       TO ELCIO-FILE-DDNAME3.               ELELCEML
00946      MOVE EL-ELPCV-REC-LEN   TO ELCIO-MAX-REC-LEN3                ELELCEML
00947      MOVE ELPCV-REC-LENGTH   TO ELCIO-RECORD-LEN3.                ELELCEML
00948      MOVE 'WDP'              TO ELCIO-FILE-ACCESS-CODE3.          ELELCEML
00949      MOVE 'M'                TO ELCIO-STORAGE3.                   ELELCEML
00950                                                                   ELELCEML
00951      SET  CIA-IO-PARM-AREA-PNTR   TO                              ELELCEML
00952           CIA-ELPCV-IOPARM-AREA-PNTR.                             ELELCEML
00953                                                                   ELELCEML
00954      SET  ELCIO-REC-AREA-ADDRESS3 TO                              ELELCEML
00955           CIA-ELPCV-REC-AREA-PNTR.                                ELELCEML
00956                                                                   ELELCEML
00957      EXEC CICS LINK                                               ELELCEML
00958                PROGRAM('ELAIOPGM')                                ELELCEML
00959                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCEML
00960                LENGTH(EL-CIA-POINTER-LEN)                         ELELCEML
00961      END-EXEC.                                                    ELELCEML
00962                                                                   ELELCEML
00963      IF ELCIO-DUP-KEY-ADD3                                        ELELCEML
00964          MOVE ERR-07-MSG TO EERRMO                                ELELCEML
00965          MOVE -1 TO EFCNL                                         ELELCEML
00966          GO TO 9999-RETURN-WITH-MSG                               ELELCEML
00967      ELSE                                                         ELELCEML
00968          IF NOT ELCIO-GOOD-RETURN3                                ELELCEML
00969              MOVE 'EE04' TO WS-ABEND-CODE                         ELELCEML
00970              GO TO 9999-ABEND.                                    ELELCEML
00971                                                                   ELELCEML
00972                                                                   ELELCEML
00973      MOVE '3300' TO WS-PARA-ID.                                   ELELCEML
00974      PERFORM 3320-UPDATE-DATA-ELEMENT THRU 3320-EXIT.             ELELCEML
00975                                                                   ELELCEML
00976      IF CA-FIRST-CODE EQUAL LOW-VALUES                            ELELCEML
00977          MOVE CV-CODE-KEY TO CA-FIRST-CODE.                       ELELCEML
00978                                                                   ELELCEML
00979      MOVE MAP-LITERAL5   TO EERRMO.                               ELELCEML
00980      PERFORM 4020-REINITIALIZE-SCREEN THRU 4020-EXIT.             ELELCEML
00981      PERFORM 4000-CREATE-SCREEN THRU 4000-EXIT.                   ELELCEML
00982                                                                   ELELCEML
00983  3300-EXIT.  EXIT.                                                ELELCEML
00984      SKIP3                                                        ELELCEML
00985  3310-MOVE-DESCRIPTION-LINES.                                     ELELCEML
00986      ADD 1 TO SUBB.                                               ELELCEML
00987      IF DESCL (SUBB) GREATER THAN ZERO                            ELELCEML
00988          MOVE DESC (SUBB) TO CV-VALUE-DESC-LINE (SUBA)            ELELCEML
00989      ELSE                                                         ELELCEML
00990          GO TO 3310-MOVE-DESCRIPTION-LINES.                       ELELCEML
00991  3310-EXIT.   EXIT.                                               ELELCEML
00992 /                                                                 ELELCEML
00993  3320-UPDATE-DATA-ELEMENT.                                        ELELCEML
00994                                                                   ELELCEML
00995      MOVE '3320' TO WS-PARA-ID.                                   ELELCEML
00996                                                                   ELELCEML
00997      MOVE CA-SELECTED-DE-KEY TO ELCIO-VSAM-KEY2.                  ELELCEML
00998      MOVE 'RU '              TO ELCIO-FILE-ACCESS-CODE2.          ELELCEML
00999      MOVE EL-DSN-ELPDE     TO ELCIO-FILE-DDNAME2.                 ELELCEML
01000      MOVE EL-ELPDE-REC-LEN TO ELCIO-MAX-REC-LEN2.                 ELELCEML
01001      MOVE 'M'              TO ELCIO-STORAGE2.                     ELELCEML
01002                                                                   ELELCEML
01003      SET  CIA-IO-PARM-AREA-PNTR  TO                               ELELCEML
01004           CIA-ELPDE-IOPARM-AREA-PNTR.                             ELELCEML
01005                                                                   ELELCEML
01006      SET  ELCIO-REC-AREA-ADDRESS2  TO                             ELELCEML
01007           CIA-ELPDE-REC-AREA-PNTR.                                ELELCEML
01008                                                                   ELELCEML
01009      EXEC CICS LINK                                               ELELCEML
01010                PROGRAM('ELAIOPGM')                                ELELCEML
01011                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCEML
01012                LENGTH(EL-CIA-POINTER-LEN)                         ELELCEML
01013      END-EXEC.                                                    ELELCEML
01014                                                                   ELELCEML
01015      IF NOT ELCIO-GOOD-RETURN2                                    ELELCEML
01016          MOVE 'EB05' TO WS-ABEND-CODE                             ELELCEML
01017          GO TO 9999-ABEND.                                        ELELCEML
01018                                                                   ELELCEML
01019                                                                   ELELCEML
01020      IF DE-AUTO-REPRINT-FLAG EQUAL 'E'                            ELELCEML
01021          NEXT SENTENCE                                            ELELCEML
01022      ELSE                                                         ELELCEML
01023          MOVE 'Y' TO DE-AUTO-REPRINT-FLAG.                        ELELCEML
01024                                                                   ELELCEML
01025      IF ADD-INDICATED  OR  MAP-FROM-INDICATED                     ELELCEML
01026          MOVE 'Y' TO DE-CODES-FLAG                                ELELCEML
01027          ADD 1 TO DE-CODE-VALUES-CT.                              ELELCEML
01028                                                                   ELELCEML
01029      IF DELETE-INDICATED                                          ELELCEML
01030          SUBTRACT 1 FROM DE-CODE-VALUES-CT                        ELELCEML
01031          IF DE-CODE-VALUES-CT EQUAL ZEROES                        ELELCEML
01032              MOVE SPACE TO DE-CODES-FLAG.                         ELELCEML
01033                                                                   ELELCEML
01034      MOVE 'WU '            TO ELCIO-FILE-ACCESS-CODE2.            ELELCEML
01035      MOVE EL-ELPDE-REC-LEN TO ELCIO-RECORD-LEN2.                  ELELCEML
01036                                                                   ELELCEML
01037      EXEC CICS LINK                                               ELELCEML
01038                PROGRAM('ELAIOPGM')                                ELELCEML
01039                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCEML
01040                LENGTH(EL-CIA-POINTER-LEN)                         ELELCEML
01041      END-EXEC.                                                    ELELCEML
01042                                                                   ELELCEML
01043      IF NOT ELCIO-GOOD-RETURN2                                    ELELCEML
01044          MOVE 'EB06' TO WS-ABEND-CODE                             ELELCEML
01045          GO TO 9999-ABEND.                                        ELELCEML
01046                                                                   ELELCEML
01047  3320-EXIT.   EXIT.                                               ELELCEML
01048 /                                                                 ELELCEML
01049  3400-CHANGE-DELETE-CODE-VALUE.                                   ELELCEML
01050      MOVE '3400' TO WS-PARA-ID.                                   ELELCEML
01051                                                                   ELELCEML
01052      IF DELETE-INDICATED                                          ELELCEML
01053         IF CA-SEL-CODE-VALUE EQUAL ECODEI  AND                    ELELCEML
01054            CA-CURRENT-FUNCTION EQUAL EFCNI                        ELELCEML
01055             NEXT SENTENCE                                         ELELCEML
01056         ELSE                                                      ELELCEML
01057             MOVE MAP-LITERAL3   TO EERRMO                         ELELCEML
01058             MOVE -1             TO EFCNL                          ELELCEML
01059             MOVE 'D'            TO CA-CURRENT-FUNCTION            ELELCEML
01060             GO TO 9999-RETURN-WITH-MSG.                           ELELCEML
01061                                                                   ELELCEML
01062      MOVE CA-SELECTED-CV-KEY TO ELCIO-VSAM-KEY3.                  ELELCEML
01063      MOVE EL-ELPCV-REC-LEN   TO ELCIO-MAX-REC-LEN3.               ELELCEML
01064      MOVE 'RU '              TO ELCIO-FILE-ACCESS-CODE3.          ELELCEML
01065      MOVE EL-DSN-ELPCV     TO ELCIO-FILE-DDNAME3.                 ELELCEML
01066      MOVE 'M'              TO ELCIO-STORAGE3.                     ELELCEML
01067                                                                   ELELCEML
01068      SET  CIA-IO-PARM-AREA-PNTR  TO                               ELELCEML
01069           CIA-ELPCV-IOPARM-AREA-PNTR.                             ELELCEML
01070                                                                   ELELCEML
01071      SET  ELCIO-REC-AREA-ADDRESS3  TO                             ELELCEML
01072           CIA-ELPCV-REC-AREA-PNTR.                                ELELCEML
01073                                                                   ELELCEML
01074      EXEC CICS LINK                                               ELELCEML
01075                PROGRAM('ELAIOPGM')                                ELELCEML
01076                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCEML
01077                LENGTH(EL-CIA-POINTER-LEN)                         ELELCEML
01078      END-EXEC.                                                    ELELCEML
01079                                                                   ELELCEML
01080      IF NOT ELCIO-GOOD-RETURN3                                    ELELCEML
01081          MOVE 'EB07' TO WS-ABEND-CODE                             ELELCEML
01082          GO TO 9999-ABEND.                                        ELELCEML
01083                                                                   ELELCEML
01084      IF DELETE-INDICATED                                          ELELCEML
01085          MOVE 'D' TO CV-DELETE-CODE-FLAG                          ELELCEML
01086           GO TO 3400-CONTINUE.                                    ELELCEML
01087                                                                   ELELCEML
01088      IF ENAMEL GREATER THAN ZERO                                  ELELCEML
01089          IF ENAMEI NOT EQUAL CV-CODE-NAME                         ELELCEML
01090              MOVE ENAMEI TO CV-CODE-NAME                          ELELCEML
01091              MOVE 'YES' TO CHANGE-SW.                             ELELCEML
01092                                                                   ELELCEML
01093      IF LINE-CNT NOT EQUAL CV-NBR-VALUE-DESC-LINES                ELELCEML
01094          MOVE 'YES' TO CHANGE-SW                                  ELELCEML
01095          MOVE LINE-CNT TO CV-NBR-VALUE-DESC-LINES                 ELELCEML
01096          COMPUTE ELPCV-REC-LENGTH = ELPCV-FIXED-LENGTH +          ELELCEML
01097                  (LINE-CNT * ELPCV-VALUE-DESC-LENGTH)             ELELCEML
01098          MOVE ELPCV-REC-LENGTH TO ELCIO-RECORD-LEN3               ELELCEML
01099          PERFORM 3310-MOVE-DESCRIPTION-LINES THRU 3310-EXIT       ELELCEML
01100              VARYING SUBA FROM 1 BY 1                             ELELCEML
01101              UNTIL SUBA GREATER THAN CV-NBR-VALUE-DESC-LINES      ELELCEML
01102      ELSE                                                         ELELCEML
01103          PERFORM 3410-COMPARE-DESCRIPTION THRU 3410-EXIT          ELELCEML
01104              VARYING SUBA FROM 1 BY 1                             ELELCEML
01105              UNTIL SUBA GREATER THAN 12.                          ELELCEML
01106                                                                   ELELCEML
01107                                                                   ELELCEML
01108  3400-CONTINUE.                                                   ELELCEML
01109                                                                   ELELCEML
01110      IF DATA-CHANGED  OR  DELETE-INDICATED                        ELELCEML
01111          MOVE 'WU '            TO ELCIO-FILE-ACCESS-CODE3         ELELCEML
01112                                                                   ELELCEML
01113          EXEC CICS LINK                                           ELELCEML
01114                    PROGRAM('ELAIOPGM')                            ELELCEML
01115                    COMMAREA(ADDRESS OF CIA-PARMS-RECORD)          ELELCEML
01116                    LENGTH(EL-CIA-POINTER-LEN)                     ELELCEML
01117          END-EXEC                                                 ELELCEML
01118                                                                   ELELCEML
01119          IF NOT ELCIO-GOOD-RETURN3                                ELELCEML
01120              MOVE 'EB08' TO WS-ABEND-CODE                         ELELCEML
01121              GO TO 9999-ABEND                                     ELELCEML
01122          ELSE                                                     ELELCEML
01123              PERFORM 3320-UPDATE-DATA-ELEMENT THRU 3320-EXIT      ELELCEML
01124      ELSE                                                         ELELCEML
01125          MOVE 'ULK'            TO ELCIO-FILE-ACCESS-CODE3         ELELCEML
01126                                                                   ELELCEML
01127          EXEC CICS LINK                                           ELELCEML
01128                    PROGRAM('ELAIOPGM')                            ELELCEML
01129                    COMMAREA(ADDRESS OF CIA-PARMS-RECORD)          ELELCEML
01130                    LENGTH(EL-CIA-POINTER-LEN)                     ELELCEML
01131          END-EXEC                                                 ELELCEML
01132                                                                   ELELCEML
01133          IF NOT ELCIO-GOOD-RETURN3                                ELELCEML
01134              MOVE 'EB09' TO WS-ABEND-CODE                         ELELCEML
01135              GO TO 9999-ABEND                                     ELELCEML
01136          ELSE                                                     ELELCEML
01137             MOVE ERR-10-MSG TO EERRMO                             ELELCEML
01138             MOVE -1 TO EFCNL                                      ELELCEML
01139             GO TO 9999-RETURN-WITH-MSG.                           ELELCEML
01140                                                                   ELELCEML
01141      MOVE SPACES     TO CA-SEL-CODE-NAME                          ELELCEML
01142                         CA-MAPFROM-ELEMENT-NAME.                  ELELCEML
01143      MOVE LOW-VALUES TO CA-SEL-CODE-VALUE                         ELELCEML
01144                         CA-SEL-CODE-SEQ-X                         ELELCEML
01145                         CA-MAPFROM-KEYS.                          ELELCEML
01146                                                                   ELELCEML
01147      IF DELETE-INDICATED                                          ELELCEML
01148          MOVE MAP-LITERAL2   TO EERRMO                            ELELCEML
01149          PERFORM 4020-REINITIALIZE-SCREEN THRU 4020-EXIT          ELELCEML
01150          MOVE -1       TO EFCNL                                   ELELCEML
01151          MOVE SPACES   TO CA-CURRENT-FUNCTION                     ELELCEML
01152                           CA-MAPFROM-ELEMENT-NAME                 ELELCEML
01153          MOVE LOW-VALUES TO CA-SEL-CODE-VALUE                     ELELCEML
01154                             CA-SEL-CODE-SEQ-X                     ELELCEML
01155                             CA-MAPFROM-KEYS                       ELELCEML
01156          GO TO 9999-RETURN-WITH-MSG.                              ELELCEML
01157                                                                   ELELCEML
01158      IF CHANGE-INDICATED                                          ELELCEML
01159          MOVE ECODEI TO CA-SEL-CODE-VALUE                         ELELCEML
01160          MOVE ESEQI  TO CA-SEL-CODE-SEQ-X                         ELELCEML
01161          MOVE ENAMEI TO CA-SEL-CODE-NAME                          ELELCEML
01162          MOVE SPACES     TO CA-MAPFROM-ELEMENT-NAME               ELELCEML
01163          MOVE LOW-VALUES TO CA-MAPFROM-KEYS                       ELELCEML
01164                                                                   ELELCEML
01165          MOVE MAP-LITERAL6   TO EERRMO                            ELELCEML
01166          PERFORM 4020-REINITIALIZE-SCREEN THRU 4020-EXIT          ELELCEML
01167          PERFORM 4000-CREATE-SCREEN THRU 4000-EXIT.               ELELCEML
01168                                                                   ELELCEML
01169  3400-EXIT.  EXIT.                                                ELELCEML
01170 /                                                                 ELELCEML
01171  3410-COMPARE-DESCRIPTION.                                        ELELCEML
01172          IF DESC (SUBA) NOT EQUAL CV-VALUE-DESC-LINE (SUBA)       ELELCEML
01173              MOVE 'YES' TO CHANGE-SW                              ELELCEML
01174              MOVE DESC (SUBA) TO CV-VALUE-DESC-LINE (SUBA).       ELELCEML
01175  3410-EXIT.  EXIT.                                                ELELCEML
01176 /                                                                 ELELCEML
01177  4000-CREATE-SCREEN.                                              ELELCEML
01178      MOVE '4000' TO WS-PARA-ID.                                   ELELCEML
01179                                                                   ELELCEML
01180 ** MOVE COMMON PORTION **                                         ELELCEML
01181      MOVE CA-SEL-RECORD-PREFIX          TO ERPREXO.               ELELCEML
01182      MOVE CA-SEL-RECORD-NAME            TO ERNAMEO.               ELELCEML
01183      MOVE CA-SEL-ELEMENT-NBR            TO WS-REFORMAT-ELEMENT.   ELELCEML
01184      MOVE WS-REFORMAT-ELEMENT           TO EEPREXO.               ELELCEML
01185      MOVE CA-SEL-ELEMENT-NAME           TO EENAMEO.               ELELCEML
01186      MOVE CA-TRANS-HDR                  TO ETITLEO.               ELELCEML
01187                                                                   ELELCEML
01188      IF CA-SEL-CODE-VALUE NOT EQUAL LOW-VALUES                    ELELCEML
01189          MOVE CA-SEL-CODE-VALUE         TO ECODEO.                ELELCEML
01190                                                                   ELELCEML
01191      IF CA-SEL-CODE-NAME NOT EQUAL SPACES                         ELELCEML
01192          MOVE CA-SEL-CODE-NAME          TO ENAMEO.                ELELCEML
01193                                                                   ELELCEML
01194      IF MAP-FROM-INDICATED  OR  ADD-INDICATED  OR                 ELELCEML
01195         CHANGE-INDICATED                                          ELELCEML
01196          NEXT SENTENCE                                            ELELCEML
01197      ELSE                                                         ELELCEML
01198          IF CA-INQUIRY                                            ELELCEML
01199              MOVE MAP-LITERAL10 TO ELINEO                         ELELCEML
01200              MOVE MAP-LITERAL8 TO EERRMO                          ELELCEML
01201          ELSE                                                     ELELCEML
01202              MOVE MAP-LITERAL9 TO ELINEO                          ELELCEML
01203              MOVE MAP-LITERAL1 TO EERRMO.                         ELELCEML
01204                                                                   ELELCEML
01205      IF CA-SEL-CODE-SEQ-X EQUAL LOW-VALUES                        ELELCEML
01206          MOVE 01 TO CA-SEL-CODE-SEQ.                              ELELCEML
01207                                                                   ELELCEML
01208      MOVE CA-SEL-CODE-SEQ TO ESEQO.                               ELELCEML
01209      MOVE DFHBMUNF TO ESEQA.                                      ELELCEML
01210                                                                   ELELCEML
01211      IF CA-SEL-CODE-VALUE EQUAL LOW-VALUES                        ELELCEML
01212          GO TO 4000-CONTINUE.                                     ELELCEML
01213                                                                   ELELCEML
01214      PERFORM 1300-GET-STORAGE-FOR-ELPCV THRU 1300-EXIT.           ELELCEML
01215                                                                   ELELCEML
01216      MOVE EL-DSN-ELPCV       TO ELCIO-FILE-DDNAME3.               ELELCEML
01217      MOVE EL-ELPCV-REC-LEN   TO ELCIO-MAX-REC-LEN3.               ELELCEML
01218      MOVE ELPCV-KEY-LENGTH   TO ELCIO-BROWSE-KEYLEN3.             ELELCEML
01219      MOVE 'GTE'              TO ELCIO-CIO-QUAL3.                  ELELCEML
01220      MOVE 'M'                TO ELCIO-STORAGE3.                   ELELCEML
01221      MOVE CA-SELECTED-KEYS   TO ELCIO-VSAM-KEY3.                  ELELCEML
01222      MOVE 'RD '              TO ELCIO-FILE-ACCESS-CODE3.          ELELCEML
01223                                                                   ELELCEML
01224      SET  CIA-IO-PARM-AREA-PNTR  TO                               ELELCEML
01225           CIA-ELPCV-IOPARM-AREA-PNTR.                             ELELCEML
01226                                                                   ELELCEML
01227      SET  ELCIO-REC-AREA-ADDRESS3  TO                             ELELCEML
01228           CIA-ELPCV-REC-AREA-PNTR.                                ELELCEML
01229                                                                   ELELCEML
01230      EXEC CICS LINK                                               ELELCEML
01231                PROGRAM('ELAIOPGM')                                ELELCEML
01232                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCEML
01233                LENGTH(EL-CIA-POINTER-LEN)                         ELELCEML
01234      END-EXEC.                                                    ELELCEML
01235                                                                   ELELCEML
01236      IF NOT ELCIO-GOOD-RETURN3                                    ELELCEML
01237          MOVE 'EE10' TO WS-ABEND-CODE                             ELELCEML
01238          GO TO 9999-ABEND.                                        ELELCEML
01239                                                                   ELELCEML
01240                                                                   ELELCEML
01241      MOVE '4010' TO WS-PARA-ID.                                   ELELCEML
01242      PERFORM 4010-MOVE-INFO-TO-SCREEN THRU 4010-EXIT              ELELCEML
01243         VARYING SUBA FROM 1 BY 1                                  ELELCEML
01244         UNTIL SUBA GREATER THAN CV-NBR-VALUE-DESC-LINES.          ELELCEML
01245                                                                   ELELCEML
01246  4000-CONTINUE.                                                   ELELCEML
01247      MOVE 'E' TO CA-CURRENT-PGM.                                  ELELCEML
01248                                                                   ELELCEML
01249      EXEC CICS SEND MAP ('ELCEI01')                               ELELCEML
01250                MAPSET   ('ELCESET')                               ELELCEML
01251                ERASE                                              ELELCEML
01252      END-EXEC.                                                    ELELCEML
01253      EXEC CICS RETURN                                             ELELCEML
01254                TRANSID('ELCE')                                    ELELCEML
01255                COMMAREA(DFHCOMMAREA)                              ELELCEML
01256                LENGTH(COMM-LENGTH)                                ELELCEML
01257      END-EXEC.                                                    ELELCEML
01258                                                                   ELELCEML
01259  4000-EXIT.  EXIT.                                                ELELCEML
01260      SKIP3                                                        ELELCEML
01261  4010-MOVE-INFO-TO-SCREEN.                                        ELELCEML
01262      MOVE CV-VALUE-DESC-LINE (SUBA) TO DESC (SUBA).               ELELCEML
01263      MOVE DFHBMUNF TO  DESCA (SUBA).                              ELELCEML
01264  4010-EXIT.  EXIT.                                                ELELCEML
01265 /                                                                 ELELCEML
01266  4020-REINITIALIZE-SCREEN.                                        ELELCEML
01267      MOVE '4020' TO WS-PARA-ID.                                   ELELCEML
01268      MOVE SPACES     TO EFCNO                                     ELELCEML
01269                         MFPREXO                                   ELELCEML
01270                         MFVALO                                    ELELCEML
01271                         MFNMO.                                    ELELCEML
01272      MOVE DFHBMUNP TO   EFCNA                                     ELELCEML
01273                         MFPREXA                                   ELELCEML
01274                         MFVALA                                    ELELCEML
01275                         MFNMA.                                    ELELCEML
01276      MOVE '4030' TO WS-PARA-ID.                                   ELELCEML
01277      PERFORM 4030-REINITIALIZE-DESCRIPTION THRU 4030-EXIT         ELELCEML
01278          VARYING SUBA FROM 1 BY 1                                 ELELCEML
01279          UNTIL SUBA GREATER THAN 12.                              ELELCEML
01280  4020-EXIT.  EXIT.                                                ELELCEML
01281      SKIP3                                                        ELELCEML
01282  4030-REINITIALIZE-DESCRIPTION.                                   ELELCEML
01283      MOVE SPACES     TO DESC  (SUBA).                             ELELCEML
01284      MOVE DFHBMUNP TO   DESCA (SUBA).                             ELELCEML
01285  4030-EXIT.  EXIT.                                                ELELCEML
01286 /                                                                 ELELCEML
01287  5000-BACKWARD-READ.                                              ELELCEML
01288      MOVE '5000' TO WS-PARA-ID.                                   ELELCEML
01289                                                                   ELELCEML
01290      PERFORM 1100-GET-STORAGE-FOR-ELPRL THRU 1100-EXIT.           ELELCEML
01291      PERFORM 1200-GET-STORAGE-FOR-ELPDE THRU 1200-EXIT.           ELELCEML
01292      PERFORM 1300-GET-STORAGE-FOR-ELPCV THRU 1300-EXIT.           ELELCEML
01293                                                                   ELELCEML
01294      IF (CA-SEL-CODE-SEQ - 1) EQUAL ZERO                          ELELCEML
01295          MOVE ERR-14-MSG TO EERRMO                                ELELCEML
01296          MOVE -1         TO EFCNL                                 ELELCEML
01297          GO TO 9999-RETURN-WITH-MSG.                              ELELCEML
01298                                                                   ELELCEML
01299      MOVE CA-SELECTED-KEYS TO ELCIO-VSAM-KEY3.                    ELELCEML
01300      MOVE 'SBP'            TO ELCIO-FILE-ACCESS-CODE3             ELELCEML
01301      MOVE EL-DSN-ELPCV     TO ELCIO-FILE-DDNAME3.                 ELELCEML
01302      MOVE EL-ELPCV-REC-LEN TO ELCIO-MAX-REC-LEN3.                 ELELCEML
01303      MOVE 'GTE'            TO ELCIO-CIO-QUAL3.                    ELELCEML
01304      MOVE ELPCV-KEY-LENGTH TO ELCIO-BROWSE-KEYLEN3.               ELELCEML
01305      MOVE 'M'              TO ELCIO-STORAGE3.                     ELELCEML
01306                                                                   ELELCEML
01307      SET  CIA-IO-PARM-AREA-PNTR  TO                               ELELCEML
01308           CIA-ELPCV-IOPARM-AREA-PNTR.                             ELELCEML
01309                                                                   ELELCEML
01310      SET  ELCIO-REC-AREA-ADDRESS3  TO                             ELELCEML
01311           CIA-ELPCV-REC-AREA-PNTR.                                ELELCEML
01312                                                                   ELELCEML
01313      EXEC CICS LINK                                               ELELCEML
01314                PROGRAM('ELAIOPGM')                                ELELCEML
01315                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCEML
01316                LENGTH(EL-CIA-POINTER-LEN)                         ELELCEML
01317      END-EXEC.                                                    ELELCEML
01318                                                                   ELELCEML
01319      IF ELCIO-EOF-BROWSE3                                         ELELCEML
01320          GO TO 5000-ENDFILE                                       ELELCEML
01321      ELSE                                                         ELELCEML
01322          IF NOT ELCIO-GOOD-RETURN3                                ELELCEML
01323             MOVE 'EE11' TO WS-ABEND-CODE                          ELELCEML
01324             GO TO 9999-ABEND.                                     ELELCEML
01325                                                                   ELELCEML
01326  5000-CONTINUE.                                                   ELELCEML
01327                                                                   ELELCEML
01328                                                                   ELELCEML
01329      IF CV-CODE-KEY EQUAL CA-SELECTED-KEYS                        ELELCEML
01330          EXEC CICS LINK                                           ELELCEML
01331                    PROGRAM('ELAIOPGM')                            ELELCEML
01332                    COMMAREA(ADDRESS OF CIA-PARMS-RECORD)          ELELCEML
01333                    LENGTH(EL-CIA-POINTER-LEN)                     ELELCEML
01334          END-EXEC                                                 ELELCEML
01335                                                                   ELELCEML
01336          IF ELCIO-EOF-BROWSE3                                     ELELCEML
01337              GO TO 5000-ENDFILE                                   ELELCEML
01338          ELSE                                                     ELELCEML
01339              IF NOT ELCIO-GOOD-RETURN3                            ELELCEML
01340                 MOVE 'EE12' TO WS-ABEND-CODE                      ELELCEML
01341                 GO TO 9999-ABEND                                  ELELCEML
01342              ELSE                                                 ELELCEML
01343                  GO TO 5000-CONTINUE.                             ELELCEML
01344                                                                   ELELCEML
01345      IF CV-RECORD-PREFIX  EQUAL  CA-SEL-RECORD-PREFIX  AND        ELELCEML
01346         CV-ELEMENT-NBR    EQUAL  CA-SEL-ELEMENT-NBR    AND        ELELCEML
01347         CV-CODE-VALUE     EQUAL  CA-SEL-CODE-VALUE                ELELCEML
01348          PERFORM 4020-REINITIALIZE-SCREEN THRU 4020-EXIT          ELELCEML
01349          MOVE '5000' TO WS-PARA-ID                                ELELCEML
01350          MOVE CV-CODE-VALUE      TO ECODEO                        ELELCEML
01351          MOVE CV-CODE-DESC-SEQ   TO CA-SEL-CODE-SEQ-X             ELELCEML
01352                                     ESEQO                         ELELCEML
01353          MOVE CV-CODE-NAME       TO CA-SEL-CODE-NAME              ELELCEML
01354                                     ENAMEO                        ELELCEML
01355          MOVE '4010' TO WS-PARA-ID                                ELELCEML
01356          PERFORM 4010-MOVE-INFO-TO-SCREEN THRU 4010-EXIT          ELELCEML
01357             VARYING SUBA FROM 1 BY 1                              ELELCEML
01358             UNTIL SUBA GREATER THAN CV-NBR-VALUE-DESC-LINES       ELELCEML
01359          MOVE '5000' TO WS-PARA-ID                                ELELCEML
01360          IF CA-INQUIRY                                            ELELCEML
01361              MOVE MAP-LITERAL10 TO ELINEO                         ELELCEML
01362              MOVE MAP-LITERAL8 TO EERRMO                          ELELCEML
01363          ELSE                                                     ELELCEML
01364              MOVE MAP-LITERAL9  TO ELINEO                         ELELCEML
01365              MOVE MAP-LITERAL1 TO EERRMO.                         ELELCEML
01366                                                                   ELELCEML
01367      MOVE 'EB ' TO ELCIO-FILE-ACCESS-CODE3.                       ELELCEML
01368      EXEC CICS LINK                                               ELELCEML
01369                PROGRAM('ELAIOPGM')                                ELELCEML
01370                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCEML
01371                LENGTH(EL-CIA-POINTER-LEN)                         ELELCEML
01372      END-EXEC.                                                    ELELCEML
01373                                                                   ELELCEML
01374      IF NOT ELCIO-GOOD-RETURN3                                    ELELCEML
01375          MOVE 'EE13' TO WS-ABEND-CODE                             ELELCEML
01376          GO TO 9999-ABEND.                                        ELELCEML
01377                                                                   ELELCEML
01378      IF CV-RECORD-PREFIX  EQUAL  CA-SEL-RECORD-PREFIX  AND        ELELCEML
01379         CV-ELEMENT-NBR    EQUAL  CA-SEL-ELEMENT-NBR    AND        ELELCEML
01380         CV-CODE-VALUE     EQUAL  CA-SEL-CODE-VALUE                ELELCEML
01381          MOVE 'E' TO CA-CURRENT-PGM                               ELELCEML
01382                                                                   ELELCEML
01383          EXEC CICS SEND MAP ('ELCEI01')                           ELELCEML
01384                    MAPSET   ('ELCESET')                           ELELCEML
01385                    ERASE                                          ELELCEML
01386          END-EXEC                                                 ELELCEML
01387                                                                   ELELCEML
01388          EXEC CICS RETURN                                         ELELCEML
01389                    TRANSID('ELCE')                                ELELCEML
01390                    COMMAREA(DFHCOMMAREA)                          ELELCEML
01391                    LENGTH(COMM-LENGTH)                            ELELCEML
01392          END-EXEC.                                                ELELCEML
01393                                                                   ELELCEML
01394  5000-ENDFILE.                                                    ELELCEML
01395      MOVE ERR-14-MSG TO EERRMO.                                   ELELCEML
01396      MOVE -1 TO EFCNL                                             ELELCEML
01397      GO TO 9999-RETURN-WITH-MSG.                                  ELELCEML
01398                                                                   ELELCEML
01399  5000-EXIT.  EXIT.                                                ELELCEML
01400 /                                                                 ELELCEML
01401  5050-FORWARD-READ.                                               ELELCEML
01402      MOVE '5050' TO WS-PARA-ID.                                   ELELCEML
01403                                                                   ELELCEML
01404      PERFORM 1100-GET-STORAGE-FOR-ELPRL THRU 1100-EXIT.           ELELCEML
01405      PERFORM 1200-GET-STORAGE-FOR-ELPDE THRU 1200-EXIT.           ELELCEML
01406      PERFORM 1300-GET-STORAGE-FOR-ELPCV THRU 1300-EXIT.           ELELCEML
01407                                                                   ELELCEML
01408      MOVE CA-SELECTED-KEYS   TO  ELCIO-VSAM-KEY3.                 ELELCEML
01409      MOVE 'SB'               TO  ELCIO-FILE-ACCESS-CODE3.         ELELCEML
01410      MOVE EL-DSN-ELPCV       TO  ELCIO-FILE-DDNAME3.              ELELCEML
01411      MOVE EL-ELPCV-REC-LEN   TO  ELCIO-MAX-REC-LEN3.              ELELCEML
01412      MOVE 'GTE'              TO  ELCIO-CIO-QUAL3.                 ELELCEML
01413      MOVE ELPCV-KEY-LENGTH   TO  ELCIO-BROWSE-KEYLEN3.            ELELCEML
01414      MOVE 'M'                TO  ELCIO-STORAGE3.                  ELELCEML
01415                                                                   ELELCEML
01416      SET  CIA-IO-PARM-AREA-PNTR  TO                               ELELCEML
01417           CIA-ELPCV-IOPARM-AREA-PNTR.                             ELELCEML
01418                                                                   ELELCEML
01419      SET  ELCIO-REC-AREA-ADDRESS3  TO                             ELELCEML
01420           CIA-ELPCV-REC-AREA-PNTR.                                ELELCEML
01421                                                                   ELELCEML
01422      EXEC CICS LINK                                               ELELCEML
01423                PROGRAM('ELAIOPGM')                                ELELCEML
01424                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCEML
01425                LENGTH(EL-CIA-POINTER-LEN)                         ELELCEML
01426      END-EXEC.                                                    ELELCEML
01427                                                                   ELELCEML
01428      IF ELCIO-EOF-BROWSE3                                         ELELCEML
01429          GO TO 5050-ENDFILE                                       ELELCEML
01430      ELSE                                                         ELELCEML
01431          IF NOT ELCIO-GOOD-RETURN3                                ELELCEML
01432             MOVE 'EE14' TO WS-ABEND-CODE                          ELELCEML
01433             GO TO 9999-ABEND.                                     ELELCEML
01434                                                                   ELELCEML
01435  5050-CONTINUE.                                                   ELELCEML
01436                                                                   ELELCEML
01437                                                                   ELELCEML
01438      IF CV-CODE-KEY EQUAL CA-SELECTED-KEYS                        ELELCEML
01439          EXEC CICS LINK                                           ELELCEML
01440                    PROGRAM('ELAIOPGM')                            ELELCEML
01441                    COMMAREA(ADDRESS OF CIA-PARMS-RECORD)          ELELCEML
01442                    LENGTH(EL-CIA-POINTER-LEN)                     ELELCEML
01443          END-EXEC                                                 ELELCEML
01444                                                                   ELELCEML
01445          IF ELCIO-EOF-BROWSE3                                     ELELCEML
01446              GO TO 5050-ENDFILE                                   ELELCEML
01447          ELSE                                                     ELELCEML
01448              IF NOT ELCIO-GOOD-RETURN3                            ELELCEML
01449                 MOVE 'EE15' TO WS-ABEND-CODE                      ELELCEML
01450                 GO TO 9999-ABEND                                  ELELCEML
01451              ELSE                                                 ELELCEML
01452                  GO TO 5050-CONTINUE.                             ELELCEML
01453                                                                   ELELCEML
01454                                                                   ELELCEML
01455      IF CV-RECORD-PREFIX  EQUAL  CA-SEL-RECORD-PREFIX  AND        ELELCEML
01456         CV-ELEMENT-NBR    EQUAL  CA-SEL-ELEMENT-NBR    AND        ELELCEML
01457         CV-CODE-VALUE     EQUAL  CA-SEL-CODE-VALUE                ELELCEML
01458          PERFORM 4020-REINITIALIZE-SCREEN THRU 4020-EXIT          ELELCEML
01459          MOVE '5050' TO WS-PARA-ID                                ELELCEML
01460          MOVE CV-CODE-VALUE      TO ECODEO                        ELELCEML
01461          MOVE CV-CODE-DESC-SEQ   TO CA-SEL-CODE-SEQ-X             ELELCEML
01462                                     ESEQO                         ELELCEML
01463          MOVE CV-CODE-NAME       TO CA-SEL-CODE-NAME              ELELCEML
01464                                     ENAMEO                        ELELCEML
01465          MOVE '4010' TO WS-PARA-ID                                ELELCEML
01466          PERFORM 4010-MOVE-INFO-TO-SCREEN THRU 4010-EXIT          ELELCEML
01467             VARYING SUBA FROM 1 BY 1                              ELELCEML
01468             UNTIL SUBA GREATER THAN CV-NBR-VALUE-DESC-LINES       ELELCEML
01469          MOVE '5050' TO WS-PARA-ID                                ELELCEML
01470          IF CA-INQUIRY                                            ELELCEML
01471              MOVE MAP-LITERAL10 TO ELINEO                         ELELCEML
01472              MOVE MAP-LITERAL8 TO EERRMO                          ELELCEML
01473          ELSE                                                     ELELCEML
01474              MOVE MAP-LITERAL9 TO ELINEO                          ELELCEML
01475              MOVE MAP-LITERAL1 TO EERRMO.                         ELELCEML
01476                                                                   ELELCEML
01477                                                                   ELELCEML
01478      MOVE 'EB ' TO ELCIO-FILE-ACCESS-CODE3.                       ELELCEML
01479      EXEC CICS LINK                                               ELELCEML
01480                PROGRAM('ELAIOPGM')                                ELELCEML
01481                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCEML
01482                LENGTH(EL-CIA-POINTER-LEN)                         ELELCEML
01483      END-EXEC.                                                    ELELCEML
01484                                                                   ELELCEML
01485      IF NOT ELCIO-GOOD-RETURN3                                    ELELCEML
01486          MOVE 'EE16' TO WS-ABEND-CODE                             ELELCEML
01487          GO TO 9999-ABEND.                                        ELELCEML
01488                                                                   ELELCEML
01489      IF CV-RECORD-PREFIX  EQUAL  CA-SEL-RECORD-PREFIX  AND        ELELCEML
01490         CV-ELEMENT-NBR    EQUAL  CA-SEL-ELEMENT-NBR    AND        ELELCEML
01491         CV-CODE-VALUE     EQUAL  CA-SEL-CODE-VALUE                ELELCEML
01492          MOVE 'E' TO CA-CURRENT-PGM                               ELELCEML
01493                                                                   ELELCEML
01494          EXEC CICS SEND MAP ('ELCEI01')                           ELELCEML
01495                    MAPSET   ('ELCESET')                           ELELCEML
01496                    ERASE                                          ELELCEML
01497          END-EXEC                                                 ELELCEML
01498                                                                   ELELCEML
01499          EXEC CICS RETURN                                         ELELCEML
01500                    TRANSID('ELCE')                                ELELCEML
01501                    COMMAREA(DFHCOMMAREA)                          ELELCEML
01502                    LENGTH(COMM-LENGTH)                            ELELCEML
01503          END-EXEC.                                                ELELCEML
01504                                                                   ELELCEML
01505                                                                   ELELCEML
01506  5050-ENDFILE.                                                    ELELCEML
01507      MOVE ERR-13-MSG TO EERRMO.                                   ELELCEML
01508      MOVE -1 TO EFCNL                                             ELELCEML
01509      GO TO 9999-RETURN-WITH-MSG.                                  ELELCEML
01510                                                                   ELELCEML
01511  5050-EXIT.  EXIT.                                                ELELCEML
01512 /                                                                 ELELCEML
01513  6000-REINITIALIZE-SCREEN.                                        ELELCEML
01514 **************************************************************    ELELCEML
01515 *                                                            *    ELELCEML
01516 *    IF THE USER HIT PF10 KEY, SEND BACK TO THE USER A       *    ELELCEML
01517 *    CLEARED SCREEN FOR ANY ADDITONAL CODE VALUE(S).         *    ELELCEML
01518 **************************************************************    ELELCEML
01519                                                                   ELELCEML
01520      MOVE '6000' TO WS-PARA-ID.                                   ELELCEML
01521                                                                   ELELCEML
01522      PERFORM 1100-GET-STORAGE-FOR-ELPRL THRU 1100-EXIT.           ELELCEML
01523      PERFORM 1200-GET-STORAGE-FOR-ELPDE THRU 1200-EXIT.           ELELCEML
01524      PERFORM 1300-GET-STORAGE-FOR-ELPCV THRU 1300-EXIT.           ELELCEML
01525                                                                   ELELCEML
01526      IF CA-INQUIRY                                                ELELCEML
01527          MOVE ERR-01-MSG TO EERRMO                                ELELCEML
01528          MOVE -1 TO EFCNL                                         ELELCEML
01529          GO TO 9999-RETURN-WITH-MSG.                              ELELCEML
01530                                                                   ELELCEML
01531      PERFORM 4020-REINITIALIZE-SCREEN THRU 4020-EXIT.             ELELCEML
01532                                                                   ELELCEML
01533      MOVE SPACES TO ECODEO                                        ELELCEML
01534                     ENAMEO.                                       ELELCEML
01535      MOVE DFHBMUNP TO ECODEA                                      ELELCEML
01536                       ENAMEA.                                     ELELCEML
01537                                                                   ELELCEML
01538      MOVE 01 TO ESEQO.                                            ELELCEML
01539      MOVE DFHBMUNF TO ESEQA.                                      ELELCEML
01540      MOVE MAP-LITERAL1 TO EERRMO.                                 ELELCEML
01541      MOVE 'E' TO CA-CURRENT-PGM.                                  ELELCEML
01542                                                                   ELELCEML
01543      EXEC CICS SEND MAP ('ELCEI01')                               ELELCEML
01544                MAPSET   ('ELCESET')                               ELELCEML
01545                ERASE                                              ELELCEML
01546      END-EXEC.                                                    ELELCEML
01547                                                                   ELELCEML
01548      EXEC CICS RETURN                                             ELELCEML
01549                TRANSID('ELCE')                                    ELELCEML
01550                COMMAREA(DFHCOMMAREA)                              ELELCEML
01551                LENGTH(COMM-LENGTH)                                ELELCEML
01552      END-EXEC.                                                    ELELCEML
01553  6000-EXIT.  EXIT.                                                ELELCEML
01554 /                                                                 ELELCEML
01555  8000-MAP-FROM-PROCESS.                                           ELELCEML
01556                                                                   ELELCEML
01557      MOVE '8000' TO WS-PARA-ID.                                   ELELCEML
01558                                                                   ELELCEML
01559      IF CA-SEL-CODE-VALUE       EQUAL ECODEI  AND                 ELELCEML
01560         CA-CURRENT-FUNCTION     EQUAL EFCNI AND                   ELELCEML
01561         CA-MF-RECORD-PREFIX     EQUAL MFPREXI  AND                ELELCEML
01562         CA-MF-CODE-VALUE        EQUAL MFVALI   AND                ELELCEML
01563         CA-MAPFROM-ELEMENT-NAME EQUAL MFNMI                       ELELCEML
01564          NEXT SENTENCE                                            ELELCEML
01565      ELSE                                                         ELELCEML
01566          MOVE ECODEI         TO CA-SEL-CODE-VALUE                 ELELCEML
01567          MOVE EFCNI          TO CA-SEL-CODE-NAME                  ELELCEML
01568          MOVE MFPREXI        TO CA-MF-RECORD-PREFIX               ELELCEML
01569          MOVE MFVALI         TO CA-MF-CODE-VALUE                  ELELCEML
01570          MOVE MFNMI          TO CA-MAPFROM-ELEMENT-NAME           ELELCEML
01571          MOVE MAP-LITERAL4   TO EERRMO                            ELELCEML
01572          MOVE -1             TO EFCNL                             ELELCEML
01573          MOVE 'M'            TO CA-CURRENT-FUNCTION               ELELCEML
01574          GO TO 9999-RETURN-WITH-MSG.                              ELELCEML
01575                                                                   ELELCEML
01576      PERFORM 1350-SET-ADDRESS-OF-ELPCV THRU 1350-EXIT.            ELELCEML
01577      MOVE 01                   TO CA-SEL-CODE-SEQ.                ELELCEML
01578                                                                   ELELCEML
01579      MOVE '8025' TO WS-PARA-ID.                                   ELELCEML
01580      PERFORM 8025-DELETE-PRESENT-VALUES THRU 8025-EXIT            ELELCEML
01581          UNTIL DELETED-ALL-PRESENT-VALUES.                        ELELCEML
01582                                                                   ELELCEML
01583                                                                   ELELCEML
01584      PERFORM 1250-SET-ADDRESS-OF-ELPDE THRU 1250-EXIT.            ELELCEML
01585                                                                   ELELCEML
01586      MOVE MFPREXI TO CIA-ELPEN-RECORD-ID.                         ELELCEML
01587      MOVE MFNMI   TO CIA-ELPEN-ENG-NAME.                          ELELCEML
01588                                                                   ELELCEML
01589      MOVE EL-DSN-ELPDE       TO ELCIO-FILE-DDNAME2.               ELELCEML
01590      MOVE EL-DSN-ELPEN       TO ELCIO-ALT-INDEX-FILE-DDNAME2.     ELELCEML
01591      MOVE EL-ELPDE-REC-LEN   TO ELCIO-MAX-REC-LEN2.               ELELCEML
01592      MOVE ELPDE-KEY-LENGTH   TO ELCIO-BROWSE-KEYLEN2.             ELELCEML
01593      MOVE 'EQ '              TO ELCIO-CIO-QUAL2.                  ELELCEML
01594      MOVE 'M'                TO ELCIO-STORAGE2.                   ELELCEML
01595      MOVE CIA-ELPEN-KEY      TO ELCIO-VSAM-KEY2.                  ELELCEML
01596      MOVE 'RD '              TO ELCIO-FILE-ACCESS-CODE2.          ELELCEML
01597                                                                   ELELCEML
01598      SET  CIA-IO-PARM-AREA-PNTR  TO                               ELELCEML
01599           CIA-ELPDE-IOPARM-AREA-PNTR.                             ELELCEML
01600                                                                   ELELCEML
01601      SET  ELCIO-REC-AREA-ADDRESS2  TO                             ELELCEML
01602           CIA-ELPDE-REC-AREA-PNTR.                                ELELCEML
01603                                                                   ELELCEML
01604      EXEC CICS LINK                                               ELELCEML
01605                PROGRAM('ELAIOPGM')                                ELELCEML
01606                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCEML
01607                LENGTH(EL-CIA-POINTER-LEN)                         ELELCEML
01608      END-EXEC.                                                    ELELCEML
01609                                                                   ELELCEML
01610      IF NOT ELCIO-GOOD-RETURN2                                    ELELCEML
01611          MOVE 'EE17' TO WS-ABEND-CODE                             ELELCEML
01612          GO TO 9999-ABEND.                                        ELELCEML
01613                                                                   ELELCEML
01614      PERFORM 1350-SET-ADDRESS-OF-ELPCV THRU 1350-EXIT.            ELELCEML
01615                                                                   ELELCEML
01616      MOVE EL-DSN-ELPCV       TO ELCIO-FILE-DDNAME3.               ELELCEML
01617      MOVE EL-ELPCV-REC-LEN   TO ELCIO-MAX-REC-LEN3.               ELELCEML
01618      MOVE ELPCV-KEY-LENGTH   TO ELCIO-BROWSE-KEYLEN3.             ELELCEML
01619      MOVE 'GTE'              TO ELCIO-CIO-QUAL3.                  ELELCEML
01620      MOVE 'M'                TO ELCIO-STORAGE3.                   ELELCEML
01621      MOVE CA-MAPFROM-CV-KEY  TO ELCIO-VSAM-KEY3.                  ELELCEML
01622      MOVE 'RD '              TO ELCIO-FILE-ACCESS-CODE3.          ELELCEML
01623                                                                   ELELCEML
01624      SET  CIA-IO-PARM-AREA-PNTR  TO                               ELELCEML
01625           CIA-ELPCV-IOPARM-AREA-PNTR.                             ELELCEML
01626                                                                   ELELCEML
01627      SET  ELCIO-REC-AREA-ADDRESS3  TO                             ELELCEML
01628           CIA-ELPCV-REC-AREA-PNTR.                                ELELCEML
01629                                                                   ELELCEML
01630      EXEC CICS LINK                                               ELELCEML
01631                PROGRAM('ELAIOPGM')                                ELELCEML
01632                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCEML
01633                LENGTH(EL-CIA-POINTER-LEN)                         ELELCEML
01634      END-EXEC.                                                    ELELCEML
01635                                                                   ELELCEML
01636      IF NOT ELCIO-GOOD-RETURN3                                    ELELCEML
01637          MOVE 'EE18' TO WS-ABEND-CODE                             ELELCEML
01638          GO TO 9999-ABEND.                                        ELELCEML
01639                                                                   ELELCEML
01640                                                                   ELELCEML
01641      MOVE 'NO ' TO COPY-SW.                                       ELELCEML
01642      MOVE '8050' TO WS-PARA-ID.                                   ELELCEML
01643      PERFORM 8050-ADD-NEW-CODE-VALUE THRU 8050-EXIT               ELELCEML
01644          UNTIL COPIED-ALL-VALUE-SEQS.                             ELELCEML
01645                                                                   ELELCEML
01646      PERFORM 3320-UPDATE-DATA-ELEMENT THRU 3320-EXIT.             ELELCEML
01647                                                                   ELELCEML
01648      MOVE '8000' TO WS-PARA-ID.                                   ELELCEML
01649                                                                   ELELCEML
01650      MOVE SPACES     TO CA-MAPFROM-ELEMENT-NAME.                  ELELCEML
01651      MOVE LOW-VALUES TO CA-MAPFROM-KEYS.                          ELELCEML
01652      MOVE ECODEI TO CA-SEL-CODE-VALUE.                            ELELCEML
01653      MOVE ESEQI TO CA-SEL-CODE-SEQ-X.                             ELELCEML
01654      MOVE ENAMEI TO CA-SEL-CODE-NAME.                             ELELCEML
01655                                                                   ELELCEML
01656                                                                   ELELCEML
01657      MOVE MAP-LITERAL7   TO EERRMO.                               ELELCEML
01658      PERFORM 4020-REINITIALIZE-SCREEN THRU 4020-EXIT.             ELELCEML
01659      PERFORM 4000-CREATE-SCREEN THRU 4000-EXIT.                   ELELCEML
01660                                                                   ELELCEML
01661  8000-EXIT.  EXIT.                                                ELELCEML
01662 /                                                                 ELELCEML
01663  8025-DELETE-PRESENT-VALUES.                                      ELELCEML
01664                                                                   ELELCEML
01665      MOVE CA-SELECTED-CV-KEY   TO ELCIO-VSAM-KEY3.                ELELCEML
01666      MOVE EL-DSN-ELPCV         TO ELCIO-FILE-DDNAME3.             ELELCEML
01667      MOVE EL-ELPCV-REC-LEN     TO ELCIO-MAX-REC-LEN3.             ELELCEML
01668      MOVE ELPCV-KEY-LENGTH     TO ELCIO-BROWSE-KEYLEN3.           ELELCEML
01669      MOVE 'GTE'                TO ELCIO-CIO-QUAL3.                ELELCEML
01670      MOVE 'M'                  TO ELCIO-STORAGE3.                 ELELCEML
01671      MOVE 'RD '                TO ELCIO-FILE-ACCESS-CODE3.        ELELCEML
01672                                                                   ELELCEML
01673      SET  CIA-IO-PARM-AREA-PNTR  TO                               ELELCEML
01674           CIA-ELPCV-IOPARM-AREA-PNTR.                             ELELCEML
01675                                                                   ELELCEML
01676      SET  ELCIO-REC-AREA-ADDRESS3  TO                             ELELCEML
01677           CIA-ELPCV-REC-AREA-PNTR.                                ELELCEML
01678                                                                   ELELCEML
01679      EXEC CICS LINK                                               ELELCEML
01680                PROGRAM('ELAIOPGM')                                ELELCEML
01681                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCEML
01682                LENGTH(EL-CIA-POINTER-LEN)                         ELELCEML
01683      END-EXEC.                                                    ELELCEML
01684                                                                   ELELCEML
01685      IF ELCIO-REC-NOT-FOUND3                                      ELELCEML
01686          GO TO 8025-ENDFILE                                       ELELCEML
01687      ELSE                                                         ELELCEML
01688          IF NOT ELCIO-GOOD-RETURN3                                ELELCEML
01689              MOVE 'EE19' TO WS-ABEND-CODE                         ELELCEML
01690              GO TO 9999-ABEND.                                    ELELCEML
01691                                                                   ELELCEML
01692                                                                   ELELCEML
01693                                                                   ELELCEML
01694      IF CV-CODE-VALUE EQUAL CA-SEL-CODE-VALUE                     ELELCEML
01695              MOVE CV-CODE-DESC-SEQ TO CA-SEL-CODE-SEQ             ELELCEML
01696              MOVE CA-SELECTED-CV-KEY   TO ELCIO-VSAM-KEY3         ELELCEML
01697              MOVE 'DL '          TO ELCIO-FILE-ACCESS-CODE3       ELELCEML
01698                                                                   ELELCEML
01699              EXEC CICS LINK                                       ELELCEML
01700                        PROGRAM('ELAIOPGM')                        ELELCEML
01701                       COMMAREA(ADDRESS OF CIA-PARMS-RECORD)       ELELCEML
01702                        LENGTH(EL-CIA-POINTER-LEN)                 ELELCEML
01703              END-EXEC                                             ELELCEML
01704                                                                   ELELCEML
01705              IF NOT ELCIO-GOOD-RETURN3                            ELELCEML
01706                  MOVE 'EE20' TO WS-ABEND-CODE                     ELELCEML
01707                  GO TO 9999-ABEND                                 ELELCEML
01708              ELSE                                                 ELELCEML
01709                  ADD 1 TO CA-SEL-CODE-SEQ                         ELELCEML
01710                  GO TO 8025-DELETE-PRESENT-VALUES.                ELELCEML
01711  8025-ENDFILE.                                                    ELELCEML
01712      MOVE 'YES' TO DELETE-SW.                                     ELELCEML
01713  8025-EXIT.  EXIT.                                                ELELCEML
01714 /                                                                 ELELCEML
01715  8050-ADD-NEW-CODE-VALUE.                                         ELELCEML
01716 *************************************************************     ELELCEML
01717 *    THE ELEMENT CODE VALUE RECORD THAT WAS READ WILL HAVE  *     ELELCEML
01718 *    THE PRIMARY KEYS MODIFIED SO THAT THE REST OF THE      *     ELELCEML
01719 *    RECORD WILL CONTAIN THE \
01720 *    SEQUENCE NO. WILL REMAIN THE SAME.                     *     ELELCEML
01721 *************************************************************     ELELCEML
01722                                                                   ELELCEML
01723 ** INITIALIZE ELEMENT CODE VALUE RECORD **                        ELELCEML
01724      MOVE CA-SEL-RECORD-PREFIX TO CV-RECORD-PREFIX.               ELELCEML
01725      MOVE CA-SEL-ELEMENT-NBR   TO CV-ELEMENT-NBR.                 ELELCEML
01726      MOVE CA-SEL-CODE-VALUE    TO CV-CODE-VALUE.                  ELELCEML
01727                                                                   ELELCEML
01728      IF ENAMEL GREATER THAN ZERO                                  ELELCEML
01729          MOVE ENAMEI           TO CV-CODE-NAME.                   ELELCEML
01730                                                                   ELELCEML
01731      MOVE 'WR '                TO ELCIO-FILE-ACCESS-CODE3.        ELELCEML
01732      MOVE CV-CODE-DESC-SEQ     TO CA-SEL-CODE-SEQ.                ELELCEML
01733      MOVE CA-SELECTED-CV-KEY   TO  ELCIO-VSAM-KEY3.               ELELCEML
01734                                                                   ELELCEML
01735      EXEC CICS LINK                                               ELELCEML
01736                PROGRAM('ELAIOPGM')                                ELELCEML
01737                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCEML
01738                LENGTH(EL-CIA-POINTER-LEN)                         ELELCEML
01739      END-EXEC.                                                    ELELCEML
01740                                                                   ELELCEML
01741      IF NOT ELCIO-GOOD-RETURN3                                    ELELCEML
01742          MOVE 'EE21' TO WS-ABEND-CODE                             ELELCEML
01743          GO TO 9999-ABEND.                                        ELELCEML
01744                                                                   ELELCEML
01745      IF CA-FIRST-CODE EQUAL LOW-VALUES                            ELELCEML
01746          MOVE CV-CODE-KEY TO CA-FIRST-CODE.                       ELELCEML
01747                                                                   ELELCEML
01748                                                                   ELELCEML
01749      MOVE CV-CODE-DESC-SEQ TO CA-MF-CODE-SEQ.                     ELELCEML
01750      ADD 1 TO CA-MF-CODE-SEQ.                                     ELELCEML
01751      MOVE CA-MAPFROM-CV-KEY  TO ELCIO-VSAM-KEY3.                  ELELCEML
01752                                                                   ELELCEML
01753      MOVE 'RD '          TO ELCIO-FILE-ACCESS-CODE3.              ELELCEML
01754      MOVE 'GTE'          TO ELCIO-CIO-QUAL3.                      ELELCEML
01755                                                                   ELELCEML
01756      EXEC CICS LINK                                               ELELCEML
01757                PROGRAM('ELAIOPGM')                                ELELCEML
01758                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCEML
01759                LENGTH(EL-CIA-POINTER-LEN)                         ELELCEML
01760      END-EXEC.                                                    ELELCEML
01761                                                                   ELELCEML
01762      IF ELCIO-REC-NOT-FOUND3                                      ELELCEML
01763          GO TO 8050-ENDFILE                                       ELELCEML
01764      ELSE                                                         ELELCEML
01765          IF NOT ELCIO-GOOD-RETURN3                                ELELCEML
01766              MOVE 'EE22' TO WS-ABEND-CODE                         ELELCEML
01767              GO TO 9999-ABEND.                                    ELELCEML
01768                                                                   ELELCEML
01769      IF CV-CODE-VALUE EQUAL CA-MF-CODE-VALUE                      ELELCEML
01770          GO TO 8050-ADD-NEW-CODE-VALUE.                           ELELCEML
01771                                                                   ELELCEML
01772  8050-ENDFILE.                                                    ELELCEML
01773      MOVE 'YES' TO COPY-SW.                                       ELELCEML
01774  8050-EXIT.  EXIT.                                                ELELCEML
01775 /                                                                 ELELCEML
01776  9000-XCTL-ELELCAML.                                              ELELCEML
01777                                                                   ELELCEML
01778      PERFORM 1100-GET-STORAGE-FOR-ELPRL THRU 1100-EXIT.           ELELCEML
01779      PERFORM 1200-GET-STORAGE-FOR-ELPDE THRU 1200-EXIT.           ELELCEML
01780      PERFORM 1300-GET-STORAGE-FOR-ELPCV THRU 1300-EXIT.           ELELCEML
01781                                                                   ELELCEML
01782      MOVE 'E' TO CA-CURRENT-PGM.                                  ELELCEML
01783      MOVE SPACE TO CA-CURRENT-FUNCTION.                           ELELCEML
01784      MOVE LOW-VALUES TO CA-SELECTED-CV-KEY                        ELELCEML
01785                         CA-SELECTED-NAMES.                        ELELCEML
01786                                                                   ELELCEML
01787      EXEC CICS XCTL                                               ELELCEML
01788                PROGRAM('ELELCAML')                                ELELCEML
01789                COMMAREA(DFHCOMMAREA)                              ELELCEML
01790                LENGTH(COMM-LENGTH)                                ELELCEML
01791      END-EXEC.                                                    ELELCEML
01792  9010-XCTL-ELELCDML.                                              ELELCEML
01793                                                                   ELELCEML
01794      PERFORM 1100-GET-STORAGE-FOR-ELPRL THRU 1100-EXIT.           ELELCEML
01795      PERFORM 1200-GET-STORAGE-FOR-ELPDE THRU 1200-EXIT.           ELELCEML
01796      PERFORM 1300-GET-STORAGE-FOR-ELPCV THRU 1300-EXIT.           ELELCEML
01797                                                                   ELELCEML
01798      MOVE 'E' TO CA-CURRENT-PGM.                                  ELELCEML
01799      MOVE SPACE TO CA-CURRENT-FUNCTION.                           ELELCEML
01800      MOVE LOW-VALUES TO CA-SEL-CODE-VALUE                         ELELCEML
01801                         CA-SEL-CODE-NAME.                         ELELCEML
01802                                                                   ELELCEML
01803      EXEC CICS XCTL                                               ELELCEML
01804                PROGRAM('ELELCDML')                                ELELCEML
01805                COMMAREA(DFHCOMMAREA)                              ELELCEML
01806                LENGTH(COMM-LENGTH)                                ELELCEML
01807      END-EXEC.                                                    ELELCEML
01808  9999-MAPFAIL.                                                    ELELCEML
01809      MOVE ERR-07-MSG TO EERRMO.                                   ELELCEML
01810      MOVE 'MAPF' TO WS-ABEND-CODE.                                ELELCEML
01811      MOVE -1 TO EFCNL.                                            ELELCEML
01812      GO TO 9999-RETURN-WITH-MSG.                                  ELELCEML
01813                                                                   ELELCEML
01814  9999-RETURN-WITH-MSG.                                            ELELCEML
01815      MOVE 'E' TO CA-CURRENT-PGM.                                  ELELCEML
01816                                                                   ELELCEML
01817      EXEC CICS SEND MAP ('ELCEI01')                               ELELCEML
01818                MAPSET   ('ELCESET')                               ELELCEML
01819                DATAONLY                                           ELELCEML
01820                CURSOR                                             ELELCEML
01821      END-EXEC.                                                    ELELCEML
01822      EXEC CICS RETURN                                             ELELCEML
01823                TRANSID('ELCE')                                    ELELCEML
01824                COMMAREA(DFHCOMMAREA)                              ELELCEML
01825                LENGTH(COMM-LENGTH)                                ELELCEML
01826      END-EXEC.                                                    ELELCEML
01827                                                                   ELELCEML
01828  9999-ABEND.                                                      ELELCEML
01829      EXEC CICS ABEND                                              ELELCEML
01830                ABCODE(WS-ABEND-CODE)                              ELELCEML
01831      END-EXEC.                                                    ELELCEML
01832                                                                   ELELCEML
