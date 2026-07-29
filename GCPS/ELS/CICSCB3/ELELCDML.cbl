00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID.    ELELCDML.                                         ELELCDML
00003  AUTHOR.        NINA CERVANTES.                                      LV001
00004  INSTALLATION.  BCBS/HCMS.                                        ELELCDML
00005  DATE-WRITTEN.  10/85.                                            ELELCDML
00006  DATE-COMPILED.   /  /  .                                         ELELCDML
00007 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ELELCDML
00008 *       ||||||| ||||||||  ||||||||  ||||||||  |||   ||||||||||  * ELELCDML
00009 *     |||      |||   ||| |||   ||| |||   ||| |||     |||  |||   * ELELCDML
00010 *    |||      |||   ||| |||   ||| |||   ||| |||      |||  |||   * ELELCDML
00011 *   |||      |||   ||| ||||||||| |||   ||| |||       |||  |||   * ELELCDML
00012 *  |||      |||   ||| |||   ||| |||   ||| |||        |||  |||   * ELELCDML
00013 * |||      |||   ||| |||   ||| |||   ||| |||         |||  |||   * ELELCDML
00014 * |||||||| |||||||| ||||||||| ||||||||| |||||||||   ||||||||||  * ELELCDML
00015 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ELELCDML
00016                                                                   ELELCDML
00017 /**************************************************************** ELELCDML
00018 *            ELELCDML - ELS: SELECT CODE VALUE                  * ELELCDML
00019 *                                                               * ELELCDML
00020 *    THIS MODULE IS RESPONSIBLE FOR LISTING ALL CODE VALUES     * ELELCDML
00021 *    WITHIN A GIVEN ELEMENT. THE FOLLOWING FUNCTIONS COULD THEN * ELELCDML
00022 *    BE PERFORMED:                                              * ELELCDML
00023 *                                                               * ELELCDML
00024 *    (1)  SELECT ACTUAL CODE VALUE FOR MAINTENCE BY SUBSEQUENT  * ELELCDML
00025 *         SCREEN.  THIS CAN BE DONE BY 2 METHODS:               * ELELCDML
00026 *         (A)  USER MAY ENTER 'S' FOR FUNCTION CODE             * ELELCDML
00027 *         (B)  USER MAY ENTER 'SELECT CODE VALUE';      THE     * ELELCDML
00028 *              CODE VALUE     MUST NOT BE IN DELETE STATUS AND  * ELELCDML
00029 *              IT MUST EXIST ON FILE.                           * ELELCDML
00030 *                                                               * ELELCDML
00031 *    (2)  UNDELETE CODE VALUE FROM DELETE STATUS.    THE STATUS * ELELCDML
00032 *         MUST BE IN DELETE STATUS ALREADY.  THE USER MAY ENTER * ELELCDML
00033 *         'U' FOR FUNCTION CODE.  THE MODULE WILL FIRST SEND    * ELELCDML
00034 *         BACK THE SCREEN WITH A MESSAGE FOR THE USER TO HIT THE* ELELCDML
00035 *         PF6 KEY FOR CONFIRMATION AND THEN A SECOND SCREEN WILL* ELELCDML
00036 *         BE SENT TO VERIFY THAT ALL WENT WELL.                 * ELELCDML
00037 *                                                               * ELELCDML
00038 *    (3)  PF3 KEY    TRANSFER CONTROL TO ELELCCML.              * ELELCDML
00039 *                                                               * ELELCDML
00040 *    (4)  PF4 KEY    TRANSFER CONTROL TO ELELCEML; INDICATES A  * ELELCDML
00041 *                    NEW CODE VALUE WILL BE ADDED FOR THIS      * ELELCDML
00042 *                    ELEMENT.                                   * ELELCDML
00043 *                                                               * ELELCDML
00044 *    (5)  PF7/PF8    BACKWARD/FORWARD POSITIONING.              * ELELCDML
00045 *                                                               * ELELCDML
00046 *    (6)  PF9 KEY    TRANSFER CONTROL TO ELELCAML.              * ELELCDML
00047 *                                                               * ELELCDML
00048 *    (7)  CLEAR      RETURN TO CICS.                            * ELELCDML
00049 *                                                               * ELELCDML
00050 ***************************************************************** ELELCDML
00051 /                                                                 ELELCDML
00052  ENVIRONMENT DIVISION.                                            ELELCDML
00053  DATA DIVISION.                                                   ELELCDML
00054  WORKING-STORAGE SECTION.                                         ELELCDML
00055  01  FILLER                 PIC X(42)        VALUE                ELELCDML
00056      '***ELELCDML WORKING STORAGE BEGINS HERE***'.                ELELCDML
00057  01  WS-PARA-ID             PIC X(4)         VALUE 'XXXX'.        ELELCDML
00058  01  WS-ABEND-CODE          PIC X(4)         VALUE 'XXXX'.        ELELCDML
00059                                                                   ELELCDML
00060 ** FILE PARMS**                                                   ELELCDML
00061  01  ELPCV-KEY-LENGTH        PIC S9(4)   COMP VALUE +023.         ELELCDML
00062  01  ELPCV-GEN-KEY-LENGTH    PIC S9(4)   COMP VALUE +021.         ELELCDML
00063  01  COMM-LENGTH             PIC S9(4)   COMP VALUE +500.         ELELCDML
00064  01  RLEN.                                                        ELELCDML
00065      COPY ELCDRLEN.                                               ELELCDML
00066                                                                   ELELCDML
00067 /* SWITCHES **                                                    ELELCDML
00068  01  SCREEN-SW               PIC XXX   VALUE 'NO '.               ELELCDML
00069      88  SCREEN-BUILT                  VALUE 'YES'.               ELELCDML
00070  01  FUNC-SW                 PIC XXX   VALUE 'NO '.               ELELCDML
00071      88  FUNCTION-ENTERED              VALUE 'YES'.               ELELCDML
00072  01  CVALUE-SW               PIC XXX   VALUE 'NO '.               ELELCDML
00073      88  CVALUE-ENTERED                VALUE 'YES'.               ELELCDML
00074  01  REPOSITION-SW           PIC XXX   VALUE 'NO '.               ELELCDML
00075      88  REPOSITION-INDICATED          VALUE 'YES'.               ELELCDML
00076  01  UNDELETE-SW             PIC XXX   VALUE 'NO '.               ELELCDML
00077      88  UNDELETED-ALL-VALUE-SEQS      VALUE 'YES'.               ELELCDML
00078  01  ENCOUNTER-SW            PIC XXX   VALUE 'NO '.               ELELCDML
00079      88  ENCOUNTERED-ANOTHER           VALUE 'YES'.               ELELCDML
00080                                                                   ELELCDML
00081 /* WORK AREAS**                                                   ELELCDML
00082  01  SUBA                   PIC 99.                               ELELCDML
00083  01  SUBB                   PIC 99.                               ELELCDML
00084  01  WS-REFORMAT-ELEMENT    PIC ZZ9.99.                           ELELCDML
00085  01  WS-SAVE-FUNCTION       PIC X               VALUE SPACE.      ELELCDML
00086      88  SELECT-INDICATED        VALUE 'S'.                       ELELCDML
00087      88  UNDELETE-INDICATED      VALUE 'U'.                       ELELCDML
00088  01  WS-SAVE-SUBA           PIC 99              VALUE ZERO.       ELELCDML
00089  01  WS-SAVE-CVALUE-KEY     PIC X(10)           VALUE SPACES.     ELELCDML
00090  01  WS-SAVE-CDESC-SEQ      PIC 99              VALUE ZERO.       ELELCDML
00091  01  WS-HEX-00              PIC X               VALUE LOW-VALUES. ELELCDML
00092                                                                   ELELCDML
00093 /* MESSAGES **                                                    ELELCDML
00094  01  MAP-LITERAL1           PIC X(79)  VALUE                      ELELCDML
00095      '(PF3=ELEM DEF)(PF4=NEW CODE VALUE)(PF7/8=UP/DOWN)(PF9=RECORDELELCDML
00096 -    'SEL)(CLEAR=EXIT)'.                                          ELELCDML
00097  01  MAP-LITERAL2           PIC X(79)  VALUE                      ELELCDML
00098      'UNDELETE FUNCTION WAS SUCCESSFUL'.                          ELELCDML
00099                                                                   ELELCDML
00100  01  MAP-LITERAL3           PIC X(79)  VALUE                      ELELCDML
00101      'PLEASE ENTER PF6 TO CONFIRM UNDELETE'.                      ELELCDML
00102                                                                   ELELCDML
00103  01  MAP-LITERAL4           PIC X(79)  VALUE                      ELELCDML
00104      '(PF3=ELEM DEF)(PF7/8=UP/DOWN)(PF9=RECORD LIST)(CLEAR=EXIT)'.ELELCDML
00105                                                                   ELELCDML
00106  01  MAP-LITERAL5           PIC X(79)  VALUE                      ELELCDML
00107      'FCN IS (S)ELECT (U)NDELETE'.                                ELELCDML
00108                                                                   ELELCDML
00109  01  MAP-LITERAL6           PIC X(79)  VALUE                      ELELCDML
00110      'FCN IS (S)ELECT '.                                          ELELCDML
00111                                                                   ELELCDML
00112  01  ERR-03-MSG.                                                  ELELCDML
00113      03  FILLER             PIC X(31)  VALUE SPACES.              ELELCDML
00114      03  FILLER             PIC X(16)  VALUE                      ELELCDML
00115      'END OF RETRIEVAL'.                                          ELELCDML
00116      03  FILLER             PIC X(32)  VALUE SPACES.              ELELCDML
00117                                                                   ELELCDML
00118  01  ERR-04-MSG.                                                  ELELCDML
00119      03  FILLER             PIC X(34)  VALUE SPACES.              ELELCDML
00120      03  FILLER             PIC X(10)  VALUE                      ELELCDML
00121      'FIRST PAGE'.                                                ELELCDML
00122      03  FILLER             PIC X(35)  VALUE SPACES.              ELELCDML
00123                                                                   ELELCDML
00124  01  ERR-05-MSG             PIC X(79)  VALUE                      ELELCDML
00125      'PLEASE INDICATE WHICH FUNCTION IS TO BE PERFORMED'.         ELELCDML
00126                                                                   ELELCDML
00127  01  ERR-06-MSG             PIC X(79)  VALUE                      ELELCDML
00128      'INVALID FUNCTION CODE ENTERED'.                             ELELCDML
00129                                                                   ELELCDML
00130  01  ERR-07-MSG             PIC X(79)  VALUE                      ELELCDML
00131      'SELECT NOT ALLOWED FOR CODE VALUE IN DELETE STATUS'.        ELELCDML
00132                                                                   ELELCDML
00133  01  ERR-08-MSG             PIC X(79)  VALUE                      ELELCDML
00134      'UNDELETE ALLOWED FOR CODE VALUE IN DELETE STATUS ONLY'.     ELELCDML
00135                                                                   ELELCDML
00136  01  ERR-09-MSG             PIC X(79)  VALUE                      ELELCDML
00137      'ENTER ONLY ONE FUNCTION CODE AT A TIME'.                    ELELCDML
00138                                                                   ELELCDML
00139  01  ERR-10-MSG             PIC X(79)  VALUE                      ELELCDML
00140      'ENTER FUNCTION CODE OR CODE VALUE NUMBER - NOT BOTH'.       ELELCDML
00141                                                                   ELELCDML
00142  01  ERR-11-MSG             PIC X(79)  VALUE                      ELELCDML
00143      'CODE VALUE FOR FUNCTION ENTERED CAN NOT BE FOUND'.          ELELCDML
00144                                                                   ELELCDML
00145  01  ERR-12-MSG             PIC X(79)  VALUE                      ELELCDML
00146      'MAPFAIL - PLEASE NOTIFY SYSTEMS GROUP'.                     ELELCDML
00147                                                                   ELELCDML
00148  01  ERR-13-MSG             PIC X(79)  VALUE                      ELELCDML
00149      'INQUIRY MODE ONLY - NO OTHER FUNCTIONS ALLOWED'.            ELELCDML
00150                                                                   ELELCDML
00151 /* MAP AREA **                                                    ELELCDML
00152  COPY ELCDSETC.                                                   ELELCDML
00153  01  FILLER  REDEFINES  ELCDI01I.                                 ELELCDML
00154      03  FILLER             PIC X(214).                           ELELCDML
00155      03  MAP-GRP  OCCURS  14  TIMES.                              ELELCDML
00156          05  FUNCL          PIC S9(4) COMP.                       ELELCDML
00157          05  FUNCA          PIC X.                                ELELCDML
00158          05  FUNC           PIC X.                                ELELCDML
00159          05  CVALL          PIC S9(4) COMP.                       ELELCDML
00160          05  CVALA          PIC X.                                ELELCDML
00161          05  CVAL           PIC X(10).                            ELELCDML
00162          05  FILLER         PIC X.                                ELELCDML
00163          05  NAMEL          PIC S9(4) COMP.                       ELELCDML
00164          05  NAMEA          PIC X.                                ELELCDML
00165          05  NAME           PIC X(50).                            ELELCDML
00166          05  FILLER         PIC X.                                ELELCDML
00167          05  STATL          PIC S9(4) COMP.                       ELELCDML
00168          05  STATA          PIC X.                                ELELCDML
00169          05  STAT           PIC X(7).                             ELELCDML
00170      03  FILLER             PIC X(96).                            ELELCDML
00171                                                                   ELELCDML
00172 /* ATTRIBUTES **                                                  ELELCDML
00173  COPY DFHBMSCA.                                                   ELELCDML
00174      02  DFHBMADF                PIC X VALUE 'Z'.                 ELELCDML
00175                                                                   ELELCDML
00176 /* ATTENTION IDENTIFIERS **                                       ELELCDML
00177  COPY DFHAID.                                                     ELELCDML
00178  01  FILLER                 PIC X(31)         VALUE               ELELCDML
00179      '***WORKING STORAGE ENDS HERE***'.                           ELELCDML
00180 /                                                                 ELELCDML
00181  LINKAGE SECTION.                                                 ELELCDML
00182  01  DFHCOMMAREA.                                                 ELELCDML
00183  COPY ELPCOMMC.                                                   ELELCDML
00184  01  CIA-PARMS-RECORD.                                            ELELCDML
00185      COPY ELCDCIA.                                                ELELCDML
00186 /                                                                 ELELCDML
00187  01  IOPARM-RECORD-LIST.                                          ELELCDML
00188      COPY ELCDIOPM.                                               ELELCDML
00189  01  EL-RECORD-LIST.                                              ELELCDML
00190      COPY ELPRLC.                                                 ELELCDML
00191 /                                                                 ELELCDML
00192  01  IOPARM-DATA-ELEMENT.                                         ELELCDML
00193      COPY ELCDIOP2.                                               ELELCDML
00194  01  EL-DATA-ELEMENT.                                             ELELCDML
00195      COPY ELPDEC                                                  ELELCDML
00196      REPLACING == OCCURS 1 TO 11 ==                               ELELCDML
00197             BY == OCCURS      11 ==                               ELELCDML
00198                == DEPENDING ON DE-NBR-DESC-LINES ==               ELELCDML
00199             BY ==                                ==.              ELELCDML
00200 /                                                                 ELELCDML
00201  01  IOPARM-CODE-VALUE.                                           ELELCDML
00202      COPY ELCDIOP3.                                               ELELCDML
00203  01  EL-CODE-VALUE.                                               ELELCDML
00204      COPY ELPCVC                                                  ELELCDML
00205      REPLACING == OCCURS 1 TO 12 TIMES ==                         ELELCDML
00206             BY == OCCURS      12 TIMES. ==                        ELELCDML
00207                == DEPENDING ON    ==                              ELELCDML
00208             BY ==                 ==                              ELELCDML
00209                == CV-NBR-VALUE-DESC-LINES. ==                     ELELCDML
00210             BY ==                          ==.                    ELELCDML
00211 /                                                                 ELELCDML
00212  PROCEDURE DIVISION.                                              ELELCDML
00213      PERFORM 1000-HOUSEKEEPING THRU 1000-EXIT.                    ELELCDML
00214      PERFORM 2000-MAINLINE-PROCESSING THRU 2000-EXIT.             ELELCDML
00215  0000-RETURN.                                                     ELELCDML
00216      EXEC CICS RETURN                                             ELELCDML
00217      END-EXEC.                                                    ELELCDML
00218      GOBACK.                                                      ELELCDML
00219 /                                                                 ELELCDML
00220  1000-HOUSEKEEPING.                                               ELELCDML
00221      MOVE '1000' TO WS-PARA-ID.                                   ELELCDML
00222                                                                   ELELCDML
00223      IF CA-SELECT-CV                                              ELELCDML
00224          EXEC CICS GETMAIN                                        ELELCDML
00225                    SET(ADDRESS OF CIA-PARMS-RECORD)               ELELCDML
00226                    INITIMG(WS-HEX-00)                             ELELCDML
00227                    LENGTH(EL-CIA-REC-REC-LEN)                     ELELCDML
00228          END-EXEC                                                 ELELCDML
00229          SET  CA-CIA-POINTER  TO                                  ELELCDML
00230               ADDRESS OF CIA-PARMS-RECORD                         ELELCDML
00231      ELSE                                                         ELELCDML
00232          SET  ADDRESS OF CIA-PARMS-RECORD     TO                  ELELCDML
00233               CA-CIA-POINTER.                                     ELELCDML
00234                                                                   ELELCDML
00235                                                                   ELELCDML
00236      MOVE LOW-VALUES TO ELCDI01I.                                 ELELCDML
00237                                                                   ELELCDML
00238      IF EIBCALEN EQUAL ZEROES                                     ELELCDML
00239          MOVE 'EC00' TO WS-ABEND-CODE                             ELELCDML
00240          GO TO 9999-ABEND.                                        ELELCDML
00241                                                                   ELELCDML
00242      EXEC CICS HANDLE AID                                         ELELCDML
00243                CLEAR(0000-RETURN)                                 ELELCDML
00244                PF3  (9010-XCTL-ELELCCML)                          ELELCDML
00245                PF4  (9011-XCTL-ELELCEML)                          ELELCDML
00246                PF7  (5000-BACKWARD-READ)                          ELELCDML
00247                PF8  (6000-FORWARD-READ)                           ELELCDML
00248                PF9  (9000-XCTL-ELELCAML)                          ELELCDML
00249      END-EXEC.                                                    ELELCDML
00250                                                                   ELELCDML
00251      EXEC CICS HANDLE AID                                         ELELCDML
00252                CLEAR(0000-RETURN)                                 ELELCDML
00253                PF15 (9010-XCTL-ELELCCML)                          ELELCDML
00254                PF16 (9011-XCTL-ELELCEML)                          ELELCDML
00255                PF19 (5000-BACKWARD-READ)                          ELELCDML
00256                PF20 (6000-FORWARD-READ)                           ELELCDML
00257                PF21 (9000-XCTL-ELELCAML)                          ELELCDML
00258      END-EXEC.                                                    ELELCDML
00259      EXEC CICS HANDLE CONDITION                                   ELELCDML
00260                MAPFAIL(9999-MAPFAIL)                              ELELCDML
00261      END-EXEC.                                                    ELELCDML
00262                                                                   ELELCDML
00263  1000-EXIT.  EXIT.                                                ELELCDML
00264 /                                                                 ELELCDML
00265  1100-GET-STORAGE-FOR-ELPRL.                                      ELELCDML
00266      MOVE '1100'       TO WS-PARA-ID.                             ELELCDML
00267                                                                   ELELCDML
00268      EXEC CICS GETMAIN                                            ELELCDML
00269                SET(ADDRESS OF IOPARM-RECORD-LIST)                 ELELCDML
00270                INITIMG(WS-HEX-00)                                 ELELCDML
00271                LENGTH(EL-IOPARMS-REC-LEN)                         ELELCDML
00272      END-EXEC.                                                    ELELCDML
00273                                                                   ELELCDML
00274      SET  CIA-ELPRL-IOPARM-AREA-PNTR  TO                          ELELCDML
00275           ADDRESS OF IOPARM-RECORD-LIST.                          ELELCDML
00276                                                                   ELELCDML
00277      SET  CIA-IO-PARM-AREA-PNTR       TO                          ELELCDML
00278           CIA-ELPRL-IOPARM-AREA-PNTR.                             ELELCDML
00279                                                                   ELELCDML
00280      EXEC CICS GETMAIN                                            ELELCDML
00281                SET(ADDRESS OF EL-RECORD-LIST)                     ELELCDML
00282                INITIMG(WS-HEX-00)                                 ELELCDML
00283                LENGTH(EL-ELPRL-REC-LEN)                           ELELCDML
00284      END-EXEC.                                                    ELELCDML
00285                                                                   ELELCDML
00286      SET  CIA-ELPRL-REC-AREA-PNTR     TO                          ELELCDML
00287           ADDRESS OF EL-RECORD-LIST.                              ELELCDML
00288                                                                   ELELCDML
00289  1100-EXIT.  EXIT.                                                ELELCDML
00290                                                                   ELELCDML
00291  1150-SET-ADDRESS-OF-ELPRL.                                       ELELCDML
00292      MOVE '1150'       TO WS-PARA-ID.                             ELELCDML
00293                                                                   ELELCDML
00294      SET  ADDRESS OF IOPARM-RECORD-LIST     TO                    ELELCDML
00295           CIA-ELPRL-IOPARM-AREA-PNTR.                             ELELCDML
00296                                                                   ELELCDML
00297      SET  CIA-IO-PARM-AREA-PNTR             TO                    ELELCDML
00298           CIA-ELPRL-IOPARM-AREA-PNTR.                             ELELCDML
00299                                                                   ELELCDML
00300      SET  ADDRESS OF EL-RECORD-LIST         TO                    ELELCDML
00301           CIA-ELPRL-REC-AREA-PNTR.                                ELELCDML
00302                                                                   ELELCDML
00303  1150-EXIT.  EXIT.                                                ELELCDML
00304 /                                                                 ELELCDML
00305  1200-GET-STORAGE-FOR-ELPDE.                                      ELELCDML
00306      MOVE '1200' TO WS-PARA-ID.                                   ELELCDML
00307                                                                   ELELCDML
00308      EXEC CICS GETMAIN                                            ELELCDML
00309                SET(ADDRESS OF IOPARM-DATA-ELEMENT)                ELELCDML
00310                INITIMG(WS-HEX-00)                                 ELELCDML
00311                LENGTH(EL-IOPARMS-REC-LEN)                         ELELCDML
00312      END-EXEC.                                                    ELELCDML
00313                                                                   ELELCDML
00314      SET  CIA-ELPDE-IOPARM-AREA-PNTR  TO                          ELELCDML
00315           ADDRESS OF IOPARM-DATA-ELEMENT.                         ELELCDML
00316                                                                   ELELCDML
00317      SET  CIA-IO-PARM-AREA-PNTR       TO                          ELELCDML
00318           CIA-ELPDE-IOPARM-AREA-PNTR.                             ELELCDML
00319                                                                   ELELCDML
00320      MOVE EL-DSN-ELPDE TO CIA-IO-GETMAIN-DDNAME.                  ELELCDML
00321                                                                   ELELCDML
00322      EXEC CICS GETMAIN                                            ELELCDML
00323                SET(ADDRESS OF EL-DATA-ELEMENT)                    ELELCDML
00324                INITIMG(WS-HEX-00)                                 ELELCDML
00325                LENGTH(EL-ELPDE-REC-LEN)                           ELELCDML
00326      END-EXEC.                                                    ELELCDML
00327                                                                   ELELCDML
00328      SET  CIA-ELPDE-REC-AREA-PNTR     TO                          ELELCDML
00329           ADDRESS OF EL-DATA-ELEMENT.                             ELELCDML
00330                                                                   ELELCDML
00331  1200-EXIT.  EXIT.                                                ELELCDML
00332                                                                   ELELCDML
00333  1250-SET-ADDRESS-OF-ELPDE.                                       ELELCDML
00334      MOVE '1250' TO WS-PARA-ID.                                   ELELCDML
00335                                                                   ELELCDML
00336      SET  ADDRESS OF   IOPARM-DATA-ELEMENT  TO                    ELELCDML
00337           CIA-ELPDE-IOPARM-AREA-PNTR.                             ELELCDML
00338                                                                   ELELCDML
00339      SET  CIA-IO-PARM-AREA-PNTR             TO                    ELELCDML
00340           CIA-ELPDE-IOPARM-AREA-PNTR.                             ELELCDML
00341                                                                   ELELCDML
00342      SET  ADDRESS OF   EL-DATA-ELEMENT      TO                    ELELCDML
00343           CIA-ELPDE-REC-AREA-PNTR.                                ELELCDML
00344                                                                   ELELCDML
00345  1250-EXIT.  EXIT.                                                ELELCDML
00346 /                                                                 ELELCDML
00347  1300-GET-STORAGE-FOR-ELPCV.                                      ELELCDML
00348      MOVE '1300' TO WS-PARA-ID.                                   ELELCDML
00349                                                                   ELELCDML
00350      EXEC CICS GETMAIN                                            ELELCDML
00351                SET(ADDRESS OF IOPARM-CODE-VALUE)                  ELELCDML
00352                INITIMG(WS-HEX-00)                                 ELELCDML
00353                LENGTH(EL-IOPARMS-REC-LEN)                         ELELCDML
00354      END-EXEC.                                                    ELELCDML
00355                                                                   ELELCDML
00356      SET  CIA-ELPCV-IOPARM-AREA-PNTR  TO                          ELELCDML
00357           ADDRESS OF IOPARM-CODE-VALUE.                           ELELCDML
00358                                                                   ELELCDML
00359      SET  CIA-IO-PARM-AREA-PNTR       TO                          ELELCDML
00360           CIA-ELPCV-IOPARM-AREA-PNTR.                             ELELCDML
00361                                                                   ELELCDML
00362      MOVE EL-DSN-ELPCV TO CIA-IO-GETMAIN-DDNAME.                  ELELCDML
00363                                                                   ELELCDML
00364      EXEC CICS GETMAIN                                            ELELCDML
00365                SET(ADDRESS OF EL-CODE-VALUE)                      ELELCDML
00366                INITIMG(WS-HEX-00)                                 ELELCDML
00367                LENGTH(EL-ELPCV-REC-LEN)                           ELELCDML
00368      END-EXEC.                                                    ELELCDML
00369                                                                   ELELCDML
00370      SET  CIA-ELPCV-REC-AREA-PNTR     TO                          ELELCDML
00371           ADDRESS OF EL-CODE-VALUE.                               ELELCDML
00372                                                                   ELELCDML
00373      MOVE EL-DSN-ELPCV TO CIA-IO-GETMAIN-DDNAME.                  ELELCDML
00374                                                                   ELELCDML
00375  1300-EXIT.  EXIT.                                                ELELCDML
00376                                                                   ELELCDML
00377  1350-SET-ADDRESS-OF-ELPCV.                                       ELELCDML
00378      MOVE '1350' TO WS-PARA-ID.                                   ELELCDML
00379                                                                   ELELCDML
00380      SET  ADDRESS OF   IOPARM-CODE-VALUE    TO                    ELELCDML
00381           CIA-ELPCV-IOPARM-AREA-PNTR.                             ELELCDML
00382                                                                   ELELCDML
00383      SET  CIA-IO-PARM-AREA-PNTR             TO                    ELELCDML
00384           CIA-ELPCV-IOPARM-AREA-PNTR.                             ELELCDML
00385                                                                   ELELCDML
00386      SET  ADDRESS OF   EL-CODE-VALUE        TO                    ELELCDML
00387           CIA-ELPCV-REC-AREA-PNTR.                                ELELCDML
00388                                                                   ELELCDML
00389  1350-EXIT.  EXIT.                                                ELELCDML
00390 /                                                                 ELELCDML
00391  2000-MAINLINE-PROCESSING.                                        ELELCDML
00392      MOVE '2000' TO WS-PARA-ID.                                   ELELCDML
00393      IF CA-DE-DEFINE                                              ELELCDML
00394          PERFORM 4000-CREATE-SCREEN THRU 4000-EXIT                ELELCDML
00395      ELSE                                                         ELELCDML
00396        IF CA-CV-EDIT                                              ELELCDML
00397          IF CA-FIRST-CODE EQUAL LOW-VALUES                        ELELCDML
00398              GO TO 9010-XCTL-ELELCCML                             ELELCDML
00399          ELSE                                                     ELELCDML
00400              PERFORM 4000-CREATE-SCREEN THRU 4000-EXIT            ELELCDML
00401        ELSE                                                       ELELCDML
00402          IF CA-SELECT-CV                                          ELELCDML
00403              PERFORM 3000-RECEIVE-SCREEN THRU 3000-EXIT           ELELCDML
00404          ELSE                                                     ELELCDML
00405              MOVE 'ED01' TO WS-ABEND-CODE                         ELELCDML
00406              GO TO 9999-ABEND.                                    ELELCDML
00407  2000-EXIT.  EXIT.                                                ELELCDML
00408 /                                                                 ELELCDML
00409  3000-RECEIVE-SCREEN.                                             ELELCDML
00410      MOVE '3000' TO WS-PARA-ID.                                   ELELCDML
00411                                                                   ELELCDML
00412      EXEC CICS RECEIVE MAP('ELCDI01')                             ELELCDML
00413                MAPSET     ('ELCDSET')                             ELELCDML
00414      END-EXEC.                                                    ELELCDML
00415                                                                   ELELCDML
00416      PERFORM 1100-GET-STORAGE-FOR-ELPRL THRU 1100-EXIT.           ELELCDML
00417      PERFORM 1200-GET-STORAGE-FOR-ELPDE THRU 1200-EXIT.           ELELCDML
00418      PERFORM 1300-GET-STORAGE-FOR-ELPCV THRU 1300-EXIT.           ELELCDML
00419                                                                   ELELCDML
00420      IF EIBAID EQUAL DFHENTER  OR                                 ELELCDML
00421                      DFHPF6  OR  DFHPF18                          ELELCDML
00422          NEXT SENTENCE                                            ELELCDML
00423      ELSE                                                         ELELCDML
00424          MOVE ERR-05-MSG TO DERRMO                                ELELCDML
00425          MOVE -1 TO DFCN1L                                        ELELCDML
00426          GO TO   9999-RETURN-WITH-MSG.                            ELELCDML
00427                                                                   ELELCDML
00428 ** VERIFY THAT THE CORRECT MAP WAS RECEIVED **                    ELELCDML
00429      IF DFUNCI NOT EQUAL 'ELCD'                                   ELELCDML
00430          GO TO 9000-XCTL-ELELCAML.                                ELELCDML
00431                                                                   ELELCDML
00432                                                                   ELELCDML
00433      MOVE '3100' TO WS-PARA-ID.                                   ELELCDML
00434      PERFORM 3100-EDIT-SCREEN THRU 3100-EXIT                      ELELCDML
00435          VARYING SUBA FROM 1 BY 1                                 ELELCDML
00436          UNTIL SUBA GREATER THAN 14.                              ELELCDML
00437                                                                   ELELCDML
00438      MOVE '3000' TO WS-PARA-ID.                                   ELELCDML
00439                                                                   ELELCDML
00440      IF DSCODL GREATER THAN ZERO                                  ELELCDML
00441          MOVE 'YES' TO CVALUE-SW.                                 ELELCDML
00442                                                                   ELELCDML
00443      IF UNDELETE-INDICATED  AND  CA-INQUIRY                       ELELCDML
00444          MOVE ERR-13-MSG   TO DERRMO                              ELELCDML
00445          MOVE -1 TO FUNCL (WS-SAVE-SUBA)                          ELELCDML
00446          MOVE DFHBMUBF TO FUNCA (WS-SAVE-SUBA)                    ELELCDML
00447          GO TO 9999-RETURN-WITH-MSG.                              ELELCDML
00448                                                                   ELELCDML
00449 ** CHECK IF PF6/PF18 WAS ENTERED FOR CONFIRMATION **              ELELCDML
00450                                                                   ELELCDML
00451      IF EIBAID EQUAL DFHPF6  OR DFHPF18                           ELELCDML
00452         IF CA-SEL-CODE-VALUE EQUAL WS-SAVE-CVALUE-KEY AND         ELELCDML
00453            CA-CURRENT-FUNCTION EQUAL WS-SAVE-FUNCTION AND         ELELCDML
00454            UNDELETE-INDICATED                         AND         ELELCDML
00455            NOT CVALUE-ENTERED                                     ELELCDML
00456             PERFORM 3200-UNDELETE-FUNCTION THRU 3200-EXIT         ELELCDML
00457         ELSE                                                      ELELCDML
00458             MOVE -1 TO FUNCL (1)                                  ELELCDML
00459             MOVE ERR-05-MSG TO DERRMO                             ELELCDML
00460             GO TO 9999-RETURN-WITH-MSG.                           ELELCDML
00461                                                                   ELELCDML
00462      IF CVALUE-ENTERED                                            ELELCDML
00463          MOVE DSCODI TO CA-SEL-CODE-VALUE.                        ELELCDML
00464                                                                   ELELCDML
00465      IF FUNCTION-ENTERED                                          ELELCDML
00466          MOVE WS-SAVE-CVALUE-KEY TO CA-SEL-CODE-VALUE.            ELELCDML
00467                                                                   ELELCDML
00468 ** VERIFY THAT CODE VALUE EXISTS ON FILE **                       ELELCDML
00469      IF FUNCTION-ENTERED  OR                                      ELELCDML
00470         CVALUE-ENTERED                                            ELELCDML
00471          MOVE EL-DSN-ELPCV     TO ELCIO-FILE-DDNAME3              ELELCDML
00472          MOVE EL-ELPCV-REC-LEN TO ELCIO-MAX-REC-LEN3              ELELCDML
00473          MOVE 'GTE'            TO ELCIO-CIO-QUAL3                 ELELCDML
00474          MOVE ELPCV-KEY-LENGTH TO ELCIO-BROWSE-KEYLEN3            ELELCDML
00475          MOVE 'M'              TO ELCIO-STORAGE3                  ELELCDML
00476          MOVE LOW-VALUES         TO CA-SEL-CODE-SEQ-X             ELELCDML
00477          MOVE CA-SELECTED-CV-KEY TO ELCIO-VSAM-KEY3               ELELCDML
00478          MOVE 'RD '              TO ELCIO-FILE-ACCESS-CODE3       ELELCDML
00479          SET  ELCIO-REC-AREA-ADDRESS3  TO CIA-ELPCV-REC-AREA-PNTR ELELCDML
00480          SET  CIA-IO-PARM-AREA-PNTR    TO                         ELELCDML
00481               CIA-ELPCV-IOPARM-AREA-PNTR                          ELELCDML
00482                                                                   ELELCDML
00483          EXEC CICS LINK                                           ELELCDML
00484                    PROGRAM('ELAIOPGM')                            ELELCDML
00485                    COMMAREA(ADDRESS OF CIA-PARMS-RECORD)          ELELCDML
00486                    LENGTH(EL-CIA-POINTER-LEN)                     ELELCDML
00487          END-EXEC                                                 ELELCDML
00488                                                                   ELELCDML
00489          IF ELCIO-REC-NOT-FOUND3                                  ELELCDML
00490              GO TO 9999-NOTFND                                    ELELCDML
00491          ELSE                                                     ELELCDML
00492              IF NOT ELCIO-GOOD-RETURN3                            ELELCDML
00493                  MOVE 'ED02' TO WS-ABEND-CODE                     ELELCDML
00494                  GO TO 9999-ABEND.                                ELELCDML
00495                                                                   ELELCDML
00496                                                                   ELELCDML
00497      IF FUNCTION-ENTERED                                          ELELCDML
00498          IF SELECT-INDICATED                                      ELELCDML
00499              MOVE CV-CODE-VALUE TO CA-SEL-CODE-VALUE              ELELCDML
00500              MOVE CV-CODE-NAME   TO CA-SEL-CODE-NAME              ELELCDML
00501              MOVE CV-CODE-DESC-SEQ TO CA-SEL-CODE-SEQ-X           ELELCDML
00502              GO TO 9011-XCTL-ELELCEML                             ELELCDML
00503          ELSE                                                     ELELCDML
00504              MOVE CV-CODE-VALUE TO CA-SEL-CODE-VALUE              ELELCDML
00505              MOVE CV-CODE-NAME   TO CA-SEL-CODE-NAME              ELELCDML
00506              MOVE CV-CODE-DESC-SEQ TO CA-SEL-CODE-SEQ-X           ELELCDML
00507              MOVE 'U'            TO CA-CURRENT-FUNCTION           ELELCDML
00508              MOVE MAP-LITERAL3 TO DERRMO                          ELELCDML
00509              MOVE -1 TO FUNCL (WS-SAVE-SUBA)                      ELELCDML
00510              GO TO 9999-RETURN-WITH-MSG.                          ELELCDML
00511                                                                   ELELCDML
00512      IF CVALUE-ENTERED                                            ELELCDML
00513          IF CV-DELETE                                             ELELCDML
00514              MOVE CV-CODE-VALUE TO CA-FC-CODE-VALUE               ELELCDML
00515              MOVE 'YES' TO REPOSITION-SW                          ELELCDML
00516              PERFORM 4000-CREATE-SCREEN THRU 4000-EXIT            ELELCDML
00517          ELSE                                                     ELELCDML
00518             MOVE CV-CODE-VALUE TO CA-SEL-CODE-VALUE               ELELCDML
00519             MOVE CV-CODE-NAME   TO CA-SEL-CODE-NAME               ELELCDML
00520             MOVE CV-CODE-DESC-SEQ TO CA-SEL-CODE-SEQ-X            ELELCDML
00521             GO TO 9011-XCTL-ELELCEML.                             ELELCDML
00522                                                                   ELELCDML
00523 ** AT THIS POINT NO FUNCTION WAS INDICATED, SO SCROLL FORWARD **  ELELCDML
00524                                                                   ELELCDML
00525          GO TO 6000-FORWARD-READ.                                 ELELCDML
00526  3000-EXIT.  EXIT.                                                ELELCDML
00527 /                                                                 ELELCDML
00528  3100-EDIT-SCREEN.                                                ELELCDML
00529                                                                   ELELCDML
00530 **  IGNORE SPACE IF ENTERED FOR FUNCTION CODE  **                 ELELCDML
00531                                                                   ELELCDML
00532      IF FUNCL (SUBA) GREATER THAN ZERO  AND                       ELELCDML
00533         FUNC  (SUBA) EQUAL SPACE                                  ELELCDML
00534             GO TO 3100-EXIT.                                      ELELCDML
00535                                                                   ELELCDML
00536      IF FUNCL (SUBA) GREATER THAN ZERO                            ELELCDML
00537          IF FUNC (SUBA) NOT EQUAL 'S' AND 'U'                     ELELCDML
00538              MOVE -1 TO FUNCL (SUBA)                              ELELCDML
00539              MOVE ERR-06-MSG TO DERRMO                            ELELCDML
00540              MOVE DFHBMUBF TO FUNCA (SUBA)                        ELELCDML
00541              GO TO 9999-RETURN-WITH-MSG                           ELELCDML
00542          ELSE                                                     ELELCDML
00543              IF FUNC (SUBA) EQUAL 'S'                             ELELCDML
00544                  IF STAT (SUBA) EQUAL 'DELETED'                   ELELCDML
00545                      MOVE -1 TO FUNCL (SUBA)                      ELELCDML
00546                      MOVE ERR-07-MSG TO DERRMO                    ELELCDML
00547                      MOVE DFHBMUBF TO FUNCA (SUBA)                ELELCDML
00548                      GO TO 9999-RETURN-WITH-MSG                   ELELCDML
00549                  ELSE                                             ELELCDML
00550                      NEXT SENTENCE                                ELELCDML
00551              ELSE                                                 ELELCDML
00552                  IF STAT (SUBA) EQUAL SPACES                      ELELCDML
00553                      MOVE -1 TO FUNCL (SUBA)                      ELELCDML
00554                      MOVE ERR-08-MSG TO DERRMO                    ELELCDML
00555                      MOVE DFHBMUBF TO FUNCA (SUBA)                ELELCDML
00556                      GO TO 9999-RETURN-WITH-MSG.                  ELELCDML
00557                                                                   ELELCDML
00558      IF FUNCL (SUBA) GREATER THAN ZERO                            ELELCDML
00559          IF FUNCTION-ENTERED                                      ELELCDML
00560              MOVE -1 TO FUNCL (SUBA)                              ELELCDML
00561              MOVE ERR-09-MSG TO DERRMO                            ELELCDML
00562              MOVE DFHBMUBF TO FUNCA (SUBA)                        ELELCDML
00563              GO TO 9999-RETURN-WITH-MSG                           ELELCDML
00564          ELSE                                                     ELELCDML
00565              MOVE 'YES' TO FUNC-SW                                ELELCDML
00566              IF DSCODL GREATER THAN ZERO                          ELELCDML
00567                  MOVE -1 TO FUNCL (SUBA)                          ELELCDML
00568                  MOVE ERR-09-MSG TO DERRMO                        ELELCDML
00569                  MOVE DFHBMUBF TO FUNCA (SUBA)                    ELELCDML
00570                  GO TO 9999-RETURN-WITH-MSG.                      ELELCDML
00571                                                                   ELELCDML
00572      IF FUNCL (SUBA) GREATER THAN ZERO                            ELELCDML
00573          MOVE FUNC (SUBA) TO WS-SAVE-FUNCTION                     ELELCDML
00574          MOVE SUBA  TO WS-SAVE-SUBA                               ELELCDML
00575          MOVE CVAL (SUBA) TO WS-SAVE-CVALUE-KEY.                  ELELCDML
00576                                                                   ELELCDML
00577  3100-EXIT.  EXIT.                                                ELELCDML
00578 /                                                                 ELELCDML
00579  3200-UNDELETE-FUNCTION.                                          ELELCDML
00580                                                                   ELELCDML
00581      PERFORM 1350-SET-ADDRESS-OF-ELPCV THRU 1350-EXIT.            ELELCDML
00582      MOVE '3200'                   TO WS-PARA-ID.                 ELELCDML
00583                                                                   ELELCDML
00584      MOVE WS-SAVE-CVALUE-KEY       TO  CA-SEL-CODE-VALUE.         ELELCDML
00585      MOVE CA-SELECTED-CV-KEY       TO  ELCIO-VSAM-KEY3.           ELELCDML
00586                                                                   ELELCDML
00587      MOVE EL-DSN-ELPCV             TO  ELCIO-FILE-DDNAME3,        ELELCDML
00588                                        CIA-IO-GETMAIN-DDNAME.     ELELCDML
00589      MOVE EL-ELPCV-REC-LEN         TO  ELCIO-MAX-REC-LEN3.        ELELCDML
00590      MOVE 'EQ '                    TO  ELCIO-CIO-QUAL3.           ELELCDML
00591      MOVE 'M'                      TO  ELCIO-STORAGE3.            ELELCDML
00592      MOVE 'RU '                    TO  ELCIO-FILE-ACCESS-CODE3.   ELELCDML
00593      SET  ELCIO-REC-AREA-ADDRESS3  TO  CIA-ELPCV-REC-AREA-PNTR.   ELELCDML
00594      SET  CIA-IO-PARM-AREA-PNTR    TO  CIA-ELPCV-IOPARM-AREA-PNTR.ELELCDML
00595                                                                   ELELCDML
00596      EXEC CICS LINK                                               ELELCDML
00597                PROGRAM('ELAIOPGM')                                ELELCDML
00598                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCDML
00599                LENGTH(EL-CIA-POINTER-LEN)                         ELELCDML
00600      END-EXEC.                                                    ELELCDML
00601                                                                   ELELCDML
00602      IF NOT ELCIO-GOOD-RETURN3                                    ELELCDML
00603          MOVE 'ED03' TO WS-ABEND-CODE                             ELELCDML
00604          GO TO 9999-ABEND.                                        ELELCDML
00605                                                                   ELELCDML
00606      MOVE '3250' TO WS-PARA-ID.                                   ELELCDML
00607      PERFORM 3250-UNDELETE-VALUE-SEQS THRU 3250-EXIT              ELELCDML
00608          UNTIL UNDELETED-ALL-VALUE-SEQS.                          ELELCDML
00609                                                                   ELELCDML
00610      MOVE '3200' TO WS-PARA-ID.                                   ELELCDML
00611                                                                   ELELCDML
00612      MOVE SPACE          TO CA-CURRENT-FUNCTION.                  ELELCDML
00613      MOVE LOW-VALUES     TO CA-SEL-CODE-VALUE.                    ELELCDML
00614      MOVE MAP-LITERAL2   TO DERRMO.                               ELELCDML
00615      MOVE SPACES         TO STAT (WS-SAVE-SUBA)                   ELELCDML
00616                             FUNC (WS-SAVE-SUBA).                  ELELCDML
00617      MOVE DFHBMUNP       TO FUNCA (WS-SAVE-SUBA).                 ELELCDML
00618      MOVE -1             TO FUNCL (1).                            ELELCDML
00619      GO TO 9999-RETURN-WITH-MSG.                                  ELELCDML
00620  3200-EXIT.  EXIT.                                                ELELCDML
00621 /                                                                 ELELCDML
00622  3250-UNDELETE-VALUE-SEQS.                                        ELELCDML
00623                                                                   ELELCDML
00624      MOVE SPACE             TO  CV-DELETE-CODE-FLAG.              ELELCDML
00625      MOVE 'WU '             TO  ELCIO-FILE-ACCESS-CODE3.          ELELCDML
00626      MOVE EL-ELPCV-REC-LEN  TO  ELCIO-RECORD-LEN3.                ELELCDML
00627                                                                   ELELCDML
00628      EXEC CICS LINK                                               ELELCDML
00629                PROGRAM('ELAIOPGM')                                ELELCDML
00630                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCDML
00631                LENGTH(EL-CIA-POINTER-LEN)                         ELELCDML
00632      END-EXEC.                                                    ELELCDML
00633                                                                   ELELCDML
00634      IF NOT ELCIO-GOOD-RETURN3                                    ELELCDML
00635          MOVE 'ED04' TO WS-ABEND-CODE                             ELELCDML
00636          GO TO 9999-ABEND.                                        ELELCDML
00637                                                                   ELELCDML
00638                                                                   ELELCDML
00639      ADD 1 TO CA-SEL-CODE-SEQ.                                    ELELCDML
00640                                                                   ELELCDML
00641      MOVE CA-SELECTED-CV-KEY TO ELCIO-VSAM-KEY3.                  ELELCDML
00642      MOVE 'RD '              TO ELCIO-FILE-ACCESS-CODE3.          ELELCDML
00643      MOVE 'GTE'              TO ELCIO-CIO-QUAL3.                  ELELCDML
00644                                                                   ELELCDML
00645      EXEC CICS LINK                                               ELELCDML
00646                PROGRAM('ELAIOPGM')                                ELELCDML
00647                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCDML
00648                LENGTH(EL-CIA-POINTER-LEN)                         ELELCDML
00649      END-EXEC.                                                    ELELCDML
00650                                                                   ELELCDML
00651      IF ELCIO-REC-NOT-FOUND3                                      ELELCDML
00652          GO TO 3250-ENDFILE                                       ELELCDML
00653      ELSE                                                         ELELCDML
00654          IF NOT ELCIO-GOOD-RETURN3                                ELELCDML
00655              MOVE 'ED05' TO WS-ABEND-CODE                         ELELCDML
00656              GO TO 9999-ABEND.                                    ELELCDML
00657                                                                   ELELCDML
00658                                                                   ELELCDML
00659      IF CV-ELEMENT-NBR EQUAL CA-SEL-ELEMENT-NBR  AND              ELELCDML
00660         CV-CODE-VALUE EQUAL CA-SEL-CODE-VALUE                     ELELCDML
00661          MOVE 'RU '              TO ELCIO-FILE-ACCESS-CODE3       ELELCDML
00662                                                                   ELELCDML
00663          MOVE CV-CODE-KEY        TO ELCIO-VSAM-KEY3               ELELCDML
00664          EXEC CICS LINK                                           ELELCDML
00665                    PROGRAM('ELAIOPGM')                            ELELCDML
00666                    COMMAREA(ADDRESS OF CIA-PARMS-RECORD)          ELELCDML
00667                    LENGTH(EL-CIA-POINTER-LEN)                     ELELCDML
00668          END-EXEC                                                 ELELCDML
00669                                                                   ELELCDML
00670          IF NOT ELCIO-GOOD-RETURN3                                ELELCDML
00671              MOVE 'ED06' TO WS-ABEND-CODE                         ELELCDML
00672              GO TO 9999-ABEND                                     ELELCDML
00673          ELSE                                                     ELELCDML
00674              GO TO 3250-UNDELETE-VALUE-SEQS.                      ELELCDML
00675                                                                   ELELCDML
00676                                                                   ELELCDML
00677  3250-ENDFILE.                                                    ELELCDML
00678      MOVE 'YES' TO UNDELETE-SW.                                   ELELCDML
00679  3250-EXIT.  EXIT.                                                ELELCDML
00680 /                                                                 ELELCDML
00681                                                                   ELELCDML
00682  4000-CREATE-SCREEN.                                              ELELCDML
00683                                                                   ELELCDML
00684      PERFORM 1300-GET-STORAGE-FOR-ELPCV THRU 1300-EXIT.           ELELCDML
00685                                                                   ELELCDML
00686      MOVE '4000' TO WS-PARA-ID.                                   ELELCDML
00687                                                                   ELELCDML
00688 ** MOVE COMMON PORTION **                                         ELELCDML
00689      MOVE CA-SEL-RECORD-PREFIX          TO DRPREXO.               ELELCDML
00690      MOVE CA-SEL-RECORD-NAME            TO DRNAMEO.               ELELCDML
00691      MOVE CA-SEL-ELEMENT-NBR            TO WS-REFORMAT-ELEMENT.   ELELCDML
00692      MOVE WS-REFORMAT-ELEMENT           TO DEPREXO.               ELELCDML
00693      MOVE CA-SEL-ELEMENT-NAME           TO DENAMEO.               ELELCDML
00694      MOVE CA-TRANS-HDR                  TO DTITLEO.               ELELCDML
00695                                                                   ELELCDML
00696      MOVE 'SB '               TO ELCIO-FILE-ACCESS-CODE3.         ELELCDML
00697      PERFORM 4100-BROWSE-DATA-CVALUE-FILE THRU 4100-EXIT.         ELELCDML
00698                                                                   ELELCDML
00699      IF ELCIO-EOF-BROWSE3                                         ELELCDML
00700          NEXT SENTENCE                                            ELELCDML
00701      ELSE                                                         ELELCDML
00702          IF NOT ELCIO-GOOD-RETURN3                                ELELCDML
00703              MOVE 'ED07' TO WS-ABEND-CODE                         ELELCDML
00704              GO TO 9999-ABEND                                     ELELCDML
00705          ELSE                                                     ELELCDML
00706              MOVE ZERO TO SUBA                                    ELELCDML
00707              PERFORM 4200-READNEXT-WITHIN-FILE THRU 4200-EXIT     ELELCDML
00708                      UNTIL SCREEN-BUILT.                          ELELCDML
00709                                                                   ELELCDML
00710      IF NOT SCREEN-BUILT                                          ELELCDML
00711          MOVE 'YES' TO SCREEN-SW                                  ELELCDML
00712          MOVE HIGH-VALUES TO CA-LC-CODE-VALUE                     ELELCDML
00713                              CA-LC-CODE-SEQ-X                     ELELCDML
00714      ELSE                                                         ELELCDML
00715          IF NOT ELCIO-EOF-BROWSE3                                 ELELCDML
00716              PERFORM 9999-ENDBR THRU 9999-ENDBR-EXIT.             ELELCDML
00717                                                                   ELELCDML
00718      IF REPOSITION-INDICATED                                      ELELCDML
00719          MOVE ERR-07-MSG TO DERRMO                                ELELCDML
00720      ELSE                                                         ELELCDML
00721          IF CA-INQUIRY                                            ELELCDML
00722              MOVE MAP-LITERAL6 TO DLINEO                          ELELCDML
00723              MOVE MAP-LITERAL4 TO DERRMO                          ELELCDML
00724          ELSE                                                     ELELCDML
00725              MOVE MAP-LITERAL5 TO DLINEO                          ELELCDML
00726              MOVE MAP-LITERAL1 TO DERRMO.                         ELELCDML
00727                                                                   ELELCDML
00728 **  INITIALIZE THE BEGINNING SCROLL KEY REGARDLESS OF WHAT WAS    ELELCDML
00729 **  SET IN PARA. 4200 BECAUSE THIS IS THE FIRST SCREEN.           ELELCDML
00730      IF NOT CA-CV-EDIT                                            ELELCDML
00731          MOVE LOW-VALUES TO CA-FC-CODE-VALUE                      ELELCDML
00732                             CA-FC-CODE-SEQ-X.                     ELELCDML
00733                                                                   ELELCDML
00734      MOVE LOW-VALUES   TO CA-SEL-CODE-VALUE.                      ELELCDML
00735      MOVE 'D' TO CA-CURRENT-PGM.                                  ELELCDML
00736                                                                   ELELCDML
00737      EXEC CICS SEND MAP ('ELCDI01')                               ELELCDML
00738                MAPSET   ('ELCDSET')                               ELELCDML
00739                ERASE                                              ELELCDML
00740      END-EXEC.                                                    ELELCDML
00741                                                                   ELELCDML
00742      EXEC CICS RETURN                                             ELELCDML
00743                TRANSID('ELCD')                                    ELELCDML
00744                COMMAREA(DFHCOMMAREA)                              ELELCDML
00745                LENGTH(COMM-LENGTH)                                ELELCDML
00746      END-EXEC.                                                    ELELCDML
00747                                                                   ELELCDML
00748  4000-EXIT.  EXIT.                                                ELELCDML
00749 /                                                                 ELELCDML
00750  4100-BROWSE-DATA-CVALUE-FILE.                                    ELELCDML
00751      MOVE '4100' TO WS-PARA-ID.                                   ELELCDML
00752      IF CA-DE-DEFINE                                              ELELCDML
00753          MOVE LOW-VALUES           TO CA-SEL-CODE-SEQ-X           ELELCDML
00754          MOVE CA-SELECTED-CV-KEY TO ELCIO-VSAM-KEY3               ELELCDML
00755      ELSE                                                         ELELCDML
00756          IF CA-CV-EDIT    OR  REPOSITION-INDICATED                ELELCDML
00757              MOVE CA-FIRST-CODE TO ELCIO-VSAM-KEY3                ELELCDML
00758          ELSE                                                     ELELCDML
00759              IF EIBAID EQUAL DFHPF8  OR  DFHENTER                 ELELCDML
00760                  MOVE CA-LAST-CODE TO ELCIO-VSAM-KEY3             ELELCDML
00761              ELSE                                                 ELELCDML
00762                  IF EIBAID EQUAL DFHPF7                           ELELCDML
00763                      MOVE CA-FIRST-CODE TO ELCIO-VSAM-KEY3.       ELELCDML
00764                                                                   ELELCDML
00765      MOVE EL-DSN-ELPCV            TO ELCIO-FILE-DDNAME3.          ELELCDML
00766      MOVE EL-ELPCV-REC-LEN        TO ELCIO-MAX-REC-LEN3.          ELELCDML
00767      MOVE 'GTE'                   TO ELCIO-CIO-QUAL3.             ELELCDML
00768      MOVE ELPCV-KEY-LENGTH        TO ELCIO-BROWSE-KEYLEN3.        ELELCDML
00769      MOVE 'M'                     TO ELCIO-STORAGE3.              ELELCDML
00770      SET  ELCIO-REC-AREA-ADDRESS3 TO CIA-ELPCV-REC-AREA-PNTR.     ELELCDML
00771      SET  CIA-IO-PARM-AREA-PNTR   TO CIA-ELPCV-IOPARM-AREA-PNTR.  ELELCDML
00772                                                                   ELELCDML
00773      EXEC CICS LINK                                               ELELCDML
00774                PROGRAM('ELAIOPGM')                                ELELCDML
00775                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCDML
00776                LENGTH(EL-CIA-POINTER-LEN)                         ELELCDML
00777      END-EXEC.                                                    ELELCDML
00778                                                                   ELELCDML
00779  4100-EXIT. EXIT.                                                 ELELCDML
00780 /                                                                 ELELCDML
00781  4200-READNEXT-WITHIN-FILE.                                       ELELCDML
00782      MOVE '4200' TO WS-PARA-ID.                                   ELELCDML
00783                                                                   ELELCDML
00784      IF CA-SEL-RECORD-PREFIX EQUAL CV-RECORD-PREFIX   AND         ELELCDML
00785         CA-SEL-ELEMENT-NBR EQUAL CV-ELEMENT-NBR                   ELELCDML
00786          IF CV-CODE-VALUE  EQUAL   WS-SAVE-CVALUE-KEY             ELELCDML
00787              MOVE 99          TO CV-CODE-DESC-SEQ                 ELELCDML
00788              MOVE CV-CODE-KEY TO ELCIO-VSAM-KEY3                  ELELCDML
00789              GO TO 4200-CONTINUE                                  ELELCDML
00790          ELSE                                                     ELELCDML
00791              MOVE CV-CODE-VALUE TO WS-SAVE-CVALUE-KEY             ELELCDML
00792      ELSE                                                         ELELCDML
00793          MOVE 'YES' TO SCREEN-SW                                  ELELCDML
00794          MOVE HIGH-VALUES TO CA-LC-CODE-VALUE                     ELELCDML
00795                              CA-LC-CODE-SEQ-X                     ELELCDML
00796          GO TO 4200-EXIT.                                         ELELCDML
00797                                                                   ELELCDML
00798      ADD 1 TO SUBA.                                               ELELCDML
00799                                                                   ELELCDML
00800      IF SUBA EQUAL 14                                             ELELCDML
00801          MOVE 'YES' TO SCREEN-SW                                  ELELCDML
00802          MOVE CV-CODE-KEY TO CA-LAST-CODE                         ELELCDML
00803          PERFORM 4400-MOVE-INFO-TO-SCREEN THRU 4400-EXIT          ELELCDML
00804          GO TO 4200-EXIT                                          ELELCDML
00805      ELSE                                                         ELELCDML
00806          IF SUBA EQUAL 1                                          ELELCDML
00807              MOVE CV-CODE-KEY TO CA-FIRST-CODE                    ELELCDML
00808              MOVE SPACES     TO DSCODO                            ELELCDML
00809              MOVE DFHBMUNP   TO DSCODA                            ELELCDML
00810              PERFORM 4500-REINITIALIZE-SCREEN THRU 4500-EXIT      ELELCDML
00811                  VARYING SUBB FROM 1 BY 1                         ELELCDML
00812                  UNTIL SUBB GREATER THAN 14.                      ELELCDML
00813                                                                   ELELCDML
00814                                                                   ELELCDML
00815      PERFORM 4400-MOVE-INFO-TO-SCREEN THRU 4400-EXIT.             ELELCDML
00816                                                                   ELELCDML
00817  4200-CONTINUE.                                                   ELELCDML
00818                                                                   ELELCDML
00819      EXEC CICS LINK                                               ELELCDML
00820                PROGRAM('ELAIOPGM')                                ELELCDML
00821                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCDML
00822                LENGTH(EL-CIA-POINTER-LEN)                         ELELCDML
00823      END-EXEC.                                                    ELELCDML
00824                                                                   ELELCDML
00825      IF ELCIO-EOF-BROWSE3                                         ELELCDML
00826          MOVE 'YES' TO SCREEN-SW                                  ELELCDML
00827          MOVE HIGH-VALUES TO CA-LC-CODE-VALUE                     ELELCDML
00828                              CA-LC-CODE-SEQ-X                     ELELCDML
00829          GO TO 4200-EXIT                                          ELELCDML
00830      ELSE                                                         ELELCDML
00831          IF NOT ELCIO-GOOD-RETURN3                                ELELCDML
00832              MOVE 'ED08' TO WS-ABEND-CODE                         ELELCDML
00833              GO TO 9999-ABEND.                                    ELELCDML
00834                                                                   ELELCDML
00835  4200-EXIT.  EXIT.                                                ELELCDML
00836 /                                                                 ELELCDML
00837  4300-READPREV-WITHIN-FILE.                                       ELELCDML
00838                                                                   ELELCDML
00839      EXEC CICS LINK                                               ELELCDML
00840                PROGRAM('ELAIOPGM')                                ELELCDML
00841                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCDML
00842                LENGTH(EL-CIA-POINTER-LEN)                         ELELCDML
00843      END-EXEC.                                                    ELELCDML
00844                                                                   ELELCDML
00845      IF ELCIO-EOF-BROWSE3                                         ELELCDML
00846          GO TO 4300-EXIT                                          ELELCDML
00847      ELSE                                                         ELELCDML
00848          IF NOT ELCIO-GOOD-RETURN3                                ELELCDML
00849              MOVE 'ED09' TO WS-ABEND-CODE                         ELELCDML
00850              GO TO 9999-ABEND.                                    ELELCDML
00851                                                                   ELELCDML
00852      IF SUBA EQUAL 15                                             ELELCDML
00853          IF CV-RECORD-PREFIX  NOT EQUAL CA-SEL-RECORD-PREFIX OR   ELELCDML
00854             CV-ELEMENT-NBR  NOT EQUAL CA-SEL-ELEMENT-NBR          ELELCDML
00855              MOVE 'YES' TO SCREEN-SW                              ELELCDML
00856              PERFORM 9999-ENDBR THRU 9999-ENDBR-EXIT              ELELCDML
00857              GO TO 9999-ENDFILE.                                  ELELCDML
00858                                                                   ELELCDML
00859      IF CA-SEL-RECORD-PREFIX NOT EQUAL CV-RECORD-PREFIX  OR       ELELCDML
00860         CA-SEL-ELEMENT-NBR NOT EQUAL CV-ELEMENT-NBR               ELELCDML
00861          MOVE 'YES' TO SCREEN-SW                                  ELELCDML
00862          MOVE LOW-VALUES TO CA-FC-CODE-VALUE                      ELELCDML
00863                             CA-FC-CODE-SEQ-X                      ELELCDML
00864          GO TO 4300-EXIT                                          ELELCDML
00865      ELSE                                                         ELELCDML
00866         IF CV-CODE-KEY EQUAL CA-FIRST-CODE                        ELELCDML
00867             GO TO 4300-READPREV-WITHIN-FILE.                      ELELCDML
00868                                                                   ELELCDML
00869 **  SHOULD THE SAVE CODE VALUE EXIST (DIFFERENT SEQUENCE CODE)    ELELCDML
00870 **  CONTINUE TO OVERLAY THE DATA LINE UNTIL DIFFERENT CODE        ELELCDML
00871 **  VALUE APPEARS.                                                ELELCDML
00872                                                                   ELELCDML
00873      IF CV-CODE-VALUE  EQUAL   WS-SAVE-CVALUE-KEY                 ELELCDML
00874          PERFORM 4400-MOVE-INFO-TO-SCREEN THRU 4400-EXIT          ELELCDML
00875          IF SUBA EQUAL 1                                          ELELCDML
00876              MOVE CV-CODE-KEY TO CA-FIRST-CODE                    ELELCDML
00877              GO TO 4300-READPREV-WITHIN-FILE                      ELELCDML
00878          ELSE                                                     ELELCDML
00879              GO TO 4300-READPREV-WITHIN-FILE                      ELELCDML
00880      ELSE                                                         ELELCDML
00881          MOVE CV-CODE-VALUE TO WS-SAVE-CVALUE-KEY.                ELELCDML
00882                                                                   ELELCDML
00883      SUBTRACT 1 FROM SUBA.                                        ELELCDML
00884      IF SUBA EQUAL ZERO                                           ELELCDML
00885          MOVE 'YES' TO SCREEN-SW                                  ELELCDML
00886          GO TO 4300-EXIT.                                         ELELCDML
00887                                                                   ELELCDML
00888      IF SUBA EQUAL 1                                              ELELCDML
00889          MOVE CV-CODE-KEY TO CA-FIRST-CODE                        ELELCDML
00890      ELSE                                                         ELELCDML
00891          IF SUBA EQUAL 14                                         ELELCDML
00892              MOVE CV-CODE-KEY TO CA-LAST-CODE                     ELELCDML
00893              MOVE SPACES     TO DSCODO                            ELELCDML
00894              MOVE DFHBMUNP   TO DSCODA                            ELELCDML
00895              PERFORM 4500-REINITIALIZE-SCREEN THRU 4500-EXIT      ELELCDML
00896                  VARYING SUBB FROM 1 BY 1                         ELELCDML
00897                  UNTIL SUBB GREATER THAN 14.                      ELELCDML
00898                                                                   ELELCDML
00899      PERFORM 4400-MOVE-INFO-TO-SCREEN THRU 4400-EXIT.             ELELCDML
00900  4300-EXIT.  EXIT.                                                ELELCDML
00901 /                                                                 ELELCDML
00902  4400-MOVE-INFO-TO-SCREEN.                                        ELELCDML
00903      MOVE '4400' TO WS-PARA-ID.                                   ELELCDML
00904                                                                   ELELCDML
00905      MOVE CV-CODE-VALUE        TO CVAL    (SUBA).                 ELELCDML
00906      MOVE CV-CODE-NAME         TO NAME    (SUBA).                 ELELCDML
00907      IF CV-DELETE                                                 ELELCDML
00908          MOVE 'DELETED'        TO STAT    (SUBA)                  ELELCDML
00909      ELSE                                                         ELELCDML
00910          MOVE SPACES           TO STAT    (SUBA).                 ELELCDML
00911  4400-EXIT.  EXIT.                                                ELELCDML
00912 /                                                                 ELELCDML
00913  4500-REINITIALIZE-SCREEN.                                        ELELCDML
00914      MOVE SPACES     TO FUNC   (SUBB)                             ELELCDML
00915                         NAME   (SUBB)                             ELELCDML
00916                         CVAL   (SUBB)                             ELELCDML
00917                         STAT   (SUBB).                            ELELCDML
00918      MOVE DFHBMUNP   TO FUNCA  (SUBB).                            ELELCDML
00919  4500-EXIT.  EXIT.                                                ELELCDML
00920 /                                                                 ELELCDML
00921  4600-ENDBR.                                                      ELELCDML
00922                                                                   ELELCDML
00923      IF NOT ELCIO-EOF-BROWSE3                                     ELELCDML
00924          PERFORM 9999-ENDBR THRU 9999-ENDBR-EXIT.                 ELELCDML
00925                                                                   ELELCDML
00926      IF CA-INQUIRY                                                ELELCDML
00927          MOVE MAP-LITERAL6 TO DLINEO                              ELELCDML
00928          MOVE MAP-LITERAL4 TO DERRMO                              ELELCDML
00929      ELSE                                                         ELELCDML
00930          MOVE MAP-LITERAL5 TO DLINEO                              ELELCDML
00931          MOVE MAP-LITERAL1 TO DERRMO.                             ELELCDML
00932                                                                   ELELCDML
00933      MOVE LOW-VALUES   TO CA-SEL-CODE-VALUE.                      ELELCDML
00934      MOVE SPACES       TO CA-CURRENT-FUNCTION                     ELELCDML
00935                           CA-SEL-CODE-NAME.                       ELELCDML
00936      MOVE 'D' TO CA-CURRENT-PGM.                                  ELELCDML
00937                                                                   ELELCDML
00938      EXEC CICS SEND MAP ('ELCDI01')                               ELELCDML
00939                MAPSET   ('ELCDSET')                               ELELCDML
00940                ERASE                                              ELELCDML
00941      END-EXEC.                                                    ELELCDML
00942                                                                   ELELCDML
00943      EXEC CICS RETURN                                             ELELCDML
00944                TRANSID('ELCD')                                    ELELCDML
00945                COMMAREA(DFHCOMMAREA)                              ELELCDML
00946                LENGTH(COMM-LENGTH)                                ELELCDML
00947      END-EXEC.                                                    ELELCDML
00948                                                                   ELELCDML
00949  4600-EXIT.  EXIT.                                                ELELCDML
00950 /                                                                 ELELCDML
00951  5000-BACKWARD-READ.                                              ELELCDML
00952      MOVE '5000' TO WS-PARA-ID.                                   ELELCDML
00953                                                                   ELELCDML
00954      PERFORM 1300-GET-STORAGE-FOR-ELPCV THRU 1300-EXIT.           ELELCDML
00955                                                                   ELELCDML
00956      IF CA-FC-CODE-VALUE EQUAL LOW-VALUES  OR                     ELELCDML
00957         CA-FC-CODE-SEQ-X EQUAL LOW-VALUES                         ELELCDML
00958          GO TO 9999-ENDFILE.                                      ELELCDML
00959                                                                   ELELCDML
00960                                                                   ELELCDML
00961      MOVE 'SBP' TO ELCIO-FILE-ACCESS-CODE3.                       ELELCDML
00962      PERFORM 4100-BROWSE-DATA-CVALUE-FILE THRU 4100-EXIT.         ELELCDML
00963                                                                   ELELCDML
00964                                                                   ELELCDML
00965      IF ELCIO-EOF-BROWSE3                                         ELELCDML
00966          GO TO 9999-ENDFILE                                       ELELCDML
00967      ELSE                                                         ELELCDML
00968          IF NOT ELCIO-GOOD-RETURN3                                ELELCDML
00969              MOVE 'EB10' TO WS-ABEND-CODE                         ELELCDML
00970              GO TO 9999-ABEND.                                    ELELCDML
00971                                                                   ELELCDML
00972                                                                   ELELCDML
00973      IF CV-CODE-KEY NOT EQUAL CA-FIRST-CODE                       ELELCDML
00974         PERFORM 9999-ENDBR THRU 9999-ENDBR-EXIT                   ELELCDML
00975         GO TO 9999-ENDFILE.                                       ELELCDML
00976                                                                   ELELCDML
00977      MOVE 15 TO SUBA.                                             ELELCDML
00978      MOVE '4300' TO WS-PARA-ID.                                   ELELCDML
00979      PERFORM 4300-READPREV-WITHIN-FILE THRU 4300-EXIT             ELELCDML
00980          UNTIL SCREEN-BUILT.                                      ELELCDML
00981                                                                   ELELCDML
00982      GO TO 4600-ENDBR.                                            ELELCDML
00983                                                                   ELELCDML
00984  5000-EXIT.  EXIT.                                                ELELCDML
00985 /                                                                 ELELCDML
00986  6000-FORWARD-READ.                                               ELELCDML
00987      MOVE '6000' TO WS-PARA-ID.                                   ELELCDML
00988                                                                   ELELCDML
00989      PERFORM 1300-GET-STORAGE-FOR-ELPCV THRU 1300-EXIT.           ELELCDML
00990                                                                   ELELCDML
00991      IF CA-LC-CODE-VALUE EQUAL HIGH-VALUES                        ELELCDML
00992          GO TO 9999-ENDFILE.                                      ELELCDML
00993                                                                   ELELCDML
00994      MOVE 'SB '  TO ELCIO-FILE-ACCESS-CODE3.                      ELELCDML
00995      PERFORM 4100-BROWSE-DATA-CVALUE-FILE THRU 4100-EXIT.         ELELCDML
00996                                                                   ELELCDML
00997      IF ELCIO-EOF-BROWSE3                                         ELELCDML
00998          GO TO 9999-ENDFILE                                       ELELCDML
00999      ELSE                                                         ELELCDML
01000          IF NOT ELCIO-GOOD-RETURN3                                ELELCDML
01001              MOVE 'EB11' TO WS-ABEND-CODE                         ELELCDML
01002              GO TO 9999-ABEND.                                    ELELCDML
01003                                                                   ELELCDML
01004      IF CV-CODE-KEY NOT EQUAL CA-LAST-CODE                        ELELCDML
01005          PERFORM 9999-ENDBR THRU 9999-ENDBR-EXIT                  ELELCDML
01006          GO TO 9999-ENDFILE.                                      ELELCDML
01007                                                                   ELELCDML
01008      MOVE CV-CODE-VALUE TO WS-SAVE-CVALUE-KEY.                    ELELCDML
01009                                                                   ELELCDML
01010  6000-CONTINUE.                                                   ELELCDML
01011                                                                   ELELCDML
01012      EXEC CICS LINK                                               ELELCDML
01013                PROGRAM('ELAIOPGM')                                ELELCDML
01014                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCDML
01015                LENGTH(EL-CIA-POINTER-LEN)                         ELELCDML
01016      END-EXEC.                                                    ELELCDML
01017                                                                   ELELCDML
01018      IF ELCIO-EOF-BROWSE3                                         ELELCDML
01019          NEXT SENTENCE                                            ELELCDML
01020      ELSE                                                         ELELCDML
01021          IF NOT ELCIO-GOOD-RETURN3                                ELELCDML
01022              MOVE 'EB12' TO WS-ABEND-CODE                         ELELCDML
01023              GO TO 9999-ABEND                                     ELELCDML
01024          ELSE                                                     ELELCDML
01025              IF CV-CODE-VALUE EQUAL WS-SAVE-CVALUE-KEY            ELELCDML
01026                  MOVE 99 TO CV-CODE-DESC-SEQ                      ELELCDML
01027                  MOVE CV-CODE-KEY TO ELCIO-VSAM-KEY3              ELELCDML
01028                  GO TO 6000-CONTINUE                              ELELCDML
01029              ELSE                                                 ELELCDML
01030                  MOVE ZERO TO SUBA                                ELELCDML
01031                  PERFORM 4200-READNEXT-WITHIN-FILE THRU 4200-EXIT ELELCDML
01032                      UNTIL SCREEN-BUILT.                          ELELCDML
01033                                                                   ELELCDML
01034      GO TO 4600-ENDBR.                                            ELELCDML
01035  6000-EXIT.  EXIT.                                                ELELCDML
01036 /                                                                 ELELCDML
01037  9000-XCTL-ELELCAML.                                              ELELCDML
01038                                                                   ELELCDML
01039      PERFORM 1100-GET-STORAGE-FOR-ELPRL THRU 1100-EXIT.           ELELCDML
01040      PERFORM 1200-GET-STORAGE-FOR-ELPDE THRU 1200-EXIT.           ELELCDML
01041      PERFORM 1300-GET-STORAGE-FOR-ELPCV THRU 1300-EXIT.           ELELCDML
01042                                                                   ELELCDML
01043      MOVE 'D' TO CA-CURRENT-PGM.                                  ELELCDML
01044      MOVE LOW-VALUES TO CA-SEL-CODE-VALUE.                        ELELCDML
01045      MOVE SPACES TO CA-SEL-CODE-NAME                              ELELCDML
01046                     CA-CURRENT-FUNCTION.                          ELELCDML
01047                                                                   ELELCDML
01048      EXEC CICS XCTL                                               ELELCDML
01049                PROGRAM('ELELCAML')                                ELELCDML
01050                COMMAREA(DFHCOMMAREA)                              ELELCDML
01051                LENGTH(COMM-LENGTH)                                ELELCDML
01052      END-EXEC.                                                    ELELCDML
01053  9010-XCTL-ELELCCML.                                              ELELCDML
01054                                                                   ELELCDML
01055      PERFORM 1100-GET-STORAGE-FOR-ELPRL THRU 1100-EXIT.           ELELCDML
01056      PERFORM 1200-GET-STORAGE-FOR-ELPDE THRU 1200-EXIT.           ELELCDML
01057      PERFORM 1300-GET-STORAGE-FOR-ELPCV THRU 1300-EXIT.           ELELCDML
01058                                                                   ELELCDML
01059      MOVE 'D' TO CA-CURRENT-PGM.                                  ELELCDML
01060      IF EIBAID EQUAL DFHPF4                                       ELELCDML
01061          MOVE LOW-VALUES TO CA-SEL-CODE-VALUE                     ELELCDML
01062          MOVE SPACES TO CA-SEL-CODE-NAME                          ELELCDML
01063                         CA-CURRENT-FUNCTION.                      ELELCDML
01064                                                                   ELELCDML
01065                                                                   ELELCDML
01066      EXEC CICS XCTL                                               ELELCDML
01067                PROGRAM('ELELCCML')                                ELELCDML
01068                COMMAREA(DFHCOMMAREA)                              ELELCDML
01069                LENGTH(COMM-LENGTH)                                ELELCDML
01070      END-EXEC.                                                    ELELCDML
01071 /                                                                 ELELCDML
01072  9011-XCTL-ELELCEML.                                              ELELCDML
01073                                                                   ELELCDML
01074      PERFORM 1100-GET-STORAGE-FOR-ELPRL THRU 1100-EXIT.           ELELCDML
01075      PERFORM 1200-GET-STORAGE-FOR-ELPDE THRU 1200-EXIT.           ELELCDML
01076      PERFORM 1300-GET-STORAGE-FOR-ELPCV THRU 1300-EXIT.           ELELCDML
01077                                                                   ELELCDML
01078      IF CA-INQUIRY                                                ELELCDML
01079          IF SELECT-INDICATED  OR  CVALUE-ENTERED                  ELELCDML
01080              NEXT SENTENCE                                        ELELCDML
01081          ELSE                                                     ELELCDML
01082              MOVE ERR-13-MSG TO DERRMO                            ELELCDML
01083              MOVE -1 TO FUNCL (1)                                 ELELCDML
01084              GO TO 9999-RETURN-WITH-MSG.                          ELELCDML
01085                                                                   ELELCDML
01086      MOVE 'D' TO CA-CURRENT-PGM.                                  ELELCDML
01087      MOVE SPACES TO CA-CURRENT-FUNCTION                           ELELCDML
01088                     CA-MAPFROM-ELEMENT-NAME.                      ELELCDML
01089      MOVE LOW-VALUES TO CA-MAPFROM-KEYS.                          ELELCDML
01090                                                                   ELELCDML
01091      EXEC CICS XCTL                                               ELELCDML
01092                PROGRAM('ELELCEML')                                ELELCDML
01093                COMMAREA(DFHCOMMAREA)                              ELELCDML
01094                LENGTH(COMM-LENGTH)                                ELELCDML
01095      END-EXEC.                                                    ELELCDML
01096                                                                   ELELCDML
01097 /                                                                 ELELCDML
01098  9999-ENDBR.                                                      ELELCDML
01099      MOVE 'EB '            TO ELCIO-FILE-ACCESS-CODE3.            ELELCDML
01100                                                                   ELELCDML
01101      EXEC CICS LINK                                               ELELCDML
01102                PROGRAM('ELAIOPGM')                                ELELCDML
01103                COMMAREA(ADDRESS OF CIA-PARMS-RECORD)              ELELCDML
01104                LENGTH(EL-CIA-POINTER-LEN)                         ELELCDML
01105      END-EXEC.                                                    ELELCDML
01106                                                                   ELELCDML
01107      IF NOT ELCIO-GOOD-RETURN3                                    ELELCDML
01108          MOVE 'EB13' TO WS-ABEND-CODE                             ELELCDML
01109          GO TO 9999-ABEND.                                        ELELCDML
01110                                                                   ELELCDML
01111  9999-ENDBR-EXIT.  EXIT.                                          ELELCDML
01112 /                                                                 ELELCDML
01113  9999-ENDFILE.                                                    ELELCDML
01114                                                                   ELELCDML
01115      IF EIBAID EQUAL DFHPF8  OR  DFHPF20  OR  DFHENTER            ELELCDML
01116          MOVE ERR-03-MSG TO DERRMO                                ELELCDML
01117          MOVE HIGH-VALUES TO CA-LC-CODE-VALUE                     ELELCDML
01118                              CA-LC-CODE-SEQ-X                     ELELCDML
01119      ELSE                                                         ELELCDML
01120          MOVE ERR-04-MSG TO DERRMO                                ELELCDML
01121          MOVE LOW-VALUES TO CA-FC-CODE-VALUE                      ELELCDML
01122                             CA-FC-CODE-SEQ-X.                     ELELCDML
01123                                                                   ELELCDML
01124      MOVE -1 TO FUNCL (1).                                        ELELCDML
01125      GO TO 9999-RETURN-WITH-MSG.                                  ELELCDML
01126                                                                   ELELCDML
01127 /                                                                 ELELCDML
01128  9999-NOTFND.                                                     ELELCDML
01129      MOVE ERR-11-MSG TO DERRMO.                                   ELELCDML
01130      IF FUNCTION-ENTERED                                          ELELCDML
01131          MOVE -1 TO FUNCL (WS-SAVE-SUBA)                          ELELCDML
01132      ELSE                                                         ELELCDML
01133          MOVE -1 TO FUNCL (1).                                    ELELCDML
01134      GO TO 9999-RETURN-WITH-MSG.                                  ELELCDML
01135                                                                   ELELCDML
01136  9999-MAPFAIL.                                                    ELELCDML
01137      MOVE ERR-12-MSG TO DERRMO.                                   ELELCDML
01138      MOVE -1 TO FUNCL (1).                                        ELELCDML
01139      GO TO 9999-RETURN-WITH-MSG.                                  ELELCDML
01140                                                                   ELELCDML
01141  9999-RETURN-WITH-MSG.                                            ELELCDML
01142      MOVE 'D' TO CA-CURRENT-PGM.                                  ELELCDML
01143                                                                   ELELCDML
01144      EXEC CICS SEND MAP ('ELCDI01')                               ELELCDML
01145                MAPSET   ('ELCDSET')                               ELELCDML
01146                DATAONLY                                           ELELCDML
01147                CURSOR                                             ELELCDML
01148      END-EXEC.                                                    ELELCDML
01149      EXEC CICS RETURN                                             ELELCDML
01150                TRANSID('ELCD')                                    ELELCDML
01151                COMMAREA(DFHCOMMAREA)                              ELELCDML
01152                LENGTH(COMM-LENGTH)                                ELELCDML
01153      END-EXEC.                                                    ELELCDML
01154                                                                   ELELCDML
01155  9999-ABEND.                                                      ELELCDML
01156      EXEC CICS ABEND                                              ELELCDML
01157                ABCODE(WS-ABEND-CODE)                              ELELCDML
01158      END-EXEC.                                                    ELELCDML
