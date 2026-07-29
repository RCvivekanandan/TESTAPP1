00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID.  ELELCAML.                                           ELELCAML
00003 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    LV001
00004 *       CCCCCCC OOOOOOOO  BBBBBBBBB OOOOOOOO LLLL   2222222222  * ELELCAML
00005 *     CCC      OOO   OOO BBB   BBB OOO   OOO LLL     222  222   * ELELCAML
00006 *    CCC      OOO   OOO BBB   BBB OOO   OOO LLL      222  222   * ELELCAML
00007 *   CCC      OOO   OOO BBBBBBBBB OOO   OOO LLL       222  222   * ELELCAML
00008 *  CCC      OOO   OOO BBB   BBB OOO   OOO LLL        222  222   * ELELCAML
00009 * CCC      OOO   OOO BBB   BBB OOO   OOO LLL         222  222   * ELELCAML
00010 * CCCCCCCC OOOOOOOO BBBBBBBBB OOOOOOOOO LLLLLLLLL   2222222222  * ELELCAML
00011 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ELELCAML
00012  AUTHOR.   JOHN CURIN --- KEANE,INC.                              ELELCAML
00013  DATE-WRITTEN.  09/30/85.                                         ELELCAML
00014  DATE-COMPILED.                                                   ELELCAML
00015 /-------------> P R O G R A M   H I S T O R Y <-----------------* ELELCAML
00016 *                                                               * ELELCAML
00017 *   DEC  1990   RKH   CONVERTED TO COBOL II                     * ELELCAML
00018 *                                                               * ELELCAML
00019 *---------------------------------------------------------------* ELELCAML
00020  ENVIRONMENT DIVISION.                                            ELELCAML
00021  DATA DIVISION.                                                   ELELCAML
00022  WORKING-STORAGE SECTION.                                         ELELCAML
00023                                                                   ELELCAML
00024  TITLE 'WORKING STORAGE - ELELCAML '.                             ELELCAML
00025  01  WORK-STORAGE               PIC X(27)           VALUE         ELELCAML
00026      'WORKING STORAGE STARTS HERE'.                               ELELCAML
00027                                                                   ELELCAML
00028  01  TRANS-ID                   PIC X(04)           VALUE 'ELCA'. ELELCAML
00029                                                                   ELELCAML
00030  01  PROGRAM-COUNTERS.                                            ELELCAML
00031      05  MAP-CTR                COMP  PIC S9(4).                  ELELCAML
00032      05  SCREEN-CTR             COMP  PIC S9(4).                  ELELCAML
00033      05  SELECT-CTR             COMP  PIC S9(4).                  ELELCAML
00034      05  DCU-CTR                COMP  PIC S9(4).                  ELELCAML
00035                                                                   ELELCAML
00036  01  PROGRAM-MESSAGES.                                            ELELCAML
00037      05  ADD-MUST-BE-LAST-LINE  PIC X(45)           VALUE         ELELCAML
00038          'AN ADD MUST BE ON LAST FUNCTION ON THE SCREEN'.         ELELCAML
00039      05  BEGIN-FILE-ENCOUNTERED PIC X(46)           VALUE         ELELCAML
00040          'THE BEGINNING OF THE FILE HAS BEEN ENCOUNTERED'.        ELELCAML
00041      05  DELETED                PIC X(08)           VALUE         ELELCAML
00042          'DELETED '.                                              ELELCAML
00043      05  DELETE-CANNOT-SELECT   PIC X(33)           VALUE         ELELCAML
00044          'DELETED RECORD CANNOT BE SELECTED'.                     ELELCAML
00045      05  END-FILE-ENCOUNTERED   PIC X(44)           VALUE         ELELCAML
00046          'THE ENDING OF THE FILE HAS BEEN ENCOUNTERED'.           ELELCAML
00047      05  IGNORED                PIC X(07)           VALUE         ELELCAML
00048          'IGNORED'.                                               ELELCAML
00049      05  INITIAL-INQUIRY        PIC X(79)           VALUE         ELELCAML
00050          'FCN IS (S)ELECT ONLY'.                                  ELELCAML
00051      05  INITIAL-MAINT          PIC X(79)           VALUE         ELELCAML
00052          'FCN IS (A)DD (C)HANGE (D)ELETE (M)AP TO NEW RECORD (S)ELELELCAML
00053 -        'ECT (U)NDELETE'.                                        ELELCAML
00054      05  INITIAL-SUPERV         PIC X(79)           VALUE         ELELCAML
00055          'FCN IS (A)DD (C)HANGE (D)ELETE (M)AP FROM (S)ELECT (U)NDELELCAML
00056 -        'ELETE'.                                                 ELELCAML
00057      05  INVALID-SELECT-17      PIC X(65)           VALUE         ELELCAML
00058          'THE FUNCTION SELECTED NOT VALID ON LAST LINE, ONLY A OR ELELCAML
00059 -        'M ALLOWED'.                                             ELELCAML
00060      05  INVALID-USE-PF4        PIC X(40)           VALUE         ELELCAML
00061          'PF4 TO BE USED ONLY WITH SELECT FUNCTION'.              ELELCAML
00062      05  INVALID-USE-PF6        PIC X(60)           VALUE         ELELCAML
00063          'PF6 TO BE USED ONLY AS CONFIRMATION OF MAP, DELETE, UNDEELELCAML
00064 -        'LETE'.                                                  ELELCAML
00065      05  INITIAL-VALUE          PIC X(48)           VALUE         ELELCAML
00066          '(PF4=ELEMENT SELECT)(PF7/8=BACK/FWD)(CLEAR=EXIT)'.      ELELCAML
00067      05  INVALID-FILE-CODE      PIC X(49)           VALUE         ELELCAML
00068          'INVALID FILE CODE.  VALID CODES ARE B, C, G, OR T'.     ELELCAML
00069      05  INVALID-SELECT         PIC X(45)           VALUE         ELELCAML
00070          'FUNCTION SELECTED NOT VALID.  PLEASE RE-ENTER'.         ELELCAML
00071      05  MAP-FAIL               PIC X(37)           VALUE         ELELCAML
00072          'PLEASE ENTER DATA OR USE VALID PF KEY'.                 ELELCAML
00073      05  MAP-FROM               PIC X(08)           VALUE         ELELCAML
00074          'MAP FROM'.                                              ELELCAML
00075      05  S-TATUS                PIC X(09)           VALUE         ELELCAML
00076          ' STATUS  '.                                             ELELCAML
00077      05  STATUS-OR              PIC X(09)           VALUE         ELELCAML
00078          'STATUS OR'.                                             ELELCAML
00079      05  MAP-FROM-NOTFND        PIC X(37)           VALUE         ELELCAML
00080          'RECORD SELECTED FOR MAPPING NOT FOUND'.                 ELELCAML
00081      05  MAP-FROM-MSG           PIC X(51)           VALUE         ELELCAML
00082          'MAP FROM WILL TAKE A LONG TIME.  USE PF6 TO CONFIRM'.   ELELCAML
00083      05  MAP-NOT-ALLOW-FROM-DELETE PIC X(41)        VALUE         ELELCAML
00084          'MAPPING NOT ALLOWED FROM A DELETED RECORD'.             ELELCAML
00085      05  MAP-NOT-ALLOW-TO-DELETE PIC X(39)          VALUE         ELELCAML
00086          'MAPPING NOT ALLOWED TO A DELETED RECORD'.               ELELCAML
00087      05  MAP-OVERLAY-MSG        PIC X(58)           VALUE         ELELCAML
00088          'MAP FROM WILL OVERLAY EXISTING RECORD. USE PF6 TO CONFIRELELCAML
00089 -        'M'.                                                     ELELCAML
00090      05  NO-CHANGES-MADE        PIC X(16)           VALUE         ELELCAML
00091          'NO CHANGES MADE'.                                       ELELCAML
00092      05  RECORD-PREF-REQ-ADD    PIC X(34)           VALUE         ELELCAML
00093          'RECORD PREFIX REQUIRED FOR AN ADD'.                     ELELCAML
00094      05  DELETE-UNDELETE-CONFIRM PIC X(35)          VALUE         ELELCAML
00095          'USE PF6 TO CONFIRM DELETE/UNDELETE'.                    ELELCAML
00096      05  MORE-THAN-ONE-SELECT   PIC X(35)           VALUE         ELELCAML
00097          'ONLY ONE SELECT ALLOWED PER SCREEN'.                    ELELCAML
00098      05  MULTI-MAP-FR-NOT-ALLOW PIC X(46)           VALUE         ELELCAML
00099          'ONLY ONE MAP FROM FUNCTION ALLOWED PER SCREEN'.         ELELCAML
00100      05  NEW-PREFIX-ON-FILE     PIC X(76)           VALUE         ELELCAML
00101          'NEW PREFIX ON FILE, BUT SCHEDULED FOR BATCH DELETE RE-ENELELCAML
00102 -        'TER AFTER BATCH RUN'.                                   ELELCAML
00103      05  NO-NEW-PREFIX-ENTERED  PIC X(41)           VALUE         ELELCAML
00104          'NO TO PREFIX ENTERED ON MAP FROM FUNCTION'.             ELELCAML
00105      05  RECORD-NOTFND          PIC X(22)           VALUE         ELELCAML
00106          'RECORD NOT FOUND      '.                                ELELCAML
00107      05  RECORD-ON-FILE         PIC X(22)           VALUE         ELELCAML
00108          'RECORD ALREADY ON FILE'.                                ELELCAML
00109      05  REC-SCH-FOR-DEL-NCHG   PIC X(51)           VALUE         ELELCAML
00110          'RECORD SCHEDULED FOR DELETION. NO CHANGES ALLOWED'.     ELELCAML
00111      05  UNABLE-TO-START-BR     PIC X(23)           VALUE         ELELCAML
00112          'START OF BROWSE FAILED'.                                ELELCAML
00113      05  UPDATE-SUCCESSFUL      PIC X(22)           VALUE         ELELCAML
00114          'UPDATE SUCESSFUL'.                                      ELELCAML
00115      05  UNAUTHOR-MAINTAIN      PIC X(41)           VALUE         ELELCAML
00116          'USE CORRECT PF KEY FOR INQUIRY SELECTION '.             ELELCAML
00117      05  UNAUTHOR-OVERLAY       PIC X(54)           VALUE         ELELCAML
00118          'YOU ARE NOT AUTHORIZED TO OVERLAY AN EXISTING RECORD'.  ELELCAML
00119 /                                                                 ELELCAML
00120  01  PROGRAM-SWITCHES.                                            ELELCAML
00121      05  ADD-ERROR                        PIC X.                  ELELCAML
00122      05  BEGINNING-FILE-SW                PIC X.                  ELELCAML
00123      05  BYPASS-SELECT-SW                 PIC X.                  ELELCAML
00124      05  DELETE-OR-UNDELETE               PIC X.                  ELELCAML
00125      05  DELETE-SW                        PIC X.                  ELELCAML
00126      05  ERASEAUP-SW                      PIC X.                  ELELCAML
00127      05  ERROR-SW                         PIC X.                  ELELCAML
00128      05  FOUND-SW                         PIC X.                  ELELCAML
00129      05  INVALID-17-SW                    PIC X.                  ELELCAML
00130      05  OVERLAY-SW                       PIC X.                  ELELCAML
00131      05  PF6-SW                           PIC X.                  ELELCAML
00132      05  REWRITE-SW                       PIC X.                  ELELCAML
00133      05  SAVE-CV-NBR                      PIC S9(03)V99.          ELELCAML
00134      05  SAVE-DE-NBR                      PIC S9(03)V99.          ELELCAML
00135      05  SAVE-CODE-VALUE                  PIC X(10).              ELELCAML
00136      05  SAVE-CODE-DESC-SEQ               PIC 99.                 ELELCAML
00137      05  SAVE-RL-FILE                     PIC X.                  ELELCAML
00138      05  SAVE-RL-RECORD-NAME              PIC X(50).              ELELCAML
00139 /                                                                 ELELCAML
00140  01  VALIDATAION-AREA.                                            ELELCAML
00141      05  FILE-CHECK             PIC X.                            ELELCAML
00142          88 VALID-FILE           VALUES ARE 'B', 'C', 'G', 'T'.   ELELCAML
00143      05  MAX-DISPLAY-LINE       PIC 99          VALUE 16.         ELELCAML
00144      05  MAX-PROCESS-LINE       PIC 99          VALUE 17.         ELELCAML
00145      05  MAP-POSITION           PIC S9(4) COMP  VALUE +1.         ELELCAML
00146      05  SELECT-POSITION        PIC S9(4) COMP  VALUE +0.         ELELCAML
00147      05  LOCATE-PREFIX          PIC X(08)       VALUE SPACES.     ELELCAML
00148                                                                   ELELCAML
00149  01  PROGRAM-COM-KEY-AREA.                                        ELELCAML
00150      05  COMM-LENGTH            PIC S9(4) COMP       VALUE +500.  ELELCAML
00151      05  ELPRL-KEY-LENGTH       PIC S9(4) COMP       VALUE +008.  ELELCAML
00152      05  ELPDE-KEY-LENGTH       PIC S9(4) COMP       VALUE +011.  ELELCAML
00153      05  ELPCV-KEY-LENGTH       PIC S9(4) COMP       VALUE +023.  ELELCAML
00154      05  WS-LOW-VALUES          PIC X         VALUE LOW-VALUES.   ELELCAML
00155 /                                                                 ELELCAML
00156  01  WS-RECORD-LENGTHS.                                           ELELCAML
00157  COPY ELCDRLEN.                                                   ELELCAML
00158 /                                                                 ELELCAML
00159  COPY ELCASETC.                                                   ELELCAML
00160                                                                   ELELCAML
00161  01  CODE-MAN-MAINT-MAP REDEFINES ELCAI01I.                       ELELCAML
00162      05  FILLER                 PIC X(12).                        ELELCAML
00163      05  FUNC-L                 COMP  PIC S9(4).                  ELELCAML
00164      05  FUNC-A                 PIC X.                            ELELCAML
00165      05  FUNC-D                 PIC X(4).                         ELELCAML
00166      05  FILLER                 PIC X.                            ELELCAML
00167      05  TITLE-L                COMP  PIC S9(4).                  ELELCAML
00168      05  TITLE-A                PIC X.                            ELELCAML
00169      05  TITLE-D                PIC X(36).                        ELELCAML
00170      05  FILLER                 PIC X.                            ELELCAML
00171      05  STAT1-L                COMP  PIC S9(4).                  ELELCAML
00172      05  STAT1-A                PIC X.                            ELELCAML
00173      05  STAT1-D                PIC X(09).                        ELELCAML
00174      05  STAT2-L                COMP  PIC S9(4).                  ELELCAML
00175      05  STAT2-A                PIC X.                            ELELCAML
00176      05  STAT2-D                PIC X(08).                        ELELCAML
00177      05  FILLER                 PIC X.                            ELELCAML
00178      05  MAINTENANCE-AREA.                                        ELELCAML
00179          10  MAINTENANCE-LINES  OCCURS 17 TIMES.                  ELELCAML
00180              15  FCN-L              COMP  PIC S9(4).              ELELCAML
00181              15  FCN-A              PIC X.                        ELELCAML
00182              15  FCN-D              PIC X.                        ELELCAML
00183              15  PREF-L             COMP  PIC S9(4).              ELELCAML
00184              15  PREF-A             PIC X.                        ELELCAML
00185              15  PREF-D             PIC X(08).                    ELELCAML
00186              15  FILLER             PIC X.                        ELELCAML
00187              15  NAME-L             COMP  PIC S9(4).              ELELCAML
00188              15  NAME-A             PIC X.                        ELELCAML
00189              15  NAME-D             PIC X(50).                    ELELCAML
00190              15  FILLER             PIC X.                        ELELCAML
00191              15  FILE-L             COMP  PIC S9(4).              ELELCAML
00192              15  FILE-A             PIC X.                        ELELCAML
00193              15  FILE-D             PIC X.                        ELELCAML
00194              15  STATUS-L           COMP  PIC S9(4).              ELELCAML
00195              15  STATUS-A           PIC X.                        ELELCAML
00196              15  STATUS-D           PIC X(08).                    ELELCAML
00197              15  FILLER             PIC X.                        ELELCAML
00198      05  SPREF-L                COMP  PIC S9(4).                  ELELCAML
00199      05  SPREF-A                PIC X.                            ELELCAML
00200      05  SPREF-D                PIC X(08).                        ELELCAML
00201      05  FILLER                 PIC X.                            ELELCAML
00202      05  MSG-L                  COMP  PIC S9(4).                  ELELCAML
00203      05  MSG-A                  PIC X.                            ELELCAML
00204      05  MSG-D                  PIC X(79).                        ELELCAML
00205      05  ERRM-L                 COMP  PIC S9(4).                  ELELCAML
00206      05  ERRM-A                 PIC X.                            ELELCAML
00207      05  ERRM-D                 PIC X(79).                        ELELCAML
00208                                                                   ELELCAML
00209 /                                                                 ELELCAML
00210  01  ATTRIB                     PIC X(10)           VALUE         ELELCAML
00211      'ATTRIBUTES'.                                                ELELCAML
00212  COPY DFHBMSCA.                                                   ELELCAML
00213                                                                   ELELCAML
00214  01  AID-KEYS                   PIC X(08)           VALUE         ELELCAML
00215      'AID-KEYS'.                                                  ELELCAML
00216  COPY DFHAID.                                                     ELELCAML
00217 /                                                                 ELELCAML
00218  01  COMM-AREA.                                                   ELELCAML
00219  COPY ELPCOMMC.                                                   ELELCAML
00220                                                                   ELELCAML
00221  TITLE 'LINKAGE SECTION - ELELCAML '.                             ELELCAML
00222  LINKAGE SECTION.                                                 ELELCAML
00223  01  DFHCOMMAREA                PIC X(500).                       ELELCAML
00224 /                                                                 ELELCAML
00225  01  CIA-PARMS-RECORD.                                            ELELCAML
00226  COPY ELCDCIA.                                                    ELELCAML
00227 /                                                                 ELELCAML
00228  01  IO-PARM-RECORD-LIST.                                         ELELCAML
00229  COPY ELCDIOPM.                                                   ELELCAML
00230 /                                                                 ELELCAML
00231  01  EL-RECORD-LIST.                                              ELELCAML
00232  COPY ELPRLC.                                                     ELELCAML
00233 /                                                                 ELELCAML
00234  01  IO-PARM-DATA-ELEMENT.                                        ELELCAML
00235  COPY ELCDIOP2.                                                   ELELCAML
00236 /                                                                 ELELCAML
00237  01  EL-DATA-ELEMENT.                                             ELELCAML
00238  COPY ELPDEC.                                                     ELELCAML
00239 /                                                                 ELELCAML
00240  01  IO-PARM-CODE-VALUE.                                          ELELCAML
00241  COPY ELCDIOP3.                                                   ELELCAML
00242 /                                                                 ELELCAML
00243  01  EL-CODE-VALUE.                                               ELELCAML
00244  COPY ELPCVC.                                                     ELELCAML
00245                                                                   ELELCAML
00246  TITLE 'PROCEDURE DIVISION  - ELELCAML '.                         ELELCAML
00247  PROCEDURE DIVISION.                                              ELELCAML
00248                                                                   ELELCAML
00249  0001-MAIN-LINE.                                                  ELELCAML
00250                                                                   ELELCAML
00251 *->   SET HANDLE CONDITIONS                                       ELELCAML
00252                                                                   ELELCAML
00253      EXEC CICS HANDLE CONDITION                                   ELELCAML
00254             MAPFAIL(5000-MAPFAIL)                                 ELELCAML
00255      END-EXEC.                                                    ELELCAML
00256                                                                   ELELCAML
00257      EXEC CICS HANDLE AID                                         ELELCAML
00258             CLEAR(9000-RETURN-CICS)                               ELELCAML
00259               PA1(9000-RETURN-CICS)                               ELELCAML
00260               PA2(9000-RETURN-CICS)                               ELELCAML
00261      END-EXEC.                                                    ELELCAML
00262                                                                   ELELCAML
00263 *->   CHECK THE COMMLENGTH                                        ELELCAML
00264                                                                   ELELCAML
00265      IF EIBCALEN = 0                                              ELELCAML
00266           EXEC CICS                                               ELELCAML
00267              ABEND                                                ELELCAML
00268                 ABCODE('COMM')                                    ELELCAML
00269           END-EXEC.                                               ELELCAML
00270                                                                   ELELCAML
00271      MOVE DFHCOMMAREA TO COMM-AREA.                               ELELCAML
00272                                                                   ELELCAML
00273 *->   SET THE INITIAL SWITCH VALUES                               ELELCAML
00274                                                                   ELELCAML
00275      MOVE 'N' TO PF6-SW,                                          ELELCAML
00276                  ERASEAUP-SW                                      ELELCAML
00277                  BEGINNING-FILE-SW.                               ELELCAML
00278      MOVE 0   TO SCREEN-CTR.                                      ELELCAML
00279                                                                   ELELCAML
00280 *->   IF IT IS THE FIRST TIME THROUGH THIS PROGRAM                ELELCAML
00281 *->      CA-SIGNON WILL BE TRUE                                   ELELCAML
00282                                                                   ELELCAML
00283      IF CA-RECORD-LIST                                            ELELCAML
00284         PERFORM 0012-ISSUE-GETMAIN         THRU 0012-EXIT         ELELCAML
00285         PERFORM 1000-RECEIVE-MAP           THRU 1000-EXIT         ELELCAML
00286         PERFORM 1500-VALIDATE-FUNCTIONS    THRU 1500-EXIT         ELELCAML
00287         PERFORM 1600-PROCESS-MAP           THRU 1600-EXIT         ELELCAML
00288      ELSE                                                         ELELCAML
00289        IF CA-SIGNON                                               ELELCAML
00290           PERFORM 0012-ISSUE-GETMAIN       THRU 0012-EXIT         ELELCAML
00291           MOVE LOW-VALUES  TO CA-LAST-RECORD,                     ELELCAML
00292           MOVE HIGH-VALUES TO CA-FIRST-RECORD                     ELELCAML
00293           MOVE LOW-VALUES  TO CODE-MAN-MAINT-MAP                  ELELCAML
00294           PERFORM 0011-INITIAL-MAP-HEADING  THRU 0011-EXIT        ELELCAML
00295           PERFORM 8000-SCROLL-FWD           THRU 8000-EXIT        ELELCAML
00296        ELSE                                                       ELELCAML
00297          PERFORM  0020-SET-ADDRESS          THRU 0020-EXIT        ELELCAML
00298          MOVE CA-FIRST-RECORD TO CA-LAST-RECORD                   ELELCAML
00299          MOVE LOW-VALUES  TO CODE-MAN-MAINT-MAP                   ELELCAML
00300          PERFORM 0011-INITIAL-MAP-HEADING   THRU 0011-EXIT        ELELCAML
00301          PERFORM 8000-SCROLL-FWD            THRU 8000-EXIT.       ELELCAML
00302                                                                   ELELCAML
00303                                                                   ELELCAML
00304  0001-EXIT.  EXIT.                                                ELELCAML
00305 /                                                                 ELELCAML
00306  0011-INITIAL-MAP-HEADING.                                        ELELCAML
00307 ***************************************************************** ELELCAML
00308 *          DISPLAY INITIAL SCREEN MESSAGES                      * ELELCAML
00309 ***************************************************************** ELELCAML
00310                                                                   ELELCAML
00311      MOVE TRANS-ID           TO FUNC-D.                           ELELCAML
00312      MOVE CA-TRANS-HDR       TO TITLE-D.                          ELELCAML
00313                                                                   ELELCAML
00314      IF CA-INQUIRY                                                ELELCAML
00315         MOVE S-TATUS         TO STAT1-D                           ELELCAML
00316         MOVE SPACE           TO STAT2-D                           ELELCAML
00317         MOVE INITIAL-INQUIRY TO MSG-D                             ELELCAML
00318      ELSE                                                         ELELCAML
00319         MOVE STATUS-OR       TO STAT1-D                           ELELCAML
00320         MOVE MAP-FROM        TO STAT2-D.                          ELELCAML
00321                                                                   ELELCAML
00322      IF CA-MAINTENANCE                                            ELELCAML
00323         MOVE INITIAL-MAINT   TO MSG-D.                            ELELCAML
00324                                                                   ELELCAML
00325      IF CA-SUPERVISORY                                            ELELCAML
00326         MOVE INITIAL-SUPERV  TO MSG-D.                            ELELCAML
00327  0011-EXIT.  EXIT.                                                ELELCAML
00328 /                                                                 ELELCAML
00329  0012-ISSUE-GETMAIN.                                              ELELCAML
00330 ***************************************************************** ELELCAML
00331 *       OBTAIN THE STORAGE AREAS FOR CODES MANUAL RECORDS       * ELELCAML
00332 ***************************************************************** ELELCAML
00333                                                                   ELELCAML
00334 *--> GETMAIN FOR CIA PARMS AREA                                   ELELCAML
00335                                                                   ELELCAML
00336      EXEC CICS                                                    ELELCAML
00337           GETMAIN                                                 ELELCAML
00338             SET(ADDRESS OF CIA-PARMS-RECORD)                      ELELCAML
00339             LENGTH(EL-CIA-REC-REC-LEN)                            ELELCAML
00340      END-EXEC.                                                    ELELCAML
00341                                                                   ELELCAML
00342      SET CA-CIA-POINTER  TO ADDRESS OF CIA-PARMS-RECORD.          ELELCAML
00343                                                                   ELELCAML
00344 *--> GETMAIN FOR RECORD LIST I/O PARM                             ELELCAML
00345                                                                   ELELCAML
00346      EXEC CICS                                                    ELELCAML
00347           GETMAIN                                                 ELELCAML
00348             SET(ADDRESS OF IO-PARM-RECORD-LIST)                   ELELCAML
00349             LENGTH(EL-IOPARMS-REC-LEN)                            ELELCAML
00350             INITIMG(WS-LOW-VALUES)                                ELELCAML
00351      END-EXEC.                                                    ELELCAML
00352                                                                   ELELCAML
00353      SET CIA-ELPRL-IOPARM-AREA-PNTR TO                            ELELCAML
00354                 ADDRESS OF IO-PARM-RECORD-LIST.                   ELELCAML
00355      MOVE EL-DSN-ELPRL  TO ELCIO-FILE-DDNAME.                     ELELCAML
00356                                                                   ELELCAML
00357 *--> GETMAIN FOR RECORD LIST                                      ELELCAML
00358                                                                   ELELCAML
00359      EXEC CICS                                                    ELELCAML
00360           GETMAIN                                                 ELELCAML
00361                 SET(ADDRESS OF EL-RECORD-LIST)                    ELELCAML
00362                 LENGTH(EL-ELPRL-REC-LEN)                          ELELCAML
00363                 INITIMG(WS-LOW-VALUES)                            ELELCAML
00364      END-EXEC.                                                    ELELCAML
00365                                                                   ELELCAML
00366      SET CIA-ELPRL-REC-AREA-PNTR TO                               ELELCAML
00367                 ADDRESS OF EL-RECORD-LIST.                        ELELCAML
00368                                                                   ELELCAML
00369 *--> GETMAIN FOR DATA ELEMENT PARMS LIST                          ELELCAML
00370                                                                   ELELCAML
00371      EXEC CICS                                                    ELELCAML
00372           GETMAIN                                                 ELELCAML
00373             SET(ADDRESS OF IO-PARM-DATA-ELEMENT)                  ELELCAML
00374             LENGTH(EL-IOPARMS-REC-LEN)                            ELELCAML
00375             INITIMG(WS-LOW-VALUES)                                ELELCAML
00376      END-EXEC.                                                    ELELCAML
00377                                                                   ELELCAML
00378      SET CIA-ELPDE-IOPARM-AREA-PNTR TO                            ELELCAML
00379                 ADDRESS OF IO-PARM-DATA-ELEMENT.                  ELELCAML
00380      MOVE EL-DSN-ELPDE  TO ELCIO-FILE-DDNAME2.                    ELELCAML
00381                                                                   ELELCAML
00382 *--> GETMAIN FOR DATA ELEMENT LIST                                ELELCAML
00383                                                                   ELELCAML
00384      EXEC CICS                                                    ELELCAML
00385           GETMAIN                                                 ELELCAML
00386                 SET(ADDRESS OF EL-DATA-ELEMENT)                   ELELCAML
00387                 LENGTH(EL-ELPDE-REC-LEN)                          ELELCAML
00388                 INITIMG(WS-LOW-VALUES)                            ELELCAML
00389      END-EXEC.                                                    ELELCAML
00390                                                                   ELELCAML
00391      SET CIA-ELPDE-REC-AREA-PNTR TO                               ELELCAML
00392                 ADDRESS OF EL-DATA-ELEMENT.                       ELELCAML
00393                                                                   ELELCAML
00394 *--> GETMAIN FOR CODE VALUE PARMS LIST                            ELELCAML
00395                                                                   ELELCAML
00396      EXEC CICS                                                    ELELCAML
00397           GETMAIN                                                 ELELCAML
00398             SET(ADDRESS OF IO-PARM-CODE-VALUE)                    ELELCAML
00399             LENGTH(EL-IOPARMS-REC-LEN)                            ELELCAML
00400             INITIMG(WS-LOW-VALUES)                                ELELCAML
00401      END-EXEC.                                                    ELELCAML
00402                                                                   ELELCAML
00403      SET CIA-ELPCV-IOPARM-AREA-PNTR TO                            ELELCAML
00404                 ADDRESS OF IO-PARM-CODE-VALUE.                    ELELCAML
00405                                                                   ELELCAML
00406      MOVE EL-DSN-ELPCV  TO ELCIO-FILE-DDNAME3.                    ELELCAML
00407 *--> GETMAIN FOR CODE VALUE                                       ELELCAML
00408                                                                   ELELCAML
00409      EXEC CICS                                                    ELELCAML
00410           GETMAIN                                                 ELELCAML
00411                 SET(ADDRESS OF EL-CODE-VALUE)                     ELELCAML
00412                 LENGTH(EL-ELPCV-REC-LEN)                          ELELCAML
00413                 INITIMG(WS-LOW-VALUES)                            ELELCAML
00414      END-EXEC.                                                    ELELCAML
00415                                                                   ELELCAML
00416      SET CIA-ELPCV-REC-AREA-PNTR TO                               ELELCAML
00417                 ADDRESS OF EL-CODE-VALUE.                         ELELCAML
00418                                                                   ELELCAML
00419                                                                   ELELCAML
00420  0012-EXIT.   EXIT.                                               ELELCAML
00421 /                                                                 ELELCAML
00422  0020-SET-ADDRESS.                                                ELELCAML
00423                                                                   ELELCAML
00424      SET ADDRESS OF CIA-PARMS-RECORD TO CA-CIA-POINTER.           ELELCAML
00425                                                                   ELELCAML
00426      SET ADDRESS OF IO-PARM-RECORD-LIST TO                        ELELCAML
00427           CIA-ELPRL-IOPARM-AREA-PNTR.                             ELELCAML
00428                                                                   ELELCAML
00429      SET ADDRESS OF EL-RECORD-LIST TO                             ELELCAML
00430           CIA-ELPRL-REC-AREA-PNTR.                                ELELCAML
00431                                                                   ELELCAML
00432      SET ADDRESS OF IO-PARM-DATA-ELEMENT TO                       ELELCAML
00433           CIA-ELPDE-IOPARM-AREA-PNTR.                             ELELCAML
00434                                                                   ELELCAML
00435      SET ADDRESS OF EL-DATA-ELEMENT TO                            ELELCAML
00436           CIA-ELPDE-REC-AREA-PNTR.                                ELELCAML
00437                                                                   ELELCAML
00438      SET ADDRESS OF IO-PARM-CODE-VALUE TO                         ELELCAML
00439           CIA-ELPCV-IOPARM-AREA-PNTR.                             ELELCAML
00440                                                                   ELELCAML
00441      SET ADDRESS OF EL-CODE-VALUE TO                              ELELCAML
00442           CIA-ELPCV-REC-AREA-PNTR.                                ELELCAML
00443                                                                   ELELCAML
00444  0020-EXIT.   EXIT.                                               ELELCAML
00445 /                                                                 ELELCAML
00446  1000-RECEIVE-MAP.                                                ELELCAML
00447      EXEC CICS                                                    ELELCAML
00448         RECEIVE MAP('ELCAI01')                                    ELELCAML
00449                 MAPSET('ELCASET')                                 ELELCAML
00450      END-EXEC.                                                    ELELCAML
00451  1000-EXIT.   EXIT.                                               ELELCAML
00452 /                                                                 ELELCAML
00453  1500-VALIDATE-FUNCTIONS.                                         ELELCAML
00454      IF EIBAID = DFHPF6                                           ELELCAML
00455           GO TO 6000-CONFIRM.                                     ELELCAML
00456                                                                   ELELCAML
00457      IF EIBAID = DFHPF7                                           ELELCAML
00458           GO TO 7000-SCROLL-BACK.                                 ELELCAML
00459                                                                   ELELCAML
00460      IF EIBAID = DFHPF8                                           ELELCAML
00461           GO TO 8000-SCROLL-FWD.                                  ELELCAML
00462                                                                   ELELCAML
00463      PERFORM 2000-SEARCH-MAP-FOR-DMU  THRU 2000-EXIT.             ELELCAML
00464 ****  RESET SCREEN COUNTER FIELD ****                             ELELCAML
00465      MOVE 0 TO SCREEN-CTR.                                        ELELCAML
00466                                                                   ELELCAML
00467 ****** PROCESSING ERROR CHECKS ********                           ELELCAML
00468      IF CA-INQUIRY                                                ELELCAML
00469          GO TO 1500-INQUIRY-VALIDATE.                             ELELCAML
00470                                                                   ELELCAML
00471      IF INVALID-17-SW = 'Y'                                       ELELCAML
00472          MOVE INVALID-SELECT-17        TO ERRM-D                  ELELCAML
00473          MOVE DFHBMASB                 TO ERRM-A                  ELELCAML
00474          MOVE 'A'                      TO CA-CURRENT-PGM          ELELCAML
00475          MOVE SPACE                    TO CA-CURRENT-FUNCTION     ELELCAML
00476          PERFORM 1800-SET-ATTRIBUTES   THRU 1800-EXIT             ELELCAML
00477          PERFORM 3000-SEND-SCREEN      THRU 3000-EXIT.            ELELCAML
00478                                                                   ELELCAML
00479      IF MAP-CTR > 1                                               ELELCAML
00480          MOVE MULTI-MAP-FR-NOT-ALLOW   TO ERRM-D                  ELELCAML
00481          MOVE DFHBMASB                 TO ERRM-A                  ELELCAML
00482          MOVE 'A'                      TO CA-CURRENT-PGM          ELELCAML
00483          MOVE SPACE                    TO CA-CURRENT-FUNCTION     ELELCAML
00484          PERFORM 1800-SET-ATTRIBUTES   THRU 1800-EXIT             ELELCAML
00485          PERFORM 3000-SEND-SCREEN      THRU 3000-EXIT.            ELELCAML
00486                                                                   ELELCAML
00487      IF ADD-ERROR = 'Y'                                           ELELCAML
00488          MOVE ADD-MUST-BE-LAST-LINE    TO ERRM-D                  ELELCAML
00489          MOVE DFHBMASB                 TO ERRM-A                  ELELCAML
00490          MOVE 'A'                      TO CA-CURRENT-PGM          ELELCAML
00491          MOVE SPACE                    TO CA-CURRENT-FUNCTION     ELELCAML
00492          PERFORM 1800-SET-ATTRIBUTES   THRU 1800-EXIT             ELELCAML
00493          PERFORM 3000-SEND-SCREEN      THRU 3000-EXIT.            ELELCAML
00494                                                                   ELELCAML
00495  1500-INQUIRY-VALIDATE.                                           ELELCAML
00496      IF SELECT-CTR > 1                                            ELELCAML
00497         OR (SELECT-CTR = 1 AND SPREF-L > +0)                      ELELCAML
00498            MOVE MORE-THAN-ONE-SELECT   TO ERRM-D                  ELELCAML
00499            MOVE DFHBMASB               TO ERRM-A                  ELELCAML
00500            PERFORM 1800-SET-ATTRIBUTES   THRU 1800-EXIT           ELELCAML
00501            PERFORM 3000-SEND-SCREEN      THRU 3000-EXIT.          ELELCAML
00502                                                                   ELELCAML
00503      IF EIBAID = DFHPF4                                           ELELCAML
00504         IF SELECT-CTR > 0                                         ELELCAML
00505            IF DCU-CTR > 0 OR MAP-CTR > 0                          ELELCAML
00506               MOVE INVALID-USE-PF4     TO ERRM-D                  ELELCAML
00507               MOVE DFHBMASB            TO ERRM-A                  ELELCAML
00508               MOVE -1                  TO FCN-L (1)               ELELCAML
00509               PERFORM 1800-SET-ATTRIBUTES   THRU 1800-EXIT        ELELCAML
00510               PERFORM 3000-SEND-SCREEN      THRU 3000-EXIT        ELELCAML
00511            ELSE                                                   ELELCAML
00512               NEXT SENTENCE                                       ELELCAML
00513         ELSE                                                      ELELCAML
00514            MOVE INVALID-USE-PF4        TO ERRM-D                  ELELCAML
00515            MOVE DFHBMASB               TO ERRM-A                  ELELCAML
00516            MOVE -1                     TO FCN-L (1)               ELELCAML
00517            PERFORM 1800-SET-ATTRIBUTES   THRU 1800-EXIT           ELELCAML
00518            PERFORM 3000-SEND-SCREEN      THRU 3000-EXIT.          ELELCAML
00519                                                                   ELELCAML
00520      IF ERROR-SW = 'Y'                                            ELELCAML
00521            PERFORM 1800-SET-ATTRIBUTES   THRU 1800-EXIT           ELELCAML
00522            PERFORM 3000-SEND-SCREEN      THRU 3000-EXIT.          ELELCAML
00523                                                                   ELELCAML
00524      IF DCU-CTR = 0                                               ELELCAML
00525         AND SELECT-CTR = 0                                        ELELCAML
00526            AND MAP-CTR = 0                                        ELELCAML
00527               AND SPREF-L = 0                                     ELELCAML
00528                  AND FCN-D (MAX-PROCESS-LINE) NOT = 'A'           ELELCAML
00529                     IF EIBAID = DFHENTER                          ELELCAML
00530                        PERFORM 8000-SCROLL-FWD  THRU              ELELCAML
00531                                8000-EXIT                          ELELCAML
00532                     ELSE                                          ELELCAML
00533                        PERFORM 5000-MAPFAIL    THRU               ELELCAML
00534                                5000-EXIT.                         ELELCAML
00535                                                                   ELELCAML
00536 *** FROM 4400-PROCESS-SELECT CONTROL IS PAST TO ELCBPGM  ***      ELELCAML
00537 ***           OR ELCCPGM                                 ***      ELELCAML
00538      IF SPREF-L > +0  OR                                          ELELCAML
00539         SELECT-POSITION > +0                                      ELELCAML
00540          PERFORM  4400-PROCESS-SELECT  THRU                       ELELCAML
00541                   4400-EXIT.                                      ELELCAML
00542                                                                   ELELCAML
00543      IF CA-INQUIRY                                                ELELCAML
00544          MOVE UNAUTHOR-MAINTAIN        TO ERRM-D                  ELELCAML
00545          MOVE DFHBMASB                 TO ERRM-A                  ELELCAML
00546          MOVE SPACE                    TO CA-CURRENT-FUNCTION     ELELCAML
00547          PERFORM 3000-SEND-SCREEN      THRU 3000-EXIT.            ELELCAML
00548                                                                   ELELCAML
00549      IF MAP-CTR = 1                                               ELELCAML
00550          PERFORM 2100-MAP-FUNCTION-INITAL                         ELELCAML
00551             THRU 2100-EXIT                                        ELELCAML
00552          MOVE 'M'                      TO CA-CURRENT-FUNCTION     ELELCAML
00553          MOVE DFHBMASB                 TO ERRM-A                  ELELCAML
00554          MOVE 1                        TO SCREEN-CTR              ELELCAML
00555          PERFORM 1800-SET-ATTRIBUTES   THRU 1800-EXIT             ELELCAML
00556          PERFORM 3000-SEND-SCREEN      THRU 3000-EXIT.            ELELCAML
00557                                                                   ELELCAML
00558      IF DELETE-OR-UNDELETE = 'Y'                                  ELELCAML
00559          MOVE 'D' TO CA-CURRENT-FUNCTION                          ELELCAML
00560          MOVE DELETE-UNDELETE-CONFIRM  TO ERRM-D                  ELELCAML
00561          MOVE DFHBMASB                 TO ERRM-A                  ELELCAML
00562          MOVE 1                        TO SCREEN-CTR              ELELCAML
00563          MOVE -1                       TO FCN-L (SCREEN-CTR)      ELELCAML
00564          PERFORM 1800-SET-ATTRIBUTES   THRU 1800-EXIT             ELELCAML
00565          PERFORM 3000-SEND-SCREEN      THRU 3000-EXIT.            ELELCAML
00566                                                                   ELELCAML
00567      IF FCN-D (MAX-PROCESS-LINE) = 'A'                            ELELCAML
00568        IF PREF-L (MAX-PROCESS-LINE) NOT > +0                      ELELCAML
00569            MOVE RECORD-PREF-REQ-ADD    TO ERRM-D                  ELELCAML
00570            MOVE -1                  TO PREF-L (MAX-PROCESS-LINE)  ELELCAML
00571            PERFORM 1800-SET-ATTRIBUTES   THRU 1800-EXIT           ELELCAML
00572            PERFORM 3000-SEND-SCREEN      THRU 3000-EXIT           ELELCAML
00573        ELSE                                                       ELELCAML
00574          MOVE MAX-PROCESS-LINE         TO SCREEN-CTR              ELELCAML
00575          PERFORM 4000-PROCESS-ADD        THRU 4000-EXIT           ELELCAML
00576          IF ADD-ERROR = 'Y'                                       ELELCAML
00577            PERFORM 1800-SET-ATTRIBUTES   THRU 1800-EXIT           ELELCAML
00578            PERFORM 3000-SEND-SCREEN      THRU 3000-EXIT           ELELCAML
00579          ELSE                                                     ELELCAML
00580            EXEC CICS                                              ELELCAML
00581                XCTL PROGRAM('ELELCCML')                           ELELCAML
00582                     COMMAREA(COMM-AREA)                           ELELCAML
00583                     LENGTH(COMM-LENGTH)                           ELELCAML
00584            END-EXEC.                                              ELELCAML
00585                                                                   ELELCAML
00586  1500-EXIT.    EXIT.                                              ELELCAML
00587 /                                                                 ELELCAML
00588  1600-PROCESS-MAP.                                                ELELCAML
00589      IF ERROR-SW = 'Y'                                            ELELCAML
00590          PERFORM 1800-SET-ATTRIBUTES   THRU 1800-EXIT             ELELCAML
00591          PERFORM 3000-SEND-SCREEN      THRU 3000-EXIT             ELELCAML
00592          GO TO 1600-EXIT.                                         ELELCAML
00593                                                                   ELELCAML
00594      ADD 1  TO SCREEN-CTR.                                        ELELCAML
00595      IF SCREEN-CTR > MAX-DISPLAY-LINE                             ELELCAML
00596          MOVE SPACE                TO CA-CURRENT-FUNCTION         ELELCAML
00597          MOVE UPDATE-SUCCESSFUL    TO ERRM-D                      ELELCAML
00598          MOVE DFHBMASB             TO ERRM-A                      ELELCAML
00599          MOVE 'A'                  TO CA-CURRENT-PGM              ELELCAML
00600          MOVE 1                    TO SCREEN-CTR                  ELELCAML
00601          MOVE -1                   TO FCN-L (SCREEN-CTR)          ELELCAML
00602          PERFORM 1800-SET-ATTRIBUTES   THRU 1800-EXIT             ELELCAML
00603          PERFORM 3000-SEND-SCREEN      THRU 3000-EXIT.            ELELCAML
00604                                                                   ELELCAML
00605      IF FCN-L (SCREEN-CTR) > +0                                   ELELCAML
00606          NEXT SENTENCE                                            ELELCAML
00607      ELSE                                                         ELELCAML
00608        GO TO 1600-PROCESS-MAP.                                    ELELCAML
00609                                                                   ELELCAML
00610 ***** SPACE IS VALID *****                                        ELELCAML
00611      IF FCN-D (SCREEN-CTR) = SPACE                                ELELCAML
00612          GO TO 1600-PROCESS-MAP.                                  ELELCAML
00613                                                                   ELELCAML
00614      MOVE FCN-D (SCREEN-CTR) TO CA-CURRENT-FUNCTION.              ELELCAML
00615      IF CA-CHANGE                                                 ELELCAML
00616         PERFORM 4100-PROCESS-CHANGE                               ELELCAML
00617            THRU 4100-EXIT                                         ELELCAML
00618         GO TO 1600-PROCESS-MAP.                                   ELELCAML
00619                                                                   ELELCAML
00620      IF CA-DELETE OR CA-UNDELETE                                  ELELCAML
00621         PERFORM 4200-PROCESS-DEL-UNDEL                            ELELCAML
00622            THRU 4200-EXIT                                         ELELCAML
00623         GO TO 1600-PROCESS-MAP.                                   ELELCAML
00624                                                                   ELELCAML
00625      IF CA-MAP-FROM                                               ELELCAML
00626         PERFORM 4300-PROCESS-MAP-FUNCTION                         ELELCAML
00627            THRU 4300-EXIT                                         ELELCAML
00628         GO TO 1600-PROCESS-MAP.                                   ELELCAML
00629                                                                   ELELCAML
00630  1600-EXIT.     EXIT.                                             ELELCAML
00631 /                                                                 ELELCAML
00632  1800-SET-ATTRIBUTES.                                             ELELCAML
00633                                                                   ELELCAML
00634      IF SCREEN-CTR < 1                                            ELELCAML
00635           MOVE 1 TO SCREEN-CTR.                                   ELELCAML
00636                                                                   ELELCAML
00637      IF SCREEN-CTR > MAX-DISPLAY-LINE                             ELELCAML
00638          PERFORM 1850-CHECK-LAST  THRU 1850-EXIT                  ELELCAML
00639          GO TO 1800-EXIT.                                         ELELCAML
00640                                                                   ELELCAML
00641      IF FCN-L (SCREEN-CTR) = -1                                   ELELCAML
00642         NEXT SENTENCE                                             ELELCAML
00643      ELSE                                                         ELELCAML
00644         MOVE DFHBMFSE TO FCN-A (SCREEN-CTR).                      ELELCAML
00645                                                                   ELELCAML
00646      IF CA-INQUIRY                                                ELELCAML
00647          MOVE DFHBMUNP TO NAME-A (SCREEN-CTR)                     ELELCAML
00648                           FILE-A (SCREEN-CTR)                     ELELCAML
00649                           STATUS-A (SCREEN-CTR)                   ELELCAML
00650          ADD 1 TO SCREEN-CTR                                      ELELCAML
00651          GO TO 1800-SET-ATTRIBUTES.                               ELELCAML
00652                                                                   ELELCAML
00653      IF NAME-L (SCREEN-CTR) > +0                                  ELELCAML
00654         MOVE DFHBMFSE TO NAME-A (SCREEN-CTR)                      ELELCAML
00655      ELSE                                                         ELELCAML
00656       MOVE DFHBMUNP TO NAME-A (SCREEN-CTR).                       ELELCAML
00657                                                                   ELELCAML
00658      IF FILE-L (SCREEN-CTR) = -1                                  ELELCAML
00659          NEXT SENTENCE                                            ELELCAML
00660      ELSE                                                         ELELCAML
00661          IF FILE-L (SCREEN-CTR) > +0                              ELELCAML
00662             MOVE DFHBMFSE TO FILE-A (SCREEN-CTR)                  ELELCAML
00663          ELSE                                                     ELELCAML
00664             MOVE DFHBMUNP TO FILE-A (SCREEN-CTR).                 ELELCAML
00665                                                                   ELELCAML
00666      IF STATUS-D (SCREEN-CTR) = DELETED                           ELELCAML
00667         MOVE DFHBMUBF TO STATUS-A (SCREEN-CTR)                    ELELCAML
00668      ELSE                                                         ELELCAML
00669         IF STATUS-L (SCREEN-CTR) = -1                             ELELCAML
00670            NEXT SENTENCE                                          ELELCAML
00671         ELSE                                                      ELELCAML
00672            IF STATUS-L (SCREEN-CTR) > +0                          ELELCAML
00673               MOVE DFHBMFSE TO STATUS-A (SCREEN-CTR)              ELELCAML
00674            ELSE                                                   ELELCAML
00675               MOVE DFHBMUNP TO STATUS-A (SCREEN-CTR).             ELELCAML
00676                                                                   ELELCAML
00677      IF PREF-L (SCREEN-CTR) = -1                                  ELELCAML
00678         NEXT SENTENCE                                             ELELCAML
00679      ELSE                                                         ELELCAML
00680         IF ERASEAUP-SW = 'Y'                                      ELELCAML
00681            MOVE DFHBMUNP TO PREF-A (SCREEN-CTR)                   ELELCAML
00682         ELSE                                                      ELELCAML
00683            MOVE DFHBMPRF TO PREF-A (SCREEN-CTR).                  ELELCAML
00684                                                                   ELELCAML
00685      ADD 1 TO SCREEN-CTR.                                         ELELCAML
00686      GO TO 1800-SET-ATTRIBUTES.                                   ELELCAML
00687                                                                   ELELCAML
00688  1800-EXIT.    EXIT.                                              ELELCAML
00689                                                                   ELELCAML
00690 /                                                                 ELELCAML
00691  1850-CHECK-LAST.                                                 ELELCAML
00692      MOVE DFHBMPRF        TO FUNC-A.                              ELELCAML
00693      MOVE DFHBMASB        TO TITLE-A,                             ELELCAML
00694                              MSG-A.                               ELELCAML
00695      IF CA-INQUIRY                                                ELELCAML
00696          MOVE DFHBMUNP        TO NAME-A (MAX-PROCESS-LINE)        ELELCAML
00697                                  FILE-A (MAX-PROCESS-LINE)        ELELCAML
00698                                  STATUS-A (MAX-PROCESS-LINE)      ELELCAML
00699                                  FCN-A (MAX-PROCESS-LINE)         ELELCAML
00700                                  PREF-A (MAX-PROCESS-LINE)        ELELCAML
00701          MOVE DFHBMFSE        TO SPREF-A                          ELELCAML
00702          GO TO 1850-EXIT.                                         ELELCAML
00703                                                                   ELELCAML
00704      IF FCN-L (MAX-PROCESS-LINE) > +0                             ELELCAML
00705           MOVE DFHBMFSE TO FCN-A (MAX-PROCESS-LINE),              ELELCAML
00706                            STATUS-A (MAX-PROCESS-LINE).           ELELCAML
00707      IF PREF-L (MAX-PROCESS-LINE) > +0                            ELELCAML
00708           MOVE DFHBMFSE TO PREF-A (MAX-PROCESS-LINE),             ELELCAML
00709      IF NAME-L (MAX-PROCESS-LINE) > +0                            ELELCAML
00710           MOVE DFHBMFSE TO NAME-A (MAX-PROCESS-LINE).             ELELCAML
00711      IF FILE-L (MAX-PROCESS-LINE) > +0                            ELELCAML
00712           MOVE DFHBMFSE TO FILE-A (MAX-PROCESS-LINE).             ELELCAML
00713      IF SPREF-L > +0                                              ELELCAML
00714          MOVE DFHBMFSE TO SPREF-A.                                ELELCAML
00715                                                                   ELELCAML
00716  1850-EXIT.    EXIT.                                              ELELCAML
00717 /                                                                 ELELCAML
00718  2000-SEARCH-MAP-FOR-DMU.                                         ELELCAML
00719      MOVE 0 TO DCU-CTR,                                           ELELCAML
00720                MAP-CTR,                                           ELELCAML
00721                SELECT-CTR.                                        ELELCAML
00722                                                                   ELELCAML
00723      MOVE 'N' TO ADD-ERROR,                                       ELELCAML
00724                  DELETE-OR-UNDELETE,                              ELELCAML
00725                  ERROR-SW,                                        ELELCAML
00726                  INVALID-17-SW.                                   ELELCAML
00727                                                                   ELELCAML
00728      MOVE +0     TO SELECT-POSITION.                              ELELCAML
00729      IF FCN-L (MAX-PROCESS-LINE) > +0                             ELELCAML
00730         IF FCN-D (MAX-PROCESS-LINE) = 'M'                         ELELCAML
00731            MOVE MAX-PROCESS-LINE TO MAP-POSITION                  ELELCAML
00732            ADD 1 TO MAP-CTR                                       ELELCAML
00733         ELSE                                                      ELELCAML
00734          IF FCN-D (MAX-PROCESS-LINE) NOT = 'A'                    ELELCAML
00735              MOVE DFHBMUBF TO FCN-A (MAX-PROCESS-LINE)            ELELCAML
00736              MOVE 'Y' TO INVALID-17-SW                            ELELCAML
00737              MOVE -1  TO FCN-L (MAX-PROCESS-LINE)                 ELELCAML
00738              GO TO 2000-EXIT.                                     ELELCAML
00739 /                                                                 ELELCAML
00740  2000-SEARCH-LOOP.                                                ELELCAML
00741      ADD 1 TO SCREEN-CTR.                                         ELELCAML
00742      IF SCREEN-CTR > 16                                           ELELCAML
00743          GO TO 2000-EXIT.                                         ELELCAML
00744                                                                   ELELCAML
00745      IF CA-INQUIRY                                                ELELCAML
00746         IF FCN-D (SCREEN-CTR) = 'A' OR 'C' OR 'D'                 ELELCAML
00747                                     OR 'M' OR 'U'                 ELELCAML
00748              MOVE DFHBMUBF TO FCN-A (SCREEN-CTR)                  ELELCAML
00749              MOVE -1              TO FCN-L (SCREEN-CTR)           ELELCAML
00750 ***** THIS MOVE FORCES THE INQUIRY MAINTENANCE MESSAGE TO OCCUR   ELELCAML
00751              MOVE  1 TO DCU-CTR                                   ELELCAML
00752              GO TO 2000-EXIT.                                     ELELCAML
00753                                                                   ELELCAML
00754      IF FCN-D (SCREEN-CTR) = SPACE OR                             ELELCAML
00755        FCN-D (SCREEN-CTR) = LOW-VALUES                            ELELCAML
00756         IF NAME-L (SCREEN-CTR) > +0 OR                            ELELCAML
00757            FILE-L (SCREEN-CTR) > +0                               ELELCAML
00758               MOVE -1 TO FCN-L (SCREEN-CTR)                       ELELCAML
00759               MOVE 'Y' TO ERROR-SW                                ELELCAML
00760               MOVE INVALID-SELECT  TO ERRM-D                      ELELCAML
00761               GO TO 2000-EXIT.                                    ELELCAML
00762                                                                   ELELCAML
00763      IF FCN-L (SCREEN-CTR) > +0                                   ELELCAML
00764          NEXT SENTENCE                                            ELELCAML
00765      ELSE                                                         ELELCAML
00766       GO TO 2000-SEARCH-LOOP.                                     ELELCAML
00767                                                                   ELELCAML
00768 *** SPACE IS VALID                                                ELELCAML
00769      IF FCN-D (SCREEN-CTR) = SPACE                                ELELCAML
00770          GO TO 2000-SEARCH-LOOP.                                  ELELCAML
00771                                                                   ELELCAML
00772      IF FCN-D (SCREEN-CTR) = 'A'                                  ELELCAML
00773          MOVE 'Y' TO ADD-ERROR                                    ELELCAML
00774          MOVE DFHBMUBF TO FCN-A (SCREEN-CTR)                      ELELCAML
00775          MOVE -1  TO FCN-L (SCREEN-CTR)                           ELELCAML
00776          GO TO 2000-EXIT.                                         ELELCAML
00777                                                                   ELELCAML
00778      IF FCN-D (SCREEN-CTR) = 'M'                                  ELELCAML
00779          ADD 1 TO MAP-CTR                                         ELELCAML
00780          IF MAP-CTR > 1                                           ELELCAML
00781              MOVE DFHBMUBF TO FCN-A (SCREEN-CTR)                  ELELCAML
00782              MOVE 'Y'             TO ERROR-SW                     ELELCAML
00783              MOVE -1              TO FCN-L (SCREEN-CTR)           ELELCAML
00784              GO TO 2000-EXIT                                      ELELCAML
00785          ELSE                                                     ELELCAML
00786           MOVE SCREEN-CTR TO MAP-POSITION                         ELELCAML
00787           GO TO 2000-SEARCH-LOOP.                                 ELELCAML
00788                                                                   ELELCAML
00789      IF FCN-D (SCREEN-CTR) = 'D' OR 'U'                           ELELCAML
00790          ADD 1 TO DCU-CTR                                         ELELCAML
00791          MOVE 'Y' TO DELETE-OR-UNDELETE                           ELELCAML
00792          GO TO 2000-SEARCH-LOOP.                                  ELELCAML
00793                                                                   ELELCAML
00794      IF FCN-D (SCREEN-CTR) = 'C'                                  ELELCAML
00795          ADD 1 TO DCU-CTR                                         ELELCAML
00796          GO TO 2000-SEARCH-LOOP.                                  ELELCAML
00797                                                                   ELELCAML
00798      IF FCN-D (SCREEN-CTR) = 'S'                                  ELELCAML
00799          ADD 1 TO SELECT-CTR                                      ELELCAML
00800          MOVE SCREEN-CTR TO SELECT-POSITION                       ELELCAML
00801          IF SELECT-CTR > 1                                        ELELCAML
00802              MOVE DFHBMUBF TO FCN-A (SCREEN-CTR)                  ELELCAML
00803              MOVE 'Y'             TO ERROR-SW                     ELELCAML
00804              MOVE -1              TO FCN-L (SCREEN-CTR)           ELELCAML
00805              GO TO 2000-EXIT                                      ELELCAML
00806          ELSE                                                     ELELCAML
00807           GO TO 2000-SEARCH-LOOP.                                 ELELCAML
00808                                                                   ELELCAML
00809      MOVE 'Y' TO ERROR-SW.                                        ELELCAML
00810      MOVE INVALID-SELECT  TO ERRM-D.                              ELELCAML
00811      MOVE DFHBMUBF TO FCN-A (SCREEN-CTR).                         ELELCAML
00812      MOVE -1              TO FCN-L (SCREEN-CTR).                  ELELCAML
00813      MOVE DFHBMASB TO ERRM-A.                                     ELELCAML
00814                                                                   ELELCAML
00815  2000-EXIT.    EXIT.                                              ELELCAML
00816 /                                                                 ELELCAML
00817  2100-MAP-FUNCTION-INITAL.                                        ELELCAML
00818      MOVE MAP-POSITION TO SCREEN-CTR.                             ELELCAML
00819      MOVE 'Y'          TO OVERLAY-SW.                             ELELCAML
00820      MOVE -1           TO FCN-L (SCREEN-CTR).                     ELELCAML
00821      IF SCREEN-CTR NOT = MAX-PROCESS-LINE                         ELELCAML
00822          MOVE PREF-D (SCREEN-CTR) TO LOCATE-PREFIX                ELELCAML
00823          PERFORM 2200-FIND-MAP-FROM-REC                           ELELCAML
00824             THRU 2200-EXIT                                        ELELCAML
00825          IF DELETE-SW = 'Y'                                       ELELCAML
00826             MOVE MAP-NOT-ALLOW-TO-DELETE TO ERRM-D                ELELCAML
00827             GO TO 2100-EXIT.                                      ELELCAML
00828                                                                   ELELCAML
00829      IF SCREEN-CTR = MAX-PROCESS-LINE                             ELELCAML
00830          MOVE PREF-D (SCREEN-CTR) TO LOCATE-PREFIX                ELELCAML
00831          MOVE 'N' TO OVERLAY-SW                                   ELELCAML
00832          PERFORM 2200-FIND-MAP-FROM-REC                           ELELCAML
00833             THRU 2200-EXIT                                        ELELCAML
00834          IF FOUND-SW = 'Y'                                        ELELCAML
00835             MOVE 'Y' TO OVERLAY-SW.                               ELELCAML
00836      MOVE STATUS-D (SCREEN-CTR) TO LOCATE-PREFIX,                 ELELCAML
00837                                    CA-MF-RECORD-PREFIX.           ELELCAML
00838      PERFORM 2200-FIND-MAP-FROM-REC  THRU                         ELELCAML
00839              2200-EXIT.                                           ELELCAML
00840                                                                   ELELCAML
00841      IF DELETE-SW = 'Y'                                           ELELCAML
00842         MOVE MAP-NOT-ALLOW-FROM-DELETE TO ERRM-D                  ELELCAML
00843         GO TO 2100-EXIT.                                          ELELCAML
00844                                                                   ELELCAML
00845      IF FOUND-SW = 'Y'                                            ELELCAML
00846        IF OVERLAY-SW = 'Y'                                        ELELCAML
00847           IF CA-SUPERVISORY                                       ELELCAML
00848             MOVE MAP-OVERLAY-MSG TO ERRM-D                        ELELCAML
00849           ELSE                                                    ELELCAML
00850             MOVE UNAUTHOR-OVERLAY TO ERRM-D                       ELELCAML
00851             MOVE DFHBMUBF         TO FCN-A (SCREEN-CTR)           ELELCAML
00852        ELSE                                                       ELELCAML
00853          MOVE MAP-FROM-MSG      TO ERRM-D                         ELELCAML
00854      ELSE                                                         ELELCAML
00855       MOVE 0               TO FCN-L (SCREEN-CTR)                  ELELCAML
00856       MOVE DFHBMUBF TO STATUS-A (SCREEN-CTR)                      ELELCAML
00857       MOVE -1              TO STATUS-L (SCREEN-CTR)               ELELCAML
00858       MOVE MAP-FROM-NOTFND TO ERRM-D.                             ELELCAML
00859                                                                   ELELCAML
00860  2100-EXIT.   EXIT.                                               ELELCAML
00861 /                                                                 ELELCAML
00862  2200-FIND-MAP-FROM-REC.                                          ELELCAML
00863      MOVE 'N' TO FOUND-SW,                                        ELELCAML
00864                  DELETE-SW.                                       ELELCAML
00865      MOVE LOCATE-PREFIX    TO RL-RECORD-PREFIX.                   ELELCAML
00866      MOVE 'RD '            TO ELCIO-FILE-ACCESS-CODE.             ELELCAML
00867      MOVE EL-ELPRL-REC-LEN TO ELCIO-MAX-REC-LEN.                  ELELCAML
00868      MOVE RL-RECORD-PREFIX TO ELCIO-VSAM-KEY.                     ELELCAML
00869      MOVE EL-DSN-ELPRL     TO ELCIO-FILE-DDNAME,                  ELELCAML
00870                               CIA-IO-GETMAIN-DDNAME.              ELELCAML
00871      MOVE 'M'              TO ELCIO-STORAGE.                      ELELCAML
00872                                                                   ELELCAML
00873      SET  CIA-IO-PARM-AREA-PNTR        TO                         ELELCAML
00874           CIA-ELPRL-IOPARM-AREA-PNTR.                             ELELCAML
00875      SET  ELCIO-REC-AREA-ADDRESS       TO                         ELELCAML
00876           CIA-ELPRL-REC-AREA-PNTR.                                ELELCAML
00877      EXEC CICS LINK                                               ELELCAML
00878           PROGRAM('ELAIOPGM')                                     ELELCAML
00879           COMMAREA(ADDRESS OF  CIA-PARMS-RECORD)                  ELELCAML
00880           LENGTH(EL-CIA-POINTER-LEN)                              ELELCAML
00881      END-EXEC.                                                    ELELCAML
00882                                                                   ELELCAML
00883      IF ELCIO-GOOD-RETURN                                         ELELCAML
00884         MOVE 'Y' TO FOUND-SW                                      ELELCAML
00885         IF RL-DELETE                                              ELELCAML
00886            MOVE 'Y' TO DELETE-SW.                                 ELELCAML
00887                                                                   ELELCAML
00888  2200-EXIT.    EXIT.                                              ELELCAML
00889 /                                                                 ELELCAML
00890  3000-SEND-SCREEN.                                                ELELCAML
00891      IF CA-RECORD-LIST                                            ELELCAML
00892        IF ERASEAUP-SW = 'Y'                                       ELELCAML
00893             EXEC CICS                                             ELELCAML
00894                SEND                                               ELELCAML
00895                 MAP('ELCAI01')                                    ELELCAML
00896                 MAPSET('ELCASET')                                 ELELCAML
00897                 DATAONLY                                          ELELCAML
00898                 ERASEAUP                                          ELELCAML
00899             END-EXEC                                              ELELCAML
00900        ELSE                                                       ELELCAML
00901          EXEC CICS                                                ELELCAML
00902             SEND                                                  ELELCAML
00903              MAP('ELCAI01')                                       ELELCAML
00904              MAPSET('ELCASET')                                    ELELCAML
00905              DATAONLY                                             ELELCAML
00906              CURSOR                                               ELELCAML
00907          END-EXEC                                                 ELELCAML
00908      ELSE                                                         ELELCAML
00909        MOVE 'A' TO CA-CURRENT-PGM                                 ELELCAML
00910        MOVE LOW-VALUES TO CA-ELEMENT-SCROLL-KEYS,                 ELELCAML
00911                           CA-CODE-VALUE-SCROLL-KEYS               ELELCAML
00912        EXEC CICS                                                  ELELCAML
00913           SEND                                                    ELELCAML
00914             MAP('ELCAI01')                                        ELELCAML
00915             MAPSET('ELCASET')                                     ELELCAML
00916             ERASE                                                 ELELCAML
00917        END-EXEC.                                                  ELELCAML
00918                                                                   ELELCAML
00919      EXEC CICS                                                    ELELCAML
00920           RETURN                                                  ELELCAML
00921             TRANSID('ELCA')                                       ELELCAML
00922             COMMAREA(COMM-AREA)                                   ELELCAML
00923             LENGTH(COMM-LENGTH)                                   ELELCAML
00924      END-EXEC.                                                    ELELCAML
00925  3000-EXIT.    EXIT.                                              ELELCAML
00926 /                                                                 ELELCAML
00927  4000-PROCESS-ADD.                                                ELELCAML
00928      MOVE 'N' TO ADD-ERROR.                                       ELELCAML
00929      MOVE 'A' TO CA-CURRENT-FUNCTION.                             ELELCAML
00930      MOVE PREF-D (SCREEN-CTR) TO RL-RECORD-PREFIX.                ELELCAML
00931 ******* RD = DIRECT READ ************************                 ELELCAML
00932      MOVE 'RD '            TO ELCIO-FILE-ACCESS-CODE.             ELELCAML
00933      MOVE EL-ELPRL-REC-LEN TO ELCIO-MAX-REC-LEN.                  ELELCAML
00934      MOVE RL-RECORD-PREFIX TO ELCIO-VSAM-KEY.                     ELELCAML
00935      MOVE EL-DSN-ELPRL     TO ELCIO-FILE-DDNAME,                  ELELCAML
00936                               CIA-IO-GETMAIN-DDNAME.              ELELCAML
00937      MOVE 'M'              TO ELCIO-STORAGE.                      ELELCAML
00938                                                                   ELELCAML
00939      SET  CIA-IO-PARM-AREA-PNTR        TO                         ELELCAML
00940           CIA-ELPRL-IOPARM-AREA-PNTR.                             ELELCAML
00941      SET  ELCIO-REC-AREA-ADDRESS       TO                         ELELCAML
00942           CIA-ELPRL-REC-AREA-PNTR.                                ELELCAML
00943      EXEC CICS LINK                                               ELELCAML
00944           PROGRAM('ELAIOPGM')                                     ELELCAML
00945           COMMAREA(ADDRESS OF  CIA-PARMS-RECORD)                  ELELCAML
00946           LENGTH(EL-CIA-POINTER-LEN)                              ELELCAML
00947      END-EXEC.                                                    ELELCAML
00948                                                                   ELELCAML
00949      IF ELCIO-REC-NOT-FOUND                                       ELELCAML
00950           PERFORM 4010-ADD-OK                                     ELELCAML
00951              THRU 4010-EXIT                                       ELELCAML
00952           GO TO 4000-EXIT.                                        ELELCAML
00953                                                                   ELELCAML
00954      MOVE -1              TO PREF-L (SCREEN-CTR).                 ELELCAML
00955      MOVE DFHBMUBF TO PREF-A (SCREEN-CTR).                        ELELCAML
00956      IF RL-DELETE                                                 ELELCAML
00957          MOVE NEW-PREFIX-ON-FILE TO ERRM-D                        ELELCAML
00958      ELSE                                                         ELELCAML
00959       MOVE RECORD-ON-FILE TO ERRM-D.                              ELELCAML
00960      MOVE DFHBMASB TO ERRM-A.                                     ELELCAML
00961      MOVE 'Y' TO ADD-ERROR.                                       ELELCAML
00962  4000-EXIT.     EXIT.                                             ELELCAML
00963 /                                                                 ELELCAML
00964  4010-ADD-OK.                                                     ELELCAML
00965      MOVE FILE-D (SCREEN-CTR) TO FILE-CHECK.                      ELELCAML
00966      IF VALID-FILE                                                ELELCAML
00967         PERFORM 4050-WRITE-TO-FILE                                ELELCAML
00968            THRU 4050-EXIT                                         ELELCAML
00969         GO TO 4010-EXIT.                                          ELELCAML
00970                                                                   ELELCAML
00971      MOVE INVALID-FILE-CODE TO ERRM-D.                            ELELCAML
00972      MOVE -1                TO FILE-L (SCREEN-CTR).               ELELCAML
00973      MOVE DFHBMUBF          TO FILE-A (SCREEN-CTR).               ELELCAML
00974      MOVE DFHBMASB          TO ERRM-A.                            ELELCAML
00975      MOVE 'Y' TO ADD-ERROR.                                       ELELCAML
00976  4010-EXIT.     EXIT.                                             ELELCAML
00977 /                                                                 ELELCAML
00978  4050-WRITE-TO-FILE.                                              ELELCAML
00979      MOVE PREF-D (SCREEN-CTR) TO RL-RECORD-PREFIX.                ELELCAML
00980      MOVE FILE-D (SCREEN-CTR) TO RL-FILE.                         ELELCAML
00981      MOVE NAME-D (SCREEN-CTR) TO RL-RECORD-NAME.                  ELELCAML
00982      MOVE 'Y'                 TO RL-AUTO-REPRINT-FLAG.            ELELCAML
00983      MOVE SPACE               TO RL-DELETE-RECORD-FLAG.           ELELCAML
00984                                                                   ELELCAML
00985 ******* WR = (WRITE ADD A NEW RECORD)************                 ELELCAML
00986      MOVE 'WR '            TO ELCIO-FILE-ACCESS-CODE.             ELELCAML
00987      MOVE RL-RECORD-PREFIX TO ELCIO-VSAM-KEY.                     ELELCAML
00988      MOVE EL-ELPRL-REC-LEN TO ELCIO-RECORD-LEN.                   ELELCAML
00989                                                                   ELELCAML
00990      SET  CIA-IO-PARM-AREA-PNTR        TO                         ELELCAML
00991           CIA-ELPRL-IOPARM-AREA-PNTR.                             ELELCAML
00992      SET  ELCIO-REC-AREA-ADDRESS       TO                         ELELCAML
00993           CIA-ELPRL-REC-AREA-PNTR.                                ELELCAML
00994      EXEC CICS LINK                                               ELELCAML
00995           PROGRAM('ELAIOPGM')                                     ELELCAML
00996           COMMAREA(ADDRESS OF  CIA-PARMS-RECORD)                  ELELCAML
00997           LENGTH(EL-CIA-POINTER-LEN)                              ELELCAML
00998      END-EXEC.                                                    ELELCAML
00999                                                                   ELELCAML
01000      MOVE RL-RECORD-PREFIX    TO CA-SEL-RECORD-PREFIX.            ELELCAML
01001      MOVE RL-RECORD-NAME      TO CA-SEL-RECORD-NAME.              ELELCAML
01002      MOVE LOW-VALUES          TO CA-SEL-ELEMENT-NBR-X.            ELELCAML
01003      MOVE SPACES              TO CA-SEL-ELEMENT-NAME,             ELELCAML
01004                                  CA-SEL-CODE-VALUE,               ELELCAML
01005                                  CA-SEL-CODE-NAME.                ELELCAML
01006  4050-EXIT.    EXIT.                                              ELELCAML
01007 /                                                                 ELELCAML
01008  4100-PROCESS-CHANGE.                                             ELELCAML
01009      MOVE 'N' TO ERROR-SW,                                        ELELCAML
01010                  REWRITE-SW.                                      ELELCAML
01011      MOVE PREF-D (SCREEN-CTR) TO RL-RECORD-PREFIX.                ELELCAML
01012                                                                   ELELCAML
01013 ******* RU = DIRECT READ FOR UPDATE**************                 ELELCAML
01014      MOVE 'RU '            TO ELCIO-FILE-ACCESS-CODE.             ELELCAML
01015      MOVE RL-RECORD-PREFIX TO ELCIO-VSAM-KEY.                     ELELCAML
01016      MOVE EL-ELPRL-REC-LEN TO ELCIO-MAX-REC-LEN.                  ELELCAML
01017      MOVE EL-DSN-ELPRL     TO ELCIO-FILE-DDNAME,                  ELELCAML
01018                               CIA-IO-GETMAIN-DDNAME.              ELELCAML
01019      MOVE 'M'              TO ELCIO-STORAGE.                      ELELCAML
01020                                                                   ELELCAML
01021      SET  CIA-IO-PARM-AREA-PNTR        TO                         ELELCAML
01022           CIA-ELPRL-IOPARM-AREA-PNTR.                             ELELCAML
01023      SET  ELCIO-REC-AREA-ADDRESS       TO                         ELELCAML
01024           CIA-ELPRL-REC-AREA-PNTR.                                ELELCAML
01025      EXEC CICS LINK                                               ELELCAML
01026           PROGRAM('ELAIOPGM')                                     ELELCAML
01027           COMMAREA(ADDRESS OF  CIA-PARMS-RECORD)                  ELELCAML
01028           LENGTH(EL-CIA-POINTER-LEN)                              ELELCAML
01029      END-EXEC.                                                    ELELCAML
01030                                                                   ELELCAML
01031      IF ELCIO-REC-NOT-FOUND                                       ELELCAML
01032         MOVE RECORD-NOTFND   TO ERRM-D                            ELELCAML
01033         MOVE DFHBMASB TO ERRM-A                                   ELELCAML
01034         MOVE 'Y' TO ERROR-SW                                      ELELCAML
01035         GO TO 4100-EXIT.                                          ELELCAML
01036                                                                   ELELCAML
01037 ******* ULK = UNLOCK RECORD         **************                ELELCAML
01038      IF RL-DELETE                                                 ELELCAML
01039         MOVE 'ULK'            TO ELCIO-FILE-ACCESS-CODE           ELELCAML
01040          EXEC CICS LINK                                           ELELCAML
01041             PROGRAM('ELAIOPGM')                                   ELELCAML
01042             COMMAREA(ADDRESS OF  CIA-PARMS-RECORD)                ELELCAML
01043             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01044         END-EXEC                                                  ELELCAML
01045         MOVE REC-SCH-FOR-DEL-NCHG TO ERRM-D                       ELELCAML
01046         MOVE DFHBMASB             TO ERRM-A                       ELELCAML
01047         MOVE 'Y' TO ERROR-SW                                      ELELCAML
01048         MOVE -1  TO FCN-L (SCREEN-CTR)                            ELELCAML
01049         MOVE DFHBMBRY TO FCN-A (SCREEN-CTR)                       ELELCAML
01050         GO TO 4100-EXIT.                                          ELELCAML
01051                                                                   ELELCAML
01052      IF FILE-L (SCREEN-CTR) > +0                                  ELELCAML
01053         IF FILE-D (SCREEN-CTR) NOT = RL-FILE                      ELELCAML
01054            MOVE FILE-D (SCREEN-CTR) TO FILE-CHECK,                ELELCAML
01055                                        RL-FILE                    ELELCAML
01056            MOVE 'Y' TO RL-AUTO-REPRINT-FLAG                       ELELCAML
01057            IF VALID-FILE                                          ELELCAML
01058                MOVE 'Y' TO REWRITE-SW                             ELELCAML
01059            ELSE                                                   ELELCAML
01060                MOVE INVALID-FILE-CODE TO ERRM-D                   ELELCAML
01061                MOVE DFHBMASB TO ERRM-A                            ELELCAML
01062                MOVE 'Y' TO ERROR-SW                               ELELCAML
01063                MOVE -1  TO FILE-L (SCREEN-CTR)                    ELELCAML
01064                MOVE DFHBMUBF TO FILE-A (SCREEN-CTR)               ELELCAML
01065                GO TO 4100-EXIT.                                   ELELCAML
01066                                                                   ELELCAML
01067      IF NAME-L (SCREEN-CTR) > +0                                  ELELCAML
01068        IF NAME-D (SCREEN-CTR) NOT = RL-RECORD-NAME                ELELCAML
01069          MOVE NAME-D (SCREEN-CTR) TO RL-RECORD-NAME               ELELCAML
01070          MOVE 'Y'                 TO RL-AUTO-REPRINT-FLAG,        ELELCAML
01071                                      REWRITE-SW.                  ELELCAML
01072                                                                   ELELCAML
01073 ******* WU  = REWRITE RECORD        **************                ELELCAML
01074      IF REWRITE-SW = 'Y'                                          ELELCAML
01075         MOVE 'WU '            TO ELCIO-FILE-ACCESS-CODE           ELELCAML
01076         EXEC CICS LINK                                            ELELCAML
01077              PROGRAM('ELAIOPGM')                                  ELELCAML
01078              COMMAREA(ADDRESS OF  CIA-PARMS-RECORD)               ELELCAML
01079              LENGTH(EL-CIA-POINTER-LEN)                           ELELCAML
01080         END-EXEC                                                  ELELCAML
01081      ELSE                                                         ELELCAML
01082 ******* ULK = UNLOCK RECORD         **************                ELELCAML
01083         MOVE 'ULK'            TO ELCIO-FILE-ACCESS-CODE           ELELCAML
01084          EXEC CICS LINK                                           ELELCAML
01085             PROGRAM('ELAIOPGM')                                   ELELCAML
01086             COMMAREA(ADDRESS OF  CIA-PARMS-RECORD)                ELELCAML
01087             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01088         END-EXEC                                                  ELELCAML
01089         MOVE NO-CHANGES-MADE       TO ERRM-D                      ELELCAML
01090         MOVE DFHBMASB              TO ERRM-A                      ELELCAML
01091         MOVE -1                    TO FCN-L (SCREEN-CTR)          ELELCAML
01092         MOVE DFHBMUBF              TO FCN-A (SCREEN-CTR)          ELELCAML
01093         MOVE 'Y'                   TO ERROR-SW.                   ELELCAML
01094                                                                   ELELCAML
01095      IF ERROR-SW = 'N'                                            ELELCAML
01096         MOVE SPACE TO FCN-D (SCREEN-CTR).                         ELELCAML
01097                                                                   ELELCAML
01098      MOVE +0  TO FILE-L (SCREEN-CTR),                             ELELCAML
01099                  NAME-L (SCREEN-CTR).                             ELELCAML
01100                                                                   ELELCAML
01101  4100-EXIT.    EXIT.                                              ELELCAML
01102 /                                                                 ELELCAML
01103  4200-PROCESS-DEL-UNDEL.                                          ELELCAML
01104      MOVE 'N' TO ERROR-SW.                                        ELELCAML
01105      MOVE PREF-D (SCREEN-CTR) TO RL-RECORD-PREFIX.                ELELCAML
01106 ******* RU = DIRECT READ FOR UPDATE**************                 ELELCAML
01107      MOVE 'RU '            TO ELCIO-FILE-ACCESS-CODE.             ELELCAML
01108      MOVE EL-ELPRL-REC-LEN TO ELCIO-MAX-REC-LEN.                  ELELCAML
01109      MOVE RL-RECORD-PREFIX TO ELCIO-VSAM-KEY.                     ELELCAML
01110      MOVE EL-DSN-ELPRL     TO ELCIO-FILE-DDNAME,                  ELELCAML
01111                               CIA-IO-GETMAIN-DDNAME.              ELELCAML
01112      MOVE 'M'              TO ELCIO-STORAGE.                      ELELCAML
01113                                                                   ELELCAML
01114      SET  CIA-IO-PARM-AREA-PNTR        TO                         ELELCAML
01115           CIA-ELPRL-IOPARM-AREA-PNTR.                             ELELCAML
01116      SET  ELCIO-REC-AREA-ADDRESS       TO                         ELELCAML
01117           CIA-ELPRL-REC-AREA-PNTR.                                ELELCAML
01118      EXEC CICS LINK                                               ELELCAML
01119           PROGRAM('ELAIOPGM')                                     ELELCAML
01120           COMMAREA(ADDRESS OF  CIA-PARMS-RECORD)                  ELELCAML
01121           LENGTH(EL-CIA-POINTER-LEN)                              ELELCAML
01122      END-EXEC.                                                    ELELCAML
01123                                                                   ELELCAML
01124      IF ELCIO-GOOD-RETURN                                         ELELCAML
01125         NEXT SENTENCE                                             ELELCAML
01126      ELSE                                                         ELELCAML
01127         MOVE -1              TO  FILE-L (SCREEN-CTR)              ELELCAML
01128         MOVE DFHBMUBF TO         FILE-A (SCREEN-CTR)              ELELCAML
01129         MOVE RECORD-NOTFND   TO  ERRM-D                           ELELCAML
01130         MOVE DFHBMASB TO         ERRM-A                           ELELCAML
01131         MOVE 'Y'             TO  ERROR-SW                         ELELCAML
01132         GO TO  4200-EXIT.                                         ELELCAML
01133                                                                   ELELCAML
01134      IF (FCN-D (SCREEN-CTR) = 'U' AND NOT RL-DELETE)              ELELCAML
01135         OR                                                        ELELCAML
01136         (FCN-D (SCREEN-CTR) = 'D' AND RL-DELETE)                  ELELCAML
01137 ******* ULK = UNLOCK RECORD         **************                ELELCAML
01138         MOVE 'ULK'            TO ELCIO-FILE-ACCESS-CODE           ELELCAML
01139         EXEC CICS LINK                                            ELELCAML
01140              PROGRAM('ELAIOPGM')                                  ELELCAML
01141             COMMAREA(ADDRESS OF  CIA-PARMS-RECORD)                ELELCAML
01142             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01143         END-EXEC                                                  ELELCAML
01144         GO TO 4200-EXIT.                                          ELELCAML
01145                                                                   ELELCAML
01146      IF RL-DELETE AND FCN-D (SCREEN-CTR) = 'U'                    ELELCAML
01147         MOVE SPACE TO RL-DELETE-RECORD-FLAG                       ELELCAML
01148                       RL-AUTO-REPRINT-FLAG.                       ELELCAML
01149                                                                   ELELCAML
01150      IF FCN-D (SCREEN-CTR) = 'D'                                  ELELCAML
01151        MOVE 'D' TO RL-DELETE-RECORD-FLAG                          ELELCAML
01152        MOVE 'Y' TO RL-AUTO-REPRINT-FLAG.                          ELELCAML
01153                                                                   ELELCAML
01154 ******* WU  = REWRITE RECORD        **************                ELELCAML
01155      MOVE 'WU '            TO ELCIO-FILE-ACCESS-CODE.             ELELCAML
01156      EXEC CICS LINK                                               ELELCAML
01157           PROGRAM('ELAIOPGM')                                     ELELCAML
01158           COMMAREA(ADDRESS OF  CIA-PARMS-RECORD)                  ELELCAML
01159           LENGTH(EL-CIA-POINTER-LEN)                              ELELCAML
01160      END-EXEC.                                                    ELELCAML
01161      MOVE SPACE TO FCN-D (SCREEN-CTR).                            ELELCAML
01162                                                                   ELELCAML
01163      IF RL-DELETE                                                 ELELCAML
01164         MOVE DELETED TO STATUS-D (SCREEN-CTR)                     ELELCAML
01165      ELSE                                                         ELELCAML
01166        MOVE SPACE TO STATUS-D (SCREEN-CTR).                       ELELCAML
01167                                                                   ELELCAML
01168  4200-EXIT.    EXIT.                                              ELELCAML
01169 /                                                                 ELELCAML
01170  4300-PROCESS-MAP-FUNCTION.                                       ELELCAML
01171      IF STATUS-L (SCREEN-CTR) NOT > +0                            ELELCAML
01172         MOVE NO-NEW-PREFIX-ENTERED TO ERRM-D                      ELELCAML
01173         MOVE DFHBMASB              TO ERRM-A                      ELELCAML
01174         MOVE 'Y'                   TO ERROR-SW                    ELELCAML
01175         MOVE -1                    TO STATUS-L (SCREEN-CTR)       ELELCAML
01176         MOVE DFHBMUBF              TO STATUS-A (SCREEN-CTR)       ELELCAML
01177         GO TO 4300-EXIT.                                          ELELCAML
01178                                                                   ELELCAML
01179      IF STATUS-D (SCREEN-CTR) NOT = CA-MF-RECORD-PREFIX           ELELCAML
01180         MOVE SCREEN-CTR TO MAP-POSITION                           ELELCAML
01181         PERFORM 2100-MAP-FUNCTION-INITAL     THRU                 ELELCAML
01182                 2100-EXIT                                         ELELCAML
01183         MOVE DFHBMASB         TO ERRM-A                           ELELCAML
01184         PERFORM 1800-SET-ATTRIBUTES  THRU 1800-EXIT               ELELCAML
01185         PERFORM 3000-SEND-SCREEN     THRU 3000-EXIT.              ELELCAML
01186                                                                   ELELCAML
01187      MOVE LOW-VALUES          TO SAVE-RL-RECORD-NAME,             ELELCAML
01188                                  SAVE-RL-FILE.                    ELELCAML
01189      MOVE PREF-D (SCREEN-CTR) TO RL-RECORD-PREFIX.                ELELCAML
01190                                                                   ELELCAML
01191      MOVE 'RD '               TO ELCIO-FILE-ACCESS-CODE.          ELELCAML
01192      MOVE EL-ELPRL-REC-LEN    TO ELCIO-MAX-REC-LEN.               ELELCAML
01193      MOVE RL-RECORD-PREFIX    TO ELCIO-VSAM-KEY.                  ELELCAML
01194      MOVE EL-DSN-ELPRL        TO ELCIO-FILE-DDNAME,               ELELCAML
01195                                  CIA-IO-GETMAIN-DDNAME.           ELELCAML
01196      MOVE 'M'                 TO ELCIO-STORAGE.                   ELELCAML
01197                                                                   ELELCAML
01198      SET  CIA-IO-PARM-AREA-PNTR        TO                         ELELCAML
01199           CIA-ELPRL-IOPARM-AREA-PNTR.                             ELELCAML
01200      SET  ELCIO-REC-AREA-ADDRESS       TO                         ELELCAML
01201           CIA-ELPRL-REC-AREA-PNTR.                                ELELCAML
01202      EXEC CICS LINK                                               ELELCAML
01203           PROGRAM('ELAIOPGM')                                     ELELCAML
01204           COMMAREA(ADDRESS OF  CIA-PARMS-RECORD)                  ELELCAML
01205           LENGTH(EL-CIA-POINTER-LEN)                              ELELCAML
01206      END-EXEC.                                                    ELELCAML
01207                                                                   ELELCAML
01208      IF ELCIO-REC-NOT-FOUND                                       ELELCAML
01209           PERFORM 4310-NOTFND-OK  THRU                            ELELCAML
01210                   4310-EXIT                                       ELELCAML
01211           GO TO 4300-EXIT.                                        ELELCAML
01212                                                                   ELELCAML
01213      IF CA-SUPERVISORY                                            ELELCAML
01214        AND PF6-SW = 'Y'                                           ELELCAML
01215          MOVE RL-RECORD-NAME TO SAVE-RL-RECORD-NAME,              ELELCAML
01216          MOVE RL-FILE        TO SAVE-RL-FILE,                     ELELCAML
01217          PERFORM 4800-DELETE-REC-STRUCT                           ELELCAML
01218             THRU 4800-EXIT                                        ELELCAML
01219          PERFORM 4310-NOTFND-OK                                   ELELCAML
01220             THRU 4310-EXIT                                        ELELCAML
01221      ELSE                                                         ELELCAML
01222          MOVE -1               TO FCN-L (SCREEN-CTR)              ELELCAML
01223          MOVE DFHBMUBF         TO FCN-A (SCREEN-CTR)              ELELCAML
01224          MOVE UNAUTHOR-OVERLAY TO ERRM-D                          ELELCAML
01225          MOVE DFHBMASB         TO ERRM-A                          ELELCAML
01226          MOVE 'Y' TO ERROR-SW.                                    ELELCAML
01227                                                                   ELELCAML
01228  4300-EXIT.     EXIT.                                             ELELCAML
01229 /                                                                 ELELCAML
01230  4310-NOTFND-OK.                                                  ELELCAML
01231      MOVE STATUS-D (SCREEN-CTR) TO RL-RECORD-PREFIX.              ELELCAML
01232      MOVE 'RD '            TO ELCIO-FILE-ACCESS-CODE.             ELELCAML
01233      MOVE EL-ELPRL-REC-LEN TO ELCIO-MAX-REC-LEN.                  ELELCAML
01234      MOVE RL-RECORD-PREFIX TO ELCIO-VSAM-KEY.                     ELELCAML
01235      MOVE EL-DSN-ELPRL     TO ELCIO-FILE-DDNAME,                  ELELCAML
01236                               CIA-IO-GETMAIN-DDNAME.              ELELCAML
01237      MOVE 'M'              TO ELCIO-STORAGE.                      ELELCAML
01238                                                                   ELELCAML
01239      SET  CIA-IO-PARM-AREA-PNTR   TO  CIA-ELPRL-IOPARM-AREA-PNTR. ELELCAML
01240      SET  ELCIO-REC-AREA-ADDRESS  TO  CIA-ELPRL-REC-AREA-PNTR.    ELELCAML
01241      EXEC CICS LINK                                               ELELCAML
01242           PROGRAM('ELAIOPGM')                                     ELELCAML
01243           COMMAREA(ADDRESS OF  CIA-PARMS-RECORD)                  ELELCAML
01244           LENGTH(EL-CIA-POINTER-LEN)                              ELELCAML
01245      END-EXEC.                                                    ELELCAML
01246                                                                   ELELCAML
01247      IF ELCIO-REC-NOT-FOUND                                       ELELCAML
01248         MOVE -1                 TO   PREF-L (SCREEN-CTR)          ELELCAML
01249         MOVE DFHBMUBF           TO   PREF-A (SCREEN-CTR)          ELELCAML
01250         MOVE RECORD-NOTFND      TO   ERRM-D                       ELELCAML
01251         MOVE DFHBMASB           TO   ERRM-A                       ELELCAML
01252         MOVE 'Y'                TO   ERROR-SW                     ELELCAML
01253         GO TO 4310-EXIT.                                          ELELCAML
01254                                                                   ELELCAML
01255      MOVE PREF-D (SCREEN-CTR) TO RL-RECORD-PREFIX.                ELELCAML
01256      IF SCREEN-CTR = MAX-PROCESS-LINE                             ELELCAML
01257          NEXT SENTENCE                                            ELELCAML
01258      ELSE                                                         ELELCAML
01259         IF SAVE-RL-RECORD-NAME NOT = LOW-VALUES AND               ELELCAML
01260            SAVE-RL-FILE NOT = LOW-VALUES                          ELELCAML
01261                MOVE SAVE-RL-RECORD-NAME TO RL-RECORD-NAME         ELELCAML
01262                MOVE SAVE-RL-FILE        TO RL-FILE                ELELCAML
01263                MOVE 'Y'                 TO RL-AUTO-REPRINT-FLAG   ELELCAML
01264                PERFORM 4320-WRITE THRU 4320-EXIT                  ELELCAML
01265                GO TO 4310-EXIT                                    ELELCAML
01266         ELSE                                                      ELELCAML
01267             PERFORM  4320-WRITE THRU 4320-EXIT                    ELELCAML
01268             GO TO 4310-EXIT.                                      ELELCAML
01269                                                                   ELELCAML
01270      IF NAME-L (SCREEN-CTR) > +0                                  ELELCAML
01271          MOVE NAME-D (SCREEN-CTR) TO RL-RECORD-NAME.              ELELCAML
01272      IF FILE-L (SCREEN-CTR) > +0                                  ELELCAML
01273            MOVE FILE-D (SCREEN-CTR) TO FILE-CHECK,                ELELCAML
01274                                        RL-FILE                    ELELCAML
01275            MOVE 'Y' TO RL-AUTO-REPRINT-FLAG                       ELELCAML
01276            IF VALID-FILE                                          ELELCAML
01277                NEXT SENTENCE                                      ELELCAML
01278            ELSE                                                   ELELCAML
01279             MOVE -1              TO FILE-L (SCREEN-CTR)           ELELCAML
01280             MOVE DFHBMUBF TO FILE-A (SCREEN-CTR)                  ELELCAML
01281             MOVE INVALID-FILE-CODE TO ERRM-D                      ELELCAML
01282             MOVE DFHBMASB TO ERRM-A                               ELELCAML
01283             MOVE 'Y' TO ERROR-SW.                                 ELELCAML
01284                                                                   ELELCAML
01285  4310-EXIT.     EXIT.                                             ELELCAML
01286 /                                                                 ELELCAML
01287  4320-WRITE.                                                      ELELCAML
01288      MOVE RL-RECORD-PREFIX TO ELCIO-VSAM-KEY.                     ELELCAML
01289      MOVE 'WR '                 TO ELCIO-FILE-ACCESS-CODE.        ELELCAML
01290      EXEC CICS LINK                                               ELELCAML
01291           PROGRAM('ELAIOPGM')                                     ELELCAML
01292           COMMAREA(ADDRESS OF  CIA-PARMS-RECORD)                  ELELCAML
01293           LENGTH(EL-CIA-POINTER-LEN)                              ELELCAML
01294      END-EXEC.                                                    ELELCAML
01295                                                                   ELELCAML
01296      PERFORM 4500-DATA-ELEMENT                                    ELELCAML
01297         THRU 4500-EXIT.                                           ELELCAML
01298      PERFORM 4600-CODE-VALUE                                      ELELCAML
01299         THRU 4600-EXIT.                                           ELELCAML
01300      MOVE SPACE TO FCN-D (SCREEN-CTR).                            ELELCAML
01301  4320-EXIT.    EXIT.                                              ELELCAML
01302 /                                                                 ELELCAML
01303  4400-PROCESS-SELECT.                                             ELELCAML
01304      IF SPREF-L > +0                                              ELELCAML
01305          MOVE SPREF-D TO RL-RECORD-PREFIX                         ELELCAML
01306          MOVE -1      TO SPREF-L                                  ELELCAML
01307          MOVE DFHBMUBF TO SPREF-A                                 ELELCAML
01308      ELSE                                                         ELELCAML
01309       MOVE PREF-D (SELECT-POSITION) TO RL-RECORD-PREFIX           ELELCAML
01310       MOVE -1                       TO FCN-L (SELECT-POSITION).   ELELCAML
01311                                                                   ELELCAML
01312      MOVE 'RD '                  TO  ELCIO-FILE-ACCESS-CODE.      ELELCAML
01313      MOVE EL-ELPRL-REC-LEN       TO  ELCIO-MAX-REC-LEN.           ELELCAML
01314      MOVE RL-RECORD-PREFIX       TO  ELCIO-VSAM-KEY.              ELELCAML
01315      MOVE EL-DSN-ELPRL           TO  ELCIO-FILE-DDNAME,           ELELCAML
01316                                      CIA-IO-GETMAIN-DDNAME.       ELELCAML
01317      MOVE 'M'                    TO  ELCIO-STORAGE.               ELELCAML
01318      SET  CIA-IO-PARM-AREA-PNTR  TO  CIA-ELPRL-IOPARM-AREA-PNTR.  ELELCAML
01319      SET  ELCIO-REC-AREA-ADDRESS TO  CIA-ELPRL-REC-AREA-PNTR.     ELELCAML
01320      EXEC CICS  LINK                                              ELELCAML
01321           PROGRAM('ELAIOPGM')                                     ELELCAML
01322           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCAML
01323           LENGTH(EL-CIA-POINTER-LEN)                              ELELCAML
01324      END-EXEC.                                                    ELELCAML
01325                                                                   ELELCAML
01326      IF ELCIO-REC-NOT-FOUND                                       ELELCAML
01327         MOVE RECORD-NOTFND   TO ERRM-D                            ELELCAML
01328         MOVE DFHBMASB TO ERRM-A                                   ELELCAML
01329         PERFORM 3000-SEND-SCREEN  THRU 3000-EXIT                  ELELCAML
01330         GO TO  4400-EXIT.                                         ELELCAML
01331                                                                   ELELCAML
01332      IF RL-DELETE                                                 ELELCAML
01333           MOVE DELETE-CANNOT-SELECT TO ERRM-D                     ELELCAML
01334           PERFORM 1800-SET-ATTRIBUTES   THRU 1800-EXIT            ELELCAML
01335           PERFORM 3000-SEND-SCREEN      THRU 3000-EXIT.           ELELCAML
01336                                                                   ELELCAML
01337      MOVE LOW-VALUES            TO CA-SELECTED-KEYS.              ELELCAML
01338      MOVE 'S'                   TO CA-CURRENT-FUNCTION.           ELELCAML
01339      MOVE RL-RECORD-PREFIX      TO CA-SEL-RECORD-PREFIX.          ELELCAML
01340      MOVE SPACES                TO CA-SELECTED-NAMES.             ELELCAML
01341      MOVE RL-RECORD-NAME        TO CA-SEL-RECORD-NAME.            ELELCAML
01342      MOVE CA-SEL-RECORD-PREFIX  TO DE-RECORD-PREFIX.              ELELCAML
01343      MOVE ZEROS                 TO DE-ELEMENT-NBR.                ELELCAML
01344      MOVE 'A'                   TO CA-CURRENT-PGM.                ELELCAML
01345 **** GB  = GENERIC BROWSE                ********                 ELELCAML
01346      MOVE 'GB '                 TO ELCIO-FILE-ACCESS-CODE2.       ELELCAML
01347      MOVE 'EQ '                 TO ELCIO-CIO-QUAL2.               ELELCAML
01348      MOVE ELPRL-KEY-LENGTH      TO ELCIO-BROWSE-KEYLEN2.          ELELCAML
01349      MOVE EL-ELPDE-REC-LEN      TO ELCIO-MAX-REC-LEN2.            ELELCAML
01350      MOVE DE-PRIMARY-KEY        TO ELCIO-VSAM-KEY2.               ELELCAML
01351      MOVE EL-DSN-ELPDE          TO ELCIO-FILE-DDNAME2,            ELELCAML
01352                                    CIA-IO-GETMAIN-DDNAME.         ELELCAML
01353      MOVE 'M'                   TO ELCIO-STORAGE2.                ELELCAML
01354      SET  CIA-IO-PARM-AREA-PNTR TO  CIA-ELPDE-IOPARM-AREA-PNTR.   ELELCAML
01355      SET  ELCIO-REC-AREA-ADDRESS2  TO   CIA-ELPDE-REC-AREA-PNTR.  ELELCAML
01356      EXEC CICS  LINK                                              ELELCAML
01357             PROGRAM('ELAIOPGM')                                   ELELCAML
01358             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01359             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01360      END-EXEC.                                                    ELELCAML
01361                                                                   ELELCAML
01362      IF  NOT ELCIO-GOOD-RETURN2                                   ELELCAML
01363          MOVE 'A'              TO CA-CURRENT-FUNCTION             ELELCAML
01364          EXEC CICS                                                ELELCAML
01365               XCTL PROGRAM('ELELCCML')                            ELELCAML
01366               COMMAREA(COMM-AREA)                                 ELELCAML
01367               LENGTH(COMM-LENGTH)                                 ELELCAML
01368          END-EXEC.                                                ELELCAML
01369                                                                   ELELCAML
01370                                                                   ELELCAML
01371      MOVE 'EB '             TO ELCIO-FILE-ACCESS-CODE2.           ELELCAML
01372 *-->   LINK TO ASSEMBLER I/O PGM FOR END BROWSE    ------         ELELCAML
01373                                                                   ELELCAML
01374        EXEC CICS  LINK                                            ELELCAML
01375             PROGRAM('ELAIOPGM')                                   ELELCAML
01376             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01377             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01378        END-EXEC.                                                  ELELCAML
01379                                                                   ELELCAML
01380 **********************************************                    ELELCAML
01381 **** CHECK TO SEE IF THE PROGRAM HAS RETRIEVED A DATA ELEMENT     ELELCAML
01382 ****  OF A DIFFERENT RECORD                                       ELELCAML
01383 **********************************************                    ELELCAML
01384      IF DE-RECORD-PREFIX NOT = CA-SEL-RECORD-PREFIX               ELELCAML
01385         MOVE 'A'              TO CA-CURRENT-FUNCTION              ELELCAML
01386         EXEC CICS                                                 ELELCAML
01387               XCTL PROGRAM('ELELCCML')                            ELELCAML
01388               COMMAREA(COMM-AREA)                                 ELELCAML
01389               LENGTH(COMM-LENGTH)                                 ELELCAML
01390         END-EXEC                                                  ELELCAML
01391      ELSE                                                         ELELCAML
01392         EXEC CICS                                                 ELELCAML
01393              XCTL PROGRAM('ELELCBML')                             ELELCAML
01394              COMMAREA(COMM-AREA)                                  ELELCAML
01395              LENGTH(COMM-LENGTH)                                  ELELCAML
01396      END-EXEC.                                                    ELELCAML
01397                                                                   ELELCAML
01398  4400-EXIT.    EXIT.                                              ELELCAML
01399 /                                                                 ELELCAML
01400  4500-DATA-ELEMENT.                                               ELELCAML
01401      MOVE STATUS-D (SCREEN-CTR)   TO DE-RECORD-PREFIX.            ELELCAML
01402      MOVE ZEROS                   TO DE-ELEMENT-NBR.              ELELCAML
01403      MOVE 'GB '                   TO ELCIO-FILE-ACCESS-CODE2.     ELELCAML
01404      MOVE 'EQ '                   TO ELCIO-CIO-QUAL2.             ELELCAML
01405      MOVE ELPRL-KEY-LENGTH        TO ELCIO-BROWSE-KEYLEN2.        ELELCAML
01406      MOVE EL-ELPDE-REC-LEN        TO ELCIO-MAX-REC-LEN2.          ELELCAML
01407      MOVE DE-PRIMARY-KEY          TO ELCIO-VSAM-KEY2.             ELELCAML
01408      MOVE EL-DSN-ELPDE            TO ELCIO-FILE-DDNAME2,          ELELCAML
01409                                      CIA-IO-GETMAIN-DDNAME.       ELELCAML
01410      MOVE 'M'                     TO ELCIO-STORAGE2.              ELELCAML
01411      SET  CIA-IO-PARM-AREA-PNTR   TO CIA-ELPDE-IOPARM-AREA-PNTR.  ELELCAML
01412      SET  ELCIO-REC-AREA-ADDRESS2 TO CIA-ELPDE-REC-AREA-PNTR.     ELELCAML
01413      EXEC CICS  LINK                                              ELELCAML
01414           PROGRAM('ELAIOPGM')                                     ELELCAML
01415           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCAML
01416           LENGTH(EL-CIA-POINTER-LEN)                              ELELCAML
01417      END-EXEC.                                                    ELELCAML
01418                                                                   ELELCAML
01419      IF ELCIO-GOOD-RETURN2                                        ELELCAML
01420         NEXT SENTENCE                                             ELELCAML
01421      ELSE                                                         ELELCAML
01422          GO TO 4500-EXIT.                                         ELELCAML
01423                                                                   ELELCAML
01424 /                                                                 ELELCAML
01425  4500-READNEXT-CHECK.                                             ELELCAML
01426      IF STATUS-D (SCREEN-CTR) EQUAL DE-RECORD-PREFIX              ELELCAML
01427         NEXT SENTENCE                                             ELELCAML
01428      ELSE                                                         ELELCAML
01429           GO TO 4500-EXIT.                                        ELELCAML
01430                                                                   ELELCAML
01431      MOVE DE-ELEMENT-NBR   TO SAVE-DE-NBR.                        ELELCAML
01432      MOVE 'EB '            TO ELCIO-FILE-ACCESS-CODE2.            ELELCAML
01433      EXEC CICS  LINK                                              ELELCAML
01434             PROGRAM('ELAIOPGM')                                   ELELCAML
01435             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01436             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01437      END-EXEC.                                                    ELELCAML
01438      MOVE PREF-D (SCREEN-CTR) TO DE-RECORD-PREFIX,                ELELCAML
01439                                  DE-RECORD-PREFIX-N.              ELELCAML
01440      MOVE DE-PRIMARY-KEY      TO ELCIO-VSAM-KEY2.                 ELELCAML
01441      MOVE 'WR '               TO ELCIO-FILE-ACCESS-CODE2.         ELELCAML
01442      EXEC CICS  LINK                                              ELELCAML
01443             PROGRAM('ELAIOPGM')                                   ELELCAML
01444             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01445             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01446      END-EXEC.                                                    ELELCAML
01447                                                                   ELELCAML
01448      MOVE STATUS-D (SCREEN-CTR)   TO DE-RECORD-PREFIX.            ELELCAML
01449      MOVE SAVE-DE-NBR             TO DE-ELEMENT-NBR.              ELELCAML
01450                                                                   ELELCAML
01451      MOVE 'SB '                   TO ELCIO-FILE-ACCESS-CODE2.     ELELCAML
01452      MOVE 'EQ '                   TO ELCIO-CIO-QUAL2.             ELELCAML
01453      MOVE ELPDE-KEY-LENGTH        TO ELCIO-BROWSE-KEYLEN2.        ELELCAML
01454      MOVE EL-ELPDE-REC-LEN        TO ELCIO-MAX-REC-LEN2.          ELELCAML
01455      MOVE DE-PRIMARY-KEY          TO ELCIO-VSAM-KEY2.             ELELCAML
01456      MOVE EL-DSN-ELPDE            TO ELCIO-FILE-DDNAME2           ELELCAML
01457                                      CIA-IO-GETMAIN-DDNAME.       ELELCAML
01458      MOVE 'M'                     TO ELCIO-STORAGE2.              ELELCAML
01459      SET  CIA-IO-PARM-AREA-PNTR   TO    CIA-ELPDE-IOPARM-AREA-PNTRELELCAML
01460      SET  ELCIO-REC-AREA-ADDRESS2 TO    CIA-ELPDE-REC-AREA-PNTR.  ELELCAML
01461      EXEC CICS  LINK                                              ELELCAML
01462             PROGRAM('ELAIOPGM')                                   ELELCAML
01463             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01464             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01465      END-EXEC.                                                    ELELCAML
01466                                                                   ELELCAML
01467      IF ELCIO-GOOD-RETURN2                                        ELELCAML
01468         NEXT SENTENCE                                             ELELCAML
01469      ELSE                                                         ELELCAML
01470          GO TO 4500-EXIT.                                         ELELCAML
01471                                                                   ELELCAML
01472 ******************************************************************ELELCAML
01473 **** SB ACCESS CODE IS CHANGED TO RN BY ELAIOPGM SO THAT JUST     ELELCAML
01474 ****   A CALL TO IT WILL CAUSE A READ NEXT TO OCCUR               ELELCAML
01475 ******************************************************************ELELCAML
01476                                                                   ELELCAML
01477      EXEC CICS  LINK                                              ELELCAML
01478             PROGRAM('ELAIOPGM')                                   ELELCAML
01479             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01480             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01481      END-EXEC.                                                    ELELCAML
01482                                                                   ELELCAML
01483      IF NOT ELCIO-GOOD-RETURN2                                    ELELCAML
01484         NEXT SENTENCE                                             ELELCAML
01485      ELSE                                                         ELELCAML
01486         GO TO 4500-READNEXT-CHECK.                                ELELCAML
01487                                                                   ELELCAML
01488  4500-EXIT.    EXIT.                                              ELELCAML
01489 /                                                                 ELELCAML
01490  4600-CODE-VALUE.                                                 ELELCAML
01491      MOVE STATUS-D (SCREEN-CTR)   TO CV-RECORD-PREFIX.            ELELCAML
01492      MOVE ZEROS                   TO CV-ELEMENT-NBR.              ELELCAML
01493      MOVE LOW-VALUES              TO CV-CODE-VALUE.               ELELCAML
01494      MOVE ZEROS                   TO CV-CODE-DESC-SEQ.            ELELCAML
01495      MOVE 'GB '                   TO ELCIO-FILE-ACCESS-CODE3.     ELELCAML
01496      MOVE 'EQ '                   TO ELCIO-CIO-QUAL3.             ELELCAML
01497      MOVE ELPRL-KEY-LENGTH        TO ELCIO-BROWSE-KEYLEN3.        ELELCAML
01498      MOVE EL-ELPCV-REC-LEN        TO ELCIO-MAX-REC-LEN3.          ELELCAML
01499      MOVE CV-CODE-KEY             TO ELCIO-VSAM-KEY3.             ELELCAML
01500      MOVE EL-DSN-ELPCV            TO ELCIO-FILE-DDNAME3,          ELELCAML
01501                                      CIA-IO-GETMAIN-DDNAME.       ELELCAML
01502      MOVE 'M'                     TO ELCIO-STORAGE3.              ELELCAML
01503      SET  CIA-IO-PARM-AREA-PNTR   TO CIA-ELPCV-IOPARM-AREA-PNTR.  ELELCAML
01504      SET  ELCIO-REC-AREA-ADDRESS3 TO CIA-ELPCV-REC-AREA-PNTR.     ELELCAML
01505      EXEC CICS  LINK                                              ELELCAML
01506             PROGRAM('ELAIOPGM')                                   ELELCAML
01507             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01508             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01509      END-EXEC.                                                    ELELCAML
01510                                                                   ELELCAML
01511      IF ELCIO-GOOD-RETURN3                                        ELELCAML
01512         NEXT SENTENCE                                             ELELCAML
01513      ELSE                                                         ELELCAML
01514          GO TO 4600-EXIT.                                         ELELCAML
01515                                                                   ELELCAML
01516 /                                                                 ELELCAML
01517  4600-READNEXT-CHECK.                                             ELELCAML
01518      IF STATUS-D (SCREEN-CTR) EQUAL CV-RECORD-PREFIX              ELELCAML
01519         NEXT SENTENCE                                             ELELCAML
01520      ELSE                                                         ELELCAML
01521          GO TO 4600-EXIT.                                         ELELCAML
01522                                                                   ELELCAML
01523      MOVE CV-CODE-VALUE         TO SAVE-CODE-VALUE.               ELELCAML
01524      MOVE CV-ELEMENT-NBR        TO SAVE-CV-NBR.                   ELELCAML
01525      MOVE CV-CODE-DESC-SEQ      TO SAVE-CODE-DESC-SEQ.            ELELCAML
01526      MOVE 'EB '                 TO ELCIO-FILE-ACCESS-CODE3.       ELELCAML
01527      EXEC CICS  LINK                                              ELELCAML
01528             PROGRAM('ELAIOPGM')                                   ELELCAML
01529             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01530             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01531      END-EXEC.                                                    ELELCAML
01532                                                                   ELELCAML
01533      IF ELCIO-GOOD-RETURN3                                        ELELCAML
01534         NEXT SENTENCE                                             ELELCAML
01535      ELSE                                                         ELELCAML
01536          GO TO 4600-EXIT.                                         ELELCAML
01537                                                                   ELELCAML
01538      MOVE PREF-D (SCREEN-CTR) TO CV-RECORD-PREFIX.                ELELCAML
01539      MOVE CV-CODE-KEY         TO ELCIO-VSAM-KEY3.                 ELELCAML
01540      MOVE 'WR '               TO ELCIO-FILE-ACCESS-CODE3.         ELELCAML
01541      EXEC CICS  LINK                                              ELELCAML
01542             PROGRAM('ELAIOPGM')                                   ELELCAML
01543             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01544             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01545      END-EXEC.                                                    ELELCAML
01546                                                                   ELELCAML
01547      MOVE STATUS-D (SCREEN-CTR) TO CV-RECORD-PREFIX.              ELELCAML
01548      MOVE SAVE-CV-NBR           TO CV-ELEMENT-NBR.                ELELCAML
01549      MOVE SAVE-CODE-VALUE       TO CV-CODE-VALUE.                 ELELCAML
01550      MOVE SAVE-CODE-DESC-SEQ    TO CV-CODE-DESC-SEQ.              ELELCAML
01551      MOVE 'SB '                 TO ELCIO-FILE-ACCESS-CODE3.       ELELCAML
01552      MOVE 'EQ '                 TO ELCIO-CIO-QUAL3.               ELELCAML
01553      MOVE ELPCV-KEY-LENGTH      TO ELCIO-BROWSE-KEYLEN3.          ELELCAML
01554      MOVE EL-ELPCV-REC-LEN      TO ELCIO-MAX-REC-LEN3.            ELELCAML
01555      MOVE CV-CODE-KEY           TO ELCIO-VSAM-KEY3.               ELELCAML
01556      MOVE EL-DSN-ELPCV          TO ELCIO-FILE-DDNAME3,            ELELCAML
01557                                    CIA-IO-GETMAIN-DDNAME.         ELELCAML
01558      MOVE 'M'                   TO ELCIO-STORAGE3.                ELELCAML
01559      SET  CIA-IO-PARM-AREA-PNTR   TO  CIA-ELPCV-IOPARM-AREA-PNTR. ELELCAML
01560      SET  ELCIO-REC-AREA-ADDRESS3 TO  CIA-ELPCV-REC-AREA-PNTR.    ELELCAML
01561      EXEC CICS  LINK                                              ELELCAML
01562             PROGRAM('ELAIOPGM')                                   ELELCAML
01563             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01564             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01565      END-EXEC.                                                    ELELCAML
01566                                                                   ELELCAML
01567      IF ELCIO-GOOD-RETURN3                                        ELELCAML
01568         NEXT SENTENCE                                             ELELCAML
01569      ELSE                                                         ELELCAML
01570          GO TO 4600-EXIT.                                         ELELCAML
01571                                                                   ELELCAML
01572 ***************************************************************** ELELCAML
01573 **** SB CHANGE ACCESS CODE TO RN THEREFORE ONLY NEED TO CALL      ELELCAML
01574 ****   ELAIOPGM                                                   ELELCAML
01575 ***************************************************************** ELELCAML
01576      EXEC CICS  LINK                                              ELELCAML
01577             PROGRAM('ELAIOPGM')                                   ELELCAML
01578             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01579             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01580      END-EXEC.                                                    ELELCAML
01581                                                                   ELELCAML
01582      IF NOT ELCIO-GOOD-RETURN3                                    ELELCAML
01583         NEXT SENTENCE                                             ELELCAML
01584      ELSE                                                         ELELCAML
01585         GO TO 4600-READNEXT-CHECK.                                ELELCAML
01586                                                                   ELELCAML
01587  4600-EXIT.    EXIT.                                              ELELCAML
01588 /                                                                 ELELCAML
01589  4800-DELETE-REC-STRUCT.                                          ELELCAML
01590 **** DELETE LOWEST LEVEL OF STURCTURE FIRST ****                  ELELCAML
01591                                                                   ELELCAML
01592      PERFORM 4900-DELETE-CODE-VALUES                              ELELCAML
01593         THRU 4900-EXIT.                                           ELELCAML
01594                                                                   ELELCAML
01595 **** DELETE LOWER LEVEL DATA ELEMENTS BEFORE RECORD LIST ****     ELELCAML
01596      MOVE PREF-D (SCREEN-CTR)   TO DE-RECORD-PREFIX.              ELELCAML
01597      MOVE ZEROS                 TO DE-ELEMENT-NBR.                ELELCAML
01598 **** GB = GENERIC BROWSE                 ********                 ELELCAML
01599      MOVE 'GB '                 TO ELCIO-FILE-ACCESS-CODE2.       ELELCAML
01600      MOVE 'EQ '                 TO ELCIO-CIO-QUAL2.               ELELCAML
01601      MOVE ELPRL-KEY-LENGTH      TO ELCIO-BROWSE-KEYLEN2.          ELELCAML
01602      MOVE EL-ELPDE-REC-LEN      TO ELCIO-MAX-REC-LEN2.            ELELCAML
01603      MOVE DE-PRIMARY-KEY        TO ELCIO-VSAM-KEY2.               ELELCAML
01604      MOVE EL-DSN-ELPDE          TO ELCIO-FILE-DDNAME2,            ELELCAML
01605                                    CIA-IO-GETMAIN-DDNAME.         ELELCAML
01606      MOVE 'M'                   TO ELCIO-STORAGE2.                ELELCAML
01607      SET  CIA-IO-PARM-AREA-PNTR   TO  CIA-ELPDE-IOPARM-AREA-PNTR. ELELCAML
01608      SET  ELCIO-REC-AREA-ADDRESS2 TO  CIA-ELPDE-REC-AREA-PNTR.    ELELCAML
01609      EXEC CICS  LINK                                              ELELCAML
01610             PROGRAM('ELAIOPGM')                                   ELELCAML
01611             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01612             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01613      END-EXEC.                                                    ELELCAML
01614                                                                   ELELCAML
01615      IF ELCIO-GOOD-RETURN2                                        ELELCAML
01616         NEXT SENTENCE                                             ELELCAML
01617      ELSE                                                         ELELCAML
01618         GO TO 4800-DELETE-RL.                                     ELELCAML
01619                                                                   ELELCAML
01620 /                                                                 ELELCAML
01621  4800-DE-DELETE.                                                  ELELCAML
01622      MOVE 'EB '            TO ELCIO-FILE-ACCESS-CODE2.            ELELCAML
01623      EXEC CICS  LINK                                              ELELCAML
01624             PROGRAM('ELAIOPGM')                                   ELELCAML
01625             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01626             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01627      END-EXEC.                                                    ELELCAML
01628                                                                   ELELCAML
01629      IF ELCIO-GOOD-RETURN2                                        ELELCAML
01630         NEXT SENTENCE                                             ELELCAML
01631      ELSE                                                         ELELCAML
01632         GO TO 4800-DELETE-RL.                                     ELELCAML
01633                                                                   ELELCAML
01634 **** READ AND DELETE DATA ELEMENT RECORDS ****                    ELELCAML
01635      IF PREF-D (SCREEN-CTR) EQUAL DE-RECORD-PREFIX                ELELCAML
01636         NEXT SENTENCE                                             ELELCAML
01637      ELSE                                                         ELELCAML
01638         GO TO 4800-DELETE-RL.                                     ELELCAML
01639                                                                   ELELCAML
01640 **********************************************                    ELELCAML
01641 **** DL  = DELETE                     ********                    ELELCAML
01642 **********************************************                    ELELCAML
01643      MOVE 'DL '            TO ELCIO-FILE-ACCESS-CODE2.            ELELCAML
01644      MOVE DE-PRIMARY-KEY   TO ELCIO-VSAM-KEY2.                    ELELCAML
01645      EXEC CICS  LINK                                              ELELCAML
01646             PROGRAM('ELAIOPGM')                                   ELELCAML
01647             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01648             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01649      END-EXEC.                                                    ELELCAML
01650      IF ELCIO-REC-NOT-FOUND2                                      ELELCAML
01651           GO TO 4800-DELETE-RL.                                   ELELCAML
01652                                                                   ELELCAML
01653 ***** DO A SB TO GET THE NEXT RECORD **********                   ELELCAML
01654      MOVE 'SB '             TO ELCIO-FILE-ACCESS-CODE2.           ELELCAML
01655      MOVE 'GTE'             TO ELCIO-CIO-QUAL2.                   ELELCAML
01656      EXEC CICS  LINK                                              ELELCAML
01657             PROGRAM('ELAIOPGM')                                   ELELCAML
01658             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01659             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01660      END-EXEC.                                                    ELELCAML
01661                                                                   ELELCAML
01662      IF ELCIO-GOOD-RETURN2                                        ELELCAML
01663         GO TO 4800-DE-DELETE.                                     ELELCAML
01664                                                                   ELELCAML
01665 /                                                                 ELELCAML
01666  4800-DELETE-RL.                                                  ELELCAML
01667      MOVE PREF-D (SCREEN-CTR) TO RL-RECORD-PREFIX.                ELELCAML
01668 **** DL  = DELETE WHEN KEY IS KNOWN      ********                 ELELCAML
01669      MOVE 'DL '                  TO ELCIO-FILE-ACCESS-CODE.       ELELCAML
01670      MOVE EL-ELPRL-REC-LEN       TO ELCIO-MAX-REC-LEN.            ELELCAML
01671      MOVE RL-RECORD-PREFIX       TO ELCIO-VSAM-KEY.               ELELCAML
01672      MOVE EL-DSN-ELPRL           TO ELCIO-FILE-DDNAME,            ELELCAML
01673                                      CIA-IO-GETMAIN-DDNAME.       ELELCAML
01674      MOVE 'M'                    TO  ELCIO-STORAGE.               ELELCAML
01675      SET  CIA-IO-PARM-AREA-PNTR  TO  CIA-ELPRL-IOPARM-AREA-PNTR.  ELELCAML
01676      SET  ELCIO-REC-AREA-ADDRESS TO  CIA-ELPRL-REC-AREA-PNTR.     ELELCAML
01677      EXEC CICS  LINK                                              ELELCAML
01678             PROGRAM('ELAIOPGM')                                   ELELCAML
01679             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01680             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01681      END-EXEC.                                                    ELELCAML
01682                                                                   ELELCAML
01683  4800-EXIT.  EXIT.                                                ELELCAML
01684 /                                                                 ELELCAML
01685  4900-DELETE-CODE-VALUES.                                         ELELCAML
01686      MOVE PREF-D (SCREEN-CTR) TO CV-RECORD-PREFIX.                ELELCAML
01687      MOVE ZEROS                 TO CV-ELEMENT-NBR.                ELELCAML
01688      MOVE LOW-VALUES            TO CV-CODE-VALUE.                 ELELCAML
01689      MOVE ZEROS                 TO CV-CODE-DESC-SEQ.              ELELCAML
01690 **** GB  = GENERIC BROWSE                ********                 ELELCAML
01691      MOVE 'GB '                 TO ELCIO-FILE-ACCESS-CODE3.       ELELCAML
01692      MOVE 'EQ '                 TO ELCIO-CIO-QUAL3.               ELELCAML
01693      MOVE ELPRL-KEY-LENGTH      TO ELCIO-BROWSE-KEYLEN3.          ELELCAML
01694      MOVE EL-ELPCV-REC-LEN      TO ELCIO-MAX-REC-LEN3.            ELELCAML
01695      MOVE CV-CODE-KEY           TO ELCIO-VSAM-KEY3.               ELELCAML
01696      MOVE EL-DSN-ELPCV          TO ELCIO-FILE-DDNAME3,            ELELCAML
01697                                    CIA-IO-GETMAIN-DDNAME.         ELELCAML
01698      MOVE 'M'                   TO ELCIO-STORAGE3.                ELELCAML
01699      SET  CIA-IO-PARM-AREA-PNTR    TO  CIA-ELPCV-IOPARM-AREA-PNTR.ELELCAML
01700      SET  ELCIO-REC-AREA-ADDRESS3  TO  CIA-ELPCV-REC-AREA-PNTR.   ELELCAML
01701      EXEC CICS  LINK                                              ELELCAML
01702             PROGRAM('ELAIOPGM')                                   ELELCAML
01703             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01704             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01705      END-EXEC.                                                    ELELCAML
01706                                                                   ELELCAML
01707      IF ELCIO-REC-NOT-FOUND3                                      ELELCAML
01708           GO TO 4900-EXIT.                                        ELELCAML
01709 /                                                                 ELELCAML
01710  4900-CV-DELETE.                                                  ELELCAML
01711      MOVE 'EB '            TO ELCIO-FILE-ACCESS-CODE3.            ELELCAML
01712      EXEC CICS  LINK                                              ELELCAML
01713             PROGRAM('ELAIOPGM')                                   ELELCAML
01714             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01715             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01716      END-EXEC.                                                    ELELCAML
01717      IF ELCIO-REC-NOT-FOUND3                                      ELELCAML
01718           GO TO 4900-EXIT.                                        ELELCAML
01719                                                                   ELELCAML
01720 **********************************************                    ELELCAML
01721 **** READ AND DELETE CODE VALUES ****                             ELELCAML
01722 **********************************************                    ELELCAML
01723      IF PREF-D (SCREEN-CTR) NOT = CV-RECORD-PREFIX                ELELCAML
01724           GO TO 4900-EXIT.                                        ELELCAML
01725                                                                   ELELCAML
01726 **** DL  = DELETE WITH KEY KNOWN         ********                 ELELCAML
01727      MOVE 'DL '            TO ELCIO-FILE-ACCESS-CODE3.            ELELCAML
01728      MOVE CV-CODE-KEY      TO ELCIO-VSAM-KEY3.                    ELELCAML
01729      EXEC CICS  LINK                                              ELELCAML
01730             PROGRAM('ELAIOPGM')                                   ELELCAML
01731             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01732             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01733      END-EXEC.                                                    ELELCAML
01734      IF ELCIO-REC-NOT-FOUND3                                      ELELCAML
01735           GO TO 4900-EXIT.                                        ELELCAML
01736                                                                   ELELCAML
01737 **** SB  = TO GET READ NEXT GET NEXT CODE VALUE ********          ELELCAML
01738      MOVE 'SB '            TO ELCIO-FILE-ACCESS-CODE3.            ELELCAML
01739      MOVE 'GTE'            TO ELCIO-CIO-QUAL3.                    ELELCAML
01740      EXEC CICS  LINK                                              ELELCAML
01741             PROGRAM('ELAIOPGM')                                   ELELCAML
01742             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01743             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01744      END-EXEC.                                                    ELELCAML
01745      IF ELCIO-REC-NOT-FOUND3                                      ELELCAML
01746           GO TO 4900-EXIT.                                        ELELCAML
01747      IF NOT ELCIO-GOOD-RETURN3                                    ELELCAML
01748           GO TO 4900-EXIT.                                        ELELCAML
01749                                                                   ELELCAML
01750      GO TO 4900-CV-DELETE.                                        ELELCAML
01751  4900-EXIT.  EXIT.                                                ELELCAML
01752 /                                                                 ELELCAML
01753  5000-MAPFAIL.                                                    ELELCAML
01754      MOVE MAP-FAIL        TO ERRM-D.                              ELELCAML
01755      MOVE DFHBMASB TO ERRM-A.                                     ELELCAML
01756      MOVE -1              TO FCN-L (1).                           ELELCAML
01757      PERFORM 3000-SEND-SCREEN    THRU 3000-EXIT.                  ELELCAML
01758  5000-EXIT.    EXIT.                                              ELELCAML
01759 /                                                                 ELELCAML
01760  6000-CONFIRM.                                                    ELELCAML
01761      IF CA-DELETE OR                                              ELELCAML
01762           CA-MAP-FROM OR                                          ELELCAML
01763             CA-UNDELETE                                           ELELCAML
01764                NEXT SENTENCE                                      ELELCAML
01765      ELSE                                                         ELELCAML
01766        MOVE INVALID-USE-PF6 TO ERRM-D                             ELELCAML
01767        MOVE DFHBMASB TO ERRM-A                                    ELELCAML
01768        MOVE 1 TO SCREEN-CTR                                       ELELCAML
01769        MOVE -1 TO FCN-L (SCREEN-CTR)                              ELELCAML
01770        PERFORM 1800-SET-ATTRIBUTES    THRU 1800-EXIT              ELELCAML
01771        PERFORM 3000-SEND-SCREEN       THRU 3000-EXIT.             ELELCAML
01772                                                                   ELELCAML
01773      MOVE 'Y' TO BYPASS-SELECT-SW,                                ELELCAML
01774                  PF6-SW.                                          ELELCAML
01775      IF FCN-L (MAX-PROCESS-LINE) > +0                             ELELCAML
01776        IF FCN-D (MAX-PROCESS-LINE) NOT = 'M'                      ELELCAML
01777          MOVE IGNORED TO STATUS-D (MAX-PROCESS-LINE)              ELELCAML
01778          MOVE DFHBMASB TO STATUS-A (MAX-PROCESS-LINE)             ELELCAML
01779        ELSE                                                       ELELCAML
01780         MOVE MAX-PROCESS-LINE TO SCREEN-CTR                       ELELCAML
01781         MOVE FCN-D (SCREEN-CTR) TO CA-CURRENT-FUNCTION            ELELCAML
01782         PERFORM 4300-PROCESS-MAP-FUNCTION                         ELELCAML
01783            THRU 4300-EXIT.                                        ELELCAML
01784      MOVE 0 TO SCREEN-CTR.                                        ELELCAML
01785      PERFORM 1600-PROCESS-MAP                                     ELELCAML
01786         THRU 1600-EXIT.                                           ELELCAML
01787  6000-EXIT.    EXIT.                                              ELELCAML
01788 /                                                                 ELELCAML
01789  7000-SCROLL-BACK.                                                ELELCAML
01790                                                                   ELELCAML
01791 *-->   SET UP FOR RECORD LIST   <---------------*                 ELELCAML
01792      MOVE LOW-VALUES TO SPREF-D.                                  ELELCAML
01793      MOVE CA-FIRST-RECORD  TO RL-RECORD-PREFIX,                   ELELCAML
01794                               ELCIO-VSAM-KEY.                     ELELCAML
01795      MOVE 'SBP'            TO ELCIO-FILE-ACCESS-CODE.             ELELCAML
01796      MOVE 'EQ '            TO ELCIO-CIO-QUAL.                     ELELCAML
01797      MOVE EL-ELPRL-REC-LEN TO ELCIO-MAX-REC-LEN.                  ELELCAML
01798      MOVE 'M'              TO ELCIO-STORAGE.                      ELELCAML
01799      MOVE EL-DSN-ELPRL     TO ELCIO-FILE-DDNAME,                  ELELCAML
01800                               CIA-IO-GETMAIN-DDNAME.              ELELCAML
01801                                                                   ELELCAML
01802      SET  CIA-IO-PARM-AREA-PNTR  TO  ADDRESS OF                   ELELCAML
01803           IO-PARM-RECORD-LIST.                                    ELELCAML
01804      SET  ELCIO-REC-AREA-ADDRESS TO  ADDRESS OF                   ELELCAML
01805           EL-RECORD-LIST.                                         ELELCAML
01806                                                                   ELELCAML
01807        EXEC CICS  LINK                                            ELELCAML
01808             PROGRAM('ELAIOPGM')                                   ELELCAML
01809             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01810             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01811        END-EXEC.                                                  ELELCAML
01812 **************************************************                ELELCAML
01813      IF ELCIO-EOF-BROWSE                                          ELELCAML
01814           PERFORM 7020-ENDFILE-FULL  THRU                         ELELCAML
01815                   7020-EXIT                                       ELELCAML
01816           GO TO 7000-EXIT.                                        ELELCAML
01817                                                                   ELELCAML
01818      IF CA-FIRST-RECORD = HIGH-VALUES                             ELELCAML
01819           NEXT SENTENCE                                           ELELCAML
01820      ELSE                                                         ELELCAML
01821 ***** THE SBP AUTOMATICALLY CHANGES THE ACCESS TO CODE TO         ELELCAML
01822 *****  A RP READ PREVIOUS SO ALL THAT IS NEEDED IS A CALL TO      ELELCAML
01823 *****    ELAIOPGM                                                 ELELCAML
01824        EXEC CICS  LINK                                            ELELCAML
01825             PROGRAM('ELAIOPGM')                                   ELELCAML
01826             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01827             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01828        END-EXEC                                                   ELELCAML
01829        IF ELCIO-EOF-BROWSE                                        ELELCAML
01830             PERFORM 7010-END-BROWSE  THRU                         ELELCAML
01831                     7010-EXIT                                     ELELCAML
01832             GO TO 7000-EXIT.                                      ELELCAML
01833                                                                   ELELCAML
01834      MOVE 1  TO SCREEN-CTR.                                       ELELCAML
01835      PERFORM 1800-SET-ATTRIBUTES                                  ELELCAML
01836         THRU 1800-EXIT.                                           ELELCAML
01837      MOVE 16 TO SCREEN-CTR.                                       ELELCAML
01838 /                                                                 ELELCAML
01839  7000-READPREV.                                                   ELELCAML
01840      IF SCREEN-CTR < 1                                            ELELCAML
01841         PERFORM 7010-END-BROWSE  THRU                             ELELCAML
01842                 7010-EXIT                                         ELELCAML
01843         GO TO 7000-EXIT.                                          ELELCAML
01844                                                                   ELELCAML
01845      IF SCREEN-CTR = MAX-DISPLAY-LINE                             ELELCAML
01846         IF RL-RECORD-PREFIX = LOW-VALUES                          ELELCAML
01847            PERFORM 7020-ENDFILE-FULL  THRU   7020-EXIT            ELELCAML
01848            GO TO 7000-EXIT.                                       ELELCAML
01849                                                                   ELELCAML
01850      IF SCREEN-CTR = MAX-DISPLAY-LINE                             ELELCAML
01851           MOVE RL-RECORD-PREFIX TO CA-LAST-RECORD                 ELELCAML
01852           MOVE LOW-VALUES TO MAINTENANCE-AREA                     ELELCAML
01853           MOVE INITIAL-VALUE TO ERRM-D                            ELELCAML
01854           MOVE 'Y' TO ERASEAUP-SW.                                ELELCAML
01855                                                                   ELELCAML
01856      MOVE RL-RECORD-NAME   TO NAME-D (SCREEN-CTR).                ELELCAML
01857      MOVE RL-RECORD-PREFIX TO PREF-D (SCREEN-CTR),                ELELCAML
01858      MOVE RL-FILE          TO FILE-D (SCREEN-CTR).                ELELCAML
01859      MOVE DFHBMPRF         TO PREF-A (SCREEN-CTR).                ELELCAML
01860      MOVE DFHBMUNP         TO FCN-A (SCREEN-CTR).                 ELELCAML
01861      IF NOT CA-INQUIRY                                            ELELCAML
01862           MOVE DFHBMUNP         TO NAME-A (SCREEN-CTR),           ELELCAML
01863           MOVE DFHBMUNP         TO FILE-A (SCREEN-CTR),           ELELCAML
01864                                    STATUS-A (SCREEN-CTR)          ELELCAML
01865      ELSE                                                         ELELCAML
01866 ****NEED TO CLEAR THE STATUS FIELD FOR INQUIRY SINCE IT IS        ELELCAML
01867 ****  A PROTECTED FIELD THAT IS NOT ERASED WHEN THE MAP IS        ELELCAML
01868 ****  RESENT.                                                     ELELCAML
01869        MOVE SPACES TO STATUS-D (SCREEN-CTR).                      ELELCAML
01870      IF SCREEN-CTR = 1                                            ELELCAML
01871          MOVE RL-RECORD-PREFIX TO CA-FIRST-RECORD.                ELELCAML
01872                                                                   ELELCAML
01873      IF RL-DELETE                                                 ELELCAML
01874          MOVE DELETED         TO STATUS-D (SCREEN-CTR)            ELELCAML
01875          IF CA-INQUIRY                                            ELELCAML
01876             MOVE DFHBMASB TO STATUS-A (SCREEN-CTR)                ELELCAML
01877          ELSE                                                     ELELCAML
01878           MOVE DFHBMBRY TO STATUS-A (SCREEN-CTR).                 ELELCAML
01879      SUBTRACT 1 FROM SCREEN-CTR.                                  ELELCAML
01880                                                                   ELELCAML
01881      EXEC CICS  LINK                                              ELELCAML
01882             PROGRAM('ELAIOPGM')                                   ELELCAML
01883             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01884             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01885      END-EXEC.                                                    ELELCAML
01886      IF ELCIO-EOF-BROWSE                                          ELELCAML
01887             PERFORM 7010-END-BROWSE  THRU                         ELELCAML
01888                     7010-EXIT                                     ELELCAML
01889      ELSE                                                         ELELCAML
01890           GO TO 7000-READPREV.                                    ELELCAML
01891  7000-EXIT.    EXIT.                                              ELELCAML
01892                                                                   ELELCAML
01893  7010-END-BROWSE.                                                 ELELCAML
01894 **** IF THE SCREEN IS NOT FILLED BY BACKWARD SCROLL THE PROGRAM   ELELCAML
01895 ****   WILL BRANCH TO SCROLL-FWD TO DISPLAY A FULL SCREEN         ELELCAML
01896      IF SCREEN-CTR > 0                                            ELELCAML
01897         MOVE 'Y' TO BEGINNING-FILE-SW                             ELELCAML
01898         MOVE LOW-VALUES  TO CA-LAST-RECORD                        ELELCAML
01899         PERFORM 8000-SCROLL-FWD THRU      8000-EXIT.              ELELCAML
01900         MOVE -1  TO FCN-L (1).                                    ELELCAML
01901         PERFORM 3000-SEND-SCREEN      THRU 3000-EXIT.             ELELCAML
01902                                                                   ELELCAML
01903  7010-EXIT.    EXIT.                                              ELELCAML
01904                                                                   ELELCAML
01905  7020-ENDFILE-FULL.                                               ELELCAML
01906                                                                   ELELCAML
01907      MOVE 'EB '            TO ELCIO-FILE-ACCESS-CODE.             ELELCAML
01908      EXEC CICS  LINK                                              ELELCAML
01909             PROGRAM('ELAIOPGM')                                   ELELCAML
01910             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01911             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01912      END-EXEC.                                                    ELELCAML
01913                                                                   ELELCAML
01914      MOVE BEGIN-FILE-ENCOUNTERED TO ERRM-D.                       ELELCAML
01915      MOVE -1  TO FCN-L (1).                                       ELELCAML
01916      PERFORM 3000-SEND-SCREEN  THRU 3000-EXIT.                    ELELCAML
01917                                                                   ELELCAML
01918  7020-EXIT.    EXIT.                                              ELELCAML
01919 /                                                                 ELELCAML
01920  8000-SCROLL-FWD.                                                 ELELCAML
01921      MOVE LOW-VALUES TO SPREF-D.                                  ELELCAML
01922      IF CA-LAST-RECORD = HIGH-VALUES                              ELELCAML
01923           MOVE 17 TO SCREEN-CTR                                   ELELCAML
01924           MOVE END-FILE-ENCOUNTERED TO ERRM-D                     ELELCAML
01925           MOVE DFHBMASB             TO ERRM-A                     ELELCAML
01926           GO TO 8000-SEND-SCREEN.                                 ELELCAML
01927                                                                   ELELCAML
01928 *-->   SET UP FOR RECORD LIST   <---------------*                 ELELCAML
01929      MOVE CA-LAST-RECORD   TO RL-RECORD-PREFIX,                   ELELCAML
01930                               ELCIO-VSAM-KEY.                     ELELCAML
01931      MOVE 'SB '            TO ELCIO-FILE-ACCESS-CODE.             ELELCAML
01932      MOVE 'EQ '            TO ELCIO-CIO-QUAL.                     ELELCAML
01933      MOVE EL-ELPRL-REC-LEN TO ELCIO-MAX-REC-LEN.                  ELELCAML
01934      MOVE 'M'              TO ELCIO-STORAGE.                      ELELCAML
01935      MOVE EL-DSN-ELPRL     TO ELCIO-FILE-DDNAME,                  ELELCAML
01936                               CIA-IO-GETMAIN-DDNAME.              ELELCAML
01937                                                                   ELELCAML
01938      SET  CIA-IO-PARM-AREA-PNTR    TO  ADDRESS OF                 ELELCAML
01939           IO-PARM-RECORD-LIST.                                    ELELCAML
01940      SET  ELCIO-REC-AREA-ADDRESS   TO  ADDRESS OF                 ELELCAML
01941           EL-RECORD-LIST.                                         ELELCAML
01942                                                                   ELELCAML
01943        EXEC CICS  LINK                                            ELELCAML
01944             PROGRAM('ELAIOPGM')                                   ELELCAML
01945             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01946             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01947        END-EXEC.                                                  ELELCAML
01948                                                                   ELELCAML
01949      IF  ELCIO-EOF-BROWSE                                         ELELCAML
01950          GO TO 8000-NOTFND-ERROR.                                 ELELCAML
01951                                                                   ELELCAML
01952      IF CA-RECORD-LIST                                            ELELCAML
01953        OR CA-SIGNON                                               ELELCAML
01954 ***** THE SBP AUTOMATICALLY CHANGES THE ACCESS TO CODE TO         ELELCAML
01955 *****  A RP READ PREVIOUS SO ALL THAT IS NEEDED IS A CALL TO      ELELCAML
01956 *****    ELAIOPGM                                                 ELELCAML
01957        EXEC CICS  LINK                                            ELELCAML
01958             PROGRAM('ELAIOPGM')                                   ELELCAML
01959             COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                 ELELCAML
01960             LENGTH(EL-CIA-POINTER-LEN)                            ELELCAML
01961        END-EXEC                                                   ELELCAML
01962        IF ELCIO-EOF-BROWSE                                        ELELCAML
01963             GO TO 8000-SEND-SCREEN.                               ELELCAML
01964      MOVE 1   TO SCREEN-CTR.                                      ELELCAML
01965 /                                                                 ELELCAML
01966  8000-READNEXT.                                                   ELELCAML
01967      IF SCREEN-CTR > MAX-DISPLAY-LINE                             ELELCAML
01968          MOVE 'EB '  TO ELCIO-FILE-ACCESS-CODE                    ELELCAML
01969          EXEC CICS LINK                                           ELELCAML
01970              PROGRAM('ELAIOPGM')                                  ELELCAML
01971              COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                ELELCAML
01972              LENGTH(EL-CIA-POINTER-LEN)                           ELELCAML
01973          END-EXEC                                                 ELELCAML
01974          GO TO 8000-SEND-SCREEN.                                  ELELCAML
01975                                                                   ELELCAML
01976      IF SCREEN-CTR = 1                                            ELELCAML
01977        MOVE RL-RECORD-PREFIX TO CA-FIRST-RECORD                   ELELCAML
01978        MOVE LOW-VALUES TO MAINTENANCE-AREA                        ELELCAML
01979        MOVE 'Y' TO ERASEAUP-SW                                    ELELCAML
01980        PERFORM 1800-SET-ATTRIBUTES                                ELELCAML
01981           THRU 1800-EXIT                                          ELELCAML
01982        MOVE 1   TO SCREEN-CTR                                     ELELCAML
01983        IF BEGINNING-FILE-SW = 'Y'                                 ELELCAML
01984            MOVE BEGIN-FILE-ENCOUNTERED TO ERRM-D                  ELELCAML
01985        ELSE                                                       ELELCAML
01986           MOVE INITIAL-VALUE TO ERRM-D.                           ELELCAML
01987                                                                   ELELCAML
01988      MOVE RL-RECORD-PREFIX TO PREF-D (SCREEN-CTR).                ELELCAML
01989      MOVE RL-RECORD-NAME   TO NAME-D (SCREEN-CTR).                ELELCAML
01990      MOVE RL-FILE          TO FILE-D (SCREEN-CTR).                ELELCAML
01991      MOVE DFHBMPRF         TO PREF-A (SCREEN-CTR).                ELELCAML
01992      MOVE DFHBMUNP         TO FCN-A (SCREEN-CTR).                 ELELCAML
01993      IF NOT CA-INQUIRY                                            ELELCAML
01994           MOVE DFHBMUNP         TO NAME-A (SCREEN-CTR),           ELELCAML
01995           MOVE DFHBMUNP         TO FILE-A (SCREEN-CTR),           ELELCAML
01996                                    STATUS-A (SCREEN-CTR)          ELELCAML
01997      ELSE                                                         ELELCAML
01998 ****NEED TO CLEAR THE STATUS FIELD FOR INQUIRY SINCE IT IS        ELELCAML
01999 ****  A PROTECTED FIELD THAT IS NOT ERASED WHEN THE MAP IS        ELELCAML
02000 ****  RESENT                                                      ELELCAML
02001        MOVE SPACES TO STATUS-D (SCREEN-CTR).                      ELELCAML
02002      IF RL-DELETE                                                 ELELCAML
02003          MOVE DELETED         TO STATUS-D (SCREEN-CTR)            ELELCAML
02004          IF CA-INQUIRY                                            ELELCAML
02005              MOVE DFHBMASB TO STATUS-A (SCREEN-CTR)               ELELCAML
02006          ELSE                                                     ELELCAML
02007              MOVE DFHBMBRY TO STATUS-A (SCREEN-CTR).              ELELCAML
02008                                                                   ELELCAML
02009      IF SCREEN-CTR = MAX-DISPLAY-LINE                             ELELCAML
02010           MOVE RL-RECORD-PREFIX TO CA-LAST-RECORD.                ELELCAML
02011      ADD 1 TO SCREEN-CTR.                                         ELELCAML
02012                                                                   ELELCAML
02013 ***** THE SBP AUTOMATICALLY CHANGES THE ACCESS TO CODE TO         ELELCAML
02014 *****  A RP READ PREVIOUS SO ALL THAT IS NEEDED IS A CALL TO      ELELCAML
02015 *****    ELAIOPGM                                                 ELELCAML
02016      EXEC CICS LINK                                               ELELCAML
02017           PROGRAM('ELAIOPGM')                                     ELELCAML
02018           COMMAREA(ADDRESS OF CIA-PARMS-RECORD)                   ELELCAML
02019           LENGTH(EL-CIA-POINTER-LEN)                              ELELCAML
02020      END-EXEC.                                                    ELELCAML
02021      IF ELCIO-EOF-BROWSE                                          ELELCAML
02022           GO TO 8000-SEND-SCREEN.                                 ELELCAML
02023 *****************************************************             ELELCAML
02024      GO TO 8000-READNEXT.                                         ELELCAML
02025 /                                                                 ELELCAML
02026  8000-NOTFND-ERROR.                                               ELELCAML
02027      MOVE UNABLE-TO-START-BR TO ERRM-D.                           ELELCAML
02028      MOVE DFHBMASB          TO ERRM-A.                            ELELCAML
02029  8000-SEND-SCREEN.                                                ELELCAML
02030      IF SCREEN-CTR = ZERO                                         ELELCAML
02031          MOVE 1 TO SCREEN-CTR.                                    ELELCAML
02032      IF SCREEN-CTR NOT > MAX-DISPLAY-LINE                         ELELCAML
02033           MOVE END-FILE-ENCOUNTERED TO ERRM-D                     ELELCAML
02034 ******************************************************************ELELCAML
02035 **** THE IF STATEMENT HERE INSURES PROPER SCROLLING FUNCTION     *ELELCAML
02036 ****   WHEN THE LAST RECORDS IN THE FILE EXACTLY FILL THE LAST   *ELELCAML
02037 ****   SCREEN                                                    *ELELCAML
02038 ******************************************************************ELELCAML
02039           IF SCREEN-CTR = 1                                       ELELCAML
02040              MOVE 17          TO SCREEN-CTR                       ELELCAML
02041           ELSE                                                    ELELCAML
02042              MOVE HIGH-VALUES     TO CA-LAST-RECORD.              ELELCAML
02043  8000-CLEAR-REMAIN.                                               ELELCAML
02044      IF SCREEN-CTR NOT > MAX-DISPLAY-LINE                         ELELCAML
02045              MOVE DFHBMUNP TO PREF-A (SCREEN-CTR),                ELELCAML
02046                                      FCN-A (SCREEN-CTR),          ELELCAML
02047                                      NAME-A (SCREEN-CTR),         ELELCAML
02048                                      FILE-A (SCREEN-CTR),         ELELCAML
02049                                      STATUS-A (SCREEN-CTR)        ELELCAML
02050              MOVE SPACES          TO PREF-D (SCREEN-CTR)          ELELCAML
02051                                      NAME-D (SCREEN-CTR)          ELELCAML
02052                                      FILE-D (SCREEN-CTR)          ELELCAML
02053                                      STATUS-D (SCREEN-CTR)        ELELCAML
02054              ADD 1 TO SCREEN-CTR                                  ELELCAML
02055              GO TO 8000-CLEAR-REMAIN.                             ELELCAML
02056      IF NOT CA-INQUIRY                                            ELELCAML
02057         MOVE DFHBMUNP TO PREF-A (MAX-PROCESS-LINE).               ELELCAML
02058      MOVE -1  TO FCN-L (1).                                       ELELCAML
02059      PERFORM 3000-SEND-SCREEN    THRU 3000-EXIT.                  ELELCAML
02060  8000-EXIT.    EXIT.                                              ELELCAML
02061 /                                                                 ELELCAML
02062  9000-RETURN-CICS.                                                ELELCAML
02063      EXEC CICS                                                    ELELCAML
02064          RETURN                                                   ELELCAML
02065      END-EXEC.                                                    ELELCAML
02066      GOBACK.                                                      ELELCAML
02067  9000-EXIT.    EXIT.                                              ELELCAML
02068  TITLE 'CODES MANUAL PROGRAM - ELELCAML'.                         ELELCAML
