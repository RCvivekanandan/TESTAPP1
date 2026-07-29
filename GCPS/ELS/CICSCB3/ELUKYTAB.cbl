00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELUKYTAB
00003  PROGRAM-ID.         ELUKYTAB.                                       LV002
00004                                                                   ELUKYTAB
00005  AUTHOR.             RICHARD J. LUKETICH                          ELUKYTAB
00006                                                                   ELUKYTAB
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELUKYTAB
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELUKYTAB
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELUKYTAB
00010                      233 N. MICHIGAN AVE                          ELUKYTAB
00011                      CHICAGO, ILLINOIS 60601                      ELUKYTAB
00012                                                                   ELUKYTAB
00013  DATE-WRITTEN.       28-OCT-1986.                                 ELUKYTAB
00014                                                                   ELUKYTAB
00015  DATE-COMPILED.                                                   ELUKYTAB
00016                                                                   ELUKYTAB
00017  SECURITY.           COPYRIGHT 1986,                              ELUKYTAB
00018                      HEALTH CARE SERVICE CORPORATION              ELUKYTAB
00019      TITLE 'ELS KEY TABLE BUILDER MODULE'.                        ELUKYTAB
00020 *************************************************************     ELUKYTAB
00021 *     * * * U P D A T E   H IS T O R Y * * *                      ELUKYTAB
00022 *                                                                 ELUKYTAB
00023 *   DATE     BY       COMMENTS                                    ELUKYTAB
00024 *                                                                 ELUKYTAB
00025 *  10-28-86  RJL      CREATED                                     ELUKYTAB
00026 *                                                                 ELUKYTAB
00027 *  10-15-87  AKK      ADDED CODE TO COMPARE THE COVERED FROM      ELUKYTAB
00028 *                     AND TO DATES IN THE GRP SPEC AND CONTRACT   ELUKYTAB
00029 *                     FILES WITH A CONSTANT (99365 FOR COVERED    ELUKYTAB
00030 *                     FROM FATE, 0 FOR COVERED TO DATE).  THE     ELUKYTAB
00031 *                     RESULTS OF THE COMPARISON IS PASSED TO      ELUKYTAB
00032 *                     ELSBEGIN AND WILL HELP IT DETERMINE IF A    ELUKYTAB
00033 *                     VALID CONTRACT EXISTS FOR THE DATE          ELUKYTAB
00034 *                     SPECIFIED.                                  ELUKYTAB
00035 *                                                                 ELUKYTAB
00036 *  10-28-87  AKK      ADDED CODE SO THE SECTION TABLE WILL        ELUKYTAB
00037 *                     BE CREATED BY MERGING THE GROUP SPECIFIC    ELUKYTAB
00038 *                     AND CONTRACT SECTION FILES  FOR THE GROUP   ELUKYTAB
00039 *                     SELECTED.  TWO NEW ABEND CODES WERE         ELUKYTAB
00040 *                     THEN ADDED TO THE CIA BLOCK (EL53 AND       ELUKYTAB
00041 *                     EL54) TO TRAP GROUP/SECTIONS WHERE NO       ELUKYTAB
00042 *                     GROUP SPECIFIC OR NO CONTRACT RECORD        ELUKYTAB
00043 *                     EXIST.                                      ELUKYTAB
00044 *  01-25-88  EGL      ADDED CODE TO MOVE FILL IN THE GROUP        ELUKYTAB
00045 *                     PPO OPTION TO KTG-PARTICIPAT-PROV-OPTION    ELUKYTAB
00046 *  07-19-88  EGL      ADDED CODE TO USE NEW STORAGE MANAGEMENT    ELUKYTAB
00047 *                     ROUTINES.                                   ELUKYTAB
00048 *                                                                 ELUKYTAB
00049 *  10-31-88  JPB      ADDED CODE TO CHECK FOR INTERRELATIONAL     ELUKYTAB
00050 *                     CODES EQUAL TO ZERO.                        ELUKYTAB
00051 *                                                                 ELUKYTAB
00052 *  12-13-89  EGL     -DESTRUCTED PROGRAM AND CLEANED IT UP.       ELUKYTAB
00053 *                    -REVISED LOGIC DEALING WITH INTERRELATIONAL  ELUKYTAB
00054 *                     CODE EQUAL TO ZERO.                         ELUKYTAB
00055 *                                                                 ELUKYTAB
00056 *  07-26-90  RJL     -REMOVED RETURN CODE FOR INTERRELATIONAL     ELUKYTAB
00057 *                     CODE EQUAL TO ZERO FOUND.  (CAUSES ENTIRE   ELUKYTAB
00058 *                     SECTION TO BE UNAVAILABLE.)                 ELUKYTAB
00059 *                    -MOVED GOBACK STATEMENT TO MAINLINE OF       ELUKYTAB
00060 *                     PROCEDURE DIVISION.                         ELUKYTAB
00061 *                                                                 ELUKYTAB
00062 *  09-27-91  JPB     -ADDED POPULATION OF KTG-POS-PARTICP-IND     ELUKYTAB
00063 *                     AND KTG-NEW-POS-IND.                        ELUKYTAB
00064 *                                                                 ELUKYTAB
00065 *  01-06-93  AKK     POPULATED KTG-RPO-PARTICPIAT-IND             ELUKYTAB
00066 *                                                                 ELUKYTAB
00067 *  02-03-94  AKK     POPULATED KTG-PRODUCT-TYPE FOR USE IN        ELUKYTAB
00068 *                    ELSNOTE1.                                    ELUKYTAB
00069 *                                                                 ELUKYTAB
00070 *  03-07-95  AKK     POPULATED KTG-CPO-PARTICP-IND FOR USE IN     ELUKYTAB
00071 *                    ELSNOTE1.                                    ELUKYTAB
00072 *                                                                 ELUKYTAB
00073 *  05-23-95  AKK     POPULATED KTG-BLUE-SCRIPT FOR USE IN         ELUKYTAB
00074 *                    ELSNOTE1.                                    ELUKYTAB
00075 *                                                                 ELUKYTAB
00076 *  09-13-95  AKK     POPULATED KTG-ALLIANCE-IND FOR USE IN        ELUKYTAB
00077 *                    ELSNOTE1.  THE INFO IS FROM MEMBERSHIP       ELUKYTAB
00078 *                    BUT RE: CUSTOMER REQUEST A SENTENCE IS       ELUKYTAB
00079 *                    REQUESTED TO BE DISPLAYED ON NOTIFICATION    ELUKYTAB
00080 *                    SCREEN.                                      ELUKYTAB
00081 *                                                                 ELUKYTAB
00082 *  04-09-96  AKK     POPULATED KTG-CBL AND PAN INDICATORS FOR     ELUKYTAB
00083 *                    ELSNOTE1.                                    ELUKYTAB
00084 *                                                                 ELUKYTAB
00085 *  11-04-96  AKK     ADDED AN ABEND CODE TO SIGNAL-IO-ERROR       ELUKYTAB
00086 *                    PARAGRAPH.  WITHOUT IT, GCD01B IS            ELUKYTAB
00087 *                    ABENDING PERIODICALLY. USING ELXX.           ELUKYTAB
00088 *                                                                 ELUKYTAB
00089 *  10-03-97  AKK     ADDED SUPPORT FOR YR2000 AND TEXAS MERGER.   ELUKYTAB
00090 *                                                                 ELUKYTAB
00091 *  02-08-00  JP      ADDED SPERATE ROUTINE FOR BUILDING KTS TABLE ELUKYTAB
00092 *                    WHEN BOTH GROUP AND SECTION ARE PROVIDED BY  ELUKYTAB
00093 *                    USER ON THE PRIMARY SCREEN.                  ELUKYTAB
00094 *                                                                 ELUKYTAB
00095 *       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE       *ELUKYTAB
00096 ******************************************************************ELUKYTAB
00097  ENVIRONMENT DIVISION.                                            ELUKYTAB
00098                                                                   ELUKYTAB
00099  CONFIGURATION SECTION.                                           ELUKYTAB
00100  SOURCE-COMPUTER.    IBM-3033.                                    ELUKYTAB
00101  OBJECT-COMPUTER.    IBM-3033.                                    ELUKYTAB
00102 /                                                                 ELUKYTAB
00103  DATA DIVISION.                                                   ELUKYTAB
00104                                                                   ELUKYTAB
00105  WORKING-STORAGE SECTION.                                         ELUKYTAB
00106                                                                   ELUKYTAB
00107  01  PROGRAM-CONSTANTS.                                           ELUKYTAB
00108      03  PC-FROM-DTA     PIC S9(07)  VALUE +9999365.              ELUKYTAB
00109      03  PC-TO-DTA       PIC S9(07)  VALUE +0.                    ELUKYTAB
00110                                                                   ELUKYTAB
00111  01  WS-TWA-PTR          POINTER     VALUE NULL.                  ELUKYTAB
00112  01  WS-TWA-HOLD-PTR     POINTER     VALUE NULL.                  ELUKYTAB
00113                                                                   ELUKYTAB
00114  01  WS-MISC-STUFF.                                               ELUKYTAB
00115                                                                   ELUKYTAB
00116      05  WS-KEY-LEN          PIC 9(04)   VALUE 0.                 ELUKYTAB
00117                                                                   ELUKYTAB
00118      05  WS-GROUP-REC-IND    PIC X(01)   VALUE 'N'.               ELUKYTAB
00119          88  GRP-FOUND                   VALUE 'Y'.               ELUKYTAB
00120          88  GRP-NOT-FOUND               VALUE 'N'.               ELUKYTAB
00121                                                                   ELUKYTAB
00122      05  WS-GRP-NBR.                                              ELUKYTAB
00123          10  FILLER          PIC X(03)   VALUE '000'.             ELUKYTAB
00124          10  WS-GRP-NBRA     PIC X(06)   VALUE SPACE.             ELUKYTAB
00125                                                                   ELUKYTAB
00126      05  WS-GROUP-EOF-IND    PIC X(01)   VALUE 'N'.               ELUKYTAB
00127          88  GRP-EOF                     VALUE 'Y'.               ELUKYTAB
00128          88  GRP-NOT-EOF                 VALUE 'N'.               ELUKYTAB
00129                                                                   ELUKYTAB
00130      05  WS-CONTRACT-REC-IND PIC X(01)   VALUE 'N'.               ELUKYTAB
00131          88  CONT-FOUND                  VALUE 'Y'.               ELUKYTAB
00132          88  CONT-NOT-FOUND              VALUE 'N'.               ELUKYTAB
00133                                                                   ELUKYTAB
00134      05  WS-CONTRACT-EOF-IND PIC X(01)   VALUE 'N'.               ELUKYTAB
00135          88  CONT-EOF                    VALUE 'Y'.               ELUKYTAB
00136          88  CONT-NOT-EOF                VALUE 'N'.               ELUKYTAB
00137                                                                   ELUKYTAB
00138      05  WS-LOOP-CONTROL-IND PIC X(01)   VALUE 'N'.               ELUKYTAB
00139          88  CONTINUE-LOOP               VALUE 'Y'.               ELUKYTAB
00140          88  TERMINATE-LOOP              VALUE 'N'.               ELUKYTAB
00141                                                                   ELUKYTAB
00142      05  WS-EXCLUDE-IND      PIC X(01)   VALUE 'N'.               ELUKYTAB
00143          88  EXCLUDE-FOUND               VALUE 'Y'.               ELUKYTAB
00144          88  EXCLUDE-NOT-FOUND           VALUE 'N'.               ELUKYTAB
00145                                                                   ELUKYTAB
00146      05  WS-SECT-FND-IND     PIC X(01)   VALUE 'Y'.               ELUKYTAB
00147          88  SECTION-FOUND               VALUE 'Y'.               ELUKYTAB
00148          88  SECTION-NOT-FOUND           VALUE 'N'.               ELUKYTAB
00149                                                                   ELUKYTAB
00150      05  WS-MAX-KTS          PIC S9(4)   COMP SYNC.               ELUKYTAB
00151      05  WS-MAX-KTG          PIC S9(4)   COMP SYNC.               ELUKYTAB
00152      05  WS-MAX-KTC          PIC S9(4)   COMP SYNC.               ELUKYTAB
00153      05  WS-KTC-SUB          PIC S9(4)   COMP SYNC.               ELUKYTAB
00154                                                                   ELUKYTAB
00155      05  WS-DUMMY-PTR        POINTER.                             ELUKYTAB
00156                                                                   ELUKYTAB
00157      05  WS-ELSKTBG-SW       PIC X(01)   VALUE 'N'.               ELUKYTAB
00158          88 WS-ELSKTBG-ALLOCATED         VALUE 'Y'.               ELUKYTAB
00159          88 WS-ELSKTBG-NOT-ALLOCATED     VALUE 'N'.               ELUKYTAB
00160 /                                                                 ELUKYTAB
00161  LINKAGE SECTION.                                                 ELUKYTAB
00162                                                                   ELUKYTAB
00163  01  DFHCOMMAREA.                                                 ELUKYTAB
00164      COPY ELSCOMMC.                                               ELUKYTAB
00165 /                                                                 ELUKYTAB
00166      COPY COB2XGMF.                                               ELUKYTAB
00167 /                                                                 ELUKYTAB
00168      COPY ELSCIA2C.                                               ELUKYTAB
00169 /                                                                 ELUKYTAB
00170      COPY ELSIOPMC.                                               ELUKYTAB
00171 /                                                                 ELUKYTAB
00172      COPY ELSKTBCC.                                               ELUKYTAB
00173 /                                                                 ELUKYTAB
00174      COPY ELSKTBGC.                                               ELUKYTAB
00175 /                                                                 ELUKYTAB
00176      COPY ELSMEMSC.                                               ELUKYTAB
00177 /                                                                 ELUKYTAB
00178      COPY ELSKTBSC.                                               ELUKYTAB
00179 /                                                                 ELUKYTAB
00180      COPY ELSSSCBC.                                               ELUKYTAB
00181 /                                                                 ELUKYTAB
00182      COPY ELSKEYSC.                                               ELUKYTAB
00183 /                                                                 ELUKYTAB
00184      COPY ELSTWAC.                                                ELUKYTAB
00185 /                                                                 ELUKYTAB
00186  01  GCG-GROUP-SPECIFIC-RECORD-AREA.                              ELUKYTAB
00187      COPY GCGROUPC.                                               ELUKYTAB
00188 /                                                                 ELUKYTAB
00189  01  GCT-CONTRACT-RECORD-AREA.                                    ELUKYTAB
00190      COPY GCCONTRC.                                               ELUKYTAB
00191 /                                                                 ELUKYTAB
00192  01  000-MEMBER-HEADER.                                           ELUKYTAB
00193      COPY MSRDN000.                                               ELUKYTAB
00194 /                                                                 ELUKYTAB
00195 ******************************************************************ELUKYTAB
00196 *                                                                 ELUKYTAB
00197                                                                   ELUKYTAB
00198 * TO DETERMINE IF THIS GROUP AND SECTION IS AN ALLIANCE GRP       ELUKYTAB
00199 * IN ORDER TO SEND OUT MESSAGE FOR ELSNOTE1                       ELUKYTAB
00200 ******************************************************************ELUKYTAB
00201 *                                                                 ELUKYTAB
00202  PROCEDURE DIVISION.                                              ELUKYTAB
00203      PERFORM BUILD-KEY-TABLES-INITIALIZE.                         ELUKYTAB
00204      PERFORM BUILD-KEY-TABLES-PROCESS.                            ELUKYTAB
00205      PERFORM BUILD-KEY-TABLES-TERMINATE.                          ELUKYTAB
00206      GOBACK.                                                      ELUKYTAB
00207                                                                   ELUKYTAB
00208  BUILD-KEY-TABLES-INITIALIZE.                                     ELUKYTAB
00209      IF EIBCALEN IS NOT EQUAL TO LENGTH OF DFHCOMMAREA            ELUKYTAB
00210          PERFORM SIGNAL-COMMAREA-LENGTH-ERROR.                    ELUKYTAB
00211      PERFORM ESTAB-ADDR-OF-ELSCIAC.                               ELUKYTAB
00212      PERFORM ESTAB-ADDR-OF-ELSSSCBC.                              ELUKYTAB
00213      MOVE ZEROES TO SSB-PLAN-CODE.                                ELUKYTAB
00214      PERFORM ESTAB-ADDR-OF-ELSKEYSC.                              ELUKYTAB
00215      MOVE PC-FROM-DTA TO SSB-COVRD-FROM-DATE-CEN.                 ELUKYTAB
00216      MOVE PC-TO-DTA TO SSB-COVRD-TO-DATE-CEN.                     ELUKYTAB
00217                                                                   ELUKYTAB
00218  ESTAB-ADDR-OF-ELSCIAC.                                           ELUKYTAB
00219      CALL 'ELUINISM' USING DFHCOMMAREA                            ELUKYTAB
00220                 ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.         ELUKYTAB
00221      IF ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA = NULL           ELUKYTAB
00222          PERFORM SIGNAL-CIA-ADDRESSING-ERROR.                     ELUKYTAB
00223                                                                   ELUKYTAB
00224  ESTAB-ADDR-OF-ELSSSCBC.                                          ELUKYTAB
00225      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELUKYTAB
00226      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYTAB
00227          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELUKYTAB
00228      IF NOT CIA-RC-OK                                             ELUKYTAB
00229          PERFORM SIGNAL-MISSING-PARAMETER.                        ELUKYTAB
00230                                                                   ELUKYTAB
00231  ESTAB-ADDR-OF-ELSKEYSC.                                          ELUKYTAB
00232      PERFORM SET-KWA-ADDRESS.                                     ELUKYTAB
00233      IF NOT CIA-RC-OK                                             ELUKYTAB
00234          PERFORM ALLOCATE-ELSKEYSC.                               ELUKYTAB
00235                                                                   ELUKYTAB
00236  ALLOCATE-ELSKEYSC.                                               ELUKYTAB
00237      SET  CIA-ELSKEYS-DDN TO TRUE.                                ELUKYTAB
00238      MOVE ZERO TO CIA-AREA-LEN.                                   ELUKYTAB
00239      PERFORM ACQUIRE-CONTROLLED-STORAGE.                          ELUKYTAB
00240      PERFORM SET-KWA-ADDRESS.                                     ELUKYTAB
00241                                                                   ELUKYTAB
00242  SET-KWA-ADDRESS.                                                 ELUKYTAB
00243      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELUKYTAB
00244      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYTAB
00245           ADDRESS OF KWA-FILE-KEY-WORK-AREA.                      ELUKYTAB
00246                                                                   ELUKYTAB
00247 /***********************************************************      ELUKYTAB
00248 *                                                          *      ELUKYTAB
00249 *        BUILD KEY TABLES.PROCESS                          *      ELUKYTAB
00250 *                                                          *      ELUKYTAB
00251 ************************************************************      ELUKYTAB
00252  BUILD-KEY-TABLES-PROCESS.                                        ELUKYTAB
00253      IF SSB-GROUP-NUMBER IS NOT EQUAL TO LOW-VALUES               ELUKYTAB
00254          PERFORM ESTABLISH-ADDRESS-OF-ELSKTBGC                    ELUKYTAB
00255          PERFORM ALLOCATE-ELSKTBGC                                ELUKYTAB
00256          PERFORM BUILD-ALL-TABLES                                 ELUKYTAB
00257          PERFORM SEARCH-FOR-ALLIANCE                              ELUKYTAB
00258      ELSE                                                         ELUKYTAB
00259          PERFORM SIGNAL-UNKNOWN-ERROR.                            ELUKYTAB
00260                                                                   ELUKYTAB
00261  BUILD-ALL-TABLES.                                                ELUKYTAB
00262      IF SSB-SS-GET-GROUP AND SSB-SECTN-NO = LOW-VALUES            ELUKYTAB
00263          PERFORM BUILD-GROUP-TABLE                                ELUKYTAB
00264        ELSE                                                       ELUKYTAB
00265          IF SSB-SS-GET-GROUP AND SSB-SECTN-NO  NOT = LOW-VALUES   ELUKYTAB
00266             PERFORM BUILD-GROUP-TABLE-2                           ELUKYTAB
00267        ELSE                                                       ELUKYTAB
00268          IF SSB-NO-SECTN-NO                                       ELUKYTAB
00269              PERFORM SIGNAL-UNKNOWN-ERROR                         ELUKYTAB
00270          ELSE                                                     ELUKYTAB
00271              PERFORM BUILD-GROUP-SECTION-KEY-TABS.                ELUKYTAB
00272                                                                   ELUKYTAB
00273  BUILD-GROUP-TABLE.                                               ELUKYTAB
00274      PERFORM BUILD-SECTION-TABLE.                                 ELUKYTAB
00275      IF  SSB-NO-SECTN-NO OR GRP-NOT-FOUND OR CONT-NOT-FOUND       ELUKYTAB
00276          CONTINUE                                                 ELUKYTAB
00277      ELSE                                                         ELUKYTAB
00278          PERFORM BUILD-GROUP-SECTION-KEY-TABS                     ELUKYTAB
00279      END-IF.                                                      ELUKYTAB
00280                                                                   ELUKYTAB
00281  BUILD-GROUP-TABLE-2.                                             ELUKYTAB
00282      PERFORM BUILD-SECTION-TABLE-2.                               ELUKYTAB
00283      IF  GRP-NOT-FOUND OR CONT-NOT-FOUND                          ELUKYTAB
00284          CONTINUE                                                 ELUKYTAB
00285      ELSE                                                         ELUKYTAB
00286          PERFORM BUILD-GROUP-SECTION-KEY-TABS                     ELUKYTAB
00287      END-IF.                                                      ELUKYTAB
00288 /***********************************************************      ELUKYTAB
00289 *                                                          *      ELUKYTAB
00290 *        BUILD SECTION TABLE                               *      ELUKYTAB
00291 *                                                          *      ELUKYTAB
00292 ************************************************************      ELUKYTAB
00293  BUILD-SECTION-TABLE.                                             ELUKYTAB
00294      PERFORM BUILD-SECTION-TABLE-BEGIN.                           ELUKYTAB
00295      PERFORM BUILD-SECTION-TABLE-PROCESS.                         ELUKYTAB
00296      PERFORM BUILD-SECTION-TABLE-END.                             ELUKYTAB
00297                                                                   ELUKYTAB
00298  BUILD-SECTION-TABLE-2.                                           ELUKYTAB
00299      PERFORM BUILD-SECTION-TABLE-BEGIN.                           ELUKYTAB
00300      PERFORM BUILD-SECTION-TABLE-PROCESS-2.                       ELUKYTAB
00301      PERFORM BUILD-SECTION-TABLE-END.                             ELUKYTAB
00302                                                                   ELUKYTAB
00303  BUILD-SECTION-TABLE-BEGIN.                                       ELUKYTAB
00304      PERFORM ALLOCATE-SECTION-TABLE.                              ELUKYTAB
00305      PERFORM INITIALIZE-SECTION-TABLE-COUNT.                      ELUKYTAB
00306                                                                   ELUKYTAB
00307  ALLOCATE-SECTION-TABLE.                                          ELUKYTAB
00308      SET CIA-ELSKTBS-DDN TO TRUE.                                 ELUKYTAB
00309      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYTAB
00310                            WS-DUMMY-PTR.                          ELUKYTAB
00311      MOVE CIA-MVO  TO  WS-MAX-KTS.                                ELUKYTAB
00312      COMPUTE CIA-AREA-LEN =   LENGTH OF KTS-NBR-KEYS              ELUKYTAB
00313                + ( WS-MAX-KTS * LENGTH OF KTS-KEY-TBL ).          ELUKYTAB
00314      PERFORM ACQUIRE-CONTROLLED-STORAGE.                          ELUKYTAB
00315      SET CIA-ELSKTBS-DDN TO TRUE.                                 ELUKYTAB
00316      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYTAB
00317                 ADDRESS OF KTS-SECTIONS-KEY-TABLE.                ELUKYTAB
00318                                                                   ELUKYTAB
00319  INITIALIZE-SECTION-TABLE-COUNT.                                  ELUKYTAB
00320      MOVE ZERO TO KTS-NBR-KEYS.                                   ELUKYTAB
00321      SET KTS-IDX TO KTS-NBR-KEYS.                                 ELUKYTAB
00322 /***********************************************************      ELUKYTAB
00323 *                                                          *      ELUKYTAB
00324 *        BUILD SECTION TABLE.PROCESS                       *      ELUKYTAB
00325 *                                                          *      ELUKYTAB
00326 ************************************************************      ELUKYTAB
00327  BUILD-SECTION-TABLE-PROCESS.                                     ELUKYTAB
00328      PERFORM START-SECTION-SCANS.                                 ELUKYTAB
00329      IF GRP-FOUND OR CONT-FOUND                                   ELUKYTAB
00330          PERFORM DO-FIRST-READS.                                  ELUKYTAB
00331      IF GRP-FOUND OR CONT-FOUND                                   ELUKYTAB
00332          PERFORM COMPLETE-GROUP-CONTRACT-MERGE.                   ELUKYTAB
00333                                                                   ELUKYTAB
00334  BUILD-SECTION-TABLE-PROCESS-2.                                   ELUKYTAB
00335      PERFORM START-SECTION-SCANS-2.                               ELUKYTAB
00336      IF GRP-FOUND OR CONT-FOUND                                   ELUKYTAB
00337          PERFORM DO-FIRST-READS-2.                                ELUKYTAB
00338      IF GRP-FOUND OR CONT-FOUND                                   ELUKYTAB
00339          PERFORM COMPLETE-GROUP-CONTRACT-MERGE2.                  ELUKYTAB
00340                                                                   ELUKYTAB
00341  START-SECTION-SCANS.                                             ELUKYTAB
00342      PERFORM START-GROUP-SPEC-BROWSE.                             ELUKYTAB
00343      IF IOP-RC-OK                                                 ELUKYTAB
00344          SET GRP-FOUND    TO TRUE                                 ELUKYTAB
00345          SET GRP-NOT-EOF  TO TRUE                                 ELUKYTAB
00346      ELSE                                                         ELUKYTAB
00347          IF IOP-RC-NOTFND                                         ELUKYTAB
00348              SET GRP-NOT-FOUND TO TRUE                            ELUKYTAB
00349              SET GRP-EOF       TO TRUE                            ELUKYTAB
00350          ELSE                                                     ELUKYTAB
00351              PERFORM SIGNAL-UNKNOWN-ERROR                         ELUKYTAB
00352          END-IF                                                   ELUKYTAB
00353      END-IF.                                                      ELUKYTAB
00354                                                                   ELUKYTAB
00355      PERFORM START-CONTRACT-BROWSE.                               ELUKYTAB
00356      IF IOP-RC-OK                                                 ELUKYTAB
00357          SET CONT-FOUND   TO TRUE                                 ELUKYTAB
00358          SET CONT-NOT-EOF TO TRUE                                 ELUKYTAB
00359      ELSE                                                         ELUKYTAB
00360          IF IOP-RC-NOTFND                                         ELUKYTAB
00361              SET CONT-NOT-FOUND TO TRUE                           ELUKYTAB
00362              SET CONT-EOF       TO TRUE                           ELUKYTAB
00363          ELSE                                                     ELUKYTAB
00364              PERFORM SIGNAL-UNKNOWN-ERROR                         ELUKYTAB
00365          END-IF                                                   ELUKYTAB
00366      END-IF.                                                      ELUKYTAB
00367                                                                   ELUKYTAB
00368  START-SECTION-SCANS-2.                                           ELUKYTAB
00369      PERFORM START-GROUP-SPEC-BROWSE-2.                           ELUKYTAB
00370      IF IOP-RC-OK                                                 ELUKYTAB
00371          SET GRP-FOUND    TO TRUE                                 ELUKYTAB
00372          SET GRP-NOT-EOF  TO TRUE                                 ELUKYTAB
00373      ELSE                                                         ELUKYTAB
00374          IF IOP-RC-NOTFND                                         ELUKYTAB
00375              SET GRP-NOT-FOUND TO TRUE                            ELUKYTAB
00376              SET GRP-EOF       TO TRUE                            ELUKYTAB
00377          ELSE                                                     ELUKYTAB
00378              PERFORM SIGNAL-UNKNOWN-ERROR                         ELUKYTAB
00379          END-IF                                                   ELUKYTAB
00380      END-IF.                                                      ELUKYTAB
00381                                                                   ELUKYTAB
00382      IF GRP-FOUND                                                 ELUKYTAB
00383         PERFORM SET-ADDRESS-OF-GCGRPSPC                           ELUKYTAB
00384         PERFORM END-BROWSE-OF-GROUP-SPECIFIC                      ELUKYTAB
00385         PERFORM START-GROUP-SPEC-BROWSE-2                         ELUKYTAB
00386           IF IOP-RC-OK                                            ELUKYTAB
00387              SET SECTION-FOUND    TO TRUE                         ELUKYTAB
00388              SET GRP-NOT-EOF      TO TRUE                         ELUKYTAB
00389           ELSE                                                    ELUKYTAB
00390             IF IOP-RC-NOTFND                                      ELUKYTAB
00391                SET SECTION-NOT-FOUND TO TRUE                      ELUKYTAB
00392                SET GRP-NOT-FOUND TO TRUE                          ELUKYTAB
00393                SET GRP-EOF       TO TRUE                          ELUKYTAB
00394             ELSE                                                  ELUKYTAB
00395                  PERFORM SIGNAL-UNKNOWN-ERROR                     ELUKYTAB
00396             END-IF                                                ELUKYTAB
00397           END-IF                                                  ELUKYTAB
00398      END-IF.                                                      ELUKYTAB
00399                                                                   ELUKYTAB
00400      PERFORM START-CONTRACT-BROWSE.                               ELUKYTAB
00401      IF IOP-RC-OK                                                 ELUKYTAB
00402          SET CONT-FOUND   TO TRUE                                 ELUKYTAB
00403          SET CONT-NOT-EOF TO TRUE                                 ELUKYTAB
00404      ELSE                                                         ELUKYTAB
00405          IF IOP-RC-NOTFND                                         ELUKYTAB
00406              SET CONT-NOT-FOUND TO TRUE                           ELUKYTAB
00407              SET CONT-EOF       TO TRUE                           ELUKYTAB
00408          ELSE                                                     ELUKYTAB
00409              PERFORM SIGNAL-UNKNOWN-ERROR                         ELUKYTAB
00410          END-IF                                                   ELUKYTAB
00411      END-IF.                                                      ELUKYTAB
00412                                                                   ELUKYTAB
00413      IF CONT-FOUND                                                ELUKYTAB
00414         PERFORM SET-IOPM-ADDRESS-FOR-CONTRACT                     ELUKYTAB
00415         PERFORM END-BROWSE-OF-CONTRACT-FILE                       ELUKYTAB
00416         PERFORM START-CONTRACT-BROWSE-2                           ELUKYTAB
00417           IF IOP-RC-OK                                            ELUKYTAB
00418              SET SECTION-FOUND   TO TRUE                          ELUKYTAB
00419              SET CONT-NOT-EOF    TO TRUE                          ELUKYTAB
00420           ELSE                                                    ELUKYTAB
00421              IF IOP-RC-NOTFND                                     ELUKYTAB
00422                 SET SECTION-NOT-FOUND TO TRUE                     ELUKYTAB
00423                 SET CONT-NOT-FOUND    TO TRUE                     ELUKYTAB
00424                 SET CONT-EOF          TO TRUE                     ELUKYTAB
00425              ELSE                                                 ELUKYTAB
00426                 PERFORM SIGNAL-UNKNOWN-ERROR                      ELUKYTAB
00427              END-IF                                               ELUKYTAB
00428           END-IF                                                  ELUKYTAB
00429      END-IF.                                                      ELUKYTAB
00430 /***********************************************************      ELUKYTAB
00431 *                                                          *      ELUKYTAB
00432 *        DO FIRST READS                                    *      ELUKYTAB
00433 *                                                          *      ELUKYTAB
00434 ************************************************************      ELUKYTAB
00435  DO-FIRST-READS.                                                  ELUKYTAB
00436      IF GRP-FOUND                                                 ELUKYTAB
00437          PERFORM SET-ADDRESS-OF-GCGRPSPC                          ELUKYTAB
00438          PERFORM READ-NEXT-GROUP-SPEC-REC                         ELUKYTAB
00439 *        SET ADDRESS OF 000-MEMBER-HEADER TO                      ELUKYTAB
00440 *                        GMFX-RECORD-POINTERS(1)                  ELUKYTAB
00441          IF IOP-RC-OK AND GCG-GROUP-NUM = SSB-GROUP-NUMBER        ELUKYTAB
00442                SET GRP-NOT-EOF TO TRUE                            ELUKYTAB
00443          ELSE                                                     ELUKYTAB
00444                SET GRP-EOF     TO TRUE.                           ELUKYTAB
00445      IF CONT-FOUND                                                ELUKYTAB
00446          PERFORM SET-IOPM-ADDRESS-FOR-CONTRACT                    ELUKYTAB
00447          PERFORM READ-NEXT-CONTRACT-RECORD                        ELUKYTAB
00448          IF IOP-RC-OK AND GCT-GROUP-NUM = SSB-GROUP-NUMBER        ELUKYTAB
00449                SET CONT-NOT-EOF TO TRUE                           ELUKYTAB
00450          ELSE                                                     ELUKYTAB
00451                SET CONT-EOF     TO TRUE.                          ELUKYTAB
00452                                                                   ELUKYTAB
00453  DO-FIRST-READS-2.                                                ELUKYTAB
00454      IF GRP-FOUND                                                 ELUKYTAB
00455          PERFORM SET-ADDRESS-OF-GCGRPSPC                          ELUKYTAB
00456          PERFORM READ-NEXT-GROUP-SPEC-REC                         ELUKYTAB
00457 *        SET ADDRESS OF 000-MEMBER-HEADER TO                      ELUKYTAB
00458 *                        GMFX-RECORD-POINTERS(1)                  ELUKYTAB
00459          IF IOP-RC-OK AND GCG-GROUP-NUM = SSB-GROUP-NUMBER AND    ELUKYTAB
00460                GCG-SECTN-NO    = SSB-SECT-NO                      ELUKYTAB
00461                SET GRP-NOT-EOF TO TRUE                            ELUKYTAB
00462          ELSE                                                     ELUKYTAB
00463                SET GRP-EOF     TO TRUE.                           ELUKYTAB
00464      IF CONT-FOUND                                                ELUKYTAB
00465          PERFORM SET-IOPM-ADDRESS-FOR-CONTRACT                    ELUKYTAB
00466          PERFORM READ-NEXT-CONTRACT-RECORD                        ELUKYTAB
00467          IF IOP-RC-OK AND GCT-GROUP-NUM = SSB-GROUP-NUMBER AND    ELUKYTAB
00468                GCT-SECTN-NO    = SSB-SECT-NO                      ELUKYTAB
00469                SET CONT-NOT-EOF TO TRUE                           ELUKYTAB
00470          ELSE                                                     ELUKYTAB
00471                SET CONT-EOF     TO TRUE.                          ELUKYTAB
00472 /***********************************************************      ELUKYTAB
00473 *                                                          *      ELUKYTAB
00474 *        START GROUP SPEC BROWSE                           *      ELUKYTAB
00475 *                                                          *      ELUKYTAB
00476 ************************************************************      ELUKYTAB
00477  START-GROUP-SPEC-BROWSE.                                         ELUKYTAB
00478      PERFORM ESTAB-ADDR-OF-GCGRPSPC-BLOCK.                        ELUKYTAB
00479      PERFORM BUILD-GROUP-BROWSE-KEY-FOR-SEC.                      ELUKYTAB
00480      PERFORM START-GROUP-BROWSE.                                  ELUKYTAB
00481                                                                   ELUKYTAB
00482  START-GROUP-SPEC-BROWSE-2.                                       ELUKYTAB
00483      PERFORM ESTAB-ADDR-OF-GCGRPSPC-BLOCK.                        ELUKYTAB
00484      PERFORM BUILD-GRP-BROWSE-KEY-FOR-SEC-2.                      ELUKYTAB
00485      PERFORM START-GROUP-BROWSE.                                  ELUKYTAB
00486                                                                   ELUKYTAB
00487                                                                   ELUKYTAB
00488 ************************************************************      ELUKYTAB
00489 *                                                          *      ELUKYTAB
00490 *        START CONTRACT BROWSE                             *      ELUKYTAB
00491 *                                                          *      ELUKYTAB
00492 ************************************************************      ELUKYTAB
00493  START-CONTRACT-BROWSE.                                           ELUKYTAB
00494      PERFORM ESTAB-ADDR-OF-GCCONTR-BLOCK.                         ELUKYTAB
00495      PERFORM BUILD-CONTRACT-BROWSE-KEY-FORX.                      ELUKYTAB
00496      PERFORM START-BROWSE-OF-CONTRACT-FILE.                       ELUKYTAB
00497                                                                   ELUKYTAB
00498  START-CONTRACT-BROWSE-2.                                         ELUKYTAB
00499      PERFORM ESTAB-ADDR-OF-GCCONTR-BLOCK.                         ELUKYTAB
00500      PERFORM BUILD-CONT-BROWSE-KEY-FORX-2.                        ELUKYTAB
00501      PERFORM START-BROWSE-OF-CONTRACT-FILE.                       ELUKYTAB
00502                                                                   ELUKYTAB
00503                                                                   ELUKYTAB
00504 ************************************************************      ELUKYTAB
00505 *                                                          *      ELUKYTAB
00506 *        SET ADDRESS OF GCGRPSPC                           *      ELUKYTAB
00507 *                                                          *      ELUKYTAB
00508 ************************************************************      ELUKYTAB
00509  SET-ADDRESS-OF-GCGRPSPC.                                         ELUKYTAB
00510      SET CIA-GCGRPSPC-DDN TO TRUE.                                ELUKYTAB
00511      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYTAB
00512                 ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.           ELUKYTAB
00513      EJECT                                                        ELUKYTAB
00514                                                                   ELUKYTAB
00515                                                                   ELUKYTAB
00516 ************************************************************      ELUKYTAB
00517 *                                                          *      ELUKYTAB
00518 *        SET IOPM ADDRESS FOR CONTRACT                     *      ELUKYTAB
00519 *                                                          *      ELUKYTAB
00520 ************************************************************      ELUKYTAB
00521  SET-IOPM-ADDRESS-FOR-CONTRACT.                                   ELUKYTAB
00522      SET CIA-GCCONTR-DDN TO TRUE.                                 ELUKYTAB
00523      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYTAB
00524                 ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.           ELUKYTAB
00525                                                                   ELUKYTAB
00526                                                                   ELUKYTAB
00527 ************************************************************      ELUKYTAB
00528 *                                                          *      ELUKYTAB
00529 *        BUILD GROUP BROWSE KEY FOR SECTIONS               *      ELUKYTAB
00530 *                                                          *      ELUKYTAB
00531 ************************************************************      ELUKYTAB
00532  BUILD-GROUP-BROWSE-KEY-FOR-SEC.                                  ELUKYTAB
00533      INITIALIZE KWA-GCGRPSPC-KEY.                                 ELUKYTAB
00534      MOVE ZERO TO KWA-GCG-PLAN-CODE.                              ELUKYTAB
00535      MOVE SSB-GROUP-NUMBER TO KWA-GCG-GROUP-NUMBER.               ELUKYTAB
00536      MOVE LOW-VALUES TO KWA-GCG-SECTION-NUMBER,                   ELUKYTAB
00537        KWA-GCG-FAM-REL-LVL, KWA-GCG-PKG-CODE.                     ELUKYTAB
00538      MOVE KWA-FILE-KEY TO IOP-FILE-KEY.                           ELUKYTAB
00539      COMPUTE WS-KEY-LEN  = LENGTH OF GCG-GROUP-NUM +              ELUKYTAB
00540                     LENGTH OF GCG-PLAN-CODE.                      ELUKYTAB
00541      MOVE WS-KEY-LEN TO IOP-KEY-LEN.                              ELUKYTAB
00542 *    MOVE LENGTH OF GCG-GROUP-NUM TO IOP-KEY-LEN.                 ELUKYTAB
00543      EJECT                                                        ELUKYTAB
00544                                                                   ELUKYTAB
00545                                                                   ELUKYTAB
00546  BUILD-GRP-BROWSE-KEY-FOR-SEC-2.                                  ELUKYTAB
00547      INITIALIZE KWA-GCGRPSPC-KEY.                                 ELUKYTAB
00548      MOVE ZEROES TO KWA-GCG-PLAN-CODE, KWA-GCG-SECTN-1,           ELUKYTAB
00549                   SSB-SECT-NO-1                                   ELUKYTAB
00550      MOVE SSB-GROUP-NUMBER TO KWA-GCG-GROUP-NUMBER.               ELUKYTAB
00551      MOVE SSB-SECT-NO  TO  KWA-GCG-SECTN                          ELUKYTAB
00552      MOVE LOW-VALUES TO                                           ELUKYTAB
00553        KWA-GCG-FAM-REL-LVL, KWA-GCG-PKG-CODE.                     ELUKYTAB
00554      MOVE KWA-FILE-KEY TO IOP-FILE-KEY.                           ELUKYTAB
00555      COMPUTE WS-KEY-LEN  = LENGTH OF GCG-GROUP-NUM +              ELUKYTAB
00556           LENGTH OF GCG-PLAN-CODE + LENGTH OF GCG-SECTION-NUM     ELUKYTAB
00557      MOVE WS-KEY-LEN TO IOP-KEY-LEN.                              ELUKYTAB
00558 *    MOVE LENGTH OF GCG-GROUP-NUM TO IOP-KEY-LEN.                 ELUKYTAB
00559      EJECT                                                        ELUKYTAB
00560                                                                   ELUKYTAB
00561 ************************************************************      ELUKYTAB
00562 *                                                          *      ELUKYTAB
00563 *        BUILD CONTRACT BROWSE KEY FOR SECTIONS            *      ELUKYTAB
00564 *                                                          *      ELUKYTAB
00565 ************************************************************      ELUKYTAB
00566  BUILD-CONTRACT-BROWSE-KEY-FORX.                                  ELUKYTAB
00567      INITIALIZE KWA-GCCONTR-KEY.                                  ELUKYTAB
00568      MOVE ZERO TO KWA-GCT-PLAN-CODE.                              ELUKYTAB
00569      MOVE SSB-GROUP-NUMBER TO KWA-GCT-GROUP-NUMBER.               ELUKYTAB
00570      MOVE LOW-VALUES TO KWA-GCT-SECTION-NUMBER,                   ELUKYTAB
00571            KWA-GCT-PKG-CODE, KWA-GCT-FAM-REL-LVL.                 ELUKYTAB
00572      MOVE KWA-FILE-KEY TO IOP-FILE-KEY.                           ELUKYTAB
00573      COMPUTE WS-KEY-LEN  = LENGTH OF GCT-GROUP-NUM +              ELUKYTAB
00574                     LENGTH OF GCT-PLAN-CODE.                      ELUKYTAB
00575      MOVE WS-KEY-LEN TO IOP-KEY-LEN.                              ELUKYTAB
00576 *    MOVE LENGTH OF GCT-GROUP-NUM TO IOP-KEY-LEN.                 ELUKYTAB
00577                                                                   ELUKYTAB
00578  BUILD-CONT-BROWSE-KEY-FORX-2.                                    ELUKYTAB
00579      INITIALIZE KWA-GCCONTR-KEY.                                  ELUKYTAB
00580      MOVE ZEROES TO KWA-GCT-PLAN-CODE, KWA-GCT-SECT-NO-1,         ELUKYTAB
00581                   SSB-SECT-NO-1.                                  ELUKYTAB
00582      MOVE SSB-GROUP-NUMBER TO KWA-GCT-GROUP-NUMBER.               ELUKYTAB
00583      MOVE SSB-SECT-NO     TO KWA-GCT-SECTN-NO                     ELUKYTAB
00584      MOVE LOW-VALUES TO                                           ELUKYTAB
00585            KWA-GCT-PKG-CODE, KWA-GCT-FAM-REL-LVL.                 ELUKYTAB
00586      MOVE KWA-FILE-KEY TO IOP-FILE-KEY.                           ELUKYTAB
00587      COMPUTE WS-KEY-LEN  = LENGTH OF GCT-GROUP-NUM +              ELUKYTAB
00588            LENGTH OF GCT-PLAN-CODE + LENGTH OF GCT-SECTION-NUM    ELUKYTAB
00589      MOVE WS-KEY-LEN TO IOP-KEY-LEN.                              ELUKYTAB
00590 *    MOVE LENGTH OF GCT-GROUP-NUM TO IOP-KEY-LEN.                 ELUKYTAB
00591 /***********************************************************      ELUKYTAB
00592 *                                                          *      ELUKYTAB
00593 *        COMPLETE GROUP CONTRACT MERGE                     *      ELUKYTAB
00594 *                                                          *      ELUKYTAB
00595 ************************************************************      ELUKYTAB
00596  COMPLETE-GROUP-CONTRACT-MERGE.                                   ELUKYTAB
00597      PERFORM DO-CONTRACT-AND-GROUP-SCANS                          ELUKYTAB
00598          UNTIL (GRP-EOF AND CONT-EOF) OR KTS-NBR-KEYS > 500.      ELUKYTAB
00599      IF KTS-NBR-KEYS > 500                                        ELUKYTAB
00600         SET CIA-AB-INCR-TBL-SIZE TO TRUE                          ELUKYTAB
00601      END-IF.                                                      ELUKYTAB
00602      PERFORM DO-END-BROWSES.                                      ELUKYTAB
00603                                                                   ELUKYTAB
00604  COMPLETE-GROUP-CONTRACT-MERGE2.                                  ELUKYTAB
00605      PERFORM DO-CONTRACT-AND-GROUP-SCANS-2                        ELUKYTAB
00606          UNTIL GRP-EOF AND CONT-EOF.                              ELUKYTAB
00607      PERFORM DO-END-BROWSES.                                      ELUKYTAB
00608                                                                   ELUKYTAB
00609  DO-CONTRACT-AND-GROUP-SCANS.                                     ELUKYTAB
00610      EVALUATE TRUE                                                ELUKYTAB
00611        WHEN GRP-EOF                                               ELUKYTAB
00612              PERFORM ACCEPT-CONTRACT-SECTION                      ELUKYTAB
00613        WHEN CONT-EOF                                              ELUKYTAB
00614              PERFORM ACCECPT-GROUP-SECTION                        ELUKYTAB
00615        WHEN GCG-SECTION-NUM LESS THAN GCT-SECTION-NUM             ELUKYTAB
00616              PERFORM ACCECPT-GROUP-SECTION                        ELUKYTAB
00617        WHEN GCG-SECTION-NUM EQUAL GCT-SECTION-NUM                 ELUKYTAB
00618              PERFORM ACCEPT-A-MATCH                               ELUKYTAB
00619        WHEN GCG-SECTION-NUM GREATER THAN GCT-SECTION-NUM          ELUKYTAB
00620              PERFORM ACCEPT-CONTRACT-SECTION                      ELUKYTAB
00621      END-EVALUATE.                                                ELUKYTAB
00622                                                                   ELUKYTAB
00623  DO-CONTRACT-AND-GROUP-SCANS-2.                                   ELUKYTAB
00624      EVALUATE TRUE                                                ELUKYTAB
00625        WHEN GRP-EOF                                               ELUKYTAB
00626              PERFORM ACCEPT-CONTRACT-SECTION-2                    ELUKYTAB
00627        WHEN CONT-EOF                                              ELUKYTAB
00628              PERFORM ACCECPT-GROUP-SECTION-2                      ELUKYTAB
00629        WHEN GCG-SECTION-NUM LESS THAN GCT-SECTION-NUM             ELUKYTAB
00630              PERFORM ACCECPT-GROUP-SECTION-2                      ELUKYTAB
00631        WHEN GCG-SECTION-NUM EQUAL GCT-SECTION-NUM                 ELUKYTAB
00632              PERFORM ACCEPT-A-MATCH-2                             ELUKYTAB
00633        WHEN GCG-SECTION-NUM GREATER THAN GCT-SECTION-NUM          ELUKYTAB
00634              PERFORM ACCEPT-CONTRACT-SECTION-2                    ELUKYTAB
00635      END-EVALUATE.                                                ELUKYTAB
00636                                                                   ELUKYTAB
00637  ACCEPT-CONTRACT-SECTION.                                         ELUKYTAB
00638      PERFORM BUILD-KTS-FROM-CONTRACT.                             ELUKYTAB
00639      PERFORM FIND-NEXT-CONTRACT-SECTION.                          ELUKYTAB
00640                                                                   ELUKYTAB
00641  ACCEPT-CONTRACT-SECTION-2.                                       ELUKYTAB
00642      PERFORM BUILD-KTS-FROM-CONTRACT.                             ELUKYTAB
00643      PERFORM FIND-NEXT-CONTRACT-SECTION-2.                        ELUKYTAB
00644                                                                   ELUKYTAB
00645  ACCECPT-GROUP-SECTION.                                           ELUKYTAB
00646      PERFORM BUILD-KTS-FROM-GROUP.                                ELUKYTAB
00647      PERFORM FIND-NEXT-GROUP-SECTION.                             ELUKYTAB
00648                                                                   ELUKYTAB
00649  ACCECPT-GROUP-SECTION-2.                                         ELUKYTAB
00650      PERFORM BUILD-KTS-FROM-GROUP.                                ELUKYTAB
00651      PERFORM FIND-NEXT-GROUP-SECTION-2.                           ELUKYTAB
00652                                                                   ELUKYTAB
00653  ACCEPT-A-MATCH.                                                  ELUKYTAB
00654      PERFORM BUILD-KTS-FROM-GROUP.                                ELUKYTAB
00655      PERFORM FIND-NEXT-GROUP-SECTION.                             ELUKYTAB
00656      PERFORM FIND-NEXT-CONTRACT-SECTION.                          ELUKYTAB
00657                                                                   ELUKYTAB
00658  ACCEPT-A-MATCH-2.                                                ELUKYTAB
00659      PERFORM BUILD-KTS-FROM-GROUP.                                ELUKYTAB
00660      PERFORM FIND-NEXT-GROUP-SECTION-2.                           ELUKYTAB
00661      PERFORM FIND-NEXT-CONTRACT-SECTION-2.                        ELUKYTAB
00662                                                                   ELUKYTAB
00663  BUILD-KTS-FROM-GROUP.                                            ELUKYTAB
00664      SET KTS-IDX UP BY 1.                                         ELUKYTAB
00665      ADD 1 TO KTS-NBR-KEYS.                                       ELUKYTAB
00666      IF KTS-NBR-KEYS > WS-MAX-KTS                                 ELUKYTAB
00667          SET CIA-ELSKTBS-DDN TO TRUE                              ELUKYTAB
00668          PERFORM SIGNAL-TABLE-OVERFLOW                            ELUKYTAB
00669      ELSE                                                         ELUKYTAB
00670          MOVE GCG-SECTION-NUM TO KTS-SECTION-NUMBER (KTS-IDX)     ELUKYTAB
00671          MOVE GCG-PKG-CODE TO KTS-PKG-CODE (KTS-IDX)              ELUKYTAB
00672      END-IF.                                                      ELUKYTAB
00673                                                                   ELUKYTAB
00674  BUILD-KTS-FROM-CONTRACT.                                         ELUKYTAB
00675      SET KTS-IDX UP BY 1.                                         ELUKYTAB
00676      ADD 1 TO KTS-NBR-KEYS.                                       ELUKYTAB
00677      IF KTS-NBR-KEYS > WS-MAX-KTS                                 ELUKYTAB
00678          SET CIA-ELSKTBS-DDN TO TRUE                              ELUKYTAB
00679          PERFORM SIGNAL-TABLE-OVERFLOW                            ELUKYTAB
00680      ELSE                                                         ELUKYTAB
00681          MOVE GCT-SECTION-NUM TO KTS-SECTION-NUMBER (KTS-IDX)     ELUKYTAB
00682          MOVE GCT-PKG-CODE    TO KTS-PKG-CODE (KTS-IDX)           ELUKYTAB
00683      END-IF.                                                      ELUKYTAB
00684 /***********************************************************      ELUKYTAB
00685 *                                                          *      ELUKYTAB
00686 *        FIND NEXT GROUP SECTION                           *      ELUKYTAB
00687 *                                                          *      ELUKYTAB
00688 ************************************************************      ELUKYTAB
00689  FIND-NEXT-GROUP-SECTION.                                         ELUKYTAB
00690      PERFORM SET-ADDRESS-OF-GCGRPSPC.                             ELUKYTAB
00691      PERFORM WITH TEST AFTER                                      ELUKYTAB
00692              UNTIL GRP-EOF OR TERMINATE-LOOP                      ELUKYTAB
00693          PERFORM READ-NEXT-GROUP-SPEC-REC                         ELUKYTAB
00694          IF GCG-SECTION-NUM = KTS-SECTION-NUMBER(KTS-IDX) AND     ELUKYTAB
00695             GCG-GROUP-NUM = SSB-GROUP-NUMBER       AND            ELUKYTAB
00696             GCG-PKG-CODE = KTS-PKG-CODE(KTS-IDX)  AND             ELUKYTAB
00697             IOP-RC-OK                                             ELUKYTAB
00698                 SET CONTINUE-LOOP TO TRUE                         ELUKYTAB
00699          ELSE                                                     ELUKYTAB
00700                 SET TERMINATE-LOOP TO TRUE                        ELUKYTAB
00701          END-IF                                                   ELUKYTAB
00702          IF IOP-RC-ENDFILE      OR                                ELUKYTAB
00703             GCG-GROUP-NUM NOT = SSB-GROUP-NUMBER                  ELUKYTAB
00704              SET GRP-EOF TO TRUE                                  ELUKYTAB
00705          END-IF                                                   ELUKYTAB
00706      END-PERFORM.                                                 ELUKYTAB
00707                                                                   ELUKYTAB
00708  FIND-NEXT-GROUP-SECTION-2.                                       ELUKYTAB
00709      PERFORM SET-ADDRESS-OF-GCGRPSPC.                             ELUKYTAB
00710      PERFORM WITH TEST AFTER                                      ELUKYTAB
00711              UNTIL GRP-EOF OR TERMINATE-LOOP                      ELUKYTAB
00712          PERFORM READ-NEXT-GROUP-SPEC-REC                         ELUKYTAB
00713          IF GCG-SECTION-NUM = SSB-SECTN-NO        AND             ELUKYTAB
00714             GCG-GROUP-NUM = SSB-GROUP-NUMBER      AND             ELUKYTAB
00715             GCG-PKG-CODE = KTS-PKG-CODE(KTS-IDX)  AND             ELUKYTAB
00716             IOP-RC-OK                                             ELUKYTAB
00717                 SET CONTINUE-LOOP TO TRUE                         ELUKYTAB
00718          ELSE                                                     ELUKYTAB
00719                 SET TERMINATE-LOOP TO TRUE                        ELUKYTAB
00720          END-IF                                                   ELUKYTAB
00721          IF IOP-RC-ENDFILE      OR                                ELUKYTAB
00722             GCG-GROUP-NUM NOT = SSB-GROUP-NUMBER   OR             ELUKYTAB
00723             GCG-SECTION-NUM  NOT  = SSB-SECTN-NO                  ELUKYTAB
00724             SET GRP-EOF TO TRUE                                   ELUKYTAB
00725          END-IF                                                   ELUKYTAB
00726      END-PERFORM.                                                 ELUKYTAB
00727                                                                   ELUKYTAB
00728 ************************************************************      ELUKYTAB
00729 *                                                          *      ELUKYTAB
00730 *        FIND NEXT CONTRACT SECTION                        *      ELUKYTAB
00731 *                                                          *      ELUKYTAB
00732 ************************************************************      ELUKYTAB
00733  FIND-NEXT-CONTRACT-SECTION.                                      ELUKYTAB
00734      PERFORM SET-IOPM-ADDRESS-FOR-CONTRACT.                       ELUKYTAB
00735      PERFORM WITH TEST AFTER                                      ELUKYTAB
00736              UNTIL CONT-EOF OR TERMINATE-LOOP                     ELUKYTAB
00737          PERFORM READ-NEXT-CONTRACT-RECORD                        ELUKYTAB
00738          IF GCT-SECTION-NUM  = KTS-SECTION-NUMBER (KTS-IDX) AND   ELUKYTAB
00739             GCT-GROUP-NUM = SSB-GROUP-NUMBER   AND                ELUKYTAB
00740             IOP-RC-OK                                             ELUKYTAB
00741                 SET CONTINUE-LOOP TO TRUE                         ELUKYTAB
00742          ELSE                                                     ELUKYTAB
00743                 SET TERMINATE-LOOP TO TRUE                        ELUKYTAB
00744          END-IF                                                   ELUKYTAB
00745          IF IOP-RC-ENDFILE    OR                                  ELUKYTAB
00746             GCT-GROUP-NUM NOT = SSB-GROUP-NUMBER                  ELUKYTAB
00747              SET CONT-EOF TO TRUE                                 ELUKYTAB
00748          END-IF                                                   ELUKYTAB
00749      END-PERFORM.                                                 ELUKYTAB
00750                                                                   ELUKYTAB
00751  FIND-NEXT-CONTRACT-SECTION-2.                                    ELUKYTAB
00752      PERFORM SET-IOPM-ADDRESS-FOR-CONTRACT.                       ELUKYTAB
00753      PERFORM WITH TEST AFTER                                      ELUKYTAB
00754              UNTIL CONT-EOF OR TERMINATE-LOOP                     ELUKYTAB
00755          PERFORM READ-NEXT-CONTRACT-RECORD                        ELUKYTAB
00756          IF GCT-SECTION-NUM  = SSB-SECTN-NO    AND                ELUKYTAB
00757             GCT-GROUP-NUM = SSB-GROUP-NUMBER   AND                ELUKYTAB
00758             GCT-PKG-CODE  = KTS-PKG-CODE (KTS-IDX) AND            ELUKYTAB
00759             IOP-RC-OK                                             ELUKYTAB
00760                 SET CONTINUE-LOOP TO TRUE                         ELUKYTAB
00761          ELSE                                                     ELUKYTAB
00762                 SET TERMINATE-LOOP TO TRUE                        ELUKYTAB
00763          END-IF                                                   ELUKYTAB
00764          IF IOP-RC-ENDFILE    OR                                  ELUKYTAB
00765             GCT-GROUP-NUM NOT = SSB-GROUP-NUMBER  OR              ELUKYTAB
00766             GCT-SECTION-NUM NOT = SSB-SECTN-NO                    ELUKYTAB
00767              SET CONT-EOF TO TRUE                                 ELUKYTAB
00768          END-IF                                                   ELUKYTAB
00769      END-PERFORM.                                                 ELUKYTAB
00770 /***********************************************************      ELUKYTAB
00771 *                                                          *      ELUKYTAB
00772 *        DO END BROWSES                                    *      ELUKYTAB
00773 *                                                          *      ELUKYTAB
00774 ************************************************************      ELUKYTAB
00775  DO-END-BROWSES.                                                  ELUKYTAB
00776      IF GRP-FOUND                                                 ELUKYTAB
00777          PERFORM END-GRP-SPECIFIC-BROWSE.                         ELUKYTAB
00778      IF CONT-FOUND                                                ELUKYTAB
00779          PERFORM END-CONT-BROWSE.                                 ELUKYTAB
00780                                                                   ELUKYTAB
00781  END-GRP-SPECIFIC-BROWSE.                                         ELUKYTAB
00782      PERFORM SET-ADDRESS-OF-GCGRPSPC.                             ELUKYTAB
00783      PERFORM END-BROWSE-OF-GROUP-SPECIFIC.                        ELUKYTAB
00784                                                                   ELUKYTAB
00785  END-CONT-BROWSE.                                                 ELUKYTAB
00786      PERFORM SET-IOPM-ADDRESS-FOR-CONTRACT.                       ELUKYTAB
00787      PERFORM END-BROWSE-OF-CONTRACT-FILE.                         ELUKYTAB
00788                                                                   ELUKYTAB
00789 ************************************************************      ELUKYTAB
00790 *                                                          *      ELUKYTAB
00791 *        BUILD SECTION TABLE.END                           *      ELUKYTAB
00792 *                                                          *      ELUKYTAB
00793 ************************************************************      ELUKYTAB
00794  BUILD-SECTION-TABLE-END.                                         ELUKYTAB
00795      IF KTS-NBR-KEYS IS EQUAL TO 1                                ELUKYTAB
00796          MOVE KTS-PKG-CODE (1) TO SSB-PKG-CODE                    ELUKYTAB
00797          MOVE KTS-SECTION-NUMBER (1) TO SSB-SECTN-NO.             ELUKYTAB
00798      SET  CIA-ELSKTBS-DDN TO TRUE.                                ELUKYTAB
00799      MOVE LENGTH OF KTS-SECTIONS-KEY-TABLE TO                     ELUKYTAB
00800          CIA-AREA-LEN.                                            ELUKYTAB
00801      PERFORM STOW-KEY-TABLE.                                      ELUKYTAB
00802 /***********************************************************      ELUKYTAB
00803 *                                                          *      ELUKYTAB
00804 *        BUILD GROUP SECTION KEY TABS                      *      ELUKYTAB
00805 *                                                          *      ELUKYTAB
00806 ************************************************************      ELUKYTAB
00807  BUILD-GROUP-SECTION-KEY-TABS.                                    ELUKYTAB
00808      PERFORM VERIFY-SECTION-EXISTS.                               ELUKYTAB
00809      IF SECTION-FOUND                                             ELUKYTAB
00810          PERFORM BUILD-KTG-AND-KTC-TABLES.                        ELUKYTAB
00811                                                                   ELUKYTAB
00812  VERIFY-SECTION-EXISTS.                                           ELUKYTAB
00813      SET CIA-ELSKTBS-DDN TO TRUE.                                 ELUKYTAB
00814      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYTAB
00815                            WS-DUMMY-PTR.                          ELUKYTAB
00816      IF NOT CIA-RC-OK                                             ELUKYTAB
00817          PERFORM RETRIEVE-GROUP-SECTION-TABLE.                    ELUKYTAB
00818      PERFORM LOCATE-SECTION-IN-KTS.                               ELUKYTAB
00819                                                                   ELUKYTAB
00820  RETRIEVE-GROUP-SECTION-TABLE.                                    ELUKYTAB
00821      SET CIA-ELSKTBS-DDN TO TRUE.                                 ELUKYTAB
00822      SET CIA-STG-RETRIEVE TO TRUE.                                ELUKYTAB
00823      PERFORM CALL-STORAGE-MANAGEMENT.                             ELUKYTAB
00824      SET CIA-ELSKTBS-DDN TO TRUE.                                 ELUKYTAB
00825      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYTAB
00826                 ADDRESS OF KTS-SECTIONS-KEY-TABLE.                ELUKYTAB
00827 /***********************************************************      ELUKYTAB
00828 *                                                          *      ELUKYTAB
00829 *        LOCATE SECTION IN KTS                             *      ELUKYTAB
00830 *                                                          *      ELUKYTAB
00831 ************************************************************      ELUKYTAB
00832  LOCATE-SECTION-IN-KTS.                                           ELUKYTAB
00833      SET KTS-IDX TO 1.                                            ELUKYTAB
00834      SEARCH KTS-KEY-TBL VARYING KTS-IDX                           ELUKYTAB
00835             AT END                                                ELUKYTAB
00836                 SET SECTION-NOT-FOUND TO TRUE                     ELUKYTAB
00837             WHEN SSB-SECTN-NO = KTS-SECTION-NUMBER (KTS-IDX)      ELUKYTAB
00838                 SET SECTION-FOUND TO TRUE                         ELUKYTAB
00839             WHEN KTS-SECTION-NUMBER (KTS-IDX) > SSB-SECTN-NO      ELUKYTAB
00840                 SET SECTION-NOT-FOUND TO TRUE.                    ELUKYTAB
00841                                                                   ELUKYTAB
00842 ************************************************************      ELUKYTAB
00843 *                                                          *      ELUKYTAB
00844 *        BUILD KTG AND KTC TABLES                          *      ELUKYTAB
00845 *                                                          *      ELUKYTAB
00846 ************************************************************      ELUKYTAB
00847  BUILD-KTG-AND-KTC-TABLES.                                        ELUKYTAB
00848      PERFORM BUILD-KTG-TABLE.                                     ELUKYTAB
00849      PERFORM BUILD-KTC-TABLE.                                     ELUKYTAB
00850                                                                   ELUKYTAB
00851                                                                   ELUKYTAB
00852 ************************************************************      ELUKYTAB
00853 *                                                          *      ELUKYTAB
00854 *        BUILD KTG TABLE                                   *      ELUKYTAB
00855 *                                                          *      ELUKYTAB
00856 ************************************************************      ELUKYTAB
00857  BUILD-KTG-TABLE.                                                 ELUKYTAB
00858      PERFORM BUILD-KTG-TABLE-BEGIN.                               ELUKYTAB
00859      PERFORM BUILD-KTG-TABLE-PROCESS.                             ELUKYTAB
00860      PERFORM BUILD-KTG-TABLE-END.                                 ELUKYTAB
00861                                                                   ELUKYTAB
00862                                                                   ELUKYTAB
00863 ************************************************************      ELUKYTAB
00864 *                                                          *      ELUKYTAB
00865 *        BUILD KTG TABLE.BEGIN                             *      ELUKYTAB
00866 *                                                          *      ELUKYTAB
00867 ************************************************************      ELUKYTAB
00868  BUILD-KTG-TABLE-BEGIN.                                           ELUKYTAB
00869      PERFORM ESTABLISH-ADDRESS-OF-ELSKTBGC.                       ELUKYTAB
00870      PERFORM INITIALIZE-KTG-TABLE.                                ELUKYTAB
00871                                                                   ELUKYTAB
00872 /***********************************************************      ELUKYTAB
00873 *                                                          *      ELUKYTAB
00874 *        ESTABLISH ADDRESS OF KTB                          *      ELUKYTAB
00875 *                                                          *      ELUKYTAB
00876 ************************************************************      ELUKYTAB
00877  ESTABLISH-ADDRESS-OF-ELSKTBGC.                                   ELUKYTAB
00878      SET CIA-ELSKTBG-DDN TO TRUE.                                 ELUKYTAB
00879      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYTAB
00880                            WS-DUMMY-PTR.                          ELUKYTAB
00881      IF NOT CIA-RC-OK                                             ELUKYTAB
00882          PERFORM ALLOCATE-ELSKTBGC.                               ELUKYTAB
00883                                                                   ELUKYTAB
00884 /***********************************************************      ELUKYTAB
00885 *                                                          *      ELUKYTAB
00886 *        ALLOCATE ELSKTBGC                                 *      ELUKYTAB
00887 *                                                          *      ELUKYTAB
00888 ************************************************************      ELUKYTAB
00889  ALLOCATE-ELSKTBGC.                                               ELUKYTAB
00890      MOVE CIA-MVO  TO  WS-MAX-KTG.                                ELUKYTAB
00891      COMPUTE CIA-AREA-LEN =   LENGTH OF KTG-NBR-KEYS              ELUKYTAB
00892           + LENGTH OF KTG-ALLIANCE-IND                            ELUKYTAB
00893          + ( WS-MAX-KTG * LENGTH OF KTG-KEY-TBL ).                ELUKYTAB
00894      PERFORM ACQUIRE-CONTROLLED-STORAGE.                          ELUKYTAB
00895      SET CIA-ELSKTBG-DDN TO TRUE.                                 ELUKYTAB
00896      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYTAB
00897                 ADDRESS OF KTG-GCGRPSPC-KEY-TABLE.                ELUKYTAB
00898                                                                   ELUKYTAB
00899 /***********************************************************      ELUKYTAB
00900 *                                                          *      ELUKYTAB
00901 *        ALLOCATE KTG TABLE                                *      ELUKYTAB
00902 *                                                          *      ELUKYTAB
00903 ************************************************************      ELUKYTAB
00904 *ALLOCATE-KTG-TABLE.                                              ELUKYTAB
00905 *    SET CIA-ELSKTBG-DDN TO TRUE.                                 ELUKYTAB
00906 *    CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYTAB
00907 *                          WS-DUMMY-PTR.                          ELUKYTAB
00908 *    MOVE CIA-MVO  TO  WS-MAX-KTG.                                ELUKYTAB
00909 *    COMPUTE CIA-AREA-LEN =   LENGTH OF KTG-NBR-KEYS              ELUKYTAB
00910 *         + LENGTH OF KTG-ALLIANCE-IND                            ELUKYTAB
00911 *        + ( WS-MAX-KTG * LENGTH OF KTG-KEY-TBL ).                ELUKYTAB
00912 *    PERFORM ACQUIRE-CONTROLLED-STORAGE.                          ELUKYTAB
00913 *    SET CIA-ELSKTBG-DDN TO TRUE.                                 ELUKYTAB
00914 *    CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYTAB
00915 *               ADDRESS OF KTG-GCGRPSPC-KEY-TABLE.                ELUKYTAB
00916                                                                   ELUKYTAB
00917  INITIALIZE-KTG-TABLE.                                            ELUKYTAB
00918      MOVE ZERO TO KTG-NBR-KEYS.                                   ELUKYTAB
00919      SET KTG-IDX TO KTG-NBR-KEYS.                                 ELUKYTAB
00920 /***********************************************************      ELUKYTAB
00921 *                                                          *      ELUKYTAB
00922 *        BUILD KTG TABLE.PROCESS                           *      ELUKYTAB
00923 *                                                          *      ELUKYTAB
00924 ************************************************************      ELUKYTAB
00925  BUILD-KTG-TABLE-PROCESS.                                         ELUKYTAB
00926      PERFORM START-SCAN-OF-GROUP-SPECIFIC.                        ELUKYTAB
00927      PERFORM COMPLETE-KTG-TABLE.                                  ELUKYTAB
00928                                                                   ELUKYTAB
00929  START-SCAN-OF-GROUP-SPECIFIC.                                    ELUKYTAB
00930      SET CIA-RC-OK TO TRUE.                                       ELUKYTAB
00931      PERFORM START-GROUP-BROWSE-FOR-KEYS.                         ELUKYTAB
00932      IF IOP-RC-OK                                                 ELUKYTAB
00933          SET GRP-FOUND TO TRUE                                    ELUKYTAB
00934          PERFORM READ-NEXT-GROUP-SPEC-REC                         ELUKYTAB
00935      ELSE IF IOP-RC-NOTFND                                        ELUKYTAB
00936          PERFORM SIGNAL-NO-GROUP-SECTION                          ELUKYTAB
00937      ELSE                                                         ELUKYTAB
00938          PERFORM SIGNAL-UNKNOWN-ERROR.                            ELUKYTAB
00939                                                                   ELUKYTAB
00940  START-GROUP-BROWSE-FOR-KEYS.                                     ELUKYTAB
00941      PERFORM ESTAB-ADDR-OF-GCGRPSPC-BLOCK.                        ELUKYTAB
00942      PERFORM BUILD-GROUP-BROWSE-KEY.                              ELUKYTAB
00943      PERFORM START-GROUP-BROWSE.                                  ELUKYTAB
00944                                                                   ELUKYTAB
00945  BUILD-GROUP-BROWSE-KEY.                                          ELUKYTAB
00946      INITIALIZE KWA-GCGRPSPC-KEY.                                 ELUKYTAB
00947      MOVE SSB-PLAN-CODE    TO KWA-GCG-PLAN-CODE.                  ELUKYTAB
00948      MOVE SSB-GROUP-NUMBER TO KWA-GCG-GROUP-NUMBER.               ELUKYTAB
00949      MOVE SSB-SECTN-NO     TO KWA-GCG-SECTION-NUMBER.             ELUKYTAB
00950      MOVE LOW-VALUES TO KWA-GCG-FAM-REL-LVL,                      ELUKYTAB
00951           KWA-GCG-PKG-CODE.                                       ELUKYTAB
00952      MOVE KWA-FILE-KEY TO IOP-FILE-KEY.                           ELUKYTAB
00953      COMPUTE IOP-KEY-LEN =   LENGTH OF GCG-GROUP-NUM              ELUKYTAB
00954                            + LENGTH OF GCG-SECTION-NUM            ELUKYTAB
00955                            + LENGTH OF GCG-PLAN-CODE.             ELUKYTAB
00956 /***********************************************************      ELUKYTAB
00957 *                                                          *      ELUKYTAB
00958 *        COMPLETE KTG TABLE                                *      ELUKYTAB
00959 *                                                          *      ELUKYTAB
00960 ************************************************************      ELUKYTAB
00961  COMPLETE-KTG-TABLE.                                              ELUKYTAB
00962      PERFORM PASS-GROUP-SPECIFIC-FILE                             ELUKYTAB
00963          UNTIL    GCG-GROUP-NUM IS NOT EQUAL TO SSB-GROUP-NUMBER  ELUKYTAB
00964                OR GCG-SECTION-NUM  IS NOT EQUAL TO SSB-SECTN-NO   ELUKYTAB
00965                OR KTG-TBL-FULL                                    ELUKYTAB
00966                OR IOP-RC-ENDFILE.                                 ELUKYTAB
00967      PERFORM END-BROWSE-OF-GROUP-SPECIFIC.                        ELUKYTAB
00968                                                                   ELUKYTAB
00969  PASS-GROUP-SPECIFIC-FILE.                                        ELUKYTAB
00970      PERFORM EXAMINE-GRSP-EFF-TERM-DATES.                         ELUKYTAB
00971      PERFORM BUILD-KTG-ENTRY.                                     ELUKYTAB
00972      PERFORM READ-NEXT-GROUP-SPEC-REC.                            ELUKYTAB
00973                                                                   ELUKYTAB
00974  BUILD-KTG-ENTRY.                                                 ELUKYTAB
00975      SET KTG-IDX UP BY 1.                                         ELUKYTAB
00976      ADD 1 TO KTG-NBR-KEYS.                                       ELUKYTAB
00977      MOVE GCG-PLAN-CODE   TO KTG-PLAN-CODE(KTG-IDX).              ELUKYTAB
00978      MOVE GCG-PKG-CODE    TO KTG-PKG-CODE(KTG-IDX).               ELUKYTAB
00979      MOVE GCG-FAM-REL-LVL TO KTG-FAM-REL-LVL (KTG-IDX).           ELUKYTAB
00980      MOVE GCG-EFFDT-CEN   TO KTG-EFF-DT-CENTURY (KTG-IDX).        ELUKYTAB
00981      MOVE GCG-TERMDT-CEN  TO KTG-TERM-DT-CENTURY (KTG-IDX).       ELUKYTAB
00982      MOVE GCG-L-O-B-CONTRACT-LEVEL-IND                            ELUKYTAB
00983        TO KTG-L-O-B-CONTRACT-LEVEL-IND (KTG-IDX).                 ELUKYTAB
00984      MOVE GCG-PARTICIPAT-PROV-OPTION                              ELUKYTAB
00985        TO KTG-PARTICIPAT-PROV-OPTION (KTG-IDX).                   ELUKYTAB
00986      MOVE GCG-POS-PARTICP-IND                                     ELUKYTAB
00987        TO KTG-POS-PARTICP-IND (KTG-IDX).                          ELUKYTAB
00988      MOVE GCG-PAN-PARTICIPATION-IND                               ELUKYTAB
00989        TO KTG-PAN-PARTICP-IND (KTG-IDX).                          ELUKYTAB
00990      MOVE GCG-CBL-PARTICIPATION-IND                               ELUKYTAB
00991        TO KTG-CBL-PARTICP-IND (KTG-IDX).                          ELUKYTAB
00992      MOVE GCG-CPO-PARTICIPATION-IND                               ELUKYTAB
00993        TO KTG-CPO-PARTICP-IND (KTG-IDX).                          ELUKYTAB
00994      MOVE GCG-RPO-INDICATOR                                       ELUKYTAB
00995        TO KTG-RPO-PARTICP-IND (KTG-IDX).                          ELUKYTAB
00996      MOVE GCG-NEW-POS-IND                                         ELUKYTAB
00997        TO KTG-NEW-POS-IND (KTG-IDX).                              ELUKYTAB
00998      MOVE GCG-PRODUCT-TYPE                                        ELUKYTAB
00999        TO KTG-PRODUCT-TYPE (KTG-IDX).                             ELUKYTAB
01000      MOVE GCG-ELEC-PRES-DRUG-PGM-IND                              ELUKYTAB
01001        TO KTG-BLUE-SCRIPT (KTG-IDX).                              ELUKYTAB
01002      MOVE GCG-PROV-CONTROL-CONT-BC-IND TO                         ELUKYTAB
01003          KTG-PC-LVL-INST-BAS-IND (KTG-IDX).                       ELUKYTAB
01004      MOVE GCG-PROV-CONTROL-CONT-BS-IND TO                         ELUKYTAB
01005          KTG-PC-LVL-PROF-BAS-IND (KTG-IDX).                       ELUKYTAB
01006      MOVE GCG-PROV-CONTROL-CONT-MM-IND TO                         ELUKYTAB
01007          KTG-PC-LVL-INST-SUP-IND (KTG-IDX)                        ELUKYTAB
01008          KTG-PC-LVL-PROF-SUP-IND (KTG-IDX).                       ELUKYTAB
01009                                                                   ELUKYTAB
01010      IF GCG-INTER-RELATIONAL-CODE = ZEROES                        ELUKYTAB
01011          SET KTG-EXC (KTG-IDX) TO TRUE                            ELUKYTAB
01012          SET EXCLUDE-FOUND TO TRUE                                ELUKYTAB
01013      ELSE                                                         ELUKYTAB
01014          SET KTG-SEL (KTG-IDX) TO TRUE                            ELUKYTAB
01015      END-IF.                                                      ELUKYTAB
01016                                                                   ELUKYTAB
01017  EXAMINE-GRSP-EFF-TERM-DATES.                                     ELUKYTAB
01018      IF GCG-EFFDT-CEN LESS THAN SSB-COVRD-FROM-DATE-CEN           ELUKYTAB
01019          MOVE GCG-EFFDT-CEN  TO SSB-COVRD-FROM-DATE-CEN.          ELUKYTAB
01020      IF GCG-TERMDT-CEN GREATER THAN SSB-COVRD-TO-DATE-CEN         ELUKYTAB
01021          MOVE GCG-TERMDT-CEN TO SSB-COVRD-TO-DATE-CEN.            ELUKYTAB
01022 /***********************************************************      ELUKYTAB
01023 *                                                          *      ELUKYTAB
01024 *        BUILD KTG TABLE.END                               *      ELUKYTAB
01025 *                                                          *      ELUKYTAB
01026 ************************************************************      ELUKYTAB
01027  BUILD-KTG-TABLE-END.                                             ELUKYTAB
01028      SET  CIA-ELSKTBG-DDN TO TRUE.                                ELUKYTAB
01029      MOVE LENGTH OF KTG-GCGRPSPC-KEY-TABLE TO                     ELUKYTAB
01030          CIA-AREA-LEN.                                            ELUKYTAB
01031      PERFORM STOW-KEY-TABLE.                                      ELUKYTAB
01032 /***********************************************************      ELUKYTAB
01033 *                                                          *      ELUKYTAB
01034 *        BUILD KTC TABLE                                   *      ELUKYTAB
01035 *                                                          *      ELUKYTAB
01036 ************************************************************      ELUKYTAB
01037  BUILD-KTC-TABLE.                                                 ELUKYTAB
01038      PERFORM BUILD-KTC-TABLE-BEGIN.                               ELUKYTAB
01039      PERFORM BUILD-KTC-TABLE-PROCESS.                             ELUKYTAB
01040      PERFORM BUILD-KTC-TABLE-END.                                 ELUKYTAB
01041                                                                   ELUKYTAB
01042  BUILD-KTC-TABLE-BEGIN.                                           ELUKYTAB
01043      PERFORM ALLOCATE-KTC-TABLE.                                  ELUKYTAB
01044      PERFORM INITIALIZE-KTC-TABLE.                                ELUKYTAB
01045                                                                   ELUKYTAB
01046  ALLOCATE-KTC-TABLE.                                              ELUKYTAB
01047      SET CIA-ELSKTBC-DDN TO TRUE.                                 ELUKYTAB
01048      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYTAB
01049                            WS-DUMMY-PTR.                          ELUKYTAB
01050      MOVE CIA-MVO    TO  WS-MAX-KTC.                              ELUKYTAB
01051      COMPUTE CIA-AREA-LEN =   LENGTH OF KTC-NBR-KEYS              ELUKYTAB
01052         + (  CIA-MVO             * LENGTH OF KTC-KEY-TBL ).       ELUKYTAB
01053      PERFORM ACQUIRE-CONTROLLED-STORAGE.                          ELUKYTAB
01054      SET CIA-ELSKTBC-DDN TO TRUE.                                 ELUKYTAB
01055      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYTAB
01056                 ADDRESS OF KTC-GCCONTR-KEY-TABLE.                 ELUKYTAB
01057                                                                   ELUKYTAB
01058  INITIALIZE-KTC-TABLE.                                            ELUKYTAB
01059      MOVE ZERO TO KTC-NBR-KEYS.                                   ELUKYTAB
01060      SET KTC-IDX TO KTC-NBR-KEYS.                                 ELUKYTAB
01061 /***********************************************************      ELUKYTAB
01062 *                                                          *      ELUKYTAB
01063 *        BUILD KTC TABLE.PROCESS                           *      ELUKYTAB
01064 *                                                          *      ELUKYTAB
01065 ************************************************************      ELUKYTAB
01066  BUILD-KTC-TABLE-PROCESS.                                         ELUKYTAB
01067      PERFORM START-SCAN-OF-CONTRACT.                              ELUKYTAB
01068      PERFORM COMPLETE-CONTRACT-KEY-TABLE.                         ELUKYTAB
01069                                                                   ELUKYTAB
01070  START-SCAN-OF-CONTRACT.                                          ELUKYTAB
01071      PERFORM START-CONTRACT-BROWSE-FOR-KEYS.                      ELUKYTAB
01072      IF IOP-RC-OK                                                 ELUKYTAB
01073          SET CONT-FOUND TO TRUE                                   ELUKYTAB
01074          PERFORM READ-NEXT-CONTRACT-RECORD                        ELUKYTAB
01075      ELSE IF IOP-RC-NOTFND                                        ELUKYTAB
01076          PERFORM SIGNAL-NO-CONTRACT-SECTION                       ELUKYTAB
01077      ELSE                                                         ELUKYTAB
01078          PERFORM SIGNAL-UNKNOWN-ERROR.                            ELUKYTAB
01079                                                                   ELUKYTAB
01080  START-CONTRACT-BROWSE-FOR-KEYS.                                  ELUKYTAB
01081      PERFORM ESTAB-ADDR-OF-GCCONTR-BLOCK.                         ELUKYTAB
01082      PERFORM BUILD-CONTRACT-BROWSE-KEY.                           ELUKYTAB
01083      PERFORM START-BROWSE-OF-CONTRACT-FILE.                       ELUKYTAB
01084                                                                   ELUKYTAB
01085  BUILD-CONTRACT-BROWSE-KEY.                                       ELUKYTAB
01086      INITIALIZE KWA-GCCONTR-KEY.                                  ELUKYTAB
01087      MOVE ZERO TO KWA-GCT-PLAN-CODE                               ELUKYTAB
01088      MOVE SSB-GROUP-NUMBER TO KWA-GCT-GROUP-NUMBER.               ELUKYTAB
01089      MOVE SSB-SECTN-NO TO KWA-GCT-SECTION-NUMBER.                 ELUKYTAB
01090      MOVE LOW-VALUES TO KWA-GCT-FAM-REL-LVL, KWA-GCT-PKG-CODE,    ELUKYTAB
01091       KWA-GCT-PROVDR-CONTROL.                                     ELUKYTAB
01092      MOVE KWA-FILE-KEY TO IOP-FILE-KEY.                           ELUKYTAB
01093      COMPUTE IOP-KEY-LEN =   LENGTH OF GCT-GROUP-NUM              ELUKYTAB
01094                            + LENGTH OF GCT-SECTION-NUM            ELUKYTAB
01095                            + LENGTH OF GCT-PLAN-CODE.             ELUKYTAB
01096 /***********************************************************      ELUKYTAB
01097 *                                                          *      ELUKYTAB
01098 *        COMPLETE CONTRACT KEY TABLE                       *      ELUKYTAB
01099 *                                                          *      ELUKYTAB
01100 ************************************************************      ELUKYTAB
01101  COMPLETE-CONTRACT-KEY-TABLE.                                     ELUKYTAB
01102      PERFORM SCAN-CONTRACT-FILE-FOR-KEYS                          ELUKYTAB
01103          UNTIL    GCT-GROUP-NUM IS NOT EQUAL TO SSB-GROUP-NUMBER  ELUKYTAB
01104                OR GCT-SECTION-NUM IS NOT EQUAL TO SSB-SECTN-NO    ELUKYTAB
01105                OR KTC-TBL-FULL                                    ELUKYTAB
01106                OR IOP-RC-ENDFILE.                                 ELUKYTAB
01107      PERFORM END-BROWSE-OF-CONTRACT-FILE.                         ELUKYTAB
01108                                                                   ELUKYTAB
01109  SCAN-CONTRACT-FILE-FOR-KEYS.                                     ELUKYTAB
01110      PERFORM EXAMINE-CONTRACT-EFF-TERM-DATE.                      ELUKYTAB
01111      PERFORM ADD-CONTRACT-KEY-TO-TABLE.                           ELUKYTAB
01112      PERFORM READ-NEXT-CONTRACT-RECORD.                           ELUKYTAB
01113                                                                   ELUKYTAB
01114  ADD-CONTRACT-KEY-TO-TABLE.                                       ELUKYTAB
01115      SET KTC-IDX UP BY 1.                                         ELUKYTAB
01116      ADD 1 TO KTC-NBR-KEYS.                                       ELUKYTAB
01117      MOVE GCT-PLAN-CODE TO KTC-PLAN-CODE (KTC-IDX).               ELUKYTAB
01118      MOVE GCT-PKG-CODE TO KTC-PKG-CODE (KTC-IDX).                 ELUKYTAB
01119      MOVE GCT-L-O-B TO KTC-L-O-B (KTC-IDX).                       ELUKYTAB
01120      MOVE GCT-FAM-REL-LVL TO KTC-FAM-REL-LVL (KTC-IDX).           ELUKYTAB
01121      MOVE GCT-PROVDR-CONTROL TO KTC-PROVDR-CONTROL                ELUKYTAB
01122          (KTC-IDX).                                               ELUKYTAB
01123      MOVE GCT-EFFDT-CEN  TO KTC-EFF-DT-CENTURY (KTC-IDX).         ELUKYTAB
01124      MOVE GCT-TERMDT-CEN TO KTC-TERM-DT-CENTURY (KTC-IDX).        ELUKYTAB
01125                                                                   ELUKYTAB
01126      IF GCT-INTER-REL-CD = ZEROES                                 ELUKYTAB
01127          PERFORM VARYING WS-KTC-SUB FROM 1 BY 1                   ELUKYTAB
01128                  UNTIL WS-KTC-SUB > 4                             ELUKYTAB
01129              SET KTC-SEL-IDX TO WS-KTC-SUB                        ELUKYTAB
01130              SET KTC-EXC (KTC-IDX KTC-SEL-IDX) TO TRUE            ELUKYTAB
01131          END-PERFORM                                              ELUKYTAB
01132          SET EXCLUDE-FOUND TO TRUE                                ELUKYTAB
01133      ELSE                                                         ELUKYTAB
01134          PERFORM VARYING WS-KTC-SUB FROM 1 BY 1                   ELUKYTAB
01135                  UNTIL WS-KTC-SUB > 4                             ELUKYTAB
01136              SET KTC-SEL-IDX TO WS-KTC-SUB                        ELUKYTAB
01137              SET KTC-SEL (KTC-IDX KTC-SEL-IDX) TO TRUE            ELUKYTAB
01138          END-PERFORM                                              ELUKYTAB
01139      END-IF.                                                      ELUKYTAB
01140                                                                   ELUKYTAB
01141  EXAMINE-CONTRACT-EFF-TERM-DATE.                                  ELUKYTAB
01142      IF GCT-EFFDT-CEN   LESS THAN SSB-COVRD-FROM-DATE-CEN         ELUKYTAB
01143          MOVE GCT-EFFDT-CEN   TO SSB-COVRD-FROM-DATE-CEN.         ELUKYTAB
01144      IF GCT-TERMDT-CEN  GREATER THAN SSB-COVRD-TO-DATE-CEN        ELUKYTAB
01145          MOVE GCT-TERMDT-CEN   TO SSB-COVRD-TO-DATE-CEN.          ELUKYTAB
01146 /***********************************************************      ELUKYTAB
01147 *                                                          *      ELUKYTAB
01148 *        BUILD KTC TABLE.END                               *      ELUKYTAB
01149 *                                                          *      ELUKYTAB
01150 ************************************************************      ELUKYTAB
01151  BUILD-KTC-TABLE-END.                                             ELUKYTAB
01152      SET  CIA-ELSKTBC-DDN TO TRUE.                                ELUKYTAB
01153      MOVE LENGTH OF KTC-GCCONTR-KEY-TABLE TO                      ELUKYTAB
01154          CIA-AREA-LEN.                                            ELUKYTAB
01155      PERFORM STOW-KEY-TABLE.                                      ELUKYTAB
01156 /***********************************************************      ELUKYTAB
01157 *                                                          *      ELUKYTAB
01158 *        ESTAB ADDR OF GCGRPSPC BLOCK                      *      ELUKYTAB
01159 *                                                          *      ELUKYTAB
01160 ************************************************************      ELUKYTAB
01161  ESTAB-ADDR-OF-GCGRPSPC-BLOCK.                                    ELUKYTAB
01162      PERFORM SET-ADDRESS-OF-GCGRPSPC.                             ELUKYTAB
01163      IF NOT CIA-RC-OK                                             ELUKYTAB
01164          PERFORM ALLOCATE-GCGRPSPC-BLOCK.                         ELUKYTAB
01165                                                                   ELUKYTAB
01166                                                                   ELUKYTAB
01167 ************************************************************      ELUKYTAB
01168 *                                                          *      ELUKYTAB
01169 *        ESTAB ADDR OF GCCONTR BLOCK                       *      ELUKYTAB
01170 *                                                          *      ELUKYTAB
01171 ************************************************************      ELUKYTAB
01172  ESTAB-ADDR-OF-GCCONTR-BLOCK.                                     ELUKYTAB
01173      PERFORM SET-IOPM-ADDRESS-FOR-CONTRACT.                       ELUKYTAB
01174      IF NOT CIA-RC-OK                                             ELUKYTAB
01175          PERFORM ALLOCATE-GCCONTR-BLOCK.                          ELUKYTAB
01176      EJECT                                                        ELUKYTAB
01177                                                                   ELUKYTAB
01178                                                                   ELUKYTAB
01179 ************************************************************      ELUKYTAB
01180 *                                                          *      ELUKYTAB
01181 *        START GROUP BROWSE                                *      ELUKYTAB
01182 *                                                          *      ELUKYTAB
01183 ************************************************************      ELUKYTAB
01184  START-GROUP-BROWSE.                                              ELUKYTAB
01185      SET CIA-GCGRPSPC-DDN TO TRUE.                                ELUKYTAB
01186      SET IOP-ST-BR TO TRUE.                                       ELUKYTAB
01187      SET IOP-FCQ-GEN TO TRUE.                                     ELUKYTAB
01188      SET IOP-KVQ-EQ TO TRUE.                                      ELUKYTAB
01189      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELUKYTAB
01190      PERFORM CALL-INPUT-OUTPUT.                                   ELUKYTAB
01191                                                                   ELUKYTAB
01192                                                                   ELUKYTAB
01193 ************************************************************      ELUKYTAB
01194 *                                                          *      ELUKYTAB
01195 *        END BROWSE OF GROUP SPECIFIC                      *      ELUKYTAB
01196 *                                                          *      ELUKYTAB
01197 ************************************************************      ELUKYTAB
01198  END-BROWSE-OF-GROUP-SPECIFIC.                                    ELUKYTAB
01199      SET CIA-GCGRPSPC-DDN TO TRUE.                                ELUKYTAB
01200      SET IOP-END-BR TO TRUE.                                      ELUKYTAB
01201      SET IOP-FCQ-NONE TO TRUE.                                    ELUKYTAB
01202      SET IOP-KVQ-NONE TO TRUE.                                    ELUKYTAB
01203      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELUKYTAB
01204      PERFORM CALL-INPUT-OUTPUT.                                   ELUKYTAB
01205 /***********************************************************      ELUKYTAB
01206 *                                                          *      ELUKYTAB
01207 *        READ NEXT GROUP SPEC REC                          *      ELUKYTAB
01208 *                                                          *      ELUKYTAB
01209 ************************************************************      ELUKYTAB
01210  READ-NEXT-GROUP-SPEC-REC.                                        ELUKYTAB
01211      SET CIA-GCGRPSPC-DDN TO TRUE.                                ELUKYTAB
01212      SET IOP-RD-NXT TO TRUE.                                      ELUKYTAB
01213      SET IOP-FCQ-NONE TO TRUE.                                    ELUKYTAB
01214      SET IOP-KVQ-NONE TO TRUE.                                    ELUKYTAB
01215      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELUKYTAB
01216      PERFORM CALL-INPUT-OUTPUT.                                   ELUKYTAB
01217      SET ADDRESS OF GCG-GROUP-SPECIFIC-RECORD-AREA TO             ELUKYTAB
01218          IOP-REC-PTR.                                             ELUKYTAB
01219                                                                   ELUKYTAB
01220                                                                   ELUKYTAB
01221 ************************************************************      ELUKYTAB
01222 *                                                          *      ELUKYTAB
01223 *        START BROWSE OF CONTRACT FILE                     *      ELUKYTAB
01224 *                                                          *      ELUKYTAB
01225 ************************************************************      ELUKYTAB
01226  START-BROWSE-OF-CONTRACT-FILE.                                   ELUKYTAB
01227      SET CIA-GCCONTR-DDN TO TRUE.                                 ELUKYTAB
01228      SET IOP-ST-BR TO TRUE.                                       ELUKYTAB
01229      SET IOP-FCQ-GEN TO TRUE.                                     ELUKYTAB
01230      SET IOP-KVQ-EQ TO TRUE.                                      ELUKYTAB
01231      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELUKYTAB
01232      PERFORM CALL-INPUT-OUTPUT.                                   ELUKYTAB
01233                                                                   ELUKYTAB
01234                                                                   ELUKYTAB
01235 ************************************************************      ELUKYTAB
01236 *                                                          *      ELUKYTAB
01237 *        END BROWSE OF CONTRACT FILE                       *      ELUKYTAB
01238 *                                                          *      ELUKYTAB
01239 ************************************************************      ELUKYTAB
01240  END-BROWSE-OF-CONTRACT-FILE.                                     ELUKYTAB
01241      SET CIA-GCCONTR-DDN TO TRUE.                                 ELUKYTAB
01242      SET IOP-END-BR TO TRUE.                                      ELUKYTAB
01243      SET IOP-FCQ-NONE TO TRUE.                                    ELUKYTAB
01244      SET IOP-KVQ-NONE TO TRUE.                                    ELUKYTAB
01245      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELUKYTAB
01246      PERFORM CALL-INPUT-OUTPUT.                                   ELUKYTAB
01247 /***********************************************************      ELUKYTAB
01248 *                                                          *      ELUKYTAB
01249 *        READ NEXT CONTRACT RECORD                         *      ELUKYTAB
01250 *                                                          *      ELUKYTAB
01251 ************************************************************      ELUKYTAB
01252  READ-NEXT-CONTRACT-RECORD.                                       ELUKYTAB
01253      SET CIA-GCCONTR-DDN TO TRUE.                                 ELUKYTAB
01254      SET IOP-RD-NXT TO TRUE.                                      ELUKYTAB
01255      SET IOP-FCQ-NONE TO TRUE.                                    ELUKYTAB
01256      SET IOP-KVQ-NONE TO TRUE.                                    ELUKYTAB
01257      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELUKYTAB
01258      PERFORM CALL-INPUT-OUTPUT.                                   ELUKYTAB
01259      SET ADDRESS OF GCT-CONTRACT-RECORD-AREA TO IOP-REC-PTR.      ELUKYTAB
01260                                                                   ELUKYTAB
01261                                                                   ELUKYTAB
01262 ************************************************************      ELUKYTAB
01263 *                                                          *      ELUKYTAB
01264 *        BUILD KEY TABLES.TERMINATE                        *      ELUKYTAB
01265 *                                                          *      ELUKYTAB
01266 ************************************************************      ELUKYTAB
01267  BUILD-KEY-TABLES-TERMINATE.                                      ELUKYTAB
01268      EVALUATE TRUE                                                ELUKYTAB
01269      WHEN SECTION-NOT-FOUND                                       ELUKYTAB
01270          SET CIA-RC-KTB-SECTN-NOTFND TO TRUE                      ELUKYTAB
01271      WHEN GRP-NOT-FOUND OR CONT-NOT-FOUND                         ELUKYTAB
01272          SET CIA-RC-KTB-GRP-NOTFND TO TRUE                        ELUKYTAB
01273      WHEN OTHER                                                   ELUKYTAB
01274          SET CIA-RC-OK TO TRUE                                    ELUKYTAB
01275      END-EVALUATE.                                                ELUKYTAB
01276 /***********************************************************      ELUKYTAB
01277 *                                                          *      ELUKYTAB
01278 *        ALLOCATE GCGRPSPC BLOCK                           *      ELUKYTAB
01279 *                                                          *      ELUKYTAB
01280 ************************************************************      ELUKYTAB
01281  ALLOCATE-GCGRPSPC-BLOCK.                                         ELUKYTAB
01282      SET CIA-GCGRPSPC-DDN TO TRUE.                                ELUKYTAB
01283      PERFORM ACQUIRE-CONTROLLED-STORAGE.                          ELUKYTAB
01284      PERFORM SET-ADDRESS-OF-GCGRPSPC.                             ELUKYTAB
01285                                                                   ELUKYTAB
01286                                                                   ELUKYTAB
01287 ************************************************************      ELUKYTAB
01288 *                                                          *      ELUKYTAB
01289 *        ALLOCATE GCCONTR BLOCK                            *      ELUKYTAB
01290 *                                                          *      ELUKYTAB
01291 ************************************************************      ELUKYTAB
01292  ALLOCATE-GCCONTR-BLOCK.                                          ELUKYTAB
01293      SET CIA-GCCONTR-DDN TO TRUE.                                 ELUKYTAB
01294      PERFORM ACQUIRE-CONTROLLED-STORAGE.                          ELUKYTAB
01295      PERFORM SET-IOPM-ADDRESS-FOR-CONTRACT.                       ELUKYTAB
01296                                                                   ELUKYTAB
01297                                                                   ELUKYTAB
01298 ************************************************************      ELUKYTAB
01299 *                                                          *      ELUKYTAB
01300 *        ACQUIRE CONTROLLED STORAGE                        *      ELUKYTAB
01301 *                                                          *      ELUKYTAB
01302 ************************************************************      ELUKYTAB
01303  ACQUIRE-CONTROLLED-STORAGE.                                      ELUKYTAB
01304      SET CIA-STG-GETMAIN TO TRUE.                                 ELUKYTAB
01305      PERFORM CALL-STORAGE-MANAGEMENT.                             ELUKYTAB
01306                                                                   ELUKYTAB
01307                                                                   ELUKYTAB
01308 ************************************************************      ELUKYTAB
01309 *                                                          *      ELUKYTAB
01310 *        STOW KEY TABLE                                    *      ELUKYTAB
01311 *                                                          *      ELUKYTAB
01312 ************************************************************      ELUKYTAB
01313  STOW-KEY-TABLE.                                                  ELUKYTAB
01314      SET CIA-STG-STOW TO TRUE.                                    ELUKYTAB
01315      PERFORM CALL-STORAGE-MANAGEMENT.                             ELUKYTAB
01316 ************************************************************      ELUKYTAB
01317 *                                                          *      ELUKYTAB
01318 *        CALL INPUT-OUTPUT                                 *      ELUKYTAB
01319 *                                                          *      ELUKYTAB
01320 ************************************************************      ELUKYTAB
01321  CALL-INPUT-OUTPUT.                                               ELUKYTAB
01322      CALL 'ELUIOPGM' USING DFHEIBLK, DFHCOMMAREA.                 ELUKYTAB
01323                                                                   ELUKYTAB
01324 ************************************************************      ELUKYTAB
01325 *                                                          *      ELUKYTAB
01326 *        SIGNAL UNKNOWN ERROR                              *      ELUKYTAB
01327 *                                                          *      ELUKYTAB
01328 ************************************************************      ELUKYTAB
01329  SIGNAL-UNKNOWN-ERROR.                                            ELUKYTAB
01330      SET CIA-AB-UNDEF TO TRUE.                                    ELUKYTAB
01331      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELUKYTAB
01332                                                                   ELUKYTAB
01333                                                                   ELUKYTAB
01334 ************************************************************      ELUKYTAB
01335 *                                                          *      ELUKYTAB
01336 *        SIGNAL NO GROUP SECTION                           *      ELUKYTAB
01337 *                                                          *      ELUKYTAB
01338 ************************************************************      ELUKYTAB
01339  SIGNAL-NO-GROUP-SECTION.                                         ELUKYTAB
01340      SET CIA-AB-NOTFND-GRP-SECT TO TRUE.                          ELUKYTAB
01341      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELUKYTAB
01342 /***********************************************************      ELUKYTAB
01343 *                                                          *      ELUKYTAB
01344 *        SIGNAL NO CONTRACT SECTION                        *      ELUKYTAB
01345 *                                                          *      ELUKYTAB
01346 ************************************************************      ELUKYTAB
01347  SIGNAL-NO-CONTRACT-SECTION.                                      ELUKYTAB
01348      SET CIA-AB-NOTFND-CONT-SECT TO TRUE.                         ELUKYTAB
01349      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELUKYTAB
01350                                                                   ELUKYTAB
01351                                                                   ELUKYTAB
01352 ************************************************************      ELUKYTAB
01353 *                                                          *      ELUKYTAB
01354 *        SIGNAL COMMAREA LENGTH ERROR                      *      ELUKYTAB
01355 *                                                          *      ELUKYTAB
01356 ************************************************************      ELUKYTAB
01357  SIGNAL-COMMAREA-LENGTH-ERROR.                                    ELUKYTAB
01358      SET CIA-AB-DFHCOMMAREA TO TRUE.                              ELUKYTAB
01359      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELUKYTAB
01360                                                                   ELUKYTAB
01361                                                                   ELUKYTAB
01362 ************************************************************      ELUKYTAB
01363 *                                                          *      ELUKYTAB
01364 *        SIGNAL CIA ADDRESSING ERROR                       *      ELUKYTAB
01365 *                                                          *      ELUKYTAB
01366 ************************************************************      ELUKYTAB
01367  SIGNAL-CIA-ADDRESSING-ERROR.                                     ELUKYTAB
01368      SET CIA-AB-ELSCIA-PTR TO TRUE.                               ELUKYTAB
01369      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELUKYTAB
01370                                                                   ELUKYTAB
01371                                                                   ELUKYTAB
01372 ************************************************************      ELUKYTAB
01373 *                                                          *      ELUKYTAB
01374 *        SIGNAL TABLE OVERFLOW                             *      ELUKYTAB
01375 *                                                          *      ELUKYTAB
01376 ************************************************************      ELUKYTAB
01377  SIGNAL-TABLE-OVERFLOW.                                           ELUKYTAB
01378      SET CIA-AB-INCR-TBL-SIZE TO TRUE.                            ELUKYTAB
01379      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELUKYTAB
01380                                                                   ELUKYTAB
01381                                                                   ELUKYTAB
01382 ************************************************************      ELUKYTAB
01383 *                                                          *      ELUKYTAB
01384 *        SIGNAL MISSING PARAMETER                          *      ELUKYTAB
01385 *                                                          *      ELUKYTAB
01386 ************************************************************      ELUKYTAB
01387  SIGNAL-MISSING-PARAMETER.                                        ELUKYTAB
01388      SET CIA-AB-PARM-MISSING TO TRUE.                             ELUKYTAB
01389      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELUKYTAB
01390                                                                   ELUKYTAB
01391 ************************************************************      ELUKYTAB
01392 *                                                          *      ELUKYTAB
01393 *        SEARCH FOR ALLIANCE                               *      ELUKYTAB
01394 *                                                          *      ELUKYTAB
01395 ************************************************************      ELUKYTAB
01396  SEARCH-FOR-ALLIANCE.                                             ELUKYTAB
01397      PERFORM GETMAIN-MEMBERSHIP-INFORMATION.                      ELUKYTAB
01398      PERFORM GETMAIN-CSEXECIO-CONTROL.                            ELUKYTAB
01399      EXEC CICS ADDRESS                                            ELUKYTAB
01400           TWA (WS-TWA-PTR)                                        ELUKYTAB
01401      END-EXEC.                                                    ELUKYTAB
01402      SET ADDRESS OF TWA-TRANSACTION-WORK-AREA TO WS-TWA-PTR.      ELUKYTAB
01403      PERFORM READ-GROUP-MASTER-RECORD.                            ELUKYTAB
01404                                                                   ELUKYTAB
01405  GETMAIN-MEMBERSHIP-INFORMATION.                                  ELUKYTAB
01406      SET CIA-ELSMEMS-DDN TO TRUE.                                 ELUKYTAB
01407      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYTAB
01408                 ADDRESS OF MSI-MEMBERSHIP-INTERFACE.              ELUKYTAB
01409      COMPUTE CIA-AREA-LEN = LENGTH OF MSI-MBR-INFO +              ELUKYTAB
01410          (CIA-MVO * LENGTH OF MSI-MBR-SECN-TBL).                  ELUKYTAB
01411      SET CIA-STG-GETMAIN TO TRUE.                                 ELUKYTAB
01412      PERFORM CALL-STRGE-MANAGER.                                  ELUKYTAB
01413      SET CIA-ELSMEMS-DDN TO TRUE.                                 ELUKYTAB
01414      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYTAB
01415                 ADDRESS OF MSI-MEMBERSHIP-INTERFACE.              ELUKYTAB
01416                                                                   ELUKYTAB
01417  CALL-STRGE-MANAGER.                                              ELUKYTAB
01418      SET CIA-COBXIO-DDN TO TRUE.                                  ELUKYTAB
01419      SET CIA-STG-GETMAIN TO TRUE.                                 ELUKYTAB
01420      PERFORM CALL-STORAGE-MANAGEMENT.                             ELUKYTAB
01421      SET CIA-COBXIO-DDN TO TRUE.                                  ELUKYTAB
01422      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYTAB
01423                 ADDRESS OF GMF-CSEXECIO.                          ELUKYTAB
01424                                                                   ELUKYTAB
01425  GETMAIN-CSEXECIO-CONTROL.                                        ELUKYTAB
01426      SET CIA-COBXIO-DDN TO TRUE.                                  ELUKYTAB
01427      SET CIA-STG-GETMAIN TO TRUE.                                 ELUKYTAB
01428      PERFORM CALL-STORAGE-MANAGEMENT.                             ELUKYTAB
01429      SET CIA-COBXIO-DDN TO TRUE.                                  ELUKYTAB
01430      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYTAB
01431           ADDRESS OF GMF-CSEXECIO.                                ELUKYTAB
01432                                                                   ELUKYTAB
01433  READ-GROUP-MASTER-RECORD.                                        ELUKYTAB
01434      MOVE ZEROES TO KTG-ALLIANCE-IND.                             ELUKYTAB
01435      SET GMFX-FIND TO TRUE.                                       ELUKYTAB
01436      MOVE 'GMF' TO GMFX-GMF.                                      ELUKYTAB
01437      MOVE LOW-VALUES TO GMFX-SECKY.                               ELUKYTAB
01438      MOVE SSB-GROUP-NUMBER TO WS-GRP-NBR.                         ELUKYTAB
01439      MOVE WS-GRP-NBR TO GMFX-GRPKY.                               ELUKYTAB
01440      PERFORM SETUP-IO-PARMS-AND-LINK-TO-MEM.                      ELUKYTAB
01441      SET ADDRESS OF 000-MEMBER-HEADER TO GMFX-RECORD-POINTERS(1). ELUKYTAB
01442      IF GMFX-OK                                                   ELUKYTAB
01443         IF 000-ID-MESSAGE = 'A '                                  ELUKYTAB
01444             MOVE 000-ID-MESSAGE TO KTG-ALLIANCE-IND               ELUKYTAB
01445         END-IF                                                    ELUKYTAB
01446      END-IF.                                                      ELUKYTAB
01447                                                                   ELUKYTAB
01448  SETUP-IO-PARMS-AND-LINK-TO-MEM.                                  ELUKYTAB
01449      MOVE '1' TO GMFX-PARAM.                                      ELUKYTAB
01450      SET GMFX-PTR-MODE TO TRUE.                                   ELUKYTAB
01451 *    SET GMFX-OK TO TRUE.                                         ELUKYTAB
01452      PERFORM CALL-CSEXECIO-INTERFACE.                             ELUKYTAB
01453                                                                   ELUKYTAB
01454  CALL-CSEXECIO-INTERFACE.                                         ELUKYTAB
01455      PERFORM PRESERVE-TWA-AREA.                                   ELUKYTAB
01456      EXEC CICS LINK                                               ELUKYTAB
01457                PROGRAM('CSEXECIO')                                ELUKYTAB
01458                COMMAREA(GMF-CSEXECIO)                             ELUKYTAB
01459                END-EXEC.                                          ELUKYTAB
01460      PERFORM RESTORE-TWA-AREA.                                    ELUKYTAB
01461      IF GMFX-INVALID-REQUEST OR                                   ELUKYTAB
01462         GMFX-IO-ERROR                                             ELUKYTAB
01463        PERFORM SIGNAL-IO-ERROR                                    ELUKYTAB
01464      END-IF.                                                      ELUKYTAB
01465                                                                   ELUKYTAB
01466  PRESERVE-TWA-AREA.                                               ELUKYTAB
01467      SET WS-TWA-PTR TO TWA-ELSCOMM-PTR.                           ELUKYTAB
01468      SET TWA-ELSCOMM-PTR TO WS-TWA-HOLD-PTR.                      ELUKYTAB
01469                                                                   ELUKYTAB
01470  RESTORE-TWA-AREA.                                                ELUKYTAB
01471      SET WS-TWA-HOLD-PTR TO TWA-ELSCOMM-PTR.                      ELUKYTAB
01472      SET TWA-ELSCOMM-PTR TO WS-TWA-PTR.                           ELUKYTAB
01473                                                                   ELUKYTAB
01474  CALL-STORAGE-COBXIO.                                             ELUKYTAB
01475      SET CIA-COBXIO-DDN TO TRUE.                                  ELUKYTAB
01476      SET CIA-STG-GETMAIN TO TRUE.                                 ELUKYTAB
01477      PERFORM CALL-STORAGE-MANAGEMENT.                             ELUKYTAB
01478      SET CIA-COBXIO-DDN TO TRUE.                                  ELUKYTAB
01479      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYTAB
01480                 ADDRESS OF GMF-CSEXECIO.                          ELUKYTAB
01481                                                                   ELUKYTAB
01482 /***********************************************************      ELUKYTAB
01483 *                                                          *      ELUKYTAB
01484 *        CALL STORAGE MANAGEMENT                           *      ELUKYTAB
01485 *                                                          *      ELUKYTAB
01486 ************************************************************      ELUKYTAB
01487  CALL-STORAGE-MANAGEMENT.                                         ELUKYTAB
01488      CALL 'ELUSTGMG' USING DFHEIBLK, DFHCOMMAREA.                 ELUKYTAB
01489                                                                   ELUKYTAB
01490  SIGNAL-IO-ERROR.                                                 ELUKYTAB
01491       MOVE 'EL62' TO CIA-ABCODE.                                  ELUKYTAB
01492      EXEC CICS ABEND                                              ELUKYTAB
01493         ABCODE(CIA-ABCODE)                                        ELUKYTAB
01494      END-EXEC.                                                    ELUKYTAB
01495                                                                   ELUKYTAB
01496 ************************************************************      ELUKYTAB
01497  SET-KTG-ADDRESS.                                                 ELUKYTAB
01498      SET CIA-ELSKTBG-DDN TO TRUE.                                 ELUKYTAB
01499      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYTAB
01500                            KTG-GCGRPSPC-KEY-TABLE.                ELUKYTAB
