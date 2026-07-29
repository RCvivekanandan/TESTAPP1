00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELTATCP 
00003  PROGRAM-ID.         ELTATCP.                                        LV002
00004                                                                   ELTATCP 
00005  AUTHOR.             RICK BARILEAU.                               ELTATCP 
00006                                                                   ELTATCP 
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTATCP 
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELTATCP 
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTATCP 
00010                      233 N. MICHIGAN AVE                          ELTATCP 
00011                      CHICAGO, ILLINOIS 60601                      ELTATCP 
00012                                                                   ELTATCP 
00013  DATE-WRITTEN.       16-JUN-1987.                                 ELTATCP 
00014                                                                   ELTATCP 
00015  DATE-COMPILED.                                                   ELTATCP 
00016                                                                   ELTATCP 
00017  SECURITY.           COPYRIGHT 1986,                              ELTATCP 
00018                      HEALTH CARE SERVICE CORPORATION              ELTATCP 
00019      SKIP3                                                        ELTATCP 
00020  ENVIRONMENT DIVISION.                                            ELTATCP 
00021                                                                   ELTATCP 
00022  CONFIGURATION SECTION.                                           ELTATCP 
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELTATCP 
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELTATCP 
00025      EJECT                                                        ELTATCP 
00026 ******************************************************************ELTATCP 
00027 *                                                                *ELTATCP 
00028 *    COPYBOOK:   ELTATCP                                         *ELTATCP 
00029 *    DATE:       16-JUN-1987                                     *ELTATCP 
00030 *    AUTHOR:     RICK BARILEAU                                   *ELTATCP 
00031 *    FUNCTION:   THIS WILL DISPLAY ALL OUTPUT ASSOCIATED WITH    *ELTATCP 
00032 *                THE ADDITIONAL TRANSPLANT COVERAGE PROGRAM.     *ELTATCP 
00033 *                                                                *ELTATCP 
00034 *                                                                *ELTATCP 
00035 ******************************************************************ELTATCP 
00036 *                                                                *ELTATCP 
00037 *                      MAINTENANCE HISTORY                       *ELTATCP 
00038 *                                                                *ELTATCP 
00039 *  MOD     DATE     BY  DRPT                ACTION               *ELTATCP 
00040 * ----- ----------- --- ----- ---------------------------------- *ELTATCP 
00041 * 01.00 16-JUN-1987 REB       CREATED                            *ELTATCP 
00042 *                                                                *ELTATCP 
00043 * 01.01 02-SEP-1987 REB       THE USER NOW WANTS G-TABULAR INFO  *ELTATCP 
00044 *                             FOLLOWING THE SENTENCE STATING THAT*ELTATCP 
00045 *                             THEY ARE INCLUDED. OLD CODE IS     *ELTATCP 
00046 *                             COMMENTED OUT IN CASE OF CHANGE    *ELTATCP 
00047 *                                                                *ELTATCP 
00048 * 01.02 13-JAN-1988 REB       THE COBOL NAME FOR PAYMENT METHOD  *ELTATCP 
00049 *                             WAS CHANGED ON CODES MANUAL. THIS  *ELTATCP 
00050 *                             IS A FIX FOR A DISCREPANCY.        *ELTATCP 
00051 *                                                                *ELTATCP 
00052 * 01.03 17-OCT-1990 JPB       CHANGED STORAGE MANAGEMENT.        *ELTATCP 
00053 *                                                                *ELTATCP 
00054 * 01.04 09-NOV-1990 JPB       CHANGED GCG-ADDL-TRNSPLNT-COVRG-   *ELTATCP 
00055 *                             IND REFERENCES TO ACCOMODATE FOR   *ELTATCP 
00056 *                             NEW SIZE IN FIELD.                 *ELTATCP 
00057 *                                                                *ELTATCP 
00058 * 01.05 07-JUN-1991 JPB       ADDED TRANSLATION AND DISPLAY OF   *ELTATCP 
00059 *                             PARTICIPATION INDICATOR            *ELTATCP 
00060 *                             (ADDL-TRNSPLNT-COVRG-IND).         *ELTATCP 
00061 *                                                                *ELTATCP 
00062 * 01.06 18-MAR-1992 BAK       CORRECT PROCESS ROUTINE TO COMPARE *ELTATCP 
00063 *                             '00' INSTEAD OF 'ZERO'.            *ELTATCP 
00064 *       13-AUG-2003 AKK       REGEN FOR ORDER OF COMPILE TEST    *ELTATCP 
00065 ******************************************************************ELTATCP 
00066                                                                   ELTATCP 
00067  DATA DIVISION.                                                   ELTATCP 
00068  WORKING-STORAGE SECTION.                                         ELTATCP 
00069                                                                   ELTATCP 
00070  01  WS-MISC.                                                     ELTATCP 
00071      05  FILLER                   PIC X(29) VALUE                 ELTATCP 
00072      '** ELTATCP WORKING STORAGE **'.                             ELTATCP 
00073      05  WS-POINTER2              POINTER.                        ELTATCP 
00074      05  WS-POINTER3              POINTER.                        ELTATCP 
00075                                                                   ELTATCP 
00076  01  WS-SWITCHES.                                                 ELTATCP 
00077      05  ADDITIONAL-TEXT-SWITCH   PIC X(01) VALUE SPACE.          ELTATCP 
00078          88  ADDITIONAL-TEXT                VALUE 'A'.            ELTATCP 
00079          88  BLANK-LINE-NEEDED              VALUE 'B'.            ELTATCP 
00080      05  CONTINUED-PROCESSING-SW  PIC X(01) VALUE SPACE.          ELTATCP 
00081          88  DONE-PROCESSING                VALUE 'D'.            ELTATCP 
00082          88  PROCESSING-CMF-TEXT            VALUE 'P'.            ELTATCP 
00083      05  WS-GHOB-SWITCH           PIC X(01) VALUE 'N'.            ELTATCP 
00084          88  GHOB-IS-PRESENT                VALUE 'Y'.            ELTATCP 
00085      05  WS-GHOR-SWITCH           PIC X(01) VALUE 'N'.            ELTATCP 
00086          88  GHOR-IS-PRESENT                VALUE 'Y'.            ELTATCP 
00087      05  WS-PERIOD-SWITCH         PIC X(01) VALUE 'N'.            ELTATCP 
00088          88  PERIOD-NEEDED                  VALUE 'Y'.            ELTATCP 
00089      05  WS-TABULAR-SWITCH        PIC X(01) VALUE 'N'.            ELTATCP 
00090          88  TABULAR-IS-UNDEFINED           VALUE 'Y'.            ELTATCP 
00091      05  APPROVAL-SOURCE-SW       PIC X     VALUE SPACE.          ELTATCP 
00092          88  NOT-HOLDING-APPROVAL-SRCE      VALUE 'N'.            ELTATCP 
00093          88  HOLDING-APPROVAL-SOURCE        VALUE 'H'.            ELTATCP 
00094                                                                   ELTATCP 
00095  01  WS-HOLD-AREA.                                                ELTATCP 
00096      05  WS-GHOB-PROV-ID               PIC X(06)  VALUE SPACES.   ELTATCP 
00097      05  WS-GHOB-PROV-SLOT-NO  COMP-3  PIC S9(07) VALUE +0.       ELTATCP 
00098      05  WS-GHOR-PROV-ID               PIC X(06)  VALUE SPACES.   ELTATCP 
00099      05  WS-GHOR-PROV-SLOT-NO  COMP-3  PIC S9(07) VALUE +0.       ELTATCP 
00100                                                                   ELTATCP 
00101 **************************************************************    ELTATCP 
00102 ***                   PROGRAM CONSTANTS                           ELTATCP 
00103 **************************************************************    ELTATCP 
00104      05  WS-GCCP                  PIC X(06) VALUE '#GCCP '.       ELTATCP 
00105      05  WS-GHOB                  PIC X(06) VALUE '#GHOB '.       ELTATCP 
00106      05  WS-GHOR                  PIC X(06) VALUE '#GHOR '.       ELTATCP 
00107      05  WS-GROUP                 PIC X(06) VALUE 'GROUP '.       ELTATCP 
00108      05  WS-INST                  PIC X(13) VALUE 'INSTITUTIONAL'.ELTATCP 
00109      05  WS-PROF                  PIC X(13) VALUE 'PROFESSIONAL '.ELTATCP 
00110                                                                   ELTATCP 
00111 ***************************************************************   ELTATCP 
00112 ***                   HEADER LINE                                 ELTATCP 
00113 ***************************************************************   ELTATCP 
00114      05  WS-HEADER-LINE.                                          ELTATCP 
00115          10  FILLER               PIC X(14) VALUE SPACES.         ELTATCP 
00116          10  FILLER               PIC X(39) VALUE                 ELTATCP 
00117          'ADDITIONAL TRANSPLANT COVERAGE PROGRAM '.               ELTATCP 
00118          10  WS-HDR-LINE-BCBS     PIC X(13) VALUE SPACES.         ELTATCP 
00119          10  FILLER               PIC X(13) VALUE SPACES.         ELTATCP 
00120                                                                   ELTATCP 
00121 **************************************************************    ELTATCP 
00122 ***                   SCREEN BODY LINES                           ELTATCP 
00123 **************************************************************    ELTATCP 
00124  01  WS-SCREEN-LINE-AREA.                                         ELTATCP 
00125      05  WS-APPRVL-SRCE-LINE.                                     ELTATCP 
00126          10  FILLER               PIC X(52) VALUE                 ELTATCP 
00127          'THE ADDITIONAL TRANSPLANT COVERAGE PROGRAM SERVICES '.  ELTATCP 
00128          10  FILLER               PIC X(20) VALUE                 ELTATCP 
00129          'MUST BE APPROVED BY '.                                  ELTATCP 
00130          10  FILLER               PIC X(07) VALUE SPACES.         ELTATCP 
00131                                                                   ELTATCP 
00132      05  WS-PYMT-LINE.                                            ELTATCP 
00133          10  FILLER               PIC X(21) VALUE                 ELTATCP 
00134          'BENEFITS ARE PAID AT '.                                 ELTATCP 
00135          10  FILLER               PIC X(58) VALUE SPACES.         ELTATCP 
00136                                                                   ELTATCP 
00137      05  WS-ALT-PRIC-LINE-BC.                                     ELTATCP 
00138          10  FILLER               PIC X(26) VALUE                 ELTATCP 
00139          'THE ALTERNATE PRICING FOR '.                            ELTATCP 
00140          10  FILLER               PIC X(26) VALUE                 ELTATCP 
00141          'INSTITUTIONAL SERVICES IS '.                            ELTATCP 
00142          10  FILLER               PIC X(27) VALUE SPACES.         ELTATCP 
00143                                                                   ELTATCP 
00144      05  WS-ALT-PRIC-LINE-BS.                                     ELTATCP 
00145          10  FILLER               PIC X(26) VALUE                 ELTATCP 
00146          'THE ALTERNATE PRICING FOR '.                            ELTATCP 
00147          10  FILLER               PIC X(25) VALUE                 ELTATCP 
00148          'PROFESSIONAL SERVICES IS '.                             ELTATCP 
00149          10  FILLER               PIC X(28) VALUE SPACES.         ELTATCP 
00150                                                                   ELTATCP 
00151 **************************************************************    ELTATCP 
00152 ** SPECIAL MESSAGE FOR THE VOLUNTARY AND NOT APPLICABLE CASES     ELTATCP 
00153 ** ALSO THE FIXED TEXT FOR TABULARS GHOB,GHOR                     ELTATCP 
00154 **************************************************************    ELTATCP 
00155      05  WS-NOT-APPLICABLE-MSG.                                   ELTATCP 
00156          10  FILLER               PIC  X(50) VALUE                ELTATCP 
00157          'THE ADDITIONAL TRANSPLANT COVERAGE PROGRAM IS NOT '.    ELTATCP 
00158          10  FILLER               PIC  X(29) VALUE                ELTATCP 
00159          'APPLICABLE.'.                                           ELTATCP 
00160                                                                   ELTATCP 
00161      05  WS-VOLUNTARY-MSG.                                        ELTATCP 
00162          10  FILLER               PIC  X(46) VALUE                ELTATCP 
00163          'THE ADDITIONAL TRANSPLANT COVERAGE PROGRAM IS '.        ELTATCP 
00164          10  FILLER               PIC  X(33) VALUE                ELTATCP 
00165          'VOLUNTARY.'.                                            ELTATCP 
00166                                                                   ELTATCP 
00167      05  WS-PARTICIPATION-LINE.                                   ELTATCP 
00168          10  FILLER               PIC X(79) VALUE                 ELTATCP 
00169          'THE ADDITIONAL TRANSPLANT PROGRAM APPLIES TO '.         ELTATCP 
00170                                                                   ELTATCP 
00171      05  WS-DISCLAIMER-MSG.                                       ELTATCP 
00172          10  FILLER               PIC X(79) VALUE                 ELTATCP 
00173          '*** SUBJECT TO OTHER CONTRACT LIMITATIONS ***'.         ELTATCP 
00174                                                                   ELTATCP 
00175      05  WS-SPEC-SERV-MSG.                                        ELTATCP 
00176          10  FILLER               PIC X(79) VALUE                 ELTATCP 
00177          'THERE ARE SPECIAL RELATED SERVICES INCLUDED IN THIS COSTELTATCP 
00178 -        ' CONTAINMENT PROGRAM.'.                                 ELTATCP 
00179                                                                   ELTATCP 
00180      05  WS-SPEC-PROC-MSG.                                        ELTATCP 
00181          10  FILLER               PIC X(79) VALUE                 ELTATCP 
00182          'THERE ARE SPECIAL PROCEDURES INCLUDED IN THIS COST CONTAELTATCP 
00183 -        'INMENT PROGRAM.'.                                       ELTATCP 
00184                                                                   ELTATCP 
00185 /                                                                 ELTATCP 
00186  LINKAGE SECTION.                                                 ELTATCP 
00187  01  DFHCOMMAREA.                                                 ELTATCP 
00188      COPY ELSCOMMC.                                               ELTATCP 
00189 /                                                                 ELTATCP 
00190      COPY ELSCIA2C.                                               ELTATCP 
00191 /                                                                 ELTATCP 
00192      COPY ELSCMDSC.                                               ELTATCP 
00193 /                                                                 ELTATCP 
00194      COPY ELSCMIFC.                                               ELTATCP 
00195 /                                                                 ELTATCP 
00196      COPY ELSIOPMC.                                               ELTATCP 
00197 /                                                                 ELTATCP 
00198      COPY ELSKEYSC.                                               ELTATCP 
00199 /                                                                 ELTATCP 
00200      COPY ELSOUTPC.                                               ELTATCP 
00201 /                                                                 ELTATCP 
00202      COPY ELSSRTPC.                                               ELTATCP 
00203 /                                                                 ELTATCP 
00204      COPY ELSTCWAC.                                               ELTATCP 
00205 /                                                                 ELTATCP 
00206      COPY ELSSSCBC.                                               ELTATCP 
00207 /                                                                 ELTATCP 
00208  01  ELR-GRP-REC-AREA.                                            ELTATCP 
00209      COPY GCGROUPC.                                               ELTATCP 
00210 /                                                                 ELTATCP 
00211  01  GCCP-TABULAR-REC-AREA.                                       ELTATCP 
00212      COPY GCTGCCPC.                                               ELTATCP 
00213      EJECT                                                        ELTATCP 
00214  PROCEDURE DIVISION.                                              ELTATCP 
00215 ************************************************************      ELTATCP 
00216 *                                                          *      ELTATCP 
00217 *                    PROCEDURE DIVISION                    *      ELTATCP 
00218 *                                                          *      ELTATCP 
00219 ************************************************************      ELTATCP 
00220                                                                   ELTATCP 
00221                                                                   ELTATCP 
00222 ************************************************************      ELTATCP 
00223 *                                                          *      ELTATCP 
00224 *        ADDITIONAL TRANSPLANT COVERAGE PROGRAM            *      ELTATCP 
00225 *                                                          *      ELTATCP 
00226 ************************************************************      ELTATCP 
00227  ADDITIONAL-TRANSPLANT-COVERAGE.                                  ELTATCP 
00228      PERFORM INITIALIZE-RTN.                                      ELTATCP 
00229      PERFORM PROCESS.                                             ELTATCP 
00230      GOBACK.                                                      ELTATCP 
00231                                                                   ELTATCP 
00232                                                                   ELTATCP 
00233 ************************************************************      ELTATCP 
00234 *                                                          *      ELTATCP 
00235 *        INITIALIZE-RTN                                    *      ELTATCP 
00236 *                                                          *      ELTATCP 
00237 ************************************************************      ELTATCP 
00238  INITIALIZE-RTN.                                                  ELTATCP 
00239      PERFORM ESTABLISH-ADDRESS-OF-CNTL-BLKS.                      ELTATCP 
00240      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTATCP 
00241                                                                   ELTATCP 
00242                                                                   ELTATCP 
00243 ************************************************************      ELTATCP 
00244 *                                                          *      ELTATCP 
00245 *        ESTABLISH ADDRESS OF CNTL BLKS                    *      ELTATCP 
00246 *                                                          *      ELTATCP 
00247 ************************************************************      ELTATCP 
00248  ESTABLISH-ADDRESS-OF-CNTL-BLKS.                                  ELTATCP 
00249      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTATCP 
00250      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTATCP 
00251      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTATCP 
00252                                                                   ELTATCP 
00253                                                                   ELTATCP 
00254 ************************************************************      ELTATCP 
00255 *                                                          *      ELTATCP 
00256 *        CHECK FOR VALID COMMAREA                          *      ELTATCP 
00257 *                                                          *      ELTATCP 
00258 ************************************************************      ELTATCP 
00259  CHECK-FOR-VALID-COMMAREA.                                        ELTATCP 
00260      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTATCP 
00261          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTATCP 
00262                                                                   ELTATCP 
00263                                                                   ELTATCP 
00264 ************************************************************      ELTATCP 
00265 *                                                          *      ELTATCP 
00266 *        SIGNAL INVALID COMMAREA                           *      ELTATCP 
00267 *                                                          *      ELTATCP 
00268 ************************************************************      ELTATCP 
00269  SIGNAL-INVALID-COMMAREA.                                         ELTATCP 
00270      EXEC CICS ABEND                                              ELTATCP 
00271                ABCODE('EL01')                                     ELTATCP 
00272         END-EXEC.                                                 ELTATCP 
00273      EJECT                                                        ELTATCP 
00274                                                                   ELTATCP 
00275                                                                   ELTATCP 
00276 ************************************************************      ELTATCP 
00277 *                                                          *      ELTATCP 
00278 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTATCP 
00279 *                                                          *      ELTATCP 
00280 ************************************************************      ELTATCP 
00281  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTATCP 
00282      IF ECA-CIA-PTR = NULL                                        ELTATCP 
00283          PERFORM SIGNAL-INVALID-CIA                               ELTATCP 
00284      ELSE                                                         ELTATCP 
00285          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTATCP 
00286                                                                   ELTATCP 
00287                                                                   ELTATCP 
00288 ************************************************************      ELTATCP 
00289 *                                                          *      ELTATCP 
00290 *        SIGNAL INVALID CIA                                *      ELTATCP 
00291 *                                                          *      ELTATCP 
00292 ************************************************************      ELTATCP 
00293  SIGNAL-INVALID-CIA.                                              ELTATCP 
00294      EXEC CICS ABEND                                              ELTATCP 
00295                ABCODE('EL02')                                     ELTATCP 
00296         END-EXEC.                                                 ELTATCP 
00297                                                                   ELTATCP 
00298                                                                   ELTATCP 
00299 ************************************************************      ELTATCP 
00300 *                                                          *      ELTATCP 
00301 *        ESTABLISH ADDRESS OF CIA                          *      ELTATCP 
00302 *                                                          *      ELTATCP 
00303 ************************************************************      ELTATCP 
00304  ESTABLISH-ADDRESS-OF-CIA.                                        ELTATCP 
00305      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTATCP 
00306                        ADDRESS OF                                 ELTATCP 
00307          CIA-ELS-COMMON-INTERFACE-AREA.                           ELTATCP 
00308      EJECT                                                        ELTATCP 
00309                                                                   ELTATCP 
00310                                                                   ELTATCP 
00311 ************************************************************      ELTATCP 
00312 *                                                          *      ELTATCP 
00313 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTATCP 
00314 *                                                          *      ELTATCP 
00315 ************************************************************      ELTATCP 
00316  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTATCP 
00317      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTATCP 
00318      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTATCP 
00319                            ADDRESS OF                             ELTATCP 
00320          SSB-SELECTOR-STATUS-CTL-BLK.                             ELTATCP 
00321      IF CIA-RC-PTR-NULL                                           ELTATCP 
00322          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTATCP 
00323                                                                   ELTATCP 
00324                                                                   ELTATCP 
00325 ************************************************************      ELTATCP 
00326 *                                                          *      ELTATCP 
00327 *        SIGNAL UNALLOC AREA ERROR                         *      ELTATCP 
00328 *                                                          *      ELTATCP 
00329 ************************************************************      ELTATCP 
00330  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTATCP 
00331      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTATCP 
00332      PERFORM SIGNAL-ABEND.                                        ELTATCP 
00333                                                                   ELTATCP 
00334                                                                   ELTATCP 
00335 ************************************************************      ELTATCP 
00336 *                                                          *      ELTATCP 
00337 *        SIGNAL ABEND                                      *      ELTATCP 
00338 *                                                          *      ELTATCP 
00339 ************************************************************      ELTATCP 
00340  SIGNAL-ABEND.                                                    ELTATCP 
00341      EXEC CICS ABEND                                              ELTATCP 
00342                ABCODE(CIA-ABCODE)                                 ELTATCP 
00343         END-EXEC.                                                 ELTATCP 
00344      EJECT                                                        ELTATCP 
00345                                                                   ELTATCP 
00346                                                                   ELTATCP 
00347 ************************************************************      ELTATCP 
00348 *                                                          *      ELTATCP 
00349 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTATCP 
00350 *                                                          *      ELTATCP 
00351 ************************************************************      ELTATCP 
00352  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTATCP 
00353      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTATCP 
00354      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTATCP 
00355      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTATCP 
00356      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTATCP 
00357      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTATCP 
00358      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTATCP 
00359      PERFORM ESTABLISH-ADDRESSABILITY-OF-CS.                      ELTATCP 
00360                                                                   ELTATCP 
00361                                                                   ELTATCP 
00362 ************************************************************      ELTATCP 
00363 *                                                          *      ELTATCP 
00364 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTATCP 
00365 *                                                          *      ELTATCP 
00366 ************************************************************      ELTATCP 
00367  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTATCP 
00368      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTATCP 
00369      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTATCP 
00370                           ADDRESS OF                              ELTATCP 
00371          CMF-CODES-MANUAL-INTERFACE.                              ELTATCP 
00372      IF CIA-RC-PTR-NULL                                           ELTATCP 
00373          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTATCP 
00374      EJECT                                                        ELTATCP 
00375                                                                   ELTATCP 
00376                                                                   ELTATCP 
00377 ************************************************************      ELTATCP 
00378 *                                                          *      ELTATCP 
00379 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTATCP 
00380 *                                                          *      ELTATCP 
00381 ************************************************************      ELTATCP 
00382  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTATCP 
00383      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTATCP 
00384      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTATCP 
00385                            ADDRESS OF                             ELTATCP 
00386          COF-OUTPUT-INTERFACE.                                    ELTATCP 
00387      IF CIA-RC-PTR-NULL                                           ELTATCP 
00388          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTATCP 
00389      EJECT                                                        ELTATCP 
00390                                                                   ELTATCP 
00391                                                                   ELTATCP 
00392 ************************************************************      ELTATCP 
00393 *                                                          *      ELTATCP 
00394 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTATCP 
00395 *                                                          *      ELTATCP 
00396 ************************************************************      ELTATCP 
00397  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTATCP 
00398      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTATCP 
00399      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTATCP 
00400                            ADDRESS OF                             ELTATCP 
00401          SRP-SUBROUTINE-PARAMETERS.                               ELTATCP 
00402      IF CIA-RC-PTR-NULL                                           ELTATCP 
00403          PERFORM ALLOCATE-SUBROUTINE-AREA.                        ELTATCP 
00404      EJECT                                                        ELTATCP 
00405                                                                   ELTATCP 
00406                                                                   ELTATCP 
00407 ************************************************************      ELTATCP 
00408 *                                                          *      ELTATCP 
00409 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTATCP 
00410 *                                                          *      ELTATCP 
00411 ************************************************************      ELTATCP 
00412  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTATCP 
00413      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTATCP 
00414      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTATCP 
00415                            ADDRESS OF                             ELTATCP 
00416          TCAR-COMPRESSION-WORK-AREA.                              ELTATCP 
00417      IF CIA-RC-PTR-NULL                                           ELTATCP 
00418          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTATCP 
00419      EJECT                                                        ELTATCP 
00420                                                                   ELTATCP 
00421                                                                   ELTATCP 
00422 ************************************************************      ELTATCP 
00423 *                                                          *      ELTATCP 
00424 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTATCP 
00425 *                                                          *      ELTATCP 
00426 ************************************************************      ELTATCP 
00427  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTATCP 
00428      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTATCP 
00429      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTATCP 
00430                        ADDRESS OF                                 ELTATCP 
00431          KWA-FILE-KEY-WORK-AREA.                                  ELTATCP 
00432      IF CIA-RC-PTR-NULL                                           ELTATCP 
00433          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTATCP 
00434      EJECT                                                        ELTATCP 
00435                                                                   ELTATCP 
00436                                                                   ELTATCP 
00437 ************************************************************      ELTATCP 
00438 *                                                          *      ELTATCP 
00439 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELTATCP 
00440 *                                                          *      ELTATCP 
00441 ************************************************************      ELTATCP 
00442  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTATCP 
00443      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTATCP 
00444      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTATCP 
00445                        ADDRESS OF ELR-GRP-REC-AREA.               ELTATCP 
00446      IF CIA-RC-PTR-NULL                                           ELTATCP 
00447          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTATCP 
00448      EJECT                                                        ELTATCP 
00449                                                                   ELTATCP 
00450                                                                   ELTATCP 
00451 ************************************************************      ELTATCP 
00452 *                                                          *      ELTATCP 
00453 *        ESTABLISH ADDRESSABILITY OF CST CONTAINMENT PROGRA*      ELTATCP 
00454 *                                                          *      ELTATCP 
00455 ************************************************************      ELTATCP 
00456  ESTABLISH-ADDRESSABILITY-OF-CS.                                  ELTATCP 
00457      SET CIA-GCTABULR-DDN TO TRUE.                                ELTATCP 
00458      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTATCP 
00459                        ADDRESS OF                                 ELTATCP 
00460          GCCP-TABULAR-REC-AREA.                                   ELTATCP 
00461      IF CIA-RC-PTR-NULL                                           ELTATCP 
00462          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTATCP 
00463                                                                   ELTATCP 
00464                                                                   ELTATCP 
00465 ************************************************************      ELTATCP 
00466 *                                                          *      ELTATCP 
00467 *        ALLOCATE SUBROUTINE AREA                          *      ELTATCP 
00468 *                                                          *      ELTATCP 
00469 ************************************************************      ELTATCP 
00470  ALLOCATE-SUBROUTINE-AREA.                                        ELTATCP 
00471      SET CIA-STG-GETMAIN  TO  TRUE.                               ELTATCP 
00472      EXEC CICS LINK PROGRAM ('ELUSTGMG')                          ELTATCP 
00473                     COMMAREA (DFHCOMMAREA)                        ELTATCP 
00474                     LENGTH (LENGTH OF DFHCOMMAREA)                ELTATCP 
00475                     END-EXEC.                                     ELTATCP 
00476      EJECT                                                        ELTATCP 
00477                                                                   ELTATCP 
00478                                                                   ELTATCP 
00479 ************************************************************      ELTATCP 
00480 *                                                          *      ELTATCP 
00481 *        PROCESS                                           *      ELTATCP 
00482 *                                                          *      ELTATCP 
00483 ************************************************************      ELTATCP 
00484  PROCESS.                                                         ELTATCP 
00485      IF GCG-ADDL-TRNSPLNT-COVRG-IND  =  '00'                      ELTATCP 
00486          PERFORM GENERATE-NOT-APPLICABLE-MESSAG                   ELTATCP 
00487      ELSE IF GCG-ADDL-TRNSPLNT-COVRG-IND  =  '08'                 ELTATCP 
00488          PERFORM GENERATE-VOLUNTARY-MESSAGE                       ELTATCP 
00489      ELSE                                                         ELTATCP 
00490          PERFORM GENERATE-ATCP-TEXT.                              ELTATCP 
00491      PERFORM TERMINATE-OUTPUT.                                    ELTATCP 
00492                                                                   ELTATCP 
00493                                                                   ELTATCP 
00494 ************************************************************      ELTATCP 
00495 *                                                          *      ELTATCP 
00496 *        GENERATE NOT APPLICABLE MESSAGE                   *      ELTATCP 
00497 *                                                          *      ELTATCP 
00498 ************************************************************      ELTATCP 
00499  GENERATE-NOT-APPLICABLE-MESSAG.                                  ELTATCP 
00500      PERFORM SET-UP-OUTPUT-SUBCRIPTS.                             ELTATCP 
00501      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTATCP 
00502      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTATCP 
00503      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTATCP 
00504          (COF-NBR-DTL-LINES).                                     ELTATCP 
00505      PERFORM EJECT-NEW-PAGE.                                      ELTATCP 
00506      EJECT                                                        ELTATCP 
00507                                                                   ELTATCP 
00508                                                                   ELTATCP 
00509 ************************************************************      ELTATCP 
00510 *                                                          *      ELTATCP 
00511 *        GENERATE VOLUNTARY MESSAGE                        *      ELTATCP 
00512 *                                                          *      ELTATCP 
00513 ************************************************************      ELTATCP 
00514  GENERATE-VOLUNTARY-MESSAGE.                                      ELTATCP 
00515      PERFORM SET-UP-OUTPUT-SUBCRIPTS.                             ELTATCP 
00516      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTATCP 
00517      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTATCP 
00518      MOVE WS-VOLUNTARY-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).   ELTATCP 
00519      PERFORM EJECT-NEW-PAGE.                                      ELTATCP 
00520                                                                   ELTATCP 
00521                                                                   ELTATCP 
00522 ************************************************************      ELTATCP 
00523 *                                                          *      ELTATCP 
00524 *        SET UP OUTPUT SUBCRIPTS                           *      ELTATCP 
00525 *                                                          *      ELTATCP 
00526 ************************************************************      ELTATCP 
00527  SET-UP-OUTPUT-SUBCRIPTS.                                         ELTATCP 
00528      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTATCP 
00529      MOVE +2 TO COF-NBR-HDR-LINES.                                ELTATCP 
00530      EJECT                                                        ELTATCP 
00531                                                                   ELTATCP 
00532                                                                   ELTATCP 
00533 ************************************************************      ELTATCP 
00534 *                                                          *      ELTATCP 
00535 *        EJECT NEW PAGE                                    *      ELTATCP 
00536 *                                                          *      ELTATCP 
00537 ************************************************************      ELTATCP 
00538  EJECT-NEW-PAGE.                                                  ELTATCP 
00539      SET COF-NEW-PAGE    TO TRUE.                                 ELTATCP 
00540      MOVE WS-HEADER-LINE TO COF-HDR-LINE (COF-NBR-HDR-LINES).     ELTATCP 
00541      PERFORM LINK-TO-OUTPUT.                                      ELTATCP 
00542      EJECT                                                        ELTATCP 
00543                                                                   ELTATCP 
00544                                                                   ELTATCP 
00545 ************************************************************      ELTATCP 
00546 *                                                          *      ELTATCP 
00547 *        GENERATE ATCP TEXT                                *      ELTATCP 
00548 *                                                          *      ELTATCP 
00549 ************************************************************      ELTATCP 
00550  GENERATE-ATCP-TEXT.                                              ELTATCP 
00551      SET WS-POINTER2 TO NULL.                                     ELTATCP 
00552      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTATCP 
00553      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTATCP 
00554                            WS-POINTER2.                           ELTATCP 
00555      SET WS-POINTER3 TO NULL.                                     ELTATCP 
00556      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTATCP 
00557      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTATCP 
00558                            WS-POINTER3.                           ELTATCP 
00559      PERFORM SEARCH-FOR-GCCP-TABULAR.                             ELTATCP 
00560      IF SSB-PROV-CLASS-INST OR                                    ELTATCP 
00561                 SSB-PROV-CLASS-BOTH                               ELTATCP 
00562          PERFORM GENERATE-INSTITUTIONAL-TEXT.                     ELTATCP 
00563      IF SSB-PROV-CLASS-PROF OR                                    ELTATCP 
00564                 SSB-PROV-CLASS-BOTH                               ELTATCP 
00565          PERFORM GENERATE-PROFESSIONAL-TEXT.                      ELTATCP 
00566      EJECT                                                        ELTATCP 
00567                                                                   ELTATCP 
00568                                                                   ELTATCP 
00569 ************************************************************      ELTATCP 
00570 *                                                          *      ELTATCP 
00571 *        SEARCH FOR GCCP TABULAR                           *      ELTATCP 
00572 *                                                          *      ELTATCP 
00573 ************************************************************      ELTATCP 
00574  SEARCH-FOR-GCCP-TABULAR.                                         ELTATCP 
00575      SET GCG-INDEX TO +1.                                         ELTATCP 
00576      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTATCP 
00577         AT END                                                    ELTATCP 
00578              MOVE ZEROES TO KWA-PROVISION-SLOT-NO                 ELTATCP 
00579         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GCCP                     ELTATCP 
00580              MOVE GCG-TAB-ID (GCG-INDEX)                          ELTATCP 
00581                  TO KWA-PROVISION-ID                              ELTATCP 
00582              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTATCP 
00583                  TO KWA-PROVISION-SLOT-NO                         ELTATCP 
00584         END-SEARCH.                                               ELTATCP 
00585      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELTATCP 
00586          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTATCP 
00587      PERFORM GET-GCCP-TABULAR.                                    ELTATCP 
00588      PERFORM SEARCH-THE-GSS-ENTRY.                                ELTATCP 
00589      EJECT                                                        ELTATCP 
00590                                                                   ELTATCP 
00591                                                                   ELTATCP 
00592 ************************************************************      ELTATCP 
00593 *                                                          *      ELTATCP 
00594 *        GENERATE INSTITUTIONAL TEXT                       *      ELTATCP 
00595 *                                                          *      ELTATCP 
00596 ************************************************************      ELTATCP 
00597  GENERATE-INSTITUTIONAL-TEXT.                                     ELTATCP 
00598      PERFORM SET-UP-OUTPUT-SUBCRIPTS.                             ELTATCP 
00599      MOVE WS-INST TO WS-HDR-LINE-BCBS.                            ELTATCP 
00600      PERFORM EJECT-NEW-PAGE.                                      ELTATCP 
00601      PERFORM TRANSLATE-PARTICIPATION-INDICA.                      ELTATCP 
00602      IF GSS-AT-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL ZEROES   ELTATCP 
00603          AND                                                      ELTATCP 
00604                 SPACES AND LOW-VALUES                             ELTATCP 
00605          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTATCP 
00606      IF GSS-AT-PAYMENT-METHOD (GSS-INDEX) NOT EQUAL ZEROES        ELTATCP 
00607          AND                                                      ELTATCP 
00608                 SPACES AND LOW-VALUES                             ELTATCP 
00609          PERFORM TRANSLATE-PAYMENT-METHOD.                        ELTATCP 
00610      IF GSS-AT-BC-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL ZEROES   ELTATCP 
00611          AND                                                      ELTATCP 
00612                 SPACES AND LOW-VALUES                             ELTATCP 
00613          PERFORM TRANSLATE-BC-ALT-PRIC-METH.                      ELTATCP 
00614      SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTATCP 
00615      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTATCP 
00616      IF GSS-AT-BC-CALC-METHOD (GSS-INDEX) NOT EQUAL ZEROES        ELTATCP 
00617          AND                                                      ELTATCP 
00618                 SPACES AND LOW-VALUES                             ELTATCP 
00619          PERFORM TRANSLATE-BC-CALC-METHOD.                        ELTATCP 
00620      PERFORM GENERATE-ASSOCIATED-TABULAR-DA.                      ELTATCP 
00621      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTATCP 
00622                                                                   ELTATCP 
00623                                                                   ELTATCP 
00624 ************************************************************      ELTATCP 
00625 *                                                          *      ELTATCP 
00626 *        SIGNAL UNDEFINED TABULAR ERROR                    *      ELTATCP 
00627 *                                                          *      ELTATCP 
00628 ************************************************************      ELTATCP 
00629  SIGNAL-UNDEFINED-TABULAR-ERROR.                                  ELTATCP 
00630      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTATCP 
00631      PERFORM SIGNAL-ABEND.                                        ELTATCP 
00632      EJECT                                                        ELTATCP 
00633                                                                   ELTATCP 
00634                                                                   ELTATCP 
00635 ************************************************************      ELTATCP 
00636 *                                                          *      ELTATCP 
00637 *        SEARCH THE GSS ENTRY                              *      ELTATCP 
00638 *                                                          *      ELTATCP 
00639 ************************************************************      ELTATCP 
00640  SEARCH-THE-GSS-ENTRY.                                            ELTATCP 
00641      SET GSS-INDEX TO 1.                                          ELTATCP 
00642      SEARCH GSS-ENTRY                                             ELTATCP 
00643          AT END                                                   ELTATCP 
00644               SET TABULAR-IS-UNDEFINED TO TRUE                    ELTATCP 
00645          WHEN GSS-AT-PROG-CODE-CHR (GSS-INDEX)                    ELTATCP 
00646               CONTINUE                                            ELTATCP 
00647          END-SEARCH.                                              ELTATCP 
00648      IF TABULAR-IS-UNDEFINED                                      ELTATCP 
00649          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTATCP 
00650      EJECT                                                        ELTATCP 
00651                                                                   ELTATCP 
00652                                                                   ELTATCP 
00653 ************************************************************      ELTATCP 
00654 *                                                          *      ELTATCP 
00655 *        GENERATE PROFESSIONAL TEXT                        *      ELTATCP 
00656 *                                                          *      ELTATCP 
00657 ************************************************************      ELTATCP 
00658  GENERATE-PROFESSIONAL-TEXT.                                      ELTATCP 
00659      PERFORM SET-UP-OUTPUT-SUBCRIPTS.                             ELTATCP 
00660      MOVE WS-PROF TO WS-HDR-LINE-BCBS.                            ELTATCP 
00661      PERFORM EJECT-NEW-PAGE.                                      ELTATCP 
00662      PERFORM TRANSLATE-PARTICIPATION-INDICA.                      ELTATCP 
00663      IF GSS-AT-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL ZEROES   ELTATCP 
00664          AND                                                      ELTATCP 
00665                 SPACES AND LOW-VALUES                             ELTATCP 
00666          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTATCP 
00667      IF GSS-AT-PAYMENT-METHOD (GSS-INDEX) NOT EQUAL ZEROES        ELTATCP 
00668          AND                                                      ELTATCP 
00669                 SPACES AND LOW-VALUES                             ELTATCP 
00670          PERFORM TRANSLATE-PAYMENT-METHOD.                        ELTATCP 
00671      IF GSS-AT-BS-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL ZEROES   ELTATCP 
00672          AND                                                      ELTATCP 
00673                 SPACES AND LOW-VALUES                             ELTATCP 
00674          PERFORM TRANSLATE-BS-ALT-PRIC-METH.                      ELTATCP 
00675      SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTATCP 
00676      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTATCP 
00677      IF GSS-AT-BS-CALC-METHOD (GSS-INDEX) NOT EQUAL ZEROES        ELTATCP 
00678          AND                                                      ELTATCP 
00679                 SPACES AND LOW-VALUES                             ELTATCP 
00680          PERFORM TRANSLATE-BS-CALC-METHOD.                        ELTATCP 
00681      PERFORM GENERATE-ASSOCIATED-TABULAR-DA.                      ELTATCP 
00682      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTATCP 
00683      EJECT                                                        ELTATCP 
00684                                                                   ELTATCP 
00685                                                                   ELTATCP 
00686 ************************************************************      ELTATCP 
00687 *                                                          *      ELTATCP 
00688 *        GENERATE DISCLAIMER MESSAGE                       *      ELTATCP 
00689 *                                                          *      ELTATCP 
00690 ************************************************************      ELTATCP 
00691  GENERATE-DISCLAIMER-MESSAGE.                                     ELTATCP 
00692      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTATCP 
00693      MOVE WS-DISCLAIMER-MSG TO COF-DTL-LINE                       ELTATCP 
00694          (COF-NBR-DTL-LINES).                                     ELTATCP 
00695      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTATCP 
00696      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTATCP 
00697      PERFORM LINK-TO-OUTPUT.                                      ELTATCP 
00698      EJECT                                                        ELTATCP 
00699                                                                   ELTATCP 
00700                                                                   ELTATCP 
00701 ************************************************************      ELTATCP 
00702 *                                                          *      ELTATCP 
00703 *        TRANSLATE PARTICIPATION INDICATOR                 *      ELTATCP 
00704 *                                                          *      ELTATCP 
00705 ************************************************************      ELTATCP 
00706  TRANSLATE-PARTICIPATION-INDICA.                                  ELTATCP 
00707      INITIALIZE TCAR-FROM-AREA.                                   ELTATCP 
00708      INITIALIZE WS-PERIOD-SWITCH.                                 ELTATCP 
00709      MOVE +1 TO TCAR-FROM-SUB.                                    ELTATCP 
00710      MOVE WS-PARTICIPATION-LINE TO TCAR-FROM-LINE                 ELTATCP 
00711          (TCAR-FROM-SUB).                                         ELTATCP 
00712      ADD  +1 TO TCAR-FROM-SUB.                                    ELTATCP 
00713      SET BLANK-LINE-NEEDED TO TRUE.                               ELTATCP 
00714      MOVE GCG-ADDL-TRNSPLNT-COVRG-IND TO CMF-CODE-VALUE.          ELTATCP 
00715      MOVE    'ADDL-TRNSPLNT-COVRG-IND' TO                         ELTATCP 
00716          CMF-ELEMENT-SYSTEM-NAME.                                 ELTATCP 
00717      PERFORM CALL-GROUP-CODES-MANUAL-INTERF.                      ELTATCP 
00718      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTATCP 
00719      EJECT                                                        ELTATCP 
00720                                                                   ELTATCP 
00721                                                                   ELTATCP 
00722 ************************************************************      ELTATCP 
00723 *                                                          *      ELTATCP 
00724 *        TRANSLATE APPROVAL SOURCE                         *      ELTATCP 
00725 *                                                          *      ELTATCP 
00726 ************************************************************      ELTATCP 
00727  TRANSLATE-APPROVAL-SOURCE.                                       ELTATCP 
00728      INITIALIZE TCAR-FROM-AREA.                                   ELTATCP 
00729      INITIALIZE WS-PERIOD-SWITCH.                                 ELTATCP 
00730      MOVE +1 TO TCAR-FROM-SUB.                                    ELTATCP 
00731      MOVE WS-APPRVL-SRCE-LINE TO TCAR-FROM-LINE (TCAR-FROM-SUB).  ELTATCP 
00732      ADD  +1 TO TCAR-FROM-SUB.                                    ELTATCP 
00733      SET BLANK-LINE-NEEDED TO TRUE.                               ELTATCP 
00734      SET PERIOD-NEEDED TO TRUE.                                   ELTATCP 
00735      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTATCP 
00736      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTATCP 
00737                            ADDRESS OF CMF-DESCR.                  ELTATCP 
00738      IF CIA-RC-PTR-NULL                                           ELTATCP 
00739          PERFORM GET-APPROVAL-TRANSLATION                         ELTATCP 
00740      ELSE                                                         ELTATCP 
00741          PERFORM USE-EXISTING-APPROVAL-TRANSLAT.                  ELTATCP 
00742      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTATCP 
00743      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTATCP 
00744      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTATCP 
00745                            WS-POINTER3.                           ELTATCP 
00746      EJECT                                                        ELTATCP 
00747                                                                   ELTATCP 
00748                                                                   ELTATCP 
00749 ************************************************************      ELTATCP 
00750 *                                                          *      ELTATCP 
00751 *        GET APPROVAL TRANSLATION                          *      ELTATCP 
00752 *                                                          *      ELTATCP 
00753 ************************************************************      ELTATCP 
00754  GET-APPROVAL-TRANSLATION.                                        ELTATCP 
00755      MOVE 'AT-APPROVAL-SOURCE-IND' TO                             ELTATCP 
00756          CMF-ELEMENT-SYSTEM-NAME.                                 ELTATCP 
00757      MOVE GSS-AT-APPROVAL-SOURCE-IND (GSS-INDEX) TO               ELTATCP 
00758          CMF-CODE-VALUE.                                          ELTATCP 
00759      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTATCP 
00760      SET HOLDING-APPROVAL-SOURCE TO TRUE.                         ELTATCP 
00761      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTATCP 
00762      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTATCP 
00763                            ADDRESS OF CMF-DESCR.                  ELTATCP 
00764      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTATCP 
00765      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTATCP 
00766                            ADDRESS OF CMF-DESCR.                  ELTATCP 
00767      SET WS-POINTER2 TO ADDRESS OF CMF-DESCR.                     ELTATCP 
00768                                                                   ELTATCP 
00769                                                                   ELTATCP 
00770 ************************************************************      ELTATCP 
00771 *                                                          *      ELTATCP 
00772 *        CALL GROUP CODES MANUAL INTERFACE                 *      ELTATCP 
00773 *                                                          *      ELTATCP 
00774 ************************************************************      ELTATCP 
00775  CALL-GROUP-CODES-MANUAL-INTERF.                                  ELTATCP 
00776      MOVE WS-GROUP TO CMF-RECORD-PREFIX.                          ELTATCP 
00777      EXEC CICS LINK                                               ELTATCP 
00778                PROGRAM ('ELUCMIF')                                ELTATCP 
00779                COMMAREA (DFHCOMMAREA)                             ELTATCP 
00780        END-EXEC.                                                  ELTATCP 
00781                                                                   ELTATCP 
00782                                                                   ELTATCP 
00783 ************************************************************      ELTATCP 
00784 *                                                          *      ELTATCP 
00785 *        CALL CODES MANUAL INTERFACE                       *      ELTATCP 
00786 *                                                          *      ELTATCP 
00787 ************************************************************      ELTATCP 
00788  CALL-CODES-MANUAL-INTERFACE.                                     ELTATCP 
00789      MOVE WS-GCCP TO CMF-RECORD-PREFIX.                           ELTATCP 
00790      EXEC CICS LINK                                               ELTATCP 
00791                PROGRAM ('ELUCMIF')                                ELTATCP 
00792                COMMAREA (DFHCOMMAREA)                             ELTATCP 
00793        END-EXEC.                                                  ELTATCP 
00794      EJECT                                                        ELTATCP 
00795                                                                   ELTATCP 
00796                                                                   ELTATCP 
00797 ************************************************************      ELTATCP 
00798 *                                                          *      ELTATCP 
00799 *        USE EXISTING APPROVAL TRANSLATION                 *      ELTATCP 
00800 *                                                          *      ELTATCP 
00801 ************************************************************      ELTATCP 
00802  USE-EXISTING-APPROVAL-TRANSLAT.                                  ELTATCP 
00803      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTATCP 
00804      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTATCP 
00805                            ADDRESS OF CMF-DESCR.                  ELTATCP 
00806      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTATCP 
00807      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTATCP 
00808                            WS-POINTER2.                           ELTATCP 
00809                                                                   ELTATCP 
00810                                                                   ELTATCP 
00811 ************************************************************      ELTATCP 
00812 *                                                          *      ELTATCP 
00813 *        TRANSLATE AND DISPLAY CODE VALUE                  *      ELTATCP 
00814 *                                                          *      ELTATCP 
00815 ************************************************************      ELTATCP 
00816  TRANSLATE-AND-DISPLAY-CODE-VAL.                                  ELTATCP 
00817      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTATCP 
00818      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTATCP 
00819      EJECT                                                        ELTATCP 
00820                                                                   ELTATCP 
00821                                                                   ELTATCP 
00822 ************************************************************      ELTATCP 
00823 *                                                          *      ELTATCP 
00824 *        TRANSLATE PAYMENT METHOD                          *      ELTATCP 
00825 *                                                          *      ELTATCP 
00826 ************************************************************      ELTATCP 
00827  TRANSLATE-PAYMENT-METHOD.                                        ELTATCP 
00828      INITIALIZE TCAR-FROM-AREA.                                   ELTATCP 
00829      MOVE +1 TO TCAR-FROM-SUB.                                    ELTATCP 
00830      MOVE WS-PYMT-LINE TO TCAR-FROM-LINE                          ELTATCP 
00831          (TCAR-FROM-SUB).                                         ELTATCP 
00832      ADD  +1 TO TCAR-FROM-SUB.                                    ELTATCP 
00833      SET BLANK-LINE-NEEDED TO TRUE.                               ELTATCP 
00834      SET PERIOD-NEEDED TO TRUE.                                   ELTATCP 
00835      MOVE GSS-AT-PAYMENT-METHOD (GSS-INDEX)  TO CMF-CODE-VALUE.   ELTATCP 
00836      MOVE 'AT-PAYMENT-METHOD'  TO CMF-ELEMENT-SYSTEM-NAME.        ELTATCP 
00837      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTATCP 
00838      EJECT                                                        ELTATCP 
00839                                                                   ELTATCP 
00840                                                                   ELTATCP 
00841 ************************************************************      ELTATCP 
00842 *                                                          *      ELTATCP 
00843 *        TRANSLATE BC CALC METHOD                          *      ELTATCP 
00844 *                                                          *      ELTATCP 
00845 ************************************************************      ELTATCP 
00846  TRANSLATE-BC-CALC-METHOD.                                        ELTATCP 
00847      INITIALIZE TCAR-FROM-AREA.                                   ELTATCP 
00848      MOVE +1 TO TCAR-FROM-SUB.                                    ELTATCP 
00849      SET BLANK-LINE-NEEDED TO TRUE.                               ELTATCP 
00850      SET PERIOD-NEEDED TO TRUE.                                   ELTATCP 
00851      MOVE GSS-AT-BC-CALC-METHOD (GSS-INDEX)  TO CMF-CODE-VALUE.   ELTATCP 
00852      MOVE 'AT-BC-CALC-METHOD'  TO CMF-ELEMENT-SYSTEM-NAME.        ELTATCP 
00853      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTATCP 
00854      EJECT                                                        ELTATCP 
00855                                                                   ELTATCP 
00856                                                                   ELTATCP 
00857 ************************************************************      ELTATCP 
00858 *                                                          *      ELTATCP 
00859 *        TRANSLATE BS CALC METHOD                          *      ELTATCP 
00860 *                                                          *      ELTATCP 
00861 ************************************************************      ELTATCP 
00862  TRANSLATE-BS-CALC-METHOD.                                        ELTATCP 
00863      INITIALIZE TCAR-FROM-AREA.                                   ELTATCP 
00864      MOVE +1 TO TCAR-FROM-SUB.                                    ELTATCP 
00865      SET BLANK-LINE-NEEDED TO TRUE.                               ELTATCP 
00866      SET PERIOD-NEEDED TO TRUE.                                   ELTATCP 
00867      MOVE GSS-AT-BS-CALC-METHOD (GSS-INDEX)  TO CMF-CODE-VALUE.   ELTATCP 
00868      MOVE 'AT-BS-CALC-METHOD'  TO CMF-ELEMENT-SYSTEM-NAME.        ELTATCP 
00869      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTATCP 
00870      EJECT                                                        ELTATCP 
00871                                                                   ELTATCP 
00872                                                                   ELTATCP 
00873 ************************************************************      ELTATCP 
00874 *                                                          *      ELTATCP 
00875 *        TRANSLATE BC ALT PRIC METH                        *      ELTATCP 
00876 *                                                          *      ELTATCP 
00877 ************************************************************      ELTATCP 
00878  TRANSLATE-BC-ALT-PRIC-METH.                                      ELTATCP 
00879      INITIALIZE TCAR-FROM-AREA.                                   ELTATCP 
00880      MOVE +1 TO TCAR-FROM-SUB.                                    ELTATCP 
00881      MOVE WS-ALT-PRIC-LINE-BC TO TCAR-FROM-LINE (TCAR-FROM-SUB).  ELTATCP 
00882      ADD  +1 TO TCAR-FROM-SUB.                                    ELTATCP 
00883      SET BLANK-LINE-NEEDED TO TRUE.                               ELTATCP 
00884      SET PERIOD-NEEDED TO TRUE.                                   ELTATCP 
00885      MOVE GSS-AT-BC-ALT-PRICING-METH (GSS-INDEX) TO               ELTATCP 
00886          CMF-CODE-VALUE.                                          ELTATCP 
00887      MOVE 'AT-BC-ALT-PRICING-METH' TO CMF-ELEMENT-SYSTEM-NAME.    ELTATCP 
00888      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTATCP 
00889      EJECT                                                        ELTATCP 
00890                                                                   ELTATCP 
00891                                                                   ELTATCP 
00892 ************************************************************      ELTATCP 
00893 *                                                          *      ELTATCP 
00894 *        TRANSLATE BS ALT PRIC METH                        *      ELTATCP 
00895 *                                                          *      ELTATCP 
00896 ************************************************************      ELTATCP 
00897  TRANSLATE-BS-ALT-PRIC-METH.                                      ELTATCP 
00898      INITIALIZE TCAR-FROM-AREA.                                   ELTATCP 
00899      MOVE +1 TO TCAR-FROM-SUB.                                    ELTATCP 
00900      MOVE WS-ALT-PRIC-LINE-BS TO TCAR-FROM-LINE (TCAR-FROM-SUB).  ELTATCP 
00901      ADD  +1 TO TCAR-FROM-SUB.                                    ELTATCP 
00902      SET BLANK-LINE-NEEDED TO TRUE.                               ELTATCP 
00903      SET PERIOD-NEEDED TO TRUE.                                   ELTATCP 
00904      MOVE GSS-AT-BS-ALT-PRICING-METH (GSS-INDEX) TO               ELTATCP 
00905          CMF-CODE-VALUE.                                          ELTATCP 
00906      MOVE 'AT-BS-ALT-PRICING-METH' TO CMF-ELEMENT-SYSTEM-NAME.    ELTATCP 
00907      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTATCP 
00908      EJECT                                                        ELTATCP 
00909                                                                   ELTATCP 
00910                                                                   ELTATCP 
00911 ************************************************************      ELTATCP 
00912 *                                                          *      ELTATCP 
00913 *        GET GCCP TABULAR                                  *      ELTATCP 
00914 *                                                          *      ELTATCP 
00915 ************************************************************      ELTATCP 
00916  GET-GCCP-TABULAR.                                                ELTATCP 
00917      PERFORM EST-ADDRESSABILITY-OF-GCTABULA.                      ELTATCP 
00918      SET IOP-RD              TO TRUE.                             ELTATCP 
00919      SET IOP-STG-MODE-MOVE   TO TRUE.                             ELTATCP 
00920      SET IOP-FCQ-NONE        TO TRUE.                             ELTATCP 
00921      SET IOP-KVQ-EQ          TO TRUE.                             ELTATCP 
00922      MOVE KWA-GCTABULR-KEY   TO IOP-FILE-KEY.                     ELTATCP 
00923      PERFORM CALL-INPUT-OUTPUT-MODULE.                            ELTATCP 
00924      IF IOP-RC-OK                                                 ELTATCP 
00925          PERFORM EST-ADDRESSABILITY-OF-GCCP-TAB                   ELTATCP 
00926      ELSE IF IOP-RC-NOTFND                                        ELTATCP 
00927          PERFORM SIGNAL-NOT-FOUND-GCTAB-ERROR                     ELTATCP 
00928      ELSE                                                         ELTATCP 
00929          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTATCP 
00930      EJECT                                                        ELTATCP 
00931                                                                   ELTATCP 
00932                                                                   ELTATCP 
00933 ************************************************************      ELTATCP 
00934 *                                                          *      ELTATCP 
00935 *        EST ADDRESSABILITY OF GCTABULAR IO PARAMETER BLOCK*      ELTATCP 
00936 *                                                          *      ELTATCP 
00937 ************************************************************      ELTATCP 
00938  EST-ADDRESSABILITY-OF-GCTABULA.                                  ELTATCP 
00939      SET CIA-GCTABULR-DDN TO TRUE.                                ELTATCP 
00940      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTATCP 
00941                            ADDRESS OF                             ELTATCP 
00942          IOP-INPUT-OUTPUT-PARAMETERS.                             ELTATCP 
00943      EJECT                                                        ELTATCP 
00944                                                                   ELTATCP 
00945                                                                   ELTATCP 
00946 ************************************************************      ELTATCP 
00947 *                                                          *      ELTATCP 
00948 *        CALL INPUT OUTPUT MODULE                          *      ELTATCP 
00949 *                                                          *      ELTATCP 
00950 ************************************************************      ELTATCP 
00951  CALL-INPUT-OUTPUT-MODULE.                                        ELTATCP 
00952      EXEC CICS LINK                                               ELTATCP 
00953                PROGRAM ('ELUIOPGM')                               ELTATCP 
00954                COMMAREA (DFHCOMMAREA)                             ELTATCP 
00955         END-EXEC.                                                 ELTATCP 
00956      EJECT                                                        ELTATCP 
00957                                                                   ELTATCP 
00958                                                                   ELTATCP 
00959 ************************************************************      ELTATCP 
00960 *                                                          *      ELTATCP 
00961 *        EST ADDRESSABILITY OF GCCP TABULAR                *      ELTATCP 
00962 *                                                          *      ELTATCP 
00963 ************************************************************      ELTATCP 
00964  EST-ADDRESSABILITY-OF-GCCP-TAB.                                  ELTATCP 
00965      SET ADDRESS OF GCCP-TABULAR-REC-AREA TO                      ELTATCP 
00966          IOP-REC-PTR.                                             ELTATCP 
00967      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELTATCP 
00968      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTATCP 
00969                            IOP-REC-PTR.                           ELTATCP 
00970      SET IOP-REC-PTR TO NULL.                                     ELTATCP 
00971      EJECT                                                        ELTATCP 
00972                                                                   ELTATCP 
00973                                                                   ELTATCP 
00974 ************************************************************      ELTATCP 
00975 *                                                          *      ELTATCP 
00976 *        SIGNAL NOT FOUND GCTAB ERROR                      *      ELTATCP 
00977 *                                                          *      ELTATCP 
00978 ************************************************************      ELTATCP 
00979  SIGNAL-NOT-FOUND-GCTAB-ERROR.                                    ELTATCP 
00980      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTATCP 
00981      PERFORM SIGNAL-ABEND.                                        ELTATCP 
00982      EJECT                                                        ELTATCP 
00983                                                                   ELTATCP 
00984                                                                   ELTATCP 
00985 ************************************************************      ELTATCP 
00986 *                                                          *      ELTATCP 
00987 *        SIGNAL CRITICAL IO ERROR                          *      ELTATCP 
00988 *                                                          *      ELTATCP 
00989 ************************************************************      ELTATCP 
00990  SIGNAL-CRITICAL-IO-ERROR.                                        ELTATCP 
00991      SET CIA-AB-CRITIO TO TRUE.                                   ELTATCP 
00992      PERFORM SIGNAL-ABEND.                                        ELTATCP 
00993      EJECT                                                        ELTATCP 
00994                                                                   ELTATCP 
00995                                                                   ELTATCP 
00996 ************************************************************      ELTATCP 
00997 *                                                          *      ELTATCP 
00998 *        GENERATE ACCUM TABULAR DATA                       *      ELTATCP 
00999 *                                                          *      ELTATCP 
01000 ************************************************************      ELTATCP 
01001  GENERATE-ACCUM-TABULAR-DATA.                                     ELTATCP 
01002      PERFORM GENERATE-MAXIMUM.                                    ELTATCP 
01003      PERFORM GENERATE-COINSURANCE.                                ELTATCP 
01004      PERFORM GENERATE-DEDUCTIBLE.                                 ELTATCP 
01005      EJECT                                                        ELTATCP 
01006                                                                   ELTATCP 
01007                                                                   ELTATCP 
01008 ************************************************************      ELTATCP 
01009 *                                                          *      ELTATCP 
01010 *        GENERATE ASSOCIATED TABULAR DATA                  *      ELTATCP 
01011 *                                                          *      ELTATCP 
01012 ************************************************************      ELTATCP 
01013  GENERATE-ASSOCIATED-TABULAR-DA.                                  ELTATCP 
01014      PERFORM GENERATE-RELATED-SERVICES-SENT.                      ELTATCP 
01015      PERFORM GENERATE-SPECIAL-PROCEDURES-SE.                      ELTATCP 
01016                                                                   ELTATCP 
01017                                                                   ELTATCP 
01018 ************************************************************      ELTATCP 
01019 *                                                          *      ELTATCP 
01020 *        GENERATE RELATED SERVICES SENTENCE                *      ELTATCP 
01021 *                                                          *      ELTATCP 
01022 ************************************************************      ELTATCP 
01023  GENERATE-RELATED-SERVICES-SENT.                                  ELTATCP 
01024      SET GCG-INDEX TO +1.                                         ELTATCP 
01025      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTATCP 
01026         AT END                                                    ELTATCP 
01027              MOVE ZEROES TO WS-GHOB-PROV-SLOT-NO                  ELTATCP 
01028         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GHOB                     ELTATCP 
01029              MOVE GCG-TAB-ID (GCG-INDEX)      TO                  ELTATCP 
01030          WS-GHOB-PROV-ID                                          ELTATCP 
01031              MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                  ELTATCP 
01032          WS-GHOB-PROV-SLOT-NO                                     ELTATCP 
01033         END-SEARCH.                                               ELTATCP 
01034      IF WS-GHOB-PROV-ID EQUAL WS-GHOB                             ELTATCP 
01035                 AND WS-GHOB-PROV-SLOT-NO NOT EQUAL                ELTATCP 
01036          ZEROES                                                   ELTATCP 
01037          PERFORM DISPLAY-RELATED-SERVICES-SENTE.                  ELTATCP 
01038                                                                   ELTATCP 
01039                                                                   ELTATCP 
01040 ************************************************************      ELTATCP 
01041 *                                                          *      ELTATCP 
01042 *        DISPLAY RELATED SERVICES SENTENCE                 *      ELTATCP 
01043 *                                                          *      ELTATCP 
01044 ************************************************************      ELTATCP 
01045  DISPLAY-RELATED-SERVICES-SENTE.                                  ELTATCP 
01046      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTATCP 
01047      MOVE WS-SPEC-SERV-MSG  TO COF-DTL-LINE                       ELTATCP 
01048          (COF-NBR-DTL-LINES).                                     ELTATCP 
01049      PERFORM LINK-TO-OUTPUT.                                      ELTATCP 
01050      PERFORM GENERATE-RELATED-SERVICES-TEXT.                      ELTATCP 
01051      EJECT                                                        ELTATCP 
01052                                                                   ELTATCP 
01053                                                                   ELTATCP 
01054 ************************************************************      ELTATCP 
01055 *                                                          *      ELTATCP 
01056 *        GENERATE SPECIAL PROCEDURES SENTENCE              *      ELTATCP 
01057 *                                                          *      ELTATCP 
01058 ************************************************************      ELTATCP 
01059  GENERATE-SPECIAL-PROCEDURES-SE.                                  ELTATCP 
01060      SET GCG-INDEX TO +1.                                         ELTATCP 
01061      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTATCP 
01062         AT END                                                    ELTATCP 
01063              MOVE ZEROES TO WS-GHOR-PROV-SLOT-NO                  ELTATCP 
01064         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GHOR                     ELTATCP 
01065              MOVE GCG-TAB-ID (GCG-INDEX)      TO                  ELTATCP 
01066          WS-GHOR-PROV-ID                                          ELTATCP 
01067              MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                  ELTATCP 
01068          WS-GHOR-PROV-SLOT-NO                                     ELTATCP 
01069         END-SEARCH.                                               ELTATCP 
01070      IF WS-GHOR-PROV-ID EQUAL WS-GHOR                             ELTATCP 
01071                 AND WS-GHOR-PROV-SLOT-NO NOT EQUAL                ELTATCP 
01072          ZEROS                                                    ELTATCP 
01073          PERFORM DISPLAY-SPECIAL-PROCEDURES-SEN.                  ELTATCP 
01074                                                                   ELTATCP 
01075                                                                   ELTATCP 
01076 ************************************************************      ELTATCP 
01077 *                                                          *      ELTATCP 
01078 *        DISPLAY SPECIAL PROCEDURES SENTENCE               *      ELTATCP 
01079 *                                                          *      ELTATCP 
01080 ************************************************************      ELTATCP 
01081  DISPLAY-SPECIAL-PROCEDURES-SEN.                                  ELTATCP 
01082      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTATCP 
01083      MOVE WS-SPEC-PROC-MSG  TO COF-DTL-LINE                       ELTATCP 
01084          (COF-NBR-DTL-LINES).                                     ELTATCP 
01085      PERFORM LINK-TO-OUTPUT.                                      ELTATCP 
01086      PERFORM GENERATE-SPECIAL-PROCEDURES-TE.                      ELTATCP 
01087                                                                   ELTATCP 
01088                                                                   ELTATCP 
01089 ************************************************************      ELTATCP 
01090 *                                                          *      ELTATCP 
01091 *        GENERATE MAXIMUM                                  *      ELTATCP 
01092 *                                                          *      ELTATCP 
01093 ************************************************************      ELTATCP 
01094  GENERATE-MAXIMUM.                                                ELTATCP 
01095      EXEC CICS LINK                                               ELTATCP 
01096                PROGRAM ('ELGABMCC')                               ELTATCP 
01097                COMMAREA (DFHCOMMAREA)                             ELTATCP 
01098         END-EXEC.                                                 ELTATCP 
01099                                                                   ELTATCP 
01100                                                                   ELTATCP 
01101 ************************************************************      ELTATCP 
01102 *                                                          *      ELTATCP 
01103 *        GENERATE COINSURANCE                              *      ELTATCP 
01104 *                                                          *      ELTATCP 
01105 ************************************************************      ELTATCP 
01106  GENERATE-COINSURANCE.                                            ELTATCP 
01107      EXEC CICS LINK                                               ELTATCP 
01108                PROGRAM ('ELGACLCC')                               ELTATCP 
01109                COMMAREA (DFHCOMMAREA)                             ELTATCP 
01110         END-EXEC.                                                 ELTATCP 
01111                                                                   ELTATCP 
01112                                                                   ELTATCP 
01113 ************************************************************      ELTATCP 
01114 *                                                          *      ELTATCP 
01115 *        GENERATE DEDUCTIBLE                               *      ELTATCP 
01116 *                                                          *      ELTATCP 
01117 ************************************************************      ELTATCP 
01118  GENERATE-DEDUCTIBLE.                                             ELTATCP 
01119      EXEC CICS LINK                                               ELTATCP 
01120                PROGRAM ('ELGADLCC')                               ELTATCP 
01121                COMMAREA (DFHCOMMAREA)                             ELTATCP 
01122         END-EXEC.                                                 ELTATCP 
01123      EJECT                                                        ELTATCP 
01124                                                                   ELTATCP 
01125                                                                   ELTATCP 
01126 ************************************************************      ELTATCP 
01127 *                                                          *      ELTATCP 
01128 *        GENERATE RELATED SERVICES TEXT                    *      ELTATCP 
01129 *                                                          *      ELTATCP 
01130 ************************************************************      ELTATCP 
01131  GENERATE-RELATED-SERVICES-TEXT.                                  ELTATCP 
01132      MOVE 'ADDITIONAL TRANSPLANT COVERAGE' TO                     ELTATCP 
01133          SRP-CCP-NAME.                                            ELTATCP 
01134      MOVE  WS-GHOB-PROV-ID                 TO SRP-TABULAR-ID.     ELTATCP 
01135      MOVE  WS-GHOB-PROV-SLOT-NO            TO                     ELTATCP 
01136          SRP-TABULAR-SLOT-NO.                                     ELTATCP 
01137      PERFORM CALL-RELATED-SERVICES-GENERATO.                      ELTATCP 
01138                                                                   ELTATCP 
01139                                                                   ELTATCP 
01140 ************************************************************      ELTATCP 
01141 *                                                          *      ELTATCP 
01142 *        CALL RELATED SERVICES GENERATOR                   *      ELTATCP 
01143 *                                                          *      ELTATCP 
01144 ************************************************************      ELTATCP 
01145  CALL-RELATED-SERVICES-GENERATO.                                  ELTATCP 
01146      EXEC CICS LINK                                               ELTATCP 
01147                PROGRAM ('ELGGXXB')                                ELTATCP 
01148                COMMAREA (DFHCOMMAREA)                             ELTATCP 
01149         END-EXEC.                                                 ELTATCP 
01150      EJECT                                                        ELTATCP 
01151                                                                   ELTATCP 
01152                                                                   ELTATCP 
01153 ************************************************************      ELTATCP 
01154 *                                                          *      ELTATCP 
01155 *        GENERATE SPECIAL PROCEDURES TEXT                  *      ELTATCP 
01156 *                                                          *      ELTATCP 
01157 ************************************************************      ELTATCP 
01158  GENERATE-SPECIAL-PROCEDURES-TE.                                  ELTATCP 
01159      MOVE 'ADDITIONAL TRANSPLANT COVERAGE' TO                     ELTATCP 
01160          SRP-CCP-NAME.                                            ELTATCP 
01161      MOVE  WS-GHOR-PROV-ID                 TO SRP-TABULAR-ID.     ELTATCP 
01162      MOVE  WS-GHOR-PROV-SLOT-NO            TO                     ELTATCP 
01163          SRP-TABULAR-SLOT-NO.                                     ELTATCP 
01164      PERFORM CALL-SPECIAL-PROCEDURES-GENERA.                      ELTATCP 
01165                                                                   ELTATCP 
01166                                                                   ELTATCP 
01167 ************************************************************      ELTATCP 
01168 *                                                          *      ELTATCP 
01169 *        CALL SPECIAL PROCEDURES GENERATOR                 *      ELTATCP 
01170 *                                                          *      ELTATCP 
01171 ************************************************************      ELTATCP 
01172  CALL-SPECIAL-PROCEDURES-GENERA.                                  ELTATCP 
01173      EXEC CICS LINK                                               ELTATCP 
01174                PROGRAM ('ELGGXXR')                                ELTATCP 
01175                COMMAREA (DFHCOMMAREA)                             ELTATCP 
01176         END-EXEC.                                                 ELTATCP 
01177      EJECT                                                        ELTATCP 
01178                                                                   ELTATCP 
01179                                                                   ELTATCP 
01180 ************************************************************      ELTATCP 
01181 *                                                          *      ELTATCP 
01182 *        TERMINATE OUTPUT                                  *      ELTATCP 
01183 *                                                          *      ELTATCP 
01184 ************************************************************      ELTATCP 
01185  TERMINATE-OUTPUT.                                                ELTATCP 
01186      SET COF-END TO TRUE.                                         ELTATCP 
01187      PERFORM LINK-TO-OUTPUT.                                      ELTATCP 
01188                                                                   ELTATCP 
01189                                                                   ELTATCP 
01190 ************************************************************      ELTATCP 
01191 *                                                          *      ELTATCP 
01192 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTATCP 
01193 *                                                          *      ELTATCP 
01194 ************************************************************      ELTATCP 
01195  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTATCP 
01196      PERFORM INITIALIZE-CMOUT.                                    ELTATCP 
01197      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTATCP 
01198      EJECT                                                        ELTATCP 
01199                                                                   ELTATCP 
01200                                                                   ELTATCP 
01201 ************************************************************      ELTATCP 
01202 *                                                          *      ELTATCP 
01203 *        PREPARE TEXT FOR OUTPUT                           *      ELTATCP 
01204 *                                                          *      ELTATCP 
01205 ************************************************************      ELTATCP 
01206  PREPARE-TEXT-FOR-OUTPUT.                                         ELTATCP 
01207      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTATCP 
01208          UNTIL CMF-DESCR-IDX                                      ELTATCP 
01209                                    GREATER THAN                   ELTATCP 
01210              CMF-NBR-DESCR-LINES.                                 ELTATCP 
01211      EJECT                                                        ELTATCP 
01212                                                                   ELTATCP 
01213                                                                   ELTATCP 
01214 ************************************************************      ELTATCP 
01215 *                                                          *      ELTATCP 
01216 *        INITIALIZE CMOUT                                  *      ELTATCP 
01217 *                                                          *      ELTATCP 
01218 ************************************************************      ELTATCP 
01219  INITIALIZE-CMOUT.                                                ELTATCP 
01220      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTATCP 
01221      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTATCP 
01222          ADDRESS OF CMF-DESCR.                                    ELTATCP 
01223      SET CMF-DESCR-IDX TO 1.                                      ELTATCP 
01224      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTATCP 
01225                                                                   ELTATCP 
01226                                                                   ELTATCP 
01227 ************************************************************      ELTATCP 
01228 *                                                          *      ELTATCP 
01229 *        MOVE CMF TEXT TO OUTPUT                           *      ELTATCP 
01230 *                                                          *      ELTATCP 
01231 ************************************************************      ELTATCP 
01232  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTATCP 
01233      PERFORM MOVE-A-LINE.                                         ELTATCP 
01234      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTATCP 
01235          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTATCP 
01236      IF TCAR-FROM-SUB GREATER THAN 20                             ELTATCP 
01237               OR CMF-DESCR-IDX GREATER THAN                       ELTATCP 
01238          CMF-NBR-DESCR-LINES                                      ELTATCP 
01239          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTATCP 
01240                                                                   ELTATCP 
01241                                                                   ELTATCP 
01242 ************************************************************      ELTATCP 
01243 *                                                          *      ELTATCP 
01244 *        FINISH CODES MANUAL TEXT                          *      ELTATCP 
01245 *                                                          *      ELTATCP 
01246 ************************************************************      ELTATCP 
01247  FINISH-CODES-MANUAL-TEXT.                                        ELTATCP 
01248      SET DONE-PROCESSING TO TRUE.                                 ELTATCP 
01249      IF PERIOD-NEEDED                                             ELTATCP 
01250          PERFORM GET-AND-MOVE-PERIOD.                             ELTATCP 
01251                                                                   ELTATCP 
01252                                                                   ELTATCP 
01253 ************************************************************      ELTATCP 
01254 *                                                          *      ELTATCP 
01255 *        GET AND MOVE PERIOD                               *      ELTATCP 
01256 *                                                          *      ELTATCP 
01257 ************************************************************      ELTATCP 
01258  GET-AND-MOVE-PERIOD.                                             ELTATCP 
01259      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTATCP 
01260          (TCAR-FROM-SUB).                                         ELTATCP 
01261                                                                   ELTATCP 
01262                                                                   ELTATCP 
01263 ************************************************************      ELTATCP 
01264 *                                                          *      ELTATCP 
01265 *        SAVE LAST LINE                                    *      ELTATCP 
01266 *                                                          *      ELTATCP 
01267 ************************************************************      ELTATCP 
01268  SAVE-LAST-LINE.                                                  ELTATCP 
01269      MOVE +1 TO TCAR-FROM-SUB.                                    ELTATCP 
01270      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTATCP 
01271         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTATCP 
01272      ADD 1 TO TCAR-FROM-SUB.                                      ELTATCP 
01273      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTATCP 
01274                                                                   ELTATCP 
01275                                                                   ELTATCP 
01276 ************************************************************      ELTATCP 
01277 *                                                          *      ELTATCP 
01278 *        OUTPUT LAST LINE                                  *      ELTATCP 
01279 *                                                          *      ELTATCP 
01280 ************************************************************      ELTATCP 
01281  OUTPUT-LAST-LINE.                                                ELTATCP 
01282      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTATCP 
01283          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTATCP 
01284      IF BLANK-LINE-NEEDED                                         ELTATCP 
01285          PERFORM CREATE-A-BLANK-LINE.                             ELTATCP 
01286                                                                   ELTATCP 
01287                                                                   ELTATCP 
01288 ************************************************************      ELTATCP 
01289 *                                                          *      ELTATCP 
01290 *        CREATE A BLANK LINE                               *      ELTATCP 
01291 *                                                          *      ELTATCP 
01292 ************************************************************      ELTATCP 
01293  CREATE-A-BLANK-LINE.                                             ELTATCP 
01294      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTATCP 
01295      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTATCP 
01296                                                                   ELTATCP 
01297                                                                   ELTATCP 
01298 ************************************************************      ELTATCP 
01299 *                                                          *      ELTATCP 
01300 *        MOVE A LINE                                       *      ELTATCP 
01301 *                                                          *      ELTATCP 
01302 ************************************************************      ELTATCP 
01303  MOVE-A-LINE.                                                     ELTATCP 
01304      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTATCP 
01305          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTATCP 
01306      SET CMF-DESCR-IDX UP BY 1.                                   ELTATCP 
01307      ADD 1 TO TCAR-FROM-SUB.                                      ELTATCP 
01308      EJECT                                                        ELTATCP 
01309                                                                   ELTATCP 
01310                                                                   ELTATCP 
01311 ************************************************************      ELTATCP 
01312 *                                                          *      ELTATCP 
01313 *        REFORMAT AND WRITE TEXT                           *      ELTATCP 
01314 *                                                          *      ELTATCP 
01315 ************************************************************      ELTATCP 
01316  REFORMAT-AND-WRITE-TEXT.                                         ELTATCP 
01317      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTATCP 
01318      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTATCP 
01319      PERFORM UNSTRING-TEXT.                                       ELTATCP 
01320      MOVE +1 TO TCAR-FROM-SUB.                                    ELTATCP 
01321      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTATCP 
01322      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTATCP 
01323          UNTIL COF-NBR-DTL-LINES GREATER                          ELTATCP 
01324                                   TCAR-OUTPUT-FIELDS-USED -       ELTATCP 
01325              1.                                                   ELTATCP 
01326      PERFORM DISPOSE-OF-LAST-LINE.                                ELTATCP 
01327      PERFORM LINK-TO-OUTPUT.                                      ELTATCP 
01328                                                                   ELTATCP 
01329                                                                   ELTATCP 
01330 ************************************************************      ELTATCP 
01331 *                                                          *      ELTATCP 
01332 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTATCP 
01333 *                                                          *      ELTATCP 
01334 ************************************************************      ELTATCP 
01335  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTATCP 
01336      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTATCP 
01337           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTATCP 
01338      ADD +1 TO TCAR-FROM-SUB.                                     ELTATCP 
01339      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTATCP 
01340      EJECT                                                        ELTATCP 
01341                                                                   ELTATCP 
01342                                                                   ELTATCP 
01343 ************************************************************      ELTATCP 
01344 *                                                          *      ELTATCP 
01345 *        UNSTRING TEXT                                     *      ELTATCP 
01346 *                                                          *      ELTATCP 
01347 ************************************************************      ELTATCP 
01348  UNSTRING-TEXT.                                                   ELTATCP 
01349      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTATCP 
01350      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTATCP 
01351      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTATCP 
01352      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTATCP 
01353      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTATCP 
01354      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTATCP 
01355      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTATCP 
01356      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTATCP 
01357      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTATCP 
01358      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTATCP 
01359      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTATCP 
01360      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTATCP 
01361      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTATCP 
01362      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTATCP 
01363      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTATCP 
01364      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTATCP 
01365      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTATCP 
01366      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTATCP 
01367      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTATCP 
01368      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTATCP 
01369      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTATCP 
01370      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTATCP 
01371                                                                   ELTATCP 
01372                                                                   ELTATCP 
01373 ************************************************************      ELTATCP 
01374 *                                                          *      ELTATCP 
01375 *        LINK TO OUTPUT                                    *      ELTATCP 
01376 *                                                          *      ELTATCP 
01377 ************************************************************      ELTATCP 
01378  LINK-TO-OUTPUT.                                                  ELTATCP 
01379      EXEC CICS LINK                                               ELTATCP 
01380          PROGRAM ('ELUOUTPT')                                     ELTATCP 
01381          COMMAREA (DFHCOMMAREA)                                   ELTATCP 
01382          END-EXEC.                                                ELTATCP 
01383      EJECT                                                        ELTATCP 
01384                                                                   ELTATCP 
01385                                                                   ELTATCP 
01386 ************************************************************      ELTATCP 
01387 *                                                          *      ELTATCP 
01388 *        DISPOSE OF LAST LINE                              *      ELTATCP 
01389 *                                                          *      ELTATCP 
01390 ************************************************************      ELTATCP 
01391  DISPOSE-OF-LAST-LINE.                                            ELTATCP 
01392      IF NOT ADDITIONAL-TEXT                                       ELTATCP 
01393          PERFORM INITIALIZE-CONTINUED-SW.                         ELTATCP 
01394      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTATCP 
01395          PERFORM SAVE-LAST-LINE                                   ELTATCP 
01396      ELSE                                                         ELTATCP 
01397          PERFORM OUTPUT-LAST-LINE.                                ELTATCP 
01398                                                                   ELTATCP 
01399                                                                   ELTATCP 
01400 ************************************************************      ELTATCP 
01401 *                                                          *      ELTATCP 
01402 *        INITIALIZE CONTINUED SW                           *      ELTATCP 
01403 *                                                          *      ELTATCP 
01404 ************************************************************      ELTATCP 
01405  INITIALIZE-CONTINUED-SW.                                         ELTATCP 
01406      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTATCP 
