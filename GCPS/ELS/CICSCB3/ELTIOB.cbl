00001 *      LAST MAINTENANCE TIME:  7.51.34  DATE: 06/13/91            06/29/02
00002  IDENTIFICATION DIVISION.                                         ELTIOB  
00003                                                                      LV001
00004  PROGRAM-ID.         ELTIOB.                                      ELTIOB  
00005                                                                   ELTIOB  
00006  AUTHOR.             RICK BARILEAU.                               ELTIOB  
00007                                                                   ELTIOB  
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTIOB  
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELTIOB  
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTIOB  
00011                      233 N. MICHIGAN AVE                          ELTIOB  
00012                      CHICAGO, ILLINOIS 60601                      ELTIOB  
00013                                                                   ELTIOB  
00014  DATE-WRITTEN.       17-JUN-1987.                                 ELTIOB  
00015                                                                   ELTIOB  
00016  DATE-COMPILED.                                                   ELTIOB  
00017                                                                   ELTIOB  
00018  SECURITY.           COPYRIGHT 1986,                              ELTIOB  
00019                      HEALTH CARE SERVICE CORPORATION              ELTIOB  
00020      SKIP3                                                        ELTIOB  
00021  ENVIRONMENT DIVISION.                                            ELTIOB  
00022                                                                   ELTIOB  
00023  CONFIGURATION SECTION.                                           ELTIOB  
00024  SOURCE-COMPUTER.    IBM-3090.                                    ELTIOB  
00025  OBJECT-COMPUTER.    IBM-3090.                                    ELTIOB  
00026      EJECT                                                        ELTIOB  
00027 ******************************************************************ELTIOB  
00028 *                                                                *ELTIOB  
00029 *    COPYBOOK:   ELTIOB                                          *ELTIOB  
00030 *    DATE:       17-JUN-1987                                     *ELTIOB  
00031 *    AUTHOR:     RICK BARILEAU                                   *ELTIOB  
00032 *    FUNCTION:   THIS WILL GENERATE ALL OUTPUT ASSOCIATED WITH   *ELTIOB  
00033 *                THE INCENTIVE OBSTETRICAL PROGRAM.              *ELTIOB  
00034 *    NOTES:      X---                                            *ELTIOB  
00035 *                                                                *ELTIOB  
00036 ******************************************************************ELTIOB  
00037 *                                                                *ELTIOB  
00038 *                      MAINTENANCE HISTORY                       *ELTIOB  
00039 *                                                                *ELTIOB  
00040 *  MOD     DATE     BY  DRPT                ACTION               *ELTIOB  
00041 * ----- ----------- --- ----- ---------------------------------- *ELTIOB  
00042 * 01.00 17-JUN-1987 REB       CREATED                            *ELTIOB  
00043 *                                                                *ELTIOB  
00044 * 01.01 31-OCT-1990 JPB       CHANGED STORAGE MANAGEMENT         *ELTIOB  
00045 *                                                                *ELTIOB  
00046 * 01.02 09-NOV-1990 JPB       REFERENCES TO GCG-INCENTIVE-OB-IND *ELTIOB  
00047 *                             CHANGED TO ACCOMODATE NEW LENGTH.  *ELTIOB  
00048 *                                                                *ELTIOB  
00049 * 01.03 06-JUN-1991 GEM       ADD CCP PARTIC IND TO IOB.         *ELTIOB  
00050 *                                                                *ELTIOB  
00051 ******************************************************************ELTIOB  
00052                                                                   ELTIOB  
00053  DATA DIVISION.                                                   ELTIOB  
00054  WORKING-STORAGE SECTION.                                         ELTIOB  
00055  01  WS-MISC.                                                     ELTIOB  
00056      05  WS-BEGIN                 PIC X(16) VALUE                 ELTIOB  
00057      'ELTIOB WS BEGINS'.                                          ELTIOB  
00058      05  WS-POINTER2              POINTER.                        ELTIOB  
00059      05  WS-POINTER3              POINTER.                        ELTIOB  
00060                                                                   ELTIOB  
00061  01  WS-SWITCHES.                                                 ELTIOB  
00062      05  ADDITIONAL-TEXT-SW       PIC X(01) VALUE SPACE.          ELTIOB  
00063          88  ADDITIONAL-TEXT                VALUE 'A'.            ELTIOB  
00064          88  BLANK-LINE-NEEDED              VALUE 'B'.            ELTIOB  
00065      05  CONTINUED-PROCESSING-SW  PIC X(01) VALUE SPACE.          ELTIOB  
00066          88  DONE-PROCESSING                VALUE 'D'.            ELTIOB  
00067          88  PROCESSING-CMF-TEXT            VALUE 'P'.            ELTIOB  
00068      05  WS-PERIOD-SWITCH         PIC X(01) VALUE 'N'.            ELTIOB  
00069          88  PERIOD-NEEDED                  VALUE 'Y'.            ELTIOB  
00070      05  WS-TABULAR-SWITCH        PIC X(01) VALUE 'N'.            ELTIOB  
00071          88  TABULAR-IS-UNDEFINED           VALUE 'Y'.            ELTIOB  
00072      05  WS-IOB-PROG-SWITCH       PIC X     VALUE SPACE.          ELTIOB  
00073          88  NOT-HOLDING-IOB-PROG           VALUE 'N'.            ELTIOB  
00074          88  HOLDING-IOB-PROG               VALUE 'H'.            ELTIOB  
00075                                                                   ELTIOB  
00076 **************************************************************    ELTIOB  
00077 ***                   PROGRAM CONSTANTS                           ELTIOB  
00078 **************************************************************    ELTIOB  
00079      05  PC-AND                   PIC X(05) VALUE ' AND '.        ELTIOB  
00080      05  PC-GRP                   PIC X(06) VALUE 'GROUP'.        ELTIOB  
00081      05  PC-GCCP                  PIC X(06) VALUE '#GCCP '.       ELTIOB  
00082      05  PC-INST                  PIC X(13) VALUE 'INSTITUTIONAL'.ELTIOB  
00083      05  PC-PROF                  PIC X(13) VALUE 'PROFESSIONAL '.ELTIOB  
00084      05  PC-SUPP                  PIC X(13) VALUE 'SUPPLEMENTAL '.ELTIOB  
00085                                                                   ELTIOB  
00086 **************************************************************    ELTIOB  
00087 ***                   HEADER LINE                                 ELTIOB  
00088 **************************************************************    ELTIOB  
00089      05  WS-HEADER-LINE.                                          ELTIOB  
00090          10  FILLER               PIC X(18) VALUE SPACES.         ELTIOB  
00091          10  FILLER               PIC X(30) VALUE                 ELTIOB  
00092          'INCENTIVE OBSTETRICAL PROGRAM '.                        ELTIOB  
00093          10  WS-HDR-LINE-BCBSMM   PIC X(13) VALUE SPACES.         ELTIOB  
00094          10  FILLER               PIC X(18) VALUE SPACES.         ELTIOB  
00095                                                                   ELTIOB  
00096 **************************************************************    ELTIOB  
00097 ***                   SCREEN BODY LINES                           ELTIOB  
00098 **************************************************************    ELTIOB  
00099  01  WS-SCREEN-LINE-AREA.                                         ELTIOB  
00100      05  WS-PAYMENT-LINE.                                         ELTIOB  
00101          10  FILLER               PIC X(34) VALUE                 ELTIOB  
00102          'THE INCENTIVE OBSTETRICAL PROGRAM '.                    ELTIOB  
00103          10  FILLER               PIC X(22) VALUE                 ELTIOB  
00104          'ADDITIONAL PAYMENT IS '.                                ELTIOB  
00105          10  WS-PYMT-LINE-AMT1    PIC $ZZZ.99.                    ELTIOB  
00106          10  FILLER               PIC X(05) VALUE ' AND '.        ELTIOB  
00107          10  WS-PYMT-LINE-AMT2    PIC $ZZZ.99.                    ELTIOB  
00108          10  FILLER               PIC X(01) VALUE '.'.            ELTIOB  
00109          10  FILLER               PIC X(03) VALUE SPACE.          ELTIOB  
00110                                                                   ELTIOB  
00111      05  WS-IOB-APPLIES.                                          ELTIOB  
00112          10  FILLER               PIC  X(43) VALUE                ELTIOB  
00113          'THE INCENTIVE OBSTETRICS PROGRAM APPLIES TO'.           ELTIOB  
00114                                                                   ELTIOB  
00115 **************************************************************    ELTIOB  
00116 ** SPECIAL MESSAGE FOR THE VOLUNTARY,NOT APPLICABLE OR            ELTIOB  
00117 ** DOES NOT APPLY TO LINE OF BUSINESS                             ELTIOB  
00118 **************************************************************    ELTIOB  
00119      05  WS-NOT-APPLICABLE-MSG.                                   ELTIOB  
00120          10  FILLER               PIC  X(79) VALUE                ELTIOB  
00121          'THE INCENTIVE OBSTETRICS PROGRAM IS NOT APPLICABLE.'.   ELTIOB  
00122                                                                   ELTIOB  
00123      05  WS-VOLUNTARY-MSG.                                        ELTIOB  
00124          10  FILLER               PIC  X(79) VALUE                ELTIOB  
00125          'THE INCENTIVE OBSTETRICS PROGRAM IS VOLUNTARY.'.        ELTIOB  
00126                                                                   ELTIOB  
00127      05  WS-DISCLAIMER-MSG.                                       ELTIOB  
00128          10  FILLER               PIC  X(79) VALUE                ELTIOB  
00129          '*** SUBJECT TO OTHER CONTRACT LIMITATIONS ***'.         ELTIOB  
00130                                                                   ELTIOB  
00131      05  WS-NOT-APPLY-TO-LOB-BC.                                  ELTIOB  
00132          10  FILLER               PIC  X(48) VALUE                ELTIOB  
00133          'INCENTIVE OBSTETRICS PROGRAM DOES NOT APPLY FOR '.      ELTIOB  
00134          10  FILLER               PIC  X(31) VALUE                ELTIOB  
00135          'INSTITUTIONAL BENEFITS.'.                               ELTIOB  
00136                                                                   ELTIOB  
00137      05  WS-NOT-APPLY-TO-LOB-BS.                                  ELTIOB  
00138          10  FILLER               PIC  X(48) VALUE                ELTIOB  
00139          'INCENTIVE OBSTETRICS PROGRAM DOES NOT APPLY FOR '.      ELTIOB  
00140          10  FILLER               PIC  X(31) VALUE                ELTIOB  
00141          'PROFESSIONAL BENEFITS.'.                                ELTIOB  
00142                                                                   ELTIOB  
00143      05  WS-NOT-APPLY-TO-LOB-MM.                                  ELTIOB  
00144          10  FILLER               PIC  X(48) VALUE                ELTIOB  
00145          'INCENTIVE OBSTETRICS PROGRAM DOES NOT APPLY FOR '.      ELTIOB  
00146          10  FILLER               PIC  X(31) VALUE                ELTIOB  
00147          'SUPPLEMENTAL BENEFITS.'.                                ELTIOB  
00148 /                                                                 ELTIOB  
00149  LINKAGE SECTION.                                                 ELTIOB  
00150  01  DFHCOMMAREA.                                                 ELTIOB  
00151      COPY ELSCOMMC.                                               ELTIOB  
00152 /                                                                 ELTIOB  
00153      COPY ELSCIA2C.                                               ELTIOB  
00154 /                                                                 ELTIOB  
00155      COPY ELSCMDSC.                                               ELTIOB  
00156 /                                                                 ELTIOB  
00157      COPY ELSCMIFC.                                               ELTIOB  
00158 /                                                                 ELTIOB  
00159      COPY ELSIOPMC.                                               ELTIOB  
00160 /                                                                 ELTIOB  
00161      COPY ELSKEYSC.                                               ELTIOB  
00162 /                                                                 ELTIOB  
00163      COPY ELSOUTPC.                                               ELTIOB  
00164 /                                                                 ELTIOB  
00165      COPY ELSSRTPC.                                               ELTIOB  
00166 /                                                                 ELTIOB  
00167      COPY ELSTCWAC.                                               ELTIOB  
00168 /                                                                 ELTIOB  
00169      COPY ELSSSCBC.                                               ELTIOB  
00170 /                                                                 ELTIOB  
00171  01  GROUP-SPECIFIC-REC.                                          ELTIOB  
00172  COPY GCGROUPC.                                                   ELTIOB  
00173 /                                                                 ELTIOB  
00174  01  GCCP-TABULAR-REC-AREA.                                       ELTIOB  
00175  COPY GCTGCCPC.                                                   ELTIOB  
00176      EJECT                                                        ELTIOB  
00177  PROCEDURE DIVISION.                                              ELTIOB  
00178 ************************************************************      ELTIOB  
00179 *                                                          *      ELTIOB  
00180 *                    PROCEDURE DIVISION                    *      ELTIOB  
00181 *                                                          *      ELTIOB  
00182 ************************************************************      ELTIOB  
00183                                                                   ELTIOB  
00184                                                                   ELTIOB  
00185 ************************************************************      ELTIOB  
00186 *                                                          *      ELTIOB  
00187 *        INCENTIVE OBSTETRICAL PROGRAM                     *      ELTIOB  
00188 *                                                          *      ELTIOB  
00189 ************************************************************      ELTIOB  
00190  INCENTIVE-OBSTETRICAL-PROGRAM.                                   ELTIOB  
00191      PERFORM INITIALIZATION-ROUTINE.                              ELTIOB  
00192      PERFORM PROCESS.                                             ELTIOB  
00193      GOBACK.                                                      ELTIOB  
00194                                                                   ELTIOB  
00195                                                                   ELTIOB  
00196 ************************************************************      ELTIOB  
00197 *                                                          *      ELTIOB  
00198 *        INITIALIZATION ROUTINE                            *      ELTIOB  
00199 *                                                          *      ELTIOB  
00200 ************************************************************      ELTIOB  
00201  INITIALIZATION-ROUTINE.                                          ELTIOB  
00202      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTIOB  
00203      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTIOB  
00204                                                                   ELTIOB  
00205                                                                   ELTIOB  
00206 ************************************************************      ELTIOB  
00207 *                                                          *      ELTIOB  
00208 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTIOB  
00209 *                                                          *      ELTIOB  
00210 ************************************************************      ELTIOB  
00211  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTIOB  
00212      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTIOB  
00213      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTIOB  
00214      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTIOB  
00215                                                                   ELTIOB  
00216                                                                   ELTIOB  
00217 ************************************************************      ELTIOB  
00218 *                                                          *      ELTIOB  
00219 *        CHECK FOR VALID COMMAREA                          *      ELTIOB  
00220 *                                                          *      ELTIOB  
00221 ************************************************************      ELTIOB  
00222  CHECK-FOR-VALID-COMMAREA.                                        ELTIOB  
00223      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTIOB  
00224          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTIOB  
00225                                                                   ELTIOB  
00226                                                                   ELTIOB  
00227 ************************************************************      ELTIOB  
00228 *                                                          *      ELTIOB  
00229 *        SIGNAL INVALID COMMAREA                           *      ELTIOB  
00230 *                                                          *      ELTIOB  
00231 ************************************************************      ELTIOB  
00232  SIGNAL-INVALID-COMMAREA.                                         ELTIOB  
00233      EXEC CICS ABEND                                              ELTIOB  
00234                ABCODE('EL01')                                     ELTIOB  
00235         END-EXEC.                                                 ELTIOB  
00236      EJECT                                                        ELTIOB  
00237                                                                   ELTIOB  
00238                                                                   ELTIOB  
00239 ************************************************************      ELTIOB  
00240 *                                                          *      ELTIOB  
00241 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTIOB  
00242 *                                                          *      ELTIOB  
00243 ************************************************************      ELTIOB  
00244  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTIOB  
00245      IF ECA-CIA-PTR = NULL                                        ELTIOB  
00246          PERFORM SIGNAL-INVALID-CIA                               ELTIOB  
00247      ELSE                                                         ELTIOB  
00248          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTIOB  
00249                                                                   ELTIOB  
00250                                                                   ELTIOB  
00251 ************************************************************      ELTIOB  
00252 *                                                          *      ELTIOB  
00253 *        ESTABLISH ADDRESS OF CIA                          *      ELTIOB  
00254 *                                                          *      ELTIOB  
00255 ************************************************************      ELTIOB  
00256  ESTABLISH-ADDRESS-OF-CIA.                                        ELTIOB  
00257      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTIOB  
00258                            ADDRESS OF                             ELTIOB  
00259          CIA-ELS-COMMON-INTERFACE-AREA.                           ELTIOB  
00260                                                                   ELTIOB  
00261                                                                   ELTIOB  
00262 ************************************************************      ELTIOB  
00263 *                                                          *      ELTIOB  
00264 *        SIGNAL INVALID CIA                                *      ELTIOB  
00265 *                                                          *      ELTIOB  
00266 ************************************************************      ELTIOB  
00267  SIGNAL-INVALID-CIA.                                              ELTIOB  
00268      EXEC CICS ABEND                                              ELTIOB  
00269                ABCODE('EL02')                                     ELTIOB  
00270         END-EXEC.                                                 ELTIOB  
00271      EJECT                                                        ELTIOB  
00272                                                                   ELTIOB  
00273                                                                   ELTIOB  
00274 ************************************************************      ELTIOB  
00275 *                                                          *      ELTIOB  
00276 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTIOB  
00277 *                                                          *      ELTIOB  
00278 ************************************************************      ELTIOB  
00279  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTIOB  
00280      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTIOB  
00281      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTIOB  
00282                            ADDRESS OF                             ELTIOB  
00283          SSB-SELECTOR-STATUS-CTL-BLK.                             ELTIOB  
00284      IF CIA-RC-PTR-NULL                                           ELTIOB  
00285          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTIOB  
00286                                                                   ELTIOB  
00287                                                                   ELTIOB  
00288 ************************************************************      ELTIOB  
00289 *                                                          *      ELTIOB  
00290 *        SIGNAL UNALLOC AREA ERROR                         *      ELTIOB  
00291 *                                                          *      ELTIOB  
00292 ************************************************************      ELTIOB  
00293  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTIOB  
00294      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTIOB  
00295      PERFORM SIGNAL-ABEND.                                        ELTIOB  
00296                                                                   ELTIOB  
00297                                                                   ELTIOB  
00298 ************************************************************      ELTIOB  
00299 *                                                          *      ELTIOB  
00300 *        SIGNAL ABEND                                      *      ELTIOB  
00301 *                                                          *      ELTIOB  
00302 ************************************************************      ELTIOB  
00303  SIGNAL-ABEND.                                                    ELTIOB  
00304      EXEC CICS ABEND                                              ELTIOB  
00305                ABCODE(CIA-ABCODE)                                 ELTIOB  
00306         END-EXEC.                                                 ELTIOB  
00307      EJECT                                                        ELTIOB  
00308                                                                   ELTIOB  
00309                                                                   ELTIOB  
00310 ************************************************************      ELTIOB  
00311 *                                                          *      ELTIOB  
00312 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTIOB  
00313 *                                                          *      ELTIOB  
00314 ************************************************************      ELTIOB  
00315  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTIOB  
00316      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTIOB  
00317      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTIOB  
00318      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTIOB  
00319      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTIOB  
00320      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTIOB  
00321      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTIOB  
00322      PERFORM ESTABLISH-ADDRESSABILITY-OF-CS.                      ELTIOB  
00323                                                                   ELTIOB  
00324                                                                   ELTIOB  
00325 ************************************************************      ELTIOB  
00326 *                                                          *      ELTIOB  
00327 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTIOB  
00328 *                                                          *      ELTIOB  
00329 ************************************************************      ELTIOB  
00330  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTIOB  
00331      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTIOB  
00332      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTIOB  
00333                            ADDRESS OF                             ELTIOB  
00334          CMF-CODES-MANUAL-INTERFACE.                              ELTIOB  
00335      IF CIA-RC-PTR-NULL                                           ELTIOB  
00336          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTIOB  
00337      EJECT                                                        ELTIOB  
00338                                                                   ELTIOB  
00339                                                                   ELTIOB  
00340 ************************************************************      ELTIOB  
00341 *                                                          *      ELTIOB  
00342 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTIOB  
00343 *                                                          *      ELTIOB  
00344 ************************************************************      ELTIOB  
00345  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTIOB  
00346      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTIOB  
00347      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTIOB  
00348                            ADDRESS OF                             ELTIOB  
00349          COF-OUTPUT-INTERFACE.                                    ELTIOB  
00350      IF CIA-RC-PTR-NULL                                           ELTIOB  
00351          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTIOB  
00352      EJECT                                                        ELTIOB  
00353                                                                   ELTIOB  
00354                                                                   ELTIOB  
00355 ************************************************************      ELTIOB  
00356 *                                                          *      ELTIOB  
00357 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTIOB  
00358 *                                                          *      ELTIOB  
00359 ************************************************************      ELTIOB  
00360  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTIOB  
00361      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTIOB  
00362      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTIOB  
00363                            ADDRESS OF                             ELTIOB  
00364          SRP-SUBROUTINE-PARAMETERS.                               ELTIOB  
00365      IF CIA-RC-PTR-NULL                                           ELTIOB  
00366          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTIOB  
00367      EJECT                                                        ELTIOB  
00368                                                                   ELTIOB  
00369                                                                   ELTIOB  
00370 ************************************************************      ELTIOB  
00371 *                                                          *      ELTIOB  
00372 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTIOB  
00373 *                                                          *      ELTIOB  
00374 ************************************************************      ELTIOB  
00375  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTIOB  
00376      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTIOB  
00377      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTIOB  
00378                            ADDRESS OF                             ELTIOB  
00379          TCAR-COMPRESSION-WORK-AREA.                              ELTIOB  
00380      IF CIA-RC-PTR-NULL                                           ELTIOB  
00381          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTIOB  
00382      EJECT                                                        ELTIOB  
00383                                                                   ELTIOB  
00384                                                                   ELTIOB  
00385 ************************************************************      ELTIOB  
00386 *                                                          *      ELTIOB  
00387 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTIOB  
00388 *                                                          *      ELTIOB  
00389 ************************************************************      ELTIOB  
00390  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTIOB  
00391      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTIOB  
00392      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTIOB  
00393                            ADDRESS OF                             ELTIOB  
00394          KWA-FILE-KEY-WORK-AREA.                                  ELTIOB  
00395      IF CIA-RC-PTR-NULL                                           ELTIOB  
00396          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTIOB  
00397      EJECT                                                        ELTIOB  
00398                                                                   ELTIOB  
00399                                                                   ELTIOB  
00400 ************************************************************      ELTIOB  
00401 *                                                          *      ELTIOB  
00402 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELTIOB  
00403 *                                                          *      ELTIOB  
00404 ************************************************************      ELTIOB  
00405  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTIOB  
00406      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTIOB  
00407      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTIOB  
00408                            ADDRESS OF                             ELTIOB  
00409          GROUP-SPECIFIC-REC.                                      ELTIOB  
00410      IF CIA-RC-PTR-NULL                                           ELTIOB  
00411          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTIOB  
00412      EJECT                                                        ELTIOB  
00413                                                                   ELTIOB  
00414                                                                   ELTIOB  
00415 ************************************************************      ELTIOB  
00416 *                                                          *      ELTIOB  
00417 *        ESTABLISH ADDRESSABILITY OF CST CONTAINMENT PROGRA*      ELTIOB  
00418 *                                                          *      ELTIOB  
00419 ************************************************************      ELTIOB  
00420  ESTABLISH-ADDRESSABILITY-OF-CS.                                  ELTIOB  
00421      SET CIA-GCTABULR-DDN TO TRUE.                                ELTIOB  
00422      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTIOB  
00423                            ADDRESS OF                             ELTIOB  
00424          GCCP-TABULAR-REC-AREA.                                   ELTIOB  
00425      IF CIA-RC-PTR-NULL                                           ELTIOB  
00426          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTIOB  
00427      EJECT                                                        ELTIOB  
00428                                                                   ELTIOB  
00429                                                                   ELTIOB  
00430 ************************************************************      ELTIOB  
00431 *                                                          *      ELTIOB  
00432 *        PROCESS                                           *      ELTIOB  
00433 *                                                          *      ELTIOB  
00434 ************************************************************      ELTIOB  
00435  PROCESS.                                                         ELTIOB  
00436      IF GCG-INCENTIVE-OB-IND  =  '00'                             ELTIOB  
00437          PERFORM GENERATE-NOT-APPLICABLE-MESSAG                   ELTIOB  
00438      ELSE IF GCG-INCENTIVE-OB-IND  =  '08'                        ELTIOB  
00439          PERFORM GENERATE-VOLUNTARY-MESSAGE                       ELTIOB  
00440      ELSE                                                         ELTIOB  
00441          PERFORM GENERATE-IOB-TEXT.                               ELTIOB  
00442      PERFORM TERMINATE-OUTPUT.                                    ELTIOB  
00443                                                                   ELTIOB  
00444                                                                   ELTIOB  
00445 ************************************************************      ELTIOB  
00446 *                                                          *      ELTIOB  
00447 *        GENERATE NOT APPLICABLE MESSAGE                   *      ELTIOB  
00448 *                                                          *      ELTIOB  
00449 ************************************************************      ELTIOB  
00450  GENERATE-NOT-APPLICABLE-MESSAG.                                  ELTIOB  
00451      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTIOB  
00452      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTIOB  
00453      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTIOB  
00454      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTIOB  
00455          (COF-NBR-DTL-LINES).                                     ELTIOB  
00456      PERFORM EJECT-NEW-PAGE.                                      ELTIOB  
00457      EJECT                                                        ELTIOB  
00458                                                                   ELTIOB  
00459                                                                   ELTIOB  
00460 ************************************************************      ELTIOB  
00461 *                                                          *      ELTIOB  
00462 *        GENERATE VOLUNTARY MESSAGE                        *      ELTIOB  
00463 *                                                          *      ELTIOB  
00464 ************************************************************      ELTIOB  
00465  GENERATE-VOLUNTARY-MESSAGE.                                      ELTIOB  
00466      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTIOB  
00467      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTIOB  
00468      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTIOB  
00469      MOVE WS-VOLUNTARY-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).   ELTIOB  
00470                                                                   ELTIOB  
00471                                                                   ELTIOB  
00472 ************************************************************      ELTIOB  
00473 *                                                          *      ELTIOB  
00474 *        SET UP OUTPUT SUBSCRIPTS                          *      ELTIOB  
00475 *                                                          *      ELTIOB  
00476 ************************************************************      ELTIOB  
00477  SET-UP-OUTPUT-SUBSCRIPTS.                                        ELTIOB  
00478      MOVE +2 TO COF-NBR-HDR-LINES.                                ELTIOB  
00479      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTIOB  
00480      EJECT                                                        ELTIOB  
00481                                                                   ELTIOB  
00482                                                                   ELTIOB  
00483 ************************************************************      ELTIOB  
00484 *                                                          *      ELTIOB  
00485 *        EJECT NEW PAGE                                    *      ELTIOB  
00486 *                                                          *      ELTIOB  
00487 ************************************************************      ELTIOB  
00488  EJECT-NEW-PAGE.                                                  ELTIOB  
00489      SET COF-NEW-PAGE     TO TRUE.                                ELTIOB  
00490      MOVE WS-HEADER-LINE  TO COF-HDR-LINE (COF-NBR-HDR-LINES).    ELTIOB  
00491      PERFORM LINK-TO-OUTPUT.                                      ELTIOB  
00492      EJECT                                                        ELTIOB  
00493                                                                   ELTIOB  
00494                                                                   ELTIOB  
00495 ************************************************************      ELTIOB  
00496 *                                                          *      ELTIOB  
00497 *        GENERATE IOB TEXT                                 *      ELTIOB  
00498 *                                                          *      ELTIOB  
00499 ************************************************************      ELTIOB  
00500  GENERATE-IOB-TEXT.                                               ELTIOB  
00501      SET WS-POINTER2 TO NULL.                                     ELTIOB  
00502      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTIOB  
00503      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTIOB  
00504                            WS-POINTER2.                           ELTIOB  
00505      SET WS-POINTER3 TO NULL.                                     ELTIOB  
00506      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTIOB  
00507      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTIOB  
00508                            WS-POINTER3.                           ELTIOB  
00509      PERFORM SEARCH-FOR-GCCP-TABULAR.                             ELTIOB  
00510      PERFORM DETERMINE-SELECTION.                                 ELTIOB  
00511      EJECT                                                        ELTIOB  
00512                                                                   ELTIOB  
00513                                                                   ELTIOB  
00514 ************************************************************      ELTIOB  
00515 *                                                          *      ELTIOB  
00516 *        SEARCH FOR GCCP TABULAR                           *      ELTIOB  
00517 *                                                          *      ELTIOB  
00518 ************************************************************      ELTIOB  
00519  SEARCH-FOR-GCCP-TABULAR.                                         ELTIOB  
00520      SET GCG-INDEX TO +1.                                         ELTIOB  
00521      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTIOB  
00522         AT END                                                    ELTIOB  
00523              MOVE ZEROES TO KWA-PROVISION-SLOT-NO                 ELTIOB  
00524         WHEN GCG-TAB-ID (GCG-INDEX) = PC-GCCP                     ELTIOB  
00525              MOVE GCG-TAB-ID (GCG-INDEX)                          ELTIOB  
00526                  TO KWA-PROVISION-ID                              ELTIOB  
00527              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTIOB  
00528                  TO KWA-PROVISION-SLOT-NO                         ELTIOB  
00529         END-SEARCH.                                               ELTIOB  
00530      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELTIOB  
00531          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTIOB  
00532      PERFORM GET-GCCP-TABULAR.                                    ELTIOB  
00533      PERFORM SEARCH-THE-GSS-ENTRY.                                ELTIOB  
00534      EJECT                                                        ELTIOB  
00535                                                                   ELTIOB  
00536                                                                   ELTIOB  
00537 ************************************************************      ELTIOB  
00538 *                                                          *      ELTIOB  
00539 *        SEARCH THE GSS ENTRY                              *      ELTIOB  
00540 *                                                          *      ELTIOB  
00541 ************************************************************      ELTIOB  
00542  SEARCH-THE-GSS-ENTRY.                                            ELTIOB  
00543      SET GSS-INDEX TO 1.                                          ELTIOB  
00544      SEARCH GSS-ENTRY                                             ELTIOB  
00545          AT END                                                   ELTIOB  
00546               SET TABULAR-IS-UNDEFINED TO TRUE                    ELTIOB  
00547          WHEN GSS-IO-PROG-CODE-CHR (GSS-INDEX)                    ELTIOB  
00548               CONTINUE                                            ELTIOB  
00549          END-SEARCH.                                              ELTIOB  
00550      IF TABULAR-IS-UNDEFINED                                      ELTIOB  
00551          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTIOB  
00552                                                                   ELTIOB  
00553                                                                   ELTIOB  
00554 ************************************************************      ELTIOB  
00555 *                                                          *      ELTIOB  
00556 *        SIGNAL UNDEFINED TABULAR ERROR                    *      ELTIOB  
00557 *                                                          *      ELTIOB  
00558 ************************************************************      ELTIOB  
00559  SIGNAL-UNDEFINED-TABULAR-ERROR.                                  ELTIOB  
00560      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTIOB  
00561      PERFORM SIGNAL-ABEND.                                        ELTIOB  
00562      EJECT                                                        ELTIOB  
00563                                                                   ELTIOB  
00564                                                                   ELTIOB  
00565 ************************************************************      ELTIOB  
00566 *                                                          *      ELTIOB  
00567 *        DETERMINE SELECTION                               *      ELTIOB  
00568 *                                                          *      ELTIOB  
00569 ************************************************************      ELTIOB  
00570  DETERMINE-SELECTION.                                             ELTIOB  
00571      IF (SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH)              ELTIOB  
00572          PERFORM CREATE-INSTITUTIONAL-SCREEN.                     ELTIOB  
00573      IF (SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH)              ELTIOB  
00574          PERFORM CREATE-PROFESSIONAL-SCREEN.                      ELTIOB  
00575      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL '03' OR '04' OR       ELTIOB  
00576                  '06' OR '08')                                    ELTIOB  
00577          PERFORM CREATE-SUPPLEMENTAL-SCREEN.                      ELTIOB  
00578                                                                   ELTIOB  
00579                                                                   ELTIOB  
00580 ************************************************************      ELTIOB  
00581 *                                                          *      ELTIOB  
00582 *        CREATE INSTITUTIONAL SCREEN                       *      ELTIOB  
00583 *                                                          *      ELTIOB  
00584 ************************************************************      ELTIOB  
00585  CREATE-INSTITUTIONAL-SCREEN.                                     ELTIOB  
00586      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTIOB  
00587      MOVE PC-INST TO WS-HDR-LINE-BCBSMM.                          ELTIOB  
00588      IF GSS-IO-BC-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTIOB  
00589          ZEROES                                                   ELTIOB  
00590                 AND LOW-VALUES                                    ELTIOB  
00591          PERFORM GENERATE-INSTITUTIONAL-TEXT                      ELTIOB  
00592      ELSE                                                         ELTIOB  
00593          PERFORM GENERATE-INSTITUTIONAL-NOT-APP.                  ELTIOB  
00594      EJECT                                                        ELTIOB  
00595                                                                   ELTIOB  
00596                                                                   ELTIOB  
00597 ************************************************************      ELTIOB  
00598 *                                                          *      ELTIOB  
00599 *        GENERATE INSTITUTIONAL TEXT                       *      ELTIOB  
00600 *                                                          *      ELTIOB  
00601 ************************************************************      ELTIOB  
00602  GENERATE-INSTITUTIONAL-TEXT.                                     ELTIOB  
00603      SET NOT-HOLDING-IOB-PROG TO TRUE.                            ELTIOB  
00604      PERFORM EJECT-NEW-PAGE.                                      ELTIOB  
00605      PERFORM TRANSLATE-AND-DISPLAY-IOB-IND.                       ELTIOB  
00606      IF GSS-IO-PROG-IND (GSS-INDEX) NOT EQUAL SPACES AND          ELTIOB  
00607          ZEROES                                                   ELTIOB  
00608                 AND LOW-VALUES                                    ELTIOB  
00609          PERFORM TRANSLATE-IOB-PROG-IND.                          ELTIOB  
00610      PERFORM TRANSLATE-BC-IND.                                    ELTIOB  
00611      IF ((GSS-IO-INCENTIVE-AMT-1 (GSS-INDEX) NOT EQUAL ZEROES))   ELTIOB  
00612          OR                                                       ELTIOB  
00613                 ((GSS-IO-INCENTIVE-AMT-2 (GSS-INDEX) NOT EQUAL    ELTIOB  
00614          ZEROES))                                                 ELTIOB  
00615          PERFORM CHECK-INCENTIVE-AMOUNTS-ONE-AN.                  ELTIOB  
00616      SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTIOB  
00617      PERFORM GENERATE-ASSOCIATED-TABULAR-DA.                      ELTIOB  
00618      IF GSS-IO-BC-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES        ELTIOB  
00619          AND                                                      ELTIOB  
00620                 ZEROES AND LOW-VALUES                             ELTIOB  
00621          PERFORM TRANSLATE-IOB-BC-CALC-METHOD.                    ELTIOB  
00622      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTIOB  
00623                                                                   ELTIOB  
00624                                                                   ELTIOB  
00625 ************************************************************      ELTIOB  
00626 *                                                          *      ELTIOB  
00627 *        GENERATE INSTITUTIONAL NOT APPLICABLE             *      ELTIOB  
00628 *                                                          *      ELTIOB  
00629 ************************************************************      ELTIOB  
00630  GENERATE-INSTITUTIONAL-NOT-APP.                                  ELTIOB  
00631      MOVE WS-NOT-APPLY-TO-LOB-BC TO COF-DTL-LINE                  ELTIOB  
00632          (COF-NBR-DTL-LINES).                                     ELTIOB  
00633      PERFORM EJECT-NEW-PAGE.                                      ELTIOB  
00634      EJECT                                                        ELTIOB  
00635                                                                   ELTIOB  
00636                                                                   ELTIOB  
00637 ************************************************************      ELTIOB  
00638 *                                                          *      ELTIOB  
00639 *        GENERATE DISCLAIMER MESSAGE                       *      ELTIOB  
00640 *                                                          *      ELTIOB  
00641 ************************************************************      ELTIOB  
00642  GENERATE-DISCLAIMER-MESSAGE.                                     ELTIOB  
00643      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTIOB  
00644      MOVE WS-DISCLAIMER-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).  ELTIOB  
00645      ADD  +1 TO COF-NBR-DTL-LINES.                                ELTIOB  
00646      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTIOB  
00647      PERFORM LINK-TO-OUTPUT.                                      ELTIOB  
00648      EJECT                                                        ELTIOB  
00649                                                                   ELTIOB  
00650                                                                   ELTIOB  
00651 ************************************************************      ELTIOB  
00652 *                                                          *      ELTIOB  
00653 *        TRANSLATE AND DISPLAY IOB IND                     *      ELTIOB  
00654 *                                                          *      ELTIOB  
00655 ************************************************************      ELTIOB  
00656  TRANSLATE-AND-DISPLAY-IOB-IND.                                   ELTIOB  
00657      INITIALIZE TCAR-FROM-AREA.                                   ELTIOB  
00658      MOVE +1 TO TCAR-FROM-SUB.                                    ELTIOB  
00659      MOVE WS-IOB-APPLIES TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELTIOB  
00660      ADD  +1 TO TCAR-FROM-SUB.                                    ELTIOB  
00661      SET BLANK-LINE-NEEDED TO TRUE.                               ELTIOB  
00662      SET PERIOD-NEEDED TO TRUE.                                   ELTIOB  
00663      MOVE GCG-INCENTIVE-OB-IND TO CMF-CODE-VALUE.                 ELTIOB  
00664      MOVE 'INCENTIVE-OB-IND' TO CMF-ELEMENT-SYSTEM-NAME.          ELTIOB  
00665      MOVE PC-GRP TO CMF-RECORD-PREFIX.                            ELTIOB  
00666      EXEC CICS LINK                                               ELTIOB  
00667                PROGRAM ('ELUCMIF')                                ELTIOB  
00668                COMMAREA (DFHCOMMAREA)                             ELTIOB  
00669         END-EXEC.                                                 ELTIOB  
00670      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTIOB  
00671      MOVE SPACE TO ADDITIONAL-TEXT-SW.                            ELTIOB  
00672      EJECT                                                        ELTIOB  
00673                                                                   ELTIOB  
00674                                                                   ELTIOB  
00675 ************************************************************      ELTIOB  
00676 *                                                          *      ELTIOB  
00677 *        TRANSLATE IOB PROG IND                            *      ELTIOB  
00678 *                                                          *      ELTIOB  
00679 ************************************************************      ELTIOB  
00680  TRANSLATE-IOB-PROG-IND.                                          ELTIOB  
00681      INITIALIZE TCAR-FROM-AREA.                                   ELTIOB  
00682      MOVE +1 TO TCAR-FROM-SUB.                                    ELTIOB  
00683      SET BLANK-LINE-NEEDED TO TRUE.                               ELTIOB  
00684      SET PERIOD-NEEDED TO TRUE.                                   ELTIOB  
00685      IF NOT-HOLDING-IOB-PROG                                      ELTIOB  
00686          PERFORM GET-PROGRAM-IND-TRANSLATION                      ELTIOB  
00687      ELSE                                                         ELTIOB  
00688          PERFORM USE-EXISTING-PROGRAM-TRANSLATI.                  ELTIOB  
00689      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTIOB  
00690      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTIOB  
00691      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTIOB  
00692                            WS-POINTER3.                           ELTIOB  
00693      EJECT                                                        ELTIOB  
00694                                                                   ELTIOB  
00695                                                                   ELTIOB  
00696 ************************************************************      ELTIOB  
00697 *                                                          *      ELTIOB  
00698 *        GET PROGRAM IND TRANSLATION                       *      ELTIOB  
00699 *                                                          *      ELTIOB  
00700 ************************************************************      ELTIOB  
00701  GET-PROGRAM-IND-TRANSLATION.                                     ELTIOB  
00702      MOVE GSS-IO-PROG-IND (GSS-INDEX) TO                          ELTIOB  
00703          CMF-CODE-VALUE.                                          ELTIOB  
00704      MOVE 'IO-PROG-IND'               TO CMF-ELEMENT-SYSTEM-NAME. ELTIOB  
00705      SET HOLDING-IOB-PROG TO TRUE.                                ELTIOB  
00706      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTIOB  
00707      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTIOB  
00708      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTIOB  
00709                            WS-POINTER3.                           ELTIOB  
00710      EJECT                                                        ELTIOB  
00711                                                                   ELTIOB  
00712                                                                   ELTIOB  
00713 ************************************************************      ELTIOB  
00714 *                                                          *      ELTIOB  
00715 *        USE EXISTING PROGRAM TRANSLATION                  *      ELTIOB  
00716 *                                                          *      ELTIOB  
00717 ************************************************************      ELTIOB  
00718  USE-EXISTING-PROGRAM-TRANSLATI.                                  ELTIOB  
00719      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTIOB  
00720      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTIOB  
00721                            ADDRESS OF CMF-DESCR.                  ELTIOB  
00722      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTIOB  
00723      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTIOB  
00724                            WS-POINTER2.                           ELTIOB  
00725      EJECT                                                        ELTIOB  
00726                                                                   ELTIOB  
00727                                                                   ELTIOB  
00728 ************************************************************      ELTIOB  
00729 *                                                          *      ELTIOB  
00730 *        TRANSLATE BC IND                                  *      ELTIOB  
00731 *                                                          *      ELTIOB  
00732 ************************************************************      ELTIOB  
00733  TRANSLATE-BC-IND.                                                ELTIOB  
00734      INITIALIZE TCAR-FROM-AREA.                                   ELTIOB  
00735      MOVE +1 TO TCAR-FROM-SUB.                                    ELTIOB  
00736      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTIOB  
00737      SET PERIOD-NEEDED TO TRUE.                                   ELTIOB  
00738      MOVE GSS-IO-BC-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTIOB  
00739      MOVE 'IO-BC-IND'               TO                            ELTIOB  
00740          CMF-ELEMENT-SYSTEM-NAME.                                 ELTIOB  
00741      PERFORM TRANSLATE-AND-DISPLAY-VALUE.                         ELTIOB  
00742      EJECT                                                        ELTIOB  
00743                                                                   ELTIOB  
00744                                                                   ELTIOB  
00745 ************************************************************      ELTIOB  
00746 *                                                          *      ELTIOB  
00747 *        TRANSLATE IOB BC CALC METHOD                      *      ELTIOB  
00748 *                                                          *      ELTIOB  
00749 ************************************************************      ELTIOB  
00750  TRANSLATE-IOB-BC-CALC-METHOD.                                    ELTIOB  
00751      INITIALIZE TCAR-FROM-AREA.                                   ELTIOB  
00752      MOVE +1 TO TCAR-FROM-SUB.                                    ELTIOB  
00753      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTIOB  
00754      SET PERIOD-NEEDED TO TRUE.                                   ELTIOB  
00755      MOVE GSS-IO-BC-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTIOB  
00756      MOVE 'IO-BC-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.         ELTIOB  
00757      PERFORM TRANSLATE-AND-DISPLAY-VALUE.                         ELTIOB  
00758      EJECT                                                        ELTIOB  
00759                                                                   ELTIOB  
00760                                                                   ELTIOB  
00761 ************************************************************      ELTIOB  
00762 *                                                          *      ELTIOB  
00763 *        CHECK INCENTIVE AMOUNTS ONE AND TWO               *      ELTIOB  
00764 *                                                          *      ELTIOB  
00765 ************************************************************      ELTIOB  
00766  CHECK-INCENTIVE-AMOUNTS-ONE-AN.                                  ELTIOB  
00767      IF GSS-IO-INCENTIVE-AMT-1 (GSS-INDEX) NOT EQUAL              ELTIOB  
00768          ZEROES                                                   ELTIOB  
00769          PERFORM MOVE-IOB-INCENTIVE-AMT-1-OUT.                    ELTIOB  
00770      IF GSS-IO-INCENTIVE-AMT-2 (GSS-INDEX) NOT EQUAL              ELTIOB  
00771          ZEROES                                                   ELTIOB  
00772          PERFORM MOVE-IOB-INCENTIVE-AMT-2-OUT.                    ELTIOB  
00773      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTIOB  
00774      MOVE WS-PAYMENT-LINE TO COF-DTL-LINE                         ELTIOB  
00775          (COF-NBR-DTL-LINES).                                     ELTIOB  
00776      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTIOB  
00777      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTIOB  
00778      PERFORM LINK-TO-OUTPUT.                                      ELTIOB  
00779                                                                   ELTIOB  
00780                                                                   ELTIOB  
00781 ************************************************************      ELTIOB  
00782 *                                                          *      ELTIOB  
00783 *        MOVE IOB-INCENTIVE-AMT-1 OUT                      *      ELTIOB  
00784 *                                                          *      ELTIOB  
00785 ************************************************************      ELTIOB  
00786  MOVE-IOB-INCENTIVE-AMT-1-OUT.                                    ELTIOB  
00787      MOVE GSS-IO-INCENTIVE-AMT-1 (GSS-INDEX) TO                   ELTIOB  
00788          WS-PYMT-LINE-AMT1.                                       ELTIOB  
00789                                                                   ELTIOB  
00790                                                                   ELTIOB  
00791 ************************************************************      ELTIOB  
00792 *                                                          *      ELTIOB  
00793 *        MOVE IOB-INCENTIVE-AMT-2 OUT                      *      ELTIOB  
00794 *                                                          *      ELTIOB  
00795 ************************************************************      ELTIOB  
00796  MOVE-IOB-INCENTIVE-AMT-2-OUT.                                    ELTIOB  
00797      MOVE GSS-IO-INCENTIVE-AMT-2 (GSS-INDEX) TO                   ELTIOB  
00798          WS-PYMT-LINE-AMT2.                                       ELTIOB  
00799      EJECT                                                        ELTIOB  
00800                                                                   ELTIOB  
00801                                                                   ELTIOB  
00802 ************************************************************      ELTIOB  
00803 *                                                          *      ELTIOB  
00804 *        CREATE PROFESSIONAL SCREEN                        *      ELTIOB  
00805 *                                                          *      ELTIOB  
00806 ************************************************************      ELTIOB  
00807  CREATE-PROFESSIONAL-SCREEN.                                      ELTIOB  
00808      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTIOB  
00809      MOVE PC-PROF TO WS-HDR-LINE-BCBSMM.                          ELTIOB  
00810      IF GSS-IO-BS-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTIOB  
00811          ZEROES                                                   ELTIOB  
00812                 AND LOW-VALUES                                    ELTIOB  
00813          PERFORM GENERATE-PROFESSIONAL-TEXT                       ELTIOB  
00814      ELSE                                                         ELTIOB  
00815          PERFORM GENERATE-PROFESSIONAL-NOT-APPL.                  ELTIOB  
00816      EJECT                                                        ELTIOB  
00817                                                                   ELTIOB  
00818                                                                   ELTIOB  
00819 ************************************************************      ELTIOB  
00820 *                                                          *      ELTIOB  
00821 *        GENERATE PROFESSIONAL TEXT                        *      ELTIOB  
00822 *                                                          *      ELTIOB  
00823 ************************************************************      ELTIOB  
00824  GENERATE-PROFESSIONAL-TEXT.                                      ELTIOB  
00825      PERFORM EJECT-NEW-PAGE.                                      ELTIOB  
00826      PERFORM TRANSLATE-AND-DISPLAY-IOB-IND.                       ELTIOB  
00827      IF GSS-IO-PROG-IND (GSS-INDEX) NOT EQUAL SPACES AND          ELTIOB  
00828          ZEROES                                                   ELTIOB  
00829                 AND LOW-VALUES                                    ELTIOB  
00830          PERFORM TRANSLATE-IOB-PROG-IND.                          ELTIOB  
00831      PERFORM TRANSLATE-BS-IND.                                    ELTIOB  
00832      IF ((GSS-IO-INCENTIVE-AMT-1 (GSS-INDEX) NOT EQUAL ZEROES))   ELTIOB  
00833          OR                                                       ELTIOB  
00834                 ((GSS-IO-INCENTIVE-AMT-2 (GSS-INDEX) NOT EQUAL    ELTIOB  
00835          ZEROES))                                                 ELTIOB  
00836          PERFORM CHECK-INCENTIVE-AMOUNTS-ONE-AN.                  ELTIOB  
00837      SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTIOB  
00838      PERFORM GENERATE-ASSOCIATED-TABULAR-DA.                      ELTIOB  
00839      IF GSS-IO-BS-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES        ELTIOB  
00840          AND                                                      ELTIOB  
00841                 ZEROES AND LOW-VALUES                             ELTIOB  
00842          PERFORM TRANSLATE-IOB-BS-CALC-METHOD.                    ELTIOB  
00843      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTIOB  
00844                                                                   ELTIOB  
00845                                                                   ELTIOB  
00846 ************************************************************      ELTIOB  
00847 *                                                          *      ELTIOB  
00848 *        GENERATE PROFESSIONAL NOT APPLICABLE              *      ELTIOB  
00849 *                                                          *      ELTIOB  
00850 ************************************************************      ELTIOB  
00851  GENERATE-PROFESSIONAL-NOT-APPL.                                  ELTIOB  
00852      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTIOB  
00853      ADD  +1 TO COF-NBR-DTL-LINES.                                ELTIOB  
00854      MOVE WS-NOT-APPLY-TO-LOB-BS TO COF-DTL-LINE                  ELTIOB  
00855          (COF-NBR-DTL-LINES).                                     ELTIOB  
00856      PERFORM EJECT-NEW-PAGE.                                      ELTIOB  
00857      EJECT                                                        ELTIOB  
00858                                                                   ELTIOB  
00859                                                                   ELTIOB  
00860 ************************************************************      ELTIOB  
00861 *                                                          *      ELTIOB  
00862 *        TRANSLATE BS IND                                  *      ELTIOB  
00863 *                                                          *      ELTIOB  
00864 ************************************************************      ELTIOB  
00865  TRANSLATE-BS-IND.                                                ELTIOB  
00866      INITIALIZE TCAR-FROM-AREA.                                   ELTIOB  
00867      MOVE +1 TO TCAR-FROM-SUB.                                    ELTIOB  
00868      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTIOB  
00869      SET PERIOD-NEEDED TO TRUE.                                   ELTIOB  
00870      MOVE GSS-IO-BS-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTIOB  
00871      MOVE 'IO-BS-IND'  TO CMF-ELEMENT-SYSTEM-NAME.                ELTIOB  
00872      PERFORM TRANSLATE-AND-DISPLAY-VALUE.                         ELTIOB  
00873      EJECT                                                        ELTIOB  
00874                                                                   ELTIOB  
00875                                                                   ELTIOB  
00876 ************************************************************      ELTIOB  
00877 *                                                          *      ELTIOB  
00878 *        TRANSLATE IOB BS CALC METHOD                      *      ELTIOB  
00879 *                                                          *      ELTIOB  
00880 ************************************************************      ELTIOB  
00881  TRANSLATE-IOB-BS-CALC-METHOD.                                    ELTIOB  
00882      INITIALIZE TCAR-FROM-AREA.                                   ELTIOB  
00883      MOVE +1 TO TCAR-FROM-SUB.                                    ELTIOB  
00884      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTIOB  
00885      SET PERIOD-NEEDED TO TRUE.                                   ELTIOB  
00886      MOVE GSS-IO-BS-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTIOB  
00887      MOVE 'IO-BS-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.         ELTIOB  
00888      PERFORM TRANSLATE-AND-DISPLAY-VALUE.                         ELTIOB  
00889      EJECT                                                        ELTIOB  
00890                                                                   ELTIOB  
00891                                                                   ELTIOB  
00892 ************************************************************      ELTIOB  
00893 *                                                          *      ELTIOB  
00894 *        CREATE SUPPLEMENTAL SCREEN                        *      ELTIOB  
00895 *                                                          *      ELTIOB  
00896 ************************************************************      ELTIOB  
00897  CREATE-SUPPLEMENTAL-SCREEN.                                      ELTIOB  
00898      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTIOB  
00899      MOVE PC-SUPP TO WS-HDR-LINE-BCBSMM.                          ELTIOB  
00900      IF GSS-IO-MM-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTIOB  
00901          ZEROES                                                   ELTIOB  
00902                 AND LOW-VALUES                                    ELTIOB  
00903          PERFORM GENERATE-SUPPLEMENTAL-TEXT                       ELTIOB  
00904      ELSE                                                         ELTIOB  
00905          PERFORM GENERATE-SUPPLEMENTAL-NOT-APPL.                  ELTIOB  
00906      EJECT                                                        ELTIOB  
00907                                                                   ELTIOB  
00908                                                                   ELTIOB  
00909 ************************************************************      ELTIOB  
00910 *                                                          *      ELTIOB  
00911 *        GENERATE SUPPLEMENTAL TEXT                        *      ELTIOB  
00912 *                                                          *      ELTIOB  
00913 ************************************************************      ELTIOB  
00914  GENERATE-SUPPLEMENTAL-TEXT.                                      ELTIOB  
00915      PERFORM EJECT-NEW-PAGE.                                      ELTIOB  
00916      PERFORM TRANSLATE-AND-DISPLAY-IOB-IND.                       ELTIOB  
00917      IF GSS-IO-PROG-IND (GSS-INDEX) NOT EQUAL SPACES AND          ELTIOB  
00918          ZEROES                                                   ELTIOB  
00919                 AND LOW-VALUES                                    ELTIOB  
00920          PERFORM TRANSLATE-IOB-PROG-IND.                          ELTIOB  
00921      PERFORM TRANSLATE-MM-IND.                                    ELTIOB  
00922      IF ((GSS-IO-INCENTIVE-AMT-1 (GSS-INDEX) NOT EQUAL ZEROES))   ELTIOB  
00923          OR                                                       ELTIOB  
00924                 ((GSS-IO-INCENTIVE-AMT-2 (GSS-INDEX) NOT EQUAL    ELTIOB  
00925          ZEROES))                                                 ELTIOB  
00926          PERFORM CHECK-INCENTIVE-AMOUNTS-ONE-AN.                  ELTIOB  
00927      SET SRP-ACCUM-PROV-CLASS-SUPP TO TRUE.                       ELTIOB  
00928      PERFORM GENERATE-ASSOCIATED-TABULAR-DA.                      ELTIOB  
00929      IF GSS-IO-MM-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES        ELTIOB  
00930          AND                                                      ELTIOB  
00931                 ZEROES AND LOW-VALUES                             ELTIOB  
00932          PERFORM TRANSLATE-IOB-MM-CALC-METHOD.                    ELTIOB  
00933      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTIOB  
00934                                                                   ELTIOB  
00935                                                                   ELTIOB  
00936 ************************************************************      ELTIOB  
00937 *                                                          *      ELTIOB  
00938 *        GENERATE SUPPLEMENTAL NOT APPLICABLE              *      ELTIOB  
00939 *                                                          *      ELTIOB  
00940 ************************************************************      ELTIOB  
00941  GENERATE-SUPPLEMENTAL-NOT-APPL.                                  ELTIOB  
00942      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTIOB  
00943      ADD  +1 TO COF-NBR-DTL-LINES.                                ELTIOB  
00944      MOVE WS-NOT-APPLY-TO-LOB-MM TO COF-DTL-LINE                  ELTIOB  
00945          (COF-NBR-DTL-LINES).                                     ELTIOB  
00946      PERFORM EJECT-NEW-PAGE.                                      ELTIOB  
00947      EJECT                                                        ELTIOB  
00948                                                                   ELTIOB  
00949                                                                   ELTIOB  
00950 ************************************************************      ELTIOB  
00951 *                                                          *      ELTIOB  
00952 *        TRANSLATE MM IND                                  *      ELTIOB  
00953 *                                                          *      ELTIOB  
00954 ************************************************************      ELTIOB  
00955  TRANSLATE-MM-IND.                                                ELTIOB  
00956      INITIALIZE TCAR-FROM-AREA.                                   ELTIOB  
00957      MOVE +1 TO TCAR-FROM-SUB.                                    ELTIOB  
00958      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTIOB  
00959      SET PERIOD-NEEDED TO TRUE.                                   ELTIOB  
00960      MOVE GSS-IO-MM-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTIOB  
00961      MOVE 'IO-MM-IND'  TO CMF-ELEMENT-SYSTEM-NAME.                ELTIOB  
00962      PERFORM TRANSLATE-AND-DISPLAY-VALUE.                         ELTIOB  
00963      EJECT                                                        ELTIOB  
00964                                                                   ELTIOB  
00965                                                                   ELTIOB  
00966 ************************************************************      ELTIOB  
00967 *                                                          *      ELTIOB  
00968 *        TRANSLATE IOB MM CALC METHOD                      *      ELTIOB  
00969 *                                                          *      ELTIOB  
00970 ************************************************************      ELTIOB  
00971  TRANSLATE-IOB-MM-CALC-METHOD.                                    ELTIOB  
00972      INITIALIZE TCAR-FROM-AREA.                                   ELTIOB  
00973      MOVE +1 TO TCAR-FROM-SUB.                                    ELTIOB  
00974      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTIOB  
00975      SET PERIOD-NEEDED TO TRUE.                                   ELTIOB  
00976      MOVE GSS-IO-MM-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTIOB  
00977      MOVE 'IO-MM-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.         ELTIOB  
00978      PERFORM TRANSLATE-AND-DISPLAY-VALUE.                         ELTIOB  
00979                                                                   ELTIOB  
00980                                                                   ELTIOB  
00981 ************************************************************      ELTIOB  
00982 *                                                          *      ELTIOB  
00983 *        TRANSLATE AND DISPLAY VALUE                       *      ELTIOB  
00984 *                                                          *      ELTIOB  
00985 ************************************************************      ELTIOB  
00986  TRANSLATE-AND-DISPLAY-VALUE.                                     ELTIOB  
00987      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTIOB  
00988      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTIOB  
00989                                                                   ELTIOB  
00990                                                                   ELTIOB  
00991 ************************************************************      ELTIOB  
00992 *                                                          *      ELTIOB  
00993 *        CALL CODES MANUAL INTERFACE                       *      ELTIOB  
00994 *                                                          *      ELTIOB  
00995 ************************************************************      ELTIOB  
00996  CALL-CODES-MANUAL-INTERFACE.                                     ELTIOB  
00997      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTIOB  
00998      EXEC CICS LINK                                               ELTIOB  
00999                PROGRAM ('ELUCMIF')                                ELTIOB  
01000                COMMAREA (DFHCOMMAREA)                             ELTIOB  
01001         END-EXEC.                                                 ELTIOB  
01002      EJECT                                                        ELTIOB  
01003                                                                   ELTIOB  
01004                                                                   ELTIOB  
01005 ************************************************************      ELTIOB  
01006 *                                                          *      ELTIOB  
01007 *        GET GCCP TABULAR                                  *      ELTIOB  
01008 *                                                          *      ELTIOB  
01009 ************************************************************      ELTIOB  
01010  GET-GCCP-TABULAR.                                                ELTIOB  
01011      PERFORM ESTABLISH-ADDRESSITY-OF-GCTABU.                      ELTIOB  
01012      SET IOP-RD              TO TRUE.                             ELTIOB  
01013      SET IOP-STG-MODE-MOVE   TO TRUE.                             ELTIOB  
01014      SET IOP-FCQ-NONE        TO TRUE.                             ELTIOB  
01015      SET IOP-KVQ-EQ          TO TRUE.                             ELTIOB  
01016      MOVE KWA-GCTABULR-KEY   TO IOP-FILE-KEY.                     ELTIOB  
01017      PERFORM CALL-INPUT-OUTPUT-MODULE.                            ELTIOB  
01018      IF IOP-RC-OK                                                 ELTIOB  
01019          PERFORM ESTABLISH-ADDRESSABILITY-OF-GC                   ELTIOB  
01020      ELSE IF IOP-RC-NOTFND                                        ELTIOB  
01021          PERFORM SIGNAL-NOT-FOUND-GCTAB-ERROR                     ELTIOB  
01022      ELSE                                                         ELTIOB  
01023          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTIOB  
01024      EJECT                                                        ELTIOB  
01025                                                                   ELTIOB  
01026                                                                   ELTIOB  
01027 ************************************************************      ELTIOB  
01028 *                                                          *      ELTIOB  
01029 *        ESTABLISH ADDRESSITY OF GCTABULAR IO PARAMETER BLO*      ELTIOB  
01030 *                                                          *      ELTIOB  
01031 ************************************************************      ELTIOB  
01032  ESTABLISH-ADDRESSITY-OF-GCTABU.                                  ELTIOB  
01033      SET CIA-GCTABULR-DDN TO TRUE.                                ELTIOB  
01034      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTIOB  
01035                            ADDRESS OF                             ELTIOB  
01036          IOP-INPUT-OUTPUT-PARAMETERS.                             ELTIOB  
01037      EJECT                                                        ELTIOB  
01038                                                                   ELTIOB  
01039                                                                   ELTIOB  
01040 ************************************************************      ELTIOB  
01041 *                                                          *      ELTIOB  
01042 *        CALL INPUT OUTPUT MODULE                          *      ELTIOB  
01043 *                                                          *      ELTIOB  
01044 ************************************************************      ELTIOB  
01045  CALL-INPUT-OUTPUT-MODULE.                                        ELTIOB  
01046      EXEC CICS LINK                                               ELTIOB  
01047                PROGRAM ('ELUIOPGM')                               ELTIOB  
01048                COMMAREA (DFHCOMMAREA)                             ELTIOB  
01049         END-EXEC.                                                 ELTIOB  
01050      EJECT                                                        ELTIOB  
01051                                                                   ELTIOB  
01052                                                                   ELTIOB  
01053 ************************************************************      ELTIOB  
01054 *                                                          *      ELTIOB  
01055 *        ESTABLISH ADDRESSABILITY OF GCCP TABULAR          *      ELTIOB  
01056 *                                                          *      ELTIOB  
01057 ************************************************************      ELTIOB  
01058  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTIOB  
01059      SET ADDRESS OF GCCP-TABULAR-REC-AREA TO                      ELTIOB  
01060          IOP-REC-PTR.                                             ELTIOB  
01061      SET IOP-REC-PTR TO NULL.                                     ELTIOB  
01062      EJECT                                                        ELTIOB  
01063                                                                   ELTIOB  
01064                                                                   ELTIOB  
01065 ************************************************************      ELTIOB  
01066 *                                                          *      ELTIOB  
01067 *        SIGNAL NOT FOUND GCTAB ERROR                      *      ELTIOB  
01068 *                                                          *      ELTIOB  
01069 ************************************************************      ELTIOB  
01070  SIGNAL-NOT-FOUND-GCTAB-ERROR.                                    ELTIOB  
01071      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTIOB  
01072      PERFORM SIGNAL-ABEND.                                        ELTIOB  
01073      EJECT                                                        ELTIOB  
01074                                                                   ELTIOB  
01075                                                                   ELTIOB  
01076 ************************************************************      ELTIOB  
01077 *                                                          *      ELTIOB  
01078 *        SIGNAL CRITICAL IO ERROR                          *      ELTIOB  
01079 *                                                          *      ELTIOB  
01080 ************************************************************      ELTIOB  
01081  SIGNAL-CRITICAL-IO-ERROR.                                        ELTIOB  
01082      SET CIA-AB-CRITIO TO TRUE.                                   ELTIOB  
01083      PERFORM SIGNAL-ABEND.                                        ELTIOB  
01084      EJECT                                                        ELTIOB  
01085                                                                   ELTIOB  
01086                                                                   ELTIOB  
01087 ************************************************************      ELTIOB  
01088 *                                                          *      ELTIOB  
01089 *        GENERATE ASSOCIATED TABULAR DATA                  *      ELTIOB  
01090 *                                                          *      ELTIOB  
01091 ************************************************************      ELTIOB  
01092  GENERATE-ASSOCIATED-TABULAR-DA.                                  ELTIOB  
01093      PERFORM GENERATE-MAXIMUM.                                    ELTIOB  
01094      PERFORM GENERATE-COINSURANCE.                                ELTIOB  
01095      PERFORM GENERATE-COPAY.                                      ELTIOB  
01096      PERFORM GENERATE-DEDUCTIBLE.                                 ELTIOB  
01097                                                                   ELTIOB  
01098                                                                   ELTIOB  
01099 ************************************************************      ELTIOB  
01100 *                                                          *      ELTIOB  
01101 *        GENERATE MAXIMUM                                  *      ELTIOB  
01102 *                                                          *      ELTIOB  
01103 ************************************************************      ELTIOB  
01104  GENERATE-MAXIMUM.                                                ELTIOB  
01105      EXEC CICS LINK                                               ELTIOB  
01106                PROGRAM ('ELGABMCC')                               ELTIOB  
01107                COMMAREA (DFHCOMMAREA)                             ELTIOB  
01108         END-EXEC.                                                 ELTIOB  
01109                                                                   ELTIOB  
01110                                                                   ELTIOB  
01111 ************************************************************      ELTIOB  
01112 *                                                          *      ELTIOB  
01113 *        GENERATE COPAY                                    *      ELTIOB  
01114 *                                                          *      ELTIOB  
01115 ************************************************************      ELTIOB  
01116  GENERATE-COPAY.                                                  ELTIOB  
01117      EXEC CICS LINK                                               ELTIOB  
01118                PROGRAM ('ELGACPCC')                               ELTIOB  
01119                COMMAREA (DFHCOMMAREA)                             ELTIOB  
01120         END-EXEC.                                                 ELTIOB  
01121                                                                   ELTIOB  
01122 ************************************************************      ELTIOB  
01123 *                                                          *      ELTIOB  
01124 *        GENERATE COINSURANCE                              *      ELTIOB  
01125 *                                                          *      ELTIOB  
01126 ************************************************************      ELTIOB  
01127  GENERATE-COINSURANCE.                                            ELTIOB  
01128      EXEC CICS LINK                                               ELTIOB  
01129                PROGRAM ('ELGACLCC')                               ELTIOB  
01130                COMMAREA (DFHCOMMAREA)                             ELTIOB  
01131         END-EXEC.                                                 ELTIOB  
01132                                                                   ELTIOB  
01133                                                                   ELTIOB  
01134 ************************************************************      ELTIOB  
01135 *                                                          *      ELTIOB  
01136 *        GENERATE DEDUCTIBLE                               *      ELTIOB  
01137 *                                                          *      ELTIOB  
01138 ************************************************************      ELTIOB  
01139  GENERATE-DEDUCTIBLE.                                             ELTIOB  
01140      EXEC CICS LINK                                               ELTIOB  
01141                PROGRAM ('ELGADLCC')                               ELTIOB  
01142                COMMAREA (DFHCOMMAREA)                             ELTIOB  
01143         END-EXEC.                                                 ELTIOB  
01144      EJECT                                                        ELTIOB  
01145                                                                   ELTIOB  
01146                                                                   ELTIOB  
01147 ************************************************************      ELTIOB  
01148 *                                                          *      ELTIOB  
01149 *        TERMINATE OUTPUT                                  *      ELTIOB  
01150 *                                                          *      ELTIOB  
01151 ************************************************************      ELTIOB  
01152  TERMINATE-OUTPUT.                                                ELTIOB  
01153      SET COF-END TO TRUE.                                         ELTIOB  
01154      PERFORM LINK-TO-OUTPUT.                                      ELTIOB  
01155                                                                   ELTIOB  
01156                                                                   ELTIOB  
01157 ************************************************************      ELTIOB  
01158 *                                                          *      ELTIOB  
01159 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTIOB  
01160 *                                                          *      ELTIOB  
01161 ************************************************************      ELTIOB  
01162  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTIOB  
01163      PERFORM INITIALIZE-CMOUT.                                    ELTIOB  
01164      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTIOB  
01165      EJECT                                                        ELTIOB  
01166                                                                   ELTIOB  
01167                                                                   ELTIOB  
01168 ************************************************************      ELTIOB  
01169 *                                                          *      ELTIOB  
01170 *        PREPARE TEXT FOR OUTPUT                           *      ELTIOB  
01171 *                                                          *      ELTIOB  
01172 ************************************************************      ELTIOB  
01173  PREPARE-TEXT-FOR-OUTPUT.                                         ELTIOB  
01174      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTIOB  
01175          UNTIL CMF-DESCR-IDX                                      ELTIOB  
01176                                    GREATER THAN                   ELTIOB  
01177              CMF-NBR-DESCR-LINES.                                 ELTIOB  
01178      EJECT                                                        ELTIOB  
01179                                                                   ELTIOB  
01180                                                                   ELTIOB  
01181 ************************************************************      ELTIOB  
01182 *                                                          *      ELTIOB  
01183 *        INITIALIZE CMOUT                                  *      ELTIOB  
01184 *                                                          *      ELTIOB  
01185 ************************************************************      ELTIOB  
01186  INITIALIZE-CMOUT.                                                ELTIOB  
01187      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTIOB  
01188      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTIOB  
01189          ADDRESS OF CMF-DESCR.                                    ELTIOB  
01190      SET CMF-DESCR-IDX TO 1.                                      ELTIOB  
01191      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTIOB  
01192                                                                   ELTIOB  
01193                                                                   ELTIOB  
01194 ************************************************************      ELTIOB  
01195 *                                                          *      ELTIOB  
01196 *        MOVE CMF TEXT TO OUTPUT                           *      ELTIOB  
01197 *                                                          *      ELTIOB  
01198 ************************************************************      ELTIOB  
01199  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTIOB  
01200      PERFORM MOVE-A-LINE.                                         ELTIOB  
01201      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTIOB  
01202          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTIOB  
01203      IF TCAR-FROM-SUB GREATER THAN 20                             ELTIOB  
01204               OR CMF-DESCR-IDX GREATER THAN                       ELTIOB  
01205          CMF-NBR-DESCR-LINES                                      ELTIOB  
01206          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTIOB  
01207                                                                   ELTIOB  
01208                                                                   ELTIOB  
01209 ************************************************************      ELTIOB  
01210 *                                                          *      ELTIOB  
01211 *        FINISH CODES MANUAL TEXT                          *      ELTIOB  
01212 *                                                          *      ELTIOB  
01213 ************************************************************      ELTIOB  
01214  FINISH-CODES-MANUAL-TEXT.                                        ELTIOB  
01215      SET DONE-PROCESSING TO TRUE.                                 ELTIOB  
01216      IF PERIOD-NEEDED                                             ELTIOB  
01217          PERFORM GET-AND-MOVE-PERIOD.                             ELTIOB  
01218                                                                   ELTIOB  
01219                                                                   ELTIOB  
01220 ************************************************************      ELTIOB  
01221 *                                                          *      ELTIOB  
01222 *        GET AND MOVE PERIOD                               *      ELTIOB  
01223 *                                                          *      ELTIOB  
01224 ************************************************************      ELTIOB  
01225  GET-AND-MOVE-PERIOD.                                             ELTIOB  
01226      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTIOB  
01227          (TCAR-FROM-SUB).                                         ELTIOB  
01228                                                                   ELTIOB  
01229                                                                   ELTIOB  
01230 ************************************************************      ELTIOB  
01231 *                                                          *      ELTIOB  
01232 *        SAVE LAST LINE                                    *      ELTIOB  
01233 *                                                          *      ELTIOB  
01234 ************************************************************      ELTIOB  
01235  SAVE-LAST-LINE.                                                  ELTIOB  
01236      MOVE +1 TO TCAR-FROM-SUB.                                    ELTIOB  
01237      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTIOB  
01238         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTIOB  
01239      ADD 1 TO TCAR-FROM-SUB.                                      ELTIOB  
01240      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTIOB  
01241                                                                   ELTIOB  
01242                                                                   ELTIOB  
01243 ************************************************************      ELTIOB  
01244 *                                                          *      ELTIOB  
01245 *        OUTPUT LAST LINE                                  *      ELTIOB  
01246 *                                                          *      ELTIOB  
01247 ************************************************************      ELTIOB  
01248  OUTPUT-LAST-LINE.                                                ELTIOB  
01249      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTIOB  
01250          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTIOB  
01251      IF BLANK-LINE-NEEDED                                         ELTIOB  
01252          PERFORM CREATE-A-BLANK-LINE.                             ELTIOB  
01253                                                                   ELTIOB  
01254                                                                   ELTIOB  
01255 ************************************************************      ELTIOB  
01256 *                                                          *      ELTIOB  
01257 *        CREATE A BLANK LINE                               *      ELTIOB  
01258 *                                                          *      ELTIOB  
01259 ************************************************************      ELTIOB  
01260  CREATE-A-BLANK-LINE.                                             ELTIOB  
01261      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTIOB  
01262      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTIOB  
01263                                                                   ELTIOB  
01264                                                                   ELTIOB  
01265 ************************************************************      ELTIOB  
01266 *                                                          *      ELTIOB  
01267 *        MOVE A LINE                                       *      ELTIOB  
01268 *                                                          *      ELTIOB  
01269 ************************************************************      ELTIOB  
01270  MOVE-A-LINE.                                                     ELTIOB  
01271      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTIOB  
01272          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTIOB  
01273      SET CMF-DESCR-IDX UP BY 1.                                   ELTIOB  
01274      ADD 1 TO TCAR-FROM-SUB.                                      ELTIOB  
01275      EJECT                                                        ELTIOB  
01276                                                                   ELTIOB  
01277                                                                   ELTIOB  
01278 ************************************************************      ELTIOB  
01279 *                                                          *      ELTIOB  
01280 *        REFORMAT AND WRITE TEXT                           *      ELTIOB  
01281 *                                                          *      ELTIOB  
01282 ************************************************************      ELTIOB  
01283  REFORMAT-AND-WRITE-TEXT.                                         ELTIOB  
01284      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTIOB  
01285      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTIOB  
01286      PERFORM UNSTRING-TEXT.                                       ELTIOB  
01287      MOVE +1 TO TCAR-FROM-SUB.                                    ELTIOB  
01288      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTIOB  
01289      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTIOB  
01290          UNTIL COF-NBR-DTL-LINES GREATER                          ELTIOB  
01291                                   TCAR-OUTPUT-FIELDS-USED -       ELTIOB  
01292              1.                                                   ELTIOB  
01293      PERFORM DISPOSE-OF-LAST-LINE.                                ELTIOB  
01294      PERFORM LINK-TO-OUTPUT.                                      ELTIOB  
01295                                                                   ELTIOB  
01296                                                                   ELTIOB  
01297 ************************************************************      ELTIOB  
01298 *                                                          *      ELTIOB  
01299 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTIOB  
01300 *                                                          *      ELTIOB  
01301 ************************************************************      ELTIOB  
01302  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTIOB  
01303      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTIOB  
01304           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTIOB  
01305      ADD +1 TO TCAR-FROM-SUB.                                     ELTIOB  
01306      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTIOB  
01307      EJECT                                                        ELTIOB  
01308                                                                   ELTIOB  
01309                                                                   ELTIOB  
01310 ************************************************************      ELTIOB  
01311 *                                                          *      ELTIOB  
01312 *        UNSTRING TEXT                                     *      ELTIOB  
01313 *                                                          *      ELTIOB  
01314 ************************************************************      ELTIOB  
01315  UNSTRING-TEXT.                                                   ELTIOB  
01316      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTIOB  
01317      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTIOB  
01318      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTIOB  
01319      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTIOB  
01320      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTIOB  
01321      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTIOB  
01322      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTIOB  
01323      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTIOB  
01324      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTIOB  
01325      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTIOB  
01326      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTIOB  
01327      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTIOB  
01328      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTIOB  
01329      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTIOB  
01330      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTIOB  
01331      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTIOB  
01332      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTIOB  
01333      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTIOB  
01334      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTIOB  
01335      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTIOB  
01336      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTIOB  
01337      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTIOB  
01338                                                                   ELTIOB  
01339                                                                   ELTIOB  
01340 ************************************************************      ELTIOB  
01341 *                                                          *      ELTIOB  
01342 *        LINK TO OUTPUT                                    *      ELTIOB  
01343 *                                                          *      ELTIOB  
01344 ************************************************************      ELTIOB  
01345  LINK-TO-OUTPUT.                                                  ELTIOB  
01346      EXEC CICS LINK                                               ELTIOB  
01347          PROGRAM ('ELUOUTPT')                                     ELTIOB  
01348          COMMAREA (DFHCOMMAREA)                                   ELTIOB  
01349          END-EXEC.                                                ELTIOB  
01350      EJECT                                                        ELTIOB  
01351                                                                   ELTIOB  
01352                                                                   ELTIOB  
01353 ************************************************************      ELTIOB  
01354 *                                                          *      ELTIOB  
01355 *        DISPOSE OF LAST LINE                              *      ELTIOB  
01356 *                                                          *      ELTIOB  
01357 ************************************************************      ELTIOB  
01358  DISPOSE-OF-LAST-LINE.                                            ELTIOB  
01359      IF NOT ADDITIONAL-TEXT                                       ELTIOB  
01360          PERFORM INITIALIZE-CONTINUED-SW.                         ELTIOB  
01361      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTIOB  
01362          PERFORM SAVE-LAST-LINE                                   ELTIOB  
01363      ELSE                                                         ELTIOB  
01364          PERFORM OUTPUT-LAST-LINE.                                ELTIOB  
01365                                                                   ELTIOB  
01366                                                                   ELTIOB  
01367 ************************************************************      ELTIOB  
01368 *                                                          *      ELTIOB  
01369 *        INITIALIZE CONTINUED SW                           *      ELTIOB  
01370 *                                                          *      ELTIOB  
01371 ************************************************************      ELTIOB  
01372  INITIALIZE-CONTINUED-SW.                                         ELTIOB  
01373      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTIOB  
