00001 *      LAST MAINTENANCE TIME:  7.54.00  DATE: 06/13/91            06/29/02
00002  IDENTIFICATION DIVISION.                                         ELTCPO  
00003                                                                      LV001
00004  PROGRAM-ID.         ELTCPO.                                      ELTCPO  
00005                                                                   ELTCPO  
00006  AUTHOR.             ANNE KEFFER KING.                            ELTCPO  
00007                      CLONED FROM ELTRPO.                          ELTCPO  
00008                                                                   ELTCPO  
00009  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTCPO  
00010                      A MUTUAL LEGAL RESERVE COMPANY               ELTCPO  
00011                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTCPO  
00012                      233 N. MICHIGAN AVE                          ELTCPO  
00013                      CHICAGO, ILLINOIS 60601                      ELTCPO  
00014                                                                   ELTCPO  
00015  DATE-WRITTEN.       05-JAN-1993.                                 ELTCPO  
00016                                                                   ELTCPO  
00017  DATE-COMPILED.                                                   ELTCPO  
00018                                                                   ELTCPO  
00019  SECURITY.           COPYRIGHT 1986,                              ELTCPO  
00020                      HEALTH CARE SERVICE CORPORATION              ELTCPO  
00021      SKIP3                                                        ELTCPO  
00022  ENVIRONMENT DIVISION.                                            ELTCPO  
00023                                                                   ELTCPO  
00024  CONFIGURATION SECTION.                                           ELTCPO  
00025  SOURCE-COMPUTER.    IBM-3090.                                    ELTCPO  
00026  OBJECT-COMPUTER.    IBM-3090.                                    ELTCPO  
00027      EJECT                                                        ELTCPO  
00028 ******************************************************************ELTCPO  
00029 *                                                                *ELTCPO  
00030 *    COPYBOOK:   ELTCPO                                          *ELTCPO  
00031 *    DATE:       22-FEB-1995                                     *ELTCPO  
00032 *    AUTHOR:     ANNE KEFFER KING                                *ELTCPO  
00033 *    FUNCTION:   THIS MODULE WILL DISPLAY ALL OUTPUT ASSOCIATED  *ELTCPO  
00034 *                WITH COMMUNITY PARTICIPATING OPTION PROGRAM.    *ELTCPO  
00035 *    NOTES:      X---                                            *ELTCPO  
00036 *                                                                *ELTCPO  
00037 ******************************************************************ELTCPO  
00038 *                                                                *ELTCPO  
00039 *                      MAINTENANCE HISTORY                       *ELTCPO  
00040 *                                                                *ELTCPO  
00041 *  MOD     DATE     BY  DRPT                ACTION               *ELTCPO  
00042 * ----- ----------- --- ----- ---------------------------------- *ELTCPO  
00043 * 01.00 22-FEB-1995 AKK       CREATED.                           *ELTCPO  
00044 *                                                                *ELTCPO  
00045 ******************************************************************ELTCPO  
00046                                                                   ELTCPO  
00047  DATA DIVISION.                                                   ELTCPO  
00048                                                                   ELTCPO  
00049  WORKING-STORAGE SECTION.                                         ELTCPO  
00050  01  WS-MISC.                                                     ELTCPO  
00051      05  WS-BEGIN                        PIC X(26) VALUE          ELTCPO  
00052      '*** ELTCPO WS BEGINS ***'.                                  ELTCPO  
00053      05  WS-POINTER2                     POINTER.                 ELTCPO  
00054      05  WS-POINTER3                     POINTER.                 ELTCPO  
00055                                                                   ELTCPO  
00056  01  WS-SWITCHES.                                                 ELTCPO  
00057      05  ADDITIONAL-TEXT-SWITCH   PIC X(01) VALUE SPACE.          ELTCPO  
00058          88  ADDITIONAL-TEXT                VALUE 'A'.            ELTCPO  
00059          88  BLANK-LINE-NEEDED              VALUE 'B'.            ELTCPO  
00060      05  CONTINUED-PROCESSING-SW  PIC X(01) VALUE SPACE.          ELTCPO  
00061          88  DONE-PROCESSING                VALUE 'D'.            ELTCPO  
00062          88  PROCESSING-CMF-TEXT            VALUE 'P'.            ELTCPO  
00063      05  WS-TABULAR-SWITCH        PIC X(01) VALUE 'N'.            ELTCPO  
00064          88  TABULAR-IS-UNDEFINED           VALUE 'Y'.            ELTCPO  
00065      05  WS-PERIOD-SWITCH         PIC X(01) VALUE 'N'.            ELTCPO  
00066          88  PERIOD-NEEDED                  VALUE 'Y'.            ELTCPO  
00067      05  WS-APPROVAL-SOURCE-SW    PIC X     VALUE SPACE.          ELTCPO  
00068          88  NOT-HOLDING-APPROVAL-SRCE      VALUE 'N'.            ELTCPO  
00069          88  HOLDING-APPROVAL-SOURCE        VALUE 'H'.            ELTCPO  
00070                                                                   ELTCPO  
00071  01  WS-HOLD-AREA.                                                ELTCPO  
00072      05  WS-GCPO-PROV-SLOT-NO   COMP-3 PIC S9(07) VALUE +0.       ELTCPO  
00073                                                                   ELTCPO  
00074 **************************************************************    ELTCPO  
00075 ***                   PROGRAM CONSTANTS                           ELTCPO  
00076 **************************************************************    ELTCPO  
00077      05  WS-GRP                   PIC X(06) VALUE 'GROUP'.        ELTCPO  
00078      05  WS-GCCP                  PIC X(06) VALUE '#GCCP '.       ELTCPO  
00079      05  WS-GCPO                  PIC X(06) VALUE '#GCPO '.       ELTCPO  
00080      05  WS-INST                  PIC X(13) VALUE 'INSTITUTIONAL'.ELTCPO  
00081      05  WS-PROF                  PIC X(13) VALUE 'PROFESSIONAL '.ELTCPO  
00082      05  WS-SUPP                  PIC X(13) VALUE 'SUPPLEMENTAL '.ELTCPO  
00083      05  WS-APPROVAL              PIC X(09) VALUE 'APPROVAL.'.    ELTCPO  
00084                                                                   ELTCPO  
00085 **************************************************************    ELTCPO  
00086 ***                   HEADER  LINE                                ELTCPO  
00087 **************************************************************    ELTCPO  
00088      05  WS-HEADER-LINE.                                          ELTCPO  
00089          10  FILLER               PIC X(14) VALUE SPACES.         ELTCPO  
00090          10  FILLER               PIC X(39) VALUE                 ELTCPO  
00091          'COMMUNITY PARTICIPATING OPTION PROGRAM '.               ELTCPO  
00092          10  WS-HDR-LINE-BCBSMM   PIC X(13) VALUE SPACES.         ELTCPO  
00093          10  FILLER               PIC X(13) VALUE SPACES.         ELTCPO  
00094                                                                   ELTCPO  
00095 **************************************************************    ELTCPO  
00096 ***                   SCREEN BODY LINES                           ELTCPO  
00097 **************************************************************    ELTCPO  
00098  01  WS-SCREEN-LINE-AREA.                                         ELTCPO  
00099      05  WS-APPRVL-LINE.                                          ELTCPO  
00100          10  FILLER               PIC X(49) VALUE                 ELTCPO  
00101          'COMMUNITY PARTICIPATING OPTION PROGRAM REQUIRES '.      ELTCPO  
00102          10  FILLER               PIC X(30) VALUE SPACES.         ELTCPO  
00103                                                                   ELTCPO  
00104      05  WS-ALT-PRICING-LINE-BC.                                  ELTCPO  
00105          10  FILLER               PIC X(52) VALUE                 ELTCPO  
00106          'THE ALTERNATE PRICING FOR INSTITUTIONAL SERVICES IS '.  ELTCPO  
00107          10  FILLER               PIC X(27) VALUE SPACES.         ELTCPO  
00108                                                                   ELTCPO  
00109      05  WS-ALT-PRICING-LINE-BS.                                  ELTCPO  
00110          10  FILLER               PIC X(51) VALUE                 ELTCPO  
00111          'THE ALTERNATE PRICING FOR PROFESSIONAL SERVICES IS '.   ELTCPO  
00112          10  FILLER               PIC X(28) VALUE SPACES.         ELTCPO  
00113                                                                   ELTCPO  
00114      05  WS-BENE-REDUCT-LINE.                                     ELTCPO  
00115          10  FILLER               PIC X(47) VALUE                 ELTCPO  
00116          'DENIED OR REDUCED BENEFITS DUE TO THIS PROGRAM:'.       ELTCPO  
00117          10  FILLER               PIC X(32) VALUE SPACES.         ELTCPO  
00118                                                                   ELTCPO  
00119      05  WS-SPILL-OVER-LINE.                                      ELTCPO  
00120          10  FILLER               PIC X(51) VALUE                 ELTCPO  
00121          'UNPAID SERVICES AFTER BASIC BENEFITS REDUCTION ARE '.   ELTCPO  
00122          10  FILLER               PIC X(28) VALUE SPACES.         ELTCPO  
00123                                                                   ELTCPO  
00124 **************************************************************    ELTCPO  
00125 ** SPECIAL MESSAGE FOR THE NOT APPLICABLE                         ELTCPO  
00126 ** ALSO THE FIXED TEXT FOR TABULAR GCPO                           ELTCPO  
00127 **************************************************************    ELTCPO  
00128      05  WS-CPO-APPLIES.                                          ELTCPO  
00129          10  FILLER               PIC X(79) VALUE                 ELTCPO  
00130          'THE COMMUNITY PARTICIPATING OPTION PROGRAM APPLIES TO'. ELTCPO  
00131                                                                   ELTCPO  
00132      05  WS-NOT-APPLICABLE-MSG.                                   ELTCPO  
00133          10  FILLER               PIC X(44) VALUE                 ELTCPO  
00134          'THE COMMUNITY PARTICIPATING OPTION PROGRAM '.           ELTCPO  
00135          10  FILLER               PIC X(18) VALUE                 ELTCPO  
00136          'IS NOT APPLICABLE.'.                                    ELTCPO  
00137          10  FILLER               PIC X(17) VALUE SPACES.         ELTCPO  
00138                                                                   ELTCPO  
00139      05  WS-SPEC-PROV-MSG.                                        ELTCPO  
00140          10  FILLER               PIC X(79) VALUE                 ELTCPO  
00141          'THERE ARE SPECIAL PROVIDERS INCLUDED IN THIS COST CONTAIELTCPO  
00142 -        'NMENT PROGRAM.'.                                        ELTCPO  
00143                                                                   ELTCPO  
00144      05  WS-DISCLAIMER-MSG.                                       ELTCPO  
00145          10  FILLER               PIC X(79) VALUE                 ELTCPO  
00146          '*** SUBJECT TO OTHER CONTRACT LIMITATIONS ***'.         ELTCPO  
00147                                                                   ELTCPO  
00148      05  WS-NOT-APPLY-TO-LOB-BC.                                  ELTCPO  
00149          10  FILLER               PIC X(49) VALUE                 ELTCPO  
00150          'COMMUNITY PROVIDER OPTION PROGRAM DOES NOT APPLY '.     ELTCPO  
00151          10  FILLER               PIC X(30) VALUE                 ELTCPO  
00152          'FOR INSTITUTIONAL BENEFITS'.                            ELTCPO  
00153                                                                   ELTCPO  
00154      05  WS-NOT-APPLY-TO-LOB-BS.                                  ELTCPO  
00155          10  FILLER               PIC X(49) VALUE                 ELTCPO  
00156          'COMMUNITY PROVIDER OPTION PROGRAM DOES NOT APPLY '.     ELTCPO  
00157          10  FILLER               PIC X(30) VALUE                 ELTCPO  
00158          'FOR PROFESSIONAL BENEFITS.'.                            ELTCPO  
00159                                                                   ELTCPO  
00160      05  WS-NOT-APPLY-TO-LOB-MM.                                  ELTCPO  
00161          10  FILLER               PIC X(49) VALUE                 ELTCPO  
00162          'COMMUNITY PROVIDER OPTION PROGRAM DOES NOT APPLY '.     ELTCPO  
00163          10  FILLER               PIC X(30) VALUE                 ELTCPO  
00164          'FOR SUPPLEMENTAL BENEFITS.'.                            ELTCPO  
00165                                                                   ELTCPO  
00166  LINKAGE SECTION.                                                 ELTCPO  
00167  01  DFHCOMMAREA.                                                 ELTCPO  
00168      COPY ELSCOMMC.                                               ELTCPO  
00169 /                                                                 ELTCPO  
00170      COPY ELSCIA2C.                                               ELTCPO  
00171 /                                                                 ELTCPO  
00172      COPY ELSCMDSC.                                               ELTCPO  
00173 /                                                                 ELTCPO  
00174      COPY ELSCMIFC.                                               ELTCPO  
00175 /                                                                 ELTCPO  
00176      COPY ELSIOPMC.                                               ELTCPO  
00177 /                                                                 ELTCPO  
00178      COPY ELSKEYSC.                                               ELTCPO  
00179 /                                                                 ELTCPO  
00180      COPY ELSOUTPC.                                               ELTCPO  
00181 /                                                                 ELTCPO  
00182      COPY ELSSRTPC.                                               ELTCPO  
00183 /                                                                 ELTCPO  
00184      COPY ELSTCWAC.                                               ELTCPO  
00185 /                                                                 ELTCPO  
00186      COPY ELSSSCBC.                                               ELTCPO  
00187 /                                                                 ELTCPO  
00188  01  GROUP-SPECIFIC-REC.                                          ELTCPO  
00189      COPY GCGROUPC.                                               ELTCPO  
00190 /                                                                 ELTCPO  
00191  01  GCCP-TABULAR-REC-AREA.                                       ELTCPO  
00192      COPY GCTGCCPC.                                               ELTCPO  
00193      EJECT                                                        ELTCPO  
00194  PROCEDURE DIVISION.                                              ELTCPO  
00195 ************************************************************      ELTCPO  
00196 *                                                          *      ELTCPO  
00197 *                    PROCEDURE DIVISION                    *      ELTCPO  
00198 *                                                          *      ELTCPO  
00199 ************************************************************      ELTCPO  
00200                                                                   ELTCPO  
00201                                                                   ELTCPO  
00202 ************************************************************      ELTCPO  
00203 *                                                          *      ELTCPO  
00204 *        COMMUNITY PROVIDER OPTION                         *      ELTCPO  
00205 *                                                          *      ELTCPO  
00206 ************************************************************      ELTCPO  
00207  RESTRICTED-PROVIDER-OPTION.                                      ELTCPO  
00208      PERFORM INITIALIZATION.                                      ELTCPO  
00209      PERFORM PROCESS.                                             ELTCPO  
00210      GOBACK.                                                      ELTCPO  
00211                                                                   ELTCPO  
00212 ************************************************************      ELTCPO  
00213 *                                                          *      ELTCPO  
00214 *        INITIALIZATION                                    *      ELTCPO  
00215 *                                                          *      ELTCPO  
00216 ************************************************************      ELTCPO  
00217  INITIALIZATION.                                                  ELTCPO  
00218      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTCPO  
00219      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTCPO  
00220                                                                   ELTCPO  
00221 ************************************************************      ELTCPO  
00222 *                                                          *      ELTCPO  
00223 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTCPO  
00224 *                                                          *      ELTCPO  
00225 ************************************************************      ELTCPO  
00226  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTCPO  
00227      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTCPO  
00228      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTCPO  
00229      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTCPO  
00230                                                                   ELTCPO  
00231 ************************************************************      ELTCPO  
00232 *                                                          *      ELTCPO  
00233 *        CHECK FOR VALID COMMAREA                          *      ELTCPO  
00234 *                                                          *      ELTCPO  
00235 ************************************************************      ELTCPO  
00236  CHECK-FOR-VALID-COMMAREA.                                        ELTCPO  
00237      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTCPO  
00238          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTCPO  
00239                                                                   ELTCPO  
00240 ************************************************************      ELTCPO  
00241 *                                                          *      ELTCPO  
00242 *        SIGNAL INVALID COMMAREA                           *      ELTCPO  
00243 *                                                          *      ELTCPO  
00244 ************************************************************      ELTCPO  
00245  SIGNAL-INVALID-COMMAREA.                                         ELTCPO  
00246      EXEC CICS ABEND                                              ELTCPO  
00247                ABCODE('EL01')                                     ELTCPO  
00248         END-EXEC.                                                 ELTCPO  
00249      EJECT                                                        ELTCPO  
00250                                                                   ELTCPO  
00251 ************************************************************      ELTCPO  
00252 *                                                          *      ELTCPO  
00253 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTCPO  
00254 *                                                          *      ELTCPO  
00255 ************************************************************      ELTCPO  
00256  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTCPO  
00257      IF ECA-CIA-PTR = NULL                                        ELTCPO  
00258          PERFORM SIGNAL-INVALID-CIA                               ELTCPO  
00259      ELSE                                                         ELTCPO  
00260          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTCPO  
00261                                                                   ELTCPO  
00262                                                                   ELTCPO  
00263 ************************************************************      ELTCPO  
00264 *                                                          *      ELTCPO  
00265 *        SIGNAL INVALID CIA                                *      ELTCPO  
00266 *                                                          *      ELTCPO  
00267 ************************************************************      ELTCPO  
00268  SIGNAL-INVALID-CIA.                                              ELTCPO  
00269      EXEC CICS ABEND                                              ELTCPO  
00270                ABCODE('EL02')                                     ELTCPO  
00271         END-EXEC.                                                 ELTCPO  
00272                                                                   ELTCPO  
00273 ************************************************************      ELTCPO  
00274 *                                                          *      ELTCPO  
00275 *        ESTABLISH ADDRESS OF CIA                          *      ELTCPO  
00276 *                                                          *      ELTCPO  
00277 ************************************************************      ELTCPO  
00278  ESTABLISH-ADDRESS-OF-CIA.                                        ELTCPO  
00279      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTCPO  
00280                            ADDRESS OF                             ELTCPO  
00281          CIA-ELS-COMMON-INTERFACE-AREA.                           ELTCPO  
00282      EJECT                                                        ELTCPO  
00283                                                                   ELTCPO  
00284 ************************************************************      ELTCPO  
00285 *                                                          *      ELTCPO  
00286 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTCPO  
00287 *                                                          *      ELTCPO  
00288 ************************************************************      ELTCPO  
00289  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTCPO  
00290      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTCPO  
00291      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCPO  
00292                            ADDRESS OF                             ELTCPO  
00293          SSB-SELECTOR-STATUS-CTL-BLK.                             ELTCPO  
00294      IF CIA-RC-PTR-NULL                                           ELTCPO  
00295          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTCPO  
00296                                                                   ELTCPO  
00297 ************************************************************      ELTCPO  
00298 *                                                          *      ELTCPO  
00299 *        SIGNAL UNALLOC AREA ERROR                         *      ELTCPO  
00300 *                                                          *      ELTCPO  
00301 ************************************************************      ELTCPO  
00302  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTCPO  
00303      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTCPO  
00304      PERFORM SIGNAL-ABEND.                                        ELTCPO  
00305                                                                   ELTCPO  
00306 ************************************************************      ELTCPO  
00307 *                                                          *      ELTCPO  
00308 *        SIGNAL ABEND                                      *      ELTCPO  
00309 *                                                          *      ELTCPO  
00310 ************************************************************      ELTCPO  
00311  SIGNAL-ABEND.                                                    ELTCPO  
00312      EXEC CICS ABEND                                              ELTCPO  
00313                ABCODE(CIA-ABCODE)                                 ELTCPO  
00314         END-EXEC.                                                 ELTCPO  
00315      EJECT                                                        ELTCPO  
00316                                                                   ELTCPO  
00317 ************************************************************      ELTCPO  
00318 *                                                          *      ELTCPO  
00319 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTCPO  
00320 *                                                          *      ELTCPO  
00321 ************************************************************      ELTCPO  
00322  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTCPO  
00323      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTCPO  
00324      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTCPO  
00325      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTCPO  
00326      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTCPO  
00327      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTCPO  
00328      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTCPO  
00329      PERFORM ESTABLISH-ADDRESSABILITY-OF-CS.                      ELTCPO  
00330                                                                   ELTCPO  
00331 ************************************************************      ELTCPO  
00332 *                                                          *      ELTCPO  
00333 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTCPO  
00334 *                                                          *      ELTCPO  
00335 ************************************************************      ELTCPO  
00336  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTCPO  
00337      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTCPO  
00338      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCPO  
00339                            ADDRESS OF                             ELTCPO  
00340          CMF-CODES-MANUAL-INTERFACE.                              ELTCPO  
00341      IF CIA-RC-PTR-NULL                                           ELTCPO  
00342          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTCPO  
00343      EJECT                                                        ELTCPO  
00344                                                                   ELTCPO  
00345 ************************************************************      ELTCPO  
00346 *                                                          *      ELTCPO  
00347 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTCPO  
00348 *                                                          *      ELTCPO  
00349 ************************************************************      ELTCPO  
00350  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTCPO  
00351      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTCPO  
00352      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCPO  
00353                            ADDRESS OF                             ELTCPO  
00354          COF-OUTPUT-INTERFACE.                                    ELTCPO  
00355      IF CIA-RC-PTR-NULL                                           ELTCPO  
00356          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTCPO  
00357      EJECT                                                        ELTCPO  
00358                                                                   ELTCPO  
00359 ************************************************************      ELTCPO  
00360 *                                                          *      ELTCPO  
00361 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTCPO  
00362 *                                                          *      ELTCPO  
00363 ************************************************************      ELTCPO  
00364  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTCPO  
00365      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTCPO  
00366      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCPO  
00367                            ADDRESS OF                             ELTCPO  
00368          SRP-SUBROUTINE-PARAMETERS.                               ELTCPO  
00369      IF CIA-RC-PTR-NULL                                           ELTCPO  
00370          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTCPO  
00371      EJECT                                                        ELTCPO  
00372                                                                   ELTCPO  
00373 ************************************************************      ELTCPO  
00374 *                                                          *      ELTCPO  
00375 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTCPO  
00376 *                                                          *      ELTCPO  
00377 ************************************************************      ELTCPO  
00378  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTCPO  
00379      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTCPO  
00380      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCPO  
00381                            ADDRESS OF                             ELTCPO  
00382          TCAR-COMPRESSION-WORK-AREA.                              ELTCPO  
00383      IF CIA-RC-PTR-NULL                                           ELTCPO  
00384          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTCPO  
00385      EJECT                                                        ELTCPO  
00386                                                                   ELTCPO  
00387 ************************************************************      ELTCPO  
00388 *                                                          *      ELTCPO  
00389 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTCPO  
00390 *                                                          *      ELTCPO  
00391 ************************************************************      ELTCPO  
00392  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTCPO  
00393      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTCPO  
00394      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCPO  
00395                            ADDRESS OF                             ELTCPO  
00396          KWA-FILE-KEY-WORK-AREA.                                  ELTCPO  
00397      IF CIA-RC-PTR-NULL                                           ELTCPO  
00398          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTCPO  
00399      EJECT                                                        ELTCPO  
00400                                                                   ELTCPO  
00401 ************************************************************      ELTCPO  
00402 *                                                          *      ELTCPO  
00403 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELTCPO  
00404 *                                                          *      ELTCPO  
00405 ************************************************************      ELTCPO  
00406  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTCPO  
00407      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTCPO  
00408      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCPO  
00409                            ADDRESS OF                             ELTCPO  
00410          GROUP-SPECIFIC-REC.                                      ELTCPO  
00411      IF CIA-RC-PTR-NULL                                           ELTCPO  
00412          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTCPO  
00413      EJECT                                                        ELTCPO  
00414                                                                   ELTCPO  
00415 ************************************************************      ELTCPO  
00416 *                                                          *      ELTCPO  
00417 *        ESTABLISH ADDRESSABILITY OF CST CONTAINMENT PROGRA*      ELTCPO  
00418 *                                                          *      ELTCPO  
00419 ************************************************************      ELTCPO  
00420  ESTABLISH-ADDRESSABILITY-OF-CS.                                  ELTCPO  
00421      SET CIA-GCTABULR-DDN TO TRUE.                                ELTCPO  
00422      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCPO  
00423                            ADDRESS OF                             ELTCPO  
00424          GCCP-TABULAR-REC-AREA.                                   ELTCPO  
00425      IF CIA-RC-PTR-NULL                                           ELTCPO  
00426          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTCPO  
00427      EJECT                                                        ELTCPO  
00428                                                                   ELTCPO  
00429 ************************************************************      ELTCPO  
00430 *                                                          *      ELTCPO  
00431 *        PROCESS                                           *      ELTCPO  
00432 *                                                          *      ELTCPO  
00433 ************************************************************      ELTCPO  
00434  PROCESS.                                                         ELTCPO  
00435      IF GCG-CPO-PARTICIPATION-IND = ZERO                          ELTCPO  
00436          PERFORM GENERATE-NOT-APPLICABLE-MESSAG                   ELTCPO  
00437      ELSE                                                         ELTCPO  
00438          PERFORM GENERATE-CPO-TEXT.                               ELTCPO  
00439      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTCPO  
00440      SET COF-NEW-PAGE      TO TRUE.                               ELTCPO  
00441      MOVE 'ACCUMULATORS' TO WS-HDR-LINE-BCBSMM.                   ELTCPO  
00442      MOVE WS-HEADER-LINE   TO COF-HDR-LINE(COF-NBR-HDR-LINES).    ELTCPO  
00443      PERFORM LINK-TO-OUTPUT.                                      ELTCPO  
00444      IF SSB-PROV-CLASS-INST                                       ELTCPO  
00445         SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                     ELTCPO  
00446         PERFORM GENERATE-ACCUM-TABULAR-DATA.                      ELTCPO  
00447      IF SSB-PROV-CLASS-PROF                                       ELTCPO  
00448         SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                     ELTCPO  
00449         PERFORM GENERATE-ACCUM-TABULAR-DATA.                      ELTCPO  
00450      IF SSB-PROV-CLASS-BOTH                                       ELTCPO  
00451         SET SRP-ACCUM-PROV-CLASS-INST TO TRUE                     ELTCPO  
00452         PERFORM GENERATE-ACCUM-TABULAR-DATA.                      ELTCPO  
00453      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTCPO  
00454      PERFORM TERMINATE-OUTPUT.                                    ELTCPO  
00455                                                                   ELTCPO  
00456 ************************************************************      ELTCPO  
00457 *                                                          *      ELTCPO  
00458 *        EJECT NEW PAGE                                    *      ELTCPO  
00459 *                                                          *      ELTCPO  
00460 ************************************************************      ELTCPO  
00461  EJECT-NEW-PAGE.                                                  ELTCPO  
00462      SET COF-NEW-PAGE      TO TRUE.                               ELTCPO  
00463      MOVE WS-HEADER-LINE   TO COF-HDR-LINE                        ELTCPO  
00464          (COF-NBR-HDR-LINES).                                     ELTCPO  
00465      PERFORM LINK-TO-OUTPUT.                                      ELTCPO  
00466      EJECT                                                        ELTCPO  
00467                                                                   ELTCPO  
00468 ************************************************************      ELTCPO  
00469 *                                                          *      ELTCPO  
00470 *        TRANSLATE AND DISPLAY CPO IND                     *      ELTCPO  
00471 *                                                          *      ELTCPO  
00472 ************************************************************      ELTCPO  
00473  TRANSLATE-AND-DISPLAY-CPO-IND.                                   ELTCPO  
00474      INITIALIZE TCAR-FROM-AREA.                                   ELTCPO  
00475      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCPO  
00476      MOVE WS-CPO-APPLIES TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELTCPO  
00477      ADD  +1 TO TCAR-FROM-SUB.                                    ELTCPO  
00478      SET BLANK-LINE-NEEDED TO TRUE.                               ELTCPO  
00479      SET PERIOD-NEEDED TO TRUE.                                   ELTCPO  
00480      MOVE GCG-CPO-PARTICIPATION-IND TO CMF-CODE-VALUE.            ELTCPO  
00481      MOVE 'CPO-PARTICIPATION-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTCPO  
00482      MOVE WS-GRP TO CMF-RECORD-PREFIX.                            ELTCPO  
00483      EXEC CICS LINK                                               ELTCPO  
00484                PROGRAM ('ELUCMIF')                                ELTCPO  
00485                COMMAREA (DFHCOMMAREA)                             ELTCPO  
00486         END-EXEC.                                                 ELTCPO  
00487      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTCPO  
00488      MOVE SPACE TO ADDITIONAL-TEXT-SWITCH.                        ELTCPO  
00489                                                                   ELTCPO  
00490 ************************************************************      ELTCPO  
00491 *                                                          *      ELTCPO  
00492 *        SET UP OUTPUT SUBSCRIPTS                          *      ELTCPO  
00493 *                                                          *      ELTCPO  
00494 ************************************************************      ELTCPO  
00495  SET-UP-OUTPUT-SUBSCRIPTS.                                        ELTCPO  
00496      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTCPO  
00497      MOVE +2 TO COF-NBR-HDR-LINES.                                ELTCPO  
00498                                                                   ELTCPO  
00499 ************************************************************      ELTCPO  
00500 *                                                          *      ELTCPO  
00501 *        GENERATE NOT APPLICABLE MESSAGE                   *      ELTCPO  
00502 *                                                          *      ELTCPO  
00503 ************************************************************      ELTCPO  
00504  GENERATE-NOT-APPLICABLE-MESSAG.                                  ELTCPO  
00505      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTCPO  
00506      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTCPO  
00507      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTCPO  
00508      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTCPO  
00509          (COF-NBR-DTL-LINES).                                     ELTCPO  
00510      PERFORM EJECT-NEW-PAGE.                                      ELTCPO  
00511      EJECT                                                        ELTCPO  
00512                                                                   ELTCPO  
00513 ************************************************************      ELTCPO  
00514 *                                                          *      ELTCPO  
00515 *        GENERATE CPO TEXT                                 *      ELTCPO  
00516 *                                                          *      ELTCPO  
00517 ************************************************************      ELTCPO  
00518  GENERATE-CPO-TEXT.                                               ELTCPO  
00519      SET WS-POINTER2 TO NULLS.                                    ELTCPO  
00520      SET WS-POINTER3 TO NULLS.                                    ELTCPO  
00521      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTCPO  
00522      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTCPO  
00523                            WS-POINTER2.                           ELTCPO  
00524      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTCPO  
00525      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTCPO  
00526                            WS-POINTER3.                           ELTCPO  
00527      PERFORM SEARCH-FOR-GCCP-TABULAR.                             ELTCPO  
00528      PERFORM DETERMINE-SELECTION.                                 ELTCPO  
00529      EJECT                                                        ELTCPO  
00530                                                                   ELTCPO  
00531 ************************************************************      ELTCPO  
00532 *                                                          *      ELTCPO  
00533 *        TERMINATE OUTPUT                                  *      ELTCPO  
00534 *                                                          *      ELTCPO  
00535 ************************************************************      ELTCPO  
00536  TERMINATE-OUTPUT.                                                ELTCPO  
00537      SET COF-END TO TRUE.                                         ELTCPO  
00538      PERFORM LINK-TO-OUTPUT.                                      ELTCPO  
00539      EJECT                                                        ELTCPO  
00540                                                                   ELTCPO  
00541 ************************************************************      ELTCPO  
00542 *                                                          *      ELTCPO  
00543 *        SEARCH FOR GCCP TABULAR                           *      ELTCPO  
00544 *                                                          *      ELTCPO  
00545 ************************************************************      ELTCPO  
00546  SEARCH-FOR-GCCP-TABULAR.                                         ELTCPO  
00547      SET GCG-INDEX TO +1.                                         ELTCPO  
00548      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTCPO  
00549         AT END                                                    ELTCPO  
00550              MOVE ZEROES TO KWA-PROVISION-SLOT-NO                 ELTCPO  
00551         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GCCP                     ELTCPO  
00552              MOVE GCG-TAB-ID (GCG-INDEX)                          ELTCPO  
00553                  TO KWA-PROVISION-ID                              ELTCPO  
00554              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTCPO  
00555                  TO KWA-PROVISION-SLOT-NO                         ELTCPO  
00556         END-SEARCH.                                               ELTCPO  
00557      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELTCPO  
00558          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTCPO  
00559      PERFORM GET-GCCP-TABULAR.                                    ELTCPO  
00560      PERFORM SEARCH-THE-GSS-ENTRY.                                ELTCPO  
00561      EJECT                                                        ELTCPO  
00562                                                                   ELTCPO  
00563 ************************************************************      ELTCPO  
00564 *                                                          *      ELTCPO  
00565 *        TRANSLATE APPROVAL SOURCE                         *      ELTCPO  
00566 *                                                          *      ELTCPO  
00567 ************************************************************      ELTCPO  
00568  TRANSLATE-APPROVAL-SOURCE.                                       ELTCPO  
00569      INITIALIZE WS-PERIOD-SWITCH.                                 ELTCPO  
00570      INITIALIZE TCAR-FROM-AREA.                                   ELTCPO  
00571      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCPO  
00572      MOVE WS-APPRVL-LINE TO TCAR-FROM-LINE                        ELTCPO  
00573          (TCAR-FROM-SUB).                                         ELTCPO  
00574      ADD  +1 TO TCAR-FROM-SUB.                                    ELTCPO  
00575      SET ADDITIONAL-TEXT TO TRUE.                                 ELTCPO  
00576      IF NOT-HOLDING-APPROVAL-SRCE                                 ELTCPO  
00577          PERFORM GET-APPROVAL-TRANSLATION                         ELTCPO  
00578      ELSE                                                         ELTCPO  
00579          PERFORM USE-EXISTING-APPROVAL-TRANSLAT.                  ELTCPO  
00580      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTCPO  
00581      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTCPO  
00582      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTCPO  
00583                            WS-POINTER3.                           ELTCPO  
00584      MOVE WS-APPROVAL TO TCAR-FROM-LINE                           ELTCPO  
00585          (TCAR-FROM-SUB).                                         ELTCPO  
00586      PERFORM FINISH-SENTENCE.                                     ELTCPO  
00587      EJECT                                                        ELTCPO  
00588                                                                   ELTCPO  
00589                                                                   ELTCPO  
00590 ************************************************************      ELTCPO  
00591 *                                                          *      ELTCPO  
00592 *        GET APPROVAL TRANSLATION                          *      ELTCPO  
00593 *                                                          *      ELTCPO  
00594 ************************************************************      ELTCPO  
00595  GET-APPROVAL-TRANSLATION.                                        ELTCPO  
00596      MOVE GSS-CP-APPROVAL-SRC-IND (GSS-INDEX) TO CMF-CODE-VALUE.  ELTCPO  
00597      MOVE   'CP-APPROVAL-SRC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTCPO  
00598      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTCPO  
00599      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTCPO  
00600      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTCPO  
00601                            ADDRESS OF CMF-DESCR.                  ELTCPO  
00602      EJECT                                                        ELTCPO  
00603                                                                   ELTCPO  
00604 ************************************************************      ELTCPO  
00605 *                                                          *      ELTCPO  
00606 *        USE EXISTING APPROVAL TRANSLATION                 *      ELTCPO  
00607 *                                                          *      ELTCPO  
00608 ************************************************************      ELTCPO  
00609  USE-EXISTING-APPROVAL-TRANSLAT.                                  ELTCPO  
00610      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTCPO  
00611      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTCPO  
00612                            ADDRESS OF CMF-DESCR.                  ELTCPO  
00613      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTCPO  
00614      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTCPO  
00615                            WS-POINTER2.                           ELTCPO  
00616      EJECT                                                        ELTCPO  
00617                                                                   ELTCPO  
00618 ************************************************************      ELTCPO  
00619 *                                                          *      ELTCPO  
00620 *        FINISH SENTENCE                                   *      ELTCPO  
00621 *                                                          *      ELTCPO  
00622 ************************************************************      ELTCPO  
00623  FINISH-SENTENCE.                                                 ELTCPO  
00624      SET BLANK-LINE-NEEDED TO TRUE.                               ELTCPO  
00625      PERFORM REFORMAT-AND-WRITE-TEXT.                             ELTCPO  
00626                                                                   ELTCPO  
00627 ************************************************************      ELTCPO  
00628 *                                                          *      ELTCPO  
00629 *        TRANSLATE AND DISPLAY CODE VALUE                  *      ELTCPO  
00630 *                                                          *      ELTCPO  
00631 ************************************************************      ELTCPO  
00632  TRANSLATE-AND-DISPLAY-CODE-VAL.                                  ELTCPO  
00633      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTCPO  
00634      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTCPO  
00635                                                                   ELTCPO  
00636 ************************************************************      ELTCPO  
00637 *                                                          *      ELTCPO  
00638 *        SIGNAL UNDEFINED TABULAR ERROR                    *      ELTCPO  
00639 *                                                          *      ELTCPO  
00640 ************************************************************      ELTCPO  
00641  SIGNAL-UNDEFINED-TABULAR-ERROR.                                  ELTCPO  
00642      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTCPO  
00643      PERFORM SIGNAL-ABEND.                                        ELTCPO  
00644      EJECT                                                        ELTCPO  
00645                                                                   ELTCPO  
00646 ************************************************************      ELTCPO  
00647 *                                                          *      ELTCPO  
00648 *        DETERMINE SELECTION                               *      ELTCPO  
00649 *                                                          *      ELTCPO  
00650 ************************************************************      ELTCPO  
00651  DETERMINE-SELECTION.                                             ELTCPO  
00652      SET NOT-HOLDING-APPROVAL-SRCE TO TRUE.                       ELTCPO  
00653      IF SSB-PROV-CLASS-INST OR                                    ELTCPO  
00654                   SSB-PROV-CLASS-BOTH                             ELTCPO  
00655          PERFORM CREATE-INSTITUTIONAL-SCREEN.                     ELTCPO  
00656      IF SSB-PROV-CLASS-PROF OR                                    ELTCPO  
00657                   SSB-PROV-CLASS-BOTH                             ELTCPO  
00658          PERFORM CREATE-PROFESSIONAL-SCREEN.                      ELTCPO  
00659      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL '03' OR '04' OR       ELTCPO  
00660                    '06' OR '08')                                  ELTCPO  
00661          PERFORM CREATE-SUPPLEMENTAL-SCREEN.                      ELTCPO  
00662                                                                   ELTCPO  
00663 ************************************************************      ELTCPO  
00664 *                                                          *      ELTCPO  
00665 *        CREATE INSTITUTIONAL SCREEN                       *      ELTCPO  
00666 *                                                          *      ELTCPO  
00667 ************************************************************      ELTCPO  
00668  CREATE-INSTITUTIONAL-SCREEN.                                     ELTCPO  
00669      MOVE WS-INST TO WS-HDR-LINE-BCBSMM.                          ELTCPO  
00670      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTCPO  
00671      IF GSS-CP-BC-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTCPO  
00672          ZEROES                                                   ELTCPO  
00673                 AND LOW-VALUES                                    ELTCPO  
00674          PERFORM GENERATE-INSTITUTIONAL-TEXT                      ELTCPO  
00675      ELSE                                                         ELTCPO  
00676          PERFORM GENERATE-INSTITUTIONAL-NOT-APP.                  ELTCPO  
00677      EJECT                                                        ELTCPO  
00678                                                                   ELTCPO  
00679 ************************************************************      ELTCPO  
00680 *                                                          *      ELTCPO  
00681 *        GENERATE INSTITUTIONAL TEXT                       *      ELTCPO  
00682 *                                                          *      ELTCPO  
00683 ************************************************************      ELTCPO  
00684  GENERATE-INSTITUTIONAL-TEXT.                                     ELTCPO  
00685      PERFORM EJECT-NEW-PAGE.                                      ELTCPO  
00686      PERFORM TRANSLATE-AND-DISPLAY-CPO-IND.                       ELTCPO  
00687      IF GSS-CP-APPROVAL-SRC-IND (GSS-INDEX) NOT EQUAL SPACES      ELTCPO  
00688          AND                                                      ELTCPO  
00689                 ZEROES AND LOW-VALUES                             ELTCPO  
00690          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTCPO  
00691      PERFORM TRANSLATE-BC-IND.                                    ELTCPO  
00692      IF GSS-CP-BC-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL SPACES  ELTCPO  
00693           AND ZEROES AND LOW-VALUES                               ELTCPO  
00694            PERFORM TRANSLATE-BC-PAYMENT-LEVEL-IND.                ELTCPO  
00695 *    SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTCPO  
00696 *    PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTCPO  
00697      IF GSS-CP-BC-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES AND    ELTCPO  
00698          ZEROES                                                   ELTCPO  
00699                 AND LOW-VALUES                                    ELTCPO  
00700          PERFORM TRANSLATE-BC-CALC-METHOD.                        ELTCPO  
00701      IF GSS-CP-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTCPO  
00702          SPACES AND                                               ELTCPO  
00703                 ZEROES AND LOW-VALUES                             ELTCPO  
00704          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTCPO  
00705      IF GSS-CP-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL SPACES AND    ELTCPO  
00706          ZEROES                                                   ELTCPO  
00707                 AND LOW-VALUES                                    ELTCPO  
00708          PERFORM GENERATE-SPILL-OVER-TEXT.                        ELTCPO  
00709      PERFORM GENERATE-SPECIAL-PROVIDERS-SEN.                      ELTCPO  
00710      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTCPO  
00711      EJECT                                                        ELTCPO  
00712                                                                   ELTCPO  
00713 ************************************************************      ELTCPO  
00714 *                                                          *      ELTCPO  
00715 *        GENERATE INSTITUTIONAL NOT APPLICABLE             *      ELTCPO  
00716 *                                                          *      ELTCPO  
00717 ************************************************************      ELTCPO  
00718  GENERATE-INSTITUTIONAL-NOT-APP.                                  ELTCPO  
00719      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTCPO  
00720      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTCPO  
00721      MOVE WS-NOT-APPLY-TO-LOB-BC TO COF-DTL-LINE                  ELTCPO  
00722          (COF-NBR-DTL-LINES).                                     ELTCPO  
00723      PERFORM EJECT-NEW-PAGE.                                      ELTCPO  
00724      EJECT                                                        ELTCPO  
00725                                                                   ELTCPO  
00726                                                                   ELTCPO  
00727 ************************************************************      ELTCPO  
00728 *                                                          *      ELTCPO  
00729 *        GENERATE DISCLAIMER MESSAGE                       *      ELTCPO  
00730 *                                                          *      ELTCPO  
00731 ************************************************************      ELTCPO  
00732  GENERATE-DISCLAIMER-MESSAGE.                                     ELTCPO  
00733      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTCPO  
00734      MOVE WS-DISCLAIMER-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).  ELTCPO  
00735      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTCPO  
00736      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTCPO  
00737      PERFORM LINK-TO-OUTPUT.                                      ELTCPO  
00738      EJECT                                                        ELTCPO  
00739                                                                   ELTCPO  
00740 ************************************************************      ELTCPO  
00741 *                                                          *      ELTCPO  
00742 *        TRANSLATE BC IND                                  *      ELTCPO  
00743 *                                                          *      ELTCPO  
00744 ************************************************************      ELTCPO  
00745  TRANSLATE-BC-IND.                                                ELTCPO  
00746      INITIALIZE TCAR-FROM-AREA.                                   ELTCPO  
00747      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCPO  
00748      SET BLANK-LINE-NEEDED TO TRUE.                               ELTCPO  
00749      SET PERIOD-NEEDED TO TRUE.                                   ELTCPO  
00750      MOVE GSS-CP-BC-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTCPO  
00751      MOVE   'CP-BC-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTCPO  
00752      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTCPO  
00753      EJECT                                                        ELTCPO  
00754                                                                   ELTCPO  
00755 ************************************************************      ELTCPO  
00756 *                                                          *      ELTCPO  
00757 *        TRANSLATE BC PAYMENT LEVEL INDICATOR              *      ELTCPO  
00758 *                                                          *      ELTCPO  
00759 ************************************************************      ELTCPO  
00760  TRANSLATE-BC-PAYMENT-LEVEL-IND.                                  ELTCPO  
00761      INITIALIZE TCAR-FROM-AREA.                                   ELTCPO  
00762      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCPO  
00763      MOVE 'CP-BC-PAYMENT-LEVEL-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTCPO  
00764      MOVE GSS-CP-BC-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTCPO  
00765         CMF-CODE-VALUE.                                           ELTCPO  
00766      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTCPO  
00767      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTCPO  
00768                                                                   ELTCPO  
00769 ************************************************************      ELTCPO  
00770 *                                                          *      ELTCPO  
00771 *        TRANSLATE BC CALC METHOD                          *      ELTCPO  
00772 *                                                          *      ELTCPO  
00773 ************************************************************      ELTCPO  
00774  TRANSLATE-BC-CALC-METHOD.                                        ELTCPO  
00775      INITIALIZE TCAR-FROM-AREA.                                   ELTCPO  
00776      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCPO  
00777      SET BLANK-LINE-NEEDED TO TRUE.                               ELTCPO  
00778      SET PERIOD-NEEDED TO TRUE.                                   ELTCPO  
00779      MOVE GSS-CP-BC-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTCPO  
00780      MOVE   'CP-BC-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTCPO  
00781      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTCPO  
00782      EJECT                                                        ELTCPO  
00783                                                                   ELTCPO  
00784 ************************************************************      ELTCPO  
00785 *                                                          *      ELTCPO  
00786 *        GENERATE ACCUM TABULAR DATA                       *      ELTCPO  
00787 *                                                          *      ELTCPO  
00788 ************************************************************      ELTCPO  
00789  GENERATE-ACCUM-TABULAR-DATA.                                     ELTCPO  
00790      PERFORM GENERATE-COINSURANCE.                                ELTCPO  
00791      PERFORM GENERATE-COPAY.                                      ELTCPO  
00792      PERFORM GENERATE-DEDUCTIBLE.                                 ELTCPO  
00793      PERFORM GENERATE-MAXIMUM.                                    ELTCPO  
00794      PERFORM GENERATE-OUT-OF-POCKET.                              ELTCPO  
00795      EJECT                                                        ELTCPO  
00796                                                                   ELTCPO  
00797 ************************************************************      ELTCPO  
00798 *                                                          *      ELTCPO  
00799 *        GENERATE COMBINED BENEFITS REDUCTION TEXT         *      ELTCPO  
00800 *                                                          *      ELTCPO  
00801 ************************************************************      ELTCPO  
00802  GENERATE-COMBINED-BENEFITS-RED.                                  ELTCPO  
00803      MOVE 'CP' TO SRP-COST-CONT-TYPE.                             ELTCPO  
00804      MOVE 'COMMUNITY PROVIDER OPTION PROGRAM' TO SRP-CCP-NAME.    ELTCPO  
00805      MOVE GSS-CP-COMB-BENE-REDUCT-IND (GSS-INDEX)                 ELTCPO  
00806                  TO SRP-CCP-COMB-BENE-REDUCT-IND.                 ELTCPO  
00807      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELTCPO  
00808      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTCPO  
00809                            ADDRESS OF                             ELTCPO  
00810          GCCP-TABULAR-REC-AREA.                                   ELTCPO  
00811      PERFORM CALL-CBRI-INTERFACE.                                 ELTCPO  
00812      EJECT                                                        ELTCPO  
00813                                                                   ELTCPO  
00814 ************************************************************      ELTCPO  
00815 *                                                          *      ELTCPO  
00816 *        CREATE PROFESSIONAL SCREEN                        *      ELTCPO  
00817 *                                                          *      ELTCPO  
00818 ************************************************************      ELTCPO  
00819  CREATE-PROFESSIONAL-SCREEN.                                      ELTCPO  
00820      MOVE WS-PROF TO WS-HDR-LINE-BCBSMM.                          ELTCPO  
00821      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTCPO  
00822      IF GSS-CP-BS-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTCPO  
00823          ZEROES                                                   ELTCPO  
00824                 AND LOW-VALUES                                    ELTCPO  
00825          PERFORM GENERATE-PROFESSIONAL-TEXT                       ELTCPO  
00826      ELSE                                                         ELTCPO  
00827          PERFORM GENERATE-PROFESSIONAL-NOT-APPL.                  ELTCPO  
00828      EJECT                                                        ELTCPO  
00829                                                                   ELTCPO  
00830 ************************************************************      ELTCPO  
00831 *                                                          *      ELTCPO  
00832 *        GENERATE PROFESSIONAL TEXT                        *      ELTCPO  
00833 *                                                          *      ELTCPO  
00834 ************************************************************      ELTCPO  
00835  GENERATE-PROFESSIONAL-TEXT.                                      ELTCPO  
00836      PERFORM EJECT-NEW-PAGE.                                      ELTCPO  
00837      PERFORM TRANSLATE-AND-DISPLAY-CPO-IND.                       ELTCPO  
00838      IF GSS-CP-APPROVAL-SRC-IND (GSS-INDEX) NOT EQUAL             ELTCPO  
00839          SPACES                                                   ELTCPO  
00840                 AND ZEROES AND LOW-VALUES                         ELTCPO  
00841          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTCPO  
00842      PERFORM TRANSLATE-BS-IND.                                    ELTCPO  
00843      IF GSS-CP-BS-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL SPACES  ELTCPO  
00844            AND ZEROES AND LOW-VALUES                              ELTCPO  
00845            PERFORM TRANSLATE-BS-PAYMENT-LEVEL-IND.                ELTCPO  
00846      IF GSS-CP-BS-ALT-PRICING-METHOD (GSS-INDEX) NOT EQUAL        ELTCPO  
00847          SPACES AND                                               ELTCPO  
00848                 ZEROES AND LOW-VALUES                             ELTCPO  
00849          PERFORM TRANSLATE-BS-ALT-PRIC.                           ELTCPO  
00850 *    SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTCPO  
00851 *    PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTCPO  
00852      IF GSS-CP-BS-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES AND    ELTCPO  
00853          ZEROES                                                   ELTCPO  
00854                 AND LOW-VALUES                                    ELTCPO  
00855          PERFORM TRANSLATE-BS-CALC-METHOD.                        ELTCPO  
00856      IF GSS-CP-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTCPO  
00857          SPACES                                                   ELTCPO  
00858                 AND ZEROES AND LOW-VALUES                         ELTCPO  
00859          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTCPO  
00860      IF GSS-CP-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL SPACES AND    ELTCPO  
00861          ZEROES                                                   ELTCPO  
00862                 AND LOW-VALUES                                    ELTCPO  
00863          PERFORM GENERATE-SPILL-OVER-TEXT.                        ELTCPO  
00864      PERFORM GENERATE-SPECIAL-PROVIDERS-SEN.                      ELTCPO  
00865      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTCPO  
00866                                                                   ELTCPO  
00867 ************************************************************      ELTCPO  
00868 *                                                          *      ELTCPO  
00869 *        GENERATE PROFESSIONAL NOT APPLICABLE              *      ELTCPO  
00870 *                                                          *      ELTCPO  
00871 ************************************************************      ELTCPO  
00872  GENERATE-PROFESSIONAL-NOT-APPL.                                  ELTCPO  
00873      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTCPO  
00874      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTCPO  
00875      MOVE WS-NOT-APPLY-TO-LOB-BS TO COF-DTL-LINE                  ELTCPO  
00876          (COF-NBR-DTL-LINES).                                     ELTCPO  
00877      PERFORM EJECT-NEW-PAGE.                                      ELTCPO  
00878      EJECT                                                        ELTCPO  
00879                                                                   ELTCPO  
00880 ************************************************************      ELTCPO  
00881 *                                                          *      ELTCPO  
00882 *        TRANSLATE BS IND                                  *      ELTCPO  
00883 *                                                          *      ELTCPO  
00884 ************************************************************      ELTCPO  
00885  TRANSLATE-BS-IND.                                                ELTCPO  
00886      INITIALIZE TCAR-FROM-AREA.                                   ELTCPO  
00887      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCPO  
00888      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTCPO  
00889      SET PERIOD-NEEDED TO TRUE.                                   ELTCPO  
00890      MOVE GSS-CP-BS-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTCPO  
00891      MOVE   'CP-BS-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTCPO  
00892      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTCPO  
00893      EJECT                                                        ELTCPO  
00894                                                                   ELTCPO  
00895 ************************************************************      ELTCPO  
00896 *                                                          *      ELTCPO  
00897 *        TRANSLATE BS PAYMENT LEVEL INDICATOR              *      ELTCPO  
00898 *                                                          *      ELTCPO  
00899 ************************************************************      ELTCPO  
00900  TRANSLATE-BS-PAYMENT-LEVEL-IND.                                  ELTCPO  
00901      MOVE 'CP-BS-PAYMENT-LEVEL-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTCPO  
00902      MOVE GSS-CP-BS-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTCPO  
00903         CMF-CODE-VALUE.                                           ELTCPO  
00904      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTCPO  
00905      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTCPO  
00906                                                                   ELTCPO  
00907 ************************************************************      ELTCPO  
00908 *                                                          *      ELTCPO  
00909 *        TRANSLATE BS CALC METHOD                          *      ELTCPO  
00910 *                                                          *      ELTCPO  
00911 ************************************************************      ELTCPO  
00912  TRANSLATE-BS-CALC-METHOD.                                        ELTCPO  
00913      INITIALIZE TCAR-FROM-AREA.                                   ELTCPO  
00914      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCPO  
00915      SET BLANK-LINE-NEEDED TO TRUE.                               ELTCPO  
00916      SET PERIOD-NEEDED TO TRUE.                                   ELTCPO  
00917      MOVE GSS-CP-BS-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTCPO  
00918      MOVE   'CP-BS-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTCPO  
00919      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTCPO  
00920      EJECT                                                        ELTCPO  
00921                                                                   ELTCPO  
00922 ************************************************************      ELTCPO  
00923 *                                                          *      ELTCPO  
00924 *        TRANSLATE BS ALT PRIC                             *      ELTCPO  
00925 *                                                          *      ELTCPO  
00926 ************************************************************      ELTCPO  
00927  TRANSLATE-BS-ALT-PRIC.                                           ELTCPO  
00928      INITIALIZE TCAR-FROM-AREA.                                   ELTCPO  
00929      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCPO  
00930      MOVE WS-ALT-PRICING-LINE-BS TO TCAR-FROM-LINE                ELTCPO  
00931          (TCAR-FROM-SUB).                                         ELTCPO  
00932      ADD  +1 TO TCAR-FROM-SUB.                                    ELTCPO  
00933      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTCPO  
00934      SET PERIOD-NEEDED TO TRUE.                                   ELTCPO  
00935      MOVE GSS-CP-BS-ALT-PRICING-METHOD (GSS-INDEX) TO             ELTCPO  
00936          CMF-CODE-VALUE.                                          ELTCPO  
00937      MOVE  'CP-BS-ALT-PRICING-METHOD' TO CMF-ELEMENT-SYSTEM-NAME. ELTCPO  
00938      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTCPO  
00939      EJECT                                                        ELTCPO  
00940                                                                   ELTCPO  
00941 ************************************************************      ELTCPO  
00942 *                                                          *      ELTCPO  
00943 *        CREATE SUPPLEMENTAL SCREEN                        *      ELTCPO  
00944 *                                                          *      ELTCPO  
00945 ************************************************************      ELTCPO  
00946  CREATE-SUPPLEMENTAL-SCREEN.                                      ELTCPO  
00947      MOVE WS-SUPP TO WS-HDR-LINE-BCBSMM.                          ELTCPO  
00948      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTCPO  
00949      IF GSS-CP-MM-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTCPO  
00950          ZEROES                                                   ELTCPO  
00951                 AND LOW-VALUES                                    ELTCPO  
00952          PERFORM GENERATE-SUPPLEMENTAL-TEXT                       ELTCPO  
00953      ELSE                                                         ELTCPO  
00954          PERFORM GENERATE-SUPPLEMENTAL-NOT-APPL.                  ELTCPO  
00955      EJECT                                                        ELTCPO  
00956                                                                   ELTCPO  
00957 ************************************************************      ELTCPO  
00958 *                                                          *      ELTCPO  
00959 *        GENERATE SUPPLEMENTAL TEXT                        *      ELTCPO  
00960 *                                                          *      ELTCPO  
00961 ************************************************************      ELTCPO  
00962  GENERATE-SUPPLEMENTAL-TEXT.                                      ELTCPO  
00963      PERFORM EJECT-NEW-PAGE.                                      ELTCPO  
00964      PERFORM TRANSLATE-AND-DISPLAY-CPO-IND.                       ELTCPO  
00965      IF GSS-CP-APPROVAL-SRC-IND (GSS-INDEX) NOT EQUAL             ELTCPO  
00966          SPACES                                                   ELTCPO  
00967                 AND ZEROES AND LOW-VALUES                         ELTCPO  
00968          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTCPO  
00969      PERFORM TRANSLATE-MM-IND.                                    ELTCPO  
00970      IF GSS-CP-MM-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL SPACES  ELTCPO  
00971         AND SPACES AND LOW-VALUES                                 ELTCPO  
00972            PERFORM TRANSLATE-MM-PAYMENT-LEVEL-IND.                ELTCPO  
00973 *    SET SRP-ACCUM-PROV-CLASS-SUPP TO TRUE.                       ELTCPO  
00974 *    PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTCPO  
00975      IF GSS-CP-MM-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES AND    ELTCPO  
00976          ZEROES                                                   ELTCPO  
00977                 AND LOW-VALUES                                    ELTCPO  
00978          PERFORM TRANSLATE-MM-CALC-METHOD.                        ELTCPO  
00979      IF GSS-CP-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTCPO  
00980          SPACES                                                   ELTCPO  
00981                 AND ZEROES AND LOW-VALUES                         ELTCPO  
00982          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTCPO  
00983      PERFORM GENERATE-SPECIAL-PROVIDERS-SEN.                      ELTCPO  
00984      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTCPO  
00985                                                                   ELTCPO  
00986                                                                   ELTCPO  
00987 ************************************************************      ELTCPO  
00988 *                                                          *      ELTCPO  
00989 *        GENERATE SUPPLEMENTAL NOT APPLICABLE              *      ELTCPO  
00990 *                                                          *      ELTCPO  
00991 ************************************************************      ELTCPO  
00992  GENERATE-SUPPLEMENTAL-NOT-APPL.                                  ELTCPO  
00993      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTCPO  
00994      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTCPO  
00995      MOVE WS-NOT-APPLY-TO-LOB-MM TO COF-DTL-LINE                  ELTCPO  
00996          (COF-NBR-DTL-LINES).                                     ELTCPO  
00997      PERFORM EJECT-NEW-PAGE.                                      ELTCPO  
00998      EJECT                                                        ELTCPO  
00999                                                                   ELTCPO  
01000 ************************************************************      ELTCPO  
01001 *                                                          *      ELTCPO  
01002 *        TRANSLATE MM IND                                  *      ELTCPO  
01003 *                                                          *      ELTCPO  
01004 ************************************************************      ELTCPO  
01005  TRANSLATE-MM-IND.                                                ELTCPO  
01006      INITIALIZE TCAR-FROM-AREA.                                   ELTCPO  
01007      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCPO  
01008      SET BLANK-LINE-NEEDED TO TRUE.                               ELTCPO  
01009      SET PERIOD-NEEDED TO TRUE.                                   ELTCPO  
01010      MOVE GSS-CP-MM-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTCPO  
01011      MOVE   'CP-MM-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTCPO  
01012      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTCPO  
01013      EJECT                                                        ELTCPO  
01014                                                                   ELTCPO  
01015 ************************************************************      ELTCPO  
01016 *                                                          *      ELTCPO  
01017 *        TRANSLATE MM PAYMENT LEVEL INDICATOR              *      ELTCPO  
01018 *                                                          *      ELTCPO  
01019 ************************************************************      ELTCPO  
01020  TRANSLATE-MM-PAYMENT-LEVEL-IND.                                  ELTCPO  
01021      MOVE 'CP-MM-PAYMENT-LEVEL-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTCPO  
01022      MOVE GSS-CP-MM-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTCPO  
01023         CMF-CODE-VALUE.                                           ELTCPO  
01024      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTCPO  
01025      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTCPO  
01026                                                                   ELTCPO  
01027 ************************************************************      ELTCPO  
01028 *                                                          *      ELTCPO  
01029 *        TRANSLATE MM CALC METHOD                          *      ELTCPO  
01030 *                                                          *      ELTCPO  
01031 ************************************************************      ELTCPO  
01032  TRANSLATE-MM-CALC-METHOD.                                        ELTCPO  
01033      INITIALIZE TCAR-FROM-AREA.                                   ELTCPO  
01034      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCPO  
01035      SET BLANK-LINE-NEEDED TO TRUE.                               ELTCPO  
01036      SET PERIOD-NEEDED TO TRUE.                                   ELTCPO  
01037      MOVE GSS-CP-MM-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTCPO  
01038      MOVE   'CP-MM-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTCPO  
01039      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTCPO  
01040      EJECT                                                        ELTCPO  
01041                                                                   ELTCPO  
01042                                                                   ELTCPO  
01043 ************************************************************      ELTCPO  
01044 *                                                          *      ELTCPO  
01045 *        GENERATE SPILL OVER TEXT                          *      ELTCPO  
01046 *                                                          *      ELTCPO  
01047 ************************************************************      ELTCPO  
01048  GENERATE-SPILL-OVER-TEXT.                                        ELTCPO  
01049      INITIALIZE TCAR-FROM-AREA.                                   ELTCPO  
01050      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCPO  
01051      MOVE WS-SPILL-OVER-LINE TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELTCPO  
01052      ADD  +1 TO TCAR-FROM-SUB.                                    ELTCPO  
01053      SET BLANK-LINE-NEEDED TO TRUE.                               ELTCPO  
01054      SET PERIOD-NEEDED TO TRUE.                                   ELTCPO  
01055      MOVE GSS-CP-SPILL-OVER-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTCPO  
01056      MOVE 'CP-SPILL-OVER-IND' TO CMF-ELEMENT-SYSTEM-NAME.         ELTCPO  
01057      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTCPO  
01058      EJECT                                                        ELTCPO  
01059                                                                   ELTCPO  
01060 ************************************************************      ELTCPO  
01061 *                                                          *      ELTCPO  
01062 *        GENERATE SPECIAL PROVIDERS SENTENCE               *      ELTCPO  
01063 *                                                          *      ELTCPO  
01064 ************************************************************      ELTCPO  
01065  GENERATE-SPECIAL-PROVIDERS-SEN.                                  ELTCPO  
01066      SET GCG-INDEX TO +1.                                         ELTCPO  
01067      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTCPO  
01068         AT END                                                    ELTCPO  
01069              MOVE ZEROES TO WS-GCPO-PROV-SLOT-NO                  ELTCPO  
01070         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GCPO                     ELTCPO  
01071              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTCPO  
01072                  TO WS-GCPO-PROV-SLOT-NO                          ELTCPO  
01073         END-SEARCH.                                               ELTCPO  
01074      IF WS-GCPO-PROV-SLOT-NO NOT EQUAL ZEROES                     ELTCPO  
01075          PERFORM DISPLAY-SPECIAL-PROVIDERS-SENT                   ELTCPO  
01076          PERFORM GENERATE-SPECIAL-PROVIDERS-TEX.                  ELTCPO  
01077                                                                   ELTCPO  
01078 ************************************************************      ELTCPO  
01079 *                                                          *      ELTCPO  
01080 *        DISPLAY SPECIAL PROVIDERS SENTENCE                *      ELTCPO  
01081 *                                                          *      ELTCPO  
01082 ************************************************************      ELTCPO  
01083  DISPLAY-SPECIAL-PROVIDERS-SENT.                                  ELTCPO  
01084      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTCPO  
01085      MOVE WS-SPEC-PROV-MSG  TO COF-DTL-LINE                       ELTCPO  
01086          (COF-NBR-DTL-LINES).                                     ELTCPO  
01087      PERFORM LINK-TO-OUTPUT.                                      ELTCPO  
01088      EJECT                                                        ELTCPO  
01089                                                                   ELTCPO  
01090 ************************************************************      ELTCPO  
01091 *                                                          *      ELTCPO  
01092 *        SEARCH THE GSS ENTRY                              *      ELTCPO  
01093 *                                                          *      ELTCPO  
01094 ************************************************************      ELTCPO  
01095  SEARCH-THE-GSS-ENTRY.                                            ELTCPO  
01096      SET GSS-INDEX TO 1.                                          ELTCPO  
01097      SEARCH GSS-ENTRY                                             ELTCPO  
01098          AT END                                                   ELTCPO  
01099               SET TABULAR-IS-UNDEFINED TO TRUE                    ELTCPO  
01100          WHEN GSS-CP-PROG-CODE-CHR (GSS-INDEX)                    ELTCPO  
01101               CONTINUE                                            ELTCPO  
01102         END-SEARCH.                                               ELTCPO  
01103      IF TABULAR-IS-UNDEFINED                                      ELTCPO  
01104          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTCPO  
01105                                                                   ELTCPO  
01106 ************************************************************      ELTCPO  
01107 *                                                          *      ELTCPO  
01108 *        CALL CODES MANUAL INTERFACE                       *      ELTCPO  
01109 *                                                          *      ELTCPO  
01110 ************************************************************      ELTCPO  
01111  CALL-CODES-MANUAL-INTERFACE.                                     ELTCPO  
01112      MOVE WS-GCCP TO CMF-RECORD-PREFIX.                           ELTCPO  
01113      EXEC CICS LINK                                               ELTCPO  
01114                PROGRAM ('ELUCMIF')                                ELTCPO  
01115                COMMAREA (DFHCOMMAREA)                             ELTCPO  
01116        END-EXEC.                                                  ELTCPO  
01117      EJECT                                                        ELTCPO  
01118                                                                   ELTCPO  
01119 ************************************************************      ELTCPO  
01120 *                                                          *      ELTCPO  
01121 *        GET GCCP TABULAR                                  *      ELTCPO  
01122 *                                                          *      ELTCPO  
01123 ************************************************************      ELTCPO  
01124  GET-GCCP-TABULAR.                                                ELTCPO  
01125      PERFORM ESTABLISH-ADDRESSABILITY-OF-GC.                      ELTCPO  
01126      SET IOP-RD              TO TRUE.                             ELTCPO  
01127      SET IOP-STG-MODE-MOVE   TO TRUE.                             ELTCPO  
01128      SET IOP-FCQ-NONE        TO TRUE.                             ELTCPO  
01129      SET IOP-KVQ-EQ          TO TRUE.                             ELTCPO  
01130      MOVE KWA-GCTABULR-KEY   TO IOP-FILE-KEY.                     ELTCPO  
01131      PERFORM CALL-INPUT-OUTPUT-MODULE.                            ELTCPO  
01132      IF IOP-RC-OK                                                 ELTCPO  
01133          PERFORM ESTABLISH-ADDRESSY-OF-GCCP-TAB                   ELTCPO  
01134      ELSE IF IOP-RC-NOTFND                                        ELTCPO  
01135          PERFORM SIGNAL-NOT-FOUND-GCTAB-ERROR                     ELTCPO  
01136      ELSE                                                         ELTCPO  
01137          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTCPO  
01138      EJECT                                                        ELTCPO  
01139                                                                   ELTCPO  
01140 ************************************************************      ELTCPO  
01141 *                                                          *      ELTCPO  
01142 *        ESTABLISH ADDRESSABILITY OF GCTABULAR IO PARAMETER*      ELTCPO  
01143 *                                                          *      ELTCPO  
01144 ************************************************************      ELTCPO  
01145  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTCPO  
01146      SET CIA-GCTABULR-DDN TO TRUE.                                ELTCPO  
01147      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCPO  
01148                            ADDRESS OF                             ELTCPO  
01149          IOP-INPUT-OUTPUT-PARAMETERS.                             ELTCPO  
01150      EJECT                                                        ELTCPO  
01151                                                                   ELTCPO  
01152 ************************************************************      ELTCPO  
01153 *                                                          *      ELTCPO  
01154 *        CALL INPUT OUTPUT MODULE                          *      ELTCPO  
01155 *                                                          *      ELTCPO  
01156 ************************************************************      ELTCPO  
01157  CALL-INPUT-OUTPUT-MODULE.                                        ELTCPO  
01158      EXEC CICS LINK                                               ELTCPO  
01159                PROGRAM ('ELUIOPGM')                               ELTCPO  
01160                COMMAREA (DFHCOMMAREA)                             ELTCPO  
01161        END-EXEC.                                                  ELTCPO  
01162      EJECT                                                        ELTCPO  
01163                                                                   ELTCPO  
01164 ************************************************************      ELTCPO  
01165 *                                                          *      ELTCPO  
01166 *        ESTABLISH ADDRESSY OF GCCP TABULAR                *      ELTCPO  
01167 *                                                          *      ELTCPO  
01168 ************************************************************      ELTCPO  
01169  ESTABLISH-ADDRESSY-OF-GCCP-TAB.                                  ELTCPO  
01170      SET ADDRESS OF GCCP-TABULAR-REC-AREA TO                      ELTCPO  
01171          IOP-REC-PTR.                                             ELTCPO  
01172      SET IOP-REC-PTR TO NULL.                                     ELTCPO  
01173      EJECT                                                        ELTCPO  
01174                                                                   ELTCPO  
01175 ************************************************************      ELTCPO  
01176 *                                                          *      ELTCPO  
01177 *        SIGNAL NOT FOUND GCTAB ERROR                      *      ELTCPO  
01178 *                                                          *      ELTCPO  
01179 ************************************************************      ELTCPO  
01180  SIGNAL-NOT-FOUND-GCTAB-ERROR.                                    ELTCPO  
01181      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTCPO  
01182      PERFORM SIGNAL-ABEND.                                        ELTCPO  
01183      EJECT                                                        ELTCPO  
01184                                                                   ELTCPO  
01185 ************************************************************      ELTCPO  
01186 *                                                          *      ELTCPO  
01187 *        SIGNAL CRITICAL IO ERROR                          *      ELTCPO  
01188 *                                                          *      ELTCPO  
01189 ************************************************************      ELTCPO  
01190  SIGNAL-CRITICAL-IO-ERROR.                                        ELTCPO  
01191      SET CIA-AB-CRITIO TO TRUE.                                   ELTCPO  
01192      PERFORM SIGNAL-ABEND.                                        ELTCPO  
01193      EJECT                                                        ELTCPO  
01194                                                                   ELTCPO  
01195 ************************************************************      ELTCPO  
01196 *                                                          *      ELTCPO  
01197 *        GENERATE SPECIAL PROVIDERS TEXT                   *      ELTCPO  
01198 *                                                          *      ELTCPO  
01199 ************************************************************      ELTCPO  
01200  GENERATE-SPECIAL-PROVIDERS-TEX.                                  ELTCPO  
01201      MOVE 'COMMUNITY PROVIDER OPTION' TO SRP-CCP-NAME.            ELTCPO  
01202      MOVE  WS-GCPO                        TO SRP-TABULAR-ID.      ELTCPO  
01203      MOVE  WS-GCPO-PROV-SLOT-NO           TO SRP-TABULAR-SLOT-NO. ELTCPO  
01204      PERFORM CALL-SPECIAL-PROVIDERS-GENERAT.                      ELTCPO  
01205                                                                   ELTCPO  
01206                                                                   ELTCPO  
01207 ************************************************************      ELTCPO  
01208 *                                                          *      ELTCPO  
01209 *        CALL SPECIAL PROVIDERS GENERATOR                  *      ELTCPO  
01210 *                                                          *      ELTCPO  
01211 ************************************************************      ELTCPO  
01212  CALL-SPECIAL-PROVIDERS-GENERAT.                                  ELTCPO  
01213      EXEC CICS LINK                                               ELTCPO  
01214                PROGRAM ('ELGGXXC')                                ELTCPO  
01215                COMMAREA (DFHCOMMAREA)                             ELTCPO  
01216         END-EXEC.                                                 ELTCPO  
01217                                                                   ELTCPO  
01218 ************************************************************      ELTCPO  
01219 *                                                          *      ELTCPO  
01220 *        GENERATE COPAY                                    *      ELTCPO  
01221 *                                                          *      ELTCPO  
01222 ************************************************************      ELTCPO  
01223  GENERATE-COPAY.                                                  ELTCPO  
01224      EXEC CICS LINK                                               ELTCPO  
01225                PROGRAM ('ELGACPCC')                               ELTCPO  
01226                COMMAREA (DFHCOMMAREA)                             ELTCPO  
01227         END-EXEC.                                                 ELTCPO  
01228                                                                   ELTCPO  
01229 ************************************************************      ELTCPO  
01230 *                                                          *      ELTCPO  
01231 *        GENERATE COINSURANCE                              *      ELTCPO  
01232 *                                                          *      ELTCPO  
01233 ************************************************************      ELTCPO  
01234  GENERATE-COINSURANCE.                                            ELTCPO  
01235      EXEC CICS LINK                                               ELTCPO  
01236                PROGRAM ('ELGACLCC')                               ELTCPO  
01237                COMMAREA (DFHCOMMAREA)                             ELTCPO  
01238         END-EXEC.                                                 ELTCPO  
01239                                                                   ELTCPO  
01240 ************************************************************      ELTCPO  
01241 *                                                          *      ELTCPO  
01242 *        GENERATE DEDUCTIBLE                               *      ELTCPO  
01243 *                                                          *      ELTCPO  
01244 ************************************************************      ELTCPO  
01245  GENERATE-DEDUCTIBLE.                                             ELTCPO  
01246      EXEC CICS LINK                                               ELTCPO  
01247                PROGRAM ('ELGADLCC')                               ELTCPO  
01248                COMMAREA (DFHCOMMAREA)                             ELTCPO  
01249         END-EXEC.                                                 ELTCPO  
01250                                                                   ELTCPO  
01251 ************************************************************      ELTCPO  
01252 *                                                          *      ELTCPO  
01253 *        GENERATE MAXIMUM                                  *      ELTCPO  
01254 *                                                          *      ELTCPO  
01255 ************************************************************      ELTCPO  
01256  GENERATE-MAXIMUM.                                                ELTCPO  
01257      EXEC CICS LINK                                               ELTCPO  
01258                PROGRAM ('ELGABMCC')                               ELTCPO  
01259                COMMAREA (DFHCOMMAREA)                             ELTCPO  
01260         END-EXEC.                                                 ELTCPO  
01261      EJECT                                                        ELTCPO  
01262                                                                   ELTCPO  
01263 ************************************************************      ELTCPO  
01264 *                                                          *      ELTCPO  
01265 *        GENERATE OUT OF POCKET                            *      ELTCPO  
01266 *                                                          *      ELTCPO  
01267 ************************************************************      ELTCPO  
01268  GENERATE-OUT-OF-POCKET.                                          ELTCPO  
01269      EXEC CICS LINK                                               ELTCPO  
01270                PROGRAM ('ELGAOLCC')                               ELTCPO  
01271                COMMAREA (DFHCOMMAREA)                             ELTCPO  
01272         END-EXEC.                                                 ELTCPO  
01273                                                                   ELTCPO  
01274 ************************************************************      ELTCPO  
01275 *                                                          *      ELTCPO  
01276 *        CALL CBRI INTERFACE                               *      ELTCPO  
01277 *                                                          *      ELTCPO  
01278 ************************************************************      ELTCPO  
01279  CALL-CBRI-INTERFACE.                                             ELTCPO  
01280      CALL 'ELGCBRI' USING DFHEIBLK                                ELTCPO  
01281                           DFHCOMMAREA.                            ELTCPO  
01282                                                                   ELTCPO  
01283 ************************************************************      ELTCPO  
01284 *                                                          *      ELTCPO  
01285 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTCPO  
01286 *                                                          *      ELTCPO  
01287 ************************************************************      ELTCPO  
01288  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTCPO  
01289      PERFORM INITIALIZE-CMOUT.                                    ELTCPO  
01290      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTCPO  
01291      EJECT                                                        ELTCPO  
01292                                                                   ELTCPO  
01293 ************************************************************      ELTCPO  
01294 *                                                          *      ELTCPO  
01295 *        PREPARE TEXT FOR OUTPUT                           *      ELTCPO  
01296 *                                                          *      ELTCPO  
01297 ************************************************************      ELTCPO  
01298  PREPARE-TEXT-FOR-OUTPUT.                                         ELTCPO  
01299      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTCPO  
01300          UNTIL CMF-DESCR-IDX                                      ELTCPO  
01301                                    GREATER THAN                   ELTCPO  
01302              CMF-NBR-DESCR-LINES.                                 ELTCPO  
01303      EJECT                                                        ELTCPO  
01304                                                                   ELTCPO  
01305 ************************************************************      ELTCPO  
01306 *                                                          *      ELTCPO  
01307 *        INITIALIZE CMOUT                                  *      ELTCPO  
01308 *                                                          *      ELTCPO  
01309 ************************************************************      ELTCPO  
01310  INITIALIZE-CMOUT.                                                ELTCPO  
01311      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTCPO  
01312      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCPO  
01313          ADDRESS OF CMF-DESCR.                                    ELTCPO  
01314      SET CMF-DESCR-IDX TO 1.                                      ELTCPO  
01315      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTCPO  
01316                                                                   ELTCPO  
01317                                                                   ELTCPO  
01318 ************************************************************      ELTCPO  
01319 *                                                          *      ELTCPO  
01320 *        MOVE CMF TEXT TO OUTPUT                           *      ELTCPO  
01321 *                                                          *      ELTCPO  
01322 ************************************************************      ELTCPO  
01323  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTCPO  
01324      PERFORM MOVE-A-LINE.                                         ELTCPO  
01325      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTCPO  
01326          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTCPO  
01327      IF TCAR-FROM-SUB GREATER THAN 20                             ELTCPO  
01328               OR CMF-DESCR-IDX GREATER THAN                       ELTCPO  
01329          CMF-NBR-DESCR-LINES                                      ELTCPO  
01330          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTCPO  
01331                                                                   ELTCPO  
01332                                                                   ELTCPO  
01333 ************************************************************      ELTCPO  
01334 *                                                          *      ELTCPO  
01335 *        FINISH CODES MANUAL TEXT                          *      ELTCPO  
01336 *                                                          *      ELTCPO  
01337 ************************************************************      ELTCPO  
01338  FINISH-CODES-MANUAL-TEXT.                                        ELTCPO  
01339      SET DONE-PROCESSING TO TRUE.                                 ELTCPO  
01340      IF PERIOD-NEEDED                                             ELTCPO  
01341          PERFORM GET-AND-MOVE-PERIOD.                             ELTCPO  
01342      EJECT                                                        ELTCPO  
01343                                                                   ELTCPO  
01344                                                                   ELTCPO  
01345 ************************************************************      ELTCPO  
01346 *                                                          *      ELTCPO  
01347 *        GET AND MOVE PERIOD                               *      ELTCPO  
01348 *                                                          *      ELTCPO  
01349 ************************************************************      ELTCPO  
01350  GET-AND-MOVE-PERIOD.                                             ELTCPO  
01351      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTCPO  
01352          (TCAR-FROM-SUB).                                         ELTCPO  
01353                                                                   ELTCPO  
01354                                                                   ELTCPO  
01355 ************************************************************      ELTCPO  
01356 *                                                          *      ELTCPO  
01357 *        SAVE LAST LINE                                    *      ELTCPO  
01358 *                                                          *      ELTCPO  
01359 ************************************************************      ELTCPO  
01360  SAVE-LAST-LINE.                                                  ELTCPO  
01361      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCPO  
01362      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTCPO  
01363         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTCPO  
01364      ADD 1 TO TCAR-FROM-SUB.                                      ELTCPO  
01365      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTCPO  
01366                                                                   ELTCPO  
01367                                                                   ELTCPO  
01368 ************************************************************      ELTCPO  
01369 *                                                          *      ELTCPO  
01370 *        OUTPUT LAST LINE                                  *      ELTCPO  
01371 *                                                          *      ELTCPO  
01372 ************************************************************      ELTCPO  
01373  OUTPUT-LAST-LINE.                                                ELTCPO  
01374      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTCPO  
01375          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTCPO  
01376      IF BLANK-LINE-NEEDED                                         ELTCPO  
01377          PERFORM CREATE-A-BLANK-LINE.                             ELTCPO  
01378                                                                   ELTCPO  
01379                                                                   ELTCPO  
01380 ************************************************************      ELTCPO  
01381 *                                                          *      ELTCPO  
01382 *        CREATE A BLANK LINE                               *      ELTCPO  
01383 *                                                          *      ELTCPO  
01384 ************************************************************      ELTCPO  
01385  CREATE-A-BLANK-LINE.                                             ELTCPO  
01386      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTCPO  
01387      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTCPO  
01388                                                                   ELTCPO  
01389                                                                   ELTCPO  
01390 ************************************************************      ELTCPO  
01391 *                                                          *      ELTCPO  
01392 *        MOVE A LINE                                       *      ELTCPO  
01393 *                                                          *      ELTCPO  
01394 ************************************************************      ELTCPO  
01395  MOVE-A-LINE.                                                     ELTCPO  
01396      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTCPO  
01397          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTCPO  
01398      SET CMF-DESCR-IDX UP BY 1.                                   ELTCPO  
01399      ADD 1 TO TCAR-FROM-SUB.                                      ELTCPO  
01400      EJECT                                                        ELTCPO  
01401                                                                   ELTCPO  
01402                                                                   ELTCPO  
01403 ************************************************************      ELTCPO  
01404 *                                                          *      ELTCPO  
01405 *        REFORMAT AND WRITE TEXT                           *      ELTCPO  
01406 *                                                          *      ELTCPO  
01407 ************************************************************      ELTCPO  
01408  REFORMAT-AND-WRITE-TEXT.                                         ELTCPO  
01409      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTCPO  
01410      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTCPO  
01411      PERFORM UNSTRING-TEXT.                                       ELTCPO  
01412      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCPO  
01413      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTCPO  
01414      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTCPO  
01415          UNTIL COF-NBR-DTL-LINES GREATER                          ELTCPO  
01416                                   TCAR-OUTPUT-FIELDS-USED -       ELTCPO  
01417              1.                                                   ELTCPO  
01418      PERFORM DISPOSE-OF-LAST-LINE.                                ELTCPO  
01419      PERFORM LINK-TO-OUTPUT.                                      ELTCPO  
01420                                                                   ELTCPO  
01421                                                                   ELTCPO  
01422 ************************************************************      ELTCPO  
01423 *                                                          *      ELTCPO  
01424 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTCPO  
01425 *                                                          *      ELTCPO  
01426 ************************************************************      ELTCPO  
01427  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTCPO  
01428      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTCPO  
01429           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTCPO  
01430      ADD +1 TO TCAR-FROM-SUB.                                     ELTCPO  
01431      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTCPO  
01432      EJECT                                                        ELTCPO  
01433                                                                   ELTCPO  
01434                                                                   ELTCPO  
01435 ************************************************************      ELTCPO  
01436 *                                                          *      ELTCPO  
01437 *        UNSTRING TEXT                                     *      ELTCPO  
01438 *                                                          *      ELTCPO  
01439 ************************************************************      ELTCPO  
01440  UNSTRING-TEXT.                                                   ELTCPO  
01441      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTCPO  
01442      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTCPO  
01443      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTCPO  
01444      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTCPO  
01445      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTCPO  
01446      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTCPO  
01447      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTCPO  
01448      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTCPO  
01449      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTCPO  
01450      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTCPO  
01451      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTCPO  
01452      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTCPO  
01453      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTCPO  
01454      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTCPO  
01455      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTCPO  
01456      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTCPO  
01457      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTCPO  
01458      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTCPO  
01459      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTCPO  
01460      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTCPO  
01461      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTCPO  
01462      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTCPO  
01463      EJECT                                                        ELTCPO  
01464                                                                   ELTCPO  
01465                                                                   ELTCPO  
01466 ************************************************************      ELTCPO  
01467 *                                                          *      ELTCPO  
01468 *        LINK TO OUTPUT                                    *      ELTCPO  
01469 *                                                          *      ELTCPO  
01470 ************************************************************      ELTCPO  
01471  LINK-TO-OUTPUT.                                                  ELTCPO  
01472      EXEC CICS LINK                                               ELTCPO  
01473          PROGRAM ('ELUOUTPT')                                     ELTCPO  
01474          COMMAREA (DFHCOMMAREA)                                   ELTCPO  
01475          END-EXEC.                                                ELTCPO  
01476      EJECT                                                        ELTCPO  
01477                                                                   ELTCPO  
01478                                                                   ELTCPO  
01479 ************************************************************      ELTCPO  
01480 *                                                          *      ELTCPO  
01481 *        DISPOSE OF LAST LINE                              *      ELTCPO  
01482 *                                                          *      ELTCPO  
01483 ************************************************************      ELTCPO  
01484  DISPOSE-OF-LAST-LINE.                                            ELTCPO  
01485      IF NOT ADDITIONAL-TEXT                                       ELTCPO  
01486          PERFORM INITIALIZE-CONTINUED-SW.                         ELTCPO  
01487      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTCPO  
01488          PERFORM SAVE-LAST-LINE                                   ELTCPO  
01489      ELSE                                                         ELTCPO  
01490          PERFORM OUTPUT-LAST-LINE.                                ELTCPO  
01491                                                                   ELTCPO  
01492                                                                   ELTCPO  
01493 ************************************************************      ELTCPO  
01494 *                                                          *      ELTCPO  
01495 *        INITIALIZE CONTINUED SW                           *      ELTCPO  
01496 *                                                          *      ELTCPO  
01497 ************************************************************      ELTCPO  
01498  INITIALIZE-CONTINUED-SW.                                         ELTCPO  
01499      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTCPO  
