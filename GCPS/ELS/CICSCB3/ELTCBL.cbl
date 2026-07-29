00001 *      LAST MAINTENANCE TIME:  7.54.00  DATE: 06/13/91            06/29/02
00002  IDENTIFICATION DIVISION.                                         ELTCBL  
00003                                                                      LV001
00004  PROGRAM-ID.         ELTCBL.                                      ELTCBL  
00005                                                                   ELTCBL  
00006  AUTHOR.             ANNE KEFFER KING.                            ELTCBL  
00007                      CLONED FORM ELTRPO.                          ELTCBL  
00008                                                                   ELTCBL  
00009  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTCBL  
00010                      A MUTUAL LEGAL RESERVE COMPANY               ELTCBL  
00011                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTCBL  
00012                      233 N. MICHIGAN AVE                          ELTCBL  
00013                      CHICAGO, ILLINOIS 60601                      ELTCBL  
00014                                                                   ELTCBL  
00015  DATE-WRITTEN.       01-MAR-1996.                                 ELTCBL  
00016                                                                   ELTCBL  
00017  DATE-COMPILED.                                                   ELTCBL  
00018                                                                   ELTCBL  
00019  SECURITY.           COPYRIGHT 1986,                              ELTCBL  
00020                      HEALTH CARE SERVICE CORPORATION              ELTCBL  
00021      SKIP3                                                        ELTCBL  
00022  ENVIRONMENT DIVISION.                                            ELTCBL  
00023                                                                   ELTCBL  
00024  CONFIGURATION SECTION.                                           ELTCBL  
00025  SOURCE-COMPUTER.    IBM-3090.                                    ELTCBL  
00026  OBJECT-COMPUTER.    IBM-3090.                                    ELTCBL  
00027      EJECT                                                        ELTCBL  
00028 ******************************************************************ELTCBL  
00029 *                                                                *ELTCBL  
00030 *    COPYBOOK:   ELTCBL                                          *ELTCBL  
00031 *    DATE:       01-MAR-1994                                     *ELTCBL  
00032 *    AUTHOR:     ANNE KEFFER KING.                               *ELTCBL  
00033 *    FUNCTION:   THIS MODULE WILL DISPLAY ALL OUTPUT ASSOCIATED  *ELTCBL  
00034 *                WITH RESTRICTED PROVIDER OPTION PROGRAM.        *ELTCBL  
00035 *    NOTES:      X---                                            *ELTCBL  
00036 *                                                                *ELTCBL  
00037 ******************************************************************ELTCBL  
00038 *                                                                *ELTCBL  
00039 *                      MAINTENANCE HISTORY                       *ELTCBL  
00040 *                                                                *ELTCBL  
00041 *  MOD     DATE     BY  DRPT                ACTION               *ELTCBL  
00042 * ----- ----------- --- ----- ---------------------------------- *ELTCBL  
00043 * 01.00 01-MAR-1996 AKK       CREATED.                           *ELTCBL  
00044 *                                                                *ELTCBL  
00045 ******************************************************************ELTCBL  
00046                                                                   ELTCBL  
00047  DATA DIVISION.                                                   ELTCBL  
00048                                                                   ELTCBL  
00049  WORKING-STORAGE SECTION.                                         ELTCBL  
00050  01  WS-MISC.                                                     ELTCBL  
00051      05  WS-BEGIN                        PIC X(26) VALUE          ELTCBL  
00052      '*** ELTCBL WS BEGINS ***'.                                  ELTCBL  
00053      05  WS-POINTER2                     POINTER.                 ELTCBL  
00054      05  WS-POINTER3                     POINTER.                 ELTCBL  
00055                                                                   ELTCBL  
00056  01  WS-SWITCHES.                                                 ELTCBL  
00057      05  ADDITIONAL-TEXT-SWITCH   PIC X(01) VALUE SPACE.          ELTCBL  
00058          88  ADDITIONAL-TEXT                VALUE 'A'.            ELTCBL  
00059          88  BLANK-LINE-NEEDED              VALUE 'B'.            ELTCBL  
00060      05  CONTINUED-PROCESSING-SW  PIC X(01) VALUE SPACE.          ELTCBL  
00061          88  DONE-PROCESSING                VALUE 'D'.            ELTCBL  
00062          88  PROCESSING-CMF-TEXT            VALUE 'P'.            ELTCBL  
00063      05  WS-TABULAR-SWITCH        PIC X(01) VALUE 'N'.            ELTCBL  
00064          88  TABULAR-IS-UNDEFINED           VALUE 'Y'.            ELTCBL  
00065      05  WS-PERIOD-SWITCH         PIC X(01) VALUE 'N'.            ELTCBL  
00066          88  PERIOD-NEEDED                  VALUE 'Y'.            ELTCBL  
00067      05  WS-APPROVAL-SOURCE-SW    PIC X     VALUE SPACE.          ELTCBL  
00068          88  NOT-HOLDING-APPROVAL-SRCE      VALUE 'N'.            ELTCBL  
00069          88  HOLDING-APPROVAL-SOURCE        VALUE 'H'.            ELTCBL  
00070                                                                   ELTCBL  
00071  01  WS-HOLD-AREA.                                                ELTCBL  
00072      05  WS-GCBL-PROV-SLOT-NO   COMP-3 PIC S9(07) VALUE +0.       ELTCBL  
00073                                                                   ELTCBL  
00074 **************************************************************    ELTCBL  
00075 ***                   PROGRAM CONSTANTS                           ELTCBL  
00076 **************************************************************    ELTCBL  
00077      05  WS-GRP                   PIC X(06) VALUE 'GROUP'.        ELTCBL  
00078      05  WS-GCCP                  PIC X(06) VALUE '#GCCP '.       ELTCBL  
00079      05  WS-GCBL                  PIC X(06) VALUE '#GCBL '.       ELTCBL  
00080      05  WS-INST                  PIC X(13) VALUE 'INSTITUTIONAL'.ELTCBL  
00081      05  WS-PROF                  PIC X(13) VALUE 'PROFESSIONAL '.ELTCBL  
00082      05  WS-SUPP                  PIC X(13) VALUE 'SUPPLEMENTAL '.ELTCBL  
00083      05  WS-APPROVAL              PIC X(09) VALUE 'APPROVAL.'.    ELTCBL  
00084                                                                   ELTCBL  
00085 **************************************************************    ELTCBL  
00086 ***                   HEADER  LINE                                ELTCBL  
00087 **************************************************************    ELTCBL  
00088      05  WS-HEADER-LINE.                                          ELTCBL  
00089          10  FILLER               PIC X(18) VALUE SPACES.         ELTCBL  
00090          10  FILLER               PIC X(30) VALUE                 ELTCBL  
00091          'COMMUNITY BLUE OPTION PROGRAM '.                        ELTCBL  
00092          10  WS-HDR-LINE-BCBSMM   PIC X(13) VALUE SPACES.         ELTCBL  
00093          10  FILLER               PIC X(17) VALUE SPACES.         ELTCBL  
00094                                                                   ELTCBL  
00095 **************************************************************    ELTCBL  
00096 ***                   SCREEN BODY LINES                           ELTCBL  
00097 **************************************************************    ELTCBL  
00098  01  WS-SCREEN-LINE-AREA.                                         ELTCBL  
00099      05  WS-APPRVL-LINE.                                          ELTCBL  
00100          10  FILLER               PIC X(47) VALUE                 ELTCBL  
00101          'COMMUNITY BLUE OPTION PROGRAM REQUIRES '.               ELTCBL  
00102          10  FILLER               PIC X(32) VALUE SPACES.         ELTCBL  
00103                                                                   ELTCBL  
00104      05  WS-ALT-PRICING-LINE-BC.                                  ELTCBL  
00105          10  FILLER               PIC X(52) VALUE                 ELTCBL  
00106          'THE ALTERNATE PRICING FOR INSTITUTIONAL SERVICES IS '.  ELTCBL  
00107          10  FILLER               PIC X(27) VALUE SPACES.         ELTCBL  
00108                                                                   ELTCBL  
00109      05  WS-ALT-PRICING-LINE-BS.                                  ELTCBL  
00110          10  FILLER               PIC X(51) VALUE                 ELTCBL  
00111          'THE ALTERNATE PRICING FOR PROFESSIONAL SERVICES IS '.   ELTCBL  
00112          10  FILLER               PIC X(28) VALUE SPACES.         ELTCBL  
00113                                                                   ELTCBL  
00114      05  WS-ALT-PRICING-LINE-MM.                                  ELTCBL  
00115          10  FILLER               PIC X(51) VALUE                 ELTCBL  
00116          'THE ALTERNATE PRICING FOR SUPPLEMENTAL BENEFITS IS '.   ELTCBL  
00117          10  FILLER               PIC X(28) VALUE SPACES.         ELTCBL  
00118                                                                   ELTCBL  
00119      05  WS-BENE-REDUCT-LINE.                                     ELTCBL  
00120          10  FILLER               PIC X(47) VALUE                 ELTCBL  
00121          'DENIED OR REDUCED BENEFITS DUE TO THIS PROGRAM:'.       ELTCBL  
00122          10  FILLER               PIC X(32) VALUE SPACES.         ELTCBL  
00123                                                                   ELTCBL  
00124      05  WS-SPILL-OVER-LINE.                                      ELTCBL  
00125          10  FILLER               PIC X(51) VALUE                 ELTCBL  
00126          'UNPAID SERVICES AFTER BASIC BENEFITS REDUCTION ARE '.   ELTCBL  
00127          10  FILLER               PIC X(28) VALUE SPACES.         ELTCBL  
00128                                                                   ELTCBL  
00129 **************************************************************    ELTCBL  
00130 ** SPECIAL MESSAGE FOR THE NOT APPLICABLE                         ELTCBL  
00131 ** ALSO THE FIXED TEXT FOR TABULAR GCBL                           ELTCBL  
00132 **************************************************************    ELTCBL  
00133      05  WS-CBL-APPLIES.                                          ELTCBL  
00134          10  FILLER               PIC X(79) VALUE                 ELTCBL  
00135          'THE COMMUNITY BLUE OPTION PROGRAM APPLIES TO'.          ELTCBL  
00136                                                                   ELTCBL  
00137      05  WS-NOT-APPLICABLE-MSG.                                   ELTCBL  
00138          10  FILLER               PIC X(34) VALUE                 ELTCBL  
00139          'THE COMMUNITY BLUE OPTION PROGRAM '.                    ELTCBL  
00140          10  FILLER               PIC X(18) VALUE                 ELTCBL  
00141          'IS NOT APPLICABLE.'.                                    ELTCBL  
00142          10  FILLER               PIC X(27) VALUE SPACES.         ELTCBL  
00143                                                                   ELTCBL  
00144      05  WS-SPEC-PROV-MSG.                                        ELTCBL  
00145          10  FILLER               PIC X(79) VALUE                 ELTCBL  
00146          'THERE ARE SPECIAL PROVIDERS INCLUDED IN THIS COST CONTAIELTCBL  
00147 -        'NMENT PROGRAM.'.                                        ELTCBL  
00148                                                                   ELTCBL  
00149      05  WS-DISCLAIMER-MSG.                                       ELTCBL  
00150          10  FILLER               PIC X(79) VALUE                 ELTCBL  
00151          '*** SUBJECT TO OTHER CONTRACT LIMITATIONS ***'.         ELTCBL  
00152                                                                   ELTCBL  
00153      05  WS-NOT-APPLY-TO-LOB-BC.                                  ELTCBL  
00154          10  FILLER               PIC X(45) VALUE                 ELTCBL  
00155          'COMMUNITY BLUE OPTION PROGRAM DOES NOT APPLY '.         ELTCBL  
00156          10  FILLER               PIC X(34) VALUE                 ELTCBL  
00157          'FOR INSTITUTIONAL BENEFITS'.                            ELTCBL  
00158                                                                   ELTCBL  
00159      05  WS-NOT-APPLY-TO-LOB-BS.                                  ELTCBL  
00160          10  FILLER               PIC X(45) VALUE                 ELTCBL  
00161          'COMMUNITY BLUE OPTION PROGRAM DOES NOT APPLY '.         ELTCBL  
00162          10  FILLER               PIC X(34) VALUE                 ELTCBL  
00163          'FOR PROFESSIONAL BENEFITS.'.                            ELTCBL  
00164                                                                   ELTCBL  
00165      05  WS-NOT-APPLY-TO-LOB-MM.                                  ELTCBL  
00166          10  FILLER               PIC X(45) VALUE                 ELTCBL  
00167          'COMMUNITY BLUE OPTION PROGRAM DOES NOT APPLY '.         ELTCBL  
00168          10  FILLER               PIC X(34) VALUE                 ELTCBL  
00169          'FOR SUPPLEMENTAL BENEFITS.'.                            ELTCBL  
00170                                                                   ELTCBL  
00171  LINKAGE SECTION.                                                 ELTCBL  
00172  01  DFHCOMMAREA.                                                 ELTCBL  
00173      COPY ELSCOMMC.                                               ELTCBL  
00174 /                                                                 ELTCBL  
00175      COPY ELSCIA2C.                                               ELTCBL  
00176 /                                                                 ELTCBL  
00177      COPY ELSCMDSC.                                               ELTCBL  
00178 /                                                                 ELTCBL  
00179      COPY ELSCMIFC.                                               ELTCBL  
00180 /                                                                 ELTCBL  
00181      COPY ELSIOPMC.                                               ELTCBL  
00182 /                                                                 ELTCBL  
00183      COPY ELSKEYSC.                                               ELTCBL  
00184 /                                                                 ELTCBL  
00185      COPY ELSOUTPC.                                               ELTCBL  
00186 /                                                                 ELTCBL  
00187      COPY ELSSRTPC.                                               ELTCBL  
00188 /                                                                 ELTCBL  
00189      COPY ELSTCWAC.                                               ELTCBL  
00190 /                                                                 ELTCBL  
00191      COPY ELSSSCBC.                                               ELTCBL  
00192 /                                                                 ELTCBL  
00193  01  GROUP-SPECIFIC-REC.                                          ELTCBL  
00194      COPY GCGROUPC.                                               ELTCBL  
00195 /                                                                 ELTCBL  
00196  01  GCCP-TABULAR-REC-AREA.                                       ELTCBL  
00197      COPY GCTGCCPC.                                               ELTCBL  
00198      EJECT                                                        ELTCBL  
00199  PROCEDURE DIVISION.                                              ELTCBL  
00200 ************************************************************      ELTCBL  
00201 *                                                          *      ELTCBL  
00202 *                    PROCEDURE DIVISION                    *      ELTCBL  
00203 *                                                          *      ELTCBL  
00204 ************************************************************      ELTCBL  
00205                                                                   ELTCBL  
00206                                                                   ELTCBL  
00207 ************************************************************      ELTCBL  
00208 *                                                          *      ELTCBL  
00209 *        COMMUNITY PROVIDER OPTION                         *      ELTCBL  
00210 *                                                          *      ELTCBL  
00211 ************************************************************      ELTCBL  
00212  COMMUNITY-BLUE-PROVIDER-OPTION.                                  ELTCBL  
00213      PERFORM INITIALIZATION.                                      ELTCBL  
00214      PERFORM PROCESS.                                             ELTCBL  
00215      GOBACK.                                                      ELTCBL  
00216                                                                   ELTCBL  
00217 ************************************************************      ELTCBL  
00218 *                                                          *      ELTCBL  
00219 *        INITIALIZATION                                    *      ELTCBL  
00220 *                                                          *      ELTCBL  
00221 ************************************************************      ELTCBL  
00222  INITIALIZATION.                                                  ELTCBL  
00223      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTCBL  
00224      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTCBL  
00225                                                                   ELTCBL  
00226 ************************************************************      ELTCBL  
00227 *                                                          *      ELTCBL  
00228 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTCBL  
00229 *                                                          *      ELTCBL  
00230 ************************************************************      ELTCBL  
00231  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTCBL  
00232      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTCBL  
00233      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTCBL  
00234      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTCBL  
00235                                                                   ELTCBL  
00236 ************************************************************      ELTCBL  
00237 *                                                          *      ELTCBL  
00238 *        CHECK FOR VALID COMMAREA                          *      ELTCBL  
00239 *                                                          *      ELTCBL  
00240 ************************************************************      ELTCBL  
00241  CHECK-FOR-VALID-COMMAREA.                                        ELTCBL  
00242      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTCBL  
00243          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTCBL  
00244                                                                   ELTCBL  
00245 ************************************************************      ELTCBL  
00246 *                                                          *      ELTCBL  
00247 *        SIGNAL INVALID COMMAREA                           *      ELTCBL  
00248 *                                                          *      ELTCBL  
00249 ************************************************************      ELTCBL  
00250  SIGNAL-INVALID-COMMAREA.                                         ELTCBL  
00251      EXEC CICS ABEND                                              ELTCBL  
00252                ABCODE('EL01')                                     ELTCBL  
00253         END-EXEC.                                                 ELTCBL  
00254      EJECT                                                        ELTCBL  
00255                                                                   ELTCBL  
00256 ************************************************************      ELTCBL  
00257 *                                                          *      ELTCBL  
00258 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTCBL  
00259 *                                                          *      ELTCBL  
00260 ************************************************************      ELTCBL  
00261  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTCBL  
00262      IF ECA-CIA-PTR = NULL                                        ELTCBL  
00263          PERFORM SIGNAL-INVALID-CIA                               ELTCBL  
00264      ELSE                                                         ELTCBL  
00265          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTCBL  
00266                                                                   ELTCBL  
00267                                                                   ELTCBL  
00268 ************************************************************      ELTCBL  
00269 *                                                          *      ELTCBL  
00270 *        SIGNAL INVALID CIA                                *      ELTCBL  
00271 *                                                          *      ELTCBL  
00272 ************************************************************      ELTCBL  
00273  SIGNAL-INVALID-CIA.                                              ELTCBL  
00274      EXEC CICS ABEND                                              ELTCBL  
00275                ABCODE('EL02')                                     ELTCBL  
00276         END-EXEC.                                                 ELTCBL  
00277                                                                   ELTCBL  
00278 ************************************************************      ELTCBL  
00279 *                                                          *      ELTCBL  
00280 *        ESTABLISH ADDRESS OF CIA                          *      ELTCBL  
00281 *                                                          *      ELTCBL  
00282 ************************************************************      ELTCBL  
00283  ESTABLISH-ADDRESS-OF-CIA.                                        ELTCBL  
00284      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTCBL  
00285                            ADDRESS OF                             ELTCBL  
00286          CIA-ELS-COMMON-INTERFACE-AREA.                           ELTCBL  
00287      EJECT                                                        ELTCBL  
00288                                                                   ELTCBL  
00289 ************************************************************      ELTCBL  
00290 *                                                          *      ELTCBL  
00291 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTCBL  
00292 *                                                          *      ELTCBL  
00293 ************************************************************      ELTCBL  
00294  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTCBL  
00295      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTCBL  
00296      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCBL  
00297                            ADDRESS OF                             ELTCBL  
00298          SSB-SELECTOR-STATUS-CTL-BLK.                             ELTCBL  
00299      IF CIA-RC-PTR-NULL                                           ELTCBL  
00300          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTCBL  
00301                                                                   ELTCBL  
00302 ************************************************************      ELTCBL  
00303 *                                                          *      ELTCBL  
00304 *        SIGNAL UNALLOC AREA ERROR                         *      ELTCBL  
00305 *                                                          *      ELTCBL  
00306 ************************************************************      ELTCBL  
00307  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTCBL  
00308      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTCBL  
00309      PERFORM SIGNAL-ABEND.                                        ELTCBL  
00310                                                                   ELTCBL  
00311 ************************************************************      ELTCBL  
00312 *                                                          *      ELTCBL  
00313 *        SIGNAL ABEND                                      *      ELTCBL  
00314 *                                                          *      ELTCBL  
00315 ************************************************************      ELTCBL  
00316  SIGNAL-ABEND.                                                    ELTCBL  
00317      EXEC CICS ABEND                                              ELTCBL  
00318                ABCODE(CIA-ABCODE)                                 ELTCBL  
00319         END-EXEC.                                                 ELTCBL  
00320      EJECT                                                        ELTCBL  
00321                                                                   ELTCBL  
00322 ************************************************************      ELTCBL  
00323 *                                                          *      ELTCBL  
00324 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTCBL  
00325 *                                                          *      ELTCBL  
00326 ************************************************************      ELTCBL  
00327  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTCBL  
00328      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTCBL  
00329      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTCBL  
00330      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTCBL  
00331      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTCBL  
00332      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTCBL  
00333      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTCBL  
00334      PERFORM ESTABLISH-ADDRESSABILITY-OF-CS.                      ELTCBL  
00335                                                                   ELTCBL  
00336 ************************************************************      ELTCBL  
00337 *                                                          *      ELTCBL  
00338 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTCBL  
00339 *                                                          *      ELTCBL  
00340 ************************************************************      ELTCBL  
00341  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTCBL  
00342      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTCBL  
00343      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCBL  
00344                            ADDRESS OF                             ELTCBL  
00345          CMF-CODES-MANUAL-INTERFACE.                              ELTCBL  
00346      IF CIA-RC-PTR-NULL                                           ELTCBL  
00347          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTCBL  
00348      EJECT                                                        ELTCBL  
00349                                                                   ELTCBL  
00350 ************************************************************      ELTCBL  
00351 *                                                          *      ELTCBL  
00352 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTCBL  
00353 *                                                          *      ELTCBL  
00354 ************************************************************      ELTCBL  
00355  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTCBL  
00356      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTCBL  
00357      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCBL  
00358                            ADDRESS OF                             ELTCBL  
00359          COF-OUTPUT-INTERFACE.                                    ELTCBL  
00360      IF CIA-RC-PTR-NULL                                           ELTCBL  
00361          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTCBL  
00362      EJECT                                                        ELTCBL  
00363                                                                   ELTCBL  
00364 ************************************************************      ELTCBL  
00365 *                                                          *      ELTCBL  
00366 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTCBL  
00367 *                                                          *      ELTCBL  
00368 ************************************************************      ELTCBL  
00369  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTCBL  
00370      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTCBL  
00371      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCBL  
00372                            ADDRESS OF                             ELTCBL  
00373          SRP-SUBROUTINE-PARAMETERS.                               ELTCBL  
00374      IF CIA-RC-PTR-NULL                                           ELTCBL  
00375          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTCBL  
00376      EJECT                                                        ELTCBL  
00377                                                                   ELTCBL  
00378 ************************************************************      ELTCBL  
00379 *                                                          *      ELTCBL  
00380 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTCBL  
00381 *                                                          *      ELTCBL  
00382 ************************************************************      ELTCBL  
00383  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTCBL  
00384      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTCBL  
00385      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCBL  
00386                            ADDRESS OF                             ELTCBL  
00387          TCAR-COMPRESSION-WORK-AREA.                              ELTCBL  
00388      IF CIA-RC-PTR-NULL                                           ELTCBL  
00389          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTCBL  
00390      EJECT                                                        ELTCBL  
00391                                                                   ELTCBL  
00392 ************************************************************      ELTCBL  
00393 *                                                          *      ELTCBL  
00394 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTCBL  
00395 *                                                          *      ELTCBL  
00396 ************************************************************      ELTCBL  
00397  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTCBL  
00398      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTCBL  
00399      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCBL  
00400                            ADDRESS OF                             ELTCBL  
00401          KWA-FILE-KEY-WORK-AREA.                                  ELTCBL  
00402      IF CIA-RC-PTR-NULL                                           ELTCBL  
00403          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTCBL  
00404      EJECT                                                        ELTCBL  
00405                                                                   ELTCBL  
00406 ************************************************************      ELTCBL  
00407 *                                                          *      ELTCBL  
00408 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELTCBL  
00409 *                                                          *      ELTCBL  
00410 ************************************************************      ELTCBL  
00411  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTCBL  
00412      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTCBL  
00413      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCBL  
00414                            ADDRESS OF                             ELTCBL  
00415          GROUP-SPECIFIC-REC.                                      ELTCBL  
00416      IF CIA-RC-PTR-NULL                                           ELTCBL  
00417          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTCBL  
00418      EJECT                                                        ELTCBL  
00419                                                                   ELTCBL  
00420 ************************************************************      ELTCBL  
00421 *                                                          *      ELTCBL  
00422 *        ESTABLISH ADDRESSABILITY OF CST CONTAINMENT PROGRA*      ELTCBL  
00423 *                                                          *      ELTCBL  
00424 ************************************************************      ELTCBL  
00425  ESTABLISH-ADDRESSABILITY-OF-CS.                                  ELTCBL  
00426      SET CIA-GCTABULR-DDN TO TRUE.                                ELTCBL  
00427      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCBL  
00428                            ADDRESS OF                             ELTCBL  
00429          GCCP-TABULAR-REC-AREA.                                   ELTCBL  
00430      IF CIA-RC-PTR-NULL                                           ELTCBL  
00431          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTCBL  
00432      EJECT                                                        ELTCBL  
00433                                                                   ELTCBL  
00434 ************************************************************      ELTCBL  
00435 *                                                          *      ELTCBL  
00436 *        PROCESS                                           *      ELTCBL  
00437 *                                                          *      ELTCBL  
00438 ************************************************************      ELTCBL  
00439  PROCESS.                                                         ELTCBL  
00440      IF GCG-CBL-PARTICIPATION-IND = ZERO                          ELTCBL  
00441          PERFORM GENERATE-NOT-APPLICABLE-MESSAG                   ELTCBL  
00442      ELSE                                                         ELTCBL  
00443          PERFORM GENERATE-CBL-TEXT.                               ELTCBL  
00444      PERFORM TERMINATE-OUTPUT.                                    ELTCBL  
00445                                                                   ELTCBL  
00446 ************************************************************      ELTCBL  
00447 *                                                          *      ELTCBL  
00448 *        EJECT NEW PAGE                                    *      ELTCBL  
00449 *                                                          *      ELTCBL  
00450 ************************************************************      ELTCBL  
00451  EJECT-NEW-PAGE.                                                  ELTCBL  
00452      SET COF-NEW-PAGE      TO TRUE.                               ELTCBL  
00453      MOVE WS-HEADER-LINE   TO COF-HDR-LINE                        ELTCBL  
00454          (COF-NBR-HDR-LINES).                                     ELTCBL  
00455      PERFORM LINK-TO-OUTPUT.                                      ELTCBL  
00456      EJECT                                                        ELTCBL  
00457                                                                   ELTCBL  
00458 ************************************************************      ELTCBL  
00459 *                                                          *      ELTCBL  
00460 *        TRANSLATE AND DISPLAY CBL IND                     *      ELTCBL  
00461 *                                                          *      ELTCBL  
00462 ************************************************************      ELTCBL  
00463  TRANSLATE-AND-DISPLAY-CBL-IND.                                   ELTCBL  
00464      INITIALIZE TCAR-FROM-AREA.                                   ELTCBL  
00465      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCBL  
00466      MOVE WS-CBL-APPLIES TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELTCBL  
00467      ADD  +1 TO TCAR-FROM-SUB.                                    ELTCBL  
00468      SET BLANK-LINE-NEEDED TO TRUE.                               ELTCBL  
00469      SET PERIOD-NEEDED TO TRUE.                                   ELTCBL  
00470      MOVE GCG-CBL-PARTICIPATION-IND TO CMF-CODE-VALUE.            ELTCBL  
00471      MOVE 'CBL-PARTICIPATION-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTCBL  
00472      MOVE WS-GRP TO CMF-RECORD-PREFIX.                            ELTCBL  
00473      EXEC CICS LINK                                               ELTCBL  
00474                PROGRAM ('ELUCMIF')                                ELTCBL  
00475                COMMAREA (DFHCOMMAREA)                             ELTCBL  
00476         END-EXEC.                                                 ELTCBL  
00477      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTCBL  
00478      MOVE SPACE TO ADDITIONAL-TEXT-SWITCH.                        ELTCBL  
00479                                                                   ELTCBL  
00480 ************************************************************      ELTCBL  
00481 *                                                          *      ELTCBL  
00482 *        SET UP OUTPUT SUBSCRIPTS                          *      ELTCBL  
00483 *                                                          *      ELTCBL  
00484 ************************************************************      ELTCBL  
00485  SET-UP-OUTPUT-SUBSCRIPTS.                                        ELTCBL  
00486      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTCBL  
00487      MOVE +2 TO COF-NBR-HDR-LINES.                                ELTCBL  
00488                                                                   ELTCBL  
00489 ************************************************************      ELTCBL  
00490 *                                                          *      ELTCBL  
00491 *        GENERATE NOT APPLICABLE MESSAGE                   *      ELTCBL  
00492 *                                                          *      ELTCBL  
00493 ************************************************************      ELTCBL  
00494  GENERATE-NOT-APPLICABLE-MESSAG.                                  ELTCBL  
00495      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTCBL  
00496      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTCBL  
00497      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTCBL  
00498      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTCBL  
00499          (COF-NBR-DTL-LINES).                                     ELTCBL  
00500      PERFORM EJECT-NEW-PAGE.                                      ELTCBL  
00501      EJECT                                                        ELTCBL  
00502                                                                   ELTCBL  
00503 ************************************************************      ELTCBL  
00504 *                                                          *      ELTCBL  
00505 *        GENERATE CBL TEXT                                 *      ELTCBL  
00506 *                                                          *      ELTCBL  
00507 ************************************************************      ELTCBL  
00508  GENERATE-CBL-TEXT.                                               ELTCBL  
00509      SET WS-POINTER2 TO NULLS.                                    ELTCBL  
00510      SET WS-POINTER3 TO NULLS.                                    ELTCBL  
00511      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTCBL  
00512      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTCBL  
00513                            WS-POINTER2.                           ELTCBL  
00514      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTCBL  
00515      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTCBL  
00516                            WS-POINTER3.                           ELTCBL  
00517      PERFORM SEARCH-FOR-GCCP-TABULAR.                             ELTCBL  
00518      PERFORM DETERMINE-SELECTION.                                 ELTCBL  
00519      EJECT                                                        ELTCBL  
00520                                                                   ELTCBL  
00521 ************************************************************      ELTCBL  
00522 *                                                          *      ELTCBL  
00523 *        TERMINATE OUTPUT                                  *      ELTCBL  
00524 *                                                          *      ELTCBL  
00525 ************************************************************      ELTCBL  
00526  TERMINATE-OUTPUT.                                                ELTCBL  
00527      SET COF-END TO TRUE.                                         ELTCBL  
00528      PERFORM LINK-TO-OUTPUT.                                      ELTCBL  
00529      EJECT                                                        ELTCBL  
00530                                                                   ELTCBL  
00531 ************************************************************      ELTCBL  
00532 *                                                          *      ELTCBL  
00533 *        SEARCH FOR GCCP TABULAR                           *      ELTCBL  
00534 *                                                          *      ELTCBL  
00535 ************************************************************      ELTCBL  
00536  SEARCH-FOR-GCCP-TABULAR.                                         ELTCBL  
00537      SET GCG-INDEX TO +1.                                         ELTCBL  
00538      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTCBL  
00539         AT END                                                    ELTCBL  
00540              MOVE ZEROES TO KWA-PROVISION-SLOT-NO                 ELTCBL  
00541         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GCCP                     ELTCBL  
00542              MOVE GCG-TAB-ID (GCG-INDEX)                          ELTCBL  
00543                  TO KWA-PROVISION-ID                              ELTCBL  
00544              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTCBL  
00545                  TO KWA-PROVISION-SLOT-NO                         ELTCBL  
00546         END-SEARCH.                                               ELTCBL  
00547      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELTCBL  
00548          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTCBL  
00549      PERFORM GET-GCCP-TABULAR.                                    ELTCBL  
00550      PERFORM SEARCH-THE-GSS-ENTRY.                                ELTCBL  
00551      EJECT                                                        ELTCBL  
00552                                                                   ELTCBL  
00553 ************************************************************      ELTCBL  
00554 *                                                          *      ELTCBL  
00555 *        TRANSLATE APPROVAL SOURCE                         *      ELTCBL  
00556 *                                                          *      ELTCBL  
00557 ************************************************************      ELTCBL  
00558  TRANSLATE-APPROVAL-SOURCE.                                       ELTCBL  
00559      INITIALIZE WS-PERIOD-SWITCH.                                 ELTCBL  
00560      INITIALIZE TCAR-FROM-AREA.                                   ELTCBL  
00561 *    MOVE +1 TO TCAR-FROM-SUB.                                    ELTCBL  
00562      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTCBL  
00563 *    MOVE WS-APPRVL-LINE TO TCAR-FROM-LINE                        ELTCBL  
00564 *        (TCAR-FROM-SUB).                                         ELTCBL  
00565      MOVE WS-APPRVL-LINE TO COF-DTL-LINE                          ELTCBL  
00566          (COF-NBR-DTL-LINES).                                     ELTCBL  
00567 *    ADD  +1 TO TCAR-FROM-SUB.                                    ELTCBL  
00568      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTCBL  
00569      SET ADDITIONAL-TEXT TO TRUE.                                 ELTCBL  
00570      IF NOT-HOLDING-APPROVAL-SRCE                                 ELTCBL  
00571          PERFORM GET-APPROVAL-TRANSLATION                         ELTCBL  
00572      ELSE                                                         ELTCBL  
00573          PERFORM USE-EXISTING-APPROVAL-TRANSLAT.                  ELTCBL  
00574 *    PERFORM INITIALIZE-CMOUT.                                    ELTCBL  
00575 *    PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTCBL  
00576      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTCBL  
00577      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTCBL  
00578                            WS-POINTER3.                           ELTCBL  
00579      PERFORM MOVE-TRANS-TO-OUTPUT.                                ELTCBL  
00580      PERFORM LINK-TO-OUTPUT.                                      ELTCBL  
00581 *    MOVE WS-APPROVAL TO TCAR-FROM-LINE                           ELTCBL  
00582 *        (TCAR-FROM-SUB).                                         ELTCBL  
00583 *    PERFORM FINISH-SENTENCE.                                     ELTCBL  
00584      EJECT                                                        ELTCBL  
00585                                                                   ELTCBL  
00586                                                                   ELTCBL  
00587 ************************************************************      ELTCBL  
00588 *                                                          *      ELTCBL  
00589 *        GET APPROVAL TRANSLATION                          *      ELTCBL  
00590 *                                                          *      ELTCBL  
00591 ************************************************************      ELTCBL  
00592  GET-APPROVAL-TRANSLATION.                                        ELTCBL  
00593      MOVE GSS-CB-APPROVAL-SOURCE-IND (GSS-INDEX)                  ELTCBL  
00594                      TO CMF-CODE-VALUE.                           ELTCBL  
00595      MOVE   'CB-APPROVAL-SOURCE-IND'                              ELTCBL  
00596                    TO CMF-ELEMENT-SYSTEM-NAME.                    ELTCBL  
00597      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTCBL  
00598      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTCBL  
00599      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTCBL  
00600                            ADDRESS OF CMF-DESCR.                  ELTCBL  
00601      EJECT                                                        ELTCBL  
00602                                                                   ELTCBL  
00603 ************************************************************      ELTCBL  
00604 *                                                          *      ELTCBL  
00605 *        USE EXISTING APPROVAL TRANSLATION                 *      ELTCBL  
00606 *                                                          *      ELTCBL  
00607 ************************************************************      ELTCBL  
00608  USE-EXISTING-APPROVAL-TRANSLAT.                                  ELTCBL  
00609      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTCBL  
00610      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTCBL  
00611                            ADDRESS OF CMF-DESCR.                  ELTCBL  
00612      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTCBL  
00613      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTCBL  
00614                            WS-POINTER2.                           ELTCBL  
00615      EJECT                                                        ELTCBL  
00616                                                                   ELTCBL  
00617 ************************************************************      ELTCBL  
00618 *                                                          *      ELTCBL  
00619 *        FINISH SENTENCE                                   *      ELTCBL  
00620 *                                                          *      ELTCBL  
00621 ************************************************************      ELTCBL  
00622  FINISH-SENTENCE.                                                 ELTCBL  
00623      SET BLANK-LINE-NEEDED TO TRUE.                               ELTCBL  
00624      PERFORM REFORMAT-AND-WRITE-TEXT.                             ELTCBL  
00625                                                                   ELTCBL  
00626 ************************************************************      ELTCBL  
00627 *                                                          *      ELTCBL  
00628 *        TRANSLATE AND DISPLAY CODE VALUE                  *      ELTCBL  
00629 *                                                          *      ELTCBL  
00630 ************************************************************      ELTCBL  
00631  TRANSLATE-AND-DISPLAY-CODE-VAL.                                  ELTCBL  
00632      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTCBL  
00633      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTCBL  
00634                                                                   ELTCBL  
00635 ************************************************************      ELTCBL  
00636 *                                                          *      ELTCBL  
00637 *        SIGNAL UNDEFINED TABULAR ERROR                    *      ELTCBL  
00638 *                                                          *      ELTCBL  
00639 ************************************************************      ELTCBL  
00640  SIGNAL-UNDEFINED-TABULAR-ERROR.                                  ELTCBL  
00641      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTCBL  
00642      PERFORM SIGNAL-ABEND.                                        ELTCBL  
00643      EJECT                                                        ELTCBL  
00644                                                                   ELTCBL  
00645 ************************************************************      ELTCBL  
00646 *                                                          *      ELTCBL  
00647 *        DETERMINE SELECTION                               *      ELTCBL  
00648 *                                                          *      ELTCBL  
00649 ************************************************************      ELTCBL  
00650  DETERMINE-SELECTION.                                             ELTCBL  
00651      SET NOT-HOLDING-APPROVAL-SRCE TO TRUE.                       ELTCBL  
00652      IF SSB-PROV-CLASS-INST OR                                    ELTCBL  
00653                   SSB-PROV-CLASS-BOTH                             ELTCBL  
00654          PERFORM CREATE-INSTITUTIONAL-SCREEN.                     ELTCBL  
00655      IF SSB-PROV-CLASS-PROF OR                                    ELTCBL  
00656                   SSB-PROV-CLASS-BOTH                             ELTCBL  
00657          PERFORM CREATE-PROFESSIONAL-SCREEN.                      ELTCBL  
00658      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL '03' OR '04' OR       ELTCBL  
00659                    '06' OR '08')                                  ELTCBL  
00660          PERFORM CREATE-SUPPLEMENTAL-SCREEN.                      ELTCBL  
00661                                                                   ELTCBL  
00662 ************************************************************      ELTCBL  
00663 *                                                          *      ELTCBL  
00664 *        CREATE INSTITUTIONAL SCREEN                       *      ELTCBL  
00665 *                                                          *      ELTCBL  
00666 ************************************************************      ELTCBL  
00667  CREATE-INSTITUTIONAL-SCREEN.                                     ELTCBL  
00668      MOVE WS-INST TO WS-HDR-LINE-BCBSMM.                          ELTCBL  
00669      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTCBL  
00670      IF GSS-CB-BC-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTCBL  
00671          ZEROES                                                   ELTCBL  
00672                 AND LOW-VALUES                                    ELTCBL  
00673          PERFORM GENERATE-INSTITUTIONAL-TEXT                      ELTCBL  
00674      ELSE                                                         ELTCBL  
00675          PERFORM GENERATE-INSTITUTIONAL-NOT-APP.                  ELTCBL  
00676      EJECT                                                        ELTCBL  
00677                                                                   ELTCBL  
00678 ************************************************************      ELTCBL  
00679 *                                                          *      ELTCBL  
00680 *        GENERATE INSTITUTIONAL TEXT                       *      ELTCBL  
00681 *                                                          *      ELTCBL  
00682 ************************************************************      ELTCBL  
00683  GENERATE-INSTITUTIONAL-TEXT.                                     ELTCBL  
00684      PERFORM EJECT-NEW-PAGE.                                      ELTCBL  
00685      PERFORM TRANSLATE-AND-DISPLAY-CBL-IND.                       ELTCBL  
00686      IF GSS-CB-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL SPACES   ELTCBL  
00687          AND                                                      ELTCBL  
00688                 ZEROES AND LOW-VALUES                             ELTCBL  
00689          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTCBL  
00690      PERFORM TRANSLATE-BC-IND.                                    ELTCBL  
00691      IF GSS-CB-BC-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL SPACES  ELTCBL  
00692           AND ZEROES AND LOW-VALUES                               ELTCBL  
00693            PERFORM TRANSLATE-BC-PAYMENT-LEVEL-IND.                ELTCBL  
00694      IF GSS-CB-BC-ALT-PRICING-METHOD (GSS-INDEX) NOT EQUAL        ELTCBL  
00695          SPACES AND                                               ELTCBL  
00696                 ZEROES AND LOW-VALUES                             ELTCBL  
00697          PERFORM TRANSLATE-BC-ALT-PRIC.                           ELTCBL  
00698      SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTCBL  
00699      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTCBL  
00700      IF GSS-CB-BC-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES AND    ELTCBL  
00701          ZEROES                                                   ELTCBL  
00702                 AND LOW-VALUES                                    ELTCBL  
00703          PERFORM TRANSLATE-BC-CALC-METHOD.                        ELTCBL  
00704      IF GSS-CB-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTCBL  
00705          SPACES AND                                               ELTCBL  
00706                 ZEROES AND LOW-VALUES                             ELTCBL  
00707          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTCBL  
00708      IF GSS-CB-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL SPACES AND    ELTCBL  
00709          ZEROES                                                   ELTCBL  
00710                 AND LOW-VALUES                                    ELTCBL  
00711          PERFORM GENERATE-SPILL-OVER-TEXT.                        ELTCBL  
00712      PERFORM GENERATE-SPECIAL-PROVIDERS-SEN.                      ELTCBL  
00713      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTCBL  
00714      EJECT                                                        ELTCBL  
00715                                                                   ELTCBL  
00716 ************************************************************      ELTCBL  
00717 *                                                          *      ELTCBL  
00718 *        GENERATE INSTITUTIONAL NOT APPLICABLE             *      ELTCBL  
00719 *                                                          *      ELTCBL  
00720 ************************************************************      ELTCBL  
00721  GENERATE-INSTITUTIONAL-NOT-APP.                                  ELTCBL  
00722      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTCBL  
00723      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTCBL  
00724      MOVE WS-NOT-APPLY-TO-LOB-BC TO COF-DTL-LINE                  ELTCBL  
00725          (COF-NBR-DTL-LINES).                                     ELTCBL  
00726      PERFORM EJECT-NEW-PAGE.                                      ELTCBL  
00727      EJECT                                                        ELTCBL  
00728                                                                   ELTCBL  
00729                                                                   ELTCBL  
00730 ************************************************************      ELTCBL  
00731 *                                                          *      ELTCBL  
00732 *        GENERATE DISCLAIMER MESSAGE                       *      ELTCBL  
00733 *                                                          *      ELTCBL  
00734 ************************************************************      ELTCBL  
00735  GENERATE-DISCLAIMER-MESSAGE.                                     ELTCBL  
00736      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTCBL  
00737      MOVE WS-DISCLAIMER-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).  ELTCBL  
00738      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTCBL  
00739      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTCBL  
00740      PERFORM LINK-TO-OUTPUT.                                      ELTCBL  
00741      EJECT                                                        ELTCBL  
00742                                                                   ELTCBL  
00743 ************************************************************      ELTCBL  
00744 *                                                          *      ELTCBL  
00745 *        TRANSLATE BC IND                                  *      ELTCBL  
00746 *                                                          *      ELTCBL  
00747 ************************************************************      ELTCBL  
00748  TRANSLATE-BC-IND.                                                ELTCBL  
00749      INITIALIZE TCAR-FROM-AREA.                                   ELTCBL  
00750      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCBL  
00751      SET BLANK-LINE-NEEDED TO TRUE.                               ELTCBL  
00752      SET PERIOD-NEEDED TO TRUE.                                   ELTCBL  
00753      MOVE GSS-CB-BC-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTCBL  
00754      MOVE   'CB-BC-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTCBL  
00755      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTCBL  
00756      PERFORM INITIALIZE-CMOUT.                                    ELTCBL  
00757      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELTCBL  
00758      PERFORM MOVE-TRANS-TO-OUTPUT.                                ELTCBL  
00759      PERFORM LINK-TO-OUTPUT.                                      ELTCBL  
00760      EJECT                                                        ELTCBL  
00761                                                                   ELTCBL  
00762 ************************************************************      ELTCBL  
00763 *                                                          *      ELTCBL  
00764 *        TRANSLATE BC PAYMENT LEVEL INDICATOR              *      ELTCBL  
00765 *                                                          *      ELTCBL  
00766 ************************************************************      ELTCBL  
00767  TRANSLATE-BC-PAYMENT-LEVEL-IND.                                  ELTCBL  
00768      INITIALIZE TCAR-FROM-AREA.                                   ELTCBL  
00769      MOVE 1 TO TCAR-FROM-SUB.                                     ELTCBL  
00770      MOVE 'CB-BC-PAYMENT-LEVEL-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTCBL  
00771      MOVE GSS-CB-BC-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTCBL  
00772         CMF-CODE-VALUE.                                           ELTCBL  
00773      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTCBL  
00774      PERFORM INITIALIZE-CMOUT.                                    ELTCBL  
00775      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELTCBL  
00776      PERFORM MOVE-TRANS-TO-OUTPUT.                                ELTCBL  
00777      PERFORM LINK-TO-OUTPUT.                                      ELTCBL  
00778 *    PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTCBL  
00779                                                                   ELTCBL  
00780 ************************************************************      ELTCBL  
00781 *                                                          *      ELTCBL  
00782 *        TRANSLATE BC ALT PRIC                             *      ELTCBL  
00783 *                                                          *      ELTCBL  
00784 ************************************************************      ELTCBL  
00785  TRANSLATE-BC-ALT-PRIC.                                           ELTCBL  
00786      INITIALIZE TCAR-FROM-AREA.                                   ELTCBL  
00787      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCBL  
00788      MOVE WS-ALT-PRICING-LINE-BC TO TCAR-FROM-LINE                ELTCBL  
00789          (TCAR-FROM-SUB).                                         ELTCBL  
00790      ADD  +1 TO TCAR-FROM-SUB.                                    ELTCBL  
00791      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTCBL  
00792      SET PERIOD-NEEDED TO TRUE.                                   ELTCBL  
00793      MOVE GSS-CB-BC-ALT-PRICING-METHOD (GSS-INDEX) TO             ELTCBL  
00794          CMF-CODE-VALUE.                                          ELTCBL  
00795      MOVE  'CB-BC-ALT-PRICING-METHOD' TO CMF-ELEMENT-SYSTEM-NAME. ELTCBL  
00796      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTCBL  
00797      EJECT                                                        ELTCBL  
00798                                                                   ELTCBL  
00799 ************************************************************      ELTCBL  
00800 *                                                          *      ELTCBL  
00801 *        TRANSLATE BC CALC METHOD                          *      ELTCBL  
00802 *                                                          *      ELTCBL  
00803 ************************************************************      ELTCBL  
00804  TRANSLATE-BC-CALC-METHOD.                                        ELTCBL  
00805      INITIALIZE TCAR-FROM-AREA.                                   ELTCBL  
00806      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCBL  
00807      SET BLANK-LINE-NEEDED TO TRUE.                               ELTCBL  
00808      SET PERIOD-NEEDED TO TRUE.                                   ELTCBL  
00809      MOVE GSS-CB-BC-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTCBL  
00810      MOVE   'CB-BC-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTCBL  
00811      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTCBL  
00812      EJECT                                                        ELTCBL  
00813                                                                   ELTCBL  
00814 ************************************************************      ELTCBL  
00815 *                                                          *      ELTCBL  
00816 *        GENERATE ACCUM TABULAR DATA                       *      ELTCBL  
00817 *                                                          *      ELTCBL  
00818 ************************************************************      ELTCBL  
00819  GENERATE-ACCUM-TABULAR-DATA.                                     ELTCBL  
00820      PERFORM GENERATE-COINSURANCE.                                ELTCBL  
00821      PERFORM GENERATE-COPAY.                                      ELTCBL  
00822      PERFORM GENERATE-DEDUCTIBLE.                                 ELTCBL  
00823      PERFORM GENERATE-MAXIMUM.                                    ELTCBL  
00824      PERFORM GENERATE-OUT-OF-POCKET.                              ELTCBL  
00825      EJECT                                                        ELTCBL  
00826                                                                   ELTCBL  
00827 ************************************************************      ELTCBL  
00828 *                                                          *      ELTCBL  
00829 *        GENERATE COMBINED BENEFITS REDUCTION TEXT         *      ELTCBL  
00830 *                                                          *      ELTCBL  
00831 ************************************************************      ELTCBL  
00832  GENERATE-COMBINED-BENEFITS-RED.                                  ELTCBL  
00833      MOVE 'CB' TO SRP-COST-CONT-TYPE.                             ELTCBL  
00834      MOVE 'COMMUNITY BLUE OPTION PROGRAM' TO SRP-CCP-NAME.        ELTCBL  
00835      MOVE GSS-CB-COMB-BENE-REDUCT-IND (GSS-INDEX)                 ELTCBL  
00836                  TO SRP-CCP-COMB-BENE-REDUCT-IND.                 ELTCBL  
00837      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELTCBL  
00838      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTCBL  
00839                            ADDRESS OF                             ELTCBL  
00840          GCCP-TABULAR-REC-AREA.                                   ELTCBL  
00841      PERFORM CALL-CBRI-INTERFACE.                                 ELTCBL  
00842      EJECT                                                        ELTCBL  
00843                                                                   ELTCBL  
00844 ************************************************************      ELTCBL  
00845 *                                                          *      ELTCBL  
00846 *        CREATE PROFESSIONAL SCREEN                        *      ELTCBL  
00847 *                                                          *      ELTCBL  
00848 ************************************************************      ELTCBL  
00849  CREATE-PROFESSIONAL-SCREEN.                                      ELTCBL  
00850      MOVE WS-PROF TO WS-HDR-LINE-BCBSMM.                          ELTCBL  
00851      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTCBL  
00852      IF GSS-CB-BS-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTCBL  
00853          ZEROES                                                   ELTCBL  
00854                 AND LOW-VALUES                                    ELTCBL  
00855          PERFORM GENERATE-PROFESSIONAL-TEXT                       ELTCBL  
00856      ELSE                                                         ELTCBL  
00857          PERFORM GENERATE-PROFESSIONAL-NOT-APPL.                  ELTCBL  
00858      EJECT                                                        ELTCBL  
00859                                                                   ELTCBL  
00860 ************************************************************      ELTCBL  
00861 *                                                          *      ELTCBL  
00862 *        GENERATE PROFESSIONAL TEXT                        *      ELTCBL  
00863 *                                                          *      ELTCBL  
00864 ************************************************************      ELTCBL  
00865  GENERATE-PROFESSIONAL-TEXT.                                      ELTCBL  
00866      PERFORM EJECT-NEW-PAGE.                                      ELTCBL  
00867      PERFORM TRANSLATE-AND-DISPLAY-CBL-IND.                       ELTCBL  
00868      IF GSS-CB-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTCBL  
00869          SPACES                                                   ELTCBL  
00870                 AND ZEROES AND LOW-VALUES                         ELTCBL  
00871          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTCBL  
00872      PERFORM TRANSLATE-BS-IND.                                    ELTCBL  
00873      IF GSS-CB-BS-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL SPACES  ELTCBL  
00874            AND ZEROES AND LOW-VALUES                              ELTCBL  
00875            PERFORM TRANSLATE-BS-PAYMENT-LEVEL-IND.                ELTCBL  
00876      IF GSS-CB-BS-ALT-PRICING-METHOD (GSS-INDEX) NOT EQUAL        ELTCBL  
00877          SPACES AND                                               ELTCBL  
00878                 ZEROES AND LOW-VALUES                             ELTCBL  
00879          PERFORM TRANSLATE-BS-ALT-PRIC.                           ELTCBL  
00880      SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTCBL  
00881      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTCBL  
00882      IF GSS-CB-BS-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES AND    ELTCBL  
00883          ZEROES                                                   ELTCBL  
00884                 AND LOW-VALUES                                    ELTCBL  
00885          PERFORM TRANSLATE-BS-CALC-METHOD.                        ELTCBL  
00886      IF GSS-CB-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTCBL  
00887          SPACES                                                   ELTCBL  
00888                 AND ZEROES AND LOW-VALUES                         ELTCBL  
00889          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTCBL  
00890      IF GSS-CB-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL SPACES AND    ELTCBL  
00891          ZEROES                                                   ELTCBL  
00892                 AND LOW-VALUES                                    ELTCBL  
00893          PERFORM GENERATE-SPILL-OVER-TEXT.                        ELTCBL  
00894      PERFORM GENERATE-SPECIAL-PROVIDERS-SEN.                      ELTCBL  
00895      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTCBL  
00896                                                                   ELTCBL  
00897 ************************************************************      ELTCBL  
00898 *                                                          *      ELTCBL  
00899 *        GENERATE PROFESSIONAL NOT APPLICABLE              *      ELTCBL  
00900 *                                                          *      ELTCBL  
00901 ************************************************************      ELTCBL  
00902  GENERATE-PROFESSIONAL-NOT-APPL.                                  ELTCBL  
00903      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTCBL  
00904      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTCBL  
00905      MOVE WS-NOT-APPLY-TO-LOB-BS TO COF-DTL-LINE                  ELTCBL  
00906          (COF-NBR-DTL-LINES).                                     ELTCBL  
00907      PERFORM EJECT-NEW-PAGE.                                      ELTCBL  
00908      EJECT                                                        ELTCBL  
00909                                                                   ELTCBL  
00910 ************************************************************      ELTCBL  
00911 *                                                          *      ELTCBL  
00912 *        TRANSLATE BS IND                                  *      ELTCBL  
00913 *                                                          *      ELTCBL  
00914 ************************************************************      ELTCBL  
00915  TRANSLATE-BS-IND.                                                ELTCBL  
00916      INITIALIZE TCAR-FROM-AREA.                                   ELTCBL  
00917      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCBL  
00918      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTCBL  
00919      SET PERIOD-NEEDED TO TRUE.                                   ELTCBL  
00920      MOVE GSS-CB-BS-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTCBL  
00921      MOVE   'CB-BS-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTCBL  
00922      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTCBL  
00923      PERFORM INITIALIZE-CMOUT.                                    ELTCBL  
00924      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELTCBL  
00925      PERFORM MOVE-TRANS-TO-OUTPUT.                                ELTCBL  
00926      PERFORM LINK-TO-OUTPUT.                                      ELTCBL  
00927 *    PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTCBL  
00928      EJECT                                                        ELTCBL  
00929                                                                   ELTCBL  
00930 ************************************************************      ELTCBL  
00931 *                                                          *      ELTCBL  
00932 *        TRANSLATE BS PAYMENT LEVEL INDICATOR              *      ELTCBL  
00933 *                                                          *      ELTCBL  
00934 ************************************************************      ELTCBL  
00935  TRANSLATE-BS-PAYMENT-LEVEL-IND.                                  ELTCBL  
00936      MOVE 'CB-BS-PAYMENT-LEVEL-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTCBL  
00937      MOVE GSS-CB-BS-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTCBL  
00938         CMF-CODE-VALUE.                                           ELTCBL  
00939      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTCBL  
00940      PERFORM INITIALIZE-CMOUT.                                    ELTCBL  
00941      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELTCBL  
00942      PERFORM MOVE-TRANS-TO-OUTPUT.                                ELTCBL  
00943      PERFORM LINK-TO-OUTPUT.                                      ELTCBL  
00944 *    PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTCBL  
00945                                                                   ELTCBL  
00946 ************************************************************      ELTCBL  
00947 *                                                          *      ELTCBL  
00948 *        TRANSLATE BS CALC METHOD                          *      ELTCBL  
00949 *                                                          *      ELTCBL  
00950 ************************************************************      ELTCBL  
00951  TRANSLATE-BS-CALC-METHOD.                                        ELTCBL  
00952      INITIALIZE TCAR-FROM-AREA.                                   ELTCBL  
00953      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCBL  
00954      SET BLANK-LINE-NEEDED TO TRUE.                               ELTCBL  
00955      SET PERIOD-NEEDED TO TRUE.                                   ELTCBL  
00956      MOVE GSS-CB-BS-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTCBL  
00957      MOVE   'CB-BS-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTCBL  
00958      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTCBL  
00959      EJECT                                                        ELTCBL  
00960                                                                   ELTCBL  
00961 ************************************************************      ELTCBL  
00962 *                                                          *      ELTCBL  
00963 *        TRANSLATE BS ALT PRIC                             *      ELTCBL  
00964 *                                                          *      ELTCBL  
00965 ************************************************************      ELTCBL  
00966  TRANSLATE-BS-ALT-PRIC.                                           ELTCBL  
00967      INITIALIZE TCAR-FROM-AREA.                                   ELTCBL  
00968      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCBL  
00969      MOVE WS-ALT-PRICING-LINE-BS TO TCAR-FROM-LINE                ELTCBL  
00970          (TCAR-FROM-SUB).                                         ELTCBL  
00971      ADD  +1 TO TCAR-FROM-SUB.                                    ELTCBL  
00972      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTCBL  
00973      SET PERIOD-NEEDED TO TRUE.                                   ELTCBL  
00974      MOVE GSS-CB-BS-ALT-PRICING-METHOD (GSS-INDEX) TO             ELTCBL  
00975          CMF-CODE-VALUE.                                          ELTCBL  
00976      MOVE  'CB-BS-ALT-PRICING-METHOD' TO CMF-ELEMENT-SYSTEM-NAME. ELTCBL  
00977      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTCBL  
00978      EJECT                                                        ELTCBL  
00979                                                                   ELTCBL  
00980 ************************************************************      ELTCBL  
00981 *                                                          *      ELTCBL  
00982 *        CREATE SUPPLEMENTAL SCREEN                        *      ELTCBL  
00983 *                                                          *      ELTCBL  
00984 ************************************************************      ELTCBL  
00985  CREATE-SUPPLEMENTAL-SCREEN.                                      ELTCBL  
00986      MOVE WS-SUPP TO WS-HDR-LINE-BCBSMM.                          ELTCBL  
00987      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTCBL  
00988      IF GSS-CB-MM-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTCBL  
00989          ZEROES                                                   ELTCBL  
00990                 AND LOW-VALUES                                    ELTCBL  
00991          PERFORM GENERATE-SUPPLEMENTAL-TEXT                       ELTCBL  
00992      ELSE                                                         ELTCBL  
00993          PERFORM GENERATE-SUPPLEMENTAL-NOT-APPL.                  ELTCBL  
00994      EJECT                                                        ELTCBL  
00995                                                                   ELTCBL  
00996 ************************************************************      ELTCBL  
00997 *                                                          *      ELTCBL  
00998 *        GENERATE SUPPLEMENTAL TEXT                        *      ELTCBL  
00999 *                                                          *      ELTCBL  
01000 ************************************************************      ELTCBL  
01001  GENERATE-SUPPLEMENTAL-TEXT.                                      ELTCBL  
01002      PERFORM EJECT-NEW-PAGE.                                      ELTCBL  
01003      PERFORM TRANSLATE-AND-DISPLAY-CBL-IND.                       ELTCBL  
01004      IF GSS-CB-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTCBL  
01005          SPACES                                                   ELTCBL  
01006                 AND ZEROES AND LOW-VALUES                         ELTCBL  
01007          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTCBL  
01008      PERFORM TRANSLATE-MM-IND.                                    ELTCBL  
01009      IF GSS-CB-MM-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL SPACES  ELTCBL  
01010         AND SPACES AND LOW-VALUES                                 ELTCBL  
01011            PERFORM TRANSLATE-MM-PAYMENT-LEVEL-IND.                ELTCBL  
01012      IF GSS-CB-MM-ALT-PRICING-METHOD (GSS-INDEX) NOT EQUAL        ELTCBL  
01013          SPACES AND                                               ELTCBL  
01014                 ZEROES AND LOW-VALUES                             ELTCBL  
01015          PERFORM TRANSLATE-MM-ALT-PRIC.                           ELTCBL  
01016      SET SRP-ACCUM-PROV-CLASS-SUPP TO TRUE.                       ELTCBL  
01017      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTCBL  
01018      IF GSS-CB-MM-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES AND    ELTCBL  
01019          ZEROES                                                   ELTCBL  
01020                 AND LOW-VALUES                                    ELTCBL  
01021          PERFORM TRANSLATE-MM-CALC-METHOD.                        ELTCBL  
01022      IF GSS-CB-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTCBL  
01023          SPACES                                                   ELTCBL  
01024                 AND ZEROES AND LOW-VALUES                         ELTCBL  
01025          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTCBL  
01026      PERFORM GENERATE-SPECIAL-PROVIDERS-SEN.                      ELTCBL  
01027      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTCBL  
01028                                                                   ELTCBL  
01029                                                                   ELTCBL  
01030 ************************************************************      ELTCBL  
01031 *                                                          *      ELTCBL  
01032 *        GENERATE SUPPLEMENTAL NOT APPLICABLE              *      ELTCBL  
01033 *                                                          *      ELTCBL  
01034 ************************************************************      ELTCBL  
01035  GENERATE-SUPPLEMENTAL-NOT-APPL.                                  ELTCBL  
01036      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTCBL  
01037      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTCBL  
01038      MOVE WS-NOT-APPLY-TO-LOB-MM TO COF-DTL-LINE                  ELTCBL  
01039          (COF-NBR-DTL-LINES).                                     ELTCBL  
01040      PERFORM EJECT-NEW-PAGE.                                      ELTCBL  
01041      EJECT                                                        ELTCBL  
01042                                                                   ELTCBL  
01043 ************************************************************      ELTCBL  
01044 *                                                          *      ELTCBL  
01045 *        TRANSLATE MM IND                                  *      ELTCBL  
01046 *                                                          *      ELTCBL  
01047 ************************************************************      ELTCBL  
01048  TRANSLATE-MM-IND.                                                ELTCBL  
01049      INITIALIZE TCAR-FROM-AREA.                                   ELTCBL  
01050      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCBL  
01051      SET BLANK-LINE-NEEDED TO TRUE.                               ELTCBL  
01052      SET PERIOD-NEEDED TO TRUE.                                   ELTCBL  
01053      MOVE GSS-CB-MM-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTCBL  
01054      MOVE   'CB-MM-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTCBL  
01055      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTCBL  
01056      PERFORM INITIALIZE-CMOUT.                                    ELTCBL  
01057      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELTCBL  
01058      PERFORM MOVE-TRANS-TO-OUTPUT.                                ELTCBL  
01059      PERFORM LINK-TO-OUTPUT.                                      ELTCBL  
01060 *    PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTCBL  
01061      EJECT                                                        ELTCBL  
01062                                                                   ELTCBL  
01063 ************************************************************      ELTCBL  
01064 *                                                          *      ELTCBL  
01065 *        TRANSLATE MM PAYMENT LEVEL INDICATOR              *      ELTCBL  
01066 *                                                          *      ELTCBL  
01067 ************************************************************      ELTCBL  
01068  TRANSLATE-MM-PAYMENT-LEVEL-IND.                                  ELTCBL  
01069      MOVE 'CB-MM-PAYMENT-LEVEL-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTCBL  
01070      MOVE GSS-CB-MM-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTCBL  
01071         CMF-CODE-VALUE.                                           ELTCBL  
01072      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTCBL  
01073      PERFORM INITIALIZE-CMOUT.                                    ELTCBL  
01074      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELTCBL  
01075      PERFORM MOVE-TRANS-TO-OUTPUT.                                ELTCBL  
01076      PERFORM LINK-TO-OUTPUT.                                      ELTCBL  
01077 *    PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTCBL  
01078                                                                   ELTCBL  
01079 ************************************************************      ELTCBL  
01080 *                                                          *      ELTCBL  
01081 *        TRANSLATE MM ALT PRIC                             *      ELTCBL  
01082 *                                                          *      ELTCBL  
01083 ************************************************************      ELTCBL  
01084  TRANSLATE-MM-ALT-PRIC.                                           ELTCBL  
01085      INITIALIZE TCAR-FROM-AREA.                                   ELTCBL  
01086      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCBL  
01087      MOVE WS-ALT-PRICING-LINE-MM TO TCAR-FROM-LINE                ELTCBL  
01088          (TCAR-FROM-SUB).                                         ELTCBL  
01089      ADD  +1 TO TCAR-FROM-SUB.                                    ELTCBL  
01090      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTCBL  
01091      SET PERIOD-NEEDED TO TRUE.                                   ELTCBL  
01092      MOVE GSS-CB-MM-ALT-PRICING-METHOD (GSS-INDEX) TO             ELTCBL  
01093          CMF-CODE-VALUE.                                          ELTCBL  
01094      MOVE  'CB-MM-ALT-PRICING-METHOD' TO CMF-ELEMENT-SYSTEM-NAME. ELTCBL  
01095      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTCBL  
01096      EJECT                                                        ELTCBL  
01097                                                                   ELTCBL  
01098 ************************************************************      ELTCBL  
01099 *                                                          *      ELTCBL  
01100 *        TRANSLATE MM CALC METHOD                          *      ELTCBL  
01101 *                                                          *      ELTCBL  
01102 ************************************************************      ELTCBL  
01103  TRANSLATE-MM-CALC-METHOD.                                        ELTCBL  
01104      INITIALIZE TCAR-FROM-AREA.                                   ELTCBL  
01105      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCBL  
01106      SET BLANK-LINE-NEEDED TO TRUE.                               ELTCBL  
01107      SET PERIOD-NEEDED TO TRUE.                                   ELTCBL  
01108      MOVE GSS-CB-MM-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTCBL  
01109      MOVE   'CB-MM-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTCBL  
01110      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTCBL  
01111      EJECT                                                        ELTCBL  
01112                                                                   ELTCBL  
01113                                                                   ELTCBL  
01114 ************************************************************      ELTCBL  
01115 *                                                          *      ELTCBL  
01116 *        GENERATE SPILL OVER TEXT                          *      ELTCBL  
01117 *                                                          *      ELTCBL  
01118 ************************************************************      ELTCBL  
01119  GENERATE-SPILL-OVER-TEXT.                                        ELTCBL  
01120      INITIALIZE TCAR-FROM-AREA.                                   ELTCBL  
01121      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCBL  
01122      MOVE WS-SPILL-OVER-LINE TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELTCBL  
01123      ADD  +1 TO TCAR-FROM-SUB.                                    ELTCBL  
01124      SET BLANK-LINE-NEEDED TO TRUE.                               ELTCBL  
01125      SET PERIOD-NEEDED TO TRUE.                                   ELTCBL  
01126      MOVE GSS-CB-SPILL-OVER-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTCBL  
01127      MOVE 'CB-SPILL-OVER-IND' TO CMF-ELEMENT-SYSTEM-NAME.         ELTCBL  
01128      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTCBL  
01129      EJECT                                                        ELTCBL  
01130                                                                   ELTCBL  
01131 ************************************************************      ELTCBL  
01132 *                                                          *      ELTCBL  
01133 *        GENERATE SPECIAL PROVIDERS SENTENCE               *      ELTCBL  
01134 *                                                          *      ELTCBL  
01135 ************************************************************      ELTCBL  
01136  GENERATE-SPECIAL-PROVIDERS-SEN.                                  ELTCBL  
01137      SET GCG-INDEX TO +1.                                         ELTCBL  
01138      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTCBL  
01139         AT END                                                    ELTCBL  
01140              MOVE ZEROES TO WS-GCBL-PROV-SLOT-NO                  ELTCBL  
01141         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GCBL                     ELTCBL  
01142              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTCBL  
01143                  TO WS-GCBL-PROV-SLOT-NO                          ELTCBL  
01144         END-SEARCH.                                               ELTCBL  
01145      IF WS-GCBL-PROV-SLOT-NO NOT EQUAL ZEROES                     ELTCBL  
01146          PERFORM DISPLAY-SPECIAL-PROVIDERS-SENT                   ELTCBL  
01147          PERFORM GENERATE-SPECIAL-PROVIDERS-TEX.                  ELTCBL  
01148                                                                   ELTCBL  
01149 ************************************************************      ELTCBL  
01150 *                                                          *      ELTCBL  
01151 *        DISPLAY SPECIAL PROVIDERS SENTENCE                *      ELTCBL  
01152 *                                                          *      ELTCBL  
01153 ************************************************************      ELTCBL  
01154  DISPLAY-SPECIAL-PROVIDERS-SENT.                                  ELTCBL  
01155      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTCBL  
01156      MOVE WS-SPEC-PROV-MSG  TO COF-DTL-LINE                       ELTCBL  
01157          (COF-NBR-DTL-LINES).                                     ELTCBL  
01158      PERFORM LINK-TO-OUTPUT.                                      ELTCBL  
01159      EJECT                                                        ELTCBL  
01160                                                                   ELTCBL  
01161 ************************************************************      ELTCBL  
01162 *                                                          *      ELTCBL  
01163 *        SEARCH THE GSS ENTRY                              *      ELTCBL  
01164 *                                                          *      ELTCBL  
01165 ************************************************************      ELTCBL  
01166  SEARCH-THE-GSS-ENTRY.                                            ELTCBL  
01167      SET GSS-INDEX TO 1.                                          ELTCBL  
01168      SEARCH GSS-ENTRY                                             ELTCBL  
01169          AT END                                                   ELTCBL  
01170               SET TABULAR-IS-UNDEFINED TO TRUE                    ELTCBL  
01171          WHEN GSS-CB-PROG-CODE-CHR (GSS-INDEX)                    ELTCBL  
01172               CONTINUE                                            ELTCBL  
01173         END-SEARCH.                                               ELTCBL  
01174      IF TABULAR-IS-UNDEFINED                                      ELTCBL  
01175          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTCBL  
01176                                                                   ELTCBL  
01177 ************************************************************      ELTCBL  
01178 *                                                          *      ELTCBL  
01179 *        CALL CODES MANUAL INTERFACE                       *      ELTCBL  
01180 *                                                          *      ELTCBL  
01181 ************************************************************      ELTCBL  
01182  CALL-CODES-MANUAL-INTERFACE.                                     ELTCBL  
01183      MOVE WS-GCCP TO CMF-RECORD-PREFIX.                           ELTCBL  
01184      EXEC CICS LINK                                               ELTCBL  
01185                PROGRAM ('ELUCMIF')                                ELTCBL  
01186                COMMAREA (DFHCOMMAREA)                             ELTCBL  
01187        END-EXEC.                                                  ELTCBL  
01188      EJECT                                                        ELTCBL  
01189                                                                   ELTCBL  
01190 ************************************************************      ELTCBL  
01191 *                                                          *      ELTCBL  
01192 *        GET GCCP TABULAR                                  *      ELTCBL  
01193 *                                                          *      ELTCBL  
01194 ************************************************************      ELTCBL  
01195  GET-GCCP-TABULAR.                                                ELTCBL  
01196      PERFORM ESTABLISH-ADDRESSABILITY-OF-GC.                      ELTCBL  
01197      SET IOP-RD              TO TRUE.                             ELTCBL  
01198      SET IOP-STG-MODE-MOVE   TO TRUE.                             ELTCBL  
01199      SET IOP-FCQ-NONE        TO TRUE.                             ELTCBL  
01200      SET IOP-KVQ-EQ          TO TRUE.                             ELTCBL  
01201      MOVE KWA-GCTABULR-KEY   TO IOP-FILE-KEY.                     ELTCBL  
01202      PERFORM CALL-INPUT-OUTPUT-MODULE.                            ELTCBL  
01203      IF IOP-RC-OK                                                 ELTCBL  
01204          PERFORM ESTABLISH-ADDRESSY-OF-GCCP-TAB                   ELTCBL  
01205      ELSE IF IOP-RC-NOTFND                                        ELTCBL  
01206          PERFORM SIGNAL-NOT-FOUND-GCTAB-ERROR                     ELTCBL  
01207      ELSE                                                         ELTCBL  
01208          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTCBL  
01209      EJECT                                                        ELTCBL  
01210                                                                   ELTCBL  
01211 ************************************************************      ELTCBL  
01212 *                                                          *      ELTCBL  
01213 *        ESTABLISH ADDRESSABILITY OF GCTABULAR IO PARAMETER*      ELTCBL  
01214 *                                                          *      ELTCBL  
01215 ************************************************************      ELTCBL  
01216  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTCBL  
01217      SET CIA-GCTABULR-DDN TO TRUE.                                ELTCBL  
01218      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCBL  
01219                            ADDRESS OF                             ELTCBL  
01220          IOP-INPUT-OUTPUT-PARAMETERS.                             ELTCBL  
01221      EJECT                                                        ELTCBL  
01222                                                                   ELTCBL  
01223 ************************************************************      ELTCBL  
01224 *                                                          *      ELTCBL  
01225 *        CALL INPUT OUTPUT MODULE                          *      ELTCBL  
01226 *                                                          *      ELTCBL  
01227 ************************************************************      ELTCBL  
01228  CALL-INPUT-OUTPUT-MODULE.                                        ELTCBL  
01229      EXEC CICS LINK                                               ELTCBL  
01230                PROGRAM ('ELUIOPGM')                               ELTCBL  
01231                COMMAREA (DFHCOMMAREA)                             ELTCBL  
01232        END-EXEC.                                                  ELTCBL  
01233      EJECT                                                        ELTCBL  
01234                                                                   ELTCBL  
01235 ************************************************************      ELTCBL  
01236 *                                                          *      ELTCBL  
01237 *        ESTABLISH ADDRESSY OF GCCP TABULAR                *      ELTCBL  
01238 *                                                          *      ELTCBL  
01239 ************************************************************      ELTCBL  
01240  ESTABLISH-ADDRESSY-OF-GCCP-TAB.                                  ELTCBL  
01241      SET ADDRESS OF GCCP-TABULAR-REC-AREA TO                      ELTCBL  
01242          IOP-REC-PTR.                                             ELTCBL  
01243      SET IOP-REC-PTR TO NULL.                                     ELTCBL  
01244      EJECT                                                        ELTCBL  
01245                                                                   ELTCBL  
01246 ************************************************************      ELTCBL  
01247 *                                                          *      ELTCBL  
01248 *        SIGNAL NOT FOUND GCTAB ERROR                      *      ELTCBL  
01249 *                                                          *      ELTCBL  
01250 ************************************************************      ELTCBL  
01251  SIGNAL-NOT-FOUND-GCTAB-ERROR.                                    ELTCBL  
01252      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTCBL  
01253      PERFORM SIGNAL-ABEND.                                        ELTCBL  
01254      EJECT                                                        ELTCBL  
01255                                                                   ELTCBL  
01256 ************************************************************      ELTCBL  
01257 *                                                          *      ELTCBL  
01258 *        SIGNAL CRITICAL IO ERROR                          *      ELTCBL  
01259 *                                                          *      ELTCBL  
01260 ************************************************************      ELTCBL  
01261  SIGNAL-CRITICAL-IO-ERROR.                                        ELTCBL  
01262      SET CIA-AB-CRITIO TO TRUE.                                   ELTCBL  
01263      PERFORM SIGNAL-ABEND.                                        ELTCBL  
01264      EJECT                                                        ELTCBL  
01265                                                                   ELTCBL  
01266 ************************************************************      ELTCBL  
01267 *                                                          *      ELTCBL  
01268 *        GENERATE SPECIAL PROVIDERS TEXT                   *      ELTCBL  
01269 *                                                          *      ELTCBL  
01270 ************************************************************      ELTCBL  
01271  GENERATE-SPECIAL-PROVIDERS-TEX.                                  ELTCBL  
01272      MOVE 'COMMUNITY BLUE OPTION' TO SRP-CCP-NAME.                ELTCBL  
01273      MOVE  WS-GCBL                        TO SRP-TABULAR-ID.      ELTCBL  
01274      MOVE  WS-GCBL-PROV-SLOT-NO           TO SRP-TABULAR-SLOT-NO. ELTCBL  
01275      PERFORM CALL-SPECIAL-PROVIDERS-GENERAT.                      ELTCBL  
01276                                                                   ELTCBL  
01277                                                                   ELTCBL  
01278 ************************************************************      ELTCBL  
01279 *                                                          *      ELTCBL  
01280 *        CALL SPECIAL PROVIDERS GENERATOR                  *      ELTCBL  
01281 *                                                          *      ELTCBL  
01282 ************************************************************      ELTCBL  
01283  CALL-SPECIAL-PROVIDERS-GENERAT.                                  ELTCBL  
01284      EXEC CICS LINK                                               ELTCBL  
01285                PROGRAM ('ELGGXXC')                                ELTCBL  
01286                COMMAREA (DFHCOMMAREA)                             ELTCBL  
01287         END-EXEC.                                                 ELTCBL  
01288                                                                   ELTCBL  
01289 ************************************************************      ELTCBL  
01290 *                                                          *      ELTCBL  
01291 *        GENERATE COINSURANCE                              *      ELTCBL  
01292 *                                                          *      ELTCBL  
01293 ************************************************************      ELTCBL  
01294  GENERATE-COINSURANCE.                                            ELTCBL  
01295      EXEC CICS LINK                                               ELTCBL  
01296                PROGRAM ('ELGACLCC')                               ELTCBL  
01297                COMMAREA (DFHCOMMAREA)                             ELTCBL  
01298         END-EXEC.                                                 ELTCBL  
01299                                                                   ELTCBL  
01300 ************************************************************      ELTCBL  
01301 *                                                          *      ELTCBL  
01302 *        GENERATE COPAY                                    *      ELTCBL  
01303 *                                                          *      ELTCBL  
01304 ************************************************************      ELTCBL  
01305  GENERATE-COPAY.                                                  ELTCBL  
01306      EXEC CICS LINK                                               ELTCBL  
01307                PROGRAM ('ELGACPCC')                               ELTCBL  
01308                COMMAREA (DFHCOMMAREA)                             ELTCBL  
01309         END-EXEC.                                                 ELTCBL  
01310                                                                   ELTCBL  
01311 ************************************************************      ELTCBL  
01312 *                                                          *      ELTCBL  
01313 *        GENERATE DEDUCTIBLE                               *      ELTCBL  
01314 *                                                          *      ELTCBL  
01315 ************************************************************      ELTCBL  
01316  GENERATE-DEDUCTIBLE.                                             ELTCBL  
01317      EXEC CICS LINK                                               ELTCBL  
01318                PROGRAM ('ELGADLCC')                               ELTCBL  
01319                COMMAREA (DFHCOMMAREA)                             ELTCBL  
01320         END-EXEC.                                                 ELTCBL  
01321                                                                   ELTCBL  
01322 ************************************************************      ELTCBL  
01323 *                                                          *      ELTCBL  
01324 *        GENERATE MAXIMUM                                  *      ELTCBL  
01325 *                                                          *      ELTCBL  
01326 ************************************************************      ELTCBL  
01327  GENERATE-MAXIMUM.                                                ELTCBL  
01328      EXEC CICS LINK                                               ELTCBL  
01329                PROGRAM ('ELGABMCC')                               ELTCBL  
01330                COMMAREA (DFHCOMMAREA)                             ELTCBL  
01331         END-EXEC.                                                 ELTCBL  
01332      EJECT                                                        ELTCBL  
01333                                                                   ELTCBL  
01334 ************************************************************      ELTCBL  
01335 *                                                          *      ELTCBL  
01336 *        GENERATE OUT OF POCKET                            *      ELTCBL  
01337 *                                                          *      ELTCBL  
01338 ************************************************************      ELTCBL  
01339  GENERATE-OUT-OF-POCKET.                                          ELTCBL  
01340      EXEC CICS LINK                                               ELTCBL  
01341                PROGRAM ('ELGAOLCC')                               ELTCBL  
01342                COMMAREA (DFHCOMMAREA)                             ELTCBL  
01343         END-EXEC.                                                 ELTCBL  
01344                                                                   ELTCBL  
01345 ************************************************************      ELTCBL  
01346 *                                                          *      ELTCBL  
01347 *        CALL CBRI INTERFACE                               *      ELTCBL  
01348 *                                                          *      ELTCBL  
01349 ************************************************************      ELTCBL  
01350  CALL-CBRI-INTERFACE.                                             ELTCBL  
01351      CALL 'ELGCBRI' USING DFHEIBLK                                ELTCBL  
01352                           DFHCOMMAREA.                            ELTCBL  
01353                                                                   ELTCBL  
01354 ************************************************************      ELTCBL  
01355 *                                                          *      ELTCBL  
01356 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTCBL  
01357 *                                                          *      ELTCBL  
01358 ************************************************************      ELTCBL  
01359  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTCBL  
01360      PERFORM INITIALIZE-CMOUT.                                    ELTCBL  
01361      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTCBL  
01362      EJECT                                                        ELTCBL  
01363                                                                   ELTCBL  
01364 ************************************************************      ELTCBL  
01365 *                                                          *      ELTCBL  
01366 *        PREPARE TEXT FOR OUTPUT                           *      ELTCBL  
01367 *                                                          *      ELTCBL  
01368 ************************************************************      ELTCBL  
01369  PREPARE-TEXT-FOR-OUTPUT.                                         ELTCBL  
01370      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTCBL  
01371          UNTIL CMF-DESCR-IDX                                      ELTCBL  
01372                                    GREATER THAN                   ELTCBL  
01373              CMF-NBR-DESCR-LINES.                                 ELTCBL  
01374      EJECT                                                        ELTCBL  
01375                                                                   ELTCBL  
01376 ************************************************************      ELTCBL  
01377 *                                                          *      ELTCBL  
01378 *        INITIALIZE CMOUT                                  *      ELTCBL  
01379 *                                                          *      ELTCBL  
01380 ************************************************************      ELTCBL  
01381  INITIALIZE-CMOUT.                                                ELTCBL  
01382      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTCBL  
01383      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCBL  
01384          ADDRESS OF CMF-DESCR.                                    ELTCBL  
01385      SET CMF-DESCR-IDX TO 1.                                      ELTCBL  
01386      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTCBL  
01387                                                                   ELTCBL  
01388                                                                   ELTCBL  
01389 ************************************************************      ELTCBL  
01390 *                                                          *      ELTCBL  
01391 *        MOVE CMF TEXT TO OUTPUT                           *      ELTCBL  
01392 *                                                          *      ELTCBL  
01393 ************************************************************      ELTCBL  
01394  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTCBL  
01395      PERFORM MOVE-A-LINE.                                         ELTCBL  
01396      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTCBL  
01397          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTCBL  
01398      IF TCAR-FROM-SUB GREATER THAN 20                             ELTCBL  
01399               OR CMF-DESCR-IDX GREATER THAN                       ELTCBL  
01400          CMF-NBR-DESCR-LINES                                      ELTCBL  
01401          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTCBL  
01402                                                                   ELTCBL  
01403                                                                   ELTCBL  
01404 ************************************************************      ELTCBL  
01405 *                                                          *      ELTCBL  
01406 *        FINISH CODES MANUAL TEXT                          *      ELTCBL  
01407 *                                                          *      ELTCBL  
01408 ************************************************************      ELTCBL  
01409  FINISH-CODES-MANUAL-TEXT.                                        ELTCBL  
01410      SET DONE-PROCESSING TO TRUE.                                 ELTCBL  
01411      IF PERIOD-NEEDED                                             ELTCBL  
01412          PERFORM GET-AND-MOVE-PERIOD.                             ELTCBL  
01413      EJECT                                                        ELTCBL  
01414                                                                   ELTCBL  
01415                                                                   ELTCBL  
01416 ************************************************************      ELTCBL  
01417 *                                                          *      ELTCBL  
01418 *        GET AND MOVE PERIOD                               *      ELTCBL  
01419 *                                                          *      ELTCBL  
01420 ************************************************************      ELTCBL  
01421  GET-AND-MOVE-PERIOD.                                             ELTCBL  
01422      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTCBL  
01423          (TCAR-FROM-SUB).                                         ELTCBL  
01424                                                                   ELTCBL  
01425                                                                   ELTCBL  
01426 ************************************************************      ELTCBL  
01427 *                                                          *      ELTCBL  
01428 *        SAVE LAST LINE                                    *      ELTCBL  
01429 *                                                          *      ELTCBL  
01430 ************************************************************      ELTCBL  
01431  SAVE-LAST-LINE.                                                  ELTCBL  
01432      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCBL  
01433      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTCBL  
01434         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTCBL  
01435      ADD 1 TO TCAR-FROM-SUB.                                      ELTCBL  
01436      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTCBL  
01437                                                                   ELTCBL  
01438                                                                   ELTCBL  
01439 ************************************************************      ELTCBL  
01440 *                                                          *      ELTCBL  
01441 *        OUTPUT LAST LINE                                  *      ELTCBL  
01442 *                                                          *      ELTCBL  
01443 ************************************************************      ELTCBL  
01444  OUTPUT-LAST-LINE.                                                ELTCBL  
01445      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTCBL  
01446          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTCBL  
01447      IF BLANK-LINE-NEEDED                                         ELTCBL  
01448          PERFORM CREATE-A-BLANK-LINE.                             ELTCBL  
01449                                                                   ELTCBL  
01450                                                                   ELTCBL  
01451 ************************************************************      ELTCBL  
01452 *                                                          *      ELTCBL  
01453 *        CREATE A BLANK LINE                               *      ELTCBL  
01454 *                                                          *      ELTCBL  
01455 ************************************************************      ELTCBL  
01456  CREATE-A-BLANK-LINE.                                             ELTCBL  
01457      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTCBL  
01458      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTCBL  
01459                                                                   ELTCBL  
01460                                                                   ELTCBL  
01461 ************************************************************      ELTCBL  
01462 *                                                          *      ELTCBL  
01463 *        MOVE A LINE                                       *      ELTCBL  
01464 *                                                          *      ELTCBL  
01465 ************************************************************      ELTCBL  
01466  MOVE-A-LINE.                                                     ELTCBL  
01467      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTCBL  
01468          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTCBL  
01469      SET CMF-DESCR-IDX UP BY 1.                                   ELTCBL  
01470      ADD 1 TO TCAR-FROM-SUB.                                      ELTCBL  
01471      EJECT                                                        ELTCBL  
01472                                                                   ELTCBL  
01473                                                                   ELTCBL  
01474 ************************************************************      ELTCBL  
01475 *                                                          *      ELTCBL  
01476 *        REFORMAT AND WRITE TEXT                           *      ELTCBL  
01477 *                                                          *      ELTCBL  
01478 ************************************************************      ELTCBL  
01479  REFORMAT-AND-WRITE-TEXT.                                         ELTCBL  
01480      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTCBL  
01481      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTCBL  
01482      PERFORM UNSTRING-TEXT.                                       ELTCBL  
01483      MOVE +1 TO TCAR-FROM-SUB.                                    ELTCBL  
01484      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTCBL  
01485      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTCBL  
01486          UNTIL COF-NBR-DTL-LINES GREATER                          ELTCBL  
01487                                   TCAR-OUTPUT-FIELDS-USED -       ELTCBL  
01488              1.                                                   ELTCBL  
01489      PERFORM DISPOSE-OF-LAST-LINE.                                ELTCBL  
01490      PERFORM LINK-TO-OUTPUT.                                      ELTCBL  
01491                                                                   ELTCBL  
01492                                                                   ELTCBL  
01493 ************************************************************      ELTCBL  
01494 *                                                          *      ELTCBL  
01495 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTCBL  
01496 *                                                          *      ELTCBL  
01497 ************************************************************      ELTCBL  
01498  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTCBL  
01499      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTCBL  
01500           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTCBL  
01501      ADD +1 TO TCAR-FROM-SUB.                                     ELTCBL  
01502      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTCBL  
01503      EJECT                                                        ELTCBL  
01504                                                                   ELTCBL  
01505                                                                   ELTCBL  
01506 ************************************************************      ELTCBL  
01507 *                                                          *      ELTCBL  
01508 *        UNSTRING TEXT                                     *      ELTCBL  
01509 *                                                          *      ELTCBL  
01510 ************************************************************      ELTCBL  
01511  UNSTRING-TEXT.                                                   ELTCBL  
01512      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTCBL  
01513      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTCBL  
01514      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTCBL  
01515      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTCBL  
01516      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTCBL  
01517      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTCBL  
01518      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTCBL  
01519      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTCBL  
01520      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTCBL  
01521      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTCBL  
01522      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTCBL  
01523      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTCBL  
01524      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTCBL  
01525      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTCBL  
01526      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTCBL  
01527      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTCBL  
01528      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTCBL  
01529      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTCBL  
01530      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTCBL  
01531      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTCBL  
01532      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTCBL  
01533      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTCBL  
01534      EJECT                                                        ELTCBL  
01535                                                                   ELTCBL  
01536                                                                   ELTCBL  
01537 ************************************************************      ELTCBL  
01538 *                                                          *      ELTCBL  
01539 *        LINK TO OUTPUT                                    *      ELTCBL  
01540 *                                                          *      ELTCBL  
01541 ************************************************************      ELTCBL  
01542  LINK-TO-OUTPUT.                                                  ELTCBL  
01543      EXEC CICS LINK                                               ELTCBL  
01544          PROGRAM ('ELUOUTPT')                                     ELTCBL  
01545          COMMAREA (DFHCOMMAREA)                                   ELTCBL  
01546          END-EXEC.                                                ELTCBL  
01547      EJECT                                                        ELTCBL  
01548                                                                   ELTCBL  
01549                                                                   ELTCBL  
01550 ************************************************************      ELTCBL  
01551 *                                                          *      ELTCBL  
01552 *        DISPOSE OF LAST LINE                              *      ELTCBL  
01553 *                                                          *      ELTCBL  
01554 ************************************************************      ELTCBL  
01555  DISPOSE-OF-LAST-LINE.                                            ELTCBL  
01556      IF NOT ADDITIONAL-TEXT                                       ELTCBL  
01557          PERFORM INITIALIZE-CONTINUED-SW.                         ELTCBL  
01558      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTCBL  
01559          PERFORM SAVE-LAST-LINE                                   ELTCBL  
01560      ELSE                                                         ELTCBL  
01561          PERFORM OUTPUT-LAST-LINE.                                ELTCBL  
01562                                                                   ELTCBL  
01563                                                                   ELTCBL  
01564 ************************************************************      ELTCBL  
01565 *                                                          *      ELTCBL  
01566 *        INITIALIZE CONTINUED SW                           *      ELTCBL  
01567 *                                                          *      ELTCBL  
01568 ************************************************************      ELTCBL  
01569  INITIALIZE-CONTINUED-SW.                                         ELTCBL  
01570      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTCBL  
01571                                                                   ELTCBL  
01572 ************************************************************      ELTCBL  
01573 *                                                          *      ELTCBL  
01574 *        MOVE TRANSLATION TO OUTPUT                        *      ELTCBL  
01575 *                                                          *      ELTCBL  
01576 ************************************************************      ELTCBL  
01577  MOVE-TRANS-TO-OUTPUT.                                            ELTCBL  
01578 *    SET COF-DTL-IDX TO 1.                                        ELTCBL  
01579 *    MOVE 1 TO COF-NBR-DTL-LINES.                                 ELTCBL  
01580      PERFORM VARYING CMF-DESCR-IDX FROM 1 BY 1 UNTIL              ELTCBL  
01581         CMF-DESCR-IDX > CMF-NBR-DESCR-LINES                       ELTCBL  
01582          MOVE CMF-DESCR-LINE(CMF-DESCR-IDX)                       ELTCBL  
01583             TO COF-DTL-LINE(COF-NBR-DTL-LINES)                    ELTCBL  
01584          ADD 1 TO COF-NBR-DTL-LINES                               ELTCBL  
01585      END-PERFORM.                                                 ELTCBL  
