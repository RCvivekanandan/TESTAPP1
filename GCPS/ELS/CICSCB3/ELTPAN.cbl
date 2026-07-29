00001 *      LAST MAINTENANCE TIME:  7.54.00  DATE: 06/13/91            06/29/02
00002  IDENTIFICATION DIVISION.                                         ELTPAN  
00003                                                                      LV001
00004  PROGRAM-ID.         ELTPAN.                                      ELTPAN  
00005                                                                   ELTPAN  
00006  AUTHOR.             ANNE KEFFER KING.                            ELTPAN  
00007                      CLONED FORM ELTRPO.                          ELTPAN  
00008                                                                   ELTPAN  
00009  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTPAN  
00010                      A MUTUAL LEGAL RESERVE COMPANY               ELTPAN  
00011                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTPAN  
00012                      233 N. MICHIGAN AVE                          ELTPAN  
00013                      CHICAGO, ILLINOIS 60601                      ELTPAN  
00014                                                                   ELTPAN  
00015  DATE-WRITTEN.       07-MAR-1996.                                 ELTPAN  
00016                                                                   ELTPAN  
00017  DATE-COMPILED.                                                   ELTPAN  
00018                                                                   ELTPAN  
00019  SECURITY.           COPYRIGHT 1986,                              ELTPAN  
00020                      HEALTH CARE SERVICE CORPORATION              ELTPAN  
00021      SKIP3                                                        ELTPAN  
00022  ENVIRONMENT DIVISION.                                            ELTPAN  
00023                                                                   ELTPAN  
00024  CONFIGURATION SECTION.                                           ELTPAN  
00025  SOURCE-COMPUTER.    IBM-3090.                                    ELTPAN  
00026  OBJECT-COMPUTER.    IBM-3090.                                    ELTPAN  
00027      EJECT                                                        ELTPAN  
00028 ******************************************************************ELTPAN  
00029 *                                                                *ELTPAN  
00030 *    COPYBOOK:   ELTPAN                                          *ELTPAN  
00031 *    DATE:       07-MAR-1996                                     *ELTPAN  
00032 *    AUTHOR:     ANNE KEFFER KING.                               *ELTPAN  
00033 *    FUNCTION:   THIS MODULE WILL DISPLAY ALL OUTPUT ASSOCIATED  *ELTPAN  
00034 *                WITH RESTRICTED PROVIDER OPTION PROGRAM.        *ELTPAN  
00035 *    NOTES:      X---                                            *ELTPAN  
00036 *                                                                *ELTPAN  
00037 ******************************************************************ELTPAN  
00038 *                                                                *ELTPAN  
00039 *                      MAINTENANCE HISTORY                       *ELTPAN  
00040 *                                                                *ELTPAN  
00041 *  MOD     DATE     BY  DRPT                ACTION               *ELTPAN  
00042 * ----- ----------- --- ----- ---------------------------------- *ELTPAN  
00043 * 01.00 07-MAR-1996 AKK       CREATED.                           *ELTPAN  
00044 *                                                                *ELTPAN  
00045 ******************************************************************ELTPAN  
00046                                                                   ELTPAN  
00047  DATA DIVISION.                                                   ELTPAN  
00048                                                                   ELTPAN  
00049  WORKING-STORAGE SECTION.                                         ELTPAN  
00050  01  WS-MISC.                                                     ELTPAN  
00051      05  WS-BEGIN                        PIC X(26) VALUE          ELTPAN  
00052      '*** ELTPAN WS BEGINS ***'.                                  ELTPAN  
00053      05  WS-POINTER2                     POINTER.                 ELTPAN  
00054      05  WS-POINTER3                     POINTER.                 ELTPAN  
00055                                                                   ELTPAN  
00056  01  WS-SWITCHES.                                                 ELTPAN  
00057      05  ADDITIONAL-TEXT-SWITCH   PIC X(01) VALUE SPACE.          ELTPAN  
00058          88  ADDITIONAL-TEXT                VALUE 'A'.            ELTPAN  
00059          88  BLANK-LINE-NEEDED              VALUE 'B'.            ELTPAN  
00060      05  CONTINUED-PROCESSING-SW  PIC X(01) VALUE SPACE.          ELTPAN  
00061          88  DONE-PROCESSING                VALUE 'D'.            ELTPAN  
00062          88  PROCESSING-CMF-TEXT            VALUE 'P'.            ELTPAN  
00063      05  WS-TABULAR-SWITCH        PIC X(01) VALUE 'N'.            ELTPAN  
00064          88  TABULAR-IS-UNDEFINED           VALUE 'Y'.            ELTPAN  
00065      05  WS-PERIOD-SWITCH         PIC X(01) VALUE 'N'.            ELTPAN  
00066          88  PERIOD-NEEDED                  VALUE 'Y'.            ELTPAN  
00067      05  WS-APPROVAL-SOURCE-SW    PIC X     VALUE SPACE.          ELTPAN  
00068          88  NOT-HOLDING-APPROVAL-SRCE      VALUE 'N'.            ELTPAN  
00069          88  HOLDING-APPROVAL-SOURCE        VALUE 'H'.            ELTPAN  
00070                                                                   ELTPAN  
00071  01  WS-HOLD-AREA.                                                ELTPAN  
00072      05  WS-GPAN-PROV-SLOT-NO   COMP-3 PIC S9(07) VALUE +0.       ELTPAN  
00073                                                                   ELTPAN  
00074 **************************************************************    ELTPAN  
00075 ***                   PROGRAM CONSTANTS                           ELTPAN  
00076 **************************************************************    ELTPAN  
00077      05  WS-GRP                   PIC X(06) VALUE 'GROUP'.        ELTPAN  
00078      05  WS-GCCP                  PIC X(06) VALUE '#GCCP '.       ELTPAN  
00079      05  WS-GPAN                  PIC X(06) VALUE '#GPAN '.       ELTPAN  
00080      05  WS-INST                  PIC X(13) VALUE 'INSTITUTIONAL'.ELTPAN  
00081      05  WS-PROF                  PIC X(13) VALUE 'PROFESSIONAL '.ELTPAN  
00082      05  WS-SUPP                  PIC X(13) VALUE 'SUPPLEMENTAL '.ELTPAN  
00083      05  WS-APPROVAL              PIC X(09) VALUE 'APPROVAL.'.    ELTPAN  
00084                                                                   ELTPAN  
00085 **************************************************************    ELTPAN  
00086 ***                   HEADER  LINE                                ELTPAN  
00087 **************************************************************    ELTPAN  
00088      05  WS-HEADER-LINE.                                          ELTPAN  
00089          10  FILLER               PIC X(15) VALUE SPACES.         ELTPAN  
00090          10  FILLER               PIC X(36) VALUE                 ELTPAN  
00091          'PREFERRED ANCILLARY NETWORK PROGRAM '.                  ELTPAN  
00092          10  WS-HDR-LINE-BCBSMM   PIC X(13) VALUE SPACES.         ELTPAN  
00093          10  FILLER               PIC X(20) VALUE SPACES.         ELTPAN  
00094                                                                   ELTPAN  
00095 **************************************************************    ELTPAN  
00096 ***                   SCREEN BODY LINES                           ELTPAN  
00097 **************************************************************    ELTPAN  
00098  01  WS-SCREEN-LINE-AREA.                                         ELTPAN  
00099      05  WS-APPRVL-LINE.                                          ELTPAN  
00100          10  FILLER               PIC X(47) VALUE                 ELTPAN  
00101          'PREFERRED ANCILLARY NETWORK PROGRAM REQUIRES '.         ELTPAN  
00102          10  FILLER               PIC X(32) VALUE SPACES.         ELTPAN  
00103                                                                   ELTPAN  
00104      05  WS-ALT-PRICING-LINE-BC.                                  ELTPAN  
00105          10  FILLER               PIC X(52) VALUE                 ELTPAN  
00106          'THE ALTERNATE PRICING FOR INSTITUTIONAL SERVICES IS '.  ELTPAN  
00107          10  FILLER               PIC X(27) VALUE SPACES.         ELTPAN  
00108                                                                   ELTPAN  
00109      05  WS-ALT-PRICING-LINE-BS.                                  ELTPAN  
00110          10  FILLER               PIC X(51) VALUE                 ELTPAN  
00111          'THE ALTERNATE PRICING FOR PROFESSIONAL SERVICES IS '.   ELTPAN  
00112          10  FILLER               PIC X(28) VALUE SPACES.         ELTPAN  
00113                                                                   ELTPAN  
00114      05  WS-ALT-PRICING-LINE-MM.                                  ELTPAN  
00115          10  FILLER               PIC X(51) VALUE                 ELTPAN  
00116          'THE ALTERNATE PRICING FOR SUPPLEMENTAL BENEFITS IS '.   ELTPAN  
00117          10  FILLER               PIC X(28) VALUE SPACES.         ELTPAN  
00118                                                                   ELTPAN  
00119      05  WS-BENE-REDUCT-LINE.                                     ELTPAN  
00120          10  FILLER               PIC X(47) VALUE                 ELTPAN  
00121          'DENIED OR REDUCED BENEFITS DUE TO THIS PROGRAM:'.       ELTPAN  
00122          10  FILLER               PIC X(32) VALUE SPACES.         ELTPAN  
00123                                                                   ELTPAN  
00124      05  WS-SPILL-OVER-LINE.                                      ELTPAN  
00125          10  FILLER               PIC X(51) VALUE                 ELTPAN  
00126          'UNPAID SERVICES AFTER BASIC BENEFITS REDUCTION ARE '.   ELTPAN  
00127          10  FILLER               PIC X(28) VALUE SPACES.         ELTPAN  
00128                                                                   ELTPAN  
00129 **************************************************************    ELTPAN  
00130 ** SPECIAL MESSAGE FOR THE NOT APPLICABLE                         ELTPAN  
00131 ** ALSO THE FIXED TEXT FOR TABULAR GPAN                           ELTPAN  
00132 **************************************************************    ELTPAN  
00133      05  WS-PAN-APPLIES.                                          ELTPAN  
00134          10  FILLER               PIC X(79) VALUE                 ELTPAN  
00135          'THE PREFERRED ANCILLARY NETWORK PROGRAM APPLIES TO'.    ELTPAN  
00136                                                                   ELTPAN  
00137      05  WS-NOT-APPLICABLE-MSG.                                   ELTPAN  
00138          10  FILLER               PIC X(40) VALUE                 ELTPAN  
00139          'THE PREFERRED ANCILLARY NETWORK PROGRAM '.              ELTPAN  
00140          10  FILLER               PIC X(18) VALUE                 ELTPAN  
00141          'IS NOT APPLICABLE.'.                                    ELTPAN  
00142          10  FILLER               PIC X(19) VALUE SPACES.         ELTPAN  
00143                                                                   ELTPAN  
00144      05  WS-SPEC-PROV-MSG.                                        ELTPAN  
00145          10  FILLER               PIC X(79) VALUE                 ELTPAN  
00146          'THERE ARE SPECIAL PROVIDERS INCLUDED IN THIS COST CONTAIELTPAN  
00147 -        'NMENT PROGRAM.'.                                        ELTPAN  
00148                                                                   ELTPAN  
00149      05  WS-DISCLAIMER-MSG.                                       ELTPAN  
00150          10  FILLER               PIC X(79) VALUE                 ELTPAN  
00151          '*** SUBJECT TO OTHER CONTRACT LIMITATIONS ***'.         ELTPAN  
00152                                                                   ELTPAN  
00153      05  WS-NOT-APPLY-TO-LOB-BC.                                  ELTPAN  
00154          10  FILLER               PIC X(54) VALUE                 ELTPAN  
00155          'THE PREFERRED ANCILLARY NETWORK PROGRAM DOES NOT APPLY'.ELTPAN  
00156          10  FILLER               PIC X(28) VALUE                 ELTPAN  
00157          ' FOR INSTITUTIONAL BENEFITS.'.                          ELTPAN  
00158                                                                   ELTPAN  
00159      05  WS-NOT-APPLY-TO-LOB-BS.                                  ELTPAN  
00160          10  FILLER               PIC X(54) VALUE                 ELTPAN  
00161          'THE PREFERRED ANCILLARY NETWORK PROGRAM DOES NOT APPLY'.ELTPAN  
00162          10  FILLER               PIC X(27) VALUE                 ELTPAN  
00163          ' FOR PROFESSIONAL BENEFITS.'.                           ELTPAN  
00164                                                                   ELTPAN  
00165      05  WS-NOT-APPLY-TO-LOB-MM.                                  ELTPAN  
00166          10  FILLER               PIC X(54) VALUE                 ELTPAN  
00167          'THE PREFERRED ANCILLARY NETWORK PROGRAM DOES NOT APPLY'.ELTPAN  
00168          10  FILLER               PIC X(27) VALUE                 ELTPAN  
00169          ' FOR SUPPLEMENTAL BENEFITS.'.                           ELTPAN  
00170                                                                   ELTPAN  
00171  LINKAGE SECTION.                                                 ELTPAN  
00172  01  DFHCOMMAREA.                                                 ELTPAN  
00173      COPY ELSCOMMC.                                               ELTPAN  
00174 /                                                                 ELTPAN  
00175      COPY ELSCIA2C.                                               ELTPAN  
00176 /                                                                 ELTPAN  
00177      COPY ELSCMDSC.                                               ELTPAN  
00178 /                                                                 ELTPAN  
00179      COPY ELSCMIFC.                                               ELTPAN  
00180 /                                                                 ELTPAN  
00181      COPY ELSIOPMC.                                               ELTPAN  
00182 /                                                                 ELTPAN  
00183      COPY ELSKEYSC.                                               ELTPAN  
00184 /                                                                 ELTPAN  
00185      COPY ELSOUTPC.                                               ELTPAN  
00186 /                                                                 ELTPAN  
00187      COPY ELSSRTPC.                                               ELTPAN  
00188 /                                                                 ELTPAN  
00189      COPY ELSTCWAC.                                               ELTPAN  
00190 /                                                                 ELTPAN  
00191      COPY ELSSSCBC.                                               ELTPAN  
00192 /                                                                 ELTPAN  
00193  01  GROUP-SPECIFIC-REC.                                          ELTPAN  
00194      COPY GCGROUPC.                                               ELTPAN  
00195 /                                                                 ELTPAN  
00196  01  GCCP-TABULAR-REC-AREA.                                       ELTPAN  
00197      COPY GCTGCCPC.                                               ELTPAN  
00198      EJECT                                                        ELTPAN  
00199  PROCEDURE DIVISION.                                              ELTPAN  
00200 ************************************************************      ELTPAN  
00201 *                                                          *      ELTPAN  
00202 *                    PROCEDURE DIVISION                    *      ELTPAN  
00203 *                                                          *      ELTPAN  
00204 ************************************************************      ELTPAN  
00205                                                                   ELTPAN  
00206                                                                   ELTPAN  
00207 ************************************************************      ELTPAN  
00208 *                                                          *      ELTPAN  
00209 *        COMMUNITY PROVIDER OPTION                         *      ELTPAN  
00210 *                                                          *      ELTPAN  
00211 ************************************************************      ELTPAN  
00212  COMMUNITY-BLUE-PROVIDER-OPTION.                                  ELTPAN  
00213      PERFORM INITIALIZATION.                                      ELTPAN  
00214      PERFORM PROCESS.                                             ELTPAN  
00215      GOBACK.                                                      ELTPAN  
00216                                                                   ELTPAN  
00217 ************************************************************      ELTPAN  
00218 *                                                          *      ELTPAN  
00219 *        INITIALIZATION                                    *      ELTPAN  
00220 *                                                          *      ELTPAN  
00221 ************************************************************      ELTPAN  
00222  INITIALIZATION.                                                  ELTPAN  
00223      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTPAN  
00224      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTPAN  
00225                                                                   ELTPAN  
00226 ************************************************************      ELTPAN  
00227 *                                                          *      ELTPAN  
00228 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTPAN  
00229 *                                                          *      ELTPAN  
00230 ************************************************************      ELTPAN  
00231  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTPAN  
00232      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTPAN  
00233      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTPAN  
00234      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTPAN  
00235                                                                   ELTPAN  
00236 ************************************************************      ELTPAN  
00237 *                                                          *      ELTPAN  
00238 *        CHECK FOR VALID COMMAREA                          *      ELTPAN  
00239 *                                                          *      ELTPAN  
00240 ************************************************************      ELTPAN  
00241  CHECK-FOR-VALID-COMMAREA.                                        ELTPAN  
00242      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTPAN  
00243          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTPAN  
00244                                                                   ELTPAN  
00245 ************************************************************      ELTPAN  
00246 *                                                          *      ELTPAN  
00247 *        SIGNAL INVALID COMMAREA                           *      ELTPAN  
00248 *                                                          *      ELTPAN  
00249 ************************************************************      ELTPAN  
00250  SIGNAL-INVALID-COMMAREA.                                         ELTPAN  
00251      EXEC CICS ABEND                                              ELTPAN  
00252                ABCODE('EL01')                                     ELTPAN  
00253         END-EXEC.                                                 ELTPAN  
00254      EJECT                                                        ELTPAN  
00255                                                                   ELTPAN  
00256 ************************************************************      ELTPAN  
00257 *                                                          *      ELTPAN  
00258 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTPAN  
00259 *                                                          *      ELTPAN  
00260 ************************************************************      ELTPAN  
00261  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTPAN  
00262      IF ECA-CIA-PTR = NULL                                        ELTPAN  
00263          PERFORM SIGNAL-INVALID-CIA                               ELTPAN  
00264      ELSE                                                         ELTPAN  
00265          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTPAN  
00266                                                                   ELTPAN  
00267                                                                   ELTPAN  
00268 ************************************************************      ELTPAN  
00269 *                                                          *      ELTPAN  
00270 *        SIGNAL INVALID CIA                                *      ELTPAN  
00271 *                                                          *      ELTPAN  
00272 ************************************************************      ELTPAN  
00273  SIGNAL-INVALID-CIA.                                              ELTPAN  
00274      EXEC CICS ABEND                                              ELTPAN  
00275                ABCODE('EL02')                                     ELTPAN  
00276         END-EXEC.                                                 ELTPAN  
00277                                                                   ELTPAN  
00278 ************************************************************      ELTPAN  
00279 *                                                          *      ELTPAN  
00280 *        ESTABLISH ADDRESS OF CIA                          *      ELTPAN  
00281 *                                                          *      ELTPAN  
00282 ************************************************************      ELTPAN  
00283  ESTABLISH-ADDRESS-OF-CIA.                                        ELTPAN  
00284      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTPAN  
00285                            ADDRESS OF                             ELTPAN  
00286          CIA-ELS-COMMON-INTERFACE-AREA.                           ELTPAN  
00287      EJECT                                                        ELTPAN  
00288                                                                   ELTPAN  
00289 ************************************************************      ELTPAN  
00290 *                                                          *      ELTPAN  
00291 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTPAN  
00292 *                                                          *      ELTPAN  
00293 ************************************************************      ELTPAN  
00294  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTPAN  
00295      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTPAN  
00296      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAN  
00297                            ADDRESS OF                             ELTPAN  
00298          SSB-SELECTOR-STATUS-CTL-BLK.                             ELTPAN  
00299      IF CIA-RC-PTR-NULL                                           ELTPAN  
00300          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAN  
00301                                                                   ELTPAN  
00302 ************************************************************      ELTPAN  
00303 *                                                          *      ELTPAN  
00304 *        SIGNAL UNALLOC AREA ERROR                         *      ELTPAN  
00305 *                                                          *      ELTPAN  
00306 ************************************************************      ELTPAN  
00307  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTPAN  
00308      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTPAN  
00309      PERFORM SIGNAL-ABEND.                                        ELTPAN  
00310                                                                   ELTPAN  
00311 ************************************************************      ELTPAN  
00312 *                                                          *      ELTPAN  
00313 *        SIGNAL ABEND                                      *      ELTPAN  
00314 *                                                          *      ELTPAN  
00315 ************************************************************      ELTPAN  
00316  SIGNAL-ABEND.                                                    ELTPAN  
00317      EXEC CICS ABEND                                              ELTPAN  
00318                ABCODE(CIA-ABCODE)                                 ELTPAN  
00319         END-EXEC.                                                 ELTPAN  
00320      EJECT                                                        ELTPAN  
00321                                                                   ELTPAN  
00322 ************************************************************      ELTPAN  
00323 *                                                          *      ELTPAN  
00324 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTPAN  
00325 *                                                          *      ELTPAN  
00326 ************************************************************      ELTPAN  
00327  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTPAN  
00328      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTPAN  
00329      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTPAN  
00330      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTPAN  
00331      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTPAN  
00332      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTPAN  
00333      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTPAN  
00334      PERFORM ESTABLISH-ADDRESSABILITY-OF-CS.                      ELTPAN  
00335                                                                   ELTPAN  
00336 ************************************************************      ELTPAN  
00337 *                                                          *      ELTPAN  
00338 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTPAN  
00339 *                                                          *      ELTPAN  
00340 ************************************************************      ELTPAN  
00341  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTPAN  
00342      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTPAN  
00343      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAN  
00344                            ADDRESS OF                             ELTPAN  
00345          CMF-CODES-MANUAL-INTERFACE.                              ELTPAN  
00346      IF CIA-RC-PTR-NULL                                           ELTPAN  
00347          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAN  
00348      EJECT                                                        ELTPAN  
00349                                                                   ELTPAN  
00350 ************************************************************      ELTPAN  
00351 *                                                          *      ELTPAN  
00352 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTPAN  
00353 *                                                          *      ELTPAN  
00354 ************************************************************      ELTPAN  
00355  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTPAN  
00356      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTPAN  
00357      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAN  
00358                            ADDRESS OF                             ELTPAN  
00359          COF-OUTPUT-INTERFACE.                                    ELTPAN  
00360      IF CIA-RC-PTR-NULL                                           ELTPAN  
00361          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAN  
00362      EJECT                                                        ELTPAN  
00363                                                                   ELTPAN  
00364 ************************************************************      ELTPAN  
00365 *                                                          *      ELTPAN  
00366 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTPAN  
00367 *                                                          *      ELTPAN  
00368 ************************************************************      ELTPAN  
00369  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTPAN  
00370      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTPAN  
00371      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAN  
00372                            ADDRESS OF                             ELTPAN  
00373          SRP-SUBROUTINE-PARAMETERS.                               ELTPAN  
00374      IF CIA-RC-PTR-NULL                                           ELTPAN  
00375          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAN  
00376      EJECT                                                        ELTPAN  
00377                                                                   ELTPAN  
00378 ************************************************************      ELTPAN  
00379 *                                                          *      ELTPAN  
00380 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTPAN  
00381 *                                                          *      ELTPAN  
00382 ************************************************************      ELTPAN  
00383  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTPAN  
00384      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTPAN  
00385      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAN  
00386                            ADDRESS OF                             ELTPAN  
00387          TCAR-COMPRESSION-WORK-AREA.                              ELTPAN  
00388      IF CIA-RC-PTR-NULL                                           ELTPAN  
00389          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAN  
00390      EJECT                                                        ELTPAN  
00391                                                                   ELTPAN  
00392 ************************************************************      ELTPAN  
00393 *                                                          *      ELTPAN  
00394 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTPAN  
00395 *                                                          *      ELTPAN  
00396 ************************************************************      ELTPAN  
00397  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTPAN  
00398      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTPAN  
00399      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAN  
00400                            ADDRESS OF                             ELTPAN  
00401          KWA-FILE-KEY-WORK-AREA.                                  ELTPAN  
00402      IF CIA-RC-PTR-NULL                                           ELTPAN  
00403          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAN  
00404      EJECT                                                        ELTPAN  
00405                                                                   ELTPAN  
00406 ************************************************************      ELTPAN  
00407 *                                                          *      ELTPAN  
00408 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELTPAN  
00409 *                                                          *      ELTPAN  
00410 ************************************************************      ELTPAN  
00411  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTPAN  
00412      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTPAN  
00413      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAN  
00414                            ADDRESS OF                             ELTPAN  
00415          GROUP-SPECIFIC-REC.                                      ELTPAN  
00416      IF CIA-RC-PTR-NULL                                           ELTPAN  
00417          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAN  
00418      EJECT                                                        ELTPAN  
00419                                                                   ELTPAN  
00420 ************************************************************      ELTPAN  
00421 *                                                          *      ELTPAN  
00422 *        ESTABLISH ADDRESSABILITY OF CST CONTAINMENT PROGRA*      ELTPAN  
00423 *                                                          *      ELTPAN  
00424 ************************************************************      ELTPAN  
00425  ESTABLISH-ADDRESSABILITY-OF-CS.                                  ELTPAN  
00426      SET CIA-GCTABULR-DDN TO TRUE.                                ELTPAN  
00427      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAN  
00428                            ADDRESS OF                             ELTPAN  
00429          GCCP-TABULAR-REC-AREA.                                   ELTPAN  
00430      IF CIA-RC-PTR-NULL                                           ELTPAN  
00431          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAN  
00432      EJECT                                                        ELTPAN  
00433                                                                   ELTPAN  
00434 ************************************************************      ELTPAN  
00435 *                                                          *      ELTPAN  
00436 *        PROCESS                                           *      ELTPAN  
00437 *                                                          *      ELTPAN  
00438 ************************************************************      ELTPAN  
00439  PROCESS.                                                         ELTPAN  
00440      IF GCG-PAN-PARTICIPATION-IND = ZERO                          ELTPAN  
00441          PERFORM GENERATE-NOT-APPLICABLE-MESSAG                   ELTPAN  
00442      ELSE                                                         ELTPAN  
00443          PERFORM GENERATE-PAN-TEXT.                               ELTPAN  
00444      PERFORM TERMINATE-OUTPUT.                                    ELTPAN  
00445                                                                   ELTPAN  
00446 ************************************************************      ELTPAN  
00447 *                                                          *      ELTPAN  
00448 *        EJECT NEW PAGE                                    *      ELTPAN  
00449 *                                                          *      ELTPAN  
00450 ************************************************************      ELTPAN  
00451  EJECT-NEW-PAGE.                                                  ELTPAN  
00452      SET COF-NEW-PAGE      TO TRUE.                               ELTPAN  
00453      MOVE WS-HEADER-LINE   TO COF-HDR-LINE                        ELTPAN  
00454          (COF-NBR-HDR-LINES).                                     ELTPAN  
00455      PERFORM LINK-TO-OUTPUT.                                      ELTPAN  
00456      EJECT                                                        ELTPAN  
00457                                                                   ELTPAN  
00458 ************************************************************      ELTPAN  
00459 *                                                          *      ELTPAN  
00460 *        TRANSLATE AND DISPLAY PAN IND                     *      ELTPAN  
00461 *                                                          *      ELTPAN  
00462 ************************************************************      ELTPAN  
00463  TRANSLATE-AND-DISPLAY-PAN-IND.                                   ELTPAN  
00464      INITIALIZE TCAR-FROM-AREA.                                   ELTPAN  
00465      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAN  
00466      MOVE WS-PAN-APPLIES TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELTPAN  
00467      ADD  +1 TO TCAR-FROM-SUB.                                    ELTPAN  
00468      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAN  
00469      SET PERIOD-NEEDED TO TRUE.                                   ELTPAN  
00470      MOVE GCG-PAN-PARTICIPATION-IND TO CMF-CODE-VALUE.            ELTPAN  
00471      MOVE 'PAN-PARTICIPATION-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTPAN  
00472      MOVE WS-GRP TO CMF-RECORD-PREFIX.                            ELTPAN  
00473      EXEC CICS LINK                                               ELTPAN  
00474                PROGRAM ('ELUCMIF')                                ELTPAN  
00475                COMMAREA (DFHCOMMAREA)                             ELTPAN  
00476         END-EXEC.                                                 ELTPAN  
00477      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAN  
00478      MOVE SPACE TO ADDITIONAL-TEXT-SWITCH.                        ELTPAN  
00479                                                                   ELTPAN  
00480 ************************************************************      ELTPAN  
00481 *                                                          *      ELTPAN  
00482 *        SET UP OUTPUT SUBSCRIPTS                          *      ELTPAN  
00483 *                                                          *      ELTPAN  
00484 ************************************************************      ELTPAN  
00485  SET-UP-OUTPUT-SUBSCRIPTS.                                        ELTPAN  
00486      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAN  
00487      MOVE +2 TO COF-NBR-HDR-LINES.                                ELTPAN  
00488                                                                   ELTPAN  
00489 ************************************************************      ELTPAN  
00490 *                                                          *      ELTPAN  
00491 *        GENERATE NOT APPLICABLE MESSAGE                   *      ELTPAN  
00492 *                                                          *      ELTPAN  
00493 ************************************************************      ELTPAN  
00494  GENERATE-NOT-APPLICABLE-MESSAG.                                  ELTPAN  
00495      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTPAN  
00496      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTPAN  
00497      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPAN  
00498      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTPAN  
00499          (COF-NBR-DTL-LINES).                                     ELTPAN  
00500      PERFORM EJECT-NEW-PAGE.                                      ELTPAN  
00501      EJECT                                                        ELTPAN  
00502                                                                   ELTPAN  
00503 ************************************************************      ELTPAN  
00504 *                                                          *      ELTPAN  
00505 *        GENERATE CBL TEXT                                 *      ELTPAN  
00506 *                                                          *      ELTPAN  
00507 ************************************************************      ELTPAN  
00508  GENERATE-PAN-TEXT.                                               ELTPAN  
00509      SET WS-POINTER2 TO NULLS.                                    ELTPAN  
00510      SET WS-POINTER3 TO NULLS.                                    ELTPAN  
00511      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTPAN  
00512      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTPAN  
00513                            WS-POINTER2.                           ELTPAN  
00514      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTPAN  
00515      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTPAN  
00516                            WS-POINTER3.                           ELTPAN  
00517      PERFORM SEARCH-FOR-GCCP-TABULAR.                             ELTPAN  
00518      PERFORM DETERMINE-SELECTION.                                 ELTPAN  
00519      EJECT                                                        ELTPAN  
00520                                                                   ELTPAN  
00521 ************************************************************      ELTPAN  
00522 *                                                          *      ELTPAN  
00523 *        TERMINATE OUTPUT                                  *      ELTPAN  
00524 *                                                          *      ELTPAN  
00525 ************************************************************      ELTPAN  
00526  TERMINATE-OUTPUT.                                                ELTPAN  
00527      SET COF-END TO TRUE.                                         ELTPAN  
00528      PERFORM LINK-TO-OUTPUT.                                      ELTPAN  
00529      EJECT                                                        ELTPAN  
00530                                                                   ELTPAN  
00531 ************************************************************      ELTPAN  
00532 *                                                          *      ELTPAN  
00533 *        SEARCH FOR GCCP TABULAR                           *      ELTPAN  
00534 *                                                          *      ELTPAN  
00535 ************************************************************      ELTPAN  
00536  SEARCH-FOR-GCCP-TABULAR.                                         ELTPAN  
00537      SET GCG-INDEX TO +1.                                         ELTPAN  
00538      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTPAN  
00539         AT END                                                    ELTPAN  
00540              MOVE ZEROES TO KWA-PROVISION-SLOT-NO                 ELTPAN  
00541         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GCCP                     ELTPAN  
00542              MOVE GCG-TAB-ID (GCG-INDEX)                          ELTPAN  
00543                  TO KWA-PROVISION-ID                              ELTPAN  
00544              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTPAN  
00545                  TO KWA-PROVISION-SLOT-NO                         ELTPAN  
00546         END-SEARCH.                                               ELTPAN  
00547      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELTPAN  
00548          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTPAN  
00549      PERFORM GET-GCCP-TABULAR.                                    ELTPAN  
00550      PERFORM SEARCH-THE-GSS-ENTRY.                                ELTPAN  
00551      EJECT                                                        ELTPAN  
00552                                                                   ELTPAN  
00553 ************************************************************      ELTPAN  
00554 *                                                          *      ELTPAN  
00555 *        TRANSLATE APPROVAL SOURCE                         *      ELTPAN  
00556 *                                                          *      ELTPAN  
00557 ************************************************************      ELTPAN  
00558  TRANSLATE-APPROVAL-SOURCE.                                       ELTPAN  
00559      INITIALIZE WS-PERIOD-SWITCH.                                 ELTPAN  
00560      INITIALIZE TCAR-FROM-AREA.                                   ELTPAN  
00561 *    MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAN  
00562      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAN  
00563      MOVE WS-APPRVL-LINE TO COF-DTL-LINE                          ELTPAN  
00564          (COF-NBR-DTL-LINES).                                     ELTPAN  
00565 *    ADD  +1 TO TCAR-FROM-SUB.                                    ELTPAN  
00566      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPAN  
00567      SET ADDITIONAL-TEXT TO TRUE.                                 ELTPAN  
00568      IF NOT-HOLDING-APPROVAL-SRCE                                 ELTPAN  
00569          PERFORM GET-APPROVAL-TRANSLATION                         ELTPAN  
00570      ELSE                                                         ELTPAN  
00571          PERFORM USE-EXISTING-APPROVAL-TRANSLAT.                  ELTPAN  
00572 *    PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAN  
00573      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTPAN  
00574      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTPAN  
00575                            WS-POINTER3.                           ELTPAN  
00576      PERFORM MOVE-TRANS-TO-OUTPUT.                                ELTPAN  
00577      PERFORM LINK-TO-OUTPUT.                                      ELTPAN  
00578 *    MOVE WS-APPROVAL TO TCAR-FROM-LINE                           ELTPAN  
00579 *        (TCAR-FROM-SUB).                                         ELTPAN  
00580 *    PERFORM FINISH-SENTENCE.                                     ELTPAN  
00581      EJECT                                                        ELTPAN  
00582                                                                   ELTPAN  
00583                                                                   ELTPAN  
00584 ************************************************************      ELTPAN  
00585 *                                                          *      ELTPAN  
00586 *        GET APPROVAL TRANSLATION                          *      ELTPAN  
00587 *                                                          *      ELTPAN  
00588 ************************************************************      ELTPAN  
00589  GET-APPROVAL-TRANSLATION.                                        ELTPAN  
00590      MOVE GSS-PA-APPROVAL-SOURCE-IND (GSS-INDEX)                  ELTPAN  
00591                 TO CMF-CODE-VALUE.                                ELTPAN  
00592      MOVE   'PA-APPROVAL-SOURCE-IND'                              ELTPAN  
00593             TO CMF-ELEMENT-SYSTEM-NAME.                           ELTPAN  
00594      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTPAN  
00595      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTPAN  
00596      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTPAN  
00597                            ADDRESS OF CMF-DESCR.                  ELTPAN  
00598      EJECT                                                        ELTPAN  
00599                                                                   ELTPAN  
00600 ************************************************************      ELTPAN  
00601 *                                                          *      ELTPAN  
00602 *        USE EXISTING APPROVAL TRANSLATION                 *      ELTPAN  
00603 *                                                          *      ELTPAN  
00604 ************************************************************      ELTPAN  
00605  USE-EXISTING-APPROVAL-TRANSLAT.                                  ELTPAN  
00606      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTPAN  
00607      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTPAN  
00608                            ADDRESS OF CMF-DESCR.                  ELTPAN  
00609      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTPAN  
00610      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTPAN  
00611                            WS-POINTER2.                           ELTPAN  
00612      EJECT                                                        ELTPAN  
00613                                                                   ELTPAN  
00614 ************************************************************      ELTPAN  
00615 *                                                          *      ELTPAN  
00616 *        FINISH SENTENCE                                   *      ELTPAN  
00617 *                                                          *      ELTPAN  
00618 ************************************************************      ELTPAN  
00619  FINISH-SENTENCE.                                                 ELTPAN  
00620      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAN  
00621      PERFORM REFORMAT-AND-WRITE-TEXT.                             ELTPAN  
00622                                                                   ELTPAN  
00623 ************************************************************      ELTPAN  
00624 *                                                          *      ELTPAN  
00625 *        TRANSLATE AND DISPLAY CODE VALUE                  *      ELTPAN  
00626 *                                                          *      ELTPAN  
00627 ************************************************************      ELTPAN  
00628  TRANSLATE-AND-DISPLAY-CODE-VAL.                                  ELTPAN  
00629      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTPAN  
00630      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAN  
00631                                                                   ELTPAN  
00632 ************************************************************      ELTPAN  
00633 *                                                          *      ELTPAN  
00634 *        SIGNAL UNDEFINED TABULAR ERROR                    *      ELTPAN  
00635 *                                                          *      ELTPAN  
00636 ************************************************************      ELTPAN  
00637  SIGNAL-UNDEFINED-TABULAR-ERROR.                                  ELTPAN  
00638      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTPAN  
00639      PERFORM SIGNAL-ABEND.                                        ELTPAN  
00640      EJECT                                                        ELTPAN  
00641                                                                   ELTPAN  
00642 ************************************************************      ELTPAN  
00643 *                                                          *      ELTPAN  
00644 *        DETERMINE SELECTION                               *      ELTPAN  
00645 *                                                          *      ELTPAN  
00646 ************************************************************      ELTPAN  
00647  DETERMINE-SELECTION.                                             ELTPAN  
00648      SET NOT-HOLDING-APPROVAL-SRCE TO TRUE.                       ELTPAN  
00649      IF SSB-PROV-CLASS-INST OR                                    ELTPAN  
00650                   SSB-PROV-CLASS-BOTH                             ELTPAN  
00651          PERFORM CREATE-INSTITUTIONAL-SCREEN.                     ELTPAN  
00652      IF SSB-PROV-CLASS-PROF OR                                    ELTPAN  
00653                   SSB-PROV-CLASS-BOTH                             ELTPAN  
00654          PERFORM CREATE-PROFESSIONAL-SCREEN.                      ELTPAN  
00655      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL '03' OR '04' OR       ELTPAN  
00656                    '06' OR '08')                                  ELTPAN  
00657          PERFORM CREATE-SUPPLEMENTAL-SCREEN.                      ELTPAN  
00658                                                                   ELTPAN  
00659 ************************************************************      ELTPAN  
00660 *                                                          *      ELTPAN  
00661 *        CREATE INSTITUTIONAL SCREEN                       *      ELTPAN  
00662 *                                                          *      ELTPAN  
00663 ************************************************************      ELTPAN  
00664  CREATE-INSTITUTIONAL-SCREEN.                                     ELTPAN  
00665      MOVE WS-INST TO WS-HDR-LINE-BCBSMM.                          ELTPAN  
00666      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTPAN  
00667      IF GSS-PA-BC-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTPAN  
00668          ZEROES                                                   ELTPAN  
00669                 AND LOW-VALUES                                    ELTPAN  
00670          PERFORM GENERATE-INSTITUTIONAL-TEXT                      ELTPAN  
00671      ELSE                                                         ELTPAN  
00672          PERFORM GENERATE-INSTITUTIONAL-NOT-APP.                  ELTPAN  
00673      EJECT                                                        ELTPAN  
00674                                                                   ELTPAN  
00675 ************************************************************      ELTPAN  
00676 *                                                          *      ELTPAN  
00677 *        GENERATE INSTITUTIONAL TEXT                       *      ELTPAN  
00678 *                                                          *      ELTPAN  
00679 ************************************************************      ELTPAN  
00680  GENERATE-INSTITUTIONAL-TEXT.                                     ELTPAN  
00681      PERFORM EJECT-NEW-PAGE.                                      ELTPAN  
00682      PERFORM TRANSLATE-AND-DISPLAY-PAN-IND.                       ELTPAN  
00683      IF GSS-PA-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL SPACES   ELTPAN  
00684          AND                                                      ELTPAN  
00685                 ZEROES AND LOW-VALUES                             ELTPAN  
00686          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTPAN  
00687      PERFORM TRANSLATE-BC-IND.                                    ELTPAN  
00688      IF GSS-PA-BC-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL SPACES  ELTPAN  
00689           AND ZEROES AND LOW-VALUES                               ELTPAN  
00690            PERFORM TRANSLATE-BC-PAYMENT-LEVEL-IND.                ELTPAN  
00691      IF GSS-PA-BC-ALT-PRICING-METHOD (GSS-INDEX) NOT EQUAL        ELTPAN  
00692          SPACES AND                                               ELTPAN  
00693                 ZEROES AND LOW-VALUES                             ELTPAN  
00694          PERFORM TRANSLATE-BC-ALT-PRIC.                           ELTPAN  
00695      SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTPAN  
00696      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTPAN  
00697      IF GSS-PA-BC-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES AND    ELTPAN  
00698          ZEROES                                                   ELTPAN  
00699                 AND LOW-VALUES                                    ELTPAN  
00700          PERFORM TRANSLATE-BC-CALC-METHOD.                        ELTPAN  
00701      IF GSS-PA-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTPAN  
00702          SPACES AND                                               ELTPAN  
00703                 ZEROES AND LOW-VALUES                             ELTPAN  
00704          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTPAN  
00705      IF GSS-PA-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL SPACES AND    ELTPAN  
00706          ZEROES                                                   ELTPAN  
00707                 AND LOW-VALUES                                    ELTPAN  
00708          PERFORM GENERATE-SPILL-OVER-TEXT.                        ELTPAN  
00709      PERFORM GENERATE-SPECIAL-PROVIDERS-SEN.                      ELTPAN  
00710      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTPAN  
00711      EJECT                                                        ELTPAN  
00712                                                                   ELTPAN  
00713 ************************************************************      ELTPAN  
00714 *                                                          *      ELTPAN  
00715 *        GENERATE INSTITUTIONAL NOT APPLICABLE             *      ELTPAN  
00716 *                                                          *      ELTPAN  
00717 ************************************************************      ELTPAN  
00718  GENERATE-INSTITUTIONAL-NOT-APP.                                  ELTPAN  
00719      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTPAN  
00720      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPAN  
00721      MOVE WS-NOT-APPLY-TO-LOB-BC TO COF-DTL-LINE                  ELTPAN  
00722          (COF-NBR-DTL-LINES).                                     ELTPAN  
00723      PERFORM EJECT-NEW-PAGE.                                      ELTPAN  
00724      EJECT                                                        ELTPAN  
00725                                                                   ELTPAN  
00726                                                                   ELTPAN  
00727 ************************************************************      ELTPAN  
00728 *                                                          *      ELTPAN  
00729 *        GENERATE DISCLAIMER MESSAGE                       *      ELTPAN  
00730 *                                                          *      ELTPAN  
00731 ************************************************************      ELTPAN  
00732  GENERATE-DISCLAIMER-MESSAGE.                                     ELTPAN  
00733      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAN  
00734      MOVE WS-DISCLAIMER-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).  ELTPAN  
00735      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPAN  
00736      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTPAN  
00737      PERFORM LINK-TO-OUTPUT.                                      ELTPAN  
00738      EJECT                                                        ELTPAN  
00739                                                                   ELTPAN  
00740 ************************************************************      ELTPAN  
00741 *                                                          *      ELTPAN  
00742 *        TRANSLATE BC IND                                  *      ELTPAN  
00743 *                                                          *      ELTPAN  
00744 ************************************************************      ELTPAN  
00745  TRANSLATE-BC-IND.                                                ELTPAN  
00746      INITIALIZE TCAR-FROM-AREA.                                   ELTPAN  
00747      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAN  
00748      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAN  
00749      SET PERIOD-NEEDED TO TRUE.                                   ELTPAN  
00750      MOVE GSS-PA-BC-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTPAN  
00751      MOVE   'PA-BC-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTPAN  
00752      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTPAN  
00753      PERFORM INITIALIZE-CMOUT.                                    ELTPAN  
00754      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELTPAN  
00755      PERFORM MOVE-TRANS-TO-OUTPUT.                                ELTPAN  
00756      PERFORM LINK-TO-OUTPUT.                                      ELTPAN  
00757                                                                   ELTPAN  
00758 ************************************************************      ELTPAN  
00759 *                                                          *      ELTPAN  
00760 *        TRANSLATE BC PAYMENT LEVEL INDICATOR              *      ELTPAN  
00761 *                                                          *      ELTPAN  
00762 ************************************************************      ELTPAN  
00763  TRANSLATE-BC-PAYMENT-LEVEL-IND.                                  ELTPAN  
00764      INITIALIZE TCAR-FROM-AREA.                                   ELTPAN  
00765      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAN  
00766      MOVE 'PA-BC-PAYMENT-LEVEL-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTPAN  
00767      MOVE GSS-PA-BC-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTPAN  
00768         CMF-CODE-VALUE.                                           ELTPAN  
00769      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTPAN  
00770      PERFORM INITIALIZE-CMOUT.                                    ELTPAN  
00771      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELTPAN  
00772      PERFORM MOVE-TRANS-TO-OUTPUT.                                ELTPAN  
00773      PERFORM LINK-TO-OUTPUT.                                      ELTPAN  
00774                                                                   ELTPAN  
00775 ************************************************************      ELTPAN  
00776 *                                                          *      ELTPAN  
00777 *        TRANSLATE BC ALT PRIC                             *      ELTPAN  
00778 *                                                          *      ELTPAN  
00779 ************************************************************      ELTPAN  
00780  TRANSLATE-BC-ALT-PRIC.                                           ELTPAN  
00781      INITIALIZE TCAR-FROM-AREA.                                   ELTPAN  
00782      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAN  
00783      MOVE WS-ALT-PRICING-LINE-BC TO TCAR-FROM-LINE                ELTPAN  
00784          (TCAR-FROM-SUB).                                         ELTPAN  
00785      ADD  +1 TO TCAR-FROM-SUB.                                    ELTPAN  
00786      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTPAN  
00787      SET PERIOD-NEEDED TO TRUE.                                   ELTPAN  
00788      MOVE GSS-PA-BC-ALT-PRICING-METHOD (GSS-INDEX) TO             ELTPAN  
00789          CMF-CODE-VALUE.                                          ELTPAN  
00790      MOVE  'PA-BC-ALT-PRICING-METHOD' TO CMF-ELEMENT-SYSTEM-NAME. ELTPAN  
00791      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAN  
00792      EJECT                                                        ELTPAN  
00793                                                                   ELTPAN  
00794 ************************************************************      ELTPAN  
00795 *                                                          *      ELTPAN  
00796 *        TRANSLATE BC CALC METHOD                          *      ELTPAN  
00797 *                                                          *      ELTPAN  
00798 ************************************************************      ELTPAN  
00799  TRANSLATE-BC-CALC-METHOD.                                        ELTPAN  
00800      INITIALIZE TCAR-FROM-AREA.                                   ELTPAN  
00801      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAN  
00802      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAN  
00803      SET PERIOD-NEEDED TO TRUE.                                   ELTPAN  
00804      MOVE GSS-PA-BC-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTPAN  
00805      MOVE   'PA-BC-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTPAN  
00806      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAN  
00807      EJECT                                                        ELTPAN  
00808                                                                   ELTPAN  
00809 ************************************************************      ELTPAN  
00810 *                                                          *      ELTPAN  
00811 *        GENERATE ACCUM TABULAR DATA                       *      ELTPAN  
00812 *                                                          *      ELTPAN  
00813 ************************************************************      ELTPAN  
00814  GENERATE-ACCUM-TABULAR-DATA.                                     ELTPAN  
00815      PERFORM GENERATE-COINSURANCE.                                ELTPAN  
00816      PERFORM GENERATE-COPAY.                                      ELTPAN  
00817      PERFORM GENERATE-DEDUCTIBLE.                                 ELTPAN  
00818      PERFORM GENERATE-MAXIMUM.                                    ELTPAN  
00819      PERFORM GENERATE-OUT-OF-POCKET.                              ELTPAN  
00820      EJECT                                                        ELTPAN  
00821                                                                   ELTPAN  
00822 ************************************************************      ELTPAN  
00823 *                                                          *      ELTPAN  
00824 *        GENERATE COMBINED BENEFITS REDUCTION TEXT         *      ELTPAN  
00825 *                                                          *      ELTPAN  
00826 ************************************************************      ELTPAN  
00827  GENERATE-COMBINED-BENEFITS-RED.                                  ELTPAN  
00828      MOVE 'PA' TO SRP-COST-CONT-TYPE.                             ELTPAN  
00829      MOVE 'PREFERRED ANCILLARY NETWORK PROGRAM' TO SRP-CCP-NAME.  ELTPAN  
00830      MOVE GSS-PA-COMB-BENE-REDUCT-IND (GSS-INDEX)                 ELTPAN  
00831                  TO SRP-CCP-COMB-BENE-REDUCT-IND.                 ELTPAN  
00832      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELTPAN  
00833      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTPAN  
00834                            ADDRESS OF                             ELTPAN  
00835          GCCP-TABULAR-REC-AREA.                                   ELTPAN  
00836      PERFORM CALL-CBRI-INTERFACE.                                 ELTPAN  
00837      EJECT                                                        ELTPAN  
00838                                                                   ELTPAN  
00839 ************************************************************      ELTPAN  
00840 *                                                          *      ELTPAN  
00841 *        CREATE PROFESSIONAL SCREEN                        *      ELTPAN  
00842 *                                                          *      ELTPAN  
00843 ************************************************************      ELTPAN  
00844  CREATE-PROFESSIONAL-SCREEN.                                      ELTPAN  
00845      MOVE WS-PROF TO WS-HDR-LINE-BCBSMM.                          ELTPAN  
00846      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTPAN  
00847      IF GSS-PA-BS-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTPAN  
00848          ZEROES                                                   ELTPAN  
00849                 AND LOW-VALUES                                    ELTPAN  
00850          PERFORM GENERATE-PROFESSIONAL-TEXT                       ELTPAN  
00851      ELSE                                                         ELTPAN  
00852          PERFORM GENERATE-PROFESSIONAL-NOT-APPL.                  ELTPAN  
00853      EJECT                                                        ELTPAN  
00854                                                                   ELTPAN  
00855 ************************************************************      ELTPAN  
00856 *                                                          *      ELTPAN  
00857 *        GENERATE PROFESSIONAL TEXT                        *      ELTPAN  
00858 *                                                          *      ELTPAN  
00859 ************************************************************      ELTPAN  
00860  GENERATE-PROFESSIONAL-TEXT.                                      ELTPAN  
00861      PERFORM EJECT-NEW-PAGE.                                      ELTPAN  
00862      PERFORM TRANSLATE-AND-DISPLAY-PAN-IND.                       ELTPAN  
00863      IF GSS-PA-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTPAN  
00864          SPACES                                                   ELTPAN  
00865                 AND ZEROES AND LOW-VALUES                         ELTPAN  
00866          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTPAN  
00867      PERFORM TRANSLATE-BS-IND.                                    ELTPAN  
00868      IF GSS-PA-BS-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL SPACES  ELTPAN  
00869            AND ZEROES AND LOW-VALUES                              ELTPAN  
00870            PERFORM TRANSLATE-BS-PAYMENT-LEVEL-IND.                ELTPAN  
00871      IF GSS-PA-BS-ALT-PRICING-METHOD (GSS-INDEX) NOT EQUAL        ELTPAN  
00872          SPACES AND                                               ELTPAN  
00873                 ZEROES AND LOW-VALUES                             ELTPAN  
00874          PERFORM TRANSLATE-BS-ALT-PRIC.                           ELTPAN  
00875      SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTPAN  
00876      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTPAN  
00877      IF GSS-PA-BS-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES AND    ELTPAN  
00878          ZEROES                                                   ELTPAN  
00879                 AND LOW-VALUES                                    ELTPAN  
00880          PERFORM TRANSLATE-BS-CALC-METHOD.                        ELTPAN  
00881      IF GSS-PA-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTPAN  
00882          SPACES                                                   ELTPAN  
00883                 AND ZEROES AND LOW-VALUES                         ELTPAN  
00884          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTPAN  
00885      IF GSS-PA-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL SPACES AND    ELTPAN  
00886          ZEROES                                                   ELTPAN  
00887                 AND LOW-VALUES                                    ELTPAN  
00888          PERFORM GENERATE-SPILL-OVER-TEXT.                        ELTPAN  
00889      PERFORM GENERATE-SPECIAL-PROVIDERS-SEN.                      ELTPAN  
00890      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTPAN  
00891                                                                   ELTPAN  
00892 ************************************************************      ELTPAN  
00893 *                                                          *      ELTPAN  
00894 *        GENERATE PROFESSIONAL NOT APPLICABLE              *      ELTPAN  
00895 *                                                          *      ELTPAN  
00896 ************************************************************      ELTPAN  
00897  GENERATE-PROFESSIONAL-NOT-APPL.                                  ELTPAN  
00898      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPAN  
00899      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPAN  
00900      MOVE WS-NOT-APPLY-TO-LOB-BS TO COF-DTL-LINE                  ELTPAN  
00901          (COF-NBR-DTL-LINES).                                     ELTPAN  
00902      PERFORM EJECT-NEW-PAGE.                                      ELTPAN  
00903      EJECT                                                        ELTPAN  
00904                                                                   ELTPAN  
00905 ************************************************************      ELTPAN  
00906 *                                                          *      ELTPAN  
00907 *        TRANSLATE BS IND                                  *      ELTPAN  
00908 *                                                          *      ELTPAN  
00909 ************************************************************      ELTPAN  
00910  TRANSLATE-BS-IND.                                                ELTPAN  
00911      INITIALIZE TCAR-FROM-AREA.                                   ELTPAN  
00912      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAN  
00913      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTPAN  
00914      SET PERIOD-NEEDED TO TRUE.                                   ELTPAN  
00915      MOVE GSS-PA-BS-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTPAN  
00916      MOVE   'PA-BS-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTPAN  
00917      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTPAN  
00918      PERFORM INITIALIZE-CMOUT.                                    ELTPAN  
00919      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELTPAN  
00920      PERFORM MOVE-TRANS-TO-OUTPUT.                                ELTPAN  
00921      PERFORM LINK-TO-OUTPUT.                                      ELTPAN  
00922 *    PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAN  
00923      EJECT                                                        ELTPAN  
00924                                                                   ELTPAN  
00925 ************************************************************      ELTPAN  
00926 *                                                          *      ELTPAN  
00927 *        TRANSLATE BS PAYMENT LEVEL INDICATOR              *      ELTPAN  
00928 *                                                          *      ELTPAN  
00929 ************************************************************      ELTPAN  
00930  TRANSLATE-BS-PAYMENT-LEVEL-IND.                                  ELTPAN  
00931      INITIALIZE TCAR-FROM-AREA.                                   ELTPAN  
00932      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAN  
00933      MOVE 'PA-BS-PAYMENT-LEVEL-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTPAN  
00934      MOVE GSS-PA-BS-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTPAN  
00935         CMF-CODE-VALUE.                                           ELTPAN  
00936      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTPAN  
00937      PERFORM INITIALIZE-CMOUT.                                    ELTPAN  
00938      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELTPAN  
00939      PERFORM MOVE-TRANS-TO-OUTPUT.                                ELTPAN  
00940      PERFORM LINK-TO-OUTPUT.                                      ELTPAN  
00941 *    PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAN  
00942                                                                   ELTPAN  
00943 ************************************************************      ELTPAN  
00944 *                                                          *      ELTPAN  
00945 *        TRANSLATE BS CALC METHOD                          *      ELTPAN  
00946 *                                                          *      ELTPAN  
00947 ************************************************************      ELTPAN  
00948  TRANSLATE-BS-CALC-METHOD.                                        ELTPAN  
00949      INITIALIZE TCAR-FROM-AREA.                                   ELTPAN  
00950      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAN  
00951      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAN  
00952      SET PERIOD-NEEDED TO TRUE.                                   ELTPAN  
00953      MOVE GSS-PA-BS-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTPAN  
00954      MOVE   'PA-BS-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTPAN  
00955      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAN  
00956      EJECT                                                        ELTPAN  
00957                                                                   ELTPAN  
00958 ************************************************************      ELTPAN  
00959 *                                                          *      ELTPAN  
00960 *        TRANSLATE BS ALT PRIC                             *      ELTPAN  
00961 *                                                          *      ELTPAN  
00962 ************************************************************      ELTPAN  
00963  TRANSLATE-BS-ALT-PRIC.                                           ELTPAN  
00964      INITIALIZE TCAR-FROM-AREA.                                   ELTPAN  
00965      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAN  
00966      MOVE WS-ALT-PRICING-LINE-BS TO TCAR-FROM-LINE                ELTPAN  
00967          (TCAR-FROM-SUB).                                         ELTPAN  
00968      ADD  +1 TO TCAR-FROM-SUB.                                    ELTPAN  
00969      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTPAN  
00970      SET PERIOD-NEEDED TO TRUE.                                   ELTPAN  
00971      MOVE GSS-PA-BS-ALT-PRICING-METHOD (GSS-INDEX) TO             ELTPAN  
00972          CMF-CODE-VALUE.                                          ELTPAN  
00973      MOVE  'PA-BS-ALT-PRICING-METHOD' TO CMF-ELEMENT-SYSTEM-NAME. ELTPAN  
00974      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAN  
00975      EJECT                                                        ELTPAN  
00976                                                                   ELTPAN  
00977 ************************************************************      ELTPAN  
00978 *                                                          *      ELTPAN  
00979 *        CREATE SUPPLEMENTAL SCREEN                        *      ELTPAN  
00980 *                                                          *      ELTPAN  
00981 ************************************************************      ELTPAN  
00982  CREATE-SUPPLEMENTAL-SCREEN.                                      ELTPAN  
00983      MOVE WS-SUPP TO WS-HDR-LINE-BCBSMM.                          ELTPAN  
00984      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTPAN  
00985      IF GSS-PA-MM-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTPAN  
00986          ZEROES                                                   ELTPAN  
00987                 AND LOW-VALUES                                    ELTPAN  
00988          PERFORM GENERATE-SUPPLEMENTAL-TEXT                       ELTPAN  
00989      ELSE                                                         ELTPAN  
00990          PERFORM GENERATE-SUPPLEMENTAL-NOT-APPL.                  ELTPAN  
00991      EJECT                                                        ELTPAN  
00992                                                                   ELTPAN  
00993 ************************************************************      ELTPAN  
00994 *                                                          *      ELTPAN  
00995 *        GENERATE SUPPLEMENTAL TEXT                        *      ELTPAN  
00996 *                                                          *      ELTPAN  
00997 ************************************************************      ELTPAN  
00998  GENERATE-SUPPLEMENTAL-TEXT.                                      ELTPAN  
00999      PERFORM EJECT-NEW-PAGE.                                      ELTPAN  
01000      PERFORM TRANSLATE-AND-DISPLAY-PAN-IND.                       ELTPAN  
01001      IF GSS-PA-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTPAN  
01002          SPACES                                                   ELTPAN  
01003                 AND ZEROES AND LOW-VALUES                         ELTPAN  
01004          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTPAN  
01005      PERFORM TRANSLATE-MM-IND.                                    ELTPAN  
01006      IF GSS-PA-MM-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL SPACES  ELTPAN  
01007         AND SPACES AND LOW-VALUES                                 ELTPAN  
01008            PERFORM TRANSLATE-MM-PAYMENT-LEVEL-IND.                ELTPAN  
01009      IF GSS-PA-MM-ALT-PRICING-METHOD (GSS-INDEX) NOT EQUAL        ELTPAN  
01010          SPACES AND                                               ELTPAN  
01011                 ZEROES AND LOW-VALUES                             ELTPAN  
01012          PERFORM TRANSLATE-MM-ALT-PRIC.                           ELTPAN  
01013      SET SRP-ACCUM-PROV-CLASS-SUPP TO TRUE.                       ELTPAN  
01014      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTPAN  
01015      IF GSS-PA-MM-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES AND    ELTPAN  
01016          ZEROES                                                   ELTPAN  
01017                 AND LOW-VALUES                                    ELTPAN  
01018          PERFORM TRANSLATE-MM-CALC-METHOD.                        ELTPAN  
01019      IF GSS-PA-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTPAN  
01020          SPACES                                                   ELTPAN  
01021                 AND ZEROES AND LOW-VALUES                         ELTPAN  
01022          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTPAN  
01023      PERFORM GENERATE-SPECIAL-PROVIDERS-SEN.                      ELTPAN  
01024      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTPAN  
01025                                                                   ELTPAN  
01026                                                                   ELTPAN  
01027 ************************************************************      ELTPAN  
01028 *                                                          *      ELTPAN  
01029 *        GENERATE SUPPLEMENTAL NOT APPLICABLE              *      ELTPAN  
01030 *                                                          *      ELTPAN  
01031 ************************************************************      ELTPAN  
01032  GENERATE-SUPPLEMENTAL-NOT-APPL.                                  ELTPAN  
01033      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPAN  
01034      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPAN  
01035      MOVE WS-NOT-APPLY-TO-LOB-MM TO COF-DTL-LINE                  ELTPAN  
01036          (COF-NBR-DTL-LINES).                                     ELTPAN  
01037      PERFORM EJECT-NEW-PAGE.                                      ELTPAN  
01038      EJECT                                                        ELTPAN  
01039                                                                   ELTPAN  
01040 ************************************************************      ELTPAN  
01041 *                                                          *      ELTPAN  
01042 *        TRANSLATE MM IND                                  *      ELTPAN  
01043 *                                                          *      ELTPAN  
01044 ************************************************************      ELTPAN  
01045  TRANSLATE-MM-IND.                                                ELTPAN  
01046      INITIALIZE TCAR-FROM-AREA.                                   ELTPAN  
01047      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAN  
01048      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAN  
01049      SET PERIOD-NEEDED TO TRUE.                                   ELTPAN  
01050      MOVE GSS-PA-MM-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTPAN  
01051      MOVE   'PA-MM-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTPAN  
01052      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAN  
01053      EJECT                                                        ELTPAN  
01054                                                                   ELTPAN  
01055 ************************************************************      ELTPAN  
01056 *                                                          *      ELTPAN  
01057 *        TRANSLATE MM PAYMENT LEVEL INDICATOR              *      ELTPAN  
01058 *                                                          *      ELTPAN  
01059 ************************************************************      ELTPAN  
01060  TRANSLATE-MM-PAYMENT-LEVEL-IND.                                  ELTPAN  
01061      INITIALIZE TCAR-FROM-AREA.                                   ELTPAN  
01062      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAN  
01063      MOVE 'PA-MM-PAYMENT-LEVEL-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTPAN  
01064      MOVE GSS-PA-MM-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTPAN  
01065         CMF-CODE-VALUE.                                           ELTPAN  
01066      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTPAN  
01067      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAN  
01068                                                                   ELTPAN  
01069 ************************************************************      ELTPAN  
01070 *                                                          *      ELTPAN  
01071 *        TRANSLATE MM ALT PRIC                             *      ELTPAN  
01072 *                                                          *      ELTPAN  
01073 ************************************************************      ELTPAN  
01074  TRANSLATE-MM-ALT-PRIC.                                           ELTPAN  
01075      INITIALIZE TCAR-FROM-AREA.                                   ELTPAN  
01076      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAN  
01077      MOVE WS-ALT-PRICING-LINE-MM TO TCAR-FROM-LINE                ELTPAN  
01078          (TCAR-FROM-SUB).                                         ELTPAN  
01079      ADD  +1 TO TCAR-FROM-SUB.                                    ELTPAN  
01080      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTPAN  
01081      SET PERIOD-NEEDED TO TRUE.                                   ELTPAN  
01082      MOVE GSS-PA-MM-ALT-PRICING-METHOD (GSS-INDEX) TO             ELTPAN  
01083          CMF-CODE-VALUE.                                          ELTPAN  
01084      MOVE  'PA-MM-ALT-PRICING-METHOD' TO CMF-ELEMENT-SYSTEM-NAME. ELTPAN  
01085      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAN  
01086      EJECT                                                        ELTPAN  
01087                                                                   ELTPAN  
01088 ************************************************************      ELTPAN  
01089 *                                                          *      ELTPAN  
01090 *        TRANSLATE MM CALC METHOD                          *      ELTPAN  
01091 *                                                          *      ELTPAN  
01092 ************************************************************      ELTPAN  
01093  TRANSLATE-MM-CALC-METHOD.                                        ELTPAN  
01094      INITIALIZE TCAR-FROM-AREA.                                   ELTPAN  
01095      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAN  
01096      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAN  
01097      SET PERIOD-NEEDED TO TRUE.                                   ELTPAN  
01098      MOVE GSS-PA-MM-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTPAN  
01099      MOVE   'PA-MM-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTPAN  
01100      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAN  
01101      EJECT                                                        ELTPAN  
01102                                                                   ELTPAN  
01103                                                                   ELTPAN  
01104 ************************************************************      ELTPAN  
01105 *                                                          *      ELTPAN  
01106 *        GENERATE SPILL OVER TEXT                          *      ELTPAN  
01107 *                                                          *      ELTPAN  
01108 ************************************************************      ELTPAN  
01109  GENERATE-SPILL-OVER-TEXT.                                        ELTPAN  
01110      INITIALIZE TCAR-FROM-AREA.                                   ELTPAN  
01111      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAN  
01112      MOVE WS-SPILL-OVER-LINE TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELTPAN  
01113      ADD  +1 TO TCAR-FROM-SUB.                                    ELTPAN  
01114      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAN  
01115      SET PERIOD-NEEDED TO TRUE.                                   ELTPAN  
01116      MOVE GSS-PA-SPILL-OVER-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTPAN  
01117      MOVE 'PS-SPILL-OVER-IND' TO CMF-ELEMENT-SYSTEM-NAME.         ELTPAN  
01118      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAN  
01119      EJECT                                                        ELTPAN  
01120                                                                   ELTPAN  
01121 ************************************************************      ELTPAN  
01122 *                                                          *      ELTPAN  
01123 *        GENERATE SPECIAL PROVIDERS SENTENCE               *      ELTPAN  
01124 *                                                          *      ELTPAN  
01125 ************************************************************      ELTPAN  
01126  GENERATE-SPECIAL-PROVIDERS-SEN.                                  ELTPAN  
01127      SET GCG-INDEX TO +1.                                         ELTPAN  
01128      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTPAN  
01129         AT END                                                    ELTPAN  
01130              MOVE ZEROES TO WS-GPAN-PROV-SLOT-NO                  ELTPAN  
01131         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GPAN                     ELTPAN  
01132              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTPAN  
01133                  TO WS-GPAN-PROV-SLOT-NO                          ELTPAN  
01134         END-SEARCH.                                               ELTPAN  
01135      IF WS-GPAN-PROV-SLOT-NO NOT EQUAL ZEROES                     ELTPAN  
01136          PERFORM DISPLAY-SPECIAL-PROVIDERS-SENT                   ELTPAN  
01137          PERFORM GENERATE-SPECIAL-PROVIDERS-TEX.                  ELTPAN  
01138                                                                   ELTPAN  
01139 ************************************************************      ELTPAN  
01140 *                                                          *      ELTPAN  
01141 *        DISPLAY SPECIAL PROVIDERS SENTENCE                *      ELTPAN  
01142 *                                                          *      ELTPAN  
01143 ************************************************************      ELTPAN  
01144  DISPLAY-SPECIAL-PROVIDERS-SENT.                                  ELTPAN  
01145      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAN  
01146      MOVE WS-SPEC-PROV-MSG  TO COF-DTL-LINE                       ELTPAN  
01147          (COF-NBR-DTL-LINES).                                     ELTPAN  
01148      PERFORM LINK-TO-OUTPUT.                                      ELTPAN  
01149      EJECT                                                        ELTPAN  
01150                                                                   ELTPAN  
01151 ************************************************************      ELTPAN  
01152 *                                                          *      ELTPAN  
01153 *        SEARCH THE GSS ENTRY                              *      ELTPAN  
01154 *                                                          *      ELTPAN  
01155 ************************************************************      ELTPAN  
01156  SEARCH-THE-GSS-ENTRY.                                            ELTPAN  
01157      SET GSS-INDEX TO 1.                                          ELTPAN  
01158      SEARCH GSS-ENTRY                                             ELTPAN  
01159          AT END                                                   ELTPAN  
01160               SET TABULAR-IS-UNDEFINED TO TRUE                    ELTPAN  
01161          WHEN GSS-PA-PROG-CODE-CHR (GSS-INDEX)                    ELTPAN  
01162               CONTINUE                                            ELTPAN  
01163         END-SEARCH.                                               ELTPAN  
01164      IF TABULAR-IS-UNDEFINED                                      ELTPAN  
01165          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTPAN  
01166                                                                   ELTPAN  
01167 ************************************************************      ELTPAN  
01168 *                                                          *      ELTPAN  
01169 *        CALL CODES MANUAL INTERFACE                       *      ELTPAN  
01170 *                                                          *      ELTPAN  
01171 ************************************************************      ELTPAN  
01172  CALL-CODES-MANUAL-INTERFACE.                                     ELTPAN  
01173      MOVE WS-GCCP TO CMF-RECORD-PREFIX.                           ELTPAN  
01174      EXEC CICS LINK                                               ELTPAN  
01175                PROGRAM ('ELUCMIF')                                ELTPAN  
01176                COMMAREA (DFHCOMMAREA)                             ELTPAN  
01177        END-EXEC.                                                  ELTPAN  
01178      EJECT                                                        ELTPAN  
01179                                                                   ELTPAN  
01180 ************************************************************      ELTPAN  
01181 *                                                          *      ELTPAN  
01182 *        GET GCCP TABULAR                                  *      ELTPAN  
01183 *                                                          *      ELTPAN  
01184 ************************************************************      ELTPAN  
01185  GET-GCCP-TABULAR.                                                ELTPAN  
01186      PERFORM ESTABLISH-ADDRESSABILITY-OF-GC.                      ELTPAN  
01187      SET IOP-RD              TO TRUE.                             ELTPAN  
01188      SET IOP-STG-MODE-MOVE   TO TRUE.                             ELTPAN  
01189      SET IOP-FCQ-NONE        TO TRUE.                             ELTPAN  
01190      SET IOP-KVQ-EQ          TO TRUE.                             ELTPAN  
01191      MOVE KWA-GCTABULR-KEY   TO IOP-FILE-KEY.                     ELTPAN  
01192      PERFORM CALL-INPUT-OUTPUT-MODULE.                            ELTPAN  
01193      IF IOP-RC-OK                                                 ELTPAN  
01194          PERFORM ESTABLISH-ADDRESSY-OF-GCCP-TAB                   ELTPAN  
01195      ELSE IF IOP-RC-NOTFND                                        ELTPAN  
01196          PERFORM SIGNAL-NOT-FOUND-GCTAB-ERROR                     ELTPAN  
01197      ELSE                                                         ELTPAN  
01198          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTPAN  
01199      EJECT                                                        ELTPAN  
01200                                                                   ELTPAN  
01201 ************************************************************      ELTPAN  
01202 *                                                          *      ELTPAN  
01203 *        ESTABLISH ADDRESSABILITY OF GCTABULAR IO PARAMETER*      ELTPAN  
01204 *                                                          *      ELTPAN  
01205 ************************************************************      ELTPAN  
01206  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTPAN  
01207      SET CIA-GCTABULR-DDN TO TRUE.                                ELTPAN  
01208      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAN  
01209                            ADDRESS OF                             ELTPAN  
01210          IOP-INPUT-OUTPUT-PARAMETERS.                             ELTPAN  
01211      EJECT                                                        ELTPAN  
01212                                                                   ELTPAN  
01213 ************************************************************      ELTPAN  
01214 *                                                          *      ELTPAN  
01215 *        CALL INPUT OUTPUT MODULE                          *      ELTPAN  
01216 *                                                          *      ELTPAN  
01217 ************************************************************      ELTPAN  
01218  CALL-INPUT-OUTPUT-MODULE.                                        ELTPAN  
01219      EXEC CICS LINK                                               ELTPAN  
01220                PROGRAM ('ELUIOPGM')                               ELTPAN  
01221                COMMAREA (DFHCOMMAREA)                             ELTPAN  
01222        END-EXEC.                                                  ELTPAN  
01223      EJECT                                                        ELTPAN  
01224                                                                   ELTPAN  
01225 ************************************************************      ELTPAN  
01226 *                                                          *      ELTPAN  
01227 *        ESTABLISH ADDRESSY OF GCCP TABULAR                *      ELTPAN  
01228 *                                                          *      ELTPAN  
01229 ************************************************************      ELTPAN  
01230  ESTABLISH-ADDRESSY-OF-GCCP-TAB.                                  ELTPAN  
01231      SET ADDRESS OF GCCP-TABULAR-REC-AREA TO                      ELTPAN  
01232          IOP-REC-PTR.                                             ELTPAN  
01233      SET IOP-REC-PTR TO NULL.                                     ELTPAN  
01234      EJECT                                                        ELTPAN  
01235                                                                   ELTPAN  
01236 ************************************************************      ELTPAN  
01237 *                                                          *      ELTPAN  
01238 *        SIGNAL NOT FOUND GCTAB ERROR                      *      ELTPAN  
01239 *                                                          *      ELTPAN  
01240 ************************************************************      ELTPAN  
01241  SIGNAL-NOT-FOUND-GCTAB-ERROR.                                    ELTPAN  
01242      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTPAN  
01243      PERFORM SIGNAL-ABEND.                                        ELTPAN  
01244      EJECT                                                        ELTPAN  
01245                                                                   ELTPAN  
01246 ************************************************************      ELTPAN  
01247 *                                                          *      ELTPAN  
01248 *        SIGNAL CRITICAL IO ERROR                          *      ELTPAN  
01249 *                                                          *      ELTPAN  
01250 ************************************************************      ELTPAN  
01251  SIGNAL-CRITICAL-IO-ERROR.                                        ELTPAN  
01252      SET CIA-AB-CRITIO TO TRUE.                                   ELTPAN  
01253      PERFORM SIGNAL-ABEND.                                        ELTPAN  
01254      EJECT                                                        ELTPAN  
01255                                                                   ELTPAN  
01256 ************************************************************      ELTPAN  
01257 *                                                          *      ELTPAN  
01258 *        GENERATE SPECIAL PROVIDERS TEXT                   *      ELTPAN  
01259 *                                                          *      ELTPAN  
01260 ************************************************************      ELTPAN  
01261  GENERATE-SPECIAL-PROVIDERS-TEX.                                  ELTPAN  
01262      MOVE 'PREFERRED ANCILLARY NETWORK PROGRAM' TO SRP-CCP-NAME.  ELTPAN  
01263      MOVE  WS-GPAN                        TO SRP-TABULAR-ID.      ELTPAN  
01264      MOVE  WS-GPAN-PROV-SLOT-NO           TO SRP-TABULAR-SLOT-NO. ELTPAN  
01265      PERFORM CALL-SPECIAL-PROVIDERS-GENERAT.                      ELTPAN  
01266                                                                   ELTPAN  
01267                                                                   ELTPAN  
01268 ************************************************************      ELTPAN  
01269 *                                                          *      ELTPAN  
01270 *        CALL SPECIAL PROVIDERS GENERATOR                  *      ELTPAN  
01271 *                                                          *      ELTPAN  
01272 ************************************************************      ELTPAN  
01273  CALL-SPECIAL-PROVIDERS-GENERAT.                                  ELTPAN  
01274      EXEC CICS LINK                                               ELTPAN  
01275                PROGRAM ('ELGGXXC')                                ELTPAN  
01276                COMMAREA (DFHCOMMAREA)                             ELTPAN  
01277         END-EXEC.                                                 ELTPAN  
01278                                                                   ELTPAN  
01279 ************************************************************      ELTPAN  
01280 *                                                          *      ELTPAN  
01281 *        GENERATE COINSURANCE                              *      ELTPAN  
01282 *                                                          *      ELTPAN  
01283 ************************************************************      ELTPAN  
01284  GENERATE-COINSURANCE.                                            ELTPAN  
01285      EXEC CICS LINK                                               ELTPAN  
01286                PROGRAM ('ELGACLCC')                               ELTPAN  
01287                COMMAREA (DFHCOMMAREA)                             ELTPAN  
01288         END-EXEC.                                                 ELTPAN  
01289                                                                   ELTPAN  
01290 ************************************************************      ELTPAN  
01291 *                                                          *      ELTPAN  
01292 *        GENERATE DEDUCTIBLE                               *      ELTPAN  
01293 *                                                          *      ELTPAN  
01294 ************************************************************      ELTPAN  
01295  GENERATE-DEDUCTIBLE.                                             ELTPAN  
01296      EXEC CICS LINK                                               ELTPAN  
01297                PROGRAM ('ELGADLCC')                               ELTPAN  
01298                COMMAREA (DFHCOMMAREA)                             ELTPAN  
01299         END-EXEC.                                                 ELTPAN  
01300                                                                   ELTPAN  
01301 ************************************************************      ELTPAN  
01302 *                                                          *      ELTPAN  
01303 *        GENERATE CO PAYS                                  *      ELTPAN  
01304 *                                                          *      ELTPAN  
01305 ************************************************************      ELTPAN  
01306  GENERATE-COPAY.                                                  ELTPAN  
01307      EXEC CICS LINK                                               ELTPAN  
01308                PROGRAM ('ELGACPCC')                               ELTPAN  
01309                COMMAREA (DFHCOMMAREA)                             ELTPAN  
01310         END-EXEC.                                                 ELTPAN  
01311                                                                   ELTPAN  
01312 ************************************************************      ELTPAN  
01313 *                                                          *      ELTPAN  
01314 *        GENERATE MAXIMUM                                  *      ELTPAN  
01315 *                                                          *      ELTPAN  
01316 ************************************************************      ELTPAN  
01317  GENERATE-MAXIMUM.                                                ELTPAN  
01318      EXEC CICS LINK                                               ELTPAN  
01319                PROGRAM ('ELGABMCC')                               ELTPAN  
01320                COMMAREA (DFHCOMMAREA)                             ELTPAN  
01321         END-EXEC.                                                 ELTPAN  
01322      EJECT                                                        ELTPAN  
01323                                                                   ELTPAN  
01324 ************************************************************      ELTPAN  
01325 *                                                          *      ELTPAN  
01326 *        GENERATE OUT OF POCKET                            *      ELTPAN  
01327 *                                                          *      ELTPAN  
01328 ************************************************************      ELTPAN  
01329  GENERATE-OUT-OF-POCKET.                                          ELTPAN  
01330      EXEC CICS LINK                                               ELTPAN  
01331                PROGRAM ('ELGAOLCC')                               ELTPAN  
01332                COMMAREA (DFHCOMMAREA)                             ELTPAN  
01333         END-EXEC.                                                 ELTPAN  
01334                                                                   ELTPAN  
01335 ************************************************************      ELTPAN  
01336 *                                                          *      ELTPAN  
01337 *        CALL CBRI INTERFACE                               *      ELTPAN  
01338 *                                                          *      ELTPAN  
01339 ************************************************************      ELTPAN  
01340  CALL-CBRI-INTERFACE.                                             ELTPAN  
01341      CALL 'ELGCBRI' USING DFHEIBLK                                ELTPAN  
01342                           DFHCOMMAREA.                            ELTPAN  
01343                                                                   ELTPAN  
01344 ************************************************************      ELTPAN  
01345 *                                                          *      ELTPAN  
01346 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTPAN  
01347 *                                                          *      ELTPAN  
01348 ************************************************************      ELTPAN  
01349  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTPAN  
01350      PERFORM INITIALIZE-CMOUT.                                    ELTPAN  
01351      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTPAN  
01352      EJECT                                                        ELTPAN  
01353                                                                   ELTPAN  
01354 ************************************************************      ELTPAN  
01355 *                                                          *      ELTPAN  
01356 *        PREPARE TEXT FOR OUTPUT                           *      ELTPAN  
01357 *                                                          *      ELTPAN  
01358 ************************************************************      ELTPAN  
01359  PREPARE-TEXT-FOR-OUTPUT.                                         ELTPAN  
01360      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTPAN  
01361          UNTIL CMF-DESCR-IDX                                      ELTPAN  
01362                                    GREATER THAN                   ELTPAN  
01363              CMF-NBR-DESCR-LINES.                                 ELTPAN  
01364      EJECT                                                        ELTPAN  
01365                                                                   ELTPAN  
01366 ************************************************************      ELTPAN  
01367 *                                                          *      ELTPAN  
01368 *        INITIALIZE CMOUT                                  *      ELTPAN  
01369 *                                                          *      ELTPAN  
01370 ************************************************************      ELTPAN  
01371  INITIALIZE-CMOUT.                                                ELTPAN  
01372      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTPAN  
01373      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAN  
01374          ADDRESS OF CMF-DESCR.                                    ELTPAN  
01375      SET CMF-DESCR-IDX TO 1.                                      ELTPAN  
01376      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTPAN  
01377                                                                   ELTPAN  
01378                                                                   ELTPAN  
01379 ************************************************************      ELTPAN  
01380 *                                                          *      ELTPAN  
01381 *        MOVE CMF TEXT TO OUTPUT                           *      ELTPAN  
01382 *                                                          *      ELTPAN  
01383 ************************************************************      ELTPAN  
01384  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTPAN  
01385      PERFORM MOVE-A-LINE.                                         ELTPAN  
01386      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTPAN  
01387          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTPAN  
01388      IF TCAR-FROM-SUB GREATER THAN 20                             ELTPAN  
01389               OR CMF-DESCR-IDX GREATER THAN                       ELTPAN  
01390          CMF-NBR-DESCR-LINES                                      ELTPAN  
01391          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTPAN  
01392                                                                   ELTPAN  
01393                                                                   ELTPAN  
01394 ************************************************************      ELTPAN  
01395 *                                                          *      ELTPAN  
01396 *        FINISH CODES MANUAL TEXT                          *      ELTPAN  
01397 *                                                          *      ELTPAN  
01398 ************************************************************      ELTPAN  
01399  FINISH-CODES-MANUAL-TEXT.                                        ELTPAN  
01400      SET DONE-PROCESSING TO TRUE.                                 ELTPAN  
01401      IF PERIOD-NEEDED                                             ELTPAN  
01402          PERFORM GET-AND-MOVE-PERIOD.                             ELTPAN  
01403      EJECT                                                        ELTPAN  
01404                                                                   ELTPAN  
01405                                                                   ELTPAN  
01406 ************************************************************      ELTPAN  
01407 *                                                          *      ELTPAN  
01408 *        GET AND MOVE PERIOD                               *      ELTPAN  
01409 *                                                          *      ELTPAN  
01410 ************************************************************      ELTPAN  
01411  GET-AND-MOVE-PERIOD.                                             ELTPAN  
01412      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTPAN  
01413          (TCAR-FROM-SUB).                                         ELTPAN  
01414                                                                   ELTPAN  
01415                                                                   ELTPAN  
01416 ************************************************************      ELTPAN  
01417 *                                                          *      ELTPAN  
01418 *        SAVE LAST LINE                                    *      ELTPAN  
01419 *                                                          *      ELTPAN  
01420 ************************************************************      ELTPAN  
01421  SAVE-LAST-LINE.                                                  ELTPAN  
01422      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAN  
01423      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTPAN  
01424         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTPAN  
01425      ADD 1 TO TCAR-FROM-SUB.                                      ELTPAN  
01426      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTPAN  
01427                                                                   ELTPAN  
01428                                                                   ELTPAN  
01429 ************************************************************      ELTPAN  
01430 *                                                          *      ELTPAN  
01431 *        OUTPUT LAST LINE                                  *      ELTPAN  
01432 *                                                          *      ELTPAN  
01433 ************************************************************      ELTPAN  
01434  OUTPUT-LAST-LINE.                                                ELTPAN  
01435      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTPAN  
01436          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTPAN  
01437      IF BLANK-LINE-NEEDED                                         ELTPAN  
01438          PERFORM CREATE-A-BLANK-LINE.                             ELTPAN  
01439                                                                   ELTPAN  
01440                                                                   ELTPAN  
01441 ************************************************************      ELTPAN  
01442 *                                                          *      ELTPAN  
01443 *        CREATE A BLANK LINE                               *      ELTPAN  
01444 *                                                          *      ELTPAN  
01445 ************************************************************      ELTPAN  
01446  CREATE-A-BLANK-LINE.                                             ELTPAN  
01447      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTPAN  
01448      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPAN  
01449                                                                   ELTPAN  
01450                                                                   ELTPAN  
01451 ************************************************************      ELTPAN  
01452 *                                                          *      ELTPAN  
01453 *        MOVE A LINE                                       *      ELTPAN  
01454 *                                                          *      ELTPAN  
01455 ************************************************************      ELTPAN  
01456  MOVE-A-LINE.                                                     ELTPAN  
01457      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTPAN  
01458          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTPAN  
01459      SET CMF-DESCR-IDX UP BY 1.                                   ELTPAN  
01460      ADD 1 TO TCAR-FROM-SUB.                                      ELTPAN  
01461      EJECT                                                        ELTPAN  
01462                                                                   ELTPAN  
01463                                                                   ELTPAN  
01464 ************************************************************      ELTPAN  
01465 *                                                          *      ELTPAN  
01466 *        REFORMAT AND WRITE TEXT                           *      ELTPAN  
01467 *                                                          *      ELTPAN  
01468 ************************************************************      ELTPAN  
01469  REFORMAT-AND-WRITE-TEXT.                                         ELTPAN  
01470      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTPAN  
01471      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTPAN  
01472      PERFORM UNSTRING-TEXT.                                       ELTPAN  
01473      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAN  
01474      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAN  
01475      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTPAN  
01476          UNTIL COF-NBR-DTL-LINES GREATER                          ELTPAN  
01477                                   TCAR-OUTPUT-FIELDS-USED -       ELTPAN  
01478              1.                                                   ELTPAN  
01479      PERFORM DISPOSE-OF-LAST-LINE.                                ELTPAN  
01480      PERFORM LINK-TO-OUTPUT.                                      ELTPAN  
01481                                                                   ELTPAN  
01482                                                                   ELTPAN  
01483 ************************************************************      ELTPAN  
01484 *                                                          *      ELTPAN  
01485 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTPAN  
01486 *                                                          *      ELTPAN  
01487 ************************************************************      ELTPAN  
01488  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTPAN  
01489      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTPAN  
01490           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTPAN  
01491      ADD +1 TO TCAR-FROM-SUB.                                     ELTPAN  
01492      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPAN  
01493      EJECT                                                        ELTPAN  
01494                                                                   ELTPAN  
01495                                                                   ELTPAN  
01496 ************************************************************      ELTPAN  
01497 *                                                          *      ELTPAN  
01498 *        UNSTRING TEXT                                     *      ELTPAN  
01499 *                                                          *      ELTPAN  
01500 ************************************************************      ELTPAN  
01501  UNSTRING-TEXT.                                                   ELTPAN  
01502      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTPAN  
01503      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTPAN  
01504      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTPAN  
01505      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTPAN  
01506      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTPAN  
01507      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTPAN  
01508      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTPAN  
01509      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTPAN  
01510      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTPAN  
01511      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTPAN  
01512      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTPAN  
01513      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTPAN  
01514      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTPAN  
01515      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTPAN  
01516      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTPAN  
01517      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTPAN  
01518      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTPAN  
01519      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTPAN  
01520      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTPAN  
01521      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTPAN  
01522      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTPAN  
01523      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTPAN  
01524      EJECT                                                        ELTPAN  
01525                                                                   ELTPAN  
01526                                                                   ELTPAN  
01527 ************************************************************      ELTPAN  
01528 *                                                          *      ELTPAN  
01529 *        LINK TO OUTPUT                                    *      ELTPAN  
01530 *                                                          *      ELTPAN  
01531 ************************************************************      ELTPAN  
01532  LINK-TO-OUTPUT.                                                  ELTPAN  
01533      EXEC CICS LINK                                               ELTPAN  
01534          PROGRAM ('ELUOUTPT')                                     ELTPAN  
01535          COMMAREA (DFHCOMMAREA)                                   ELTPAN  
01536          END-EXEC.                                                ELTPAN  
01537      EJECT                                                        ELTPAN  
01538                                                                   ELTPAN  
01539                                                                   ELTPAN  
01540 ************************************************************      ELTPAN  
01541 *                                                          *      ELTPAN  
01542 *        DISPOSE OF LAST LINE                              *      ELTPAN  
01543 *                                                          *      ELTPAN  
01544 ************************************************************      ELTPAN  
01545  DISPOSE-OF-LAST-LINE.                                            ELTPAN  
01546      IF NOT ADDITIONAL-TEXT                                       ELTPAN  
01547          PERFORM INITIALIZE-CONTINUED-SW.                         ELTPAN  
01548      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTPAN  
01549          PERFORM SAVE-LAST-LINE                                   ELTPAN  
01550      ELSE                                                         ELTPAN  
01551          PERFORM OUTPUT-LAST-LINE.                                ELTPAN  
01552                                                                   ELTPAN  
01553                                                                   ELTPAN  
01554 ************************************************************      ELTPAN  
01555 *                                                          *      ELTPAN  
01556 *        INITIALIZE CONTINUED SW                           *      ELTPAN  
01557 *                                                          *      ELTPAN  
01558 ************************************************************      ELTPAN  
01559  INITIALIZE-CONTINUED-SW.                                         ELTPAN  
01560      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTPAN  
01561                                                                   ELTPAN  
01562 *                                                          *      ELTPAN  
01563 *        MOVE TRANSLATION TO OUTPUT                        *      ELTPAN  
01564 *                                                          *      ELTPAN  
01565 ************************************************************      ELTPAN  
01566  MOVE-TRANS-TO-OUTPUT.                                            ELTPAN  
01567      PERFORM VARYING CMF-DESCR-IDX FROM 1 BY 1 UNTIL              ELTPAN  
01568         CMF-DESCR-IDX > CMF-NBR-DESCR-LINES                       ELTPAN  
01569          MOVE CMF-DESCR-LINE(CMF-DESCR-IDX)                       ELTPAN  
01570             TO COF-DTL-LINE(COF-NBR-DTL-LINES)                    ELTPAN  
01571          ADD 1 TO COF-NBR-DTL-LINES                               ELTPAN  
01572      END-PERFORM.                                                 ELTPAN  
