00001 *      LAST MAINTENANCE TIME:  7.54.00  DATE: 06/13/91            09/03/03
00002  IDENTIFICATION DIVISION.                                         ELTBAE  
00003                                                                      LV002
00004  PROGRAM-ID.         ELTBAE.                                      ELTBAE  
00005                                                                   ELTBAE  
00006  AUTHOR.             ANNE KEFFER KING.                            ELTBAE  
00007                      CLONED FORM ELTRPO.                          ELTBAE  
00008                                                                   ELTBAE  
00009  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTBAE  
00010                      A MUTUAL LEGAL RESERVE COMPANY               ELTBAE  
00011                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTBAE  
00012                      233 N. MICHIGAN AVE                          ELTBAE  
00013                      CHICAGO, ILLINOIS 60601                      ELTBAE  
00014                                                                   ELTBAE  
00015  DATE-WRITTEN.       24-MAR-1999.                                 ELTBAE  
00016                                                                   ELTBAE  
00017  DATE-COMPILED.                                                   ELTBAE  
00018                                                                   ELTBAE  
00019  SECURITY.           COPYRIGHT 1986,                              ELTBAE  
00020                      HEALTH CARE SERVICE CORPORATION              ELTBAE  
00021      SKIP3                                                        ELTBAE  
00022  ENVIRONMENT DIVISION.                                            ELTBAE  
00023                                                                   ELTBAE  
00024  CONFIGURATION SECTION.                                           ELTBAE  
00025  SOURCE-COMPUTER.    IBM-3090.                                    ELTBAE  
00026  OBJECT-COMPUTER.    IBM-3090.                                    ELTBAE  
00027      EJECT                                                        ELTBAE  
00028 ******************************************************************ELTBAE  
00029 *                                                                *ELTBAE  
00030 *    COPYBOOK:   ELTBAE                                          *ELTBAE  
00031 *    DATE:       23-MAR-1999                                     *ELTBAE  
00032 *    AUTHOR:     ANNE KING                                       *ELTBAE  
00033 *    FUNCTION:   THIS MODULE WILL DISPLAY ALL OUTPUT ASSOCIATED  *ELTBAE  
00034 *                WITH BLUE ADVANTAGE ENTREPRENUER PROGRAM        *ELTBAE  
00035 *    NOTES:      X---                                            *ELTBAE  
00036 *                                                                *ELTBAE  
00037 ******************************************************************ELTBAE  
00038 *                                                                *ELTBAE  
00039 *                      MAINTENANCE HISTORY                       *ELTBAE  
00040 *                                                                *ELTBAE  
00041 *  MOD     DATE     BY  DRPT                ACTION               *ELTBAE  
00042 * ----- ----------- --- ----- ---------------------------------- *ELTBAE  
00043 * 01.00 23-MAR-1999 AKK       CREATED.                           *ELTBAE  
00044 *                                                                 ELTBAE  
00045 *       13-AUG-2003 AKK       TESTING FOR COMPILE LEVEL          *ELTBAE  
00046 ******************************************************************ELTBAE  
00047                                                                   ELTBAE  
00048  DATA DIVISION.                                                   ELTBAE  
00049                                                                   ELTBAE  
00050  WORKING-STORAGE SECTION.                                         ELTBAE  
00051  01  WS-MISC.                                                     ELTBAE  
00052      05  WS-BEGIN                        PIC X(26) VALUE          ELTBAE  
00053      '*** ELTBAE WS BEGINS ***'.                                  ELTBAE  
00054      05  WS-POINTER2                     POINTER.                 ELTBAE  
00055      05  WS-POINTER3                     POINTER.                 ELTBAE  
00056                                                                   ELTBAE  
00057  01  WS-SWITCHES.                                                 ELTBAE  
00058      05  ADDITIONAL-TEXT-SWITCH   PIC X(01) VALUE SPACE.          ELTBAE  
00059          88  ADDITIONAL-TEXT                VALUE 'A'.            ELTBAE  
00060          88  BLANK-LINE-NEEDED              VALUE 'B'.            ELTBAE  
00061      05  CONTINUED-PROCESSING-SW  PIC X(01) VALUE SPACE.          ELTBAE  
00062          88  DONE-PROCESSING                VALUE 'D'.            ELTBAE  
00063          88  PROCESSING-CMF-TEXT            VALUE 'P'.            ELTBAE  
00064      05  WS-TABULAR-SWITCH        PIC X(01) VALUE 'N'.            ELTBAE  
00065          88  TABULAR-IS-UNDEFINED           VALUE 'Y'.            ELTBAE  
00066      05  WS-PERIOD-SWITCH         PIC X(01) VALUE 'N'.            ELTBAE  
00067          88  PERIOD-NEEDED                  VALUE 'Y'.            ELTBAE  
00068      05  WS-APPROVAL-SOURCE-SW    PIC X     VALUE SPACE.          ELTBAE  
00069          88  NOT-HOLDING-APPROVAL-SRCE      VALUE 'N'.            ELTBAE  
00070          88  HOLDING-APPROVAL-SOURCE        VALUE 'H'.            ELTBAE  
00071                                                                   ELTBAE  
00072  01  WS-HOLD-AREA.                                                ELTBAE  
00073      05  WS-GBAE-PROV-SLOT-NO   COMP-3 PIC S9(07) VALUE +0.       ELTBAE  
00074                                                                   ELTBAE  
00075 **************************************************************    ELTBAE  
00076 ***                   PROGRAM CONSTANTS                           ELTBAE  
00077 **************************************************************    ELTBAE  
00078      05  WS-GRP                   PIC X(06) VALUE 'GROUP'.        ELTBAE  
00079      05  WS-GCCP                  PIC X(06) VALUE '#GCCP '.       ELTBAE  
00080      05  WS-GBAE                  PIC X(06) VALUE '#GBAE '.       ELTBAE  
00081      05  WS-INST                  PIC X(13) VALUE 'INSTITUTIONAL'.ELTBAE  
00082      05  WS-PROF                  PIC X(13) VALUE 'PROFESSIONAL '.ELTBAE  
00083      05  WS-SUPP                  PIC X(13) VALUE 'SUPPLEMENTAL '.ELTBAE  
00084      05  WS-APPROVAL              PIC X(09) VALUE 'APPROVAL.'.    ELTBAE  
00085                                                                   ELTBAE  
00086 **************************************************************    ELTBAE  
00087 ***                   HEADER  LINE                                ELTBAE  
00088 **************************************************************    ELTBAE  
00089      05  WS-HEADER-LINE.                                          ELTBAE  
00090          10  FILLER               PIC X(14) VALUE SPACES.         ELTBAE  
00091          10  FILLER               PIC X(36) VALUE                 ELTBAE  
00092          'BLUE ADVANTAGE ENTREPRENEUR PROGRAM '.                  ELTBAE  
00093          10  WS-HDR-LINE-BCBSMM   PIC X(13) VALUE SPACES.         ELTBAE  
00094          10  FILLER               PIC X(16) VALUE SPACES.         ELTBAE  
00095                                                                   ELTBAE  
00096 **************************************************************    ELTBAE  
00097 ***                   SCREEN BODY LINES                           ELTBAE  
00098 **************************************************************    ELTBAE  
00099  01  WS-SCREEN-LINE-AREA.                                         ELTBAE  
00100      05  WS-APPRVL-LINE.                                          ELTBAE  
00101          10  FILLER               PIC X(45) VALUE                 ELTBAE  
00102          'BLUE ADVANTAGE ENTREPRENEUR PROGRAM REQUIRES '.         ELTBAE  
00103          10  FILLER               PIC X(34) VALUE SPACES.         ELTBAE  
00104                                                                   ELTBAE  
00105      05  WS-ALT-PRICING-LINE-BC.                                  ELTBAE  
00106          10  FILLER               PIC X(52) VALUE                 ELTBAE  
00107          'THE ALTERNATE PRICING FOR INSTITUTIONAL SERVICES IS '.  ELTBAE  
00108          10  FILLER               PIC X(27) VALUE SPACES.         ELTBAE  
00109                                                                   ELTBAE  
00110      05  WS-ALT-PRICING-LINE-BS.                                  ELTBAE  
00111          10  FILLER               PIC X(51) VALUE                 ELTBAE  
00112          'THE ALTERNATE PRICING FOR PROFESSIONAL SERVICES IS '.   ELTBAE  
00113          10  FILLER               PIC X(28) VALUE SPACES.         ELTBAE  
00114                                                                   ELTBAE  
00115      05  WS-BENE-REDUCT-LINE.                                     ELTBAE  
00116          10  FILLER               PIC X(47) VALUE                 ELTBAE  
00117          'DENIED OR REDUCED BENEFITS DUE TO THIS PROGRAM:'.       ELTBAE  
00118          10  FILLER               PIC X(32) VALUE SPACES.         ELTBAE  
00119                                                                   ELTBAE  
00120      05  WS-SPILL-OVER-LINE.                                      ELTBAE  
00121          10  FILLER               PIC X(51) VALUE                 ELTBAE  
00122          'UNPAID SERVICES AFTER BASIC BENEFITS REDUCTION ARE '.   ELTBAE  
00123          10  FILLER               PIC X(28) VALUE SPACES.         ELTBAE  
00124                                                                   ELTBAE  
00125 **************************************************************    ELTBAE  
00126 ** SPECIAL MESSAGE FOR THE NOT APPLICABLE                         ELTBAE  
00127 ** ALSO THE FIXED TEXT FOR TABULAR GBAE                           ELTBAE  
00128 **************************************************************    ELTBAE  
00129      05  WS-BAE-APPLIES.                                          ELTBAE  
00130          10  FILLER               PIC X(79) VALUE                 ELTBAE  
00131          'THE BLUE ADVANTAGE ENTREPRENEUR PROGRAM APPLIES TO'.    ELTBAE  
00132                                                                   ELTBAE  
00133      05  WS-NOT-APPLICABLE-MSG.                                   ELTBAE  
00134          10  FILLER               PIC X(40) VALUE                 ELTBAE  
00135          'THE BLUE ADVANTAGE ENTREPRENEUR PROGRAM '.              ELTBAE  
00136          10  FILLER               PIC X(18) VALUE                 ELTBAE  
00137          'IS NOT APPLICABLE.'.                                    ELTBAE  
00138          10  FILLER               PIC X(21) VALUE SPACES.         ELTBAE  
00139                                                                   ELTBAE  
00140      05  WS-SPEC-PROV-MSG.                                        ELTBAE  
00141          10  FILLER               PIC X(79) VALUE                 ELTBAE  
00142          'THERE ARE SPECIAL PROVIDERS INCLUDED IN THIS COST CONTAIELTBAE  
00143 -        'NMENT PROGRAM.'.                                        ELTBAE  
00144                                                                   ELTBAE  
00145      05  WS-DISCLAIMER-MSG.                                       ELTBAE  
00146          10  FILLER               PIC X(79) VALUE                 ELTBAE  
00147          '*** SUBJECT TO OTHER CONTRACT LIMITATIONS ***'.         ELTBAE  
00148                                                                   ELTBAE  
00149      05  WS-NOT-APPLY-TO-LOB-BC.                                  ELTBAE  
00150          10  FILLER               PIC X(51) VALUE                 ELTBAE  
00151          'BLUE ADVANTAGE ENTREPRENEUR PROGRAM DOES NOT APPLY '.   ELTBAE  
00152          10  FILLER               PIC X(29) VALUE                 ELTBAE  
00153          'FOR INSTITUTIONAL BENEFITS'.                            ELTBAE  
00154                                                                   ELTBAE  
00155      05  WS-NOT-APPLY-TO-LOB-BS.                                  ELTBAE  
00156          10  FILLER               PIC X(51) VALUE                 ELTBAE  
00157          'BLUE ADVANTAGE ENTREPRENEUR PROGRAM DOES NOT APPLY '.   ELTBAE  
00158          10  FILLER               PIC X(29) VALUE                 ELTBAE  
00159          'FOR PROFESSIONAL BENEFITS.'.                            ELTBAE  
00160                                                                   ELTBAE  
00161      05  WS-NOT-APPLY-TO-LOB-MM.                                  ELTBAE  
00162          10  FILLER               PIC X(51) VALUE                 ELTBAE  
00163          'BLUE ADVANTAGE ENTREPRENEUR PROGRAM DOES NOT APPLY '.   ELTBAE  
00164          10  FILLER               PIC X(29) VALUE                 ELTBAE  
00165          'FOR SUPPLEMENTAL BENEFITS.'.                            ELTBAE  
00166                                                                   ELTBAE  
00167  LINKAGE SECTION.                                                 ELTBAE  
00168  01  DFHCOMMAREA.                                                 ELTBAE  
00169      COPY ELSCOMMC.                                               ELTBAE  
00170 /                                                                 ELTBAE  
00171      COPY ELSCIA2C.                                               ELTBAE  
00172 /                                                                 ELTBAE  
00173      COPY ELSCMDSC.                                               ELTBAE  
00174 /                                                                 ELTBAE  
00175      COPY ELSCMIFC.                                               ELTBAE  
00176 /                                                                 ELTBAE  
00177      COPY ELSIOPMC.                                               ELTBAE  
00178 /                                                                 ELTBAE  
00179      COPY ELSKEYSC.                                               ELTBAE  
00180 /                                                                 ELTBAE  
00181      COPY ELSOUTPC.                                               ELTBAE  
00182 /                                                                 ELTBAE  
00183      COPY ELSSRTPC.                                               ELTBAE  
00184 /                                                                 ELTBAE  
00185      COPY ELSTCWAC.                                               ELTBAE  
00186 /                                                                 ELTBAE  
00187      COPY ELSSSCBC.                                               ELTBAE  
00188 /                                                                 ELTBAE  
00189  01  GROUP-SPECIFIC-REC.                                          ELTBAE  
00190      COPY GCGROUPC.                                               ELTBAE  
00191 /                                                                 ELTBAE  
00192  01  GCCP-TABULAR-REC-AREA.                                       ELTBAE  
00193      COPY GCTGCCPC.                                               ELTBAE  
00194      EJECT                                                        ELTBAE  
00195  PROCEDURE DIVISION.                                              ELTBAE  
00196 ************************************************************      ELTBAE  
00197 *                                                          *      ELTBAE  
00198 *                    PROCEDURE DIVISION                    *      ELTBAE  
00199 *                                                          *      ELTBAE  
00200 ************************************************************      ELTBAE  
00201                                                                   ELTBAE  
00202                                                                   ELTBAE  
00203 ************************************************************      ELTBAE  
00204 *                                                          *      ELTBAE  
00205 *        BLUE ADVANTAGE ENTREPRENEUR PROGRAM               *      ELTBAE  
00206 *                                                          *      ELTBAE  
00207 ************************************************************      ELTBAE  
00208  BLUE-ADVANTAGE-ENTRE-OPTION.                                     ELTBAE  
00209      PERFORM INITIALIZATION.                                      ELTBAE  
00210      PERFORM PROCESS.                                             ELTBAE  
00211      GOBACK.                                                      ELTBAE  
00212                                                                   ELTBAE  
00213 ************************************************************      ELTBAE  
00214 *                                                          *      ELTBAE  
00215 *        INITIALIZATION                                    *      ELTBAE  
00216 *                                                          *      ELTBAE  
00217 ************************************************************      ELTBAE  
00218  INITIALIZATION.                                                  ELTBAE  
00219      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTBAE  
00220      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTBAE  
00221                                                                   ELTBAE  
00222 ************************************************************      ELTBAE  
00223 *                                                          *      ELTBAE  
00224 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTBAE  
00225 *                                                          *      ELTBAE  
00226 ************************************************************      ELTBAE  
00227  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTBAE  
00228      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTBAE  
00229      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTBAE  
00230      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTBAE  
00231                                                                   ELTBAE  
00232 ************************************************************      ELTBAE  
00233 *                                                          *      ELTBAE  
00234 *        CHECK FOR VALID COMMAREA                          *      ELTBAE  
00235 *                                                          *      ELTBAE  
00236 ************************************************************      ELTBAE  
00237  CHECK-FOR-VALID-COMMAREA.                                        ELTBAE  
00238      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTBAE  
00239          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTBAE  
00240                                                                   ELTBAE  
00241 ************************************************************      ELTBAE  
00242 *                                                          *      ELTBAE  
00243 *        SIGNAL INVALID COMMAREA                           *      ELTBAE  
00244 *                                                          *      ELTBAE  
00245 ************************************************************      ELTBAE  
00246  SIGNAL-INVALID-COMMAREA.                                         ELTBAE  
00247      EXEC CICS ABEND                                              ELTBAE  
00248                ABCODE('EL01')                                     ELTBAE  
00249         END-EXEC.                                                 ELTBAE  
00250      EJECT                                                        ELTBAE  
00251                                                                   ELTBAE  
00252 ************************************************************      ELTBAE  
00253 *                                                          *      ELTBAE  
00254 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTBAE  
00255 *                                                          *      ELTBAE  
00256 ************************************************************      ELTBAE  
00257  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTBAE  
00258      IF ECA-CIA-PTR = NULL                                        ELTBAE  
00259          PERFORM SIGNAL-INVALID-CIA                               ELTBAE  
00260      ELSE                                                         ELTBAE  
00261          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTBAE  
00262                                                                   ELTBAE  
00263                                                                   ELTBAE  
00264 ************************************************************      ELTBAE  
00265 *                                                          *      ELTBAE  
00266 *        SIGNAL INVALID CIA                                *      ELTBAE  
00267 *                                                          *      ELTBAE  
00268 ************************************************************      ELTBAE  
00269  SIGNAL-INVALID-CIA.                                              ELTBAE  
00270      EXEC CICS ABEND                                              ELTBAE  
00271                ABCODE('EL02')                                     ELTBAE  
00272         END-EXEC.                                                 ELTBAE  
00273                                                                   ELTBAE  
00274 ************************************************************      ELTBAE  
00275 *                                                          *      ELTBAE  
00276 *        ESTABLISH ADDRESS OF CIA                          *      ELTBAE  
00277 *                                                          *      ELTBAE  
00278 ************************************************************      ELTBAE  
00279  ESTABLISH-ADDRESS-OF-CIA.                                        ELTBAE  
00280      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTBAE  
00281                            ADDRESS OF                             ELTBAE  
00282          CIA-ELS-COMMON-INTERFACE-AREA.                           ELTBAE  
00283      EJECT                                                        ELTBAE  
00284                                                                   ELTBAE  
00285 ************************************************************      ELTBAE  
00286 *                                                          *      ELTBAE  
00287 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTBAE  
00288 *                                                          *      ELTBAE  
00289 ************************************************************      ELTBAE  
00290  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTBAE  
00291      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTBAE  
00292      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBAE  
00293                            ADDRESS OF                             ELTBAE  
00294          SSB-SELECTOR-STATUS-CTL-BLK.                             ELTBAE  
00295      IF CIA-RC-PTR-NULL                                           ELTBAE  
00296          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTBAE  
00297                                                                   ELTBAE  
00298 ************************************************************      ELTBAE  
00299 *                                                          *      ELTBAE  
00300 *        SIGNAL UNALLOC AREA ERROR                         *      ELTBAE  
00301 *                                                          *      ELTBAE  
00302 ************************************************************      ELTBAE  
00303  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTBAE  
00304      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTBAE  
00305      PERFORM SIGNAL-ABEND.                                        ELTBAE  
00306                                                                   ELTBAE  
00307 ************************************************************      ELTBAE  
00308 *                                                          *      ELTBAE  
00309 *        SIGNAL ABEND                                      *      ELTBAE  
00310 *                                                          *      ELTBAE  
00311 ************************************************************      ELTBAE  
00312  SIGNAL-ABEND.                                                    ELTBAE  
00313      EXEC CICS ABEND                                              ELTBAE  
00314                ABCODE(CIA-ABCODE)                                 ELTBAE  
00315         END-EXEC.                                                 ELTBAE  
00316      EJECT                                                        ELTBAE  
00317                                                                   ELTBAE  
00318 ************************************************************      ELTBAE  
00319 *                                                          *      ELTBAE  
00320 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTBAE  
00321 *                                                          *      ELTBAE  
00322 ************************************************************      ELTBAE  
00323  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTBAE  
00324      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTBAE  
00325      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTBAE  
00326      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTBAE  
00327      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTBAE  
00328      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTBAE  
00329      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTBAE  
00330      PERFORM ESTABLISH-ADDRESSABILITY-OF-CS.                      ELTBAE  
00331                                                                   ELTBAE  
00332 ************************************************************      ELTBAE  
00333 *                                                          *      ELTBAE  
00334 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTBAE  
00335 *                                                          *      ELTBAE  
00336 ************************************************************      ELTBAE  
00337  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTBAE  
00338      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTBAE  
00339      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBAE  
00340                            ADDRESS OF                             ELTBAE  
00341          CMF-CODES-MANUAL-INTERFACE.                              ELTBAE  
00342      IF CIA-RC-PTR-NULL                                           ELTBAE  
00343          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTBAE  
00344      EJECT                                                        ELTBAE  
00345                                                                   ELTBAE  
00346 ************************************************************      ELTBAE  
00347 *                                                          *      ELTBAE  
00348 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTBAE  
00349 *                                                          *      ELTBAE  
00350 ************************************************************      ELTBAE  
00351  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTBAE  
00352      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTBAE  
00353      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBAE  
00354                            ADDRESS OF                             ELTBAE  
00355          COF-OUTPUT-INTERFACE.                                    ELTBAE  
00356      IF CIA-RC-PTR-NULL                                           ELTBAE  
00357          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTBAE  
00358      EJECT                                                        ELTBAE  
00359                                                                   ELTBAE  
00360 ************************************************************      ELTBAE  
00361 *                                                          *      ELTBAE  
00362 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTBAE  
00363 *                                                          *      ELTBAE  
00364 ************************************************************      ELTBAE  
00365  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTBAE  
00366      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTBAE  
00367      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBAE  
00368                            ADDRESS OF                             ELTBAE  
00369          SRP-SUBROUTINE-PARAMETERS.                               ELTBAE  
00370      IF CIA-RC-PTR-NULL                                           ELTBAE  
00371          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTBAE  
00372      EJECT                                                        ELTBAE  
00373                                                                   ELTBAE  
00374 ************************************************************      ELTBAE  
00375 *                                                          *      ELTBAE  
00376 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTBAE  
00377 *                                                          *      ELTBAE  
00378 ************************************************************      ELTBAE  
00379  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTBAE  
00380      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTBAE  
00381      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBAE  
00382                            ADDRESS OF                             ELTBAE  
00383          TCAR-COMPRESSION-WORK-AREA.                              ELTBAE  
00384      IF CIA-RC-PTR-NULL                                           ELTBAE  
00385          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTBAE  
00386      EJECT                                                        ELTBAE  
00387                                                                   ELTBAE  
00388 ************************************************************      ELTBAE  
00389 *                                                          *      ELTBAE  
00390 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTBAE  
00391 *                                                          *      ELTBAE  
00392 ************************************************************      ELTBAE  
00393  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTBAE  
00394      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTBAE  
00395      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBAE  
00396                            ADDRESS OF                             ELTBAE  
00397          KWA-FILE-KEY-WORK-AREA.                                  ELTBAE  
00398      IF CIA-RC-PTR-NULL                                           ELTBAE  
00399          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTBAE  
00400      EJECT                                                        ELTBAE  
00401                                                                   ELTBAE  
00402 ************************************************************      ELTBAE  
00403 *                                                          *      ELTBAE  
00404 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELTBAE  
00405 *                                                          *      ELTBAE  
00406 ************************************************************      ELTBAE  
00407  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTBAE  
00408      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTBAE  
00409      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBAE  
00410                            ADDRESS OF                             ELTBAE  
00411          GROUP-SPECIFIC-REC.                                      ELTBAE  
00412      IF CIA-RC-PTR-NULL                                           ELTBAE  
00413          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTBAE  
00414      EJECT                                                        ELTBAE  
00415                                                                   ELTBAE  
00416 ************************************************************      ELTBAE  
00417 *                                                          *      ELTBAE  
00418 *        ESTABLISH ADDRESSABILITY OF CST CONTAINMENT PROGRA*      ELTBAE  
00419 *                                                          *      ELTBAE  
00420 ************************************************************      ELTBAE  
00421  ESTABLISH-ADDRESSABILITY-OF-CS.                                  ELTBAE  
00422      SET CIA-GCTABULR-DDN TO TRUE.                                ELTBAE  
00423      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBAE  
00424                            ADDRESS OF                             ELTBAE  
00425          GCCP-TABULAR-REC-AREA.                                   ELTBAE  
00426      IF CIA-RC-PTR-NULL                                           ELTBAE  
00427          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTBAE  
00428      EJECT                                                        ELTBAE  
00429                                                                   ELTBAE  
00430 ************************************************************      ELTBAE  
00431 *                                                          *      ELTBAE  
00432 *        PROCESS                                           *      ELTBAE  
00433 *                                                          *      ELTBAE  
00434 ************************************************************      ELTBAE  
00435  PROCESS.                                                         ELTBAE  
00436      IF GCG-BAE-INDICATOR = ZERO                                  ELTBAE  
00437          PERFORM GENERATE-NOT-APPLICABLE-MESSAG                   ELTBAE  
00438      ELSE                                                         ELTBAE  
00439          PERFORM GENERATE-BAE-TEXT.                               ELTBAE  
00440      PERFORM TERMINATE-OUTPUT.                                    ELTBAE  
00441                                                                   ELTBAE  
00442 ************************************************************      ELTBAE  
00443 *                                                          *      ELTBAE  
00444 *        EJECT NEW PAGE                                    *      ELTBAE  
00445 *                                                          *      ELTBAE  
00446 ************************************************************      ELTBAE  
00447  EJECT-NEW-PAGE.                                                  ELTBAE  
00448      SET COF-NEW-PAGE      TO TRUE.                               ELTBAE  
00449      MOVE WS-HEADER-LINE   TO COF-HDR-LINE                        ELTBAE  
00450          (COF-NBR-HDR-LINES).                                     ELTBAE  
00451      PERFORM LINK-TO-OUTPUT.                                      ELTBAE  
00452      EJECT                                                        ELTBAE  
00453                                                                   ELTBAE  
00454 ************************************************************      ELTBAE  
00455 *                                                          *      ELTBAE  
00456 *        TRANSLATE AND DISPLAY BAE IND                     *      ELTBAE  
00457 *                                                          *      ELTBAE  
00458 ************************************************************      ELTBAE  
00459  TRANSLATE-AND-DISPLAY-BAE-IND.                                   ELTBAE  
00460      INITIALIZE TCAR-FROM-AREA.                                   ELTBAE  
00461      MOVE +1 TO TCAR-FROM-SUB.                                    ELTBAE  
00462      MOVE WS-BAE-APPLIES TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELTBAE  
00463      ADD  +1 TO TCAR-FROM-SUB.                                    ELTBAE  
00464      SET BLANK-LINE-NEEDED TO TRUE.                               ELTBAE  
00465      SET PERIOD-NEEDED TO TRUE.                                   ELTBAE  
00466      MOVE GCG-BAE-INDICATOR TO CMF-CODE-VALUE.                    ELTBAE  
00467      MOVE 'BAE-INDICATOR'  TO CMF-ELEMENT-SYSTEM-NAME.            ELTBAE  
00468      MOVE WS-GRP TO CMF-RECORD-PREFIX.                            ELTBAE  
00469      EXEC CICS LINK                                               ELTBAE  
00470                PROGRAM ('ELUCMIF')                                ELTBAE  
00471                COMMAREA (DFHCOMMAREA)                             ELTBAE  
00472         END-EXEC.                                                 ELTBAE  
00473      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTBAE  
00474      MOVE SPACE TO ADDITIONAL-TEXT-SWITCH.                        ELTBAE  
00475                                                                   ELTBAE  
00476 ************************************************************      ELTBAE  
00477 *                                                          *      ELTBAE  
00478 *        SET UP OUTPUT SUBSCRIPTS                          *      ELTBAE  
00479 *                                                          *      ELTBAE  
00480 ************************************************************      ELTBAE  
00481  SET-UP-OUTPUT-SUBSCRIPTS.                                        ELTBAE  
00482      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTBAE  
00483      MOVE +2 TO COF-NBR-HDR-LINES.                                ELTBAE  
00484                                                                   ELTBAE  
00485 ************************************************************      ELTBAE  
00486 *                                                          *      ELTBAE  
00487 *        GENERATE NOT APPLICABLE MESSAGE                   *      ELTBAE  
00488 *                                                          *      ELTBAE  
00489 ************************************************************      ELTBAE  
00490  GENERATE-NOT-APPLICABLE-MESSAG.                                  ELTBAE  
00491      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTBAE  
00492      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTBAE  
00493      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTBAE  
00494      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTBAE  
00495          (COF-NBR-DTL-LINES).                                     ELTBAE  
00496      PERFORM EJECT-NEW-PAGE.                                      ELTBAE  
00497      EJECT                                                        ELTBAE  
00498                                                                   ELTBAE  
00499 ************************************************************      ELTBAE  
00500 *                                                          *      ELTBAE  
00501 *        GENERATE BAE TEXT                                 *      ELTBAE  
00502 *                                                          *      ELTBAE  
00503 ************************************************************      ELTBAE  
00504  GENERATE-BAE-TEXT.                                               ELTBAE  
00505      SET WS-POINTER2 TO NULLS.                                    ELTBAE  
00506      SET WS-POINTER3 TO NULLS.                                    ELTBAE  
00507      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTBAE  
00508      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTBAE  
00509                            WS-POINTER2.                           ELTBAE  
00510      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTBAE  
00511      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTBAE  
00512                            WS-POINTER3.                           ELTBAE  
00513      PERFORM SEARCH-FOR-GCCP-TABULAR.                             ELTBAE  
00514      PERFORM DETERMINE-SELECTION.                                 ELTBAE  
00515      EJECT                                                        ELTBAE  
00516                                                                   ELTBAE  
00517 ************************************************************      ELTBAE  
00518 *                                                          *      ELTBAE  
00519 *        TERMINATE OUTPUT                                  *      ELTBAE  
00520 *                                                          *      ELTBAE  
00521 ************************************************************      ELTBAE  
00522  TERMINATE-OUTPUT.                                                ELTBAE  
00523      SET COF-END TO TRUE.                                         ELTBAE  
00524      PERFORM LINK-TO-OUTPUT.                                      ELTBAE  
00525      EJECT                                                        ELTBAE  
00526                                                                   ELTBAE  
00527 ************************************************************      ELTBAE  
00528 *                                                          *      ELTBAE  
00529 *        SEARCH FOR GCCP TABULAR                           *      ELTBAE  
00530 *                                                          *      ELTBAE  
00531 ************************************************************      ELTBAE  
00532  SEARCH-FOR-GCCP-TABULAR.                                         ELTBAE  
00533      SET GCG-INDEX TO +1.                                         ELTBAE  
00534      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTBAE  
00535         AT END                                                    ELTBAE  
00536              MOVE ZEROES TO KWA-PROVISION-SLOT-NO                 ELTBAE  
00537         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GCCP                     ELTBAE  
00538              MOVE GCG-TAB-ID (GCG-INDEX)                          ELTBAE  
00539                  TO KWA-PROVISION-ID                              ELTBAE  
00540              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTBAE  
00541                  TO KWA-PROVISION-SLOT-NO                         ELTBAE  
00542         END-SEARCH.                                               ELTBAE  
00543      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELTBAE  
00544          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTBAE  
00545      PERFORM GET-GCCP-TABULAR.                                    ELTBAE  
00546      PERFORM SEARCH-THE-GSS-ENTRY.                                ELTBAE  
00547      EJECT                                                        ELTBAE  
00548                                                                   ELTBAE  
00549 ************************************************************      ELTBAE  
00550 *                                                          *      ELTBAE  
00551 *        TRANSLATE APPROVAL SOURCE                         *      ELTBAE  
00552 *                                                          *      ELTBAE  
00553 ************************************************************      ELTBAE  
00554  TRANSLATE-APPROVAL-SOURCE.                                       ELTBAE  
00555      INITIALIZE WS-PERIOD-SWITCH.                                 ELTBAE  
00556      INITIALIZE TCAR-FROM-AREA.                                   ELTBAE  
00557      MOVE +1 TO TCAR-FROM-SUB.                                    ELTBAE  
00558      MOVE WS-APPRVL-LINE TO TCAR-FROM-LINE                        ELTBAE  
00559          (TCAR-FROM-SUB).                                         ELTBAE  
00560      ADD  +1 TO TCAR-FROM-SUB.                                    ELTBAE  
00561      SET ADDITIONAL-TEXT TO TRUE.                                 ELTBAE  
00562      IF NOT-HOLDING-APPROVAL-SRCE                                 ELTBAE  
00563          PERFORM GET-APPROVAL-TRANSLATION                         ELTBAE  
00564      ELSE                                                         ELTBAE  
00565          PERFORM USE-EXISTING-APPROVAL-TRANSLAT.                  ELTBAE  
00566      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTBAE  
00567      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTBAE  
00568      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTBAE  
00569                            WS-POINTER3.                           ELTBAE  
00570      MOVE WS-APPROVAL TO TCAR-FROM-LINE                           ELTBAE  
00571          (TCAR-FROM-SUB).                                         ELTBAE  
00572      PERFORM FINISH-SENTENCE.                                     ELTBAE  
00573      EJECT                                                        ELTBAE  
00574                                                                   ELTBAE  
00575                                                                   ELTBAE  
00576 ************************************************************      ELTBAE  
00577 *                                                          *      ELTBAE  
00578 *        GET APPROVAL TRANSLATION                          *      ELTBAE  
00579 *                                                          *      ELTBAE  
00580 ************************************************************      ELTBAE  
00581  GET-APPROVAL-TRANSLATION.                                        ELTBAE  
00582      MOVE GSS-BA-APPROVAL-SRC-IND (GSS-INDEX) TO CMF-CODE-VALUE.  ELTBAE  
00583      MOVE   'BA-APPROVAL-SRC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTBAE  
00584      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTBAE  
00585      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTBAE  
00586      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTBAE  
00587                            ADDRESS OF CMF-DESCR.                  ELTBAE  
00588      EJECT                                                        ELTBAE  
00589                                                                   ELTBAE  
00590 ************************************************************      ELTBAE  
00591 *                                                          *      ELTBAE  
00592 *        USE EXISTING APPROVAL TRANSLATION                 *      ELTBAE  
00593 *                                                          *      ELTBAE  
00594 ************************************************************      ELTBAE  
00595  USE-EXISTING-APPROVAL-TRANSLAT.                                  ELTBAE  
00596      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTBAE  
00597      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTBAE  
00598                            ADDRESS OF CMF-DESCR.                  ELTBAE  
00599      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTBAE  
00600      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTBAE  
00601                            WS-POINTER2.                           ELTBAE  
00602      EJECT                                                        ELTBAE  
00603                                                                   ELTBAE  
00604 ************************************************************      ELTBAE  
00605 *                                                          *      ELTBAE  
00606 *        FINISH SENTENCE                                   *      ELTBAE  
00607 *                                                          *      ELTBAE  
00608 ************************************************************      ELTBAE  
00609  FINISH-SENTENCE.                                                 ELTBAE  
00610      SET BLANK-LINE-NEEDED TO TRUE.                               ELTBAE  
00611      PERFORM REFORMAT-AND-WRITE-TEXT.                             ELTBAE  
00612                                                                   ELTBAE  
00613 ************************************************************      ELTBAE  
00614 *                                                          *      ELTBAE  
00615 *        TRANSLATE AND DISPLAY CODE VALUE                  *      ELTBAE  
00616 *                                                          *      ELTBAE  
00617 ************************************************************      ELTBAE  
00618  TRANSLATE-AND-DISPLAY-CODE-VAL.                                  ELTBAE  
00619      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTBAE  
00620      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTBAE  
00621                                                                   ELTBAE  
00622 ************************************************************      ELTBAE  
00623 *                                                          *      ELTBAE  
00624 *        SIGNAL UNDEFINED TABULAR ERROR                    *      ELTBAE  
00625 *                                                          *      ELTBAE  
00626 ************************************************************      ELTBAE  
00627  SIGNAL-UNDEFINED-TABULAR-ERROR.                                  ELTBAE  
00628      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTBAE  
00629      PERFORM SIGNAL-ABEND.                                        ELTBAE  
00630      EJECT                                                        ELTBAE  
00631                                                                   ELTBAE  
00632 ************************************************************      ELTBAE  
00633 *                                                          *      ELTBAE  
00634 *        DETERMINE SELECTION                               *      ELTBAE  
00635 *                                                          *      ELTBAE  
00636 ************************************************************      ELTBAE  
00637  DETERMINE-SELECTION.                                             ELTBAE  
00638      SET NOT-HOLDING-APPROVAL-SRCE TO TRUE.                       ELTBAE  
00639      IF SSB-PROV-CLASS-INST OR                                    ELTBAE  
00640                   SSB-PROV-CLASS-BOTH                             ELTBAE  
00641          PERFORM CREATE-INSTITUTIONAL-SCREEN.                     ELTBAE  
00642      IF SSB-PROV-CLASS-PROF OR                                    ELTBAE  
00643                   SSB-PROV-CLASS-BOTH                             ELTBAE  
00644          PERFORM CREATE-PROFESSIONAL-SCREEN.                      ELTBAE  
00645      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL '03' OR '04' OR       ELTBAE  
00646                    '06' OR '08')                                  ELTBAE  
00647          PERFORM CREATE-SUPPLEMENTAL-SCREEN.                      ELTBAE  
00648                                                                   ELTBAE  
00649 ************************************************************      ELTBAE  
00650 *                                                          *      ELTBAE  
00651 *        CREATE INSTITUTIONAL SCREEN                       *      ELTBAE  
00652 *                                                          *      ELTBAE  
00653 ************************************************************      ELTBAE  
00654  CREATE-INSTITUTIONAL-SCREEN.                                     ELTBAE  
00655      MOVE WS-INST TO WS-HDR-LINE-BCBSMM.                          ELTBAE  
00656      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTBAE  
00657      IF GSS-BA-BC-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTBAE  
00658          ZEROES                                                   ELTBAE  
00659                 AND LOW-VALUES                                    ELTBAE  
00660          PERFORM GENERATE-INSTITUTIONAL-TEXT                      ELTBAE  
00661      ELSE                                                         ELTBAE  
00662          PERFORM GENERATE-INSTITUTIONAL-NOT-APP.                  ELTBAE  
00663      EJECT                                                        ELTBAE  
00664                                                                   ELTBAE  
00665 ************************************************************      ELTBAE  
00666 *                                                          *      ELTBAE  
00667 *        GENERATE INSTITUTIONAL TEXT                       *      ELTBAE  
00668 *                                                          *      ELTBAE  
00669 ************************************************************      ELTBAE  
00670  GENERATE-INSTITUTIONAL-TEXT.                                     ELTBAE  
00671      PERFORM EJECT-NEW-PAGE.                                      ELTBAE  
00672      PERFORM TRANSLATE-AND-DISPLAY-BAE-IND.                       ELTBAE  
00673      IF GSS-BA-APPROVAL-SRC-IND (GSS-INDEX) NOT EQUAL SPACES      ELTBAE  
00674          AND                                                      ELTBAE  
00675                 ZEROES AND LOW-VALUES                             ELTBAE  
00676          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTBAE  
00677      PERFORM TRANSLATE-BC-IND.                                    ELTBAE  
00678      IF GSS-BA-BC-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL SPACES  ELTBAE  
00679           AND ZEROES AND LOW-VALUES                               ELTBAE  
00680            PERFORM TRANSLATE-BC-PAYMENT-LEVEL-IND.                ELTBAE  
00681      SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTBAE  
00682      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTBAE  
00683      IF GSS-BA-BC-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES AND    ELTBAE  
00684          ZEROES                                                   ELTBAE  
00685                 AND LOW-VALUES                                    ELTBAE  
00686          PERFORM TRANSLATE-BC-CALC-METHOD.                        ELTBAE  
00687      IF GSS-BA-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTBAE  
00688          SPACES AND                                               ELTBAE  
00689                 ZEROES AND LOW-VALUES                             ELTBAE  
00690          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTBAE  
00691      IF GSS-BA-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL SPACES AND    ELTBAE  
00692          ZEROES                                                   ELTBAE  
00693                 AND LOW-VALUES                                    ELTBAE  
00694          PERFORM GENERATE-SPILL-OVER-TEXT.                        ELTBAE  
00695      PERFORM GENERATE-SPECIAL-PROVIDERS-SEN.                      ELTBAE  
00696      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTBAE  
00697      EJECT                                                        ELTBAE  
00698                                                                   ELTBAE  
00699 ************************************************************      ELTBAE  
00700 *                                                          *      ELTBAE  
00701 *        GENERATE INSTITUTIONAL NOT APPLICABLE             *      ELTBAE  
00702 *                                                          *      ELTBAE  
00703 ************************************************************      ELTBAE  
00704  GENERATE-INSTITUTIONAL-NOT-APP.                                  ELTBAE  
00705      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTBAE  
00706      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTBAE  
00707      MOVE WS-NOT-APPLY-TO-LOB-BC TO COF-DTL-LINE                  ELTBAE  
00708          (COF-NBR-DTL-LINES).                                     ELTBAE  
00709      PERFORM EJECT-NEW-PAGE.                                      ELTBAE  
00710      EJECT                                                        ELTBAE  
00711                                                                   ELTBAE  
00712                                                                   ELTBAE  
00713 ************************************************************      ELTBAE  
00714 *                                                          *      ELTBAE  
00715 *        GENERATE DISCLAIMER MESSAGE                       *      ELTBAE  
00716 *                                                          *      ELTBAE  
00717 ************************************************************      ELTBAE  
00718  GENERATE-DISCLAIMER-MESSAGE.                                     ELTBAE  
00719      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTBAE  
00720      MOVE WS-DISCLAIMER-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).  ELTBAE  
00721      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTBAE  
00722      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTBAE  
00723      PERFORM LINK-TO-OUTPUT.                                      ELTBAE  
00724      EJECT                                                        ELTBAE  
00725                                                                   ELTBAE  
00726 ************************************************************      ELTBAE  
00727 *                                                          *      ELTBAE  
00728 *        TRANSLATE BC IND                                  *      ELTBAE  
00729 *                                                          *      ELTBAE  
00730 ************************************************************      ELTBAE  
00731  TRANSLATE-BC-IND.                                                ELTBAE  
00732      INITIALIZE TCAR-FROM-AREA.                                   ELTBAE  
00733      MOVE +1 TO TCAR-FROM-SUB.                                    ELTBAE  
00734      SET BLANK-LINE-NEEDED TO TRUE.                               ELTBAE  
00735      SET PERIOD-NEEDED TO TRUE.                                   ELTBAE  
00736      MOVE GSS-BA-BC-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTBAE  
00737      MOVE   'BA-BC-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTBAE  
00738      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTBAE  
00739      EJECT                                                        ELTBAE  
00740                                                                   ELTBAE  
00741 ************************************************************      ELTBAE  
00742 *                                                          *      ELTBAE  
00743 *        TRANSLATE BC PAYMENT LEVEL INDICATOR              *      ELTBAE  
00744 *                                                          *      ELTBAE  
00745 ************************************************************      ELTBAE  
00746  TRANSLATE-BC-PAYMENT-LEVEL-IND.                                  ELTBAE  
00747      INITIALIZE TCAR-FROM-AREA.                                   ELTBAE  
00748      MOVE +1 TO TCAR-FROM-SUB.                                    ELTBAE  
00749      MOVE 'BA-BC-PAYMENT-LEVEL-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTBAE  
00750      MOVE GSS-BA-BC-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTBAE  
00751         CMF-CODE-VALUE.                                           ELTBAE  
00752      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTBAE  
00753      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTBAE  
00754                                                                   ELTBAE  
00755 ************************************************************      ELTBAE  
00756 *                                                          *      ELTBAE  
00757 *        TRANSLATE BC CALC METHOD                          *      ELTBAE  
00758 *                                                          *      ELTBAE  
00759 ************************************************************      ELTBAE  
00760  TRANSLATE-BC-CALC-METHOD.                                        ELTBAE  
00761      INITIALIZE TCAR-FROM-AREA.                                   ELTBAE  
00762      MOVE +1 TO TCAR-FROM-SUB.                                    ELTBAE  
00763      SET BLANK-LINE-NEEDED TO TRUE.                               ELTBAE  
00764      SET PERIOD-NEEDED TO TRUE.                                   ELTBAE  
00765      MOVE GSS-BA-BC-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTBAE  
00766      MOVE   'BA-BC-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTBAE  
00767      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTBAE  
00768      EJECT                                                        ELTBAE  
00769                                                                   ELTBAE  
00770 ************************************************************      ELTBAE  
00771 *                                                          *      ELTBAE  
00772 *        GENERATE ACCUM TABULAR DATA                       *      ELTBAE  
00773 *                                                          *      ELTBAE  
00774 ************************************************************      ELTBAE  
00775  GENERATE-ACCUM-TABULAR-DATA.                                     ELTBAE  
00776      PERFORM GENERATE-COINSURANCE.                                ELTBAE  
00777      PERFORM GENERATE-COPAY.                                      ELTBAE  
00778      PERFORM GENERATE-DEDUCTIBLE.                                 ELTBAE  
00779      PERFORM GENERATE-MAXIMUM.                                    ELTBAE  
00780      PERFORM GENERATE-OUT-OF-POCKET.                              ELTBAE  
00781      EJECT                                                        ELTBAE  
00782                                                                   ELTBAE  
00783 ************************************************************      ELTBAE  
00784 *                                                          *      ELTBAE  
00785 *        GENERATE COMBINED BENEFITS REDUCTION TEXT         *      ELTBAE  
00786 *                                                          *      ELTBAE  
00787 ************************************************************      ELTBAE  
00788  GENERATE-COMBINED-BENEFITS-RED.                                  ELTBAE  
00789      MOVE 'BA' TO SRP-COST-CONT-TYPE.                             ELTBAE  
00790      MOVE 'BLUE ADVANTAGE ENTREPRENEUR PROGRAM'  TO SRP-CCP-NAME. ELTBAE  
00791      MOVE GSS-BA-COMB-BENE-REDUCT-IND (GSS-INDEX)                 ELTBAE  
00792                  TO SRP-CCP-COMB-BENE-REDUCT-IND.                 ELTBAE  
00793      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELTBAE  
00794      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTBAE  
00795                            ADDRESS OF                             ELTBAE  
00796          GCCP-TABULAR-REC-AREA.                                   ELTBAE  
00797      PERFORM CALL-CBRI-INTERFACE.                                 ELTBAE  
00798      EJECT                                                        ELTBAE  
00799                                                                   ELTBAE  
00800 ************************************************************      ELTBAE  
00801 *                                                          *      ELTBAE  
00802 *        CREATE PROFESSIONAL SCREEN                        *      ELTBAE  
00803 *                                                          *      ELTBAE  
00804 ************************************************************      ELTBAE  
00805  CREATE-PROFESSIONAL-SCREEN.                                      ELTBAE  
00806      MOVE WS-PROF TO WS-HDR-LINE-BCBSMM.                          ELTBAE  
00807      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTBAE  
00808      IF GSS-BA-BS-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTBAE  
00809          ZEROES                                                   ELTBAE  
00810                 AND LOW-VALUES                                    ELTBAE  
00811          PERFORM GENERATE-PROFESSIONAL-TEXT                       ELTBAE  
00812      ELSE                                                         ELTBAE  
00813          PERFORM GENERATE-PROFESSIONAL-NOT-APPL.                  ELTBAE  
00814      EJECT                                                        ELTBAE  
00815                                                                   ELTBAE  
00816 ************************************************************      ELTBAE  
00817 *                                                          *      ELTBAE  
00818 *        GENERATE PROFESSIONAL TEXT                        *      ELTBAE  
00819 *                                                          *      ELTBAE  
00820 ************************************************************      ELTBAE  
00821  GENERATE-PROFESSIONAL-TEXT.                                      ELTBAE  
00822      PERFORM EJECT-NEW-PAGE.                                      ELTBAE  
00823      PERFORM TRANSLATE-AND-DISPLAY-BAE-IND.                       ELTBAE  
00824      IF GSS-BA-APPROVAL-SRC-IND (GSS-INDEX) NOT EQUAL             ELTBAE  
00825          SPACES                                                   ELTBAE  
00826                 AND ZEROES AND LOW-VALUES                         ELTBAE  
00827          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTBAE  
00828      PERFORM TRANSLATE-BS-IND.                                    ELTBAE  
00829      IF GSS-BA-BS-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL SPACES  ELTBAE  
00830            AND ZEROES AND LOW-VALUES                              ELTBAE  
00831            PERFORM TRANSLATE-BS-PAYMENT-LEVEL-IND.                ELTBAE  
00832      IF GSS-BA-BS-ALT-PRICING-METHOD (GSS-INDEX) NOT EQUAL        ELTBAE  
00833          SPACES AND                                               ELTBAE  
00834                 ZEROES AND LOW-VALUES                             ELTBAE  
00835          PERFORM TRANSLATE-BS-ALT-PRIC.                           ELTBAE  
00836      SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTBAE  
00837      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTBAE  
00838      IF GSS-BA-BS-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES AND    ELTBAE  
00839          ZEROES                                                   ELTBAE  
00840                 AND LOW-VALUES                                    ELTBAE  
00841          PERFORM TRANSLATE-BS-CALC-METHOD.                        ELTBAE  
00842      IF GSS-BA-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTBAE  
00843          SPACES                                                   ELTBAE  
00844                 AND ZEROES AND LOW-VALUES                         ELTBAE  
00845          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTBAE  
00846      IF GSS-BA-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL SPACES AND    ELTBAE  
00847          ZEROES                                                   ELTBAE  
00848                 AND LOW-VALUES                                    ELTBAE  
00849          PERFORM GENERATE-SPILL-OVER-TEXT.                        ELTBAE  
00850      PERFORM GENERATE-SPECIAL-PROVIDERS-SEN.                      ELTBAE  
00851      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTBAE  
00852                                                                   ELTBAE  
00853 ************************************************************      ELTBAE  
00854 *                                                          *      ELTBAE  
00855 *        GENERATE PROFESSIONAL NOT APPLICABLE              *      ELTBAE  
00856 *                                                          *      ELTBAE  
00857 ************************************************************      ELTBAE  
00858  GENERATE-PROFESSIONAL-NOT-APPL.                                  ELTBAE  
00859      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTBAE  
00860      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTBAE  
00861      MOVE WS-NOT-APPLY-TO-LOB-BS TO COF-DTL-LINE                  ELTBAE  
00862          (COF-NBR-DTL-LINES).                                     ELTBAE  
00863      PERFORM EJECT-NEW-PAGE.                                      ELTBAE  
00864      EJECT                                                        ELTBAE  
00865                                                                   ELTBAE  
00866 ************************************************************      ELTBAE  
00867 *                                                          *      ELTBAE  
00868 *        TRANSLATE BS IND                                  *      ELTBAE  
00869 *                                                          *      ELTBAE  
00870 ************************************************************      ELTBAE  
00871  TRANSLATE-BS-IND.                                                ELTBAE  
00872      INITIALIZE TCAR-FROM-AREA.                                   ELTBAE  
00873      MOVE +1 TO TCAR-FROM-SUB.                                    ELTBAE  
00874      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTBAE  
00875      SET PERIOD-NEEDED TO TRUE.                                   ELTBAE  
00876      MOVE GSS-BA-BS-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTBAE  
00877      MOVE   'BA-BS-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTBAE  
00878      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTBAE  
00879      EJECT                                                        ELTBAE  
00880                                                                   ELTBAE  
00881 ************************************************************      ELTBAE  
00882 *                                                          *      ELTBAE  
00883 *        TRANSLATE BS PAYMENT LEVEL INDICATOR              *      ELTBAE  
00884 *                                                          *      ELTBAE  
00885 ************************************************************      ELTBAE  
00886  TRANSLATE-BS-PAYMENT-LEVEL-IND.                                  ELTBAE  
00887      MOVE 'BA-BS-PAYMENT-LEVEL-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTBAE  
00888      MOVE GSS-BA-BS-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTBAE  
00889         CMF-CODE-VALUE.                                           ELTBAE  
00890      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTBAE  
00891      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTBAE  
00892                                                                   ELTBAE  
00893 ************************************************************      ELTBAE  
00894 *                                                          *      ELTBAE  
00895 *        TRANSLATE BS CALC METHOD                          *      ELTBAE  
00896 *                                                          *      ELTBAE  
00897 ************************************************************      ELTBAE  
00898  TRANSLATE-BS-CALC-METHOD.                                        ELTBAE  
00899      INITIALIZE TCAR-FROM-AREA.                                   ELTBAE  
00900      MOVE +1 TO TCAR-FROM-SUB.                                    ELTBAE  
00901      SET BLANK-LINE-NEEDED TO TRUE.                               ELTBAE  
00902      SET PERIOD-NEEDED TO TRUE.                                   ELTBAE  
00903      MOVE GSS-BA-BS-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTBAE  
00904      MOVE   'BA-BS-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTBAE  
00905      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTBAE  
00906      EJECT                                                        ELTBAE  
00907                                                                   ELTBAE  
00908 ************************************************************      ELTBAE  
00909 *                                                          *      ELTBAE  
00910 *        TRANSLATE BS ALT PRIC                             *      ELTBAE  
00911 *                                                          *      ELTBAE  
00912 ************************************************************      ELTBAE  
00913  TRANSLATE-BS-ALT-PRIC.                                           ELTBAE  
00914      INITIALIZE TCAR-FROM-AREA.                                   ELTBAE  
00915      MOVE +1 TO TCAR-FROM-SUB.                                    ELTBAE  
00916      MOVE WS-ALT-PRICING-LINE-BS TO TCAR-FROM-LINE                ELTBAE  
00917          (TCAR-FROM-SUB).                                         ELTBAE  
00918      ADD  +1 TO TCAR-FROM-SUB.                                    ELTBAE  
00919      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTBAE  
00920      SET PERIOD-NEEDED TO TRUE.                                   ELTBAE  
00921      MOVE GSS-BA-BS-ALT-PRICING-METHOD (GSS-INDEX) TO             ELTBAE  
00922          CMF-CODE-VALUE.                                          ELTBAE  
00923      MOVE  'BA-BS-ALT-PRICING-METHOD' TO CMF-ELEMENT-SYSTEM-NAME. ELTBAE  
00924      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTBAE  
00925      EJECT                                                        ELTBAE  
00926                                                                   ELTBAE  
00927 ************************************************************      ELTBAE  
00928 *                                                          *      ELTBAE  
00929 *        CREATE SUPPLEMENTAL SCREEN                        *      ELTBAE  
00930 *                                                          *      ELTBAE  
00931 ************************************************************      ELTBAE  
00932  CREATE-SUPPLEMENTAL-SCREEN.                                      ELTBAE  
00933      MOVE WS-SUPP TO WS-HDR-LINE-BCBSMM.                          ELTBAE  
00934      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTBAE  
00935      IF GSS-BA-MM-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTBAE  
00936          ZEROES                                                   ELTBAE  
00937                 AND LOW-VALUES                                    ELTBAE  
00938          PERFORM GENERATE-SUPPLEMENTAL-TEXT                       ELTBAE  
00939      ELSE                                                         ELTBAE  
00940          PERFORM GENERATE-SUPPLEMENTAL-NOT-APPL.                  ELTBAE  
00941      EJECT                                                        ELTBAE  
00942                                                                   ELTBAE  
00943 ************************************************************      ELTBAE  
00944 *                                                          *      ELTBAE  
00945 *        GENERATE SUPPLEMENTAL TEXT                        *      ELTBAE  
00946 *                                                          *      ELTBAE  
00947 ************************************************************      ELTBAE  
00948  GENERATE-SUPPLEMENTAL-TEXT.                                      ELTBAE  
00949      PERFORM EJECT-NEW-PAGE.                                      ELTBAE  
00950      PERFORM TRANSLATE-AND-DISPLAY-BAE-IND.                       ELTBAE  
00951      IF GSS-BA-APPROVAL-SRC-IND (GSS-INDEX) NOT EQUAL             ELTBAE  
00952          SPACES                                                   ELTBAE  
00953                 AND ZEROES AND LOW-VALUES                         ELTBAE  
00954          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTBAE  
00955      PERFORM TRANSLATE-MM-IND.                                    ELTBAE  
00956      IF GSS-BA-MM-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL SPACES  ELTBAE  
00957         AND SPACES AND LOW-VALUES                                 ELTBAE  
00958            PERFORM TRANSLATE-MM-PAYMENT-LEVEL-IND.                ELTBAE  
00959      SET SRP-ACCUM-PROV-CLASS-SUPP TO TRUE.                       ELTBAE  
00960      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTBAE  
00961      IF GSS-BA-MM-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES AND    ELTBAE  
00962          ZEROES                                                   ELTBAE  
00963                 AND LOW-VALUES                                    ELTBAE  
00964          PERFORM TRANSLATE-MM-CALC-METHOD.                        ELTBAE  
00965      IF GSS-BA-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTBAE  
00966          SPACES                                                   ELTBAE  
00967                 AND ZEROES AND LOW-VALUES                         ELTBAE  
00968          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTBAE  
00969      PERFORM GENERATE-SPECIAL-PROVIDERS-SEN.                      ELTBAE  
00970      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTBAE  
00971                                                                   ELTBAE  
00972                                                                   ELTBAE  
00973 ************************************************************      ELTBAE  
00974 *                                                          *      ELTBAE  
00975 *        GENERATE SUPPLEMENTAL NOT APPLICABLE              *      ELTBAE  
00976 *                                                          *      ELTBAE  
00977 ************************************************************      ELTBAE  
00978  GENERATE-SUPPLEMENTAL-NOT-APPL.                                  ELTBAE  
00979      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTBAE  
00980      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTBAE  
00981      MOVE WS-NOT-APPLY-TO-LOB-MM TO COF-DTL-LINE                  ELTBAE  
00982          (COF-NBR-DTL-LINES).                                     ELTBAE  
00983      PERFORM EJECT-NEW-PAGE.                                      ELTBAE  
00984      EJECT                                                        ELTBAE  
00985                                                                   ELTBAE  
00986 ************************************************************      ELTBAE  
00987 *                                                          *      ELTBAE  
00988 *        TRANSLATE MM IND                                  *      ELTBAE  
00989 *                                                          *      ELTBAE  
00990 ************************************************************      ELTBAE  
00991  TRANSLATE-MM-IND.                                                ELTBAE  
00992      INITIALIZE TCAR-FROM-AREA.                                   ELTBAE  
00993      MOVE +1 TO TCAR-FROM-SUB.                                    ELTBAE  
00994      SET BLANK-LINE-NEEDED TO TRUE.                               ELTBAE  
00995      SET PERIOD-NEEDED TO TRUE.                                   ELTBAE  
00996      MOVE GSS-BA-MM-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTBAE  
00997      MOVE   'BA-MM-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTBAE  
00998      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTBAE  
00999      EJECT                                                        ELTBAE  
01000                                                                   ELTBAE  
01001 ************************************************************      ELTBAE  
01002 *                                                          *      ELTBAE  
01003 *        TRANSLATE MM PAYMENT LEVEL INDICATOR              *      ELTBAE  
01004 *                                                          *      ELTBAE  
01005 ************************************************************      ELTBAE  
01006  TRANSLATE-MM-PAYMENT-LEVEL-IND.                                  ELTBAE  
01007      MOVE 'BA-MM-PAYMENT-LEVEL-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTBAE  
01008      MOVE GSS-BA-MM-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTBAE  
01009         CMF-CODE-VALUE.                                           ELTBAE  
01010      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTBAE  
01011      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTBAE  
01012                                                                   ELTBAE  
01013 ************************************************************      ELTBAE  
01014 *                                                          *      ELTBAE  
01015 *        TRANSLATE MM CALC METHOD                          *      ELTBAE  
01016 *                                                          *      ELTBAE  
01017 ************************************************************      ELTBAE  
01018  TRANSLATE-MM-CALC-METHOD.                                        ELTBAE  
01019      INITIALIZE TCAR-FROM-AREA.                                   ELTBAE  
01020      MOVE +1 TO TCAR-FROM-SUB.                                    ELTBAE  
01021      SET BLANK-LINE-NEEDED TO TRUE.                               ELTBAE  
01022      SET PERIOD-NEEDED TO TRUE.                                   ELTBAE  
01023      MOVE GSS-BA-MM-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTBAE  
01024      MOVE   'BA-MM-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTBAE  
01025      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTBAE  
01026      EJECT                                                        ELTBAE  
01027                                                                   ELTBAE  
01028                                                                   ELTBAE  
01029 ************************************************************      ELTBAE  
01030 *                                                          *      ELTBAE  
01031 *        GENERATE SPILL OVER TEXT                          *      ELTBAE  
01032 *                                                          *      ELTBAE  
01033 ************************************************************      ELTBAE  
01034  GENERATE-SPILL-OVER-TEXT.                                        ELTBAE  
01035      INITIALIZE TCAR-FROM-AREA.                                   ELTBAE  
01036      MOVE +1 TO TCAR-FROM-SUB.                                    ELTBAE  
01037      MOVE WS-SPILL-OVER-LINE TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELTBAE  
01038      ADD  +1 TO TCAR-FROM-SUB.                                    ELTBAE  
01039      SET BLANK-LINE-NEEDED TO TRUE.                               ELTBAE  
01040      SET PERIOD-NEEDED TO TRUE.                                   ELTBAE  
01041      MOVE GSS-BA-SPILL-OVER-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTBAE  
01042      MOVE 'BA-SPILL-OVER-IND' TO CMF-ELEMENT-SYSTEM-NAME.         ELTBAE  
01043      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTBAE  
01044      EJECT                                                        ELTBAE  
01045                                                                   ELTBAE  
01046 ************************************************************      ELTBAE  
01047 *                                                          *      ELTBAE  
01048 *        GENERATE SPECIAL PROVIDERS SENTENCE               *      ELTBAE  
01049 *                                                          *      ELTBAE  
01050 ************************************************************      ELTBAE  
01051  GENERATE-SPECIAL-PROVIDERS-SEN.                                  ELTBAE  
01052      SET GCG-INDEX TO +1.                                         ELTBAE  
01053      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTBAE  
01054         AT END                                                    ELTBAE  
01055              MOVE ZEROES TO WS-GBAE-PROV-SLOT-NO                  ELTBAE  
01056         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GBAE                     ELTBAE  
01057              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTBAE  
01058                  TO WS-GBAE-PROV-SLOT-NO                          ELTBAE  
01059         END-SEARCH.                                               ELTBAE  
01060      IF WS-GBAE-PROV-SLOT-NO NOT EQUAL ZEROES                     ELTBAE  
01061          PERFORM DISPLAY-SPECIAL-PROVIDERS-SENT                   ELTBAE  
01062          PERFORM GENERATE-SPECIAL-PROVIDERS-TEX.                  ELTBAE  
01063                                                                   ELTBAE  
01064 ************************************************************      ELTBAE  
01065 *                                                          *      ELTBAE  
01066 *        DISPLAY SPECIAL PROVIDERS SENTENCE                *      ELTBAE  
01067 *                                                          *      ELTBAE  
01068 ************************************************************      ELTBAE  
01069  DISPLAY-SPECIAL-PROVIDERS-SENT.                                  ELTBAE  
01070      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTBAE  
01071      MOVE WS-SPEC-PROV-MSG  TO COF-DTL-LINE                       ELTBAE  
01072          (COF-NBR-DTL-LINES).                                     ELTBAE  
01073      PERFORM LINK-TO-OUTPUT.                                      ELTBAE  
01074      EJECT                                                        ELTBAE  
01075                                                                   ELTBAE  
01076 ************************************************************      ELTBAE  
01077 *                                                          *      ELTBAE  
01078 *        SEARCH THE GSS ENTRY                              *      ELTBAE  
01079 *                                                          *      ELTBAE  
01080 ************************************************************      ELTBAE  
01081  SEARCH-THE-GSS-ENTRY.                                            ELTBAE  
01082      SET GSS-INDEX TO 1.                                          ELTBAE  
01083      SEARCH GSS-ENTRY                                             ELTBAE  
01084          AT END                                                   ELTBAE  
01085               SET TABULAR-IS-UNDEFINED TO TRUE                    ELTBAE  
01086          WHEN GSS-BA-PROG-CODE-CHR (GSS-INDEX)                    ELTBAE  
01087               CONTINUE                                            ELTBAE  
01088         END-SEARCH.                                               ELTBAE  
01089      IF TABULAR-IS-UNDEFINED                                      ELTBAE  
01090          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTBAE  
01091                                                                   ELTBAE  
01092 ************************************************************      ELTBAE  
01093 *                                                          *      ELTBAE  
01094 *        CALL CODES MANUAL INTERFACE                       *      ELTBAE  
01095 *                                                          *      ELTBAE  
01096 ************************************************************      ELTBAE  
01097  CALL-CODES-MANUAL-INTERFACE.                                     ELTBAE  
01098      MOVE WS-GCCP TO CMF-RECORD-PREFIX.                           ELTBAE  
01099      EXEC CICS LINK                                               ELTBAE  
01100                PROGRAM ('ELUCMIF')                                ELTBAE  
01101                COMMAREA (DFHCOMMAREA)                             ELTBAE  
01102        END-EXEC.                                                  ELTBAE  
01103      EJECT                                                        ELTBAE  
01104                                                                   ELTBAE  
01105 ************************************************************      ELTBAE  
01106 *                                                          *      ELTBAE  
01107 *        GET GCCP TABULAR                                  *      ELTBAE  
01108 *                                                          *      ELTBAE  
01109 ************************************************************      ELTBAE  
01110  GET-GCCP-TABULAR.                                                ELTBAE  
01111      PERFORM ESTABLISH-ADDRESSABILITY-OF-GC.                      ELTBAE  
01112      SET IOP-RD              TO TRUE.                             ELTBAE  
01113      SET IOP-STG-MODE-MOVE   TO TRUE.                             ELTBAE  
01114      SET IOP-FCQ-NONE        TO TRUE.                             ELTBAE  
01115      SET IOP-KVQ-EQ          TO TRUE.                             ELTBAE  
01116      MOVE KWA-GCTABULR-KEY   TO IOP-FILE-KEY.                     ELTBAE  
01117      PERFORM CALL-INPUT-OUTPUT-MODULE.                            ELTBAE  
01118      IF IOP-RC-OK                                                 ELTBAE  
01119          PERFORM ESTABLISH-ADDRESSY-OF-GCCP-TAB                   ELTBAE  
01120      ELSE IF IOP-RC-NOTFND                                        ELTBAE  
01121          PERFORM SIGNAL-NOT-FOUND-GCTAB-ERROR                     ELTBAE  
01122      ELSE                                                         ELTBAE  
01123          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTBAE  
01124      EJECT                                                        ELTBAE  
01125                                                                   ELTBAE  
01126 ************************************************************      ELTBAE  
01127 *                                                          *      ELTBAE  
01128 *        ESTABLISH ADDRESSABILITY OF GCTABULAR IO PARAMETER*      ELTBAE  
01129 *                                                          *      ELTBAE  
01130 ************************************************************      ELTBAE  
01131  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTBAE  
01132      SET CIA-GCTABULR-DDN TO TRUE.                                ELTBAE  
01133      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBAE  
01134                            ADDRESS OF                             ELTBAE  
01135          IOP-INPUT-OUTPUT-PARAMETERS.                             ELTBAE  
01136      EJECT                                                        ELTBAE  
01137                                                                   ELTBAE  
01138 ************************************************************      ELTBAE  
01139 *                                                          *      ELTBAE  
01140 *        CALL INPUT OUTPUT MODULE                          *      ELTBAE  
01141 *                                                          *      ELTBAE  
01142 ************************************************************      ELTBAE  
01143  CALL-INPUT-OUTPUT-MODULE.                                        ELTBAE  
01144      EXEC CICS LINK                                               ELTBAE  
01145                PROGRAM ('ELUIOPGM')                               ELTBAE  
01146                COMMAREA (DFHCOMMAREA)                             ELTBAE  
01147        END-EXEC.                                                  ELTBAE  
01148      EJECT                                                        ELTBAE  
01149                                                                   ELTBAE  
01150 ************************************************************      ELTBAE  
01151 *                                                          *      ELTBAE  
01152 *        ESTABLISH ADDRESSY OF GCCP TABULAR                *      ELTBAE  
01153 *                                                          *      ELTBAE  
01154 ************************************************************      ELTBAE  
01155  ESTABLISH-ADDRESSY-OF-GCCP-TAB.                                  ELTBAE  
01156      SET ADDRESS OF GCCP-TABULAR-REC-AREA TO                      ELTBAE  
01157          IOP-REC-PTR.                                             ELTBAE  
01158      SET IOP-REC-PTR TO NULL.                                     ELTBAE  
01159      EJECT                                                        ELTBAE  
01160                                                                   ELTBAE  
01161 ************************************************************      ELTBAE  
01162 *                                                          *      ELTBAE  
01163 *        SIGNAL NOT FOUND GCTAB ERROR                      *      ELTBAE  
01164 *                                                          *      ELTBAE  
01165 ************************************************************      ELTBAE  
01166  SIGNAL-NOT-FOUND-GCTAB-ERROR.                                    ELTBAE  
01167      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTBAE  
01168      PERFORM SIGNAL-ABEND.                                        ELTBAE  
01169      EJECT                                                        ELTBAE  
01170                                                                   ELTBAE  
01171 ************************************************************      ELTBAE  
01172 *                                                          *      ELTBAE  
01173 *        SIGNAL CRITICAL IO ERROR                          *      ELTBAE  
01174 *                                                          *      ELTBAE  
01175 ************************************************************      ELTBAE  
01176  SIGNAL-CRITICAL-IO-ERROR.                                        ELTBAE  
01177      SET CIA-AB-CRITIO TO TRUE.                                   ELTBAE  
01178      PERFORM SIGNAL-ABEND.                                        ELTBAE  
01179      EJECT                                                        ELTBAE  
01180                                                                   ELTBAE  
01181 ************************************************************      ELTBAE  
01182 *                                                          *      ELTBAE  
01183 *        GENERATE SPECIAL PROVIDERS TEXT                   *      ELTBAE  
01184 *                                                          *      ELTBAE  
01185 ************************************************************      ELTBAE  
01186  GENERATE-SPECIAL-PROVIDERS-TEX.                                  ELTBAE  
01187      MOVE 'BLUE ADVANTAGE ENTREPRENEUR' TO    SRP-CCP-NAME.       ELTBAE  
01188      MOVE  WS-GBAE                        TO SRP-TABULAR-ID.      ELTBAE  
01189      MOVE  WS-GBAE-PROV-SLOT-NO           TO SRP-TABULAR-SLOT-NO. ELTBAE  
01190      PERFORM CALL-SPECIAL-PROVIDERS-GENERAT.                      ELTBAE  
01191                                                                   ELTBAE  
01192                                                                   ELTBAE  
01193 ************************************************************      ELTBAE  
01194 *                                                          *      ELTBAE  
01195 *        CALL SPECIAL PROVIDERS GENERATOR                  *      ELTBAE  
01196 *                                                          *      ELTBAE  
01197 ************************************************************      ELTBAE  
01198  CALL-SPECIAL-PROVIDERS-GENERAT.                                  ELTBAE  
01199      EXEC CICS LINK                                               ELTBAE  
01200                PROGRAM ('ELGGXXC')                                ELTBAE  
01201                COMMAREA (DFHCOMMAREA)                             ELTBAE  
01202         END-EXEC.                                                 ELTBAE  
01203                                                                   ELTBAE  
01204 ************************************************************      ELTBAE  
01205 *                                                          *      ELTBAE  
01206 *        GENERATE COINSURANCE                              *      ELTBAE  
01207 *                                                          *      ELTBAE  
01208 ************************************************************      ELTBAE  
01209  GENERATE-COINSURANCE.                                            ELTBAE  
01210      EXEC CICS LINK                                               ELTBAE  
01211                PROGRAM ('ELGACLCC')                               ELTBAE  
01212                COMMAREA (DFHCOMMAREA)                             ELTBAE  
01213         END-EXEC.                                                 ELTBAE  
01214                                                                   ELTBAE  
01215 ************************************************************      ELTBAE  
01216 *                                                          *      ELTBAE  
01217 *        GENERATE COPAY                                    *      ELTBAE  
01218 *                                                          *      ELTBAE  
01219 ************************************************************      ELTBAE  
01220  GENERATE-COPAY.                                                  ELTBAE  
01221      EXEC CICS LINK                                               ELTBAE  
01222                PROGRAM ('ELGACPCC')                               ELTBAE  
01223                COMMAREA (DFHCOMMAREA)                             ELTBAE  
01224         END-EXEC.                                                 ELTBAE  
01225                                                                   ELTBAE  
01226 ************************************************************      ELTBAE  
01227 *                                                          *      ELTBAE  
01228 *        GENERATE DEDUCTIBLE                               *      ELTBAE  
01229 *                                                          *      ELTBAE  
01230 ************************************************************      ELTBAE  
01231  GENERATE-DEDUCTIBLE.                                             ELTBAE  
01232      EXEC CICS LINK                                               ELTBAE  
01233                PROGRAM ('ELGADLCC')                               ELTBAE  
01234                COMMAREA (DFHCOMMAREA)                             ELTBAE  
01235         END-EXEC.                                                 ELTBAE  
01236                                                                   ELTBAE  
01237 ************************************************************      ELTBAE  
01238 *                                                          *      ELTBAE  
01239 *        GENERATE MAXIMUM                                  *      ELTBAE  
01240 *                                                          *      ELTBAE  
01241 ************************************************************      ELTBAE  
01242  GENERATE-MAXIMUM.                                                ELTBAE  
01243      EXEC CICS LINK                                               ELTBAE  
01244                PROGRAM ('ELGABMCC')                               ELTBAE  
01245                COMMAREA (DFHCOMMAREA)                             ELTBAE  
01246         END-EXEC.                                                 ELTBAE  
01247      EJECT                                                        ELTBAE  
01248                                                                   ELTBAE  
01249 ************************************************************      ELTBAE  
01250 *                                                          *      ELTBAE  
01251 *        GENERATE OUT OF POCKET                            *      ELTBAE  
01252 *                                                          *      ELTBAE  
01253 ************************************************************      ELTBAE  
01254  GENERATE-OUT-OF-POCKET.                                          ELTBAE  
01255      EXEC CICS LINK                                               ELTBAE  
01256                PROGRAM ('ELGAOLCC')                               ELTBAE  
01257                COMMAREA (DFHCOMMAREA)                             ELTBAE  
01258         END-EXEC.                                                 ELTBAE  
01259                                                                   ELTBAE  
01260 ************************************************************      ELTBAE  
01261 *                                                          *      ELTBAE  
01262 *        CALL CBRI INTERFACE                               *      ELTBAE  
01263 *                                                          *      ELTBAE  
01264 ************************************************************      ELTBAE  
01265  CALL-CBRI-INTERFACE.                                             ELTBAE  
01266      CALL 'ELGCBRI' USING DFHEIBLK                                ELTBAE  
01267                           DFHCOMMAREA.                            ELTBAE  
01268                                                                   ELTBAE  
01269 ************************************************************      ELTBAE  
01270 *                                                          *      ELTBAE  
01271 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTBAE  
01272 *                                                          *      ELTBAE  
01273 ************************************************************      ELTBAE  
01274  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTBAE  
01275      PERFORM INITIALIZE-CMOUT.                                    ELTBAE  
01276      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTBAE  
01277      EJECT                                                        ELTBAE  
01278                                                                   ELTBAE  
01279 ************************************************************      ELTBAE  
01280 *                                                          *      ELTBAE  
01281 *        PREPARE TEXT FOR OUTPUT                           *      ELTBAE  
01282 *                                                          *      ELTBAE  
01283 ************************************************************      ELTBAE  
01284  PREPARE-TEXT-FOR-OUTPUT.                                         ELTBAE  
01285      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTBAE  
01286          UNTIL CMF-DESCR-IDX                                      ELTBAE  
01287                                    GREATER THAN                   ELTBAE  
01288              CMF-NBR-DESCR-LINES.                                 ELTBAE  
01289      EJECT                                                        ELTBAE  
01290                                                                   ELTBAE  
01291 ************************************************************      ELTBAE  
01292 *                                                          *      ELTBAE  
01293 *        INITIALIZE CMOUT                                  *      ELTBAE  
01294 *                                                          *      ELTBAE  
01295 ************************************************************      ELTBAE  
01296  INITIALIZE-CMOUT.                                                ELTBAE  
01297      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTBAE  
01298      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBAE  
01299          ADDRESS OF CMF-DESCR.                                    ELTBAE  
01300      SET CMF-DESCR-IDX TO 1.                                      ELTBAE  
01301      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTBAE  
01302                                                                   ELTBAE  
01303                                                                   ELTBAE  
01304 ************************************************************      ELTBAE  
01305 *                                                          *      ELTBAE  
01306 *        MOVE CMF TEXT TO OUTPUT                           *      ELTBAE  
01307 *                                                          *      ELTBAE  
01308 ************************************************************      ELTBAE  
01309  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTBAE  
01310      PERFORM MOVE-A-LINE.                                         ELTBAE  
01311      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTBAE  
01312          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTBAE  
01313      IF TCAR-FROM-SUB GREATER THAN 20                             ELTBAE  
01314               OR CMF-DESCR-IDX GREATER THAN                       ELTBAE  
01315          CMF-NBR-DESCR-LINES                                      ELTBAE  
01316          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTBAE  
01317                                                                   ELTBAE  
01318                                                                   ELTBAE  
01319 ************************************************************      ELTBAE  
01320 *                                                          *      ELTBAE  
01321 *        FINISH CODES MANUAL TEXT                          *      ELTBAE  
01322 *                                                          *      ELTBAE  
01323 ************************************************************      ELTBAE  
01324  FINISH-CODES-MANUAL-TEXT.                                        ELTBAE  
01325      SET DONE-PROCESSING TO TRUE.                                 ELTBAE  
01326      IF PERIOD-NEEDED                                             ELTBAE  
01327          PERFORM GET-AND-MOVE-PERIOD.                             ELTBAE  
01328      EJECT                                                        ELTBAE  
01329                                                                   ELTBAE  
01330                                                                   ELTBAE  
01331 ************************************************************      ELTBAE  
01332 *                                                          *      ELTBAE  
01333 *        GET AND MOVE PERIOD                               *      ELTBAE  
01334 *                                                          *      ELTBAE  
01335 ************************************************************      ELTBAE  
01336  GET-AND-MOVE-PERIOD.                                             ELTBAE  
01337      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTBAE  
01338          (TCAR-FROM-SUB).                                         ELTBAE  
01339                                                                   ELTBAE  
01340                                                                   ELTBAE  
01341 ************************************************************      ELTBAE  
01342 *                                                          *      ELTBAE  
01343 *        SAVE LAST LINE                                    *      ELTBAE  
01344 *                                                          *      ELTBAE  
01345 ************************************************************      ELTBAE  
01346  SAVE-LAST-LINE.                                                  ELTBAE  
01347      MOVE +1 TO TCAR-FROM-SUB.                                    ELTBAE  
01348      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTBAE  
01349         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTBAE  
01350      ADD 1 TO TCAR-FROM-SUB.                                      ELTBAE  
01351      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTBAE  
01352                                                                   ELTBAE  
01353                                                                   ELTBAE  
01354 ************************************************************      ELTBAE  
01355 *                                                          *      ELTBAE  
01356 *        OUTPUT LAST LINE                                  *      ELTBAE  
01357 *                                                          *      ELTBAE  
01358 ************************************************************      ELTBAE  
01359  OUTPUT-LAST-LINE.                                                ELTBAE  
01360      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTBAE  
01361          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTBAE  
01362      IF BLANK-LINE-NEEDED                                         ELTBAE  
01363          PERFORM CREATE-A-BLANK-LINE.                             ELTBAE  
01364                                                                   ELTBAE  
01365                                                                   ELTBAE  
01366 ************************************************************      ELTBAE  
01367 *                                                          *      ELTBAE  
01368 *        CREATE A BLANK LINE                               *      ELTBAE  
01369 *                                                          *      ELTBAE  
01370 ************************************************************      ELTBAE  
01371  CREATE-A-BLANK-LINE.                                             ELTBAE  
01372      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTBAE  
01373      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTBAE  
01374                                                                   ELTBAE  
01375                                                                   ELTBAE  
01376 ************************************************************      ELTBAE  
01377 *                                                          *      ELTBAE  
01378 *        MOVE A LINE                                       *      ELTBAE  
01379 *                                                          *      ELTBAE  
01380 ************************************************************      ELTBAE  
01381  MOVE-A-LINE.                                                     ELTBAE  
01382      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTBAE  
01383          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTBAE  
01384      SET CMF-DESCR-IDX UP BY 1.                                   ELTBAE  
01385      ADD 1 TO TCAR-FROM-SUB.                                      ELTBAE  
01386      EJECT                                                        ELTBAE  
01387                                                                   ELTBAE  
01388                                                                   ELTBAE  
01389 ************************************************************      ELTBAE  
01390 *                                                          *      ELTBAE  
01391 *        REFORMAT AND WRITE TEXT                           *      ELTBAE  
01392 *                                                          *      ELTBAE  
01393 ************************************************************      ELTBAE  
01394  REFORMAT-AND-WRITE-TEXT.                                         ELTBAE  
01395      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTBAE  
01396      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTBAE  
01397      PERFORM UNSTRING-TEXT.                                       ELTBAE  
01398      MOVE +1 TO TCAR-FROM-SUB.                                    ELTBAE  
01399      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTBAE  
01400      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTBAE  
01401          UNTIL COF-NBR-DTL-LINES GREATER                          ELTBAE  
01402                                   TCAR-OUTPUT-FIELDS-USED -       ELTBAE  
01403              1.                                                   ELTBAE  
01404      PERFORM DISPOSE-OF-LAST-LINE.                                ELTBAE  
01405      PERFORM LINK-TO-OUTPUT.                                      ELTBAE  
01406                                                                   ELTBAE  
01407                                                                   ELTBAE  
01408 ************************************************************      ELTBAE  
01409 *                                                          *      ELTBAE  
01410 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTBAE  
01411 *                                                          *      ELTBAE  
01412 ************************************************************      ELTBAE  
01413  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTBAE  
01414      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTBAE  
01415           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTBAE  
01416      ADD +1 TO TCAR-FROM-SUB.                                     ELTBAE  
01417      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTBAE  
01418      EJECT                                                        ELTBAE  
01419                                                                   ELTBAE  
01420                                                                   ELTBAE  
01421 ************************************************************      ELTBAE  
01422 *                                                          *      ELTBAE  
01423 *        UNSTRING TEXT                                     *      ELTBAE  
01424 *                                                          *      ELTBAE  
01425 ************************************************************      ELTBAE  
01426  UNSTRING-TEXT.                                                   ELTBAE  
01427      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTBAE  
01428      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTBAE  
01429      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTBAE  
01430      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTBAE  
01431      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTBAE  
01432      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTBAE  
01433      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTBAE  
01434      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTBAE  
01435      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTBAE  
01436      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTBAE  
01437      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTBAE  
01438      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTBAE  
01439      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTBAE  
01440      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTBAE  
01441      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTBAE  
01442      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTBAE  
01443      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTBAE  
01444      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTBAE  
01445      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTBAE  
01446      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTBAE  
01447      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTBAE  
01448      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTBAE  
01449      EJECT                                                        ELTBAE  
01450                                                                   ELTBAE  
01451                                                                   ELTBAE  
01452 ************************************************************      ELTBAE  
01453 *                                                          *      ELTBAE  
01454 *        LINK TO OUTPUT                                    *      ELTBAE  
01455 *                                                          *      ELTBAE  
01456 ************************************************************      ELTBAE  
01457  LINK-TO-OUTPUT.                                                  ELTBAE  
01458      EXEC CICS LINK                                               ELTBAE  
01459          PROGRAM ('ELUOUTPT')                                     ELTBAE  
01460          COMMAREA (DFHCOMMAREA)                                   ELTBAE  
01461          END-EXEC.                                                ELTBAE  
01462      EJECT                                                        ELTBAE  
01463                                                                   ELTBAE  
01464                                                                   ELTBAE  
01465 ************************************************************      ELTBAE  
01466 *                                                          *      ELTBAE  
01467 *        DISPOSE OF LAST LINE                              *      ELTBAE  
01468 *                                                          *      ELTBAE  
01469 ************************************************************      ELTBAE  
01470  DISPOSE-OF-LAST-LINE.                                            ELTBAE  
01471      IF NOT ADDITIONAL-TEXT                                       ELTBAE  
01472          PERFORM INITIALIZE-CONTINUED-SW.                         ELTBAE  
01473      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTBAE  
01474          PERFORM SAVE-LAST-LINE                                   ELTBAE  
01475      ELSE                                                         ELTBAE  
01476          PERFORM OUTPUT-LAST-LINE.                                ELTBAE  
01477                                                                   ELTBAE  
01478                                                                   ELTBAE  
01479 ************************************************************      ELTBAE  
01480 *                                                          *      ELTBAE  
01481 *        INITIALIZE CONTINUED SW                           *      ELTBAE  
01482 *                                                          *      ELTBAE  
01483 ************************************************************      ELTBAE  
01484  INITIALIZE-CONTINUED-SW.                                         ELTBAE  
01485      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTBAE  
