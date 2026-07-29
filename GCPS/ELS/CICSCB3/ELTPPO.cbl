00001 *      LAST MAINTENANCE TIME:  7.54.00  DATE: 06/13/91            06/29/02
00002  IDENTIFICATION DIVISION.                                         ELTPPO  
00003                                                                      LV001
00004  PROGRAM-ID.         ELTPPO.                                      ELTPPO  
00005                                                                   ELTPPO  
00006  AUTHOR.             RICK BARILEAU.                               ELTPPO  
00007                                                                   ELTPPO  
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTPPO  
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELTPPO  
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTPPO  
00011                      233 N. MICHIGAN AVE                          ELTPPO  
00012                      CHICAGO, ILLINOIS 60601                      ELTPPO  
00013                                                                   ELTPPO  
00014  DATE-WRITTEN.       01-JUN-1987.                                 ELTPPO  
00015                                                                   ELTPPO  
00016  DATE-COMPILED.                                                   ELTPPO  
00017                                                                   ELTPPO  
00018  SECURITY.           COPYRIGHT 1986,                              ELTPPO  
00019                      HEALTH CARE SERVICE CORPORATION              ELTPPO  
00020      SKIP3                                                        ELTPPO  
00021  ENVIRONMENT DIVISION.                                            ELTPPO  
00022                                                                   ELTPPO  
00023  CONFIGURATION SECTION.                                           ELTPPO  
00024  SOURCE-COMPUTER.    IBM-3090.                                    ELTPPO  
00025  OBJECT-COMPUTER.    IBM-3090.                                    ELTPPO  
00026      EJECT                                                        ELTPPO  
00027 ******************************************************************ELTPPO  
00028 *                                                                *ELTPPO  
00029 *    COPYBOOK:   ELTPPO                                          *ELTPPO  
00030 *    DATE:       01-JUN-1987                                     *ELTPPO  
00031 *    AUTHOR:     RICK BARILEAU                                   *ELTPPO  
00032 *    FUNCTION:   THIS MODULE WILL DISPLAY ALL OUTPUT ASSOCIATED  *ELTPPO  
00033 *                WITH PARTICIPATING PROVIDER OPTION PROGRAM.     *ELTPPO  
00034 *    NOTES:      X---                                            *ELTPPO  
00035 *                                                                *ELTPPO  
00036 ******************************************************************ELTPPO  
00037 *                                                                *ELTPPO  
00038 *                      MAINTENANCE HISTORY                       *ELTPPO  
00039 *                                                                *ELTPPO  
00040 *  MOD     DATE     BY  DRPT                ACTION               *ELTPPO  
00041 * ----- ----------- --- ----- ---------------------------------- *ELTPPO  
00042 * 01.00 01-JUN-1987 REB       CREATED                            *ELTPPO  
00043 *                                                                *ELTPPO  
00044 * 01.01 02-SEP-1987 REB       REARRANGE ORDER OF G-TABS TO FOLLOW*ELTPPO  
00045 *                             FIXED SENTENCE. ALSO, MOVED THE    *ELTPPO  
00046 *                             SPILL OVER SENTENCE IN FRONT OF THE*ELTPPO  
00047 *                             G-TABS INFORMATION.                *ELTPPO  
00048 *                                                                *ELTPPO  
00049 * 01.02 22-SEP-1987 REB       ADDED LOGIC TO LINK ELGAOLCC SINCE *ELTPPO  
00050 *                             OUT-OF-POCKET ACCUMS HAVE BEEN     *ELTPPO  
00051 *                             ADDED TO CC LEVEL.                 *ELTPPO  
00052 *                                                                *ELTPPO  
00053 * 01.03 06-NOV-1990 JPB       CHANGED STORAGE MANAGEMENT.        *ELTPPO  
00054 *                                                                *ELTPPO  
00055 * 01.04 12-NOV-1990 JPB       CHANGED REFERENCES TO GCG-         *ELTPPO  
00056 *                             PARTICIPAT-PROV-OPTION TO REFLECT  *ELTPPO  
00057 *                             NEW FIELD SIZE.                    *ELTPPO  
00058 *                                                                *ELTPPO  
00059 * 01.05 11-JUN-1991 GEM       ADD CCP PARTIC IND TO PPO.         *ELTPPO  
00060 *                                                                *ELTPPO  
00061 * 01.06 18-FEB-1992 GEM       RESOLVE ASRA WHEN PROFESSIONAL     *ELTPPO  
00062 *                             PROCESSING SELECTED.               *ELTPPO  
00063 *                                                                *ELTPPO  
00064 * 01.07 16-FEB-1993 AKK       FOUND THAT ELGGXXC WAS BEING       *ELTPPO  
00065 *                             CALLED WHETHER GCCP TABULAR        *ELTPPO  
00066 *                             EXISTED OR NOT - CORRECTED THAT    *ELTPPO  
00067 ******************************************************************ELTPPO  
00068                                                                   ELTPPO  
00069  DATA DIVISION.                                                   ELTPPO  
00070                                                                   ELTPPO  
00071  WORKING-STORAGE SECTION.                                         ELTPPO  
00072  01  WS-MISC.                                                     ELTPPO  
00073      05  WS-BEGIN                        PIC X(26) VALUE          ELTPPO  
00074      '*** ELTPPO WS BEGINS ***'.                                  ELTPPO  
00075      05  WS-POINTER2                     POINTER.                 ELTPPO  
00076      05  WS-POINTER3                     POINTER.                 ELTPPO  
00077                                                                   ELTPPO  
00078  01  WS-SWITCHES.                                                 ELTPPO  
00079      05  ADDITIONAL-TEXT-SWITCH   PIC X(01) VALUE SPACE.          ELTPPO  
00080          88  ADDITIONAL-TEXT                VALUE 'A'.            ELTPPO  
00081          88  BLANK-LINE-NEEDED              VALUE 'B'.            ELTPPO  
00082      05  CONTINUED-PROCESSING-SW  PIC X(01) VALUE SPACE.          ELTPPO  
00083          88  DONE-PROCESSING                VALUE 'D'.            ELTPPO  
00084          88  PROCESSING-CMF-TEXT            VALUE 'P'.            ELTPPO  
00085      05  WS-TABULAR-SWITCH        PIC X(01) VALUE 'N'.            ELTPPO  
00086          88  TABULAR-IS-UNDEFINED           VALUE 'Y'.            ELTPPO  
00087      05  WS-PERIOD-SWITCH         PIC X(01) VALUE 'N'.            ELTPPO  
00088          88  PERIOD-NEEDED                  VALUE 'Y'.            ELTPPO  
00089      05  WS-APPROVAL-SOURCE-SW    PIC X     VALUE SPACE.          ELTPPO  
00090          88  NOT-HOLDING-APPROVAL-SRCE      VALUE 'N'.            ELTPPO  
00091          88  HOLDING-APPROVAL-SOURCE        VALUE 'H'.            ELTPPO  
00092                                                                   ELTPPO  
00093  01  WS-HOLD-AREA.                                                ELTPPO  
00094      05  WS-GPPO-PROV-SLOT-NO   COMP-3 PIC S9(07) VALUE +0.       ELTPPO  
00095                                                                   ELTPPO  
00096 **************************************************************    ELTPPO  
00097 ***                   PROGRAM CONSTANTS                           ELTPPO  
00098 **************************************************************    ELTPPO  
00099      05  WS-GRP                   PIC X(06) VALUE 'GROUP'.        ELTPPO  
00100      05  WS-GCCP                  PIC X(06) VALUE '#GCCP '.       ELTPPO  
00101      05  WS-GPPO                  PIC X(06) VALUE '#GPPO '.       ELTPPO  
00102      05  WS-INST                  PIC X(13) VALUE 'INSTITUTIONAL'.ELTPPO  
00103      05  WS-PROF                  PIC X(13) VALUE 'PROFESSIONAL '.ELTPPO  
00104      05  WS-SUPP                  PIC X(13) VALUE 'SUPPLEMENTAL '.ELTPPO  
00105      05  WS-APPROVAL              PIC X(09) VALUE 'APPROVAL.'.    ELTPPO  
00106                                                                   ELTPPO  
00107 **************************************************************    ELTPPO  
00108 ***                   HEADER  LINE                                ELTPPO  
00109 **************************************************************    ELTPPO  
00110      05  WS-HEADER-LINE.                                          ELTPPO  
00111          10  FILLER               PIC X(14) VALUE SPACES.         ELTPPO  
00112          10  FILLER               PIC X(34) VALUE                 ELTPPO  
00113          'PREFERRED PROVIDER OPTION PROGRAM '.                    ELTPPO  
00114          10  WS-HDR-LINE-BCBSMM   PIC X(13) VALUE SPACES.         ELTPPO  
00115          10  FILLER               PIC X(18) VALUE SPACES.         ELTPPO  
00116                                                                   ELTPPO  
00117 **************************************************************    ELTPPO  
00118 ***                   SCREEN BODY LINES                           ELTPPO  
00119 **************************************************************    ELTPPO  
00120  01  WS-SCREEN-LINE-AREA.                                         ELTPPO  
00121      05  WS-APPRVL-LINE.                                          ELTPPO  
00122          10  FILLER               PIC X(43) VALUE                 ELTPPO  
00123          'PREFERRED PROVIDER OPTION PROGRAM REQUIRES '.           ELTPPO  
00124          10  FILLER               PIC X(36) VALUE SPACES.         ELTPPO  
00125                                                                   ELTPPO  
00126      05  WS-ALT-PRICING-LINE-BC.                                  ELTPPO  
00127          10  FILLER               PIC X(52) VALUE                 ELTPPO  
00128          'THE ALTERNATE PRICING FOR INSTITUTIONAL SERVICES IS '.  ELTPPO  
00129          10  FILLER               PIC X(27) VALUE SPACES.         ELTPPO  
00130                                                                   ELTPPO  
00131      05  WS-ALT-PRICING-LINE-BS.                                  ELTPPO  
00132          10  FILLER               PIC X(51) VALUE                 ELTPPO  
00133          'THE ALTERNATE PRICING FOR PROFESSIONAL SERVICES IS '.   ELTPPO  
00134          10  FILLER               PIC X(28) VALUE SPACES.         ELTPPO  
00135                                                                   ELTPPO  
00136      05  WS-ALT-PRICING-LINE-MM.                                  ELTPPO  
00137          10  FILLER               PIC X(51) VALUE                 ELTPPO  
00138          'THE ALTERNATE PRICING FOR SUPPLEMENTAL SERVICES IS '.   ELTPPO  
00139          10  FILLER               PIC X(28) VALUE SPACES.         ELTPPO  
00140                                                                   ELTPPO  
00141      05  WS-BENE-REDUCT-LINE.                                     ELTPPO  
00142          10  FILLER               PIC X(47) VALUE                 ELTPPO  
00143          'DENIED OR REDUCED BENEFITS DUE TO THIS PROGRAM:'.       ELTPPO  
00144          10  FILLER               PIC X(32) VALUE SPACES.         ELTPPO  
00145                                                                   ELTPPO  
00146      05  WS-SPILL-OVER-LINE.                                      ELTPPO  
00147          10  FILLER               PIC X(51) VALUE                 ELTPPO  
00148          'UNPAID SERVICES AFTER BASIC BENEFITS REDUCTION ARE '.   ELTPPO  
00149          10  FILLER               PIC X(28) VALUE SPACES.         ELTPPO  
00150                                                                   ELTPPO  
00151 **************************************************************    ELTPPO  
00152 ** SPECIAL MESSAGE FOR THE NOT APPLICABLE                         ELTPPO  
00153 ** ALSO THE FIXED TEXT FOR TABULAR GPPO                           ELTPPO  
00154 **************************************************************    ELTPPO  
00155      05  WS-PPO-APPLIES.                                          ELTPPO  
00156          10  FILLER               PIC X(79) VALUE                 ELTPPO  
00157          'THE PREFERRED PROVIDER OPTION PROGRAM  APPLIES TO'.     ELTPPO  
00158                                                                   ELTPPO  
00159      05  WS-NOT-APPLICABLE-MSG.                                   ELTPPO  
00160          10  FILLER               PIC X(38) VALUE                 ELTPPO  
00161          'THE PREFERRED PROVIDER OPTION PROGRAM '.                ELTPPO  
00162          10  FILLER               PIC X(18) VALUE                 ELTPPO  
00163          'IS NOT APPLICABLE.'.                                    ELTPPO  
00164          10  FILLER               PIC X(23) VALUE SPACES.         ELTPPO  
00165                                                                   ELTPPO  
00166      05  WS-SPEC-PROV-MSG.                                        ELTPPO  
00167          10  FILLER               PIC X(79) VALUE                 ELTPPO  
00168          'THERE ARE SPECIAL PROVIDERS INCLUDED IN THIS COST CONTAIELTPPO  
00169 -        'NMENT PROGRAM.'.                                        ELTPPO  
00170                                                                   ELTPPO  
00171      05  WS-DISCLAIMER-MSG.                                       ELTPPO  
00172          10  FILLER               PIC X(79) VALUE                 ELTPPO  
00173          '*** SUBJECT TO OTHER CONTRACT LIMITATIONS ***'.         ELTPPO  
00174                                                                   ELTPPO  
00175      05  WS-NOT-APPLY-TO-LOB-BC.                                  ELTPPO  
00176          10  FILLER               PIC X(49) VALUE                 ELTPPO  
00177          'PREFERRED PROVIDER OPTION PROGRAM DOES NOT APPLY '.     ELTPPO  
00178          10  FILLER               PIC X(30) VALUE                 ELTPPO  
00179          'FOR INSTITUTIONAL BENEFITS'.                            ELTPPO  
00180                                                                   ELTPPO  
00181      05  WS-NOT-APPLY-TO-LOB-BS.                                  ELTPPO  
00182          10  FILLER               PIC X(49) VALUE                 ELTPPO  
00183          'PREFERRED PROVIDER OPTION PROGRAM DOES NOT APPLY '.     ELTPPO  
00184          10  FILLER               PIC X(30) VALUE                 ELTPPO  
00185          'FOR PROFESSIONAL BENEFITS.'.                            ELTPPO  
00186                                                                   ELTPPO  
00187      05  WS-NOT-APPLY-TO-LOB-MM.                                  ELTPPO  
00188          10  FILLER               PIC X(49) VALUE                 ELTPPO  
00189          'PREFERRED PROVIDER OPTION PROGRAM DOES NOT APPLY '.     ELTPPO  
00190          10  FILLER               PIC X(30) VALUE                 ELTPPO  
00191          'FOR SUPPLEMENTAL BENEFITS.'.                            ELTPPO  
00192                                                                   ELTPPO  
00193  LINKAGE SECTION.                                                 ELTPPO  
00194  01  DFHCOMMAREA.                                                 ELTPPO  
00195      COPY ELSCOMMC.                                               ELTPPO  
00196 /                                                                 ELTPPO  
00197      COPY ELSCIA2C.                                               ELTPPO  
00198 /                                                                 ELTPPO  
00199      COPY ELSCMDSC.                                               ELTPPO  
00200 /                                                                 ELTPPO  
00201      COPY ELSCMIFC.                                               ELTPPO  
00202 /                                                                 ELTPPO  
00203      COPY ELSIOPMC.                                               ELTPPO  
00204 /                                                                 ELTPPO  
00205      COPY ELSKEYSC.                                               ELTPPO  
00206 /                                                                 ELTPPO  
00207      COPY ELSOUTPC.                                               ELTPPO  
00208 /                                                                 ELTPPO  
00209      COPY ELSSRTPC.                                               ELTPPO  
00210 /                                                                 ELTPPO  
00211      COPY ELSTCWAC.                                               ELTPPO  
00212 /                                                                 ELTPPO  
00213      COPY ELSSSCBC.                                               ELTPPO  
00214 /                                                                 ELTPPO  
00215  01  GROUP-SPECIFIC-REC.                                          ELTPPO  
00216      COPY GCGROUPC.                                               ELTPPO  
00217 /                                                                 ELTPPO  
00218  01  GCCP-TABULAR-REC-AREA.                                       ELTPPO  
00219      COPY GCTGCCPC.                                               ELTPPO  
00220      EJECT                                                        ELTPPO  
00221  PROCEDURE DIVISION.                                              ELTPPO  
00222 ************************************************************      ELTPPO  
00223 *                                                          *      ELTPPO  
00224 *                    PROCEDURE DIVISION                    *      ELTPPO  
00225 *                                                          *      ELTPPO  
00226 ************************************************************      ELTPPO  
00227                                                                   ELTPPO  
00228                                                                   ELTPPO  
00229 ************************************************************      ELTPPO  
00230 *                                                          *      ELTPPO  
00231 *        PARTICIPATING PROVIDER OPTION                     *      ELTPPO  
00232 *                                                          *      ELTPPO  
00233 ************************************************************      ELTPPO  
00234  PARTICIPATING-PROVIDER-OPTION.                                   ELTPPO  
00235      PERFORM INITIALIZATION.                                      ELTPPO  
00236      PERFORM PROCESS.                                             ELTPPO  
00237      GOBACK.                                                      ELTPPO  
00238                                                                   ELTPPO  
00239                                                                   ELTPPO  
00240 ************************************************************      ELTPPO  
00241 *                                                          *      ELTPPO  
00242 *        INITIALIZATION                                    *      ELTPPO  
00243 *                                                          *      ELTPPO  
00244 ************************************************************      ELTPPO  
00245  INITIALIZATION.                                                  ELTPPO  
00246      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTPPO  
00247      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTPPO  
00248                                                                   ELTPPO  
00249                                                                   ELTPPO  
00250 ************************************************************      ELTPPO  
00251 *                                                          *      ELTPPO  
00252 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTPPO  
00253 *                                                          *      ELTPPO  
00254 ************************************************************      ELTPPO  
00255  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTPPO  
00256      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTPPO  
00257      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTPPO  
00258      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTPPO  
00259                                                                   ELTPPO  
00260                                                                   ELTPPO  
00261 ************************************************************      ELTPPO  
00262 *                                                          *      ELTPPO  
00263 *        CHECK FOR VALID COMMAREA                          *      ELTPPO  
00264 *                                                          *      ELTPPO  
00265 ************************************************************      ELTPPO  
00266  CHECK-FOR-VALID-COMMAREA.                                        ELTPPO  
00267      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTPPO  
00268          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTPPO  
00269                                                                   ELTPPO  
00270                                                                   ELTPPO  
00271 ************************************************************      ELTPPO  
00272 *                                                          *      ELTPPO  
00273 *        SIGNAL INVALID COMMAREA                           *      ELTPPO  
00274 *                                                          *      ELTPPO  
00275 ************************************************************      ELTPPO  
00276  SIGNAL-INVALID-COMMAREA.                                         ELTPPO  
00277      EXEC CICS ABEND                                              ELTPPO  
00278                ABCODE('EL01')                                     ELTPPO  
00279         END-EXEC.                                                 ELTPPO  
00280      EJECT                                                        ELTPPO  
00281                                                                   ELTPPO  
00282                                                                   ELTPPO  
00283 ************************************************************      ELTPPO  
00284 *                                                          *      ELTPPO  
00285 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTPPO  
00286 *                                                          *      ELTPPO  
00287 ************************************************************      ELTPPO  
00288  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTPPO  
00289      IF ECA-CIA-PTR = NULL                                        ELTPPO  
00290          PERFORM SIGNAL-INVALID-CIA                               ELTPPO  
00291      ELSE                                                         ELTPPO  
00292          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTPPO  
00293                                                                   ELTPPO  
00294                                                                   ELTPPO  
00295 ************************************************************      ELTPPO  
00296 *                                                          *      ELTPPO  
00297 *        SIGNAL INVALID CIA                                *      ELTPPO  
00298 *                                                          *      ELTPPO  
00299 ************************************************************      ELTPPO  
00300  SIGNAL-INVALID-CIA.                                              ELTPPO  
00301      EXEC CICS ABEND                                              ELTPPO  
00302                ABCODE('EL02')                                     ELTPPO  
00303         END-EXEC.                                                 ELTPPO  
00304                                                                   ELTPPO  
00305                                                                   ELTPPO  
00306 ************************************************************      ELTPPO  
00307 *                                                          *      ELTPPO  
00308 *        ESTABLISH ADDRESS OF CIA                          *      ELTPPO  
00309 *                                                          *      ELTPPO  
00310 ************************************************************      ELTPPO  
00311  ESTABLISH-ADDRESS-OF-CIA.                                        ELTPPO  
00312      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTPPO  
00313                            ADDRESS OF                             ELTPPO  
00314          CIA-ELS-COMMON-INTERFACE-AREA.                           ELTPPO  
00315      EJECT                                                        ELTPPO  
00316                                                                   ELTPPO  
00317                                                                   ELTPPO  
00318 ************************************************************      ELTPPO  
00319 *                                                          *      ELTPPO  
00320 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTPPO  
00321 *                                                          *      ELTPPO  
00322 ************************************************************      ELTPPO  
00323  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTPPO  
00324      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTPPO  
00325      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPPO  
00326                            ADDRESS OF                             ELTPPO  
00327          SSB-SELECTOR-STATUS-CTL-BLK.                             ELTPPO  
00328      IF CIA-RC-PTR-NULL                                           ELTPPO  
00329          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPPO  
00330                                                                   ELTPPO  
00331                                                                   ELTPPO  
00332 ************************************************************      ELTPPO  
00333 *                                                          *      ELTPPO  
00334 *        SIGNAL UNALLOC AREA ERROR                         *      ELTPPO  
00335 *                                                          *      ELTPPO  
00336 ************************************************************      ELTPPO  
00337  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTPPO  
00338      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTPPO  
00339      PERFORM SIGNAL-ABEND.                                        ELTPPO  
00340                                                                   ELTPPO  
00341                                                                   ELTPPO  
00342 ************************************************************      ELTPPO  
00343 *                                                          *      ELTPPO  
00344 *        SIGNAL ABEND                                      *      ELTPPO  
00345 *                                                          *      ELTPPO  
00346 ************************************************************      ELTPPO  
00347  SIGNAL-ABEND.                                                    ELTPPO  
00348      EXEC CICS ABEND                                              ELTPPO  
00349                ABCODE(CIA-ABCODE)                                 ELTPPO  
00350         END-EXEC.                                                 ELTPPO  
00351      EJECT                                                        ELTPPO  
00352                                                                   ELTPPO  
00353                                                                   ELTPPO  
00354 ************************************************************      ELTPPO  
00355 *                                                          *      ELTPPO  
00356 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTPPO  
00357 *                                                          *      ELTPPO  
00358 ************************************************************      ELTPPO  
00359  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTPPO  
00360      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTPPO  
00361      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTPPO  
00362      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTPPO  
00363      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTPPO  
00364      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTPPO  
00365      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTPPO  
00366      PERFORM ESTABLISH-ADDRESSABILITY-OF-CS.                      ELTPPO  
00367                                                                   ELTPPO  
00368                                                                   ELTPPO  
00369 ************************************************************      ELTPPO  
00370 *                                                          *      ELTPPO  
00371 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTPPO  
00372 *                                                          *      ELTPPO  
00373 ************************************************************      ELTPPO  
00374  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTPPO  
00375      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTPPO  
00376      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPPO  
00377                            ADDRESS OF                             ELTPPO  
00378          CMF-CODES-MANUAL-INTERFACE.                              ELTPPO  
00379      IF CIA-RC-PTR-NULL                                           ELTPPO  
00380          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPPO  
00381      EJECT                                                        ELTPPO  
00382                                                                   ELTPPO  
00383                                                                   ELTPPO  
00384 ************************************************************      ELTPPO  
00385 *                                                          *      ELTPPO  
00386 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTPPO  
00387 *                                                          *      ELTPPO  
00388 ************************************************************      ELTPPO  
00389  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTPPO  
00390      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTPPO  
00391      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPPO  
00392                            ADDRESS OF                             ELTPPO  
00393          COF-OUTPUT-INTERFACE.                                    ELTPPO  
00394      IF CIA-RC-PTR-NULL                                           ELTPPO  
00395          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPPO  
00396      EJECT                                                        ELTPPO  
00397                                                                   ELTPPO  
00398                                                                   ELTPPO  
00399 ************************************************************      ELTPPO  
00400 *                                                          *      ELTPPO  
00401 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTPPO  
00402 *                                                          *      ELTPPO  
00403 ************************************************************      ELTPPO  
00404  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTPPO  
00405      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTPPO  
00406      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPPO  
00407                            ADDRESS OF                             ELTPPO  
00408          SRP-SUBROUTINE-PARAMETERS.                               ELTPPO  
00409      IF CIA-RC-PTR-NULL                                           ELTPPO  
00410          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPPO  
00411      EJECT                                                        ELTPPO  
00412                                                                   ELTPPO  
00413                                                                   ELTPPO  
00414 ************************************************************      ELTPPO  
00415 *                                                          *      ELTPPO  
00416 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTPPO  
00417 *                                                          *      ELTPPO  
00418 ************************************************************      ELTPPO  
00419  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTPPO  
00420      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTPPO  
00421      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPPO  
00422                            ADDRESS OF                             ELTPPO  
00423          TCAR-COMPRESSION-WORK-AREA.                              ELTPPO  
00424      IF CIA-RC-PTR-NULL                                           ELTPPO  
00425          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPPO  
00426      EJECT                                                        ELTPPO  
00427                                                                   ELTPPO  
00428                                                                   ELTPPO  
00429 ************************************************************      ELTPPO  
00430 *                                                          *      ELTPPO  
00431 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTPPO  
00432 *                                                          *      ELTPPO  
00433 ************************************************************      ELTPPO  
00434  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTPPO  
00435      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTPPO  
00436      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPPO  
00437                            ADDRESS OF                             ELTPPO  
00438          KWA-FILE-KEY-WORK-AREA.                                  ELTPPO  
00439      IF CIA-RC-PTR-NULL                                           ELTPPO  
00440          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPPO  
00441      EJECT                                                        ELTPPO  
00442                                                                   ELTPPO  
00443                                                                   ELTPPO  
00444 ************************************************************      ELTPPO  
00445 *                                                          *      ELTPPO  
00446 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELTPPO  
00447 *                                                          *      ELTPPO  
00448 ************************************************************      ELTPPO  
00449  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTPPO  
00450      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTPPO  
00451      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPPO  
00452                            ADDRESS OF                             ELTPPO  
00453          GROUP-SPECIFIC-REC.                                      ELTPPO  
00454      IF CIA-RC-PTR-NULL                                           ELTPPO  
00455          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPPO  
00456      EJECT                                                        ELTPPO  
00457                                                                   ELTPPO  
00458                                                                   ELTPPO  
00459 ************************************************************      ELTPPO  
00460 *                                                          *      ELTPPO  
00461 *        ESTABLISH ADDRESSABILITY OF CST CONTAINMENT PROGRA*      ELTPPO  
00462 *                                                          *      ELTPPO  
00463 ************************************************************      ELTPPO  
00464  ESTABLISH-ADDRESSABILITY-OF-CS.                                  ELTPPO  
00465      SET CIA-GCTABULR-DDN TO TRUE.                                ELTPPO  
00466      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPPO  
00467                            ADDRESS OF                             ELTPPO  
00468          GCCP-TABULAR-REC-AREA.                                   ELTPPO  
00469      IF CIA-RC-PTR-NULL                                           ELTPPO  
00470          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPPO  
00471      EJECT                                                        ELTPPO  
00472                                                                   ELTPPO  
00473                                                                   ELTPPO  
00474 ************************************************************      ELTPPO  
00475 *                                                          *      ELTPPO  
00476 *        PROCESS                                           *      ELTPPO  
00477 *                                                          *      ELTPPO  
00478 ************************************************************      ELTPPO  
00479  PROCESS.                                                         ELTPPO  
00480      IF GCG-PARTICIPAT-PROV-OPTION = ZERO                         ELTPPO  
00481          PERFORM GENERATE-NOT-APPLICABLE-MESSAG                   ELTPPO  
00482      ELSE                                                         ELTPPO  
00483          PERFORM GENERATE-PPO-TEXT.                               ELTPPO  
00484      PERFORM TERMINATE-OUTPUT.                                    ELTPPO  
00485                                                                   ELTPPO  
00486                                                                   ELTPPO  
00487 ************************************************************      ELTPPO  
00488 *                                                          *      ELTPPO  
00489 *        EJECT NEW PAGE                                    *      ELTPPO  
00490 *                                                          *      ELTPPO  
00491 ************************************************************      ELTPPO  
00492  EJECT-NEW-PAGE.                                                  ELTPPO  
00493      SET COF-NEW-PAGE      TO TRUE.                               ELTPPO  
00494      MOVE WS-HEADER-LINE   TO COF-HDR-LINE                        ELTPPO  
00495          (COF-NBR-HDR-LINES).                                     ELTPPO  
00496      PERFORM LINK-TO-OUTPUT.                                      ELTPPO  
00497      EJECT                                                        ELTPPO  
00498                                                                   ELTPPO  
00499                                                                   ELTPPO  
00500 ************************************************************      ELTPPO  
00501 *                                                          *      ELTPPO  
00502 *        TRANSLATE AND DISPLAY PPO IND                     *      ELTPPO  
00503 *                                                          *      ELTPPO  
00504 ************************************************************      ELTPPO  
00505  TRANSLATE-AND-DISPLAY-PPO-IND.                                   ELTPPO  
00506      INITIALIZE TCAR-FROM-AREA.                                   ELTPPO  
00507      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
00508      MOVE WS-PPO-APPLIES TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELTPPO  
00509      ADD  +1 TO TCAR-FROM-SUB.                                    ELTPPO  
00510      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPPO  
00511      SET PERIOD-NEEDED TO TRUE.                                   ELTPPO  
00512      MOVE GCG-PARTICIPAT-PROV-OPTION TO CMF-CODE-VALUE.           ELTPPO  
00513      MOVE 'PARTICIPAT-PROV-OPTION' TO CMF-ELEMENT-SYSTEM-NAME.    ELTPPO  
00514      MOVE WS-GRP TO CMF-RECORD-PREFIX.                            ELTPPO  
00515      EXEC CICS LINK                                               ELTPPO  
00516                PROGRAM ('ELUCMIF')                                ELTPPO  
00517                COMMAREA (DFHCOMMAREA)                             ELTPPO  
00518         END-EXEC.                                                 ELTPPO  
00519      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPPO  
00520      MOVE SPACE TO ADDITIONAL-TEXT-SWITCH.                        ELTPPO  
00521                                                                   ELTPPO  
00522                                                                   ELTPPO  
00523 ************************************************************      ELTPPO  
00524 *                                                          *      ELTPPO  
00525 *        SET UP OUTPUT SUBSCRIPTS                          *      ELTPPO  
00526 *                                                          *      ELTPPO  
00527 ************************************************************      ELTPPO  
00528  SET-UP-OUTPUT-SUBSCRIPTS.                                        ELTPPO  
00529      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPPO  
00530      MOVE +2 TO COF-NBR-HDR-LINES.                                ELTPPO  
00531                                                                   ELTPPO  
00532                                                                   ELTPPO  
00533 ************************************************************      ELTPPO  
00534 *                                                          *      ELTPPO  
00535 *        GENERATE NOT APPLICABLE MESSAGE                   *      ELTPPO  
00536 *                                                          *      ELTPPO  
00537 ************************************************************      ELTPPO  
00538  GENERATE-NOT-APPLICABLE-MESSAG.                                  ELTPPO  
00539      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTPPO  
00540      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTPPO  
00541      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPPO  
00542      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTPPO  
00543          (COF-NBR-DTL-LINES).                                     ELTPPO  
00544      PERFORM EJECT-NEW-PAGE.                                      ELTPPO  
00545      EJECT                                                        ELTPPO  
00546                                                                   ELTPPO  
00547                                                                   ELTPPO  
00548 ************************************************************      ELTPPO  
00549 *                                                          *      ELTPPO  
00550 *        GENERATE PPO TEXT                                 *      ELTPPO  
00551 *                                                          *      ELTPPO  
00552 ************************************************************      ELTPPO  
00553  GENERATE-PPO-TEXT.                                               ELTPPO  
00554      SET WS-POINTER2 TO NULLS.                                    ELTPPO  
00555      SET WS-POINTER3 TO NULLS.                                    ELTPPO  
00556      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTPPO  
00557      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTPPO  
00558                            WS-POINTER2.                           ELTPPO  
00559      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTPPO  
00560      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTPPO  
00561                            WS-POINTER3.                           ELTPPO  
00562      PERFORM SEARCH-FOR-GCCP-TABULAR.                             ELTPPO  
00563      PERFORM DETERMINE-SELECTION.                                 ELTPPO  
00564      EJECT                                                        ELTPPO  
00565                                                                   ELTPPO  
00566                                                                   ELTPPO  
00567 ************************************************************      ELTPPO  
00568 *                                                          *      ELTPPO  
00569 *        TERMINATE OUTPUT                                  *      ELTPPO  
00570 *                                                          *      ELTPPO  
00571 ************************************************************      ELTPPO  
00572  TERMINATE-OUTPUT.                                                ELTPPO  
00573      SET COF-END TO TRUE.                                         ELTPPO  
00574      PERFORM LINK-TO-OUTPUT.                                      ELTPPO  
00575      EJECT                                                        ELTPPO  
00576                                                                   ELTPPO  
00577                                                                   ELTPPO  
00578 ************************************************************      ELTPPO  
00579 *                                                          *      ELTPPO  
00580 *        SEARCH FOR GCCP TABULAR                           *      ELTPPO  
00581 *                                                          *      ELTPPO  
00582 ************************************************************      ELTPPO  
00583  SEARCH-FOR-GCCP-TABULAR.                                         ELTPPO  
00584      SET GCG-INDEX TO +1.                                         ELTPPO  
00585      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTPPO  
00586         AT END                                                    ELTPPO  
00587              MOVE ZEROES TO KWA-PROVISION-SLOT-NO                 ELTPPO  
00588         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GCCP                     ELTPPO  
00589              MOVE GCG-TAB-ID (GCG-INDEX)                          ELTPPO  
00590                  TO KWA-PROVISION-ID                              ELTPPO  
00591              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTPPO  
00592                  TO KWA-PROVISION-SLOT-NO                         ELTPPO  
00593         END-SEARCH.                                               ELTPPO  
00594      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELTPPO  
00595          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTPPO  
00596      PERFORM GET-GCCP-TABULAR.                                    ELTPPO  
00597      PERFORM SEARCH-THE-GSS-ENTRY.                                ELTPPO  
00598      EJECT                                                        ELTPPO  
00599                                                                   ELTPPO  
00600                                                                   ELTPPO  
00601 ************************************************************      ELTPPO  
00602 *                                                          *      ELTPPO  
00603 *        TRANSLATE APPROVAL SOURCE                         *      ELTPPO  
00604 *                                                          *      ELTPPO  
00605 ************************************************************      ELTPPO  
00606  TRANSLATE-APPROVAL-SOURCE.                                       ELTPPO  
00607      INITIALIZE WS-PERIOD-SWITCH.                                 ELTPPO  
00608      INITIALIZE TCAR-FROM-AREA.                                   ELTPPO  
00609      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
00610      MOVE WS-APPRVL-LINE TO TCAR-FROM-LINE                        ELTPPO  
00611          (TCAR-FROM-SUB).                                         ELTPPO  
00612      ADD  +1 TO TCAR-FROM-SUB.                                    ELTPPO  
00613      SET ADDITIONAL-TEXT TO TRUE.                                 ELTPPO  
00614      IF NOT-HOLDING-APPROVAL-SRCE                                 ELTPPO  
00615          PERFORM GET-APPROVAL-TRANSLATION                         ELTPPO  
00616      ELSE                                                         ELTPPO  
00617          PERFORM USE-EXISTING-APPROVAL-TRANSLAT.                  ELTPPO  
00618      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPPO  
00619      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTPPO  
00620      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTPPO  
00621                            WS-POINTER3.                           ELTPPO  
00622      MOVE WS-APPROVAL TO TCAR-FROM-LINE                           ELTPPO  
00623          (TCAR-FROM-SUB).                                         ELTPPO  
00624      PERFORM FINISH-SENTENCE.                                     ELTPPO  
00625      EJECT                                                        ELTPPO  
00626                                                                   ELTPPO  
00627                                                                   ELTPPO  
00628 ************************************************************      ELTPPO  
00629 *                                                          *      ELTPPO  
00630 *        GET APPROVAL TRANSLATION                          *      ELTPPO  
00631 *                                                          *      ELTPPO  
00632 ************************************************************      ELTPPO  
00633  GET-APPROVAL-TRANSLATION.                                        ELTPPO  
00634      MOVE GSS-PP-APPROVAL-SRC-IND (GSS-INDEX) TO CMF-CODE-VALUE.  ELTPPO  
00635      MOVE   'PP-APPROVAL-SRC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTPPO  
00636      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTPPO  
00637      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTPPO  
00638      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTPPO  
00639                            ADDRESS OF CMF-DESCR.                  ELTPPO  
00640      EJECT                                                        ELTPPO  
00641                                                                   ELTPPO  
00642                                                                   ELTPPO  
00643 ************************************************************      ELTPPO  
00644 *                                                          *      ELTPPO  
00645 *        USE EXISTING APPROVAL TRANSLATION                 *      ELTPPO  
00646 *                                                          *      ELTPPO  
00647 ************************************************************      ELTPPO  
00648  USE-EXISTING-APPROVAL-TRANSLAT.                                  ELTPPO  
00649      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTPPO  
00650      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTPPO  
00651                            ADDRESS OF CMF-DESCR.                  ELTPPO  
00652      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTPPO  
00653      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTPPO  
00654                            WS-POINTER2.                           ELTPPO  
00655      EJECT                                                        ELTPPO  
00656                                                                   ELTPPO  
00657                                                                   ELTPPO  
00658 ************************************************************      ELTPPO  
00659 *                                                          *      ELTPPO  
00660 *        FINISH SENTENCE                                   *      ELTPPO  
00661 *                                                          *      ELTPPO  
00662 ************************************************************      ELTPPO  
00663  FINISH-SENTENCE.                                                 ELTPPO  
00664      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPPO  
00665      PERFORM REFORMAT-AND-WRITE-TEXT.                             ELTPPO  
00666                                                                   ELTPPO  
00667                                                                   ELTPPO  
00668 ************************************************************      ELTPPO  
00669 *                                                          *      ELTPPO  
00670 *        TRANSLATE AND DISPLAY CODE VALUE                  *      ELTPPO  
00671 *                                                          *      ELTPPO  
00672 ************************************************************      ELTPPO  
00673  TRANSLATE-AND-DISPLAY-CODE-VAL.                                  ELTPPO  
00674      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTPPO  
00675      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPPO  
00676                                                                   ELTPPO  
00677                                                                   ELTPPO  
00678 ************************************************************      ELTPPO  
00679 *                                                          *      ELTPPO  
00680 *        SIGNAL UNDEFINED TABULAR ERROR                    *      ELTPPO  
00681 *                                                          *      ELTPPO  
00682 ************************************************************      ELTPPO  
00683  SIGNAL-UNDEFINED-TABULAR-ERROR.                                  ELTPPO  
00684      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTPPO  
00685      PERFORM SIGNAL-ABEND.                                        ELTPPO  
00686      EJECT                                                        ELTPPO  
00687                                                                   ELTPPO  
00688                                                                   ELTPPO  
00689 ************************************************************      ELTPPO  
00690 *                                                          *      ELTPPO  
00691 *        DETERMINE SELECTION                               *      ELTPPO  
00692 *                                                          *      ELTPPO  
00693 ************************************************************      ELTPPO  
00694  DETERMINE-SELECTION.                                             ELTPPO  
00695      SET NOT-HOLDING-APPROVAL-SRCE TO TRUE.                       ELTPPO  
00696      IF SSB-PROV-CLASS-INST OR                                    ELTPPO  
00697                   SSB-PROV-CLASS-BOTH                             ELTPPO  
00698          PERFORM CREATE-INSTITUTIONAL-SCREEN.                     ELTPPO  
00699      IF SSB-PROV-CLASS-PROF OR                                    ELTPPO  
00700                   SSB-PROV-CLASS-BOTH                             ELTPPO  
00701          PERFORM CREATE-PROFESSIONAL-SCREEN.                      ELTPPO  
00702      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL '03' OR '04' OR       ELTPPO  
00703                    '06' OR '08')                                  ELTPPO  
00704          PERFORM CREATE-SUPPLEMENTAL-SCREEN.                      ELTPPO  
00705                                                                   ELTPPO  
00706                                                                   ELTPPO  
00707 ************************************************************      ELTPPO  
00708 *                                                          *      ELTPPO  
00709 *        CREATE INSTITUTIONAL SCREEN                       *      ELTPPO  
00710 *                                                          *      ELTPPO  
00711 ************************************************************      ELTPPO  
00712  CREATE-INSTITUTIONAL-SCREEN.                                     ELTPPO  
00713      MOVE WS-INST TO WS-HDR-LINE-BCBSMM.                          ELTPPO  
00714      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTPPO  
00715      IF GSS-PP-BC-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTPPO  
00716          ZEROES                                                   ELTPPO  
00717                 AND LOW-VALUES                                    ELTPPO  
00718          PERFORM GENERATE-INSTITUTIONAL-TEXT                      ELTPPO  
00719      ELSE                                                         ELTPPO  
00720          PERFORM GENERATE-INSTITUTIONAL-NOT-APP.                  ELTPPO  
00721      EJECT                                                        ELTPPO  
00722                                                                   ELTPPO  
00723                                                                   ELTPPO  
00724 ************************************************************      ELTPPO  
00725 *                                                          *      ELTPPO  
00726 *        GENERATE INSTITUTIONAL TEXT                       *      ELTPPO  
00727 *                                                          *      ELTPPO  
00728 ************************************************************      ELTPPO  
00729  GENERATE-INSTITUTIONAL-TEXT.                                     ELTPPO  
00730      PERFORM EJECT-NEW-PAGE.                                      ELTPPO  
00731      PERFORM TRANSLATE-AND-DISPLAY-PPO-IND.                       ELTPPO  
00732      IF GSS-PP-APPROVAL-SRC-IND (GSS-INDEX) NOT EQUAL SPACES      ELTPPO  
00733          AND                                                      ELTPPO  
00734                 ZEROES AND LOW-VALUES                             ELTPPO  
00735          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTPPO  
00736      PERFORM TRANSLATE-BC-IND.                                    ELTPPO  
00737      IF GSS-PP-BC-ALT-PRICING-METHOD (GSS-INDEX) NOT EQUAL        ELTPPO  
00738          SPACES AND                                               ELTPPO  
00739                 ZEROES AND LOW-VALUES                             ELTPPO  
00740          PERFORM TRANSLATE-BC-ALT-PRIC.                           ELTPPO  
00741      SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTPPO  
00742      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTPPO  
00743      IF GSS-PP-BC-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES AND    ELTPPO  
00744          ZEROES                                                   ELTPPO  
00745                 AND LOW-VALUES                                    ELTPPO  
00746          PERFORM TRANSLATE-BC-CALC-METHOD.                        ELTPPO  
00747      IF GSS-PP-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTPPO  
00748          SPACES AND                                               ELTPPO  
00749                 ZEROES AND LOW-VALUES                             ELTPPO  
00750          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTPPO  
00751      IF ((GSS-PP-BC-DEDUCT-APPLIC-IND (GSS-INDEX) NOT EQUAL       ELTPPO  
00752          SPACES AND                                               ELTPPO  
00753                  ZEROES AND LOW-VALUES)) OR                       ELTPPO  
00754                ((GSS-PP-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT        ELTPPO  
00755          EQUAL SPACES AND                                         ELTPPO  
00756                  ZEROES AND LOW-VALUES)) OR                       ELTPPO  
00757                ((GSS-PP-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT      ELTPPO  
00758          EQUAL SPACES AND                                         ELTPPO  
00759                  ZEROES AND LOW-VALUES))                          ELTPPO  
00760          PERFORM GENERATE-BC-BENE-REDUCT-TEXT.                    ELTPPO  
00761      IF GSS-PP-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL SPACES AND    ELTPPO  
00762          ZEROES                                                   ELTPPO  
00763                 AND LOW-VALUES                                    ELTPPO  
00764          PERFORM GENERATE-SPILL-OVER-TEXT.                        ELTPPO  
00765      PERFORM GENERATE-SPECIAL-PROVIDERS-SEN.                      ELTPPO  
00766      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTPPO  
00767      EJECT                                                        ELTPPO  
00768                                                                   ELTPPO  
00769                                                                   ELTPPO  
00770 ************************************************************      ELTPPO  
00771 *                                                          *      ELTPPO  
00772 *        GENERATE INSTITUTIONAL NOT APPLICABLE             *      ELTPPO  
00773 *                                                          *      ELTPPO  
00774 ************************************************************      ELTPPO  
00775  GENERATE-INSTITUTIONAL-NOT-APP.                                  ELTPPO  
00776      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTPPO  
00777      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPPO  
00778      MOVE WS-NOT-APPLY-TO-LOB-BC TO COF-DTL-LINE                  ELTPPO  
00779          (COF-NBR-DTL-LINES).                                     ELTPPO  
00780      PERFORM EJECT-NEW-PAGE.                                      ELTPPO  
00781      EJECT                                                        ELTPPO  
00782                                                                   ELTPPO  
00783                                                                   ELTPPO  
00784 ************************************************************      ELTPPO  
00785 *                                                          *      ELTPPO  
00786 *        GENERATE DISCLAIMER MESSAGE                       *      ELTPPO  
00787 *                                                          *      ELTPPO  
00788 ************************************************************      ELTPPO  
00789  GENERATE-DISCLAIMER-MESSAGE.                                     ELTPPO  
00790      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPPO  
00791      MOVE WS-DISCLAIMER-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).  ELTPPO  
00792      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPPO  
00793      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTPPO  
00794      PERFORM LINK-TO-OUTPUT.                                      ELTPPO  
00795      EJECT                                                        ELTPPO  
00796                                                                   ELTPPO  
00797                                                                   ELTPPO  
00798 ************************************************************      ELTPPO  
00799 *                                                          *      ELTPPO  
00800 *        TRANSLATE BC IND                                  *      ELTPPO  
00801 *                                                          *      ELTPPO  
00802 ************************************************************      ELTPPO  
00803  TRANSLATE-BC-IND.                                                ELTPPO  
00804      INITIALIZE TCAR-FROM-AREA.                                   ELTPPO  
00805      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
00806      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPPO  
00807      SET PERIOD-NEEDED TO TRUE.                                   ELTPPO  
00808      MOVE GSS-PP-BC-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTPPO  
00809      MOVE   'PP-BC-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTPPO  
00810      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPPO  
00811      EJECT                                                        ELTPPO  
00812                                                                   ELTPPO  
00813                                                                   ELTPPO  
00814 ************************************************************      ELTPPO  
00815 *                                                          *      ELTPPO  
00816 *        TRANSLATE BC CALC METHOD                          *      ELTPPO  
00817 *                                                          *      ELTPPO  
00818 ************************************************************      ELTPPO  
00819  TRANSLATE-BC-CALC-METHOD.                                        ELTPPO  
00820      INITIALIZE TCAR-FROM-AREA.                                   ELTPPO  
00821      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
00822      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPPO  
00823      SET PERIOD-NEEDED TO TRUE.                                   ELTPPO  
00824      MOVE GSS-PP-BC-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTPPO  
00825      MOVE   'PP-BC-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTPPO  
00826      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPPO  
00827      EJECT                                                        ELTPPO  
00828                                                                   ELTPPO  
00829                                                                   ELTPPO  
00830 ************************************************************      ELTPPO  
00831 *                                                          *      ELTPPO  
00832 *        TRANSLATE BC ALT PRIC                             *      ELTPPO  
00833 *                                                          *      ELTPPO  
00834 ************************************************************      ELTPPO  
00835  TRANSLATE-BC-ALT-PRIC.                                           ELTPPO  
00836      INITIALIZE TCAR-FROM-AREA.                                   ELTPPO  
00837      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
00838      MOVE WS-ALT-PRICING-LINE-BC TO TCAR-FROM-LINE                ELTPPO  
00839          (TCAR-FROM-SUB).                                         ELTPPO  
00840      ADD  +1 TO TCAR-FROM-SUB.                                    ELTPPO  
00841      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPPO  
00842      SET PERIOD-NEEDED TO TRUE.                                   ELTPPO  
00843      MOVE GSS-PP-BC-ALT-PRICING-METHOD (GSS-INDEX) TO             ELTPPO  
00844          CMF-CODE-VALUE.                                          ELTPPO  
00845      MOVE  'PP-BC-ALT-PRICING-METHOD' TO CMF-ELEMENT-SYSTEM-NAME. ELTPPO  
00846      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPPO  
00847      EJECT                                                        ELTPPO  
00848                                                                   ELTPPO  
00849                                                                   ELTPPO  
00850 ************************************************************      ELTPPO  
00851 *                                                          *      ELTPPO  
00852 *        GENERATE ACCUM TABULAR DATA                       *      ELTPPO  
00853 *                                                          *      ELTPPO  
00854 ************************************************************      ELTPPO  
00855  GENERATE-ACCUM-TABULAR-DATA.                                     ELTPPO  
00856      PERFORM GENERATE-COINSURANCE.                                ELTPPO  
00857      PERFORM GENERATE-COPAY.                                      ELTPPO  
00858      PERFORM GENERATE-DEDUCTIBLE.                                 ELTPPO  
00859      PERFORM GENERATE-MAXIMUM.                                    ELTPPO  
00860      PERFORM GENERATE-OUT-OF-POCKET.                              ELTPPO  
00861      EJECT                                                        ELTPPO  
00862                                                                   ELTPPO  
00863                                                                   ELTPPO  
00864 ************************************************************      ELTPPO  
00865 *                                                          *      ELTPPO  
00866 *        GENERATE COMBINED BENEFITS REDUCTION TEXT         *      ELTPPO  
00867 *                                                          *      ELTPPO  
00868 ************************************************************      ELTPPO  
00869  GENERATE-COMBINED-BENEFITS-RED.                                  ELTPPO  
00870      MOVE 'PP' TO SRP-COST-CONT-TYPE.                             ELTPPO  
00871      MOVE 'PREFERRED PROVIDER OPTION PROGRAM' TO SRP-CCP-NAME.    ELTPPO  
00872      MOVE GSS-PP-COMB-BENE-REDUCT-IND (GSS-INDEX)                 ELTPPO  
00873                  TO SRP-CCP-COMB-BENE-REDUCT-IND.                 ELTPPO  
00874      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELTPPO  
00875      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTPPO  
00876                            ADDRESS OF                             ELTPPO  
00877          GCCP-TABULAR-REC-AREA.                                   ELTPPO  
00878      PERFORM CALL-CBRI-INTERFACE.                                 ELTPPO  
00879      EJECT                                                        ELTPPO  
00880                                                                   ELTPPO  
00881                                                                   ELTPPO  
00882 ************************************************************      ELTPPO  
00883 *                                                          *      ELTPPO  
00884 *        GENERATE BC BENE REDUCT TEXT                      *      ELTPPO  
00885 *                                                          *      ELTPPO  
00886 ************************************************************      ELTPPO  
00887  GENERATE-BC-BENE-REDUCT-TEXT.                                    ELTPPO  
00888      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPPO  
00889      MOVE WS-BENE-REDUCT-LINE TO COF-DTL-LINE                     ELTPPO  
00890          (COF-NBR-DTL-LINES).                                     ELTPPO  
00891      PERFORM LINK-TO-OUTPUT.                                      ELTPPO  
00892      INITIALIZE ADDITIONAL-TEXT-SWITCH.                           ELTPPO  
00893      INITIALIZE WS-PERIOD-SWITCH.                                 ELTPPO  
00894      INITIALIZE TCAR-FROM-AREA.                                   ELTPPO  
00895      IF GSS-PP-BC-DEDUCT-APPLIC-IND (GSS-INDEX) NOT EQUAL         ELTPPO  
00896          SPACES                                                   ELTPPO  
00897                 AND ZEROES AND LOW-VALUES                         ELTPPO  
00898          PERFORM TRANSLATE-BC-DEDUCT-IND.                         ELTPPO  
00899      IF GSS-PP-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTPPO  
00900          SPACES                                                   ELTPPO  
00901                 AND ZEROES AND LOW-VALUES                         ELTPPO  
00902          PERFORM TRANSLATE-BC-OPEX-APPLIC.                        ELTPPO  
00903      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPPO  
00904      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPPO  
00905      PERFORM LINK-TO-OUTPUT.                                      ELTPPO  
00906      IF GSS-PP-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTPPO  
00907          SPACES                                                   ELTPPO  
00908                 AND ZEROES AND LOW-VALUES                         ELTPPO  
00909          PERFORM TRANSLATE-BC-OPEX-OVERRIDE.                      ELTPPO  
00910      EJECT                                                        ELTPPO  
00911                                                                   ELTPPO  
00912                                                                   ELTPPO  
00913 ************************************************************      ELTPPO  
00914 *                                                          *      ELTPPO  
00915 *        TRANSLATE BC DEDUCT IND                           *      ELTPPO  
00916 *                                                          *      ELTPPO  
00917 ************************************************************      ELTPPO  
00918  TRANSLATE-BC-DEDUCT-IND.                                         ELTPPO  
00919      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
00920      MOVE GSS-PP-BC-DEDUCT-APPLIC-IND (GSS-INDEX) TO              ELTPPO  
00921          CMF-CODE-VALUE.                                          ELTPPO  
00922      MOVE 'PP-BC-DEDUCT-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTPPO  
00923      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPPO  
00924      EJECT                                                        ELTPPO  
00925                                                                   ELTPPO  
00926                                                                   ELTPPO  
00927 ************************************************************      ELTPPO  
00928 *                                                          *      ELTPPO  
00929 *        TRANSLATE BC OPEX APPLIC                          *      ELTPPO  
00930 *                                                          *      ELTPPO  
00931 ************************************************************      ELTPPO  
00932  TRANSLATE-BC-OPEX-APPLIC.                                        ELTPPO  
00933      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
00934      MOVE GSS-PP-BC-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTPPO  
00935          CMF-CODE-VALUE.                                          ELTPPO  
00936      MOVE 'PP-BC-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTPPO  
00937      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPPO  
00938      EJECT                                                        ELTPPO  
00939                                                                   ELTPPO  
00940                                                                   ELTPPO  
00941 ************************************************************      ELTPPO  
00942 *                                                          *      ELTPPO  
00943 *        TRANSLATE BC OPEX OVERRIDE                        *      ELTPPO  
00944 *                                                          *      ELTPPO  
00945 ************************************************************      ELTPPO  
00946  TRANSLATE-BC-OPEX-OVERRIDE.                                      ELTPPO  
00947      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
00948      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPPO  
00949      SET PERIOD-NEEDED TO TRUE.                                   ELTPPO  
00950      MOVE GSS-PP-BC-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTPPO  
00951          CMF-CODE-VALUE.                                          ELTPPO  
00952      MOVE 'PP-BC-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTPPO  
00953      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPPO  
00954      EJECT                                                        ELTPPO  
00955                                                                   ELTPPO  
00956                                                                   ELTPPO  
00957 ************************************************************      ELTPPO  
00958 *                                                          *      ELTPPO  
00959 *        CREATE PROFESSIONAL SCREEN                        *      ELTPPO  
00960 *                                                          *      ELTPPO  
00961 ************************************************************      ELTPPO  
00962  CREATE-PROFESSIONAL-SCREEN.                                      ELTPPO  
00963      MOVE WS-PROF TO WS-HDR-LINE-BCBSMM.                          ELTPPO  
00964      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTPPO  
00965      IF GSS-PP-BS-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTPPO  
00966          ZEROES                                                   ELTPPO  
00967                 AND LOW-VALUES                                    ELTPPO  
00968          PERFORM GENERATE-PROFESSIONAL-TEXT                       ELTPPO  
00969      ELSE                                                         ELTPPO  
00970          PERFORM GENERATE-PROFESSIONAL-NOT-APPL.                  ELTPPO  
00971      EJECT                                                        ELTPPO  
00972                                                                   ELTPPO  
00973                                                                   ELTPPO  
00974 ************************************************************      ELTPPO  
00975 *                                                          *      ELTPPO  
00976 *        GENERATE PROFESSIONAL TEXT                        *      ELTPPO  
00977 *                                                          *      ELTPPO  
00978 ************************************************************      ELTPPO  
00979  GENERATE-PROFESSIONAL-TEXT.                                      ELTPPO  
00980      PERFORM EJECT-NEW-PAGE.                                      ELTPPO  
00981      PERFORM TRANSLATE-AND-DISPLAY-PPO-IND.                       ELTPPO  
00982      IF GSS-PP-APPROVAL-SRC-IND (GSS-INDEX) NOT EQUAL             ELTPPO  
00983          SPACES                                                   ELTPPO  
00984                 AND ZEROES AND LOW-VALUES                         ELTPPO  
00985          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTPPO  
00986      PERFORM TRANSLATE-BS-IND.                                    ELTPPO  
00987      IF GSS-PP-BS-ALT-PRICING-METHOD (GSS-INDEX) NOT EQUAL        ELTPPO  
00988          SPACES AND                                               ELTPPO  
00989                 ZEROES AND LOW-VALUES                             ELTPPO  
00990          PERFORM TRANSLATE-BS-ALT-PRIC.                           ELTPPO  
00991      SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTPPO  
00992      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTPPO  
00993      IF GSS-PP-BS-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES AND    ELTPPO  
00994          ZEROES                                                   ELTPPO  
00995                 AND LOW-VALUES                                    ELTPPO  
00996          PERFORM TRANSLATE-BS-CALC-METHOD.                        ELTPPO  
00997      IF GSS-PP-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTPPO  
00998          SPACES                                                   ELTPPO  
00999                 AND ZEROES AND LOW-VALUES                         ELTPPO  
01000          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTPPO  
01001      IF ((GSS-PP-BS-DEDUCT-APPLIC-IND (GSS-INDEX) NOT EQUAL       ELTPPO  
01002          SPACES AND                                               ELTPPO  
01003                  ZEROES AND LOW-VALUES)) OR                       ELTPPO  
01004                ((GSS-PP-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT        ELTPPO  
01005          EQUAL SPACES AND                                         ELTPPO  
01006                  ZEROES AND LOW-VALUES)) OR                       ELTPPO  
01007                ((GSS-PP-BS-OPEX-OVERRIDE-IND (GSS-INDEX) NOT      ELTPPO  
01008          EQUAL SPACES AND                                         ELTPPO  
01009                  ZEROES AND LOW-VALUES))                          ELTPPO  
01010          PERFORM GENERATE-BS-BENE-REDUCT-TEXT.                    ELTPPO  
01011      IF GSS-PP-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL SPACES AND    ELTPPO  
01012          ZEROES                                                   ELTPPO  
01013                 AND LOW-VALUES                                    ELTPPO  
01014          PERFORM GENERATE-SPILL-OVER-TEXT.                        ELTPPO  
01015      PERFORM GENERATE-SPECIAL-PROVIDERS-SEN.                      ELTPPO  
01016      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTPPO  
01017                                                                   ELTPPO  
01018                                                                   ELTPPO  
01019 ************************************************************      ELTPPO  
01020 *                                                          *      ELTPPO  
01021 *        GENERATE PROFESSIONAL NOT APPLICABLE              *      ELTPPO  
01022 *                                                          *      ELTPPO  
01023 ************************************************************      ELTPPO  
01024  GENERATE-PROFESSIONAL-NOT-APPL.                                  ELTPPO  
01025      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPPO  
01026      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPPO  
01027      MOVE WS-NOT-APPLY-TO-LOB-BS TO COF-DTL-LINE                  ELTPPO  
01028          (COF-NBR-DTL-LINES).                                     ELTPPO  
01029      PERFORM EJECT-NEW-PAGE.                                      ELTPPO  
01030      EJECT                                                        ELTPPO  
01031                                                                   ELTPPO  
01032                                                                   ELTPPO  
01033 ************************************************************      ELTPPO  
01034 *                                                          *      ELTPPO  
01035 *        TRANSLATE BS IND                                  *      ELTPPO  
01036 *                                                          *      ELTPPO  
01037 ************************************************************      ELTPPO  
01038  TRANSLATE-BS-IND.                                                ELTPPO  
01039      INITIALIZE TCAR-FROM-AREA.                                   ELTPPO  
01040      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
01041      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTPPO  
01042      SET PERIOD-NEEDED TO TRUE.                                   ELTPPO  
01043      MOVE GSS-PP-BS-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTPPO  
01044      MOVE   'PP-BS-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTPPO  
01045      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPPO  
01046      EJECT                                                        ELTPPO  
01047                                                                   ELTPPO  
01048                                                                   ELTPPO  
01049 ************************************************************      ELTPPO  
01050 *                                                          *      ELTPPO  
01051 *        TRANSLATE BS CALC METHOD                          *      ELTPPO  
01052 *                                                          *      ELTPPO  
01053 ************************************************************      ELTPPO  
01054  TRANSLATE-BS-CALC-METHOD.                                        ELTPPO  
01055      INITIALIZE TCAR-FROM-AREA.                                   ELTPPO  
01056      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
01057      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPPO  
01058      SET PERIOD-NEEDED TO TRUE.                                   ELTPPO  
01059      MOVE GSS-PP-BS-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTPPO  
01060      MOVE   'PP-BS-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTPPO  
01061      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPPO  
01062      EJECT                                                        ELTPPO  
01063                                                                   ELTPPO  
01064                                                                   ELTPPO  
01065 ************************************************************      ELTPPO  
01066 *                                                          *      ELTPPO  
01067 *        TRANSLATE BS ALT PRIC                             *      ELTPPO  
01068 *                                                          *      ELTPPO  
01069 ************************************************************      ELTPPO  
01070  TRANSLATE-BS-ALT-PRIC.                                           ELTPPO  
01071      INITIALIZE TCAR-FROM-AREA.                                   ELTPPO  
01072      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
01073      MOVE WS-ALT-PRICING-LINE-BS TO TCAR-FROM-LINE                ELTPPO  
01074          (TCAR-FROM-SUB).                                         ELTPPO  
01075      ADD  +1 TO TCAR-FROM-SUB.                                    ELTPPO  
01076      SET  BLANK-LINE-NEEDED TO TRUE.                              ELTPPO  
01077      SET PERIOD-NEEDED TO TRUE.                                   ELTPPO  
01078      MOVE GSS-PP-BS-ALT-PRICING-METHOD (GSS-INDEX) TO             ELTPPO  
01079          CMF-CODE-VALUE.                                          ELTPPO  
01080      MOVE  'PP-BS-ALT-PRICING-METHOD' TO CMF-ELEMENT-SYSTEM-NAME. ELTPPO  
01081      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPPO  
01082      EJECT                                                        ELTPPO  
01083                                                                   ELTPPO  
01084                                                                   ELTPPO  
01085 ************************************************************      ELTPPO  
01086 *                                                          *      ELTPPO  
01087 *        GENERATE BS BENE REDUCT TEXT                      *      ELTPPO  
01088 *                                                          *      ELTPPO  
01089 ************************************************************      ELTPPO  
01090  GENERATE-BS-BENE-REDUCT-TEXT.                                    ELTPPO  
01091      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPPO  
01092      MOVE WS-BENE-REDUCT-LINE TO COF-DTL-LINE                     ELTPPO  
01093          (COF-NBR-DTL-LINES).                                     ELTPPO  
01094      PERFORM LINK-TO-OUTPUT.                                      ELTPPO  
01095      INITIALIZE ADDITIONAL-TEXT-SWITCH.                           ELTPPO  
01096      INITIALIZE WS-PERIOD-SWITCH.                                 ELTPPO  
01097      INITIALIZE TCAR-FROM-AREA.                                   ELTPPO  
01098      IF GSS-PP-BS-DEDUCT-APPLIC-IND (GSS-INDEX) NOT EQUAL         ELTPPO  
01099          SPACES                                                   ELTPPO  
01100                 AND ZEROES AND LOW-VALUES                         ELTPPO  
01101          PERFORM TRANSLATE-BS-DEDUCT-IND.                         ELTPPO  
01102      IF GSS-PP-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTPPO  
01103          SPACES                                                   ELTPPO  
01104                 AND ZEROES AND LOW-VALUES                         ELTPPO  
01105          PERFORM TRANSLATE-BS-OPEX-APPLIC.                        ELTPPO  
01106      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPPO  
01107      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPPO  
01108      PERFORM LINK-TO-OUTPUT.                                      ELTPPO  
01109      IF GSS-PP-BS-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTPPO  
01110          SPACES                                                   ELTPPO  
01111                 AND ZEROES AND LOW-VALUES                         ELTPPO  
01112          PERFORM TRANSLATE-BS-OPEX-OVERRIDE.                      ELTPPO  
01113      EJECT                                                        ELTPPO  
01114                                                                   ELTPPO  
01115                                                                   ELTPPO  
01116 ************************************************************      ELTPPO  
01117 *                                                          *      ELTPPO  
01118 *        TRANSLATE BS DEDUCT IND                           *      ELTPPO  
01119 *                                                          *      ELTPPO  
01120 ************************************************************      ELTPPO  
01121  TRANSLATE-BS-DEDUCT-IND.                                         ELTPPO  
01122      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
01123      MOVE GSS-PP-BS-DEDUCT-APPLIC-IND (GSS-INDEX) TO              ELTPPO  
01124          CMF-CODE-VALUE.                                          ELTPPO  
01125      MOVE 'PP-BS-DEDUCT-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTPPO  
01126      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPPO  
01127      EJECT                                                        ELTPPO  
01128                                                                   ELTPPO  
01129                                                                   ELTPPO  
01130 ************************************************************      ELTPPO  
01131 *                                                          *      ELTPPO  
01132 *        TRANSLATE BS OPEX APPLIC                          *      ELTPPO  
01133 *                                                          *      ELTPPO  
01134 ************************************************************      ELTPPO  
01135  TRANSLATE-BS-OPEX-APPLIC.                                        ELTPPO  
01136      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
01137      MOVE GSS-PP-BS-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTPPO  
01138          CMF-CODE-VALUE.                                          ELTPPO  
01139      MOVE 'PP-BS-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTPPO  
01140      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPPO  
01141      EJECT                                                        ELTPPO  
01142                                                                   ELTPPO  
01143                                                                   ELTPPO  
01144 ************************************************************      ELTPPO  
01145 *                                                          *      ELTPPO  
01146 *        TRANSLATE BS OPEX OVERRIDE                        *      ELTPPO  
01147 *                                                          *      ELTPPO  
01148 ************************************************************      ELTPPO  
01149  TRANSLATE-BS-OPEX-OVERRIDE.                                      ELTPPO  
01150      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
01151      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPPO  
01152      SET PERIOD-NEEDED TO TRUE.                                   ELTPPO  
01153      MOVE GSS-PP-BS-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTPPO  
01154          CMF-CODE-VALUE.                                          ELTPPO  
01155      MOVE 'PP-BS-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTPPO  
01156      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPPO  
01157      EJECT                                                        ELTPPO  
01158                                                                   ELTPPO  
01159                                                                   ELTPPO  
01160 ************************************************************      ELTPPO  
01161 *                                                          *      ELTPPO  
01162 *        CREATE SUPPLEMENTAL SCREEN                        *      ELTPPO  
01163 *                                                          *      ELTPPO  
01164 ************************************************************      ELTPPO  
01165  CREATE-SUPPLEMENTAL-SCREEN.                                      ELTPPO  
01166      MOVE WS-SUPP TO WS-HDR-LINE-BCBSMM.                          ELTPPO  
01167      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTPPO  
01168      IF GSS-PP-MM-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTPPO  
01169          ZEROES                                                   ELTPPO  
01170                 AND LOW-VALUES                                    ELTPPO  
01171          PERFORM GENERATE-SUPPLEMENTAL-TEXT                       ELTPPO  
01172      ELSE                                                         ELTPPO  
01173          PERFORM GENERATE-SUPPLEMENTAL-NOT-APPL.                  ELTPPO  
01174      EJECT                                                        ELTPPO  
01175                                                                   ELTPPO  
01176                                                                   ELTPPO  
01177 ************************************************************      ELTPPO  
01178 *                                                          *      ELTPPO  
01179 *        GENERATE SUPPLEMENTAL TEXT                        *      ELTPPO  
01180 *                                                          *      ELTPPO  
01181 ************************************************************      ELTPPO  
01182  GENERATE-SUPPLEMENTAL-TEXT.                                      ELTPPO  
01183      PERFORM EJECT-NEW-PAGE.                                      ELTPPO  
01184      PERFORM TRANSLATE-AND-DISPLAY-PPO-IND.                       ELTPPO  
01185      IF GSS-PP-APPROVAL-SRC-IND (GSS-INDEX) NOT EQUAL             ELTPPO  
01186          SPACES                                                   ELTPPO  
01187                 AND ZEROES AND LOW-VALUES                         ELTPPO  
01188          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTPPO  
01189      PERFORM TRANSLATE-MM-IND.                                    ELTPPO  
01190      IF GSS-PP-MM-ALT-PRICING-METHOD (GSS-INDEX) NOT EQUAL        ELTPPO  
01191          SPACES AND                                               ELTPPO  
01192                 ZEROES AND LOW-VALUES                             ELTPPO  
01193          PERFORM TRANSLATE-MM-ALT-PRIC.                           ELTPPO  
01194      SET SRP-ACCUM-PROV-CLASS-SUPP TO TRUE.                       ELTPPO  
01195      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTPPO  
01196      IF GSS-PP-MM-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES AND    ELTPPO  
01197          ZEROES                                                   ELTPPO  
01198                 AND LOW-VALUES                                    ELTPPO  
01199          PERFORM TRANSLATE-MM-CALC-METHOD.                        ELTPPO  
01200      IF GSS-PP-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTPPO  
01201          SPACES                                                   ELTPPO  
01202                 AND ZEROES AND LOW-VALUES                         ELTPPO  
01203          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTPPO  
01204      IF ((GSS-PP-MM-DEDUCT-APPLIC-IND (GSS-INDEX) NOT EQUAL       ELTPPO  
01205          SPACES AND                                               ELTPPO  
01206                  ZEROES AND LOW-VALUES)) OR                       ELTPPO  
01207                ((GSS-PP-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT        ELTPPO  
01208          EQUAL SPACES AND                                         ELTPPO  
01209                  ZEROES AND LOW-VALUES)) OR                       ELTPPO  
01210                ((GSS-PP-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT      ELTPPO  
01211          EQUAL SPACES AND                                         ELTPPO  
01212                  ZEROES AND LOW-VALUES))                          ELTPPO  
01213          PERFORM GENERATE-MM-BENE-REDUCT-TEXT.                    ELTPPO  
01214      PERFORM GENERATE-SPECIAL-PROVIDERS-SEN.                      ELTPPO  
01215      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTPPO  
01216                                                                   ELTPPO  
01217                                                                   ELTPPO  
01218 ************************************************************      ELTPPO  
01219 *                                                          *      ELTPPO  
01220 *        GENERATE SUPPLEMENTAL NOT APPLICABLE              *      ELTPPO  
01221 *                                                          *      ELTPPO  
01222 ************************************************************      ELTPPO  
01223  GENERATE-SUPPLEMENTAL-NOT-APPL.                                  ELTPPO  
01224      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPPO  
01225      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPPO  
01226      MOVE WS-NOT-APPLY-TO-LOB-MM TO COF-DTL-LINE                  ELTPPO  
01227          (COF-NBR-DTL-LINES).                                     ELTPPO  
01228      PERFORM EJECT-NEW-PAGE.                                      ELTPPO  
01229      EJECT                                                        ELTPPO  
01230                                                                   ELTPPO  
01231                                                                   ELTPPO  
01232 ************************************************************      ELTPPO  
01233 *                                                          *      ELTPPO  
01234 *        TRANSLATE MM IND                                  *      ELTPPO  
01235 *                                                          *      ELTPPO  
01236 ************************************************************      ELTPPO  
01237  TRANSLATE-MM-IND.                                                ELTPPO  
01238      INITIALIZE TCAR-FROM-AREA.                                   ELTPPO  
01239      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
01240      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPPO  
01241      SET PERIOD-NEEDED TO TRUE.                                   ELTPPO  
01242      MOVE GSS-PP-MM-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTPPO  
01243      MOVE   'PP-MM-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTPPO  
01244      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPPO  
01245      EJECT                                                        ELTPPO  
01246                                                                   ELTPPO  
01247                                                                   ELTPPO  
01248 ************************************************************      ELTPPO  
01249 *                                                          *      ELTPPO  
01250 *        TRANSLATE MM CALC METHOD                          *      ELTPPO  
01251 *                                                          *      ELTPPO  
01252 ************************************************************      ELTPPO  
01253  TRANSLATE-MM-CALC-METHOD.                                        ELTPPO  
01254      INITIALIZE TCAR-FROM-AREA.                                   ELTPPO  
01255      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
01256      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPPO  
01257      SET PERIOD-NEEDED TO TRUE.                                   ELTPPO  
01258      MOVE GSS-PP-MM-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTPPO  
01259      MOVE   'PP-MM-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTPPO  
01260      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPPO  
01261      EJECT                                                        ELTPPO  
01262                                                                   ELTPPO  
01263                                                                   ELTPPO  
01264 ************************************************************      ELTPPO  
01265 *                                                          *      ELTPPO  
01266 *        TRANSLATE MM ALT PRIC                             *      ELTPPO  
01267 *                                                          *      ELTPPO  
01268 ************************************************************      ELTPPO  
01269  TRANSLATE-MM-ALT-PRIC.                                           ELTPPO  
01270      INITIALIZE TCAR-FROM-AREA.                                   ELTPPO  
01271      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
01272      MOVE WS-ALT-PRICING-LINE-MM TO TCAR-FROM-LINE                ELTPPO  
01273          (TCAR-FROM-SUB).                                         ELTPPO  
01274      ADD  +1 TO TCAR-FROM-SUB.                                    ELTPPO  
01275      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPPO  
01276      SET PERIOD-NEEDED TO TRUE.                                   ELTPPO  
01277      MOVE GSS-PP-MM-ALT-PRICING-METHOD (GSS-INDEX) TO             ELTPPO  
01278          CMF-CODE-VALUE.                                          ELTPPO  
01279      MOVE  'PP-MM-ALT-PRICING-METHOD' TO CMF-ELEMENT-SYSTEM-NAME. ELTPPO  
01280      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPPO  
01281      EJECT                                                        ELTPPO  
01282                                                                   ELTPPO  
01283                                                                   ELTPPO  
01284 ************************************************************      ELTPPO  
01285 *                                                          *      ELTPPO  
01286 *        GENERATE MM BENE REDUCT TEXT                      *      ELTPPO  
01287 *                                                          *      ELTPPO  
01288 ************************************************************      ELTPPO  
01289  GENERATE-MM-BENE-REDUCT-TEXT.                                    ELTPPO  
01290      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPPO  
01291      MOVE WS-BENE-REDUCT-LINE TO COF-DTL-LINE                     ELTPPO  
01292          (COF-NBR-DTL-LINES).                                     ELTPPO  
01293      PERFORM LINK-TO-OUTPUT.                                      ELTPPO  
01294      INITIALIZE ADDITIONAL-TEXT-SWITCH.                           ELTPPO  
01295      INITIALIZE WS-PERIOD-SWITCH.                                 ELTPPO  
01296      INITIALIZE TCAR-FROM-AREA.                                   ELTPPO  
01297      IF GSS-PP-MM-DEDUCT-APPLIC-IND (GSS-INDEX) NOT EQUAL         ELTPPO  
01298          SPACES                                                   ELTPPO  
01299                 AND ZEROES AND LOW-VALUES                         ELTPPO  
01300          PERFORM TRANSLATE-MM-DEDUCT-IND.                         ELTPPO  
01301      IF GSS-PP-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTPPO  
01302          SPACES                                                   ELTPPO  
01303                 AND ZEROES AND LOW-VALUES                         ELTPPO  
01304          PERFORM TRANSLATE-MM-OPEX-APPLIC.                        ELTPPO  
01305      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPPO  
01306      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPPO  
01307      PERFORM LINK-TO-OUTPUT.                                      ELTPPO  
01308      IF GSS-PP-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTPPO  
01309          SPACES                                                   ELTPPO  
01310                 AND ZEROES AND LOW-VALUES                         ELTPPO  
01311          PERFORM TRANSLATE-MM-OPEX-OVERRIDE.                      ELTPPO  
01312      EJECT                                                        ELTPPO  
01313                                                                   ELTPPO  
01314                                                                   ELTPPO  
01315 ************************************************************      ELTPPO  
01316 *                                                          *      ELTPPO  
01317 *        TRANSLATE MM DEDUCT IND                           *      ELTPPO  
01318 *                                                          *      ELTPPO  
01319 ************************************************************      ELTPPO  
01320  TRANSLATE-MM-DEDUCT-IND.                                         ELTPPO  
01321      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
01322      MOVE GSS-PP-MM-DEDUCT-APPLIC-IND (GSS-INDEX) TO              ELTPPO  
01323          CMF-CODE-VALUE.                                          ELTPPO  
01324      MOVE 'PP-MM-DEDUCT-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTPPO  
01325      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPPO  
01326      EJECT                                                        ELTPPO  
01327                                                                   ELTPPO  
01328                                                                   ELTPPO  
01329 ************************************************************      ELTPPO  
01330 *                                                          *      ELTPPO  
01331 *        TRANSLATE MM OPEX APPLIC                          *      ELTPPO  
01332 *                                                          *      ELTPPO  
01333 ************************************************************      ELTPPO  
01334  TRANSLATE-MM-OPEX-APPLIC.                                        ELTPPO  
01335      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
01336      MOVE GSS-PP-MM-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTPPO  
01337          CMF-CODE-VALUE.                                          ELTPPO  
01338      MOVE 'PP-MM-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTPPO  
01339      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPPO  
01340      EJECT                                                        ELTPPO  
01341                                                                   ELTPPO  
01342                                                                   ELTPPO  
01343 ************************************************************      ELTPPO  
01344 *                                                          *      ELTPPO  
01345 *        TRANSLATE MM OPEX OVERRIDE                        *      ELTPPO  
01346 *                                                          *      ELTPPO  
01347 ************************************************************      ELTPPO  
01348  TRANSLATE-MM-OPEX-OVERRIDE.                                      ELTPPO  
01349      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
01350      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPPO  
01351      SET PERIOD-NEEDED TO TRUE.                                   ELTPPO  
01352      MOVE GSS-PP-MM-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTPPO  
01353          CMF-CODE-VALUE.                                          ELTPPO  
01354      MOVE 'PP-MM-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTPPO  
01355      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPPO  
01356      EJECT                                                        ELTPPO  
01357                                                                   ELTPPO  
01358                                                                   ELTPPO  
01359 ************************************************************      ELTPPO  
01360 *                                                          *      ELTPPO  
01361 *        GENERATE SPILL OVER TEXT                          *      ELTPPO  
01362 *                                                          *      ELTPPO  
01363 ************************************************************      ELTPPO  
01364  GENERATE-SPILL-OVER-TEXT.                                        ELTPPO  
01365      INITIALIZE TCAR-FROM-AREA.                                   ELTPPO  
01366      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
01367      MOVE WS-SPILL-OVER-LINE TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELTPPO  
01368      ADD  +1 TO TCAR-FROM-SUB.                                    ELTPPO  
01369      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPPO  
01370      SET PERIOD-NEEDED TO TRUE.                                   ELTPPO  
01371      MOVE GSS-PP-SPILL-OVER-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTPPO  
01372      MOVE 'PP-SPILL-OVER-IND' TO CMF-ELEMENT-SYSTEM-NAME.         ELTPPO  
01373      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPPO  
01374      EJECT                                                        ELTPPO  
01375                                                                   ELTPPO  
01376                                                                   ELTPPO  
01377 ************************************************************      ELTPPO  
01378 *                                                          *      ELTPPO  
01379 *        GENERATE SPECIAL PROVIDERS SENTENCE               *      ELTPPO  
01380 *                                                          *      ELTPPO  
01381 ************************************************************      ELTPPO  
01382  GENERATE-SPECIAL-PROVIDERS-SEN.                                  ELTPPO  
01383      SET GCG-INDEX TO +1.                                         ELTPPO  
01384      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTPPO  
01385         AT END                                                    ELTPPO  
01386              MOVE ZEROES TO WS-GPPO-PROV-SLOT-NO                  ELTPPO  
01387         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GPPO                     ELTPPO  
01388              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTPPO  
01389                  TO WS-GPPO-PROV-SLOT-NO                          ELTPPO  
01390         END-SEARCH.                                               ELTPPO  
01391      IF WS-GPPO-PROV-SLOT-NO NOT EQUAL ZEROES                     ELTPPO  
01392          PERFORM DISPLAY-SPECIAL-PROVIDERS-SENT                   ELTPPO  
01393          PERFORM GENERATE-SPECIAL-PROVIDERS-TEX.                  ELTPPO  
01394                                                                   ELTPPO  
01395                                                                   ELTPPO  
01396 ************************************************************      ELTPPO  
01397 *                                                          *      ELTPPO  
01398 *        DISPLAY SPECIAL PROVIDERS SENTENCE                *      ELTPPO  
01399 *                                                          *      ELTPPO  
01400 ************************************************************      ELTPPO  
01401  DISPLAY-SPECIAL-PROVIDERS-SENT.                                  ELTPPO  
01402      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPPO  
01403      MOVE WS-SPEC-PROV-MSG  TO COF-DTL-LINE                       ELTPPO  
01404          (COF-NBR-DTL-LINES).                                     ELTPPO  
01405      PERFORM LINK-TO-OUTPUT.                                      ELTPPO  
01406      EJECT                                                        ELTPPO  
01407                                                                   ELTPPO  
01408                                                                   ELTPPO  
01409 ************************************************************      ELTPPO  
01410 *                                                          *      ELTPPO  
01411 *        SEARCH THE GSS ENTRY                              *      ELTPPO  
01412 *                                                          *      ELTPPO  
01413 ************************************************************      ELTPPO  
01414  SEARCH-THE-GSS-ENTRY.                                            ELTPPO  
01415      SET GSS-INDEX TO 1.                                          ELTPPO  
01416      SEARCH GSS-ENTRY                                             ELTPPO  
01417          AT END                                                   ELTPPO  
01418               SET TABULAR-IS-UNDEFINED TO TRUE                    ELTPPO  
01419          WHEN GSS-PP-PROG-CODE-CHR (GSS-INDEX)                    ELTPPO  
01420               CONTINUE                                            ELTPPO  
01421         END-SEARCH.                                               ELTPPO  
01422      IF TABULAR-IS-UNDEFINED                                      ELTPPO  
01423          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTPPO  
01424                                                                   ELTPPO  
01425                                                                   ELTPPO  
01426 ************************************************************      ELTPPO  
01427 *                                                          *      ELTPPO  
01428 *        CALL CODES MANUAL INTERFACE                       *      ELTPPO  
01429 *                                                          *      ELTPPO  
01430 ************************************************************      ELTPPO  
01431  CALL-CODES-MANUAL-INTERFACE.                                     ELTPPO  
01432      MOVE WS-GCCP TO CMF-RECORD-PREFIX.                           ELTPPO  
01433      EXEC CICS LINK                                               ELTPPO  
01434                PROGRAM ('ELUCMIF')                                ELTPPO  
01435                COMMAREA (DFHCOMMAREA)                             ELTPPO  
01436        END-EXEC.                                                  ELTPPO  
01437      EJECT                                                        ELTPPO  
01438                                                                   ELTPPO  
01439                                                                   ELTPPO  
01440 ************************************************************      ELTPPO  
01441 *                                                          *      ELTPPO  
01442 *        GET GCCP TABULAR                                  *      ELTPPO  
01443 *                                                          *      ELTPPO  
01444 ************************************************************      ELTPPO  
01445  GET-GCCP-TABULAR.                                                ELTPPO  
01446      PERFORM ESTABLISH-ADDRESSABILITY-OF-GC.                      ELTPPO  
01447      SET IOP-RD              TO TRUE.                             ELTPPO  
01448      SET IOP-STG-MODE-MOVE   TO TRUE.                             ELTPPO  
01449      SET IOP-FCQ-NONE        TO TRUE.                             ELTPPO  
01450      SET IOP-KVQ-EQ          TO TRUE.                             ELTPPO  
01451      MOVE KWA-GCTABULR-KEY   TO IOP-FILE-KEY.                     ELTPPO  
01452      PERFORM CALL-INPUT-OUTPUT-MODULE.                            ELTPPO  
01453      IF IOP-RC-OK                                                 ELTPPO  
01454          PERFORM ESTABLISH-ADDRESSY-OF-GCCP-TAB                   ELTPPO  
01455      ELSE IF IOP-RC-NOTFND                                        ELTPPO  
01456          PERFORM SIGNAL-NOT-FOUND-GCTAB-ERROR                     ELTPPO  
01457      ELSE                                                         ELTPPO  
01458          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTPPO  
01459      EJECT                                                        ELTPPO  
01460                                                                   ELTPPO  
01461                                                                   ELTPPO  
01462 ************************************************************      ELTPPO  
01463 *                                                          *      ELTPPO  
01464 *        ESTABLISH ADDRESSABILITY OF GCTABULAR IO PARAMETER*      ELTPPO  
01465 *                                                          *      ELTPPO  
01466 ************************************************************      ELTPPO  
01467  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTPPO  
01468      SET CIA-GCTABULR-DDN TO TRUE.                                ELTPPO  
01469      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPPO  
01470                            ADDRESS OF                             ELTPPO  
01471          IOP-INPUT-OUTPUT-PARAMETERS.                             ELTPPO  
01472      EJECT                                                        ELTPPO  
01473                                                                   ELTPPO  
01474                                                                   ELTPPO  
01475 ************************************************************      ELTPPO  
01476 *                                                          *      ELTPPO  
01477 *        CALL INPUT OUTPUT MODULE                          *      ELTPPO  
01478 *                                                          *      ELTPPO  
01479 ************************************************************      ELTPPO  
01480  CALL-INPUT-OUTPUT-MODULE.                                        ELTPPO  
01481      EXEC CICS LINK                                               ELTPPO  
01482                PROGRAM ('ELUIOPGM')                               ELTPPO  
01483                COMMAREA (DFHCOMMAREA)                             ELTPPO  
01484        END-EXEC.                                                  ELTPPO  
01485      EJECT                                                        ELTPPO  
01486                                                                   ELTPPO  
01487                                                                   ELTPPO  
01488 ************************************************************      ELTPPO  
01489 *                                                          *      ELTPPO  
01490 *        ESTABLISH ADDRESSY OF GCCP TABULAR                *      ELTPPO  
01491 *                                                          *      ELTPPO  
01492 ************************************************************      ELTPPO  
01493  ESTABLISH-ADDRESSY-OF-GCCP-TAB.                                  ELTPPO  
01494      SET ADDRESS OF GCCP-TABULAR-REC-AREA TO                      ELTPPO  
01495          IOP-REC-PTR.                                             ELTPPO  
01496      SET IOP-REC-PTR TO NULL.                                     ELTPPO  
01497      EJECT                                                        ELTPPO  
01498                                                                   ELTPPO  
01499                                                                   ELTPPO  
01500 ************************************************************      ELTPPO  
01501 *                                                          *      ELTPPO  
01502 *        SIGNAL NOT FOUND GCTAB ERROR                      *      ELTPPO  
01503 *                                                          *      ELTPPO  
01504 ************************************************************      ELTPPO  
01505  SIGNAL-NOT-FOUND-GCTAB-ERROR.                                    ELTPPO  
01506      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTPPO  
01507      PERFORM SIGNAL-ABEND.                                        ELTPPO  
01508      EJECT                                                        ELTPPO  
01509                                                                   ELTPPO  
01510                                                                   ELTPPO  
01511 ************************************************************      ELTPPO  
01512 *                                                          *      ELTPPO  
01513 *        SIGNAL CRITICAL IO ERROR                          *      ELTPPO  
01514 *                                                          *      ELTPPO  
01515 ************************************************************      ELTPPO  
01516  SIGNAL-CRITICAL-IO-ERROR.                                        ELTPPO  
01517      SET CIA-AB-CRITIO TO TRUE.                                   ELTPPO  
01518      PERFORM SIGNAL-ABEND.                                        ELTPPO  
01519      EJECT                                                        ELTPPO  
01520                                                                   ELTPPO  
01521                                                                   ELTPPO  
01522 ************************************************************      ELTPPO  
01523 *                                                          *      ELTPPO  
01524 *        GENERATE SPECIAL PROVIDERS TEXT                   *      ELTPPO  
01525 *                                                          *      ELTPPO  
01526 ************************************************************      ELTPPO  
01527  GENERATE-SPECIAL-PROVIDERS-TEX.                                  ELTPPO  
01528      MOVE 'PREFERRED PROVIDER OPTION' TO SRP-CCP-NAME.            ELTPPO  
01529      MOVE  WS-GPPO                        TO SRP-TABULAR-ID.      ELTPPO  
01530      MOVE  WS-GPPO-PROV-SLOT-NO           TO SRP-TABULAR-SLOT-NO. ELTPPO  
01531      PERFORM CALL-SPECIAL-PROVIDERS-GENERAT.                      ELTPPO  
01532                                                                   ELTPPO  
01533                                                                   ELTPPO  
01534 ************************************************************      ELTPPO  
01535 *                                                          *      ELTPPO  
01536 *        CALL SPECIAL PROVIDERS GENERATOR                  *      ELTPPO  
01537 *                                                          *      ELTPPO  
01538 ************************************************************      ELTPPO  
01539  CALL-SPECIAL-PROVIDERS-GENERAT.                                  ELTPPO  
01540      EXEC CICS LINK                                               ELTPPO  
01541                PROGRAM ('ELGGXXC')                                ELTPPO  
01542                COMMAREA (DFHCOMMAREA)                             ELTPPO  
01543         END-EXEC.                                                 ELTPPO  
01544                                                                   ELTPPO  
01545                                                                   ELTPPO  
01546 ************************************************************      ELTPPO  
01547 *                                                          *      ELTPPO  
01548 *        GENERATE COINSURANCE                              *      ELTPPO  
01549 *                                                          *      ELTPPO  
01550 ************************************************************      ELTPPO  
01551  GENERATE-COINSURANCE.                                            ELTPPO  
01552      EXEC CICS LINK                                               ELTPPO  
01553                PROGRAM ('ELGACLCC')                               ELTPPO  
01554                COMMAREA (DFHCOMMAREA)                             ELTPPO  
01555         END-EXEC.                                                 ELTPPO  
01556                                                                   ELTPPO  
01557                                                                   ELTPPO  
01558 ************************************************************      ELTPPO  
01559 *                                                          *      ELTPPO  
01560 *        GENERATE COPAY                                    *      ELTPPO  
01561 *                                                          *      ELTPPO  
01562 ************************************************************      ELTPPO  
01563  GENERATE-COPAY.                                                  ELTPPO  
01564      EXEC CICS LINK                                               ELTPPO  
01565                PROGRAM ('ELGACPCC')                               ELTPPO  
01566                COMMAREA (DFHCOMMAREA)                             ELTPPO  
01567         END-EXEC.                                                 ELTPPO  
01568                                                                   ELTPPO  
01569 ************************************************************      ELTPPO  
01570 *                                                          *      ELTPPO  
01571 *        GENERATE DEDUCTIBLE                               *      ELTPPO  
01572 *                                                          *      ELTPPO  
01573 ************************************************************      ELTPPO  
01574  GENERATE-DEDUCTIBLE.                                             ELTPPO  
01575      EXEC CICS LINK                                               ELTPPO  
01576                PROGRAM ('ELGADLCC')                               ELTPPO  
01577                COMMAREA (DFHCOMMAREA)                             ELTPPO  
01578         END-EXEC.                                                 ELTPPO  
01579                                                                   ELTPPO  
01580                                                                   ELTPPO  
01581 ************************************************************      ELTPPO  
01582 *                                                          *      ELTPPO  
01583 *        GENERATE MAXIMUM                                  *      ELTPPO  
01584 *                                                          *      ELTPPO  
01585 ************************************************************      ELTPPO  
01586  GENERATE-MAXIMUM.                                                ELTPPO  
01587      EXEC CICS LINK                                               ELTPPO  
01588                PROGRAM ('ELGABMCC')                               ELTPPO  
01589                COMMAREA (DFHCOMMAREA)                             ELTPPO  
01590         END-EXEC.                                                 ELTPPO  
01591      EJECT                                                        ELTPPO  
01592                                                                   ELTPPO  
01593                                                                   ELTPPO  
01594 ************************************************************      ELTPPO  
01595 *                                                          *      ELTPPO  
01596 *        GENERATE OUT OF POCKET                            *      ELTPPO  
01597 *                                                          *      ELTPPO  
01598 ************************************************************      ELTPPO  
01599  GENERATE-OUT-OF-POCKET.                                          ELTPPO  
01600      EXEC CICS LINK                                               ELTPPO  
01601                PROGRAM ('ELGAOLCC')                               ELTPPO  
01602                COMMAREA (DFHCOMMAREA)                             ELTPPO  
01603         END-EXEC.                                                 ELTPPO  
01604                                                                   ELTPPO  
01605                                                                   ELTPPO  
01606 ************************************************************      ELTPPO  
01607 *                                                          *      ELTPPO  
01608 *        CALL CBRI INTERFACE                               *      ELTPPO  
01609 *                                                          *      ELTPPO  
01610 ************************************************************      ELTPPO  
01611  CALL-CBRI-INTERFACE.                                             ELTPPO  
01612      CALL 'ELGCBRI' USING DFHEIBLK                                ELTPPO  
01613                           DFHCOMMAREA.                            ELTPPO  
01614                                                                   ELTPPO  
01615                                                                   ELTPPO  
01616 ************************************************************      ELTPPO  
01617 *                                                          *      ELTPPO  
01618 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTPPO  
01619 *                                                          *      ELTPPO  
01620 ************************************************************      ELTPPO  
01621  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTPPO  
01622      PERFORM INITIALIZE-CMOUT.                                    ELTPPO  
01623      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTPPO  
01624      EJECT                                                        ELTPPO  
01625                                                                   ELTPPO  
01626                                                                   ELTPPO  
01627 ************************************************************      ELTPPO  
01628 *                                                          *      ELTPPO  
01629 *        PREPARE TEXT FOR OUTPUT                           *      ELTPPO  
01630 *                                                          *      ELTPPO  
01631 ************************************************************      ELTPPO  
01632  PREPARE-TEXT-FOR-OUTPUT.                                         ELTPPO  
01633      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTPPO  
01634          UNTIL CMF-DESCR-IDX                                      ELTPPO  
01635                                    GREATER THAN                   ELTPPO  
01636              CMF-NBR-DESCR-LINES.                                 ELTPPO  
01637      EJECT                                                        ELTPPO  
01638                                                                   ELTPPO  
01639                                                                   ELTPPO  
01640 ************************************************************      ELTPPO  
01641 *                                                          *      ELTPPO  
01642 *        INITIALIZE CMOUT                                  *      ELTPPO  
01643 *                                                          *      ELTPPO  
01644 ************************************************************      ELTPPO  
01645  INITIALIZE-CMOUT.                                                ELTPPO  
01646      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTPPO  
01647      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPPO  
01648          ADDRESS OF CMF-DESCR.                                    ELTPPO  
01649      SET CMF-DESCR-IDX TO 1.                                      ELTPPO  
01650      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTPPO  
01651                                                                   ELTPPO  
01652                                                                   ELTPPO  
01653 ************************************************************      ELTPPO  
01654 *                                                          *      ELTPPO  
01655 *        MOVE CMF TEXT TO OUTPUT                           *      ELTPPO  
01656 *                                                          *      ELTPPO  
01657 ************************************************************      ELTPPO  
01658  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTPPO  
01659      PERFORM MOVE-A-LINE.                                         ELTPPO  
01660      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTPPO  
01661          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTPPO  
01662      IF TCAR-FROM-SUB GREATER THAN 20                             ELTPPO  
01663               OR CMF-DESCR-IDX GREATER THAN                       ELTPPO  
01664          CMF-NBR-DESCR-LINES                                      ELTPPO  
01665          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTPPO  
01666                                                                   ELTPPO  
01667                                                                   ELTPPO  
01668 ************************************************************      ELTPPO  
01669 *                                                          *      ELTPPO  
01670 *        FINISH CODES MANUAL TEXT                          *      ELTPPO  
01671 *                                                          *      ELTPPO  
01672 ************************************************************      ELTPPO  
01673  FINISH-CODES-MANUAL-TEXT.                                        ELTPPO  
01674      SET DONE-PROCESSING TO TRUE.                                 ELTPPO  
01675      IF PERIOD-NEEDED                                             ELTPPO  
01676          PERFORM GET-AND-MOVE-PERIOD.                             ELTPPO  
01677      EJECT                                                        ELTPPO  
01678                                                                   ELTPPO  
01679                                                                   ELTPPO  
01680 ************************************************************      ELTPPO  
01681 *                                                          *      ELTPPO  
01682 *        GET AND MOVE PERIOD                               *      ELTPPO  
01683 *                                                          *      ELTPPO  
01684 ************************************************************      ELTPPO  
01685  GET-AND-MOVE-PERIOD.                                             ELTPPO  
01686      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTPPO  
01687          (TCAR-FROM-SUB).                                         ELTPPO  
01688                                                                   ELTPPO  
01689                                                                   ELTPPO  
01690 ************************************************************      ELTPPO  
01691 *                                                          *      ELTPPO  
01692 *        SAVE LAST LINE                                    *      ELTPPO  
01693 *                                                          *      ELTPPO  
01694 ************************************************************      ELTPPO  
01695  SAVE-LAST-LINE.                                                  ELTPPO  
01696      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
01697      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTPPO  
01698         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTPPO  
01699      ADD 1 TO TCAR-FROM-SUB.                                      ELTPPO  
01700      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTPPO  
01701                                                                   ELTPPO  
01702                                                                   ELTPPO  
01703 ************************************************************      ELTPPO  
01704 *                                                          *      ELTPPO  
01705 *        OUTPUT LAST LINE                                  *      ELTPPO  
01706 *                                                          *      ELTPPO  
01707 ************************************************************      ELTPPO  
01708  OUTPUT-LAST-LINE.                                                ELTPPO  
01709      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTPPO  
01710          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTPPO  
01711      IF BLANK-LINE-NEEDED                                         ELTPPO  
01712          PERFORM CREATE-A-BLANK-LINE.                             ELTPPO  
01713                                                                   ELTPPO  
01714                                                                   ELTPPO  
01715 ************************************************************      ELTPPO  
01716 *                                                          *      ELTPPO  
01717 *        CREATE A BLANK LINE                               *      ELTPPO  
01718 *                                                          *      ELTPPO  
01719 ************************************************************      ELTPPO  
01720  CREATE-A-BLANK-LINE.                                             ELTPPO  
01721      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTPPO  
01722      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPPO  
01723                                                                   ELTPPO  
01724                                                                   ELTPPO  
01725 ************************************************************      ELTPPO  
01726 *                                                          *      ELTPPO  
01727 *        MOVE A LINE                                       *      ELTPPO  
01728 *                                                          *      ELTPPO  
01729 ************************************************************      ELTPPO  
01730  MOVE-A-LINE.                                                     ELTPPO  
01731      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTPPO  
01732          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTPPO  
01733      SET CMF-DESCR-IDX UP BY 1.                                   ELTPPO  
01734      ADD 1 TO TCAR-FROM-SUB.                                      ELTPPO  
01735      EJECT                                                        ELTPPO  
01736                                                                   ELTPPO  
01737                                                                   ELTPPO  
01738 ************************************************************      ELTPPO  
01739 *                                                          *      ELTPPO  
01740 *        REFORMAT AND WRITE TEXT                           *      ELTPPO  
01741 *                                                          *      ELTPPO  
01742 ************************************************************      ELTPPO  
01743  REFORMAT-AND-WRITE-TEXT.                                         ELTPPO  
01744      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTPPO  
01745      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTPPO  
01746      PERFORM UNSTRING-TEXT.                                       ELTPPO  
01747      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPPO  
01748      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPPO  
01749      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTPPO  
01750          UNTIL COF-NBR-DTL-LINES GREATER                          ELTPPO  
01751                                   TCAR-OUTPUT-FIELDS-USED -       ELTPPO  
01752              1.                                                   ELTPPO  
01753      PERFORM DISPOSE-OF-LAST-LINE.                                ELTPPO  
01754      PERFORM LINK-TO-OUTPUT.                                      ELTPPO  
01755                                                                   ELTPPO  
01756                                                                   ELTPPO  
01757 ************************************************************      ELTPPO  
01758 *                                                          *      ELTPPO  
01759 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTPPO  
01760 *                                                          *      ELTPPO  
01761 ************************************************************      ELTPPO  
01762  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTPPO  
01763      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTPPO  
01764           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTPPO  
01765      ADD +1 TO TCAR-FROM-SUB.                                     ELTPPO  
01766      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPPO  
01767      EJECT                                                        ELTPPO  
01768                                                                   ELTPPO  
01769                                                                   ELTPPO  
01770 ************************************************************      ELTPPO  
01771 *                                                          *      ELTPPO  
01772 *        UNSTRING TEXT                                     *      ELTPPO  
01773 *                                                          *      ELTPPO  
01774 ************************************************************      ELTPPO  
01775  UNSTRING-TEXT.                                                   ELTPPO  
01776      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTPPO  
01777      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTPPO  
01778      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTPPO  
01779      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTPPO  
01780      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTPPO  
01781      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTPPO  
01782      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTPPO  
01783      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTPPO  
01784      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTPPO  
01785      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTPPO  
01786      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTPPO  
01787      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTPPO  
01788      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTPPO  
01789      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTPPO  
01790      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTPPO  
01791      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTPPO  
01792      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTPPO  
01793      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTPPO  
01794      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTPPO  
01795      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTPPO  
01796      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTPPO  
01797      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTPPO  
01798      EJECT                                                        ELTPPO  
01799                                                                   ELTPPO  
01800                                                                   ELTPPO  
01801 ************************************************************      ELTPPO  
01802 *                                                          *      ELTPPO  
01803 *        LINK TO OUTPUT                                    *      ELTPPO  
01804 *                                                          *      ELTPPO  
01805 ************************************************************      ELTPPO  
01806  LINK-TO-OUTPUT.                                                  ELTPPO  
01807      EXEC CICS LINK                                               ELTPPO  
01808          PROGRAM ('ELUOUTPT')                                     ELTPPO  
01809          COMMAREA (DFHCOMMAREA)                                   ELTPPO  
01810          END-EXEC.                                                ELTPPO  
01811      EJECT                                                        ELTPPO  
01812                                                                   ELTPPO  
01813                                                                   ELTPPO  
01814 ************************************************************      ELTPPO  
01815 *                                                          *      ELTPPO  
01816 *        DISPOSE OF LAST LINE                              *      ELTPPO  
01817 *                                                          *      ELTPPO  
01818 ************************************************************      ELTPPO  
01819  DISPOSE-OF-LAST-LINE.                                            ELTPPO  
01820      IF NOT ADDITIONAL-TEXT                                       ELTPPO  
01821          PERFORM INITIALIZE-CONTINUED-SW.                         ELTPPO  
01822      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTPPO  
01823          PERFORM SAVE-LAST-LINE                                   ELTPPO  
01824      ELSE                                                         ELTPPO  
01825          PERFORM OUTPUT-LAST-LINE.                                ELTPPO  
01826                                                                   ELTPPO  
01827                                                                   ELTPPO  
01828 ************************************************************      ELTPPO  
01829 *                                                          *      ELTPPO  
01830 *        INITIALIZE CONTINUED SW                           *      ELTPPO  
01831 *                                                          *      ELTPPO  
01832 ************************************************************      ELTPPO  
01833  INITIALIZE-CONTINUED-SW.                                         ELTPPO  
01834      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTPPO  
