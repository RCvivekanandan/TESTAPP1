00001 *      LAST MAINTENANCE TIME: 10.03.36  DATE: 06/28/91            06/29/02
00002  IDENTIFICATION DIVISION.                                         ELTWEEKN
00003                                                                      LV001
00004  PROGRAM-ID.         ELTWEEKN.                                    ELTWEEKN
00005                                                                   ELTWEEKN
00006  AUTHOR.             RICK BARILEAU                                ELTWEEKN
00007                                                                   ELTWEEKN
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTWEEKN
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELTWEEKN
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTWEEKN
00011                      233 N. MICHIGAN AVE                          ELTWEEKN
00012                      CHICAGO, ILLINOIS 60601                      ELTWEEKN
00013                                                                   ELTWEEKN
00014  DATE-WRITTEN.       18-JUN-1987.                                 ELTWEEKN
00015                                                                   ELTWEEKN
00016  DATE-COMPILED.                                                   ELTWEEKN
00017                                                                   ELTWEEKN
00018  SECURITY.           COPYRIGHT 1986,                              ELTWEEKN
00019                      HEALTH CARE SERVICE CORPORATION              ELTWEEKN
00020      SKIP3                                                        ELTWEEKN
00021  ENVIRONMENT DIVISION.                                            ELTWEEKN
00022                                                                   ELTWEEKN
00023  CONFIGURATION SECTION.                                           ELTWEEKN
00024  SOURCE-COMPUTER.    IBM-3090.                                    ELTWEEKN
00025  OBJECT-COMPUTER.    IBM-3090.                                    ELTWEEKN
00026      EJECT                                                        ELTWEEKN
00027 ******************************************************************ELTWEEKN
00028 *                                                                *ELTWEEKN
00029 *    COPYBOOK:   ELTWEEKN                                        *ELTWEEKN
00030 *    DATE:       18-JUN-1987                                     *ELTWEEKN
00031 *    AUTHOR:     RICK BARILEAU                                   *ELTWEEKN
00032 *    FUNCTION:   THIS WILL GENERATE ALL OUTPUT ASSOCIATED WITH   *ELTWEEKN
00033 *                THE WEEKEND ADMISSION PROGRAM.                  *ELTWEEKN
00034 *    NOTES:      X---                                            *ELTWEEKN
00035 *                                                                *ELTWEEKN
00036 ******************************************************************ELTWEEKN
00037 *                                                                *ELTWEEKN
00038 *                      MAINTENANCE HISTORY                       *ELTWEEKN
00039 *                                                                *ELTWEEKN
00040 *  MOD     DATE     BY  DRPT                ACTION               *ELTWEEKN
00041 * ----- ----------- --- ----- ---------------------------------- *ELTWEEKN
00042 * 01.00 18-JUN-1987 REB       CREATED                            *ELTWEEKN
00043 *                                                                *ELTWEEKN
00044 * 01.01 02-SEP-1987 REB       REARRANGE ORDER OF G-TABS INFO TO  *ELTWEEKN
00045 *                             FOLLOW FIXED SENTENCE. ALSO, MOVE  *ELTWEEKN
00046 *                             SPILL OVER SENTENCE BEFORE THE     *ELTWEEKN
00047 *                             G-TAB INFORMATION.                 *ELTWEEKN
00048 *                                                                *ELTWEEKN
00049 * 01.02 29-OCT-1990 JPB       CHANGED STORAGE MANAGEMENT.        *ELTWEEKN
00050 *                                                                *ELTWEEKN
00051 * 01.03 12-NOV-1990 JPB       CHANGED REFERENCES TO GCG-FRI-SAT- *ELTWEEKN
00052 *                             ADM-IND TO REFLECT NEW FIELD SIZE. *ELTWEEKN
00053 *                                                                *ELTWEEKN
00054 * 01.04 12-JUN-1991 JPB       ADDED TRANSLATION AND DISPLAY OF   *ELTWEEKN
00055 *                             PARTICIPATION INDICATOR            *ELTWEEKN
00056 *                             (GCG-FRI-SAT-ADM-IND).             *ELTWEEKN
00057 *                                                                *ELTWEEKN
00058 * 01.05 28-JUN-1991 JPB       SET APPROVAL SOURCE SWITCH TO 'N'  *ELTWEEKN
00059 *                             TO FIX ASRA WHEN PROFESSIONAL ONLY *ELTWEEKN
00060 *                             IS SELECTED.                       *ELTWEEKN
00061 *                                                                *ELTWEEKN
00062 ******************************************************************ELTWEEKN
00063                                                                   ELTWEEKN
00064  DATA DIVISION.                                                   ELTWEEKN
00065                                                                   ELTWEEKN
00066  WORKING-STORAGE SECTION.                                         ELTWEEKN
00067  01  WS-MISC.                                                     ELTWEEKN
00068      05  WS-BEGIN                        PIC X(26) VALUE          ELTWEEKN
00069      '*** ELTWEEKN WS BEGINS ***'.                                ELTWEEKN
00070      05  WS-POINTER2                     POINTER.                 ELTWEEKN
00071      05  WS-POINTER3                     POINTER.                 ELTWEEKN
00072                                                                   ELTWEEKN
00073  01  WS-SWITCHES.                                                 ELTWEEKN
00074      05  APPROVAL-SOURCE-SW       PIC X     VALUE 'N'.            ELTWEEKN
00075          88  NOT-HOLDING-APPROVAL-SRCE      VALUE 'N'.            ELTWEEKN
00076          88  HOLDING-APPROVAL-SOURCE        VALUE 'H'.            ELTWEEKN
00077                                                                   ELTWEEKN
00078      05  ADDITIONAL-TEXT-SWITCH   PIC X(01) VALUE SPACE.          ELTWEEKN
00079          88  ADDITIONAL-TEXT                VALUE 'A'.            ELTWEEKN
00080          88  BLANK-LINE-NEEDED              VALUE 'B'.            ELTWEEKN
00081      05  CONTINUED-PROCESSING-SW  PIC X(01) VALUE SPACE.          ELTWEEKN
00082          88  DONE-PROCESSING                VALUE 'D'.            ELTWEEKN
00083          88  PROCESSING-CMF-TEXT            VALUE 'P'.            ELTWEEKN
00084      05  WS-GFSB-SWITCH           PIC X(01) VALUE 'N'.            ELTWEEKN
00085          88  GFSB-IS-PRESENT                VALUE 'Y'.            ELTWEEKN
00086      05  WS-PERIOD-SWITCH         PIC X(01) VALUE 'N'.            ELTWEEKN
00087          88  PERIOD-NEEDED                  VALUE 'Y'.            ELTWEEKN
00088      05  WS-TABULAR-SWITCH        PIC X(01) VALUE 'N'.            ELTWEEKN
00089          88  TABULAR-IS-UNDEFINED           VALUE 'Y'.            ELTWEEKN
00090                                                                   ELTWEEKN
00091  01  WS-HOLD-AREA.                                                ELTWEEKN
00092      05  WS-GFSB-PROV-ID               PIC X(06)  VALUE SPACES.   ELTWEEKN
00093      05  WS-GFSB-PROV-SLOT-NO   COMP-3 PIC S9(07) VALUE +0.       ELTWEEKN
00094                                                                   ELTWEEKN
00095 **************************************************************    ELTWEEKN
00096 ***                    PROGRAM CONSTANTS                          ELTWEEKN
00097 **************************************************************    ELTWEEKN
00098      05  WS-GCCP                  PIC X(06) VALUE '#GCCP '.       ELTWEEKN
00099      05  WS-GFSB                  PIC X(06) VALUE '#GFSB '.       ELTWEEKN
00100      05  WS-GROUP                 PIC X(06) VALUE 'GROUP '.       ELTWEEKN
00101      05  WS-INST                  PIC X(13) VALUE 'INSTITUTIONAL'.ELTWEEKN
00102      05  WS-PROF                  PIC X(13) VALUE 'PROFESSIONAL '.ELTWEEKN
00103      05  WS-SUPP                  PIC X(13) VALUE 'SUPPLEMENTAL '.ELTWEEKN
00104                                                                   ELTWEEKN
00105 **************************************************************    ELTWEEKN
00106 ***                    HEADER  LINE                               ELTWEEKN
00107 **************************************************************    ELTWEEKN
00108      05  WS-HEADER-LINE.                                          ELTWEEKN
00109          10  FILLER               PIC X(20) VALUE SPACES.         ELTWEEKN
00110          10  FILLER               PIC X(26) VALUE                 ELTWEEKN
00111          'WEEKEND ADMISSION PROGRAM '.                            ELTWEEKN
00112          10  WS-HDR-LINE-BCBSMM   PIC X(13) VALUE SPACES.         ELTWEEKN
00113          10  FILLER               PIC X(20) VALUE SPACES.         ELTWEEKN
00114                                                                   ELTWEEKN
00115 **************************************************************    ELTWEEKN
00116 ***                    SCREEN BODY LINES                          ELTWEEKN
00117 **************************************************************    ELTWEEKN
00118  01  WS-SCREEN-LINE-AREA.                                         ELTWEEKN
00119      05  WS-APPRVL-LINE.                                          ELTWEEKN
00120          10  FILLER               PIC X(79) VALUE                 ELTWEEKN
00121          'THE SOURCE OF APPROVAL FOR WEEKEND ADMISSIONS IS '.     ELTWEEKN
00122                                                                   ELTWEEKN
00123      05  WS-ALT-PRICING-LINE-BC.                                  ELTWEEKN
00124          10  FILLER               PIC X(53) VALUE                 ELTWEEKN
00125          'THE ALTERNATE PRICING FOR INSTITUTIONAL SERVICES IS '.  ELTWEEKN
00126          10  FILLER               PIC X(26) VALUE SPACES.         ELTWEEKN
00127                                                                   ELTWEEKN
00128      05  WS-ALT-PRICING-LINE-BS.                                  ELTWEEKN
00129          10  FILLER               PIC X(52) VALUE                 ELTWEEKN
00130          'THE ALTERNATE PRICING FOR PROFESSIONAL SERVICES IS '.   ELTWEEKN
00131          10  FILLER               PIC X(27) VALUE SPACES.         ELTWEEKN
00132                                                                   ELTWEEKN
00133      05  WS-ALT-PRICING-LINE-MM.                                  ELTWEEKN
00134          10  FILLER               PIC X(52) VALUE                 ELTWEEKN
00135          'THE ALTERNATE PRICING FOR SUPPLEMENTAL SERVICES IS '.   ELTWEEKN
00136          10  FILLER               PIC X(27) VALUE SPACES.         ELTWEEKN
00137                                                                   ELTWEEKN
00138      05  WS-BENE-REDUCT-LINE.                                     ELTWEEKN
00139          10  FILLER               PIC X(47) VALUE                 ELTWEEKN
00140          'DENIED OR REDUCED BENEFITS DUE TO THIS PROGRAM:'.       ELTWEEKN
00141          10  FILLER               PIC X(32) VALUE SPACES.         ELTWEEKN
00142                                                                   ELTWEEKN
00143      05  WS-SPILL-OVER-LINE.                                      ELTWEEKN
00144          10  FILLER               PIC X(51) VALUE                 ELTWEEKN
00145          'UNPAID SERVICES AFTER BASIC BENEFITS REDUCTION ARE '.   ELTWEEKN
00146          10  FILLER               PIC X(28) VALUE SPACES.         ELTWEEKN
00147                                                                   ELTWEEKN
00148 **************************************************************    ELTWEEKN
00149 ** SPECIAL MESSAGE FOR THE VOLUNTARY AND NOT APPLICABLE CASES     ELTWEEKN
00150 ** ALSO THE FIXED TEXT FOR TABULARS GFSB                          ELTWEEKN
00151 **************************************************************    ELTWEEKN
00152      05  WS-NOT-APPLICABLE-MSG.                                   ELTWEEKN
00153          10  FILLER               PIC  X(79) VALUE                ELTWEEKN
00154          'THE WEEKEND ADMISSION PROGRAM IS NOT APPLICABLE.'.      ELTWEEKN
00155                                                                   ELTWEEKN
00156      05  WS-PARTICIPATION-LINE.                                   ELTWEEKN
00157          10  FILLER               PIC  X(79) VALUE                ELTWEEKN
00158          'THE WEEKEND ADMISSION PROGRAM APPLIES TO '.             ELTWEEKN
00159                                                                   ELTWEEKN
00160      05  WS-VOLUNTARY-MSG.                                        ELTWEEKN
00161          10  FILLER               PIC  X(79) VALUE                ELTWEEKN
00162          'THE WEEKEND ADMISSION PROGRAM IS VOLUNTARY.'.           ELTWEEKN
00163                                                                   ELTWEEKN
00164      05  WS-DISCLAIMER-MSG.                                       ELTWEEKN
00165          10  FILLER               PIC X(79) VALUE                 ELTWEEKN
00166          '*** SUBJECT TO OTHER CONTRACT LIMITATIONS ***'.         ELTWEEKN
00167                                                                   ELTWEEKN
00168      05  WS-SPEC-SERV-MSG.                                        ELTWEEKN
00169          10  FILLER               PIC X(79) VALUE                 ELTWEEKN
00170          'THERE ARE SPECIAL RELATED SERVICES INCLUDED IN THIS COSTELTWEEKN
00171 -        ' CONTAINMENT PROGRAM.'.                                 ELTWEEKN
00172                                                                   ELTWEEKN
00173      05  WS-NOT-APPLY-TO-LOB-BC.                                  ELTWEEKN
00174          10  FILLER               PIC X(49) VALUE                 ELTWEEKN
00175        'THE WEEKEND ADMISSION PROGRAM DOES NOT APPLY FOR '.       ELTWEEKN
00176          10  FILLER               PIC X(23) VALUE                 ELTWEEKN
00177          'INSTITUTIONAL BENEFITS.'.                               ELTWEEKN
00178          10  FILLER               PIC X(07) VALUE SPACES.         ELTWEEKN
00179                                                                   ELTWEEKN
00180      05  WS-NOT-APPLY-TO-LOB-BS.                                  ELTWEEKN
00181          10  FILLER               PIC X(49) VALUE                 ELTWEEKN
00182        'THE WEEKEND ADMISSION PROGRAM DOES NOT APPLY FOR '.       ELTWEEKN
00183          10  FILLER               PIC X(22) VALUE                 ELTWEEKN
00184          'PROFESSIONAL BENEFITS.'.                                ELTWEEKN
00185          10  FILLER               PIC X(08) VALUE SPACES.         ELTWEEKN
00186                                                                   ELTWEEKN
00187      05  WS-NOT-APPLY-TO-LOB-MM.                                  ELTWEEKN
00188          10  FILLER               PIC X(49) VALUE                 ELTWEEKN
00189        'THE WEEKEND ADMISSION PROGRAM DOES NOT APPLY FOR '.       ELTWEEKN
00190          10  FILLER               PIC X(22) VALUE                 ELTWEEKN
00191          'SUPPLEMENTAL BENEFITS.'.                                ELTWEEKN
00192          10  FILLER               PIC X(08) VALUE SPACES.         ELTWEEKN
00193                                                                   ELTWEEKN
00194  LINKAGE SECTION.                                                 ELTWEEKN
00195  01  DFHCOMMAREA.                                                 ELTWEEKN
00196      COPY ELSCOMMC.                                               ELTWEEKN
00197 /                                                                 ELTWEEKN
00198      COPY ELSCIA2C.                                               ELTWEEKN
00199 /                                                                 ELTWEEKN
00200      COPY ELSCMDSC.                                               ELTWEEKN
00201 /                                                                 ELTWEEKN
00202      COPY ELSCMIFC.                                               ELTWEEKN
00203 /                                                                 ELTWEEKN
00204      COPY ELSIOPMC.                                               ELTWEEKN
00205 /                                                                 ELTWEEKN
00206      COPY ELSKEYSC.                                               ELTWEEKN
00207 /                                                                 ELTWEEKN
00208      COPY ELSOUTPC.                                               ELTWEEKN
00209 /                                                                 ELTWEEKN
00210      COPY ELSSRTPC.                                               ELTWEEKN
00211 /                                                                 ELTWEEKN
00212      COPY ELSTCWAC.                                               ELTWEEKN
00213 /                                                                 ELTWEEKN
00214      COPY ELSSSCBC.                                               ELTWEEKN
00215 /                                                                 ELTWEEKN
00216  01  GROUP-SPECIFIC-REC-AREA.                                     ELTWEEKN
00217      COPY GCGROUPC.                                               ELTWEEKN
00218 /                                                                 ELTWEEKN
00219  01  GCCP-TABULAR-REC-AREA.                                       ELTWEEKN
00220      COPY GCTGCCPC.                                               ELTWEEKN
00221      EJECT                                                        ELTWEEKN
00222  PROCEDURE DIVISION.                                              ELTWEEKN
00223 ************************************************************      ELTWEEKN
00224 *                                                          *      ELTWEEKN
00225 *                    PROCEDURE DIVISION                    *      ELTWEEKN
00226 *                                                          *      ELTWEEKN
00227 ************************************************************      ELTWEEKN
00228                                                                   ELTWEEKN
00229                                                                   ELTWEEKN
00230 ************************************************************      ELTWEEKN
00231 *                                                          *      ELTWEEKN
00232 *        WEEKEND ADMISSION                                 *      ELTWEEKN
00233 *                                                          *      ELTWEEKN
00234 ************************************************************      ELTWEEKN
00235  WEEKEND-ADMISSION.                                               ELTWEEKN
00236      PERFORM INITIALIZATION-ROUTINE.                              ELTWEEKN
00237      PERFORM PROCESS.                                             ELTWEEKN
00238      GOBACK.                                                      ELTWEEKN
00239                                                                   ELTWEEKN
00240                                                                   ELTWEEKN
00241 ************************************************************      ELTWEEKN
00242 *                                                          *      ELTWEEKN
00243 *        INITIALIZATION ROUTINE                            *      ELTWEEKN
00244 *                                                          *      ELTWEEKN
00245 ************************************************************      ELTWEEKN
00246  INITIALIZATION-ROUTINE.                                          ELTWEEKN
00247      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTWEEKN
00248      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTWEEKN
00249                                                                   ELTWEEKN
00250                                                                   ELTWEEKN
00251 ************************************************************      ELTWEEKN
00252 *                                                          *      ELTWEEKN
00253 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTWEEKN
00254 *                                                          *      ELTWEEKN
00255 ************************************************************      ELTWEEKN
00256  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTWEEKN
00257      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTWEEKN
00258      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTWEEKN
00259      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTWEEKN
00260                                                                   ELTWEEKN
00261                                                                   ELTWEEKN
00262 ************************************************************      ELTWEEKN
00263 *                                                          *      ELTWEEKN
00264 *        CHECK FOR VALID COMMAREA                          *      ELTWEEKN
00265 *                                                          *      ELTWEEKN
00266 ************************************************************      ELTWEEKN
00267  CHECK-FOR-VALID-COMMAREA.                                        ELTWEEKN
00268      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTWEEKN
00269          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTWEEKN
00270                                                                   ELTWEEKN
00271                                                                   ELTWEEKN
00272 ************************************************************      ELTWEEKN
00273 *                                                          *      ELTWEEKN
00274 *        SIGNAL INVALID COMMAREA                           *      ELTWEEKN
00275 *                                                          *      ELTWEEKN
00276 ************************************************************      ELTWEEKN
00277  SIGNAL-INVALID-COMMAREA.                                         ELTWEEKN
00278      EXEC CICS ABEND                                              ELTWEEKN
00279                ABCODE('EL01')                                     ELTWEEKN
00280         END-EXEC.                                                 ELTWEEKN
00281      EJECT                                                        ELTWEEKN
00282                                                                   ELTWEEKN
00283                                                                   ELTWEEKN
00284 ************************************************************      ELTWEEKN
00285 *                                                          *      ELTWEEKN
00286 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTWEEKN
00287 *                                                          *      ELTWEEKN
00288 ************************************************************      ELTWEEKN
00289  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTWEEKN
00290      IF ECA-CIA-PTR = NULL                                        ELTWEEKN
00291          PERFORM SIGNAL-INVALID-CIA                               ELTWEEKN
00292      ELSE                                                         ELTWEEKN
00293          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTWEEKN
00294                                                                   ELTWEEKN
00295                                                                   ELTWEEKN
00296 ************************************************************      ELTWEEKN
00297 *                                                          *      ELTWEEKN
00298 *        SIGNAL INVALID CIA                                *      ELTWEEKN
00299 *                                                          *      ELTWEEKN
00300 ************************************************************      ELTWEEKN
00301  SIGNAL-INVALID-CIA.                                              ELTWEEKN
00302      EXEC CICS ABEND                                              ELTWEEKN
00303                ABCODE('EL02')                                     ELTWEEKN
00304         END-EXEC.                                                 ELTWEEKN
00305                                                                   ELTWEEKN
00306                                                                   ELTWEEKN
00307 ************************************************************      ELTWEEKN
00308 *                                                          *      ELTWEEKN
00309 *        ESTABLISH ADDRESS OF CIA                          *      ELTWEEKN
00310 *                                                          *      ELTWEEKN
00311 ************************************************************      ELTWEEKN
00312  ESTABLISH-ADDRESS-OF-CIA.                                        ELTWEEKN
00313      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTWEEKN
00314                            ADDRESS OF                             ELTWEEKN
00315          CIA-ELS-COMMON-INTERFACE-AREA.                           ELTWEEKN
00316      EJECT                                                        ELTWEEKN
00317                                                                   ELTWEEKN
00318                                                                   ELTWEEKN
00319 ************************************************************      ELTWEEKN
00320 *                                                          *      ELTWEEKN
00321 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTWEEKN
00322 *                                                          *      ELTWEEKN
00323 ************************************************************      ELTWEEKN
00324  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTWEEKN
00325      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTWEEKN
00326      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWEEKN
00327                            ADDRESS OF                             ELTWEEKN
00328          SSB-SELECTOR-STATUS-CTL-BLK.                             ELTWEEKN
00329      IF CIA-RC-PTR-NULL                                           ELTWEEKN
00330          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTWEEKN
00331                                                                   ELTWEEKN
00332                                                                   ELTWEEKN
00333 ************************************************************      ELTWEEKN
00334 *                                                          *      ELTWEEKN
00335 *        SIGNAL UNALLOC AREA ERROR                         *      ELTWEEKN
00336 *                                                          *      ELTWEEKN
00337 ************************************************************      ELTWEEKN
00338  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTWEEKN
00339      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTWEEKN
00340      PERFORM SIGNAL-ABEND.                                        ELTWEEKN
00341                                                                   ELTWEEKN
00342                                                                   ELTWEEKN
00343 ************************************************************      ELTWEEKN
00344 *                                                          *      ELTWEEKN
00345 *        SIGNAL ABEND                                      *      ELTWEEKN
00346 *                                                          *      ELTWEEKN
00347 ************************************************************      ELTWEEKN
00348  SIGNAL-ABEND.                                                    ELTWEEKN
00349      EXEC CICS ABEND                                              ELTWEEKN
00350                ABCODE(CIA-ABCODE)                                 ELTWEEKN
00351         END-EXEC.                                                 ELTWEEKN
00352      EJECT                                                        ELTWEEKN
00353                                                                   ELTWEEKN
00354                                                                   ELTWEEKN
00355 ************************************************************      ELTWEEKN
00356 *                                                          *      ELTWEEKN
00357 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTWEEKN
00358 *                                                          *      ELTWEEKN
00359 ************************************************************      ELTWEEKN
00360  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTWEEKN
00361      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTWEEKN
00362      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTWEEKN
00363      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTWEEKN
00364      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTWEEKN
00365      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTWEEKN
00366      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTWEEKN
00367      PERFORM ESTABLISH-ADDRESSABILITY-OF-CS.                      ELTWEEKN
00368                                                                   ELTWEEKN
00369                                                                   ELTWEEKN
00370 ************************************************************      ELTWEEKN
00371 *                                                          *      ELTWEEKN
00372 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTWEEKN
00373 *                                                          *      ELTWEEKN
00374 ************************************************************      ELTWEEKN
00375  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTWEEKN
00376      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTWEEKN
00377      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWEEKN
00378                            ADDRESS OF                             ELTWEEKN
00379          CMF-CODES-MANUAL-INTERFACE.                              ELTWEEKN
00380      IF CIA-RC-PTR-NULL                                           ELTWEEKN
00381          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTWEEKN
00382      EJECT                                                        ELTWEEKN
00383                                                                   ELTWEEKN
00384                                                                   ELTWEEKN
00385 ************************************************************      ELTWEEKN
00386 *                                                          *      ELTWEEKN
00387 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTWEEKN
00388 *                                                          *      ELTWEEKN
00389 ************************************************************      ELTWEEKN
00390  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTWEEKN
00391      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTWEEKN
00392      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWEEKN
00393                            ADDRESS OF                             ELTWEEKN
00394          COF-OUTPUT-INTERFACE.                                    ELTWEEKN
00395      IF CIA-RC-PTR-NULL                                           ELTWEEKN
00396          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTWEEKN
00397      EJECT                                                        ELTWEEKN
00398                                                                   ELTWEEKN
00399                                                                   ELTWEEKN
00400 ************************************************************      ELTWEEKN
00401 *                                                          *      ELTWEEKN
00402 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTWEEKN
00403 *                                                          *      ELTWEEKN
00404 ************************************************************      ELTWEEKN
00405  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTWEEKN
00406      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTWEEKN
00407      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWEEKN
00408                            ADDRESS OF                             ELTWEEKN
00409          SRP-SUBROUTINE-PARAMETERS.                               ELTWEEKN
00410      IF CIA-RC-PTR-NULL                                           ELTWEEKN
00411          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTWEEKN
00412      EJECT                                                        ELTWEEKN
00413                                                                   ELTWEEKN
00414                                                                   ELTWEEKN
00415 ************************************************************      ELTWEEKN
00416 *                                                          *      ELTWEEKN
00417 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTWEEKN
00418 *                                                          *      ELTWEEKN
00419 ************************************************************      ELTWEEKN
00420  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTWEEKN
00421      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTWEEKN
00422      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWEEKN
00423                            ADDRESS OF                             ELTWEEKN
00424          TCAR-COMPRESSION-WORK-AREA.                              ELTWEEKN
00425      IF CIA-RC-PTR-NULL                                           ELTWEEKN
00426          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTWEEKN
00427      EJECT                                                        ELTWEEKN
00428                                                                   ELTWEEKN
00429                                                                   ELTWEEKN
00430 ************************************************************      ELTWEEKN
00431 *                                                          *      ELTWEEKN
00432 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTWEEKN
00433 *                                                          *      ELTWEEKN
00434 ************************************************************      ELTWEEKN
00435  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTWEEKN
00436      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTWEEKN
00437      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWEEKN
00438                            ADDRESS OF                             ELTWEEKN
00439          KWA-FILE-KEY-WORK-AREA.                                  ELTWEEKN
00440      IF CIA-RC-PTR-NULL                                           ELTWEEKN
00441          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTWEEKN
00442      EJECT                                                        ELTWEEKN
00443                                                                   ELTWEEKN
00444                                                                   ELTWEEKN
00445 ************************************************************      ELTWEEKN
00446 *                                                          *      ELTWEEKN
00447 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELTWEEKN
00448 *                                                          *      ELTWEEKN
00449 ************************************************************      ELTWEEKN
00450  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTWEEKN
00451      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTWEEKN
00452      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWEEKN
00453                            ADDRESS OF                             ELTWEEKN
00454          GROUP-SPECIFIC-REC-AREA.                                 ELTWEEKN
00455      IF CIA-RC-PTR-NULL                                           ELTWEEKN
00456          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTWEEKN
00457      EJECT                                                        ELTWEEKN
00458                                                                   ELTWEEKN
00459                                                                   ELTWEEKN
00460 ************************************************************      ELTWEEKN
00461 *                                                          *      ELTWEEKN
00462 *        ESTABLISH ADDRESSABILITY OF CST CONTAINMENT PROGRA*      ELTWEEKN
00463 *                                                          *      ELTWEEKN
00464 ************************************************************      ELTWEEKN
00465  ESTABLISH-ADDRESSABILITY-OF-CS.                                  ELTWEEKN
00466      SET CIA-GCTABULR-DDN TO TRUE.                                ELTWEEKN
00467      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWEEKN
00468                            ADDRESS OF                             ELTWEEKN
00469          GCCP-TABULAR-REC-AREA.                                   ELTWEEKN
00470      IF CIA-RC-PTR-NULL                                           ELTWEEKN
00471          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTWEEKN
00472      EJECT                                                        ELTWEEKN
00473                                                                   ELTWEEKN
00474                                                                   ELTWEEKN
00475 ************************************************************      ELTWEEKN
00476 *                                                          *      ELTWEEKN
00477 *        PROCESS                                           *      ELTWEEKN
00478 *                                                          *      ELTWEEKN
00479 ************************************************************      ELTWEEKN
00480  PROCESS.                                                         ELTWEEKN
00481      IF GCG-FRI-SAT-ADM-IND  = ZERO                               ELTWEEKN
00482          PERFORM GENERATE-NOT-APPLICABLE-MESSAG                   ELTWEEKN
00483      ELSE IF GCG-FRI-SAT-ADM-IND  =  '08'                         ELTWEEKN
00484          PERFORM GENERATE-VOLUNTARY-MESSAGE                       ELTWEEKN
00485      ELSE                                                         ELTWEEKN
00486          PERFORM GENERATE-WEEKN-TEXT.                             ELTWEEKN
00487      PERFORM TERMINATE-OUTPUT.                                    ELTWEEKN
00488                                                                   ELTWEEKN
00489                                                                   ELTWEEKN
00490 ************************************************************      ELTWEEKN
00491 *                                                          *      ELTWEEKN
00492 *        SET UP OUTPUT SUBSCRIPTS                          *      ELTWEEKN
00493 *                                                          *      ELTWEEKN
00494 ************************************************************      ELTWEEKN
00495  SET-UP-OUTPUT-SUBSCRIPTS.                                        ELTWEEKN
00496      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTWEEKN
00497      MOVE +2 TO COF-NBR-HDR-LINES.                                ELTWEEKN
00498      EJECT                                                        ELTWEEKN
00499                                                                   ELTWEEKN
00500                                                                   ELTWEEKN
00501 ************************************************************      ELTWEEKN
00502 *                                                          *      ELTWEEKN
00503 *        EJECT NEW PAGE                                    *      ELTWEEKN
00504 *                                                          *      ELTWEEKN
00505 ************************************************************      ELTWEEKN
00506  EJECT-NEW-PAGE.                                                  ELTWEEKN
00507      SET COF-NEW-PAGE     TO TRUE.                                ELTWEEKN
00508      MOVE WS-HEADER-LINE  TO COF-HDR-LINE                         ELTWEEKN
00509          (COF-NBR-HDR-LINES).                                     ELTWEEKN
00510      PERFORM LINK-TO-OUTPUT.                                      ELTWEEKN
00511                                                                   ELTWEEKN
00512                                                                   ELTWEEKN
00513 ************************************************************      ELTWEEKN
00514 *                                                          *      ELTWEEKN
00515 *        GENERATE NOT APPLICABLE MESSAGE                   *      ELTWEEKN
00516 *                                                          *      ELTWEEKN
00517 ************************************************************      ELTWEEKN
00518  GENERATE-NOT-APPLICABLE-MESSAG.                                  ELTWEEKN
00519      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTWEEKN
00520      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTWEEKN
00521      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTWEEKN
00522      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTWEEKN
00523          (COF-NBR-DTL-LINES).                                     ELTWEEKN
00524      PERFORM EJECT-NEW-PAGE.                                      ELTWEEKN
00525      EJECT                                                        ELTWEEKN
00526                                                                   ELTWEEKN
00527                                                                   ELTWEEKN
00528 ************************************************************      ELTWEEKN
00529 *                                                          *      ELTWEEKN
00530 *        GENERATE VOLUNTARY MESSAGE                        *      ELTWEEKN
00531 *                                                          *      ELTWEEKN
00532 ************************************************************      ELTWEEKN
00533  GENERATE-VOLUNTARY-MESSAGE.                                      ELTWEEKN
00534      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTWEEKN
00535      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTWEEKN
00536      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTWEEKN
00537      MOVE WS-VOLUNTARY-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).   ELTWEEKN
00538      PERFORM EJECT-NEW-PAGE.                                      ELTWEEKN
00539      EJECT                                                        ELTWEEKN
00540                                                                   ELTWEEKN
00541                                                                   ELTWEEKN
00542 ************************************************************      ELTWEEKN
00543 *                                                          *      ELTWEEKN
00544 *        GENERATE WEEKN TEXT                               *      ELTWEEKN
00545 *                                                          *      ELTWEEKN
00546 ************************************************************      ELTWEEKN
00547  GENERATE-WEEKN-TEXT.                                             ELTWEEKN
00548      SET WS-POINTER2 TO NULLS.                                    ELTWEEKN
00549      SET WS-POINTER3 TO NULLS.                                    ELTWEEKN
00550      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTWEEKN
00551      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTWEEKN
00552                            WS-POINTER2.                           ELTWEEKN
00553      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTWEEKN
00554      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTWEEKN
00555                            WS-POINTER3.                           ELTWEEKN
00556      PERFORM SEARCH-FOR-GCCP-TABULAR.                             ELTWEEKN
00557      PERFORM DETERMINE-SELECTION.                                 ELTWEEKN
00558      EJECT                                                        ELTWEEKN
00559                                                                   ELTWEEKN
00560                                                                   ELTWEEKN
00561 ************************************************************      ELTWEEKN
00562 *                                                          *      ELTWEEKN
00563 *        TERMINATE OUTPUT                                  *      ELTWEEKN
00564 *                                                          *      ELTWEEKN
00565 ************************************************************      ELTWEEKN
00566  TERMINATE-OUTPUT.                                                ELTWEEKN
00567      SET COF-END TO TRUE.                                         ELTWEEKN
00568      PERFORM LINK-TO-OUTPUT.                                      ELTWEEKN
00569      EJECT                                                        ELTWEEKN
00570                                                                   ELTWEEKN
00571                                                                   ELTWEEKN
00572 ************************************************************      ELTWEEKN
00573 *                                                          *      ELTWEEKN
00574 *        SEARCH FOR GCCP TABULAR                           *      ELTWEEKN
00575 *                                                          *      ELTWEEKN
00576 ************************************************************      ELTWEEKN
00577  SEARCH-FOR-GCCP-TABULAR.                                         ELTWEEKN
00578      SET GCG-INDEX TO +1.                                         ELTWEEKN
00579      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTWEEKN
00580         AT END                                                    ELTWEEKN
00581              MOVE ZEROES TO KWA-PROVISION-SLOT-NO                 ELTWEEKN
00582         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GCCP                     ELTWEEKN
00583              MOVE GCG-TAB-ID (GCG-INDEX)                          ELTWEEKN
00584                  TO KWA-PROVISION-ID                              ELTWEEKN
00585              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTWEEKN
00586                  TO KWA-PROVISION-SLOT-NO                         ELTWEEKN
00587         END-SEARCH.                                               ELTWEEKN
00588      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELTWEEKN
00589          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTWEEKN
00590      PERFORM GET-GCCP-TABULAR.                                    ELTWEEKN
00591      PERFORM SEARCH-THE-GSS-ENTRY.                                ELTWEEKN
00592      EJECT                                                        ELTWEEKN
00593                                                                   ELTWEEKN
00594                                                                   ELTWEEKN
00595 ************************************************************      ELTWEEKN
00596 *                                                          *      ELTWEEKN
00597 *        TRANSLATE PARTICIPATION INDICATOR                 *      ELTWEEKN
00598 *                                                          *      ELTWEEKN
00599 ************************************************************      ELTWEEKN
00600  TRANSLATE-PARTICIPATION-INDICA.                                  ELTWEEKN
00601      SET PERIOD-NEEDED TO TRUE.                                   ELTWEEKN
00602      INITIALIZE TCAR-FROM-AREA.                                   ELTWEEKN
00603      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
00604      MOVE WS-PARTICIPATION-LINE TO TCAR-FROM-LINE                 ELTWEEKN
00605          (TCAR-FROM-SUB).                                         ELTWEEKN
00606      ADD  +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
00607      SET BLANK-LINE-NEEDED TO TRUE.                               ELTWEEKN
00608      MOVE GCG-FRI-SAT-ADM-IND TO CMF-CODE-VALUE.                  ELTWEEKN
00609      MOVE 'FRI-SAT-ADM-IND' TO CMF-ELEMENT-SYSTEM-NAME.           ELTWEEKN
00610      PERFORM CALL-GROUP-CODES-MANUAL-INTERF.                      ELTWEEKN
00611      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTWEEKN
00612      EJECT                                                        ELTWEEKN
00613                                                                   ELTWEEKN
00614                                                                   ELTWEEKN
00615 ************************************************************      ELTWEEKN
00616 *                                                          *      ELTWEEKN
00617 *        TRANSLATE APPROVAL SOURCE                         *      ELTWEEKN
00618 *                                                          *      ELTWEEKN
00619 ************************************************************      ELTWEEKN
00620  TRANSLATE-APPROVAL-SOURCE.                                       ELTWEEKN
00621      SET BLANK-LINE-NEEDED TO TRUE.                               ELTWEEKN
00622      SET PERIOD-NEEDED TO TRUE.                                   ELTWEEKN
00623      INITIALIZE TCAR-FROM-AREA.                                   ELTWEEKN
00624      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
00625      MOVE WS-APPRVL-LINE TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELTWEEKN
00626      ADD  +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
00627      IF NOT-HOLDING-APPROVAL-SRCE                                 ELTWEEKN
00628          PERFORM GET-APPROVAL-TRANSLATION                         ELTWEEKN
00629      ELSE                                                         ELTWEEKN
00630          PERFORM USE-EXISTING-APPROVAL-TRANSLAT.                  ELTWEEKN
00631      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTWEEKN
00632      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTWEEKN
00633      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTWEEKN
00634                            WS-POINTER2.                           ELTWEEKN
00635      EJECT                                                        ELTWEEKN
00636                                                                   ELTWEEKN
00637                                                                   ELTWEEKN
00638 ************************************************************      ELTWEEKN
00639 *                                                          *      ELTWEEKN
00640 *        GET APPROVAL TRANSLATION                          *      ELTWEEKN
00641 *                                                          *      ELTWEEKN
00642 ************************************************************      ELTWEEKN
00643  GET-APPROVAL-TRANSLATION.                                        ELTWEEKN
00644      MOVE GSS-FS-APPROVAL-SOURCE-IND (GSS-INDEX) TO               ELTWEEKN
00645          CMF-CODE-VALUE.                                          ELTWEEKN
00646      MOVE   'FS-APPROVAL-SOURCE-IND' TO CMF-ELEMENT-SYSTEM-NAME.  ELTWEEKN
00647      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTWEEKN
00648      SET HOLDING-APPROVAL-SOURCE TO TRUE.                         ELTWEEKN
00649      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTWEEKN
00650      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWEEKN
00651                            ADDRESS OF CMF-DESCR.                  ELTWEEKN
00652      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTWEEKN
00653      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTWEEKN
00654                            ADDRESS OF CMF-DESCR.                  ELTWEEKN
00655      SET WS-POINTER2 TO ADDRESS OF CMF-DESCR.                     ELTWEEKN
00656      EJECT                                                        ELTWEEKN
00657                                                                   ELTWEEKN
00658                                                                   ELTWEEKN
00659 ************************************************************      ELTWEEKN
00660 *                                                          *      ELTWEEKN
00661 *        USE EXISTING APPROVAL TRANSLATION                 *      ELTWEEKN
00662 *                                                          *      ELTWEEKN
00663 ************************************************************      ELTWEEKN
00664  USE-EXISTING-APPROVAL-TRANSLAT.                                  ELTWEEKN
00665      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTWEEKN
00666      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTWEEKN
00667                            ADDRESS OF CMF-DESCR.                  ELTWEEKN
00668      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTWEEKN
00669      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTWEEKN
00670                            WS-POINTER2.                           ELTWEEKN
00671                                                                   ELTWEEKN
00672                                                                   ELTWEEKN
00673 ************************************************************      ELTWEEKN
00674 *                                                          *      ELTWEEKN
00675 *        TRANSLATE AND DISPLAY CODE VALUE                  *      ELTWEEKN
00676 *                                                          *      ELTWEEKN
00677 ************************************************************      ELTWEEKN
00678  TRANSLATE-AND-DISPLAY-CODE-VAL.                                  ELTWEEKN
00679      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTWEEKN
00680      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTWEEKN
00681                                                                   ELTWEEKN
00682                                                                   ELTWEEKN
00683 ************************************************************      ELTWEEKN
00684 *                                                          *      ELTWEEKN
00685 *        SIGNAL UNDEFINED TABULAR ERROR                    *      ELTWEEKN
00686 *                                                          *      ELTWEEKN
00687 ************************************************************      ELTWEEKN
00688  SIGNAL-UNDEFINED-TABULAR-ERROR.                                  ELTWEEKN
00689      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTWEEKN
00690      PERFORM SIGNAL-ABEND.                                        ELTWEEKN
00691      EJECT                                                        ELTWEEKN
00692                                                                   ELTWEEKN
00693                                                                   ELTWEEKN
00694 ************************************************************      ELTWEEKN
00695 *                                                          *      ELTWEEKN
00696 *        DETERMINE SELECTION                               *      ELTWEEKN
00697 *                                                          *      ELTWEEKN
00698 ************************************************************      ELTWEEKN
00699  DETERMINE-SELECTION.                                             ELTWEEKN
00700      IF SSB-PROV-CLASS-INST OR                                    ELTWEEKN
00701                 SSB-PROV-CLASS-BOTH                               ELTWEEKN
00702          PERFORM CREATE-INSTITUTIONAL-SCREEN.                     ELTWEEKN
00703      IF SSB-PROV-CLASS-PROF OR                                    ELTWEEKN
00704                 SSB-PROV-CLASS-BOTH                               ELTWEEKN
00705          PERFORM CREATE-PROFESSIONAL-SCREEN.                      ELTWEEKN
00706      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL '03' OR '04' OR       ELTWEEKN
00707                  '06' OR '08')                                    ELTWEEKN
00708          PERFORM CREATE-SUPPLEMENTAL-SCREEN.                      ELTWEEKN
00709                                                                   ELTWEEKN
00710                                                                   ELTWEEKN
00711 ************************************************************      ELTWEEKN
00712 *                                                          *      ELTWEEKN
00713 *        CREATE INSTITUTIONAL SCREEN                       *      ELTWEEKN
00714 *                                                          *      ELTWEEKN
00715 ************************************************************      ELTWEEKN
00716  CREATE-INSTITUTIONAL-SCREEN.                                     ELTWEEKN
00717      MOVE WS-INST TO WS-HDR-LINE-BCBSMM.                          ELTWEEKN
00718      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTWEEKN
00719      IF GSS-FS-BC-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTWEEKN
00720          ZEROES                                                   ELTWEEKN
00721                 AND LOW-VALUES                                    ELTWEEKN
00722          PERFORM GENERATE-INSTITUTIONAL-TEXT                      ELTWEEKN
00723      ELSE                                                         ELTWEEKN
00724          PERFORM GENERATE-INSTITUTIONAL-NOT-APP.                  ELTWEEKN
00725      EJECT                                                        ELTWEEKN
00726                                                                   ELTWEEKN
00727                                                                   ELTWEEKN
00728 ************************************************************      ELTWEEKN
00729 *                                                          *      ELTWEEKN
00730 *        GENERATE INSTITUTIONAL TEXT                       *      ELTWEEKN
00731 *                                                          *      ELTWEEKN
00732 ************************************************************      ELTWEEKN
00733  GENERATE-INSTITUTIONAL-TEXT.                                     ELTWEEKN
00734      PERFORM EJECT-NEW-PAGE.                                      ELTWEEKN
00735      PERFORM TRANSLATE-PARTICIPATION-INDICA.                      ELTWEEKN
00736      SET NOT-HOLDING-APPROVAL-SRCE TO TRUE.                       ELTWEEKN
00737      IF GSS-FS-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTWEEKN
00738                 SPACES AND ZEROES AND LOW-VALUES                  ELTWEEKN
00739          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTWEEKN
00740      PERFORM TRANSLATE-BC-IND.                                    ELTWEEKN
00741      IF GSS-FS-BC-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL          ELTWEEKN
00742                 SPACES AND ZEROES AND LOW-VALUES                  ELTWEEKN
00743          PERFORM TRANSLATE-BC-ALT-PRIC.                           ELTWEEKN
00744      SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTWEEKN
00745      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTWEEKN
00746      IF GSS-FS-BC-CALC-METHOD (GSS-INDEX) NOT EQUAL               ELTWEEKN
00747                 SPACES AND ZEROES AND LOW-VALUES                  ELTWEEKN
00748          PERFORM TRANSLATE-BC-CALC-METHOD.                        ELTWEEKN
00749      IF GSS-FS-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTWEEKN
00750                 SPACES AND ZEROES AND LOW-VALUES                  ELTWEEKN
00751          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTWEEKN
00752      IF ((GSS-FS-BC-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL         ELTWEEKN
00753                   SPACES AND ZEROES AND LOW-VALUES)) OR           ELTWEEKN
00754                 ((GSS-FS-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT       ELTWEEKN
00755          EQUAL                                                    ELTWEEKN
00756                   SPACES AND ZEROES AND LOW-VALUES)) OR           ELTWEEKN
00757                ((GSS-FS-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT      ELTWEEKN
00758          EQUAL                                                    ELTWEEKN
00759                   SPACES AND ZEROES AND LOW-VALUES))              ELTWEEKN
00760          PERFORM GENERATE-BC-BENE-REDUCT-TEXT.                    ELTWEEKN
00761      IF GSS-FS-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL ZEROES        ELTWEEKN
00762          AND                                                      ELTWEEKN
00763                 SPACES AND LOW-VALUES                             ELTWEEKN
00764          PERFORM GENERATE-SPILL-OVER-TEXT.                        ELTWEEKN
00765      PERFORM GENERATE-RELATED-SERVICES-SENT.                      ELTWEEKN
00766      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTWEEKN
00767      EJECT                                                        ELTWEEKN
00768                                                                   ELTWEEKN
00769                                                                   ELTWEEKN
00770 ************************************************************      ELTWEEKN
00771 *                                                          *      ELTWEEKN
00772 *        GENERATE INSTITUTIONAL NOT APPLICABLE             *      ELTWEEKN
00773 *                                                          *      ELTWEEKN
00774 ************************************************************      ELTWEEKN
00775  GENERATE-INSTITUTIONAL-NOT-APP.                                  ELTWEEKN
00776      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTWEEKN
00777      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTWEEKN
00778      MOVE WS-NOT-APPLY-TO-LOB-BC TO COF-DTL-LINE                  ELTWEEKN
00779          (COF-NBR-DTL-LINES).                                     ELTWEEKN
00780      PERFORM EJECT-NEW-PAGE.                                      ELTWEEKN
00781      EJECT                                                        ELTWEEKN
00782                                                                   ELTWEEKN
00783                                                                   ELTWEEKN
00784 ************************************************************      ELTWEEKN
00785 *                                                          *      ELTWEEKN
00786 *        GENERATE DISCLAIMER MESSAGE                       *      ELTWEEKN
00787 *                                                          *      ELTWEEKN
00788 ************************************************************      ELTWEEKN
00789  GENERATE-DISCLAIMER-MESSAGE.                                     ELTWEEKN
00790      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTWEEKN
00791      MOVE WS-DISCLAIMER-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).  ELTWEEKN
00792      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTWEEKN
00793      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTWEEKN
00794      PERFORM LINK-TO-OUTPUT.                                      ELTWEEKN
00795      EJECT                                                        ELTWEEKN
00796                                                                   ELTWEEKN
00797                                                                   ELTWEEKN
00798 ************************************************************      ELTWEEKN
00799 *                                                          *      ELTWEEKN
00800 *        TRANSLATE BC IND                                  *      ELTWEEKN
00801 *                                                          *      ELTWEEKN
00802 ************************************************************      ELTWEEKN
00803  TRANSLATE-BC-IND.                                                ELTWEEKN
00804      INITIALIZE TCAR-FROM-AREA.                                   ELTWEEKN
00805      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
00806      SET BLANK-LINE-NEEDED TO TRUE.                               ELTWEEKN
00807      SET PERIOD-NEEDED TO TRUE.                                   ELTWEEKN
00808      MOVE GSS-FS-BC-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTWEEKN
00809      MOVE   'FS-BC-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTWEEKN
00810      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTWEEKN
00811      EJECT                                                        ELTWEEKN
00812                                                                   ELTWEEKN
00813                                                                   ELTWEEKN
00814 ************************************************************      ELTWEEKN
00815 *                                                          *      ELTWEEKN
00816 *        TRANSLATE BC CALC METHOD                          *      ELTWEEKN
00817 *                                                          *      ELTWEEKN
00818 ************************************************************      ELTWEEKN
00819  TRANSLATE-BC-CALC-METHOD.                                        ELTWEEKN
00820      INITIALIZE TCAR-FROM-AREA.                                   ELTWEEKN
00821      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
00822      SET BLANK-LINE-NEEDED TO TRUE.                               ELTWEEKN
00823      SET PERIOD-NEEDED TO TRUE.                                   ELTWEEKN
00824      MOVE GSS-FS-BC-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTWEEKN
00825      MOVE   'FS-BC-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTWEEKN
00826      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTWEEKN
00827      EJECT                                                        ELTWEEKN
00828                                                                   ELTWEEKN
00829                                                                   ELTWEEKN
00830 ************************************************************      ELTWEEKN
00831 *                                                          *      ELTWEEKN
00832 *        TRANSLATE BC ALT PRIC                             *      ELTWEEKN
00833 *                                                          *      ELTWEEKN
00834 ************************************************************      ELTWEEKN
00835  TRANSLATE-BC-ALT-PRIC.                                           ELTWEEKN
00836      INITIALIZE TCAR-FROM-AREA.                                   ELTWEEKN
00837      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
00838      MOVE WS-ALT-PRICING-LINE-BC TO TCAR-FROM-LINE                ELTWEEKN
00839          (TCAR-FROM-SUB).                                         ELTWEEKN
00840      ADD  +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
00841      SET BLANK-LINE-NEEDED TO TRUE.                               ELTWEEKN
00842      SET PERIOD-NEEDED TO TRUE.                                   ELTWEEKN
00843      MOVE GSS-FS-BC-ALT-PRICING-METH (GSS-INDEX) TO               ELTWEEKN
00844          CMF-CODE-VALUE.                                          ELTWEEKN
00845      MOVE  'FS-BC-ALT-PRICING-METH' TO CMF-ELEMENT-SYSTEM-NAME.   ELTWEEKN
00846      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTWEEKN
00847      EJECT                                                        ELTWEEKN
00848                                                                   ELTWEEKN
00849                                                                   ELTWEEKN
00850 ************************************************************      ELTWEEKN
00851 *                                                          *      ELTWEEKN
00852 *        GENERATE ACCUM TABULAR DATA                       *      ELTWEEKN
00853 *                                                          *      ELTWEEKN
00854 ************************************************************      ELTWEEKN
00855  GENERATE-ACCUM-TABULAR-DATA.                                     ELTWEEKN
00856      PERFORM GENERATE-COINSURANCE.                                ELTWEEKN
00857      PERFORM GENERATE-COPAY.                                      ELTWEEKN
00858      PERFORM GENERATE-DEDUCTIBLE.                                 ELTWEEKN
00859      PERFORM GENERATE-MAXIMUM.                                    ELTWEEKN
00860      EJECT                                                        ELTWEEKN
00861                                                                   ELTWEEKN
00862                                                                   ELTWEEKN
00863 ************************************************************      ELTWEEKN
00864 *                                                          *      ELTWEEKN
00865 *        GENERATE COMBINED BENEFITS REDUCTION TEXT         *      ELTWEEKN
00866 *                                                          *      ELTWEEKN
00867 ************************************************************      ELTWEEKN
00868  GENERATE-COMBINED-BENEFITS-RED.                                  ELTWEEKN
00869      MOVE 'FS' TO SRP-COST-CONT-TYPE.                             ELTWEEKN
00870      MOVE 'WEEKEND ADMISSION PROGRAM' TO SRP-CCP-NAME.            ELTWEEKN
00871      MOVE GSS-FS-COMB-BENE-REDUCT-IND (GSS-INDEX)                 ELTWEEKN
00872                  TO SRP-CCP-COMB-BENE-REDUCT-IND.                 ELTWEEKN
00873      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELTWEEKN
00874      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTWEEKN
00875                            ADDRESS OF                             ELTWEEKN
00876          GCCP-TABULAR-REC-AREA.                                   ELTWEEKN
00877      PERFORM CALL-CBRI-INTERFACE.                                 ELTWEEKN
00878      EJECT                                                        ELTWEEKN
00879                                                                   ELTWEEKN
00880                                                                   ELTWEEKN
00881 ************************************************************      ELTWEEKN
00882 *                                                          *      ELTWEEKN
00883 *        GENERATE BC BENE REDUCT TEXT                      *      ELTWEEKN
00884 *                                                          *      ELTWEEKN
00885 ************************************************************      ELTWEEKN
00886  GENERATE-BC-BENE-REDUCT-TEXT.                                    ELTWEEKN
00887      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTWEEKN
00888      MOVE WS-BENE-REDUCT-LINE TO COF-DTL-LINE                     ELTWEEKN
00889          (COF-NBR-DTL-LINES).                                     ELTWEEKN
00890      PERFORM LINK-TO-OUTPUT.                                      ELTWEEKN
00891      INITIALIZE ADDITIONAL-TEXT-SWITCH.                           ELTWEEKN
00892      INITIALIZE WS-PERIOD-SWITCH.                                 ELTWEEKN
00893      INITIALIZE TCAR-FROM-AREA.                                   ELTWEEKN
00894      IF GSS-FS-BC-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTWEEKN
00895          SPACES                                                   ELTWEEKN
00896                 AND ZEROES AND LOW-VALUES                         ELTWEEKN
00897          PERFORM TRANSLATE-BC-DEDUCT-IND.                         ELTWEEKN
00898      IF GSS-FS-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTWEEKN
00899          SPACES                                                   ELTWEEKN
00900                 AND ZEROES AND LOW-VALUES                         ELTWEEKN
00901          PERFORM TRANSLATE-BC-OPEX-APPLIC.                        ELTWEEKN
00902      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTWEEKN
00903      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTWEEKN
00904      PERFORM LINK-TO-OUTPUT.                                      ELTWEEKN
00905      IF GSS-FS-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTWEEKN
00906          SPACES                                                   ELTWEEKN
00907                 AND ZEROES AND LOW-VALUES                         ELTWEEKN
00908          PERFORM TRANSLATE-BC-OPEX-OVERRIDE.                      ELTWEEKN
00909      EJECT                                                        ELTWEEKN
00910                                                                   ELTWEEKN
00911                                                                   ELTWEEKN
00912 ************************************************************      ELTWEEKN
00913 *                                                          *      ELTWEEKN
00914 *        TRANSLATE BC DEDUCT IND                           *      ELTWEEKN
00915 *                                                          *      ELTWEEKN
00916 ************************************************************      ELTWEEKN
00917  TRANSLATE-BC-DEDUCT-IND.                                         ELTWEEKN
00918      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
00919      MOVE GSS-FS-BC-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTWEEKN
00920          CMF-CODE-VALUE.                                          ELTWEEKN
00921      MOVE 'FS-BC-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTWEEKN
00922      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTWEEKN
00923      EJECT                                                        ELTWEEKN
00924                                                                   ELTWEEKN
00925                                                                   ELTWEEKN
00926 ************************************************************      ELTWEEKN
00927 *                                                          *      ELTWEEKN
00928 *        TRANSLATE BC OPEX APPLIC                          *      ELTWEEKN
00929 *                                                          *      ELTWEEKN
00930 ************************************************************      ELTWEEKN
00931  TRANSLATE-BC-OPEX-APPLIC.                                        ELTWEEKN
00932      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
00933      MOVE GSS-FS-BC-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTWEEKN
00934          CMF-CODE-VALUE.                                          ELTWEEKN
00935      MOVE 'FS-BC-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTWEEKN
00936      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTWEEKN
00937      EJECT                                                        ELTWEEKN
00938                                                                   ELTWEEKN
00939                                                                   ELTWEEKN
00940 ************************************************************      ELTWEEKN
00941 *                                                          *      ELTWEEKN
00942 *        TRANSLATE BC OPEX OVERRIDE                        *      ELTWEEKN
00943 *                                                          *      ELTWEEKN
00944 ************************************************************      ELTWEEKN
00945  TRANSLATE-BC-OPEX-OVERRIDE.                                      ELTWEEKN
00946      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
00947      SET BLANK-LINE-NEEDED TO TRUE.                               ELTWEEKN
00948      SET PERIOD-NEEDED TO TRUE.                                   ELTWEEKN
00949      MOVE GSS-FS-BC-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTWEEKN
00950          CMF-CODE-VALUE.                                          ELTWEEKN
00951      MOVE 'FS-BC-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTWEEKN
00952      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTWEEKN
00953      EJECT                                                        ELTWEEKN
00954                                                                   ELTWEEKN
00955                                                                   ELTWEEKN
00956 ************************************************************      ELTWEEKN
00957 *                                                          *      ELTWEEKN
00958 *        CREATE PROFESSIONAL SCREEN                        *      ELTWEEKN
00959 *                                                          *      ELTWEEKN
00960 ************************************************************      ELTWEEKN
00961  CREATE-PROFESSIONAL-SCREEN.                                      ELTWEEKN
00962      MOVE WS-PROF TO WS-HDR-LINE-BCBSMM.                          ELTWEEKN
00963      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTWEEKN
00964      IF GSS-FS-BS-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTWEEKN
00965          ZEROES                                                   ELTWEEKN
00966                 AND LOW-VALUES                                    ELTWEEKN
00967          PERFORM GENERATE-PROFESSIONAL-TEXT                       ELTWEEKN
00968      ELSE                                                         ELTWEEKN
00969          PERFORM GENERATE-PROFESSIONAL-NOT-APPL.                  ELTWEEKN
00970      EJECT                                                        ELTWEEKN
00971                                                                   ELTWEEKN
00972                                                                   ELTWEEKN
00973 ************************************************************      ELTWEEKN
00974 *                                                          *      ELTWEEKN
00975 *        GENERATE PROFESSIONAL TEXT                        *      ELTWEEKN
00976 *                                                          *      ELTWEEKN
00977 ************************************************************      ELTWEEKN
00978  GENERATE-PROFESSIONAL-TEXT.                                      ELTWEEKN
00979      PERFORM EJECT-NEW-PAGE.                                      ELTWEEKN
00980      PERFORM TRANSLATE-PARTICIPATION-INDICA.                      ELTWEEKN
00981      IF GSS-FS-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTWEEKN
00982          SPACES                                                   ELTWEEKN
00983                 AND ZEROES AND LOW-VALUES                         ELTWEEKN
00984          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTWEEKN
00985      PERFORM TRANSLATE-BS-IND.                                    ELTWEEKN
00986      IF GSS-FS-BS-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL SPACES   ELTWEEKN
00987          AND                                                      ELTWEEKN
00988                 ZEROES AND LOW-VALUES                             ELTWEEKN
00989          PERFORM TRANSLATE-BS-ALT-PRIC.                           ELTWEEKN
00990      SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTWEEKN
00991      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTWEEKN
00992      IF GSS-FS-BS-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES        ELTWEEKN
00993          AND                                                      ELTWEEKN
00994                 ZEROES AND LOW-VALUES                             ELTWEEKN
00995          PERFORM TRANSLATE-BS-CALC-METHOD.                        ELTWEEKN
00996      IF GSS-FS-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTWEEKN
00997          SPACES                                                   ELTWEEKN
00998                 AND ZEROES AND LOW-VALUES                         ELTWEEKN
00999          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTWEEKN
01000      IF ((GSS-FS-BS-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL         ELTWEEKN
01001          SPACES AND                                               ELTWEEKN
01002                   ZEROES AND LOW-VALUES)) OR                      ELTWEEKN
01003                 ((GSS-FS-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT       ELTWEEKN
01004          EQUAL SPACES AND                                         ELTWEEKN
01005                   ZEROES AND LOW-VALUES)) OR                      ELTWEEKN
01006                ((GSS-FS-BS-OPEX-OVERRIDE-IND (GSS-INDEX) NOT      ELTWEEKN
01007          EQUAL SPACES AND                                         ELTWEEKN
01008                   ZEROES AND LOW-VALUES))                         ELTWEEKN
01009          PERFORM GENERATE-BS-BENE-REDUCT-TEXT.                    ELTWEEKN
01010      IF GSS-FS-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL ZEROES        ELTWEEKN
01011          AND                                                      ELTWEEKN
01012                 SPACES AND LOW-VALUES                             ELTWEEKN
01013          PERFORM GENERATE-SPILL-OVER-TEXT.                        ELTWEEKN
01014      PERFORM GENERATE-RELATED-SERVICES-SENT.                      ELTWEEKN
01015      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTWEEKN
01016                                                                   ELTWEEKN
01017                                                                   ELTWEEKN
01018 ************************************************************      ELTWEEKN
01019 *                                                          *      ELTWEEKN
01020 *        GENERATE PROFESSIONAL NOT APPLICABLE              *      ELTWEEKN
01021 *                                                          *      ELTWEEKN
01022 ************************************************************      ELTWEEKN
01023  GENERATE-PROFESSIONAL-NOT-APPL.                                  ELTWEEKN
01024      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTWEEKN
01025      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTWEEKN
01026      MOVE WS-NOT-APPLY-TO-LOB-BS TO COF-DTL-LINE                  ELTWEEKN
01027          (COF-NBR-DTL-LINES).                                     ELTWEEKN
01028      PERFORM EJECT-NEW-PAGE.                                      ELTWEEKN
01029      EJECT                                                        ELTWEEKN
01030                                                                   ELTWEEKN
01031                                                                   ELTWEEKN
01032 ************************************************************      ELTWEEKN
01033 *                                                          *      ELTWEEKN
01034 *        TRANSLATE BS IND                                  *      ELTWEEKN
01035 *                                                          *      ELTWEEKN
01036 ************************************************************      ELTWEEKN
01037  TRANSLATE-BS-IND.                                                ELTWEEKN
01038      INITIALIZE TCAR-FROM-AREA.                                   ELTWEEKN
01039      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
01040      SET BLANK-LINE-NEEDED TO TRUE.                               ELTWEEKN
01041      SET PERIOD-NEEDED TO TRUE.                                   ELTWEEKN
01042      MOVE GSS-FS-BS-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTWEEKN
01043      MOVE   'FS-BS-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTWEEKN
01044      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTWEEKN
01045      EJECT                                                        ELTWEEKN
01046                                                                   ELTWEEKN
01047                                                                   ELTWEEKN
01048 ************************************************************      ELTWEEKN
01049 *                                                          *      ELTWEEKN
01050 *        TRANSLATE BS CALC METHOD                          *      ELTWEEKN
01051 *                                                          *      ELTWEEKN
01052 ************************************************************      ELTWEEKN
01053  TRANSLATE-BS-CALC-METHOD.                                        ELTWEEKN
01054      INITIALIZE TCAR-FROM-AREA.                                   ELTWEEKN
01055      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
01056      SET BLANK-LINE-NEEDED TO TRUE.                               ELTWEEKN
01057      SET PERIOD-NEEDED TO TRUE.                                   ELTWEEKN
01058      MOVE GSS-FS-BS-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTWEEKN
01059      MOVE   'FS-BS-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTWEEKN
01060      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTWEEKN
01061      EJECT                                                        ELTWEEKN
01062                                                                   ELTWEEKN
01063                                                                   ELTWEEKN
01064 ************************************************************      ELTWEEKN
01065 *                                                          *      ELTWEEKN
01066 *        TRANSLATE BS ALT PRIC                             *      ELTWEEKN
01067 *                                                          *      ELTWEEKN
01068 ************************************************************      ELTWEEKN
01069  TRANSLATE-BS-ALT-PRIC.                                           ELTWEEKN
01070      INITIALIZE TCAR-FROM-AREA.                                   ELTWEEKN
01071      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
01072      MOVE WS-ALT-PRICING-LINE-BS TO TCAR-FROM-LINE                ELTWEEKN
01073          (TCAR-FROM-SUB).                                         ELTWEEKN
01074      ADD  +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
01075      SET BLANK-LINE-NEEDED TO TRUE.                               ELTWEEKN
01076      SET PERIOD-NEEDED TO TRUE.                                   ELTWEEKN
01077      MOVE GSS-FS-BS-ALT-PRICING-METH (GSS-INDEX) TO               ELTWEEKN
01078          CMF-CODE-VALUE.                                          ELTWEEKN
01079      MOVE  'FS-BS-ALT-PRICING-METH' TO CMF-ELEMENT-SYSTEM-NAME.   ELTWEEKN
01080      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTWEEKN
01081      EJECT                                                        ELTWEEKN
01082                                                                   ELTWEEKN
01083                                                                   ELTWEEKN
01084 ************************************************************      ELTWEEKN
01085 *                                                          *      ELTWEEKN
01086 *        GENERATE BS BENE REDUCT TEXT                      *      ELTWEEKN
01087 *                                                          *      ELTWEEKN
01088 ************************************************************      ELTWEEKN
01089  GENERATE-BS-BENE-REDUCT-TEXT.                                    ELTWEEKN
01090      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTWEEKN
01091      MOVE WS-BENE-REDUCT-LINE TO COF-DTL-LINE                     ELTWEEKN
01092          (COF-NBR-DTL-LINES).                                     ELTWEEKN
01093      PERFORM LINK-TO-OUTPUT.                                      ELTWEEKN
01094      INITIALIZE ADDITIONAL-TEXT-SWITCH.                           ELTWEEKN
01095      INITIALIZE WS-PERIOD-SWITCH.                                 ELTWEEKN
01096      INITIALIZE TCAR-FROM-AREA.                                   ELTWEEKN
01097      IF GSS-FS-BS-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTWEEKN
01098          SPACES                                                   ELTWEEKN
01099                 AND ZEROES AND LOW-VALUES                         ELTWEEKN
01100          PERFORM TRANSLATE-BS-DEDUCT-IND.                         ELTWEEKN
01101      IF GSS-FS-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTWEEKN
01102          SPACES                                                   ELTWEEKN
01103                 AND ZEROES AND LOW-VALUES                         ELTWEEKN
01104          PERFORM TRANSLATE-BS-OPEX-APPLIC.                        ELTWEEKN
01105      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTWEEKN
01106      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTWEEKN
01107      PERFORM LINK-TO-OUTPUT.                                      ELTWEEKN
01108      IF GSS-FS-BS-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTWEEKN
01109          SPACES                                                   ELTWEEKN
01110                 AND ZEROES AND LOW-VALUES                         ELTWEEKN
01111          PERFORM TRANSLATE-BS-OPEX-OVERRIDE.                      ELTWEEKN
01112      EJECT                                                        ELTWEEKN
01113                                                                   ELTWEEKN
01114                                                                   ELTWEEKN
01115 ************************************************************      ELTWEEKN
01116 *                                                          *      ELTWEEKN
01117 *        TRANSLATE BS DEDUCT IND                           *      ELTWEEKN
01118 *                                                          *      ELTWEEKN
01119 ************************************************************      ELTWEEKN
01120  TRANSLATE-BS-DEDUCT-IND.                                         ELTWEEKN
01121      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
01122      MOVE GSS-FS-BS-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTWEEKN
01123          CMF-CODE-VALUE.                                          ELTWEEKN
01124      MOVE 'FS-BS-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTWEEKN
01125      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTWEEKN
01126      EJECT                                                        ELTWEEKN
01127                                                                   ELTWEEKN
01128                                                                   ELTWEEKN
01129 ************************************************************      ELTWEEKN
01130 *                                                          *      ELTWEEKN
01131 *        TRANSLATE BS OPEX APPLIC                          *      ELTWEEKN
01132 *                                                          *      ELTWEEKN
01133 ************************************************************      ELTWEEKN
01134  TRANSLATE-BS-OPEX-APPLIC.                                        ELTWEEKN
01135      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
01136      MOVE GSS-FS-BS-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTWEEKN
01137          CMF-CODE-VALUE.                                          ELTWEEKN
01138      MOVE 'FS-BS-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTWEEKN
01139      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTWEEKN
01140      EJECT                                                        ELTWEEKN
01141                                                                   ELTWEEKN
01142                                                                   ELTWEEKN
01143 ************************************************************      ELTWEEKN
01144 *                                                          *      ELTWEEKN
01145 *        TRANSLATE BS OPEX OVERRIDE                        *      ELTWEEKN
01146 *                                                          *      ELTWEEKN
01147 ************************************************************      ELTWEEKN
01148  TRANSLATE-BS-OPEX-OVERRIDE.                                      ELTWEEKN
01149      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
01150      SET BLANK-LINE-NEEDED TO TRUE.                               ELTWEEKN
01151      SET PERIOD-NEEDED TO TRUE.                                   ELTWEEKN
01152      MOVE GSS-FS-BS-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTWEEKN
01153          CMF-CODE-VALUE.                                          ELTWEEKN
01154      MOVE 'FS-BS-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTWEEKN
01155      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTWEEKN
01156      EJECT                                                        ELTWEEKN
01157                                                                   ELTWEEKN
01158                                                                   ELTWEEKN
01159 ************************************************************      ELTWEEKN
01160 *                                                          *      ELTWEEKN
01161 *        CREATE SUPPLEMENTAL SCREEN                        *      ELTWEEKN
01162 *                                                          *      ELTWEEKN
01163 ************************************************************      ELTWEEKN
01164  CREATE-SUPPLEMENTAL-SCREEN.                                      ELTWEEKN
01165      MOVE WS-SUPP TO WS-HDR-LINE-BCBSMM.                          ELTWEEKN
01166      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTWEEKN
01167      IF GSS-FS-MM-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTWEEKN
01168          ZEROES                                                   ELTWEEKN
01169                 AND LOW-VALUES                                    ELTWEEKN
01170          PERFORM GENERATE-SUPPLEMENTAL-TEXT                       ELTWEEKN
01171      ELSE                                                         ELTWEEKN
01172          PERFORM GENERATE-SUPPLEMENTAL-NOT-APPL.                  ELTWEEKN
01173      EJECT                                                        ELTWEEKN
01174                                                                   ELTWEEKN
01175                                                                   ELTWEEKN
01176 ************************************************************      ELTWEEKN
01177 *                                                          *      ELTWEEKN
01178 *        GENERATE SUPPLEMENTAL TEXT                        *      ELTWEEKN
01179 *                                                          *      ELTWEEKN
01180 ************************************************************      ELTWEEKN
01181  GENERATE-SUPPLEMENTAL-TEXT.                                      ELTWEEKN
01182      PERFORM EJECT-NEW-PAGE.                                      ELTWEEKN
01183      PERFORM TRANSLATE-PARTICIPATION-INDICA.                      ELTWEEKN
01184      IF GSS-FS-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTWEEKN
01185          SPACES                                                   ELTWEEKN
01186                 AND ZEROES AND LOW-VALUES                         ELTWEEKN
01187          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTWEEKN
01188      PERFORM TRANSLATE-MM-IND.                                    ELTWEEKN
01189      IF GSS-FS-MM-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL SPACES   ELTWEEKN
01190          AND                                                      ELTWEEKN
01191                 ZEROES AND LOW-VALUES                             ELTWEEKN
01192          PERFORM TRANSLATE-MM-ALT-PRIC.                           ELTWEEKN
01193      SET SRP-ACCUM-PROV-CLASS-SUPP TO TRUE.                       ELTWEEKN
01194      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTWEEKN
01195      IF GSS-FS-MM-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES        ELTWEEKN
01196          AND                                                      ELTWEEKN
01197                 ZEROES AND LOW-VALUES                             ELTWEEKN
01198          PERFORM TRANSLATE-MM-CALC-METHOD.                        ELTWEEKN
01199      IF GSS-FS-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTWEEKN
01200          SPACES                                                   ELTWEEKN
01201                 AND ZEROES AND LOW-VALUES                         ELTWEEKN
01202          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTWEEKN
01203      IF ((GSS-FS-MM-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL         ELTWEEKN
01204          SPACES AND                                               ELTWEEKN
01205                   ZEROES AND LOW-VALUES)) OR                      ELTWEEKN
01206                 ((GSS-FS-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT       ELTWEEKN
01207          EQUAL SPACES AND                                         ELTWEEKN
01208                   ZEROES AND LOW-VALUES)) OR                      ELTWEEKN
01209                ((GSS-FS-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT      ELTWEEKN
01210          EQUAL SPACES AND                                         ELTWEEKN
01211                   ZEROES AND LOW-VALUES))                         ELTWEEKN
01212          PERFORM GENERATE-MM-BENE-REDUCT-TEXT.                    ELTWEEKN
01213      PERFORM GENERATE-RELATED-SERVICES-SENT.                      ELTWEEKN
01214      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTWEEKN
01215                                                                   ELTWEEKN
01216                                                                   ELTWEEKN
01217 ************************************************************      ELTWEEKN
01218 *                                                          *      ELTWEEKN
01219 *        GENERATE SUPPLEMENTAL NOT APPLICABLE              *      ELTWEEKN
01220 *                                                          *      ELTWEEKN
01221 ************************************************************      ELTWEEKN
01222  GENERATE-SUPPLEMENTAL-NOT-APPL.                                  ELTWEEKN
01223      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTWEEKN
01224      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTWEEKN
01225      MOVE WS-NOT-APPLY-TO-LOB-MM TO COF-DTL-LINE                  ELTWEEKN
01226          (COF-NBR-DTL-LINES).                                     ELTWEEKN
01227      PERFORM EJECT-NEW-PAGE.                                      ELTWEEKN
01228      EJECT                                                        ELTWEEKN
01229                                                                   ELTWEEKN
01230                                                                   ELTWEEKN
01231 ************************************************************      ELTWEEKN
01232 *                                                          *      ELTWEEKN
01233 *        TRANSLATE MM IND                                  *      ELTWEEKN
01234 *                                                          *      ELTWEEKN
01235 ************************************************************      ELTWEEKN
01236  TRANSLATE-MM-IND.                                                ELTWEEKN
01237      INITIALIZE TCAR-FROM-AREA.                                   ELTWEEKN
01238      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
01239      SET BLANK-LINE-NEEDED TO TRUE.                               ELTWEEKN
01240      SET PERIOD-NEEDED TO TRUE.                                   ELTWEEKN
01241      MOVE GSS-FS-MM-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTWEEKN
01242      MOVE   'FS-MM-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTWEEKN
01243      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTWEEKN
01244      EJECT                                                        ELTWEEKN
01245                                                                   ELTWEEKN
01246                                                                   ELTWEEKN
01247 ************************************************************      ELTWEEKN
01248 *                                                          *      ELTWEEKN
01249 *        TRANSLATE MM CALC METHOD                          *      ELTWEEKN
01250 *                                                          *      ELTWEEKN
01251 ************************************************************      ELTWEEKN
01252  TRANSLATE-MM-CALC-METHOD.                                        ELTWEEKN
01253      INITIALIZE TCAR-FROM-AREA.                                   ELTWEEKN
01254      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
01255      SET BLANK-LINE-NEEDED TO TRUE.                               ELTWEEKN
01256      SET PERIOD-NEEDED TO TRUE.                                   ELTWEEKN
01257      MOVE GSS-FS-MM-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTWEEKN
01258      MOVE   'FS-MM-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTWEEKN
01259      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTWEEKN
01260      EJECT                                                        ELTWEEKN
01261                                                                   ELTWEEKN
01262                                                                   ELTWEEKN
01263 ************************************************************      ELTWEEKN
01264 *                                                          *      ELTWEEKN
01265 *        TRANSLATE MM ALT PRIC                             *      ELTWEEKN
01266 *                                                          *      ELTWEEKN
01267 ************************************************************      ELTWEEKN
01268  TRANSLATE-MM-ALT-PRIC.                                           ELTWEEKN
01269      INITIALIZE TCAR-FROM-AREA.                                   ELTWEEKN
01270      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
01271      MOVE WS-ALT-PRICING-LINE-MM TO TCAR-FROM-LINE                ELTWEEKN
01272          (TCAR-FROM-SUB).                                         ELTWEEKN
01273      ADD  +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
01274      SET BLANK-LINE-NEEDED TO TRUE.                               ELTWEEKN
01275      SET PERIOD-NEEDED TO TRUE.                                   ELTWEEKN
01276      MOVE GSS-FS-MM-ALT-PRICING-METH (GSS-INDEX) TO               ELTWEEKN
01277          CMF-CODE-VALUE.                                          ELTWEEKN
01278      MOVE  'FS-MM-ALT-PRICING-METH' TO CMF-ELEMENT-SYSTEM-NAME.   ELTWEEKN
01279      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTWEEKN
01280      EJECT                                                        ELTWEEKN
01281                                                                   ELTWEEKN
01282                                                                   ELTWEEKN
01283 ************************************************************      ELTWEEKN
01284 *                                                          *      ELTWEEKN
01285 *        GENERATE MM BENE REDUCT TEXT                      *      ELTWEEKN
01286 *                                                          *      ELTWEEKN
01287 ************************************************************      ELTWEEKN
01288  GENERATE-MM-BENE-REDUCT-TEXT.                                    ELTWEEKN
01289      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTWEEKN
01290      MOVE WS-BENE-REDUCT-LINE TO COF-DTL-LINE                     ELTWEEKN
01291          (COF-NBR-DTL-LINES).                                     ELTWEEKN
01292      PERFORM LINK-TO-OUTPUT.                                      ELTWEEKN
01293      INITIALIZE ADDITIONAL-TEXT-SWITCH.                           ELTWEEKN
01294      INITIALIZE WS-PERIOD-SWITCH.                                 ELTWEEKN
01295      INITIALIZE TCAR-FROM-AREA.                                   ELTWEEKN
01296      IF GSS-FS-MM-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTWEEKN
01297          SPACES                                                   ELTWEEKN
01298                 AND ZEROES AND LOW-VALUES                         ELTWEEKN
01299          PERFORM TRANSLATE-MM-DEDUCT-IND.                         ELTWEEKN
01300      IF GSS-FS-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTWEEKN
01301          SPACES                                                   ELTWEEKN
01302                 AND ZEROES AND LOW-VALUES                         ELTWEEKN
01303          PERFORM TRANSLATE-MM-OPEX-APPLIC.                        ELTWEEKN
01304      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTWEEKN
01305      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTWEEKN
01306      PERFORM LINK-TO-OUTPUT.                                      ELTWEEKN
01307      IF GSS-FS-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTWEEKN
01308          SPACES                                                   ELTWEEKN
01309                 AND ZEROES AND LOW-VALUES                         ELTWEEKN
01310          PERFORM TRANSLATE-MM-OPEX-OVERRIDE.                      ELTWEEKN
01311      EJECT                                                        ELTWEEKN
01312                                                                   ELTWEEKN
01313                                                                   ELTWEEKN
01314 ************************************************************      ELTWEEKN
01315 *                                                          *      ELTWEEKN
01316 *        TRANSLATE MM DEDUCT IND                           *      ELTWEEKN
01317 *                                                          *      ELTWEEKN
01318 ************************************************************      ELTWEEKN
01319  TRANSLATE-MM-DEDUCT-IND.                                         ELTWEEKN
01320      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
01321      MOVE GSS-FS-MM-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTWEEKN
01322          CMF-CODE-VALUE.                                          ELTWEEKN
01323      MOVE 'FS-MM-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTWEEKN
01324      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTWEEKN
01325      EJECT                                                        ELTWEEKN
01326                                                                   ELTWEEKN
01327                                                                   ELTWEEKN
01328 ************************************************************      ELTWEEKN
01329 *                                                          *      ELTWEEKN
01330 *        TRANSLATE MM OPEX APPLIC                          *      ELTWEEKN
01331 *                                                          *      ELTWEEKN
01332 ************************************************************      ELTWEEKN
01333  TRANSLATE-MM-OPEX-APPLIC.                                        ELTWEEKN
01334      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
01335      MOVE GSS-FS-MM-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTWEEKN
01336          CMF-CODE-VALUE.                                          ELTWEEKN
01337      MOVE 'FS-MM-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTWEEKN
01338      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTWEEKN
01339      EJECT                                                        ELTWEEKN
01340                                                                   ELTWEEKN
01341                                                                   ELTWEEKN
01342 ************************************************************      ELTWEEKN
01343 *                                                          *      ELTWEEKN
01344 *        TRANSLATE MM OPEX OVERRIDE                        *      ELTWEEKN
01345 *                                                          *      ELTWEEKN
01346 ************************************************************      ELTWEEKN
01347  TRANSLATE-MM-OPEX-OVERRIDE.                                      ELTWEEKN
01348      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
01349      SET BLANK-LINE-NEEDED TO TRUE.                               ELTWEEKN
01350      SET PERIOD-NEEDED TO TRUE.                                   ELTWEEKN
01351      MOVE GSS-FS-MM-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTWEEKN
01352          CMF-CODE-VALUE.                                          ELTWEEKN
01353      MOVE 'FS-MM-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTWEEKN
01354      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTWEEKN
01355      EJECT                                                        ELTWEEKN
01356                                                                   ELTWEEKN
01357                                                                   ELTWEEKN
01358 ************************************************************      ELTWEEKN
01359 *                                                          *      ELTWEEKN
01360 *        GENERATE SPILL OVER TEXT                          *      ELTWEEKN
01361 *                                                          *      ELTWEEKN
01362 ************************************************************      ELTWEEKN
01363  GENERATE-SPILL-OVER-TEXT.                                        ELTWEEKN
01364      INITIALIZE TCAR-FROM-AREA.                                   ELTWEEKN
01365      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
01366      MOVE WS-SPILL-OVER-LINE TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELTWEEKN
01367      ADD  +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
01368      SET BLANK-LINE-NEEDED TO TRUE.                               ELTWEEKN
01369      SET PERIOD-NEEDED TO TRUE.                                   ELTWEEKN
01370      MOVE GSS-FS-SPILL-OVER-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTWEEKN
01371      MOVE 'FS-SPILL-OVER-IND' TO CMF-ELEMENT-SYSTEM-NAME.         ELTWEEKN
01372      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTWEEKN
01373      EJECT                                                        ELTWEEKN
01374                                                                   ELTWEEKN
01375                                                                   ELTWEEKN
01376 ************************************************************      ELTWEEKN
01377 *                                                          *      ELTWEEKN
01378 *        GENERATE RELATED SERVICES SENTENCE                *      ELTWEEKN
01379 *                                                          *      ELTWEEKN
01380 ************************************************************      ELTWEEKN
01381  GENERATE-RELATED-SERVICES-SENT.                                  ELTWEEKN
01382      SET GCG-INDEX TO +1.                                         ELTWEEKN
01383      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTWEEKN
01384         AT END                                                    ELTWEEKN
01385              MOVE ZEROES TO WS-GFSB-PROV-SLOT-NO                  ELTWEEKN
01386         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GFSB                     ELTWEEKN
01387              MOVE GCG-TAB-ID (GCG-INDEX)                          ELTWEEKN
01388                  TO WS-GFSB-PROV-ID                               ELTWEEKN
01389              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTWEEKN
01390                  TO WS-GFSB-PROV-SLOT-NO                          ELTWEEKN
01391         END-SEARCH.                                               ELTWEEKN
01392      IF WS-GFSB-PROV-ID EQUAL WS-GFSB                             ELTWEEKN
01393                 AND WS-GFSB-PROV-SLOT-NO NOT EQUAL                ELTWEEKN
01394          ZEROES                                                   ELTWEEKN
01395          PERFORM DISPLAY-RELATED-SERVICES-SENTE.                  ELTWEEKN
01396                                                                   ELTWEEKN
01397                                                                   ELTWEEKN
01398 ************************************************************      ELTWEEKN
01399 *                                                          *      ELTWEEKN
01400 *        DISPLAY RELATED SERVICES SENTENCE                 *      ELTWEEKN
01401 *                                                          *      ELTWEEKN
01402 ************************************************************      ELTWEEKN
01403  DISPLAY-RELATED-SERVICES-SENTE.                                  ELTWEEKN
01404      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTWEEKN
01405      MOVE WS-SPEC-SERV-MSG  TO COF-DTL-LINE                       ELTWEEKN
01406          (COF-NBR-DTL-LINES).                                     ELTWEEKN
01407      PERFORM LINK-TO-OUTPUT.                                      ELTWEEKN
01408      PERFORM GENERATE-RELATED-SERVICES-TEXT.                      ELTWEEKN
01409      EJECT                                                        ELTWEEKN
01410                                                                   ELTWEEKN
01411                                                                   ELTWEEKN
01412 ************************************************************      ELTWEEKN
01413 *                                                          *      ELTWEEKN
01414 *        SEARCH THE GSS ENTRY                              *      ELTWEEKN
01415 *                                                          *      ELTWEEKN
01416 ************************************************************      ELTWEEKN
01417  SEARCH-THE-GSS-ENTRY.                                            ELTWEEKN
01418      SET GSS-INDEX TO 1.                                          ELTWEEKN
01419      SEARCH GSS-ENTRY                                             ELTWEEKN
01420          AT END                                                   ELTWEEKN
01421               SET TABULAR-IS-UNDEFINED TO TRUE                    ELTWEEKN
01422          WHEN GSS-FS-PROG-CODE-CHR (GSS-INDEX)                    ELTWEEKN
01423               CONTINUE                                            ELTWEEKN
01424         END-SEARCH.                                               ELTWEEKN
01425      IF TABULAR-IS-UNDEFINED                                      ELTWEEKN
01426          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTWEEKN
01427                                                                   ELTWEEKN
01428                                                                   ELTWEEKN
01429 ************************************************************      ELTWEEKN
01430 *                                                          *      ELTWEEKN
01431 *        CALL GROUP CODES MANUAL INTERFACE                 *      ELTWEEKN
01432 *                                                          *      ELTWEEKN
01433 ************************************************************      ELTWEEKN
01434  CALL-GROUP-CODES-MANUAL-INTERF.                                  ELTWEEKN
01435      MOVE WS-GROUP TO CMF-RECORD-PREFIX.                          ELTWEEKN
01436      EXEC CICS LINK                                               ELTWEEKN
01437                PROGRAM ('ELUCMIF')                                ELTWEEKN
01438                COMMAREA (DFHCOMMAREA)                             ELTWEEKN
01439        END-EXEC.                                                  ELTWEEKN
01440                                                                   ELTWEEKN
01441                                                                   ELTWEEKN
01442 ************************************************************      ELTWEEKN
01443 *                                                          *      ELTWEEKN
01444 *        CALL CODES MANUAL INTERFACE                       *      ELTWEEKN
01445 *                                                          *      ELTWEEKN
01446 ************************************************************      ELTWEEKN
01447  CALL-CODES-MANUAL-INTERFACE.                                     ELTWEEKN
01448      MOVE WS-GCCP TO CMF-RECORD-PREFIX.                           ELTWEEKN
01449      EXEC CICS LINK                                               ELTWEEKN
01450                PROGRAM ('ELUCMIF')                                ELTWEEKN
01451                COMMAREA (DFHCOMMAREA)                             ELTWEEKN
01452        END-EXEC.                                                  ELTWEEKN
01453      EJECT                                                        ELTWEEKN
01454                                                                   ELTWEEKN
01455                                                                   ELTWEEKN
01456 ************************************************************      ELTWEEKN
01457 *                                                          *      ELTWEEKN
01458 *        GET GCCP TABULAR                                  *      ELTWEEKN
01459 *                                                          *      ELTWEEKN
01460 ************************************************************      ELTWEEKN
01461  GET-GCCP-TABULAR.                                                ELTWEEKN
01462      PERFORM ESTABLISH-ADDRESSABILITY-OF-GC.                      ELTWEEKN
01463      SET IOP-RD              TO TRUE.                             ELTWEEKN
01464      SET IOP-STG-MODE-MOVE   TO TRUE.                             ELTWEEKN
01465      SET IOP-FCQ-NONE        TO TRUE.                             ELTWEEKN
01466      SET IOP-KVQ-EQ          TO TRUE.                             ELTWEEKN
01467      MOVE KWA-GCTABULR-KEY   TO IOP-FILE-KEY.                     ELTWEEKN
01468      PERFORM CALL-INPUT-OUTPUT-MODULE.                            ELTWEEKN
01469      IF IOP-RC-OK                                                 ELTWEEKN
01470          PERFORM ESTABLISH-ADDRESS-OF-GCCP-TABU                   ELTWEEKN
01471      ELSE IF IOP-RC-NOTFND                                        ELTWEEKN
01472          PERFORM SIGNAL-NOT-FOUND-GCTAB-ERROR                     ELTWEEKN
01473      ELSE                                                         ELTWEEKN
01474          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTWEEKN
01475      EJECT                                                        ELTWEEKN
01476                                                                   ELTWEEKN
01477                                                                   ELTWEEKN
01478 ************************************************************      ELTWEEKN
01479 *                                                          *      ELTWEEKN
01480 *        ESTABLISH ADDRESSABILITY OF GCTABULAR IO PARAMETER*      ELTWEEKN
01481 *                                                          *      ELTWEEKN
01482 ************************************************************      ELTWEEKN
01483  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTWEEKN
01484      SET CIA-GCTABULR-DDN TO TRUE.                                ELTWEEKN
01485      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWEEKN
01486                            ADDRESS OF                             ELTWEEKN
01487          IOP-INPUT-OUTPUT-PARAMETERS.                             ELTWEEKN
01488      EJECT                                                        ELTWEEKN
01489                                                                   ELTWEEKN
01490                                                                   ELTWEEKN
01491 ************************************************************      ELTWEEKN
01492 *                                                          *      ELTWEEKN
01493 *        CALL INPUT OUTPUT MODULE                          *      ELTWEEKN
01494 *                                                          *      ELTWEEKN
01495 ************************************************************      ELTWEEKN
01496  CALL-INPUT-OUTPUT-MODULE.                                        ELTWEEKN
01497      EXEC CICS LINK                                               ELTWEEKN
01498                PROGRAM ('ELUIOPGM')                               ELTWEEKN
01499                COMMAREA (DFHCOMMAREA)                             ELTWEEKN
01500        END-EXEC.                                                  ELTWEEKN
01501      EJECT                                                        ELTWEEKN
01502                                                                   ELTWEEKN
01503                                                                   ELTWEEKN
01504 ************************************************************      ELTWEEKN
01505 *                                                          *      ELTWEEKN
01506 *        ESTABLISH ADDRESS OF GCCP TABULAR                 *      ELTWEEKN
01507 *                                                          *      ELTWEEKN
01508 ************************************************************      ELTWEEKN
01509  ESTABLISH-ADDRESS-OF-GCCP-TABU.                                  ELTWEEKN
01510      SET ADDRESS OF GCCP-TABULAR-REC-AREA TO                      ELTWEEKN
01511          IOP-REC-PTR.                                             ELTWEEKN
01512      SET IOP-REC-PTR TO NULL.                                     ELTWEEKN
01513      EJECT                                                        ELTWEEKN
01514                                                                   ELTWEEKN
01515                                                                   ELTWEEKN
01516 ************************************************************      ELTWEEKN
01517 *                                                          *      ELTWEEKN
01518 *        SIGNAL NOT FOUND GCTAB ERROR                      *      ELTWEEKN
01519 *                                                          *      ELTWEEKN
01520 ************************************************************      ELTWEEKN
01521  SIGNAL-NOT-FOUND-GCTAB-ERROR.                                    ELTWEEKN
01522      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTWEEKN
01523      PERFORM SIGNAL-ABEND.                                        ELTWEEKN
01524      EJECT                                                        ELTWEEKN
01525                                                                   ELTWEEKN
01526                                                                   ELTWEEKN
01527 ************************************************************      ELTWEEKN
01528 *                                                          *      ELTWEEKN
01529 *        SIGNAL CRITICAL IO ERROR                          *      ELTWEEKN
01530 *                                                          *      ELTWEEKN
01531 ************************************************************      ELTWEEKN
01532  SIGNAL-CRITICAL-IO-ERROR.                                        ELTWEEKN
01533      SET CIA-AB-CRITIO TO TRUE.                                   ELTWEEKN
01534      PERFORM SIGNAL-ABEND.                                        ELTWEEKN
01535      EJECT                                                        ELTWEEKN
01536                                                                   ELTWEEKN
01537                                                                   ELTWEEKN
01538 ************************************************************      ELTWEEKN
01539 *                                                          *      ELTWEEKN
01540 *        GENERATE RELATED SERVICES TEXT                    *      ELTWEEKN
01541 *                                                          *      ELTWEEKN
01542 ************************************************************      ELTWEEKN
01543  GENERATE-RELATED-SERVICES-TEXT.                                  ELTWEEKN
01544      MOVE 'WEEKEND ADMISSION'   TO SRP-CCP-NAME.                  ELTWEEKN
01545      MOVE  WS-GFSB-PROV-ID      TO SRP-TABULAR-ID.                ELTWEEKN
01546      MOVE  WS-GFSB-PROV-SLOT-NO TO SRP-TABULAR-SLOT-NO.           ELTWEEKN
01547      PERFORM CALL-RELATED-SERVICES-GENERATO.                      ELTWEEKN
01548                                                                   ELTWEEKN
01549                                                                   ELTWEEKN
01550 ************************************************************      ELTWEEKN
01551 *                                                          *      ELTWEEKN
01552 *        CALL RELATED SERVICES GENERATOR                   *      ELTWEEKN
01553 *                                                          *      ELTWEEKN
01554 ************************************************************      ELTWEEKN
01555  CALL-RELATED-SERVICES-GENERATO.                                  ELTWEEKN
01556      EXEC CICS LINK                                               ELTWEEKN
01557                PROGRAM ('ELGGXXB')                                ELTWEEKN
01558                COMMAREA (DFHCOMMAREA)                             ELTWEEKN
01559         END-EXEC.                                                 ELTWEEKN
01560                                                                   ELTWEEKN
01561                                                                   ELTWEEKN
01562 ************************************************************      ELTWEEKN
01563 *                                                          *      ELTWEEKN
01564 *        GENERATE COINSURANCE                              *      ELTWEEKN
01565 *                                                          *      ELTWEEKN
01566 ************************************************************      ELTWEEKN
01567  GENERATE-COINSURANCE.                                            ELTWEEKN
01568      EXEC CICS LINK                                               ELTWEEKN
01569                PROGRAM ('ELGACLCC')                               ELTWEEKN
01570                COMMAREA (DFHCOMMAREA)                             ELTWEEKN
01571         END-EXEC.                                                 ELTWEEKN
01572                                                                   ELTWEEKN
01573                                                                   ELTWEEKN
01574 ************************************************************      ELTWEEKN
01575 *                                                          *      ELTWEEKN
01576 *        GENERATE COPAY                                    *      ELTWEEKN
01577 *                                                          *      ELTWEEKN
01578 ************************************************************      ELTWEEKN
01579  GENERATE-COPAY.                                                  ELTWEEKN
01580      EXEC CICS LINK                                               ELTWEEKN
01581                PROGRAM ('ELGACPCC')                               ELTWEEKN
01582                COMMAREA (DFHCOMMAREA)                             ELTWEEKN
01583         END-EXEC.                                                 ELTWEEKN
01584                                                                   ELTWEEKN
01585                                                                   ELTWEEKN
01586 ************************************************************      ELTWEEKN
01587 *                                                          *      ELTWEEKN
01588 *        GENERATE DEDUCTIBLE                               *      ELTWEEKN
01589 *                                                          *      ELTWEEKN
01590 ************************************************************      ELTWEEKN
01591  GENERATE-DEDUCTIBLE.                                             ELTWEEKN
01592      EXEC CICS LINK                                               ELTWEEKN
01593                PROGRAM ('ELGACLCC')                               ELTWEEKN
01594                COMMAREA (DFHCOMMAREA)                             ELTWEEKN
01595         END-EXEC.                                                 ELTWEEKN
01596                                                                   ELTWEEKN
01597                                                                   ELTWEEKN
01598 ************************************************************      ELTWEEKN
01599 *                                                          *      ELTWEEKN
01600 *        GENERATE MAXIMUM                                  *      ELTWEEKN
01601 *                                                          *      ELTWEEKN
01602 ************************************************************      ELTWEEKN
01603  GENERATE-MAXIMUM.                                                ELTWEEKN
01604      EXEC CICS LINK                                               ELTWEEKN
01605                PROGRAM ('ELGABMCC')                               ELTWEEKN
01606                COMMAREA (DFHCOMMAREA)                             ELTWEEKN
01607         END-EXEC.                                                 ELTWEEKN
01608                                                                   ELTWEEKN
01609                                                                   ELTWEEKN
01610 ************************************************************      ELTWEEKN
01611 *                                                          *      ELTWEEKN
01612 *        CALL CBRI INTERFACE                               *      ELTWEEKN
01613 *                                                          *      ELTWEEKN
01614 ************************************************************      ELTWEEKN
01615  CALL-CBRI-INTERFACE.                                             ELTWEEKN
01616      CALL 'ELGCBRI' USING DFHCOMMAREA                             ELTWEEKN
01617                           DFHEIBLK.                               ELTWEEKN
01618                                                                   ELTWEEKN
01619                                                                   ELTWEEKN
01620 ************************************************************      ELTWEEKN
01621 *                                                          *      ELTWEEKN
01622 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTWEEKN
01623 *                                                          *      ELTWEEKN
01624 ************************************************************      ELTWEEKN
01625  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTWEEKN
01626      PERFORM INITIALIZE-CMOUT.                                    ELTWEEKN
01627      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTWEEKN
01628      EJECT                                                        ELTWEEKN
01629                                                                   ELTWEEKN
01630                                                                   ELTWEEKN
01631 ************************************************************      ELTWEEKN
01632 *                                                          *      ELTWEEKN
01633 *        PREPARE TEXT FOR OUTPUT                           *      ELTWEEKN
01634 *                                                          *      ELTWEEKN
01635 ************************************************************      ELTWEEKN
01636  PREPARE-TEXT-FOR-OUTPUT.                                         ELTWEEKN
01637      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTWEEKN
01638          UNTIL CMF-DESCR-IDX                                      ELTWEEKN
01639                                    GREATER THAN                   ELTWEEKN
01640              CMF-NBR-DESCR-LINES.                                 ELTWEEKN
01641      EJECT                                                        ELTWEEKN
01642                                                                   ELTWEEKN
01643                                                                   ELTWEEKN
01644 ************************************************************      ELTWEEKN
01645 *                                                          *      ELTWEEKN
01646 *        INITIALIZE CMOUT                                  *      ELTWEEKN
01647 *                                                          *      ELTWEEKN
01648 ************************************************************      ELTWEEKN
01649  INITIALIZE-CMOUT.                                                ELTWEEKN
01650      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTWEEKN
01651      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWEEKN
01652          ADDRESS OF CMF-DESCR.                                    ELTWEEKN
01653      SET CMF-DESCR-IDX TO 1.                                      ELTWEEKN
01654      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTWEEKN
01655                                                                   ELTWEEKN
01656                                                                   ELTWEEKN
01657 ************************************************************      ELTWEEKN
01658 *                                                          *      ELTWEEKN
01659 *        MOVE CMF TEXT TO OUTPUT                           *      ELTWEEKN
01660 *                                                          *      ELTWEEKN
01661 ************************************************************      ELTWEEKN
01662  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTWEEKN
01663      PERFORM MOVE-A-LINE.                                         ELTWEEKN
01664      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTWEEKN
01665          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTWEEKN
01666      IF TCAR-FROM-SUB GREATER THAN 20                             ELTWEEKN
01667               OR CMF-DESCR-IDX GREATER THAN                       ELTWEEKN
01668          CMF-NBR-DESCR-LINES                                      ELTWEEKN
01669          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTWEEKN
01670                                                                   ELTWEEKN
01671                                                                   ELTWEEKN
01672 ************************************************************      ELTWEEKN
01673 *                                                          *      ELTWEEKN
01674 *        FINISH CODES MANUAL TEXT                          *      ELTWEEKN
01675 *                                                          *      ELTWEEKN
01676 ************************************************************      ELTWEEKN
01677  FINISH-CODES-MANUAL-TEXT.                                        ELTWEEKN
01678      SET DONE-PROCESSING TO TRUE.                                 ELTWEEKN
01679      IF PERIOD-NEEDED                                             ELTWEEKN
01680          PERFORM GET-AND-MOVE-PERIOD.                             ELTWEEKN
01681      EJECT                                                        ELTWEEKN
01682                                                                   ELTWEEKN
01683                                                                   ELTWEEKN
01684 ************************************************************      ELTWEEKN
01685 *                                                          *      ELTWEEKN
01686 *        GET AND MOVE PERIOD                               *      ELTWEEKN
01687 *                                                          *      ELTWEEKN
01688 ************************************************************      ELTWEEKN
01689  GET-AND-MOVE-PERIOD.                                             ELTWEEKN
01690      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTWEEKN
01691          (TCAR-FROM-SUB).                                         ELTWEEKN
01692                                                                   ELTWEEKN
01693                                                                   ELTWEEKN
01694 ************************************************************      ELTWEEKN
01695 *                                                          *      ELTWEEKN
01696 *        SAVE LAST LINE                                    *      ELTWEEKN
01697 *                                                          *      ELTWEEKN
01698 ************************************************************      ELTWEEKN
01699  SAVE-LAST-LINE.                                                  ELTWEEKN
01700      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
01701      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTWEEKN
01702         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTWEEKN
01703      ADD 1 TO TCAR-FROM-SUB.                                      ELTWEEKN
01704      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTWEEKN
01705                                                                   ELTWEEKN
01706                                                                   ELTWEEKN
01707 ************************************************************      ELTWEEKN
01708 *                                                          *      ELTWEEKN
01709 *        OUTPUT LAST LINE                                  *      ELTWEEKN
01710 *                                                          *      ELTWEEKN
01711 ************************************************************      ELTWEEKN
01712  OUTPUT-LAST-LINE.                                                ELTWEEKN
01713      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTWEEKN
01714          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTWEEKN
01715      IF BLANK-LINE-NEEDED                                         ELTWEEKN
01716          PERFORM CREATE-A-BLANK-LINE.                             ELTWEEKN
01717                                                                   ELTWEEKN
01718                                                                   ELTWEEKN
01719 ************************************************************      ELTWEEKN
01720 *                                                          *      ELTWEEKN
01721 *        CREATE A BLANK LINE                               *      ELTWEEKN
01722 *                                                          *      ELTWEEKN
01723 ************************************************************      ELTWEEKN
01724  CREATE-A-BLANK-LINE.                                             ELTWEEKN
01725      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTWEEKN
01726      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTWEEKN
01727                                                                   ELTWEEKN
01728                                                                   ELTWEEKN
01729 ************************************************************      ELTWEEKN
01730 *                                                          *      ELTWEEKN
01731 *        MOVE A LINE                                       *      ELTWEEKN
01732 *                                                          *      ELTWEEKN
01733 ************************************************************      ELTWEEKN
01734  MOVE-A-LINE.                                                     ELTWEEKN
01735      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTWEEKN
01736          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTWEEKN
01737      SET CMF-DESCR-IDX UP BY 1.                                   ELTWEEKN
01738      ADD 1 TO TCAR-FROM-SUB.                                      ELTWEEKN
01739      EJECT                                                        ELTWEEKN
01740                                                                   ELTWEEKN
01741                                                                   ELTWEEKN
01742 ************************************************************      ELTWEEKN
01743 *                                                          *      ELTWEEKN
01744 *        REFORMAT AND WRITE TEXT                           *      ELTWEEKN
01745 *                                                          *      ELTWEEKN
01746 ************************************************************      ELTWEEKN
01747  REFORMAT-AND-WRITE-TEXT.                                         ELTWEEKN
01748      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTWEEKN
01749      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTWEEKN
01750      PERFORM UNSTRING-TEXT.                                       ELTWEEKN
01751      MOVE +1 TO TCAR-FROM-SUB.                                    ELTWEEKN
01752      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTWEEKN
01753      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTWEEKN
01754          UNTIL COF-NBR-DTL-LINES GREATER                          ELTWEEKN
01755                                   TCAR-OUTPUT-FIELDS-USED -       ELTWEEKN
01756              1.                                                   ELTWEEKN
01757      PERFORM DISPOSE-OF-LAST-LINE.                                ELTWEEKN
01758      PERFORM LINK-TO-OUTPUT.                                      ELTWEEKN
01759                                                                   ELTWEEKN
01760                                                                   ELTWEEKN
01761 ************************************************************      ELTWEEKN
01762 *                                                          *      ELTWEEKN
01763 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTWEEKN
01764 *                                                          *      ELTWEEKN
01765 ************************************************************      ELTWEEKN
01766  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTWEEKN
01767      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTWEEKN
01768           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTWEEKN
01769      ADD +1 TO TCAR-FROM-SUB.                                     ELTWEEKN
01770      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTWEEKN
01771      EJECT                                                        ELTWEEKN
01772                                                                   ELTWEEKN
01773                                                                   ELTWEEKN
01774 ************************************************************      ELTWEEKN
01775 *                                                          *      ELTWEEKN
01776 *        UNSTRING TEXT                                     *      ELTWEEKN
01777 *                                                          *      ELTWEEKN
01778 ************************************************************      ELTWEEKN
01779  UNSTRING-TEXT.                                                   ELTWEEKN
01780      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTWEEKN
01781      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTWEEKN
01782      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTWEEKN
01783      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTWEEKN
01784      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTWEEKN
01785      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTWEEKN
01786      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTWEEKN
01787      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTWEEKN
01788      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTWEEKN
01789      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTWEEKN
01790      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTWEEKN
01791      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTWEEKN
01792      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTWEEKN
01793      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTWEEKN
01794      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTWEEKN
01795      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTWEEKN
01796      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTWEEKN
01797      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTWEEKN
01798      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTWEEKN
01799      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTWEEKN
01800      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTWEEKN
01801      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTWEEKN
01802                                                                   ELTWEEKN
01803                                                                   ELTWEEKN
01804 ************************************************************      ELTWEEKN
01805 *                                                          *      ELTWEEKN
01806 *        LINK TO OUTPUT                                    *      ELTWEEKN
01807 *                                                          *      ELTWEEKN
01808 ************************************************************      ELTWEEKN
01809  LINK-TO-OUTPUT.                                                  ELTWEEKN
01810      EXEC CICS LINK                                               ELTWEEKN
01811          PROGRAM ('ELUOUTPT')                                     ELTWEEKN
01812          COMMAREA (DFHCOMMAREA)                                   ELTWEEKN
01813          END-EXEC.                                                ELTWEEKN
01814      EJECT                                                        ELTWEEKN
01815                                                                   ELTWEEKN
01816                                                                   ELTWEEKN
01817 ************************************************************      ELTWEEKN
01818 *                                                          *      ELTWEEKN
01819 *        DISPOSE OF LAST LINE                              *      ELTWEEKN
01820 *                                                          *      ELTWEEKN
01821 ************************************************************      ELTWEEKN
01822  DISPOSE-OF-LAST-LINE.                                            ELTWEEKN
01823      IF NOT ADDITIONAL-TEXT                                       ELTWEEKN
01824          PERFORM INITIALIZE-CONTINUED-SW.                         ELTWEEKN
01825      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTWEEKN
01826          PERFORM SAVE-LAST-LINE                                   ELTWEEKN
01827      ELSE                                                         ELTWEEKN
01828          PERFORM OUTPUT-LAST-LINE.                                ELTWEEKN
01829                                                                   ELTWEEKN
01830                                                                   ELTWEEKN
01831 ************************************************************      ELTWEEKN
01832 *                                                          *      ELTWEEKN
01833 *        INITIALIZE CONTINUED SW                           *      ELTWEEKN
01834 *                                                          *      ELTWEEKN
01835 ************************************************************      ELTWEEKN
01836  INITIALIZE-CONTINUED-SW.                                         ELTWEEKN
01837      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTWEEKN
