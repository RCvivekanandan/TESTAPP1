00001 *      LAST MAINTENANCE TIME: 15.08.20  DATE: 06/27/91            06/29/02
00002  IDENTIFICATION DIVISION.                                         ELTMOPS 
00003                                                                      LV001
00004  PROGRAM-ID.         ELTMOPS.                                     ELTMOPS 
00005                                                                   ELTMOPS 
00006  AUTHOR.             RICK BARILEAU.                               ELTMOPS 
00007                                                                   ELTMOPS 
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTMOPS 
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELTMOPS 
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTMOPS 
00011                      233 N. MICHIGAN AVE                          ELTMOPS 
00012                      CHICAGO, ILLINOIS 60601                      ELTMOPS 
00013                                                                   ELTMOPS 
00014  DATE-WRITTEN.       18-JUN-1987.                                 ELTMOPS 
00015                                                                   ELTMOPS 
00016  DATE-COMPILED.                                                   ELTMOPS 
00017                                                                   ELTMOPS 
00018  SECURITY.           COPYRIGHT 1986,                              ELTMOPS 
00019                      HEALTH CARE SERVICE CORPORATION              ELTMOPS 
00020      SKIP3                                                        ELTMOPS 
00021  ENVIRONMENT DIVISION.                                            ELTMOPS 
00022                                                                   ELTMOPS 
00023  CONFIGURATION SECTION.                                           ELTMOPS 
00024  SOURCE-COMPUTER.    IBM-3090.                                    ELTMOPS 
00025  OBJECT-COMPUTER.    IBM-3090.                                    ELTMOPS 
00026      EJECT                                                        ELTMOPS 
00027 ******************************************************************ELTMOPS 
00028 *                                                                *ELTMOPS 
00029 *    COPYBOOK:   ELTMOPS                                         *ELTMOPS 
00030 *    DATE:       18-JUN-1987                                     *ELTMOPS 
00031 *    AUTHOR:     RICK BARILEAU                                   *ELTMOPS 
00032 *    FUNCTION:   THIS WILL GENERATE ALL OUTPUT ASSOCIATED WITH   *ELTMOPS 
00033 *                THE MANDATORY OUTPATIENT SURGERY PROGRAM.       *ELTMOPS 
00034 *    NOTES:      X---                                            *ELTMOPS 
00035 *                                                                *ELTMOPS 
00036 ******************************************************************ELTMOPS 
00037 *                                                                *ELTMOPS 
00038 *                      MAINTENANCE HISTORY                       *ELTMOPS 
00039 *                                                                *ELTMOPS 
00040 *  MOD     DATE     BY  DRPT                ACTION               *ELTMOPS 
00041 * ----- ----------- --- ----- ---------------------------------- *ELTMOPS 
00042 * 01.00 18-JUN-1987 REB       CREATED                            *ELTMOPS 
00043 *                                                                *ELTMOPS 
00044 * 01.01 02-SEP-1987 REB       REARRANGE ORDER OF G-TABS UNDER THE*ELTMOPS 
00045 *                             SENTENCE NOW. THE SPILL OVER IND   *ELTMOPS 
00046 *                             TRANSLATION WILL BE BEFORE G-TABS. *ELTMOPS 
00047 *                                                                *ELTMOPS 
00048 * 01.02 05-NOV-1990 JPB       CHANGED STORAGE MANAGEMENT.        *ELTMOPS 
00049 *                                                                *ELTMOPS 
00050 * 01.03 09-NOV-1990 JPB       CHANGED REFERENCES TO GCG-MAND-OP  *ELTMOPS 
00051 *                             -SURG-PROG-IND TO ACCOMODATE       *ELTMOPS 
00052 *                             CHANGES IN FIELD SIZE.             *ELTMOPS 
00053 *                                                                *ELTMOPS 
00054 * 01.04 30-MAY-1991 JPB       ADDED TRANSLATION AND DISPLAY OF   *ELTMOPS 
00055 *                             PARTICIPATION INDICATOR.           *ELTMOPS 
00056 *                                                                *ELTMOPS 
00057 * 01.05 27-JUN-1991 JPB       MOVED VALUE 'N' TO APPROVAL-SOURCE *ELTMOPS 
00058 *                             SWITCH IN WORKING-STORAGE.         *ELTMOPS 
00059 *                                                                *ELTMOPS 
00060 ******************************************************************ELTMOPS 
00061                                                                   ELTMOPS 
00062  DATA DIVISION.                                                   ELTMOPS 
00063  WORKING-STORAGE SECTION.                                         ELTMOPS 
00064  01  WS-MISC.                                                     ELTMOPS 
00065      05  FILLER                   PIC X(27) VALUE                 ELTMOPS 
00066      '*** ELTMOPS BEGINS HERE ***'.                               ELTMOPS 
00067                                                                   ELTMOPS 
00068  01  WS-POINTERS.                                                 ELTMOPS 
00069      05  WS-POINTER2              POINTER.                        ELTMOPS 
00070      05  WS-POINTER3              POINTER.                        ELTMOPS 
00071                                                                   ELTMOPS 
00072  01  WS-SWITCHES.                                                 ELTMOPS 
00073      05  ADDITIONAL-TEXT-SWITCH   PIC X(01) VALUE SPACE.          ELTMOPS 
00074          88  ADDITIONAL-TEXT                VALUE 'A'.            ELTMOPS 
00075          88  BLANK-LINE-NEEDED              VALUE 'B'.            ELTMOPS 
00076      05  CONTINUED-PROCESSING-SW  PIC X(01) VALUE SPACE.          ELTMOPS 
00077          88  DONE-PROCESSING                VALUE 'D'.            ELTMOPS 
00078          88  PROCESSING-CMF-TEXT            VALUE 'P'.            ELTMOPS 
00079      05  WS-GMPB-SWITCH           PIC X(01) VALUE 'N'.            ELTMOPS 
00080          88  GMPB-IS-PRESENT                VALUE 'Y'.            ELTMOPS 
00081      05  WS-GMPR-SWITCH           PIC X(01) VALUE 'N'.            ELTMOPS 
00082          88  GMPR-IS-PRESENT                VALUE 'Y'.            ELTMOPS 
00083      05  WS-PERIOD-SWITCH         PIC X(01) VALUE 'N'.            ELTMOPS 
00084          88  PERIOD-NEEDED                  VALUE 'Y'.            ELTMOPS 
00085      05  WS-TABULAR-SWITCH        PIC X(01) VALUE 'N'.            ELTMOPS 
00086          88  TABULAR-IS-UNDEFINED           VALUE 'Y'.            ELTMOPS 
00087      05  WS-APPROVAL-SOURCE-SW    PIC X     VALUE 'N'.            ELTMOPS 
00088          88  NOT-HOLDING-APPROVAL-SRCE      VALUE 'N'.            ELTMOPS 
00089          88  HOLDING-APPROVAL-SOURCE        VALUE 'H'.            ELTMOPS 
00090                                                                   ELTMOPS 
00091  01  WS-HOLD-AREA.                                                ELTMOPS 
00092      05  WS-GMPB-PROV-ID               PIC X(06)  VALUE SPACES.   ELTMOPS 
00093      05  WS-GMPB-PROV-SLOT-NO   COMP-3 PIC S9(07) VALUE +0.       ELTMOPS 
00094      05  WS-GMPR-PROV-ID               PIC X(06)  VALUE SPACES.   ELTMOPS 
00095      05  WS-GMPR-PROV-SLOT-NO   COMP-3 PIC S9(07) VALUE +0.       ELTMOPS 
00096                                                                   ELTMOPS 
00097 **************************************************************    ELTMOPS 
00098 ***                  PROGRAM CONSTANTS                            ELTMOPS 
00099 **************************************************************    ELTMOPS 
00100      05  WS-GCCP                  PIC X(06) VALUE '#GCCP '.       ELTMOPS 
00101      05  WS-GMPB                  PIC X(06) VALUE '#GMPB '.       ELTMOPS 
00102      05  WS-GMPR                  PIC X(06) VALUE '#GMPR '.       ELTMOPS 
00103      05  WS-GROUP                 PIC X(06) VALUE 'GROUP '.       ELTMOPS 
00104      05  WS-INST                  PIC X(13) VALUE 'INSTITUTIONAL'.ELTMOPS 
00105      05  WS-PROF                  PIC X(13) VALUE 'PROFESSIONAL '.ELTMOPS 
00106      05  WS-SUPP                  PIC X(13) VALUE 'SUPPLEMENTAL '.ELTMOPS 
00107      05  WS-APPROVAL              PIC X(09) VALUE 'APPROVAL.'.    ELTMOPS 
00108      05  WS-INPATIENT             PIC X(10) VALUE 'INPATIENT;'.   ELTMOPS 
00109      05  WS-INPATIENT-END         PIC X(10) VALUE 'INPATIENT.'.   ELTMOPS 
00110      05  WS-OUTPATIENT            PIC X(11) VALUE 'OUTPATIENT.'.  ELTMOPS 
00111                                                                   ELTMOPS 
00112 **********************************************************        ELTMOPS 
00113 *** HEADER LINE                                                   ELTMOPS 
00114 **********************************************************        ELTMOPS 
00115      05  WS-HEADER-LINE.                                          ELTMOPS 
00116          10  FILLER                 PIC X(15) VALUE SPACES.       ELTMOPS 
00117          10  FILLER                 PIC X(37) VALUE               ELTMOPS 
00118          'MANDATORY OUTPATIENT SURGERY PROGRAM '.                 ELTMOPS 
00119          10  WS-HEADER-LINE-BCBSMM  PIC X(13) VALUE SPACES.       ELTMOPS 
00120          10  FILLER                 PIC X(14) VALUE SPACES.       ELTMOPS 
00121                                                                   ELTMOPS 
00122 **************************************************************    ELTMOPS 
00123 *** SCREEN BODY LINES                                             ELTMOPS 
00124 **************************************************************    ELTMOPS 
00125  01  WS-SCREEN-LINE-AREA.                                         ELTMOPS 
00126      05  WS-APPRVL-SRCE-LINE.                                     ELTMOPS 
00127          10  FILLER               PIC X(38) VALUE                 ELTMOPS 
00128          'MANDATORY OUTPATIENT SURGERY REQUIRES '.                ELTMOPS 
00129          10  FILLER               PIC X(41) VALUE SPACES.         ELTMOPS 
00130                                                                   ELTMOPS 
00131      05  WS-ALT-PRICING-LINE-BC.                                  ELTMOPS 
00132          10  FILLER               PIC X(26) VALUE                 ELTMOPS 
00133          'THE ALTERNATE PRICING FOR '.                            ELTMOPS 
00134          10  FILLER               PIC X(26) VALUE                 ELTMOPS 
00135          'INSTITUTIONAL SERVICES IS '.                            ELTMOPS 
00136          10  FILLER               PIC X(27) VALUE SPACES.         ELTMOPS 
00137                                                                   ELTMOPS 
00138      05  WS-ALT-PRICING-LINE-BS.                                  ELTMOPS 
00139          10  FILLER               PIC X(26) VALUE                 ELTMOPS 
00140          'THE ALTERNATE PRICING FOR '.                            ELTMOPS 
00141          10  FILLER               PIC X(25) VALUE                 ELTMOPS 
00142          'PROFESSIONAL SERVICES IS '.                             ELTMOPS 
00143          10  FILLER               PIC X(28) VALUE SPACES.         ELTMOPS 
00144                                                                   ELTMOPS 
00145      05  WS-ALT-PRICING-LINE-MM.                                  ELTMOPS 
00146          10  FILLER               PIC X(26) VALUE                 ELTMOPS 
00147          'THE ALTERNATE PRICING FOR '.                            ELTMOPS 
00148          10  FILLER               PIC X(25) VALUE                 ELTMOPS 
00149          'SUPPLEMENTAL SERVICES IS '.                             ELTMOPS 
00150          10  FILLER               PIC X(28) VALUE SPACES.         ELTMOPS 
00151                                                                   ELTMOPS 
00152      05  WS-BENE-REDUCT-LINE.                                     ELTMOPS 
00153          10  FILLER               PIC X(47) VALUE                 ELTMOPS 
00154          'DENIED OR REDUCED BENEFITS DUE TO THIS PROGRAM:'.       ELTMOPS 
00155          10  FILLER               PIC X(32) VALUE SPACES.         ELTMOPS 
00156                                                                   ELTMOPS 
00157      05  WS-SPILL-OVER-LINE.                                      ELTMOPS 
00158          10  FILLER               PIC X(79) VALUE                 ELTMOPS 
00159          'UNPAID SERVICES AFTER BASIC BENEFITS REDUCTION ARE '.   ELTMOPS 
00160                                                                   ELTMOPS 
00161 **************************************************************    ELTMOPS 
00162 ** SPECIAL MESSAGE FOR THE VOLUNTARY AND NOT APPLICABLE CASES     ELTMOPS 
00163 ** ALSO THE FIXED TEXT FOR TABULARS GMPB,GMPR                     ELTMOPS 
00164 **************************************************************    ELTMOPS 
00165      05  WS-NOT-APPLICABLE-MSG.                                   ELTMOPS 
00166          10  FILLER               PIC  X(55) VALUE                ELTMOPS 
00167        'MANDATORY OUTPATIENT SURGERY PROGRAM IS NOT APPLICABLE.'. ELTMOPS 
00168          10  FILLER               PIC  X(24) VALUE SPACES.        ELTMOPS 
00169                                                                   ELTMOPS 
00170      05  WS-VOLUNTARY-MSG.                                        ELTMOPS 
00171          10  FILLER               PIC  X(50) VALUE                ELTMOPS 
00172          'MANDATORY OUTPATIENT SURGERY PROGRAM IS VOLUNTARY.'.    ELTMOPS 
00173          10  FILLER               PIC  X(29) VALUE SPACES.        ELTMOPS 
00174                                                                   ELTMOPS 
00175      05  WS-SPEC-SERV-MSG.                                        ELTMOPS 
00176          10  FILLER               PIC X(79) VALUE                 ELTMOPS 
00177          'THERE ARE SPECIAL RELATED SERVICES INCLUDED IN THIS COSTELTMOPS 
00178 -        ' CONTAINMENT PROGRAM.'.                                 ELTMOPS 
00179                                                                   ELTMOPS 
00180      05  WS-SPEC-PROC-MSG.                                        ELTMOPS 
00181          10  FILLER               PIC X(79) VALUE                 ELTMOPS 
00182          'THERE ARE SPECIAL PROCEDURES INCLUDED IN THIS COST CONTAELTMOPS 
00183 -        'INMENT PROGRAM.'.                                       ELTMOPS 
00184                                                                   ELTMOPS 
00185      05  WS-PARTICIPATION-LINE.                                   ELTMOPS 
00186          10  FILLER               PIC X(79) VALUE                 ELTMOPS 
00187          'THE MANDATORY OUTPATIENT SURGERY PROGRAM APPLIES TO '.  ELTMOPS 
00188                                                                   ELTMOPS 
00189      05  WS-DISCLAIMER-MSG.                                       ELTMOPS 
00190          10  FILLER               PIC X(79) VALUE                 ELTMOPS 
00191          '*** SUBJECT TO OTHER CONTRACT LIMITATIONS ***'.         ELTMOPS 
00192                                                                   ELTMOPS 
00193      05  WS-NOT-APPLY-TO-LOB-BC.                                  ELTMOPS 
00194          10  FILLER               PIC X(56) VALUE                 ELTMOPS 
00195        'MANDATORY OUTPATIENT SURGERY PROGRAM DOES NOT APPLY FOR '.ELTMOPS 
00196          10  FILLER               PIC X(23) VALUE                 ELTMOPS 
00197        'INSTITUTIONAL BENEFITS.'.                                 ELTMOPS 
00198                                                                   ELTMOPS 
00199      05  WS-NOT-APPLY-TO-LOB-BS.                                  ELTMOPS 
00200          10  FILLER               PIC X(56) VALUE                 ELTMOPS 
00201        'MANDATORY OUTPATIENT SURGERY PROGRAM DOES NOT APPLY FOR '.ELTMOPS 
00202          10  FILLER               PIC X(23) VALUE                 ELTMOPS 
00203        'PROFESSIONAL BENEFITS.'.                                  ELTMOPS 
00204                                                                   ELTMOPS 
00205      05  WS-NOT-APPLY-TO-LOB-MM.                                  ELTMOPS 
00206          10  FILLER               PIC X(56) VALUE                 ELTMOPS 
00207        'MANDATORY OUTPATIENT SURGERY PROGRAM DOES NOT APPLY FOR '.ELTMOPS 
00208          10  FILLER               PIC X(23) VALUE                 ELTMOPS 
00209        'SUPPLEMENTAL BENEFITS.'.                                  ELTMOPS 
00210                                                                   ELTMOPS 
00211 /                                                                 ELTMOPS 
00212  LINKAGE SECTION.                                                 ELTMOPS 
00213  01  DFHCOMMAREA.                                                 ELTMOPS 
00214      COPY ELSCOMMC.                                               ELTMOPS 
00215 /                                                                 ELTMOPS 
00216      COPY ELSCIA2C.                                               ELTMOPS 
00217 /                                                                 ELTMOPS 
00218      COPY ELSCMDSC.                                               ELTMOPS 
00219 /                                                                 ELTMOPS 
00220      COPY ELSCMIFC.                                               ELTMOPS 
00221 /                                                                 ELTMOPS 
00222      COPY ELSIOPMC.                                               ELTMOPS 
00223 /                                                                 ELTMOPS 
00224      COPY ELSKEYSC.                                               ELTMOPS 
00225 /                                                                 ELTMOPS 
00226      COPY ELSOUTPC.                                               ELTMOPS 
00227 /                                                                 ELTMOPS 
00228      COPY ELSSRTPC.                                               ELTMOPS 
00229 /                                                                 ELTMOPS 
00230      COPY ELSTCWAC.                                               ELTMOPS 
00231 /                                                                 ELTMOPS 
00232      COPY ELSSSCBC.                                               ELTMOPS 
00233 /                                                                 ELTMOPS 
00234  01  GROUP-SPECIFIC-REC.                                          ELTMOPS 
00235      COPY GCGROUPC.                                               ELTMOPS 
00236 /                                                                 ELTMOPS 
00237  01  GCCP-TABULAR-REC-AREA.                                       ELTMOPS 
00238      COPY GCTGCCPC.                                               ELTMOPS 
00239      EJECT                                                        ELTMOPS 
00240  PROCEDURE DIVISION.                                              ELTMOPS 
00241 ************************************************************      ELTMOPS 
00242 *                                                          *      ELTMOPS 
00243 *                    PROCEDURE DIVISION                    *      ELTMOPS 
00244 *                                                          *      ELTMOPS 
00245 ************************************************************      ELTMOPS 
00246                                                                   ELTMOPS 
00247                                                                   ELTMOPS 
00248 ************************************************************      ELTMOPS 
00249 *                                                          *      ELTMOPS 
00250 *        MANDATORY OUTPATIENT SURGERY PROGRAM              *      ELTMOPS 
00251 *                                                          *      ELTMOPS 
00252 ************************************************************      ELTMOPS 
00253  MANDATORY-OUTPATIENT-SURGERY-P.                                  ELTMOPS 
00254      PERFORM INITIALIZATION.                                      ELTMOPS 
00255      PERFORM PROCESS.                                             ELTMOPS 
00256      GOBACK.                                                      ELTMOPS 
00257                                                                   ELTMOPS 
00258                                                                   ELTMOPS 
00259 ************************************************************      ELTMOPS 
00260 *                                                          *      ELTMOPS 
00261 *        INITIALIZATION                                    *      ELTMOPS 
00262 *                                                          *      ELTMOPS 
00263 ************************************************************      ELTMOPS 
00264  INITIALIZATION.                                                  ELTMOPS 
00265      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTMOPS 
00266      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTMOPS 
00267                                                                   ELTMOPS 
00268                                                                   ELTMOPS 
00269 ************************************************************      ELTMOPS 
00270 *                                                          *      ELTMOPS 
00271 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTMOPS 
00272 *                                                          *      ELTMOPS 
00273 ************************************************************      ELTMOPS 
00274  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTMOPS 
00275      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTMOPS 
00276      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTMOPS 
00277      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTMOPS 
00278                                                                   ELTMOPS 
00279                                                                   ELTMOPS 
00280 ************************************************************      ELTMOPS 
00281 *                                                          *      ELTMOPS 
00282 *        CHECK FOR VALID COMMAREA                          *      ELTMOPS 
00283 *                                                          *      ELTMOPS 
00284 ************************************************************      ELTMOPS 
00285  CHECK-FOR-VALID-COMMAREA.                                        ELTMOPS 
00286      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTMOPS 
00287          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTMOPS 
00288                                                                   ELTMOPS 
00289                                                                   ELTMOPS 
00290 ************************************************************      ELTMOPS 
00291 *                                                          *      ELTMOPS 
00292 *        SIGNAL INVALID COMMAREA                           *      ELTMOPS 
00293 *                                                          *      ELTMOPS 
00294 ************************************************************      ELTMOPS 
00295  SIGNAL-INVALID-COMMAREA.                                         ELTMOPS 
00296      EXEC CICS ABEND                                              ELTMOPS 
00297                ABCODE('EL01')                                     ELTMOPS 
00298         END-EXEC.                                                 ELTMOPS 
00299      EJECT                                                        ELTMOPS 
00300                                                                   ELTMOPS 
00301                                                                   ELTMOPS 
00302 ************************************************************      ELTMOPS 
00303 *                                                          *      ELTMOPS 
00304 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTMOPS 
00305 *                                                          *      ELTMOPS 
00306 ************************************************************      ELTMOPS 
00307  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTMOPS 
00308      IF ECA-CIA-PTR = NULL                                        ELTMOPS 
00309          PERFORM SIGNAL-INVALID-CIA                               ELTMOPS 
00310      ELSE                                                         ELTMOPS 
00311          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTMOPS 
00312                                                                   ELTMOPS 
00313                                                                   ELTMOPS 
00314 ************************************************************      ELTMOPS 
00315 *                                                          *      ELTMOPS 
00316 *        SIGNAL INVALID CIA                                *      ELTMOPS 
00317 *                                                          *      ELTMOPS 
00318 ************************************************************      ELTMOPS 
00319  SIGNAL-INVALID-CIA.                                              ELTMOPS 
00320      EXEC CICS ABEND                                              ELTMOPS 
00321                ABCODE('EL02')                                     ELTMOPS 
00322         END-EXEC.                                                 ELTMOPS 
00323                                                                   ELTMOPS 
00324                                                                   ELTMOPS 
00325 ************************************************************      ELTMOPS 
00326 *                                                          *      ELTMOPS 
00327 *        ESTABLISH ADDRESS OF CIA                          *      ELTMOPS 
00328 *                                                          *      ELTMOPS 
00329 ************************************************************      ELTMOPS 
00330  ESTABLISH-ADDRESS-OF-CIA.                                        ELTMOPS 
00331      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTMOPS 
00332                            ADDRESS OF                             ELTMOPS 
00333          CIA-ELS-COMMON-INTERFACE-AREA.                           ELTMOPS 
00334      EJECT                                                        ELTMOPS 
00335                                                                   ELTMOPS 
00336                                                                   ELTMOPS 
00337 ************************************************************      ELTMOPS 
00338 *                                                          *      ELTMOPS 
00339 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTMOPS 
00340 *                                                          *      ELTMOPS 
00341 ************************************************************      ELTMOPS 
00342  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTMOPS 
00343      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTMOPS 
00344      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMOPS 
00345                            ADDRESS OF                             ELTMOPS 
00346          SSB-SELECTOR-STATUS-CTL-BLK.                             ELTMOPS 
00347      IF CIA-RC-PTR-NULL                                           ELTMOPS 
00348          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMOPS 
00349                                                                   ELTMOPS 
00350                                                                   ELTMOPS 
00351 ************************************************************      ELTMOPS 
00352 *                                                          *      ELTMOPS 
00353 *        SIGNAL UNALLOC AREA ERROR                         *      ELTMOPS 
00354 *                                                          *      ELTMOPS 
00355 ************************************************************      ELTMOPS 
00356  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTMOPS 
00357      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTMOPS 
00358      PERFORM SIGNAL-ABEND.                                        ELTMOPS 
00359                                                                   ELTMOPS 
00360                                                                   ELTMOPS 
00361 ************************************************************      ELTMOPS 
00362 *                                                          *      ELTMOPS 
00363 *        SIGNAL ABEND                                      *      ELTMOPS 
00364 *                                                          *      ELTMOPS 
00365 ************************************************************      ELTMOPS 
00366  SIGNAL-ABEND.                                                    ELTMOPS 
00367      EXEC CICS ABEND                                              ELTMOPS 
00368                ABCODE(CIA-ABCODE)                                 ELTMOPS 
00369         END-EXEC.                                                 ELTMOPS 
00370      EJECT                                                        ELTMOPS 
00371                                                                   ELTMOPS 
00372                                                                   ELTMOPS 
00373 ************************************************************      ELTMOPS 
00374 *                                                          *      ELTMOPS 
00375 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTMOPS 
00376 *                                                          *      ELTMOPS 
00377 ************************************************************      ELTMOPS 
00378  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTMOPS 
00379      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTMOPS 
00380      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTMOPS 
00381      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTMOPS 
00382      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTMOPS 
00383      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTMOPS 
00384      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTMOPS 
00385      PERFORM ESTABLISH-ADDRESSABILITY-OF-CS.                      ELTMOPS 
00386                                                                   ELTMOPS 
00387                                                                   ELTMOPS 
00388 ************************************************************      ELTMOPS 
00389 *                                                          *      ELTMOPS 
00390 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTMOPS 
00391 *                                                          *      ELTMOPS 
00392 ************************************************************      ELTMOPS 
00393  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTMOPS 
00394      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTMOPS 
00395      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMOPS 
00396                            ADDRESS OF                             ELTMOPS 
00397          CMF-CODES-MANUAL-INTERFACE.                              ELTMOPS 
00398      IF CIA-RC-PTR-NULL                                           ELTMOPS 
00399          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMOPS 
00400      EJECT                                                        ELTMOPS 
00401                                                                   ELTMOPS 
00402                                                                   ELTMOPS 
00403 ************************************************************      ELTMOPS 
00404 *                                                          *      ELTMOPS 
00405 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTMOPS 
00406 *                                                          *      ELTMOPS 
00407 ************************************************************      ELTMOPS 
00408  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTMOPS 
00409      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTMOPS 
00410      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMOPS 
00411                            ADDRESS OF                             ELTMOPS 
00412          COF-OUTPUT-INTERFACE.                                    ELTMOPS 
00413      IF CIA-RC-PTR-NULL                                           ELTMOPS 
00414          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMOPS 
00415      EJECT                                                        ELTMOPS 
00416                                                                   ELTMOPS 
00417                                                                   ELTMOPS 
00418 ************************************************************      ELTMOPS 
00419 *                                                          *      ELTMOPS 
00420 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTMOPS 
00421 *                                                          *      ELTMOPS 
00422 ************************************************************      ELTMOPS 
00423  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTMOPS 
00424      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTMOPS 
00425      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMOPS 
00426                            ADDRESS OF                             ELTMOPS 
00427          SRP-SUBROUTINE-PARAMETERS.                               ELTMOPS 
00428      IF CIA-RC-PTR-NULL                                           ELTMOPS 
00429          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMOPS 
00430      EJECT                                                        ELTMOPS 
00431                                                                   ELTMOPS 
00432                                                                   ELTMOPS 
00433 ************************************************************      ELTMOPS 
00434 *                                                          *      ELTMOPS 
00435 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTMOPS 
00436 *                                                          *      ELTMOPS 
00437 ************************************************************      ELTMOPS 
00438  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTMOPS 
00439      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTMOPS 
00440      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMOPS 
00441                            ADDRESS OF                             ELTMOPS 
00442          TCAR-COMPRESSION-WORK-AREA.                              ELTMOPS 
00443      IF CIA-RC-PTR-NULL                                           ELTMOPS 
00444          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMOPS 
00445      EJECT                                                        ELTMOPS 
00446                                                                   ELTMOPS 
00447                                                                   ELTMOPS 
00448 ************************************************************      ELTMOPS 
00449 *                                                          *      ELTMOPS 
00450 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTMOPS 
00451 *                                                          *      ELTMOPS 
00452 ************************************************************      ELTMOPS 
00453  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTMOPS 
00454      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTMOPS 
00455      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMOPS 
00456                            ADDRESS OF                             ELTMOPS 
00457          KWA-FILE-KEY-WORK-AREA.                                  ELTMOPS 
00458      IF CIA-RC-PTR-NULL                                           ELTMOPS 
00459          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMOPS 
00460      EJECT                                                        ELTMOPS 
00461                                                                   ELTMOPS 
00462                                                                   ELTMOPS 
00463 ************************************************************      ELTMOPS 
00464 *                                                          *      ELTMOPS 
00465 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELTMOPS 
00466 *                                                          *      ELTMOPS 
00467 ************************************************************      ELTMOPS 
00468  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTMOPS 
00469      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTMOPS 
00470      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMOPS 
00471                            ADDRESS OF                             ELTMOPS 
00472          GROUP-SPECIFIC-REC.                                      ELTMOPS 
00473      IF CIA-RC-PTR-NULL                                           ELTMOPS 
00474          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMOPS 
00475      EJECT                                                        ELTMOPS 
00476                                                                   ELTMOPS 
00477                                                                   ELTMOPS 
00478 ************************************************************      ELTMOPS 
00479 *                                                          *      ELTMOPS 
00480 *        ESTABLISH ADDRESSABILITY OF CST CONTAINMENT PROGRA*      ELTMOPS 
00481 *                                                          *      ELTMOPS 
00482 ************************************************************      ELTMOPS 
00483  ESTABLISH-ADDRESSABILITY-OF-CS.                                  ELTMOPS 
00484      SET CIA-GCTABULR-DDN TO TRUE.                                ELTMOPS 
00485      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMOPS 
00486                            ADDRESS OF                             ELTMOPS 
00487          GCCP-TABULAR-REC-AREA.                                   ELTMOPS 
00488      IF CIA-RC-PTR-NULL                                           ELTMOPS 
00489          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMOPS 
00490      EJECT                                                        ELTMOPS 
00491                                                                   ELTMOPS 
00492                                                                   ELTMOPS 
00493 ************************************************************      ELTMOPS 
00494 *                                                          *      ELTMOPS 
00495 *        PROCESS                                           *      ELTMOPS 
00496 *                                                          *      ELTMOPS 
00497 ************************************************************      ELTMOPS 
00498  PROCESS.                                                         ELTMOPS 
00499      IF GCG-MAND-OP-SURG-PROG-IND  =  ZERO                        ELTMOPS 
00500          PERFORM GENERATE-NOT-APPLICABLE-MESSAG                   ELTMOPS 
00501      ELSE IF GCG-MAND-OP-SURG-PROG-IND  =  '08'                   ELTMOPS 
00502          PERFORM GENERATE-VOLUNTARY-MESSAGE                       ELTMOPS 
00503      ELSE                                                         ELTMOPS 
00504          PERFORM GENERATE-MOPS-TEXT.                              ELTMOPS 
00505      PERFORM TERMINATE-OUTPUT.                                    ELTMOPS 
00506                                                                   ELTMOPS 
00507                                                                   ELTMOPS 
00508 ************************************************************      ELTMOPS 
00509 *                                                          *      ELTMOPS 
00510 *        GENERATE NOT APPLICABLE MESSAGE                   *      ELTMOPS 
00511 *                                                          *      ELTMOPS 
00512 ************************************************************      ELTMOPS 
00513  GENERATE-NOT-APPLICABLE-MESSAG.                                  ELTMOPS 
00514      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTMOPS 
00515      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTMOPS 
00516      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMOPS 
00517      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTMOPS 
00518          (COF-NBR-DTL-LINES).                                     ELTMOPS 
00519      PERFORM EJECT-NEW-PAGE.                                      ELTMOPS 
00520      EJECT                                                        ELTMOPS 
00521                                                                   ELTMOPS 
00522                                                                   ELTMOPS 
00523 ************************************************************      ELTMOPS 
00524 *                                                          *      ELTMOPS 
00525 *        GENERATE VOLUNTARY MESSAGE                        *      ELTMOPS 
00526 *                                                          *      ELTMOPS 
00527 ************************************************************      ELTMOPS 
00528  GENERATE-VOLUNTARY-MESSAGE.                                      ELTMOPS 
00529      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTMOPS 
00530      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTMOPS 
00531      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMOPS 
00532      MOVE WS-VOLUNTARY-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).   ELTMOPS 
00533      PERFORM EJECT-NEW-PAGE.                                      ELTMOPS 
00534                                                                   ELTMOPS 
00535                                                                   ELTMOPS 
00536 ************************************************************      ELTMOPS 
00537 *                                                          *      ELTMOPS 
00538 *        SET UP OUTPUT SUBSCRIPTS                          *      ELTMOPS 
00539 *                                                          *      ELTMOPS 
00540 ************************************************************      ELTMOPS 
00541  SET-UP-OUTPUT-SUBSCRIPTS.                                        ELTMOPS 
00542      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMOPS 
00543      MOVE +2 TO COF-NBR-HDR-LINES.                                ELTMOPS 
00544      EJECT                                                        ELTMOPS 
00545                                                                   ELTMOPS 
00546                                                                   ELTMOPS 
00547 ************************************************************      ELTMOPS 
00548 *                                                          *      ELTMOPS 
00549 *        EJECT NEW PAGE                                    *      ELTMOPS 
00550 *                                                          *      ELTMOPS 
00551 ************************************************************      ELTMOPS 
00552  EJECT-NEW-PAGE.                                                  ELTMOPS 
00553      SET COF-NEW-PAGE    TO TRUE.                                 ELTMOPS 
00554      MOVE WS-HEADER-LINE TO COF-HDR-LINE (COF-NBR-HDR-LINES).     ELTMOPS 
00555      PERFORM LINK-TO-OUTPUT.                                      ELTMOPS 
00556      EJECT                                                        ELTMOPS 
00557                                                                   ELTMOPS 
00558                                                                   ELTMOPS 
00559 ************************************************************      ELTMOPS 
00560 *                                                          *      ELTMOPS 
00561 *        GENERATE MOPS TEXT                                *      ELTMOPS 
00562 *                                                          *      ELTMOPS 
00563 ************************************************************      ELTMOPS 
00564  GENERATE-MOPS-TEXT.                                              ELTMOPS 
00565      SET WS-POINTER2 TO NULLS.                                    ELTMOPS 
00566      SET WS-POINTER3 TO NULLS.                                    ELTMOPS 
00567      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTMOPS 
00568      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMOPS 
00569                            WS-POINTER2.                           ELTMOPS 
00570      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTMOPS 
00571      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMOPS 
00572                            WS-POINTER3.                           ELTMOPS 
00573      PERFORM SEARCH-FOR-GCCP-TABULAR.                             ELTMOPS 
00574      PERFORM DETERMINE-SELECTION.                                 ELTMOPS 
00575      EJECT                                                        ELTMOPS 
00576                                                                   ELTMOPS 
00577                                                                   ELTMOPS 
00578 ************************************************************      ELTMOPS 
00579 *                                                          *      ELTMOPS 
00580 *        TERMINATE OUTPUT                                  *      ELTMOPS 
00581 *                                                          *      ELTMOPS 
00582 ************************************************************      ELTMOPS 
00583  TERMINATE-OUTPUT.                                                ELTMOPS 
00584      SET COF-END TO TRUE.                                         ELTMOPS 
00585      PERFORM LINK-TO-OUTPUT.                                      ELTMOPS 
00586      EJECT                                                        ELTMOPS 
00587                                                                   ELTMOPS 
00588                                                                   ELTMOPS 
00589 ************************************************************      ELTMOPS 
00590 *                                                          *      ELTMOPS 
00591 *        SEARCH FOR GCCP TABULAR                           *      ELTMOPS 
00592 *                                                          *      ELTMOPS 
00593 ************************************************************      ELTMOPS 
00594  SEARCH-FOR-GCCP-TABULAR.                                         ELTMOPS 
00595      SET GCG-INDEX TO +1.                                         ELTMOPS 
00596      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTMOPS 
00597         AT END                                                    ELTMOPS 
00598              MOVE ZEROES TO KWA-PROVISION-SLOT-NO                 ELTMOPS 
00599         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GCCP                     ELTMOPS 
00600              MOVE GCG-TAB-ID (GCG-INDEX)                          ELTMOPS 
00601                  TO KWA-PROVISION-ID                              ELTMOPS 
00602              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTMOPS 
00603                  TO KWA-PROVISION-SLOT-NO                         ELTMOPS 
00604         END-SEARCH.                                               ELTMOPS 
00605      IF (KWA-PROVISION-SLOT-NO EQUAL ZEROES)                      ELTMOPS 
00606          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTMOPS 
00607      PERFORM GET-GCCP-TABULAR.                                    ELTMOPS 
00608      PERFORM SEARCH-THE-GSS-ENTRY.                                ELTMOPS 
00609      EJECT                                                        ELTMOPS 
00610                                                                   ELTMOPS 
00611                                                                   ELTMOPS 
00612 ************************************************************      ELTMOPS 
00613 *                                                          *      ELTMOPS 
00614 *        TRANSLATE APPROVAL SOURCE                         *      ELTMOPS 
00615 *                                                          *      ELTMOPS 
00616 ************************************************************      ELTMOPS 
00617  TRANSLATE-APPROVAL-SOURCE.                                       ELTMOPS 
00618      INITIALIZE WS-PERIOD-SWITCH.                                 ELTMOPS 
00619      INITIALIZE TCAR-FROM-AREA.                                   ELTMOPS 
00620      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
00621      MOVE WS-APPRVL-SRCE-LINE TO TCAR-FROM-LINE                   ELTMOPS 
00622          (TCAR-FROM-SUB).                                         ELTMOPS 
00623      ADD  +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
00624      SET ADDITIONAL-TEXT TO TRUE.                                 ELTMOPS 
00625      IF NOT-HOLDING-APPROVAL-SRCE                                 ELTMOPS 
00626          PERFORM GET-APPROVAL-TRANSLATION                         ELTMOPS 
00627      ELSE                                                         ELTMOPS 
00628          PERFORM USE-EXISTING-APPROVAL-TRANSLAT.                  ELTMOPS 
00629      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMOPS 
00630      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTMOPS 
00631      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMOPS 
00632                            WS-POINTER3.                           ELTMOPS 
00633      MOVE WS-APPROVAL TO TCAR-FROM-LINE                           ELTMOPS 
00634          (TCAR-FROM-SUB).                                         ELTMOPS 
00635      PERFORM FINISH-SENTENCE.                                     ELTMOPS 
00636      EJECT                                                        ELTMOPS 
00637                                                                   ELTMOPS 
00638                                                                   ELTMOPS 
00639 ************************************************************      ELTMOPS 
00640 *                                                          *      ELTMOPS 
00641 *        GET APPROVAL TRANSLATION                          *      ELTMOPS 
00642 *                                                          *      ELTMOPS 
00643 ************************************************************      ELTMOPS 
00644  GET-APPROVAL-TRANSLATION.                                        ELTMOPS 
00645      MOVE GSS-MO-APPROVAL-SOURCE-IND (GSS-INDEX) TO               ELTMOPS 
00646          CMF-CODE-VALUE.                                          ELTMOPS 
00647      MOVE   'MO-APPROVAL-SOURCE-IND' TO CMF-ELEMENT-SYSTEM-NAME.  ELTMOPS 
00648      PERFORM LINK-TO-CODES-MANUAL-INTERFACE.                      ELTMOPS 
00649      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTMOPS 
00650      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMOPS 
00651                            ADDRESS OF CMF-DESCR.                  ELTMOPS 
00652      EJECT                                                        ELTMOPS 
00653                                                                   ELTMOPS 
00654                                                                   ELTMOPS 
00655 ************************************************************      ELTMOPS 
00656 *                                                          *      ELTMOPS 
00657 *        USE EXISTING APPROVAL TRANSLATION                 *      ELTMOPS 
00658 *                                                          *      ELTMOPS 
00659 ************************************************************      ELTMOPS 
00660  USE-EXISTING-APPROVAL-TRANSLAT.                                  ELTMOPS 
00661      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTMOPS 
00662      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMOPS 
00663                            ADDRESS OF CMF-DESCR.                  ELTMOPS 
00664      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTMOPS 
00665      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMOPS 
00666                            WS-POINTER2.                           ELTMOPS 
00667                                                                   ELTMOPS 
00668                                                                   ELTMOPS 
00669 ************************************************************      ELTMOPS 
00670 *                                                          *      ELTMOPS 
00671 *        TRANSLATE AND DISPLAY CODE VALUE                  *      ELTMOPS 
00672 *                                                          *      ELTMOPS 
00673 ************************************************************      ELTMOPS 
00674  TRANSLATE-AND-DISPLAY-CODE-VAL.                                  ELTMOPS 
00675      PERFORM LINK-TO-CODES-MANUAL-INTERFACE.                      ELTMOPS 
00676      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMOPS 
00677                                                                   ELTMOPS 
00678                                                                   ELTMOPS 
00679 ************************************************************      ELTMOPS 
00680 *                                                          *      ELTMOPS 
00681 *        SIGNAL UNDEFINED TABULAR ERROR                    *      ELTMOPS 
00682 *                                                          *      ELTMOPS 
00683 ************************************************************      ELTMOPS 
00684  SIGNAL-UNDEFINED-TABULAR-ERROR.                                  ELTMOPS 
00685      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTMOPS 
00686      PERFORM SIGNAL-ABEND.                                        ELTMOPS 
00687      EJECT                                                        ELTMOPS 
00688                                                                   ELTMOPS 
00689                                                                   ELTMOPS 
00690 ************************************************************      ELTMOPS 
00691 *                                                          *      ELTMOPS 
00692 *        DETERMINE SELECTION                               *      ELTMOPS 
00693 *                                                          *      ELTMOPS 
00694 ************************************************************      ELTMOPS 
00695  DETERMINE-SELECTION.                                             ELTMOPS 
00696      SET NOT-HOLDING-APPROVAL-SRCE TO TRUE.                       ELTMOPS 
00697      IF SSB-PROV-CLASS-INST OR                                    ELTMOPS 
00698                 SSB-PROV-CLASS-BOTH                               ELTMOPS 
00699          PERFORM CREATE-INSTITUTIONAL-SCREEN.                     ELTMOPS 
00700      IF SSB-PROV-CLASS-PROF OR                                    ELTMOPS 
00701                 SSB-PROV-CLASS-BOTH                               ELTMOPS 
00702          PERFORM CREATE-PROFESSIONAL-SCREEN.                      ELTMOPS 
00703      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL '03' OR '04' OR       ELTMOPS 
00704                  '06' OR '08')                                    ELTMOPS 
00705          PERFORM CREATE-SUPPLEMENTAL-SCREEN.                      ELTMOPS 
00706                                                                   ELTMOPS 
00707                                                                   ELTMOPS 
00708 ************************************************************      ELTMOPS 
00709 *                                                          *      ELTMOPS 
00710 *        CREATE INSTITUTIONAL SCREEN                       *      ELTMOPS 
00711 *                                                          *      ELTMOPS 
00712 ************************************************************      ELTMOPS 
00713  CREATE-INSTITUTIONAL-SCREEN.                                     ELTMOPS 
00714      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTMOPS 
00715      MOVE WS-INST TO WS-HEADER-LINE-BCBSMM.                       ELTMOPS 
00716      IF GSS-MO-BC-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTMOPS 
00717          ZEROES                                                   ELTMOPS 
00718                 AND LOW-VALUES                                    ELTMOPS 
00719          PERFORM GENERATE-INSTITUTIONAL-TEXT                      ELTMOPS 
00720      ELSE                                                         ELTMOPS 
00721          PERFORM GENERATE-INSTITUTIONAL-NOT-APP.                  ELTMOPS 
00722      EJECT                                                        ELTMOPS 
00723                                                                   ELTMOPS 
00724                                                                   ELTMOPS 
00725 ************************************************************      ELTMOPS 
00726 *                                                          *      ELTMOPS 
00727 *        GENERATE INSTITUTIONAL TEXT                       *      ELTMOPS 
00728 *                                                          *      ELTMOPS 
00729 ************************************************************      ELTMOPS 
00730  GENERATE-INSTITUTIONAL-TEXT.                                     ELTMOPS 
00731      PERFORM EJECT-NEW-PAGE.                                      ELTMOPS 
00732      PERFORM TRANSLATE-PARTICIPATION-IND.                         ELTMOPS 
00733      IF GSS-MO-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTMOPS 
00734          SPACES                                                   ELTMOPS 
00735                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
00736          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMOPS 
00737      PERFORM TRANSLATE-BC-IND.                                    ELTMOPS 
00738      IF ((GSS-MO-BC-IP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL     ELTMOPS 
00739          SPACES                                                   ELTMOPS 
00740                  AND ZEROES AND LOW-VALUES)) OR                   ELTMOPS 
00741                ((GSS-MO-BC-OP-ALT-PRICING-METH (GSS-INDEX) NOT    ELTMOPS 
00742          EQUAL SPACES                                             ELTMOPS 
00743                  AND ZEROES AND LOW-VALUES))                      ELTMOPS 
00744          PERFORM GENERATE-BC-ALT-PRIC-TEXT.                       ELTMOPS 
00745      SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTMOPS 
00746      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTMOPS 
00747      IF GSS-MO-BC-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES        ELTMOPS 
00748                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
00749          PERFORM GENERATE-BC-CALCULATION-METHOD.                  ELTMOPS 
00750      IF GSS-MO-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTMOPS 
00751          SPACES                                                   ELTMOPS 
00752                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
00753          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTMOPS 
00754      IF ((GSS-MO-BC-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL         ELTMOPS 
00755          SPACES AND                                               ELTMOPS 
00756                   ZEROES AND LOW-VALUES)) OR                      ELTMOPS 
00757                 ((GSS-MO-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT       ELTMOPS 
00758          EQUAL SPACES AND                                         ELTMOPS 
00759                   ZEROES AND LOW-VALUES)) OR                      ELTMOPS 
00760                ((GSS-MO-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT      ELTMOPS 
00761          EQUAL SPACES AND                                         ELTMOPS 
00762                   ZEROES AND LOW-VALUES))                         ELTMOPS 
00763          PERFORM GENERATE-BC-BENE-REDUCT-TEXT.                    ELTMOPS 
00764      IF GSS-MO-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL SPACES AND    ELTMOPS 
00765          ZEROES                                                   ELTMOPS 
00766                 AND LOW-VALUES                                    ELTMOPS 
00767          PERFORM GENERATE-SPILL-OVER-TEXT.                        ELTMOPS 
00768      PERFORM GENERATE-OTHER-TABULAR-INFO.                         ELTMOPS 
00769      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTMOPS 
00770      EJECT                                                        ELTMOPS 
00771                                                                   ELTMOPS 
00772                                                                   ELTMOPS 
00773 ************************************************************      ELTMOPS 
00774 *                                                          *      ELTMOPS 
00775 *        GENERATE INSTITUTIONAL NOT APPLICABLE             *      ELTMOPS 
00776 *                                                          *      ELTMOPS 
00777 ************************************************************      ELTMOPS 
00778  GENERATE-INSTITUTIONAL-NOT-APP.                                  ELTMOPS 
00779      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTMOPS 
00780      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMOPS 
00781      MOVE WS-NOT-APPLY-TO-LOB-BC TO COF-DTL-LINE                  ELTMOPS 
00782          (COF-NBR-DTL-LINES).                                     ELTMOPS 
00783      PERFORM EJECT-NEW-PAGE.                                      ELTMOPS 
00784      EJECT                                                        ELTMOPS 
00785                                                                   ELTMOPS 
00786                                                                   ELTMOPS 
00787 ************************************************************      ELTMOPS 
00788 *                                                          *      ELTMOPS 
00789 *        TRANSLATE PARTICIPATION IND                       *      ELTMOPS 
00790 *                                                          *      ELTMOPS 
00791 ************************************************************      ELTMOPS 
00792  TRANSLATE-PARTICIPATION-IND.                                     ELTMOPS 
00793      INITIALIZE TCAR-FROM-AREA.                                   ELTMOPS 
00794      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
00795      MOVE WS-PARTICIPATION-LINE TO TCAR-FROM-LINE(TCAR-FROM-SUB). ELTMOPS 
00796      ADD +1 TO TCAR-FROM-SUB.                                     ELTMOPS 
00797      SET ADDITIONAL-TEXT   TO TRUE.                               ELTMOPS 
00798      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMOPS 
00799      SET PERIOD-NEEDED     TO TRUE.                               ELTMOPS 
00800      MOVE GCG-MAND-OP-SURG-PROG-IND TO CMF-CODE-VALUE.            ELTMOPS 
00801      MOVE   'MAND-OP-SURG-PROG-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTMOPS 
00802      PERFORM LINK-TO-GROUP-CODES-MANUAL-INT.                      ELTMOPS 
00803      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMOPS 
00804      EJECT                                                        ELTMOPS 
00805                                                                   ELTMOPS 
00806                                                                   ELTMOPS 
00807 ************************************************************      ELTMOPS 
00808 *                                                          *      ELTMOPS 
00809 *        TRANSLATE BC IND                                  *      ELTMOPS 
00810 *                                                          *      ELTMOPS 
00811 ************************************************************      ELTMOPS 
00812  TRANSLATE-BC-IND.                                                ELTMOPS 
00813      INITIALIZE TCAR-FROM-AREA.                                   ELTMOPS 
00814      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
00815      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMOPS 
00816      SET PERIOD-NEEDED TO TRUE.                                   ELTMOPS 
00817      MOVE GSS-MO-BC-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTMOPS 
00818      MOVE   'MO-BC-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTMOPS 
00819      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
00820      EJECT                                                        ELTMOPS 
00821                                                                   ELTMOPS 
00822                                                                   ELTMOPS 
00823 ************************************************************      ELTMOPS 
00824 *                                                          *      ELTMOPS 
00825 *        GENERATE BC CALCULATION METHOD                    *      ELTMOPS 
00826 *                                                          *      ELTMOPS 
00827 ************************************************************      ELTMOPS 
00828  GENERATE-BC-CALCULATION-METHOD.                                  ELTMOPS 
00829      INITIALIZE TCAR-FROM-AREA.                                   ELTMOPS 
00830      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
00831      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMOPS 
00832      SET PERIOD-NEEDED TO TRUE.                                   ELTMOPS 
00833      MOVE GSS-MO-BC-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMOPS 
00834      MOVE   'MO-BC-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTMOPS 
00835      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
00836      EJECT                                                        ELTMOPS 
00837                                                                   ELTMOPS 
00838                                                                   ELTMOPS 
00839 ************************************************************      ELTMOPS 
00840 *                                                          *      ELTMOPS 
00841 *        GENERATE BC ALT PRIC TEXT                         *      ELTMOPS 
00842 *                                                          *      ELTMOPS 
00843 ************************************************************      ELTMOPS 
00844  GENERATE-BC-ALT-PRIC-TEXT.                                       ELTMOPS 
00845      INITIALIZE TCAR-FROM-AREA.                                   ELTMOPS 
00846      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
00847      INITIALIZE WS-PERIOD-SWITCH.                                 ELTMOPS 
00848      MOVE WS-ALT-PRICING-LINE-BC TO  TCAR-FROM-LINE               ELTMOPS 
00849          (TCAR-FROM-SUB).                                         ELTMOPS 
00850      ADD +1 TO TCAR-FROM-SUB.                                     ELTMOPS 
00851      SET ADDITIONAL-TEXT TO TRUE.                                 ELTMOPS 
00852      IF GSS-MO-BC-IP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL       ELTMOPS 
00853          SPACES                                                   ELTMOPS 
00854                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
00855          PERFORM TRANSLATE-BC-IP-ALT-PRIC-METH.                   ELTMOPS 
00856      IF GSS-MO-BC-OP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL       ELTMOPS 
00857          SPACES                                                   ELTMOPS 
00858                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
00859          PERFORM TRANSLATE-BC-OP-ALT-PRIC-METH.                   ELTMOPS 
00860      EJECT                                                        ELTMOPS 
00861                                                                   ELTMOPS 
00862                                                                   ELTMOPS 
00863 ************************************************************      ELTMOPS 
00864 *                                                          *      ELTMOPS 
00865 *        TRANSLATE BC IP ALT PRIC METH                     *      ELTMOPS 
00866 *                                                          *      ELTMOPS 
00867 ************************************************************      ELTMOPS 
00868  TRANSLATE-BC-IP-ALT-PRIC-METH.                                   ELTMOPS 
00869      MOVE GSS-MO-BC-IP-ALT-PRICING-METH (GSS-INDEX) TO            ELTMOPS 
00870          CMF-CODE-VALUE.                                          ELTMOPS 
00871      MOVE 'MO-BC-IP-ALT-PRICING-METH' TO CMF-ELEMENT-SYSTEM-NAME. ELTMOPS 
00872      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
00873      IF GSS-MO-BC-OP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL       ELTMOPS 
00874          SPACES                                                   ELTMOPS 
00875                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
00876          PERFORM CONTINUE-ALT-PRIC-SENTENCE                       ELTMOPS 
00877      ELSE                                                         ELTMOPS 
00878          PERFORM FINISH-ALT-PRIC-SENTENCE.                        ELTMOPS 
00879                                                                   ELTMOPS 
00880                                                                   ELTMOPS 
00881 ************************************************************      ELTMOPS 
00882 *                                                          *      ELTMOPS 
00883 *        CONTINUE ALT PRIC SENTENCE                        *      ELTMOPS 
00884 *                                                          *      ELTMOPS 
00885 ************************************************************      ELTMOPS 
00886  CONTINUE-ALT-PRIC-SENTENCE.                                      ELTMOPS 
00887      MOVE WS-INPATIENT TO TCAR-FROM-LINE                          ELTMOPS 
00888          (TCAR-FROM-SUB).                                         ELTMOPS 
00889      ADD  +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
00890                                                                   ELTMOPS 
00891                                                                   ELTMOPS 
00892 ************************************************************      ELTMOPS 
00893 *                                                          *      ELTMOPS 
00894 *        FINISH ALT PRIC SENTENCE                          *      ELTMOPS 
00895 *                                                          *      ELTMOPS 
00896 ************************************************************      ELTMOPS 
00897  FINISH-ALT-PRIC-SENTENCE.                                        ELTMOPS 
00898      MOVE WS-INPATIENT-END TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELTMOPS 
00899      PERFORM FINISH-SENTENCE.                                     ELTMOPS 
00900                                                                   ELTMOPS 
00901                                                                   ELTMOPS 
00902 ************************************************************      ELTMOPS 
00903 *                                                          *      ELTMOPS 
00904 *        TRANSLATE BC OP ALT PRIC METH                     *      ELTMOPS 
00905 *                                                          *      ELTMOPS 
00906 ************************************************************      ELTMOPS 
00907  TRANSLATE-BC-OP-ALT-PRIC-METH.                                   ELTMOPS 
00908      MOVE GSS-MO-BC-OP-ALT-PRICING-METH (GSS-INDEX) TO            ELTMOPS 
00909          CMF-CODE-VALUE.                                          ELTMOPS 
00910      MOVE 'MO-BC-OP-ALT-PRICING-METH' TO CMF-ELEMENT-SYSTEM-NAME. ELTMOPS 
00911      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
00912      MOVE WS-OUTPATIENT TO TCAR-FROM-LINE (TCAR-FROM-SUB).        ELTMOPS 
00913      PERFORM FINISH-SENTENCE.                                     ELTMOPS 
00914      EJECT                                                        ELTMOPS 
00915                                                                   ELTMOPS 
00916                                                                   ELTMOPS 
00917 ************************************************************      ELTMOPS 
00918 *                                                          *      ELTMOPS 
00919 *        FINISH SENTENCE                                   *      ELTMOPS 
00920 *                                                          *      ELTMOPS 
00921 ************************************************************      ELTMOPS 
00922  FINISH-SENTENCE.                                                 ELTMOPS 
00923      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMOPS 
00924      PERFORM REFORMAT-AND-WRITE-TEXT.                             ELTMOPS 
00925      EJECT                                                        ELTMOPS 
00926                                                                   ELTMOPS 
00927                                                                   ELTMOPS 
00928 ************************************************************      ELTMOPS 
00929 *                                                          *      ELTMOPS 
00930 *        GENERATE ACCUM TABULAR DATA                       *      ELTMOPS 
00931 *                                                          *      ELTMOPS 
00932 ************************************************************      ELTMOPS 
00933  GENERATE-ACCUM-TABULAR-DATA.                                     ELTMOPS 
00934      PERFORM GENERATE-COINSURANCE.                                ELTMOPS 
00935      PERFORM GENERATE-COPAY.                                      ELTMOPS 
00936      PERFORM GENERATE-DEDUCTIBLE.                                 ELTMOPS 
00937      PERFORM GENERATE-MAXIMUM.                                    ELTMOPS 
00938      EJECT                                                        ELTMOPS 
00939                                                                   ELTMOPS 
00940                                                                   ELTMOPS 
00941 ************************************************************      ELTMOPS 
00942 *                                                          *      ELTMOPS 
00943 *        GENERATE COMBINED BENEFITS REDUCTION TEXT         *      ELTMOPS 
00944 *                                                          *      ELTMOPS 
00945 ************************************************************      ELTMOPS 
00946  GENERATE-COMBINED-BENEFITS-RED.                                  ELTMOPS 
00947      MOVE 'MO' TO SRP-COST-CONT-TYPE.                             ELTMOPS 
00948      MOVE 'MANDATORY OUTPATIENT SURGERY PROGRAM ' TO              ELTMOPS 
00949          SRP-CCP-NAME.                                            ELTMOPS 
00950      MOVE GSS-MO-COMB-BENE-REDUCT-IND (GSS-INDEX)                 ELTMOPS 
00951                TO SRP-CCP-COMB-BENE-REDUCT-IND.                   ELTMOPS 
00952      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELTMOPS 
00953      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMOPS 
00954                            ADDRESS OF                             ELTMOPS 
00955          GCCP-TABULAR-REC-AREA.                                   ELTMOPS 
00956      PERFORM CALL-CBRI-INTERFACE.                                 ELTMOPS 
00957      EJECT                                                        ELTMOPS 
00958                                                                   ELTMOPS 
00959                                                                   ELTMOPS 
00960 ************************************************************      ELTMOPS 
00961 *                                                          *      ELTMOPS 
00962 *        GENERATE BC BENE REDUCT TEXT                      *      ELTMOPS 
00963 *                                                          *      ELTMOPS 
00964 ************************************************************      ELTMOPS 
00965  GENERATE-BC-BENE-REDUCT-TEXT.                                    ELTMOPS 
00966      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMOPS 
00967      MOVE WS-BENE-REDUCT-LINE TO COF-DTL-LINE                     ELTMOPS 
00968          (COF-NBR-DTL-LINES).                                     ELTMOPS 
00969      PERFORM LINK-TO-OUTPUT.                                      ELTMOPS 
00970      INITIALIZE ADDITIONAL-TEXT-SWITCH.                           ELTMOPS 
00971      INITIALIZE WS-PERIOD-SWITCH.                                 ELTMOPS 
00972      INITIALIZE TCAR-FROM-AREA.                                   ELTMOPS 
00973      IF GSS-MO-BC-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMOPS 
00974          SPACES                                                   ELTMOPS 
00975                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
00976          PERFORM TRANSLATE-BC-DEDUCT-IND.                         ELTMOPS 
00977      IF GSS-MO-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMOPS 
00978          SPACES                                                   ELTMOPS 
00979                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
00980          PERFORM TRANSLATE-BC-OPEX-APPLIC.                        ELTMOPS 
00981      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMOPS 
00982      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMOPS 
00983      PERFORM LINK-TO-OUTPUT.                                      ELTMOPS 
00984      IF GSS-MO-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTMOPS 
00985          SPACES                                                   ELTMOPS 
00986                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
00987          PERFORM TRANSLATE-BC-OPEX-OVERRIDE.                      ELTMOPS 
00988      EJECT                                                        ELTMOPS 
00989                                                                   ELTMOPS 
00990                                                                   ELTMOPS 
00991 ************************************************************      ELTMOPS 
00992 *                                                          *      ELTMOPS 
00993 *        TRANSLATE BC DEDUCT IND                           *      ELTMOPS 
00994 *                                                          *      ELTMOPS 
00995 ************************************************************      ELTMOPS 
00996  TRANSLATE-BC-DEDUCT-IND.                                         ELTMOPS 
00997      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
00998      MOVE GSS-MO-BC-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTMOPS 
00999          CMF-CODE-VALUE.                                          ELTMOPS 
01000      MOVE 'MO-BC-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMOPS 
01001      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
01002      EJECT                                                        ELTMOPS 
01003                                                                   ELTMOPS 
01004                                                                   ELTMOPS 
01005 ************************************************************      ELTMOPS 
01006 *                                                          *      ELTMOPS 
01007 *        TRANSLATE BC OPEX APPLIC                          *      ELTMOPS 
01008 *                                                          *      ELTMOPS 
01009 ************************************************************      ELTMOPS 
01010  TRANSLATE-BC-OPEX-APPLIC.                                        ELTMOPS 
01011      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
01012      MOVE GSS-MO-BC-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTMOPS 
01013          CMF-CODE-VALUE.                                          ELTMOPS 
01014      MOVE 'MO-BC-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMOPS 
01015      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
01016      EJECT                                                        ELTMOPS 
01017                                                                   ELTMOPS 
01018                                                                   ELTMOPS 
01019 ************************************************************      ELTMOPS 
01020 *                                                          *      ELTMOPS 
01021 *        TRANSLATE BC OPEX OVERRIDE                        *      ELTMOPS 
01022 *                                                          *      ELTMOPS 
01023 ************************************************************      ELTMOPS 
01024  TRANSLATE-BC-OPEX-OVERRIDE.                                      ELTMOPS 
01025      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
01026      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMOPS 
01027      SET PERIOD-NEEDED TO TRUE.                                   ELTMOPS 
01028      MOVE GSS-MO-BC-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTMOPS 
01029          CMF-CODE-VALUE.                                          ELTMOPS 
01030      MOVE 'MO-BC-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTMOPS 
01031      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
01032      EJECT                                                        ELTMOPS 
01033                                                                   ELTMOPS 
01034                                                                   ELTMOPS 
01035 ************************************************************      ELTMOPS 
01036 *                                                          *      ELTMOPS 
01037 *        GENERATE DISCLAIMER MESSAGE                       *      ELTMOPS 
01038 *                                                          *      ELTMOPS 
01039 ************************************************************      ELTMOPS 
01040  GENERATE-DISCLAIMER-MESSAGE.                                     ELTMOPS 
01041      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMOPS 
01042      MOVE WS-DISCLAIMER-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).  ELTMOPS 
01043      ADD  +1 TO COF-NBR-DTL-LINES.                                ELTMOPS 
01044      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMOPS 
01045      PERFORM LINK-TO-OUTPUT.                                      ELTMOPS 
01046      EJECT                                                        ELTMOPS 
01047                                                                   ELTMOPS 
01048                                                                   ELTMOPS 
01049 ************************************************************      ELTMOPS 
01050 *                                                          *      ELTMOPS 
01051 *        CREATE PROFESSIONAL SCREEN                        *      ELTMOPS 
01052 *                                                          *      ELTMOPS 
01053 ************************************************************      ELTMOPS 
01054  CREATE-PROFESSIONAL-SCREEN.                                      ELTMOPS 
01055      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTMOPS 
01056      MOVE WS-PROF TO WS-HEADER-LINE-BCBSMM.                       ELTMOPS 
01057      IF GSS-MO-BS-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTMOPS 
01058          ZEROES                                                   ELTMOPS 
01059                 AND LOW-VALUES                                    ELTMOPS 
01060          PERFORM GENERATE-PROFESSIONAL-TEXT                       ELTMOPS 
01061      ELSE                                                         ELTMOPS 
01062          PERFORM GENERATE-PROFESSIONAL-NOT-APPL.                  ELTMOPS 
01063      EJECT                                                        ELTMOPS 
01064                                                                   ELTMOPS 
01065                                                                   ELTMOPS 
01066 ************************************************************      ELTMOPS 
01067 *                                                          *      ELTMOPS 
01068 *        GENERATE PROFESSIONAL TEXT                        *      ELTMOPS 
01069 *                                                          *      ELTMOPS 
01070 ************************************************************      ELTMOPS 
01071  GENERATE-PROFESSIONAL-TEXT.                                      ELTMOPS 
01072      PERFORM EJECT-NEW-PAGE.                                      ELTMOPS 
01073      PERFORM TRANSLATE-PARTICIPATION-IND.                         ELTMOPS 
01074      IF GSS-MO-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTMOPS 
01075          SPACES                                                   ELTMOPS 
01076                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
01077          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMOPS 
01078      PERFORM TRANSLATE-BS-IND.                                    ELTMOPS 
01079      IF ((GSS-MO-BS-IP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL     ELTMOPS 
01080          SPACES                                                   ELTMOPS 
01081                  AND ZEROES AND LOW-VALUES)) OR                   ELTMOPS 
01082                ((GSS-MO-BS-OP-ALT-PRICING-METH (GSS-INDEX) NOT    ELTMOPS 
01083          EQUAL SPACES                                             ELTMOPS 
01084                  AND ZEROES AND LOW-VALUES))                      ELTMOPS 
01085          PERFORM GENERATE-BS-ALT-PRIC-TEXT.                       ELTMOPS 
01086      SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTMOPS 
01087      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTMOPS 
01088      IF GSS-MO-BS-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES        ELTMOPS 
01089                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
01090          PERFORM GENERATE-BS-CALCULATION-METHOD.                  ELTMOPS 
01091      IF GSS-MO-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTMOPS 
01092          SPACES                                                   ELTMOPS 
01093                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
01094          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTMOPS 
01095      IF ((GSS-MO-BS-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL         ELTMOPS 
01096          SPACES AND                                               ELTMOPS 
01097                   ZEROES AND LOW-VALUES)) OR                      ELTMOPS 
01098                 ((GSS-MO-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT       ELTMOPS 
01099          EQUAL SPACES AND                                         ELTMOPS 
01100                   ZEROES AND LOW-VALUES)) OR                      ELTMOPS 
01101                ((GSS-MO-BS-OPEX-OVERRIDE-IND (GSS-INDEX) NOT      ELTMOPS 
01102          EQUAL SPACES AND                                         ELTMOPS 
01103                   ZEROES AND LOW-VALUES))                         ELTMOPS 
01104          PERFORM GENERATE-BS-BENE-REDUCT-TEXT.                    ELTMOPS 
01105      IF GSS-MO-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL SPACES AND    ELTMOPS 
01106          ZEROES                                                   ELTMOPS 
01107                 AND LOW-VALUES                                    ELTMOPS 
01108          PERFORM GENERATE-SPILL-OVER-TEXT.                        ELTMOPS 
01109      PERFORM GENERATE-OTHER-TABULAR-INFO.                         ELTMOPS 
01110      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTMOPS 
01111                                                                   ELTMOPS 
01112                                                                   ELTMOPS 
01113 ************************************************************      ELTMOPS 
01114 *                                                          *      ELTMOPS 
01115 *        GENERATE PROFESSIONAL NOT APPLICABLE              *      ELTMOPS 
01116 *                                                          *      ELTMOPS 
01117 ************************************************************      ELTMOPS 
01118  GENERATE-PROFESSIONAL-NOT-APPL.                                  ELTMOPS 
01119      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMOPS 
01120      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMOPS 
01121      MOVE WS-NOT-APPLY-TO-LOB-BS TO COF-DTL-LINE                  ELTMOPS 
01122          (COF-NBR-DTL-LINES).                                     ELTMOPS 
01123      PERFORM EJECT-NEW-PAGE.                                      ELTMOPS 
01124      EJECT                                                        ELTMOPS 
01125                                                                   ELTMOPS 
01126                                                                   ELTMOPS 
01127 ************************************************************      ELTMOPS 
01128 *                                                          *      ELTMOPS 
01129 *        TRANSLATE BS IND                                  *      ELTMOPS 
01130 *                                                          *      ELTMOPS 
01131 ************************************************************      ELTMOPS 
01132  TRANSLATE-BS-IND.                                                ELTMOPS 
01133      INITIALIZE TCAR-FROM-AREA.                                   ELTMOPS 
01134      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
01135      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMOPS 
01136      SET PERIOD-NEEDED TO TRUE.                                   ELTMOPS 
01137      MOVE GSS-MO-BS-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTMOPS 
01138      MOVE   'MO-BS-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTMOPS 
01139      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
01140      EJECT                                                        ELTMOPS 
01141                                                                   ELTMOPS 
01142                                                                   ELTMOPS 
01143 ************************************************************      ELTMOPS 
01144 *                                                          *      ELTMOPS 
01145 *        GENERATE BS CALCULATION METHOD                    *      ELTMOPS 
01146 *                                                          *      ELTMOPS 
01147 ************************************************************      ELTMOPS 
01148  GENERATE-BS-CALCULATION-METHOD.                                  ELTMOPS 
01149      INITIALIZE TCAR-FROM-AREA.                                   ELTMOPS 
01150      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
01151      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMOPS 
01152      SET PERIOD-NEEDED TO TRUE.                                   ELTMOPS 
01153      MOVE GSS-MO-BS-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMOPS 
01154      MOVE   'MO-BS-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTMOPS 
01155      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
01156      EJECT                                                        ELTMOPS 
01157                                                                   ELTMOPS 
01158                                                                   ELTMOPS 
01159 ************************************************************      ELTMOPS 
01160 *                                                          *      ELTMOPS 
01161 *        GENERATE BS ALT PRIC TEXT                         *      ELTMOPS 
01162 *                                                          *      ELTMOPS 
01163 ************************************************************      ELTMOPS 
01164  GENERATE-BS-ALT-PRIC-TEXT.                                       ELTMOPS 
01165      INITIALIZE TCAR-FROM-AREA.                                   ELTMOPS 
01166      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
01167      INITIALIZE WS-PERIOD-SWITCH.                                 ELTMOPS 
01168      MOVE WS-ALT-PRICING-LINE-BS TO TCAR-FROM-LINE                ELTMOPS 
01169          (TCAR-FROM-SUB).                                         ELTMOPS 
01170      ADD +1 TO TCAR-FROM-SUB.                                     ELTMOPS 
01171      SET ADDITIONAL-TEXT TO TRUE.                                 ELTMOPS 
01172      IF GSS-MO-BS-IP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL       ELTMOPS 
01173          SPACES                                                   ELTMOPS 
01174                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
01175          PERFORM TRANSLATE-BS-IP-ALT-PRIC-METH.                   ELTMOPS 
01176      IF GSS-MO-BS-OP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL       ELTMOPS 
01177          SPACES                                                   ELTMOPS 
01178                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
01179          PERFORM TRANSLATE-BS-OP-ALT-PRIC-METH.                   ELTMOPS 
01180      EJECT                                                        ELTMOPS 
01181                                                                   ELTMOPS 
01182                                                                   ELTMOPS 
01183 ************************************************************      ELTMOPS 
01184 *                                                          *      ELTMOPS 
01185 *        TRANSLATE BS IP ALT PRIC METH                     *      ELTMOPS 
01186 *                                                          *      ELTMOPS 
01187 ************************************************************      ELTMOPS 
01188  TRANSLATE-BS-IP-ALT-PRIC-METH.                                   ELTMOPS 
01189      MOVE GSS-MO-BS-IP-ALT-PRICING-METH (GSS-INDEX) TO            ELTMOPS 
01190          CMF-CODE-VALUE.                                          ELTMOPS 
01191      MOVE 'MO-BS-IP-ALT-PRICING-METH' TO CMF-ELEMENT-SYSTEM-NAME. ELTMOPS 
01192      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
01193      IF GSS-MO-BS-OP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL       ELTMOPS 
01194          SPACES                                                   ELTMOPS 
01195                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
01196          PERFORM CONTINUE-ALT-PRIC-SENTENCE                       ELTMOPS 
01197      ELSE                                                         ELTMOPS 
01198          PERFORM FINISH-ALT-PRIC-SENTENCE.                        ELTMOPS 
01199                                                                   ELTMOPS 
01200                                                                   ELTMOPS 
01201 ************************************************************      ELTMOPS 
01202 *                                                          *      ELTMOPS 
01203 *        TRANSLATE BS OP ALT PRIC METH                     *      ELTMOPS 
01204 *                                                          *      ELTMOPS 
01205 ************************************************************      ELTMOPS 
01206  TRANSLATE-BS-OP-ALT-PRIC-METH.                                   ELTMOPS 
01207      MOVE GSS-MO-BS-OP-ALT-PRICING-METH (GSS-INDEX) TO            ELTMOPS 
01208          CMF-CODE-VALUE.                                          ELTMOPS 
01209      MOVE 'MO-BS-OP-ALT-PRICING-METH' TO CMF-ELEMENT-SYSTEM-NAME. ELTMOPS 
01210      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
01211      MOVE WS-OUTPATIENT TO TCAR-FROM-LINE (TCAR-FROM-SUB).        ELTMOPS 
01212      PERFORM FINISH-SENTENCE.                                     ELTMOPS 
01213      EJECT                                                        ELTMOPS 
01214                                                                   ELTMOPS 
01215                                                                   ELTMOPS 
01216 ************************************************************      ELTMOPS 
01217 *                                                          *      ELTMOPS 
01218 *        GENERATE BS BENE REDUCT TEXT                      *      ELTMOPS 
01219 *                                                          *      ELTMOPS 
01220 ************************************************************      ELTMOPS 
01221  GENERATE-BS-BENE-REDUCT-TEXT.                                    ELTMOPS 
01222      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMOPS 
01223      MOVE WS-BENE-REDUCT-LINE TO COF-DTL-LINE                     ELTMOPS 
01224          (COF-NBR-DTL-LINES).                                     ELTMOPS 
01225      PERFORM LINK-TO-OUTPUT.                                      ELTMOPS 
01226      INITIALIZE ADDITIONAL-TEXT-SWITCH.                           ELTMOPS 
01227      INITIALIZE WS-PERIOD-SWITCH.                                 ELTMOPS 
01228      INITIALIZE TCAR-FROM-AREA.                                   ELTMOPS 
01229      IF GSS-MO-BS-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMOPS 
01230          SPACES                                                   ELTMOPS 
01231                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
01232          PERFORM TRANSLATE-BS-DEDUCT-IND.                         ELTMOPS 
01233      IF GSS-MO-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMOPS 
01234          SPACES                                                   ELTMOPS 
01235                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
01236          PERFORM TRANSLATE-BS-OPEX-APPLIC.                        ELTMOPS 
01237      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMOPS 
01238      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMOPS 
01239      PERFORM LINK-TO-OUTPUT.                                      ELTMOPS 
01240      IF GSS-MO-BS-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTMOPS 
01241          SPACES                                                   ELTMOPS 
01242                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
01243          PERFORM TRANSLATE-BS-OPEX-OVERRIDE.                      ELTMOPS 
01244      EJECT                                                        ELTMOPS 
01245                                                                   ELTMOPS 
01246                                                                   ELTMOPS 
01247 ************************************************************      ELTMOPS 
01248 *                                                          *      ELTMOPS 
01249 *        TRANSLATE BS DEDUCT IND                           *      ELTMOPS 
01250 *                                                          *      ELTMOPS 
01251 ************************************************************      ELTMOPS 
01252  TRANSLATE-BS-DEDUCT-IND.                                         ELTMOPS 
01253      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
01254      MOVE GSS-MO-BS-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTMOPS 
01255          CMF-CODE-VALUE.                                          ELTMOPS 
01256      MOVE 'MO-BS-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMOPS 
01257      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
01258      EJECT                                                        ELTMOPS 
01259                                                                   ELTMOPS 
01260                                                                   ELTMOPS 
01261 ************************************************************      ELTMOPS 
01262 *                                                          *      ELTMOPS 
01263 *        TRANSLATE BS OPEX APPLIC                          *      ELTMOPS 
01264 *                                                          *      ELTMOPS 
01265 ************************************************************      ELTMOPS 
01266  TRANSLATE-BS-OPEX-APPLIC.                                        ELTMOPS 
01267      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
01268      MOVE GSS-MO-BS-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTMOPS 
01269          CMF-CODE-VALUE.                                          ELTMOPS 
01270      MOVE 'MO-BS-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMOPS 
01271      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
01272      EJECT                                                        ELTMOPS 
01273                                                                   ELTMOPS 
01274                                                                   ELTMOPS 
01275 ************************************************************      ELTMOPS 
01276 *                                                          *      ELTMOPS 
01277 *        TRANSLATE BS OPEX OVERRIDE                        *      ELTMOPS 
01278 *                                                          *      ELTMOPS 
01279 ************************************************************      ELTMOPS 
01280  TRANSLATE-BS-OPEX-OVERRIDE.                                      ELTMOPS 
01281      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
01282      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMOPS 
01283      SET PERIOD-NEEDED TO TRUE.                                   ELTMOPS 
01284      MOVE GSS-MO-BS-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTMOPS 
01285          CMF-CODE-VALUE.                                          ELTMOPS 
01286      MOVE 'MO-BS-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTMOPS 
01287      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
01288      EJECT                                                        ELTMOPS 
01289                                                                   ELTMOPS 
01290                                                                   ELTMOPS 
01291 ************************************************************      ELTMOPS 
01292 *                                                          *      ELTMOPS 
01293 *        CREATE SUPPLEMENTAL SCREEN                        *      ELTMOPS 
01294 *                                                          *      ELTMOPS 
01295 ************************************************************      ELTMOPS 
01296  CREATE-SUPPLEMENTAL-SCREEN.                                      ELTMOPS 
01297      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTMOPS 
01298      MOVE WS-SUPP TO WS-HEADER-LINE-BCBSMM.                       ELTMOPS 
01299      IF GSS-MO-MM-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTMOPS 
01300          ZEROES                                                   ELTMOPS 
01301                 AND LOW-VALUES                                    ELTMOPS 
01302          PERFORM GENERATE-SUPPLEMENTAL-TEXT                       ELTMOPS 
01303      ELSE                                                         ELTMOPS 
01304          PERFORM GENERATE-SUPPLEMENTAL-NOT-APPL.                  ELTMOPS 
01305      EJECT                                                        ELTMOPS 
01306                                                                   ELTMOPS 
01307                                                                   ELTMOPS 
01308 ************************************************************      ELTMOPS 
01309 *                                                          *      ELTMOPS 
01310 *        GENERATE SUPPLEMENTAL TEXT                        *      ELTMOPS 
01311 *                                                          *      ELTMOPS 
01312 ************************************************************      ELTMOPS 
01313  GENERATE-SUPPLEMENTAL-TEXT.                                      ELTMOPS 
01314      PERFORM EJECT-NEW-PAGE.                                      ELTMOPS 
01315      PERFORM TRANSLATE-PARTICIPATION-IND.                         ELTMOPS 
01316      IF GSS-MO-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTMOPS 
01317          SPACES                                                   ELTMOPS 
01318                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
01319          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMOPS 
01320      PERFORM TRANSLATE-MM-IND.                                    ELTMOPS 
01321      IF ((GSS-MO-MM-IP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL     ELTMOPS 
01322          SPACES                                                   ELTMOPS 
01323                  AND ZEROES AND LOW-VALUES)) OR                   ELTMOPS 
01324                ((GSS-MO-MM-OP-ALT-PRICING-METH (GSS-INDEX) NOT    ELTMOPS 
01325          EQUAL SPACES                                             ELTMOPS 
01326                  AND ZEROES AND LOW-VALUES))                      ELTMOPS 
01327          PERFORM GENERATE-MM-ALT-PRIC-TEXT.                       ELTMOPS 
01328      SET SRP-ACCUM-PROV-CLASS-SUPP TO TRUE.                       ELTMOPS 
01329      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTMOPS 
01330      IF GSS-MO-MM-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES        ELTMOPS 
01331                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
01332          PERFORM GENERATE-MM-CALCULATION-METHOD.                  ELTMOPS 
01333      IF GSS-MO-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTMOPS 
01334          SPACES                                                   ELTMOPS 
01335                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
01336          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTMOPS 
01337      IF ((GSS-MO-MM-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL         ELTMOPS 
01338          SPACES AND                                               ELTMOPS 
01339                   ZEROES AND LOW-VALUES)) OR                      ELTMOPS 
01340                 ((GSS-MO-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT       ELTMOPS 
01341          EQUAL SPACES AND                                         ELTMOPS 
01342                   ZEROES AND LOW-VALUES)) OR                      ELTMOPS 
01343                ((GSS-MO-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT      ELTMOPS 
01344          EQUAL SPACES AND                                         ELTMOPS 
01345                   ZEROES AND LOW-VALUES))                         ELTMOPS 
01346          PERFORM GENERATE-MM-BENE-REDUCT-TEXT.                    ELTMOPS 
01347      PERFORM GENERATE-OTHER-TABULAR-INFO.                         ELTMOPS 
01348      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTMOPS 
01349                                                                   ELTMOPS 
01350                                                                   ELTMOPS 
01351 ************************************************************      ELTMOPS 
01352 *                                                          *      ELTMOPS 
01353 *        GENERATE SUPPLEMENTAL NOT APPLICABLE              *      ELTMOPS 
01354 *                                                          *      ELTMOPS 
01355 ************************************************************      ELTMOPS 
01356  GENERATE-SUPPLEMENTAL-NOT-APPL.                                  ELTMOPS 
01357      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES ).            ELTMOPS 
01358      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMOPS 
01359      MOVE WS-NOT-APPLY-TO-LOB-MM TO COF-DTL-LINE                  ELTMOPS 
01360          (COF-NBR-DTL-LINES).                                     ELTMOPS 
01361      PERFORM EJECT-NEW-PAGE.                                      ELTMOPS 
01362      EJECT                                                        ELTMOPS 
01363                                                                   ELTMOPS 
01364                                                                   ELTMOPS 
01365 ************************************************************      ELTMOPS 
01366 *                                                          *      ELTMOPS 
01367 *        TRANSLATE MM IND                                  *      ELTMOPS 
01368 *                                                          *      ELTMOPS 
01369 ************************************************************      ELTMOPS 
01370  TRANSLATE-MM-IND.                                                ELTMOPS 
01371      INITIALIZE TCAR-FROM-AREA.                                   ELTMOPS 
01372      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
01373      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMOPS 
01374      SET PERIOD-NEEDED TO TRUE.                                   ELTMOPS 
01375      MOVE GSS-MO-MM-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTMOPS 
01376      MOVE   'MO-MM-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTMOPS 
01377      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
01378      EJECT                                                        ELTMOPS 
01379                                                                   ELTMOPS 
01380                                                                   ELTMOPS 
01381 ************************************************************      ELTMOPS 
01382 *                                                          *      ELTMOPS 
01383 *        GENERATE MM CALCULATION METHOD                    *      ELTMOPS 
01384 *                                                          *      ELTMOPS 
01385 ************************************************************      ELTMOPS 
01386  GENERATE-MM-CALCULATION-METHOD.                                  ELTMOPS 
01387      INITIALIZE TCAR-FROM-AREA.                                   ELTMOPS 
01388      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
01389      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMOPS 
01390      SET PERIOD-NEEDED TO TRUE.                                   ELTMOPS 
01391      MOVE GSS-MO-MM-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMOPS 
01392      MOVE   'MO-MM-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELTMOPS 
01393      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
01394      EJECT                                                        ELTMOPS 
01395                                                                   ELTMOPS 
01396                                                                   ELTMOPS 
01397 ************************************************************      ELTMOPS 
01398 *                                                          *      ELTMOPS 
01399 *        GENERATE MM ALT PRIC TEXT                         *      ELTMOPS 
01400 *                                                          *      ELTMOPS 
01401 ************************************************************      ELTMOPS 
01402  GENERATE-MM-ALT-PRIC-TEXT.                                       ELTMOPS 
01403      INITIALIZE TCAR-FROM-AREA.                                   ELTMOPS 
01404      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
01405      INITIALIZE WS-PERIOD-SWITCH.                                 ELTMOPS 
01406      MOVE WS-ALT-PRICING-LINE-MM TO TCAR-FROM-LINE                ELTMOPS 
01407          (TCAR-FROM-SUB).                                         ELTMOPS 
01408      ADD +1 TO TCAR-FROM-SUB.                                     ELTMOPS 
01409      SET ADDITIONAL-TEXT TO TRUE.                                 ELTMOPS 
01410      IF GSS-MO-MM-IP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL       ELTMOPS 
01411          SPACES                                                   ELTMOPS 
01412                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
01413          PERFORM TRANSLATE-MM-IP-ALT-PRIC-METH.                   ELTMOPS 
01414      IF GSS-MO-MM-OP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL       ELTMOPS 
01415          SPACES                                                   ELTMOPS 
01416                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
01417          PERFORM TRANSLATE-MM-OP-ALT-PRIC-METH.                   ELTMOPS 
01418      EJECT                                                        ELTMOPS 
01419                                                                   ELTMOPS 
01420                                                                   ELTMOPS 
01421 ************************************************************      ELTMOPS 
01422 *                                                          *      ELTMOPS 
01423 *        TRANSLATE MM IP ALT PRIC METH                     *      ELTMOPS 
01424 *                                                          *      ELTMOPS 
01425 ************************************************************      ELTMOPS 
01426  TRANSLATE-MM-IP-ALT-PRIC-METH.                                   ELTMOPS 
01427      MOVE GSS-MO-MM-IP-ALT-PRICING-METH (GSS-INDEX) TO            ELTMOPS 
01428          CMF-CODE-VALUE.                                          ELTMOPS 
01429      MOVE 'MO-MM-IP-ALT-PRICING-METH' TO CMF-ELEMENT-SYSTEM-NAME. ELTMOPS 
01430      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
01431      IF GSS-MO-MM-OP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL       ELTMOPS 
01432          SPACES                                                   ELTMOPS 
01433                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
01434          PERFORM CONTINUE-ALT-PRIC-SENTENCE                       ELTMOPS 
01435      ELSE                                                         ELTMOPS 
01436          PERFORM FINISH-ALT-PRIC-SENTENCE.                        ELTMOPS 
01437                                                                   ELTMOPS 
01438                                                                   ELTMOPS 
01439 ************************************************************      ELTMOPS 
01440 *                                                          *      ELTMOPS 
01441 *        TRANSLATE MM OP ALT PRIC METH                     *      ELTMOPS 
01442 *                                                          *      ELTMOPS 
01443 ************************************************************      ELTMOPS 
01444  TRANSLATE-MM-OP-ALT-PRIC-METH.                                   ELTMOPS 
01445      MOVE GSS-MO-MM-OP-ALT-PRICING-METH (GSS-INDEX) TO            ELTMOPS 
01446          CMF-CODE-VALUE.                                          ELTMOPS 
01447      MOVE 'MO-MM-OP-ALT-PRICING-METH' TO CMF-ELEMENT-SYSTEM-NAME. ELTMOPS 
01448      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
01449      MOVE WS-OUTPATIENT TO TCAR-FROM-LINE (TCAR-FROM-SUB).        ELTMOPS 
01450      PERFORM FINISH-SENTENCE.                                     ELTMOPS 
01451      EJECT                                                        ELTMOPS 
01452                                                                   ELTMOPS 
01453                                                                   ELTMOPS 
01454 ************************************************************      ELTMOPS 
01455 *                                                          *      ELTMOPS 
01456 *        GENERATE MM BENE REDUCT TEXT                      *      ELTMOPS 
01457 *                                                          *      ELTMOPS 
01458 ************************************************************      ELTMOPS 
01459  GENERATE-MM-BENE-REDUCT-TEXT.                                    ELTMOPS 
01460      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMOPS 
01461      MOVE WS-BENE-REDUCT-LINE TO COF-DTL-LINE                     ELTMOPS 
01462          (COF-NBR-DTL-LINES).                                     ELTMOPS 
01463      PERFORM LINK-TO-OUTPUT.                                      ELTMOPS 
01464      INITIALIZE ADDITIONAL-TEXT-SWITCH.                           ELTMOPS 
01465      INITIALIZE WS-PERIOD-SWITCH.                                 ELTMOPS 
01466      INITIALIZE TCAR-FROM-AREA.                                   ELTMOPS 
01467      IF GSS-MO-MM-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMOPS 
01468          SPACES                                                   ELTMOPS 
01469                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
01470          PERFORM TRANSLATE-MM-DEDUCT-IND.                         ELTMOPS 
01471      IF GSS-MO-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMOPS 
01472          SPACES                                                   ELTMOPS 
01473                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
01474          PERFORM TRANSLATE-MM-OPEX-APPLIC.                        ELTMOPS 
01475      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMOPS 
01476      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMOPS 
01477      PERFORM LINK-TO-OUTPUT.                                      ELTMOPS 
01478      IF GSS-MO-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTMOPS 
01479          SPACES                                                   ELTMOPS 
01480                 AND ZEROES AND LOW-VALUES                         ELTMOPS 
01481          PERFORM TRANSLATE-MM-OPEX-OVERRIDE.                      ELTMOPS 
01482      EJECT                                                        ELTMOPS 
01483                                                                   ELTMOPS 
01484                                                                   ELTMOPS 
01485 ************************************************************      ELTMOPS 
01486 *                                                          *      ELTMOPS 
01487 *        TRANSLATE MM DEDUCT IND                           *      ELTMOPS 
01488 *                                                          *      ELTMOPS 
01489 ************************************************************      ELTMOPS 
01490  TRANSLATE-MM-DEDUCT-IND.                                         ELTMOPS 
01491      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
01492      MOVE GSS-MO-MM-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTMOPS 
01493          CMF-CODE-VALUE.                                          ELTMOPS 
01494      MOVE 'MO-MM-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMOPS 
01495      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
01496      EJECT                                                        ELTMOPS 
01497                                                                   ELTMOPS 
01498                                                                   ELTMOPS 
01499 ************************************************************      ELTMOPS 
01500 *                                                          *      ELTMOPS 
01501 *        TRANSLATE MM OPEX APPLIC                          *      ELTMOPS 
01502 *                                                          *      ELTMOPS 
01503 ************************************************************      ELTMOPS 
01504  TRANSLATE-MM-OPEX-APPLIC.                                        ELTMOPS 
01505      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
01506      MOVE GSS-MO-MM-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTMOPS 
01507          CMF-CODE-VALUE.                                          ELTMOPS 
01508      MOVE 'MO-MM-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMOPS 
01509      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
01510      EJECT                                                        ELTMOPS 
01511                                                                   ELTMOPS 
01512                                                                   ELTMOPS 
01513 ************************************************************      ELTMOPS 
01514 *                                                          *      ELTMOPS 
01515 *        TRANSLATE MM OPEX OVERRIDE                        *      ELTMOPS 
01516 *                                                          *      ELTMOPS 
01517 ************************************************************      ELTMOPS 
01518  TRANSLATE-MM-OPEX-OVERRIDE.                                      ELTMOPS 
01519      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
01520      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMOPS 
01521      SET PERIOD-NEEDED TO TRUE.                                   ELTMOPS 
01522      MOVE GSS-MO-MM-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTMOPS 
01523          CMF-CODE-VALUE.                                          ELTMOPS 
01524      MOVE 'MO-MM-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTMOPS 
01525      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
01526      EJECT                                                        ELTMOPS 
01527                                                                   ELTMOPS 
01528                                                                   ELTMOPS 
01529 ************************************************************      ELTMOPS 
01530 *                                                          *      ELTMOPS 
01531 *        GENERATE SPILL OVER TEXT                          *      ELTMOPS 
01532 *                                                          *      ELTMOPS 
01533 ************************************************************      ELTMOPS 
01534  GENERATE-SPILL-OVER-TEXT.                                        ELTMOPS 
01535      INITIALIZE TCAR-FROM-AREA.                                   ELTMOPS 
01536      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
01537      MOVE WS-SPILL-OVER-LINE TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELTMOPS 
01538      ADD  +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
01539      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMOPS 
01540      SET PERIOD-NEEDED TO TRUE.                                   ELTMOPS 
01541      MOVE GSS-MO-SPILL-OVER-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMOPS 
01542      MOVE 'MO-SPILL-OVER-IND' TO CMF-ELEMENT-SYSTEM-NAME.         ELTMOPS 
01543      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTMOPS 
01544      EJECT                                                        ELTMOPS 
01545                                                                   ELTMOPS 
01546                                                                   ELTMOPS 
01547 ************************************************************      ELTMOPS 
01548 *                                                          *      ELTMOPS 
01549 *        GENERATE OTHER TABULAR INFO                       *      ELTMOPS 
01550 *                                                          *      ELTMOPS 
01551 ************************************************************      ELTMOPS 
01552  GENERATE-OTHER-TABULAR-INFO.                                     ELTMOPS 
01553      PERFORM GENERATE-RELATED-SERVICES-SENT.                      ELTMOPS 
01554      PERFORM GENERATE-SPECIAL-PROCEDURES-SE.                      ELTMOPS 
01555                                                                   ELTMOPS 
01556                                                                   ELTMOPS 
01557 ************************************************************      ELTMOPS 
01558 *                                                          *      ELTMOPS 
01559 *        GENERATE RELATED SERVICES SENTENCE                *      ELTMOPS 
01560 *                                                          *      ELTMOPS 
01561 ************************************************************      ELTMOPS 
01562  GENERATE-RELATED-SERVICES-SENT.                                  ELTMOPS 
01563      SET GCG-INDEX TO +1.                                         ELTMOPS 
01564      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTMOPS 
01565         AT END                                                    ELTMOPS 
01566              MOVE ZEROES TO WS-GMPB-PROV-SLOT-NO                  ELTMOPS 
01567         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GMPB                     ELTMOPS 
01568              MOVE GCG-TAB-ID (GCG-INDEX)                          ELTMOPS 
01569                  TO WS-GMPB-PROV-ID                               ELTMOPS 
01570              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTMOPS 
01571                  TO WS-GMPB-PROV-SLOT-NO                          ELTMOPS 
01572         END-SEARCH.                                               ELTMOPS 
01573      IF WS-GMPB-PROV-ID EQUAL WS-GMPB                             ELTMOPS 
01574                 AND WS-GMPB-PROV-SLOT-NO GREATER THAN             ELTMOPS 
01575          ZEROES                                                   ELTMOPS 
01576          PERFORM DISPLAY-RELATED-SERVICES-SENTE.                  ELTMOPS 
01577      EJECT                                                        ELTMOPS 
01578                                                                   ELTMOPS 
01579                                                                   ELTMOPS 
01580 ************************************************************      ELTMOPS 
01581 *                                                          *      ELTMOPS 
01582 *        DISPLAY RELATED SERVICES SENTENCE                 *      ELTMOPS 
01583 *                                                          *      ELTMOPS 
01584 ************************************************************      ELTMOPS 
01585  DISPLAY-RELATED-SERVICES-SENTE.                                  ELTMOPS 
01586      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMOPS 
01587      MOVE WS-SPEC-SERV-MSG  TO COF-DTL-LINE                       ELTMOPS 
01588          (COF-NBR-DTL-LINES).                                     ELTMOPS 
01589      PERFORM LINK-TO-OUTPUT.                                      ELTMOPS 
01590      PERFORM GENERATE-RELATED-SERVICES-TEXT.                      ELTMOPS 
01591      EJECT                                                        ELTMOPS 
01592                                                                   ELTMOPS 
01593                                                                   ELTMOPS 
01594 ************************************************************      ELTMOPS 
01595 *                                                          *      ELTMOPS 
01596 *        GENERATE SPECIAL PROCEDURES SENTENCE              *      ELTMOPS 
01597 *                                                          *      ELTMOPS 
01598 ************************************************************      ELTMOPS 
01599  GENERATE-SPECIAL-PROCEDURES-SE.                                  ELTMOPS 
01600      SET GCG-INDEX TO +1.                                         ELTMOPS 
01601      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTMOPS 
01602         AT END                                                    ELTMOPS 
01603              MOVE ZEROES TO WS-GMPR-PROV-SLOT-NO                  ELTMOPS 
01604         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GMPR                     ELTMOPS 
01605              MOVE GCG-TAB-ID (GCG-INDEX)                          ELTMOPS 
01606                  TO WS-GMPR-PROV-ID                               ELTMOPS 
01607              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTMOPS 
01608                  TO WS-GMPR-PROV-SLOT-NO                          ELTMOPS 
01609         END-SEARCH.                                               ELTMOPS 
01610      IF WS-GMPR-PROV-ID EQUAL WS-GMPR                             ELTMOPS 
01611                 AND WS-GMPR-PROV-SLOT-NO GREATER THAN             ELTMOPS 
01612          ZEROES                                                   ELTMOPS 
01613          PERFORM DISPLAY-SPECIAL-PROCEDURES-SEN.                  ELTMOPS 
01614                                                                   ELTMOPS 
01615                                                                   ELTMOPS 
01616 ************************************************************      ELTMOPS 
01617 *                                                          *      ELTMOPS 
01618 *        DISPLAY SPECIAL PROCEDURES SENTENCE               *      ELTMOPS 
01619 *                                                          *      ELTMOPS 
01620 ************************************************************      ELTMOPS 
01621  DISPLAY-SPECIAL-PROCEDURES-SEN.                                  ELTMOPS 
01622      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMOPS 
01623      MOVE WS-SPEC-PROC-MSG   TO  COF-DTL-LINE                     ELTMOPS 
01624          (COF-NBR-DTL-LINES).                                     ELTMOPS 
01625      PERFORM LINK-TO-OUTPUT.                                      ELTMOPS 
01626      PERFORM GENERATE-SPECIAL-PROCEDURES-TE.                      ELTMOPS 
01627      EJECT                                                        ELTMOPS 
01628                                                                   ELTMOPS 
01629                                                                   ELTMOPS 
01630 ************************************************************      ELTMOPS 
01631 *                                                          *      ELTMOPS 
01632 *        SEARCH THE GSS ENTRY                              *      ELTMOPS 
01633 *                                                          *      ELTMOPS 
01634 ************************************************************      ELTMOPS 
01635  SEARCH-THE-GSS-ENTRY.                                            ELTMOPS 
01636      SET GSS-INDEX TO 1.                                          ELTMOPS 
01637      SEARCH GSS-ENTRY                                             ELTMOPS 
01638          AT END                                                   ELTMOPS 
01639               SET TABULAR-IS-UNDEFINED TO TRUE                    ELTMOPS 
01640          WHEN GSS-MO-PROG-CODE-CHR (GSS-INDEX)                    ELTMOPS 
01641               CONTINUE                                            ELTMOPS 
01642         END-SEARCH.                                               ELTMOPS 
01643      IF TABULAR-IS-UNDEFINED                                      ELTMOPS 
01644          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTMOPS 
01645                                                                   ELTMOPS 
01646                                                                   ELTMOPS 
01647 ************************************************************      ELTMOPS 
01648 *                                                          *      ELTMOPS 
01649 *        LINK TO CODES MANUAL INTERFACE                    *      ELTMOPS 
01650 *                                                          *      ELTMOPS 
01651 ************************************************************      ELTMOPS 
01652  LINK-TO-CODES-MANUAL-INTERFACE.                                  ELTMOPS 
01653      MOVE WS-GCCP TO CMF-RECORD-PREFIX.                           ELTMOPS 
01654      EXEC CICS LINK                                               ELTMOPS 
01655                PROGRAM ('ELUCMIF')                                ELTMOPS 
01656                COMMAREA (DFHCOMMAREA)                             ELTMOPS 
01657        END-EXEC.                                                  ELTMOPS 
01658                                                                   ELTMOPS 
01659                                                                   ELTMOPS 
01660 ************************************************************      ELTMOPS 
01661 *                                                          *      ELTMOPS 
01662 *        LINK TO GROUP CODES MANUAL INTERFACE              *      ELTMOPS 
01663 *                                                          *      ELTMOPS 
01664 ************************************************************      ELTMOPS 
01665  LINK-TO-GROUP-CODES-MANUAL-INT.                                  ELTMOPS 
01666      MOVE WS-GROUP TO CMF-RECORD-PREFIX.                          ELTMOPS 
01667      EXEC CICS LINK                                               ELTMOPS 
01668                PROGRAM ('ELUCMIF')                                ELTMOPS 
01669                COMMAREA (DFHCOMMAREA)                             ELTMOPS 
01670        END-EXEC.                                                  ELTMOPS 
01671      EJECT                                                        ELTMOPS 
01672                                                                   ELTMOPS 
01673                                                                   ELTMOPS 
01674 ************************************************************      ELTMOPS 
01675 *                                                          *      ELTMOPS 
01676 *        GET GCCP TABULAR                                  *      ELTMOPS 
01677 *                                                          *      ELTMOPS 
01678 ************************************************************      ELTMOPS 
01679  GET-GCCP-TABULAR.                                                ELTMOPS 
01680      PERFORM ESTABLISH-ADDRESSY-OF-GCTABULA.                      ELTMOPS 
01681      SET IOP-RD              TO TRUE.                             ELTMOPS 
01682      SET IOP-STG-MODE-MOVE   TO TRUE.                             ELTMOPS 
01683      SET IOP-FCQ-NONE        TO TRUE.                             ELTMOPS 
01684      SET IOP-KVQ-EQ          TO TRUE.                             ELTMOPS 
01685      MOVE KWA-GCTABULR-KEY   TO IOP-FILE-KEY.                     ELTMOPS 
01686      PERFORM CALL-INPUT-OUTPUT-MODULE.                            ELTMOPS 
01687      IF IOP-RC-OK                                                 ELTMOPS 
01688          PERFORM ESTABLISH-ADDRESSABILITY-OF-GC                   ELTMOPS 
01689      ELSE IF IOP-RC-NOTFND                                        ELTMOPS 
01690          PERFORM SIGNAL-NOT-FOUND-GCTAB-ERROR                     ELTMOPS 
01691      ELSE                                                         ELTMOPS 
01692          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTMOPS 
01693      EJECT                                                        ELTMOPS 
01694                                                                   ELTMOPS 
01695                                                                   ELTMOPS 
01696 ************************************************************      ELTMOPS 
01697 *                                                          *      ELTMOPS 
01698 *        ESTABLISH ADDRESSY OF GCTABULAR IO PARAMETER BLOCK*      ELTMOPS 
01699 *                                                          *      ELTMOPS 
01700 ************************************************************      ELTMOPS 
01701  ESTABLISH-ADDRESSY-OF-GCTABULA.                                  ELTMOPS 
01702      SET CIA-GCTABULR-DDN TO TRUE.                                ELTMOPS 
01703      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMOPS 
01704                            ADDRESS OF                             ELTMOPS 
01705          IOP-INPUT-OUTPUT-PARAMETERS.                             ELTMOPS 
01706      EJECT                                                        ELTMOPS 
01707                                                                   ELTMOPS 
01708                                                                   ELTMOPS 
01709 ************************************************************      ELTMOPS 
01710 *                                                          *      ELTMOPS 
01711 *        CALL INPUT OUTPUT MODULE                          *      ELTMOPS 
01712 *                                                          *      ELTMOPS 
01713 ************************************************************      ELTMOPS 
01714  CALL-INPUT-OUTPUT-MODULE.                                        ELTMOPS 
01715      EXEC CICS LINK                                               ELTMOPS 
01716                PROGRAM ('ELUIOPGM')                               ELTMOPS 
01717                COMMAREA (DFHCOMMAREA)                             ELTMOPS 
01718        END-EXEC.                                                  ELTMOPS 
01719      EJECT                                                        ELTMOPS 
01720                                                                   ELTMOPS 
01721                                                                   ELTMOPS 
01722 ************************************************************      ELTMOPS 
01723 *                                                          *      ELTMOPS 
01724 *        ESTABLISH ADDRESSABILITY OF GCCP TABULAR          *      ELTMOPS 
01725 *                                                          *      ELTMOPS 
01726 ************************************************************      ELTMOPS 
01727  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTMOPS 
01728      SET ADDRESS OF GCCP-TABULAR-REC-AREA TO                      ELTMOPS 
01729          IOP-REC-PTR.                                             ELTMOPS 
01730      SET IOP-REC-PTR TO NULL.                                     ELTMOPS 
01731      EJECT                                                        ELTMOPS 
01732                                                                   ELTMOPS 
01733                                                                   ELTMOPS 
01734 ************************************************************      ELTMOPS 
01735 *                                                          *      ELTMOPS 
01736 *        SIGNAL NOT FOUND GCTAB ERROR                      *      ELTMOPS 
01737 *                                                          *      ELTMOPS 
01738 ************************************************************      ELTMOPS 
01739  SIGNAL-NOT-FOUND-GCTAB-ERROR.                                    ELTMOPS 
01740      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTMOPS 
01741      PERFORM SIGNAL-ABEND.                                        ELTMOPS 
01742      EJECT                                                        ELTMOPS 
01743                                                                   ELTMOPS 
01744                                                                   ELTMOPS 
01745 ************************************************************      ELTMOPS 
01746 *                                                          *      ELTMOPS 
01747 *        SIGNAL CRITICAL IO ERROR                          *      ELTMOPS 
01748 *                                                          *      ELTMOPS 
01749 ************************************************************      ELTMOPS 
01750  SIGNAL-CRITICAL-IO-ERROR.                                        ELTMOPS 
01751      SET CIA-AB-CRITIO TO TRUE.                                   ELTMOPS 
01752      PERFORM SIGNAL-ABEND.                                        ELTMOPS 
01753                                                                   ELTMOPS 
01754                                                                   ELTMOPS 
01755 ************************************************************      ELTMOPS 
01756 *                                                          *      ELTMOPS 
01757 *        GENERATE RELATED SERVICES TEXT                    *      ELTMOPS 
01758 *                                                          *      ELTMOPS 
01759 ************************************************************      ELTMOPS 
01760  GENERATE-RELATED-SERVICES-TEXT.                                  ELTMOPS 
01761      MOVE 'MANDATORY OUTPATIENT SURGERY' TO SRP-CCP-NAME.         ELTMOPS 
01762      MOVE  WS-GMPB-PROV-ID               TO SRP-TABULAR-ID.       ELTMOPS 
01763      MOVE  WS-GMPB-PROV-SLOT-NO          TO SRP-TABULAR-SLOT-NO.  ELTMOPS 
01764      PERFORM CALL-RELATED-SERVICES-GENERATO.                      ELTMOPS 
01765                                                                   ELTMOPS 
01766                                                                   ELTMOPS 
01767 ************************************************************      ELTMOPS 
01768 *                                                          *      ELTMOPS 
01769 *        CALL RELATED SERVICES GENERATOR                   *      ELTMOPS 
01770 *                                                          *      ELTMOPS 
01771 ************************************************************      ELTMOPS 
01772  CALL-RELATED-SERVICES-GENERATO.                                  ELTMOPS 
01773      EXEC CICS LINK                                               ELTMOPS 
01774                PROGRAM ('ELGGXXB')                                ELTMOPS 
01775                COMMAREA (DFHCOMMAREA)                             ELTMOPS 
01776         END-EXEC.                                                 ELTMOPS 
01777      EJECT                                                        ELTMOPS 
01778                                                                   ELTMOPS 
01779                                                                   ELTMOPS 
01780 ************************************************************      ELTMOPS 
01781 *                                                          *      ELTMOPS 
01782 *        GENERATE SPECIAL PROCEDURES TEXT                  *      ELTMOPS 
01783 *                                                          *      ELTMOPS 
01784 ************************************************************      ELTMOPS 
01785  GENERATE-SPECIAL-PROCEDURES-TE.                                  ELTMOPS 
01786      MOVE 'MANDATORY OUTPATIENT SURGERY' TO                       ELTMOPS 
01787          SRP-CCP-NAME.                                            ELTMOPS 
01788      MOVE  WS-GMPR-PROV-ID               TO SRP-TABULAR-ID.       ELTMOPS 
01789      MOVE  WS-GMPR-PROV-SLOT-NO          TO SRP-TABULAR-SLOT-NO.  ELTMOPS 
01790      PERFORM CALL-SPECIAL-PROCEDURES-GENERA.                      ELTMOPS 
01791                                                                   ELTMOPS 
01792                                                                   ELTMOPS 
01793 ************************************************************      ELTMOPS 
01794 *                                                          *      ELTMOPS 
01795 *        CALL SPECIAL PROCEDURES GENERATOR                 *      ELTMOPS 
01796 *                                                          *      ELTMOPS 
01797 ************************************************************      ELTMOPS 
01798  CALL-SPECIAL-PROCEDURES-GENERA.                                  ELTMOPS 
01799      EXEC CICS LINK                                               ELTMOPS 
01800                PROGRAM ('ELGGXXR')                                ELTMOPS 
01801                COMMAREA (DFHCOMMAREA)                             ELTMOPS 
01802         END-EXEC.                                                 ELTMOPS 
01803                                                                   ELTMOPS 
01804                                                                   ELTMOPS 
01805 ************************************************************      ELTMOPS 
01806 *                                                          *      ELTMOPS 
01807 *        GENERATE COINSURANCE                              *      ELTMOPS 
01808 *                                                          *      ELTMOPS 
01809 ************************************************************      ELTMOPS 
01810  GENERATE-COINSURANCE.                                            ELTMOPS 
01811      EXEC CICS LINK                                               ELTMOPS 
01812                PROGRAM ('ELGACLCC')                               ELTMOPS 
01813                COMMAREA (DFHCOMMAREA)                             ELTMOPS 
01814         END-EXEC.                                                 ELTMOPS 
01815                                                                   ELTMOPS 
01816 ************************************************************      ELTMOPS 
01817 *                                                          *      ELTMOPS 
01818 *        GENERATE COPAY                                    *      ELTMOPS 
01819 *                                                          *      ELTMOPS 
01820 ************************************************************      ELTMOPS 
01821  GENERATE-COPAY.                                                  ELTMOPS 
01822      EXEC CICS LINK                                               ELTMOPS 
01823                PROGRAM ('ELGACPCC')                               ELTMOPS 
01824                COMMAREA (DFHCOMMAREA)                             ELTMOPS 
01825         END-EXEC.                                                 ELTMOPS 
01826                                                                   ELTMOPS 
01827                                                                   ELTMOPS 
01828 ************************************************************      ELTMOPS 
01829 *                                                          *      ELTMOPS 
01830 *        GENERATE DEDUCTIBLE                               *      ELTMOPS 
01831 *                                                          *      ELTMOPS 
01832 ************************************************************      ELTMOPS 
01833  GENERATE-DEDUCTIBLE.                                             ELTMOPS 
01834      EXEC CICS LINK                                               ELTMOPS 
01835                PROGRAM ('ELGADLCC')                               ELTMOPS 
01836                COMMAREA (DFHCOMMAREA)                             ELTMOPS 
01837         END-EXEC.                                                 ELTMOPS 
01838                                                                   ELTMOPS 
01839                                                                   ELTMOPS 
01840 ************************************************************      ELTMOPS 
01841 *                                                          *      ELTMOPS 
01842 *        GENERATE MAXIMUM                                  *      ELTMOPS 
01843 *                                                          *      ELTMOPS 
01844 ************************************************************      ELTMOPS 
01845  GENERATE-MAXIMUM.                                                ELTMOPS 
01846      EXEC CICS LINK                                               ELTMOPS 
01847                PROGRAM ('ELGABMCC')                               ELTMOPS 
01848                COMMAREA (DFHCOMMAREA)                             ELTMOPS 
01849         END-EXEC.                                                 ELTMOPS 
01850                                                                   ELTMOPS 
01851                                                                   ELTMOPS 
01852 ************************************************************      ELTMOPS 
01853 *                                                          *      ELTMOPS 
01854 *        CALL CBRI INTERFACE                               *      ELTMOPS 
01855 *                                                          *      ELTMOPS 
01856 ************************************************************      ELTMOPS 
01857  CALL-CBRI-INTERFACE.                                             ELTMOPS 
01858      CALL 'ELGCBRI' USING DFHEIBLK                                ELTMOPS 
01859                           DFHCOMMAREA.                            ELTMOPS 
01860      EJECT                                                        ELTMOPS 
01861                                                                   ELTMOPS 
01862                                                                   ELTMOPS 
01863 ************************************************************      ELTMOPS 
01864 *                                                          *      ELTMOPS 
01865 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTMOPS 
01866 *                                                          *      ELTMOPS 
01867 ************************************************************      ELTMOPS 
01868  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTMOPS 
01869      PERFORM INITIALIZE-CMOUT.                                    ELTMOPS 
01870      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTMOPS 
01871                                                                   ELTMOPS 
01872                                                                   ELTMOPS 
01873 ************************************************************      ELTMOPS 
01874 *                                                          *      ELTMOPS 
01875 *        PREPARE TEXT FOR OUTPUT                           *      ELTMOPS 
01876 *                                                          *      ELTMOPS 
01877 ************************************************************      ELTMOPS 
01878  PREPARE-TEXT-FOR-OUTPUT.                                         ELTMOPS 
01879      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTMOPS 
01880          UNTIL CMF-DESCR-IDX                                      ELTMOPS 
01881                                    GREATER THAN                   ELTMOPS 
01882              CMF-NBR-DESCR-LINES.                                 ELTMOPS 
01883                                                                   ELTMOPS 
01884                                                                   ELTMOPS 
01885 ************************************************************      ELTMOPS 
01886 *                                                          *      ELTMOPS 
01887 *        INITIALIZE CMOUT                                  *      ELTMOPS 
01888 *                                                          *      ELTMOPS 
01889 ************************************************************      ELTMOPS 
01890  INITIALIZE-CMOUT.                                                ELTMOPS 
01891      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTMOPS 
01892      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMOPS 
01893          ADDRESS OF CMF-DESCR.                                    ELTMOPS 
01894      SET CMF-DESCR-IDX TO 1.                                      ELTMOPS 
01895      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTMOPS 
01896                                                                   ELTMOPS 
01897                                                                   ELTMOPS 
01898 ************************************************************      ELTMOPS 
01899 *                                                          *      ELTMOPS 
01900 *        MOVE CMF TEXT TO OUTPUT                           *      ELTMOPS 
01901 *                                                          *      ELTMOPS 
01902 ************************************************************      ELTMOPS 
01903  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTMOPS 
01904      PERFORM MOVE-A-LINE.                                         ELTMOPS 
01905      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTMOPS 
01906          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTMOPS 
01907      IF TCAR-FROM-SUB GREATER THAN 20                             ELTMOPS 
01908               OR CMF-DESCR-IDX GREATER THAN                       ELTMOPS 
01909          CMF-NBR-DESCR-LINES                                      ELTMOPS 
01910          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTMOPS 
01911      EJECT                                                        ELTMOPS 
01912                                                                   ELTMOPS 
01913                                                                   ELTMOPS 
01914 ************************************************************      ELTMOPS 
01915 *                                                          *      ELTMOPS 
01916 *        FINISH CODES MANUAL TEXT                          *      ELTMOPS 
01917 *                                                          *      ELTMOPS 
01918 ************************************************************      ELTMOPS 
01919  FINISH-CODES-MANUAL-TEXT.                                        ELTMOPS 
01920      SET DONE-PROCESSING TO TRUE.                                 ELTMOPS 
01921      IF PERIOD-NEEDED                                             ELTMOPS 
01922          PERFORM GET-AND-MOVE-PERIOD.                             ELTMOPS 
01923                                                                   ELTMOPS 
01924                                                                   ELTMOPS 
01925 ************************************************************      ELTMOPS 
01926 *                                                          *      ELTMOPS 
01927 *        GET AND MOVE PERIOD                               *      ELTMOPS 
01928 *                                                          *      ELTMOPS 
01929 ************************************************************      ELTMOPS 
01930  GET-AND-MOVE-PERIOD.                                             ELTMOPS 
01931      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTMOPS 
01932          (TCAR-FROM-SUB).                                         ELTMOPS 
01933                                                                   ELTMOPS 
01934                                                                   ELTMOPS 
01935 ************************************************************      ELTMOPS 
01936 *                                                          *      ELTMOPS 
01937 *        SAVE LAST LINE                                    *      ELTMOPS 
01938 *                                                          *      ELTMOPS 
01939 ************************************************************      ELTMOPS 
01940  SAVE-LAST-LINE.                                                  ELTMOPS 
01941      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
01942      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTMOPS 
01943         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTMOPS 
01944      ADD 1 TO TCAR-FROM-SUB.                                      ELTMOPS 
01945      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTMOPS 
01946                                                                   ELTMOPS 
01947                                                                   ELTMOPS 
01948 ************************************************************      ELTMOPS 
01949 *                                                          *      ELTMOPS 
01950 *        OUTPUT LAST LINE                                  *      ELTMOPS 
01951 *                                                          *      ELTMOPS 
01952 ************************************************************      ELTMOPS 
01953  OUTPUT-LAST-LINE.                                                ELTMOPS 
01954      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTMOPS 
01955          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTMOPS 
01956      IF BLANK-LINE-NEEDED                                         ELTMOPS 
01957          PERFORM CREATE-A-BLANK-LINE.                             ELTMOPS 
01958                                                                   ELTMOPS 
01959                                                                   ELTMOPS 
01960 ************************************************************      ELTMOPS 
01961 *                                                          *      ELTMOPS 
01962 *        CREATE A BLANK LINE                               *      ELTMOPS 
01963 *                                                          *      ELTMOPS 
01964 ************************************************************      ELTMOPS 
01965  CREATE-A-BLANK-LINE.                                             ELTMOPS 
01966      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTMOPS 
01967      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMOPS 
01968      EJECT                                                        ELTMOPS 
01969                                                                   ELTMOPS 
01970                                                                   ELTMOPS 
01971 ************************************************************      ELTMOPS 
01972 *                                                          *      ELTMOPS 
01973 *        MOVE A LINE                                       *      ELTMOPS 
01974 *                                                          *      ELTMOPS 
01975 ************************************************************      ELTMOPS 
01976  MOVE-A-LINE.                                                     ELTMOPS 
01977      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTMOPS 
01978          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTMOPS 
01979      SET CMF-DESCR-IDX UP BY 1.                                   ELTMOPS 
01980      ADD 1 TO TCAR-FROM-SUB.                                      ELTMOPS 
01981      EJECT                                                        ELTMOPS 
01982                                                                   ELTMOPS 
01983                                                                   ELTMOPS 
01984 ************************************************************      ELTMOPS 
01985 *                                                          *      ELTMOPS 
01986 *        REFORMAT AND WRITE TEXT                           *      ELTMOPS 
01987 *                                                          *      ELTMOPS 
01988 ************************************************************      ELTMOPS 
01989  REFORMAT-AND-WRITE-TEXT.                                         ELTMOPS 
01990      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTMOPS 
01991      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTMOPS 
01992      PERFORM UNSTRING-TEXT.                                       ELTMOPS 
01993      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMOPS 
01994      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMOPS 
01995      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTMOPS 
01996          UNTIL COF-NBR-DTL-LINES GREATER                          ELTMOPS 
01997                                   TCAR-OUTPUT-FIELDS-USED -       ELTMOPS 
01998              1.                                                   ELTMOPS 
01999      PERFORM DISPOSE-OF-LAST-LINE.                                ELTMOPS 
02000      PERFORM LINK-TO-OUTPUT.                                      ELTMOPS 
02001                                                                   ELTMOPS 
02002                                                                   ELTMOPS 
02003 ************************************************************      ELTMOPS 
02004 *                                                          *      ELTMOPS 
02005 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTMOPS 
02006 *                                                          *      ELTMOPS 
02007 ************************************************************      ELTMOPS 
02008  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTMOPS 
02009      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTMOPS 
02010           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTMOPS 
02011      ADD +1 TO TCAR-FROM-SUB.                                     ELTMOPS 
02012      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMOPS 
02013      EJECT                                                        ELTMOPS 
02014                                                                   ELTMOPS 
02015                                                                   ELTMOPS 
02016 ************************************************************      ELTMOPS 
02017 *                                                          *      ELTMOPS 
02018 *        UNSTRING TEXT                                     *      ELTMOPS 
02019 *                                                          *      ELTMOPS 
02020 ************************************************************      ELTMOPS 
02021  UNSTRING-TEXT.                                                   ELTMOPS 
02022      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTMOPS 
02023      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTMOPS 
02024      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTMOPS 
02025      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTMOPS 
02026      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTMOPS 
02027      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTMOPS 
02028      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTMOPS 
02029      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTMOPS 
02030      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTMOPS 
02031      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTMOPS 
02032      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTMOPS 
02033      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTMOPS 
02034      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTMOPS 
02035      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTMOPS 
02036      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTMOPS 
02037      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTMOPS 
02038      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTMOPS 
02039      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTMOPS 
02040      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTMOPS 
02041      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTMOPS 
02042      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTMOPS 
02043      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTMOPS 
02044                                                                   ELTMOPS 
02045                                                                   ELTMOPS 
02046 ************************************************************      ELTMOPS 
02047 *                                                          *      ELTMOPS 
02048 *        LINK TO OUTPUT                                    *      ELTMOPS 
02049 *                                                          *      ELTMOPS 
02050 ************************************************************      ELTMOPS 
02051  LINK-TO-OUTPUT.                                                  ELTMOPS 
02052      EXEC CICS LINK                                               ELTMOPS 
02053          PROGRAM ('ELUOUTPT')                                     ELTMOPS 
02054          COMMAREA (DFHCOMMAREA)                                   ELTMOPS 
02055          END-EXEC.                                                ELTMOPS 
02056      EJECT                                                        ELTMOPS 
02057                                                                   ELTMOPS 
02058                                                                   ELTMOPS 
02059 ************************************************************      ELTMOPS 
02060 *                                                          *      ELTMOPS 
02061 *        DISPOSE OF LAST LINE                              *      ELTMOPS 
02062 *                                                          *      ELTMOPS 
02063 ************************************************************      ELTMOPS 
02064  DISPOSE-OF-LAST-LINE.                                            ELTMOPS 
02065      IF NOT ADDITIONAL-TEXT                                       ELTMOPS 
02066          PERFORM INITIALIZE-CONTINUED-SW.                         ELTMOPS 
02067      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTMOPS 
02068          PERFORM SAVE-LAST-LINE                                   ELTMOPS 
02069      ELSE                                                         ELTMOPS 
02070          PERFORM OUTPUT-LAST-LINE.                                ELTMOPS 
02071                                                                   ELTMOPS 
02072                                                                   ELTMOPS 
02073 ************************************************************      ELTMOPS 
02074 *                                                          *      ELTMOPS 
02075 *        INITIALIZE CONTINUED SW                           *      ELTMOPS 
02076 *                                                          *      ELTMOPS 
02077 ************************************************************      ELTMOPS 
02078  INITIALIZE-CONTINUED-SW.                                         ELTMOPS 
02079      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTMOPS 
