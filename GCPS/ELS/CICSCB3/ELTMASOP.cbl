00001 *      LAST MAINTENANCE TIME: 10.44.55  DATE: 07/01/91            06/29/02
00002  IDENTIFICATION DIVISION.                                         ELTMASOP
00003                                                                      LV001
00004  PROGRAM-ID.         ELTMASOP.                                    ELTMASOP
00005                                                                   ELTMASOP
00006  AUTHOR.             ANNE KEFFER-KING.                            ELTMASOP
00007                                                                   ELTMASOP
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTMASOP
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELTMASOP
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTMASOP
00011                      233 N. MICHIGAN AVE                          ELTMASOP
00012                      CHICAGO, ILLINOIS 60601                      ELTMASOP
00013                                                                   ELTMASOP
00014  DATE-WRITTEN.       01-JUN-1987.                                 ELTMASOP
00015                                                                   ELTMASOP
00016  DATE-COMPILED.                                                   ELTMASOP
00017                                                                   ELTMASOP
00018  SECURITY.           COPYRIGHT 1986,                              ELTMASOP
00019                      HEALTH CARE SERVICE CORPORATION              ELTMASOP
00020      SKIP3                                                        ELTMASOP
00021  ENVIRONMENT DIVISION.                                            ELTMASOP
00022                                                                   ELTMASOP
00023  CONFIGURATION SECTION.                                           ELTMASOP
00024  SOURCE-COMPUTER.    IBM-3090.                                    ELTMASOP
00025  OBJECT-COMPUTER.    IBM-3090.                                    ELTMASOP
00026      EJECT                                                        ELTMASOP
00027 ******************************************************************ELTMASOP
00028 *                                                                *ELTMASOP
00029 *    COPYBOOK:   XXXXXXXX                                        *ELTMASOP
00030 *    DATE:       99-XXX-9999                                     *ELTMASOP
00031 *    AUTHOR:     X---                                            *ELTMASOP
00032 *    FUNCTION:   X---                                            *ELTMASOP
00033 *                                                                *ELTMASOP
00034 *    NOTES:      X---                                            *ELTMASOP
00035 *                                                                *ELTMASOP
00036 ******************************************************************ELTMASOP
00037 *                                                                *ELTMASOP
00038 *                      MAINTENANCE HISTORY                       *ELTMASOP
00039 *                                                                *ELTMASOP
00040 *  MOD     DATE     BY  DRPT                ACTION               *ELTMASOP
00041 * ----- ----------- --- ----- ---------------------------------- *ELTMASOP
00042 * 01.00 01-JUN-1987 AKK       CREATED                            *ELTMASOP
00043 *                                                                *ELTMASOP
00044 * 01.01 02-NOV-1990 JPB       CHANGED STORAGE MANAGEMENT         *ELTMASOP
00045 *                                                                *ELTMASOP
00046 * 01.02 09-NOV-1990 JPB       CHANGED REFERENCES TO GCG-MAND-    *ELTMASOP
00047 *                             ADDL-SURG-OPN-IND TO ACCOMODATE    *ELTMASOP
00048 *                             FIELD SIZE CHANGES.                *ELTMASOP
00049 *                                                                *ELTMASOP
00050 * 01.03 18-JUN-1991 GEM       ADD CCP PARTIC IND TO MASOP.       *ELTMASOP
00051 *                                                                *ELTMASOP
00052 * 01.04 28-JUN-1991 JPB       FIXED SO APPROVAL SOURCE IS        *ELTMASOP
00053 *                             DISPLAYED.                         *ELTMASOP
00054 *                                                                *ELTMASOP
00055 ******************************************************************ELTMASOP
00056                                                                   ELTMASOP
00057  DATA DIVISION.                                                   ELTMASOP
00058 *ENGLISH CONTRACT ON-LINE SYSTEM.  THIS CONTRACT WILL CREATE      ELTMASOP
00059 *TRANSLATED SCREEN RECORDS FOR DISPLAY OF COST CONTAINMENT TOPIC  ELTMASOP
00060 *MANDATORY ADDITIONAL SECOND OPINION IN ENGLISH LANGUAGE FORMAT.  ELTMASOP
00061 *                                                                 ELTMASOP
00062 *INPUT IS THE GCG GROUP SPECIFIC RECORD AND THE MANDATORY ADD-    ELTMASOP
00063 *ITIONAL SECOND OPINION TABULAR RECORD OF THE GCCP TABULAR.       ELTMASOP
00064 *                                                                 ELTMASOP
00065 *PROGRAM CONTROL - CICS LINK TO:                                  ELTMASOP
00066 *ELUIOPGM - I/O PROGRAM FOR FILES                                 ELTMASOP
00067 *ELUCMIF -  CODE/INDICATOR TRANSLATOR MODULE TO CONVERT CODE/     ELTMASOP
00068 *           INDICATOR TO ENGLIS TEXT BASED ON INFO SUPPLIED       ELTMASOP
00069 *           BY THIS PROGRAM IN ITS INTERFACE AREA.                ELTMASOP
00070 *ELUOUTPT - WRITES SCREEN LINES FOR SCREEN DISPLAY PROGRAM        ELTMASOP
00071 *           BASED ON INFO SUPPLIED BY THIS PROGRAM IN ITS         ELTMASOP
00072 *           INTERFACE AREA.                                       ELTMASOP
00073 *                                                                 ELTMASOP
00074 /                                                                 ELTMASOP
00075  WORKING-STORAGE SECTION.                                         ELTMASOP
00076  01  WS-MISC.                                                     ELTMASOP
00077      05  FILLER             PIC X(42) VALUE                       ELTMASOP
00078      '***ELTMASOP WORKING STORAGE BEGINS HERE***'.                ELTMASOP
00079      05  WS-GMOR-PROC-ID    PIC X(06) VALUE SPACES.               ELTMASOP
00080      05  WS-GMOR-PROC-SLOT-NO                                     ELTMASOP
00081                             PIC S9(04) COMP-3                     ELTMASOP
00082                                       VALUE ZEROES.               ELTMASOP
00083      05  WS-GMOB-SRVS-ID    PIC X(06) VALUE SPACES.               ELTMASOP
00084      05  WS-GMOB-SRVS-SLOT-NO                                     ELTMASOP
00085                             PIC S9(04) COMP-3                     ELTMASOP
00086                                       VALUE ZEROES.               ELTMASOP
00087      05  WS-POINTER2        POINTER.                              ELTMASOP
00088      05  WS-POINTER3        POINTER.                              ELTMASOP
00089 *                                                                 ELTMASOP
00090      05  SCREEN-TYPE        PIC X     VALUE SPACES.               ELTMASOP
00091          88  INSTITUTIONAL-SCREEN     VALUE 'I'.                  ELTMASOP
00092          88  PROFESSIONAL-SCREEN      VALUE 'P'.                  ELTMASOP
00093          88  SUPPLEMENTAL-SCREEN      VALUE 'B'.                  ELTMASOP
00094 *                                                                 ELTMASOP
00095  01  WS-SWITCHES.                                                 ELTMASOP
00096      05  UNDEFINED-TABULAR-SW   PIC X   VALUE 'N'.                ELTMASOP
00097          88  TABULAR-IS-UNDEFINED       VALUE 'Y'.                ELTMASOP
00098 *                                                                 ELTMASOP
00099      05  DEFINED-TABULAR-SW     PIC X   VALUE 'N'.                ELTMASOP
00100          88  TABULAR-IS-DEFINED         VALUE 'Y'.                ELTMASOP
00101 *                                                                 ELTMASOP
00102      05  ADDITIONAL-TEXT-SW     PIC X    VALUE SPACES.            ELTMASOP
00103          88 BLANK-LINE-NEEDED            VALUE 'B'.               ELTMASOP
00104          88 ADDITIONAL-TEXT              VALUE 'Y'.               ELTMASOP
00105 *                                                                 ELTMASOP
00106      05  APPROVAL-SOURCE-SW     PIC X     VALUE 'N'.              ELTMASOP
00107          88  NOT-HOLDING-APPROVAL-SRCE    VALUE 'N'.              ELTMASOP
00108          88  HOLDING-APPROVAL-SOURCE      VALUE 'H'.              ELTMASOP
00109 *                                                                 ELTMASOP
00110      05  CONTINUED-PROCESSING-SW PIC X    VALUE SPACES.           ELTMASOP
00111          88 PROCESSING-CMF-TEXT           VALUE 'P'.              ELTMASOP
00112          88 DONE-PROCESSING               VALUE 'D'.              ELTMASOP
00113 *                                                                 ELTMASOP
00114      05  WS-PERIOD-SW            PIC X    VALUE 'N'.              ELTMASOP
00115          88 PERIOD-NEEDED                 VALUE 'Y'.              ELTMASOP
00116 ******************************************************************ELTMASOP
00117 ***SCREEN BODY LINES                                              ELTMASOP
00118 ***************************************************************** ELTMASOP
00119  01  WS-HDR-LN2.                                                  ELTMASOP
00120      05  FILLER                  PIC X(08) VALUE SPACES.          ELTMASOP
00121      05  FILLER                  PIC X(50) VALUE                  ELTMASOP
00122          '            MANDATORY ADDITIONAL SURGICAL OPINION '.    ELTMASOP
00123      05  HDR-TITLE               PIC X(13) VALUE SPACES.          ELTMASOP
00124      05  FILLER                  PIC X(08) VALUE SPACES.          ELTMASOP
00125 *                                                                 ELTMASOP
00126  01  WS-MASOP-APPLIES.                                            ELTMASOP
00127      05  FILLER                  PIC  X(60) VALUE                 ELTMASOP
00128          'THE MANDATORY ADDITIONAL SURGICAL OPINION PROGRAM APPLIEELTMASOP
00129 -        'S TO'.                                                  ELTMASOP
00130      05  FILLER                  PIC  X(19) VALUE SPACES.         ELTMASOP
00131 *                                                                 ELTMASOP
00132  01  WS-APPROVAL-SOURCE.                                          ELTMASOP
00133      05  FILLER                  PIC X(55) VALUE                  ELTMASOP
00134        'MANDATORY ADDITIONAL SURGICAL OPINION PROGRAM REQUIRES '. ELTMASOP
00135      05  FILLER                  PIC X(24) VALUE SPACES.          ELTMASOP
00136 *                                                                 ELTMASOP
00137  01  WS-APPROVAL-SOURCEA.                                         ELTMASOP
00138      05  FILLER                  PIC X(79) VALUE ' APPROVAL.'.    ELTMASOP
00139 *                                                                 ELTMASOP
00140  01  WS-BC-ALT-PRICING.                                           ELTMASOP
00141      05  FILLER                  PIC X(51) VALUE                  ELTMASOP
00142          'THE ALTERNATE PRICING FOR INSTITUTIONAL SERVICE IS '.   ELTMASOP
00143      05  FILLER                  PIC X(34) VALUE SPACE.           ELTMASOP
00144 *                                                                 ELTMASOP
00145  01  WS-BS-ALT-PRICING.                                           ELTMASOP
00146      05  FILLER                  PIC X(50) VALUE                  ELTMASOP
00147          'THE ALTERNATE PRICING FOR PROFESSIONAL SERVICE IS '.    ELTMASOP
00148      05  FILLER                  PIC X(35) VALUE SPACE.           ELTMASOP
00149 *                                                                 ELTMASOP
00150  01  WS-MM-ALT-PRICING.                                           ELTMASOP
00151      05  FILLER                  PIC X(50) VALUE                  ELTMASOP
00152          'THE ALTERNATE PRICING FOR SUPPLEMENTAL SERVICE IS '.    ELTMASOP
00153      05  FILLER                  PIC X(35) VALUE SPACE.           ELTMASOP
00154 *                                                                 ELTMASOP
00155  01  WS-BENEFITS-REDUCTION.                                       ELTMASOP
00156      05  FILLER                  PIC X(52) VALUE                  ELTMASOP
00157         'DENIED OR REDUCED BENEFITS DUE TO COST CONTAINMENT: '.   ELTMASOP
00158 *                                                                 ELTMASOP
00159  01  WS-SPILL-OVER.                                               ELTMASOP
00160      05  FILLER                  PIC X(51) VALUE                  ELTMASOP
00161         'UNPAID SERVICES AFTER BASIC BENEFIT REDUCTIONS ARE '.    ELTMASOP
00162      05  FILLER                  PIC X(28) VALUE SPACES.          ELTMASOP
00163 *                                                                 ELTMASOP
00164  01  WS-END-SPILL-OVER.                                           ELTMASOP
00165      05  FILLER                  PIC X(33) VALUE                  ELTMASOP
00166         'UNDER SUPPLEMENTAL MAJOR MEDICAL.'.                      ELTMASOP
00167      05  FILLER                  PIC X(66) VALUE SPACES.          ELTMASOP
00168 ******************************************************************ELTMASOP
00169 ***SPECIAL MESSAGES                                               ELTMASOP
00170 ******************************************************************ELTMASOP
00171  01  WS-NOT-APPLICABLE-MSG.                                       ELTMASOP
00172      05  FILLER                  PIC  X(66) VALUE                 ELTMASOP
00173          'THE MANDATORY ADDITIONAL SECOND OPINION PROGRAM IS NOT AELTMASOP
00174 -        'PPLICABLE.'.                                            ELTMASOP
00175      05  FILLER                  PIC  X(13) VALUE SPACES.         ELTMASOP
00176 *                                                                 ELTMASOP
00177  01  WS-VOLUNTARY-MSG.                                            ELTMASOP
00178      05  FILLER                  PIC  X(61) VALUE                 ELTMASOP
00179          'THE MANDATORY ADDITIONAL SECOND OPINION PROGRAM IS VOLUNELTMASOP
00180 -        'TARY.'.                                                 ELTMASOP
00181      05  FILLER                  PIC  X(14) VALUE SPACES.         ELTMASOP
00182 *                                                                 ELTMASOP
00183  01  WS-NOT-INSTITUTIONAL-MSG.                                    ELTMASOP
00184      05  FILLER                  PIC X(79) VALUE                  ELTMASOP
00185          'MANDATORY ADDITIONAL SECOND OPINION DOES NOT APPLY FOR IELTMASOP
00186 -        'NSTITUTIONAL BENEFITS.'.                                ELTMASOP
00187 *                                                                 ELTMASOP
00188  01  WS-NOT-PROFESSIONAL-MSG.                                     ELTMASOP
00189      05  FILLER                  PIC X(79) VALUE                  ELTMASOP
00190          'MANDATORY ADDITIONAL SECOND OPINION DOES NOT APPLY FOR PELTMASOP
00191 -        'ROFESSIONAL BENEFITS.'.                                 ELTMASOP
00192 *                                                                 ELTMASOP
00193  01  WS-NOT-SUPPLEMENTAL-MSG.                                     ELTMASOP
00194      05  FILLER                  PIC X(79) VALUE                  ELTMASOP
00195          'MANDATORY ADDITIONAL SECOND OPINION DOES NOT APPLY FOR SELTMASOP
00196 -        'UPPLEMENTAL BENEFITS.'.                                 ELTMASOP
00197 *                                                                 ELTMASOP
00198  01  SPECIAL-SERVICES-MSG.                                        ELTMASOP
00199      05  FILLER                  PIC X(79) VALUE                  ELTMASOP
00200         'THERE ARE SPECIAL RELATED SERVICES INCLUDED IN THIS COST ELTMASOP
00201 -       'CONTAINMENT PROGRAM.'.                                   ELTMASOP
00202 *                                                                 ELTMASOP
00203  01  SPECIAL-PROCEDURES-MSG.                                      ELTMASOP
00204      05  FILLER                  PIC X(79) VALUE                  ELTMASOP
00205         'THERE ARE SPECIAL RELATED PROCEDURES INCLUDED IN THIS COSELTMASOP
00206 -       'T CONTAINMENT PROGRAM.'.                                 ELTMASOP
00207 *                                                                 ELTMASOP
00208  01  WS-DISCLAIMER.                                               ELTMASOP
00209      05  FILLER                  PIC X(79) VALUE                  ELTMASOP
00210      '*** SUBJECT TO OTHER CONTRACT LIMITATIONS ***'.             ELTMASOP
00211 *                                                                 ELTMASOP
00212  01  PROGRAM-CONSTANTS.                                           ELTMASOP
00213      05  PC-GRP                  PIC  X(06) VALUE 'GROUP'.        ELTMASOP
00214      05  PC-GCCP                 PIC  X(06) VALUE '#GCCP '.       ELTMASOP
00215      05  PC-GMOR                 PIC  X(06) VALUE '#GMOR '.       ELTMASOP
00216      05  PC-GMOB                 PIC  X(06) VALUE '#GMOB '.       ELTMASOP
00217 /                                                                 ELTMASOP
00218  LINKAGE SECTION.                                                 ELTMASOP
00219  01  DFHCOMMAREA.                                                 ELTMASOP
00220      COPY ELSCOMMC.                                               ELTMASOP
00221 /                                                                 ELTMASOP
00222      COPY ELSCIA2C.                                               ELTMASOP
00223 /                                                                 ELTMASOP
00224      COPY ELSCMDSC.                                               ELTMASOP
00225 /                                                                 ELTMASOP
00226      COPY ELSCMIFC.                                               ELTMASOP
00227 /                                                                 ELTMASOP
00228      COPY ELSIOPMC.                                               ELTMASOP
00229 /                                                                 ELTMASOP
00230      COPY ELSKEYSC.                                               ELTMASOP
00231 /                                                                 ELTMASOP
00232      COPY ELSOUTPC.                                               ELTMASOP
00233 /                                                                 ELTMASOP
00234      COPY ELSSRTPC.                                               ELTMASOP
00235 /                                                                 ELTMASOP
00236      COPY ELSTCWAC.                                               ELTMASOP
00237 /                                                                 ELTMASOP
00238      COPY ELSSSCBC.                                               ELTMASOP
00239 /                                                                 ELTMASOP
00240  01  GCGROUPC-REC.                                                ELTMASOP
00241      COPY GCGROUPC.                                               ELTMASOP
00242 /                                                                 ELTMASOP
00243  01  GCCP-TABULAR-REC.                                            ELTMASOP
00244      COPY GCTGCCPC.                                               ELTMASOP
00245 /                                                                 ELTMASOP
00246      EJECT                                                        ELTMASOP
00247  PROCEDURE DIVISION.                                              ELTMASOP
00248 ************************************************************      ELTMASOP
00249 *                                                          *      ELTMASOP
00250 *                    PROCEDURE DIVISION                    *      ELTMASOP
00251 *                                                          *      ELTMASOP
00252 ************************************************************      ELTMASOP
00253                                                                   ELTMASOP
00254                                                                   ELTMASOP
00255 ************************************************************      ELTMASOP
00256 *                                                          *      ELTMASOP
00257 *        MANDATORY ADDITIONAL SECOND OPINION               *      ELTMASOP
00258 *                                                          *      ELTMASOP
00259 ************************************************************      ELTMASOP
00260  MANDATORY-ADDITIONAL-SECOND-OP.                                  ELTMASOP
00261      PERFORM INITIALIZATION.                                      ELTMASOP
00262      PERFORM PROCESS-MASOP.                                       ELTMASOP
00263      GOBACK.                                                      ELTMASOP
00264                                                                   ELTMASOP
00265                                                                   ELTMASOP
00266 ************************************************************      ELTMASOP
00267 *                                                          *      ELTMASOP
00268 *        INITIALIZATION                                    *      ELTMASOP
00269 *                                                          *      ELTMASOP
00270 ************************************************************      ELTMASOP
00271  INITIALIZATION.                                                  ELTMASOP
00272      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTMASOP
00273      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTMASOP
00274                                                                   ELTMASOP
00275                                                                   ELTMASOP
00276 ************************************************************      ELTMASOP
00277 *                                                          *      ELTMASOP
00278 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTMASOP
00279 *                                                          *      ELTMASOP
00280 ************************************************************      ELTMASOP
00281  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTMASOP
00282      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTMASOP
00283      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTMASOP
00284      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTMASOP
00285                                                                   ELTMASOP
00286                                                                   ELTMASOP
00287 ************************************************************      ELTMASOP
00288 *                                                          *      ELTMASOP
00289 *        CHECK FOR VALID COMMAREA                          *      ELTMASOP
00290 *                                                          *      ELTMASOP
00291 ************************************************************      ELTMASOP
00292  CHECK-FOR-VALID-COMMAREA.                                        ELTMASOP
00293      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTMASOP
00294          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTMASOP
00295                                                                   ELTMASOP
00296                                                                   ELTMASOP
00297 ************************************************************      ELTMASOP
00298 *                                                          *      ELTMASOP
00299 *        SIGNAL INVALID COMMAREA                           *      ELTMASOP
00300 *                                                          *      ELTMASOP
00301 ************************************************************      ELTMASOP
00302  SIGNAL-INVALID-COMMAREA.                                         ELTMASOP
00303      EXEC CICS ABEND                                              ELTMASOP
00304                ABCODE('EL01')                                     ELTMASOP
00305         END-EXEC.                                                 ELTMASOP
00306      EJECT                                                        ELTMASOP
00307                                                                   ELTMASOP
00308                                                                   ELTMASOP
00309 ************************************************************      ELTMASOP
00310 *                                                          *      ELTMASOP
00311 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTMASOP
00312 *                                                          *      ELTMASOP
00313 ************************************************************      ELTMASOP
00314  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTMASOP
00315      IF ECA-CIA-PTR = NULL                                        ELTMASOP
00316          PERFORM SIGNAL-INVALID-CIA                               ELTMASOP
00317      ELSE                                                         ELTMASOP
00318          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTMASOP
00319                                                                   ELTMASOP
00320                                                                   ELTMASOP
00321 ************************************************************      ELTMASOP
00322 *                                                          *      ELTMASOP
00323 *        SIGNAL INVALID CIA                                *      ELTMASOP
00324 *                                                          *      ELTMASOP
00325 ************************************************************      ELTMASOP
00326  SIGNAL-INVALID-CIA.                                              ELTMASOP
00327      EXEC CICS ABEND                                              ELTMASOP
00328                ABCODE('EL02')                                     ELTMASOP
00329         END-EXEC.                                                 ELTMASOP
00330      EJECT                                                        ELTMASOP
00331                                                                   ELTMASOP
00332                                                                   ELTMASOP
00333 ************************************************************      ELTMASOP
00334 *                                                          *      ELTMASOP
00335 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTMASOP
00336 *                                                          *      ELTMASOP
00337 ************************************************************      ELTMASOP
00338  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTMASOP
00339      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTMASOP
00340      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMASOP
00341                            ADDRESS OF                             ELTMASOP
00342          SSB-SELECTOR-STATUS-CTL-BLK.                             ELTMASOP
00343      IF CIA-RC-PTR-NULL                                           ELTMASOP
00344          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMASOP
00345                                                                   ELTMASOP
00346                                                                   ELTMASOP
00347 ************************************************************      ELTMASOP
00348 *                                                          *      ELTMASOP
00349 *        SIGNAL UNALLOC AREA ERROR                         *      ELTMASOP
00350 *                                                          *      ELTMASOP
00351 ************************************************************      ELTMASOP
00352  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTMASOP
00353      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTMASOP
00354      PERFORM SIGNAL-ABEND.                                        ELTMASOP
00355                                                                   ELTMASOP
00356                                                                   ELTMASOP
00357 ************************************************************      ELTMASOP
00358 *                                                          *      ELTMASOP
00359 *        SIGNAL ABEND                                      *      ELTMASOP
00360 *                                                          *      ELTMASOP
00361 ************************************************************      ELTMASOP
00362  SIGNAL-ABEND.                                                    ELTMASOP
00363      EXEC CICS ABEND                                              ELTMASOP
00364                ABCODE(CIA-ABCODE)                                 ELTMASOP
00365         END-EXEC.                                                 ELTMASOP
00366      EJECT                                                        ELTMASOP
00367                                                                   ELTMASOP
00368                                                                   ELTMASOP
00369 ************************************************************      ELTMASOP
00370 *                                                          *      ELTMASOP
00371 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTMASOP
00372 *                                                          *      ELTMASOP
00373 ************************************************************      ELTMASOP
00374  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTMASOP
00375      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTMASOP
00376      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTMASOP
00377      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTMASOP
00378      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTMASOP
00379      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTMASOP
00380      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTMASOP
00381      PERFORM ESTABLISH-ADDRESSABILITY-OF-CC.                      ELTMASOP
00382                                                                   ELTMASOP
00383                                                                   ELTMASOP
00384 ************************************************************      ELTMASOP
00385 *                                                          *      ELTMASOP
00386 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTMASOP
00387 *                                                          *      ELTMASOP
00388 ************************************************************      ELTMASOP
00389  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTMASOP
00390      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTMASOP
00391      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMASOP
00392                            ADDRESS OF                             ELTMASOP
00393          CMF-CODES-MANUAL-INTERFACE.                              ELTMASOP
00394      IF CIA-RC-PTR-NULL                                           ELTMASOP
00395          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMASOP
00396      EJECT                                                        ELTMASOP
00397                                                                   ELTMASOP
00398                                                                   ELTMASOP
00399 ************************************************************      ELTMASOP
00400 *                                                          *      ELTMASOP
00401 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTMASOP
00402 *                                                          *      ELTMASOP
00403 ************************************************************      ELTMASOP
00404  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTMASOP
00405      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTMASOP
00406      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMASOP
00407                            ADDRESS OF                             ELTMASOP
00408          COF-OUTPUT-INTERFACE.                                    ELTMASOP
00409      IF CIA-RC-PTR-NULL                                           ELTMASOP
00410          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMASOP
00411      EJECT                                                        ELTMASOP
00412                                                                   ELTMASOP
00413                                                                   ELTMASOP
00414 ************************************************************      ELTMASOP
00415 *                                                          *      ELTMASOP
00416 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTMASOP
00417 *                                                          *      ELTMASOP
00418 ************************************************************      ELTMASOP
00419  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTMASOP
00420      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTMASOP
00421      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMASOP
00422                            ADDRESS OF                             ELTMASOP
00423          SRP-SUBROUTINE-PARAMETERS.                               ELTMASOP
00424      IF CIA-RC-PTR-NULL                                           ELTMASOP
00425          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMASOP
00426      EJECT                                                        ELTMASOP
00427                                                                   ELTMASOP
00428                                                                   ELTMASOP
00429 ************************************************************      ELTMASOP
00430 *                                                          *      ELTMASOP
00431 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTMASOP
00432 *                                                          *      ELTMASOP
00433 ************************************************************      ELTMASOP
00434  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTMASOP
00435      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTMASOP
00436      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMASOP
00437                            ADDRESS OF                             ELTMASOP
00438          TCAR-COMPRESSION-WORK-AREA.                              ELTMASOP
00439      IF CIA-RC-PTR-NULL                                           ELTMASOP
00440          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMASOP
00441      EJECT                                                        ELTMASOP
00442                                                                   ELTMASOP
00443                                                                   ELTMASOP
00444 ************************************************************      ELTMASOP
00445 *                                                          *      ELTMASOP
00446 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTMASOP
00447 *                                                          *      ELTMASOP
00448 ************************************************************      ELTMASOP
00449  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTMASOP
00450      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTMASOP
00451      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMASOP
00452                            ADDRESS OF                             ELTMASOP
00453          KWA-FILE-KEY-WORK-AREA.                                  ELTMASOP
00454      IF CIA-RC-PTR-NULL                                           ELTMASOP
00455          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMASOP
00456      EJECT                                                        ELTMASOP
00457                                                                   ELTMASOP
00458                                                                   ELTMASOP
00459 ************************************************************      ELTMASOP
00460 *                                                          *      ELTMASOP
00461 *        ESTABLISH ADDRESSABILITY OF GRP SPEC              *      ELTMASOP
00462 *                                                          *      ELTMASOP
00463 ************************************************************      ELTMASOP
00464  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTMASOP
00465      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTMASOP
00466      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMASOP
00467                            ADDRESS OF GCGROUPC-REC.               ELTMASOP
00468      IF CIA-RC-PTR-NULL                                           ELTMASOP
00469          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMASOP
00470      EJECT                                                        ELTMASOP
00471                                                                   ELTMASOP
00472                                                                   ELTMASOP
00473 ************************************************************      ELTMASOP
00474 *                                                          *      ELTMASOP
00475 *        ESTABLISH ADDRESSABILITY OF CCP                   *      ELTMASOP
00476 *                                                          *      ELTMASOP
00477 ************************************************************      ELTMASOP
00478  ESTABLISH-ADDRESSABILITY-OF-CC.                                  ELTMASOP
00479      SET CIA-GCTABULR-DDN TO TRUE.                                ELTMASOP
00480      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMASOP
00481                            ADDRESS OF GCCP-TABULAR-REC.           ELTMASOP
00482      IF CIA-RC-PTR-NULL                                           ELTMASOP
00483          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMASOP
00484                                                                   ELTMASOP
00485                                                                   ELTMASOP
00486 ************************************************************      ELTMASOP
00487 *                                                          *      ELTMASOP
00488 *        ESTABLISH ADDRESS OF CIA                          *      ELTMASOP
00489 *                                                          *      ELTMASOP
00490 ************************************************************      ELTMASOP
00491  ESTABLISH-ADDRESS-OF-CIA.                                        ELTMASOP
00492      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTMASOP
00493                            ADDRESS OF                             ELTMASOP
00494          CIA-ELS-COMMON-INTERFACE-AREA.                           ELTMASOP
00495      EJECT                                                        ELTMASOP
00496                                                                   ELTMASOP
00497                                                                   ELTMASOP
00498 ************************************************************      ELTMASOP
00499 *                                                          *      ELTMASOP
00500 *        PROCESS MASOP                                     *      ELTMASOP
00501 *                                                          *      ELTMASOP
00502 ************************************************************      ELTMASOP
00503  PROCESS-MASOP.                                                   ELTMASOP
00504      IF GCG-MAND-ADDL-SURG-OPN-IND EQUAL ZERO                     ELTMASOP
00505               OR GCG-MAND-ADDL-SURG-OPN-IND EQUAL '08'            ELTMASOP
00506          PERFORM TEST-APPLICABILITY                               ELTMASOP
00507      ELSE                                                         ELTMASOP
00508          PERFORM GENERATE-MASOP-TEXT.                             ELTMASOP
00509      MOVE 'E' TO COF-FUNCTION.                                    ELTMASOP
00510      MOVE ZERO TO COF-NBR-DTL-LINES                               ELTMASOP
00511                   COF-NBR-HDR-LINES.                              ELTMASOP
00512      PERFORM LINK-TO-OUTPUT.                                      ELTMASOP
00513      EJECT                                                        ELTMASOP
00514                                                                   ELTMASOP
00515                                                                   ELTMASOP
00516 ************************************************************      ELTMASOP
00517 *                                                          *      ELTMASOP
00518 *        GENERATE MASOP TEXT                               *      ELTMASOP
00519 *                                                          *      ELTMASOP
00520 ************************************************************      ELTMASOP
00521  GENERATE-MASOP-TEXT.                                             ELTMASOP
00522      SET WS-POINTER2 TO NULLS.                                    ELTMASOP
00523      SET WS-POINTER3 TO NULLS.                                    ELTMASOP
00524      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTMASOP
00525      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMASOP
00526                            WS-POINTER2.                           ELTMASOP
00527      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTMASOP
00528      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMASOP
00529                            WS-POINTER3.                           ELTMASOP
00530      PERFORM VERIFY-MASOP-IN-GCCP-RECORD.                         ELTMASOP
00531      PERFORM BUILD-MASOP-TEXT.                                    ELTMASOP
00532      EJECT                                                        ELTMASOP
00533                                                                   ELTMASOP
00534                                                                   ELTMASOP
00535 ************************************************************      ELTMASOP
00536 *                                                          *      ELTMASOP
00537 *        TEST APPLICABILITY                                *      ELTMASOP
00538 *                                                          *      ELTMASOP
00539 ************************************************************      ELTMASOP
00540  TEST-APPLICABILITY.                                              ELTMASOP
00541      PERFORM GENERATE-HEADINGS.                                   ELTMASOP
00542      IF GCG-MAND-ADDL-SURG-OPN-IND  EQUAL ZERO                    ELTMASOP
00543          PERFORM SIGNAL-NOT-APPLICABLE-MSG                        ELTMASOP
00544      ELSE IF GCG-MAND-ADDL-SURG-OPN-IND  EQUAL  '08'              ELTMASOP
00545          PERFORM SIGNAL-VOLUNTARY-MSG.                            ELTMASOP
00546      PERFORM LINK-TO-OUTPUT.                                      ELTMASOP
00547                                                                   ELTMASOP
00548                                                                   ELTMASOP
00549 ************************************************************      ELTMASOP
00550 *                                                          *      ELTMASOP
00551 *        VERIFY MASOP IN GCCP RECORD                       *      ELTMASOP
00552 *                                                          *      ELTMASOP
00553 ************************************************************      ELTMASOP
00554  VERIFY-MASOP-IN-GCCP-RECORD.                                     ELTMASOP
00555      PERFORM ACQUIRE-GCCP-RECORD.                                 ELTMASOP
00556      PERFORM OBTAIN-MASOP-WITHIN-GCCP-RECOR.                      ELTMASOP
00557      EJECT                                                        ELTMASOP
00558                                                                   ELTMASOP
00559                                                                   ELTMASOP
00560 ************************************************************      ELTMASOP
00561 *                                                          *      ELTMASOP
00562 *        ACQUIRE GCCP RECORD                               *      ELTMASOP
00563 *                                                          *      ELTMASOP
00564 ************************************************************      ELTMASOP
00565  ACQUIRE-GCCP-RECORD.                                             ELTMASOP
00566      MOVE SPACES TO KWA-PROVISION-ID.                             ELTMASOP
00567      SET GCG-INDEX TO 1.                                          ELTMASOP
00568      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTMASOP
00569          AT END                                                   ELTMASOP
00570             MOVE ZEROES TO KWA-PROVISION-SLOT-NO                  ELTMASOP
00571             WHEN GCG-TAB-ID (GCG-INDEX) = PC-GCCP                 ELTMASOP
00572                 MOVE GCG-TAB-ID (GCG-INDEX) TO                    ELTMASOP
00573          KWA-PROVISION-ID                                         ELTMASOP
00574                 MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                  ELTMASOP
00575                     TO KWA-PROVISION-SLOT-NO                      ELTMASOP
00576          END-SEARCH.                                              ELTMASOP
00577      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELTMASOP
00578          PERFORM SIGNAL-UNDEFINED-TABULAR                         ELTMASOP
00579      ELSE                                                         ELTMASOP
00580          PERFORM READ-GCCP-RECORD.                                ELTMASOP
00581                                                                   ELTMASOP
00582                                                                   ELTMASOP
00583 ************************************************************      ELTMASOP
00584 *                                                          *      ELTMASOP
00585 *        SIGNAL UNDEFINED TABULAR                          *      ELTMASOP
00586 *                                                          *      ELTMASOP
00587 ************************************************************      ELTMASOP
00588  SIGNAL-UNDEFINED-TABULAR.                                        ELTMASOP
00589      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTMASOP
00590      PERFORM SIGNAL-ABEND.                                        ELTMASOP
00591      EJECT                                                        ELTMASOP
00592                                                                   ELTMASOP
00593                                                                   ELTMASOP
00594 ************************************************************      ELTMASOP
00595 *                                                          *      ELTMASOP
00596 *        OBTAIN MASOP WITHIN GCCP RECORD                   *      ELTMASOP
00597 *                                                          *      ELTMASOP
00598 ************************************************************      ELTMASOP
00599  OBTAIN-MASOP-WITHIN-GCCP-RECOR.                                  ELTMASOP
00600      SET GSS-INDEX TO 1.                                          ELTMASOP
00601      SEARCH GSS-ENTRY                                             ELTMASOP
00602         AT END                                                    ELTMASOP
00603            SET TABULAR-IS-UNDEFINED TO TRUE                       ELTMASOP
00604         WHEN GSS-MA-PROG-CODE-CHR (GSS-INDEX)                     ELTMASOP
00605            SET TABULAR-IS-DEFINED TO TRUE                         ELTMASOP
00606          END-SEARCH.                                              ELTMASOP
00607      IF TABULAR-IS-UNDEFINED                                      ELTMASOP
00608          PERFORM SIGNAL-UNDEFINED-TABULAR.                        ELTMASOP
00609      EJECT                                                        ELTMASOP
00610                                                                   ELTMASOP
00611                                                                   ELTMASOP
00612 ************************************************************      ELTMASOP
00613 *                                                          *      ELTMASOP
00614 *        BUILD MASOP TEXT                                  *      ELTMASOP
00615 *                                                          *      ELTMASOP
00616 ************************************************************      ELTMASOP
00617  BUILD-MASOP-TEXT.                                                ELTMASOP
00618      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTMASOP
00619          PERFORM GENERATE-INSTITUTIONAL.                          ELTMASOP
00620      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELTMASOP
00621          PERFORM GENERATE-PROFESSIONAL.                           ELTMASOP
00622      IF GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                        ELTMASOP
00623                 '03' OR '04' OR '06' OR '08'                      ELTMASOP
00624          PERFORM PROCESS-SUPPLEMENTAL.                            ELTMASOP
00625                                                                   ELTMASOP
00626                                                                   ELTMASOP
00627 ************************************************************      ELTMASOP
00628 *                                                          *      ELTMASOP
00629 *        GENERATE INSTITUTIONAL                            *      ELTMASOP
00630 *                                                          *      ELTMASOP
00631 ************************************************************      ELTMASOP
00632  GENERATE-INSTITUTIONAL.                                          ELTMASOP
00633      SET INSTITUTIONAL-SCREEN TO TRUE.                            ELTMASOP
00634      PERFORM GENERATE-HEADINGS.                                   ELTMASOP
00635      PERFORM BUILD-INSTITUTIONAL-TEXT.                            ELTMASOP
00636      EJECT                                                        ELTMASOP
00637                                                                   ELTMASOP
00638                                                                   ELTMASOP
00639 ************************************************************      ELTMASOP
00640 *                                                          *      ELTMASOP
00641 *        GENERATE PROFESSIONAL                             *      ELTMASOP
00642 *                                                          *      ELTMASOP
00643 ************************************************************      ELTMASOP
00644  GENERATE-PROFESSIONAL.                                           ELTMASOP
00645      SET PROFESSIONAL-SCREEN TO TRUE.                             ELTMASOP
00646      PERFORM GENERATE-HEADINGS.                                   ELTMASOP
00647      PERFORM BUILD-PROFESSIONAL-TEXT.                             ELTMASOP
00648      EJECT                                                        ELTMASOP
00649                                                                   ELTMASOP
00650                                                                   ELTMASOP
00651 ************************************************************      ELTMASOP
00652 *                                                          *      ELTMASOP
00653 *        PROCESS SUPPLEMENTAL                              *      ELTMASOP
00654 *                                                          *      ELTMASOP
00655 ************************************************************      ELTMASOP
00656  PROCESS-SUPPLEMENTAL.                                            ELTMASOP
00657      SET SUPPLEMENTAL-SCREEN TO TRUE.                             ELTMASOP
00658      PERFORM GENERATE-HEADINGS.                                   ELTMASOP
00659      PERFORM BUILD-SUPPLEMENTAL-TEXT.                             ELTMASOP
00660      EJECT                                                        ELTMASOP
00661                                                                   ELTMASOP
00662                                                                   ELTMASOP
00663 ************************************************************      ELTMASOP
00664 *                                                          *      ELTMASOP
00665 *        GENERATE HEADINGS                                 *      ELTMASOP
00666 *                                                          *      ELTMASOP
00667 ************************************************************      ELTMASOP
00668  GENERATE-HEADINGS.                                               ELTMASOP
00669      SET COF-NEW-PAGE TO TRUE.                                    ELTMASOP
00670      IF INSTITUTIONAL-SCREEN                                      ELTMASOP
00671          PERFORM MOVE-INST-HEADINGS                               ELTMASOP
00672      ELSE                                                         ELTMASOP
00673         IF PROFESSIONAL-SCREEN                                    ELTMASOP
00674             PERFORM MOVE-PROF-HEADINGS                            ELTMASOP
00675         ELSE                                                      ELTMASOP
00676            IF SUPPLEMENTAL-SCREEN                                 ELTMASOP
00677              PERFORM MOVE-SUPP-HEADINGS.                          ELTMASOP
00678      MOVE 2 TO COF-NBR-HDR-LINES.                                 ELTMASOP
00679      MOVE WS-HDR-LN2 TO COF-HDR-LINE                              ELTMASOP
00680          (COF-NBR-HDR-LINES).                                     ELTMASOP
00681      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMASOP
00682      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMASOP
00683      IF INSTITUTIONAL-SCREEN                                      ELTMASOP
00684              OR PROFESSIONAL-SCREEN                               ELTMASOP
00685              OR SUPPLEMENTAL-SCREEN                               ELTMASOP
00686          PERFORM LINK-TO-OUTPUT.                                  ELTMASOP
00687                                                                   ELTMASOP
00688                                                                   ELTMASOP
00689 ************************************************************      ELTMASOP
00690 *                                                          *      ELTMASOP
00691 *        MOVE INST HEADINGS                                *      ELTMASOP
00692 *                                                          *      ELTMASOP
00693 ************************************************************      ELTMASOP
00694  MOVE-INST-HEADINGS.                                              ELTMASOP
00695      MOVE 'INSTITUTIONAL' TO HDR-TITLE.                           ELTMASOP
00696                                                                   ELTMASOP
00697                                                                   ELTMASOP
00698 ************************************************************      ELTMASOP
00699 *                                                          *      ELTMASOP
00700 *        MOVE PROF HEADINGS                                *      ELTMASOP
00701 *                                                          *      ELTMASOP
00702 ************************************************************      ELTMASOP
00703  MOVE-PROF-HEADINGS.                                              ELTMASOP
00704      MOVE 'PROFESSIONAL' TO HDR-TITLE.                            ELTMASOP
00705                                                                   ELTMASOP
00706                                                                   ELTMASOP
00707 ************************************************************      ELTMASOP
00708 *                                                          *      ELTMASOP
00709 *        MOVE SUPP HEADINGS                                *      ELTMASOP
00710 *                                                          *      ELTMASOP
00711 ************************************************************      ELTMASOP
00712  MOVE-SUPP-HEADINGS.                                              ELTMASOP
00713      MOVE 'SUPPLEMENTAL' TO HDR-TITLE.                            ELTMASOP
00714                                                                   ELTMASOP
00715                                                                   ELTMASOP
00716 ************************************************************      ELTMASOP
00717 *                                                          *      ELTMASOP
00718 *        BUILD INSTITUTIONAL TEXT                          *      ELTMASOP
00719 *                                                          *      ELTMASOP
00720 ************************************************************      ELTMASOP
00721  BUILD-INSTITUTIONAL-TEXT.                                        ELTMASOP
00722      IF GSS-MA-BC-IND (GSS-INDEX) EQUAL ZEROES OR SPACES          ELTMASOP
00723                 OR LOW-VALUES                                     ELTMASOP
00724          PERFORM SIGNAL-NOT-APPLICABLE-FOR-CROS                   ELTMASOP
00725      ELSE                                                         ELTMASOP
00726          PERFORM CONSTRUCT-BC-TEXT-AND-SCREEN.                    ELTMASOP
00727                                                                   ELTMASOP
00728                                                                   ELTMASOP
00729 ************************************************************      ELTMASOP
00730 *                                                          *      ELTMASOP
00731 *        BUILD PROFESSIONAL TEXT                           *      ELTMASOP
00732 *                                                          *      ELTMASOP
00733 ************************************************************      ELTMASOP
00734  BUILD-PROFESSIONAL-TEXT.                                         ELTMASOP
00735      IF GSS-MA-BS-IND (GSS-INDEX) EQUAL ZEROES OR SPACES          ELTMASOP
00736                 OR LOW-VALUES                                     ELTMASOP
00737          PERFORM SIGNAL-NOT-APPLICABLE-FOR-SHEI                   ELTMASOP
00738      ELSE                                                         ELTMASOP
00739          PERFORM CONSTRUCT-BS-TEXT-AND-SCREEN.                    ELTMASOP
00740                                                                   ELTMASOP
00741                                                                   ELTMASOP
00742 ************************************************************      ELTMASOP
00743 *                                                          *      ELTMASOP
00744 *        BUILD SUPPLEMENTAL TEXT                           *      ELTMASOP
00745 *                                                          *      ELTMASOP
00746 ************************************************************      ELTMASOP
00747  BUILD-SUPPLEMENTAL-TEXT.                                         ELTMASOP
00748      IF GSS-MA-MM-IND (GSS-INDEX)  EQUAL ZEROES OR SPACES         ELTMASOP
00749          OR                                                       ELTMASOP
00750               LOW-VALUES                                          ELTMASOP
00751          PERFORM SIGNAL-NOT-APPLICABLE-FOR-LOBX                   ELTMASOP
00752      ELSE                                                         ELTMASOP
00753          PERFORM CONSTRUCT-MM-TEXT-AND-SCREEN.                    ELTMASOP
00754      EJECT                                                        ELTMASOP
00755                                                                   ELTMASOP
00756                                                                   ELTMASOP
00757 ************************************************************      ELTMASOP
00758 *                                                          *      ELTMASOP
00759 *        SIGNAL NOT APPLICABLE FOR CROSS                   *      ELTMASOP
00760 *                                                          *      ELTMASOP
00761 ************************************************************      ELTMASOP
00762  SIGNAL-NOT-APPLICABLE-FOR-CROS.                                  ELTMASOP
00763      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMASOP
00764      MOVE WS-NOT-INSTITUTIONAL-MSG TO COF-DTL-LINE                ELTMASOP
00765          (COF-NBR-DTL-LINES).                                     ELTMASOP
00766      PERFORM LINK-TO-OUTPUT.                                      ELTMASOP
00767                                                                   ELTMASOP
00768                                                                   ELTMASOP
00769 ************************************************************      ELTMASOP
00770 *                                                          *      ELTMASOP
00771 *        SIGNAL NOT APPLICABLE FOR SHEILD                  *      ELTMASOP
00772 *                                                          *      ELTMASOP
00773 ************************************************************      ELTMASOP
00774  SIGNAL-NOT-APPLICABLE-FOR-SHEI.                                  ELTMASOP
00775      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMASOP
00776      MOVE WS-NOT-PROFESSIONAL-MSG TO COF-DTL-LINE                 ELTMASOP
00777          (COF-NBR-DTL-LINES).                                     ELTMASOP
00778      PERFORM LINK-TO-OUTPUT.                                      ELTMASOP
00779                                                                   ELTMASOP
00780                                                                   ELTMASOP
00781 ************************************************************      ELTMASOP
00782 *                                                          *      ELTMASOP
00783 *        SIGNAL NOT APPLICABLE FOR LOB MM                  *      ELTMASOP
00784 *                                                          *      ELTMASOP
00785 ************************************************************      ELTMASOP
00786  SIGNAL-NOT-APPLICABLE-FOR-LOBX.                                  ELTMASOP
00787      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMASOP
00788      MOVE WS-NOT-SUPPLEMENTAL-MSG TO COF-DTL-LINE                 ELTMASOP
00789          (COF-NBR-DTL-LINES).                                     ELTMASOP
00790      PERFORM LINK-TO-OUTPUT.                                      ELTMASOP
00791      EJECT                                                        ELTMASOP
00792                                                                   ELTMASOP
00793                                                                   ELTMASOP
00794 ************************************************************      ELTMASOP
00795 *                                                          *      ELTMASOP
00796 *        CONSTRUCT BC TEXT AND SCREEN                      *      ELTMASOP
00797 *                                                          *      ELTMASOP
00798 ************************************************************      ELTMASOP
00799  CONSTRUCT-BC-TEXT-AND-SCREEN.                                    ELTMASOP
00800      PERFORM TRANSLATE-DISPLAY-MASOP-IND.                         ELTMASOP
00801      IF GSS-MA-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTMASOP
00802          ZERO                                                     ELTMASOP
00803                 AND SPACES AND LOW-VALUES                         ELTMASOP
00804          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMASOP
00805      PERFORM TRANSLATE-BC-INDICATOR.                              ELTMASOP
00806      IF GSS-MA-BC-IP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL       ELTMASOP
00807          ZERO                                                     ELTMASOP
00808               AND SPACES AND LOW-VALUES                           ELTMASOP
00809          PERFORM TRANSLATE-BC-IP-ALT-PRICING.                     ELTMASOP
00810      IF GSS-MA-BC-OP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL       ELTMASOP
00811          ZERO                                                     ELTMASOP
00812                AND SPACES AND LOW-VALUES                          ELTMASOP
00813          PERFORM TRANSLATE-BC-OP-ALT-PRICING.                     ELTMASOP
00814      SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTMASOP
00815      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTMASOP
00816      PERFORM GENERATE-BC-CALC-METHOD-SENTEN.                      ELTMASOP
00817      PERFORM GENERATE-BC-BEN-REDUCT-SENT.                         ELTMASOP
00818      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                       ELTMASOP
00819               '03' OR '04' OR '06' OR '08')                       ELTMASOP
00820            AND                                                    ELTMASOP
00821             (GSS-MA-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL          ELTMASOP
00822          ZERO                                                     ELTMASOP
00823                         AND SPACES AND LOW-VALUES)                ELTMASOP
00824          PERFORM GENERATE-SPILL-OVER-INDICATOR.                   ELTMASOP
00825      PERFORM GENERATE-ADDITIONAL-TABULARS.                        ELTMASOP
00826      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTMASOP
00827      MOVE SPACES TO SCREEN-TYPE.                                  ELTMASOP
00828      EJECT                                                        ELTMASOP
00829                                                                   ELTMASOP
00830                                                                   ELTMASOP
00831 ************************************************************      ELTMASOP
00832 *                                                          *      ELTMASOP
00833 *        CONSTRUCT BS TEXT AND SCREEN                      *      ELTMASOP
00834 *                                                          *      ELTMASOP
00835 ************************************************************      ELTMASOP
00836  CONSTRUCT-BS-TEXT-AND-SCREEN.                                    ELTMASOP
00837      PERFORM TRANSLATE-DISPLAY-MASOP-IND.                         ELTMASOP
00838      IF GSS-MA-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTMASOP
00839          ZERO                                                     ELTMASOP
00840                 AND SPACES AND LOW-VALUES                         ELTMASOP
00841          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMASOP
00842      PERFORM TRANSLATE-BS-INDICATOR.                              ELTMASOP
00843      IF GSS-MA-BS-IP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL       ELTMASOP
00844          ZERO                                                     ELTMASOP
00845               AND SPACES AND LOW-VALUES                           ELTMASOP
00846          PERFORM TRANSLATE-BS-IP-ALT-PRICING.                     ELTMASOP
00847      IF GSS-MA-BS-OP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL       ELTMASOP
00848          ZERO                                                     ELTMASOP
00849                AND SPACES AND LOW-VALUES                          ELTMASOP
00850          PERFORM TRANSLATE-BS-OP-ALT-PRICING.                     ELTMASOP
00851      SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTMASOP
00852      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTMASOP
00853      PERFORM GENERATE-BS-CALC-METHOD-SENTEN.                      ELTMASOP
00854      PERFORM GENERATE-BS-BEN-REDUCT-SENT.                         ELTMASOP
00855      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                       ELTMASOP
00856                   '03' OR '04' OR '06' OR '08')                   ELTMASOP
00857            AND                                                    ELTMASOP
00858             (GSS-MA-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL          ELTMASOP
00859          ZERO                                                     ELTMASOP
00860                         AND SPACES AND LOW-VALUES)                ELTMASOP
00861          PERFORM GENERATE-SPILL-OVER-INDICATOR.                   ELTMASOP
00862      PERFORM GENERATE-ADDITIONAL-TABULARS.                        ELTMASOP
00863      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTMASOP
00864      MOVE SPACES TO SCREEN-TYPE.                                  ELTMASOP
00865      EJECT                                                        ELTMASOP
00866                                                                   ELTMASOP
00867                                                                   ELTMASOP
00868 ************************************************************      ELTMASOP
00869 *                                                          *      ELTMASOP
00870 *        CONSTRUCT MM TEXT AND SCREEN                      *      ELTMASOP
00871 *                                                          *      ELTMASOP
00872 ************************************************************      ELTMASOP
00873  CONSTRUCT-MM-TEXT-AND-SCREEN.                                    ELTMASOP
00874      PERFORM TRANSLATE-DISPLAY-MASOP-IND.                         ELTMASOP
00875      IF GSS-MA-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTMASOP
00876          ZERO                                                     ELTMASOP
00877                 AND SPACES AND LOW-VALUES                         ELTMASOP
00878          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMASOP
00879      PERFORM TRANSLATE-MM-INDICATOR.                              ELTMASOP
00880      IF GSS-MA-MM-IP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL       ELTMASOP
00881          ZERO                                                     ELTMASOP
00882               AND SPACES AND LOW-VALUES                           ELTMASOP
00883          PERFORM TRANSLATE-MM-IP-ALT-PRICING.                     ELTMASOP
00884      IF GSS-MA-MM-OP-ALT-PRICING-METH (GSS-INDEX) NOT EQUAL       ELTMASOP
00885          ZERO                                                     ELTMASOP
00886                AND SPACES AND LOW-VALUES                          ELTMASOP
00887          PERFORM TRANSLATE-MM-OP-ALT-PRICING.                     ELTMASOP
00888      SET SRP-ACCUM-PROV-CLASS-SUPP TO TRUE.                       ELTMASOP
00889      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTMASOP
00890      PERFORM GENERATE-MM-CALC-METHOD-SENTEN.                      ELTMASOP
00891      PERFORM GENERATE-MM-BEN-REDUCT-SENT.                         ELTMASOP
00892      PERFORM GENERATE-ADDITIONAL-TABULARS.                        ELTMASOP
00893      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTMASOP
00894      EJECT                                                        ELTMASOP
00895                                                                   ELTMASOP
00896                                                                   ELTMASOP
00897 ************************************************************      ELTMASOP
00898 *                                                          *      ELTMASOP
00899 *        GENERATE DISCLAIMER SENTENCE                      *      ELTMASOP
00900 *                                                          *      ELTMASOP
00901 ************************************************************      ELTMASOP
00902  GENERATE-DISCLAIMER-SENTENCE.                                    ELTMASOP
00903      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMASOP
00904      MOVE WS-DISCLAIMER TO COF-DTL-LINE                           ELTMASOP
00905          (COF-NBR-DTL-LINES).                                     ELTMASOP
00906      PERFORM LINK-TO-OUTPUT.                                      ELTMASOP
00907      EJECT                                                        ELTMASOP
00908                                                                   ELTMASOP
00909                                                                   ELTMASOP
00910 ************************************************************      ELTMASOP
00911 *                                                          *      ELTMASOP
00912 *        TRANSLATE DISPLAY MASOP IND                       *      ELTMASOP
00913 *                                                          *      ELTMASOP
00914 ************************************************************      ELTMASOP
00915  TRANSLATE-DISPLAY-MASOP-IND.                                     ELTMASOP
00916      INITIALIZE TCAR-FROM-AREA.                                   ELTMASOP
00917      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
00918      MOVE WS-MASOP-APPLIES TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELTMASOP
00919      ADD  +1 TO TCAR-FROM-SUB.                                    ELTMASOP
00920      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMASOP
00921      SET PERIOD-NEEDED TO TRUE.                                   ELTMASOP
00922      MOVE GCG-MAND-ADDL-SURG-OPN-IND  TO CMF-CODE-VALUE.          ELTMASOP
00923      MOVE 'MAND-ADDL-SURG-OPN-IND'                                ELTMASOP
00924                              TO                                   ELTMASOP
00925          CMF-ELEMENT-SYSTEM-NAME.                                 ELTMASOP
00926      MOVE PC-GRP TO CMF-RECORD-PREFIX.                            ELTMASOP
00927      EXEC CICS LINK                                               ELTMASOP
00928                PROGRAM ('ELUCMIF')                                ELTMASOP
00929                COMMAREA (DFHCOMMAREA)                             ELTMASOP
00930         END-EXEC.                                                 ELTMASOP
00931      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
00932      MOVE SPACE TO ADDITIONAL-TEXT-SW.                            ELTMASOP
00933      EJECT                                                        ELTMASOP
00934                                                                   ELTMASOP
00935                                                                   ELTMASOP
00936 ************************************************************      ELTMASOP
00937 *                                                          *      ELTMASOP
00938 *        TRANSLATE APPROVAL SOURCE                         *      ELTMASOP
00939 *                                                          *      ELTMASOP
00940 ************************************************************      ELTMASOP
00941  TRANSLATE-APPROVAL-SOURCE.                                       ELTMASOP
00942      INITIALIZE WS-PERIOD-SW.                                     ELTMASOP
00943      INITIALIZE TCAR-FROM-AREA.                                   ELTMASOP
00944      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
00945      MOVE WS-APPROVAL-SOURCE TO TCAR-FROM-LINE                    ELTMASOP
00946          (TCAR-FROM-SUB).                                         ELTMASOP
00947      ADD 1 TO TCAR-FROM-SUB.                                      ELTMASOP
00948      SET ADDITIONAL-TEXT TO TRUE.                                 ELTMASOP
00949      IF NOT-HOLDING-APPROVAL-SRCE                                 ELTMASOP
00950          PERFORM GET-APPROVAL-SOURCE-CODE-TRANS                   ELTMASOP
00951      ELSE                                                         ELTMASOP
00952          PERFORM USE-EXISTING-APPROVAL-TRANSLAT.                  ELTMASOP
00953      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
00954      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTMASOP
00955      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMASOP
00956                            WS-POINTER3.                           ELTMASOP
00957      MOVE WS-APPROVAL-SOURCEA  TO TCAR-FROM-LINE                  ELTMASOP
00958          (TCAR-FROM-SUB).                                         ELTMASOP
00959      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMASOP
00960      PERFORM REFORMAT-AND-WRITE-TEXT.                             ELTMASOP
00961      EJECT                                                        ELTMASOP
00962                                                                   ELTMASOP
00963                                                                   ELTMASOP
00964 ************************************************************      ELTMASOP
00965 *                                                          *      ELTMASOP
00966 *        GET APPROVAL SOURCE CODE TRANSLATION              *      ELTMASOP
00967 *                                                          *      ELTMASOP
00968 ************************************************************      ELTMASOP
00969  GET-APPROVAL-SOURCE-CODE-TRANS.                                  ELTMASOP
00970      MOVE 'MA-APPROVAL-SOURCE-IND' TO CMF-ELEMENT-SYSTEM-NAME.    ELTMASOP
00971      MOVE GSS-MA-APPROVAL-SOURCE-IND (GSS-INDEX) TO               ELTMASOP
00972          CMF-CODE-VALUE.                                          ELTMASOP
00973      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
00974      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTMASOP
00975      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMASOP
00976                            ADDRESS OF CMF-DESCR.                  ELTMASOP
00977      EJECT                                                        ELTMASOP
00978                                                                   ELTMASOP
00979                                                                   ELTMASOP
00980 ************************************************************      ELTMASOP
00981 *                                                          *      ELTMASOP
00982 *        USE EXISTING APPROVAL TRANSLATION                 *      ELTMASOP
00983 *                                                          *      ELTMASOP
00984 ************************************************************      ELTMASOP
00985  USE-EXISTING-APPROVAL-TRANSLAT.                                  ELTMASOP
00986      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTMASOP
00987      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMASOP
00988                            ADDRESS OF CMF-DESCR.                  ELTMASOP
00989      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTMASOP
00990      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMASOP
00991                            WS-POINTER2.                           ELTMASOP
00992      EJECT                                                        ELTMASOP
00993                                                                   ELTMASOP
00994                                                                   ELTMASOP
00995 ************************************************************      ELTMASOP
00996 *                                                          *      ELTMASOP
00997 *        TRANSLATE BC INDICATOR                            *      ELTMASOP
00998 *                                                          *      ELTMASOP
00999 ************************************************************      ELTMASOP
01000  TRANSLATE-BC-INDICATOR.                                          ELTMASOP
01001      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMASOP
01002      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMASOP
01003      SET PERIOD-NEEDED TO TRUE.                                   ELTMASOP
01004      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
01005      MOVE 'MA-BC-IND'  TO CMF-ELEMENT-SYSTEM-NAME.                ELTMASOP
01006      MOVE GSS-MA-BC-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTMASOP
01007      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01008      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01009      EJECT                                                        ELTMASOP
01010                                                                   ELTMASOP
01011                                                                   ELTMASOP
01012 ************************************************************      ELTMASOP
01013 *                                                          *      ELTMASOP
01014 *        TRANSLATE BC IP ALT PRICING                       *      ELTMASOP
01015 *                                                          *      ELTMASOP
01016 ************************************************************      ELTMASOP
01017  TRANSLATE-BC-IP-ALT-PRICING.                                     ELTMASOP
01018      PERFORM GET-BC-ALT-PRICING-FIXED-TEXT.                       ELTMASOP
01019      MOVE 'MA-BC-IP-ALT-PRICING-METH' TO                          ELTMASOP
01020          CMF-ELEMENT-SYSTEM-NAME.                                 ELTMASOP
01021      MOVE GSS-MA-BC-IP-ALT-PRICING-METH (GSS-INDEX) TO            ELTMASOP
01022          CMF-CODE-VALUE.                                          ELTMASOP
01023      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01024      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01025      PERFORM WRITE-FINAL-IP-ALT-FIXED-TEXT.                       ELTMASOP
01026                                                                   ELTMASOP
01027                                                                   ELTMASOP
01028 ************************************************************      ELTMASOP
01029 *                                                          *      ELTMASOP
01030 *        GET BC ALT PRICING FIXED TEXT                     *      ELTMASOP
01031 *                                                          *      ELTMASOP
01032 ************************************************************      ELTMASOP
01033  GET-BC-ALT-PRICING-FIXED-TEXT.                                   ELTMASOP
01034      INITIALIZE WS-PERIOD-SW.                                     ELTMASOP
01035      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMASOP
01036      SET ADDITIONAL-TEXT TO TRUE.                                 ELTMASOP
01037      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
01038      MOVE WS-BC-ALT-PRICING TO TCAR-FROM-LINE (TCAR-FROM-SUB).    ELTMASOP
01039      ADD 1 TO TCAR-FROM-SUB.                                      ELTMASOP
01040                                                                   ELTMASOP
01041                                                                   ELTMASOP
01042 ************************************************************      ELTMASOP
01043 *                                                          *      ELTMASOP
01044 *        WRITE FINAL IP ALT FIXED TEXT                     *      ELTMASOP
01045 *                                                          *      ELTMASOP
01046 ************************************************************      ELTMASOP
01047  WRITE-FINAL-IP-ALT-FIXED-TEXT.                                   ELTMASOP
01048      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMASOP
01049      MOVE ' INPATIENT.' TO TCAR-FROM-LINE (TCAR-FROM-SUB).        ELTMASOP
01050      PERFORM REFORMAT-AND-WRITE-TEXT.                             ELTMASOP
01051      EJECT                                                        ELTMASOP
01052                                                                   ELTMASOP
01053                                                                   ELTMASOP
01054 ************************************************************      ELTMASOP
01055 *                                                          *      ELTMASOP
01056 *        TRANSLATE BC OP ALT PRICING                       *      ELTMASOP
01057 *                                                          *      ELTMASOP
01058 ************************************************************      ELTMASOP
01059  TRANSLATE-BC-OP-ALT-PRICING.                                     ELTMASOP
01060      PERFORM GET-BC-ALT-PRICING-FIXED-TEXT.                       ELTMASOP
01061      MOVE 'MA-BC-OP-ALT-PRICING-METH' TO CMF-ELEMENT-SYSTEM-NAME. ELTMASOP
01062      MOVE GSS-MA-BC-OP-ALT-PRICING-METH (GSS-INDEX) TO            ELTMASOP
01063          CMF-CODE-VALUE.                                          ELTMASOP
01064      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01065      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01066      PERFORM WRITE-FINAL-OP-ALT-FIXED-TEXT.                       ELTMASOP
01067                                                                   ELTMASOP
01068                                                                   ELTMASOP
01069 ************************************************************      ELTMASOP
01070 *                                                          *      ELTMASOP
01071 *        WRITE FINAL OP ALT FIXED TEXT                     *      ELTMASOP
01072 *                                                          *      ELTMASOP
01073 ************************************************************      ELTMASOP
01074  WRITE-FINAL-OP-ALT-FIXED-TEXT.                                   ELTMASOP
01075      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMASOP
01076      MOVE ' OUTPATIENT.' TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELTMASOP
01077      PERFORM REFORMAT-AND-WRITE-TEXT.                             ELTMASOP
01078      EJECT                                                        ELTMASOP
01079                                                                   ELTMASOP
01080                                                                   ELTMASOP
01081 ************************************************************      ELTMASOP
01082 *                                                          *      ELTMASOP
01083 *        GENERATE ASSOCIATED ACCUMULATORS                  *      ELTMASOP
01084 *                                                          *      ELTMASOP
01085 ************************************************************      ELTMASOP
01086  GENERATE-ASSOCIATED-ACCUMULATO.                                  ELTMASOP
01087      PERFORM GENERATE-COINSURANCE-TEXT.                           ELTMASOP
01088      PERFORM GENERATE-COPAY-TEXT.                                 ELTMASOP
01089      PERFORM GENERATE-DEDUCTIBLE-TEXT.                            ELTMASOP
01090      PERFORM GENERATE-BENEFIT-MAXIMUMS-TEXT.                      ELTMASOP
01091                                                                   ELTMASOP
01092                                                                   ELTMASOP
01093 ************************************************************      ELTMASOP
01094 *                                                          *      ELTMASOP
01095 *        GENERATE COINSURANCE TEXT                         *      ELTMASOP
01096 *                                                          *      ELTMASOP
01097 ************************************************************      ELTMASOP
01098  GENERATE-COINSURANCE-TEXT.                                       ELTMASOP
01099      EXEC CICS LINK                                               ELTMASOP
01100          PROGRAM ('ELGACLCC')                                     ELTMASOP
01101          COMMAREA (DFHCOMMAREA)                                   ELTMASOP
01102          END-EXEC.                                                ELTMASOP
01103                                                                   ELTMASOP
01104 ************************************************************      ELTMASOP
01105 *                                                          *      ELTMASOP
01106 *        GENERATE COPAY-TEXT                               *      ELTMASOP
01107 *                                                          *      ELTMASOP
01108 ************************************************************      ELTMASOP
01109  GENERATE-COPAY-TEXT.                                             ELTMASOP
01110      EXEC CICS LINK                                               ELTMASOP
01111          PROGRAM ('ELGACPCC')                                     ELTMASOP
01112          COMMAREA (DFHCOMMAREA)                                   ELTMASOP
01113          END-EXEC.                                                ELTMASOP
01114                                                                   ELTMASOP
01115                                                                   ELTMASOP
01116 ************************************************************      ELTMASOP
01117 *                                                          *      ELTMASOP
01118 *        GENERATE DEDUCTIBLE TEXT                          *      ELTMASOP
01119 *                                                          *      ELTMASOP
01120 ************************************************************      ELTMASOP
01121  GENERATE-DEDUCTIBLE-TEXT.                                        ELTMASOP
01122      EXEC CICS LINK                                               ELTMASOP
01123          PROGRAM ('ELGADLCC')                                     ELTMASOP
01124          COMMAREA (DFHCOMMAREA)                                   ELTMASOP
01125          END-EXEC.                                                ELTMASOP
01126                                                                   ELTMASOP
01127                                                                   ELTMASOP
01128 ************************************************************      ELTMASOP
01129 *                                                          *      ELTMASOP
01130 *        GENERATE BENEFIT MAXIMUMS TEXT                    *      ELTMASOP
01131 *                                                          *      ELTMASOP
01132 ************************************************************      ELTMASOP
01133  GENERATE-BENEFIT-MAXIMUMS-TEXT.                                  ELTMASOP
01134      EXEC CICS LINK                                               ELTMASOP
01135          PROGRAM ('ELGABMCC')                                     ELTMASOP
01136          COMMAREA (DFHCOMMAREA)                                   ELTMASOP
01137          END-EXEC.                                                ELTMASOP
01138      EJECT                                                        ELTMASOP
01139                                                                   ELTMASOP
01140                                                                   ELTMASOP
01141 ************************************************************      ELTMASOP
01142 *                                                          *      ELTMASOP
01143 *        GENERATE BC CALC METHOD SENTENCE                  *      ELTMASOP
01144 *                                                          *      ELTMASOP
01145 ************************************************************      ELTMASOP
01146  GENERATE-BC-CALC-METHOD-SENTEN.                                  ELTMASOP
01147      IF GSS-MA-BC-CALC-METHOD (GSS-INDEX)  NOT EQUAL              ELTMASOP
01148                  SPACES AND ZEROES AND LOW-VALUES                 ELTMASOP
01149          PERFORM CREATE-BC-CALC-SENTENCE.                         ELTMASOP
01150                                                                   ELTMASOP
01151                                                                   ELTMASOP
01152 ************************************************************      ELTMASOP
01153 *                                                          *      ELTMASOP
01154 *        CREATE BC CALC SENTENCE                           *      ELTMASOP
01155 *                                                          *      ELTMASOP
01156 ************************************************************      ELTMASOP
01157  CREATE-BC-CALC-SENTENCE.                                         ELTMASOP
01158      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMASOP
01159      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMASOP
01160      SET PERIOD-NEEDED TO TRUE.                                   ELTMASOP
01161      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
01162      MOVE 'MA-BC-CALC-METHOD' TO                                  ELTMASOP
01163          CMF-ELEMENT-SYSTEM-NAME.                                 ELTMASOP
01164      MOVE GSS-MA-BC-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMASOP
01165      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01166      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01167      EJECT                                                        ELTMASOP
01168                                                                   ELTMASOP
01169                                                                   ELTMASOP
01170 ************************************************************      ELTMASOP
01171 *                                                          *      ELTMASOP
01172 *        GENERATE BS CALC METHOD SENTENCE                  *      ELTMASOP
01173 *                                                          *      ELTMASOP
01174 ************************************************************      ELTMASOP
01175  GENERATE-BS-CALC-METHOD-SENTEN.                                  ELTMASOP
01176      IF GSS-MA-BS-CALC-METHOD (GSS-INDEX) NOT EQUAL               ELTMASOP
01177                  SPACES AND ZEROES AND LOW-VALUES                 ELTMASOP
01178          PERFORM CREATE-BS-CALC-SENTENCE.                         ELTMASOP
01179                                                                   ELTMASOP
01180                                                                   ELTMASOP
01181 ************************************************************      ELTMASOP
01182 *                                                          *      ELTMASOP
01183 *        CREATE BS CALC SENTENCE                           *      ELTMASOP
01184 *                                                          *      ELTMASOP
01185 ************************************************************      ELTMASOP
01186  CREATE-BS-CALC-SENTENCE.                                         ELTMASOP
01187      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMASOP
01188      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMASOP
01189      SET PERIOD-NEEDED TO TRUE.                                   ELTMASOP
01190      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
01191      MOVE 'MA-BS-CALC-METHOD' TO                                  ELTMASOP
01192          CMF-ELEMENT-SYSTEM-NAME.                                 ELTMASOP
01193      MOVE GSS-MA-BS-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMASOP
01194      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01195      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01196      EJECT                                                        ELTMASOP
01197                                                                   ELTMASOP
01198                                                                   ELTMASOP
01199 ************************************************************      ELTMASOP
01200 *                                                          *      ELTMASOP
01201 *        GENERATE MM CALC METHOD SENTENCE                  *      ELTMASOP
01202 *                                                          *      ELTMASOP
01203 ************************************************************      ELTMASOP
01204  GENERATE-MM-CALC-METHOD-SENTEN.                                  ELTMASOP
01205      IF GSS-MA-MM-CALC-METHOD (GSS-INDEX) NOT EQUAL ZEROES        ELTMASOP
01206          AND                                                      ELTMASOP
01207                  SPACES  AND LOW-VALUES                           ELTMASOP
01208          PERFORM CREATE-MM-CALC-SENTENCE.                         ELTMASOP
01209                                                                   ELTMASOP
01210                                                                   ELTMASOP
01211 ************************************************************      ELTMASOP
01212 *                                                          *      ELTMASOP
01213 *        CREATE MM CALC SENTENCE                           *      ELTMASOP
01214 *                                                          *      ELTMASOP
01215 ************************************************************      ELTMASOP
01216  CREATE-MM-CALC-SENTENCE.                                         ELTMASOP
01217      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMASOP
01218      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMASOP
01219      SET PERIOD-NEEDED TO TRUE.                                   ELTMASOP
01220      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
01221      MOVE 'MA-MM-CALC-METHOD' TO                                  ELTMASOP
01222          CMF-ELEMENT-SYSTEM-NAME.                                 ELTMASOP
01223      MOVE GSS-MA-MM-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMASOP
01224      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01225      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01226                                                                   ELTMASOP
01227                                                                   ELTMASOP
01228 ************************************************************      ELTMASOP
01229 *                                                          *      ELTMASOP
01230 *        GENERATE COMBINED BEN REDUCT SENT                 *      ELTMASOP
01231 *                                                          *      ELTMASOP
01232 ************************************************************      ELTMASOP
01233  GENERATE-COMBINED-BEN-REDUCT-S.                                  ELTMASOP
01234      MOVE 'MA' TO SRP-COST-CONT-TYPE.                             ELTMASOP
01235      MOVE 'MANDATORY ADDITIONAL SECOND OPINION PROGRAM' TO        ELTMASOP
01236            SRP-CCP-NAME.                                          ELTMASOP
01237      MOVE GSS-MA-COMB-BENE-REDUCT-IND (GSS-INDEX)                 ELTMASOP
01238                TO SRP-CCP-COMB-BENE-REDUCT-IND.                   ELTMASOP
01239      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELTMASOP
01240      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMASOP
01241                            ADDRESS OF GCCP-TABULAR-REC.           ELTMASOP
01242      CALL 'ELGCBRI' USING DFHEIBLK                                ELTMASOP
01243                          DFHCOMMAREA.                             ELTMASOP
01244      EJECT                                                        ELTMASOP
01245                                                                   ELTMASOP
01246                                                                   ELTMASOP
01247 ************************************************************      ELTMASOP
01248 *                                                          *      ELTMASOP
01249 *        GENERATE RELATED PROCEDURES                       *      ELTMASOP
01250 *                                                          *      ELTMASOP
01251 ************************************************************      ELTMASOP
01252  GENERATE-RELATED-PROCEDURES.                                     ELTMASOP
01253      MOVE 'MANDATORY ADDITIONAL SECOND OPINION' TO                ELTMASOP
01254          SRP-CCP-NAME.                                            ELTMASOP
01255      MOVE WS-GMOR-PROC-ID TO SRP-TABULAR-ID.                      ELTMASOP
01256      MOVE WS-GMOR-PROC-SLOT-NO TO SRP-TABULAR-SLOT-NO.            ELTMASOP
01257      EXEC CICS LINK                                               ELTMASOP
01258          PROGRAM ('ELGGXXR')                                      ELTMASOP
01259          COMMAREA (DFHCOMMAREA)                                   ELTMASOP
01260          END-EXEC.                                                ELTMASOP
01261      EJECT                                                        ELTMASOP
01262                                                                   ELTMASOP
01263                                                                   ELTMASOP
01264 ************************************************************      ELTMASOP
01265 *                                                          *      ELTMASOP
01266 *        GENERATE RELATED SERVICES                         *      ELTMASOP
01267 *                                                          *      ELTMASOP
01268 ************************************************************      ELTMASOP
01269  GENERATE-RELATED-SERVICES.                                       ELTMASOP
01270      MOVE 'MANDATORY ADDITIONAL SECOND OPINION' TO                ELTMASOP
01271          SRP-CCP-NAME.                                            ELTMASOP
01272      MOVE WS-GMOB-SRVS-ID TO SRP-TABULAR-ID.                      ELTMASOP
01273      MOVE WS-GMOB-SRVS-SLOT-NO TO SRP-TABULAR-SLOT-NO.            ELTMASOP
01274      EXEC CICS LINK                                               ELTMASOP
01275          PROGRAM ('ELGGXXB')                                      ELTMASOP
01276          COMMAREA (DFHCOMMAREA)                                   ELTMASOP
01277          END-EXEC.                                                ELTMASOP
01278      EJECT                                                        ELTMASOP
01279                                                                   ELTMASOP
01280                                                                   ELTMASOP
01281 ************************************************************      ELTMASOP
01282 *                                                          *      ELTMASOP
01283 *        GENERATE BC BEN REDUCT SENT                       *      ELTMASOP
01284 *                                                          *      ELTMASOP
01285 ************************************************************      ELTMASOP
01286  GENERATE-BC-BEN-REDUCT-SENT.                                     ELTMASOP
01287      IF GSS-MA-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT               ELTMASOP
01288                 EQUAL ZERO AND SPACES AND LOW-VALUES              ELTMASOP
01289          PERFORM GENERATE-COMBINED-BEN-REDUCT-S.                  ELTMASOP
01290      IF (GSS-MA-BC-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL          ELTMASOP
01291          ZERO                                                     ELTMASOP
01292                             AND SPACES AND LOW-VALUES) OR         ELTMASOP
01293                 (GSS-MA-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT        ELTMASOP
01294          EQUAL ZERO                                               ELTMASOP
01295                             AND SPACES AND LOW-VALUES)   OR       ELTMASOP
01296                 (GSS-MA-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT      ELTMASOP
01297          EQUAL ZERO                                               ELTMASOP
01298                             AND SPACES AND LOW-VALUES)            ELTMASOP
01299          PERFORM GENERATE-BC-BENEFITS-REDUCTION.                  ELTMASOP
01300      EJECT                                                        ELTMASOP
01301                                                                   ELTMASOP
01302                                                                   ELTMASOP
01303 ************************************************************      ELTMASOP
01304 *                                                          *      ELTMASOP
01305 *        GENERATE BC BENEFITS REDUCTIONS TEXT              *      ELTMASOP
01306 *                                                          *      ELTMASOP
01307 ************************************************************      ELTMASOP
01308  GENERATE-BC-BENEFITS-REDUCTION.                                  ELTMASOP
01309      PERFORM GENERATE-REDUCTIONS-HEADINGS.                        ELTMASOP
01310      IF GSS-MA-BC-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMASOP
01311          ZERO                                                     ELTMASOP
01312                             AND SPACES AND LOW-VALUES             ELTMASOP
01313          PERFORM TRANSLATE-BC-DEDU-APPLIC.                        ELTMASOP
01314      IF GSS-MA-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMASOP
01315          ZERO                                                     ELTMASOP
01316                             AND SPACES AND LOW-VALUES             ELTMASOP
01317          PERFORM TRANSLATE-BC-OPEX-APPLIC.                        ELTMASOP
01318      PERFORM CREATE-A-BLANK-LINE.                                 ELTMASOP
01319      PERFORM LINK-TO-OUTPUT.                                      ELTMASOP
01320      IF GSS-MA-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTMASOP
01321          ZERO                                                     ELTMASOP
01322                             AND SPACES AND LOW-VALUES             ELTMASOP
01323          PERFORM TRANSLATE-BC-OPEX-OVERRIDE.                      ELTMASOP
01324      EJECT                                                        ELTMASOP
01325                                                                   ELTMASOP
01326                                                                   ELTMASOP
01327 ************************************************************      ELTMASOP
01328 *                                                          *      ELTMASOP
01329 *        GENERATE REDUCTIONS HEADINGS                      *      ELTMASOP
01330 *                                                          *      ELTMASOP
01331 ************************************************************      ELTMASOP
01332  GENERATE-REDUCTIONS-HEADINGS.                                    ELTMASOP
01333      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMASOP
01334      INITIALIZE ADDITIONAL-TEXT-SW.                               ELTMASOP
01335      INITIALIZE WS-PERIOD-SW.                                     ELTMASOP
01336      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMASOP
01337      MOVE WS-BENEFITS-REDUCTION TO COF-DTL-LINE                   ELTMASOP
01338          (COF-NBR-DTL-LINES).                                     ELTMASOP
01339      PERFORM LINK-TO-OUTPUT.                                      ELTMASOP
01340      EJECT                                                        ELTMASOP
01341                                                                   ELTMASOP
01342                                                                   ELTMASOP
01343 ************************************************************      ELTMASOP
01344 *                                                          *      ELTMASOP
01345 *        GENERATE ADDITIONAL TABULARS                      *      ELTMASOP
01346 *                                                          *      ELTMASOP
01347 ************************************************************      ELTMASOP
01348  GENERATE-ADDITIONAL-TABULARS.                                    ELTMASOP
01349      PERFORM OBTAIN-GMOB.                                         ELTMASOP
01350      PERFORM OBTAIN-GMOR.                                         ELTMASOP
01351      EJECT                                                        ELTMASOP
01352                                                                   ELTMASOP
01353                                                                   ELTMASOP
01354 ************************************************************      ELTMASOP
01355 *                                                          *      ELTMASOP
01356 *        TRANSLATE BS INDICATOR                            *      ELTMASOP
01357 *                                                          *      ELTMASOP
01358 ************************************************************      ELTMASOP
01359  TRANSLATE-BS-INDICATOR.                                          ELTMASOP
01360      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMASOP
01361      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMASOP
01362      SET PERIOD-NEEDED TO TRUE.                                   ELTMASOP
01363      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
01364      MOVE 'MA-BS-IND' TO CMF-ELEMENT-SYSTEM-NAME.                 ELTMASOP
01365      MOVE GSS-MA-BS-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTMASOP
01366      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01367      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01368      EJECT                                                        ELTMASOP
01369                                                                   ELTMASOP
01370                                                                   ELTMASOP
01371 ************************************************************      ELTMASOP
01372 *                                                          *      ELTMASOP
01373 *        TRANSLATE BS IP ALT PRICING                       *      ELTMASOP
01374 *                                                          *      ELTMASOP
01375 ************************************************************      ELTMASOP
01376  TRANSLATE-BS-IP-ALT-PRICING.                                     ELTMASOP
01377      PERFORM GET-BS-ALT-PRICING-FIXED-TEXT.                       ELTMASOP
01378      MOVE 'MA-BS-IP-ALT-PRICING-METH' TO CMF-ELEMENT-SYSTEM-NAME. ELTMASOP
01379      MOVE GSS-MA-BS-IP-ALT-PRICING-METH (GSS-INDEX) TO            ELTMASOP
01380          CMF-CODE-VALUE.                                          ELTMASOP
01381      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01382      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01383      PERFORM WRITE-FINAL-IP-ALT-FIXED-TEXT.                       ELTMASOP
01384      EJECT                                                        ELTMASOP
01385                                                                   ELTMASOP
01386                                                                   ELTMASOP
01387 ************************************************************      ELTMASOP
01388 *                                                          *      ELTMASOP
01389 *        TRANSLATE BS OP ALT PRICING                       *      ELTMASOP
01390 *                                                          *      ELTMASOP
01391 ************************************************************      ELTMASOP
01392  TRANSLATE-BS-OP-ALT-PRICING.                                     ELTMASOP
01393      PERFORM GET-BS-ALT-PRICING-FIXED-TEXT.                       ELTMASOP
01394      MOVE 'MA-BS-OP-ALT-PRICING-METH' TO CMF-ELEMENT-SYSTEM-NAME. ELTMASOP
01395      MOVE GSS-MA-BS-OP-ALT-PRICING-METH (GSS-INDEX) TO            ELTMASOP
01396          CMF-CODE-VALUE.                                          ELTMASOP
01397      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01398      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01399      PERFORM WRITE-FINAL-OP-ALT-FIXED-TEXT.                       ELTMASOP
01400                                                                   ELTMASOP
01401                                                                   ELTMASOP
01402 ************************************************************      ELTMASOP
01403 *                                                          *      ELTMASOP
01404 *        GET BS ALT PRICING FIXED TEXT                     *      ELTMASOP
01405 *                                                          *      ELTMASOP
01406 ************************************************************      ELTMASOP
01407  GET-BS-ALT-PRICING-FIXED-TEXT.                                   ELTMASOP
01408      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMASOP
01409      INITIALIZE WS-PERIOD-SW.                                     ELTMASOP
01410      SET ADDITIONAL-TEXT TO TRUE.                                 ELTMASOP
01411      MOVE WS-BS-ALT-PRICING TO TCAR-FROM-LINE (TCAR-FROM-SUB).    ELTMASOP
01412      ADD 1 TO TCAR-FROM-SUB.                                      ELTMASOP
01413      EJECT                                                        ELTMASOP
01414                                                                   ELTMASOP
01415                                                                   ELTMASOP
01416 ************************************************************      ELTMASOP
01417 *                                                          *      ELTMASOP
01418 *        GENERATE BS BEN REDUCT SENT                       *      ELTMASOP
01419 *                                                          *      ELTMASOP
01420 ************************************************************      ELTMASOP
01421  GENERATE-BS-BEN-REDUCT-SENT.                                     ELTMASOP
01422      IF GSS-MA-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT               ELTMASOP
01423                 EQUAL ZERO AND SPACES AND LOW-VALUES              ELTMASOP
01424          PERFORM GENERATE-COMBINED-BEN-REDUCT-S.                  ELTMASOP
01425      IF (GSS-MA-BS-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL          ELTMASOP
01426          ZERO                                                     ELTMASOP
01427                      AND SPACES AND LOW-VALUES)  OR               ELTMASOP
01428                 (GSS-MA-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT        ELTMASOP
01429          EQUAL ZERO                                               ELTMASOP
01430                      AND SPACES AND LOW-VALUES)  OR               ELTMASOP
01431                 (GSS-MS-BS-OPEX-OVERRIDE-IND (GSS-INDEX) NOT      ELTMASOP
01432          EQUAL ZERO                                               ELTMASOP
01433                      AND SPACES AND LOW-VALUES)                   ELTMASOP
01434          PERFORM GENERATE-BS-BENEFITS-REDUCTION.                  ELTMASOP
01435      EJECT                                                        ELTMASOP
01436                                                                   ELTMASOP
01437                                                                   ELTMASOP
01438 ************************************************************      ELTMASOP
01439 *                                                          *      ELTMASOP
01440 *        GENERATE BS BENEFITS REDUCTIONS TEXT              *      ELTMASOP
01441 *                                                          *      ELTMASOP
01442 ************************************************************      ELTMASOP
01443  GENERATE-BS-BENEFITS-REDUCTION.                                  ELTMASOP
01444      PERFORM GENERATE-REDUCTIONS-HEADINGS.                        ELTMASOP
01445      IF GSS-MA-BS-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMASOP
01446          ZERO                                                     ELTMASOP
01447                      AND SPACES AND LOW-VALUES                    ELTMASOP
01448          PERFORM TRANSLATE-BS-DEDU-APPLIC.                        ELTMASOP
01449      IF GSS-MA-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMASOP
01450          ZERO                                                     ELTMASOP
01451                      AND SPACES AND LOW-VALUES                    ELTMASOP
01452          PERFORM TRANSLATE-BS-OPEX-APPLIC.                        ELTMASOP
01453      PERFORM CREATE-A-BLANK-LINE.                                 ELTMASOP
01454      PERFORM LINK-TO-OUTPUT.                                      ELTMASOP
01455      IF GSS-MA-BS-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTMASOP
01456          ZERO                                                     ELTMASOP
01457                      AND SPACES AND LOW-VALUES                    ELTMASOP
01458          PERFORM TRANSLATE-BS-OPEX-OVERRIDE.                      ELTMASOP
01459      EJECT                                                        ELTMASOP
01460                                                                   ELTMASOP
01461                                                                   ELTMASOP
01462 ************************************************************      ELTMASOP
01463 *                                                          *      ELTMASOP
01464 *        TRANSLATE MM INDICATOR                            *      ELTMASOP
01465 *                                                          *      ELTMASOP
01466 ************************************************************      ELTMASOP
01467  TRANSLATE-MM-INDICATOR.                                          ELTMASOP
01468      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMASOP
01469      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMASOP
01470      SET PERIOD-NEEDED TO TRUE.                                   ELTMASOP
01471      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
01472      MOVE 'MA-MM-IND'  TO CMF-ELEMENT-SYSTEM-NAME.                ELTMASOP
01473      MOVE GSS-MA-MM-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTMASOP
01474      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01475      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01476      EJECT                                                        ELTMASOP
01477                                                                   ELTMASOP
01478                                                                   ELTMASOP
01479 ************************************************************      ELTMASOP
01480 *                                                          *      ELTMASOP
01481 *        TRANSLATE MM IP ALT PRICING                       *      ELTMASOP
01482 *                                                          *      ELTMASOP
01483 ************************************************************      ELTMASOP
01484  TRANSLATE-MM-IP-ALT-PRICING.                                     ELTMASOP
01485      PERFORM GET-MM-ALT-PRICING-FIXED-TEXT.                       ELTMASOP
01486      MOVE 'MA-MM-IP-ALT-PRICING-METH' TO                          ELTMASOP
01487          CMF-ELEMENT-SYSTEM-NAME.                                 ELTMASOP
01488      MOVE GSS-MA-MM-IP-ALT-PRICING-METH (GSS-INDEX) TO            ELTMASOP
01489          CMF-CODE-VALUE.                                          ELTMASOP
01490      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01491      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01492      PERFORM WRITE-FINAL-IP-ALT-FIXED-TEXT.                       ELTMASOP
01493      EJECT                                                        ELTMASOP
01494                                                                   ELTMASOP
01495                                                                   ELTMASOP
01496 ************************************************************      ELTMASOP
01497 *                                                          *      ELTMASOP
01498 *        TRANSLATE MM OP ALT PRICING                       *      ELTMASOP
01499 *                                                          *      ELTMASOP
01500 ************************************************************      ELTMASOP
01501  TRANSLATE-MM-OP-ALT-PRICING.                                     ELTMASOP
01502      PERFORM GET-MM-ALT-PRICING-FIXED-TEXT.                       ELTMASOP
01503      MOVE 'MA-MM-OP-ALT-PRICING-METH' TO CMF-ELEMENT-SYSTEM-NAME. ELTMASOP
01504      MOVE GSS-MA-MM-OP-ALT-PRICING-METH (GSS-INDEX) TO            ELTMASOP
01505          CMF-CODE-VALUE.                                          ELTMASOP
01506      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01507      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01508      PERFORM WRITE-FINAL-OP-ALT-FIXED-TEXT.                       ELTMASOP
01509                                                                   ELTMASOP
01510                                                                   ELTMASOP
01511 ************************************************************      ELTMASOP
01512 *                                                          *      ELTMASOP
01513 *        GET MM ALT PRICING FIXED TEXT                     *      ELTMASOP
01514 *                                                          *      ELTMASOP
01515 ************************************************************      ELTMASOP
01516  GET-MM-ALT-PRICING-FIXED-TEXT.                                   ELTMASOP
01517      SET ADDITIONAL-TEXT TO TRUE.                                 ELTMASOP
01518      INITIALIZE WS-PERIOD-SW.                                     ELTMASOP
01519      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
01520      MOVE WS-BS-ALT-PRICING TO TCAR-FROM-LINE (TCAR-FROM-SUB).    ELTMASOP
01521      ADD 1 TO TCAR-FROM-SUB.                                      ELTMASOP
01522      EJECT                                                        ELTMASOP
01523                                                                   ELTMASOP
01524                                                                   ELTMASOP
01525 ************************************************************      ELTMASOP
01526 *                                                          *      ELTMASOP
01527 *        GENERATE MM BEN REDUCT SENT                       *      ELTMASOP
01528 *                                                          *      ELTMASOP
01529 ************************************************************      ELTMASOP
01530  GENERATE-MM-BEN-REDUCT-SENT.                                     ELTMASOP
01531      IF GSS-MA-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT               ELTMASOP
01532                 EQUAL ZERO AND SPACES AND LOW-VALUES              ELTMASOP
01533          PERFORM GENERATE-COMBINED-BEN-REDUCT-S.                  ELTMASOP
01534      IF (GSS-MA-MM-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL          ELTMASOP
01535          ZERO                                                     ELTMASOP
01536                             AND SPACES AND LOW-VALUES) OR         ELTMASOP
01537                 (GSS-MA-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT        ELTMASOP
01538          EQUAL ZERO                                               ELTMASOP
01539                             AND SPACES AND LOW-VALUES)  OR        ELTMASOP
01540                 (GSS-MA-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT      ELTMASOP
01541          EQUAL ZERO                                               ELTMASOP
01542                             AND SPACES AND LOW-VALUES)            ELTMASOP
01543          PERFORM GENERATE-MM-BENEFITS-REDUCTION.                  ELTMASOP
01544      EJECT                                                        ELTMASOP
01545                                                                   ELTMASOP
01546                                                                   ELTMASOP
01547 ************************************************************      ELTMASOP
01548 *                                                          *      ELTMASOP
01549 *        GENERATE MM BENEFITS REDUCTIONS TEXT              *      ELTMASOP
01550 *                                                          *      ELTMASOP
01551 ************************************************************      ELTMASOP
01552  GENERATE-MM-BENEFITS-REDUCTION.                                  ELTMASOP
01553      PERFORM GENERATE-REDUCTIONS-HEADINGS.                        ELTMASOP
01554      IF GSS-MA-MM-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMASOP
01555          ZERO                                                     ELTMASOP
01556                             AND SPACES AND LOW-VALUES             ELTMASOP
01557          PERFORM TRANSLATE-MM-DEDU-APPLIC.                        ELTMASOP
01558      IF GSS-MA-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMASOP
01559          ZERO                                                     ELTMASOP
01560                             AND SPACES AND LOW-VALUES             ELTMASOP
01561          PERFORM TRANSLATE-MM-OPEX-APPLIC.                        ELTMASOP
01562      PERFORM CREATE-A-BLANK-LINE.                                 ELTMASOP
01563      PERFORM LINK-TO-OUTPUT.                                      ELTMASOP
01564      IF GSS-MA-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTMASOP
01565          ZERO                                                     ELTMASOP
01566                             AND SPACES AND LOW-VALUES             ELTMASOP
01567          PERFORM TRANSLATE-MM-OPEX-OVERRIDE.                      ELTMASOP
01568                                                                   ELTMASOP
01569                                                                   ELTMASOP
01570 ************************************************************      ELTMASOP
01571 *                                                          *      ELTMASOP
01572 *        OBTAIN GMOB                                       *      ELTMASOP
01573 *                                                          *      ELTMASOP
01574 ************************************************************      ELTMASOP
01575  OBTAIN-GMOB.                                                     ELTMASOP
01576      SET GCG-INDEX TO 1.                                          ELTMASOP
01577      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTMASOP
01578            AT END                                                 ELTMASOP
01579               MOVE ZEROES TO WS-GMOB-SRVS-SLOT-NO                 ELTMASOP
01580            WHEN GCG-TAB-ID (GCG-INDEX) EQUAL PC-GMOB              ELTMASOP
01581               MOVE GCG-TAB-ID (GCG-INDEX) TO                      ELTMASOP
01582          WS-GMOB-SRVS-ID                                          ELTMASOP
01583               MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                 ELTMASOP
01584          WS-GMOB-SRVS-SLOT-NO                                     ELTMASOP
01585         END-SEARCH.                                               ELTMASOP
01586      IF WS-GMOB-SRVS-SLOT-NO NOT EQUAL ZEROES                     ELTMASOP
01587                 AND WS-GMOB-SRVS-ID EQUAL PC-GMOB                 ELTMASOP
01588          PERFORM DISPLAY-RELATED-SERVICES-SENTE.                  ELTMASOP
01589      IF WS-GMOB-SRVS-SLOT-NO NOT EQUAL ZEROES                     ELTMASOP
01590                  AND WS-GMOB-SRVS-ID  EQUAL PC-GMOB               ELTMASOP
01591          PERFORM GENERATE-RELATED-SERVICES.                       ELTMASOP
01592      EJECT                                                        ELTMASOP
01593                                                                   ELTMASOP
01594                                                                   ELTMASOP
01595 ************************************************************      ELTMASOP
01596 *                                                          *      ELTMASOP
01597 *        OBTAIN GMOR                                       *      ELTMASOP
01598 *                                                          *      ELTMASOP
01599 ************************************************************      ELTMASOP
01600  OBTAIN-GMOR.                                                     ELTMASOP
01601      SET GCG-INDEX TO 1.                                          ELTMASOP
01602      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTMASOP
01603            AT END                                                 ELTMASOP
01604               MOVE ZEROES TO WS-GMOR-PROC-SLOT-NO                 ELTMASOP
01605            WHEN GCG-TAB-ID (GCG-INDEX) EQUAL PC-GMOR              ELTMASOP
01606               MOVE GCG-TAB-ID (GCG-INDEX) TO                      ELTMASOP
01607          WS-GMOR-PROC-ID                                          ELTMASOP
01608               MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                    ELTMASOP
01609                   TO WS-GMOR-PROC-SLOT-NO                         ELTMASOP
01610         END-SEARCH.                                               ELTMASOP
01611      IF WS-GMOR-PROC-SLOT-NO NOT EQUAL ZEROES                     ELTMASOP
01612                 AND WS-GMOR-PROC-ID EQUAL PC-GMOR                 ELTMASOP
01613          PERFORM DISPLAY-RELATED-PROCEDURES-SEN.                  ELTMASOP
01614      IF WS-GMOR-PROC-SLOT-NO NOT EQUAL ZEROES                     ELTMASOP
01615                 AND WS-GMOR-PROC-ID  EQUAL PC-GMOR                ELTMASOP
01616          PERFORM GENERATE-RELATED-PROCEDURES.                     ELTMASOP
01617                                                                   ELTMASOP
01618                                                                   ELTMASOP
01619 ************************************************************      ELTMASOP
01620 *                                                          *      ELTMASOP
01621 *        DISPLAY RELATED PROCEDURES SENTENCE               *      ELTMASOP
01622 *                                                          *      ELTMASOP
01623 ************************************************************      ELTMASOP
01624  DISPLAY-RELATED-PROCEDURES-SEN.                                  ELTMASOP
01625      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMASOP
01626      MOVE SPECIAL-PROCEDURES-MSG  TO COF-DTL-LINE                 ELTMASOP
01627          (COF-NBR-DTL-LINES).                                     ELTMASOP
01628      PERFORM LINK-TO-OUTPUT.                                      ELTMASOP
01629      EJECT                                                        ELTMASOP
01630                                                                   ELTMASOP
01631                                                                   ELTMASOP
01632 ************************************************************      ELTMASOP
01633 *                                                          *      ELTMASOP
01634 *        DISPLAY RELATED SERVICES SENTENCE                 *      ELTMASOP
01635 *                                                          *      ELTMASOP
01636 ************************************************************      ELTMASOP
01637  DISPLAY-RELATED-SERVICES-SENTE.                                  ELTMASOP
01638      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMASOP
01639      MOVE SPECIAL-SERVICES-MSG  TO COF-DTL-LINE                   ELTMASOP
01640          (COF-NBR-DTL-LINES).                                     ELTMASOP
01641      PERFORM LINK-TO-OUTPUT.                                      ELTMASOP
01642      EJECT                                                        ELTMASOP
01643                                                                   ELTMASOP
01644                                                                   ELTMASOP
01645 ************************************************************      ELTMASOP
01646 *                                                          *      ELTMASOP
01647 *        TRANSLATE BC DEDU APPLIC                          *      ELTMASOP
01648 *                                                          *      ELTMASOP
01649 ************************************************************      ELTMASOP
01650  TRANSLATE-BC-DEDU-APPLIC.                                        ELTMASOP
01651      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMASOP
01652      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
01653      MOVE  'MA-BC-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.    ELTMASOP
01654      MOVE GSS-MA-BC-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTMASOP
01655          CMF-CODE-VALUE.                                          ELTMASOP
01656      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01657      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01658      EJECT                                                        ELTMASOP
01659                                                                   ELTMASOP
01660                                                                   ELTMASOP
01661 ************************************************************      ELTMASOP
01662 *                                                          *      ELTMASOP
01663 *        TRANSLATE BC OPEX APPLIC                          *      ELTMASOP
01664 *                                                          *      ELTMASOP
01665 ************************************************************      ELTMASOP
01666  TRANSLATE-BC-OPEX-APPLIC.                                        ELTMASOP
01667      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMASOP
01668      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
01669      MOVE  'MA-BC-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.    ELTMASOP
01670      MOVE GSS-MA-BC-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTMASOP
01671          CMF-CODE-VALUE.                                          ELTMASOP
01672      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01673      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01674      EJECT                                                        ELTMASOP
01675                                                                   ELTMASOP
01676                                                                   ELTMASOP
01677 ************************************************************      ELTMASOP
01678 *                                                          *      ELTMASOP
01679 *        TRANSLATE BC OPEX OVERRIDE                        *      ELTMASOP
01680 *                                                          *      ELTMASOP
01681 ************************************************************      ELTMASOP
01682  TRANSLATE-BC-OPEX-OVERRIDE.                                      ELTMASOP
01683      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMASOP
01684      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMASOP
01685      SET PERIOD-NEEDED TO TRUE.                                   ELTMASOP
01686      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
01687      MOVE 'MA-BC-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTMASOP
01688      MOVE GSS-MA-BC-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTMASOP
01689          CMF-CODE-VALUE.                                          ELTMASOP
01690      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01691      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01692      EJECT                                                        ELTMASOP
01693                                                                   ELTMASOP
01694                                                                   ELTMASOP
01695 ************************************************************      ELTMASOP
01696 *                                                          *      ELTMASOP
01697 *        TRANSLATE BS DEDU APPLIC                          *      ELTMASOP
01698 *                                                          *      ELTMASOP
01699 ************************************************************      ELTMASOP
01700  TRANSLATE-BS-DEDU-APPLIC.                                        ELTMASOP
01701      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMASOP
01702      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
01703      MOVE 'MA-BS-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMASOP
01704      MOVE GSS-MA-BS-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTMASOP
01705          CMF-CODE-VALUE.                                          ELTMASOP
01706      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01707      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01708      EJECT                                                        ELTMASOP
01709                                                                   ELTMASOP
01710                                                                   ELTMASOP
01711 ************************************************************      ELTMASOP
01712 *                                                          *      ELTMASOP
01713 *        TRANSLATE BS OPEX APPLIC                          *      ELTMASOP
01714 *                                                          *      ELTMASOP
01715 ************************************************************      ELTMASOP
01716  TRANSLATE-BS-OPEX-APPLIC.                                        ELTMASOP
01717      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMASOP
01718      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
01719      MOVE 'MA-BS-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMASOP
01720      MOVE GSS-MA-BS-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTMASOP
01721          CMF-CODE-VALUE.                                          ELTMASOP
01722      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01723      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01724      EJECT                                                        ELTMASOP
01725                                                                   ELTMASOP
01726                                                                   ELTMASOP
01727 ************************************************************      ELTMASOP
01728 *                                                          *      ELTMASOP
01729 *        TRANSLATE BS OPEX OVERRIDE                        *      ELTMASOP
01730 *                                                          *      ELTMASOP
01731 ************************************************************      ELTMASOP
01732  TRANSLATE-BS-OPEX-OVERRIDE.                                      ELTMASOP
01733      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMASOP
01734      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMASOP
01735      SET PERIOD-NEEDED TO TRUE.                                   ELTMASOP
01736      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
01737      MOVE 'MA-BS-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTMASOP
01738      MOVE GSS-MA-BS-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTMASOP
01739          CMF-CODE-VALUE.                                          ELTMASOP
01740      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01741      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01742      EJECT                                                        ELTMASOP
01743                                                                   ELTMASOP
01744                                                                   ELTMASOP
01745 ************************************************************      ELTMASOP
01746 *                                                          *      ELTMASOP
01747 *        TRANSLATE MM DEDU APPLIC                          *      ELTMASOP
01748 *                                                          *      ELTMASOP
01749 ************************************************************      ELTMASOP
01750  TRANSLATE-MM-DEDU-APPLIC.                                        ELTMASOP
01751      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMASOP
01752      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
01753      MOVE 'MA-MM-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMASOP
01754      MOVE GSS-MA-MM-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTMASOP
01755          CMF-CODE-VALUE.                                          ELTMASOP
01756      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01757      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01758      EJECT                                                        ELTMASOP
01759                                                                   ELTMASOP
01760                                                                   ELTMASOP
01761 ************************************************************      ELTMASOP
01762 *                                                          *      ELTMASOP
01763 *        TRANSLATE MM OPEX APPLIC                          *      ELTMASOP
01764 *                                                          *      ELTMASOP
01765 ************************************************************      ELTMASOP
01766  TRANSLATE-MM-OPEX-APPLIC.                                        ELTMASOP
01767      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMASOP
01768      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
01769      MOVE 'MA-MM-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMASOP
01770      MOVE GSS-MA-MM-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTMASOP
01771          CMF-CODE-VALUE.                                          ELTMASOP
01772      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01773      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01774      EJECT                                                        ELTMASOP
01775                                                                   ELTMASOP
01776                                                                   ELTMASOP
01777 ************************************************************      ELTMASOP
01778 *                                                          *      ELTMASOP
01779 *        TRANSLATE MM OPEX OVERRIDE                        *      ELTMASOP
01780 *                                                          *      ELTMASOP
01781 ************************************************************      ELTMASOP
01782  TRANSLATE-MM-OPEX-OVERRIDE.                                      ELTMASOP
01783      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMASOP
01784      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMASOP
01785      SET PERIOD-NEEDED TO TRUE.                                   ELTMASOP
01786      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
01787      MOVE 'MA-MM-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTMASOP
01788      MOVE GSS-MA-MM-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTMASOP
01789          CMF-CODE-VALUE.                                          ELTMASOP
01790      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01791      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01792      EJECT                                                        ELTMASOP
01793                                                                   ELTMASOP
01794                                                                   ELTMASOP
01795 ************************************************************      ELTMASOP
01796 *                                                          *      ELTMASOP
01797 *        GENERATE SPILL OVER INDICATOR                     *      ELTMASOP
01798 *                                                          *      ELTMASOP
01799 ************************************************************      ELTMASOP
01800  GENERATE-SPILL-OVER-INDICATOR.                                   ELTMASOP
01801      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMASOP
01802      SET ADDITIONAL-TEXT TO TRUE.                                 ELTMASOP
01803      INITIALIZE WS-PERIOD-SW.                                     ELTMASOP
01804      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
01805      MOVE WS-SPILL-OVER TO TCAR-FROM-LINE (TCAR-FROM-SUB).        ELTMASOP
01806      ADD 1 TO TCAR-FROM-SUB.                                      ELTMASOP
01807      MOVE 'MA-SPILL-OVER-IND' TO CMF-ELEMENT-SYSTEM-NAME.         ELTMASOP
01808      MOVE GSS-MA-SPILL-OVER-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMASOP
01809      PERFORM LINK-TO-TRANSLATOR.                                  ELTMASOP
01810      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMASOP
01811      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMASOP
01812      MOVE WS-END-SPILL-OVER TO TCAR-FROM-LINE (TCAR-FROM-SUB).    ELTMASOP
01813      PERFORM REFORMAT-AND-WRITE-TEXT.                             ELTMASOP
01814                                                                   ELTMASOP
01815                                                                   ELTMASOP
01816 ************************************************************      ELTMASOP
01817 *                                                          *      ELTMASOP
01818 *        SIGNAL NOT APPLICABLE MSG                         *      ELTMASOP
01819 *                                                          *      ELTMASOP
01820 ************************************************************      ELTMASOP
01821  SIGNAL-NOT-APPLICABLE-MSG.                                       ELTMASOP
01822      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTMASOP
01823      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTMASOP
01824          (COF-NBR-DTL-LINES).                                     ELTMASOP
01825                                                                   ELTMASOP
01826                                                                   ELTMASOP
01827 ************************************************************      ELTMASOP
01828 *                                                          *      ELTMASOP
01829 *        SIGNAL VOLUNTARY MSG                              *      ELTMASOP
01830 *                                                          *      ELTMASOP
01831 ************************************************************      ELTMASOP
01832  SIGNAL-VOLUNTARY-MSG.                                            ELTMASOP
01833      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTMASOP
01834      MOVE WS-VOLUNTARY-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).   ELTMASOP
01835                                                                   ELTMASOP
01836                                                                   ELTMASOP
01837 ************************************************************      ELTMASOP
01838 *                                                          *      ELTMASOP
01839 *        LINK TO TRANSLATOR                                *      ELTMASOP
01840 *                                                          *      ELTMASOP
01841 ************************************************************      ELTMASOP
01842  LINK-TO-TRANSLATOR.                                              ELTMASOP
01843      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMASOP
01844      EXEC CICS LINK                                               ELTMASOP
01845           PROGRAM('ELUCMIF')                                      ELTMASOP
01846           COMMAREA(DFHCOMMAREA)                                   ELTMASOP
01847           END-EXEC.                                               ELTMASOP
01848      EJECT                                                        ELTMASOP
01849                                                                   ELTMASOP
01850                                                                   ELTMASOP
01851 ************************************************************      ELTMASOP
01852 *                                                          *      ELTMASOP
01853 *        READ GCCP RECORD                                  *      ELTMASOP
01854 *                                                          *      ELTMASOP
01855 ************************************************************      ELTMASOP
01856  READ-GCCP-RECORD.                                                ELTMASOP
01857      SET CIA-GCTABULR-DDN TO TRUE.                                ELTMASOP
01858      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMASOP
01859                            ADDRESS OF                             ELTMASOP
01860          IOP-INPUT-OUTPUT-PARAMETERS.                             ELTMASOP
01861      SET IOP-STG-MODE-MOVE TO TRUE.                               ELTMASOP
01862      SET IOP-RD              TO TRUE.                             ELTMASOP
01863      SET IOP-FCQ-NONE        TO TRUE.                             ELTMASOP
01864      SET IOP-KVQ-EQ          TO TRUE.                             ELTMASOP
01865      MOVE SPACES TO IOP-AIX-DDNAME.                               ELTMASOP
01866      MOVE KWA-GCTABULR-KEY   TO IOP-FILE-KEY.                     ELTMASOP
01867      PERFORM LINK-TO-I-O-PGM.                                     ELTMASOP
01868      EJECT                                                        ELTMASOP
01869                                                                   ELTMASOP
01870                                                                   ELTMASOP
01871 ************************************************************      ELTMASOP
01872 *                                                          *      ELTMASOP
01873 *        LINK TO I O PGM                                   *      ELTMASOP
01874 *                                                          *      ELTMASOP
01875 ************************************************************      ELTMASOP
01876  LINK-TO-I-O-PGM.                                                 ELTMASOP
01877      EXEC CICS LINK PROGRAM ('ELUIOPGM')                          ELTMASOP
01878           COMMAREA (DFHCOMMAREA)                                  ELTMASOP
01879           END-EXEC.                                               ELTMASOP
01880      IF IOP-RC-OK                                                 ELTMASOP
01881          PERFORM ESTABLISH-ADDRESSABILITY-OF-GC                   ELTMASOP
01882      ELSE IF IOP-RC-NOTFND                                        ELTMASOP
01883          PERFORM SIGNAL-NOT-FOUND-GCTAB                           ELTMASOP
01884      ELSE                                                         ELTMASOP
01885          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTMASOP
01886                                                                   ELTMASOP
01887                                                                   ELTMASOP
01888 ************************************************************      ELTMASOP
01889 *                                                          *      ELTMASOP
01890 *        SIGNAL CRITICAL IO ERROR                          *      ELTMASOP
01891 *                                                          *      ELTMASOP
01892 ************************************************************      ELTMASOP
01893  SIGNAL-CRITICAL-IO-ERROR.                                        ELTMASOP
01894      SET CIA-AB-CRITIO TO TRUE.                                   ELTMASOP
01895      PERFORM SIGNAL-ABEND.                                        ELTMASOP
01896                                                                   ELTMASOP
01897                                                                   ELTMASOP
01898 ************************************************************      ELTMASOP
01899 *                                                          *      ELTMASOP
01900 *        SIGNAL NOT FOUND GCTAB                            *      ELTMASOP
01901 *                                                          *      ELTMASOP
01902 ************************************************************      ELTMASOP
01903  SIGNAL-NOT-FOUND-GCTAB.                                          ELTMASOP
01904      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTMASOP
01905      PERFORM SIGNAL-ABEND.                                        ELTMASOP
01906                                                                   ELTMASOP
01907                                                                   ELTMASOP
01908 ************************************************************      ELTMASOP
01909 *                                                          *      ELTMASOP
01910 *        ESTABLISH ADDRESSABILITY OF GCCP RECORD           *      ELTMASOP
01911 *                                                          *      ELTMASOP
01912 ************************************************************      ELTMASOP
01913  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTMASOP
01914      SET ADDRESS OF GCCP-TABULAR-REC TO IOP-REC-PTR.              ELTMASOP
01915      SET IOP-REC-PTR TO NULL.                                     ELTMASOP
01916      EJECT                                                        ELTMASOP
01917                                                                   ELTMASOP
01918                                                                   ELTMASOP
01919 ************************************************************      ELTMASOP
01920 *                                                          *      ELTMASOP
01921 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTMASOP
01922 *                                                          *      ELTMASOP
01923 ************************************************************      ELTMASOP
01924  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTMASOP
01925      PERFORM INITIALIZE-CMOUT.                                    ELTMASOP
01926      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTMASOP
01927                                                                   ELTMASOP
01928                                                                   ELTMASOP
01929 ************************************************************      ELTMASOP
01930 *                                                          *      ELTMASOP
01931 *        PREPARE TEXT FOR OUTPUT                           *      ELTMASOP
01932 *                                                          *      ELTMASOP
01933 ************************************************************      ELTMASOP
01934  PREPARE-TEXT-FOR-OUTPUT.                                         ELTMASOP
01935      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTMASOP
01936          UNTIL CMF-DESCR-IDX                                      ELTMASOP
01937                                    GREATER THAN                   ELTMASOP
01938              CMF-NBR-DESCR-LINES.                                 ELTMASOP
01939                                                                   ELTMASOP
01940                                                                   ELTMASOP
01941 ************************************************************      ELTMASOP
01942 *                                                          *      ELTMASOP
01943 *        INITIALIZE CMOUT                                  *      ELTMASOP
01944 *                                                          *      ELTMASOP
01945 ************************************************************      ELTMASOP
01946  INITIALIZE-CMOUT.                                                ELTMASOP
01947      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTMASOP
01948      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMASOP
01949          ADDRESS OF CMF-DESCR.                                    ELTMASOP
01950      SET CMF-DESCR-IDX TO 1.                                      ELTMASOP
01951      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTMASOP
01952                                                                   ELTMASOP
01953                                                                   ELTMASOP
01954 ************************************************************      ELTMASOP
01955 *                                                          *      ELTMASOP
01956 *        MOVE CMF TEXT TO OUTPUT                           *      ELTMASOP
01957 *                                                          *      ELTMASOP
01958 ************************************************************      ELTMASOP
01959  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTMASOP
01960      PERFORM MOVE-A-LINE.                                         ELTMASOP
01961      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTMASOP
01962          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTMASOP
01963      IF TCAR-FROM-SUB GREATER THAN 20                             ELTMASOP
01964               OR CMF-DESCR-IDX GREATER THAN                       ELTMASOP
01965          CMF-NBR-DESCR-LINES                                      ELTMASOP
01966          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTMASOP
01967      EJECT                                                        ELTMASOP
01968                                                                   ELTMASOP
01969                                                                   ELTMASOP
01970 ************************************************************      ELTMASOP
01971 *                                                          *      ELTMASOP
01972 *        FINISH CODES MANUAL TEXT                          *      ELTMASOP
01973 *                                                          *      ELTMASOP
01974 ************************************************************      ELTMASOP
01975  FINISH-CODES-MANUAL-TEXT.                                        ELTMASOP
01976      SET DONE-PROCESSING TO TRUE.                                 ELTMASOP
01977      IF PERIOD-NEEDED                                             ELTMASOP
01978          PERFORM GET-AND-MOVE-PERIOD.                             ELTMASOP
01979                                                                   ELTMASOP
01980                                                                   ELTMASOP
01981 ************************************************************      ELTMASOP
01982 *                                                          *      ELTMASOP
01983 *        GET AND MOVE PERIOD                               *      ELTMASOP
01984 *                                                          *      ELTMASOP
01985 ************************************************************      ELTMASOP
01986  GET-AND-MOVE-PERIOD.                                             ELTMASOP
01987      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTMASOP
01988          (TCAR-FROM-SUB).                                         ELTMASOP
01989                                                                   ELTMASOP
01990                                                                   ELTMASOP
01991 ************************************************************      ELTMASOP
01992 *                                                          *      ELTMASOP
01993 *        SAVE LAST LINE                                    *      ELTMASOP
01994 *                                                          *      ELTMASOP
01995 ************************************************************      ELTMASOP
01996  SAVE-LAST-LINE.                                                  ELTMASOP
01997      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
01998      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTMASOP
01999         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTMASOP
02000      ADD 1 TO TCAR-FROM-SUB.                                      ELTMASOP
02001      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTMASOP
02002                                                                   ELTMASOP
02003                                                                   ELTMASOP
02004 ************************************************************      ELTMASOP
02005 *                                                          *      ELTMASOP
02006 *        OUTPUT LAST LINE                                  *      ELTMASOP
02007 *                                                          *      ELTMASOP
02008 ************************************************************      ELTMASOP
02009  OUTPUT-LAST-LINE.                                                ELTMASOP
02010      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTMASOP
02011          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTMASOP
02012      IF BLANK-LINE-NEEDED                                         ELTMASOP
02013          PERFORM CREATE-A-BLANK-LINE.                             ELTMASOP
02014                                                                   ELTMASOP
02015                                                                   ELTMASOP
02016 ************************************************************      ELTMASOP
02017 *                                                          *      ELTMASOP
02018 *        CREATE A BLANK LINE                               *      ELTMASOP
02019 *                                                          *      ELTMASOP
02020 ************************************************************      ELTMASOP
02021  CREATE-A-BLANK-LINE.                                             ELTMASOP
02022      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTMASOP
02023      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMASOP
02024      EJECT                                                        ELTMASOP
02025                                                                   ELTMASOP
02026                                                                   ELTMASOP
02027 ************************************************************      ELTMASOP
02028 *                                                          *      ELTMASOP
02029 *        MOVE A LINE                                       *      ELTMASOP
02030 *                                                          *      ELTMASOP
02031 ************************************************************      ELTMASOP
02032  MOVE-A-LINE.                                                     ELTMASOP
02033      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTMASOP
02034          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTMASOP
02035      SET CMF-DESCR-IDX UP BY 1.                                   ELTMASOP
02036      ADD 1 TO TCAR-FROM-SUB.                                      ELTMASOP
02037      EJECT                                                        ELTMASOP
02038                                                                   ELTMASOP
02039                                                                   ELTMASOP
02040 ************************************************************      ELTMASOP
02041 *                                                          *      ELTMASOP
02042 *        REFORMAT AND WRITE TEXT                           *      ELTMASOP
02043 *                                                          *      ELTMASOP
02044 ************************************************************      ELTMASOP
02045  REFORMAT-AND-WRITE-TEXT.                                         ELTMASOP
02046      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTMASOP
02047      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTMASOP
02048      PERFORM UNSTRING-TEXT.                                       ELTMASOP
02049      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMASOP
02050      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMASOP
02051      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTMASOP
02052          UNTIL COF-NBR-DTL-LINES GREATER                          ELTMASOP
02053                                   TCAR-OUTPUT-FIELDS-USED -       ELTMASOP
02054              1.                                                   ELTMASOP
02055      PERFORM DISPOSE-OF-LAST-LINE.                                ELTMASOP
02056      PERFORM LINK-TO-OUTPUT.                                      ELTMASOP
02057                                                                   ELTMASOP
02058                                                                   ELTMASOP
02059 ************************************************************      ELTMASOP
02060 *                                                          *      ELTMASOP
02061 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTMASOP
02062 *                                                          *      ELTMASOP
02063 ************************************************************      ELTMASOP
02064  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTMASOP
02065      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTMASOP
02066           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTMASOP
02067      ADD +1 TO TCAR-FROM-SUB.                                     ELTMASOP
02068      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMASOP
02069      EJECT                                                        ELTMASOP
02070                                                                   ELTMASOP
02071                                                                   ELTMASOP
02072 ************************************************************      ELTMASOP
02073 *                                                          *      ELTMASOP
02074 *        UNSTRING TEXT                                     *      ELTMASOP
02075 *                                                          *      ELTMASOP
02076 ************************************************************      ELTMASOP
02077  UNSTRING-TEXT.                                                   ELTMASOP
02078      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTMASOP
02079      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTMASOP
02080      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTMASOP
02081      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTMASOP
02082      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTMASOP
02083      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTMASOP
02084      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTMASOP
02085      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTMASOP
02086      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTMASOP
02087      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTMASOP
02088      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTMASOP
02089      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTMASOP
02090      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTMASOP
02091      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTMASOP
02092      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTMASOP
02093      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTMASOP
02094      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTMASOP
02095      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTMASOP
02096      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTMASOP
02097      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTMASOP
02098      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTMASOP
02099      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTMASOP
02100                                                                   ELTMASOP
02101                                                                   ELTMASOP
02102 ************************************************************      ELTMASOP
02103 *                                                          *      ELTMASOP
02104 *        LINK TO OUTPUT                                    *      ELTMASOP
02105 *                                                          *      ELTMASOP
02106 ************************************************************      ELTMASOP
02107  LINK-TO-OUTPUT.                                                  ELTMASOP
02108      EXEC CICS LINK                                               ELTMASOP
02109          PROGRAM ('ELUOUTPT')                                     ELTMASOP
02110          COMMAREA (DFHCOMMAREA)                                   ELTMASOP
02111          END-EXEC.                                                ELTMASOP
02112      EJECT                                                        ELTMASOP
02113                                                                   ELTMASOP
02114                                                                   ELTMASOP
02115 ************************************************************      ELTMASOP
02116 *                                                          *      ELTMASOP
02117 *        DISPOSE OF LAST LINE                              *      ELTMASOP
02118 *                                                          *      ELTMASOP
02119 ************************************************************      ELTMASOP
02120  DISPOSE-OF-LAST-LINE.                                            ELTMASOP
02121      IF NOT ADDITIONAL-TEXT                                       ELTMASOP
02122          PERFORM INITIALIZE-CONTINUED-SW.                         ELTMASOP
02123      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTMASOP
02124          PERFORM SAVE-LAST-LINE                                   ELTMASOP
02125      ELSE                                                         ELTMASOP
02126          PERFORM OUTPUT-LAST-LINE.                                ELTMASOP
02127                                                                   ELTMASOP
02128                                                                   ELTMASOP
02129 ************************************************************      ELTMASOP
02130 *                                                          *      ELTMASOP
02131 *        INITIALIZE CONTINUED SW                           *      ELTMASOP
02132 *                                                          *      ELTMASOP
02133 ************************************************************      ELTMASOP
02134  INITIALIZE-CONTINUED-SW.                                         ELTMASOP
02135      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTMASOP
