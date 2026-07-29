00001 *      LAST MAINTENANCE TIME: 11.35.07  DATE: 06/13/91            06/29/02
00002  IDENTIFICATION DIVISION.                                         ELTPAR  
00003                                                                      LV001
00004  PROGRAM-ID.         ELTPAR.                                      ELTPAR  
00005                                                                   ELTPAR  
00006  AUTHOR.             ANNE KEFFER KING.                            ELTPAR  
00007                                                                   ELTPAR  
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTPAR  
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELTPAR  
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTPAR  
00011                      233 N. MICHIGAN AVE                          ELTPAR  
00012                      CHICAGO, ILLINOIS 60601                      ELTPAR  
00013                                                                   ELTPAR  
00014  DATE-WRITTEN.       19-JUN-1987.                                 ELTPAR  
00015                                                                   ELTPAR  
00016  DATE-COMPILED.                                                   ELTPAR  
00017                                                                   ELTPAR  
00018  SECURITY.           COPYRIGHT 1986,                              ELTPAR  
00019                      HEALTH CARE SERVICE CORPORATION              ELTPAR  
00020 ******************************************************************ELTPAR  
00021 *  RECORDS                                                       *ELTPAR  
00022 *  ACCESSED: ELCDCIA RECORD                                      *ELTPAR  
00023 *             GROUP SPECIFIC RECORD                              *ELTPAR  
00024 *             #GCCP TABULAR RECORD                               *ELTPAR  
00025 *                                                                *ELTPAR  
00026 *  PROCESSING                                                    *ELTPAR  
00027 *  FUNCTIONS: THIS MODULE PERFORMS THE FOLLOWING FUNCTIONS:      *ELTPAR  
00028 *                                                                *ELTPAR  
00029 *              1. INITIALIZES WORK DATA ELEMENTS.                *ELTPAR  
00030 *                                                                *ELTPAR  
00031 *              2. ACQUIRE NECESSARY RECORDS FOR PROCESSING.      *ELTPAR  
00032 *                                                                *ELTPAR  
00033 *              3. PROCESS PRE-ADMISSION REVIEW PROGRAM TABULAR   *ELTPAR  
00034 *                 RECORD FOR BLUE CROSS PRODUCING OUTPUT         *ELTPAR  
00035 *                 TEXT AS REQUIRED.                              *ELTPAR  
00036 *                                                                *ELTPAR  
00037 *              4. PROCESS PRE-ADMISSION REVIEW PROGRAM TABULAR   *ELTPAR  
00038 *                 RECORD FOR BLUE SHIELD PRODUCING OUTPUT        *ELTPAR  
00039 *                 TEXT AS REQUIRED.                              *ELTPAR  
00040 *                                                                *ELTPAR  
00041 *              5. PROCESS PRE-ADMISSION REVIEW PROGRAM TABULAR   *ELTPAR  
00042 *                 RECORD FOR MAJOR MEDICAL PRODUCING OUTPUT      *ELTPAR  
00043 *                 TEXT AS REQUIRED.                              *ELTPAR  
00044 ******************************************************************ELTPAR  
00045 *                     MAINTENANCE HISTORY                        *ELTPAR  
00046 *                                                                *ELTPAR  
00047 *  MOD     DATE     BY  DPRT           ACTION                    *ELTPAR  
00048 * ----- ----------- --- ----- ---------------------------------- *ELTPAR  
00049 * 01.01 06-NOV-1990 JPB       CHANGED STORAGE MANAGEMENT         *ELTPAR  
00050 *                                                                *ELTPAR  
00051 * 01.02 12-NOV-1990 JPB       CHANGED REFERENCES TO GCG-PRE-ADM- *ELTPAR  
00052 *                             REVIEW-IND TO REFLECT NEW FIELD    *ELTPAR  
00053 *                             SIZE.                              *ELTPAR  
00054 *                                                                *ELTPAR  
00055 * 01.03 12-JUN-1991 JPB       ADDED TRANSLATION AND DISPLAY OF   *ELTPAR  
00056 *                             PARTICIPATION INDICATOR            *ELTPAR  
00057 *                             (GCG-PRE-ADM-REVIEW-IND).          *ELTPAR  
00058 *                                                                *ELTPAR  
00059 ******************************************************************ELTPAR  
00060      SKIP3                                                        ELTPAR  
00061  ENVIRONMENT DIVISION.                                            ELTPAR  
00062                                                                   ELTPAR  
00063  CONFIGURATION SECTION.                                           ELTPAR  
00064  SOURCE-COMPUTER.    IBM-3090.                                    ELTPAR  
00065  OBJECT-COMPUTER.    IBM-3090.                                    ELTPAR  
00066      EJECT                                                        ELTPAR  
00067                                                                   ELTPAR  
00068  DATA DIVISION.                                                   ELTPAR  
00069 /                                                                 ELTPAR  
00070 *                                                                 ELTPAR  
00071  WORKING-STORAGE SECTION.                                         ELTPAR  
00072  01  WS-BEGIN                            PIC X(24) VALUE          ELTPAR  
00073                                 '** ELTPAR WS BEGINS **'.         ELTPAR  
00074  01  WS-MISC-FIELDS.                                              ELTPAR  
00075      05  WS-GPAR-PROC-ID         PIC X(06)    VALUE SPACES.       ELTPAR  
00076      05  WS-GPAR-PROC-SLOT-NO    PIC S9(04) COMP-3                ELTPAR  
00077                                               VALUE ZEROES.       ELTPAR  
00078 *                                                                 ELTPAR  
00079      05  WS-GPAB-SRVS-ID         PIC X(06)    VALUE SPACES.       ELTPAR  
00080      05  WS-GPAB-SRVS-SLOT-NO    PIC S9(04) COMP-3                ELTPAR  
00081                                               VALUE ZEROES.       ELTPAR  
00082 *                                                                 ELTPAR  
00083      05  WS-GPAC-PROV-ID         PIC X(06)    VALUE SPACES.       ELTPAR  
00084      05  WS-GPAC-PROV-SLOT-NO    PIC S9(04) COMP-3                ELTPAR  
00085                                               VALUE ZEROES.       ELTPAR  
00086 *                                                                 ELTPAR  
00087      05  WS-GPAD-DIAG-ID         PIC X(06)    VALUE SPACES.       ELTPAR  
00088      05  WS-GPAD-DIAG-SLOT-NO    PIC S9(04) COMP-3                ELTPAR  
00089                                               VALUE ZEROES.       ELTPAR  
00090 *                                                                 ELTPAR  
00091                                                                   ELTPAR  
00092      05  SCREEN-TYPE                   PIC X  VALUE SPACES.       ELTPAR  
00093          88  INSTITUTIONAL-SCREEN             VALUE 'I'.          ELTPAR  
00094          88  PROFESSIONAL-SCREEN              VALUE 'P'.          ELTPAR  
00095          88  SUPPLEMENTAL-SCREEN              VALUE 'S'.          ELTPAR  
00096 *                                                                 ELTPAR  
00097  01  PROGRAM-CONSTANTS.                                           ELTPAR  
00098      05  PC-GCCP                 PIC X(06)  VALUE '#GCCP '.       ELTPAR  
00099      05  PC-GPAR                 PIC X(06)  VALUE '#GPAR '.       ELTPAR  
00100      05  PC-GPAB                 PIC X(06)  VALUE '#GPAB '.       ELTPAR  
00101      05  PC-GPAC                 PIC X(06)  VALUE '#GPAC '.       ELTPAR  
00102      05  PC-GPAD                 PIC X(06)  VALUE '#GPAD '.       ELTPAR  
00103      05  PC-GROUP                PIC X(06)  VALUE 'GROUP '.       ELTPAR  
00104 *                                                                 ELTPAR  
00105  01  WS-SWITCHES.                                                 ELTPAR  
00106      05  UNDEFINED-TABULAR-SW     PIC X      VALUE 'N'.           ELTPAR  
00107          88 TABULAR-IS-UNDEFINED             VALUE 'Y'.           ELTPAR  
00108      05  DEFINED-TABULAR-SW       PIC X      VALUE 'N'.           ELTPAR  
00109          88 TABULAR-IS-DEFINED               VALUE 'Y'.           ELTPAR  
00110 *                                                                 ELTPAR  
00111      05  ADDITIONAL-TEXT-SW       PIC X      VALUE SPACE.         ELTPAR  
00112          88 BLANK-LINE-NEEDED                VALUE 'B'.           ELTPAR  
00113          88 ADDITIONAL-TEXT                  VALUE 'Y'.           ELTPAR  
00114 *                                                                 ELTPAR  
00115      05  CONTINUED-PROCESSING-SW  PIC X      VALUE SPACE.         ELTPAR  
00116          88 PROCESSING-CMF-TEXT              VALUE 'P'.           ELTPAR  
00117          88 DONE-PROCESSING                  VALUE 'D'.           ELTPAR  
00118 *                                                                 ELTPAR  
00119      05  WS-PERIOD-SW             PIC X      VALUE 'N'.           ELTPAR  
00120          88 PERIOD-NEEDED                    VALUE 'Y'.           ELTPAR  
00121 *                                                                 ELTPAR  
00122 ******************************************************************ELTPAR  
00123 *SCREEN BODY LINES                                                ELTPAR  
00124 ******************************************************************ELTPAR  
00125  01  WS-HDR-LN2.                                                  ELTPAR  
00126      05  FILLER            PIC X(18)         VALUE SPACES.        ELTPAR  
00127      05  FILLER            PIC X(29)         VALUE                ELTPAR  
00128          'PRE-ADMISSION REVIEW PROGRAM '.                         ELTPAR  
00129      05  HDR-TITLE         PIC X(13)         VALUE SPACES.        ELTPAR  
00130      05  FILLER            PIC X(18)         VALUE SPACES.        ELTPAR  
00131                                                                   ELTPAR  
00132  01  WS-PARTICIPATION-LINE.                                       ELTPAR  
00133      05  FILLER             PIC X(79)        VALUE                ELTPAR  
00134          'THE PRE-ADMISSION REVIEW PROGRAM APPLIES TO '.          ELTPAR  
00135 *                                                                 ELTPAR  
00136  01  WS-PROGRAM-SOURCE.                                           ELTPAR  
00137      05  FILLER             PIC X(79)        VALUE                ELTPAR  
00138          'THE PRE-ADMISSION REVIEW PROGRAM REQUIRES THE APPROVAL OELTPAR  
00139 -        'F '.                                                    ELTPAR  
00140 *                                                                 ELTPAR  
00141  01  WS-INDICATOR.                                                ELTPAR  
00142      05  FILLER             PIC X(79)        VALUE                ELTPAR  
00143          'THE PRE-ADMISSION REVIEW PROGRAM '.                     ELTPAR  
00144 *                                                                 ELTPAR  
00145  01  WS-PAYMENT-LEVEL.                                            ELTPAR  
00146      05  FILLER                  PIC X(79)   VALUE                ELTPAR  
00147      'IF PRE-ADMISSION REVIEW IS NOT PERFORMED OR ADMISSION IS NOTELTPAR  
00148 -       ' APPROVED, '.                                            ELTPAR  
00149 *                                                                 ELTPAR  
00150  01  WS-BENEFITS-REDUCTION.                                       ELTPAR  
00151      05  FILLER                  PIC X(79)   VALUE                ELTPAR  
00152          'DENIED OR REDUCED BENEFITS DUE TO COST CONTAINMENT:'.   ELTPAR  
00153 *                                                                 ELTPAR  
00154  01  WS-SPILLOVER-SENTENCE.                                       ELTPAR  
00155      05  FILLER                 PIC X(79)    VALUE                ELTPAR  
00156          'UNPAID SERVICES AFTER BASIC BENEFIT REDUCTIONS ARE '.   ELTPAR  
00157 *                                                                 ELTPAR  
00158  01  RELATED-DIAGNOSES-MSG.                                       ELTPAR  
00159      05  FILLER                   PIC X(79)    VALUE              ELTPAR  
00160          'THERE ARE SPECIAL RELATED DIAGNOSES INCLUDED IN THIS COSELTPAR  
00161 -        'T CONTAINMENT PROGRAM.'.                                ELTPAR  
00162 *                                                                 ELTPAR  
00163  01  RELATED-SERVICES-MSG.                                        ELTPAR  
00164      05  FILLER                   PIC X(79)     VALUE             ELTPAR  
00165          'THERE ARE SPECIAL RELATED SERVICES INCLUDED IN THIS COSTELTPAR  
00166 -        ' CONTAINMENT PROGRAM.'.                                 ELTPAR  
00167 *                                                                 ELTPAR  
00168 *                                                                 ELTPAR  
00169  01  RELATED-PROCEDURES-MSG.                                      ELTPAR  
00170      05  FILLER                   PIC X(79)    VALUE              ELTPAR  
00171      'THERE ARE SPECIAL RELATED PROCEDURES INCLUDED IN THIS COST CELTPAR  
00172 -    'ONTAINMENT '.                                               ELTPAR  
00173 *                                                                 ELTPAR  
00174  01  RELATED-PROVIDERS-MSG.                                       ELTPAR  
00175      05  FILLER                   PIC X(79)    VALUE              ELTPAR  
00176      'THERE ARE SPECIAL RELATED PROCEDURES INCLUDED IN THIS COST CELTPAR  
00177 -    'ONTAINMENT '.                                               ELTPAR  
00178 *                                                                 ELTPAR  
00179  01  WS-NOT-APPLICABLE-LOB-BC.                                    ELTPAR  
00180      05  FILLER                    PIC X(79)   VALUE              ELTPAR  
00181          'THE PRE-ADMISSION PROGRAM DOES NOT APPLY TO INSTITUTIONAELTPAR  
00182 -        'L BENEFITS.'.                                           ELTPAR  
00183 *                                                                 ELTPAR  
00184  01  WS-NOT-APPLICABLE-LOB-BS.                                    ELTPAR  
00185      05  FILLER                    PIC X(79)   VALUE              ELTPAR  
00186          'THE PRE-ADMISSION PROGRAM DOES NOT APPLY TO PROFESSIONALELTPAR  
00187 -        ' BENEFITS.'.                                            ELTPAR  
00188 *                                                                 ELTPAR  
00189  01  WS-NOT-APPLICABLE-LOB-MM.                                    ELTPAR  
00190      05  FILLER                    PIC X(79)   VALUE              ELTPAR  
00191          'THE PRE-ADMISSION PROGRAM DOES NOT APPLY TO SUPPLEMENTALELTPAR  
00192 -        ' BENEFITS.'.                                            ELTPAR  
00193 *                                                                 ELTPAR  
00194  01  WS-VOLUNTARY-MSG.                                            ELTPAR  
00195      05  FILLER                    PIC  X(79) VALUE               ELTPAR  
00196          'THE PRE-ADMISSION PROGRAM IS VOLUNTARY.'.               ELTPAR  
00197 *                                                                 ELTPAR  
00198  01  WS-NOT-APPLICABLE-MSG.                                       ELTPAR  
00199      05  FILLER                    PIC X(79)  VALUE               ELTPAR  
00200          'THE PRE-ADMISSION PROGRAM IS NOT APPLICABLE.'.          ELTPAR  
00201 *                                                                 ELTPAR  
00202  01  WS-DISCLAIMER.                                               ELTPAR  
00203      05  FILLER                    PIC X(79)  VALUE               ELTPAR  
00204         '*** SUBJECT TO CONTRACT LIMITATIONS ***'.                ELTPAR  
00205  01  WS-END                              PIC X(18) VALUE          ELTPAR  
00206                                          '*** END OF W/S ***'.    ELTPAR  
00207  LINKAGE SECTION.                                                 ELTPAR  
00208  01  DFHCOMMAREA.                                                 ELTPAR  
00209      COPY ELSCOMMC.                                               ELTPAR  
00210 /                                                                 ELTPAR  
00211      COPY ELSCIA2C.                                               ELTPAR  
00212 /                                                                 ELTPAR  
00213      COPY ELSCMDSC.                                               ELTPAR  
00214 /                                                                 ELTPAR  
00215      COPY ELSCMIFC.                                               ELTPAR  
00216 /                                                                 ELTPAR  
00217      COPY ELSIOPMC.                                               ELTPAR  
00218 /                                                                 ELTPAR  
00219      COPY ELSKEYSC.                                               ELTPAR  
00220 /                                                                 ELTPAR  
00221      COPY ELSOUTPC.                                               ELTPAR  
00222 /                                                                 ELTPAR  
00223      COPY ELSSRTPC.                                               ELTPAR  
00224 /                                                                 ELTPAR  
00225      COPY ELSTCWAC.                                               ELTPAR  
00226 /                                                                 ELTPAR  
00227      COPY ELSSSCBC.                                               ELTPAR  
00228 /                                                                 ELTPAR  
00229  01  GROUP-SPECIFIC-RECORD.                                       ELTPAR  
00230      COPY GCGROUPC.                                               ELTPAR  
00231 /                                                                 ELTPAR  
00232  01  GCCP-TABULAR-REC.                                            ELTPAR  
00233      COPY GCTGCCPC.                                               ELTPAR  
00234 /                                                                 ELTPAR  
00235      EJECT                                                        ELTPAR  
00236  PROCEDURE DIVISION.                                              ELTPAR  
00237 ************************************************************      ELTPAR  
00238 *                                                          *      ELTPAR  
00239 *                    PROCEDURE DIVISION                    *      ELTPAR  
00240 *                                                          *      ELTPAR  
00241 ************************************************************      ELTPAR  
00242                                                                   ELTPAR  
00243                                                                   ELTPAR  
00244 ************************************************************      ELTPAR  
00245 *                                                          *      ELTPAR  
00246 *        PRE-ADMISSION REVIEW                              *      ELTPAR  
00247 *                                                          *      ELTPAR  
00248 ************************************************************      ELTPAR  
00249  PRE-ADMISSION-REVIEW.                                            ELTPAR  
00250      PERFORM INITIALIZATION.                                      ELTPAR  
00251      PERFORM PROCESS-PAR.                                         ELTPAR  
00252      GOBACK.                                                      ELTPAR  
00253                                                                   ELTPAR  
00254                                                                   ELTPAR  
00255 ************************************************************      ELTPAR  
00256 *                                                          *      ELTPAR  
00257 *        INITIALIZATION                                    *      ELTPAR  
00258 *                                                          *      ELTPAR  
00259 ************************************************************      ELTPAR  
00260  INITIALIZATION.                                                  ELTPAR  
00261      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTPAR  
00262      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTPAR  
00263                                                                   ELTPAR  
00264                                                                   ELTPAR  
00265 ************************************************************      ELTPAR  
00266 *                                                          *      ELTPAR  
00267 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTPAR  
00268 *                                                          *      ELTPAR  
00269 ************************************************************      ELTPAR  
00270  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTPAR  
00271      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTPAR  
00272      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTPAR  
00273      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTPAR  
00274                                                                   ELTPAR  
00275                                                                   ELTPAR  
00276 ************************************************************      ELTPAR  
00277 *                                                          *      ELTPAR  
00278 *        CHECK FOR VALID COMMAREA                          *      ELTPAR  
00279 *                                                          *      ELTPAR  
00280 ************************************************************      ELTPAR  
00281  CHECK-FOR-VALID-COMMAREA.                                        ELTPAR  
00282      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTPAR  
00283          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTPAR  
00284                                                                   ELTPAR  
00285                                                                   ELTPAR  
00286 ************************************************************      ELTPAR  
00287 *                                                          *      ELTPAR  
00288 *        SIGNAL INVALID COMMAREA                           *      ELTPAR  
00289 *                                                          *      ELTPAR  
00290 ************************************************************      ELTPAR  
00291  SIGNAL-INVALID-COMMAREA.                                         ELTPAR  
00292      EXEC CICS ABEND                                              ELTPAR  
00293                ABCODE('EL01')                                     ELTPAR  
00294         END-EXEC.                                                 ELTPAR  
00295      EJECT                                                        ELTPAR  
00296                                                                   ELTPAR  
00297                                                                   ELTPAR  
00298 ************************************************************      ELTPAR  
00299 *                                                          *      ELTPAR  
00300 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTPAR  
00301 *                                                          *      ELTPAR  
00302 ************************************************************      ELTPAR  
00303  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTPAR  
00304      IF ECA-CIA-PTR = NULL                                        ELTPAR  
00305          PERFORM SIGNAL-INVALID-CIA                               ELTPAR  
00306      ELSE                                                         ELTPAR  
00307          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTPAR  
00308                                                                   ELTPAR  
00309                                                                   ELTPAR  
00310 ************************************************************      ELTPAR  
00311 *                                                          *      ELTPAR  
00312 *        ESTABLISH ADDRESS OF CIA                          *      ELTPAR  
00313 *                                                          *      ELTPAR  
00314 ************************************************************      ELTPAR  
00315  ESTABLISH-ADDRESS-OF-CIA.                                        ELTPAR  
00316      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTPAR  
00317                            ADDRESS OF                             ELTPAR  
00318          CIA-ELS-COMMON-INTERFACE-AREA.                           ELTPAR  
00319                                                                   ELTPAR  
00320                                                                   ELTPAR  
00321 ************************************************************      ELTPAR  
00322 *                                                          *      ELTPAR  
00323 *        SIGNAL INVALID CIA                                *      ELTPAR  
00324 *                                                          *      ELTPAR  
00325 ************************************************************      ELTPAR  
00326  SIGNAL-INVALID-CIA.                                              ELTPAR  
00327      EXEC CICS ABEND                                              ELTPAR  
00328                ABCODE('EL02')                                     ELTPAR  
00329         END-EXEC.                                                 ELTPAR  
00330      EJECT                                                        ELTPAR  
00331                                                                   ELTPAR  
00332                                                                   ELTPAR  
00333 ************************************************************      ELTPAR  
00334 *                                                          *      ELTPAR  
00335 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTPAR  
00336 *                                                          *      ELTPAR  
00337 ************************************************************      ELTPAR  
00338  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTPAR  
00339      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTPAR  
00340      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAR  
00341                            ADDRESS OF                             ELTPAR  
00342          SSB-SELECTOR-STATUS-CTL-BLK.                             ELTPAR  
00343      IF CIA-RC-PTR-NULL                                           ELTPAR  
00344          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAR  
00345                                                                   ELTPAR  
00346                                                                   ELTPAR  
00347 ************************************************************      ELTPAR  
00348 *                                                          *      ELTPAR  
00349 *        SIGNAL UNALLOC AREA ERROR                         *      ELTPAR  
00350 *                                                          *      ELTPAR  
00351 ************************************************************      ELTPAR  
00352  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTPAR  
00353      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTPAR  
00354      PERFORM SIGNAL-ABEND.                                        ELTPAR  
00355                                                                   ELTPAR  
00356                                                                   ELTPAR  
00357 ************************************************************      ELTPAR  
00358 *                                                          *      ELTPAR  
00359 *        SIGNAL ABEND                                      *      ELTPAR  
00360 *                                                          *      ELTPAR  
00361 ************************************************************      ELTPAR  
00362  SIGNAL-ABEND.                                                    ELTPAR  
00363      EXEC CICS ABEND                                              ELTPAR  
00364                ABCODE(CIA-ABCODE)                                 ELTPAR  
00365         END-EXEC.                                                 ELTPAR  
00366      EJECT                                                        ELTPAR  
00367                                                                   ELTPAR  
00368                                                                   ELTPAR  
00369 ************************************************************      ELTPAR  
00370 *                                                          *      ELTPAR  
00371 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTPAR  
00372 *                                                          *      ELTPAR  
00373 ************************************************************      ELTPAR  
00374  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTPAR  
00375      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTPAR  
00376      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTPAR  
00377      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTPAR  
00378      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTPAR  
00379      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTPAR  
00380      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTPAR  
00381      PERFORM ESTABLISH-ADDRESSABILITY-OF-CS.                      ELTPAR  
00382                                                                   ELTPAR  
00383                                                                   ELTPAR  
00384 ************************************************************      ELTPAR  
00385 *                                                          *      ELTPAR  
00386 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTPAR  
00387 *                                                          *      ELTPAR  
00388 ************************************************************      ELTPAR  
00389  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTPAR  
00390      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTPAR  
00391      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAR  
00392                            ADDRESS OF                             ELTPAR  
00393          CMF-CODES-MANUAL-INTERFACE.                              ELTPAR  
00394      IF CIA-RC-PTR-NULL                                           ELTPAR  
00395          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAR  
00396      EJECT                                                        ELTPAR  
00397                                                                   ELTPAR  
00398                                                                   ELTPAR  
00399 ************************************************************      ELTPAR  
00400 *                                                          *      ELTPAR  
00401 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTPAR  
00402 *                                                          *      ELTPAR  
00403 ************************************************************      ELTPAR  
00404  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTPAR  
00405      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTPAR  
00406      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAR  
00407                            ADDRESS OF                             ELTPAR  
00408          COF-OUTPUT-INTERFACE.                                    ELTPAR  
00409      IF CIA-RC-PTR-NULL                                           ELTPAR  
00410          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAR  
00411      EJECT                                                        ELTPAR  
00412                                                                   ELTPAR  
00413                                                                   ELTPAR  
00414 ************************************************************      ELTPAR  
00415 *                                                          *      ELTPAR  
00416 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTPAR  
00417 *                                                          *      ELTPAR  
00418 ************************************************************      ELTPAR  
00419  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTPAR  
00420      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTPAR  
00421      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAR  
00422                            ADDRESS OF                             ELTPAR  
00423          SRP-SUBROUTINE-PARAMETERS.                               ELTPAR  
00424      IF CIA-RC-PTR-NULL                                           ELTPAR  
00425          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAR  
00426      EJECT                                                        ELTPAR  
00427                                                                   ELTPAR  
00428                                                                   ELTPAR  
00429 ************************************************************      ELTPAR  
00430 *                                                          *      ELTPAR  
00431 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTPAR  
00432 *                                                          *      ELTPAR  
00433 ************************************************************      ELTPAR  
00434  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTPAR  
00435      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTPAR  
00436      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAR  
00437                            ADDRESS OF                             ELTPAR  
00438          TCAR-COMPRESSION-WORK-AREA.                              ELTPAR  
00439      IF CIA-RC-PTR-NULL                                           ELTPAR  
00440          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAR  
00441      EJECT                                                        ELTPAR  
00442                                                                   ELTPAR  
00443                                                                   ELTPAR  
00444 ************************************************************      ELTPAR  
00445 *                                                          *      ELTPAR  
00446 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTPAR  
00447 *                                                          *      ELTPAR  
00448 ************************************************************      ELTPAR  
00449  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTPAR  
00450      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTPAR  
00451      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAR  
00452                            ADDRESS OF                             ELTPAR  
00453          KWA-FILE-KEY-WORK-AREA.                                  ELTPAR  
00454      IF CIA-RC-PTR-NULL                                           ELTPAR  
00455          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAR  
00456      EJECT                                                        ELTPAR  
00457                                                                   ELTPAR  
00458                                                                   ELTPAR  
00459 ************************************************************      ELTPAR  
00460 *                                                          *      ELTPAR  
00461 *        ESTABLISH ADDRESSABILITY OF GRP SPECIFIC          *      ELTPAR  
00462 *                                                          *      ELTPAR  
00463 ************************************************************      ELTPAR  
00464  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTPAR  
00465      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTPAR  
00466      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAR  
00467                            ADDRESS OF                             ELTPAR  
00468          GROUP-SPECIFIC-RECORD.                                   ELTPAR  
00469      IF CIA-RC-PTR-NULL                                           ELTPAR  
00470          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAR  
00471      EJECT                                                        ELTPAR  
00472                                                                   ELTPAR  
00473                                                                   ELTPAR  
00474 ************************************************************      ELTPAR  
00475 *                                                          *      ELTPAR  
00476 *        ESTABLISH ADDRESSABILITY OF CST CONTAINMENT       *      ELTPAR  
00477 *                                                          *      ELTPAR  
00478 ************************************************************      ELTPAR  
00479  ESTABLISH-ADDRESSABILITY-OF-CS.                                  ELTPAR  
00480      SET CIA-GCTABULR-DDN TO TRUE.                                ELTPAR  
00481      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAR  
00482                            ADDRESS OF GCCP-TABULAR-REC.           ELTPAR  
00483      IF CIA-RC-PTR-NULL                                           ELTPAR  
00484          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAR  
00485      EJECT                                                        ELTPAR  
00486                                                                   ELTPAR  
00487                                                                   ELTPAR  
00488 ************************************************************      ELTPAR  
00489 *                                                          *      ELTPAR  
00490 *        PROCESS PAR                                       *      ELTPAR  
00491 *                                                          *      ELTPAR  
00492 ************************************************************      ELTPAR  
00493  PROCESS-PAR.                                                     ELTPAR  
00494      IF GCG-PRE-ADM-REVIEW-IND EQUAL ZERO                         ELTPAR  
00495                  OR '08'                                          ELTPAR  
00496          PERFORM TEST-APPLICABILITY                               ELTPAR  
00497      ELSE                                                         ELTPAR  
00498          PERFORM GENERATE-PRE-ADMISSION-REVIEWX.                  ELTPAR  
00499      MOVE 'E' TO  COF-FUNCTION.                                   ELTPAR  
00500      MOVE ZEROS TO COF-NBR-DTL-LINES                              ELTPAR  
00501                COF-NBR-HDR-LINES.                                 ELTPAR  
00502      PERFORM LINK-TO-OUTPUT.                                      ELTPAR  
00503                                                                   ELTPAR  
00504                                                                   ELTPAR  
00505 ************************************************************      ELTPAR  
00506 *                                                          *      ELTPAR  
00507 *        GENERATE PRE ADMISSION REVIEW TEXT                *      ELTPAR  
00508 *                                                          *      ELTPAR  
00509 ************************************************************      ELTPAR  
00510  GENERATE-PRE-ADMISSION-REVIEWX.                                  ELTPAR  
00511      PERFORM VERIFY-PRE-ADMISSION-REVIEW-IN.                      ELTPAR  
00512      PERFORM BUILD-PRE-ADMISSION-REVIEW-TEX.                      ELTPAR  
00513      EJECT                                                        ELTPAR  
00514                                                                   ELTPAR  
00515                                                                   ELTPAR  
00516 ************************************************************      ELTPAR  
00517 *                                                          *      ELTPAR  
00518 *        TEST APPLICABILITY                                *      ELTPAR  
00519 *                                                          *      ELTPAR  
00520 ************************************************************      ELTPAR  
00521  TEST-APPLICABILITY.                                              ELTPAR  
00522      PERFORM GENERATE-HEADINGS.                                   ELTPAR  
00523      IF GCG-PRE-ADM-REVIEW-IND  EQUAL ZERO                        ELTPAR  
00524          PERFORM SIGNAL-NOT-APPLICABLE-MSG                        ELTPAR  
00525      ELSE IF GCG-PRE-ADM-REVIEW-IND  EQUAL '08'                   ELTPAR  
00526          PERFORM SIGNAL-VOLUNTARY-MSG.                            ELTPAR  
00527      PERFORM LINK-TO-OUTPUT.                                      ELTPAR  
00528                                                                   ELTPAR  
00529                                                                   ELTPAR  
00530 ************************************************************      ELTPAR  
00531 *                                                          *      ELTPAR  
00532 *        VERIFY PRE ADMISSION REVIEW IN GCCP RECORD        *      ELTPAR  
00533 *                                                          *      ELTPAR  
00534 ************************************************************      ELTPAR  
00535  VERIFY-PRE-ADMISSION-REVIEW-IN.                                  ELTPAR  
00536      PERFORM ACQUIRE-GCCP-RECORD.                                 ELTPAR  
00537      PERFORM OBTAIN-PRE-ADMISSION-REVIEW-WI.                      ELTPAR  
00538      EJECT                                                        ELTPAR  
00539                                                                   ELTPAR  
00540                                                                   ELTPAR  
00541 ************************************************************      ELTPAR  
00542 *                                                          *      ELTPAR  
00543 *        ACQUIRE GCCP RECORD                               *      ELTPAR  
00544 *                                                          *      ELTPAR  
00545 ************************************************************      ELTPAR  
00546  ACQUIRE-GCCP-RECORD.                                             ELTPAR  
00547      MOVE SPACES TO KWA-PROVISION-ID.                             ELTPAR  
00548      SET GCG-INDEX TO 1.                                          ELTPAR  
00549      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTPAR  
00550          AT END                                                   ELTPAR  
00551             MOVE ZEROES TO KWA-PROVISION-SLOT-NO                  ELTPAR  
00552             WHEN GCG-TAB-ID (GCG-INDEX) = PC-GCCP                 ELTPAR  
00553                 MOVE GCG-TAB-ID (GCG-INDEX) TO                    ELTPAR  
00554          KWA-PROVISION-ID                                         ELTPAR  
00555                 MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                  ELTPAR  
00556                     TO KWA-PROVISION-SLOT-NO                      ELTPAR  
00557          END-SEARCH.                                              ELTPAR  
00558      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELTPAR  
00559          PERFORM SIGNAL-UNDEFINED-TABULAR                         ELTPAR  
00560      ELSE                                                         ELTPAR  
00561          PERFORM READ-GCCP-RECORD.                                ELTPAR  
00562                                                                   ELTPAR  
00563                                                                   ELTPAR  
00564 ************************************************************      ELTPAR  
00565 *                                                          *      ELTPAR  
00566 *        SIGNAL UNDEFINED TABULAR                          *      ELTPAR  
00567 *                                                          *      ELTPAR  
00568 ************************************************************      ELTPAR  
00569  SIGNAL-UNDEFINED-TABULAR.                                        ELTPAR  
00570      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTPAR  
00571      PERFORM SIGNAL-ABEND.                                        ELTPAR  
00572      EJECT                                                        ELTPAR  
00573                                                                   ELTPAR  
00574                                                                   ELTPAR  
00575 ************************************************************      ELTPAR  
00576 *                                                          *      ELTPAR  
00577 *        OBTAIN PRE ADMISSION REVIEW WITHIN GCCP RECORD    *      ELTPAR  
00578 *                                                          *      ELTPAR  
00579 ************************************************************      ELTPAR  
00580  OBTAIN-PRE-ADMISSION-REVIEW-WI.                                  ELTPAR  
00581      SET GSS-INDEX TO 1.                                          ELTPAR  
00582      SEARCH GSS-ENTRY                                             ELTPAR  
00583         AT END                                                    ELTPAR  
00584            SET TABULAR-IS-UNDEFINED TO TRUE                       ELTPAR  
00585            WHEN GSS-PR-PROG-CODE-CHR (GSS-INDEX)                  ELTPAR  
00586                 CONTINUE                                          ELTPAR  
00587          END-SEARCH.                                              ELTPAR  
00588      IF TABULAR-IS-UNDEFINED                                      ELTPAR  
00589          PERFORM SIGNAL-UNDEFINED-TABULAR.                        ELTPAR  
00590      EJECT                                                        ELTPAR  
00591                                                                   ELTPAR  
00592                                                                   ELTPAR  
00593 ************************************************************      ELTPAR  
00594 *                                                          *      ELTPAR  
00595 *        BUILD PRE ADMISSION REVIEW TEXT                   *      ELTPAR  
00596 *                                                          *      ELTPAR  
00597 ************************************************************      ELTPAR  
00598  BUILD-PRE-ADMISSION-REVIEW-TEX.                                  ELTPAR  
00599      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTPAR  
00600          PERFORM GENERATE-INSTITUTIONAL.                          ELTPAR  
00601      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELTPAR  
00602          PERFORM GENERATE-PROFESSIONAL.                           ELTPAR  
00603      IF GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                        ELTPAR  
00604                 '03' OR '04' OR '06' OR '08'                      ELTPAR  
00605          PERFORM PROCESS-SUPPLEMENTAL.                            ELTPAR  
00606                                                                   ELTPAR  
00607                                                                   ELTPAR  
00608 ************************************************************      ELTPAR  
00609 *                                                          *      ELTPAR  
00610 *        GENERATE INSTITUTIONAL                            *      ELTPAR  
00611 *                                                          *      ELTPAR  
00612 ************************************************************      ELTPAR  
00613  GENERATE-INSTITUTIONAL.                                          ELTPAR  
00614      SET INSTITUTIONAL-SCREEN TO TRUE.                            ELTPAR  
00615      PERFORM GENERATE-HEADINGS.                                   ELTPAR  
00616      PERFORM BUILD-INSTITUTIONAL-TEXT.                            ELTPAR  
00617      EJECT                                                        ELTPAR  
00618                                                                   ELTPAR  
00619                                                                   ELTPAR  
00620 ************************************************************      ELTPAR  
00621 *                                                          *      ELTPAR  
00622 *        GENERATE PROFESSIONAL                             *      ELTPAR  
00623 *                                                          *      ELTPAR  
00624 ************************************************************      ELTPAR  
00625  GENERATE-PROFESSIONAL.                                           ELTPAR  
00626      SET PROFESSIONAL-SCREEN TO TRUE.                             ELTPAR  
00627      PERFORM GENERATE-HEADINGS.                                   ELTPAR  
00628      PERFORM BUILD-PROFESSIONAL-TEXT.                             ELTPAR  
00629      EJECT                                                        ELTPAR  
00630                                                                   ELTPAR  
00631                                                                   ELTPAR  
00632 ************************************************************      ELTPAR  
00633 *                                                          *      ELTPAR  
00634 *        PROCESS SUPPLEMENTAL                              *      ELTPAR  
00635 *                                                          *      ELTPAR  
00636 ************************************************************      ELTPAR  
00637  PROCESS-SUPPLEMENTAL.                                            ELTPAR  
00638      SET SUPPLEMENTAL-SCREEN TO TRUE.                             ELTPAR  
00639      PERFORM GENERATE-HEADINGS.                                   ELTPAR  
00640      PERFORM BUILD-SUPPLEMENTAL-TEXT.                             ELTPAR  
00641      EJECT                                                        ELTPAR  
00642                                                                   ELTPAR  
00643                                                                   ELTPAR  
00644 ************************************************************      ELTPAR  
00645 *                                                          *      ELTPAR  
00646 *        GENERATE HEADINGS                                 *      ELTPAR  
00647 *                                                          *      ELTPAR  
00648 ************************************************************      ELTPAR  
00649  GENERATE-HEADINGS.                                               ELTPAR  
00650      SET COF-NEW-PAGE TO TRUE.                                    ELTPAR  
00651      IF INSTITUTIONAL-SCREEN                                      ELTPAR  
00652          PERFORM MOVE-INST-HEADINGS                               ELTPAR  
00653      ELSE IF PROFESSIONAL-SCREEN                                  ELTPAR  
00654          PERFORM MOVE-PROF-HEADINGS                               ELTPAR  
00655      ELSE IF SUPPLEMENTAL-SCREEN                                  ELTPAR  
00656          PERFORM MOVE-SUPP-HEADINGS.                              ELTPAR  
00657      MOVE 2            TO COF-NBR-HDR-LINES.                      ELTPAR  
00658      MOVE WS-HDR-LN2   TO COF-HDR-LINE                            ELTPAR  
00659          (COF-NBR-HDR-LINES).                                     ELTPAR  
00660      MOVE +1           TO COF-NBR-DTL-LINES.                      ELTPAR  
00661      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPAR  
00662      IF INSTITUTIONAL-SCREEN                                      ELTPAR  
00663             OR PROFESSIONAL-SCREEN                                ELTPAR  
00664             OR SUPPLEMENTAL-SCREEN                                ELTPAR  
00665          PERFORM LINK-TO-OUTPUT.                                  ELTPAR  
00666                                                                   ELTPAR  
00667                                                                   ELTPAR  
00668 ************************************************************      ELTPAR  
00669 *                                                          *      ELTPAR  
00670 *        MOVE INST HEADINGS                                *      ELTPAR  
00671 *                                                          *      ELTPAR  
00672 ************************************************************      ELTPAR  
00673  MOVE-INST-HEADINGS.                                              ELTPAR  
00674      MOVE 'INSTITUTIONAL' TO HDR-TITLE.                           ELTPAR  
00675      EJECT                                                        ELTPAR  
00676                                                                   ELTPAR  
00677                                                                   ELTPAR  
00678 ************************************************************      ELTPAR  
00679 *                                                          *      ELTPAR  
00680 *        MOVE PROF HEADINGS                                *      ELTPAR  
00681 *                                                          *      ELTPAR  
00682 ************************************************************      ELTPAR  
00683  MOVE-PROF-HEADINGS.                                              ELTPAR  
00684      MOVE 'PROFESSIONAL' TO HDR-TITLE.                            ELTPAR  
00685      EJECT                                                        ELTPAR  
00686                                                                   ELTPAR  
00687                                                                   ELTPAR  
00688 ************************************************************      ELTPAR  
00689 *                                                          *      ELTPAR  
00690 *        MOVE SUPP HEADINGS                                *      ELTPAR  
00691 *                                                          *      ELTPAR  
00692 ************************************************************      ELTPAR  
00693  MOVE-SUPP-HEADINGS.                                              ELTPAR  
00694      MOVE 'SUPPLEMENTAL' TO HDR-TITLE.                            ELTPAR  
00695                                                                   ELTPAR  
00696                                                                   ELTPAR  
00697 ************************************************************      ELTPAR  
00698 *                                                          *      ELTPAR  
00699 *        BUILD INSTITUTIONAL TEXT                          *      ELTPAR  
00700 *                                                          *      ELTPAR  
00701 ************************************************************      ELTPAR  
00702  BUILD-INSTITUTIONAL-TEXT.                                        ELTPAR  
00703      IF GSS-PR-BC-IND (GSS-INDEX) EQUAL ZEROES OR SPACES          ELTPAR  
00704                 OR LOW-VALUES                                     ELTPAR  
00705          PERFORM SIGNAL-NOT-APPLICABLE-FOR-CROS                   ELTPAR  
00706      ELSE                                                         ELTPAR  
00707          PERFORM CONSTRUCT-BC-TEXT-AND-SCREEN.                    ELTPAR  
00708                                                                   ELTPAR  
00709                                                                   ELTPAR  
00710 ************************************************************      ELTPAR  
00711 *                                                          *      ELTPAR  
00712 *        BUILD PROFESSIONAL TEXT                           *      ELTPAR  
00713 *                                                          *      ELTPAR  
00714 ************************************************************      ELTPAR  
00715  BUILD-PROFESSIONAL-TEXT.                                         ELTPAR  
00716      IF GSS-PR-BS-IND (GSS-INDEX) EQUAL ZEROES OR SPACES          ELTPAR  
00717                 OR LOW-VALUES                                     ELTPAR  
00718          PERFORM SIGNAL-NOT-APPLICABLE-FOR-SHEI                   ELTPAR  
00719      ELSE                                                         ELTPAR  
00720          PERFORM CONSTRUCT-BS-TEXT-AND-SCREEN.                    ELTPAR  
00721                                                                   ELTPAR  
00722                                                                   ELTPAR  
00723 ************************************************************      ELTPAR  
00724 *                                                          *      ELTPAR  
00725 *        BUILD SUPPLEMENTAL TEXT                           *      ELTPAR  
00726 *                                                          *      ELTPAR  
00727 ************************************************************      ELTPAR  
00728  BUILD-SUPPLEMENTAL-TEXT.                                         ELTPAR  
00729      IF GSS-PR-MM-IND (GSS-INDEX) EQUAL ZEROES OR SPACES          ELTPAR  
00730                 OR LOW-VALUES                                     ELTPAR  
00731          PERFORM SIGNAL-NOT-APPLICABLE-FOR-LOBX                   ELTPAR  
00732      ELSE                                                         ELTPAR  
00733          PERFORM CONSTRUCT-MM-TEXT-AND-SCREEN.                    ELTPAR  
00734      EJECT                                                        ELTPAR  
00735                                                                   ELTPAR  
00736                                                                   ELTPAR  
00737 ************************************************************      ELTPAR  
00738 *                                                          *      ELTPAR  
00739 *        SIGNAL NOT APPLICABLE FOR CROSS                   *      ELTPAR  
00740 *                                                          *      ELTPAR  
00741 ************************************************************      ELTPAR  
00742  SIGNAL-NOT-APPLICABLE-FOR-CROS.                                  ELTPAR  
00743      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAR  
00744      MOVE WS-NOT-APPLICABLE-LOB-BC TO COF-DTL-LINE                ELTPAR  
00745          (COF-NBR-DTL-LINES).                                     ELTPAR  
00746      PERFORM LINK-TO-OUTPUT.                                      ELTPAR  
00747                                                                   ELTPAR  
00748                                                                   ELTPAR  
00749 ************************************************************      ELTPAR  
00750 *                                                          *      ELTPAR  
00751 *        SIGNAL NOT APPLICABLE FOR SHEILD                  *      ELTPAR  
00752 *                                                          *      ELTPAR  
00753 ************************************************************      ELTPAR  
00754  SIGNAL-NOT-APPLICABLE-FOR-SHEI.                                  ELTPAR  
00755      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAR  
00756      MOVE WS-NOT-APPLICABLE-LOB-BS TO COF-DTL-LINE                ELTPAR  
00757          (COF-NBR-DTL-LINES).                                     ELTPAR  
00758      PERFORM LINK-TO-OUTPUT.                                      ELTPAR  
00759                                                                   ELTPAR  
00760                                                                   ELTPAR  
00761 ************************************************************      ELTPAR  
00762 *                                                          *      ELTPAR  
00763 *        SIGNAL NOT APPLICABLE FOR LOB MAJ MED             *      ELTPAR  
00764 *                                                          *      ELTPAR  
00765 ************************************************************      ELTPAR  
00766  SIGNAL-NOT-APPLICABLE-FOR-LOBX.                                  ELTPAR  
00767      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAR  
00768      MOVE WS-NOT-APPLICABLE-LOB-MM TO COF-DTL-LINE                ELTPAR  
00769          (COF-NBR-DTL-LINES).                                     ELTPAR  
00770      PERFORM LINK-TO-OUTPUT.                                      ELTPAR  
00771      EJECT                                                        ELTPAR  
00772                                                                   ELTPAR  
00773                                                                   ELTPAR  
00774 ************************************************************      ELTPAR  
00775 *                                                          *      ELTPAR  
00776 *        CONSTRUCT BC TEXT AND SCREEN                      *      ELTPAR  
00777 *                                                          *      ELTPAR  
00778 ************************************************************      ELTPAR  
00779  CONSTRUCT-BC-TEXT-AND-SCREEN.                                    ELTPAR  
00780      PERFORM TRANSLATE-PARTICIPATION-INDICA.                      ELTPAR  
00781      IF GSS-PR-PROG-SOURCE-IND (GSS-INDEX) NOT EQUAL ZERO         ELTPAR  
00782                 AND SPACES AND LOW-VALUES                         ELTPAR  
00783          PERFORM TRANSLATE-PROGRAM-SOURCE.                        ELTPAR  
00784      PERFORM TRANSLATE-BC-INDICATOR.                              ELTPAR  
00785      IF GSS-PR-BC-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL         ELTPAR  
00786          ZERO                                                     ELTPAR  
00787               AND SPACES AND LOW-VALUES                           ELTPAR  
00788          PERFORM TRANSLATE-BC-PAYMENT-LEVEL-IND.                  ELTPAR  
00789      SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTPAR  
00790      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTPAR  
00791      PERFORM GENERATE-BC-CALC-METHOD-SENTEN.                      ELTPAR  
00792      PERFORM GENERATE-BC-BENEFITS-REDUCTION.                      ELTPAR  
00793      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                       ELTPAR  
00794               '03' OR '04' OR '06' OR '08')                       ELTPAR  
00795            AND                                                    ELTPAR  
00796             GSS-PR-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL           ELTPAR  
00797          ZERO                                                     ELTPAR  
00798                         AND SPACES AND LOW-VALUES                 ELTPAR  
00799          PERFORM GENERATE-SPILL-OVER-INDICATOR.                   ELTPAR  
00800      PERFORM GENERATE-ADDITIONAL-TABULARS.                        ELTPAR  
00801      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTPAR  
00802      MOVE SPACES TO SCREEN-TYPE.                                  ELTPAR  
00803      EJECT                                                        ELTPAR  
00804                                                                   ELTPAR  
00805                                                                   ELTPAR  
00806 ************************************************************      ELTPAR  
00807 *                                                          *      ELTPAR  
00808 *        CONSTRUCT BS TEXT AND SCREEN                      *      ELTPAR  
00809 *                                                          *      ELTPAR  
00810 ************************************************************      ELTPAR  
00811  CONSTRUCT-BS-TEXT-AND-SCREEN.                                    ELTPAR  
00812      SET PROFESSIONAL-SCREEN TO TRUE.                             ELTPAR  
00813      PERFORM TRANSLATE-PARTICIPATION-INDICA.                      ELTPAR  
00814      IF GSS-PR-PROG-SOURCE-IND (GSS-INDEX) NOT EQUAL ZERO         ELTPAR  
00815                 AND SPACES AND LOW-VALUES                         ELTPAR  
00816          PERFORM TRANSLATE-PROGRAM-SOURCE.                        ELTPAR  
00817      PERFORM TRANSLATE-BS-INDICATOR.                              ELTPAR  
00818      IF GSS-PR-BS-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL         ELTPAR  
00819          ZERO                                                     ELTPAR  
00820               AND SPACES AND LOW-VALUES                           ELTPAR  
00821          PERFORM TRANSLATE-BS-PAYMENT-LEVEL-IND.                  ELTPAR  
00822      SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTPAR  
00823      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTPAR  
00824      PERFORM GENERATE-BS-CALC-METHOD-SENTEN.                      ELTPAR  
00825      PERFORM GENERATE-BS-BENEFITS-REDUCTION.                      ELTPAR  
00826      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                       ELTPAR  
00827                   '03' OR '04' OR '06' OR '08')                   ELTPAR  
00828            AND                                                    ELTPAR  
00829             GSS-PR-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL           ELTPAR  
00830          ZERO                                                     ELTPAR  
00831                         AND SPACES AND LOW-VALUES                 ELTPAR  
00832          PERFORM GENERATE-SPILL-OVER-INDICATOR.                   ELTPAR  
00833      PERFORM GENERATE-ADDITIONAL-TABULARS.                        ELTPAR  
00834      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTPAR  
00835      MOVE SPACES TO SCREEN-TYPE.                                  ELTPAR  
00836      EJECT                                                        ELTPAR  
00837                                                                   ELTPAR  
00838                                                                   ELTPAR  
00839 ************************************************************      ELTPAR  
00840 *                                                          *      ELTPAR  
00841 *        CONSTRUCT MM TEXT AND SCREEN                      *      ELTPAR  
00842 *                                                          *      ELTPAR  
00843 ************************************************************      ELTPAR  
00844  CONSTRUCT-MM-TEXT-AND-SCREEN.                                    ELTPAR  
00845      PERFORM TRANSLATE-PARTICIPATION-INDICA.                      ELTPAR  
00846      IF GSS-PR-PROG-SOURCE-IND (GSS-INDEX) NOT EQUAL ZERO         ELTPAR  
00847                 AND SPACES AND LOW-VALUES                         ELTPAR  
00848          PERFORM TRANSLATE-PROGRAM-SOURCE.                        ELTPAR  
00849      PERFORM TRANSLATE-MM-INDICATOR.                              ELTPAR  
00850      IF GSS-PR-MM-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL         ELTPAR  
00851          ZERO                                                     ELTPAR  
00852             AND SPACES AND LOW-VALUES                             ELTPAR  
00853          PERFORM TRANSLATE-MM-PAYMENT-LEVEL-IND.                  ELTPAR  
00854      SET SRP-ACCUM-PROV-CLASS-SUPP TO TRUE.                       ELTPAR  
00855      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTPAR  
00856      PERFORM GENERATE-MM-CALC-METHOD-SENTEN.                      ELTPAR  
00857      PERFORM GENERATE-MM-BENEFITS-REDUCTION.                      ELTPAR  
00858      PERFORM GENERATE-ADDITIONAL-TABULARS.                        ELTPAR  
00859      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTPAR  
00860      EJECT                                                        ELTPAR  
00861                                                                   ELTPAR  
00862                                                                   ELTPAR  
00863 ************************************************************      ELTPAR  
00864 *                                                          *      ELTPAR  
00865 *        GENERATE ASSOCIATED ACCUMULATORS                  *      ELTPAR  
00866 *                                                          *      ELTPAR  
00867 ************************************************************      ELTPAR  
00868  GENERATE-ASSOCIATED-ACCUMULATO.                                  ELTPAR  
00869      PERFORM GENERATE-COINSURANCE-TEXT.                           ELTPAR  
00870      PERFORM GENERATE-COPAY-TEXT.                                 ELTPAR  
00871      PERFORM GENERATE-DEDUCTIBLE-TEXT.                            ELTPAR  
00872      PERFORM GENERATE-BENEFIT-MAXIMUMS-TEXT.                      ELTPAR  
00873      EJECT                                                        ELTPAR  
00874                                                                   ELTPAR  
00875                                                                   ELTPAR  
00876 ************************************************************      ELTPAR  
00877 *                                                          *      ELTPAR  
00878 *        GENERATE DISCLAIMER SENTENCE                      *      ELTPAR  
00879 *                                                          *      ELTPAR  
00880 ************************************************************      ELTPAR  
00881  GENERATE-DISCLAIMER-SENTENCE.                                    ELTPAR  
00882      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAR  
00883      MOVE WS-DISCLAIMER TO COF-DTL-LINE                           ELTPAR  
00884          (COF-NBR-DTL-LINES).                                     ELTPAR  
00885      PERFORM LINK-TO-OUTPUT.                                      ELTPAR  
00886      EJECT                                                        ELTPAR  
00887                                                                   ELTPAR  
00888                                                                   ELTPAR  
00889 ************************************************************      ELTPAR  
00890 *                                                          *      ELTPAR  
00891 *        TRANSLATE PARTICIPATION INDICATOR                 *      ELTPAR  
00892 *                                                          *      ELTPAR  
00893 ************************************************************      ELTPAR  
00894  TRANSLATE-PARTICIPATION-INDICA.                                  ELTPAR  
00895      SET PERIOD-NEEDED TO TRUE.                                   ELTPAR  
00896      INITIALIZE TCAR-FROM-AREA.                                   ELTPAR  
00897      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAR  
00898      MOVE WS-PARTICIPATION-LINE TO TCAR-FROM-LINE                 ELTPAR  
00899          (TCAR-FROM-SUB).                                         ELTPAR  
00900      ADD 1 TO TCAR-FROM-SUB.                                      ELTPAR  
00901      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAR  
00902      MOVE GCG-PRE-ADM-REVIEW-IND TO CMF-CODE-VALUE.               ELTPAR  
00903      MOVE 'PRE-ADM-REVIEW-IND' TO CMF-ELEMENT-SYSTEM-NAME.        ELTPAR  
00904      PERFORM GROUP-LINK-TO-TRANSLATOR.                            ELTPAR  
00905      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAR  
00906      EJECT                                                        ELTPAR  
00907                                                                   ELTPAR  
00908                                                                   ELTPAR  
00909 ************************************************************      ELTPAR  
00910 *                                                          *      ELTPAR  
00911 *        TRANSLATE PROGRAM SOURCE                          *      ELTPAR  
00912 *                                                          *      ELTPAR  
00913 ************************************************************      ELTPAR  
00914  TRANSLATE-PROGRAM-SOURCE.                                        ELTPAR  
00915      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPAR  
00916      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAR  
00917      SET PERIOD-NEEDED TO TRUE.                                   ELTPAR  
00918      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAR  
00919      MOVE WS-PROGRAM-SOURCE TO TCAR-FROM-LINE (TCAR-FROM-SUB).    ELTPAR  
00920      ADD 1 TO TCAR-FROM-SUB.                                      ELTPAR  
00921      MOVE 'PR-PROG-SOURCE-IND' TO CMF-ELEMENT-SYSTEM-NAME.        ELTPAR  
00922      MOVE GSS-PR-PROG-SOURCE-IND (GSS-INDEX) TO CMF-CODE-VALUE.   ELTPAR  
00923      PERFORM LINK-TO-TRANSLATOR.                                  ELTPAR  
00924      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAR  
00925                                                                   ELTPAR  
00926                                                                   ELTPAR  
00927 ************************************************************      ELTPAR  
00928 *                                                          *      ELTPAR  
00929 *        GET INDICATOR FIXED TEXT                          *      ELTPAR  
00930 *                                                          *      ELTPAR  
00931 ************************************************************      ELTPAR  
00932  GET-INDICATOR-FIXED-TEXT.                                        ELTPAR  
00933      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPAR  
00934      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAR  
00935      SET PERIOD-NEEDED TO TRUE.                                   ELTPAR  
00936      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAR  
00937      MOVE WS-INDICATOR TO TCAR-FROM-LINE (TCAR-FROM-SUB).         ELTPAR  
00938      ADD 1 TO TCAR-FROM-SUB.                                      ELTPAR  
00939      EJECT                                                        ELTPAR  
00940                                                                   ELTPAR  
00941                                                                   ELTPAR  
00942 ************************************************************      ELTPAR  
00943 *                                                          *      ELTPAR  
00944 *        TRANSLATE BC INDICATOR                            *      ELTPAR  
00945 *                                                          *      ELTPAR  
00946 ************************************************************      ELTPAR  
00947  TRANSLATE-BC-INDICATOR.                                          ELTPAR  
00948      PERFORM GET-INDICATOR-FIXED-TEXT.                            ELTPAR  
00949      MOVE 'PR-BC-IND'  TO CMF-ELEMENT-SYSTEM-NAME.                ELTPAR  
00950      MOVE GSS-PR-BC-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTPAR  
00951      PERFORM LINK-TO-TRANSLATOR.                                  ELTPAR  
00952      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAR  
00953      EJECT                                                        ELTPAR  
00954                                                                   ELTPAR  
00955                                                                   ELTPAR  
00956 ************************************************************      ELTPAR  
00957 *                                                          *      ELTPAR  
00958 *        TRANSLATE BC PAYMENT LEVEL INDICATOR              *      ELTPAR  
00959 *                                                          *      ELTPAR  
00960 ************************************************************      ELTPAR  
00961  TRANSLATE-BC-PAYMENT-LEVEL-IND.                                  ELTPAR  
00962      PERFORM SETUP-PAYMENT-LEVEL-FIXED-TEXT.                      ELTPAR  
00963      MOVE 'PR-BC-PAYMENT-LEVEL-IND'  TO CMF-ELEMENT-SYSTEM-NAME.  ELTPAR  
00964      MOVE GSS-PR-BC-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTPAR  
00965          CMF-CODE-VALUE.                                          ELTPAR  
00966      PERFORM LINK-TO-TRANSLATOR.                                  ELTPAR  
00967      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAR  
00968                                                                   ELTPAR  
00969                                                                   ELTPAR  
00970 ************************************************************      ELTPAR  
00971 *                                                          *      ELTPAR  
00972 *        SETUP PAYMENT LEVEL FIXED TEXT                    *      ELTPAR  
00973 *                                                          *      ELTPAR  
00974 ************************************************************      ELTPAR  
00975  SETUP-PAYMENT-LEVEL-FIXED-TEXT.                                  ELTPAR  
00976      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPAR  
00977      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAR  
00978      SET PERIOD-NEEDED TO TRUE.                                   ELTPAR  
00979      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAR  
00980      MOVE WS-PAYMENT-LEVEL TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELTPAR  
00981      ADD 1 TO TCAR-FROM-SUB.                                      ELTPAR  
00982                                                                   ELTPAR  
00983                                                                   ELTPAR  
00984 ************************************************************      ELTPAR  
00985 *                                                          *      ELTPAR  
00986 *        GENERATE COINSURANCE TEXT                         *      ELTPAR  
00987 *                                                          *      ELTPAR  
00988 ************************************************************      ELTPAR  
00989  GENERATE-COINSURANCE-TEXT.                                       ELTPAR  
00990      EXEC CICS LINK                                               ELTPAR  
00991          PROGRAM ('ELGACLCC')                                     ELTPAR  
00992          COMMAREA (DFHCOMMAREA)                                   ELTPAR  
00993          END-EXEC.                                                ELTPAR  
00994                                                                   ELTPAR  
00995                                                                   ELTPAR  
00996 ************************************************************      ELTPAR  
00997 *                                                          *      ELTPAR  
00998 *        GENERATE DEDUCTIBLE TEXT                          *      ELTPAR  
00999 *                                                          *      ELTPAR  
01000 ************************************************************      ELTPAR  
01001  GENERATE-DEDUCTIBLE-TEXT.                                        ELTPAR  
01002      EXEC CICS LINK                                               ELTPAR  
01003          PROGRAM ('ELGADLCC')                                     ELTPAR  
01004          COMMAREA (DFHCOMMAREA)                                   ELTPAR  
01005          END-EXEC.                                                ELTPAR  
01006                                                                   ELTPAR  
01007                                                                   ELTPAR  
01008 ************************************************************      ELTPAR  
01009 *                                                          *      ELTPAR  
01010 *        GENERATE COPAY      TEXT                          *      ELTPAR  
01011 *                                                          *      ELTPAR  
01012 ************************************************************      ELTPAR  
01013  GENERATE-COPAY-TEXT.                                             ELTPAR  
01014      EXEC CICS LINK                                               ELTPAR  
01015          PROGRAM ('ELGACPCC')                                     ELTPAR  
01016          COMMAREA (DFHCOMMAREA)                                   ELTPAR  
01017          END-EXEC.                                                ELTPAR  
01018                                                                   ELTPAR  
01019                                                                   ELTPAR  
01020 ************************************************************      ELTPAR  
01021 *                                                          *      ELTPAR  
01022 *        GENERATE BENEFIT MAXIMUMS TEXT                    *      ELTPAR  
01023 *                                                          *      ELTPAR  
01024 ************************************************************      ELTPAR  
01025  GENERATE-BENEFIT-MAXIMUMS-TEXT.                                  ELTPAR  
01026      EXEC CICS LINK                                               ELTPAR  
01027          PROGRAM ('ELGABMCC')                                     ELTPAR  
01028          COMMAREA (DFHCOMMAREA)                                   ELTPAR  
01029          END-EXEC.                                                ELTPAR  
01030      EJECT                                                        ELTPAR  
01031                                                                   ELTPAR  
01032                                                                   ELTPAR  
01033 ************************************************************      ELTPAR  
01034 *                                                          *      ELTPAR  
01035 *        GENERATE BC CALC METHOD SENTENCE                  *      ELTPAR  
01036 *                                                          *      ELTPAR  
01037 ************************************************************      ELTPAR  
01038  GENERATE-BC-CALC-METHOD-SENTEN.                                  ELTPAR  
01039      IF GSS-PR-BC-CALC-METHOD (GSS-INDEX) NOT EQUAL ZEROES        ELTPAR  
01040                AND SPACES AND LOW-VALUES                          ELTPAR  
01041          PERFORM CREATE-BC-CALC-SENTENCE.                         ELTPAR  
01042                                                                   ELTPAR  
01043                                                                   ELTPAR  
01044 ************************************************************      ELTPAR  
01045 *                                                          *      ELTPAR  
01046 *        CREATE BC CALC SENTENCE                           *      ELTPAR  
01047 *                                                          *      ELTPAR  
01048 ************************************************************      ELTPAR  
01049  CREATE-BC-CALC-SENTENCE.                                         ELTPAR  
01050      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPAR  
01051      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAR  
01052      SET PERIOD-NEEDED TO TRUE.                                   ELTPAR  
01053      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAR  
01054      MOVE 'PR-BC-CALC-METHOD'  TO                                 ELTPAR  
01055          CMF-ELEMENT-SYSTEM-NAME.                                 ELTPAR  
01056      MOVE GSS-PR-BC-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTPAR  
01057      PERFORM LINK-TO-TRANSLATOR.                                  ELTPAR  
01058      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAR  
01059      EJECT                                                        ELTPAR  
01060                                                                   ELTPAR  
01061                                                                   ELTPAR  
01062 ************************************************************      ELTPAR  
01063 *                                                          *      ELTPAR  
01064 *        GENERATE BS CALC METHOD SENTENCE                  *      ELTPAR  
01065 *                                                          *      ELTPAR  
01066 ************************************************************      ELTPAR  
01067  GENERATE-BS-CALC-METHOD-SENTEN.                                  ELTPAR  
01068      IF GSS-PR-BS-CALC-METHOD (GSS-INDEX) NOT EQUAL ZEROES        ELTPAR  
01069                AND SPACES AND LOW-VALUES                          ELTPAR  
01070          PERFORM CREATE-BS-CALC-SENTENCE.                         ELTPAR  
01071                                                                   ELTPAR  
01072                                                                   ELTPAR  
01073 ************************************************************      ELTPAR  
01074 *                                                          *      ELTPAR  
01075 *        CREATE BS CALC SENTENCE                           *      ELTPAR  
01076 *                                                          *      ELTPAR  
01077 ************************************************************      ELTPAR  
01078  CREATE-BS-CALC-SENTENCE.                                         ELTPAR  
01079      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPAR  
01080      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAR  
01081      SET PERIOD-NEEDED TO TRUE.                                   ELTPAR  
01082      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAR  
01083      MOVE 'PR-BS-CALC-METHOD'  TO                                 ELTPAR  
01084          CMF-ELEMENT-SYSTEM-NAME.                                 ELTPAR  
01085      MOVE GSS-PR-BS-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTPAR  
01086      PERFORM LINK-TO-TRANSLATOR.                                  ELTPAR  
01087      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAR  
01088      EJECT                                                        ELTPAR  
01089                                                                   ELTPAR  
01090                                                                   ELTPAR  
01091 ************************************************************      ELTPAR  
01092 *                                                          *      ELTPAR  
01093 *        GENERATE MM CALC METHOD SENTENCE                  *      ELTPAR  
01094 *                                                          *      ELTPAR  
01095 ************************************************************      ELTPAR  
01096  GENERATE-MM-CALC-METHOD-SENTEN.                                  ELTPAR  
01097      IF GSS-PR-MM-CALC-METHOD (GSS-INDEX) NOT EQUAL ZEROES        ELTPAR  
01098                AND SPACES AND LOW-VALUES                          ELTPAR  
01099          PERFORM CREATE-MM-CALC-SENTENCE.                         ELTPAR  
01100                                                                   ELTPAR  
01101                                                                   ELTPAR  
01102 ************************************************************      ELTPAR  
01103 *                                                          *      ELTPAR  
01104 *        CREATE MM CALC SENTENCE                           *      ELTPAR  
01105 *                                                          *      ELTPAR  
01106 ************************************************************      ELTPAR  
01107  CREATE-MM-CALC-SENTENCE.                                         ELTPAR  
01108      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPAR  
01109      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAR  
01110      SET PERIOD-NEEDED TO TRUE.                                   ELTPAR  
01111      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAR  
01112      MOVE 'PR-MM-CALC-METHOD'  TO                                 ELTPAR  
01113          CMF-ELEMENT-SYSTEM-NAME.                                 ELTPAR  
01114      MOVE GSS-PR-MM-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTPAR  
01115      PERFORM LINK-TO-TRANSLATOR.                                  ELTPAR  
01116      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAR  
01117                                                                   ELTPAR  
01118                                                                   ELTPAR  
01119 ************************************************************      ELTPAR  
01120 *                                                          *      ELTPAR  
01121 *        GENERATE COMBINED BENEFITS REDUCTION SENTENCE     *      ELTPAR  
01122 *                                                          *      ELTPAR  
01123 ************************************************************      ELTPAR  
01124  GENERATE-COMBINED-BENEFITS-RED.                                  ELTPAR  
01125      MOVE 'PR' TO SRP-COST-CONT-TYPE.                             ELTPAR  
01126      MOVE 'PRE-ADMISSION PROGRAM' TO SRP-CCP-NAME.                ELTPAR  
01127      MOVE GSS-PR-COMB-BENE-REDUCT-IND (GSS-INDEX) TO              ELTPAR  
01128           SRP-CCP-COMB-BENE-REDUCT-IND.                           ELTPAR  
01129      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELTPAR  
01130      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTPAR  
01131                            ADDRESS OF GCCP-TABULAR-REC.           ELTPAR  
01132      CALL 'ELGCBRI' USING DFHEIBLK                                ELTPAR  
01133                           DFHCOMMAREA.                            ELTPAR  
01134      EJECT                                                        ELTPAR  
01135                                                                   ELTPAR  
01136                                                                   ELTPAR  
01137 ************************************************************      ELTPAR  
01138 *                                                          *      ELTPAR  
01139 *        GENERATE RELATED DIAGNOSES                        *      ELTPAR  
01140 *                                                          *      ELTPAR  
01141 ************************************************************      ELTPAR  
01142  GENERATE-RELATED-DIAGNOSES.                                      ELTPAR  
01143      MOVE 'PRE-ADMISSION REVIEW' TO SRP-CCP-NAME.                 ELTPAR  
01144      MOVE WS-GPAD-DIAG-ID TO SRP-TABULAR-ID.                      ELTPAR  
01145      MOVE WS-GPAD-DIAG-SLOT-NO TO SRP-TABULAR-SLOT-NO.            ELTPAR  
01146      EXEC CICS LINK                                               ELTPAR  
01147          PROGRAM ('ELGGXXD')                                      ELTPAR  
01148          COMMAREA (DFHCOMMAREA)                                   ELTPAR  
01149          END-EXEC.                                                ELTPAR  
01150      EJECT                                                        ELTPAR  
01151                                                                   ELTPAR  
01152                                                                   ELTPAR  
01153 ************************************************************      ELTPAR  
01154 *                                                          *      ELTPAR  
01155 *        GENERATE RELATED SERVICES                         *      ELTPAR  
01156 *                                                          *      ELTPAR  
01157 ************************************************************      ELTPAR  
01158  GENERATE-RELATED-SERVICES.                                       ELTPAR  
01159      MOVE 'PRE-ADMISSION REVIEW' TO SRP-CCP-NAME.                 ELTPAR  
01160      MOVE WS-GPAB-SRVS-ID TO SRP-TABULAR-ID.                      ELTPAR  
01161      MOVE WS-GPAB-SRVS-SLOT-NO TO SRP-TABULAR-SLOT-NO.            ELTPAR  
01162      EXEC CICS LINK                                               ELTPAR  
01163          PROGRAM ('ELGGXXB')                                      ELTPAR  
01164          COMMAREA (DFHCOMMAREA)                                   ELTPAR  
01165          END-EXEC.                                                ELTPAR  
01166      EJECT                                                        ELTPAR  
01167                                                                   ELTPAR  
01168                                                                   ELTPAR  
01169 ************************************************************      ELTPAR  
01170 *                                                          *      ELTPAR  
01171 *        GENERATE RELATED PROCEDURES                       *      ELTPAR  
01172 *                                                          *      ELTPAR  
01173 ************************************************************      ELTPAR  
01174  GENERATE-RELATED-PROCEDURES.                                     ELTPAR  
01175      MOVE 'PRE-ADMISSION REVIEW' TO SRP-CCP-NAME.                 ELTPAR  
01176      MOVE WS-GPAR-PROC-ID TO SRP-TABULAR-ID.                      ELTPAR  
01177      MOVE WS-GPAR-PROC-SLOT-NO TO SRP-TABULAR-SLOT-NO.            ELTPAR  
01178      EXEC CICS LINK                                               ELTPAR  
01179          PROGRAM ('ELGGXXR')                                      ELTPAR  
01180          COMMAREA (DFHCOMMAREA)                                   ELTPAR  
01181          END-EXEC.                                                ELTPAR  
01182      EJECT                                                        ELTPAR  
01183                                                                   ELTPAR  
01184                                                                   ELTPAR  
01185 ************************************************************      ELTPAR  
01186 *                                                          *      ELTPAR  
01187 *        GENERATE RELATED PROVIDERS                        *      ELTPAR  
01188 *                                                          *      ELTPAR  
01189 ************************************************************      ELTPAR  
01190  GENERATE-RELATED-PROVIDERS.                                      ELTPAR  
01191      MOVE 'PRE-ADMISSION REVIEW' TO SRP-CCP-NAME.                 ELTPAR  
01192      MOVE WS-GPAC-PROV-ID TO SRP-TABULAR-ID.                      ELTPAR  
01193      MOVE WS-GPAC-PROV-SLOT-NO TO SRP-TABULAR-SLOT-NO.            ELTPAR  
01194      EXEC CICS LINK                                               ELTPAR  
01195          PROGRAM ('ELGGXXC')                                      ELTPAR  
01196          COMMAREA (DFHCOMMAREA)                                   ELTPAR  
01197          END-EXEC.                                                ELTPAR  
01198      EJECT                                                        ELTPAR  
01199                                                                   ELTPAR  
01200                                                                   ELTPAR  
01201 ************************************************************      ELTPAR  
01202 *                                                          *      ELTPAR  
01203 *        GENERATE BC BENEFITS REDUCTION SENTENCE           *      ELTPAR  
01204 *                                                          *      ELTPAR  
01205 ************************************************************      ELTPAR  
01206  GENERATE-BC-BENEFITS-REDUCTION.                                  ELTPAR  
01207      IF GSS-PR-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTPAR  
01208                  ZERO AND SPACES AND LOW-VALUES                   ELTPAR  
01209          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTPAR  
01210      IF (GSS-PR-BC-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL          ELTPAR  
01211          ZERO                                                     ELTPAR  
01212                             AND SPACES AND LOW-VALUES)  OR        ELTPAR  
01213                (GSS-PR-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL   ELTPAR  
01214          ZERO                                                     ELTPAR  
01215                             AND SPACES AND LOW-VALUES)  OR        ELTPAR  
01216                (GSS-PR-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT       ELTPAR  
01217          EQUAL ZERO                                               ELTPAR  
01218                             AND SPACES AND LOW-VALUES)            ELTPAR  
01219          PERFORM GENERATE-BC-BENEFIT-REDUCTIONX.                  ELTPAR  
01220      EJECT                                                        ELTPAR  
01221                                                                   ELTPAR  
01222                                                                   ELTPAR  
01223 ************************************************************      ELTPAR  
01224 *                                                          *      ELTPAR  
01225 *        GENERATE BC BENEFIT REDUCTION TEXT                *      ELTPAR  
01226 *                                                          *      ELTPAR  
01227 ************************************************************      ELTPAR  
01228  GENERATE-BC-BENEFIT-REDUCTIONX.                                  ELTPAR  
01229      PERFORM GENERATE-REDUCTIONS-HEADINGS.                        ELTPAR  
01230      IF GSS-PR-BC-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTPAR  
01231          ZERO                                                     ELTPAR  
01232                             AND SPACES AND LOW-VALUES             ELTPAR  
01233          PERFORM TRANSLATE-BC-DEDU-APPLIC.                        ELTPAR  
01234      IF GSS-PR-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTPAR  
01235          ZERO                                                     ELTPAR  
01236                             AND SPACES AND LOW-VALUES             ELTPAR  
01237          PERFORM TRANSLATE-BC-OPEX-APPLIC.                        ELTPAR  
01238      PERFORM CREATE-A-BLANK-LINE.                                 ELTPAR  
01239      PERFORM LINK-TO-OUTPUT.                                      ELTPAR  
01240      IF GSS-PR-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTPAR  
01241          ZERO                                                     ELTPAR  
01242                             AND SPACES AND LOW-VALUES             ELTPAR  
01243          PERFORM TRANSLATE-BC-OPEX-OVERRIDE.                      ELTPAR  
01244      EJECT                                                        ELTPAR  
01245                                                                   ELTPAR  
01246                                                                   ELTPAR  
01247 ************************************************************      ELTPAR  
01248 *                                                          *      ELTPAR  
01249 *        GENERATE REDUCTIONS HEADINGS                      *      ELTPAR  
01250 *                                                          *      ELTPAR  
01251 ************************************************************      ELTPAR  
01252  GENERATE-REDUCTIONS-HEADINGS.                                    ELTPAR  
01253      INITIALIZE WS-PERIOD-SW.                                     ELTPAR  
01254      INITIALIZE ADDITIONAL-TEXT-SW.                               ELTPAR  
01255      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAR  
01256      MOVE WS-BENEFITS-REDUCTION TO COF-DTL-LINE                   ELTPAR  
01257          (COF-NBR-DTL-LINES).                                     ELTPAR  
01258      PERFORM LINK-TO-OUTPUT.                                      ELTPAR  
01259      EJECT                                                        ELTPAR  
01260                                                                   ELTPAR  
01261                                                                   ELTPAR  
01262 ************************************************************      ELTPAR  
01263 *                                                          *      ELTPAR  
01264 *        GENERATE ADDITIONAL TABULARS                      *      ELTPAR  
01265 *                                                          *      ELTPAR  
01266 ************************************************************      ELTPAR  
01267  GENERATE-ADDITIONAL-TABULARS.                                    ELTPAR  
01268      PERFORM OBTAIN-GPAD.                                         ELTPAR  
01269      PERFORM OBTAIN-GPAB.                                         ELTPAR  
01270      PERFORM OBTAIN-GPAR.                                         ELTPAR  
01271      PERFORM OBTAIN-GPAC.                                         ELTPAR  
01272      EJECT                                                        ELTPAR  
01273                                                                   ELTPAR  
01274                                                                   ELTPAR  
01275 ************************************************************      ELTPAR  
01276 *                                                          *      ELTPAR  
01277 *        TRANSLATE BS INDICATOR                            *      ELTPAR  
01278 *                                                          *      ELTPAR  
01279 ************************************************************      ELTPAR  
01280  TRANSLATE-BS-INDICATOR.                                          ELTPAR  
01281      PERFORM GET-INDICATOR-FIXED-TEXT.                            ELTPAR  
01282      MOVE 'PR-BS-IND' TO CMF-ELEMENT-SYSTEM-NAME.                 ELTPAR  
01283      MOVE GSS-PR-BS-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTPAR  
01284      PERFORM LINK-TO-TRANSLATOR.                                  ELTPAR  
01285      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAR  
01286      EJECT                                                        ELTPAR  
01287                                                                   ELTPAR  
01288                                                                   ELTPAR  
01289 ************************************************************      ELTPAR  
01290 *                                                          *      ELTPAR  
01291 *        TRANSLATE BS PAYMENT LEVEL INDICATOR              *      ELTPAR  
01292 *                                                          *      ELTPAR  
01293 ************************************************************      ELTPAR  
01294  TRANSLATE-BS-PAYMENT-LEVEL-IND.                                  ELTPAR  
01295      PERFORM SETUP-PAYMENT-LEVEL-FIXED-TEXT.                      ELTPAR  
01296      MOVE 'PR-BS-PAYMENT-LEVEL-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTPAR  
01297      MOVE GSS-PR-BS-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTPAR  
01298          CMF-CODE-VALUE.                                          ELTPAR  
01299      PERFORM LINK-TO-TRANSLATOR.                                  ELTPAR  
01300      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAR  
01301      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPAR  
01302      EJECT                                                        ELTPAR  
01303                                                                   ELTPAR  
01304                                                                   ELTPAR  
01305 ************************************************************      ELTPAR  
01306 *                                                          *      ELTPAR  
01307 *        GENERATE BS BENEFITS REDUCTION SENTENCE           *      ELTPAR  
01308 *                                                          *      ELTPAR  
01309 ************************************************************      ELTPAR  
01310  GENERATE-BS-BENEFITS-REDUCTION.                                  ELTPAR  
01311      IF GSS-PR-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTPAR  
01312                  ZERO AND SPACES AND LOW-VALUES                   ELTPAR  
01313          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTPAR  
01314      IF (GSS-PR-BS-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL          ELTPAR  
01315          ZERO                                                     ELTPAR  
01316                             AND SPACES AND LOW-VALUES)  OR        ELTPAR  
01317               (GSS-PR-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL    ELTPAR  
01318          ZERO                                                     ELTPAR  
01319                             AND SPACES AND LOW-VALUES)  OR        ELTPAR  
01320                (GSS-PR-BS-OPEX-OVERRIDE-IND (GSS-INDEX) NOT       ELTPAR  
01321          EQUAL ZERO                                               ELTPAR  
01322                             AND SPACES AND LOW-VALUES)            ELTPAR  
01323          PERFORM GENERATE-BS-BENEFIT-REDUCTIONX.                  ELTPAR  
01324      EJECT                                                        ELTPAR  
01325                                                                   ELTPAR  
01326                                                                   ELTPAR  
01327 ************************************************************      ELTPAR  
01328 *                                                          *      ELTPAR  
01329 *        GENERATE BS BENEFIT REDUCTION TEXT                *      ELTPAR  
01330 *                                                          *      ELTPAR  
01331 ************************************************************      ELTPAR  
01332  GENERATE-BS-BENEFIT-REDUCTIONX.                                  ELTPAR  
01333      PERFORM GENERATE-REDUCTIONS-HEADINGS.                        ELTPAR  
01334      IF GSS-PR-BS-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTPAR  
01335          ZERO                                                     ELTPAR  
01336                             AND SPACES AND LOW-VALUES             ELTPAR  
01337          PERFORM TRANSLATE-BS-DEDU-APPLIC.                        ELTPAR  
01338      IF GSS-PR-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTPAR  
01339          ZERO                                                     ELTPAR  
01340                             AND SPACES AND LOW-VALUES             ELTPAR  
01341          PERFORM TRANSLATE-BS-OPEX-APPLIC.                        ELTPAR  
01342      PERFORM CREATE-A-BLANK-LINE.                                 ELTPAR  
01343      PERFORM LINK-TO-OUTPUT.                                      ELTPAR  
01344      IF GSS-PR-BS-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTPAR  
01345          ZERO                                                     ELTPAR  
01346                             AND SPACES AND LOW-VALUES             ELTPAR  
01347          PERFORM TRANSLATE-BS-OPEX-OVERRIDE.                      ELTPAR  
01348      EJECT                                                        ELTPAR  
01349                                                                   ELTPAR  
01350                                                                   ELTPAR  
01351 ************************************************************      ELTPAR  
01352 *                                                          *      ELTPAR  
01353 *        TRANSLATE MM INDICATOR                            *      ELTPAR  
01354 *                                                          *      ELTPAR  
01355 ************************************************************      ELTPAR  
01356  TRANSLATE-MM-INDICATOR.                                          ELTPAR  
01357      PERFORM GET-INDICATOR-FIXED-TEXT.                            ELTPAR  
01358      MOVE 'PR-MM-IND'  TO CMF-ELEMENT-SYSTEM-NAME.                ELTPAR  
01359      MOVE GSS-PR-MM-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTPAR  
01360      PERFORM LINK-TO-TRANSLATOR.                                  ELTPAR  
01361      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAR  
01362      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPAR  
01363      EJECT                                                        ELTPAR  
01364                                                                   ELTPAR  
01365                                                                   ELTPAR  
01366 ************************************************************      ELTPAR  
01367 *                                                          *      ELTPAR  
01368 *        TRANSLATE MM PAYMENT LEVEL INDICATOR              *      ELTPAR  
01369 *                                                          *      ELTPAR  
01370 ************************************************************      ELTPAR  
01371  TRANSLATE-MM-PAYMENT-LEVEL-IND.                                  ELTPAR  
01372      PERFORM SETUP-PAYMENT-LEVEL-FIXED-TEXT.                      ELTPAR  
01373      MOVE 'PR-MM-PAYMENT-LEVEL-IND' TO                            ELTPAR  
01374          CMF-ELEMENT-SYSTEM-NAME.                                 ELTPAR  
01375      MOVE GSS-PR-MM-PAYMENT-LEVEL-IND (GSS-INDEX)                 ELTPAR  
01376                                   TO CMF-CODE-VALUE.              ELTPAR  
01377      PERFORM LINK-TO-TRANSLATOR.                                  ELTPAR  
01378      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAR  
01379      EJECT                                                        ELTPAR  
01380                                                                   ELTPAR  
01381                                                                   ELTPAR  
01382 ************************************************************      ELTPAR  
01383 *                                                          *      ELTPAR  
01384 *        GENERATE MM BENEFITS REDUCTION SENTENCE           *      ELTPAR  
01385 *                                                          *      ELTPAR  
01386 ************************************************************      ELTPAR  
01387  GENERATE-MM-BENEFITS-REDUCTION.                                  ELTPAR  
01388      IF GSS-PR-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTPAR  
01389                  ZERO AND SPACES AND LOW-VALUES                   ELTPAR  
01390          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTPAR  
01391      IF (GSS-PR-MM-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL          ELTPAR  
01392          ZERO                                                     ELTPAR  
01393                             AND SPACES AND LOW-VALUES)  OR        ELTPAR  
01394                (GSS-PR-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL   ELTPAR  
01395          ZERO                                                     ELTPAR  
01396                             AND SPACES AND LOW-VALUES)  OR        ELTPAR  
01397                (GSS-PR-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT       ELTPAR  
01398          EQUAL ZERO                                               ELTPAR  
01399                             AND SPACES AND LOW-VALUES)            ELTPAR  
01400          PERFORM GENERATE-MM-BENEFIT-REDUCTIONX.                  ELTPAR  
01401      EJECT                                                        ELTPAR  
01402                                                                   ELTPAR  
01403                                                                   ELTPAR  
01404 ************************************************************      ELTPAR  
01405 *                                                          *      ELTPAR  
01406 *        GENERATE MM BENEFIT REDUCTION TEXT                *      ELTPAR  
01407 *                                                          *      ELTPAR  
01408 ************************************************************      ELTPAR  
01409  GENERATE-MM-BENEFIT-REDUCTIONX.                                  ELTPAR  
01410      PERFORM GENERATE-REDUCTIONS-HEADINGS.                        ELTPAR  
01411      IF GSS-PR-MM-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTPAR  
01412          ZERO                                                     ELTPAR  
01413                             AND SPACES AND LOW-VALUES             ELTPAR  
01414          PERFORM TRANSLATE-MM-DEDU-APPLIC.                        ELTPAR  
01415      IF GSS-PR-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTPAR  
01416          ZERO                                                     ELTPAR  
01417                             AND SPACES AND LOW-VALUES             ELTPAR  
01418          PERFORM TRANSLATE-MM-OPEX-APPLIC.                        ELTPAR  
01419      PERFORM CREATE-A-BLANK-LINE.                                 ELTPAR  
01420      PERFORM LINK-TO-OUTPUT.                                      ELTPAR  
01421      IF GSS-PR-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTPAR  
01422          ZERO                                                     ELTPAR  
01423                             AND SPACES AND LOW-VALUES             ELTPAR  
01424          PERFORM TRANSLATE-MM-OPEX-OVERRIDE.                      ELTPAR  
01425      EJECT                                                        ELTPAR  
01426                                                                   ELTPAR  
01427                                                                   ELTPAR  
01428 ************************************************************      ELTPAR  
01429 *                                                          *      ELTPAR  
01430 *        OBTAIN GPAD                                       *      ELTPAR  
01431 *                                                          *      ELTPAR  
01432 ************************************************************      ELTPAR  
01433  OBTAIN-GPAD.                                                     ELTPAR  
01434      SET GCG-INDEX TO 1.                                          ELTPAR  
01435      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTPAR  
01436            AT END                                                 ELTPAR  
01437               MOVE ZEROES TO WS-GPAD-DIAG-SLOT-NO                 ELTPAR  
01438            WHEN GCG-TAB-ID (GCG-INDEX) EQUAL PC-GPAD              ELTPAR  
01439               MOVE GCG-TAB-ID (GCG-INDEX) TO                      ELTPAR  
01440          WS-GPAD-DIAG-ID                                          ELTPAR  
01441               MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                    ELTPAR  
01442                   TO WS-GPAD-DIAG-SLOT-NO                         ELTPAR  
01443         END-SEARCH.                                               ELTPAR  
01444      IF WS-GPAD-DIAG-SLOT-NO NOT EQUAL ZEROES                     ELTPAR  
01445                  AND WS-GPAD-DIAG-ID EQUAL PC-GPAD                ELTPAR  
01446          PERFORM DISPLAY-RELATED-DIAGNOSES-SENT.                  ELTPAR  
01447      IF WS-GPAD-DIAG-SLOT-NO NOT EQUAL ZEROES                     ELTPAR  
01448                  AND WS-GPAD-DIAG-ID EQUAL PC-GPAD                ELTPAR  
01449          PERFORM GENERATE-RELATED-DIAGNOSES.                      ELTPAR  
01450      EJECT                                                        ELTPAR  
01451                                                                   ELTPAR  
01452                                                                   ELTPAR  
01453 ************************************************************      ELTPAR  
01454 *                                                          *      ELTPAR  
01455 *        OBTAIN GPAB                                       *      ELTPAR  
01456 *                                                          *      ELTPAR  
01457 ************************************************************      ELTPAR  
01458  OBTAIN-GPAB.                                                     ELTPAR  
01459      SET GCG-INDEX TO 1.                                          ELTPAR  
01460      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTPAR  
01461            AT END                                                 ELTPAR  
01462               MOVE ZEROES TO WS-GPAB-SRVS-SLOT-NO                 ELTPAR  
01463           WHEN GCG-TAB-ID (GCG-INDEX) EQUAL PC-GPAB               ELTPAR  
01464               MOVE GCG-TAB-ID (GCG-INDEX) TO                      ELTPAR  
01465          WS-GPAB-SRVS-ID                                          ELTPAR  
01466               MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                    ELTPAR  
01467                  TO WS-GPAB-SRVS-SLOT-NO                          ELTPAR  
01468         END-SEARCH.                                               ELTPAR  
01469      IF WS-GPAB-SRVS-SLOT-NO NOT EQUAL ZEROES                     ELTPAR  
01470                  AND WS-GPAB-SRVS-ID EQUAL PC-GPAB                ELTPAR  
01471          PERFORM DISPLAY-RELATED-SERVICES-SENTE.                  ELTPAR  
01472      IF WS-GPAB-SRVS-SLOT-NO NOT EQUAL ZEROES                     ELTPAR  
01473                  AND WS-GPAB-SRVS-ID EQUAL PC-GPAB                ELTPAR  
01474          PERFORM GENERATE-RELATED-SERVICES.                       ELTPAR  
01475      EJECT                                                        ELTPAR  
01476                                                                   ELTPAR  
01477                                                                   ELTPAR  
01478 ************************************************************      ELTPAR  
01479 *                                                          *      ELTPAR  
01480 *        OBTAIN GPAR                                       *      ELTPAR  
01481 *                                                          *      ELTPAR  
01482 ************************************************************      ELTPAR  
01483  OBTAIN-GPAR.                                                     ELTPAR  
01484      SET GCG-INDEX TO 1.                                          ELTPAR  
01485      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTPAR  
01486            AT END                                                 ELTPAR  
01487               MOVE ZEROES TO WS-GPAR-PROC-SLOT-NO                 ELTPAR  
01488            WHEN GCG-TAB-ID (GCG-INDEX) EQUAL PC-GPAR              ELTPAR  
01489               MOVE GCG-TAB-ID (GCG-INDEX) TO                      ELTPAR  
01490          WS-GPAR-PROC-ID                                          ELTPAR  
01491               MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                    ELTPAR  
01492                   TO WS-GPAR-PROC-SLOT-NO                         ELTPAR  
01493           END-SEARCH.                                             ELTPAR  
01494      IF WS-GPAR-PROC-SLOT-NO NOT EQUAL ZEROES                     ELTPAR  
01495                  AND WS-GPAR-PROC-ID EQUAL PC-GPAR                ELTPAR  
01496          PERFORM DISPLAY-RELATED-PROCEDURES-SEN.                  ELTPAR  
01497      IF WS-GPAR-PROC-SLOT-NO NOT EQUAL ZEROES                     ELTPAR  
01498                  AND WS-GPAR-PROC-ID EQUAL PC-GPAR                ELTPAR  
01499          PERFORM GENERATE-RELATED-PROCEDURES.                     ELTPAR  
01500      EJECT                                                        ELTPAR  
01501                                                                   ELTPAR  
01502                                                                   ELTPAR  
01503 ************************************************************      ELTPAR  
01504 *                                                          *      ELTPAR  
01505 *        OBTAIN GPAC                                       *      ELTPAR  
01506 *                                                          *      ELTPAR  
01507 ************************************************************      ELTPAR  
01508  OBTAIN-GPAC.                                                     ELTPAR  
01509      SET GCG-INDEX TO 1.                                          ELTPAR  
01510      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTPAR  
01511            AT END                                                 ELTPAR  
01512               MOVE ZEROES TO WS-GPAC-PROV-SLOT-NO                 ELTPAR  
01513            WHEN GCG-TAB-ID (GCG-INDEX) EQUAL PC-GPAC              ELTPAR  
01514               MOVE GCG-TAB-ID (GCG-INDEX) TO                      ELTPAR  
01515          WS-GPAC-PROV-ID                                          ELTPAR  
01516               MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                    ELTPAR  
01517                   TO WS-GPAC-PROV-SLOT-NO                         ELTPAR  
01518         END-SEARCH.                                               ELTPAR  
01519      IF WS-GPAC-PROV-SLOT-NO NOT EQUAL ZEROES                     ELTPAR  
01520                  AND WS-GPAC-PROV-ID EQUAL PC-GPAC                ELTPAR  
01521          PERFORM DISPLAY-RELATED-PROVIDERS-SENT.                  ELTPAR  
01522      IF WS-GPAC-PROV-SLOT-NO NOT EQUAL ZEROES                     ELTPAR  
01523                   AND WS-GPAC-PROV-ID EQUAL PC-GPAC               ELTPAR  
01524          PERFORM GENERATE-RELATED-PROVIDERS.                      ELTPAR  
01525                                                                   ELTPAR  
01526                                                                   ELTPAR  
01527 ************************************************************      ELTPAR  
01528 *                                                          *      ELTPAR  
01529 *        DISPLAY RELATED DIAGNOSES SENTENCE                *      ELTPAR  
01530 *                                                          *      ELTPAR  
01531 ************************************************************      ELTPAR  
01532  DISPLAY-RELATED-DIAGNOSES-SENT.                                  ELTPAR  
01533      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAR  
01534      MOVE RELATED-DIAGNOSES-MSG  TO COF-DTL-LINE                  ELTPAR  
01535          (COF-NBR-DTL-LINES).                                     ELTPAR  
01536      PERFORM LINK-TO-OUTPUT.                                      ELTPAR  
01537                                                                   ELTPAR  
01538                                                                   ELTPAR  
01539 ************************************************************      ELTPAR  
01540 *                                                          *      ELTPAR  
01541 *        DISPLAY RELATED SERVICES SENTENCE                 *      ELTPAR  
01542 *                                                          *      ELTPAR  
01543 ************************************************************      ELTPAR  
01544  DISPLAY-RELATED-SERVICES-SENTE.                                  ELTPAR  
01545      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAR  
01546      MOVE RELATED-SERVICES-MSG  TO COF-DTL-LINE                   ELTPAR  
01547          (COF-NBR-DTL-LINES).                                     ELTPAR  
01548      PERFORM LINK-TO-OUTPUT.                                      ELTPAR  
01549                                                                   ELTPAR  
01550                                                                   ELTPAR  
01551 ************************************************************      ELTPAR  
01552 *                                                          *      ELTPAR  
01553 *        DISPLAY RELATED PROCEDURES SENTENCE               *      ELTPAR  
01554 *                                                          *      ELTPAR  
01555 ************************************************************      ELTPAR  
01556  DISPLAY-RELATED-PROCEDURES-SEN.                                  ELTPAR  
01557      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAR  
01558      MOVE RELATED-PROCEDURES-MSG  TO COF-DTL-LINE                 ELTPAR  
01559          (COF-NBR-DTL-LINES).                                     ELTPAR  
01560      PERFORM LINK-TO-OUTPUT.                                      ELTPAR  
01561                                                                   ELTPAR  
01562                                                                   ELTPAR  
01563 ************************************************************      ELTPAR  
01564 *                                                          *      ELTPAR  
01565 *        DISPLAY RELATED PROVIDERS SENTENCE                *      ELTPAR  
01566 *                                                          *      ELTPAR  
01567 ************************************************************      ELTPAR  
01568  DISPLAY-RELATED-PROVIDERS-SENT.                                  ELTPAR  
01569      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAR  
01570      MOVE RELATED-PROVIDERS-MSG  TO COF-DTL-LINE                  ELTPAR  
01571          (COF-NBR-DTL-LINES).                                     ELTPAR  
01572      PERFORM LINK-TO-OUTPUT.                                      ELTPAR  
01573      EJECT                                                        ELTPAR  
01574                                                                   ELTPAR  
01575                                                                   ELTPAR  
01576 ************************************************************      ELTPAR  
01577 *                                                          *      ELTPAR  
01578 *        TRANSLATE BC DEDU APPLIC                          *      ELTPAR  
01579 *                                                          *      ELTPAR  
01580 ************************************************************      ELTPAR  
01581  TRANSLATE-BC-DEDU-APPLIC.                                        ELTPAR  
01582      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPAR  
01583      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAR  
01584      MOVE  'PR-BC-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.    ELTPAR  
01585      MOVE GSS-PR-BC-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTPAR  
01586          CMF-CODE-VALUE.                                          ELTPAR  
01587      PERFORM LINK-TO-TRANSLATOR.                                  ELTPAR  
01588      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAR  
01589      EJECT                                                        ELTPAR  
01590                                                                   ELTPAR  
01591                                                                   ELTPAR  
01592 ************************************************************      ELTPAR  
01593 *                                                          *      ELTPAR  
01594 *        TRANSLATE BC OPEX APPLIC                          *      ELTPAR  
01595 *                                                          *      ELTPAR  
01596 ************************************************************      ELTPAR  
01597  TRANSLATE-BC-OPEX-APPLIC.                                        ELTPAR  
01598      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPAR  
01599      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAR  
01600      MOVE 'PR-BC-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTPAR  
01601      MOVE GSS-PR-BC-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTPAR  
01602          CMF-CODE-VALUE.                                          ELTPAR  
01603      PERFORM LINK-TO-TRANSLATOR.                                  ELTPAR  
01604      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAR  
01605      EJECT                                                        ELTPAR  
01606                                                                   ELTPAR  
01607                                                                   ELTPAR  
01608 ************************************************************      ELTPAR  
01609 *                                                          *      ELTPAR  
01610 *        TRANSLATE BC OPEX OVERRIDE                        *      ELTPAR  
01611 *                                                          *      ELTPAR  
01612 ************************************************************      ELTPAR  
01613  TRANSLATE-BC-OPEX-OVERRIDE.                                      ELTPAR  
01614      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPAR  
01615      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAR  
01616      SET PERIOD-NEEDED TO TRUE.                                   ELTPAR  
01617      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAR  
01618      MOVE 'PR-BC-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTPAR  
01619      MOVE GSS-PR-BC-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTPAR  
01620          CMF-CODE-VALUE.                                          ELTPAR  
01621      PERFORM LINK-TO-TRANSLATOR.                                  ELTPAR  
01622      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAR  
01623      EJECT                                                        ELTPAR  
01624                                                                   ELTPAR  
01625                                                                   ELTPAR  
01626 ************************************************************      ELTPAR  
01627 *                                                          *      ELTPAR  
01628 *        TRANSLATE BS DEDU APPLIC                          *      ELTPAR  
01629 *                                                          *      ELTPAR  
01630 ************************************************************      ELTPAR  
01631  TRANSLATE-BS-DEDU-APPLIC.                                        ELTPAR  
01632      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPAR  
01633      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAR  
01634      MOVE 'PR-BS-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTPAR  
01635      MOVE GSS-PR-BS-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTPAR  
01636          CMF-CODE-VALUE.                                          ELTPAR  
01637      PERFORM LINK-TO-TRANSLATOR.                                  ELTPAR  
01638      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAR  
01639      EJECT                                                        ELTPAR  
01640                                                                   ELTPAR  
01641                                                                   ELTPAR  
01642 ************************************************************      ELTPAR  
01643 *                                                          *      ELTPAR  
01644 *        TRANSLATE BS OPEX APPLIC                          *      ELTPAR  
01645 *                                                          *      ELTPAR  
01646 ************************************************************      ELTPAR  
01647  TRANSLATE-BS-OPEX-APPLIC.                                        ELTPAR  
01648      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPAR  
01649      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAR  
01650      MOVE 'PR-BS-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTPAR  
01651      MOVE GSS-PR-BS-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTPAR  
01652          CMF-CODE-VALUE.                                          ELTPAR  
01653      PERFORM LINK-TO-TRANSLATOR.                                  ELTPAR  
01654      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAR  
01655      EJECT                                                        ELTPAR  
01656                                                                   ELTPAR  
01657                                                                   ELTPAR  
01658 ************************************************************      ELTPAR  
01659 *                                                          *      ELTPAR  
01660 *        TRANSLATE BS OPEX OVERRIDE                        *      ELTPAR  
01661 *                                                          *      ELTPAR  
01662 ************************************************************      ELTPAR  
01663  TRANSLATE-BS-OPEX-OVERRIDE.                                      ELTPAR  
01664      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPAR  
01665      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAR  
01666      SET PERIOD-NEEDED TO TRUE.                                   ELTPAR  
01667      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAR  
01668      MOVE 'PR-BS-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTPAR  
01669      MOVE GSS-PR-BS-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTPAR  
01670          CMF-CODE-VALUE.                                          ELTPAR  
01671      PERFORM LINK-TO-TRANSLATOR.                                  ELTPAR  
01672      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAR  
01673      EJECT                                                        ELTPAR  
01674                                                                   ELTPAR  
01675                                                                   ELTPAR  
01676 ************************************************************      ELTPAR  
01677 *                                                          *      ELTPAR  
01678 *        TRANSLATE MM DEDU APPLIC                          *      ELTPAR  
01679 *                                                          *      ELTPAR  
01680 ************************************************************      ELTPAR  
01681  TRANSLATE-MM-DEDU-APPLIC.                                        ELTPAR  
01682      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPAR  
01683      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAR  
01684      MOVE 'PR-MM-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTPAR  
01685      MOVE GSS-PR-MM-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTPAR  
01686          CMF-CODE-VALUE.                                          ELTPAR  
01687      PERFORM LINK-TO-TRANSLATOR.                                  ELTPAR  
01688      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAR  
01689      EJECT                                                        ELTPAR  
01690                                                                   ELTPAR  
01691                                                                   ELTPAR  
01692 ************************************************************      ELTPAR  
01693 *                                                          *      ELTPAR  
01694 *        TRANSLATE MM OPEX APPLIC                          *      ELTPAR  
01695 *                                                          *      ELTPAR  
01696 ************************************************************      ELTPAR  
01697  TRANSLATE-MM-OPEX-APPLIC.                                        ELTPAR  
01698      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPAR  
01699      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAR  
01700      MOVE 'PR-MM-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTPAR  
01701      MOVE GSS-PR-MM-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTPAR  
01702          CMF-CODE-VALUE.                                          ELTPAR  
01703      PERFORM LINK-TO-TRANSLATOR.                                  ELTPAR  
01704      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAR  
01705      EJECT                                                        ELTPAR  
01706                                                                   ELTPAR  
01707                                                                   ELTPAR  
01708 ************************************************************      ELTPAR  
01709 *                                                          *      ELTPAR  
01710 *        TRANSLATE MM OPEX OVERRIDE                        *      ELTPAR  
01711 *                                                          *      ELTPAR  
01712 ************************************************************      ELTPAR  
01713  TRANSLATE-MM-OPEX-OVERRIDE.                                      ELTPAR  
01714      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPAR  
01715      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAR  
01716      SET PERIOD-NEEDED TO TRUE.                                   ELTPAR  
01717      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAR  
01718      MOVE 'PR-MM-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTPAR  
01719      MOVE GSS-PR-MM-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTPAR  
01720          CMF-CODE-VALUE.                                          ELTPAR  
01721      PERFORM LINK-TO-TRANSLATOR.                                  ELTPAR  
01722      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAR  
01723      EJECT                                                        ELTPAR  
01724                                                                   ELTPAR  
01725                                                                   ELTPAR  
01726 ************************************************************      ELTPAR  
01727 *                                                          *      ELTPAR  
01728 *        GENERATE SPILL OVER INDICATOR                     *      ELTPAR  
01729 *                                                          *      ELTPAR  
01730 ************************************************************      ELTPAR  
01731  GENERATE-SPILL-OVER-INDICATOR.                                   ELTPAR  
01732      INITIALIZE ADDITIONAL-TEXT-SW.                               ELTPAR  
01733      MOVE SPACES TO TCAR-FROM-AREA.                               ELTPAR  
01734      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAR  
01735      SET PERIOD-NEEDED TO TRUE.                                   ELTPAR  
01736      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAR  
01737      MOVE WS-SPILLOVER-SENTENCE TO TCAR-FROM-LINE                 ELTPAR  
01738          (TCAR-FROM-SUB).                                         ELTPAR  
01739      ADD 1 TO TCAR-FROM-SUB.                                      ELTPAR  
01740      MOVE 'PR-SPILL-OVER-IND' TO CMF-ELEMENT-SYSTEM-NAME.         ELTPAR  
01741      MOVE GSS-PR-SPILL-OVER-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTPAR  
01742      PERFORM LINK-TO-TRANSLATOR.                                  ELTPAR  
01743      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAR  
01744                                                                   ELTPAR  
01745                                                                   ELTPAR  
01746 ************************************************************      ELTPAR  
01747 *                                                          *      ELTPAR  
01748 *        SIGNAL NOT APPLICABLE MSG                         *      ELTPAR  
01749 *                                                          *      ELTPAR  
01750 ************************************************************      ELTPAR  
01751  SIGNAL-NOT-APPLICABLE-MSG.                                       ELTPAR  
01752      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPAR  
01753      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTPAR  
01754          (COF-NBR-DTL-LINES).                                     ELTPAR  
01755                                                                   ELTPAR  
01756                                                                   ELTPAR  
01757 ************************************************************      ELTPAR  
01758 *                                                          *      ELTPAR  
01759 *        SIGNAL VOLUNTARY MSG                              *      ELTPAR  
01760 *                                                          *      ELTPAR  
01761 ************************************************************      ELTPAR  
01762  SIGNAL-VOLUNTARY-MSG.                                            ELTPAR  
01763      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPAR  
01764      MOVE WS-VOLUNTARY-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).   ELTPAR  
01765                                                                   ELTPAR  
01766                                                                   ELTPAR  
01767 ************************************************************      ELTPAR  
01768 *                                                          *      ELTPAR  
01769 *        GROUP LINK TO TRANSLATOR                          *      ELTPAR  
01770 *                                                          *      ELTPAR  
01771 ************************************************************      ELTPAR  
01772  GROUP-LINK-TO-TRANSLATOR.                                        ELTPAR  
01773      MOVE PC-GROUP TO CMF-RECORD-PREFIX.                          ELTPAR  
01774      EXEC CICS LINK                                               ELTPAR  
01775           PROGRAM('ELUCMIF')                                      ELTPAR  
01776           COMMAREA(DFHCOMMAREA)                                   ELTPAR  
01777           END-EXEC.                                               ELTPAR  
01778                                                                   ELTPAR  
01779                                                                   ELTPAR  
01780 ************************************************************      ELTPAR  
01781 *                                                          *      ELTPAR  
01782 *        LINK TO TRANSLATOR                                *      ELTPAR  
01783 *                                                          *      ELTPAR  
01784 ************************************************************      ELTPAR  
01785  LINK-TO-TRANSLATOR.                                              ELTPAR  
01786      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTPAR  
01787      EXEC CICS LINK                                               ELTPAR  
01788           PROGRAM('ELUCMIF')                                      ELTPAR  
01789           COMMAREA(DFHCOMMAREA)                                   ELTPAR  
01790           END-EXEC.                                               ELTPAR  
01791      EJECT                                                        ELTPAR  
01792                                                                   ELTPAR  
01793                                                                   ELTPAR  
01794 ************************************************************      ELTPAR  
01795 *                                                          *      ELTPAR  
01796 *        READ GCCP RECORD                                  *      ELTPAR  
01797 *                                                          *      ELTPAR  
01798 ************************************************************      ELTPAR  
01799  READ-GCCP-RECORD.                                                ELTPAR  
01800      SET CIA-GCTABULR-DDN TO TRUE.                                ELTPAR  
01801      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAR  
01802                            ADDRESS OF                             ELTPAR  
01803          IOP-INPUT-OUTPUT-PARAMETERS.                             ELTPAR  
01804      SET IOP-RD TO TRUE.                                          ELTPAR  
01805      SET IOP-FCQ-NONE TO TRUE.                                    ELTPAR  
01806      SET IOP-KVQ-EQ TO TRUE.                                      ELTPAR  
01807      SET IOP-STG-MODE-MOVE TO TRUE.                               ELTPAR  
01808      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELTPAR  
01809      PERFORM LINK-TO-I-O-PGM.                                     ELTPAR  
01810      EJECT                                                        ELTPAR  
01811                                                                   ELTPAR  
01812                                                                   ELTPAR  
01813 ************************************************************      ELTPAR  
01814 *                                                          *      ELTPAR  
01815 *        LINK TO I O PGM                                   *      ELTPAR  
01816 *                                                          *      ELTPAR  
01817 ************************************************************      ELTPAR  
01818  LINK-TO-I-O-PGM.                                                 ELTPAR  
01819      EXEC CICS LINK PROGRAM ('ELUIOPGM')                          ELTPAR  
01820           COMMAREA (DFHCOMMAREA)                                  ELTPAR  
01821           END-EXEC.                                               ELTPAR  
01822      IF IOP-RC-OK                                                 ELTPAR  
01823          PERFORM ESTABLISH-ADDRESSABILITY-OF-GC                   ELTPAR  
01824      ELSE IF IOP-RC-NOTFND                                        ELTPAR  
01825          PERFORM SIGNAL-NOT-FOUND-GCTAB                           ELTPAR  
01826      ELSE                                                         ELTPAR  
01827          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTPAR  
01828                                                                   ELTPAR  
01829                                                                   ELTPAR  
01830 ************************************************************      ELTPAR  
01831 *                                                          *      ELTPAR  
01832 *        SIGNAL CRITICAL IO ERROR                          *      ELTPAR  
01833 *                                                          *      ELTPAR  
01834 ************************************************************      ELTPAR  
01835  SIGNAL-CRITICAL-IO-ERROR.                                        ELTPAR  
01836      SET CIA-AB-CRITIO TO TRUE.                                   ELTPAR  
01837      PERFORM SIGNAL-ABEND.                                        ELTPAR  
01838                                                                   ELTPAR  
01839                                                                   ELTPAR  
01840 ************************************************************      ELTPAR  
01841 *                                                          *      ELTPAR  
01842 *        SIGNAL NOT FOUND GCTAB                            *      ELTPAR  
01843 *                                                          *      ELTPAR  
01844 ************************************************************      ELTPAR  
01845  SIGNAL-NOT-FOUND-GCTAB.                                          ELTPAR  
01846      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTPAR  
01847      PERFORM SIGNAL-ABEND.                                        ELTPAR  
01848                                                                   ELTPAR  
01849                                                                   ELTPAR  
01850 ************************************************************      ELTPAR  
01851 *                                                          *      ELTPAR  
01852 *        ESTABLISH ADDRESSABILITY OF GCCP RECORD           *      ELTPAR  
01853 *                                                          *      ELTPAR  
01854 ************************************************************      ELTPAR  
01855  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTPAR  
01856      SET ADDRESS OF GCCP-TABULAR-REC TO IOP-REC-PTR.              ELTPAR  
01857      SET IOP-REC-PTR TO NULL.                                     ELTPAR  
01858                                                                   ELTPAR  
01859                                                                   ELTPAR  
01860 ************************************************************      ELTPAR  
01861 *                                                          *      ELTPAR  
01862 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTPAR  
01863 *                                                          *      ELTPAR  
01864 ************************************************************      ELTPAR  
01865  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTPAR  
01866      PERFORM INITIALIZE-CMOUT.                                    ELTPAR  
01867      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTPAR  
01868      EJECT                                                        ELTPAR  
01869                                                                   ELTPAR  
01870                                                                   ELTPAR  
01871 ************************************************************      ELTPAR  
01872 *                                                          *      ELTPAR  
01873 *        PREPARE TEXT FOR OUTPUT                           *      ELTPAR  
01874 *                                                          *      ELTPAR  
01875 ************************************************************      ELTPAR  
01876  PREPARE-TEXT-FOR-OUTPUT.                                         ELTPAR  
01877      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTPAR  
01878          UNTIL CMF-DESCR-IDX                                      ELTPAR  
01879                                    GREATER THAN                   ELTPAR  
01880              CMF-NBR-DESCR-LINES.                                 ELTPAR  
01881      EJECT                                                        ELTPAR  
01882                                                                   ELTPAR  
01883                                                                   ELTPAR  
01884 ************************************************************      ELTPAR  
01885 *                                                          *      ELTPAR  
01886 *        INITIALIZE CMOUT                                  *      ELTPAR  
01887 *                                                          *      ELTPAR  
01888 ************************************************************      ELTPAR  
01889  INITIALIZE-CMOUT.                                                ELTPAR  
01890      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTPAR  
01891      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAR  
01892          ADDRESS OF CMF-DESCR.                                    ELTPAR  
01893      SET CMF-DESCR-IDX TO 1.                                      ELTPAR  
01894      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTPAR  
01895                                                                   ELTPAR  
01896                                                                   ELTPAR  
01897 ************************************************************      ELTPAR  
01898 *                                                          *      ELTPAR  
01899 *        MOVE CMF TEXT TO OUTPUT                           *      ELTPAR  
01900 *                                                          *      ELTPAR  
01901 ************************************************************      ELTPAR  
01902  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTPAR  
01903      PERFORM MOVE-A-LINE.                                         ELTPAR  
01904      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTPAR  
01905          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTPAR  
01906      IF TCAR-FROM-SUB GREATER THAN 20                             ELTPAR  
01907               OR CMF-DESCR-IDX GREATER THAN                       ELTPAR  
01908          CMF-NBR-DESCR-LINES                                      ELTPAR  
01909          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTPAR  
01910                                                                   ELTPAR  
01911                                                                   ELTPAR  
01912 ************************************************************      ELTPAR  
01913 *                                                          *      ELTPAR  
01914 *        FINISH CODES MANUAL TEXT                          *      ELTPAR  
01915 *                                                          *      ELTPAR  
01916 ************************************************************      ELTPAR  
01917  FINISH-CODES-MANUAL-TEXT.                                        ELTPAR  
01918      SET DONE-PROCESSING TO TRUE.                                 ELTPAR  
01919      IF PERIOD-NEEDED                                             ELTPAR  
01920          PERFORM GET-AND-MOVE-PERIOD.                             ELTPAR  
01921                                                                   ELTPAR  
01922                                                                   ELTPAR  
01923 ************************************************************      ELTPAR  
01924 *                                                          *      ELTPAR  
01925 *        GET AND MOVE PERIOD                               *      ELTPAR  
01926 *                                                          *      ELTPAR  
01927 ************************************************************      ELTPAR  
01928  GET-AND-MOVE-PERIOD.                                             ELTPAR  
01929      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTPAR  
01930          (TCAR-FROM-SUB).                                         ELTPAR  
01931                                                                   ELTPAR  
01932                                                                   ELTPAR  
01933 ************************************************************      ELTPAR  
01934 *                                                          *      ELTPAR  
01935 *        SAVE LAST LINE                                    *      ELTPAR  
01936 *                                                          *      ELTPAR  
01937 ************************************************************      ELTPAR  
01938  SAVE-LAST-LINE.                                                  ELTPAR  
01939      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAR  
01940      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTPAR  
01941         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTPAR  
01942      ADD 1 TO TCAR-FROM-SUB.                                      ELTPAR  
01943      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTPAR  
01944                                                                   ELTPAR  
01945                                                                   ELTPAR  
01946 ************************************************************      ELTPAR  
01947 *                                                          *      ELTPAR  
01948 *        OUTPUT LAST LINE                                  *      ELTPAR  
01949 *                                                          *      ELTPAR  
01950 ************************************************************      ELTPAR  
01951  OUTPUT-LAST-LINE.                                                ELTPAR  
01952      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTPAR  
01953          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTPAR  
01954      IF BLANK-LINE-NEEDED                                         ELTPAR  
01955          PERFORM CREATE-A-BLANK-LINE.                             ELTPAR  
01956                                                                   ELTPAR  
01957                                                                   ELTPAR  
01958 ************************************************************      ELTPAR  
01959 *                                                          *      ELTPAR  
01960 *        CREATE A BLANK LINE                               *      ELTPAR  
01961 *                                                          *      ELTPAR  
01962 ************************************************************      ELTPAR  
01963  CREATE-A-BLANK-LINE.                                             ELTPAR  
01964      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTPAR  
01965      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPAR  
01966                                                                   ELTPAR  
01967                                                                   ELTPAR  
01968 ************************************************************      ELTPAR  
01969 *                                                          *      ELTPAR  
01970 *        MOVE A LINE                                       *      ELTPAR  
01971 *                                                          *      ELTPAR  
01972 ************************************************************      ELTPAR  
01973  MOVE-A-LINE.                                                     ELTPAR  
01974      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTPAR  
01975          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTPAR  
01976      SET CMF-DESCR-IDX UP BY 1.                                   ELTPAR  
01977      ADD 1 TO TCAR-FROM-SUB.                                      ELTPAR  
01978      EJECT                                                        ELTPAR  
01979                                                                   ELTPAR  
01980                                                                   ELTPAR  
01981 ************************************************************      ELTPAR  
01982 *                                                          *      ELTPAR  
01983 *        REFORMAT AND WRITE TEXT                           *      ELTPAR  
01984 *                                                          *      ELTPAR  
01985 ************************************************************      ELTPAR  
01986  REFORMAT-AND-WRITE-TEXT.                                         ELTPAR  
01987      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTPAR  
01988      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTPAR  
01989      PERFORM UNSTRING-TEXT.                                       ELTPAR  
01990      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAR  
01991      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAR  
01992      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTPAR  
01993          UNTIL COF-NBR-DTL-LINES GREATER                          ELTPAR  
01994                                   TCAR-OUTPUT-FIELDS-USED -       ELTPAR  
01995              1.                                                   ELTPAR  
01996      PERFORM DISPOSE-OF-LAST-LINE.                                ELTPAR  
01997      PERFORM LINK-TO-OUTPUT.                                      ELTPAR  
01998                                                                   ELTPAR  
01999                                                                   ELTPAR  
02000 ************************************************************      ELTPAR  
02001 *                                                          *      ELTPAR  
02002 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTPAR  
02003 *                                                          *      ELTPAR  
02004 ************************************************************      ELTPAR  
02005  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTPAR  
02006      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTPAR  
02007           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTPAR  
02008      ADD +1 TO TCAR-FROM-SUB.                                     ELTPAR  
02009      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPAR  
02010      EJECT                                                        ELTPAR  
02011                                                                   ELTPAR  
02012                                                                   ELTPAR  
02013 ************************************************************      ELTPAR  
02014 *                                                          *      ELTPAR  
02015 *        UNSTRING TEXT                                     *      ELTPAR  
02016 *                                                          *      ELTPAR  
02017 ************************************************************      ELTPAR  
02018  UNSTRING-TEXT.                                                   ELTPAR  
02019      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTPAR  
02020      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTPAR  
02021      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTPAR  
02022      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTPAR  
02023      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTPAR  
02024      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTPAR  
02025      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTPAR  
02026      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTPAR  
02027      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTPAR  
02028      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTPAR  
02029      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTPAR  
02030      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTPAR  
02031      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTPAR  
02032      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTPAR  
02033      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTPAR  
02034      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTPAR  
02035      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTPAR  
02036      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTPAR  
02037      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTPAR  
02038      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTPAR  
02039      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTPAR  
02040      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTPAR  
02041      EJECT                                                        ELTPAR  
02042                                                                   ELTPAR  
02043                                                                   ELTPAR  
02044 ************************************************************      ELTPAR  
02045 *                                                          *      ELTPAR  
02046 *        LINK TO OUTPUT                                    *      ELTPAR  
02047 *                                                          *      ELTPAR  
02048 ************************************************************      ELTPAR  
02049  LINK-TO-OUTPUT.                                                  ELTPAR  
02050      EXEC CICS LINK                                               ELTPAR  
02051          PROGRAM ('ELUOUTPT')                                     ELTPAR  
02052          COMMAREA (DFHCOMMAREA)                                   ELTPAR  
02053          END-EXEC.                                                ELTPAR  
02054      EJECT                                                        ELTPAR  
02055                                                                   ELTPAR  
02056                                                                   ELTPAR  
02057 ************************************************************      ELTPAR  
02058 *                                                          *      ELTPAR  
02059 *        DISPOSE OF LAST LINE                              *      ELTPAR  
02060 *                                                          *      ELTPAR  
02061 ************************************************************      ELTPAR  
02062  DISPOSE-OF-LAST-LINE.                                            ELTPAR  
02063      IF NOT ADDITIONAL-TEXT                                       ELTPAR  
02064          PERFORM INITIALIZE-CONTINUED-SW.                         ELTPAR  
02065      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTPAR  
02066          PERFORM SAVE-LAST-LINE                                   ELTPAR  
02067      ELSE                                                         ELTPAR  
02068          PERFORM OUTPUT-LAST-LINE.                                ELTPAR  
02069                                                                   ELTPAR  
02070                                                                   ELTPAR  
02071 ************************************************************      ELTPAR  
02072 *                                                          *      ELTPAR  
02073 *        INITIALIZE CONTINUED SW                           *      ELTPAR  
02074 *                                                          *      ELTPAR  
02075 ************************************************************      ELTPAR  
02076  INITIALIZE-CONTINUED-SW.                                         ELTPAR  
02077      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTPAR  
