00001 *      LAST MAINTENANCE TIME:  7.53.56  DATE: 06/13/91            06/29/02
00002  IDENTIFICATION DIVISION.                                         ELTREIMB
00003                                                                      LV001
00004  PROGRAM-ID.         ELTREIMB.                                    ELTREIMB
00005                                                                   ELTREIMB
00006  AUTHOR.             ANNE KEFFER KING.                            ELTREIMB
00007                                                                   ELTREIMB
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTREIMB
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELTREIMB
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTREIMB
00011                      233 N. MICHIGAN AVE                          ELTREIMB
00012                      CHICAGO, ILLINOIS 60601                      ELTREIMB
00013                                                                   ELTREIMB
00014  DATE-WRITTEN.       20-JUN-1987.                                 ELTREIMB
00015                      REWRITTEN 12-JUL-95.                         ELTREIMB
00016  DATE-COMPILED.                                                   ELTREIMB
00017                                                                   ELTREIMB
00018  SECURITY.           COPYRIGHT 1986,                              ELTREIMB
00019                      HEALTH CARE SERVICE CORPORATION              ELTREIMB
00020      SKIP3                                                        ELTREIMB
00021  ENVIRONMENT DIVISION.                                            ELTREIMB
00022                                                                   ELTREIMB
00023  CONFIGURATION SECTION.                                           ELTREIMB
00024  SOURCE-COMPUTER.    IBM-3090.                                    ELTREIMB
00025  OBJECT-COMPUTER.    IBM-3090.                                    ELTREIMB
00026      EJECT                                                        ELTREIMB
00027 ******************************************************************ELTREIMB
00028 *    OVERVIEW:                                                   *ELTREIMB
00029 *                                                                *ELTREIMB
00030 *  RECORDS                                                       *ELTREIMB
00031 *  ACCESSED: ELCDCIA RECORD                                      *ELTREIMB
00032 *             GROUP SPECIFIC RECORD                              *ELTREIMB
00033 *             #GCCP TABULAR RECORD                               *ELTREIMB
00034 *  PROCESSING                                                    *ELTREIMB
00035 *  FUNCTIONS: THIS MODULE PERFORMS THE FOLLOWING FUNCTIONS:      *ELTREIMB
00036 *                                                                *ELTREIMB
00037 *              1. INITIALIZES WORK DATA ELEMENTS.                *ELTREIMB
00038 *              2. ACQUIRE NECESSARY RECORDS FOR PROCESSING.      *ELTREIMB
00039 *              3. PROCESS REIMBURSEMENT SUBROGATION PROGRAM      *ELTREIMB
00040 *                 TABULAR RECORD FOR BLUE CROSS PRODUCING OUTPUT *ELTREIMB
00041 *                 TEXT AS REQUIRED.                              *ELTREIMB
00042 *              4. PROCESS REIMBURSEMENT SUBROGATION PROGRAM      *ELTREIMB
00043 *                 TABULAR RECORD FOR BLUE SHIELD PRODUCING OUTPUT*ELTREIMB
00044 *                 TEXT AS REQUIRED.                              *ELTREIMB
00045 ******************************************************************ELTREIMB
00046 *                    MAINTENANCE HISTORY                         *ELTREIMB
00047 *                                                                *ELTREIMB
00048 *  MOD     DATE     BY  DPRT            ACTION                   *ELTREIMB
00049 * ----- ----------- --- ----- ---------------------------------- *ELTREIMB
00050 * 01.00 20-JUN-1987 AKK       CREATED - ORIGIANL VERSION.        *ELTREIMB
00051 *                                                                *ELTREIMB
00052 * 02.00 12-JUL-1995 AKK       REWRITTEN DUE TO CHANGES IN        *ELTREIMB
00053 *                             ADMINSTRATION OF R/S.               ELTREIMB
00054 *                                                                *ELTREIMB
00055 ******************************************************************ELTREIMB
00056 /                                                                 ELTREIMB
00057  DATA DIVISION.                                                   ELTREIMB
00058                                                                   ELTREIMB
00059  WORKING-STORAGE SECTION.                                         ELTREIMB
00060  01  WS-BEGIN                    PIC X(26)  VALUE                 ELTREIMB
00061                                 '*** ELTREIMB WS BEGINS ***'.     ELTREIMB
00062 *                                                                 ELTREIMB
00063  01  WS-MISC-FIELDS.                                              ELTREIMB
00064      05  SCREEN-TYPE             PIC X      VALUE SPACE.          ELTREIMB
00065          88  INSTITUTIONAL-SCREEN           VALUE 'I'.            ELTREIMB
00066          88  PROFESSIONAL-SCREEN            VALUE 'P'.            ELTREIMB
00067 *                                                                 ELTREIMB
00068      05  FULL-PART-SW            PIC X      VALUE SPACE.          ELTREIMB
00069          88  FULL-SENTENCE                  VALUE 'F'.            ELTREIMB
00070          88  PART-SENTENCE                  VALUE 'P'.            ELTREIMB
00071 *                                                                 ELTREIMB
00072     05   WS-GRID-DIAG-ID         PIC  X(06) VALUE SPACES.         ELTREIMB
00073     05   WS-GRID-DIAG-SLOT-NO    PIC S9(04) COMP-3                ELTREIMB
00074                                             VALUE ZEROES.         ELTREIMB
00075  01  WS-SWITCHES.                                                 ELTREIMB
00076      05  UNDEFINED-TABULAR-SW    PIC X      VALUE 'N'.            ELTREIMB
00077          88  TABULAR-IS-UNDEFINED           VALUE 'Y'.            ELTREIMB
00078 *                                                                 ELTREIMB
00079      05  DEFINED-TABULAR-SW      PIC X      VALUE 'N'.            ELTREIMB
00080          88  TABULAR-IS-DEFINED             VALUE 'Y'.            ELTREIMB
00081 *                                                                 ELTREIMB
00082      05  ADDITIONAL-TEXT-SW      PIC X      VALUE SPACE.          ELTREIMB
00083          88  BLANK-LINE-NEEDED              VALUE 'B'.            ELTREIMB
00084          88  ADDITIONAL-TEXT                VALUE 'Y'.            ELTREIMB
00085 *                                                                 ELTREIMB
00086      05  CONTINUED-PROCESSING-SW PIC X      VALUE SPACE.          ELTREIMB
00087          88  PROCESSING-CMF-TEXT            VALUE 'P'.            ELTREIMB
00088          88  DONE-PROCESSING                VALUE 'D'.            ELTREIMB
00089 *                                                                 ELTREIMB
00090      05  WS-PERIOD-NEEDED-SW     PIC X      VALUE 'N'.            ELTREIMB
00091          88  PERIOD-NEEDED                  VALUE 'Y'.            ELTREIMB
00092 *                                                                 ELTREIMB
00093  01  PROGRAM-CONSTANTS.                                           ELTREIMB
00094      05  PC-GRP                  PIC X(06)  VALUE 'GROUP'.        ELTREIMB
00095      05  PC-GCCP                 PIC X(06)  VALUE '#GCCP '.       ELTREIMB
00096      05  PC-GRID                 PIC X(06)  VALUE '#GRID '.       ELTREIMB
00097 ******************************************************************ELTREIMB
00098 *SCREEN BODY LINES                                                ELTREIMB
00099 ******************************************************************ELTREIMB
00100  01  WS-HDR-LN2.                                                  ELTREIMB
00101      05  FILLER                  PIC X(22)   VALUE SPACES.        ELTREIMB
00102      05  FILLER                  PIC X(34)   VALUE                ELTREIMB
00103          'REIMBURSEMENT SUBROGATION PROGRAM '.                    ELTREIMB
00104      05  WS-HDR-TITLE            PIC X(13)   VALUE SPACES.        ELTREIMB
00105      05  FILLER                  PIC X(10)   VALUE SPACES.        ELTREIMB
00106 *                                                                 ELTREIMB
00107 *01  WS-INVESTIGATION.                                            ELTREIMB
00108 *    05  FILLER                 PIC X(79)    VALUE                ELTREIMB
00109 *        'CLAIMS ARE TO BE INVESTIGATED FOR '.                    ELTREIMB
00110 *                                                                 ELTREIMB
00111  01  WS-DENIAL-PHRASE.                                            ELTREIMB
00112      05  FILLER                 PIC X(79)    VALUE                ELTREIMB
00113      'WHEN NO RESPONSE IS RECEIVED TO REIMBURSEMENT/SUBROGATION'. ELTREIMB
00114 *                                                                 ELTREIMB
00115  01  WS-DOC-FREQUENCY.                                            ELTREIMB
00116      05  FILLER                 PIC X(79)    VALUE                ELTREIMB
00117      'REIMBURSEMENT/SUBROGATION QUESTIONNAIRES WILL BE SENT OUT USELTREIMB
00118 -    'ING '.                                                      ELTREIMB
00119 *                                                                 ELTREIMB
00120  01  WS-DENIAL-PHRASE2.                                           ELTREIMB
00121      05  FILLER                 PIC X(79)    VALUE                ELTREIMB
00122      'QUESTIONNAIRES'.                                            ELTREIMB
00123 *                                                                 ELTREIMB
00124  01  WS-INV-PERFORMED-AS.                                         ELTREIMB
00125      05  FILLER                 PIC X(79)    VALUE                ELTREIMB
00126          'INVESTIGATION IS PERFORMED AS '.                        ELTREIMB
00127 *                                                                 ELTREIMB
00128  01  WS-RESPONSIBILITY-IND.                                       ELTREIMB
00129      05  FILLER                 PIC X(79)    VALUE                ELTREIMB
00130          'REIMBURSEMENT/SUBROGATION IS '.                         ELTREIMB
00131 *                                                                 ELTREIMB
00132  01  WS-QUALITY-TEXT.                                             ELTREIMB
00133      05  FILLER                 PIC X(79)    VALUE                ELTREIMB
00134          'INITIAL INVESTIGATION OF CLAIMS '.                      ELTREIMB
00135 *                                                                 ELTREIMB
00136  01  WS-SUB-TEXT.                                                 ELTREIMB
00137      05  FILLER                 PIC X(79)    VALUE                ELTREIMB
00138          'SUBSEQUENT INVESTIGATION '.                             ELTREIMB
00139 *                                                                 ELTREIMB
00140  01  WS-MINIMUM-TEXT.                                             ELTREIMB
00141      05  FILLER                 PIC X(79)    VALUE                ELTREIMB
00142          'WITH A MINIMUM OF '.                                    ELTREIMB
00143 *                                                                 ELTREIMB
00144  01  WS-IS-TEXT.                                                  ELTREIMB
00145      05  FILLER                 PIC X(03)    VALUE                ELTREIMB
00146          'IS '.                                                   ELTREIMB
00147 *                                                                 ELTREIMB
00148  01  WS-FOR.                                                      ELTREIMB
00149      05  FILLER                 PIC X(79)   VALUE 'FOR '.         ELTREIMB
00150 *                                                                 ELTREIMB
00151  01  WS-IS.                                                       ELTREIMB
00152      05  FILLER                 PIC X(79)   VALUE 'IS '.          ELTREIMB
00153 *                                                                 ELTREIMB
00154  01  WS-DOLLAR-AMT              PIC $$$$$.99 VALUE ZERO.          ELTREIMB
00155 *                                                                 ELTREIMB
00156  01  WS-NOT-APPLICABLE-LOB-BC.                                    ELTREIMB
00157      05  FILLER                          PIC X(79)  VALUE         ELTREIMB
00158          'REIMBURSEMENT SUBROGATION DOES NOT APPLY TO INSTITUTIONAELTREIMB
00159 -        'L BENEFITS.'.                                           ELTREIMB
00160 *                                                                 ELTREIMB
00161  01  WS-NOT-APPLICABLE-LOB-BS.                                    ELTREIMB
00162      05  FILLER                          PIC X(79)  VALUE         ELTREIMB
00163          'REIMBURSEMENT SUBROGATION DOES NOT APPLY TO PROFESSIONALELTREIMB
00164 -        ' BENEFITS.'.                                            ELTREIMB
00165 *                                                                 ELTREIMB
00166  01  WS-RELATED-DIAGNOSES-MSG.                                    ELTREIMB
00167      05  FILLER                          PIC X(79)  VALUE         ELTREIMB
00168          'THERE ARE SPECIAL RELATED DIAGNOSES INCLUDED IN THIS COSELTREIMB
00169 -        'T CONTAINMENT PROGRAM.'.                                ELTREIMB
00170 *                                                                 ELTREIMB
00171 *01  WS-REIMB-APPLIES.                                            ELTREIMB
00172 *    05  FILLER                          PIC  X(79) VALUE         ELTREIMB
00173 *        'REIMBURSEMENT SUBROGATION PROGRAM APPLIES TO'.          ELTREIMB
00174 *                                                                 ELTREIMB
00175  01  WS-NOT-APPLICABLE-MSG.                                       ELTREIMB
00176      05  FILLER                          PIC  X(79) VALUE         ELTREIMB
00177          'REIMBURSEMENT SUBROGATION IS NOT APPLICABLE.'.          ELTREIMB
00178 *                                                                 ELTREIMB
00179  01  WS-VOLUNTARY-MSG.                                            ELTREIMB
00180      05  FILLER                          PIC  X(79) VALUE         ELTREIMB
00181          'REIMBURSEMENT SUBROGATION IS VOLUNTARY.'.               ELTREIMB
00182 *                                                                 ELTREIMB
00183  01  WS-DISCLAIMER.                                               ELTREIMB
00184      05  FILLER                          PIC X(79) VALUE          ELTREIMB
00185          '*** SUBJECT TO OTHER CONTRACT LIMITATIONS ***'.         ELTREIMB
00186  01  WS-END                              PIC X(18) VALUE          ELTREIMB
00187                                          '*** END OF W/S ***'.    ELTREIMB
00188                                                                   ELTREIMB
00189  LINKAGE SECTION.                                                 ELTREIMB
00190  01  DFHCOMMAREA.                                                 ELTREIMB
00191      COPY ELSCOMMC.                                               ELTREIMB
00192 /                                                                 ELTREIMB
00193      COPY ELSCIA2C.                                               ELTREIMB
00194 /                                                                 ELTREIMB
00195      COPY ELSCMDSC.                                               ELTREIMB
00196 /                                                                 ELTREIMB
00197      COPY ELSCMIFC.                                               ELTREIMB
00198 /                                                                 ELTREIMB
00199      COPY ELSIOPMC.                                               ELTREIMB
00200 /                                                                 ELTREIMB
00201      COPY ELSKEYSC.                                               ELTREIMB
00202 /                                                                 ELTREIMB
00203      COPY ELSOUTPC.                                               ELTREIMB
00204 /                                                                 ELTREIMB
00205      COPY ELSSRTPC.                                               ELTREIMB
00206 /                                                                 ELTREIMB
00207      COPY ELSTCWAC.                                               ELTREIMB
00208 /                                                                 ELTREIMB
00209      COPY ELSSSCBC.                                               ELTREIMB
00210 /                                                                 ELTREIMB
00211  01  GROUP-SPECIFIC-RECORD.                                       ELTREIMB
00212      COPY GCGROUPC.                                               ELTREIMB
00213 /                                                                 ELTREIMB
00214  01  GCCP-TABULAR-REC.                                            ELTREIMB
00215      COPY GCTGCCPC.                                               ELTREIMB
00216 /                                                                 ELTREIMB
00217      EJECT                                                        ELTREIMB
00218  PROCEDURE DIVISION.                                              ELTREIMB
00219 ************************************************************      ELTREIMB
00220 *                                                          *      ELTREIMB
00221 *                    PROCEDURE DIVISION                    *      ELTREIMB
00222 *                                                          *      ELTREIMB
00223 ************************************************************      ELTREIMB
00224                                                                   ELTREIMB
00225                                                                   ELTREIMB
00226 ************************************************************      ELTREIMB
00227 *                                                          *      ELTREIMB
00228 *        REIMBURSEMENT SUBROGATION                         *      ELTREIMB
00229 *                                                          *      ELTREIMB
00230 ************************************************************      ELTREIMB
00231  REIMBURSEMENT-SUBROGATION.                                       ELTREIMB
00232      PERFORM INITIALIZATION-RTN.                                  ELTREIMB
00233      PERFORM PROCESS-REIMBURSEMENT-SUBROGAT.                      ELTREIMB
00234      GOBACK.                                                      ELTREIMB
00235                                                                   ELTREIMB
00236                                                                   ELTREIMB
00237 ************************************************************      ELTREIMB
00238 *                                                          *      ELTREIMB
00239 *        INITIALIZATION-RTN                                *      ELTREIMB
00240 *                                                          *      ELTREIMB
00241 ************************************************************      ELTREIMB
00242  INITIALIZATION-RTN.                                              ELTREIMB
00243      PERFORM ESTABLISH-ADDRESS-OF-CNTRL-BLK.                      ELTREIMB
00244      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTREIMB
00245                                                                   ELTREIMB
00246                                                                   ELTREIMB
00247 ************************************************************      ELTREIMB
00248 *                                                          *      ELTREIMB
00249 *        ESTABLISH ADDRESS OF CNTRL BLKS                   *      ELTREIMB
00250 *                                                          *      ELTREIMB
00251 ************************************************************      ELTREIMB
00252  ESTABLISH-ADDRESS-OF-CNTRL-BLK.                                  ELTREIMB
00253      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTREIMB
00254      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTREIMB
00255      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTREIMB
00256                                                                   ELTREIMB
00257                                                                   ELTREIMB
00258 ************************************************************      ELTREIMB
00259 *                                                          *      ELTREIMB
00260 *        CHECK FOR VALID COMMAREA                          *      ELTREIMB
00261 *                                                          *      ELTREIMB
00262 ************************************************************      ELTREIMB
00263  CHECK-FOR-VALID-COMMAREA.                                        ELTREIMB
00264      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTREIMB
00265          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTREIMB
00266                                                                   ELTREIMB
00267                                                                   ELTREIMB
00268 ************************************************************      ELTREIMB
00269 *                                                          *      ELTREIMB
00270 *        SIGNAL INVALID COMMAREA                           *      ELTREIMB
00271 *                                                          *      ELTREIMB
00272 ************************************************************      ELTREIMB
00273  SIGNAL-INVALID-COMMAREA.                                         ELTREIMB
00274      EXEC CICS ABEND                                              ELTREIMB
00275                ABCODE('EL01')                                     ELTREIMB
00276         END-EXEC.                                                 ELTREIMB
00277      EJECT                                                        ELTREIMB
00278                                                                   ELTREIMB
00279                                                                   ELTREIMB
00280 ************************************************************      ELTREIMB
00281 *                                                          *      ELTREIMB
00282 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTREIMB
00283 *                                                          *      ELTREIMB
00284 ************************************************************      ELTREIMB
00285  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTREIMB
00286      IF ECA-CIA-PTR = NULL                                        ELTREIMB
00287          PERFORM SIGNAL-INVALID-CIA                               ELTREIMB
00288      ELSE                                                         ELTREIMB
00289          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTREIMB
00290                                                                   ELTREIMB
00291                                                                   ELTREIMB
00292 ************************************************************      ELTREIMB
00293 *                                                          *      ELTREIMB
00294 *        SIGNAL INVALID CIA                                *      ELTREIMB
00295 *                                                          *      ELTREIMB
00296 ************************************************************      ELTREIMB
00297  SIGNAL-INVALID-CIA.                                              ELTREIMB
00298      EXEC CICS ABEND                                              ELTREIMB
00299                ABCODE('EL02')                                     ELTREIMB
00300         END-EXEC.                                                 ELTREIMB
00301      EJECT                                                        ELTREIMB
00302                                                                   ELTREIMB
00303                                                                   ELTREIMB
00304 ************************************************************      ELTREIMB
00305 *                                                          *      ELTREIMB
00306 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTREIMB
00307 *                                                          *      ELTREIMB
00308 ************************************************************      ELTREIMB
00309  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTREIMB
00310      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTREIMB
00311      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTREIMB
00312                            ADDRESS OF                             ELTREIMB
00313          SSB-SELECTOR-STATUS-CTL-BLK.                             ELTREIMB
00314      IF CIA-RC-PTR-NULL                                           ELTREIMB
00315          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTREIMB
00316                                                                   ELTREIMB
00317                                                                   ELTREIMB
00318 ************************************************************      ELTREIMB
00319 *                                                          *      ELTREIMB
00320 *        SIGNAL UNALLOC AREA ERROR                         *      ELTREIMB
00321 *                                                          *      ELTREIMB
00322 ************************************************************      ELTREIMB
00323  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTREIMB
00324      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTREIMB
00325      PERFORM SIGNAL-ABEND.                                        ELTREIMB
00326                                                                   ELTREIMB
00327                                                                   ELTREIMB
00328 ************************************************************      ELTREIMB
00329 *                                                          *      ELTREIMB
00330 *        SIGNAL ABEND                                      *      ELTREIMB
00331 *                                                          *      ELTREIMB
00332 ************************************************************      ELTREIMB
00333  SIGNAL-ABEND.                                                    ELTREIMB
00334      EXEC CICS ABEND                                              ELTREIMB
00335                ABCODE(CIA-ABCODE)                                 ELTREIMB
00336         END-EXEC.                                                 ELTREIMB
00337      EJECT                                                        ELTREIMB
00338                                                                   ELTREIMB
00339                                                                   ELTREIMB
00340 ************************************************************      ELTREIMB
00341 *                                                          *      ELTREIMB
00342 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTREIMB
00343 *                                                          *      ELTREIMB
00344 ************************************************************      ELTREIMB
00345  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTREIMB
00346      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTREIMB
00347      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTREIMB
00348      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTREIMB
00349      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTREIMB
00350      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTREIMB
00351      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTREIMB
00352      PERFORM ESTABLISH-ADDRESSABILITY-OF-CS.                      ELTREIMB
00353                                                                   ELTREIMB
00354                                                                   ELTREIMB
00355 ************************************************************      ELTREIMB
00356 *                                                          *      ELTREIMB
00357 *        ESTABLISH ADDRESS OF CIA                          *      ELTREIMB
00358 *                                                          *      ELTREIMB
00359 ************************************************************      ELTREIMB
00360  ESTABLISH-ADDRESS-OF-CIA.                                        ELTREIMB
00361      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTREIMB
00362                            ADDRESS OF                             ELTREIMB
00363          CIA-ELS-COMMON-INTERFACE-AREA.                           ELTREIMB
00364                                                                   ELTREIMB
00365                                                                   ELTREIMB
00366 ************************************************************      ELTREIMB
00367 *                                                          *      ELTREIMB
00368 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTREIMB
00369 *                                                          *      ELTREIMB
00370 ************************************************************      ELTREIMB
00371  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTREIMB
00372      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTREIMB
00373      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTREIMB
00374                            ADDRESS OF                             ELTREIMB
00375          CMF-CODES-MANUAL-INTERFACE.                              ELTREIMB
00376      IF CIA-RC-PTR-NULL                                           ELTREIMB
00377          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTREIMB
00378      EJECT                                                        ELTREIMB
00379                                                                   ELTREIMB
00380                                                                   ELTREIMB
00381 ************************************************************      ELTREIMB
00382 *                                                          *      ELTREIMB
00383 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTREIMB
00384 *                                                          *      ELTREIMB
00385 ************************************************************      ELTREIMB
00386  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTREIMB
00387      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTREIMB
00388      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTREIMB
00389                            ADDRESS OF                             ELTREIMB
00390          COF-OUTPUT-INTERFACE.                                    ELTREIMB
00391      IF CIA-RC-PTR-NULL                                           ELTREIMB
00392          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTREIMB
00393      EJECT                                                        ELTREIMB
00394                                                                   ELTREIMB
00395                                                                   ELTREIMB
00396 ************************************************************      ELTREIMB
00397 *                                                          *      ELTREIMB
00398 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTREIMB
00399 *                                                          *      ELTREIMB
00400 ************************************************************      ELTREIMB
00401  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTREIMB
00402      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTREIMB
00403      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTREIMB
00404                            ADDRESS OF                             ELTREIMB
00405          SRP-SUBROUTINE-PARAMETERS.                               ELTREIMB
00406      IF CIA-RC-PTR-NULL                                           ELTREIMB
00407          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTREIMB
00408      EJECT                                                        ELTREIMB
00409                                                                   ELTREIMB
00410                                                                   ELTREIMB
00411 ************************************************************      ELTREIMB
00412 *                                                          *      ELTREIMB
00413 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTREIMB
00414 *                                                          *      ELTREIMB
00415 ************************************************************      ELTREIMB
00416  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTREIMB
00417      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTREIMB
00418      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTREIMB
00419                            ADDRESS OF                             ELTREIMB
00420          TCAR-COMPRESSION-WORK-AREA.                              ELTREIMB
00421      IF CIA-RC-PTR-NULL                                           ELTREIMB
00422          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTREIMB
00423      EJECT                                                        ELTREIMB
00424                                                                   ELTREIMB
00425                                                                   ELTREIMB
00426 ************************************************************      ELTREIMB
00427 *                                                          *      ELTREIMB
00428 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTREIMB
00429 *                                                          *      ELTREIMB
00430 ************************************************************      ELTREIMB
00431  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTREIMB
00432      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTREIMB
00433      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTREIMB
00434                            ADDRESS OF                             ELTREIMB
00435          KWA-FILE-KEY-WORK-AREA.                                  ELTREIMB
00436      IF CIA-RC-PTR-NULL                                           ELTREIMB
00437          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTREIMB
00438      EJECT                                                        ELTREIMB
00439                                                                   ELTREIMB
00440                                                                   ELTREIMB
00441 ************************************************************      ELTREIMB
00442 *                                                          *      ELTREIMB
00443 *        ESTABLISH ADDRESSABILITY OF GRP SPECIFIC          *      ELTREIMB
00444 *                                                          *      ELTREIMB
00445 ************************************************************      ELTREIMB
00446  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTREIMB
00447      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTREIMB
00448      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTREIMB
00449                            ADDRESS OF                             ELTREIMB
00450          GROUP-SPECIFIC-RECORD.                                   ELTREIMB
00451      IF CIA-RC-PTR-NULL                                           ELTREIMB
00452          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTREIMB
00453      EJECT                                                        ELTREIMB
00454                                                                   ELTREIMB
00455                                                                   ELTREIMB
00456 ************************************************************      ELTREIMB
00457 *                                                          *      ELTREIMB
00458 *        ESTABLISH ADDRESSABILITY OF CST CONTAINMENT       *      ELTREIMB
00459 *                                                          *      ELTREIMB
00460 ************************************************************      ELTREIMB
00461  ESTABLISH-ADDRESSABILITY-OF-CS.                                  ELTREIMB
00462      SET CIA-GCTABULR-DDN TO TRUE.                                ELTREIMB
00463      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTREIMB
00464                            ADDRESS OF GCCP-TABULAR-REC.           ELTREIMB
00465      IF CIA-RC-PTR-NULL                                           ELTREIMB
00466          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTREIMB
00467      EJECT                                                        ELTREIMB
00468                                                                   ELTREIMB
00469                                                                   ELTREIMB
00470 ************************************************************      ELTREIMB
00471 *                                                          *      ELTREIMB
00472 *        PROCESS REIMBURSEMENT SUBROGATION                 *      ELTREIMB
00473 *                                                          *      ELTREIMB
00474 ************************************************************      ELTREIMB
00475  PROCESS-REIMBURSEMENT-SUBROGAT.                                  ELTREIMB
00476      IF GCG-REIMBUR-SUBROG-IND EQUAL ZERO OR '08'                 ELTREIMB
00477          PERFORM TEST-APPLICABILITY                               ELTREIMB
00478      ELSE                                                         ELTREIMB
00479          PERFORM GENERATE-REIMBURSEMENT-SUBROGA.                  ELTREIMB
00480      MOVE 'E' TO  COF-FUNCTION.                                   ELTREIMB
00481      MOVE ZEROS TO COF-NBR-DTL-LINES                              ELTREIMB
00482                COF-NBR-HDR-LINES.                                 ELTREIMB
00483      PERFORM LINK-TO-OUTPUT.                                      ELTREIMB
00484                                                                   ELTREIMB
00485                                                                   ELTREIMB
00486 ************************************************************      ELTREIMB
00487 *                                                          *      ELTREIMB
00488 *        GENERATE REIMBURSEMENT SUBROGATION TEXT           *      ELTREIMB
00489 *                                                          *      ELTREIMB
00490 ************************************************************      ELTREIMB
00491  GENERATE-REIMBURSEMENT-SUBROGA.                                  ELTREIMB
00492      PERFORM VERIFY-REIMBURSEMENT-SUBROGATI.                      ELTREIMB
00493      PERFORM BUILD-REIMBURSEMENT-SUBROGATIO.                      ELTREIMB
00494      EJECT                                                        ELTREIMB
00495                                                                   ELTREIMB
00496                                                                   ELTREIMB
00497 ************************************************************      ELTREIMB
00498 *                                                          *      ELTREIMB
00499 *        TEST APPLICABILITY                                *      ELTREIMB
00500 *                                                          *      ELTREIMB
00501 ************************************************************      ELTREIMB
00502  TEST-APPLICABILITY.                                              ELTREIMB
00503      PERFORM GENERATE-HEADINGS.                                   ELTREIMB
00504      IF GCG-REIMBUR-SUBROG-IND EQUAL ZERO                         ELTREIMB
00505          PERFORM SIGNAL-NOT-APPLICABLE-MSG                        ELTREIMB
00506      ELSE IF GCG-REIMBUR-SUBROG-IND EQUAL '08'                    ELTREIMB
00507          PERFORM SIGNAL-VOLUNTARY-MSG.                            ELTREIMB
00508      PERFORM LINK-TO-OUTPUT.                                      ELTREIMB
00509                                                                   ELTREIMB
00510                                                                   ELTREIMB
00511 ************************************************************      ELTREIMB
00512 *                                                          *      ELTREIMB
00513 *        VERIFY REIMBURSEMENT SUBROGATION IN GCCP RECORD   *      ELTREIMB
00514 *                                                          *      ELTREIMB
00515 ************************************************************      ELTREIMB
00516  VERIFY-REIMBURSEMENT-SUBROGATI.                                  ELTREIMB
00517      PERFORM ACQUIRE-GCCP-RECORD.                                 ELTREIMB
00518      PERFORM OBTAIN-REIMBURSEMENT-SUBROGATI.                      ELTREIMB
00519      EJECT                                                        ELTREIMB
00520                                                                   ELTREIMB
00521                                                                   ELTREIMB
00522 ************************************************************      ELTREIMB
00523 *                                                          *      ELTREIMB
00524 *        ACQUIRE GCCP RECORD                               *      ELTREIMB
00525 *                                                          *      ELTREIMB
00526 ************************************************************      ELTREIMB
00527  ACQUIRE-GCCP-RECORD.                                             ELTREIMB
00528      MOVE SPACES TO KWA-PROVISION-ID.                             ELTREIMB
00529      SET GCG-INDEX TO 1.                                          ELTREIMB
00530      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTREIMB
00531          AT END                                                   ELTREIMB
00532             MOVE ZEROES TO KWA-PROVISION-SLOT-NO                  ELTREIMB
00533          WHEN GCG-TAB-ID (GCG-INDEX) = PC-GCCP                    ELTREIMB
00534                 MOVE GCG-TAB-ID (GCG-INDEX) TO                    ELTREIMB
00535          KWA-PROVISION-ID                                         ELTREIMB
00536                 MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                  ELTREIMB
00537                     TO KWA-PROVISION-SLOT-NO                      ELTREIMB
00538          END-SEARCH.                                              ELTREIMB
00539      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELTREIMB
00540          PERFORM SIGNAL-UNDEFINED-TABULAR                         ELTREIMB
00541      ELSE                                                         ELTREIMB
00542          PERFORM READ-GCCP-RECORD.                                ELTREIMB
00543                                                                   ELTREIMB
00544                                                                   ELTREIMB
00545 ************************************************************      ELTREIMB
00546 *                                                          *      ELTREIMB
00547 *        SIGNAL UNDEFINED TABULAR                          *      ELTREIMB
00548 *                                                          *      ELTREIMB
00549 ************************************************************      ELTREIMB
00550  SIGNAL-UNDEFINED-TABULAR.                                        ELTREIMB
00551      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTREIMB
00552      PERFORM SIGNAL-ABEND.                                        ELTREIMB
00553      EJECT                                                        ELTREIMB
00554                                                                   ELTREIMB
00555                                                                   ELTREIMB
00556 ************************************************************      ELTREIMB
00557 *                                                          *      ELTREIMB
00558 *        OBTAIN REIMBURSEMENT SUBROGATION WITHIN GCCP RECOR*      ELTREIMB
00559 *                                                          *      ELTREIMB
00560 ************************************************************      ELTREIMB
00561  OBTAIN-REIMBURSEMENT-SUBROGATI.                                  ELTREIMB
00562      SET GSS-INDEX TO 1.                                          ELTREIMB
00563      SEARCH GSS-ENTRY                                             ELTREIMB
00564         AT END                                                    ELTREIMB
00565            SET TABULAR-IS-UNDEFINED TO TRUE                       ELTREIMB
00566         WHEN GSS-RS-PROG-CODE-CHR (GSS-INDEX)                     ELTREIMB
00567            SET TABULAR-IS-DEFINED TO TRUE                         ELTREIMB
00568          END-SEARCH.                                              ELTREIMB
00569      IF TABULAR-IS-UNDEFINED                                      ELTREIMB
00570          PERFORM SIGNAL-UNDEFINED-TABULAR.                        ELTREIMB
00571      EJECT                                                        ELTREIMB
00572                                                                   ELTREIMB
00573                                                                   ELTREIMB
00574 ************************************************************      ELTREIMB
00575 *                                                          *      ELTREIMB
00576 *        BUILD REIMBURSEMENT SUBROGATION TEXT              *      ELTREIMB
00577 *                                                          *      ELTREIMB
00578 ************************************************************      ELTREIMB
00579  BUILD-REIMBURSEMENT-SUBROGATIO.                                  ELTREIMB
00580      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTREIMB
00581          PERFORM GENERATE-INSTITUTIONAL.                          ELTREIMB
00582      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELTREIMB
00583          PERFORM GENERATE-PROFESSIONAL.                           ELTREIMB
00584                                                                   ELTREIMB
00585                                                                   ELTREIMB
00586 ************************************************************      ELTREIMB
00587 *                                                          *      ELTREIMB
00588 *        GENERATE INSTITUTIONAL                            *      ELTREIMB
00589 *                                                          *      ELTREIMB
00590 ************************************************************      ELTREIMB
00591  GENERATE-INSTITUTIONAL.                                          ELTREIMB
00592      SET INSTITUTIONAL-SCREEN TO TRUE.                            ELTREIMB
00593      PERFORM GENERATE-HEADINGS.                                   ELTREIMB
00594      PERFORM TRANSLATE-REIMBUR-SUBROG-IND.                        ELTREIMB
00595      PERFORM BUILD-INSTITUTIONAL-TEXT.                            ELTREIMB
00596      EJECT                                                        ELTREIMB
00597                                                                   ELTREIMB
00598                                                                   ELTREIMB
00599 ************************************************************      ELTREIMB
00600 *                                                          *      ELTREIMB
00601 *        GENERATE PROFESSIONAL                             *      ELTREIMB
00602 *                                                          *      ELTREIMB
00603 ************************************************************      ELTREIMB
00604  GENERATE-PROFESSIONAL.                                           ELTREIMB
00605      SET PROFESSIONAL-SCREEN TO TRUE.                             ELTREIMB
00606      PERFORM GENERATE-HEADINGS.                                   ELTREIMB
00607      PERFORM TRANSLATE-REIMBUR-SUBROG-IND.                        ELTREIMB
00608      PERFORM BUILD-PROFESSIONAL-TEXT.                             ELTREIMB
00609      EJECT                                                        ELTREIMB
00610                                                                   ELTREIMB
00611                                                                   ELTREIMB
00612 ************************************************************      ELTREIMB
00613 *                                                          *      ELTREIMB
00614 *        GENERATE HEADINGS                                 *      ELTREIMB
00615 *                                                          *      ELTREIMB
00616 ************************************************************      ELTREIMB
00617  GENERATE-HEADINGS.                                               ELTREIMB
00618      SET COF-NEW-PAGE TO TRUE.                                    ELTREIMB
00619      IF INSTITUTIONAL-SCREEN                                      ELTREIMB
00620          PERFORM MOVE-INST-HEADINGS                               ELTREIMB
00621      ELSE IF PROFESSIONAL-SCREEN                                  ELTREIMB
00622          PERFORM MOVE-PROF-HEADINGS.                              ELTREIMB
00623      MOVE 2 TO COF-NBR-HDR-LINES.                                 ELTREIMB
00624      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTREIMB
00625      MOVE WS-HDR-LN2 TO COF-HDR-LINE                              ELTREIMB
00626          (COF-NBR-HDR-LINES).                                     ELTREIMB
00627      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTREIMB
00628      IF INSTITUTIONAL-SCREEN                                      ELTREIMB
00629              OR PROFESSIONAL-SCREEN                               ELTREIMB
00630          PERFORM LINK-TO-OUTPUT.                                  ELTREIMB
00631                                                                   ELTREIMB
00632                                                                   ELTREIMB
00633 ************************************************************      ELTREIMB
00634 *                                                          *      ELTREIMB
00635 *        MOVE INST HEADINGS                                *      ELTREIMB
00636 *                                                          *      ELTREIMB
00637 ************************************************************      ELTREIMB
00638  MOVE-INST-HEADINGS.                                              ELTREIMB
00639      MOVE 'INSTITUTIONAL' TO WS-HDR-TITLE.                        ELTREIMB
00640                                                                   ELTREIMB
00641                                                                   ELTREIMB
00642 ************************************************************      ELTREIMB
00643 *                                                          *      ELTREIMB
00644 *        MOVE PROF HEADINGS                                *      ELTREIMB
00645 *                                                          *      ELTREIMB
00646 ************************************************************      ELTREIMB
00647  MOVE-PROF-HEADINGS.                                              ELTREIMB
00648      MOVE 'PROFESSIONAL' TO WS-HDR-TITLE.                         ELTREIMB
00649      EJECT                                                        ELTREIMB
00650                                                                   ELTREIMB
00651                                                                   ELTREIMB
00652 ************************************************************      ELTREIMB
00653 *                                                          *      ELTREIMB
00654 *        TRANSLATE REIMBUR SUBROG IND                      *      ELTREIMB
00655 *                                                          *      ELTREIMB
00656 ************************************************************      ELTREIMB
00657  TRANSLATE-REIMBUR-SUBROG-IND.                                    ELTREIMB
00658      INITIALIZE TCAR-FROM-AREA.                                   ELTREIMB
00659      MOVE +1 TO TCAR-FROM-SUB.                                    ELTREIMB
00660 *    MOVE WS-REIMB-APPLIES TO TCAR-FROM-LINE                      ELTREIMB
00661 *        (TCAR-FROM-SUB).                                         ELTREIMB
00662 *    ADD +1 TO TCAR-FROM-SUB.                                     ELTREIMB
00663      SET BLANK-LINE-NEEDED TO TRUE.                               ELTREIMB
00664      SET PERIOD-NEEDED TO TRUE.                                   ELTREIMB
00665      MOVE GCG-REIMBUR-SUBROG-IND TO CMF-CODE-VALUE.               ELTREIMB
00666      MOVE 'REIMBUR-SUBROG-IND' TO CMF-ELEMENT-SYSTEM-NAME.        ELTREIMB
00667      MOVE PC-GRP TO CMF-RECORD-PREFIX.                            ELTREIMB
00668      EXEC CICS LINK                                               ELTREIMB
00669           PROGRAM('ELUCMIF')                                      ELTREIMB
00670           COMMAREA(DFHCOMMAREA)                                   ELTREIMB
00671        END-EXEC.                                                  ELTREIMB
00672      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTREIMB
00673      MOVE SPACE TO ADDITIONAL-TEXT-SW.                            ELTREIMB
00674                                                                   ELTREIMB
00675                                                                   ELTREIMB
00676 ************************************************************      ELTREIMB
00677 *                                                          *      ELTREIMB
00678 *        BUILD INSTITUTIONAL TEXT                          *      ELTREIMB
00679 *                                                          *      ELTREIMB
00680 ************************************************************      ELTREIMB
00681  BUILD-INSTITUTIONAL-TEXT.                                        ELTREIMB
00682      IF GSS-RS-BC-IND (GSS-INDEX) EQUAL                           ELTREIMB
00683          ZEROES                                                   ELTREIMB
00684                 OR SPACES OR LOW-VALUES                           ELTREIMB
00685          PERFORM SIGNAL-NOT-APPLICABLE-FOR-LOBC                   ELTREIMB
00686      ELSE                                                         ELTREIMB
00687          PERFORM CONSTRUCT-BC-TEXT-AND-SCREEN.                    ELTREIMB
00688                                                                   ELTREIMB
00689                                                                   ELTREIMB
00690 ************************************************************      ELTREIMB
00691 *                                                          *      ELTREIMB
00692 *        BUILD PROFESSIONAL TEXT                           *      ELTREIMB
00693 *                                                          *      ELTREIMB
00694 ************************************************************      ELTREIMB
00695  BUILD-PROFESSIONAL-TEXT.                                         ELTREIMB
00696      IF GSS-RS-BC-IND (GSS-INDEX) EQUAL                           ELTREIMB
00697          ZEROES                                                   ELTREIMB
00698                 OR SPACES OR LOW-VALUES                           ELTREIMB
00699          PERFORM SIGNAL-NOT-APPLICABLE-FOR-LOBS                   ELTREIMB
00700      ELSE                                                         ELTREIMB
00701          PERFORM CONSTRUCT-BS-TEXT-AND-SCREEN.                    ELTREIMB
00702                                                                   ELTREIMB
00703                                                                   ELTREIMB
00704 ************************************************************      ELTREIMB
00705 *                                                          *      ELTREIMB
00706 *        SIGNAL NOT APPLICABLE FOR LOBC                    *      ELTREIMB
00707 *                                                          *      ELTREIMB
00708 ************************************************************      ELTREIMB
00709  SIGNAL-NOT-APPLICABLE-FOR-LOBC.                                  ELTREIMB
00710      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTREIMB
00711      MOVE WS-NOT-APPLICABLE-LOB-BC TO COF-DTL-LINE                ELTREIMB
00712          (COF-NBR-DTL-LINES).                                     ELTREIMB
00713      PERFORM LINK-TO-OUTPUT.                                      ELTREIMB
00714                                                                   ELTREIMB
00715                                                                   ELTREIMB
00716 ************************************************************      ELTREIMB
00717 *                                                          *      ELTREIMB
00718 *        SIGNAL NOT APPLICABLE FOR LOBS                    *      ELTREIMB
00719 *                                                          *      ELTREIMB
00720 ************************************************************      ELTREIMB
00721  SIGNAL-NOT-APPLICABLE-FOR-LOBS.                                  ELTREIMB
00722      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTREIMB
00723      MOVE WS-NOT-APPLICABLE-LOB-BS TO COF-DTL-LINE                ELTREIMB
00724          (COF-NBR-DTL-LINES).                                     ELTREIMB
00725      PERFORM LINK-TO-OUTPUT.                                      ELTREIMB
00726      EJECT                                                        ELTREIMB
00727                                                                   ELTREIMB
00728                                                                   ELTREIMB
00729 ************************************************************      ELTREIMB
00730 *                                                          *      ELTREIMB
00731 *        CONSTRUCT BC TEXT AND SCREEN                      *      ELTREIMB
00732 * IF INVEST-METHOD IS '1' THEM DENIAL PARM WILL BE ZERO    *      ELTREIMB
00733 ************************************************************      ELTREIMB
00734  CONSTRUCT-BC-TEXT-AND-SCREEN.                                    ELTREIMB
00735      INITIALIZE WS-PERIOD-NEEDED-SW.                              ELTREIMB
00736      PERFORM TRANSLATE-RESPONSIBILITY-IND.                        ELTREIMB
00737      PERFORM TRANSLATE-BC-INVEST-AND-IND.                         ELTREIMB
00738      PERFORM TRANSLATE-BC-INI-QUAL-MIN-SENT.                      ELTREIMB
00739      PERFORM TRANSLATE-BC-SUB-QUAL-INV.                           ELTREIMB
00740      PERFORM TRANSLATE-BC-INV-METH.                               ELTREIMB
00741      IF GSS-RS-RESPONSIBILITY-IND (GSS-INDEX) = '1' OR '2'        ELTREIMB
00742        PERFORM TRANSLATE-DOC-FREQ-IND                             ELTREIMB
00743      ELSE                                                         ELTREIMB
00744         CONTINUE                                                  ELTREIMB
00745      END-IF.                                                      ELTREIMB
00746      IF GSS-RS-BC-INVEST-METHOD (GSS-INDEX) = '1'                 ELTREIMB
00747         OR (GSS-RS-BC-DENIAL-PARAMETER (GSS-INDEX) =              ELTREIMB
00748             ZEROES AND SPACES AND LOW-VALUES)                     ELTREIMB
00749         CONTINUE                                                  ELTREIMB
00750      ELSE                                                         ELTREIMB
00751         PERFORM TRANSLATE-BC-DENIAL-PARM                          ELTREIMB
00752      END-IF.                                                      ELTREIMB
00753      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTREIMB
00754      MOVE SPACES TO SCREEN-TYPE.                                  ELTREIMB
00755                                                                   ELTREIMB
00756 ************************************************************      ELTREIMB
00757 *                                                          *      ELTREIMB
00758 *        CONSTRUCT BS TEXT AND SCREEN                      *      ELTREIMB
00759 * IF INVEST-METHOD IS '1' THEM DENIAL PARM WILL BE ZERO    *      ELTREIMB
00760 ************************************************************      ELTREIMB
00761  CONSTRUCT-BS-TEXT-AND-SCREEN.                                    ELTREIMB
00762      INITIALIZE WS-PERIOD-NEEDED-SW.                              ELTREIMB
00763      PERFORM TRANSLATE-BS-INVEST-AND-IND.                         ELTREIMB
00764      PERFORM TRANSLATE-BS-INI-QUAL-MIN-SENT.                      ELTREIMB
00765      PERFORM TRANSLATE-BS-SUB-QUAL-INV.                           ELTREIMB
00766      PERFORM TRANSLATE-BS-INV-METH.                               ELTREIMB
00767      IF GSS-RS-RESPONSIBILITY-IND (GSS-INDEX) = '1' OR '2'        ELTREIMB
00768        PERFORM TRANSLATE-DOC-FREQ-IND                             ELTREIMB
00769      ELSE                                                         ELTREIMB
00770         CONTINUE                                                  ELTREIMB
00771      END-IF.                                                      ELTREIMB
00772      IF GSS-RS-BS-INVEST-METHOD (GSS-INDEX) = '1'                 ELTREIMB
00773         OR (GSS-RS-BC-DENIAL-PARAMETER (GSS-INDEX) =              ELTREIMB
00774             ZEROES AND SPACES AND LOW-VALUES)                     ELTREIMB
00775         CONTINUE                                                  ELTREIMB
00776      ELSE                                                         ELTREIMB
00777         PERFORM TRANSLATE-BS-DENIAL-PARM                          ELTREIMB
00778      END-IF.                                                      ELTREIMB
00779      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTREIMB
00780      MOVE SPACES TO SCREEN-TYPE.                                  ELTREIMB
00781                                                                   ELTREIMB
00782 ************************************************************      ELTREIMB
00783 *                                                          *      ELTREIMB
00784 *      TRANSLATE RESPONSIBILITY INDICATOR                  *      ELTREIMB
00785 *                                                          *      ELTREIMB
00786 ************************************************************      ELTREIMB
00787  TRANSLATE-RESPONSIBILITY-IND.                                    ELTREIMB
00788      SET PERIOD-NEEDED                                            ELTREIMB
00789          BLANK-LINE-NEEDED TO TRUE.                               ELTREIMB
00790      MOVE SPACES TO TCAR-FROM-AREA.                               ELTREIMB
00791      MOVE +1 TO TCAR-FROM-SUB.                                    ELTREIMB
00792      MOVE WS-RESPONSIBILITY-IND                                   ELTREIMB
00793               TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELTREIMB
00794      ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
00795      MOVE 'RS-RESPONSIBILITY-IND' TO                              ELTREIMB
00796          CMF-ELEMENT-SYSTEM-NAME.                                 ELTREIMB
00797      MOVE GSS-RS-RESPONSIBILITY-IND (GSS-INDEX) TO                ELTREIMB
00798          CMF-CODE-VALUE.                                          ELTREIMB
00799      PERFORM LINK-TO-TRANSLATOR.                                  ELTREIMB
00800      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTREIMB
00801                                                                   ELTREIMB
00802 ************************************************************      ELTREIMB
00803 *                                                          *      ELTREIMB
00804 *      TRANSLATE BC SUBSEQUENT INVESTIGATION SENTENCE      *      ELTREIMB
00805 *                                                          *      ELTREIMB
00806 ************************************************************      ELTREIMB
00807  TRANSLATE-BC-SUB-QUAL-INV.                                       ELTREIMB
00808      INITIALIZE TCAR-FROM-AREA                                    ELTREIMB
00809                 ADDITIONAL-TEXT-SW.                               ELTREIMB
00810      MOVE 1 TO TCAR-FROM-SUB.                                     ELTREIMB
00811      IF GSS-RS-BC-SUB-DOL-QUAL (GSS-INDEX) NOT EQUAL ZERO         ELTREIMB
00812         IF GSS-RS-BC-SUB-DOL-MIN (GSS-INDEX) NOT EQUAL ZERO       ELTREIMB
00813             SET FULL-SENTENCE TO TRUE                             ELTREIMB
00814             PERFORM CREATE-BC-SUB-QUAL-FULL-SENT                  ELTREIMB
00815             INITIALIZE FULL-PART-SW                               ELTREIMB
00816         ELSE                                                      ELTREIMB
00817            SET PART-SENTENCE TO TRUE                              ELTREIMB
00818            PERFORM CREATE-BC-SUB-QUAL-PART-SENT                   ELTREIMB
00819            INITIALIZE FULL-PART-SW                                ELTREIMB
00820         END-IF                                                    ELTREIMB
00821      ELSE                                                         ELTREIMB
00822         CONTINUE                                                  ELTREIMB
00823      END-IF.                                                      ELTREIMB
00824      EJECT                                                        ELTREIMB
00825                                                                   ELTREIMB
00826 ************************************************************      ELTREIMB
00827 *                                                          *      ELTREIMB
00828 *      TRANSLATE BC DOLLAR MIN AND QUALIFIER SENTENCE      *      ELTREIMB
00829 *                                                          *      ELTREIMB
00830 ************************************************************      ELTREIMB
00831  TRANSLATE-BC-INI-QUAL-MIN-SENT.                                  ELTREIMB
00832      INITIALIZE TCAR-FROM-AREA                                    ELTREIMB
00833                 ADDITIONAL-TEXT-SW.                               ELTREIMB
00834      MOVE 1 TO TCAR-FROM-SUB.                                     ELTREIMB
00835      IF (GSS-RS-BC-INI-DOL-QUAL (GSS-INDEX) NOT EQUAL ZERO        ELTREIMB
00836                  AND SPACES AND LOW-VALUES)                       ELTREIMB
00837         IF GSS-RS-BC-INI-DOL-MIN (GSS-INDEX) NOT EQUAL ZERO       ELTREIMB
00838             SET FULL-SENTENCE TO TRUE                             ELTREIMB
00839             PERFORM CREATE-BC-MIN-QUAL-FULL-SENT                  ELTREIMB
00840             INITIALIZE FULL-PART-SW                               ELTREIMB
00841         ELSE                                                      ELTREIMB
00842            SET PART-SENTENCE TO TRUE                              ELTREIMB
00843            PERFORM CREATE-BC-MIN-QUAL-PART-SENT                   ELTREIMB
00844            INITIALIZE FULL-PART-SW                                ELTREIMB
00845         END-IF                                                    ELTREIMB
00846      ELSE                                                         ELTREIMB
00847         CONTINUE                                                  ELTREIMB
00848      END-IF.                                                      ELTREIMB
00849      EJECT                                                        ELTREIMB
00850                                                                   ELTREIMB
00851 ************************************************************      ELTREIMB
00852 *                                                          *      ELTREIMB
00853 *        CREATE BC SUB MIN AND QUAL FULL SENTENCE          *      ELTREIMB
00854 *                                                          *      ELTREIMB
00855 ************************************************************      ELTREIMB
00856  CREATE-BC-SUB-QUAL-FULL-SENT.                                    ELTREIMB
00857      PERFORM GET-SUB-QUAL-FIXED-TEXT.                             ELTREIMB
00858      PERFORM GET-MINIMUM-TEXT.                                    ELTREIMB
00859      MOVE GSS-RS-BC-SUB-DOL-MIN (GSS-INDEX) TO                    ELTREIMB
00860          WS-DOLLAR-AMT.                                           ELTREIMB
00861      MOVE WS-DOLLAR-AMT TO TCAR-FROM-LINE (TCAR-FROM-SUB).        ELTREIMB
00862      ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
00863      PERFORM GET-IS-TEXT.                                         ELTREIMB
00864      MOVE 'RS-BC-SUB-DOL-QUAL'  TO                                ELTREIMB
00865          CMF-ELEMENT-SYSTEM-NAME.                                 ELTREIMB
00866      MOVE GSS-RS-BC-SUB-DOL-QUAL (GSS-INDEX) TO                   ELTREIMB
00867          CMF-CODE-VALUE.                                          ELTREIMB
00868      PERFORM LINK-TO-TRANSLATOR.                                  ELTREIMB
00869      PERFORM MOVE-A-LINE UNTIL                                    ELTREIMB
00870          CMF-DESCR-IDX > CMF-NBR-DESCR-LINES.                     ELTREIMB
00871      PERFORM GET-CONNECTING-FIXED-TEXT.                           ELTREIMB
00872      SET PERIOD-NEEDED                                            ELTREIMB
00873          BLANK-LINE-NEEDED TO TRUE.                               ELTREIMB
00874      MOVE 'RS-BC-SUB-CLAIM-DESCRPTOR'  TO                         ELTREIMB
00875          CMF-ELEMENT-SYSTEM-NAME.                                 ELTREIMB
00876      MOVE GSS-RS-BC-SUB-CLAIM-DESCRPTOR (GSS-INDEX) TO            ELTREIMB
00877          CMF-CODE-VALUE.                                          ELTREIMB
00878      PERFORM LINK-TO-TRANSLATOR.                                  ELTREIMB
00879      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTREIMB
00880                                                                   ELTREIMB
00881 ************************************************************      ELTREIMB
00882 *                                                          *      ELTREIMB
00883 *        TRANSLATE BC DOLL MIN AND QUAL PART SENTENCE      *      ELTREIMB
00884 *                                                          *      ELTREIMB
00885 ************************************************************      ELTREIMB
00886  CREATE-BC-MIN-QUAL-PART-SENT.                                    ELTREIMB
00887      PERFORM GET-QUAL-FIXED-TEXT.                                 ELTREIMB
00888      PERFORM GET-IS-TEXT.                                         ELTREIMB
00889      SET PERIOD-NEEDED                                            ELTREIMB
00890          BLANK-LINE-NEEDED TO TRUE.                               ELTREIMB
00891      MOVE 'RS-BC-INI-DOL-QUAL'  TO                                ELTREIMB
00892          CMF-ELEMENT-SYSTEM-NAME.                                 ELTREIMB
00893      MOVE GSS-RS-BC-INI-DOL-QUAL (GSS-INDEX) TO                   ELTREIMB
00894          CMF-CODE-VALUE.                                          ELTREIMB
00895      PERFORM LINK-TO-TRANSLATOR.                                  ELTREIMB
00896      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTREIMB
00897                                                                   ELTREIMB
00898 ************************************************************      ELTREIMB
00899 *                                                          *      ELTREIMB
00900 *        TRANSLATE BC DOLL SUB AND QUAL PART SENTENCE      *      ELTREIMB
00901 *                                                          *      ELTREIMB
00902 ************************************************************      ELTREIMB
00903  CREATE-BC-SUB-QUAL-PART-SENT.                                    ELTREIMB
00904      PERFORM GET-SUB-QUAL-FIXED-TEXT.                             ELTREIMB
00905      PERFORM GET-IS-TEXT.                                         ELTREIMB
00906      SET PERIOD-NEEDED                                            ELTREIMB
00907          BLANK-LINE-NEEDED TO TRUE.                               ELTREIMB
00908      MOVE 'RS-BC-SUB-DOL-QUAL'  TO                                ELTREIMB
00909          CMF-ELEMENT-SYSTEM-NAME.                                 ELTREIMB
00910      MOVE GSS-RS-BC-SUB-DOL-QUAL (GSS-INDEX) TO                   ELTREIMB
00911          CMF-CODE-VALUE.                                          ELTREIMB
00912      PERFORM LINK-TO-TRANSLATOR.                                  ELTREIMB
00913      SET CMF-DESCR-IDX TO 1.                                      ELTREIMB
00914      PERFORM MOVE-A-LINE UNTIL                                    ELTREIMB
00915         CMF-DESCR-IDX > CMF-NBR-DESCR-LINES.                      ELTREIMB
00916      PERFORM GET-CONNECTING-FIXED-TEXT.                           ELTREIMB
00917      SET PERIOD-NEEDED                                            ELTREIMB
00918          BLANK-LINE-NEEDED TO TRUE.                               ELTREIMB
00919      MOVE 'RS-BC-SUB-CLAIM-DESCRPTOR'  TO                         ELTREIMB
00920          CMF-ELEMENT-SYSTEM-NAME.                                 ELTREIMB
00921      MOVE GSS-RS-BC-SUB-CLAIM-DESCRPTOR (GSS-INDEX) TO            ELTREIMB
00922          CMF-CODE-VALUE.                                          ELTREIMB
00923      PERFORM LINK-TO-TRANSLATOR.                                  ELTREIMB
00924      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTREIMB
00925                                                                   ELTREIMB
00926                                                                   ELTREIMB
00927 ************************************************************      ELTREIMB
00928 *                                                          *      ELTREIMB
00929 *        TRANSLATE BC DOLL INI AND QUAL FULL SENTENCE      *      ELTREIMB
00930 *                                                          *      ELTREIMB
00931 ************************************************************      ELTREIMB
00932  CREATE-BC-MIN-QUAL-FULL-SENT.                                    ELTREIMB
00933      INITIALIZE WS-PERIOD-NEEDED-SW.                              ELTREIMB
00934      PERFORM GET-QUAL-FIXED-TEXT.                                 ELTREIMB
00935      PERFORM GET-MINIMUM-TEXT.                                    ELTREIMB
00936      MOVE GSS-RS-BC-INI-DOL-MIN (GSS-INDEX) TO                    ELTREIMB
00937          WS-DOLLAR-AMT.                                           ELTREIMB
00938      MOVE WS-DOLLAR-AMT TO TCAR-FROM-LINE (TCAR-FROM-SUB).        ELTREIMB
00939      ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
00940      PERFORM GET-IS-TEXT.                                         ELTREIMB
00941      SET PERIOD-NEEDED                                            ELTREIMB
00942          BLANK-LINE-NEEDED TO TRUE.                               ELTREIMB
00943      MOVE 'RS-BC-INI-DOL-QUAL'  TO                                ELTREIMB
00944          CMF-ELEMENT-SYSTEM-NAME.                                 ELTREIMB
00945      MOVE GSS-RS-BC-INI-DOL-QUAL (GSS-INDEX) TO                   ELTREIMB
00946          CMF-CODE-VALUE.                                          ELTREIMB
00947      PERFORM LINK-TO-TRANSLATOR.                                  ELTREIMB
00948      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTREIMB
00949                                                                   ELTREIMB
00950 ************************************************************      ELTREIMB
00951 *                                                          *      ELTREIMB
00952 *        TRANSLATE BC INVESTIGATION METHOD SENTENCE        *      ELTREIMB
00953 *                                                          *      ELTREIMB
00954 ************************************************************      ELTREIMB
00955  TRANSLATE-BC-INV-METH.                                           ELTREIMB
00956      MOVE SPACES TO TCAR-FROM-AREA.                               ELTREIMB
00957      MOVE 1 TO TCAR-FROM-SUB.                                     ELTREIMB
00958      MOVE +1 TO TCAR-FROM-SUB.                                    ELTREIMB
00959      MOVE WS-INV-PERFORMED-AS                                     ELTREIMB
00960               TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELTREIMB
00961      ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
00962      MOVE 'RS-BC-INVEST-METHOD' TO                                ELTREIMB
00963          CMF-ELEMENT-SYSTEM-NAME.                                 ELTREIMB
00964      MOVE GSS-RS-BC-INVEST-METHOD (GSS-INDEX) TO                  ELTREIMB
00965          CMF-CODE-VALUE.                                          ELTREIMB
00966      SET PERIOD-NEEDED                                            ELTREIMB
00967          BLANK-LINE-NEEDED TO TRUE.                               ELTREIMB
00968      PERFORM LINK-TO-TRANSLATOR.                                  ELTREIMB
00969      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTREIMB
00970                                                                   ELTREIMB
00971 ************************************************************      ELTREIMB
00972 *                                                          *      ELTREIMB
00973 *        TRANSLATE BC DOCUMENT FREQUENCY INDICATOR         *      ELTREIMB
00974 *                                                          *      ELTREIMB
00975 ************************************************************      ELTREIMB
00976  TRANSLATE-DOC-FREQ-IND.                                          ELTREIMB
00977      SET PERIOD-NEEDED                                            ELTREIMB
00978          BLANK-LINE-NEEDED TO TRUE.                               ELTREIMB
00979      MOVE SPACES TO TCAR-FROM-AREA.                               ELTREIMB
00980      MOVE +1 TO TCAR-FROM-SUB.                                    ELTREIMB
00981      MOVE WS-DOC-FREQUENCY                                        ELTREIMB
00982               TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELTREIMB
00983      ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
00984      MOVE 'RS-DOC-FREQUENCY-IND' TO                               ELTREIMB
00985          CMF-ELEMENT-SYSTEM-NAME.                                 ELTREIMB
00986      MOVE GSS-RS-DOC-FREQUENCY-IND (GSS-INDEX) TO                 ELTREIMB
00987          CMF-CODE-VALUE.                                          ELTREIMB
00988      PERFORM LINK-TO-TRANSLATOR.                                  ELTREIMB
00989      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTREIMB
00990                                                                   ELTREIMB
00991 ************************************************************      ELTREIMB
00992 *                                                          *      ELTREIMB
00993 *        TRANSLATE BC DENIAL PARAMETER                     *      ELTREIMB
00994 *                                                          *      ELTREIMB
00995 ************************************************************      ELTREIMB
00996  TRANSLATE-BC-DENIAL-PARM.                                        ELTREIMB
00997      SET PERIOD-NEEDED TO TRUE.                                   ELTREIMB
00998      MOVE SPACES TO TCAR-FROM-AREA.                               ELTREIMB
00999      MOVE +1 TO TCAR-FROM-SUB.                                    ELTREIMB
01000      MOVE WS-DENIAL-PHRASE                                        ELTREIMB
01001               TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELTREIMB
01002      ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
01003      MOVE WS-DENIAL-PHRASE2                                       ELTREIMB
01004               TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELTREIMB
01005      ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
01006      MOVE 'RS-BC-DENIAL-PARAMETER' TO                             ELTREIMB
01007          CMF-ELEMENT-SYSTEM-NAME.                                 ELTREIMB
01008      MOVE GSS-RS-BC-DENIAL-PARAMETER (GSS-INDEX) TO               ELTREIMB
01009          CMF-CODE-VALUE.                                          ELTREIMB
01010      PERFORM LINK-TO-TRANSLATOR.                                  ELTREIMB
01011      SET PERIOD-NEEDED                                            ELTREIMB
01012          BLANK-LINE-NEEDED TO TRUE.                               ELTREIMB
01013      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTREIMB
01014                                                                   ELTREIMB
01015 ************************************************************      ELTREIMB
01016 *                                                          *      ELTREIMB
01017 *        GET QUALIFIED FIXED TEXT                          *      ELTREIMB
01018 *                                                          *      ELTREIMB
01019 ************************************************************      ELTREIMB
01020  GET-QUAL-FIXED-TEXT.                                             ELTREIMB
01021      MOVE SPACES TO TCAR-FROM-AREA.                               ELTREIMB
01022      SET ADDITIONAL-TEXT TO TRUE.                                 ELTREIMB
01023      MOVE +1 TO TCAR-FROM-SUB.                                    ELTREIMB
01024      MOVE WS-QUALITY-TEXT TO TCAR-FROM-LINE (TCAR-FROM-SUB).      ELTREIMB
01025      ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
01026      EJECT                                                        ELTREIMB
01027                                                                   ELTREIMB
01028                                                                   ELTREIMB
01029 ************************************************************      ELTREIMB
01030 *                                                          *      ELTREIMB
01031 *        GET SUB QUALIFIED FIXED TEXT                      *      ELTREIMB
01032 *                                                          *      ELTREIMB
01033 ************************************************************      ELTREIMB
01034  GET-SUB-QUAL-FIXED-TEXT.                                         ELTREIMB
01035      MOVE SPACES TO TCAR-FROM-AREA.                               ELTREIMB
01036      SET ADDITIONAL-TEXT TO TRUE.                                 ELTREIMB
01037      MOVE +1 TO TCAR-FROM-SUB.                                    ELTREIMB
01038      MOVE WS-SUB-TEXT TO TCAR-FROM-LINE (TCAR-FROM-SUB).          ELTREIMB
01039      ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
01040      EJECT                                                        ELTREIMB
01041                                                                   ELTREIMB
01042 ************************************************************      ELTREIMB
01043 *                                                          *      ELTREIMB
01044 *        GET MINIMUM TEXT                                  *      ELTREIMB
01045 *                                                          *      ELTREIMB
01046 ************************************************************      ELTREIMB
01047  GET-MINIMUM-TEXT.                                                ELTREIMB
01048 *    MOVE SPACES TO TCAR-FROM-AREA.                               ELTREIMB
01049 *    SET ADDITIONAL-TEXT TO TRUE.                                 ELTREIMB
01050 *    MOVE +1 TO TCAR-FROM-SUB.                                    ELTREIMB
01051      MOVE WS-MINIMUM-TEXT TO TCAR-FROM-LINE (TCAR-FROM-SUB).      ELTREIMB
01052      ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
01053      EJECT                                                        ELTREIMB
01054                                                                   ELTREIMB
01055 ************************************************************      ELTREIMB
01056 *                                                          *      ELTREIMB
01057 *        GET IS TEXT                                       *      ELTREIMB
01058 *                                                          *      ELTREIMB
01059 ************************************************************      ELTREIMB
01060  GET-IS-TEXT.                                                     ELTREIMB
01061      MOVE WS-IS-TEXT TO TCAR-FROM-LINE (TCAR-FROM-SUB).           ELTREIMB
01062      ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
01063      EJECT                                                        ELTREIMB
01064                                                                   ELTREIMB
01065 ************************************************************      ELTREIMB
01066 *                                                          *      ELTREIMB
01067 *        TRANSLATE BC INVEST AND IND                       *      ELTREIMB
01068 *                                                          *      ELTREIMB
01069 ************************************************************      ELTREIMB
01070  TRANSLATE-BC-INVEST-AND-IND.                                     ELTREIMB
01071      INITIALIZE WS-PERIOD-NEEDED-SW                               ELTREIMB
01072                 TCAR-FROM-AREA.                                   ELTREIMB
01073      SET ADDITIONAL-TEXT TO TRUE.                                 ELTREIMB
01074      MOVE 1 TO TCAR-FROM-SUB.                                     ELTREIMB
01075 *    PERFORM GET-INVESTIGATION-FIXED-TEXT.                        ELTREIMB
01076      MOVE 'RS-BC-IND'  TO                                         ELTREIMB
01077          CMF-ELEMENT-SYSTEM-NAME.                                 ELTREIMB
01078      MOVE GSS-RS-BC-IND (GSS-INDEX) TO                            ELTREIMB
01079          CMF-CODE-VALUE.                                          ELTREIMB
01080      PERFORM LINK-TO-TRANSLATOR.                                  ELTREIMB
01081      SET CMF-DESCR-IDX TO 1.                                      ELTREIMB
01082      PERFORM MOVE-A-LINE UNTIL                                    ELTREIMB
01083          CMF-DESCR-IDX > CMF-NBR-DESCR-LINES.                     ELTREIMB
01084      ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
01085      PERFORM GET-CONNECTING-FIXED-TEXT.                           ELTREIMB
01086      SET PERIOD-NEEDED                                            ELTREIMB
01087          BLANK-LINE-NEEDED TO TRUE.                               ELTREIMB
01088      PERFORM TRANSLATE-MEMBER-RELATIONSHIPX.                      ELTREIMB
01089      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTREIMB
01090      EJECT                                                        ELTREIMB
01091                                                                   ELTREIMB
01092                                                                   ELTREIMB
01093 ************************************************************      ELTREIMB
01094 *                                                          *      ELTREIMB
01095 *        GET CONNECTING FIXED TEXT                         *      ELTREIMB
01096 *                                                          *      ELTREIMB
01097 ************************************************************      ELTREIMB
01098  GET-CONNECTING-FIXED-TEXT.                                       ELTREIMB
01099      MOVE WS-FOR TO TCAR-FROM-LINE (TCAR-FROM-SUB).               ELTREIMB
01100      ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
01101      EJECT                                                        ELTREIMB
01102                                                                   ELTREIMB
01103                                                                   ELTREIMB
01104 ************************************************************      ELTREIMB
01105 *                                                          *      ELTREIMB
01106 *        GET INVESTIGATION FIXED TEXT                      *      ELTREIMB
01107 *                                                          *      ELTREIMB
01108 ************************************************************      ELTREIMB
01109 *GET-INVESTIGATION-FIXED-TEXT.                                    ELTREIMB
01110 *    MOVE SPACES TO TCAR-FROM-AREA.                               ELTREIMB
01111 *    SET ADDITIONAL-TEXT TO TRUE.                                 ELTREIMB
01112 *    MOVE +1 TO TCAR-FROM-SUB.                                    ELTREIMB
01113 *    MOVE WS-INVESTIGATION TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELTREIMB
01114 *    ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
01115      EJECT                                                        ELTREIMB
01116                                                                   ELTREIMB
01117                                                                   ELTREIMB
01118 ************************************************************      ELTREIMB
01119 *                                                          *      ELTREIMB
01120 *        TRANSLATE MEMBER RELATIONSHIP IND                 *      ELTREIMB
01121 *                                                          *      ELTREIMB
01122 ************************************************************      ELTREIMB
01123  TRANSLATE-MEMBER-RELATIONSHIPX.                                  ELTREIMB
01124      SET PERIOD-NEEDED TO TRUE.                                   ELTREIMB
01125      SET BLANK-LINE-NEEDED TO TRUE.                               ELTREIMB
01126      MOVE 'RS-MEMB-RELATIONSHIP-IND'  TO CMF-ELEMENT-SYSTEM-NAME. ELTREIMB
01127      MOVE GSS-RS-MEMB-RELATIONSHIP-IND (GSS-INDEX) TO             ELTREIMB
01128          CMF-CODE-VALUE.                                          ELTREIMB
01129      PERFORM LINK-TO-TRANSLATOR.                                  ELTREIMB
01130      EJECT                                                        ELTREIMB
01131                                                                   ELTREIMB
01132 ************************************************************      ELTREIMB
01133 *                                                          *      ELTREIMB
01134 *        GENERATE DISCLAIMER SENTENCE                      *      ELTREIMB
01135 *                                                          *      ELTREIMB
01136 ************************************************************      ELTREIMB
01137  GENERATE-DISCLAIMER-SENTENCE.                                    ELTREIMB
01138      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTREIMB
01139      MOVE WS-DISCLAIMER TO COF-DTL-LINE (COF-NBR-DTL-LINES).      ELTREIMB
01140      PERFORM LINK-TO-OUTPUT.                                      ELTREIMB
01141      EJECT                                                        ELTREIMB
01142                                                                   ELTREIMB
01143 ************************************************************      ELTREIMB
01144 *                                                          *      ELTREIMB
01145 *        SIGNAL NOT APPLICABLE MSG                         *      ELTREIMB
01146 *                                                          *      ELTREIMB
01147 ************************************************************      ELTREIMB
01148  SIGNAL-NOT-APPLICABLE-MSG.                                       ELTREIMB
01149      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTREIMB
01150      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTREIMB
01151          (COF-NBR-DTL-LINES).                                     ELTREIMB
01152      PERFORM LINK-TO-OUTPUT.                                      ELTREIMB
01153                                                                   ELTREIMB
01154                                                                   ELTREIMB
01155 ************************************************************      ELTREIMB
01156 *                                                          *      ELTREIMB
01157 *        SIGNAL VOLUNTARY MSG                              *      ELTREIMB
01158 *                                                          *      ELTREIMB
01159 ************************************************************      ELTREIMB
01160  SIGNAL-VOLUNTARY-MSG.                                            ELTREIMB
01161      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTREIMB
01162      MOVE WS-VOLUNTARY-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).   ELTREIMB
01163      PERFORM LINK-TO-OUTPUT.                                      ELTREIMB
01164      EJECT                                                        ELTREIMB
01165                                                                   ELTREIMB
01166                                                                   ELTREIMB
01167 ************************************************************      ELTREIMB
01168 *                                                          *      ELTREIMB
01169 *        LINK TO TRANSLATOR                                *      ELTREIMB
01170 *                                                          *      ELTREIMB
01171 ************************************************************      ELTREIMB
01172  LINK-TO-TRANSLATOR.                                              ELTREIMB
01173      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTREIMB
01174      EXEC CICS LINK                                               ELTREIMB
01175           PROGRAM('ELUCMIF')                                      ELTREIMB
01176           COMMAREA(DFHCOMMAREA)                                   ELTREIMB
01177           END-EXEC.                                               ELTREIMB
01178      EJECT                                                        ELTREIMB
01179                                                                   ELTREIMB
01180                                                                   ELTREIMB
01181 ************************************************************      ELTREIMB
01182 *                                                          *      ELTREIMB
01183 *        READ GCCP RECORD                                  *      ELTREIMB
01184 *                                                          *      ELTREIMB
01185 ************************************************************      ELTREIMB
01186  READ-GCCP-RECORD.                                                ELTREIMB
01187      SET CIA-GCTABULR-DDN TO TRUE.                                ELTREIMB
01188      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTREIMB
01189                            ADDRESS OF                             ELTREIMB
01190          IOP-INPUT-OUTPUT-PARAMETERS.                             ELTREIMB
01191      SET IOP-RD TO TRUE.                                          ELTREIMB
01192      SET IOP-FCQ-NONE TO TRUE.                                    ELTREIMB
01193      SET IOP-KVQ-EQ TO TRUE.                                      ELTREIMB
01194      SET IOP-STG-MODE-MOVE TO TRUE.                               ELTREIMB
01195      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELTREIMB
01196      MOVE SPACES TO IOP-AIX-DDNAME.                               ELTREIMB
01197      PERFORM LINK-TO-I-O-PGM.                                     ELTREIMB
01198      EJECT                                                        ELTREIMB
01199                                                                   ELTREIMB
01200                                                                   ELTREIMB
01201 ************************************************************      ELTREIMB
01202 *                                                          *      ELTREIMB
01203 *        LINK TO I O PGM                                   *      ELTREIMB
01204 *                                                          *      ELTREIMB
01205 ************************************************************      ELTREIMB
01206  LINK-TO-I-O-PGM.                                                 ELTREIMB
01207      EXEC CICS LINK PROGRAM ('ELUIOPGM')                          ELTREIMB
01208           COMMAREA (DFHCOMMAREA)                                  ELTREIMB
01209           END-EXEC.                                               ELTREIMB
01210      IF IOP-RC-OK                                                 ELTREIMB
01211          PERFORM ESTABLISH-ADDRESSABILITY-OF-GC                   ELTREIMB
01212      ELSE IF IOP-RC-NOTFND                                        ELTREIMB
01213          PERFORM SIGNAL-NOT-FOUND-GCTAB                           ELTREIMB
01214      ELSE                                                         ELTREIMB
01215          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTREIMB
01216                                                                   ELTREIMB
01217                                                                   ELTREIMB
01218 ************************************************************      ELTREIMB
01219 *                                                          *      ELTREIMB
01220 *        SIGNAL CRITICAL IO ERROR                          *      ELTREIMB
01221 *                                                          *      ELTREIMB
01222 ************************************************************      ELTREIMB
01223  SIGNAL-CRITICAL-IO-ERROR.                                        ELTREIMB
01224      SET CIA-AB-CRITIO TO TRUE.                                   ELTREIMB
01225      PERFORM SIGNAL-ABEND.                                        ELTREIMB
01226                                                                   ELTREIMB
01227                                                                   ELTREIMB
01228 ************************************************************      ELTREIMB
01229 *                                                          *      ELTREIMB
01230 *        SIGNAL NOT FOUND GCTAB                            *      ELTREIMB
01231 *                                                          *      ELTREIMB
01232 ************************************************************      ELTREIMB
01233  SIGNAL-NOT-FOUND-GCTAB.                                          ELTREIMB
01234      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTREIMB
01235      PERFORM SIGNAL-ABEND.                                        ELTREIMB
01236                                                                   ELTREIMB
01237                                                                   ELTREIMB
01238 ************************************************************      ELTREIMB
01239 *                                                          *      ELTREIMB
01240 *        ESTABLISH ADDRESSABILITY OF GCCP RECORD           *      ELTREIMB
01241 *                                                          *      ELTREIMB
01242 ************************************************************      ELTREIMB
01243  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTREIMB
01244      SET ADDRESS OF GCCP-TABULAR-REC TO IOP-REC-PTR.              ELTREIMB
01245      SET IOP-REC-PTR TO NULL.                                     ELTREIMB
01246                                                                   ELTREIMB
01247 ************************************************************      ELTREIMB
01248 ************************************************************      ELTREIMB
01249                                                                   ELTREIMB
01250 ************************************************************      ELTREIMB
01251 *                                                          *      ELTREIMB
01252 *        TRANSLATE BS INVEST AND IND                       *      ELTREIMB
01253 *                                                          *      ELTREIMB
01254 ************************************************************      ELTREIMB
01255  TRANSLATE-BS-INVEST-AND-IND.                                     ELTREIMB
01256      INITIALIZE WS-PERIOD-NEEDED-SW                               ELTREIMB
01257                 TCAR-FROM-AREA.                                   ELTREIMB
01258      SET ADDITIONAL-TEXT TO TRUE.                                 ELTREIMB
01259      MOVE 1 TO TCAR-FROM-SUB.                                     ELTREIMB
01260 *    PERFORM GET-INVESTIGATION-FIXED-TEXT.                        ELTREIMB
01261      MOVE 'RS-BS-IND'  TO                                         ELTREIMB
01262          CMF-ELEMENT-SYSTEM-NAME.                                 ELTREIMB
01263      MOVE GSS-RS-BS-IND (GSS-INDEX) TO                            ELTREIMB
01264          CMF-CODE-VALUE.                                          ELTREIMB
01265      PERFORM LINK-TO-TRANSLATOR.                                  ELTREIMB
01266      SET CMF-DESCR-IDX TO 1.                                      ELTREIMB
01267      PERFORM MOVE-A-LINE UNTIL                                    ELTREIMB
01268          CMF-DESCR-IDX > CMF-NBR-DESCR-LINES.                     ELTREIMB
01269      ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
01270      PERFORM GET-CONNECTING-FIXED-TEXT.                           ELTREIMB
01271      SET PERIOD-NEEDED                                            ELTREIMB
01272          BLANK-LINE-NEEDED TO TRUE.                               ELTREIMB
01273      PERFORM TRANSLATE-MEMBER-RELATIONSHIPX.                      ELTREIMB
01274      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTREIMB
01275      EJECT                                                        ELTREIMB
01276                                                                   ELTREIMB
01277 ************************************************************      ELTREIMB
01278 *                                                          *      ELTREIMB
01279 *      TRANSLATE BS DOLLAR MIN AND QUALIFIER SENTENCE      *      ELTREIMB
01280 *                                                          *      ELTREIMB
01281 ************************************************************      ELTREIMB
01282  TRANSLATE-BS-INI-QUAL-MIN-SENT.                                  ELTREIMB
01283      INITIALIZE TCAR-FROM-AREA                                    ELTREIMB
01284                 ADDITIONAL-TEXT-SW.                               ELTREIMB
01285      MOVE 1 TO TCAR-FROM-SUB.                                     ELTREIMB
01286      IF (GSS-RS-BS-INI-DOL-QUAL (GSS-INDEX) NOT EQUAL ZERO        ELTREIMB
01287                  AND SPACES AND LOW-VALUES)                       ELTREIMB
01288         IF GSS-RS-BS-INI-DOL-MIN (GSS-INDEX) NOT EQUAL ZERO       ELTREIMB
01289             SET FULL-SENTENCE TO TRUE                             ELTREIMB
01290             PERFORM CREATE-BS-MIN-QUAL-FULL-SENT                  ELTREIMB
01291             INITIALIZE FULL-PART-SW                               ELTREIMB
01292         ELSE                                                      ELTREIMB
01293            SET PART-SENTENCE TO TRUE                              ELTREIMB
01294            PERFORM CREATE-BS-MIN-QUAL-PART-SENT                   ELTREIMB
01295            INITIALIZE FULL-PART-SW                                ELTREIMB
01296         END-IF                                                    ELTREIMB
01297      ELSE                                                         ELTREIMB
01298         CONTINUE                                                  ELTREIMB
01299      END-IF.                                                      ELTREIMB
01300      EJECT                                                        ELTREIMB
01301                                                                   ELTREIMB
01302 ************************************************************      ELTREIMB
01303 *                                                          *      ELTREIMB
01304 *        TRANSLATE BS DOLL MIN AND QUAL PART SENTENCE      *      ELTREIMB
01305 *                                                          *      ELTREIMB
01306 ************************************************************      ELTREIMB
01307  CREATE-BS-MIN-QUAL-PART-SENT.                                    ELTREIMB
01308      PERFORM GET-QUAL-FIXED-TEXT.                                 ELTREIMB
01309      PERFORM GET-IS-TEXT.                                         ELTREIMB
01310      SET PERIOD-NEEDED                                            ELTREIMB
01311          BLANK-LINE-NEEDED TO TRUE.                               ELTREIMB
01312      MOVE 'RS-BS-INI-DOL-QUAL'  TO                                ELTREIMB
01313          CMF-ELEMENT-SYSTEM-NAME.                                 ELTREIMB
01314      MOVE GSS-RS-BS-INI-DOL-QUAL (GSS-INDEX) TO                   ELTREIMB
01315          CMF-CODE-VALUE.                                          ELTREIMB
01316      PERFORM LINK-TO-TRANSLATOR.                                  ELTREIMB
01317      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTREIMB
01318                                                                   ELTREIMB
01319 ************************************************************      ELTREIMB
01320 *                                                          *      ELTREIMB
01321 *        TRANSLATE BS DOLL INI AND QUAL FULL SENTENCE      *      ELTREIMB
01322 *                                                          *      ELTREIMB
01323 ************************************************************      ELTREIMB
01324  CREATE-BS-MIN-QUAL-FULL-SENT.                                    ELTREIMB
01325      INITIALIZE WS-PERIOD-NEEDED-SW.                              ELTREIMB
01326      PERFORM GET-QUAL-FIXED-TEXT.                                 ELTREIMB
01327      PERFORM GET-MINIMUM-TEXT.                                    ELTREIMB
01328      MOVE GSS-RS-BS-INI-DOL-MIN (GSS-INDEX) TO                    ELTREIMB
01329          WS-DOLLAR-AMT.                                           ELTREIMB
01330      MOVE WS-DOLLAR-AMT TO TCAR-FROM-LINE (TCAR-FROM-SUB).        ELTREIMB
01331      ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
01332      PERFORM GET-IS-TEXT.                                         ELTREIMB
01333      SET PERIOD-NEEDED                                            ELTREIMB
01334          BLANK-LINE-NEEDED TO TRUE.                               ELTREIMB
01335      MOVE 'RS-BS-INI-DOL-QUAL'  TO                                ELTREIMB
01336          CMF-ELEMENT-SYSTEM-NAME.                                 ELTREIMB
01337      MOVE GSS-RS-BS-INI-DOL-QUAL (GSS-INDEX) TO                   ELTREIMB
01338          CMF-CODE-VALUE.                                          ELTREIMB
01339      PERFORM LINK-TO-TRANSLATOR.                                  ELTREIMB
01340      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTREIMB
01341                                                                   ELTREIMB
01342 ************************************************************      ELTREIMB
01343 *                                                          *      ELTREIMB
01344 *      TRANSLATE BS SUBSEQUENT INVESTIGATION SENTENCE      *      ELTREIMB
01345 *                                                          *      ELTREIMB
01346 ************************************************************      ELTREIMB
01347  TRANSLATE-BS-SUB-QUAL-INV.                                       ELTREIMB
01348      INITIALIZE TCAR-FROM-AREA                                    ELTREIMB
01349                 ADDITIONAL-TEXT-SW.                               ELTREIMB
01350      MOVE 1 TO TCAR-FROM-SUB.                                     ELTREIMB
01351      IF GSS-RS-BS-SUB-DOL-QUAL (GSS-INDEX) NOT EQUAL ZERO         ELTREIMB
01352         IF GSS-RS-BS-SUB-DOL-MIN (GSS-INDEX) NOT EQUAL ZERO       ELTREIMB
01353             SET FULL-SENTENCE TO TRUE                             ELTREIMB
01354             PERFORM CREATE-BS-SUB-QUAL-FULL-SENT                  ELTREIMB
01355             INITIALIZE FULL-PART-SW                               ELTREIMB
01356         ELSE                                                      ELTREIMB
01357            SET PART-SENTENCE TO TRUE                              ELTREIMB
01358            PERFORM CREATE-BS-SUB-QUAL-PART-SENT                   ELTREIMB
01359            INITIALIZE FULL-PART-SW                                ELTREIMB
01360         END-IF                                                    ELTREIMB
01361      ELSE                                                         ELTREIMB
01362         CONTINUE                                                  ELTREIMB
01363      END-IF.                                                      ELTREIMB
01364      EJECT                                                        ELTREIMB
01365                                                                   ELTREIMB
01366 ************************************************************      ELTREIMB
01367 *                                                          *      ELTREIMB
01368 *        CREATE BS SUB MIN AND QUAL FULL SENTENCE          *      ELTREIMB
01369 *                                                          *      ELTREIMB
01370 ************************************************************      ELTREIMB
01371  CREATE-BS-SUB-QUAL-FULL-SENT.                                    ELTREIMB
01372      PERFORM GET-QUAL-FIXED-TEXT.                                 ELTREIMB
01373      PERFORM GET-MINIMUM-TEXT.                                    ELTREIMB
01374      MOVE GSS-RS-BS-SUB-DOL-MIN (GSS-INDEX) TO                    ELTREIMB
01375          WS-DOLLAR-AMT.                                           ELTREIMB
01376      MOVE WS-DOLLAR-AMT TO TCAR-FROM-LINE (TCAR-FROM-SUB).        ELTREIMB
01377      ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
01378      PERFORM GET-IS-TEXT.                                         ELTREIMB
01379      MOVE 'RS-BS-SUB-DOL-QUAL'  TO                                ELTREIMB
01380          CMF-ELEMENT-SYSTEM-NAME.                                 ELTREIMB
01381      MOVE GSS-RS-BS-SUB-DOL-QUAL (GSS-INDEX) TO                   ELTREIMB
01382          CMF-CODE-VALUE.                                          ELTREIMB
01383      PERFORM LINK-TO-TRANSLATOR.                                  ELTREIMB
01384      PERFORM MOVE-A-LINE UNTIL                                    ELTREIMB
01385          CMF-DESCR-IDX > CMF-NBR-DESCR-LINES.                     ELTREIMB
01386      PERFORM GET-CONNECTING-FIXED-TEXT.                           ELTREIMB
01387      SET PERIOD-NEEDED                                            ELTREIMB
01388          BLANK-LINE-NEEDED TO TRUE.                               ELTREIMB
01389      MOVE 'RS-BC-SUB-CLAIM-DESCRPTOR'  TO                         ELTREIMB
01390          CMF-ELEMENT-SYSTEM-NAME.                                 ELTREIMB
01391      MOVE GSS-RS-BC-SUB-CLAIM-DESCRPTOR (GSS-INDEX) TO            ELTREIMB
01392          CMF-CODE-VALUE.                                          ELTREIMB
01393      PERFORM LINK-TO-TRANSLATOR.                                  ELTREIMB
01394      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTREIMB
01395                                                                   ELTREIMB
01396 ************************************************************      ELTREIMB
01397 *                                                          *      ELTREIMB
01398 *        TRANSLATE BS DOLL SUB AND QUAL PART SENTENCE      *      ELTREIMB
01399 *                                                          *      ELTREIMB
01400 ************************************************************      ELTREIMB
01401  CREATE-BS-SUB-QUAL-PART-SENT.                                    ELTREIMB
01402      PERFORM GET-SUB-QUAL-FIXED-TEXT.                             ELTREIMB
01403      PERFORM GET-IS-TEXT.                                         ELTREIMB
01404      SET PERIOD-NEEDED                                            ELTREIMB
01405          BLANK-LINE-NEEDED TO TRUE.                               ELTREIMB
01406      MOVE 'RS-BS-SUB-DOL-QUAL'  TO                                ELTREIMB
01407          CMF-ELEMENT-SYSTEM-NAME.                                 ELTREIMB
01408      MOVE GSS-RS-BS-SUB-DOL-QUAL (GSS-INDEX) TO                   ELTREIMB
01409          CMF-CODE-VALUE.                                          ELTREIMB
01410      PERFORM LINK-TO-TRANSLATOR.                                  ELTREIMB
01411      SET CMF-DESCR-IDX TO 1.                                      ELTREIMB
01412      PERFORM MOVE-A-LINE UNTIL                                    ELTREIMB
01413         CMF-DESCR-IDX > CMF-NBR-DESCR-LINES.                      ELTREIMB
01414      PERFORM GET-CONNECTING-FIXED-TEXT.                           ELTREIMB
01415      SET PERIOD-NEEDED                                            ELTREIMB
01416          BLANK-LINE-NEEDED TO TRUE.                               ELTREIMB
01417      MOVE 'RS-BS-SUB-CLAIM-DESCRPTOR'  TO                         ELTREIMB
01418          CMF-ELEMENT-SYSTEM-NAME.                                 ELTREIMB
01419      MOVE GSS-RS-BS-SUB-CLAIM-DESCRPTOR (GSS-INDEX) TO            ELTREIMB
01420          CMF-CODE-VALUE.                                          ELTREIMB
01421      PERFORM LINK-TO-TRANSLATOR.                                  ELTREIMB
01422      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTREIMB
01423                                                                   ELTREIMB
01424 ************************************************************      ELTREIMB
01425 *                                                          *      ELTREIMB
01426 *        TRANSLATE BS INVESTIGATION METHOD SENTENCE        *      ELTREIMB
01427 *                                                          *      ELTREIMB
01428 ************************************************************      ELTREIMB
01429  TRANSLATE-BS-INV-METH.                                           ELTREIMB
01430      MOVE SPACES TO TCAR-FROM-AREA.                               ELTREIMB
01431      MOVE +1 TO TCAR-FROM-SUB.                                    ELTREIMB
01432      MOVE WS-INV-PERFORMED-AS                                     ELTREIMB
01433               TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELTREIMB
01434      ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
01435      MOVE 'RS-BS-INVEST-METHOD' TO                                ELTREIMB
01436          CMF-ELEMENT-SYSTEM-NAME.                                 ELTREIMB
01437      MOVE GSS-RS-BS-INVEST-METHOD (GSS-INDEX) TO                  ELTREIMB
01438          CMF-CODE-VALUE.                                          ELTREIMB
01439      SET PERIOD-NEEDED                                            ELTREIMB
01440          BLANK-LINE-NEEDED TO TRUE.                               ELTREIMB
01441      PERFORM LINK-TO-TRANSLATOR.                                  ELTREIMB
01442      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTREIMB
01443                                                                   ELTREIMB
01444 ************************************************************      ELTREIMB
01445 *                                                          *      ELTREIMB
01446 *        TRANSLATE BS DENIAL PARAMETER                     *      ELTREIMB
01447 *                                                          *      ELTREIMB
01448 ************************************************************      ELTREIMB
01449  TRANSLATE-BS-DENIAL-PARM.                                        ELTREIMB
01450      SET PERIOD-NEEDED TO TRUE.                                   ELTREIMB
01451      MOVE SPACES TO TCAR-FROM-AREA.                               ELTREIMB
01452      MOVE +1 TO TCAR-FROM-SUB.                                    ELTREIMB
01453      MOVE WS-DENIAL-PHRASE                                        ELTREIMB
01454               TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELTREIMB
01455      ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
01456      MOVE WS-DENIAL-PHRASE2                                       ELTREIMB
01457               TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELTREIMB
01458      ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
01459      MOVE 'RS-BS-DENIAL-PARAMETER' TO                             ELTREIMB
01460          CMF-ELEMENT-SYSTEM-NAME.                                 ELTREIMB
01461      MOVE GSS-RS-BS-DENIAL-PARAMETER (GSS-INDEX) TO               ELTREIMB
01462          CMF-CODE-VALUE.                                          ELTREIMB
01463      PERFORM LINK-TO-TRANSLATOR.                                  ELTREIMB
01464      SET PERIOD-NEEDED                                            ELTREIMB
01465          BLANK-LINE-NEEDED TO TRUE.                               ELTREIMB
01466      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTREIMB
01467                                                                   ELTREIMB
01468 ************************************************************      ELTREIMB
01469 *                                                          *      ELTREIMB
01470 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTREIMB
01471 *                                                          *      ELTREIMB
01472 ************************************************************      ELTREIMB
01473  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTREIMB
01474      PERFORM INITIALIZE-CMOUT.                                    ELTREIMB
01475      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTREIMB
01476      EJECT                                                        ELTREIMB
01477                                                                   ELTREIMB
01478                                                                   ELTREIMB
01479 ************************************************************      ELTREIMB
01480 *                                                          *      ELTREIMB
01481 *        PREPARE TEXT FOR OUTPUT                           *      ELTREIMB
01482 *                                                          *      ELTREIMB
01483 ************************************************************      ELTREIMB
01484  PREPARE-TEXT-FOR-OUTPUT.                                         ELTREIMB
01485      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTREIMB
01486          UNTIL CMF-DESCR-IDX                                      ELTREIMB
01487                                    GREATER THAN                   ELTREIMB
01488              CMF-NBR-DESCR-LINES.                                 ELTREIMB
01489      EJECT                                                        ELTREIMB
01490                                                                   ELTREIMB
01491                                                                   ELTREIMB
01492 ************************************************************      ELTREIMB
01493 *                                                          *      ELTREIMB
01494 *        INITIALIZE CMOUT                                  *      ELTREIMB
01495 *                                                          *      ELTREIMB
01496 ************************************************************      ELTREIMB
01497  INITIALIZE-CMOUT.                                                ELTREIMB
01498      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTREIMB
01499      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTREIMB
01500          ADDRESS OF CMF-DESCR.                                    ELTREIMB
01501      SET CMF-DESCR-IDX TO 1.                                      ELTREIMB
01502      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTREIMB
01503                                                                   ELTREIMB
01504                                                                   ELTREIMB
01505 ************************************************************      ELTREIMB
01506 *                                                          *      ELTREIMB
01507 *        MOVE CMF TEXT TO OUTPUT                           *      ELTREIMB
01508 *                                                          *      ELTREIMB
01509 ************************************************************      ELTREIMB
01510  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTREIMB
01511      PERFORM MOVE-A-LINE.                                         ELTREIMB
01512      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTREIMB
01513          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTREIMB
01514      IF TCAR-FROM-SUB GREATER THAN 20                             ELTREIMB
01515               OR CMF-DESCR-IDX GREATER THAN                       ELTREIMB
01516          CMF-NBR-DESCR-LINES                                      ELTREIMB
01517          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTREIMB
01518                                                                   ELTREIMB
01519                                                                   ELTREIMB
01520 ************************************************************      ELTREIMB
01521 *                                                          *      ELTREIMB
01522 *        FINISH CODES MANUAL TEXT                          *      ELTREIMB
01523 *                                                          *      ELTREIMB
01524 ************************************************************      ELTREIMB
01525  FINISH-CODES-MANUAL-TEXT.                                        ELTREIMB
01526      SET DONE-PROCESSING TO TRUE.                                 ELTREIMB
01527      IF PERIOD-NEEDED                                             ELTREIMB
01528          PERFORM GET-AND-MOVE-PERIOD.                             ELTREIMB
01529                                                                   ELTREIMB
01530                                                                   ELTREIMB
01531 ************************************************************      ELTREIMB
01532 *                                                          *      ELTREIMB
01533 *        GET AND MOVE PERIOD                               *      ELTREIMB
01534 *                                                          *      ELTREIMB
01535 ************************************************************      ELTREIMB
01536  GET-AND-MOVE-PERIOD.                                             ELTREIMB
01537      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTREIMB
01538          (TCAR-FROM-SUB).                                         ELTREIMB
01539                                                                   ELTREIMB
01540                                                                   ELTREIMB
01541 ************************************************************      ELTREIMB
01542 *                                                          *      ELTREIMB
01543 *        SAVE LAST LINE                                    *      ELTREIMB
01544 *                                                          *      ELTREIMB
01545 ************************************************************      ELTREIMB
01546  SAVE-LAST-LINE.                                                  ELTREIMB
01547      MOVE +1 TO TCAR-FROM-SUB.                                    ELTREIMB
01548      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTREIMB
01549         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTREIMB
01550      ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
01551      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTREIMB
01552                                                                   ELTREIMB
01553                                                                   ELTREIMB
01554 ************************************************************      ELTREIMB
01555 *                                                          *      ELTREIMB
01556 *        OUTPUT LAST LINE                                  *      ELTREIMB
01557 *                                                          *      ELTREIMB
01558 ************************************************************      ELTREIMB
01559  OUTPUT-LAST-LINE.                                                ELTREIMB
01560      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTREIMB
01561          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTREIMB
01562      IF BLANK-LINE-NEEDED                                         ELTREIMB
01563          PERFORM CREATE-A-BLANK-LINE.                             ELTREIMB
01564                                                                   ELTREIMB
01565                                                                   ELTREIMB
01566 ************************************************************      ELTREIMB
01567 *                                                          *      ELTREIMB
01568 *        CREATE A BLANK LINE                               *      ELTREIMB
01569 *                                                          *      ELTREIMB
01570 ************************************************************      ELTREIMB
01571  CREATE-A-BLANK-LINE.                                             ELTREIMB
01572      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTREIMB
01573      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTREIMB
01574                                                                   ELTREIMB
01575                                                                   ELTREIMB
01576 ************************************************************      ELTREIMB
01577 *                                                          *      ELTREIMB
01578 *        MOVE A LINE                                       *      ELTREIMB
01579 *                                                          *      ELTREIMB
01580 ************************************************************      ELTREIMB
01581  MOVE-A-LINE.                                                     ELTREIMB
01582      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTREIMB
01583          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTREIMB
01584      SET CMF-DESCR-IDX UP BY 1.                                   ELTREIMB
01585      ADD 1 TO TCAR-FROM-SUB.                                      ELTREIMB
01586      EJECT                                                        ELTREIMB
01587                                                                   ELTREIMB
01588                                                                   ELTREIMB
01589 ************************************************************      ELTREIMB
01590 *                                                          *      ELTREIMB
01591 *        REFORMAT AND WRITE TEXT                           *      ELTREIMB
01592 *                                                          *      ELTREIMB
01593 ************************************************************      ELTREIMB
01594  REFORMAT-AND-WRITE-TEXT.                                         ELTREIMB
01595      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTREIMB
01596      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTREIMB
01597      PERFORM UNSTRING-TEXT.                                       ELTREIMB
01598      MOVE +1 TO TCAR-FROM-SUB.                                    ELTREIMB
01599      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTREIMB
01600      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTREIMB
01601          UNTIL COF-NBR-DTL-LINES GREATER                          ELTREIMB
01602                                   TCAR-OUTPUT-FIELDS-USED -       ELTREIMB
01603              1.                                                   ELTREIMB
01604      PERFORM DISPOSE-OF-LAST-LINE.                                ELTREIMB
01605      PERFORM LINK-TO-OUTPUT.                                      ELTREIMB
01606                                                                   ELTREIMB
01607                                                                   ELTREIMB
01608 ************************************************************      ELTREIMB
01609 *                                                          *      ELTREIMB
01610 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTREIMB
01611 *                                                          *      ELTREIMB
01612 ************************************************************      ELTREIMB
01613  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTREIMB
01614      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTREIMB
01615           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTREIMB
01616      ADD +1 TO TCAR-FROM-SUB.                                     ELTREIMB
01617      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTREIMB
01618      EJECT                                                        ELTREIMB
01619                                                                   ELTREIMB
01620                                                                   ELTREIMB
01621 ************************************************************      ELTREIMB
01622 *                                                          *      ELTREIMB
01623 *        UNSTRING TEXT                                     *      ELTREIMB
01624 *                                                          *      ELTREIMB
01625 ************************************************************      ELTREIMB
01626  UNSTRING-TEXT.                                                   ELTREIMB
01627      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTREIMB
01628      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTREIMB
01629      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTREIMB
01630      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTREIMB
01631      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTREIMB
01632      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTREIMB
01633      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTREIMB
01634      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTREIMB
01635      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTREIMB
01636      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTREIMB
01637      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTREIMB
01638      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTREIMB
01639      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTREIMB
01640      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTREIMB
01641      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTREIMB
01642      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTREIMB
01643      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTREIMB
01644      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTREIMB
01645      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTREIMB
01646      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTREIMB
01647      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTREIMB
01648      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTREIMB
01649      EJECT                                                        ELTREIMB
01650                                                                   ELTREIMB
01651                                                                   ELTREIMB
01652 ************************************************************      ELTREIMB
01653 *                                                          *      ELTREIMB
01654 *        LINK TO OUTPUT                                    *      ELTREIMB
01655 *                                                          *      ELTREIMB
01656 ************************************************************      ELTREIMB
01657  LINK-TO-OUTPUT.                                                  ELTREIMB
01658      EXEC CICS LINK                                               ELTREIMB
01659          PROGRAM ('ELUOUTPT')                                     ELTREIMB
01660          COMMAREA (DFHCOMMAREA)                                   ELTREIMB
01661          END-EXEC.                                                ELTREIMB
01662      EJECT                                                        ELTREIMB
01663                                                                   ELTREIMB
01664                                                                   ELTREIMB
01665 ************************************************************      ELTREIMB
01666 *                                                          *      ELTREIMB
01667 *        DISPOSE OF LAST LINE                              *      ELTREIMB
01668 *                                                          *      ELTREIMB
01669 ************************************************************      ELTREIMB
01670  DISPOSE-OF-LAST-LINE.                                            ELTREIMB
01671      IF NOT ADDITIONAL-TEXT                                       ELTREIMB
01672          PERFORM INITIALIZE-CONTINUED-SW.                         ELTREIMB
01673      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTREIMB
01674          PERFORM SAVE-LAST-LINE                                   ELTREIMB
01675      ELSE                                                         ELTREIMB
01676          PERFORM OUTPUT-LAST-LINE.                                ELTREIMB
01677                                                                   ELTREIMB
01678                                                                   ELTREIMB
01679 ************************************************************      ELTREIMB
01680 *                                                          *      ELTREIMB
01681 *        INITIALIZE CONTINUED SW                           *      ELTREIMB
01682 *                                                          *      ELTREIMB
01683 ************************************************************      ELTREIMB
01684  INITIALIZE-CONTINUED-SW.                                         ELTREIMB
01685      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTREIMB
