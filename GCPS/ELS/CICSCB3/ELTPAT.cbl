00001 *      LAST MAINTENANCE TIME: 11.52.00  DATE: 06/17/91            06/29/02
00002  IDENTIFICATION DIVISION.                                         ELTPAT  
00003                                                                      LV001
00004  PROGRAM-ID.         ELTPAT.                                      ELTPAT  
00005                                                                   ELTPAT  
00006  AUTHOR.             RICK BARILEAU.                               ELTPAT  
00007                                                                   ELTPAT  
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTPAT  
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELTPAT  
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTPAT  
00011                      233 N. MICHIGAN AVE                          ELTPAT  
00012                      CHICAGO, ILLINOIS 60601                      ELTPAT  
00013                                                                   ELTPAT  
00014  DATE-WRITTEN.       18-JUN-1987.                                 ELTPAT  
00015                                                                   ELTPAT  
00016  DATE-COMPILED.                                                   ELTPAT  
00017                                                                   ELTPAT  
00018  SECURITY.           COPYRIGHT 1986,                              ELTPAT  
00019                      HEALTH CARE SERVICE CORPORATION              ELTPAT  
00020      SKIP3                                                        ELTPAT  
00021  ENVIRONMENT DIVISION.                                            ELTPAT  
00022                                                                   ELTPAT  
00023  CONFIGURATION SECTION.                                           ELTPAT  
00024  SOURCE-COMPUTER.    IBM-3090.                                    ELTPAT  
00025  OBJECT-COMPUTER.    IBM-3090.                                    ELTPAT  
00026      EJECT                                                        ELTPAT  
00027 ******************************************************************ELTPAT  
00028 *                                                                *ELTPAT  
00029 *    COPYBOOK:   ELTPAT                                          *ELTPAT  
00030 *    DATE:       18-JUN-1987                                     *ELTPAT  
00031 *    AUTHOR:     RICK BARILEAU                                   *ELTPAT  
00032 *    FUNCTION:   THIS MODULE WILL GENERATE TEXT PERTAINING TO    *ELTPAT  
00033 *                THE PRE-ADMISSION TESTING PROGRAM.              *ELTPAT  
00034 *    NOTES:      X---                                            *ELTPAT  
00035 *                                                                *ELTPAT  
00036 ******************************************************************ELTPAT  
00037 *                                                                *ELTPAT  
00038 *                      MAINTENANCE HISTORY                       *ELTPAT  
00039 *                                                                *ELTPAT  
00040 *  MOD     DATE     BY  DRPT                ACTION               *ELTPAT  
00041 * ----- ----------- --- ----- ---------------------------------- *ELTPAT  
00042 * 01.00 18-JUN-1987 REB       CREATED                            *ELTPAT  
00043 *                                                                *ELTPAT  
00044 * 01.01 26-OCT-1990 JPB       CHANGED STORAGE MANAGEMENT         *ELTPAT  
00045 *                                                                *ELTPAT  
00046 * 01.02 12-NOV-1990 JPB       CHANGED REFERENCES TO GCG-PRE-ADM- *ELTPAT  
00047 *                             TESTING-PROGRAM TO RELECT NEW      *ELTPAT  
00048 *                             FIELD SIZE.                        *ELTPAT  
00049 *                                                                *ELTPAT  
00050 * 01.03 30-MAY-1991 JPB       ADDED TRANSLATION AND DISPLAY OF   *ELTPAT  
00051 *                             PARTICIPATION INDICATOR            *ELTPAT  
00052 *                             (PRE-ADM-TESTING-PROGRAM).         *ELTPAT  
00053 ******************************************************************ELTPAT  
00054                                                                   ELTPAT  
00055  DATA DIVISION.                                                   ELTPAT  
00056  WORKING-STORAGE SECTION.                                         ELTPAT  
00057  01  WS-MISC.                                                     ELTPAT  
00058      05  FILLER                   PIC X(27) VALUE                 ELTPAT  
00059      'WORKING STORAGE STARTS HERE'.                               ELTPAT  
00060                                                                   ELTPAT  
00061  01  WS-SWITCHES.                                                 ELTPAT  
00062      05  ADDITIONAL-TEXT-SWITCH   PIC X(01) VALUE SPACE.          ELTPAT  
00063          88  ADDITIONAL-TEXT                VALUE 'A'.            ELTPAT  
00064          88  BLANK-LINE-NEEDED              VALUE 'B'.            ELTPAT  
00065      05  CONTINUED-PROCESSING-SW  PIC X(01) VALUE SPACE.          ELTPAT  
00066          88  DONE-PROCESSING                VALUE 'D'.            ELTPAT  
00067          88  PROCESSING-CMF-TEXT            VALUE 'P'.            ELTPAT  
00068      05  WS-PERIOD-SWITCH         PIC X(01) VALUE 'N'.            ELTPAT  
00069          88  PERIOD-NEEDED                  VALUE 'Y'.            ELTPAT  
00070      05  WS-TABULAR-SWITCH        PIC X(01) VALUE 'N'.            ELTPAT  
00071          88  TABULAR-IS-UNDEFINED           VALUE 'Y'.            ELTPAT  
00072                                                                   ELTPAT  
00073 **************************************************************    ELTPAT  
00074 ***                   PROGRAM CONSTANTS                           ELTPAT  
00075 **************************************************************    ELTPAT  
00076      05  WS-GCCP                  PIC X(06) VALUE '#GCCP '.       ELTPAT  
00077      05  WS-GROUP                 PIC X(06) VALUE 'GROUP '.       ELTPAT  
00078      05  WS-INST                  PIC X(13) VALUE 'INSTITUTIONAL'.ELTPAT  
00079      05  WS-PROF                  PIC X(13) VALUE 'PROFESSIONAL '.ELTPAT  
00080      05  WS-SUPP                  PIC X(13) VALUE 'SUPPLEMENTAL '.ELTPAT  
00081      05  WS-INPATIENT             PIC X(10) VALUE 'INPATIENT;'.   ELTPAT  
00082      05  WS-INPATIENT-END         PIC X(10) VALUE 'INPATIENT.'.   ELTPAT  
00083      05  WS-OUTPATIENT            PIC X(11) VALUE 'OUTPATIENT.'.  ELTPAT  
00084                                                                   ELTPAT  
00085 **********************************************************        ELTPAT  
00086 ***                   HEADER LINE                                 ELTPAT  
00087 **********************************************************        ELTPAT  
00088      05  WS-HEADER-LINE.                                          ELTPAT  
00089          10  FILLER               PIC X(18) VALUE SPACES.         ELTPAT  
00090          10  FILLER               PIC X(30) VALUE                 ELTPAT  
00091          'PRE-ADMISSION TESTING PROGRAM '.                        ELTPAT  
00092          10  WS-HDR-LINE-BCBSMM   PIC X(13) VALUE SPACES.         ELTPAT  
00093          10  FILLER               PIC X(18) VALUE SPACES.         ELTPAT  
00094                                                                   ELTPAT  
00095 **************************************************************    ELTPAT  
00096 ***                   SCREEN BODY LINES                           ELTPAT  
00097 **************************************************************    ELTPAT  
00098  01  WS-SCREEN-LINE-AREA.                                         ELTPAT  
00099      05  WS-PARTICIPATION-LINE.                                   ELTPAT  
00100          10  FILLER               PIC X(79) VALUE                 ELTPAT  
00101          'THE PRE-ADMISSION TESTING PROGRAM APPLIES TO '.         ELTPAT  
00102                                                                   ELTPAT  
00103      05  WS-BCBSMM-IND-LINE.                                      ELTPAT  
00104          10  FILLER               PIC X(79) VALUE                 ELTPAT  
00105          'THE PRE-ADMISSION TESTING PROGRAM '.                    ELTPAT  
00106                                                                   ELTPAT  
00107      05  WS-ALT-PRICING-LINE-BC.                                  ELTPAT  
00108          10  FILLER               PIC X(26) VALUE                 ELTPAT  
00109          'THE ALTERNATE PRICING FOR '.                            ELTPAT  
00110          10  FILLER               PIC X(27) VALUE                 ELTPAT  
00111          'INSTITUTIONAL SERVICES IS '.                            ELTPAT  
00112          10  FILLER               PIC X(16) VALUE SPACES.         ELTPAT  
00113                                                                   ELTPAT  
00114      05  WS-ALT-PRICING-LINE-BS.                                  ELTPAT  
00115          10  FILLER               PIC X(26) VALUE                 ELTPAT  
00116          'THE ALTERNATE PRICING FOR '.                            ELTPAT  
00117          10  FILLER               PIC X(26) VALUE                 ELTPAT  
00118          'PROFESSIONAL SERVICES IS '.                             ELTPAT  
00119          10  FILLER               PIC X(17) VALUE SPACES.         ELTPAT  
00120                                                                   ELTPAT  
00121      05  WS-ALT-PRICING-LINE-MM.                                  ELTPAT  
00122          10  FILLER               PIC X(26) VALUE                 ELTPAT  
00123          'THE ALTERNATE PRICING FOR '.                            ELTPAT  
00124          10  FILLER               PIC X(26) VALUE                 ELTPAT  
00125          'SUPPLEMENTAL SERVICES IS '.                             ELTPAT  
00126          10  FILLER               PIC X(17) VALUE SPACES.         ELTPAT  
00127                                                                   ELTPAT  
00128      05  WS-BENE-REDUCT-LINE.                                     ELTPAT  
00129          10  FILLER               PIC X(47) VALUE                 ELTPAT  
00130          'DENIED OR REDUCED BENEFITS DUE TO THIS PROGRAM:'.       ELTPAT  
00131          10  FILLER               PIC X(32) VALUE SPACES.         ELTPAT  
00132                                                                   ELTPAT  
00133      05  WS-SPILL-OVER-LINE.                                      ELTPAT  
00134          10  FILLER               PIC X(79) VALUE                 ELTPAT  
00135          'UNPAID SERVICES AFTER BASIC BENEFITS REDUCTION ARE '.   ELTPAT  
00136                                                                   ELTPAT  
00137 **************************************************************    ELTPAT  
00138 ** SPECIAL MESSAGE FOR THE VOLUNTARY AND NOT APPLICABLE CASES     ELTPAT  
00139 **************************************************************    ELTPAT  
00140      05  WS-NOT-APPLICABLE-MSG.                                   ELTPAT  
00141          10  FILLER               PIC  X(79) VALUE                ELTPAT  
00142          'THE PRE-ADMISSION TESTING PROGRAM IS NOT APPLICABLE.'.  ELTPAT  
00143                                                                   ELTPAT  
00144      05  WS-VOLUNTARY-MSG.                                        ELTPAT  
00145          10  FILLER               PIC  X(79) VALUE                ELTPAT  
00146          'THE PRE-ADMISSION TESTING PROGRAM IS VOLUNTARY.'.       ELTPAT  
00147                                                                   ELTPAT  
00148      05  WS-DISCLAIMER-MSG.                                       ELTPAT  
00149          10  FILLER               PIC  X(79) VALUE                ELTPAT  
00150          '*** SUBJECT TO OTHER CONTRACT LIMITATIONS ***'.         ELTPAT  
00151                                                                   ELTPAT  
00152      05  WS-NOT-APPLY-TO-LOB-BC.                                  ELTPAT  
00153          10  FILLER               PIC X(53) VALUE                 ELTPAT  
00154          'THE PRE-ADMISSION TESTING PROGRAM DOES NOT APPLY FOR'.  ELTPAT  
00155          10  FILLER               PIC X(26) VALUE                 ELTPAT  
00156          ' INSTITUTIONAL BENEFITS.'.                              ELTPAT  
00157                                                                   ELTPAT  
00158      05  WS-NOT-APPLY-TO-LOB-BS.                                  ELTPAT  
00159          10  FILLER               PIC X(53) VALUE                 ELTPAT  
00160          'THE PRE-ADMISSION TESTING PROGRAM DOES NOT APPLY FOR'.  ELTPAT  
00161          10  FILLER               PIC X(26) VALUE                 ELTPAT  
00162          ' PROFESSIONAL BENEFITS.'.                               ELTPAT  
00163                                                                   ELTPAT  
00164      05  WS-NOT-APPLY-TO-LOB-MM.                                  ELTPAT  
00165          10  FILLER               PIC X(53) VALUE                 ELTPAT  
00166          'THE PRE-ADMISSION TESTING PROGRAM DOES NOT APPLY FOR'.  ELTPAT  
00167          10  FILLER               PIC X(26) VALUE                 ELTPAT  
00168          ' SUPPLEMENTAL BENEFITS.'.                               ELTPAT  
00169                                                                   ELTPAT  
00170                                                                   ELTPAT  
00171 /                                                                 ELTPAT  
00172  LINKAGE SECTION.                                                 ELTPAT  
00173  01  DFHCOMMAREA.                                                 ELTPAT  
00174      COPY ELSCOMMC.                                               ELTPAT  
00175 /                                                                 ELTPAT  
00176      COPY ELSCIA2C.                                               ELTPAT  
00177 /                                                                 ELTPAT  
00178      COPY ELSCMDSC.                                               ELTPAT  
00179 /                                                                 ELTPAT  
00180      COPY ELSCMIFC.                                               ELTPAT  
00181 /                                                                 ELTPAT  
00182      COPY ELSIOPMC.                                               ELTPAT  
00183 /                                                                 ELTPAT  
00184      COPY ELSKEYSC.                                               ELTPAT  
00185 /                                                                 ELTPAT  
00186      COPY ELSOUTPC.                                               ELTPAT  
00187 /                                                                 ELTPAT  
00188      COPY ELSSRTPC.                                               ELTPAT  
00189 /                                                                 ELTPAT  
00190      COPY ELSTCWAC.                                               ELTPAT  
00191 /                                                                 ELTPAT  
00192      COPY ELSSSCBC.                                               ELTPAT  
00193 /                                                                 ELTPAT  
00194  01  GROUP-SPECIFIC-REC.                                          ELTPAT  
00195      COPY GCGROUPC.                                               ELTPAT  
00196 /                                                                 ELTPAT  
00197  01  GCCP-TABULAR-REC-AREA.                                       ELTPAT  
00198      COPY GCTGCCPC.                                               ELTPAT  
00199      EJECT                                                        ELTPAT  
00200  PROCEDURE DIVISION.                                              ELTPAT  
00201 ************************************************************      ELTPAT  
00202 *                                                          *      ELTPAT  
00203 *                    PROCEDURE DIVISION                    *      ELTPAT  
00204 *                                                          *      ELTPAT  
00205 ************************************************************      ELTPAT  
00206                                                                   ELTPAT  
00207                                                                   ELTPAT  
00208 ************************************************************      ELTPAT  
00209 *                                                          *      ELTPAT  
00210 *        PRE-ADMISSION TESTING                             *      ELTPAT  
00211 *                                                          *      ELTPAT  
00212 ************************************************************      ELTPAT  
00213  PRE-ADMISSION-TESTING.                                           ELTPAT  
00214      PERFORM INITIALIZATION-ROUTINE.                              ELTPAT  
00215      PERFORM PROCESS.                                             ELTPAT  
00216      GOBACK.                                                      ELTPAT  
00217                                                                   ELTPAT  
00218                                                                   ELTPAT  
00219 ************************************************************      ELTPAT  
00220 *                                                          *      ELTPAT  
00221 *        INITIALIZATION ROUTINE                            *      ELTPAT  
00222 *                                                          *      ELTPAT  
00223 ************************************************************      ELTPAT  
00224  INITIALIZATION-ROUTINE.                                          ELTPAT  
00225      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTPAT  
00226      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTPAT  
00227                                                                   ELTPAT  
00228                                                                   ELTPAT  
00229 ************************************************************      ELTPAT  
00230 *                                                          *      ELTPAT  
00231 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTPAT  
00232 *                                                          *      ELTPAT  
00233 ************************************************************      ELTPAT  
00234  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTPAT  
00235      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTPAT  
00236      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTPAT  
00237      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTPAT  
00238                                                                   ELTPAT  
00239                                                                   ELTPAT  
00240 ************************************************************      ELTPAT  
00241 *                                                          *      ELTPAT  
00242 *        CHECK FOR VALID COMMAREA                          *      ELTPAT  
00243 *                                                          *      ELTPAT  
00244 ************************************************************      ELTPAT  
00245  CHECK-FOR-VALID-COMMAREA.                                        ELTPAT  
00246      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTPAT  
00247          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTPAT  
00248                                                                   ELTPAT  
00249                                                                   ELTPAT  
00250 ************************************************************      ELTPAT  
00251 *                                                          *      ELTPAT  
00252 *        SIGNAL INVALID COMMAREA                           *      ELTPAT  
00253 *                                                          *      ELTPAT  
00254 ************************************************************      ELTPAT  
00255  SIGNAL-INVALID-COMMAREA.                                         ELTPAT  
00256      EXEC CICS ABEND                                              ELTPAT  
00257                ABCODE('EL01')                                     ELTPAT  
00258         END-EXEC.                                                 ELTPAT  
00259      EJECT                                                        ELTPAT  
00260                                                                   ELTPAT  
00261                                                                   ELTPAT  
00262 ************************************************************      ELTPAT  
00263 *                                                          *      ELTPAT  
00264 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTPAT  
00265 *                                                          *      ELTPAT  
00266 ************************************************************      ELTPAT  
00267  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTPAT  
00268      IF ECA-CIA-PTR = NULL                                        ELTPAT  
00269          PERFORM SIGNAL-INVALID-CIA                               ELTPAT  
00270      ELSE                                                         ELTPAT  
00271          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTPAT  
00272                                                                   ELTPAT  
00273                                                                   ELTPAT  
00274 ************************************************************      ELTPAT  
00275 *                                                          *      ELTPAT  
00276 *        SIGNAL INVALID CIA                                *      ELTPAT  
00277 *                                                          *      ELTPAT  
00278 ************************************************************      ELTPAT  
00279  SIGNAL-INVALID-CIA.                                              ELTPAT  
00280      EXEC CICS ABEND                                              ELTPAT  
00281                ABCODE('EL02')                                     ELTPAT  
00282         END-EXEC.                                                 ELTPAT  
00283      EJECT                                                        ELTPAT  
00284                                                                   ELTPAT  
00285                                                                   ELTPAT  
00286 ************************************************************      ELTPAT  
00287 *                                                          *      ELTPAT  
00288 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTPAT  
00289 *                                                          *      ELTPAT  
00290 ************************************************************      ELTPAT  
00291  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTPAT  
00292      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTPAT  
00293      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAT  
00294                            ADDRESS OF                             ELTPAT  
00295          SSB-SELECTOR-STATUS-CTL-BLK.                             ELTPAT  
00296      IF CIA-RC-PTR-NULL                                           ELTPAT  
00297          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAT  
00298                                                                   ELTPAT  
00299                                                                   ELTPAT  
00300 ************************************************************      ELTPAT  
00301 *                                                          *      ELTPAT  
00302 *        SIGNAL UNALLOC AREA ERROR                         *      ELTPAT  
00303 *                                                          *      ELTPAT  
00304 ************************************************************      ELTPAT  
00305  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTPAT  
00306      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTPAT  
00307      PERFORM SIGNAL-ABEND.                                        ELTPAT  
00308                                                                   ELTPAT  
00309                                                                   ELTPAT  
00310 ************************************************************      ELTPAT  
00311 *                                                          *      ELTPAT  
00312 *        SIGNAL ABEND                                      *      ELTPAT  
00313 *                                                          *      ELTPAT  
00314 ************************************************************      ELTPAT  
00315  SIGNAL-ABEND.                                                    ELTPAT  
00316      EXEC CICS ABEND                                              ELTPAT  
00317                ABCODE(CIA-ABCODE)                                 ELTPAT  
00318         END-EXEC.                                                 ELTPAT  
00319      EJECT                                                        ELTPAT  
00320                                                                   ELTPAT  
00321                                                                   ELTPAT  
00322 ************************************************************      ELTPAT  
00323 *                                                          *      ELTPAT  
00324 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTPAT  
00325 *                                                          *      ELTPAT  
00326 ************************************************************      ELTPAT  
00327  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTPAT  
00328      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTPAT  
00329      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTPAT  
00330      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTPAT  
00331      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTPAT  
00332      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTPAT  
00333      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTPAT  
00334      PERFORM ESTABLISH-ADDRESSABILITY-OF-CS.                      ELTPAT  
00335      EJECT                                                        ELTPAT  
00336                                                                   ELTPAT  
00337                                                                   ELTPAT  
00338 ************************************************************      ELTPAT  
00339 *                                                          *      ELTPAT  
00340 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTPAT  
00341 *                                                          *      ELTPAT  
00342 ************************************************************      ELTPAT  
00343  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTPAT  
00344      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTPAT  
00345      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAT  
00346                            ADDRESS OF                             ELTPAT  
00347                                                                   ELTPAT  
00348          CMF-CODES-MANUAL-INTERFACE.                              ELTPAT  
00349      IF CIA-RC-PTR-NULL                                           ELTPAT  
00350          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAT  
00351                                                                   ELTPAT  
00352                                                                   ELTPAT  
00353 ************************************************************      ELTPAT  
00354 *                                                          *      ELTPAT  
00355 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTPAT  
00356 *                                                          *      ELTPAT  
00357 ************************************************************      ELTPAT  
00358  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTPAT  
00359      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTPAT  
00360      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAT  
00361                            ADDRESS OF                             ELTPAT  
00362          COF-OUTPUT-INTERFACE.                                    ELTPAT  
00363      IF CIA-RC-PTR-NULL                                           ELTPAT  
00364          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAT  
00365      EJECT                                                        ELTPAT  
00366                                                                   ELTPAT  
00367                                                                   ELTPAT  
00368 ************************************************************      ELTPAT  
00369 *                                                          *      ELTPAT  
00370 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTPAT  
00371 *                                                          *      ELTPAT  
00372 ************************************************************      ELTPAT  
00373  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTPAT  
00374      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTPAT  
00375      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAT  
00376                            ADDRESS OF                             ELTPAT  
00377          SRP-SUBROUTINE-PARAMETERS.                               ELTPAT  
00378      IF CIA-RC-PTR-NULL                                           ELTPAT  
00379          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAT  
00380      EJECT                                                        ELTPAT  
00381                                                                   ELTPAT  
00382                                                                   ELTPAT  
00383 ************************************************************      ELTPAT  
00384 *                                                          *      ELTPAT  
00385 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTPAT  
00386 *                                                          *      ELTPAT  
00387 ************************************************************      ELTPAT  
00388  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTPAT  
00389      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTPAT  
00390      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAT  
00391                            ADDRESS OF                             ELTPAT  
00392          TCAR-COMPRESSION-WORK-AREA.                              ELTPAT  
00393      IF CIA-RC-PTR-NULL                                           ELTPAT  
00394          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAT  
00395      EJECT                                                        ELTPAT  
00396                                                                   ELTPAT  
00397                                                                   ELTPAT  
00398 ************************************************************      ELTPAT  
00399 *                                                          *      ELTPAT  
00400 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTPAT  
00401 *                                                          *      ELTPAT  
00402 ************************************************************      ELTPAT  
00403  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTPAT  
00404      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTPAT  
00405      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAT  
00406                            ADDRESS OF                             ELTPAT  
00407          KWA-FILE-KEY-WORK-AREA.                                  ELTPAT  
00408      IF CIA-RC-PTR-NULL                                           ELTPAT  
00409          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAT  
00410      EJECT                                                        ELTPAT  
00411                                                                   ELTPAT  
00412                                                                   ELTPAT  
00413 ************************************************************      ELTPAT  
00414 *                                                          *      ELTPAT  
00415 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELTPAT  
00416 *                                                          *      ELTPAT  
00417 ************************************************************      ELTPAT  
00418  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTPAT  
00419      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTPAT  
00420      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAT  
00421                            ADDRESS OF                             ELTPAT  
00422          GROUP-SPECIFIC-REC.                                      ELTPAT  
00423      IF CIA-RC-PTR-NULL                                           ELTPAT  
00424          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAT  
00425      EJECT                                                        ELTPAT  
00426                                                                   ELTPAT  
00427                                                                   ELTPAT  
00428 ************************************************************      ELTPAT  
00429 *                                                          *      ELTPAT  
00430 *        ESTABLISH ADDRESSABILITY OF CST CONTAINMENT PROGRA*      ELTPAT  
00431 *                                                          *      ELTPAT  
00432 ************************************************************      ELTPAT  
00433  ESTABLISH-ADDRESSABILITY-OF-CS.                                  ELTPAT  
00434      SET CIA-GCTABULR-DDN TO TRUE.                                ELTPAT  
00435      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAT  
00436                            ADDRESS OF                             ELTPAT  
00437          GCCP-TABULAR-REC-AREA.                                   ELTPAT  
00438      IF CIA-RC-PTR-NULL                                           ELTPAT  
00439          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTPAT  
00440                                                                   ELTPAT  
00441                                                                   ELTPAT  
00442 ************************************************************      ELTPAT  
00443 *                                                          *      ELTPAT  
00444 *        ESTABLISH ADDRESS OF CIA                          *      ELTPAT  
00445 *                                                          *      ELTPAT  
00446 ************************************************************      ELTPAT  
00447  ESTABLISH-ADDRESS-OF-CIA.                                        ELTPAT  
00448      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTPAT  
00449                            ADDRESS OF                             ELTPAT  
00450          CIA-ELS-COMMON-INTERFACE-AREA.                           ELTPAT  
00451      EJECT                                                        ELTPAT  
00452                                                                   ELTPAT  
00453                                                                   ELTPAT  
00454 ************************************************************      ELTPAT  
00455 *                                                          *      ELTPAT  
00456 *        PROCESS                                           *      ELTPAT  
00457 *                                                          *      ELTPAT  
00458 ************************************************************      ELTPAT  
00459  PROCESS.                                                         ELTPAT  
00460      IF GCG-PRE-ADM-TESTING-PROGRAM  =  ZERO                      ELTPAT  
00461          PERFORM GENERATE-NOT-APPLICABLE-MESSAG                   ELTPAT  
00462      ELSE IF GCG-PRE-ADM-TESTING-PROGRAM  =  '08'                 ELTPAT  
00463          PERFORM GENERATE-VOLUNTARY-MESSAGE                       ELTPAT  
00464      ELSE                                                         ELTPAT  
00465          PERFORM GENERATE-PAT-TEXT.                               ELTPAT  
00466      PERFORM TERMINATE-OUTPUT.                                    ELTPAT  
00467                                                                   ELTPAT  
00468                                                                   ELTPAT  
00469 ************************************************************      ELTPAT  
00470 *                                                          *      ELTPAT  
00471 *        SET UP OUTPUT SUBSCRIPTS                          *      ELTPAT  
00472 *                                                          *      ELTPAT  
00473 ************************************************************      ELTPAT  
00474  SET-UP-OUTPUT-SUBSCRIPTS.                                        ELTPAT  
00475      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAT  
00476      MOVE +2 TO COF-NBR-HDR-LINES.                                ELTPAT  
00477      EJECT                                                        ELTPAT  
00478                                                                   ELTPAT  
00479                                                                   ELTPAT  
00480 ************************************************************      ELTPAT  
00481 *                                                          *      ELTPAT  
00482 *        EJECT NEW PAGE                                    *      ELTPAT  
00483 *                                                          *      ELTPAT  
00484 ************************************************************      ELTPAT  
00485  EJECT-NEW-PAGE.                                                  ELTPAT  
00486      SET COF-NEW-PAGE  TO TRUE.                                   ELTPAT  
00487      MOVE WS-HEADER-LINE TO COF-HDR-LINE                          ELTPAT  
00488          (COF-NBR-HDR-LINES).                                     ELTPAT  
00489      PERFORM LINK-TO-OUTPUT.                                      ELTPAT  
00490                                                                   ELTPAT  
00491                                                                   ELTPAT  
00492 ************************************************************      ELTPAT  
00493 *                                                          *      ELTPAT  
00494 *        GENERATE NOT APPLICABLE MESSAGE                   *      ELTPAT  
00495 *                                                          *      ELTPAT  
00496 ************************************************************      ELTPAT  
00497  GENERATE-NOT-APPLICABLE-MESSAG.                                  ELTPAT  
00498      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTPAT  
00499      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPAT  
00500      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPAT  
00501      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTPAT  
00502          (COF-NBR-DTL-LINES).                                     ELTPAT  
00503      PERFORM EJECT-NEW-PAGE.                                      ELTPAT  
00504      EJECT                                                        ELTPAT  
00505                                                                   ELTPAT  
00506                                                                   ELTPAT  
00507 ************************************************************      ELTPAT  
00508 *                                                          *      ELTPAT  
00509 *        GENERATE VOLUNTARY MESSAGE                        *      ELTPAT  
00510 *                                                          *      ELTPAT  
00511 ************************************************************      ELTPAT  
00512  GENERATE-VOLUNTARY-MESSAGE.                                      ELTPAT  
00513      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTPAT  
00514      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPAT  
00515      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPAT  
00516      MOVE WS-VOLUNTARY-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).   ELTPAT  
00517      PERFORM EJECT-NEW-PAGE.                                      ELTPAT  
00518      EJECT                                                        ELTPAT  
00519                                                                   ELTPAT  
00520                                                                   ELTPAT  
00521 ************************************************************      ELTPAT  
00522 *                                                          *      ELTPAT  
00523 *        GENERATE PAT TEXT                                 *      ELTPAT  
00524 *                                                          *      ELTPAT  
00525 ************************************************************      ELTPAT  
00526  GENERATE-PAT-TEXT.                                               ELTPAT  
00527      PERFORM SEARCH-FOR-GCCP-TABULAR.                             ELTPAT  
00528      PERFORM DETERMINE-SELECTION.                                 ELTPAT  
00529      EJECT                                                        ELTPAT  
00530                                                                   ELTPAT  
00531                                                                   ELTPAT  
00532 ************************************************************      ELTPAT  
00533 *                                                          *      ELTPAT  
00534 *        SEARCH FOR GCCP TABULAR                           *      ELTPAT  
00535 *                                                          *      ELTPAT  
00536 ************************************************************      ELTPAT  
00537  SEARCH-FOR-GCCP-TABULAR.                                         ELTPAT  
00538      SET GCG-INDEX TO +1.                                         ELTPAT  
00539      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTPAT  
00540         AT END                                                    ELTPAT  
00541              MOVE ZEROES TO KWA-PROVISION-SLOT-NO                 ELTPAT  
00542         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GCCP                     ELTPAT  
00543              MOVE GCG-TAB-ID (GCG-INDEX)                          ELTPAT  
00544                  TO KWA-PROVISION-ID                              ELTPAT  
00545              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTPAT  
00546                  TO KWA-PROVISION-SLOT-NO                         ELTPAT  
00547         END-SEARCH.                                               ELTPAT  
00548      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELTPAT  
00549          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTPAT  
00550      PERFORM GET-GCCP-TABULAR.                                    ELTPAT  
00551      PERFORM SEARCH-THE-GSS-ENTRY.                                ELTPAT  
00552                                                                   ELTPAT  
00553                                                                   ELTPAT  
00554 ************************************************************      ELTPAT  
00555 *                                                          *      ELTPAT  
00556 *        SIGNAL UNDEFINED TABULAR ERROR                    *      ELTPAT  
00557 *                                                          *      ELTPAT  
00558 ************************************************************      ELTPAT  
00559  SIGNAL-UNDEFINED-TABULAR-ERROR.                                  ELTPAT  
00560      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTPAT  
00561      PERFORM SIGNAL-ABEND.                                        ELTPAT  
00562                                                                   ELTPAT  
00563                                                                   ELTPAT  
00564 ************************************************************      ELTPAT  
00565 *                                                          *      ELTPAT  
00566 *        DETERMINE SELECTION                               *      ELTPAT  
00567 *                                                          *      ELTPAT  
00568 ************************************************************      ELTPAT  
00569  DETERMINE-SELECTION.                                             ELTPAT  
00570      IF SSB-PROV-CLASS-INST OR                                    ELTPAT  
00571                   SSB-PROV-CLASS-BOTH                             ELTPAT  
00572          PERFORM CREATE-INSTITUTIONAL-SCREEN.                     ELTPAT  
00573      IF SSB-PROV-CLASS-PROF OR                                    ELTPAT  
00574                   SSB-PROV-CLASS-BOTH                             ELTPAT  
00575          PERFORM CREATE-PROFESSIONAL-SCREEN.                      ELTPAT  
00576      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL '03' OR '04' OR       ELTPAT  
00577                    '06' OR '08')                                  ELTPAT  
00578          PERFORM CREATE-SUPPLEMENTAL-SCREEN.                      ELTPAT  
00579      EJECT                                                        ELTPAT  
00580                                                                   ELTPAT  
00581                                                                   ELTPAT  
00582 ************************************************************      ELTPAT  
00583 *                                                          *      ELTPAT  
00584 *        CREATE INSTITUTIONAL SCREEN                       *      ELTPAT  
00585 *                                                          *      ELTPAT  
00586 ************************************************************      ELTPAT  
00587  CREATE-INSTITUTIONAL-SCREEN.                                     ELTPAT  
00588      MOVE WS-INST TO WS-HDR-LINE-BCBSMM.                          ELTPAT  
00589      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTPAT  
00590      IF GSS-PT-BC-IND (GSS-INDEX) NOT EQUAL ZEROES AND SPACES     ELTPAT  
00591          AND                                                      ELTPAT  
00592                 LOW-VALUES                                        ELTPAT  
00593          PERFORM GENERATE-INSTITUTIONAL-TEXT                      ELTPAT  
00594      ELSE                                                         ELTPAT  
00595          PERFORM GENERATE-INSTITUTIONAL-NOT-APP.                  ELTPAT  
00596      EJECT                                                        ELTPAT  
00597                                                                   ELTPAT  
00598                                                                   ELTPAT  
00599 ************************************************************      ELTPAT  
00600 *                                                          *      ELTPAT  
00601 *        GENERATE INSTITUTIONAL TEXT                       *      ELTPAT  
00602 *                                                          *      ELTPAT  
00603 ************************************************************      ELTPAT  
00604  GENERATE-INSTITUTIONAL-TEXT.                                     ELTPAT  
00605      PERFORM EJECT-NEW-PAGE.                                      ELTPAT  
00606      PERFORM TRANSLATE-PARTICIPATION-IND.                         ELTPAT  
00607      PERFORM TRANSLATE-BC-IND.                                    ELTPAT  
00608      IF ((GSS-PT-BC-IP-ALT-PRIC-METH (GSS-INDEX) NOT EQUAL        ELTPAT  
00609          ZEROES AND                                               ELTPAT  
00610                  SPACES AND LOW-VALUES)) OR                       ELTPAT  
00611                ((GSS-PT-BC-OP-ALT-PRIC-METH (GSS-INDEX) NOT       ELTPAT  
00612          EQUAL ZEROES AND                                         ELTPAT  
00613                  SPACES AND LOW-VALUES))                          ELTPAT  
00614          PERFORM GENERATE-BC-ALT-PRIC-TEXT.                       ELTPAT  
00615      SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTPAT  
00616      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTPAT  
00617      IF GSS-PT-CALC-METHOD (GSS-INDEX) NOT EQUAL ZEROES AND       ELTPAT  
00618          SPACES                                                   ELTPAT  
00619                 AND LOW-VALUES                                    ELTPAT  
00620          PERFORM TRANSLATE-PAT-CALC-METHOD.                       ELTPAT  
00621      IF ((GSS-PT-BC-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL         ELTPAT  
00622          ZEROES AND                                               ELTPAT  
00623                   SPACES AND LOW-VALUES)) OR                      ELTPAT  
00624                 ((GSS-PT-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT       ELTPAT  
00625          EQUAL ZEROES AND                                         ELTPAT  
00626                   SPACES AND LOW-VALUES)) OR                      ELTPAT  
00627                ((GSS-PT-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT      ELTPAT  
00628          EQUAL ZEROES AND                                         ELTPAT  
00629                   SPACES AND LOW-VALUES))                         ELTPAT  
00630          PERFORM GENERATE-BC-BENE-REDUCT-TEXT.                    ELTPAT  
00631      IF GSS-PT-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL ZEROES        ELTPAT  
00632          AND                                                      ELTPAT  
00633                 SPACES AND LOW-VALUES                             ELTPAT  
00634          PERFORM GENERATE-SPILL-OVER-TEXT.                        ELTPAT  
00635      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTPAT  
00636                                                                   ELTPAT  
00637                                                                   ELTPAT  
00638 ************************************************************      ELTPAT  
00639 *                                                          *      ELTPAT  
00640 *        GENERATE INSTITUTIONAL NOT APPLICABLE             *      ELTPAT  
00641 *                                                          *      ELTPAT  
00642 ************************************************************      ELTPAT  
00643  GENERATE-INSTITUTIONAL-NOT-APP.                                  ELTPAT  
00644      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTPAT  
00645      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPAT  
00646      MOVE WS-NOT-APPLY-TO-LOB-BC TO COF-DTL-LINE                  ELTPAT  
00647          (COF-NBR-DTL-LINES).                                     ELTPAT  
00648      PERFORM EJECT-NEW-PAGE.                                      ELTPAT  
00649      EJECT                                                        ELTPAT  
00650                                                                   ELTPAT  
00651                                                                   ELTPAT  
00652 ************************************************************      ELTPAT  
00653 *                                                          *      ELTPAT  
00654 *        GENERATE DISCLAIMER MESSAGE                       *      ELTPAT  
00655 *                                                          *      ELTPAT  
00656 ************************************************************      ELTPAT  
00657  GENERATE-DISCLAIMER-MESSAGE.                                     ELTPAT  
00658      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAT  
00659      MOVE WS-DISCLAIMER-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).  ELTPAT  
00660      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPAT  
00661      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELTPAT  
00662      PERFORM LINK-TO-OUTPUT.                                      ELTPAT  
00663      EJECT                                                        ELTPAT  
00664                                                                   ELTPAT  
00665                                                                   ELTPAT  
00666 ************************************************************      ELTPAT  
00667 *                                                          *      ELTPAT  
00668 *        TRANSLATE PARTICIPATION IND                       *      ELTPAT  
00669 *                                                          *      ELTPAT  
00670 ************************************************************      ELTPAT  
00671  TRANSLATE-PARTICIPATION-IND.                                     ELTPAT  
00672      SET PERIOD-NEEDED TO TRUE.                                   ELTPAT  
00673      INITIALIZE TCAR-FROM-AREA.                                   ELTPAT  
00674      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAT  
00675      MOVE WS-PARTICIPATION-LINE TO TCAR-FROM-LINE                 ELTPAT  
00676          (TCAR-FROM-SUB).                                         ELTPAT  
00677      ADD  1 TO TCAR-FROM-SUB.                                     ELTPAT  
00678      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAT  
00679      MOVE GCG-PRE-ADM-TESTING-PROGRAM TO CMF-CODE-VALUE.          ELTPAT  
00680      MOVE    'PRE-ADM-TESTING-PROGRAM' TO                         ELTPAT  
00681          CMF-ELEMENT-SYSTEM-NAME.                                 ELTPAT  
00682      PERFORM LINK-TO-GROUP-CODES-MANUAL-INT.                      ELTPAT  
00683      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAT  
00684      EJECT                                                        ELTPAT  
00685                                                                   ELTPAT  
00686                                                                   ELTPAT  
00687 ************************************************************      ELTPAT  
00688 *                                                          *      ELTPAT  
00689 *        TRANSLATE BC IND                                  *      ELTPAT  
00690 *                                                          *      ELTPAT  
00691 ************************************************************      ELTPAT  
00692  TRANSLATE-BC-IND.                                                ELTPAT  
00693      INITIALIZE TCAR-FROM-AREA.                                   ELTPAT  
00694      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAT  
00695      MOVE WS-BCBSMM-IND-LINE TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELTPAT  
00696      ADD  +1 TO TCAR-FROM-SUB.                                    ELTPAT  
00697      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAT  
00698      SET PERIOD-NEEDED TO TRUE.                                   ELTPAT  
00699      MOVE GSS-PT-BC-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTPAT  
00700      MOVE    'PT-BC-IND'            TO CMF-ELEMENT-SYSTEM-NAME.   ELTPAT  
00701      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAT  
00702      EJECT                                                        ELTPAT  
00703                                                                   ELTPAT  
00704                                                                   ELTPAT  
00705 ************************************************************      ELTPAT  
00706 *                                                          *      ELTPAT  
00707 *        TRANSLATE PAT CALC METHOD                         *      ELTPAT  
00708 *                                                          *      ELTPAT  
00709 ************************************************************      ELTPAT  
00710  TRANSLATE-PAT-CALC-METHOD.                                       ELTPAT  
00711      INITIALIZE TCAR-FROM-AREA.                                   ELTPAT  
00712      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAT  
00713      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAT  
00714      SET PERIOD-NEEDED TO TRUE.                                   ELTPAT  
00715      MOVE GSS-PT-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.       ELTPAT  
00716      MOVE 'PT-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.            ELTPAT  
00717      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAT  
00718      EJECT                                                        ELTPAT  
00719                                                                   ELTPAT  
00720                                                                   ELTPAT  
00721 ************************************************************      ELTPAT  
00722 *                                                          *      ELTPAT  
00723 *        GENERATE BC ALT PRIC TEXT                         *      ELTPAT  
00724 *                                                          *      ELTPAT  
00725 ************************************************************      ELTPAT  
00726  GENERATE-BC-ALT-PRIC-TEXT.                                       ELTPAT  
00727      INITIALIZE WS-PERIOD-SWITCH.                                 ELTPAT  
00728      INITIALIZE TCAR-FROM-AREA.                                   ELTPAT  
00729      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAT  
00730      MOVE WS-ALT-PRICING-LINE-BC TO TCAR-FROM-LINE                ELTPAT  
00731          (TCAR-FROM-SUB).                                         ELTPAT  
00732      ADD  +1 TO TCAR-FROM-SUB.                                    ELTPAT  
00733      SET ADDITIONAL-TEXT TO TRUE.                                 ELTPAT  
00734      IF GSS-PT-BC-IP-ALT-PRIC-METH (GSS-INDEX) NOT EQUAL ZEROES   ELTPAT  
00735          AND                                                      ELTPAT  
00736                SPACES AND LOW-VALUES                              ELTPAT  
00737          PERFORM TRANSLATE-BC-IP-ALT-PRIC-METH.                   ELTPAT  
00738      IF GSS-PT-BC-OP-ALT-PRIC-METH (GSS-INDEX) NOT EQUAL ZEROES   ELTPAT  
00739          AND                                                      ELTPAT  
00740                SPACES AND LOW-VALUES                              ELTPAT  
00741          PERFORM TRANSLATE-BC-OP-ALT-PRIC-METH.                   ELTPAT  
00742      EJECT                                                        ELTPAT  
00743                                                                   ELTPAT  
00744                                                                   ELTPAT  
00745 ************************************************************      ELTPAT  
00746 *                                                          *      ELTPAT  
00747 *        FINISH SENTENCE                                   *      ELTPAT  
00748 *                                                          *      ELTPAT  
00749 ************************************************************      ELTPAT  
00750  FINISH-SENTENCE.                                                 ELTPAT  
00751      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAT  
00752      PERFORM REFORMAT-AND-WRITE-TEXT.                             ELTPAT  
00753      EJECT                                                        ELTPAT  
00754                                                                   ELTPAT  
00755                                                                   ELTPAT  
00756 ************************************************************      ELTPAT  
00757 *                                                          *      ELTPAT  
00758 *        TRANSLATE BC IP ALT PRIC METH                     *      ELTPAT  
00759 *                                                          *      ELTPAT  
00760 ************************************************************      ELTPAT  
00761  TRANSLATE-BC-IP-ALT-PRIC-METH.                                   ELTPAT  
00762      MOVE GSS-PT-BC-IP-ALT-PRIC-METH (GSS-INDEX) TO               ELTPAT  
00763          CMF-CODE-VALUE.                                          ELTPAT  
00764      MOVE 'PT-BC-IP-ALT-PRIC-METH' TO CMF-ELEMENT-SYSTEM-NAME.    ELTPAT  
00765      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAT  
00766      IF GSS-PT-BC-OP-ALT-PRIC-METH (GSS-INDEX) NOT EQUAL          ELTPAT  
00767          SPACES                                                   ELTPAT  
00768                 AND ZEROES AND LOW-VALUES                         ELTPAT  
00769          PERFORM CONTINUE-ALT-PRIC-SENTENCE                       ELTPAT  
00770      ELSE                                                         ELTPAT  
00771          PERFORM FINISH-ALT-PRIC-SENTENCE.                        ELTPAT  
00772                                                                   ELTPAT  
00773                                                                   ELTPAT  
00774 ************************************************************      ELTPAT  
00775 *                                                          *      ELTPAT  
00776 *        CONTINUE ALT PRIC SENTENCE                        *      ELTPAT  
00777 *                                                          *      ELTPAT  
00778 ************************************************************      ELTPAT  
00779  CONTINUE-ALT-PRIC-SENTENCE.                                      ELTPAT  
00780      MOVE WS-INPATIENT TO TCAR-FROM-LINE                          ELTPAT  
00781          (TCAR-FROM-SUB).                                         ELTPAT  
00782      ADD  +1 TO TCAR-FROM-SUB.                                    ELTPAT  
00783                                                                   ELTPAT  
00784                                                                   ELTPAT  
00785 ************************************************************      ELTPAT  
00786 *                                                          *      ELTPAT  
00787 *        FINISH ALT PRIC SENTENCE                          *      ELTPAT  
00788 *                                                          *      ELTPAT  
00789 ************************************************************      ELTPAT  
00790  FINISH-ALT-PRIC-SENTENCE.                                        ELTPAT  
00791      MOVE WS-INPATIENT-END TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELTPAT  
00792      PERFORM FINISH-SENTENCE.                                     ELTPAT  
00793                                                                   ELTPAT  
00794                                                                   ELTPAT  
00795 ************************************************************      ELTPAT  
00796 *                                                          *      ELTPAT  
00797 *        TRANSLATE BC OP ALT PRIC METH                     *      ELTPAT  
00798 *                                                          *      ELTPAT  
00799 ************************************************************      ELTPAT  
00800  TRANSLATE-BC-OP-ALT-PRIC-METH.                                   ELTPAT  
00801      MOVE GSS-PT-BC-OP-ALT-PRIC-METH (GSS-INDEX) TO               ELTPAT  
00802          CMF-CODE-VALUE.                                          ELTPAT  
00803      MOVE 'PT-BC-OP-ALT-PRIC-METH' TO CMF-ELEMENT-SYSTEM-NAME.    ELTPAT  
00804      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAT  
00805      MOVE WS-OUTPATIENT TO TCAR-FROM-LINE (TCAR-FROM-SUB).        ELTPAT  
00806      PERFORM FINISH-SENTENCE.                                     ELTPAT  
00807      EJECT                                                        ELTPAT  
00808                                                                   ELTPAT  
00809                                                                   ELTPAT  
00810 ************************************************************      ELTPAT  
00811 *                                                          *      ELTPAT  
00812 *        GENERATE BC BENE REDUCT TEXT                      *      ELTPAT  
00813 *                                                          *      ELTPAT  
00814 ************************************************************      ELTPAT  
00815  GENERATE-BC-BENE-REDUCT-TEXT.                                    ELTPAT  
00816      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAT  
00817      MOVE WS-BENE-REDUCT-LINE TO COF-DTL-LINE                     ELTPAT  
00818          (COF-NBR-DTL-LINES).                                     ELTPAT  
00819      PERFORM LINK-TO-OUTPUT.                                      ELTPAT  
00820      INITIALIZE ADDITIONAL-TEXT-SWITCH.                           ELTPAT  
00821      INITIALIZE WS-PERIOD-SWITCH.                                 ELTPAT  
00822      INITIALIZE TCAR-FROM-AREA.                                   ELTPAT  
00823      IF GSS-PT-BC-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL ZEROES    ELTPAT  
00824          AND                                                      ELTPAT  
00825                SPACES AND LOW-VALUES                              ELTPAT  
00826          PERFORM TRANSLATE-BC-DEDUCT-IND.                         ELTPAT  
00827      IF GSS-PT-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL ZEROES    ELTPAT  
00828          AND                                                      ELTPAT  
00829                SPACES AND LOW-VALUES                              ELTPAT  
00830          PERFORM TRANSLATE-BC-OPEX-APPLIC.                        ELTPAT  
00831      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAT  
00832      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPAT  
00833      PERFORM LINK-TO-OUTPUT.                                      ELTPAT  
00834      IF GSS-PT-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTPAT  
00835          ZEROES AND                                               ELTPAT  
00836               SPACES AND LOW-VALUES                               ELTPAT  
00837          PERFORM TRANSLATE-BC-OPEX-OVERRIDE.                      ELTPAT  
00838      EJECT                                                        ELTPAT  
00839                                                                   ELTPAT  
00840                                                                   ELTPAT  
00841 ************************************************************      ELTPAT  
00842 *                                                          *      ELTPAT  
00843 *        TRANSLATE BC DEDUCT IND                           *      ELTPAT  
00844 *                                                          *      ELTPAT  
00845 ************************************************************      ELTPAT  
00846  TRANSLATE-BC-DEDUCT-IND.                                         ELTPAT  
00847      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAT  
00848      MOVE GSS-PT-BC-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTPAT  
00849          CMF-CODE-VALUE.                                          ELTPAT  
00850      MOVE 'PT-BC-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTPAT  
00851      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAT  
00852      EJECT                                                        ELTPAT  
00853                                                                   ELTPAT  
00854                                                                   ELTPAT  
00855 ************************************************************      ELTPAT  
00856 *                                                          *      ELTPAT  
00857 *        TRANSLATE BC OPEX APPLIC                          *      ELTPAT  
00858 *                                                          *      ELTPAT  
00859 ************************************************************      ELTPAT  
00860  TRANSLATE-BC-OPEX-APPLIC.                                        ELTPAT  
00861      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAT  
00862      MOVE GSS-PT-BC-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTPAT  
00863          CMF-CODE-VALUE.                                          ELTPAT  
00864      MOVE 'PT-BC-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTPAT  
00865      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAT  
00866      EJECT                                                        ELTPAT  
00867                                                                   ELTPAT  
00868                                                                   ELTPAT  
00869 ************************************************************      ELTPAT  
00870 *                                                          *      ELTPAT  
00871 *        TRANSLATE BC OPEX OVERRIDE                        *      ELTPAT  
00872 *                                                          *      ELTPAT  
00873 ************************************************************      ELTPAT  
00874  TRANSLATE-BC-OPEX-OVERRIDE.                                      ELTPAT  
00875      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAT  
00876      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAT  
00877      SET PERIOD-NEEDED TO TRUE.                                   ELTPAT  
00878      MOVE GSS-PT-BC-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTPAT  
00879          CMF-CODE-VALUE.                                          ELTPAT  
00880      MOVE 'PT-BC-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTPAT  
00881      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAT  
00882      EJECT                                                        ELTPAT  
00883                                                                   ELTPAT  
00884                                                                   ELTPAT  
00885 ************************************************************      ELTPAT  
00886 *                                                          *      ELTPAT  
00887 *        CREATE PROFESSIONAL SCREEN                        *      ELTPAT  
00888 *                                                          *      ELTPAT  
00889 ************************************************************      ELTPAT  
00890  CREATE-PROFESSIONAL-SCREEN.                                      ELTPAT  
00891      MOVE WS-PROF TO WS-HDR-LINE-BCBSMM.                          ELTPAT  
00892      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTPAT  
00893      IF GSS-PT-BS-IND (GSS-INDEX) NOT EQUAL ZEROES AND SPACES     ELTPAT  
00894          AND                                                      ELTPAT  
00895                 LOW-VALUES                                        ELTPAT  
00896          PERFORM GENERATE-PROFESSIONAL-TEXT                       ELTPAT  
00897      ELSE                                                         ELTPAT  
00898          PERFORM GENERATE-PROFESSIONAL-NOT-APPL.                  ELTPAT  
00899      EJECT                                                        ELTPAT  
00900                                                                   ELTPAT  
00901                                                                   ELTPAT  
00902 ************************************************************      ELTPAT  
00903 *                                                          *      ELTPAT  
00904 *        GENERATE PROFESSIONAL TEXT                        *      ELTPAT  
00905 *                                                          *      ELTPAT  
00906 ************************************************************      ELTPAT  
00907  GENERATE-PROFESSIONAL-TEXT.                                      ELTPAT  
00908      PERFORM EJECT-NEW-PAGE.                                      ELTPAT  
00909      PERFORM TRANSLATE-PARTICIPATION-IND.                         ELTPAT  
00910      PERFORM TRANSLATE-BS-IND.                                    ELTPAT  
00911      IF ((GSS-PT-BS-IP-ALT-PRIC-METH (GSS-INDEX) NOT EQUAL        ELTPAT  
00912          ZEROES AND                                               ELTPAT  
00913                  SPACES AND LOW-VALUES)) OR                       ELTPAT  
00914                ((GSS-PT-BS-OP-ALT-PRIC-METH (GSS-INDEX) NOT       ELTPAT  
00915          EQUAL ZEROES AND                                         ELTPAT  
00916                  SPACES AND LOW-VALUES))                          ELTPAT  
00917          PERFORM GENERATE-BS-ALT-PRIC-TEXT.                       ELTPAT  
00918      SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTPAT  
00919      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTPAT  
00920      IF GSS-PT-CALC-METHOD (GSS-INDEX) NOT EQUAL ZEROES AND       ELTPAT  
00921          SPACES                                                   ELTPAT  
00922                 AND LOW-VALUES                                    ELTPAT  
00923          PERFORM TRANSLATE-PAT-CALC-METHOD.                       ELTPAT  
00924      IF ((GSS-PT-BS-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL         ELTPAT  
00925          ZEROES AND                                               ELTPAT  
00926                   SPACES AND LOW-VALUES)) OR                      ELTPAT  
00927                 ((GSS-PT-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT       ELTPAT  
00928          EQUAL ZEROES AND                                         ELTPAT  
00929                   SPACES AND LOW-VALUES)) OR                      ELTPAT  
00930                ((GSS-PT-BS-OPEX-OVERRIDE-IND (GSS-INDEX) NOT      ELTPAT  
00931          EQUAL ZEROES AND                                         ELTPAT  
00932                   SPACES AND LOW-VALUES))                         ELTPAT  
00933          PERFORM GENERATE-BS-BENE-REDUCT-TEXT.                    ELTPAT  
00934      IF GSS-PT-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL ZEROES        ELTPAT  
00935          AND                                                      ELTPAT  
00936                 SPACES AND LOW-VALUES                             ELTPAT  
00937          PERFORM GENERATE-SPILL-OVER-TEXT.                        ELTPAT  
00938      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTPAT  
00939                                                                   ELTPAT  
00940                                                                   ELTPAT  
00941 ************************************************************      ELTPAT  
00942 *                                                          *      ELTPAT  
00943 *        GENERATE PROFESSIONAL NOT APPLICABLE              *      ELTPAT  
00944 *                                                          *      ELTPAT  
00945 ************************************************************      ELTPAT  
00946  GENERATE-PROFESSIONAL-NOT-APPL.                                  ELTPAT  
00947      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPAT  
00948      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPAT  
00949      MOVE WS-NOT-APPLY-TO-LOB-BS TO COF-DTL-LINE                  ELTPAT  
00950          (COF-NBR-DTL-LINES).                                     ELTPAT  
00951      PERFORM EJECT-NEW-PAGE.                                      ELTPAT  
00952      EJECT                                                        ELTPAT  
00953                                                                   ELTPAT  
00954                                                                   ELTPAT  
00955 ************************************************************      ELTPAT  
00956 *                                                          *      ELTPAT  
00957 *        TRANSLATE BS IND                                  *      ELTPAT  
00958 *                                                          *      ELTPAT  
00959 ************************************************************      ELTPAT  
00960  TRANSLATE-BS-IND.                                                ELTPAT  
00961      INITIALIZE TCAR-FROM-AREA.                                   ELTPAT  
00962      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAT  
00963      MOVE WS-BCBSMM-IND-LINE TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELTPAT  
00964      ADD  +1 TO TCAR-FROM-SUB.                                    ELTPAT  
00965      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAT  
00966      SET PERIOD-NEEDED TO TRUE.                                   ELTPAT  
00967      MOVE GSS-PT-BS-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTPAT  
00968      MOVE    'PT-BS-IND'            TO CMF-ELEMENT-SYSTEM-NAME.   ELTPAT  
00969      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAT  
00970      EJECT                                                        ELTPAT  
00971                                                                   ELTPAT  
00972                                                                   ELTPAT  
00973 ************************************************************      ELTPAT  
00974 *                                                          *      ELTPAT  
00975 *        GENERATE BS ALT PRIC TEXT                         *      ELTPAT  
00976 *                                                          *      ELTPAT  
00977 ************************************************************      ELTPAT  
00978  GENERATE-BS-ALT-PRIC-TEXT.                                       ELTPAT  
00979      INITIALIZE WS-PERIOD-SWITCH.                                 ELTPAT  
00980      INITIALIZE TCAR-FROM-AREA.                                   ELTPAT  
00981      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAT  
00982      MOVE WS-ALT-PRICING-LINE-BS TO TCAR-FROM-LINE                ELTPAT  
00983          (TCAR-FROM-SUB).                                         ELTPAT  
00984      ADD  +1 TO TCAR-FROM-SUB.                                    ELTPAT  
00985      SET ADDITIONAL-TEXT TO TRUE.                                 ELTPAT  
00986      IF GSS-PT-BS-IP-ALT-PRIC-METH (GSS-INDEX) NOT EQUAL ZEROES   ELTPAT  
00987          AND                                                      ELTPAT  
00988                SPACES AND LOW-VALUES                              ELTPAT  
00989          PERFORM TRANSLATE-BS-IP-ALT-PRIC-METH.                   ELTPAT  
00990      IF GSS-PT-BS-OP-ALT-PRIC-METH (GSS-INDEX) NOT EQUAL ZEROES   ELTPAT  
00991          AND                                                      ELTPAT  
00992                SPACES AND LOW-VALUES                              ELTPAT  
00993          PERFORM TRANSLATE-BS-OP-ALT-PRIC-METH.                   ELTPAT  
00994      EJECT                                                        ELTPAT  
00995                                                                   ELTPAT  
00996                                                                   ELTPAT  
00997 ************************************************************      ELTPAT  
00998 *                                                          *      ELTPAT  
00999 *        TRANSLATE BS IP ALT PRIC METH                     *      ELTPAT  
01000 *                                                          *      ELTPAT  
01001 ************************************************************      ELTPAT  
01002  TRANSLATE-BS-IP-ALT-PRIC-METH.                                   ELTPAT  
01003      MOVE GSS-PT-BS-IP-ALT-PRIC-METH (GSS-INDEX) TO               ELTPAT  
01004          CMF-CODE-VALUE.                                          ELTPAT  
01005      MOVE 'PT-BS-IP-ALT-PRIC-METH' TO CMF-ELEMENT-SYSTEM-NAME.    ELTPAT  
01006      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAT  
01007      IF GSS-PT-BS-OP-ALT-PRIC-METH (GSS-INDEX) NOT EQUAL          ELTPAT  
01008          SPACES                                                   ELTPAT  
01009                 AND ZEROES AND LOW-VALUES                         ELTPAT  
01010          PERFORM CONTINUE-ALT-PRIC-SENTENCE                       ELTPAT  
01011      ELSE                                                         ELTPAT  
01012          PERFORM FINISH-ALT-PRIC-SENTENCE.                        ELTPAT  
01013                                                                   ELTPAT  
01014                                                                   ELTPAT  
01015 ************************************************************      ELTPAT  
01016 *                                                          *      ELTPAT  
01017 *        TRANSLATE BS OP ALT PRIC METH                     *      ELTPAT  
01018 *                                                          *      ELTPAT  
01019 ************************************************************      ELTPAT  
01020  TRANSLATE-BS-OP-ALT-PRIC-METH.                                   ELTPAT  
01021      MOVE GSS-PT-BS-OP-ALT-PRIC-METH (GSS-INDEX) TO               ELTPAT  
01022          CMF-CODE-VALUE.                                          ELTPAT  
01023      MOVE 'PT-BS-OP-ALT-PRIC-METH' TO CMF-ELEMENT-SYSTEM-NAME.    ELTPAT  
01024      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAT  
01025      MOVE WS-OUTPATIENT TO TCAR-FROM-LINE (TCAR-FROM-SUB).        ELTPAT  
01026      PERFORM FINISH-SENTENCE.                                     ELTPAT  
01027      EJECT                                                        ELTPAT  
01028                                                                   ELTPAT  
01029                                                                   ELTPAT  
01030 ************************************************************      ELTPAT  
01031 *                                                          *      ELTPAT  
01032 *        GENERATE BS BENE REDUCT TEXT                      *      ELTPAT  
01033 *                                                          *      ELTPAT  
01034 ************************************************************      ELTPAT  
01035  GENERATE-BS-BENE-REDUCT-TEXT.                                    ELTPAT  
01036      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAT  
01037      MOVE WS-BENE-REDUCT-LINE TO COF-DTL-LINE                     ELTPAT  
01038          (COF-NBR-DTL-LINES).                                     ELTPAT  
01039      PERFORM LINK-TO-OUTPUT.                                      ELTPAT  
01040      INITIALIZE ADDITIONAL-TEXT-SWITCH.                           ELTPAT  
01041      INITIALIZE WS-PERIOD-SWITCH.                                 ELTPAT  
01042      INITIALIZE TCAR-FROM-AREA.                                   ELTPAT  
01043      IF GSS-PT-BS-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL ZEROES    ELTPAT  
01044          AND                                                      ELTPAT  
01045                SPACES AND LOW-VALUES                              ELTPAT  
01046          PERFORM TRANSLATE-BS-DEDUCT-IND.                         ELTPAT  
01047      IF GSS-PT-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL ZEROES    ELTPAT  
01048          AND                                                      ELTPAT  
01049                SPACES AND LOW-VALUES                              ELTPAT  
01050          PERFORM TRANSLATE-BS-OPEX-APPLIC.                        ELTPAT  
01051      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAT  
01052      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPAT  
01053      PERFORM LINK-TO-OUTPUT.                                      ELTPAT  
01054      IF GSS-PT-BS-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTPAT  
01055          ZEROES AND                                               ELTPAT  
01056               SPACES AND LOW-VALUES                               ELTPAT  
01057          PERFORM TRANSLATE-BS-OPEX-OVERRIDE.                      ELTPAT  
01058      EJECT                                                        ELTPAT  
01059                                                                   ELTPAT  
01060                                                                   ELTPAT  
01061 ************************************************************      ELTPAT  
01062 *                                                          *      ELTPAT  
01063 *        TRANSLATE BS DEDUCT IND                           *      ELTPAT  
01064 *                                                          *      ELTPAT  
01065 ************************************************************      ELTPAT  
01066  TRANSLATE-BS-DEDUCT-IND.                                         ELTPAT  
01067      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAT  
01068      MOVE GSS-PT-BS-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTPAT  
01069          CMF-CODE-VALUE.                                          ELTPAT  
01070      MOVE 'PT-BS-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTPAT  
01071      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAT  
01072      EJECT                                                        ELTPAT  
01073                                                                   ELTPAT  
01074                                                                   ELTPAT  
01075 ************************************************************      ELTPAT  
01076 *                                                          *      ELTPAT  
01077 *        TRANSLATE BS OPEX APPLIC                          *      ELTPAT  
01078 *                                                          *      ELTPAT  
01079 ************************************************************      ELTPAT  
01080  TRANSLATE-BS-OPEX-APPLIC.                                        ELTPAT  
01081      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAT  
01082      MOVE GSS-PT-BS-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTPAT  
01083          CMF-CODE-VALUE.                                          ELTPAT  
01084      MOVE 'PT-BS-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTPAT  
01085      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAT  
01086      EJECT                                                        ELTPAT  
01087                                                                   ELTPAT  
01088                                                                   ELTPAT  
01089 ************************************************************      ELTPAT  
01090 *                                                          *      ELTPAT  
01091 *        TRANSLATE BS OPEX OVERRIDE                        *      ELTPAT  
01092 *                                                          *      ELTPAT  
01093 ************************************************************      ELTPAT  
01094  TRANSLATE-BS-OPEX-OVERRIDE.                                      ELTPAT  
01095      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAT  
01096      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAT  
01097      SET PERIOD-NEEDED TO TRUE.                                   ELTPAT  
01098      MOVE GSS-PT-BS-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTPAT  
01099          CMF-CODE-VALUE.                                          ELTPAT  
01100      MOVE 'PT-BS-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTPAT  
01101      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAT  
01102      EJECT                                                        ELTPAT  
01103                                                                   ELTPAT  
01104                                                                   ELTPAT  
01105 ************************************************************      ELTPAT  
01106 *                                                          *      ELTPAT  
01107 *        CREATE SUPPLEMENTAL SCREEN                        *      ELTPAT  
01108 *                                                          *      ELTPAT  
01109 ************************************************************      ELTPAT  
01110  CREATE-SUPPLEMENTAL-SCREEN.                                      ELTPAT  
01111      MOVE WS-SUPP TO WS-HDR-LINE-BCBSMM.                          ELTPAT  
01112      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTPAT  
01113      IF GSS-PT-MM-IND (GSS-INDEX) NOT EQUAL SPACES AND ZEROES     ELTPAT  
01114          AND                                                      ELTPAT  
01115                 LOW-VALUES                                        ELTPAT  
01116          PERFORM GENERATE-SUPPLEMENTAL-TEXT                       ELTPAT  
01117      ELSE                                                         ELTPAT  
01118          PERFORM GENERATE-SUPPLEMENTAL-NOT-APPL.                  ELTPAT  
01119      EJECT                                                        ELTPAT  
01120                                                                   ELTPAT  
01121                                                                   ELTPAT  
01122 ************************************************************      ELTPAT  
01123 *                                                          *      ELTPAT  
01124 *        GENERATE SUPPLEMENTAL TEXT                        *      ELTPAT  
01125 *                                                          *      ELTPAT  
01126 ************************************************************      ELTPAT  
01127  GENERATE-SUPPLEMENTAL-TEXT.                                      ELTPAT  
01128      PERFORM EJECT-NEW-PAGE.                                      ELTPAT  
01129      PERFORM TRANSLATE-PARTICIPATION-IND.                         ELTPAT  
01130      PERFORM TRANSLATE-MM-IND.                                    ELTPAT  
01131      IF ((GSS-PT-MM-IP-ALT-PRIC-METH (GSS-INDEX) NOT EQUAL        ELTPAT  
01132          ZEROES AND                                               ELTPAT  
01133                  SPACES AND LOW-VALUES)) OR                       ELTPAT  
01134                ((GSS-PT-MM-OP-ALT-PRIC-METH (GSS-INDEX) NOT       ELTPAT  
01135          EQUAL ZEROES AND                                         ELTPAT  
01136                  SPACES AND LOW-VALUES))                          ELTPAT  
01137          PERFORM GENERATE-MM-ALT-PRIC-TEXT.                       ELTPAT  
01138      SET SRP-ACCUM-PROV-CLASS-SUPP TO TRUE.                       ELTPAT  
01139      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTPAT  
01140      IF GSS-PT-CALC-METHOD (GSS-INDEX) NOT EQUAL ZEROES AND       ELTPAT  
01141          SPACES                                                   ELTPAT  
01142                 AND LOW-VALUES                                    ELTPAT  
01143          PERFORM TRANSLATE-PAT-CALC-METHOD.                       ELTPAT  
01144      IF ((GSS-PT-MM-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL         ELTPAT  
01145          ZEROES AND                                               ELTPAT  
01146                   SPACES AND LOW-VALUES)) OR                      ELTPAT  
01147                  ((GSS-PT-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT      ELTPAT  
01148          EQUAL ZEROES AND                                         ELTPAT  
01149                   SPACES AND LOW-VALUES)) OR                      ELTPAT  
01150                ((GSS-PT-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT      ELTPAT  
01151          EQUAL ZEROES AND                                         ELTPAT  
01152                   SPACES AND LOW-VALUES))                         ELTPAT  
01153          PERFORM GENERATE-MM-BENE-REDUCT-TEXT.                    ELTPAT  
01154      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTPAT  
01155                                                                   ELTPAT  
01156                                                                   ELTPAT  
01157 ************************************************************      ELTPAT  
01158 *                                                          *      ELTPAT  
01159 *        GENERATE SUPPLEMENTAL NOT APPLICABLE              *      ELTPAT  
01160 *                                                          *      ELTPAT  
01161 ************************************************************      ELTPAT  
01162  GENERATE-SUPPLEMENTAL-NOT-APPL.                                  ELTPAT  
01163      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPAT  
01164      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPAT  
01165      MOVE WS-NOT-APPLY-TO-LOB-MM TO COF-DTL-LINE                  ELTPAT  
01166          (COF-NBR-DTL-LINES).                                     ELTPAT  
01167      PERFORM EJECT-NEW-PAGE.                                      ELTPAT  
01168      EJECT                                                        ELTPAT  
01169                                                                   ELTPAT  
01170                                                                   ELTPAT  
01171 ************************************************************      ELTPAT  
01172 *                                                          *      ELTPAT  
01173 *        TRANSLATE MM IND                                  *      ELTPAT  
01174 *                                                          *      ELTPAT  
01175 ************************************************************      ELTPAT  
01176  TRANSLATE-MM-IND.                                                ELTPAT  
01177      INITIALIZE TCAR-FROM-AREA.                                   ELTPAT  
01178      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAT  
01179      MOVE WS-BCBSMM-IND-LINE TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELTPAT  
01180      ADD  +1 TO TCAR-FROM-SUB.                                    ELTPAT  
01181      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAT  
01182      SET PERIOD-NEEDED TO TRUE.                                   ELTPAT  
01183      MOVE GSS-PT-MM-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTPAT  
01184      MOVE    'PT-MM-IND'            TO CMF-ELEMENT-SYSTEM-NAME.   ELTPAT  
01185      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAT  
01186      EJECT                                                        ELTPAT  
01187                                                                   ELTPAT  
01188                                                                   ELTPAT  
01189 ************************************************************      ELTPAT  
01190 *                                                          *      ELTPAT  
01191 *        GENERATE MM ALT PRIC TEXT                         *      ELTPAT  
01192 *                                                          *      ELTPAT  
01193 ************************************************************      ELTPAT  
01194  GENERATE-MM-ALT-PRIC-TEXT.                                       ELTPAT  
01195      INITIALIZE WS-PERIOD-SWITCH.                                 ELTPAT  
01196      INITIALIZE TCAR-FROM-AREA.                                   ELTPAT  
01197      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAT  
01198      MOVE WS-ALT-PRICING-LINE-MM TO TCAR-FROM-LINE                ELTPAT  
01199          (TCAR-FROM-SUB).                                         ELTPAT  
01200      ADD  +1 TO TCAR-FROM-SUB.                                    ELTPAT  
01201      SET ADDITIONAL-TEXT TO TRUE.                                 ELTPAT  
01202      IF GSS-PT-MM-IP-ALT-PRIC-METH (GSS-INDEX) NOT EQUAL ZEROES   ELTPAT  
01203          AND                                                      ELTPAT  
01204                SPACES AND LOW-VALUES                              ELTPAT  
01205          PERFORM TRANSLATE-MM-IP-ALT-PRIC-METH.                   ELTPAT  
01206      IF GSS-PT-MM-OP-ALT-PRIC-METH (GSS-INDEX) NOT EQUAL ZEROES   ELTPAT  
01207          AND                                                      ELTPAT  
01208                SPACES AND LOW-VALUES                              ELTPAT  
01209          PERFORM TRANSLATE-MM-OP-ALT-PRIC-METH.                   ELTPAT  
01210      EJECT                                                        ELTPAT  
01211                                                                   ELTPAT  
01212                                                                   ELTPAT  
01213 ************************************************************      ELTPAT  
01214 *                                                          *      ELTPAT  
01215 *        TRANSLATE MM IP ALT PRIC METH                     *      ELTPAT  
01216 *                                                          *      ELTPAT  
01217 ************************************************************      ELTPAT  
01218  TRANSLATE-MM-IP-ALT-PRIC-METH.                                   ELTPAT  
01219      MOVE GSS-PT-MM-IP-ALT-PRIC-METH (GSS-INDEX) TO               ELTPAT  
01220          CMF-CODE-VALUE.                                          ELTPAT  
01221      MOVE 'PT-MM-IP-ALT-PRIC-METH' TO CMF-ELEMENT-SYSTEM-NAME.    ELTPAT  
01222      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAT  
01223      IF GSS-PT-MM-OP-ALT-PRIC-METH (GSS-INDEX) NOT EQUAL          ELTPAT  
01224          SPACES                                                   ELTPAT  
01225                 AND ZEROES AND LOW-VALUES                         ELTPAT  
01226          PERFORM CONTINUE-ALT-PRIC-SENTENCE                       ELTPAT  
01227      ELSE                                                         ELTPAT  
01228          PERFORM FINISH-ALT-PRIC-SENTENCE.                        ELTPAT  
01229                                                                   ELTPAT  
01230                                                                   ELTPAT  
01231 ************************************************************      ELTPAT  
01232 *                                                          *      ELTPAT  
01233 *        TRANSLATE MM OP ALT PRIC METH                     *      ELTPAT  
01234 *                                                          *      ELTPAT  
01235 ************************************************************      ELTPAT  
01236  TRANSLATE-MM-OP-ALT-PRIC-METH.                                   ELTPAT  
01237      MOVE GSS-PT-MM-OP-ALT-PRIC-METH (GSS-INDEX) TO               ELTPAT  
01238          CMF-CODE-VALUE.                                          ELTPAT  
01239      MOVE 'PT-MM-OP-ALT-PRIC-METH' TO CMF-ELEMENT-SYSTEM-NAME.    ELTPAT  
01240      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAT  
01241      MOVE WS-OUTPATIENT TO TCAR-FROM-LINE (TCAR-FROM-SUB).        ELTPAT  
01242      PERFORM FINISH-SENTENCE.                                     ELTPAT  
01243      EJECT                                                        ELTPAT  
01244                                                                   ELTPAT  
01245                                                                   ELTPAT  
01246 ************************************************************      ELTPAT  
01247 *                                                          *      ELTPAT  
01248 *        GENERATE MM BENE REDUCT TEXT                      *      ELTPAT  
01249 *                                                          *      ELTPAT  
01250 ************************************************************      ELTPAT  
01251  GENERATE-MM-BENE-REDUCT-TEXT.                                    ELTPAT  
01252      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAT  
01253      MOVE WS-BENE-REDUCT-LINE TO COF-DTL-LINE                     ELTPAT  
01254          (COF-NBR-DTL-LINES).                                     ELTPAT  
01255      PERFORM LINK-TO-OUTPUT.                                      ELTPAT  
01256      INITIALIZE ADDITIONAL-TEXT-SWITCH.                           ELTPAT  
01257      INITIALIZE WS-PERIOD-SWITCH.                                 ELTPAT  
01258      INITIALIZE TCAR-FROM-AREA.                                   ELTPAT  
01259      IF GSS-PT-MM-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL ZEROES    ELTPAT  
01260          AND                                                      ELTPAT  
01261                SPACES AND LOW-VALUES                              ELTPAT  
01262          PERFORM TRANSLATE-MM-DEDUCT-IND.                         ELTPAT  
01263      IF GSS-PT-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL ZEROES    ELTPAT  
01264          AND                                                      ELTPAT  
01265               SPACES AND LOW-VALUES                               ELTPAT  
01266          PERFORM TRANSLATE-MM-OPEX-APPLIC.                        ELTPAT  
01267      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAT  
01268      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPAT  
01269      PERFORM LINK-TO-OUTPUT.                                      ELTPAT  
01270      IF GSS-PT-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTPAT  
01271          ZEROES AND                                               ELTPAT  
01272               SPACES AND LOW-VALUES                               ELTPAT  
01273          PERFORM TRANSLATE-MM-OPEX-OVERRIDE.                      ELTPAT  
01274      EJECT                                                        ELTPAT  
01275                                                                   ELTPAT  
01276                                                                   ELTPAT  
01277 ************************************************************      ELTPAT  
01278 *                                                          *      ELTPAT  
01279 *        TRANSLATE MM DEDUCT IND                           *      ELTPAT  
01280 *                                                          *      ELTPAT  
01281 ************************************************************      ELTPAT  
01282  TRANSLATE-MM-DEDUCT-IND.                                         ELTPAT  
01283      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAT  
01284      MOVE GSS-PT-MM-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTPAT  
01285          CMF-CODE-VALUE.                                          ELTPAT  
01286      MOVE 'PT-MM-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTPAT  
01287      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAT  
01288      EJECT                                                        ELTPAT  
01289                                                                   ELTPAT  
01290                                                                   ELTPAT  
01291 ************************************************************      ELTPAT  
01292 *                                                          *      ELTPAT  
01293 *        TRANSLATE MM OPEX APPLIC                          *      ELTPAT  
01294 *                                                          *      ELTPAT  
01295 ************************************************************      ELTPAT  
01296  TRANSLATE-MM-OPEX-APPLIC.                                        ELTPAT  
01297      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAT  
01298      MOVE GSS-PT-MM-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTPAT  
01299          CMF-CODE-VALUE.                                          ELTPAT  
01300      MOVE 'PT-MM-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTPAT  
01301      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAT  
01302      EJECT                                                        ELTPAT  
01303                                                                   ELTPAT  
01304                                                                   ELTPAT  
01305 ************************************************************      ELTPAT  
01306 *                                                          *      ELTPAT  
01307 *        TRANSLATE MM OPEX OVERRIDE                        *      ELTPAT  
01308 *                                                          *      ELTPAT  
01309 ************************************************************      ELTPAT  
01310  TRANSLATE-MM-OPEX-OVERRIDE.                                      ELTPAT  
01311      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAT  
01312      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAT  
01313      SET PERIOD-NEEDED TO TRUE.                                   ELTPAT  
01314      MOVE GSS-PT-MM-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTPAT  
01315          CMF-CODE-VALUE.                                          ELTPAT  
01316      MOVE 'PT-MM-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTPAT  
01317      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAT  
01318      EJECT                                                        ELTPAT  
01319                                                                   ELTPAT  
01320                                                                   ELTPAT  
01321 ************************************************************      ELTPAT  
01322 *                                                          *      ELTPAT  
01323 *        SEARCH THE GSS ENTRY                              *      ELTPAT  
01324 *                                                          *      ELTPAT  
01325 ************************************************************      ELTPAT  
01326  SEARCH-THE-GSS-ENTRY.                                            ELTPAT  
01327      SET GSS-INDEX TO 1.                                          ELTPAT  
01328      SEARCH GSS-ENTRY                                             ELTPAT  
01329          AT END                                                   ELTPAT  
01330               SET TABULAR-IS-UNDEFINED TO TRUE                    ELTPAT  
01331          WHEN GSS-PT-PROG-CODE-CHR (GSS-INDEX)                    ELTPAT  
01332               CONTINUE                                            ELTPAT  
01333          END-SEARCH.                                              ELTPAT  
01334      IF TABULAR-IS-UNDEFINED                                      ELTPAT  
01335          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTPAT  
01336      EJECT                                                        ELTPAT  
01337                                                                   ELTPAT  
01338                                                                   ELTPAT  
01339 ************************************************************      ELTPAT  
01340 *                                                          *      ELTPAT  
01341 *        GENERATE ACCUM TABULAR DATA                       *      ELTPAT  
01342 *                                                          *      ELTPAT  
01343 ************************************************************      ELTPAT  
01344  GENERATE-ACCUM-TABULAR-DATA.                                     ELTPAT  
01345      PERFORM GENERATE-COINSURANCE.                                ELTPAT  
01346      PERFORM GENERATE-COPAY.                                      ELTPAT  
01347      PERFORM GENERATE-DEDUCTIBLE.                                 ELTPAT  
01348      PERFORM GENERATE-MAXIMUM.                                    ELTPAT  
01349                                                                   ELTPAT  
01350                                                                   ELTPAT  
01351 ************************************************************      ELTPAT  
01352 *                                                          *      ELTPAT  
01353 *        LINK TO CODES MANUAL INTERFACE                    *      ELTPAT  
01354 *                                                          *      ELTPAT  
01355 ************************************************************      ELTPAT  
01356  LINK-TO-CODES-MANUAL-INTERFACE.                                  ELTPAT  
01357      MOVE WS-GCCP TO CMF-RECORD-PREFIX.                           ELTPAT  
01358      EXEC CICS LINK                                               ELTPAT  
01359                PROGRAM ('ELUCMIF')                                ELTPAT  
01360                COMMAREA (DFHCOMMAREA)                             ELTPAT  
01361        END-EXEC.                                                  ELTPAT  
01362                                                                   ELTPAT  
01363                                                                   ELTPAT  
01364 ************************************************************      ELTPAT  
01365 *                                                          *      ELTPAT  
01366 *        LINK TO GROUP CODES MANUAL INTERFACE              *      ELTPAT  
01367 *                                                          *      ELTPAT  
01368 ************************************************************      ELTPAT  
01369  LINK-TO-GROUP-CODES-MANUAL-INT.                                  ELTPAT  
01370      MOVE WS-GROUP TO CMF-RECORD-PREFIX.                          ELTPAT  
01371      EXEC CICS LINK                                               ELTPAT  
01372                PROGRAM ('ELUCMIF')                                ELTPAT  
01373                COMMAREA (DFHCOMMAREA)                             ELTPAT  
01374        END-EXEC.                                                  ELTPAT  
01375      EJECT                                                        ELTPAT  
01376                                                                   ELTPAT  
01377                                                                   ELTPAT  
01378 ************************************************************      ELTPAT  
01379 *                                                          *      ELTPAT  
01380 *        GET GCCP TABULAR                                  *      ELTPAT  
01381 *                                                          *      ELTPAT  
01382 ************************************************************      ELTPAT  
01383  GET-GCCP-TABULAR.                                                ELTPAT  
01384      PERFORM ESTABLISH-ADDRESSABILITY-GCTAB.                      ELTPAT  
01385      SET IOP-RD              TO TRUE.                             ELTPAT  
01386      SET IOP-STG-MODE-MOVE   TO TRUE.                             ELTPAT  
01387      SET IOP-FCQ-NONE        TO TRUE.                             ELTPAT  
01388      SET IOP-KVQ-EQ          TO TRUE.                             ELTPAT  
01389      MOVE KWA-GCTABULR-KEY   TO IOP-FILE-KEY.                     ELTPAT  
01390      PERFORM CALL-INPUT-OUTPUT-MODULE.                            ELTPAT  
01391      IF IOP-RC-OK                                                 ELTPAT  
01392          PERFORM ESTABLISH-ADDRESSABILITY-OF-GC                   ELTPAT  
01393      ELSE IF IOP-RC-NOTFND                                        ELTPAT  
01394          PERFORM SIGNAL-NOT-FOUND-GCTAB-ERROR                     ELTPAT  
01395      ELSE                                                         ELTPAT  
01396          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTPAT  
01397      EJECT                                                        ELTPAT  
01398                                                                   ELTPAT  
01399                                                                   ELTPAT  
01400 ************************************************************      ELTPAT  
01401 *                                                          *      ELTPAT  
01402 *        ESTABLISH ADDRESSABILITY GCTABULAR IO PARAMETER BL*      ELTPAT  
01403 *                                                          *      ELTPAT  
01404 ************************************************************      ELTPAT  
01405  ESTABLISH-ADDRESSABILITY-GCTAB.                                  ELTPAT  
01406      SET CIA-GCTABULR-DDN TO TRUE.                                ELTPAT  
01407      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAT  
01408                            ADDRESS OF                             ELTPAT  
01409          IOP-INPUT-OUTPUT-PARAMETERS.                             ELTPAT  
01410      EJECT                                                        ELTPAT  
01411                                                                   ELTPAT  
01412                                                                   ELTPAT  
01413 ************************************************************      ELTPAT  
01414 *                                                          *      ELTPAT  
01415 *        CALL INPUT OUTPUT MODULE                          *      ELTPAT  
01416 *                                                          *      ELTPAT  
01417 ************************************************************      ELTPAT  
01418  CALL-INPUT-OUTPUT-MODULE.                                        ELTPAT  
01419      EXEC CICS LINK                                               ELTPAT  
01420                PROGRAM ('ELUIOPGM')                               ELTPAT  
01421                COMMAREA (DFHCOMMAREA)                             ELTPAT  
01422        END-EXEC.                                                  ELTPAT  
01423      EJECT                                                        ELTPAT  
01424                                                                   ELTPAT  
01425                                                                   ELTPAT  
01426 ************************************************************      ELTPAT  
01427 *                                                          *      ELTPAT  
01428 *        ESTABLISH ADDRESSABILITY OF GCCP TABULAR          *      ELTPAT  
01429 *                                                          *      ELTPAT  
01430 ************************************************************      ELTPAT  
01431  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTPAT  
01432      SET ADDRESS OF GCCP-TABULAR-REC-AREA TO                      ELTPAT  
01433          IOP-REC-PTR.                                             ELTPAT  
01434      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELTPAT  
01435      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTPAT  
01436                            IOP-REC-PTR.                           ELTPAT  
01437      SET IOP-REC-PTR TO NULL.                                     ELTPAT  
01438      EJECT                                                        ELTPAT  
01439                                                                   ELTPAT  
01440                                                                   ELTPAT  
01441 ************************************************************      ELTPAT  
01442 *                                                          *      ELTPAT  
01443 *        SIGNAL NOT FOUND GCTAB ERROR                      *      ELTPAT  
01444 *                                                          *      ELTPAT  
01445 ************************************************************      ELTPAT  
01446  SIGNAL-NOT-FOUND-GCTAB-ERROR.                                    ELTPAT  
01447      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTPAT  
01448      PERFORM SIGNAL-ABEND.                                        ELTPAT  
01449      EJECT                                                        ELTPAT  
01450                                                                   ELTPAT  
01451                                                                   ELTPAT  
01452 ************************************************************      ELTPAT  
01453 *                                                          *      ELTPAT  
01454 *        SIGNAL CRITICAL IO ERROR                          *      ELTPAT  
01455 *                                                          *      ELTPAT  
01456 ************************************************************      ELTPAT  
01457  SIGNAL-CRITICAL-IO-ERROR.                                        ELTPAT  
01458      SET CIA-AB-CRITIO TO TRUE.                                   ELTPAT  
01459      PERFORM SIGNAL-ABEND.                                        ELTPAT  
01460                                                                   ELTPAT  
01461                                                                   ELTPAT  
01462 ************************************************************      ELTPAT  
01463 *                                                          *      ELTPAT  
01464 *        GENERATE COINSURANCE                              *      ELTPAT  
01465 *                                                          *      ELTPAT  
01466 ************************************************************      ELTPAT  
01467  GENERATE-COINSURANCE.                                            ELTPAT  
01468      EXEC CICS LINK                                               ELTPAT  
01469                PROGRAM ('ELGACLCC')                               ELTPAT  
01470                COMMAREA (DFHCOMMAREA)                             ELTPAT  
01471        END-EXEC.                                                  ELTPAT  
01472                                                                   ELTPAT  
01473                                                                   ELTPAT  
01474 ************************************************************      ELTPAT  
01475 *                                                          *      ELTPAT  
01476 *        GENERATE DEDUCTIBLE                               *      ELTPAT  
01477 *                                                          *      ELTPAT  
01478 ************************************************************      ELTPAT  
01479  GENERATE-COPAY.                                                  ELTPAT  
01480      EXEC CICS LINK                                               ELTPAT  
01481                PROGRAM ('ELGACPCC')                               ELTPAT  
01482                COMMAREA (DFHCOMMAREA)                             ELTPAT  
01483        END-EXEC.                                                  ELTPAT  
01484                                                                   ELTPAT  
01485                                                                   ELTPAT  
01486 ************************************************************      ELTPAT  
01487 *                                                          *      ELTPAT  
01488 *        GENERATE DEDUCTIBLE                               *      ELTPAT  
01489 *                                                          *      ELTPAT  
01490 ************************************************************      ELTPAT  
01491  GENERATE-DEDUCTIBLE.                                             ELTPAT  
01492      EXEC CICS LINK                                               ELTPAT  
01493                PROGRAM ('ELGADLCC')                               ELTPAT  
01494                COMMAREA (DFHCOMMAREA)                             ELTPAT  
01495        END-EXEC.                                                  ELTPAT  
01496                                                                   ELTPAT  
01497                                                                   ELTPAT  
01498 ************************************************************      ELTPAT  
01499 *                                                          *      ELTPAT  
01500 *        GENERATE MAXIMUM                                  *      ELTPAT  
01501 *                                                          *      ELTPAT  
01502 ************************************************************      ELTPAT  
01503  GENERATE-MAXIMUM.                                                ELTPAT  
01504      EXEC CICS LINK                                               ELTPAT  
01505                PROGRAM ('ELGABMCC')                               ELTPAT  
01506                COMMAREA (DFHCOMMAREA)                             ELTPAT  
01507        END-EXEC.                                                  ELTPAT  
01508      EJECT                                                        ELTPAT  
01509                                                                   ELTPAT  
01510                                                                   ELTPAT  
01511 ************************************************************      ELTPAT  
01512 *                                                          *      ELTPAT  
01513 *        GENERATE SPILL OVER TEXT                          *      ELTPAT  
01514 *                                                          *      ELTPAT  
01515 ************************************************************      ELTPAT  
01516  GENERATE-SPILL-OVER-TEXT.                                        ELTPAT  
01517      INITIALIZE TCAR-FROM-AREA.                                   ELTPAT  
01518      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAT  
01519      MOVE WS-SPILL-OVER-LINE TO TCAR-FROM-LINE                    ELTPAT  
01520          (TCAR-FROM-SUB).                                         ELTPAT  
01521      ADD  +1 TO TCAR-FROM-SUB.                                    ELTPAT  
01522      SET BLANK-LINE-NEEDED TO TRUE.                               ELTPAT  
01523      SET PERIOD-NEEDED TO TRUE.                                   ELTPAT  
01524      MOVE GSS-PT-SPILL-OVER-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTPAT  
01525      MOVE 'PT-SPILL-OVER-IND' TO CMF-ELEMENT-SYSTEM-NAME.         ELTPAT  
01526      PERFORM TRANSLATE-AND-DISPLAY-CODE-VAL.                      ELTPAT  
01527                                                                   ELTPAT  
01528                                                                   ELTPAT  
01529 ************************************************************      ELTPAT  
01530 *                                                          *      ELTPAT  
01531 *        TRANSLATE AND DISPLAY CODE VALUE                  *      ELTPAT  
01532 *                                                          *      ELTPAT  
01533 ************************************************************      ELTPAT  
01534  TRANSLATE-AND-DISPLAY-CODE-VAL.                                  ELTPAT  
01535      PERFORM LINK-TO-CODES-MANUAL-INTERFACE.                      ELTPAT  
01536      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTPAT  
01537      EJECT                                                        ELTPAT  
01538                                                                   ELTPAT  
01539                                                                   ELTPAT  
01540 ************************************************************      ELTPAT  
01541 *                                                          *      ELTPAT  
01542 *        TERMINATE OUTPUT                                  *      ELTPAT  
01543 *                                                          *      ELTPAT  
01544 ************************************************************      ELTPAT  
01545  TERMINATE-OUTPUT.                                                ELTPAT  
01546      SET COF-END TO TRUE.                                         ELTPAT  
01547      PERFORM LINK-TO-OUTPUT.                                      ELTPAT  
01548                                                                   ELTPAT  
01549                                                                   ELTPAT  
01550 ************************************************************      ELTPAT  
01551 *                                                          *      ELTPAT  
01552 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTPAT  
01553 *                                                          *      ELTPAT  
01554 ************************************************************      ELTPAT  
01555  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTPAT  
01556      PERFORM INITIALIZE-CMOUT.                                    ELTPAT  
01557      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTPAT  
01558      EJECT                                                        ELTPAT  
01559                                                                   ELTPAT  
01560                                                                   ELTPAT  
01561 ************************************************************      ELTPAT  
01562 *                                                          *      ELTPAT  
01563 *        PREPARE TEXT FOR OUTPUT                           *      ELTPAT  
01564 *                                                          *      ELTPAT  
01565 ************************************************************      ELTPAT  
01566  PREPARE-TEXT-FOR-OUTPUT.                                         ELTPAT  
01567      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTPAT  
01568          UNTIL CMF-DESCR-IDX                                      ELTPAT  
01569                                    GREATER THAN                   ELTPAT  
01570              CMF-NBR-DESCR-LINES.                                 ELTPAT  
01571      EJECT                                                        ELTPAT  
01572                                                                   ELTPAT  
01573                                                                   ELTPAT  
01574 ************************************************************      ELTPAT  
01575 *                                                          *      ELTPAT  
01576 *        INITIALIZE CMOUT                                  *      ELTPAT  
01577 *                                                          *      ELTPAT  
01578 ************************************************************      ELTPAT  
01579  INITIALIZE-CMOUT.                                                ELTPAT  
01580      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTPAT  
01581      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPAT  
01582          ADDRESS OF CMF-DESCR.                                    ELTPAT  
01583      SET CMF-DESCR-IDX TO 1.                                      ELTPAT  
01584      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTPAT  
01585                                                                   ELTPAT  
01586                                                                   ELTPAT  
01587 ************************************************************      ELTPAT  
01588 *                                                          *      ELTPAT  
01589 *        MOVE CMF TEXT TO OUTPUT                           *      ELTPAT  
01590 *                                                          *      ELTPAT  
01591 ************************************************************      ELTPAT  
01592  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTPAT  
01593      PERFORM MOVE-A-LINE.                                         ELTPAT  
01594      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTPAT  
01595          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTPAT  
01596      IF TCAR-FROM-SUB GREATER THAN 20                             ELTPAT  
01597               OR CMF-DESCR-IDX GREATER THAN                       ELTPAT  
01598          CMF-NBR-DESCR-LINES                                      ELTPAT  
01599          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTPAT  
01600                                                                   ELTPAT  
01601                                                                   ELTPAT  
01602 ************************************************************      ELTPAT  
01603 *                                                          *      ELTPAT  
01604 *        FINISH CODES MANUAL TEXT                          *      ELTPAT  
01605 *                                                          *      ELTPAT  
01606 ************************************************************      ELTPAT  
01607  FINISH-CODES-MANUAL-TEXT.                                        ELTPAT  
01608      SET DONE-PROCESSING TO TRUE.                                 ELTPAT  
01609      IF PERIOD-NEEDED                                             ELTPAT  
01610          PERFORM GET-AND-MOVE-PERIOD.                             ELTPAT  
01611                                                                   ELTPAT  
01612                                                                   ELTPAT  
01613 ************************************************************      ELTPAT  
01614 *                                                          *      ELTPAT  
01615 *        GET AND MOVE PERIOD                               *      ELTPAT  
01616 *                                                          *      ELTPAT  
01617 ************************************************************      ELTPAT  
01618  GET-AND-MOVE-PERIOD.                                             ELTPAT  
01619      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTPAT  
01620          (TCAR-FROM-SUB).                                         ELTPAT  
01621                                                                   ELTPAT  
01622                                                                   ELTPAT  
01623 ************************************************************      ELTPAT  
01624 *                                                          *      ELTPAT  
01625 *        SAVE LAST LINE                                    *      ELTPAT  
01626 *                                                          *      ELTPAT  
01627 ************************************************************      ELTPAT  
01628  SAVE-LAST-LINE.                                                  ELTPAT  
01629      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAT  
01630      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTPAT  
01631         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTPAT  
01632      ADD 1 TO TCAR-FROM-SUB.                                      ELTPAT  
01633      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTPAT  
01634                                                                   ELTPAT  
01635                                                                   ELTPAT  
01636 ************************************************************      ELTPAT  
01637 *                                                          *      ELTPAT  
01638 *        OUTPUT LAST LINE                                  *      ELTPAT  
01639 *                                                          *      ELTPAT  
01640 ************************************************************      ELTPAT  
01641  OUTPUT-LAST-LINE.                                                ELTPAT  
01642      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTPAT  
01643          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTPAT  
01644      IF BLANK-LINE-NEEDED                                         ELTPAT  
01645          PERFORM CREATE-A-BLANK-LINE.                             ELTPAT  
01646                                                                   ELTPAT  
01647                                                                   ELTPAT  
01648 ************************************************************      ELTPAT  
01649 *                                                          *      ELTPAT  
01650 *        CREATE A BLANK LINE                               *      ELTPAT  
01651 *                                                          *      ELTPAT  
01652 ************************************************************      ELTPAT  
01653  CREATE-A-BLANK-LINE.                                             ELTPAT  
01654      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTPAT  
01655      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTPAT  
01656                                                                   ELTPAT  
01657                                                                   ELTPAT  
01658 ************************************************************      ELTPAT  
01659 *                                                          *      ELTPAT  
01660 *        MOVE A LINE                                       *      ELTPAT  
01661 *                                                          *      ELTPAT  
01662 ************************************************************      ELTPAT  
01663  MOVE-A-LINE.                                                     ELTPAT  
01664      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTPAT  
01665          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTPAT  
01666      SET CMF-DESCR-IDX UP BY 1.                                   ELTPAT  
01667      ADD 1 TO TCAR-FROM-SUB.                                      ELTPAT  
01668      EJECT                                                        ELTPAT  
01669                                                                   ELTPAT  
01670                                                                   ELTPAT  
01671 ************************************************************      ELTPAT  
01672 *                                                          *      ELTPAT  
01673 *        REFORMAT AND WRITE TEXT                           *      ELTPAT  
01674 *                                                          *      ELTPAT  
01675 ************************************************************      ELTPAT  
01676  REFORMAT-AND-WRITE-TEXT.                                         ELTPAT  
01677      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTPAT  
01678      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTPAT  
01679      PERFORM UNSTRING-TEXT.                                       ELTPAT  
01680      MOVE +1 TO TCAR-FROM-SUB.                                    ELTPAT  
01681      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTPAT  
01682      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTPAT  
01683          UNTIL COF-NBR-DTL-LINES GREATER                          ELTPAT  
01684                                   TCAR-OUTPUT-FIELDS-USED -       ELTPAT  
01685              1.                                                   ELTPAT  
01686      PERFORM DISPOSE-OF-LAST-LINE.                                ELTPAT  
01687      PERFORM LINK-TO-OUTPUT.                                      ELTPAT  
01688                                                                   ELTPAT  
01689                                                                   ELTPAT  
01690 ************************************************************      ELTPAT  
01691 *                                                          *      ELTPAT  
01692 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTPAT  
01693 *                                                          *      ELTPAT  
01694 ************************************************************      ELTPAT  
01695  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTPAT  
01696      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTPAT  
01697           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTPAT  
01698      ADD +1 TO TCAR-FROM-SUB.                                     ELTPAT  
01699      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTPAT  
01700      EJECT                                                        ELTPAT  
01701                                                                   ELTPAT  
01702                                                                   ELTPAT  
01703 ************************************************************      ELTPAT  
01704 *                                                          *      ELTPAT  
01705 *        UNSTRING TEXT                                     *      ELTPAT  
01706 *                                                          *      ELTPAT  
01707 ************************************************************      ELTPAT  
01708  UNSTRING-TEXT.                                                   ELTPAT  
01709      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTPAT  
01710      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTPAT  
01711      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTPAT  
01712      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTPAT  
01713      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTPAT  
01714      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTPAT  
01715      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTPAT  
01716      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTPAT  
01717      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTPAT  
01718      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTPAT  
01719      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTPAT  
01720      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTPAT  
01721      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTPAT  
01722      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTPAT  
01723      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTPAT  
01724      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTPAT  
01725      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTPAT  
01726      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTPAT  
01727      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTPAT  
01728      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTPAT  
01729      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTPAT  
01730      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTPAT  
01731                                                                   ELTPAT  
01732                                                                   ELTPAT  
01733 ************************************************************      ELTPAT  
01734 *                                                          *      ELTPAT  
01735 *        LINK TO OUTPUT                                    *      ELTPAT  
01736 *                                                          *      ELTPAT  
01737 ************************************************************      ELTPAT  
01738  LINK-TO-OUTPUT.                                                  ELTPAT  
01739      EXEC CICS LINK                                               ELTPAT  
01740          PROGRAM ('ELUOUTPT')                                     ELTPAT  
01741          COMMAREA (DFHCOMMAREA)                                   ELTPAT  
01742          END-EXEC.                                                ELTPAT  
01743      EJECT                                                        ELTPAT  
01744                                                                   ELTPAT  
01745                                                                   ELTPAT  
01746 ************************************************************      ELTPAT  
01747 *                                                          *      ELTPAT  
01748 *        DISPOSE OF LAST LINE                              *      ELTPAT  
01749 *                                                          *      ELTPAT  
01750 ************************************************************      ELTPAT  
01751  DISPOSE-OF-LAST-LINE.                                            ELTPAT  
01752      IF NOT ADDITIONAL-TEXT                                       ELTPAT  
01753          PERFORM INITIALIZE-CONTINUED-SW.                         ELTPAT  
01754      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTPAT  
01755          PERFORM SAVE-LAST-LINE                                   ELTPAT  
01756      ELSE                                                         ELTPAT  
01757          PERFORM OUTPUT-LAST-LINE.                                ELTPAT  
01758                                                                   ELTPAT  
01759                                                                   ELTPAT  
01760 ************************************************************      ELTPAT  
01761 *                                                          *      ELTPAT  
01762 *        INITIALIZE CONTINUED SW                           *      ELTPAT  
01763 *                                                          *      ELTPAT  
01764 ************************************************************      ELTPAT  
01765  INITIALIZE-CONTINUED-SW.                                         ELTPAT  
01766      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTPAT  
