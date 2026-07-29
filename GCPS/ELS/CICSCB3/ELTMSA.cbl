00001 *      LAST MAINTENANCE TIME: 10.12.49  DATE: 06/14/91            06/29/02
00002  IDENTIFICATION DIVISION.                                         ELTMSA  
00003                                                                      LV001
00004  PROGRAM-ID.         ELTMSA.                                      ELTMSA  
00005                                                                   ELTMSA  
00006  AUTHOR.             RICK BARILEAU.                               ELTMSA  
00007                                                                   ELTMSA  
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTMSA  
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELTMSA  
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTMSA  
00011                      233 N. MICHIGAN AVE                          ELTMSA  
00012                      CHICAGO, ILLINOIS 60601                      ELTMSA  
00013                                                                   ELTMSA  
00014  DATE-WRITTEN.       18-JUN-1987.                                 ELTMSA  
00015                                                                   ELTMSA  
00016  DATE-COMPILED.                                                   ELTMSA  
00017                                                                   ELTMSA  
00018  SECURITY.           COPYRIGHT 1986,                              ELTMSA  
00019                      HEALTH CARE SERVICE CORPORATION              ELTMSA  
00020      SKIP3                                                        ELTMSA  
00021  ENVIRONMENT DIVISION.                                            ELTMSA  
00022                                                                   ELTMSA  
00023  CONFIGURATION SECTION.                                           ELTMSA  
00024  SOURCE-COMPUTER.    IBM-3090.                                    ELTMSA  
00025  OBJECT-COMPUTER.    IBM-3090.                                    ELTMSA  
00026      EJECT                                                        ELTMSA  
00027 ******************************************************************ELTMSA  
00028 *                                                                *ELTMSA  
00029 *    COPYBOOK:   ELTMSA                                          *ELTMSA  
00030 *    DATE:       18-JUN-1987                                     *ELTMSA  
00031 *    AUTHOR:     RICK BARILEAU                                   *ELTMSA  
00032 *    FUNCTION:   THIS MODULE WILL GENERATE ALL OUTPUT ASSOCIATED *ELTMSA  
00033 *                WITH THE MEDICAL SERVICES ADVISORY PROGRAM.     *ELTMSA  
00034 *    NOTES:      X---                                            *ELTMSA  
00035 *                                                                *ELTMSA  
00036 ******************************************************************ELTMSA  
00037 *                                                                *ELTMSA  
00038 *                      MAINTENANCE HISTORY                       *ELTMSA  
00039 *                                                                *ELTMSA  
00040 *  MOD     DATE     BY  DRPT                ACTION               *ELTMSA  
00041 * ----- ----------- --- ----- ---------------------------------- *ELTMSA  
00042 * 01.00 18-JUN-1987 REB       CREATED                            *ELTMSA  
00043 *                                                                *ELTMSA  
00044 * 01.01 02-SEP-1987 REB       REARRANGE ORDER OF G-TABS INFO TO  *ELTMSA  
00045 *                             FOLLOW SENTENCE STATING THEY'RE    *ELTMSA  
00046 *                             INCLUDED. ALSO, MOVE THE SPILL OVER*ELTMSA  
00047 *                             SENTENCE BEFORE G-TAB INFORMATION. *ELTMSA  
00048 *                                                                *ELTMSA  
00049 * 01.02 06-NOV-1990 JPB       CHANGED STORAGE MANAGEMENT         *ELTMSA  
00050 *                                                                *ELTMSA  
00051 * 01.03 12-NOV-1990 JPB       CHANGED REFERENCES TO GCG-MED-SERV-*ELTMSA  
00052 *                             ADV-PROG-IND TO REFLECT NEW FIELD  *ELTMSA  
00053 *                             SIZE.                              *ELTMSA  
00054 *                                                                *ELTMSA  
00055 * 01.04 12-JUN-1991 GEM       ADD CCP PARTIC IND MSA.            *ELTMSA  
00056 ******************************************************************ELTMSA  
00057                                                                   ELTMSA  
00058  DATA DIVISION.                                                   ELTMSA  
00059  WORKING-STORAGE SECTION.                                         ELTMSA  
00060  01  WS-MISC.                                                     ELTMSA  
00061      05  WS-BEGIN                 PIC X(24) VALUE                 ELTMSA  
00062          '** ELTMSA WS BEGINS **'.                                ELTMSA  
00063                                                                   ELTMSA  
00064      05  WS-POINTER2              POINTER.                        ELTMSA  
00065      05  WS-POINTER3              POINTER.                        ELTMSA  
00066                                                                   ELTMSA  
00067  01  WS-SWITCHES.                                                 ELTMSA  
00068      05  ADDITIONAL-TEXT-SWITCH   PIC X(01) VALUE SPACE.          ELTMSA  
00069          88  ADDITIONAL-TEXT                VALUE 'A'.            ELTMSA  
00070          88  BLANK-LINE-NEEDED              VALUE 'B'.            ELTMSA  
00071      05  CONTINUED-PROCESSING-SW  PIC X(01) VALUE SPACE.          ELTMSA  
00072          88  DONE-PROCESSING                VALUE 'D'.            ELTMSA  
00073          88  PROCESSING-CMF-TEXT            VALUE 'P'.            ELTMSA  
00074      05  WS-GMSB-SWITCH           PIC X(01) VALUE 'N'.            ELTMSA  
00075          88  GMSB-IS-PRESENT                VALUE 'Y'.            ELTMSA  
00076      05  WS-GMSR-SWITCH           PIC X(01) VALUE 'N'.            ELTMSA  
00077          88  GMSR-IS-PRESENT                VALUE 'Y'.            ELTMSA  
00078      05  WS-GMSC-SWITCH           PIC X(01) VALUE 'N'.            ELTMSA  
00079          88  GMSC-IS-PRESENT                VALUE 'Y'.            ELTMSA  
00080      05  WS-PERIOD-SWITCH         PIC X(01) VALUE 'N'.            ELTMSA  
00081          88  PERIOD-NEEDED                  VALUE 'Y'.            ELTMSA  
00082      05  WS-TABULAR-SWITCH        PIC X(01) VALUE 'N'.            ELTMSA  
00083          88  TABULAR-IS-UNDEFINED           VALUE 'Y'.            ELTMSA  
00084      05  WS-APPROVAL-SOURCE-SW    PIC X     VALUE SPACE.          ELTMSA  
00085          88  NOT-HOLDING-APPROVAL-SRCE      VALUE 'N'.            ELTMSA  
00086          88  HOLDING-APPROVAL-SOURCE        VALUE 'H'.            ELTMSA  
00087                                                                   ELTMSA  
00088  01  WS-HOLD-AREA.                                                ELTMSA  
00089      05  WS-GMSB-PROV-ID               PIC X(06)  VALUE SPACES.   ELTMSA  
00090      05  WS-GMSB-PROV-SLOT-NO   COMP-3 PIC S9(07) VALUE +0.       ELTMSA  
00091      05  WS-GMSR-PROV-ID               PIC X(06)  VALUE SPACES.   ELTMSA  
00092      05  WS-GMSR-PROV-SLOT-NO   COMP-3 PIC S9(07) VALUE +0.       ELTMSA  
00093      05  WS-GMSC-PROV-ID               PIC X(06)  VALUE SPACES.   ELTMSA  
00094      05  WS-GMSC-PROV-SLOT-NO   COMP-3 PIC S9(07) VALUE +0.       ELTMSA  
00095                                                                   ELTMSA  
00096 **************************************************************    ELTMSA  
00097 *** PROGRAM CONSTANTS                                             ELTMSA  
00098 **************************************************************    ELTMSA  
00099      05  WS-GRP                   PIC X(06) VALUE 'GROUP'.        ELTMSA  
00100      05  WS-GCCP                  PIC X(06) VALUE '#GCCP '.       ELTMSA  
00101      05  WS-GMSB                  PIC X(06) VALUE '#GMSB '.       ELTMSA  
00102      05  WS-GMSR                  PIC X(06) VALUE '#GMSR '.       ELTMSA  
00103      05  WS-GMSC                  PIC X(06) VALUE '#GMSC '.       ELTMSA  
00104      05  WS-INST                  PIC X(13) VALUE 'INSTITUTIONAL'.ELTMSA  
00105      05  WS-PROF                  PIC X(13) VALUE 'PROFESSIONAL '.ELTMSA  
00106      05  WS-SUPP                  PIC X(13) VALUE 'SUPPLEMENTAL '.ELTMSA  
00107      05  WS-APPROVAL              PIC X(09) VALUE 'APPROVAL.'.    ELTMSA  
00108                                                                   ELTMSA  
00109 **************************************************************    ELTMSA  
00110 *** H E A D E R   L I N E                                         ELTMSA  
00111 **************************************************************    ELTMSA  
00112      05  WS-HEADER-LINE.                                          ELTMSA  
00113          10  FILLER               PIC X(16) VALUE SPACES.         ELTMSA  
00114          10  FILLER               PIC X(34) VALUE                 ELTMSA  
00115          'MEDICAL SERVICES ADVISORY PROGRAM '.                    ELTMSA  
00116          10  WS-HDR-LINE-BCBSMM   PIC X(13) VALUE SPACES.         ELTMSA  
00117          10  FILLER               PIC X(16) VALUE SPACES.         ELTMSA  
00118                                                                   ELTMSA  
00119 **************************************************************    ELTMSA  
00120 *** SCREEN BODY LINES                                             ELTMSA  
00121 **************************************************************    ELTMSA  
00122  01  WS-SCREEN-LINE-AREA.                                         ELTMSA  
00123      05  WS-APPRVL-SRCE-LINE.                                     ELTMSA  
00124          10  FILLER               PIC X(35) VALUE                 ELTMSA  
00125          'MEDICAL SERVICES ADVISORY REQUIRES '.                   ELTMSA  
00126          10  FILLER               PIC X(44) VALUE SPACES.         ELTMSA  
00127                                                                   ELTMSA  
00128      05  WS-BCBSMM-IND-LINE.                                      ELTMSA  
00129          10  FILLER               PIC X(79) VALUE                 ELTMSA  
00130          'FOR MEDICAL SERVICES ADVISORY '.                        ELTMSA  
00131                                                                   ELTMSA  
00132      05  WS-BENE-REDUCT-LINE.                                     ELTMSA  
00133          10  FILLER               PIC X(47) VALUE                 ELTMSA  
00134          'DENIED OR REDUCED BENEFITS DUE TO THIS PROGRAM:'.       ELTMSA  
00135          10  FILLER               PIC X(32) VALUE SPACES.         ELTMSA  
00136                                                                   ELTMSA  
00137      05  WS-SPILL-OVER-LINE.                                      ELTMSA  
00138          10  FILLER               PIC X(79) VALUE                 ELTMSA  
00139          'UNPAID SERVICES AFTER BASIC BENEFITS REDUCTION ARE '.   ELTMSA  
00140                                                                   ELTMSA  
00141      05  WS-MSA-APPLIES.                                          ELTMSA  
00142          10  FILLER               PIC  X(44) VALUE                ELTMSA  
00143          'MEDICAL SERVICES ADVISORY PROGRAM APPLIES TO'.          ELTMSA  
00144          10  FILLER               PIC  X(35) VALUE SPACES.        ELTMSA  
00145                                                                   ELTMSA  
00146 **************************************************************    ELTMSA  
00147 ** SPECIAL MESSAGE FOR THE VOLUNTARY AND NOT APPLICABLE CASES     ELTMSA  
00148 ** ALSO THE FIXED TEXT FOR TABULARS GMSB,GMSR                     ELTMSA  
00149 **************************************************************    ELTMSA  
00150      05  WS-NOT-APPLICABLE-MSG.                                   ELTMSA  
00151          10  FILLER               PIC  X(52) VALUE                ELTMSA  
00152          'MEDICAL SERVICES ADVISORY PROGRAM IS NOT APPLICABLE.'.  ELTMSA  
00153          10  FILLER               PIC  X(27) VALUE SPACES.        ELTMSA  
00154                                                                   ELTMSA  
00155      05  WS-VOLUNTARY-MSG.                                        ELTMSA  
00156          10  FILLER               PIC  X(47) VALUE                ELTMSA  
00157          'MEDICAL SERVICES ADVISORY PROGRAM IS VOLUNTARY.'.       ELTMSA  
00158          10  FILLER               PIC  X(32) VALUE SPACES.        ELTMSA  
00159                                                                   ELTMSA  
00160      05  WS-DISCLAIMER-MSG.                                       ELTMSA  
00161          10  FILLER               PIC  X(79) VALUE                ELTMSA  
00162          '*** SUBJECT TO OTHER CONTRACT LIMITATIONS ***'.         ELTMSA  
00163                                                                   ELTMSA  
00164      05  WS-SPEC-SERV-MSG.                                        ELTMSA  
00165          10  FILLER               PIC X(79) VALUE                 ELTMSA  
00166          'THERE ARE SPECIAL RELATED SERVICES INCLUDED IN THIS COSTELTMSA  
00167 -        ' CONTAINMENT PROGRAM.'.                                 ELTMSA  
00168                                                                   ELTMSA  
00169      05  WS-SPEC-PROC-MSG.                                        ELTMSA  
00170          10  FILLER               PIC X(79) VALUE                 ELTMSA  
00171          'THERE ARE SPECIAL PROCEDURES INCLUDED IN THIS COST CONTAELTMSA  
00172 -        'INMENT PROGRAM.'.                                       ELTMSA  
00173                                                                   ELTMSA  
00174      05  WS-SPEC-PROV-MSG.                                        ELTMSA  
00175          10  FILLER               PIC X(79) VALUE                 ELTMSA  
00176          'THERE ARE SPECIAL PROVIDERS INCLUDED IN THIS COST CONTAIELTMSA  
00177 -        'NMENT PROGRAM.'.                                        ELTMSA  
00178                                                                   ELTMSA  
00179      05  WS-NOT-APPLY-TO-LOB-BC.                                  ELTMSA  
00180          10  FILLER               PIC X(53) VALUE                 ELTMSA  
00181        'MEDICAL SERVICES ADVISORY PROGRAM DOES NOT APPLY FOR '.   ELTMSA  
00182          10  FILLER               PIC X(26) VALUE                 ELTMSA  
00183        'INSTITUTIONAL BENEFITS.'.                                 ELTMSA  
00184                                                                   ELTMSA  
00185      05  WS-NOT-APPLY-TO-LOB-BS.                                  ELTMSA  
00186          10  FILLER               PIC X(53) VALUE                 ELTMSA  
00187        'MEDICAL SERVICES ADVISORY PROGRAM DOES NOT APPLY FOR '.   ELTMSA  
00188          10  FILLER               PIC X(26) VALUE                 ELTMSA  
00189        'PROFESSIONAL BENEFITS.'.                                  ELTMSA  
00190                                                                   ELTMSA  
00191      05  WS-NOT-APPLY-TO-LOB-MM.                                  ELTMSA  
00192          10  FILLER               PIC X(53) VALUE                 ELTMSA  
00193        'MEDICAL SERVICES ADVISORY PROGRAM DOES NOT APPLY FOR '.   ELTMSA  
00194          10  FILLER               PIC X(26) VALUE                 ELTMSA  
00195        'SUPPLEMENTAL BENEFITS.'.                                  ELTMSA  
00196                                                                   ELTMSA  
00197  LINKAGE SECTION.                                                 ELTMSA  
00198  01  DFHCOMMAREA.                                                 ELTMSA  
00199      COPY ELSCOMMC.                                               ELTMSA  
00200 /                                                                 ELTMSA  
00201      COPY ELSCIA2C.                                               ELTMSA  
00202 /                                                                 ELTMSA  
00203      COPY ELSCMDSC.                                               ELTMSA  
00204 /                                                                 ELTMSA  
00205      COPY ELSCMIFC.                                               ELTMSA  
00206 /                                                                 ELTMSA  
00207      COPY ELSIOPMC.                                               ELTMSA  
00208 /                                                                 ELTMSA  
00209      COPY ELSKEYSC.                                               ELTMSA  
00210 /                                                                 ELTMSA  
00211      COPY ELSOUTPC.                                               ELTMSA  
00212 /                                                                 ELTMSA  
00213      COPY ELSSRTPC.                                               ELTMSA  
00214 /                                                                 ELTMSA  
00215      COPY ELSTCWAC.                                               ELTMSA  
00216 /                                                                 ELTMSA  
00217      COPY ELSSSCBC.                                               ELTMSA  
00218 /                                                                 ELTMSA  
00219  01  GROUP-SPECIFIC-REC-AREA.                                     ELTMSA  
00220      COPY GCGROUPC.                                               ELTMSA  
00221 /                                                                 ELTMSA  
00222  01  GCCP-TABULAR-REC-AREA.                                       ELTMSA  
00223      COPY GCTGCCPC.                                               ELTMSA  
00224      EJECT                                                        ELTMSA  
00225  PROCEDURE DIVISION.                                              ELTMSA  
00226 ************************************************************      ELTMSA  
00227 *                                                          *      ELTMSA  
00228 *                    PROCEDURE DIVISION                    *      ELTMSA  
00229 *                                                          *      ELTMSA  
00230 ************************************************************      ELTMSA  
00231                                                                   ELTMSA  
00232                                                                   ELTMSA  
00233 ************************************************************      ELTMSA  
00234 *                                                          *      ELTMSA  
00235 *        MEDICAL SERVICES ADVISORY                         *      ELTMSA  
00236 *                                                          *      ELTMSA  
00237 ************************************************************      ELTMSA  
00238  MEDICAL-SERVICES-ADVISORY.                                       ELTMSA  
00239      PERFORM INITIALIZATION.                                      ELTMSA  
00240      PERFORM PROCESS.                                             ELTMSA  
00241      GOBACK.                                                      ELTMSA  
00242                                                                   ELTMSA  
00243                                                                   ELTMSA  
00244 ************************************************************      ELTMSA  
00245 *                                                          *      ELTMSA  
00246 *        INITIALIZATION                                    *      ELTMSA  
00247 *                                                          *      ELTMSA  
00248 ************************************************************      ELTMSA  
00249  INITIALIZATION.                                                  ELTMSA  
00250      SET NOT-HOLDING-APPROVAL-SRCE TO TRUE.                       ELTMSA  
00251      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTMSA  
00252      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTMSA  
00253                                                                   ELTMSA  
00254                                                                   ELTMSA  
00255 ************************************************************      ELTMSA  
00256 *                                                          *      ELTMSA  
00257 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTMSA  
00258 *                                                          *      ELTMSA  
00259 ************************************************************      ELTMSA  
00260  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTMSA  
00261      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTMSA  
00262      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTMSA  
00263      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTMSA  
00264                                                                   ELTMSA  
00265                                                                   ELTMSA  
00266 ************************************************************      ELTMSA  
00267 *                                                          *      ELTMSA  
00268 *        CHECK FOR VALID COMMAREA                          *      ELTMSA  
00269 *                                                          *      ELTMSA  
00270 ************************************************************      ELTMSA  
00271  CHECK-FOR-VALID-COMMAREA.                                        ELTMSA  
00272      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTMSA  
00273          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTMSA  
00274                                                                   ELTMSA  
00275                                                                   ELTMSA  
00276 ************************************************************      ELTMSA  
00277 *                                                          *      ELTMSA  
00278 *        SIGNAL INVALID COMMAREA                           *      ELTMSA  
00279 *                                                          *      ELTMSA  
00280 ************************************************************      ELTMSA  
00281  SIGNAL-INVALID-COMMAREA.                                         ELTMSA  
00282      EXEC CICS ABEND                                              ELTMSA  
00283                ABCODE('EL01')                                     ELTMSA  
00284         END-EXEC.                                                 ELTMSA  
00285      EJECT                                                        ELTMSA  
00286                                                                   ELTMSA  
00287                                                                   ELTMSA  
00288 ************************************************************      ELTMSA  
00289 *                                                          *      ELTMSA  
00290 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTMSA  
00291 *                                                          *      ELTMSA  
00292 ************************************************************      ELTMSA  
00293  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTMSA  
00294      IF ECA-CIA-PTR = NULL                                        ELTMSA  
00295          PERFORM SIGNAL-INVALID-CIA                               ELTMSA  
00296      ELSE                                                         ELTMSA  
00297          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTMSA  
00298                                                                   ELTMSA  
00299                                                                   ELTMSA  
00300 ************************************************************      ELTMSA  
00301 *                                                          *      ELTMSA  
00302 *        SIGNAL INVALID CIA                                *      ELTMSA  
00303 *                                                          *      ELTMSA  
00304 ************************************************************      ELTMSA  
00305  SIGNAL-INVALID-CIA.                                              ELTMSA  
00306      EXEC CICS ABEND                                              ELTMSA  
00307                ABCODE('EL02')                                     ELTMSA  
00308         END-EXEC.                                                 ELTMSA  
00309                                                                   ELTMSA  
00310                                                                   ELTMSA  
00311 ************************************************************      ELTMSA  
00312 *                                                          *      ELTMSA  
00313 *        ESTABLISH ADDRESS OF CIA                          *      ELTMSA  
00314 *                                                          *      ELTMSA  
00315 ************************************************************      ELTMSA  
00316  ESTABLISH-ADDRESS-OF-CIA.                                        ELTMSA  
00317      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTMSA  
00318                            ADDRESS OF                             ELTMSA  
00319          CIA-ELS-COMMON-INTERFACE-AREA.                           ELTMSA  
00320      EJECT                                                        ELTMSA  
00321                                                                   ELTMSA  
00322                                                                   ELTMSA  
00323 ************************************************************      ELTMSA  
00324 *                                                          *      ELTMSA  
00325 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTMSA  
00326 *                                                          *      ELTMSA  
00327 ************************************************************      ELTMSA  
00328  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTMSA  
00329      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTMSA  
00330      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMSA  
00331                            ADDRESS OF                             ELTMSA  
00332          SSB-SELECTOR-STATUS-CTL-BLK.                             ELTMSA  
00333      IF CIA-RC-PTR-NULL                                           ELTMSA  
00334          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMSA  
00335                                                                   ELTMSA  
00336                                                                   ELTMSA  
00337 ************************************************************      ELTMSA  
00338 *                                                          *      ELTMSA  
00339 *        SIGNAL UNALLOC AREA ERROR                         *      ELTMSA  
00340 *                                                          *      ELTMSA  
00341 ************************************************************      ELTMSA  
00342  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTMSA  
00343      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTMSA  
00344      PERFORM SIGNAL-ABEND.                                        ELTMSA  
00345                                                                   ELTMSA  
00346                                                                   ELTMSA  
00347 ************************************************************      ELTMSA  
00348 *                                                          *      ELTMSA  
00349 *        SIGNAL ABEND                                      *      ELTMSA  
00350 *                                                          *      ELTMSA  
00351 ************************************************************      ELTMSA  
00352  SIGNAL-ABEND.                                                    ELTMSA  
00353      EXEC CICS ABEND                                              ELTMSA  
00354                ABCODE(CIA-ABCODE)                                 ELTMSA  
00355         END-EXEC.                                                 ELTMSA  
00356      EJECT                                                        ELTMSA  
00357                                                                   ELTMSA  
00358                                                                   ELTMSA  
00359 ************************************************************      ELTMSA  
00360 *                                                          *      ELTMSA  
00361 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTMSA  
00362 *                                                          *      ELTMSA  
00363 ************************************************************      ELTMSA  
00364  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTMSA  
00365      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTMSA  
00366      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTMSA  
00367      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTMSA  
00368      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTMSA  
00369      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTMSA  
00370      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTMSA  
00371      PERFORM ESTABLISH-ADDRESSABILITY-OF-CS.                      ELTMSA  
00372                                                                   ELTMSA  
00373                                                                   ELTMSA  
00374 ************************************************************      ELTMSA  
00375 *                                                          *      ELTMSA  
00376 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTMSA  
00377 *                                                          *      ELTMSA  
00378 ************************************************************      ELTMSA  
00379  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTMSA  
00380      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTMSA  
00381      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMSA  
00382                            ADDRESS OF                             ELTMSA  
00383          CMF-CODES-MANUAL-INTERFACE.                              ELTMSA  
00384      IF CIA-RC-PTR-NULL                                           ELTMSA  
00385          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMSA  
00386      EJECT                                                        ELTMSA  
00387                                                                   ELTMSA  
00388                                                                   ELTMSA  
00389 ************************************************************      ELTMSA  
00390 *                                                          *      ELTMSA  
00391 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTMSA  
00392 *                                                          *      ELTMSA  
00393 ************************************************************      ELTMSA  
00394  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTMSA  
00395      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTMSA  
00396      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMSA  
00397                            ADDRESS OF                             ELTMSA  
00398          COF-OUTPUT-INTERFACE.                                    ELTMSA  
00399      IF CIA-RC-PTR-NULL                                           ELTMSA  
00400          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMSA  
00401      EJECT                                                        ELTMSA  
00402                                                                   ELTMSA  
00403                                                                   ELTMSA  
00404 ************************************************************      ELTMSA  
00405 *                                                          *      ELTMSA  
00406 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTMSA  
00407 *                                                          *      ELTMSA  
00408 ************************************************************      ELTMSA  
00409  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTMSA  
00410      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTMSA  
00411      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMSA  
00412          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELTMSA  
00413      IF CIA-RC-PTR-NULL                                           ELTMSA  
00414          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMSA  
00415      EJECT                                                        ELTMSA  
00416                                                                   ELTMSA  
00417                                                                   ELTMSA  
00418 ************************************************************      ELTMSA  
00419 *                                                          *      ELTMSA  
00420 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTMSA  
00421 *                                                          *      ELTMSA  
00422 ************************************************************      ELTMSA  
00423  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTMSA  
00424      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTMSA  
00425      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMSA  
00426                            ADDRESS OF                             ELTMSA  
00427          TCAR-COMPRESSION-WORK-AREA.                              ELTMSA  
00428      IF CIA-RC-PTR-NULL                                           ELTMSA  
00429          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMSA  
00430      EJECT                                                        ELTMSA  
00431                                                                   ELTMSA  
00432                                                                   ELTMSA  
00433 ************************************************************      ELTMSA  
00434 *                                                          *      ELTMSA  
00435 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTMSA  
00436 *                                                          *      ELTMSA  
00437 ************************************************************      ELTMSA  
00438  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTMSA  
00439      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTMSA  
00440      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMSA  
00441                            ADDRESS OF                             ELTMSA  
00442          KWA-FILE-KEY-WORK-AREA.                                  ELTMSA  
00443      IF CIA-RC-PTR-NULL                                           ELTMSA  
00444          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMSA  
00445      EJECT                                                        ELTMSA  
00446                                                                   ELTMSA  
00447                                                                   ELTMSA  
00448 ************************************************************      ELTMSA  
00449 *                                                          *      ELTMSA  
00450 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELTMSA  
00451 *                                                          *      ELTMSA  
00452 ************************************************************      ELTMSA  
00453  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTMSA  
00454      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTMSA  
00455      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMSA  
00456                            ADDRESS OF                             ELTMSA  
00457          GROUP-SPECIFIC-REC-AREA.                                 ELTMSA  
00458      IF CIA-RC-PTR-NULL                                           ELTMSA  
00459          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMSA  
00460      EJECT                                                        ELTMSA  
00461                                                                   ELTMSA  
00462                                                                   ELTMSA  
00463 ************************************************************      ELTMSA  
00464 *                                                          *      ELTMSA  
00465 *        ESTABLISH ADDRESSABILITY OF CST CONTAINMENT PROGRA*      ELTMSA  
00466 *                                                          *      ELTMSA  
00467 ************************************************************      ELTMSA  
00468  ESTABLISH-ADDRESSABILITY-OF-CS.                                  ELTMSA  
00469      SET CIA-GCTABULR-DDN TO TRUE.                                ELTMSA  
00470      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMSA  
00471                            ADDRESS OF                             ELTMSA  
00472          GCCP-TABULAR-REC-AREA.                                   ELTMSA  
00473      IF CIA-RC-PTR-NULL                                           ELTMSA  
00474          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMSA  
00475      EJECT                                                        ELTMSA  
00476                                                                   ELTMSA  
00477                                                                   ELTMSA  
00478 ************************************************************      ELTMSA  
00479 *                                                          *      ELTMSA  
00480 *        PROCESS                                           *      ELTMSA  
00481 *                                                          *      ELTMSA  
00482 ************************************************************      ELTMSA  
00483  PROCESS.                                                         ELTMSA  
00484      IF GCG-MED-SERV-ADV-PROG-IND  = ZERO                         ELTMSA  
00485          PERFORM GENERATE-NOT-APPLICABLE-MESSAG                   ELTMSA  
00486      ELSE IF GCG-MED-SERV-ADV-PROG-IND  =  '08'                   ELTMSA  
00487          PERFORM GENERATE-VOLUNTARY-MESSAGE                       ELTMSA  
00488      ELSE                                                         ELTMSA  
00489          PERFORM GENERATE-MSA-TEXT.                               ELTMSA  
00490      PERFORM TERMINATE-OUTPUT.                                    ELTMSA  
00491                                                                   ELTMSA  
00492                                                                   ELTMSA  
00493 ************************************************************      ELTMSA  
00494 *                                                          *      ELTMSA  
00495 *        SET UP OUTPUT SUBSCRIPTS                          *      ELTMSA  
00496 *                                                          *      ELTMSA  
00497 ************************************************************      ELTMSA  
00498  SET-UP-OUTPUT-SUBSCRIPTS.                                        ELTMSA  
00499      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMSA  
00500      MOVE +2 TO COF-NBR-HDR-LINES.                                ELTMSA  
00501      EJECT                                                        ELTMSA  
00502                                                                   ELTMSA  
00503                                                                   ELTMSA  
00504 ************************************************************      ELTMSA  
00505 *                                                          *      ELTMSA  
00506 *        EJECT NEW PAGE                                    *      ELTMSA  
00507 *                                                          *      ELTMSA  
00508 ************************************************************      ELTMSA  
00509  EJECT-NEW-PAGE.                                                  ELTMSA  
00510      SET COF-NEW-PAGE     TO TRUE.                                ELTMSA  
00511      MOVE WS-HEADER-LINE  TO COF-HDR-LINE                         ELTMSA  
00512          (COF-NBR-HDR-LINES).                                     ELTMSA  
00513      PERFORM LINK-TO-OUTPUT.                                      ELTMSA  
00514                                                                   ELTMSA  
00515                                                                   ELTMSA  
00516 ************************************************************      ELTMSA  
00517 *                                                          *      ELTMSA  
00518 *        GENERATE NOT APPLICABLE MESSAGE                   *      ELTMSA  
00519 *                                                          *      ELTMSA  
00520 ************************************************************      ELTMSA  
00521  GENERATE-NOT-APPLICABLE-MESSAG.                                  ELTMSA  
00522      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTMSA  
00523      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMSA  
00524      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMSA  
00525      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTMSA  
00526          (COF-NBR-DTL-LINES).                                     ELTMSA  
00527      PERFORM EJECT-NEW-PAGE.                                      ELTMSA  
00528      EJECT                                                        ELTMSA  
00529                                                                   ELTMSA  
00530                                                                   ELTMSA  
00531 ************************************************************      ELTMSA  
00532 *                                                          *      ELTMSA  
00533 *        GENERATE VOLUNTARY MESSAGE                        *      ELTMSA  
00534 *                                                          *      ELTMSA  
00535 ************************************************************      ELTMSA  
00536  GENERATE-VOLUNTARY-MESSAGE.                                      ELTMSA  
00537      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTMSA  
00538      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMSA  
00539      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMSA  
00540      MOVE WS-VOLUNTARY-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).   ELTMSA  
00541      PERFORM EJECT-NEW-PAGE.                                      ELTMSA  
00542      EJECT                                                        ELTMSA  
00543                                                                   ELTMSA  
00544                                                                   ELTMSA  
00545 ************************************************************      ELTMSA  
00546 *                                                          *      ELTMSA  
00547 *        GENERATE MSA TEXT                                 *      ELTMSA  
00548 *                                                          *      ELTMSA  
00549 ************************************************************      ELTMSA  
00550  GENERATE-MSA-TEXT.                                               ELTMSA  
00551      SET WS-POINTER2 TO NULLS.                                    ELTMSA  
00552      SET WS-POINTER3 TO NULLS.                                    ELTMSA  
00553      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTMSA  
00554      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMSA  
00555                            WS-POINTER2.                           ELTMSA  
00556      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTMSA  
00557      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMSA  
00558                            WS-POINTER3.                           ELTMSA  
00559      PERFORM SEARCH-FOR-GCCP-TABULAR.                             ELTMSA  
00560      PERFORM DETERMINE-SELECTION.                                 ELTMSA  
00561      EJECT                                                        ELTMSA  
00562                                                                   ELTMSA  
00563                                                                   ELTMSA  
00564 ************************************************************      ELTMSA  
00565 *                                                          *      ELTMSA  
00566 *        TERMINATE OUTPUT                                  *      ELTMSA  
00567 *                                                          *      ELTMSA  
00568 ************************************************************      ELTMSA  
00569  TERMINATE-OUTPUT.                                                ELTMSA  
00570      SET COF-END TO TRUE.                                         ELTMSA  
00571      PERFORM LINK-TO-OUTPUT.                                      ELTMSA  
00572      EJECT                                                        ELTMSA  
00573                                                                   ELTMSA  
00574                                                                   ELTMSA  
00575 ************************************************************      ELTMSA  
00576 *                                                          *      ELTMSA  
00577 *        SEARCH FOR GCCP TABULAR                           *      ELTMSA  
00578 *                                                          *      ELTMSA  
00579 ************************************************************      ELTMSA  
00580  SEARCH-FOR-GCCP-TABULAR.                                         ELTMSA  
00581      SET GCG-INDEX TO +1.                                         ELTMSA  
00582      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTMSA  
00583         AT END                                                    ELTMSA  
00584              MOVE ZEROES TO KWA-PROVISION-SLOT-NO                 ELTMSA  
00585         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GCCP                     ELTMSA  
00586              MOVE GCG-TAB-ID (GCG-INDEX)                          ELTMSA  
00587                  TO KWA-PROVISION-ID                              ELTMSA  
00588              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTMSA  
00589                  TO KWA-PROVISION-SLOT-NO                         ELTMSA  
00590         END-SEARCH.                                               ELTMSA  
00591      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELTMSA  
00592          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTMSA  
00593      PERFORM GET-GCCP-TABULAR.                                    ELTMSA  
00594      PERFORM SEARCH-THE-GSS-ENTRY.                                ELTMSA  
00595      EJECT                                                        ELTMSA  
00596                                                                   ELTMSA  
00597                                                                   ELTMSA  
00598 ************************************************************      ELTMSA  
00599 *                                                          *      ELTMSA  
00600 *        TRANSLATE APPROVAL SOURCE                         *      ELTMSA  
00601 *                                                          *      ELTMSA  
00602 ************************************************************      ELTMSA  
00603  TRANSLATE-APPROVAL-SOURCE.                                       ELTMSA  
00604      INITIALIZE WS-PERIOD-SWITCH.                                 ELTMSA  
00605      INITIALIZE TCAR-FROM-AREA.                                   ELTMSA  
00606      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMSA  
00607      MOVE WS-APPRVL-SRCE-LINE TO TCAR-FROM-LINE                   ELTMSA  
00608          (TCAR-FROM-SUB).                                         ELTMSA  
00609      ADD  +1 TO TCAR-FROM-SUB.                                    ELTMSA  
00610      SET ADDITIONAL-TEXT TO TRUE.                                 ELTMSA  
00611      IF NOT-HOLDING-APPROVAL-SRCE                                 ELTMSA  
00612          PERFORM GET-APPROVAL-TRANSLATION                         ELTMSA  
00613      ELSE                                                         ELTMSA  
00614          PERFORM USE-EXISTING-APPROVAL-TRANSLAT.                  ELTMSA  
00615      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMSA  
00616      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTMSA  
00617      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMSA  
00618                            WS-POINTER3.                           ELTMSA  
00619      MOVE WS-APPROVAL TO TCAR-FROM-LINE                           ELTMSA  
00620          (TCAR-FROM-SUB).                                         ELTMSA  
00621      PERFORM FINISH-SENTENCE.                                     ELTMSA  
00622      EJECT                                                        ELTMSA  
00623                                                                   ELTMSA  
00624                                                                   ELTMSA  
00625 ************************************************************      ELTMSA  
00626 *                                                          *      ELTMSA  
00627 *        GET APPROVAL TRANSLATION                          *      ELTMSA  
00628 *                                                          *      ELTMSA  
00629 ************************************************************      ELTMSA  
00630  GET-APPROVAL-TRANSLATION.                                        ELTMSA  
00631      MOVE GSS-MS-PROG-SOURCE-IND (GSS-INDEX) TO CMF-CODE-VALUE.   ELTMSA  
00632      MOVE   'MS-PROG-SOURCE-IND' TO CMF-ELEMENT-SYSTEM-NAME.      ELTMSA  
00633      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTMSA  
00634      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTMSA  
00635      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMSA  
00636                            ADDRESS OF CMF-DESCR.                  ELTMSA  
00637      EJECT                                                        ELTMSA  
00638                                                                   ELTMSA  
00639                                                                   ELTMSA  
00640 ************************************************************      ELTMSA  
00641 *                                                          *      ELTMSA  
00642 *        USE EXISTING APPROVAL TRANSLATION                 *      ELTMSA  
00643 *                                                          *      ELTMSA  
00644 ************************************************************      ELTMSA  
00645  USE-EXISTING-APPROVAL-TRANSLAT.                                  ELTMSA  
00646      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTMSA  
00647      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMSA  
00648                            ADDRESS OF CMF-DESCR.                  ELTMSA  
00649      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTMSA  
00650      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMSA  
00651                            WS-POINTER2.                           ELTMSA  
00652      EJECT                                                        ELTMSA  
00653                                                                   ELTMSA  
00654                                                                   ELTMSA  
00655 ************************************************************      ELTMSA  
00656 *                                                          *      ELTMSA  
00657 *        FINISH SENTENCE                                   *      ELTMSA  
00658 *                                                          *      ELTMSA  
00659 ************************************************************      ELTMSA  
00660  FINISH-SENTENCE.                                                 ELTMSA  
00661      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMSA  
00662      PERFORM REFORMAT-AND-WRITE-TEXT.                             ELTMSA  
00663                                                                   ELTMSA  
00664                                                                   ELTMSA  
00665 ************************************************************      ELTMSA  
00666 *                                                          *      ELTMSA  
00667 *        TRANSLATE AND OUTPUT CODE VALUE                   *      ELTMSA  
00668 *                                                          *      ELTMSA  
00669 ************************************************************      ELTMSA  
00670  TRANSLATE-AND-OUTPUT-CODE-VALU.                                  ELTMSA  
00671      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELTMSA  
00672      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMSA  
00673                                                                   ELTMSA  
00674                                                                   ELTMSA  
00675 ************************************************************      ELTMSA  
00676 *                                                          *      ELTMSA  
00677 *        SIGNAL UNDEFINED TABULAR ERROR                    *      ELTMSA  
00678 *                                                          *      ELTMSA  
00679 ************************************************************      ELTMSA  
00680  SIGNAL-UNDEFINED-TABULAR-ERROR.                                  ELTMSA  
00681      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTMSA  
00682      PERFORM SIGNAL-ABEND.                                        ELTMSA  
00683      EJECT                                                        ELTMSA  
00684                                                                   ELTMSA  
00685                                                                   ELTMSA  
00686 ************************************************************      ELTMSA  
00687 *                                                          *      ELTMSA  
00688 *        DETERMINE SELECTION                               *      ELTMSA  
00689 *                                                          *      ELTMSA  
00690 ************************************************************      ELTMSA  
00691  DETERMINE-SELECTION.                                             ELTMSA  
00692      IF SSB-PROV-CLASS-INST OR                                    ELTMSA  
00693                   SSB-PROV-CLASS-BOTH                             ELTMSA  
00694          PERFORM CREATE-INSTITUTIONAL-SCREEN.                     ELTMSA  
00695      IF SSB-PROV-CLASS-PROF OR                                    ELTMSA  
00696                   SSB-PROV-CLASS-BOTH                             ELTMSA  
00697          PERFORM CREATE-PROFESSIONAL-SCREEN.                      ELTMSA  
00698      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL '03' OR '04' OR       ELTMSA  
00699                    '06' OR '08')                                  ELTMSA  
00700          PERFORM CREATE-SUPPLEMENTAL-SCREEN.                      ELTMSA  
00701                                                                   ELTMSA  
00702                                                                   ELTMSA  
00703 ************************************************************      ELTMSA  
00704 *                                                          *      ELTMSA  
00705 *        CREATE INSTITUTIONAL SCREEN                       *      ELTMSA  
00706 *                                                          *      ELTMSA  
00707 ************************************************************      ELTMSA  
00708  CREATE-INSTITUTIONAL-SCREEN.                                     ELTMSA  
00709      MOVE WS-INST TO WS-HDR-LINE-BCBSMM.                          ELTMSA  
00710      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTMSA  
00711      IF GSS-MS-BC-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTMSA  
00712          ZEROES                                                   ELTMSA  
00713                 AND LOW-VALUES                                    ELTMSA  
00714          PERFORM GENERATE-INSTITUTIONAL-TEXT                      ELTMSA  
00715      ELSE                                                         ELTMSA  
00716          PERFORM GENERATE-INSTITUTIONAL-NOT-APP.                  ELTMSA  
00717      EJECT                                                        ELTMSA  
00718                                                                   ELTMSA  
00719                                                                   ELTMSA  
00720 ************************************************************      ELTMSA  
00721 *                                                          *      ELTMSA  
00722 *        GENERATE INSTITUTIONAL TEXT                       *      ELTMSA  
00723 *                                                          *      ELTMSA  
00724 ************************************************************      ELTMSA  
00725  GENERATE-INSTITUTIONAL-TEXT.                                     ELTMSA  
00726      PERFORM EJECT-NEW-PAGE.                                      ELTMSA  
00727      PERFORM TRANSLATE-DISPLAY-MSA-IND.                           ELTMSA  
00728      IF GSS-MS-PROG-SOURCE-IND (GSS-INDEX) NOT EQUAL SPACES       ELTMSA  
00729                 AND ZEROES AND LOW-VALUES                         ELTMSA  
00730          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMSA  
00731      PERFORM TRANSLATE-BC-IND.                                    ELTMSA  
00732      SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTMSA  
00733      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTMSA  
00734      IF GSS-MS-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES AND       ELTMSA  
00735                 ZEROES AND LOW-VALUES                             ELTMSA  
00736          PERFORM TRANSLATE-MSA-CALC-METHOD.                       ELTMSA  
00737      IF GSS-MS-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTMSA  
00738          SPACES                                                   ELTMSA  
00739                 AND ZEROES AND LOW-VALUES                         ELTMSA  
00740          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTMSA  
00741      IF ((GSS-MS-BC-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL         ELTMSA  
00742          SPACES AND                                               ELTMSA  
00743                   ZEROES AND LOW-VALUES)) OR                      ELTMSA  
00744                 ((GSS-MS-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT       ELTMSA  
00745          EQUAL SPACES AND                                         ELTMSA  
00746                   ZEROES AND LOW-VALUES)) OR                      ELTMSA  
00747                ((GSS-MS-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT      ELTMSA  
00748          EQUAL SPACES AND                                         ELTMSA  
00749                   ZEROES AND LOW-VALUES))                         ELTMSA  
00750          PERFORM GENERATE-BC-BENE-REDUCT-TEXT.                    ELTMSA  
00751      IF GSS-MS-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL ZEROES AND    ELTMSA  
00752          SPACES                                                   ELTMSA  
00753                 AND LOW-VALUES                                    ELTMSA  
00754          PERFORM GENERATE-SPILL-OVER-TEXT.                        ELTMSA  
00755      PERFORM GENERATE-OTHER-TABULAR-INFO.                         ELTMSA  
00756      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTMSA  
00757      EJECT                                                        ELTMSA  
00758                                                                   ELTMSA  
00759                                                                   ELTMSA  
00760 ************************************************************      ELTMSA  
00761 *                                                          *      ELTMSA  
00762 *        GENERATE INSTITUTIONAL NOT APPLICABLE             *      ELTMSA  
00763 *                                                          *      ELTMSA  
00764 ************************************************************      ELTMSA  
00765  GENERATE-INSTITUTIONAL-NOT-APP.                                  ELTMSA  
00766      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMSA  
00767      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMSA  
00768      MOVE WS-NOT-APPLY-TO-LOB-BC TO COF-DTL-LINE                  ELTMSA  
00769          (COF-NBR-DTL-LINES).                                     ELTMSA  
00770      PERFORM EJECT-NEW-PAGE.                                      ELTMSA  
00771      EJECT                                                        ELTMSA  
00772                                                                   ELTMSA  
00773                                                                   ELTMSA  
00774 ************************************************************      ELTMSA  
00775 *                                                          *      ELTMSA  
00776 *        GENERATE DISCLAIMER MESSAGE                       *      ELTMSA  
00777 *                                                          *      ELTMSA  
00778 ************************************************************      ELTMSA  
00779  GENERATE-DISCLAIMER-MESSAGE.                                     ELTMSA  
00780      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMSA  
00781      MOVE WS-DISCLAIMER-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).  ELTMSA  
00782      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMSA  
00783      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMSA  
00784      PERFORM LINK-TO-OUTPUT.                                      ELTMSA  
00785      EJECT                                                        ELTMSA  
00786                                                                   ELTMSA  
00787                                                                   ELTMSA  
00788 ************************************************************      ELTMSA  
00789 *                                                          *      ELTMSA  
00790 *        TRANSLATE DISPLAY MSA IND                         *      ELTMSA  
00791 *                                                          *      ELTMSA  
00792 ************************************************************      ELTMSA  
00793  TRANSLATE-DISPLAY-MSA-IND.                                       ELTMSA  
00794      INITIALIZE TCAR-FROM-AREA.                                   ELTMSA  
00795      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMSA  
00796      MOVE WS-MSA-APPLIES TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELTMSA  
00797      ADD  +1 TO TCAR-FROM-SUB.                                    ELTMSA  
00798      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMSA  
00799      SET PERIOD-NEEDED TO TRUE.                                   ELTMSA  
00800      MOVE GCG-MED-SERV-ADV-PROG-IND TO CMF-CODE-VALUE.            ELTMSA  
00801      MOVE 'MED-SERV-ADV-PROG-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMSA  
00802      MOVE WS-GRP TO CMF-RECORD-PREFIX.                            ELTMSA  
00803      EXEC CICS LINK                                               ELTMSA  
00804                PROGRAM ('ELUCMIF')                                ELTMSA  
00805                COMMAREA (DFHCOMMAREA)                             ELTMSA  
00806         END-EXEC.                                                 ELTMSA  
00807      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMSA  
00808      MOVE SPACE TO ADDITIONAL-TEXT-SWITCH.                        ELTMSA  
00809      EJECT                                                        ELTMSA  
00810                                                                   ELTMSA  
00811                                                                   ELTMSA  
00812 ************************************************************      ELTMSA  
00813 *                                                          *      ELTMSA  
00814 *        TRANSLATE BC IND                                  *      ELTMSA  
00815 *                                                          *      ELTMSA  
00816 ************************************************************      ELTMSA  
00817  TRANSLATE-BC-IND.                                                ELTMSA  
00818      INITIALIZE TCAR-FROM-AREA.                                   ELTMSA  
00819      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMSA  
00820      MOVE WS-BCBSMM-IND-LINE TO TCAR-FROM-LINE                    ELTMSA  
00821          (TCAR-FROM-SUB).                                         ELTMSA  
00822      ADD  +1 TO TCAR-FROM-SUB.                                    ELTMSA  
00823      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMSA  
00824      SET PERIOD-NEEDED TO TRUE.                                   ELTMSA  
00825      MOVE GSS-MS-BC-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTMSA  
00826      MOVE   'MS-BC-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTMSA  
00827      PERFORM TRANSLATE-AND-OUTPUT-CODE-VALU.                      ELTMSA  
00828      EJECT                                                        ELTMSA  
00829                                                                   ELTMSA  
00830                                                                   ELTMSA  
00831 ************************************************************      ELTMSA  
00832 *                                                          *      ELTMSA  
00833 *        TRANSLATE MSA CALC METHOD                         *      ELTMSA  
00834 *                                                          *      ELTMSA  
00835 ************************************************************      ELTMSA  
00836  TRANSLATE-MSA-CALC-METHOD.                                       ELTMSA  
00837      INITIALIZE TCAR-FROM-AREA.                                   ELTMSA  
00838      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMSA  
00839      SET PERIOD-NEEDED TO TRUE.                                   ELTMSA  
00840      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMSA  
00841      MOVE GSS-MS-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.       ELTMSA  
00842      MOVE 'MS-CALC-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.            ELTMSA  
00843      PERFORM TRANSLATE-AND-OUTPUT-CODE-VALU.                      ELTMSA  
00844      EJECT                                                        ELTMSA  
00845                                                                   ELTMSA  
00846                                                                   ELTMSA  
00847 ************************************************************      ELTMSA  
00848 *                                                          *      ELTMSA  
00849 *        GENERATE ACCUM TABULAR DATA                       *      ELTMSA  
00850 *                                                          *      ELTMSA  
00851 ************************************************************      ELTMSA  
00852  GENERATE-ACCUM-TABULAR-DATA.                                     ELTMSA  
00853      PERFORM GENERATE-COINSURANCE.                                ELTMSA  
00854      PERFORM GENERATE-COPAY.                                      ELTMSA  
00855      PERFORM GENERATE-DEDUCTIBLE.                                 ELTMSA  
00856      PERFORM GENERATE-MAXIMUM.                                    ELTMSA  
00857      EJECT                                                        ELTMSA  
00858                                                                   ELTMSA  
00859                                                                   ELTMSA  
00860 ************************************************************      ELTMSA  
00861 *                                                          *      ELTMSA  
00862 *        GENERATE COMBINED BENEFITS REDUCTION TEXT         *      ELTMSA  
00863 *                                                          *      ELTMSA  
00864 ************************************************************      ELTMSA  
00865  GENERATE-COMBINED-BENEFITS-RED.                                  ELTMSA  
00866      MOVE 'MS' TO SRP-COST-CONT-TYPE.                             ELTMSA  
00867      MOVE 'MEDICAL SERVICES ADVISORY PROGRAM' TO SRP-CCP-NAME.    ELTMSA  
00868      MOVE GSS-MS-COMB-BENE-REDUCT-IND (GSS-INDEX)                 ELTMSA  
00869                  TO SRP-CCP-COMB-BENE-REDUCT-IND.                 ELTMSA  
00870      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELTMSA  
00871      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMSA  
00872                            ADDRESS OF                             ELTMSA  
00873          GCCP-TABULAR-REC-AREA.                                   ELTMSA  
00874      PERFORM CALL-CBRI-INTERFACE.                                 ELTMSA  
00875      EJECT                                                        ELTMSA  
00876                                                                   ELTMSA  
00877                                                                   ELTMSA  
00878 ************************************************************      ELTMSA  
00879 *                                                          *      ELTMSA  
00880 *        GENERATE BC BENE REDUCT TEXT                      *      ELTMSA  
00881 *                                                          *      ELTMSA  
00882 ************************************************************      ELTMSA  
00883  GENERATE-BC-BENE-REDUCT-TEXT.                                    ELTMSA  
00884      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMSA  
00885      MOVE WS-BENE-REDUCT-LINE TO COF-DTL-LINE                     ELTMSA  
00886          (COF-NBR-DTL-LINES).                                     ELTMSA  
00887      PERFORM LINK-TO-OUTPUT.                                      ELTMSA  
00888      INITIALIZE ADDITIONAL-TEXT-SWITCH.                           ELTMSA  
00889      INITIALIZE WS-PERIOD-SWITCH.                                 ELTMSA  
00890      INITIALIZE TCAR-FROM-AREA.                                   ELTMSA  
00891      IF GSS-MS-BC-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMSA  
00892          SPACES                                                   ELTMSA  
00893                 AND ZEROES AND LOW-VALUES                         ELTMSA  
00894          PERFORM TRANSLATE-BC-DEDUCT-IND.                         ELTMSA  
00895      IF GSS-MS-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMSA  
00896          SPACES                                                   ELTMSA  
00897                 AND ZEROES AND LOW-VALUES                         ELTMSA  
00898          PERFORM TRANSLATE-BC-OPEX-APPLIC.                        ELTMSA  
00899      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMSA  
00900      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMSA  
00901      PERFORM LINK-TO-OUTPUT.                                      ELTMSA  
00902      IF GSS-MS-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTMSA  
00903          SPACES                                                   ELTMSA  
00904                 AND ZEROES AND LOW-VALUES                         ELTMSA  
00905          PERFORM TRANSLATE-BC-OPEX-OVERRIDE.                      ELTMSA  
00906      EJECT                                                        ELTMSA  
00907                                                                   ELTMSA  
00908                                                                   ELTMSA  
00909 ************************************************************      ELTMSA  
00910 *                                                          *      ELTMSA  
00911 *        TRANSLATE BC DEDUCT IND                           *      ELTMSA  
00912 *                                                          *      ELTMSA  
00913 ************************************************************      ELTMSA  
00914  TRANSLATE-BC-DEDUCT-IND.                                         ELTMSA  
00915      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMSA  
00916      MOVE GSS-MS-BC-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTMSA  
00917          CMF-CODE-VALUE.                                          ELTMSA  
00918      MOVE 'MS-BC-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMSA  
00919      PERFORM TRANSLATE-AND-OUTPUT-CODE-VALU.                      ELTMSA  
00920      EJECT                                                        ELTMSA  
00921                                                                   ELTMSA  
00922                                                                   ELTMSA  
00923 ************************************************************      ELTMSA  
00924 *                                                          *      ELTMSA  
00925 *        TRANSLATE BC OPEX APPLIC                          *      ELTMSA  
00926 *                                                          *      ELTMSA  
00927 ************************************************************      ELTMSA  
00928  TRANSLATE-BC-OPEX-APPLIC.                                        ELTMSA  
00929      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMSA  
00930      MOVE GSS-MS-BC-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTMSA  
00931          CMF-CODE-VALUE.                                          ELTMSA  
00932      MOVE 'MS-BC-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMSA  
00933      PERFORM TRANSLATE-AND-OUTPUT-CODE-VALU.                      ELTMSA  
00934      EJECT                                                        ELTMSA  
00935                                                                   ELTMSA  
00936                                                                   ELTMSA  
00937 ************************************************************      ELTMSA  
00938 *                                                          *      ELTMSA  
00939 *        TRANSLATE BC OPEX OVERRIDE                        *      ELTMSA  
00940 *                                                          *      ELTMSA  
00941 ************************************************************      ELTMSA  
00942  TRANSLATE-BC-OPEX-OVERRIDE.                                      ELTMSA  
00943      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMSA  
00944      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMSA  
00945      SET PERIOD-NEEDED TO TRUE.                                   ELTMSA  
00946      MOVE GSS-MS-BC-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTMSA  
00947          CMF-CODE-VALUE.                                          ELTMSA  
00948      MOVE 'MS-BC-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTMSA  
00949      PERFORM TRANSLATE-AND-OUTPUT-CODE-VALU.                      ELTMSA  
00950      EJECT                                                        ELTMSA  
00951                                                                   ELTMSA  
00952                                                                   ELTMSA  
00953 ************************************************************      ELTMSA  
00954 *                                                          *      ELTMSA  
00955 *        CREATE PROFESSIONAL SCREEN                        *      ELTMSA  
00956 *                                                          *      ELTMSA  
00957 ************************************************************      ELTMSA  
00958  CREATE-PROFESSIONAL-SCREEN.                                      ELTMSA  
00959      MOVE WS-PROF TO WS-HDR-LINE-BCBSMM.                          ELTMSA  
00960      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTMSA  
00961      IF GSS-MS-BS-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTMSA  
00962          ZEROES                                                   ELTMSA  
00963                 AND LOW-VALUES                                    ELTMSA  
00964          PERFORM GENERATE-PROFESSIONAL-TEXT                       ELTMSA  
00965      ELSE                                                         ELTMSA  
00966          PERFORM GENERATE-PROFESSIONAL-NOT-APPL.                  ELTMSA  
00967      EJECT                                                        ELTMSA  
00968                                                                   ELTMSA  
00969                                                                   ELTMSA  
00970 ************************************************************      ELTMSA  
00971 *                                                          *      ELTMSA  
00972 *        GENERATE PROFESSIONAL TEXT                        *      ELTMSA  
00973 *                                                          *      ELTMSA  
00974 ************************************************************      ELTMSA  
00975  GENERATE-PROFESSIONAL-TEXT.                                      ELTMSA  
00976      PERFORM EJECT-NEW-PAGE.                                      ELTMSA  
00977      PERFORM TRANSLATE-DISPLAY-MSA-IND.                           ELTMSA  
00978      IF GSS-MS-PROG-SOURCE-IND (GSS-INDEX) NOT EQUAL SPACES       ELTMSA  
00979                 AND ZEROES AND LOW-VALUES                         ELTMSA  
00980          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMSA  
00981      PERFORM TRANSLATE-BS-IND.                                    ELTMSA  
00982      SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTMSA  
00983      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTMSA  
00984      IF GSS-MS-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES AND       ELTMSA  
00985                 ZEROES AND LOW-VALUES                             ELTMSA  
00986          PERFORM TRANSLATE-MSA-CALC-METHOD.                       ELTMSA  
00987      IF GSS-MS-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTMSA  
00988          SPACES                                                   ELTMSA  
00989                 AND ZEROES AND LOW-VALUES                         ELTMSA  
00990          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTMSA  
00991      IF ((GSS-MS-BS-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL         ELTMSA  
00992          SPACES AND                                               ELTMSA  
00993                   ZEROES AND LOW-VALUES)) OR                      ELTMSA  
00994                 ((GSS-MS-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT       ELTMSA  
00995          EQUAL SPACES AND                                         ELTMSA  
00996                   ZEROES AND LOW-VALUES)) OR                      ELTMSA  
00997                ((GSS-MS-BS-OPEX-OVERRIDE-IND (GSS-INDEX) NOT      ELTMSA  
00998          EQUAL SPACES AND                                         ELTMSA  
00999                   ZEROES AND LOW-VALUES))                         ELTMSA  
01000          PERFORM GENERATE-BS-BENE-REDUCT-TEXT.                    ELTMSA  
01001      IF GSS-MS-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL ZEROES AND    ELTMSA  
01002          SPACES                                                   ELTMSA  
01003                 AND LOW-VALUES                                    ELTMSA  
01004          PERFORM GENERATE-SPILL-OVER-TEXT.                        ELTMSA  
01005      PERFORM GENERATE-OTHER-TABULAR-INFO.                         ELTMSA  
01006      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTMSA  
01007                                                                   ELTMSA  
01008                                                                   ELTMSA  
01009 ************************************************************      ELTMSA  
01010 *                                                          *      ELTMSA  
01011 *        GENERATE PROFESSIONAL NOT APPLICABLE              *      ELTMSA  
01012 *                                                          *      ELTMSA  
01013 ************************************************************      ELTMSA  
01014  GENERATE-PROFESSIONAL-NOT-APPL.                                  ELTMSA  
01015      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMSA  
01016      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMSA  
01017      MOVE WS-NOT-APPLY-TO-LOB-BS TO COF-DTL-LINE                  ELTMSA  
01018          (COF-NBR-DTL-LINES).                                     ELTMSA  
01019      PERFORM EJECT-NEW-PAGE.                                      ELTMSA  
01020      EJECT                                                        ELTMSA  
01021                                                                   ELTMSA  
01022                                                                   ELTMSA  
01023 ************************************************************      ELTMSA  
01024 *                                                          *      ELTMSA  
01025 *        TRANSLATE BS IND                                  *      ELTMSA  
01026 *                                                          *      ELTMSA  
01027 ************************************************************      ELTMSA  
01028  TRANSLATE-BS-IND.                                                ELTMSA  
01029      INITIALIZE TCAR-FROM-AREA.                                   ELTMSA  
01030      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMSA  
01031      MOVE WS-BCBSMM-IND-LINE TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELTMSA  
01032      ADD  +1 TO TCAR-FROM-SUB.                                    ELTMSA  
01033      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMSA  
01034      SET PERIOD-NEEDED TO TRUE.                                   ELTMSA  
01035      MOVE GSS-MS-BS-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTMSA  
01036      MOVE   'MS-BS-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTMSA  
01037      PERFORM TRANSLATE-AND-OUTPUT-CODE-VALU.                      ELTMSA  
01038      EJECT                                                        ELTMSA  
01039                                                                   ELTMSA  
01040                                                                   ELTMSA  
01041 ************************************************************      ELTMSA  
01042 *                                                          *      ELTMSA  
01043 *        GENERATE BS BENE REDUCT TEXT                      *      ELTMSA  
01044 *                                                          *      ELTMSA  
01045 ************************************************************      ELTMSA  
01046  GENERATE-BS-BENE-REDUCT-TEXT.                                    ELTMSA  
01047      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMSA  
01048      MOVE WS-BENE-REDUCT-LINE TO COF-DTL-LINE                     ELTMSA  
01049          (COF-NBR-DTL-LINES).                                     ELTMSA  
01050      PERFORM LINK-TO-OUTPUT.                                      ELTMSA  
01051      INITIALIZE ADDITIONAL-TEXT-SWITCH.                           ELTMSA  
01052      INITIALIZE WS-PERIOD-SWITCH.                                 ELTMSA  
01053      INITIALIZE TCAR-FROM-AREA.                                   ELTMSA  
01054      IF GSS-MS-BS-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMSA  
01055          SPACES                                                   ELTMSA  
01056                 AND ZEROES AND LOW-VALUES                         ELTMSA  
01057          PERFORM TRANSLATE-BS-DEDUCT-IND.                         ELTMSA  
01058      IF GSS-MS-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMSA  
01059          SPACES                                                   ELTMSA  
01060                 AND ZEROES AND LOW-VALUES                         ELTMSA  
01061          PERFORM TRANSLATE-BS-OPEX-APPLIC.                        ELTMSA  
01062      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMSA  
01063      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMSA  
01064      PERFORM LINK-TO-OUTPUT.                                      ELTMSA  
01065      IF GSS-MS-BS-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTMSA  
01066          SPACES                                                   ELTMSA  
01067                 AND ZEROES AND LOW-VALUES                         ELTMSA  
01068          PERFORM TRANSLATE-BS-OPEX-OVERRIDE.                      ELTMSA  
01069      EJECT                                                        ELTMSA  
01070                                                                   ELTMSA  
01071                                                                   ELTMSA  
01072 ************************************************************      ELTMSA  
01073 *                                                          *      ELTMSA  
01074 *        TRANSLATE BS DEDUCT IND                           *      ELTMSA  
01075 *                                                          *      ELTMSA  
01076 ************************************************************      ELTMSA  
01077  TRANSLATE-BS-DEDUCT-IND.                                         ELTMSA  
01078      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMSA  
01079      MOVE GSS-MS-BS-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTMSA  
01080          CMF-CODE-VALUE.                                          ELTMSA  
01081      MOVE 'MS-BS-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMSA  
01082      PERFORM TRANSLATE-AND-OUTPUT-CODE-VALU.                      ELTMSA  
01083      EJECT                                                        ELTMSA  
01084                                                                   ELTMSA  
01085                                                                   ELTMSA  
01086 ************************************************************      ELTMSA  
01087 *                                                          *      ELTMSA  
01088 *        TRANSLATE BS OPEX APPLIC                          *      ELTMSA  
01089 *                                                          *      ELTMSA  
01090 ************************************************************      ELTMSA  
01091  TRANSLATE-BS-OPEX-APPLIC.                                        ELTMSA  
01092      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMSA  
01093      MOVE GSS-MS-BS-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTMSA  
01094          CMF-CODE-VALUE.                                          ELTMSA  
01095      MOVE 'MS-BS-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMSA  
01096      PERFORM TRANSLATE-AND-OUTPUT-CODE-VALU.                      ELTMSA  
01097      EJECT                                                        ELTMSA  
01098                                                                   ELTMSA  
01099                                                                   ELTMSA  
01100 ************************************************************      ELTMSA  
01101 *                                                          *      ELTMSA  
01102 *        TRANSLATE BS OPEX OVERRIDE                        *      ELTMSA  
01103 *                                                          *      ELTMSA  
01104 ************************************************************      ELTMSA  
01105  TRANSLATE-BS-OPEX-OVERRIDE.                                      ELTMSA  
01106      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMSA  
01107      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMSA  
01108      SET PERIOD-NEEDED TO TRUE.                                   ELTMSA  
01109      MOVE GSS-MS-BS-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTMSA  
01110          CMF-CODE-VALUE.                                          ELTMSA  
01111      MOVE 'MS-BS-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTMSA  
01112      PERFORM TRANSLATE-AND-OUTPUT-CODE-VALU.                      ELTMSA  
01113      EJECT                                                        ELTMSA  
01114                                                                   ELTMSA  
01115                                                                   ELTMSA  
01116 ************************************************************      ELTMSA  
01117 *                                                          *      ELTMSA  
01118 *        CREATE SUPPLEMENTAL SCREEN                        *      ELTMSA  
01119 *                                                          *      ELTMSA  
01120 ************************************************************      ELTMSA  
01121  CREATE-SUPPLEMENTAL-SCREEN.                                      ELTMSA  
01122      MOVE WS-SUPP TO WS-HDR-LINE-BCBSMM.                          ELTMSA  
01123      PERFORM SET-UP-OUTPUT-SUBSCRIPTS.                            ELTMSA  
01124      IF GSS-MS-MM-IND (GSS-INDEX) NOT EQUAL SPACES AND            ELTMSA  
01125          ZEROES                                                   ELTMSA  
01126                 AND LOW-VALUES                                    ELTMSA  
01127          PERFORM GENERATE-SUPPLEMENTAL-TEXT                       ELTMSA  
01128      ELSE                                                         ELTMSA  
01129          PERFORM GENERATE-SUPPLEMENTAL-NOT-APPL.                  ELTMSA  
01130      EJECT                                                        ELTMSA  
01131                                                                   ELTMSA  
01132                                                                   ELTMSA  
01133 ************************************************************      ELTMSA  
01134 *                                                          *      ELTMSA  
01135 *        GENERATE SUPPLEMENTAL TEXT                        *      ELTMSA  
01136 *                                                          *      ELTMSA  
01137 ************************************************************      ELTMSA  
01138  GENERATE-SUPPLEMENTAL-TEXT.                                      ELTMSA  
01139      PERFORM EJECT-NEW-PAGE.                                      ELTMSA  
01140      PERFORM TRANSLATE-DISPLAY-MSA-IND.                           ELTMSA  
01141      IF GSS-MS-PROG-SOURCE-IND (GSS-INDEX) NOT EQUAL SPACES       ELTMSA  
01142                 AND ZEROES AND LOW-VALUES                         ELTMSA  
01143          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMSA  
01144      PERFORM TRANSLATE-MM-IND.                                    ELTMSA  
01145      SET SRP-ACCUM-PROV-CLASS-SUPP TO TRUE.                       ELTMSA  
01146      PERFORM GENERATE-ACCUM-TABULAR-DATA.                         ELTMSA  
01147      IF GSS-MS-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES AND       ELTMSA  
01148                 ZEROES AND LOW-VALUES                             ELTMSA  
01149          PERFORM TRANSLATE-MSA-CALC-METHOD.                       ELTMSA  
01150      IF GSS-MS-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTMSA  
01151          SPACES                                                   ELTMSA  
01152                 AND ZEROES AND LOW-VALUES                         ELTMSA  
01153          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTMSA  
01154      IF ((GSS-MS-MM-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL         ELTMSA  
01155          SPACES AND                                               ELTMSA  
01156                   ZEROES AND LOW-VALUES)) OR                      ELTMSA  
01157                 ((GSS-MS-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT       ELTMSA  
01158          EQUAL SPACES AND                                         ELTMSA  
01159                   ZEROES AND LOW-VALUES)) OR                      ELTMSA  
01160                ((GSS-MS-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT      ELTMSA  
01161          EQUAL SPACES AND                                         ELTMSA  
01162                   ZEROES AND LOW-VALUES))                         ELTMSA  
01163          PERFORM GENERATE-MM-BENE-REDUCT-TEXT.                    ELTMSA  
01164      PERFORM GENERATE-OTHER-TABULAR-INFO.                         ELTMSA  
01165      PERFORM GENERATE-DISCLAIMER-MESSAGE.                         ELTMSA  
01166                                                                   ELTMSA  
01167                                                                   ELTMSA  
01168 ************************************************************      ELTMSA  
01169 *                                                          *      ELTMSA  
01170 *        GENERATE SUPPLEMENTAL NOT APPLICABLE              *      ELTMSA  
01171 *                                                          *      ELTMSA  
01172 ************************************************************      ELTMSA  
01173  GENERATE-SUPPLEMENTAL-NOT-APPL.                                  ELTMSA  
01174      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMSA  
01175      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMSA  
01176      MOVE WS-NOT-APPLY-TO-LOB-MM TO COF-DTL-LINE                  ELTMSA  
01177          (COF-NBR-DTL-LINES).                                     ELTMSA  
01178      PERFORM EJECT-NEW-PAGE.                                      ELTMSA  
01179      EJECT                                                        ELTMSA  
01180                                                                   ELTMSA  
01181                                                                   ELTMSA  
01182 ************************************************************      ELTMSA  
01183 *                                                          *      ELTMSA  
01184 *        TRANSLATE MM IND                                  *      ELTMSA  
01185 *                                                          *      ELTMSA  
01186 ************************************************************      ELTMSA  
01187  TRANSLATE-MM-IND.                                                ELTMSA  
01188      INITIALIZE TCAR-FROM-AREA.                                   ELTMSA  
01189      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMSA  
01190      MOVE WS-BCBSMM-IND-LINE TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELTMSA  
01191      ADD  +1 TO TCAR-FROM-SUB.                                    ELTMSA  
01192      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMSA  
01193      SET PERIOD-NEEDED TO TRUE.                                   ELTMSA  
01194      MOVE GSS-MS-MM-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTMSA  
01195      MOVE   'MS-MM-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELTMSA  
01196      PERFORM TRANSLATE-AND-OUTPUT-CODE-VALU.                      ELTMSA  
01197      EJECT                                                        ELTMSA  
01198                                                                   ELTMSA  
01199                                                                   ELTMSA  
01200 ************************************************************      ELTMSA  
01201 *                                                          *      ELTMSA  
01202 *        GENERATE MM BENE REDUCT TEXT                      *      ELTMSA  
01203 *                                                          *      ELTMSA  
01204 ************************************************************      ELTMSA  
01205  GENERATE-MM-BENE-REDUCT-TEXT.                                    ELTMSA  
01206      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMSA  
01207      MOVE WS-BENE-REDUCT-LINE TO COF-DTL-LINE                     ELTMSA  
01208          (COF-NBR-DTL-LINES).                                     ELTMSA  
01209      PERFORM LINK-TO-OUTPUT.                                      ELTMSA  
01210      INITIALIZE ADDITIONAL-TEXT-SWITCH.                           ELTMSA  
01211      INITIALIZE WS-PERIOD-SWITCH.                                 ELTMSA  
01212      INITIALIZE TCAR-FROM-AREA.                                   ELTMSA  
01213      IF GSS-MS-MM-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMSA  
01214          SPACES                                                   ELTMSA  
01215                 AND ZEROES AND LOW-VALUES                         ELTMSA  
01216          PERFORM TRANSLATE-MM-DEDUCT-IND.                         ELTMSA  
01217      IF GSS-MS-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMSA  
01218          SPACES                                                   ELTMSA  
01219                 AND ZEROES AND LOW-VALUES                         ELTMSA  
01220          PERFORM TRANSLATE-MM-OPEX-APPLIC.                        ELTMSA  
01221      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMSA  
01222      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMSA  
01223      PERFORM LINK-TO-OUTPUT.                                      ELTMSA  
01224      IF GSS-MS-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTMSA  
01225          SPACES                                                   ELTMSA  
01226                 AND ZEROES AND LOW-VALUES                         ELTMSA  
01227          PERFORM TRANSLATE-MM-OPEX-OVERRIDE.                      ELTMSA  
01228      EJECT                                                        ELTMSA  
01229                                                                   ELTMSA  
01230                                                                   ELTMSA  
01231 ************************************************************      ELTMSA  
01232 *                                                          *      ELTMSA  
01233 *        TRANSLATE MM DEDUCT IND                           *      ELTMSA  
01234 *                                                          *      ELTMSA  
01235 ************************************************************      ELTMSA  
01236  TRANSLATE-MM-DEDUCT-IND.                                         ELTMSA  
01237      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMSA  
01238      MOVE GSS-MS-MM-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTMSA  
01239          CMF-CODE-VALUE.                                          ELTMSA  
01240      MOVE 'MS-MM-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMSA  
01241      PERFORM TRANSLATE-AND-OUTPUT-CODE-VALU.                      ELTMSA  
01242      EJECT                                                        ELTMSA  
01243                                                                   ELTMSA  
01244                                                                   ELTMSA  
01245 ************************************************************      ELTMSA  
01246 *                                                          *      ELTMSA  
01247 *        TRANSLATE MM OPEX APPLIC                          *      ELTMSA  
01248 *                                                          *      ELTMSA  
01249 ************************************************************      ELTMSA  
01250  TRANSLATE-MM-OPEX-APPLIC.                                        ELTMSA  
01251      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMSA  
01252      MOVE GSS-MS-MM-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTMSA  
01253          CMF-CODE-VALUE.                                          ELTMSA  
01254      MOVE 'MS-MM-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMSA  
01255      PERFORM TRANSLATE-AND-OUTPUT-CODE-VALU.                      ELTMSA  
01256      EJECT                                                        ELTMSA  
01257                                                                   ELTMSA  
01258                                                                   ELTMSA  
01259 ************************************************************      ELTMSA  
01260 *                                                          *      ELTMSA  
01261 *        TRANSLATE MM OPEX OVERRIDE                        *      ELTMSA  
01262 *                                                          *      ELTMSA  
01263 ************************************************************      ELTMSA  
01264  TRANSLATE-MM-OPEX-OVERRIDE.                                      ELTMSA  
01265      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMSA  
01266      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMSA  
01267      SET PERIOD-NEEDED TO TRUE.                                   ELTMSA  
01268      MOVE GSS-MS-MM-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTMSA  
01269          CMF-CODE-VALUE.                                          ELTMSA  
01270      MOVE 'MS-MM-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTMSA  
01271      PERFORM TRANSLATE-AND-OUTPUT-CODE-VALU.                      ELTMSA  
01272      EJECT                                                        ELTMSA  
01273                                                                   ELTMSA  
01274                                                                   ELTMSA  
01275 ************************************************************      ELTMSA  
01276 *                                                          *      ELTMSA  
01277 *        GENERATE SPILL OVER TEXT                          *      ELTMSA  
01278 *                                                          *      ELTMSA  
01279 ************************************************************      ELTMSA  
01280  GENERATE-SPILL-OVER-TEXT.                                        ELTMSA  
01281      INITIALIZE TCAR-FROM-AREA.                                   ELTMSA  
01282      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMSA  
01283      MOVE WS-SPILL-OVER-LINE TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELTMSA  
01284      ADD  +1 TO TCAR-FROM-SUB.                                    ELTMSA  
01285      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMSA  
01286      SET PERIOD-NEEDED TO TRUE.                                   ELTMSA  
01287      MOVE GSS-MS-SPILL-OVER-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMSA  
01288      MOVE 'MS-SPILL-OVER-IND' TO CMF-ELEMENT-SYSTEM-NAME.         ELTMSA  
01289      PERFORM TRANSLATE-AND-OUTPUT-CODE-VALU.                      ELTMSA  
01290      EJECT                                                        ELTMSA  
01291                                                                   ELTMSA  
01292                                                                   ELTMSA  
01293 ************************************************************      ELTMSA  
01294 *                                                          *      ELTMSA  
01295 *        GENERATE OTHER TABULAR INFO                       *      ELTMSA  
01296 *                                                          *      ELTMSA  
01297 ************************************************************      ELTMSA  
01298  GENERATE-OTHER-TABULAR-INFO.                                     ELTMSA  
01299      PERFORM GENERATE-RELATED-SERVICES-SENT.                      ELTMSA  
01300      PERFORM GENERATE-SPECIAL-PROCEDURES-SE.                      ELTMSA  
01301      PERFORM GENERATE-SPECIAL-PROVIDERS-SEN.                      ELTMSA  
01302                                                                   ELTMSA  
01303                                                                   ELTMSA  
01304 ************************************************************      ELTMSA  
01305 *                                                          *      ELTMSA  
01306 *        GENERATE RELATED SERVICES SENTENCE                *      ELTMSA  
01307 *                                                          *      ELTMSA  
01308 ************************************************************      ELTMSA  
01309  GENERATE-RELATED-SERVICES-SENT.                                  ELTMSA  
01310      SET GCG-INDEX TO +1.                                         ELTMSA  
01311      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTMSA  
01312         AT END                                                    ELTMSA  
01313              MOVE ZEROES TO WS-GMSB-PROV-SLOT-NO                  ELTMSA  
01314         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GMSB                     ELTMSA  
01315              MOVE GCG-TAB-ID (GCG-INDEX)                          ELTMSA  
01316                  TO WS-GMSB-PROV-ID                               ELTMSA  
01317              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTMSA  
01318                  TO WS-GMSB-PROV-SLOT-NO                          ELTMSA  
01319         END-SEARCH.                                               ELTMSA  
01320      IF WS-GMSB-PROV-ID EQUAL WS-GMSB                             ELTMSA  
01321                 AND WS-GMSB-PROV-SLOT-NO NOT EQUAL                ELTMSA  
01322          ZEROES                                                   ELTMSA  
01323          PERFORM DISPLAY-RELATED-SERVICES-SENTE.                  ELTMSA  
01324      EJECT                                                        ELTMSA  
01325                                                                   ELTMSA  
01326                                                                   ELTMSA  
01327 ************************************************************      ELTMSA  
01328 *                                                          *      ELTMSA  
01329 *        DISPLAY RELATED SERVICES SENTENCE                 *      ELTMSA  
01330 *                                                          *      ELTMSA  
01331 ************************************************************      ELTMSA  
01332  DISPLAY-RELATED-SERVICES-SENTE.                                  ELTMSA  
01333      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMSA  
01334      MOVE WS-SPEC-SERV-MSG  TO COF-DTL-LINE                       ELTMSA  
01335          (COF-NBR-DTL-LINES).                                     ELTMSA  
01336      PERFORM LINK-TO-OUTPUT.                                      ELTMSA  
01337      PERFORM GENERATE-RELATED-SERVICES-TEXT.                      ELTMSA  
01338      EJECT                                                        ELTMSA  
01339                                                                   ELTMSA  
01340                                                                   ELTMSA  
01341 ************************************************************      ELTMSA  
01342 *                                                          *      ELTMSA  
01343 *        GENERATE SPECIAL PROCEDURES SENTENCE              *      ELTMSA  
01344 *                                                          *      ELTMSA  
01345 ************************************************************      ELTMSA  
01346  GENERATE-SPECIAL-PROCEDURES-SE.                                  ELTMSA  
01347      SET GCG-INDEX TO +1.                                         ELTMSA  
01348      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTMSA  
01349         AT END                                                    ELTMSA  
01350              MOVE ZEROES TO WS-GMSR-PROV-SLOT-NO                  ELTMSA  
01351         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GMSR                     ELTMSA  
01352              MOVE GCG-TAB-ID (GCG-INDEX)                          ELTMSA  
01353                  TO WS-GMSR-PROV-ID                               ELTMSA  
01354              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTMSA  
01355                  TO WS-GMSR-PROV-SLOT-NO                          ELTMSA  
01356         END-SEARCH.                                               ELTMSA  
01357      IF WS-GMSR-PROV-ID EQUAL WS-GMSR                             ELTMSA  
01358                 AND WS-GMSR-PROV-SLOT-NO NOT EQUAL                ELTMSA  
01359          ZEROES                                                   ELTMSA  
01360          PERFORM DISPLAY-SPECIAL-PROCEDURES-SEN.                  ELTMSA  
01361                                                                   ELTMSA  
01362                                                                   ELTMSA  
01363 ************************************************************      ELTMSA  
01364 *                                                          *      ELTMSA  
01365 *        DISPLAY SPECIAL PROCEDURES SENTENCE               *      ELTMSA  
01366 *                                                          *      ELTMSA  
01367 ************************************************************      ELTMSA  
01368  DISPLAY-SPECIAL-PROCEDURES-SEN.                                  ELTMSA  
01369      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMSA  
01370      MOVE WS-SPEC-PROC-MSG   TO COF-DTL-LINE                      ELTMSA  
01371          (COF-NBR-DTL-LINES).                                     ELTMSA  
01372      PERFORM LINK-TO-OUTPUT.                                      ELTMSA  
01373      PERFORM GENERATE-SPECIAL-PROCEDURES-TE.                      ELTMSA  
01374      EJECT                                                        ELTMSA  
01375                                                                   ELTMSA  
01376                                                                   ELTMSA  
01377 ************************************************************      ELTMSA  
01378 *                                                          *      ELTMSA  
01379 *        GENERATE SPECIAL PROVIDERS SENTENCE               *      ELTMSA  
01380 *                                                          *      ELTMSA  
01381 ************************************************************      ELTMSA  
01382  GENERATE-SPECIAL-PROVIDERS-SEN.                                  ELTMSA  
01383      SET GCG-INDEX TO +1.                                         ELTMSA  
01384      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTMSA  
01385         AT END                                                    ELTMSA  
01386              MOVE ZEROES TO WS-GMSC-PROV-SLOT-NO                  ELTMSA  
01387         WHEN GCG-TAB-ID (GCG-INDEX) = WS-GMSC                     ELTMSA  
01388              MOVE GCG-TAB-ID (GCG-INDEX)                          ELTMSA  
01389                  TO WS-GMSC-PROV-ID                               ELTMSA  
01390              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELTMSA  
01391                  TO WS-GMSC-PROV-SLOT-NO                          ELTMSA  
01392         END-SEARCH.                                               ELTMSA  
01393      IF WS-GMSC-PROV-ID EQUAL WS-GMSC                             ELTMSA  
01394                 AND WS-GMSC-PROV-SLOT-NO NOT EQUAL                ELTMSA  
01395          ZEROES                                                   ELTMSA  
01396          PERFORM DISPLAY-SPECIAL-PROVIDERS-SENT.                  ELTMSA  
01397                                                                   ELTMSA  
01398                                                                   ELTMSA  
01399 ************************************************************      ELTMSA  
01400 *                                                          *      ELTMSA  
01401 *        DISPLAY SPECIAL PROVIDERS SENTENCE                *      ELTMSA  
01402 *                                                          *      ELTMSA  
01403 ************************************************************      ELTMSA  
01404  DISPLAY-SPECIAL-PROVIDERS-SENT.                                  ELTMSA  
01405      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMSA  
01406      MOVE WS-SPEC-PROV-MSG  TO COF-DTL-LINE                       ELTMSA  
01407          (COF-NBR-DTL-LINES).                                     ELTMSA  
01408      PERFORM LINK-TO-OUTPUT.                                      ELTMSA  
01409      PERFORM GENERATE-SPECIAL-PROVIDERS-TEX.                      ELTMSA  
01410      EJECT                                                        ELTMSA  
01411                                                                   ELTMSA  
01412                                                                   ELTMSA  
01413 ************************************************************      ELTMSA  
01414 *                                                          *      ELTMSA  
01415 *        SEARCH THE GSS ENTRY                              *      ELTMSA  
01416 *                                                          *      ELTMSA  
01417 ************************************************************      ELTMSA  
01418  SEARCH-THE-GSS-ENTRY.                                            ELTMSA  
01419      SET GSS-INDEX TO 1.                                          ELTMSA  
01420      SEARCH GSS-ENTRY                                             ELTMSA  
01421          AT END                                                   ELTMSA  
01422               SET TABULAR-IS-UNDEFINED TO TRUE                    ELTMSA  
01423          WHEN GSS-MS-PROG-CODE-CHR (GSS-INDEX)                    ELTMSA  
01424               CONTINUE                                            ELTMSA  
01425         END-SEARCH.                                               ELTMSA  
01426      IF TABULAR-IS-UNDEFINED                                      ELTMSA  
01427          PERFORM SIGNAL-UNDEFINED-TABULAR-ERROR.                  ELTMSA  
01428                                                                   ELTMSA  
01429                                                                   ELTMSA  
01430 ************************************************************      ELTMSA  
01431 *                                                          *      ELTMSA  
01432 *        CALL CODES MANUAL INTERFACE                       *      ELTMSA  
01433 *                                                          *      ELTMSA  
01434 ************************************************************      ELTMSA  
01435  CALL-CODES-MANUAL-INTERFACE.                                     ELTMSA  
01436      MOVE WS-GCCP TO CMF-RECORD-PREFIX.                           ELTMSA  
01437      EXEC CICS LINK                                               ELTMSA  
01438                PROGRAM ('ELUCMIF')                                ELTMSA  
01439                COMMAREA (DFHCOMMAREA)                             ELTMSA  
01440        END-EXEC.                                                  ELTMSA  
01441      EJECT                                                        ELTMSA  
01442                                                                   ELTMSA  
01443                                                                   ELTMSA  
01444 ************************************************************      ELTMSA  
01445 *                                                          *      ELTMSA  
01446 *        GET GCCP TABULAR                                  *      ELTMSA  
01447 *                                                          *      ELTMSA  
01448 ************************************************************      ELTMSA  
01449  GET-GCCP-TABULAR.                                                ELTMSA  
01450      PERFORM ESTABLISH-ADDRESSABILITY-OF-GC.                      ELTMSA  
01451      SET IOP-RD              TO TRUE.                             ELTMSA  
01452      SET IOP-STG-MODE-MOVE   TO TRUE.                             ELTMSA  
01453      SET IOP-FCQ-NONE        TO TRUE.                             ELTMSA  
01454      SET IOP-KVQ-EQ          TO TRUE.                             ELTMSA  
01455      MOVE KWA-GCTABULR-KEY   TO IOP-FILE-KEY.                     ELTMSA  
01456      PERFORM CALL-INPUT-OUTPUT-MODULE.                            ELTMSA  
01457      IF IOP-RC-OK                                                 ELTMSA  
01458          PERFORM ESTABLISH-ADDRESSY-OF-GCCP-TAB                   ELTMSA  
01459      ELSE IF IOP-RC-NOTFND                                        ELTMSA  
01460          PERFORM SIGNAL-NOT-FOUND-GCTAB-ERROR                     ELTMSA  
01461      ELSE                                                         ELTMSA  
01462          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTMSA  
01463      EJECT                                                        ELTMSA  
01464                                                                   ELTMSA  
01465                                                                   ELTMSA  
01466 ************************************************************      ELTMSA  
01467 *                                                          *      ELTMSA  
01468 *        ESTABLISH ADDRESSABILITY OF GCTABULAR IO PARAMETER*      ELTMSA  
01469 *                                                          *      ELTMSA  
01470 ************************************************************      ELTMSA  
01471  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTMSA  
01472      SET CIA-GCTABULR-DDN TO TRUE.                                ELTMSA  
01473      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMSA  
01474                            ADDRESS OF                             ELTMSA  
01475          IOP-INPUT-OUTPUT-PARAMETERS.                             ELTMSA  
01476      EJECT                                                        ELTMSA  
01477                                                                   ELTMSA  
01478                                                                   ELTMSA  
01479 ************************************************************      ELTMSA  
01480 *                                                          *      ELTMSA  
01481 *        CALL INPUT OUTPUT MODULE                          *      ELTMSA  
01482 *                                                          *      ELTMSA  
01483 ************************************************************      ELTMSA  
01484  CALL-INPUT-OUTPUT-MODULE.                                        ELTMSA  
01485      EXEC CICS LINK                                               ELTMSA  
01486                PROGRAM ('ELUIOPGM')                               ELTMSA  
01487                COMMAREA (DFHCOMMAREA)                             ELTMSA  
01488        END-EXEC.                                                  ELTMSA  
01489      EJECT                                                        ELTMSA  
01490                                                                   ELTMSA  
01491                                                                   ELTMSA  
01492 ************************************************************      ELTMSA  
01493 *                                                          *      ELTMSA  
01494 *        ESTABLISH ADDRESSY OF GCCP TABULAR                *      ELTMSA  
01495 *                                                          *      ELTMSA  
01496 ************************************************************      ELTMSA  
01497  ESTABLISH-ADDRESSY-OF-GCCP-TAB.                                  ELTMSA  
01498      SET ADDRESS OF GCCP-TABULAR-REC-AREA TO                      ELTMSA  
01499          IOP-REC-PTR.                                             ELTMSA  
01500      SET IOP-REC-PTR TO NULL.                                     ELTMSA  
01501      EJECT                                                        ELTMSA  
01502                                                                   ELTMSA  
01503                                                                   ELTMSA  
01504 ************************************************************      ELTMSA  
01505 *                                                          *      ELTMSA  
01506 *        SIGNAL NOT FOUND GCTAB ERROR                      *      ELTMSA  
01507 *                                                          *      ELTMSA  
01508 ************************************************************      ELTMSA  
01509  SIGNAL-NOT-FOUND-GCTAB-ERROR.                                    ELTMSA  
01510      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTMSA  
01511      PERFORM SIGNAL-ABEND.                                        ELTMSA  
01512      EJECT                                                        ELTMSA  
01513                                                                   ELTMSA  
01514                                                                   ELTMSA  
01515 ************************************************************      ELTMSA  
01516 *                                                          *      ELTMSA  
01517 *        SIGNAL CRITICAL IO ERROR                          *      ELTMSA  
01518 *                                                          *      ELTMSA  
01519 ************************************************************      ELTMSA  
01520  SIGNAL-CRITICAL-IO-ERROR.                                        ELTMSA  
01521      SET CIA-AB-CRITIO TO TRUE.                                   ELTMSA  
01522      PERFORM SIGNAL-ABEND.                                        ELTMSA  
01523                                                                   ELTMSA  
01524                                                                   ELTMSA  
01525 ************************************************************      ELTMSA  
01526 *                                                          *      ELTMSA  
01527 *        GENERATE RELATED SERVICES TEXT                    *      ELTMSA  
01528 *                                                          *      ELTMSA  
01529 ************************************************************      ELTMSA  
01530  GENERATE-RELATED-SERVICES-TEXT.                                  ELTMSA  
01531      MOVE 'MEDICAL SERVICES ADVISORY' TO SRP-CCP-NAME.            ELTMSA  
01532      MOVE  WS-GMSB-PROV-ID            TO SRP-TABULAR-ID.          ELTMSA  
01533      MOVE  WS-GMSB-PROV-SLOT-NO       TO SRP-TABULAR-SLOT-NO.     ELTMSA  
01534      PERFORM CALL-RELATED-SERVICES-GENERATO.                      ELTMSA  
01535                                                                   ELTMSA  
01536                                                                   ELTMSA  
01537 ************************************************************      ELTMSA  
01538 *                                                          *      ELTMSA  
01539 *        CALL RELATED SERVICES GENERATOR                   *      ELTMSA  
01540 *                                                          *      ELTMSA  
01541 ************************************************************      ELTMSA  
01542  CALL-RELATED-SERVICES-GENERATO.                                  ELTMSA  
01543      EXEC CICS LINK                                               ELTMSA  
01544                PROGRAM ('ELGGXXB')                                ELTMSA  
01545                COMMAREA (DFHCOMMAREA)                             ELTMSA  
01546         END-EXEC.                                                 ELTMSA  
01547      EJECT                                                        ELTMSA  
01548                                                                   ELTMSA  
01549                                                                   ELTMSA  
01550 ************************************************************      ELTMSA  
01551 *                                                          *      ELTMSA  
01552 *        GENERATE SPECIAL PROCEDURES TEXT                  *      ELTMSA  
01553 *                                                          *      ELTMSA  
01554 ************************************************************      ELTMSA  
01555  GENERATE-SPECIAL-PROCEDURES-TE.                                  ELTMSA  
01556      MOVE 'MEDICAL SERVICES ADVISORY' TO SRP-CCP-NAME.            ELTMSA  
01557      MOVE  WS-GMSR-PROV-ID            TO                          ELTMSA  
01558          SRP-TABULAR-ID.                                          ELTMSA  
01559      MOVE  WS-GMSR-PROV-SLOT-NO       TO SRP-TABULAR-SLOT-NO.     ELTMSA  
01560      PERFORM CALL-SPECIAL-PROCEDURES-GENERA.                      ELTMSA  
01561                                                                   ELTMSA  
01562                                                                   ELTMSA  
01563 ************************************************************      ELTMSA  
01564 *                                                          *      ELTMSA  
01565 *        CALL SPECIAL PROCEDURES GENERATOR                 *      ELTMSA  
01566 *                                                          *      ELTMSA  
01567 ************************************************************      ELTMSA  
01568  CALL-SPECIAL-PROCEDURES-GENERA.                                  ELTMSA  
01569      EXEC CICS LINK                                               ELTMSA  
01570                PROGRAM ('ELGGXXR')                                ELTMSA  
01571                COMMAREA (DFHCOMMAREA)                             ELTMSA  
01572         END-EXEC.                                                 ELTMSA  
01573      EJECT                                                        ELTMSA  
01574                                                                   ELTMSA  
01575                                                                   ELTMSA  
01576 ************************************************************      ELTMSA  
01577 *                                                          *      ELTMSA  
01578 *        GENERATE SPECIAL PROVIDERS TEXT                   *      ELTMSA  
01579 *                                                          *      ELTMSA  
01580 ************************************************************      ELTMSA  
01581  GENERATE-SPECIAL-PROVIDERS-TEX.                                  ELTMSA  
01582      MOVE 'MEDICAL SERVICES ADVISORY' TO SRP-CCP-NAME.            ELTMSA  
01583      MOVE  WS-GMSC-PROV-ID            TO                          ELTMSA  
01584          SRP-TABULAR-ID.                                          ELTMSA  
01585      MOVE  WS-GMSC-PROV-SLOT-NO       TO SRP-TABULAR-SLOT-NO.     ELTMSA  
01586      PERFORM CALL-SPECIAL-PROVIDERS-GENERAT.                      ELTMSA  
01587                                                                   ELTMSA  
01588                                                                   ELTMSA  
01589 ************************************************************      ELTMSA  
01590 *                                                          *      ELTMSA  
01591 *        CALL SPECIAL PROVIDERS GENERATOR                  *      ELTMSA  
01592 *                                                          *      ELTMSA  
01593 ************************************************************      ELTMSA  
01594  CALL-SPECIAL-PROVIDERS-GENERAT.                                  ELTMSA  
01595      EXEC CICS LINK                                               ELTMSA  
01596                PROGRAM ('ELGGXXC')                                ELTMSA  
01597                COMMAREA (DFHCOMMAREA)                             ELTMSA  
01598         END-EXEC.                                                 ELTMSA  
01599                                                                   ELTMSA  
01600                                                                   ELTMSA  
01601 ************************************************************      ELTMSA  
01602 *                                                          *      ELTMSA  
01603 *        GENERATE COINSURANCE                              *      ELTMSA  
01604 *                                                          *      ELTMSA  
01605 ************************************************************      ELTMSA  
01606  GENERATE-COINSURANCE.                                            ELTMSA  
01607      EXEC CICS LINK                                               ELTMSA  
01608                PROGRAM ('ELGACLCC')                               ELTMSA  
01609                COMMAREA (DFHCOMMAREA)                             ELTMSA  
01610         END-EXEC.                                                 ELTMSA  
01611                                                                   ELTMSA  
01612 ************************************************************      ELTMSA  
01613 *                                                          *      ELTMSA  
01614 *        GENERATE COPAY                                    *      ELTMSA  
01615 *                                                          *      ELTMSA  
01616 ************************************************************      ELTMSA  
01617  GENERATE-COPAY.                                                  ELTMSA  
01618      EXEC CICS LINK                                               ELTMSA  
01619                PROGRAM ('ELGACPCC')                               ELTMSA  
01620                COMMAREA (DFHCOMMAREA)                             ELTMSA  
01621         END-EXEC.                                                 ELTMSA  
01622                                                                   ELTMSA  
01623                                                                   ELTMSA  
01624 ************************************************************      ELTMSA  
01625 *                                                          *      ELTMSA  
01626 *        GENERATE DEDUCTIBLE                               *      ELTMSA  
01627 *                                                          *      ELTMSA  
01628 ************************************************************      ELTMSA  
01629  GENERATE-DEDUCTIBLE.                                             ELTMSA  
01630      EXEC CICS LINK                                               ELTMSA  
01631                PROGRAM ('ELGADLCC')                               ELTMSA  
01632                COMMAREA (DFHCOMMAREA)                             ELTMSA  
01633         END-EXEC.                                                 ELTMSA  
01634                                                                   ELTMSA  
01635                                                                   ELTMSA  
01636 ************************************************************      ELTMSA  
01637 *                                                          *      ELTMSA  
01638 *        GENERATE MAXIMUM                                  *      ELTMSA  
01639 *                                                          *      ELTMSA  
01640 ************************************************************      ELTMSA  
01641  GENERATE-MAXIMUM.                                                ELTMSA  
01642      EXEC CICS LINK                                               ELTMSA  
01643                PROGRAM ('ELGABMCC')                               ELTMSA  
01644                COMMAREA (DFHCOMMAREA)                             ELTMSA  
01645         END-EXEC.                                                 ELTMSA  
01646                                                                   ELTMSA  
01647                                                                   ELTMSA  
01648 ************************************************************      ELTMSA  
01649 *                                                          *      ELTMSA  
01650 *        CALL CBRI INTERFACE                               *      ELTMSA  
01651 *                                                          *      ELTMSA  
01652 ************************************************************      ELTMSA  
01653  CALL-CBRI-INTERFACE.                                             ELTMSA  
01654      CALL 'ELGCBRI' USING DFHEIBLK                                ELTMSA  
01655                           DFHCOMMAREA.                            ELTMSA  
01656                                                                   ELTMSA  
01657                                                                   ELTMSA  
01658 ************************************************************      ELTMSA  
01659 *                                                          *      ELTMSA  
01660 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTMSA  
01661 *                                                          *      ELTMSA  
01662 ************************************************************      ELTMSA  
01663  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTMSA  
01664      PERFORM INITIALIZE-CMOUT.                                    ELTMSA  
01665      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTMSA  
01666      EJECT                                                        ELTMSA  
01667                                                                   ELTMSA  
01668                                                                   ELTMSA  
01669 ************************************************************      ELTMSA  
01670 *                                                          *      ELTMSA  
01671 *        PREPARE TEXT FOR OUTPUT                           *      ELTMSA  
01672 *                                                          *      ELTMSA  
01673 ************************************************************      ELTMSA  
01674  PREPARE-TEXT-FOR-OUTPUT.                                         ELTMSA  
01675      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTMSA  
01676          UNTIL CMF-DESCR-IDX                                      ELTMSA  
01677                                    GREATER THAN                   ELTMSA  
01678              CMF-NBR-DESCR-LINES.                                 ELTMSA  
01679      EJECT                                                        ELTMSA  
01680                                                                   ELTMSA  
01681                                                                   ELTMSA  
01682 ************************************************************      ELTMSA  
01683 *                                                          *      ELTMSA  
01684 *        INITIALIZE CMOUT                                  *      ELTMSA  
01685 *                                                          *      ELTMSA  
01686 ************************************************************      ELTMSA  
01687  INITIALIZE-CMOUT.                                                ELTMSA  
01688      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTMSA  
01689      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMSA  
01690          ADDRESS OF CMF-DESCR.                                    ELTMSA  
01691      SET CMF-DESCR-IDX TO 1.                                      ELTMSA  
01692      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTMSA  
01693                                                                   ELTMSA  
01694                                                                   ELTMSA  
01695 ************************************************************      ELTMSA  
01696 *                                                          *      ELTMSA  
01697 *        MOVE CMF TEXT TO OUTPUT                           *      ELTMSA  
01698 *                                                          *      ELTMSA  
01699 ************************************************************      ELTMSA  
01700  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTMSA  
01701      PERFORM MOVE-A-LINE.                                         ELTMSA  
01702      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTMSA  
01703          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTMSA  
01704      IF TCAR-FROM-SUB GREATER THAN 20                             ELTMSA  
01705               OR CMF-DESCR-IDX GREATER THAN                       ELTMSA  
01706          CMF-NBR-DESCR-LINES                                      ELTMSA  
01707          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTMSA  
01708                                                                   ELTMSA  
01709                                                                   ELTMSA  
01710 ************************************************************      ELTMSA  
01711 *                                                          *      ELTMSA  
01712 *        FINISH CODES MANUAL TEXT                          *      ELTMSA  
01713 *                                                          *      ELTMSA  
01714 ************************************************************      ELTMSA  
01715  FINISH-CODES-MANUAL-TEXT.                                        ELTMSA  
01716      SET DONE-PROCESSING TO TRUE.                                 ELTMSA  
01717      IF PERIOD-NEEDED                                             ELTMSA  
01718          PERFORM GET-AND-MOVE-PERIOD.                             ELTMSA  
01719      EJECT                                                        ELTMSA  
01720                                                                   ELTMSA  
01721                                                                   ELTMSA  
01722 ************************************************************      ELTMSA  
01723 *                                                          *      ELTMSA  
01724 *        GET AND MOVE PERIOD                               *      ELTMSA  
01725 *                                                          *      ELTMSA  
01726 ************************************************************      ELTMSA  
01727  GET-AND-MOVE-PERIOD.                                             ELTMSA  
01728      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTMSA  
01729          (TCAR-FROM-SUB).                                         ELTMSA  
01730                                                                   ELTMSA  
01731                                                                   ELTMSA  
01732 ************************************************************      ELTMSA  
01733 *                                                          *      ELTMSA  
01734 *        SAVE LAST LINE                                    *      ELTMSA  
01735 *                                                          *      ELTMSA  
01736 ************************************************************      ELTMSA  
01737  SAVE-LAST-LINE.                                                  ELTMSA  
01738      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMSA  
01739      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTMSA  
01740         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTMSA  
01741      ADD 1 TO TCAR-FROM-SUB.                                      ELTMSA  
01742      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTMSA  
01743                                                                   ELTMSA  
01744                                                                   ELTMSA  
01745 ************************************************************      ELTMSA  
01746 *                                                          *      ELTMSA  
01747 *        OUTPUT LAST LINE                                  *      ELTMSA  
01748 *                                                          *      ELTMSA  
01749 ************************************************************      ELTMSA  
01750  OUTPUT-LAST-LINE.                                                ELTMSA  
01751      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTMSA  
01752          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTMSA  
01753      IF BLANK-LINE-NEEDED                                         ELTMSA  
01754          PERFORM CREATE-A-BLANK-LINE.                             ELTMSA  
01755                                                                   ELTMSA  
01756                                                                   ELTMSA  
01757 ************************************************************      ELTMSA  
01758 *                                                          *      ELTMSA  
01759 *        CREATE A BLANK LINE                               *      ELTMSA  
01760 *                                                          *      ELTMSA  
01761 ************************************************************      ELTMSA  
01762  CREATE-A-BLANK-LINE.                                             ELTMSA  
01763      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTMSA  
01764      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMSA  
01765                                                                   ELTMSA  
01766                                                                   ELTMSA  
01767 ************************************************************      ELTMSA  
01768 *                                                          *      ELTMSA  
01769 *        MOVE A LINE                                       *      ELTMSA  
01770 *                                                          *      ELTMSA  
01771 ************************************************************      ELTMSA  
01772  MOVE-A-LINE.                                                     ELTMSA  
01773      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTMSA  
01774          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTMSA  
01775      SET CMF-DESCR-IDX UP BY 1.                                   ELTMSA  
01776      ADD 1 TO TCAR-FROM-SUB.                                      ELTMSA  
01777      EJECT                                                        ELTMSA  
01778                                                                   ELTMSA  
01779                                                                   ELTMSA  
01780 ************************************************************      ELTMSA  
01781 *                                                          *      ELTMSA  
01782 *        REFORMAT AND WRITE TEXT                           *      ELTMSA  
01783 *                                                          *      ELTMSA  
01784 ************************************************************      ELTMSA  
01785  REFORMAT-AND-WRITE-TEXT.                                         ELTMSA  
01786      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTMSA  
01787      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTMSA  
01788      PERFORM UNSTRING-TEXT.                                       ELTMSA  
01789      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMSA  
01790      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMSA  
01791      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTMSA  
01792          UNTIL COF-NBR-DTL-LINES GREATER                          ELTMSA  
01793                                   TCAR-OUTPUT-FIELDS-USED -       ELTMSA  
01794              1.                                                   ELTMSA  
01795      PERFORM DISPOSE-OF-LAST-LINE.                                ELTMSA  
01796      PERFORM LINK-TO-OUTPUT.                                      ELTMSA  
01797                                                                   ELTMSA  
01798                                                                   ELTMSA  
01799 ************************************************************      ELTMSA  
01800 *                                                          *      ELTMSA  
01801 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTMSA  
01802 *                                                          *      ELTMSA  
01803 ************************************************************      ELTMSA  
01804  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTMSA  
01805      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTMSA  
01806           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTMSA  
01807      ADD +1 TO TCAR-FROM-SUB.                                     ELTMSA  
01808      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMSA  
01809      EJECT                                                        ELTMSA  
01810                                                                   ELTMSA  
01811                                                                   ELTMSA  
01812 ************************************************************      ELTMSA  
01813 *                                                          *      ELTMSA  
01814 *        UNSTRING TEXT                                     *      ELTMSA  
01815 *                                                          *      ELTMSA  
01816 ************************************************************      ELTMSA  
01817  UNSTRING-TEXT.                                                   ELTMSA  
01818      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTMSA  
01819      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTMSA  
01820      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTMSA  
01821      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTMSA  
01822      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTMSA  
01823      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTMSA  
01824      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTMSA  
01825      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTMSA  
01826      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTMSA  
01827      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTMSA  
01828      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTMSA  
01829      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTMSA  
01830      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTMSA  
01831      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTMSA  
01832      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTMSA  
01833      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTMSA  
01834      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTMSA  
01835      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTMSA  
01836      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTMSA  
01837      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTMSA  
01838      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTMSA  
01839      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTMSA  
01840                                                                   ELTMSA  
01841                                                                   ELTMSA  
01842 ************************************************************      ELTMSA  
01843 *                                                          *      ELTMSA  
01844 *        LINK TO OUTPUT                                    *      ELTMSA  
01845 *                                                          *      ELTMSA  
01846 ************************************************************      ELTMSA  
01847  LINK-TO-OUTPUT.                                                  ELTMSA  
01848      EXEC CICS LINK                                               ELTMSA  
01849          PROGRAM ('ELUOUTPT')                                     ELTMSA  
01850          COMMAREA (DFHCOMMAREA)                                   ELTMSA  
01851          END-EXEC.                                                ELTMSA  
01852      EJECT                                                        ELTMSA  
01853                                                                   ELTMSA  
01854                                                                   ELTMSA  
01855 ************************************************************      ELTMSA  
01856 *                                                          *      ELTMSA  
01857 *        DISPOSE OF LAST LINE                              *      ELTMSA  
01858 *                                                          *      ELTMSA  
01859 ************************************************************      ELTMSA  
01860  DISPOSE-OF-LAST-LINE.                                            ELTMSA  
01861      IF NOT ADDITIONAL-TEXT                                       ELTMSA  
01862          PERFORM INITIALIZE-CONTINUED-SW.                         ELTMSA  
01863      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTMSA  
01864          PERFORM SAVE-LAST-LINE                                   ELTMSA  
01865      ELSE                                                         ELTMSA  
01866          PERFORM OUTPUT-LAST-LINE.                                ELTMSA  
01867                                                                   ELTMSA  
01868                                                                   ELTMSA  
01869 ************************************************************      ELTMSA  
01870 *                                                          *      ELTMSA  
01871 *        INITIALIZE CONTINUED SW                           *      ELTMSA  
01872 *                                                          *      ELTMSA  
01873 ************************************************************      ELTMSA  
01874  INITIALIZE-CONTINUED-SW.                                         ELTMSA  
01875      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTMSA  
