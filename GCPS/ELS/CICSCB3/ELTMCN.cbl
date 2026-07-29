00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELTMCN  
00003  PROGRAM-ID.         ELTMCN.                                         LV001
00004                                                                   ELTMCN  
00005  AUTHOR.             ANNE KEFFER KING.                            ELTMCN  
00006                                                                   ELTMCN  
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTMCN  
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELTMCN  
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTMCN  
00010                      233 N. MICHIGAN AVE                          ELTMCN  
00011                      CHICAGO, ILLINOIS 60601                      ELTMCN  
00012                                                                   ELTMCN  
00013  DATE-WRITTEN.       25-SEP-1990.                                 ELTMCN  
00014                                                                   ELTMCN  
00015  DATE-COMPILED.                                                   ELTMCN  
00016                                                                   ELTMCN  
00017  SECURITY.           COPYRIGHT 1986,                              ELTMCN  
00018                      HEALTH CARE SERVICE CORPORATION              ELTMCN  
00019 ******************************************************************ELTMCN  
00020 *  RECORDS                                                       *ELTMCN  
00021 *  ACCESSED: ELCDCIA RECORD                                      *ELTMCN  
00022 *             GROUP SPECIFIC RECORD                              *ELTMCN  
00023 *             #GCCP TABULAR RECORD                               *ELTMCN  
00024 *                                                                *ELTMCN  
00025 *  PROCESSING                                                    *ELTMCN  
00026 *  FUNCTIONS: THIS MODULE PERFORMS THE FOLLOWING FUNCTIONS:      *ELTMCN  
00027 *                                                                *ELTMCN  
00028 *              1. INITIALIZES WORK DATA ELEMENTS.                *ELTMCN  
00029 *                                                                *ELTMCN  
00030 *              2. ACQUIRE NECESSARY RECORDS FOR PROCESSING.      *ELTMCN  
00031 *                                                                *ELTMCN  
00032 *              3. PROCESS MANAGED CARE NETWORK PROGRAM TABULAR   *ELTMCN  
00033 *                 RECORD FOR BLUE CROSS PRODUCING OUTPUT         *ELTMCN  
00034 *                 TEXT AS REQUIRED.                              *ELTMCN  
00035 *                                                                *ELTMCN  
00036 *              4. PROCESS PRE-ADMISSION REVIEW PROGRAM TABULAR   *ELTMCN  
00037 *                 RECORD FOR BLUE SHIELD PRODUCING OUTPUT        *ELTMCN  
00038 *                 TEXT AS REQUIRED.                              *ELTMCN  
00039 *                                                                *ELTMCN  
00040 *              5. PROCESS MANAGED CARE NETWORK PROGRAM TABULAR   *ELTMCN  
00041 *                 RECORD FOR MAJOR MEDICAL PRODUCING OUTPUT      *ELTMCN  
00042 *                 TEXT AS REQUIRED.                              *ELTMCN  
00043 ******************************************************************ELTMCN  
00044 *                U P D A T E  L O G                              *ELTMCN  
00045 *                                                                *ELTMCN  
00046 *  MOD      DATE      WHO        DESCRIPTION                      ELTMCN  
00047 *                                                                 ELTMCN  
00048 *  1.00               AKK        CREATED                          ELTMCN  
00049 *                                                                 ELTMCN  
00050 *  1.01    10/18/90   AKK        PER AUGGIE'S REQUEST- CHANGED    ELTMCN  
00051 *                                CODE SO IF A PAYMENT LEVEL IND   ELTMCN  
00052 *                                AND A CALC METHOD ALSO CODED     ELTMCN  
00053 *                                ONLY THE PAYMENT LEVEL IND       ELTMCN  
00054 *                                SHOULD BE DISPLAYED.             ELTMCN  
00055 *                                                                 ELTMCN  
00056 * 1.02     11/12/90   AKK        CHANGED TEST FOR GCG MCN IND     ELTMCN  
00057 *                                TO ZERO AND '08' DUE TO EXPANDED ELTMCN  
00058 *                                FIELDS IN THE GROUP SPECIFIC.    ELTMCN  
00059 *                                                                 ELTMCN  
00060 * 1.03     05/14/91   AKK        ADDED CODE TO PROCESS GMCR       ELTMCN  
00061 *                                RELATED PROCEDURES TABULAR.      ELTMCN  
00062 *                                ISSR #11836.                     ELTMCN  
00063 *                                                                 ELTMCN  
00064 * 1.04     06/04/91   AKK        ADDED TRANSLATION OF GROUP       ELTMCN  
00065 *                                PARTICPATION IND.                ELTMCN  
00066 *                                CHANGED LINK TO ELUCMIF AND      ELTMCN  
00067 *                                ELUIOPGM TO CALLS.               ELTMCN  
00068 *                                ISSR #11836.                     ELTMCN  
00069 *                                                                 ELTMCN  
00070 * 1.05     03/18/92   BAK        ADD CODE TO PROCESS #GMCS-       ELTMCN  
00071 *                                SPECIAL SERVICES TABULAR.        ELTMCN  
00072 *                                ALSO REMOVE PERFORMS FOR         ELTMCN  
00073 *                                GENERATE HEADING ROUTINE AND     ELTMCN  
00074 *                                REPLACE WITH HEADING MOVES.      ELTMCN  
00075 ***************************************************************** ELTMCN  
00076      SKIP3                                                        ELTMCN  
00077                                                                   ELTMCN  
00078  ENVIRONMENT DIVISION.                                            ELTMCN  
00079                                                                   ELTMCN  
00080  CONFIGURATION SECTION.                                           ELTMCN  
00081  SOURCE-COMPUTER.    IBM-3090.                                    ELTMCN  
00082  OBJECT-COMPUTER.    IBM-3090.                                    ELTMCN  
00083      EJECT                                                        ELTMCN  
00084                                                                   ELTMCN  
00085                                                                   ELTMCN  
00086  DATA DIVISION.                                                   ELTMCN  
00087 /                                                                 ELTMCN  
00088 *                                                                 ELTMCN  
00089  WORKING-STORAGE SECTION.                                         ELTMCN  
00090  01  WS-BEGIN                            PIC X(24) VALUE          ELTMCN  
00091                                 '** ELTMCN WS BEGINS **'.         ELTMCN  
00092  01  WS-MISC.                                                     ELTMCN  
00093      05  PC-GCCP                       PIC X(06)                  ELTMCN  
00094                                               VALUE '#GCCP'.      ELTMCN  
00095      05  PC-GROUP                      PIC X(06)                  ELTMCN  
00096                                               VALUE 'GROUP '.     ELTMCN  
00097      05  PC-GMCR                       PIC X(06)                  ELTMCN  
00098                                               VALUE '#GMCR'.      ELTMCN  
00099      05  WS-GMCR-PROC-ID               PIC X(06) VALUE SPACE.     ELTMCN  
00100      05  WS-GMCR-PROC-SLOT-NO          PIC S9(04) COMP-3          ELTMCN  
00101                                                  VALUE ZERO.      ELTMCN  
00102      05  PC-GMCS                       PIC X(06)                  ELTMCN  
00103                                               VALUE '#GMCS'.      ELTMCN  
00104      05  WS-GMCS-SPEC-ID               PIC X(06) VALUE SPACE.     ELTMCN  
00105      05  WS-GMCS-SPEC-SLOT-NO          PIC S9(04) COMP-3          ELTMCN  
00106                                                  VALUE ZERO.      ELTMCN  
00107      05  WS-CMF-SUB                    PIC S9(04) COMP.           ELTMCN  
00108      05  SCREEN-TYPE                   PIC X  VALUE SPACES.       ELTMCN  
00109          88  INSTITUTIONAL-SCREEN             VALUE 'I'.          ELTMCN  
00110          88  PROFESSIONAL-SCREEN              VALUE 'P'.          ELTMCN  
00111          88  SUPPLEMENTAL-SCREEN              VALUE 'S'.          ELTMCN  
00112 *                                                                 ELTMCN  
00113  01  WS-SWITCHES.                                                 ELTMCN  
00114      05  UNDEFINED-TABULAR-SW     PIC X      VALUE 'N'.           ELTMCN  
00115          88 TABULAR-IS-UNDEFINED             VALUE 'Y'.           ELTMCN  
00116      05  DEFINED-TABULAR-SW       PIC X      VALUE 'N'.           ELTMCN  
00117          88 TABULAR-IS-DEFINED               VALUE 'Y'.           ELTMCN  
00118 *                                                                 ELTMCN  
00119      05  ADDITIONAL-TEXT-SW       PIC X      VALUE SPACE.         ELTMCN  
00120          88 BLANK-LINE-NEEDED                VALUE 'B'.           ELTMCN  
00121          88 ADDITIONAL-TEXT                  VALUE 'Y'.           ELTMCN  
00122 *                                                                 ELTMCN  
00123      05  CONTINUED-PROCESSING-SW  PIC X      VALUE SPACE.         ELTMCN  
00124          88 PROCESSING-CMF-TEXT              VALUE 'P'.           ELTMCN  
00125          88 DONE-PROCESSING                  VALUE 'D'.           ELTMCN  
00126 *                                                                 ELTMCN  
00127      05  WS-PERIOD-SW             PIC X      VALUE 'N'.           ELTMCN  
00128          88 PERIOD-NEEDED                    VALUE 'Y'.           ELTMCN  
00129 *                                                                 ELTMCN  
00130      05  WS-PAYMENT-LEVEL-SW      PIC X      VALUE SPACE.         ELTMCN  
00131          88 PAYMENT-LVL-TRANSLATED           VALUE 'Y'.           ELTMCN  
00132          88 PAYMENT-LVL-NOT-TRANSLATED       VALUE 'N'.           ELTMCN  
00133 *                                                                 ELTMCN  
00134 ******************************************************************ELTMCN  
00135 *SCREEN BODY LINES                                                ELTMCN  
00136 ******************************************************************ELTMCN  
00137  01  WS-HDR-LN2.                                                  ELTMCN  
00138      05  FILLER            PIC X(18)         VALUE SPACES.        ELTMCN  
00139      05  FILLER            PIC X(29)         VALUE                ELTMCN  
00140          'MANAGED CARE NETWORK PROGRAM '.                         ELTMCN  
00141      05  HDR-TITLE         PIC X(13)         VALUE SPACES.        ELTMCN  
00142      05  FILLER            PIC X(18)         VALUE SPACES.        ELTMCN  
00143                                                                   ELTMCN  
00144  01  WS-PARTICIPATION-IND.                                        ELTMCN  
00145      05  FILLER             PIC X(79)        VALUE                ELTMCN  
00146          'THE MANAGED CARE NETWORK PROGRAM APPLIES TO '.          ELTMCN  
00147 *                                                                 ELTMCN  
00148                                                                   ELTMCN  
00149  01  WS-APPROVAL-SOURCE.                                          ELTMCN  
00150      05  FILLER             PIC X(79)        VALUE                ELTMCN  
00151          'THE MANAGED CARE NETWORK PROGRAM REQUIRES THE APPROVAL OELTMCN  
00152 -        'F '.                                                    ELTMCN  
00153 *                                                                 ELTMCN  
00154  01  WS-INDICATOR.                                                ELTMCN  
00155      05  FILLER             PIC X(79)        VALUE                ELTMCN  
00156          'THE MANAGED CARE NETWORK PROGRAM '.                     ELTMCN  
00157 *                                                                 ELTMCN  
00158  01  WS-PAYMENT-LEVEL.                                            ELTMCN  
00159      05  FILLER                  PIC X(79)   VALUE                ELTMCN  
00160      'MANAGED CARE NETWORK PAYMENT LEVEL RULES ARE AS FOLLOWS:  '.ELTMCN  
00161 *                                                                 ELTMCN  
00162  01  WS-CALC-METHOD.                                              ELTMCN  
00163      05  FILLER                  PIC X(79)   VALUE  'THE METHOD FOELTMCN  
00164 -    'R CALCULATING MANAGED CARE NETWORK BENEFITS IS:'.           ELTMCN  
00165 *                                                                 ELTMCN  
00166  01  WS-BENEFITS-REDUCTION.                                       ELTMCN  
00167      05  FILLER                  PIC X(79)   VALUE                ELTMCN  
00168          'DENIED OR REDUCED BENEFITS DUE TO COST CONTAINMENT:'.   ELTMCN  
00169 *                                                                 ELTMCN  
00170  01  WS-SPILLOVER-SENTENCE.                                       ELTMCN  
00171      05  FILLER                 PIC X(79)    VALUE                ELTMCN  
00172          'UNPAID SERVICES AFTER BASIC BENEFIT REDUCTIONS ARE '.   ELTMCN  
00173 *                                                                 ELTMCN  
00174  01  WS-NOT-APPLICABLE-LOB-BC.                                    ELTMCN  
00175      05  FILLER                    PIC X(79)   VALUE              ELTMCN  
00176          'THE MANAGED CARE PROGRAM DOES NOT APPLY TO INSTITUTIONALELTMCN  
00177 -        ' BENEFITS.'.                                            ELTMCN  
00178 *                                                                 ELTMCN  
00179  01  WS-NOT-APPLICABLE-LOB-BS.                                    ELTMCN  
00180      05  FILLER                    PIC X(79)   VALUE              ELTMCN  
00181          'THE MANAGED CARE PROGRAM DOES NOT APPLY TO PROFESSIONAL ELTMCN  
00182 -        'BENEFITS.'.                                             ELTMCN  
00183 *                                                                 ELTMCN  
00184  01  WS-NOT-APPLICABLE-LOB-MM.                                    ELTMCN  
00185      05  FILLER                    PIC X(79)   VALUE              ELTMCN  
00186          'THE MANAGED CARE PROGRAM DOES NOT APPLY TO SUPPLEMENTAL ELTMCN  
00187 -        'BENEFITS.'.                                             ELTMCN  
00188 *                                                                 ELTMCN  
00189  01  WS-NOT-APPLICABLE-MSG.                                       ELTMCN  
00190      05  FILLER                    PIC X(79)  VALUE               ELTMCN  
00191          'THE MANAGED CARE PROGRAM IS NOT APPLICABLE.'.           ELTMCN  
00192 *                                                                 ELTMCN  
00193  01  SPECIAL-PROCEDURES-MSG.                                      ELTMCN  
00194      05  FILLER                    PIC X(79)  VALUE               ELTMCN  
00195         'THERE ARE SPECIAL RELATED PROCEDURES INCLUDED IN THIS COSELTMCN  
00196 -       'T CONTAINMENT PROGRAM.'.                                 ELTMCN  
00197 *                                                                 ELTMCN  
00198  01  SPECIAL-SERVICES-MSG.                                        ELTMCN  
00199      05  FILLER                    PIC X(79)  VALUE               ELTMCN  
00200         'THERE ARE SPECIAL RELATED SERVICES INCLUDED IN THIS COST ELTMCN  
00201 -       'CONTAINMENT PROGRAM.'.                                   ELTMCN  
00202 *                                                                 ELTMCN  
00203  01  WS-DISCLAIMER.                                               ELTMCN  
00204      05  FILLER                    PIC X(79)  VALUE               ELTMCN  
00205         '*** SUBJECT TO CONTRACT LIMITATIONS ***'.                ELTMCN  
00206  01  WS-END                              PIC X(18) VALUE          ELTMCN  
00207                                          '*** END OF W/S ***'.    ELTMCN  
00208  LINKAGE SECTION.                                                 ELTMCN  
00209  01  DFHCOMMAREA.                                                 ELTMCN  
00210      COPY ELSCOMMC.                                               ELTMCN  
00211 /                                                                 ELTMCN  
00212      COPY ELSCIA2C.                                               ELTMCN  
00213 /                                                                 ELTMCN  
00214      COPY ELSCMDSC.                                               ELTMCN  
00215 /                                                                 ELTMCN  
00216      COPY ELSCMIFC.                                               ELTMCN  
00217 /                                                                 ELTMCN  
00218      COPY ELSIOPMC.                                               ELTMCN  
00219 /                                                                 ELTMCN  
00220      COPY ELSKEYSC.                                               ELTMCN  
00221 /                                                                 ELTMCN  
00222      COPY ELSOUTPC.                                               ELTMCN  
00223 /                                                                 ELTMCN  
00224      COPY ELSSRTPC.                                               ELTMCN  
00225 /                                                                 ELTMCN  
00226      COPY ELSTCWAC.                                               ELTMCN  
00227 /                                                                 ELTMCN  
00228      COPY ELSSSCBC.                                               ELTMCN  
00229 /                                                                 ELTMCN  
00230  01  GROUP-SPECIFIC-RECORD.                                       ELTMCN  
00231      COPY GCGROUPC.                                               ELTMCN  
00232 /                                                                 ELTMCN  
00233  01  GCCP-TABULAR-REC.                                            ELTMCN  
00234      COPY GCTGCCPC.                                               ELTMCN  
00235 /                                                                 ELTMCN  
00236      EJECT                                                        ELTMCN  
00237  PROCEDURE DIVISION.                                              ELTMCN  
00238 ************************************************************      ELTMCN  
00239 *                                                          *      ELTMCN  
00240 *                    PROCEDURE DIVISION                    *      ELTMCN  
00241 *                                                          *      ELTMCN  
00242 ************************************************************      ELTMCN  
00243                                                                   ELTMCN  
00244                                                                   ELTMCN  
00245 ************************************************************      ELTMCN  
00246 *                                                          *      ELTMCN  
00247 *        MANAGED CARE NETWORK                              *      ELTMCN  
00248 *                                                          *      ELTMCN  
00249 ************************************************************      ELTMCN  
00250  MANAGED-CARE-NETWORK.                                            ELTMCN  
00251      PERFORM INITIALIZATION.                                      ELTMCN  
00252      PERFORM PROCESS-MCN.                                         ELTMCN  
00253      GOBACK.                                                      ELTMCN  
00254                                                                   ELTMCN  
00255                                                                   ELTMCN  
00256 ************************************************************      ELTMCN  
00257 *                                                          *      ELTMCN  
00258 *        INITIALIZATION                                    *      ELTMCN  
00259 *                                                          *      ELTMCN  
00260 ************************************************************      ELTMCN  
00261  INITIALIZATION.                                                  ELTMCN  
00262      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTMCN  
00263      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTMCN  
00264                                                                   ELTMCN  
00265                                                                   ELTMCN  
00266 ************************************************************      ELTMCN  
00267 *                                                          *      ELTMCN  
00268 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTMCN  
00269 *                                                          *      ELTMCN  
00270 ************************************************************      ELTMCN  
00271  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTMCN  
00272      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTMCN  
00273      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTMCN  
00274      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTMCN  
00275                                                                   ELTMCN  
00276                                                                   ELTMCN  
00277 ************************************************************      ELTMCN  
00278 *                                                          *      ELTMCN  
00279 *        CHECK FOR VALID COMMAREA                          *      ELTMCN  
00280 *                                                          *      ELTMCN  
00281 ************************************************************      ELTMCN  
00282  CHECK-FOR-VALID-COMMAREA.                                        ELTMCN  
00283      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTMCN  
00284          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTMCN  
00285                                                                   ELTMCN  
00286                                                                   ELTMCN  
00287 ************************************************************      ELTMCN  
00288 *                                                          *      ELTMCN  
00289 *        SIGNAL INVALID COMMAREA                           *      ELTMCN  
00290 *                                                          *      ELTMCN  
00291 ************************************************************      ELTMCN  
00292  SIGNAL-INVALID-COMMAREA.                                         ELTMCN  
00293      EXEC CICS ABEND                                              ELTMCN  
00294                ABCODE('EL01')                                     ELTMCN  
00295         END-EXEC.                                                 ELTMCN  
00296      EJECT                                                        ELTMCN  
00297                                                                   ELTMCN  
00298                                                                   ELTMCN  
00299 ************************************************************      ELTMCN  
00300 *                                                          *      ELTMCN  
00301 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTMCN  
00302 *                                                          *      ELTMCN  
00303 ************************************************************      ELTMCN  
00304  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTMCN  
00305      IF ECA-CIA-PTR = NULL                                        ELTMCN  
00306          PERFORM SIGNAL-INVALID-CIA                               ELTMCN  
00307      ELSE                                                         ELTMCN  
00308          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTMCN  
00309                                                                   ELTMCN  
00310                                                                   ELTMCN  
00311 ************************************************************      ELTMCN  
00312 *                                                          *      ELTMCN  
00313 *        SIGNAL INVALID CIA                                *      ELTMCN  
00314 *                                                          *      ELTMCN  
00315 ************************************************************      ELTMCN  
00316  SIGNAL-INVALID-CIA.                                              ELTMCN  
00317      EXEC CICS ABEND                                              ELTMCN  
00318                ABCODE('EL02')                                     ELTMCN  
00319         END-EXEC.                                                 ELTMCN  
00320      EJECT                                                        ELTMCN  
00321                                                                   ELTMCN  
00322                                                                   ELTMCN  
00323 ************************************************************      ELTMCN  
00324 *                                                          *      ELTMCN  
00325 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTMCN  
00326 *                                                          *      ELTMCN  
00327 ************************************************************      ELTMCN  
00328  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTMCN  
00329      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTMCN  
00330      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMCN  
00331          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTMCN  
00332      IF CIA-RC-PTR-NULL                                           ELTMCN  
00333          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMCN  
00334                                                                   ELTMCN  
00335                                                                   ELTMCN  
00336 ************************************************************      ELTMCN  
00337 *                                                          *      ELTMCN  
00338 *        SIGNAL UNALLOC AREA ERROR                         *      ELTMCN  
00339 *                                                          *      ELTMCN  
00340 ************************************************************      ELTMCN  
00341  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTMCN  
00342      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTMCN  
00343      PERFORM SIGNAL-ABEND.                                        ELTMCN  
00344                                                                   ELTMCN  
00345                                                                   ELTMCN  
00346 ************************************************************      ELTMCN  
00347 *                                                          *      ELTMCN  
00348 *        SIGNAL ABEND                                      *      ELTMCN  
00349 *                                                          *      ELTMCN  
00350 ************************************************************      ELTMCN  
00351  SIGNAL-ABEND.                                                    ELTMCN  
00352      EXEC CICS ABEND                                              ELTMCN  
00353                ABCODE(CIA-ABCODE)                                 ELTMCN  
00354         END-EXEC.                                                 ELTMCN  
00355      EJECT                                                        ELTMCN  
00356                                                                   ELTMCN  
00357                                                                   ELTMCN  
00358 ************************************************************      ELTMCN  
00359 *                                                          *      ELTMCN  
00360 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTMCN  
00361 *                                                          *      ELTMCN  
00362 ************************************************************      ELTMCN  
00363  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTMCN  
00364      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTMCN  
00365      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTMCN  
00366      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTMCN  
00367      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTMCN  
00368      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTMCN  
00369      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTMCN  
00370      PERFORM ESTABLISH-ADDRESSABILITY-OF-CO.                      ELTMCN  
00371                                                                   ELTMCN  
00372                                                                   ELTMCN  
00373 ************************************************************      ELTMCN  
00374 *                                                          *      ELTMCN  
00375 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTMCN  
00376 *                                                          *      ELTMCN  
00377 ************************************************************      ELTMCN  
00378  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTMCN  
00379      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTMCN  
00380      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMCN  
00381          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTMCN  
00382      IF CIA-RC-PTR-NULL                                           ELTMCN  
00383          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMCN  
00384      EJECT                                                        ELTMCN  
00385                                                                   ELTMCN  
00386                                                                   ELTMCN  
00387 ************************************************************      ELTMCN  
00388 *                                                          *      ELTMCN  
00389 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTMCN  
00390 *                                                          *      ELTMCN  
00391 ************************************************************      ELTMCN  
00392  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTMCN  
00393      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTMCN  
00394      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMCN  
00395          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTMCN  
00396      IF CIA-RC-PTR-NULL                                           ELTMCN  
00397          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMCN  
00398      EJECT                                                        ELTMCN  
00399                                                                   ELTMCN  
00400                                                                   ELTMCN  
00401 ************************************************************      ELTMCN  
00402 *                                                          *      ELTMCN  
00403 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTMCN  
00404 *                                                          *      ELTMCN  
00405 ************************************************************      ELTMCN  
00406  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTMCN  
00407      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTMCN  
00408      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMCN  
00409          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELTMCN  
00410      IF CIA-RC-PTR-NULL                                           ELTMCN  
00411          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMCN  
00412      EJECT                                                        ELTMCN  
00413                                                                   ELTMCN  
00414                                                                   ELTMCN  
00415 ************************************************************      ELTMCN  
00416 *                                                          *      ELTMCN  
00417 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTMCN  
00418 *                                                          *      ELTMCN  
00419 ************************************************************      ELTMCN  
00420  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTMCN  
00421      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTMCN  
00422      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMCN  
00423          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTMCN  
00424      IF CIA-RC-PTR-NULL                                           ELTMCN  
00425          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMCN  
00426      EJECT                                                        ELTMCN  
00427                                                                   ELTMCN  
00428                                                                   ELTMCN  
00429 ************************************************************      ELTMCN  
00430 *                                                          *      ELTMCN  
00431 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTMCN  
00432 *                                                          *      ELTMCN  
00433 ************************************************************      ELTMCN  
00434  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTMCN  
00435      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTMCN  
00436      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMCN  
00437          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTMCN  
00438      IF CIA-RC-PTR-NULL                                           ELTMCN  
00439          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMCN  
00440      EJECT                                                        ELTMCN  
00441                                                                   ELTMCN  
00442                                                                   ELTMCN  
00443 ************************************************************      ELTMCN  
00444 *                                                          *      ELTMCN  
00445 *        ESTABLISH ADDRESSABILITY OF GRP SPECIFIC          *      ELTMCN  
00446 *                                                          *      ELTMCN  
00447 ************************************************************      ELTMCN  
00448  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTMCN  
00449      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTMCN  
00450      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMCN  
00451          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELTMCN  
00452      IF CIA-RC-PTR-NULL                                           ELTMCN  
00453          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMCN  
00454      EJECT                                                        ELTMCN  
00455                                                                   ELTMCN  
00456                                                                   ELTMCN  
00457 ************************************************************      ELTMCN  
00458 *                                                          *      ELTMCN  
00459 *        ESTABLISH ADDRESSABILITY OF COST CONTAINMENT      *      ELTMCN  
00460 *                                                          *      ELTMCN  
00461 ************************************************************      ELTMCN  
00462  ESTABLISH-ADDRESSABILITY-OF-CO.                                  ELTMCN  
00463      SET CIA-GCTABULR-DDN TO TRUE.                                ELTMCN  
00464      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMCN  
00465          ADDRESS OF GCCP-TABULAR-REC.                             ELTMCN  
00466      IF CIA-RC-PTR-NULL                                           ELTMCN  
00467          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMCN  
00468                                                                   ELTMCN  
00469                                                                   ELTMCN  
00470 ************************************************************      ELTMCN  
00471 *                                                          *      ELTMCN  
00472 *        ESTABLISH ADDRESS OF CIA                          *      ELTMCN  
00473 *                                                          *      ELTMCN  
00474 ************************************************************      ELTMCN  
00475  ESTABLISH-ADDRESS-OF-CIA.                                        ELTMCN  
00476      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTMCN  
00477          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTMCN  
00478      EJECT                                                        ELTMCN  
00479                                                                   ELTMCN  
00480                                                                   ELTMCN  
00481 ************************************************************      ELTMCN  
00482 *                                                          *      ELTMCN  
00483 *        PROCESS MCN                                       *      ELTMCN  
00484 *                                                          *      ELTMCN  
00485 ************************************************************      ELTMCN  
00486  PROCESS-MCN.                                                     ELTMCN  
00487      IF GCG-POS-PARTICP-IND EQUAL ZERO                            ELTMCN  
00488          PERFORM TEST-APPLICABILITY                               ELTMCN  
00489      ELSE                                                         ELTMCN  
00490          PERFORM GENERATE-MANAGED-CARE-NETWORKX.                  ELTMCN  
00491      MOVE 'E' TO  COF-FUNCTION.                                   ELTMCN  
00492      MOVE ZEROS TO COF-NBR-DTL-LINES                              ELTMCN  
00493                COF-NBR-HDR-LINES.                                 ELTMCN  
00494      PERFORM LINK-TO-OUTPUT.                                      ELTMCN  
00495      EJECT                                                        ELTMCN  
00496                                                                   ELTMCN  
00497                                                                   ELTMCN  
00498 ************************************************************      ELTMCN  
00499 *                                                          *      ELTMCN  
00500 *        GENERATE MANAGED CARE NETWORK TEXT                *      ELTMCN  
00501 *                                                          *      ELTMCN  
00502 ************************************************************      ELTMCN  
00503  GENERATE-MANAGED-CARE-NETWORKX.                                  ELTMCN  
00504      PERFORM VERIFY-MANAGED-CARE-NETWORK-IN.                      ELTMCN  
00505      PERFORM BUILD-MANAGED-CARE-NETWORK-TEX.                      ELTMCN  
00506                                                                   ELTMCN  
00507                                                                   ELTMCN  
00508 ************************************************************      ELTMCN  
00509 *                                                          *      ELTMCN  
00510 *        TEST APPLICABILITY                                *      ELTMCN  
00511 *                                                          *      ELTMCN  
00512 ************************************************************      ELTMCN  
00513  TEST-APPLICABILITY.                                              ELTMCN  
00514      PERFORM GENERATE-HEADINGS.                                   ELTMCN  
00515      IF GCG-POS-PARTICP-IND  EQUAL ZERO                           ELTMCN  
00516          PERFORM SIGNAL-NOT-APPLICABLE-MSG.                       ELTMCN  
00517      PERFORM LINK-TO-OUTPUT.                                      ELTMCN  
00518                                                                   ELTMCN  
00519                                                                   ELTMCN  
00520 ************************************************************      ELTMCN  
00521 *                                                          *      ELTMCN  
00522 *        VERIFY MANAGED CARE NETWORK IN GCCP RECORD        *      ELTMCN  
00523 *                                                          *      ELTMCN  
00524 ************************************************************      ELTMCN  
00525  VERIFY-MANAGED-CARE-NETWORK-IN.                                  ELTMCN  
00526      PERFORM ACQUIRE-GCCP-RECORD.                                 ELTMCN  
00527      PERFORM OBTAIN-MANAGED-CARE-NETWORK-WI.                      ELTMCN  
00528      EJECT                                                        ELTMCN  
00529                                                                   ELTMCN  
00530                                                                   ELTMCN  
00531 ************************************************************      ELTMCN  
00532 *                                                          *      ELTMCN  
00533 *        ACQUIRE GCCP RECORD                               *      ELTMCN  
00534 *                                                          *      ELTMCN  
00535 ************************************************************      ELTMCN  
00536  ACQUIRE-GCCP-RECORD.                                             ELTMCN  
00537      MOVE SPACES TO KWA-PROVISION-ID.                             ELTMCN  
00538      SET GCG-INDEX TO 1.                                          ELTMCN  
00539      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTMCN  
00540          AT END                                                   ELTMCN  
00541             MOVE ZEROES TO KWA-PROVISION-SLOT-NO                  ELTMCN  
00542             WHEN GCG-TAB-ID (GCG-INDEX) = PC-GCCP                 ELTMCN  
00543                 MOVE GCG-TAB-ID (GCG-INDEX) TO                    ELTMCN  
00544          KWA-PROVISION-ID                                         ELTMCN  
00545                 MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                  ELTMCN  
00546                     TO KWA-PROVISION-SLOT-NO                      ELTMCN  
00547          END-SEARCH.                                              ELTMCN  
00548      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELTMCN  
00549          PERFORM SIGNAL-UNDEFINED-TABULAR                         ELTMCN  
00550      ELSE                                                         ELTMCN  
00551          PERFORM READ-GCCP-RECORD.                                ELTMCN  
00552                                                                   ELTMCN  
00553                                                                   ELTMCN  
00554 ************************************************************      ELTMCN  
00555 *                                                          *      ELTMCN  
00556 *        SIGNAL UNDEFINED TABULAR                          *      ELTMCN  
00557 *                                                          *      ELTMCN  
00558 ************************************************************      ELTMCN  
00559  SIGNAL-UNDEFINED-TABULAR.                                        ELTMCN  
00560      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTMCN  
00561      PERFORM SIGNAL-ABEND.                                        ELTMCN  
00562                                                                   ELTMCN  
00563                                                                   ELTMCN  
00564 ************************************************************      ELTMCN  
00565 *                                                          *      ELTMCN  
00566 *        OBTAIN MANAGED CARE NETWORK WITHIN GCCP RECORD    *      ELTMCN  
00567 *                                                          *      ELTMCN  
00568 ************************************************************      ELTMCN  
00569  OBTAIN-MANAGED-CARE-NETWORK-WI.                                  ELTMCN  
00570      SET GSS-INDEX TO 1.                                          ELTMCN  
00571      SEARCH GSS-ENTRY                                             ELTMCN  
00572         AT END                                                    ELTMCN  
00573            SET TABULAR-IS-UNDEFINED TO TRUE                       ELTMCN  
00574            WHEN GSS-PS-PROG-CODE-CHR (GSS-INDEX)                  ELTMCN  
00575                 CONTINUE                                          ELTMCN  
00576          END-SEARCH.                                              ELTMCN  
00577      IF TABULAR-IS-UNDEFINED                                      ELTMCN  
00578          PERFORM SIGNAL-UNDEFINED-TABULAR.                        ELTMCN  
00579      EJECT                                                        ELTMCN  
00580                                                                   ELTMCN  
00581                                                                   ELTMCN  
00582 ************************************************************      ELTMCN  
00583 *                                                          *      ELTMCN  
00584 *        BUILD MANAGED CARE NETWORK TEXT                   *      ELTMCN  
00585 *                                                          *      ELTMCN  
00586 ************************************************************      ELTMCN  
00587  BUILD-MANAGED-CARE-NETWORK-TEX.                                  ELTMCN  
00588      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTMCN  
00589          PERFORM GENERATE-INSTITUTIONAL.                          ELTMCN  
00590      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELTMCN  
00591          PERFORM GENERATE-PROFESSIONAL.                           ELTMCN  
00592      IF GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                        ELTMCN  
00593                 '03' OR '04' OR '06' OR '08'                      ELTMCN  
00594          PERFORM PROCESS-SUPPLEMENTAL.                            ELTMCN  
00595                                                                   ELTMCN  
00596                                                                   ELTMCN  
00597 ************************************************************      ELTMCN  
00598 *                                                          *      ELTMCN  
00599 *        GENERATE INSTITUTIONAL                            *      ELTMCN  
00600 *                                                          *      ELTMCN  
00601 ************************************************************      ELTMCN  
00602  GENERATE-INSTITUTIONAL.                                          ELTMCN  
00603      SET INSTITUTIONAL-SCREEN TO TRUE.                            ELTMCN  
00604      MOVE 'INSTITUTIONAL' TO HDR-TITLE.                           ELTMCN  
00605      PERFORM GENERATE-HEADINGS.                                   ELTMCN  
00606      PERFORM BUILD-INSTITUTIONAL-TEXT.                            ELTMCN  
00607      EJECT                                                        ELTMCN  
00608                                                                   ELTMCN  
00609                                                                   ELTMCN  
00610 ************************************************************      ELTMCN  
00611 *                                                          *      ELTMCN  
00612 *        GENERATE PROFESSIONAL                             *      ELTMCN  
00613 *                                                          *      ELTMCN  
00614 ************************************************************      ELTMCN  
00615  GENERATE-PROFESSIONAL.                                           ELTMCN  
00616      SET PROFESSIONAL-SCREEN TO TRUE.                             ELTMCN  
00617      MOVE 'PROFESSIONAL' TO HDR-TITLE.                            ELTMCN  
00618      PERFORM GENERATE-HEADINGS.                                   ELTMCN  
00619      PERFORM BUILD-PROFESSIONAL-TEXT.                             ELTMCN  
00620      EJECT                                                        ELTMCN  
00621                                                                   ELTMCN  
00622                                                                   ELTMCN  
00623 ************************************************************      ELTMCN  
00624 *                                                          *      ELTMCN  
00625 *        PROCESS SUPPLEMENTAL                              *      ELTMCN  
00626 *                                                          *      ELTMCN  
00627 ************************************************************      ELTMCN  
00628  PROCESS-SUPPLEMENTAL.                                            ELTMCN  
00629      SET SUPPLEMENTAL-SCREEN TO TRUE.                             ELTMCN  
00630      MOVE 'SUPPLEMENTAL' TO HDR-TITLE.                            ELTMCN  
00631      PERFORM GENERATE-HEADINGS.                                   ELTMCN  
00632      PERFORM BUILD-SUPPLEMENTAL-TEXT.                             ELTMCN  
00633      EJECT                                                        ELTMCN  
00634                                                                   ELTMCN  
00635                                                                   ELTMCN  
00636 ************************************************************      ELTMCN  
00637 *                                                          *      ELTMCN  
00638 *        GENERATE HEADINGS                                 *      ELTMCN  
00639 *                                                          *      ELTMCN  
00640 ************************************************************      ELTMCN  
00641  GENERATE-HEADINGS.                                               ELTMCN  
00642      SET COF-NEW-PAGE TO TRUE.                                    ELTMCN  
00643      MOVE 2            TO COF-NBR-HDR-LINES.                      ELTMCN  
00644      MOVE WS-HDR-LN2   TO COF-HDR-LINE                            ELTMCN  
00645          (COF-NBR-HDR-LINES).                                     ELTMCN  
00646      MOVE +1           TO COF-NBR-DTL-LINES.                      ELTMCN  
00647      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMCN  
00648      PERFORM LINK-TO-OUTPUT.                                      ELTMCN  
00649                                                                   ELTMCN  
00650                                                                   ELTMCN  
00651 ************************************************************      ELTMCN  
00652 *                                                          *      ELTMCN  
00653 *        BUILD INSTITUTIONAL TEXT                          *      ELTMCN  
00654 *                                                          *      ELTMCN  
00655 ************************************************************      ELTMCN  
00656  BUILD-INSTITUTIONAL-TEXT.                                        ELTMCN  
00657      IF GSS-PS-BC-IND (GSS-INDEX) EQUAL ZEROES OR SPACES          ELTMCN  
00658                 OR LOW-VALUES                                     ELTMCN  
00659          PERFORM SIGNAL-NOT-APPLICABLE-FOR-BC-L                   ELTMCN  
00660      ELSE                                                         ELTMCN  
00661          PERFORM CONSTRUCT-BC-TEXT-AND-SCREEN.                    ELTMCN  
00662                                                                   ELTMCN  
00663                                                                   ELTMCN  
00664 ************************************************************      ELTMCN  
00665 *                                                          *      ELTMCN  
00666 *        BUILD PROFESSIONAL TEXT                           *      ELTMCN  
00667 *                                                          *      ELTMCN  
00668 ************************************************************      ELTMCN  
00669  BUILD-PROFESSIONAL-TEXT.                                         ELTMCN  
00670      IF GSS-PS-BS-IND (GSS-INDEX) EQUAL ZEROES OR SPACES          ELTMCN  
00671                 OR LOW-VALUES                                     ELTMCN  
00672          PERFORM SIGNAL-NOT-APPLICABLE-FOR-BS-L                   ELTMCN  
00673      ELSE                                                         ELTMCN  
00674          PERFORM CONSTRUCT-BS-TEXT-AND-SCREEN.                    ELTMCN  
00675                                                                   ELTMCN  
00676                                                                   ELTMCN  
00677 ************************************************************      ELTMCN  
00678 *                                                          *      ELTMCN  
00679 *        BUILD SUPPLEMENTAL TEXT                           *      ELTMCN  
00680 *                                                          *      ELTMCN  
00681 ************************************************************      ELTMCN  
00682  BUILD-SUPPLEMENTAL-TEXT.                                         ELTMCN  
00683      IF GSS-PS-MM-IND (GSS-INDEX) EQUAL ZEROES OR SPACES          ELTMCN  
00684                 OR LOW-VALUES                                     ELTMCN  
00685          PERFORM SIGNAL-NOT-APPLICABLE-FOR-MM-L                   ELTMCN  
00686      ELSE                                                         ELTMCN  
00687          PERFORM CONSTRUCT-MM-TEXT-AND-SCREEN.                    ELTMCN  
00688      EJECT                                                        ELTMCN  
00689                                                                   ELTMCN  
00690                                                                   ELTMCN  
00691 ************************************************************      ELTMCN  
00692 *                                                          *      ELTMCN  
00693 *        SIGNAL NOT APPLICABLE FOR BC LOB                  *      ELTMCN  
00694 *                                                          *      ELTMCN  
00695 ************************************************************      ELTMCN  
00696  SIGNAL-NOT-APPLICABLE-FOR-BC-L.                                  ELTMCN  
00697      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMCN  
00698      MOVE WS-NOT-APPLICABLE-LOB-BC TO COF-DTL-LINE                ELTMCN  
00699          (COF-NBR-DTL-LINES).                                     ELTMCN  
00700      PERFORM LINK-TO-OUTPUT.                                      ELTMCN  
00701                                                                   ELTMCN  
00702                                                                   ELTMCN  
00703 ************************************************************      ELTMCN  
00704 *                                                          *      ELTMCN  
00705 *        SIGNAL NOT APPLICABLE FOR BS LOB                  *      ELTMCN  
00706 *                                                          *      ELTMCN  
00707 ************************************************************      ELTMCN  
00708  SIGNAL-NOT-APPLICABLE-FOR-BS-L.                                  ELTMCN  
00709      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMCN  
00710      MOVE WS-NOT-APPLICABLE-LOB-BS TO COF-DTL-LINE                ELTMCN  
00711          (COF-NBR-DTL-LINES).                                     ELTMCN  
00712      PERFORM LINK-TO-OUTPUT.                                      ELTMCN  
00713                                                                   ELTMCN  
00714                                                                   ELTMCN  
00715 ************************************************************      ELTMCN  
00716 *                                                          *      ELTMCN  
00717 *        SIGNAL NOT APPLICABLE FOR MM LOB                  *      ELTMCN  
00718 *                                                          *      ELTMCN  
00719 ************************************************************      ELTMCN  
00720  SIGNAL-NOT-APPLICABLE-FOR-MM-L.                                  ELTMCN  
00721      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMCN  
00722      MOVE WS-NOT-APPLICABLE-LOB-MM TO COF-DTL-LINE                ELTMCN  
00723          (COF-NBR-DTL-LINES).                                     ELTMCN  
00724      PERFORM LINK-TO-OUTPUT.                                      ELTMCN  
00725      EJECT                                                        ELTMCN  
00726                                                                   ELTMCN  
00727                                                                   ELTMCN  
00728 ************************************************************      ELTMCN  
00729 *                                                          *      ELTMCN  
00730 *        CONSTRUCT BC TEXT AND SCREEN                      *      ELTMCN  
00731 *                                                          *      ELTMCN  
00732 ************************************************************      ELTMCN  
00733  CONSTRUCT-BC-TEXT-AND-SCREEN.                                    ELTMCN  
00734      SET PAYMENT-LVL-NOT-TRANSLATED TO TRUE.                      ELTMCN  
00735      PERFORM TRANSLATE-PARTICIPATION-IND.                         ELTMCN  
00736      IF GSS-PS-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTMCN  
00737          ZERO                                                     ELTMCN  
00738                 AND SPACES AND LOW-VALUES                         ELTMCN  
00739          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMCN  
00740      PERFORM TRANSLATE-BC-INDICATOR.                              ELTMCN  
00741      IF GSS-PS-BC-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL         ELTMCN  
00742          ZERO                                                     ELTMCN  
00743               AND SPACES AND LOW-VALUES                           ELTMCN  
00744          PERFORM TRANSLATE-BC-PAYMENT-LEVEL-IND.                  ELTMCN  
00745      SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTMCN  
00746      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTMCN  
00747      IF PAYMENT-LVL-NOT-TRANSLATED                                ELTMCN  
00748          PERFORM GENERATE-BC-CALC-METHOD-SENTEN.                  ELTMCN  
00749      PERFORM GENERATE-BC-BENEFITS-REDUCTION.                      ELTMCN  
00750      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                       ELTMCN  
00751               '03' OR '04' OR '06' OR '08')                       ELTMCN  
00752            AND                                                    ELTMCN  
00753             GSS-PS-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL           ELTMCN  
00754          ZERO                                                     ELTMCN  
00755                         AND SPACES AND LOW-VALUES                 ELTMCN  
00756          PERFORM GENERATE-SPILL-OVER-INDICATOR.                   ELTMCN  
00757      PERFORM GENERATE-RELATED-TABULAR.                            ELTMCN  
00758      PERFORM GENERATE-SPECIAL-TABULAR.                            ELTMCN  
00759      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTMCN  
00760      MOVE SPACES TO SCREEN-TYPE.                                  ELTMCN  
00761      INITIALIZE WS-PAYMENT-LEVEL-SW.                              ELTMCN  
00762      EJECT                                                        ELTMCN  
00763                                                                   ELTMCN  
00764                                                                   ELTMCN  
00765 ************************************************************      ELTMCN  
00766 *                                                          *      ELTMCN  
00767 *        CONSTRUCT BS TEXT AND SCREEN                      *      ELTMCN  
00768 *                                                          *      ELTMCN  
00769 ************************************************************      ELTMCN  
00770  CONSTRUCT-BS-TEXT-AND-SCREEN.                                    ELTMCN  
00771      SET PAYMENT-LVL-NOT-TRANSLATED TO TRUE.                      ELTMCN  
00772      SET PROFESSIONAL-SCREEN TO TRUE.                             ELTMCN  
00773      PERFORM TRANSLATE-PARTICIPATION-IND.                         ELTMCN  
00774      IF GSS-PS-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTMCN  
00775          ZERO                                                     ELTMCN  
00776                 AND SPACES AND LOW-VALUES                         ELTMCN  
00777          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMCN  
00778      PERFORM TRANSLATE-BS-INDICATOR.                              ELTMCN  
00779      IF GSS-PS-BS-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL         ELTMCN  
00780          ZERO                                                     ELTMCN  
00781               AND SPACES AND LOW-VALUES                           ELTMCN  
00782          PERFORM TRANSLATE-BS-PAYMENT-LEVEL-IND.                  ELTMCN  
00783      SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTMCN  
00784      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTMCN  
00785      IF PAYMENT-LVL-NOT-TRANSLATED                                ELTMCN  
00786          PERFORM GENERATE-BS-CALC-METHOD-SENTEN.                  ELTMCN  
00787      PERFORM GENERATE-BS-BENEFITS-REDUCTION.                      ELTMCN  
00788      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                       ELTMCN  
00789                   '03' OR '04' OR '06' OR '08')                   ELTMCN  
00790            AND                                                    ELTMCN  
00791             GSS-PS-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL           ELTMCN  
00792          ZERO                                                     ELTMCN  
00793                         AND SPACES AND LOW-VALUES                 ELTMCN  
00794          PERFORM GENERATE-SPILL-OVER-INDICATOR.                   ELTMCN  
00795      PERFORM GENERATE-RELATED-TABULAR.                            ELTMCN  
00796      PERFORM GENERATE-SPECIAL-TABULAR.                            ELTMCN  
00797      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTMCN  
00798      MOVE SPACES TO SCREEN-TYPE.                                  ELTMCN  
00799      INITIALIZE WS-PAYMENT-LEVEL-SW.                              ELTMCN  
00800      EJECT                                                        ELTMCN  
00801                                                                   ELTMCN  
00802                                                                   ELTMCN  
00803 ************************************************************      ELTMCN  
00804 *                                                          *      ELTMCN  
00805 *        CONSTRUCT MM TEXT AND SCREEN                      *      ELTMCN  
00806 *                                                          *      ELTMCN  
00807 ************************************************************      ELTMCN  
00808  CONSTRUCT-MM-TEXT-AND-SCREEN.                                    ELTMCN  
00809      PERFORM TRANSLATE-PARTICIPATION-IND.                         ELTMCN  
00810      IF GSS-PS-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTMCN  
00811          ZERO                                                     ELTMCN  
00812                 AND SPACES AND LOW-VALUES                         ELTMCN  
00813          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMCN  
00814      PERFORM TRANSLATE-MM-INDICATOR.                              ELTMCN  
00815      IF GSS-PS-MM-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL         ELTMCN  
00816          ZERO                                                     ELTMCN  
00817             AND SPACES AND LOW-VALUES                             ELTMCN  
00818          PERFORM TRANSLATE-MM-PAYMENT-LEVEL-IND.                  ELTMCN  
00819      SET SRP-ACCUM-PROV-CLASS-SUPP TO TRUE.                       ELTMCN  
00820      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTMCN  
00821      IF PAYMENT-LVL-NOT-TRANSLATED                                ELTMCN  
00822          PERFORM GENERATE-MM-CALC-METHOD-SENTEN.                  ELTMCN  
00823      PERFORM GENERATE-MM-BENEFITS-REDUCTION.                      ELTMCN  
00824      PERFORM GENERATE-RELATED-TABULAR.                            ELTMCN  
00825      PERFORM GENERATE-SPECIAL-TABULAR.                            ELTMCN  
00826      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTMCN  
00827      EJECT                                                        ELTMCN  
00828                                                                   ELTMCN  
00829                                                                   ELTMCN  
00830 ************************************************************      ELTMCN  
00831 *                                                          *      ELTMCN  
00832 *        GENERATE ASSOCIATED ACCUMULATORS                  *      ELTMCN  
00833 *                                                          *      ELTMCN  
00834 ************************************************************      ELTMCN  
00835  GENERATE-ASSOCIATED-ACCUMULATO.                                  ELTMCN  
00836      PERFORM GENERATE-COINSURANCE-TEXT.                           ELTMCN  
00837      PERFORM GENERATE-COPAY-TEXT.                                 ELTMCN  
00838      PERFORM GENERATE-DEDUCTIBLE-TEXT.                            ELTMCN  
00839      PERFORM GENERATE-BENEFIT-MAXIMUMS-TEXT.                      ELTMCN  
00840      EJECT                                                        ELTMCN  
00841                                                                   ELTMCN  
00842                                                                   ELTMCN  
00843 ************************************************************      ELTMCN  
00844 *                                                          *      ELTMCN  
00845 *        GENERATE DISCLAIMER SENTENCE                      *      ELTMCN  
00846 *                                                          *      ELTMCN  
00847 ************************************************************      ELTMCN  
00848  GENERATE-DISCLAIMER-SENTENCE.                                    ELTMCN  
00849      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMCN  
00850      MOVE WS-DISCLAIMER TO COF-DTL-LINE                           ELTMCN  
00851          (COF-NBR-DTL-LINES).                                     ELTMCN  
00852      PERFORM LINK-TO-OUTPUT.                                      ELTMCN  
00853      EJECT                                                        ELTMCN  
00854                                                                   ELTMCN  
00855                                                                   ELTMCN  
00856 ************************************************************      ELTMCN  
00857 *                                                          *      ELTMCN  
00858 *        TRANSLATE PARTICIPATION IND                       *      ELTMCN  
00859 *                                                          *      ELTMCN  
00860 ************************************************************      ELTMCN  
00861  TRANSLATE-PARTICIPATION-IND.                                     ELTMCN  
00862      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMCN  
00863      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMCN  
00864      SET PERIOD-NEEDED TO TRUE.                                   ELTMCN  
00865      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMCN  
00866      MOVE WS-PARTICIPATION-IND TO TCAR-FROM-LINE (TCAR-FROM-SUB). ELTMCN  
00867      ADD 1 TO TCAR-FROM-SUB.                                      ELTMCN  
00868      MOVE PC-GROUP TO CMF-RECORD-PREFIX.                          ELTMCN  
00869      MOVE 'POS-PARTICP-IND' TO CMF-ELEMENT-SYSTEM-NAME.           ELTMCN  
00870      MOVE GCG-POS-PARTICP-IND TO CMF-CODE-VALUE.                  ELTMCN  
00871      PERFORM CALL-TRANSLATOR.                                     ELTMCN  
00872      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMCN  
00873      EJECT                                                        ELTMCN  
00874                                                                   ELTMCN  
00875                                                                   ELTMCN  
00876 ************************************************************      ELTMCN  
00877 *                                                          *      ELTMCN  
00878 *        TRANSLATE APPROVAL SOURCE                         *      ELTMCN  
00879 *                                                          *      ELTMCN  
00880 ************************************************************      ELTMCN  
00881  TRANSLATE-APPROVAL-SOURCE.                                       ELTMCN  
00882      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMCN  
00883      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMCN  
00884      SET PERIOD-NEEDED TO TRUE.                                   ELTMCN  
00885      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMCN  
00886      MOVE WS-APPROVAL-SOURCE TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELTMCN  
00887      ADD 1 TO TCAR-FROM-SUB.                                      ELTMCN  
00888      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMCN  
00889      MOVE 'PS-APPROVAL-SOURCE-IND' TO CMF-ELEMENT-SYSTEM-NAME.    ELTMCN  
00890      MOVE GSS-PS-APPROVAL-SOURCE-IND (GSS-INDEX) TO               ELTMCN  
00891          CMF-CODE-VALUE.                                          ELTMCN  
00892      PERFORM CALL-TRANSLATOR.                                     ELTMCN  
00893      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMCN  
00894                                                                   ELTMCN  
00895                                                                   ELTMCN  
00896 ************************************************************      ELTMCN  
00897 *                                                          *      ELTMCN  
00898 *        GET INDICATOR FIXED TEXT                          *      ELTMCN  
00899 *                                                          *      ELTMCN  
00900 ************************************************************      ELTMCN  
00901  GET-INDICATOR-FIXED-TEXT.                                        ELTMCN  
00902      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMCN  
00903      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMCN  
00904      SET PERIOD-NEEDED TO TRUE.                                   ELTMCN  
00905      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMCN  
00906      MOVE WS-INDICATOR TO TCAR-FROM-LINE (TCAR-FROM-SUB).         ELTMCN  
00907      ADD 1 TO TCAR-FROM-SUB.                                      ELTMCN  
00908      EJECT                                                        ELTMCN  
00909                                                                   ELTMCN  
00910                                                                   ELTMCN  
00911 ************************************************************      ELTMCN  
00912 *                                                          *      ELTMCN  
00913 *        TRANSLATE BC INDICATOR                            *      ELTMCN  
00914 *                                                          *      ELTMCN  
00915 ************************************************************      ELTMCN  
00916  TRANSLATE-BC-INDICATOR.                                          ELTMCN  
00917      PERFORM GET-INDICATOR-FIXED-TEXT.                            ELTMCN  
00918      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMCN  
00919      MOVE 'PS-BC-IND'  TO CMF-ELEMENT-SYSTEM-NAME.                ELTMCN  
00920      MOVE GSS-PS-BC-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTMCN  
00921      PERFORM CALL-TRANSLATOR.                                     ELTMCN  
00922      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMCN  
00923      EJECT                                                        ELTMCN  
00924                                                                   ELTMCN  
00925                                                                   ELTMCN  
00926 ************************************************************      ELTMCN  
00927 *                                                          *      ELTMCN  
00928 *        TRANSLATE BC PAYMENT LEVEL INDICATOR              *      ELTMCN  
00929 *                                                          *      ELTMCN  
00930 ************************************************************      ELTMCN  
00931  TRANSLATE-BC-PAYMENT-LEVEL-IND.                                  ELTMCN  
00932      PERFORM SETUP-PAYMENT-LEVEL-FIXED-TEXT.                      ELTMCN  
00933      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMCN  
00934      MOVE 'PS-BC-PAYMENT-LEVEL-IND'  TO CMF-ELEMENT-SYSTEM-NAME.  ELTMCN  
00935      MOVE GSS-PS-BC-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTMCN  
00936          CMF-CODE-VALUE.                                          ELTMCN  
00937      PERFORM CALL-TRANSLATOR.                                     ELTMCN  
00938      PERFORM MOVE-TRANSLATED-TEXT-TO-OUTPUT.                      ELTMCN  
00939      SET PAYMENT-LVL-TRANSLATED TO TRUE.                          ELTMCN  
00940                                                                   ELTMCN  
00941                                                                   ELTMCN  
00942 ************************************************************      ELTMCN  
00943 *                                                          *      ELTMCN  
00944 *        MOVE TRANSLATED TEXT TO OUTPUT                    *      ELTMCN  
00945 *                                                          *      ELTMCN  
00946 ************************************************************      ELTMCN  
00947  MOVE-TRANSLATED-TEXT-TO-OUTPUT.                                  ELTMCN  
00948      PERFORM DO-MOVE-OF-TEXT-TEXT                                 ELTMCN  
00949          VARYING WS-CMF-SUB FROM 1                                ELTMCN  
00950                       BY 1 UNTIL WS-CMF-SUB                       ELTMCN  
00951                         > CMF-NBR-DESCR-LINES.                    ELTMCN  
00952      PERFORM LINK-TO-OUTPUT.                                      ELTMCN  
00953      EJECT                                                        ELTMCN  
00954                                                                   ELTMCN  
00955                                                                   ELTMCN  
00956 ************************************************************      ELTMCN  
00957 *                                                          *      ELTMCN  
00958 *        DO MOVE OF TEXT TEXT                              *      ELTMCN  
00959 *                                                          *      ELTMCN  
00960 ************************************************************      ELTMCN  
00961  DO-MOVE-OF-TEXT-TEXT.                                            ELTMCN  
00962      SET CMF-DESCR-IDX TO WS-CMF-SUB.                             ELTMCN  
00963      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX)                          ELTMCN  
00964          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTMCN  
00965      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMCN  
00966                                                                   ELTMCN  
00967                                                                   ELTMCN  
00968 ************************************************************      ELTMCN  
00969 *                                                          *      ELTMCN  
00970 *        SETUP PAYMENT LEVEL FIXED TEXT                    *      ELTMCN  
00971 *                                                          *      ELTMCN  
00972 ************************************************************      ELTMCN  
00973  SETUP-PAYMENT-LEVEL-FIXED-TEXT.                                  ELTMCN  
00974      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMCN  
00975      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMCN  
00976      STRING WS-PAYMENT-LEVEL                                      ELTMCN  
00977          DELIMITED BY SIZE INTO COF-DTL-LINE                      ELTMCN  
00978          (COF-NBR-DTL-LINES).                                     ELTMCN  
00979      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTMCN  
00980                                                                   ELTMCN  
00981                                                                   ELTMCN  
00982 ************************************************************      ELTMCN  
00983 *                                                          *      ELTMCN  
00984 *        GENERATE COINSURANCE TEXT                         *      ELTMCN  
00985 *                                                          *      ELTMCN  
00986 ************************************************************      ELTMCN  
00987  GENERATE-COINSURANCE-TEXT.                                       ELTMCN  
00988      EXEC CICS LINK                                               ELTMCN  
00989          PROGRAM ('ELGACLCC')                                     ELTMCN  
00990          COMMAREA (DFHCOMMAREA)                                   ELTMCN  
00991          END-EXEC.                                                ELTMCN  
00992                                                                   ELTMCN  
00993 ************************************************************      ELTMCN  
00994 *                                                          *      ELTMCN  
00995 *        GENERATE COPAY TEXT                               *      ELTMCN  
00996 *                                                          *      ELTMCN  
00997 ************************************************************      ELTMCN  
00998  GENERATE-COPAY-TEXT.                                             ELTMCN  
00999      EXEC CICS LINK                                               ELTMCN  
01000          PROGRAM ('ELGACPCC')                                     ELTMCN  
01001          COMMAREA (DFHCOMMAREA)                                   ELTMCN  
01002          END-EXEC.                                                ELTMCN  
01003                                                                   ELTMCN  
01004                                                                   ELTMCN  
01005 ************************************************************      ELTMCN  
01006 *                                                          *      ELTMCN  
01007 *        GENERATE DEDUCTIBLE TEXT                          *      ELTMCN  
01008 *                                                          *      ELTMCN  
01009 ************************************************************      ELTMCN  
01010  GENERATE-DEDUCTIBLE-TEXT.                                        ELTMCN  
01011      EXEC CICS LINK                                               ELTMCN  
01012          PROGRAM ('ELGADLCC')                                     ELTMCN  
01013          COMMAREA (DFHCOMMAREA)                                   ELTMCN  
01014          END-EXEC.                                                ELTMCN  
01015                                                                   ELTMCN  
01016                                                                   ELTMCN  
01017 ************************************************************      ELTMCN  
01018 *                                                          *      ELTMCN  
01019 *        GENERATE BENEFIT MAXIMUMS TEXT                    *      ELTMCN  
01020 *                                                          *      ELTMCN  
01021 ************************************************************      ELTMCN  
01022  GENERATE-BENEFIT-MAXIMUMS-TEXT.                                  ELTMCN  
01023      EXEC CICS LINK                                               ELTMCN  
01024          PROGRAM ('ELGABMCC')                                     ELTMCN  
01025          COMMAREA (DFHCOMMAREA)                                   ELTMCN  
01026          END-EXEC.                                                ELTMCN  
01027      EJECT                                                        ELTMCN  
01028                                                                   ELTMCN  
01029                                                                   ELTMCN  
01030 ************************************************************      ELTMCN  
01031 *                                                          *      ELTMCN  
01032 *        GENERATE BC CALC METHOD SENTENCE                  *      ELTMCN  
01033 *                                                          *      ELTMCN  
01034 ************************************************************      ELTMCN  
01035  GENERATE-BC-CALC-METHOD-SENTEN.                                  ELTMCN  
01036      IF GSS-PS-BC-CALC-METHOD (GSS-INDEX) NOT EQUAL ZEROES        ELTMCN  
01037                AND SPACES AND LOW-VALUES                          ELTMCN  
01038          PERFORM CREATE-BC-CALC-SENTENCE.                         ELTMCN  
01039                                                                   ELTMCN  
01040                                                                   ELTMCN  
01041 ************************************************************      ELTMCN  
01042 *                                                          *      ELTMCN  
01043 *        CREATE BC CALC SENTENCE                           *      ELTMCN  
01044 *                                                          *      ELTMCN  
01045 ************************************************************      ELTMCN  
01046  CREATE-BC-CALC-SENTENCE.                                         ELTMCN  
01047      PERFORM CREATE-CALC-METHOD-FIXED-TEXT.                       ELTMCN  
01048      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMCN  
01049      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMCN  
01050      MOVE 'PS-BC-CALC-METHOD'  TO                                 ELTMCN  
01051          CMF-ELEMENT-SYSTEM-NAME.                                 ELTMCN  
01052      MOVE GSS-PS-BC-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMCN  
01053      PERFORM CALL-TRANSLATOR.                                     ELTMCN  
01054      PERFORM MOVE-TRANSLATED-TEXT-TO-OUTPUT.                      ELTMCN  
01055                                                                   ELTMCN  
01056                                                                   ELTMCN  
01057 ************************************************************      ELTMCN  
01058 *                                                          *      ELTMCN  
01059 *        CREATE CALC METHOD FIXED TEXT                     *      ELTMCN  
01060 *                                                          *      ELTMCN  
01061 ************************************************************      ELTMCN  
01062  CREATE-CALC-METHOD-FIXED-TEXT.                                   ELTMCN  
01063      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMCN  
01064      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMCN  
01065      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMCN  
01066      STRING WS-CALC-METHOD                                        ELTMCN  
01067                DELIMITED BY SIZE INTO COF-DTL-LINE                ELTMCN  
01068          (COF-NBR-DTL-LINES).                                     ELTMCN  
01069      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMCN  
01070      EJECT                                                        ELTMCN  
01071                                                                   ELTMCN  
01072                                                                   ELTMCN  
01073 ************************************************************      ELTMCN  
01074 *                                                          *      ELTMCN  
01075 *        GENERATE BS CALC METHOD SENTENCE                  *      ELTMCN  
01076 *                                                          *      ELTMCN  
01077 ************************************************************      ELTMCN  
01078  GENERATE-BS-CALC-METHOD-SENTEN.                                  ELTMCN  
01079      IF GSS-PS-BS-CALC-METHOD (GSS-INDEX) NOT EQUAL ZEROES        ELTMCN  
01080                AND SPACES AND LOW-VALUES                          ELTMCN  
01081          PERFORM CREATE-BS-CALC-SENTENCE.                         ELTMCN  
01082                                                                   ELTMCN  
01083                                                                   ELTMCN  
01084 ************************************************************      ELTMCN  
01085 *                                                          *      ELTMCN  
01086 *        CREATE BS CALC SENTENCE                           *      ELTMCN  
01087 *                                                          *      ELTMCN  
01088 ************************************************************      ELTMCN  
01089  CREATE-BS-CALC-SENTENCE.                                         ELTMCN  
01090      PERFORM CREATE-CALC-METHOD-FIXED-TEXT.                       ELTMCN  
01091      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMCN  
01092      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMCN  
01093      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMCN  
01094      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMCN  
01095      MOVE 'PS-BS-CALC-METHOD'  TO                                 ELTMCN  
01096          CMF-ELEMENT-SYSTEM-NAME.                                 ELTMCN  
01097      MOVE GSS-PS-BS-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMCN  
01098      PERFORM CALL-TRANSLATOR.                                     ELTMCN  
01099      PERFORM MOVE-TRANSLATED-TEXT-TO-OUTPUT.                      ELTMCN  
01100      EJECT                                                        ELTMCN  
01101                                                                   ELTMCN  
01102                                                                   ELTMCN  
01103 ************************************************************      ELTMCN  
01104 *                                                          *      ELTMCN  
01105 *        GENERATE MM CALC METHOD SENTENCE                  *      ELTMCN  
01106 *                                                          *      ELTMCN  
01107 ************************************************************      ELTMCN  
01108  GENERATE-MM-CALC-METHOD-SENTEN.                                  ELTMCN  
01109      IF GSS-PS-MM-CALC-METHOD (GSS-INDEX) NOT EQUAL ZEROES        ELTMCN  
01110                AND SPACES AND LOW-VALUES                          ELTMCN  
01111          PERFORM CREATE-MM-CALC-SENTENCE.                         ELTMCN  
01112                                                                   ELTMCN  
01113                                                                   ELTMCN  
01114 ************************************************************      ELTMCN  
01115 *                                                          *      ELTMCN  
01116 *        CREATE MM CALC SENTENCE                           *      ELTMCN  
01117 *                                                          *      ELTMCN  
01118 ************************************************************      ELTMCN  
01119  CREATE-MM-CALC-SENTENCE.                                         ELTMCN  
01120      PERFORM CREATE-CALC-METHOD-FIXED-TEXT.                       ELTMCN  
01121      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMCN  
01122      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMCN  
01123      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMCN  
01124      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMCN  
01125      MOVE 'PS-MM-CALC-METHOD'  TO                                 ELTMCN  
01126          CMF-ELEMENT-SYSTEM-NAME.                                 ELTMCN  
01127      MOVE GSS-PS-MM-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMCN  
01128      PERFORM CALL-TRANSLATOR.                                     ELTMCN  
01129      PERFORM MOVE-TRANSLATED-TEXT-TO-OUTPUT.                      ELTMCN  
01130                                                                   ELTMCN  
01131                                                                   ELTMCN  
01132 ************************************************************      ELTMCN  
01133 *                                                          *      ELTMCN  
01134 *        GENERATE COMBINED BENEFITS REDUCTION SENTENCE     *      ELTMCN  
01135 *                                                          *      ELTMCN  
01136 ************************************************************      ELTMCN  
01137  GENERATE-COMBINED-BENEFITS-RED.                                  ELTMCN  
01138      MOVE 'PS' TO SRP-COST-CONT-TYPE.                             ELTMCN  
01139      MOVE 'MANAGED CARE NETWORK PROGRAM' TO SRP-CCP-NAME.         ELTMCN  
01140      MOVE GSS-PS-COMB-BENE-REDUCT-IND (GSS-INDEX) TO              ELTMCN  
01141           SRP-CCP-COMB-BENE-REDUCT-IND.                           ELTMCN  
01142      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELTMCN  
01143      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMCN  
01144          ADDRESS OF GCCP-TABULAR-REC.                             ELTMCN  
01145      EXEC CICS LINK                                               ELTMCN  
01146          PROGRAM ('ELGCBRI')                                      ELTMCN  
01147          COMMAREA (DFHCOMMAREA)                                   ELTMCN  
01148          END-EXEC.                                                ELTMCN  
01149      EJECT                                                        ELTMCN  
01150                                                                   ELTMCN  
01151                                                                   ELTMCN  
01152 ************************************************************      ELTMCN  
01153 *                                                          *      ELTMCN  
01154 *        GENERATE BC BENEFITS REDUCTION SENTENCE           *      ELTMCN  
01155 *                                                          *      ELTMCN  
01156 ************************************************************      ELTMCN  
01157  GENERATE-BC-BENEFITS-REDUCTION.                                  ELTMCN  
01158      IF GSS-PS-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTMCN  
01159                  ZERO AND SPACES AND LOW-VALUES                   ELTMCN  
01160          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTMCN  
01161      IF (GSS-PS-BC-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL          ELTMCN  
01162          ZERO                                                     ELTMCN  
01163                             AND SPACES AND LOW-VALUES)  OR        ELTMCN  
01164                (GSS-PS-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL   ELTMCN  
01165          ZERO                                                     ELTMCN  
01166                             AND SPACES AND LOW-VALUES)  OR        ELTMCN  
01167                (GSS-PS-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT       ELTMCN  
01168          EQUAL ZERO                                               ELTMCN  
01169                             AND SPACES AND LOW-VALUES)            ELTMCN  
01170          PERFORM GENERATE-BC-BENEFIT-REDUCTIONX.                  ELTMCN  
01171      EJECT                                                        ELTMCN  
01172                                                                   ELTMCN  
01173                                                                   ELTMCN  
01174 ************************************************************      ELTMCN  
01175 *                                                          *      ELTMCN  
01176 *        GENERATE BC BENEFIT REDUCTION TEXT                *      ELTMCN  
01177 *                                                          *      ELTMCN  
01178 ************************************************************      ELTMCN  
01179  GENERATE-BC-BENEFIT-REDUCTIONX.                                  ELTMCN  
01180      PERFORM GENERATE-REDUCTIONS-HEADINGS.                        ELTMCN  
01181      IF GSS-PS-BC-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMCN  
01182          ZERO                                                     ELTMCN  
01183                             AND SPACES AND LOW-VALUES             ELTMCN  
01184          PERFORM TRANSLATE-BC-DEDU-APPLIC.                        ELTMCN  
01185      IF GSS-PS-BC-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMCN  
01186          ZERO                                                     ELTMCN  
01187                             AND SPACES AND LOW-VALUES             ELTMCN  
01188          PERFORM TRANSLATE-BC-OPEX-APPLIC.                        ELTMCN  
01189      PERFORM CREATE-A-BLANK-LINE.                                 ELTMCN  
01190      PERFORM LINK-TO-OUTPUT.                                      ELTMCN  
01191      IF GSS-PS-BC-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTMCN  
01192          ZERO                                                     ELTMCN  
01193                             AND SPACES AND LOW-VALUES             ELTMCN  
01194          PERFORM TRANSLATE-BC-OPEX-OVERRIDE.                      ELTMCN  
01195      EJECT                                                        ELTMCN  
01196                                                                   ELTMCN  
01197                                                                   ELTMCN  
01198 ************************************************************      ELTMCN  
01199 *                                                          *      ELTMCN  
01200 *        GENERATE REDUCTIONS HEADINGS                      *      ELTMCN  
01201 *                                                          *      ELTMCN  
01202 ************************************************************      ELTMCN  
01203  GENERATE-REDUCTIONS-HEADINGS.                                    ELTMCN  
01204      INITIALIZE WS-PERIOD-SW.                                     ELTMCN  
01205      INITIALIZE ADDITIONAL-TEXT-SW.                               ELTMCN  
01206      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMCN  
01207      MOVE WS-BENEFITS-REDUCTION TO COF-DTL-LINE                   ELTMCN  
01208          (COF-NBR-DTL-LINES).                                     ELTMCN  
01209      PERFORM LINK-TO-OUTPUT.                                      ELTMCN  
01210      EJECT                                                        ELTMCN  
01211                                                                   ELTMCN  
01212                                                                   ELTMCN  
01213 ************************************************************      ELTMCN  
01214 *                                                          *      ELTMCN  
01215 *        GENERATE RELATED TABULAR                          *      ELTMCN  
01216 *                                                          *      ELTMCN  
01217 ************************************************************      ELTMCN  
01218  GENERATE-RELATED-TABULAR.                                        ELTMCN  
01219      SET GCG-INDEX TO 1.                                          ELTMCN  
01220      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTMCN  
01221            AT END                                                 ELTMCN  
01222               MOVE ZEROES TO WS-GMCR-PROC-SLOT-NO                 ELTMCN  
01223            WHEN GCG-TAB-ID (GCG-INDEX) EQUAL PC-GMCR              ELTMCN  
01224               MOVE GCG-TAB-ID (GCG-INDEX) TO                      ELTMCN  
01225          WS-GMCR-PROC-ID                                          ELTMCN  
01226               MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                    ELTMCN  
01227                   TO WS-GMCR-PROC-SLOT-NO                         ELTMCN  
01228         END-SEARCH.                                               ELTMCN  
01229      IF WS-GMCR-PROC-SLOT-NO NOT EQUAL ZEROES                     ELTMCN  
01230                 AND WS-GMCR-PROC-ID EQUAL PC-GMCR                 ELTMCN  
01231          PERFORM DISPLAY-RELATED-PROCEDURES-SEN.                  ELTMCN  
01232                                                                   ELTMCN  
01233                                                                   ELTMCN  
01234 ************************************************************      ELTMCN  
01235 *                                                          *      ELTMCN  
01236 *        DISPLAY RELATED PROCEDURES SENTENCE               *      ELTMCN  
01237 *                                                          *      ELTMCN  
01238 ************************************************************      ELTMCN  
01239  DISPLAY-RELATED-PROCEDURES-SEN.                                  ELTMCN  
01240      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMCN  
01241      MOVE SPECIAL-PROCEDURES-MSG TO                               ELTMCN  
01242          COF-DTL-LINE (COF-NBR-DTL-LINES).                        ELTMCN  
01243      PERFORM LINK-TO-OUTPUT.                                      ELTMCN  
01244      PERFORM GENERATE-RELATED-PROCEDURES-TE.                      ELTMCN  
01245      EJECT                                                        ELTMCN  
01246                                                                   ELTMCN  
01247                                                                   ELTMCN  
01248 ************************************************************      ELTMCN  
01249 *                                                          *      ELTMCN  
01250 *        GENERATE RELATED PROCEDURES TEXT                  *      ELTMCN  
01251 *                                                          *      ELTMCN  
01252 ************************************************************      ELTMCN  
01253  GENERATE-RELATED-PROCEDURES-TE.                                  ELTMCN  
01254      MOVE 'MANAGED CARE NETWORK' TO SRP-CCP-NAME.                 ELTMCN  
01255      MOVE WS-GMCR-PROC-ID TO SRP-TABULAR-ID.                      ELTMCN  
01256      MOVE WS-GMCR-PROC-SLOT-NO TO SRP-TABULAR-SLOT-NO.            ELTMCN  
01257      PERFORM CALL-SPECIAL-PROCEDURES-GENERA.                      ELTMCN  
01258                                                                   ELTMCN  
01259                                                                   ELTMCN  
01260 ************************************************************      ELTMCN  
01261 *                                                          *      ELTMCN  
01262 *        CALL SPECIAL PROCEDURES GENERATOR                 *      ELTMCN  
01263 *                                                          *      ELTMCN  
01264 ************************************************************      ELTMCN  
01265  CALL-SPECIAL-PROCEDURES-GENERA.                                  ELTMCN  
01266      EXEC CICS LINK                                               ELTMCN  
01267                PROGRAM ('ELGGXXR')                                ELTMCN  
01268                COMMAREA (DFHCOMMAREA)                             ELTMCN  
01269         END-EXEC.                                                 ELTMCN  
01270      EJECT                                                        ELTMCN  
01271                                                                   ELTMCN  
01272                                                                   ELTMCN  
01273                                                                   ELTMCN  
01274                                                                   ELTMCN  
01275 ************************************************************      ELTMCN  
01276 *                                                          *      ELTMCN  
01277 *        GENERATE SPECIAL SERVICES TABULAR                 *      ELTMCN  
01278 *                                                          *      ELTMCN  
01279 ************************************************************      ELTMCN  
01280  GENERATE-SPECIAL-TABULAR.                                        ELTMCN  
01281      SET GCG-INDEX TO 1.                                          ELTMCN  
01282      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTMCN  
01283            AT END                                                 ELTMCN  
01284               MOVE ZEROES TO WS-GMCS-SPEC-SLOT-NO                 ELTMCN  
01285            WHEN GCG-TAB-ID (GCG-INDEX) EQUAL PC-GMCS              ELTMCN  
01286               MOVE GCG-TAB-ID (GCG-INDEX) TO                      ELTMCN  
01287          WS-GMCS-SPEC-ID                                          ELTMCN  
01288               MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                    ELTMCN  
01289                   TO WS-GMCS-SPEC-SLOT-NO                         ELTMCN  
01290         END-SEARCH.                                               ELTMCN  
01291      IF WS-GMCS-SPEC-SLOT-NO NOT EQUAL ZEROES                     ELTMCN  
01292                 AND WS-GMCS-SPEC-ID EQUAL PC-GMCS                 ELTMCN  
01293          PERFORM DISPLAY-SPECIAL-SERVICES-SEN.                    ELTMCN  
01294                                                                   ELTMCN  
01295                                                                   ELTMCN  
01296 ************************************************************      ELTMCN  
01297 *                                                          *      ELTMCN  
01298 *        DISPLAY SPECIAL SERVICES SENTENCE SS              *      ELTMCN  
01299 *                                                          *      ELTMCN  
01300 ************************************************************      ELTMCN  
01301  DISPLAY-SPECIAL-SERVICES-SEN.                                    ELTMCN  
01302      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMCN  
01303      MOVE SPECIAL-SERVICES-MSG TO                                 ELTMCN  
01304          COF-DTL-LINE (COF-NBR-DTL-LINES).                        ELTMCN  
01305      PERFORM LINK-TO-OUTPUT.                                      ELTMCN  
01306      PERFORM GENERATE-SPECIAL-SERVICES-TE.                        ELTMCN  
01307      EJECT                                                        ELTMCN  
01308                                                                   ELTMCN  
01309                                                                   ELTMCN  
01310 ************************************************************      ELTMCN  
01311 *                                                          *      ELTMCN  
01312 *        GENERATE SPECIAL SERVICES TEXT                    *      ELTMCN  
01313 *                                                          *      ELTMCN  
01314 ************************************************************      ELTMCN  
01315  GENERATE-SPECIAL-SERVICES-TE.                                    ELTMCN  
01316      MOVE 'SPECIAL SERVICES' TO SRP-CCP-NAME.                     ELTMCN  
01317      MOVE WS-GMCS-SPEC-ID TO SRP-TABULAR-ID.                      ELTMCN  
01318      MOVE WS-GMCS-SPEC-SLOT-NO TO SRP-TABULAR-SLOT-NO.            ELTMCN  
01319      PERFORM CALL-SPECIAL-SERVICES-GENERA.                        ELTMCN  
01320                                                                   ELTMCN  
01321                                                                   ELTMCN  
01322 ************************************************************      ELTMCN  
01323 *                                                          *      ELTMCN  
01324 *        CALL SPECIAL SERVICES GENERATOR                   *      ELTMCN  
01325 *                                                          *      ELTMCN  
01326 ************************************************************      ELTMCN  
01327  CALL-SPECIAL-SERVICES-GENERA.                                    ELTMCN  
01328      EXEC CICS LINK                                               ELTMCN  
01329                PROGRAM ('ELGGXXB')                                ELTMCN  
01330                COMMAREA (DFHCOMMAREA)                             ELTMCN  
01331         END-EXEC.                                                 ELTMCN  
01332      EJECT                                                        ELTMCN  
01333                                                                   ELTMCN  
01334                                                                   ELTMCN  
01335 ************************************************************      ELTMCN  
01336 *                                                          *      ELTMCN  
01337 *        TRANSLATE BS INDICATOR                            *      ELTMCN  
01338 *                                                          *      ELTMCN  
01339 ************************************************************      ELTMCN  
01340  TRANSLATE-BS-INDICATOR.                                          ELTMCN  
01341      PERFORM GET-INDICATOR-FIXED-TEXT.                            ELTMCN  
01342      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMCN  
01343      MOVE 'PS-BS-IND' TO CMF-ELEMENT-SYSTEM-NAME.                 ELTMCN  
01344      MOVE GSS-PS-BS-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTMCN  
01345      PERFORM CALL-TRANSLATOR.                                     ELTMCN  
01346      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMCN  
01347      EJECT                                                        ELTMCN  
01348                                                                   ELTMCN  
01349                                                                   ELTMCN  
01350 ************************************************************      ELTMCN  
01351 *                                                          *      ELTMCN  
01352 *        TRANSLATE BS PAYMENT LEVEL INDICATOR              *      ELTMCN  
01353 *                                                          *      ELTMCN  
01354 ************************************************************      ELTMCN  
01355  TRANSLATE-BS-PAYMENT-LEVEL-IND.                                  ELTMCN  
01356      PERFORM SETUP-PAYMENT-LEVEL-FIXED-TEXT.                      ELTMCN  
01357      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMCN  
01358      MOVE 'PS-BS-PAYMENT-LEVEL-IND' TO                            ELTMCN  
01359          CMF-ELEMENT-SYSTEM-NAME.                                 ELTMCN  
01360      MOVE GSS-PS-BS-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTMCN  
01361          CMF-CODE-VALUE.                                          ELTMCN  
01362      PERFORM CALL-TRANSLATOR.                                     ELTMCN  
01363      PERFORM MOVE-TRANSLATED-TEXT-TO-OUTPUT.                      ELTMCN  
01364      SET PAYMENT-LVL-TRANSLATED TO TRUE.                          ELTMCN  
01365      EJECT                                                        ELTMCN  
01366                                                                   ELTMCN  
01367                                                                   ELTMCN  
01368 ************************************************************      ELTMCN  
01369 *                                                          *      ELTMCN  
01370 *        GENERATE BS BENEFITS REDUCTION SENTENCE           *      ELTMCN  
01371 *                                                          *      ELTMCN  
01372 ************************************************************      ELTMCN  
01373  GENERATE-BS-BENEFITS-REDUCTION.                                  ELTMCN  
01374      IF GSS-PS-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTMCN  
01375                  ZERO AND SPACES AND LOW-VALUES                   ELTMCN  
01376          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTMCN  
01377      IF (GSS-PS-BS-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL          ELTMCN  
01378          ZERO                                                     ELTMCN  
01379                             AND SPACES AND LOW-VALUES)  OR        ELTMCN  
01380               (GSS-PS-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL    ELTMCN  
01381          ZERO                                                     ELTMCN  
01382                             AND SPACES AND LOW-VALUES)  OR        ELTMCN  
01383                (GSS-PS-BS-OPEX-OVERRIDE-IND (GSS-INDEX) NOT       ELTMCN  
01384          EQUAL ZERO                                               ELTMCN  
01385                             AND SPACES AND LOW-VALUES)            ELTMCN  
01386          PERFORM GENERATE-BS-BENEFIT-REDUCTIONX.                  ELTMCN  
01387      EJECT                                                        ELTMCN  
01388                                                                   ELTMCN  
01389                                                                   ELTMCN  
01390 ************************************************************      ELTMCN  
01391 *                                                          *      ELTMCN  
01392 *        GENERATE BS BENEFIT REDUCTION TEXT                *      ELTMCN  
01393 *                                                          *      ELTMCN  
01394 ************************************************************      ELTMCN  
01395  GENERATE-BS-BENEFIT-REDUCTIONX.                                  ELTMCN  
01396      PERFORM GENERATE-REDUCTIONS-HEADINGS.                        ELTMCN  
01397      IF GSS-PS-BS-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMCN  
01398          ZERO                                                     ELTMCN  
01399                             AND SPACES AND LOW-VALUES             ELTMCN  
01400          PERFORM TRANSLATE-BS-DEDU-APPLIC.                        ELTMCN  
01401      IF GSS-PS-BS-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMCN  
01402          ZERO                                                     ELTMCN  
01403                             AND SPACES AND LOW-VALUES             ELTMCN  
01404          PERFORM TRANSLATE-BS-OPEX-APPLIC.                        ELTMCN  
01405      PERFORM CREATE-A-BLANK-LINE.                                 ELTMCN  
01406      PERFORM LINK-TO-OUTPUT.                                      ELTMCN  
01407      IF GSS-PS-BS-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTMCN  
01408          ZERO                                                     ELTMCN  
01409                             AND SPACES AND LOW-VALUES             ELTMCN  
01410          PERFORM TRANSLATE-BS-OPEX-OVERRIDE.                      ELTMCN  
01411      EJECT                                                        ELTMCN  
01412                                                                   ELTMCN  
01413                                                                   ELTMCN  
01414 ************************************************************      ELTMCN  
01415 *                                                          *      ELTMCN  
01416 *        TRANSLATE MM INDICATOR                            *      ELTMCN  
01417 *                                                          *      ELTMCN  
01418 ************************************************************      ELTMCN  
01419  TRANSLATE-MM-INDICATOR.                                          ELTMCN  
01420      PERFORM GET-INDICATOR-FIXED-TEXT.                            ELTMCN  
01421      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMCN  
01422      MOVE 'PS-MM-IND'  TO CMF-ELEMENT-SYSTEM-NAME.                ELTMCN  
01423      MOVE GSS-PS-MM-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTMCN  
01424      PERFORM CALL-TRANSLATOR.                                     ELTMCN  
01425      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMCN  
01426      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMCN  
01427      EJECT                                                        ELTMCN  
01428                                                                   ELTMCN  
01429                                                                   ELTMCN  
01430 ************************************************************      ELTMCN  
01431 *                                                          *      ELTMCN  
01432 *        TRANSLATE MM PAYMENT LEVEL INDICATOR              *      ELTMCN  
01433 *                                                          *      ELTMCN  
01434 ************************************************************      ELTMCN  
01435  TRANSLATE-MM-PAYMENT-LEVEL-IND.                                  ELTMCN  
01436      PERFORM SETUP-PAYMENT-LEVEL-FIXED-TEXT.                      ELTMCN  
01437      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMCN  
01438      MOVE 'PS-MM-PAYMENT-LEVEL-IND' TO                            ELTMCN  
01439          CMF-ELEMENT-SYSTEM-NAME.                                 ELTMCN  
01440      MOVE GSS-PS-MM-PAYMENT-LEVEL-IND (GSS-INDEX)                 ELTMCN  
01441                                   TO CMF-CODE-VALUE.              ELTMCN  
01442      PERFORM CALL-TRANSLATOR.                                     ELTMCN  
01443      PERFORM MOVE-TRANSLATED-TEXT-TO-OUTPUT.                      ELTMCN  
01444      SET PAYMENT-LVL-TRANSLATED TO TRUE.                          ELTMCN  
01445      EJECT                                                        ELTMCN  
01446                                                                   ELTMCN  
01447                                                                   ELTMCN  
01448 ************************************************************      ELTMCN  
01449 *                                                          *      ELTMCN  
01450 *        GENERATE MM BENEFITS REDUCTION SENTENCE           *      ELTMCN  
01451 *                                                          *      ELTMCN  
01452 ************************************************************      ELTMCN  
01453  GENERATE-MM-BENEFITS-REDUCTION.                                  ELTMCN  
01454      IF GSS-PS-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTMCN  
01455                  ZERO AND SPACES AND LOW-VALUES                   ELTMCN  
01456          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTMCN  
01457      IF (GSS-PS-MM-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL          ELTMCN  
01458          ZERO                                                     ELTMCN  
01459                             AND SPACES AND LOW-VALUES)  OR        ELTMCN  
01460                (GSS-PS-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL   ELTMCN  
01461          ZERO                                                     ELTMCN  
01462                             AND SPACES AND LOW-VALUES)  OR        ELTMCN  
01463                (GSS-PS-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT       ELTMCN  
01464          EQUAL ZERO                                               ELTMCN  
01465                             AND SPACES AND LOW-VALUES)            ELTMCN  
01466          PERFORM GENERATE-MM-BENEFIT-REDUCTIONX.                  ELTMCN  
01467      EJECT                                                        ELTMCN  
01468                                                                   ELTMCN  
01469                                                                   ELTMCN  
01470 ************************************************************      ELTMCN  
01471 *                                                          *      ELTMCN  
01472 *        GENERATE MM BENEFIT REDUCTION TEXT                *      ELTMCN  
01473 *                                                          *      ELTMCN  
01474 ************************************************************      ELTMCN  
01475  GENERATE-MM-BENEFIT-REDUCTIONX.                                  ELTMCN  
01476      PERFORM GENERATE-REDUCTIONS-HEADINGS.                        ELTMCN  
01477      IF GSS-PS-MM-DEDU-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMCN  
01478          ZERO                                                     ELTMCN  
01479                             AND SPACES AND LOW-VALUES             ELTMCN  
01480          PERFORM TRANSLATE-MM-DEDU-APPLIC.                        ELTMCN  
01481      IF GSS-PS-MM-OPEX-APPLIC-IND (GSS-INDEX) NOT EQUAL           ELTMCN  
01482          ZERO                                                     ELTMCN  
01483                             AND SPACES AND LOW-VALUES             ELTMCN  
01484          PERFORM TRANSLATE-MM-OPEX-APPLIC.                        ELTMCN  
01485      PERFORM CREATE-A-BLANK-LINE.                                 ELTMCN  
01486      PERFORM LINK-TO-OUTPUT.                                      ELTMCN  
01487      IF GSS-PS-MM-OPEX-OVERRIDE-IND (GSS-INDEX) NOT EQUAL         ELTMCN  
01488          ZERO                                                     ELTMCN  
01489                             AND SPACES AND LOW-VALUES             ELTMCN  
01490          PERFORM TRANSLATE-MM-OPEX-OVERRIDE.                      ELTMCN  
01491      EJECT                                                        ELTMCN  
01492                                                                   ELTMCN  
01493                                                                   ELTMCN  
01494 ************************************************************      ELTMCN  
01495 *                                                          *      ELTMCN  
01496 *        TRANSLATE BC DEDU APPLIC                          *      ELTMCN  
01497 *                                                          *      ELTMCN  
01498 ************************************************************      ELTMCN  
01499  TRANSLATE-BC-DEDU-APPLIC.                                        ELTMCN  
01500      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMCN  
01501      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMCN  
01502      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMCN  
01503      MOVE  'PS-BC-DEDU-APPLIC-IND' TO                             ELTMCN  
01504          CMF-ELEMENT-SYSTEM-NAME.                                 ELTMCN  
01505      MOVE GSS-PS-BC-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTMCN  
01506          CMF-CODE-VALUE.                                          ELTMCN  
01507      PERFORM CALL-TRANSLATOR.                                     ELTMCN  
01508      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMCN  
01509      EJECT                                                        ELTMCN  
01510                                                                   ELTMCN  
01511                                                                   ELTMCN  
01512 ************************************************************      ELTMCN  
01513 *                                                          *      ELTMCN  
01514 *        TRANSLATE BC OPEX APPLIC                          *      ELTMCN  
01515 *                                                          *      ELTMCN  
01516 ************************************************************      ELTMCN  
01517  TRANSLATE-BC-OPEX-APPLIC.                                        ELTMCN  
01518      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMCN  
01519      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMCN  
01520      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMCN  
01521      MOVE 'PS-BC-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMCN  
01522      MOVE GSS-PS-BC-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTMCN  
01523          CMF-CODE-VALUE.                                          ELTMCN  
01524      PERFORM CALL-TRANSLATOR.                                     ELTMCN  
01525      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMCN  
01526      EJECT                                                        ELTMCN  
01527                                                                   ELTMCN  
01528                                                                   ELTMCN  
01529 ************************************************************      ELTMCN  
01530 *                                                          *      ELTMCN  
01531 *        TRANSLATE BC OPEX OVERRIDE                        *      ELTMCN  
01532 *                                                          *      ELTMCN  
01533 ************************************************************      ELTMCN  
01534  TRANSLATE-BC-OPEX-OVERRIDE.                                      ELTMCN  
01535      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMCN  
01536      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMCN  
01537      SET PERIOD-NEEDED TO TRUE.                                   ELTMCN  
01538      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMCN  
01539      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMCN  
01540      MOVE 'PS-BC-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTMCN  
01541      MOVE GSS-PS-BC-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTMCN  
01542          CMF-CODE-VALUE.                                          ELTMCN  
01543      PERFORM CALL-TRANSLATOR.                                     ELTMCN  
01544      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMCN  
01545      EJECT                                                        ELTMCN  
01546                                                                   ELTMCN  
01547                                                                   ELTMCN  
01548 ************************************************************      ELTMCN  
01549 *                                                          *      ELTMCN  
01550 *        TRANSLATE BS DEDU APPLIC                          *      ELTMCN  
01551 *                                                          *      ELTMCN  
01552 ************************************************************      ELTMCN  
01553  TRANSLATE-BS-DEDU-APPLIC.                                        ELTMCN  
01554      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMCN  
01555      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMCN  
01556      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMCN  
01557      MOVE 'PS-BS-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMCN  
01558      MOVE GSS-PS-BS-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTMCN  
01559          CMF-CODE-VALUE.                                          ELTMCN  
01560      PERFORM CALL-TRANSLATOR.                                     ELTMCN  
01561      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMCN  
01562      EJECT                                                        ELTMCN  
01563                                                                   ELTMCN  
01564                                                                   ELTMCN  
01565 ************************************************************      ELTMCN  
01566 *                                                          *      ELTMCN  
01567 *        TRANSLATE BS OPEX APPLIC                          *      ELTMCN  
01568 *                                                          *      ELTMCN  
01569 ************************************************************      ELTMCN  
01570  TRANSLATE-BS-OPEX-APPLIC.                                        ELTMCN  
01571      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMCN  
01572      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMCN  
01573      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMCN  
01574      MOVE 'PS-BS-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMCN  
01575      MOVE GSS-PS-BS-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTMCN  
01576          CMF-CODE-VALUE.                                          ELTMCN  
01577      PERFORM CALL-TRANSLATOR.                                     ELTMCN  
01578      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMCN  
01579      EJECT                                                        ELTMCN  
01580                                                                   ELTMCN  
01581                                                                   ELTMCN  
01582 ************************************************************      ELTMCN  
01583 *                                                          *      ELTMCN  
01584 *        TRANSLATE BS OPEX OVERRIDE                        *      ELTMCN  
01585 *                                                          *      ELTMCN  
01586 ************************************************************      ELTMCN  
01587  TRANSLATE-BS-OPEX-OVERRIDE.                                      ELTMCN  
01588      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMCN  
01589      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMCN  
01590      SET PERIOD-NEEDED TO TRUE.                                   ELTMCN  
01591      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMCN  
01592      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMCN  
01593      MOVE 'PS-BS-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTMCN  
01594      MOVE GSS-PS-BS-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTMCN  
01595          CMF-CODE-VALUE.                                          ELTMCN  
01596      PERFORM CALL-TRANSLATOR.                                     ELTMCN  
01597      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMCN  
01598      EJECT                                                        ELTMCN  
01599                                                                   ELTMCN  
01600                                                                   ELTMCN  
01601 ************************************************************      ELTMCN  
01602 *                                                          *      ELTMCN  
01603 *        TRANSLATE MM DEDU APPLIC                          *      ELTMCN  
01604 *                                                          *      ELTMCN  
01605 ************************************************************      ELTMCN  
01606  TRANSLATE-MM-DEDU-APPLIC.                                        ELTMCN  
01607      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMCN  
01608      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMCN  
01609      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMCN  
01610      MOVE 'PS-MM-DEDU-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMCN  
01611      MOVE GSS-PS-MM-DEDU-APPLIC-IND (GSS-INDEX) TO                ELTMCN  
01612          CMF-CODE-VALUE.                                          ELTMCN  
01613      PERFORM CALL-TRANSLATOR.                                     ELTMCN  
01614      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMCN  
01615      EJECT                                                        ELTMCN  
01616                                                                   ELTMCN  
01617                                                                   ELTMCN  
01618 ************************************************************      ELTMCN  
01619 *                                                          *      ELTMCN  
01620 *        TRANSLATE MM OPEX APPLIC                          *      ELTMCN  
01621 *                                                          *      ELTMCN  
01622 ************************************************************      ELTMCN  
01623  TRANSLATE-MM-OPEX-APPLIC.                                        ELTMCN  
01624      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMCN  
01625      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMCN  
01626      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMCN  
01627      MOVE 'PS-MM-OPEX-APPLIC-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTMCN  
01628      MOVE GSS-PS-MM-OPEX-APPLIC-IND (GSS-INDEX) TO                ELTMCN  
01629          CMF-CODE-VALUE.                                          ELTMCN  
01630      PERFORM CALL-TRANSLATOR.                                     ELTMCN  
01631      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMCN  
01632      EJECT                                                        ELTMCN  
01633                                                                   ELTMCN  
01634                                                                   ELTMCN  
01635 ************************************************************      ELTMCN  
01636 *                                                          *      ELTMCN  
01637 *        TRANSLATE MM OPEX OVERRIDE                        *      ELTMCN  
01638 *                                                          *      ELTMCN  
01639 ************************************************************      ELTMCN  
01640  TRANSLATE-MM-OPEX-OVERRIDE.                                      ELTMCN  
01641      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMCN  
01642      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMCN  
01643      SET PERIOD-NEEDED TO TRUE.                                   ELTMCN  
01644      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMCN  
01645      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMCN  
01646      MOVE 'PS-MM-OPEX-OVERRIDE-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTMCN  
01647      MOVE GSS-PS-MM-OPEX-OVERRIDE-IND (GSS-INDEX) TO              ELTMCN  
01648          CMF-CODE-VALUE.                                          ELTMCN  
01649      PERFORM CALL-TRANSLATOR.                                     ELTMCN  
01650      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMCN  
01651      EJECT                                                        ELTMCN  
01652                                                                   ELTMCN  
01653                                                                   ELTMCN  
01654 ************************************************************      ELTMCN  
01655 *                                                          *      ELTMCN  
01656 *        GENERATE SPILL OVER INDICATOR                     *      ELTMCN  
01657 *                                                          *      ELTMCN  
01658 ************************************************************      ELTMCN  
01659  GENERATE-SPILL-OVER-INDICATOR.                                   ELTMCN  
01660      INITIALIZE ADDITIONAL-TEXT-SW.                               ELTMCN  
01661      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMCN  
01662      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMCN  
01663      SET PERIOD-NEEDED TO TRUE.                                   ELTMCN  
01664      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMCN  
01665      MOVE WS-SPILLOVER-SENTENCE TO TCAR-FROM-LINE                 ELTMCN  
01666          (TCAR-FROM-SUB).                                         ELTMCN  
01667      ADD 1 TO TCAR-FROM-SUB.                                      ELTMCN  
01668      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMCN  
01669      MOVE 'PS-SPILL-OVER-IND' TO CMF-ELEMENT-SYSTEM-NAME.         ELTMCN  
01670      MOVE GSS-PS-SPILL-OVER-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMCN  
01671      PERFORM CALL-TRANSLATOR.                                     ELTMCN  
01672      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMCN  
01673                                                                   ELTMCN  
01674                                                                   ELTMCN  
01675 ************************************************************      ELTMCN  
01676 *                                                          *      ELTMCN  
01677 *        SIGNAL NOT APPLICABLE MSG                         *      ELTMCN  
01678 *                                                          *      ELTMCN  
01679 ************************************************************      ELTMCN  
01680  SIGNAL-NOT-APPLICABLE-MSG.                                       ELTMCN  
01681      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMCN  
01682      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTMCN  
01683          (COF-NBR-DTL-LINES).                                     ELTMCN  
01684                                                                   ELTMCN  
01685                                                                   ELTMCN  
01686 ************************************************************      ELTMCN  
01687 *                                                          *      ELTMCN  
01688 *        CALL TRANSLATOR                                   *      ELTMCN  
01689 *                                                          *      ELTMCN  
01690 ************************************************************      ELTMCN  
01691  CALL-TRANSLATOR.                                                 ELTMCN  
01692      EXEC CICS LINK                                               ELTMCN  
01693           PROGRAM ('ELUCMIF')                                     ELTMCN  
01694           COMMAREA (DFHCOMMAREA)                                  ELTMCN  
01695           END-EXEC.                                               ELTMCN  
01696      EJECT                                                        ELTMCN  
01697                                                                   ELTMCN  
01698                                                                   ELTMCN  
01699 ************************************************************      ELTMCN  
01700 *                                                          *      ELTMCN  
01701 *        READ GCCP RECORD                                  *      ELTMCN  
01702 *                                                          *      ELTMCN  
01703 ************************************************************      ELTMCN  
01704  READ-GCCP-RECORD.                                                ELTMCN  
01705      SET CIA-GCTABULR-DDN TO TRUE.                                ELTMCN  
01706      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMCN  
01707           ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                 ELTMCN  
01708      SET IOP-RD TO TRUE.                                          ELTMCN  
01709      SET IOP-FCQ-NONE TO TRUE.                                    ELTMCN  
01710      SET IOP-KVQ-EQ TO TRUE.                                      ELTMCN  
01711      SET IOP-STG-MODE-MOVE TO TRUE.                               ELTMCN  
01712      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELTMCN  
01713      PERFORM CALL-I-O-PGM.                                        ELTMCN  
01714      EJECT                                                        ELTMCN  
01715                                                                   ELTMCN  
01716                                                                   ELTMCN  
01717 ************************************************************      ELTMCN  
01718 *                                                          *      ELTMCN  
01719 *        CALL I O PGM                                      *      ELTMCN  
01720 *                                                          *      ELTMCN  
01721 ************************************************************      ELTMCN  
01722  CALL-I-O-PGM.                                                    ELTMCN  
01723      EXEC CICS LINK                                               ELTMCN  
01724           PROGRAM ('ELUIOPGM')                                    ELTMCN  
01725           COMMAREA (DFHCOMMAREA)                                  ELTMCN  
01726           END-EXEC.                                               ELTMCN  
01727      IF IOP-RC-OK                                                 ELTMCN  
01728          PERFORM ESTABLISH-ADDRESSABILITY-OF-GC                   ELTMCN  
01729      ELSE IF IOP-RC-NOTFND                                        ELTMCN  
01730          PERFORM SIGNAL-NOT-FOUND-GCTAB                           ELTMCN  
01731      ELSE                                                         ELTMCN  
01732          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTMCN  
01733      EJECT                                                        ELTMCN  
01734                                                                   ELTMCN  
01735                                                                   ELTMCN  
01736 ************************************************************      ELTMCN  
01737 *                                                          *      ELTMCN  
01738 *        SIGNAL CRITICAL IO ERROR                          *      ELTMCN  
01739 *                                                          *      ELTMCN  
01740 ************************************************************      ELTMCN  
01741  SIGNAL-CRITICAL-IO-ERROR.                                        ELTMCN  
01742      SET CIA-AB-CRITIO TO TRUE.                                   ELTMCN  
01743      PERFORM SIGNAL-ABEND.                                        ELTMCN  
01744                                                                   ELTMCN  
01745                                                                   ELTMCN  
01746 ************************************************************      ELTMCN  
01747 *                                                          *      ELTMCN  
01748 *        SIGNAL NOT FOUND GCTAB                            *      ELTMCN  
01749 *                                                          *      ELTMCN  
01750 ************************************************************      ELTMCN  
01751  SIGNAL-NOT-FOUND-GCTAB.                                          ELTMCN  
01752      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTMCN  
01753      PERFORM SIGNAL-ABEND.                                        ELTMCN  
01754                                                                   ELTMCN  
01755                                                                   ELTMCN  
01756 ************************************************************      ELTMCN  
01757 *                                                          *      ELTMCN  
01758 *        ESTABLISH ADDRESSABILITY OF GCCP RECORD           *      ELTMCN  
01759 *                                                          *      ELTMCN  
01760 ************************************************************      ELTMCN  
01761  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTMCN  
01762      SET ADDRESS OF GCCP-TABULAR-REC TO IOP-REC-PTR.              ELTMCN  
01763      SET IOP-REC-PTR TO NULL.                                     ELTMCN  
01764                                                                   ELTMCN  
01765                                                                   ELTMCN  
01766 ************************************************************      ELTMCN  
01767 *                                                          *      ELTMCN  
01768 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTMCN  
01769 *                                                          *      ELTMCN  
01770 ************************************************************      ELTMCN  
01771  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTMCN  
01772      PERFORM INITIALIZE-CMOUT.                                    ELTMCN  
01773      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTMCN  
01774      EJECT                                                        ELTMCN  
01775                                                                   ELTMCN  
01776                                                                   ELTMCN  
01777 ************************************************************      ELTMCN  
01778 *                                                          *      ELTMCN  
01779 *        PREPARE TEXT FOR OUTPUT                           *      ELTMCN  
01780 *                                                          *      ELTMCN  
01781 ************************************************************      ELTMCN  
01782  PREPARE-TEXT-FOR-OUTPUT.                                         ELTMCN  
01783      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTMCN  
01784          UNTIL CMF-DESCR-IDX                                      ELTMCN  
01785                                    GREATER THAN                   ELTMCN  
01786              CMF-NBR-DESCR-LINES.                                 ELTMCN  
01787      EJECT                                                        ELTMCN  
01788                                                                   ELTMCN  
01789                                                                   ELTMCN  
01790 ************************************************************      ELTMCN  
01791 *                                                          *      ELTMCN  
01792 *        INITIALIZE CMOUT                                  *      ELTMCN  
01793 *                                                          *      ELTMCN  
01794 ************************************************************      ELTMCN  
01795  INITIALIZE-CMOUT.                                                ELTMCN  
01796      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTMCN  
01797      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMCN  
01798          ADDRESS OF CMF-DESCR.                                    ELTMCN  
01799      SET CMF-DESCR-IDX TO 1.                                      ELTMCN  
01800      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTMCN  
01801                                                                   ELTMCN  
01802                                                                   ELTMCN  
01803 ************************************************************      ELTMCN  
01804 *                                                          *      ELTMCN  
01805 *        MOVE CMF TEXT TO OUTPUT                           *      ELTMCN  
01806 *                                                          *      ELTMCN  
01807 ************************************************************      ELTMCN  
01808  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTMCN  
01809      PERFORM MOVE-A-LINE.                                         ELTMCN  
01810      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTMCN  
01811          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTMCN  
01812      IF TCAR-FROM-SUB GREATER THAN 20                             ELTMCN  
01813               OR CMF-DESCR-IDX GREATER THAN                       ELTMCN  
01814          CMF-NBR-DESCR-LINES                                      ELTMCN  
01815          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTMCN  
01816                                                                   ELTMCN  
01817                                                                   ELTMCN  
01818 ************************************************************      ELTMCN  
01819 *                                                          *      ELTMCN  
01820 *        FINISH CODES MANUAL TEXT                          *      ELTMCN  
01821 *                                                          *      ELTMCN  
01822 ************************************************************      ELTMCN  
01823  FINISH-CODES-MANUAL-TEXT.                                        ELTMCN  
01824      SET DONE-PROCESSING TO TRUE.                                 ELTMCN  
01825      IF PERIOD-NEEDED                                             ELTMCN  
01826          PERFORM GET-AND-MOVE-PERIOD.                             ELTMCN  
01827                                                                   ELTMCN  
01828                                                                   ELTMCN  
01829 ************************************************************      ELTMCN  
01830 *                                                          *      ELTMCN  
01831 *        GET AND MOVE PERIOD                               *      ELTMCN  
01832 *                                                          *      ELTMCN  
01833 ************************************************************      ELTMCN  
01834  GET-AND-MOVE-PERIOD.                                             ELTMCN  
01835      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTMCN  
01836          (TCAR-FROM-SUB).                                         ELTMCN  
01837                                                                   ELTMCN  
01838                                                                   ELTMCN  
01839 ************************************************************      ELTMCN  
01840 *                                                          *      ELTMCN  
01841 *        SAVE LAST LINE                                    *      ELTMCN  
01842 *                                                          *      ELTMCN  
01843 ************************************************************      ELTMCN  
01844  SAVE-LAST-LINE.                                                  ELTMCN  
01845      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMCN  
01846      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTMCN  
01847         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTMCN  
01848      ADD 1 TO TCAR-FROM-SUB.                                      ELTMCN  
01849      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTMCN  
01850                                                                   ELTMCN  
01851                                                                   ELTMCN  
01852 ************************************************************      ELTMCN  
01853 *                                                          *      ELTMCN  
01854 *        OUTPUT LAST LINE                                  *      ELTMCN  
01855 *                                                          *      ELTMCN  
01856 ************************************************************      ELTMCN  
01857  OUTPUT-LAST-LINE.                                                ELTMCN  
01858      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTMCN  
01859          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTMCN  
01860      IF BLANK-LINE-NEEDED                                         ELTMCN  
01861          PERFORM CREATE-A-BLANK-LINE.                             ELTMCN  
01862                                                                   ELTMCN  
01863                                                                   ELTMCN  
01864 ************************************************************      ELTMCN  
01865 *                                                          *      ELTMCN  
01866 *        CREATE A BLANK LINE                               *      ELTMCN  
01867 *                                                          *      ELTMCN  
01868 ************************************************************      ELTMCN  
01869  CREATE-A-BLANK-LINE.                                             ELTMCN  
01870      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTMCN  
01871      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMCN  
01872                                                                   ELTMCN  
01873                                                                   ELTMCN  
01874 ************************************************************      ELTMCN  
01875 *                                                          *      ELTMCN  
01876 *        MOVE A LINE                                       *      ELTMCN  
01877 *                                                          *      ELTMCN  
01878 ************************************************************      ELTMCN  
01879  MOVE-A-LINE.                                                     ELTMCN  
01880      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTMCN  
01881          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTMCN  
01882      SET CMF-DESCR-IDX UP BY 1.                                   ELTMCN  
01883      ADD 1 TO TCAR-FROM-SUB.                                      ELTMCN  
01884      EJECT                                                        ELTMCN  
01885                                                                   ELTMCN  
01886                                                                   ELTMCN  
01887 ************************************************************      ELTMCN  
01888 *                                                          *      ELTMCN  
01889 *        REFORMAT AND WRITE TEXT                           *      ELTMCN  
01890 *                                                          *      ELTMCN  
01891 ************************************************************      ELTMCN  
01892  REFORMAT-AND-WRITE-TEXT.                                         ELTMCN  
01893      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTMCN  
01894      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTMCN  
01895      PERFORM UNSTRING-TEXT.                                       ELTMCN  
01896      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMCN  
01897      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMCN  
01898      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTMCN  
01899          UNTIL COF-NBR-DTL-LINES GREATER                          ELTMCN  
01900                                   TCAR-OUTPUT-FIELDS-USED -       ELTMCN  
01901              1.                                                   ELTMCN  
01902      PERFORM DISPOSE-OF-LAST-LINE.                                ELTMCN  
01903      PERFORM LINK-TO-OUTPUT.                                      ELTMCN  
01904                                                                   ELTMCN  
01905                                                                   ELTMCN  
01906 ************************************************************      ELTMCN  
01907 *                                                          *      ELTMCN  
01908 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTMCN  
01909 *                                                          *      ELTMCN  
01910 ************************************************************      ELTMCN  
01911  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTMCN  
01912      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTMCN  
01913           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTMCN  
01914      ADD +1 TO TCAR-FROM-SUB.                                     ELTMCN  
01915      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMCN  
01916      EJECT                                                        ELTMCN  
01917                                                                   ELTMCN  
01918                                                                   ELTMCN  
01919 ************************************************************      ELTMCN  
01920 *                                                          *      ELTMCN  
01921 *        UNSTRING TEXT                                     *      ELTMCN  
01922 *                                                          *      ELTMCN  
01923 ************************************************************      ELTMCN  
01924  UNSTRING-TEXT.                                                   ELTMCN  
01925      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTMCN  
01926      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTMCN  
01927      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTMCN  
01928      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTMCN  
01929      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTMCN  
01930      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTMCN  
01931      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTMCN  
01932      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTMCN  
01933      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTMCN  
01934      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTMCN  
01935      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTMCN  
01936      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTMCN  
01937      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTMCN  
01938      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTMCN  
01939      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTMCN  
01940      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTMCN  
01941      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTMCN  
01942      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTMCN  
01943      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTMCN  
01944      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTMCN  
01945      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTMCN  
01946      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTMCN  
01947      EJECT                                                        ELTMCN  
01948                                                                   ELTMCN  
01949                                                                   ELTMCN  
01950 ************************************************************      ELTMCN  
01951 *                                                          *      ELTMCN  
01952 *        LINK TO OUTPUT                                    *      ELTMCN  
01953 *                                                          *      ELTMCN  
01954 ************************************************************      ELTMCN  
01955  LINK-TO-OUTPUT.                                                  ELTMCN  
01956      EXEC CICS LINK                                               ELTMCN  
01957          PROGRAM ('ELUOUTPT')                                     ELTMCN  
01958          COMMAREA (DFHCOMMAREA)                                   ELTMCN  
01959          END-EXEC.                                                ELTMCN  
01960      EJECT                                                        ELTMCN  
01961                                                                   ELTMCN  
01962                                                                   ELTMCN  
01963 ************************************************************      ELTMCN  
01964 *                                                          *      ELTMCN  
01965 *        DISPOSE OF LAST LINE                              *      ELTMCN  
01966 *                                                          *      ELTMCN  
01967 ************************************************************      ELTMCN  
01968  DISPOSE-OF-LAST-LINE.                                            ELTMCN  
01969      IF NOT ADDITIONAL-TEXT                                       ELTMCN  
01970          PERFORM INITIALIZE-CONTINUED-SW.                         ELTMCN  
01971      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTMCN  
01972          PERFORM SAVE-LAST-LINE                                   ELTMCN  
01973      ELSE                                                         ELTMCN  
01974          PERFORM OUTPUT-LAST-LINE.                                ELTMCN  
01975                                                                   ELTMCN  
01976                                                                   ELTMCN  
01977 ************************************************************      ELTMCN  
01978 *                                                          *      ELTMCN  
01979 *        INITIALIZE CONTINUED SW                           *      ELTMCN  
01980 *                                                          *      ELTMCN  
01981 ************************************************************      ELTMCN  
01982  INITIALIZE-CONTINUED-SW.                                         ELTMCN  
01983      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTMCN  
