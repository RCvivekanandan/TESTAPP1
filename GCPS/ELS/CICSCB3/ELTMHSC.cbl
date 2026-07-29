00001 *      LAST MAINTENANCE TIME: 13.58.43  DATE: 06/27/91            06/29/02
00002  IDENTIFICATION DIVISION.                                         ELTMHSC 
00003                                                                      LV001
00004  PROGRAM-ID.         ELTMHSC.                                     ELTMHSC 
00005                                                                   ELTMHSC 
00006  AUTHOR.             ANNE KEFFER KING.                            ELTMHSC 
00007                                                                   ELTMHSC 
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTMHSC 
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELTMHSC 
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTMHSC 
00011                      233 N. MICHIGAN AVE                          ELTMHSC 
00012                      CHICAGO, ILLINOIS 60601                      ELTMHSC 
00013                                                                   ELTMHSC 
00014  DATE-WRITTEN.       28-MAY-1991.                                 ELTMHSC 
00015                                                                   ELTMHSC 
00016  DATE-COMPILED.                                                   ELTMHSC 
00017                                                                   ELTMHSC 
00018  SECURITY.           COPYRIGHT 1986,                              ELTMHSC 
00019                      HEALTH CARE SERVICE CORPORATION              ELTMHSC 
00020                                                                   ELTMHSC 
00021  ENVIRONMENT DIVISION.                                            ELTMHSC 
00022                                                                   ELTMHSC 
00023  CONFIGURATION SECTION.                                           ELTMHSC 
00024  SOURCE-COMPUTER.    IBM-3090.                                    ELTMHSC 
00025  OBJECT-COMPUTER.    IBM-3090.                                    ELTMHSC 
00026      EJECT                                                        ELTMHSC 
00027                                                                   ELTMHSC 
00028 ******************************************************************ELTMHSC 
00029 *                       MAINTENANCE HISTORY                      *ELTMHSC 
00030 *                                                                *ELTMHSC 
00031 * CHANGE ISSR     DATE     BY                ACTION              *ELTMHSC 
00032 * ------ ----- ----------- --- --------------------------------- *ELTMHSC 
00033 * 01.00        10-SEP-1990 AKK CREATED                           *ELTMHSC 
00034 *                                                                *ELTMHSC 
00035 * 01.01        13-OCT-1992 JPB ADDED LOGIC TO HANDLE A VOLUNTARY *ELTMHSC 
00036 *                              GCG-NEW-MEN-SUB-ABUSE-IND (CODE 8)*ELTMHSC 
00037 *                                                                *ELTMHSC 
00038 * 01.02        19-OCT-1992 JPB MOVED \
00039 *                              CHECKED IN IF XXX NOT EQUAL       *ELTMHSC 
00040 *                               LOW-VALUES AND SPACES AND ZERO.  *ELTMHSC 
00041 *                               CHANGED LINK TO ELGCBRI TO CALL. *ELTMHSC 
00042 *                                                                *ELTMHSC 
00043 * 01.03        09-FEB-1994 JPB ADDED TRANSLATION AND DISPLAY OF  *ELTMHSC 
00044 *                              NETWORK UTILIZATION REVIEW IND-   *ELTMHSC 
00045 *                              ICATOR.                           *ELTMHSC 
00046 *                                                                *ELTMHSC 
00047 ******************************************************************ELTMHSC 
00048  DATA DIVISION.                                                   ELTMHSC 
00049 /                                                                 ELTMHSC 
00050 *                                                                 ELTMHSC 
00051  WORKING-STORAGE SECTION.                                         ELTMHSC 
00052  01  WS-BEGIN                            PIC X(24) VALUE          ELTMHSC 
00053                                '** ELTMHSC WS BEGINS **'.         ELTMHSC 
00054  01  WS-MISC-FIELDS.                                              ELTMHSC 
00055      05  WS-CMF-SUB                    PIC S9(04) COMP.           ELTMHSC 
00056      05  SCREEN-TYPE                   PIC X  VALUE SPACES.       ELTMHSC 
00057          88  INSTITUTIONAL-SCREEN             VALUE 'I'.          ELTMHSC 
00058          88  PROFESSIONAL-SCREEN              VALUE 'P'.          ELTMHSC 
00059          88  SUPPLEMENTAL-SCREEN              VALUE 'S'.          ELTMHSC 
00060 *                                                                 ELTMHSC 
00061  01  PROGRAM-CONSTANTS.                                           ELTMHSC 
00062      05  PC-GCCP                 PIC X(06)  VALUE '#GCCP '.       ELTMHSC 
00063      05  PC-GROUP                PIC X(06)  VALUE 'GROUP '.       ELTMHSC 
00064 *                                                                 ELTMHSC 
00065  01  WS-SWITCHES.                                                 ELTMHSC 
00066      05  UNDEFINED-TABULAR-SW     PIC X      VALUE 'N'.           ELTMHSC 
00067          88 TABULAR-IS-UNDEFINED             VALUE 'Y'.           ELTMHSC 
00068      05  DEFINED-TABULAR-SW       PIC X      VALUE 'N'.           ELTMHSC 
00069          88 TABULAR-IS-DEFINED               VALUE 'Y'.           ELTMHSC 
00070 *                                                                 ELTMHSC 
00071      05  ADDITIONAL-TEXT-SW       PIC X      VALUE SPACE.         ELTMHSC 
00072          88 BLANK-LINE-NEEDED                VALUE 'B'.           ELTMHSC 
00073          88 ADDITIONAL-TEXT                  VALUE 'Y'.           ELTMHSC 
00074 *                                                                 ELTMHSC 
00075      05  CONTINUED-PROCESSING-SW  PIC X      VALUE SPACE.         ELTMHSC 
00076          88 PROCESSING-CMF-TEXT              VALUE 'P'.           ELTMHSC 
00077          88 DONE-PROCESSING                  VALUE 'D'.           ELTMHSC 
00078 *                                                                 ELTMHSC 
00079      05  WS-PERIOD-SW             PIC X      VALUE 'N'.           ELTMHSC 
00080          88 PERIOD-NEEDED                    VALUE 'Y'.           ELTMHSC 
00081 *                                                                 ELTMHSC 
00082      05  WS-PAYMENT-LEVEL-SW      PIC X      VALUE SPACE.         ELTMHSC 
00083          88 PAYMENT-LVL-TRANSLATED           VALUE 'N'.           ELTMHSC 
00084          88 PAYMENT-LVL-NOT-TRANSLATED       VALUE 'Y'.           ELTMHSC 
00085 *                                                                 ELTMHSC 
00086 ******************************************************************ELTMHSC 
00087 *SCREEN BODY LINES                                                ELTMHSC 
00088 ******************************************************************ELTMHSC 
00089  01  WS-HDR-LN2.                                                  ELTMHSC 
00090      05  FILLER            PIC X(12)         VALUE SPACES.        ELTMHSC 
00091      05  FILLER            PIC X(43)         VALUE                ELTMHSC 
00092          'MENTAL HEALTH SUBSTANCE ABUSE CARE PROGRAM '.           ELTMHSC 
00093      05  HDR-TITLE         PIC X(13)         VALUE SPACES.        ELTMHSC 
00094      05  FILLER            PIC X(13)         VALUE SPACES.        ELTMHSC 
00095                                                                   ELTMHSC 
00096  01  WS-APPROVAL-SOURCE.                                          ELTMHSC 
00097      05  FILLER             PIC X(79)        VALUE                ELTMHSC 
00098          'THE MENTAL HEALTH CARE SUBSTANCE ABUSE CARE PROGRAM REQUELTMHSC 
00099 -        'IRES THE APPROVAL OF '.                                 ELTMHSC 
00100 *                                                                 ELTMHSC 
00101  01  WS-PARTICIPATION-IND.                                        ELTMHSC 
00102      05  FILLER             PIC X(79)        VALUE                ELTMHSC 
00103          'THE MENTAL HEALTH SUBSTANCE ABUSE CARE PROGRAM APPLIES TELTMHSC 
00104 -        'O '.                                                    ELTMHSC 
00105 *                                                                 ELTMHSC 
00106  01  WS-INDICATOR.                                                ELTMHSC 
00107      05  FILLER             PIC X(79)        VALUE                ELTMHSC 
00108          'THE MENTAL HEALTH SUBSTANCE ABUSE CARE PROGRAM '.       ELTMHSC 
00109 *                                                                 ELTMHSC 
00110  01  WS-NETWORK-UTIL-REV-PHR.                                     ELTMHSC 
00111      05  FILLER             PIC X(79)        VALUE                ELTMHSC 
00112      'MENTAL HEALTH/SUBSTANCE ABUSE PROCESSING IS BASED ON - '.   ELTMHSC 
00113 *                                                                 ELTMHSC 
00114  01  WS-PAYMENT-LEVEL.                                            ELTMHSC 
00115      05  FILLER                  PIC X(79)   VALUE                ELTMHSC 
00116      'MENTAL HEALTH SUBSTANCE ABUSE CARE PAYMENT LEVEL RULES ARE  ELTMHSC 
00117 -    ' AS FOLLOWS:  '.                                            ELTMHSC 
00118 *                                                                 ELTMHSC 
00119  01  WS-CALC-METHOD.                                              ELTMHSC 
00120      05  FILLER                  PIC X(79)   VALUE                ELTMHSC 
00121      'THE METHOD(S) FOR CALCULATING MENTAL HEALTH CARE SUBSTANCE  ELTMHSC 
00122 -    'ABUSE BENEFITS IS '.                                        ELTMHSC 
00123 *                                                                 ELTMHSC 
00124  01  WS-CALC-METHODA.                                             ELTMHSC 
00125      05  FILLER                  PIC X(79)   VALUE                ELTMHSC 
00126      'AS FOLLOWS:'.                                               ELTMHSC 
00127 *                                                                 ELTMHSC 
00128  01  WS-ALTERNATE-PRICING.                                        ELTMHSC 
00129      05  FILLER                  PIC X(79)   VALUE                ELTMHSC 
00130          'THE ALTERNATE PRICING FOR PROFESSIONAL SERVICES IS '.   ELTMHSC 
00131 *                                                                 ELTMHSC 
00132  01  WS-BENEFITS-REDUCTION.                                       ELTMHSC 
00133      05  FILLER                  PIC X(79)   VALUE                ELTMHSC 
00134          'DENIED OR REDUCED BENEFITS DUE TO COST CONTAINMENT:'.   ELTMHSC 
00135 *                                                                 ELTMHSC 
00136  01  WS-SPILLOVER-SENTENCE.                                       ELTMHSC 
00137      05  FILLER                 PIC X(79)    VALUE                ELTMHSC 
00138          'UNPAID SERVICES AFTER BASIC BENEFIT REDUCTIONS ARE '.   ELTMHSC 
00139 *                                                                 ELTMHSC 
00140  01  WS-NOT-APPLICABLE-LOB-BC.                                    ELTMHSC 
00141      05  FILLER                    PIC X(79)   VALUE              ELTMHSC 
00142          'THE MENTAL HEALTH CARE SUBSTANCE ABUSE PROGRAM DOES NOT ELTMHSC 
00143 -        'APPLY TO INSTITUTIONAL'.                                ELTMHSC 
00144 *                                                                 ELTMHSC 
00145  01  WS-NOT-APPLICABLE-LOB-BCA.                                   ELTMHSC 
00146      05  FILLER                    PIC X(09)   VALUE              ELTMHSC 
00147          'BENEFITS.'.                                             ELTMHSC 
00148 *                                                                 ELTMHSC 
00149  01  WS-NOT-APPLICABLE-LOB-BS.                                    ELTMHSC 
00150      05  FILLER                    PIC X(79)   VALUE              ELTMHSC 
00151          'THE MENTAL HEALTH CARE SUBSTANCE ABUSE PROGRAM DOES NOT ELTMHSC 
00152 -        'APPLY TO PROFESSIONAL '.                                ELTMHSC 
00153 *                                                                 ELTMHSC 
00154  01  WS-NOT-APPLICABLE-LOB-BSA.                                   ELTMHSC 
00155      05  FILLER                    PIC X(79)   VALUE              ELTMHSC 
00156          'BENEFITS.'.                                             ELTMHSC 
00157 *                                                                 ELTMHSC 
00158  01  WS-NOT-APPLICABLE-LOB-MM.                                    ELTMHSC 
00159      05  FILLER                    PIC X(79)   VALUE              ELTMHSC 
00160          'THE MENTAL HEALTH CARE SUBSTANCE ABUSE PROGRAM DOES NOT ELTMHSC 
00161 -        'APPLY TO SUPPLEMENTAL '.                                ELTMHSC 
00162 *                                                                 ELTMHSC 
00163  01  WS-NOT-APPLICABLE-LOB-MMA.                                   ELTMHSC 
00164      05  FILLER                    PIC X(09)   VALUE              ELTMHSC 
00165          'BENEFITS.'.                                             ELTMHSC 
00166 *                                                                 ELTMHSC 
00167  01  WS-NOT-APPLICABLE-MSG.                                       ELTMHSC 
00168      05  FILLER                    PIC X(79)  VALUE               ELTMHSC 
00169          'THE MENTAL HEALTH CARE SUBSTANCE ABUSE PROGRAM IS NOT A ELTMHSC 
00170 -        'PPLICABLE.'.                                            ELTMHSC 
00171 *                                                                 ELTMHSC 
00172  01  WS-VOLUNTARY-MSG.                                            ELTMHSC 
00173      05  FILLER                    PIC X(74)  VALUE               ELTMHSC 
00174          'THE MENTAL HEALTH CARE SUBSTANCE ABUSE PROGRAM IS VOLUNTELTMHSC 
00175 -        'ARY.'.                                                  ELTMHSC 
00176 *                                                                 ELTMHSC 
00177  01  WS-DISCLAIMER.                                               ELTMHSC 
00178      05  FILLER                    PIC X(79)  VALUE               ELTMHSC 
00179         '*** SUBJECT TO CONTRACT LIMITATIONS ***'.                ELTMHSC 
00180  01  WS-END                              PIC X(18) VALUE          ELTMHSC 
00181                                          '*** END OF W/S ***'.    ELTMHSC 
00182  LINKAGE SECTION.                                                 ELTMHSC 
00183  01  DFHCOMMAREA.                                                 ELTMHSC 
00184      COPY ELSCOMMC.                                               ELTMHSC 
00185 /                                                                 ELTMHSC 
00186      COPY ELSCIA2C.                                               ELTMHSC 
00187 /                                                                 ELTMHSC 
00188      COPY ELSCMDSC.                                               ELTMHSC 
00189 /                                                                 ELTMHSC 
00190      COPY ELSCMIFC.                                               ELTMHSC 
00191 /                                                                 ELTMHSC 
00192      COPY ELSIOPMC.                                               ELTMHSC 
00193 /                                                                 ELTMHSC 
00194      COPY ELSKEYSC.                                               ELTMHSC 
00195 /                                                                 ELTMHSC 
00196      COPY ELSOUTPC.                                               ELTMHSC 
00197 /                                                                 ELTMHSC 
00198      COPY ELSSRTPC.                                               ELTMHSC 
00199 /                                                                 ELTMHSC 
00200      COPY ELSTCWAC.                                               ELTMHSC 
00201 /                                                                 ELTMHSC 
00202      COPY ELSSSCBC.                                               ELTMHSC 
00203 /                                                                 ELTMHSC 
00204  01  GROUP-SPECIFIC-RECORD.                                       ELTMHSC 
00205      COPY GCGROUPC.                                               ELTMHSC 
00206 /                                                                 ELTMHSC 
00207  01  GCCP-TABULAR-REC.                                            ELTMHSC 
00208      COPY GCTGCCPC.                                               ELTMHSC 
00209 /                                                                 ELTMHSC 
00210      EJECT                                                        ELTMHSC 
00211  PROCEDURE DIVISION.                                              ELTMHSC 
00212 ************************************************************      ELTMHSC 
00213 *                                                          *      ELTMHSC 
00214 *                    PROCEDURE DIVISION                    *      ELTMHSC 
00215 *                                                          *      ELTMHSC 
00216 ************************************************************      ELTMHSC 
00217                                                                   ELTMHSC 
00218                                                                   ELTMHSC 
00219 ************************************************************      ELTMHSC 
00220 *                                                          *      ELTMHSC 
00221 *        MENTAL HEALTH CARE SUBSTANCE ABUSE                *      ELTMHSC 
00222 *                                                          *      ELTMHSC 
00223 ************************************************************      ELTMHSC 
00224  MENTAL-HEALTH-CARE-SUBSTANCE-A.                                  ELTMHSC 
00225      PERFORM INITIALIZATION.                                      ELTMHSC 
00226      PERFORM PROCESS-MHSC.                                        ELTMHSC 
00227      GOBACK.                                                      ELTMHSC 
00228                                                                   ELTMHSC 
00229                                                                   ELTMHSC 
00230 ************************************************************      ELTMHSC 
00231 *                                                          *      ELTMHSC 
00232 *        INITIALIZATION                                    *      ELTMHSC 
00233 *                                                          *      ELTMHSC 
00234 ************************************************************      ELTMHSC 
00235  INITIALIZATION.                                                  ELTMHSC 
00236      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTMHSC 
00237      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTMHSC 
00238                                                                   ELTMHSC 
00239                                                                   ELTMHSC 
00240 ************************************************************      ELTMHSC 
00241 *                                                          *      ELTMHSC 
00242 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTMHSC 
00243 *                                                          *      ELTMHSC 
00244 ************************************************************      ELTMHSC 
00245  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTMHSC 
00246      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTMHSC 
00247      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTMHSC 
00248      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTMHSC 
00249                                                                   ELTMHSC 
00250                                                                   ELTMHSC 
00251 ************************************************************      ELTMHSC 
00252 *                                                          *      ELTMHSC 
00253 *        CHECK FOR VALID COMMAREA                          *      ELTMHSC 
00254 *                                                          *      ELTMHSC 
00255 ************************************************************      ELTMHSC 
00256  CHECK-FOR-VALID-COMMAREA.                                        ELTMHSC 
00257      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTMHSC 
00258          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTMHSC 
00259                                                                   ELTMHSC 
00260                                                                   ELTMHSC 
00261 ************************************************************      ELTMHSC 
00262 *                                                          *      ELTMHSC 
00263 *        SIGNAL INVALID COMMAREA                           *      ELTMHSC 
00264 *                                                          *      ELTMHSC 
00265 ************************************************************      ELTMHSC 
00266  SIGNAL-INVALID-COMMAREA.                                         ELTMHSC 
00267      EXEC CICS ABEND                                              ELTMHSC 
00268                ABCODE('EL01')                                     ELTMHSC 
00269         END-EXEC.                                                 ELTMHSC 
00270      EJECT                                                        ELTMHSC 
00271                                                                   ELTMHSC 
00272                                                                   ELTMHSC 
00273 ************************************************************      ELTMHSC 
00274 *                                                          *      ELTMHSC 
00275 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTMHSC 
00276 *                                                          *      ELTMHSC 
00277 ************************************************************      ELTMHSC 
00278  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTMHSC 
00279      IF ECA-CIA-PTR = NULL                                        ELTMHSC 
00280          PERFORM SIGNAL-INVALID-CIA                               ELTMHSC 
00281      ELSE                                                         ELTMHSC 
00282          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTMHSC 
00283                                                                   ELTMHSC 
00284                                                                   ELTMHSC 
00285 ************************************************************      ELTMHSC 
00286 *                                                          *      ELTMHSC 
00287 *        SIGNAL INVALID CIA                                *      ELTMHSC 
00288 *                                                          *      ELTMHSC 
00289 ************************************************************      ELTMHSC 
00290  SIGNAL-INVALID-CIA.                                              ELTMHSC 
00291      EXEC CICS ABEND                                              ELTMHSC 
00292                ABCODE('EL02')                                     ELTMHSC 
00293         END-EXEC.                                                 ELTMHSC 
00294      EJECT                                                        ELTMHSC 
00295                                                                   ELTMHSC 
00296                                                                   ELTMHSC 
00297 ************************************************************      ELTMHSC 
00298 *                                                          *      ELTMHSC 
00299 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTMHSC 
00300 *                                                          *      ELTMHSC 
00301 ************************************************************      ELTMHSC 
00302  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTMHSC 
00303      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTMHSC 
00304      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMHSC 
00305          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTMHSC 
00306      IF CIA-RC-PTR-NULL                                           ELTMHSC 
00307          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMHSC 
00308                                                                   ELTMHSC 
00309                                                                   ELTMHSC 
00310 ************************************************************      ELTMHSC 
00311 *                                                          *      ELTMHSC 
00312 *        SIGNAL UNALLOC AREA ERROR                         *      ELTMHSC 
00313 *                                                          *      ELTMHSC 
00314 ************************************************************      ELTMHSC 
00315  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTMHSC 
00316      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTMHSC 
00317      PERFORM SIGNAL-ABEND.                                        ELTMHSC 
00318                                                                   ELTMHSC 
00319                                                                   ELTMHSC 
00320 ************************************************************      ELTMHSC 
00321 *                                                          *      ELTMHSC 
00322 *        SIGNAL ABEND                                      *      ELTMHSC 
00323 *                                                          *      ELTMHSC 
00324 ************************************************************      ELTMHSC 
00325  SIGNAL-ABEND.                                                    ELTMHSC 
00326      EXEC CICS ABEND                                              ELTMHSC 
00327                ABCODE(CIA-ABCODE)                                 ELTMHSC 
00328         END-EXEC.                                                 ELTMHSC 
00329      EJECT                                                        ELTMHSC 
00330                                                                   ELTMHSC 
00331                                                                   ELTMHSC 
00332 ************************************************************      ELTMHSC 
00333 *                                                          *      ELTMHSC 
00334 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTMHSC 
00335 *                                                          *      ELTMHSC 
00336 ************************************************************      ELTMHSC 
00337  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTMHSC 
00338      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTMHSC 
00339      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTMHSC 
00340      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTMHSC 
00341      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTMHSC 
00342      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTMHSC 
00343      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTMHSC 
00344      PERFORM ESTABLISH-ADDRESSABILITY-OF-CO.                      ELTMHSC 
00345                                                                   ELTMHSC 
00346                                                                   ELTMHSC 
00347 ************************************************************      ELTMHSC 
00348 *                                                          *      ELTMHSC 
00349 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTMHSC 
00350 *                                                          *      ELTMHSC 
00351 ************************************************************      ELTMHSC 
00352  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTMHSC 
00353      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTMHSC 
00354      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMHSC 
00355          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTMHSC 
00356      IF CIA-RC-PTR-NULL                                           ELTMHSC 
00357          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMHSC 
00358      EJECT                                                        ELTMHSC 
00359                                                                   ELTMHSC 
00360                                                                   ELTMHSC 
00361 ************************************************************      ELTMHSC 
00362 *                                                          *      ELTMHSC 
00363 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTMHSC 
00364 *                                                          *      ELTMHSC 
00365 ************************************************************      ELTMHSC 
00366  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTMHSC 
00367      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTMHSC 
00368      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMHSC 
00369          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTMHSC 
00370      IF CIA-RC-PTR-NULL                                           ELTMHSC 
00371          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMHSC 
00372      EJECT                                                        ELTMHSC 
00373                                                                   ELTMHSC 
00374                                                                   ELTMHSC 
00375 ************************************************************      ELTMHSC 
00376 *                                                          *      ELTMHSC 
00377 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTMHSC 
00378 *                                                          *      ELTMHSC 
00379 ************************************************************      ELTMHSC 
00380  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTMHSC 
00381      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTMHSC 
00382      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMHSC 
00383          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELTMHSC 
00384      IF CIA-RC-PTR-NULL                                           ELTMHSC 
00385          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMHSC 
00386      EJECT                                                        ELTMHSC 
00387                                                                   ELTMHSC 
00388                                                                   ELTMHSC 
00389 ************************************************************      ELTMHSC 
00390 *                                                          *      ELTMHSC 
00391 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTMHSC 
00392 *                                                          *      ELTMHSC 
00393 ************************************************************      ELTMHSC 
00394  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTMHSC 
00395      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTMHSC 
00396      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMHSC 
00397          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTMHSC 
00398      IF CIA-RC-PTR-NULL                                           ELTMHSC 
00399          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMHSC 
00400      EJECT                                                        ELTMHSC 
00401                                                                   ELTMHSC 
00402                                                                   ELTMHSC 
00403 ************************************************************      ELTMHSC 
00404 *                                                          *      ELTMHSC 
00405 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTMHSC 
00406 *                                                          *      ELTMHSC 
00407 ************************************************************      ELTMHSC 
00408  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTMHSC 
00409      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTMHSC 
00410      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMHSC 
00411          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTMHSC 
00412      IF CIA-RC-PTR-NULL                                           ELTMHSC 
00413          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMHSC 
00414      EJECT                                                        ELTMHSC 
00415                                                                   ELTMHSC 
00416                                                                   ELTMHSC 
00417 ************************************************************      ELTMHSC 
00418 *                                                          *      ELTMHSC 
00419 *        ESTABLISH ADDRESSABILITY OF GRP SPECIFIC          *      ELTMHSC 
00420 *                                                          *      ELTMHSC 
00421 ************************************************************      ELTMHSC 
00422  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTMHSC 
00423      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTMHSC 
00424      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMHSC 
00425          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELTMHSC 
00426      IF CIA-RC-PTR-NULL                                           ELTMHSC 
00427          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMHSC 
00428      EJECT                                                        ELTMHSC 
00429                                                                   ELTMHSC 
00430                                                                   ELTMHSC 
00431 ************************************************************      ELTMHSC 
00432 *                                                          *      ELTMHSC 
00433 *        ESTABLISH ADDRESSABILITY OF COST CONTAINMENT      *      ELTMHSC 
00434 *                                                          *      ELTMHSC 
00435 ************************************************************      ELTMHSC 
00436  ESTABLISH-ADDRESSABILITY-OF-CO.                                  ELTMHSC 
00437      SET CIA-GCTABULR-DDN TO TRUE.                                ELTMHSC 
00438      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMHSC 
00439          ADDRESS OF GCCP-TABULAR-REC.                             ELTMHSC 
00440      IF CIA-RC-PTR-NULL                                           ELTMHSC 
00441          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMHSC 
00442                                                                   ELTMHSC 
00443                                                                   ELTMHSC 
00444 ************************************************************      ELTMHSC 
00445 *                                                          *      ELTMHSC 
00446 *        ESTABLISH ADDRESS OF CIA                          *      ELTMHSC 
00447 *                                                          *      ELTMHSC 
00448 ************************************************************      ELTMHSC 
00449  ESTABLISH-ADDRESS-OF-CIA.                                        ELTMHSC 
00450      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTMHSC 
00451          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTMHSC 
00452      EJECT                                                        ELTMHSC 
00453                                                                   ELTMHSC 
00454                                                                   ELTMHSC 
00455 ************************************************************      ELTMHSC 
00456 *                                                          *      ELTMHSC 
00457 *        PROCESS MHSC                                      *      ELTMHSC 
00458 *                                                          *      ELTMHSC 
00459 ************************************************************      ELTMHSC 
00460  PROCESS-MHSC.                                                    ELTMHSC 
00461      IF GCG-NEW-MEN-SUB-ABUSE-IND EQUAL ZERO                      ELTMHSC 
00462         OR GCG-NEW-MEN-SUB-ABUSE-IND EQUAL '08'                   ELTMHSC 
00463          PERFORM TEST-APPLICABILITY                               ELTMHSC 
00464      ELSE                                                         ELTMHSC 
00465          PERFORM GENERATE-MHSC-TEXT.                              ELTMHSC 
00466      MOVE 'E' TO  COF-FUNCTION.                                   ELTMHSC 
00467      MOVE ZEROS TO COF-NBR-DTL-LINES                              ELTMHSC 
00468                COF-NBR-HDR-LINES.                                 ELTMHSC 
00469      PERFORM LINK-TO-OUTPUT.                                      ELTMHSC 
00470      EJECT                                                        ELTMHSC 
00471                                                                   ELTMHSC 
00472                                                                   ELTMHSC 
00473 ************************************************************      ELTMHSC 
00474 *                                                          *      ELTMHSC 
00475 *        GENERATE MHSC TEXT                                *      ELTMHSC 
00476 *                                                          *      ELTMHSC 
00477 ************************************************************      ELTMHSC 
00478  GENERATE-MHSC-TEXT.                                              ELTMHSC 
00479      PERFORM VERIFY-MHSC-IN-GCCP-RECORD.                          ELTMHSC 
00480      PERFORM BUILD-MHSC-TEXT.                                     ELTMHSC 
00481                                                                   ELTMHSC 
00482                                                                   ELTMHSC 
00483 ************************************************************      ELTMHSC 
00484 *                                                          *      ELTMHSC 
00485 *        TEST APPLICABILITY                                *      ELTMHSC 
00486 *                                                          *      ELTMHSC 
00487 ************************************************************      ELTMHSC 
00488  TEST-APPLICABILITY.                                              ELTMHSC 
00489      PERFORM GENERATE-HEADINGS.                                   ELTMHSC 
00490      IF GCG-NEW-MEN-SUB-ABUSE-IND EQUAL ZERO                      ELTMHSC 
00491         PERFORM SIGNAL-NOT-APPLICABLE-MSG                         ELTMHSC 
00492      ELSE IF GCG-NEW-MEN-SUB-ABUSE-IND EQUAL '08'                 ELTMHSC 
00493              PERFORM SIGNAL-VOLUNTARY-MSG.                        ELTMHSC 
00494      PERFORM LINK-TO-OUTPUT.                                      ELTMHSC 
00495                                                                   ELTMHSC 
00496                                                                   ELTMHSC 
00497 ************************************************************      ELTMHSC 
00498 *                                                          *      ELTMHSC 
00499 *        GENERATE NOT APPLICABLE SENTENCE                  *      ELTMHSC 
00500 *                                                          *      ELTMHSC 
00501 ************************************************************      ELTMHSC 
00502  GENERATE-NOT-APPLICABLE-SENTEN.                                  ELTMHSC 
00503      PERFORM GENERATE-HEADINGS.                                   ELTMHSC 
00504      PERFORM SIGNAL-NOT-APPLICABLE-MSG.                           ELTMHSC 
00505      PERFORM LINK-TO-OUTPUT.                                      ELTMHSC 
00506                                                                   ELTMHSC 
00507                                                                   ELTMHSC 
00508 ************************************************************      ELTMHSC 
00509 *                                                          *      ELTMHSC 
00510 *        VERIFY MHSC IN GCCP RECORD                        *      ELTMHSC 
00511 *                                                          *      ELTMHSC 
00512 ************************************************************      ELTMHSC 
00513  VERIFY-MHSC-IN-GCCP-RECORD.                                      ELTMHSC 
00514      PERFORM ACQUIRE-GCCP-RECORD.                                 ELTMHSC 
00515      PERFORM OBTAIN-MHSC-WITHIN-GCCP-RECORD.                      ELTMHSC 
00516      EJECT                                                        ELTMHSC 
00517                                                                   ELTMHSC 
00518                                                                   ELTMHSC 
00519 ************************************************************      ELTMHSC 
00520 *                                                          *      ELTMHSC 
00521 *        ACQUIRE GCCP RECORD                               *      ELTMHSC 
00522 *                                                          *      ELTMHSC 
00523 ************************************************************      ELTMHSC 
00524  ACQUIRE-GCCP-RECORD.                                             ELTMHSC 
00525      MOVE SPACES TO KWA-PROVISION-ID.                             ELTMHSC 
00526      SET GCG-INDEX TO 1.                                          ELTMHSC 
00527      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTMHSC 
00528          AT END                                                   ELTMHSC 
00529             MOVE ZEROES TO KWA-PROVISION-SLOT-NO                  ELTMHSC 
00530          WHEN GCG-TAB-ID (GCG-INDEX) = PC-GCCP                    ELTMHSC 
00531                 MOVE GCG-TAB-ID (GCG-INDEX) TO                    ELTMHSC 
00532          KWA-PROVISION-ID                                         ELTMHSC 
00533                 MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                  ELTMHSC 
00534                     TO KWA-PROVISION-SLOT-NO                      ELTMHSC 
00535          END-SEARCH.                                              ELTMHSC 
00536      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELTMHSC 
00537          PERFORM SIGNAL-UNDEFINED-TABULAR                         ELTMHSC 
00538      ELSE                                                         ELTMHSC 
00539          PERFORM READ-GCCP-RECORD.                                ELTMHSC 
00540                                                                   ELTMHSC 
00541                                                                   ELTMHSC 
00542 ************************************************************      ELTMHSC 
00543 *                                                          *      ELTMHSC 
00544 *        SIGNAL UNDEFINED TABULAR                          *      ELTMHSC 
00545 *                                                          *      ELTMHSC 
00546 ************************************************************      ELTMHSC 
00547  SIGNAL-UNDEFINED-TABULAR.                                        ELTMHSC 
00548      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTMHSC 
00549      PERFORM SIGNAL-ABEND.                                        ELTMHSC 
00550                                                                   ELTMHSC 
00551                                                                   ELTMHSC 
00552 ************************************************************      ELTMHSC 
00553 *                                                          *      ELTMHSC 
00554 *        OBTAIN MHSC WITHIN GCCP RECORD                    *      ELTMHSC 
00555 *                                                          *      ELTMHSC 
00556 ************************************************************      ELTMHSC 
00557  OBTAIN-MHSC-WITHIN-GCCP-RECORD.                                  ELTMHSC 
00558      SET GSS-INDEX TO 1.                                          ELTMHSC 
00559      SEARCH GSS-ENTRY                                             ELTMHSC 
00560         AT END                                                    ELTMHSC 
00561            SET TABULAR-IS-UNDEFINED TO TRUE                       ELTMHSC 
00562         WHEN GSS-S1-PROG-CODE-CHR (GSS-INDEX)                     ELTMHSC 
00563                 CONTINUE                                          ELTMHSC 
00564          END-SEARCH.                                              ELTMHSC 
00565      IF TABULAR-IS-UNDEFINED                                      ELTMHSC 
00566          PERFORM SIGNAL-UNDEFINED-TABULAR.                        ELTMHSC 
00567      EJECT                                                        ELTMHSC 
00568                                                                   ELTMHSC 
00569                                                                   ELTMHSC 
00570 ************************************************************      ELTMHSC 
00571 *                                                          *      ELTMHSC 
00572 *        BUILD MHSC TEXT                                   *      ELTMHSC 
00573 *                                                          *      ELTMHSC 
00574 ************************************************************      ELTMHSC 
00575  BUILD-MHSC-TEXT.                                                 ELTMHSC 
00576      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTMHSC 
00577          PERFORM GENERATE-INSTITUTIONAL.                          ELTMHSC 
00578      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELTMHSC 
00579          PERFORM GENERATE-PROFESSIONAL.                           ELTMHSC 
00580      IF GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                        ELTMHSC 
00581                 '03' OR '04' OR '06' OR '08'                      ELTMHSC 
00582          PERFORM PROCESS-SUPPLEMENTAL.                            ELTMHSC 
00583                                                                   ELTMHSC 
00584                                                                   ELTMHSC 
00585 ************************************************************      ELTMHSC 
00586 *                                                          *      ELTMHSC 
00587 *        GENERATE INSTITUTIONAL                            *      ELTMHSC 
00588 *                                                          *      ELTMHSC 
00589 ************************************************************      ELTMHSC 
00590  GENERATE-INSTITUTIONAL.                                          ELTMHSC 
00591      SET INSTITUTIONAL-SCREEN TO TRUE.                            ELTMHSC 
00592      PERFORM GENERATE-HEADINGS.                                   ELTMHSC 
00593      IF GSS-S1-BC-IND (GSS-INDEX) EQUAL ZEROES OR SPACES          ELTMHSC 
00594                 OR LOW-VALUES                                     ELTMHSC 
00595          PERFORM SIGNAL-NOT-APPLICABLE-FOR-BC-L                   ELTMHSC 
00596      ELSE                                                         ELTMHSC 
00597          PERFORM CONSTRUCT-BC-TEXT-AND-SCREEN.                    ELTMHSC 
00598      EJECT                                                        ELTMHSC 
00599                                                                   ELTMHSC 
00600                                                                   ELTMHSC 
00601 ************************************************************      ELTMHSC 
00602 *                                                          *      ELTMHSC 
00603 *        GENERATE PROFESSIONAL                             *      ELTMHSC 
00604 *                                                          *      ELTMHSC 
00605 ************************************************************      ELTMHSC 
00606  GENERATE-PROFESSIONAL.                                           ELTMHSC 
00607      SET PROFESSIONAL-SCREEN TO TRUE.                             ELTMHSC 
00608      PERFORM GENERATE-HEADINGS.                                   ELTMHSC 
00609      IF GSS-S1-BS-IND (GSS-INDEX) EQUAL ZEROES OR SPACES          ELTMHSC 
00610                 OR LOW-VALUES                                     ELTMHSC 
00611          PERFORM SIGNAL-NOT-APPLICABLE-FOR-BS-L                   ELTMHSC 
00612      ELSE                                                         ELTMHSC 
00613          PERFORM CONSTRUCT-BS-TEXT-AND-SCREEN.                    ELTMHSC 
00614      EJECT                                                        ELTMHSC 
00615                                                                   ELTMHSC 
00616                                                                   ELTMHSC 
00617 ************************************************************      ELTMHSC 
00618 *                                                          *      ELTMHSC 
00619 *        PROCESS SUPPLEMENTAL                              *      ELTMHSC 
00620 *                                                          *      ELTMHSC 
00621 ************************************************************      ELTMHSC 
00622  PROCESS-SUPPLEMENTAL.                                            ELTMHSC 
00623      SET SUPPLEMENTAL-SCREEN TO TRUE.                             ELTMHSC 
00624      PERFORM GENERATE-HEADINGS.                                   ELTMHSC 
00625      IF GSS-S1-MM-IND (GSS-INDEX) EQUAL ZEROES OR SPACES          ELTMHSC 
00626                 OR LOW-VALUES                                     ELTMHSC 
00627          PERFORM SIGNAL-NOT-APPLICABLE-FOR-MM-L                   ELTMHSC 
00628      ELSE                                                         ELTMHSC 
00629          PERFORM CONSTRUCT-MM-TEXT-AND-SCREEN.                    ELTMHSC 
00630      EJECT                                                        ELTMHSC 
00631                                                                   ELTMHSC 
00632                                                                   ELTMHSC 
00633 ************************************************************      ELTMHSC 
00634 *                                                          *      ELTMHSC 
00635 *        GENERATE HEADINGS                                 *      ELTMHSC 
00636 *                                                          *      ELTMHSC 
00637 ************************************************************      ELTMHSC 
00638  GENERATE-HEADINGS.                                               ELTMHSC 
00639      SET COF-NEW-PAGE TO TRUE.                                    ELTMHSC 
00640      IF INSTITUTIONAL-SCREEN                                      ELTMHSC 
00641          PERFORM MOVE-INST-HEADINGS                               ELTMHSC 
00642      ELSE IF PROFESSIONAL-SCREEN                                  ELTMHSC 
00643          PERFORM MOVE-PROF-HEADINGS                               ELTMHSC 
00644      ELSE IF SUPPLEMENTAL-SCREEN                                  ELTMHSC 
00645          PERFORM MOVE-SUPP-HEADINGS.                              ELTMHSC 
00646      MOVE 2            TO COF-NBR-HDR-LINES.                      ELTMHSC 
00647      MOVE WS-HDR-LN2   TO COF-HDR-LINE                            ELTMHSC 
00648          (COF-NBR-HDR-LINES).                                     ELTMHSC 
00649      MOVE +1           TO COF-NBR-DTL-LINES.                      ELTMHSC 
00650      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMHSC 
00651      PERFORM LINK-TO-OUTPUT.                                      ELTMHSC 
00652                                                                   ELTMHSC 
00653                                                                   ELTMHSC 
00654 ************************************************************      ELTMHSC 
00655 *                                                          *      ELTMHSC 
00656 *        MOVE INST HEADINGS                                *      ELTMHSC 
00657 *                                                          *      ELTMHSC 
00658 ************************************************************      ELTMHSC 
00659  MOVE-INST-HEADINGS.                                              ELTMHSC 
00660      MOVE 'INSTITUTIONAL' TO HDR-TITLE.                           ELTMHSC 
00661                                                                   ELTMHSC 
00662                                                                   ELTMHSC 
00663 ************************************************************      ELTMHSC 
00664 *                                                          *      ELTMHSC 
00665 *        MOVE PROF HEADINGS                                *      ELTMHSC 
00666 *                                                          *      ELTMHSC 
00667 ************************************************************      ELTMHSC 
00668  MOVE-PROF-HEADINGS.                                              ELTMHSC 
00669      MOVE 'PROFESSIONAL' TO HDR-TITLE.                            ELTMHSC 
00670                                                                   ELTMHSC 
00671                                                                   ELTMHSC 
00672 ************************************************************      ELTMHSC 
00673 *                                                          *      ELTMHSC 
00674 *        MOVE SUPP HEADINGS                                *      ELTMHSC 
00675 *                                                          *      ELTMHSC 
00676 ************************************************************      ELTMHSC 
00677  MOVE-SUPP-HEADINGS.                                              ELTMHSC 
00678      MOVE 'SUPPLEMENTAL' TO HDR-TITLE.                            ELTMHSC 
00679      EJECT                                                        ELTMHSC 
00680                                                                   ELTMHSC 
00681                                                                   ELTMHSC 
00682 ************************************************************      ELTMHSC 
00683 *                                                          *      ELTMHSC 
00684 *        SIGNAL NOT APPLICABLE FOR BC LOB                  *      ELTMHSC 
00685 *                                                          *      ELTMHSC 
00686 ************************************************************      ELTMHSC 
00687  SIGNAL-NOT-APPLICABLE-FOR-BC-L.                                  ELTMHSC 
00688      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMHSC 
00689      MOVE WS-NOT-APPLICABLE-LOB-BC TO COF-DTL-LINE                ELTMHSC 
00690          (COF-NBR-DTL-LINES).                                     ELTMHSC 
00691      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMHSC 
00692      MOVE WS-NOT-APPLICABLE-LOB-BCA TO COF-DTL-LINE               ELTMHSC 
00693          (COF-NBR-DTL-LINES).                                     ELTMHSC 
00694      PERFORM LINK-TO-OUTPUT.                                      ELTMHSC 
00695                                                                   ELTMHSC 
00696                                                                   ELTMHSC 
00697 ************************************************************      ELTMHSC 
00698 *                                                          *      ELTMHSC 
00699 *        SIGNAL NOT APPLICABLE FOR BS LOB                  *      ELTMHSC 
00700 *                                                          *      ELTMHSC 
00701 ************************************************************      ELTMHSC 
00702  SIGNAL-NOT-APPLICABLE-FOR-BS-L.                                  ELTMHSC 
00703      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMHSC 
00704      MOVE WS-NOT-APPLICABLE-LOB-BS TO COF-DTL-LINE                ELTMHSC 
00705          (COF-NBR-DTL-LINES).                                     ELTMHSC 
00706      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMHSC 
00707      MOVE WS-NOT-APPLICABLE-LOB-BSA TO COF-DTL-LINE               ELTMHSC 
00708          (COF-NBR-DTL-LINES).                                     ELTMHSC 
00709      PERFORM LINK-TO-OUTPUT.                                      ELTMHSC 
00710                                                                   ELTMHSC 
00711                                                                   ELTMHSC 
00712 ************************************************************      ELTMHSC 
00713 *                                                          *      ELTMHSC 
00714 *        SIGNAL NOT APPLICABLE FOR MM LOB                  *      ELTMHSC 
00715 *                                                          *      ELTMHSC 
00716 ************************************************************      ELTMHSC 
00717  SIGNAL-NOT-APPLICABLE-FOR-MM-L.                                  ELTMHSC 
00718      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMHSC 
00719      MOVE WS-NOT-APPLICABLE-LOB-MM TO COF-DTL-LINE                ELTMHSC 
00720          (COF-NBR-DTL-LINES).                                     ELTMHSC 
00721      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMHSC 
00722      MOVE WS-NOT-APPLICABLE-LOB-MMA TO COF-DTL-LINE               ELTMHSC 
00723          (COF-NBR-DTL-LINES).                                     ELTMHSC 
00724      PERFORM LINK-TO-OUTPUT.                                      ELTMHSC 
00725      EJECT                                                        ELTMHSC 
00726                                                                   ELTMHSC 
00727                                                                   ELTMHSC 
00728 ************************************************************      ELTMHSC 
00729 *                                                          *      ELTMHSC 
00730 *        CONSTRUCT BC TEXT AND SCREEN                      *      ELTMHSC 
00731 *                                                          *      ELTMHSC 
00732 ************************************************************      ELTMHSC 
00733  CONSTRUCT-BC-TEXT-AND-SCREEN.                                    ELTMHSC 
00734      SET PAYMENT-LVL-NOT-TRANSLATED TO TRUE.                      ELTMHSC 
00735      PERFORM GENERATE-PARTICIPATION-IND.                          ELTMHSC 
00736      IF GSS-S1-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTMHSC 
00737          SPACES AND LOW-VALUES                                    ELTMHSC 
00738               AND ZERO                                            ELTMHSC 
00739          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMHSC 
00740      PERFORM TRANSLATE-BC-INDICATOR.                              ELTMHSC 
00741      IF GSS-S1-BC-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL         ELTMHSC 
00742          SPACES AND LOW-VALUES                                    ELTMHSC 
00743               AND ZERO                                            ELTMHSC 
00744          PERFORM TRANSLATE-BC-PAYMENT-LEVEL-IND.                  ELTMHSC 
00745      IF GCG-NETWORK-UTIL-REVIEW-IND NOT EQUAL                     ELTMHSC 
00746               SPACES AND LOW-VALUES AND ZERO                      ELTMHSC 
00747          PERFORM TRANSLATE-NETWORK-UTIL-REV-IND.                  ELTMHSC 
00748      SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTMHSC 
00749      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTMHSC 
00750      IF (GSS-S1-BC-CALC-METHOD (GSS-INDEX) NOT EQUAL              ELTMHSC 
00751               SPACES AND LOW-VALUES AND ZERO)                     ELTMHSC 
00752               AND PAYMENT-LVL-NOT-TRANSLATED                      ELTMHSC 
00753          PERFORM GENERATE-BC-CALC-METHOD.                         ELTMHSC 
00754      IF GSS-S1-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTMHSC 
00755                  SPACES AND LOW-VALUES AND ZERO                   ELTMHSC 
00756          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTMHSC 
00757      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                       ELTMHSC 
00758               '03' OR '04' OR '06' OR '08')                       ELTMHSC 
00759            AND                                                    ELTMHSC 
00760             GSS-S1-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL           ELTMHSC 
00761          SPACES                                                   ELTMHSC 
00762                AND LOW-VALUES AND ZERO                            ELTMHSC 
00763          PERFORM GENERATE-SPILL-OVER-INDICATOR.                   ELTMHSC 
00764      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTMHSC 
00765      MOVE SPACES TO SCREEN-TYPE.                                  ELTMHSC 
00766      EJECT                                                        ELTMHSC 
00767                                                                   ELTMHSC 
00768                                                                   ELTMHSC 
00769 ************************************************************      ELTMHSC 
00770 *                                                          *      ELTMHSC 
00771 *        CONSTRUCT BS TEXT AND SCREEN                      *      ELTMHSC 
00772 *                                                          *      ELTMHSC 
00773 ************************************************************      ELTMHSC 
00774  CONSTRUCT-BS-TEXT-AND-SCREEN.                                    ELTMHSC 
00775      SET PAYMENT-LVL-NOT-TRANSLATED TO TRUE.                      ELTMHSC 
00776      PERFORM GENERATE-PARTICIPATION-IND.                          ELTMHSC 
00777      SET PROFESSIONAL-SCREEN TO TRUE.                             ELTMHSC 
00778      IF GSS-S1-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTMHSC 
00779          SPACES                                                   ELTMHSC 
00780                AND LOW-VALUES AND ZERO                            ELTMHSC 
00781          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMHSC 
00782      PERFORM TRANSLATE-BS-INDICATOR.                              ELTMHSC 
00783      IF GSS-S1-BS-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL         ELTMHSC 
00784          LOW-VALUES                                               ELTMHSC 
00785               AND SPACES AND ZERO                                 ELTMHSC 
00786          PERFORM TRANSLATE-BS-PAYMENT-LEVEL-IND.                  ELTMHSC 
00787      IF GSS-S1-BS-ALT-PRICING-METHOD (GSS-INDEX) NOT EQUAL        ELTMHSC 
00788          SPACES                                                   ELTMHSC 
00789                  AND LOW-VALUES AND ZERO                          ELTMHSC 
00790          PERFORM GENERATE-BS-ALT-PRIC-TEXT.                       ELTMHSC 
00791      IF GCG-NETWORK-UTIL-REVIEW-IND NOT EQUAL                     ELTMHSC 
00792               SPACES AND LOW-VALUES AND ZERO                      ELTMHSC 
00793          PERFORM TRANSLATE-NETWORK-UTIL-REV-IND.                  ELTMHSC 
00794      SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTMHSC 
00795      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTMHSC 
00796      IF (GSS-S1-BS-CALC-METHOD (GSS-INDEX) NOT EQUAL SPACES       ELTMHSC 
00797               AND LOW-VALUES AND ZERO)                            ELTMHSC 
00798               AND PAYMENT-LVL-NOT-TRANSLATED                      ELTMHSC 
00799          PERFORM GENERATE-BS-CALC-METHOD.                         ELTMHSC 
00800      IF GSS-S1-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTMHSC 
00801                  SPACES AND LOW-VALUES AND ZERO                   ELTMHSC 
00802          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTMHSC 
00803      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                       ELTMHSC 
00804                   '03' OR '04' OR '06' OR '08')                   ELTMHSC 
00805            AND                                                    ELTMHSC 
00806             GSS-S1-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL           ELTMHSC 
00807          LOW-VALUES                                               ELTMHSC 
00808                         AND SPACES AND ZERO                       ELTMHSC 
00809          PERFORM GENERATE-SPILL-OVER-INDICATOR.                   ELTMHSC 
00810      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTMHSC 
00811      MOVE SPACES TO SCREEN-TYPE.                                  ELTMHSC 
00812      EJECT                                                        ELTMHSC 
00813                                                                   ELTMHSC 
00814                                                                   ELTMHSC 
00815 ************************************************************      ELTMHSC 
00816 *                                                          *      ELTMHSC 
00817 *        TRANSLATE NETWORK UTILIZATION REVIEW INDICATOR    *      ELTMHSC 
00818 *                                                          *      ELTMHSC 
00819 ************************************************************      ELTMHSC 
00820  TRANSLATE-NETWORK-UTIL-REV-IND.                                  ELTMHSC 
00821      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMHSC 
00822      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMHSC 
00823      SET PERIOD-NEEDED TO TRUE.                                   ELTMHSC 
00824      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMHSC 
00825      MOVE WS-NETWORK-UTIL-REV-PHR                                 ELTMHSC 
00826        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELTMHSC 
00827      ADD +1 TO TCAR-FROM-SUB.                                     ELTMHSC 
00828      MOVE PC-GROUP TO CMF-RECORD-PREFIX.                          ELTMHSC 
00829      MOVE 'NETWORK-UTIL-REVIEW-IND'                               ELTMHSC 
00830        TO  CMF-ELEMENT-SYSTEM-NAME.                               ELTMHSC 
00831      MOVE GCG-NETWORK-UTIL-REVIEW-IND                             ELTMHSC 
00832        TO CMF-CODE-VALUE.                                         ELTMHSC 
00833      PERFORM LINK-TO-TRANSLATOR.                                  ELTMHSC 
00834      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMHSC 
00835                                                                   ELTMHSC 
00836 ************************************************************      ELTMHSC 
00837 *                                                          *      ELTMHSC 
00838 *        CONSTRUCT MM TEXT AND SCREEN                      *      ELTMHSC 
00839 *                                                          *      ELTMHSC 
00840 ************************************************************      ELTMHSC 
00841  CONSTRUCT-MM-TEXT-AND-SCREEN.                                    ELTMHSC 
00842      SET PAYMENT-LVL-NOT-TRANSLATED TO TRUE.                      ELTMHSC 
00843      PERFORM GENERATE-PARTICIPATION-IND.                          ELTMHSC 
00844      IF GSS-S1-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTMHSC 
00845          LOW-VALUES                                               ELTMHSC 
00846                 AND SPACES AND ZERO                               ELTMHSC 
00847          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMHSC 
00848      PERFORM TRANSLATE-MM-INDICATOR.                              ELTMHSC 
00849      IF GSS-S1-MM-PAYMENT-LEVEL-IND (GSS-INDEX) NOT EQUAL         ELTMHSC 
00850          LOW-VALUES                                               ELTMHSC 
00851             AND SPACES AND ZERO                                   ELTMHSC 
00852          PERFORM TRANSLATE-MM-PAYMENT-LEVEL-IND.                  ELTMHSC 
00853      IF GCG-NETWORK-UTIL-REVIEW-IND NOT EQUAL                     ELTMHSC 
00854               SPACES AND LOW-VALUES AND ZERO                      ELTMHSC 
00855          PERFORM TRANSLATE-NETWORK-UTIL-REV-IND.                  ELTMHSC 
00856      SET SRP-ACCUM-PROV-CLASS-SUPP TO TRUE.                       ELTMHSC 
00857      PERFORM GENERATE-ASSOCIATED-ACCUMULATO.                      ELTMHSC 
00858      IF (GSS-S1-MM-CALC-METHOD (GSS-INDEX) NOT EQUAL LOW-VALUES   ELTMHSC 
00859               AND SPACES AND ZERO)                                ELTMHSC 
00860               AND PAYMENT-LVL-NOT-TRANSLATED                      ELTMHSC 
00861          PERFORM GENERATE-MM-CALC-METHOD.                         ELTMHSC 
00862      IF GSS-S1-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL         ELTMHSC 
00863                  SPACES AND LOW-VALUES AND ZERO                   ELTMHSC 
00864          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTMHSC 
00865      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTMHSC 
00866      EJECT                                                        ELTMHSC 
00867                                                                   ELTMHSC 
00868                                                                   ELTMHSC 
00869 ************************************************************      ELTMHSC 
00870 *                                                          *      ELTMHSC 
00871 *        GENERATE PARTICIPATION IND                        *      ELTMHSC 
00872 *                                                          *      ELTMHSC 
00873 ************************************************************      ELTMHSC 
00874  GENERATE-PARTICIPATION-IND.                                      ELTMHSC 
00875      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMHSC 
00876      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMHSC 
00877      SET PERIOD-NEEDED TO TRUE.                                   ELTMHSC 
00878      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMHSC 
00879      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMHSC 
00880      MOVE WS-PARTICIPATION-IND TO TCAR-FROM-LINE                  ELTMHSC 
00881          (TCAR-FROM-SUB).                                         ELTMHSC 
00882      ADD +1 TO TCAR-FROM-SUB.                                     ELTMHSC 
00883      MOVE PC-GROUP TO CMF-RECORD-PREFIX.                          ELTMHSC 
00884      MOVE 'NEW-MEN-SUB-ABUSE-IND'  TO CMF-ELEMENT-SYSTEM-NAME.    ELTMHSC 
00885      MOVE GCG-NEW-MEN-SUB-ABUSE-IND TO CMF-CODE-VALUE.            ELTMHSC 
00886      PERFORM LINK-TO-TRANSLATOR.                                  ELTMHSC 
00887      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMHSC 
00888      EJECT                                                        ELTMHSC 
00889                                                                   ELTMHSC 
00890                                                                   ELTMHSC 
00891 ************************************************************      ELTMHSC 
00892 *                                                          *      ELTMHSC 
00893 *        GENERATE ASSOCIATED ACCUMULATORS                  *      ELTMHSC 
00894 *                                                          *      ELTMHSC 
00895 ************************************************************      ELTMHSC 
00896  GENERATE-ASSOCIATED-ACCUMULATO.                                  ELTMHSC 
00897      PERFORM GENERATE-COINSURANCE-TEXT.                           ELTMHSC 
00898      PERFORM GENERATE-COPAY-TEXT.                                 ELTMHSC 
00899      PERFORM GENERATE-DEDUCTIBLE-TEXT.                            ELTMHSC 
00900      PERFORM GENERATE-BENEFIT-MAXIMUMS-TEXT.                      ELTMHSC 
00901      EJECT                                                        ELTMHSC 
00902                                                                   ELTMHSC 
00903                                                                   ELTMHSC 
00904 ************************************************************      ELTMHSC 
00905 *                                                          *      ELTMHSC 
00906 *        GENERATE DISCLAIMER SENTENCE                      *      ELTMHSC 
00907 *                                                          *      ELTMHSC 
00908 ************************************************************      ELTMHSC 
00909  GENERATE-DISCLAIMER-SENTENCE.                                    ELTMHSC 
00910      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMHSC 
00911      MOVE WS-DISCLAIMER TO COF-DTL-LINE (COF-NBR-DTL-LINES).      ELTMHSC 
00912      PERFORM LINK-TO-OUTPUT.                                      ELTMHSC 
00913      EJECT                                                        ELTMHSC 
00914                                                                   ELTMHSC 
00915                                                                   ELTMHSC 
00916 ************************************************************      ELTMHSC 
00917 *                                                          *      ELTMHSC 
00918 *        TRANSLATE APPROVAL SOURCE                         *      ELTMHSC 
00919 *                                                          *      ELTMHSC 
00920 ************************************************************      ELTMHSC 
00921  TRANSLATE-APPROVAL-SOURCE.                                       ELTMHSC 
00922      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMHSC 
00923      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMHSC 
00924      SET PERIOD-NEEDED TO TRUE.                                   ELTMHSC 
00925      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMHSC 
00926      MOVE WS-APPROVAL-SOURCE TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELTMHSC 
00927      ADD 1 TO TCAR-FROM-SUB.                                      ELTMHSC 
00928      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMHSC 
00929      MOVE 'S1-APPROVAL-SOURCE-IND' TO CMF-ELEMENT-SYSTEM-NAME.    ELTMHSC 
00930      MOVE GSS-S1-APPROVAL-SOURCE-IND (GSS-INDEX) TO               ELTMHSC 
00931          CMF-CODE-VALUE.                                          ELTMHSC 
00932      PERFORM LINK-TO-TRANSLATOR.                                  ELTMHSC 
00933      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMHSC 
00934                                                                   ELTMHSC 
00935                                                                   ELTMHSC 
00936 ************************************************************      ELTMHSC 
00937 *                                                          *      ELTMHSC 
00938 *        GET INDICATOR FIXED TEXT                          *      ELTMHSC 
00939 *                                                          *      ELTMHSC 
00940 ************************************************************      ELTMHSC 
00941  GET-INDICATOR-FIXED-TEXT.                                        ELTMHSC 
00942      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMHSC 
00943      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMHSC 
00944      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMHSC 
00945      MOVE WS-INDICATOR TO TCAR-FROM-LINE (TCAR-FROM-SUB).         ELTMHSC 
00946      ADD 1 TO TCAR-FROM-SUB.                                      ELTMHSC 
00947      EJECT                                                        ELTMHSC 
00948                                                                   ELTMHSC 
00949                                                                   ELTMHSC 
00950 ************************************************************      ELTMHSC 
00951 *                                                          *      ELTMHSC 
00952 *        TRANSLATE BC INDICATOR                            *      ELTMHSC 
00953 *                                                          *      ELTMHSC 
00954 ************************************************************      ELTMHSC 
00955  TRANSLATE-BC-INDICATOR.                                          ELTMHSC 
00956      PERFORM GET-INDICATOR-FIXED-TEXT.                            ELTMHSC 
00957      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMHSC 
00958      MOVE 'S1-BC-IND'  TO CMF-ELEMENT-SYSTEM-NAME.                ELTMHSC 
00959      MOVE GSS-S1-BC-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTMHSC 
00960      PERFORM LINK-TO-TRANSLATOR.                                  ELTMHSC 
00961      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMHSC 
00962      EJECT                                                        ELTMHSC 
00963                                                                   ELTMHSC 
00964                                                                   ELTMHSC 
00965 ************************************************************      ELTMHSC 
00966 *                                                          *      ELTMHSC 
00967 *        TRANSLATE BC PAYMENT LEVEL INDICATOR              *      ELTMHSC 
00968 *                                                          *      ELTMHSC 
00969 ************************************************************      ELTMHSC 
00970  TRANSLATE-BC-PAYMENT-LEVEL-IND.                                  ELTMHSC 
00971      PERFORM SETUP-PAYMENT-LEVEL-FIXED-TEXT.                      ELTMHSC 
00972      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMHSC 
00973      MOVE 'S1-BC-PAYMENT-LEVEL-IND'  TO CMF-ELEMENT-SYSTEM-NAME.  ELTMHSC 
00974      MOVE GSS-S1-BC-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTMHSC 
00975          CMF-CODE-VALUE.                                          ELTMHSC 
00976      PERFORM LINK-TO-TRANSLATOR.                                  ELTMHSC 
00977      PERFORM MOVE-PREFORMATTED-TEXT-TO-OUTP.                      ELTMHSC 
00978      SET PAYMENT-LVL-TRANSLATED TO TRUE.                          ELTMHSC 
00979      EJECT                                                        ELTMHSC 
00980                                                                   ELTMHSC 
00981                                                                   ELTMHSC 
00982 ************************************************************      ELTMHSC 
00983 *                                                          *      ELTMHSC 
00984 *        GENERATE BS ALT PRIC TEXT                         *      ELTMHSC 
00985 *                                                          *      ELTMHSC 
00986 ************************************************************      ELTMHSC 
00987  GENERATE-BS-ALT-PRIC-TEXT.                                       ELTMHSC 
00988      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMHSC 
00989      SET PERIOD-NEEDED TO TRUE.                                   ELTMHSC 
00990      INITIALIZE TCAR-FROM-AREA.                                   ELTMHSC 
00991      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMHSC 
00992      INITIALIZE WS-PERIOD-SW.                                     ELTMHSC 
00993      MOVE WS-ALTERNATE-PRICING  TO TCAR-FROM-LINE                 ELTMHSC 
00994          (TCAR-FROM-SUB).                                         ELTMHSC 
00995      ADD +1 TO TCAR-FROM-SUB.                                     ELTMHSC 
00996      SET ADDITIONAL-TEXT TO TRUE.                                 ELTMHSC 
00997      PERFORM TRANSLATE-BS-ALT-PRIC-METH.                          ELTMHSC 
00998                                                                   ELTMHSC 
00999                                                                   ELTMHSC 
01000 ************************************************************      ELTMHSC 
01001 *                                                          *      ELTMHSC 
01002 *        TRANSLATE BS ALT PRIC METH                        *      ELTMHSC 
01003 *                                                          *      ELTMHSC 
01004 ************************************************************      ELTMHSC 
01005  TRANSLATE-BS-ALT-PRIC-METH.                                      ELTMHSC 
01006      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMHSC 
01007      MOVE 'S1-BS-ALT-PRICING-METHOD' TO CMF-ELEMENT-SYSTEM-NAME.  ELTMHSC 
01008      MOVE  GSS-S1-BS-ALT-PRICING-METHOD (GSS-INDEX)               ELTMHSC 
01009                     TO CMF-CODE-VALUE.                            ELTMHSC 
01010      PERFORM LINK-TO-TRANSLATOR.                                  ELTMHSC 
01011      PERFORM MOVE-PREFORMATTED-TEXT-TO-OUTP.                      ELTMHSC 
01012                                                                   ELTMHSC 
01013                                                                   ELTMHSC 
01014 ************************************************************      ELTMHSC 
01015 *                                                          *      ELTMHSC 
01016 *        MOVE PREFORMATTED TEXT TO OUTPUT                  *      ELTMHSC 
01017 *                                                          *      ELTMHSC 
01018 ************************************************************      ELTMHSC 
01019  MOVE-PREFORMATTED-TEXT-TO-OUTP.                                  ELTMHSC 
01020      PERFORM DO-MOVE-OF-TEXT                                      ELTMHSC 
01021          VARYING WS-CMF-SUB FROM 1 BY 1 UNTIL WS-CMF-SUB          ELTMHSC 
01022                    > CMF-NBR-DESCR-LINES.                         ELTMHSC 
01023      PERFORM LINK-TO-OUTPUT.                                      ELTMHSC 
01024      EJECT                                                        ELTMHSC 
01025                                                                   ELTMHSC 
01026                                                                   ELTMHSC 
01027 ************************************************************      ELTMHSC 
01028 *                                                          *      ELTMHSC 
01029 *        DO MOVE OF TEXT                                   *      ELTMHSC 
01030 *                                                          *      ELTMHSC 
01031 ************************************************************      ELTMHSC 
01032  DO-MOVE-OF-TEXT.                                                 ELTMHSC 
01033      SET CMF-DESCR-IDX TO WS-CMF-SUB.                             ELTMHSC 
01034      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTMHSC 
01035          COF-DTL-LINE (COF-NBR-DTL-LINES).                        ELTMHSC 
01036      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMHSC 
01037                                                                   ELTMHSC 
01038                                                                   ELTMHSC 
01039 ************************************************************      ELTMHSC 
01040 *                                                          *      ELTMHSC 
01041 *        SETUP PAYMENT LEVEL FIXED TEXT                    *      ELTMHSC 
01042 *                                                          *      ELTMHSC 
01043 ************************************************************      ELTMHSC 
01044  SETUP-PAYMENT-LEVEL-FIXED-TEXT.                                  ELTMHSC 
01045      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMHSC 
01046      STRING WS-PAYMENT-LEVEL                                      ELTMHSC 
01047          DELIMITED BY SIZE INTO COF-DTL-LINE                      ELTMHSC 
01048          (COF-NBR-DTL-LINES).                                     ELTMHSC 
01049      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMHSC 
01050                                                                   ELTMHSC 
01051                                                                   ELTMHSC 
01052 ************************************************************      ELTMHSC 
01053 *                                                          *      ELTMHSC 
01054 *        GENERATE COINSURANCE TEXT                         *      ELTMHSC 
01055 *                                                          *      ELTMHSC 
01056 ************************************************************      ELTMHSC 
01057  GENERATE-COINSURANCE-TEXT.                                       ELTMHSC 
01058      EXEC CICS LINK                                               ELTMHSC 
01059          PROGRAM ('ELGACLCC')                                     ELTMHSC 
01060          COMMAREA (DFHCOMMAREA)                                   ELTMHSC 
01061          END-EXEC.                                                ELTMHSC 
01062                                                                   ELTMHSC 
01063 ************************************************************      ELTMHSC 
01064 *                                                          *      ELTMHSC 
01065 *        GENERATE COPAY       TEXT                         *      ELTMHSC 
01066 *                                                          *      ELTMHSC 
01067 ************************************************************      ELTMHSC 
01068  GENERATE-COPAY-TEXT.                                             ELTMHSC 
01069      EXEC CICS LINK                                               ELTMHSC 
01070          PROGRAM ('ELGACPCC')                                     ELTMHSC 
01071          COMMAREA (DFHCOMMAREA)                                   ELTMHSC 
01072          END-EXEC.                                                ELTMHSC 
01073                                                                   ELTMHSC 
01074                                                                   ELTMHSC 
01075 ************************************************************      ELTMHSC 
01076 *                                                          *      ELTMHSC 
01077 *        GENERATE DEDUCTIBLE TEXT                          *      ELTMHSC 
01078 *                                                          *      ELTMHSC 
01079 ************************************************************      ELTMHSC 
01080  GENERATE-DEDUCTIBLE-TEXT.                                        ELTMHSC 
01081      EXEC CICS LINK                                               ELTMHSC 
01082          PROGRAM ('ELGADLCC')                                     ELTMHSC 
01083          COMMAREA (DFHCOMMAREA)                                   ELTMHSC 
01084          END-EXEC.                                                ELTMHSC 
01085                                                                   ELTMHSC 
01086                                                                   ELTMHSC 
01087 ************************************************************      ELTMHSC 
01088 *                                                          *      ELTMHSC 
01089 *        GENERATE BENEFIT MAXIMUMS TEXT                    *      ELTMHSC 
01090 *                                                          *      ELTMHSC 
01091 ************************************************************      ELTMHSC 
01092  GENERATE-BENEFIT-MAXIMUMS-TEXT.                                  ELTMHSC 
01093      EXEC CICS LINK                                               ELTMHSC 
01094          PROGRAM ('ELGABMCC')                                     ELTMHSC 
01095          COMMAREA (DFHCOMMAREA)                                   ELTMHSC 
01096          END-EXEC.                                                ELTMHSC 
01097      EJECT                                                        ELTMHSC 
01098                                                                   ELTMHSC 
01099                                                                   ELTMHSC 
01100 ************************************************************      ELTMHSC 
01101 *                                                          *      ELTMHSC 
01102 *        GENERATE BC CALC METHOD                           *      ELTMHSC 
01103 *                                                          *      ELTMHSC 
01104 ************************************************************      ELTMHSC 
01105  GENERATE-BC-CALC-METHOD.                                         ELTMHSC 
01106      PERFORM GET-CALC-METHOD-FIXED-TEXT.                          ELTMHSC 
01107      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMHSC 
01108      MOVE 'S1-CALC-METHOD'  TO CMF-ELEMENT-SYSTEM-NAME.           ELTMHSC 
01109      MOVE GSS-S1-BC-CALC-METHOD (GSS-INDEX) TO                    ELTMHSC 
01110          CMF-CODE-VALUE.                                          ELTMHSC 
01111      PERFORM LINK-TO-TRANSLATOR.                                  ELTMHSC 
01112      PERFORM MOVE-PREFORMATTED-TEXT-TO-OUTP.                      ELTMHSC 
01113      EJECT                                                        ELTMHSC 
01114                                                                   ELTMHSC 
01115                                                                   ELTMHSC 
01116 ************************************************************      ELTMHSC 
01117 *                                                          *      ELTMHSC 
01118 *        GENERATE BS CALC METHOD                           *      ELTMHSC 
01119 *                                                          *      ELTMHSC 
01120 ************************************************************      ELTMHSC 
01121  GENERATE-BS-CALC-METHOD.                                         ELTMHSC 
01122      PERFORM GET-CALC-METHOD-FIXED-TEXT.                          ELTMHSC 
01123      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMHSC 
01124      MOVE 'S1-BS-CALC-METHOD'  TO CMF-ELEMENT-SYSTEM-NAME.        ELTMHSC 
01125      MOVE GSS-S1-BS-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMHSC 
01126      PERFORM LINK-TO-TRANSLATOR.                                  ELTMHSC 
01127      PERFORM MOVE-PREFORMATTED-TEXT-TO-OUTP.                      ELTMHSC 
01128      EJECT                                                        ELTMHSC 
01129                                                                   ELTMHSC 
01130                                                                   ELTMHSC 
01131 ************************************************************      ELTMHSC 
01132 *                                                          *      ELTMHSC 
01133 *        GENERATE MM CALC METHOD                           *      ELTMHSC 
01134 *                                                          *      ELTMHSC 
01135 ************************************************************      ELTMHSC 
01136  GENERATE-MM-CALC-METHOD.                                         ELTMHSC 
01137      PERFORM GET-CALC-METHOD-FIXED-TEXT.                          ELTMHSC 
01138      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMHSC 
01139      MOVE 'S1-MM-CALC-METHOD'  TO CMF-ELEMENT-SYSTEM-NAME.        ELTMHSC 
01140      MOVE GSS-S1-MM-CALC-METHOD (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMHSC 
01141      PERFORM LINK-TO-TRANSLATOR.                                  ELTMHSC 
01142      PERFORM MOVE-PREFORMATTED-TEXT-TO-OUTP.                      ELTMHSC 
01143                                                                   ELTMHSC 
01144                                                                   ELTMHSC 
01145 ************************************************************      ELTMHSC 
01146 *                                                          *      ELTMHSC 
01147 *        GET CALC METHOD FIXED TEXT                        *      ELTMHSC 
01148 *                                                          *      ELTMHSC 
01149 ************************************************************      ELTMHSC 
01150  GET-CALC-METHOD-FIXED-TEXT.                                      ELTMHSC 
01151      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMHSC 
01152      MOVE  WS-CALC-METHOD                                         ELTMHSC 
01153          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTMHSC 
01154      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMHSC 
01155      MOVE  WS-CALC-METHODA                                        ELTMHSC 
01156          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTMHSC 
01157      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMHSC 
01158      EJECT                                                        ELTMHSC 
01159                                                                   ELTMHSC 
01160                                                                   ELTMHSC 
01161 ************************************************************      ELTMHSC 
01162 *                                                          *      ELTMHSC 
01163 *        GENERATE COMBINED BENEFITS REDUCTION SENTENCE     *      ELTMHSC 
01164 *                                                          *      ELTMHSC 
01165 ************************************************************      ELTMHSC 
01166  GENERATE-COMBINED-BENEFITS-RED.                                  ELTMHSC 
01167      MOVE 'S1' TO SRP-COST-CONT-TYPE.                             ELTMHSC 
01168      MOVE 'MENTAL HEALTH SUBSTANCE ABUSE CARE PROGRAM'            ELTMHSC 
01169           TO SRP-CCP-NAME.                                        ELTMHSC 
01170      MOVE GSS-S1-COMB-BENE-REDUCT-IND (GSS-INDEX) TO              ELTMHSC 
01171           SRP-CCP-COMB-BENE-REDUCT-IND.                           ELTMHSC 
01172      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELTMHSC 
01173      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMHSC 
01174          ADDRESS OF GCCP-TABULAR-REC.                             ELTMHSC 
01175      CALL 'ELGCBRI' USING DFHEIBLK                                ELTMHSC 
01176                           DFHCOMMAREA.                            ELTMHSC 
01177      EJECT                                                        ELTMHSC 
01178                                                                   ELTMHSC 
01179                                                                   ELTMHSC 
01180 ************************************************************      ELTMHSC 
01181 *                                                          *      ELTMHSC 
01182 *        TRANSLATE BS INDICATOR                            *      ELTMHSC 
01183 *                                                          *      ELTMHSC 
01184 ************************************************************      ELTMHSC 
01185  TRANSLATE-BS-INDICATOR.                                          ELTMHSC 
01186      PERFORM GET-INDICATOR-FIXED-TEXT.                            ELTMHSC 
01187      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMHSC 
01188      MOVE 'S1-BS-IND' TO CMF-ELEMENT-SYSTEM-NAME.                 ELTMHSC 
01189      MOVE GSS-S1-BS-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTMHSC 
01190      PERFORM LINK-TO-TRANSLATOR.                                  ELTMHSC 
01191      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMHSC 
01192      EJECT                                                        ELTMHSC 
01193                                                                   ELTMHSC 
01194                                                                   ELTMHSC 
01195 ************************************************************      ELTMHSC 
01196 *                                                          *      ELTMHSC 
01197 *        TRANSLATE BS PAYMENT LEVEL INDICATOR              *      ELTMHSC 
01198 *                                                          *      ELTMHSC 
01199 ************************************************************      ELTMHSC 
01200  TRANSLATE-BS-PAYMENT-LEVEL-IND.                                  ELTMHSC 
01201      PERFORM SETUP-PAYMENT-LEVEL-FIXED-TEXT.                      ELTMHSC 
01202      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMHSC 
01203      MOVE 'S1-BS-PAYMENT-LEVEL-IND' TO                            ELTMHSC 
01204          CMF-ELEMENT-SYSTEM-NAME.                                 ELTMHSC 
01205      MOVE GSS-S1-BS-PAYMENT-LEVEL-IND (GSS-INDEX) TO              ELTMHSC 
01206          CMF-CODE-VALUE.                                          ELTMHSC 
01207      PERFORM LINK-TO-TRANSLATOR.                                  ELTMHSC 
01208      PERFORM MOVE-PREFORMATTED-TEXT-TO-OUTP.                      ELTMHSC 
01209      SET PAYMENT-LVL-TRANSLATED TO TRUE.                          ELTMHSC 
01210      EJECT                                                        ELTMHSC 
01211                                                                   ELTMHSC 
01212                                                                   ELTMHSC 
01213 ************************************************************      ELTMHSC 
01214 *                                                          *      ELTMHSC 
01215 *        TRANSLATE MM INDICATOR                            *      ELTMHSC 
01216 *                                                          *      ELTMHSC 
01217 ************************************************************      ELTMHSC 
01218  TRANSLATE-MM-INDICATOR.                                          ELTMHSC 
01219      PERFORM GET-INDICATOR-FIXED-TEXT.                            ELTMHSC 
01220      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMHSC 
01221      MOVE 'S1-MM-IND'  TO CMF-ELEMENT-SYSTEM-NAME.                ELTMHSC 
01222      MOVE GSS-S1-MM-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTMHSC 
01223      PERFORM LINK-TO-TRANSLATOR.                                  ELTMHSC 
01224      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMHSC 
01225      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMHSC 
01226      EJECT                                                        ELTMHSC 
01227                                                                   ELTMHSC 
01228                                                                   ELTMHSC 
01229 ************************************************************      ELTMHSC 
01230 *                                                          *      ELTMHSC 
01231 *        TRANSLATE MM PAYMENT LEVEL INDICATOR              *      ELTMHSC 
01232 *                                                          *      ELTMHSC 
01233 ************************************************************      ELTMHSC 
01234  TRANSLATE-MM-PAYMENT-LEVEL-IND.                                  ELTMHSC 
01235      PERFORM SETUP-PAYMENT-LEVEL-FIXED-TEXT.                      ELTMHSC 
01236      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMHSC 
01237      MOVE 'S1-MM-PAYMENT-LEVEL-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELTMHSC 
01238      MOVE GSS-S1-MM-PAYMENT-LEVEL-IND (GSS-INDEX)                 ELTMHSC 
01239                                   TO CMF-CODE-VALUE.              ELTMHSC 
01240      PERFORM LINK-TO-TRANSLATOR.                                  ELTMHSC 
01241      PERFORM MOVE-PREFORMATTED-TEXT-TO-OUTP.                      ELTMHSC 
01242      SET PAYMENT-LVL-TRANSLATED TO TRUE.                          ELTMHSC 
01243      EJECT                                                        ELTMHSC 
01244                                                                   ELTMHSC 
01245                                                                   ELTMHSC 
01246 ************************************************************      ELTMHSC 
01247 *                                                          *      ELTMHSC 
01248 *        GENERATE SPILL OVER INDICATOR                     *      ELTMHSC 
01249 *                                                          *      ELTMHSC 
01250 ************************************************************      ELTMHSC 
01251  GENERATE-SPILL-OVER-INDICATOR.                                   ELTMHSC 
01252      INITIALIZE ADDITIONAL-TEXT-SW.                               ELTMHSC 
01253      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMHSC 
01254      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMHSC 
01255      SET PERIOD-NEEDED TO TRUE.                                   ELTMHSC 
01256      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMHSC 
01257      MOVE WS-SPILLOVER-SENTENCE TO TCAR-FROM-LINE                 ELTMHSC 
01258          (TCAR-FROM-SUB).                                         ELTMHSC 
01259      ADD 1 TO TCAR-FROM-SUB.                                      ELTMHSC 
01260      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMHSC 
01261      MOVE 'S1-SPILL-OVER-IND' TO CMF-ELEMENT-SYSTEM-NAME.         ELTMHSC 
01262      MOVE GSS-S1-SPILL-OVER-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMHSC 
01263      PERFORM LINK-TO-TRANSLATOR.                                  ELTMHSC 
01264      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMHSC 
01265                                                                   ELTMHSC 
01266                                                                   ELTMHSC 
01267 ************************************************************      ELTMHSC 
01268 *                                                          *      ELTMHSC 
01269 *        SIGNAL VOLUNTARY MSG                              *      ELTMHSC 
01270 *                                                          *      ELTMHSC 
01271 ************************************************************      ELTMHSC 
01272  SIGNAL-VOLUNTARY-MSG.                                            ELTMHSC 
01273      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMHSC 
01274      MOVE WS-VOLUNTARY-MSG TO COF-DTL-LINE (COF-NBR-DTL-LINES).   ELTMHSC 
01275                                                                   ELTMHSC 
01276                                                                   ELTMHSC 
01277 ************************************************************      ELTMHSC 
01278 *                                                          *      ELTMHSC 
01279 *        SIGNAL NOT APPLICABLE MSG                         *      ELTMHSC 
01280 *                                                          *      ELTMHSC 
01281 ************************************************************      ELTMHSC 
01282  SIGNAL-NOT-APPLICABLE-MSG.                                       ELTMHSC 
01283      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMHSC 
01284      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTMHSC 
01285          (COF-NBR-DTL-LINES).                                     ELTMHSC 
01286                                                                   ELTMHSC 
01287                                                                   ELTMHSC 
01288 ************************************************************      ELTMHSC 
01289 *                                                          *      ELTMHSC 
01290 *        LINK TO TRANSLATOR                                *      ELTMHSC 
01291 *                                                          *      ELTMHSC 
01292 ************************************************************      ELTMHSC 
01293  LINK-TO-TRANSLATOR.                                              ELTMHSC 
01294      EXEC CICS LINK                                               ELTMHSC 
01295           PROGRAM('ELUCMIF')                                      ELTMHSC 
01296           COMMAREA(DFHCOMMAREA)                                   ELTMHSC 
01297           END-EXEC.                                               ELTMHSC 
01298      EJECT                                                        ELTMHSC 
01299                                                                   ELTMHSC 
01300                                                                   ELTMHSC 
01301 ************************************************************      ELTMHSC 
01302 *                                                          *      ELTMHSC 
01303 *        READ GCCP RECORD                                  *      ELTMHSC 
01304 *                                                          *      ELTMHSC 
01305 ************************************************************      ELTMHSC 
01306  READ-GCCP-RECORD.                                                ELTMHSC 
01307      SET CIA-GCTABULR-DDN TO TRUE.                                ELTMHSC 
01308      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMHSC 
01309          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTMHSC 
01310      SET IOP-RD TO TRUE.                                          ELTMHSC 
01311      SET IOP-FCQ-NONE TO TRUE.                                    ELTMHSC 
01312      SET IOP-KVQ-EQ TO TRUE.                                      ELTMHSC 
01313      SET IOP-STG-MODE-MOVE TO TRUE.                               ELTMHSC 
01314      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELTMHSC 
01315      PERFORM LINK-TO-I-O-PGM.                                     ELTMHSC 
01316      EJECT                                                        ELTMHSC 
01317                                                                   ELTMHSC 
01318                                                                   ELTMHSC 
01319 ************************************************************      ELTMHSC 
01320 *                                                          *      ELTMHSC 
01321 *        LINK TO I O PGM                                   *      ELTMHSC 
01322 *                                                          *      ELTMHSC 
01323 ************************************************************      ELTMHSC 
01324  LINK-TO-I-O-PGM.                                                 ELTMHSC 
01325      EXEC CICS LINK PROGRAM ('ELUIOPGM')                          ELTMHSC 
01326           COMMAREA (DFHCOMMAREA)                                  ELTMHSC 
01327           END-EXEC.                                               ELTMHSC 
01328      IF IOP-RC-OK                                                 ELTMHSC 
01329          PERFORM ESTABLISH-ADDRESSABILITY-OF-GC                   ELTMHSC 
01330      ELSE IF IOP-RC-NOTFND                                        ELTMHSC 
01331          PERFORM SIGNAL-NOT-FOUND-GCTAB                           ELTMHSC 
01332      ELSE                                                         ELTMHSC 
01333          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTMHSC 
01334                                                                   ELTMHSC 
01335                                                                   ELTMHSC 
01336 ************************************************************      ELTMHSC 
01337 *                                                          *      ELTMHSC 
01338 *        SIGNAL CRITICAL IO ERROR                          *      ELTMHSC 
01339 *                                                          *      ELTMHSC 
01340 ************************************************************      ELTMHSC 
01341  SIGNAL-CRITICAL-IO-ERROR.                                        ELTMHSC 
01342      SET CIA-AB-CRITIO TO TRUE.                                   ELTMHSC 
01343      PERFORM SIGNAL-ABEND.                                        ELTMHSC 
01344                                                                   ELTMHSC 
01345                                                                   ELTMHSC 
01346 ************************************************************      ELTMHSC 
01347 *                                                          *      ELTMHSC 
01348 *        SIGNAL NOT FOUND GCTAB                            *      ELTMHSC 
01349 *                                                          *      ELTMHSC 
01350 ************************************************************      ELTMHSC 
01351  SIGNAL-NOT-FOUND-GCTAB.                                          ELTMHSC 
01352      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTMHSC 
01353      PERFORM SIGNAL-ABEND.                                        ELTMHSC 
01354                                                                   ELTMHSC 
01355                                                                   ELTMHSC 
01356 ************************************************************      ELTMHSC 
01357 *                                                          *      ELTMHSC 
01358 *        ESTABLISH ADDRESSABILITY OF GCCP RECORD           *      ELTMHSC 
01359 *                                                          *      ELTMHSC 
01360 ************************************************************      ELTMHSC 
01361  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTMHSC 
01362      SET ADDRESS OF GCCP-TABULAR-REC TO IOP-REC-PTR.              ELTMHSC 
01363      SET IOP-REC-PTR TO NULL.                                     ELTMHSC 
01364      EJECT                                                        ELTMHSC 
01365                                                                   ELTMHSC 
01366                                                                   ELTMHSC 
01367 ************************************************************      ELTMHSC 
01368 *                                                          *      ELTMHSC 
01369 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTMHSC 
01370 *                                                          *      ELTMHSC 
01371 ************************************************************      ELTMHSC 
01372  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTMHSC 
01373      PERFORM INITIALIZE-CMOUT.                                    ELTMHSC 
01374      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTMHSC 
01375                                                                   ELTMHSC 
01376                                                                   ELTMHSC 
01377 ************************************************************      ELTMHSC 
01378 *                                                          *      ELTMHSC 
01379 *        PREPARE TEXT FOR OUTPUT                           *      ELTMHSC 
01380 *                                                          *      ELTMHSC 
01381 ************************************************************      ELTMHSC 
01382  PREPARE-TEXT-FOR-OUTPUT.                                         ELTMHSC 
01383      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTMHSC 
01384          UNTIL CMF-DESCR-IDX                                      ELTMHSC 
01385                                    GREATER THAN                   ELTMHSC 
01386              CMF-NBR-DESCR-LINES.                                 ELTMHSC 
01387                                                                   ELTMHSC 
01388                                                                   ELTMHSC 
01389 ************************************************************      ELTMHSC 
01390 *                                                          *      ELTMHSC 
01391 *        INITIALIZE CMOUT                                  *      ELTMHSC 
01392 *                                                          *      ELTMHSC 
01393 ************************************************************      ELTMHSC 
01394  INITIALIZE-CMOUT.                                                ELTMHSC 
01395      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTMHSC 
01396      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMHSC 
01397          ADDRESS OF CMF-DESCR.                                    ELTMHSC 
01398      SET CMF-DESCR-IDX TO 1.                                      ELTMHSC 
01399      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTMHSC 
01400                                                                   ELTMHSC 
01401                                                                   ELTMHSC 
01402 ************************************************************      ELTMHSC 
01403 *                                                          *      ELTMHSC 
01404 *        MOVE CMF TEXT TO OUTPUT                           *      ELTMHSC 
01405 *                                                          *      ELTMHSC 
01406 ************************************************************      ELTMHSC 
01407  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTMHSC 
01408      PERFORM MOVE-A-LINE.                                         ELTMHSC 
01409      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTMHSC 
01410          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTMHSC 
01411      IF TCAR-FROM-SUB GREATER THAN 20                             ELTMHSC 
01412               OR CMF-DESCR-IDX GREATER THAN                       ELTMHSC 
01413          CMF-NBR-DESCR-LINES                                      ELTMHSC 
01414          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTMHSC 
01415      EJECT                                                        ELTMHSC 
01416                                                                   ELTMHSC 
01417                                                                   ELTMHSC 
01418 ************************************************************      ELTMHSC 
01419 *                                                          *      ELTMHSC 
01420 *        FINISH CODES MANUAL TEXT                          *      ELTMHSC 
01421 *                                                          *      ELTMHSC 
01422 ************************************************************      ELTMHSC 
01423  FINISH-CODES-MANUAL-TEXT.                                        ELTMHSC 
01424      SET DONE-PROCESSING TO TRUE.                                 ELTMHSC 
01425      IF PERIOD-NEEDED                                             ELTMHSC 
01426          PERFORM GET-AND-MOVE-PERIOD.                             ELTMHSC 
01427                                                                   ELTMHSC 
01428                                                                   ELTMHSC 
01429 ************************************************************      ELTMHSC 
01430 *                                                          *      ELTMHSC 
01431 *        GET AND MOVE PERIOD                               *      ELTMHSC 
01432 *                                                          *      ELTMHSC 
01433 ************************************************************      ELTMHSC 
01434  GET-AND-MOVE-PERIOD.                                             ELTMHSC 
01435      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTMHSC 
01436          (TCAR-FROM-SUB).                                         ELTMHSC 
01437                                                                   ELTMHSC 
01438                                                                   ELTMHSC 
01439 ************************************************************      ELTMHSC 
01440 *                                                          *      ELTMHSC 
01441 *        SAVE LAST LINE                                    *      ELTMHSC 
01442 *                                                          *      ELTMHSC 
01443 ************************************************************      ELTMHSC 
01444  SAVE-LAST-LINE.                                                  ELTMHSC 
01445      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMHSC 
01446      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTMHSC 
01447         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTMHSC 
01448      ADD 1 TO TCAR-FROM-SUB.                                      ELTMHSC 
01449      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTMHSC 
01450                                                                   ELTMHSC 
01451                                                                   ELTMHSC 
01452 ************************************************************      ELTMHSC 
01453 *                                                          *      ELTMHSC 
01454 *        OUTPUT LAST LINE                                  *      ELTMHSC 
01455 *                                                          *      ELTMHSC 
01456 ************************************************************      ELTMHSC 
01457  OUTPUT-LAST-LINE.                                                ELTMHSC 
01458      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTMHSC 
01459          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTMHSC 
01460      IF BLANK-LINE-NEEDED                                         ELTMHSC 
01461          PERFORM CREATE-A-BLANK-LINE.                             ELTMHSC 
01462                                                                   ELTMHSC 
01463                                                                   ELTMHSC 
01464 ************************************************************      ELTMHSC 
01465 *                                                          *      ELTMHSC 
01466 *        CREATE A BLANK LINE                               *      ELTMHSC 
01467 *                                                          *      ELTMHSC 
01468 ************************************************************      ELTMHSC 
01469  CREATE-A-BLANK-LINE.                                             ELTMHSC 
01470      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTMHSC 
01471      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMHSC 
01472      EJECT                                                        ELTMHSC 
01473                                                                   ELTMHSC 
01474                                                                   ELTMHSC 
01475 ************************************************************      ELTMHSC 
01476 *                                                          *      ELTMHSC 
01477 *        MOVE A LINE                                       *      ELTMHSC 
01478 *                                                          *      ELTMHSC 
01479 ************************************************************      ELTMHSC 
01480  MOVE-A-LINE.                                                     ELTMHSC 
01481      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTMHSC 
01482          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTMHSC 
01483      SET CMF-DESCR-IDX UP BY 1.                                   ELTMHSC 
01484      ADD 1 TO TCAR-FROM-SUB.                                      ELTMHSC 
01485      EJECT                                                        ELTMHSC 
01486                                                                   ELTMHSC 
01487                                                                   ELTMHSC 
01488 ************************************************************      ELTMHSC 
01489 *                                                          *      ELTMHSC 
01490 *        REFORMAT AND WRITE TEXT                           *      ELTMHSC 
01491 *                                                          *      ELTMHSC 
01492 ************************************************************      ELTMHSC 
01493  REFORMAT-AND-WRITE-TEXT.                                         ELTMHSC 
01494      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTMHSC 
01495      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTMHSC 
01496      PERFORM UNSTRING-TEXT.                                       ELTMHSC 
01497      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMHSC 
01498      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMHSC 
01499      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTMHSC 
01500          UNTIL COF-NBR-DTL-LINES GREATER                          ELTMHSC 
01501                                   TCAR-OUTPUT-FIELDS-USED -       ELTMHSC 
01502              1.                                                   ELTMHSC 
01503      PERFORM DISPOSE-OF-LAST-LINE.                                ELTMHSC 
01504      PERFORM LINK-TO-OUTPUT.                                      ELTMHSC 
01505                                                                   ELTMHSC 
01506                                                                   ELTMHSC 
01507 ************************************************************      ELTMHSC 
01508 *                                                          *      ELTMHSC 
01509 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTMHSC 
01510 *                                                          *      ELTMHSC 
01511 ************************************************************      ELTMHSC 
01512  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTMHSC 
01513      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTMHSC 
01514           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTMHSC 
01515      ADD +1 TO TCAR-FROM-SUB.                                     ELTMHSC 
01516      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMHSC 
01517      EJECT                                                        ELTMHSC 
01518                                                                   ELTMHSC 
01519                                                                   ELTMHSC 
01520 ************************************************************      ELTMHSC 
01521 *                                                          *      ELTMHSC 
01522 *        UNSTRING TEXT                                     *      ELTMHSC 
01523 *                                                          *      ELTMHSC 
01524 ************************************************************      ELTMHSC 
01525  UNSTRING-TEXT.                                                   ELTMHSC 
01526      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTMHSC 
01527      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTMHSC 
01528      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTMHSC 
01529      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTMHSC 
01530      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTMHSC 
01531      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTMHSC 
01532      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTMHSC 
01533      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTMHSC 
01534      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTMHSC 
01535      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTMHSC 
01536      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTMHSC 
01537      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTMHSC 
01538      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTMHSC 
01539      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTMHSC 
01540      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTMHSC 
01541      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTMHSC 
01542      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTMHSC 
01543      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTMHSC 
01544      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTMHSC 
01545      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTMHSC 
01546      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTMHSC 
01547      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTMHSC 
01548      EJECT                                                        ELTMHSC 
01549                                                                   ELTMHSC 
01550                                                                   ELTMHSC 
01551 ************************************************************      ELTMHSC 
01552 *                                                          *      ELTMHSC 
01553 *        LINK TO OUTPUT                                    *      ELTMHSC 
01554 *                                                          *      ELTMHSC 
01555 ************************************************************      ELTMHSC 
01556  LINK-TO-OUTPUT.                                                  ELTMHSC 
01557      EXEC CICS LINK                                               ELTMHSC 
01558          PROGRAM ('ELUOUTPT')                                     ELTMHSC 
01559          COMMAREA (DFHCOMMAREA)                                   ELTMHSC 
01560          END-EXEC.                                                ELTMHSC 
01561      EJECT                                                        ELTMHSC 
01562                                                                   ELTMHSC 
01563                                                                   ELTMHSC 
01564 ************************************************************      ELTMHSC 
01565 *                                                          *      ELTMHSC 
01566 *        DISPOSE OF LAST LINE                              *      ELTMHSC 
01567 *                                                          *      ELTMHSC 
01568 ************************************************************      ELTMHSC 
01569  DISPOSE-OF-LAST-LINE.                                            ELTMHSC 
01570      IF NOT ADDITIONAL-TEXT                                       ELTMHSC 
01571          PERFORM INITIALIZE-CONTINUED-SW.                         ELTMHSC 
01572      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTMHSC 
01573          PERFORM SAVE-LAST-LINE                                   ELTMHSC 
01574      ELSE                                                         ELTMHSC 
01575          PERFORM OUTPUT-LAST-LINE.                                ELTMHSC 
01576                                                                   ELTMHSC 
01577                                                                   ELTMHSC 
01578 ************************************************************      ELTMHSC 
01579 *                                                          *      ELTMHSC 
01580 *        INITIALIZE CONTINUED SW                           *      ELTMHSC 
01581 *                                                          *      ELTMHSC 
01582 ************************************************************      ELTMHSC 
01583  INITIALIZE-CONTINUED-SW.                                         ELTMHSC 
01584      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTMHSC 
