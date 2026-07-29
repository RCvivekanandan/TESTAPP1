00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELUCONDB
00003  PROGRAM-ID.         ELUCONDB.                                       LV002
00004                                                                   ELUCONDB
00005  AUTHOR.             NINA CERVANTES.                              ELUCONDB
00006                                                                   ELUCONDB
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELUCONDB
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELUCONDB
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELUCONDB
00010                      233 N. MICHIGAN AVE                          ELUCONDB
00011                      CHICAGO, ILLINOIS 60601                      ELUCONDB
00012                                                                   ELUCONDB
00013  DATE-WRITTEN.       24-OCT-1986.                                 ELUCONDB
00014                                                                   ELUCONDB
00015  DATE-COMPILED.                                                   ELUCONDB
00016                                                                   ELUCONDB
00017  SECURITY.           COPYRIGHT 1986,                              ELUCONDB
00018                      HEALTH CARE SERVICE CORPORATION.             ELUCONDB
00019      SKIP3                                                        ELUCONDB
00020 ******************************************************************ELUCONDB
00021 *                                                                *ELUCONDB
00022 *    PROGRAM:    ELUCONDB                                        *ELUCONDB
00023 *    DATE:       24-OCT-1986                                     *ELUCONDB
00024 *    AUTHOR:     NINA CERVANTES                                  *ELUCONDB
00025 *    FUNCTION:   TRANSLATE ACCUMULATOR CONDITION BITS INTO       *ELUCONDB
00026 *                APPROPRIATE ENGLISH PHRASE FOR DISPLAY.         *ELUCONDB
00027 *                                                                *ELUCONDB
00028 *    NOTES:      LOGIC SIGNIFICANTLY REVISED AND CONVERTED FROM  *ELUCONDB
00029 *                STRUCTURE(S) CODE GENERATOR JULY 1989.          *ELUCONDB
00030 *                                                                *ELUCONDB
00031 ******************************************************************ELUCONDB
00032 *                                                                *ELUCONDB
00033 *                      MAINTENANCE HISTORY                       *ELUCONDB
00034 *                                                                *ELUCONDB
00035 *  MOD     DATE     BY  DRPT                ACTION               *ELUCONDB
00036 * ----- ----------- --- ----- ---------------------------------- *ELUCONDB
00037 *                                                                *ELUCONDB
00038 * 01.01 10-APR-1987 NAC       APPEND \
00039 *                             TRANSLATIONS.                      *ELUCONDB
00040 *                                                                *ELUCONDB
00041 * 01.02 15-OCT-1987 NAC       ADDED SUICIDE CONDITION BIT.       *ELUCONDB
00042 *                                                                *ELUCONDB
00043 * 01.03 14-SEP-1988 NAC       REMOVE GETMAIN AND FREEMAN FOR     *ELUCONDB
00044 *                             CODES MANUAL AREA - CAUSES STORAGE *ELUCONDB
00045 *                             VIOLATION IN PROD.  INCLUDE        *ELUCONDB
00046 *                             STORAGE MANAGEMENT ENHANCEMENTS.   *ELUCONDB
00047 *                                                                *ELUCONDB
00048 * 02.00 15-JUN-1989 RJL       CONVERTED FROM STRUCTURE(S) TO     *ELUCONDB
00049 *                             DIRECT VS COBOL II.  ADDED         *ELUCONDB
00050 *                             CONDITION BITS TMJ AND INF.        *ELUCONDB
00051 *                                                                *ELUCONDB
00052 * 02.01 18-MAR-1992 BAK       ADD LIFE THREATENING CONDITON BIT  *ELUCONDB
00053 *                                                                *ELUCONDB
00054 * 02.02 06-JAN-2003 AKK       ADD SERIOUS METNAL BIT AND NON-    *ELUCONDB
00055 *                             SERIOUS MENTAL BITS.               *ELUCONDB
00056 *                                                                *ELUCONDB
00057 * 02.03 08-JAN-2003 AKK       ADDED EMC EAC TO LIST AS TYH NEED  *ELUCONDB
00058 *                             TO BE IN PLACE FOR SMI AND NSMI TO *ELUCONDB
00059 *                             BE FOUND..... BUT NO DISPLAY OF    *ELUCONDB
00060 *                             EMC AND EAC NEED TO BE DONE.       *ELUCONDB
00061 ******************************************************************ELUCONDB
00062      EJECT                                                        ELUCONDB
00063  ENVIRONMENT DIVISION.                                            ELUCONDB
00064                                                                   ELUCONDB
00065  CONFIGURATION SECTION.                                           ELUCONDB
00066                                                                   ELUCONDB
00067  SOURCE-COMPUTER.    IBM-3033.                                    ELUCONDB
00068  OBJECT-COMPUTER.    IBM-3033.                                    ELUCONDB
00069      EJECT                                                        ELUCONDB
00070  DATA DIVISION.                                                   ELUCONDB
00071                                                                   ELUCONDB
00072  WORKING-STORAGE SECTION.                                         ELUCONDB
00073                                                                   ELUCONDB
00074  01  WS-BEGIN                    PICTURE  X(36) VALUE             ELUCONDB
00075      'ELCONDBT WORKING STORAGE BEGINS HERE'.                      ELUCONDB
00076                                                                   ELUCONDB
00077  01  SUB1                        PICTURE S9(4) COMP.              ELUCONDB
00078                                                                   ELUCONDB
00079  01  WS-CONDITION-BITS.                                           ELUCONDB
00080      02 FILLER                   PICTURE X(3).                    ELUCONDB
00081         88 COND-SPECIFIC         VALUE '000'.                     ELUCONDB
00082         88 COND-ALL              VALUE '100'.                     ELUCONDB
00083         88 COND-ALL-EXC          VALUE '110'.                     ELUCONDB
00084         88 COND-ICD              VALUE '001', '101'.              ELUCONDB
00085         88 COND-ICD-EXC          VALUE '011', '111'.              ELUCONDB
00086      02 FILLER                   PICTURE  X(1).                   ELUCONDB
00087         88 COND-TB               VALUE '1'.                       ELUCONDB
00088      02 FILLER                   PICTURE  X(1).                   ELUCONDB
00089         88 COND-MEN              VALUE '1'.                       ELUCONDB
00090      02 FILLER                   PICTURE  X(1).                   ELUCONDB
00091         88 COND-DRG              VALUE '1'.                       ELUCONDB
00092      02 FILLER                   PICTURE  X(1).                   ELUCONDB
00093         88 COND-ALC              VALUE '1'.                       ELUCONDB
00094      02 FILLER                   PICTURE  X(1).                   ELUCONDB
00095         88 COND-OBC              VALUE '1'.                       ELUCONDB
00096      02 FILLER                   PICTURE  X(1).                   ELUCONDB
00097         88 COND-OBN              VALUE '1'.                       ELUCONDB
00098      02 FILLER                   PICTURE  X(1).                   ELUCONDB
00099         88 COND-MAL              VALUE '1'.                       ELUCONDB
00100      02 FILLER                   PICTURE  X(1).                   ELUCONDB
00101         88 COND-CAR              VALUE '1'.                       ELUCONDB
00102      02 FILLER                   PICTURE  X(1).                   ELUCONDB
00103         88 COND-OBS              VALUE '1'.                       ELUCONDB
00104      02 FILLER                   PICTURE  X(1).                   ELUCONDB
00105         88 COND-KID              VALUE '1'.                       ELUCONDB
00106      02 FILLER                   PICTURE  X(1).                   ELUCONDB
00107         88 COND-ACC              VALUE '1'.                       ELUCONDB
00108      02 FILLER                   PICTURE  X(1).                   ELUCONDB
00109         88 COND-PEC              VALUE '1'.                       ELUCONDB
00110      02 FILLER                   PICTURE  X(1).                   ELUCONDB
00111         88 COND-NEM              VALUE '1'.                       ELUCONDB
00112      02 FILLER                   PICTURE  X(1).                   ELUCONDB
00113         88 COND-SUI              VALUE '1'.                       ELUCONDB
00114      02 FILLER                   PICTURE  X(1).                   ELUCONDB
00115         88 COND-TMJ              VALUE '1'.                       ELUCONDB
00116      02 FILLER                   PICTURE  X(1).                   ELUCONDB
00117         88 COND-INF              VALUE '1'.                       ELUCONDB
00118      02 FILLER                   PICTURE  X(1).                   ELUCONDB
00119         88 COND-LIF              VALUE '1'.                       ELUCONDB
00120      02 FILLER                   PICTURE  X(1).                   ELUCONDB
00121         88 COND-EMC              VALUE '1'.                       ELUCONDB
00122      02 FILLER                   PICTURE  X(1).                   ELUCONDB
00123         88 COND-EAC              VALUE '1'.                       ELUCONDB
00124      02 FILLER                   PICTURE  X(1).                   ELUCONDB
00125         88 COND-SMI              VALUE '1'.                       ELUCONDB
00126      02 FILLER                   PICTURE  X(1).                   ELUCONDB
00127         88 COND-NSMI             VALUE '1'.                       ELUCONDB
00128      02 FILLER                   PICTURE  X(10).                  ELUCONDB
00129         88 COND-ZZZ              VALUE '1         '.              ELUCONDB
00130      EJECT                                                        ELUCONDB
00131  01  WS-DESCRIPTIONS.                                             ELUCONDB
00132      02 WS-DESC-ALL              PICTURE  X(14)                   ELUCONDB
00133                                  VALUE  'ALL CONDITIONS'.         ELUCONDB
00134      02 WS-DESC-ALL-EXC          PICTURE  X(22)                   ELUCONDB
00135                                  VALUE  'ALL CONDITIONS EXCEPT '. ELUCONDB
00136      02 WS-DESC-ICD              PICTURE  X(43)                   ELUCONDB
00137                                  VALUE  'EACH ILLNESS OR ACCIDENT ELUCONDB
00138 -                                       'CONDITION'.              ELUCONDB
00139      02 WS-DESC-ICD-EXC          PICTURE  X(51)                   ELUCONDB
00140                                  VALUE  'EACH ILLNESS OR ACCIDENT ELUCONDB
00141 -                                       'CONDITION EXCEPT '.      ELUCONDB
00142      02 WS-DESC-TB               PICTURE  X(14)                   ELUCONDB
00143                                  VALUE '**TUBERCULOSIS'.          ELUCONDB
00144      02 WS-DESC-MEN              PICTURE  X(08)                   ELUCONDB
00145                                  VALUE  '**MENTAL'.               ELUCONDB
00146      02 WS-DESC-DRG              PICTURE  X(06)                   ELUCONDB
00147                                  VALUE  '**DRUG'.                 ELUCONDB
00148      02 WS-DESC-ALC              PICTURE  X(09)                   ELUCONDB
00149                                  VALUE  '**ALCOHOL'.              ELUCONDB
00150      02 WS-DESC-OBC              PICTURE  X(23)                   ELUCONDB
00151                                  VALUE '**COMPLICATED OBSTETRIC'. ELUCONDB
00152      02 WS-DESC-OBN              PICTURE  X(18)                   ELUCONDB
00153                                  VALUE '**NORMAL OBSTETRIC'.      ELUCONDB
00154      02 WS-DESC-MAL              PICTURE  X(12)                   ELUCONDB
00155                                  VALUE '**MALIGNANCY'.            ELUCONDB
00156      02 WS-DESC-CAR              PICTURE  X(17)                   ELUCONDB
00157                                  VALUE '**CARDIAC DISEASE'.       ELUCONDB
00158      02 WS-DESC-OBS              PICTURE  X(09)                   ELUCONDB
00159                                  VALUE '**OBESITY'.               ELUCONDB
00160      02 WS-DESC-KID              PICTURE  X(16)                   ELUCONDB
00161                                  VALUE '**KIDNEY DISEASE'.        ELUCONDB
00162      02 WS-DESC-ACC              PICTURE  X(10)                   ELUCONDB
00163                                  VALUE '**ACCIDENT'.              ELUCONDB
00164      02 WS-DESC-PEC              PICTURE  X(14)                   ELUCONDB
00165                                  VALUE  '**PRE-EXISTING'.         ELUCONDB
00166      02 WS-DESC-NEM              PICTURE  X(15)                   ELUCONDB
00167                                  VALUE  '**NON-EMERGENCY'.        ELUCONDB
00168      02 WS-DESC-SUI              PICTURE  X(09)                   ELUCONDB
00169                                  VALUE  '**SUICIDE'.              ELUCONDB
00170      02 WS-DESC-TMJ              PICTURE  X(25)                   ELUCONDB
00171                                  VALUE  '**TEMPRO-MANDIBULAR JOINTELUCONDB
00172 -                                       ''.                       ELUCONDB
00173      02 WS-DESC-INF              PICTURE  X(13)                   ELUCONDB
00174                                  VALUE  '**INFERTILITY'.          ELUCONDB
00175                                                                   ELUCONDB
00176      02 WS-DESC-LIF              PICTURE  X(18)                   ELUCONDB
00177                                  VALUE  '**LIFE THREATENING'.     ELUCONDB
00178                                                                   ELUCONDB
00179      02 WS-DESC-SMI              PICTURE  X(24)                   ELUCONDB
00180                       VALUE  '**SERIOUS MENTAL ILLNESS'.          ELUCONDB
00181                                                                   ELUCONDB
00182      02 WS-DESC-NSMI             PICTURE  X(28)                   ELUCONDB
00183                       VALUE  '**NON-SERIOUS MENTAL ILLNESS'.      ELUCONDB
00184                                                                   ELUCONDB
00185  01  WS-ERROR-MSG                PIC X(67)                        ELUCONDB
00186                                  VALUE  'UNABLE TO TRANSLATE CONDIELUCONDB
00187 -                                       'TION BITS.  CONTACT SSD TELUCONDB
00188 -                                       'ECHNICAL SUPPORT.'.      ELUCONDB
00189      EJECT                                                        ELUCONDB
00190  LINKAGE SECTION.                                                 ELUCONDB
00191                                                                   ELUCONDB
00192  01  DFHCOMMAREA.                                                 ELUCONDB
00193      COPY ELSCOMMC.                                               ELUCONDB
00194      EJECT                                                        ELUCONDB
00195      COPY ELSCIA2C.                                               ELUCONDB
00196      EJECT                                                        ELUCONDB
00197      COPY ELSSSCBC.                                               ELUCONDB
00198      EJECT                                                        ELUCONDB
00199      COPY ELSCMIFC.                                               ELUCONDB
00200      EJECT                                                        ELUCONDB
00201      COPY ELSCMDSC.                                               ELUCONDB
00202      EJECT                                                        ELUCONDB
00203      COPY ELSTCWAC.                                               ELUCONDB
00204      EJECT                                                        ELUCONDB
00205 ************************************************************      ELUCONDB
00206 *                                                          *      ELUCONDB
00207 *        CONDITION BITS MAINLINE                           *      ELUCONDB
00208 *                                                          *      ELUCONDB
00209 ************************************************************      ELUCONDB
00210                                                                   ELUCONDB
00211  PROCEDURE DIVISION.                                              ELUCONDB
00212                                                                   ELUCONDB
00213  000-CONDITION-BITS-MAINLINE.                                     ELUCONDB
00214      PERFORM 001-INITIALIZE.                                      ELUCONDB
00215      PERFORM 100-PROCESS.                                         ELUCONDB
00216      GOBACK.                                                      ELUCONDB
00217                                                                   ELUCONDB
00218                                                                   ELUCONDB
00219 ************************************************************      ELUCONDB
00220 *                                                          *      ELUCONDB
00221 *        INITIALIZE                                        *      ELUCONDB
00222 *                                                          *      ELUCONDB
00223 ************************************************************      ELUCONDB
00224                                                                   ELUCONDB
00225  001-INITIALIZE.                                                  ELUCONDB
00226      PERFORM 901-CHECK-COMMAREA-LENGTH.                           ELUCONDB
00227      PERFORM 911-EST-ADDR-CIA.                                    ELUCONDB
00228      PERFORM 912-ESTAB-ADDR-ELSSSCB.                              ELUCONDB
00229      PERFORM 913-ESTAB-ADDR-ELSCMIF.                              ELUCONDB
00230      PERFORM 914-ESTAB-ADDR-ELSCMDS.                              ELUCONDB
00231      PERFORM 915-ESTAB-ADDR-ELSTCWA.                              ELUCONDB
00232                                                                   ELUCONDB
00233      INITIALIZE TCAR-FROM-AREA.                                   ELUCONDB
00234                                                                   ELUCONDB
00235                                                                   ELUCONDB
00236 ************************************************************      ELUCONDB
00237 *                                                          *      ELUCONDB
00238 *        PROCESS                                           *      ELUCONDB
00239 *                                                          *      ELUCONDB
00240 ************************************************************      ELUCONDB
00241                                                                   ELUCONDB
00242  100-PROCESS.                                                     ELUCONDB
00243      MOVE CMF-CONDITION-BITS  TO  WS-CONDITION-BITS.              ELUCONDB
00244      EVALUATE TRUE                                                ELUCONDB
00245         WHEN COND-ALL                                             ELUCONDB
00246            PERFORM 101-XLAT-ALL                                   ELUCONDB
00247         WHEN COND-ALL-EXC                                         ELUCONDB
00248            PERFORM 102-XLAT-ALL-EXC                               ELUCONDB
00249            PERFORM 110-XLAT-SPECIFICS                             ELUCONDB
00250         WHEN COND-ICD                                             ELUCONDB
00251            PERFORM 103-XLAT-ICD                                   ELUCONDB
00252         WHEN COND-ICD-EXC                                         ELUCONDB
00253            PERFORM 104-XLAT-ICD-EXC                               ELUCONDB
00254            PERFORM 110-XLAT-SPECIFICS                             ELUCONDB
00255         WHEN OTHER                                                ELUCONDB
00256            PERFORM 110-XLAT-SPECIFICS.                            ELUCONDB
00257      EJECT                                                        ELUCONDB
00258 ************************************************************      ELUCONDB
00259 *                                                          *      ELUCONDB
00260 *        TRANSLATE ALL CONDITIONS PHRASE                   *      ELUCONDB
00261 *                                                          *      ELUCONDB
00262 ************************************************************      ELUCONDB
00263                                                                   ELUCONDB
00264  101-XLAT-ALL.                                                    ELUCONDB
00265      MOVE 1 TO CMF-NBR-DESCR-LINES.                               ELUCONDB
00266      MOVE WS-DESC-ALL TO CMF-DESCR-LINE (1).                      ELUCONDB
00267                                                                   ELUCONDB
00268                                                                   ELUCONDB
00269 ************************************************************      ELUCONDB
00270 *                                                          *      ELUCONDB
00271 *        ALL CONDITIONS EXCEPT PHRASE                      *      ELUCONDB
00272 *                                                          *      ELUCONDB
00273 ************************************************************      ELUCONDB
00274                                                                   ELUCONDB
00275  102-XLAT-ALL-EXC.                                                ELUCONDB
00276      MOVE WS-DESC-ALL-EXC TO TCAR-FROM-AREA.                      ELUCONDB
00277                                                                   ELUCONDB
00278                                                                   ELUCONDB
00279 ************************************************************      ELUCONDB
00280 *                                                          *      ELUCONDB
00281 *        TRANSLATE PER ILLNES OR ACCIDENT CONDITION PHRASE *      ELUCONDB
00282 *                                                          *      ELUCONDB
00283 ************************************************************      ELUCONDB
00284                                                                   ELUCONDB
00285  103-XLAT-ICD.                                                    ELUCONDB
00286      MOVE 1 TO CMF-NBR-DESCR-LINES.                               ELUCONDB
00287      MOVE WS-DESC-ICD TO CMF-DESCR-LINE (1).                      ELUCONDB
00288                                                                   ELUCONDB
00289                                                                   ELUCONDB
00290 ************************************************************      ELUCONDB
00291 *                                                          *      ELUCONDB
00292 *        TRANSLATE PER ILLNESS OR ACCIDENT CONDITION       *      ELUCONDB
00293 *        EXCEPT PHRASE                                     *      ELUCONDB
00294 *                                                          *      ELUCONDB
00295 ************************************************************      ELUCONDB
00296                                                                   ELUCONDB
00297  104-XLAT-ICD-EXC.                                                ELUCONDB
00298      MOVE WS-DESC-ICD-EXC TO TCAR-FROM-AREA.                      ELUCONDB
00299                                                                   ELUCONDB
00300                                                                   ELUCONDB
00301 ************************************************************      ELUCONDB
00302 *                                                          *      ELUCONDB
00303 *        PROCESS BIT CONT                                  *      ELUCONDB
00304 *                                                          *      ELUCONDB
00305 ************************************************************      ELUCONDB
00306                                                                   ELUCONDB
00307  110-XLAT-SPECIFICS.                                              ELUCONDB
00308      IF COND-TB  THEN PERFORM 201-XLAT-TB.                        ELUCONDB
00309      IF COND-MEN THEN PERFORM 202-XLAT-MEN.                       ELUCONDB
00310      IF COND-DRG THEN PERFORM 203-XLAT-DRG.                       ELUCONDB
00311      IF COND-ALC THEN PERFORM 204-XLAT-ALC.                       ELUCONDB
00312      IF COND-OBC THEN PERFORM 205-XLAT-OBC.                       ELUCONDB
00313      IF COND-OBN THEN PERFORM 206-XLAT-OBN.                       ELUCONDB
00314      IF COND-MAL THEN PERFORM 207-XLAT-MAL.                       ELUCONDB
00315      IF COND-CAR THEN PERFORM 208-XLAT-CAR.                       ELUCONDB
00316      IF COND-OBS THEN PERFORM 209-XLAT-OBS.                       ELUCONDB
00317      IF COND-KID THEN PERFORM 210-XLAT-KID.                       ELUCONDB
00318      IF COND-ACC THEN PERFORM 211-XLAT-ACC.                       ELUCONDB
00319      IF COND-PEC THEN PERFORM 212-XLAT-PEC.                       ELUCONDB
00320      IF COND-NEM THEN PERFORM 213-XLAT-NEM.                       ELUCONDB
00321      IF COND-SUI THEN PERFORM 214-XLAT-SUI.                       ELUCONDB
00322      IF COND-TMJ THEN PERFORM 215-XLAT-TMJ.                       ELUCONDB
00323      IF COND-INF THEN PERFORM 216-XLAT-INF.                       ELUCONDB
00324      IF COND-LIF THEN PERFORM 217-XLAT-LIF.                       ELUCONDB
00325      IF COND-SMI THEN PERFORM 218-XLAT-SMI.                       ELUCONDB
00326      IF COND-NSMI THEN PERFORM 219-XLAT-NSMI.                     ELUCONDB
00327                                                                   ELUCONDB
00328      PERFORM 300-COMPRESS-OUTPUT-TEXT.                            ELUCONDB
00329      EJECT                                                        ELUCONDB
00330 ************************************************************      ELUCONDB
00331 *                                                          *      ELUCONDB
00332 *        TRANSLATE TUBERCULOSIS CONDITION BIT              *      ELUCONDB
00333 *                                                          *      ELUCONDB
00334 ************************************************************      ELUCONDB
00335                                                                   ELUCONDB
00336  201-XLAT-TB.                                                     ELUCONDB
00337      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELUCONDB
00338         WS-DESC-TB  DELIMITED BY SIZE INTO TCAR-FROM-AREA.        ELUCONDB
00339                                                                   ELUCONDB
00340                                                                   ELUCONDB
00341 ************************************************************      ELUCONDB
00342 *                                                          *      ELUCONDB
00343 *        MENTAL                                            *      ELUCONDB
00344 *                                                          *      ELUCONDB
00345 ************************************************************      ELUCONDB
00346                                                                   ELUCONDB
00347  202-XLAT-MEN.                                                    ELUCONDB
00348      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELUCONDB
00349         WS-DESC-MEN DELIMITED BY SIZE INTO TCAR-FROM-AREA.        ELUCONDB
00350                                                                   ELUCONDB
00351                                                                   ELUCONDB
00352 ************************************************************      ELUCONDB
00353 *                                                          *      ELUCONDB
00354 *        DRUGS                                             *      ELUCONDB
00355 *                                                          *      ELUCONDB
00356 ************************************************************      ELUCONDB
00357                                                                   ELUCONDB
00358  203-XLAT-DRG.                                                    ELUCONDB
00359      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELUCONDB
00360         WS-DESC-DRG DELIMITED BY SIZE INTO TCAR-FROM-AREA.        ELUCONDB
00361                                                                   ELUCONDB
00362                                                                   ELUCONDB
00363 ************************************************************      ELUCONDB
00364 *                                                          *      ELUCONDB
00365 *        ALCOHOL                                           *      ELUCONDB
00366 *                                                          *      ELUCONDB
00367 ************************************************************      ELUCONDB
00368                                                                   ELUCONDB
00369  204-XLAT-ALC.                                                    ELUCONDB
00370      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELUCONDB
00371         WS-DESC-ALC DELIMITED BY SIZE INTO TCAR-FROM-AREA.        ELUCONDB
00372                                                                   ELUCONDB
00373                                                                   ELUCONDB
00374 ************************************************************      ELUCONDB
00375 *                                                          *      ELUCONDB
00376 *        COMPLICATED OB                                    *      ELUCONDB
00377 *                                                          *      ELUCONDB
00378 ************************************************************      ELUCONDB
00379                                                                   ELUCONDB
00380  205-XLAT-OBC.                                                    ELUCONDB
00381      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELUCONDB
00382         WS-DESC-OBC DELIMITED BY SIZE INTO TCAR-FROM-AREA.        ELUCONDB
00383                                                                   ELUCONDB
00384                                                                   ELUCONDB
00385 ************************************************************      ELUCONDB
00386 *                                                          *      ELUCONDB
00387 *        NORMAL OB                                         *      ELUCONDB
00388 *                                                          *      ELUCONDB
00389 ************************************************************      ELUCONDB
00390                                                                   ELUCONDB
00391  206-XLAT-OBN.                                                    ELUCONDB
00392      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELUCONDB
00393         WS-DESC-OBN DELIMITED BY SIZE INTO TCAR-FROM-AREA.        ELUCONDB
00394                                                                   ELUCONDB
00395                                                                   ELUCONDB
00396 ************************************************************      ELUCONDB
00397 *                                                          *      ELUCONDB
00398 *        MALIGNANCY                                        *      ELUCONDB
00399 *                                                          *      ELUCONDB
00400 ************************************************************      ELUCONDB
00401                                                                   ELUCONDB
00402  207-XLAT-MAL.                                                    ELUCONDB
00403      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELUCONDB
00404         WS-DESC-MAL DELIMITED BY SIZE INTO TCAR-FROM-AREA.        ELUCONDB
00405                                                                   ELUCONDB
00406                                                                   ELUCONDB
00407 ************************************************************      ELUCONDB
00408 *                                                          *      ELUCONDB
00409 *        CARDIAC DISEASE                                   *      ELUCONDB
00410 *                                                          *      ELUCONDB
00411 ************************************************************      ELUCONDB
00412                                                                   ELUCONDB
00413  208-XLAT-CAR.                                                    ELUCONDB
00414      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELUCONDB
00415         WS-DESC-CAR DELIMITED BY SIZE INTO TCAR-FROM-AREA.        ELUCONDB
00416                                                                   ELUCONDB
00417                                                                   ELUCONDB
00418 ************************************************************      ELUCONDB
00419 *                                                          *      ELUCONDB
00420 *        OBESITY                                           *      ELUCONDB
00421 *                                                          *      ELUCONDB
00422 ************************************************************      ELUCONDB
00423                                                                   ELUCONDB
00424  209-XLAT-OBS.                                                    ELUCONDB
00425      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELUCONDB
00426         WS-DESC-OBS DELIMITED BY SIZE INTO TCAR-FROM-AREA.        ELUCONDB
00427                                                                   ELUCONDB
00428                                                                   ELUCONDB
00429 ************************************************************      ELUCONDB
00430 *                                                          *      ELUCONDB
00431 *        KIDNEY DISEASE                                    *      ELUCONDB
00432 *                                                          *      ELUCONDB
00433 ************************************************************      ELUCONDB
00434                                                                   ELUCONDB
00435  210-XLAT-KID.                                                    ELUCONDB
00436      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELUCONDB
00437         WS-DESC-KID DELIMITED BY SIZE INTO TCAR-FROM-AREA.        ELUCONDB
00438                                                                   ELUCONDB
00439                                                                   ELUCONDB
00440 ************************************************************      ELUCONDB
00441 *                                                          *      ELUCONDB
00442 *        ACCIDENT                                          *      ELUCONDB
00443 *                                                          *      ELUCONDB
00444 ************************************************************      ELUCONDB
00445                                                                   ELUCONDB
00446  211-XLAT-ACC.                                                    ELUCONDB
00447      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELUCONDB
00448         WS-DESC-ACC DELIMITED BY SIZE INTO TCAR-FROM-AREA.        ELUCONDB
00449                                                                   ELUCONDB
00450                                                                   ELUCONDB
00451 ************************************************************      ELUCONDB
00452 *                                                          *      ELUCONDB
00453 *        PRE EXISTING                                      *      ELUCONDB
00454 *                                                          *      ELUCONDB
00455 ************************************************************      ELUCONDB
00456                                                                   ELUCONDB
00457  212-XLAT-PEC.                                                    ELUCONDB
00458      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELUCONDB
00459         WS-DESC-PEC DELIMITED BY SIZE INTO TCAR-FROM-AREA.        ELUCONDB
00460                                                                   ELUCONDB
00461                                                                   ELUCONDB
00462 ************************************************************      ELUCONDB
00463 *                                                          *      ELUCONDB
00464 *        NON EMERGENCY                                     *      ELUCONDB
00465 *                                                          *      ELUCONDB
00466 ************************************************************      ELUCONDB
00467                                                                   ELUCONDB
00468  213-XLAT-NEM.                                                    ELUCONDB
00469      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELUCONDB
00470         WS-DESC-NEM DELIMITED BY SIZE INTO TCAR-FROM-AREA.        ELUCONDB
00471                                                                   ELUCONDB
00472                                                                   ELUCONDB
00473 ************************************************************      ELUCONDB
00474 *                                                          *      ELUCONDB
00475 *        TRANSLATE SUICIDE CONDITION BIT                   *      ELUCONDB
00476 *                                                          *      ELUCONDB
00477 ************************************************************      ELUCONDB
00478                                                                   ELUCONDB
00479  214-XLAT-SUI.                                                    ELUCONDB
00480      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELUCONDB
00481         WS-DESC-SUI DELIMITED BY SIZE INTO TCAR-FROM-AREA.        ELUCONDB
00482                                                                   ELUCONDB
00483                                                                   ELUCONDB
00484 ************************************************************      ELUCONDB
00485 *                                                          *      ELUCONDB
00486 *        TRANSLATE TEMPRO-MANDIBULAR JOINT CONDITION BIT   *      ELUCONDB
00487 *                                                          *      ELUCONDB
00488 ************************************************************      ELUCONDB
00489                                                                   ELUCONDB
00490  215-XLAT-TMJ.                                                    ELUCONDB
00491      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELUCONDB
00492         WS-DESC-TMJ DELIMITED BY SIZE INTO TCAR-FROM-AREA.        ELUCONDB
00493                                                                   ELUCONDB
00494                                                                   ELUCONDB
00495 ************************************************************      ELUCONDB
00496 *                                                          *      ELUCONDB
00497 *        TRANSLATE INFERTILITY CONDITION BIT               *      ELUCONDB
00498 *                                                          *      ELUCONDB
00499 ************************************************************      ELUCONDB
00500                                                                   ELUCONDB
00501  216-XLAT-INF.                                                    ELUCONDB
00502      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELUCONDB
00503         WS-DESC-INF DELIMITED BY SIZE INTO TCAR-FROM-AREA.        ELUCONDB
00504                                                                   ELUCONDB
00505                                                                   ELUCONDB
00506 ************************************************************      ELUCONDB
00507 *                                                          *      ELUCONDB
00508 *        TRANSLATE LIFE THREATENING BIT                    *      ELUCONDB
00509 *                                                          *      ELUCONDB
00510 ************************************************************      ELUCONDB
00511                                                                   ELUCONDB
00512  217-XLAT-LIF.                                                    ELUCONDB
00513      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELUCONDB
00514         WS-DESC-LIF DELIMITED BY SIZE INTO TCAR-FROM-AREA.        ELUCONDB
00515      EJECT                                                        ELUCONDB
00516 ************************************************************      ELUCONDB
00517 *                                                          *      ELUCONDB
00518 *        TRANSLATE SERIOUS MENTAL ILLNESS BIT              *      ELUCONDB
00519 *                                                          *      ELUCONDB
00520 ************************************************************      ELUCONDB
00521                                                                   ELUCONDB
00522  218-XLAT-SMI.                                                    ELUCONDB
00523      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELUCONDB
00524         WS-DESC-SMI DELIMITED BY SIZE INTO TCAR-FROM-AREA.        ELUCONDB
00525      EJECT                                                        ELUCONDB
00526 ************************************************************      ELUCONDB
00527 *                                                          *      ELUCONDB
00528 *        TRANSLATE SERIOUS MENTAL ILLNESS BIT              *      ELUCONDB
00529 *                                                          *      ELUCONDB
00530 ************************************************************      ELUCONDB
00531                                                                   ELUCONDB
00532  219-XLAT-NSMI.                                                   ELUCONDB
00533      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELUCONDB
00534         WS-DESC-NSMI DELIMITED BY SIZE INTO TCAR-FROM-AREA.       ELUCONDB
00535      EJECT                                                        ELUCONDB
00536 ************************************************************      ELUCONDB
00537 *                                                          *      ELUCONDB
00538 *        MANIPULATE DATA FOR OUTPUT                        *      ELUCONDB
00539 *                                                          *      ELUCONDB
00540 ************************************************************      ELUCONDB
00541                                                                   ELUCONDB
00542  300-COMPRESS-OUTPUT-TEXT.                                        ELUCONDB
00543                                                                   ELUCONDB
00544      INSPECT TCAR-FROM-AREA  REPLACING FIRST '**'                 ELUCONDB
00545              BY SPACES.                                           ELUCONDB
00546      INSPECT TCAR-FROM-AREA  REPLACING ALL '**'                   ELUCONDB
00547              BY ', '.                                             ELUCONDB
00548      IF COND-SPECIFIC                                             ELUCONDB
00549         STRING TCAR-FROM-AREA DELIMITED BY '   '                  ELUCONDB
00550             ' CONDITIONS' DELIMITED BY SIZE INTO TCAR-FROM-AREA.  ELUCONDB
00551                                                                   ELUCONDB
00552      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELUCONDB
00553                                                                   ELUCONDB
00554      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELUCONDB
00555      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELUCONDB
00556      MOVE +75 TO TCAR-OUTPUT-FIELD-1-LEN                          ELUCONDB
00557                  TCAR-OUTPUT-FIELD-2-LEN                          ELUCONDB
00558                  TCAR-OUTPUT-FIELD-3-LEN                          ELUCONDB
00559                  TCAR-OUTPUT-FIELD-4-LEN                          ELUCONDB
00560                  TCAR-OUTPUT-FIELD-5-LEN                          ELUCONDB
00561                  TCAR-OUTPUT-FIELD-6-LEN                          ELUCONDB
00562                  TCAR-OUTPUT-FIELD-7-LEN                          ELUCONDB
00563                  TCAR-OUTPUT-FIELD-8-LEN                          ELUCONDB
00564                  TCAR-OUTPUT-FIELD-9-LEN                          ELUCONDB
00565                  TCAR-OUTPUT-FIELD-10-LEN                         ELUCONDB
00566                  TCAR-OUTPUT-FIELD-11-LEN                         ELUCONDB
00567                  TCAR-OUTPUT-FIELD-12-LEN                         ELUCONDB
00568                  TCAR-OUTPUT-FIELD-13-LEN                         ELUCONDB
00569                  TCAR-OUTPUT-FIELD-14-LEN                         ELUCONDB
00570                  TCAR-OUTPUT-FIELD-15-LEN                         ELUCONDB
00571                  TCAR-OUTPUT-FIELD-16-LEN                         ELUCONDB
00572                  TCAR-OUTPUT-FIELD-17-LEN                         ELUCONDB
00573                  TCAR-OUTPUT-FIELD-18-LEN                         ELUCONDB
00574                  TCAR-OUTPUT-FIELD-19-LEN                         ELUCONDB
00575                  TCAR-OUTPUT-FIELD-20-LEN.                        ELUCONDB
00576      PERFORM TCPR-000-TEXT-UNSTRING.                              ELUCONDB
00577                                                                   ELUCONDB
00578      MOVE TCAR-OUTPUT-FIELDS-USED TO CMF-NBR-DESCR-LINES.         ELUCONDB
00579      PERFORM WITH TEST BEFORE                                     ELUCONDB
00580         VARYING SUB1 FROM 1 BY 1                                  ELUCONDB
00581           UNTIL SUB1 GREATER THAN TCAR-OUTPUT-FIELDS-USED         ELUCONDB
00582         MOVE TCAR-OPF-DATA (SUB1) TO CMF-DESCR-LINE (SUB1)        ELUCONDB
00583         END-PERFORM.                                              ELUCONDB
00584                                                                   ELUCONDB
00585      IF CMF-DESCR-LINE (1) = SPACES                               ELUCONDB
00586      THEN                                                         ELUCONDB
00587         SET CIA-RC-CONDB-UNABLE TO TRUE                           ELUCONDB
00588         MOVE 1 TO CMF-NBR-DESCR-LINES                             ELUCONDB
00589         MOVE WS-ERROR-MSG TO CMF-DESCR-LINE (1).                  ELUCONDB
00590      EJECT                                                        ELUCONDB
00591      COPY ELSTCOMP.                                               ELUCONDB
00592      EJECT                                                        ELUCONDB
00593 ************************************************************      ELUCONDB
00594 *                                                          *      ELUCONDB
00595 *        CHECK COMMAREA LENGTH                             *      ELUCONDB
00596 *                                                          *      ELUCONDB
00597 ************************************************************      ELUCONDB
00598                                                                   ELUCONDB
00599  901-CHECK-COMMAREA-LENGTH.                                       ELUCONDB
00600      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELUCONDB
00601      THEN                                                         ELUCONDB
00602         PERFORM 991-SIGNAL-COMM-LEN-ERROR.                        ELUCONDB
00603                                                                   ELUCONDB
00604 ************************************************************      ELUCONDB
00605 *                                                          *      ELUCONDB
00606 *        ESTABLISH ADDRESSABILITY OF CIA                   *      ELUCONDB
00607 *                                                          *      ELUCONDB
00608 ************************************************************      ELUCONDB
00609                                                                   ELUCONDB
00610  911-EST-ADDR-CIA.                                                ELUCONDB
00611      IF ECA-CIA-PTR NOT = NULL                                    ELUCONDB
00612      THEN                                                         ELUCONDB
00613         CALL 'ELUINISM'                                           ELUCONDB
00614            USING DFHCOMMAREA                                      ELUCONDB
00615                  ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA         ELUCONDB
00616      ELSE                                                         ELUCONDB
00617          PERFORM 992-SIGNAL-INVALID-CIA.                          ELUCONDB
00618                                                                   ELUCONDB
00619                                                                   ELUCONDB
00620 ************************************************************      ELUCONDB
00621 *                                                          *      ELUCONDB
00622 *        ESTABLISH ADDRESSABILITY OF ELSSSCBC              *      ELUCONDB
00623 *                                                          *      ELUCONDB
00624 ************************************************************      ELUCONDB
00625                                                                   ELUCONDB
00626  912-ESTAB-ADDR-ELSSSCB.                                          ELUCONDB
00627      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELUCONDB
00628      CALL 'ELUSETAD'                                              ELUCONDB
00629          USING DFHCOMMAREA                                        ELUCONDB
00630                ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.            ELUCONDB
00631      IF CIA-RC-PTR-NULL                                           ELUCONDB
00632          PERFORM 993-SIGNAL-UNALLOC-AREA.                         ELUCONDB
00633                                                                   ELUCONDB
00634                                                                   ELUCONDB
00635 ************************************************************      ELUCONDB
00636 *                                                          *      ELUCONDB
00637 *        ESTABLISH ADDRESSABILITY OF ELSCMIFC              *      ELUCONDB
00638 *                                                          *      ELUCONDB
00639 ************************************************************      ELUCONDB
00640                                                                   ELUCONDB
00641  913-ESTAB-ADDR-ELSCMIF.                                          ELUCONDB
00642      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELUCONDB
00643      CALL 'ELUSETAD'                                              ELUCONDB
00644         USING DFHCOMMAREA                                         ELUCONDB
00645         ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                    ELUCONDB
00646      IF CIA-RC-PTR-NULL                                           ELUCONDB
00647          PERFORM 993-SIGNAL-UNALLOC-AREA.                         ELUCONDB
00648                                                                   ELUCONDB
00649                                                                   ELUCONDB
00650 ************************************************************      ELUCONDB
00651 *                                                          *      ELUCONDB
00652 *        ESTABLISH ADDRESSABILITY OF ELSCMDSC              *      ELUCONDB
00653 *                                                          *      ELUCONDB
00654 ************************************************************      ELUCONDB
00655                                                                   ELUCONDB
00656  914-ESTAB-ADDR-ELSCMDS.                                          ELUCONDB
00657      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELUCONDB
00658      CALL 'ELUSETAD'                                              ELUCONDB
00659         USING DFHCOMMAREA                                         ELUCONDB
00660         ADDRESS OF CMF-DESCR.                                     ELUCONDB
00661      IF CIA-RC-PTR-NULL                                           ELUCONDB
00662          PERFORM 993-SIGNAL-UNALLOC-AREA.                         ELUCONDB
00663                                                                   ELUCONDB
00664                                                                   ELUCONDB
00665 ************************************************************      ELUCONDB
00666 *                                                          *      ELUCONDB
00667 *        ESTABLISH ADDRESSABILITY OF ELSTCWA               *      ELUCONDB
00668 *                                                          *      ELUCONDB
00669 ************************************************************      ELUCONDB
00670                                                                   ELUCONDB
00671  915-ESTAB-ADDR-ELSTCWA.                                          ELUCONDB
00672      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELUCONDB
00673      CALL 'ELUSETAD'                                              ELUCONDB
00674         USING DFHCOMMAREA                                         ELUCONDB
00675         ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                    ELUCONDB
00676      IF CIA-RC-PTR-NULL                                           ELUCONDB
00677          PERFORM 993-SIGNAL-UNALLOC-AREA.                         ELUCONDB
00678      EJECT                                                        ELUCONDB
00679 ************************************************************      ELUCONDB
00680 *                                                          *      ELUCONDB
00681 *        SIGNAL COMMAREA LENGTH ERROR                      *      ELUCONDB
00682 *                                                          *      ELUCONDB
00683 ************************************************************      ELUCONDB
00684                                                                   ELUCONDB
00685  991-SIGNAL-COMM-LEN-ERROR.                                       ELUCONDB
00686      EXEC CICS ABEND                                              ELUCONDB
00687                ABCODE('EL01')                                     ELUCONDB
00688                END-EXEC.                                          ELUCONDB
00689                                                                   ELUCONDB
00690                                                                   ELUCONDB
00691 ************************************************************      ELUCONDB
00692 *                                                          *      ELUCONDB
00693 *        SIGNAL INVALID CIA                                *      ELUCONDB
00694 *                                                          *      ELUCONDB
00695 ************************************************************      ELUCONDB
00696                                                                   ELUCONDB
00697  992-SIGNAL-INVALID-CIA.                                          ELUCONDB
00698      EXEC CICS ABEND                                              ELUCONDB
00699                ABCODE('EL02')                                     ELUCONDB
00700                END-EXEC.                                          ELUCONDB
00701                                                                   ELUCONDB
00702                                                                   ELUCONDB
00703 ************************************************************      ELUCONDB
00704 *                                                          *      ELUCONDB
00705 *        SIGNAL UNALLOCATED AREA                           *      ELUCONDB
00706 *                                                          *      ELUCONDB
00707 ************************************************************      ELUCONDB
00708                                                                   ELUCONDB
00709  993-SIGNAL-UNALLOC-AREA.                                         ELUCONDB
00710      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELUCONDB
00711      EXEC CICS ABEND                                              ELUCONDB
00712                ABCODE(CIA-ABCODE)                                 ELUCONDB
00713                END-EXEC.                                          ELUCONDB
