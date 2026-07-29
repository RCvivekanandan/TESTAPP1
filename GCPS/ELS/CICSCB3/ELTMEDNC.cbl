00001 *      LAST MAINTENANCE TIME: 15.33.43  DATE: 06/27/91            06/29/02
00002  IDENTIFICATION DIVISION.                                         ELTMEDNC
00003                                                                      LV001
00004  PROGRAM-ID.         ELTMEDNC.                                    ELTMEDNC
00005                                                                   ELTMEDNC
00006  AUTHOR.             ANNE KEFFER KING.                            ELTMEDNC
00007                                                                   ELTMEDNC
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTMEDNC
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELTMEDNC
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTMEDNC
00011                      233 N. MICHIGAN AVE                          ELTMEDNC
00012                      CHICAGO, ILLINOIS 60601                      ELTMEDNC
00013                                                                   ELTMEDNC
00014  DATE-WRITTEN.       12-JUN-1897.                                 ELTMEDNC
00015                                                                   ELTMEDNC
00016  DATE-COMPILED.                                                   ELTMEDNC
00017                                                                   ELTMEDNC
00018  SECURITY.           COPYRIGHT 1986,                              ELTMEDNC
00019                      HEALTH CARE SERVICE CORPORATION              ELTMEDNC
00020 ******************************************************************ELTMEDNC
00021 * ENGLISH CONTRACT INQUIRY ON-LINE SYSTEM                        *ELTMEDNC
00022 * THIS PROGRAM WILL CREATE TRANSLATED SCREEN RECORDS FOR         *ELTMEDNC
00023 * DISPLAY OF TOPIC - COST CONTAINMENT - MEDICAL NECESSITY        *ELTMEDNC
00024 * APPROVAL SOURCE CODE IN ENGLISH LANGUAGE FORMAT.               *ELTMEDNC
00025 *                                                                *ELTMEDNC
00026 * INPUT IS THE GCG GROUP SPECIFIC RECORD AND                     *ELTMEDNC
00027 *          THE MEDICAL NECESSITY TABULAR RECORD.                 *ELTMEDNC
00028 *          OF THE GCCP TABULAR.                                  *ELTMEDNC
00029 *                                                                *ELTMEDNC
00030 ******************************************************************ELTMEDNC
00031 *                                                                *ELTMEDNC
00032 *         MAINTENANCE HISTORY                                     ELTMEDNC
00033 *                                                                *ELTMEDNC
00034 *  MOD     DATE     BY  DRPT              ACTION                 *ELTMEDNC
00035 * ----- ----------- --- ----- ---------------------------------- *ELTMEDNC
00036 * 01.00 12-JUN-1987 AKK       CREATED                            *ELTMEDNC
00037 *                                                                *ELTMEDNC
00038 * 01.01 22-OCT-1990 JPB       CHANGED STORAGE MANAGEMENT         *ELTMEDNC
00039 *                                                                *ELTMEDNC
00040 * 01.02 09-NOV-1990 JPB       CHANGED REFERENCES TO GCG-MED-     *ELTMEDNC
00041 *                             NECESSITY-HCNR-IPS-IN TO ACCOMODATE*ELTMEDNC
00042 *                             CHANGE IN FIELD SIZE.              *ELTMEDNC
00043 *                                                                *ELTMEDNC
00044 * 01.03 11-JUN-1991 GEM       ADD CCP PARTIC IND TO MEDNC.       *ELTMEDNC
00045 *                                                                *ELTMEDNC
00046 * 01.04 27-JUN-1991 JPB       CHANGED INITIAL VALUE OF APPROVAL- *ELTMEDNC
00047 *                             SOURCE SWITCH IN WORKING STORAGE   *ELTMEDNC
00048 *                             TO 'N'.                            *ELTMEDNC
00049 *                                                                *ELTMEDNC
00050 ******************************************************************ELTMEDNC
00051      SKIP3                                                        ELTMEDNC
00052  ENVIRONMENT DIVISION.                                            ELTMEDNC
00053                                                                   ELTMEDNC
00054  CONFIGURATION SECTION.                                           ELTMEDNC
00055  SOURCE-COMPUTER.    IBM-3090.                                    ELTMEDNC
00056  OBJECT-COMPUTER.    IBM-3090.                                    ELTMEDNC
00057      EJECT                                                        ELTMEDNC
00058                                                                   ELTMEDNC
00059  DATA DIVISION.                                                   ELTMEDNC
00060  WORKING-STORAGE SECTION.                                         ELTMEDNC
00061  01  WS-MISC.                                                     ELTMEDNC
00062      05  FILLER                  PIC X(24) VALUE                  ELTMEDNC
00063          'ELTMEDNC WORKING STORAGE'.                              ELTMEDNC
00064      05  WS-POINTER2             POINTER.                         ELTMEDNC
00065      05  WS-POINTER3             POINTER.                         ELTMEDNC
00066 *                                                                 ELTMEDNC
00067  01  WS-SWITCHES.                                                 ELTMEDNC
00068      05  APPROVAL-SOURCE-SWITCH  PIC X      VALUE 'N'.            ELTMEDNC
00069          88  NOT-HOLDING-APPROVAL-SRCE      VALUE 'N'.            ELTMEDNC
00070          88  HOLDING-APPROVAL-SOURCE        VALUE 'Y'.            ELTMEDNC
00071                                                                   ELTMEDNC
00072      05  ADDITIONAL-TEXT-SW      PIC X      VALUE SPACES.         ELTMEDNC
00073          88  BLANK-LINE-NEEDED              VALUE 'B'.            ELTMEDNC
00074          88  ADDITIONAL-TEXT                VALUE 'Y'.            ELTMEDNC
00075 *                                                                 ELTMEDNC
00076      05  CONTINUED-PROCESSING-SW PIC X      VALUE SPACES.         ELTMEDNC
00077          88  PROCESSING-CMF-TEXT            VALUE 'P'.            ELTMEDNC
00078          88  DONE-PROCESSING                VALUE 'D'.            ELTMEDNC
00079 *                                                                 ELTMEDNC
00080      05  WS-PERIOD-SW            PIC X      VALUE 'N'.            ELTMEDNC
00081          88  PERIOD-NEEDED                  VALUE 'Y'.            ELTMEDNC
00082 *                                                                 ELTMEDNC
00083      05  UNDEFINED-TABULAR-SW    PIC X      VALUE 'N'.            ELTMEDNC
00084          88  TABULAR-IS-UNDEFINED           VALUE 'Y'.            ELTMEDNC
00085 *                                                                 ELTMEDNC
00086      05  DEFINED-TABULAR-SW      PIC X      VALUE 'N'.            ELTMEDNC
00087          88  TABULAR-IS-DEFINED             VALUE 'Y'.            ELTMEDNC
00088 *                                                                 ELTMEDNC
00089      05  SCREEN-TYPE             PIC X      VALUE SPACES.         ELTMEDNC
00090          88  INSTITUTIONAL-SCREEN           VALUE 'I'.            ELTMEDNC
00091          88  PROFESSIONAL-SCREEN            VALUE 'P'.            ELTMEDNC
00092          88  SUPPLEMENTAL-SCREEN            VALUE 'S'.            ELTMEDNC
00093 *                                                                 ELTMEDNC
00094      05  WS-GMDN-SRVS-ID         PIC X(06)  VALUE SPACE.          ELTMEDNC
00095      05  WS-GMDN-SRVS-SLOT-NO    PIC S9(04) COMP-3                ELTMEDNC
00096                                             VALUE ZERO.           ELTMEDNC
00097 *                                                                 ELTMEDNC
00098  01  PROGRAM-CONSTANTS.                                           ELTMEDNC
00099      05   PC-GRP                  PIC X(06) VALUE 'GROUP'.        ELTMEDNC
00100      05   PC-GCCP                 PIC X(06) VALUE '#GCCP '.       ELTMEDNC
00101      05   PC-GMDN                 PIC X(06) VALUE '#GMDN '.       ELTMEDNC
00102 ***************************************************************   ELTMEDNC
00103 **** H E A D E R  L I N E S                                       ELTMEDNC
00104 *************************************************************     ELTMEDNC
00105  01  WS-HDR-LN2.                                                  ELTMEDNC
00106      05  FILLER                  PIC X(25)  VALUE SPACES.         ELTMEDNC
00107      05  FILLER                  PIC X(25)  VALUE                 ELTMEDNC
00108          'MEDICAL NECESSITY PROGRAM'.                             ELTMEDNC
00109      05  FILLER                  PIC X(01)  VALUE SPACES.         ELTMEDNC
00110      05  HDR-TITLE               PIC X(13)  VALUE SPACES.         ELTMEDNC
00111      05  FILLER                  PIC X(15)  VALUE SPACES.         ELTMEDNC
00112 **************************************************************    ELTMEDNC
00113 *** SCREEN BODY LINES                                             ELTMEDNC
00114 **************************************************************    ELTMEDNC
00115  01  WS-INDICATOR.                                                ELTMEDNC
00116      05  FILLER                  PIC X(79) VALUE                  ELTMEDNC
00117          'THE MEDICAL NECESSITY PROGRAM WILL '.                   ELTMEDNC
00118 *                                                                 ELTMEDNC
00119  01  WS-APPROVAL-SOURCE.                                          ELTMEDNC
00120      05  FILLER                  PIC X(64) VALUE                  ELTMEDNC
00121         'MEDICAL NECESSITY PROGRAM REQUIRES THAT SERVICES BE APPROELTMEDNC
00122 -       'VED BY '.                                                ELTMEDNC
00123      05  FILLER                  PIC X(15) VALUE SPACES.          ELTMEDNC
00124 *                                                                 ELTMEDNC
00125  01  WS-APPROVAL-SOURCEA.                                         ELTMEDNC
00126      05  FILLER                  PIC X(45) VALUE                  ELTMEDNC
00127          'AND ARE SUBJECT TO MEDICAL REVIEW FOR PAYMENT'.         ELTMEDNC
00128      05  FILLER                  PIC X(13) VALUE                  ELTMEDNC
00129          ' ELIGIBILITY.'.                                         ELTMEDNC
00130      05  FILLER                   PIC X(21) VALUE SPACES.         ELTMEDNC
00131 *                                                                 ELTMEDNC
00132  01  WS-SPILLOVER-SENTENCE.                                       ELTMEDNC
00133      05  FILLER                  PIC X(79)  VALUE                 ELTMEDNC
00134         ' UNPAID SERVICES AFTER BASIC BENEFITS REDUCTIONS ARE '.  ELTMEDNC
00135 *                                                                 ELTMEDNC
00136  01  WS-MEDNC-APPLIES.                                            ELTMEDNC
00137      05  FILLER                   PIC  X(40) VALUE                ELTMEDNC
00138          'THE MEDICAL NECESSITY PROGRAM APPLIES TO'.              ELTMEDNC
00139      05  FILLER                   PIC  X(39) VALUE SPACES.        ELTMEDNC
00140 *                                                                 ELTMEDNC
00141 ***************************************************************   ELTMEDNC
00142 **** SPECIAL MESSAGE FOR NOT APPLICABLE CASES                     ELTMEDNC
00143 ***************************************************************   ELTMEDNC
00144  01  WS-NOT-APPLICABLE-MSG.                                       ELTMEDNC
00145      05  FILLER                   PIC  X(48) VALUE                ELTMEDNC
00146          'THE MEDICAL NECESSITY PROGRAM IS NOT APPLICABLE.'.      ELTMEDNC
00147      05  FILLER                   PIC  X(31) VALUE SPACES.        ELTMEDNC
00148 *                                                                 ELTMEDNC
00149  01  WS-NOT-APPLICABLE-LOB-BC.                                    ELTMEDNC
00150      05  FILLER                   PIC X(60)  VALUE                ELTMEDNC
00151         'MEDICAL NECESSITY DOES NOT APPLY FOR INSTITUTIONAL BENEFIELTMEDNC
00152 -       'TS.'.                                                    ELTMEDNC
00153      05  FILLER                   PIC X(19)  VALUE SPACES.        ELTMEDNC
00154 *                                                                 ELTMEDNC
00155  01  WS-NOT-APPLICABLE-LOB-BS.                                    ELTMEDNC
00156      05  FILLER                   PIC X(59)  VALUE                ELTMEDNC
00157         'MEDICAL NECESSITY DOES NOT APPLY FOR PROFESSIONAL BENEFITELTMEDNC
00158 -       'S.'.                                                     ELTMEDNC
00159      05  FILLER                   PIC X(20)  VALUE SPACES.        ELTMEDNC
00160 *                                                                 ELTMEDNC
00161  01  WS-NOT-APPLICABLE-LOB-MM.                                    ELTMEDNC
00162      05  FILLER                   PIC X(59)  VALUE                ELTMEDNC
00163         'MEDICAL NECESSITY DOES NOT APPLY FOR SUPPLEMENTAL BENEFITELTMEDNC
00164 -       'S.'.                                                     ELTMEDNC
00165      05  FILLER                   PIC X(20)  VALUE SPACES.        ELTMEDNC
00166 *                                                                 ELTMEDNC
00167  01  WS-SPECIAL-SERVICES-MSG.                                     ELTMEDNC
00168      05  FILLER                    PIC X(79)  VALUE               ELTMEDNC
00169         'THERE ARE SPECIAL RELATED SERVICES INCLUDED IN THIS COST ELTMEDNC
00170 -       'CONTAINMENT PROGRAM.'.                                   ELTMEDNC
00171 *                                                                 ELTMEDNC
00172  01  WS-DISCLAIMER.                                               ELTMEDNC
00173      05  FILLER                    PIC X(79)  VALUE               ELTMEDNC
00174          '*** SUBJECT TO OTHER CONTRACT LIMITATIONS ***'.         ELTMEDNC
00175 /                                                                 ELTMEDNC
00176  LINKAGE SECTION.                                                 ELTMEDNC
00177  01  DFHCOMMAREA.                                                 ELTMEDNC
00178      COPY ELSCOMMC.                                               ELTMEDNC
00179 /                                                                 ELTMEDNC
00180      COPY ELSCIA2C.                                               ELTMEDNC
00181 /                                                                 ELTMEDNC
00182      COPY ELSCMDSC.                                               ELTMEDNC
00183 /                                                                 ELTMEDNC
00184      COPY ELSCMIFC.                                               ELTMEDNC
00185 /                                                                 ELTMEDNC
00186      COPY ELSIOPMC.                                               ELTMEDNC
00187 /                                                                 ELTMEDNC
00188      COPY ELSKEYSC.                                               ELTMEDNC
00189 /                                                                 ELTMEDNC
00190      COPY ELSOUTPC.                                               ELTMEDNC
00191 /                                                                 ELTMEDNC
00192      COPY ELSSRTPC.                                               ELTMEDNC
00193 /                                                                 ELTMEDNC
00194      COPY ELSTCWAC.                                               ELTMEDNC
00195 /                                                                 ELTMEDNC
00196      COPY ELSSSCBC.                                               ELTMEDNC
00197 /                                                                 ELTMEDNC
00198  01  GRP-SPECIFIC-REC.                                            ELTMEDNC
00199  COPY GCGROUPC.                                                   ELTMEDNC
00200 /                                                                 ELTMEDNC
00201  01  GCCP-TABULAR-REC.                                            ELTMEDNC
00202  COPY GCTGCCPC.                                                   ELTMEDNC
00203 ************************************************************      ELTMEDNC
00204 /                                                                 ELTMEDNC
00205      EJECT                                                        ELTMEDNC
00206  PROCEDURE DIVISION.                                              ELTMEDNC
00207 ************************************************************      ELTMEDNC
00208 *                                                          *      ELTMEDNC
00209 *                    PROCEDURE DIVISION                    *      ELTMEDNC
00210 *                                                          *      ELTMEDNC
00211 ************************************************************      ELTMEDNC
00212                                                                   ELTMEDNC
00213                                                                   ELTMEDNC
00214 ************************************************************      ELTMEDNC
00215 *                                                          *      ELTMEDNC
00216 *        MEDICAL NECESSITY                                 *      ELTMEDNC
00217 *                                                          *      ELTMEDNC
00218 ************************************************************      ELTMEDNC
00219  MEDICAL-NECESSITY.                                               ELTMEDNC
00220      PERFORM INITIALIZATION.                                      ELTMEDNC
00221      PERFORM PROCESS-MEDICAL-NECESSITY.                           ELTMEDNC
00222      GOBACK.                                                      ELTMEDNC
00223                                                                   ELTMEDNC
00224                                                                   ELTMEDNC
00225 ************************************************************      ELTMEDNC
00226 *                                                          *      ELTMEDNC
00227 *        INITIALIZATION                                    *      ELTMEDNC
00228 *                                                          *      ELTMEDNC
00229 ************************************************************      ELTMEDNC
00230  INITIALIZATION.                                                  ELTMEDNC
00231      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTMEDNC
00232      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTMEDNC
00233                                                                   ELTMEDNC
00234                                                                   ELTMEDNC
00235 ************************************************************      ELTMEDNC
00236 *                                                          *      ELTMEDNC
00237 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTMEDNC
00238 *                                                          *      ELTMEDNC
00239 ************************************************************      ELTMEDNC
00240  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTMEDNC
00241      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTMEDNC
00242      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTMEDNC
00243      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTMEDNC
00244                                                                   ELTMEDNC
00245                                                                   ELTMEDNC
00246 ************************************************************      ELTMEDNC
00247 *                                                          *      ELTMEDNC
00248 *        CHECK FOR VALID COMMAREA                          *      ELTMEDNC
00249 *                                                          *      ELTMEDNC
00250 ************************************************************      ELTMEDNC
00251  CHECK-FOR-VALID-COMMAREA.                                        ELTMEDNC
00252      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTMEDNC
00253          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTMEDNC
00254                                                                   ELTMEDNC
00255                                                                   ELTMEDNC
00256 ************************************************************      ELTMEDNC
00257 *                                                          *      ELTMEDNC
00258 *        SIGNAL INVALID COMMAREA                           *      ELTMEDNC
00259 *                                                          *      ELTMEDNC
00260 ************************************************************      ELTMEDNC
00261  SIGNAL-INVALID-COMMAREA.                                         ELTMEDNC
00262      EXEC CICS ABEND                                              ELTMEDNC
00263                ABCODE('EL01')                                     ELTMEDNC
00264         END-EXEC.                                                 ELTMEDNC
00265      EJECT                                                        ELTMEDNC
00266                                                                   ELTMEDNC
00267                                                                   ELTMEDNC
00268 ************************************************************      ELTMEDNC
00269 *                                                          *      ELTMEDNC
00270 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTMEDNC
00271 *                                                          *      ELTMEDNC
00272 ************************************************************      ELTMEDNC
00273  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTMEDNC
00274      IF ECA-CIA-PTR = NULL                                        ELTMEDNC
00275          PERFORM SIGNAL-INVALID-CIA                               ELTMEDNC
00276      ELSE                                                         ELTMEDNC
00277          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTMEDNC
00278                                                                   ELTMEDNC
00279                                                                   ELTMEDNC
00280 ************************************************************      ELTMEDNC
00281 *                                                          *      ELTMEDNC
00282 *        SIGNAL INVALID CIA                                *      ELTMEDNC
00283 *                                                          *      ELTMEDNC
00284 ************************************************************      ELTMEDNC
00285  SIGNAL-INVALID-CIA.                                              ELTMEDNC
00286      EXEC CICS ABEND                                              ELTMEDNC
00287                ABCODE('EL02')                                     ELTMEDNC
00288         END-EXEC.                                                 ELTMEDNC
00289                                                                   ELTMEDNC
00290                                                                   ELTMEDNC
00291 ************************************************************      ELTMEDNC
00292 *                                                          *      ELTMEDNC
00293 *        ESTABLISH ADDRESS OF CIA                          *      ELTMEDNC
00294 *                                                          *      ELTMEDNC
00295 ************************************************************      ELTMEDNC
00296  ESTABLISH-ADDRESS-OF-CIA.                                        ELTMEDNC
00297      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTMEDNC
00298                           ADDRESS OF                              ELTMEDNC
00299          CIA-ELS-COMMON-INTERFACE-AREA.                           ELTMEDNC
00300      EJECT                                                        ELTMEDNC
00301                                                                   ELTMEDNC
00302                                                                   ELTMEDNC
00303 ************************************************************      ELTMEDNC
00304 *                                                          *      ELTMEDNC
00305 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTMEDNC
00306 *                                                          *      ELTMEDNC
00307 ************************************************************      ELTMEDNC
00308  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTMEDNC
00309      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTMEDNC
00310      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMEDNC
00311                            ADDRESS OF                             ELTMEDNC
00312          SSB-SELECTOR-STATUS-CTL-BLK.                             ELTMEDNC
00313      IF CIA-RC-PTR-NULL                                           ELTMEDNC
00314          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMEDNC
00315                                                                   ELTMEDNC
00316                                                                   ELTMEDNC
00317 ************************************************************      ELTMEDNC
00318 *                                                          *      ELTMEDNC
00319 *        SIGNAL UNALLOC AREA ERROR                         *      ELTMEDNC
00320 *                                                          *      ELTMEDNC
00321 ************************************************************      ELTMEDNC
00322  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTMEDNC
00323      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTMEDNC
00324      PERFORM SIGNAL-ABEND.                                        ELTMEDNC
00325                                                                   ELTMEDNC
00326                                                                   ELTMEDNC
00327 ************************************************************      ELTMEDNC
00328 *                                                          *      ELTMEDNC
00329 *        SIGNAL ABEND                                      *      ELTMEDNC
00330 *                                                          *      ELTMEDNC
00331 ************************************************************      ELTMEDNC
00332  SIGNAL-ABEND.                                                    ELTMEDNC
00333      EXEC CICS ABEND                                              ELTMEDNC
00334                ABCODE(CIA-ABCODE)                                 ELTMEDNC
00335         END-EXEC.                                                 ELTMEDNC
00336      EJECT                                                        ELTMEDNC
00337                                                                   ELTMEDNC
00338                                                                   ELTMEDNC
00339 ************************************************************      ELTMEDNC
00340 *                                                          *      ELTMEDNC
00341 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTMEDNC
00342 *                                                          *      ELTMEDNC
00343 ************************************************************      ELTMEDNC
00344  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTMEDNC
00345      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELTMEDNC
00346      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTMEDNC
00347      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELTMEDNC
00348      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTMEDNC
00349      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTMEDNC
00350      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTMEDNC
00351      PERFORM ESTABLISH-ADDRESSABILITY-OF-CS.                      ELTMEDNC
00352                                                                   ELTMEDNC
00353                                                                   ELTMEDNC
00354 ************************************************************      ELTMEDNC
00355 *                                                          *      ELTMEDNC
00356 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTMEDNC
00357 *                                                          *      ELTMEDNC
00358 ************************************************************      ELTMEDNC
00359  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELTMEDNC
00360      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTMEDNC
00361      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMEDNC
00362                            ADDRESS OF                             ELTMEDNC
00363          CMF-CODES-MANUAL-INTERFACE.                              ELTMEDNC
00364      IF CIA-RC-PTR-NULL                                           ELTMEDNC
00365          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMEDNC
00366      EJECT                                                        ELTMEDNC
00367                                                                   ELTMEDNC
00368                                                                   ELTMEDNC
00369 ************************************************************      ELTMEDNC
00370 *                                                          *      ELTMEDNC
00371 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTMEDNC
00372 *                                                          *      ELTMEDNC
00373 ************************************************************      ELTMEDNC
00374  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTMEDNC
00375      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTMEDNC
00376      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMEDNC
00377                            ADDRESS OF                             ELTMEDNC
00378          COF-OUTPUT-INTERFACE.                                    ELTMEDNC
00379      IF CIA-RC-PTR-NULL                                           ELTMEDNC
00380          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMEDNC
00381      EJECT                                                        ELTMEDNC
00382                                                                   ELTMEDNC
00383                                                                   ELTMEDNC
00384 ************************************************************      ELTMEDNC
00385 *                                                          *      ELTMEDNC
00386 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELTMEDNC
00387 *                                                          *      ELTMEDNC
00388 ************************************************************      ELTMEDNC
00389  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELTMEDNC
00390      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTMEDNC
00391      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMEDNC
00392                            ADDRESS OF                             ELTMEDNC
00393          SRP-SUBROUTINE-PARAMETERS.                               ELTMEDNC
00394      IF CIA-RC-PTR-NULL                                           ELTMEDNC
00395          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMEDNC
00396      EJECT                                                        ELTMEDNC
00397                                                                   ELTMEDNC
00398                                                                   ELTMEDNC
00399 ************************************************************      ELTMEDNC
00400 *                                                          *      ELTMEDNC
00401 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTMEDNC
00402 *                                                          *      ELTMEDNC
00403 ************************************************************      ELTMEDNC
00404  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTMEDNC
00405      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTMEDNC
00406      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMEDNC
00407                            ADDRESS OF                             ELTMEDNC
00408          TCAR-COMPRESSION-WORK-AREA.                              ELTMEDNC
00409      IF CIA-RC-PTR-NULL                                           ELTMEDNC
00410          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMEDNC
00411      EJECT                                                        ELTMEDNC
00412                                                                   ELTMEDNC
00413                                                                   ELTMEDNC
00414 ************************************************************      ELTMEDNC
00415 *                                                          *      ELTMEDNC
00416 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTMEDNC
00417 *                                                          *      ELTMEDNC
00418 ************************************************************      ELTMEDNC
00419  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTMEDNC
00420      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTMEDNC
00421      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMEDNC
00422                            ADDRESS OF                             ELTMEDNC
00423          KWA-FILE-KEY-WORK-AREA.                                  ELTMEDNC
00424      IF CIA-RC-PTR-NULL                                           ELTMEDNC
00425          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMEDNC
00426      EJECT                                                        ELTMEDNC
00427                                                                   ELTMEDNC
00428                                                                   ELTMEDNC
00429 ************************************************************      ELTMEDNC
00430 *                                                          *      ELTMEDNC
00431 *        ESTABLISH ADDRESSABILITY OF GRP SPECIFIC          *      ELTMEDNC
00432 *                                                          *      ELTMEDNC
00433 ************************************************************      ELTMEDNC
00434  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTMEDNC
00435      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTMEDNC
00436      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMEDNC
00437                           ADDRESS OF GRP-SPECIFIC-REC.            ELTMEDNC
00438      IF CIA-RC-PTR-NULL                                           ELTMEDNC
00439          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMEDNC
00440      EJECT                                                        ELTMEDNC
00441                                                                   ELTMEDNC
00442                                                                   ELTMEDNC
00443 ************************************************************      ELTMEDNC
00444 *                                                          *      ELTMEDNC
00445 *        ESTABLISH ADDRESSABILITY OF CST CONTAINMENT       *      ELTMEDNC
00446 *                                                          *      ELTMEDNC
00447 ************************************************************      ELTMEDNC
00448  ESTABLISH-ADDRESSABILITY-OF-CS.                                  ELTMEDNC
00449      SET CIA-GCTABULR-DDN TO TRUE.                                ELTMEDNC
00450      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMEDNC
00451                            ADDRESS OF GCCP-TABULAR-REC.           ELTMEDNC
00452      IF CIA-RC-PTR-NULL                                           ELTMEDNC
00453          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTMEDNC
00454      EJECT                                                        ELTMEDNC
00455                                                                   ELTMEDNC
00456                                                                   ELTMEDNC
00457 ************************************************************      ELTMEDNC
00458 *                                                          *      ELTMEDNC
00459 *        PROCESS MEDICAL NECESSITY                         *      ELTMEDNC
00460 *                                                          *      ELTMEDNC
00461 ************************************************************      ELTMEDNC
00462  PROCESS-MEDICAL-NECESSITY.                                       ELTMEDNC
00463      IF GCG-MED-NECESSITY-HCNR-IPS-IN EQUAL ZERO                  ELTMEDNC
00464          PERFORM SIGNAL-NOT-APPLICABLE-MSG                        ELTMEDNC
00465      ELSE                                                         ELTMEDNC
00466          PERFORM GENERATE-MEDICAL-NECESSITY-TEX.                  ELTMEDNC
00467      MOVE 'E' TO  COF-FUNCTION.                                   ELTMEDNC
00468      MOVE ZEROS TO COF-NBR-DTL-LINES                              ELTMEDNC
00469                COF-NBR-HDR-LINES.                                 ELTMEDNC
00470      PERFORM LINK-TO-OUTPUT.                                      ELTMEDNC
00471      EJECT                                                        ELTMEDNC
00472                                                                   ELTMEDNC
00473                                                                   ELTMEDNC
00474 ************************************************************      ELTMEDNC
00475 *                                                          *      ELTMEDNC
00476 *        GENERATE MEDICAL NECESSITY TEXT                   *      ELTMEDNC
00477 *                                                          *      ELTMEDNC
00478 ************************************************************      ELTMEDNC
00479  GENERATE-MEDICAL-NECESSITY-TEX.                                  ELTMEDNC
00480      PERFORM VERIFY-MEDICAL-NECESSITY-IN-GC.                      ELTMEDNC
00481      PERFORM BUILD-MEDICAL-NECESSITY-TEXT.                        ELTMEDNC
00482                                                                   ELTMEDNC
00483                                                                   ELTMEDNC
00484 ************************************************************      ELTMEDNC
00485 *                                                          *      ELTMEDNC
00486 *        VERIFY MEDICAL NECESSITY IN GCCP RECORD           *      ELTMEDNC
00487 *                                                          *      ELTMEDNC
00488 ************************************************************      ELTMEDNC
00489  VERIFY-MEDICAL-NECESSITY-IN-GC.                                  ELTMEDNC
00490      SET WS-POINTER2 TO NULLS.                                    ELTMEDNC
00491      SET WS-POINTER3 TO NULLS.                                    ELTMEDNC
00492      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTMEDNC
00493      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMEDNC
00494                            WS-POINTER2.                           ELTMEDNC
00495      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTMEDNC
00496      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMEDNC
00497                              WS-POINTER3.                         ELTMEDNC
00498      PERFORM AQUIRE-GCCP-RECORD.                                  ELTMEDNC
00499      PERFORM OBTAIN-MEDICAL-NECESSITY-WITHI.                      ELTMEDNC
00500      EJECT                                                        ELTMEDNC
00501                                                                   ELTMEDNC
00502                                                                   ELTMEDNC
00503 ************************************************************      ELTMEDNC
00504 *                                                          *      ELTMEDNC
00505 *        AQUIRE GCCP RECORD                                *      ELTMEDNC
00506 *                                                          *      ELTMEDNC
00507 ************************************************************      ELTMEDNC
00508  AQUIRE-GCCP-RECORD.                                              ELTMEDNC
00509      MOVE SPACES TO KWA-PROVISION-ID.                             ELTMEDNC
00510      SET GCG-INDEX TO 1.                                          ELTMEDNC
00511      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTMEDNC
00512          AT END                                                   ELTMEDNC
00513             MOVE ZEROES TO KWA-PROVISION-SLOT-NO                  ELTMEDNC
00514             WHEN GCG-TAB-ID (GCG-INDEX) = PC-GCCP                 ELTMEDNC
00515                 MOVE GCG-TAB-ID (GCG-INDEX) TO                    ELTMEDNC
00516          KWA-PROVISION-ID                                         ELTMEDNC
00517                 MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                  ELTMEDNC
00518                     TO KWA-PROVISION-SLOT-NO                      ELTMEDNC
00519          END-SEARCH.                                              ELTMEDNC
00520      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELTMEDNC
00521          PERFORM SIGNAL-UNDEFINED-TABULAR                         ELTMEDNC
00522      ELSE                                                         ELTMEDNC
00523          PERFORM READ-GCCP-RECORD.                                ELTMEDNC
00524                                                                   ELTMEDNC
00525                                                                   ELTMEDNC
00526 ************************************************************      ELTMEDNC
00527 *                                                          *      ELTMEDNC
00528 *        SIGNAL UNDEFINED TABULAR                          *      ELTMEDNC
00529 *                                                          *      ELTMEDNC
00530 ************************************************************      ELTMEDNC
00531  SIGNAL-UNDEFINED-TABULAR.                                        ELTMEDNC
00532      SET CIA-AB-TAB-UNDEF TO TRUE.                                ELTMEDNC
00533      PERFORM SIGNAL-ABEND.                                        ELTMEDNC
00534      EJECT                                                        ELTMEDNC
00535                                                                   ELTMEDNC
00536                                                                   ELTMEDNC
00537 ************************************************************      ELTMEDNC
00538 *                                                          *      ELTMEDNC
00539 *        OBTAIN MEDICAL NECESSITY WITHIN GCCP RECORD       *      ELTMEDNC
00540 *                                                          *      ELTMEDNC
00541 ************************************************************      ELTMEDNC
00542  OBTAIN-MEDICAL-NECESSITY-WITHI.                                  ELTMEDNC
00543      SET GSS-INDEX TO 1.                                          ELTMEDNC
00544      SEARCH GSS-ENTRY                                             ELTMEDNC
00545         AT END                                                    ELTMEDNC
00546            SET TABULAR-IS-UNDEFINED TO TRUE                       ELTMEDNC
00547        WHEN GSS-MN-PROG-CODE-CHR (GSS-INDEX)                      ELTMEDNC
00548            SET TABULAR-IS-DEFINED TO TRUE                         ELTMEDNC
00549          END-SEARCH.                                              ELTMEDNC
00550      IF TABULAR-IS-UNDEFINED                                      ELTMEDNC
00551          PERFORM SIGNAL-UNDEFINED-TABULAR.                        ELTMEDNC
00552      EJECT                                                        ELTMEDNC
00553                                                                   ELTMEDNC
00554                                                                   ELTMEDNC
00555 ************************************************************      ELTMEDNC
00556 *                                                          *      ELTMEDNC
00557 *        BUILD MEDICAL NECESSITY TEXT                      *      ELTMEDNC
00558 *                                                          *      ELTMEDNC
00559 ************************************************************      ELTMEDNC
00560  BUILD-MEDICAL-NECESSITY-TEXT.                                    ELTMEDNC
00561      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTMEDNC
00562          PERFORM GENERATE-INSTITUTIONAL.                          ELTMEDNC
00563      IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH                ELTMEDNC
00564          PERFORM GENERATE-PROFESSIONAL.                           ELTMEDNC
00565      IF GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                        ELTMEDNC
00566                 '03' OR '04' OR '06' OR '08'                      ELTMEDNC
00567          PERFORM GENERATE-SUPPLEMENTAL.                           ELTMEDNC
00568                                                                   ELTMEDNC
00569                                                                   ELTMEDNC
00570 ************************************************************      ELTMEDNC
00571 *                                                          *      ELTMEDNC
00572 *        GENERATE INSTITUTIONAL                            *      ELTMEDNC
00573 *                                                          *      ELTMEDNC
00574 ************************************************************      ELTMEDNC
00575  GENERATE-INSTITUTIONAL.                                          ELTMEDNC
00576      SET INSTITUTIONAL-SCREEN TO TRUE.                            ELTMEDNC
00577      PERFORM GENERATE-HEADINGS.                                   ELTMEDNC
00578      PERFORM TRANSLATE-DISPLAY-MEDNC-IND.                         ELTMEDNC
00579      PERFORM BUILD-INSTITUTIONAL-TEXT.                            ELTMEDNC
00580      EJECT                                                        ELTMEDNC
00581                                                                   ELTMEDNC
00582                                                                   ELTMEDNC
00583 ************************************************************      ELTMEDNC
00584 *                                                          *      ELTMEDNC
00585 *        GENERATE PROFESSIONAL                             *      ELTMEDNC
00586 *                                                          *      ELTMEDNC
00587 ************************************************************      ELTMEDNC
00588  GENERATE-PROFESSIONAL.                                           ELTMEDNC
00589      SET PROFESSIONAL-SCREEN TO TRUE.                             ELTMEDNC
00590      PERFORM GENERATE-HEADINGS.                                   ELTMEDNC
00591      PERFORM TRANSLATE-DISPLAY-MEDNC-IND.                         ELTMEDNC
00592      PERFORM BUILD-PROFESSIONAL-TEXT.                             ELTMEDNC
00593      EJECT                                                        ELTMEDNC
00594                                                                   ELTMEDNC
00595                                                                   ELTMEDNC
00596 ************************************************************      ELTMEDNC
00597 *                                                          *      ELTMEDNC
00598 *        GENERATE SUPPLEMENTAL                             *      ELTMEDNC
00599 *                                                          *      ELTMEDNC
00600 ************************************************************      ELTMEDNC
00601  GENERATE-SUPPLEMENTAL.                                           ELTMEDNC
00602      SET SUPPLEMENTAL-SCREEN TO TRUE.                             ELTMEDNC
00603      PERFORM GENERATE-HEADINGS.                                   ELTMEDNC
00604      PERFORM TRANSLATE-DISPLAY-MEDNC-IND.                         ELTMEDNC
00605      PERFORM BUILD-SUPPLEMENTAL-TEXT.                             ELTMEDNC
00606      EJECT                                                        ELTMEDNC
00607                                                                   ELTMEDNC
00608                                                                   ELTMEDNC
00609 ************************************************************      ELTMEDNC
00610 *                                                          *      ELTMEDNC
00611 *        GENERATE HEADINGS                                 *      ELTMEDNC
00612 *                                                          *      ELTMEDNC
00613 ************************************************************      ELTMEDNC
00614  GENERATE-HEADINGS.                                               ELTMEDNC
00615      SET COF-NEW-PAGE TO TRUE.                                    ELTMEDNC
00616      IF INSTITUTIONAL-SCREEN                                      ELTMEDNC
00617          PERFORM MOVE-INST-HEADINGS                               ELTMEDNC
00618      ELSE IF PROFESSIONAL-SCREEN                                  ELTMEDNC
00619          PERFORM MOVE-PROF-HEADINGS                               ELTMEDNC
00620      ELSE IF SUPPLEMENTAL-SCREEN                                  ELTMEDNC
00621          PERFORM MOVE-SUPP-HEADINGS.                              ELTMEDNC
00622      MOVE 2            TO COF-NBR-HDR-LINES.                      ELTMEDNC
00623      MOVE +1           TO COF-NBR-DTL-LINES.                      ELTMEDNC
00624      MOVE WS-HDR-LN2      TO COF-HDR-LINE                         ELTMEDNC
00625          (COF-NBR-HDR-LINES).                                     ELTMEDNC
00626      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMEDNC
00627      PERFORM LINK-TO-OUTPUT.                                      ELTMEDNC
00628      EJECT                                                        ELTMEDNC
00629                                                                   ELTMEDNC
00630                                                                   ELTMEDNC
00631 ************************************************************      ELTMEDNC
00632 *                                                          *      ELTMEDNC
00633 *        TRANSLATE DISPLAY MEDNC IND                       *      ELTMEDNC
00634 *                                                          *      ELTMEDNC
00635 ************************************************************      ELTMEDNC
00636  TRANSLATE-DISPLAY-MEDNC-IND.                                     ELTMEDNC
00637      INITIALIZE TCAR-FROM-AREA.                                   ELTMEDNC
00638      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMEDNC
00639      MOVE WS-MEDNC-APPLIES TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELTMEDNC
00640      ADD  +1 TO TCAR-FROM-SUB.                                    ELTMEDNC
00641      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMEDNC
00642      SET PERIOD-NEEDED TO TRUE.                                   ELTMEDNC
00643      MOVE GCG-MED-NECESSITY-HCNR-IPS-IN TO CMF-CODE-VALUE.        ELTMEDNC
00644      MOVE 'MED-NECESSITY-HCNR-IPS-IN' TO CMF-ELEMENT-SYSTEM-NAME. ELTMEDNC
00645      MOVE PC-GRP TO CMF-RECORD-PREFIX.                            ELTMEDNC
00646      EXEC CICS LINK                                               ELTMEDNC
00647                PROGRAM ('ELUCMIF')                                ELTMEDNC
00648                COMMAREA (DFHCOMMAREA)                             ELTMEDNC
00649         END-EXEC.                                                 ELTMEDNC
00650      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMEDNC
00651      MOVE SPACE TO ADDITIONAL-TEXT-SW.                            ELTMEDNC
00652                                                                   ELTMEDNC
00653                                                                   ELTMEDNC
00654 ************************************************************      ELTMEDNC
00655 *                                                          *      ELTMEDNC
00656 *        MOVE INST HEADINGS                                *      ELTMEDNC
00657 *                                                          *      ELTMEDNC
00658 ************************************************************      ELTMEDNC
00659  MOVE-INST-HEADINGS.                                              ELTMEDNC
00660      MOVE 'INSTITUTIONAL' TO HDR-TITLE.                           ELTMEDNC
00661                                                                   ELTMEDNC
00662                                                                   ELTMEDNC
00663 ************************************************************      ELTMEDNC
00664 *                                                          *      ELTMEDNC
00665 *        MOVE PROF HEADINGS                                *      ELTMEDNC
00666 *                                                          *      ELTMEDNC
00667 ************************************************************      ELTMEDNC
00668  MOVE-PROF-HEADINGS.                                              ELTMEDNC
00669      MOVE 'PROFESSIONAL' TO HDR-TITLE.                            ELTMEDNC
00670                                                                   ELTMEDNC
00671                                                                   ELTMEDNC
00672 ************************************************************      ELTMEDNC
00673 *                                                          *      ELTMEDNC
00674 *        MOVE SUPP HEADINGS                                *      ELTMEDNC
00675 *                                                          *      ELTMEDNC
00676 ************************************************************      ELTMEDNC
00677  MOVE-SUPP-HEADINGS.                                              ELTMEDNC
00678      MOVE 'SUPPLEMENTAL' TO HDR-TITLE.                            ELTMEDNC
00679      EJECT                                                        ELTMEDNC
00680                                                                   ELTMEDNC
00681                                                                   ELTMEDNC
00682 ************************************************************      ELTMEDNC
00683 *                                                          *      ELTMEDNC
00684 *        BUILD INSTITUTIONAL TEXT                          *      ELTMEDNC
00685 *                                                          *      ELTMEDNC
00686 ************************************************************      ELTMEDNC
00687  BUILD-INSTITUTIONAL-TEXT.                                        ELTMEDNC
00688      IF GSS-MN-BC-PAYMENT-IND (GSS-INDEX) EQUAL ZEROES OR         ELTMEDNC
00689          SPACES                                                   ELTMEDNC
00690                 OR LOW-VALUES                                     ELTMEDNC
00691          PERFORM SIGNAL-NOT-APPLICABLE-BC                         ELTMEDNC
00692      ELSE                                                         ELTMEDNC
00693          PERFORM CONSTRUCT-BC-TEXT-AND-SCREEN.                    ELTMEDNC
00694                                                                   ELTMEDNC
00695                                                                   ELTMEDNC
00696 ************************************************************      ELTMEDNC
00697 *                                                          *      ELTMEDNC
00698 *        BUILD PROFESSIONAL TEXT                           *      ELTMEDNC
00699 *                                                          *      ELTMEDNC
00700 ************************************************************      ELTMEDNC
00701  BUILD-PROFESSIONAL-TEXT.                                         ELTMEDNC
00702      IF GSS-MN-BS-PAYMENT-IND (GSS-INDEX) EQUAL ZEROES OR         ELTMEDNC
00703          SPACES                                                   ELTMEDNC
00704                 OR LOW-VALUES                                     ELTMEDNC
00705          PERFORM SIGNAL-NOT-APPLICABLE-BS                         ELTMEDNC
00706      ELSE                                                         ELTMEDNC
00707          PERFORM CONSTRUCT-BS-TEXT-AND-SCREEN.                    ELTMEDNC
00708                                                                   ELTMEDNC
00709                                                                   ELTMEDNC
00710 ************************************************************      ELTMEDNC
00711 *                                                          *      ELTMEDNC
00712 *        BUILD SUPPLEMENTAL TEXT                           *      ELTMEDNC
00713 *                                                          *      ELTMEDNC
00714 ************************************************************      ELTMEDNC
00715  BUILD-SUPPLEMENTAL-TEXT.                                         ELTMEDNC
00716      IF GSS-MN-MM-PAYMENT-IND (GSS-INDEX)  EQUAL ZEROES           ELTMEDNC
00717                 OR SPACES OR LOW-VALUES                           ELTMEDNC
00718          PERFORM SIGNAL-NOT-APPLICABLE-MM                         ELTMEDNC
00719      ELSE                                                         ELTMEDNC
00720          PERFORM CONSTRUCT-MM-TEXT-AND-SCREEN.                    ELTMEDNC
00721                                                                   ELTMEDNC
00722                                                                   ELTMEDNC
00723 ************************************************************      ELTMEDNC
00724 *                                                          *      ELTMEDNC
00725 *        SIGNAL NOT APPLICABLE BC                          *      ELTMEDNC
00726 *                                                          *      ELTMEDNC
00727 ************************************************************      ELTMEDNC
00728  SIGNAL-NOT-APPLICABLE-BC.                                        ELTMEDNC
00729      MOVE  +1 TO COF-NBR-DTL-LINES.                               ELTMEDNC
00730      MOVE WS-NOT-APPLICABLE-LOB-BC TO COF-DTL-LINE                ELTMEDNC
00731          (COF-NBR-DTL-LINES).                                     ELTMEDNC
00732      PERFORM LINK-TO-OUTPUT.                                      ELTMEDNC
00733                                                                   ELTMEDNC
00734                                                                   ELTMEDNC
00735 ************************************************************      ELTMEDNC
00736 *                                                          *      ELTMEDNC
00737 *        SIGNAL NOT APPLICABLE BS                          *      ELTMEDNC
00738 *                                                          *      ELTMEDNC
00739 ************************************************************      ELTMEDNC
00740  SIGNAL-NOT-APPLICABLE-BS.                                        ELTMEDNC
00741      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMEDNC
00742      MOVE WS-NOT-APPLICABLE-LOB-BS TO COF-DTL-LINE                ELTMEDNC
00743          (COF-NBR-DTL-LINES).                                     ELTMEDNC
00744      PERFORM LINK-TO-OUTPUT.                                      ELTMEDNC
00745                                                                   ELTMEDNC
00746                                                                   ELTMEDNC
00747 ************************************************************      ELTMEDNC
00748 *                                                          *      ELTMEDNC
00749 *        SIGNAL NOT APPLICABLE MM                          *      ELTMEDNC
00750 *                                                          *      ELTMEDNC
00751 ************************************************************      ELTMEDNC
00752  SIGNAL-NOT-APPLICABLE-MM.                                        ELTMEDNC
00753      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMEDNC
00754      MOVE WS-NOT-APPLICABLE-LOB-MM TO COF-DTL-LINE                ELTMEDNC
00755          (COF-NBR-DTL-LINES).                                     ELTMEDNC
00756      PERFORM LINK-TO-OUTPUT.                                      ELTMEDNC
00757                                                                   ELTMEDNC
00758                                                                   ELTMEDNC
00759 ************************************************************      ELTMEDNC
00760 *                                                          *      ELTMEDNC
00761 *        CONSTRUCT BC TEXT AND SCREEN                      *      ELTMEDNC
00762 *                                                          *      ELTMEDNC
00763 ************************************************************      ELTMEDNC
00764  CONSTRUCT-BC-TEXT-AND-SCREEN.                                    ELTMEDNC
00765      IF GCG-MED-NECESSITY-HCNR-IPS-IN  EQUAL                      ELTMEDNC
00766               '0A' OR '0B' OR '06' OR '07' OR '08' OR             ELTMEDNC
00767          '09'                                                     ELTMEDNC
00768          PERFORM TRANSLATE-BC-EASY-RIDER-TEXT                     ELTMEDNC
00769      ELSE                                                         ELTMEDNC
00770          PERFORM TRANSLATE-BC-HARD-RIDER-TEXT.                    ELTMEDNC
00771                                                                   ELTMEDNC
00772                                                                   ELTMEDNC
00773 ************************************************************      ELTMEDNC
00774 *                                                          *      ELTMEDNC
00775 *        TRANSLATE BC EASY RIDER TEXT                      *      ELTMEDNC
00776 *                                                          *      ELTMEDNC
00777 ************************************************************      ELTMEDNC
00778  TRANSLATE-BC-EASY-RIDER-TEXT.                                    ELTMEDNC
00779      IF GSS-MN-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTMEDNC
00780          ZERO                                                     ELTMEDNC
00781                 AND SPACES AND LOW-VALUES                         ELTMEDNC
00782          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMEDNC
00783      PERFORM TRANSLATE-BC-INDICATOR.                              ELTMEDNC
00784      EJECT                                                        ELTMEDNC
00785                                                                   ELTMEDNC
00786                                                                   ELTMEDNC
00787 ************************************************************      ELTMEDNC
00788 *                                                          *      ELTMEDNC
00789 *        TRANSLATE BC HARD RIDER TEXT                      *      ELTMEDNC
00790 *                                                          *      ELTMEDNC
00791 ************************************************************      ELTMEDNC
00792  TRANSLATE-BC-HARD-RIDER-TEXT.                                    ELTMEDNC
00793      IF GSS-MN-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTMEDNC
00794          ZERO                                                     ELTMEDNC
00795                 AND SPACES AND LOW-VALUES                         ELTMEDNC
00796          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMEDNC
00797      PERFORM TRANSLATE-BC-INDICATOR.                              ELTMEDNC
00798      IF GSS-MN-BC-PAYMENT-IND (GSS-INDEX) NOT EQUAL ZERO          ELTMEDNC
00799                 AND SPACES AND LOW-VALUES                         ELTMEDNC
00800          PERFORM TRANSLATE-BC-PAYMENT-INDICATOR.                  ELTMEDNC
00801      SET SRP-ACCUM-PROV-CLASS-INST TO TRUE.                       ELTMEDNC
00802      PERFORM GENERATE-ASSOCIATED-ACCUMLATOR.                      ELTMEDNC
00803      IF GSS-MN-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL ZERO    ELTMEDNC
00804          AND                                                      ELTMEDNC
00805                SPACES AND LOW-VALUES                              ELTMEDNC
00806          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTMEDNC
00807      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                       ELTMEDNC
00808               '03' OR '04' OR '06' OR '08')                       ELTMEDNC
00809            AND                                                    ELTMEDNC
00810             (GSS-MN-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL          ELTMEDNC
00811          ZERO                                                     ELTMEDNC
00812                         AND SPACES AND LOW-VALUES)                ELTMEDNC
00813          PERFORM GENERATE-SPILL-OVER-INDICATOR.                   ELTMEDNC
00814      PERFORM GENERATE-GMDN-TABULAR.                               ELTMEDNC
00815      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTMEDNC
00816      MOVE SPACES TO SCREEN-TYPE.                                  ELTMEDNC
00817      EJECT                                                        ELTMEDNC
00818                                                                   ELTMEDNC
00819                                                                   ELTMEDNC
00820 ************************************************************      ELTMEDNC
00821 *                                                          *      ELTMEDNC
00822 *        CONSTRUCT BS TEXT AND SCREEN                      *      ELTMEDNC
00823 *                                                          *      ELTMEDNC
00824 ************************************************************      ELTMEDNC
00825  CONSTRUCT-BS-TEXT-AND-SCREEN.                                    ELTMEDNC
00826      IF GCG-MED-NECESSITY-HCNR-IPS-IN  EQUAL                      ELTMEDNC
00827               '0A' OR '0B' OR '06' OR '07' OR '08' OR             ELTMEDNC
00828          '09'                                                     ELTMEDNC
00829          PERFORM TRANSLATE-BS-EASY-RIDER-TEXT                     ELTMEDNC
00830      ELSE                                                         ELTMEDNC
00831          PERFORM TRANSLATE-BS-HARD-RIDER-TEXT.                    ELTMEDNC
00832                                                                   ELTMEDNC
00833                                                                   ELTMEDNC
00834 ************************************************************      ELTMEDNC
00835 *                                                          *      ELTMEDNC
00836 *        TRANSLATE BS EASY RIDER TEXT                      *      ELTMEDNC
00837 *                                                          *      ELTMEDNC
00838 ************************************************************      ELTMEDNC
00839  TRANSLATE-BS-EASY-RIDER-TEXT.                                    ELTMEDNC
00840      IF GSS-MN-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTMEDNC
00841          ZERO                                                     ELTMEDNC
00842                 AND SPACES AND LOW-VALUES                         ELTMEDNC
00843          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMEDNC
00844      PERFORM TRANSLATE-BS-INDICATOR.                              ELTMEDNC
00845      EJECT                                                        ELTMEDNC
00846                                                                   ELTMEDNC
00847                                                                   ELTMEDNC
00848 ************************************************************      ELTMEDNC
00849 *                                                          *      ELTMEDNC
00850 *        TRANSLATE BS HARD RIDER TEXT                      *      ELTMEDNC
00851 *                                                          *      ELTMEDNC
00852 ************************************************************      ELTMEDNC
00853  TRANSLATE-BS-HARD-RIDER-TEXT.                                    ELTMEDNC
00854      IF GSS-MN-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTMEDNC
00855          ZERO                                                     ELTMEDNC
00856                 AND SPACES AND LOW-VALUES                         ELTMEDNC
00857          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMEDNC
00858      PERFORM TRANSLATE-BS-INDICATOR.                              ELTMEDNC
00859      IF GSS-MN-BS-PAYMENT-IND (GSS-INDEX) NOT EQUAL ZERO          ELTMEDNC
00860                 AND SPACES AND LOW-VALUES                         ELTMEDNC
00861          PERFORM TRANSLATE-BS-PAYMENT-INDICATOR.                  ELTMEDNC
00862      SET SRP-ACCUM-PROV-CLASS-PROF TO TRUE.                       ELTMEDNC
00863      PERFORM GENERATE-ASSOCIATED-ACCUMLATOR.                      ELTMEDNC
00864      IF GSS-MN-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL ZERO    ELTMEDNC
00865          AND                                                      ELTMEDNC
00866                SPACES AND LOW-VALUES                              ELTMEDNC
00867          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTMEDNC
00868      IF (GCG-L-O-B-CONTRACT-LEVEL-IND EQUAL                       ELTMEDNC
00869                   '03' OR '04' OR '06' OR '08')                   ELTMEDNC
00870            AND                                                    ELTMEDNC
00871             (GSS-MN-SPILL-OVER-IND (GSS-INDEX) NOT EQUAL          ELTMEDNC
00872          ZERO                                                     ELTMEDNC
00873                         AND SPACES AND LOW-VALUES)                ELTMEDNC
00874          PERFORM GENERATE-SPILL-OVER-INDICATOR.                   ELTMEDNC
00875      PERFORM GENERATE-GMDN-TABULAR.                               ELTMEDNC
00876      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTMEDNC
00877      MOVE SPACES TO SCREEN-TYPE.                                  ELTMEDNC
00878      EJECT                                                        ELTMEDNC
00879                                                                   ELTMEDNC
00880                                                                   ELTMEDNC
00881 ************************************************************      ELTMEDNC
00882 *                                                          *      ELTMEDNC
00883 *        CONSTRUCT MM TEXT AND SCREEN                      *      ELTMEDNC
00884 *                                                          *      ELTMEDNC
00885 ************************************************************      ELTMEDNC
00886  CONSTRUCT-MM-TEXT-AND-SCREEN.                                    ELTMEDNC
00887      IF GCG-MED-NECESSITY-HCNR-IPS-IN  EQUAL                      ELTMEDNC
00888               '0A' OR '0B' OR '06' OR '07' OR '08' OR             ELTMEDNC
00889          '09'                                                     ELTMEDNC
00890          PERFORM TRANSLATE-MM-EASY-RIDER-TEXT                     ELTMEDNC
00891      ELSE                                                         ELTMEDNC
00892          PERFORM TRANSLATE-MM-HARD-RIDER-TEXT.                    ELTMEDNC
00893      EJECT                                                        ELTMEDNC
00894                                                                   ELTMEDNC
00895                                                                   ELTMEDNC
00896 ************************************************************      ELTMEDNC
00897 *                                                          *      ELTMEDNC
00898 *        GENERATE DISCLAIMER SENTENCE                      *      ELTMEDNC
00899 *                                                          *      ELTMEDNC
00900 ************************************************************      ELTMEDNC
00901  GENERATE-DISCLAIMER-SENTENCE.                                    ELTMEDNC
00902      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMEDNC
00903      MOVE WS-DISCLAIMER TO COF-DTL-LINE                           ELTMEDNC
00904          (COF-NBR-DTL-LINES).                                     ELTMEDNC
00905      PERFORM LINK-TO-OUTPUT.                                      ELTMEDNC
00906      EJECT                                                        ELTMEDNC
00907                                                                   ELTMEDNC
00908                                                                   ELTMEDNC
00909 ************************************************************      ELTMEDNC
00910 *                                                          *      ELTMEDNC
00911 *        GENERATE ASSOCIATED ACCUMLATORS                   *      ELTMEDNC
00912 *                                                          *      ELTMEDNC
00913 ************************************************************      ELTMEDNC
00914  GENERATE-ASSOCIATED-ACCUMLATOR.                                  ELTMEDNC
00915      PERFORM GENERATE-COINSURANCE-TEXT.                           ELTMEDNC
00916      PERFORM GENERATE-COPAY-TEXT.                                 ELTMEDNC
00917      PERFORM GENERATE-DEDUCTIBLE-TEXT.                            ELTMEDNC
00918      PERFORM GENERATE-BENEFIT-MAXIMUMS-TEXT.                      ELTMEDNC
00919                                                                   ELTMEDNC
00920                                                                   ELTMEDNC
00921 ************************************************************      ELTMEDNC
00922 *                                                          *      ELTMEDNC
00923 *        TRANSLATE MM EASY RIDER TEXT                      *      ELTMEDNC
00924 *                                                          *      ELTMEDNC
00925 ************************************************************      ELTMEDNC
00926  TRANSLATE-MM-EASY-RIDER-TEXT.                                    ELTMEDNC
00927      IF GSS-MN-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTMEDNC
00928          ZERO                                                     ELTMEDNC
00929                 AND SPACES AND LOW-VALUES                         ELTMEDNC
00930          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMEDNC
00931      PERFORM TRANSLATE-MM-INDICATOR.                              ELTMEDNC
00932      EJECT                                                        ELTMEDNC
00933                                                                   ELTMEDNC
00934                                                                   ELTMEDNC
00935 ************************************************************      ELTMEDNC
00936 *                                                          *      ELTMEDNC
00937 *        TRANSLATE MM HARD RIDER TEXT                      *      ELTMEDNC
00938 *                                                          *      ELTMEDNC
00939 ************************************************************      ELTMEDNC
00940  TRANSLATE-MM-HARD-RIDER-TEXT.                                    ELTMEDNC
00941      IF GSS-MN-APPROVAL-SOURCE-IND (GSS-INDEX) NOT EQUAL          ELTMEDNC
00942          ZERO                                                     ELTMEDNC
00943                 AND SPACES AND LOW-VALUES                         ELTMEDNC
00944          PERFORM TRANSLATE-APPROVAL-SOURCE.                       ELTMEDNC
00945      PERFORM TRANSLATE-MM-INDICATOR.                              ELTMEDNC
00946      IF GSS-MN-MM-PAYMENT-IND (GSS-INDEX) NOT EQUAL ZERO          ELTMEDNC
00947                 AND SPACES AND LOW-VALUES                         ELTMEDNC
00948          PERFORM TRANSLATE-MM-PAYMENT-INDICATOR.                  ELTMEDNC
00949      SET SRP-ACCUM-PROV-CLASS-SUPP TO TRUE.                       ELTMEDNC
00950      PERFORM GENERATE-ASSOCIATED-ACCUMLATOR.                      ELTMEDNC
00951      IF GSS-MN-COMB-BENE-REDUCT-IND (GSS-INDEX) NOT EQUAL ZERO    ELTMEDNC
00952          AND                                                      ELTMEDNC
00953                SPACES AND LOW-VALUES                              ELTMEDNC
00954          PERFORM GENERATE-COMBINED-BENEFITS-RED.                  ELTMEDNC
00955      PERFORM GENERATE-DISCLAIMER-SENTENCE.                        ELTMEDNC
00956      EJECT                                                        ELTMEDNC
00957                                                                   ELTMEDNC
00958                                                                   ELTMEDNC
00959 ************************************************************      ELTMEDNC
00960 *                                                          *      ELTMEDNC
00961 *        TRANSLATE APPROVAL SOURCE                         *      ELTMEDNC
00962 *                                                          *      ELTMEDNC
00963 ************************************************************      ELTMEDNC
00964  TRANSLATE-APPROVAL-SOURCE.                                       ELTMEDNC
00965      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMEDNC
00966      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMEDNC
00967      SET ADDITIONAL-TEXT TO TRUE.                                 ELTMEDNC
00968      INITIALIZE WS-PERIOD-SW.                                     ELTMEDNC
00969      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMEDNC
00970      MOVE WS-APPROVAL-SOURCE TO TCAR-FROM-LINE                    ELTMEDNC
00971          (TCAR-FROM-SUB).                                         ELTMEDNC
00972      ADD 1 TO TCAR-FROM-SUB.                                      ELTMEDNC
00973      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTMEDNC
00974      IF NOT-HOLDING-APPROVAL-SRCE                                 ELTMEDNC
00975          PERFORM GET-APPROVAL-SOURCE                              ELTMEDNC
00976      ELSE                                                         ELTMEDNC
00977          PERFORM USE-EXISTING-APPROVAL-TRANSLAT.                  ELTMEDNC
00978      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMEDNC
00979      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTMEDNC
00980      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMEDNC
00981                            WS-POINTER3.                           ELTMEDNC
00982      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMEDNC
00983      MOVE WS-APPROVAL-SOURCEA TO TCAR-FROM-LINE                   ELTMEDNC
00984          (TCAR-FROM-SUB).                                         ELTMEDNC
00985      PERFORM REFORMAT-AND-WRITE-TEXT.                             ELTMEDNC
00986      EJECT                                                        ELTMEDNC
00987                                                                   ELTMEDNC
00988                                                                   ELTMEDNC
00989 ************************************************************      ELTMEDNC
00990 *                                                          *      ELTMEDNC
00991 *        GET APPROVAL SOURCE                               *      ELTMEDNC
00992 *                                                          *      ELTMEDNC
00993 ************************************************************      ELTMEDNC
00994  GET-APPROVAL-SOURCE.                                             ELTMEDNC
00995      MOVE GSS-MN-APPROVAL-SOURCE-IND (GSS-INDEX) TO               ELTMEDNC
00996          CMF-CODE-VALUE.                                          ELTMEDNC
00997      MOVE 'MN-APPROVAL-SOURCE-IND' TO CMF-ELEMENT-SYSTEM-NAME.    ELTMEDNC
00998      PERFORM LINK-TO-TRANSLATOR.                                  ELTMEDNC
00999      SET HOLDING-APPROVAL-SOURCE TO TRUE.                         ELTMEDNC
01000      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTMEDNC
01001      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMEDNC
01002                            ADDRESS OF CMF-DESCR.                  ELTMEDNC
01003      SET CIA-ELSPGMW2-DDN TO TRUE.                                ELTMEDNC
01004      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMEDNC
01005                            ADDRESS OF CMF-DESCR.                  ELTMEDNC
01006      SET WS-POINTER2 TO ADDRESS OF CMF-DESCR.                     ELTMEDNC
01007      EJECT                                                        ELTMEDNC
01008                                                                   ELTMEDNC
01009                                                                   ELTMEDNC
01010 ************************************************************      ELTMEDNC
01011 *                                                          *      ELTMEDNC
01012 *        USE EXISTING APPROVAL TRANSLATION                 *      ELTMEDNC
01013 *                                                          *      ELTMEDNC
01014 ************************************************************      ELTMEDNC
01015  USE-EXISTING-APPROVAL-TRANSLAT.                                  ELTMEDNC
01016      SET CIA-ELSPGMW3-DDN TO TRUE.                                ELTMEDNC
01017      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMEDNC
01018                            ADDRESS OF CMF-DESCR.                  ELTMEDNC
01019      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTMEDNC
01020      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMEDNC
01021                            WS-POINTER2.                           ELTMEDNC
01022                                                                   ELTMEDNC
01023                                                                   ELTMEDNC
01024 ************************************************************      ELTMEDNC
01025 *                                                          *      ELTMEDNC
01026 *        GET INDICATOR SENTENCE                            *      ELTMEDNC
01027 *                                                          *      ELTMEDNC
01028 ************************************************************      ELTMEDNC
01029  GET-INDICATOR-SENTENCE.                                          ELTMEDNC
01030      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMEDNC
01031      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMEDNC
01032      SET PERIOD-NEEDED TO TRUE.                                   ELTMEDNC
01033      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMEDNC
01034      MOVE WS-INDICATOR TO TCAR-FROM-LINE                          ELTMEDNC
01035          (TCAR-FROM-SUB).                                         ELTMEDNC
01036      ADD 1 TO TCAR-FROM-SUB.                                      ELTMEDNC
01037      EJECT                                                        ELTMEDNC
01038                                                                   ELTMEDNC
01039                                                                   ELTMEDNC
01040 ************************************************************      ELTMEDNC
01041 *                                                          *      ELTMEDNC
01042 *        TRANSLATE BC INDICATOR                            *      ELTMEDNC
01043 *                                                          *      ELTMEDNC
01044 ************************************************************      ELTMEDNC
01045  TRANSLATE-BC-INDICATOR.                                          ELTMEDNC
01046      PERFORM GET-INDICATOR-SENTENCE.                              ELTMEDNC
01047      MOVE 'MN-BC-IND'  TO CMF-ELEMENT-SYSTEM-NAME.                ELTMEDNC
01048      MOVE GSS-MN-BC-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTMEDNC
01049      PERFORM LINK-TO-TRANSLATOR.                                  ELTMEDNC
01050      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMEDNC
01051      EJECT                                                        ELTMEDNC
01052                                                                   ELTMEDNC
01053                                                                   ELTMEDNC
01054 ************************************************************      ELTMEDNC
01055 *                                                          *      ELTMEDNC
01056 *        TRANSLATE BC PAYMENT INDICATOR                    *      ELTMEDNC
01057 *                                                          *      ELTMEDNC
01058 ************************************************************      ELTMEDNC
01059  TRANSLATE-BC-PAYMENT-INDICATOR.                                  ELTMEDNC
01060      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMEDNC
01061      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMEDNC
01062      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMEDNC
01063      SET PERIOD-NEEDED TO TRUE.                                   ELTMEDNC
01064      MOVE 'MN-BC-PAYMENT-IND'  TO CMF-ELEMENT-SYSTEM-NAME.        ELTMEDNC
01065      MOVE GSS-MN-BC-PAYMENT-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMEDNC
01066      PERFORM LINK-TO-TRANSLATOR.                                  ELTMEDNC
01067      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMEDNC
01068                                                                   ELTMEDNC
01069                                                                   ELTMEDNC
01070 ************************************************************      ELTMEDNC
01071 *                                                          *      ELTMEDNC
01072 *        GENERATE COINSURANCE TEXT                         *      ELTMEDNC
01073 *                                                          *      ELTMEDNC
01074 ************************************************************      ELTMEDNC
01075  GENERATE-COINSURANCE-TEXT.                                       ELTMEDNC
01076      EXEC CICS LINK                                               ELTMEDNC
01077          PROGRAM ('ELGACLCC')                                     ELTMEDNC
01078          COMMAREA (DFHCOMMAREA)                                   ELTMEDNC
01079          END-EXEC.                                                ELTMEDNC
01080                                                                   ELTMEDNC
01081 ************************************************************      ELTMEDNC
01082 *                                                          *      ELTMEDNC
01083 *        GENERATE COPAY TEXT                               *      ELTMEDNC
01084 *                                                          *      ELTMEDNC
01085 ************************************************************      ELTMEDNC
01086  GENERATE-COPAY-TEXT.                                             ELTMEDNC
01087      EXEC CICS LINK                                               ELTMEDNC
01088          PROGRAM ('ELGACPCC')                                     ELTMEDNC
01089          COMMAREA (DFHCOMMAREA)                                   ELTMEDNC
01090          END-EXEC.                                                ELTMEDNC
01091                                                                   ELTMEDNC
01092                                                                   ELTMEDNC
01093 ************************************************************      ELTMEDNC
01094 *                                                          *      ELTMEDNC
01095 *        GENERATE DEDUCTIBLE TEXT                          *      ELTMEDNC
01096 *                                                          *      ELTMEDNC
01097 ************************************************************      ELTMEDNC
01098  GENERATE-DEDUCTIBLE-TEXT.                                        ELTMEDNC
01099      EXEC CICS LINK                                               ELTMEDNC
01100          PROGRAM ('ELGADLCC')                                     ELTMEDNC
01101          COMMAREA (DFHCOMMAREA)                                   ELTMEDNC
01102          END-EXEC.                                                ELTMEDNC
01103                                                                   ELTMEDNC
01104                                                                   ELTMEDNC
01105 ************************************************************      ELTMEDNC
01106 *                                                          *      ELTMEDNC
01107 *        GENERATE BENEFIT MAXIMUMS TEXT                    *      ELTMEDNC
01108 *                                                          *      ELTMEDNC
01109 ************************************************************      ELTMEDNC
01110  GENERATE-BENEFIT-MAXIMUMS-TEXT.                                  ELTMEDNC
01111      EXEC CICS LINK                                               ELTMEDNC
01112          PROGRAM ('ELGABMCC')                                     ELTMEDNC
01113          COMMAREA (DFHCOMMAREA)                                   ELTMEDNC
01114          END-EXEC.                                                ELTMEDNC
01115      EJECT                                                        ELTMEDNC
01116                                                                   ELTMEDNC
01117                                                                   ELTMEDNC
01118 ************************************************************      ELTMEDNC
01119 *                                                          *      ELTMEDNC
01120 *        GENERATE COMBINED BENEFITS REDUCTION SENTENCE     *      ELTMEDNC
01121 *                                                          *      ELTMEDNC
01122 ************************************************************      ELTMEDNC
01123  GENERATE-COMBINED-BENEFITS-RED.                                  ELTMEDNC
01124      MOVE 'MN' TO SRP-COST-CONT-TYPE.                             ELTMEDNC
01125      MOVE 'MEDICAL NECESSITY PROGRAM' TO SRP-CCP-NAME.            ELTMEDNC
01126      MOVE GSS-MN-COMB-BENE-REDUCT-IND (GSS-INDEX) TO              ELTMEDNC
01127             SRP-CCP-COMB-BENE-REDUCT-IND.                         ELTMEDNC
01128      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELTMEDNC
01129      CALL 'ELUSAVAD' USING DFHCOMMAREA                            ELTMEDNC
01130                            ADDRESS OF GCCP-TABULAR-REC.           ELTMEDNC
01131      CALL 'ELGCBRI' USING DFHEIBLK                                ELTMEDNC
01132                           DFHCOMMAREA.                            ELTMEDNC
01133      EJECT                                                        ELTMEDNC
01134                                                                   ELTMEDNC
01135                                                                   ELTMEDNC
01136 ************************************************************      ELTMEDNC
01137 *                                                          *      ELTMEDNC
01138 *        GENERATE RELATED SERVICES                         *      ELTMEDNC
01139 *                                                          *      ELTMEDNC
01140 ************************************************************      ELTMEDNC
01141  GENERATE-RELATED-SERVICES.                                       ELTMEDNC
01142      MOVE 'MEDICAL NECESSITY' TO SRP-CCP-NAME.                    ELTMEDNC
01143      MOVE WS-GMDN-SRVS-ID TO SRP-TABULAR-ID.                      ELTMEDNC
01144      MOVE WS-GMDN-SRVS-SLOT-NO TO SRP-TABULAR-SLOT-NO.            ELTMEDNC
01145      EXEC CICS LINK                                               ELTMEDNC
01146          PROGRAM ('ELGGXXB')                                      ELTMEDNC
01147          COMMAREA (DFHCOMMAREA)                                   ELTMEDNC
01148          END-EXEC.                                                ELTMEDNC
01149                                                                   ELTMEDNC
01150                                                                   ELTMEDNC
01151 ************************************************************      ELTMEDNC
01152 *                                                          *      ELTMEDNC
01153 *        TRANSLATE BS INDICATOR                            *      ELTMEDNC
01154 *                                                          *      ELTMEDNC
01155 ************************************************************      ELTMEDNC
01156  TRANSLATE-BS-INDICATOR.                                          ELTMEDNC
01157      PERFORM GET-INDICATOR-SENTENCE.                              ELTMEDNC
01158      MOVE 'MN-BS-IND' TO CMF-ELEMENT-SYSTEM-NAME.                 ELTMEDNC
01159      MOVE GSS-MN-BS-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTMEDNC
01160      PERFORM LINK-TO-TRANSLATOR.                                  ELTMEDNC
01161      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMEDNC
01162      EJECT                                                        ELTMEDNC
01163                                                                   ELTMEDNC
01164                                                                   ELTMEDNC
01165 ************************************************************      ELTMEDNC
01166 *                                                          *      ELTMEDNC
01167 *        TRANSLATE BS PAYMENT INDICATOR                    *      ELTMEDNC
01168 *                                                          *      ELTMEDNC
01169 ************************************************************      ELTMEDNC
01170  TRANSLATE-BS-PAYMENT-INDICATOR.                                  ELTMEDNC
01171      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMEDNC
01172      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMEDNC
01173      SET PERIOD-NEEDED TO TRUE.                                   ELTMEDNC
01174      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMEDNC
01175      MOVE 'MN-BS-PAYMENT-IND'  TO                                 ELTMEDNC
01176          CMF-ELEMENT-SYSTEM-NAME.                                 ELTMEDNC
01177      MOVE GSS-MN-BS-PAYMENT-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMEDNC
01178      PERFORM LINK-TO-TRANSLATOR.                                  ELTMEDNC
01179      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMEDNC
01180                                                                   ELTMEDNC
01181                                                                   ELTMEDNC
01182 ************************************************************      ELTMEDNC
01183 *                                                          *      ELTMEDNC
01184 *        TRANSLATE MM INDICATOR                            *      ELTMEDNC
01185 *                                                          *      ELTMEDNC
01186 ************************************************************      ELTMEDNC
01187  TRANSLATE-MM-INDICATOR.                                          ELTMEDNC
01188      PERFORM GET-INDICATOR-SENTENCE.                              ELTMEDNC
01189      MOVE 'MN-MM-IND'  TO CMF-ELEMENT-SYSTEM-NAME.                ELTMEDNC
01190      MOVE GSS-MN-MM-IND (GSS-INDEX) TO CMF-CODE-VALUE.            ELTMEDNC
01191      PERFORM LINK-TO-TRANSLATOR.                                  ELTMEDNC
01192      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMEDNC
01193      EJECT                                                        ELTMEDNC
01194                                                                   ELTMEDNC
01195                                                                   ELTMEDNC
01196 ************************************************************      ELTMEDNC
01197 *                                                          *      ELTMEDNC
01198 *        TRANSLATE MM PAYMENT INDICATOR                    *      ELTMEDNC
01199 *                                                          *      ELTMEDNC
01200 ************************************************************      ELTMEDNC
01201  TRANSLATE-MM-PAYMENT-INDICATOR.                                  ELTMEDNC
01202      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMEDNC
01203      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMEDNC
01204      SET PERIOD-NEEDED TO TRUE.                                   ELTMEDNC
01205      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMEDNC
01206      MOVE 'MN-MM-PAYMENT-IND'  TO CMF-ELEMENT-SYSTEM-NAME.        ELTMEDNC
01207      MOVE GSS-MN-MM-PAYMENT-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMEDNC
01208      PERFORM LINK-TO-TRANSLATOR.                                  ELTMEDNC
01209      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMEDNC
01210      EJECT                                                        ELTMEDNC
01211                                                                   ELTMEDNC
01212                                                                   ELTMEDNC
01213 ************************************************************      ELTMEDNC
01214 *                                                          *      ELTMEDNC
01215 *        GENERATE GMDN TABULAR                             *      ELTMEDNC
01216 *                                                          *      ELTMEDNC
01217 ************************************************************      ELTMEDNC
01218  GENERATE-GMDN-TABULAR.                                           ELTMEDNC
01219      SET GCG-INDEX TO 1.                                          ELTMEDNC
01220      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELTMEDNC
01221            AT END                                                 ELTMEDNC
01222               MOVE ZEROES TO WS-GMDN-SRVS-SLOT-NO                 ELTMEDNC
01223            WHEN GCG-TAB-ID (GCG-INDEX) EQUAL PC-GMDN              ELTMEDNC
01224               MOVE GCG-TAB-ID (GCG-INDEX) TO                      ELTMEDNC
01225          WS-GMDN-SRVS-ID                                          ELTMEDNC
01226               MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                    ELTMEDNC
01227                   TO WS-GMDN-SRVS-SLOT-NO                         ELTMEDNC
01228         END-SEARCH.                                               ELTMEDNC
01229      IF WS-GMDN-SRVS-SLOT-NO NOT EQUAL ZEROES                     ELTMEDNC
01230                   AND WS-GMDN-SRVS-ID EQUAL PC-GMDN               ELTMEDNC
01231          PERFORM DISPLAY-RELATED-SERVICES-SENTE.                  ELTMEDNC
01232      IF WS-GMDN-SRVS-SLOT-NO NOT EQUAL ZEROES                     ELTMEDNC
01233                  AND WS-GMDN-SRVS-ID EQUAL PC-GMDN                ELTMEDNC
01234          PERFORM GENERATE-RELATED-SERVICES.                       ELTMEDNC
01235                                                                   ELTMEDNC
01236                                                                   ELTMEDNC
01237 ************************************************************      ELTMEDNC
01238 *                                                          *      ELTMEDNC
01239 *        DISPLAY RELATED SERVICES SENTENCE                 *      ELTMEDNC
01240 *                                                          *      ELTMEDNC
01241 ************************************************************      ELTMEDNC
01242  DISPLAY-RELATED-SERVICES-SENTE.                                  ELTMEDNC
01243      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMEDNC
01244      MOVE WS-SPECIAL-SERVICES-MSG  TO COF-DTL-LINE                ELTMEDNC
01245          (COF-NBR-DTL-LINES).                                     ELTMEDNC
01246      PERFORM LINK-TO-OUTPUT.                                      ELTMEDNC
01247      EJECT                                                        ELTMEDNC
01248                                                                   ELTMEDNC
01249                                                                   ELTMEDNC
01250 ************************************************************      ELTMEDNC
01251 *                                                          *      ELTMEDNC
01252 *        GENERATE SPILL OVER INDICATOR                     *      ELTMEDNC
01253 *                                                          *      ELTMEDNC
01254 ************************************************************      ELTMEDNC
01255  GENERATE-SPILL-OVER-INDICATOR.                                   ELTMEDNC
01256      MOVE SPACES TO TCAR-FROM-AREA.                               ELTMEDNC
01257      SET BLANK-LINE-NEEDED TO TRUE.                               ELTMEDNC
01258      SET PERIOD-NEEDED TO TRUE.                                   ELTMEDNC
01259      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMEDNC
01260      MOVE WS-SPILLOVER-SENTENCE TO TCAR-FROM-LINE                 ELTMEDNC
01261          (TCAR-FROM-SUB).                                         ELTMEDNC
01262      ADD 1 TO TCAR-FROM-SUB.                                      ELTMEDNC
01263      MOVE 'MN-SPILL-OVER-IND'      TO CMF-ELEMENT-SYSTEM-NAME.    ELTMEDNC
01264      MOVE GSS-MN-SPILL-OVER-IND (GSS-INDEX) TO CMF-CODE-VALUE.    ELTMEDNC
01265      PERFORM LINK-TO-TRANSLATOR.                                  ELTMEDNC
01266      PERFORM MOVE-CMF-DESCRIPTION-TO-OUTPUT.                      ELTMEDNC
01267                                                                   ELTMEDNC
01268                                                                   ELTMEDNC
01269 ************************************************************      ELTMEDNC
01270 *                                                          *      ELTMEDNC
01271 *        SIGNAL NOT APPLICABLE MSG                         *      ELTMEDNC
01272 *                                                          *      ELTMEDNC
01273 ************************************************************      ELTMEDNC
01274  SIGNAL-NOT-APPLICABLE-MSG.                                       ELTMEDNC
01275      PERFORM GENERATE-HEADINGS.                                   ELTMEDNC
01276      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMEDNC
01277      MOVE WS-NOT-APPLICABLE-MSG TO COF-DTL-LINE                   ELTMEDNC
01278          (COF-NBR-DTL-LINES).                                     ELTMEDNC
01279      PERFORM LINK-TO-OUTPUT.                                      ELTMEDNC
01280                                                                   ELTMEDNC
01281                                                                   ELTMEDNC
01282 ************************************************************      ELTMEDNC
01283 *                                                          *      ELTMEDNC
01284 *        LINK TO TRANSLATOR                                *      ELTMEDNC
01285 *                                                          *      ELTMEDNC
01286 ************************************************************      ELTMEDNC
01287  LINK-TO-TRANSLATOR.                                              ELTMEDNC
01288      MOVE PC-GCCP TO CMF-RECORD-PREFIX.                           ELTMEDNC
01289      EXEC CICS LINK                                               ELTMEDNC
01290           PROGRAM('ELUCMIF')                                      ELTMEDNC
01291           COMMAREA(DFHCOMMAREA)                                   ELTMEDNC
01292           END-EXEC.                                               ELTMEDNC
01293      EJECT                                                        ELTMEDNC
01294                                                                   ELTMEDNC
01295                                                                   ELTMEDNC
01296 ************************************************************      ELTMEDNC
01297 *                                                          *      ELTMEDNC
01298 *        READ GCCP RECORD                                  *      ELTMEDNC
01299 *                                                          *      ELTMEDNC
01300 ************************************************************      ELTMEDNC
01301  READ-GCCP-RECORD.                                                ELTMEDNC
01302      SET CIA-GCTABULR-DDN TO TRUE.                                ELTMEDNC
01303      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMEDNC
01304                            ADDRESS OF                             ELTMEDNC
01305          IOP-INPUT-OUTPUT-PARAMETERS.                             ELTMEDNC
01306      SET IOP-STG-MODE-MOVE TO TRUE.                               ELTMEDNC
01307      SET IOP-RD TO TRUE.                                          ELTMEDNC
01308      SET IOP-FCQ-NONE TO TRUE.                                    ELTMEDNC
01309      SET IOP-KVQ-EQ TO TRUE.                                      ELTMEDNC
01310      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELTMEDNC
01311      PERFORM LINK-TO-I-O-PGM.                                     ELTMEDNC
01312      EJECT                                                        ELTMEDNC
01313                                                                   ELTMEDNC
01314                                                                   ELTMEDNC
01315 ************************************************************      ELTMEDNC
01316 *                                                          *      ELTMEDNC
01317 *        LINK TO I O PGM                                   *      ELTMEDNC
01318 *                                                          *      ELTMEDNC
01319 ************************************************************      ELTMEDNC
01320  LINK-TO-I-O-PGM.                                                 ELTMEDNC
01321      EXEC CICS LINK PROGRAM ('ELUIOPGM')                          ELTMEDNC
01322           COMMAREA (DFHCOMMAREA)                                  ELTMEDNC
01323           END-EXEC.                                               ELTMEDNC
01324      IF IOP-RC-OK                                                 ELTMEDNC
01325          PERFORM ESTABLISH-ADDRESSABILITY-OF-GC                   ELTMEDNC
01326      ELSE IF IOP-RC-NOTFND                                        ELTMEDNC
01327          PERFORM SIGNAL-NOT-FOUND-GCTAB                           ELTMEDNC
01328      ELSE                                                         ELTMEDNC
01329          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELTMEDNC
01330                                                                   ELTMEDNC
01331                                                                   ELTMEDNC
01332 ************************************************************      ELTMEDNC
01333 *                                                          *      ELTMEDNC
01334 *        SIGNAL CRITICAL IO ERROR                          *      ELTMEDNC
01335 *                                                          *      ELTMEDNC
01336 ************************************************************      ELTMEDNC
01337  SIGNAL-CRITICAL-IO-ERROR.                                        ELTMEDNC
01338      SET CIA-AB-CRITIO TO TRUE.                                   ELTMEDNC
01339      PERFORM SIGNAL-ABEND.                                        ELTMEDNC
01340                                                                   ELTMEDNC
01341                                                                   ELTMEDNC
01342 ************************************************************      ELTMEDNC
01343 *                                                          *      ELTMEDNC
01344 *        SIGNAL NOT FOUND GCTAB                            *      ELTMEDNC
01345 *                                                          *      ELTMEDNC
01346 ************************************************************      ELTMEDNC
01347  SIGNAL-NOT-FOUND-GCTAB.                                          ELTMEDNC
01348      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELTMEDNC
01349      PERFORM SIGNAL-ABEND.                                        ELTMEDNC
01350                                                                   ELTMEDNC
01351                                                                   ELTMEDNC
01352 ************************************************************      ELTMEDNC
01353 *                                                          *      ELTMEDNC
01354 *        ESTABLISH ADDRESSABILITY OF GCCP RECORD           *      ELTMEDNC
01355 *                                                          *      ELTMEDNC
01356 ************************************************************      ELTMEDNC
01357  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELTMEDNC
01358      SET ADDRESS OF GCCP-TABULAR-REC TO IOP-REC-PTR.              ELTMEDNC
01359      SET IOP-REC-PTR TO NULL.                                     ELTMEDNC
01360                                                                   ELTMEDNC
01361                                                                   ELTMEDNC
01362 ************************************************************      ELTMEDNC
01363 *                                                          *      ELTMEDNC
01364 *        MOVE CMF DESCRIPTION TO OUTPUT                    *      ELTMEDNC
01365 *                                                          *      ELTMEDNC
01366 ************************************************************      ELTMEDNC
01367  MOVE-CMF-DESCRIPTION-TO-OUTPUT.                                  ELTMEDNC
01368      PERFORM INITIALIZE-CMOUT.                                    ELTMEDNC
01369      PERFORM PREPARE-TEXT-FOR-OUTPUT.                             ELTMEDNC
01370      EJECT                                                        ELTMEDNC
01371                                                                   ELTMEDNC
01372                                                                   ELTMEDNC
01373 ************************************************************      ELTMEDNC
01374 *                                                          *      ELTMEDNC
01375 *        PREPARE TEXT FOR OUTPUT                           *      ELTMEDNC
01376 *                                                          *      ELTMEDNC
01377 ************************************************************      ELTMEDNC
01378  PREPARE-TEXT-FOR-OUTPUT.                                         ELTMEDNC
01379      PERFORM MOVE-CMF-TEXT-TO-OUTPUT                              ELTMEDNC
01380          UNTIL CMF-DESCR-IDX                                      ELTMEDNC
01381                                    GREATER THAN                   ELTMEDNC
01382              CMF-NBR-DESCR-LINES.                                 ELTMEDNC
01383      EJECT                                                        ELTMEDNC
01384                                                                   ELTMEDNC
01385                                                                   ELTMEDNC
01386 ************************************************************      ELTMEDNC
01387 *                                                          *      ELTMEDNC
01388 *        INITIALIZE CMOUT                                  *      ELTMEDNC
01389 *                                                          *      ELTMEDNC
01390 ************************************************************      ELTMEDNC
01391  INITIALIZE-CMOUT.                                                ELTMEDNC
01392      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTMEDNC
01393      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTMEDNC
01394          ADDRESS OF CMF-DESCR.                                    ELTMEDNC
01395      SET CMF-DESCR-IDX TO 1.                                      ELTMEDNC
01396      SET PROCESSING-CMF-TEXT TO TRUE.                             ELTMEDNC
01397                                                                   ELTMEDNC
01398                                                                   ELTMEDNC
01399 ************************************************************      ELTMEDNC
01400 *                                                          *      ELTMEDNC
01401 *        MOVE CMF TEXT TO OUTPUT                           *      ELTMEDNC
01402 *                                                          *      ELTMEDNC
01403 ************************************************************      ELTMEDNC
01404  MOVE-CMF-TEXT-TO-OUTPUT.                                         ELTMEDNC
01405      PERFORM MOVE-A-LINE.                                         ELTMEDNC
01406      IF CMF-DESCR-IDX GREATER CMF-NBR-DESCR-LINES                 ELTMEDNC
01407          PERFORM FINISH-CODES-MANUAL-TEXT.                        ELTMEDNC
01408      IF TCAR-FROM-SUB GREATER THAN 20                             ELTMEDNC
01409               OR CMF-DESCR-IDX GREATER THAN                       ELTMEDNC
01410          CMF-NBR-DESCR-LINES                                      ELTMEDNC
01411          PERFORM REFORMAT-AND-WRITE-TEXT.                         ELTMEDNC
01412                                                                   ELTMEDNC
01413                                                                   ELTMEDNC
01414 ************************************************************      ELTMEDNC
01415 *                                                          *      ELTMEDNC
01416 *        FINISH CODES MANUAL TEXT                          *      ELTMEDNC
01417 *                                                          *      ELTMEDNC
01418 ************************************************************      ELTMEDNC
01419  FINISH-CODES-MANUAL-TEXT.                                        ELTMEDNC
01420      SET DONE-PROCESSING TO TRUE.                                 ELTMEDNC
01421      IF PERIOD-NEEDED                                             ELTMEDNC
01422          PERFORM GET-AND-MOVE-PERIOD.                             ELTMEDNC
01423                                                                   ELTMEDNC
01424                                                                   ELTMEDNC
01425 ************************************************************      ELTMEDNC
01426 *                                                          *      ELTMEDNC
01427 *        GET AND MOVE PERIOD                               *      ELTMEDNC
01428 *                                                          *      ELTMEDNC
01429 ************************************************************      ELTMEDNC
01430  GET-AND-MOVE-PERIOD.                                             ELTMEDNC
01431      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT                        ELTMEDNC
01432          (TCAR-FROM-SUB).                                         ELTMEDNC
01433                                                                   ELTMEDNC
01434                                                                   ELTMEDNC
01435 ************************************************************      ELTMEDNC
01436 *                                                          *      ELTMEDNC
01437 *        SAVE LAST LINE                                    *      ELTMEDNC
01438 *                                                          *      ELTMEDNC
01439 ************************************************************      ELTMEDNC
01440  SAVE-LAST-LINE.                                                  ELTMEDNC
01441      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMEDNC
01442      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTMEDNC
01443         TO TCAR-FROM-LINE (TCAR-FROM-SUB).                        ELTMEDNC
01444      ADD 1 TO TCAR-FROM-SUB.                                      ELTMEDNC
01445      SUBTRACT 1 FROM COF-NBR-DTL-LINES.                           ELTMEDNC
01446                                                                   ELTMEDNC
01447                                                                   ELTMEDNC
01448 ************************************************************      ELTMEDNC
01449 *                                                          *      ELTMEDNC
01450 *        OUTPUT LAST LINE                                  *      ELTMEDNC
01451 *                                                          *      ELTMEDNC
01452 ************************************************************      ELTMEDNC
01453  OUTPUT-LAST-LINE.                                                ELTMEDNC
01454      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELTMEDNC
01455          TO COF-DTL-LINE (COF-NBR-DTL-LINES).                     ELTMEDNC
01456      IF BLANK-LINE-NEEDED                                         ELTMEDNC
01457          PERFORM CREATE-A-BLANK-LINE.                             ELTMEDNC
01458                                                                   ELTMEDNC
01459                                                                   ELTMEDNC
01460 ************************************************************      ELTMEDNC
01461 *                                                          *      ELTMEDNC
01462 *        CREATE A BLANK LINE                               *      ELTMEDNC
01463 *                                                          *      ELTMEDNC
01464 ************************************************************      ELTMEDNC
01465  CREATE-A-BLANK-LINE.                                             ELTMEDNC
01466      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTMEDNC
01467      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTMEDNC
01468                                                                   ELTMEDNC
01469                                                                   ELTMEDNC
01470 ************************************************************      ELTMEDNC
01471 *                                                          *      ELTMEDNC
01472 *        MOVE A LINE                                       *      ELTMEDNC
01473 *                                                          *      ELTMEDNC
01474 ************************************************************      ELTMEDNC
01475  MOVE-A-LINE.                                                     ELTMEDNC
01476      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELTMEDNC
01477          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTMEDNC
01478      SET CMF-DESCR-IDX UP BY 1.                                   ELTMEDNC
01479      ADD 1 TO TCAR-FROM-SUB.                                      ELTMEDNC
01480      EJECT                                                        ELTMEDNC
01481                                                                   ELTMEDNC
01482                                                                   ELTMEDNC
01483 ************************************************************      ELTMEDNC
01484 *                                                          *      ELTMEDNC
01485 *        REFORMAT AND WRITE TEXT                           *      ELTMEDNC
01486 *                                                          *      ELTMEDNC
01487 ************************************************************      ELTMEDNC
01488  REFORMAT-AND-WRITE-TEXT.                                         ELTMEDNC
01489      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTMEDNC
01490      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTMEDNC
01491      PERFORM UNSTRING-TEXT.                                       ELTMEDNC
01492      MOVE +1 TO TCAR-FROM-SUB.                                    ELTMEDNC
01493      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTMEDNC
01494      PERFORM GENERATE-TRANSLATED-AND-FORMAT                       ELTMEDNC
01495          UNTIL COF-NBR-DTL-LINES GREATER                          ELTMEDNC
01496                                   TCAR-OUTPUT-FIELDS-USED -       ELTMEDNC
01497              1.                                                   ELTMEDNC
01498      PERFORM DISPOSE-OF-LAST-LINE.                                ELTMEDNC
01499      PERFORM LINK-TO-OUTPUT.                                      ELTMEDNC
01500                                                                   ELTMEDNC
01501                                                                   ELTMEDNC
01502 ************************************************************      ELTMEDNC
01503 *                                                          *      ELTMEDNC
01504 *        GENERATE TRANSLATED AND FORMATTED TEXT            *      ELTMEDNC
01505 *                                                          *      ELTMEDNC
01506 ************************************************************      ELTMEDNC
01507  GENERATE-TRANSLATED-AND-FORMAT.                                  ELTMEDNC
01508      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                        ELTMEDNC
01509           COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTMEDNC
01510      ADD +1 TO TCAR-FROM-SUB.                                     ELTMEDNC
01511      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTMEDNC
01512      EJECT                                                        ELTMEDNC
01513                                                                   ELTMEDNC
01514                                                                   ELTMEDNC
01515 ************************************************************      ELTMEDNC
01516 *                                                          *      ELTMEDNC
01517 *        UNSTRING TEXT                                     *      ELTMEDNC
01518 *                                                          *      ELTMEDNC
01519 ************************************************************      ELTMEDNC
01520  UNSTRING-TEXT.                                                   ELTMEDNC
01521      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTMEDNC
01522      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTMEDNC
01523      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTMEDNC
01524      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTMEDNC
01525      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTMEDNC
01526      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTMEDNC
01527      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTMEDNC
01528      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTMEDNC
01529      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTMEDNC
01530      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTMEDNC
01531      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTMEDNC
01532      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTMEDNC
01533      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTMEDNC
01534      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTMEDNC
01535      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTMEDNC
01536      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTMEDNC
01537      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTMEDNC
01538      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTMEDNC
01539      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTMEDNC
01540      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTMEDNC
01541      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTMEDNC
01542      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTMEDNC
01543                                                                   ELTMEDNC
01544                                                                   ELTMEDNC
01545 ************************************************************      ELTMEDNC
01546 *                                                          *      ELTMEDNC
01547 *        LINK TO OUTPUT                                    *      ELTMEDNC
01548 *                                                          *      ELTMEDNC
01549 ************************************************************      ELTMEDNC
01550  LINK-TO-OUTPUT.                                                  ELTMEDNC
01551      EXEC CICS LINK                                               ELTMEDNC
01552          PROGRAM ('ELUOUTPT')                                     ELTMEDNC
01553          COMMAREA (DFHCOMMAREA)                                   ELTMEDNC
01554          END-EXEC.                                                ELTMEDNC
01555      EJECT                                                        ELTMEDNC
01556                                                                   ELTMEDNC
01557                                                                   ELTMEDNC
01558 ************************************************************      ELTMEDNC
01559 *                                                          *      ELTMEDNC
01560 *        DISPOSE OF LAST LINE                              *      ELTMEDNC
01561 *                                                          *      ELTMEDNC
01562 ************************************************************      ELTMEDNC
01563  DISPOSE-OF-LAST-LINE.                                            ELTMEDNC
01564      IF NOT ADDITIONAL-TEXT                                       ELTMEDNC
01565          PERFORM INITIALIZE-CONTINUED-SW.                         ELTMEDNC
01566      IF ADDITIONAL-TEXT OR PROCESSING-CMF-TEXT                    ELTMEDNC
01567          PERFORM SAVE-LAST-LINE                                   ELTMEDNC
01568      ELSE                                                         ELTMEDNC
01569          PERFORM OUTPUT-LAST-LINE.                                ELTMEDNC
01570                                                                   ELTMEDNC
01571                                                                   ELTMEDNC
01572 ************************************************************      ELTMEDNC
01573 *                                                          *      ELTMEDNC
01574 *        INITIALIZE CONTINUED SW                           *      ELTMEDNC
01575 *                                                          *      ELTMEDNC
01576 ************************************************************      ELTMEDNC
01577  INITIALIZE-CONTINUED-SW.                                         ELTMEDNC
01578      INITIALIZE CONTINUED-PROCESSING-SW.                          ELTMEDNC
