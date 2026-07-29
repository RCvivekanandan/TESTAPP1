00001 *      LAST MAINTENANCE TIME: 14.44.09  DATE: 01/11/91            06/29/02
00002                                                                   ELTOUTCC
00003  IDENTIFICATION DIVISION.                                            LV001
00004                                                                   ELTOUTCC
00005  PROGRAM-ID.         ELTOUTCC.                                    ELTOUTCC
00006                                                                   ELTOUTCC
00007  AUTHOR.             NINA CERVANTES                               ELTOUTCC
00008                                                                   ELTOUTCC
00009  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTOUTCC
00010                      A MUTUAL LEGAL RESERVE COMPANY               ELTOUTCC
00011                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTOUTCC
00012                      233 N. MICHIGAN AVE                          ELTOUTCC
00013                      CHICAGO, ILLINOIS 60601                      ELTOUTCC
00014                                                                   ELTOUTCC
00015  DATE-WRITTEN.       16-APR-1987.                                 ELTOUTCC
00016                                                                   ELTOUTCC
00017  DATE-COMPILED.                                                   ELTOUTCC
00018                                                                   ELTOUTCC
00019  SECURITY.           COPYRIGHT 1986,                              ELTOUTCC
00020                      HEALTH CARE SERVICE CORPORATION              ELTOUTCC
00021      SKIP3                                                        ELTOUTCC
00022 *                                                                *ELTOUTCC
00023 ******************************************************************ELTOUTCC
00024 *                                                                *ELTOUTCC
00025 *                      MAINTENANCE HISTORY                       *ELTOUTCC
00026 *                                                                *ELTOUTCC
00027 *  MOD     DATE     BY  DRPT                ACTION               *ELTOUTCC
00028 * ----- ----------- --- ----- ---------------------------------- *ELTOUTCC
00029 * 01.00 16-APR-1987 NAC       CREATED                            *ELTOUTCC
00030 *                                                                *ELTOUTCC
00031 * 02.00 05-MAY-1988 AKK       CHANGED PLELITCOMP TO ELITCOMPPL   *ELTOUTCC
00032 *                             ALSO ADDED CODE TO ACCOMODATE      *ELTOUTCC
00033 *                             SPLIT SCREEN OUTPUT FORMAT.        *ELTOUTCC
00034 * 03.00 07-JAN-1991 JPB       STORAGE MANAGEMENT CHANGES         *ELTOUTCC
00035 *                                                                *ELTOUTCC
00036 ******************************************************************ELTOUTCC
00037 /                                                                 ELTOUTCC
00038  ENVIRONMENT DIVISION.                                            ELTOUTCC
00039                                                                   ELTOUTCC
00040  CONFIGURATION SECTION.                                           ELTOUTCC
00041  SOURCE-COMPUTER.    IBM-3090.                                    ELTOUTCC
00042  OBJECT-COMPUTER.    IBM-3090.                                    ELTOUTCC
00043      EJECT                                                        ELTOUTCC
00044  DATA DIVISION.                                                   ELTOUTCC
00045  WORKING-STORAGE SECTION.                                         ELTOUTCC
00046  77  FILLER                       PIC X(30) VALUE                 ELTOUTCC
00047      '***ELTOUTCC WORKING STORAGE***'.                            ELTOUTCC
00048  77  SUB1                         PIC 99    VALUE ZEROES.         ELTOUTCC
00049  77  TEXT-SW                      PIC X     VALUE SPACES.         ELTOUTCC
00050      88  PRODUCED-NO-TEXT         VALUE 'N'.                      ELTOUTCC
00051      88  PRODUCED-TEXT            VALUE 'Y'.                      ELTOUTCC
00052  01  POINTERS.                                                    ELTOUTCC
00053      05  WS-POINTER1              POINTER.                        ELTOUTCC
00054      05  WS-POINTER2              POINTER.                        ELTOUTCC
00055                                                                   ELTOUTCC
00056  01  WS-HEADER-LINE.                                              ELTOUTCC
00057        10  FILLER                 PIC X(29) VALUE SPACES.         ELTOUTCC
00058        10  FILLER                 PIC X(22) VALUE                 ELTOUTCC
00059            'OUT OF COUNTRY CLAIMS '.                              ELTOUTCC
00060        10  FILLER                 PIC X(28) VALUE SPACES.         ELTOUTCC
00061  01  WS-DISPLAY-TEXT.                                             ELTOUTCC
00062        10  WS-INST-BASIC-HEADER   PIC X(19) VALUE                 ELTOUTCC
00063            'INSTITUTIONAL BASIC'.                                 ELTOUTCC
00064        10  WS-INST-SUPP-HEADER    PIC X(26) VALUE                 ELTOUTCC
00065            'INSTITUTIONAL SUPPLEMENTAL'.                          ELTOUTCC
00066        10  WS-PROF-BASIC-HEADER    PIC X(18) VALUE                ELTOUTCC
00067            'PROFESSIONAL BASIC'.                                  ELTOUTCC
00068        10  WS-PROF-SUPP-HEADER    PIC X(25) VALUE                 ELTOUTCC
00069            'PROFESSIONAL SUPPLEMENTAL'.                           ELTOUTCC
00070 *                                                                 ELTOUTCC
00071  01  WS-OUTPUT-AREA.                                              ELTOUTCC
00072       10  WS-LINE-CNT            PIC S9(04)  VALUE +0.            ELTOUTCC
00073       10  WS-OUTPUT              PIC X(1580) VALUE SPACES.        ELTOUTCC
00074       10 WS-OUTPUT-ENTRY REDEFINES WS-OUTPUT                      ELTOUTCC
00075                                    OCCURS 20 TIMES                ELTOUTCC
00076                                    INDEXED BY WS-OUTPUT-IDX.      ELTOUTCC
00077          15 WS-OUTPUT-LINE.                                       ELTOUTCC
00078             20  WS-HEADING         PIC X(30).                     ELTOUTCC
00079             20  WS-DIVIDER         PIC X(01).                     ELTOUTCC
00080             20  FILLER             PIC X(02).                     ELTOUTCC
00081             20  WS-TEXT            PIC X(46).                     ELTOUTCC
00082 *                                                                 ELTOUTCC
00083  01  WS-MASK-LINE.                                                ELTOUTCC
00084      10  FILLER                     PIC X(30)  VALUE SPACES.      ELTOUTCC
00085      10  FILLER                     PIC X(01)  VALUE '|'.         ELTOUTCC
00086      10  FILLER                     PIC X(48)  VALUE SPACES.      ELTOUTCC
00087 /                                                                 ELTOUTCC
00088  LINKAGE SECTION.                                                 ELTOUTCC
00089  01  DFHCOMMAREA.                                                 ELTOUTCC
00090      COPY ELSCOMMC.                                               ELTOUTCC
00091 /                                                                 ELTOUTCC
00092      COPY ELSCIA2C.                                               ELTOUTCC
00093 /                                                                 ELTOUTCC
00094      COPY ELSCMDSC.                                               ELTOUTCC
00095 /                                                                 ELTOUTCC
00096      COPY ELSCMIFC.                                               ELTOUTCC
00097 /                                                                 ELTOUTCC
00098      COPY ELSIOPMC.                                               ELTOUTCC
00099 /                                                                 ELTOUTCC
00100      COPY ELSKEYSC.                                               ELTOUTCC
00101 /                                                                 ELTOUTCC
00102      COPY ELSOUTPC.                                               ELTOUTCC
00103 /                                                                 ELTOUTCC
00104      COPY ELSTCWAC.                                               ELTOUTCC
00105 /                                                                 ELTOUTCC
00106      COPY ELSSSCBC.                                               ELTOUTCC
00107 /                                                                 ELTOUTCC
00108  01  ELR-GRP-REC-AREA.                                            ELTOUTCC
00109      COPY GCGROUPC.                                               ELTOUTCC
00110 /                                                                 ELTOUTCC
00111  01  ELR-CONTR-REC-AREA.                                          ELTOUTCC
00112      COPY GCCONTRC.                                               ELTOUTCC
00113 /                                                                 ELTOUTCC
00114      EJECT                                                        ELTOUTCC
00115  PROCEDURE DIVISION.                                              ELTOUTCC
00116 ************************************************************      ELTOUTCC
00117 *                                                          *      ELTOUTCC
00118 *                    PROCEDURE DIVISION                    *      ELTOUTCC
00119 *                                                          *      ELTOUTCC
00120 ************************************************************      ELTOUTCC
00121                                                                   ELTOUTCC
00122                                                                   ELTOUTCC
00123 ************************************************************      ELTOUTCC
00124 *                                                          *      ELTOUTCC
00125 *        TOPIC OUT OF COUNTRY                              *      ELTOUTCC
00126 *                                                          *      ELTOUTCC
00127 ************************************************************      ELTOUTCC
00128  TOPIC-OUT-OF-COUNTRY.                                            ELTOUTCC
00129      PERFORM INITIALIZATION.                                      ELTOUTCC
00130      PERFORM PROCESS.                                             ELTOUTCC
00131      GOBACK.                                                      ELTOUTCC
00132                                                                   ELTOUTCC
00133                                                                   ELTOUTCC
00134 ************************************************************      ELTOUTCC
00135 *                                                          *      ELTOUTCC
00136 *        INITIALIZATION                                    *      ELTOUTCC
00137 *                                                          *      ELTOUTCC
00138 ************************************************************      ELTOUTCC
00139  INITIALIZATION.                                                  ELTOUTCC
00140      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELTOUTCC
00141      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELTOUTCC
00142                                                                   ELTOUTCC
00143                                                                   ELTOUTCC
00144 ************************************************************      ELTOUTCC
00145 *                                                          *      ELTOUTCC
00146 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELTOUTCC
00147 *                                                          *      ELTOUTCC
00148 ************************************************************      ELTOUTCC
00149  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELTOUTCC
00150      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELTOUTCC
00151      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELTOUTCC
00152      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELTOUTCC
00153                                                                   ELTOUTCC
00154                                                                   ELTOUTCC
00155 ************************************************************      ELTOUTCC
00156 *                                                          *      ELTOUTCC
00157 *        CHECK FOR VALID COMMAREA                          *      ELTOUTCC
00158 *                                                          *      ELTOUTCC
00159 ************************************************************      ELTOUTCC
00160  CHECK-FOR-VALID-COMMAREA.                                        ELTOUTCC
00161      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTOUTCC
00162          PERFORM SIGNAL-INVALID-COMMAREA.                         ELTOUTCC
00163                                                                   ELTOUTCC
00164                                                                   ELTOUTCC
00165 ************************************************************      ELTOUTCC
00166 *                                                          *      ELTOUTCC
00167 *        SIGNAL INVALID COMMAREA                           *      ELTOUTCC
00168 *                                                          *      ELTOUTCC
00169 ************************************************************      ELTOUTCC
00170  SIGNAL-INVALID-COMMAREA.                                         ELTOUTCC
00171      EXEC CICS ABEND                                              ELTOUTCC
00172                ABCODE('EL01')                                     ELTOUTCC
00173         END-EXEC.                                                 ELTOUTCC
00174      EJECT                                                        ELTOUTCC
00175                                                                   ELTOUTCC
00176                                                                   ELTOUTCC
00177 ************************************************************      ELTOUTCC
00178 *                                                          *      ELTOUTCC
00179 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELTOUTCC
00180 *                                                          *      ELTOUTCC
00181 ************************************************************      ELTOUTCC
00182  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELTOUTCC
00183      IF ECA-CIA-PTR = NULL                                        ELTOUTCC
00184          PERFORM SIGNAL-INVALID-CIA                               ELTOUTCC
00185      ELSE                                                         ELTOUTCC
00186          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELTOUTCC
00187                                                                   ELTOUTCC
00188                                                                   ELTOUTCC
00189 ************************************************************      ELTOUTCC
00190 *                                                          *      ELTOUTCC
00191 *        SIGNAL INVALID CIA                                *      ELTOUTCC
00192 *                                                          *      ELTOUTCC
00193 ************************************************************      ELTOUTCC
00194  SIGNAL-INVALID-CIA.                                              ELTOUTCC
00195      EXEC CICS ABEND                                              ELTOUTCC
00196                ABCODE('EL02')                                     ELTOUTCC
00197         END-EXEC.                                                 ELTOUTCC
00198      EJECT                                                        ELTOUTCC
00199                                                                   ELTOUTCC
00200                                                                   ELTOUTCC
00201 ************************************************************      ELTOUTCC
00202 *                                                          *      ELTOUTCC
00203 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELTOUTCC
00204 *                                                          *      ELTOUTCC
00205 ************************************************************      ELTOUTCC
00206  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELTOUTCC
00207      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTOUTCC
00208      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOUTCC
00209                             ADDRESS OF                            ELTOUTCC
00210          SSB-SELECTOR-STATUS-CTL-BLK.                             ELTOUTCC
00211      IF CIA-RC-PTR-NULL                                           ELTOUTCC
00212          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTOUTCC
00213                                                                   ELTOUTCC
00214                                                                   ELTOUTCC
00215 ************************************************************      ELTOUTCC
00216 *                                                          *      ELTOUTCC
00217 *        SIGNAL UNALLOC AREA ERROR                         *      ELTOUTCC
00218 *                                                          *      ELTOUTCC
00219 ************************************************************      ELTOUTCC
00220  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTOUTCC
00221      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTOUTCC
00222      PERFORM SIGNAL-ABEND.                                        ELTOUTCC
00223                                                                   ELTOUTCC
00224                                                                   ELTOUTCC
00225 ************************************************************      ELTOUTCC
00226 *                                                          *      ELTOUTCC
00227 *        SIGNAL ABEND                                      *      ELTOUTCC
00228 *                                                          *      ELTOUTCC
00229 ************************************************************      ELTOUTCC
00230  SIGNAL-ABEND.                                                    ELTOUTCC
00231      EXEC CICS ABEND                                              ELTOUTCC
00232                ABCODE(CIA-ABCODE)                                 ELTOUTCC
00233         END-EXEC.                                                 ELTOUTCC
00234      EJECT                                                        ELTOUTCC
00235                                                                   ELTOUTCC
00236                                                                   ELTOUTCC
00237 ************************************************************      ELTOUTCC
00238 *                                                          *      ELTOUTCC
00239 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELTOUTCC
00240 *                                                          *      ELTOUTCC
00241 ************************************************************      ELTOUTCC
00242  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELTOUTCC
00243      PERFORM ESTABLISH-ADDRESSABILITY-OF-CO.                      ELTOUTCC
00244      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELTOUTCC
00245      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELTOUTCC
00246      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELTOUTCC
00247      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELTOUTCC
00248                                                                   ELTOUTCC
00249                                                                   ELTOUTCC
00250 ************************************************************      ELTOUTCC
00251 *                                                          *      ELTOUTCC
00252 *        ESTABLISH ADDRESSABILITY OF CODES MANUAL INTERFACE*      ELTOUTCC
00253 *                                                          *      ELTOUTCC
00254 ************************************************************      ELTOUTCC
00255  ESTABLISH-ADDRESSABILITY-OF-CO.                                  ELTOUTCC
00256      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTOUTCC
00257      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOUTCC
00258                             ADDRESS OF                            ELTOUTCC
00259          CMF-CODES-MANUAL-INTERFACE.                              ELTOUTCC
00260      IF CIA-RC-PTR-NULL                                           ELTOUTCC
00261          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTOUTCC
00262      EJECT                                                        ELTOUTCC
00263                                                                   ELTOUTCC
00264                                                                   ELTOUTCC
00265 ************************************************************      ELTOUTCC
00266 *                                                          *      ELTOUTCC
00267 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTOUTCC
00268 *                                                          *      ELTOUTCC
00269 ************************************************************      ELTOUTCC
00270  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELTOUTCC
00271      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTOUTCC
00272      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOUTCC
00273                             ADDRESS OF                            ELTOUTCC
00274          COF-OUTPUT-INTERFACE.                                    ELTOUTCC
00275      IF CIA-RC-PTR-NULL                                           ELTOUTCC
00276          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTOUTCC
00277      EJECT                                                        ELTOUTCC
00278                                                                   ELTOUTCC
00279                                                                   ELTOUTCC
00280 ************************************************************      ELTOUTCC
00281 *                                                          *      ELTOUTCC
00282 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELTOUTCC
00283 *                                                          *      ELTOUTCC
00284 ************************************************************      ELTOUTCC
00285  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELTOUTCC
00286      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTOUTCC
00287      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOUTCC
00288                             ADDRESS OF                            ELTOUTCC
00289          ELR-GRP-REC-AREA.                                        ELTOUTCC
00290      IF CIA-RC-PTR-NULL                                           ELTOUTCC
00291          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTOUTCC
00292      EJECT                                                        ELTOUTCC
00293                                                                   ELTOUTCC
00294                                                                   ELTOUTCC
00295 ************************************************************      ELTOUTCC
00296 *                                                          *      ELTOUTCC
00297 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELTOUTCC
00298 *                                                          *      ELTOUTCC
00299 ************************************************************      ELTOUTCC
00300  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELTOUTCC
00301      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTOUTCC
00302      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOUTCC
00303                               ADDRESS OF                          ELTOUTCC
00304          TCAR-COMPRESSION-WORK-AREA.                              ELTOUTCC
00305      IF CIA-RC-PTR-NULL                                           ELTOUTCC
00306          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTOUTCC
00307      EJECT                                                        ELTOUTCC
00308                                                                   ELTOUTCC
00309                                                                   ELTOUTCC
00310 ************************************************************      ELTOUTCC
00311 *                                                          *      ELTOUTCC
00312 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELTOUTCC
00313 *                                                          *      ELTOUTCC
00314 ************************************************************      ELTOUTCC
00315  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELTOUTCC
00316      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTOUTCC
00317      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOUTCC
00318                            ADDRESS OF                             ELTOUTCC
00319          KWA-FILE-KEY-WORK-AREA.                                  ELTOUTCC
00320      IF CIA-RC-PTR-NULL                                           ELTOUTCC
00321          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTOUTCC
00322                                                                   ELTOUTCC
00323                                                                   ELTOUTCC
00324 ************************************************************      ELTOUTCC
00325 *                                                          *      ELTOUTCC
00326 *        ESTABLISH ADDRESS OF CIA                          *      ELTOUTCC
00327 *                                                          *      ELTOUTCC
00328 ************************************************************      ELTOUTCC
00329  ESTABLISH-ADDRESS-OF-CIA.                                        ELTOUTCC
00330      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTOUTCC
00331                            ADDRESS OF                             ELTOUTCC
00332          CIA-ELS-COMMON-INTERFACE-AREA.                           ELTOUTCC
00333      EJECT                                                        ELTOUTCC
00334                                                                   ELTOUTCC
00335                                                                   ELTOUTCC
00336 ************************************************************      ELTOUTCC
00337 *                                                          *      ELTOUTCC
00338 *        PROCESS                                           *      ELTOUTCC
00339 *                                                          *      ELTOUTCC
00340 ************************************************************      ELTOUTCC
00341  PROCESS.                                                         ELTOUTCC
00342      MOVE WS-MASK-LINE TO COF-MASK-LINE.                          ELTOUTCC
00343      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTOUTCC
00344          PERFORM CHECK-INST-BASIC.                                ELTOUTCC
00345      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTOUTCC
00346          PERFORM CHECK-PROF-BASIC.                                ELTOUTCC
00347      SET WS-POINTER1 TO NULLS.                                    ELTOUTCC
00348      SET WS-POINTER2 TO NULLS.                                    ELTOUTCC
00349      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTOUTCC
00350          PERFORM CHECK-INST-SUPP.                                 ELTOUTCC
00351      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTOUTCC
00352          PERFORM CHECK-PROF-SUPP.                                 ELTOUTCC
00353      PERFORM END-OUTPUT-PAGE.                                     ELTOUTCC
00354      EJECT                                                        ELTOUTCC
00355                                                                   ELTOUTCC
00356                                                                   ELTOUTCC
00357 ************************************************************      ELTOUTCC
00358 *                                                          *      ELTOUTCC
00359 *        CHECK INST BASIC                                  *      ELTOUTCC
00360 *                                                          *      ELTOUTCC
00361 ************************************************************      ELTOUTCC
00362  CHECK-INST-BASIC.                                                ELTOUTCC
00363      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTOUTCC
00364      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOUTCC
00365                            ADDRESS OF                             ELTOUTCC
00366          ELR-CONTR-REC-AREA.                                      ELTOUTCC
00367      SET WS-POINTER1 TO     ADDRESS OF ELR-CONTR-REC-AREA.        ELTOUTCC
00368      IF CIA-RC-PTR-NULL                                           ELTOUTCC
00369          PERFORM CONTINUE-RTN                                     ELTOUTCC
00370      ELSE                                                         ELTOUTCC
00371          PERFORM CONSTRUCT-INSTITUTIONAL-BASICX.                  ELTOUTCC
00372      EJECT                                                        ELTOUTCC
00373                                                                   ELTOUTCC
00374                                                                   ELTOUTCC
00375 ************************************************************      ELTOUTCC
00376 *                                                          *      ELTOUTCC
00377 *        CONSTRUCT INSTITUTIONAL BASIC VERBIAGE            *      ELTOUTCC
00378 *                                                          *      ELTOUTCC
00379 ************************************************************      ELTOUTCC
00380  CONSTRUCT-INSTITUTIONAL-BASICX.                                  ELTOUTCC
00381      INITIALIZE WS-OUTPUT-AREA.                                   ELTOUTCC
00382      SET WS-OUTPUT-IDX TO 1.                                      ELTOUTCC
00383      MOVE +1 TO TCAR-FROM-SUB.                                    ELTOUTCC
00384      PERFORM DO-HEADING-ROUTINE.                                  ELTOUTCC
00385      MOVE WS-INST-BASIC-HEADER TO WS-HEADING                      ELTOUTCC
00386          (WS-OUTPUT-IDX).                                         ELTOUTCC
00387      SET PRODUCED-NO-TEXT TO TRUE.                                ELTOUTCC
00388      IF GCG-BC-FORGN-CLM-IND = SPACE OR ZERO                      ELTOUTCC
00389          PERFORM CONTINUE-RTN                                     ELTOUTCC
00390      ELSE                                                         ELTOUTCC
00391          PERFORM SET-UP-FOR-BC-FOREIGN-CLAIM-TR.                  ELTOUTCC
00392      IF GCT-OUT-COUNTRY-CLM-PROC-IND = ZERO OR SPACE              ELTOUTCC
00393          PERFORM CONTINUE-RTN                                     ELTOUTCC
00394      ELSE                                                         ELTOUTCC
00395          PERFORM SET-UP-FOR-OUT-OF-COUNTRY-CLAI.                  ELTOUTCC
00396      IF PRODUCED-NO-TEXT                                          ELTOUTCC
00397          PERFORM PRODUCE-CONTRACT-NOT-COVERED-T.                  ELTOUTCC
00398      PERFORM OUTPUT-TEXT                                          ELTOUTCC
00399          VARYING WS-OUTPUT-IDX FROM 1 BY 1                        ELTOUTCC
00400                     UNTIL WS-OUTPUT-IDX > WS-LINE-CNT + 1.        ELTOUTCC
00401      PERFORM CALL-ELUOUTPT.                                       ELTOUTCC
00402      EJECT                                                        ELTOUTCC
00403                                                                   ELTOUTCC
00404                                                                   ELTOUTCC
00405 ************************************************************      ELTOUTCC
00406 *                                                          *      ELTOUTCC
00407 *        CHECK PROF BASIC                                  *      ELTOUTCC
00408 *                                                          *      ELTOUTCC
00409 ************************************************************      ELTOUTCC
00410  CHECK-PROF-BASIC.                                                ELTOUTCC
00411      PERFORM ESTABLISH-ADDRESS-OF-PROF-BASI.                      ELTOUTCC
00412      IF CIA-RC-PTR-NULL AND (WS-POINTER1 = WS-POINTER2)           ELTOUTCC
00413          PERFORM CONTINUE-RTN                                     ELTOUTCC
00414      ELSE                                                         ELTOUTCC
00415          PERFORM CONSTRUCT-PROFESSIONAL-BASIC-V.                  ELTOUTCC
00416                                                                   ELTOUTCC
00417                                                                   ELTOUTCC
00418 ************************************************************      ELTOUTCC
00419 *                                                          *      ELTOUTCC
00420 *        ESTABLISH ADDRESS OF PROF BASIC CONTRACT          *      ELTOUTCC
00421 *                                                          *      ELTOUTCC
00422 ************************************************************      ELTOUTCC
00423  ESTABLISH-ADDRESS-OF-PROF-BASI.                                  ELTOUTCC
00424      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTOUTCC
00425      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOUTCC
00426                            ADDRESS OF                             ELTOUTCC
00427          ELR-CONTR-REC-AREA.                                      ELTOUTCC
00428      SET WS-POINTER2 TO    ADDRESS OF ELR-CONTR-REC-AREA.         ELTOUTCC
00429      IF CIA-RC-PTR-NULL                                           ELTOUTCC
00430          PERFORM CONTINUE-RTN.                                    ELTOUTCC
00431      EJECT                                                        ELTOUTCC
00432                                                                   ELTOUTCC
00433                                                                   ELTOUTCC
00434 ************************************************************      ELTOUTCC
00435 *                                                          *      ELTOUTCC
00436 *        CONSTRUCT PROFESSIONAL BASIC VERBIAGE             *      ELTOUTCC
00437 *                                                          *      ELTOUTCC
00438 ************************************************************      ELTOUTCC
00439  CONSTRUCT-PROFESSIONAL-BASIC-V.                                  ELTOUTCC
00440      INITIALIZE WS-OUTPUT-AREA.                                   ELTOUTCC
00441      SET WS-OUTPUT-IDX TO 1.                                      ELTOUTCC
00442      MOVE WS-PROF-BASIC-HEADER TO WS-HEADING                      ELTOUTCC
00443          (WS-OUTPUT-IDX).                                         ELTOUTCC
00444      SET PRODUCED-NO-TEXT TO TRUE.                                ELTOUTCC
00445      IF GCG-BS-FORGN-CLM-IND = SPACE OR ZERO                      ELTOUTCC
00446          PERFORM CONTINUE-RTN                                     ELTOUTCC
00447      ELSE                                                         ELTOUTCC
00448          PERFORM SET-UP-FOR-BS-FOREIGN-CLAIM-TR.                  ELTOUTCC
00449      IF GCT-OUT-COUNTRY-CLM-PROC-IND = ZERO OR SPACE              ELTOUTCC
00450          PERFORM CONTINUE-RTN                                     ELTOUTCC
00451      ELSE                                                         ELTOUTCC
00452          PERFORM SET-UP-FOR-OUT-OF-COUNTRY-CLAI.                  ELTOUTCC
00453      IF PRODUCED-NO-TEXT                                          ELTOUTCC
00454          PERFORM PRODUCE-CONTRACT-NOT-COVERED-T.                  ELTOUTCC
00455      PERFORM OUTPUT-TEXT                                          ELTOUTCC
00456          VARYING WS-OUTPUT-IDX FROM 1 BY 1                        ELTOUTCC
00457                     UNTIL WS-OUTPUT-IDX > WS-LINE-CNT + 1.        ELTOUTCC
00458      PERFORM CALL-ELUOUTPT.                                       ELTOUTCC
00459      EJECT                                                        ELTOUTCC
00460                                                                   ELTOUTCC
00461                                                                   ELTOUTCC
00462 ************************************************************      ELTOUTCC
00463 *                                                          *      ELTOUTCC
00464 *        CHECK INST SUPP                                   *      ELTOUTCC
00465 *                                                          *      ELTOUTCC
00466 ************************************************************      ELTOUTCC
00467  CHECK-INST-SUPP.                                                 ELTOUTCC
00468      PERFORM ESTABLISH-ADDRESS-OF-INST-SUPP.                      ELTOUTCC
00469      IF CIA-RC-PTR-NULL AND (WS-POINTER1 = WS-POINTER2)           ELTOUTCC
00470          PERFORM CONTINUE-RTN                                     ELTOUTCC
00471      ELSE                                                         ELTOUTCC
00472          PERFORM CONSTRUCT-INSTITUTIONAL-SUPPLE.                  ELTOUTCC
00473                                                                   ELTOUTCC
00474                                                                   ELTOUTCC
00475 ************************************************************      ELTOUTCC
00476 *                                                          *      ELTOUTCC
00477 *        ESTABLISH ADDRESS OF INST SUPP CONTRACT           *      ELTOUTCC
00478 *                                                          *      ELTOUTCC
00479 ************************************************************      ELTOUTCC
00480  ESTABLISH-ADDRESS-OF-INST-SUPP.                                  ELTOUTCC
00481      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTOUTCC
00482      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOUTCC
00483                            ADDRESS OF                             ELTOUTCC
00484          ELR-CONTR-REC-AREA.                                      ELTOUTCC
00485      IF CIA-RC-PTR-NULL                                           ELTOUTCC
00486          PERFORM CONTINUE-RTN                                     ELTOUTCC
00487      ELSE                                                         ELTOUTCC
00488          PERFORM SAVE-ADDRESS-OF-INST-SUPP.                       ELTOUTCC
00489                                                                   ELTOUTCC
00490                                                                   ELTOUTCC
00491 ************************************************************      ELTOUTCC
00492 *                                                          *      ELTOUTCC
00493 *        SAVE ADDRESS OF INST SUPP                         *      ELTOUTCC
00494 *                                                          *      ELTOUTCC
00495 ************************************************************      ELTOUTCC
00496  SAVE-ADDRESS-OF-INST-SUPP.                                       ELTOUTCC
00497      SET WS-POINTER1 TO    ADDRESS OF                             ELTOUTCC
00498          ELR-CONTR-REC-AREA.                                      ELTOUTCC
00499      EJECT                                                        ELTOUTCC
00500                                                                   ELTOUTCC
00501                                                                   ELTOUTCC
00502 ************************************************************      ELTOUTCC
00503 *                                                          *      ELTOUTCC
00504 *        CONSTRUCT INSTITUTIONAL SUPPLEMENTAL VERBIAGE     *      ELTOUTCC
00505 *                                                          *      ELTOUTCC
00506 ************************************************************      ELTOUTCC
00507  CONSTRUCT-INSTITUTIONAL-SUPPLE.                                  ELTOUTCC
00508      INITIALIZE WS-OUTPUT-AREA.                                   ELTOUTCC
00509      SET WS-OUTPUT-IDX TO 1.                                      ELTOUTCC
00510      MOVE WS-INST-SUPP-HEADER TO WS-HEADING (WS-OUTPUT-IDX).      ELTOUTCC
00511      SET PRODUCED-NO-TEXT TO TRUE.                                ELTOUTCC
00512      IF GCG-MM-FORGN-CLM-IND = SPACE OR ZERO                      ELTOUTCC
00513          PERFORM CONTINUE-RTN                                     ELTOUTCC
00514      ELSE                                                         ELTOUTCC
00515          PERFORM SET-UP-FOR-MM-FOREIGN-CLAIM-TR.                  ELTOUTCC
00516      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTOUTCC
00517      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOUTCC
00518                            ADDRESS OF                             ELTOUTCC
00519          ELR-CONTR-REC-AREA.                                      ELTOUTCC
00520      IF GCT-OUT-COUNTRY-CLM-PROC-IND = ZERO OR SPACE              ELTOUTCC
00521          PERFORM CONTINUE-RTN                                     ELTOUTCC
00522      ELSE                                                         ELTOUTCC
00523          PERFORM SET-UP-FOR-OUT-OF-COUNTRY-CLAI.                  ELTOUTCC
00524      IF PRODUCED-NO-TEXT                                          ELTOUTCC
00525          PERFORM PRODUCE-CONTRACT-NOT-COVERED-T.                  ELTOUTCC
00526      PERFORM OUTPUT-TEXT                                          ELTOUTCC
00527          VARYING WS-OUTPUT-IDX FROM 1 BY 1                        ELTOUTCC
00528                     UNTIL WS-OUTPUT-IDX > WS-LINE-CNT + 1.        ELTOUTCC
00529      PERFORM CALL-ELUOUTPT.                                       ELTOUTCC
00530      EJECT                                                        ELTOUTCC
00531                                                                   ELTOUTCC
00532                                                                   ELTOUTCC
00533 ************************************************************      ELTOUTCC
00534 *                                                          *      ELTOUTCC
00535 *        CHECK PROF SUPP                                   *      ELTOUTCC
00536 *                                                          *      ELTOUTCC
00537 ************************************************************      ELTOUTCC
00538  CHECK-PROF-SUPP.                                                 ELTOUTCC
00539      PERFORM ESTABLISH-ADDRESS-OF-PROF-SUPP.                      ELTOUTCC
00540      IF CIA-RC-PTR-NULL AND (WS-POINTER1 = WS-POINTER2)           ELTOUTCC
00541          PERFORM CONTINUE-RTN                                     ELTOUTCC
00542      ELSE                                                         ELTOUTCC
00543          PERFORM CONSTRUCT-PROFESSIONAL-SUPPLEM.                  ELTOUTCC
00544                                                                   ELTOUTCC
00545                                                                   ELTOUTCC
00546 ************************************************************      ELTOUTCC
00547 *                                                          *      ELTOUTCC
00548 *        ESTABLISH ADDRESS OF PROF SUPP CONTRACT           *      ELTOUTCC
00549 *                                                          *      ELTOUTCC
00550 ************************************************************      ELTOUTCC
00551  ESTABLISH-ADDRESS-OF-PROF-SUPP.                                  ELTOUTCC
00552      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTOUTCC
00553      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOUTCC
00554                            ADDRESS OF                             ELTOUTCC
00555          ELR-CONTR-REC-AREA.                                      ELTOUTCC
00556      IF CIA-RC-PTR-NULL                                           ELTOUTCC
00557          PERFORM CONTINUE-RTN                                     ELTOUTCC
00558      ELSE                                                         ELTOUTCC
00559          PERFORM SAVE-ADDRESS-OF-PROF-SUPP.                       ELTOUTCC
00560                                                                   ELTOUTCC
00561                                                                   ELTOUTCC
00562 ************************************************************      ELTOUTCC
00563 *                                                          *      ELTOUTCC
00564 *        SAVE ADDRESS OF PROF SUPP                         *      ELTOUTCC
00565 *                                                          *      ELTOUTCC
00566 ************************************************************      ELTOUTCC
00567  SAVE-ADDRESS-OF-PROF-SUPP.                                       ELTOUTCC
00568      SET WS-POINTER2 TO    ADDRESS OF                             ELTOUTCC
00569          ELR-CONTR-REC-AREA.                                      ELTOUTCC
00570      EJECT                                                        ELTOUTCC
00571                                                                   ELTOUTCC
00572                                                                   ELTOUTCC
00573 ************************************************************      ELTOUTCC
00574 *                                                          *      ELTOUTCC
00575 *        CONSTRUCT PROFESSIONAL SUPPLEMENTAL VERBIAGE      *      ELTOUTCC
00576 *                                                          *      ELTOUTCC
00577 ************************************************************      ELTOUTCC
00578  CONSTRUCT-PROFESSIONAL-SUPPLEM.                                  ELTOUTCC
00579      INITIALIZE WS-OUTPUT-AREA.                                   ELTOUTCC
00580      SET WS-OUTPUT-IDX TO 1.                                      ELTOUTCC
00581      MOVE WS-PROF-SUPP-HEADER TO WS-HEADING (WS-OUTPUT-IDX).      ELTOUTCC
00582      SET PRODUCED-NO-TEXT TO TRUE.                                ELTOUTCC
00583      IF GCG-MM-FORGN-CLM-IND = SPACE OR ZERO                      ELTOUTCC
00584          PERFORM CONTINUE-RTN                                     ELTOUTCC
00585      ELSE                                                         ELTOUTCC
00586          PERFORM SET-UP-FOR-MM-FOREIGN-CLAIM-TR.                  ELTOUTCC
00587      IF GCT-OUT-COUNTRY-CLM-PROC-IND = ZERO OR SPACE              ELTOUTCC
00588          PERFORM CONTINUE-RTN                                     ELTOUTCC
00589      ELSE                                                         ELTOUTCC
00590          PERFORM SET-UP-FOR-OUT-OF-COUNTRY-CLAI.                  ELTOUTCC
00591      IF PRODUCED-NO-TEXT                                          ELTOUTCC
00592          PERFORM PRODUCE-CONTRACT-NOT-COVERED-T.                  ELTOUTCC
00593      PERFORM OUTPUT-TEXT                                          ELTOUTCC
00594          VARYING WS-OUTPUT-IDX FROM 1 BY 1                        ELTOUTCC
00595                     UNTIL WS-OUTPUT-IDX > WS-LINE-CNT + 1.        ELTOUTCC
00596      PERFORM CALL-ELUOUTPT.                                       ELTOUTCC
00597                                                                   ELTOUTCC
00598                                                                   ELTOUTCC
00599 ************************************************************      ELTOUTCC
00600 *                                                          *      ELTOUTCC
00601 *        CONTINUE RTN                                      *      ELTOUTCC
00602 *                                                          *      ELTOUTCC
00603 ************************************************************      ELTOUTCC
00604  CONTINUE-RTN.                                                    ELTOUTCC
00605      CONTINUE.                                                    ELTOUTCC
00606      EJECT                                                        ELTOUTCC
00607                                                                   ELTOUTCC
00608                                                                   ELTOUTCC
00609 ************************************************************      ELTOUTCC
00610 *                                                          *      ELTOUTCC
00611 *        SET UP FOR BS FOREIGN CLAIM TRANSLATION           *      ELTOUTCC
00612 *                                                          *      ELTOUTCC
00613 ************************************************************      ELTOUTCC
00614  SET-UP-FOR-BS-FOREIGN-CLAIM-TR.                                  ELTOUTCC
00615      MOVE GCG-BS-FORGN-CLM-IND TO CMF-CODE-VALUE.                 ELTOUTCC
00616      MOVE    'BS-FORGN-CLM-IND' TO                                ELTOUTCC
00617          CMF-ELEMENT-SYSTEM-NAME.                                 ELTOUTCC
00618      PERFORM TRANSLATE-GROUP-SPECIFIC-FIELD.                      ELTOUTCC
00619      EJECT                                                        ELTOUTCC
00620                                                                   ELTOUTCC
00621                                                                   ELTOUTCC
00622 ************************************************************      ELTOUTCC
00623 *                                                          *      ELTOUTCC
00624 *        SET UP FOR BC FOREIGN CLAIM TRANSLATION           *      ELTOUTCC
00625 *                                                          *      ELTOUTCC
00626 ************************************************************      ELTOUTCC
00627  SET-UP-FOR-BC-FOREIGN-CLAIM-TR.                                  ELTOUTCC
00628      MOVE GCG-BC-FORGN-CLM-IND TO CMF-CODE-VALUE.                 ELTOUTCC
00629      MOVE    'BC-FORGN-CLM-IND' TO CMF-ELEMENT-SYSTEM-NAME.       ELTOUTCC
00630      PERFORM TRANSLATE-GROUP-SPECIFIC-FIELD.                      ELTOUTCC
00631      EJECT                                                        ELTOUTCC
00632                                                                   ELTOUTCC
00633                                                                   ELTOUTCC
00634 ************************************************************      ELTOUTCC
00635 *                                                          *      ELTOUTCC
00636 *        SET UP FOR MM FOREIGN CLAIM TRANSLATION           *      ELTOUTCC
00637 *                                                          *      ELTOUTCC
00638 ************************************************************      ELTOUTCC
00639  SET-UP-FOR-MM-FOREIGN-CLAIM-TR.                                  ELTOUTCC
00640      MOVE GCG-MM-FORGN-CLM-IND TO CMF-CODE-VALUE.                 ELTOUTCC
00641      MOVE    'MM-FORGN-CLM-IND' TO CMF-ELEMENT-SYSTEM-NAME.       ELTOUTCC
00642      PERFORM TRANSLATE-GROUP-SPECIFIC-FIELD.                      ELTOUTCC
00643      EJECT                                                        ELTOUTCC
00644                                                                   ELTOUTCC
00645                                                                   ELTOUTCC
00646 ************************************************************      ELTOUTCC
00647 *                                                          *      ELTOUTCC
00648 *        SET UP FOR OUT OF COUNTRY CLAIM TRANSLATION       *      ELTOUTCC
00649 *                                                          *      ELTOUTCC
00650 ************************************************************      ELTOUTCC
00651  SET-UP-FOR-OUT-OF-COUNTRY-CLAI.                                  ELTOUTCC
00652      MOVE  GCT-OUT-COUNTRY-CLM-PROC-IND TO CMF-CODE-VALUE.        ELTOUTCC
00653      MOVE     'OUT-COUNTRY-CLM-PROC-IND' TO                       ELTOUTCC
00654          CMF-ELEMENT-SYSTEM-NAME.                                 ELTOUTCC
00655      PERFORM TRANSLATE-CONTRACT-FIELD.                            ELTOUTCC
00656      EJECT                                                        ELTOUTCC
00657                                                                   ELTOUTCC
00658                                                                   ELTOUTCC
00659 ************************************************************      ELTOUTCC
00660 *                                                          *      ELTOUTCC
00661 *        TRANSLATE CONTRACT FIELD                          *      ELTOUTCC
00662 *                                                          *      ELTOUTCC
00663 ************************************************************      ELTOUTCC
00664  TRANSLATE-CONTRACT-FIELD.                                        ELTOUTCC
00665      PERFORM INITIALIZE-TEXT-COMPRESSION.                         ELTOUTCC
00666      SET PRODUCED-TEXT TO TRUE.                                   ELTOUTCC
00667      MOVE 'CONTRACT'  TO CMF-RECORD-PREFIX.                       ELTOUTCC
00668      PERFORM LINK-TO-CODES-MANUAL.                                ELTOUTCC
00669      PERFORM STRING-IN-DESCRIPTION-PHRASE                         ELTOUTCC
00670          VARYING SUB1 FROM 1 BY 1 UNTIL                           ELTOUTCC
00671                 SUB1 GREATER THAN CMF-NBR-DESCR-LINES.            ELTOUTCC
00672      PERFORM ADD-PERIOD-TO-SENTENCE.                              ELTOUTCC
00673      PERFORM DO-TEXT-COMPRESSION.                                 ELTOUTCC
00674      PERFORM SET-UP-OUTPUT-FIELD-LENGTH.                          ELTOUTCC
00675      PERFORM DO-TEXT-UNSTRING.                                    ELTOUTCC
00676      PERFORM MOVE-LINES-OUT                                       ELTOUTCC
00677          VARYING TCAR-FROM-SUB FROM 1 BY 1                        ELTOUTCC
00678                    UNTIL TCAR-FROM-SUB GREATER THAN               ELTOUTCC
00679              TCAR-OUTPUT-FIELDS-USED.                             ELTOUTCC
00680      EJECT                                                        ELTOUTCC
00681                                                                   ELTOUTCC
00682                                                                   ELTOUTCC
00683 ************************************************************      ELTOUTCC
00684 *                                                          *      ELTOUTCC
00685 *        TRANSLATE GROUP SPECIFIC FIELD                    *      ELTOUTCC
00686 *                                                          *      ELTOUTCC
00687 ************************************************************      ELTOUTCC
00688  TRANSLATE-GROUP-SPECIFIC-FIELD.                                  ELTOUTCC
00689      PERFORM INITIALIZE-TEXT-COMPRESSION.                         ELTOUTCC
00690      SET PRODUCED-TEXT TO TRUE.                                   ELTOUTCC
00691      MOVE 'GROUP'  TO CMF-RECORD-PREFIX.                          ELTOUTCC
00692      PERFORM LINK-TO-CODES-MANUAL.                                ELTOUTCC
00693      PERFORM STRING-IN-DESCRIPTION-PHRASE                         ELTOUTCC
00694          VARYING SUB1 FROM 1 BY 1 UNTIL                           ELTOUTCC
00695                 SUB1 GREATER THAN CMF-NBR-DESCR-LINES.            ELTOUTCC
00696      PERFORM ADD-PERIOD-TO-SENTENCE.                              ELTOUTCC
00697      PERFORM DO-TEXT-COMPRESSION.                                 ELTOUTCC
00698      PERFORM SET-UP-OUTPUT-FIELD-LENGTH.                          ELTOUTCC
00699      PERFORM DO-TEXT-UNSTRING.                                    ELTOUTCC
00700      PERFORM MOVE-LINES-OUT                                       ELTOUTCC
00701          VARYING TCAR-FROM-SUB FROM 1 BY 1                        ELTOUTCC
00702                    UNTIL TCAR-FROM-SUB GREATER THAN               ELTOUTCC
00703              TCAR-OUTPUT-FIELDS-USED.                             ELTOUTCC
00704      EJECT                                                        ELTOUTCC
00705                                                                   ELTOUTCC
00706                                                                   ELTOUTCC
00707 ************************************************************      ELTOUTCC
00708 *                                                          *      ELTOUTCC
00709 *        DO HEADING ROUTINE                                *      ELTOUTCC
00710 *                                                          *      ELTOUTCC
00711 ************************************************************      ELTOUTCC
00712  DO-HEADING-ROUTINE.                                              ELTOUTCC
00713      SET COF-NEW-PAGE              TO TRUE.                       ELTOUTCC
00714      MOVE +3                       TO COF-NBR-HDR-LINES.          ELTOUTCC
00715      MOVE WS-HEADER-LINE           TO COF-HDR-LINE (2).           ELTOUTCC
00716      MOVE +1                       TO COF-NBR-DTL-LINES.          ELTOUTCC
00717      MOVE ALL '-' TO COF-HDR-LINE (3).                            ELTOUTCC
00718      MOVE +0                       TO COF-NBR-DTL-LINES.          ELTOUTCC
00719      EJECT                                                        ELTOUTCC
00720                                                                   ELTOUTCC
00721                                                                   ELTOUTCC
00722 ************************************************************      ELTOUTCC
00723 *                                                          *      ELTOUTCC
00724 *        PRODUCE CONTRACT NOT COVERED TEXT                 *      ELTOUTCC
00725 *                                                          *      ELTOUTCC
00726 ************************************************************      ELTOUTCC
00727  PRODUCE-CONTRACT-NOT-COVERED-T.                                  ELTOUTCC
00728      SET WS-OUTPUT-IDX TO 1.                                      ELTOUTCC
00729      MOVE '|' TO WS-DIVIDER (WS-OUTPUT-IDX).                      ELTOUTCC
00730      MOVE 'OUT OF COUNTRY SERVICES ARE NOT COVERED.'              ELTOUTCC
00731          TO WS-TEXT (WS-OUTPUT-IDX).                              ELTOUTCC
00732      EJECT                                                        ELTOUTCC
00733                                                                   ELTOUTCC
00734                                                                   ELTOUTCC
00735 ************************************************************      ELTOUTCC
00736 *                                                          *      ELTOUTCC
00737 *        LINK TO CODES MANUAL                              *      ELTOUTCC
00738 *                                                          *      ELTOUTCC
00739 ************************************************************      ELTOUTCC
00740  LINK-TO-CODES-MANUAL.                                            ELTOUTCC
00741      EXEC CICS LINK                                               ELTOUTCC
00742                PROGRAM ('ELUCMIF')                                ELTOUTCC
00743                COMMAREA (DFHCOMMAREA)                             ELTOUTCC
00744                END-EXEC.                                          ELTOUTCC
00745      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTOUTCC
00746      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOUTCC
00747                            ADDRESS OF CMF-DESCR.                  ELTOUTCC
00748      EJECT                                                        ELTOUTCC
00749                                                                   ELTOUTCC
00750                                                                   ELTOUTCC
00751 ************************************************************      ELTOUTCC
00752 *                                                          *      ELTOUTCC
00753 *        END OUTPUT PAGE                                   *      ELTOUTCC
00754 *                                                          *      ELTOUTCC
00755 ************************************************************      ELTOUTCC
00756  END-OUTPUT-PAGE.                                                 ELTOUTCC
00757      PERFORM DISPLAY-LINE-AT-END-OF-OUTPUT.                       ELTOUTCC
00758      SET COF-END TO TRUE.                                         ELTOUTCC
00759      PERFORM CALL-ELUOUTPT.                                       ELTOUTCC
00760      INITIALIZE COF-NBR-HDR-LINES                                 ELTOUTCC
00761                 COF-NBR-DTL-LINES.                                ELTOUTCC
00762                                                                   ELTOUTCC
00763                                                                   ELTOUTCC
00764 ************************************************************      ELTOUTCC
00765 *                                                          *      ELTOUTCC
00766 *        DISPLAY LINE AT END OF OUTPUT                     *      ELTOUTCC
00767 *                                                          *      ELTOUTCC
00768 ************************************************************      ELTOUTCC
00769  DISPLAY-LINE-AT-END-OF-OUTPUT.                                   ELTOUTCC
00770      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTOUTCC
00771      MOVE ALL '-' TO COF-DTL-LINE (COF-NBR-DTL-LINES).            ELTOUTCC
00772      PERFORM CALL-ELUOUTPT.                                       ELTOUTCC
00773                                                                   ELTOUTCC
00774                                                                   ELTOUTCC
00775 ************************************************************      ELTOUTCC
00776 *                                                          *      ELTOUTCC
00777 *        CALL ELUOUTPT                                     *      ELTOUTCC
00778 *                                                          *      ELTOUTCC
00779 ************************************************************      ELTOUTCC
00780  CALL-ELUOUTPT.                                                   ELTOUTCC
00781      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTOUTCC
00782                       COMMAREA(DFHCOMMAREA)                       ELTOUTCC
00783                       END-EXEC.                                   ELTOUTCC
00784      SET COF-CONTINUE TO TRUE.                                    ELTOUTCC
00785      INITIALIZE COF-NBR-DTL-LINES.                                ELTOUTCC
00786      EJECT                                                        ELTOUTCC
00787                                                                   ELTOUTCC
00788                                                                   ELTOUTCC
00789 ************************************************************      ELTOUTCC
00790 *                                                          *      ELTOUTCC
00791 *        MOVE LINES OUT                                    *      ELTOUTCC
00792 *                                                          *      ELTOUTCC
00793 ************************************************************      ELTOUTCC
00794  MOVE-LINES-OUT.                                                  ELTOUTCC
00795      ADD +1 TO WS-LINE-CNT.                                       ELTOUTCC
00796      MOVE '|' TO WS-DIVIDER (WS-OUTPUT-IDX).                      ELTOUTCC
00797      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO WS-TEXT                ELTOUTCC
00798          (WS-OUTPUT-IDX).                                         ELTOUTCC
00799      SET WS-OUTPUT-IDX UP BY 1.                                   ELTOUTCC
00800      EJECT                                                        ELTOUTCC
00801                                                                   ELTOUTCC
00802                                                                   ELTOUTCC
00803 ************************************************************      ELTOUTCC
00804 *                                                          *      ELTOUTCC
00805 *        STRING IN DESCRIPTION PHRASE                      *      ELTOUTCC
00806 *                                                          *      ELTOUTCC
00807 ************************************************************      ELTOUTCC
00808  STRING-IN-DESCRIPTION-PHRASE.                                    ELTOUTCC
00809      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELTOUTCC
00810             ' ' CMF-DESCR-LINE (SUB1) DELIMITED BY SIZE           ELTOUTCC
00811             INTO TCAR-FROM-AREA.                                  ELTOUTCC
00812      EJECT                                                        ELTOUTCC
00813                                                                   ELTOUTCC
00814                                                                   ELTOUTCC
00815 ************************************************************      ELTOUTCC
00816 *                                                          *      ELTOUTCC
00817 *        SET UP OUTPUT FIELD LENGTH                        *      ELTOUTCC
00818 *                                                          *      ELTOUTCC
00819 ************************************************************      ELTOUTCC
00820  SET-UP-OUTPUT-FIELD-LENGTH.                                      ELTOUTCC
00821      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTOUTCC
00822      MOVE +46 TO TCAR-OUTPUT-FIELD-1-LEN                          ELTOUTCC
00823                  TCAR-OUTPUT-FIELD-2-LEN                          ELTOUTCC
00824                  TCAR-OUTPUT-FIELD-3-LEN                          ELTOUTCC
00825                  TCAR-OUTPUT-FIELD-4-LEN                          ELTOUTCC
00826                  TCAR-OUTPUT-FIELD-5-LEN                          ELTOUTCC
00827                  TCAR-OUTPUT-FIELD-6-LEN                          ELTOUTCC
00828                  TCAR-OUTPUT-FIELD-7-LEN                          ELTOUTCC
00829                  TCAR-OUTPUT-FIELD-8-LEN                          ELTOUTCC
00830                  TCAR-OUTPUT-FIELD-9-LEN                          ELTOUTCC
00831                  TCAR-OUTPUT-FIELD-10-LEN                         ELTOUTCC
00832                  TCAR-OUTPUT-FIELD-11-LEN                         ELTOUTCC
00833                  TCAR-OUTPUT-FIELD-12-LEN                         ELTOUTCC
00834                  TCAR-OUTPUT-FIELD-13-LEN                         ELTOUTCC
00835                  TCAR-OUTPUT-FIELD-14-LEN                         ELTOUTCC
00836                  TCAR-OUTPUT-FIELD-15-LEN                         ELTOUTCC
00837                  TCAR-OUTPUT-FIELD-16-LEN                         ELTOUTCC
00838                  TCAR-OUTPUT-FIELD-17-LEN                         ELTOUTCC
00839                  TCAR-OUTPUT-FIELD-18-LEN                         ELTOUTCC
00840                  TCAR-OUTPUT-FIELD-19-LEN                         ELTOUTCC
00841                  TCAR-OUTPUT-FIELD-20-LEN.                        ELTOUTCC
00842      EJECT                                                        ELTOUTCC
00843                                                                   ELTOUTCC
00844                                                                   ELTOUTCC
00845 ************************************************************      ELTOUTCC
00846 *                                                          *      ELTOUTCC
00847 *        OUTPUT TEXT                                       *      ELTOUTCC
00848 *                                                          *      ELTOUTCC
00849 ************************************************************      ELTOUTCC
00850  OUTPUT-TEXT.                                                     ELTOUTCC
00851      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTOUTCC
00852      MOVE '|' TO WS-DIVIDER (WS-OUTPUT-IDX).                      ELTOUTCC
00853      MOVE WS-OUTPUT-LINE (WS-OUTPUT-IDX) TO COF-DTL-LINE          ELTOUTCC
00854          (COF-NBR-DTL-LINES).                                     ELTOUTCC
00855      EJECT                                                        ELTOUTCC
00856                                                                   ELTOUTCC
00857                                                                   ELTOUTCC
00858 ************************************************************      ELTOUTCC
00859 *                                                          *      ELTOUTCC
00860 *        ADD PERIOD TO SENTENCE                            *      ELTOUTCC
00861 *                                                          *      ELTOUTCC
00862 ************************************************************      ELTOUTCC
00863  ADD-PERIOD-TO-SENTENCE.                                          ELTOUTCC
00864      ADD +1 TO SUB1.                                              ELTOUTCC
00865      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT (SUB1).                ELTOUTCC
00866      EJECT                                                        ELTOUTCC
00867                                                                   ELTOUTCC
00868                                                                   ELTOUTCC
00869 ************************************************************      ELTOUTCC
00870 *                                                          *      ELTOUTCC
00871 *        INITIALIZE TEXT COMPRESSION                       *      ELTOUTCC
00872 *                                                          *      ELTOUTCC
00873 ************************************************************      ELTOUTCC
00874  INITIALIZE-TEXT-COMPRESSION.                                     ELTOUTCC
00875      INITIALIZE TCAR-FROM-SUB                                     ELTOUTCC
00876                 TCAR-FROM-AREA                                    ELTOUTCC
00877                 TCAR-FROM-LENGTH.                                 ELTOUTCC
00878      EJECT                                                        ELTOUTCC
00879                                                                   ELTOUTCC
00880                                                                   ELTOUTCC
00881 ************************************************************      ELTOUTCC
00882 *                                                          *      ELTOUTCC
00883 *        DO TEXT COMPRESSION                               *      ELTOUTCC
00884 *                                                          *      ELTOUTCC
00885 ************************************************************      ELTOUTCC
00886  DO-TEXT-COMPRESSION.                                             ELTOUTCC
00887      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTOUTCC
00888      EJECT                                                        ELTOUTCC
00889                                                                   ELTOUTCC
00890                                                                   ELTOUTCC
00891 ************************************************************      ELTOUTCC
00892 *                                                          *      ELTOUTCC
00893 *        DO TEXT UNSTRING                                  *      ELTOUTCC
00894 *                                                          *      ELTOUTCC
00895 ************************************************************      ELTOUTCC
00896  DO-TEXT-UNSTRING.                                                ELTOUTCC
00897      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTOUTCC
