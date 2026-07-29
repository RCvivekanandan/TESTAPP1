00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELTCOOBN
00003  PROGRAM-ID.         ELTCOOBN.                                       LV002
00004                                                                   ELTCOOBN
00005  AUTHOR.             EDWARD G LISS                                ELTCOOBN
00006                                                                   ELTCOOBN
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTCOOBN
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELTCOOBN
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTCOOBN
00010                      233 N. MICHIGAN AVE                          ELTCOOBN
00011                      CHICAGO, ILLINOIS 60601                      ELTCOOBN
00012                                                                   ELTCOOBN
00013  DATE-WRITTEN.       15-APR-1987.                                 ELTCOOBN
00014                                                                   ELTCOOBN
00015  DATE-COMPILED.                                                   ELTCOOBN
00016                                                                   ELTCOOBN
00017  SECURITY.           COPYRIGHT 1987,                              ELTCOOBN
00018                      HEALTH CARE SERVICE CORPORATION              ELTCOOBN
00019      SKIP3                                                        ELTCOOBN
00020  ENVIRONMENT DIVISION.                                            ELTCOOBN
00021                                                                   ELTCOOBN
00022  CONFIGURATION SECTION.                                           ELTCOOBN
00023  SOURCE-COMPUTER.    IBM-3033.                                    ELTCOOBN
00024  OBJECT-COMPUTER.    IBM-3033.                                    ELTCOOBN
00025      EJECT                                                        ELTCOOBN
00026 ******************************************************************ELTCOOBN
00027 *                                                                *ELTCOOBN
00028 *    PROGRAM:    ELTCOOBN                                        *ELTCOOBN
00029 *    DATE:       15-APR-1987                                     *ELTCOOBN
00030 *    AUTHOR:     EDWARD G LISS                                   *ELTCOOBN
00031 *    FUNCTION:                                                   *ELTCOOBN
00032 *      THIS MODULE WILL CREATE TRANSLATED SCREEN RECORDS FOR     *ELTCOOBN
00033 *      THE COORDINATION OF BENFITS TOPIC.                        *ELTCOOBN
00034 *                                                                *ELTCOOBN
00035 ******************************************************************ELTCOOBN
00036 *                                                                *ELTCOOBN
00037 *                      MAINTENANCE HISTORY                       *ELTCOOBN
00038 *                                                                *ELTCOOBN
00039 *  MOD     DATE     BY  DRPT                ACTION               *ELTCOOBN
00040 * ----- ----------- --- ----- ---------------------------------- *ELTCOOBN
00041 * 01.00 15-APR-1987 EGL       CREATED                            *ELTCOOBN
00042 *                                                                *ELTCOOBN
00043 * 01.01 04-MAY-1988 NAC       REVISED TO 2-COL FORMAT DISPLAY    *ELTCOOBN
00044 *                                                                *ELTCOOBN
00045 * 01.02 20-NOV-1992 GEM       STORAGE MANAGEMENT ENHANCEMENTS    *ELTCOOBN
00046 *                                                                *ELTCOOBN
00047 * 01.03 01-JUL-1993 RGO    1) PREVENT UNNECESSARY EL12 ABENDS    *ELTCOOBN
00048 *                             WHEN ATTEMPTING TO ESTABLISH       *ELTCOOBN
00049 *                             ADDRESSABILITY TO THE CODES MANUAL *ELTCOOBN
00050 *                             DESCRIPTION AREA. DELETED CODE THAT*ELTCOOBN
00051 *                             ADDRESSES ELSCMDSC, BECAUSE THIS IS*ELTCOOBN
00052 *                             DONE IN ELUCMIF.                   *ELTCOOBN
00053 *                                                                *ELTCOOBN
00054 *                          2) INCORRECT SETTING OF WS CONTRACT   *ELTCOOBN
00055 *                             POINTERS (WS-ELSCONIB-PTR, ETC)    *ELTCOOBN
00056 *                             CAUSED INVALID CODE VALUE MESSAGES.*ELTCOOBN
00057 * 01.04 10-NOV-1992 RJL       CORRECTED FOR INST/PROF/BOTH       *ELTCOOBN
00058 *                                                                *ELTCOOBN
00059 * 01.05 19-JAN-1994 JPB    FIXED ABEND CAUSED WHEN PROF-ONLY IS  *ELTCOOBN
00060 *                          SELECTED AND THERE ARE NO PROFESSIONAL*ELTCOOBN
00061 *                          BENEFITS TO DISPLAY.                  *ELTCOOBN
00062 *                                                                *ELTCOOBN
00063 *       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE       *ELTCOOBN
00064 ******************************************************************ELTCOOBN
00065      EJECT                                                        ELTCOOBN
00066  DATA DIVISION.                                                   ELTCOOBN
00067  WORKING-STORAGE SECTION.                                         ELTCOOBN
00068  01  WS-MISC.                                                     ELTCOOBN
00069      05  WS-CONTRACT-SW           PIC X VALUE 'N'.                ELTCOOBN
00070          88  WS-CONTRACT-PROCESSED      VALUE 'Y'.                ELTCOOBN
00071          88  WS-NO-CONTRACT-PROCESSED   VALUE 'N'.                ELTCOOBN
00072 **************************************************************    ELTCOOBN
00073 *** SCREEN BODY LINES                                             ELTCOOBN
00074 **************************************************************    ELTCOOBN
00075  01  HEADER-LINE.                                                 ELTCOOBN
00076      03  FILLER         PIC X(25)      VALUE SPACES.              ELTCOOBN
00077      03  FILLER         PIC X(54)      VALUE                      ELTCOOBN
00078          'COORDINATION  OF  BENEFITS'.                            ELTCOOBN
00079                                                                   ELTCOOBN
00080  01  MASK-LINE.                                                   ELTCOOBN
00081      03  FILLER         PIC X(30)      VALUE SPACES.              ELTCOOBN
00082      03  FILLER         PIC X(49)      VALUE '|'.                 ELTCOOBN
00083                                                                   ELTCOOBN
00084  01  DETAIL-LINE.                                                 ELTCOOBN
00085      03  LEFT-SIDE    PIC X(31).                                  ELTCOOBN
00086      03  FILLER       PIC XX             VALUE SPACES.            ELTCOOBN
00087      03  RIGHT-SIDE   PIC X(46).                                  ELTCOOBN
00088                                                                   ELTCOOBN
00089  01  EDIT-LITERALS.                                               ELTCOOBN
00090      03  WS-MEMBER.                                               ELTCOOBN
00091          05 FILLER            PIC X(22)      VALUE SPACES.        ELTCOOBN
00092          05 FILLER            PIC X(09)      VALUE                ELTCOOBN
00093              'MEMBER  |'.                                         ELTCOOBN
00094      03  WS-SPOUSE.                                               ELTCOOBN
00095          05 FILLER            PIC X(22)      VALUE SPACES.        ELTCOOBN
00096          05 FILLER            PIC X(09)      VALUE                ELTCOOBN
00097              'SPOUSE  |'.                                         ELTCOOBN
00098      03  WS-DEPENDENT.                                            ELTCOOBN
00099          05 FILLER            PIC X(19)      VALUE SPACES.        ELTCOOBN
00100          05 FILLER            PIC X(12)      VALUE                ELTCOOBN
00101              'DEPENDENT  |'.                                      ELTCOOBN
00102      03  WS-INST-BASIC.                                           ELTCOOBN
00103          05 FILLER            PIC X(08)      VALUE SPACES.        ELTCOOBN
00104          05 FILLER            PIC X(23)      VALUE                ELTCOOBN
00105              'INSTITUTIONAL BASIC:  |'.                           ELTCOOBN
00106      03  WS-INST-SUPPL.                                           ELTCOOBN
00107          05 FILLER            PIC X(01)      VALUE SPACES.        ELTCOOBN
00108          05 FILLER            PIC X(30)      VALUE                ELTCOOBN
00109              'INSTITUTIONAL SUPPLEMENTAL:  |'.                    ELTCOOBN
00110      03  WS-PROF-BASIC.                                           ELTCOOBN
00111          05 FILLER            PIC X(09)      VALUE SPACES.        ELTCOOBN
00112          05 FILLER            PIC X(22)      VALUE                ELTCOOBN
00113              'PROFESSIONAL BASIC:  |'.                            ELTCOOBN
00114      03  WS-PROF-SUPPL.                                           ELTCOOBN
00115          05 FILLER            PIC X(02)      VALUE SPACES.        ELTCOOBN
00116          05 FILLER            PIC X(29)      VALUE                ELTCOOBN
00117              'PROFESSIONAL SUPPLEMENTAL:  |'.                     ELTCOOBN
00118      03  WS-NOT-APPL.                                             ELTCOOBN
00119          05 FILLER            PIC X(16)      VALUE                ELTCOOBN
00120              '  NOT APPLICABLE'.                                  ELTCOOBN
00121      03  WS-NO-INST-BENS-MSG.                                     ELTCOOBN
00122          05 FILLER            PIC X(74)      VALUE                ELTCOOBN
00123              '  THERE ARE NO INSTITUTIONAL BENEFITS FOR THESE DATEELTCOOBN
00124 -            'S. COB DOES NOT APPLY.'.                            ELTCOOBN
00125      03  WS-NO-PROF-BENS-MSG.                                     ELTCOOBN
00126          05 FILLER            PIC X(73)      VALUE                ELTCOOBN
00127              '  THERE ARE NO PROFESSIONAL BENEFITS FOR THESE DATESELTCOOBN
00128 -            '. COB DOES NOT APPLY.'.                             ELTCOOBN
00129      03  WS-NO-COVERAGE-MSG.                                      ELTCOOBN
00130          05 FILLER            PIC X(59)      VALUE                ELTCOOBN
00131              '  THERE IS NO COVERAGE FOR THESE DATES. COB DOES NOTELTCOOBN
00132 -            ' APPLY.'.                                           ELTCOOBN
00133 *    ELSCON POINTER AREAS                                         ELTCOOBN
00134  01  WS-ELSCONIB-PTR          USAGE IS POINTER.                   ELTCOOBN
00135  01  WS-ELSCONIS-PTR          USAGE IS POINTER.                   ELTCOOBN
00136  01  WS-ELSCONPB-PTR          USAGE IS POINTER.                   ELTCOOBN
00137  01  WS-ELSCONPS-PTR          USAGE IS POINTER.                   ELTCOOBN
00138 /                                                                 ELTCOOBN
00139  LINKAGE SECTION.                                                 ELTCOOBN
00140  01  DFHCOMMAREA.                                                 ELTCOOBN
00141      COPY ELSCOMMC.                                               ELTCOOBN
00142 /                                                                 ELTCOOBN
00143      COPY ELSCIA2C.                                               ELTCOOBN
00144 /                                                                 ELTCOOBN
00145      COPY ELSSSCBC.                                               ELTCOOBN
00146 /                                                                 ELTCOOBN
00147      COPY ELSCMDSC.                                               ELTCOOBN
00148 /                                                                 ELTCOOBN
00149      COPY ELSCMIFC.                                               ELTCOOBN
00150 /                                                                 ELTCOOBN
00151      COPY ELSKEYSC.                                               ELTCOOBN
00152 /                                                                 ELTCOOBN
00153      COPY ELSOUTPC.                                               ELTCOOBN
00154 /                                                                 ELTCOOBN
00155      COPY ELSTCWAC.                                               ELTCOOBN
00156 /                                                                 ELTCOOBN
00157  01  ELR-CONTR-REC-AREA.                                          ELTCOOBN
00158      COPY GCCONTRC.                                               ELTCOOBN
00159 /                                                                 ELTCOOBN
00160      EJECT                                                        ELTCOOBN
00161  PROCEDURE DIVISION.                                              ELTCOOBN
00162 ************************************************************      ELTCOOBN
00163 *                                                          *      ELTCOOBN
00164 *                    PROCEDURE DIVISION                    *      ELTCOOBN
00165 *                                                          *      ELTCOOBN
00166 ************************************************************      ELTCOOBN
00167                                                                   ELTCOOBN
00168                                                                   ELTCOOBN
00169 ************************************************************      ELTCOOBN
00170 *                                                          *      ELTCOOBN
00171 *       COORDINATION OF BENEFITS                           *      ELTCOOBN
00172 *                                                          *      ELTCOOBN
00173 ************************************************************      ELTCOOBN
00174  00001-COORDINATION-OF-BENEFITS.                                  ELTCOOBN
00175      PERFORM 00005-INITIALIZATION.                                ELTCOOBN
00176      PERFORM 00034-MAIN-PROCESSING.                               ELTCOOBN
00177                                                                   ELTCOOBN
00178 *                                                          *      ELTCOOBN
00179 *        TERMINATION                                       *      ELTCOOBN
00180 *                                                          *      ELTCOOBN
00181          INITIALIZE COF-DTL                                       ELTCOOBN
00182          ADD  +1      TO COF-NBR-DTL-LINES                        ELTCOOBN
00183          MOVE ALL '-' TO COF-DTL-LINE (COF-NBR-DTL-LINES)         ELTCOOBN
00184          PERFORM 00141-LINK-TO-ELUOUTPT                           ELTCOOBN
00185          SET COF-END TO TRUE                                      ELTCOOBN
00186          MOVE ZEROS TO COF-NBR-HDR-LINES                          ELTCOOBN
00187                    COF-NBR-DTL-LINES                              ELTCOOBN
00188          PERFORM 00141-LINK-TO-ELUOUTPT                           ELTCOOBN
00189          EXEC CICS RETURN                                         ELTCOOBN
00190         END-EXEC.                                                 ELTCOOBN
00191                                                                   ELTCOOBN
00192                                                                   ELTCOOBN
00193                                                                   ELTCOOBN
00194 ************************************************************      ELTCOOBN
00195 *                                                          *      ELTCOOBN
00196 *        INITIALIZATION                                    *      ELTCOOBN
00197 *                                                          *      ELTCOOBN
00198 ************************************************************      ELTCOOBN
00199  00005-INITIALIZATION.                                            ELTCOOBN
00200      IF EIBCALEN = LENGTH OF DFHCOMMAREA                          ELTCOOBN
00201          PERFORM 00019-ESTABLISH-ADDRESSABILITY                   ELTCOOBN
00202      ELSE                                                         ELTCOOBN
00203 *                                                          *      ELTCOOBN
00204 *        INVALID COMMAREA                                  *      ELTCOOBN
00205 *                                                          *      ELTCOOBN
00206                                                                   ELTCOOBN
00207          EXEC CICS ABEND ABCODE ('EL01') END-EXEC.                ELTCOOBN
00208                                                                   ELTCOOBN
00209                                                                   ELTCOOBN
00210 *                                                          *      ELTCOOBN
00211 *        SET UP NEW PAGE                                   *      ELTCOOBN
00212 *                                                          *      ELTCOOBN
00213      SET COF-NEW-PAGE           TO TRUE                           ELTCOOBN
00214      MOVE MASK-LINE             TO COF-MASK-LINE                  ELTCOOBN
00215      MOVE +3                    TO COF-NBR-HDR-LINES              ELTCOOBN
00216      MOVE ZERO                  TO COF-NBR-DTL-LINES              ELTCOOBN
00217      MOVE HEADER-LINE           TO COF-HDR-LINE (2)               ELTCOOBN
00218      MOVE ALL '-'               TO COF-HDR-LINE (3)               ELTCOOBN
00219      PERFORM 00141-LINK-TO-ELUOUTPT                               ELTCOOBN
00220      SET COF-CONTINUE           TO TRUE                           ELTCOOBN
00221      MOVE ZERO                  TO COF-NBR-HDR-LINES.             ELTCOOBN
00222                                                                   ELTCOOBN
00223      EJECT                                                        ELTCOOBN
00224                                                                   ELTCOOBN
00225                                                                   ELTCOOBN
00226 ************************************************************      ELTCOOBN
00227 *                                                          *      ELTCOOBN
00228 *        ESTABLISH ADDRESSABILITY                          *      ELTCOOBN
00229 *                                                          *      ELTCOOBN
00230 ************************************************************      ELTCOOBN
00231  00019-ESTABLISH-ADDRESSABILITY.                                  ELTCOOBN
00232      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTCOOBN
00233          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTCOOBN
00234                                                                   ELTCOOBN
00235                                                                   ELTCOOBN
00236      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTCOOBN
00237      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOBN
00238          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTCOOBN
00239      IF NOT CIA-RC-OK                                             ELTCOOBN
00240         PERFORM 00998-SIGNAL-UNALL-AREA-ERROR.                    ELTCOOBN
00241                                                                   ELTCOOBN
00242      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTCOOBN
00243      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOBN
00244          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTCOOBN
00245      IF NOT CIA-RC-OK                                             ELTCOOBN
00246         PERFORM 00998-SIGNAL-UNALL-AREA-ERROR.                    ELTCOOBN
00247                                                                   ELTCOOBN
00248      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTCOOBN
00249      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOBN
00250          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTCOOBN
00251      IF NOT CIA-RC-OK                                             ELTCOOBN
00252         PERFORM 00998-SIGNAL-UNALL-AREA-ERROR.                    ELTCOOBN
00253                                                                   ELTCOOBN
00254      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTCOOBN
00255      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOBN
00256          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTCOOBN
00257      IF NOT CIA-RC-OK                                             ELTCOOBN
00258         PERFORM 00998-SIGNAL-UNALL-AREA-ERROR.                    ELTCOOBN
00259                                                                   ELTCOOBN
00260      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTCOOBN
00261      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOBN
00262          ADDRESS OF TCAR-COMPRESSION-WORK-AREA                    ELTCOOBN
00263      IF NOT CIA-RC-OK                                             ELTCOOBN
00264         PERFORM 00998-SIGNAL-UNALL-AREA-ERROR.                    ELTCOOBN
00265                                                                   ELTCOOBN
00266      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTCOOBN
00267      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOBN
00268                            WS-ELSCONIB-PTR.                       ELTCOOBN
00269                                                                   ELTCOOBN
00270      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTCOOBN
00271      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOBN
00272                            WS-ELSCONIS-PTR.                       ELTCOOBN
00273                                                                   ELTCOOBN
00274      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTCOOBN
00275      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOBN
00276                            WS-ELSCONPB-PTR.                       ELTCOOBN
00277                                                                   ELTCOOBN
00278      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTCOOBN
00279      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOBN
00280                            WS-ELSCONPS-PTR.                       ELTCOOBN
00281                                                                   ELTCOOBN
00282      EJECT                                                        ELTCOOBN
00283                                                                   ELTCOOBN
00284                                                                   ELTCOOBN
00285 ************************************************************      ELTCOOBN
00286 *                                                          *      ELTCOOBN
00287 *        MAIN PROCESSING                                   *      ELTCOOBN
00288 *                                                          *      ELTCOOBN
00289 ************************************************************      ELTCOOBN
00290  00034-MAIN-PROCESSING.                                           ELTCOOBN
00291      IF WS-ELSCONIB-PTR  =  NULL  AND                             ELTCOOBN
00292         WS-ELSCONIS-PTR  =  NULL  AND                             ELTCOOBN
00293         WS-ELSCONPB-PTR  =  NULL  AND                             ELTCOOBN
00294         WS-ELSCONPS-PTR  =  NULL                                  ELTCOOBN
00295                     PERFORM 00035-CHECK-SELECTION                 ELTCOOBN
00296      ELSE                                                         ELTCOOBN
00297         PERFORM 00040-PROCESS-COB.                                ELTCOOBN
00298                                                                   ELTCOOBN
00299 ************************************************************      ELTCOOBN
00300 *                                                          *      ELTCOOBN
00301 *        CHECK SELECTION, SEND L-O-B NOT APPLICABLE MSG    *      ELTCOOBN
00302 *                                                          *      ELTCOOBN
00303 ************************************************************      ELTCOOBN
00304  00035-CHECK-SELECTION.                                           ELTCOOBN
00305      INITIALIZE COF-DTL                                           ELTCOOBN
00306                 COF-NBR-DTL-LINES                                 ELTCOOBN
00307      IF SSB-PROV-CLASS-INST                                       ELTCOOBN
00308         ADD 1 TO COF-NBR-DTL-LINES                                ELTCOOBN
00309         MOVE WS-NO-INST-BENS-MSG                                  ELTCOOBN
00310           TO COF-DTL-LINE (COF-NBR-DTL-LINES).                    ELTCOOBN
00311      IF SSB-PROV-CLASS-PROF                                       ELTCOOBN
00312         ADD 1 TO COF-NBR-DTL-LINES                                ELTCOOBN
00313         MOVE WS-NO-PROF-BENS-MSG                                  ELTCOOBN
00314           TO COF-DTL-LINE (COF-NBR-DTL-LINES).                    ELTCOOBN
00315      IF SSB-PROV-CLASS-BOTH                                       ELTCOOBN
00316         ADD 1 TO COF-NBR-DTL-LINES                                ELTCOOBN
00317         MOVE WS-NO-COVERAGE-MSG                                   ELTCOOBN
00318           TO COF-DTL-LINE (COF-NBR-DTL-LINES).                    ELTCOOBN
00319      PERFORM 00141-LINK-TO-ELUOUTPT.                              ELTCOOBN
00320                                                                   ELTCOOBN
00321                                                                   ELTCOOBN
00322 ************************************************************      ELTCOOBN
00323 *                                                          *      ELTCOOBN
00324 *        PROCESS COB                                       *      ELTCOOBN
00325 *                                                          *      ELTCOOBN
00326 ************************************************************      ELTCOOBN
00327  00040-PROCESS-COB.                                               ELTCOOBN
00328      IF (SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH)              ELTCOOBN
00329                   AND WS-ELSCONIB-PTR NOT = NULL                  ELTCOOBN
00330          PERFORM 00050-TRANSLATE-INST-BASIC-CON.                  ELTCOOBN
00331                                                                   ELTCOOBN
00332      IF (SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH)              ELTCOOBN
00333                   AND WS-ELSCONIS-PTR NOT = NULL                  ELTCOOBN
00334          PERFORM 00062-TRANSLATE-INST-SUPPL-CON.                  ELTCOOBN
00335                                                                   ELTCOOBN
00336      IF (SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH)              ELTCOOBN
00337                   AND WS-ELSCONPB-PTR NOT = NULL                  ELTCOOBN
00338          PERFORM 00074-TRANSLATE-PROF-BASIC-CON.                  ELTCOOBN
00339                                                                   ELTCOOBN
00340      IF (SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH)              ELTCOOBN
00341                   AND WS-ELSCONPS-PTR NOT = NULL                  ELTCOOBN
00342          PERFORM 00086-TRANSLATE-PROF-SUPPL-CON.                  ELTCOOBN
00343      EJECT                                                        ELTCOOBN
00344                                                                   ELTCOOBN
00345 ************************************************************      ELTCOOBN
00346 *                                                          *      ELTCOOBN
00347 *        TRANSLATE INST BASIC CONTRACT                     *      ELTCOOBN
00348 *                                                          *      ELTCOOBN
00349 ************************************************************      ELTCOOBN
00350  00050-TRANSLATE-INST-BASIC-CON.                                  ELTCOOBN
00351      SET ADDRESS OF ELR-CONTR-REC-AREA TO WS-ELSCONIB-PTR.        ELTCOOBN
00352      SET WS-NO-CONTRACT-PROCESSED TO TRUE.                        ELTCOOBN
00353      INITIALIZE COF-DTL                                           ELTCOOBN
00354                 COF-NBR-DTL-LINES                                 ELTCOOBN
00355                 DETAIL-LINE.                                      ELTCOOBN
00356      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTCOOBN
00357      MOVE MASK-LINE TO COF-DTL-LINE (1).                          ELTCOOBN
00358      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTCOOBN
00359      MOVE WS-INST-BASIC TO LEFT-SIDE.                             ELTCOOBN
00360      MOVE DETAIL-LINE   TO COF-DTL-LINE (COF-NBR-DTL-LINES).      ELTCOOBN
00361      PERFORM 00098-TRANSLATE-CONTRACT.                            ELTCOOBN
00362      EJECT                                                        ELTCOOBN
00363                                                                   ELTCOOBN
00364                                                                   ELTCOOBN
00365 ************************************************************      ELTCOOBN
00366 *                                                          *      ELTCOOBN
00367 *        TRANSLATE INST SUPPL CONTRACT                     *      ELTCOOBN
00368 *                                                          *      ELTCOOBN
00369 ************************************************************      ELTCOOBN
00370  00062-TRANSLATE-INST-SUPPL-CON.                                  ELTCOOBN
00371      SET ADDRESS OF ELR-CONTR-REC-AREA TO WS-ELSCONIS-PTR.        ELTCOOBN
00372      SET WS-NO-CONTRACT-PROCESSED TO TRUE.                        ELTCOOBN
00373      INITIALIZE COF-DTL                                           ELTCOOBN
00374                 COF-NBR-DTL-LINES                                 ELTCOOBN
00375                 DETAIL-LINE.                                      ELTCOOBN
00376      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTCOOBN
00377      MOVE MASK-LINE TO COF-DTL-LINE (1).                          ELTCOOBN
00378      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTCOOBN
00379      MOVE WS-INST-SUPPL TO LEFT-SIDE.                             ELTCOOBN
00380      MOVE DETAIL-LINE   TO COF-DTL-LINE (COF-NBR-DTL-LINES).      ELTCOOBN
00381      PERFORM 00098-TRANSLATE-CONTRACT.                            ELTCOOBN
00382      EJECT                                                        ELTCOOBN
00383                                                                   ELTCOOBN
00384                                                                   ELTCOOBN
00385 ************************************************************      ELTCOOBN
00386 *                                                          *      ELTCOOBN
00387 *        TRANSLATE PROF BASIC CONTRACT                     *      ELTCOOBN
00388 *                                                          *      ELTCOOBN
00389 ************************************************************      ELTCOOBN
00390  00074-TRANSLATE-PROF-BASIC-CON.                                  ELTCOOBN
00391      SET ADDRESS OF ELR-CONTR-REC-AREA TO WS-ELSCONPB-PTR.        ELTCOOBN
00392      SET WS-NO-CONTRACT-PROCESSED TO TRUE.                        ELTCOOBN
00393      INITIALIZE COF-DTL                                           ELTCOOBN
00394                 COF-NBR-DTL-LINES                                 ELTCOOBN
00395                 DETAIL-LINE.                                      ELTCOOBN
00396      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTCOOBN
00397      MOVE MASK-LINE TO COF-DTL-LINE (1).                          ELTCOOBN
00398      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTCOOBN
00399      MOVE WS-PROF-BASIC TO LEFT-SIDE.                             ELTCOOBN
00400      MOVE DETAIL-LINE   TO COF-DTL-LINE (COF-NBR-DTL-LINES).      ELTCOOBN
00401      PERFORM 00098-TRANSLATE-CONTRACT.                            ELTCOOBN
00402      EJECT                                                        ELTCOOBN
00403                                                                   ELTCOOBN
00404                                                                   ELTCOOBN
00405 ************************************************************      ELTCOOBN
00406 *                                                          *      ELTCOOBN
00407 *        TRANSLATE PROF SUPPL CONTRACT                     *      ELTCOOBN
00408 *                                                          *      ELTCOOBN
00409 ************************************************************      ELTCOOBN
00410  00086-TRANSLATE-PROF-SUPPL-CON.                                  ELTCOOBN
00411      SET ADDRESS OF ELR-CONTR-REC-AREA TO WS-ELSCONPS-PTR.        ELTCOOBN
00412      SET WS-NO-CONTRACT-PROCESSED TO TRUE.                        ELTCOOBN
00413      INITIALIZE COF-DTL                                           ELTCOOBN
00414                 COF-NBR-DTL-LINES                                 ELTCOOBN
00415                 DETAIL-LINE.                                      ELTCOOBN
00416      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTCOOBN
00417      MOVE MASK-LINE TO COF-DTL-LINE (1).                          ELTCOOBN
00418      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTCOOBN
00419      MOVE WS-PROF-SUPPL TO LEFT-SIDE.                             ELTCOOBN
00420      MOVE DETAIL-LINE   TO COF-DTL-LINE (COF-NBR-DTL-LINES).      ELTCOOBN
00421      PERFORM 00098-TRANSLATE-CONTRACT.                            ELTCOOBN
00422      EJECT                                                        ELTCOOBN
00423                                                                   ELTCOOBN
00424                                                                   ELTCOOBN
00425 ************************************************************      ELTCOOBN
00426 *                                                          *      ELTCOOBN
00427 *        TRANSLATE CONTRACT                                *      ELTCOOBN
00428 *                                                          *      ELTCOOBN
00429 ************************************************************      ELTCOOBN
00430  00098-TRANSLATE-CONTRACT.                                        ELTCOOBN
00431      MOVE 'CONTRACT'  TO CMF-RECORD-PREFIX.                       ELTCOOBN
00432      IF GCT-COB-MEM-CD NOT = ZERO AND                             ELTCOOBN
00433                  GCT-COB-MEM-CD NOT = SPACE                       ELTCOOBN
00434          PERFORM 00108-TRANSLATE-MEMBER-CODE.                     ELTCOOBN
00435      IF GCT-COB-SPS-CD NOT = ZERO AND                             ELTCOOBN
00436                  GCT-COB-SPS-CD NOT = SPACE                       ELTCOOBN
00437          PERFORM 00117-TRANSLATE-SPOUSE-CODE.                     ELTCOOBN
00438      IF GCT-COB-DEP-CD NOT = ZERO AND                             ELTCOOBN
00439                  GCT-COB-DEP-CD NOT = SPACE                       ELTCOOBN
00440          PERFORM 00126-TRANSLATE-DEPENDENT-CODE.                  ELTCOOBN
00441      IF WS-NO-CONTRACT-PROCESSED                                  ELTCOOBN
00442          PERFORM 00105-DISPLAY-NOT-APPLICABLE.                    ELTCOOBN
00443      PERFORM 00141-LINK-TO-ELUOUTPT.                              ELTCOOBN
00444                                                                   ELTCOOBN
00445                                                                   ELTCOOBN
00446 ************************************************************      ELTCOOBN
00447 *                                                          *      ELTCOOBN
00448 *        DISPLAY NOT APPLICABLE                            *      ELTCOOBN
00449 *                                                          *      ELTCOOBN
00450 ************************************************************      ELTCOOBN
00451  00105-DISPLAY-NOT-APPLICABLE.                                    ELTCOOBN
00452      MOVE WS-NOT-APPL   TO RIGHT-SIDE.                            ELTCOOBN
00453      MOVE DETAIL-LINE   TO COF-DTL-LINE (COF-NBR-DTL-LINES).      ELTCOOBN
00454      EJECT                                                        ELTCOOBN
00455                                                                   ELTCOOBN
00456                                                                   ELTCOOBN
00457 ************************************************************      ELTCOOBN
00458 *                                                          *      ELTCOOBN
00459 *        TRANSLATE MEMBER CODE                             *      ELTCOOBN
00460 *                                                          *      ELTCOOBN
00461 ************************************************************      ELTCOOBN
00462  00108-TRANSLATE-MEMBER-CODE.                                     ELTCOOBN
00463      MOVE 'COB-MEM-CD'                  TO                        ELTCOOBN
00464          CMF-ELEMENT-SYSTEM-NAME.                                 ELTCOOBN
00465      MOVE GCT-COB-MEM-CD                TO CMF-CODE-VALUE.        ELTCOOBN
00466      PERFORM 00135-LINK-TO-ELUCMIF.                               ELTCOOBN
00467      MOVE WS-MEMBER          TO LEFT-SIDE.                        ELTCOOBN
00468      MOVE CMF-DESCR-LINE (1) TO RIGHT-SIDE.                       ELTCOOBN
00469      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTCOOBN
00470      MOVE DETAIL-LINE TO COF-DTL-LINE (COF-NBR-DTL-LINES).        ELTCOOBN
00471      SET WS-CONTRACT-PROCESSED TO TRUE.                           ELTCOOBN
00472      EJECT                                                        ELTCOOBN
00473                                                                   ELTCOOBN
00474                                                                   ELTCOOBN
00475 ************************************************************      ELTCOOBN
00476 *                                                          *      ELTCOOBN
00477 *        TRANSLATE SPOUSE CODE                             *      ELTCOOBN
00478 *                                                          *      ELTCOOBN
00479 ************************************************************      ELTCOOBN
00480  00117-TRANSLATE-SPOUSE-CODE.                                     ELTCOOBN
00481      MOVE 'COB-SPS-CD'                  TO                        ELTCOOBN
00482          CMF-ELEMENT-SYSTEM-NAME.                                 ELTCOOBN
00483      MOVE GCT-COB-SPS-CD                TO CMF-CODE-VALUE.        ELTCOOBN
00484      PERFORM 00135-LINK-TO-ELUCMIF.                               ELTCOOBN
00485      MOVE WS-SPOUSE          TO LEFT-SIDE.                        ELTCOOBN
00486      MOVE CMF-DESCR-LINE (1) TO RIGHT-SIDE.                       ELTCOOBN
00487      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTCOOBN
00488      MOVE DETAIL-LINE TO COF-DTL-LINE (COF-NBR-DTL-LINES).        ELTCOOBN
00489      SET WS-CONTRACT-PROCESSED TO TRUE.                           ELTCOOBN
00490      EJECT                                                        ELTCOOBN
00491                                                                   ELTCOOBN
00492                                                                   ELTCOOBN
00493 ************************************************************      ELTCOOBN
00494 *                                                          *      ELTCOOBN
00495 *        TRANSLATE DEPENDENT CODE                          *      ELTCOOBN
00496 *                                                          *      ELTCOOBN
00497 ************************************************************      ELTCOOBN
00498  00126-TRANSLATE-DEPENDENT-CODE.                                  ELTCOOBN
00499      MOVE 'COB-DEP-CD'                  TO                        ELTCOOBN
00500          CMF-ELEMENT-SYSTEM-NAME.                                 ELTCOOBN
00501      MOVE GCT-COB-DEP-CD                TO CMF-CODE-VALUE.        ELTCOOBN
00502      PERFORM 00135-LINK-TO-ELUCMIF.                               ELTCOOBN
00503      MOVE WS-DEPENDENT       TO LEFT-SIDE.                        ELTCOOBN
00504      MOVE CMF-DESCR-LINE (1) TO RIGHT-SIDE.                       ELTCOOBN
00505      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTCOOBN
00506      MOVE DETAIL-LINE TO COF-DTL-LINE (COF-NBR-DTL-LINES).        ELTCOOBN
00507      SET WS-CONTRACT-PROCESSED TO TRUE.                           ELTCOOBN
00508                                                                   ELTCOOBN
00509                                                                   ELTCOOBN
00510 ************************************************************      ELTCOOBN
00511 *                                                          *      ELTCOOBN
00512 *        LINK TO ELUCMIF                                   *      ELTCOOBN
00513 *                                                          *      ELTCOOBN
00514 ************************************************************      ELTCOOBN
00515  00135-LINK-TO-ELUCMIF.                                           ELTCOOBN
00516      EXEC CICS LINK                                               ELTCOOBN
00517              PROGRAM ('ELUCMIF')                                  ELTCOOBN
00518              COMMAREA (DFHCOMMAREA)                               ELTCOOBN
00519         END-EXEC.                                                 ELTCOOBN
00520      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTCOOBN
00521      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCOOBN
00522          ADDRESS OF CMF-DESCR.                                    ELTCOOBN
00523                                                                   ELTCOOBN
00524                                                                   ELTCOOBN
00525                                                                   ELTCOOBN
00526 ************************************************************      ELTCOOBN
00527 *                                                          *      ELTCOOBN
00528 *        LINK TO ELUOUTPT                                  *      ELTCOOBN
00529 *                                                          *      ELTCOOBN
00530 ************************************************************      ELTCOOBN
00531  00141-LINK-TO-ELUOUTPT.                                          ELTCOOBN
00532      EXEC CICS LINK                                               ELTCOOBN
00533                PROGRAM ('ELUOUTPT')                               ELTCOOBN
00534                COMMAREA (DFHCOMMAREA)                             ELTCOOBN
00535         END-EXEC.                                                 ELTCOOBN
00536                                                                   ELTCOOBN
00537 ************************************************************      ELTCOOBN
00538 *                                                          *      ELTCOOBN
00539 *        SIGNAL UNALL AREA ERROR                           *      ELTCOOBN
00540 *                                                          *      ELTCOOBN
00541 ************************************************************      ELTCOOBN
00542  00998-SIGNAL-UNALL-AREA-ERROR.                                   ELTCOOBN
00543      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTCOOBN
00544      EXEC CICS ABEND  ABCODE (CIA-ABCODE) END-EXEC.               ELTCOOBN
00545                                                                   ELTCOOBN
00546 ************************************************************      ELTCOOBN
00547 *                                                          *      ELTCOOBN
00548 *        SIGNAL CIA AB UNDEF                               *      ELTCOOBN
00549 *                                                          *      ELTCOOBN
00550 ************************************************************      ELTCOOBN
00551  00999-SIGNAL-CIA-AB-UNDEF.                                       ELTCOOBN
00552      SET CIA-AB-UNDEF TO TRUE.                                    ELTCOOBN
00553      EXEC CICS ABEND  ABCODE (CIA-ABCODE) END-EXEC.               ELTCOOBN
00554                                                                   ELTCOOBN
00555      EJECT                                                        ELTCOOBN
00556  GOBACK-PARAGRAPH.                                                ELTCOOBN
00557 ************************************************************      ELTCOOBN
00558 *                                                          *      ELTCOOBN
00559 *                         STOP RUN                         *      ELTCOOBN
00560 *                                                          *      ELTCOOBN
00561 ************************************************************      ELTCOOBN
00562      GOBACK.                                                      ELTCOOBN
