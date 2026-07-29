00001 *      LAST MAINTENANCE TIME: 15.41.55  DATE: 01/22/91            06/29/02
00002  IDENTIFICATION DIVISION.                                         ELUCSOUT
00003                                                                      LV001
00004  PROGRAM-ID.         ELUCSOUT.                                    ELUCSOUT
00005                                                                   ELUCSOUT
00006  AUTHOR.             RICK BARILEAU.                               ELUCSOUT
00007                                                                   ELUCSOUT
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELUCSOUT
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELUCSOUT
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELUCSOUT
00011                      233 N. MICHIGAN AVE                          ELUCSOUT
00012                      CHICAGO, ILLINOIS 60601                      ELUCSOUT
00013                                                                   ELUCSOUT
00014  DATE-WRITTEN.       02-FEB-1988.                                 ELUCSOUT
00015                                                                   ELUCSOUT
00016  DATE-COMPILED.                                                   ELUCSOUT
00017                                                                   ELUCSOUT
00018  SECURITY.           COPYRIGHT 1986,                              ELUCSOUT
00019                      HEALTH CARE SERVICE CORPORATION              ELUCSOUT
00020      SKIP3                                                        ELUCSOUT
00021                                                                   ELUCSOUT
00022  ENVIRONMENT DIVISION.                                            ELUCSOUT
00023                                                                   ELUCSOUT
00024  CONFIGURATION SECTION.                                           ELUCSOUT
00025  SOURCE-COMPUTER.    IBM-3090.                                    ELUCSOUT
00026  OBJECT-COMPUTER.    IBM-3090.                                    ELUCSOUT
00027      EJECT                                                        ELUCSOUT
00028 ******************************************************************ELUCSOUT
00029 *                                                                *ELUCSOUT
00030 *  ELUCSOUT - ELS:  THIS MODULE IS RESPONSIBLE FOR GENERATING    *ELUCSOUT
00031 *                   THE APPROPIATE TEXT WITH THE BENEFIT         *ELUCSOUT
00032 *                   PROVISIONS THAT HAVE BEEN FOUND TO BE GROUPED*ELUCSOUT
00033 *                   AND PAID THE SAME. IT WILL ALSO DISPLAY ANY  *ELUCSOUT
00034 *                   DIFFERENCES AMONG THEM.                      *ELUCSOUT
00035 *                                                                *ELUCSOUT
00036 ******************************************************************ELUCSOUT
00037 *                                                                *ELUCSOUT
00038 *                      MAINTENANCE HISTORY                       *ELUCSOUT
00039 *                                                                *ELUCSOUT
00040 *  MOD     DATE     BY  DRPT                ACTION               *ELUCSOUT
00041 * ----- ----------- --- ----- ---------------------------------- *ELUCSOUT
00042 * 01.00 02-FEB-1988 REB       CREATED                            *ELUCSOUT
00043 *                                                                *ELUCSOUT
00044 * 01.01 05-FEB-1988 REB       ERRORS WITH HARD CODED TABLES      *ELUCSOUT
00045 *                                                                *ELUCSOUT
00046 * 01.02 09-FEB-1988 REB       MADE CHANGES TO CORRESPOND TO NEW  *ELUCSOUT
00047 *                             VERSION OF COPYBOOK 'ELSCSPTC'.    *ELUCSOUT
00048 *                                                                *ELUCSOUT
00049 * 01.03 19-FEB-1988 REB       ADDED CHECK FOR > 19 FOR OUTPUT.   *ELUCSOUT
00050 *                                                                *ELUCSOUT
00051 * 01.04 12-MAR-1988 REB       MOVED SOME CODE IN CORRECT PLACE.  *ELUCSOUT
00052 *                                                                *ELUCSOUT
00053 * 01.05 28-MAR-1988 REB       USERS WANT THE CHC VERBIAGE        *ELUCSOUT
00054 *                             SUPPRESSED.                        *ELUCSOUT
00055 *                                                                *ELUCSOUT
00056 * 01.06 31-MAR-1988 REB       CLEAR OUT OUTP BLOCK EACH TIME     *ELUCSOUT
00057 *                             NOTICE PROBLEM WITH USING PF2.     *ELUCSOUT
00058 ******************************************************************ELUCSOUT
00059                                                                   ELUCSOUT
00060                                                                   ELUCSOUT
00061  DATA DIVISION.                                                   ELUCSOUT
00062  WORKING-STORAGE SECTION.                                         ELUCSOUT
00063  01  WS-MISC.                                                     ELUCSOUT
00064      05  FILLER                        PIC  X(24) VALUE           ELUCSOUT
00065          '** ELUCSOUT WS BEGINS **'.                              ELUCSOUT
00066                                                                   ELUCSOUT
00067  01  SWITCHES.                                                    ELUCSOUT
00068      05  WS-BP-HEADINGS-SW             PIC  X(01) VALUE 'C'.      ELUCSOUT
00069          88  HDGS-NEEDED                          VALUE 'N'.      ELUCSOUT
00070          88  HDGS-COMPLETED                       VALUE 'C'.      ELUCSOUT
00071                                                                   ELUCSOUT
00072      05  WS-MATCHING-SW                PIC  X(01) VALUE SPACE.    ELUCSOUT
00073          88  MATCH-FOUND                          VALUE 'Y'.      ELUCSOUT
00074          88  MATCH-NOT-FOUND                      VALUE 'N'.      ELUCSOUT
00075                                                                   ELUCSOUT
00076      05  WS-PROVN-COVERAGE-SW          PIC  X(01) VALUE SPACE.    ELUCSOUT
00077          88  PROVN-NOT-COVERED                    VALUE 'N'.      ELUCSOUT
00078                                                                   ELUCSOUT
00079      05  WS-SUBTOPIC-PTRS-SW           PIC  X(01) VALUE 'N'.      ELUCSOUT
00080          88  SUBTOPIC-PTRS-NOT-FOUND              VALUE 'N'.      ELUCSOUT
00081          88  SUBTOPIC-PTRS-FOUND                  VALUE 'Y'.      ELUCSOUT
00082                                                                   ELUCSOUT
00083  01  PROGRAM-CONSTANTS.                                           ELUCSOUT
00084      05  PC-BAR                        PIC  X(01) VALUE '|'.      ELUCSOUT
00085      05  PC-INST                       PIC  X(15) VALUE           ELUCSOUT
00086          'INSTITUTIONAL :'.                                       ELUCSOUT
00087      05  PC-PROF                       PIC  X(14) VALUE           ELUCSOUT
00088          'PROFESSIONAL :'.                                        ELUCSOUT
00089                                                                   ELUCSOUT
00090  01  WS-LAST-LINE-IND                  PIC S9(04) VALUE +0 COMP-3.ELUCSOUT
00091  01  WS-CSPG-KEY.                                                 ELUCSOUT
00092      05  WS-CSPG-SUBTOPIC              PIC  X(03).                ELUCSOUT
00093      05  WS-CSPG-PROVN-GRP             PIC  9(02).                ELUCSOUT
00094                                                                   ELUCSOUT
00095  01  RGT-TABLE.                                                   ELUCSOUT
00096      03  RGT-NBR-ENTRIES               PIC S9(04) COMP.           ELUCSOUT
00097      03  RGT-TABLE-ENTRY               OCCURS 50 TIMES            ELUCSOUT
00098                                        INDEXED BY RGT-IDX         ELUCSOUT
00099                                                   RGT-CURR-IDX    ELUCSOUT
00100                                                   RGT-PREV-IDX.   ELUCSOUT
00101          05 RGT-KEY.                                              ELUCSOUT
00102             07  RGT-PROVISION-GRP      PIC S9(04) COMP.           ELUCSOUT
00103             07  RGT-BP-ID              PIC  X(05).                ELUCSOUT
00104          05 RGT-DATA.                                             ELUCSOUT
00105             07  RGT-INST-PAYMENT-LVL   PIC S9(04) COMP.           ELUCSOUT
00106             07  RGT-PROF-PAYMENT-LVL   PIC S9(04) COMP.           ELUCSOUT
00107          05 RGT-ADDL-INFO.                                        ELUCSOUT
00108             07  RGT-COORD-PAYMENT-LVL   PIC S9(04) COMP.          ELUCSOUT
00109             07  RGT-COORD-PAYMENT-COVRG PIC  X(01).               ELUCSOUT
00110                 88 RGT-COORD-COVERED              VALUE 'Y'.      ELUCSOUT
00111                 88 RGT-COORD-NOT-COVERED          VALUE 'N'.      ELUCSOUT
00112                                                                   ELUCSOUT
00113  01  WS-OUTPUT-TABLE.                                             ELUCSOUT
00114      03  WS-OUTPUT-AREA                PIC X(1580).               ELUCSOUT
00115      03  WS-OUTPUT-ENTRY REDEFINES WS-OUTPUT-AREA                 ELUCSOUT
00116                                        OCCURS 20 TIMES            ELUCSOUT
00117                                        INDEXED BY WS-HDG-IDX      ELUCSOUT
00118                                                   WS-TXT-IDX.     ELUCSOUT
00119          05  WS-OUTPUT-LINE.                                      ELUCSOUT
00120              07  WS-HEADING            PIC X(30).                 ELUCSOUT
00121              07  WS-DIVIDER            PIC X(01).                 ELUCSOUT
00122              07  FILLER                PIC X(02).                 ELUCSOUT
00123              07  WS-TEXT               PIC X(46).                 ELUCSOUT
00124                                                                   ELUCSOUT
00125                                                                   ELUCSOUT
00126 /    COPYBOOK FOR CONTRACT SUMMARY PROVISION GROUP HEADINGS       ELUCSOUT
00127      COPY ELSCSGHC.                                               ELUCSOUT
00128 /    COPYBOOK FOR CONTRACT SUMMARY BENEFIT PROVISION HEADINGS     ELUCSOUT
00129      COPY ELSCSPHC.                                               ELUCSOUT
00130                                                                   ELUCSOUT
00131  LINKAGE SECTION.                                                 ELUCSOUT
00132  01  DFHCOMMAREA.                                                 ELUCSOUT
00133      COPY ELSCOMMC.                                               ELUCSOUT
00134 /                                                                 ELUCSOUT
00135      COPY ELSCIA2C.                                               ELUCSOUT
00136 /                                                                 ELUCSOUT
00137      COPY ELSIOPMC.                                               ELUCSOUT
00138 /                                                                 ELUCSOUT
00139      COPY ELSSSCBC.                                               ELUCSOUT
00140 /                                                                 ELUCSOUT
00141      COPY ELSOUTPC.                                               ELUCSOUT
00142 /    COPYBOOK IS FOR CONTRACT SUMMARY BENEFIT PROVISION MATRIX    ELUCSOUT
00143      COPY ELSCSBPC.                                               ELUCSOUT
00144 /    COPYBOOK IS FOR CONTRACT SUMMARY SENTENCES STRUCTURES        ELUCSOUT
00145      COPY ELSCSENC.                                               ELUCSOUT
00146 /    COPYBOOK IS FOR CONTRACT SUMMARY PROVISION GROUPING TABLE    ELUCSOUT
00147      COPY ELSCSPGC.                                               ELUCSOUT
00148 /    COPYBOOK IS FOR CONTRACT SUMMARY POINTERS                    ELUCSOUT
00149      COPY ELSCSPTC.                                               ELUCSOUT
00150                                                                   ELUCSOUT
00151      EJECT                                                        ELUCSOUT
00152  PROCEDURE DIVISION.                                              ELUCSOUT
00153 ************************************************************      ELUCSOUT
00154 *                                                          *      ELUCSOUT
00155 *                    PROCEDURE DIVISION                    *      ELUCSOUT
00156 *                                                          *      ELUCSOUT
00157 ************************************************************      ELUCSOUT
00158                                                                   ELUCSOUT
00159                                                                   ELUCSOUT
00160 ************************************************************      ELUCSOUT
00161 *                                                          *      ELUCSOUT
00162 *        ASSEMBLE AND OUTPUT BENEFIT PROVISION SENTENCES   *      ELUCSOUT
00163 *                                                          *      ELUCSOUT
00164 ************************************************************      ELUCSOUT
00165  ASSEMBLE-AND-OUTPUT-BENEFIT-PR.                                  ELUCSOUT
00166      PERFORM INITIALIZATION.                                      ELUCSOUT
00167      PERFORM PROCESS.                                             ELUCSOUT
00168      GOBACK.                                                      ELUCSOUT
00169                                                                   ELUCSOUT
00170                                                                   ELUCSOUT
00171 ************************************************************      ELUCSOUT
00172 *                                                          *      ELUCSOUT
00173 *        INITIALIZATION                                    *      ELUCSOUT
00174 *                                                          *      ELUCSOUT
00175 ************************************************************      ELUCSOUT
00176  INITIALIZATION.                                                  ELUCSOUT
00177      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELUCSOUT
00178      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELUCSOUT
00179                                                                   ELUCSOUT
00180                                                                   ELUCSOUT
00181 ************************************************************      ELUCSOUT
00182 *                                                          *      ELUCSOUT
00183 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELUCSOUT
00184 *                                                          *      ELUCSOUT
00185 ************************************************************      ELUCSOUT
00186  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELUCSOUT
00187      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELUCSOUT
00188      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELUCSOUT
00189      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELUCSOUT
00190                                                                   ELUCSOUT
00191                                                                   ELUCSOUT
00192 ************************************************************      ELUCSOUT
00193 *                                                          *      ELUCSOUT
00194 *        CHECK FOR VALID COMMAREA                          *      ELUCSOUT
00195 *                                                          *      ELUCSOUT
00196 ************************************************************      ELUCSOUT
00197  CHECK-FOR-VALID-COMMAREA.                                        ELUCSOUT
00198      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELUCSOUT
00199          PERFORM SIGNAL-INVALID-COMMAREA.                         ELUCSOUT
00200                                                                   ELUCSOUT
00201                                                                   ELUCSOUT
00202 ************************************************************      ELUCSOUT
00203 *                                                          *      ELUCSOUT
00204 *        SIGNAL INVALID COMMAREA                           *      ELUCSOUT
00205 *                                                          *      ELUCSOUT
00206 ************************************************************      ELUCSOUT
00207  SIGNAL-INVALID-COMMAREA.                                         ELUCSOUT
00208      EXEC CICS ABEND                                              ELUCSOUT
00209                ABCODE('EL01')                                     ELUCSOUT
00210         END-EXEC.                                                 ELUCSOUT
00211      EJECT                                                        ELUCSOUT
00212                                                                   ELUCSOUT
00213                                                                   ELUCSOUT
00214 ************************************************************      ELUCSOUT
00215 *                                                          *      ELUCSOUT
00216 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELUCSOUT
00217 *                                                          *      ELUCSOUT
00218 ************************************************************      ELUCSOUT
00219  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELUCSOUT
00220      IF ECA-CIA-PTR = NULL                                        ELUCSOUT
00221          PERFORM SIGNAL-INVALID-CIA                               ELUCSOUT
00222      ELSE                                                         ELUCSOUT
00223          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELUCSOUT
00224                                                                   ELUCSOUT
00225                                                                   ELUCSOUT
00226 ************************************************************      ELUCSOUT
00227 *                                                          *      ELUCSOUT
00228 *        SIGNAL INVALID CIA                                *      ELUCSOUT
00229 *                                                          *      ELUCSOUT
00230 ************************************************************      ELUCSOUT
00231  SIGNAL-INVALID-CIA.                                              ELUCSOUT
00232      EXEC CICS ABEND                                              ELUCSOUT
00233                ABCODE('EL02')                                     ELUCSOUT
00234         END-EXEC.                                                 ELUCSOUT
00235      EJECT                                                        ELUCSOUT
00236                                                                   ELUCSOUT
00237                                                                   ELUCSOUT
00238 ************************************************************      ELUCSOUT
00239 *                                                          *      ELUCSOUT
00240 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELUCSOUT
00241 *                                                          *      ELUCSOUT
00242 ************************************************************      ELUCSOUT
00243  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELUCSOUT
00244      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELUCSOUT
00245      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSOUT
00246               ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.             ELUCSOUT
00247      IF CIA-RC-PTR-NULL                                           ELUCSOUT
00248          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELUCSOUT
00249                                                                   ELUCSOUT
00250                                                                   ELUCSOUT
00251 ************************************************************      ELUCSOUT
00252 *                                                          *      ELUCSOUT
00253 *        SIGNAL UNALLOC AREA ERROR                         *      ELUCSOUT
00254 *                                                          *      ELUCSOUT
00255 ************************************************************      ELUCSOUT
00256  SIGNAL-UNALLOC-AREA-ERROR.                                       ELUCSOUT
00257      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELUCSOUT
00258      PERFORM SIGNAL-ABEND.                                        ELUCSOUT
00259                                                                   ELUCSOUT
00260                                                                   ELUCSOUT
00261 ************************************************************      ELUCSOUT
00262 *                                                          *      ELUCSOUT
00263 *        SIGNAL ABEND                                      *      ELUCSOUT
00264 *                                                          *      ELUCSOUT
00265 ************************************************************      ELUCSOUT
00266  SIGNAL-ABEND.                                                    ELUCSOUT
00267      EXEC CICS ABEND                                              ELUCSOUT
00268                ABCODE(CIA-ABCODE)                                 ELUCSOUT
00269         END-EXEC.                                                 ELUCSOUT
00270      EJECT                                                        ELUCSOUT
00271                                                                   ELUCSOUT
00272                                                                   ELUCSOUT
00273 ************************************************************      ELUCSOUT
00274 *                                                          *      ELUCSOUT
00275 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELUCSOUT
00276 *                                                          *      ELUCSOUT
00277 ************************************************************      ELUCSOUT
00278  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELUCSOUT
00279      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELUCSOUT
00280      PERFORM ESTABLISH-ADDRESS-OF-CS-POINTE.                      ELUCSOUT
00281      PERFORM FIND-SUBTOPIC-POINTERS-NEEDED                        ELUCSOUT
00282          VARYING CSPT-IDX FROM +1 BY +1                           ELUCSOUT
00283                  UNTIL   CSPT-IDX > +5                            ELUCSOUT
00284                  OR      SUBTOPIC-PTRS-FOUND.                     ELUCSOUT
00285      IF SUBTOPIC-PTRS-NOT-FOUND                                   ELUCSOUT
00286          PERFORM SIGNAL-PARAMETER-MISSING.                        ELUCSOUT
00287                                                                   ELUCSOUT
00288                                                                   ELUCSOUT
00289 ************************************************************      ELUCSOUT
00290 *                                                          *      ELUCSOUT
00291 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELUCSOUT
00292 *                                                          *      ELUCSOUT
00293 ************************************************************      ELUCSOUT
00294  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELUCSOUT
00295      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELUCSOUT
00296      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSOUT
00297               ADDRESS OF COF-OUTPUT-INTERFACE.                    ELUCSOUT
00298      IF CIA-RC-PTR-NULL                                           ELUCSOUT
00299          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELUCSOUT
00300      EJECT                                                        ELUCSOUT
00301                                                                   ELUCSOUT
00302                                                                   ELUCSOUT
00303 ************************************************************      ELUCSOUT
00304 *                                                          *      ELUCSOUT
00305 *        ESTABLISH ADDRESS OF CS POINTER TABLE             *      ELUCSOUT
00306 *                                                          *      ELUCSOUT
00307 ************************************************************      ELUCSOUT
00308  ESTABLISH-ADDRESS-OF-CS-POINTE.                                  ELUCSOUT
00309      SET CIA-ELSCSPTC-DDN TO TRUE.                                ELUCSOUT
00310      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUCSOUT
00311               ADDRESS OF CSPT-POINTER-LIST.                       ELUCSOUT
00312      IF CIA-RC-PTR-NULL                                           ELUCSOUT
00313          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELUCSOUT
00314                                                                   ELUCSOUT
00315                                                                   ELUCSOUT
00316 ************************************************************      ELUCSOUT
00317 *                                                          *      ELUCSOUT
00318 *        FIND SUBTOPIC POINTERS NEEDED                     *      ELUCSOUT
00319 *                                                          *      ELUCSOUT
00320 ************************************************************      ELUCSOUT
00321  FIND-SUBTOPIC-POINTERS-NEEDED.                                   ELUCSOUT
00322      IF PROCESS-SUBTOPIC (CSPT-IDX)                               ELUCSOUT
00323          PERFORM ESTABLISH-THE-NECESSARY-POINTE.                  ELUCSOUT
00324                                                                   ELUCSOUT
00325                                                                   ELUCSOUT
00326 ************************************************************      ELUCSOUT
00327 *                                                          *      ELUCSOUT
00328 *        SIGNAL PARAMETER MISSING                          *      ELUCSOUT
00329 *                                                          *      ELUCSOUT
00330 ************************************************************      ELUCSOUT
00331  SIGNAL-PARAMETER-MISSING.                                        ELUCSOUT
00332      SET CIA-AB-PARM-MISSING TO TRUE.                             ELUCSOUT
00333      PERFORM SIGNAL-ABEND.                                        ELUCSOUT
00334      EJECT                                                        ELUCSOUT
00335                                                                   ELUCSOUT
00336                                                                   ELUCSOUT
00337 ************************************************************      ELUCSOUT
00338 *                                                          *      ELUCSOUT
00339 *        ESTABLISH THE NECESSARY POINTERS FOR SUBTOPIC     *      ELUCSOUT
00340 *                                                          *      ELUCSOUT
00341 ************************************************************      ELUCSOUT
00342  ESTABLISH-THE-NECESSARY-POINTE.                                  ELUCSOUT
00343      PERFORM ESTABLISH-ADDRESS-OF-CS-BENEFI.                      ELUCSOUT
00344      PERFORM ESTABLISH-ADDRESSABILITY-OF-CS.                      ELUCSOUT
00345      SET SUBTOPIC-PTRS-FOUND TO TRUE.                             ELUCSOUT
00346                                                                   ELUCSOUT
00347                                                                   ELUCSOUT
00348 ************************************************************      ELUCSOUT
00349 *                                                          *      ELUCSOUT
00350 *        ESTABLISH ADDRESS OF CS BENEFIT PROVISION TABLE   *      ELUCSOUT
00351 *                                                          *      ELUCSOUT
00352 ************************************************************      ELUCSOUT
00353  ESTABLISH-ADDRESS-OF-CS-BENEFI.                                  ELUCSOUT
00354      IF CSPT-BP-TBL-PTR (CSPT-IDX) = NULL                         ELUCSOUT
00355          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELUCSOUT
00356      ELSE                                                         ELUCSOUT
00357          PERFORM ESTABLISH-ADDRESS-OF-CSBP.                       ELUCSOUT
00358                                                                   ELUCSOUT
00359                                                                   ELUCSOUT
00360 ************************************************************      ELUCSOUT
00361 *                                                          *      ELUCSOUT
00362 *        ESTABLISH ADDRESSABILITY OF CS PROVISION GROUP TAB*      ELUCSOUT
00363 *                                                          *      ELUCSOUT
00364 ************************************************************      ELUCSOUT
00365  ESTABLISH-ADDRESSABILITY-OF-CS.                                  ELUCSOUT
00366      IF CSPT-PROV-GRP-PTR (CSPT-IDX) = NULL                       ELUCSOUT
00367          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELUCSOUT
00368      ELSE                                                         ELUCSOUT
00369          PERFORM ESTABLISH-ADDRESS-OF-CSPG.                       ELUCSOUT
00370                                                                   ELUCSOUT
00371                                                                   ELUCSOUT
00372 ************************************************************      ELUCSOUT
00373 *                                                          *      ELUCSOUT
00374 *        ESTABLISH ADDRESS OF CIA                          *      ELUCSOUT
00375 *                                                          *      ELUCSOUT
00376 ************************************************************      ELUCSOUT
00377  ESTABLISH-ADDRESS-OF-CIA.                                        ELUCSOUT
00378      CALL 'ELUINISM' USING DFHCOMMAREA                            ELUCSOUT
00379               ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.           ELUCSOUT
00380                                                                   ELUCSOUT
00381                                                                   ELUCSOUT
00382 ************************************************************      ELUCSOUT
00383 *                                                          *      ELUCSOUT
00384 *        ESTABLISH ADDRESS OF CSBP                         *      ELUCSOUT
00385 *                                                          *      ELUCSOUT
00386 ************************************************************      ELUCSOUT
00387  ESTABLISH-ADDRESS-OF-CSBP.                                       ELUCSOUT
00388      SET ADDRESS OF CSBP-BENEFIT-PROVISION-TABLE                  ELUCSOUT
00389                  TO CSPT-BP-TBL-PTR (CSPT-IDX).                   ELUCSOUT
00390                                                                   ELUCSOUT
00391                                                                   ELUCSOUT
00392 ************************************************************      ELUCSOUT
00393 *                                                          *      ELUCSOUT
00394 *        ESTABLISH ADDRESS OF CSPG                         *      ELUCSOUT
00395 *                                                          *      ELUCSOUT
00396 ************************************************************      ELUCSOUT
00397  ESTABLISH-ADDRESS-OF-CSPG.                                       ELUCSOUT
00398      SET ADDRESS OF CSPG-PROVISION-GROUP-TABLE                    ELUCSOUT
00399                  TO CSPT-PROV-GRP-PTR (CSPT-IDX).                 ELUCSOUT
00400      EJECT                                                        ELUCSOUT
00401                                                                   ELUCSOUT
00402                                                                   ELUCSOUT
00403 ************************************************************      ELUCSOUT
00404 *                                                          *      ELUCSOUT
00405 *        PROCESS                                           *      ELUCSOUT
00406 *                                                          *      ELUCSOUT
00407 ************************************************************      ELUCSOUT
00408  PROCESS.                                                         ELUCSOUT
00409      PERFORM REGROUP-PAYMENT-LEVELS-TO-COOR.                      ELUCSOUT
00410      PERFORM ASSEMBLE-AND-OUTPUT-HDRS-AND-S.                      ELUCSOUT
00411                                                                   ELUCSOUT
00412                                                                   ELUCSOUT
00413 ************************************************************      ELUCSOUT
00414 *                                                          *      ELUCSOUT
00415 *        REGROUP PAYMENT LEVELS TO COORDINATE INSTITUTIONAL*      ELUCSOUT
00416 *                                                          *      ELUCSOUT
00417 ************************************************************      ELUCSOUT
00418  REGROUP-PAYMENT-LEVELS-TO-COOR.                                  ELUCSOUT
00419      INITIALIZE RGT-NBR-ENTRIES.                                  ELUCSOUT
00420      PERFORM INITIALIZE-REGROUPING-TABLE                          ELUCSOUT
00421          VARYING RGT-IDX FROM +1 BY +1                            ELUCSOUT
00422                  UNTIL   RGT-IDX > +50.                           ELUCSOUT
00423      PERFORM BUILD-REGROUPING-TABLE                               ELUCSOUT
00424          VARYING CSBP-X-IDX FROM +1 BY +1                         ELUCSOUT
00425                  UNTIL   CSBP-X-IDX > CSBP-TBL-CNT.               ELUCSOUT
00426      PERFORM REGROUP-PAYMENT-LEVELS.                              ELUCSOUT
00427                                                                   ELUCSOUT
00428                                                                   ELUCSOUT
00429 ************************************************************      ELUCSOUT
00430 *                                                          *      ELUCSOUT
00431 *        INITIALIZE REGROUPING TABLE                       *      ELUCSOUT
00432 *                                                          *      ELUCSOUT
00433 ************************************************************      ELUCSOUT
00434  INITIALIZE-REGROUPING-TABLE.                                     ELUCSOUT
00435      INITIALIZE RGT-KEY (RGT-IDX)                                 ELUCSOUT
00436                 RGT-DATA (RGT-IDX)                                ELUCSOUT
00437                 RGT-COORD-PAYMENT-LVL (RGT-IDX).                  ELUCSOUT
00438      SET RGT-COORD-NOT-COVERED (RGT-IDX) TO TRUE.                 ELUCSOUT
00439      EJECT                                                        ELUCSOUT
00440                                                                   ELUCSOUT
00441                                                                   ELUCSOUT
00442 ************************************************************      ELUCSOUT
00443 *                                                          *      ELUCSOUT
00444 *        BUILD REGROUPING TABLE                            *      ELUCSOUT
00445 *                                                          *      ELUCSOUT
00446 ************************************************************      ELUCSOUT
00447  BUILD-REGROUPING-TABLE.                                          ELUCSOUT
00448      SET MATCH-NOT-FOUND TO TRUE.                                 ELUCSOUT
00449      PERFORM SCAN-REGROUPING-TABLE-FOR-EXIS                       ELUCSOUT
00450          VARYING RGT-IDX FROM +1 BY +1                            ELUCSOUT
00451                  UNTIL   RGT-IDX >  RGT-NBR-ENTRIES   OR          ELUCSOUT
00452                  MATCH-FOUND.                                     ELUCSOUT
00453      IF MATCH-NOT-FOUND                                           ELUCSOUT
00454          PERFORM ADD-PAYMENT-LEVEL-FOR-NEW-ENTR.                  ELUCSOUT
00455                                                                   ELUCSOUT
00456                                                                   ELUCSOUT
00457 ************************************************************      ELUCSOUT
00458 *                                                          *      ELUCSOUT
00459 *        SCAN REGROUPING TABLE FOR EXISTING ENTRY          *      ELUCSOUT
00460 *                                                          *      ELUCSOUT
00461 ************************************************************      ELUCSOUT
00462  SCAN-REGROUPING-TABLE-FOR-EXIS.                                  ELUCSOUT
00463      IF RGT-PROVISION-GRP (RGT-IDX) = CSBP-PROVISION-GRP          ELUCSOUT
00464          (CSBP-X-IDX)                                             ELUCSOUT
00465                AND (RGT-BP-ID (RGT-IDX) = CSBP-BP-ID              ELUCSOUT
00466          (CSBP-X-IDX))                                            ELUCSOUT
00467          PERFORM ADD-PAYMENT-LEVEL-FOR-EXISTING.                  ELUCSOUT
00468                                                                   ELUCSOUT
00469                                                                   ELUCSOUT
00470 ************************************************************      ELUCSOUT
00471 *                                                          *      ELUCSOUT
00472 *        ADD PAYMENT LEVEL FOR EXISTING ENTRY              *      ELUCSOUT
00473 *                                                          *      ELUCSOUT
00474 ************************************************************      ELUCSOUT
00475  ADD-PAYMENT-LEVEL-FOR-EXISTING.                                  ELUCSOUT
00476      IF CSBP-INSTITUTIONAL (CSBP-X-IDX)                           ELUCSOUT
00477          PERFORM ADD-INSTITUTIONAL-PAYMENT-LEVE                   ELUCSOUT
00478      ELSE                                                         ELUCSOUT
00479          PERFORM ADD-PROFESSIONAL-PAYMENT-LEVEL.                  ELUCSOUT
00480      IF CSBP-COVERED (CSBP-X-IDX) OR                              ELUCSOUT
00481                 CSBP-COVERED-ON-SUPP (CSBP-X-IDX)                 ELUCSOUT
00482          PERFORM INDICATE-COORDINATED-PAYMENT-L.                  ELUCSOUT
00483      SET MATCH-FOUND TO TRUE.                                     ELUCSOUT
00484                                                                   ELUCSOUT
00485                                                                   ELUCSOUT
00486 ************************************************************      ELUCSOUT
00487 *                                                          *      ELUCSOUT
00488 *        ADD INSTITUTIONAL PAYMENT LEVEL                   *      ELUCSOUT
00489 *                                                          *      ELUCSOUT
00490 ************************************************************      ELUCSOUT
00491  ADD-INSTITUTIONAL-PAYMENT-LEVE.                                  ELUCSOUT
00492      MOVE CSBP-PAYMENT-LVL (CSBP-X-IDX) TO                        ELUCSOUT
00493           RGT-INST-PAYMENT-LVL (RGT-IDX).                         ELUCSOUT
00494      EJECT                                                        ELUCSOUT
00495                                                                   ELUCSOUT
00496                                                                   ELUCSOUT
00497 ************************************************************      ELUCSOUT
00498 *                                                          *      ELUCSOUT
00499 *        ADD PROFESSIONAL PAYMENT LEVEL                    *      ELUCSOUT
00500 *                                                          *      ELUCSOUT
00501 ************************************************************      ELUCSOUT
00502  ADD-PROFESSIONAL-PAYMENT-LEVEL.                                  ELUCSOUT
00503      MOVE CSBP-PAYMENT-LVL (CSBP-X-IDX) TO                        ELUCSOUT
00504           RGT-PROF-PAYMENT-LVL (RGT-IDX).                         ELUCSOUT
00505                                                                   ELUCSOUT
00506                                                                   ELUCSOUT
00507 ************************************************************      ELUCSOUT
00508 *                                                          *      ELUCSOUT
00509 *        INDICATE COORDINATED PAYMENT LEVEL HAS COVERAGE   *      ELUCSOUT
00510 *                                                          *      ELUCSOUT
00511 ************************************************************      ELUCSOUT
00512  INDICATE-COORDINATED-PAYMENT-L.                                  ELUCSOUT
00513      SET RGT-COORD-COVERED (RGT-IDX) TO TRUE.                     ELUCSOUT
00514      EJECT                                                        ELUCSOUT
00515                                                                   ELUCSOUT
00516                                                                   ELUCSOUT
00517 ************************************************************      ELUCSOUT
00518 *                                                          *      ELUCSOUT
00519 *        ADD PAYMENT LEVEL FOR NEW ENTRY                   *      ELUCSOUT
00520 *                                                          *      ELUCSOUT
00521 ************************************************************      ELUCSOUT
00522  ADD-PAYMENT-LEVEL-FOR-NEW-ENTR.                                  ELUCSOUT
00523      ADD 1                                TO                      ELUCSOUT
00524          RGT-NBR-ENTRIES.                                         ELUCSOUT
00525      SET RGT-IDX                          TO RGT-NBR-ENTRIES.     ELUCSOUT
00526      MOVE CSBP-PROVISION-GRP (CSBP-X-IDX) TO                      ELUCSOUT
00527           RGT-PROVISION-GRP (RGT-IDX).                            ELUCSOUT
00528      MOVE CSBP-BP-ID (CSBP-X-IDX)         TO                      ELUCSOUT
00529           RGT-BP-ID (RGT-IDX).                                    ELUCSOUT
00530      IF CSBP-INSTITUTIONAL (CSBP-X-IDX)                           ELUCSOUT
00531          PERFORM ADD-INSTITUTIONAL-PAYMENT-LEVE                   ELUCSOUT
00532      ELSE                                                         ELUCSOUT
00533          PERFORM ADD-PROFESSIONAL-PAYMENT-LEVEL.                  ELUCSOUT
00534      IF CSBP-COVERED (CSBP-X-IDX) OR                              ELUCSOUT
00535                 CSBP-COVERED-ON-SUPP (CSBP-X-IDX)                 ELUCSOUT
00536          PERFORM INDICATE-COORDINATED-PAYMENT-L.                  ELUCSOUT
00537                                                                   ELUCSOUT
00538                                                                   ELUCSOUT
00539 ************************************************************      ELUCSOUT
00540 *                                                          *      ELUCSOUT
00541 *        REGROUP PAYMENT LEVELS                            *      ELUCSOUT
00542 *                                                          *      ELUCSOUT
00543 ************************************************************      ELUCSOUT
00544  REGROUP-PAYMENT-LEVELS.                                          ELUCSOUT
00545      PERFORM REGROUP-EACH-COORDINATED-PROVI                       ELUCSOUT
00546          VARYING RGT-CURR-IDX FROM +1 BY +1                       ELUCSOUT
00547                  UNTIL   RGT-CURR-IDX > RGT-NBR-ENTRIES.          ELUCSOUT
00548      EJECT                                                        ELUCSOUT
00549                                                                   ELUCSOUT
00550                                                                   ELUCSOUT
00551 ************************************************************      ELUCSOUT
00552 *                                                          *      ELUCSOUT
00553 *        REGROUP EACH COORDINATED PROVISION ID             *      ELUCSOUT
00554 *                                                          *      ELUCSOUT
00555 ************************************************************      ELUCSOUT
00556  REGROUP-EACH-COORDINATED-PROVI.                                  ELUCSOUT
00557      SET RGT-PREV-IDX TO +1.                                      ELUCSOUT
00558      PERFORM CHECK-CURRENT-ENTRY-FOR-AN-EXI                       ELUCSOUT
00559          UNTIL RGT-PREV-IDX NOT < RGT-CURR-IDX  OR                ELUCSOUT
00560                        RGT-COORD-PAYMENT-LVL (RGT-CURR-IDX) >     ELUCSOUT
00561              0.                                                   ELUCSOUT
00562      IF RGT-COORD-PAYMENT-LVL (RGT-CURR-IDX) = 0                  ELUCSOUT
00563          PERFORM CREATE-NEW-COORDINATED-PAYMENT.                  ELUCSOUT
00564                                                                   ELUCSOUT
00565                                                                   ELUCSOUT
00566 ************************************************************      ELUCSOUT
00567 *                                                          *      ELUCSOUT
00568 *        CHECK CURRENT ENTRY FOR AN EXISTING COORDINATED PA*      ELUCSOUT
00569 *                                                          *      ELUCSOUT
00570 ************************************************************      ELUCSOUT
00571  CHECK-CURRENT-ENTRY-FOR-AN-EXI.                                  ELUCSOUT
00572      IF RGT-COORD-PAYMENT-LVL (RGT-PREV-IDX) =                    ELUCSOUT
00573          RGT-PREV-IDX                                             ELUCSOUT
00574          PERFORM CHECK-CURRENT-ENTRY-FOR-SAME-C.                  ELUCSOUT
00575      SET RGT-PREV-IDX UP BY +1.                                   ELUCSOUT
00576                                                                   ELUCSOUT
00577                                                                   ELUCSOUT
00578 ************************************************************      ELUCSOUT
00579 *                                                          *      ELUCSOUT
00580 *        CHECK CURRENT ENTRY FOR SAME COORDINATED PAYMENT L*      ELUCSOUT
00581 *                                                          *      ELUCSOUT
00582 ************************************************************      ELUCSOUT
00583  CHECK-CURRENT-ENTRY-FOR-SAME-C.                                  ELUCSOUT
00584      IF RGT-DATA (RGT-CURR-IDX) = RGT-DATA                        ELUCSOUT
00585          (RGT-PREV-IDX)                                           ELUCSOUT
00586          PERFORM ASSIGN-CURRENT-ENTRY-TO-EXISTI.                  ELUCSOUT
00587                                                                   ELUCSOUT
00588                                                                   ELUCSOUT
00589 ************************************************************      ELUCSOUT
00590 *                                                          *      ELUCSOUT
00591 *        ASSIGN CURRENT ENTRY TO EXISTING COORDINATED PAYME*      ELUCSOUT
00592 *                                                          *      ELUCSOUT
00593 ************************************************************      ELUCSOUT
00594  ASSIGN-CURRENT-ENTRY-TO-EXISTI.                                  ELUCSOUT
00595      SET RGT-COORD-PAYMENT-LVL (RGT-CURR-IDX) TO                  ELUCSOUT
00596          RGT-PREV-IDX.                                            ELUCSOUT
00597                                                                   ELUCSOUT
00598                                                                   ELUCSOUT
00599 ************************************************************      ELUCSOUT
00600 *                                                          *      ELUCSOUT
00601 *        CREATE NEW COORDINATED PAYMENT LEVEL              *      ELUCSOUT
00602 *                                                          *      ELUCSOUT
00603 ************************************************************      ELUCSOUT
00604  CREATE-NEW-COORDINATED-PAYMENT.                                  ELUCSOUT
00605      SET RGT-COORD-PAYMENT-LVL (RGT-CURR-IDX) TO RGT-CURR-IDX.    ELUCSOUT
00606      EJECT                                                        ELUCSOUT
00607                                                                   ELUCSOUT
00608                                                                   ELUCSOUT
00609 ************************************************************      ELUCSOUT
00610 *                                                          *      ELUCSOUT
00611 *        ASSEMBLE AND OUTPUT HDRS AND SENTENCES            *      ELUCSOUT
00612 *                                                          *      ELUCSOUT
00613 ************************************************************      ELUCSOUT
00614  ASSEMBLE-AND-OUTPUT-HDRS-AND-S.                                  ELUCSOUT
00615      IF CSPG-SUBTOPIC = 'IHS'                                     ELUCSOUT
00616          PERFORM INCREMENT-CSPG-INDEX-TO-TWO-TO                   ELUCSOUT
00617      ELSE                                                         ELUCSOUT
00618          PERFORM INITIALIZE-CSPG-INDEX-TO-ONE.                    ELUCSOUT
00619      PERFORM ASSEMBLE-AND-OUTPUT-HEADERS-AN                       ELUCSOUT
00620          VARYING CSPG-IDX FROM CSPG-IDX BY +1                     ELUCSOUT
00621                  UNTIL   CSPG-IDX > CSPG-TBL-CNT.                 ELUCSOUT
00622                                                                   ELUCSOUT
00623                                                                   ELUCSOUT
00624 ************************************************************      ELUCSOUT
00625 *                                                          *      ELUCSOUT
00626 *        INCREMENT CSPG INDEX TO TWO TO SUPPRESS CHC TEXT  *      ELUCSOUT
00627 *                                                          *      ELUCSOUT
00628 ************************************************************      ELUCSOUT
00629  INCREMENT-CSPG-INDEX-TO-TWO-TO.                                  ELUCSOUT
00630      SET CSPG-IDX TO +2.                                          ELUCSOUT
00631                                                                   ELUCSOUT
00632                                                                   ELUCSOUT
00633 ************************************************************      ELUCSOUT
00634 *                                                          *      ELUCSOUT
00635 *        INITIALIZE CSPG INDEX TO ONE                      *      ELUCSOUT
00636 *                                                          *      ELUCSOUT
00637 ************************************************************      ELUCSOUT
00638  INITIALIZE-CSPG-INDEX-TO-ONE.                                    ELUCSOUT
00639      SET CSPG-IDX TO +1.                                          ELUCSOUT
00640      EJECT                                                        ELUCSOUT
00641                                                                   ELUCSOUT
00642                                                                   ELUCSOUT
00643 ************************************************************      ELUCSOUT
00644 *                                                          *      ELUCSOUT
00645 *        ASSEMBLE AND OUTPUT HEADERS AND SENTENCES FOR EACH*      ELUCSOUT
00646 *                                                          *      ELUCSOUT
00647 ************************************************************      ELUCSOUT
00648  ASSEMBLE-AND-OUTPUT-HEADERS-AN.                                  ELUCSOUT
00649      PERFORM INITIALIZE-OUTPUT-WORK-AREA.                         ELUCSOUT
00650      INITIALIZE WS-PROVN-COVERAGE-SW.                             ELUCSOUT
00651      PERFORM PUT-PROVISION-GROUP-HEADING-IN.                      ELUCSOUT
00652      IF CSPG-GROUP-COVERED (CSPG-IDX)                             ELUCSOUT
00653          PERFORM LIST-COVERED-BENEFIT-PROVISION                   ELUCSOUT
00654      ELSE                                                         ELUCSOUT
00655          PERFORM INDICATE-PROVISION-IN-GROUP-NO.                  ELUCSOUT
00656      IF PROVN-NOT-COVERED                                         ELUCSOUT
00657          PERFORM LIST-NOT-COVERED-BENEFIT-PROVI.                  ELUCSOUT
00658                                                                   ELUCSOUT
00659                                                                   ELUCSOUT
00660 ************************************************************      ELUCSOUT
00661 *                                                          *      ELUCSOUT
00662 *        INITIALIZE OUTPUT WORK AREA                       *      ELUCSOUT
00663 *                                                          *      ELUCSOUT
00664 ************************************************************      ELUCSOUT
00665  INITIALIZE-OUTPUT-WORK-AREA.                                     ELUCSOUT
00666      INITIALIZE WS-OUTPUT-AREA.                                   ELUCSOUT
00667      PERFORM INITIALIZE-OUTPUT-INTERFACE-BL                       ELUCSOUT
00668          VARYING COF-DTL-IDX FROM +1 BY +1                        ELUCSOUT
00669                  UNTIL   COF-DTL-IDX > +20.                       ELUCSOUT
00670      SET WS-HDG-IDX TO +1.                                        ELUCSOUT
00671      SET WS-TXT-IDX TO +1.                                        ELUCSOUT
00672                                                                   ELUCSOUT
00673                                                                   ELUCSOUT
00674 ************************************************************      ELUCSOUT
00675 *                                                          *      ELUCSOUT
00676 *        INITIALIZE OUTPUT INTERFACE BLOCK                 *      ELUCSOUT
00677 *                                                          *      ELUCSOUT
00678 ************************************************************      ELUCSOUT
00679  INITIALIZE-OUTPUT-INTERFACE-BL.                                  ELUCSOUT
00680      MOVE SPACES            TO COF-DTL-LINE                       ELUCSOUT
00681          (COF-DTL-IDX).                                           ELUCSOUT
00682      EJECT                                                        ELUCSOUT
00683                                                                   ELUCSOUT
00684                                                                   ELUCSOUT
00685 ************************************************************      ELUCSOUT
00686 *                                                          *      ELUCSOUT
00687 *        PUT PROVISION GROUP HEADING IN OUTPUT WORK AREA   *      ELUCSOUT
00688 *                                                          *      ELUCSOUT
00689 ************************************************************      ELUCSOUT
00690  PUT-PROVISION-GROUP-HEADING-IN.                                  ELUCSOUT
00691      MOVE CSPG-SUBTOPIC     TO WS-CSPG-SUBTOPIC.                  ELUCSOUT
00692      SET  WS-CSPG-PROVN-GRP TO CSPG-IDX.                          ELUCSOUT
00693      SET WS-PGHT-IDX TO +1.                                       ELUCSOUT
00694      SEARCH ALL WS-PROVN-GRP-HDG-TBL                              ELUCSOUT
00695             AT END                                                ELUCSOUT
00696                  SET CIA-AB-PARM-ERR TO TRUE                      ELUCSOUT
00697                  EXEC CICS ABEND                                  ELUCSOUT
00698                            ABCODE (CIA-ABCODE)                    ELUCSOUT
00699                  END-EXEC                                         ELUCSOUT
00700             WHEN WS-PGHT-KEY (WS-PGHT-IDX) =                      ELUCSOUT
00701          WS-CSPG-KEY                                              ELUCSOUT
00702                  MOVE WS-PGHT-PROVN-GRP-HDG (WS-PGHT-IDX)         ELUCSOUT
00703          TO                                                       ELUCSOUT
00704                       WS-HEADING (WS-HDG-IDX)                     ELUCSOUT
00705         END-SEARCH.                                               ELUCSOUT
00706      SET WS-HDG-IDX UP BY +1.                                     ELUCSOUT
00707      SET WS-TXT-IDX UP BY +1.                                     ELUCSOUT
00708      EJECT                                                        ELUCSOUT
00709                                                                   ELUCSOUT
00710                                                                   ELUCSOUT
00711 ************************************************************      ELUCSOUT
00712 *                                                          *      ELUCSOUT
00713 *        LIST COVERED BENEFIT PROVISIONS IN PROVISION GROUP*      ELUCSOUT
00714 *                                                          *      ELUCSOUT
00715 ************************************************************      ELUCSOUT
00716  LIST-COVERED-BENEFIT-PROVISION.                                  ELUCSOUT
00717      PERFORM LIST-EACH-COVERED-COORDINATEDX                       ELUCSOUT
00718          VARYING RGT-IDX FROM +1 BY +1                            ELUCSOUT
00719                  UNTIL   RGT-IDX > RGT-NBR-ENTRIES                ELUCSOUT
00720                  OR      RGT-PROVISION-GRP (RGT-IDX) >            ELUCSOUT
00721              CSPG-IDX.                                            ELUCSOUT
00722                                                                   ELUCSOUT
00723                                                                   ELUCSOUT
00724 ************************************************************      ELUCSOUT
00725 *                                                          *      ELUCSOUT
00726 *        LIST EACH COVERED COORDINATED PAYMENT LEVEL       *      ELUCSOUT
00727 *                                                          *      ELUCSOUT
00728 ************************************************************      ELUCSOUT
00729  LIST-EACH-COVERED-COORDINATEDX.                                  ELUCSOUT
00730      IF RGT-COORD-PAYMENT-LVL (RGT-IDX) = RGT-IDX  AND            ELUCSOUT
00731                 RGT-COORD-COVERED (RGT-IDX)                       ELUCSOUT
00732          AND                                                      ELUCSOUT
00733                (RGT-PROVISION-GRP (RGT-IDX) = CSPG-IDX)           ELUCSOUT
00734          AND                                                      ELUCSOUT
00735                (RGT-INST-PAYMENT-LVL (RGT-IDX) > ZEROS            ELUCSOUT
00736          OR                                                       ELUCSOUT
00737                 RGT-PROF-PAYMENT-LVL (RGT-IDX) > ZEROS)           ELUCSOUT
00738          PERFORM LIST-COORDINATED-PAYMENT-LEVEL                   ELUCSOUT
00739      ELSE IF RGT-COORD-PAYMENT-LVL (RGT-IDX) = RGT-IDX  AND       ELUCSOUT
00740                 RGT-COORD-NOT-COVERED (RGT-IDX)                   ELUCSOUT
00741          AND                                                      ELUCSOUT
00742                (RGT-PROVISION-GRP (RGT-IDX) = CSPG-IDX)           ELUCSOUT
00743          PERFORM INDICATE-PROVISION-IN-GROUP-NO.                  ELUCSOUT
00744      EJECT                                                        ELUCSOUT
00745                                                                   ELUCSOUT
00746                                                                   ELUCSOUT
00747 ************************************************************      ELUCSOUT
00748 *                                                          *      ELUCSOUT
00749 *        INDICATE PROVISION IN GROUP NOT COVERED           *      ELUCSOUT
00750 *                                                          *      ELUCSOUT
00751 ************************************************************      ELUCSOUT
00752  INDICATE-PROVISION-IN-GROUP-NO.                                  ELUCSOUT
00753      SET PROVN-NOT-COVERED TO TRUE.                               ELUCSOUT
00754      EJECT                                                        ELUCSOUT
00755                                                                   ELUCSOUT
00756                                                                   ELUCSOUT
00757 ************************************************************      ELUCSOUT
00758 *                                                          *      ELUCSOUT
00759 *        LIST COORDINATED PAYMENT LEVEL                    *      ELUCSOUT
00760 *                                                          *      ELUCSOUT
00761 ************************************************************      ELUCSOUT
00762  LIST-COORDINATED-PAYMENT-LEVEL.                                  ELUCSOUT
00763      PERFORM MOVE-BENEFIT-PROVISION-HEADING.                      ELUCSOUT
00764      IF RGT-INST-PAYMENT-LVL (RGT-IDX) > ZEROS                    ELUCSOUT
00765          PERFORM MOVE-INSTITUTIONAL-SENTENCE-IN.                  ELUCSOUT
00766      IF RGT-PROF-PAYMENT-LVL (RGT-IDX) > ZEROS                    ELUCSOUT
00767          PERFORM MOVE-PROFESSIONAL-SENTENCE-INT.                  ELUCSOUT
00768      PERFORM OUTPUT-COORDINATED-PAYMENT-LEV.                      ELUCSOUT
00769      PERFORM DISPLAY-REMAINING-BENEFIT-PROV                       ELUCSOUT
00770          UNTIL HDGS-COMPLETED.                                    ELUCSOUT
00771                                                                   ELUCSOUT
00772                                                                   ELUCSOUT
00773 ************************************************************      ELUCSOUT
00774 *                                                          *      ELUCSOUT
00775 *        DISPLAY REMAINING BENEFIT PROVISIONS HEADINGS     *      ELUCSOUT
00776 *                                                          *      ELUCSOUT
00777 ************************************************************      ELUCSOUT
00778  DISPLAY-REMAINING-BENEFIT-PROV.                                  ELUCSOUT
00779      PERFORM MOVE-BENEFIT-PROVISION-HEADING.                      ELUCSOUT
00780      PERFORM OUTPUT-COORDINATED-PAYMENT-LEV.                      ELUCSOUT
00781                                                                   ELUCSOUT
00782                                                                   ELUCSOUT
00783 ************************************************************      ELUCSOUT
00784 *                                                          *      ELUCSOUT
00785 *        DETERMINE HOW MANY LINES TO SEND                  *      ELUCSOUT
00786 *                                                          *      ELUCSOUT
00787 ************************************************************      ELUCSOUT
00788  DETERMINE-HOW-MANY-LINES-TO-SE.                                  ELUCSOUT
00789      IF WS-HDG-IDX > WS-TXT-IDX                                   ELUCSOUT
00790          PERFORM USE-THE-HEADER-INDEX-FOR-GUIDE                   ELUCSOUT
00791      ELSE                                                         ELUCSOUT
00792          PERFORM USE-THE-TEXT-INDEX-FOR-GUIDE.                    ELUCSOUT
00793                                                                   ELUCSOUT
00794                                                                   ELUCSOUT
00795 ************************************************************      ELUCSOUT
00796 *                                                          *      ELUCSOUT
00797 *        USE THE HEADER INDEX FOR GUIDE                    *      ELUCSOUT
00798 *                                                          *      ELUCSOUT
00799 ************************************************************      ELUCSOUT
00800  USE-THE-HEADER-INDEX-FOR-GUIDE.                                  ELUCSOUT
00801      SET WS-LAST-LINE-IND TO WS-HDG-IDX.                          ELUCSOUT
00802                                                                   ELUCSOUT
00803                                                                   ELUCSOUT
00804 ************************************************************      ELUCSOUT
00805 *                                                          *      ELUCSOUT
00806 *        USE THE TEXT INDEX FOR GUIDE                      *      ELUCSOUT
00807 *                                                          *      ELUCSOUT
00808 ************************************************************      ELUCSOUT
00809  USE-THE-TEXT-INDEX-FOR-GUIDE.                                    ELUCSOUT
00810      SET WS-LAST-LINE-IND TO WS-TXT-IDX.                          ELUCSOUT
00811      EJECT                                                        ELUCSOUT
00812                                                                   ELUCSOUT
00813                                                                   ELUCSOUT
00814 ************************************************************      ELUCSOUT
00815 *                                                          *      ELUCSOUT
00816 *        MOVE BENEFIT PROVISION HEADINGS INTO WORK AREA    *      ELUCSOUT
00817 *                                                          *      ELUCSOUT
00818 ************************************************************      ELUCSOUT
00819  MOVE-BENEFIT-PROVISION-HEADING.                                  ELUCSOUT
00820      PERFORM MOVE-INDIVIDUAL-BENEFIT-PROVIS                       ELUCSOUT
00821          VARYING RGT-CURR-IDX FROM RGT-IDX BY +1                  ELUCSOUT
00822                  UNTIL   RGT-CURR-IDX > RGT-NBR-ENTRIES           ELUCSOUT
00823                  OR      WS-HDG-IDX > +19.                        ELUCSOUT
00824      IF WS-HDG-IDX > +19                                          ELUCSOUT
00825          PERFORM INDICATE-MORE-BP-HEADINGS-NEED                   ELUCSOUT
00826      ELSE                                                         ELUCSOUT
00827          PERFORM INDICATE-BP-HEADINGS-COMPLETED.                  ELUCSOUT
00828                                                                   ELUCSOUT
00829                                                                   ELUCSOUT
00830 ************************************************************      ELUCSOUT
00831 *                                                          *      ELUCSOUT
00832 *        INDICATE MORE BP HEADINGS NEEDED                  *      ELUCSOUT
00833 *                                                          *      ELUCSOUT
00834 ************************************************************      ELUCSOUT
00835  INDICATE-MORE-BP-HEADINGS-NEED.                                  ELUCSOUT
00836      SET HDGS-NEEDED TO TRUE.                                     ELUCSOUT
00837                                                                   ELUCSOUT
00838                                                                   ELUCSOUT
00839 ************************************************************      ELUCSOUT
00840 *                                                          *      ELUCSOUT
00841 *        INDICATE BP HEADINGS COMPLETED                    *      ELUCSOUT
00842 *                                                          *      ELUCSOUT
00843 ************************************************************      ELUCSOUT
00844  INDICATE-BP-HEADINGS-COMPLETED.                                  ELUCSOUT
00845      SET HDGS-COMPLETED TO TRUE.                                  ELUCSOUT
00846                                                                   ELUCSOUT
00847                                                                   ELUCSOUT
00848 ************************************************************      ELUCSOUT
00849 *                                                          *      ELUCSOUT
00850 *        MOVE INDIVIDUAL BENEFIT PROVISION HEADING INTO WOR*      ELUCSOUT
00851 *                                                          *      ELUCSOUT
00852 ************************************************************      ELUCSOUT
00853  MOVE-INDIVIDUAL-BENEFIT-PROVIS.                                  ELUCSOUT
00854      IF (RGT-COORD-PAYMENT-LVL (RGT-CURR-IDX)  =                  ELUCSOUT
00855                  RGT-COORD-PAYMENT-LVL (RGT-IDX))                 ELUCSOUT
00856          PERFORM MOVE-BENEFIT-PROVISION-HDNG-FO.                  ELUCSOUT
00857      EJECT                                                        ELUCSOUT
00858                                                                   ELUCSOUT
00859                                                                   ELUCSOUT
00860 ************************************************************      ELUCSOUT
00861 *                                                          *      ELUCSOUT
00862 *        MOVE BENEFIT PROVISION HDNG FOR THIS PROVISION    *      ELUCSOUT
00863 *                                                          *      ELUCSOUT
00864 ************************************************************      ELUCSOUT
00865  MOVE-BENEFIT-PROVISION-HDNG-FO.                                  ELUCSOUT
00866      SET WS-BPHT-IDX TO +1.                                       ELUCSOUT
00867      SEARCH ALL WS-PROVN-HDG-TBL                                  ELUCSOUT
00868             AT END                                                ELUCSOUT
00869                SET CIA-AB-PARM-ERR TO TRUE                        ELUCSOUT
00870                EXEC CICS ABEND                                    ELUCSOUT
00871                          ABCODE (CIA-ABCODE)                      ELUCSOUT
00872                END-EXEC                                           ELUCSOUT
00873           WHEN WS-PROVN-BP-ID (WS-BPHT-IDX) = RGT-BP-ID           ELUCSOUT
00874          (RGT-CURR-IDX)                                           ELUCSOUT
00875                MOVE WS-PROVN-HDG (WS-BPHT-IDX) TO                 ELUCSOUT
00876                     WS-HEADING (WS-HDG-IDX)                       ELUCSOUT
00877         END-SEARCH.                                               ELUCSOUT
00878      SET WS-HDG-IDX UP BY +1.                                     ELUCSOUT
00879      EJECT                                                        ELUCSOUT
00880                                                                   ELUCSOUT
00881                                                                   ELUCSOUT
00882 ************************************************************      ELUCSOUT
00883 *                                                          *      ELUCSOUT
00884 *        MOVE INSTITUTIONAL SENTENCE INTO WORK AREA        *      ELUCSOUT
00885 *                                                          *      ELUCSOUT
00886 ************************************************************      ELUCSOUT
00887  MOVE-INSTITUTIONAL-SENTENCE-IN.                                  ELUCSOUT
00888      SET CSPG-IDX TO RGT-PROVISION-GRP (RGT-IDX).                 ELUCSOUT
00889      IF CSPG-BOTH (CSPG-IDX)                                      ELUCSOUT
00890          PERFORM MOVE-INSTITUTIONAL-HEADING-INT.                  ELUCSOUT
00891      SET CSBP-X-IDX TO RGT-INST-PAYMENT-LVL (RGT-IDX).            ELUCSOUT
00892      IF CSBP-SENTENCE-PTR (CSBP-X-IDX) NOT = NULL                 ELUCSOUT
00893          PERFORM MOVE-INSTITUTIONAL-SENTENCE-TE.                  ELUCSOUT
00894                                                                   ELUCSOUT
00895                                                                   ELUCSOUT
00896 ************************************************************      ELUCSOUT
00897 *                                                          *      ELUCSOUT
00898 *        MOVE INSTITUTIONAL HEADING INTO WORK AREA         *      ELUCSOUT
00899 *                                                          *      ELUCSOUT
00900 ************************************************************      ELUCSOUT
00901  MOVE-INSTITUTIONAL-HEADING-INT.                                  ELUCSOUT
00902      MOVE PC-INST      TO WS-TEXT (WS-TXT-IDX).                   ELUCSOUT
00903      SET WS-TXT-IDX UP BY +1.                                     ELUCSOUT
00904                                                                   ELUCSOUT
00905                                                                   ELUCSOUT
00906 ************************************************************      ELUCSOUT
00907 *                                                          *      ELUCSOUT
00908 *        MOVE INSTITUTIONAL SENTENCE TEXT INTO WORK AREA   *      ELUCSOUT
00909 *                                                          *      ELUCSOUT
00910 ************************************************************      ELUCSOUT
00911  MOVE-INSTITUTIONAL-SENTENCE-TE.                                  ELUCSOUT
00912      SET ADDRESS OF CSEN-SENTENCE-TABLE                           ELUCSOUT
00913                  TO CSBP-SENTENCE-PTR (CSBP-X-IDX).               ELUCSOUT
00914      SET CSEN-Y-IDX TO +1.                                        ELUCSOUT
00915      PERFORM MOVE-SENTENCE-TO-OUTPUT-WORK-A                       ELUCSOUT
00916          UNTIL CSEN-Y-IDX > CSEN-NBR-SENTENCES.                   ELUCSOUT
00917      EJECT                                                        ELUCSOUT
00918                                                                   ELUCSOUT
00919                                                                   ELUCSOUT
00920 ************************************************************      ELUCSOUT
00921 *                                                          *      ELUCSOUT
00922 *        MOVE PROFESSIONAL SENTENCE INTO WORK AREA         *      ELUCSOUT
00923 *                                                          *      ELUCSOUT
00924 ************************************************************      ELUCSOUT
00925  MOVE-PROFESSIONAL-SENTENCE-INT.                                  ELUCSOUT
00926      SET CSPG-IDX TO RGT-PROVISION-GRP (RGT-IDX).                 ELUCSOUT
00927      IF CSPG-BOTH (CSPG-IDX)                                      ELUCSOUT
00928          PERFORM MOVE-PROFESSIONAL-HEADING-INTO.                  ELUCSOUT
00929      SET CSBP-X-IDX TO RGT-PROF-PAYMENT-LVL (RGT-IDX).            ELUCSOUT
00930      IF CSBP-SENTENCE-PTR (CSBP-X-IDX) NOT = NULL                 ELUCSOUT
00931          PERFORM MOVE-PROFESSIONAL-SENTENCE-TEX.                  ELUCSOUT
00932                                                                   ELUCSOUT
00933                                                                   ELUCSOUT
00934 ************************************************************      ELUCSOUT
00935 *                                                          *      ELUCSOUT
00936 *        MOVE PROFESSIONAL HEADING INTO WORK AREA          *      ELUCSOUT
00937 *                                                          *      ELUCSOUT
00938 ************************************************************      ELUCSOUT
00939  MOVE-PROFESSIONAL-HEADING-INTO.                                  ELUCSOUT
00940      MOVE PC-PROF      TO WS-TEXT (WS-TXT-IDX).                   ELUCSOUT
00941      SET WS-TXT-IDX UP BY +1.                                     ELUCSOUT
00942                                                                   ELUCSOUT
00943                                                                   ELUCSOUT
00944 ************************************************************      ELUCSOUT
00945 *                                                          *      ELUCSOUT
00946 *        MOVE PROFESSIONAL SENTENCE TEXT INTO WORK AREA    *      ELUCSOUT
00947 *                                                          *      ELUCSOUT
00948 ************************************************************      ELUCSOUT
00949  MOVE-PROFESSIONAL-SENTENCE-TEX.                                  ELUCSOUT
00950      SET ADDRESS OF CSEN-SENTENCE-TABLE                           ELUCSOUT
00951                  TO CSBP-SENTENCE-PTR (CSBP-X-IDX).               ELUCSOUT
00952      SET CSEN-Y-IDX TO +1.                                        ELUCSOUT
00953      PERFORM MOVE-SENTENCE-TO-OUTPUT-WORK-A                       ELUCSOUT
00954          UNTIL CSEN-Y-IDX > CSEN-NBR-SENTENCES.                   ELUCSOUT
00955                                                                   ELUCSOUT
00956                                                                   ELUCSOUT
00957 ************************************************************      ELUCSOUT
00958 *                                                          *      ELUCSOUT
00959 *        MOVE SENTENCE TO OUTPUT WORK AREA                 *      ELUCSOUT
00960 *                                                          *      ELUCSOUT
00961 ************************************************************      ELUCSOUT
00962  MOVE-SENTENCE-TO-OUTPUT-WORK-A.                                  ELUCSOUT
00963      PERFORM INSERT-SENTENCES-TO-WS-TEXT-AR                       ELUCSOUT
00964          VARYING CSEN-Y-IDX FROM CSEN-Y-IDX BY +1                 ELUCSOUT
00965                  UNTIL   CSEN-Y-IDX > CSEN-NBR-SENTENCES          ELUCSOUT
00966                  OR      WS-TXT-IDX > +19.                        ELUCSOUT
00967      IF WS-TXT-IDX > +19                                          ELUCSOUT
00968          PERFORM OUTPUT-COORDINATED-PAYMENT-LEV.                  ELUCSOUT
00969      EJECT                                                        ELUCSOUT
00970                                                                   ELUCSOUT
00971                                                                   ELUCSOUT
00972 ************************************************************      ELUCSOUT
00973 *                                                          *      ELUCSOUT
00974 *        INSERT SENTENCES TO WS TEXT AREA                  *      ELUCSOUT
00975 *                                                          *      ELUCSOUT
00976 ************************************************************      ELUCSOUT
00977  INSERT-SENTENCES-TO-WS-TEXT-AR.                                  ELUCSOUT
00978      MOVE CSEN-SENTENCES (CSEN-Y-IDX) TO WS-TEXT                  ELUCSOUT
00979          (WS-TXT-IDX).                                            ELUCSOUT
00980      SET WS-TXT-IDX UP BY +1.                                     ELUCSOUT
00981      EJECT                                                        ELUCSOUT
00982                                                                   ELUCSOUT
00983                                                                   ELUCSOUT
00984 ************************************************************      ELUCSOUT
00985 *                                                          *      ELUCSOUT
00986 *        OUTPUT COORDINATED PAYMENT LEVEL                  *      ELUCSOUT
00987 *                                                          *      ELUCSOUT
00988 ************************************************************      ELUCSOUT
00989  OUTPUT-COORDINATED-PAYMENT-LEV.                                  ELUCSOUT
00990      PERFORM DETERMINE-HOW-MANY-LINES-TO-SE.                      ELUCSOUT
00991 ***************************************************************   ELUCSOUT
00992 ** PROCESSING UNTIL WS-LAST-LINE-IND WILL HAVE THE EFFECT OF **   ELUCSOUT
00993 ** ISSUING A BLANK LINE AFTER ALL TEXT HAS BEEN DISPLAYED    **   ELUCSOUT
00994 ***************************************************************   ELUCSOUT
00995      PERFORM INSERT-THE-LINES-IN-WS-TO-THEX                       ELUCSOUT
00996          VARYING WS-HDG-IDX FROM +1 BY +1                         ELUCSOUT
00997                  UNTIL   WS-HDG-IDX > WS-LAST-LINE-IND.           ELUCSOUT
00998      PERFORM LINK-TO-OUTPUT-INTERFACE.                            ELUCSOUT
00999      PERFORM INITIALIZE-OUTPUT-WORK-AREA.                         ELUCSOUT
01000      EJECT                                                        ELUCSOUT
01001                                                                   ELUCSOUT
01002                                                                   ELUCSOUT
01003 ************************************************************      ELUCSOUT
01004 *                                                          *      ELUCSOUT
01005 *        INSERT THE LINES IN WS TO THE OUTPUT AREA         *      ELUCSOUT
01006 *                                                          *      ELUCSOUT
01007 ************************************************************      ELUCSOUT
01008  INSERT-THE-LINES-IN-WS-TO-THEX.                                  ELUCSOUT
01009      MOVE PC-BAR                      TO WS-DIVIDER               ELUCSOUT
01010          (WS-HDG-IDX).                                            ELUCSOUT
01011      ADD +1                           TO COF-NBR-DTL-LINES.       ELUCSOUT
01012      MOVE WS-OUTPUT-LINE (WS-HDG-IDX) TO COF-DTL-LINE             ELUCSOUT
01013          (COF-NBR-DTL-LINES).                                     ELUCSOUT
01014      EJECT                                                        ELUCSOUT
01015                                                                   ELUCSOUT
01016                                                                   ELUCSOUT
01017 ************************************************************      ELUCSOUT
01018 *                                                          *      ELUCSOUT
01019 *        LINK TO OUTPUT INTERFACE                          *      ELUCSOUT
01020 *                                                          *      ELUCSOUT
01021 ************************************************************      ELUCSOUT
01022  LINK-TO-OUTPUT-INTERFACE.                                        ELUCSOUT
01023      EXEC CICS LINK                                               ELUCSOUT
01024                PROGRAM ('ELUOUTPT')                               ELUCSOUT
01025                COMMAREA (DFHCOMMAREA)                             ELUCSOUT
01026         END-EXEC.                                                 ELUCSOUT
01027      EJECT                                                        ELUCSOUT
01028                                                                   ELUCSOUT
01029                                                                   ELUCSOUT
01030 ************************************************************      ELUCSOUT
01031 *                                                          *      ELUCSOUT
01032 *        LIST NOT COVERED BENEFIT PROVISIONS IN PROVISION G*      ELUCSOUT
01033 *                                                          *      ELUCSOUT
01034 ************************************************************      ELUCSOUT
01035  LIST-NOT-COVERED-BENEFIT-PROVI.                                  ELUCSOUT
01036      PERFORM FIND-ALL-NOT-COVERED-PROVISION                       ELUCSOUT
01037          VARYING RGT-IDX FROM +1 BY +1                            ELUCSOUT
01038                  UNTIL   RGT-IDX > RGT-NBR-ENTRIES                ELUCSOUT
01039                  OR      RGT-PROVISION-GRP (RGT-IDX) >            ELUCSOUT
01040              CSPG-IDX.                                            ELUCSOUT
01041                                                                   ELUCSOUT
01042                                                                   ELUCSOUT
01043 ************************************************************      ELUCSOUT
01044 *                                                          *      ELUCSOUT
01045 *        FIND ALL NOT COVERED PROVISIONS IN CURRENT PROVISI*      ELUCSOUT
01046 *                                                          *      ELUCSOUT
01047 ************************************************************      ELUCSOUT
01048  FIND-ALL-NOT-COVERED-PROVISION.                                  ELUCSOUT
01049      IF RGT-COORD-PAYMENT-LVL (RGT-IDX) = RGT-IDX AND             ELUCSOUT
01050                 RGT-COORD-NOT-COVERED (RGT-IDX)                   ELUCSOUT
01051          AND                                                      ELUCSOUT
01052                (RGT-PROVISION-GRP (RGT-IDX) = CSPG-IDX)           ELUCSOUT
01053          PERFORM LIST-COORDINATED-PAYMENT-LEVEL.                  ELUCSOUT
