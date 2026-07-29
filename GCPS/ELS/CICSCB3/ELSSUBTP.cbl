00001 *      LAST MAINTENANCE TIME: 15.23.36  DATE: 04/24/89            09/03/03
00002 * STRUCTURE(S) MEMBER ELSSUBTPPL - LEVEL 012 AS OF 10/02/87       ELSSUBTP
00003 * FROM PANLIB R360059.STRUCTPL.PANLIB                                LV002
00004 *                                                                 ELSSUBTP
00005  IDENTIFICATION DIVISION.                                         ELSSUBTP
00006                                                                   ELSSUBTP
00007  PROGRAM-ID.         ELSSUBTP.                                    ELSSUBTP
00008                                                                   ELSSUBTP
00009  AUTHOR.             JOHN CURIN,  KEANE, INC.                     ELSSUBTP
00010                                                                   ELSSUBTP
00011  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSSUBTP
00012                      A MUTUAL LEGAL RESERVE COMPANY               ELSSUBTP
00013                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSSUBTP
00014                      233 N. MICHIGAN AVE                          ELSSUBTP
00015                      CHICAGO, ILLINOIS 60601                      ELSSUBTP
00016                                                                   ELSSUBTP
00017  DATE-WRITTEN.       24-OCT-1986.                                 ELSSUBTP
00018                                                                   ELSSUBTP
00019  DATE-COMPILED.                                                   ELSSUBTP
00020                                                                   ELSSUBTP
00021  SECURITY.           COPYRIGHT 1986,                              ELSSUBTP
00022                      HEALTH CARE SERVICE CORPORATION              ELSSUBTP
00023      SKIP3                                                        ELSSUBTP
00024  ENVIRONMENT DIVISION.                                            ELSSUBTP
00025                                                                   ELSSUBTP
00026  CONFIGURATION SECTION.                                           ELSSUBTP
00027  SOURCE-COMPUTER.    IBM-3033.                                    ELSSUBTP
00028  OBJECT-COMPUTER.    IBM-3033.                                    ELSSUBTP
00029      EJECT                                                        ELSSUBTP
00030 ***********************************************************       ELSSUBTP
00031 *              UPDATE HISTORY                                     ELSSUBTP
00032 *                                                                 ELSSUBTP
00033 * DATE         BY    ACTION                                       ELSSUBTP
00034 *                                                                 ELSSUBTP
00035 * ??/??/??    JTC    CREATED                                      ELSSUBTP
00036 *                                                                 ELSSUBTP
00037 * 04/24/89    AKK    DESTRUCTED PROGRAM.                          ELSSUBTP
00038 *                                                                 ELSSUBTP
00039 * 04/25/89    GEM    STORAGE MANAGEMENT ENHANCEMENT.              ELSSUBTP
00040 *                                                                 ELSSUBTP
00041 * 08/16/95    AKK    ADD SUBTOPIC PROCESSING FOR OB.              ELSSUBTP
00042 *                                                                 ELSSUBTP
00043 *       13-AUG-2003 AKK       REGEN FOR ORDER OF COMPILE TEST     ELSSUBTP
00044 ***********************************************************       ELSSUBTP
00045  DATA DIVISION.                                                   ELSSUBTP
00046                                                                   ELSSUBTP
00047  FILE SECTION.                                                    ELSSUBTP
00048                                                                   ELSSUBTP
00049  WORKING-STORAGE SECTION.                                         ELSSUBTP
00050                                                                   ELSSUBTP
00051 *** TOPIC SELECTOR WORK AREA                                      ELSSUBTP
00052      COPY ELSSUBTP.                                               ELSSUBTP
00053                                                                   ELSSUBTP
00054  01  WS-HOLD-SUBTOPIC-NAME.                                       ELSSUBTP
00055      05  WS-HOLD-SUBTOPIC-NUMBER     PIC XXX.                     ELSSUBTP
00056      05  WS-HOLD-SUBTOPIC-NAME-ONLY  PIC X(57).                   ELSSUBTP
00057                                                                   ELSSUBTP
00058  01  WS-DUMMY-PTR                    POINTER.                     ELSSUBTP
00059      EJECT                                                        ELSSUBTP
00060  LINKAGE SECTION.                                                 ELSSUBTP
00061                                                                   ELSSUBTP
00062  01  DFHCOMMAREA.                                                 ELSSUBTP
00063      COPY ELSCOMMC.                                               ELSSUBTP
00064      EJECT                                                        ELSSUBTP
00065      COPY ELSCIA2C.                                               ELSSUBTP
00066      EJECT                                                        ELSSUBTP
00067      COPY ELSIOPMC.                                               ELSSUBTP
00068      EJECT                                                        ELSSUBTP
00069      COPY ELSSSCBC.                                               ELSSUBTP
00070      EJECT                                                        ELSSUBTP
00071      COPY ELSMENUC.                                               ELSSUBTP
00072      EJECT                                                        ELSSUBTP
00073      COPY ELSMHDGC.                                               ELSSUBTP
00074      EJECT                                                        ELSSUBTP
00075      COPY ELSMOPTC.                                               ELSSUBTP
00076      EJECT                                                        ELSSUBTP
00077      COPY ELSSUBTG.                                               ELSSUBTP
00078      EJECT                                                        ELSSUBTP
00079      EJECT                                                        ELSSUBTP
00080  PROCEDURE DIVISION.                                              ELSSUBTP
00081 ************************************************************      ELSSUBTP
00082 *                                                          *      ELSSUBTP
00083 *                    PROCEDURE DIVISION                    *      ELSSUBTP
00084 *                                                          *      ELSSUBTP
00085 ************************************************************      ELSSUBTP
00086                                                                   ELSSUBTP
00087                                                                   ELSSUBTP
00088 ************************************************************      ELSSUBTP
00089 *                                                          *      ELSSUBTP
00090 *        IDENTIFICATION DIVISION                           *      ELSSUBTP
00091 *                                                          *      ELSSUBTP
00092 ************************************************************      ELSSUBTP
00093  IDENTIFICATION-DIVISION.                                         ELSSUBTP
00094      PERFORM INITIALIZATION                                       ELSSUBTP
00095      PERFORM PROCESS-SUB-TOPIC-SELECTOR-REQ.                      ELSSUBTP
00096      GOBACK.                                                      ELSSUBTP
00097                                                                   ELSSUBTP
00098                                                                   ELSSUBTP
00099 ************************************************************      ELSSUBTP
00100 *                                                          *      ELSSUBTP
00101 *        INITIALIZE                                        *      ELSSUBTP
00102 *                                                          *      ELSSUBTP
00103 ************************************************************      ELSSUBTP
00104  INITIALIZATION.                                                  ELSSUBTP
00105      PERFORM CHECK-COMMAREA-LENGTH.                               ELSSUBTP
00106      PERFORM ESTABLISH-ADDRESSING-TO-COMMON.                      ELSSUBTP
00107      PERFORM ESTABLISH-ADDRESSING-TO-SELECT.                      ELSSUBTP
00108      EJECT                                                        ELSSUBTP
00109                                                                   ELSSUBTP
00110                                                                   ELSSUBTP
00111 ************************************************************      ELSSUBTP
00112 *                                                          *      ELSSUBTP
00113 *        PROCESS SUB-TOPIC SELECTOR REQUEST                *      ELSSUBTP
00114 *                                                          *      ELSSUBTP
00115 ************************************************************      ELSSUBTP
00116  PROCESS-SUB-TOPIC-SELECTOR-REQ.                                  ELSSUBTP
00117      IF SSB-TOPIC = 'DX              '                            ELSSUBTP
00118          PERFORM PROCESS-DIAGNOSTIC-SERVICES-SU                   ELSSUBTP
00119      ELSE IF SSB-TOPIC = 'EMER            '                       ELSSUBTP
00120          PERFORM PROCESS-EMERGENCY-CARE-SUBTOPI                   ELSSUBTP
00121      ELSE IF SSB-TOPIC = 'PROST           '                       ELSSUBTP
00122          PERFORM PROCESS-PROSTHETICS-ORTHOTICSX                   ELSSUBTP
00123      ELSE IF SSB-TOPIC = 'SURG            '                       ELSSUBTP
00124          PERFORM PROCESS-SURGERY-SUBTOPIC                         ELSSUBTP
00125      ELSE IF SSB-TOPIC = 'THER            '                       ELSSUBTP
00126          PERFORM PROCESS-THERAPIES-SUBTOPIC                       ELSSUBTP
00127      ELSE IF SSB-TOPIC = 'ADMIN           '                       ELSSUBTP
00128          PERFORM PROCESS-GENERAL-ADMINISTRATION                   ELSSUBTP
00129      ELSE IF SSB-TOPIC = 'OB              '                       ELSSUBTP
00130          PERFORM PROCESS-OB-SERVICES-SUBTOPICS                    ELSSUBTP
00131      ELSE                                                         ELSSUBTP
00132          PERFORM SIGNAL-INVALID-SUB-TOPIC-SELEC.                  ELSSUBTP
00133      EJECT                                                        ELSSUBTP
00134                                                                   ELSSUBTP
00135                                                                   ELSSUBTP
00136 ************************************************************      ELSSUBTP
00137 *                                                          *      ELSSUBTP
00138 *        PROCESS DIAGNOSTIC SERVICES SUBTOPIC              *      ELSSUBTP
00139 *                                                          *      ELSSUBTP
00140 ************************************************************      ELSSUBTP
00141  PROCESS-DIAGNOSTIC-SERVICES-SU.                                  ELSSUBTP
00142      PERFORM SET-DIAGNOSTIC-SERVICES-LINKAG.                      ELSSUBTP
00143      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSSUBTP
00144          PERFORM PROCESS-BUILD-DIAGNOSTIC-SERVI                   ELSSUBTP
00145      ELSE IF SSB-MENU-COMPLETE(SSB-SELECTOR-STATE)                ELSSUBTP
00146          PERFORM PROCESS-MENU-COMPLETED-REQUEST                   ELSSUBTP
00147      ELSE                                                         ELSSUBTP
00148          PERFORM SIGNAL-INVALID-SUB-TOPIC-SELEC.                  ELSSUBTP
00149                                                                   ELSSUBTP
00150                                                                   ELSSUBTP
00151 ************************************************************      ELSSUBTP
00152 *                                                          *      ELSSUBTP
00153 *        PROCESS BUILD DIAGNOSTIC SERVICES MENU REQUEST    *      ELSSUBTP
00154 *                                                          *      ELSSUBTP
00155 ************************************************************      ELSSUBTP
00156  PROCESS-BUILD-DIAGNOSTIC-SERVI.                                  ELSSUBTP
00157      MOVE SBT-DI-TOPIC-TITLE TO SSB-MNU-TITLE.                    ELSSUBTP
00158      PERFORM PROCESS-BUILD-MENU-REQUEST.                          ELSSUBTP
00159                                                                   ELSSUBTP
00160                                                                   ELSSUBTP
00161 ************************************************************      ELSSUBTP
00162 *                                                          *      ELSSUBTP
00163 *        SET DIAGNOSTIC SERVICES LINKAGE ADDRESS           *      ELSSUBTP
00164 *                                                          *      ELSSUBTP
00165 ************************************************************      ELSSUBTP
00166  SET-DIAGNOSTIC-SERVICES-LINKAG.                                  ELSSUBTP
00167      CALL 'ELUADDRS' USING SBT-DI-TABLE-COUNTS,                   ELSSUBTP
00168            ADDRESS OF GEN-TABLE-COUNTS.                           ELSSUBTP
00169      CALL 'ELUADDRS' USING SBT-DI-TOPIC-HEADINGS,                 ELSSUBTP
00170            ADDRESS OF GEN-TOPIC-HEAD.                             ELSSUBTP
00171      CALL 'ELUADDRS' USING                                        ELSSUBTP
00172          SBT-DI-TOPIC-VALID-DEFINITION,                           ELSSUBTP
00173            ADDRESS OF GEN-TOPIC-TABLE.                            ELSSUBTP
00174      EJECT                                                        ELSSUBTP
00175                                                                   ELSSUBTP
00176                                                                   ELSSUBTP
00177 ************************************************************      ELSSUBTP
00178 *                                                          *      ELSSUBTP
00179 *        PROCESS EMERGENCY CARE SUBTOPIC                   *      ELSSUBTP
00180 *                                                          *      ELSSUBTP
00181 ************************************************************      ELSSUBTP
00182  PROCESS-EMERGENCY-CARE-SUBTOPI.                                  ELSSUBTP
00183      PERFORM SET-EMERGENCY-CARE-LINKAGE-ADD.                      ELSSUBTP
00184      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSSUBTP
00185          PERFORM PROCESS-BUILD-EMERGENCY-CARE-M                   ELSSUBTP
00186      ELSE IF SSB-MENU-COMPLETE(SSB-SELECTOR-STATE)                ELSSUBTP
00187          PERFORM PROCESS-MENU-COMPLETED-REQUEST                   ELSSUBTP
00188      ELSE                                                         ELSSUBTP
00189          PERFORM SIGNAL-INVALID-SUB-TOPIC-SELEC.                  ELSSUBTP
00190                                                                   ELSSUBTP
00191                                                                   ELSSUBTP
00192 ************************************************************      ELSSUBTP
00193 *                                                          *      ELSSUBTP
00194 *        PROCESS BUILD EMERGENCY CARE MENU REQUEST         *      ELSSUBTP
00195 *                                                          *      ELSSUBTP
00196 ************************************************************      ELSSUBTP
00197  PROCESS-BUILD-EMERGENCY-CARE-M.                                  ELSSUBTP
00198      MOVE SBT-ER-TOPIC-TITLE TO SSB-MNU-TITLE.                    ELSSUBTP
00199      PERFORM PROCESS-BUILD-MENU-REQUEST.                          ELSSUBTP
00200                                                                   ELSSUBTP
00201                                                                   ELSSUBTP
00202 ************************************************************      ELSSUBTP
00203 *                                                          *      ELSSUBTP
00204 *        SET EMERGENCY CARE LINKAGE ADDRESS                *      ELSSUBTP
00205 *                                                          *      ELSSUBTP
00206 ************************************************************      ELSSUBTP
00207  SET-EMERGENCY-CARE-LINKAGE-ADD.                                  ELSSUBTP
00208      CALL 'ELUADDRS' USING SBT-ER-TABLE-COUNTS,                   ELSSUBTP
00209            ADDRESS OF GEN-TABLE-COUNTS.                           ELSSUBTP
00210      CALL 'ELUADDRS' USING SBT-ER-TOPIC-HEADINGS,                 ELSSUBTP
00211            ADDRESS OF GEN-TOPIC-HEAD.                             ELSSUBTP
00212      CALL 'ELUADDRS' USING                                        ELSSUBTP
00213          SBT-ER-TOPIC-VALID-DEFINITION,                           ELSSUBTP
00214            ADDRESS OF GEN-TOPIC-TABLE.                            ELSSUBTP
00215      EJECT                                                        ELSSUBTP
00216                                                                   ELSSUBTP
00217                                                                   ELSSUBTP
00218 ************************************************************      ELSSUBTP
00219 *                                                          *      ELSSUBTP
00220 *        PROCESS PROSTHETICS ORTHOTICS SUBTOPIC            *      ELSSUBTP
00221 *                                                          *      ELSSUBTP
00222 ************************************************************      ELSSUBTP
00223  PROCESS-PROSTHETICS-ORTHOTICSX.                                  ELSSUBTP
00224      PERFORM SET-PROSTHETICS-ORTHOTICS-LINK.                      ELSSUBTP
00225      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSSUBTP
00226          PERFORM PROCESS-BUILD-PROSTHETICS-ORTH                   ELSSUBTP
00227      ELSE IF SSB-MENU-COMPLETE(SSB-SELECTOR-STATE)                ELSSUBTP
00228          PERFORM PROCESS-MENU-COMPLETED-REQUEST                   ELSSUBTP
00229      ELSE                                                         ELSSUBTP
00230          PERFORM SIGNAL-INVALID-SUB-TOPIC-SELEC.                  ELSSUBTP
00231                                                                   ELSSUBTP
00232                                                                   ELSSUBTP
00233 ************************************************************      ELSSUBTP
00234 *                                                          *      ELSSUBTP
00235 *        PROCESS BUILD PROSTHETICS ORTHOTICS MENU REQUEST  *      ELSSUBTP
00236 *                                                          *      ELSSUBTP
00237 ************************************************************      ELSSUBTP
00238  PROCESS-BUILD-PROSTHETICS-ORTH.                                  ELSSUBTP
00239      MOVE SBT-PO-TOPIC-TITLE TO SSB-MNU-TITLE.                    ELSSUBTP
00240      PERFORM PROCESS-BUILD-MENU-REQUEST.                          ELSSUBTP
00241                                                                   ELSSUBTP
00242                                                                   ELSSUBTP
00243 ************************************************************      ELSSUBTP
00244 *                                                          *      ELSSUBTP
00245 *        SET PROSTHETICS ORTHOTICS LINKAGE ADDRESS         *      ELSSUBTP
00246 *                                                          *      ELSSUBTP
00247 ************************************************************      ELSSUBTP
00248  SET-PROSTHETICS-ORTHOTICS-LINK.                                  ELSSUBTP
00249      CALL 'ELUADDRS' USING SBT-PO-TABLE-COUNTS,                   ELSSUBTP
00250            ADDRESS OF GEN-TABLE-COUNTS.                           ELSSUBTP
00251      CALL 'ELUADDRS' USING SBT-PO-TOPIC-HEADINGS,                 ELSSUBTP
00252            ADDRESS OF GEN-TOPIC-HEAD.                             ELSSUBTP
00253      CALL 'ELUADDRS' USING                                        ELSSUBTP
00254          SBT-PO-TOPIC-VALID-DEFINITION,                           ELSSUBTP
00255            ADDRESS OF GEN-TOPIC-TABLE.                            ELSSUBTP
00256      EJECT                                                        ELSSUBTP
00257                                                                   ELSSUBTP
00258                                                                   ELSSUBTP
00259 ************************************************************      ELSSUBTP
00260 *                                                          *      ELSSUBTP
00261 *        PROCESS SURGERY SUBTOPIC                          *      ELSSUBTP
00262 *                                                          *      ELSSUBTP
00263 ************************************************************      ELSSUBTP
00264  PROCESS-SURGERY-SUBTOPIC.                                        ELSSUBTP
00265      PERFORM SET-SURGERY-LINKAGE-ADDRESS.                         ELSSUBTP
00266      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSSUBTP
00267          PERFORM PROCESS-BUILD-SURGERY-MENU-REQ                   ELSSUBTP
00268      ELSE IF SSB-MENU-COMPLETE(SSB-SELECTOR-STATE)                ELSSUBTP
00269          PERFORM PROCESS-MENU-COMPLETED-REQUEST                   ELSSUBTP
00270      ELSE                                                         ELSSUBTP
00271          PERFORM SIGNAL-INVALID-SUB-TOPIC-SELEC.                  ELSSUBTP
00272                                                                   ELSSUBTP
00273                                                                   ELSSUBTP
00274 ************************************************************      ELSSUBTP
00275 *                                                          *      ELSSUBTP
00276 *        PROCESS BUILD SURGERY MENU REQUEST                *      ELSSUBTP
00277 *                                                          *      ELSSUBTP
00278 ************************************************************      ELSSUBTP
00279  PROCESS-BUILD-SURGERY-MENU-REQ.                                  ELSSUBTP
00280      MOVE SBT-SG-TOPIC-TITLE TO SSB-MNU-TITLE.                    ELSSUBTP
00281      PERFORM PROCESS-BUILD-MENU-REQUEST.                          ELSSUBTP
00282                                                                   ELSSUBTP
00283                                                                   ELSSUBTP
00284 ************************************************************      ELSSUBTP
00285 *                                                          *      ELSSUBTP
00286 *        SET SURGERY LINKAGE ADDRESS                       *      ELSSUBTP
00287 *                                                          *      ELSSUBTP
00288 ************************************************************      ELSSUBTP
00289  SET-SURGERY-LINKAGE-ADDRESS.                                     ELSSUBTP
00290      CALL 'ELUADDRS' USING SBT-SG-TABLE-COUNTS,                   ELSSUBTP
00291            ADDRESS OF GEN-TABLE-COUNTS.                           ELSSUBTP
00292      CALL 'ELUADDRS' USING SBT-SG-TOPIC-HEADINGS,                 ELSSUBTP
00293            ADDRESS OF GEN-TOPIC-HEAD.                             ELSSUBTP
00294      CALL 'ELUADDRS' USING                                        ELSSUBTP
00295          SBT-SG-TOPIC-VALID-DEFINITION,                           ELSSUBTP
00296            ADDRESS OF GEN-TOPIC-TABLE.                            ELSSUBTP
00297      EJECT                                                        ELSSUBTP
00298                                                                   ELSSUBTP
00299                                                                   ELSSUBTP
00300 ************************************************************      ELSSUBTP
00301 *                                                          *      ELSSUBTP
00302 *        PROCESS THERAPIES SUBTOPIC                        *      ELSSUBTP
00303 *                                                          *      ELSSUBTP
00304 ************************************************************      ELSSUBTP
00305  PROCESS-THERAPIES-SUBTOPIC.                                      ELSSUBTP
00306      PERFORM SET-THERAPIES-LINKAGE-ADDRESS.                       ELSSUBTP
00307      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSSUBTP
00308          PERFORM PROCESS-BUILD-THERAPIES-MENU-R                   ELSSUBTP
00309      ELSE IF SSB-MENU-COMPLETE(SSB-SELECTOR-STATE)                ELSSUBTP
00310          PERFORM PROCESS-MENU-COMPLETED-REQUEST                   ELSSUBTP
00311      ELSE                                                         ELSSUBTP
00312          PERFORM SIGNAL-INVALID-SUB-TOPIC-SELEC.                  ELSSUBTP
00313                                                                   ELSSUBTP
00314                                                                   ELSSUBTP
00315 ************************************************************      ELSSUBTP
00316 *                                                          *      ELSSUBTP
00317 *        PROCESS BUILD THERAPIES MENU REQUEST              *      ELSSUBTP
00318 *                                                          *      ELSSUBTP
00319 ************************************************************      ELSSUBTP
00320  PROCESS-BUILD-THERAPIES-MENU-R.                                  ELSSUBTP
00321      MOVE SBT-TR-TOPIC-TITLE TO SSB-MNU-TITLE.                    ELSSUBTP
00322      PERFORM PROCESS-BUILD-MENU-REQUEST.                          ELSSUBTP
00323                                                                   ELSSUBTP
00324                                                                   ELSSUBTP
00325 ************************************************************      ELSSUBTP
00326 *                                                          *      ELSSUBTP
00327 *        SET THERAPIES LINKAGE ADDRESS                     *      ELSSUBTP
00328 *                                                          *      ELSSUBTP
00329 ************************************************************      ELSSUBTP
00330  SET-THERAPIES-LINKAGE-ADDRESS.                                   ELSSUBTP
00331      CALL 'ELUADDRS' USING SBT-TR-TABLE-COUNTS,                   ELSSUBTP
00332            ADDRESS OF GEN-TABLE-COUNTS.                           ELSSUBTP
00333      CALL 'ELUADDRS' USING SBT-TR-TOPIC-HEADINGS,                 ELSSUBTP
00334            ADDRESS OF GEN-TOPIC-HEAD.                             ELSSUBTP
00335      CALL 'ELUADDRS' USING                                        ELSSUBTP
00336          SBT-TR-TOPIC-VALID-DEFINITION,                           ELSSUBTP
00337            ADDRESS OF GEN-TOPIC-TABLE.                            ELSSUBTP
00338      EJECT                                                        ELSSUBTP
00339                                                                   ELSSUBTP
00340                                                                   ELSSUBTP
00341 ************************************************************      ELSSUBTP
00342 *                                                          *      ELSSUBTP
00343 *        PROCESS GENERAL ADMINISTRATION RULES SUBTOPIC     *      ELSSUBTP
00344 *                                                          *      ELSSUBTP
00345 ************************************************************      ELSSUBTP
00346  PROCESS-GENERAL-ADMINISTRATION.                                  ELSSUBTP
00347      PERFORM SET-GENERAL-ADMINISTRATION-RUL.                      ELSSUBTP
00348      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSSUBTP
00349          PERFORM PROCESS-BUILD-GENERAL-ADMINIST                   ELSSUBTP
00350      ELSE IF SSB-MENU-COMPLETE(SSB-SELECTOR-STATE)                ELSSUBTP
00351          PERFORM PROCESS-MENU-COMPLETED-REQUEST                   ELSSUBTP
00352      ELSE                                                         ELSSUBTP
00353          PERFORM SIGNAL-INVALID-SUB-TOPIC-SELEC.                  ELSSUBTP
00354                                                                   ELSSUBTP
00355                                                                   ELSSUBTP
00356 ************************************************************      ELSSUBTP
00357 *                                                          *      ELSSUBTP
00358 *        PROCESS BUILD GENERAL ADMINISTRATION RULES MENU RE*      ELSSUBTP
00359 *                                                          *      ELSSUBTP
00360 ************************************************************      ELSSUBTP
00361  PROCESS-BUILD-GENERAL-ADMINIST.                                  ELSSUBTP
00362      MOVE SBT-GENRL-TOPIC-TITLE TO SSB-MNU-TITLE.                 ELSSUBTP
00363      PERFORM PROCESS-BUILD-MENU-REQUEST.                          ELSSUBTP
00364                                                                   ELSSUBTP
00365                                                                   ELSSUBTP
00366 ************************************************************      ELSSUBTP
00367 *                                                          *      ELSSUBTP
00368 *        SET GENERAL ADMINISTRATION RULES LINKAGE ADDRESS  *      ELSSUBTP
00369 *                                                          *      ELSSUBTP
00370 ************************************************************      ELSSUBTP
00371  SET-GENERAL-ADMINISTRATION-RUL.                                  ELSSUBTP
00372      CALL 'ELUADDRS' USING SBT-GENRL-TABLE-COUNTS,                ELSSUBTP
00373            ADDRESS OF GEN-TABLE-COUNTS.                           ELSSUBTP
00374      CALL 'ELUADDRS' USING SBT-GENRL-TOPIC-HEADINGS,              ELSSUBTP
00375            ADDRESS OF GEN-TOPIC-HEAD.                             ELSSUBTP
00376      CALL 'ELUADDRS' USING                                        ELSSUBTP
00377          SBT-GENRL-TOP-VALID-DEFINITION,                          ELSSUBTP
00378            ADDRESS OF GEN-TOPIC-TABLE.                            ELSSUBTP
00379      EJECT                                                        ELSSUBTP
00380                                                                   ELSSUBTP
00381                                                                   ELSSUBTP
00382 ************************************************************      ELSSUBTP
00383 *                                                          *      ELSSUBTP
00384 *        PROCESS OBSTETRICAL SERVICES SUBTOPICS            *      ELSSUBTP
00385 *                                                          *      ELSSUBTP
00386 ************************************************************      ELSSUBTP
00387  PROCESS-OB-SERVICES-SUBTOPICS.                                   ELSSUBTP
00388      PERFORM SET-OB-SERVICES-RUL.                                 ELSSUBTP
00389      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSSUBTP
00390          PERFORM PROCESS-BUILD-OB-SUBTOPICS                       ELSSUBTP
00391      ELSE IF SSB-MENU-COMPLETE(SSB-SELECTOR-STATE)                ELSSUBTP
00392          PERFORM PROCESS-MENU-COMPLETED-REQUEST                   ELSSUBTP
00393      ELSE                                                         ELSSUBTP
00394          PERFORM SIGNAL-INVALID-SUB-TOPIC-SELEC.                  ELSSUBTP
00395                                                                   ELSSUBTP
00396                                                                   ELSSUBTP
00397 ************************************************************      ELSSUBTP
00398 *                                                          *      ELSSUBTP
00399 *        PROCESS BUILD OB SUBTOPICS RULES                  *      ELSSUBTP
00400 *                                                          *      ELSSUBTP
00401 ************************************************************      ELSSUBTP
00402  PROCESS-BUILD-OB-SUBTOPICS.                                      ELSSUBTP
00403      MOVE SBT-OB-TOPIC-TITLE TO SSB-MNU-TITLE.                    ELSSUBTP
00404      PERFORM PROCESS-BUILD-MENU-REQUEST.                          ELSSUBTP
00405                                                                   ELSSUBTP
00406                                                                   ELSSUBTP
00407 ************************************************************      ELSSUBTP
00408 *                                                          *      ELSSUBTP
00409 *        SET OBSTETRICAL SERVICES RULES LINKAGE ADDRESS    *      ELSSUBTP
00410 *                                                          *      ELSSUBTP
00411 ************************************************************      ELSSUBTP
00412  SET-OB-SERVICES-RUL.                                             ELSSUBTP
00413      CALL 'ELUADDRS' USING SBT-OB-TABLE-COUNTS,                   ELSSUBTP
00414            ADDRESS OF GEN-TABLE-COUNTS.                           ELSSUBTP
00415      CALL 'ELUADDRS' USING SBT-OB-TOPIC-HEADINGS,                 ELSSUBTP
00416            ADDRESS OF GEN-TOPIC-HEAD.                             ELSSUBTP
00417      CALL 'ELUADDRS' USING                                        ELSSUBTP
00418          SBT-OB-TOP-VALID-DEFINITION,                             ELSSUBTP
00419            ADDRESS OF GEN-TOPIC-TABLE.                            ELSSUBTP
00420      EJECT                                                        ELSSUBTP
00421                                                                   ELSSUBTP
00422                                                                   ELSSUBTP
00423 ************************************************************      ELSSUBTP
00424 *                                                          *      ELSSUBTP
00425 *        PROCESS BUILD MENU REQUEST                        *      ELSSUBTP
00426 *                                                          *      ELSSUBTP
00427 ************************************************************      ELSSUBTP
00428  PROCESS-BUILD-MENU-REQUEST.                                      ELSSUBTP
00429      INITIALIZE SSB-MNU-CHOICE (1).                               ELSSUBTP
00430      PERFORM DELETE-MENU-FILE.                                    ELSSUBTP
00431      PERFORM ACQUIRE-STORAGE-AREAS.                               ELSSUBTP
00432      PERFORM BUILD-MENU-HEADERS.                                  ELSSUBTP
00433      PERFORM BUILD-TOPIC-TITLES.                                  ELSSUBTP
00434      PERFORM BUILD-VALID-TOPIC-SELECTIONS.                        ELSSUBTP
00435      SET SSB-START-MENU (SSB-SELECTOR-STATE) TO TRUE.             ELSSUBTP
00436      EJECT                                                        ELSSUBTP
00437                                                                   ELSSUBTP
00438                                                                   ELSSUBTP
00439 ************************************************************      ELSSUBTP
00440 *                                                          *      ELSSUBTP
00441 *        DELETE MENU FILE                                  *      ELSSUBTP
00442 *                                                          *      ELSSUBTP
00443 ************************************************************      ELSSUBTP
00444  DELETE-MENU-FILE.                                                ELSSUBTP
00445      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSSUBTP
00446      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSUBTP
00447                            WS-DUMMY-PTR.                          ELSSUBTP
00448      IF CIA-RC-PTR-NULL                                           ELSSUBTP
00449          PERFORM ALLOCATE-MENU-AREA.                              ELSSUBTP
00450      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSSUBTP
00451      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSUBTP
00452          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSSUBTP
00453                                                                   ELSSUBTP
00454      SET CIA-ELSMENU-DDN  TO TRUE.                                ELSSUBTP
00455      SET IOP-DEL          TO TRUE.                                ELSSUBTP
00456      SET IOP-FCQ-NONE     TO TRUE.                                ELSSUBTP
00457      SET IOP-KVQ-NONE     TO TRUE.                                ELSSUBTP
00458      PERFORM CALL-INPUT-OUTPUT-SUBPROGRAM.                        ELSSUBTP
00459                                                                   ELSSUBTP
00460                                                                   ELSSUBTP
00461 ************************************************************      ELSSUBTP
00462 *                                                          *      ELSSUBTP
00463 *        ACQUIRE STORAGE AREAS                             *      ELSSUBTP
00464 *                                                          *      ELSSUBTP
00465 ************************************************************      ELSSUBTP
00466  ACQUIRE-STORAGE-AREAS.                                           ELSSUBTP
00467      PERFORM GET-HEADING-STORAGE-AREA.                            ELSSUBTP
00468      PERFORM GET-SELECTION-CODE-KEYWORD-ARE.                      ELSSUBTP
00469      PERFORM GET-DESCRIPTION-LINE-AREA.                           ELSSUBTP
00470      EJECT                                                        ELSSUBTP
00471                                                                   ELSSUBTP
00472                                                                   ELSSUBTP
00473 ************************************************************      ELSSUBTP
00474 *                                                          *      ELSSUBTP
00475 *        GET HEADING STORAGE AREA                          *      ELSSUBTP
00476 *                                                          *      ELSSUBTP
00477 ************************************************************      ELSSUBTP
00478  GET-HEADING-STORAGE-AREA.                                        ELSSUBTP
00479      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSSUBTP
00480      COMPUTE CIA-AREA-LEN = LENGTH OF MHD-NBR-HDG-LINES +         ELSSUBTP
00481               (LENGTH OF MHD-HDG-LINE *                           ELSSUBTP
00482          GEN-NUMBER-HEADINGS).                                    ELSSUBTP
00483      SET CIA-STG-GETMAIN TO TRUE.                                 ELSSUBTP
00484      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSSUBTP
00485      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSSUBTP
00486      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSUBTP
00487          ADDRESS OF MHD-MENU-HEADINGS.                            ELSSUBTP
00488      EJECT                                                        ELSSUBTP
00489                                                                   ELSSUBTP
00490                                                                   ELSSUBTP
00491 ************************************************************      ELSSUBTP
00492 *                                                          *      ELSSUBTP
00493 *        GET SELECTION CODE KEYWORD AREA                   *      ELSSUBTP
00494 *                                                          *      ELSSUBTP
00495 ************************************************************      ELSSUBTP
00496  GET-SELECTION-CODE-KEYWORD-ARE.                                  ELSSUBTP
00497      SET  CIA-ELSMOPT-DDN TO TRUE.                                ELSSUBTP
00498      COMPUTE CIA-AREA-LEN = LENGTH OF MSO-MENU-OPTS-HEADER +      ELSSUBTP
00499              (LENGTH OF MSO-MENU-OPT *                            ELSSUBTP
00500          GEN-NUMBER-CODES).                                       ELSSUBTP
00501      SET CIA-STG-GETMAIN TO TRUE.                                 ELSSUBTP
00502      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSSUBTP
00503      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSSUBTP
00504      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSUBTP
00505          ADDRESS OF MSO-MENU-SELECTION-VALUES.                    ELSSUBTP
00506      EJECT                                                        ELSSUBTP
00507                                                                   ELSSUBTP
00508                                                                   ELSSUBTP
00509 ************************************************************      ELSSUBTP
00510 *                                                          *      ELSSUBTP
00511 *        GET DESCRIPTION LINE AREA                         *      ELSSUBTP
00512 *                                                          *      ELSSUBTP
00513 ************************************************************      ELSSUBTP
00514  GET-DESCRIPTION-LINE-AREA.                                       ELSSUBTP
00515      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSSUBTP
00516      SET CIA-STG-GETMAIN TO TRUE.                                 ELSSUBTP
00517      SET IOP-GETMAIN-REC TO TRUE.                                 ELSSUBTP
00518      COMPUTE IOP-REC-LEN = LENGTH OF MSD-NBR-DESCR-LINES +        ELSSUBTP
00519                (LENGTH OF MSD-DESCR-LINE *                        ELSSUBTP
00520          GEN-MENU-LINE-COUNT).                                    ELSSUBTP
00521      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSSUBTP
00522      SET ADDRESS OF MSD-MENU-ITEM-DESCRIPTIONS TO IOP-REC-PTR.    ELSSUBTP
00523                                                                   ELSSUBTP
00524                                                                   ELSSUBTP
00525 ************************************************************      ELSSUBTP
00526 *                                                          *      ELSSUBTP
00527 *        ALLOCATE MENU AREA                                *      ELSSUBTP
00528 *                                                          *      ELSSUBTP
00529 ************************************************************      ELSSUBTP
00530  ALLOCATE-MENU-AREA.                                              ELSSUBTP
00531      SET CIA-ELSMENU-DDN  TO TRUE.                                ELSSUBTP
00532      SET CIA-STG-GETMAIN  TO TRUE.                                ELSSUBTP
00533      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSSUBTP
00534                                                                   ELSSUBTP
00535                                                                   ELSSUBTP
00536 ************************************************************      ELSSUBTP
00537 *                                                          *      ELSSUBTP
00538 *        CALL STORAGE SUBPROGRAM                           *      ELSSUBTP
00539 *                                                          *      ELSSUBTP
00540 ************************************************************      ELSSUBTP
00541  CALL-STORAGE-SUBPROGRAM.                                         ELSSUBTP
00542      EXEC CICS LINK PROGRAM ('ELUSTGMG')                          ELSSUBTP
00543                     COMMAREA (DFHCOMMAREA)                        ELSSUBTP
00544                     END-EXEC.                                     ELSSUBTP
00545      EJECT                                                        ELSSUBTP
00546                                                                   ELSSUBTP
00547                                                                   ELSSUBTP
00548 ************************************************************      ELSSUBTP
00549 *                                                          *      ELSSUBTP
00550 *        BUILD MENU HEADERS                                *      ELSSUBTP
00551 *                                                          *      ELSSUBTP
00552 ************************************************************      ELSSUBTP
00553  BUILD-MENU-HEADERS.                                              ELSSUBTP
00554      MOVE GEN-NUMBER-HEADINGS TO MHD-NBR-HDG-LINES.               ELSSUBTP
00555      SET MHD-IDX TO 1.                                            ELSSUBTP
00556      PERFORM LOAD-MENU-HEADINGS                                   ELSSUBTP
00557          VARYING GEN-HEAD-INDEX FROM 1 BY 1 UNTIL                 ELSSUBTP
00558                     GEN-HEAD-INDEX > GEN-NUMBER-HEADINGS.         ELSSUBTP
00559                                                                   ELSSUBTP
00560                                                                   ELSSUBTP
00561 ************************************************************      ELSSUBTP
00562 *                                                          *      ELSSUBTP
00563 *        LOAD MENU HEADINGS                                *      ELSSUBTP
00564 *                                                          *      ELSSUBTP
00565 ************************************************************      ELSSUBTP
00566  LOAD-MENU-HEADINGS.                                              ELSSUBTP
00567      MOVE GEN-TOPIC-HEADING-LINE (GEN-HEAD-INDEX)                 ELSSUBTP
00568                              TO MHD-HDG-LINE (MHD-IDX).           ELSSUBTP
00569      SET MHD-IDX UP BY 1.                                         ELSSUBTP
00570      EJECT                                                        ELSSUBTP
00571                                                                   ELSSUBTP
00572                                                                   ELSSUBTP
00573 ************************************************************      ELSSUBTP
00574 *                                                          *      ELSSUBTP
00575 *        BUILD TOPIC TITLES                                *      ELSSUBTP
00576 *                                                          *      ELSSUBTP
00577 ************************************************************      ELSSUBTP
00578  BUILD-TOPIC-TITLES.                                              ELSSUBTP
00579      SET MSD-IDX TO 1.                                            ELSSUBTP
00580      MOVE GEN-MENU-LINE-COUNT            TO                       ELSSUBTP
00581          MSD-NBR-DESCR-LINES.                                     ELSSUBTP
00582      SET IOP-ADD       TO TRUE.                                   ELSSUBTP
00583      SET IOP-FCQ-NONE  TO TRUE.                                   ELSSUBTP
00584      SET IOP-KVQ-NONE  TO TRUE.                                   ELSSUBTP
00585      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSSUBTP
00586      PERFORM LOAD-TOPIC-NAMES                                     ELSSUBTP
00587          VARYING GEN-TOPIC-INDEX FROM 1 BY 1                      ELSSUBTP
00588                     UNTIL GEN-TOPIC-INDEX IS GREATER THAN         ELSSUBTP
00589              GEN-NUMBER-CODES.                                    ELSSUBTP
00590                                                                   ELSSUBTP
00591                                                                   ELSSUBTP
00592 ************************************************************      ELSSUBTP
00593 *                                                          *      ELSSUBTP
00594 *        LOAD TOPIC NAMES                                  *      ELSSUBTP
00595 *                                                          *      ELSSUBTP
00596 ************************************************************      ELSSUBTP
00597  LOAD-TOPIC-NAMES.                                                ELSSUBTP
00598      MOVE GEN-TOPIC-NAME (GEN-TOPIC-INDEX) TO MSD-DESCR-LINE      ELSSUBTP
00599          (MSD-IDX).                                               ELSSUBTP
00600      MOVE LENGTH OF MSD-MENU-ITEM-DESCRIPTIONS TO IOP-REC-LEN.    ELSSUBTP
00601      PERFORM CALL-INPUT-OUTPUT-SUBPROGRAM.                        ELSSUBTP
00602      EJECT                                                        ELSSUBTP
00603                                                                   ELSSUBTP
00604                                                                   ELSSUBTP
00605 ************************************************************      ELSSUBTP
00606 *                                                          *      ELSSUBTP
00607 *        BUILD VALID TOPIC SELECTIONS                      *      ELSSUBTP
00608 *                                                          *      ELSSUBTP
00609 ************************************************************      ELSSUBTP
00610  BUILD-VALID-TOPIC-SELECTIONS.                                    ELSSUBTP
00611      PERFORM LOAD-MENU-OPTS-HEADER.                               ELSSUBTP
00612      SET MSO-IDX TO 1.                                            ELSSUBTP
00613      PERFORM LOAD-TOPIC-CODES-KEYWORD                             ELSSUBTP
00614          VARYING GEN-TOPIC-INDEX FROM 1 BY 1                      ELSSUBTP
00615                  UNTIL GEN-TOPIC-INDEX IS GREATER THAN            ELSSUBTP
00616              GEN-NUMBER-CODES.                                    ELSSUBTP
00617                                                                   ELSSUBTP
00618                                                                   ELSSUBTP
00619 ************************************************************      ELSSUBTP
00620 *                                                          *      ELSSUBTP
00621 *        LOAD MENU OPTS HEADER                             *      ELSSUBTP
00622 *                                                          *      ELSSUBTP
00623 ************************************************************      ELSSUBTP
00624  LOAD-MENU-OPTS-HEADER.                                           ELSSUBTP
00625      MOVE GEN-NUMBER-CODES TO MSO-NBR-MENU-OPTS.                  ELSSUBTP
00626      MOVE 1   TO MSO-MIN-CHOICES                                  ELSSUBTP
00627                  MSO-MAX-CHOICES.                                 ELSSUBTP
00628      MOVE LENGTH OF GEN-VALID-SELECT TO MSO-OPT-LEN.              ELSSUBTP
00629      SET MSO-OPT-TYP-NUM  TO TRUE.                                ELSSUBTP
00630                                                                   ELSSUBTP
00631                                                                   ELSSUBTP
00632 ************************************************************      ELSSUBTP
00633 *                                                          *      ELSSUBTP
00634 *        LOAD TOPIC CODES KEYWORD                          *      ELSSUBTP
00635 *                                                          *      ELSSUBTP
00636 ************************************************************      ELSSUBTP
00637  LOAD-TOPIC-CODES-KEYWORD.                                        ELSSUBTP
00638      MOVE GEN-VALID-SELECT(GEN-TOPIC-INDEX) TO                    ELSSUBTP
00639           MSO-OPT-SEL(MSO-IDX).                                   ELSSUBTP
00640      MOVE GEN-KEYWORD(GEN-TOPIC-INDEX) TO                         ELSSUBTP
00641           MSO-OPT-KWD(MSO-IDX).                                   ELSSUBTP
00642      SET MSO-IDX UP BY 1.                                         ELSSUBTP
00643      EJECT                                                        ELSSUBTP
00644                                                                   ELSSUBTP
00645                                                                   ELSSUBTP
00646 ************************************************************      ELSSUBTP
00647 *                                                          *      ELSSUBTP
00648 *        PROCESS MENU COMPLETED REQUEST                    *      ELSSUBTP
00649 *                                                          *      ELSSUBTP
00650 ************************************************************      ELSSUBTP
00651  PROCESS-MENU-COMPLETED-REQUEST.                                  ELSSUBTP
00652      MOVE SSB-MNU-CHOICE (1) TO SSB-SUB-TOPIC.                    ELSSUBTP
00653      PERFORM MOVE-TOPIC-TITLE-TO-SSB-TOPICX.                      ELSSUBTP
00654      SET SSB-COMPLETED (SSB-SELECTOR-STATE)     TO                ELSSUBTP
00655          TRUE.                                                    ELSSUBTP
00656                                                                   ELSSUBTP
00657                                                                   ELSSUBTP
00658 ************************************************************      ELSSUBTP
00659 *                                                          *      ELSSUBTP
00660 *        MOVE TOPIC TITLE TO SSB-TOPIC-PHRASE              *      ELSSUBTP
00661 *                                                          *      ELSSUBTP
00662 ************************************************************      ELSSUBTP
00663  MOVE-TOPIC-TITLE-TO-SSB-TOPICX.                                  ELSSUBTP
00664      SET GEN-TOPIC-INDEX TO 1.                                    ELSSUBTP
00665      SEARCH GEN-TOPIC-INFO                                        ELSSUBTP
00666                VARYING GEN-TOPIC-INDEX                            ELSSUBTP
00667                  AT END                                           ELSSUBTP
00668                    SET CIA-AB-PGM-LOGIC TO TRUE                   ELSSUBTP
00669                    EXEC CICS ABEND                                ELSSUBTP
00670                              ABCODE(CIA-ABCODE)                   ELSSUBTP
00671                    END-EXEC                                       ELSSUBTP
00672         WHEN                                                      ELSSUBTP
00673          SSB-MNU-CHOICE (1) =                                     ELSSUBTP
00674          GEN-KEYWORD(GEN-TOPIC-INDEX)                             ELSSUBTP
00675               MOVE GEN-TOPIC-NAME(GEN-TOPIC-INDEX)                ELSSUBTP
00676                     TO WS-HOLD-SUBTOPIC-NAME                      ELSSUBTP
00677               MOVE WS-HOLD-SUBTOPIC-NAME-ONLY                     ELSSUBTP
00678                     TO SSB-SUB-TOPIC-PHRASE.                      ELSSUBTP
00679                                                                   ELSSUBTP
00680                                                                   ELSSUBTP
00681 ************************************************************      ELSSUBTP
00682 *                                                          *      ELSSUBTP
00683 *        CHECK COMMAREA LENGTH                             *      ELSSUBTP
00684 *                                                          *      ELSSUBTP
00685 ************************************************************      ELSSUBTP
00686  CHECK-COMMAREA-LENGTH.                                           ELSSUBTP
00687      IF EIBCALEN IS NOT EQUAL TO LENGTH OF DFHCOMMAREA            ELSSUBTP
00688          PERFORM SIGNAL-COMMAREA-LENGTH-ERROR.                    ELSSUBTP
00689                                                                   ELSSUBTP
00690                                                                   ELSSUBTP
00691 ************************************************************      ELSSUBTP
00692 *                                                          *      ELSSUBTP
00693 *        SIGNAL COMMAREA LENGTH ERROR                      *      ELSSUBTP
00694 *                                                          *      ELSSUBTP
00695 ************************************************************      ELSSUBTP
00696  SIGNAL-COMMAREA-LENGTH-ERROR.                                    ELSSUBTP
00697      SET CIA-AB-DFHCOMMAREA                                       ELSSUBTP
00698        TO TRUE.                                                   ELSSUBTP
00699      EXEC CICS ABEND                                              ELSSUBTP
00700                ABCODE(CIA-ABCODE)                                 ELSSUBTP
00701                END-EXEC.                                          ELSSUBTP
00702                                                                   ELSSUBTP
00703                                                                   ELSSUBTP
00704 ************************************************************      ELSSUBTP
00705 *                                                          *      ELSSUBTP
00706 *        CALL INPUT-OUTPUT SUBPROGRAM                      *      ELSSUBTP
00707 *                                                          *      ELSSUBTP
00708 ************************************************************      ELSSUBTP
00709  CALL-INPUT-OUTPUT-SUBPROGRAM.                                    ELSSUBTP
00710      EXEC CICS LINK PROGRAM ('ELUIOPGM')                          ELSSUBTP
00711                     COMMAREA(DFHCOMMAREA)                         ELSSUBTP
00712                     END-EXEC.                                     ELSSUBTP
00713      EJECT                                                        ELSSUBTP
00714                                                                   ELSSUBTP
00715                                                                   ELSSUBTP
00716 ************************************************************      ELSSUBTP
00717 *                                                          *      ELSSUBTP
00718 *        ESTABLISH ADDRESSING TO COMMON INTERFACE AREA     *      ELSSUBTP
00719 *                                                          *      ELSSUBTP
00720 ************************************************************      ELSSUBTP
00721  ESTABLISH-ADDRESSING-TO-COMMON.                                  ELSSUBTP
00722      IF ECA-CIA-PTR IS NOT EQUAL NULL                             ELSSUBTP
00723          PERFORM SET-CIA-ADDRESS                                  ELSSUBTP
00724      ELSE                                                         ELSSUBTP
00725          PERFORM SIGNAL-CIA-ADDRESSING-ERROR.                     ELSSUBTP
00726                                                                   ELSSUBTP
00727                                                                   ELSSUBTP
00728 ************************************************************      ELSSUBTP
00729 *                                                          *      ELSSUBTP
00730 *        SET CIA ADDRESS                                   *      ELSSUBTP
00731 *                                                          *      ELSSUBTP
00732 ************************************************************      ELSSUBTP
00733  SET-CIA-ADDRESS.                                                 ELSSUBTP
00734      CALL 'ELUINISM' USING DFHCOMMAREA                            ELSSUBTP
00735          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELSSUBTP
00736                                                                   ELSSUBTP
00737                                                                   ELSSUBTP
00738 ************************************************************      ELSSUBTP
00739 *                                                          *      ELSSUBTP
00740 *        SIGNAL CIA ADDRESSING ERROR                       *      ELSSUBTP
00741 *                                                          *      ELSSUBTP
00742 ************************************************************      ELSSUBTP
00743  SIGNAL-CIA-ADDRESSING-ERROR.                                     ELSSUBTP
00744      SET  CIA-AB-ELSCIA-PTR                                       ELSSUBTP
00745         TO TRUE.                                                  ELSSUBTP
00746      EXEC CICS ABEND                                              ELSSUBTP
00747                ABCODE(CIA-ABCODE)                                 ELSSUBTP
00748                END-EXEC.                                          ELSSUBTP
00749      EJECT                                                        ELSSUBTP
00750                                                                   ELSSUBTP
00751                                                                   ELSSUBTP
00752 ************************************************************      ELSSUBTP
00753 *                                                          *      ELSSUBTP
00754 *        ESTABLISH ADDRESSING TO SELECTOR CONTROL AREA     *      ELSSUBTP
00755 *                                                          *      ELSSUBTP
00756 ************************************************************      ELSSUBTP
00757  ESTABLISH-ADDRESSING-TO-SELECT.                                  ELSSUBTP
00758      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSSUBTP
00759      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSUBTP
00760                            WS-DUMMY-PTR.                          ELSSUBTP
00761      IF NOT CIA-RC-PTR-NULL                                       ELSSUBTP
00762          PERFORM SET-SSCB-ADDRESS                                 ELSSUBTP
00763      ELSE                                                         ELSSUBTP
00764          PERFORM SIGNAL-MISSING-PARAMETER.                        ELSSUBTP
00765                                                                   ELSSUBTP
00766                                                                   ELSSUBTP
00767 ************************************************************      ELSSUBTP
00768 *                                                          *      ELSSUBTP
00769 *        SET SSCB ADDRESS                                  *      ELSSUBTP
00770 *                                                          *      ELSSUBTP
00771 ************************************************************      ELSSUBTP
00772  SET-SSCB-ADDRESS.                                                ELSSUBTP
00773      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSSUBTP
00774      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSUBTP
00775          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELSSUBTP
00776                                                                   ELSSUBTP
00777                                                                   ELSSUBTP
00778 ************************************************************      ELSSUBTP
00779 *                                                          *      ELSSUBTP
00780 *        SIGNAL MISSING PARAMETER                          *      ELSSUBTP
00781 *                                                          *      ELSSUBTP
00782 ************************************************************      ELSSUBTP
00783  SIGNAL-MISSING-PARAMETER.                                        ELSSUBTP
00784      SET CIA-AB-PARM-MISSING TO TRUE.                             ELSSUBTP
00785      EXEC CICS ABEND ABCODE(CIA-ABCODE)                           ELSSUBTP
00786                END-EXEC.                                          ELSSUBTP
00787      EJECT                                                        ELSSUBTP
00788                                                                   ELSSUBTP
00789                                                                   ELSSUBTP
00790 ************************************************************      ELSSUBTP
00791 *                                                          *      ELSSUBTP
00792 *        SIGNAL INVALID SUB-TOPIC SELECTOR REQUEST         *      ELSSUBTP
00793 *                                                          *      ELSSUBTP
00794 ************************************************************      ELSSUBTP
00795  SIGNAL-INVALID-SUB-TOPIC-SELEC.                                  ELSSUBTP
00796      SET CIA-AB-UNDEF TO TRUE.                                    ELSSUBTP
00797      EXEC CICS ABEND                                              ELSSUBTP
00798                ABCODE(CIA-ABCODE)                                 ELSSUBTP
00799                END-EXEC.                                          ELSSUBTP
