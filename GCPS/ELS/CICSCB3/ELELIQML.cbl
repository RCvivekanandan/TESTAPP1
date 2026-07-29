00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELELIQML
00003  PROGRAM-ID.         ELELIQML.                                       LV002
00004                                                                   ELELIQML
00005  AUTHOR.             EDWARD G. LISS                               ELELIQML
00006                                                                   ELELIQML
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELELIQML
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELELIQML
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELELIQML
00010                      233 N. MICHIGAN AVE                          ELELIQML
00011                      CHICAGO, ILLINOIS 60601                      ELELIQML
00012                                                                   ELELIQML
00013  DATE-WRITTEN.       28-OCT-1986.                                 ELELIQML
00014                                                                   ELELIQML
00015  DATE-COMPILED.                                                   ELELIQML
00016                                                                   ELELIQML
00017  SECURITY.           COPYRIGHT 1986,                              ELELIQML
00018                      HEALTH CARE SERVICE CORPORATION              ELELIQML
00019      SKIP3                                                        ELELIQML
00020  TITLE 'ELS - ELIQ MAINLINE PROGRAM                       '.      ELELIQML
00021  ENVIRONMENT DIVISION.                                            ELELIQML
00022                                                                   ELELIQML
00023  CONFIGURATION SECTION.                                           ELELIQML
00024  SOURCE-COMPUTER.    IBM-3033.                                    ELELIQML
00025  OBJECT-COMPUTER.    IBM-3033.                                    ELELIQML
00026      EJECT                                                        ELELIQML
00027 ******************************************************************ELELIQML
00028 *                                                                *ELELIQML
00029 *    PROGRAM:    ELELIQML                                        *ELELIQML
00030 *    DATE:       28-OCT-1986                                     *ELELIQML
00031 *    AUTHOR:     EDWARD G LISS                                   *ELELIQML
00032 *    FUNCTION:                                                   *ELELIQML
00033 *      THIS MODULE CONTROLS THE OVERALL OPERATION OF THE         *ELELIQML
00034 *      ENGLISH LANGUAGE INQUIRY.                                 *ELELIQML
00035 *                                                                *ELELIQML
00036 *      USING TABLES, THIS MODULE CALL THE SELECTOR MODULES       *ELELIQML
00037 *      TO DECIDE WHAT DATA IS NEEDED.  BASED ON THE RESULTS      *ELELIQML
00038 *      OF THESE SELECTOR CALLS, THE MAINLINE WILL CALL THE       *ELELIQML
00039 *      APPROPRIATE TOPIC MODULES.  UPON COMPLETION OF OUTPUT     *ELELIQML
00040 *      GENERATION, THE DISPLAY PROCESSOR WILL BE CALLED.         *ELELIQML
00041 *                                                                *ELELIQML
00042 *      THE MAINLINE ALSO CHECK THAT THE PFKS ARE USED            *ELELIQML
00043 *      CORRECTLY.  IF A BAD PFK IS ENTERED, NO ACTION            *ELELIQML
00044 *      WILL TAKE PLACE.                                          *ELELIQML
00045 *                                                                *ELELIQML
00046 *    NOTES:      NONE                                            *ELELIQML
00047 *                                                                *ELELIQML
00048 ******************************************************************ELELIQML
00049 *                                                                *ELELIQML
00050 *                      MAINTENANCE HISTORY                       *ELELIQML
00051 *                                                                *ELELIQML
00052 * MOD      DATE     BY  DRPT                ACTION               *ELELIQML
00053 * ----- ----------- --- ----- ---------------------------------- *ELELIQML
00054 * 01.00 28-OCT-1986 EGL       CREATED                            *ELELIQML
00055 * 01.01 24-NOV-1987 EGL       SHIFTED PF3 TO PF9 AND REASSIGNED  *ELELIQML
00056 *                             PF3 TO MENU BACKOUT                *ELELIQML
00057 * 01.02 21-JUN-1988 EGL       CHANGED TO SUPPORT NEW STORAGE     *ELELIQML
00058 *                             MANAGEMENT SCHEME                  *ELELIQML
00059 * 01.03 19-MAR-1990 AKK       DESTRUCT PROGRAM.  REMOVE PF12     *ELELIQML
00060 *                             SUPPORT.                           *ELELIQML
00061 *                                                                *ELELIQML
00062 * 01.04 08-JUL-1995 AKK       REENABLED ABEND HANDLING THAT WAS  *ELELIQML
00063 *                             DISABLED LAST JULY.                *ELELIQML
00064 *                                                                *ELELIQML
00065 * 01.05 28-MAR-1997 AKK       ADD 96 BYTES TO THE COMMAREA TO TRY*ELELIQML
00066 *                             AND AVOID STORAGE VIOLATIONS.      *ELELIQML
00067 *                                                                *ELELIQML
00068 * 01.06 02-OCT-1997 AKK       ADD SUPPORT FOR YR2000 AND TEXAS   *ELELIQML
00069 *                             MERGER.                            *ELELIQML
00070 *       12-AUG-2003 AKK       GEN IN QWIKEDIT TO TESTCOMPILE ORDERELELIQML
00070 *       05-DEC-2017 SRI       RECOMPILED FOR CHANGES IN ELUKYSEL *ELELIQML
00071 ******************************************************************ELELIQML
00072      EJECT                                                        ELELIQML
00073  DATA DIVISION.                                                   ELELIQML
00074  WORKING-STORAGE SECTION.                                         ELELIQML
00075  01  FILLER                     PICTURE X(32)                     ELELIQML
00076           VALUE '******* WS STARTS HERE *******'.                 ELELIQML
00077                                                                   ELELIQML
00078  01  WS-MODE-STATUS-SW          PICTURE X VALUE '0'.              ELELIQML
00079      88  SELECTION-IN-PROGRESS            VALUE '0'.              ELELIQML
00080      88  SELECTION-COMPLETED              VALUE '1'.              ELELIQML
00081      88  DISPLAY-MODE                     VALUE '2'.              ELELIQML
00082                                                                   ELELIQML
00083  01  WS-TWA-PTR                 POINTER.                          ELELIQML
00084  01  WS-SEARCH-SUB              PICTURE S9(4) COMP.               ELELIQML
00085  01  AID-CODE-VALID             PICTURE X  VALUE 'N'.             ELELIQML
00086      88  AID-CODE-BAD                      VALUE 'N'.             ELELIQML
00087      88  AID-CODE-OK                       VALUE 'Y'.             ELELIQML
00088                                                                   ELELIQML
00089  01  WS-INVALID-PFK-MSG         PICTURE X(79) VALUE               ELELIQML
00090      'YOU CANNOT USE THAT KEY AT THIS TIME'.                      ELELIQML
00091                                                                   ELELIQML
00092  01  WS-ACTION-MODULE           PICTURE X(8) VALUE SPACES.        ELELIQML
00093 *                                                                 ELELIQML
00094 *    TEMPORARY COMMAREA - USED ONLY UPON FIRST ENTRY              ELELIQML
00095 *                                                                 ELELIQML
00096  01  WS-COMMAREA.                                                 ELELIQML
00097      05  WS-CIA-PTR             POINTER.                          ELELIQML
00098      EJECT                                                        ELELIQML
00099  01  FILLER                     PICTURE X(16)                     ELELIQML
00100                                 VALUE '*** ELSCIAC ***'.          ELELIQML
00101  COPY ELSCIAC.                                                    ELELIQML
00102      EJECT                                                        ELELIQML
00103  01  FILLER                     PICTURE X(16)                     ELELIQML
00104                                 VALUE '****ELIQM00 ****'.         ELELIQML
00105  COPY EL00SETC.                                                   ELELIQML
00106      EJECT                                                        ELELIQML
00107  01  FILLER                     PICTURE X(16)                     ELELIQML
00108                                 VALUE '**** DFHAID ****'.         ELELIQML
00109  COPY DFHAID.                                                     ELELIQML
00110      EJECT                                                        ELELIQML
00111  LINKAGE SECTION.                                                 ELELIQML
00112  01  DFHCOMMAREA.                                                 ELELIQML
00113                                                                   ELELIQML
00114  COPY ELSCOMMC.                                                   ELELIQML
00115      02 FILLER           PIC X(96).                               ELELIQML
00116      EJECT                                                        ELELIQML
00117 *                                                                 ELELIQML
00118 *   THE DUMMY CIA-AREA IS USED TO GET A POINTER TO THE            ELELIQML
00119 *   ACTUAL CIA WHICH IS IN WORKING STORAGE.  EACH MODULE          ELELIQML
00120 *   IN ELIQ WILL BE POINTED TO THE CIA IN THIS MODULE'S           ELELIQML
00121 *   WORKING STORAGE SECTION.                                      ELELIQML
00122 *                                                                 ELELIQML
00123  01  DUMMY-CIA-AREA             PICTURE X.                        ELELIQML
00124      EJECT                                                        ELELIQML
00125  COPY ELSSSCBC.                                                   ELELIQML
00126      EJECT                                                        ELELIQML
00127  COPY ELSTWAC.                                                    ELELIQML
00128      EJECT                                                        ELELIQML
00129  COPY ELSIOPMC.                                                   ELELIQML
00130      EJECT                                                        ELELIQML
00131      EJECT                                                        ELELIQML
00132  PROCEDURE DIVISION.                                              ELELIQML
00133 ************************************************************      ELELIQML
00134 *                                                          *      ELELIQML
00135 *                    PROCEDURE DIVISION                    *      ELELIQML
00136 *                                                          *      ELELIQML
00137 ************************************************************      ELELIQML
00138                                                                   ELELIQML
00139                                                                   ELELIQML
00140 ************************************************************      ELELIQML
00141 *                                                          *      ELELIQML
00142 *        PROCESS ELELIQML                                  *      ELELIQML
00143 *                                                          *      ELELIQML
00144 ************************************************************      ELELIQML
00145  PROCESS-ELELIQML.                                                ELELIQML
00146      IF EIBCALEN = ZERO                                           ELELIQML
00147          PERFORM INITIALIZE-INQUIRY-ENVIRONMENT                   ELELIQML
00148      ELSE                                                         ELELIQML
00149          PERFORM RESTORE-INQUIRY-ENVIRONMENT.                     ELELIQML
00150      MOVE +4 TO EIBCALEN.                                         ELELIQML
00151      PERFORM PROCESS-AID-CODE.                                    ELELIQML
00152      IF AID-CODE-OK                                               ELELIQML
00153          PERFORM MAIN-PROCESSING.                                 ELELIQML
00154      PERFORM SAVE-INQUIRY-ENVIRONMENT.                            ELELIQML
00155      EXEC CICS RETURN                                             ELELIQML
00156                TRANSID(EIBTRNID)                                  ELELIQML
00157                COMMAREA(DFHCOMMAREA)                              ELELIQML
00158                END-EXEC.                                          ELELIQML
00159      EJECT                                                        ELELIQML
00160                                                                   ELELIQML
00161                                                                   ELELIQML
00162 ************************************************************      ELELIQML
00163 *                                                          *      ELELIQML
00164 *        MAIN PROCESSING                                   *      ELELIQML
00165 *                                                          *      ELELIQML
00166 ************************************************************      ELELIQML
00167  MAIN-PROCESSING.                                                 ELELIQML
00168      IF SELECTION-IN-PROGRESS                                     ELELIQML
00169          PERFORM ITEM-SELECTION.                                  ELELIQML
00170      IF SELECTION-COMPLETED                                       ELELIQML
00171          PERFORM OUTPUT-GENERATION-PROCESS.                       ELELIQML
00172      IF DISPLAY-MODE                                              ELELIQML
00173          PERFORM DISPLAY-PROCESSING.                              ELELIQML
00174      EJECT                                                        ELELIQML
00175                                                                   ELELIQML
00176                                                                   ELELIQML
00177 ************************************************************      ELELIQML
00178 *                                                          *      ELELIQML
00179 *        INITIALIZE INQUIRY ENVIRONMENT                    *      ELELIQML
00180 *                                                          *      ELELIQML
00181 ************************************************************      ELELIQML
00182  INITIALIZE-INQUIRY-ENVIRONMENT.                                  ELELIQML
00183      CALL 'ELUADDRS' USING WS-COMMAREA                            ELELIQML
00184                            ADDRESS OF DFHCOMMAREA.                ELELIQML
00185      PERFORM ESTABLISH-ABEND-HANDLING.                            ELELIQML
00186      PERFORM INITIALIZE-POINTERS.                                 ELELIQML
00187      IF EIBAID NOT = DFHENTER                                     ELELIQML
00188          PERFORM TERMINATE-INQUIRY.                               ELELIQML
00189      PERFORM INITIALIZE-TEMPORARY-STORAGE.                        ELELIQML
00190      SET ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK                   ELELIQML
00191          TO CIA-ELSSSCB-PTR.                                      ELELIQML
00192      PERFORM SET-UP-NEW-INQUIRY.                                  ELELIQML
00193      PERFORM PARSE-COMMAND-LINE-FOR-PARAMET.                      ELELIQML
00194                                                                   ELELIQML
00195                                                                   ELELIQML
00196 ************************************************************      ELELIQML
00197 *                                                          *      ELELIQML
00198 *        INITIALIZE TEMPORARY STORAGE                      *      ELELIQML
00199 *                                                          *      ELELIQML
00200 ************************************************************      ELELIQML
00201  INITIALIZE-TEMPORARY-STORAGE.                                    ELELIQML
00202      SET CIA-STG-INITIALIZE TO TRUE.                              ELELIQML
00203      PERFORM CALL-STORAGE-MANAGER.                                ELELIQML
00204      EJECT                                                        ELELIQML
00205                                                                   ELELIQML
00206                                                                   ELELIQML
00207 ************************************************************      ELELIQML
00208 *                                                          *      ELELIQML
00209 *        PARSE COMMAND LINE FOR PARAMETERS                 *      ELELIQML
00210 *                                                          *      ELELIQML
00211 ************************************************************      ELELIQML
00212  PARSE-COMMAND-LINE-FOR-PARAMET.                                  ELELIQML
00213      EXEC CICS LINK                                               ELELIQML
00214                PROGRAM('ELSCMDLN')                                ELELIQML
00215                LENGTH (4 )                                        ELELIQML
00216                COMMAREA(DFHCOMMAREA)                              ELELIQML
00217                END-EXEC.                                          ELELIQML
00218      EJECT                                                        ELELIQML
00219                                                                   ELELIQML
00220                                                                   ELELIQML
00221 ************************************************************      ELELIQML
00222 *                                                          *      ELELIQML
00223 *        RESTORE INQUIRY ENVIRONMENT                       *      ELELIQML
00224 *                                                          *      ELELIQML
00225 ************************************************************      ELELIQML
00226  RESTORE-INQUIRY-ENVIRONMENT.                                     ELELIQML
00227      PERFORM ESTABLISH-ABEND-HANDLING.                            ELELIQML
00228      PERFORM INITIALIZE-POINTERS.                                 ELELIQML
00229      PERFORM READ-SELECTOR-STATUS-BLOCK.                          ELELIQML
00230      PERFORM DETERMINE-CORRECT-MODE.                              ELELIQML
00231                                                                   ELELIQML
00232                                                                   ELELIQML
00233 ************************************************************      ELELIQML
00234 *                                                          *      ELELIQML
00235 *        DETERMINE CORRECT MODE                            *      ELELIQML
00236 *                                                          *      ELELIQML
00237 ************************************************************      ELELIQML
00238  DETERMINE-CORRECT-MODE.                                          ELELIQML
00239      IF SSB-SS-SELECTION-DONE                                     ELELIQML
00240         SET DISPLAY-MODE TO TRUE                                  ELELIQML
00241      ELSE                                                         ELELIQML
00242         SET SELECTION-IN-PROGRESS TO TRUE.                        ELELIQML
00243                                                                   ELELIQML
00244      EJECT                                                        ELELIQML
00245                                                                   ELELIQML
00246                                                                   ELELIQML
00247 ************************************************************      ELELIQML
00248 *                                                          *      ELELIQML
00249 *        ESTABLISH ABEND HANDLING                          *      ELELIQML
00250 *                                                          *      ELELIQML
00251 ************************************************************      ELELIQML
00252  ESTABLISH-ABEND-HANDLING.                                        ELELIQML
00253      EXEC CICS ADDRESS                                            ELELIQML
00254                TWA(WS-TWA-PTR)                                    ELELIQML
00255                END-EXEC.                                          ELELIQML
00256      SET ADDRESS OF TWA-TRANSACTION-WORK-AREA                     ELELIQML
00257           TO WS-TWA-PTR.                                          ELELIQML
00258      SET TWA-ELSCOMM-PTR                                          ELELIQML
00259           TO ADDRESS OF DFHCOMMAREA.                              ELELIQML
00260      EXEC CICS HANDLE ABEND                                       ELELIQML
00261                PROGRAM('ELUABEND')                                ELELIQML
00262                END-EXEC.                                          ELELIQML
00263      EJECT                                                        ELELIQML
00264                                                                   ELELIQML
00265                                                                   ELELIQML
00266 ************************************************************      ELELIQML
00267 *                                                          *      ELELIQML
00268 *        INITIALIZE POINTERS                               *      ELELIQML
00269 *                                                          *      ELELIQML
00270 ************************************************************      ELELIQML
00271  INITIALIZE-POINTERS.                                             ELELIQML
00272      PERFORM INITIALIZE-CIA-POINTER.                              ELELIQML
00273      PERFORM INITIALIZE-SMA-POINTER.                              ELELIQML
00274                                                                   ELELIQML
00275                                                                   ELELIQML
00276 ************************************************************      ELELIQML
00277 *                                                          *      ELELIQML
00278 *        INITIALIZE CIA POINTER                            *      ELELIQML
00279 *                                                          *      ELELIQML
00280 ************************************************************      ELELIQML
00281  INITIALIZE-CIA-POINTER.                                          ELELIQML
00282      CALL 'ELUADDRS' USING                                        ELELIQML
00283          CIA-ELS-COMMON-INTERFACE-AREA                            ELELIQML
00284                            ADDRESS OF DUMMY-CIA-AREA.             ELELIQML
00285      SET ECA-CIA-PTR IN DFHCOMMAREA                               ELELIQML
00286           TO  ADDRESS OF DUMMY-CIA-AREA.                          ELELIQML
00287      SET CIA-ELSCIA-PTR TO ECA-CIA-PTR.                           ELELIQML
00288                                                                   ELELIQML
00289                                                                   ELELIQML
00290 ************************************************************      ELELIQML
00291 *                                                          *      ELELIQML
00292 *        INITIALIZE SMA POINTER                            *      ELELIQML
00293 *                                                          *      ELELIQML
00294 ************************************************************      ELELIQML
00295  INITIALIZE-SMA-POINTER.                                          ELELIQML
00296      CALL 'ELUADDRS' USING CIA-STG-MGT                            ELELIQML
00297                            CIA-ELSSMA-PTR.                        ELELIQML
00298                                                                   ELELIQML
00299                                                                   ELELIQML
00300 ************************************************************      ELELIQML
00301 *                                                          *      ELELIQML
00302 *        READ SELECTOR STATUS BLOCK                        *      ELELIQML
00303 *                                                          *      ELELIQML
00304 ************************************************************      ELELIQML
00305  READ-SELECTOR-STATUS-BLOCK.                                      ELELIQML
00306      SET CIA-STG-RETRIEVE TO TRUE.                                ELELIQML
00307      MOVE CIA-ELSSSCB-DDN TO CIA-DDNAME.                          ELELIQML
00308      PERFORM CALL-STORAGE-MANAGER.                                ELELIQML
00309      SET ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK                   ELELIQML
00310          TO CIA-ELSSSCB-PTR.                                      ELELIQML
00311                                                                   ELELIQML
00312                                                                   ELELIQML
00313 ************************************************************      ELELIQML
00314 *                                                          *      ELELIQML
00315 *        PROCESS AID CODE                                  *      ELELIQML
00316 *                                                          *      ELELIQML
00317 ************************************************************      ELELIQML
00318  PROCESS-AID-CODE.                                                ELELIQML
00319      SET AID-CODE-OK  TO TRUE.                                    ELELIQML
00320      IF EIBAID = DFHENTER                                         ELELIQML
00321          PERFORM ENTER-AID-CODE                                   ELELIQML
00322      ELSE IF EIBAID = DFHPF5 OR DFHPF17                           ELELIQML
00323          PERFORM SET-UP-NEW-INQUIRY                               ELELIQML
00324      ELSE IF EIBAID = DFHCLEAR                                    ELELIQML
00325          PERFORM TERMINATE-INQUIRY                                ELELIQML
00326      ELSE                                                         ELELIQML
00327          PERFORM CHECK-FOR-COMPLEX-KEYS.                          ELELIQML
00328                                                                   ELELIQML
00329                                                                   ELELIQML
00330 ************************************************************      ELELIQML
00331 *                                                          *      ELELIQML
00332 *        CHECK FOR COMPLEX KEYS                            *      ELELIQML
00333 *                                                          *      ELELIQML
00334 ************************************************************      ELELIQML
00335  CHECK-FOR-COMPLEX-KEYS.                                          ELELIQML
00336      IF (DISPLAY-MODE OR                                          ELELIQML
00337                   SSB-IN-MENU (SSB-SELECTOR-STATE))               ELELIQML
00338               AND                                                 ELELIQML
00339                  (EIBAID =   DFHPF7  OR DFHPF19 OR                ELELIQML
00340                              DFHPF8  OR DFHPF20 OR                ELELIQML
00341                              DFHPF10 OR DFHPF22 OR                ELELIQML
00342                              DFHPF11 OR DFHPF23)                  ELELIQML
00343          PERFORM SCROLL-DISPLAY-REQUEST                           ELELIQML
00344      ELSE IF DISPLAY-MODE AND                                     ELELIQML
00345                  (EIBAID =   DFHPF2 OR DFHPF14)                   ELELIQML
00346          PERFORM PRINT-ALL-KEY                                    ELELIQML
00347      ELSE IF SSB-SS-GET-GROUP OR                                  ELELIQML
00348                  SSB-SS-GET-SECTION                               ELELIQML
00349          PERFORM DISPLAY-UNDEFINED-PFKS                           ELELIQML
00350      ELSE                                                         ELELIQML
00351          PERFORM CHECK-FOR-RESELECTION-PFK.                       ELELIQML
00352                                                                   ELELIQML
00353                                                                   ELELIQML
00354 ************************************************************      ELELIQML
00355 *                                                          *      ELELIQML
00356 *        CHECK FOR RESELECTION PFK                         *      ELELIQML
00357 *                                                          *      ELELIQML
00358 ************************************************************      ELELIQML
00359  CHECK-FOR-RESELECTION-PFK.                                       ELELIQML
00360      IF EIBAID = DFHPF9 OR DFHPF21                                ELELIQML
00361          PERFORM RESELECT-WITH-MENU                               ELELIQML
00362      ELSE IF EIBAID = DFHPF3 OR DFHPF15                           ELELIQML
00363               OR EIBAID = DFHPF4 OR DFHPF16                       ELELIQML
00364          PERFORM RESELECT-WITHOUT-MENU                            ELELIQML
00365      ELSE                                                         ELELIQML
00366          PERFORM DISPLAY-UNDEFINED-PFKS.                          ELELIQML
00367                                                                   ELELIQML
00368                                                                   ELELIQML
00369 ************************************************************      ELELIQML
00370 *                                                          *      ELELIQML
00371 *        SCROLL DISPLAY REQUEST                            *      ELELIQML
00372 *                                                          *      ELELIQML
00373 ************************************************************      ELELIQML
00374  SCROLL-DISPLAY-REQUEST.                                          ELELIQML
00375      CONTINUE.                                                    ELELIQML
00376                                                                   ELELIQML
00377                                                                   ELELIQML
00378 ************************************************************      ELELIQML
00379 *                                                          *      ELELIQML
00380 *        PRINT ALL KEY                                     *      ELELIQML
00381 *                                                          *      ELELIQML
00382 ************************************************************      ELELIQML
00383  PRINT-ALL-KEY.                                                   ELELIQML
00384      CONTINUE.                                                    ELELIQML
00385                                                                   ELELIQML
00386                                                                   ELELIQML
00387 ************************************************************      ELELIQML
00388 *                                                          *      ELELIQML
00389 *        DISPLAY UNDEFINED PFKS                            *      ELELIQML
00390 *                                                          *      ELELIQML
00391 ************************************************************      ELELIQML
00392  DISPLAY-UNDEFINED-PFKS.                                          ELELIQML
00393      SET AID-CODE-BAD  TO  TRUE.                                  ELELIQML
00394      MOVE WS-INVALID-PFK-MSG TO ERRMSGO.                          ELELIQML
00395      PERFORM SEND-ERROR-MAP-AND-DATA.                             ELELIQML
00396                                                                   ELELIQML
00397                                                                   ELELIQML
00398 ************************************************************      ELELIQML
00399 *                                                          *      ELELIQML
00400 *        SEND ERROR MAP AND DATA                           *      ELELIQML
00401 *                                                          *      ELELIQML
00402 ************************************************************      ELELIQML
00403  SEND-ERROR-MAP-AND-DATA.                                         ELELIQML
00404      EXEC CICS SEND MAP('EL00MAP')                                ELELIQML
00405                MAPSET('EL00SET')                                  ELELIQML
00406                FROM(EL00MAPO)                                     ELELIQML
00407                CURSOR(EIBCPOSN)                                   ELELIQML
00408                END-EXEC.                                          ELELIQML
00409      EJECT                                                        ELELIQML
00410                                                                   ELELIQML
00411                                                                   ELELIQML
00412 ************************************************************      ELELIQML
00413 *                                                          *      ELELIQML
00414 *        SET UP NEW INQUIRY                                *      ELELIQML
00415 *                                                          *      ELELIQML
00416 ************************************************************      ELELIQML
00417  SET-UP-NEW-INQUIRY.                                              ELELIQML
00418      IF DISPLAY-MODE                                              ELELIQML
00419          PERFORM DISPLAY-PROCESSING.                              ELELIQML
00420      MOVE LOW-VALUES           TO                                 ELELIQML
00421          SSB-SELECTOR-STATUS-CTL-BLK.                             ELELIQML
00422      MOVE SPACES               TO  SSB-ACTION-MODULE.             ELELIQML
00423      MOVE ALL '0'              TO  SSB-MODULE-STATUS-TABLE.       ELELIQML
00424      SET SSB-SS-GET-RESELECT   TO  TRUE.                          ELELIQML
00425      SET SSB-COMPLETED (SSB-SELECTOR-STATE)                       ELELIQML
00426          TO TRUE.                                                 ELELIQML
00427      SET SSB-SS-SELECTION-DONE TO  TRUE.                          ELELIQML
00428      COMPUTE SSB-MAX-MODULES = SSB-SELECTOR-STATE - 1.            ELELIQML
00429      SET SSB-SS-GET-GROUP      TO  TRUE.                          ELELIQML
00430      SET SELECTION-IN-PROGRESS TO  TRUE.                          ELELIQML
00431      SET SSB-STACK-EMPTY       TO  TRUE.                          ELELIQML
00432                                                                   ELELIQML
00433                                                                   ELELIQML
00434 ************************************************************      ELELIQML
00435 *                                                          *      ELELIQML
00436 *        RESELECT WITHOUT MENU                             *      ELELIQML
00437 *                                                          *      ELELIQML
00438 ************************************************************      ELELIQML
00439  RESELECT-WITHOUT-MENU.                                           ELELIQML
00440      PERFORM RESELECT-WITH-MENU.                                  ELELIQML
00441      EXEC CICS LINK                                               ELELIQML
00442                PROGRAM('ELSRESEL')                                ELELIQML
00443                LENGTH (4 )                                        ELELIQML
00444                COMMAREA(DFHCOMMAREA)                              ELELIQML
00445           END-EXEC.                                               ELELIQML
00446                                                                   ELELIQML
00447                                                                   ELELIQML
00448 ************************************************************      ELELIQML
00449 *                                                          *      ELELIQML
00450 *        RESELECT WITH MENU                                *      ELELIQML
00451 *                                                          *      ELELIQML
00452 ************************************************************      ELELIQML
00453  RESELECT-WITH-MENU.                                              ELELIQML
00454      IF DISPLAY-MODE                                              ELELIQML
00455          PERFORM DISPLAY-PROCESSING.                              ELELIQML
00456      IF NOT SSB-SS-SELECTION-DONE                                 ELELIQML
00457          PERFORM RESET-CURRENT-SELECTOR.                          ELELIQML
00458      SET SSB-SS-GET-RESELECT TO TRUE.                             ELELIQML
00459      PERFORM RESET-CURRENT-SELECTOR.                              ELELIQML
00460      SET SELECTION-IN-PROGRESS TO TRUE.                           ELELIQML
00461                                                                   ELELIQML
00462                                                                   ELELIQML
00463 ************************************************************      ELELIQML
00464 *                                                          *      ELELIQML
00465 *        RESET CURRENT SELECTOR                            *      ELELIQML
00466 *                                                          *      ELELIQML
00467 ************************************************************      ELELIQML
00468  RESET-CURRENT-SELECTOR.                                          ELELIQML
00469      SET SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                    ELELIQML
00470           TO TRUE.                                                ELELIQML
00471      EJECT                                                        ELELIQML
00472                                                                   ELELIQML
00473                                                                   ELELIQML
00474 ************************************************************      ELELIQML
00475 *                                                          *      ELELIQML
00476 *        TERMINATE INQUIRY                                 *      ELELIQML
00477 *                                                          *      ELELIQML
00478 ************************************************************      ELELIQML
00479  TERMINATE-INQUIRY.                                               ELELIQML
00480      PERFORM PURGE-ALL-TEMPORARY-STORAGE-QU.                      ELELIQML
00481      PERFORM PURGE-MAIN-STORAGE.                                  ELELIQML
00482      EXEC CICS SEND CONTROL                                       ELELIQML
00483                ERASE                                              ELELIQML
00484                CURSOR(0)                                          ELELIQML
00485                END-EXEC.                                          ELELIQML
00486      EXEC CICS RETURN                                             ELELIQML
00487                END-EXEC.                                          ELELIQML
00488      EJECT                                                        ELELIQML
00489                                                                   ELELIQML
00490                                                                   ELELIQML
00491 ************************************************************      ELELIQML
00492 *                                                          *      ELELIQML
00493 *        PURGE ALL TEMPORARY STORAGE QUEUES                *      ELELIQML
00494 *                                                          *      ELELIQML
00495 ************************************************************      ELELIQML
00496  PURGE-ALL-TEMPORARY-STORAGE-QU.                                  ELELIQML
00497      MOVE CIA-ELSMENU-DDN TO CIA-DDNAME.                          ELELIQML
00498      SET CIA-STG-GETMAIN TO TRUE.                                 ELELIQML
00499      IF CIA-ELSMENU-PTR = NULL                                    ELELIQML
00500          PERFORM CALL-STORAGE-MANAGER.                            ELELIQML
00501      SET ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                   ELELIQML
00502          TO CIA-ELSMENU-PTR.                                      ELELIQML
00503      SET IOP-PURGE-TSQ TO TRUE.                                   ELELIQML
00504      EXEC CICS LINK                                               ELELIQML
00505                PROGRAM('ELUIOPGM')                                ELELIQML
00506                LENGTH (4 )                                        ELELIQML
00507                COMMAREA(DFHCOMMAREA)                              ELELIQML
00508           END-EXEC.                                               ELELIQML
00509                                                                   ELELIQML
00510                                                                   ELELIQML
00511 ************************************************************      ELELIQML
00512 *                                                          *      ELELIQML
00513 *        PURGE MAIN STORAGE                                *      ELELIQML
00514 *                                                          *      ELELIQML
00515 ************************************************************      ELELIQML
00516  PURGE-MAIN-STORAGE.                                              ELELIQML
00517      SET CIA-STG-PURGE TO TRUE.                                   ELELIQML
00518      PERFORM CALL-STORAGE-MANAGER.                                ELELIQML
00519                                                                   ELELIQML
00520                                                                   ELELIQML
00521 ************************************************************      ELELIQML
00522 *                                                          *      ELELIQML
00523 *        ENTER AID CODE                                    *      ELELIQML
00524 *                                                          *      ELELIQML
00525 ************************************************************      ELELIQML
00526  ENTER-AID-CODE.                                                  ELELIQML
00527      CONTINUE.                                                    ELELIQML
00528      EJECT                                                        ELELIQML
00529                                                                   ELELIQML
00530                                                                   ELELIQML
00531 ************************************************************      ELELIQML
00532 *                                                          *      ELELIQML
00533 *        ITEM SELECTION                                    *      ELELIQML
00534 *                                                          *      ELELIQML
00535 ************************************************************      ELELIQML
00536  ITEM-SELECTION.                                                  ELELIQML
00537      IF SSB-IN-MENU (SSB-SELECTOR-STATE)                          ELELIQML
00538          PERFORM MENU-PROCESSING.                                 ELELIQML
00539      IF SSB-PRIMARY-SCREEN-OUT   (SSB-SELECTOR-STATE) OR          ELELIQML
00540                  SSB-SECONDARY-SCREEN-OUT (SSB-SELECTOR-STATE)    ELELIQML
00541          OR                                                       ELELIQML
00542                  SSB-CMDLN-INPUT          (SSB-SELECTOR-STATE)    ELELIQML
00543          OR                                                       ELELIQML
00544                  SSB-MENU-COMPLETE                                ELELIQML
00545          (SSB-SELECTOR-STATE)                                     ELELIQML
00546          PERFORM RUN-SELECTOR-PROGRAM.                            ELELIQML
00547      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE) OR                  ELELIQML
00548                  SSB-RESELECTION  (SSB-SELECTOR-STATE) OR         ELELIQML
00549                  SSB-COMPLETED    (SSB-SELECTOR-STATE)            ELELIQML
00550          PERFORM DETERMINE-AND-RUN-NEW-SELECTOR.                  ELELIQML
00551      IF SSB-SS-SELECTION-DONE                                     ELELIQML
00552         SET SELECTION-COMPLETED TO TRUE.                          ELELIQML
00553      EJECT                                                        ELELIQML
00554                                                                   ELELIQML
00555                                                                   ELELIQML
00556 ************************************************************      ELELIQML
00557 *                                                          *      ELELIQML
00558 *        DETERMINE AND RUN NEW SELECTOR PROGRAM            *      ELELIQML
00559 *                                                          *      ELELIQML
00560 ************************************************************      ELELIQML
00561  DETERMINE-AND-RUN-NEW-SELECTOR.                                  ELELIQML
00562      MOVE SPACES TO SSB-ACTION-MODULE.                            ELELIQML
00563      PERFORM DETERMINE-SELECTOR                                   ELELIQML
00564          UNTIL SSB-ACTION-MODULE NOT = SPACES                     ELELIQML
00565                  OR SSB-SS-SELECTION-DONE.                        ELELIQML
00566      IF SSB-ACTION-MODULE NOT = SPACES                            ELELIQML
00567          PERFORM RUN-SELECTOR-PROGRAM.                            ELELIQML
00568      EJECT                                                        ELELIQML
00569                                                                   ELELIQML
00570                                                                   ELELIQML
00571 ************************************************************      ELELIQML
00572 *                                                          *      ELELIQML
00573 *        DETERMINE SELECTOR                                *      ELELIQML
00574 *                                                          *      ELELIQML
00575 ************************************************************      ELELIQML
00576  DETERMINE-SELECTOR.                                              ELELIQML
00577      IF CIA-SEL-TYP-KEY (SSB-SELECTOR-STATE)                      ELELIQML
00578          PERFORM DO-KEY-SELECTION                                 ELELIQML
00579      ELSE IF CIA-SEL-TYP-TOP (SSB-SELECTOR-STATE)                 ELELIQML
00580          PERFORM DO-TOPIC-SELECTION                               ELELIQML
00581      ELSE IF SSB-SS-GET-RESELECT                                  ELELIQML
00582               AND (EIBAID = DFHPF9 OR DFHPF21)                    ELELIQML
00583          PERFORM DO-RESELECTION                                   ELELIQML
00584      ELSE IF CIA-SEL-TYP-OTH (SSB-SELECTOR-STATE)                 ELELIQML
00585          ADD 1 TO SSB-SELECTOR-STATE                              ELELIQML
00586      ELSE                                                         ELELIQML
00587          PERFORM BAD-INTERNAL-TABLE.                              ELELIQML
00588                                                                   ELELIQML
00589                                                                   ELELIQML
00590 ************************************************************      ELELIQML
00591 *                                                          *      ELELIQML
00592 *        BAD INTERNAL TABLE                                *      ELELIQML
00593 *                                                          *      ELELIQML
00594 ************************************************************      ELELIQML
00595  BAD-INTERNAL-TABLE.                                              ELELIQML
00596      SET CIA-AB-PGM-LOGIC TO TRUE.                                ELELIQML
00597      EXEC CICS ABEND                                              ELELIQML
00598                ABCODE(CIA-ABCODE)                                 ELELIQML
00599                END-EXEC.                                          ELELIQML
00600      EJECT                                                        ELELIQML
00601                                                                   ELELIQML
00602                                                                   ELELIQML
00603 ************************************************************      ELELIQML
00604 *                                                          *      ELELIQML
00605 *        DO KEY SELECTION                                  *      ELELIQML
00606 *                                                          *      ELELIQML
00607 ************************************************************      ELELIQML
00608  DO-KEY-SELECTION.                                                ELELIQML
00609      MOVE 'ELUKYSEL' TO WS-ACTION-MODULE.                         ELELIQML
00610      PERFORM CALL-ACTION-MODULE.                                  ELELIQML
00611      EJECT                                                        ELELIQML
00612                                                                   ELELIQML
00613                                                                   ELELIQML
00614 ************************************************************      ELELIQML
00615 *                                                          *      ELELIQML
00616 *        DO TOPIC SELECTION                                *      ELELIQML
00617 *                                                          *      ELELIQML
00618 ************************************************************      ELELIQML
00619  DO-TOPIC-SELECTION.                                              ELELIQML
00620      MOVE 'ELUTPSEL' TO WS-ACTION-MODULE.                         ELELIQML
00621      PERFORM CALL-ACTION-MODULE.                                  ELELIQML
00622                                                                   ELELIQML
00623                                                                   ELELIQML
00624 ************************************************************      ELELIQML
00625 *                                                          *      ELELIQML
00626 *        DO RESELECTION                                    *      ELELIQML
00627 *                                                          *      ELELIQML
00628 ************************************************************      ELELIQML
00629  DO-RESELECTION.                                                  ELELIQML
00630      MOVE 'ELSRESEL' TO SSB-ACTION-MODULE.                        ELELIQML
00631                                                                   ELELIQML
00632                                                                   ELELIQML
00633 ************************************************************      ELELIQML
00634 *                                                          *      ELELIQML
00635 *        RUN SELECTOR PROGRAM                              *      ELELIQML
00636 *                                                          *      ELELIQML
00637 ************************************************************      ELELIQML
00638  RUN-SELECTOR-PROGRAM.                                            ELELIQML
00639      MOVE SSB-ACTION-MODULE TO WS-ACTION-MODULE.                  ELELIQML
00640      PERFORM CALL-ACTION-MODULE.                                  ELELIQML
00641      IF SSB-START-MENU (SSB-SELECTOR-STATE)                       ELELIQML
00642               OR SSB-IN-MENU (SSB-SELECTOR-STATE)                 ELELIQML
00643          PERFORM MENU-PROCESSING.                                 ELELIQML
00644                                                                   ELELIQML
00645                                                                   ELELIQML
00646 ************************************************************      ELELIQML
00647 *                                                          *      ELELIQML
00648 *        MENU PROCESSING                                   *      ELELIQML
00649 *                                                          *      ELELIQML
00650 ************************************************************      ELELIQML
00651  MENU-PROCESSING.                                                 ELELIQML
00652      MOVE 'ELSMENUP' TO WS-ACTION-MODULE.                         ELELIQML
00653      PERFORM CALL-ACTION-MODULE.                                  ELELIQML
00654                                                                   ELELIQML
00655                                                                   ELELIQML
00656 ************************************************************      ELELIQML
00657 *                                                          *      ELELIQML
00658 *        OUTPUT GENERATION PROCESS                         *      ELELIQML
00659 *                                                          *      ELELIQML
00660 ************************************************************      ELELIQML
00661  OUTPUT-GENERATION-PROCESS.                                       ELELIQML
00662      PERFORM READ-ALL-THE-REQUIRED-RECORDS.                       ELELIQML
00663      PERFORM CALL-THE-PROLOG.                                     ELELIQML
00664      PERFORM RUN-TOPIC-PROGRAM.                                   ELELIQML
00665      SET DISPLAY-MODE TO TRUE.                                    ELELIQML
00666      EJECT                                                        ELELIQML
00667                                                                   ELELIQML
00668                                                                   ELELIQML
00669 ************************************************************      ELELIQML
00670 *                                                          *      ELELIQML
00671 *        RUN TOPIC PROGRAM                                 *      ELELIQML
00672 *                                                          *      ELELIQML
00673 ************************************************************      ELELIQML
00674  RUN-TOPIC-PROGRAM.                                               ELELIQML
00675      MOVE SSB-TOPIC-PGM TO WS-ACTION-MODULE.                      ELELIQML
00676      PERFORM CALL-ACTION-MODULE.                                  ELELIQML
00677                                                                   ELELIQML
00678                                                                   ELELIQML
00679 ************************************************************      ELELIQML
00680 *                                                          *      ELELIQML
00681 *        READ ALL THE REQUIRED RECORDS                     *      ELELIQML
00682 *                                                          *      ELELIQML
00683 ************************************************************      ELELIQML
00684  READ-ALL-THE-REQUIRED-RECORDS.                                   ELELIQML
00685      MOVE 'ELUSETUP'  TO  WS-ACTION-MODULE.                       ELELIQML
00686      PERFORM CALL-ACTION-MODULE.                                  ELELIQML
00687                                                                   ELELIQML
00688                                                                   ELELIQML
00689 ************************************************************      ELELIQML
00690 *                                                          *      ELELIQML
00691 *        CALL THE PROLOG                                   *      ELELIQML
00692 *                                                          *      ELELIQML
00693 ************************************************************      ELELIQML
00694  CALL-THE-PROLOG.                                                 ELELIQML
00695      MOVE 'ELUPROLG'  TO  WS-ACTION-MODULE.                       ELELIQML
00696      PERFORM CALL-ACTION-MODULE.                                  ELELIQML
00697      EJECT                                                        ELELIQML
00698                                                                   ELELIQML
00699                                                                   ELELIQML
00700 ************************************************************      ELELIQML
00701 *                                                          *      ELELIQML
00702 *        DISPLAY PROCESSING                                *      ELELIQML
00703 *                                                          *      ELELIQML
00704 ************************************************************      ELELIQML
00705  DISPLAY-PROCESSING.                                              ELELIQML
00706      MOVE 'ELSPGDSP' TO WS-ACTION-MODULE.                         ELELIQML
00707      PERFORM CALL-ACTION-MODULE.                                  ELELIQML
00708                                                                   ELELIQML
00709                                                                   ELELIQML
00710 ************************************************************      ELELIQML
00711 *                                                          *      ELELIQML
00712 *        SAVE INQUIRY ENVIRONMENT                          *      ELELIQML
00713 *                                                          *      ELELIQML
00714 ************************************************************      ELELIQML
00715  SAVE-INQUIRY-ENVIRONMENT.                                        ELELIQML
00716      SET CIA-STG-STOW  TO  TRUE.                                  ELELIQML
00717      MOVE CIA-ELSSSCB-DDN TO CIA-DDNAME.                          ELELIQML
00718      PERFORM CALL-STORAGE-MANAGER.                                ELELIQML
00719      EJECT                                                        ELELIQML
00720                                                                   ELELIQML
00721                                                                   ELELIQML
00722 ************************************************************      ELELIQML
00723 *                                                          *      ELELIQML
00724 *        CALL STORAGE MANAGER                              *      ELELIQML
00725 *                                                          *      ELELIQML
00726 ************************************************************      ELELIQML
00727  CALL-STORAGE-MANAGER.                                            ELELIQML
00728      MOVE 'ELUSTGMG'  TO  WS-ACTION-MODULE.                       ELELIQML
00729      PERFORM CALL-ACTION-MODULE.                                  ELELIQML
00730      EJECT                                                        ELELIQML
00731                                                                   ELELIQML
00732                                                                   ELELIQML
00733 ************************************************************      ELELIQML
00734 *                                                          *      ELELIQML
00735 *        CALL ACTION MODULE                                *      ELELIQML
00736 *                                                          *      ELELIQML
00737 ************************************************************      ELELIQML
00738  CALL-ACTION-MODULE.                                              ELELIQML
00739      EXEC CICS LINK                                               ELELIQML
00740                PROGRAM(WS-ACTION-MODULE)                          ELELIQML
00741                LENGTH (4 )                                        ELELIQML
00742                COMMAREA(DFHCOMMAREA)                              ELELIQML
00743                END-EXEC.                                          ELELIQML
00744      EJECT                                                        ELELIQML
00745  GOBACK-PARAGRAPH.                                                ELELIQML
00746 ************************************************************      ELELIQML
00747 *                                                          *      ELELIQML
00748 *                         STOP RUN                         *      ELELIQML
00749 *                                                          *      ELELIQML
00750 ************************************************************      ELELIQML
00751      GOBACK.                                                      ELELIQML
