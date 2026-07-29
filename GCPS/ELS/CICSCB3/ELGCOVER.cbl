00001  ID DIVISION.                                                     09/03/03
00002  PROGRAM-ID.   ELGCOVER.                                          ELGCOVER
00003  AUTHOR.       JERRY ARKEMA.                                         LV002
00004  DATE-WRITTEN. 03/28/86.                                          ELGCOVER
00005  DATE-COMPILED.                                                   ELGCOVER
00006                                                                   ELGCOVER
00007 *@>ELGCOVER                                                       ELGCOVER
00008 *@¬                                                               ELGCOVER
00009 *                        PROGRAM ABSTRACT                         ELGCOVER
00010 *                                                                 ELGCOVER
00011 *@¬ PROGRAM NAME:   ENGLISH CONTRACT INQUIRY                      ELGCOVER
00012 *@¬                                                               ELGCOVER
00013 *@¬ PROGRAM I.D.:   ELGCOVER                                      ELGCOVER
00014 *@¬                                                               ELGCOVER
00015 *@¬ PURPOSE:  BENEFIT PROVISION DETERMINATION COMMON SUBROUTINE   ELGCOVER
00016 *@¬                                                               ELGCOVER
00017 *@¬ OVERVIEW:  THIS PROGRAM DETERMINES WHETHER OR NOT A PARTICULARELGCOVER
00018 *@¬           TOPIC  IS  COVERED  IN  TERMS OF BENEFIT PROVISIONS.ELGCOVER
00019 *@¬           THIS  IS  ACCOMPLISHED  BY PASSING ONE TO FOUR LISTSELGCOVER
00020 *@¬           OF  PROVISIONS  BY  THE CONTRACT(S) AVAILABLE TO IT.ELGCOVER
00021 *@¬           THE  TOPIC  IS  CONSIDERED  TO  HAVE  NO COVERAGE IFELGCOVER
00022 *@¬           NONE  OF  THE BENEFIT PROVISIONS IN A LIST APPEAR INELGCOVER
00023 *@¬           THE  AVAILABLE  CONTRACT(S).  THE TOPIC IS DEEMED TOELGCOVER
00024 *@¬           BE  PARTIALLY COVERED IF SOME OF THE PROVISIONS IN AELGCOVER
00025 *@¬           LIST  APPEAR  IN THE AVAILABLE CONTRACTS.  THE TOPICELGCOVER
00026 *@¬           IS CONSIDERED TO BE FULLY COVERED IF ALL THE BENEFITELGCOVER
00027 *@¬           PROVISIONS  IN  A  LIST  ARE  FOUND IN THE AVAILABLEELGCOVER
00028 *@¬           CONTRACT(S).                                        ELGCOVER
00029 *@¬                                                               ELGCOVER
00030 *@¬ RECORDS                                                       ELGCOVER
00031 *@¬ ACCESSED: ELCDCIA RECORD            --+                       ELGCOVER
00032 *@¬           GROUP SPECIFIC RECORD       |  ALL RECORDS ARE      ELGCOVER
00033 *@¬           CONTRACT RECORD(S)          |   PASSED FROM CALLING ELGCOVER
00034 *@¬           BENEFIT PROVISION LIST(S) --+   PROGRAM.            ELGCOVER
00035 *@¬                                                               ELGCOVER
00036 *@¬ PROCESSING                                                    ELGCOVER
00037 *@¬ FUNCTIONS: THIS MODULE PERFORMS THE FOLLOWING FUNCTIONS:      ELGCOVER
00038 *@¬                                                               ELGCOVER
00039 *@¬            1. INITIALIZES WORK DATA ELEMENTS.                 ELGCOVER
00040 *@¬                                                               ELGCOVER
00041 *@¬            2. PROCESS FOR EACH BENEFIT PROVISION LIST:        ELGCOVER
00042 *@¬               A) SCAN  ALL AVAILABLE CONTRACT(S) FOR  A MATCH ELGCOVER
00043 *@¬                  ON  EACH  BENEFIT  PROVISION  IN  THE  LIST. ELGCOVER
00044 *@¬               B) WHEN  A  MATCH  IS FOUND,  EXTRACT  THE SLOT ELGCOVER
00045 *@¬                  NUMBER  FROM  THE  CONTRACT  AND PLACE IT IN ELGCOVER
00046 *@¬                  THE APPROPRIATE ENTRY OF THE PROVISION LIST. ELGCOVER
00047 *@¬                                                               ELGCOVER
00048 *@¬            3. UPDATE  THE   PROVISION LIST, MARKING IT AS \
00049 *@¬               COVERED\
00050 *@¬               COVERED\
00051 *@¬               RECORD SEARCH IN \
00052 *@¬                                                               ELGCOVER
00053 *@¬            4. BASED  ON  THE  MARKED  COVERAGE FROM \
00054 *@¬               APPROPRIATE MESSAGES TO THE PAGE FILE.          ELGCOVER
00055 *@¬                                                               ELGCOVER
00056                                                                   ELGCOVER
00057 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          ELGCOVER
00058 *    *-*         U P D A T E   H I S T O R Y         *-*          ELGCOVER
00059 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          ELGCOVER
00060                                                                   ELGCOVER
00061 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* ELGCOVER
00062                                                                   ELGCOVER
00063 *  XXXX      03/28/86  JLA  INITIAL WRITING.                      ELGCOVER
00064 *  0001      04/30/86  JLA  USE LONG CODES MANUAL DESCRIPTIONS    ELGCOVER
00065 *                            INSTEAD OF SHORT.                    ELGCOVER
00066 *  0002      10/16/86  JTC  VS COBOL II CONVERSION                ELGCOVER
00067 *  0003      10/20/87  EGL  CHANGED FIXED TEXT AS FOLLOWS         ELGCOVER
00068 *                         'NOT COVERED'        --> 'NOT COVERED.' ELGCOVER
00069 *                         'COVERED'            --> 'COVERED.'     ELGCOVER
00070 *                         'COVERED EXCEPT FOR' --> 'NOT COVERED:' ELGCOVER
00071 *                      ALSO, ADDED A LINE OF * AFTER COVERAGE TEXTELGCOVER
00072 *                                                                 ELGCOVER
00073 *  ----      04/21/88  REB  THERE IS FURTHER CONSIDERATION IN     ELGCOVER
00074 *                           DETERMINING IF PROVISIONS 'DRB  A' ANDELGCOVER
00075 *                           'PVTR A' ARE COVERED FOR ECF TOPIC. CEELGCOVER
00076 *                           PROVIDER CODES MUST BE ON #PVE TABULARELGCOVER
00077 *                                                                 ELGCOVER
00078 *  ----      11/10/88  NAC  CORRECT LOGIC NOT TO FALL THRU TO     ELGCOVER
00079 *                           TEXT COMPRESSION AND UNSTRING WHEN    ELGCOVER
00080 *                           PERFORMING ANOTHER SECTION; INCLUDE NEELGCOVER
00081 *                           STORAGE ENHANCEMENTS.                 ELGCOVER
00082 *  ----      11/14/88  NAC  CORRECT LOGIC TO CHECK AGAINST INST   ELGCOVER
00083 *                           CONTRACT FOR FORMATS A,B,W INSTEAD OF ELGCOVER
00084 *                           PROFESSIONAL.                         ELGCOVER
00085 *  ----      12/02/88  NAC  HANDLE OVERFLOW CONDITION WHEN LISTINGELGCOVER
00086 *                           BENEFIT PROVISIONS.                   ELGCOVER
00087 *       21-AUG-2003 AKK       GEN TO TEST COMPILE ORDER          *ELGCOVER
00088 ******************************************************************ELGCOVER
00089 /                                                                 ELGCOVER
00090  ENVIRONMENT DIVISION.                                            ELGCOVER
00091  DATA DIVISION.                                                   ELGCOVER
00092                                                                   ELGCOVER
00093  WORKING-STORAGE SECTION.                                         ELGCOVER
00094  01  FILLER                              PIC X(26) VALUE          ELGCOVER
00095                                 '*** ELGCOVER WS BEGINS ***'.     ELGCOVER
00096  01  WS-MISC-COUNTERS.                                            ELGCOVER
00097      05  WS-ELCDELII-COUNTER           VALUE +0  PIC S9(3) COMP-3.ELGCOVER
00098      05  WS-DETAIL-LINE-COUNTER        VALUE +0  PIC S9(3) COMP-3.ELGCOVER
00099  01  WS-MISC-FIELDS.                                              ELGCOVER
00100      05  WS-LINE-OF-BUSINESS           VALUE ' ' PIC  X(1).       ELGCOVER
00101          88  WS-BASIC-LINE-OF-BUSINESS VALUE '1'.                 ELGCOVER
00102          88  WS-MM-LINE-OF-BUSINESS    VALUE '2'.                 ELGCOVER
00103      05  WS-INST-PROF-TEST             VALUE ' ' PIC  X(1).       ELGCOVER
00104          88  WS-INSTITUTIONAL-TEST     VALUE 'I'.                 ELGCOVER
00105          88  WS-PROFESSIONAL-TEST      VALUE 'P'.                 ELGCOVER
00106  01  WS-FIELDS-FOR-PVE-PROCESS.                                   ELGCOVER
00107      05  PC-PVE                        PIC X(06)   VALUE '#PVE'.  ELGCOVER
00108                                                                   ELGCOVER
00109  01  WS-ECF-PROVIDER-FOUND-TABLE.                                 ELGCOVER
00110      05  WS-ECF-PROVIDER-ENTRY         OCCURS 2 TIMES             ELGCOVER
00111                                        INDEXED BY WS-ECF-IDX.     ELGCOVER
00112          10  WS-ECF-PROVIDER-CODE-IND  PIC X(01).                 ELGCOVER
00113              88  ECF-PROVIDER-CODE-FOUND           VALUE 'E'.     ELGCOVER
00114                                                                   ELGCOVER
00115  01  WS-END                            PIC X(18)   VALUE          ELGCOVER
00116                                          '*** END OF W/S ***'.    ELGCOVER
00117                                                                   ELGCOVER
00118  LINKAGE SECTION.                                                 ELGCOVER
00119  01  DFHCOMMAREA.                                                 ELGCOVER
00120      COPY ELSCOMMC.                                               ELGCOVER
00121                                                                   ELGCOVER
00122 /  *** CIA  AREA ***                                              ELGCOVER
00123      COPY ELSCIA2C.                                               ELGCOVER
00124 /  *** SSCB AREA ***                                              ELGCOVER
00125      COPY ELSSSCBC.                                               ELGCOVER
00126 /  *** IO PARM AREA ***                                           ELGCOVER
00127      COPY ELSIOPMC.                                               ELGCOVER
00128 /  *** KEY AREA ***                                               ELGCOVER
00129      COPY ELSKEYSC.                                               ELGCOVER
00130 /  *** OUTPUT TEXT AREA ***                                       ELGCOVER
00131      COPY ELSOUTPC.                                               ELGCOVER
00132 /  *** CODE MANUAL INTERFACE ***                                  ELGCOVER
00133      COPY ELSCMIFC.                                               ELGCOVER
00134 /  *** CODE MANUAL DESCRIPTION AREA ***                           ELGCOVER
00135      COPY ELSCMDSC.                                               ELGCOVER
00136 /  *** BENEFIT PROVISION TABLE ***                                ELGCOVER
00137      COPY ELSPRVNC.                                               ELGCOVER
00138 /  *** COMPRESSION TEXT WORK-AREA ***                             ELGCOVER
00139      COPY ELSTCWAC.                                               ELGCOVER
00140 /    B E N E F I T   P R O V I S I O N   T A B L E   O F   F L D SELGCOVER
00141      COPY ELSPLGTB.                                               ELGCOVER
00142 /        G R O U P   S P E C I F I C   R E C O R D                ELGCOVER
00143  01  CONTRACT-RECORD.                                             ELGCOVER
00144      COPY GCCONTRC.                                               ELGCOVER
00145 /                                                                 ELGCOVER
00146  01  PVE-RECORD.                                                  ELGCOVER
00147      COPY GCTPVEC.                                                ELGCOVER
00148 /                                                                 ELGCOVER
00149  01  BENEFIT-PROVISION-RECORD.                                    ELGCOVER
00150      COPY GCBENPVC.                                               ELGCOVER
00151 /                                                                 ELGCOVER
00152  PROCEDURE DIVISION.                                              ELGCOVER
00153                                                                   ELGCOVER
00154 ******************************************************************ELGCOVER
00155 *                                                                *ELGCOVER
00156 *  0000          M A I N L I N E                                 *ELGCOVER
00157 *                                                                *ELGCOVER
00158 ******************************************************************ELGCOVER
00159                                                                   ELGCOVER
00160  0000-000-MAINLINE SECTION.                                       ELGCOVER
00161  0000-010.                                                        ELGCOVER
00162                                                                   ELGCOVER
00163      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELGCOVER
00164          EXEC CICS ABEND                                          ELGCOVER
00165                    ABCODE ('EL01')                                ELGCOVER
00166          END-EXEC                                                 ELGCOVER
00167      END-IF.                                                      ELGCOVER
00168                                                                   ELGCOVER
00169      IF ECA-CIA-PTR = NULL                                        ELGCOVER
00170          EXEC CICS ABEND                                          ELGCOVER
00171                    ABCODE ('EL02')                                ELGCOVER
00172          END-EXEC                                                 ELGCOVER
00173      END-IF.                                                      ELGCOVER
00174                                                                   ELGCOVER
00175      CALL 'ELUINISM' USING DFHCOMMAREA                            ELGCOVER
00176                      ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.    ELGCOVER
00177                                                                   ELGCOVER
00178 ***  ADDRESS DATA AREAS USING PASSED POINTERS  ***                ELGCOVER
00179                                                                   ELGCOVER
00180      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELGCOVER
00181      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCOVER
00182                      ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.      ELGCOVER
00183      IF NOT CIA-RC-OK                                             ELGCOVER
00184          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELGCOVER
00185                                                                   ELGCOVER
00186      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGCOVER
00187      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCOVER
00188                      ADDRESS OF COF-OUTPUT-INTERFACE.             ELGCOVER
00189      IF NOT CIA-RC-OK                                             ELGCOVER
00190          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELGCOVER
00191                                                                   ELGCOVER
00192      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGCOVER
00193      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCOVER
00194                      ADDRESS OF KWA-FILE-KEY-WORK-AREA.           ELGCOVER
00195      IF NOT CIA-RC-OK                                             ELGCOVER
00196          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELGCOVER
00197                                                                   ELGCOVER
00198      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELGCOVER
00199      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCOVER
00200                      ADDRESS OF CMF-CODES-MANUAL-INTERFACE.       ELGCOVER
00201      IF NOT CIA-RC-OK                                             ELGCOVER
00202          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELGCOVER
00203                                                                   ELGCOVER
00204      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELGCOVER
00205      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCOVER
00206                      ADDRESS OF TCAR-COMPRESSION-WORK-AREA.       ELGCOVER
00207      IF NOT CIA-RC-OK                                             ELGCOVER
00208          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELGCOVER
00209                                                                   ELGCOVER
00210      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELGCOVER
00211      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCOVER
00212                      ADDRESS OF PVN-BENEFIT-PROVISION-LIST.       ELGCOVER
00213                                                                   ELGCOVER
00214      INITIALIZE CMF-CODES-MANUAL-INTERFACE                        ELGCOVER
00215                 TCAR-FROM-AREA.                                   ELGCOVER
00216                                                                   ELGCOVER
00217 *------- PROCESS INSTITUTIONAL  INPATIENT BEN PROVISION LIST------ELGCOVER
00218                                                                   ELGCOVER
00219      MOVE ZEROS                    TO WS-ELCDELII-COUNTER.        ELGCOVER
00220      PERFORM 1000-000-ELCDELII-COV-TAB-LOOP.                      ELGCOVER
00221                                                                   ELGCOVER
00222 *------- UPDATE BENEFIT PROVISION LISTS --------------------------ELGCOVER
00223                                                                   ELGCOVER
00224      PERFORM 5000-000-COVERAGE-UPDATE.                            ELGCOVER
00225                                                                   ELGCOVER
00226 *------- PRINT COVERAGE OUTPUT -----------------------------------ELGCOVER
00227                                                                   ELGCOVER
00228      PERFORM 6000-000-COVERAGE-PRINT.                             ELGCOVER
00229                                                                   ELGCOVER
00230      EXEC CICS RETURN END-EXEC.                                   ELGCOVER
00231      GOBACK.                                                      ELGCOVER
00232                                                                   ELGCOVER
00233  0000-900-EXIT.                                                   ELGCOVER
00234      EXIT.                                                        ELGCOVER
00235  0098-SIGNAL-UNALL-AREA-ERROR    SECTION.                         ELGCOVER
00236      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELGCOVER
00237      EXEC CICS ABEND                                              ELGCOVER
00238                ABCODE(CIA-ABCODE)                                 ELGCOVER
00239      END-EXEC.                                                    ELGCOVER
00240  0098-000-EXIT.                                                   ELGCOVER
00241      EXIT.                                                        ELGCOVER
00242 /*****************************************************************ELGCOVER
00243 *                                                                *ELGCOVER
00244 * 1000  INSTITUTIONAL INPATIENT COVERAGE TABLE LOOP              *ELGCOVER
00245 *                                                                *ELGCOVER
00246 *       MATCH THE INSTITUTIONAL INPATIENT LIST OF BENEFIT        *ELGCOVER
00247 *       PROVISIONS AGAINST BLUE CROSS / COMP MAJOR MED / AND     *ELGCOVER
00248 *       SUPP MAJOR CONTRACTS IF AVAILABLE.                       *ELGCOVER
00249 *                                                                *ELGCOVER
00250 ******************************************************************ELGCOVER
00251  1000-000-ELCDELII-COV-TAB-LOOP  SECTION.                         ELGCOVER
00252  1000-010.                                                        ELGCOVER
00253                                                                   ELGCOVER
00254      IF PVN-BEN-FMT (1) = 'A' OR 'B' OR 'W'                       ELGCOVER
00255          SET CIA-ELSCONIB-DDN TO TRUE                             ELGCOVER
00256          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELGCOVER
00257                          ADDRESS OF CONTRACT-RECORD               ELGCOVER
00258          IF CIA-RC-OK                                             ELGCOVER
00259              MOVE '1' TO WS-LINE-OF-BUSINESS                      ELGCOVER
00260              SET WS-INSTITUTIONAL-TEST TO TRUE                    ELGCOVER
00261              PERFORM 1200-000-ELCDELII-LOOP                       ELGCOVER
00262                 VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1             ELGCOVER
00263                   UNTIL PVN-BEN-PROVN-IDX > PVN-NBR-BEN-PROVN     ELGCOVER
00264          END-IF                                                   ELGCOVER
00265          SET CIA-ELSCONIS-DDN TO TRUE                             ELGCOVER
00266          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELGCOVER
00267                          ADDRESS OF CONTRACT-RECORD               ELGCOVER
00268          IF CIA-RC-OK                                             ELGCOVER
00269              MOVE '2' TO WS-LINE-OF-BUSINESS                      ELGCOVER
00270              SET WS-INSTITUTIONAL-TEST TO TRUE                    ELGCOVER
00271              PERFORM 1200-000-ELCDELII-LOOP                       ELGCOVER
00272                 VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1             ELGCOVER
00273                 UNTIL PVN-BEN-PROVN-IDX > PVN-NBR-BEN-PROVN       ELGCOVER
00274          END-IF                                                   ELGCOVER
00275      ELSE                                                         ELGCOVER
00276          SET CIA-ELSCONPB-DDN TO TRUE                             ELGCOVER
00277          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELGCOVER
00278                          ADDRESS OF CONTRACT-RECORD               ELGCOVER
00279          IF CIA-RC-OK                                             ELGCOVER
00280              MOVE '1' TO WS-LINE-OF-BUSINESS                      ELGCOVER
00281              SET WS-PROFESSIONAL-TEST TO TRUE                     ELGCOVER
00282              PERFORM 1200-000-ELCDELII-LOOP                       ELGCOVER
00283                 VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1             ELGCOVER
00284                 UNTIL PVN-BEN-PROVN-IDX > PVN-NBR-BEN-PROVN       ELGCOVER
00285          END-IF                                                   ELGCOVER
00286          SET CIA-ELSCONPS-DDN TO TRUE                             ELGCOVER
00287          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELGCOVER
00288                          ADDRESS OF CONTRACT-RECORD               ELGCOVER
00289          IF CIA-RC-OK                                             ELGCOVER
00290              MOVE '2' TO WS-LINE-OF-BUSINESS                      ELGCOVER
00291              SET WS-PROFESSIONAL-TEST TO TRUE                     ELGCOVER
00292              PERFORM 1200-000-ELCDELII-LOOP                       ELGCOVER
00293                 VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1             ELGCOVER
00294                 UNTIL PVN-BEN-PROVN-IDX > PVN-NBR-BEN-PROVN       ELGCOVER
00295          END-IF                                                   ELGCOVER
00296      END-IF.                                                      ELGCOVER
00297                                                                   ELGCOVER
00298  1000-900-EXIT.                                                   ELGCOVER
00299      EXIT.                                                        ELGCOVER
00300 /*****************************************************************ELGCOVER
00301 *                                                                *ELGCOVER
00302 * 1200  INSTITUTIONAL INPATIENT COVERAGE TABLE LOOP              *ELGCOVER
00303 *       ( PROCESS BLUE CROSS CONTRACT   OR                       *ELGCOVER
00304 *                 MAJOR MED  CONTRACT )                          *ELGCOVER
00305 *                                                                *ELGCOVER
00306 ******************************************************************ELGCOVER
00307  1200-000-ELCDELII-LOOP       SECTION.                            ELGCOVER
00308  1200-010.                                                        ELGCOVER
00309                                                                   ELGCOVER
00310      PERFORM 1250-000-ELCDELII-GCT-LOOP                           ELGCOVER
00311         VARYING GCT-INDEX FROM 1 BY 1                             ELGCOVER
00312           UNTIL GCT-INDEX > GCT-COUNT-BEN-PROVN-POINTERS.         ELGCOVER
00313                                                                   ELGCOVER
00314  1200-900-EXIT.                                                   ELGCOVER
00315      EXIT.                                                        ELGCOVER
00316 /*****************************************************************ELGCOVER
00317 *                                                                *ELGCOVER
00318 * 1250  INSTITUTIONAL INPATIENT COVERAGE TABLE LOOP              *ELGCOVER
00319 *       ( PROCESS BLUE CROSS CONTRACT )                          *ELGCOVER
00320 *                                                                *ELGCOVER
00321 ******************************************************************ELGCOVER
00322  1250-000-ELCDELII-GCT-LOOP      SECTION.                         ELGCOVER
00323  1250-010.                                                        ELGCOVER
00324                                                                   ELGCOVER
00325      IF  PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX) =                    ELGCOVER
00326          GCT-BEN-PROVN-ID(GCT-INDEX)         AND                  ELGCOVER
00327          GCT-BEN-PROVN-SLOT-NO (GCT-INDEX) > +0                   ELGCOVER
00328          IF  WS-BASIC-LINE-OF-BUSINESS                            ELGCOVER
00329              PERFORM 2000-000-INSERT-BASIC-SLOT                   ELGCOVER
00330          ELSE                                                     ELGCOVER
00331              PERFORM 3000-000-INSERT-SUPP-SLOT                    ELGCOVER
00332          END-IF                                                   ELGCOVER
00333      END-IF.                                                      ELGCOVER
00334                                                                   ELGCOVER
00335  1250-900-EXIT.                                                   ELGCOVER
00336      EXIT.                                                        ELGCOVER
00337 /*****************************************************************ELGCOVER
00338 *                                                                *ELGCOVER
00339 * 2000  INSERT BASIC SLOT                                        *ELGCOVER
00340 *                                                                *ELGCOVER
00341 *       THIS WILL INSERT THE BASIC SLOT NUMBER FOUND ON THE      *ELGCOVER
00342 *       CONTRACT INTO THE COPYBOOK 'ELSPRVNC'. IT WILL ALSO      *ELGCOVER
00343 *       CHECK TO SEE IF FURTHER PROCESSING IS DESIRED WHEN       *ELGCOVER
00344 *       DEALING WITH THE ECF TOPIC. IF SO IT WILL READ IN        *ELGCOVER
00345 *       THE BENEFIT PROVISION RECORD TO SEARCH FOR A #PVE        *ELGCOVER
00346 *       TABULAR CODED.                                           *ELGCOVER
00347 *                                                                *ELGCOVER
00348 ******************************************************************ELGCOVER
00349  2000-000-INSERT-BASIC-SLOT      SECTION.                         ELGCOVER
00350      MOVE GCT-BEN-PROVN-SLOT-NO(GCT-INDEX)     TO                 ELGCOVER
00351           PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)                     ELGCOVER
00352      IF  PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) > +0                 ELGCOVER
00353             NEXT SENTENCE                                         ELGCOVER
00354      ELSE                                                         ELGCOVER
00355          IF FIND-ECF-PROV-ON-PVE                                  ELGCOVER
00356             MOVE PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) TO          ELGCOVER
00357                  KWA-GCP-PROVN-SLOT-NO                            ELGCOVER
00358             PERFORM 2100-000-READ-IN-BP-RECORD                    ELGCOVER
00359          ELSE                                                     ELGCOVER
00360             ADD +1 TO WS-ELCDELII-COUNTER.                        ELGCOVER
00361  2000-900-EXIT.                                                   ELGCOVER
00362      EXIT.                                                        ELGCOVER
00363 /*****************************************************************ELGCOVER
00364 *                                                                *ELGCOVER
00365 * 2100  READ IN BP RECORD                                        *ELGCOVER
00366 *                                                                *ELGCOVER
00367 *       THIS WILL READ IN THE BENEFIT PROVISION RECORDS THAT     *ELGCOVER
00368 *       ARE INVOLVED WITH THE ECF TOPIC. IF THE READ WAS         *ELGCOVER
00369 *       SUCCESSFUL IT WILL SEARCH THE RECORD TO SEE IF A         *ELGCOVER
00370 *       #PVE TABULAR IS ATTACHED.                                *ELGCOVER
00371 *                                                                *ELGCOVER
00372 ******************************************************************ELGCOVER
00373  2100-000-READ-IN-BP-RECORD      SECTION.                         ELGCOVER
00374      MOVE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)   TO              ELGCOVER
00375           KWA-GCP-PROVN-ID.                                       ELGCOVER
00376      SET CIA-GCBENPRV-DDN TO TRUE.                                ELGCOVER
00377      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCOVER
00378                      ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.      ELGCOVER
00379      IF NOT CIA-RC-OK                                             ELGCOVER
00380           PERFORM 2200-000-OBTAIN-STORAGE                         ELGCOVER
00381           SET CIA-GCBENPRV-DDN TO TRUE                            ELGCOVER
00382           CALL 'ELUSETAD' USING DFHCOMMAREA                       ELGCOVER
00383                           ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS  ELGCOVER
00384      END-IF.                                                      ELGCOVER
00385      MOVE KWA-GCBENPRV-KEY TO IOP-FILE-KEY.                       ELGCOVER
00386      PERFORM 2300-000-SETUP-AND-LINK-IOPGM.                       ELGCOVER
00387      IF NOT IOP-RC-OK                                             ELGCOVER
00388         SET  CIA-AB-NOTFND-GCBENPRV               TO TRUE         ELGCOVER
00389         EXEC CICS  ABEND ABCODE(CIA-ABCODE)  END-EXEC             ELGCOVER
00390      ELSE                                                         ELGCOVER
00391         SET ADDRESS OF BENEFIT-PROVISION-RECORD   TO              ELGCOVER
00392             IOP-REC-PTR.                                          ELGCOVER
00393      PERFORM 2400-000-SCAN-FOR-PVE-TABULAR.                       ELGCOVER
00394   2100-900-EXIT.                                                  ELGCOVER
00395       EXIT.                                                       ELGCOVER
00396 /*****************************************************************ELGCOVER
00397 *                                                                *ELGCOVER
00398 * 2200  OBTAIN STORAGE                                           *ELGCOVER
00399 *                                                                *ELGCOVER
00400 *       THIS WILL OBTAIN STORAGE AREA FOR THE DDNAME THAT        *ELGCOVER
00401 *       IS GIVEN BEFORE EXECUTING THIS PARAGRAPH.                *ELGCOVER
00402 *                                                                *ELGCOVER
00403 ******************************************************************ELGCOVER
00404  2200-000-OBTAIN-STORAGE         SECTION.                         ELGCOVER
00405      SET  CIA-STG-GETMAIN     TO TRUE.                            ELGCOVER
00406      EXEC CICS LINK                                               ELGCOVER
00407                PROGRAM ('ELUSTGMG')                               ELGCOVER
00408                COMMAREA (DFHCOMMAREA)                             ELGCOVER
00409      END-EXEC.                                                    ELGCOVER
00410  2200-900-EXIT.                                                   ELGCOVER
00411      EXIT.                                                        ELGCOVER
00412 /*****************************************************************ELGCOVER
00413 *                                                                *ELGCOVER
00414 * 2300  SETUP AND LINK IOPGM                                     *ELGCOVER
00415 *                                                                *ELGCOVER
00416 *       THIS WILL SETUP ALL COMMON NECESSARY IO PARAMETERS       *ELGCOVER
00417 *       IN ORDER TO READ WHATEVER RECORD WAS REQUESTED.          *ELGCOVER
00418 *       THEN IT WILL LINK TO THE IO MODULE ITSELF.               *ELGCOVER
00419 *                                                                *ELGCOVER
00420 ******************************************************************ELGCOVER
00421  2300-000-SETUP-AND-LINK-IOPGM   SECTION.                         ELGCOVER
00422      SET  IOP-RD                 TO TRUE.                         ELGCOVER
00423      SET  IOP-FCQ-NONE           TO TRUE.                         ELGCOVER
00424      SET  IOP-KVQ-EQ             TO TRUE.                         ELGCOVER
00425      SET  IOP-STG-MODE-LOCATE    TO TRUE.                         ELGCOVER
00426      MOVE SPACES                 TO IOP-AIX-DDNAME.               ELGCOVER
00427      EXEC CICS LINK                                               ELGCOVER
00428                PROGRAM ('ELUIOPGM')                               ELGCOVER
00429                COMMAREA (DFHCOMMAREA)                             ELGCOVER
00430      END-EXEC.                                                    ELGCOVER
00431  2300-900-EXIT.                                                   ELGCOVER
00432      EXIT.                                                        ELGCOVER
00433 /*****************************************************************ELGCOVER
00434 *                                                                *ELGCOVER
00435 * 2400  SCAN FOR PVE TABULAR                                     *ELGCOVER
00436 *                                                                *ELGCOVER
00437 ******************************************************************ELGCOVER
00438  2400-000-SCAN-FOR-PVE-TABULAR   SECTION.                         ELGCOVER
00439      PERFORM 2500-SEARCH-FOR-PVE                                  ELGCOVER
00440              VARYING GCP-INDEX FROM 1 BY 1                        ELGCOVER
00441              UNTIL   GCP-INDEX = GCP-COUNT-TAB-PROVN-POINTERS     ELGCOVER
00442              OR      GCP-BP-ID (GCP-INDEX) > PC-PVE.              ELGCOVER
00443  2400-900-EXIT.                                                   ELGCOVER
00444      EXIT.                                                        ELGCOVER
00445 /*****************************************************************ELGCOVER
00446 *                                                                *ELGCOVER
00447 * 2500  SEARCH FOR PVE                                           *ELGCOVER
00448 *                                                                *ELGCOVER
00449 *       LOOKING ON BENEFIT PROVISION RECORD FOR A #PVE TABULAR   *ELGCOVER
00450 *                                                                *ELGCOVER
00451 ******************************************************************ELGCOVER
00452  2500-SEARCH-FOR-PVE             SECTION.                         ELGCOVER
00453      IF GCP-BP-ID       (GCP-INDEX) EQUAL PC-PVE AND              ELGCOVER
00454         GCP-BP-SLOT-NO  (GCP-INDEX) > +0                          ELGCOVER
00455         PERFORM 2600-PROCESS-PVE-TABULAR                          ELGCOVER
00456      ELSE                                                         ELGCOVER
00457         NEXT SENTENCE.                                            ELGCOVER
00458  2500-900-EXIT.                                                   ELGCOVER
00459      EXIT.                                                        ELGCOVER
00460 /*****************************************************************ELGCOVER
00461 *                                                                *ELGCOVER
00462 * 2600  PROCESS PVE TABULAR                                      *ELGCOVER
00463 *                                                                *ELGCOVER
00464 *       THIS WILL READ THE #PVE TABULAR FOUND AND DETERMINE      *ELGCOVER
00465 *       IF THERE ARE ANY ECF PROVIDER CODES PRESENT. IT WILL     *ELGCOVER
00466 *       THEN BE CONSIDERED AS COVERED IF ANY OF THOSE CODES      *ELGCOVER
00467 *       ARE FOUND.                                               *ELGCOVER
00468 *                                                                *ELGCOVER
00469 ******************************************************************ELGCOVER
00470  2600-PROCESS-PVE-TABULAR        SECTION.                         ELGCOVER
00471      PERFORM 2700-READ-PVE-TABULAR.                               ELGCOVER
00472      PERFORM 2800-SCAN-FOR-ECF-PROVIDERS.                         ELGCOVER
00473  2600-900-EXIT.                                                   ELGCOVER
00474      EXIT.                                                        ELGCOVER
00475 /*****************************************************************ELGCOVER
00476 *                                                                *ELGCOVER
00477 * 2700  READ PVE TABULAR                                         *ELGCOVER
00478 *                                                                *ELGCOVER
00479 ******************************************************************ELGCOVER
00480  2700-READ-PVE-TABULAR           SECTION.                         ELGCOVER
00481      SET CIA-GCTABULR-DDN TO TRUE.                                ELGCOVER
00482      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCOVER
00483                      ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.      ELGCOVER
00484      IF NOT CIA-RC-OK                                             ELGCOVER
00485           PERFORM 2200-000-OBTAIN-STORAGE                         ELGCOVER
00486           SET CIA-GCTABULR-DDN TO TRUE                            ELGCOVER
00487           CALL 'ELUSETAD' USING DFHCOMMAREA                       ELGCOVER
00488                           ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS  ELGCOVER
00489      END-IF.                                                      ELGCOVER
00490      MOVE PC-PVE                      TO KWA-PROVISION-ID.        ELGCOVER
00491      MOVE GCP-BP-SLOT-NO (GCP-INDEX)  TO KWA-PROVISION-SLOT-NO.   ELGCOVER
00492      MOVE KWA-GCTABULR-KEY            TO IOP-FILE-KEY.            ELGCOVER
00493      PERFORM 2300-000-SETUP-AND-LINK-IOPGM.                       ELGCOVER
00494      IF NOT IOP-RC-OK                                             ELGCOVER
00495         SET  CIA-AB-NOTFND-GCTABULR   TO TRUE                     ELGCOVER
00496         EXEC CICS  ABEND ABCODE(CIA-ABCODE)  END-EXEC             ELGCOVER
00497      ELSE                                                         ELGCOVER
00498         SET ADDRESS OF PVE-RECORD     TO IOP-REC-PTR.             ELGCOVER
00499  2700-900-EXIT.                                                   ELGCOVER
00500      EXIT.                                                        ELGCOVER
00501 /*****************************************************************ELGCOVER
00502 *                                                                *ELGCOVER
00503 * 2800  SCAN FOR ECF PROVIDERS                                   *ELGCOVER
00504 *                                                                *ELGCOVER
00505 *       THIS WILL SEARCH THE #PVE RECORD AND DETERMINE IF IT     *ELGCOVER
00506 *       HAS AN ECF PROVIDER CODE ('0O, 0Q) ON IT. IF SO THEN     *ELGCOVER
00507 *       THE BENEFIT PROVISION IS CONSIDERED COVERED AND WILL     *ELGCOVER
00508 *       INDICATE THE OCCURENCE IT WAS ON.                        *ELGCOVER
00509 *                                                                *ELGCOVER
00510 ******************************************************************ELGCOVER
00511  2800-SCAN-FOR-ECF-PROVIDERS     SECTION.                         ELGCOVER
00512      SET WS-ECF-IDX              TO PVN-BEN-PROVN-IDX.            ELGCOVER
00513      INITIALIZE WS-ECF-PROVIDER-CODE-IND (WS-ECF-IDX).            ELGCOVER
00514      PERFORM 2900-SEARCH-FOR-ECF-CODES                            ELGCOVER
00515              VARYING GBJ-INDEX FROM 1 BY 1                        ELGCOVER
00516              UNTIL   GBJ-INDEX = GBJ-ENTRY-COUNT                  ELGCOVER
00517              OR      ECF-PROVIDER-CODE-FOUND (WS-ECF-IDX).        ELGCOVER
00518  2800-900-EXIT.                                                   ELGCOVER
00519      EXIT.                                                        ELGCOVER
00520 /*****************************************************************ELGCOVER
00521 *                                                                *ELGCOVER
00522 * 2900  SEARCH FOR ECF CODES                                     *ELGCOVER
00523 *                                                                *ELGCOVER
00524 ******************************************************************ELGCOVER
00525  2900-SEARCH-FOR-ECF-CODES       SECTION.                         ELGCOVER
00526      IF GBJ-PROVIDER-CODE (GBJ-INDEX) EQUAL '0O' OR '0Q'          ELGCOVER
00527         SET ECF-PROVIDER-CODE-FOUND (WS-ECF-IDX) TO TRUE          ELGCOVER
00528         ADD +1                         TO WS-ELCDELII-COUNTER     ELGCOVER
00529      ELSE                                                         ELGCOVER
00530         NEXT SENTENCE.                                            ELGCOVER
00531  2900-900-EXIT.                                                   ELGCOVER
00532      EXIT.                                                        ELGCOVER
00533 /*****************************************************************ELGCOVER
00534 *                                                                *ELGCOVER
00535 * 3000  INSERT SUPP SLOT                                         *ELGCOVER
00536 *                                                                *ELGCOVER
00537 *       THIS WILL INSERT THE SUPP SLOT NUMBER FOUND ON THE       *ELGCOVER
00538 *       CONTRACT INTO THE COPYBOOK 'ELSPRVNC'. IT WILL ALSO      *ELGCOVER
00539 *       CHECK TO SEE IF FURTHER PROCESSING IS DESIRED WHEN       *ELGCOVER
00540 *       DEALING WITH THE ECF TOPIC. IF SO IT WILL READ IN        *ELGCOVER
00541 *       THE BENEFIT PROVISION RECORD TO SEARCH FOR A #PVE        *ELGCOVER
00542 *       TABULAR CODED.                                           *ELGCOVER
00543 *                                                                *ELGCOVER
00544 ******************************************************************ELGCOVER
00545  3000-000-INSERT-SUPP-SLOT       SECTION.                         ELGCOVER
00546      MOVE GCT-BEN-PROVN-SLOT-NO (GCT-INDEX)     TO                ELGCOVER
00547           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)                    ELGCOVER
00548      IF  PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX) > +0               ELGCOVER
00549             NEXT SENTENCE                                         ELGCOVER
00550      ELSE                                                         ELGCOVER
00551          IF FIND-ECF-PROV-ON-PVE                                  ELGCOVER
00552             MOVE PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) TO          ELGCOVER
00553                  KWA-GCP-PROVN-SLOT-NO                            ELGCOVER
00554             PERFORM 2100-000-READ-IN-BP-RECORD                    ELGCOVER
00555          ELSE                                                     ELGCOVER
00556             ADD +1 TO WS-ELCDELII-COUNTER.                        ELGCOVER
00557  3000-900-EXIT.                                                   ELGCOVER
00558      EXIT.                                                        ELGCOVER
00559 /*****************************************************************ELGCOVER
00560 *                                                                *ELGCOVER
00561 * 5000  COVERAGE UPDATE                                          *ELGCOVER
00562 *                                                                *ELGCOVER
00563 *       UPDATE PROVISION LISTS AS \
00564 *       COVERED\
00565 *       OF MATCHING THE LISTS AGAINST THE AVAILABLE CONTRACTS.   *ELGCOVER
00566 *                                                                *ELGCOVER
00567 ******************************************************************ELGCOVER
00568  5000-000-COVERAGE-UPDATE        SECTION.                         ELGCOVER
00569  5000-010.                                                        ELGCOVER
00570                                                                   ELGCOVER
00571      IF WS-ELCDELII-COUNTER = +0                                  ELGCOVER
00572              MOVE 'N'  TO  PVN-COVERAGE-IND                       ELGCOVER
00573          ELSE                                                     ELGCOVER
00574              IF  WS-ELCDELII-COUNTER = PVN-NBR-BEN-PROVN          ELGCOVER
00575                  MOVE 'F'  TO  PVN-COVERAGE-IND                   ELGCOVER
00576              ELSE                                                 ELGCOVER
00577                  MOVE 'P'  TO  PVN-COVERAGE-IND.                  ELGCOVER
00578                                                                   ELGCOVER
00579                                                                   ELGCOVER
00580  5000-900-EXIT.                                                   ELGCOVER
00581      EXIT.                                                        ELGCOVER
00582 /*****************************************************************ELGCOVER
00583 *                                                                *ELGCOVER
00584 * 6000  COVERAGE PRINT                                           *ELGCOVER
00585 *                                                                *ELGCOVER
00586 *       PRINT COVERAGE MESSAGES:                                 *ELGCOVER
00587 *        1. NO COVERAGE                                          *ELGCOVER
00588 *        2. FULL COVERAGE                                        *ELGCOVER
00589 *        3. PARTIAL COVERAGE                                     *ELGCOVER
00590 *                                                                *ELGCOVER
00591 ******************************************************************ELGCOVER
00592  6000-000-COVERAGE-PRINT         SECTION.                         ELGCOVER
00593  6000-010.                                                        ELGCOVER
00594                                                                   ELGCOVER
00595      MOVE COF-NBR-DTL-LINES TO WS-DETAIL-LINE-COUNTER.            ELGCOVER
00596                                                                   ELGCOVER
00597      IF WS-DETAIL-LINE-COUNTER = +20                              ELGCOVER
00598          PERFORM 8100-000-LINK-TO-ELUOUTPT                        ELGCOVER
00599          MOVE +0 TO WS-DETAIL-LINE-COUNTER                        ELGCOVER
00600      END-IF.                                                      ELGCOVER
00601 *------- PROCESS INSTITUTIONAL  INPATIENT ------------------------ELGCOVER
00602                                                                   ELGCOVER
00603      IF  PVN-COVERAGE-IND = 'N'                                   ELGCOVER
00604              STRING SSB-TOPIC-PHRASE DELIMITED BY ':'             ELGCOVER
00605                     ' NOT COVERED.'   DELIMITED BY SIZE           ELGCOVER
00606                     INTO TCAR-FROM-AREA                           ELGCOVER
00607              PERFORM TCPR-000-TEXT-COMPRESSION                    ELGCOVER
00608              ADD  +1 TO WS-DETAIL-LINE-COUNTER                    ELGCOVER
00609              MOVE LOW-VALUES                                      ELGCOVER
00610                TO COF-DTL-LINE (WS-DETAIL-LINE-COUNTER)           ELGCOVER
00611              IF WS-DETAIL-LINE-COUNTER = +20                      ELGCOVER
00612                  PERFORM 8100-000-LINK-TO-ELUOUTPT                ELGCOVER
00613                  MOVE +0 TO WS-DETAIL-LINE-COUNTER                ELGCOVER
00614              END-IF                                               ELGCOVER
00615              ADD  +1 TO WS-DETAIL-LINE-COUNTER                    ELGCOVER
00616              MOVE TCAR-TO-AREA                                    ELGCOVER
00617                TO COF-DTL-LINE (WS-DETAIL-LINE-COUNTER)           ELGCOVER
00618              IF WS-DETAIL-LINE-COUNTER = +20                      ELGCOVER
00619                  PERFORM 8100-000-LINK-TO-ELUOUTPT                ELGCOVER
00620                  MOVE +0 TO WS-DETAIL-LINE-COUNTER                ELGCOVER
00621              END-IF                                               ELGCOVER
00622          ELSE                                                     ELGCOVER
00623              IF  PVN-COVERAGE-IND = 'F'                           ELGCOVER
00624                  STRING SSB-TOPIC-PHRASE DELIMITED BY ':'         ELGCOVER
00625                         ' COVERED.'      DELIMITED BY SIZE        ELGCOVER
00626                     INTO TCAR-FROM-AREA                           ELGCOVER
00627                  PERFORM TCPR-000-TEXT-COMPRESSION                ELGCOVER
00628                  ADD  +1 TO WS-DETAIL-LINE-COUNTER                ELGCOVER
00629                  MOVE LOW-VALUES                                  ELGCOVER
00630                    TO COF-DTL-LINE (WS-DETAIL-LINE-COUNTER)       ELGCOVER
00631                  IF WS-DETAIL-LINE-COUNTER = +20                  ELGCOVER
00632                      PERFORM 8100-000-LINK-TO-ELUOUTPT            ELGCOVER
00633                      MOVE +0 TO WS-DETAIL-LINE-COUNTER            ELGCOVER
00634                  END-IF                                           ELGCOVER
00635                  ADD  +1 TO WS-DETAIL-LINE-COUNTER                ELGCOVER
00636                  MOVE TCAR-TO-AREA                                ELGCOVER
00637                    TO COF-DTL-LINE (WS-DETAIL-LINE-COUNTER)       ELGCOVER
00638                  IF WS-DETAIL-LINE-COUNTER = +20                  ELGCOVER
00639                      PERFORM 8100-000-LINK-TO-ELUOUTPT            ELGCOVER
00640                      MOVE +0 TO WS-DETAIL-LINE-COUNTER            ELGCOVER
00641                  END-IF                                           ELGCOVER
00642              ELSE                                                 ELGCOVER
00643                  IF  PVN-COVERAGE-IND = 'P'                       ELGCOVER
00644                      STRING SSB-TOPIC-PHRASE DELIMITED BY ':'     ELGCOVER
00645                            ' NOT COVERED: '  DELIMITED BY SIZE    ELGCOVER
00646                             INTO TCAR-FROM-AREA                   ELGCOVER
00647                      PERFORM TCPR-000-TEXT-COMPRESSION            ELGCOVER
00648                      ADD  +1 TO WS-DETAIL-LINE-COUNTER            ELGCOVER
00649                      MOVE LOW-VALUES                              ELGCOVER
00650                        TO COF-DTL-LINE (WS-DETAIL-LINE-COUNTER)   ELGCOVER
00651                      IF WS-DETAIL-LINE-COUNTER = +20              ELGCOVER
00652                           PERFORM 8100-000-LINK-TO-ELUOUTPT       ELGCOVER
00653                           MOVE +0 TO WS-DETAIL-LINE-COUNTER       ELGCOVER
00654                      END-IF                                       ELGCOVER
00655                      ADD  +1 TO WS-DETAIL-LINE-COUNTER            ELGCOVER
00656                      MOVE TCAR-TO-AREA                            ELGCOVER
00657                        TO COF-DTL-LINE (WS-DETAIL-LINE-COUNTER)   ELGCOVER
00658                      IF WS-DETAIL-LINE-COUNTER = +20              ELGCOVER
00659                           PERFORM 8100-000-LINK-TO-ELUOUTPT       ELGCOVER
00660                           MOVE +0 TO WS-DETAIL-LINE-COUNTER       ELGCOVER
00661                      END-IF                                       ELGCOVER
00662                      PERFORM 6100-000-ELII-PARTIAL-COVERAGE       ELGCOVER
00663                         VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1     ELGCOVER
00664                           UNTIL PVN-BEN-PROVN-IDX >               ELGCOVER
00665                                 PVN-NBR-BEN-PROVN                 ELGCOVER
00666                      ADD +1 TO WS-DETAIL-LINE-COUNTER             ELGCOVER
00667                      MOVE ALL '* ' TO                             ELGCOVER
00668                            COF-DTL-LINE (WS-DETAIL-LINE-COUNTER)  ELGCOVER
00669                      IF WS-DETAIL-LINE-COUNTER = +20              ELGCOVER
00670                           PERFORM 8100-000-LINK-TO-ELUOUTPT       ELGCOVER
00671                           MOVE +0 TO WS-DETAIL-LINE-COUNTER       ELGCOVER
00672                      END-IF                                       ELGCOVER
00673                  ELSE                                             ELGCOVER
00674                      NEXT SENTENCE.                               ELGCOVER
00675                                                                   ELGCOVER
00676                                                                   ELGCOVER
00677      MOVE WS-DETAIL-LINE-COUNTER TO COF-NBR-DTL-LINES.            ELGCOVER
00678                                                                   ELGCOVER
00679  6000-900-EXIT.                                                   ELGCOVER
00680      EXIT.                                                        ELGCOVER
00681 /*****************************************************************ELGCOVER
00682 *                                                                *ELGCOVER
00683 *  6100   ELII PARTIAL COVERAGE                                  *ELGCOVER
00684 *                                                                *ELGCOVER
00685 *         PRINT INSTITUTIONAL INPATIENT PARTIAL COVERAGE         *ELGCOVER
00686 *                                                                *ELGCOVER
00687 ******************************************************************ELGCOVER
00688  6100-000-ELII-PARTIAL-COVERAGE  SECTION.                         ELGCOVER
00689  6100-010.                                                        ELGCOVER
00690                                                                   ELGCOVER
00691      IF  FIND-ECF-PROV-ON-PVE                    AND              ELGCOVER
00692          ECF-PROVIDER-CODE-FOUND (PVN-BEN-PROVN-IDX)              ELGCOVER
00693          GO TO 6100-900-EXIT                                      ELGCOVER
00694      ELSE                                                         ELGCOVER
00695         IF (PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) > +0   OR         ELGCOVER
00696             PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) > +0) AND        ELGCOVER
00697             NOT FIND-ECF-PROV-ON-PVE                              ELGCOVER
00698             GO TO 6100-900-EXIT                                   ELGCOVER
00699         ELSE                                                      ELGCOVER
00700             NEXT SENTENCE.                                        ELGCOVER
00701                                                                   ELGCOVER
00702      ADD  +1 TO WS-DETAIL-LINE-COUNTER.                           ELGCOVER
00703      MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                          ELGCOVER
00704        TO CMF-CODE-VALUE.                                         ELGCOVER
00705      PERFORM 8000-000-LINK-TO-ELCODESM.                           ELGCOVER
00706      STRING '     ' CMF-DESCR-LINE(1)                             ELGCOVER
00707             DELIMITED BY SIZE                                     ELGCOVER
00708             INTO COF-DTL-LINE (WS-DETAIL-LINE-COUNTER).           ELGCOVER
00709                                                                   ELGCOVER
00710      IF  WS-DETAIL-LINE-COUNTER = +20                             ELGCOVER
00711          PERFORM 8100-000-LINK-TO-ELUOUTPT                        ELGCOVER
00712          MOVE +0 TO WS-DETAIL-LINE-COUNTER.                       ELGCOVER
00713                                                                   ELGCOVER
00714  6100-900-EXIT.                                                   ELGCOVER
00715      EXIT.                                                        ELGCOVER
00716 /*****************************************************************ELGCOVER
00717 *                                                                *ELGCOVER
00718 *  8000   LINK TO ELCODESM (CODES MANUAL ACCESS MODULE)          *ELGCOVER
00719 *                                                                *ELGCOVER
00720 ******************************************************************ELGCOVER
00721  8000-000-LINK-TO-ELCODESM      SECTION.                          ELGCOVER
00722  8000-010.                                                        ELGCOVER
00723                                                                   ELGCOVER
00724      MOVE 'BP      '    TO CMF-RECORD-PREFIX.                     ELGCOVER
00725      MOVE 'BEN-PR-ID  ' TO CMF-ELEMENT-SYSTEM-NAME.               ELGCOVER
00726                                                                   ELGCOVER
00727      EXEC CICS LINK PROGRAM ('ELUCMIF')                           ELGCOVER
00728                     COMMAREA(DFHCOMMAREA)                         ELGCOVER
00729                     END-EXEC.                                     ELGCOVER
00730                                                                   ELGCOVER
00731      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGCOVER
00732      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCOVER
00733                      ADDRESS OF CMF-DESCR.                        ELGCOVER
00734                                                                   ELGCOVER
00735  8000-900-EXIT.                                                   ELGCOVER
00736      EXIT.                                                        ELGCOVER
00737 /*****************************************************************ELGCOVER
00738 *                                                                *ELGCOVER
00739 *  8100   LINK TO ELUOUTPT (PAGE FILE WRITE MODULE)              *ELGCOVER
00740 *                                                                *ELGCOVER
00741 ******************************************************************ELGCOVER
00742  8100-000-LINK-TO-ELUOUTPT      SECTION.                          ELGCOVER
00743  8100-010.                                                        ELGCOVER
00744                                                                   ELGCOVER
00745      MOVE WS-DETAIL-LINE-COUNTER TO COF-NBR-DTL-LINES.            ELGCOVER
00746                                                                   ELGCOVER
00747      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELGCOVER
00748                     COMMAREA(DFHCOMMAREA)                         ELGCOVER
00749                     END-EXEC.                                     ELGCOVER
00750                                                                   ELGCOVER
00751      MOVE ZEROES     TO COF-NBR-HDR-LINES                         ELGCOVER
00752                         COF-NBR-DTL-LINES.                        ELGCOVER
00753  8100-900-EXIT.                                                   ELGCOVER
00754      EXIT.                                                        ELGCOVER
00755                                                                   ELGCOVER
00756  9999-000-DUMMEY                SECTION.                          ELGCOVER
00757      COPY ELSTCOMP.                                               ELGCOVER
