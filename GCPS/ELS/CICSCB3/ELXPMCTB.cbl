00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELXPMCTB
00003  PROGRAM-ID.         ELXPMCTB.                                       LV004
00004                                                                   ELXPMCTB
00005  AUTHOR.             BARBARA KEIB                                 ELXPMCTB
00006                                                                   ELXPMCTB
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELXPMCTB
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELXPMCTB
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELXPMCTB
00010                      233 N. MICHIGAN AVE                          ELXPMCTB
00011                      CHICAGO, ILLINOIS 60601                      ELXPMCTB
00012                                                                   ELXPMCTB
00013                                                                   ELXPMCTB
00014  DATE-WRITTEN.       19-AUG-1992.                                 ELXPMCTB
00015                                                                   ELXPMCTB
00016  DATE-COMPILED.                                                   ELXPMCTB
00017                                                                   ELXPMCTB
00018  SECURITY.           COPYRIGHT 1986,                              ELXPMCTB
00019                      HEALTH CARE SERVICE CORPORATION              ELXPMCTB
00020      SKIP3                                                        ELXPMCTB
00021  ENVIRONMENT DIVISION.                                            ELXPMCTB
00022                                                                   ELXPMCTB
00023  CONFIGURATION SECTION.                                           ELXPMCTB
00024  SOURCE-COMPUTER.    IBM-3033.                                    ELXPMCTB
00025  OBJECT-COMPUTER.    IBM-3033.                                    ELXPMCTB
00026      EJECT                                                        ELXPMCTB
00027 ******************************************************************ELXPMCTB
00028 *AKK 12/06/05 REGEN FOR TEST                                     *ELXPMCTB
00029 *      READ TABULAR RECORDS TO DERIVE THE PROVIDER CONTROL       *ELXPMCTB
00030 *      FOR CONTRACT RECORD SELECTION.                            *ELXPMCTB
00031 *                                                                *ELXPMCTB
00032 *                                                                *ELXPMCTB
00033 ******************************************************************ELXPMCTB
00034 *                      MAINTENANCE HISTORY                       *ELXPMCTB
00035 *                                                                *ELXPMCTB
00036 *  MOD     DATE     BY  DRPT                ACTION               *ELXPMCTB
00037 * ----- ----------- --- ----- ---------------------------------- *ELXPMCTB
00038 * 01.00 18-AUG-1992 BAK       CREATED                            *ELXPMCTB
00039 * 02.00 03-AUG-1993 BAK       CHANGE TO NEW TABULAR FILE FOR     *ELXPMCTB
00040 *                             GVL'S ISR-13184                    *ELXPMCTB
00041 * 03.00 20-SEP-1993 BAK       MOVE 2 DIGIT INDICATORS FROM GVL TO*ELXPMCTB
00042 *                             CALLING PROGRAM ELXPMCGC.          *ELXPMCTB
00043 * 04.00 06-AUG-1998 AAK       UPDATES FOR YEAR 2000              *ELXPMCTB
00044 * 04.01 01-APR-2003 AAK       MORE CHANGES DUE TO ENDEVOR        *ELXPMCTB
00045 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELXPMCTB
00046 *                                                                *ELXPMCTB
00047 * 02.01 09-JAN-2004 AKK S0C7 INTERTEST                           *ELXPMCTB
00048 * 02.02 23-JAN-2004 AKK RECOMPILE AFTER COMPILER FIX             *ELXPMCTB
00049 *                                                                *ELXPMCTB
00050 *                                                                *ELXPMCTB
00051 ******************************************************************ELXPMCTB
00052                                                                   ELXPMCTB
00053      EJECT                                                        ELXPMCTB
00054  DATA DIVISION.                                                   ELXPMCTB
00055  WORKING-STORAGE SECTION.                                         ELXPMCTB
00056  01  FILLER                     PICTURE X(32)                     ELXPMCTB
00057           VALUE '****ELXPMCTB WORKING STORAGE****'.               ELXPMCTB
00058                                                                   ELXPMCTB
00059  01  WS-SWITCHES.                                                 ELXPMCTB
00060    05  WS-TAB-CONTROL             PIC X(01) VALUE 'N'.            ELXPMCTB
00061      88  TAB-FOUND                        VALUE 'Y'.              ELXPMCTB
00062      88  TAB-NOT-FOUND                    VALUE 'N'.              ELXPMCTB
00063                                                                   ELXPMCTB
00064    05  WS-GVL-TAB-KEY-CONTROL     PIC X(01) VALUE 'N'.            ELXPMCTB
00065      88  TAB-KEY-FOUND                    VALUE 'Y'.              ELXPMCTB
00066      88  TAB-KEY-NOT-FOUND                VALUE 'N'.              ELXPMCTB
00067                                                                   ELXPMCTB
00068    05  WS-PROVIDER-CONTROL        PIC X(01) VALUE 'N'.            ELXPMCTB
00069      88  ENTRY-FOUND                      VALUE 'Y'.              ELXPMCTB
00070      88  ENTRY-NOT-FOUND                  VALUE 'N'.              ELXPMCTB
00071                                                                   ELXPMCTB
00072    05  WS-TAB-END-IND             PIC X(01) VALUE 'N'.            ELXPMCTB
00073      88  END-TAB                          VALUE 'Y'.              ELXPMCTB
00074      88  NOT-END-TAB                      VALUE 'N'.              ELXPMCTB
00075                                                                   ELXPMCTB
00076  01  WS-MAX                     PIC S9(4) COMP.                   ELXPMCTB
00077  01  WS-MAX-INDEX               PIC S9(4) COMP.                   ELXPMCTB
00078  01  WS-SUB                     PIC S9(4) COMP.                   ELXPMCTB
00079                                                                   ELXPMCTB
00080  01  WS-TAB-KEY.                                                  ELXPMCTB
00081      05  WS-TAB-PROVISION       PIC X(06) VALUE SPACES.           ELXPMCTB
00082      05  WS-TAB-SLOT            PIC S9(7) COMP-3.                 ELXPMCTB
00083                                                                   ELXPMCTB
00084  01  WS-SWITCHES.                                                 ELXPMCTB
00085      05                         PIC X(01).                        ELXPMCTB
00086         88  SW-TRMNL-ERR                  VALUE 'Y'.              ELXPMCTB
00087         88  SW-NO-TRMNL-ERR               VALUE 'N'.              ELXPMCTB
00088                                                                   ELXPMCTB
00089  01  FILLER                     PICTURE X(32)                     ELXPMCTB
00090           VALUE '*END ELXPMCTB WORKING STORAGE***'.               ELXPMCTB
00091      EJECT                                                        ELXPMCTB
00092  LINKAGE SECTION.                                                 ELXPMCTB
00093  01  DFHCOMMAREA.                                                 ELXPMCTB
00094  COPY ELSCOMMC.                                                   ELXPMCTB
00095 *    EJECT                                                        ELXPMCTB
00096  COPY ELSCIA2C.                                                   ELXPMCTB
00097 *    EJECT                                                        ELXPMCTB
00098  COPY ELSIOPMC.                                                   ELXPMCTB
00099 *    EJECT                                                        ELXPMCTB
00100  COPY ELSKEYSC.                                                   ELXPMCTB
00101 *    EJECT                                                        ELXPMCTB
00102  COPY ELSPMCID.                                                   ELXPMCTB
00103 *    EJECT                                                        ELXPMCTB
00104  01  PMCI-COMM-AREA.                                              ELXPMCTB
00105  COPY PMCCOMM.                                                    ELXPMCTB
00106 *    EJECT                                                        ELXPMCTB
00107  01  GCG-GCGRPSPC-RECORD.                                         ELXPMCTB
00108  COPY GCGROUPC.                                                   ELXPMCTB
00109 *    EJECT                                                        ELXPMCTB
00110  01  GCG-TABULAR-RECORD.                                          ELXPMCTB
00111  COPY GCTGVLC.                                                    ELXPMCTB
00112      EJECT                                                        ELXPMCTB
00113  PROCEDURE DIVISION.                                              ELXPMCTB
00114 ************************************************************      ELXPMCTB
00115 *                                                          *      ELXPMCTB
00116 *        TABULAR READ REQUEST                              *      ELXPMCTB
00117 *                                                          *      ELXPMCTB
00118 ************************************************************      ELXPMCTB
00119  0000-TABULAR-READ-REQUEST.                                       ELXPMCTB
00120                                                                   ELXPMCTB
00121      SET SW-NO-TRMNL-ERR TO TRUE.                                 ELXPMCTB
00122      IF ECA-CIA-PTR = NULL                                        ELXPMCTB
00123          CONTINUE                                                 ELXPMCTB
00124      ELSE                                                         ELXPMCTB
00125          IF EIBCALEN < LENGTH OF DFHCOMMAREA                      ELXPMCTB
00126              CONTINUE                                             ELXPMCTB
00127          ELSE                                                     ELXPMCTB
00128              PERFORM 0100-INITIALIZATION                          ELXPMCTB
00129              IF SW-NO-TRMNL-ERR                                   ELXPMCTB
00130                  PERFORM 1000-PROCESS-TABULAR-READ-REQ            ELXPMCTB
00131              END-IF                                               ELXPMCTB
00132          END-IF                                                   ELXPMCTB
00133      END-IF.                                                      ELXPMCTB
00134      GOBACK.                                                      ELXPMCTB
00135                                                                   ELXPMCTB
00136 ************************************************************      ELXPMCTB
00137 *                                                          *      ELXPMCTB
00138 *        INITIALIZATION                                    *      ELXPMCTB
00139 *                                                          *      ELXPMCTB
00140 ************************************************************      ELXPMCTB
00141  0100-INITIALIZATION.                                             ELXPMCTB
00142                                                                   ELXPMCTB
00143      SET ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA                 ELXPMCTB
00144          TO ECA-CIA-PTR.                                          ELXPMCTB
00145      CALL 'ELUINISM' USING DFHCOMMAREA                            ELXPMCTB
00146          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELXPMCTB
00147      SET CIA-PMCCOMM-DDN TO TRUE.                                 ELXPMCTB
00148      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCTB
00149          ADDRESS OF PMCI-COMM-AREA.                               ELXPMCTB
00150      IF CIA-RC-PTR-NULL                                           ELXPMCTB
00151      THEN                                                         ELXPMCTB
00152          SET SW-TRMNL-ERR TO TRUE                                 ELXPMCTB
00153          MOVE 25 TO PMCI-BLUE-CHIP-ERROR-CODE                     ELXPMCTB
00154          SET PMCI-BC-INTERNAL-ERROR TO TRUE                       ELXPMCTB
00155          CONTINUE                                                 ELXPMCTB
00156      ELSE                                                         ELXPMCTB
00157          SET CIA-GCGRPSPC-DDN TO TRUE                             ELXPMCTB
00158          PERFORM 8000-CALL-STORAGE-MANAGER                        ELXPMCTB
00159          SET CIA-GCGRPSPC-DDN TO TRUE                             ELXPMCTB
00160          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELXPMCTB
00161               ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS              ELXPMCTB
00162          IF CIA-RC-PTR-NULL                                       ELXPMCTB
00163      THEN                                                         ELXPMCTB
00164              SET SW-TRMNL-ERR TO TRUE                             ELXPMCTB
00165              MOVE 24 TO PMCI-BLUE-CHIP-ERROR-CODE                 ELXPMCTB
00166              SET PMCI-BC-INTERNAL-ERROR TO TRUE                   ELXPMCTB
00167              CONTINUE                                             ELXPMCTB
00168      ELSE                                                         ELXPMCTB
00169              SET CIA-GCPRVTB2-DDN TO TRUE                         ELXPMCTB
00170              PERFORM 8000-CALL-STORAGE-MANAGER                    ELXPMCTB
00171              SET CIA-GCPRVTB2-DDN TO TRUE                         ELXPMCTB
00172              CALL 'ELUSETAD' USING DFHCOMMAREA                    ELXPMCTB
00173                  ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS           ELXPMCTB
00174              IF CIA-RC-PTR-NULL                                   ELXPMCTB
00175          THEN                                                     ELXPMCTB
00176                  SET SW-TRMNL-ERR TO TRUE                         ELXPMCTB
00177                  MOVE 27 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCTB
00178                  SET PMCI-BC-INTERNAL-ERROR TO TRUE               ELXPMCTB
00179                  CONTINUE                                         ELXPMCTB
00180          ELSE                                                     ELXPMCTB
00181                  SET CIA-ELSKEYS-DDN TO TRUE                      ELXPMCTB
00182                  PERFORM 8000-CALL-STORAGE-MANAGER                ELXPMCTB
00183                  SET CIA-ELSKEYS-DDN TO TRUE                      ELXPMCTB
00184                  CALL 'ELUSETAD' USING DFHCOMMAREA                ELXPMCTB
00185                      ADDRESS OF KWA-FILE-KEY-WORK-AREA            ELXPMCTB
00186                  IF CIA-RC-PTR-NULL                               ELXPMCTB
00187              THEN                                                 ELXPMCTB
00188                      SET SW-TRMNL-ERR TO TRUE                     ELXPMCTB
00189                      MOVE 23 TO PMCI-BLUE-CHIP-ERROR-CODE         ELXPMCTB
00190                      SET PMCI-BC-INTERNAL-ERROR TO TRUE           ELXPMCTB
00191                      CONTINUE                                     ELXPMCTB
00192              ELSE                                                 ELXPMCTB
00193                      SET CIA-ELSPMCID-DDN TO TRUE                 ELXPMCTB
00194                      CALL 'ELUSETAD' USING DFHCOMMAREA            ELXPMCTB
00195                          ADDRESS OF NAES-INTERMEDIATE-DATA        ELXPMCTB
00196                      IF CIA-RC-PTR-NULL                           ELXPMCTB
00197                  THEN                                             ELXPMCTB
00198                          SET SW-TRMNL-ERR TO TRUE                 ELXPMCTB
00199                          MOVE 26 TO PMCI-BLUE-CHIP-ERROR-CODE     ELXPMCTB
00200                          SET PMCI-BC-INTERNAL-ERROR TO TRUE       ELXPMCTB
00201                          CONTINUE                                 ELXPMCTB
00202                  ELSE                                             ELXPMCTB
00203                          MOVE ZEROS TO PMCI-BLUE-CHIP-ERROR-CODE  ELXPMCTB
00204                          SET PMCI-BC-SUCCESSFUL TO TRUE           ELXPMCTB
00205                      END-IF                                       ELXPMCTB
00206                  END-IF                                           ELXPMCTB
00207              END-IF                                               ELXPMCTB
00208          END-IF                                                   ELXPMCTB
00209      END-IF.                                                      ELXPMCTB
00210                                                                   ELXPMCTB
00211 ************************************************************      ELXPMCTB
00212 *                                                          *      ELXPMCTB
00213 *        PROCESS TABULAR READ REQUEST                      *      ELXPMCTB
00214 *                                                          *      ELXPMCTB
00215 ************************************************************      ELXPMCTB
00216  1000-PROCESS-TABULAR-READ-REQ.                                   ELXPMCTB
00217                                                                   ELXPMCTB
00218      MOVE 'ZZ' TO NAES-TABS-IND1, NAES-TABS-IND2.                 ELXPMCTB
00219      SET CIA-ELSGRPSP-DDN  TO TRUE.                               ELXPMCTB
00220      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCTB
00221               ADDRESS OF GCG-GCGRPSPC-RECORD.                     ELXPMCTB
00222      MOVE GCG-COUNT-TAB-PROVN-POINTERS TO WS-MAX.                 ELXPMCTB
00223      IF PMCI-INSTITUTIONAL                                        ELXPMCTB
00224          PERFORM 2000-SET-INSTITUTIONAL-REQUEST                   ELXPMCTB
00225      ELSE                                                         ELXPMCTB
00226          PERFORM 2100-SET-PROFESSIONAL-REQUEST.                   ELXPMCTB
00227                                                                   ELXPMCTB
00228 ************************************************************      ELXPMCTB
00229 *                                                          *      ELXPMCTB
00230 *        SET INSTITUTIONAL REQUEST                         *      ELXPMCTB
00231 *                                                          *      ELXPMCTB
00232 ************************************************************      ELXPMCTB
00233  2000-SET-INSTITUTIONAL-REQUEST.                                  ELXPMCTB
00234                                                                   ELXPMCTB
00235      SET TAB-KEY-NOT-FOUND TO TRUE.                               ELXPMCTB
00236      SET NOT-END-TAB TO TRUE.                                     ELXPMCTB
00237      SET NAES-TAB-INDX TO 3.                                      ELXPMCTB
00238      SET NAES-MAX-INDX TO NAES-TAB-INDX.                          ELXPMCTB
00239      PERFORM 2200-READ-GROUP-TAB-POINTERS                         ELXPMCTB
00240          VARYING NAES-TAB-INDX FROM 1 BY 1                        ELXPMCTB
00241             UNTIL NAES-TAB-INDX > NAES-MAX-INDX OR                ELXPMCTB
00242                END-TAB.                                           ELXPMCTB
00243                                                                   ELXPMCTB
00244 ************************************************************      ELXPMCTB
00245 *                                                          *      ELXPMCTB
00246 *        SET PROFESSIONAL  REQUEST                         *      ELXPMCTB
00247 *                                                          *      ELXPMCTB
00248 ************************************************************      ELXPMCTB
00249  2100-SET-PROFESSIONAL-REQUEST.                                   ELXPMCTB
00250                                                                   ELXPMCTB
00251      SET TAB-KEY-NOT-FOUND TO TRUE.                               ELXPMCTB
00252      SET NOT-END-TAB TO TRUE.                                     ELXPMCTB
00253      SET NAES-TAB-INDX TO 6.                                      ELXPMCTB
00254      SET NAES-MAX-INDX TO NAES-TAB-INDX.                          ELXPMCTB
00255      PERFORM 2200-READ-GROUP-TAB-POINTERS                         ELXPMCTB
00256         VARYING NAES-TAB-INDX FROM 4 BY 1                         ELXPMCTB
00257             UNTIL NAES-TAB-INDX > NAES-MAX-INDX OR                ELXPMCTB
00258                END-TAB.                                           ELXPMCTB
00259                                                                   ELXPMCTB
00260 ************************************************************      ELXPMCTB
00261 *                                                          *      ELXPMCTB
00262 *        READ GROUP SPECIFIC TABULAR RECORD POINTERS       *      ELXPMCTB
00263 *                                                          *      ELXPMCTB
00264 ************************************************************      ELXPMCTB
00265  2200-READ-GROUP-TAB-POINTERS.                                    ELXPMCTB
00266                                                                   ELXPMCTB
00267      IF NAES-TABS-READ-IND (NAES-TAB-INDX) = 'Y'                  ELXPMCTB
00268          PERFORM 2300-SEARCH-GROUP-SPEC-TABS                      ELXPMCTB
00269                VARYING WS-SUB FROM 1 BY 1                         ELXPMCTB
00270                  UNTIL WS-SUB > WS-MAX                            ELXPMCTB
00271                     OR TAB-KEY-FOUND.                             ELXPMCTB
00272                                                                   ELXPMCTB
00273 ************************************************************      ELXPMCTB
00274 *                                                          *      ELXPMCTB
00275 *        READ GROUP SPECIFIC TABULAR RECORD POINTERS       *      ELXPMCTB
00276 *                                                          *      ELXPMCTB
00277 ************************************************************      ELXPMCTB
00278  2300-SEARCH-GROUP-SPEC-TABS.                                     ELXPMCTB
00279                                                                   ELXPMCTB
00280      IF NAES-TABS-ID (NAES-TAB-INDX) =                            ELXPMCTB
00281                  GCG-TAB-ID (WS-SUB)                              ELXPMCTB
00282          SET GCG-INDEX TO WS-SUB                                  ELXPMCTB
00283          MOVE GCG-TAB-ID (WS-SUB) TO KWA-GVL-PRVDR-ID             ELXPMCTB
00284          MOVE GCG-TAB-SLOT-NO (WS-SUB) TO KWA-GVL-PRVDR-SLOT-NO   ELXPMCTB
00285          MOVE PMCI-PROVIDER-NUMBER TO KWA-GVL-PRVDR-NBR           ELXPMCTB
00286          MOVE ZEROS TO KWA-GVL-PRVDR-SEQ-NBR                      ELXPMCTB
00287          SET TAB-KEY-FOUND TO TRUE                                ELXPMCTB
00288          PERFORM 3000-READ-TABULAR-RECORD.                        ELXPMCTB
00289                                                                   ELXPMCTB
00290 ************************************************************      ELXPMCTB
00291 *                                                          *      ELXPMCTB
00292 *        READ TABULAR RECORDS                              *      ELXPMCTB
00293 *                                                          *      ELXPMCTB
00294 ************************************************************      ELXPMCTB
00295  3000-READ-TABULAR-RECORD.                                        ELXPMCTB
00296                                                                   ELXPMCTB
00297      SET ENTRY-NOT-FOUND TO TRUE                                  ELXPMCTB
00298      SET CIA-GCPRVTB2-DDN  TO TRUE.                               ELXPMCTB
00299      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCTB
00300               ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.             ELXPMCTB
00301      SET CIA-GCPRVTB2-DDN  TO TRUE.                               ELXPMCTB
00302      SET IOP-STG-MODE-MOVE TO TRUE.                               ELXPMCTB
00303      SET IOP-RD            TO TRUE.                               ELXPMCTB
00304      SET IOP-FCQ-NONE      TO TRUE.                               ELXPMCTB
00305      SET IOP-KVQ-EQ        TO TRUE.                               ELXPMCTB
00306      MOVE SPACES TO IOP-AIX-DDNAME.                               ELXPMCTB
00307      MOVE KWA-FILE-KEY TO IOP-FILE-KEY.                           ELXPMCTB
00308      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELXPMCTB
00309      IF IOP-RC-OK                                                 ELXPMCTB
00310          SET ADDRESS OF GCG-TABULAR-RECORD                        ELXPMCTB
00311                  TO IOP-REC-PTR                                   ELXPMCTB
00312          SET TAB-FOUND TO TRUE                                    ELXPMCTB
00313          SET GVL-INDEX TO GVL-PROVIDER-CNT                        ELXPMCTB
00314          SET WS-MAX-INDEX TO GVL-INDEX                            ELXPMCTB
00315          PERFORM 3100-VERIFY-PROVIDER-NBR                         ELXPMCTB
00316              VARYING GVL-INDEX FROM 1 BY 1                        ELXPMCTB
00317                 UNTIL ENTRY-FOUND OR                              ELXPMCTB
00318                    GVL-INDEX > WS-MAX-INDEX.                      ELXPMCTB
00319                                                                   ELXPMCTB
00320 ************************************************************      ELXPMCTB
00321 *                                                          *      ELXPMCTB
00322 *        VERIFY IF PROVIDER NUMBER IS IN TABULAR RECORD    *      ELXPMCTB
00323 *                                                          *      ELXPMCTB
00324 ************************************************************      ELXPMCTB
00325  3100-VERIFY-PROVIDER-NBR.                                        ELXPMCTB
00326                                                                   ELXPMCTB
00327          IF GVL-PROVIDER-EFFDT-CEN  (GVL-INDEX) = 9999999 OR      ELXPMCTB
00328              GVL-PROVIDER-EFFDT-CEN  (GVL-INDEX) >                ELXPMCTB
00329                   NAES-PRV-SELECT-DATE-CEN                        ELXPMCTB
00330              SET ENTRY-NOT-FOUND TO TRUE                          ELXPMCTB
00331              SET GVL-INDEX TO WS-MAX-INDEX                        ELXPMCTB
00332          ELSE                                                     ELXPMCTB
00333              IF GVL-PROVIDER-EFFDT-CEN (GVL-INDEX) <=             ELXPMCTB
00334                           NAES-PRV-SELECT-DATE-CEN OR             ELXPMCTB
00335                 GVL-PROVIDER-TERMDT-CEN (GVL-INDEX) >=            ELXPMCTB
00336                              NAES-PRV-SELECT-DATE-CEN             ELXPMCTB
00337                 MOVE GVL-PROVIDER-CTL-1 (GVL-INDEX) TO            ELXPMCTB
00338                                            NAES-TABS-IND1         ELXPMCTB
00339                 MOVE GVL-PROVIDER-CTL-2 (GVL-INDEX) TO            ELXPMCTB
00340                                            NAES-TABS-IND2         ELXPMCTB
00341                 MOVE 'Y' TO NAES-TABS-RETN-CODE                   ELXPMCTB
00342                 SET ENTRY-FOUND TO TRUE                           ELXPMCTB
00343                 SET END-TAB TO TRUE                               ELXPMCTB
00344              END-IF                                               ELXPMCTB
00345          END-IF.                                                  ELXPMCTB
00346                                                                   ELXPMCTB
00347 ************************************************************      ELXPMCTB
00348 *                                                          *      ELXPMCTB
00349 *        CALL STORAGE MANAGER                              *      ELXPMCTB
00350 *                                                          *      ELXPMCTB
00351 ************************************************************      ELXPMCTB
00352  8000-CALL-STORAGE-MANAGER.                                       ELXPMCTB
00353                                                                   ELXPMCTB
00354      MOVE ZERO TO CIA-AREA-LEN.                                   ELXPMCTB
00355      SET CIA-STG-GETMAIN TO TRUE.                                 ELXPMCTB
00356      CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELXPMCTB
00357                                                                   ELXPMCTB
