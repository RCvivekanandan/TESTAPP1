00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELP031  
00003  PROGRAM-ID.         ELP031.                                         LV001
00004                                                                   ELP031  
00005  AUTHOR.             ANNE KEFFER KING.                            ELP031  
00006                                                                   ELP031  
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELP031  
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELP031  
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELP031  
00010                      233 N. MICHIGAN AVE                          ELP031  
00011                      CHICAGO, ILLINOIS 60601                      ELP031  
00012                                                                   ELP031  
00013  DATE-WRITTEN.       08-AUG-1989.                                 ELP031  
00014                                                                   ELP031  
00015  DATE-COMPILED.                                                   ELP031  
00016                                                                   ELP031  
00017  SECURITY.           COPYRIGHT 1988,                              ELP031  
00018                      HEALTH CARE SERVICE CORPORATION              ELP031  
00019      SKIP3                                                        ELP031  
00020  ENVIRONMENT DIVISION.                                            ELP031  
00021                                                                   ELP031  
00022  CONFIGURATION SECTION.                                           ELP031  
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELP031  
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELP031  
00025      EJECT                                                        ELP031  
00026 ******************************************************************ELP031  
00027 *                                                                *ELP031  
00028 *                    ELS ABEND PROCESSING                        *ELP031  
00029 *                                                                *ELP031  
00030 *   ELP031 - THIS PROGRAM READS, FORMATS AND PRINTS THE          *ELP031  
00031 *            SD ABEND REPORT.  THE READS ARE DONE BY THE         *ELP031  
00032 *            ELPRDSNA SUBROUTINE.  THE WRITES ARE DONE BY        *ELP031  
00033 *            ELPPR SUBROUTINE.                                   *ELP031  
00034 *                                                                *ELP031  
00035 ******************************************************************ELP031  
00036 *                                                                *ELP031  
00037 *                      MAINTENANCE HISTORY                       *ELP031  
00038 *                                                                *ELP031  
00039 *  MOD     DATE     BY  DRPT                ACTION               *ELP031  
00040 * ----- ----------- --- ----- ---------------------------------- *ELP031  
00041 * 01.00 08-AUG-1989 AKK       CREATED                            *ELP031  
00042 * 01.01 21-SEP-1989 EGL      -ADDED COPYBOOK ELSNAPPC TO DES-    *ELP031  
00043 *                             CRIBE THE HEADER RECORD.           *ELP031  
00044 *                            -ADDED CALL TO ELPSUMRY             *ELP031  
00045 *                            -CHANGED TO ALLOW HEADER RECORDS    *ELP031  
00046 *                             FOR ABEND CODES NOT IN THE TABLE   *ELP031  
00047 *                             TO PRINT.                          *ELP031  
00048 * 01.02 23-OCT-1989 AKK      -REMOVED CODE FROM 1110-VERIFY PARA-*ELP031  
00049 *                             GRAPH CAUSING RECORDS TO BE MISSED,*ELP031  
00050 * 01.03 20-MAR-1990 AKK      -ADDED CALL TO ELPNFCV AND ELPNFDE  *ELP031  
00051 * 01.04 20-APR-1990 AKK      -ADDED CALL TO ELPNFPRV             *ELP031  
00052 * 01.05 05-NOV-1997 AKK      -ADDED SUPPORT FOR YEAR 2000 AND    *ELP031  
00053 *                             TX MERGE.                          *ELP031  
00054 * 01.06 29-JAN-1998 AKK      -ADDED EOF CHECK AFTER READ IN 1100-*ELP031  
00055 ******************************************************************ELP031  
00056                                                                   ELP031  
00057  INPUT-OUTPUT SECTION.                                            ELP031  
00058  FILE-CONTROL.                                                    ELP031  
00059 *                                                                 ELP031  
00060  DATA DIVISION.                                                   ELP031  
00061  FILE SECTION.                                                    ELP031  
00062 /                                                                 ELP031  
00063  WORKING-STORAGE SECTION.                                         ELP031  
00064  01  FILLER                 PIC X(17)   VALUE '*START OF ELP031*'.ELP031  
00065 *                                                                 ELP031  
00066  01  WS-SWITCHES.                                                 ELP031  
00067      05 WS-EOF-INDICATOR    PIC X(03)   VALUE 'YES'.              ELP031  
00068         88  MORE-RECORDS-TO-READ        VALUE 'YES'.              ELP031  
00069         88  EOF-FOUND                   VALUE 'EOF'.              ELP031  
00070      05 WS-PAGE6-SWITCH     PIC X(03)   VALUE 'YES'.              ELP031  
00071         88  FIRST-PRINT-PAGE6           VALUE 'YES'.              ELP031  
00072         88  NOT-FIRST-PRINT-PAGE6       VALUE 'NO '.              ELP031  
00073      05 WS-PAGE6-PRINT-SW   PIC X(03)   VALUE 'YES'.              ELP031  
00074         88  OK-TO-PRINT-PAGE6           VALUE 'YES'.              ELP031  
00075         88  PAGE6-PRINTED               VALUE 'NO '.              ELP031  
00076      05 WS-TABLE1-2-SWITCH  PIC X(03)   VALUE SPACE.              ELP031  
00077         88  TABLE1-OK                   VALUE 'TB1'.              ELP031  
00078         88  TABLE2-OK                   VALUE 'TB2'.              ELP031  
00079         88  TABLE1-AND-TABLE2-OK        VALUE 'T12'.              ELP031  
00080 *                                                                 ELP031  
00081  01  WS-WORK-AREAS.                                               ELP031  
00082      05 WS-TABLE-SUB       PIC S9(04)   COMP.                     ELP031  
00083      05 WS-CONTRACT-SUB    PIC S9(04)   COMP.                     ELP031  
00084      05 WS-NO-SSCB-MESSAGE PIC  X(36)   VALUE                     ELP031  
00085         'NO SYSTEM STATUS CONTROL BLOCK FOUND'.                   ELP031  
00086      05 WS-NO-DUMP-HEADER  PIC  X(26)   VALUE                     ELP031  
00087         'NO DUMP HEADER BLOCK FOUND'.                             ELP031  
00088      05 WS-NO-SD-RPT-MSG  PIC  X(47)   VALUE                      ELP031  
00089         'NO SD ABENDS PRINTED, NOTHING TO REPORT FOR SD.'.        ELP031  
00090      05 WS-END-REPORT-MSG PIC  X(17)   VALUE                      ELP031  
00091         'END OF SD REPORT.'.                                      ELP031  
00092      05 WS-DATE            PIC 9(06).                             ELP031  
00093      05 WS-JUL-DATE        PIC 9(05).                             ELP031  
00094      05 WS-GREG-DATE       PIC 9(06).                             ELP031  
00095      05 WS-INVALID-ABEND-CODE                                     ELP031  
00096                            PIC X(04)    VALUE 'ELXX'.             ELP031  
00097 *                                                                 ELP031  
00098  01  PROGRAM-CONSTANTS.                                           ELP031  
00099      05  PC-ELP031         PIC X(06)    VALUE 'ELP031'.           ELP031  
00100      05  PC-ELSKTBCC       PIC X(07)    VALUE 'ELSKTBC'.          ELP031  
00101      05  PC-ELSKTBGC       PIC X(07)    VALUE 'ELSKTBG'.          ELP031  
00102      05  PC-ELSKTBSC       PIC X(07)    VALUE 'ELSKTBS'.          ELP031  
00103      05  PC-ELSMEMSC       PIC X(07)    VALUE 'ELSMEMS'.          ELP031  
00104      05  PC-ELSGRPSP       PIC X(08)    VALUE 'ELSGRPSP'.         ELP031  
00105      05  PC-ELSCONIB       PIC X(08)    VALUE 'ELSCONIB'.         ELP031  
00106      05  PC-ELSCONPB       PIC X(08)    VALUE 'ELSCONPB'.         ELP031  
00107      05  PC-ELSCONIS       PIC X(08)    VALUE 'ELSCONIS'.         ELP031  
00108      05  PC-ELSCONPS       PIC X(08)    VALUE 'ELSCONPS'.         ELP031  
00109      05  PC-NO-SSCB        PIC X(07)    VALUE 'NO SSCB'.          ELP031  
00110 /                                                                 ELP031  
00111 ****************************************************************  ELP031  
00112 *                         MLDATE                               *  ELP031  
00113 ****************************************************************  ELP031  
00114  COPY MLDATE01.                                                   ELP031  
00115                                                                   ELP031  
00116 *                                                                 ELP031  
00117      COPY HSCDATES.                                               ELP031  
00118 *                                                                 ELP031  
00119 /                                                                 ELP031  
00120 ***************************************************************** ELP031  
00121 *THE COPYBOOKS USED ONLY FOR ABEND PROCESSING ARE AS FOLLOWS:   * ELP031  
00122 *   ELSNAPRC, ELSPRCBC, ELSPRKYC, ELSRDPAC.                     * ELP031  
00123 ***************************************************************** ELP031  
00124 *                                                                 ELP031  
00125      COPY ELSNAPRC.                                               ELP031  
00126 /                                                                 ELP031  
00127      COPY ELSPRCBC.                                               ELP031  
00128 /                                                                 ELP031  
00129      COPY ELSPRKYC.                                               ELP031  
00130 /                                                                 ELP031  
00131      COPY ELSRDPAC.                                               ELP031  
00132 /                                                                 ELP031  
00133  LINKAGE SECTION.                                                 ELP031  
00134 ****************************************************************  ELP031  
00135 *THE COPYBOOKS THAT WILL SUPPLY THE DATA APPEARING ON THE      *  ELP031  
00136 *REPORT ARE AS FOLLOWS:                                        *  ELP031  
00137 * ELSMEMSC, ELSKTBCC, ELSKTBGC, ELSKTBSC, ELSSSCBC, GCCONTRC,  *  ELP031  
00138 * GCGRPSC.                                                     *  ELP031  
00139 ****************************************************************  ELP031  
00140 *                                                                 ELP031  
00141      COPY ELSMEMSC.                                               ELP031  
00142 /                                                                 ELP031  
00143      COPY ELSKTBCC.                                               ELP031  
00144 /                                                                 ELP031  
00145      COPY ELSKTBGC.                                               ELP031  
00146 /                                                                 ELP031  
00147      COPY ELSKTBSC.                                               ELP031  
00148 /                                                                 ELP031  
00149      COPY ELSNAPPC.                                               ELP031  
00150 /                                                                 ELP031  
00151      COPY ELSSSCBC.                                               ELP031  
00152 /                                                                 ELP031  
00153  01  CONTRACT-RECORD.                                             ELP031  
00154      COPY GCCONTRC.                                               ELP031  
00155 /                                                                 ELP031  
00156  01  GROUP-SPECIFIC-RECORD.                                       ELP031  
00157      COPY GCGROUPC.                                               ELP031  
00158 *                                                                 ELP031  
00159  PROCEDURE DIVISION.                                              ELP031  
00160      PERFORM 0000-INITIALIZATION.                                 ELP031  
00161      PERFORM 1000-PROCESS-SD-ABEND-REPORT                         ELP031  
00162           UNTIL RDP-CLOSE-FILE.                                   ELP031  
00163      PERFORM 9000-TERMINATION.                                    ELP031  
00164      GOBACK.                                                      ELP031  
00165 *                                                                 ELP031  
00166  0000-INITIALIZATION.                                             ELP031  
00167      PERFORM 0050-INITIALIZE-ELSPRKY.                             ELP031  
00168      SET RDP-OPEN-FILE TO TRUE.                                   ELP031  
00169      CALL 'ELPRDSNA' USING RDP-READ-PARAMETERS.                   ELP031  
00170      MOVE ALL '?' TO PCB-GROUP-NUM                                ELP031  
00171                      PCB-PLAN-CODE                                ELP031  
00172                      PCB-PKG-CODE                                 ELP031  
00173                      PCB-SECTION-NUM.                             ELP031  
00174      SET PCB-OPEN-PRINTER TO TRUE.                                ELP031  
00175      MOVE PC-ELP031 TO PCB-PRINT-TEXT.                            ELP031  
00176      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELP031  
00177      PERFORM 1360-DO-CALL-TO-TCDTES.                              ELP031  
00178 *                                                                 ELP031  
00179  0050-INITIALIZE-ELSPRKY.                                         ELP031  
00180      INITIALIZE KYA-GROUP-SPEC-KEY                                ELP031  
00181                 KYA-CONTRACT-TYPE.                                ELP031  
00182      PERFORM VARYING WS-CONTRACT-SUB FROM 1                       ELP031  
00183             BY 1 UNTIL WS-CONTRACT-SUB > 4                        ELP031  
00184           SET KYA-IDX TO WS-CONTRACT-SUB                          ELP031  
00185           INITIALIZE KYA-CONTRACT-KEYS (KYA-IDX)                  ELP031  
00186           MOVE ZEROES TO KYA-CK-EFF-DT-CENTURY (KYA-IDX)          ELP031  
00187      END-PERFORM.                                                 ELP031  
00188 *                                                                 ELP031  
00189  1000-PROCESS-SD-ABEND-REPORT.                                    ELP031  
00190      PERFORM 1100-EDIT-AND-PRINT-SD-REPORT                        ELP031  
00191             UNTIL EOF-FOUND.                                      ELP031  
00192      IF EOF-FOUND                                                 ELP031  
00193         SET RDP-CLOSE-FILE TO TRUE                                ELP031  
00194         CALL 'ELPRDSNA' USING RDP-READ-PARAMETERS                 ELP031  
00195         PERFORM 5000-VERIFY-PAGE6-PRINTED                         ELP031  
00196         INITIALIZE PCB-PAGE-HEADING-INFO                          ELP031  
00197         CALL 'ELPSUMRY' USING PCB-PRINT-CONTROL-BLOCK             ELP031  
00198         CALL 'ELPNFCV' USING PCB-PRINT-CONTROL-BLOCK              ELP031  
00199         CALL 'ELPNFDE' USING PCB-PRINT-CONTROL-BLOCK              ELP031  
00200         CALL 'ELPNFPRV' USING PCB-PRINT-CONTROL-BLOCK             ELP031  
00201         IF PCB-PAGE-NUMBER = ZERO                                 ELP031  
00202            PERFORM 1025-PRINT-NO-REPORT-MSG                       ELP031  
00203         ELSE                                                      ELP031  
00204            PERFORM 1050-PRINT-END-MSG.                            ELP031  
00205 *                                                                 ELP031  
00206  1025-PRINT-NO-REPORT-MSG.                                        ELP031  
00207      SET PCB-EJECT TO TRUE.                                       ELP031  
00208      SET PCB-PRINT-LINE TO TRUE.                                  ELP031  
00209      MOVE WS-NO-SD-RPT-MSG TO PCB-PRINT-TEXT.                     ELP031  
00210      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELP031  
00211 *                                                                 ELP031  
00212  1050-PRINT-END-MSG.                                              ELP031  
00213      SET PCB-EJECT TO TRUE.                                       ELP031  
00214      SET PCB-PRINT-LINE TO TRUE.                                  ELP031  
00215      MOVE WS-END-REPORT-MSG TO PCB-PRINT-TEXT.                    ELP031  
00216      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELP031  
00217 *                                                                 ELP031  
00218  1100-EDIT-AND-PRINT-SD-REPORT.                                   ELP031  
00219      SET RDP-READ-FILE TO TRUE.                                   ELP031  
00220      CALL 'ELPRDSNA' USING RDP-READ-PARAMETERS.                   ELP031  
00221      IF RDP-EOF                                                   ELP031  
00222         CONTINUE                                                  ELP031  
00223      ELSE                                                         ELP031  
00224         PERFORM 1110-VERIFY-ABEND-AND-RETURNS.                    ELP031  
00225      IF RDP-EOF                                                   ELP031  
00226          SET EOF-FOUND TO TRUE                                    ELP031  
00227      ELSE                                                         ELP031  
00228          PERFORM 1125-EDIT-SNAP-SHOT-RECORD.                      ELP031  
00229 *                                                                 ELP031  
00230  1110-VERIFY-ABEND-AND-RETURNS.                                   ELP031  
00231      IF (RDP-ABEND-CODE = WS-INVALID-ABEND-CODE)                  ELP031  
00232         OR RDP-SHORT-RECORD OR RDP-INVALID-DATA                   ELP031  
00233         OR RDP-ABEND-CODE = LOW-VALUES                            ELP031  
00234         IF RDP-ABEND-CODE = LOW-VALUES                            ELP031  
00235            MOVE 'NOCD' TO RDP-ABEND-CODE                          ELP031  
00236         END-IF                                                    ELP031  
00237         PERFORM 1150-PRINT-REPORT-PAGE7                           ELP031  
00238         PERFORM 1115-CALL-TO-ELSRDSNA                             ELP031  
00239      END-IF.                                                      ELP031  
00240 *                                                                 ELP031  
00241  1115-CALL-TO-ELSRDSNA.                                           ELP031  
00242      SET RDP-CALL-OK TO TRUE.                                     ELP031  
00243      SET RDP-READ-FILE TO TRUE.                                   ELP031  
00244      CALL 'ELPRDSNA' USING RDP-READ-PARAMETERS.                   ELP031  
00245 *                                                                 ELP031  
00246  1125-EDIT-SNAP-SHOT-RECORD.                                      ELP031  
00247      INITIALIZE WS-TABLE1-2-SWITCH.                               ELP031  
00248      PERFORM VARYING WS-TABLE-SUB                                 ELP031  
00249         FROM 1 BY 1 UNTIL                                         ELP031  
00250           TABLE1-OK OR (WS-TABLE-SUB > ART-MAX-ABEND-CODES)       ELP031  
00251             IF ART-ABEND-CODE (WS-TABLE-SUB) = RDP-ABEND-CODE     ELP031  
00252              SET ART-ABEND-IDX TO WS-TABLE-SUB                    ELP031  
00253                SET TABLE1-OK TO TRUE                              ELP031  
00254              END-IF                                               ELP031  
00255      END-PERFORM.                                                 ELP031  
00256      IF TABLE1-OK                                                 ELP031  
00257         PERFORM VARYING WS-TABLE-SUB                              ELP031  
00258            FROM 1 BY 1 UNTIL                                      ELP031  
00259              TABLE1-AND-TABLE2-OK OR                              ELP031  
00260                (WS-TABLE-SUB > ART-MAX-DDNAMES)                   ELP031  
00261                 SET ART-DDNAME-IDX TO WS-TABLE-SUB                ELP031  
00262                 IF ART-DDNAME (WS-TABLE-SUB) = RDP-DDNAME         ELP031  
00263                    SET TABLE1-AND-TABLE2-OK TO TRUE               ELP031  
00264                 END-IF                                            ELP031  
00265         END-PERFORM                                               ELP031  
00266         IF TABLE1-AND-TABLE2-OK                                   ELP031  
00267           SET ART-SD-IDX TO ART-PRINT-COL (ART-ABEND-IDX)         ELP031  
00268           IF ART-PRINT-SD (ART-DDNAME-IDX, ART-SD-IDX)            ELP031  
00269              PERFORM 1200-DO-SD-REPORT-PRINT                      ELP031  
00270           END-IF                                                  ELP031  
00271         END-IF                                                    ELP031  
00272      ELSE                                                         ELP031  
00273         IF RDP-DUMP-HEADER                                        ELP031  
00274            PERFORM 1200-DO-SD-REPORT-PRINT                        ELP031  
00275         END-IF                                                    ELP031  
00276      END-IF.                                                      ELP031  
00277 *                                                                 ELP031  
00278  1150-PRINT-REPORT-PAGE7.                                         ELP031  
00279      IF RDP-ABEND-TIME NOT NUMERIC                                ELP031  
00280          MOVE ZEROES TO RDP-ABEND-TIME                            ELP031  
00281      END-IF.                                                      ELP031  
00282      IF RDP-ABEND-DATE NOT NUMERIC                                ELP031  
00283          MOVE ZEROES TO RDP-ABEND-DATE                            ELP031  
00284      END-IF.                                                      ELP031  
00285      CALL 'ELPPRDMP' USING PCB-PRINT-CONTROL-BLOCK                ELP031  
00286                            RDP-READ-PARAMETERS.                   ELP031  
00287 *                                                                 ELP031  
00288  1200-DO-SD-REPORT-PRINT.                                         ELP031  
00289      SET PCB-PRINT-LINE TO TRUE.                                  ELP031  
00290      EVALUATE TRUE                                                ELP031  
00291         WHEN  (RDP-ABEND-CODE NOT = PCB-ABEND-CODE                ELP031  
00292             OR RDP-ABEND-DATE NOT = PCB-ABEND-DATE-CENTURY        ELP031  
00293             OR RDP-ABEND-TIME NOT = PCB-ABEND-TIME                ELP031  
00294             OR RDP-TERMINAL-ID NOT = PCB-TERMINAL-ID)             ELP031  
00295               PERFORM 1300-CONTROL-BREAK                          ELP031  
00296         WHEN   RDP-DDNAME = PC-ELSKTBCC                           ELP031  
00297               PERFORM 1215-PRINT-REPORT-PAGE2                     ELP031  
00298         WHEN   RDP-DDNAME = PC-ELSKTBGC                           ELP031  
00299               PERFORM 1230-PRINT-REPORT-PAGE3                     ELP031  
00300         WHEN   RDP-DDNAME = PC-ELSKTBSC                           ELP031  
00301               PERFORM 1245-PRINT-REPORT-PAGE4                     ELP031  
00302         WHEN   RDP-DDNAME = PC-ELSMEMSC                           ELP031  
00303               PERFORM 1260-PRINT-REPORT-PAGE5                     ELP031  
00304         WHEN  (RDP-DDNAME = PC-ELSGRPSP OR PC-ELSCONIB            ELP031  
00305                  OR PC-ELSCONPB OR PC-ELSCONIS OR PC-ELSCONPS)    ELP031  
00306               PERFORM 1400-ACCUM-REPORT-PAGE6-DATA                ELP031  
00307         WHEN OTHER                                                ELP031  
00308               PERFORM 1150-PRINT-REPORT-PAGE7                     ELP031  
00309               SET PCB-OK TO TRUE.                                 ELP031  
00310         IF PCB-INVALID-DATA-FOUND                                 ELP031  
00311             PERFORM 1150-PRINT-REPORT-PAGE7.                      ELP031  
00312 *                                                                 ELP031  
00313  1215-PRINT-REPORT-PAGE2.                                         ELP031  
00314      CALL 'ELUADDRS' USING RDP-AREA                               ELP031  
00315                     ADDRESS OF KTC-GCCONTR-KEY-TABLE.             ELP031  
00316      CALL 'ELPPRKTC' USING PCB-PRINT-CONTROL-BLOCK                ELP031  
00317                            KTC-GCCONTR-KEY-TABLE.                 ELP031  
00318 *                                                                 ELP031  
00319  1230-PRINT-REPORT-PAGE3.                                         ELP031  
00320      CALL 'ELUADDRS' USING RDP-AREA                               ELP031  
00321                     ADDRESS OF KTG-GCGRPSPC-KEY-TABLE.            ELP031  
00322      CALL 'ELPPRKTG' USING PCB-PRINT-CONTROL-BLOCK                ELP031  
00323                            KTG-GCGRPSPC-KEY-TABLE.                ELP031  
00324 *                                                                 ELP031  
00325  1245-PRINT-REPORT-PAGE4.                                         ELP031  
00326      CALL 'ELUADDRS' USING RDP-AREA                               ELP031  
00327                     ADDRESS OF KTS-SECTIONS-KEY-TABLE.            ELP031  
00328      CALL 'ELPPRKTS' USING PCB-PRINT-CONTROL-BLOCK                ELP031  
00329                            KTS-SECTIONS-KEY-TABLE.                ELP031  
00330 *                                                                 ELP031  
00331  1260-PRINT-REPORT-PAGE5.                                         ELP031  
00332      CALL 'ELUADDRS' USING RDP-AREA                               ELP031  
00333                     ADDRESS OF MSI-MEMBERSHIP-INTERFACE.          ELP031  
00334      CALL 'ELPMEMSC' USING PCB-PRINT-CONTROL-BLOCK                ELP031  
00335                            MSI-MEMBERSHIP-INTERFACE.              ELP031  
00336 *                                                                 ELP031  
00337  1300-CONTROL-BREAK.                                              ELP031  
00338      CALL 'ELUADDRS' USING RDP-AREA                               ELP031  
00339               ADDRESS OF SSP-SNAP-SHOT-PREFIX.                    ELP031  
00340      CALL 'ELUADDRS' USING SSP-STATUS-REC (1)                     ELP031  
00341               ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.             ELP031  
00342      IF FIRST-PRINT-PAGE6                                         ELP031  
00343         SET NOT-FIRST-PRINT-PAGE6 TO TRUE                         ELP031  
00344      ELSE                                                         ELP031  
00345         PERFORM 1600-CALL-ELPPRKY                                 ELP031  
00346      END-IF.                                                      ELP031  
00347      IF RDP-DUMP-HEADER                                           ELP031  
00348         PERFORM 1355-MOVE-ABEND-INFO                              ELP031  
00349         PERFORM 1350-PRINT-REPORT-PAGE1                           ELP031  
00350      ELSE                                                         ELP031  
00351         SET PCB-PRINT-LINE TO TRUE                                ELP031  
00352         SET PCB-EJECT TO TRUE                                     ELP031  
00353         MOVE WS-NO-DUMP-HEADER TO PCB-PRINT-TEXT                  ELP031  
00354         PERFORM 1355-MOVE-ABEND-INFO                              ELP031  
00355         CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.               ELP031  
00356 *                                                                 ELP031  
00357  1350-PRINT-REPORT-PAGE1.                                         ELP031  
00358      SET PCB-PRINT-LINE TO TRUE.                                  ELP031  
00359      SET PCB-EJECT TO TRUE.                                       ELP031  
00360      IF RDP-DDNAME = PC-NO-SSCB                                   ELP031  
00361           MOVE WS-NO-SSCB-MESSAGE TO PCB-PRINT-TEXT               ELP031  
00362           CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK              ELP031  
00363      ELSE                                                         ELP031  
00364         CALL 'ELPPRSSB' USING PCB-PRINT-CONTROL-BLOCK             ELP031  
00365                               SSB-SELECTOR-STATUS-CTL-BLK.        ELP031  
00366      IF PCB-INVALID-DATA-FOUND                                    ELP031  
00367         PERFORM 1150-PRINT-REPORT-PAGE7.                          ELP031  
00368 *                                                                 ELP031  
00369  1355-MOVE-ABEND-INFO.                                            ELP031  
00370      IF RDP-DUMP-HEADER                                           ELP031  
00371          MOVE SSB-PLAN-CODE TO PCB-PLAN-CODE                      ELP031  
00372          MOVE SSB-GROUP-NUMBER TO PCB-GROUP-NUM                   ELP031  
00373          MOVE SSB-PKG-CODE TO PCB-PKG-CODE                        ELP031  
00374          MOVE SSB-SECTN-NO TO PCB-SECTION-NUM                     ELP031  
00375          MOVE SSB-PKG-CODE TO PCB-PKG-CODE.                       ELP031  
00376      SET OK-TO-PRINT-PAGE6 TO TRUE.                               ELP031  
00377      MOVE RDP-ABEND-CODE TO PCB-ABEND-CODE.                       ELP031  
00378      MOVE RDP-ABEND-DATE TO PCB-ABEND-DATE-CENTURY.               ELP031  
00379      MOVE RDP-ABEND-TIME TO PCB-ABEND-TIME.                       ELP031  
00380      MOVE RDP-CICS-APPL-ID TO PCB-CICS-APPL-ID.                   ELP031  
00381      MOVE RDP-CICS-SYSTEM-ID TO PCB-CICS-SYSTEM-ID.               ELP031  
00382      MOVE RDP-TERMINAL-ID TO PCB-TERMINAL-ID.                     ELP031  
00383 *                                                                 ELP031  
00384  1360-DO-CALL-TO-TCDTES.                                          ELP031  
00385      CALL 'TCDTES' USING HSCDATES.                                ELP031  
00386      MOVE TODAY TO WS-DATE.                                       ELP031  
00387      MOVE WS-DATE TO PCB-PRINT-DATE.                              ELP031  
00388 *    MOVE 'TDY' TO MLDATE-FUNC.                                   ELP031  
00389 *    MOVE 'Y' TO MLDATE-FORM1.                                    ELP031  
00390 *    CALL 'MLDATE' USING MLDATE01.                                ELP031  
00391 *    MOVE MLDATE1-DATE1 TO WS-DATE.                               ELP031  
00392 *    MOVE WS-DATE TO PCB-PRINT-DATE.                              ELP031  
00393 *                                                                 ELP031  
00394  1400-ACCUM-REPORT-PAGE6-DATA.                                    ELP031  
00395      EVALUATE TRUE                                                ELP031  
00396         WHEN RDP-DDNAME = PC-ELSGRPSP                             ELP031  
00397            PERFORM 1415-CAPTURE-GRP-SPEC-DATA                     ELP031  
00398         WHEN RDP-DDNAME = PC-ELSCONIB                             ELP031  
00399            PERFORM 1430-CAPTURE-INST-BASIC-DATA                   ELP031  
00400         WHEN RDP-DDNAME = PC-ELSCONIS                             ELP031  
00401            PERFORM 1445-CAPTURE-INST-SUPP-DATA                    ELP031  
00402         WHEN RDP-DDNAME = PC-ELSCONPB                             ELP031  
00403            PERFORM 1460-CAPTURE-PROF-BASIC-DATA                   ELP031  
00404         WHEN RDP-DDNAME = PC-ELSCONPS                             ELP031  
00405            PERFORM 1475-CAPTURE-PROF-SUPP-DATA.                   ELP031  
00406 *                                                                 ELP031  
00407  1415-CAPTURE-GRP-SPEC-DATA.                                      ELP031  
00408      CALL 'ELUADDRS' USING RDP-AREA                               ELP031  
00409          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELP031  
00410      MOVE GCG-PLAN-CODE TO KYA-GSK-PLAN-CODE.                     ELP031  
00411      MOVE GCG-GRP-NO TO KYA-GSK-GROUP.                            ELP031  
00412      MOVE GCG-SECTN-NO TO KYA-GSK-SECT.                           ELP031  
00413      MOVE GCG-PKG-CODE TO KYA-GSK-PKG-CODE.                       ELP031  
00414      MOVE GCG-FAM-REL-LVL TO KYA-GSK-FAM-REL-LVL.                 ELP031  
00415      MOVE GCG-EFFDT-CEN  TO KYA-GSK-EFF-DT-CENTURY.               ELP031  
00416      SET KYA-GSK-PRINT TO TRUE.                                   ELP031  
00417 *                                                                 ELP031  
00418  1430-CAPTURE-INST-BASIC-DATA.                                    ELP031  
00419      SET KYA-CONTRACT-IB TO TRUE.                                 ELP031  
00420      PERFORM 1435-SET-IDX-AND-KEYS.                               ELP031  
00421 *                                                                 ELP031  
00422  1435-SET-IDX-AND-KEYS.                                           ELP031  
00423      SET KYA-IDX TO KYA-CONTRACT-TYPE.                            ELP031  
00424      CALL 'ELUADDRS' USING RDP-AREA                               ELP031  
00425          ADDRESS OF CONTRACT-RECORD.                              ELP031  
00426      MOVE GCT-PLAN-CODE TO KYA-CK-PLAN-CODE (KYA-IDX).            ELP031  
00427      MOVE GCT-GRP-NO TO KYA-CK-GRP (KYA-IDX).                     ELP031  
00428      MOVE GCT-PKG-CODE TO KYA-CK-PKG-CODE (KYA-IDX).              ELP031  
00429      MOVE GCT-SECTN-NO TO KYA-CK-SECT (KYA-IDX).                  ELP031  
00430      MOVE GCT-L-O-B TO KYA-CK-L-O-B (KYA-IDX).                    ELP031  
00431      MOVE GCT-PROVDR-CONTROL                                      ELP031  
00432                    TO KYA-CK-PROVDR-CONTROL (KYA-IDX).            ELP031  
00433      MOVE GCT-FAM-REL-LVL TO KYA-CK-FAM-REL-LVL (KYA-IDX).        ELP031  
00434      MOVE GCT-EFFDT-CEN TO KYA-CK-EFF-DT-CENTURY (KYA-IDX).       ELP031  
00435      SET KYA-CK-PRINT (KYA-IDX) TO TRUE.                          ELP031  
00436 *                                                                 ELP031  
00437  1445-CAPTURE-INST-SUPP-DATA.                                     ELP031  
00438      SET KYA-CONTRACT-IS TO TRUE.                                 ELP031  
00439      PERFORM 1435-SET-IDX-AND-KEYS.                               ELP031  
00440 *                                                                 ELP031  
00441  1460-CAPTURE-PROF-BASIC-DATA.                                    ELP031  
00442      SET KYA-CONTRACT-PB TO TRUE.                                 ELP031  
00443      PERFORM 1435-SET-IDX-AND-KEYS.                               ELP031  
00444 *                                                                 ELP031  
00445  1475-CAPTURE-PROF-SUPP-DATA.                                     ELP031  
00446      SET KYA-CONTRACT-PS TO TRUE.                                 ELP031  
00447      PERFORM 1435-SET-IDX-AND-KEYS.                               ELP031  
00448 *                                                                 ELP031  
00449  1600-CALL-ELPPRKY.                                               ELP031  
00450      CALL 'ELPPRKY' USING PCB-PRINT-CONTROL-BLOCK                 ELP031  
00451                           KYA-KEY-HOLD-AREA.                      ELP031  
00452      IF PCB-INVALID-DATA-FOUND                                    ELP031  
00453         PERFORM 1150-PRINT-REPORT-PAGE7                           ELP031  
00454      END-IF.                                                      ELP031  
00455      PERFORM 0050-INITIALIZE-ELSPRKY.                             ELP031  
00456      SET PAGE6-PRINTED TO TRUE.                                   ELP031  
00457 *                                                                 ELP031  
00458  5000-VERIFY-PAGE6-PRINTED.                                       ELP031  
00459      IF OK-TO-PRINT-PAGE6                                         ELP031  
00460        PERFORM 1600-CALL-ELPPRKY.                                 ELP031  
00461 *                                                                 ELP031  
00462  9000-TERMINATION.                                                ELP031  
00463      SET PCB-CLOSE-PRINTER TO TRUE.                               ELP031  
00464      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELP031  
