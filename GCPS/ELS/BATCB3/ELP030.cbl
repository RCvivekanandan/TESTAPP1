00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELP030  
00003  PROGRAM-ID.         ELP030.                                         LV001
00004                                                                   ELP030  
00005  AUTHOR.             ANNE KEFFER KING.                            ELP030  
00006                                                                   ELP030  
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELP030  
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELP030  
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELP030  
00010                      233 N. MICHIGAN AVE                          ELP030  
00011                      CHICAGO, ILLINOIS 60601                      ELP030  
00012                                                                   ELP030  
00013  DATE-WRITTEN.       21-JUN-1989.                                 ELP030  
00014                                                                   ELP030  
00015  DATE-COMPILED.                                                   ELP030  
00016                                                                   ELP030  
00017  SECURITY.           COPYRIGHT 1988,                              ELP030  
00018                      HEALTH CARE SERVICE CORPORATION              ELP030  
00019      SKIP3                                                        ELP030  
00020  ENVIRONMENT DIVISION.                                            ELP030  
00021                                                                   ELP030  
00022  CONFIGURATION SECTION.                                           ELP030  
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELP030  
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELP030  
00025      EJECT                                                        ELP030  
00026 ******************************************************************ELP030  
00027 *                                                                *ELP030  
00028 *                    ELS ABEND PROCESSING                        *ELP030  
00029 *                                                                *ELP030  
00030 *   ELP030 - THIS PROGRAM READS, FORMATS AND PRINTS THE          *ELP030  
00031 *            SSD ABEND REPORT.  THE READS ARE DONE BY THE        *ELP030  
00032 *            ELPRDSNA SUBROUTINE.  THE WRITES ARE DONE BY        *ELP030  
00033 *            ELPPR SUBROUTINE.                                   *ELP030  
00034 *                                                                *ELP030  
00035 ******************************************************************ELP030  
00036 *                                                                *ELP030  
00037 *                      MAINTENANCE HISTORY                       *ELP030  
00038 *                                                                *ELP030  
00039 *  MOD     DATE     BY  DRPT                ACTION               *ELP030  
00040 * ----- ----------- --- ----- ---------------------------------- *ELP030  
00041 * 01.00 21-JUN-1989 AKK       CREATED                            *ELP030  
00042 * 01.01 21-SEP-1989 EGL      -ADDED COPYBOOK ELSNAPPC TO DES-    *ELP030  
00043 *                             CRIBE THE HEADER RECORD.           *ELP030  
00044 *                            -ADDED CALL TO ELPSUMRY             *ELP030  
00045 * 01.02 08-NOV-1989 EGL      -ADDED ABILITY TO DUMP RECORD KEYS  *ELP030  
00046 *                             FROM IOP BLOCKS                    *ELP030  
00047 * 01.03 20-MAR-1990 AKK      -ADDED CALL TO ELPNFCV AND ELPNFDE  *ELP030  
00048 * 01.04 05-NOV-1997 AKK      -ADDED SUPPORT FOR YR 2000 AND TX   *ELP030  
00049 *                             MERGE.                             *ELP030  
00050 ******************************************************************ELP030  
00051                                                                   ELP030  
00052  INPUT-OUTPUT SECTION.                                            ELP030  
00053  FILE-CONTROL.                                                    ELP030  
00054 *                                                                 ELP030  
00055  DATA DIVISION.                                                   ELP030  
00056  FILE SECTION.                                                    ELP030  
00057 /                                                                 ELP030  
00058  WORKING-STORAGE SECTION.                                         ELP030  
00059  01  FILLER                 PIC X(17)   VALUE '*START OF ELP030*'.ELP030  
00060 *                                                                 ELP030  
00061  01  WS-SWITCHES.                                                 ELP030  
00062      05 WS-EOF-INDICATOR    PIC X(03)   VALUE 'YES'.              ELP030  
00063         88  MORE-RECORDS-TO-READ        VALUE 'YES'.              ELP030  
00064         88  EOF-FOUND                   VALUE 'EOF'.              ELP030  
00065      05 WS-PAGE6-SWITCH     PIC X(03)   VALUE 'YES'.              ELP030  
00066         88  FIRST-PRINT-PAGE6           VALUE 'YES'.              ELP030  
00067         88  NOT-FIRST-PRINT-PAGE6       VALUE 'NO '.              ELP030  
00068      05 WS-PAGE6-PRINT-SW   PIC X(03)   VALUE 'YES'.              ELP030  
00069         88  OK-TO-PRINT-PAGE6           VALUE 'YES'.              ELP030  
00070         88  PAGE6-PRINTED               VALUE 'NO '.              ELP030  
00071      05 WS-TABLE1-2-SWITCH  PIC X(03)   VALUE SPACE.              ELP030  
00072         88  TABLE1-OK                   VALUE 'TB1'.              ELP030  
00073         88  TABLE2-OK                   VALUE 'TB2'.              ELP030  
00074         88  TABLE1-AND-TABLE2-OK        VALUE 'T12'.              ELP030  
00075 *                                                                 ELP030  
00076  01  WS-WORK-AREAS.                                               ELP030  
00077      05 WS-TABLE-SUB       PIC S9(04)   COMP.                     ELP030  
00078      05 WS-CONTRACT-SUB    PIC S9(04)   COMP.                     ELP030  
00079      05 WS-NO-SSCB-MESSAGE PIC  X(36)   VALUE                     ELP030  
00080         'NO SYSTEM STATUS CONTROL BLOCK FOUND'.                   ELP030  
00081      05 WS-NO-DUMP-HEADER  PIC  X(26)   VALUE                     ELP030  
00082         'NO DUMP HEADER BLOCK FOUND'.                             ELP030  
00083      05 WS-NO-SSD-RPT-MSG  PIC  X(49)   VALUE                     ELP030  
00084         'NO SSD ABENDS PRINTED, NOTHING TO REPORT FOR SSD.'.      ELP030  
00085      05 WS-END-REPORT-MSG  PIC  X(18)   VALUE                     ELP030  
00086         'END OF SSD REPORT.'.                                     ELP030  
00087      05 WS-DATE            PIC 9(06).                             ELP030  
00088      05 WS-JUL-DATE        PIC 9(05).                             ELP030  
00089      05 WS-GREG-DATE       PIC 9(06).                             ELP030  
00090      05 WS-INVALID-ABEND-CODE                                     ELP030  
00091                            PIC X(04)    VALUE 'ELXX'.             ELP030  
00092 *                                                                 ELP030  
00093  01  PROGRAM-CONSTANTS.                                           ELP030  
00094      05  PC-ELP030         PIC X(06)    VALUE 'ELP030'.           ELP030  
00095      05  PC-ELSKTBCC       PIC X(07)    VALUE 'ELSKTBC'.          ELP030  
00096      05  PC-ELSKTBGC       PIC X(07)    VALUE 'ELSKTBG'.          ELP030  
00097      05  PC-ELSKTBSC       PIC X(07)    VALUE 'ELSKTBS'.          ELP030  
00098      05  PC-ELSMEMSC       PIC X(07)    VALUE 'ELSMEMS'.          ELP030  
00099      05  PC-ELSGRPSP       PIC X(08)    VALUE 'ELSGRPSP'.         ELP030  
00100      05  PC-ELSCONIB       PIC X(08)    VALUE 'ELSCONIB'.         ELP030  
00101      05  PC-ELSCONPB       PIC X(08)    VALUE 'ELSCONPB'.         ELP030  
00102      05  PC-ELSCONIS       PIC X(08)    VALUE 'ELSCONIS'.         ELP030  
00103      05  PC-ELSCONPS       PIC X(08)    VALUE 'ELSCONPS'.         ELP030  
00104      05  PC-NO-SSCB        PIC X(07)    VALUE 'NO SSCB'.          ELP030  
00105 /                                                                 ELP030  
00106 *                                                                 ELP030  
00107      COPY HSCDATES.                                               ELP030  
00108 *                                                                 ELP030  
00109 /                                                                 ELP030  
00110 ***************************************************************** ELP030  
00111 *THE COPYBOOKS USED ONLY FOR ABEND PROCESSING ARE AS FOLLOWS:   * ELP030  
00112 *   ELSNAPRC, ELSPRCBC, ELSPRKYC, ELSRDPAC.                     * ELP030  
00113 ***************************************************************** ELP030  
00114 *                                                                 ELP030  
00115      COPY ELSNAPRC.                                               ELP030  
00116 /                                                                 ELP030  
00117      COPY ELSPRCBC.                                               ELP030  
00118 /                                                                 ELP030  
00119      COPY ELSPRKYC.                                               ELP030  
00120 /                                                                 ELP030  
00121      COPY ELSRDPAC.                                               ELP030  
00122 /                                                                 ELP030  
00123  LINKAGE SECTION.                                                 ELP030  
00124 ****************************************************************  ELP030  
00125 *THE COPYBOOKS THAT WILL SUPPLY THE DATA APPEARING ON THE      *  ELP030  
00126 *REPORT ARE AS FOLLOWS:                                        *  ELP030  
00127 * ELSMEMSC, ELSKTBCC, ELSKTBGC, ELSKTBSC, ELSSSCBC, GCCONTRC,  *  ELP030  
00128 * GCGRPSC.                                                     *  ELP030  
00129 ****************************************************************  ELP030  
00130 *                                                                 ELP030  
00131      COPY ELSMEMSC.                                               ELP030  
00132 /                                                                 ELP030  
00133      COPY ELSKTBCC.                                               ELP030  
00134 /                                                                 ELP030  
00135      COPY ELSKTBGC.                                               ELP030  
00136 /                                                                 ELP030  
00137      COPY ELSKTBSC.                                               ELP030  
00138 /                                                                 ELP030  
00139      COPY ELSNAPPC.                                               ELP030  
00140 /                                                                 ELP030  
00141      COPY ELSSSCBC.                                               ELP030  
00142 /                                                                 ELP030  
00143  01  CONTRACT-RECORD.                                             ELP030  
00144      COPY GCCONTRC.                                               ELP030  
00145 /                                                                 ELP030  
00146  01  GROUP-SPECIFIC-RECORD.                                       ELP030  
00147      COPY GCGROUPC.                                               ELP030  
00148 *                                                                 ELP030  
00149  PROCEDURE DIVISION.                                              ELP030  
00150      PERFORM 0000-INITIALIZATION.                                 ELP030  
00151      PERFORM 1000-PROCESS-SSD-ABEND-REPORT                        ELP030  
00152           UNTIL RDP-CLOSE-FILE.                                   ELP030  
00153      PERFORM 9000-TERMINATION.                                    ELP030  
00154      GOBACK.                                                      ELP030  
00155 *                                                                 ELP030  
00156  0000-INITIALIZATION.                                             ELP030  
00157      PERFORM 0050-INITIALIZE-ELSPRKY.                             ELP030  
00158      SET RDP-OPEN-FILE TO TRUE.                                   ELP030  
00159      CALL 'ELPRDSNA' USING RDP-READ-PARAMETERS.                   ELP030  
00160      MOVE ALL '?' TO PCB-GROUP-NUM                                ELP030  
00161                      PCB-PLAN-CODE                                ELP030  
00162                      PCB-PKG-CODE                                 ELP030  
00163                      PCB-SECTION-NUM.                             ELP030  
00164      SET PCB-OPEN-PRINTER TO TRUE.                                ELP030  
00165      MOVE PC-ELP030 TO PCB-PRINT-TEXT.                            ELP030  
00166      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELP030  
00167      PERFORM 1360-DO-CALL-TO-TCDTES.                              ELP030  
00168 *                                                                 ELP030  
00169  0050-INITIALIZE-ELSPRKY.                                         ELP030  
00170      INITIALIZE KYA-GROUP-SPEC-KEY                                ELP030  
00171                 KYA-CONTRACT-TYPE.                                ELP030  
00172      PERFORM VARYING WS-CONTRACT-SUB FROM 1                       ELP030  
00173             BY 1 UNTIL WS-CONTRACT-SUB > 4                        ELP030  
00174           SET KYA-IDX TO WS-CONTRACT-SUB                          ELP030  
00175           MOVE SPACES TO KYA-CONTRACT-KEYS (KYA-IDX)              ELP030  
00176      END-PERFORM.                                                 ELP030  
00177 *                                                                 ELP030  
00178  1000-PROCESS-SSD-ABEND-REPORT.                                   ELP030  
00179      PERFORM 1100-EDIT-AND-PRINT-SSD-REPORT                       ELP030  
00180             UNTIL EOF-FOUND.                                      ELP030  
00181      IF EOF-FOUND                                                 ELP030  
00182         SET RDP-CLOSE-FILE TO TRUE                                ELP030  
00183         CALL 'ELPRDSNA' USING RDP-READ-PARAMETERS                 ELP030  
00184         PERFORM 5000-VERIFY-PAGE6-PRINTED                         ELP030  
00185         INITIALIZE PCB-PAGE-HEADING-INFO                          ELP030  
00186         CALL 'ELPSUMRY' USING PCB-PRINT-CONTROL-BLOCK             ELP030  
00187         CALL 'ELPNFCV' USING PCB-PRINT-CONTROL-BLOCK              ELP030  
00188         CALL 'ELPNFDE' USING PCB-PRINT-CONTROL-BLOCK              ELP030  
00189         IF PCB-PAGE-NUMBER = ZERO                                 ELP030  
00190            PERFORM 1025-PRINT-NO-REPORT-MSG                       ELP030  
00191         ELSE                                                      ELP030  
00192            PERFORM 1050-PRINT-END-REPORT-MSG.                     ELP030  
00193 *                                                                 ELP030  
00194  1025-PRINT-NO-REPORT-MSG.                                        ELP030  
00195      SET PCB-PRINT-LINE TO TRUE.                                  ELP030  
00196      SET PCB-EJECT TO TRUE.                                       ELP030  
00197      MOVE WS-NO-SSD-RPT-MSG TO PCB-PRINT-TEXT.                    ELP030  
00198      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELP030  
00199 *                                                                 ELP030  
00200  1050-PRINT-END-REPORT-MSG.                                       ELP030  
00201      SET PCB-PRINT-LINE TO TRUE.                                  ELP030  
00202      SET PCB-EJECT TO TRUE.                                       ELP030  
00203      MOVE WS-END-REPORT-MSG TO PCB-PRINT-TEXT.                    ELP030  
00204      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELP030  
00205 *                                                                 ELP030  
00206  1100-EDIT-AND-PRINT-SSD-REPORT.                                  ELP030  
00207      SET RDP-READ-FILE TO TRUE.                                   ELP030  
00208      CALL 'ELPRDSNA' USING RDP-READ-PARAMETERS.                   ELP030  
00209      PERFORM 1110-VERIFY-ABEND-AND-RETURNS.                       ELP030  
00210      IF RDP-EOF                                                   ELP030  
00211          SET EOF-FOUND TO TRUE                                    ELP030  
00212      ELSE                                                         ELP030  
00213          PERFORM 1125-EDIT-SNAP-SHOT-RECORD.                      ELP030  
00214 *                                                                 ELP030  
00215  1110-VERIFY-ABEND-AND-RETURNS.                                   ELP030  
00216      IF (RDP-ABEND-CODE = WS-INVALID-ABEND-CODE)                  ELP030  
00217         OR RDP-SHORT-RECORD                                       ELP030  
00218         SET RDP-CALL-OK TO TRUE                                   ELP030  
00219         SET RDP-READ-FILE TO TRUE                                 ELP030  
00220         CALL 'ELPRDSNA' USING RDP-READ-PARAMETERS.                ELP030  
00221 *                                                                 ELP030  
00222  1125-EDIT-SNAP-SHOT-RECORD.                                      ELP030  
00223      INITIALIZE WS-TABLE1-2-SWITCH.                               ELP030  
00224      PERFORM VARYING WS-TABLE-SUB                                 ELP030  
00225         FROM 1 BY 1 UNTIL                                         ELP030  
00226           TABLE1-OK OR (WS-TABLE-SUB > ART-MAX-ABEND-CODES)       ELP030  
00227              SET ART-ABEND-IDX TO WS-TABLE-SUB                    ELP030  
00228              IF ART-ABEND-CODE (WS-TABLE-SUB) = RDP-ABEND-CODE    ELP030  
00229                SET TABLE1-OK TO TRUE                              ELP030  
00230              END-IF                                               ELP030  
00231      END-PERFORM.                                                 ELP030  
00232      IF TABLE1-OK                                                 ELP030  
00233         PERFORM VARYING WS-TABLE-SUB                              ELP030  
00234            FROM 1 BY 1 UNTIL                                      ELP030  
00235              TABLE1-AND-TABLE2-OK OR                              ELP030  
00236                (WS-TABLE-SUB > ART-MAX-DDNAMES)                   ELP030  
00237                 SET ART-DDNAME-IDX TO WS-TABLE-SUB                ELP030  
00238                 IF ART-DDNAME (WS-TABLE-SUB) = RDP-DDNAME         ELP030  
00239                    SET TABLE1-AND-TABLE2-OK TO TRUE               ELP030  
00240                 END-IF                                            ELP030  
00241         END-PERFORM.                                              ELP030  
00242      IF TABLE1-AND-TABLE2-OK                                      ELP030  
00243        SET ART-SSD-IDX TO ART-PRINT-COL (ART-ABEND-IDX)           ELP030  
00244        IF ART-PRINT-SSD (ART-DDNAME-IDX, ART-SSD-IDX)             ELP030  
00245           PERFORM 1200-DO-SSD-REPORT-PRINT.                       ELP030  
00246 *                                                                 ELP030  
00247  1200-DO-SSD-REPORT-PRINT.                                        ELP030  
00248      SET PCB-PRINT-LINE TO TRUE.                                  ELP030  
00249      EVALUATE TRUE                                                ELP030  
00250         WHEN  (RDP-ABEND-CODE NOT = PCB-ABEND-CODE                ELP030  
00251             OR RDP-ABEND-DATE NOT = PCB-ABEND-DATE-CENTURY        ELP030  
00252             OR RDP-ABEND-TIME NOT = PCB-ABEND-TIME                ELP030  
00253             OR RDP-TERMINAL-ID NOT = PCB-TERMINAL-ID)             ELP030  
00254               PERFORM 1300-CONTROL-BREAK                          ELP030  
00255         WHEN   RDP-DDNAME = PC-ELSKTBCC                           ELP030  
00256               PERFORM 1215-PRINT-REPORT-PAGE2                     ELP030  
00257         WHEN   RDP-DDNAME = PC-ELSKTBGC                           ELP030  
00258               PERFORM 1230-PRINT-REPORT-PAGE3                     ELP030  
00259         WHEN   RDP-DDNAME = PC-ELSKTBSC                           ELP030  
00260               PERFORM 1245-PRINT-REPORT-PAGE4                     ELP030  
00261         WHEN   RDP-DDNAME = PC-ELSMEMSC                           ELP030  
00262               PERFORM 1260-PRINT-REPORT-PAGE5                     ELP030  
00263         WHEN  (RDP-DDNAME = PC-ELSGRPSP OR PC-ELSCONIB            ELP030  
00264                  OR PC-ELSCONPB OR PC-ELSCONIS OR PC-ELSCONPS)    ELP030  
00265               PERFORM 1400-ACCUM-REPORT-PAGE6-DATA                ELP030  
00266         WHEN    RDP-I-O-ITEM                                      ELP030  
00267               PERFORM 1270-PRINT-RECORD-KEY                       ELP030  
00268      END-EVALUATE.                                                ELP030  
00269 *                                                                 ELP030  
00270  1215-PRINT-REPORT-PAGE2.                                         ELP030  
00271      CALL 'ELUADDRS' USING RDP-AREA                               ELP030  
00272                     ADDRESS OF KTC-GCCONTR-KEY-TABLE.             ELP030  
00273      CALL 'ELPPRKTC' USING PCB-PRINT-CONTROL-BLOCK                ELP030  
00274                            KTC-GCCONTR-KEY-TABLE.                 ELP030  
00275 *                                                                 ELP030  
00276  1230-PRINT-REPORT-PAGE3.                                         ELP030  
00277      CALL 'ELUADDRS' USING RDP-AREA                               ELP030  
00278                     ADDRESS OF KTG-GCGRPSPC-KEY-TABLE.            ELP030  
00279      CALL 'ELPPRKTG' USING PCB-PRINT-CONTROL-BLOCK                ELP030  
00280                            KTG-GCGRPSPC-KEY-TABLE.                ELP030  
00281 *                                                                 ELP030  
00282  1245-PRINT-REPORT-PAGE4.                                         ELP030  
00283      CALL 'ELUADDRS' USING RDP-AREA                               ELP030  
00284                     ADDRESS OF KTS-SECTIONS-KEY-TABLE.            ELP030  
00285      CALL 'ELPPRKTS' USING PCB-PRINT-CONTROL-BLOCK                ELP030  
00286                            KTS-SECTIONS-KEY-TABLE.                ELP030  
00287 *                                                                 ELP030  
00288  1260-PRINT-REPORT-PAGE5.                                         ELP030  
00289      CALL 'ELUADDRS' USING RDP-AREA                               ELP030  
00290                     ADDRESS OF MSI-MEMBERSHIP-INTERFACE.          ELP030  
00291      CALL 'ELPMEMSC' USING PCB-PRINT-CONTROL-BLOCK                ELP030  
00292                            MSI-MEMBERSHIP-INTERFACE.              ELP030  
00293 *                                                                 ELP030  
00294  1270-PRINT-RECORD-KEY.                                           ELP030  
00295      CALL 'ELPPRDMP' USING PCB-PRINT-CONTROL-BLOCK                ELP030  
00296                            RDP-READ-PARAMETERS.                   ELP030  
00297 *                                                                 ELP030  
00298  1300-CONTROL-BREAK.                                              ELP030  
00299      CALL 'ELUADDRS' USING RDP-AREA                               ELP030  
00300               ADDRESS OF SSP-SNAP-SHOT-PREFIX.                    ELP030  
00301      CALL 'ELUADDRS' USING SSP-STATUS-REC (1)                     ELP030  
00302               ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.             ELP030  
00303      IF FIRST-PRINT-PAGE6                                         ELP030  
00304         SET NOT-FIRST-PRINT-PAGE6 TO TRUE                         ELP030  
00305      ELSE                                                         ELP030  
00306         CALL 'ELPPRKY' USING PCB-PRINT-CONTROL-BLOCK              ELP030  
00307                              KYA-KEY-HOLD-AREA                    ELP030  
00308         PERFORM 0050-INITIALIZE-ELSPRKY                           ELP030  
00309         SET PAGE6-PRINTED TO TRUE.                                ELP030  
00310      IF RDP-DUMP-HEADER                                           ELP030  
00311         PERFORM 1355-MOVE-ABEND-INFO                              ELP030  
00312         PERFORM 1350-PRINT-REPORT-PAGE1                           ELP030  
00313      ELSE                                                         ELP030  
00314         SET PCB-PRINT-LINE TO TRUE                                ELP030  
00315         SET PCB-EJECT TO TRUE                                     ELP030  
00316         MOVE WS-NO-DUMP-HEADER TO PCB-PRINT-TEXT                  ELP030  
00317         PERFORM 1355-MOVE-ABEND-INFO                              ELP030  
00318         CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.               ELP030  
00319 *                                                                 ELP030  
00320  1350-PRINT-REPORT-PAGE1.                                         ELP030  
00321      SET PCB-PRINT-LINE TO TRUE.                                  ELP030  
00322      SET PCB-EJECT TO TRUE.                                       ELP030  
00323      IF RDP-DDNAME = PC-NO-SSCB                                   ELP030  
00324           MOVE WS-NO-SSCB-MESSAGE TO PCB-PRINT-TEXT               ELP030  
00325           CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK              ELP030  
00326      ELSE                                                         ELP030  
00327         CALL 'ELPPRSSB' USING PCB-PRINT-CONTROL-BLOCK             ELP030  
00328                               SSB-SELECTOR-STATUS-CTL-BLK.        ELP030  
00329 *                                                                 ELP030  
00330  1355-MOVE-ABEND-INFO.                                            ELP030  
00331      IF RDP-DUMP-HEADER                                           ELP030  
00332          MOVE SSB-PLAN-CODE TO PCB-PLAN-CODE                      ELP030  
00333          MOVE SSB-GROUP-NUMBER TO PCB-GROUP-NUM                   ELP030  
00334          MOVE SSB-SECTN-NO TO PCB-SECT-NUMBER                     ELP030  
00335          MOVE SSB-PKG-CODE TO PCB-PKG-CODE.                       ELP030  
00336      SET OK-TO-PRINT-PAGE6 TO TRUE.                               ELP030  
00337      MOVE RDP-ABEND-CODE TO PCB-ABEND-CODE.                       ELP030  
00338      MOVE RDP-ABEND-DATE TO PCB-ABEND-DATE-CENTURY.               ELP030  
00339      MOVE RDP-ABEND-TIME TO PCB-ABEND-TIME.                       ELP030  
00340      MOVE RDP-CICS-APPL-ID TO PCB-CICS-APPL-ID.                   ELP030  
00341      MOVE RDP-CICS-SYSTEM-ID TO PCB-CICS-SYSTEM-ID.               ELP030  
00342      MOVE RDP-TERMINAL-ID TO PCB-TERMINAL-ID.                     ELP030  
00343 *                                                                 ELP030  
00344  1360-DO-CALL-TO-TCDTES.                                          ELP030  
00345      CALL 'TCDTES' USING HSCDATES.                                ELP030  
00346      MOVE TODAY TO WS-DATE.                                       ELP030  
00347      MOVE WS-DATE TO PCB-PRINT-DATE.                              ELP030  
00348 *                                                                 ELP030  
00349  1400-ACCUM-REPORT-PAGE6-DATA.                                    ELP030  
00350      EVALUATE TRUE                                                ELP030  
00351         WHEN RDP-DDNAME = PC-ELSGRPSP                             ELP030  
00352            PERFORM 1415-CAPTURE-GRP-SPEC-DATA                     ELP030  
00353         WHEN RDP-DDNAME = PC-ELSCONIB                             ELP030  
00354            PERFORM 1430-CAPTURE-INST-BASIC-DATA                   ELP030  
00355         WHEN RDP-DDNAME = PC-ELSCONIS                             ELP030  
00356            PERFORM 1445-CAPTURE-INST-SUPP-DATA                    ELP030  
00357         WHEN RDP-DDNAME = PC-ELSCONPB                             ELP030  
00358            PERFORM 1460-CAPTURE-PROF-BASIC-DATA                   ELP030  
00359         WHEN RDP-DDNAME = PC-ELSCONPS                             ELP030  
00360            PERFORM 1475-CAPTURE-PROF-SUPP-DATA.                   ELP030  
00361 *                                                                 ELP030  
00362  1415-CAPTURE-GRP-SPEC-DATA.                                      ELP030  
00363      CALL 'ELUADDRS' USING RDP-AREA                               ELP030  
00364          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELP030  
00365      MOVE GCG-PLAN-CODE TO KYA-GSK-PLAN-CODE.                     ELP030  
00366      MOVE GCG-GRP-NO TO KYA-GSK-GROUP.                            ELP030  
00367      MOVE GCG-SECTN-NO TO KYA-GSK-SECT.                           ELP030  
00368      MOVE GCG-PKG-CODE TO KYA-GSK-PKG-CODE.                       ELP030  
00369      MOVE GCG-FAM-REL-LVL TO KYA-GSK-FAM-REL-LVL.                 ELP030  
00370      MOVE GCG-EFFDT-CEN  TO  KYA-GSK-EFF-DT-CENTURY.              ELP030  
00371      SET KYA-GSK-PRINT TO TRUE.                                   ELP030  
00372 *                                                                 ELP030  
00373  1430-CAPTURE-INST-BASIC-DATA.                                    ELP030  
00374      SET KYA-CONTRACT-IB TO TRUE.                                 ELP030  
00375      PERFORM 1435-SET-IDX-AND-KEYS.                               ELP030  
00376 *                                                                 ELP030  
00377  1435-SET-IDX-AND-KEYS.                                           ELP030  
00378      SET KYA-IDX TO KYA-CONTRACT-TYPE.                            ELP030  
00379      CALL 'ELUADDRS' USING RDP-AREA                               ELP030  
00380          ADDRESS OF CONTRACT-RECORD.                              ELP030  
00381      MOVE GCT-PLAN-CODE TO KYA-CK-PLAN-CODE (KYA-IDX).            ELP030  
00382      MOVE GCT-GRP-NO TO KYA-CK-GRP (KYA-IDX).                     ELP030  
00383      MOVE GCT-SECTN-NO TO KYA-CK-SECT (KYA-IDX).                  ELP030  
00384      MOVE GCT-PKG-CODE TO KYA-CK-PKG-CODE (KYA-IDX).              ELP030  
00385      MOVE GCT-L-O-B TO KYA-CK-L-O-B (KYA-IDX).                    ELP030  
00386      MOVE GCT-PROVDR-CONTROL                                      ELP030  
00387                    TO KYA-CK-PROVDR-CONTROL (KYA-IDX).            ELP030  
00388      MOVE GCT-FAM-REL-LVL TO KYA-CK-FAM-REL-LVL (KYA-IDX).        ELP030  
00389      MOVE GCT-EFFDT-CEN  TO KYA-CK-EFF-DT-CENTURY (KYA-IDX).      ELP030  
00390      SET KYA-CK-PRINT (KYA-IDX) TO TRUE.                          ELP030  
00391 *                                                                 ELP030  
00392  1445-CAPTURE-INST-SUPP-DATA.                                     ELP030  
00393      SET KYA-CONTRACT-IS TO TRUE.                                 ELP030  
00394      PERFORM 1435-SET-IDX-AND-KEYS.                               ELP030  
00395 *                                                                 ELP030  
00396  1460-CAPTURE-PROF-BASIC-DATA.                                    ELP030  
00397      SET KYA-CONTRACT-PB TO TRUE.                                 ELP030  
00398      PERFORM 1435-SET-IDX-AND-KEYS.                               ELP030  
00399 *                                                                 ELP030  
00400  1475-CAPTURE-PROF-SUPP-DATA.                                     ELP030  
00401      SET KYA-CONTRACT-PS TO TRUE.                                 ELP030  
00402      PERFORM 1435-SET-IDX-AND-KEYS.                               ELP030  
00403 *                                                                 ELP030  
00404  5000-VERIFY-PAGE6-PRINTED.                                       ELP030  
00405      IF OK-TO-PRINT-PAGE6                                         ELP030  
00406         CALL 'ELPPRKY' USING PCB-PRINT-CONTROL-BLOCK              ELP030  
00407                              KYA-KEY-HOLD-AREA.                   ELP030  
00408 *                                                                 ELP030  
00409  9000-TERMINATION.                                                ELP030  
00410      SET PCB-CLOSE-PRINTER TO TRUE.                               ELP030  
00411      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELP030  
