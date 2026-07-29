00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELGGVLX 
00003  PROGRAM-ID.         ELGGVLX.                                        LV002
00004                                                                   ELGGVLX 
00005  AUTHOR.             GEORGE E MOORE.                              ELGGVLX 
00006                                                                   ELGGVLX 
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELGGVLX 
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELGGVLX 
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELGGVLX 
00010                      233 N. MICHIGAN AVE                          ELGGVLX 
00011                      CHICAGO, ILLINOIS 60601                      ELGGVLX 
00012                                                                   ELGGVLX 
00013  DATE-WRITTEN.       23-MAR-1990.                                 ELGGVLX 
00014                                                                   ELGGVLX 
00015  DATE-COMPILED.                                                   ELGGVLX 
00016                                                                   ELGGVLX 
00017  SECURITY.           COPYRIGHT 1990,                              ELGGVLX 
00018                      HEALTH CARE SERVICE CORPORATION.             ELGGVLX 
00019      SKIP3                                                        ELGGVLX 
00020  ENVIRONMENT DIVISION.                                            ELGGVLX 
00021                                                                   ELGGVLX 
00022  CONFIGURATION SECTION.                                           ELGGVLX 
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELGGVLX 
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELGGVLX 
00025      EJECT                                                        ELGGVLX 
00026 ***************************************************************   ELGGVLX 
00027 *                                                             *   ELGGVLX 
00028 *                       PROGRAM ABSTRACT                      *   ELGGVLX 
00029 *                                                             *   ELGGVLX 
00030 *   PROGRAM NAME:  E.L.S. #GVL TABULAR DISPLAY SUBROUTINE     *   ELGGVLX 
00031 *                                                             *   ELGGVLX 
00032 *   PROGRAM ID:    ELGGVLX                                    *   ELGGVLX 
00033 *                                                             *   ELGGVLX 
00034 *   PURPOSE:   TO DISPLAY SPECIAL PROVIDER CONTROL VALUE      *   ELGGVLX 
00035 *              CONSIDERATIONS BASED ON SPECIFIC PROVIDER      *   ELGGVLX 
00036 *              NUMBERS.                                       *   ELGGVLX 
00037 *                                                             *   ELGGVLX 
00038 *   RECORDS                                                   *   ELGGVLX 
00039 *   ACCESSED:  #GVL INTERNAL TABULAR                          *   ELGGVLX 
00040 *                                                             *   ELGGVLX 
00041 ***************************************************************   ELGGVLX 
00042 *                                                             *   ELGGVLX 
00043 *                      MAINTENANCE HISTORY                    *   ELGGVLX 
00044 *                                                             *   ELGGVLX 
00045 *  MOD     DATE     BY  DRPT                ACTION            *   ELGGVLX 
00046 * ----- ----------- --- ----- --------------------------------*   ELGGVLX 
00047 * 01.00 26-FEB-1990 GEM       CREATED                         *   ELGGVLX 
00048 *                                                             *   ELGGVLX 
00049 * 01.01 12-JUN-1990 AKK       ADDED CODE TO HANDLE MULTI TAB  *   ELGGVLX 
00050 *                             ON GVL TYPES OF TABULARS.  THIS *   ELGGVLX 
00051 *                             IS FOR GROUPS WHO HAVE MORE THAN*   ELGGVLX 
00052 *                             THE MAX NUMBER OF PROV ALLOWED  *   ELGGVLX 
00053 *                                                             *   ELGGVLX 
00054 * 01.02 20-JUN-1990 AKK       ADD THE WORD 'LEVEL' TO WS-     *   ELGGVLX 
00055 *                             FACILITY-INC-MSG.               *   ELGGVLX 
00056 *                             SET ADDITIONAL SWITCHES WHEN    *   ELGGVLX 
00057 *                             CALLING THE PROVIDER MASTER FILE*   ELGGVLX 
00058 *                             INORDER TO GET ALL PROVIDERS    *   ELGGVLX 
00059 *                             WHETHER ACTIVE/INACTIVE ETC.    *   ELGGVLX 
00060 *       21-AUG-2003 AKK       GEN TO TEST COMPILE ORDER         * ELGGVLX 
00061 ***************************************************************   ELGGVLX 
00062                                                                   ELGGVLX 
00063  DATA DIVISION.                                                   ELGGVLX 
00064                                                                   ELGGVLX 
00065  WORKING-STORAGE SECTION.                                         ELGGVLX 
00066  01  WS-MISC.                                                     ELGGVLX 
00067      05  WS-BEGIN                      PIC X(26)  VALUE           ELGGVLX 
00068      '*** ELTGVLX  WS BEGINS ***'.                                ELGGVLX 
00069      05  WS-99                         PIC 9(02)  VALUE 99.       ELGGVLX 
00070      05  WS-MR-TAB-SLOT                PIC S9(07) VALUE 0.        ELGGVLX 
00071      05  WS-MR-TAB-SLOT-MAX.                                      ELGGVLX 
00072          10 WS-MR-FIRST4               PIC S9(05).                ELGGVLX 
00073          10 WS-MR-MAX                  PIC S9(02).                ELGGVLX 
00074      05  WS-MULTI-TAB-SWITCH           PIC X      VALUE SPACE.    ELGGVLX 
00075          88 PROCESSING-MULTI-TAB                  VALUE 'Y'.      ELGGVLX 
00076          88 NOT-PROCESSING-MULTI-TAB              VALUE 'N'.      ELGGVLX 
00077      05  WS-FIRST-TIME-SWITCH          PIC X      VALUE SPACE.    ELGGVLX 
00078          88 FIRST-TIME-MULTI-TAB                  VALUE 'Y'.      ELGGVLX 
00079          88 NOT-FIRST-TIME-MULTI-TAB              VALUE 'N'.      ELGGVLX 
00080      05  WS-MULTI-FOUND-SWITCH         PIC X      VALUE SPACE.    ELGGVLX 
00081          88 LAST-MULTI-FOUND                      VALUE 'Y'.      ELGGVLX 
00082                                                                   ELGGVLX 
00083  01  WS-COUNTERS.                                                 ELGGVLX 
00084      05  WS-NBR-LVL-1-TXT              PIC S9(04) VALUE 0 COMP.   ELGGVLX 
00085      05  WS-NBR-LVL-2-TXT              PIC S9(04) VALUE 0 COMP.   ELGGVLX 
00086                                                                   ELGGVLX 
00087  01  WS-SUBSCRIPTS.                                               ELGGVLX 
00088      05  WS-SUB            COMP SYNC   PIC S9(03) VALUE 0.        ELGGVLX 
00089      05  WS-GVL-SUB        COMP SYNC   PIC S9(03) VALUE 0.        ELGGVLX 
00090      05  WS-LVL-SUB        COMP SYNC   PIC S9(03) VALUE 0.        ELGGVLX 
00091                                                                   ELGGVLX 
00092  01  WS-LVL-1-OUTPUT.                                             ELGGVLX 
00093      05  WS-LVL-1-LNE      OCCURS   4  TIMES.                     ELGGVLX 
00094          10  FILLER                    PIC X(01).                 ELGGVLX 
00095          10  WS-LVL-1-LIT              PIC X(09).                 ELGGVLX 
00096          10  WS-LVL-1-TXT              PIC X(70).                 ELGGVLX 
00097                                                                   ELGGVLX 
00098  01  WS-LVL-2-OUTPUT.                                             ELGGVLX 
00099      05  WS-LVL-2-LNE      OCCURS   4  TIMES.                     ELGGVLX 
00100          10  FILLER                    PIC X(01).                 ELGGVLX 
00101          10  WS-LVL-2-LIT              PIC X(09).                 ELGGVLX 
00102          10  WS-LVL-2-TXT              PIC X(70).                 ELGGVLX 
00103                                                                   ELGGVLX 
00104  01  PROGRAM-CONSTANTS.                                           ELGGVLX 
00105      05  WS-GETMAIN-SWITCH             PIC X(01)  VALUE 'N'.      ELGGVLX 
00106          88 WS-GETMAIN-DONE                       VALUE 'Y'.      ELGGVLX 
00107      05  WS-Z                          PIC X(01)  VALUE 'Z'.      ELGGVLX 
00108                                                                   ELGGVLX 
00109  01  WS-FACILITY-INC-MSG.                                         ELGGVLX 
00110      05  FILLER                        PIC X(35)  VALUE           ELGGVLX 
00111           'FACILITY PROVIDER(S) WHOSE CHARGES '.                  ELGGVLX 
00112      05  FILLER                        PIC X(35)  VALUE           ELGGVLX 
00113           'ARE PROCESSED AT A SPECIAL BENEFIT '.                  ELGGVLX 
00114      05  FILLER                        PIC X(05)  VALUE           ELGGVLX 
00115          'LEVEL'.                                                 ELGGVLX 
00116                                                                   ELGGVLX 
00117  01  WS-FACILITY-EXC-MSG.                                         ELGGVLX 
00118      05  FILLER                        PIC X(35)  VALUE           ELGGVLX 
00119           'FACILITY PROVIDER(S) WHOSE CHARGES '.                  ELGGVLX 
00120      05  FILLER                        PIC X(36)  VALUE           ELGGVLX 
00121           'ARE NOT PROCESSED AT SPECIAL BENEFIT'.                 ELGGVLX 
00122      05  FILLER                        PIC X(08)  VALUE SPACES.   ELGGVLX 
00123                                                                   ELGGVLX 
00124  01  WS-PROFESNL-INC-MSG.                                         ELGGVLX 
00125      05  FILLER                        PIC X(39)  VALUE           ELGGVLX 
00126           'PROFESSIONAL PROVIDER(S) WHOSE CHARGES '.              ELGGVLX 
00127      05  FILLER                        PIC X(32)  VALUE           ELGGVLX 
00128           'ARE PROCESSED AT SPECIAL BENEFIT'.                     ELGGVLX 
00129      05  FILLER                        PIC X(08)  VALUE SPACES.   ELGGVLX 
00130                                                                   ELGGVLX 
00131  01  WS-PROFESNL-EXC-MSG.                                         ELGGVLX 
00132      05  FILLER                        PIC X(39)  VALUE           ELGGVLX 
00133           'PROFESSIONAL PROVIDER(S) WHOSE CHARGES '.              ELGGVLX 
00134      05  FILLER                        PIC X(36)  VALUE           ELGGVLX 
00135           'ARE NOT PROCESSED AT SPECIAL BENEFIT'.                 ELGGVLX 
00136      05  FILLER                        PIC X(05)  VALUE SPACES.   ELGGVLX 
00137      EJECT                                                        ELGGVLX 
00138  LINKAGE SECTION.                                                 ELGGVLX 
00139  01  DFHCOMMAREA.                                                 ELGGVLX 
00140      COPY ELSCOMMC.                                               ELGGVLX 
00141 /       * COMMON INTERFACE AREA                                   ELGGVLX 
00142      COPY ELSCIA2C.                                               ELGGVLX 
00143 /       * CODES MANUAL DESCRIPTION LINES                          ELGGVLX 
00144      COPY ELSCMDSC.                                               ELGGVLX 
00145 /       * CODES MANUAL INTERFACE                                  ELGGVLX 
00146      COPY ELSCMIFC.                                               ELGGVLX 
00147 /       * INPUT OUTPUT PARAMETERS                                 ELGGVLX 
00148      COPY ELSIOPMC.                                               ELGGVLX 
00149 /       * FILE KEY WORK AREA                                      ELGGVLX 
00150      COPY ELSKEYSC.                                               ELGGVLX 
00151 /       * COF OUTPUT INTERFACE                                    ELGGVLX 
00152      COPY ELSOUTPC.                                               ELGGVLX 
00153 /       * SRP SUBROUTINE PARAMETERS                               ELGGVLX 
00154      COPY ELSSRTPC.                                               ELGGVLX 
00155 /       * TCAR COMPRESSION WORK AREA                              ELGGVLX 
00156      COPY ELSTCWAC.                                               ELGGVLX 
00157 /       * SELECTOR STATUS CONTROL BLOCK                           ELGGVLX 
00158      COPY ELSSSCBC.                                               ELGGVLX 
00159 /       * PROVIDER NUM TO NAME DB PARMS                           ELGGVLX 
00160  01  PDB-IO-AREA.                                                 ELGGVLX 
00161      COPY DBPIOPMC.                                               ELGGVLX 
00162 /       * PROVIDER MASTER RECORD                                  ELGGVLX 
00163  01  PROVIDER-MSTR-REC.                                           ELGGVLX 
00164      COPY PROVMSTR.                                               ELGGVLX 
00165 /                                                                 ELGGVLX 
00166  01  GVL-TABULAR-REC-AREA.                                        ELGGVLX 
00167      05  GVL-RECORD.                                              ELGGVLX 
00168 *  *   *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *    ELGGVLX 
00169 *   NOTE :                                                        ELGGVLX 
00170 *   THIS IS A GENERIC RECORD FORMAT USED FOR THE                  ELGGVLX 
00171 *   PROCESSING OF THE GROUP VARIABLE LEVEL TABULARS:              ELGGVLX 
00172 *   #GVLF  #GVLG   #GVLH   #GVLP   #GVLQ   #GVLR                  ELGGVLX 
00173 *  *   *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *    ELGGVLX 
00174        10  GVL-TAB-PROV-ID.                                       ELGGVLX 
00175          15  GVL-PROVISION-ID                     PIC X(6).       ELGGVLX 
00176          15  GVL-PROVISION-SLOT-NO        COMP-3  PIC S9(7).      ELGGVLX 
00177        10  GVL-CONTROL-ELEMENTS.                                  ELGGVLX 
00178          15  GVL-TAB-FORMAT-CODE                  PIC X.          ELGGVLX 
00179            88  GVL-TAB-FORMAT-T                VALUE 'T'.         ELGGVLX 
00180          15  GVL-DT-OF-LAST-CHANGE        COMP-3  PIC S9(5).      ELGGVLX 
00181          15  FILLER                               PIC X(21).      ELGGVLX 
00182          15  GVL-PROV-CNTL-IND-1                  PIC  X(1).      ELGGVLX 
00183          15  GVL-PROV-CNTL-IND-2                  PIC  X(1).      ELGGVLX 
00184          15  GVL-ENTRY-COUNT              COMP-3  PIC S9(5).      ELGGVLX 
00185          15  GVL-INC-EXC-IND                      PIC X.          ELGGVLX 
00186            88  GVL-PROVIDER-INCLUDED            VALUE 'I'.        ELGGVLX 
00187            88  GVL-PROVIDER-EXCLUDED            VALUE 'E'.        ELGGVLX 
00188        07  GVL-ENTRIES.                                           ELGGVLX 
00189        10  GVL-ENTRY  OCCURS 1 TO 395 TIMES                       ELGGVLX 
00190                              DEPENDING ON GVL-ENTRY-COUNT         ELGGVLX 
00191                              INDEXED BY GVL-INDEX.                ELGGVLX 
00192          15  GVL-PROVIDER-NO-ARG                  PIC  X(10).     ELGGVLX 
00193      EJECT                                                        ELGGVLX 
00194  PROCEDURE DIVISION.                                              ELGGVLX 
00195  GVL-TABULAR-DISPLAY-SUBROUTINE.                                  ELGGVLX 
00196      PERFORM ADDRESS-WRK-AREA-N-CNTL-BLKS.                        ELGGVLX 
00197      PERFORM MAINLINE.                                            ELGGVLX 
00198      GOBACK.                                                      ELGGVLX 
00199                                                                   ELGGVLX 
00200  ADDRESS-WRK-AREA-N-CNTL-BLKS.                                    ELGGVLX 
00201      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    ELGGVLX 
00202          PERFORM COMMAREA-LENGTH-ERROR.                           ELGGVLX 
00203      CALL 'ELUINISM' USING DFHCOMMAREA                            ELGGVLX 
00204          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELGGVLX 
00205      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGGVLX 
00206      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGVLX 
00207          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELGGVLX 
00208      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELGGVLX 
00209      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGVLX 
00210          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELGGVLX 
00211      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGGVLX 
00212      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGVLX 
00213          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELGGVLX 
00214      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGGVLX 
00215      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGVLX 
00216          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELGGVLX 
00217      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELGGVLX 
00218      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGVLX 
00219          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELGGVLX 
00220      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELGGVLX 
00221      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGVLX 
00222          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELGGVLX 
00223      INITIALIZE CMF-CODES-MANUAL-INTERFACE                        ELGGVLX 
00224                 TCAR-FROM-AREA.                                   ELGGVLX 
00225      SET CIA-DBPIOPM-DDN  TO TRUE.                                ELGGVLX 
00226      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGVLX 
00227          ADDRESS OF PDB-IO-AREA.                                  ELGGVLX 
00228      IF CIA-RC-PTR-NULL                                           ELGGVLX 
00229          PERFORM GETMAIN-IO-PARM-AREA.                            ELGGVLX 
00230      IF WS-GETMAIN-DONE                                           ELGGVLX 
00231          PERFORM DO-ADDRESS-OF-PDB.                               ELGGVLX 
00232                                                                   ELGGVLX 
00233  DO-ADDRESS-OF-PDB.                                               ELGGVLX 
00234      SET CIA-DBPIOPM-DDN  TO TRUE.                                ELGGVLX 
00235      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGVLX 
00236          ADDRESS OF PDB-IO-AREA.                                  ELGGVLX 
00237                                                                   ELGGVLX 
00238  COMMAREA-LENGTH-ERROR.                                           ELGGVLX 
00239      SET CIA-AB-DFHCOMMAREA  TO  TRUE.                            ELGGVLX 
00240      EXEC CICS  ABEND  ABCODE(CIA-ABCODE)  END-EXEC.              ELGGVLX 
00241                                                                   ELGGVLX 
00242  MAINLINE.                                                        ELGGVLX 
00243      SET NOT-PROCESSING-MULTI-TAB TO TRUE.                        ELGGVLX 
00244      INITIALIZE WS-MR-TAB-SLOT                                    ELGGVLX 
00245                 WS-MR-TAB-SLOT-MAX.                               ELGGVLX 
00246      IF SRP-TABULAR-SLOT-NO > 99999                               ELGGVLX 
00247         MOVE SRP-TABULAR-SLOT-NO TO WS-MR-TAB-SLOT                ELGGVLX 
00248                                     WS-MR-TAB-SLOT-MAX            ELGGVLX 
00249         MOVE WS-99 TO WS-MR-MAX                                   ELGGVLX 
00250         SET PROCESSING-MULTI-TAB TO TRUE                          ELGGVLX 
00251         SET FIRST-TIME-MULTI-TAB TO TRUE                          ELGGVLX 
00252         PERFORM PROCESS-TOPIC-FOR-MULTI                           ELGGVLX 
00253             WITH TEST AFTER                                       ELGGVLX 
00254              UNTIL LAST-MULTI-FOUND                               ELGGVLX 
00255                OR WS-MR-TAB-SLOT = WS-MR-MAX                      ELGGVLX 
00256         SET NOT-PROCESSING-MULTI-TAB TO TRUE                      ELGGVLX 
00257      ELSE                                                         ELGGVLX 
00258         PERFORM DO-MAIN-PROCESSING                                ELGGVLX 
00259      END-IF.                                                      ELGGVLX 
00260                                                                   ELGGVLX 
00261  DO-MAIN-PROCESSING.                                              ELGGVLX 
00262      PERFORM READ-INTERNAL-TABULAR-RECORD.                        ELGGVLX 
00263      IF FIRST-TIME-MULTI-TAB OR NOT-PROCESSING-MULTI-TAB          ELGGVLX 
00264         PERFORM PROCESS-SCREEN-TEXT.                              ELGGVLX 
00265      IF LAST-MULTI-FOUND                                          ELGGVLX 
00266         CONTINUE                                                  ELGGVLX 
00267      ELSE                                                         ELGGVLX 
00268         PERFORM LIST-GVL-TABULAR-CONTENTS                         ELGGVLX 
00269            VARYING WS-GVL-SUB FROM 1 BY 1                         ELGGVLX 
00270              UNTIL WS-GVL-SUB =  GVL-ENTRY-COUNT OR               ELGGVLX 
00271                GVL-PROVIDER-NO-ARG (WS-GVL-SUB) = HIGH-VALUES     ELGGVLX 
00272      END-IF.                                                      ELGGVLX 
00273                                                                   ELGGVLX 
00274  PROCESS-TOPIC-FOR-MULTI.                                         ELGGVLX 
00275      PERFORM DO-MAIN-PROCESSING.                                  ELGGVLX 
00276      ADD 1 TO WS-MR-TAB-SLOT.                                     ELGGVLX 
00277      MOVE WS-MR-TAB-SLOT TO SRP-TABULAR-SLOT-NO.                  ELGGVLX 
00278                                                                   ELGGVLX 
00279  READ-INTERNAL-TABULAR-RECORD.                                    ELGGVLX 
00280      SET CIA-GCTABULR-DDN TO TRUE.                                ELGGVLX 
00281      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGVLX 
00282         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                   ELGGVLX 
00283      IF CIA-RC-PTR-NULL                                           ELGGVLX 
00284         PERFORM GETMAIN-IO-PARM-AREA.                             ELGGVLX 
00285      SET CIA-GCTABULR-DDN TO TRUE.                                ELGGVLX 
00286      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGVLX 
00287         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                   ELGGVLX 
00288      MOVE SRP-INTERNAL-TAB TO KWA-GCTABULR-KEY.                   ELGGVLX 
00289      SET IOP-RD                                                   ELGGVLX 
00290          IOP-FCQ-NONE                                             ELGGVLX 
00291          IOP-KVQ-EQ                                               ELGGVLX 
00292          IOP-STG-MODE-LOCATE TO TRUE.                             ELGGVLX 
00293      MOVE SRP-INTERNAL-TAB TO KWA-GCTABULR-KEY.                   ELGGVLX 
00294      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELGGVLX 
00295      MOVE SPACES TO IOP-AIX-DDNAME.                               ELGGVLX 
00296      SET IOP-REC-PTR TO NULL.                                     ELGGVLX 
00297      EXEC CICS LINK                                               ELGGVLX 
00298           PROGRAM('ELUIOPGM')                                     ELGGVLX 
00299           COMMAREA(DFHCOMMAREA)                                   ELGGVLX 
00300      END-EXEC.                                                    ELGGVLX 
00301                                                                   ELGGVLX 
00302      IF NOT IOP-RC-OK                                             ELGGVLX 
00303         PERFORM DETERMINE-HOW-TO-HANDLE.                          ELGGVLX 
00304      IF IOP-RC-OK                                                 ELGGVLX 
00305         SET ADDRESS OF GVL-TABULAR-REC-AREA TO IOP-REC-PTR.       ELGGVLX 
00306                                                                   ELGGVLX 
00307  DETERMINE-HOW-TO-HANDLE.                                         ELGGVLX 
00308      IF PROCESSING-MULTI-TAB                                      ELGGVLX 
00309         IF FIRST-TIME-MULTI-TAB AND NOT IOP-RC-OK                 ELGGVLX 
00310            PERFORM TABULAR-NOT-FOUND                              ELGGVLX 
00311         ELSE                                                      ELGGVLX 
00312            IF NOT-FIRST-TIME-MULTI-TAB AND IOP-RC-NOTFND          ELGGVLX 
00313               SET LAST-MULTI-FOUND TO TRUE                        ELGGVLX 
00314            END-IF                                                 ELGGVLX 
00315         END-IF                                                    ELGGVLX 
00316      ELSE                                                         ELGGVLX 
00317         IF NOT IOP-RC-OK                                          ELGGVLX 
00318            PERFORM TABULAR-NOT-FOUND                              ELGGVLX 
00319         END-IF                                                    ELGGVLX 
00320      END-IF.                                                      ELGGVLX 
00321                                                                   ELGGVLX 
00322  PROCESS-SCREEN-TEXT.                                             ELGGVLX 
00323      PERFORM INSERT-BLANK-LINE.                                   ELGGVLX 
00324      PERFORM PROCESS-PROV-CNT-IND.                                ELGGVLX 
00325      IF PROCESSING-MULTI-TAB                                      ELGGVLX 
00326         SET NOT-FIRST-TIME-MULTI-TAB TO TRUE                      ELGGVLX 
00327      END-IF.                                                      ELGGVLX 
00328      IF GVL-PROVISION-ID = '#GVLF ' OR '#GVLG ' OR '#GVLH '       ELGGVLX 
00329         IF  GVL-INC-EXC-IND  = 'I'                                ELGGVLX 
00330             PERFORM PROCESS-INST-INCLUDE-TEXT                     ELGGVLX 
00331         ELSE                                                      ELGGVLX 
00332            IF  GVL-INC-EXC-IND  = 'E'                             ELGGVLX 
00333                PERFORM PROCESS-INST-EXCLUDE-TEXT                  ELGGVLX 
00334           END-IF.                                                 ELGGVLX 
00335                                                                   ELGGVLX 
00336      IF GVL-PROVISION-ID = '#GVLP ' OR '#GVLQ ' OR '#GVLR '       ELGGVLX 
00337       IF  GVL-INC-EXC-IND  = 'I'                                  ELGGVLX 
00338           PERFORM PROCESS-PROF-INCLUDE-TEXT                       ELGGVLX 
00339       ELSE                                                        ELGGVLX 
00340       IF  GVL-INC-EXC-IND  = 'E'                                  ELGGVLX 
00341           PERFORM PROCESS-PROF-EXCLUDE-TEXT                       ELGGVLX 
00342       END-IF.                                                     ELGGVLX 
00343      PERFORM INSERT-BLANK-LINE.                                   ELGGVLX 
00344                                                                   ELGGVLX 
00345                                                                   ELGGVLX 
00346  PROCESS-PROV-CNT-IND.                                            ELGGVLX 
00347      IF GVL-PROV-CNTL-IND-1 NOT = SPACES                          ELGGVLX 
00348         PERFORM PROCESS-PROV-CNTL-IND-1.                          ELGGVLX 
00349      IF GVL-PROV-CNTL-IND-2 NOT = SPACES                          ELGGVLX 
00350         AND GVL-PROV-CNTL-IND-2 NOT = WS-Z                        ELGGVLX 
00351         PERFORM PROCESS-PROV-CNTL-IND-2.                          ELGGVLX 
00352                                                                   ELGGVLX 
00353  PROCESS-PROV-CNTL-IND-1.                                         ELGGVLX 
00354      MOVE GVL-PROVISION-ID    TO CMF-RECORD-PREFIX.               ELGGVLX 
00355      MOVE 'PROV-CNTL-IND-1'   TO CMF-ELEMENT-SYSTEM-NAME.         ELGGVLX 
00356      MOVE GVL-PROV-CNTL-IND-1 TO CMF-CODE-VALUE.                  ELGGVLX 
00357      PERFORM TRANSLATE-PROV-CNTL-IND.                             ELGGVLX 
00358      PERFORM WITH TEST BEFORE VARYING WS-LVL-SUB FROM 1 BY 1      ELGGVLX 
00359                                 UNTIL WS-LVL-SUB  >  4            ELGGVLX 
00360          MOVE SPACES TO WS-LVL-1-LNE (WS-LVL-SUB)                 ELGGVLX 
00361      END-PERFORM.                                                 ELGGVLX 
00362      MOVE 'LEVEL 1: ' TO WS-LVL-1-LIT (1).                        ELGGVLX 
00363      MOVE ZEROES TO WS-NBR-LVL-1-TXT.                             ELGGVLX 
00364      MOVE 1 TO WS-SUB.                                            ELGGVLX 
00365      PERFORM MOVE-COMPRESSED-LVL-1-TXT                            ELGGVLX 
00366         VARYING WS-LVL-SUB FROM 1 BY 1                            ELGGVLX 
00367            UNTIL WS-LVL-SUB GREATER THAN                          ELGGVLX 
00368               TCAR-OUTPUT-FIELDS-USED OR 4.                       ELGGVLX 
00369                                                                   ELGGVLX 
00370  MOVE-COMPRESSED-LVL-1-TXT.                                       ELGGVLX 
00371      MOVE TCAR-OPF-DATA (WS-SUB) TO WS-LVL-1-TXT (WS-LVL-SUB).    ELGGVLX 
00372      ADD 1 TO WS-NBR-LVL-1-TXT.                                   ELGGVLX 
00373      ADD 1 TO WS-SUB.                                             ELGGVLX 
00374                                                                   ELGGVLX 
00375  PROCESS-PROV-CNTL-IND-2.                                         ELGGVLX 
00376      MOVE GVL-PROVISION-ID TO CMF-RECORD-PREFIX.                  ELGGVLX 
00377      MOVE 'PROV-CNTL-IND-2' TO CMF-ELEMENT-SYSTEM-NAME.           ELGGVLX 
00378      MOVE GVL-PROV-CNTL-IND-2 TO CMF-CODE-VALUE.                  ELGGVLX 
00379      PERFORM TRANSLATE-PROV-CNTL-IND.                             ELGGVLX 
00380      PERFORM WITH TEST BEFORE VARYING WS-LVL-SUB FROM 1 BY 1      ELGGVLX 
00381                                 UNTIL WS-LVL-SUB  >  4            ELGGVLX 
00382          MOVE SPACES TO WS-LVL-2-LNE (WS-LVL-SUB)                 ELGGVLX 
00383      END-PERFORM.                                                 ELGGVLX 
00384      MOVE 'LEVEL 2: ' TO WS-LVL-2-LIT (1).                        ELGGVLX 
00385      MOVE ZEROES TO WS-NBR-LVL-2-TXT.                             ELGGVLX 
00386      MOVE 1 TO WS-SUB.                                            ELGGVLX 
00387      PERFORM MOVE-COMPRESSED-LVL-2-TXT                            ELGGVLX 
00388         VARYING WS-LVL-SUB FROM 1 BY 1                            ELGGVLX 
00389            UNTIL WS-LVL-SUB GREATER THAN                          ELGGVLX 
00390               TCAR-OUTPUT-FIELDS-USED OR 4.                       ELGGVLX 
00391                                                                   ELGGVLX 
00392  MOVE-COMPRESSED-LVL-2-TXT.                                       ELGGVLX 
00393      MOVE TCAR-OPF-DATA (WS-SUB) TO WS-LVL-2-TXT (WS-LVL-SUB).    ELGGVLX 
00394      ADD 1 TO WS-NBR-LVL-2-TXT.                                   ELGGVLX 
00395      ADD 1 TO WS-SUB.                                             ELGGVLX 
00396                                                                   ELGGVLX 
00397  PROCESS-INST-INCLUDE-TEXT.                                       ELGGVLX 
00398      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGGVLX 
00399      MOVE WS-FACILITY-INC-MSG TO                                  ELGGVLX 
00400         COF-DTL-LINE(COF-NBR-DTL-LINES).                          ELGGVLX 
00401      PERFORM PROCESS-LEVEL-TEXT-FOR-PRT.                          ELGGVLX 
00402                                                                   ELGGVLX 
00403  PROCESS-INST-EXCLUDE-TEXT.                                       ELGGVLX 
00404      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGGVLX 
00405      MOVE WS-FACILITY-EXC-MSG TO                                  ELGGVLX 
00406         COF-DTL-LINE(COF-NBR-DTL-LINES).                          ELGGVLX 
00407      PERFORM PROCESS-LEVEL-TEXT-FOR-PRT.                          ELGGVLX 
00408                                                                   ELGGVLX 
00409  PROCESS-PROF-INCLUDE-TEXT.                                       ELGGVLX 
00410      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGGVLX 
00411      MOVE WS-PROFESNL-INC-MSG TO                                  ELGGVLX 
00412         COF-DTL-LINE(COF-NBR-DTL-LINES).                          ELGGVLX 
00413      PERFORM PROCESS-LEVEL-TEXT-FOR-PRT.                          ELGGVLX 
00414                                                                   ELGGVLX 
00415  PROCESS-PROF-EXCLUDE-TEXT.                                       ELGGVLX 
00416      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGGVLX 
00417      MOVE WS-PROFESNL-EXC-MSG TO                                  ELGGVLX 
00418         COF-DTL-LINE(COF-NBR-DTL-LINES).                          ELGGVLX 
00419      PERFORM PROCESS-LEVEL-TEXT-FOR-PRT.                          ELGGVLX 
00420                                                                   ELGGVLX 
00421  PROCESS-LEVEL-TEXT-FOR-PRT.                                      ELGGVLX 
00422      PERFORM MOVE-LEVEL-1-TO-PRT                                  ELGGVLX 
00423        VARYING WS-LVL-SUB FROM 1 BY 1                             ELGGVLX 
00424           UNTIL WS-LVL-SUB > WS-NBR-LVL-1-TXT.                    ELGGVLX 
00425      PERFORM MOVE-LEVEL-2-TO-PRT                                  ELGGVLX 
00426        VARYING WS-LVL-SUB FROM 1 BY 1                             ELGGVLX 
00427           UNTIL WS-LVL-SUB > WS-NBR-LVL-2-TXT.                    ELGGVLX 
00428      PERFORM CALL-OUTPUT.                                         ELGGVLX 
00429                                                                   ELGGVLX 
00430  MOVE-LEVEL-1-TO-PRT.                                             ELGGVLX 
00431      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGGVLX 
00432      MOVE WS-LVL-1-LNE (WS-LVL-SUB)                               ELGGVLX 
00433         TO COF-DTL-LINE (COF-NBR-DTL-LINES).                      ELGGVLX 
00434                                                                   ELGGVLX 
00435  MOVE-LEVEL-2-TO-PRT.                                             ELGGVLX 
00436      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGGVLX 
00437      MOVE WS-LVL-2-LNE (WS-LVL-SUB)                               ELGGVLX 
00438         TO COF-DTL-LINE (COF-NBR-DTL-LINES).                      ELGGVLX 
00439                                                                   ELGGVLX 
00440  TRANSLATE-PROV-CNTL-IND.                                         ELGGVLX 
00441      EXEC CICS LINK                                               ELGGVLX 
00442           PROGRAM('ELUCMIF')                                      ELGGVLX 
00443           COMMAREA(DFHCOMMAREA)                                   ELGGVLX 
00444      END-EXEC.                                                    ELGGVLX 
00445      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGGVLX 
00446      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGVLX 
00447         ADDRESS OF CMF-DESCR.                                     ELGGVLX 
00448      PERFORM MOVE-CMF-DESCR-FOR-COMPRESS                          ELGGVLX 
00449         VARYING WS-SUB FROM 1 BY 1                                ELGGVLX 
00450            UNTIL WS-SUB > CMF-NBR-DESCR-LINES.                    ELGGVLX 
00451      PERFORM DO-TEXT-COMPRESSION.                                 ELGGVLX 
00452      MOVE +70 TO TCAR-OUTPUT-FIELD-1-LEN                          ELGGVLX 
00453                  TCAR-OUTPUT-FIELD-2-LEN                          ELGGVLX 
00454                  TCAR-OUTPUT-FIELD-3-LEN                          ELGGVLX 
00455                  TCAR-OUTPUT-FIELD-4-LEN.                         ELGGVLX 
00456      MOVE +04 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGGVLX 
00457      PERFORM DO-TEXT-UNSTRING.                                    ELGGVLX 
00458                                                                   ELGGVLX 
00459  LIST-GVL-TABULAR-CONTENTS.                                       ELGGVLX 
00460      MOVE GVL-PROVIDER-NO-ARG (WS-GVL-SUB)                        ELGGVLX 
00461        TO PDB-I-PROVIDER-NBR.                                     ELGGVLX 
00462      MOVE 'PRVDR' TO PDB-I-REQUEST-TYPE-N.                        ELGGVLX 
00463      MOVE '02' TO PDB-I-VERSION.                                  ELGGVLX 
00464      MOVE 'D' TO PDB-I-ACCESS-MODE.                               ELGGVLX 
00465      SET PDB-I-SS-ACTIVE-INACTIVE-PROV TO TRUE.                   ELGGVLX 
00466      SET PDB-I-UA-UNLIMITED-ACCESS TO TRUE.                       ELGGVLX 
00467      EXEC CICS LINK                                               ELGGVLX 
00468           PROGRAM('DBPIOC')                                       ELGGVLX 
00469           COMMAREA(PDB-IO-AREA)                                   ELGGVLX 
00470      END-EXEC.                                                    ELGGVLX 
00471                                                                   ELGGVLX 
00472      IF PDB-O-RC-SUCCESSFUL                                       ELGGVLX 
00473         PERFORM STRING-PROVIDER-NAME-AND-TYPE                     ELGGVLX 
00474      ELSE                                                         ELGGVLX 
00475         PERFORM PROVIDER-FILE-PROBLEM.                            ELGGVLX 
00476                                                                   ELGGVLX 
00477      PERFORM CALL-OUTPUT.                                         ELGGVLX 
00478                                                                   ELGGVLX 
00479  STRING-PROVIDER-NAME-AND-TYPE.                                   ELGGVLX 
00480      SET ADDRESS OF PROVIDER-MSTR-REC                             ELGGVLX 
00481                     TO ADDRESS OF PDB-O-RECORD-AREA.              ELGGVLX 
00482      STRING GVL-PROVIDER-NO-ARG (WS-GVL-SUB),                     ELGGVLX 
00483         ' ',  DELIMITED BY SIZE,                                  ELGGVLX 
00484         PFM-PAYEE-NAME-1,  DELIMITED BY '  ',                     ELGGVLX 
00485         ' ',  DELIMITED BY SIZE,                                  ELGGVLX 
00486         PFM-PAYEE-NAME-2,   ','    DELIMITED BY ' ',              ELGGVLX 
00487         INTO  TCAR-FROM-AREA.                                     ELGGVLX 
00488      PERFORM TRANSLATE-PROVIDER-CODE.                             ELGGVLX 
00489                                                                   ELGGVLX 
00490  TRANSLATE-PROVIDER-CODE.                                         ELGGVLX 
00491      MOVE '#PVE'            TO CMF-RECORD-PREFIX.                 ELGGVLX 
00492 *    MOVE SRP-TABULAR-ID    TO CMF-RECORD-PREFIX.                 ELGGVLX 
00493      MOVE 'PROVIDER-CODE'   TO CMF-ELEMENT-SYSTEM-NAME.           ELGGVLX 
00494      MOVE PFM-PROVIDER-TYPE TO CMF-CODE-VALUE.                    ELGGVLX 
00495      EXEC CICS LINK                                               ELGGVLX 
00496           PROGRAM('ELUCMIF')                                      ELGGVLX 
00497           COMMAREA(DFHCOMMAREA)                                   ELGGVLX 
00498      END-EXEC.                                                    ELGGVLX 
00499      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGGVLX 
00500      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGVLX 
00501         ADDRESS OF CMF-DESCR.                                     ELGGVLX 
00502      PERFORM MOVE-CMF-DESCR-FOR-COMPRESS                          ELGGVLX 
00503         VARYING WS-SUB FROM 1 BY 1                                ELGGVLX 
00504            UNTIL WS-SUB  >  CMF-NBR-DESCR-LINES.                  ELGGVLX 
00505      PERFORM DO-TEXT-COMPRESSION.                                 ELGGVLX 
00506      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                          ELGGVLX 
00507                  TCAR-OUTPUT-FIELD-2-LEN                          ELGGVLX 
00508                  TCAR-OUTPUT-FIELD-3-LEN                          ELGGVLX 
00509                  TCAR-OUTPUT-FIELD-4-LEN.                         ELGGVLX 
00510      MOVE +04 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGGVLX 
00511      PERFORM DO-TEXT-UNSTRING.                                    ELGGVLX 
00512      PERFORM MOVE-COMPRESSED-PHRASE                               ELGGVLX 
00513         VARYING WS-SUB FROM 1 BY 1                                ELGGVLX 
00514            UNTIL WS-SUB > TCAR-OUTPUT-FIELDS-USED.                ELGGVLX 
00515                                                                   ELGGVLX 
00516  GETMAIN-IO-PARM-AREA.                                            ELGGVLX 
00517      SET CIA-STG-GETMAIN TO TRUE.                                 ELGGVLX 
00518      SET WS-GETMAIN-DONE TO TRUE.                                 ELGGVLX 
00519      EXEC CICS LINK                                               ELGGVLX 
00520           PROGRAM('ELUSTGMG')                                     ELGGVLX 
00521           COMMAREA(DFHCOMMAREA)                                   ELGGVLX 
00522      END-EXEC.                                                    ELGGVLX 
00523                                                                   ELGGVLX 
00524                                                                   ELGGVLX 
00525  MOVE-COMPRESSED-PHRASE.                                          ELGGVLX 
00526      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGGVLX 
00527      MOVE TCAR-OPF-DATA(WS-SUB) TO                                ELGGVLX 
00528         COF-DTL-LINE(COF-NBR-DTL-LINES).                          ELGGVLX 
00529                                                                   ELGGVLX 
00530  MOVE-CMF-DESCR-FOR-COMPRESS.                                     ELGGVLX 
00531      COMPUTE TCAR-FROM-SUB = WS-SUB +  2.                         ELGGVLX 
00532      MOVE CMF-DESCR-LINE (WS-SUB) TO                              ELGGVLX 
00533           TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGGVLX 
00534                                                                   ELGGVLX 
00535  INSERT-BLANK-LINE.                                               ELGGVLX 
00536      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGGVLX 
00537      MOVE SPACES TO COF-DTL-LINE(COF-NBR-DTL-LINES).              ELGGVLX 
00538      PERFORM CALL-OUTPUT.                                         ELGGVLX 
00539                                                                   ELGGVLX 
00540  DO-TEXT-COMPRESSION.                                             ELGGVLX 
00541      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGGVLX 
00542                                                                   ELGGVLX 
00543  DO-TEXT-UNSTRING.                                                ELGGVLX 
00544      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGGVLX 
00545                                                                   ELGGVLX 
00546  CALL-OUTPUT.                                                     ELGGVLX 
00547      EXEC CICS LINK                                               ELGGVLX 
00548           PROGRAM('ELUOUTPT')                                     ELGGVLX 
00549           COMMAREA(DFHCOMMAREA)                                   ELGGVLX 
00550      END-EXEC.                                                    ELGGVLX 
00551      INITIALIZE TCAR-FROM-AREA.                                   ELGGVLX 
00552                                                                   ELGGVLX 
00553  TABULAR-NOT-FOUND.                                               ELGGVLX 
00554      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELGGVLX 
00555      EXEC CICS ABEND                                              ELGGVLX 
00556           ABCODE(CIA-ABCODE)                                      ELGGVLX 
00557      END-EXEC.                                                    ELGGVLX 
00558                                                                   ELGGVLX 
00559  PROVIDER-FILE-PROBLEM.                                           ELGGVLX 
00560      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGGVLX 
00561      STRING 'PROVIDER NUMBER ''', PDB-I-PROVIDER-NBR              ELGGVLX 
00562          ''' IS NOT ON FILE'  DELIMITED BY SIZE                   ELGGVLX 
00563                  INTO COF-DTL-LINE(COF-NBR-DTL-LINES).            ELGGVLX 
