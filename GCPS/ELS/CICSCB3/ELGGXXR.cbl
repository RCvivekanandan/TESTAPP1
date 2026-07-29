00001 *      LAST MAINTENANCE TIME: 15.15.41  DATE: 06/11/91            09/03/03
00002  IDENTIFICATION DIVISION.                                         ELGGXXR 
00003 *                                                                    LV002
00004  PROGRAM-ID.         ELGGXXR.                                     ELGGXXR 
00005 *                                                                 ELGGXXR 
00006  AUTHOR.             DAVID SECOR  OF  A.C.I.                      ELGGXXR 
00007 *                                                                 ELGGXXR 
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELGGXXR 
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELGGXXR 
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELGGXXR 
00011                      233 N. MICHIGAN AVE                          ELGGXXR 
00012                      CHICAGO, ILLINOIS 60601                      ELGGXXR 
00013 *                                                                 ELGGXXR 
00014 *                                                                 ELGGXXR 
00015  DATE-WRITTEN.       31-JUL-1987.                                 ELGGXXR 
00016 *                                                                 ELGGXXR 
00017  DATE-COMPILED.                                                   ELGGXXR 
00018 *                                                                 ELGGXXR 
00019  SECURITY.           COPYRIGHT 1987,                              ELGGXXR 
00020                      HEALTH CARE SERVICE CORPORATION              ELGGXXR 
00021 *                                                                 ELGGXXR 
00022 ******************************************************************ELGGXXR 
00023 ******************************************************************ELGGXXR 
00024 *   ELGGXXR                                                       ELGGXXR 
00025 *                                                                 ELGGXXR 
00026 *                        PROGRAM ABSTRACT                         ELGGXXR 
00027 *                                                                 ELGGXXR 
00028 *   PROGRAM NAME:   E.L.S. GROUP SPECIFIC RELATED PROCEDURE       ELGGXXR 
00029 *                   COST CONTAINMENT SUBROUTINE                   ELGGXXR 
00030 *                                                                 ELGGXXR 
00031 *   PROGRAM I.D.:   ELGGXXR                                       ELGGXXR 
00032 *                                                                 ELGGXXR 
00033 *   PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE FIELDS  ELGGXXR 
00034 *              FOR RELATED PROCEDURE COST CONTAINMENT PROGRAMS.   ELGGXXR 
00035 *              SINCE THE RECORD FORMATS FOR THESE COST            ELGGXXR 
00036 *              CONTAINMENT RECORDS ARE IDENTICAL ONLY ONE         ELGGXXR 
00037 *              FORMAT HAS BEEN USED IN THIS PROGRAM.              ELGGXXR 
00038 *                                                                 ELGGXXR 
00039 *   RECORDS                                                       ELGGXXR 
00040 *   ACCESSED:  GHOR, GMOR, GMPR, GMSR, GMCR AND GPAR ALL          ELGGXXR 
00041 *              GROUP SPECIFIC COST CONTAINMENT RECORDS.           ELGGXXR 
00042 *                                                                 ELGGXXR 
00043 ******************************************************************ELGGXXR 
00044 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       ELGGXXR 
00045 *       *-*         U P D A T E   H I S T O R Y         *-*       ELGGXXR 
00046 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       ELGGXXR 
00047 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*ELGGXXR 
00048 *                                                                 ELGGXXR 
00049 *  ELS 2.0   07/31/87  DES  INCEPTION.                            ELGGXXR 
00050 *                                                                 ELGGXXR 
00051 *  ELS 2.0   09/01/87  DES  FIXED PROCEDURE FILE DB ACCESS TO USE ELGGXXR 
00052 *                          THE SYSTEM ID BASED ON THE FIELD LENGTHELGGXXR 
00053 *                                                                 ELGGXXR 
00054 *  ELS 2.1   09/23/87  AKK  REMOVED INSTRUCTIONS THAT CAUSED PDB- ELGGXXR 
00055 *                           I-SERVICE-CODE-SYSTEM-ID TO PRINT     ELGGXXR 
00056 *                           AS PART OF THE PROCEDURE CODE.        ELGGXXR 
00057 *                                                                 ELGGXXR 
00058 *  ELS 2.2   01/06/89  AKK  CHANGED PLELITCOMP TO ELITCOMPPL,     ELGGXXR 
00059 *                           MADE CHANGES TO ACCOMOMDATE           ELGGXXR 
00060 *                           STORAGE MANAGEMENT ENHANCEMENTS AND   ELGGXXR 
00061 *                           ADDED LOGIC TO INCLUDE READING OF THE ELGGXXR 
00062 *                           CORPORATE LIST OVERRIDE INDICATOR FOR ELGGXXR 
00063 *                           THE GMOR, GMSR, GPAR, AND GMPR TABU-  ELGGXXR 
00064 *                           LARS.                                 ELGGXXR 
00065 *                                                                 ELGGXXR 
00066 * ELS 2.3    05/08/91  AKK  ADD CODE FOR GMCR                     ELGGXXR 
00067 *                                                                 ELGGXXR 
00068 * ELS 3.0    05/07/03  AKK  RECOMPILE FOR CHANGES TO PROCEDURE    ELGGXXR 
00069 *                           AND DIAG CODES                        ELGGXXR 
00070 *            06/22/03  AKK  ADD WORKING STORASGE FIELDS TO        ELGGXXR 
00071 *                           SEND ONLY 6 CHARS TO PROCEDURE MASTER ELGGXXR 
00072 *            06/29/03  AKK  MOVING WS- SIX CHAR FILED TO CHECK    ELGGXXR 
00073 *                           FOR SPACES PRIOR TO SENDING TO PRCDR  ELGGXXR 
00074 ******************************************************************ELGGXXR 
00075 /                                                                 ELGGXXR 
00076  ENVIRONMENT DIVISION.                                            ELGGXXR 
00077  CONFIGURATION SECTION.                                           ELGGXXR 
00078  SOURCE-COMPUTER.    IBM-3081.                                    ELGGXXR 
00079  OBJECT-COMPUTER.    IBM-3090.                                    ELGGXXR 
00080 /                                                                 ELGGXXR 
00081  DATA DIVISION.                                                   ELGGXXR 
00082  WORKING-STORAGE SECTION.                                         ELGGXXR 
00083  77  PAN-VALET PIC X(28) VALUE '002 ELGGXXRPL  05/09/91 1547'.    ELGGXXR 
00084  01  WS-BEGIN               PIC X(24)  VALUE                      ELGGXXR 
00085      'ELGGXXR WORKING STORAGE*'.                                  ELGGXXR 
00086 /     W O R K F I E L D S   A N D   S W I T C H E S               ELGGXXR 
00087  01  WS-MISC-WORK.                                                ELGGXXR 
00088      05  WS-SUB             PIC S9(4)  COMP SYNC VALUE ZEROES.    ELGGXXR 
00089      05  WS-BLANK-CNT       PIC S9(4)  COMP SYNC VALUE ZEROES.    ELGGXXR 
00090      05  WS-I-E-IND         PIC X.                                ELGGXXR 
00091      05  WS-PROCEDURE-CODE.                                       ELGGXXR 
00092          10 WS-PROC-CODE-6  PIC X(06).                            ELGGXXR 
00093          10 WS-PROC-CODE-1  PIC X(01).                            ELGGXXR 
00094                                                                   ELGGXXR 
00095      05  WS-TAB-SLOT-NO     PIC 9(7).                             ELGGXXR 
00096      05  WS-FIRST-TIME-IND-VAL   PIC X.                           ELGGXXR 
00097        88  WS-FIRST-TIME-THIS-IND    VALUE 'Y'.                   ELGGXXR 
00098      05  WS-DID-GETMAIN-SWITCH   PIC X.                           ELGGXXR 
00099        88  WS-DID-GETMAIN            VALUE 'Y'.                   ELGGXXR 
00100      05  WS-TAB-TYPE-DEF.                                         ELGGXXR 
00101        10  WS-GHOR          PIC X(6)   VALUE '#GHOR '.            ELGGXXR 
00102        10  WS-GMCR          PIC X(6)   VALUE '#GMCR '.            ELGGXXR 
00103        10  WS-GMOR          PIC X(6)   VALUE '#GMOR '.            ELGGXXR 
00104        10  WS-GMPR          PIC X(6)   VALUE '#GMPR '.            ELGGXXR 
00105        10  WS-GMSR          PIC X(6)   VALUE '#GMSR '.            ELGGXXR 
00106        10  WS-GPAR          PIC X(6)   VALUE '#GPAR '.            ELGGXXR 
00107      05  WS-TAB-ID-TABLE   REDEFINES   WS-TAB-TYPE-DEF.           ELGGXXR 
00108        10  WS-TAB-TYPE      PIC X(6) OCCURS  6  TIMES             ELGGXXR 
00109                             INDEXED BY  WS-IDX.                   ELGGXXR 
00110                                                                   ELGGXXR 
00111  01  WS-RELATED-PROCEDURES    PIC X(45) VALUE                     ELGGXXR 
00112      'PROGRAM HAS THE FOLLOWING RELATED PROCEDURES'.              ELGGXXR 
00113                                                                   ELGGXXR 
00114  01  WS-RELATED-PROCEDURESA   PIC X(35) VALUE                     ELGGXXR 
00115      ' (IN PLACE OF THE CORPORATE LIST): '.                       ELGGXXR 
00116  01  WS-END                 PIC X(24)  VALUE                      ELGGXXR 
00117      '*** ELGGXXR W/S ENDS ***'.                                  ELGGXXR 
00118 /             L I N K A G E   S E C T I O N                       ELGGXXR 
00119  LINKAGE SECTION.                                                 ELGGXXR 
00120  01  DFHCOMMAREA.                                                 ELGGXXR 
00121      COPY ELSCOMMC.                                               ELGGXXR 
00122 /    C O M M O N   I N T E R F A C E   A R E A                    ELGGXXR 
00123      COPY ELSCIA2C.                                               ELGGXXR 
00124 /    C O D E S   M A N U A L   D E S C R I P T I O N   L I N E S  ELGGXXR 
00125      COPY ELSCMDSC.                                               ELGGXXR 
00126 /    C O D E S   M A N U A L   C N T L .   B L O C K              ELGGXXR 
00127      COPY ELSCMIFC.                                               ELGGXXR 
00128 /    I / O   P A R A M E T E R   B L O C K                        ELGGXXR 
00129      COPY ELSIOPMC.                                               ELGGXXR 
00130 /    W O R K   A R E A   T O   B U I L D   K E Y S                ELGGXXR 
00131      COPY ELSKEYSC.                                               ELGGXXR 
00132 /    C N T L   B L O C K   -   O U T P U T   P A G E   B L D R    ELGGXXR 
00133      COPY ELSOUTPC.                                               ELGGXXR 
00134 /    T E X T   C O M P R E S S I O N   W O R K   A R E A          ELGGXXR 
00135      COPY ELSTCWAC.                                               ELGGXXR 
00136 /    S E L E C T O R   S T A T U S   C O N T R O L   B L O C K    ELGGXXR 
00137      COPY ELSSSCBC.                                               ELGGXXR 
00138 /    S U B R O U T I N E   P A R A M E T E R   L I S T            ELGGXXR 
00139      COPY ELSSRTPC.                                               ELGGXXR 
00140 /    G R O U P   S P E C I F I C   # G H O R   T A B U L A R      ELGGXXR 
00141  01  GSO-TABULAR-REC-AREA.                                        ELGGXXR 
00142      COPY GCTGHORC.                                               ELGGXXR 
00143 /    G R O U P   S P E C I F I C   # G M O R   T A B U L A R      ELGGXXR 
00144  01  GSN-TABULAR-REC-AREA.                                        ELGGXXR 
00145      COPY GCTGMORC.                                               ELGGXXR 
00146 /    P R O V I D E R   N O .   T O   N A M E   D B   P A R M S    ELGGXXR 
00147  01  PDB-IO-AREA.                                                 ELGGXXR 
00148      COPY DBPIOPMC.                                               ELGGXXR 
00149 /    P R O C E D U R E   M A S T E R   R E C O R D   R D W        ELGGXXR 
00150  01  PROCEDURE-MSTR-REC.                                          ELGGXXR 
00151      COPY PROCMSTR.                                               ELGGXXR 
00152      EJECT                                                        ELGGXXR 
00153  PROCEDURE DIVISION.                                              ELGGXXR 
00154 ************************************************************      ELGGXXR 
00155 *                                                          *      ELGGXXR 
00156 *                    PROCEDURE DIVISION                    *      ELGGXXR 
00157 *                                                          *      ELGGXXR 
00158 ************************************************************      ELGGXXR 
00159                                                                   ELGGXXR 
00160                                                                   ELGGXXR 
00161 ************************************************************      ELGGXXR 
00162 *                                                          *      ELGGXXR 
00163 *        GXXR TABULAR MAINLINE                             *      ELGGXXR 
00164 *                                                          *      ELGGXXR 
00165 ************************************************************      ELGGXXR 
00166  GXXR-TABULAR-MAINLINE.                                           ELGGXXR 
00167      PERFORM INITIALIZATION-ROUTINE.                              ELGGXXR 
00168      PERFORM MAIN-ROUTINE.                                        ELGGXXR 
00169      GOBACK.                                                      ELGGXXR 
00170      EJECT                                                        ELGGXXR 
00171                                                                   ELGGXXR 
00172                                                                   ELGGXXR 
00173 ************************************************************      ELGGXXR 
00174 *                                                          *      ELGGXXR 
00175 *        INITIALIZATION ROUTINE                            *      ELGGXXR 
00176 *                                                          *      ELGGXXR 
00177 ************************************************************      ELGGXXR 
00178  INITIALIZATION-ROUTINE.                                          ELGGXXR 
00179      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    ELGGXXR 
00180          PERFORM COMMAREA-LENGTH-ERROR.                           ELGGXXR 
00181      CALL 'ELUINISM' USING DFHCOMMAREA                            ELGGXXR 
00182          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELGGXXR 
00183      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGGXXR 
00184      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXR 
00185          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELGGXXR 
00186      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELGGXXR 
00187      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXR 
00188          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELGGXXR 
00189      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGGXXR 
00190      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXR 
00191          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELGGXXR 
00192      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGGXXR 
00193      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXR 
00194          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELGGXXR 
00195      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELGGXXR 
00196      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXR 
00197          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELGGXXR 
00198      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELGGXXR 
00199      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXR 
00200          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELGGXXR 
00201      INITIALIZE CMF-CODES-MANUAL-INTERFACE                        ELGGXXR 
00202                 TCAR-FROM-AREA.                                   ELGGXXR 
00203      SET CIA-DBPIOPM-DDN TO TRUE.                                 ELGGXXR 
00204      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXR 
00205          ADDRESS OF PDB-IO-AREA.                                  ELGGXXR 
00206      IF CIA-RC-PTR-NULL                                           ELGGXXR 
00207          PERFORM GETMAIN-IO-PARM-AREA.                            ELGGXXR 
00208      IF WS-DID-GETMAIN                                            ELGGXXR 
00209          PERFORM DO-ADDRESS-OF-PDB.                               ELGGXXR 
00210      MOVE 'Y'  TO  WS-FIRST-TIME-IND-VAL.                         ELGGXXR 
00211      EJECT                                                        ELGGXXR 
00212                                                                   ELGGXXR 
00213                                                                   ELGGXXR 
00214 ************************************************************      ELGGXXR 
00215 *                                                          *      ELGGXXR 
00216 *        DO ADDRESS OF PDB                                 *      ELGGXXR 
00217 *                                                          *      ELGGXXR 
00218 ************************************************************      ELGGXXR 
00219  DO-ADDRESS-OF-PDB.                                               ELGGXXR 
00220      SET CIA-DBPIOPM-DDN TO TRUE.                                 ELGGXXR 
00221      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXR 
00222          ADDRESS OF PDB-IO-AREA.                                  ELGGXXR 
00223      EJECT                                                        ELGGXXR 
00224                                                                   ELGGXXR 
00225                                                                   ELGGXXR 
00226 ************************************************************      ELGGXXR 
00227 *                                                          *      ELGGXXR 
00228 *        COMMAREA LENGTH ERROR                             *      ELGGXXR 
00229 *                                                          *      ELGGXXR 
00230 ************************************************************      ELGGXXR 
00231  COMMAREA-LENGTH-ERROR.                                           ELGGXXR 
00232      SET CIA-AB-DFHCOMMAREA  TO  TRUE.                            ELGGXXR 
00233      EXEC CICS  ABEND  ABCODE(CIA-ABCODE) END-EXEC.               ELGGXXR 
00234                                                                   ELGGXXR 
00235                                                                   ELGGXXR 
00236 ************************************************************      ELGGXXR 
00237 *                                                          *      ELGGXXR 
00238 *        MAIN ROUTINE                                      *      ELGGXXR 
00239 *                                                          *      ELGGXXR 
00240 ************************************************************      ELGGXXR 
00241  MAIN-ROUTINE.                                                    ELGGXXR 
00242      PERFORM SEARCH-COST-CONTAINMENT-TABLE.                       ELGGXXR 
00243      PERFORM READ-INTERNAL-TABULAR-RECORD.                        ELGGXXR 
00244      IF SRP-TABULAR-ID  =  WS-GHOR                                ELGGXXR 
00245          PERFORM PROCESS-GHOR-RECORD.                             ELGGXXR 
00246      IF SRP-TABULAR-ID  =  WS-GMOR OR  WS-GMPR OR                 ELGGXXR 
00247                                WS-GMCR OR WS-GMSR OR              ELGGXXR 
00248          WS-GPAR                                                  ELGGXXR 
00249          PERFORM PROCESS-OTHER-RECORDS.                           ELGGXXR 
00250      EJECT                                                        ELGGXXR 
00251                                                                   ELGGXXR 
00252                                                                   ELGGXXR 
00253 ************************************************************      ELGGXXR 
00254 *                                                          *      ELGGXXR 
00255 *        READ INTERNAL TABULAR RECORD                      *      ELGGXXR 
00256 *                                                          *      ELGGXXR 
00257 ************************************************************      ELGGXXR 
00258  READ-INTERNAL-TABULAR-RECORD.                                    ELGGXXR 
00259      SET CIA-GCTABULR-DDN  TO  TRUE.                              ELGGXXR 
00260      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXR 
00261          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELGGXXR 
00262      IF CIA-RC-PTR-NULL                                           ELGGXXR 
00263          PERFORM GETMAIN-IO-PARM-AREA.                            ELGGXXR 
00264      SET IOP-RD                                                   ELGGXXR 
00265          IOP-FCQ-NONE                                             ELGGXXR 
00266          IOP-KVQ-EQ                                               ELGGXXR 
00267          IOP-STG-MODE-MOVE  TO  TRUE.                             ELGGXXR 
00268      MOVE SRP-INTERNAL-TAB  TO  KWA-GCTABULR-KEY                  ELGGXXR 
00269                             IOP-FILE-KEY.                         ELGGXXR 
00270      MOVE SPACES  TO  IOP-AIX-DDNAME.                             ELGGXXR 
00271      SET IOP-REC-PTR  TO  NULL.                                   ELGGXXR 
00272      EXEC CICS  LINK  PROGRAM('ELUIOPGM')                         ELGGXXR 
00273                       COMMAREA(DFHCOMMAREA) END-EXEC.             ELGGXXR 
00274      IF NOT IOP-RC-OK                                             ELGGXXR 
00275          PERFORM TABULAR-NOT-FOUND.                               ELGGXXR 
00276      IF SRP-TABULAR-ID  =  WS-GHOR                                ELGGXXR 
00277          PERFORM ADDRESS-GHOR-TABULAR-AREA.                       ELGGXXR 
00278      IF SRP-TABULAR-ID  =  WS-GMOR OR  WS-GMPR OR                 ELGGXXR 
00279                            WS-GMCR OR WS-GMSR OR                  ELGGXXR 
00280          WS-GPAR                                                  ELGGXXR 
00281          PERFORM ADDRESS-OTHER-TABULAR-AREA.                      ELGGXXR 
00282      EJECT                                                        ELGGXXR 
00283                                                                   ELGGXXR 
00284                                                                   ELGGXXR 
00285 ************************************************************      ELGGXXR 
00286 *                                                          *      ELGGXXR 
00287 *        GETMAIN IO PARM AREA                              *      ELGGXXR 
00288 *                                                          *      ELGGXXR 
00289 ************************************************************      ELGGXXR 
00290  GETMAIN-IO-PARM-AREA.                                            ELGGXXR 
00291      SET CIA-STG-GETMAIN  TO  TRUE.                               ELGGXXR 
00292      EXEC CICS  LINK  PROGRAM('ELUSTGMG')                         ELGGXXR 
00293                       COMMAREA(DFHCOMMAREA)   END-EXEC.           ELGGXXR 
00294      SET WS-DID-GETMAIN TO TRUE.                                  ELGGXXR 
00295      EJECT                                                        ELGGXXR 
00296                                                                   ELGGXXR 
00297                                                                   ELGGXXR 
00298 ************************************************************      ELGGXXR 
00299 *                                                          *      ELGGXXR 
00300 *        ADDRESS GHOR TABULAR AREA                         *      ELGGXXR 
00301 *                                                          *      ELGGXXR 
00302 ************************************************************      ELGGXXR 
00303  ADDRESS-GHOR-TABULAR-AREA.                                       ELGGXXR 
00304      SET ADDRESS OF GSO-TABULAR-REC-AREA  TO                      ELGGXXR 
00305          IOP-REC-PTR.                                             ELGGXXR 
00306      EJECT                                                        ELGGXXR 
00307                                                                   ELGGXXR 
00308                                                                   ELGGXXR 
00309 ************************************************************      ELGGXXR 
00310 *                                                          *      ELGGXXR 
00311 *        ADDRESS OTHER TABULAR AREA                        *      ELGGXXR 
00312 *                                                          *      ELGGXXR 
00313 ************************************************************      ELGGXXR 
00314  ADDRESS-OTHER-TABULAR-AREA.                                      ELGGXXR 
00315      SET ADDRESS OF GSN-TABULAR-REC-AREA  TO  IOP-REC-PTR.        ELGGXXR 
00316      EJECT                                                        ELGGXXR 
00317                                                                   ELGGXXR 
00318                                                                   ELGGXXR 
00319 ************************************************************      ELGGXXR 
00320 *                                                          *      ELGGXXR 
00321 *        SEARCH COST CONTAINMENT TABLE                     *      ELGGXXR 
00322 *                                                          *      ELGGXXR 
00323 ************************************************************      ELGGXXR 
00324  SEARCH-COST-CONTAINMENT-TABLE.                                   ELGGXXR 
00325      SET WS-IDX  TO  1.                                           ELGGXXR 
00326      SEARCH WS-TAB-TYPE                                           ELGGXXR 
00327            VARYING WS-IDX                                         ELGGXXR 
00328            AT END                                                 ELGGXXR 
00329               SET CIA-AB-TAB-UNDEF  TO  TRUE                      ELGGXXR 
00330               EXEC CICS  ABEND  ABCODE(CIA-ABCODE)                ELGGXXR 
00331          END-EXEC                                                 ELGGXXR 
00332            WHEN WS-TAB-TYPE(WS-IDX)  =  SRP-TABULAR-ID            ELGGXXR 
00333                NEXT SENTENCE                                      ELGGXXR 
00334         END-SEARCH.                                               ELGGXXR 
00335      EJECT                                                        ELGGXXR 
00336                                                                   ELGGXXR 
00337                                                                   ELGGXXR 
00338 ************************************************************      ELGGXXR 
00339 *                                                          *      ELGGXXR 
00340 *        PROCESS GHOR RECORD                               *      ELGGXXR 
00341 *                                                          *      ELGGXXR 
00342 ************************************************************      ELGGXXR 
00343  PROCESS-GHOR-RECORD.                                             ELGGXXR 
00344      IF GSO-PROCEDURE-ARGUMENT(1)  NOT =  HIGH-VALUES             ELGGXXR 
00345          PERFORM SAVE-GHORS-FIRST-I-E-IND                         ELGGXXR 
00346      ELSE                                                         ELGGXXR 
00347          PERFORM EMPTY-COST-CONTAINMENT-TAB.                      ELGGXXR 
00348      PERFORM INSERT-BLANK-LINE.                                   ELGGXXR 
00349      PERFORM SEARCH-GHOR-TABLE-FOR-FIRST                          ELGGXXR 
00350          VARYING GSO-INDEX  FROM  1  BY  1                        ELGGXXR 
00351             UNTIL GSO-INDEX  =  GSO-ENTRY-COUNT OR                ELGGXXR 
00352             GSO-PROCEDURE-ARGUMENT(GSO-INDEX)  =                  ELGGXXR 
00353              HIGH-VALUES.                                         ELGGXXR 
00354      PERFORM INSERT-BLANK-LINE.                                   ELGGXXR 
00355      MOVE 'Y'  TO  WS-FIRST-TIME-IND-VAL.                         ELGGXXR 
00356      PERFORM SEARCH-GHOR-TABLE-FOR-OTHER-TH                       ELGGXXR 
00357          VARYING GSO-INDEX  FROM  1  BY  1                        ELGGXXR 
00358             UNTIL GSO-INDEX  =  GSO-ENTRY-COUNT OR                ELGGXXR 
00359             GSO-PROCEDURE-ARGUMENT(GSO-INDEX)  =                  ELGGXXR 
00360              HIGH-VALUES.                                         ELGGXXR 
00361      IF  NOT WS-FIRST-TIME-THIS-IND                               ELGGXXR 
00362          PERFORM INSERT-BLANK-LINE.                               ELGGXXR 
00363      EJECT                                                        ELGGXXR 
00364                                                                   ELGGXXR 
00365                                                                   ELGGXXR 
00366 ************************************************************      ELGGXXR 
00367 *                                                          *      ELGGXXR 
00368 *        SAVE GHORS FIRST I-E IND                          *      ELGGXXR 
00369 *                                                          *      ELGGXXR 
00370 ************************************************************      ELGGXXR 
00371  SAVE-GHORS-FIRST-I-E-IND.                                        ELGGXXR 
00372      MOVE GSO-INCLUDE-EXCLUDE-IND(1)  TO  WS-I-E-IND.             ELGGXXR 
00373      EJECT                                                        ELGGXXR 
00374                                                                   ELGGXXR 
00375                                                                   ELGGXXR 
00376 ************************************************************      ELGGXXR 
00377 *                                                          *      ELGGXXR 
00378 *        SEARCH GHOR TABLE FOR FIRST                       *      ELGGXXR 
00379 *                                                          *      ELGGXXR 
00380 ************************************************************      ELGGXXR 
00381  SEARCH-GHOR-TABLE-FOR-FIRST.                                     ELGGXXR 
00382      IF GSO-INCLUDE-EXCLUDE-IND(GSO-INDEX)  =                     ELGGXXR 
00383          WS-I-E-IND                                               ELGGXXR 
00384          PERFORM LIST-GHOR-SERVICE-RELATED-PROC.                  ELGGXXR 
00385      EJECT                                                        ELGGXXR 
00386                                                                   ELGGXXR 
00387                                                                   ELGGXXR 
00388 ************************************************************      ELGGXXR 
00389 *                                                          *      ELGGXXR 
00390 *        SEARCH GHOR TABLE FOR OTHER THAN FIRST            *      ELGGXXR 
00391 *                                                          *      ELGGXXR 
00392 ************************************************************      ELGGXXR 
00393  SEARCH-GHOR-TABLE-FOR-OTHER-TH.                                  ELGGXXR 
00394      IF GSO-INCLUDE-EXCLUDE-IND(GSO-INDEX)  NOT =                 ELGGXXR 
00395          WS-I-E-IND                                               ELGGXXR 
00396          PERFORM LIST-GHOR-SERVICE-RELATED-PROC.                  ELGGXXR 
00397      EJECT                                                        ELGGXXR 
00398                                                                   ELGGXXR 
00399                                                                   ELGGXXR 
00400 ************************************************************      ELGGXXR 
00401 *                                                          *      ELGGXXR 
00402 *        TRANSLATE GHOR INCLUDE-EXCLUDE IND                *      ELGGXXR 
00403 *                                                          *      ELGGXXR 
00404 ************************************************************      ELGGXXR 
00405  TRANSLATE-GHOR-INCLUDE-EXCLUDE.                                  ELGGXXR 
00406      MOVE 'N'  TO  WS-FIRST-TIME-IND-VAL.                         ELGGXXR 
00407      INITIALIZE TCAR-FROM-AREA.                                   ELGGXXR 
00408      MOVE SRP-TABULAR-ID  TO  CMF-RECORD-PREFIX.                  ELGGXXR 
00409      MOVE 'INCLUDE-EXCLUDE-IND'  TO                               ELGGXXR 
00410          CMF-ELEMENT-SYSTEM-NAME.                                 ELGGXXR 
00411      MOVE GSO-INCLUDE-EXCLUDE-IND(GSO-INDEX)  TO  CMF-CODE-VALUE. ELGGXXR 
00412      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELGGXXR 
00413                       COMMAREA(DFHCOMMAREA)  END-EXEC.            ELGGXXR 
00414      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGGXXR 
00415      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXR 
00416          ADDRESS OF CMF-DESCR.                                    ELGGXXR 
00417      STRING 'THIS ',  SRP-CCP-NAME DELIMITED BY '  '              ELGGXXR 
00418             ' PROGRAM  '  DELIMITED BY SIZE                       ELGGXXR 
00419             CMF-DESCR-LINE(1)  DELIMITED BY '  '                  ELGGXXR 
00420             ' THE FOLLOWING PROCEDURE CODES:'  DELIMITED BY       ELGGXXR 
00421          SIZE                                                     ELGGXXR 
00422             INTO TCAR-FROM-AREA.                                  ELGGXXR 
00423      PERFORM DO-TEXT-COMPRESSION.                                 ELGGXXR 
00424      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELGGXXR 
00425      MOVE +04  TO  TCAR-OUTPUT-FIELD-COUNT.                       ELGGXXR 
00426      MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                        ELGGXXR 
00427                    TCAR-OUTPUT-FIELD-2-LEN                        ELGGXXR 
00428                    TCAR-OUTPUT-FIELD-3-LEN                        ELGGXXR 
00429                    TCAR-OUTPUT-FIELD-4-LEN.                       ELGGXXR 
00430      PERFORM DO-TEXT-UNSTRING.                                    ELGGXXR 
00431      PERFORM MOVE-COMPRESSED-PHRASE                               ELGGXXR 
00432          VARYING WS-SUB  FROM  1  BY  1                           ELGGXXR 
00433          UNTIL WS-SUB  GREATER THAN                               ELGGXXR 
00434              TCAR-OUTPUT-FIELDS-USED.                             ELGGXXR 
00435      PERFORM CALL-OUTPUT.                                         ELGGXXR 
00436      EJECT                                                        ELGGXXR 
00437                                                                   ELGGXXR 
00438                                                                   ELGGXXR 
00439 ************************************************************      ELGGXXR 
00440 *                                                          *      ELGGXXR 
00441 *        LIST GHOR SERVICE RELATED PROCEDURE               *      ELGGXXR 
00442 *                                                          *      ELGGXXR 
00443 ************************************************************      ELGGXXR 
00444  LIST-GHOR-SERVICE-RELATED-PROC.                                  ELGGXXR 
00445      IF WS-FIRST-TIME-THIS-IND                                    ELGGXXR 
00446          PERFORM TRANSLATE-GHOR-INCLUDE-EXCLUDE.                  ELGGXXR 
00447      MOVE ZERO  TO  WS-BLANK-CNT.                                 ELGGXXR 
00448      MOVE GSO-PROCEDURE-ARGUMENT(GSO-INDEX)  TO                   ELGGXXR 
00449          WS-PROCEDURE-CODE.                                       ELGGXXR 
00450      INSPECT WS-PROC-CODE-6 TALLYING                              ELGGXXR 
00451 *    INSPECT GSO-PROCEDURE-ARGUMENT(GSO-INDEX) TALLYING           ELGGXXR 
00452          WS-BLANK-CNT                                             ELGGXXR 
00453              FOR ALL SPACES.                                      ELGGXXR 
00454      IF WS-BLANK-CNT  =  1                                        ELGGXXR 
00455          PERFORM MOVE-C-TO-SYSTEM-ID                              ELGGXXR 
00456      ELSE                                                         ELGGXXR 
00457          PERFORM MOVE-H-TO-SYSTEM-ID.                             ELGGXXR 
00458      MOVE SPACES TO WS-PROCEDURE-CODE.                            ELGGXXR 
00459      MOVE GSO-PROCEDURE-ARGUMENT(GSO-INDEX)  TO                   ELGGXXR 
00460          WS-PROCEDURE-CODE.                                       ELGGXXR 
00461      MOVE WS-PROC-CODE-6 TO PDB-I-SERVICE-CODE.                   ELGGXXR 
00462      MOVE 'PRCDR03 '  TO  PDB-I-REQUEST-TYPE.                     ELGGXXR 
00463      EXEC CICS  LINK  PROGRAM('DBPIOC') COMMAREA(PDB-IO-AREA)     ELGGXXR 
00464          END-EXEC.                                                ELGGXXR 
00465      IF PDB-O-RC-SUCCESSFUL                                       ELGGXXR 
00466          PERFORM STRING-GHOR-PROCEDURE-AND-DESC                   ELGGXXR 
00467      ELSE                                                         ELGGXXR 
00468          PERFORM PROCEDURE-FILE-PROBLEM.                          ELGGXXR 
00469      PERFORM CALL-OUTPUT.                                         ELGGXXR 
00470      EJECT                                                        ELGGXXR 
00471                                                                   ELGGXXR 
00472                                                                   ELGGXXR 
00473 ************************************************************      ELGGXXR 
00474 *                                                          *      ELGGXXR 
00475 *        MOVE C TO SYSTEM ID                               *      ELGGXXR 
00476 *                                                          *      ELGGXXR 
00477 ************************************************************      ELGGXXR 
00478  MOVE-C-TO-SYSTEM-ID.                                             ELGGXXR 
00479      MOVE 'C'  TO  PDB-I-SERVICE-CODE-SYSTEM-ID.                  ELGGXXR 
00480      EJECT                                                        ELGGXXR 
00481                                                                   ELGGXXR 
00482                                                                   ELGGXXR 
00483 ************************************************************      ELGGXXR 
00484 *                                                          *      ELGGXXR 
00485 *        MOVE H TO SYSTEM ID                               *      ELGGXXR 
00486 *                                                          *      ELGGXXR 
00487 ************************************************************      ELGGXXR 
00488  MOVE-H-TO-SYSTEM-ID.                                             ELGGXXR 
00489      MOVE 'H'  TO  PDB-I-SERVICE-CODE-SYSTEM-ID.                  ELGGXXR 
00490      EJECT                                                        ELGGXXR 
00491                                                                   ELGGXXR 
00492                                                                   ELGGXXR 
00493 ************************************************************      ELGGXXR 
00494 *                                                          *      ELGGXXR 
00495 *        STRING GHOR PROCEDURE AND DESCRIPTIONS            *      ELGGXXR 
00496 *                                                          *      ELGGXXR 
00497 ************************************************************      ELGGXXR 
00498  STRING-GHOR-PROCEDURE-AND-DESC.                                  ELGGXXR 
00499      SET ADDRESS OF PROCEDURE-MSTR-REC  TO                        ELGGXXR 
00500                                      ADDRESS OF                   ELGGXXR 
00501          PDB-O-RECORD-AREA.                                       ELGGXXR 
00502      STRING GSO-PROCEDURE-ARGUMENT(GSO-INDEX),                    ELGGXXR 
00503             ' ',  DELIMITED BY SIZE,                              ELGGXXR 
00504             PM-DESCRIPTION1,  DELIMITED BY '  ',                  ELGGXXR 
00505             ' ',  DELIMITED BY SIZE,                              ELGGXXR 
00506             PM-DESCRIPTION2,  DELIMITED BY '  ',                  ELGGXXR 
00507             INTO  TCAR-FROM-AREA.                                 ELGGXXR 
00508      PERFORM DO-TEXT-COMPRESSION.                                 ELGGXXR 
00509      MOVE +74  TO  TCAR-OUTPUT-FIELD-1-LEN                        ELGGXXR 
00510                    TCAR-OUTPUT-FIELD-2-LEN.                       ELGGXXR 
00511      MOVE +2  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELGGXXR 
00512      PERFORM DO-TEXT-UNSTRING.                                    ELGGXXR 
00513      PERFORM MOVE-COMPRESSED-PHRASE                               ELGGXXR 
00514          VARYING WS-SUB  FROM  1  BY  1                           ELGGXXR 
00515          UNTIL WS-SUB  GREATER THAN                               ELGGXXR 
00516              TCAR-OUTPUT-FIELDS-USED.                             ELGGXXR 
00517      EJECT                                                        ELGGXXR 
00518                                                                   ELGGXXR 
00519                                                                   ELGGXXR 
00520 ************************************************************      ELGGXXR 
00521 *                                                          *      ELGGXXR 
00522 *        PROCESS OTHER RECORDS                             *      ELGGXXR 
00523 *                                                          *      ELGGXXR 
00524 ************************************************************      ELGGXXR 
00525  PROCESS-OTHER-RECORDS.                                           ELGGXXR 
00526      IF GSN-PROCEDURE-ARGUMENT(1)  NOT =  HIGH-VALUES             ELGGXXR 
00527          PERFORM SAVE-OTHER-FIRST-I-E-IND                         ELGGXXR 
00528      ELSE                                                         ELGGXXR 
00529          PERFORM EMPTY-COST-CONTAINMENT-TAB.                      ELGGXXR 
00530      PERFORM INSERT-BLANK-LINE.                                   ELGGXXR 
00531      PERFORM SEARCH-OTHER-TABLE-FOR-FIRST                         ELGGXXR 
00532          VARYING GSN-INDEX  FROM  1  BY  1                        ELGGXXR 
00533            UNTIL GSN-INDEX  =  GSN-ENTRY-COUNT OR                 ELGGXXR 
00534            GSN-PROCEDURE-ARGUMENT(GSN-INDEX)  =                   ELGGXXR 
00535              HIGH-VALUES.                                         ELGGXXR 
00536      PERFORM INSERT-BLANK-LINE.                                   ELGGXXR 
00537      MOVE 'Y'  TO  WS-FIRST-TIME-IND-VAL.                         ELGGXXR 
00538      PERFORM SEARCH-OTHER-TABLE-FOR-OTHER-T                       ELGGXXR 
00539          VARYING GSN-INDEX  FROM  1  BY  1                        ELGGXXR 
00540            UNTIL GSN-INDEX  =  GSN-ENTRY-COUNT OR                 ELGGXXR 
00541            GSN-PROCEDURE-ARGUMENT(GSN-INDEX)  =                   ELGGXXR 
00542              HIGH-VALUES.                                         ELGGXXR 
00543      IF  NOT WS-FIRST-TIME-THIS-IND                               ELGGXXR 
00544          PERFORM INSERT-BLANK-LINE.                               ELGGXXR 
00545      EJECT                                                        ELGGXXR 
00546                                                                   ELGGXXR 
00547                                                                   ELGGXXR 
00548 ************************************************************      ELGGXXR 
00549 *                                                          *      ELGGXXR 
00550 *        SAVE OTHER FIRST I-E IND                          *      ELGGXXR 
00551 *                                                          *      ELGGXXR 
00552 ************************************************************      ELGGXXR 
00553  SAVE-OTHER-FIRST-I-E-IND.                                        ELGGXXR 
00554      MOVE GSN-INCLUDE-EXCLUDE-IND(1)  TO  WS-I-E-IND.             ELGGXXR 
00555      EJECT                                                        ELGGXXR 
00556                                                                   ELGGXXR 
00557                                                                   ELGGXXR 
00558 ************************************************************      ELGGXXR 
00559 *                                                          *      ELGGXXR 
00560 *        SEARCH OTHER TABLE FOR FIRST                      *      ELGGXXR 
00561 *                                                          *      ELGGXXR 
00562 ************************************************************      ELGGXXR 
00563  SEARCH-OTHER-TABLE-FOR-FIRST.                                    ELGGXXR 
00564      IF GSN-INCLUDE-EXCLUDE-IND(GSN-INDEX)  =                     ELGGXXR 
00565          WS-I-E-IND                                               ELGGXXR 
00566          PERFORM LIST-OTHER-SERVICE-RELATED-PRO.                  ELGGXXR 
00567      EJECT                                                        ELGGXXR 
00568                                                                   ELGGXXR 
00569                                                                   ELGGXXR 
00570 ************************************************************      ELGGXXR 
00571 *                                                          *      ELGGXXR 
00572 *        SEARCH OTHER TABLE FOR OTHER THAN FIRST           *      ELGGXXR 
00573 *                                                          *      ELGGXXR 
00574 ************************************************************      ELGGXXR 
00575  SEARCH-OTHER-TABLE-FOR-OTHER-T.                                  ELGGXXR 
00576      IF GSN-INCLUDE-EXCLUDE-IND(GSN-INDEX)  NOT =                 ELGGXXR 
00577          WS-I-E-IND                                               ELGGXXR 
00578          PERFORM LIST-OTHER-SERVICE-RELATED-PRO.                  ELGGXXR 
00579      EJECT                                                        ELGGXXR 
00580                                                                   ELGGXXR 
00581                                                                   ELGGXXR 
00582 ************************************************************      ELGGXXR 
00583 *                                                          *      ELGGXXR 
00584 *        TRANSLATE OTHER INCLUDE-EXCLUDE IND               *      ELGGXXR 
00585 *                                                          *      ELGGXXR 
00586 ************************************************************      ELGGXXR 
00587  TRANSLATE-OTHER-INCLUDE-EXCLUD.                                  ELGGXXR 
00588      MOVE 'N'  TO  WS-FIRST-TIME-IND-VAL.                         ELGGXXR 
00589      INITIALIZE TCAR-FROM-AREA.                                   ELGGXXR 
00590      MOVE SRP-TABULAR-ID  TO  CMF-RECORD-PREFIX.                  ELGGXXR 
00591      MOVE 'INCLUDE-EXCLUDE-IND'  TO                               ELGGXXR 
00592          CMF-ELEMENT-SYSTEM-NAME.                                 ELGGXXR 
00593      MOVE GSN-INCLUDE-EXCLUDE-IND(GSN-INDEX)  TO  CMF-CODE-VALUE. ELGGXXR 
00594      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELGGXXR 
00595                       COMMAREA(DFHCOMMAREA)  END-EXEC.            ELGGXXR 
00596      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGGXXR 
00597      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXR 
00598          ADDRESS OF CMF-DESCR.                                    ELGGXXR 
00599      EJECT                                                        ELGGXXR 
00600                                                                   ELGGXXR 
00601                                                                   ELGGXXR 
00602 ************************************************************      ELGGXXR 
00603 *                                                          *      ELGGXXR 
00604 *        LIST OTHER SERVICE RELATED PROCEDURE              *      ELGGXXR 
00605 *                                                          *      ELGGXXR 
00606 ************************************************************      ELGGXXR 
00607  LIST-OTHER-SERVICE-RELATED-PRO.                                  ELGGXXR 
00608      IF WS-FIRST-TIME-THIS-IND                                    ELGGXXR 
00609          PERFORM DETERMINE-IF-CONTRACT-OVERRIDE.                  ELGGXXR 
00610      MOVE ZERO  TO  WS-BLANK-CNT.                                 ELGGXXR 
00611      MOVE GSN-PROCEDURE-ARGUMENT(GSN-INDEX)  TO                   ELGGXXR 
00612          WS-PROCEDURE-CODE.                                       ELGGXXR 
00613      INSPECT WS-PROC-CODE-6 TALLYING                              ELGGXXR 
00614          WS-BLANK-CNT                                             ELGGXXR 
00615              FOR ALL SPACES.                                      ELGGXXR 
00616      IF WS-BLANK-CNT  =  1                                        ELGGXXR 
00617          PERFORM MOVE-C-TO-SYSTEM-ID                              ELGGXXR 
00618      ELSE                                                         ELGGXXR 
00619          PERFORM MOVE-H-TO-SYSTEM-ID.                             ELGGXXR 
00620      MOVE SPACES TO WS-PROCEDURE-CODE.                            ELGGXXR 
00621      MOVE GSN-PROCEDURE-ARGUMENT(GSN-INDEX)  TO                   ELGGXXR 
00622          WS-PROCEDURE-CODE.                                       ELGGXXR 
00623      MOVE WS-PROC-CODE-6 TO PDB-I-SERVICE-CODE.                   ELGGXXR 
00624      MOVE 'PRCDR03 '  TO  PDB-I-REQUEST-TYPE.                     ELGGXXR 
00625      EXEC CICS  LINK  PROGRAM('DBPIOC') COMMAREA(PDB-IO-AREA)     ELGGXXR 
00626          END-EXEC.                                                ELGGXXR 
00627      IF PDB-O-RC-SUCCESSFUL                                       ELGGXXR 
00628          PERFORM STRING-OTHER-PROCEDURE-AND-DES                   ELGGXXR 
00629      ELSE                                                         ELGGXXR 
00630          PERFORM PROCEDURE-FILE-PROBLEM.                          ELGGXXR 
00631      PERFORM CALL-OUTPUT.                                         ELGGXXR 
00632      EJECT                                                        ELGGXXR 
00633                                                                   ELGGXXR 
00634                                                                   ELGGXXR 
00635 ************************************************************      ELGGXXR 
00636 *                                                          *      ELGGXXR 
00637 *        DETERMINE IF CONTRACT OVERRIDES CORPORATE LIST    *      ELGGXXR 
00638 *                                                          *      ELGGXXR 
00639 ************************************************************      ELGGXXR 
00640  DETERMINE-IF-CONTRACT-OVERRIDE.                                  ELGGXXR 
00641      IF GSN-CORP-LIST-OVERIDE-IND = 'N'                           ELGGXXR 
00642          PERFORM DISPLAY-CONTRACT-USES-CORPORAT                   ELGGXXR 
00643      ELSE                                                         ELGGXXR 
00644          PERFORM DISPLAY-EXCEPTION-FROM-CORPORA.                  ELGGXXR 
00645      EJECT                                                        ELGGXXR 
00646                                                                   ELGGXXR 
00647                                                                   ELGGXXR 
00648 ************************************************************      ELGGXXR 
00649 *                                                          *      ELGGXXR 
00650 *        DISPLAY CONTRACT USES CORPORATE LIST              *      ELGGXXR 
00651 *                                                          *      ELGGXXR 
00652 ************************************************************      ELGGXXR 
00653  DISPLAY-CONTRACT-USES-CORPORAT.                                  ELGGXXR 
00654      PERFORM DO-INITIALIZE-TEXT-COMPRESSION.                      ELGGXXR 
00655      IF WS-FIRST-TIME-THIS-IND                                    ELGGXXR 
00656          PERFORM TRANSLATE-OTHER-INCLUDE-EXCLUD.                  ELGGXXR 
00657      STRING  'THIS ', SRP-CCP-NAME DELIMITED BY SIZE              ELGGXXR 
00658          ' PROGRAM USES THE CORPORATE ' DELIMITED BY              ELGGXXR 
00659          SIZE                                                     ELGGXXR 
00660          SRP-CCP-NAME DELIMITED BY '  '                           ELGGXXR 
00661          ' LIST AND IN ADDITION ' DELIMITED BY SIZE               ELGGXXR 
00662          CMF-DESCR-LINE(1) DELIMITED BY '  '                      ELGGXXR 
00663          ' THE FOLLOWING PROCEDURES: ' DELIMITED BY               ELGGXXR 
00664          SIZE                                                     ELGGXXR 
00665          INTO TCAR-FROM-AREA.                                     ELGGXXR 
00666      PERFORM DO-TEXT-COMPRESSION.                                 ELGGXXR 
00667      MOVE +05 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGGXXR 
00668      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                          ELGGXXR 
00669                  TCAR-OUTPUT-FIELD-2-LEN                          ELGGXXR 
00670                  TCAR-OUTPUT-FIELD-3-LEN                          ELGGXXR 
00671                  TCAR-OUTPUT-FIELD-4-LEN                          ELGGXXR 
00672                  TCAR-OUTPUT-FIELD-5-LEN.                         ELGGXXR 
00673      PERFORM DO-TEXT-UNSTRING.                                    ELGGXXR 
00674      PERFORM MOVE-COMPRESSED-PHRASE                               ELGGXXR 
00675          VARYING WS-SUB FROM 1 BY 1 UNTIL                         ELGGXXR 
00676                     WS-SUB GREATER THAN                           ELGGXXR 
00677              TCAR-OUTPUT-FIELDS-USED.                             ELGGXXR 
00678      PERFORM CALL-OUTPUT.                                         ELGGXXR 
00679      EJECT                                                        ELGGXXR 
00680                                                                   ELGGXXR 
00681                                                                   ELGGXXR 
00682 ************************************************************      ELGGXXR 
00683 *                                                          *      ELGGXXR 
00684 *        DISPLAY EXCEPTION FROM CORPORATE LIST             *      ELGGXXR 
00685 *                                                          *      ELGGXXR 
00686 ************************************************************      ELGGXXR 
00687  DISPLAY-EXCEPTION-FROM-CORPORA.                                  ELGGXXR 
00688      MOVE 'N'  TO  WS-FIRST-TIME-IND-VAL.                         ELGGXXR 
00689      PERFORM DO-INITIALIZE-TEXT-COMPRESSION.                      ELGGXXR 
00690      STRING 'THIS ', SRP-CCP-NAME DELIMITED BY SIZE               ELGGXXR 
00691         WS-RELATED-PROCEDURES  DELIMITED BY SIZE                  ELGGXXR 
00692         WS-RELATED-PROCEDURESA DELIMITED BY SIZE                  ELGGXXR 
00693          INTO TCAR-FROM-AREA.                                     ELGGXXR 
00694      PERFORM DO-TEXT-COMPRESSION.                                 ELGGXXR 
00695      MOVE +05 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGGXXR 
00696      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                          ELGGXXR 
00697                  TCAR-OUTPUT-FIELD-2-LEN                          ELGGXXR 
00698                  TCAR-OUTPUT-FIELD-3-LEN                          ELGGXXR 
00699                  TCAR-OUTPUT-FIELD-4-LEN                          ELGGXXR 
00700                  TCAR-OUTPUT-FIELD-5-LEN.                         ELGGXXR 
00701      PERFORM DO-TEXT-UNSTRING.                                    ELGGXXR 
00702      PERFORM MOVE-COMPRESSED-PHRASE                               ELGGXXR 
00703          VARYING WS-SUB FROM 1 BY 1 UNTIL                         ELGGXXR 
00704                     WS-SUB GREATER THAN                           ELGGXXR 
00705              TCAR-OUTPUT-FIELDS-USED.                             ELGGXXR 
00706      PERFORM CALL-OUTPUT.                                         ELGGXXR 
00707      EJECT                                                        ELGGXXR 
00708                                                                   ELGGXXR 
00709                                                                   ELGGXXR 
00710 ************************************************************      ELGGXXR 
00711 *                                                          *      ELGGXXR 
00712 *        STRING OTHER PROCEDURE AND DESCRIPTIONS           *      ELGGXXR 
00713 *                                                          *      ELGGXXR 
00714 ************************************************************      ELGGXXR 
00715  STRING-OTHER-PROCEDURE-AND-DES.                                  ELGGXXR 
00716      SET ADDRESS OF PROCEDURE-MSTR-REC  TO                        ELGGXXR 
00717                                      ADDRESS OF                   ELGGXXR 
00718          PDB-O-RECORD-AREA.                                       ELGGXXR 
00719      STRING GSN-PROCEDURE-ARGUMENT(GSN-INDEX),                    ELGGXXR 
00720             ' ',  DELIMITED BY SIZE,                              ELGGXXR 
00721             PM-DESCRIPTION1,  DELIMITED BY '  ',                  ELGGXXR 
00722             ' ',  DELIMITED BY SIZE,                              ELGGXXR 
00723             PM-DESCRIPTION2,  DELIMITED BY '  ',                  ELGGXXR 
00724             INTO  TCAR-FROM-AREA.                                 ELGGXXR 
00725      PERFORM DO-TEXT-COMPRESSION.                                 ELGGXXR 
00726      MOVE +74  TO  TCAR-OUTPUT-FIELD-1-LEN                        ELGGXXR 
00727                    TCAR-OUTPUT-FIELD-2-LEN.                       ELGGXXR 
00728      MOVE +2  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELGGXXR 
00729      PERFORM DO-TEXT-UNSTRING.                                    ELGGXXR 
00730      PERFORM MOVE-COMPRESSED-PHRASE                               ELGGXXR 
00731          VARYING WS-SUB  FROM  1  BY  1                           ELGGXXR 
00732          UNTIL WS-SUB  GREATER THAN                               ELGGXXR 
00733              TCAR-OUTPUT-FIELDS-USED.                             ELGGXXR 
00734      EJECT                                                        ELGGXXR 
00735                                                                   ELGGXXR 
00736                                                                   ELGGXXR 
00737 ************************************************************      ELGGXXR 
00738 *                                                          *      ELGGXXR 
00739 *        INSERT BLANK LINE                                 *      ELGGXXR 
00740 *                                                          *      ELGGXXR 
00741 ************************************************************      ELGGXXR 
00742  INSERT-BLANK-LINE.                                               ELGGXXR 
00743      ADD  1  TO  COF-NBR-DTL-LINES.                               ELGGXXR 
00744      MOVE SPACES  TO  COF-DTL-LINE(COF-NBR-DTL-LINES).            ELGGXXR 
00745      PERFORM CALL-OUTPUT.                                         ELGGXXR 
00746      EJECT                                                        ELGGXXR 
00747                                                                   ELGGXXR 
00748                                                                   ELGGXXR 
00749 ************************************************************      ELGGXXR 
00750 *                                                          *      ELGGXXR 
00751 *        MOVE COMPRESSED PHRASE                            *      ELGGXXR 
00752 *                                                          *      ELGGXXR 
00753 ************************************************************      ELGGXXR 
00754  MOVE-COMPRESSED-PHRASE.                                          ELGGXXR 
00755      ADD 1  TO  COF-NBR-DTL-LINES.                                ELGGXXR 
00756      MOVE TCAR-OPF-DATA(WS-SUB)  TO                               ELGGXXR 
00757          COF-DTL-LINE(COF-NBR-DTL-LINES).                         ELGGXXR 
00758                                                                   ELGGXXR 
00759                                                                   ELGGXXR 
00760 ************************************************************      ELGGXXR 
00761 *                                                          *      ELGGXXR 
00762 *        CALL OUTPUT                                       *      ELGGXXR 
00763 *                                                          *      ELGGXXR 
00764 ************************************************************      ELGGXXR 
00765  CALL-OUTPUT.                                                     ELGGXXR 
00766      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELGGXXR 
00767                       COMMAREA(DFHCOMMAREA)  END-EXEC.            ELGGXXR 
00768      INITIALIZE TCAR-FROM-AREA.                                   ELGGXXR 
00769      MOVE +0 TO COF-NBR-DTL-LINES.                                ELGGXXR 
00770      EJECT                                                        ELGGXXR 
00771                                                                   ELGGXXR 
00772                                                                   ELGGXXR 
00773 ************************************************************      ELGGXXR 
00774 *                                                          *      ELGGXXR 
00775 *        EMPTY COST CONTAINMENT TAB                        *      ELGGXXR 
00776 *                                                          *      ELGGXXR 
00777 ************************************************************      ELGGXXR 
00778  EMPTY-COST-CONTAINMENT-TAB.                                      ELGGXXR 
00779      ADD 1  TO  COF-NBR-DTL-LINES.                                ELGGXXR 
00780      MOVE SPACES  TO  COF-DTL-LINE(COF-NBR-DTL-LINES).            ELGGXXR 
00781      MOVE SRP-TABULAR-SLOT-NO  TO  WS-TAB-SLOT-NO.                ELGGXXR 
00782      ADD 1  TO  COF-NBR-DTL-LINES.                                ELGGXXR 
00783      STRING 'THIS ',  SRP-CCP-NAME DELIMITED BY '  '              ELGGXXR 
00784             ' WITH TABULAR SLOT NO. ',  WS-TAB-SLOT-NO,           ELGGXXR 
00785             ' HAS AN EMPTY TABULAR RECORD'  DELIMITED BY          ELGGXXR 
00786          SIZE                                                     ELGGXXR 
00787             INTO   COF-DTL-LINE(COF-NBR-DTL-LINES).               ELGGXXR 
00788      PERFORM CALL-OUTPUT.                                         ELGGXXR 
00789      GOBACK.                                                      ELGGXXR 
00790      EJECT                                                        ELGGXXR 
00791                                                                   ELGGXXR 
00792                                                                   ELGGXXR 
00793 ************************************************************      ELGGXXR 
00794 *                                                          *      ELGGXXR 
00795 *        PROCEDURE FILE PROBLEM                            *      ELGGXXR 
00796 *                                                          *      ELGGXXR 
00797 ************************************************************      ELGGXXR 
00798  PROCEDURE-FILE-PROBLEM.                                          ELGGXXR 
00799      ADD  1  TO  COF-NBR-DTL-LINES.                               ELGGXXR 
00800      STRING 'PROCEDURE CODE ''',   PDB-I-SERVICE-CODE,            ELGGXXR 
00801             '''  IS NOT ON FILE ',    DELIMITED BY SIZE           ELGGXXR 
00802                           INTO                                    ELGGXXR 
00803          COF-DTL-LINE(COF-NBR-DTL-LINES).                         ELGGXXR 
00804      EJECT                                                        ELGGXXR 
00805                                                                   ELGGXXR 
00806                                                                   ELGGXXR 
00807 ************************************************************      ELGGXXR 
00808 *                                                          *      ELGGXXR 
00809 *        TABULAR NOT FOUND                                 *      ELGGXXR 
00810 *                                                          *      ELGGXXR 
00811 ************************************************************      ELGGXXR 
00812  TABULAR-NOT-FOUND.                                               ELGGXXR 
00813      SET CIA-AB-NOTFND-GCTABULR  TO  TRUE.                        ELGGXXR 
00814      EXEC CICS  ABEND  ABCODE(CIA-ABCODE)  END-EXEC.              ELGGXXR 
00815      EJECT                                                        ELGGXXR 
00816                                                                   ELGGXXR 
00817                                                                   ELGGXXR 
00818 ************************************************************      ELGGXXR 
00819 *                                                          *      ELGGXXR 
00820 *        DO INITIALIZE TEXT COMPRESSION                    *      ELGGXXR 
00821 *                                                          *      ELGGXXR 
00822 ************************************************************      ELGGXXR 
00823  DO-INITIALIZE-TEXT-COMPRESSION.                                  ELGGXXR 
00824      INITIALIZE TCAR-FROM-LENGTH                                  ELGGXXR 
00825                 TCAR-AREA-LENGTH                                  ELGGXXR 
00826                 TCAR-FROM-AREA                                    ELGGXXR 
00827                 TCAR-FROM-SUB.                                    ELGGXXR 
00828      EJECT                                                        ELGGXXR 
00829                                                                   ELGGXXR 
00830                                                                   ELGGXXR 
00831 ************************************************************      ELGGXXR 
00832 *                                                          *      ELGGXXR 
00833 *        DO TEXT COMPRESSION                               *      ELGGXXR 
00834 *                                                          *      ELGGXXR 
00835 ************************************************************      ELGGXXR 
00836  DO-TEXT-COMPRESSION.                                             ELGGXXR 
00837      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGGXXR 
00838      EJECT                                                        ELGGXXR 
00839                                                                   ELGGXXR 
00840                                                                   ELGGXXR 
00841 ************************************************************      ELGGXXR 
00842 *                                                          *      ELGGXXR 
00843 *        DO TEXT UNSTRING                                  *      ELGGXXR 
00844 *                                                          *      ELGGXXR 
00845 ************************************************************      ELGGXXR 
00846  DO-TEXT-UNSTRING.                                                ELGGXXR 
00847      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGGXXR 
