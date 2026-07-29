00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELTGVLX 
00003  PROGRAM-ID.         ELTGVLX.                                        LV001
00004                                                                   ELTGVLX 
00005  AUTHOR.             GEORGE E MOORE.                              ELTGVLX 
00006                                                                   ELTGVLX 
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTGVLX 
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELTGVLX 
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTGVLX 
00010                      233 N. MICHIGAN AVE                          ELTGVLX 
00011                      CHICAGO, ILLINOIS 60601.                     ELTGVLX 
00012                                                                   ELTGVLX 
00013  DATE-WRITTEN.       26-FEB-1990.                                 ELTGVLX 
00014                                                                   ELTGVLX 
00015  DATE-COMPILED.                                                   ELTGVLX 
00016                                                                   ELTGVLX 
00017  SECURITY.           COPYRIGHT 1990,                              ELTGVLX 
00018                      HEALTH CARE SERVICE CORPORATION.             ELTGVLX 
00019      SKIP3                                                        ELTGVLX 
00020  ENVIRONMENT DIVISION.                                            ELTGVLX 
00021                                                                   ELTGVLX 
00022  CONFIGURATION SECTION.                                           ELTGVLX 
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELTGVLX 
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELTGVLX 
00025      EJECT                                                        ELTGVLX 
00026 ******************************************************************ELTGVLX 
00027 *                                                                *ELTGVLX 
00028 *    COPYBOOK:   ELTGVLX                                         *ELTGVLX 
00029 *    DATE:       26-FEB-1990                                     *ELTGVLX 
00030 *    AUTHOR:     GEORGE E MOORE                                  *ELTGVLX 
00031 *    FUNCTION:   THIS MODULE WILL DISPLAY ALL OUTPUT ASSOCIATED  *ELTGVLX 
00032 *                FOR THE GROUP VARIABLE LEVEL PROVIDERS.         *ELTGVLX 
00033 *    NOTES:      X---                                            *ELTGVLX 
00034 *                                                                *ELTGVLX 
00035 ******************************************************************ELTGVLX 
00036 *                                                                *ELTGVLX 
00037 *                      MAINTENANCE HISTORY                       *ELTGVLX 
00038 *                                                                *ELTGVLX 
00039 *  MOD     DATE     BY  DRPT                ACTION               *ELTGVLX 
00040 * ----- ----------- --- ----- ---------------------------------- *ELTGVLX 
00041 * 01.00 26-FEB-1990 GEM       CREATED                            *ELTGVLX 
00042 *                                                                *ELTGVLX 
00043 ******************************************************************ELTGVLX 
00044                                                                   ELTGVLX 
00045  DATA DIVISION.                                                   ELTGVLX 
00046                                                                   ELTGVLX 
00047  WORKING-STORAGE SECTION.                                         ELTGVLX 
00048  01  WS-MISC.                                                     ELTGVLX 
00049      05  WS-BEGIN                      PIC X(26)  VALUE           ELTGVLX 
00050      '*** ELTGVLX  WS BEGINS ***'.                                ELTGVLX 
00051                                                                   ELTGVLX 
00052  01  WS-COUNTERS.                                                 ELTGVLX 
00053      05  WS-GVLX-ENTRIES   COMP-3      PIC S9(02) VALUE 0.        ELTGVLX 
00054                                                                   ELTGVLX 
00055  01  WS-SUBSCRIPTS.                                               ELTGVLX 
00056      05  WS-GCG-SUB        COMP SYNC   PIC S9(03) VALUE 0.        ELTGVLX 
00057      05  WS-HOLD-SUB       COMP SYNC   PIC S9(03) VALUE 0.        ELTGVLX 
00058                                                                   ELTGVLX 
00059  01  WS-TAB-AREA.                                                 ELTGVLX 
00060      05  WS-TABS           OCCURS   6  TIMES.                     ELTGVLX 
00061          07  WS-TAB-ID                 PIC X(06).                 ELTGVLX 
00062          07  WS-TAB-SLOT-NO            PIC S9(07) COMP-3.         ELTGVLX 
00063                                                                   ELTGVLX 
00064  01  WS-HOLD-AREA.                                                ELTGVLX 
00065      05  WS-GPPO-PROV-SLOT-NO   COMP-3 PIC S9(07) VALUE +0.       ELTGVLX 
00066                                                                   ELTGVLX 
00067  01  WS-VAR-LVL-HEADERS.                                          ELTGVLX 
00068      05  WS-VAR-LVL-1-FAC-LINE.                                   ELTGVLX 
00069          10  FILLER               PIC X(21) VALUE SPACES.         ELTGVLX 
00070          10  FILLER               PIC X(37) VALUE                 ELTGVLX 
00071          'VARIABLE LEVEL ONE FACILITY PROVIDERS'.                 ELTGVLX 
00072          10  FILLER               PIC X(21) VALUE SPACES.         ELTGVLX 
00073                                                                   ELTGVLX 
00074      05  WS-VAR-LVL-2-FAC-LINE.                                   ELTGVLX 
00075          10  FILLER               PIC X(21) VALUE SPACES.         ELTGVLX 
00076          10  FILLER               PIC X(37) VALUE                 ELTGVLX 
00077          'VARIABLE LEVEL TWO FACILITY PROVIDERS'.                 ELTGVLX 
00078          10  FILLER               PIC X(21) VALUE SPACES.         ELTGVLX 
00079                                                                   ELTGVLX 
00080      05  WS-VAR-LVL-3-FAC-LINE.                                   ELTGVLX 
00081          10  FILLER               PIC X(20) VALUE SPACES.         ELTGVLX 
00082          10  FILLER               PIC X(39) VALUE                 ELTGVLX 
00083          'VARIABLE LEVEL THREE FACILITY PROVIDERS'.               ELTGVLX 
00084          10  FILLER               PIC X(20) VALUE SPACES.         ELTGVLX 
00085                                                                   ELTGVLX 
00086      05  WS-VAR-LVL-1-PRO-LINE.                                   ELTGVLX 
00087          10  FILLER               PIC X(19) VALUE SPACES.         ELTGVLX 
00088          10  FILLER               PIC X(41) VALUE                 ELTGVLX 
00089          'VARIABLE LEVEL ONE PROFESSIONAL PROVIDERS'.             ELTGVLX 
00090          10  FILLER               PIC X(19) VALUE SPACES.         ELTGVLX 
00091                                                                   ELTGVLX 
00092      05  WS-VAR-LVL-2-PRO-LINE.                                   ELTGVLX 
00093          10  FILLER               PIC X(19) VALUE SPACES.         ELTGVLX 
00094          10  FILLER               PIC X(41) VALUE                 ELTGVLX 
00095          'VARIABLE LEVEL TWO PROFESSIONAL PROVIDERS'.             ELTGVLX 
00096          10  FILLER               PIC X(19) VALUE SPACES.         ELTGVLX 
00097                                                                   ELTGVLX 
00098      05  WS-VAR-LVL-3-PRO-LINE.                                   ELTGVLX 
00099          10  FILLER               PIC X(18) VALUE SPACES.         ELTGVLX 
00100          10  FILLER               PIC X(43) VALUE                 ELTGVLX 
00101          'VARIABLE LEVEL THREE PROFESSIONAL PROVIDERS'.           ELTGVLX 
00102          10  FILLER               PIC X(18) VALUE SPACES.         ELTGVLX 
00103                                                                   ELTGVLX 
00104      05  WS-VAR-LVL-FAC-LINE.                                     ELTGVLX 
00105          10  FILLER               PIC X(23) VALUE SPACES.         ELTGVLX 
00106          10  FILLER               PIC X(33) VALUE                 ELTGVLX 
00107          'VARIABLE LEVEL FACILITY PROVIDERS'.                     ELTGVLX 
00108          10  FILLER               PIC X(23) VALUE SPACES.         ELTGVLX 
00109                                                                   ELTGVLX 
00110      05  WS-NO-SPEC-BEN-MSG.                                      ELTGVLX 
00111          10  FILLER               PIC X(40) VALUE                 ELTGVLX 
00112          'NO SPECIAL BENEFITS APPLY FOR THIS GROUP'.              ELTGVLX 
00113          10  FILLER               PIC X(39) VALUE SPACES.         ELTGVLX 
00114      EJECT                                                        ELTGVLX 
00115  LINKAGE SECTION.                                                 ELTGVLX 
00116  01  DFHCOMMAREA.                                                 ELTGVLX 
00117      COPY ELSCOMMC.                                               ELTGVLX 
00118 /                                                                 ELTGVLX 
00119      COPY ELSCIA2C.                                               ELTGVLX 
00120 /                                                                 ELTGVLX 
00121      COPY ELSCMDSC.                                               ELTGVLX 
00122 /                                                                 ELTGVLX 
00123      COPY ELSCMIFC.                                               ELTGVLX 
00124 /                                                                 ELTGVLX 
00125      COPY ELSIOPMC.                                               ELTGVLX 
00126 /                                                                 ELTGVLX 
00127      COPY ELSKEYSC.                                               ELTGVLX 
00128 /                                                                 ELTGVLX 
00129      COPY ELSOUTPC.                                               ELTGVLX 
00130 /                                                                 ELTGVLX 
00131      COPY ELSSRTPC.                                               ELTGVLX 
00132 /                                                                 ELTGVLX 
00133      COPY ELSTCWAC.                                               ELTGVLX 
00134 /                                                                 ELTGVLX 
00135      COPY ELSSSCBC.                                               ELTGVLX 
00136 /                                                                 ELTGVLX 
00137  01  GROUP-SPECIFIC-REC.                                          ELTGVLX 
00138      COPY GCGROUPC.                                               ELTGVLX 
00139 /                                                                 ELTGVLX 
00140      EJECT                                                        ELTGVLX 
00141  PROCEDURE DIVISION.                                              ELTGVLX 
00142 ******************************************************************ELTGVLX 
00143 *                                                                 ELTGVLX 
00144 *    G R O U P   V A R I A B L E   L E V E L   P R O V I D E R S  ELTGVLX 
00145 *                                                                 ELTGVLX 
00146 ******************************************************************ELTGVLX 
00147  GROUP-VARIABLE-LEVEL-PROVIDERS.                                  ELTGVLX 
00148      PERFORM WORK-N-CONTROL-BLOCK-ADDRSSING.                      ELTGVLX 
00149      PERFORM PROCESS-GVL-PROVIDERS.                               ELTGVLX 
00150      PERFORM TERMINATE-OUTPUT.                                    ELTGVLX 
00151      GOBACK.                                                      ELTGVLX 
00152                                                                   ELTGVLX 
00153 ******************************************************************ELTGVLX 
00154 *                                                                 ELTGVLX 
00155 *    W O R K  &  C O N T R O L  B L O C K I N G  A D R E S S I N GELTGVLX 
00156 *                                                                 ELTGVLX 
00157 ******************************************************************ELTGVLX 
00158  WORK-N-CONTROL-BLOCK-ADDRSSING.                                  ELTGVLX 
00159      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTGVLX 
00160         EXEC CICS ABEND   ABCODE('EL01')   END-EXEC.              ELTGVLX 
00161                                                                   ELTGVLX 
00162      IF ECA-CIA-PTR = NULL                                        ELTGVLX 
00163         EXEC CICS ABEND  ABCODE('EL02')  END-EXEC                 ELTGVLX 
00164      ELSE                                                         ELTGVLX 
00165         CALL 'ELUINISM' USING DFHCOMMAREA                         ELTGVLX 
00166           ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.               ELTGVLX 
00167                                                                   ELTGVLX 
00168      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTGVLX 
00169      IF CIA-RC-PTR-NULL                                           ELTGVLX 
00170         PERFORM SIGNAL-UNALLOC-AREA-ERROR                         ELTGVLX 
00171      ELSE                                                         ELTGVLX 
00172         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTGVLX 
00173            ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                ELTGVLX 
00174                                                                   ELTGVLX 
00175      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTGVLX 
00176      IF CIA-RC-PTR-NULL                                           ELTGVLX 
00177         PERFORM SIGNAL-UNALLOC-AREA-ERROR                         ELTGVLX 
00178      ELSE                                                         ELTGVLX 
00179         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTGVLX 
00180            ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                 ELTGVLX 
00181                                                                   ELTGVLX 
00182      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTGVLX 
00183      IF CIA-RC-PTR-NULL                                           ELTGVLX 
00184         PERFORM SIGNAL-UNALLOC-AREA-ERROR                         ELTGVLX 
00185      ELSE                                                         ELTGVLX 
00186         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTGVLX 
00187            ADDRESS OF COF-OUTPUT-INTERFACE.                       ELTGVLX 
00188                                                                   ELTGVLX 
00189      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELTGVLX 
00190      IF CIA-RC-PTR-NULL                                           ELTGVLX 
00191         PERFORM SIGNAL-UNALLOC-AREA-ERROR                         ELTGVLX 
00192      ELSE                                                         ELTGVLX 
00193         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTGVLX 
00194            ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                  ELTGVLX 
00195                                                                   ELTGVLX 
00196      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTGVLX 
00197      IF CIA-RC-PTR-NULL                                           ELTGVLX 
00198         PERFORM SIGNAL-UNALLOC-AREA-ERROR                         ELTGVLX 
00199      ELSE                                                         ELTGVLX 
00200         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTGVLX 
00201            ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                 ELTGVLX 
00202                                                                   ELTGVLX 
00203      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTGVLX 
00204      IF CIA-RC-PTR-NULL                                           ELTGVLX 
00205         PERFORM SIGNAL-UNALLOC-AREA-ERROR                         ELTGVLX 
00206      ELSE                                                         ELTGVLX 
00207         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTGVLX 
00208            ADDRESS OF KWA-FILE-KEY-WORK-AREA.                     ELTGVLX 
00209                                                                   ELTGVLX 
00210      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTGVLX 
00211      IF CIA-RC-PTR-NULL                                           ELTGVLX 
00212         PERFORM SIGNAL-UNALLOC-AREA-ERROR                         ELTGVLX 
00213      ELSE                                                         ELTGVLX 
00214         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTGVLX 
00215            ADDRESS OF GROUP-SPECIFIC-REC.                         ELTGVLX 
00216                                                                   ELTGVLX 
00217  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTGVLX 
00218      SET CIA-AB-UNALLOC-AREA TO TRUE                              ELTGVLX 
00219      EXEC CICS ABEND                                              ELTGVLX 
00220                ABCODE(CIA-ABCODE)                                 ELTGVLX 
00221      END-EXEC.                                                    ELTGVLX 
00222      EJECT                                                        ELTGVLX 
00223 ******************************************************************ELTGVLX 
00224 *                                                                 ELTGVLX 
00225 *    P R O C E S S   G V L   P R O V I D E R S                    ELTGVLX 
00226 *                                                                 ELTGVLX 
00227 ******************************************************************ELTGVLX 
00228  PROCESS-GVL-PROVIDERS.                                           ELTGVLX 
00229      IF SSB-PROV-CLASS-INST                                       ELTGVLX 
00230         PERFORM GCG-TAB-ID-INST-LOOKUP                            ELTGVLX 
00231            VARYING WS-GCG-SUB FROM 1 BY 1                         ELTGVLX 
00232               UNTIL WS-HOLD-SUB = 3 OR                            ELTGVLX 
00233                  GCG-TAB-ID (GCG-INDEX) =  HIGH-VALUES.           ELTGVLX 
00234      IF SSB-PROV-CLASS-PROF                                       ELTGVLX 
00235         PERFORM GCG-TAB-ID-PROF-LOOKUP                            ELTGVLX 
00236            VARYING WS-GCG-SUB FROM 1 BY 1                         ELTGVLX 
00237               UNTIL WS-HOLD-SUB = 3 OR                            ELTGVLX 
00238                  GCG-TAB-ID (GCG-INDEX) =  HIGH-VALUES.           ELTGVLX 
00239      IF SSB-PROV-CLASS-BOTH                                       ELTGVLX 
00240         PERFORM GCG-TAB-ID-BOTH-LOOKUP                            ELTGVLX 
00241            VARYING WS-GCG-SUB FROM 1 BY 1                         ELTGVLX 
00242               UNTIL GCG-TAB-ID (GCG-INDEX) =  HIGH-VALUES         ELTGVLX 
00243                  OR WS-GCG-SUB > GCG-COUNT-TAB-PROVN-POINTERS.    ELTGVLX 
00244                                                                   ELTGVLX 
00245      IF WS-GVLX-ENTRIES > 0                                       ELTGVLX 
00246         PERFORM GENERATE-GVL-PROVIDERS-TEXT                       ELTGVLX 
00247            VARYING WS-HOLD-SUB FROM 1 BY 1                        ELTGVLX 
00248               UNTIL WS-HOLD-SUB > WS-GVLX-ENTRIES.                ELTGVLX 
00249                                                                   ELTGVLX 
00250      IF WS-GVLX-ENTRIES = 0                                       ELTGVLX 
00251         PERFORM GENERATE-NO-PROVIDERS-TEXT.                       ELTGVLX 
00252                                                                   ELTGVLX 
00253  GCG-TAB-ID-INST-LOOKUP.                                          ELTGVLX 
00254      SET GCG-INDEX TO WS-GCG-SUB.                                 ELTGVLX 
00255      IF GCG-TAB-ID (GCG-INDEX) = HIGH-VALUES                      ELTGVLX 
00256         CONTINUE                                                  ELTGVLX 
00257      ELSE                                                         ELTGVLX 
00258         IF GCG-TAB-SLOT-NO (GCG-INDEX) = ZEROES                   ELTGVLX 
00259            CONTINUE                                               ELTGVLX 
00260         ELSE                                                      ELTGVLX 
00261            IF GCG-TAB-ID (GCG-INDEX) =                            ELTGVLX 
00262              '#GVLF ' OR '#GVLG ' OR '#GVLH '                     ELTGVLX 
00263                     PERFORM MOVE-GCG-TAB-ID-SLOT-NO               ELTGVLX 
00264            ELSE                                                   ELTGVLX 
00265               CONTINUE                                            ELTGVLX 
00266            END-IF                                                 ELTGVLX 
00267         END-IF                                                    ELTGVLX 
00268      END-IF.                                                      ELTGVLX 
00269                                                                   ELTGVLX 
00270  GCG-TAB-ID-PROF-LOOKUP.                                          ELTGVLX 
00271      SET GCG-INDEX TO WS-GCG-SUB.                                 ELTGVLX 
00272      IF GCG-TAB-ID (GCG-INDEX) = HIGH-VALUES                      ELTGVLX 
00273         CONTINUE                                                  ELTGVLX 
00274      ELSE                                                         ELTGVLX 
00275         IF GCG-TAB-SLOT-NO (GCG-INDEX) = ZEROES                   ELTGVLX 
00276            CONTINUE                                               ELTGVLX 
00277         ELSE                                                      ELTGVLX 
00278            IF GCG-TAB-ID (GCG-INDEX) =                            ELTGVLX 
00279              '#GVLP ' OR '#GVLQ ' OR '#GVLR '                     ELTGVLX 
00280                     PERFORM MOVE-GCG-TAB-ID-SLOT-NO               ELTGVLX 
00281            ELSE                                                   ELTGVLX 
00282               CONTINUE                                            ELTGVLX 
00283            END-IF                                                 ELTGVLX 
00284         END-IF                                                    ELTGVLX 
00285      END-IF.                                                      ELTGVLX 
00286                                                                   ELTGVLX 
00287  GCG-TAB-ID-BOTH-LOOKUP.                                          ELTGVLX 
00288      SET GCG-INDEX TO WS-GCG-SUB.                                 ELTGVLX 
00289      IF GCG-TAB-ID (GCG-INDEX) = HIGH-VALUES                      ELTGVLX 
00290         CONTINUE                                                  ELTGVLX 
00291      ELSE                                                         ELTGVLX 
00292         IF GCG-TAB-SLOT-NO (GCG-INDEX) = ZEROES                   ELTGVLX 
00293            CONTINUE                                               ELTGVLX 
00294         ELSE                                                      ELTGVLX 
00295            IF GCG-TAB-ID (GCG-INDEX) =                            ELTGVLX 
00296              '#GVLF ' OR '#GVLG ' OR '#GVLH ' OR                  ELTGVLX 
00297              '#GVLP ' OR '#GVLQ ' OR '#GVLR '                     ELTGVLX 
00298                     PERFORM MOVE-GCG-TAB-ID-SLOT-NO               ELTGVLX 
00299            ELSE                                                   ELTGVLX 
00300               CONTINUE                                            ELTGVLX 
00301            END-IF                                                 ELTGVLX 
00302         END-IF                                                    ELTGVLX 
00303      END-IF.                                                      ELTGVLX 
00304                                                                   ELTGVLX 
00305  MOVE-GCG-TAB-ID-SLOT-NO.                                         ELTGVLX 
00306      ADD 1 TO WS-HOLD-SUB.                                        ELTGVLX 
00307      ADD 1 TO WS-GVLX-ENTRIES.                                    ELTGVLX 
00308      MOVE GCG-TAB-ID (GCG-INDEX)                                  ELTGVLX 
00309        TO WS-TAB-ID (WS-HOLD-SUB).                                ELTGVLX 
00310      MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                             ELTGVLX 
00311        TO WS-TAB-SLOT-NO (WS-HOLD-SUB).                           ELTGVLX 
00312                                                                   ELTGVLX 
00313  GENERATE-GVL-PROVIDERS-TEXT.                                     ELTGVLX 
00314      SET COF-NEW-PAGE           TO TRUE.                          ELTGVLX 
00315      MOVE +0                    TO COF-NBR-DTL-LINES.             ELTGVLX 
00316      MOVE +2                    TO COF-NBR-HDR-LINES.             ELTGVLX 
00317      EVALUATE WS-TAB-ID (WS-HOLD-SUB)                             ELTGVLX 
00318        WHEN '#GVLF '                                              ELTGVLX 
00319          MOVE WS-VAR-LVL-1-FAC-LINE TO                            ELTGVLX 
00320                               COF-HDR-LINE (COF-NBR-HDR-LINES)    ELTGVLX 
00321        WHEN '#GVLG '                                              ELTGVLX 
00322          MOVE WS-VAR-LVL-2-FAC-LINE TO                            ELTGVLX 
00323                               COF-HDR-LINE (COF-NBR-HDR-LINES)    ELTGVLX 
00324        WHEN '#GVLH '                                              ELTGVLX 
00325          MOVE WS-VAR-LVL-3-FAC-LINE TO                            ELTGVLX 
00326                               COF-HDR-LINE (COF-NBR-HDR-LINES)    ELTGVLX 
00327        WHEN '#GVLP '                                              ELTGVLX 
00328          MOVE WS-VAR-LVL-1-PRO-LINE TO                            ELTGVLX 
00329                               COF-HDR-LINE (COF-NBR-HDR-LINES)    ELTGVLX 
00330        WHEN '#GVLQ '                                              ELTGVLX 
00331          MOVE WS-VAR-LVL-2-PRO-LINE TO                            ELTGVLX 
00332                               COF-HDR-LINE (COF-NBR-HDR-LINES)    ELTGVLX 
00333        WHEN '#GVLR '                                              ELTGVLX 
00334          MOVE WS-VAR-LVL-3-PRO-LINE TO                            ELTGVLX 
00335                               COF-HDR-LINE (COF-NBR-HDR-LINES)    ELTGVLX 
00336        WHEN OTHER CONTINUE                                        ELTGVLX 
00337      END-EVALUATE.                                                ELTGVLX 
00338      PERFORM LINK-TO-OUTPUT.                                      ELTGVLX 
00339                                                                   ELTGVLX 
00340      MOVE WS-TAB-ID (WS-HOLD-SUB) TO SRP-TABULAR-ID.              ELTGVLX 
00341      MOVE WS-TAB-SLOT-NO (WS-HOLD-SUB) TO SRP-TABULAR-SLOT-NO.    ELTGVLX 
00342      EXEC CICS LINK                                               ELTGVLX 
00343                PROGRAM ('ELGGVLX')                                ELTGVLX 
00344                COMMAREA (DFHCOMMAREA)                             ELTGVLX 
00345      END-EXEC.                                                    ELTGVLX 
00346                                                                   ELTGVLX 
00347  GENERATE-NO-PROVIDERS-TEXT.                                      ELTGVLX 
00348      SET COF-NEW-PAGE         TO TRUE.                            ELTGVLX 
00349      MOVE +2                  TO COF-NBR-HDR-LINES.               ELTGVLX 
00350      MOVE WS-VAR-LVL-FAC-LINE TO                                  ELTGVLX 
00351                               COF-HDR-LINE (COF-NBR-HDR-LINES).   ELTGVLX 
00352      MOVE +1                  TO COF-NBR-DTL-LINES.               ELTGVLX 
00353      MOVE SPACES              TO                                  ELTGVLX 
00354                               COF-DTL-LINE (COF-NBR-DTL-LINES).   ELTGVLX 
00355      ADD  +1                  TO COF-NBR-DTL-LINES.               ELTGVLX 
00356      MOVE WS-NO-SPEC-BEN-MSG  TO                                  ELTGVLX 
00357                               COF-DTL-LINE (COF-NBR-DTL-LINES).   ELTGVLX 
00358      PERFORM LINK-TO-OUTPUT.                                      ELTGVLX 
00359                                                                   ELTGVLX 
00360  TERMINATE-OUTPUT.                                                ELTGVLX 
00361      SET COF-END TO TRUE.                                         ELTGVLX 
00362      PERFORM LINK-TO-OUTPUT.                                      ELTGVLX 
00363                                                                   ELTGVLX 
00364  LINK-TO-OUTPUT.                                                  ELTGVLX 
00365      EXEC CICS LINK                                               ELTGVLX 
00366                PROGRAM ('ELUOUTPT')                               ELTGVLX 
00367                COMMAREA (DFHCOMMAREA)                             ELTGVLX 
00368      END-EXEC.                                                    ELTGVLX 
