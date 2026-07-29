00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELSGVLM1
00003  PROGRAM-ID.         ELSGVLM1.                                       LV002
00004                                                                   ELSGVLM1
00005  AUTHOR.             JOHN BEIRNE                                  ELSGVLM1
00006                                                                   ELSGVLM1
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSGVLM1
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELSGVLM1
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSGVLM1
00010                      233 N. MICHIGAN AVE                          ELSGVLM1
00011                      CHICAGO, ILLINOIS 60601                      ELSGVLM1
00012                                                                   ELSGVLM1
00013  DATE-WRITTEN.       09-14-1993.                                  ELSGVLM1
00014                                                                   ELSGVLM1
00015  DATE-COMPILED.                                                   ELSGVLM1
00016                                                                   ELSGVLM1
00017  SECURITY.           COPYRIGHT 1993,                              ELSGVLM1
00018                      HEALTH CARE SERVICE CORPORATION              ELSGVLM1
00019      SKIP3                                                        ELSGVLM1
00020  ENVIRONMENT DIVISION.                                            ELSGVLM1
00021                                                                   ELSGVLM1
00022  CONFIGURATION SECTION.                                           ELSGVLM1
00023  SOURCE-COMPUTER.    IBM-3033.                                    ELSGVLM1
00024  OBJECT-COMPUTER.    IBM-3033.                                    ELSGVLM1
00025  TITLE 'ELS GENERIC VARIABLE LEVEL PROCESSOR              '.      ELSGVLM1
00026 ****************************************************************  ELSGVLM1
00027 *                                                              *  ELSGVLM1
00028 *    PROGRAM:    ELSGVLM1                                      *  ELSGVLM1
00029 *    DATE:       14-SEP-1993                                   *  ELSGVLM1
00030 *    AUTHOR:     JOHN BEIRNE                                   *  ELSGVLM1
00031 *    FUNCTION:                                                 *  ELSGVLM1
00032 *                                                              *  ELSGVLM1
00033 ****************************************************************  ELSGVLM1
00034 *                                                              *  ELSGVLM1
00035 *                   MAINTENANCE HISTORY                        *  ELSGVLM1
00036 *                                                              *  ELSGVLM1
00037 *  MOD     DATE     BY  DRPT              ACTION               *  ELSGVLM1
00038 * ----- ----------- --- ---- --------------------------------- *  ELSGVLM1
00039 * 01.00 14-SEP-1993 JPB      CREATED                           *  ELSGVLM1
00040 *                                                              *  ELSGVLM1
00041 * 01.01 22-DEC-1993 JPB      DELETED IF/ELSE STATEMENTS THAT   *  ELSGVLM1
00042 *                            DETERMINED SSB-PROVIDER-STATUS SO *  ELSGVLM1
00043 *                            THE PROGRAM LOOKS FOR ALL GVL'S.  *  ELSGVLM1
00044 * 01.02 23-JAN-1994 AKK      ADDED CODE TO 'NORMALIZE' SSB-    *  ELSGVLM1
00045 *                            RESPONSE TO LOOK LIKE THE GVX-    *  ELSGVLM1
00046 *                            PROVIDER-NUMBER.                  *  ELSGVLM1
00047 * 02.00 05-AUG-1998 AKK      CHANGED FOR YEAR 2000. MOST       *  ELSGVLM1
00048 *                            CHANGES FOR EIBDATE USAGE.        *  ELSGVLM1
00049 *                            PROVIDER-NUMBER.                  *  ELSGVLM1
00050 *       13-AUG-2003 AKK       TEST ORDER OF RECOMPILE          *  ELSGVLM1
00051 *                                                                 ELSGVLM1
00050 *       21-JUN-2006 AKK       INTERTEST STROAGE VIOLATION      *  ELSGVLM1
00051 ****************************************************************  ELSGVLM1
00052      EJECT                                                        ELSGVLM1
00053  DATA DIVISION.                                                   ELSGVLM1
00054  WORKING-STORAGE SECTION.                                         ELSGVLM1
00055 *                                                                 ELSGVLM1
00056 * TEXAS REGIONS FOR PACKAGE CODE CHECK                            ELSGVLM1
00057  01  WS-APPLID.                                                   ELSGVLM1
00058      02 FILLER                   PIC X(03).                       ELSGVLM1
00059      02 FILLER                   PIC X(04).                       ELSGVLM1
00060         88 TEXAS-REGION          VALUES                           ELSGVLM1
00061         'XAI1' 'XAI2' 'XAB1' 'XAB2' 'XAB3' 'XAB4' 'XAB5'          ELSGVLM1
00062         'XAB6' 'XAB7' 'XAB8' 'XAB9' 'XAS1' 'XAS2' 'XFB1'          ELSGVLM1
00063         'XFB2' 'XF01'.                                            ELSGVLM1
00064                                                                   ELSGVLM1
00065  01  WS-BEGIN.                                                    ELSGVLM1
00066                                                                   ELSGVLM1
00067      05  WS-SELECTED-KEYS-COUNTER    PIC  S9(04) COMP.            ELSGVLM1
00068                                                                   ELSGVLM1
00069      05  WS-INPUT-FIELDS-FOUND       PIC  S9(04) COMP.            ELSGVLM1
00070                                                                   ELSGVLM1
00071      05  WS-NUM-HEADINGS             PIC  S9(04) COMP.            ELSGVLM1
00072                                                                   ELSGVLM1
00073      05  WS-SAVE-KTG-IDX             USAGE IS INDEX.              ELSGVLM1
00074                                                                   ELSGVLM1
00075      05  WS-MAX-GCG-IDX              USAGE IS INDEX.              ELSGVLM1
00076                                                                   ELSGVLM1
00077      05  INPUTL                      PIC  S9(04) COMP.            ELSGVLM1
00078                                                                   ELSGVLM1
00079      05  WS-GVL-SLOT-NMBRS.                                       ELSGVLM1
00080          10  WS-GVLF-ID              PIC  X(06).                  ELSGVLM1
00081          10  WS-GVLF-SLOT-NO         PIC  S9(07) COMP-3.          ELSGVLM1
00082                                                                   ELSGVLM1
00083          10  WS-GVLG-ID              PIC  X(06).                  ELSGVLM1
00084          10  WS-GVLG-SLOT-NO         PIC  S9(07) COMP-3.          ELSGVLM1
00085                                                                   ELSGVLM1
00086          10  WS-GVLH-ID              PIC  X(06).                  ELSGVLM1
00087          10  WS-GVLH-SLOT-NO         PIC  S9(07) COMP-3.          ELSGVLM1
00088                                                                   ELSGVLM1
00089          10  WS-GVLP-ID              PIC  X(06).                  ELSGVLM1
00090          10  WS-GVLP-SLOT-NO         PIC  S9(07) COMP-3.          ELSGVLM1
00091                                                                   ELSGVLM1
00092          10  WS-GVLQ-ID              PIC  X(06).                  ELSGVLM1
00093          10  WS-GVLQ-SLOT-NO         PIC  S9(07) COMP-3.          ELSGVLM1
00094                                                                   ELSGVLM1
00095          10  WS-GVLR-ID              PIC  X(06).                  ELSGVLM1
00096          10  WS-GVLR-SLOT-NO         PIC  S9(07) COMP-3.          ELSGVLM1
00097                                                                   ELSGVLM1
00098      05  WS-GVL-FOUND-SWITCH         PIC X.                       ELSGVLM1
00099          88 NO-VALID-GVL-FOUND              VALUE 'N'.            ELSGVLM1
00100          88 VALID-GVL-FOUND                 VALUE 'Y'.            ELSGVLM1
00101                                                                   ELSGVLM1
00102      05  WS-PROV-NUM-ENTERED-SWITCH  PIC X.                       ELSGVLM1
00103          88 PROV-NUM-NOT-ENTERED            VALUE 'N'.            ELSGVLM1
00104          88 PROV-NUM-ENTERED                VALUE 'Y'.            ELSGVLM1
00105                                                                   ELSGVLM1
00106      05  WS-EMBEDDED-BLANK-SWITCH    PIC X.                       ELSGVLM1
00107          88 EMBEDDED-BLANK-FOUND            VALUE 'Y'.            ELSGVLM1
00108          88 NO-EMBEDDED-BLANK               VALUE 'N'.            ELSGVLM1
00109                                                                   ELSGVLM1
00110      05  WS-ERROR-SWITCH             PIC X.                       ELSGVLM1
00111          88  PROVIDER-NUMBER-OK             VALUE '0'.            ELSGVLM1
00112          88  PROVIDER-NUMBER-NOT-OK         VALUE '1'.            ELSGVLM1
00113                                                                   ELSGVLM1
00114      05  WS-NON-BLANK-SWTICH         PIC X.                       ELSGVLM1
00115          88  NON-BLANK-CHRCTR-FND           VALUE '0'.            ELSGVLM1
00116          88  NO-NON-BLANK-CHRCTR-FND        VALUE '1'.            ELSGVLM1
00117                                                                   ELSGVLM1
00118      05  WS-DATE-OUT                 PIC 99/99/99.                ELSGVLM1
00119                                                                   ELSGVLM1
00120      05  WS-FROM-DATE                PIC 99/99/99.                ELSGVLM1
00121                                                                   ELSGVLM1
00122      05  WS-TO-DATE                  PIC 99/99/99.                ELSGVLM1
00123                                                                   ELSGVLM1
00124      05  WS-TIME                     PIC 9(07) VALUE ZEROS.       ELSGVLM1
00125      05  WS-TIME-RED REDEFINES WS-TIME.                           ELSGVLM1
00126          10  WS-1                    PIC 9.                       ELSGVLM1
00127          10  WS-HH                   PIC 99.                      ELSGVLM1
00128          10  WS-MM                   PIC 99.                      ELSGVLM1
00129          10  WS-SS                   PIC 99.                      ELSGVLM1
00130                                                                   ELSGVLM1
00131      05  WS-TIME-OUT.                                             ELSGVLM1
00132          10  WS-HH-OUT               PIC 99.                      ELSGVLM1
00133          10  FILLER                  PIC X VALUE ':'.             ELSGVLM1
00134          10  WS-MM-OUT               PIC 99.                      ELSGVLM1
00135          10  FILLER                  PIC X VALUE ':'.             ELSGVLM1
00136          10  WS-SS-OUT               PIC 99.                      ELSGVLM1
00137                                                                   ELSGVLM1
00138      05  WS-NUMB-CHECK.                                           ELSGVLM1
00139          10  WS-NUMB-CHAR            OCCURS 10 TIMES              ELSGVLM1
00140                                      INDEXED BY NUMB-IDX          ELSGVLM1
00141                                      PIC X.                       ELSGVLM1
00142      05  WS-PRVDR-CHECK.                                          ELSGVLM1
00143          10  WS-PRVDR-CHAR           OCCURS 10 TIMES              ELSGVLM1
00144                                      INDEXED BY PRVDR-IDX         ELSGVLM1
00145                                      PIC X.                       ELSGVLM1
00146      05  WS-TEMP-PRVDR-NMBR          PIC X(10).                   ELSGVLM1
00147      05  WS-HOLD-PRVDR-NMBR REDEFINES WS-TEMP-PRVDR-NMBR.         ELSGVLM1
00148          10  WS-HOLD-PRVDR-CHAR      OCCURS 10 TIMES              ELSGVLM1
00149                                      INDEXED BY HOLD-IDX          ELSGVLM1
00150                                      PIC X.                       ELSGVLM1
00151      05  WS-CRNT-PSTN                PIC S9(04) COMP.             ELSGVLM1
00152      05  WS-STRT-PSTN                PIC S9(04) COMP.             ELSGVLM1
00153      05  WS-END-PSTN                 PIC S9(04) COMP.             ELSGVLM1
00154      05  WS-STRNG-STRT               PIC S9(04) COMP.             ELSGVLM1
00155                                                                   ELSGVLM1
00156      05  WS-INVALID-PROV-NUM         PIC X(50) VALUE              ELSGVLM1
00157          'SPACES ARE NOT ALLOWED CHECK THE NUMBER AND RETRY.'.    ELSGVLM1
00158      05  FILLER                      PIC X(49).                   ELSGVLM1
00159                                                                   ELSGVLM1
00160      05  WS-SPC-MNU-TITLE            PIC X(31) VALUE              ELSGVLM1
00161              'SPECIAL PROVIDER CONSIDERATIONS'.                   ELSGVLM1
00162                                                                   ELSGVLM1
00163      05  WS-NO-GVL-AVAILABLE-MSG.                                 ELSGVLM1
00164          10  WS-NO-GVL-AVAILABLE-MSG-1   PIC X(68) VALUE          ELSGVLM1
00165              'THIS GROUP DOES NOT HAVE A SPECIAL PROVIDER NETWORK ELSGVLM1
00166 -            'FOR THE PERIOD '.                                   ELSGVLM1
00167          10  WS-NO-GVL-AVAILABLE-MSG-2.                           ELSGVLM1
00168              15  WS-GROUP-FROM-DATE      PIC 99/99/99.            ELSGVLM1
00169              15  FILLER                  PIC X(04) VALUE ' TO '.  ELSGVLM1
00170              15  WS-GROUP-TO-DATE        PIC 99/99/99.            ELSGVLM1
00171              15  FILLER                  PIC X(48) VALUE          ELSGVLM1
00172          '.  PRESS <PF3> TO RETURN TO THE PREVIOUS SCREEN.'.      ELSGVLM1
00173                                                                   ELSGVLM1
00174      05  WS-NO-INST-GVL.                                          ELSGVLM1
00175          10  WS-NO-INST-GVL-MSG          PIC X(79) VALUE          ELSGVLM1
00176              'THIS GROUP DOES NOT HAVE ANY INSTITUTIONAL SPECIAL PELSGVLM1
00177 -            'ROVIDERS FOR THE PERIOD '.                          ELSGVLM1
00178                                                                   ELSGVLM1
00179      05  WS-NO-PROF-GVL.                                          ELSGVLM1
00180          10  WS-NO-PROF-GVL-MSG          PIC X(79) VALUE          ELSGVLM1
00181              'THIS GROUP DOES NOT HAVE ANY PROFESSIONAL SPECIAL PRELSGVLM1
00182 -            'OVIDERS FOR THE PERIOD '.                           ELSGVLM1
00183                                                                   ELSGVLM1
00184      05  WS-NO-INPUT-MSG             PIC X(49)     VALUE          ELSGVLM1
00185          'PLEASE MAKE A SELECTION OR PRESS PF3 TO RETURN.'.       ELSGVLM1
00186      05  FILLER                          PIC X(30).               ELSGVLM1
00187                                                                   ELSGVLM1
00188      05  WS-ONE-SELECTION-ONLY-MSG   PIC X(40)     VALUE          ELSGVLM1
00189          'ONLY ONE SELECTION IS ALLOWED AT A TIME.'.              ELSGVLM1
00190      05  FILLER                          PIC X(39).               ELSGVLM1
00191 /                                                                 ELSGVLM1
00192  01  WS-EIBDATE-AREA.                                             ELSGVLM1
00193      05  WS-EIBDATE.                                              ELSGVLM1
00194          10  WS-EIBDATE-CC.                                       ELSGVLM1
00195              15  WS-EIBDATE-1        PIC 9.                       ELSGVLM1
00196              15  WS-EIBDATE-2        PIC 9.                       ELSGVLM1
00197          10  WS-EIBDATE-DT           PIC 9(05).                   ELSGVLM1
00198      05  WS-EIBDATE-CEN REDEFINES WS-EIBDATE                      ELSGVLM1
00199                                      PIC 9(07).                   ELSGVLM1
00200 /                                                                 ELSGVLM1
00201  01  WS-CEN-DATE.                                                 ELSGVLM1
00202      05  WS-CEN-CC                     PIC 9(02).                 ELSGVLM1
00203      05  WS-CEN-YD                     PIC 9(05).                 ELSGVLM1
00204                                                                   ELSGVLM1
00205  01  WS-GREG-DATE-CEN.                                            ELSGVLM1
00206      05  WS-GREG-MM-CEN                PIC 99.                    ELSGVLM1
00207      05  WS-GREG-DD-CEN                PIC 99.                    ELSGVLM1
00208      05  WS-GREG-CC-CEN                PIC 99.                    ELSGVLM1
00209      05  WS-GREG-YY-CEN                PIC 99.                    ELSGVLM1
00210                                                                   ELSGVLM1
00211  01  WS-GREG-DATE.                                                ELSGVLM1
00212      05  WS-GREG-MM                    PIC 99.                    ELSGVLM1
00213      05  WS-GREG-DD                    PIC 99.                    ELSGVLM1
00214      05  WS-GREG-YY                    PIC 99.                    ELSGVLM1
00215 ****************************************************************  ELSGVLM1
00216 *  M A P   C O B O L   S C R E E N   D S E C T                 *  ELSGVLM1
00217 ****************************************************************  ELSGVLM1
00218  01  WS-IO-MAP-AREA-04           PIC  X(24) VALUE                 ELSGVLM1
00219          '*WS MAP I/O AREA EL04 *'.                               ELSGVLM1
00220      COPY EL04SETC.                                               ELSGVLM1
00221 /                                                                 ELSGVLM1
00222  01  WS-IO-MAP-AREA-00           PIC  X(24) VALUE                 ELSGVLM1
00223          '*WS MAP I/O AREA EL00 *'.                               ELSGVLM1
00224      COPY EL00SETC.                                               ELSGVLM1
00225 /                                                                 ELSGVLM1
00226      COPY MLDATE01.                                               ELSGVLM1
00227 /                                                                 ELSGVLM1
00228  01  HGADATES-PARM-LIST.                                          ELSGVLM1
00229      COPY HGCDAT01.                                               ELSGVLM1
00230 /                                                                 ELSGVLM1
00231      COPY DFHBMSCA.                                               ELSGVLM1
00232 /                                                                 ELSGVLM1
00233  LINKAGE SECTION.                                                 ELSGVLM1
00234  01  DFHCOMMAREA.                                                 ELSGVLM1
00235  COPY ELSCOMMC.                                                   ELSGVLM1
00236      EJECT                                                        ELSGVLM1
00237  COPY ELSIOPMC.                                                   ELSGVLM1
00238      EJECT                                                        ELSGVLM1
00239  COPY ELSKTBGC.                                                   ELSGVLM1
00240      EJECT                                                        ELSGVLM1
00241  COPY ELSKEYSC.                                                   ELSGVLM1
00242      EJECT                                                        ELSGVLM1
00243  COPY ELSSLNRC.                                                   ELSGVLM1
00244      EJECT                                                        ELSGVLM1
00245  COPY ELSCIA2C.                                                   ELSGVLM1
00246      EJECT                                                        ELSGVLM1
00247  COPY ELSMENUC.                                                   ELSGVLM1
00248      EJECT                                                        ELSGVLM1
00249  COPY ELSMHDGC.                                                   ELSGVLM1
00250      EJECT                                                        ELSGVLM1
00251  COPY ELSMOPTC.                                                   ELSGVLM1
00252      EJECT                                                        ELSGVLM1
00253  COPY ELSSSCBC.                                                   ELSGVLM1
00254      EJECT                                                        ELSGVLM1
00255  01 GCG-GRP-SPEC-RECORD-AREA.                                     ELSGVLM1
00256  COPY GCGROUPC.                                                   ELSGVLM1
00257      EJECT                                                        ELSGVLM1
00258  PROCEDURE DIVISION.                                              ELSGVLM1
00259 ************************************************************      ELSGVLM1
00260 *                                                          *      ELSGVLM1
00261 *        ELSGVLM1 MAINLINE                                 *      ELSGVLM1
00262 *                                                          *      ELSGVLM1
00263 ************************************************************      ELSGVLM1
00264  0000-ELSGVLM1-MAINLINE.                                          ELSGVLM1
00265      EXEC CICS ASSIGN APPLID (WS-APPLID) END-EXEC.                ELSGVLM1
00266      PERFORM 0100-INITIALIZE.                                     ELSGVLM1
00267      PERFORM 1000-PROCESS-GVLX-FILE.                              ELSGVLM1
00268      GOBACK.                                                      ELSGVLM1
00269                                                                   ELSGVLM1
00270 ************************************************************      ELSGVLM1
00271 *                                                          *      ELSGVLM1
00272 *        INITIALIZATION                                    *      ELSGVLM1
00273 *                                                          *      ELSGVLM1
00274 ************************************************************      ELSGVLM1
00275  0100-INITIALIZE.                                                 ELSGVLM1
00276      PERFORM 0200-ESTABLISH-ENVIRONMENT.                          ELSGVLM1
00277      PERFORM 0300-EST-ADDRS-TO-SSB.                               ELSGVLM1
00278      PERFORM 0400-EST-ADDRS-TO-KEY-WRK.                           ELSGVLM1
00279      PERFORM 0500-EST-ADDRS-TO-GRP-KEY-TAB.                       ELSGVLM1
00280      PERFORM 0600-EST-ADDRS-TO-SLN.                               ELSGVLM1
00281      PERFORM 0700-EST-ADDRS-TO-MSO.                               ELSGVLM1
00282      PERFORM 0800-OBTAIN-DATE.                                    ELSGVLM1
00283      PERFORM 0900-OBTAIN-TIME.                                    ELSGVLM1
00284                                                                   ELSGVLM1
00285 ************************************************************      ELSGVLM1
00286 *                                                              *  ELSGVLM1
00287 *             ESTABLISH ENVIRONMENT                            *  ELSGVLM1
00288 *                                                              *  ELSGVLM1
00289 ****************************************************************  ELSGVLM1
00290  0200-ESTABLISH-ENVIRONMENT.                                      ELSGVLM1
00291      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELSGVLM1
00292         EXEC CICS ABEND                                           ELSGVLM1
00293                   ABCODE ('EL01')                                 ELSGVLM1
00294         END-EXEC                                                  ELSGVLM1
00295         ELSE IF ECA-CIA-PTR = NULL                                ELSGVLM1
00296                 EXEC CICS ABEND                                   ELSGVLM1
00297                           ABCODE ('EL02')                         ELSGVLM1
00298                 END-EXEC                                          ELSGVLM1
00299              ELSE                                                 ELSGVLM1
00300              CALL 'ELUINISM' USING DFHCOMMAREA                    ELSGVLM1
00301                         ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA. ELSGVLM1
00302                                                                   ELSGVLM1
00303 ****************************************************************  ELSGVLM1
00304 *                                                          *      ELSGVLM1
00305 *        ESTABLISH ADDRESSABILITY TO SELECTOR STATUS BLOCK *      ELSGVLM1
00306 *                                                          *      ELSGVLM1
00307 ************************************************************      ELSGVLM1
00308  0300-EST-ADDRS-TO-SSB.                                           ELSGVLM1
00309      SET CIA-ELSSSCB-DDN TO TRUE                                  ELSGVLM1
00310      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGVLM1
00311                            ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK ELSGVLM1
00312      END-CALL                                                     ELSGVLM1
00313      IF CIA-RC-PTR-NULL                                           ELSGVLM1
00314         SET CIA-AB-PARM-MISSING TO TRUE                           ELSGVLM1
00315         EXEC CICS ABEND                                           ELSGVLM1
00316                   ABCODE (CIA-ABCODE)                             ELSGVLM1
00317         END-EXEC.                                                 ELSGVLM1
00318                                                                   ELSGVLM1
00319 ************************************************************      ELSGVLM1
00320 *                                                          *      ELSGVLM1
00321 *        ESTABLISH ADDRESSABILITY TO KEY WORK AREA         *      ELSGVLM1
00322 *                                                          *      ELSGVLM1
00323 ************************************************************      ELSGVLM1
00324  0400-EST-ADDRS-TO-KEY-WRK.                                       ELSGVLM1
00325      SET CIA-ELSKEYS-DDN TO TRUE                                  ELSGVLM1
00326      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGVLM1
00327                            ADDRESS OF KWA-FILE-KEY-WORK-AREA      ELSGVLM1
00328      END-CALL                                                     ELSGVLM1
00329      IF CIA-RC-PTR-NULL                                           ELSGVLM1
00330         SET CIA-ELSKEYS-DDN TO TRUE                               ELSGVLM1
00331         SET CIA-STG-GETMAIN TO TRUE                               ELSGVLM1
00332         PERFORM 4300-CALL-STORAGE-MANAGER                         ELSGVLM1
00333         SET CIA-ELSKEYS-DDN TO TRUE                               ELSGVLM1
00334         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELSGVLM1
00335                               ADDRESS OF KWA-FILE-KEY-WORK-AREA   ELSGVLM1
00336         END-CALL.                                                 ELSGVLM1
00337                                                                   ELSGVLM1
00338 ************************************************************      ELSGVLM1
00339 *                                                          *      ELSGVLM1
00340 *    ESTABLISH ADDRESSABILITY TO GROUP SPECIFIC KEY TABLE  *      ELSGVLM1
00341 *                                                          *      ELSGVLM1
00342 ************************************************************      ELSGVLM1
00343  0500-EST-ADDRS-TO-GRP-KEY-TAB.                                   ELSGVLM1
00344      SET CIA-ELSKTBG-DDN TO TRUE                                  ELSGVLM1
00345      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGVLM1
00346                            ADDRESS OF KTG-GCGRPSPC-KEY-TABLE      ELSGVLM1
00347      END-CALL                                                     ELSGVLM1
00348      IF CIA-RC-PTR-NULL                                           ELSGVLM1
00349         SET CIA-ELSKTBG-DDN TO TRUE                               ELSGVLM1
00350         SET CIA-STG-RETRIEVE TO TRUE                              ELSGVLM1
00351         PERFORM 4300-CALL-STORAGE-MANAGER                         ELSGVLM1
00352         SET CIA-ELSKTBG-DDN TO TRUE                               ELSGVLM1
00353         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELSGVLM1
00354                               ADDRESS OF KTG-GCGRPSPC-KEY-TABLE   ELSGVLM1
00355         END-CALL.                                                 ELSGVLM1
00356                                                                   ELSGVLM1
00357 ************************************************************      ELSGVLM1
00358 *                                                          *      ELSGVLM1
00359 *    ESTABLISH ADDRESSABILITY TO SELECTION SLOT NUMBERS    *      ELSGVLM1
00360 *                                                          *      ELSGVLM1
00361 ************************************************************      ELSGVLM1
00362  0600-EST-ADDRS-TO-SLN.                                           ELSGVLM1
00363      SET CIA-ELSSLNR-DDN TO TRUE                                  ELSGVLM1
00364      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGVLM1
00365                            ADDRESS OF SLN-SELECTION-SLOT-NUMBERS  ELSGVLM1
00366      END-CALL                                                     ELSGVLM1
00367      IF CIA-RC-PTR-NULL                                           ELSGVLM1
00368         SET CIA-ELSSLNR-DDN TO TRUE                               ELSGVLM1
00369         SET CIA-STG-GETMAIN  TO TRUE                              ELSGVLM1
00370         PERFORM 4300-CALL-STORAGE-MANAGER                         ELSGVLM1
00371         SET CIA-ELSSLNR-DDN TO TRUE                               ELSGVLM1
00372         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELSGVLM1
00373                            ADDRESS OF SLN-SELECTION-SLOT-NUMBERS  ELSGVLM1
00374         END-CALL.                                                 ELSGVLM1
00375                                                                   ELSGVLM1
00376 ************************************************************      ELSGVLM1
00377 *                                                          *      ELSGVLM1
00378 *    ESTABLISH ADDRESSABILITY TO MENU OPTIONS AREA         *      ELSGVLM1
00379 *                                                          *      ELSGVLM1
00380 ************************************************************      ELSGVLM1
00381  0700-EST-ADDRS-TO-MSO.                                           ELSGVLM1
00382      SET CIA-ELSMOPT-DDN TO TRUE                                  ELSGVLM1
00383      COMPUTE CIA-AREA-LEN = LENGTH OF MSO-MENU-OPTS-HEADER +      ELSGVLM1
00384              (LENGTH OF MSO-MENU-OPT * 1).                        ELSGVLM1
00385      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGVLM1
00386                            ADDRESS OF MSO-MENU-SELECTION-VALUES   ELSGVLM1
00387      END-CALL                                                     ELSGVLM1
00388      IF CIA-RC-PTR-NULL                                           ELSGVLM1
00389         SET CIA-ELSMOPT-DDN TO TRUE                               ELSGVLM1
00390         COMPUTE CIA-AREA-LEN = LENGTH OF MSO-MENU-OPTS-HEADER +   ELSGVLM1
00391              (LENGTH OF MSO-MENU-OPT * 1)                         ELSGVLM1
00392         SET CIA-STG-GETMAIN  TO TRUE                              ELSGVLM1
00393         PERFORM 4300-CALL-STORAGE-MANAGER                         ELSGVLM1
00394         SET CIA-ELSMOPT-DDN TO TRUE                               ELSGVLM1
00395         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELSGVLM1
00396                            ADDRESS OF MSO-MENU-SELECTION-VALUES   ELSGVLM1
00397         END-CALL.                                                 ELSGVLM1
00398                                                                   ELSGVLM1
00399 ************************************************************      ELSGVLM1
00400 *                                                          *      ELSGVLM1
00401 *        OBTAIN DATE                                       *      ELSGVLM1
00402 *                                                          *      ELSGVLM1
00403 ************************************************************      ELSGVLM1
00404  0800-OBTAIN-DATE.                                                ELSGVLM1
00405      MOVE  EIBDATE                TO WS-EIBDATE-CEN.              ELSGVLM1
00406      IF  WS-EIBDATE-2 = 0                                         ELSGVLM1
00407          MOVE 19                  TO WS-CEN-CC                    ELSGVLM1
00408          MOVE WS-EIBDATE-DT       TO WS-CEN-YD                    ELSGVLM1
00409      ELSE                                                         ELSGVLM1
00410      IF  WS-EIBDATE-2 = 1                                         ELSGVLM1
00411          MOVE 20                  TO WS-CEN-CC                    ELSGVLM1
00412          MOVE WS-EIBDATE-DT       TO WS-CEN-YD.                   ELSGVLM1
00413                                                                   ELSGVLM1
00414      MOVE WS-CEN-DATE TO MLDATE-JULIAN1.                          ELSGVLM1
00415      PERFORM 1850-CNVRT-DTE-FROM-JUL-TO-GR.                       ELSGVLM1
00416      MOVE MLDATE-DATE2 TO WS-GREG-DATE-CEN.                       ELSGVLM1
00417      MOVE WS-GREG-MM-CEN TO WS-GREG-MM.                           ELSGVLM1
00418      MOVE WS-GREG-DD-CEN TO WS-GREG-DD.                           ELSGVLM1
00419      MOVE WS-GREG-YY-CEN TO WS-GREG-YY.                           ELSGVLM1
00420      MOVE WS-GREG-DATE      TO  WS-DATE-OUT.                      ELSGVLM1
00421 *    MOVE EIBDATE           TO  HGADATE-JULIAN1.                  ELSGVLM1
00422 *    PERFORM 0850-CNVRT-DTE-FROM-JUL-TO-GR.                       ELSGVLM1
00423 *    MOVE HGADATE-DATE2     TO  WS-DATE-OUT.                      ELSGVLM1
00424      EJECT                                                        ELSGVLM1
00425                                                                   ELSGVLM1
00426 ************************************************************      ELSGVLM1
00427 *                                                          *      ELSGVLM1
00428 *        CONVERT DATE FROM JULIAN TO GREG                  *      ELSGVLM1
00429 *                                                          *      ELSGVLM1
00430 ************************************************************      ELSGVLM1
00431  0850-CNVRT-DTE-FROM-JUL-TO-GR.                                   ELSGVLM1
00432      MOVE 'CNV'  TO  HGADATE-FUNC.                                ELSGVLM1
00433      MOVE 'J'    TO  HGADATE-FORM1.                               ELSGVLM1
00434      MOVE 'M'    TO  HGADATE-FORM2.                               ELSGVLM1
00435      MOVE ZEROS  TO  HGADATE-RETURN, HGADATE-AMOUNT,              ELSGVLM1
00436          HGADATE-DATE2.                                           ELSGVLM1
00437      EXEC CICS LINK PROGRAM ('HGADATES')                          ELSGVLM1
00438                     COMMAREA (HGADATES-PARM-LIST)                 ELSGVLM1
00439                     END-EXEC.                                     ELSGVLM1
00440                                                                   ELSGVLM1
00441 ************************************************************      ELSGVLM1
00442 *                                                          *      ELSGVLM1
00443 *        CONVERT DATE FROM JULIAN TO GREG                  *      ELSGVLM1
00444 *    MLDATES                                               *      ELSGVLM1
00445 ************************************************************      ELSGVLM1
00446  1850-CNVRT-DTE-FROM-JUL-TO-GR.                                   ELSGVLM1
00447      MOVE 'CNV'  TO  MLDATE-FUNC.                                 ELSGVLM1
00448      MOVE 'J'    TO  MLDATE-FORM1.                                ELSGVLM1
00449      MOVE 'M'    TO  MLDATE-FORM2.                                ELSGVLM1
00450      MOVE ZEROS  TO  MLDATE-RETURN, MLDATE-AMOUNT,                ELSGVLM1
00451          MLDATE-DATE2.                                            ELSGVLM1
00452      EXEC CICS LINK PROGRAM ('MLDATEC')                           ELSGVLM1
00453                     COMMAREA (MLDATE01)                           ELSGVLM1
00454                     END-EXEC.                                     ELSGVLM1
00455                                                                   ELSGVLM1
00456 ************************************************************      ELSGVLM1
00457 *                                                          *      ELSGVLM1
00458 *        OBTAIN CURRENT TIME                               *      ELSGVLM1
00459 *                                                          *      ELSGVLM1
00460 ************************************************************      ELSGVLM1
00461  0900-OBTAIN-TIME.                                                ELSGVLM1
00462      MOVE EIBTIME      TO  WS-TIME.                               ELSGVLM1
00463      MOVE WS-HH        TO  WS-HH-OUT.                             ELSGVLM1
00464      MOVE WS-MM        TO  WS-MM-OUT.                             ELSGVLM1
00465      MOVE WS-SS        TO  WS-SS-OUT.                             ELSGVLM1
00466                                                                   ELSGVLM1
00467 ************************************************************      ELSGVLM1
00468 *                                                          *      ELSGVLM1
00469 *        PROCESS GVLX FILE                                 *      ELSGVLM1
00470 *                                                          *      ELSGVLM1
00471 ************************************************************      ELSGVLM1
00472  1000-PROCESS-GVLX-FILE.                                          ELSGVLM1
00473      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSGVLM1
00474         PERFORM 1100-HANDLE-INITIAL-STATE                         ELSGVLM1
00475      ELSE IF SSB-PRIMARY-SCREEN-OUT (SSB-SELECTOR-STATE) OR       ELSGVLM1
00476              SSB-SECONDARY-SCREEN-OUT (SSB-SELECTOR-STATE)        ELSGVLM1
00477              PERFORM 1500-HANDLE-PRIM-SEC-ST                      ELSGVLM1
00478           ELSE IF SSB-MENU-COMPLETE (SSB-SELECTOR-STATE)          ELSGVLM1
00479                   SET SSB-START-MENU (SSB-SELECTOR-STATE)         ELSGVLM1
00480                   TO TRUE.                                        ELSGVLM1
00481                                                                   ELSGVLM1
00482 ************************************************************      ELSGVLM1
00483 *                                                          *      ELSGVLM1
00484 *        HANDLE INITIAL STATE                              *      ELSGVLM1
00485 *                                                          *      ELSGVLM1
00486 ************************************************************      ELSGVLM1
00487  1100-HANDLE-INITIAL-STATE.                                       ELSGVLM1
00488      PERFORM 1200-DETERMINE-IF-GVL-EXISTS.                        ELSGVLM1
00489      IF VALID-GVL-FOUND                                           ELSGVLM1
00490         PERFORM 1300-SEND-PRIMARY-SCREEN                          ELSGVLM1
00491      ELSE PERFORM 2700-SEND-NO-GVL.                               ELSGVLM1
00492                                                                   ELSGVLM1
00493 ************************************************************      ELSGVLM1
00494 *                                                          *      ELSGVLM1
00495 *        DETERMINE IF GVL EXISTS                           *      ELSGVLM1
00496 *                                                          *      ELSGVLM1
00497 ************************************************************      ELSGVLM1
00498  1200-DETERMINE-IF-GVL-EXISTS.                                    ELSGVLM1
00499      PERFORM 3500-GET-GROUP-RECORD.                               ELSGVLM1
00500      SET NO-VALID-GVL-FOUND TO TRUE.                              ELSGVLM1
00501      INITIALIZE WS-GVL-SLOT-NMBRS.                                ELSGVLM1
00502      SET GCG-INDEX TO GCG-COUNT-TAB-PROVN-POINTERS.               ELSGVLM1
00503      SET WS-MAX-GCG-IDX TO GCG-INDEX.                             ELSGVLM1
00504      PERFORM 4000-SEARCH-FOR-GVLFGH.                              ELSGVLM1
00505      PERFORM 4100-SEARCH-FOR-GVLPQR.                              ELSGVLM1
00506      IF VALID-GVL-FOUND                                           ELSGVLM1
00507         PERFORM 4200-CMPR-NEW-SLTS-PREV-SLTS.                     ELSGVLM1
00508                                                                   ELSGVLM1
00509 ************************************************************      ELSGVLM1
00510 *                                                          *      ELSGVLM1
00511 *        SEND PRIMARY SCREEN                               *      ELSGVLM1
00512 *                                                          *      ELSGVLM1
00513 ************************************************************      ELSGVLM1
00514  1300-SEND-PRIMARY-SCREEN.                                        ELSGVLM1
00515      PERFORM 1400-PREP-MAP-FOR-INIT-CALL.                         ELSGVLM1
00516      MOVE WS-DATE-OUT TO DATE1O.                                  ELSGVLM1
00517      MOVE WS-TIME-OUT TO TIMEO.                                   ELSGVLM1
00518      SET SSB-PRIMARY-SCREEN-OUT (SSB-SELECTOR-STATE) TO TRUE.     ELSGVLM1
00519      EXEC CICS SEND MAP ('EL04MAP')                               ELSGVLM1
00520                     MAPSET ('EL04SET')                            ELSGVLM1
00521                     FROM (EL04MAPO)                               ELSGVLM1
00522                     ERASE                                         ELSGVLM1
00523                     CURSOR                                        ELSGVLM1
00524      END-EXEC.                                                    ELSGVLM1
00525                                                                   ELSGVLM1
00526 ************************************************************      ELSGVLM1
00527 *                                                          *      ELSGVLM1
00528 *        PREPARE MAP FOR INITIAL CALL                      *      ELSGVLM1
00529 *                                                          *      ELSGVLM1
00530 ************************************************************      ELSGVLM1
00531  1400-PREP-MAP-FOR-INIT-CALL.                                     ELSGVLM1
00532      MOVE LOW-VALUES TO EL04MAPO.                                 ELSGVLM1
00533      MOVE -1         TO NAMEL.                                    ELSGVLM1
00534      MOVE SSB-GRP-NO TO GRPNMO.                                   ELSGVLM1
00535      MOVE SSB-SECTN-NO TO SCTNO.                                  ELSGVLM1
00536      PERFORM 3300-GET-FROM-DATE.                                  ELSGVLM1
00537      PERFORM 3400-GET-TO-DATE.                                    ELSGVLM1
00538      MOVE WS-GROUP-FROM-DATE TO FRMDTO.                           ELSGVLM1
00539      MOVE WS-GROUP-TO-DATE   TO TODTO.                            ELSGVLM1
00540      IF SSB-FR-UNDEF                                              ELSGVLM1
00541         MOVE 'ALL FAMILY MEMBERS' TO FRLVLO                       ELSGVLM1
00542      ELSE                                                         ELSGVLM1
00543         IF SSB-MEMBER                                             ELSGVLM1
00544            MOVE 'MEMBER' TO FRLVLO                                ELSGVLM1
00545         ELSE                                                      ELSGVLM1
00546            IF SSB-SPOUSE                                          ELSGVLM1
00547               MOVE 'SPOUSE' TO FRLVLO                             ELSGVLM1
00548            ELSE                                                   ELSGVLM1
00549               IF SSB-DEPENDENT                                    ELSGVLM1
00550                  MOVE 'DEPENDENT' TO FRLVLO.                      ELSGVLM1
00551                                                                   ELSGVLM1
00552 ************************************************************      ELSGVLM1
00553 *                                                          *      ELSGVLM1
00554 *        HANDLE PRIMARY SECONDARY STATE                    *      ELSGVLM1
00555 *                                                          *      ELSGVLM1
00556 ************************************************************      ELSGVLM1
00557  1500-HANDLE-PRIM-SEC-ST.                                         ELSGVLM1
00558      EXEC CICS IGNORE CONDITION                                   ELSGVLM1
00559                MAPFAIL                                            ELSGVLM1
00560      END-EXEC.                                                    ELSGVLM1
00561      EXEC CICS RECEIVE MAP ('EL04MAP')                            ELSGVLM1
00562                        MAPSET ('EL04SET')                         ELSGVLM1
00563                        INTO (EL04MAPI)                            ELSGVLM1
00564      END-EXEC.                                                    ELSGVLM1
00565      EXEC CICS HANDLE CONDITION                                   ELSGVLM1
00566                MAPFAIL                                            ELSGVLM1
00567      END-EXEC.                                                    ELSGVLM1
00568      IF EIBRCODE NOT EQUAL LOW-VALUES                             ELSGVLM1
00569         SET CIA-AB-MAPFAIL TO TRUE                                ELSGVLM1
00570         EXEC CICS ABEND                                           ELSGVLM1
00571                   ABCODE (CIA-ABCODE)                             ELSGVLM1
00572         END-EXEC.                                                 ELSGVLM1
00573      PERFORM 1600-VERIFY-INPUT-DATA.                              ELSGVLM1
00574                                                                   ELSGVLM1
00575 ************************************************************      ELSGVLM1
00576 *                                                          *      ELSGVLM1
00577 *        VERIFY INPUT DATA                                 *      ELSGVLM1
00578 *                                                          *      ELSGVLM1
00579 ************************************************************      ELSGVLM1
00580  1600-VERIFY-INPUT-DATA.                                          ELSGVLM1
00581      MOVE +0 TO WS-INPUT-FIELDS-FOUND.                            ELSGVLM1
00582      IF NAMEL > 0                                                 ELSGVLM1
00583         PERFORM 1700-VERIFY-NAME-FIELD.                           ELSGVLM1
00584      IF NMBRL > 0                                                 ELSGVLM1
00585         PERFORM 1800-VERIFY-NUMB-FIELD.                           ELSGVLM1
00586      IF LISTL > 0                                                 ELSGVLM1
00587         PERFORM 1900-VERIFY-LIST-FIELD.                           ELSGVLM1
00588      IF WS-INPUT-FIELDS-FOUND = 0                                 ELSGVLM1
00589         PERFORM 2000-SEND-NO-INPUT-DET-MSG                        ELSGVLM1
00590      ELSE                                                         ELSGVLM1
00591         IF WS-INPUT-FIELDS-FOUND = 1                              ELSGVLM1
00592            PERFORM 2100-DETERMINE-SELECTION                       ELSGVLM1
00593         ELSE                                                      ELSGVLM1
00594            PERFORM 2500-SEND-ONE-SELECTION-ONLY.                  ELSGVLM1
00595                                                                   ELSGVLM1
00596 ************************************************************      ELSGVLM1
00597 *                                                          *      ELSGVLM1
00598 *        VERIFY NAME FIELD                                 *      ELSGVLM1
00599 *                                                          *      ELSGVLM1
00600 ************************************************************      ELSGVLM1
00601  1700-VERIFY-NAME-FIELD.                                          ELSGVLM1
00602      IF NAMEI = SPACES OR LOW-VALUES                              ELSGVLM1
00603         MOVE 0 TO NAMEL                                           ELSGVLM1
00604      ELSE                                                         ELSGVLM1
00605         ADD +1 TO WS-INPUT-FIELDS-FOUND.                          ELSGVLM1
00606                                                                   ELSGVLM1
00607 ************************************************************      ELSGVLM1
00608 *                                                          *      ELSGVLM1
00609 *        VERIFY NUMB FIELD                                 *      ELSGVLM1
00610 *                                                          *      ELSGVLM1
00611 ************************************************************      ELSGVLM1
00612  1800-VERIFY-NUMB-FIELD.                                          ELSGVLM1
00613      IF NMBRI = SPACES OR LOW-VALUES                              ELSGVLM1
00614         MOVE 0 TO NMBRL                                           ELSGVLM1
00615      ELSE                                                         ELSGVLM1
00616         ADD +1 TO WS-INPUT-FIELDS-FOUND.                          ELSGVLM1
00617                                                                   ELSGVLM1
00618 ************************************************************      ELSGVLM1
00619 *                                                          *      ELSGVLM1
00620 *        VERIFY LIST FIELD                                 *      ELSGVLM1
00621 *                                                          *      ELSGVLM1
00622 ************************************************************      ELSGVLM1
00623  1900-VERIFY-LIST-FIELD.                                          ELSGVLM1
00624      IF LISTI = SPACES OR LOW-VALUES                              ELSGVLM1
00625         MOVE 0 TO LISTL                                           ELSGVLM1
00626      ELSE                                                         ELSGVLM1
00627         ADD +1 TO WS-INPUT-FIELDS-FOUND.                          ELSGVLM1
00628                                                                   ELSGVLM1
00629 ************************************************************      ELSGVLM1
00630 *                                                          *      ELSGVLM1
00631 *        SEND NO INPUT DETECTED MESSAGE                    *      ELSGVLM1
00632 *                                                          *      ELSGVLM1
00633 ************************************************************      ELSGVLM1
00634  2000-SEND-NO-INPUT-DET-MSG.                                      ELSGVLM1
00635      MOVE WS-NO-INPUT-MSG TO ERRMSGO.                             ELSGVLM1
00636      MOVE -1 TO NAMEL.                                            ELSGVLM1
00637      SET SSB-SECONDARY-SCREEN-OUT (SSB-SELECTOR-STATE)            ELSGVLM1
00638       TO TRUE.                                                    ELSGVLM1
00639      PERFORM 4400-SEND-ERROR.                                     ELSGVLM1
00640                                                                   ELSGVLM1
00641 ************************************************************      ELSGVLM1
00642 *                                                          *      ELSGVLM1
00643 *        DETERMINE SELECTION                               *      ELSGVLM1
00644 *                                                          *      ELSGVLM1
00645 ************************************************************      ELSGVLM1
00646  2100-DETERMINE-SELECTION.                                        ELSGVLM1
00647      IF NAMEL > 0                                                 ELSGVLM1
00648         PERFORM 2200-SET-UP-FOR-PROV-NAME                         ELSGVLM1
00649         ELSE IF NMBRL > 0                                         ELSGVLM1
00650                 PERFORM 2300-SET-UP-FOR-PROV-NMBR                 ELSGVLM1
00651              ELSE IF LISTL > 0                                    ELSGVLM1
00652                      PERFORM 2600-SET-UP-FOR-LIST-OPTION          ELSGVLM1
00653                   ELSE PERFORM 2000-SEND-NO-INPUT-DET-MSG.        ELSGVLM1
00654                                                                   ELSGVLM1
00655 ************************************************************      ELSGVLM1
00656 *                                                          *      ELSGVLM1
00657 *        SET UP FOR PROVIDER NAME PROCESSING               *      ELSGVLM1
00658 *                                                          *      ELSGVLM1
00659 ************************************************************      ELSGVLM1
00660  2200-SET-UP-FOR-PROV-NAME.                                       ELSGVLM1
00661      MOVE 'N' TO SSB-MODIFIER-1.                                  ELSGVLM1
00662      MOVE NAMEO TO SSB-CS-RESPONSE (1).                           ELSGVLM1
00663      SET SSB-SEL-DATA-AVAIL (SSB-SELECTOR-STATE)                  ELSGVLM1
00664       TO TRUE.                                                    ELSGVLM1
00665                                                                   ELSGVLM1
00666 ************************************************************      ELSGVLM1
00667 *                                                          *      ELSGVLM1
00668 *        SET UP FOR PROVIDER NUMBER PROCESSING             *      ELSGVLM1
00669 *                                                          *      ELSGVLM1
00670 ************************************************************      ELSGVLM1
00671  2300-SET-UP-FOR-PROV-NMBR.                                       ELSGVLM1
00672      PERFORM 9000-PRVDR-NMBR-VRFCTN.                              ELSGVLM1
00673      IF PROVIDER-NUMBER-NOT-OK                                    ELSGVLM1
00674         PERFORM 2400-SEND-NO-SPACE-IN-PROV-NO                     ELSGVLM1
00675      ELSE                                                         ELSGVLM1
00676         IF WS-STRT-PSTN = ZERO                                    ELSGVLM1
00677              AND WS-END-PSTN = ZERO                               ELSGVLM1
00678            SET PROVIDER-NUMBER-NOT-OK TO TRUE                     ELSGVLM1
00679            PERFORM 2000-SEND-NO-INPUT-DET-MSG                     ELSGVLM1
00680         ELSE                                                      ELSGVLM1
00681            IF WS-END-PSTN = ZERO                                  ELSGVLM1
00682               MOVE 10 TO WS-END-PSTN                              ELSGVLM1
00683            END-IF                                                 ELSGVLM1
00684         END-IF                                                    ELSGVLM1
00685      END-IF.                                                      ELSGVLM1
00686      IF PROVIDER-NUMBER-OK                                        ELSGVLM1
00687         MOVE 'D' TO SSB-MODIFIER-1                                ELSGVLM1
00688 *       MOVE NMBRO TO SSB-CS-RESPONSE(1)                          ELSGVLM1
00689         PERFORM 9500-PRVDR-NMBR-NRMLZTN                           ELSGVLM1
00690         SET SSB-SEL-DATA-AVAIL (SSB-SELECTOR-STATE) TO TRUE.      ELSGVLM1
00691                                                                   ELSGVLM1
00692 ************************************************************      ELSGVLM1
00693 *                                                          *      ELSGVLM1
00694 *        SEND NO SPACE IN PROVIDER NUMBER MESSAGE          *      ELSGVLM1
00695 *                                                          *      ELSGVLM1
00696 ************************************************************      ELSGVLM1
00697  2400-SEND-NO-SPACE-IN-PROV-NO.                                   ELSGVLM1
00698      SET PROVIDER-NUMBER-NOT-OK TO TRUE.                          ELSGVLM1
00699      MOVE WS-INVALID-PROV-NUM TO ERRMSGO.                         ELSGVLM1
00700      MOVE -1 TO NMBRL.                                            ELSGVLM1
00701      SET SSB-SECONDARY-SCREEN-OUT (SSB-SELECTOR-STATE)            ELSGVLM1
00702       TO TRUE.                                                    ELSGVLM1
00703      PERFORM 4400-SEND-ERROR.                                     ELSGVLM1
00704                                                                   ELSGVLM1
00705 ************************************************************      ELSGVLM1
00706 *                                                          *      ELSGVLM1
00707 *        SEND OUT ONE SELECTION ONLY MESSAGE               *      ELSGVLM1
00708 *                                                          *      ELSGVLM1
00709 ************************************************************      ELSGVLM1
00710  2500-SEND-ONE-SELECTION-ONLY.                                    ELSGVLM1
00711      MOVE WS-ONE-SELECTION-ONLY-MSG TO ERRMSGO.                   ELSGVLM1
00712      MOVE -1 TO NAMEL.                                            ELSGVLM1
00713      SET SSB-SECONDARY-SCREEN-OUT (SSB-SELECTOR-STATE)            ELSGVLM1
00714       TO TRUE.                                                    ELSGVLM1
00715      PERFORM 4400-SEND-ERROR.                                     ELSGVLM1
00716                                                                   ELSGVLM1
00717 ************************************************************      ELSGVLM1
00718 *                                                          *      ELSGVLM1
00719 *        SET UP FOR LIST OPTION PROCESSING                 *      ELSGVLM1
00720 *                                                          *      ELSGVLM1
00721 ************************************************************      ELSGVLM1
00722  2600-SET-UP-FOR-LIST-OPTION.                                     ELSGVLM1
00723      MOVE 'L' TO SSB-MODIFIER-1.                                  ELSGVLM1
00724      MOVE LISTO TO SSB-CS-RESPONSE (1).                           ELSGVLM1
00725      SET SSB-SEL-DATA-AVAIL (SSB-SELECTOR-STATE)                  ELSGVLM1
00726       TO TRUE.                                                    ELSGVLM1
00727                                                                   ELSGVLM1
00728 ************************************************************      ELSGVLM1
00729 *                                                          *      ELSGVLM1
00730 *        SEND NO GVL MESSAGE                               *      ELSGVLM1
00731 *                                                          *      ELSGVLM1
00732 ************************************************************      ELSGVLM1
00733  2700-SEND-NO-GVL.                                                ELSGVLM1
00734      IF SSB-PROV-CLASS-BOTH                                       ELSGVLM1
00735         PERFORM 9999-SEND-SIMPLE-NO-GVL                           ELSGVLM1
00736      ELSE IF SSB-PROV-CLASS-INST                                  ELSGVLM1
00737              PERFORM 9999-SEND-NO-INST-GVL                        ELSGVLM1
00738           ELSE IF SSB-PROV-CLASS-PROF                             ELSGVLM1
00739                   PERFORM 9999-SEND-NO-PROF-GVL.                  ELSGVLM1
00740                                                                   ELSGVLM1
00741  9999-SEND-SIMPLE-NO-GVL.                                         ELSGVLM1
00742      MOVE WS-SPC-MNU-TITLE TO SSB-MNU-TITLE.                      ELSGVLM1
00743      PERFORM 2800-DELETE-MENU-FILE.                               ELSGVLM1
00744      PERFORM 2900-PREP-AREA-FOR-MESSAGE.                          ELSGVLM1
00745      PERFORM 3100-ACQR-HDNG-STG-AREA.                             ELSGVLM1
00746      PERFORM 3200-GET-DESCR-LINE-AREA.                            ELSGVLM1
00747      PERFORM 3300-GET-FROM-DATE.                                  ELSGVLM1
00748      PERFORM 3400-GET-TO-DATE.                                    ELSGVLM1
00749      MOVE 0 TO MHD-NBR-HDG-LINES.                                 ELSGVLM1
00750      MOVE SPACES TO MHD-HDG-LINES.                                ELSGVLM1
00751      SET MHD-IDX TO 1.                                            ELSGVLM1
00752      MOVE WS-NO-GVL-AVAILABLE-MSG-1 TO                            ELSGVLM1
00753           MHD-HDG-LINE (MHD-IDX).                                 ELSGVLM1
00754      ADD 1 TO MHD-NBR-HDG-LINES.                                  ELSGVLM1
00755      SET MHD-IDX UP BY 1                                          ELSGVLM1
00756      MOVE WS-NO-GVL-AVAILABLE-MSG-2 TO                            ELSGVLM1
00757           MHD-HDG-LINE (MHD-IDX).                                 ELSGVLM1
00758      ADD 1 TO MHD-NBR-HDG-LINES.                                  ELSGVLM1
00759      SET SSB-START-MENU (SSB-SELECTOR-STATE)                      ELSGVLM1
00760           TO TRUE.                                                ELSGVLM1
00761                                                                   ELSGVLM1
00762  9999-SEND-NO-INST-GVL.                                           ELSGVLM1
00763      MOVE WS-SPC-MNU-TITLE TO SSB-MNU-TITLE.                      ELSGVLM1
00764      PERFORM 2800-DELETE-MENU-FILE.                               ELSGVLM1
00765      PERFORM 2900-PREP-AREA-FOR-MESSAGE.                          ELSGVLM1
00766      PERFORM 3100-ACQR-HDNG-STG-AREA.                             ELSGVLM1
00767      PERFORM 3200-GET-DESCR-LINE-AREA.                            ELSGVLM1
00768      PERFORM 3300-GET-FROM-DATE.                                  ELSGVLM1
00769      PERFORM 3400-GET-TO-DATE.                                    ELSGVLM1
00770      MOVE 0 TO MHD-NBR-HDG-LINES.                                 ELSGVLM1
00771      MOVE SPACES TO MHD-HDG-LINES.                                ELSGVLM1
00772      SET MHD-IDX TO 1.                                            ELSGVLM1
00773      MOVE WS-NO-INST-GVL-MSG TO                                   ELSGVLM1
00774           MHD-HDG-LINE (MHD-IDX).                                 ELSGVLM1
00775      ADD 1 TO MHD-NBR-HDG-LINES.                                  ELSGVLM1
00776      SET MHD-IDX UP BY 1                                          ELSGVLM1
00777      MOVE WS-NO-GVL-AVAILABLE-MSG-2 TO                            ELSGVLM1
00778           MHD-HDG-LINE (MHD-IDX).                                 ELSGVLM1
00779      ADD 1 TO MHD-NBR-HDG-LINES.                                  ELSGVLM1
00780      SET SSB-START-MENU (SSB-SELECTOR-STATE)                      ELSGVLM1
00781           TO TRUE.                                                ELSGVLM1
00782                                                                   ELSGVLM1
00783  9999-SEND-NO-PROF-GVL.                                           ELSGVLM1
00784      MOVE WS-SPC-MNU-TITLE TO SSB-MNU-TITLE.                      ELSGVLM1
00785      PERFORM 2800-DELETE-MENU-FILE.                               ELSGVLM1
00786      PERFORM 2900-PREP-AREA-FOR-MESSAGE.                          ELSGVLM1
00787      PERFORM 3100-ACQR-HDNG-STG-AREA.                             ELSGVLM1
00788      PERFORM 3200-GET-DESCR-LINE-AREA.                            ELSGVLM1
00789      PERFORM 3300-GET-FROM-DATE.                                  ELSGVLM1
00790      PERFORM 3400-GET-TO-DATE.                                    ELSGVLM1
00791      MOVE 0 TO MHD-NBR-HDG-LINES.                                 ELSGVLM1
00792      MOVE SPACES TO MHD-HDG-LINES.                                ELSGVLM1
00793      SET MHD-IDX TO 1.                                            ELSGVLM1
00794      MOVE WS-NO-PROF-GVL-MSG TO                                   ELSGVLM1
00795           MHD-HDG-LINE (MHD-IDX).                                 ELSGVLM1
00796      ADD 1 TO MHD-NBR-HDG-LINES.                                  ELSGVLM1
00797      SET MHD-IDX UP BY 1                                          ELSGVLM1
00798      MOVE WS-NO-GVL-AVAILABLE-MSG-2 TO                            ELSGVLM1
00799           MHD-HDG-LINE (MHD-IDX).                                 ELSGVLM1
00800      ADD 1 TO MHD-NBR-HDG-LINES.                                  ELSGVLM1
00801      SET SSB-START-MENU (SSB-SELECTOR-STATE)                      ELSGVLM1
00802           TO TRUE.                                                ELSGVLM1
00803                                                                   ELSGVLM1
00804 ************************************************************      ELSGVLM1
00805 *                                                          *      ELSGVLM1
00806 *        DELETE MENU FILE                                  *      ELSGVLM1
00807 *                                                          *      ELSGVLM1
00808 ************************************************************      ELSGVLM1
00809  2800-DELETE-MENU-FILE.                                           ELSGVLM1
00810      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGVLM1
00811      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGVLM1
00812                            ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.ELSGVLM1
00813      IF CIA-RC-PTR-NULL                                           ELSGVLM1
00814         PERFORM 3000-ACQUIRE-MENU-STG-AREA.                       ELSGVLM1
00815      SET CIA-ELSMENU-DDN                                          ELSGVLM1
00816          IOP-DEL                                                  ELSGVLM1
00817          IOP-FCQ-NONE                                             ELSGVLM1
00818          IOP-KVQ-NONE TO TRUE.                                    ELSGVLM1
00819      CALL 'ELUIOPGM' USING DFHEIBLK                               ELSGVLM1
00820                            DFHCOMMAREA.                           ELSGVLM1
00821                                                                   ELSGVLM1
00822 ************************************************************      ELSGVLM1
00823 *                                                          *      ELSGVLM1
00824 *        PREPARE AREA FOR MESSAGE                          *      ELSGVLM1
00825 *                                                          *      ELSGVLM1
00826 ************************************************************      ELSGVLM1
00827  2900-PREP-AREA-FOR-MESSAGE.                                      ELSGVLM1
00828      INITIALIZE SSB-MNU-CHOICE (1).                               ELSGVLM1
00829      MOVE 0              TO MSO-NBR-MENU-OPTS.                    ELSGVLM1
00830      MOVE 1              TO MSO-MIN-CHOICES                       ELSGVLM1
00831                             MSO-MAX-CHOICES.                      ELSGVLM1
00832      MOVE LENGTH OF WS-NO-GVL-AVAILABLE-MSG                       ELSGVLM1
00833                          TO MSO-OPT-LEN.                          ELSGVLM1
00834      SET MSO-OPT-TYP-AN  TO TRUE.                                 ELSGVLM1
00835      SET MSO-IDX         TO 1.                                    ELSGVLM1
00836      SET IOP-ADD         TO TRUE.                                 ELSGVLM1
00837      SET IOP-FCQ-NONE    TO TRUE.                                 ELSGVLM1
00838      SET IOP-KVQ-NONE    TO TRUE.                                 ELSGVLM1
00839      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGVLM1
00840                                                                   ELSGVLM1
00841 ************************************************************      ELSGVLM1
00842 *                                                          *      ELSGVLM1
00843 *        ACQUIRE MENU STORAGE AREA                         *      ELSGVLM1
00844 *                                                          *      ELSGVLM1
00845 ************************************************************      ELSGVLM1
00846  3000-ACQUIRE-MENU-STG-AREA.                                      ELSGVLM1
00847      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGVLM1
00848      SET CIA-STG-GETMAIN TO TRUE.                                 ELSGVLM1
00849      PERFORM 4300-CALL-STORAGE-MANAGER.                           ELSGVLM1
00850      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGVLM1
00851      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGVLM1
00852                      ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.      ELSGVLM1
00853                                                                   ELSGVLM1
00854 ************************************************************      ELSGVLM1
00855 *                                                          *      ELSGVLM1
00856 *        ACQUIRE HEADING STORAGE AREA                      *      ELSGVLM1
00857 *                                                          *      ELSGVLM1
00858 ************************************************************      ELSGVLM1
00859  3100-ACQR-HDNG-STG-AREA.                                         ELSGVLM1
00860      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSGVLM1
00861      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGVLM1
00862                            ADDRESS OF MHD-MENU-HEADINGS.          ELSGVLM1
00863      COMPUTE CIA-AREA-LEN = LENGTH OF MHD-NBR-HDG-LINES +         ELSGVLM1
00864             (LENGTH OF MHD-HDG-LINE * CIA-MVO).                   ELSGVLM1
00865      SET CIA-STG-GETMAIN TO TRUE.                                 ELSGVLM1
00866      PERFORM 4300-CALL-STORAGE-MANAGER.                           ELSGVLM1
00867      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSGVLM1
00868      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGVLM1
00869                            ADDRESS OF MHD-MENU-HEADINGS.          ELSGVLM1
00870                                                                   ELSGVLM1
00871 ************************************************************      ELSGVLM1
00872 *                                                          *      ELSGVLM1
00873 *        GET DESCRIPTION LINE AREA                         *      ELSGVLM1
00874 *                                                          *      ELSGVLM1
00875 ************************************************************      ELSGVLM1
00876  3200-GET-DESCR-LINE-AREA.                                        ELSGVLM1
00877      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGVLM1
00878      SET CIA-STG-GETMAIN TO TRUE.                                 ELSGVLM1
00879      SET IOP-GETMAIN-REC TO TRUE.                                 ELSGVLM1
00880      COMPUTE IOP-REC-LEN = LENGTH OF MSD-NBR-DESCR-LINES +        ELSGVLM1
00881             (LENGTH OF MSD-DESCR-LINE * 1).                       ELSGVLM1
00882      PERFORM 4300-CALL-STORAGE-MANAGER.                           ELSGVLM1
00883      SET ADDRESS OF MSD-MENU-ITEM-DESCRIPTIONS                    ELSGVLM1
00884       TO IOP-REC-PTR.                                             ELSGVLM1
00885                                                                   ELSGVLM1
00886 ************************************************************      ELSGVLM1
00887 *                                                          *      ELSGVLM1
00888 *        GET FROM DATE                                     *      ELSGVLM1
00889 *                                                          *      ELSGVLM1
00890 ************************************************************      ELSGVLM1
00891  3300-GET-FROM-DATE.                                              ELSGVLM1
00892      MOVE SSB-SRV-FROM-DATE    TO HGADATE-JULIAN1.                ELSGVLM1
00893      PERFORM 0850-CNVRT-DTE-FROM-JUL-TO-GR.                       ELSGVLM1
00894      MOVE HGADATE-DATE2        TO WS-GROUP-FROM-DATE.             ELSGVLM1
00895                                                                   ELSGVLM1
00896 ************************************************************      ELSGVLM1
00897 *                                                          *      ELSGVLM1
00898 *        GET TO DATE                                       *      ELSGVLM1
00899 *                                                          *      ELSGVLM1
00900 ************************************************************      ELSGVLM1
00901  3400-GET-TO-DATE.                                                ELSGVLM1
00902      MOVE SSB-SRV-TO-DATE      TO HGADATE-JULIAN1.                ELSGVLM1
00903      PERFORM 0850-CNVRT-DTE-FROM-JUL-TO-GR.                       ELSGVLM1
00904      MOVE HGADATE-DATE2        TO WS-GROUP-TO-DATE.               ELSGVLM1
00905                                                                   ELSGVLM1
00906 ************************************************************      ELSGVLM1
00907 *                                                          *      ELSGVLM1
00908 *        GET GROUP SPECIFIC RECORD                         *      ELSGVLM1
00909 *                                                          *      ELSGVLM1
00910 ************************************************************      ELSGVLM1
00911  3500-GET-GROUP-RECORD.                                           ELSGVLM1
00912      INITIALIZE WS-SELECTED-KEYS-COUNTER.                         ELSGVLM1
00913      PERFORM WITH TEST BEFORE                                     ELSGVLM1
00914              VARYING KTG-IDX FROM 1 BY 1                          ELSGVLM1
00915                UNTIL KTG-IDX > KTG-NBR-KEYS                       ELSGVLM1
00916                 IF KTG-SEL (KTG-IDX)                              ELSGVLM1
00917                    PERFORM 3525-EVALUATE-TX-IL-ACTION             ELSGVLM1
00918                    IF WS-SELECTED-KEYS-COUNTER > 1                ELSGVLM1
00919                       SET CIA-AB-CRITIO TO TRUE                   ELSGVLM1
00920                       EXEC CICS ABEND                             ELSGVLM1
00921                                 ABCODE (CIA-ABCODE)               ELSGVLM1
00922                       END-EXEC                                    ELSGVLM1
00923                    END-IF                                         ELSGVLM1
00924                 END-IF                                            ELSGVLM1
00925      END-PERFORM.                                                 ELSGVLM1
00926      SET KTG-IDX TO WS-SAVE-KTG-IDX                               ELSGVLM1
00927      IF WS-SELECTED-KEYS-COUNTER = 1                              ELSGVLM1
00928         PERFORM 3600-READ-GROUP-RECORD                            ELSGVLM1
00929      ELSE                                                         ELSGVLM1
00930         SET CIA-AB-PGM-LOGIC TO TRUE                              ELSGVLM1
00931         EXEC CICS ABEND                                           ELSGVLM1
00932                   ABCODE (CIA-ABCODE)                             ELSGVLM1
00933         END-EXEC.                                                 ELSGVLM1
00934                                                                   ELSGVLM1
00935 ************************************************************      ELSGVLM1
00936 *                                                          *      ELSGVLM1
00937 *        EVALUATE TX IL ACTION                             *      ELSGVLM1
00938 *                                                          *      ELSGVLM1
00939 ************************************************************      ELSGVLM1
00940  3525-EVALUATE-TX-IL-ACTION.                                      ELSGVLM1
00941      EVALUATE TRUE                                                ELSGVLM1
00942         WHEN TEXAS-REGION AND KTG-PKG-CODE (KTG-IDX)              ELSGVLM1
00943                = SSB-PKG-CODE                                     ELSGVLM1
00944           ADD 1 TO WS-SELECTED-KEYS-COUNTER                       ELSGVLM1
00945           SET WS-SAVE-KTG-IDX TO KTG-IDX                          ELSGVLM1
00946         WHEN NOT TEXAS-REGION                                     ELSGVLM1
00947           ADD 1 TO WS-SELECTED-KEYS-COUNTER                       ELSGVLM1
00948           SET WS-SAVE-KTG-IDX TO KTG-IDX                          ELSGVLM1
00949      END-EVALUATE.                                                ELSGVLM1
00950                                                                   ELSGVLM1
00951 ************************************************************      ELSGVLM1
00952 *                                                          *      ELSGVLM1
00953 *        READ GROUP SPECIFIC RECORD                        *      ELSGVLM1
00954 *                                                          *      ELSGVLM1
00955 ************************************************************      ELSGVLM1
00956  3600-READ-GROUP-RECORD.                                          ELSGVLM1
00957      PERFORM 3700-INITIALIZE-READ.                                ELSGVLM1
00958      PERFORM 3800-MOVE-GROUP-KEY.                                 ELSGVLM1
00959      PERFORM 3900-READ-GR-SPEC-IO-MODULE.                         ELSGVLM1
00960                                                                   ELSGVLM1
00961 ************************************************************      ELSGVLM1
00962 *                                                          *      ELSGVLM1
00963 *        INITIALIZE READ                                   *      ELSGVLM1
00964 *                                                          *      ELSGVLM1
00965 ************************************************************      ELSGVLM1
00966  3700-INITIALIZE-READ.                                            ELSGVLM1
00967      SET CIA-GCGRPSPC-DDN TO TRUE.                                ELSGVLM1
00968      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGVLM1
00969          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSGVLM1
00970      IF CIA-RC-PTR-NULL                                           ELSGVLM1
00971         SET CIA-GCGRPSPC-DDN TO TRUE                              ELSGVLM1
00972         SET CIA-STG-GETMAIN  TO TRUE                              ELSGVLM1
00973         PERFORM 4300-CALL-STORAGE-MANAGER.                        ELSGVLM1
00974      SET CIA-GCGRPSPC-DDN TO TRUE.                                ELSGVLM1
00975      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGVLM1
00976          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSGVLM1
00977                                                                   ELSGVLM1
00978 ************************************************************      ELSGVLM1
00979 *                                                          *      ELSGVLM1
00980 *        MOVE GROUP SPECIFIC KEY                           *      ELSGVLM1
00981 *                                                          *      ELSGVLM1
00982 ************************************************************      ELSGVLM1
00983  3800-MOVE-GROUP-KEY.                                             ELSGVLM1
00984      MOVE SSB-PLAN-CODE             TO KWA-GCG-PLAN-CODE.         ELSGVLM1
00985      MOVE SSB-GROUP-NUMBER     TO KWA-GCG-GROUP-NUMBER.           ELSGVLM1
00986      MOVE SSB-SECTN-NO         TO KWA-GCG-SECTION-NUMBER.         ELSGVLM1
00987      MOVE SSB-PKG-CODE      TO KWA-GCG-PKG-CODE.                  ELSGVLM1
00988      MOVE KTG-FAM-REL-LVL (KTG-IDX) TO KWA-GCG-FAM-REL-LVL.       ELSGVLM1
00989      MOVE KTG-EFF-DT-CENTURY (KTG-IDX) TO                         ELSGVLM1
00990                                    KWA-GCG-EFF-DATE-CENTURY.      ELSGVLM1
00991                                                                   ELSGVLM1
00992 ************************************************************      ELSGVLM1
00993 *                                                          *      ELSGVLM1
00994 *        READ GROUP SPECIFIC IO MODULE                     *      ELSGVLM1
00995 *                                                          *      ELSGVLM1
00996 ************************************************************      ELSGVLM1
00997  3900-READ-GR-SPEC-IO-MODULE.                                     ELSGVLM1
00998      SET  CIA-GCGRPSPC-DDN TO TRUE.                               ELSGVLM1
00999      MOVE KWA-GCGRPSPC-KEY TO IOP-FILE-KEY.                       ELSGVLM1
01000      SET IOP-REC-PTR       TO NULL.                               ELSGVLM1
01001      SET IOP-RD            TO TRUE.                               ELSGVLM1
01002      SET IOP-FCQ-NONE      TO TRUE.                               ELSGVLM1
01003      SET IOP-KVQ-EQ        TO TRUE.                               ELSGVLM1
01004      SET IOP-STG-MODE-MOVE TO TRUE.                               ELSGVLM1
01005      CALL 'ELUIOPGM' USING DFHEIBLK                               ELSGVLM1
01006                            DFHCOMMAREA                            ELSGVLM1
01007      END-CALL.                                                    ELSGVLM1
01008      IF NOT IOP-RC-OK                                             ELSGVLM1
01009         SET CIA-AB-NOTFND-GCGRPSPC TO TRUE                        ELSGVLM1
01010         EXEC CICS ABEND                                           ELSGVLM1
01011                   ABCODE (CIA-ABCODE)                             ELSGVLM1
01012         END-EXEC                                                  ELSGVLM1
01013      ELSE                                                         ELSGVLM1
01014         SET ADDRESS OF GCG-GRP-SPEC-RECORD-AREA                   ELSGVLM1
01015          TO IOP-REC-PTR.                                          ELSGVLM1
01016                                                                   ELSGVLM1
01017 /***********************************************************      ELSGVLM1
01018 *                                                          *      ELSGVLM1
01019 *        SEARCH GROUP SPECIFIC FOR GVLF, GVLG OR GVLH      *      ELSGVLM1
01020 *                                                          *      ELSGVLM1
01021 ************************************************************      ELSGVLM1
01022  4000-SEARCH-FOR-GVLFGH.                                          ELSGVLM1
01023      PERFORM WITH TEST BEFORE                                     ELSGVLM1
01024         VARYING GCG-INDEX FROM 1 BY 1                             ELSGVLM1
01025         UNTIL GCG-INDEX = WS-MAX-GCG-IDX                          ELSGVLM1
01026               OR GCG-TAB-ID (GCG-INDEX) > '#GVLH '                ELSGVLM1
01027 *    -- DETERMINE IF INSTITUTIONAL GVL FOUND                      ELSGVLM1
01028         IF GCG-TAB-SLOT-NO (GCG-INDEX) > ZERO                     ELSGVLM1
01029            EVALUATE TRUE                                          ELSGVLM1
01030                WHEN GCG-TAB-ID (GCG-INDEX) = '#GVLF '             ELSGVLM1
01031                     MOVE GCG-TAB-ID (GCG-INDEX) TO WS-GVLF-ID     ELSGVLM1
01032                     MOVE GCG-TAB-SLOT-NO (GCG-INDEX)              ELSGVLM1
01033                       TO WS-GVLF-SLOT-NO                          ELSGVLM1
01034                     SET VALID-GVL-FOUND TO TRUE                   ELSGVLM1
01035                                                                   ELSGVLM1
01036                WHEN GCG-TAB-ID (GCG-INDEX) = '#GVLG '             ELSGVLM1
01037                     MOVE GCG-TAB-ID (GCG-INDEX) TO WS-GVLG-ID     ELSGVLM1
01038                     MOVE GCG-TAB-SLOT-NO (GCG-INDEX)              ELSGVLM1
01039                       TO WS-GVLG-SLOT-NO                          ELSGVLM1
01040                     SET VALID-GVL-FOUND TO TRUE                   ELSGVLM1
01041                                                                   ELSGVLM1
01042                WHEN GCG-TAB-ID (GCG-INDEX) = '#GVLH '             ELSGVLM1
01043                     MOVE GCG-TAB-ID (GCG-INDEX) TO WS-GVLH-ID     ELSGVLM1
01044                     MOVE GCG-TAB-SLOT-NO (GCG-INDEX)              ELSGVLM1
01045                       TO WS-GVLH-SLOT-NO                          ELSGVLM1
01046                     SET VALID-GVL-FOUND TO TRUE                   ELSGVLM1
01047            END-EVALUATE                                           ELSGVLM1
01048         END-IF                                                    ELSGVLM1
01049      END-PERFORM.                                                 ELSGVLM1
01050 *    IF NO-VALID-GVL-FOUND                                        ELSGVLM1
01051 *       PERFORM 9999-SEND-NO-INST-GVL-MSG.                        ELSGVLM1
01052                                                                   ELSGVLM1
01053 ************************************************************      ELSGVLM1
01054 *                                                          *      ELSGVLM1
01055 *        SEARCH GROUP SPECIFIC FOR GVLP, GVLQ OR GVLR      *      ELSGVLM1
01056 *                                                          *      ELSGVLM1
01057 ************************************************************      ELSGVLM1
01058  4100-SEARCH-FOR-GVLPQR.                                          ELSGVLM1
01059      PERFORM WITH TEST BEFORE                                     ELSGVLM1
01060         VARYING GCG-INDEX FROM 1 BY 1                             ELSGVLM1
01061         UNTIL GCG-INDEX = WS-MAX-GCG-IDX                          ELSGVLM1
01062               OR GCG-TAB-ID (GCG-INDEX) > '#GVLR '                ELSGVLM1
01063 *    -- DETERMINE IF GCG FOUND                                    ELSGVLM1
01064         IF GCG-TAB-SLOT-NO (GCG-INDEX) > ZERO                     ELSGVLM1
01065            EVALUATE TRUE                                          ELSGVLM1
01066                WHEN GCG-TAB-ID (GCG-INDEX) = '#GVLP '             ELSGVLM1
01067                     MOVE GCG-TAB-ID (GCG-INDEX) TO WS-GVLP-ID     ELSGVLM1
01068                     MOVE GCG-TAB-SLOT-NO (GCG-INDEX)              ELSGVLM1
01069                       TO WS-GVLP-SLOT-NO                          ELSGVLM1
01070                     SET VALID-GVL-FOUND TO TRUE                   ELSGVLM1
01071                                                                   ELSGVLM1
01072                WHEN GCG-TAB-ID (GCG-INDEX) = '#GVLQ '             ELSGVLM1
01073                     MOVE GCG-TAB-ID (GCG-INDEX) TO WS-GVLQ-ID     ELSGVLM1
01074                     MOVE GCG-TAB-SLOT-NO (GCG-INDEX)              ELSGVLM1
01075                       TO WS-GVLQ-SLOT-NO                          ELSGVLM1
01076                     SET VALID-GVL-FOUND TO TRUE                   ELSGVLM1
01077                                                                   ELSGVLM1
01078                WHEN GCG-TAB-ID (GCG-INDEX) = '#GVLR '             ELSGVLM1
01079                     MOVE GCG-TAB-ID (GCG-INDEX) TO WS-GVLR-ID     ELSGVLM1
01080                     MOVE GCG-TAB-SLOT-NO (GCG-INDEX)              ELSGVLM1
01081                       TO WS-GVLR-SLOT-NO                          ELSGVLM1
01082                     SET VALID-GVL-FOUND TO TRUE                   ELSGVLM1
01083            END-EVALUATE                                           ELSGVLM1
01084         END-IF                                                    ELSGVLM1
01085      END-PERFORM.                                                 ELSGVLM1
01086 *    IF NO-VALID-GVL-FOUND                                        ELSGVLM1
01087 *       PERFORM 9999-SEND-NO-PROF-GVL-MSG.                        ELSGVLM1
01088                                                                   ELSGVLM1
01089 ************************************************************      ELSGVLM1
01090 *                                                          *      ELSGVLM1
01091 *        COMPARE NEW SLOT NUMBERS IN WORKING STORAGE       *      ELSGVLM1
01092 *        TO EXISTING SLOT NUMBERS IN SLN                   *      ELSGVLM1
01093 *                                                          *      ELSGVLM1
01094 ************************************************************      ELSGVLM1
01095  4200-CMPR-NEW-SLTS-PREV-SLTS.                                    ELSGVLM1
01096      IF SLN-GVL-WRK-FL-CRTD                                       ELSGVLM1
01097         IF WS-GVL-SLOT-NMBRS =                                    ELSGVLM1
01098            SLN-GVL-SLOT-NMBRS                                     ELSGVLM1
01099            CONTINUE                                               ELSGVLM1
01100         ELSE                                                      ELSGVLM1
01101            MOVE WS-GVL-SLOT-NMBRS                                 ELSGVLM1
01102              TO SLN-GVL-SLOT-NMBRS                                ELSGVLM1
01103            SET SLN-GVL-WRK-FL-NOT-CRTD TO TRUE                    ELSGVLM1
01104      ELSE                                                         ELSGVLM1
01105         MOVE WS-GVL-SLOT-NMBRS                                    ELSGVLM1
01106           TO SLN-GVL-SLOT-NMBRS                                   ELSGVLM1
01107         SET SLN-GVL-WRK-FL-NOT-CRTD TO TRUE.                      ELSGVLM1
01108      IF SLN-GVL-WRK-FL-NOT-CRTD                                   ELSGVLM1
01109         CALL 'ELUGVLSV' USING DFHEIBLK                            ELSGVLM1
01110                               DFHCOMMAREA.                        ELSGVLM1
01111                                                                   ELSGVLM1
01112 ************************************************************      ELSGVLM1
01113 *                                                          *      ELSGVLM1
01114 *        CALL STORAGE MANAGER                              *      ELSGVLM1
01115 *                                                          *      ELSGVLM1
01116 ************************************************************      ELSGVLM1
01117  4300-CALL-STORAGE-MANAGER.                                       ELSGVLM1
01118         CALL 'ELUSTGMG' USING DFHEIBLK                            ELSGVLM1
01119                               DFHCOMMAREA                         ELSGVLM1
01120         END-CALL.                                                 ELSGVLM1
01121                                                                   ELSGVLM1
01122 ************************************************************      ELSGVLM1
01123 *                                                          *      ELSGVLM1
01124 *        SEND ERROR MESSAGE                                *      ELSGVLM1
01125 *                                                          *      ELSGVLM1
01126 ************************************************************      ELSGVLM1
01127  4400-SEND-ERROR.                                                 ELSGVLM1
01128      MOVE WS-DATE-OUT TO DATE1O.                                  ELSGVLM1
01129      MOVE WS-TIME-OUT TO TIMEO.                                   ELSGVLM1
01130      MOVE DFHBMUNF TO NAMEA, NMBRA, LISTA.                        ELSGVLM1
01131      EXEC CICS SEND MAP ('EL00MAP')                               ELSGVLM1
01132                     MAPSET ('EL00SET')                            ELSGVLM1
01133                     CURSOR                                        ELSGVLM1
01134                     FROM (EL00MAPO)                               ELSGVLM1
01135      END-EXEC.                                                    ELSGVLM1
01136      EXEC CICS SEND MAP ('EL04MAP')                               ELSGVLM1
01137                     MAPSET ('EL04SET')                            ELSGVLM1
01138                     DATAONLY                                      ELSGVLM1
01139                     ALARM                                         ELSGVLM1
01140                     FROM (EL04MAPO)                               ELSGVLM1
01141      END-EXEC.                                                    ELSGVLM1
01142                                                                   ELSGVLM1
01143  9000-PRVDR-NMBR-VRFCTN.                                          ELSGVLM1
01144      INITIALIZE WS-NUMB-CHECK                                     ELSGVLM1
01145                 WS-STRT-PSTN                                      ELSGVLM1
01146                 WS-END-PSTN.                                      ELSGVLM1
01147      SET PROVIDER-NUMBER-OK TO TRUE.                              ELSGVLM1
01148      MOVE NMBRI TO WS-NUMB-CHECK.                                 ELSGVLM1
01149      MOVE NMBRL TO INPUTL.                                        ELSGVLM1
01150      PERFORM VARYING WS-CRNT-PSTN FROM 1 BY 1                     ELSGVLM1
01151          UNTIL WS-CRNT-PSTN > INPUTL OR PROVIDER-NUMBER-NOT-OK    ELSGVLM1
01152              IF WS-NUMB-CHAR(WS-CRNT-PSTN)                        ELSGVLM1
01153                                = SPACES OR LOW-VALUES             ELSGVLM1
01154                 PERFORM 9125-CHCK-FOR-LD-TRLNG-SPCS               ELSGVLM1
01155              ELSE                                                 ELSGVLM1
01156                 PERFORM 9150-CHCK-CHRCTR                          ELSGVLM1
01157               END-IF                                              ELSGVLM1
01158      END-PERFORM.                                                 ELSGVLM1
01159                                                                   ELSGVLM1
01160  9125-CHCK-FOR-LD-TRLNG-SPCS.                                     ELSGVLM1
01161      IF WS-STRT-PSTN = ZERO                                       ELSGVLM1
01162         CONTINUE                                                  ELSGVLM1
01163      ELSE                                                         ELSGVLM1
01164          IF WS-END-PSTN = ZERO                                    ELSGVLM1
01165             COMPUTE WS-END-PSTN = WS-CRNT-PSTN - 1                ELSGVLM1
01166          END-IF                                                   ELSGVLM1
01167      END-IF.                                                      ELSGVLM1
01168                                                                   ELSGVLM1
01169  9150-CHCK-CHRCTR.                                                ELSGVLM1
01170      IF WS-STRT-PSTN = ZERO                                       ELSGVLM1
01171         MOVE WS-CRNT-PSTN TO WS-STRT-PSTN                         ELSGVLM1
01172      ELSE                                                         ELSGVLM1
01173         IF WS-END-PSTN > ZERO                                     ELSGVLM1
01174             SET PROVIDER-NUMBER-NOT-OK TO TRUE                    ELSGVLM1
01175         ELSE                                                      ELSGVLM1
01176            CONTINUE                                               ELSGVLM1
01177         END-IF                                                    ELSGVLM1
01178      END-IF.                                                      ELSGVLM1
01179                                                                   ELSGVLM1
01180  9500-PRVDR-NMBR-NRMLZTN.                                         ELSGVLM1
01181      INITIALIZE WS-PRVDR-CHECK                                    ELSGVLM1
01182                 WS-END-PSTN.                                      ELSGVLM1
01183      MOVE NMBRI TO WS-PRVDR-CHECK.                                ELSGVLM1
01184      MOVE NMBRL TO INPUTL.                                        ELSGVLM1
01185      SET NO-NON-BLANK-CHRCTR-FND TO TRUE.                         ELSGVLM1
01186      PERFORM VARYING PRVDR-IDX FROM INPUTL                        ELSGVLM1
01187         BY -1 UNTIL NON-BLANK-CHRCTR-FND OR                       ELSGVLM1
01188            PRVDR-IDX = 0                                          ELSGVLM1
01189          IF WS-PRVDR-CHAR(PRVDR-IDX) NOT =                        ELSGVLM1
01190                   SPACE AND LOW-VALUE                             ELSGVLM1
01191              SET NON-BLANK-CHRCTR-FND TO TRUE                     ELSGVLM1
01192          END-IF                                                   ELSGVLM1
01193      END-PERFORM.                                                 ELSGVLM1
01194      IF NON-BLANK-CHRCTR-FND                                      ELSGVLM1
01195         SET PRVDR-IDX UP BY 1                                     ELSGVLM1
01196         SET WS-END-PSTN TO PRVDR-IDX                              ELSGVLM1
01197      ELSE                                                         ELSGVLM1
01198         SET PROVIDER-NUMBER-NOT-OK TO TRUE                        ELSGVLM1
01199      END-IF.                                                      ELSGVLM1
01200      IF PROVIDER-NUMBER-OK                                        ELSGVLM1
01201         PERFORM 9600-CHCK-FOR-LDNG-BLNKS                          ELSGVLM1
01202      END-IF.                                                      ELSGVLM1
01203      IF NON-BLANK-CHRCTR-FND                                      ELSGVLM1
01204         PERFORM 9700-FRMT-PRVDR-NMBR                              ELSGVLM1
01205      ELSE                                                         ELSGVLM1
01206         SET PROVIDER-NUMBER-NOT-OK TO TRUE                        ELSGVLM1
01207      END-IF.                                                      ELSGVLM1
01208                                                                   ELSGVLM1
01209  9600-CHCK-FOR-LDNG-BLNKS.                                        ELSGVLM1
01210      SET NO-NON-BLANK-CHRCTR-FND TO TRUE.                         ELSGVLM1
01211      PERFORM VARYING PRVDR-IDX FROM 1                             ELSGVLM1
01212         BY 1 UNTIL NON-BLANK-CHRCTR-FND OR                        ELSGVLM1
01213           PRVDR-IDX > WS-END-PSTN                                 ELSGVLM1
01214         IF WS-PRVDR-CHAR (PRVDR-IDX) NOT = SPACE AND LOW-VALUES   ELSGVLM1
01215           SET NON-BLANK-CHRCTR-FND TO TRUE                        ELSGVLM1
01216         END-IF                                                    ELSGVLM1
01217      END-PERFORM.                                                 ELSGVLM1
01218                                                                   ELSGVLM1
01219  9700-FRMT-PRVDR-NMBR.                                            ELSGVLM1
01220      MOVE ZEROES TO WS-TEMP-PRVDR-NMBR.                           ELSGVLM1
01221      COMPUTE INPUTL  = WS-END-PSTN - (WS-STRT-PSTN - 1).          ELSGVLM1
01222      COMPUTE WS-STRNG-STRT = 10 - INPUTL + 1.                     ELSGVLM1
01223      SET HOLD-IDX TO WS-STRNG-STRT.                               ELSGVLM1
01224      PERFORM VARYING NUMB-IDX FROM WS-STRT-PSTN BY 1              ELSGVLM1
01225           UNTIL NUMB-IDX > WS-END-PSTN                            ELSGVLM1
01226         MOVE WS-NUMB-CHAR (NUMB-IDX) TO                           ELSGVLM1
01227                     WS-HOLD-PRVDR-CHAR (HOLD-IDX)                 ELSGVLM1
01228         SET HOLD-IDX UP BY 1                                      ELSGVLM1
01229      END-PERFORM.                                                 ELSGVLM1
01230      MOVE WS-HOLD-PRVDR-NMBR TO SSB-CS-RESPONSE(1).               ELSGVLM1
