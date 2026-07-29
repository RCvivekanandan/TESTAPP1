00001  ID DIVISION.                                                     08/20/03
00002  PROGRAM-ID.     GBIEPGM.                                         GBIEPGM 
00003  AUTHOR.         JUNE PON.                                           LV001
00004 ***** THIS IS A COBOL/2 PROGRAM.                                  GBIEPGM 
00005  DATE-WRITTEN.   03/27/01.                                        GBIEPGM 
00006  DATE-COMPILED.                                                   GBIEPGM 
00007                                                                   GBIEPGM 
00008 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          GBIEPGM 
00009 *    *-*         U P D A T E   H I S T O R Y         *-*          GBIEPGM 
00010 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          GBIEPGM 
00011                                                                   GBIEPGM 
00012 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* GBIEPGM 
00013                                                                   GBIEPGM 
00014 *                                                                *GBIEPGM 
00015 ******************************************************************GBIEPGM 
00016 *                                                                *GBIEPGM 
00017 * GBIBPGM - BENEFITS HIGHLIGHTS GROUPSPC RECORD SELECTION SCREEN *GBIEPGM 
00018 * ============================================================== *GBIEPGM 
00019 *                                                                *GBIEPGM 
00020 *    03/27/01  JP   1. CODED INITIAL PROGRAM USING GIJ1PGM       *GBIEPGM 
00021 *                      AS A BASE.                                *GBIEPGM 
00022 *                                                                *GBIEPGM 
00023 *    06/05/01  GSP  1. CHANGED ALL REFERENCES OF GBIA TO         *GBIEPGM 
00024 *                      GHIL (TRANSACTION WAS RENAMED).           *GBIEPGM 
00025 *                                                                *GBIEPGM 
00026 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GBIEPGM 
00027 *                                                                *GBIEPGM 
00028 *                                                                *GBIEPGM 
00029 * ============================================================== *GBIEPGM 
00030 *                                                                *GBIEPGM 
00031 *   FUNC CODE: GBIE                                              *GBIEPGM 
00032 *                                                                *GBIEPGM 
00033 *   MAPSET:    GBIESETC                                          *GBIEPGM 
00034 *   MAPTABLE:  YES                                               *GBIEPGM 
00035 *                                                                *GBIEPGM 
00036 *   FILES:     GROUP SPECIFIC                                    *GBIEPGM 
00037 *                                                                *GBIEPGM 
00038 *   ABEND CODES:                                                 *GBIEPGM 
00039 *   ------------                                                 *GBIEPGM 
00040 *         'GJPA'    LOGIC ERROR WHEN PAGING BACKWARD.            *GBIEPGM 
00041 *                   DID NOT FIND ENTRY FROM THE SCREEN IN THE    *GBIEPGM 
00042 *                   DATE TABLE.                                  *GBIEPGM 
00043 *         'GJPB'    LOGIC ERROR WHEN PAGING FORWARD.             *GBIEPGM 
00044 *                   DID NOT FIND ENTRY FROM THE SCREEN IN THE    *GBIEPGM 
00045 *                   DATE TABLE.                                  *GBIEPGM 
00046 *                                                                *GBIEPGM 
00047 ******************************************************************GBIEPGM 
00048 /                                                                 GBIEPGM 
00049  ENVIRONMENT DIVISION.                                            GBIEPGM 
00050                                                                   GBIEPGM 
00051  DATA DIVISION.                                                   GBIEPGM 
00052  WORKING-STORAGE SECTION.                                         GBIEPGM 
00053  01  WS-BEGIN                    PIC X(24)  VALUE                 GBIEPGM 
00054      '***GBIEPGM WS BEGINS***'.                                   GBIEPGM 
00055  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GBIEPGM 
00056                                                                   GBIEPGM 
00057  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GBIEPGM 
00058                                                                   GBIEPGM 
00059 ** WORKFIELDS, AND SWITCHES **                                    GBIEPGM 
00060                                                                   GBIEPGM 
00061  01  WS-WORK-FIELDS.                                              GBIEPGM 
00062      05  WS-HOLD-JULIAN-DISPLAY               PIC 9(7).           GBIEPGM 
00063                                                                   GBIEPGM 
00064      05  WS-GCDATES-ADDR             VALUE +0 PIC S9(8) COMP.     GBIEPGM 
00065      05  WS-GCDATES-ENTRY-COUNT      VALUE +0 PIC S9(3) COMP-3.   GBIEPGM 
00066                                                                   GBIEPGM 
00067      05  WS-HEX-00                            PIC X.              GBIEPGM 
00068      05  WS-QUOTIENT                          PIC 999   COMP-3.   GBIEPGM 
00069      05  WS-REMAINDER                         PIC 999   COMP-3.   GBIEPGM 
00070      05  WS-INQUIRY-RESP         PIC S9(08) COMP VALUE +0.        GBIEPGM 
00071      05  WS-INQUIRY-STATUS       PIC S9(08) COMP VALUE +0.        GBIEPGM 
00072      05  WS-HOLD-INQUIRY-RESP    PIC 9(04).                       GBIEPGM 
00073      05  WS-DISPLAY-INQUIRY-RESP-X   REDEFINES                    GBIEPGM 
00074          WS-HOLD-INQUIRY-RESP    PIC X(04).                       GBIEPGM 
00075      05  WS-GX5W-TRANS-ID        PIC X(04) VALUE 'GX5W'.          GBIEPGM 
00076      05  WS-CHECK-TRANS-ID       PIC X(04) VALUE SPACES.          GBIEPGM 
00077                                                                   GBIEPGM 
00078 **********   AHL 11/25/86                                         GBIEPGM 
00079      05  WS-CLEAR-INDEX             VALUE +0  PIC S9(4) COMP.     GBIEPGM 
00080 *************************                                         GBIEPGM 
00081      05  WS-SCREEN-SUB              VALUE +0  PIC S9(4) COMP.     GBIEPGM 
00082      05  WS-SCREEN-LN               VALUE  00 PIC  9(2).          GBIEPGM 
00083      05  WS-SCREEN-MAX-ENTRIES      VALUE +30 PIC S9(4) COMP.     GBIEPGM 
00084                                                                   GBIEPGM 
00085      05  WS-DARK-FIELD-COUNT-X       VALUE  SPACES   PIC X(02).   GBIEPGM 
00086      05  WS-DARK-FIELD-COUNT   REDEFINES  WS-DARK-FIELD-COUNT-X   GBIEPGM 
00087                                                      PIC 9(02).   GBIEPGM 
00088                                                                   GBIEPGM 
00089      05  WS-PAGE-NAME                VALUE  'PAGE:'    PIC X(05). GBIEPGM 
00090      05  WS-PAGE-COUNT-X             VALUE  ZEROES     PIC X(03). GBIEPGM 
00091      05  WS-PAGE-COUNT   REDEFINES  WS-PAGE-COUNT-X    PIC 9(03). GBIEPGM 
00092                                                                   GBIEPGM 
00093      05  WS-SELECT-MATCH-INDICATOR            PIC X     VALUE ' '.GBIEPGM 
00094          88 WS-SELECT-MATCH-MADE                        VALUE 'Y'.GBIEPGM 
00095          88 WS-SELECT-MATCH-NOT-MADE                    VALUE 'N'.GBIEPGM 
00096                                                                   GBIEPGM 
00097      05  WS-FOUND-LAST-ENTRY-ON-SCREEN        PIC X(01) VALUE '0'.GBIEPGM 
00098          88  WS-LAST-ENTRY-IS-FOUND                     VALUE '1'.GBIEPGM 
00099      05  WS-ENTRY-IN-TABLE-IS-FOUND           PIC X(01) VALUE '0'.GBIEPGM 
00100          88  WS-ENTRY-IS-FOUND                          VALUE '1'.GBIEPGM 
00101          88  WS-ENTRY-IS-NOT-FOUND                      VALUE '0'.GBIEPGM 
00102                                                                   GBIEPGM 
00103      05  WS-DT-SUB                   VALUE +0 PIC S9(4) COMP.     GBIEPGM 
00104      05  WS-DT-SUBX                  VALUE +0 PIC S9(4) COMP.     GBIEPGM 
00105      05  WS-DT-SUB2                  VALUE +0 PIC S9(4) COMP.     GBIEPGM 
00106      05  WS-DT-SUB3                  VALUE +0 PIC S9(4) COMP.     GBIEPGM 
00107      05  WS-SYSID.                                                GBIEPGM 
00108          10  FILLER                           PIC X(01).          GBIEPGM 
00109              88  WS-TEXAS-CICS-REGION         VALUE 'X'.          GBIEPGM 
00110          10  FILLER                           PIC X(03).          GBIEPGM 
00111      05  WS-SORT-EXCHANGE-INDICATOR           PIC X     VALUE ' '.GBIEPGM 
00112          88 WS-SORT-EXCHANGE-NOT-MADE                   VALUE 'N'.GBIEPGM 
00113      05  WS-COMP-SERV-DT-CEN                 PIC S9(7) COMP-3.    GBIEPGM 
00114      05  WS-HOLD-DT-ENTRY.                                        GBIEPGM 
00115          10  WS-HOLD-EFF-DATE.                                    GBIEPGM 
00116              15  WS-HOLD-EFFDT-CC                 PIC X.          GBIEPGM 
00117              15  WS-HOLD-DT-EFF-DT               PIC S9(5) COMP-3.GBIEPGM 
00118          10  WS-HOLD-EFFDT-CEN REDEFINES                          GBIEPGM 
00119                 WS-HOLD-EFF-DATE                 PIC S9(7) COMP-3.GBIEPGM 
00120          10  WS-HOLD-DT-FAM-REL                  PIC X(2).        GBIEPGM 
00121          10  WS-HOLD-TERM-DATE.                                   GBIEPGM 
00122              15 WS-HOLD-TERM-DT-CC               PIC X.           GBIEPGM 
00123              15  WS-HOLD-DT-TERM-DT              PIC S9(5) COMP-3.GBIEPGM 
00124          10  WS-HOLD-TERM-DATE-CEN REDEFINES                      GBIEPGM 
00125                  WS-HOLD-TERM-DATE               PIC S9(7) COMP-3.GBIEPGM 
00126                                                                   GBIEPGM 
00127      05  WS-SAVE-FIRST-LN-NO            PIC 9(2) VALUE ZEROES.    GBIEPGM 
00128      05  WS-SAVE-FIRST-ENTRY.                                     GBIEPGM 
00129          10  WS-SAVE-FIRST-EFF-DATE.                              GBIEPGM 
00130              15  WS-SAVE-FIRST-EFFDT-CC PIC X.                    GBIEPGM 
00131              15  WS-SAVE-FIRST-EFF-DT   PIC S9(5) COMP-3 VALUE +0.GBIEPGM 
00132          10  WS-SAVE-FIRST-EFFDT-CEN REDEFINES                    GBIEPGM 
00133                 WS-SAVE-FIRST-EFF-DATE  PIC S9(7) COMP-3.         GBIEPGM 
00134          10  WS-SAVE-FIRST-FAM-REL      PIC X(2)  VALUE SPACE.    GBIEPGM 
00135          10  WS-SAVE-FIRST-TRM-DATE.                              GBIEPGM 
00136              15  WS-SAVE-FIRST-TRMDT-CC PIC X.                    GBIEPGM 
00137              15  WS-SAVE-FIRST-TERM-DT  PIC S9(5) COMP-3 VALUE +0.GBIEPGM 
00138          10  WS-SAVE-FIRST-TRMDT-CEN REDEFINES                    GBIEPGM 
00139                 WS-SAVE-FIRST-TRM-DATE  PIC S9(7) COMP-3.         GBIEPGM 
00140                                                                   GBIEPGM 
00141      05  WS-SAVE-LAST-LN-NO             PIC 9(2) VALUE ZEROES.    GBIEPGM 
00142      05  WS-SAVE-LAST-ENTRY.                                      GBIEPGM 
00143          10  WS-SAVE-LAST-EFF-DATE.                               GBIEPGM 
00144              15  WS-SAVE-LAST-EFFDT-CC  PIC X.                    GBIEPGM 
00145              15  WS-SAVE-LAST-EFF-DT    PIC S9(5) COMP-3 VALUE +0.GBIEPGM 
00146          10  WS-SAVE-LAST-EFFDT-CEN REDEFINES                     GBIEPGM 
00147                 WS-SAVE-LAST-EFF-DATE   PIC S9(7) COMP-3.         GBIEPGM 
00148          10  WS-SAVE-LAST-FAM-REL       PIC X(2) VALUE SPACE.     GBIEPGM 
00149          10  WS-SAVE-LAST-TRM-DATE.                               GBIEPGM 
00150              15  WS-SAVE-LAST-TRMDT-CC  PIC X.                    GBIEPGM 
00151              15  WS-SAVE-LAST-TERM-DT   PIC S9(5) COMP-3 VALUE +0.GBIEPGM 
00152          10  WS-SAVE-LAST-TRMDT-CEN REDEFINES                     GBIEPGM 
00153                 WS-SAVE-LAST-TRM-DATE   PIC S9(7) COMP-3.         GBIEPGM 
00154                                                                   GBIEPGM 
00155                                                                   GBIEPGM 
00156      05  WS-SCREEN-KEY-ENTRY-SWITCHES.                            GBIEPGM 
00157          88  WS-FULL-GRPSPC-KEY-ENTERED     VALUE ALL '1'.        GBIEPGM 
00158              10  WS-ENTERED-IEPLNX            PIC X  VALUE '0'.   GBIEPGM 
00159              10  WS-ENTERED-IEGRPX            PIC X  VALUE '0'.   GBIEPGM 
00160              10  WS-ENTERED-IESECX            PIC X  VALUE '0'.   GBIEPGM 
00161              10  WS-ENTERED-IEPKGX            PIC X  VALUE '0'.   GBIEPGM 
00162              10  WS-ENTERED-IEFRX             PIC X  VALUE '0'.   GBIEPGM 
00163              10  WS-ENTERED-IEEDTX            PIC X  VALUE '0'.   GBIEPGM 
00164                                                                   GBIEPGM 
00165      05  WS-SELECTION-SWITCHES.                                   GBIEPGM 
00166          10  WS-ENTERED-IEOPT                 PIC X  VALUE '0'.   GBIEPGM 
00167          10  WS-ENTERED-IELNNO                PIC X  VALUE '0'.   GBIEPGM 
00168                                                                   GBIEPGM 
00169      05  WS-ERROR-SWITCH                      PIC X  VALUE '0'.   GBIEPGM 
00170                                                                   GBIEPGM 
00171      05  WS-GROUP-SPECIFIC-ID.                                    GBIEPGM 
00172          10  WS-PLAN-CODE                     PIC X(3)  VALUE ' '.GBIEPGM 
00173          10  WS-GROUP-NUM.                                        GBIEPGM 
00174              15 WS-GRP-NO-1-3                 PIC X(3)  VALUE ' '.GBIEPGM 
00175              15 WS-GRP-NO                     PIC X(06) VALUE ' '.GBIEPGM 
00176          10  WS-SECTION-NUM.                                      GBIEPGM 
00177              15 WS-SECTN-NO-1                 PIC X     VALUE ' '.GBIEPGM 
00178              15 WS-SECTN-NO                   PIC X(04) VALUE ' '.GBIEPGM 
00179          10  WS-PKG-CODE                      PIC X(3)  VALUE ' '.GBIEPGM 
00180          10  WS-FAM-REL-LVL                   PIC X(02) VALUE ' '.GBIEPGM 
00181          10  WS-EFF-DATE.                                         GBIEPGM 
00182              15 WS-EFFDT-CC                   PIC X.              GBIEPGM 
00183              15 WS-EFF-DT          COMP-3     PIC S9(5) VALUE +0. GBIEPGM 
00184          10  WS-EFFDT-CEN REDEFINES                               GBIEPGM 
00185                 WS-EFF-DATE        COMP-3     PIC S9(7).          GBIEPGM 
00186                                                                   GBIEPGM 
00187      05  WS-DISP-GROUPSPC.                                        GBIEPGM 
00188          10  FILLER                     PIC X(04) VALUE 'GRP='.   GBIEPGM 
00189 *        10  WS-DISP-PLAN               PIC X(03).                GBIEPGM 
00190          10  FILLER                     PIC X(01) VALUE SPACES.   GBIEPGM 
00191          10  FILLER                     PIC X(04) VALUE 'GRP:'.   GBIEPGM 
00192          10  WS-DISP-GROUP              PIC X(09).                GBIEPGM 
00193          10  FILLER                     PIC X(01) VALUE SPACES.   GBIEPGM 
00194          10  FILLER                     PIC X(04) VALUE 'SEC:'.   GBIEPGM 
00195          10  WS-DISP-SECTION            PIC X(05).                GBIEPGM 
00196          10  FILLER                     PIC X(01) VALUE SPACES.   GBIEPGM 
00197          10  FILLER                     PIC X(03) VALUE 'PK:'.    GBIEPGM 
00198          10  WS-DISP-PKG                PIC X(03).                GBIEPGM 
00199          10  FILLER                     PIC X(11) VALUE SPACES.   GBIEPGM 
00200          10  FILLER                     PIC X(01) VALUE SPACES.   GBIEPGM 
00201          10  FILLER                     PIC X(03) VALUE 'FR:'.    GBIEPGM 
00202          10  WS-DISP-FRL                PIC X(02).                GBIEPGM 
00203          10  FILLER                     PIC X(01) VALUE SPACES.   GBIEPGM 
00204          10  FILLER                     PIC X(04) VALUE 'EFF:'.   GBIEPGM 
00205          10  WS-DISP-EFFDT              PIC 9(07).                GBIEPGM 
00206          10  FILLER                     PIC X(01) VALUE SPACES.   GBIEPGM 
00207          10  FILLER                     PIC X(04) VALUE 'TRM:'.   GBIEPGM 
00208          10  WS-DISP-TRMDT              PIC 9(07).                GBIEPGM 
00209                                                                   GBIEPGM 
00210      05  WS-DISP-CONTRACT.                                        GBIEPGM 
00211          10  FILLER                     PIC X(04) VALUE 'CNT='.   GBIEPGM 
00212          10  FILLER                     PIC X(01) VALUE SPACES.   GBIEPGM 
00213          10  FILLER                     PIC X(04) VALUE 'GRP:'.   GBIEPGM 
00214 *        10  WS-DISP-PLAN-CT            PIC X(03).                GBIEPGM 
00215 *        10  FILLER                     PIC X(01) VALUE SPACES.   GBIEPGM 
00216          10  WS-DISP-GROUP-CT           PIC X(09).                GBIEPGM 
00217          10  FILLER                     PIC X(01) VALUE SPACES.   GBIEPGM 
00218          10  FILLER                     PIC X(04) VALUE 'SEC:'.   GBIEPGM 
00219          10  WS-DISP-SECTION-CT         PIC X(05).                GBIEPGM 
00220          10  FILLER                     PIC X(01) VALUE SPACES.   GBIEPGM 
00221          10  FILLER                     PIC X(03) VALUE 'PK:'.    GBIEPGM 
00222          10  WS-DISP-PKG-CT             PIC X(03).                GBIEPGM 
00223          10  FILLER                     PIC X(01) VALUE SPACES.   GBIEPGM 
00224          10  FILLER                     PIC X(03) VALUE 'LB:'.    GBIEPGM 
00225          10  WS-DISP-LOB-CT             PIC X(01).                GBIEPGM 
00226          10  FILLER                     PIC X(01) VALUE SPACES.   GBIEPGM 
00227          10  FILLER                     PIC X(03) VALUE 'PR:'.    GBIEPGM 
00228          10  WS-DISP-PRV-CT             PIC X(02).                GBIEPGM 
00229          10  FILLER                     PIC X(01) VALUE SPACES.   GBIEPGM 
00230          10  FILLER                     PIC X(03) VALUE 'FR:'.    GBIEPGM 
00231          10  WS-DISP-FRL-CT             PIC X(02).                GBIEPGM 
00232          10  FILLER                     PIC X(01) VALUE SPACES.   GBIEPGM 
00233          10  FILLER                     PIC X(04) VALUE 'EFF:'.   GBIEPGM 
00234          10  WS-DISP-EFFDT-CT           PIC 9(07).                GBIEPGM 
00235          10  FILLER                     PIC X(01) VALUE SPACES.   GBIEPGM 
00236          10  FILLER                     PIC X(04) VALUE 'TRM:'.   GBIEPGM 
00237          10  WS-DISP-TRMDT-CT           PIC 9(07).                GBIEPGM 
00238                                                                   GBIEPGM 
00239       05 WS-MAP-CONTRACT-KEY.                                     GBIEPGM 
00240          10  WS-MAP-CONT-KEY-1.                                   GBIEPGM 
00241              15 WS-MAP-PLAN-CODE                  PIC X(3).       GBIEPGM 
00242              15 WS-MAP-GROUP-NUM.                                 GBIEPGM 
00243                 20 WS-MAP-GROUP-NO-1-3            PIC X(3).       GBIEPGM 
00244                 20 WS-MAP-GRP-NO                  PIC X(6).       GBIEPGM 
00245              15 WS-MAP-SECTION-NUM.                               GBIEPGM 
00246                 20 WS-MAP-SEC-NO-1                PIC X(1).       GBIEPGM 
00247                 20 WS-MAP-SECTN-NO                PIC X(4).       GBIEPGM 
00248              15 WS-MAP-PKG-CODE                   PIC X(3).       GBIEPGM 
00249              15 WS-MAP-L-O-B                      PIC X.          GBIEPGM 
00250              15 WS-MAP-PROVDR-CONTROL             PIC XX.         GBIEPGM 
00251              15 WS-MAP-FAM-REL-LVL                PIC XX.         GBIEPGM 
00252          10  WS-MAP-CONT-KEY-2.                                   GBIEPGM 
00253              15 WS-MAP-EFFDT-CEN        COMP-3  PIC S9(7).        GBIEPGM 
00254                                                                   GBIEPGM 
00255                                                                   GBIEPGM 
00256      05  WS-GCDATES-KEY.                                          GBIEPGM 
00257          10  WS-GCDATES-PLAN-CODE             PIC X(3)  VALUE ' '.GBIEPGM 
00258          10  WS-GCDATES-GRP-NUM.                                  GBIEPGM 
00259              15 WS-GCDATES-GRP-NO-1-3         PIC X(3)  VALUE ' '.GBIEPGM 
00260              15 WS-GCDATES-GRP-NO             PIC X(06) VALUE ' '.GBIEPGM 
00261          10  WS-GCDATES-SECTN-NUM.                                GBIEPGM 
00262              15 WS-GCDATES-SECTN-NO-1         PIC X     VALUE ' '.GBIEPGM 
00263              15  WS-GCDATES-SECTN-NO          PIC X(04) VALUE ' '.GBIEPGM 
00264          10  WS-GCDATES-PKG-CODE              PIC X(3)  VALUE ' '.GBIEPGM 
00265          10  WS-GCDATES-KEY-FILLER            PIC X(11) VALUE     GBIEPGM 
00266                                                        LOW-VALUES.GBIEPGM 
00267          10  WS-GCDATES-FILE-REF-IND          PIC X(01) VALUE 'G'.GBIEPGM 
00268                                                                   GBIEPGM 
00269      05  WS-CLASS-TEST-AREA                   PIC X(10) VALUE '0'.GBIEPGM 
00270      05  WS-CLASS-TEST-DIGIT REDEFINES                            GBIEPGM 
00271          WS-CLASS-TEST-AREA  OCCURS 10 TIMES  PIC X.              GBIEPGM 
00272          88  WS-CLASS-ALPHANUMERIC            VALUES '0' THRU '9' GBIEPGM 
00273                                                      'A' THRU 'I' GBIEPGM 
00274                                                      'J' THRU 'R' GBIEPGM 
00275                                                      'S' THRU 'Z'.GBIEPGM 
00276                                                                   GBIEPGM 
00277      05  WS-XCTL-TO-PGM                       PIC X(08) VALUE ' '.GBIEPGM 
00278 /* ALTERNATIVE WORKFILE KEYS **                                   GBIEPGM 
00279  01  FILLER                      PIC X(32)  VALUE                 GBIEPGM 
00280      '*** ALTERNATIVE WORKFILE KEY ***'.                          GBIEPGM 
00281  01  WS-ALT-WORKFILE-KEYS.                                        GBIEPGM 
00282  COPY GCWRKKEY.                                                   GBIEPGM 
00283 /                                                                 GBIEPGM 
00284  01  WS-GX5W-COMMAREA.                                            GBIEPGM 
00285      05  GX5W-PLN-CODE           VALUE LOW-VALUES   PIC X(3).     GBIEPGM 
00286      05  GX5W-GRP-NO             VALUE LOW-VALUES   PIC X(9).     GBIEPGM 
00287 /                                                                 GBIEPGM 
00288  01  WS-GIEC-COMMAREA.                                            GBIEPGM 
00289      05 WS-GIEC-PLAN-CODE        PIC  X(03) VALUE LOW-VALUES.     GBIEPGM 
00290      05 WS-GIEC-GROUP-NO         PIC  X(09) VALUE LOW-VALUES.     GBIEPGM 
00291      05 WS-GIEC-SECTION-NUM      PIC  X(05) VALUE LOW-VALUES.     GBIEPGM 
00292 /                                                                 GBIEPGM 
00293  01  HGADATES-COMMAREA.                                           GBIEPGM 
00294  COPY HGCDAT01.                                                   GBIEPGM 
00295                                                                   GBIEPGM 
00296 /                                                                 GBIEPGM 
00297 **************  HEX CEN. VALUES ******************************    GBIEPGM 
00298  COPY HEXCOBOL.                                                   GBIEPGM 
00299                                                                   GBIEPGM 
00300 /                                                                 GBIEPGM 
00301 **************  MIL DATES       ******************************    GBIEPGM 
00302  COPY MLDATE01.                                                   GBIEPGM 
00303                                                                   GBIEPGM 
00304 /* ATTRIBUTES **                                                  GBIEPGM 
00305  COPY DFHBMSCA.                                                   GBIEPGM 
00306      02  DFHBMABF                PIC X VALUE 'Z'.                 GBIEPGM 
00307 /* ATTENTION IDENTIFIERS **                                       GBIEPGM 
00308  COPY DFHAID.                                                     GBIEPGM 
00309 /                                                                 GBIEPGM 
00310 ** RECORD LENGTHS **                                              GBIEPGM 
00311  01  WS-RECORD-LENGTHS.                                           GBIEPGM 
00312      05  WS-GIEC-COM-KEY-LEN          PIC S9(4) COMP   VALUE +17. GBIEPGM 
00313      05  WS-COMM-KEY-LEN              PIC S9(4) COMP   VALUE +150.GBIEPGM 
00314 **** 05  GC-GCIOPARM-LEN              PIC S9(5) COMP-3 VALUE +228.GBIEPGM 
00315                                                                   GBIEPGM 
00316      05  WS-IO-PARM-GRPSPC-LEN        PIC S9(4) COMP   VALUE +0.  GBIEPGM 
00317 **** 05  GC-GCGRPSPC-FIXED-LEN        PIC S9(5) COMP-3 VALUE +410.GBIEPGM 
00318 **** 05  GC-GCGRPSPC-VARY-LEN         PIC S9(5) COMP-3 VALUE +10. GBIEPGM 
00319 **** 05  GC-GCGRPSPC-VARY-MAX-OCUR    PIC S9(5) COMP-3 VALUE +30. GBIEPGM 
00320                                                                   GBIEPGM 
00321      05  WS-IO-PARM-GCDATES-LEN       PIC S9(4) COMP   VALUE +0.  GBIEPGM 
00322 **** 05  GC-GCDATES-FIXED-LEN         PIC S9(5) COMP-3 VALUE +39. GBIEPGM 
00323 **** 05  GC-GCDATES-VARY-LEN          PIC S9(5) COMP-3 VALUE +9.  GBIEPGM 
00324 **** 05  GC-GCDATES-VARY-MAX-OCUR     PIC S9(5) COMP-3 VALUE +440.GBIEPGM 
00325                                                                   GBIEPGM 
00326      05  WS-DATES-TABLE-LEN           PIC S9(4) COMP   VALUE +0.  GBIEPGM 
00327      05  WS-DT-FIXED-PORTION          PIC S9(5) COMP-3 VALUE +3.  GBIEPGM 
00328      05  WS-DT-VARIABLE-PORTION       PIC S9(5) COMP-3 VALUE +10. GBIEPGM 
00329                                                                   GBIEPGM 
00330      05  WS-GRPSPC-COMP               PIC S9(08) COMP  VALUE +0.  GBIEPGM 
00331      05  WS-GRPSPC-PNTR               REDEFINES WS-GRPSPC-COMP    GBIEPGM 
00332                                       USAGE POINTER.              GBIEPGM 
00333                                                                   GBIEPGM 
00334  01  WS-GCPS-RECORD-LENGTHS.                                      GBIEPGM 
00335      COPY GCCDRLEN.                                               GBIEPGM 
00336 /* MAP COBOL SCREEN DSECTS **                                     GBIEPGM 
00337  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GBIEPGM 
00338      '***  I/O MAPAREA ***'.                                      GBIEPGM 
00339  COPY GBIESETC.                                                   GBIEPGM 
00340                                                                   GBIEPGM 
00341                                                                   GBIEPGM 
00342  01  WS-END                      PIC X(16)  VALUE                 GBIEPGM 
00343      '*** W/S ENDS ***'.                                          GBIEPGM 
00344 /                                                                 GBIEPGM 
00345  LINKAGE SECTION.                                                 GBIEPGM 
00346  01  DFHCOMMAREA.                                                 GBIEPGM 
00347  COPY G2COMKEC.                                                   GBIEPGM 
00348      10   GI-REDF-AREA REDEFINES GI-FILLER.                       GBIEPGM 
00349           15  GI-SUBSCRIBER     PIC X(09).                        GBIEPGM 
00350           15  GI-LAST-NAME      PIC X(15).                        GBIEPGM 
00351           15  GI-FIRST-NAME     PIC X(09).                        GBIEPGM 
00352           15  GI-REDF-FILLER    PIC X(03).                        GBIEPGM 
00353 /                                                                 GBIEPGM 
00354                                                                   GBIEPGM 
00355 /                                                                 GBIEPGM 
00356  01  IO-PARM-GRPSPC-RECORD.                                       GBIEPGM 
00357  COPY GCIOPRM1.                                                   GBIEPGM 
00358                                                                   GBIEPGM 
00359  COPY GCGROUPC.                                                   GBIEPGM 
00360 /                                                                 GBIEPGM 
00361  01  GI-COMMAREA2-RECORD.                                         GBIEPGM 
00362  COPY G2COMKE2.                                                   GBIEPGM 
00363      10   GI2-REDF-AREA REDEFINES GI2-FILLER.                     GBIEPGM 
00364           15  GI2-SUBSCRIBER     PIC X(09).                       GBIEPGM 
00365           15  GI2-LAST-NAME      PIC X(15).                       GBIEPGM 
00366           15  GI2-FIRST-NAME     PIC X(09).                       GBIEPGM 
00367           15  GI2-REDF-FILLER    PIC X(03).                       GBIEPGM 
00368 /                                                                 GBIEPGM 
00369  01  IO-PARM-GCDATES-RECORD.                                      GBIEPGM 
00370  COPY GCIOPRM2.                                                   GBIEPGM 
00371                                                                   GBIEPGM 
00372  COPY GCDATEC5.                                                   GBIEPGM 
00373 /*****************************************************************GBIEPGM 
00374 *                                                                *GBIEPGM 
00375 *                      DATES TABLE                               *GBIEPGM 
00376 *                                                                *GBIEPGM 
00377 ******************************************************************GBIEPGM 
00378  01  DATES-TABLE.                                                 GBIEPGM 
00379      05  DT-ENTRY-COUNT                          PIC S9(5) COMP-3.GBIEPGM 
00380      05  DT-ENTRY               OCCURS 1 TO 410 TIMES             GBIEPGM 
00381                                 DEPENDING ON DT-ENTRY-COUNT.      GBIEPGM 
00382          10  DT-EFF-DATE.                                         GBIEPGM 
00383              15 DT-EFF-DT-CC                     PIC X(01).       GBIEPGM 
00384              15 DT-EFF-DT                        PIC S9(5) COMP-3.GBIEPGM 
00385          10  DT-EFF-DT-CEN   REDEFINES                            GBIEPGM 
00386              DT-EFF-DATE                         PIC S9(7) COMP-3.GBIEPGM 
00387          10  DT-FAM-REL                          PIC X(2).        GBIEPGM 
00388          10  DT-TERM-DATE.                                        GBIEPGM 
00389              15 DT-TERM-DT-CC                    PIC X(01).       GBIEPGM 
00390              15 DT-TERM-DT                       PIC S9(5) COMP-3.GBIEPGM 
00391          10  DT-TERM-DT-CEN  REDEFINES                            GBIEPGM 
00392              DT-TERM-DATE                        PIC S9(7) COMP-3.GBIEPGM 
00393 /                                                                 GBIEPGM 
00394  PROCEDURE DIVISION.                                              GBIEPGM 
00395                                                                   GBIEPGM 
00396 ******************************************************************GBIEPGM 
00397 **                                                               *GBIEPGM 
00398 ** 1000                M A I N L I N E                           *GBIEPGM 
00399 **                                                               *GBIEPGM 
00400 ******************************************************************GBIEPGM 
00401  1000-000-MAIN-LINE SECTION.                                      GBIEPGM 
00402  1000-010.                                                        GBIEPGM 
00403                                                                   GBIEPGM 
00404      MOVE '1000'  TO  WS-PARA-ID.                                 GBIEPGM 
00405                                                                   GBIEPGM 
00406      MOVE  LOW-VALUES  TO  WS-HEX-00.                             GBIEPGM 
00407                                                                   GBIEPGM 
00408      EXEC CICS HANDLE CONDITION                                   GBIEPGM 
00409                       MAPFAIL(6000-000-XCTL-TO-MAIN-MENU)         GBIEPGM 
00410                       END-EXEC.                                   GBIEPGM 
00411                                                                   GBIEPGM 
00412 *        +----------------------------------------+               GBIEPGM 
00413 *        +  ACQUIRE STORAGE FOR COMMAREA FOR      +               GBIEPGM 
00414 *        +  LOWER LEVEL MAINLINE MODULES          +               GBIEPGM 
00415 *        +----------------------------------------+               GBIEPGM 
00416                                                                   GBIEPGM 
00417      EXEC CICS GETMAIN  SET (ADDRESS OF GI-COMMAREA2-RECORD)      GBIEPGM 
00418                         INITIMG(WS-HEX-00)                        GBIEPGM 
00419                         LENGTH (WS-COMM-KEY-LEN)                  GBIEPGM 
00420                         END-EXEC.                                 GBIEPGM 
00421                                                                   GBIEPGM 
00422 *        +----------------------------------------+               GBIEPGM 
00423 *        +  IS MY TRANSACTION CODE?               +               GBIEPGM 
00424 *        +----------------------------------------+               GBIEPGM 
00425                                                                   GBIEPGM 
00426                                                                   GBIEPGM 
00427      IF EIBTRNID      =  'GBIB' OR  'GHIL' OR 'GBIG'              GBIEPGM 
00428         MOVE GIC-CONTRACT-ID  TO WS-MAP-CONTRACT-KEY              GBIEPGM 
00429         MOVE WS-MAP-CONT-KEY-1    TO IECNKEYO                     GBIEPGM 
00430         MOVE GI-EFFECTIVE-DATE    TO IECNKEDO                     GBIEPGM 
00431         PERFORM 9300-000-LOAD-GBIE-KEY                            GBIEPGM 
00432         PERFORM 9400-000-SEND-TEST-MAP                            GBIEPGM 
00433         PERFORM 2000-000-SCREEN-PROCESSING                        GBIEPGM 
00434           IF (NOT WS-FULL-GRPSPC-KEY-ENTERED)  AND                GBIEPGM 
00435              (WS-ERROR-SWITCH  =  '0')         AND                GBIEPGM 
00436 ***          (DT-ENTRY-COUNT   <  +2 )                            GBIEPGM 
00437              (WS-DT-SUBX       <  +2 )                            GBIEPGM 
00438              PERFORM 2000-000-SCREEN-PROCESSING                   GBIEPGM 
00439           END-IF                                                  GBIEPGM 
00440         GO TO 1000-800-RETURN                                     GBIEPGM 
00441      END-IF                                                       GBIEPGM 
00442                                                                   GBIEPGM 
00443                                                                   GBIEPGM 
00444 *        +----------------------------------------+               GBIEPGM 
00445 *        +  RECEIVE THE CURRENT SCREEN            +               GBIEPGM 
00446 *        +----------------------------------------+               GBIEPGM 
00447                                                                   GBIEPGM 
00448      EXEC CICS RECEIVE   MAP   ('GBIEI01')                        GBIEPGM 
00449                          MAPSET('GBIESET')                        GBIEPGM 
00450                          INTO  (GBIEI01I)                         GBIEPGM 
00451                          END-EXEC.                                GBIEPGM 
00452                                                                   GBIEPGM 
00453 *        +----------------------------------------+               GBIEPGM 
00454 *        + IS THIS SCREEN MINE?                   +               GBIEPGM 
00455 *        +----------------------------------------+               GBIEPGM 
00456                                                                   GBIEPGM 
00457      IF IESCRIDI  NOT =  '00IE0I'                                 GBIEPGM 
00458         PERFORM 6000-000-XCTL-TO-MAIN-MENU.                       GBIEPGM 
00459                                                                   GBIEPGM 
00460 *        +----------------------------------------+               GBIEPGM 
00461 *        + ENTER KEYED?                           +               GBIEPGM 
00462 *        +----------------------------------------+               GBIEPGM 
00463                                                                   GBIEPGM 
00464      IF EIBAID  =  DFHENTER   OR                                  GBIEPGM 
00465 ***                DFHPF5     OR   DFHPF17  OR                    GBIEPGM 
00466                    DFHPF7     OR   DFHPF19  OR                    GBIEPGM 
00467                    DFHPF8     OR   DFHPF20                        GBIEPGM 
00468         PERFORM 2000-000-SCREEN-PROCESSING                        GBIEPGM 
00469         GO TO 1000-800-RETURN.                                    GBIEPGM 
00470                                                                   GBIEPGM 
00471 *        +----------------------------------------+               GBIEPGM 
00472 *        + REQUEST PREVIOUS MENU?                 +               GBIEPGM 
00473 *        +----------------------------------------+               GBIEPGM 
00474                                                                   GBIEPGM 
00475      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GBIEPGM 
00476         MOVE  IEPLNXO   TO   GIC2-PLAN-CODE                       GBIEPGM 
00477         MOVE  IEGRPXO   TO   GIC2-GROUP-NUM                       GBIEPGM 
00478         MOVE  IESECXO   TO   GIC2-SECTION-NUM                     GBIEPGM 
00479         MOVE IERETCO    TO GI2-RETURN-CODE                        GBIEPGM 
00480         MOVE IESUBO        TO GI2-SUBSCRIBER                      GBIEPGM 
00481         EXEC CICS XCTL  PROGRAM('GHILPGM')                        GBIEPGM 
00482                         COMMAREA(GI-COMMAREA2-RECORD)             GBIEPGM 
00483                         LENGTH  (WS-COMM-KEY-LEN)                 GBIEPGM 
00484                         END-EXEC.                                 GBIEPGM 
00485                                                                   GBIEPGM 
00486 *        +----------------------------------------+               GBIEPGM 
00487 *        + INVALID REQUEST PROCESS                +               GBIEPGM 
00488 *        +----------------------------------------+               GBIEPGM 
00489                                                                   GBIEPGM 
00490      MOVE -1                  TO  IEPLNXL.                        GBIEPGM 
00491      MOVE '*** INVALID REQUEST. THE PF KEY USED HAS NO MEANING TO GBIEPGM 
00492 -         'THIS PROGRAM ***'  TO  IEMSGO(1).                      GBIEPGM 
00493      MOVE SPACES              TO  IEMSGO(2).                      GBIEPGM 
00494      PERFORM 7500-000-SEND-DATAONLY.                              GBIEPGM 
00495                                                                   GBIEPGM 
00496                                                                   GBIEPGM 
00497  1000-800-RETURN.                                                 GBIEPGM 
00498                                                                   GBIEPGM 
00499      EXEC CICS RETURN   END-EXEC.                                 GBIEPGM 
00500                                                                   GBIEPGM 
00501      GOBACK.                                                      GBIEPGM 
00502                                                                   GBIEPGM 
00503  1000-900-EXIT.                                                   GBIEPGM 
00504      EXIT.                                                        GBIEPGM 
00505 /*****************************************************************GBIEPGM 
00506 **                                                               *GBIEPGM 
00507 ** 2000  SCREEN PROCESSING                                       *GBIEPGM 
00508 **                                                               *GBIEPGM 
00509 ******************************************************************GBIEPGM 
00510  2000-000-SCREEN-PROCESSING    SECTION.                           GBIEPGM 
00511  2000-010.                                                        GBIEPGM 
00512                                                                   GBIEPGM 
00513      MOVE '2000'  TO  WS-PARA-ID.                                 GBIEPGM 
00514                                                                   GBIEPGM 
00515 *  +-------------------------------------------------------+      GBIEPGM 
00516 *    IF THE GROUP NUMBER IS FILLED, AND BOTH THE SECTION          GBIEPGM 
00517 *    NUMBER AND OPTION IS NOT FILLED, TRANSFER CONTROL            GBIEPGM 
00518 *    TO THE GROUP/SECTION INQUIRY PROGRAM (GIECPGM).              GBIEPGM 
00519 *  +-------------------------------------------------------+      GBIEPGM 
00520 ***                                                               GBIEPGM 
00521 *     IF IEPLNXL  >   ZERO                                        GBIEPGM 
00522 *      IF IEGRPXL   >   ZERO                                      GBIEPGM 
00523 *        IF (IESECXL  NOT >   ZERO OR IEPKGXL NOT > ZERO)         GBIEPGM 
00524 ***        IF IEOPTL    NOT  >    ZERO                            GBIEPGM 
00525 ***          IF IEOPTO    NOT =   ('1'  OR  '2'  OR  '3')         GBIEPGM 
00526 *             MOVE IEPLNXO  TO  WS-GIEC-PLAN-CODE                 GBIEPGM 
00527 *             MOVE IEGRPXO  TO  WS-GIEC-GROUP-NO                  GBIEPGM 
00528 *             MOVE IESECXO  TO  WS-GIEC-SECTION-NUM               GBIEPGM 
00529 *             EXEC CICS XCTL                                      GBIEPGM 
00530 *                       PROGRAM   ('GIECPGM')                     GBIEPGM 
00531 *                       COMMAREA  (WS-GIEC-COMMAREA)              GBIEPGM 
00532 *                       LENGTH    (WS-GIEC-COM-KEY-LEN)           GBIEPGM 
00533 *                       END-EXEC.                                 GBIEPGM 
00534                                                                   GBIEPGM 
00535                                                                   GBIEPGM 
00536      MOVE SPACES   TO WS-GROUP-SPECIFIC-ID                        GBIEPGM 
00537                       IEMSGO(1)                                   GBIEPGM 
00538                       IEMSGO(2).                                  GBIEPGM 
00539                                                                   GBIEPGM 
00540 *    MOVE DFHBMUNF TO IEPLNXA                                     GBIEPGM 
00541 *                     IEGRPXA                                     GBIEPGM 
00542 *                     IESECXA                                     GBIEPGM 
00543 *                     IEPKGXA                                     GBIEPGM 
00544 *                     IEFRXA                                      GBIEPGM 
00545 *                     IEEDTXA                                     GBIEPGM 
00546 ***                   IEOPTA                                      GBIEPGM 
00547 *                     IELNNOA.                                    GBIEPGM 
00548                                                                   GBIEPGM 
00549      MOVE DFHBMUNF TO IELNNOA.                                    GBIEPGM 
00550                                                                   GBIEPGM 
00551 *        +----------------------------------------+               GBIEPGM 
00552 *        +  FIELDS ARE EDITED  IN THE REVERSE     +               GBIEPGM 
00553 *        +  ORDER FROM WHICH THEY APPEAR ON THE   +               GBIEPGM 
00554 *        +  SCREEN TO SYNC CURSOR POSITIONING     +               GBIEPGM 
00555 *        +  AND ERROR MESSAGE.                    +               GBIEPGM 
00556 *        +----------------------------------------+               GBIEPGM 
00557                                                                   GBIEPGM 
00558      IF  IELNNOL > 0                                              GBIEPGM 
00559          IF  IELNNOO > ZERO    AND   NOT > IELNCNTO               GBIEPGM 
00560              MOVE '1' TO WS-ENTERED-IELNNO                        GBIEPGM 
00561          ELSE                                                     GBIEPGM 
00562              MOVE -1        TO IELNNOL                            GBIEPGM 
00563              MOVE DFHBMUBF  TO IELNNOA                            GBIEPGM 
00564              MOVE '1'       TO WS-ERROR-SWITCH                    GBIEPGM 
00565              MOVE 'INVALID GROUP SPECIFIC LINE NUMBER SELECTION'  GBIEPGM 
00566                TO IEMSGO(1)                                       GBIEPGM 
00567      ELSE                                                         GBIEPGM 
00568          NEXT SENTENCE.                                           GBIEPGM 
00569                                                                   GBIEPGM 
00570                                                                   GBIEPGM 
00571 *        +----------------------------------------+               GBIEPGM 
00572 *        +  OPTION SELECTION                      +               GBIEPGM 
00573 *        +----------------------------------------+               GBIEPGM 
00574                                                                   GBIEPGM 
00575 ***  IF  IEOPTL > 0                                               GBIEPGM 
00576 *        IF  IEOPTO = '1' OR '2' OR '3'                           GBIEPGM 
00577 *            MOVE '1' TO WS-ENTERED-IEOPT                         GBIEPGM 
00578 *        ELSE                                                     GBIEPGM 
00579 *            MOVE -1        TO IEOPTL                             GBIEPGM 
00580 *            MOVE DFHBMUBF  TO IEOPTA                             GBIEPGM 
00581 *            MOVE '1'       TO WS-ERROR-SWITCH                    GBIEPGM 
00582 *            MOVE 'INVALID SELECT OPTION'                         GBIEPGM 
00583 *              TO IEMSGO(1)                                       GBIEPGM 
00584 *    ELSE                                                         GBIEPGM 
00585 *        NEXT SENTENCE.                                           GBIEPGM 
00586                                                                   GBIEPGM 
00587 *        +----------------------------------------+               GBIEPGM 
00588 *        +  EFFECTIVE DATE                        +               GBIEPGM 
00589 *        +----------------------------------------+               GBIEPGM 
00590                                                                   GBIEPGM 
00591      IF  IEEDTXL > 0                                              GBIEPGM 
00592          IF  IEEDTXO  IS NUMERIC                                  GBIEPGM 
00593              MOVE IEEDTXO TO HGADATE-DATE1                        GBIEPGM 
00594              PERFORM 9200-000-GREGORIAN-TO-JULIAN                 GBIEPGM 
00595              IF  HGADATE-RETURN = ZEROS                           GBIEPGM 
00596                  MOVE '1'             TO WS-ENTERED-IEEDTX        GBIEPGM 
00597                  IF HGADATE-JULIAN2 < +70000                      GBIEPGM 
00598                     MOVE HEX-20 TO WS-EFFDT-CC                    GBIEPGM 
00599                     MOVE HGADATE-JULIAN2 TO WS-EFF-DT             GBIEPGM 
00600                   ELSE                                            GBIEPGM 
00601                     MOVE HEX-19 TO WS-EFFDT-CC                    GBIEPGM 
00602                     MOVE HGADATE-JULIAN2 TO WS-EFF-DT             GBIEPGM 
00603                   END-IF                                          GBIEPGM 
00604                  MOVE IEEDTXO          TO GI2-EFFECTIVE-DATE      GBIEPGM 
00605              ELSE                                                 GBIEPGM 
00606                  MOVE -1        TO IEEDTXL                        GBIEPGM 
00607                  MOVE DFHBMUBF  TO IEEDTXA                        GBIEPGM 
00608                  MOVE '1'       TO WS-ERROR-SWITCH                GBIEPGM 
00609                  MOVE 'INVALID EFFECTIVE DATE'                    GBIEPGM 
00610                    TO IEMSGO(1)                                   GBIEPGM 
00611          ELSE                                                     GBIEPGM 
00612              MOVE -1        TO IEEDTXL                            GBIEPGM 
00613              MOVE DFHBMUBF  TO IEEDTXA                            GBIEPGM 
00614              MOVE '1'       TO WS-ERROR-SWITCH                    GBIEPGM 
00615              MOVE 'EFFECTIVE DATE MUST BE NUMERIC'                GBIEPGM 
00616                TO IEMSGO(1)                                       GBIEPGM 
00617      ELSE                                                         GBIEPGM 
00618          NEXT SENTENCE.                                           GBIEPGM 
00619                                                                   GBIEPGM 
00620 *        +----------------------------------------+               GBIEPGM 
00621 *        +  FAMILY RELATIONSHIP LEVEL             +               GBIEPGM 
00622 *        +----------------------------------------+               GBIEPGM 
00623                                                                   GBIEPGM 
00624      IF  IEFRXL > 0                                               GBIEPGM 
00625          MOVE IEFRXO TO WS-CLASS-TEST-AREA                        GBIEPGM 
00626          IF  WS-CLASS-ALPHANUMERIC(1) AND                         GBIEPGM 
00627              WS-CLASS-ALPHANUMERIC(2)                             GBIEPGM 
00628              MOVE '1'    TO WS-ENTERED-IEFRX                      GBIEPGM 
00629              MOVE IEFRXO TO WS-FAM-REL-LVL                        GBIEPGM 
00630          ELSE                                                     GBIEPGM 
00631              MOVE -1        TO IEFRXL                             GBIEPGM 
00632              MOVE DFHBMUBF  TO IEFRXA                             GBIEPGM 
00633              MOVE '1'       TO WS-ERROR-SWITCH                    GBIEPGM 
00634              MOVE 'FAMILY RELATIONSHIP MUST BE ALPHANUMERIC'      GBIEPGM 
00635                TO IEMSGO(1)                                       GBIEPGM 
00636      ELSE                                                         GBIEPGM 
00637          NEXT SENTENCE.                                           GBIEPGM 
00638                                                                   GBIEPGM 
00639 *        +----------------------------------------+               GBIEPGM 
00640 *        +  PKG  CODE                             +               GBIEPGM 
00641 *        +----------------------------------------+               GBIEPGM 
00642                                                                   GBIEPGM 
00643      IF  IEPKGXL > 0                                              GBIEPGM 
00644          MOVE IEPKGXO TO WS-CLASS-TEST-AREA                       GBIEPGM 
00645          IF  WS-CLASS-ALPHANUMERIC(1) AND                         GBIEPGM 
00646              WS-CLASS-ALPHANUMERIC(2) AND                         GBIEPGM 
00647              WS-CLASS-ALPHANUMERIC(3)                             GBIEPGM 
00648              MOVE '1'     TO WS-ENTERED-IEPKGX                    GBIEPGM 
00649              MOVE IEPKGXO TO WS-PKG-CODE                          GBIEPGM 
00650          ELSE                                                     GBIEPGM 
00651              MOVE -1        TO IEPKGXL                            GBIEPGM 
00652              MOVE DFHBMUBF  TO IEPKGXA                            GBIEPGM 
00653              MOVE '1'       TO WS-ERROR-SWITCH                    GBIEPGM 
00654              MOVE 'INVALID PACKAGE CODE, MUST BE ALPHANUMERIC'    GBIEPGM 
00655                TO IEMSGO(1)                                       GBIEPGM 
00656      ELSE                                                         GBIEPGM 
00657 *****IF PROGRAM IS IN ILLINOIS REGION, DEFAULT PACKAGE CODE       GBIEPGM 
00658 *****TO ZEROES IF GROUP AND SECTION WERE ENTERED                  GBIEPGM 
00659          EXEC CICS                                                GBIEPGM 
00660               ASSIGN                                              GBIEPGM 
00661               SYSID(WS-SYSID)                                     GBIEPGM 
00662          END-EXEC                                                 GBIEPGM 
00663          IF WS-TEXAS-CICS-REGION                                  GBIEPGM 
00664              MOVE -1        TO IEPKGXL                            GBIEPGM 
00665              MOVE DFHBMUBF  TO IEPKGXA                            GBIEPGM 
00666              MOVE '1'       TO WS-ERROR-SWITCH                    GBIEPGM 
00667              MOVE 'PACKAGE CODE IS A REQUIRED FIELD'              GBIEPGM 
00668                TO IEMSGO(1)                                       GBIEPGM 
00669          ELSE                                                     GBIEPGM 
00670              IF IESECXL > 0 AND IEGRPXL > 0                       GBIEPGM 
00671                  MOVE +3      TO IEPKGXL                          GBIEPGM 
00672                  MOVE ZEROES  TO IEPKGXO                          GBIEPGM 
00673                  MOVE '1'     TO WS-ENTERED-IEPKGX                GBIEPGM 
00674                  MOVE IEPKGXO TO WS-PKG-CODE                      GBIEPGM 
00675              ELSE                                                 GBIEPGM 
00676                  MOVE -1        TO IEPKGXL                        GBIEPGM 
00677                  MOVE DFHBMUBF  TO IEPKGXA                        GBIEPGM 
00678                  MOVE '1'       TO WS-ERROR-SWITCH                GBIEPGM 
00679                  MOVE 'PACKAGE CODE IS A REQUIRED FIELD'          GBIEPGM 
00680                    TO IEMSGO(1).                                  GBIEPGM 
00681 *        +----------------------------------------+               GBIEPGM 
00682 *        +  SECTION                               +               GBIEPGM 
00683 *        +----------------------------------------+               GBIEPGM 
00684                                                                   GBIEPGM 
00685        IF  IESECXL > 0                                            GBIEPGM 
00686          INSPECT IESECXO REPLACING  ALL ' '  BY LOW-VALUES        GBIEPGM 
00687            MOVE IESECXO TO WS-CLASS-TEST-AREA                     GBIEPGM 
00688            IF  WS-CLASS-ALPHANUMERIC(1) AND                       GBIEPGM 
00689                WS-CLASS-ALPHANUMERIC(2) AND                       GBIEPGM 
00690                WS-CLASS-ALPHANUMERIC(3) AND                       GBIEPGM 
00691                WS-CLASS-ALPHANUMERIC(4) AND                       GBIEPGM 
00692                WS-CLASS-ALPHANUMERIC(5)                           GBIEPGM 
00693                MOVE '1'     TO WS-ENTERED-IESECX                  GBIEPGM 
00694                MOVE IESECXO TO WS-SECTION-NUM                     GBIEPGM 
00695            ELSE                                                   GBIEPGM 
00696                MOVE -1        TO IESECXL                          GBIEPGM 
00697                MOVE DFHBMUBF  TO IESECXA                          GBIEPGM 
00698                MOVE '1'       TO WS-ERROR-SWITCH                  GBIEPGM 
00699                MOVE 'INVALID SECTION, MUST BE ALPHANUMERIC'       GBIEPGM 
00700                  TO IEMSGO(1)                                     GBIEPGM 
00701        ELSE                                                       GBIEPGM 
00702            MOVE -1        TO IESECXL                              GBIEPGM 
00703            MOVE DFHBMUBF  TO IESECXA                              GBIEPGM 
00704            MOVE '1'       TO WS-ERROR-SWITCH                      GBIEPGM 
00705            MOVE 'SECTION NUMBER IS A REQUIRED FIELD'              GBIEPGM 
00706              TO IEMSGO(1).                                        GBIEPGM 
00707                                                                   GBIEPGM 
00708 *        +----------------------------------------+               GBIEPGM 
00709 *        +  GROUP                                 +               GBIEPGM 
00710 *        +----------------------------------------+               GBIEPGM 
00711                                                                   GBIEPGM 
00712      IF  IEGRPXL > 0                                              GBIEPGM 
00713        INSPECT IEGRPXO REPLACING ALL ' ' BY LOW-VALUES            GBIEPGM 
00714          MOVE IEGRPXO TO WS-CLASS-TEST-AREA                       GBIEPGM 
00715          IF  WS-CLASS-ALPHANUMERIC(1) AND                         GBIEPGM 
00716              WS-CLASS-ALPHANUMERIC(2) AND                         GBIEPGM 
00717              WS-CLASS-ALPHANUMERIC(3) AND                         GBIEPGM 
00718              WS-CLASS-ALPHANUMERIC(4) AND                         GBIEPGM 
00719              WS-CLASS-ALPHANUMERIC(5) AND                         GBIEPGM 
00720              WS-CLASS-ALPHANUMERIC(6) AND                         GBIEPGM 
00721              WS-CLASS-ALPHANUMERIC(7) AND                         GBIEPGM 
00722              WS-CLASS-ALPHANUMERIC(8) AND                         GBIEPGM 
00723              WS-CLASS-ALPHANUMERIC(9)                             GBIEPGM 
00724              MOVE '1'     TO WS-ENTERED-IEGRPX                    GBIEPGM 
00725              MOVE IEGRPXO TO WS-GROUP-NUM                         GBIEPGM 
00726          ELSE                                                     GBIEPGM 
00727              MOVE -1        TO IEGRPXL                            GBIEPGM 
00728              MOVE DFHBMUBF  TO IEGRPXA                            GBIEPGM 
00729              MOVE '1'       TO WS-ERROR-SWITCH                    GBIEPGM 
00730              MOVE 'INVALID GROUP, MUST BE ALPHANUMERIC'           GBIEPGM 
00731                TO IEMSGO(1)                                       GBIEPGM 
00732      ELSE                                                         GBIEPGM 
00733          MOVE -1        TO IEGRPXL                                GBIEPGM 
00734          MOVE DFHBMUBF  TO IEGRPXA                                GBIEPGM 
00735          MOVE '1'       TO WS-ERROR-SWITCH                        GBIEPGM 
00736          MOVE 'GROUP NUMBER IS A REQUIRED FIELD'                  GBIEPGM 
00737            TO IEMSGO(1).                                          GBIEPGM 
00738                                                                   GBIEPGM 
00739 *        +----------------------------------------+               GBIEPGM 
00740 *        +  PLAN CODE                             +               GBIEPGM 
00741 *        +----------------------------------------+               GBIEPGM 
00742                                                                   GBIEPGM 
00743      IF  IEPLNXL > 0                                              GBIEPGM 
00744          MOVE IEPLNXO TO WS-CLASS-TEST-AREA                       GBIEPGM 
00745          IF  WS-CLASS-ALPHANUMERIC(1) AND                         GBIEPGM 
00746              WS-CLASS-ALPHANUMERIC(2) AND                         GBIEPGM 
00747              WS-CLASS-ALPHANUMERIC(3)                             GBIEPGM 
00748              MOVE '1'     TO WS-ENTERED-IEPLNX                    GBIEPGM 
00749              MOVE IEPLNXO TO WS-PLAN-CODE                         GBIEPGM 
00750          ELSE                                                     GBIEPGM 
00751              MOVE -1        TO IEPLNXL                            GBIEPGM 
00752              MOVE DFHBMUBF  TO IEPLNXA                            GBIEPGM 
00753              MOVE '1'       TO WS-ERROR-SWITCH                    GBIEPGM 
00754              MOVE 'INVALID PLAN CODE, MUST BE ALPHANUMERIC'       GBIEPGM 
00755                TO IEMSGO(1)                                       GBIEPGM 
00756      ELSE                                                         GBIEPGM 
00757          MOVE -1        TO IEPLNXL                                GBIEPGM 
00758          MOVE DFHBMUBF  TO IEPLNXA                                GBIEPGM 
00759          MOVE '1'       TO WS-ERROR-SWITCH                        GBIEPGM 
00760          MOVE 'PLAN CODE IS A REQUIRED FIELD'                     GBIEPGM 
00761            TO IEMSGO(1).                                          GBIEPGM 
00762                                                                   GBIEPGM 
00763                                                                   GBIEPGM 
00764 *        +----------------------------------------+               GBIEPGM 
00765 *        +  GROUP SPECIFIC LINE NUMBER SELECTION  +               GBIEPGM 
00766 *        +----------------------------------------+               GBIEPGM 
00767                                                                   GBIEPGM 
00768      IF  NOT WS-FULL-GRPSPC-KEY-ENTERED AND                       GBIEPGM 
00769              WS-ERROR-SWITCH   = '0'      AND                     GBIEPGM 
00770              IELNCNTO          >  00      AND                     GBIEPGM 
00771 ***          WS-ENTERED-IEOPT  = '1'      AND                     GBIEPGM 
00772              WS-ENTERED-IELNNO = '0'                              GBIEPGM 
00773          MOVE -1        TO IELNNOL                                GBIEPGM 
00774          MOVE DFHBMUBF  TO IELNNOA                                GBIEPGM 
00775          MOVE '1'       TO WS-ERROR-SWITCH                        GBIEPGM 
00776          MOVE 'GROUP SPECIFIC LINE NUMBER REQUIRED'               GBIEPGM 
00777            TO IEMSGO(1)                                           GBIEPGM 
00778      ELSE                                                         GBIEPGM 
00779          NEXT SENTENCE.                                           GBIEPGM 
00780                                                                   GBIEPGM 
00781                                                                   GBIEPGM 
00782 *        +-----------------------------------------------+        GBIEPGM 
00783 *        +                                               +        GBIEPGM 
00784 *        +  IF A PARTIAL GROUP SPECIFIC KEY HAS BEEN     +        GBIEPGM 
00785 *        +    KEYED AND THERE IS A SELECTION LIST        +        GBIEPGM 
00786 *        +    ALREADY ON THE SCREEN AND THE              +        GBIEPGM 
00787 *        +    OPERATOR HAS MADE A VALID SELECTION,       +        GBIEPGM 
00788 *        +    THEN COMPLETE THE FULL GROUP SPECIFIC KEY  +        GBIEPGM 
00789 *        +    WITH THE SELECTED INFORMATION, SET         +        GBIEPGM 
00790 *        +    ALL SCREEN-KEY-ENTRY-SWITCHES TO 1'S       +        GBIEPGM 
00791 *        +    INDICATING A FULL GROUP SPECIFIC KEY HAS   +        GBIEPGM 
00792 *        +    BEEN ENTERED.                              +        GBIEPGM 
00793 *        +                                               +        GBIEPGM 
00794 *        +-----------------------------------------------+        GBIEPGM 
00795                                                                   GBIEPGM 
00796      IF  NOT WS-FULL-GRPSPC-KEY-ENTERED AND                       GBIEPGM 
00797              WS-ERROR-SWITCH   = '0'      AND                     GBIEPGM 
00798              IELNCNTO          >  00      AND                     GBIEPGM 
00799 ****         WS-ENTERED-IEOPT  = '1'      AND                     GBIEPGM 
00800              WS-ENTERED-IELNNO = '1'                              GBIEPGM 
00801          MOVE IELNNOO              TO WS-SCREEN-LN                GBIEPGM 
00802          MOVE IEFRO (WS-SCREEN-LN) TO WS-FAM-REL-LVL              GBIEPGM 
00803          MOVE IEEDTO(WS-SCREEN-LN) TO HGADATE-DATE1               GBIEPGM 
00804                                       GI2-EFFECTIVE-DATE          GBIEPGM 
00805          PERFORM 9200-000-GREGORIAN-TO-JULIAN                     GBIEPGM 
00806          IF HGADATE-JULIAN2 < +70000                              GBIEPGM 
00807             MOVE HEX-20              TO WS-EFFDT-CC               GBIEPGM 
00808             MOVE HGADATE-JULIAN2     TO WS-EFF-DT                 GBIEPGM 
00809          ELSE                                                     GBIEPGM 
00810             MOVE HEX-19              TO WS-EFFDT-CC               GBIEPGM 
00811             MOVE HGADATE-JULIAN2     TO WS-EFF-DT                 GBIEPGM 
00812          END-IF                                                   GBIEPGM 
00813           MOVE ALL '1'             TO WS-SCREEN-KEY-ENTRY-SWITCHESGBIEPGM 
00814      ELSE                                                         GBIEPGM 
00815          NEXT SENTENCE.                                           GBIEPGM 
00816                                                                   GBIEPGM 
00817 *        +----------------------------------------+               GBIEPGM 
00818 *        + IF THERE HAS NOT BEEN ANY ERRORS FOUND +               GBIEPGM 
00819 *        + CONTINUE PROCESSING,  ELSE SEND THE    +               GBIEPGM 
00820 *        + ERROR MESSAGE AND RETURN TO CICS.      +               GBIEPGM 
00821 *        +----------------------------------------+               GBIEPGM 
00822                                                                   GBIEPGM 
00823      IF  WS-ERROR-SWITCH = '0'                                    GBIEPGM 
00824          NEXT SENTENCE                                            GBIEPGM 
00825      ELSE                                                         GBIEPGM 
00826          PERFORM 7500-000-SEND-DATAONLY                           GBIEPGM 
00827          GO TO 2000-900-EXIT.                                     GBIEPGM 
00828                                                                   GBIEPGM 
00829 *        +----------------------------------------+               GBIEPGM 
00830 *        + IF A FULL GROUP SPECIFIC KEY HAS BEEN  +               GBIEPGM 
00831 *        + ENTERED READ THE CONTRACT AND XCTL TO  +               GBIEPGM 
00832 *        + THE CHOOSEN SCREEN MODULE.             +               GBIEPGM 
00833 *        +----------------------------------------+               GBIEPGM 
00834                                                                   GBIEPGM 
00835      IF  WS-FULL-GRPSPC-KEY-ENTERED                               GBIEPGM 
00836          PERFORM 2100-000-FULL-KEY-SELECTED                       GBIEPGM 
00837          GO TO 2000-900-EXIT.                                     GBIEPGM 
00838                                                                   GBIEPGM 
00839 *        +----------------------------------------+               GBIEPGM 
00840 *        + IF A PARTIAL GROUP SPECIFIC KEY HAS    +               GBIEPGM 
00841 *        + BEEN ENTERED, PROCESS IT.              +               GBIEPGM 
00842 *        +----------------------------------------+               GBIEPGM 
00843                                                                   GBIEPGM 
00844      PERFORM 2200-000-PARTIAL-KEY-SELECTED.                       GBIEPGM 
00845      GO TO 2000-900-EXIT.                                         GBIEPGM 
00846                                                                   GBIEPGM 
00847  2000-900-EXIT.                                                   GBIEPGM 
00848      EXIT.                                                        GBIEPGM 
00849 /*****************************************************************GBIEPGM 
00850 **                                                               *GBIEPGM 
00851 ** 2100     FULL KEY SELECTED                                    *GBIEPGM 
00852 **                                                               *GBIEPGM 
00853 **       1. READ THE GROUP SPECIFIC                              *GBIEPGM 
00854 **       2. XCTL TO THE SELECTED SCREEN MODULE.                  *GBIEPGM 
00855 **                                                               *GBIEPGM 
00856 ******************************************************************GBIEPGM 
00857  2100-000-FULL-KEY-SELECTED    SECTION.                           GBIEPGM 
00858  2100-010.                                                        GBIEPGM 
00859                                                                   GBIEPGM 
00860      MOVE '2100'  TO  WS-PARA-ID.                                 GBIEPGM 
00861                                                                   GBIEPGM 
00862      PERFORM 8000-000-READ-GRPSPC.                                GBIEPGM 
00863                                                                   GBIEPGM 
00864      IF  NOT GCIO-GOOD-RETURN                                     GBIEPGM 
00865          MOVE  SPACES  TO  IEPAGEO                                GBIEPGM 
00866          MOVE  -1      TO  IEPLNXL                                GBIEPGM 
00867          MOVE '*** GROUP SPECIFIC RECORD NOT FOUND ****'          GBIEPGM 
00868            TO IEMSGO(1)                                           GBIEPGM 
00869          MOVE '1'                  TO WS-ERROR-SWITCH             GBIEPGM 
00870          PERFORM 7500-000-SEND-DATAONLY                           GBIEPGM 
00871          GO TO 2100-900-EXIT                                      GBIEPGM 
00872      ELSE                                                         GBIEPGM 
00873          PERFORM 2100-020-XCTL-TO-GBID                            GBIEPGM 
00874      END-IF.                                                      GBIEPGM 
00875                                                                   GBIEPGM 
00876  2100-020-XCTL-TO-GBID.                                           GBIEPGM 
00877                                                                   GBIEPGM 
00878      MOVE 'GBIDPGM' TO WS-XCTL-TO-PGM                             GBIEPGM 
00879                                                                   GBIEPGM 
00880      MOVE  GCG-PLAN-CODE    TO   GIG2-PLAN-CODE                   GBIEPGM 
00881      MOVE  GCG-GROUP-NUM    TO   GIG2-GROUP-NUM                   GBIEPGM 
00882      MOVE  GCG-SECTION-NUM  TO   GIG2-SECTION-NUM                 GBIEPGM 
00883      MOVE  GCG-PKG-CODE     TO   GIG2-PKG-CODE                    GBIEPGM 
00884      MOVE  GCG-FAM-REL-LVL  TO   GIG2-FAM-REL-LVL                 GBIEPGM 
00885      MOVE  GCG-EFFDT-CEN    TO   GIG2-EFFDT-CEN.                  GBIEPGM 
00886                                                                   GBIEPGM 
00887      MOVE IECNKEDO TO HGADATE-DATE1                               GBIEPGM 
00888      PERFORM 9200-000-GREGORIAN-TO-JULIAN                         GBIEPGM 
00889      IF  HGADATE-RETURN = ZEROS                                   GBIEPGM 
00890          IF HGADATE-JULIAN2 < +70000                              GBIEPGM 
00891             MOVE HEX-20 TO WS-EFFDT-CC                            GBIEPGM 
00892             MOVE HGADATE-JULIAN2 TO WS-EFF-DT                     GBIEPGM 
00893          ELSE                                                     GBIEPGM 
00894             MOVE HEX-19 TO WS-EFFDT-CC                            GBIEPGM 
00895             MOVE HGADATE-JULIAN2 TO WS-EFF-DT                     GBIEPGM 
00896          END-IF                                                   GBIEPGM 
00897      END-IF                                                       GBIEPGM 
00898      MOVE IECNKEYO             TO WS-MAP-CONT-KEY-1               GBIEPGM 
00899      MOVE WS-EFFDT-CEN         TO WS-MAP-CONT-KEY-2               GBIEPGM 
00900      MOVE WS-MAP-CONTRACT-KEY  TO  GIC2-CONTRACT-ID.              GBIEPGM 
00901      MOVE IESUBO               TO GI2-SUBSCRIBER.                 GBIEPGM 
00902      MOVE IERETCO              TO GI2-RETURN-CODE                 GBIEPGM 
00903      MOVE IESVDTO              TO GIGT2-SLOT-NUMBER.              GBIEPGM 
00904                                                                   GBIEPGM 
00905      EXEC CICS XCTL PROGRAM (WS-XCTL-TO-PGM)                      GBIEPGM 
00906                     COMMAREA(GI-COMMAREA2-RECORD)                 GBIEPGM 
00907                     LENGTH  (WS-COMM-KEY-LEN)                     GBIEPGM 
00908                     END-EXEC.                                     GBIEPGM 
00909                                                                   GBIEPGM 
00910 *        MOVE  GCG-GROUP-NUM    TO   WS-DISP-GROUP                GBIEPGM 
00911 *        MOVE  GCG-SECTION-NUM  TO   WS-DISP-SECTION              GBIEPGM 
00912 *        MOVE  GCG-PKG-CODE     TO   WS-DISP-PKG                  GBIEPGM 
00913 *        MOVE  GCG-FAM-REL-LVL  TO   WS-DISP-FRL                  GBIEPGM 
00914 *        MOVE  GCG-EFFDT-CEN    TO   WS-DISP-EFFDT                GBIEPGM 
00915 *        MOVE  GCG-TERMDT-CEN   TO   WS-DISP-TRMDT                GBIEPGM 
00916 *        MOVE  WS-DISP-GROUPSPC TO   IEMSGO(2)                    GBIEPGM 
00917 *        MOVE '1'                  TO WS-ERROR-SWITCH             GBIEPGM 
00918 *        MOVE  -1            TO  IELNNOL                          GBIEPGM 
00919 *        PERFORM 7500-000-SEND-DATAONLY                           GBIEPGM 
00920 *        GO TO 2100-900-EXIT.                                     GBIEPGM 
00921                                                                   GBIEPGM 
00922  2100-900-EXIT.                                                   GBIEPGM 
00923      EXIT.                                                        GBIEPGM 
00924 /*****************************************************************GBIEPGM 
00925 **                                                               *GBIEPGM 
00926 ** 2200     PARTIAL KEY SELECTED                                 *GBIEPGM 
00927 **                                                               *GBIEPGM 
00928 ******************************************************************GBIEPGM 
00929  2200-000-PARTIAL-KEY-SELECTED SECTION.                           GBIEPGM 
00930  2200-010.                                                        GBIEPGM 
00931                                                                   GBIEPGM 
00932      MOVE '2200'  TO  WS-PARA-ID.                                 GBIEPGM 
00933                                                                   GBIEPGM 
00934      MOVE IEPLNXO     TO WS-GCDATES-PLAN-CODE.                    GBIEPGM 
00935      MOVE IEGRPXO     TO WS-GCDATES-GRP-NUM.                      GBIEPGM 
00936      MOVE IESECXO     TO WS-GCDATES-SECTN-NUM.                    GBIEPGM 
00937      MOVE IEPKGXO     TO WS-GCDATES-PKG-CODE.                     GBIEPGM 
00938      MOVE LOW-VALUES  TO WS-GCDATES-KEY-FILLER.                   GBIEPGM 
00939      MOVE 'G'         TO WS-GCDATES-FILE-REF-IND.                 GBIEPGM 
00940                                                                   GBIEPGM 
00941 *        +----------------------------------------+               GBIEPGM 
00942 *        +   RETRIEVE GCDATES RECORD              +               GBIEPGM 
00943 *        +----------------------------------------+               GBIEPGM 
00944                                                                   GBIEPGM 
00945      PERFORM 8100-000-READ-GCDATES.                               GBIEPGM 
00946      IF  GCIO2-GOOD-RETURN                                        GBIEPGM 
00947 *PFH     MOVE GCDATES-PNTR1   TO WS-GCDATES-ADDR                  GBIEPGM 
00948          MOVE DTE-ENTRY-COUNT TO WS-GCDATES-ENTRY-COUNT           GBIEPGM 
00949      ELSE                                                         GBIEPGM 
00950 *PFH     MOVE ZEROS           TO WS-GCDATES-ADDR                  GBIEPGM 
00951          MOVE ZEROS           TO WS-GCDATES-ENTRY-COUNT.          GBIEPGM 
00952                                                                   GBIEPGM 
00953 *        +----------------------------------------+               GBIEPGM 
00954 *        + REDUCE GCATES ENTRY COUNT BY 1         +               GBIEPGM 
00955 *        +   TO ACCOUNT FOR THE HIGH-VALUES ENTRY +               GBIEPGM 
00956 *        +   AT THE END OF EACH RECORD.           +               GBIEPGM 
00957 *        +----------------------------------------+               GBIEPGM 
00958                                                                   GBIEPGM 
00959 *-------- REDUCE GCDATES ENTRY COUNT BY 1 TO ACCOUNT FOR          GBIEPGM 
00960 *          THE HIGH-VALUES ENTRY AT THE END OF EACH RECORD        GBIEPGM 
00961                                                                   GBIEPGM 
00962      IF  WS-GCDATES-ENTRY-COUNT > +1                              GBIEPGM 
00963          COMPUTE WS-GCDATES-ENTRY-COUNT =                         GBIEPGM 
00964                  WS-GCDATES-ENTRY-COUNT - 1                       GBIEPGM 
00965      ELSE                                                         GBIEPGM 
00966 *PFH     MOVE ZEROS TO WS-GCDATES-ADDR                            GBIEPGM 
00967          MOVE ZEROS TO WS-GCDATES-ENTRY-COUNT.                    GBIEPGM 
00968                                                                   GBIEPGM 
00969 *        +----------------------------------------+               GBIEPGM 
00970 *        + CHECK TO SEE IF THERE IS A GCDATES     +               GBIEPGM 
00971 *        +   RECORD  TO PROCESS.                  +               GBIEPGM 
00972 *        +----------------------------------------+               GBIEPGM 
00973                                                                   GBIEPGM 
00974      IF  WS-GCDATES-ENTRY-COUNT = ZEROS                           GBIEPGM 
00975          MOVE  SPACES  TO  IEPAGEO                                GBIEPGM 
00976          MOVE  -1      TO  IEPLNXL                                GBIEPGM 
00977          MOVE '*** NO GROUP SPECIFIC DATES RECORD FOUND FOR THIS PGBIEPGM 
00978 -             'LAN, GRP, & SECTION ***'                           GBIEPGM 
00979            TO IEMSGO(1)                                           GBIEPGM 
00980          MOVE ZEROES  TO  IELNCNTO                                GBIEPGM 
00981 ***********   AHL 11/25/86                                        GBIEPGM 
00982          PERFORM 2800-000-CLEAR-MAP-TABLE                         GBIEPGM 
00983                  VARYING WS-CLEAR-INDEX FROM 1 BY 1               GBIEPGM 
00984                  UNTIL   WS-CLEAR-INDEX > WS-SCREEN-MAX-ENTRIES   GBIEPGM 
00985 **************************                                        GBIEPGM 
00986          PERFORM 7600-000-SEND-ERASE                              GBIEPGM 
00987          GO TO 2200-900-EXIT                                      GBIEPGM 
00988      ELSE                                                         GBIEPGM 
00989          NEXT SENTENCE.                                           GBIEPGM 
00990                                                                   GBIEPGM 
00991 *        +----------------------------------------+               GBIEPGM 
00992 *        + COMPUTE THE GETMAIN LENGTH FOR THE     +               GBIEPGM 
00993 *        +   DATES TABLE TO BE BUILD FROM THE     +               GBIEPGM 
00994 *        +   GCDATES RECORD(S).  ACQUIRE THE      +               GBIEPGM 
00995 *        +   STORAGE AND SET THE ENTRY COUNT.     +               GBIEPGM 
00996 *        +----------------------------------------+               GBIEPGM 
00997                                                                   GBIEPGM 
00998      COMPUTE  WS-DATES-TABLE-LEN           =                      GBIEPGM 
00999               WS-DT-FIXED-PORTION          +                      GBIEPGM 
01000             ( WS-DT-VARIABLE-PORTION       *                      GBIEPGM 
01001               WS-GCDATES-ENTRY-COUNT).                            GBIEPGM 
01002                                                                   GBIEPGM 
01003      EXEC CICS GETMAIN  SET (ADDRESS OF DATES-TABLE)              GBIEPGM 
01004                         INITIMG(WS-HEX-00)                        GBIEPGM 
01005                         LENGTH (WS-DATES-TABLE-LEN)               GBIEPGM 
01006                         END-EXEC.                                 GBIEPGM 
01007                                                                   GBIEPGM 
01008      COMPUTE  DT-ENTRY-COUNT               =                      GBIEPGM 
01009               WS-GCDATES-ENTRY-COUNT.                             GBIEPGM 
01010                                                                   GBIEPGM 
01011      MOVE +0 TO WS-DT-SUB                                         GBIEPGM 
01012                 WS-DT-SUBX                                        GBIEPGM 
01013                                                                   GBIEPGM 
01014 *        +----------------------------------------+               GBIEPGM 
01015 *        + LOAD DATES TABLE USING GCATES RECORD.  +               GBIEPGM 
01016 *        +----------------------------------------+               GBIEPGM 
01017                                                                   GBIEPGM 
01018      PERFORM 2300-000-LOAD-DATES-TABLE                            GBIEPGM 
01019             VARYING DTE-INDEX FROM 1 BY 1                         GBIEPGM 
01020               UNTIL DTE-INDEX > WS-GCDATES-ENTRY-COUNT.           GBIEPGM 
01021                                                                   GBIEPGM 
01022 *        +----------------------------------------+               GBIEPGM 
01023 *        +  SORT DATES TABLE:                     +               GBIEPGM 
01024 *        +    DESCENDING ON EFFECTIVE DATE        +               GBIEPGM 
01025 *        +    ASCENDING ON FAM REL.               +               GBIEPGM 
01026 *        +----------------------------------------+               GBIEPGM 
01027                                                                   GBIEPGM 
01028 **   IF  DT-ENTRY-COUNT < +2                                      GBIEPGM 
01029      IF  WS-DT-SUBX     < +2                                      GBIEPGM 
01030          GO TO 2200-100-NO-SORT-REQUIRED.                         GBIEPGM 
01031                                                                   GBIEPGM 
01032      PERFORM 2400-000-SORT-DATES-TABLE                            GBIEPGM 
01033         VARYING WS-DT-SUB FROM 1 BY 1                             GBIEPGM 
01034           UNTIL WS-DT-SUB > DT-ENTRY-COUNT                        GBIEPGM 
01035              OR WS-SORT-EXCHANGE-NOT-MADE.                        GBIEPGM 
01036                                                                   GBIEPGM 
01037  2200-100-NO-SORT-REQUIRED.                                       GBIEPGM 
01038                                                                   GBIEPGM 
01039 *        +----------------------------------------+               GBIEPGM 
01040 *        +  CHANGE EFFECTIVE DATES IN DATES TABLE +               GBIEPGM 
01041 *        +   BACK FROM COMPIMENTED(99999 - JULIAN)+               GBIEPGM 
01042 *        +    TO NORMAL JULIAN.                   +               GBIEPGM 
01043 *        +----------------------------------------+               GBIEPGM 
01044                                                                   GBIEPGM 
01045      PERFORM 2500-000-UNCOMPLIMENT-EFF-DATE                       GBIEPGM 
01046         VARYING WS-DT-SUB FROM 1 BY 1                             GBIEPGM 
01047 *****     UNTIL WS-DT-SUB > DT-ENTRY-COUNT.                       GBIEPGM 
01048           UNTIL WS-DT-SUB > WS-DT-SUBX.                           GBIEPGM 
01049                                                                   GBIEPGM 
01050                                                                   GBIEPGM 
01051 *        +----------------------------------------+               GBIEPGM 
01052 *        +        PAGE BACKWARD FUNCTION          +               GBIEPGM 
01053 *        +----------------------------------------+               GBIEPGM 
01054      IF EIBAID  =   DFHPF7     OR   DFHPF19                       GBIEPGM 
01055         MOVE  +1  TO   WS-SCREEN-SUB                              GBIEPGM 
01056         IF IELNI (WS-SCREEN-SUB)  >  ZEROES                       GBIEPGM 
01057           IF IEGRPXI    =    IEGROUPI                             GBIEPGM 
01058             IF IESECXI   =    IESECI                              GBIEPGM 
01059                PERFORM 2225-000-PAGE-BACKWARD                     GBIEPGM 
01060                MOVE  WS-PAGE-NAME  TO  IEPNAMEO                   GBIEPGM 
01061                MOVE  -1            TO  IELNNOL                    GBIEPGM 
01062                GO TO 2200-700-SEND-SCREEN                         GBIEPGM 
01063             ELSE                                                  GBIEPGM 
01064                NEXT SENTENCE                                      GBIEPGM 
01065           ELSE                                                    GBIEPGM 
01066              NEXT SENTENCE                                        GBIEPGM 
01067         ELSE                                                      GBIEPGM 
01068            MOVE  SPACES  TO  IEPNAMEO                             GBIEPGM 
01069            MOVE  -1      TO  IELNNOL                              GBIEPGM 
01070            MOVE '    PRESS   ENTER   TO DISPLAY GROUP SPECIFIC KEYGBIEPGM 
01071 -               'S'                                               GBIEPGM 
01072              TO IEMSGO(1)                                         GBIEPGM 
01073            GO TO 2200-700-SEND-SCREEN.                            GBIEPGM 
01074                                                                   GBIEPGM 
01075                                                                   GBIEPGM 
01076 *        +----------------------------------------+               GBIEPGM 
01077 *        +        PAGE FORWARD FUNCTION           +               GBIEPGM 
01078 *        +----------------------------------------+               GBIEPGM 
01079      IF EIBAID  =  DFHPF8     OR   DFHPF20                        GBIEPGM 
01080         MOVE  +1  TO   WS-SCREEN-SUB                              GBIEPGM 
01081         IF IELNI (WS-SCREEN-SUB)  >  ZEROES                       GBIEPGM 
01082           IF IEGRPXI    =    IEGROUPI                             GBIEPGM 
01083             IF IESECXI   =    IESECI                              GBIEPGM 
01084                PERFORM 2250-000-PAGE-FORWARD                      GBIEPGM 
01085                MOVE  WS-PAGE-NAME  TO  IEPNAMEO                   GBIEPGM 
01086                MOVE  -1            TO  IELNNOL                    GBIEPGM 
01087                GO TO 2200-700-SEND-SCREEN                         GBIEPGM 
01088             ELSE                                                  GBIEPGM 
01089                NEXT SENTENCE                                      GBIEPGM 
01090           ELSE                                                    GBIEPGM 
01091              NEXT SENTENCE                                        GBIEPGM 
01092         ELSE                                                      GBIEPGM 
01093            MOVE  SPACES  TO  IEPNAMEO                             GBIEPGM 
01094            MOVE  -1      TO  IELNNOL                              GBIEPGM 
01095            MOVE '    PRESS   ENTER   TO DISPLAY GROUP SPECIFIC KEYGBIEPGM 
01096 -               'S'                                               GBIEPGM 
01097              TO IEMSGO (1)                                        GBIEPGM 
01098            GO TO 2200-700-SEND-SCREEN.                            GBIEPGM 
01099                                                                   GBIEPGM 
01100                                                                   GBIEPGM 
01101                                                                   GBIEPGM 
01102 *        +----------------------------------------+               GBIEPGM 
01103 *        +  CLEAR TABLE ENTRIES ON SCREEN         +               GBIEPGM 
01104 *        +   (LN, FR AND EFDT                     +               GBIEPGM 
01105 *        +----------------------------------------+               GBIEPGM 
01106                                                                   GBIEPGM 
01107      PERFORM 2600-000-CLEAR-SCREEN-TABLE                          GBIEPGM 
01108         VARYING WS-SCREEN-SUB FROM 1 BY 1                         GBIEPGM 
01109           UNTIL WS-SCREEN-SUB > WS-SCREEN-MAX-ENTRIES.            GBIEPGM 
01110                                                                   GBIEPGM 
01111 *        +----------------------------------------+               GBIEPGM 
01112 *        +  BUILD SCREEN GROUP SPECIFIC SELECTION +               GBIEPGM 
01113 *        +   LIST FROM DATES TABLE.               +               GBIEPGM 
01114 *        +----------------------------------------+               GBIEPGM 
01115                                                                   GBIEPGM 
01116 * SAVE THE GROUP AND SECTION NUMBERS THAT WERE KEYED IN ON        GBIEPGM 
01117 * THE SCREEN.  SAVE THEM IN THE DARK FIELDS ON THE SCREEN,        GBIEPGM 
01118 * WHICH ARE REFERENCED WHEN PAGING FORWARD OR BACKWARDS.          GBIEPGM 
01119 *    MOVE IEPLNXI  TO  IEPLNPO.                                   GBIEPGM 
01120      MOVE IEGRPXI  TO  IEGROUPO.                                  GBIEPGM 
01121      MOVE IESECXI  TO  IESECO.                                    GBIEPGM 
01122                                                                   GBIEPGM 
01123                                                                   GBIEPGM 
01124      MOVE ZEROS TO WS-SCREEN-LN                                   GBIEPGM 
01125                    WS-SCREEN-SUB                                  GBIEPGM 
01126                    WS-DT-SUB.                                     GBIEPGM 
01127                                                                   GBIEPGM 
01128                                                                   GBIEPGM 
01129      PERFORM 2700-000-BUILD-SELECT-LIST                           GBIEPGM 
01130           UNTIL WS-SCREEN-SUB = WS-SCREEN-MAX-ENTRIES             GBIEPGM 
01131 ***          OR WS-SCREEN-SUB > DT-ENTRY-COUNT                    GBIEPGM 
01132              OR WS-SCREEN-SUB > WS-DT-SUBX                        GBIEPGM 
01133 ***          OR WS-DT-SUB     = DT-ENTRY-COUNT.                   GBIEPGM 
01134              OR WS-DT-SUB     = WS-DT-SUBX.                       GBIEPGM 
01135                                                                   GBIEPGM 
01136      IF  WS-SCREEN-SUB = 0                                        GBIEPGM 
01137          MOVE  SPACES  TO  IEPNAMEO                               GBIEPGM 
01138          MOVE  ZEROES  TO  IELNCNTO                               GBIEPGM 
01139          MOVE  -1      TO  IEPLNXL                                GBIEPGM 
01140          MOVE '*** NO GROUP SPECIFICS SELECTED BASED ON THE PARTIAGBIEPGM 
01141 -             'L KEY ENTERED ***'                                 GBIEPGM 
01142            TO IEMSGO(1)                                           GBIEPGM 
01143      ELSE                                                         GBIEPGM 
01144          MOVE  WS-PAGE-NAME      TO  IEPNAMEO                     GBIEPGM 
01145          ADD   1                 TO  WS-PAGE-COUNT                GBIEPGM 
01146          MOVE  WS-PAGE-COUNT-X   TO  IEPAGEO                      GBIEPGM 
01147          MOVE -1                 TO  IELNNOL                      GBIEPGM 
01148          MOVE '          CHOOSE GROUP SPECIFIC LINE NUMBER'       GBIEPGM 
01149            TO  IEMSGO (1)                                         GBIEPGM 
01150          MOVE '            ****   THIS IS THE FIRST PAGE   ****'  GBIEPGM 
01151            TO  IEMSGO (2).                                        GBIEPGM 
01152                                                                   GBIEPGM 
01153 ****  IF  DT-ENTRY-COUNT = +1                                     GBIEPGM 
01154       IF  WS-DT-SUBX     = +1                                     GBIEPGM 
01155           GO TO 2200-900-EXIT                                     GBIEPGM 
01156       END-IF.                                                     GBIEPGM 
01157                                                                   GBIEPGM 
01158                                                                   GBIEPGM 
01159  2200-700-SEND-SCREEN.                                            GBIEPGM 
01160      PERFORM 7600-000-SEND-ERASE.                                 GBIEPGM 
01161      GO TO 2200-900-EXIT.                                         GBIEPGM 
01162                                                                   GBIEPGM 
01163                                                                   GBIEPGM 
01164  2200-900-EXIT.                                                   GBIEPGM 
01165      EXIT.                                                        GBIEPGM 
01166 /*****************************************************************GBIEPGM 
01167 **                        PAGE BACKWARD                         **GBIEPGM 
01168 ******************************************************************GBIEPGM 
01169  2225-000-PAGE-BACKWARD    SECTION.                               GBIEPGM 
01170  2225-010.                                                        GBIEPGM 
01171                                                                   GBIEPGM 
01172      MOVE '2225'  TO  WS-PARA-ID.                                 GBIEPGM 
01173                                                                   GBIEPGM 
01174 ***  IF DT-ENTRY-COUNT   <   +31                                  GBIEPGM 
01175      IF WS-DT-SUBX       <   +31                                  GBIEPGM 
01176        IF IELNI (WS-SCREEN-SUB)   =   '01'                        GBIEPGM 
01177           PERFORM 2280-000-BRIGHT-FSET                            GBIEPGM 
01178             VARYING  WS-SCREEN-SUB   FROM  +1  BY  +1             GBIEPGM 
01179               UNTIL  WS-SCREEN-SUB   >   WS-SCREEN-MAX-ENTRIES    GBIEPGM 
01180                  OR  IELNI (WS-SCREEN-SUB)   =                    GBIEPGM 
01181                                         (LOW-VALUES   OR   SPACES)GBIEPGM 
01182            MOVE ' ***   SELECT OPTION AND CHOOSE GROUP SPECIFIC LIGBIEPGM 
01183 -             'NE NUMBER  ***'                                    GBIEPGM 
01184              TO  IEMSGO (1)                                       GBIEPGM 
01185            MOVE '            ****   THIS IS THE FIRST PAGE   ****'GBIEPGM 
01186              TO  IEMSGO (2)                                       GBIEPGM 
01187            GO TO 2225-900-EXIT.                                   GBIEPGM 
01188                                                                   GBIEPGM 
01189      PERFORM 2230-000-SAVE-FIRST-ENTRY.                           GBIEPGM 
01190                                                                   GBIEPGM 
01191      IF WS-ERROR-SWITCH  =   '1'                                  GBIEPGM 
01192         GO TO 2225-900-EXIT.                                      GBIEPGM 
01193                                                                   GBIEPGM 
01194      PERFORM 2240-000-BUILD-PREVIOUS-PAGE.                        GBIEPGM 
01195                                                                   GBIEPGM 
01196  2225-900-EXIT.                                                   GBIEPGM 
01197      EXIT.                                                        GBIEPGM 
01198 /*****************************************************************GBIEPGM 
01199 **         SAVE THE FIRST ENTRY FROM THE SCREEN TABLE            *GBIEPGM 
01200 ******************************************************************GBIEPGM 
01201  2230-000-SAVE-FIRST-ENTRY   SECTION.                             GBIEPGM 
01202  2230-010.                                                        GBIEPGM 
01203                                                                   GBIEPGM 
01204      MOVE '2230'  TO  WS-PARA-ID.                                 GBIEPGM 
01205                                                                   GBIEPGM 
01206      MOVE IELNI  (WS-SCREEN-SUB)   TO  WS-SAVE-FIRST-LN-NO.       GBIEPGM 
01207      MOVE IEFRI  (WS-SCREEN-SUB)   TO  WS-SAVE-FIRST-FAM-REL.     GBIEPGM 
01208                                                                   GBIEPGM 
01209 ** CONVERT EFFECTIVE-DATE FROM GREGORIAN TO JULIAN                GBIEPGM 
01210 **                                                                GBIEPGM 
01211      MOVE IEEDTI (WS-SCREEN-SUB)   TO  HGADATE-DATE1.             GBIEPGM 
01212      PERFORM 9200-000-GREGORIAN-TO-JULIAN.                        GBIEPGM 
01213                                                                   GBIEPGM 
01214      IF  HGADATE-RETURN   =   ZEROS                               GBIEPGM 
01215          IF HGADATE-JULIAN2 < +70000                              GBIEPGM 
01216             MOVE HEX-20           TO  WS-SAVE-FIRST-EFFDT-CC      GBIEPGM 
01217             MOVE HGADATE-JULIAN2  TO  WS-SAVE-FIRST-EFF-DT        GBIEPGM 
01218          ELSE                                                     GBIEPGM 
01219             MOVE HEX-19           TO  WS-SAVE-FIRST-EFFDT-CC      GBIEPGM 
01220             MOVE HGADATE-JULIAN2  TO  WS-SAVE-FIRST-EFF-DT        GBIEPGM 
01221          END-IF                                                   GBIEPGM 
01222      ELSE                                                         GBIEPGM 
01223          MOVE -1               TO  IEEDTL (WS-SCREEN-SUB)         GBIEPGM 
01224          MOVE DFHBMUBF         TO  IEEDTA (WS-SCREEN-SUB)         GBIEPGM 
01225          MOVE '1'              TO  WS-ERROR-SWITCH                GBIEPGM 
01226          MOVE ' INVALID EFFECTIVE DATE   CONTACT SYSTEMS'         GBIEPGM 
01227            TO  IEMSGO (2).                                        GBIEPGM 
01228                                                                   GBIEPGM 
01229                                                                   GBIEPGM 
01230 ** CONVERT TERMINATION-DATE FROM GREGORIAN TO JULIAN              GBIEPGM 
01231 **                                                                GBIEPGM 
01232      MOVE IETDTI (WS-SCREEN-SUB)  TO  HGADATE-DATE1.              GBIEPGM 
01233      PERFORM 9220-000-GREG-JULIAN-CEN.                            GBIEPGM 
01234                                                                   GBIEPGM 
01235      IF  MLDATE-RETURN   =   ZEROS                                GBIEPGM 
01236         MOVE MLDATE-JUL2         TO  WS-HOLD-JULIAN-DISPLAY       GBIEPGM 
01237         MOVE WS-HOLD-JULIAN-DISPLAY                               GBIEPGM 
01238                                  TO  WS-SAVE-FIRST-TRMDT-CEN      GBIEPGM 
01239      ELSE                                                         GBIEPGM 
01240        IF WS-ERROR-SWITCH  =   '1'                                GBIEPGM 
01241           NEXT SENTENCE                                           GBIEPGM 
01242        ELSE                                                       GBIEPGM 
01243            MOVE -1               TO  IETDTL (WS-SCREEN-SUB)       GBIEPGM 
01244            MOVE DFHBMUBF         TO  IETDTA (WS-SCREEN-SUB)       GBIEPGM 
01245            MOVE '1'              TO  WS-ERROR-SWITCH              GBIEPGM 
01246            MOVE ' INVALID TERMINATION DATE   CONTACT SYSTEMS'     GBIEPGM 
01247              TO  IEMSGO (2).                                      GBIEPGM 
01248                                                                   GBIEPGM 
01249  2230-900-EXIT.                                                   GBIEPGM 
01250      EXIT.                                                        GBIEPGM 
01251 /*****************************************************************GBIEPGM 
01252 **        BUILD THE PREVIOUS PAGE IF CONDITIONS ARE MET         **GBIEPGM 
01253 ******************************************************************GBIEPGM 
01254  2240-000-BUILD-PREVIOUS-PAGE  SECTION.                           GBIEPGM 
01255  2240-010.                                                        GBIEPGM 
01256                                                                   GBIEPGM 
01257      MOVE '2240'  TO  WS-PARA-ID.                                 GBIEPGM 
01258                                                                   GBIEPGM 
01259      MOVE +1  TO  WS-DT-SUB.                                      GBIEPGM 
01260                                                                   GBIEPGM 
01261      IF DT-ENTRY (WS-DT-SUB)   =   WS-SAVE-FIRST-ENTRY            GBIEPGM 
01262         PERFORM 2280-000-BRIGHT-FSET                              GBIEPGM 
01263           VARYING  WS-SCREEN-SUB   FROM  +1  BY  +1               GBIEPGM 
01264             UNTIL  WS-SCREEN-SUB   >   WS-SCREEN-MAX-ENTRIES      GBIEPGM 
01265                OR  IELNI (WS-SCREEN-SUB)   =                      GBIEPGM 
01266                                       (LOW-VALUES   OR   SPACES)  GBIEPGM 
01267         MOVE ' ***   SELECT OPTION AND CHOOSE GROUP SPECIFIC LINE GBIEPGM 
01268 -             'NUMBER  ***'                                       GBIEPGM 
01269           TO  IEMSGO (1)                                          GBIEPGM 
01270         MOVE '            ****   THIS IS THE FIRST PAGE   ****'   GBIEPGM 
01271           TO  IEMSGO (2)                                          GBIEPGM 
01272         GO TO 2240-900-EXIT.                                      GBIEPGM 
01273                                                                   GBIEPGM 
01274      PERFORM 2241-000-FIND-ENTRY-IN-TABLE                         GBIEPGM 
01275        VARYING  WS-DT-SUB  FROM  +1  BY  +1                       GBIEPGM 
01276          UNTIL  WS-ENTRY-IS-FOUND                                 GBIEPGM 
01277 ***         OR  WS-DT-SUB  >  DT-ENTRY-COUNT.                     GBIEPGM 
01278             OR  WS-DT-SUB  >  WS-DT-SUBX.                         GBIEPGM 
01279                                                                   GBIEPGM 
01280                                                                   GBIEPGM 
01281      IF WS-ENTRY-IS-NOT-FOUND                                     GBIEPGM 
01282          MOVE ' *** PAGE BACKWARD PROBLEM WITH DATES TABLE     PLEGBIEPGM 
01283 -             'ASE CALL SYSTEMS ***'                              GBIEPGM 
01284            TO  IEMSGO (2)                                         GBIEPGM 
01285          MOVE 'GJPA' TO WS-ABEND-CODE                             GBIEPGM 
01286          PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                   GBIEPGM 
01287                                                                   GBIEPGM 
01288      MOVE  31       TO    WS-SCREEN-LN                            GBIEPGM 
01289      MOVE +31       TO    WS-SCREEN-SUB.                          GBIEPGM 
01290      MOVE ZEROES    TO    WS-DARK-FIELD-COUNT-X.                  GBIEPGM 
01291      SUBTRACT  +1  FROM   WS-DT-SUB.                              GBIEPGM 
01292                                                                   GBIEPGM 
01293      PERFORM 2245-000-BUILD-PREV-PAGE                             GBIEPGM 
01294        UNTIL  WS-SCREEN-SUB     =   +1                            GBIEPGM 
01295           OR  WS-DT-SUB         <   +1.                           GBIEPGM 
01296                                                                   GBIEPGM 
01297                                                                   GBIEPGM 
01298      MOVE  +1  TO  WS-DT-SUB.                                     GBIEPGM 
01299      IF DT-ENTRY (WS-DT-SUB)    =    IELNI-ENTRY (WS-SCREEN-SUB)  GBIEPGM 
01300         PERFORM 2280-000-BRIGHT-FSET                              GBIEPGM 
01301           VARYING  WS-SCREEN-SUB   FROM  +1  BY  +1               GBIEPGM 
01302             UNTIL  WS-SCREEN-SUB   >   WS-SCREEN-MAX-ENTRIES      GBIEPGM 
01303                OR  IELNI (WS-SCREEN-SUB)   =                      GBIEPGM 
01304                                       (LOW-VALUES   OR   SPACES)  GBIEPGM 
01305         MOVE ' ***   SELECT OPTION AND CHOOSE GROUP SPECIFIC LINE GBIEPGM 
01306 -             'NUMBER  ***'                                       GBIEPGM 
01307           TO  IEMSGO (1)                                          GBIEPGM 
01308         MOVE '            ****   THIS IS THE FIRST PAGE   ****'   GBIEPGM 
01309           TO  IEMSGO (2)                                          GBIEPGM 
01310         GO TO 2240-900-EXIT.                                      GBIEPGM 
01311                                                                   GBIEPGM 
01312      MOVE  IEPAGEI  TO  WS-PAGE-COUNT-X.                          GBIEPGM 
01313      SUBTRACT  1  FROM  WS-PAGE-COUNT.                            GBIEPGM 
01314      MOVE  WS-PAGE-COUNT-X   TO  IEPAGEO.                         GBIEPGM 
01315      MOVE ' ***   SELECT OPTION AND CHOOSE GROUP SPECIFIC LINE    GBIEPGM 
01316 -             'NUMBER  ***'                                       GBIEPGM 
01317        TO  IEMSGO (2).                                            GBIEPGM 
01318                                                                   GBIEPGM 
01319  2240-900-EXIT.                                                   GBIEPGM 
01320      EXIT.                                                        GBIEPGM 
01321 /*****************************************************************GBIEPGM 
01322 **      SEARCH FOR THE FIRST SCREEN ENTRY IN THE DATES TABLE     *GBIEPGM 
01323 **      THAT IS SET UP IN THE LINKAGE SECTION.                   *GBIEPGM 
01324 ******************************************************************GBIEPGM 
01325  2241-000-FIND-ENTRY-IN-TABLE   SECTION.                          GBIEPGM 
01326  2241-010.                                                        GBIEPGM 
01327                                                                   GBIEPGM 
01328      MOVE '2241'  TO  WS-PARA-ID.                                 GBIEPGM 
01329                                                                   GBIEPGM 
01330      IF DT-ENTRY (WS-DT-SUB)   =   WS-SAVE-FIRST-ENTRY            GBIEPGM 
01331         MOVE '1'  TO  WS-ENTRY-IN-TABLE-IS-FOUND.                 GBIEPGM 
01332                                                                   GBIEPGM 
01333  2241-900-EXIT.                                                   GBIEPGM 
01334      EXIT.                                                        GBIEPGM 
01335 /*****************************************************************GBIEPGM 
01336 **                  BUILD THE PREVIOUS PAGE                      *GBIEPGM 
01337 ******************************************************************GBIEPGM 
01338  2245-000-BUILD-PREV-PAGE   SECTION.                              GBIEPGM 
01339  2245-010.                                                        GBIEPGM 
01340                                                                   GBIEPGM 
01341      MOVE '2245'    TO  WS-PARA-ID.                               GBIEPGM 
01342                                                                   GBIEPGM 
01343      SUBTRACT  +1  FROM   WS-DT-SUB                               GBIEPGM 
01344                           WS-SCREEN-SUB.                          GBIEPGM 
01345      SUBTRACT   1  FROM   WS-SCREEN-LN.                           GBIEPGM 
01346      ADD        1   TO    WS-DARK-FIELD-COUNT.                    GBIEPGM 
01347                                                                   GBIEPGM 
01348      PERFORM 2246-000-MOVE-DATA-TO-SCREEN.                        GBIEPGM 
01349                                                                   GBIEPGM 
01350  2245-900-EXIT.                                                   GBIEPGM 
01351      EXIT.                                                        GBIEPGM 
01352 /*****************************************************************GBIEPGM 
01353 **    MOVE ENTRIES FROM THE DATES TABLE TO THE SCREEN TABLE      *GBIEPGM 
01354 ******************************************************************GBIEPGM 
01355  2246-000-MOVE-DATA-TO-SCREEN  SECTION.                           GBIEPGM 
01356  2246-010.                                                        GBIEPGM 
01357                                                                   GBIEPGM 
01358      MOVE '2246'    TO  WS-PARA-ID.                               GBIEPGM 
01359                                                                   GBIEPGM 
01360      MOVE DFHBMABF                TO IELNA(WS-SCREEN-SUB).        GBIEPGM 
01361      MOVE WS-SCREEN-LN            TO IELNO(WS-SCREEN-SUB)         GBIEPGM 
01362      MOVE WS-DARK-FIELD-COUNT     TO IELNCNTO.                    GBIEPGM 
01363                                                                   GBIEPGM 
01364      MOVE DFHBMASF                TO IEFRA(WS-SCREEN-SUB).        GBIEPGM 
01365      MOVE DT-FAM-REL(WS-DT-SUB)   TO IEFRO(WS-SCREEN-SUB).        GBIEPGM 
01366                                                                   GBIEPGM 
01367      MOVE DT-EFF-DT(WS-DT-SUB)    TO HGADATE-JULIAN1.             GBIEPGM 
01368      PERFORM 9210-000-JULIAN-TO-GREGORIAN.                        GBIEPGM 
01369                                                                   GBIEPGM 
01370      IF  HGADATE-RETURN = ZEROS                                   GBIEPGM 
01371          MOVE DFHBMASF          TO IEEDTA(WS-SCREEN-SUB)          GBIEPGM 
01372          MOVE HGADATE-DATE2     TO IEEDTO(WS-SCREEN-SUB)          GBIEPGM 
01373      ELSE                                                         GBIEPGM 
01374          MOVE DFHBMABF          TO IEEDTA(WS-SCREEN-SUB)          GBIEPGM 
01375          MOVE HGADATE-JULIAN1   TO IEEDTO(WS-SCREEN-SUB).         GBIEPGM 
01376                                                                   GBIEPGM 
01377      MOVE DT-TERM-DT-CEN(WS-DT-SUB)  TO MLDATE-DATE1.             GBIEPGM 
01378      PERFORM 9215-000-JULIAN-GREG-CEN.                            GBIEPGM 
01379                                                                   GBIEPGM 
01380      IF  MLDATE-RETURN = ZEROS                                    GBIEPGM 
01381           MOVE DFHBMASF          TO IETDTA(WS-SCREEN-SUB)         GBIEPGM 
01382           MOVE MLDATE-DATE2      TO IETDTO(WS-SCREEN-SUB)         GBIEPGM 
01383      ELSE                                                         GBIEPGM 
01384          MOVE DFHBMABF           TO IETDTA(WS-SCREEN-SUB)         GBIEPGM 
01385          MOVE MLDATE-DATE1       TO IETDTO(WS-SCREEN-SUB).        GBIEPGM 
01386                                                                   GBIEPGM 
01387  2246-900-EXIT.                                                   GBIEPGM 
01388      EXIT.                                                        GBIEPGM 
01389 /*****************************************************************GBIEPGM 
01390 **                          PAGE FORWARD                         *GBIEPGM 
01391 ******************************************************************GBIEPGM 
01392  2250-000-PAGE-FORWARD     SECTION.                               GBIEPGM 
01393  2250-010.                                                        GBIEPGM 
01394                                                                   GBIEPGM 
01395      MOVE '2250'  TO  WS-PARA-ID.                                 GBIEPGM 
01396                                                                   GBIEPGM 
01397 ***  IF DT-ENTRY-COUNT   <   +31                                  GBIEPGM 
01398      IF WS-DT-SUBX       <   +31                                  GBIEPGM 
01399        IF IELNI (WS-SCREEN-SUB)   =   '01'                        GBIEPGM 
01400           PERFORM 2280-000-BRIGHT-FSET                            GBIEPGM 
01401             VARYING  WS-SCREEN-SUB   FROM  +1  BY  +1             GBIEPGM 
01402               UNTIL  WS-SCREEN-SUB   >   WS-SCREEN-MAX-ENTRIES    GBIEPGM 
01403                  OR  IELNI (WS-SCREEN-SUB)   =                    GBIEPGM 
01404                                         (LOW-VALUES   OR   SPACES)GBIEPGM 
01405           MOVE ' ***   SELECT OPTION AND CHOOSE GROUP SPECIFIC LINGBIEPGM 
01406 -             'E NUMBER  ***'                                     GBIEPGM 
01407             TO  IEMSGO (1)                                        GBIEPGM 
01408           MOVE '            ****   THIS IS THE ONLY PAGE   ****'  GBIEPGM 
01409             TO  IEMSGO (2)                                        GBIEPGM 
01410           GO TO 2250-900-EXIT.                                    GBIEPGM 
01411                                                                   GBIEPGM 
01412                                                                   GBIEPGM 
01413      MOVE +30  TO  WS-SCREEN-SUB.                                 GBIEPGM 
01414      IF IELNI (WS-SCREEN-SUB)   =   (LOW-VALUES   OR   SPACES)    GBIEPGM 
01415         PERFORM 2280-000-BRIGHT-FSET                              GBIEPGM 
01416           VARYING  WS-SCREEN-SUB   FROM  +1  BY  +1               GBIEPGM 
01417             UNTIL  WS-SCREEN-SUB   >   WS-SCREEN-MAX-ENTRIES      GBIEPGM 
01418                OR  IELNI (WS-SCREEN-SUB)   =                      GBIEPGM 
01419                                         (LOW-VALUES   OR   SPACES)GBIEPGM 
01420         MOVE ' ***   SELECT OPTION AND CHOOSE GROUP SPECIFIC LINE GBIEPGM 
01421 -             'NUMBER  ***'                                       GBIEPGM 
01422           TO  IEMSGO (1)                                          GBIEPGM 
01423         MOVE '            ****   THIS IS THE LAST PAGE   ****'    GBIEPGM 
01424           TO  IEMSGO (2)                                          GBIEPGM 
01425         GO TO 2250-900-EXIT.                                      GBIEPGM 
01426                                                                   GBIEPGM 
01427                                                                   GBIEPGM 
01428      MOVE +0   TO   WS-SCREEN-SUB.                                GBIEPGM 
01429                                                                   GBIEPGM 
01430      PERFORM 2253-000-SEARCH-FOR-LAST-ENTRY                       GBIEPGM 
01431        UNTIL  WS-LAST-ENTRY-IS-FOUND.                             GBIEPGM 
01432                                                                   GBIEPGM 
01433      IF WS-ERROR-SWITCH  =   '1'                                  GBIEPGM 
01434         GO TO 2250-900-EXIT.                                      GBIEPGM 
01435                                                                   GBIEPGM 
01436      PERFORM 2260-000-BUILD-NEXT-PAGE.                            GBIEPGM 
01437                                                                   GBIEPGM 
01438  2250-900-EXIT.                                                   GBIEPGM 
01439      EXIT.                                                        GBIEPGM 
01440 /*****************************************************************GBIEPGM 
01441 **            FIND THE LAST ENTRY ON THE SCREEN TABLE            *GBIEPGM 
01442 ******************************************************************GBIEPGM 
01443  2253-000-SEARCH-FOR-LAST-ENTRY  SECTION.                         GBIEPGM 
01444  2253-010.                                                        GBIEPGM 
01445                                                                   GBIEPGM 
01446      MOVE '2253'  TO  WS-PARA-ID.                                 GBIEPGM 
01447                                                                   GBIEPGM 
01448      ADD +1  TO   WS-SCREEN-SUB.                                  GBIEPGM 
01449                                                                   GBIEPGM 
01450      IF IELNI (WS-SCREEN-SUB)   =                                 GBIEPGM 
01451                    (ZEROES  OR  LOW-VALUES  OR  SPACES)           GBIEPGM 
01452         MOVE '1'  TO  WS-FOUND-LAST-ENTRY-ON-SCREEN               GBIEPGM 
01453         SUBTRACT  +1   FROM   WS-SCREEN-SUB                       GBIEPGM 
01454         PERFORM 2255-000-SAVE-LAST-ENTRY                          GBIEPGM 
01455         GO TO 2253-900-EXIT.                                      GBIEPGM 
01456                                                                   GBIEPGM 
01457                                                                   GBIEPGM 
01458      IF WS-SCREEN-SUB    =    WS-SCREEN-MAX-ENTRIES               GBIEPGM 
01459         MOVE '1'  TO  WS-FOUND-LAST-ENTRY-ON-SCREEN               GBIEPGM 
01460         PERFORM 2255-000-SAVE-LAST-ENTRY.                         GBIEPGM 
01461                                                                   GBIEPGM 
01462  2253-900-EXIT.                                                   GBIEPGM 
01463      EXIT.                                                        GBIEPGM 
01464 /*****************************************************************GBIEPGM 
01465 **         SAVE THE LAST ENTRY FROM THE SCREEN TABLE             *GBIEPGM 
01466 ******************************************************************GBIEPGM 
01467  2255-000-SAVE-LAST-ENTRY   SECTION.                              GBIEPGM 
01468  2255-010.                                                        GBIEPGM 
01469                                                                   GBIEPGM 
01470      MOVE '2255'  TO  WS-PARA-ID.                                 GBIEPGM 
01471                                                                   GBIEPGM 
01472      MOVE IELNI  (WS-SCREEN-SUB)   TO  WS-SAVE-LAST-LN-NO.        GBIEPGM 
01473      MOVE IEFRI  (WS-SCREEN-SUB)   TO  WS-SAVE-LAST-FAM-REL.      GBIEPGM 
01474                                                                   GBIEPGM 
01475 ** CONVERT EFFECTIVE-DATE FROM GREGORIAN TO JULIAN                GBIEPGM 
01476 **                                                                GBIEPGM 
01477      MOVE IEEDTI (WS-SCREEN-SUB)   TO  HGADATE-DATE1.             GBIEPGM 
01478      PERFORM 9200-000-GREGORIAN-TO-JULIAN.                        GBIEPGM 
01479                                                                   GBIEPGM 
01480      IF  HGADATE-RETURN   =   ZEROS                               GBIEPGM 
01481        IF HGADATE-JULIAN2 < +70000                                GBIEPGM 
01482          MOVE HEX-20           TO  WS-SAVE-LAST-EFFDT-CC          GBIEPGM 
01483          MOVE HGADATE-JULIAN2  TO  WS-SAVE-LAST-EFF-DT            GBIEPGM 
01484        ELSE                                                       GBIEPGM 
01485          MOVE HEX-19           TO  WS-SAVE-LAST-EFFDT-CC          GBIEPGM 
01486          MOVE HGADATE-JULIAN2  TO  WS-SAVE-LAST-EFF-DT            GBIEPGM 
01487        END-IF                                                     GBIEPGM 
01488      ELSE                                                         GBIEPGM 
01489          MOVE -1               TO  IEEDTL (WS-SCREEN-SUB)         GBIEPGM 
01490          MOVE DFHBMUBF         TO  IEEDTA (WS-SCREEN-SUB)         GBIEPGM 
01491          MOVE '1'              TO  WS-ERROR-SWITCH                GBIEPGM 
01492          MOVE ' INVALID EFFECTIVE DATE   CONTACT SYSTEMS'         GBIEPGM 
01493            TO IEMSGO (2).                                         GBIEPGM 
01494                                                                   GBIEPGM 
01495                                                                   GBIEPGM 
01496 ** CONVERT TERMINATION-DATE FROM GREGORIAN TO JULIAN              GBIEPGM 
01497 **                                                                GBIEPGM 
01498      MOVE IETDTI (WS-SCREEN-SUB)  TO  MLDATE-DATE1.               GBIEPGM 
01499      PERFORM 9220-000-GREG-JULIAN-CEN.                            GBIEPGM 
01500                                                                   GBIEPGM 
01501      IF  MLDATE-RETURN  =  ZEROS                                  GBIEPGM 
01502         MOVE MLDATE-JUL2       TO  WS-HOLD-JULIAN-DISPLAY         GBIEPGM 
01503         MOVE WS-HOLD-JULIAN-DISPLAY                               GBIEPGM 
01504                                TO  WS-SAVE-LAST-TRMDT-CEN         GBIEPGM 
01505      ELSE                                                         GBIEPGM 
01506        IF WS-ERROR-SWITCH  =   '1'                                GBIEPGM 
01507           NEXT SENTENCE                                           GBIEPGM 
01508        ELSE                                                       GBIEPGM 
01509          MOVE -1               TO  IETDTL (WS-SCREEN-SUB)         GBIEPGM 
01510          MOVE DFHBMUBF         TO  IETDTA (WS-SCREEN-SUB)         GBIEPGM 
01511          MOVE '1'              TO  WS-ERROR-SWITCH                GBIEPGM 
01512          MOVE ' INVALID TERMINATION DATE   CONTACT SYSTEMS'       GBIEPGM 
01513            TO IEMSGO (2).                                         GBIEPGM 
01514                                                                   GBIEPGM 
01515  2255-900-EXIT.                                                   GBIEPGM 
01516      EXIT.                                                        GBIEPGM 
01517 /*****************************************************************GBIEPGM 
01518 **          BUILD THE NEXT PAGE IF CONDITIONS ARE MET           **GBIEPGM 
01519 ******************************************************************GBIEPGM 
01520  2260-000-BUILD-NEXT-PAGE    SECTION.                             GBIEPGM 
01521  2260-010.                                                        GBIEPGM 
01522                                                                   GBIEPGM 
01523      MOVE '2260'  TO  WS-PARA-ID.                                 GBIEPGM 
01524                                                                   GBIEPGM 
01525      PERFORM 2265-000-FIND-ENTRY-IN-TABLE                         GBIEPGM 
01526        VARYING  WS-DT-SUB  FROM  +1  BY  +1                       GBIEPGM 
01527          UNTIL  WS-ENTRY-IS-FOUND                                 GBIEPGM 
01528 ***         OR  WS-DT-SUB  >  DT-ENTRY-COUNT.                     GBIEPGM 
01529             OR  WS-DT-SUB  >  WS-DT-SUBX.                         GBIEPGM 
01530                                                                   GBIEPGM 
01531                                                                   GBIEPGM 
01532      IF WS-ENTRY-IS-NOT-FOUND                                     GBIEPGM 
01533          MOVE ' *** PAGE FORWARD PROBLEM WITH DATES TABLE     PLEAGBIEPGM 
01534 -             'SE CALL SYSTEMS ***'                               GBIEPGM 
01535            TO  IEMSGO (2)                                         GBIEPGM 
01536          MOVE 'GJPB' TO WS-ABEND-CODE                             GBIEPGM 
01537          PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                   GBIEPGM 
01538                                                                   GBIEPGM 
01539      SUBTRACT  +1   FROM   WS-DT-SUB.                             GBIEPGM 
01540                                                                   GBIEPGM 
01541      IF WS-ENTRY-IS-FOUND                                         GBIEPGM 
01542 ***    IF WS-DT-SUB   =   DT-ENTRY-COUNT                          GBIEPGM 
01543        IF WS-DT-SUB   =   WS-DT-SUBX                              GBIEPGM 
01544           PERFORM 2280-000-BRIGHT-FSET                            GBIEPGM 
01545             VARYING  WS-SCREEN-SUB   FROM  +1  BY  +1             GBIEPGM 
01546               UNTIL  WS-SCREEN-SUB   >   WS-SCREEN-MAX-ENTRIES    GBIEPGM 
01547                  OR  IELNI (WS-SCREEN-SUB)   =                    GBIEPGM 
01548                                         (LOW-VALUES   OR   SPACES)GBIEPGM 
01549           MOVE ' ***   SELECT OPTION AND CHOOSE GROUP SPECIFIC LINGBIEPGM 
01550 -             'E NUMBER  ***'                                     GBIEPGM 
01551             TO  IEMSGO (1)                                        GBIEPGM 
01552           MOVE '            ****   THIS IS THE LAST PAGE   ****'  GBIEPGM 
01553             TO  IEMSGO (2)                                        GBIEPGM 
01554           GO TO 2260-900-EXIT.                                    GBIEPGM 
01555                                                                   GBIEPGM 
01556                                                                   GBIEPGM 
01557      MOVE ZEROES     TO    WS-SCREEN-LN                           GBIEPGM 
01558                            WS-SCREEN-SUB.                         GBIEPGM 
01559      MOVE ZEROES     TO    WS-DARK-FIELD-COUNT-X.                 GBIEPGM 
01560                                                                   GBIEPGM 
01561      PERFORM 2270-000-BUILD-NEXT-SCREEN                           GBIEPGM 
01562        UNTIL  WS-SCREEN-SUB     =   WS-SCREEN-MAX-ENTRIES         GBIEPGM 
01563 ***       OR  WS-SCREEN-SUB     =   DT-ENTRY-COUNT                GBIEPGM 
01564 ***       OR  WS-DT-SUB         =   DT-ENTRY-COUNT.               GBIEPGM 
01565           OR  WS-SCREEN-SUB     =   WS-DT-SUBX                    GBIEPGM 
01566           OR  WS-DT-SUB         =   WS-DT-SUBX.                   GBIEPGM 
01567                                                                   GBIEPGM 
01568      IF  WS-SCREEN-SUB     NOT =   WS-SCREEN-MAX-ENTRIES          GBIEPGM 
01569          PERFORM 2275-000-CLEAR-THE-PAGE                          GBIEPGM 
01570            UNTIL  WS-SCREEN-SUB     =   WS-SCREEN-MAX-ENTRIES.    GBIEPGM 
01571                                                                   GBIEPGM 
01572                                                                   GBIEPGM 
01573      MOVE  IEPAGEI  TO  WS-PAGE-COUNT-X.                          GBIEPGM 
01574      ADD  1  TO  WS-PAGE-COUNT.                                   GBIEPGM 
01575      MOVE  WS-PAGE-COUNT-X   TO  IEPAGEO.                         GBIEPGM 
01576                                                                   GBIEPGM 
01577                                                                   GBIEPGM 
01578      MOVE  WS-SCREEN-MAX-ENTRIES  TO  WS-SCREEN-SUB.              GBIEPGM 
01579      IF IELNI (WS-SCREEN-SUB)  =                                  GBIEPGM 
01580                       (ZEROES  OR  LOW-VALUES  OR  SPACES)        GBIEPGM 
01581         MOVE ' ***   SELECT OPTION AND CHOOSE GROUP SPECIFIC LINE GBIEPGM 
01582 -             'NUMBER  ***'                                       GBIEPGM 
01583           TO  IEMSGO (1)                                          GBIEPGM 
01584         MOVE '            ****   THIS IS THE LAST PAGE   ****'    GBIEPGM 
01585           TO  IEMSGO (2)                                          GBIEPGM 
01586         GO TO 2260-900-EXIT                                       GBIEPGM 
01587      ELSE                                                         GBIEPGM 
01588         MOVE ' ***   SELECT OPTION AND CHOOSE GROUP SPECIFIC LINE GBIEPGM 
01589 -             'NUMBER  ***'                                       GBIEPGM 
01590           TO  IEMSGO (2).                                         GBIEPGM 
01591                                                                   GBIEPGM 
01592                                                                   GBIEPGM 
01593 ***  IF WS-DT-SUB   =   DT-ENTRY-COUNT                            GBIEPGM 
01594      IF WS-DT-SUB   =   WS-DT-SUBX                                GBIEPGM 
01595         MOVE ' ***   SELECT OPTION AND CHOOSE GROUP SPECIFIC LINE GBIEPGM 
01596 -             'NUMBER  ***'                                       GBIEPGM 
01597           TO  IEMSGO (1)                                          GBIEPGM 
01598         MOVE '            ****   THIS IS THE LAST PAGE   ****'    GBIEPGM 
01599           TO  IEMSGO (2).                                         GBIEPGM 
01600                                                                   GBIEPGM 
01601  2260-900-EXIT.                                                   GBIEPGM 
01602      EXIT.                                                        GBIEPGM 
01603 /*****************************************************************GBIEPGM 
01604 **      SEARCH FOR THE LAST SCREEN ENTRY IN THE DATES TABLE      *GBIEPGM 
01605 **      THAT IS SET UP IN THE LINKAGE SECTION.                   *GBIEPGM 
01606 ******************************************************************GBIEPGM 
01607  2265-000-FIND-ENTRY-IN-TABLE   SECTION.                          GBIEPGM 
01608  2265-010.                                                        GBIEPGM 
01609                                                                   GBIEPGM 
01610      MOVE '2265'  TO  WS-PARA-ID.                                 GBIEPGM 
01611                                                                   GBIEPGM 
01612      IF DT-ENTRY (WS-DT-SUB)   =   WS-SAVE-LAST-ENTRY             GBIEPGM 
01613         MOVE '1'  TO  WS-ENTRY-IN-TABLE-IS-FOUND.                 GBIEPGM 
01614                                                                   GBIEPGM 
01615  2265-900-EXIT.                                                   GBIEPGM 
01616      EXIT.                                                        GBIEPGM 
01617 /*****************************************************************GBIEPGM 
01618 **                    BUILD THE NEXT SCREEN                      *GBIEPGM 
01619 ******************************************************************GBIEPGM 
01620  2270-000-BUILD-NEXT-SCREEN  SECTION.                             GBIEPGM 
01621  2270-010.                                                        GBIEPGM 
01622                                                                   GBIEPGM 
01623      MOVE '2270'  TO  WS-PARA-ID.                                 GBIEPGM 
01624                                                                   GBIEPGM 
01625      ADD  +1  TO   WS-DT-SUB                                      GBIEPGM 
01626                    WS-SCREEN-SUB.                                 GBIEPGM 
01627                                                                   GBIEPGM 
01628      ADD   1  TO   WS-SCREEN-LN                                   GBIEPGM 
01629                    WS-DARK-FIELD-COUNT.                           GBIEPGM 
01630                                                                   GBIEPGM 
01631      PERFORM 2246-000-MOVE-DATA-TO-SCREEN.                        GBIEPGM 
01632                                                                   GBIEPGM 
01633  2270-900-EXIT.                                                   GBIEPGM 
01634      EXIT.                                                        GBIEPGM 
01635 /*****************************************************************GBIEPGM 
01636 **  CLEAR THE BALANCE OF THE SCREEN TABLE ENTRIES, IF A COMPLETE *GBIEPGM 
01637 **  SCREEN TABLE WAS NOT FILLED.                                 *GBIEPGM 
01638 ******************************************************************GBIEPGM 
01639  2275-000-CLEAR-THE-PAGE  SECTION.                                GBIEPGM 
01640  2275-010.                                                        GBIEPGM 
01641                                                                   GBIEPGM 
01642      MOVE '2275'      TO  WS-PARA-ID.                             GBIEPGM 
01643                                                                   GBIEPGM 
01644      ADD  +1          TO  WS-SCREEN-SUB.                          GBIEPGM 
01645      MOVE LOW-VALUES  TO  IELNI-ENTRY (WS-SCREEN-SUB).            GBIEPGM 
01646      MOVE DFHBMASF    TO  IELNA (WS-SCREEN-SUB)                   GBIEPGM 
01647                           IEFRA (WS-SCREEN-SUB)                   GBIEPGM 
01648                           IEEDTA(WS-SCREEN-SUB)                   GBIEPGM 
01649                           IETDTA(WS-SCREEN-SUB).                  GBIEPGM 
01650                                                                   GBIEPGM 
01651  2275-900-EXIT.                                                   GBIEPGM 
01652      EXIT.                                                        GBIEPGM 
01653 /*****************************************************************GBIEPGM 
01654 **   RESET THE FSETS FOR THE SCREEN TABLE FIELDS, LINE NUMBER    *GBIEPGM 
01655 ******************************************************************GBIEPGM 
01656  2280-000-BRIGHT-FSET     SECTION.                                GBIEPGM 
01657  2280-010.                                                        GBIEPGM 
01658                                                                   GBIEPGM 
01659      MOVE '2280'      TO  WS-PARA-ID.                             GBIEPGM 
01660                                                                   GBIEPGM 
01661      MOVE DFHBMABF    TO  IELNA (WS-SCREEN-SUB).                  GBIEPGM 
01662                                                                   GBIEPGM 
01663  2280-900-EXIT.                                                   GBIEPGM 
01664      EXIT.                                                        GBIEPGM 
01665 /*****************************************************************GBIEPGM 
01666 **                                                               *GBIEPGM 
01667 ** 2300     LOAD DATES TABLE                                     *GBIEPGM 
01668 **                                                               *GBIEPGM 
01669 ******************************************************************GBIEPGM 
01670  2300-000-LOAD-DATES-TABLE     SECTION.                           GBIEPGM 
01671  2300-010.                                                        GBIEPGM 
01672                                                                   GBIEPGM 
01673      MOVE '2300'  TO  WS-PARA-ID.                                 GBIEPGM 
01674                                                                   GBIEPGM 
01675      IF GI-RETURN-CODE  = 'GS'                                    GBIEPGM 
01676         MOVE GIGT-SLOT-NUMBER TO WS-COMP-SERV-DT-CEN              GBIEPGM 
01677         IF (DTE-EFFDT-CEN(DTE-INDEX) NOT > WS-COMP-SERV-DT-CEN)   GBIEPGM 
01678             AND                                                   GBIEPGM 
01679            (DTE-TERMDT-CEN(DTE-INDEX) NOT < WS-COMP-SERV-DT-CEN)  GBIEPGM 
01680             CONTINUE                                              GBIEPGM 
01681         ELSE                                                      GBIEPGM 
01682             GO TO 2300-900-EXIT                                   GBIEPGM 
01683         END-IF                                                    GBIEPGM 
01684      END-IF.                                                      GBIEPGM 
01685                                                                   GBIEPGM 
01686      ADD +1 TO WS-DT-SUB                                          GBIEPGM 
01687                WS-DT-SUBX.                                        GBIEPGM 
01688                                                                   GBIEPGM 
01689      MOVE DTE-EFFDT-CEN(DTE-INDEX)  TO DT-EFF-DT-CEN(WS-DT-SUB).  GBIEPGM 
01690      MOVE DTE-FAMILY-RELAT-LEVEL(DTE-INDEX)                       GBIEPGM 
01691                               TO DT-FAM-REL(WS-DT-SUB).           GBIEPGM 
01692      MOVE DTE-TERMDT-CEN(DTE-INDEX) TO DT-TERM-DT-CEN(WS-DT-SUB). GBIEPGM 
01693                                                                   GBIEPGM 
01694 *        +----------------------------------------+               GBIEPGM 
01695 *        +  COMPLIMENT JULIAN EFFECTIVE DATE IN   +               GBIEPGM 
01696 *        +   DATES TABLE FOR SORT, THIS CAUSE     +               GBIEPGM 
01697 *        +   THE DATE TO BE SORTED DESCENDINGLY.  +               GBIEPGM 
01698 *        +----------------------------------------+               GBIEPGM 
01699                                                                   GBIEPGM 
01700      COMPUTE DT-EFF-DT-CEN(WS-DT-SUB) =                           GBIEPGM 
01701              +9999999             -                               GBIEPGM 
01702              DT-EFF-DT-CEN(WS-DT-SUB).                            GBIEPGM 
01703                                                                   GBIEPGM 
01704  2300-900-EXIT.                                                   GBIEPGM 
01705      EXIT.                                                        GBIEPGM 
01706 /*****************************************************************GBIEPGM 
01707 **                                                               *GBIEPGM 
01708 ** 2400     SORT DATES TABLE                                     *GBIEPGM 
01709 **                                                               *GBIEPGM 
01710 ******************************************************************GBIEPGM 
01711  2400-000-SORT-DATES-TABLE     SECTION.                           GBIEPGM 
01712  2400-010.                                                        GBIEPGM 
01713                                                                   GBIEPGM 
01714      MOVE '2400'  TO  WS-PARA-ID.                                 GBIEPGM 
01715                                                                   GBIEPGM 
01716      MOVE 'N' TO WS-SORT-EXCHANGE-INDICATOR.                      GBIEPGM 
01717                                                                   GBIEPGM 
01718      PERFORM 2410-000-SORT-ENTRY-EXCHANGE                         GBIEPGM 
01719         VARYING WS-DT-SUB2 FROM 1 BY 1                            GBIEPGM 
01720 ***       UNTIL WS-DT-SUB2 > DT-ENTRY-COUNT.                      GBIEPGM 
01721           UNTIL WS-DT-SUB2 > WS-DT-SUBX.                          GBIEPGM 
01722                                                                   GBIEPGM 
01723  2400-900-EXIT.                                                   GBIEPGM 
01724      EXIT.                                                        GBIEPGM 
01725 /*****************************************************************GBIEPGM 
01726 **                                                               *GBIEPGM 
01727 ** 2410     SORT ENTRY EXCHANGE                                  *GBIEPGM 
01728 **                                                               *GBIEPGM 
01729 ******************************************************************GBIEPGM 
01730  2410-000-SORT-ENTRY-EXCHANGE  SECTION.                           GBIEPGM 
01731  2410-010.                                                        GBIEPGM 
01732                                                                   GBIEPGM 
01733      MOVE '2410'  TO  WS-PARA-ID.                                 GBIEPGM 
01734                                                                   GBIEPGM 
01735      COMPUTE WS-DT-SUB3 = WS-DT-SUB2 + 1.                         GBIEPGM 
01736 ***  IF  WS-DT-SUB3 > DT-ENTRY-COUNT                              GBIEPGM 
01737      IF  WS-DT-SUB3 > WS-DT-SUBX                                  GBIEPGM 
01738          GO TO 2410-900-EXIT.                                     GBIEPGM 
01739                                                                   GBIEPGM 
01740      IF  DT-ENTRY(WS-DT-SUB3) < DT-ENTRY(WS-DT-SUB2)              GBIEPGM 
01741          MOVE 'Y'                  TO WS-SORT-EXCHANGE-INDICATOR  GBIEPGM 
01742          MOVE DT-ENTRY(WS-DT-SUB3) TO WS-HOLD-DT-ENTRY            GBIEPGM 
01743          MOVE DT-ENTRY(WS-DT-SUB2) TO DT-ENTRY(WS-DT-SUB3)        GBIEPGM 
01744          MOVE WS-HOLD-DT-ENTRY     TO DT-ENTRY(WS-DT-SUB2).       GBIEPGM 
01745                                                                   GBIEPGM 
01746  2410-900-EXIT.                                                   GBIEPGM 
01747      EXIT.                                                        GBIEPGM 
01748 /*****************************************************************GBIEPGM 
01749 **                                                               *GBIEPGM 
01750 ** 2500     UNCOMPLIMENT EFFECTIVE DATE                          *GBIEPGM 
01751 **                                                               *GBIEPGM 
01752 ******************************************************************GBIEPGM 
01753  2500-000-UNCOMPLIMENT-EFF-DATE SECTION.                          GBIEPGM 
01754  2500-010.                                                        GBIEPGM 
01755                                                                   GBIEPGM 
01756      MOVE '2500'  TO  WS-PARA-ID.                                 GBIEPGM 
01757                                                                   GBIEPGM 
01758      COMPUTE DT-EFF-DT-CEN(WS-DT-SUB) =                           GBIEPGM 
01759              +9999999             -                               GBIEPGM 
01760              DT-EFF-DT-CEN(WS-DT-SUB).                            GBIEPGM 
01761                                                                   GBIEPGM 
01762  2500-900-EXIT.                                                   GBIEPGM 
01763      EXIT.                                                        GBIEPGM 
01764 /*****************************************************************GBIEPGM 
01765 **                                                               *GBIEPGM 
01766 ** 2600     CLEAR SCREEN TABLE                                   *GBIEPGM 
01767 **                                                               *GBIEPGM 
01768 ******************************************************************GBIEPGM 
01769  2600-000-CLEAR-SCREEN-TABLE    SECTION.                          GBIEPGM 
01770  2600-010.                                                        GBIEPGM 
01771                                                                   GBIEPGM 
01772      MOVE '2600'  TO  WS-PARA-ID.                                 GBIEPGM 
01773                                                                   GBIEPGM 
01774      MOVE LOW-VALUES TO IELNI-ENTRY (WS-SCREEN-SUB).              GBIEPGM 
01775      MOVE DFHBMASF   TO IELNA (WS-SCREEN-SUB)                     GBIEPGM 
01776                         IEFRA (WS-SCREEN-SUB)                     GBIEPGM 
01777                         IEEDTA(WS-SCREEN-SUB)                     GBIEPGM 
01778                         IETDTA(WS-SCREEN-SUB).                    GBIEPGM 
01779                                                                   GBIEPGM 
01780  2600-900-EXIT.                                                   GBIEPGM 
01781      EXIT.                                                        GBIEPGM 
01782 /*****************************************************************GBIEPGM 
01783 **                                                               *GBIEPGM 
01784 ** 2700     BUILD SELECT LIST                                    *GBIEPGM 
01785 **                                                               *GBIEPGM 
01786 ******************************************************************GBIEPGM 
01787  2700-000-BUILD-SELECT-LIST     SECTION.                          GBIEPGM 
01788  2700-010.                                                        GBIEPGM 
01789                                                                   GBIEPGM 
01790      MOVE '2700'  TO  WS-PARA-ID.                                 GBIEPGM 
01791                                                                   GBIEPGM 
01792      ADD 1    TO WS-DT-SUB.                                       GBIEPGM 
01793      MOVE 'Y' TO WS-SELECT-MATCH-INDICATOR.                       GBIEPGM 
01794                                                                   GBIEPGM 
01795      IF  WS-ENTERED-IEFRX          = '1'      AND                 GBIEPGM 
01796          DT-FAM-REL(WS-DT-SUB) NOT = WS-FAM-REL-LVL               GBIEPGM 
01797          MOVE 'N' TO WS-SELECT-MATCH-INDICATOR.                   GBIEPGM 
01798                                                                   GBIEPGM 
01799      IF  WS-ENTERED-IEEDTX         = '1'      AND                 GBIEPGM 
01800          DT-EFF-DT-CEN(WS-DT-SUB)      > WS-EFFDT-CEN             GBIEPGM 
01801          MOVE 'N' TO WS-SELECT-MATCH-INDICATOR.                   GBIEPGM 
01802                                                                   GBIEPGM 
01803      IF  WS-SELECT-MATCH-INDICATOR = 'N'                          GBIEPGM 
01804          GO TO 2700-900-EXIT.                                     GBIEPGM 
01805                                                                   GBIEPGM 
01806                                                                   GBIEPGM 
01807      ADD 1 TO WS-SCREEN-LN                                        GBIEPGM 
01808               WS-SCREEN-SUB.                                      GBIEPGM 
01809                                                                   GBIEPGM 
01810 ***  IF  DT-ENTRY-COUNT < +2                                      GBIEPGM 
01811      IF  WS-DT-SUBX     < +2                                      GBIEPGM 
01812          PERFORM 2900-000-SINGLE-REC-PROC                         GBIEPGM 
01813          GO TO 2700-900-EXIT                                      GBIEPGM 
01814      END-IF.                                                      GBIEPGM 
01815                                                                   GBIEPGM 
01816                                                                   GBIEPGM 
01817      MOVE DFHBMABF                TO IELNA(WS-SCREEN-SUB).        GBIEPGM 
01818      MOVE WS-SCREEN-LN            TO IELNO(WS-SCREEN-SUB)         GBIEPGM 
01819                                      IELNCNTO.                    GBIEPGM 
01820                                                                   GBIEPGM 
01821      MOVE DFHBMASF                TO IEFRA(WS-SCREEN-SUB).        GBIEPGM 
01822      MOVE DT-FAM-REL(WS-DT-SUB)   TO IEFRO(WS-SCREEN-SUB).        GBIEPGM 
01823                                                                   GBIEPGM 
01824      MOVE DT-EFF-DT(WS-DT-SUB)    TO HGADATE-JULIAN1.             GBIEPGM 
01825      PERFORM 9210-000-JULIAN-TO-GREGORIAN.                        GBIEPGM 
01826      IF  HGADATE-RETURN = ZEROS                                   GBIEPGM 
01827          MOVE DFHBMASF        TO IEEDTA(WS-SCREEN-SUB)            GBIEPGM 
01828          MOVE HGADATE-DATE2   TO IEEDTO(WS-SCREEN-SUB)            GBIEPGM 
01829      ELSE                                                         GBIEPGM 
01830          MOVE DFHBMABF        TO IEEDTA(WS-SCREEN-SUB)            GBIEPGM 
01831          MOVE HGADATE-JULIAN1 TO IEEDTO(WS-SCREEN-SUB).           GBIEPGM 
01832                                                                   GBIEPGM 
01833      MOVE DT-TERM-DT-CEN(WS-DT-SUB)    TO MLDATE-DATE1.           GBIEPGM 
01834      PERFORM 9215-000-JULIAN-GREG-CEN.                            GBIEPGM 
01835      IF  MLDATE-RETURN = ZEROS                                    GBIEPGM 
01836          MOVE DFHBMASF        TO IETDTA(WS-SCREEN-SUB)            GBIEPGM 
01837          MOVE MLDATE-DATE2    TO IETDTO(WS-SCREEN-SUB)            GBIEPGM 
01838      ELSE                                                         GBIEPGM 
01839          MOVE DFHBMABF        TO IETDTA(WS-SCREEN-SUB)            GBIEPGM 
01840          MOVE MLDATE-DATE1    TO IETDTO(WS-SCREEN-SUB).           GBIEPGM 
01841                                                                   GBIEPGM 
01842  2700-900-EXIT.                                                   GBIEPGM 
01843      EXIT.                                                        GBIEPGM 
01844 /*****************************************************************GBIEPGM 
01845 **                                                               *GBIEPGM 
01846 ** 2800     C L E A R   T H E   M A P   S C R E E N              *GBIEPGM 
01847 ** AHL 11/25/86                                                  *GBIEPGM 
01848 **                                                               *GBIEPGM 
01849 ** USED ONLY TO CLEAR THE OCCURS SECTION OF THE SCREEN IF THE    *GBIEPGM 
01850 **      GROUP AND SECTION NUMBER DO NOT HAVE ANY VALID DATE      *GBIEPGM 
01851 **      RECORDS.                                                  GBIEPGM 
01852 **                                                               *GBIEPGM 
01853 ******************************************************************GBIEPGM 
01854  2800-000-CLEAR-MAP-TABLE      SECTION.                           GBIEPGM 
01855  2800-010.                                                        GBIEPGM 
01856                                                                   GBIEPGM 
01857      MOVE '2800'  TO  WS-PARA-ID.                                 GBIEPGM 
01858                                                                   GBIEPGM 
01859      MOVE SPACES     TO IELNI-ENTRY (WS-CLEAR-INDEX).             GBIEPGM 
01860      MOVE DFHBMASF   TO IELNA (WS-CLEAR-INDEX)                    GBIEPGM 
01861                         IEFRA (WS-CLEAR-INDEX)                    GBIEPGM 
01862                         IEEDTA(WS-CLEAR-INDEX)                    GBIEPGM 
01863                         IETDTA(WS-CLEAR-INDEX).                   GBIEPGM 
01864                                                                   GBIEPGM 
01865  2800-900-EXIT.                                                   GBIEPGM 
01866      EXIT.                                                        GBIEPGM 
01867 ******************************************************************GBIEPGM 
01868 **                                                               *GBIEPGM 
01869 **                                                               *GBIEPGM 
01870 ******************************************************************GBIEPGM 
01871  2900-000-SINGLE-REC-PROC       SECTION.                          GBIEPGM 
01872  2900-010.                                                        GBIEPGM 
01873                                                                   GBIEPGM 
01874      MOVE '2900'  TO  WS-PARA-ID.                                 GBIEPGM 
01875                                                                   GBIEPGM 
01876      MOVE DT-FAM-REL(WS-DT-SUB)   TO IEFRXO                       GBIEPGM 
01877      MOVE +2                      TO  IEFRXL                      GBIEPGM 
01878                                                                   GBIEPGM 
01879      MOVE DT-EFF-DT(WS-DT-SUB)    TO HGADATE-JULIAN1.             GBIEPGM 
01880      PERFORM 9210-000-JULIAN-TO-GREGORIAN.                        GBIEPGM 
01881                                                                   GBIEPGM 
01882      IF  HGADATE-RETURN = ZEROS                                   GBIEPGM 
01883          MOVE HGADATE-DATE2   TO IEEDTXO                          GBIEPGM 
01884          MOVE +6              TO  IEEDTXL                         GBIEPGM 
01885      ELSE                                                         GBIEPGM 
01886          MOVE HGADATE-JULIAN1 TO IEEDTXO                          GBIEPGM 
01887          MOVE +6              TO  IEEDTXL                         GBIEPGM 
01888      END-IF.                                                      GBIEPGM 
01889                                                                   GBIEPGM 
01890                                                                   GBIEPGM 
01891  2900-900-EXIT.                                                   GBIEPGM 
01892      EXIT.                                                        GBIEPGM 
01893 /*****************************************************************GBIEPGM 
01894 **                                                              **GBIEPGM 
01895 **           X C T L   T O   M A I N   M E N U                  **GBIEPGM 
01896 **                                                              **GBIEPGM 
01897 ******************************************************************GBIEPGM 
01898  6000-000-XCTL-TO-MAIN-MENU  SECTION.                             GBIEPGM 
01899  6000-010.                                                        GBIEPGM 
01900      MOVE '6000'  TO  WS-PARA-ID.                                 GBIEPGM 
01901      MOVE '1AP1'  TO  WS-ABEND-CODE.                              GBIEPGM 
01902                                                                   GBIEPGM 
01903      EXEC CICS XCTL   PROGRAM('GHILPGM') END-EXEC.                GBIEPGM 
01904                                                                   GBIEPGM 
01905  6000-900-EXIT.                                                   GBIEPGM 
01906      EXIT.                                                        GBIEPGM 
01907 /*****************************************************************GBIEPGM 
01908 **                                                               *GBIEPGM 
01909 ** 7500   SEND DATAONLY                                          *GBIEPGM 
01910 **                                                               *GBIEPGM 
01911 ******************************************************************GBIEPGM 
01912  7500-000-SEND-DATAONLY  SECTION.                                 GBIEPGM 
01913  7000-010.                                                        GBIEPGM 
01914                                                                   GBIEPGM 
01915      EXEC CICS SEND   MAP   ('GBIEI01')                           GBIEPGM 
01916                       MAPSET('GBIESET')                           GBIEPGM 
01917                       DATAONLY                                    GBIEPGM 
01918                       FROM  (GBIEI01O)                            GBIEPGM 
01919                       CURSOR                                      GBIEPGM 
01920                       END-EXEC.                                   GBIEPGM 
01921                                                                   GBIEPGM 
01922  7500-900-EXIT.                                                   GBIEPGM 
01923      EXIT.                                                        GBIEPGM 
01924 /*****************************************************************GBIEPGM 
01925 **                                                               *GBIEPGM 
01926 ** 7600   SEND ERASE                                             *GBIEPGM 
01927 **                                                               *GBIEPGM 
01928 ******************************************************************GBIEPGM 
01929  7600-000-SEND-ERASE  SECTION.                                    GBIEPGM 
01930  7600-010.                                                        GBIEPGM 
01931                                                                   GBIEPGM 
01932      EXEC CICS SEND   MAP   ('GBIEI01')                           GBIEPGM 
01933                       MAPSET('GBIESET')                           GBIEPGM 
01934                       ERASE                                       GBIEPGM 
01935                       FROM  (GBIEI01O)                            GBIEPGM 
01936                       CURSOR                                      GBIEPGM 
01937                       END-EXEC.                                   GBIEPGM 
01938                                                                   GBIEPGM 
01939  7600-900-EXIT.                                                   GBIEPGM 
01940      EXIT.                                                        GBIEPGM 
01941                                                                   GBIEPGM 
01942 /*****************************************************************GBIEPGM 
01943 **                                                               *GBIEPGM 
01944 ** 7700   INQUIRE ON TRANS ID                                    *GBIEPGM 
01945 **                                                               *GBIEPGM 
01946 ******************************************************************GBIEPGM 
01947  7700-INQUIRE-ON-TRANS-ID   SECTION.                              GBIEPGM 
01948                                                                   GBIEPGM 
01949      EXEC CICS  INQUIRE                                           GBIEPGM 
01950                 TRANSACTION (WS-CHECK-TRANS-ID)                   GBIEPGM 
01951                 STATUS      (WS-INQUIRY-STATUS)                   GBIEPGM 
01952                 RESP        (WS-INQUIRY-RESP)                     GBIEPGM 
01953                 END-EXEC.                                         GBIEPGM 
01954  7700-900-EXIT.                                                   GBIEPGM 
01955       EXIT.                                                       GBIEPGM 
01956                                                                   GBIEPGM 
01957 /*****************************************************************GBIEPGM 
01958 **                                                               *GBIEPGM 
01959 ** 7800   CHECK RESPONSE                                          GBIEPGM 
01960 **                                                               *GBIEPGM 
01961 ******************************************************************GBIEPGM 
01962  7800-CHECK-RESPONSE    SECTION.                                  GBIEPGM 
01963      IF WS-INQUIRY-RESP  = DFHRESP(NORMAL)                        GBIEPGM 
01964 ***     PERFORM 6700-CHECK-INQUIRY-STATUS THRU 6700-900-EXIT      GBIEPGM 
01965         GO TO 7800-900-EXIT                                       GBIEPGM 
01966      ELSE                                                         GBIEPGM 
01967         MOVE WS-INQUIRY-RESP            TO  WS-HOLD-INQUIRY-RESP  GBIEPGM 
01968         MOVE WS-DISPLAY-INQUIRY-RESP-X  TO  WS-ABEND-CODE         GBIEPGM 
01969         MOVE '*** INQUIRY ON TRAN ID DISPOSITION FAILED,          GBIEPGM 
01970 -            ' PLEASE CALL SYSTEMS ***' TO  IEMSGO(1)             GBIEPGM 
01971         GO TO 9900-000-ERROR-MSG-THEN-ABEND.                      GBIEPGM 
01972  7800-900-EXIT.                                                   GBIEPGM 
01973       EXIT.                                                       GBIEPGM 
01974                                                                   GBIEPGM 
01975 /*****************************************************************GBIEPGM 
01976 **                                                                GBIEPGM 
01977 ** 7900   CHECK INQUIRY STATUS                                    GBIEPGM 
01978 **                                                                GBIEPGM 
01979 ******************************************************************GBIEPGM 
01980 *7900-CHECK-INQUIRY-STATUS  SECTION.                              GBIEPGM 
01981 ***  IF WS-INQUIRY-STATUS  = DFHVALUE(DISABLED)                   GBIEPGM 
01982 ***     SET WT-01-INDEX TO +04                                    GBIEPGM 
01983 ***     MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH              GBIEPGM 
01984 ***     PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                      GBIEPGM 
01985 *7900-900-EXIT.                                                   GBIEPGM 
01986 ***   EXIT.                                                       GBIEPGM 
01987                                                                   GBIEPGM 
01988 /*****************************************************************GBIEPGM 
01989 **                                                               *GBIEPGM 
01990 ** 8000   READ GROUP SPECIFIC                                    *GBIEPGM 
01991 **                                                               *GBIEPGM 
01992 ******************************************************************GBIEPGM 
01993  8000-000-READ-GRPSPC  SECTION.                                   GBIEPGM 
01994  8000-010.                                                        GBIEPGM 
01995                                                                   GBIEPGM 
01996      COMPUTE  WS-IO-PARM-GRPSPC-LEN        =                      GBIEPGM 
01997               GC-GCIOPARM-LEN              +                      GBIEPGM 
01998               GC-GCGRPSPC-MAX-REC-LEN.                            GBIEPGM 
01999                                                                   GBIEPGM 
02000      EXEC CICS GETMAIN  SET (ADDRESS OF IO-PARM-GRPSPC-RECORD)    GBIEPGM 
02001                         INITIMG(WS-HEX-00)                        GBIEPGM 
02002                         LENGTH (WS-IO-PARM-GRPSPC-LEN)            GBIEPGM 
02003                         END-EXEC.                                 GBIEPGM 
02004      SET GI2-RECORD-POINTER TO ADDRESS OF IO-PARM-GRPSPC-RECORD.  GBIEPGM 
02005                                                                   GBIEPGM 
02006      MOVE GC-GCGRPSPC-VARY-MAX-OCUR                               GBIEPGM 
02007                                   TO GCG-COUNT-TAB-PROVN-POINTERS.GBIEPGM 
02008      MOVE 'GCGRPSPC'              TO GCIO-FILE-DDNAME.            GBIEPGM 
02009      MOVE '1'                     TO GCIO-IO-AREA-TO-USE.         GBIEPGM 
02010      MOVE WS-GROUP-SPECIFIC-ID    TO GIG2-GROUP-SPECIFIC-ID       GBIEPGM 
02011                                      GCIO-GROUP-SPECIFIC.         GBIEPGM 
02012      MOVE GCIO-GROUP-SPECIFIC     TO GCIO-FILE-KEY.               GBIEPGM 
02013      MOVE 'RD '                   TO GCIO-FILE-ACCESS-CODE.       GBIEPGM 
02014                                                                   GBIEPGM 
02015      EXEC CICS LINK   PROGRAM ('GCIOPGM')                         GBIEPGM 
02016                       COMMAREA(IO-PARM-GRPSPC-RECORD)             GBIEPGM 
02017                       LENGTH  (WS-IO-PARM-GRPSPC-LEN)             GBIEPGM 
02018                       END-EXEC.                                   GBIEPGM 
02019                                                                   GBIEPGM 
02020  8000-900-EXIT.                                                   GBIEPGM 
02021      EXIT.                                                        GBIEPGM 
02022 /*****************************************************************GBIEPGM 
02023 **                                                               *GBIEPGM 
02024 ** 8100   READ GCDATES                                           *GBIEPGM 
02025 **                                                               *GBIEPGM 
02026 ******************************************************************GBIEPGM 
02027  8100-000-READ-GCDATES  SECTION.                                  GBIEPGM 
02028  8100-010.                                                        GBIEPGM 
02029                                                                   GBIEPGM 
02030      COMPUTE  WS-IO-PARM-GCDATES-LEN       =                      GBIEPGM 
02031               GC-GCIOPARM-LEN              +                      GBIEPGM 
02032               GC-GCDATES-MAX-REC-LEN.                             GBIEPGM 
02033                                                                   GBIEPGM 
02034      EXEC CICS GETMAIN  SET (ADDRESS OF IO-PARM-GCDATES-RECORD)   GBIEPGM 
02035                         INITIMG(WS-HEX-00)                        GBIEPGM 
02036                         LENGTH (WS-IO-PARM-GCDATES-LEN)           GBIEPGM 
02037                         END-EXEC.                                 GBIEPGM 
02038                                                                   GBIEPGM 
02039      MOVE GC-GCDATES-VARY-MAX-OCUR TO DTE-ENTRY-COUNT.            GBIEPGM 
02040      MOVE 'GCDATES '              TO GCIO2-FILE-DDNAME.           GBIEPGM 
02041      MOVE '1'                     TO GCIO2-IO-AREA-TO-USE.        GBIEPGM 
02042      MOVE WS-GCDATES-KEY          TO GCIO2-FILE-KEY.              GBIEPGM 
02043      MOVE 'RD '                   TO GCIO2-FILE-ACCESS-CODE.      GBIEPGM 
02044                                                                   GBIEPGM 
02045      EXEC CICS LINK   PROGRAM ('GCIOPGM')                         GBIEPGM 
02046                       COMMAREA(IO-PARM-GCDATES-RECORD)            GBIEPGM 
02047                       LENGTH  (WS-IO-PARM-GCDATES-LEN)            GBIEPGM 
02048                       END-EXEC.                                   GBIEPGM 
02049                                                                   GBIEPGM 
02050  8100-900-EXIT.                                                   GBIEPGM 
02051      EXIT.                                                        GBIEPGM 
02052 /*****************************************************************GBIEPGM 
02053 *                                                                *GBIEPGM 
02054 * 9200    G R E G O R I A N   T O   J U L I A N                  *GBIEPGM 
02055 *                                                                *GBIEPGM 
02056 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *GBIEPGM 
02057 *                                                                *GBIEPGM 
02058 ******************************************************************GBIEPGM 
02059  9200-000-GREGORIAN-TO-JULIAN   SECTION.                          GBIEPGM 
02060  9200-010.                                                        GBIEPGM 
02061                                                                   GBIEPGM 
02062      MOVE 'CNV' TO  HGADATE-FUNC.                                 GBIEPGM 
02063      MOVE 'M'   TO  HGADATE-FORM1.                                GBIEPGM 
02064      MOVE 'J'   TO  HGADATE-FORM2.                                GBIEPGM 
02065      MOVE ZEROS TO  HGADATE-RETURN                                GBIEPGM 
02066                     HGADATE-AMOUNT.                               GBIEPGM 
02067      EXEC CICS LINK PROGRAM ('HGADATES')                          GBIEPGM 
02068                     COMMAREA(HGADATES-COMMAREA)                   GBIEPGM 
02069                     LENGTH  (24)                                  GBIEPGM 
02070                     END-EXEC.                                     GBIEPGM 
02071                                                                   GBIEPGM 
02072  9200-900-EXIT.                                                   GBIEPGM 
02073      EXIT.                                                        GBIEPGM 
02074 /*****************************************************************GBIEPGM 
02075 *                                                                *GBIEPGM 
02076 * 9210    J U L I A N    T O    G R E G O R I A N                *GBIEPGM 
02077 *                                                                *GBIEPGM 
02078 *   CONVERT JULIAN DATE (YYDDD) TO GREGORIAN (MMDDYY) FORMAT.    *GBIEPGM 
02079 *                                                                *GBIEPGM 
02080 ******************************************************************GBIEPGM 
02081  9210-000-JULIAN-TO-GREGORIAN   SECTION.                          GBIEPGM 
02082  9210-010.                                                        GBIEPGM 
02083                                                                   GBIEPGM 
02084      MOVE 'CNV' TO  HGADATE-FUNC.                                 GBIEPGM 
02085      MOVE 'J'   TO  HGADATE-FORM1.                                GBIEPGM 
02086      MOVE 'M'   TO  HGADATE-FORM2.                                GBIEPGM 
02087      MOVE ZEROS TO  HGADATE-RETURN                                GBIEPGM 
02088                     HGADATE-AMOUNT.                               GBIEPGM 
02089      EXEC CICS LINK PROGRAM ('HGADATES')                          GBIEPGM 
02090                     COMMAREA(HGADATES-COMMAREA)                   GBIEPGM 
02091                     LENGTH  (24)                                  GBIEPGM 
02092                     END-EXEC.                                     GBIEPGM 
02093                                                                   GBIEPGM 
02094  9210-900-EXIT.                                                   GBIEPGM 
02095      EXIT.                                                        GBIEPGM 
02096 **************** CENTURY * CENTURY *******************************GBIEPGM 
02097  9215-000-JULIAN-GREG-CEN       SECTION.                          GBIEPGM 
02098  9215-010.                                                        GBIEPGM 
02099                                                                   GBIEPGM 
02100      MOVE 'CNV' TO  MLDATE-FUNC.                                  GBIEPGM 
02101      MOVE 'J'   TO  MLDATE-FORM1.                                 GBIEPGM 
02102      MOVE 'M'   TO  MLDATE-FORM2.                                 GBIEPGM 
02103      MOVE ZEROS TO  MLDATE-RETURN                                 GBIEPGM 
02104                     MLDATE-AMOUNT.                                GBIEPGM 
02105      EXEC CICS LINK PROGRAM ('MLDATEC')                           GBIEPGM 
02106                     COMMAREA(MLDATE01)                            GBIEPGM 
02107                     LENGTH  (28)                                  GBIEPGM 
02108                     END-EXEC.                                     GBIEPGM 
02109                                                                   GBIEPGM 
02110  9215-900-900-EXIT.                                               GBIEPGM 
02111      EXIT.                                                        GBIEPGM 
02112 ****************** CENTURY * CENTURY *****************************GBIEPGM 
02113  9220-000-GREG-JULIAN-CEN       SECTION.                          GBIEPGM 
02114  9220-010.                                                        GBIEPGM 
02115                                                                   GBIEPGM 
02116      MOVE 'CNV' TO  MLDATE-FUNC.                                  GBIEPGM 
02117      MOVE 'M'   TO  MLDATE-FORM1.                                 GBIEPGM 
02118      MOVE 'J'   TO  MLDATE-FORM2.                                 GBIEPGM 
02119      MOVE ZEROS TO  MLDATE-RETURN                                 GBIEPGM 
02120                     MLDATE-AMOUNT.                                GBIEPGM 
02121      EXEC CICS LINK PROGRAM ('MLDATEC')                           GBIEPGM 
02122                     COMMAREA(MLDATE01)                            GBIEPGM 
02123                     LENGTH  (28)                                  GBIEPGM 
02124                     END-EXEC.                                     GBIEPGM 
02125                                                                   GBIEPGM 
02126  9220-900-900-EXIT.                                               GBIEPGM 
02127      EXIT.                                                        GBIEPGM 
02128                                                                   GBIEPGM 
02129                                                                   GBIEPGM 
02130                                                                   GBIEPGM 
02131  9300-000-LOAD-GBIE-KEY  SECTION.                                 GBIEPGM 
02132  9300-010.                                                        GBIEPGM 
02133                                                                   GBIEPGM 
02134      MOVE GIG-PLAN-CODE      TO  IEPLNXO                          GBIEPGM 
02135      MOVE GIG-GROUP-NUM      TO  IEGRPXO                          GBIEPGM 
02136      MOVE GIG-SECTION-NUM    TO  IESECXO                          GBIEPGM 
02137      MOVE GIG-PKG-CODE       TO  IEPKGXO                          GBIEPGM 
02138 *    MOVE GIC-FAM-REL-LVL    TO  IEFRXO                           GBIEPGM 
02139 *    MOVE GI-EFFECTIVE-DATE  TO  IEEDTXO.                         GBIEPGM 
02140         MOVE GI-RETURN-CODE TO IERETCO                            GBIEPGM 
02141                                                                   GBIEPGM 
02142      IF GI-SUBSCRIBER NOT = LOW-VALUES                            GBIEPGM 
02143         MOVE GI-SUBSCRIBER TO IESUBO                              GBIEPGM 
02144         MOVE GIGT-SLOT-NUMBER TO IESVDTO                          GBIEPGM 
02145      END-IF                                                       GBIEPGM 
02146                                                                   GBIEPGM 
02147      IF  (GIC-PLAN-CODE NOT  = LOW-VALUES) AND                    GBIEPGM 
02148          (GIC-PLAN-CODE NOT  = SPACES)                            GBIEPGM 
02149           MOVE +3            TO  IEPLNXL                          GBIEPGM 
02150      END-IF                                                       GBIEPGM 
02151                                                                   GBIEPGM 
02152      IF  (GIC-GROUP-NUM NOT  = LOW-VALUES) AND                    GBIEPGM 
02153          (GIC-GROUP-NUM NOT  = SPACES)                            GBIEPGM 
02154           MOVE +9            TO  IEGRPXL                          GBIEPGM 
02155      END-IF                                                       GBIEPGM 
02156                                                                   GBIEPGM 
02157      IF  (GIC-SECTION-NUM NOT   = LOW-VALUES) AND                 GBIEPGM 
02158          (GIC-SECTION-NUM NOT   = SPACES)                         GBIEPGM 
02159           MOVE +5            TO  IESECXL                          GBIEPGM 
02160      END-IF                                                       GBIEPGM 
02161                                                                   GBIEPGM 
02162      IF  (GIC-PKG-CODE  NOT  = LOW-VALUES) AND                    GBIEPGM 
02163          (GIC-PKG-CODE  NOT  = SPACES)                            GBIEPGM 
02164           MOVE +3            TO  IEPKGXL                          GBIEPGM 
02165      END-IF.                                                      GBIEPGM 
02166                                                                   GBIEPGM 
02167 **   IF  (GIC-FAM-REL-LVL NOT   = LOW-VALUES) AND                 GBIEPGM 
02168 *        (GIC-FAM-REL-LVL NOT   = SPACES)                         GBIEPGM 
02169 *         MOVE +2            TO  IEFRXL                           GBIEPGM 
02170 *    END-IF                                                       GBIEPGM 
02171                                                                   GBIEPGM 
02172 *    IF  (GI-EFFECTIVE-DATE NOT =   LOW-VALUES) AND               GBIEPGM 
02173 *        (GI-EFFECTIVE-DATE NOT =   SPACES)                       GBIEPGM 
02174 *         MOVE +6            TO  IEEDTXL                          GBIEPGM 
02175 *    END-IF.                                                      GBIEPGM 
02176                                                                   GBIEPGM 
02177                                                                   GBIEPGM 
02178  9300-900-EXIT.                                                   GBIEPGM 
02179           EXIT.                                                   GBIEPGM 
02180                                                                   GBIEPGM 
02181                                                                   GBIEPGM 
02182                                                                   GBIEPGM 
02183  9400-000-SEND-TEST-MAP  SECTION.                                 GBIEPGM 
02184  9400-010.                                                        GBIEPGM 
02185                                                                   GBIEPGM 
02186 *    MOVE -1 TO IEGRPXL                                           GBIEPGM 
02187                                                                   GBIEPGM 
02188 **   MOVE  GIC-PLAN-CODE      TO   WS-DISP-PLAN-CT                GBIEPGM 
02189      MOVE  GIC-GROUP-NUM      TO   WS-DISP-GROUP-CT               GBIEPGM 
02190      MOVE  GIC-SECTION-NUM    TO   WS-DISP-SECTION-CT             GBIEPGM 
02191      MOVE  GIC-PKG-CODE       TO   WS-DISP-PKG-CT                 GBIEPGM 
02192      MOVE  GIC-L-O-B          TO   WS-DISP-LOB-CT                 GBIEPGM 
02193      MOVE  GIC-PROV-CTL       TO   WS-DISP-PRV-CT                 GBIEPGM 
02194      MOVE  GIC-FAM-REL-LVL    TO   WS-DISP-FRL-CT                 GBIEPGM 
02195      MOVE  GIC-EFFDT-CEN      TO   WS-DISP-EFFDT-CT               GBIEPGM 
02196      MOVE  GICT-SLOT-NUMBER   TO   WS-DISP-TRMDT-CT               GBIEPGM 
02197      MOVE  WS-DISP-CONTRACT   TO   IEMSGO(3).                     GBIEPGM 
02198                                                                   GBIEPGM 
02199 *    PERFORM 7600-000-SEND-ERASE.                                 GBIEPGM 
02200                                                                   GBIEPGM 
02201  9400-900-EXIT.                                                   GBIEPGM 
02202           EXIT.                                                   GBIEPGM 
02203                                                                   GBIEPGM 
02204                                                                   GBIEPGM 
02205                                                                   GBIEPGM 
02206  9900-000-ERROR-MSG-THEN-ABEND  SECTION.                          GBIEPGM 
02207                                                                   GBIEPGM 
02208      MOVE -1  TO  IEPLNXL.                                        GBIEPGM 
02209      EXEC CICS SEND   MAP   ('GBIEI01')                           GBIEPGM 
02210                       MAPSET('GBIESET')                           GBIEPGM 
02211                       ERASE                                       GBIEPGM 
02212                       FROM  (GBIEI01O)                            GBIEPGM 
02213                       CURSOR                                      GBIEPGM 
02214                       WAIT                                        GBIEPGM 
02215                       END-EXEC.                                   GBIEPGM 
02216                                                                   GBIEPGM 
02217      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GBIEPGM 
02218                                                                   GBIEPGM 
02219  9900-900-EXIT.                                                   GBIEPGM 
02220      EXIT.                                                        GBIEPGM 
