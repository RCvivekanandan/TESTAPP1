00001  ID DIVISION.                                                     08/20/03
00002  PROGRAM-ID.     GBIBPGM.                                         GBIBPGM 
00003  AUTHOR.         JUNE PON.                                           LV001
00004 ***** THIS IS A COBOL/2 PROGRAM.                                  GBIBPGM 
00005  DATE-WRITTEN.   03/01/01.                                        GBIBPGM 
00006  DATE-COMPILED.                                                   GBIBPGM 
00007                                                                   GBIBPGM 
00008 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          GBIBPGM 
00009 *    *-*         U P D A T E   H I S T O R Y         *-*          GBIBPGM 
00010 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          GBIBPGM 
00011                                                                   GBIBPGM 
00012 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* GBIBPGM 
00013                                                                   GBIBPGM 
00014 *            03/01/01  JP   1. CODED INITIAL PROGRAM USING GI1APGMGBIBPGM 
00015 *                              AS A MODEL.                        GBIBPGM 
00016 *                                                                 GBIBPGM 
00017 *            06/05/01  GSP  1. CHANGED ALL REFERENCES FROM GBIA TOGBIBPGM 
00018 *                              GHIL (THIS TRANSACTION WAS RENAMED)GBIBPGM 
00019 *                                                                 GBIBPGM 
00020 *                                                                 GBIBPGM 
00021 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GBIBPGM 
00022 *                                                                *GBIBPGM 
00023 *                                                                 GBIBPGM 
00024 *                                                                *GBIBPGM 
00025 ******************************************************************GBIBPGM 
00026 *                                                                *GBIBPGM 
00027 *   GBIBPGM    BENEFITS HIGHLIGHTS CONTRACT RECORD SELECTION MENU*GBIBPGM 
00028 *                                                                *GBIBPGM 
00029 *                                                                *GBIBPGM 
00030 *   FUNC CODE: GBIB                                              *GBIBPGM 
00031 *                                                                *GBIBPGM 
00032 *   MAPSET:    GBIBSETC                                          *GBIBPGM 
00033 *   MAPTABLE:  YES                                               *GBIBPGM 
00034 *                                                                *GBIBPGM 
00035 *   FILES:     GCCONTR                                           *GBIBPGM 
00036 *              GCDATES                                           *GBIBPGM 
00037 *              GCGROUP                                           *GBIBPGM 
00038 *                                                                *GBIBPGM 
00039 *   ABEND CODES:                                                 *GBIBPGM 
00040 *   ------------                                                 *GBIBPGM 
00041 *         'GIPA'    LOGIC ERROR WHEN PAGING BACKWARD.            *GBIBPGM 
00042 *                   DID NOT FIND ENTRY FROM THE SCREEN IN THE    *GBIBPGM 
00043 *                   DATE TABLE.                                  *GBIBPGM 
00044 *         'GIPB'    LOGIC ERROR WHEN PAGING FORWARD.             *GBIBPGM 
00045 *                   DID NOT FIND ENTRY FROM THE SCREEN IN THE    *GBIBPGM 
00046 *                   DATE TABLE.                                  *GBIBPGM 
00047 *                                                                *GBIBPGM 
00048 ******************************************************************GBIBPGM 
00049 /                                                                 GBIBPGM 
00050  ENVIRONMENT DIVISION.                                            GBIBPGM 
00051                                                                   GBIBPGM 
00052  DATA DIVISION.                                                   GBIBPGM 
00053  WORKING-STORAGE SECTION.                                         GBIBPGM 
00054  01  WS-BEGIN                    PIC X(24)  VALUE                 GBIBPGM 
00055      '***GBIBPGM WS BEGINS***'.                                   GBIBPGM 
00056  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GBIBPGM 
00057                                                                   GBIBPGM 
00058  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GBIBPGM 
00059                                                                   GBIBPGM 
00060 ** WORKFIELDS, AND SWITCHES **                                    GBIBPGM 
00061  01  WS-WORK-FIELDS.                                              GBIBPGM 
00062      05  WS-HOLD-JULIAN-DISPLAY      PIC 9(7).                    GBIBPGM 
00063                                                                   GBIBPGM 
00064      05  WS-GCDATES-BC-ADDR          PIC S9(8) COMP.              GBIBPGM 
00065      05  WS-GCDATES-BC-POINTER REDEFINES WS-GCDATES-BC-ADDR       GBIBPGM 
00066                                               POINTER.            GBIBPGM 
00067      05  WS-GCDATES-BS-ADDR                   PIC S9(8) COMP.     GBIBPGM 
00068      05  WS-GCDATES-BS-POINTER REDEFINES WS-GCDATES-BS-ADDR       GBIBPGM 
00069                                               POINTER.            GBIBPGM 
00070      05  WS-GCDATES-SM-ADDR                   PIC S9(8) COMP.     GBIBPGM 
00071      05  WS-GCDATES-SM-POINTER REDEFINES WS-GCDATES-SM-ADDR       GBIBPGM 
00072                                               POINTER.            GBIBPGM 
00073      05  WS-GCDATES-CM-ADDR                   PIC S9(8) COMP.     GBIBPGM 
00074      05  WS-GCDATES-CM-POINTER REDEFINES WS-GCDATES-CM-ADDR       GBIBPGM 
00075                                               POINTER.            GBIBPGM 
00076                                                                   GBIBPGM 
00077      05  WS-GCDATES-BC-ENTRY-COUNT   VALUE +0 PIC S9(5) COMP-3.   GBIBPGM 
00078      05  WS-GCDATES-BS-ENTRY-COUNT   VALUE +0 PIC S9(5) COMP-3.   GBIBPGM 
00079      05  WS-GCDATES-SM-ENTRY-COUNT   VALUE +0 PIC S9(5) COMP-3.   GBIBPGM 
00080      05  WS-GCDATES-CM-ENTRY-COUNT   VALUE +0 PIC S9(5) COMP-3.   GBIBPGM 
00081                                                                   GBIBPGM 
00082      05  WS-HEX-00                            PIC X.              GBIBPGM 
00083      05  WS-QUOTIENT                          PIC 999   COMP-3.   GBIBPGM 
00084      05  WS-REMAINDER                         PIC 999   COMP-3.   GBIBPGM 
00085                                                                   GBIBPGM 
00086      05  WS-SCREEN-SUB               VALUE +0  PIC S9(4) COMP.    GBIBPGM 
00087      05  WS-SCREEN-LN                VALUE  00 PIC  9(2).         GBIBPGM 
00088      05  WS-SCREEN-MAX-ENTRIES       VALUE +18 PIC S9(4) COMP.    GBIBPGM 
00089                                                                   GBIBPGM 
00090      05  WS-DARK-FIELD-COUNT-X       VALUE  SPACES   PIC X(02).   GBIBPGM 
00091      05  WS-DARK-FIELD-COUNT   REDEFINES  WS-DARK-FIELD-COUNT-X   GBIBPGM 
00092                                                      PIC 9(02).   GBIBPGM 
00093                                                                   GBIBPGM 
00094      05  WS-PAGE-NAME                VALUE  'PAGE:'    PIC X(05). GBIBPGM 
00095      05  WS-PAGE-COUNT-X             VALUE  ZEROES     PIC X(03). GBIBPGM 
00096      05  WS-PAGE-COUNT   REDEFINES  WS-PAGE-COUNT-X    PIC 9(03). GBIBPGM 
00097                                                                   GBIBPGM 
00098      05  WS-SELECT-MATCH-INDICATOR            PIC X     VALUE ' '.GBIBPGM 
00099          88 WS-SELECT-MATCH-MADE                        VALUE 'Y'.GBIBPGM 
00100          88 WS-SELECT-MATCH-NOT-MADE                    VALUE 'N'.GBIBPGM 
00101                                                                   GBIBPGM 
00102      05  WS-FOUND-LAST-ENTRY-ON-SCREEN        PIC X(01) VALUE '0'.GBIBPGM 
00103          88  WS-LAST-ENTRY-IS-FOUND                     VALUE '1'.GBIBPGM 
00104      05  WS-ENTRY-IN-TABLE-IS-FOUND           PIC X(01) VALUE '0'.GBIBPGM 
00105          88  WS-ENTRY-IS-FOUND                          VALUE '1'.GBIBPGM 
00106          88  WS-ENTRY-IS-NOT-FOUND                      VALUE '0'.GBIBPGM 
00107      05  WS-SYSID.                                                GBIBPGM 
00108          10  FILLER                           PIC X(01).          GBIBPGM 
00109              88  WS-TEXAS-CICS-REGION         VALUE 'X'.          GBIBPGM 
00110          10  FILLER                           PIC X(03).          GBIBPGM 
00111                                                                   GBIBPGM 
00112 **********   AHL 11/24/86                                         GBIBPGM 
00113      05  WS-CLEAR-INDEX              VALUE +0 PIC S9(4) COMP.     GBIBPGM 
00114 *************************                                         GBIBPGM 
00115 *                                                                 GBIBPGM 
00116      05  WS-DT-SUB                   VALUE +0 PIC S9(4) COMP.     GBIBPGM 
00117      05  WS-DT-SUBX                  VALUE +0 PIC S9(4) COMP.     GBIBPGM 
00118      05  WS-DT-SUB2                  VALUE +0 PIC S9(4) COMP.     GBIBPGM 
00119      05  WS-DT-SUB3                  VALUE +0 PIC S9(4) COMP.     GBIBPGM 
00120      05  WS-COMP-SERV-DT-CEN                 PIC S9(7) COMP-3.    GBIBPGM 
00121      05  WS-SORT-EXCHANGE-INDICATOR           PIC X     VALUE ' '.GBIBPGM 
00122          88 WS-SORT-EXCHANGE-NOT-MADE                   VALUE 'N'.GBIBPGM 
00123      05  WS-HOLD-DT-ENTRY.                                        GBIBPGM 
00124          10  WS-HOLD-DT-EFF-DATE.                                 GBIBPGM 
00125              15 WS-HOLD-DT-EFF-DT-CC             PIC X(01).       GBIBPGM 
00126              15 WS-HOLD-DT-EFF-DT                PIC S9(5) COMP-3.GBIBPGM 
00127          10  WS-HOLD-DT-EFF-DATE-CEN  REDEFINES                   GBIBPGM 
00128              WS-HOLD-DT-EFF-DATE                 PIC S9(7) COMP-3.GBIBPGM 
00129          10  WS-HOLD-DT-L-O-B                    PIC X(1).        GBIBPGM 
00130          10  WS-HOLD-DT-PROV-CNTL                PIC X(2).        GBIBPGM 
00131          10  WS-HOLD-DT-FAM-REL                  PIC X(2).        GBIBPGM 
00132          10  WS-HOLD-DT-TERM-DATE.                                GBIBPGM 
00133              15 WS-HOLD-DT-TERM-DT-CC             PIC X(01).      GBIBPGM 
00134              15 WS-HOLD-DT-TERM-DT               PIC S9(5) COMP-3.GBIBPGM 
00135          10  WS-HOLD-DT-TERM-DATE-CEN  REDEFINES                  GBIBPGM 
00136              WS-HOLD-DT-TERM-DATE                PIC S9(7) COMP-3.GBIBPGM 
00137                                                                   GBIBPGM 
00138      05  WS-SAVE-FIRST-LN-NO            PIC 9(2) VALUE ZEROES.    GBIBPGM 
00139      05  WS-SAVE-FIRST-ENTRY.                                     GBIBPGM 
00140          10  WS-SAVE-FIRST-EFF-DATE.                              GBIBPGM 
00141              15 WS-SAVE-FIRST-EFF-DT-CC        PIC X(01).         GBIBPGM 
00142              15 WS-SAVE-FIRST-EFF-DT           PIC S9(5) COMP-3.  GBIBPGM 
00143          10  WS-SAVE-FIRST-EFF-DATE-CEN  REDEFINES                GBIBPGM 
00144              WS-SAVE-FIRST-EFF-DATE            PIC S9(7) COMP-3.  GBIBPGM 
00145          10  WS-SAVE-FIRST-L-O-B        PIC X(1) VALUE SPACE.     GBIBPGM 
00146          10  WS-SAVE-FIRST-PROV-CNTL    PIC X(2) VALUE SPACES.    GBIBPGM 
00147          10  WS-SAVE-FIRST-FAM-REL      PIC X(2) VALUE SPACE.     GBIBPGM 
00148          10  WS-SAVE-FIRST-TERM-DATE.                             GBIBPGM 
00149              15 WS-SAVE-FIRST-TERM-DT-CC       PIC X(01).         GBIBPGM 
00150              15 WS-SAVE-FIRST-TERM-DT          PIC S9(5) COMP-3.  GBIBPGM 
00151          10  WS-SAVE-FIRST-TERM-DATE-CEN  REDEFINES               GBIBPGM 
00152              WS-SAVE-FIRST-TERM-DATE           PIC S9(7) COMP-3.  GBIBPGM 
00153                                                                   GBIBPGM 
00154      05  WS-SAVE-LAST-LN-NO             PIC 9(2) VALUE ZEROES.    GBIBPGM 
00155      05  WS-SAVE-LAST-ENTRY.                                      GBIBPGM 
00156          10  WS-SAVE-LAST-EFF-DATE.                               GBIBPGM 
00157              15 WS-SAVE-LAST-EFF-DT-CC         PIC X(01).         GBIBPGM 
00158              15 WS-SAVE-LAST-EFF-DT            PIC S9(5) COMP-3.  GBIBPGM 
00159           10 WS-SAVE-LAST-EFF-DATE-CEN    REDEFINES               GBIBPGM 
00160              WS-SAVE-LAST-EFF-DATE      PIC S9(7) COMP-3.         GBIBPGM 
00161          10  WS-SAVE-LAST-L-O-B         PIC X(1) VALUE SPACE.     GBIBPGM 
00162          10  WS-SAVE-LAST-PROV-CNTL     PIC X(2) VALUE SPACE.     GBIBPGM 
00163          10  WS-SAVE-LAST-FAM-REL       PIC X(2) VALUE SPACE.     GBIBPGM 
00164          10  WS-SAVE-LAST-TERM-DATE.                              GBIBPGM 
00165              15 WS-SAVE-LAST-TERM-DT-CC PIC X(01).                GBIBPGM 
00166              15 WS-SAVE-LAST-TERM-DT    PIC S9(5) COMP-3.         GBIBPGM 
00167          10  WS-SAVE-LAST-TERM-DATE-CEN   REDEFINES               GBIBPGM 
00168              WS-SAVE-LAST-TERM-DATE     PIC S9(7) COMP-3.         GBIBPGM 
00169                                                                   GBIBPGM 
00170      05  WS-SCREEN-KEY-ENTRY-SWITCHES.                            GBIBPGM 
00171       88 WS-FULL-CONTRACT-KEY-ENTERED         VALUE ALL '1'.      GBIBPGM 
00172          10  WS-ENTERED-IBPLNX                PIC X     VALUE '0'.GBIBPGM 
00173          10  WS-ENTERED-IBGRPX                PIC X     VALUE '0'.GBIBPGM 
00174          10  WS-ENTERED-IBSECX                PIC X     VALUE '0'.GBIBPGM 
00175          10  WS-ENTERED-IBPKGX                PIC X     VALUE '0'.GBIBPGM 
00176          10  WS-ENTERED-IBLOBX                PIC X     VALUE '0'.GBIBPGM 
00177          10  WS-ENTERED-IBPRVX                PIC X     VALUE '0'.GBIBPGM 
00178          10  WS-ENTERED-IBFRX                 PIC X     VALUE '0'.GBIBPGM 
00179          10  WS-ENTERED-IBEDTX                PIC X     VALUE '0'.GBIBPGM 
00180                                                                   GBIBPGM 
00181      05  WS-SELECTION-SWITCHES.                                   GBIBPGM 
00182          10  WS-ENTERED-IBLNNO                PIC X     VALUE '0'.GBIBPGM 
00183                                                                   GBIBPGM 
00184      05  WS-ERROR-SWITCH                      PIC X     VALUE '0'.GBIBPGM 
00185      05  WS-END-GROUP-TEST-SW            PIC X     VALUE 'N'.     GBIBPGM 
00186          88  END-GROUP-TEST                   VALUE 'Y'.          GBIBPGM 
00187      05  WS-GROUP-MATCH-FOUND-SW         PIC X     VALUE 'N'.     GBIBPGM 
00188          88  GROUP-MATCH-FOUND                VALUE 'Y'.          GBIBPGM 
00189      05  WS-CONTRACT-FOUND-SW            PIC X     VALUE 'N'.     GBIBPGM 
00190          88  CONTRACT-FOUND                   VALUE 'Y'.          GBIBPGM 
00191                                                                   GBIBPGM 
00192                                                                   GBIBPGM 
00193                                                                   GBIBPGM 
00194      05  WS-CONTRACT-ID.                                          GBIBPGM 
00195          10  WS-PLAN-CODE                     PIC X(03) VALUE ' '.GBIBPGM 
00196          10  WS-GRP-NUMBER.                                       GBIBPGM 
00197              15 WS-GRP-NO-1-3                 PIC X(03) VALUE ' '.GBIBPGM 
00198              15 WS-GRP-NO                     PIC X(06) VALUE ' '.GBIBPGM 
00199          10  WS-SECTN-NUMBER.                                     GBIBPGM 
00200              15 WS-SECTN-NO-1                 PIC X(01) VALUE ' '.GBIBPGM 
00201              15 WS-SECTN-NO                   PIC X(04) VALUE ' '.GBIBPGM 
00202          10  WS-PKG-CODE                      PIC X(03) VALUE ' '.GBIBPGM 
00203          10  WS-L-O-B                         PIC X(01) VALUE ' '.GBIBPGM 
00204          10  WS-PROV-CTL                      PIC X(02) VALUE ' '.GBIBPGM 
00205          10  WS-FAM-REL-LVL                   PIC X(02) VALUE ' '.GBIBPGM 
00206          10  WS-EFF-DATE.                                         GBIBPGM 
00207              15 WS-EFF-DT-CC                  PIC X(01).          GBIBPGM 
00208              15 WS-EFF-DT          COMP-3     PIC S9(5).          GBIBPGM 
00209          10  WS-EFF-DATE-CEN REDEFINES                            GBIBPGM 
00210              WS-EFF-DATE           COMP-3     PIC S9(7).          GBIBPGM 
00211                                                                   GBIBPGM 
00212                                                                   GBIBPGM 
00213      05  WS-GROUPSPC-KEY.                                         GBIBPGM 
00214          10  WS-PLAN-CODE-GS                  PIC X(03) VALUE ' '.GBIBPGM 
00215          10  WS-GRP-NUMBER-GS.                                    GBIBPGM 
00216              15 WS-GRP-NO-1-3-GS              PIC X(03) VALUE ' '.GBIBPGM 
00217              15 WS-GRP-NO-GS                  PIC X(06) VALUE ' '.GBIBPGM 
00218          10  WS-SECTN-NUMBER-GS.                                  GBIBPGM 
00219              15 WS-SECTN-NO-1-GS              PIC X(01) VALUE ' '.GBIBPGM 
00220              15 WS-SECTN-NO-GS                PIC X(04) VALUE ' '.GBIBPGM 
00221          10  WS-PKG-CODE-GS                   PIC X(03) VALUE ' '.GBIBPGM 
00222          10  WS-FAM-REL-LVL-GS                PIC X(02) VALUE ' '.GBIBPGM 
00223          10  WS-EFF-DATE-GS.                                      GBIBPGM 
00224              15 WS-EFF-DT-CC-GS               PIC X(01).          GBIBPGM 
00225              15 WS-EFF-DT-GS       COMP-3     PIC S9(5).          GBIBPGM 
00226          10  WS-EFF-DATE-CEN-GS REDEFINES                         GBIBPGM 
00227              WS-EFF-DATE-GS        COMP-3     PIC S9(7).          GBIBPGM 
00228                                                                   GBIBPGM 
00229      05  WS-HOLD-GROUPSPC.                                        GBIBPGM 
00230          10  WS-HOLD-PLAN                     PIC X(03).          GBIBPGM 
00231          10  WS-HOLD-GROUP                    PIC X(09).          GBIBPGM 
00232          10  WS-HOLD-SECTION                  PIC X(05).          GBIBPGM 
00233          10  WS-HOLD-PKG                      PIC X(03).          GBIBPGM 
00234          10  WS-HOLD-FRL                      PIC X(02).          GBIBPGM 
00235                                                                   GBIBPGM 
00236      05  WS-DISP-GROUPSPC.                                        GBIBPGM 
00237          10  FILLER                     PIC X(17) VALUE           GBIBPGM 
00238              'GROUPSPC RECORD= '.                                 GBIBPGM 
00239          10  WS-DISP-PLAN               PIC X(03).                GBIBPGM 
00240          10  FILLER                     PIC X(01) VALUE SPACES.   GBIBPGM 
00241          10  WS-DISP-GROUP              PIC X(09).                GBIBPGM 
00242          10  FILLER                     PIC X(01) VALUE SPACES.   GBIBPGM 
00243          10  WS-DISP-SECTION            PIC X(05).                GBIBPGM 
00244          10  FILLER                     PIC X(01) VALUE SPACES.   GBIBPGM 
00245          10  WS-DISP-PKG                PIC X(03).                GBIBPGM 
00246          10  FILLER                     PIC X(01) VALUE SPACES.   GBIBPGM 
00247          10  WS-DISP-FRL                PIC X(02).                GBIBPGM 
00248          10  FILLER                     PIC X(01) VALUE SPACES.   GBIBPGM 
00249          10  WS-DISP-EFFDT              PIC 9(07).                GBIBPGM 
00250          10  FILLER                     PIC X(01) VALUE SPACES.   GBIBPGM 
00251          10  WS-DISP-TRMDT              PIC 9(07).                GBIBPGM 
00252                                                                   GBIBPGM 
00253      05  WS-DISP-CONTRACT.                                        GBIBPGM 
00254          10  FILLER                     PIC X(17) VALUE           GBIBPGM 
00255              'CONTRACT RECORD= '.                                 GBIBPGM 
00256          10  WS-DISP-PLAN-CT            PIC X(03).                GBIBPGM 
00257          10  FILLER                     PIC X(01) VALUE SPACES.   GBIBPGM 
00258          10  WS-DISP-GROUP-CT           PIC X(09).                GBIBPGM 
00259          10  FILLER                     PIC X(01) VALUE SPACES.   GBIBPGM 
00260          10  WS-DISP-SECTION-CT         PIC X(05).                GBIBPGM 
00261          10  FILLER                     PIC X(01) VALUE SPACES.   GBIBPGM 
00262          10  WS-DISP-PKG-CT             PIC X(03).                GBIBPGM 
00263          10  FILLER                     PIC X(01) VALUE SPACES.   GBIBPGM 
00264          10  WS-DISP-FRL-CT             PIC X(02).                GBIBPGM 
00265          10  FILLER                     PIC X(01) VALUE SPACES.   GBIBPGM 
00266          10  WS-DISP-EFFDT-CT           PIC 9(07).                GBIBPGM 
00267          10  FILLER                     PIC X(01) VALUE SPACES.   GBIBPGM 
00268          10  WS-DISP-TRMDT-CT           PIC 9(07).                GBIBPGM 
00269                                                                   GBIBPGM 
00270                                                                   GBIBPGM 
00271      05  WS-GCDATES-KEY.                                          GBIBPGM 
00272          10  WS-GCDATES-PLAN-CODE             PIC X(03) VALUE ' '.GBIBPGM 
00273          10  WS-GCDATES-GRP-NUMBER.                               GBIBPGM 
00274              15 WS-GCDATES-GRP-NO-1-3         PIC X(03) VALUE ' '.GBIBPGM 
00275              15 WS-GCDATES-GRP-NO             PIC X(06) VALUE ' '.GBIBPGM 
00276          10  WS-GCDATES-SECTN-NUMBER.                             GBIBPGM 
00277              15 WS-GCDATES-SECTN-NO-1         PIC X(01) VALUE ' '.GBIBPGM 
00278              15 WS-GCDATES-SECTN-NO           PIC X(04) VALUE ' '.GBIBPGM 
00279          10  WS-GCDATES-PKG-CODE              PIC X(03) VALUE ' '.GBIBPGM 
00280          10  WS-GCDATES-L-O-B                 PIC X(01) VALUE ' '.GBIBPGM 
00281          10  WS-GCDATES-KEY-FILLER            PIC X(10) VALUE     GBIBPGM 
00282                                                        LOW-VALUES.GBIBPGM 
00283          10  WS-GCDATES-FILE-REF-IND          PIC X(01) VALUE 'C'.GBIBPGM 
00284                                                                   GBIBPGM 
00285      05  WS-CLASS-TEST-AREA                   PIC X(10) VALUE '0'.GBIBPGM 
00286      05  WS-CLASS-TEST-DIGIT REDEFINES                            GBIBPGM 
00287          WS-CLASS-TEST-AREA  OCCURS 10 TIMES  PIC X.              GBIBPGM 
00288          88  WS-CLASS-ALPHANUMERIC            VALUES '0' THRU '9' GBIBPGM 
00289                                                      'A' THRU 'I' GBIBPGM 
00290                                                      'J' THRU 'R' GBIBPGM 
00291                                                      'S' THRU 'Z'.GBIBPGM 
00292                                                                   GBIBPGM 
00293      05  WS-XCTL-TO-PGM                       PIC X(08) VALUE ' '.GBIBPGM 
00294 /* ALTERNATIVE WORKFILE KEYS **                                   GBIBPGM 
00295  01  FILLER                      PIC X(32)  VALUE                 GBIBPGM 
00296      '*** ALTERNATIVE WORKFILE KEY ***'.                          GBIBPGM 
00297  01  WS-ALT-WORKFILE-KEYS.                                        GBIBPGM 
00298  COPY GCWRKKEY.                                                   GBIBPGM 
00299 /                                                                 GBIBPGM 
00300  01  HGADATES-COMMAREA.                                           GBIBPGM 
00301  COPY HGCDAT01.                                                   GBIBPGM 
00302                                                                   GBIBPGM 
00303 **** MIL. DATE *********************                              GBIBPGM 
00304  COPY MLDATE01.                                                   GBIBPGM 
00305 **** HEX COBOL COPYLIB                                            GBIBPGM 
00306  COPY HEXCOBOL.                                                   GBIBPGM 
00307                                                                   GBIBPGM 
00308                                                                   GBIBPGM 
00309                                                                   GBIBPGM 
00310 /* ATTRIBUTES **                                                  GBIBPGM 
00311  COPY DFHBMSCA.                                                   GBIBPGM 
00312      02  DFHBMABF                PIC X VALUE 'Z'.                 GBIBPGM 
00313 /* ATTENTION IDENTIFIERS **                                       GBIBPGM 
00314  COPY DFHAID.                                                     GBIBPGM 
00315 /                                                                 GBIBPGM 
00316  01  GCPS-RECORD-LENGTHS.                                         GBIBPGM 
00317  COPY GCCDRLEN.                                                   GBIBPGM 
00318 ** PROGRAM SPECIFIC RECORD LENGTHS **                             GBIBPGM 
00319      05  WS-GIBC-COM-KEY-LEN          PIC S9(4) COMP   VALUE +17. GBIBPGM 
00320      05  WS-COMM-KEY-LEN              PIC S9(4) COMP   VALUE +150.GBIBPGM 
00321      05  WS-IO-PARM-CONTRACT-LEN      PIC S9(4) COMP   VALUE +0.  GBIBPGM 
00322      05  WS-IO-PARM-GROUPSPC-LEN      PIC S9(4) COMP   VALUE +0.  GBIBPGM 
00323      05  WS-IO-PARM-GCDATES-LEN       PIC S9(4) COMP   VALUE +0.  GBIBPGM 
00324      05  WS-DATES-TABLE-LEN           PIC S9(4) COMP   VALUE +0.  GBIBPGM 
00325      05  WS-DT-FIXED-PORTION          PIC S9(5) COMP-3 VALUE +3.  GBIBPGM 
00326      05  WS-DT-VARIABLE-PORTION       PIC S9(5) COMP-3 VALUE +13. GBIBPGM 
00327                                                                   GBIBPGM 
00328 /* MAP COBOL SCREEN DSECTS **                                     GBIBPGM 
00329  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GBIBPGM 
00330      '***  I/O MAPAREA ***'.                                      GBIBPGM 
00331  COPY GBIBSETC.                                                   GBIBPGM 
00332                                                                   GBIBPGM 
00333                                                                   GBIBPGM 
00334  01  WS-END                      PIC X(16)  VALUE                 GBIBPGM 
00335      '*** W/S ENDS ***'.                                          GBIBPGM 
00336 /                                                                 GBIBPGM 
00337  LINKAGE SECTION.                                                 GBIBPGM 
00338  01  DFHCOMMAREA.                                                 GBIBPGM 
00339  COPY G2COMKEC.                                                   GBIBPGM 
00340      10   GI-REDF-AREA REDEFINES GI-FILLER.                       GBIBPGM 
00341           15  GI-SUBSCRIBER     PIC X(09).                        GBIBPGM 
00342           15  GI-LAST-NAME      PIC X(15).                        GBIBPGM 
00343           15  GI-FIRST-NAME     PIC X(09).                        GBIBPGM 
00344           15  GI-REDF-FILLER    PIC X(03).                        GBIBPGM 
00345 /                                                                 GBIBPGM 
00346  01  IO-PARM-CONTRACT-RECORD.                                     GBIBPGM 
00347  COPY GCIOPRM1.                                                   GBIBPGM 
00348  COPY GCCONTRC.                                                   GBIBPGM 
00349                                                                   GBIBPGM 
00350 /                                                                 GBIBPGM 
00351  01  GI-COMMAREA2-RECORD.                                         GBIBPGM 
00352  COPY G2COMKE2.                                                   GBIBPGM 
00353      10   GI2-REDF-AREA REDEFINES GI2-FILLER.                     GBIBPGM 
00354           15  GI2-SUBSCRIBER     PIC X(09).                       GBIBPGM 
00355           15  GI2-LAST-NAME      PIC X(15).                       GBIBPGM 
00356           15  GI2-FIRST-NAME     PIC X(09).                       GBIBPGM 
00357           15  GI2-REDF-FILLER    PIC X(03).                       GBIBPGM 
00358 /                                                                 GBIBPGM 
00359  01  IO-PARM-GCDATES-RECORD.                                      GBIBPGM 
00360  COPY GCIOPRM2.                                                   GBIBPGM 
00361  COPY GCDATEC5.                                                   GBIBPGM 
00362                                                                   GBIBPGM 
00363 /                                                                 GBIBPGM 
00364  01  IO-PARM-GROUPSPC-RECORD.                                     GBIBPGM 
00365  COPY GCIOPRM3.                                                   GBIBPGM 
00366  COPY GCGROUPC.                                                   GBIBPGM 
00367                                                                   GBIBPGM 
00368 /*****************************************************************GBIBPGM 
00369 *                                                                *GBIBPGM 
00370 *                      DATES TABLE                               *GBIBPGM 
00371 *                                                                *GBIBPGM 
00372 ******************************************************************GBIBPGM 
00373  01  DATES-TABLE.                                                 GBIBPGM 
00374      05  DT-ENTRY-COUNT                          PIC S9(5) COMP-3.GBIBPGM 
00375      05  DT-ENTRY               OCCURS 1 TO 410 TIMES             GBIBPGM 
00376                                 DEPENDING ON DT-ENTRY-COUNT.      GBIBPGM 
00377          10  DT-EFF-DATE.                                         GBIBPGM 
00378              15 DT-EFF-DT-CC                     PIC X(01).       GBIBPGM 
00379              15 DT-EFF-DT                        PIC S9(5) COMP-3.GBIBPGM 
00380          10  DT-EFF-DT-CEN   REDEFINES                            GBIBPGM 
00381              DT-EFF-DATE                         PIC S9(7) COMP-3.GBIBPGM 
00382          10  DT-L-O-B                            PIC X(1).        GBIBPGM 
00383          10  DT-PROV-CNTL                        PIC X(2).        GBIBPGM 
00384          10  DT-FAM-REL                          PIC X(2).        GBIBPGM 
00385          10  DT-TERM-DATE.                                        GBIBPGM 
00386              15 DT-TERM-DT-CC                    PIC X(01).       GBIBPGM 
00387              15 DT-TERM-DT                       PIC S9(5) COMP-3.GBIBPGM 
00388          10  DT-TERM-DT-CEN  REDEFINES                            GBIBPGM 
00389              DT-TERM-DATE                        PIC S9(7) COMP-3.GBIBPGM 
00390 /                                                                 GBIBPGM 
00391  PROCEDURE DIVISION.                                              GBIBPGM 
00392                                                                   GBIBPGM 
00393 ******************************************************************GBIBPGM 
00394 **                                                               *GBIBPGM 
00395 ** 1000                M A I N L I N E                           *GBIBPGM 
00396 **                                                               *GBIBPGM 
00397 ******************************************************************GBIBPGM 
00398  1000-000-MAIN-LINE SECTION.                                      GBIBPGM 
00399  1000-010.                                                        GBIBPGM 
00400                                                                   GBIBPGM 
00401      MOVE '1000'  TO  WS-PARA-ID.                                 GBIBPGM 
00402                                                                   GBIBPGM 
00403      MOVE  LOW-VALUES  TO  WS-HEX-00.                             GBIBPGM 
00404                                                                   GBIBPGM 
00405      EXEC CICS HANDLE CONDITION                                   GBIBPGM 
00406                       MAPFAIL(6000-000-XCTL-TO-MAIN-MENU)         GBIBPGM 
00407                       END-EXEC.                                   GBIBPGM 
00408                                                                   GBIBPGM 
00409 *        +----------------------------------------+               GBIBPGM 
00410 *        +  ACQUIRE STORAGE FOR COMMAREA FOR      +               GBIBPGM 
00411 *        +  LOWER LEVEL MAINLINE MODULES          +               GBIBPGM 
00412 *        +----------------------------------------+               GBIBPGM 
00413                                                                   GBIBPGM 
00414      EXEC CICS GETMAIN  SET (ADDRESS OF GI-COMMAREA2-RECORD)      GBIBPGM 
00415                         INITIMG(WS-HEX-00)                        GBIBPGM 
00416                         LENGTH (WS-COMM-KEY-LEN)                  GBIBPGM 
00417                         END-EXEC.                                 GBIBPGM 
00418                                                                   GBIBPGM 
00419 *        +----------------------------------------+               GBIBPGM 
00420 *        +  IS MY TRANSACTION CODE?               +               GBIBPGM 
00421 *        +----------------------------------------+               GBIBPGM 
00422                                                                   GBIBPGM 
00423 *    IF EIBTRNID  NOT =  'GBIB'                                   GBIBPGM 
00424 *       PERFORM 3000-000-DISPLAY-FIRST-SCREEN                     GBIBPGM 
00425 *       GO TO 1000-800-RETURN.                                    GBIBPGM 
00426                                                                   GBIBPGM 
00427                                                                   GBIBPGM 
00428      IF EIBTRNID      =  'GBIC'                                   GBIBPGM 
00429         PERFORM 3000-000-DISPLAY-FIRST-SCREEN                     GBIBPGM 
00430         GO TO 1000-800-RETURN.                                    GBIBPGM 
00431                                                                   GBIBPGM 
00432      IF EIBTRNID      =  'GHIL' OR 'GBIG'                         GBIBPGM 
00433         PERFORM 9300-000-LOAD-GHIL-KEY                            GBIBPGM 
00434         PERFORM 2000-000-SCREEN-PROCESSING                        GBIBPGM 
00435         IF (NOT WS-FULL-CONTRACT-KEY-ENTERED) AND                 GBIBPGM 
00436            (WS-ERROR-SWITCH   = '0')                              GBIBPGM 
00437 ***        IF  DT-ENTRY-COUNT < +2                                GBIBPGM 
00438            IF  WS-DT-SUBX     < +2                                GBIBPGM 
00439                PERFORM 2000-000-SCREEN-PROCESSING                 GBIBPGM 
00440            END-IF                                                 GBIBPGM 
00441         END-IF                                                    GBIBPGM 
00442         GO TO 1000-800-RETURN.                                    GBIBPGM 
00443 *        +----------------------------------------+               GBIBPGM 
00444 *        + IS THIS SCREEN MINE?                   +               GBIBPGM 
00445 *        +----------------------------------------+               GBIBPGM 
00446                                                                   GBIBPGM 
00447      PERFORM 9400-000-RECEIVE-SCR                                 GBIBPGM 
00448                                                                   GBIBPGM 
00449      IF IBSCRIDI  NOT =  '00IB0I'                                 GBIBPGM 
00450         PERFORM 6000-000-XCTL-TO-MAIN-MENU.                       GBIBPGM 
00451                                                                   GBIBPGM 
00452 *        +----------------------------------------+               GBIBPGM 
00453 *        + ENTER KEYED?                           +               GBIBPGM 
00454 *        +----------------------------------------+               GBIBPGM 
00455                                                                   GBIBPGM 
00456                                                                   GBIBPGM 
00457      IF EIBAID  =  DFHPF7     OR   DFHPF19  OR                    GBIBPGM 
00458                    DFHPF8     OR   DFHPF20                        GBIBPGM 
00459         PERFORM 2000-000-SCREEN-PROCESSING                        GBIBPGM 
00460         GO TO 1000-800-RETURN.                                    GBIBPGM 
00461                                                                   GBIBPGM 
00462                                                                   GBIBPGM 
00463      IF EIBAID  =  DFHENTER OR   DFHPF9                           GBIBPGM 
00464         PERFORM 2000-000-SCREEN-PROCESSING                        GBIBPGM 
00465         IF (NOT WS-FULL-CONTRACT-KEY-ENTERED) AND                 GBIBPGM 
00466            (WS-ERROR-SWITCH   = '0')                              GBIBPGM 
00467 ***        IF  DT-ENTRY-COUNT < +2                                GBIBPGM 
00468            IF  WS-DT-SUBX     < +2                                GBIBPGM 
00469                PERFORM 2000-000-SCREEN-PROCESSING                 GBIBPGM 
00470            END-IF                                                 GBIBPGM 
00471         END-IF                                                    GBIBPGM 
00472         GO TO 1000-800-RETURN.                                    GBIBPGM 
00473                                                                   GBIBPGM 
00474 *        +----------------------------------------+               GBIBPGM 
00475 *        + REQUEST PREVIOUS MENU?                 +               GBIBPGM 
00476 *        +----------------------------------------+               GBIBPGM 
00477                                                                   GBIBPGM 
00478      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GBIBPGM 
00479         MOVE  IBPLNXO   TO   GIC2-PLAN-CODE                       GBIBPGM 
00480         MOVE  IBGRPXO   TO   GIC2-GROUP-NUM                       GBIBPGM 
00481         MOVE  IBSECXO   TO   GIC2-SECTION-NUM                     GBIBPGM 
00482         MOVE  IBRETCO    TO   GI2-RETURN-CODE                     GBIBPGM 
00483         MOVE  IBSUBO      TO   GI2-SUBSCRIBER                     GBIBPGM 
00484         EXEC CICS XCTL  PROGRAM('GHILPGM')                        GBIBPGM 
00485                         COMMAREA(GI-COMMAREA2-RECORD)             GBIBPGM 
00486                         LENGTH  (WS-COMM-KEY-LEN)                 GBIBPGM 
00487                         END-EXEC.                                 GBIBPGM 
00488                                                                   GBIBPGM 
00489                                                                   GBIBPGM 
00490 *        +----------------------------------------+               GBIBPGM 
00491 *        + INVALID REQUEST PROCESS                +               GBIBPGM 
00492 *        +----------------------------------------+               GBIBPGM 
00493                                                                   GBIBPGM 
00494      MOVE '*** INVALID REQUEST. THE PF KEY USED HAS NO MEANING TO GBIBPGM 
00495 -         'THIS PROGRAM ***'  TO  IBMSGO(1).                      GBIBPGM 
00496      MOVE -1                  TO  IBGRPXL.                        GBIBPGM 
00497      PERFORM 7500-000-SEND-DATAONLY.                              GBIBPGM 
00498                                                                   GBIBPGM 
00499                                                                   GBIBPGM 
00500  1000-800-RETURN.                                                 GBIBPGM 
00501                                                                   GBIBPGM 
00502      EXEC CICS RETURN END-EXEC.                                   GBIBPGM 
00503                                                                   GBIBPGM 
00504      GOBACK.                                                      GBIBPGM 
00505                                                                   GBIBPGM 
00506  1000-900-EXIT.                                                   GBIBPGM 
00507      EXIT.                                                        GBIBPGM 
00508 /*****************************************************************GBIBPGM 
00509 **                                                               *GBIBPGM 
00510 ** 2000  SCREEN PROCESSING                                       *GBIBPGM 
00511 **                                                               *GBIBPGM 
00512 ******************************************************************GBIBPGM 
00513  2000-000-SCREEN-PROCESSING    SECTION.                           GBIBPGM 
00514  2000-010.                                                        GBIBPGM 
00515                                                                   GBIBPGM 
00516      MOVE '2000'  TO  WS-PARA-ID.                                 GBIBPGM 
00517                                                                   GBIBPGM 
00518 *    +-------------------------------------------------------+    GBIBPGM 
00519 *    +   IF THE GROUP NUMBER IS FILLED, AND BOTH THE         +    GBIBPGM 
00520 *    +   SECTION NUMBER AND OPTION IS NOT FILLED, TRANSFER   +    GBIBPGM 
00521 *    +   CONTROL TO THE GROUP/SECTION INQUIRY PROGRAM.       +    GBIBPGM 
00522 *    +   (GIBCPGM)                                           +    GBIBPGM 
00523 *    +-------------------------------------------------------+    GBIBPGM 
00524                                                                   GBIBPGM 
00525 *    IF  IBPLNXL > ZERO                                           GBIBPGM 
00526 *      IF  IBGRPXL > ZERO                                         GBIBPGM 
00527 *        IF  (IBSECXL NOT  >  ZERO OR IBPKGXL NOT > ZERO)         GBIBPGM 
00528 *            IF  IBOPTL NOT  >  ZERO                              GBIBPGM 
00529 *                IF  IBOPTO  NOT  =   ('1'  OR  '2'  OR  '3')     GBIBPGM 
00530 *                    MOVE IBPLNXO  TO  WS-GIBC-PLAN-CODE          GBIBPGM 
00531 *                    MOVE IBGRPXO  TO  WS-GIBC-GROUP-NUM          GBIBPGM 
00532 *                    MOVE IBSECXO  TO  WS-GIBC-SECTION-NUM        GBIBPGM 
00533 *                    EXEC CICS XCTL PROGRAM  ('GIBCPGM')          GBIBPGM 
00534 *                         COMMAREA (WS-GIBC-COMMAREA)             GBIBPGM 
00535 *                         LENGTH   (LENGTH OF WS-GIBC-COMMAREA)   GBIBPGM 
00536 *                    END-EXEC.                                    GBIBPGM 
00537                                                                   GBIBPGM 
00538      MOVE SPACES   TO WS-CONTRACT-ID                              GBIBPGM 
00539                       IBMSGO(1)                                   GBIBPGM 
00540                       IBMSGO(2).                                  GBIBPGM 
00541                                                                   GBIBPGM 
00542      MOVE DFHBMUNF TO IBPLNXA                                     GBIBPGM 
00543                       IBGRPXA                                     GBIBPGM 
00544                       IBSECXA                                     GBIBPGM 
00545                       IBPKGXA                                     GBIBPGM 
00546                       IBLOBXA                                     GBIBPGM 
00547                       IBPRVXA                                     GBIBPGM 
00548                       IBFRXA                                      GBIBPGM 
00549                       IBEDTXA                                     GBIBPGM 
00550                       IBLNNOA.                                    GBIBPGM 
00551                                                                   GBIBPGM 
00552 *        +----------------------------------------+               GBIBPGM 
00553 *        +  FIELDS ARE EDITED IN THE REVERSE      +               GBIBPGM 
00554 *        +  ORDER FROM WHICH THEY APPEAR ON THE   +               GBIBPGM 
00555 *        +  SCREEN TO SYNC CURSOR POSITIONING     +               GBIBPGM 
00556 *        +  AND ERROR MESSAGE.                    +               GBIBPGM 
00557 *        +----------------------------------------+               GBIBPGM 
00558                                                                   GBIBPGM 
00559      IF  IBLNNOL > 0                                              GBIBPGM 
00560          IF  IBLNNOO > ZERO    AND   NOT > IBLNCNTO               GBIBPGM 
00561              MOVE '1' TO WS-ENTERED-IBLNNO                        GBIBPGM 
00562          ELSE                                                     GBIBPGM 
00563              MOVE -1        TO IBLNNOL                            GBIBPGM 
00564              MOVE DFHBMUBF  TO IBLNNOA                            GBIBPGM 
00565              MOVE '1'       TO WS-ERROR-SWITCH                    GBIBPGM 
00566              MOVE 'INVALID CONTRACT LINE NUMBER SELECTION'        GBIBPGM 
00567                TO IBMSGO(1)                                       GBIBPGM 
00568      ELSE                                                         GBIBPGM 
00569          NEXT SENTENCE.                                           GBIBPGM 
00570                                                                   GBIBPGM 
00571                                                                   GBIBPGM 
00572 *        +----------------------------------------+               GBIBPGM 
00573 *        +  OPTION SELECTION                      +               GBIBPGM 
00574 *        +----------------------------------------+               GBIBPGM 
00575                                                                   GBIBPGM 
00576 *    IF  IBOPTL > 0                                               GBIBPGM 
00577 *        IF  IBOPTO = '1' OR '2' OR '3'                           GBIBPGM 
00578 *            MOVE '1' TO WS-ENTERED-IBOPT                         GBIBPGM 
00579 *        ELSE                                                     GBIBPGM 
00580 *            MOVE -1        TO IBOPTL                             GBIBPGM 
00581 *            MOVE DFHBMUBF  TO IBOPTA                             GBIBPGM 
00582 *            MOVE '1'       TO WS-ERROR-SWITCH                    GBIBPGM 
00583 *            MOVE 'INVALID SELECT OPTION'                         GBIBPGM 
00584 *              TO IBMSGO(1)                                       GBIBPGM 
00585 *    ELSE                                                         GBIBPGM 
00586 *        NEXT SENTENCE.                                           GBIBPGM 
00587                                                                   GBIBPGM 
00588 *        +----------------------------------------+               GBIBPGM 
00589 *        +  EFFECTIVE DATE                        +               GBIBPGM 
00590 *        +----------------------------------------+               GBIBPGM 
00591                                                                   GBIBPGM 
00592      IF  IBEDTXL > 0                                              GBIBPGM 
00593          IF  IBEDTXO  IS NUMERIC                                  GBIBPGM 
00594              MOVE IBEDTXO TO HGADATE-DATE1                        GBIBPGM 
00595              PERFORM 9200-000-GREGORIAN-TO-JULIAN                 GBIBPGM 
00596              IF  HGADATE-RETURN = ZEROS                           GBIBPGM 
00597                  MOVE '1'             TO WS-ENTERED-IBEDTX        GBIBPGM 
00598                  IF HGADATE-JULIAN2 < +70000                      GBIBPGM 
00599                     MOVE HEX-20          TO WS-EFF-DT-CC          GBIBPGM 
00600                     MOVE HGADATE-JULIAN2 TO WS-EFF-DT             GBIBPGM 
00601                  ELSE                                             GBIBPGM 
00602                     MOVE HEX-19          TO WS-EFF-DT-CC          GBIBPGM 
00603                     MOVE HGADATE-JULIAN2 TO WS-EFF-DT             GBIBPGM 
00604                  END-IF                                           GBIBPGM 
00605                  MOVE IBEDTXO         TO GI2-EFFECTIVE-DATE       GBIBPGM 
00606              ELSE                                                 GBIBPGM 
00607                  MOVE -1        TO IBEDTXL                        GBIBPGM 
00608                  MOVE DFHBMUBF  TO IBEDTXA                        GBIBPGM 
00609                  MOVE '1'       TO WS-ERROR-SWITCH                GBIBPGM 
00610                  MOVE 'INVALID EFFECTIVE DATE'                    GBIBPGM 
00611                    TO IBMSGO(1)                                   GBIBPGM 
00612          ELSE                                                     GBIBPGM 
00613              MOVE -1        TO IBEDTXL                            GBIBPGM 
00614              MOVE DFHBMUBF  TO IBEDTXA                            GBIBPGM 
00615              MOVE '1'       TO WS-ERROR-SWITCH                    GBIBPGM 
00616              MOVE 'EFFECTIVE DATE MUST BE NUMERIC'                GBIBPGM 
00617                TO IBMSGO(1)                                       GBIBPGM 
00618      ELSE                                                         GBIBPGM 
00619          NEXT SENTENCE.                                           GBIBPGM 
00620                                                                   GBIBPGM 
00621 *        +----------------------------------------+               GBIBPGM 
00622 *        +  FAMILY RELATIONSHIP LEVEL             +               GBIBPGM 
00623 *        +----------------------------------------+               GBIBPGM 
00624                                                                   GBIBPGM 
00625      IF  IBFRXL > 0                                               GBIBPGM 
00626 *        IF  IBFRXO   IS NUMERIC OR                               GBIBPGM 
00627 *            IBFRXO   IS ALPHABETIC                               GBIBPGM 
00628          MOVE IBFRXO TO WS-CLASS-TEST-AREA                        GBIBPGM 
00629          IF  WS-CLASS-ALPHANUMERIC(1) AND                         GBIBPGM 
00630              WS-CLASS-ALPHANUMERIC(2)                             GBIBPGM 
00631              MOVE '1'    TO WS-ENTERED-IBFRX                      GBIBPGM 
00632              MOVE IBFRXO TO WS-FAM-REL-LVL                        GBIBPGM 
00633          ELSE                                                     GBIBPGM 
00634              MOVE -1        TO IBFRXL                             GBIBPGM 
00635              MOVE DFHBMUBF  TO IBFRXA                             GBIBPGM 
00636              MOVE '1'       TO WS-ERROR-SWITCH                    GBIBPGM 
00637              MOVE 'FAMILY RELATIONSHIP MUST BE ALPHANUMERIC'      GBIBPGM 
00638                TO IBMSGO(1)                                       GBIBPGM 
00639      ELSE                                                         GBIBPGM 
00640          NEXT SENTENCE.                                           GBIBPGM 
00641                                                                   GBIBPGM 
00642 *        +----------------------------------------+               GBIBPGM 
00643 *        +  PROVIDER CONTROL                      +               GBIBPGM 
00644 *        +----------------------------------------+               GBIBPGM 
00645                                                                   GBIBPGM 
00646      IF  IBPRVXL > 0                                              GBIBPGM 
00647          MOVE IBPRVXO TO WS-CLASS-TEST-AREA                       GBIBPGM 
00648          IF  WS-CLASS-ALPHANUMERIC(1) AND                         GBIBPGM 
00649              WS-CLASS-ALPHANUMERIC(2)                             GBIBPGM 
00650              MOVE '1'     TO WS-ENTERED-IBPRVX                    GBIBPGM 
00651              MOVE IBPRVXO TO WS-PROV-CTL                          GBIBPGM 
00652          ELSE                                                     GBIBPGM 
00653              MOVE -1        TO IBPRVXL                            GBIBPGM 
00654              MOVE DFHBMUBF  TO IBPRVXA                            GBIBPGM 
00655              MOVE '1'       TO WS-ERROR-SWITCH                    GBIBPGM 
00656              MOVE 'PROVIDER CONTROL MUST BE ALPHANUMERIC'         GBIBPGM 
00657                TO IBMSGO(1)                                       GBIBPGM 
00658      ELSE                                                         GBIBPGM 
00659          NEXT SENTENCE.                                           GBIBPGM 
00660                                                                   GBIBPGM 
00661 *        +----------------------------------------+               GBIBPGM 
00662 *        +  LINE OF BUSINESS                      +               GBIBPGM 
00663 *        +----------------------------------------+               GBIBPGM 
00664                                                                   GBIBPGM 
00665      IF  IBLOBXL > 0                                              GBIBPGM 
00666          IF  IBLOBXO  IS NUMERIC                                  GBIBPGM 
00667              MOVE '1'     TO WS-ENTERED-IBLOBX                    GBIBPGM 
00668              MOVE IBLOBXO TO WS-L-O-B                             GBIBPGM 
00669          ELSE                                                     GBIBPGM 
00670              MOVE -1        TO IBLOBXL                            GBIBPGM 
00671              MOVE DFHBMUBF  TO IBLOBXA                            GBIBPGM 
00672              MOVE '1'       TO WS-ERROR-SWITCH                    GBIBPGM 
00673              MOVE 'LINE OF BUSINESS MUST BE NUMERIC'              GBIBPGM 
00674                TO IBMSGO(1)                                       GBIBPGM 
00675      ELSE                                                         GBIBPGM 
00676          NEXT SENTENCE.                                           GBIBPGM 
00677                                                                   GBIBPGM 
00678 *        +----------------------------------------+               GBIBPGM 
00679 *        +  PACKAGE CODE                          +               GBIBPGM 
00680 *        +----------------------------------------+               GBIBPGM 
00681                                                                   GBIBPGM 
00682      IF  IBPKGXL > 0                                              GBIBPGM 
00683          MOVE IBPKGXO TO WS-CLASS-TEST-AREA                       GBIBPGM 
00684          IF  WS-CLASS-ALPHANUMERIC(1) AND                         GBIBPGM 
00685              WS-CLASS-ALPHANUMERIC(2) AND                         GBIBPGM 
00686              WS-CLASS-ALPHANUMERIC(3)                             GBIBPGM 
00687              MOVE '1'     TO WS-ENTERED-IBPKGX                    GBIBPGM 
00688              MOVE IBPKGXO TO WS-PKG-CODE                          GBIBPGM 
00689          ELSE                                                     GBIBPGM 
00690              MOVE -1        TO IBPKGXL                            GBIBPGM 
00691              MOVE DFHBMUBF  TO IBPKGXA                            GBIBPGM 
00692              MOVE '1'       TO WS-ERROR-SWITCH                    GBIBPGM 
00693              MOVE 'PACKAGE CODE MUST BE ALPHANUMERIC'             GBIBPGM 
00694                TO IBMSGO(1)                                       GBIBPGM 
00695      ELSE                                                         GBIBPGM 
00696 *****IF PROGRAM IS IN ILLINOIS REGION, DEFAULT PACKAGE CODE       GBIBPGM 
00697 *****TO ZEROES IF GROUP AND SECTION WERE ENTERED                  GBIBPGM 
00698          EXEC CICS                                                GBIBPGM 
00699               ASSIGN                                              GBIBPGM 
00700               SYSID(WS-SYSID)                                     GBIBPGM 
00701          END-EXEC                                                 GBIBPGM 
00702          IF WS-TEXAS-CICS-REGION                                  GBIBPGM 
00703              NEXT SENTENCE                                        GBIBPGM 
00704          ELSE                                                     GBIBPGM 
00705              IF IBSECXL > ZEROES AND IBGRPXL > ZEROES             GBIBPGM 
00706                  MOVE +3      TO IBPKGXL                          GBIBPGM 
00707                  MOVE ZEROES  TO IBPKGXO                          GBIBPGM 
00708                  MOVE '1'     TO WS-ENTERED-IBPKGX                GBIBPGM 
00709                  MOVE IBPKGXO TO WS-PKG-CODE.                     GBIBPGM 
00710                                                                   GBIBPGM 
00711 *        +----------------------------------------+               GBIBPGM 
00712 *        +  SECTION                               +               GBIBPGM 
00713 *        +----------------------------------------+               GBIBPGM 
00714                                                                   GBIBPGM 
00715      IF  IBSECXL > 0                                              GBIBPGM 
00716        INSPECT IBSECXO REPLACING  ALL ' '  BY LOW-VALUES          GBIBPGM 
00717          MOVE IBSECXO TO WS-CLASS-TEST-AREA                       GBIBPGM 
00718          IF  WS-CLASS-ALPHANUMERIC(1) AND                         GBIBPGM 
00719              WS-CLASS-ALPHANUMERIC(2) AND                         GBIBPGM 
00720              WS-CLASS-ALPHANUMERIC(3) AND                         GBIBPGM 
00721              WS-CLASS-ALPHANUMERIC(4) AND                         GBIBPGM 
00722              WS-CLASS-ALPHANUMERIC(5)                             GBIBPGM 
00723              MOVE '1'     TO WS-ENTERED-IBSECX                    GBIBPGM 
00724              MOVE IBSECXO TO WS-SECTN-NUMBER                      GBIBPGM 
00725          ELSE                                                     GBIBPGM 
00726              MOVE -1        TO IBSECXL                            GBIBPGM 
00727              MOVE DFHBMUBF  TO IBSECXA                            GBIBPGM 
00728              MOVE '1'       TO WS-ERROR-SWITCH                    GBIBPGM 
00729              MOVE 'INVALID SECTION, MUST BE ALPHANUMERIC'         GBIBPGM 
00730                TO IBMSGO(1)                                       GBIBPGM 
00731      ELSE                                                         GBIBPGM 
00732          MOVE -1        TO IBSECXL                                GBIBPGM 
00733          MOVE DFHBMUBF  TO IBSECXA                                GBIBPGM 
00734          MOVE '1'       TO WS-ERROR-SWITCH                        GBIBPGM 
00735          MOVE 'SECTION NUMBER IS A REQUIRED FIELD'                GBIBPGM 
00736            TO IBMSGO(1).                                          GBIBPGM 
00737                                                                   GBIBPGM 
00738 *        +----------------------------------------+               GBIBPGM 
00739 *        +  GROUP                                 +               GBIBPGM 
00740 *        +----------------------------------------+               GBIBPGM 
00741                                                                   GBIBPGM 
00742      IF  IBGRPXL > 0                                              GBIBPGM 
00743        INSPECT IBGRPXO REPLACING  ALL ' '  BY LOW-VALUES          GBIBPGM 
00744          MOVE IBGRPXO TO WS-CLASS-TEST-AREA                       GBIBPGM 
00745          IF  WS-CLASS-ALPHANUMERIC(1) AND                         GBIBPGM 
00746              WS-CLASS-ALPHANUMERIC(2) AND                         GBIBPGM 
00747              WS-CLASS-ALPHANUMERIC(3) AND                         GBIBPGM 
00748              WS-CLASS-ALPHANUMERIC(4) AND                         GBIBPGM 
00749              WS-CLASS-ALPHANUMERIC(5) AND                         GBIBPGM 
00750              WS-CLASS-ALPHANUMERIC(6) AND                         GBIBPGM 
00751              WS-CLASS-ALPHANUMERIC(7) AND                         GBIBPGM 
00752              WS-CLASS-ALPHANUMERIC(8) AND                         GBIBPGM 
00753              WS-CLASS-ALPHANUMERIC(9)                             GBIBPGM 
00754              MOVE '1'     TO WS-ENTERED-IBGRPX                    GBIBPGM 
00755              MOVE IBGRPXO TO WS-GRP-NUMBER                        GBIBPGM 
00756          ELSE                                                     GBIBPGM 
00757              MOVE -1        TO IBGRPXL                            GBIBPGM 
00758              MOVE DFHBMUBF  TO IBGRPXA                            GBIBPGM 
00759              MOVE '1'       TO WS-ERROR-SWITCH                    GBIBPGM 
00760              MOVE 'INVALID GROUP, MUST BE ALPHANUMERIC'           GBIBPGM 
00761                TO IBMSGO(1)                                       GBIBPGM 
00762      ELSE                                                         GBIBPGM 
00763          MOVE -1        TO IBGRPXL                                GBIBPGM 
00764          MOVE DFHBMUBF  TO IBGRPXA                                GBIBPGM 
00765          MOVE '1'       TO WS-ERROR-SWITCH                        GBIBPGM 
00766          MOVE 'GROUP NUMBER IS A REQUIRED FIELD'                  GBIBPGM 
00767            TO IBMSGO(1).                                          GBIBPGM 
00768                                                                   GBIBPGM 
00769 *        +----------------------------------------+               GBIBPGM 
00770 *        +  PLAN CODE                             +               GBIBPGM 
00771 *        +----------------------------------------+               GBIBPGM 
00772                                                                   GBIBPGM 
00773      IF  IBPLNXL > 0                                              GBIBPGM 
00774          MOVE IBPLNXO TO WS-CLASS-TEST-AREA                       GBIBPGM 
00775          IF  WS-CLASS-ALPHANUMERIC(1) AND                         GBIBPGM 
00776              WS-CLASS-ALPHANUMERIC(2) AND                         GBIBPGM 
00777              WS-CLASS-ALPHANUMERIC(3)                             GBIBPGM 
00778              MOVE '1'     TO WS-ENTERED-IBPLNX                    GBIBPGM 
00779              MOVE IBPLNXO TO WS-PLAN-CODE                         GBIBPGM 
00780          ELSE                                                     GBIBPGM 
00781              MOVE -1        TO IBPLNXL                            GBIBPGM 
00782              MOVE DFHBMUBF  TO IBPLNXA                            GBIBPGM 
00783              MOVE '1'       TO WS-ERROR-SWITCH                    GBIBPGM 
00784              MOVE 'PLAN CODE MUST BE NUMERIC '                    GBIBPGM 
00785                TO IBMSGO(1)                                       GBIBPGM 
00786      ELSE                                                         GBIBPGM 
00787          MOVE -1        TO IBPLNXL                                GBIBPGM 
00788          MOVE DFHBMUBF  TO IBPLNXA                                GBIBPGM 
00789          MOVE '1'       TO WS-ERROR-SWITCH                        GBIBPGM 
00790          MOVE 'PLAN CODE IS A REQUIRED FIELD'                     GBIBPGM 
00791            TO IBMSGO(1).                                          GBIBPGM 
00792                                                                   GBIBPGM 
00793                                                                   GBIBPGM 
00794 *        +----------------------------------------+               GBIBPGM 
00795 *        +  CONTRACT LINE NUMBER SELECTION        +               GBIBPGM 
00796 *        +----------------------------------------+               GBIBPGM 
00797                                                                   GBIBPGM 
00798      IF  NOT WS-FULL-CONTRACT-KEY-ENTERED AND                     GBIBPGM 
00799              WS-ERROR-SWITCH   = '0'      AND                     GBIBPGM 
00800              IBLNCNTO          >  00      AND                     GBIBPGM 
00801              WS-ENTERED-IBLNNO = '0'      AND                     GBIBPGM 
00802          (EIBAID  NOT = DFHPF7    AND   DFHPF19 AND               GBIBPGM 
00803                         DFHPF8    AND   DFHPF20)                  GBIBPGM 
00804          MOVE -1        TO IBLNNOL                                GBIBPGM 
00805          MOVE DFHBMUBF  TO IBLNNOA                                GBIBPGM 
00806          MOVE '1'       TO WS-ERROR-SWITCH                        GBIBPGM 
00807          MOVE 'CONTRACT LINE NUMBER REQUIRED'                     GBIBPGM 
00808            TO IBMSGO(1)                                           GBIBPGM 
00809      ELSE                                                         GBIBPGM 
00810          NEXT SENTENCE.                                           GBIBPGM 
00811                                                                   GBIBPGM 
00812 *        +----------------------------------------+               GBIBPGM 
00813 *        +  SELECTION OPTION                      +               GBIBPGM 
00814 *        +----------------------------------------+               GBIBPGM 
00815                                                                   GBIBPGM 
00816                                                                   GBIBPGM 
00817 *        +----------------------------------------+               GBIBPGM 
00818 *        +                                        +               GBIBPGM 
00819 *        +  IF A PARTIAL CONTRACT KEY HAS BEEN    +               GBIBPGM 
00820 *        +    KEYED AND THERE IS A SELECTION LIST +               GBIBPGM 
00821 *        +    ALREADY ON THE SCREEN AND THE       +               GBIBPGM 
00822 *        +    OPERATOR HAS MADE A VALID SELECTION,+               GBIBPGM 
00823 *        +    THEN COMPLETE THE FULL CONTRACT KEY +               GBIBPGM 
00824 *        +    WITH THE SELECTED INFORMATION, SET  +               GBIBPGM 
00825 *        +    ALL SCREEN-KEY-ENTRY-SWITCHES TO 1'S+               GBIBPGM 
00826 *        +    INDICATING A FULL CONTRACT KEY HAS  +               GBIBPGM 
00827 *        +    BEEN ENTERED.                       +               GBIBPGM 
00828 *        +                                        +               GBIBPGM 
00829 *        +----------------------------------------+               GBIBPGM 
00830                                                                   GBIBPGM 
00831      IF  NOT WS-FULL-CONTRACT-KEY-ENTERED AND                     GBIBPGM 
00832              WS-ERROR-SWITCH   = '0'      AND                     GBIBPGM 
00833              IBLNCNTO          >  00      AND                     GBIBPGM 
00834              WS-ENTERED-IBLNNO = '1'                              GBIBPGM 
00835          MOVE IBLNNOO              TO WS-SCREEN-LN                GBIBPGM 
00836          MOVE IBLOBO(WS-SCREEN-LN) TO WS-L-O-B                    GBIBPGM 
00837          MOVE IBPRVO(WS-SCREEN-LN) TO WS-PROV-CTL                 GBIBPGM 
00838          MOVE IBFRO (WS-SCREEN-LN) TO WS-FAM-REL-LVL              GBIBPGM 
00839          MOVE IBEDTO(WS-SCREEN-LN) TO HGADATE-DATE1               GBIBPGM 
00840                                       GI2-EFFECTIVE-DATE          GBIBPGM 
00841          PERFORM 9200-000-GREGORIAN-TO-JULIAN                     GBIBPGM 
00842          IF HGADATE-JULIAN2 < +70000                              GBIBPGM 
00843              MOVE HEX-20           TO WS-EFF-DT-CC                GBIBPGM 
00844              MOVE HGADATE-JULIAN2  TO WS-EFF-DT                   GBIBPGM 
00845          ELSE                                                     GBIBPGM 
00846              MOVE HEX-19           TO WS-EFF-DT-CC                GBIBPGM 
00847              MOVE HGADATE-JULIAN2  TO WS-EFF-DT                   GBIBPGM 
00848          END-IF                                                   GBIBPGM 
00849          MOVE ALL '1'              TO WS-SCREEN-KEY-ENTRY-SWITCHESGBIBPGM 
00850      END-IF.                                                      GBIBPGM 
00851                                                                   GBIBPGM 
00852 *        +----------------------------------------+               GBIBPGM 
00853 *        + IF THERE HAS NOT BEEN ANY ERRORS FOUND +               GBIBPGM 
00854 *        + CONTINUE PROCESSING,  ELSE SEND THE    +               GBIBPGM 
00855 *        + ERROR MESSAGE AND RETURN TO CICS.      +               GBIBPGM 
00856 *        +----------------------------------------+               GBIBPGM 
00857                                                                   GBIBPGM 
00858      IF  WS-ERROR-SWITCH = '0'                                    GBIBPGM 
00859          NEXT SENTENCE                                            GBIBPGM 
00860      ELSE                                                         GBIBPGM 
00861          IF EIBTRNID      =  'GHIL'                               GBIBPGM 
00862             PERFORM 7600-000-SEND-ERASE                           GBIBPGM 
00863          ELSE                                                     GBIBPGM 
00864             PERFORM 7500-000-SEND-DATAONLY                        GBIBPGM 
00865          END-IF                                                   GBIBPGM 
00866          GO TO 2000-900-EXIT                                      GBIBPGM 
00867      END-IF.                                                      GBIBPGM 
00868 *        +----------------------------------------+               GBIBPGM 
00869 *        + IF A FULL CONTRACT KEY HAS BEEN ENTERED+               GBIBPGM 
00870 *        + READ THE CONTRACT AND XCTL TO THE      +               GBIBPGM 
00871 *        + CHOOSEN SCREEN MODULE.                 +               GBIBPGM 
00872 *        +----------------------------------------+               GBIBPGM 
00873                                                                   GBIBPGM 
00874      IF  WS-FULL-CONTRACT-KEY-ENTERED                             GBIBPGM 
00875          PERFORM 2100-000-FULL-KEY-SELECTED                       GBIBPGM 
00876          GO TO 2000-900-EXIT.                                     GBIBPGM 
00877                                                                   GBIBPGM 
00878 *        +----------------------------------------+               GBIBPGM 
00879 *        + IF A PARTIAL CONTRACT KEY HAS BEEN     +               GBIBPGM 
00880 *        + ENTERED, PROCESS IT.                   +               GBIBPGM 
00881 *        +----------------------------------------+               GBIBPGM 
00882                                                                   GBIBPGM 
00883      PERFORM 2200-000-PARTIAL-KEY-SELECTED.                       GBIBPGM 
00884      GO TO 2000-900-EXIT.                                         GBIBPGM 
00885                                                                   GBIBPGM 
00886  2000-900-EXIT.                                                   GBIBPGM 
00887      EXIT.                                                        GBIBPGM 
00888 /*****************************************************************GBIBPGM 
00889 **                                                               *GBIBPGM 
00890 ** 2100     FULL KEY SELECTED                                    *GBIBPGM 
00891 **                                                               *GBIBPGM 
00892 **       1. READ THE CONTRACT                                    *GBIBPGM 
00893 **       2. XCTL TO THE SELECTED SCREEN MODULE.                  *GBIBPGM 
00894 **                                                               *GBIBPGM 
00895 ******************************************************************GBIBPGM 
00896  2100-000-FULL-KEY-SELECTED    SECTION.                           GBIBPGM 
00897  2100-010.                                                        GBIBPGM 
00898                                                                   GBIBPGM 
00899      MOVE '2100'  TO  WS-PARA-ID.                                 GBIBPGM 
00900                                                                   GBIBPGM 
00901      PERFORM 8000-000-READ-CONTRACT.                              GBIBPGM 
00902                                                                   GBIBPGM 
00903      IF  NOT GCIO-GOOD-RETURN                                     GBIBPGM 
00904          MOVE  SPACES  TO  IBPAGEO                                GBIBPGM 
00905          MOVE  -1      TO  IBPLNXL                                GBIBPGM 
00906          MOVE '*** CONTRACT RECORD NOT FOUND ****' TO IBMSGO(1)   GBIBPGM 
00907          MOVE '1' TO WS-ERROR-SWITCH                              GBIBPGM 
00908 ***      PERFORM 7500-000-SEND-DATAONLY                           GBIBPGM 
00909          PERFORM 7600-000-SEND-ERASE                              GBIBPGM 
00910          GO TO 2100-900-EXIT                                      GBIBPGM 
00911      ELSE                                                         GBIBPGM 
00912          MOVE 'Y' TO WS-CONTRACT-FOUND-SW                         GBIBPGM 
00913          MOVE  GCT-PLAN-CODE    TO   WS-DISP-PLAN-CT              GBIBPGM 
00914          MOVE  GCT-GROUP-NUM    TO   WS-DISP-GROUP-CT             GBIBPGM 
00915          MOVE  GCT-SECTION-NUM  TO   WS-DISP-SECTION-CT           GBIBPGM 
00916          MOVE  GCT-PKG-CODE     TO   WS-DISP-PKG-CT               GBIBPGM 
00917          MOVE  GCT-FAM-REL-LVL  TO   WS-DISP-FRL-CT               GBIBPGM 
00918          MOVE  GCT-EFFDT-CEN    TO   WS-DISP-EFFDT-CT             GBIBPGM 
00919          MOVE  GCT-TERMDT-CEN   TO   WS-DISP-TRMDT-CT             GBIBPGM 
00920          MOVE  WS-DISP-CONTRACT       TO IBMSGO(1)                GBIBPGM 
00921 **       MOVE '1' TO WS-ERROR-SWITCH                              GBIBPGM 
00922          PERFORM 4000-000-SEARCH-GROUP-SPEC                       GBIBPGM 
00923      END-IF.                                                      GBIBPGM 
00924                                                                   GBIBPGM 
00925                                                                   GBIBPGM 
00926      IF  GROUP-MATCH-FOUND                                        GBIBPGM 
00927          MOVE  SPACES  TO  IBPAGEO                                GBIBPGM 
00928          MOVE  -1      TO  IBLNNOL                                GBIBPGM 
00929          MOVE  WS-DISP-GROUPSPC      TO IBMSGO(2)                 GBIBPGM 
00930 **       MOVE '1' TO WS-ERROR-SWITCH                              GBIBPGM 
00931          PERFORM 2100-020-XCTL-TO-GBID                            GBIBPGM 
00932 **       PERFORM 7600-000-SEND-ERASE                              GBIBPGM 
00933 **       GO TO 2100-900-EXIT                                      GBIBPGM 
00934      ELSE                                                         GBIBPGM 
00935          PERFORM 5000-000-XCTL-TO-GBIE                            GBIBPGM 
00936 **       MOVE '*** GROUP REC NOT FOUND ON READ***' TO IBMSGO(2)   GBIBPGM 
00937 **       MOVE '1' TO WS-ERROR-SWITCH                              GBIBPGM 
00938 **       PERFORM 7600-000-SEND-ERASE                              GBIBPGM 
00939 **       GO TO 2100-900-EXIT                                      GBIBPGM 
00940      END-IF.                                                      GBIBPGM 
00941                                                                   GBIBPGM 
00942                                                                   GBIBPGM 
00943  2100-020-XCTL-TO-GBID.                                           GBIBPGM 
00944                                                                   GBIBPGM 
00945      MOVE 'GBIDPGM' TO WS-XCTL-TO-PGM                             GBIBPGM 
00946                                                                   GBIBPGM 
00947      MOVE  GCG-PLAN-CODE    TO   GIG2-PLAN-CODE                   GBIBPGM 
00948      MOVE  GCG-GROUP-NUM    TO   GIG2-GROUP-NUM                   GBIBPGM 
00949      MOVE  GCG-SECTION-NUM  TO   GIG2-SECTION-NUM                 GBIBPGM 
00950      MOVE  GCG-PKG-CODE     TO   GIG2-PKG-CODE                    GBIBPGM 
00951      MOVE  GCG-FAM-REL-LVL  TO   GIG2-FAM-REL-LVL                 GBIBPGM 
00952      MOVE  GCG-EFFDT-CEN    TO   GIG2-EFFDT-CEN.                  GBIBPGM 
00953      MOVE  IBSVDTO          TO   GIGT2-SLOT-NUMBER                GBIBPGM 
00954      MOVE  IBSUBO           TO   GI2-SUBSCRIBER.                  GBIBPGM 
00955      MOVE  IBRETCO          TO   GI2-RETURN-CODE                  GBIBPGM 
00956                                                                   GBIBPGM 
00957                                                                   GBIBPGM 
00958      EXEC CICS XCTL PROGRAM (WS-XCTL-TO-PGM)                      GBIBPGM 
00959                     COMMAREA(GI-COMMAREA2-RECORD)                 GBIBPGM 
00960                     LENGTH  (WS-COMM-KEY-LEN)                     GBIBPGM 
00961                     END-EXEC.                                     GBIBPGM 
00962                                                                   GBIBPGM 
00963  2100-900-EXIT.                                                   GBIBPGM 
00964      EXIT.                                                        GBIBPGM 
00965 /*****************************************************************GBIBPGM 
00966 **                                                               *GBIBPGM 
00967 ** 2200     PARTIAL KEY SELECTED                                 *GBIBPGM 
00968 **                                                               *GBIBPGM 
00969 ******************************************************************GBIBPGM 
00970  2200-000-PARTIAL-KEY-SELECTED SECTION.                           GBIBPGM 
00971  2200-010.                                                        GBIBPGM 
00972                                                                   GBIBPGM 
00973      MOVE '2200'  TO  WS-PARA-ID.                                 GBIBPGM 
00974                                                                   GBIBPGM 
00975      MOVE IBPLNXO     TO WS-GCDATES-PLAN-CODE.                    GBIBPGM 
00976      MOVE IBGRPXO     TO WS-GCDATES-GRP-NUMBER.                   GBIBPGM 
00977      MOVE IBSECXO     TO WS-GCDATES-SECTN-NUMBER.                 GBIBPGM 
00978      MOVE IBPKGXO     TO WS-GCDATES-PKG-CODE.                     GBIBPGM 
00979      MOVE LOW-VALUES  TO WS-GCDATES-KEY-FILLER.                   GBIBPGM 
00980      MOVE 'C'         TO WS-GCDATES-FILE-REF-IND.                 GBIBPGM 
00981                                                                   GBIBPGM 
00982 *        +----------------------------------------+               GBIBPGM 
00983 *        +   ATTEMPT TO RETRIEVE GCDATES          +               GBIBPGM 
00984 *        +   RECORDS WITH LOBS 1,2,3, AND 4.      +               GBIBPGM 
00985 *        +----------------------------------------+               GBIBPGM 
00986                                                                   GBIBPGM 
00987                                                                   GBIBPGM 
00988 *        +----------------------------------------+               GBIBPGM 
00989 *        + RETRIEVE BLUE CROSS GCDATES RECORD     +               GBIBPGM 
00990 *        +----------------------------------------+               GBIBPGM 
00991                                                                   GBIBPGM 
00992      MOVE '1'     TO WS-GCDATES-L-O-B.                            GBIBPGM 
00993      PERFORM 8100-000-READ-GCDATES.                               GBIBPGM 
00994      IF  GCIO2-GOOD-RETURN                                        GBIBPGM 
00995          SET WS-GCDATES-BC-POINTER TO ADDRESS OF                  GBIBPGM 
00996                                          IO-PARM-GCDATES-RECORD   GBIBPGM 
00997          MOVE DTE-ENTRY-COUNT TO WS-GCDATES-BC-ENTRY-COUNT        GBIBPGM 
00998      ELSE                                                         GBIBPGM 
00999          MOVE ZEROS           TO WS-GCDATES-BC-ADDR               GBIBPGM 
01000                                  WS-GCDATES-BC-ENTRY-COUNT.       GBIBPGM 
01001                                                                   GBIBPGM 
01002 *        +----------------------------------------+               GBIBPGM 
01003 *        + RETRIEVE BLUE SHIELD GCDATES RECORD    +               GBIBPGM 
01004 *        +----------------------------------------+               GBIBPGM 
01005                                                                   GBIBPGM 
01006      MOVE '2'     TO WS-GCDATES-L-O-B.                            GBIBPGM 
01007      PERFORM 8100-000-READ-GCDATES.                               GBIBPGM 
01008      IF  GCIO2-GOOD-RETURN                                        GBIBPGM 
01009          SET WS-GCDATES-BS-POINTER TO ADDRESS OF                  GBIBPGM 
01010                                          IO-PARM-GCDATES-RECORD   GBIBPGM 
01011          MOVE DTE-ENTRY-COUNT TO WS-GCDATES-BS-ENTRY-COUNT        GBIBPGM 
01012      ELSE                                                         GBIBPGM 
01013          MOVE ZEROS           TO WS-GCDATES-BS-ADDR               GBIBPGM 
01014                                  WS-GCDATES-BS-ENTRY-COUNT.       GBIBPGM 
01015                                                                   GBIBPGM 
01016 *        +----------------------------------------+               GBIBPGM 
01017 *        + RETRIEVE SUPPL MM    GCDATES RECORD    +               GBIBPGM 
01018 *        +----------------------------------------+               GBIBPGM 
01019                                                                   GBIBPGM 
01020      MOVE '3'     TO WS-GCDATES-L-O-B.                            GBIBPGM 
01021      PERFORM 8100-000-READ-GCDATES.                               GBIBPGM 
01022      IF  GCIO2-GOOD-RETURN                                        GBIBPGM 
01023          SET WS-GCDATES-SM-POINTER TO ADDRESS OF                  GBIBPGM 
01024                                          IO-PARM-GCDATES-RECORD   GBIBPGM 
01025          MOVE DTE-ENTRY-COUNT TO WS-GCDATES-SM-ENTRY-COUNT        GBIBPGM 
01026      ELSE                                                         GBIBPGM 
01027          MOVE ZEROS           TO WS-GCDATES-SM-ADDR               GBIBPGM 
01028                                  WS-GCDATES-SM-ENTRY-COUNT.       GBIBPGM 
01029                                                                   GBIBPGM 
01030 *        +----------------------------------------+               GBIBPGM 
01031 *        + RETRIEVE COMP MM     GCDATES RECORD    +               GBIBPGM 
01032 *        +----------------------------------------+               GBIBPGM 
01033                                                                   GBIBPGM 
01034      MOVE '4'     TO WS-GCDATES-L-O-B.                            GBIBPGM 
01035      PERFORM 8100-000-READ-GCDATES.                               GBIBPGM 
01036      IF  GCIO2-GOOD-RETURN                                        GBIBPGM 
01037          SET WS-GCDATES-CM-POINTER TO ADDRESS OF                  GBIBPGM 
01038                                          IO-PARM-GCDATES-RECORD   GBIBPGM 
01039          MOVE DTE-ENTRY-COUNT TO WS-GCDATES-CM-ENTRY-COUNT        GBIBPGM 
01040      ELSE                                                         GBIBPGM 
01041          MOVE ZEROS           TO WS-GCDATES-CM-ADDR               GBIBPGM 
01042                                  WS-GCDATES-CM-ENTRY-COUNT.       GBIBPGM 
01043                                                                   GBIBPGM 
01044 *        +----------------------------------------+               GBIBPGM 
01045 *        + REDUCE EACH GCATES ENTRY COUNT BY 1    +               GBIBPGM 
01046 *        +   TO ACCOUNT FOR THE HIGH-VALUES ENTRY +               GBIBPGM 
01047 *        +   AT THE END OF EACH RECORD.           +               GBIBPGM 
01048 *        +----------------------------------------+               GBIBPGM 
01049                                                                   GBIBPGM 
01050 *-------- REDUCE EACH GCDATES ENTRY COUNT BY 1 TO ACCOUNT FOR     GBIBPGM 
01051 *          THE HIGH-VALUES ENTRY AT THE END OF EACH RECORD        GBIBPGM 
01052                                                                   GBIBPGM 
01053      IF  WS-GCDATES-BC-ENTRY-COUNT > +1                           GBIBPGM 
01054          COMPUTE WS-GCDATES-BC-ENTRY-COUNT =                      GBIBPGM 
01055                  WS-GCDATES-BC-ENTRY-COUNT - 1                    GBIBPGM 
01056      ELSE                                                         GBIBPGM 
01057          MOVE ZEROS TO WS-GCDATES-BC-ADDR                         GBIBPGM 
01058                        WS-GCDATES-BC-ENTRY-COUNT.                 GBIBPGM 
01059                                                                   GBIBPGM 
01060      IF  WS-GCDATES-BS-ENTRY-COUNT > +1                           GBIBPGM 
01061          COMPUTE WS-GCDATES-BS-ENTRY-COUNT =                      GBIBPGM 
01062                  WS-GCDATES-BS-ENTRY-COUNT - 1                    GBIBPGM 
01063      ELSE                                                         GBIBPGM 
01064          MOVE ZEROS TO WS-GCDATES-BS-ADDR                         GBIBPGM 
01065                        WS-GCDATES-BS-ENTRY-COUNT.                 GBIBPGM 
01066                                                                   GBIBPGM 
01067      IF  WS-GCDATES-SM-ENTRY-COUNT > +1                           GBIBPGM 
01068          COMPUTE WS-GCDATES-SM-ENTRY-COUNT =                      GBIBPGM 
01069                  WS-GCDATES-SM-ENTRY-COUNT - 1                    GBIBPGM 
01070      ELSE                                                         GBIBPGM 
01071          MOVE ZEROS TO WS-GCDATES-SM-ADDR                         GBIBPGM 
01072                        WS-GCDATES-SM-ENTRY-COUNT.                 GBIBPGM 
01073                                                                   GBIBPGM 
01074      IF  WS-GCDATES-CM-ENTRY-COUNT > +1                           GBIBPGM 
01075          COMPUTE WS-GCDATES-CM-ENTRY-COUNT =                      GBIBPGM 
01076                  WS-GCDATES-CM-ENTRY-COUNT - 1                    GBIBPGM 
01077      ELSE                                                         GBIBPGM 
01078          MOVE ZEROS TO WS-GCDATES-CM-ADDR                         GBIBPGM 
01079                        WS-GCDATES-CM-ENTRY-COUNT.                 GBIBPGM 
01080                                                                   GBIBPGM 
01081 *        +----------------------------------------+               GBIBPGM 
01082 *        + CHECK TO SEE IF THERE ARE ANY GCDATES  +               GBIBPGM 
01083 *        +   RECORDS TO PROCESS.                  +               GBIBPGM 
01084 *        +----------------------------------------+               GBIBPGM 
01085                                                                   GBIBPGM 
01086      IF  WS-GCDATES-BC-ENTRY-COUNT = ZEROS AND                    GBIBPGM 
01087          WS-GCDATES-BS-ENTRY-COUNT = ZEROS AND                    GBIBPGM 
01088          WS-GCDATES-SM-ENTRY-COUNT = ZEROS AND                    GBIBPGM 
01089          WS-GCDATES-CM-ENTRY-COUNT = ZEROS                        GBIBPGM 
01090          MOVE  SPACES  TO  IBPAGEO                                GBIBPGM 
01091          MOVE  -1      TO  IBGRPXL                                GBIBPGM 
01092          MOVE '1' TO WS-ERROR-SWITCH                              GBIBPGM 
01093          MOVE '** NO CONTRACT DATES RECORD FOUND FOR THIS PLAN, GRGBIBPGM 
01094 -             'P, SECTION, & LOB **'                              GBIBPGM 
01095            TO IBMSGO(1)                                           GBIBPGM 
01096          MOVE '** CORRECT PARTIAL KEY FIELDS ABOVE OR PRESS PF3 TOGBIBPGM 
01097 -             ' RETURN TO PREV MENU'                              GBIBPGM 
01098            TO IBMSGO(2)                                           GBIBPGM 
01099          MOVE ZEROES  TO  IBLNCNTO                                GBIBPGM 
01100 **********   AHL 11/24/86                                         GBIBPGM 
01101          PERFORM   2800-000-CLEAR-MAP-TABLE                       GBIBPGM 
01102                  VARYING WS-CLEAR-INDEX FROM 1 BY 1               GBIBPGM 
01103                  UNTIL   WS-CLEAR-INDEX > WS-SCREEN-MAX-ENTRIES   GBIBPGM 
01104 *************************                                         GBIBPGM 
01105          PERFORM 7600-000-SEND-ERASE                              GBIBPGM 
01106          GO TO 2200-900-EXIT                                      GBIBPGM 
01107      ELSE                                                         GBIBPGM 
01108          NEXT SENTENCE.                                           GBIBPGM 
01109                                                                   GBIBPGM 
01110 *        +----------------------------------------+               GBIBPGM 
01111 *        + COMPUTE THE GETMAIN LENGTH FOR THE     +               GBIBPGM 
01112 *        +   DATES TABLE TO BE BUILD FROM THE     +               GBIBPGM 
01113 *        +   GCDATES RECORD(S).  ACQUIRE THE      +               GBIBPGM 
01114 *        +   STORAGE AND SET THE ENTRY COUNT.     +               GBIBPGM 
01115 *        +----------------------------------------+               GBIBPGM 
01116                                                                   GBIBPGM 
01117      COMPUTE  WS-DATES-TABLE-LEN           =                      GBIBPGM 
01118               WS-DT-FIXED-PORTION          +                      GBIBPGM 
01119             ( WS-DT-VARIABLE-PORTION       *                      GBIBPGM 
01120              (WS-GCDATES-BC-ENTRY-COUNT    +                      GBIBPGM 
01121               WS-GCDATES-BS-ENTRY-COUNT    +                      GBIBPGM 
01122               WS-GCDATES-SM-ENTRY-COUNT    +                      GBIBPGM 
01123               WS-GCDATES-CM-ENTRY-COUNT)).                        GBIBPGM 
01124                                                                   GBIBPGM 
01125      EXEC CICS GETMAIN  SET (ADDRESS OF DATES-TABLE)              GBIBPGM 
01126                         INITIMG(WS-HEX-00)                        GBIBPGM 
01127                         LENGTH (WS-DATES-TABLE-LEN)               GBIBPGM 
01128                         END-EXEC.                                 GBIBPGM 
01129                                                                   GBIBPGM 
01130      COMPUTE  DT-ENTRY-COUNT               =                      GBIBPGM 
01131               WS-GCDATES-BC-ENTRY-COUNT    +                      GBIBPGM 
01132               WS-GCDATES-BS-ENTRY-COUNT    +                      GBIBPGM 
01133               WS-GCDATES-SM-ENTRY-COUNT    +                      GBIBPGM 
01134               WS-GCDATES-CM-ENTRY-COUNT.                          GBIBPGM 
01135                                                                   GBIBPGM 
01136      MOVE +0 TO WS-DT-SUB                                         GBIBPGM 
01137                 WS-DT-SUBX                                        GBIBPGM 
01138                                                                   GBIBPGM 
01139 *        +----------------------------------------+               GBIBPGM 
01140 *        + LOAD DATES TABLE USING GCATES BLUE     +               GBIBPGM 
01141 *        +   CROSS RECORD.                        +               GBIBPGM 
01142 *        +----------------------------------------+               GBIBPGM 
01143                                                                   GBIBPGM 
01144      IF  WS-GCDATES-BC-ADDR > +0                                  GBIBPGM 
01145          SET ADDRESS OF IO-PARM-GCDATES-RECORD TO                 GBIBPGM 
01146                 WS-GCDATES-BC-POINTER                             GBIBPGM 
01147          PERFORM 2300-000-LOAD-DATES-TABLE                        GBIBPGM 
01148             VARYING DTE-INDEX FROM 1 BY 1                         GBIBPGM 
01149               UNTIL DTE-INDEX > WS-GCDATES-BC-ENTRY-COUNT.        GBIBPGM 
01150                                                                   GBIBPGM 
01151 *        +----------------------------------------+               GBIBPGM 
01152 *        + LOAD DATES TABLE USING GCATES BLUE     +               GBIBPGM 
01153 *        +   SHIELD RECORD.                       +               GBIBPGM 
01154 *        +----------------------------------------+               GBIBPGM 
01155                                                                   GBIBPGM 
01156      IF  WS-GCDATES-BS-ADDR > +0                                  GBIBPGM 
01157          SET ADDRESS OF IO-PARM-GCDATES-RECORD TO                 GBIBPGM 
01158                 WS-GCDATES-BS-POINTER                             GBIBPGM 
01159          PERFORM 2300-000-LOAD-DATES-TABLE                        GBIBPGM 
01160             VARYING DTE-INDEX FROM 1 BY 1                         GBIBPGM 
01161               UNTIL DTE-INDEX > WS-GCDATES-BS-ENTRY-COUNT.        GBIBPGM 
01162                                                                   GBIBPGM 
01163 *        +----------------------------------------+               GBIBPGM 
01164 *        + LOAD DATES TABLE USING GCATES SUPPL    +               GBIBPGM 
01165 *        +   MAJOR MED RECORD.                    +               GBIBPGM 
01166 *        +----------------------------------------+               GBIBPGM 
01167                                                                   GBIBPGM 
01168      IF  WS-GCDATES-SM-ADDR > +0                                  GBIBPGM 
01169          SET ADDRESS OF IO-PARM-GCDATES-RECORD TO                 GBIBPGM 
01170                 WS-GCDATES-SM-POINTER                             GBIBPGM 
01171          PERFORM 2300-000-LOAD-DATES-TABLE                        GBIBPGM 
01172             VARYING DTE-INDEX FROM 1 BY 1                         GBIBPGM 
01173               UNTIL DTE-INDEX > WS-GCDATES-SM-ENTRY-COUNT.        GBIBPGM 
01174                                                                   GBIBPGM 
01175 *        +----------------------------------------+               GBIBPGM 
01176 *        + LOAD DATES TABLE USING GCATES COMP     +               GBIBPGM 
01177 *        +   MAJOR MED RECORD.                    +               GBIBPGM 
01178 *        +----------------------------------------+               GBIBPGM 
01179                                                                   GBIBPGM 
01180      IF  WS-GCDATES-CM-ADDR > +0                                  GBIBPGM 
01181          SET ADDRESS OF IO-PARM-GCDATES-RECORD TO                 GBIBPGM 
01182                 WS-GCDATES-CM-POINTER                             GBIBPGM 
01183          PERFORM 2300-000-LOAD-DATES-TABLE                        GBIBPGM 
01184             VARYING DTE-INDEX FROM 1 BY 1                         GBIBPGM 
01185               UNTIL DTE-INDEX > WS-GCDATES-CM-ENTRY-COUNT.        GBIBPGM 
01186                                                                   GBIBPGM 
01187 *        +----------------------------------------+               GBIBPGM 
01188 *        +  SORT DATES TABLE:                     +               GBIBPGM 
01189 *        +    DESCENDING ON EFFECTIVE DATE        +               GBIBPGM 
01190 *        +    ASCENDING ON LOB, PROV CNTL,FAM REL +               GBIBPGM 
01191 *        +----------------------------------------+               GBIBPGM 
01192                                                                   GBIBPGM 
01193 ***  IF  DT-ENTRY-COUNT < +2                                      GBIBPGM 
01194      IF  WS-DT-SUBX     < +2                                      GBIBPGM 
01195          GO TO 2200-100-NO-SORT-REQUIRED.                         GBIBPGM 
01196                                                                   GBIBPGM 
01197      PERFORM 2400-000-SORT-DATES-TABLE                            GBIBPGM 
01198         VARYING WS-DT-SUB FROM 1 BY 1                             GBIBPGM 
01199 ***       UNTIL WS-DT-SUB > DT-ENTRY-COUNT                        GBIBPGM 
01200           UNTIL WS-DT-SUB > WS-DT-SUBX                            GBIBPGM 
01201              OR WS-SORT-EXCHANGE-NOT-MADE.                        GBIBPGM 
01202                                                                   GBIBPGM 
01203  2200-100-NO-SORT-REQUIRED.                                       GBIBPGM 
01204                                                                   GBIBPGM 
01205 *        +----------------------------------------+               GBIBPGM 
01206 *        +  CHANGE EFFECTIVE DATES IN DATES TABLE +               GBIBPGM 
01207 *        +   BACK FROM COMPIMENTED(99999 - JULIAN)+               GBIBPGM 
01208 *        +    TO NORMAL JULIAN.                   +               GBIBPGM 
01209 *        +----------------------------------------+               GBIBPGM 
01210                                                                   GBIBPGM 
01211      PERFORM 2500-000-UNCOMPLIMENT-EFF-DATE                       GBIBPGM 
01212         VARYING WS-DT-SUB FROM 1 BY 1                             GBIBPGM 
01213 ***       UNTIL WS-DT-SUB > DT-ENTRY-COUNT.                       GBIBPGM 
01214           UNTIL WS-DT-SUB > WS-DT-SUBX.                           GBIBPGM 
01215                                                                   GBIBPGM 
01216 *        +----------------------------------------+               GBIBPGM 
01217 *        +        PAGE BACKWARD FUNCTION          +               GBIBPGM 
01218 *        +----------------------------------------+               GBIBPGM 
01219      IF EIBAID  =   DFHPF7     OR   DFHPF19                       GBIBPGM 
01220         MOVE  +1  TO   WS-SCREEN-SUB                              GBIBPGM 
01221         IF IBLNI (WS-SCREEN-SUB)  >  ZEROES                       GBIBPGM 
01222           IF IBGRPXI    =    IBGROUPI                             GBIBPGM 
01223             IF IBSECXI   =    IBSECI                              GBIBPGM 
01224                PERFORM 2225-000-PAGE-BACKWARD                     GBIBPGM 
01225                MOVE  WS-PAGE-NAME  TO  IBPNAMEO                   GBIBPGM 
01226                MOVE  -1            TO  IBLNNOL                    GBIBPGM 
01227                GO TO 2200-700-SEND-SCREEN                         GBIBPGM 
01228             ELSE                                                  GBIBPGM 
01229                NEXT SENTENCE                                      GBIBPGM 
01230           ELSE                                                    GBIBPGM 
01231              NEXT SENTENCE                                        GBIBPGM 
01232         ELSE                                                      GBIBPGM 
01233            MOVE  SPACES  TO  IBPNAMEO                             GBIBPGM 
01234            MOVE  -1            TO  IBLNNOL                        GBIBPGM 
01235            MOVE '    PRESS   ENTER   TO DISPLAY CONTRACTS        'GBIBPGM 
01236              TO IBMSGO(1)                                         GBIBPGM 
01237            GO TO 2200-700-SEND-SCREEN.                            GBIBPGM 
01238                                                                   GBIBPGM 
01239                                                                   GBIBPGM 
01240 *        +----------------------------------------+               GBIBPGM 
01241 *        +        PAGE FORWARD FUNCTION           +               GBIBPGM 
01242 *        +----------------------------------------+               GBIBPGM 
01243      IF EIBAID  =  DFHPF8     OR   DFHPF20                        GBIBPGM 
01244         MOVE  +1  TO   WS-SCREEN-SUB                              GBIBPGM 
01245         IF IBLNI (WS-SCREEN-SUB)  >  ZEROES                       GBIBPGM 
01246           IF IBGRPXI    =    IBGROUPI                             GBIBPGM 
01247             IF IBSECXI   =    IBSECI                              GBIBPGM 
01248                PERFORM 2250-000-PAGE-FORWARD                      GBIBPGM 
01249                MOVE  WS-PAGE-NAME  TO  IBPNAMEO                   GBIBPGM 
01250                MOVE  -1            TO  IBLNNOL                    GBIBPGM 
01251                GO TO 2200-700-SEND-SCREEN                         GBIBPGM 
01252             ELSE                                                  GBIBPGM 
01253                NEXT SENTENCE                                      GBIBPGM 
01254           ELSE                                                    GBIBPGM 
01255              NEXT SENTENCE                                        GBIBPGM 
01256         ELSE                                                      GBIBPGM 
01257            MOVE  SPACES  TO  IBPNAMEO                             GBIBPGM 
01258            MOVE  -1      TO  IBLNNOL                              GBIBPGM 
01259            MOVE '    PRESS   ENTER   TO DISPLAY CONTRACTS        'GBIBPGM 
01260              TO IBMSGO(1)                                         GBIBPGM 
01261            GO TO 2200-700-SEND-SCREEN.                            GBIBPGM 
01262                                                                   GBIBPGM 
01263                                                                   GBIBPGM 
01264 *        +----------------------------------------+               GBIBPGM 
01265 *        +  CLEAR TABLE ENTRIES ON SCREEN         +               GBIBPGM 
01266 *        +   (LN, LOB, PRV CNTL, FR AND EFDT)     +               GBIBPGM 
01267 *        +----------------------------------------+               GBIBPGM 
01268                                                                   GBIBPGM 
01269      PERFORM 2600-000-CLEAR-SCREEN-TABLE                          GBIBPGM 
01270         VARYING WS-SCREEN-SUB FROM 1 BY 1                         GBIBPGM 
01271           UNTIL WS-SCREEN-SUB > WS-SCREEN-MAX-ENTRIES.            GBIBPGM 
01272                                                                   GBIBPGM 
01273 *        +----------------------------------------+               GBIBPGM 
01274 *        +  BUILD SCREEN CONTRACT SELECTION LIST  +               GBIBPGM 
01275 *        +   FROM DATES TABLE.                    +               GBIBPGM 
01276 *        +----------------------------------------+               GBIBPGM 
01277                                                                   GBIBPGM 
01278      MOVE ZEROS TO WS-SCREEN-LN                                   GBIBPGM 
01279                    WS-SCREEN-SUB                                  GBIBPGM 
01280                    WS-DT-SUB.                                     GBIBPGM 
01281                                                                   GBIBPGM 
01282 * SAVE THE GROUP AND SECTION NUMBERS THAT WERE KEYED IN ON        GBIBPGM 
01283 * THE SCREEN.  SAVE THEM IN THE DARK FIELDS ON THE SCREEN,        GBIBPGM 
01284 * WHICH ARE REFERENCED WHEN PAGING FORWARD OR BACKWARDS.          GBIBPGM 
01285      MOVE IBGRPXI  TO  IBGROUPO.                                  GBIBPGM 
01286      MOVE IBSECXI  TO  IBSECO.                                    GBIBPGM 
01287                                                                   GBIBPGM 
01288                                                                   GBIBPGM 
01289      PERFORM 2700-000-BUILD-SELECT-LIST                           GBIBPGM 
01290           UNTIL WS-SCREEN-SUB = WS-SCREEN-MAX-ENTRIES             GBIBPGM 
01291 ***          OR WS-SCREEN-SUB > DT-ENTRY-COUNT                    GBIBPGM 
01292              OR WS-SCREEN-SUB > WS-DT-SUBX                        GBIBPGM 
01293 ***          OR WS-DT-SUB     = DT-ENTRY-COUNT.                   GBIBPGM 
01294              OR WS-DT-SUB     = WS-DT-SUBX.                       GBIBPGM 
01295                                                                   GBIBPGM 
01296      IF  WS-SCREEN-SUB = 0                                        GBIBPGM 
01297          MOVE  SPACES  TO  IBPNAMEO                               GBIBPGM 
01298          MOVE  ZEROES  TO  IBLNCNTO                               GBIBPGM 
01299          MOVE  -1      TO  IBGRPXL                                GBIBPGM 
01300          MOVE '*** NO CONTRACTS SELECTED BASED ON THE PARTIAL KEY GBIBPGM 
01301 -             'ENTERED ***'                                       GBIBPGM 
01302            TO IBMSGO(1)                                           GBIBPGM 
01303      ELSE                                                         GBIBPGM 
01304          MOVE  WS-PAGE-NAME      TO  IBPNAMEO                     GBIBPGM 
01305          ADD   1                 TO  WS-PAGE-COUNT                GBIBPGM 
01306          MOVE  WS-PAGE-COUNT-X   TO  IBPAGEO                      GBIBPGM 
01307          MOVE -1                 TO  IBLNNOL                      GBIBPGM 
01308          MOVE '    CHOOSE CONTRACT LINE NUMBER ' TO IBMSGO(1)     GBIBPGM 
01309          MOVE '            ****   THIS IS THE FIRST PAGE   ****'  GBIBPGM 
01310            TO  IBMSGO(2).                                         GBIBPGM 
01311                                                                   GBIBPGM 
01312 ***  IF  DT-ENTRY-COUNT = +1                                      GBIBPGM 
01313      IF  WS-DT-SUBX     = +1                                      GBIBPGM 
01314          GO TO 2200-900-EXIT                                      GBIBPGM 
01315      END-IF.                                                      GBIBPGM 
01316                                                                   GBIBPGM 
01317                                                                   GBIBPGM 
01318  2200-700-SEND-SCREEN.                                            GBIBPGM 
01319      PERFORM 7600-000-SEND-ERASE.                                 GBIBPGM 
01320      GO TO 2200-900-EXIT.                                         GBIBPGM 
01321                                                                   GBIBPGM 
01322  2200-900-EXIT.                                                   GBIBPGM 
01323      EXIT.                                                        GBIBPGM 
01324                                                                   GBIBPGM 
01325 /*****************************************************************GBIBPGM 
01326 **                        PAGE BACKWARD                         **GBIBPGM 
01327 ******************************************************************GBIBPGM 
01328  2225-000-PAGE-BACKWARD    SECTION.                               GBIBPGM 
01329  2225-010.                                                        GBIBPGM 
01330                                                                   GBIBPGM 
01331      MOVE '2225'  TO  WS-PARA-ID.                                 GBIBPGM 
01332                                                                   GBIBPGM 
01333 ***  IF DT-ENTRY-COUNT   <   +19                                  GBIBPGM 
01334      IF WS-DT-SUBX       <   +19                                  GBIBPGM 
01335        IF IBLNI (WS-SCREEN-SUB)   =   '01'                        GBIBPGM 
01336           PERFORM 2280-000-BRIGHT-FSET                            GBIBPGM 
01337             VARYING  WS-SCREEN-SUB   FROM  +1  BY  +1             GBIBPGM 
01338               UNTIL  WS-SCREEN-SUB   >   WS-SCREEN-MAX-ENTRIES    GBIBPGM 
01339                  OR  IBLNI (WS-SCREEN-SUB)   =                    GBIBPGM 
01340                                         (LOW-VALUES   OR   SPACES)GBIBPGM 
01341            MOVE SPACES TO IBMSGO(1)                               GBIBPGM 
01342            MOVE ' ***    CHOOSE CONTRACT LINE NUMBER   *** '      GBIBPGM 
01343                            TO  IBMSGO(1)                          GBIBPGM 
01344            MOVE '            ****   THIS IS THE FIRST PAGE   ****'GBIBPGM 
01345              TO  IBMSGO(2)                                        GBIBPGM 
01346            GO TO 2225-900-EXIT.                                   GBIBPGM 
01347                                                                   GBIBPGM 
01348      PERFORM 2230-000-SAVE-FIRST-ENTRY.                           GBIBPGM 
01349                                                                   GBIBPGM 
01350      IF WS-ERROR-SWITCH  =   '1'                                  GBIBPGM 
01351         GO TO 2225-900-EXIT.                                      GBIBPGM 
01352                                                                   GBIBPGM 
01353      PERFORM 2240-000-BUILD-PREVIOUS-PAGE.                        GBIBPGM 
01354                                                                   GBIBPGM 
01355  2225-900-EXIT.                                                   GBIBPGM 
01356      EXIT.                                                        GBIBPGM 
01357 /*****************************************************************GBIBPGM 
01358 **         SAVE THE FIRST ENTRY FROM THE SCREEN TABLE            *GBIBPGM 
01359 ******************************************************************GBIBPGM 
01360  2230-000-SAVE-FIRST-ENTRY   SECTION.                             GBIBPGM 
01361  2230-010.                                                        GBIBPGM 
01362                                                                   GBIBPGM 
01363      MOVE '2230'  TO  WS-PARA-ID.                                 GBIBPGM 
01364                                                                   GBIBPGM 
01365      MOVE IBLNI  (WS-SCREEN-SUB)   TO  WS-SAVE-FIRST-LN-NO.       GBIBPGM 
01366      MOVE IBLOBI (WS-SCREEN-SUB)   TO  WS-SAVE-FIRST-L-O-B.       GBIBPGM 
01367      MOVE IBPRVI (WS-SCREEN-SUB)   TO  WS-SAVE-FIRST-PROV-CNTL.   GBIBPGM 
01368      MOVE IBFRI  (WS-SCREEN-SUB)   TO  WS-SAVE-FIRST-FAM-REL.     GBIBPGM 
01369                                                                   GBIBPGM 
01370 ** CONVERT EFFECTIVE-DATE FROM GREGORIAN TO JULIAN                GBIBPGM 
01371 **                                                                GBIBPGM 
01372      MOVE IBEDTI (WS-SCREEN-SUB)   TO  HGADATE-DATE1.             GBIBPGM 
01373      PERFORM 9200-000-GREGORIAN-TO-JULIAN.                        GBIBPGM 
01374                                                                   GBIBPGM 
01375      IF  HGADATE-RETURN   =   ZEROS                               GBIBPGM 
01376          IF HGADATE-JULIAN2 < +70000                              GBIBPGM 
01377              MOVE HEX-20           TO WS-SAVE-FIRST-EFF-DT-CC     GBIBPGM 
01378              MOVE HGADATE-JULIAN2  TO WS-SAVE-FIRST-EFF-DT        GBIBPGM 
01379          ELSE                                                     GBIBPGM 
01380              MOVE HEX-19           TO WS-SAVE-FIRST-EFF-DT-CC     GBIBPGM 
01381              MOVE HGADATE-JULIAN2  TO WS-SAVE-FIRST-EFF-DT        GBIBPGM 
01382          END-IF                                                   GBIBPGM 
01383      ELSE                                                         GBIBPGM 
01384          MOVE -1               TO  IBEDTL (WS-SCREEN-SUB)         GBIBPGM 
01385          MOVE DFHBMUBF         TO  IBEDTA (WS-SCREEN-SUB)         GBIBPGM 
01386          MOVE '1'              TO  WS-ERROR-SWITCH                GBIBPGM 
01387          MOVE ' INVALID EFFECTIVE DATE   CONTACT SYSTEMS'         GBIBPGM 
01388            TO  IBMSGO(2).                                         GBIBPGM 
01389                                                                   GBIBPGM 
01390                                                                   GBIBPGM 
01391 ** CONVERT TERMINATION-DATE FROM GREGORIAN TO JULIAN              GBIBPGM 
01392 **                                                                GBIBPGM 
01393      MOVE IBTDTI (WS-SCREEN-SUB)  TO  MLDATE-DATE1.               GBIBPGM 
01394      PERFORM 9220-000-GREG-JULIAN-CEN.                            GBIBPGM 
01395                                                                   GBIBPGM 
01396      IF  MLDATE-RETURN   =   ZEROS                                GBIBPGM 
01397         MOVE MLDATE-JUL2         TO  WS-HOLD-JULIAN-DISPLAY       GBIBPGM 
01398         MOVE WS-HOLD-JULIAN-DISPLAY                               GBIBPGM 
01399                                  TO  WS-SAVE-FIRST-TERM-DATE-CEN  GBIBPGM 
01400      ELSE                                                         GBIBPGM 
01401        IF WS-ERROR-SWITCH  =   '1'                                GBIBPGM 
01402           NEXT SENTENCE                                           GBIBPGM 
01403        ELSE                                                       GBIBPGM 
01404            MOVE -1               TO  IBTDTL (WS-SCREEN-SUB)       GBIBPGM 
01405            MOVE DFHBMUBF         TO  IBTDTA (WS-SCREEN-SUB)       GBIBPGM 
01406            MOVE '1'              TO  WS-ERROR-SWITCH              GBIBPGM 
01407            MOVE ' INVALID TERMINATION DATE   CONTACT SYSTEMS'     GBIBPGM 
01408              TO  IBMSGO(2).                                       GBIBPGM 
01409                                                                   GBIBPGM 
01410  2230-900-EXIT.                                                   GBIBPGM 
01411      EXIT.                                                        GBIBPGM 
01412 /*****************************************************************GBIBPGM 
01413 **        BUILD THE PREVIOUS PAGE IF CONDITIONS ARE MET         **GBIBPGM 
01414 ******************************************************************GBIBPGM 
01415  2240-000-BUILD-PREVIOUS-PAGE  SECTION.                           GBIBPGM 
01416  2240-010.                                                        GBIBPGM 
01417                                                                   GBIBPGM 
01418      MOVE '2240'  TO  WS-PARA-ID.                                 GBIBPGM 
01419                                                                   GBIBPGM 
01420      MOVE +1  TO  WS-DT-SUB.                                      GBIBPGM 
01421                                                                   GBIBPGM 
01422      IF DT-ENTRY (WS-DT-SUB)   =   WS-SAVE-FIRST-ENTRY            GBIBPGM 
01423         PERFORM 2280-000-BRIGHT-FSET                              GBIBPGM 
01424           VARYING  WS-SCREEN-SUB   FROM  +1  BY  +1               GBIBPGM 
01425             UNTIL  WS-SCREEN-SUB   >   WS-SCREEN-MAX-ENTRIES      GBIBPGM 
01426                OR  IBLNI (WS-SCREEN-SUB)   =                      GBIBPGM 
01427                                       (LOW-VALUES   OR   SPACES)  GBIBPGM 
01428          MOVE '    CHOOSE CONTRACT LINE NUMBER ' TO IBMSGO(1)     GBIBPGM 
01429         MOVE '            ****   THIS IS THE FIRST PAGE   ****'   GBIBPGM 
01430           TO  IBMSGO(2)                                           GBIBPGM 
01431         GO TO 2240-900-EXIT.                                      GBIBPGM 
01432                                                                   GBIBPGM 
01433      PERFORM 2241-000-FIND-ENTRY-IN-TABLE                         GBIBPGM 
01434        VARYING  WS-DT-SUB  FROM  +1  BY  +1                       GBIBPGM 
01435          UNTIL  WS-ENTRY-IS-FOUND                                 GBIBPGM 
01436 ***         OR  WS-DT-SUB  >  DT-ENTRY-COUNT.                     GBIBPGM 
01437             OR  WS-DT-SUB  >  WS-DT-SUBX.                         GBIBPGM 
01438                                                                   GBIBPGM 
01439                                                                   GBIBPGM 
01440      IF WS-ENTRY-IS-NOT-FOUND                                     GBIBPGM 
01441          MOVE ' *** PAGE BACKWARD PROBLEM WITH DATES TABLE     PLEGBIBPGM 
01442 -             'ASE CALL SYSTEMS ***'                              GBIBPGM 
01443            TO  IBMSGO(2)                                          GBIBPGM 
01444          MOVE 'GIPA' TO WS-ABEND-CODE                             GBIBPGM 
01445          PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                   GBIBPGM 
01446                                                                   GBIBPGM 
01447      MOVE  19       TO    WS-SCREEN-LN                            GBIBPGM 
01448      MOVE +19       TO    WS-SCREEN-SUB.                          GBIBPGM 
01449      MOVE ZEROES    TO    WS-DARK-FIELD-COUNT-X.                  GBIBPGM 
01450      SUBTRACT  +1  FROM   WS-DT-SUB.                              GBIBPGM 
01451                                                                   GBIBPGM 
01452      PERFORM 2245-000-BUILD-PREV-PAGE                             GBIBPGM 
01453        UNTIL  WS-SCREEN-SUB     =   +1                            GBIBPGM 
01454           OR  WS-DT-SUB         <   +1.                           GBIBPGM 
01455                                                                   GBIBPGM 
01456                                                                   GBIBPGM 
01457      MOVE  +1  TO  WS-DT-SUB.                                     GBIBPGM 
01458      IF DT-ENTRY (WS-DT-SUB)    =    IBLNI-ENTRY (WS-SCREEN-SUB)  GBIBPGM 
01459         PERFORM 2280-000-BRIGHT-FSET                              GBIBPGM 
01460           VARYING  WS-SCREEN-SUB   FROM  +1  BY  +1               GBIBPGM 
01461             UNTIL  WS-SCREEN-SUB   >   WS-SCREEN-MAX-ENTRIES      GBIBPGM 
01462                OR  IBLNI (WS-SCREEN-SUB)   =                      GBIBPGM 
01463                                       (LOW-VALUES   OR   SPACES)  GBIBPGM 
01464          MOVE '    CHOOSE CONTRACT LINE NUMBER ' TO IBMSGO(1)     GBIBPGM 
01465         MOVE '            ****   THIS IS THE FIRST PAGE   ****'   GBIBPGM 
01466           TO  IBMSGO(2)                                           GBIBPGM 
01467         GO TO 2240-900-EXIT.                                      GBIBPGM 
01468                                                                   GBIBPGM 
01469      MOVE  IBPAGEI  TO  WS-PAGE-COUNT-X.                          GBIBPGM 
01470      SUBTRACT  1  FROM  WS-PAGE-COUNT.                            GBIBPGM 
01471      MOVE  WS-PAGE-COUNT-X   TO  IBPAGEO.                         GBIBPGM 
01472      MOVE '    CHOOSE CONTRACT LINE NUMBER ' TO IBMSGO(2).        GBIBPGM 
01473                                                                   GBIBPGM 
01474  2240-900-EXIT.                                                   GBIBPGM 
01475      EXIT.                                                        GBIBPGM 
01476 /*****************************************************************GBIBPGM 
01477 **      SEARCH FOR THE FIRST SCREEN ENTRY IN THE DATES TABLE     *GBIBPGM 
01478 **      THAT IS SET UP IN THE LINKAGE SECTION.                   *GBIBPGM 
01479 ******************************************************************GBIBPGM 
01480  2241-000-FIND-ENTRY-IN-TABLE   SECTION.                          GBIBPGM 
01481  2241-010.                                                        GBIBPGM 
01482                                                                   GBIBPGM 
01483      MOVE '2241'  TO  WS-PARA-ID.                                 GBIBPGM 
01484                                                                   GBIBPGM 
01485      IF DT-ENTRY (WS-DT-SUB)   =   WS-SAVE-FIRST-ENTRY            GBIBPGM 
01486         MOVE '1'  TO  WS-ENTRY-IN-TABLE-IS-FOUND.                 GBIBPGM 
01487                                                                   GBIBPGM 
01488  2241-900-EXIT.                                                   GBIBPGM 
01489      EXIT.                                                        GBIBPGM 
01490 /*****************************************************************GBIBPGM 
01491 **                  BUILD THE PREVIOUS PAGE                      *GBIBPGM 
01492 ******************************************************************GBIBPGM 
01493  2245-000-BUILD-PREV-PAGE   SECTION.                              GBIBPGM 
01494  2245-010.                                                        GBIBPGM 
01495                                                                   GBIBPGM 
01496      MOVE '2245'    TO  WS-PARA-ID.                               GBIBPGM 
01497                                                                   GBIBPGM 
01498      SUBTRACT  +1  FROM   WS-DT-SUB                               GBIBPGM 
01499                           WS-SCREEN-SUB.                          GBIBPGM 
01500      SUBTRACT   1  FROM   WS-SCREEN-LN.                           GBIBPGM 
01501      ADD        1   TO    WS-DARK-FIELD-COUNT.                    GBIBPGM 
01502                                                                   GBIBPGM 
01503      PERFORM 2246-000-MOVE-DATA-TO-SCREEN.                        GBIBPGM 
01504                                                                   GBIBPGM 
01505  2245-900-EXIT.                                                   GBIBPGM 
01506      EXIT.                                                        GBIBPGM 
01507 /*****************************************************************GBIBPGM 
01508 **    MOVE ENTRIES FROM THE DATES TABLE TO THE SCREEN TABLE      *GBIBPGM 
01509 ******************************************************************GBIBPGM 
01510  2246-000-MOVE-DATA-TO-SCREEN  SECTION.                           GBIBPGM 
01511  2246-010.                                                        GBIBPGM 
01512                                                                   GBIBPGM 
01513      MOVE '2246'    TO  WS-PARA-ID.                               GBIBPGM 
01514                                                                   GBIBPGM 
01515      MOVE DFHBMABF                TO IBLNA(WS-SCREEN-SUB).        GBIBPGM 
01516      MOVE WS-SCREEN-LN            TO IBLNO(WS-SCREEN-SUB)         GBIBPGM 
01517      MOVE WS-DARK-FIELD-COUNT     TO IBLNCNTO.                    GBIBPGM 
01518                                                                   GBIBPGM 
01519      MOVE DFHBMASF                TO IBLOBA(WS-SCREEN-SUB).       GBIBPGM 
01520      MOVE DT-L-O-B(WS-DT-SUB)     TO IBLOBO(WS-SCREEN-SUB).       GBIBPGM 
01521                                                                   GBIBPGM 
01522      MOVE DFHBMASF                TO IBPRVA(WS-SCREEN-SUB).       GBIBPGM 
01523      MOVE DT-PROV-CNTL(WS-DT-SUB) TO IBPRVO(WS-SCREEN-SUB).       GBIBPGM 
01524                                                                   GBIBPGM 
01525      MOVE DFHBMASF                TO IBFRA(WS-SCREEN-SUB).        GBIBPGM 
01526      MOVE DT-FAM-REL(WS-DT-SUB)   TO IBFRO(WS-SCREEN-SUB).        GBIBPGM 
01527                                                                   GBIBPGM 
01528      MOVE DT-EFF-DT(WS-DT-SUB) TO HGADATE-JULIAN1.                GBIBPGM 
01529      PERFORM 9210-000-JULIAN-TO-GREGORIAN.                        GBIBPGM 
01530                                                                   GBIBPGM 
01531      IF  HGADATE-RETURN = ZEROS                                   GBIBPGM 
01532          MOVE DFHBMASF        TO IBEDTA(WS-SCREEN-SUB)            GBIBPGM 
01533          MOVE HGADATE-DATE2   TO IBEDTO(WS-SCREEN-SUB)            GBIBPGM 
01534      ELSE                                                         GBIBPGM 
01535          MOVE DFHBMABF        TO IBEDTA(WS-SCREEN-SUB)            GBIBPGM 
01536          MOVE HGADATE-JULIAN1 TO IBEDTO(WS-SCREEN-SUB).           GBIBPGM 
01537                                                                   GBIBPGM 
01538      MOVE DT-TERM-DT-CEN(WS-DT-SUB) TO MLDATE-DATE1.              GBIBPGM 
01539      PERFORM 9215-000-JULIAN-GREG-CEN.                            GBIBPGM 
01540                                                                   GBIBPGM 
01541      IF  MLDATE-RETURN = ZEROS                                    GBIBPGM 
01542          MOVE DFHBMASF        TO IBTDTA(WS-SCREEN-SUB)            GBIBPGM 
01543          MOVE MLDATE-DATE2    TO IBTDTO(WS-SCREEN-SUB)            GBIBPGM 
01544      ELSE                                                         GBIBPGM 
01545          MOVE DFHBMABF        TO IBTDTA(WS-SCREEN-SUB)            GBIBPGM 
01546          MOVE MLDATE-DATE1    TO IBTDTO(WS-SCREEN-SUB).           GBIBPGM 
01547                                                                   GBIBPGM 
01548  2246-900-EXIT.                                                   GBIBPGM 
01549      EXIT.                                                        GBIBPGM 
01550 /*****************************************************************GBIBPGM 
01551 **                          PAGE FORWARD                         *GBIBPGM 
01552 ******************************************************************GBIBPGM 
01553  2250-000-PAGE-FORWARD     SECTION.                               GBIBPGM 
01554  2250-010.                                                        GBIBPGM 
01555                                                                   GBIBPGM 
01556      MOVE '2250'  TO  WS-PARA-ID.                                 GBIBPGM 
01557                                                                   GBIBPGM 
01558 ***  IF DT-ENTRY-COUNT   <   +19                                  GBIBPGM 
01559      IF WS-DT-SUBX       <   +19                                  GBIBPGM 
01560        IF IBLNI (WS-SCREEN-SUB)   =   '01'                        GBIBPGM 
01561           PERFORM 2280-000-BRIGHT-FSET                            GBIBPGM 
01562             VARYING  WS-SCREEN-SUB   FROM  +1  BY  +1             GBIBPGM 
01563               UNTIL  WS-SCREEN-SUB   >   WS-SCREEN-MAX-ENTRIES    GBIBPGM 
01564                  OR  IBLNI (WS-SCREEN-SUB)   =                    GBIBPGM 
01565                                         (LOW-VALUES   OR   SPACES)GBIBPGM 
01566          MOVE '    CHOOSE CONTRACT LINE NUMBER ' TO IBMSGO(1)     GBIBPGM 
01567           MOVE '            ****   THIS IS THE ONLY PAGE   ****'  GBIBPGM 
01568             TO  IBMSGO(2)                                         GBIBPGM 
01569           GO TO 2250-900-EXIT.                                    GBIBPGM 
01570                                                                   GBIBPGM 
01571                                                                   GBIBPGM 
01572      MOVE +18  TO  WS-SCREEN-SUB.                                 GBIBPGM 
01573      IF IBLNI (WS-SCREEN-SUB)   =   (LOW-VALUES   OR   SPACES)    GBIBPGM 
01574         PERFORM 2280-000-BRIGHT-FSET                              GBIBPGM 
01575           VARYING  WS-SCREEN-SUB   FROM  +1  BY  +1               GBIBPGM 
01576             UNTIL  WS-SCREEN-SUB   >   WS-SCREEN-MAX-ENTRIES      GBIBPGM 
01577                OR  IBLNI (WS-SCREEN-SUB)   =                      GBIBPGM 
01578                                         (LOW-VALUES   OR   SPACES)GBIBPGM 
01579          MOVE '    CHOOSE CONTRACT LINE NUMBER ' TO IBMSGO(1)     GBIBPGM 
01580         MOVE '            ****   THIS IS THE LAST PAGE   ****'    GBIBPGM 
01581           TO  IBMSGO(2)                                           GBIBPGM 
01582         GO TO 2250-900-EXIT.                                      GBIBPGM 
01583                                                                   GBIBPGM 
01584                                                                   GBIBPGM 
01585      MOVE +0   TO   WS-SCREEN-SUB.                                GBIBPGM 
01586                                                                   GBIBPGM 
01587      PERFORM 2253-000-SEARCH-FOR-LAST-ENTRY                       GBIBPGM 
01588        UNTIL  WS-LAST-ENTRY-IS-FOUND.                             GBIBPGM 
01589                                                                   GBIBPGM 
01590      IF WS-ERROR-SWITCH  =   '1'                                  GBIBPGM 
01591         GO TO 2250-900-EXIT.                                      GBIBPGM 
01592                                                                   GBIBPGM 
01593      PERFORM 2260-000-BUILD-NEXT-PAGE.                            GBIBPGM 
01594                                                                   GBIBPGM 
01595  2250-900-EXIT.                                                   GBIBPGM 
01596      EXIT.                                                        GBIBPGM 
01597 /*****************************************************************GBIBPGM 
01598 **            FIND THE LAST ENTRY ON THE SCREEN TABLE            *GBIBPGM 
01599 ******************************************************************GBIBPGM 
01600  2253-000-SEARCH-FOR-LAST-ENTRY  SECTION.                         GBIBPGM 
01601  2253-010.                                                        GBIBPGM 
01602                                                                   GBIBPGM 
01603      MOVE '2253'  TO  WS-PARA-ID.                                 GBIBPGM 
01604                                                                   GBIBPGM 
01605      ADD +1  TO   WS-SCREEN-SUB.                                  GBIBPGM 
01606                                                                   GBIBPGM 
01607      IF IBLNI (WS-SCREEN-SUB)   =                                 GBIBPGM 
01608                    (ZEROES  OR  LOW-VALUES  OR  SPACES)           GBIBPGM 
01609         MOVE '1'  TO  WS-FOUND-LAST-ENTRY-ON-SCREEN               GBIBPGM 
01610         SUBTRACT  +1   FROM   WS-SCREEN-SUB                       GBIBPGM 
01611         PERFORM 2255-000-SAVE-LAST-ENTRY                          GBIBPGM 
01612         GO TO 2253-900-EXIT.                                      GBIBPGM 
01613                                                                   GBIBPGM 
01614                                                                   GBIBPGM 
01615      IF WS-SCREEN-SUB    =    WS-SCREEN-MAX-ENTRIES               GBIBPGM 
01616         MOVE '1'  TO  WS-FOUND-LAST-ENTRY-ON-SCREEN               GBIBPGM 
01617         PERFORM 2255-000-SAVE-LAST-ENTRY.                         GBIBPGM 
01618                                                                   GBIBPGM 
01619  2253-900-EXIT.                                                   GBIBPGM 
01620      EXIT.                                                        GBIBPGM 
01621 /*****************************************************************GBIBPGM 
01622 **         SAVE THE LAST ENTRY FROM THE SCREEN TABLE             *GBIBPGM 
01623 ******************************************************************GBIBPGM 
01624  2255-000-SAVE-LAST-ENTRY   SECTION.                              GBIBPGM 
01625  2255-010.                                                        GBIBPGM 
01626                                                                   GBIBPGM 
01627      MOVE '2255'  TO  WS-PARA-ID.                                 GBIBPGM 
01628                                                                   GBIBPGM 
01629      MOVE IBLNI  (WS-SCREEN-SUB)   TO  WS-SAVE-LAST-LN-NO.        GBIBPGM 
01630      MOVE IBLOBI (WS-SCREEN-SUB)   TO  WS-SAVE-LAST-L-O-B.        GBIBPGM 
01631      MOVE IBPRVI (WS-SCREEN-SUB)   TO  WS-SAVE-LAST-PROV-CNTL.    GBIBPGM 
01632      MOVE IBFRI  (WS-SCREEN-SUB)   TO  WS-SAVE-LAST-FAM-REL.      GBIBPGM 
01633                                                                   GBIBPGM 
01634 ** CONVERT EFFECTIVE-DATE FROM GREGORIAN TO JULIAN                GBIBPGM 
01635 **                                                                GBIBPGM 
01636      MOVE IBEDTI (WS-SCREEN-SUB)   TO  HGADATE-DATE1.             GBIBPGM 
01637      PERFORM 9200-000-GREGORIAN-TO-JULIAN.                        GBIBPGM 
01638                                                                   GBIBPGM 
01639      IF  HGADATE-RETURN   =   ZEROS                               GBIBPGM 
01640         IF HGADATE-JULIAN2 < +70000                               GBIBPGM 
01641            MOVE HEX-20           TO  WS-SAVE-LAST-EFF-DT-CC       GBIBPGM 
01642            MOVE HGADATE-JULIAN2  TO  WS-SAVE-LAST-EFF-DT          GBIBPGM 
01643         ELSE                                                      GBIBPGM 
01644            MOVE HEX-19           TO  WS-SAVE-LAST-EFF-DT-CC       GBIBPGM 
01645            MOVE HGADATE-JULIAN2  TO  WS-SAVE-LAST-EFF-DT          GBIBPGM 
01646         END-IF                                                    GBIBPGM 
01647      ELSE                                                         GBIBPGM 
01648          MOVE -1               TO  IBEDTL (WS-SCREEN-SUB)         GBIBPGM 
01649          MOVE DFHBMUBF         TO  IBEDTA (WS-SCREEN-SUB)         GBIBPGM 
01650          MOVE '1'              TO  WS-ERROR-SWITCH                GBIBPGM 
01651          MOVE ' INVALID EFFECTIVE DATE   CONTACT SYSTEMS'         GBIBPGM 
01652            TO IBMSGO(2).                                          GBIBPGM 
01653                                                                   GBIBPGM 
01654                                                                   GBIBPGM 
01655 ** CONVERT TERMINATION-DATE FROM GREGORIAN TO JULIAN              GBIBPGM 
01656 **                                                                GBIBPGM 
01657      MOVE IBTDTI (WS-SCREEN-SUB)  TO  MLDATE-DATE1.               GBIBPGM 
01658      PERFORM 9220-000-GREG-JULIAN-CEN.                            GBIBPGM 
01659                                                                   GBIBPGM 
01660      IF  MLDATE-RETURN   =   ZEROS                                GBIBPGM 
01661         MOVE MLDATE-JUL2         TO  WS-HOLD-JULIAN-DISPLAY       GBIBPGM 
01662         MOVE WS-HOLD-JULIAN-DISPLAY                               GBIBPGM 
01663                                  TO  WS-SAVE-LAST-TERM-DATE-CEN   GBIBPGM 
01664      ELSE                                                         GBIBPGM 
01665        IF WS-ERROR-SWITCH  =   '1'                                GBIBPGM 
01666           NEXT SENTENCE                                           GBIBPGM 
01667        ELSE                                                       GBIBPGM 
01668          MOVE -1               TO  IBTDTL (WS-SCREEN-SUB)         GBIBPGM 
01669          MOVE DFHBMUBF         TO  IBTDTA (WS-SCREEN-SUB)         GBIBPGM 
01670          MOVE '1'              TO  WS-ERROR-SWITCH                GBIBPGM 
01671          MOVE ' INVALID TERMINATION DATE   CONTACT SYSTEMS'       GBIBPGM 
01672            TO IBMSGO(2).                                          GBIBPGM 
01673                                                                   GBIBPGM 
01674  2255-900-EXIT.                                                   GBIBPGM 
01675      EXIT.                                                        GBIBPGM 
01676 /*****************************************************************GBIBPGM 
01677 **          BUILD THE NEXT PAGE IF CONDITIONS ARE MET           **GBIBPGM 
01678 ******************************************************************GBIBPGM 
01679  2260-000-BUILD-NEXT-PAGE    SECTION.                             GBIBPGM 
01680  2260-010.                                                        GBIBPGM 
01681                                                                   GBIBPGM 
01682      MOVE '2260'  TO  WS-PARA-ID.                                 GBIBPGM 
01683                                                                   GBIBPGM 
01684      PERFORM 2265-000-FIND-ENTRY-IN-TABLE                         GBIBPGM 
01685        VARYING  WS-DT-SUB  FROM  +1  BY  +1                       GBIBPGM 
01686          UNTIL  WS-ENTRY-IS-FOUND                                 GBIBPGM 
01687 ***         OR  WS-DT-SUB  >  DT-ENTRY-COUNT.                     GBIBPGM 
01688             OR  WS-DT-SUB  >  WS-DT-SUBX.                         GBIBPGM 
01689                                                                   GBIBPGM 
01690                                                                   GBIBPGM 
01691      IF WS-ENTRY-IS-NOT-FOUND                                     GBIBPGM 
01692          MOVE ' *** PAGE FORWARD PROBLEM WITH DATES TABLE     PLEAGBIBPGM 
01693 -             'SE CALL SYSTEMS ***'                               GBIBPGM 
01694            TO  IBMSGO(2)                                          GBIBPGM 
01695          MOVE 'GIPB' TO WS-ABEND-CODE                             GBIBPGM 
01696          PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                   GBIBPGM 
01697                                                                   GBIBPGM 
01698      SUBTRACT  +1   FROM   WS-DT-SUB.                             GBIBPGM 
01699                                                                   GBIBPGM 
01700      IF WS-ENTRY-IS-FOUND                                         GBIBPGM 
01701 ***    IF WS-DT-SUB   =   DT-ENTRY-COUNT                          GBIBPGM 
01702        IF WS-DT-SUB   =   WS-DT-SUBX                              GBIBPGM 
01703           PERFORM 2280-000-BRIGHT-FSET                            GBIBPGM 
01704             VARYING  WS-SCREEN-SUB   FROM  +1  BY  +1             GBIBPGM 
01705               UNTIL  WS-SCREEN-SUB   >   WS-SCREEN-MAX-ENTRIES    GBIBPGM 
01706                  OR  IBLNI (WS-SCREEN-SUB)   =                    GBIBPGM 
01707                                         (LOW-VALUES   OR   SPACES)GBIBPGM 
01708          MOVE '    CHOOSE CONTRACT LINE NUMBER ' TO IBMSGO(1)     GBIBPGM 
01709           MOVE '            ****   THIS IS THE LAST PAGE   ****'  GBIBPGM 
01710             TO  IBMSGO(2)                                         GBIBPGM 
01711           GO TO 2260-900-EXIT.                                    GBIBPGM 
01712                                                                   GBIBPGM 
01713                                                                   GBIBPGM 
01714      MOVE ZEROES     TO    WS-SCREEN-LN                           GBIBPGM 
01715                            WS-SCREEN-SUB.                         GBIBPGM 
01716      MOVE ZEROES     TO    WS-DARK-FIELD-COUNT-X.                 GBIBPGM 
01717                                                                   GBIBPGM 
01718      PERFORM 2270-000-BUILD-NEXT-SCREEN                           GBIBPGM 
01719        UNTIL  WS-SCREEN-SUB     =   WS-SCREEN-MAX-ENTRIES         GBIBPGM 
01720 ***       OR  WS-SCREEN-SUB     =   DT-ENTRY-COUNT                GBIBPGM 
01721 ***       OR  WS-DT-SUB         =   DT-ENTRY-COUNT.               GBIBPGM 
01722           OR  WS-SCREEN-SUB     =   WS-DT-SUBX                    GBIBPGM 
01723           OR  WS-DT-SUB         =   WS-DT-SUBX.                   GBIBPGM 
01724                                                                   GBIBPGM 
01725      IF  WS-SCREEN-SUB     NOT =   WS-SCREEN-MAX-ENTRIES          GBIBPGM 
01726          PERFORM 2275-000-CLEAR-THE-PAGE                          GBIBPGM 
01727            UNTIL  WS-SCREEN-SUB     =   WS-SCREEN-MAX-ENTRIES.    GBIBPGM 
01728                                                                   GBIBPGM 
01729                                                                   GBIBPGM 
01730      MOVE  IBPAGEI  TO  WS-PAGE-COUNT-X.                          GBIBPGM 
01731      ADD  1  TO  WS-PAGE-COUNT.                                   GBIBPGM 
01732      MOVE  WS-PAGE-COUNT-X   TO  IBPAGEO.                         GBIBPGM 
01733                                                                   GBIBPGM 
01734                                                                   GBIBPGM 
01735      MOVE  WS-SCREEN-MAX-ENTRIES  TO  WS-SCREEN-SUB.              GBIBPGM 
01736      IF IBLNI (WS-SCREEN-SUB)  =                                  GBIBPGM 
01737                       (ZEROES  OR  LOW-VALUES  OR  SPACES)        GBIBPGM 
01738          MOVE '    CHOOSE CONTRACT LINE NUMBER ' TO IBMSGO(1)     GBIBPGM 
01739         MOVE '            ****   THIS IS THE LAST PAGE   ****'    GBIBPGM 
01740           TO  IBMSGO(2)                                           GBIBPGM 
01741         GO TO 2260-900-EXIT                                       GBIBPGM 
01742      ELSE                                                         GBIBPGM 
01743          MOVE '    CHOOSE CONTRACT LINE NUMBER ' TO IBMSGO(2).    GBIBPGM 
01744                                                                   GBIBPGM 
01745                                                                   GBIBPGM 
01746 ***  IF WS-DT-SUB   =   DT-ENTRY-COUNT                            GBIBPGM 
01747      IF WS-DT-SUB   =   WS-DT-SUBX                                GBIBPGM 
01748          MOVE '    CHOOSE CONTRACT LINE NUMBER ' TO IBMSGO(1)     GBIBPGM 
01749         MOVE '            ****   THIS IS THE LAST PAGE   ****'    GBIBPGM 
01750           TO  IBMSGO(2).                                          GBIBPGM 
01751                                                                   GBIBPGM 
01752  2260-900-EXIT.                                                   GBIBPGM 
01753      EXIT.                                                        GBIBPGM 
01754 /*****************************************************************GBIBPGM 
01755 **      SEARCH FOR THE LAST SCREEN ENTRY IN THE DATES TABLE      *GBIBPGM 
01756 **      THAT IS SET UP IN THE LINKAGE SECTION.                   *GBIBPGM 
01757 ******************************************************************GBIBPGM 
01758  2265-000-FIND-ENTRY-IN-TABLE   SECTION.                          GBIBPGM 
01759  2265-010.                                                        GBIBPGM 
01760                                                                   GBIBPGM 
01761      MOVE '2265'  TO  WS-PARA-ID.                                 GBIBPGM 
01762                                                                   GBIBPGM 
01763      IF DT-ENTRY (WS-DT-SUB)   =   WS-SAVE-LAST-ENTRY             GBIBPGM 
01764         MOVE '1'  TO  WS-ENTRY-IN-TABLE-IS-FOUND.                 GBIBPGM 
01765                                                                   GBIBPGM 
01766  2265-900-EXIT.                                                   GBIBPGM 
01767      EXIT.                                                        GBIBPGM 
01768 /*****************************************************************GBIBPGM 
01769 **                    BUILD THE NEXT SCREEN                      *GBIBPGM 
01770 ******************************************************************GBIBPGM 
01771  2270-000-BUILD-NEXT-SCREEN  SECTION.                             GBIBPGM 
01772  2270-010.                                                        GBIBPGM 
01773                                                                   GBIBPGM 
01774      MOVE '2270'  TO  WS-PARA-ID.                                 GBIBPGM 
01775                                                                   GBIBPGM 
01776      ADD  +1  TO   WS-DT-SUB                                      GBIBPGM 
01777                    WS-SCREEN-SUB.                                 GBIBPGM 
01778                                                                   GBIBPGM 
01779      ADD   1  TO   WS-SCREEN-LN                                   GBIBPGM 
01780                    WS-DARK-FIELD-COUNT.                           GBIBPGM 
01781                                                                   GBIBPGM 
01782      PERFORM 2246-000-MOVE-DATA-TO-SCREEN.                        GBIBPGM 
01783                                                                   GBIBPGM 
01784  2270-900-EXIT.                                                   GBIBPGM 
01785      EXIT.                                                        GBIBPGM 
01786 /*****************************************************************GBIBPGM 
01787 **  CLEAR THE BALANCE OF THE SCREEN TABLE ENTRIES, IF A COMPLETE *GBIBPGM 
01788 **  SCREEN TABLE WAS NOT FILLED.                                 *GBIBPGM 
01789 ******************************************************************GBIBPGM 
01790  2275-000-CLEAR-THE-PAGE  SECTION.                                GBIBPGM 
01791  2275-010.                                                        GBIBPGM 
01792                                                                   GBIBPGM 
01793      MOVE '2275'      TO  WS-PARA-ID.                             GBIBPGM 
01794                                                                   GBIBPGM 
01795      ADD  +1          TO  WS-SCREEN-SUB.                          GBIBPGM 
01796      MOVE LOW-VALUES  TO  IBLNI-ENTRY (WS-SCREEN-SUB).            GBIBPGM 
01797      MOVE DFHBMASF    TO  IBLNA (WS-SCREEN-SUB)                   GBIBPGM 
01798                           IBLOBA(WS-SCREEN-SUB)                   GBIBPGM 
01799                           IBPRVA(WS-SCREEN-SUB)                   GBIBPGM 
01800                           IBFRA (WS-SCREEN-SUB)                   GBIBPGM 
01801                           IBEDTA(WS-SCREEN-SUB)                   GBIBPGM 
01802                           IBTDTA(WS-SCREEN-SUB).                  GBIBPGM 
01803                                                                   GBIBPGM 
01804  2275-900-EXIT.                                                   GBIBPGM 
01805           EXIT.                                                   GBIBPGM 
01806 /*****************************************************************GBIBPGM 
01807 **   RESET THE FSETS FOR THE SCREEN TABLE FIELDS, LINE NUMBER    *GBIBPGM 
01808 ******************************************************************GBIBPGM 
01809  2280-000-BRIGHT-FSET     SECTION.                                GBIBPGM 
01810  2280-010.                                                        GBIBPGM 
01811                                                                   GBIBPGM 
01812      MOVE '2280'      TO  WS-PARA-ID.                             GBIBPGM 
01813                                                                   GBIBPGM 
01814      MOVE DFHBMABF    TO  IBLNA (WS-SCREEN-SUB).                  GBIBPGM 
01815                                                                   GBIBPGM 
01816  2280-900-EXIT.                                                   GBIBPGM 
01817           EXIT.                                                   GBIBPGM 
01818 /*****************************************************************GBIBPGM 
01819 **                                                               *GBIBPGM 
01820 ** 2300     LOAD DATES TABLE                                     *GBIBPGM 
01821 **                                                               *GBIBPGM 
01822 ******************************************************************GBIBPGM 
01823  2300-000-LOAD-DATES-TABLE     SECTION.                           GBIBPGM 
01824  2300-010.                                                        GBIBPGM 
01825                                                                   GBIBPGM 
01826      MOVE '2300'  TO  WS-PARA-ID.                                 GBIBPGM 
01827                                                                   GBIBPGM 
01828      IF GI-MULT-PATH-ID = 'GBIG'                                  GBIBPGM 
01829         MOVE GIGT-SLOT-NUMBER TO WS-COMP-SERV-DT-CEN              GBIBPGM 
01830         IF (DTE-EFFDT-CEN(DTE-INDEX) NOT > WS-COMP-SERV-DT-CEN)   GBIBPGM 
01831             AND                                                   GBIBPGM 
01832            (DTE-TERMDT-CEN(DTE-INDEX) NOT < WS-COMP-SERV-DT-CEN)  GBIBPGM 
01833             CONTINUE                                              GBIBPGM 
01834         ELSE                                                      GBIBPGM 
01835             GO TO 2300-900-EXIT                                   GBIBPGM 
01836         END-IF                                                    GBIBPGM 
01837      END-IF.                                                      GBIBPGM 
01838                                                                   GBIBPGM 
01839                                                                   GBIBPGM 
01840                                                                   GBIBPGM 
01841      ADD +1 TO WS-DT-SUB                                          GBIBPGM 
01842                WS-DT-SUBX.                                        GBIBPGM 
01843                                                                   GBIBPGM 
01844      MOVE DTE-EFFDT-CEN(DTE-INDEX)  TO DT-EFF-DT-CEN(WS-DT-SUB).  GBIBPGM 
01845      MOVE DTE-CONTR-L-O-B       TO DT-L-O-B(WS-DT-SUB).           GBIBPGM 
01846      MOVE DTE-PROVIDER-CNTRL(DTE-INDEX)                           GBIBPGM 
01847                                 TO DT-PROV-CNTL(WS-DT-SUB).       GBIBPGM 
01848      MOVE DTE-FAMILY-RELAT-LEVEL(DTE-INDEX)                       GBIBPGM 
01849                                 TO DT-FAM-REL(WS-DT-SUB).         GBIBPGM 
01850      MOVE DTE-TERMDT-CEN(DTE-INDEX) TO DT-TERM-DT-CEN(WS-DT-SUB). GBIBPGM 
01851                                                                   GBIBPGM 
01852 *        +----------------------------------------+               GBIBPGM 
01853 *        +  COMPLIMENT JULIAN EFFECTIVE DATE IN   +               GBIBPGM 
01854 *        +   DATES TABLE FOR SORT, THIS CAUSE     +               GBIBPGM 
01855 *        +   THE DATE TO BE SORTED DESCENDINGLY.  +               GBIBPGM 
01856 *        +----------------------------------------+               GBIBPGM 
01857                                                                   GBIBPGM 
01858      COMPUTE DT-EFF-DT-CEN(WS-DT-SUB) =                           GBIBPGM 
01859              +9999999             -                               GBIBPGM 
01860              DT-EFF-DT-CEN(WS-DT-SUB).                            GBIBPGM 
01861                                                                   GBIBPGM 
01862  2300-900-EXIT.                                                   GBIBPGM 
01863      EXIT.                                                        GBIBPGM 
01864 /*****************************************************************GBIBPGM 
01865 **                                                               *GBIBPGM 
01866 ** 2400     SORT DATES TABLE                                     *GBIBPGM 
01867 **                                                               *GBIBPGM 
01868 ******************************************************************GBIBPGM 
01869  2400-000-SORT-DATES-TABLE     SECTION.                           GBIBPGM 
01870  2400-010.                                                        GBIBPGM 
01871                                                                   GBIBPGM 
01872      MOVE '2400'  TO  WS-PARA-ID.                                 GBIBPGM 
01873                                                                   GBIBPGM 
01874      MOVE 'N' TO WS-SORT-EXCHANGE-INDICATOR.                      GBIBPGM 
01875                                                                   GBIBPGM 
01876      PERFORM 2410-000-SORT-ENTRY-EXCHANGE                         GBIBPGM 
01877         VARYING WS-DT-SUB2 FROM 1 BY 1                            GBIBPGM 
01878 ***       UNTIL WS-DT-SUB2 > DT-ENTRY-COUNT.                      GBIBPGM 
01879           UNTIL WS-DT-SUB2 > WS-DT-SUBX.                          GBIBPGM 
01880                                                                   GBIBPGM 
01881  2400-900-EXIT.                                                   GBIBPGM 
01882      EXIT.                                                        GBIBPGM 
01883 /*****************************************************************GBIBPGM 
01884 **                                                               *GBIBPGM 
01885 ** 2410     SORT ENTRY EXCHANGE                                  *GBIBPGM 
01886 **                                                               *GBIBPGM 
01887 ******************************************************************GBIBPGM 
01888  2410-000-SORT-ENTRY-EXCHANGE  SECTION.                           GBIBPGM 
01889  2410-010.                                                        GBIBPGM 
01890                                                                   GBIBPGM 
01891      MOVE '2410'  TO  WS-PARA-ID.                                 GBIBPGM 
01892                                                                   GBIBPGM 
01893      COMPUTE WS-DT-SUB3 = WS-DT-SUB2 + 1.                         GBIBPGM 
01894 ***  IF  WS-DT-SUB3 > DT-ENTRY-COUNT                              GBIBPGM 
01895      IF  WS-DT-SUB3 > WS-DT-SUBX                                  GBIBPGM 
01896          GO TO 2410-900-EXIT.                                     GBIBPGM 
01897                                                                   GBIBPGM 
01898      IF  DT-ENTRY(WS-DT-SUB3) < DT-ENTRY(WS-DT-SUB2)              GBIBPGM 
01899          MOVE 'Y'                  TO WS-SORT-EXCHANGE-INDICATOR  GBIBPGM 
01900          MOVE DT-ENTRY(WS-DT-SUB3) TO WS-HOLD-DT-ENTRY            GBIBPGM 
01901          MOVE DT-ENTRY(WS-DT-SUB2) TO DT-ENTRY(WS-DT-SUB3)        GBIBPGM 
01902          MOVE WS-HOLD-DT-ENTRY     TO DT-ENTRY(WS-DT-SUB2).       GBIBPGM 
01903                                                                   GBIBPGM 
01904  2410-900-EXIT.                                                   GBIBPGM 
01905      EXIT.                                                        GBIBPGM 
01906 /*****************************************************************GBIBPGM 
01907 **                                                               *GBIBPGM 
01908 ** 2500     UNCOMPLIMENT EFFECTIVE DATE                          *GBIBPGM 
01909 **                                                               *GBIBPGM 
01910 ******************************************************************GBIBPGM 
01911  2500-000-UNCOMPLIMENT-EFF-DATE SECTION.                          GBIBPGM 
01912  2500-010.                                                        GBIBPGM 
01913                                                                   GBIBPGM 
01914      MOVE '2500'  TO  WS-PARA-ID.                                 GBIBPGM 
01915                                                                   GBIBPGM 
01916      COMPUTE DT-EFF-DT-CEN(WS-DT-SUB) =                           GBIBPGM 
01917              +9999999             -                               GBIBPGM 
01918              DT-EFF-DT-CEN(WS-DT-SUB).                            GBIBPGM 
01919                                                                   GBIBPGM 
01920  2500-900-EXIT.                                                   GBIBPGM 
01921      EXIT.                                                        GBIBPGM 
01922 /*****************************************************************GBIBPGM 
01923 **                                                               *GBIBPGM 
01924 ** 2600     CLEAR SCREEN TABLE                                   *GBIBPGM 
01925 **                                                               *GBIBPGM 
01926 ******************************************************************GBIBPGM 
01927  2600-000-CLEAR-SCREEN-TABLE    SECTION.                          GBIBPGM 
01928  2600-010.                                                        GBIBPGM 
01929                                                                   GBIBPGM 
01930      MOVE '2600'  TO  WS-PARA-ID.                                 GBIBPGM 
01931                                                                   GBIBPGM 
01932      MOVE LOW-VALUES TO IBLNI-ENTRY (WS-SCREEN-SUB).              GBIBPGM 
01933      MOVE DFHBMASF   TO IBLNA (WS-SCREEN-SUB)                     GBIBPGM 
01934                         IBLOBA(WS-SCREEN-SUB)                     GBIBPGM 
01935                         IBPRVA(WS-SCREEN-SUB)                     GBIBPGM 
01936                         IBFRA (WS-SCREEN-SUB)                     GBIBPGM 
01937                         IBEDTA(WS-SCREEN-SUB)                     GBIBPGM 
01938                         IBTDTA(WS-SCREEN-SUB).                    GBIBPGM 
01939                                                                   GBIBPGM 
01940  2600-900-EXIT.                                                   GBIBPGM 
01941      EXIT.                                                        GBIBPGM 
01942                                                                   GBIBPGM 
01943 /*****************************************************************GBIBPGM 
01944 **                                                               *GBIBPGM 
01945 ** 2700     BUILD SELECT LIST                                    *GBIBPGM 
01946 **                                                               *GBIBPGM 
01947 ******************************************************************GBIBPGM 
01948  2700-000-BUILD-SELECT-LIST     SECTION.                          GBIBPGM 
01949  2700-010.                                                        GBIBPGM 
01950                                                                   GBIBPGM 
01951      MOVE '2700'  TO  WS-PARA-ID.                                 GBIBPGM 
01952                                                                   GBIBPGM 
01953      ADD 1    TO WS-DT-SUB.                                       GBIBPGM 
01954      MOVE 'Y' TO WS-SELECT-MATCH-INDICATOR.                       GBIBPGM 
01955                                                                   GBIBPGM 
01956      IF  WS-ENTERED-IBLOBX       = '1'      AND                   GBIBPGM 
01957          DT-L-O-B(WS-DT-SUB) NOT = WS-L-O-B                       GBIBPGM 
01958          MOVE 'N' TO WS-SELECT-MATCH-INDICATOR.                   GBIBPGM 
01959                                                                   GBIBPGM 
01960      IF  WS-ENTERED-IBPRVX           = '1'      AND               GBIBPGM 
01961          DT-PROV-CNTL(WS-DT-SUB) NOT = WS-PROV-CTL                GBIBPGM 
01962          MOVE 'N' TO WS-SELECT-MATCH-INDICATOR.                   GBIBPGM 
01963                                                                   GBIBPGM 
01964      IF  WS-ENTERED-IBFRX          = '1'     AND                  GBIBPGM 
01965          DT-FAM-REL(WS-DT-SUB) NOT = WS-FAM-REL-LVL               GBIBPGM 
01966          MOVE 'N' TO WS-SELECT-MATCH-INDICATOR.                   GBIBPGM 
01967                                                                   GBIBPGM 
01968      IF  WS-ENTERED-IBEDTX         = '1'      AND                 GBIBPGM 
01969          DT-EFF-DT-CEN(WS-DT-SUB)      > WS-EFF-DATE-CEN          GBIBPGM 
01970          MOVE 'N' TO WS-SELECT-MATCH-INDICATOR.                   GBIBPGM 
01971                                                                   GBIBPGM 
01972      IF  WS-SELECT-MATCH-INDICATOR = 'N'                          GBIBPGM 
01973          GO TO 2700-900-EXIT.                                     GBIBPGM 
01974                                                                   GBIBPGM 
01975                                                                   GBIBPGM 
01976      ADD 1 TO WS-SCREEN-LN                                        GBIBPGM 
01977               WS-SCREEN-SUB.                                      GBIBPGM 
01978                                                                   GBIBPGM 
01979                                                                   GBIBPGM 
01980 ***  IF  DT-ENTRY-COUNT < +2                                      GBIBPGM 
01981      IF  WS-DT-SUBX     < +2                                      GBIBPGM 
01982          PERFORM 2900-000-SINGLE-REC-PROC                         GBIBPGM 
01983          GO TO 2700-900-EXIT                                      GBIBPGM 
01984      END-IF.                                                      GBIBPGM 
01985                                                                   GBIBPGM 
01986      MOVE DFHBMABF                TO IBLNA(WS-SCREEN-SUB).        GBIBPGM 
01987      MOVE WS-SCREEN-LN            TO IBLNO(WS-SCREEN-SUB)         GBIBPGM 
01988                                      IBLNCNTO.                    GBIBPGM 
01989                                                                   GBIBPGM 
01990      MOVE DFHBMASF                TO IBLOBA(WS-SCREEN-SUB).       GBIBPGM 
01991      MOVE DT-L-O-B(WS-DT-SUB)     TO IBLOBO(WS-SCREEN-SUB).       GBIBPGM 
01992                                                                   GBIBPGM 
01993      MOVE DFHBMASF                TO IBPRVA(WS-SCREEN-SUB).       GBIBPGM 
01994      MOVE DT-PROV-CNTL(WS-DT-SUB) TO IBPRVO(WS-SCREEN-SUB).       GBIBPGM 
01995                                                                   GBIBPGM 
01996      MOVE DFHBMASF                TO IBFRA(WS-SCREEN-SUB).        GBIBPGM 
01997      MOVE DT-FAM-REL(WS-DT-SUB)   TO IBFRO(WS-SCREEN-SUB).        GBIBPGM 
01998                                                                   GBIBPGM 
01999      MOVE DT-EFF-DT(WS-DT-SUB)    TO HGADATE-JULIAN1.             GBIBPGM 
02000      PERFORM 9210-000-JULIAN-TO-GREGORIAN.                        GBIBPGM 
02001      IF  HGADATE-RETURN = ZEROS                                   GBIBPGM 
02002          MOVE DFHBMASF        TO IBEDTA(WS-SCREEN-SUB)            GBIBPGM 
02003          MOVE HGADATE-DATE2   TO IBEDTO(WS-SCREEN-SUB)            GBIBPGM 
02004      ELSE                                                         GBIBPGM 
02005          MOVE DFHBMABF        TO IBEDTA(WS-SCREEN-SUB)            GBIBPGM 
02006          MOVE HGADATE-JULIAN1 TO IBEDTO(WS-SCREEN-SUB).           GBIBPGM 
02007                                                                   GBIBPGM 
02008      MOVE DT-TERM-DT-CEN(WS-DT-SUB)    TO MLDATE-DATE1.           GBIBPGM 
02009      PERFORM 9215-000-JULIAN-GREG-CEN.                            GBIBPGM 
02010      IF  MLDATE-RETURN = ZEROS                                    GBIBPGM 
02011          MOVE DFHBMASF        TO IBTDTA(WS-SCREEN-SUB)            GBIBPGM 
02012          MOVE MLDATE-DATE2    TO IBTDTO(WS-SCREEN-SUB)            GBIBPGM 
02013      ELSE                                                         GBIBPGM 
02014          MOVE DFHBMABF        TO IBTDTA(WS-SCREEN-SUB)            GBIBPGM 
02015          MOVE MLDATE-DATE1    TO IBTDTO(WS-SCREEN-SUB).           GBIBPGM 
02016                                                                   GBIBPGM 
02017  2700-900-EXIT.                                                   GBIBPGM 
02018      EXIT.                                                        GBIBPGM 
02019 /*****************************************************************GBIBPGM 
02020 **                                                               *GBIBPGM 
02021 ** 2800     C L E A R   T H E   M A P   S C R E E N              *GBIBPGM 
02022 ** AHL 11/24/86                                                  *GBIBPGM 
02023 **                                                               *GBIBPGM 
02024 ** USED ONLY TO CLEAR THE OCCURS SECTION OF THE SCREEN IF THE    *GBIBPGM 
02025 **      GROUP AND SECTION NUMBER DO NOT HAVE ANY VALID DATE      *GBIBPGM 
02026 **      RECORDS.                                                 *GBIBPGM 
02027 **                                                               *GBIBPGM 
02028 ******************************************************************GBIBPGM 
02029  2800-000-CLEAR-MAP-TABLE       SECTION.                          GBIBPGM 
02030  2800-010.                                                        GBIBPGM 
02031      MOVE '2800'  TO  WS-PARA-ID.                                 GBIBPGM 
02032                                                                   GBIBPGM 
02033      MOVE SPACES     TO IBLNI-ENTRY (WS-CLEAR-INDEX).             GBIBPGM 
02034      MOVE DFHBMASF   TO IBLNA (WS-CLEAR-INDEX)                    GBIBPGM 
02035                         IBLOBA(WS-CLEAR-INDEX)                    GBIBPGM 
02036                         IBPRVA(WS-CLEAR-INDEX)                    GBIBPGM 
02037                         IBFRA (WS-CLEAR-INDEX)                    GBIBPGM 
02038                         IBEDTA(WS-CLEAR-INDEX)                    GBIBPGM 
02039                         IBTDTA(WS-CLEAR-INDEX).                   GBIBPGM 
02040                                                                   GBIBPGM 
02041  2800-010-EXIT.                                                   GBIBPGM 
02042      EXIT.                                                        GBIBPGM 
02043                                                                   GBIBPGM 
02044 ******************************************************************GBIBPGM 
02045 **                                                               *GBIBPGM 
02046 **                                                               *GBIBPGM 
02047 ******************************************************************GBIBPGM 
02048  2900-000-SINGLE-REC-PROC       SECTION.                          GBIBPGM 
02049  2900-010.                                                        GBIBPGM 
02050                                                                   GBIBPGM 
02051      MOVE '2900'  TO  WS-PARA-ID.                                 GBIBPGM 
02052                                                                   GBIBPGM 
02053      MOVE DT-L-O-B(WS-DT-SUB)     TO IBLOBXO                      GBIBPGM 
02054      MOVE +1                      TO IBLOBXL                      GBIBPGM 
02055                                                                   GBIBPGM 
02056      MOVE DT-PROV-CNTL(WS-DT-SUB) TO IBPRVXO                      GBIBPGM 
02057      MOVE +2                      TO  IBPRVXL                     GBIBPGM 
02058                                                                   GBIBPGM 
02059      MOVE DT-FAM-REL(WS-DT-SUB)   TO IBFRXO                       GBIBPGM 
02060      MOVE +2                      TO  IBFRXL                      GBIBPGM 
02061                                                                   GBIBPGM 
02062      MOVE DT-EFF-DT(WS-DT-SUB)    TO HGADATE-JULIAN1.             GBIBPGM 
02063      PERFORM 9210-000-JULIAN-TO-GREGORIAN.                        GBIBPGM 
02064                                                                   GBIBPGM 
02065      IF  HGADATE-RETURN = ZEROS                                   GBIBPGM 
02066          MOVE HGADATE-DATE2   TO IBEDTXO                          GBIBPGM 
02067          MOVE +6              TO  IBEDTXL                         GBIBPGM 
02068      ELSE                                                         GBIBPGM 
02069          MOVE HGADATE-JULIAN1 TO IBEDTXO                          GBIBPGM 
02070          MOVE +6              TO  IBEDTXL                         GBIBPGM 
02071      END-IF.                                                      GBIBPGM 
02072                                                                   GBIBPGM 
02073                                                                   GBIBPGM 
02074  2900-900-EXIT.                                                   GBIBPGM 
02075      EXIT.                                                        GBIBPGM 
02076 /*****************************************************************GBIBPGM 
02077 **                                                               *GBIBPGM 
02078 ** 3000     D I S P L A Y   F I R S T   S C R E E N              *GBIBPGM 
02079 **                                                               *GBIBPGM 
02080 ******************************************************************GBIBPGM 
02081  3000-000-DISPLAY-FIRST-SCREEN SECTION.                           GBIBPGM 
02082  3000-010.                                                        GBIBPGM 
02083                                                                   GBIBPGM 
02084      MOVE '3000'  TO  WS-PARA-ID.                                 GBIBPGM 
02085                                                                   GBIBPGM 
02086 *        +----------------------------------------+               GBIBPGM 
02087 *        +  ENTRY FROM HIGHER LEVEL MENU?         +               GBIBPGM 
02088 *        +----------------------------------------+               GBIBPGM 
02089                                                                   GBIBPGM 
02090      IF  EIBTRNID = 'GCGI'  OR 'GCPS'                             GBIBPGM 
02091          GO TO 3000-300-SEND-SCREEN.                              GBIBPGM 
02092                                                                   GBIBPGM 
02093 *        +----------------------------------------+               GBIBPGM 
02094 *        +  ENTRY FROM LOWER  LEVEL MENU?         +               GBIBPGM 
02095 *        +----------------------------------------+               GBIBPGM 
02096                                                                   GBIBPGM 
02097 *** MAM, G&R - ADDED TRAN-IDS OKAY TO PROCESS ***                 GBIBPGM 
02098      IF  EIBTRNID = 'GI3A' OR 'GI5A' OR 'GX5Q' OR 'GCL2' OR       GBIBPGM 
02099                     'GIBC' OR 'GX5T' OR 'GJ2A' OR 'GJ2B' OR       GBIBPGM 
02100                     'GJ2C' OR 'GJ2D' OR 'GJ2E' OR 'GJ2F' OR       GBIBPGM 
02101                     'GJ2G' OR 'GJ2H' OR 'GJ2I' OR 'GJ2J' OR       GBIBPGM 
02102                     'GJ2K' OR 'GJ2L' OR 'GJ3A' OR 'GJ1A' OR       GBIBPGM 
02103                     'GIQ3'                                        GBIBPGM 
02104          GO TO 3000-100-FILL-SCREEN.                              GBIBPGM 
02105                                                                   GBIBPGM 
02106 *        +----------------------------------------+               GBIBPGM 
02107 *        +  ENTRY FROM GI2A THROUGH GI2G?         +               GBIBPGM 
02108 *        +----------------------------------------+               GBIBPGM 
02109                                                                   GBIBPGM 
02110      IF  EIBTRNID < 'GI2A'   OR   EIBTRNID > 'GI2G'               GBIBPGM 
02111          MOVE '*** GBIBPGM GOT CONTROL FROM AN UNAUTHORIZED SOURC GBIBPGM 
02112 -             'E, PLEASE CALL SYSTEMS ***'                        GBIBPGM 
02113            TO IBMSGO(1)                                           GBIBPGM 
02114          MOVE 'LINK' TO WS-ABEND-CODE                             GBIBPGM 
02115          PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                   GBIBPGM 
02116                                                                   GBIBPGM 
02117  3000-100-FILL-SCREEN.                                            GBIBPGM 
02118                                                                   GBIBPGM 
02119      IF EIBCALEN NOT =  150                                       GBIBPGM 
02120         MOVE '                  *** COMMONAREA LENGTH INVALID ***'GBIBPGM 
02121           TO IBMSGO(1)                                            GBIBPGM 
02122         MOVE 'CALN'  TO  WS-ABEND-CODE                            GBIBPGM 
02123         PERFORM 9900-000-ERROR-MSG-THEN-ABEND.                    GBIBPGM 
02124                                                                   GBIBPGM 
02125 *** MAM, G&R ***                                                  GBIBPGM 
02126      IF EIBTRNID = 'GIQ3'                                         GBIBPGM 
02127         MOVE GIC-PLAN-CODE     TO IBPLNXO                         GBIBPGM 
02128         MOVE GIC-GROUP-NUM     TO IBGRPXO                         GBIBPGM 
02129         MOVE GIC-SECTION-NUM   TO IBSECXO                         GBIBPGM 
02130         MOVE GIC-PKG-CODE      TO IBPKGXO                         GBIBPGM 
02131         MOVE GIC-L-O-B         TO IBLOBXO                         GBIBPGM 
02132         MOVE GIC-PROV-CTL      TO IBPRVXO                         GBIBPGM 
02133         MOVE GIC-FAM-REL-LVL   TO IBFRXO                          GBIBPGM 
02134         MOVE GIC-EFF-DT        TO HGADATE-JULIAN1                 GBIBPGM 
02135         PERFORM 9210-000-JULIAN-TO-GREGORIAN                      GBIBPGM 
02136         IF  HGADATE-RETURN = ZEROS                                GBIBPGM 
02137             MOVE HGADATE-DATE2   TO IBEDTXO                       GBIBPGM 
02138         END-IF                                                    GBIBPGM 
02139         MOVE ' ***  THIS PLAN/GROUP/SEC IS NOT ON THE CONTRACT FILGBIBPGM 
02140 -            'E  ***'                                             GBIBPGM 
02141             TO IBMSGO(1)                                          GBIBPGM 
02142         MOVE -1 TO IBGRPXL                                        GBIBPGM 
02143         PERFORM 7600-000-SEND-ERASE                               GBIBPGM 
02144         GO TO 3000-900-EXIT.                                      GBIBPGM 
02145                                                                   GBIBPGM 
02146 *** MAM, G&R - ADDED TRAN-IDS FOR GROUP SPECIFIC                  GBIBPGM 
02147      IF EIBTRNID = 'GJ2A' OR 'GJ2B' OR 'GJ2C' OR 'GJ2D' OR        GBIBPGM 
02148                    'GJ2E' OR 'GJ2F' OR 'GJ2G' OR 'GJ2H' OR        GBIBPGM 
02149                    'GJ2I' OR 'GJ2J' OR 'GJ2K' OR 'GJ2L' OR        GBIBPGM 
02150                    'GJ3A' OR 'GJ1A'                               GBIBPGM 
02151         MOVE GIG-PLAN-CODE     TO IBPLNXO                         GBIBPGM 
02152         MOVE GIG-GROUP-NUM     TO IBGRPXO                         GBIBPGM 
02153         MOVE GIG-SECTION-NUM   TO IBSECXO                         GBIBPGM 
02154         MOVE GIG-PKG-CODE      TO IBPKGXO                         GBIBPGM 
02155         PERFORM 2200-000-PARTIAL-KEY-SELECTED                     GBIBPGM 
02156         GO TO 3000-900-EXIT                                       GBIBPGM 
02157      ELSE                                                         GBIBPGM 
02158         MOVE GIC-PLAN-CODE     TO IBPLNXO                         GBIBPGM 
02159         MOVE GIC-GROUP-NUM     TO IBGRPXO                         GBIBPGM 
02160         MOVE GIC-SECTION-NUM   TO IBSECXO                         GBIBPGM 
02161         MOVE GIC-PKG-CODE      TO IBPKGXO                         GBIBPGM 
02162         MOVE GIC-L-O-B         TO IBLOBXO                         GBIBPGM 
02163         MOVE GIC-PROV-CTL      TO IBPRVXO                         GBIBPGM 
02164         MOVE GIC-FAM-REL-LVL   TO IBFRXO                          GBIBPGM 
02165         MOVE GI-EFFECTIVE-DATE TO IBEDTXO.                        GBIBPGM 
02166                                                                   GBIBPGM 
02167 *    IF  EIBTRNID     =  'GIBC'                                   GBIBPGM 
02168 *        IF  GI-RETURN-CODE   >  ZEROES                           GBIBPGM 
02169 *         MOVE ' ***  THIS GROUP IS NOT ON THE CONTRACT FILE   ***GBIBPGM 
02170 *              '      '                                           GBIBPGM 
02171 *           TO IBMSGO(1)                                          GBIBPGM 
02172 *    ELSE                                                         GBIBPGM 
02173 *       IF  EIBTRNID  = 'GIBC'                                    GBIBPGM 
02174 *           IF  GI-RETURN-CODE  =  ZEROES                         GBIBPGM 
02175 *               IF  GIC-GROUP-NUM NOT =  LOW-VALUES               GBIBPGM 
02176 *                   PERFORM 2200-000-PARTIAL-KEY-SELECTED         GBIBPGM 
02177 *                   GO TO 3000-900-EXIT.                          GBIBPGM 
02178                                                                   GBIBPGM 
02179  3000-300-SEND-SCREEN.                                            GBIBPGM 
02180                                                                   GBIBPGM 
02181      MOVE -1 TO IBGRPXL.                                          GBIBPGM 
02182      MOVE ZEROES TO IBPLNXO.                                      GBIBPGM 
02183      PERFORM 7600-000-SEND-ERASE.                                 GBIBPGM 
02184                                                                   GBIBPGM 
02185  3000-900-EXIT.                                                   GBIBPGM 
02186      EXIT.                                                        GBIBPGM 
02187 /*****************************************************************GBIBPGM 
02188 **                                                               *GBIBPGM 
02189 ** 4000     SEARCH FOR CORRESPONDING GROUP SPECIFIC RECORD       *GBIBPGM 
02190 **                                                               *GBIBPGM 
02191 **   1. LOAD PARTIAL GROUP SPECIFIC KEY FROM CONTRACT RECORD     *GBIBPGM 
02192 **   2. READ GROUP SPECIFIC FILE                                 *GBIBPGM 
02193 **   2. BROWSE GROUP SPECIFIC RECORDS FOR CORRESPONDING DATES    *GBIBPGM 
02194 **                                                               *GBIBPGM 
02195 ******************************************************************GBIBPGM 
02196  4000-000-SEARCH-GROUP-SPEC    SECTION.                           GBIBPGM 
02197  4000-010.                                                        GBIBPGM 
02198                                                                   GBIBPGM 
02199      MOVE '4000'  TO  WS-PARA-ID.                                 GBIBPGM 
02200                                                                   GBIBPGM 
02201      MOVE LOW-VALUES  TO WS-GROUPSPC-KEY                          GBIBPGM 
02202                                                                   GBIBPGM 
02203      MOVE IBPLNXO     TO WS-PLAN-CODE-GS                          GBIBPGM 
02204                          WS-HOLD-PLAN                             GBIBPGM 
02205      MOVE IBGRPXO     TO WS-GRP-NUMBER-GS                         GBIBPGM 
02206                          WS-HOLD-GROUP                            GBIBPGM 
02207      MOVE IBSECXO     TO WS-SECTN-NUMBER-GS                       GBIBPGM 
02208                          WS-HOLD-SECTION                          GBIBPGM 
02209      MOVE IBPKGXO     TO WS-PKG-CODE-GS                           GBIBPGM 
02210                          WS-HOLD-PKG                              GBIBPGM 
02211 **   MOVE IBFRXO      TO WS-FAM-REL-LVL-GS                        GBIBPGM 
02212 **                       WS-HOLD-FRL                              GBIBPGM 
02213                                                                   GBIBPGM 
02214      MOVE 'N'         TO WS-END-GROUP-TEST-SW                     GBIBPGM 
02215                          WS-GROUP-MATCH-FOUND-SW                  GBIBPGM 
02216                                                                   GBIBPGM 
02217      PERFORM 8200-000-STBR-GROUPSPC.                              GBIBPGM 
02218                                                                   GBIBPGM 
02219      IF  GCIO3-GOOD-RETURN                                        GBIBPGM 
02220          MOVE  GCIO3-FILE-KEY  TO GCIO-GROUP-SPECIFIC             GBIBPGM 
02221          IF (GCIO-GRP-PLAN-CODE   =  WS-HOLD-PLAN)     AND        GBIBPGM 
02222             (GCIO-GRP-GROUP-NUM   =  WS-HOLD-GROUP)    AND        GBIBPGM 
02223             (GCIO-GRP-SECTION-NUM =  WS-HOLD-SECTION)  AND        GBIBPGM 
02224             (GCIO-GRP-PKG-CODE    =  WS-HOLD-PKG)                 GBIBPGM 
02225             PERFORM 4100-000-TEST-GRP-RECS                        GBIBPGM 
02226               UNTIL    END-GROUP-TEST OR                          GBIBPGM 
02227                        GROUP-MATCH-FOUND                          GBIBPGM 
02228      ELSE                                                         GBIBPGM 
02229          IF  GCIO3-RECORD-NOT-FOUND OR GCIO3-END-OF-FILE          GBIBPGM 
02230            MOVE  SPACES  TO  IBPAGEO                              GBIBPGM 
02231            MOVE  -1      TO  IBPLNXL                              GBIBPGM 
02232            MOVE '*** GROUPSPC RECORD NOT FOUND/STBR' TO IBMSGO(2) GBIBPGM 
02233            MOVE '1' TO WS-ERROR-SWITCH                            GBIBPGM 
02234            GO TO 4000-900-EXIT                                    GBIBPGM 
02235          ELSE                                                     GBIBPGM 
02236            MOVE  SPACES  TO  IBPAGEO                              GBIBPGM 
02237            MOVE  -1      TO  IBPLNXL                              GBIBPGM 
02238            MOVE '*** GROUPSPC START BROWSE ERR ****' TO IBMSGO(2) GBIBPGM 
02239            MOVE '1' TO WS-ERROR-SWITCH                            GBIBPGM 
02240            GO TO 4000-900-EXIT                                    GBIBPGM 
02241      END-IF.                                                      GBIBPGM 
02242                                                                   GBIBPGM 
02243      MOVE '*** TEST MESSAGE FOR 4000 ***'      TO IBMSGO(2).      GBIBPGM 
02244                                                                   GBIBPGM 
02245  4000-900-EXIT.                                                   GBIBPGM 
02246      EXIT.                                                        GBIBPGM 
02247                                                                   GBIBPGM 
02248  4100-000-TEST-GRP-RECS        SECTION.                           GBIBPGM 
02249  4100-010.                                                        GBIBPGM 
02250                                                                   GBIBPGM 
02251      MOVE '4100'  TO  WS-PARA-ID.                                 GBIBPGM 
02252                                                                   GBIBPGM 
02253      PERFORM 8300-000-RDNX-GROUPSPC                               GBIBPGM 
02254                                                                   GBIBPGM 
02255      IF  GCIO3-GOOD-RETURN                                        GBIBPGM 
02256          MOVE  GCIO3-FILE-KEY  TO GCIO-GROUP-SPECIFIC             GBIBPGM 
02257          IF (GCIO-GRP-PLAN-CODE   =  WS-HOLD-PLAN)     AND        GBIBPGM 
02258             (GCIO-GRP-GROUP-NUM   =  WS-HOLD-GROUP)    AND        GBIBPGM 
02259             (GCIO-GRP-SECTION-NUM =  WS-HOLD-SECTION)  AND        GBIBPGM 
02260             (GCIO-GRP-PKG-CODE    =  WS-HOLD-PKG)                 GBIBPGM 
02261 ***         PERFORM 4100-020-TEST-GRP-RECS                        GBIBPGM 
02262             CONTINUE                                              GBIBPGM 
02263          ELSE                                                     GBIBPGM 
02264             MOVE 'Y'   TO WS-END-GROUP-TEST-SW                    GBIBPGM 
02265             GO TO 4100-900-EXIT                                   GBIBPGM 
02266          END-IF                                                   GBIBPGM 
02267      ELSE                                                         GBIBPGM 
02268          MOVE  SPACES  TO  IBPAGEO                                GBIBPGM 
02269          MOVE  -1      TO  IBPLNXL                                GBIBPGM 
02270          MOVE '*** GROUP SPEC READ ERROR 4100****' TO IBMSGO(2)   GBIBPGM 
02271          MOVE 'Y'   TO WS-END-GROUP-TEST-SW                       GBIBPGM 
02272          GO TO 4100-900-EXIT                                      GBIBPGM 
02273      END-IF.                                                      GBIBPGM 
02274                                                                   GBIBPGM 
02275                                                                   GBIBPGM 
02276 *4100-020-TEST-GRP-RECS.                                          GBIBPGM 
02277                                                                   GBIBPGM 
02278      IF GI-RETURN-CODE = 'CT'                                     GBIBPGM 
02279         IF (GCG-FAM-REL-LVL    =   GCT-FAM-REL-LVL)  AND          GBIBPGM 
02280            (GCG-EFFDT-CEN     <=   GCT-EFFDT-CEN)    AND          GBIBPGM 
02281            (GCG-TERMDT-CEN    >=   GCT-TERMDT-CEN)                GBIBPGM 
02282            MOVE 'Y'   TO WS-GROUP-MATCH-FOUND-SW                  GBIBPGM 
02283            MOVE  GCG-PLAN-CODE    TO   WS-DISP-PLAN               GBIBPGM 
02284            MOVE  GCG-GROUP-NUM    TO   WS-DISP-GROUP              GBIBPGM 
02285            MOVE  GCG-SECTION-NUM  TO   WS-DISP-SECTION            GBIBPGM 
02286            MOVE  GCG-PKG-CODE     TO   WS-DISP-PKG                GBIBPGM 
02287            MOVE  GCG-FAM-REL-LVL  TO   WS-DISP-FRL                GBIBPGM 
02288            MOVE  GCG-EFFDT-CEN    TO   WS-DISP-EFFDT              GBIBPGM 
02289            MOVE  GCG-TERMDT-CEN   TO   WS-DISP-TRMDT              GBIBPGM 
02290 ***        GO TO 4100-900-EXIT                                    GBIBPGM 
02291 ***     ELSE                                                      GBIBPGM 
02292 ***        GO TO 4100-900-EXIT                                    GBIBPGM 
02293         END-IF                                                    GBIBPGM 
02294      END-IF                                                       GBIBPGM 
02295                                                                   GBIBPGM 
02296      IF GI-RETURN-CODE = 'GS'                                     GBIBPGM 
02297         IF (GCG-FAM-REL-LVL    =   GCT-FAM-REL-LVL)  AND          GBIBPGM 
02298            (WS-COMP-SERV-DT-CEN   >=   GCG-EFFDT-CEN)    AND      GBIBPGM 
02299            (WS-COMP-SERV-DT-CEN   <=   GCG-TERMDT-CEN)            GBIBPGM 
02300            MOVE 'Y'   TO WS-GROUP-MATCH-FOUND-SW                  GBIBPGM 
02301            MOVE  GCG-PLAN-CODE    TO   WS-DISP-PLAN               GBIBPGM 
02302            MOVE  GCG-GROUP-NUM    TO   WS-DISP-GROUP              GBIBPGM 
02303            MOVE  GCG-SECTION-NUM  TO   WS-DISP-SECTION            GBIBPGM 
02304            MOVE  GCG-PKG-CODE     TO   WS-DISP-PKG                GBIBPGM 
02305            MOVE  GCG-FAM-REL-LVL  TO   WS-DISP-FRL                GBIBPGM 
02306            MOVE  GCG-EFFDT-CEN    TO   WS-DISP-EFFDT              GBIBPGM 
02307            MOVE  GCG-TERMDT-CEN   TO   WS-DISP-TRMDT              GBIBPGM 
02308 ***        GO TO 4100-900-EXIT                                    GBIBPGM 
02309 ***     ELSE                                                      GBIBPGM 
02310 ***        GO TO 4100-900-EXIT                                    GBIBPGM 
02311         END-IF                                                    GBIBPGM 
02312      END-IF.                                                      GBIBPGM 
02313                                                                   GBIBPGM 
02314                                                                   GBIBPGM 
02315  4100-900-EXIT.                                                   GBIBPGM 
02316      EXIT.                                                        GBIBPGM 
02317                                                                   GBIBPGM 
02318 /*****************************************************************GBIBPGM 
02319 **                                                              **GBIBPGM 
02320 **           X C T L   T O   GBIEPGM TO CHOOSE GROUP SPECIFIC   **GBIBPGM 
02321 **                           RECORD                             **GBIBPGM 
02322 ******************************************************************GBIBPGM 
02323  5000-000-XCTL-TO-GBIE          SECTION.                          GBIBPGM 
02324  5000-010.                                                        GBIBPGM 
02325                                                                   GBIBPGM 
02326                                                                   GBIBPGM 
02327      MOVE 'GBIEPGM' TO WS-XCTL-TO-PGM                             GBIBPGM 
02328                                                                   GBIBPGM 
02329      MOVE  GCT-TERMDT-CEN   TO   GICT2-SLOT-NUMBER                GBIBPGM 
02330      MOVE  GCT-PLAN-CODE    TO   GIG2-PLAN-CODE                   GBIBPGM 
02331      MOVE  GCT-GROUP-NUM    TO   GIG2-GROUP-NUM                   GBIBPGM 
02332      MOVE  GCT-SECTION-NUM  TO   GIG2-SECTION-NUM                 GBIBPGM 
02333      MOVE  GCT-PKG-CODE     TO   GIG2-PKG-CODE                    GBIBPGM 
02334      MOVE  IBSVDTO          TO   GIGT2-SLOT-NUMBER                GBIBPGM 
02335      MOVE  IBSUBO           TO   GI2-SUBSCRIBER.                  GBIBPGM 
02336      MOVE  IBRETCO          TO   GI2-RETURN-CODE                  GBIBPGM 
02337                                                                   GBIBPGM 
02338      EXEC CICS XCTL PROGRAM (WS-XCTL-TO-PGM)                      GBIBPGM 
02339                     COMMAREA(GI-COMMAREA2-RECORD)                 GBIBPGM 
02340                     LENGTH  (WS-COMM-KEY-LEN)                     GBIBPGM 
02341                     END-EXEC.                                     GBIBPGM 
02342                                                                   GBIBPGM 
02343                                                                   GBIBPGM 
02344  5000-900-EXIT.                                                   GBIBPGM 
02345      EXIT.                                                        GBIBPGM 
02346                                                                   GBIBPGM 
02347 /*****************************************************************GBIBPGM 
02348 **                                                              **GBIBPGM 
02349 **           X C T L   T O   M A I N   M E N U                  **GBIBPGM 
02350 **                                                              **GBIBPGM 
02351 ******************************************************************GBIBPGM 
02352  6000-000-XCTL-TO-MAIN-MENU     SECTION.                          GBIBPGM 
02353  6000-010.                                                        GBIBPGM 
02354      MOVE '6000'  TO  WS-PARA-ID.                                 GBIBPGM 
02355      MOVE '1AP1'  TO  WS-ABEND-CODE.                              GBIBPGM 
02356                                                                   GBIBPGM 
02357      EXEC CICS XCTL   PROGRAM('GHILPGM') END-EXEC.                GBIBPGM 
02358                                                                   GBIBPGM 
02359  6000-900-EXIT.                                                   GBIBPGM 
02360      EXIT.                                                        GBIBPGM 
02361 /*****************************************************************GBIBPGM 
02362 **                                                               *GBIBPGM 
02363 ** 7500   SEND DATAONLY                                          *GBIBPGM 
02364 **                                                               *GBIBPGM 
02365 ******************************************************************GBIBPGM 
02366  7500-000-SEND-DATAONLY         SECTION.                          GBIBPGM 
02367  7000-010.                                                        GBIBPGM 
02368                                                                   GBIBPGM 
02369      EXEC CICS SEND   MAP   ('GBIBI01')                           GBIBPGM 
02370                       MAPSET('GBIBSET')                           GBIBPGM 
02371                       DATAONLY                                    GBIBPGM 
02372                       FROM  (GBIBI01O)                            GBIBPGM 
02373                       CURSOR                                      GBIBPGM 
02374                       END-EXEC.                                   GBIBPGM 
02375                                                                   GBIBPGM 
02376  7500-900-EXIT.                                                   GBIBPGM 
02377      EXIT.                                                        GBIBPGM 
02378 /*****************************************************************GBIBPGM 
02379 **                                                               *GBIBPGM 
02380 ** 7600   SEND ERASE                                             *GBIBPGM 
02381 **                                                               *GBIBPGM 
02382 ******************************************************************GBIBPGM 
02383  7600-000-SEND-ERASE            SECTION.                          GBIBPGM 
02384  7600-010.                                                        GBIBPGM 
02385                                                                   GBIBPGM 
02386      EXEC CICS SEND   MAP   ('GBIBI01')                           GBIBPGM 
02387                       MAPSET('GBIBSET')                           GBIBPGM 
02388                       ERASE                                       GBIBPGM 
02389                       FROM  (GBIBI01O)                            GBIBPGM 
02390                       CURSOR                                      GBIBPGM 
02391                       END-EXEC.                                   GBIBPGM 
02392                                                                   GBIBPGM 
02393  7600-900-EXIT.                                                   GBIBPGM 
02394      EXIT.                                                        GBIBPGM 
02395 /*****************************************************************GBIBPGM 
02396 **                                                               *GBIBPGM 
02397 ** 8000   READ CONTRACT                                          *GBIBPGM 
02398 **                                                               *GBIBPGM 
02399 ******************************************************************GBIBPGM 
02400  8000-000-READ-CONTRACT         SECTION.                          GBIBPGM 
02401  8000-010.                                                        GBIBPGM 
02402                                                                   GBIBPGM 
02403      COMPUTE  WS-IO-PARM-CONTRACT-LEN      =                      GBIBPGM 
02404               GC-GCIOPARM-LEN              +                      GBIBPGM 
02405               GC-GCCONTR-FIXED-LEN         +                      GBIBPGM 
02406             ( GC-GCCONTR-VARY-MAX-OCUR     *                      GBIBPGM 
02407               GC-GCCONTR-VARY-LEN ).                              GBIBPGM 
02408                                                                   GBIBPGM 
02409      EXEC CICS GETMAIN  SET (ADDRESS OF IO-PARM-CONTRACT-RECORD)  GBIBPGM 
02410                         INITIMG(WS-HEX-00)                        GBIBPGM 
02411                         LENGTH (WS-IO-PARM-CONTRACT-LEN)          GBIBPGM 
02412                         END-EXEC.                                 GBIBPGM 
02413      SET GI2-RECORD-POINTER TO ADDRESS OF IO-PARM-CONTRACT-RECORD.GBIBPGM 
02414                                                                   GBIBPGM 
02415      MOVE GC-GCCONTR-VARY-MAX-OCUR                                GBIBPGM 
02416                                   TO GCT-COUNT-BEN-PROVN-POINTERS.GBIBPGM 
02417      MOVE 'GCCONTR '              TO GCIO-FILE-DDNAME.            GBIBPGM 
02418      MOVE '1'                     TO GCIO-IO-AREA-TO-USE.         GBIBPGM 
02419      MOVE WS-CONTRACT-ID          TO GIC2-CONTRACT-ID             GBIBPGM 
02420                                      GCIO-CONTRACT-FILE-KEY.      GBIBPGM 
02421      MOVE GCIO-CONTRACT-FILE-KEY  TO GCIO-FILE-KEY.               GBIBPGM 
02422      MOVE 'RD '                   TO GCIO-FILE-ACCESS-CODE.       GBIBPGM 
02423                                                                   GBIBPGM 
02424      EXEC CICS LINK   PROGRAM ('GCIOPGM')                         GBIBPGM 
02425                       COMMAREA(IO-PARM-CONTRACT-RECORD)           GBIBPGM 
02426                       LENGTH  (WS-IO-PARM-CONTRACT-LEN)           GBIBPGM 
02427                       END-EXEC.                                   GBIBPGM 
02428                                                                   GBIBPGM 
02429  8000-900-EXIT.                                                   GBIBPGM 
02430      EXIT.                                                        GBIBPGM 
02431 /*****************************************************************GBIBPGM 
02432 **                                                               *GBIBPGM 
02433 ** 8100   READ GCDATES                                           *GBIBPGM 
02434 **                                                               *GBIBPGM 
02435 ******************************************************************GBIBPGM 
02436  8100-000-READ-GCDATES          SECTION.                          GBIBPGM 
02437  8100-010.                                                        GBIBPGM 
02438                                                                   GBIBPGM 
02439      COMPUTE  WS-IO-PARM-GCDATES-LEN       =                      GBIBPGM 
02440               GC-GCIOPARM-LEN              +                      GBIBPGM 
02441               GC-GCDATES-FIXED-LEN         +                      GBIBPGM 
02442             ( GC-GCDATES-VARY-MAX-OCUR     *                      GBIBPGM 
02443               GC-GCDATES-VARY-LEN ).                              GBIBPGM 
02444                                                                   GBIBPGM 
02445      EXEC CICS GETMAIN  SET (ADDRESS OF IO-PARM-GCDATES-RECORD)   GBIBPGM 
02446                         INITIMG(WS-HEX-00)                        GBIBPGM 
02447                         LENGTH (WS-IO-PARM-GCDATES-LEN)           GBIBPGM 
02448                         END-EXEC.                                 GBIBPGM 
02449                                                                   GBIBPGM 
02450      MOVE GC-GCDATES-VARY-MAX-OCUR                                GBIBPGM 
02451                                   TO DTE-ENTRY-COUNT.             GBIBPGM 
02452      MOVE 'GCDATES '              TO GCIO2-FILE-DDNAME.           GBIBPGM 
02453      MOVE '1'                     TO GCIO2-IO-AREA-TO-USE.        GBIBPGM 
02454      MOVE WS-GCDATES-KEY          TO GCIO2-FILE-KEY.              GBIBPGM 
02455      MOVE 'RD '                   TO GCIO2-FILE-ACCESS-CODE.      GBIBPGM 
02456                                                                   GBIBPGM 
02457      EXEC CICS LINK   PROGRAM ('GCIOPGM')                         GBIBPGM 
02458                       COMMAREA(IO-PARM-GCDATES-RECORD)            GBIBPGM 
02459                       LENGTH  (WS-IO-PARM-GCDATES-LEN)            GBIBPGM 
02460                       END-EXEC.                                   GBIBPGM 
02461                                                                   GBIBPGM 
02462  8100-900-EXIT.                                                   GBIBPGM 
02463      EXIT.                                                        GBIBPGM 
02464 /*****************************************************************GBIBPGM 
02465 **                                                               *GBIBPGM 
02466 ** 8200        GROUP SPECIFIC START BROWSE                       *GBIBPGM 
02467 **                                                               *GBIBPGM 
02468 ******************************************************************GBIBPGM 
02469  8200-000-STBR-GROUPSPC         SECTION.                          GBIBPGM 
02470  8200-010.                                                        GBIBPGM 
02471                                                                   GBIBPGM 
02472      MOVE  LOW-VALUES      TO  WS-ALT-WORKFILE-KEYS               GBIBPGM 
02473                                                                   GBIBPGM 
02474                                                                   GBIBPGM 
02475      COMPUTE  WS-IO-PARM-GROUPSPC-LEN      =                      GBIBPGM 
02476               GC-GCIOPARM-LEN              +                      GBIBPGM 
02477               GC-GCGRPSPC-MAX-REC-LEN.                            GBIBPGM 
02478                                                                   GBIBPGM 
02479      EXEC CICS GETMAIN  SET (ADDRESS OF IO-PARM-GROUPSPC-RECORD)  GBIBPGM 
02480                         INITIMG(WS-HEX-00)                        GBIBPGM 
02481                         LENGTH (WS-IO-PARM-GROUPSPC-LEN)          GBIBPGM 
02482                         END-EXEC.                                 GBIBPGM 
02483 **** SET GI2-RECORD-POINTER TO ADDRESS OF IO-PARM-CONTRACT-RECORD.GBIBPGM 
02484                                                                   GBIBPGM 
02485      MOVE GC-GCGRPSPC-VARY-MAX-OCUR                               GBIBPGM 
02486                                   TO GCG-COUNT-TAB-PROVN-POINTERS.GBIBPGM 
02487      MOVE 'GCGRPSPC'              TO GCIO3-FILE-DDNAME.           GBIBPGM 
02488      MOVE '1'                     TO GCIO3-IO-AREA-TO-USE.        GBIBPGM 
02489      MOVE WS-GROUPSPC-KEY         TO GIG2-GROUP-SPECIFIC-ID       GBIBPGM 
02490                                      GCIO-GROUP-SPECIFIC.         GBIBPGM 
02491      MOVE GCIO-GROUP-SPECIFIC     TO GCIO3-FILE-KEY.              GBIBPGM 
02492      MOVE 'SBO'                   TO GCIO3-FILE-ACCESS-CODE.      GBIBPGM 
02493      MOVE 'GTE'                   TO GCIO3-BROWSE-QUAL-CODE.      GBIBPGM 
02494                                                                   GBIBPGM 
02495      EXEC CICS LINK   PROGRAM ('GCIOPGM')                         GBIBPGM 
02496                       COMMAREA(IO-PARM-GROUPSPC-RECORD)           GBIBPGM 
02497                       LENGTH  (WS-IO-PARM-GROUPSPC-LEN)           GBIBPGM 
02498                       END-EXEC.                                   GBIBPGM 
02499                                                                   GBIBPGM 
02500  8200-900-EXIT.                                                   GBIBPGM 
02501      EXIT.                                                        GBIBPGM 
02502 /*****************************************************************GBIBPGM 
02503 **                                                               *GBIBPGM 
02504 ** 8300        GROUP SPECIFIC READ NEXT RECORD                   *GBIBPGM 
02505 **                                                               *GBIBPGM 
02506 ******************************************************************GBIBPGM 
02507  8300-000-RDNX-GROUPSPC         SECTION.                          GBIBPGM 
02508  8300-010.                                                        GBIBPGM 
02509                                                                   GBIBPGM 
02510                                                                   GBIBPGM 
02511      MOVE 'RN '                   TO GCIO3-FILE-ACCESS-CODE.      GBIBPGM 
02512      MOVE GCIO-GROUP-SPECIFIC     TO GCIO3-FILE-KEY.              GBIBPGM 
02513                                                                   GBIBPGM 
02514      EXEC CICS LINK   PROGRAM ('GCIOPGM')                         GBIBPGM 
02515                       COMMAREA(IO-PARM-GROUPSPC-RECORD)           GBIBPGM 
02516                       LENGTH  (WS-IO-PARM-GROUPSPC-LEN)           GBIBPGM 
02517                       END-EXEC.                                   GBIBPGM 
02518                                                                   GBIBPGM 
02519  8300-900-EXIT.                                                   GBIBPGM 
02520      EXIT.                                                        GBIBPGM 
02521 /*****************************************************************GBIBPGM 
02522 *                                                                *GBIBPGM 
02523 * 9200    G R E G O R I A N   T O   J U L I A N                  *GBIBPGM 
02524 *                                                                *GBIBPGM 
02525 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *GBIBPGM 
02526 *                                                                *GBIBPGM 
02527 ******************************************************************GBIBPGM 
02528  9200-000-GREGORIAN-TO-JULIAN   SECTION.                          GBIBPGM 
02529  9200-010.                                                        GBIBPGM 
02530                                                                   GBIBPGM 
02531      MOVE 'CNV' TO  HGADATE-FUNC.                                 GBIBPGM 
02532      MOVE 'M'   TO  HGADATE-FORM1.                                GBIBPGM 
02533      MOVE 'J'   TO  HGADATE-FORM2.                                GBIBPGM 
02534      MOVE ZEROS TO  HGADATE-RETURN                                GBIBPGM 
02535                     HGADATE-AMOUNT.                               GBIBPGM 
02536      EXEC CICS LINK PROGRAM ('HGADATES')                          GBIBPGM 
02537                     COMMAREA(HGADATES-COMMAREA)                   GBIBPGM 
02538                     LENGTH  (24)                                  GBIBPGM 
02539                     END-EXEC.                                     GBIBPGM 
02540                                                                   GBIBPGM 
02541  9200-900-EXIT.                                                   GBIBPGM 
02542      EXIT.                                                        GBIBPGM 
02543 /*****************************************************************GBIBPGM 
02544 *                                                                *GBIBPGM 
02545 * 9210    J U L I A N    T O    G R E G O R I A N                *GBIBPGM 
02546 *                                                                *GBIBPGM 
02547 *   CONVERT JULIAN DATE (YYDDD) TO GREGORIAN (MMDDYY) FORMAT.    *GBIBPGM 
02548 *                                                                *GBIBPGM 
02549 ******************************************************************GBIBPGM 
02550  9210-000-JULIAN-TO-GREGORIAN   SECTION.                          GBIBPGM 
02551  9210-010.                                                        GBIBPGM 
02552                                                                   GBIBPGM 
02553      MOVE 'CNV' TO  HGADATE-FUNC.                                 GBIBPGM 
02554      MOVE 'J'   TO  HGADATE-FORM1.                                GBIBPGM 
02555      MOVE 'M'   TO  HGADATE-FORM2.                                GBIBPGM 
02556      MOVE ZEROS TO  HGADATE-RETURN                                GBIBPGM 
02557                     HGADATE-AMOUNT.                               GBIBPGM 
02558      EXEC CICS LINK PROGRAM ('HGADATES')                          GBIBPGM 
02559                     COMMAREA(HGADATES-COMMAREA)                   GBIBPGM 
02560                     LENGTH  (24)                                  GBIBPGM 
02561                     END-EXEC.                                     GBIBPGM 
02562                                                                   GBIBPGM 
02563  9210-900-900-EXIT.                                               GBIBPGM 
02564      EXIT.                                                        GBIBPGM 
02565                                                                   GBIBPGM 
02566  9215-000-JULIAN-GREG-CEN   SECTION.                              GBIBPGM 
02567  9215-010.                                                        GBIBPGM 
02568                                                                   GBIBPGM 
02569      MOVE 'CNV' TO  MLDATE-FUNC.                                  GBIBPGM 
02570      MOVE 'J'   TO  MLDATE-FORM1.                                 GBIBPGM 
02571      MOVE 'M'   TO  MLDATE-FORM2.                                 GBIBPGM 
02572      MOVE ZEROS TO  MLDATE-RETURN                                 GBIBPGM 
02573                     MLDATE-AMOUNT.                                GBIBPGM 
02574      EXEC CICS LINK PROGRAM ('MLDATEC')                           GBIBPGM 
02575                     COMMAREA(MLDATE01)                            GBIBPGM 
02576                     LENGTH  (28)                                  GBIBPGM 
02577                     END-EXEC.                                     GBIBPGM 
02578                                                                   GBIBPGM 
02579  9215-900-EXIT.                                                   GBIBPGM 
02580      EXIT.                                                        GBIBPGM 
02581                                                                   GBIBPGM 
02582  9220-000-GREG-JULIAN-CEN   SECTION.                              GBIBPGM 
02583  9220-010.                                                        GBIBPGM 
02584                                                                   GBIBPGM 
02585      MOVE 'CNV' TO  MLDATE-FUNC.                                  GBIBPGM 
02586      MOVE 'M'   TO  MLDATE-FORM1.                                 GBIBPGM 
02587      MOVE 'J'   TO  MLDATE-FORM2.                                 GBIBPGM 
02588      MOVE ZEROS TO  MLDATE-RETURN                                 GBIBPGM 
02589                     MLDATE-AMOUNT.                                GBIBPGM 
02590      EXEC CICS LINK PROGRAM ('MLDATEC')                           GBIBPGM 
02591                     COMMAREA(MLDATE01)                            GBIBPGM 
02592                     LENGTH  (28)                                  GBIBPGM 
02593                     END-EXEC.                                     GBIBPGM 
02594                                                                   GBIBPGM 
02595  9220-900-900-EXIT.                                               GBIBPGM 
02596               EXIT.                                               GBIBPGM 
02597 /                                                                 GBIBPGM 
02598  9300-000-LOAD-GHIL-KEY  SECTION.                                 GBIBPGM 
02599  9300-010.                                                        GBIBPGM 
02600      MOVE GIC-PLAN-CODE      TO  IBPLNXO                          GBIBPGM 
02601      MOVE GIC-GROUP-NUM      TO  IBGRPXO                          GBIBPGM 
02602      MOVE GIC-SECTION-NUM    TO  IBSECXO                          GBIBPGM 
02603      MOVE GIC-PKG-CODE       TO  IBPKGXO                          GBIBPGM 
02604      MOVE GIC-L-O-B          TO  IBLOBXO                          GBIBPGM 
02605      MOVE GIC-PROV-CTL       TO  IBPRVXO                          GBIBPGM 
02606      MOVE GIC-FAM-REL-LVL    TO  IBFRXO                           GBIBPGM 
02607      MOVE GI-EFFECTIVE-DATE  TO  IBEDTXO.                         GBIBPGM 
02608           MOVE GI-RETURN-CODE TO IBRETCO                          GBIBPGM 
02609      IF   GI-SUBSCRIBER  NOT = LOW-VALUES                         GBIBPGM 
02610           MOVE GI-SUBSCRIBER TO IBSUBO                            GBIBPGM 
02611           MOVE GIGT-SLOT-NUMBER TO IBSVDTO                        GBIBPGM 
02612      END-IF                                                       GBIBPGM 
02613                                                                   GBIBPGM 
02614      IF  (GIC-PLAN-CODE NOT  = LOW-VALUES) AND                    GBIBPGM 
02615          (GIC-PLAN-CODE NOT  = SPACES)                            GBIBPGM 
02616           MOVE +3            TO  IBPLNXL                          GBIBPGM 
02617      END-IF                                                       GBIBPGM 
02618                                                                   GBIBPGM 
02619      IF  (GIC-GROUP-NUM NOT  = LOW-VALUES) AND                    GBIBPGM 
02620          (GIC-GROUP-NUM NOT  = SPACES)                            GBIBPGM 
02621           MOVE +9            TO  IBGRPXL                          GBIBPGM 
02622      END-IF                                                       GBIBPGM 
02623                                                                   GBIBPGM 
02624      IF  (GIC-SECTION-NUM NOT   = LOW-VALUES) AND                 GBIBPGM 
02625          (GIC-SECTION-NUM NOT   = SPACES)                         GBIBPGM 
02626           MOVE +5            TO  IBSECXL                          GBIBPGM 
02627      END-IF                                                       GBIBPGM 
02628                                                                   GBIBPGM 
02629      IF  (GIC-PKG-CODE  NOT  = LOW-VALUES) AND                    GBIBPGM 
02630          (GIC-PKG-CODE  NOT  = SPACES)                            GBIBPGM 
02631           MOVE +3            TO  IBPKGXL                          GBIBPGM 
02632      END-IF                                                       GBIBPGM 
02633                                                                   GBIBPGM 
02634      IF  (GIC-L-O-B     NOT  = LOW-VALUES) AND                    GBIBPGM 
02635          (GIC-L-O-B     NOT  = SPACES)                            GBIBPGM 
02636           MOVE +1            TO  IBLOBXL                          GBIBPGM 
02637      END-IF                                                       GBIBPGM 
02638                                                                   GBIBPGM 
02639      IF  (GIC-PROV-CTL  NOT  = LOW-VALUES) AND                    GBIBPGM 
02640          (GIC-PROV-CTL  NOT  = SPACES)                            GBIBPGM 
02641           MOVE +2            TO  IBPRVXL                          GBIBPGM 
02642      END-IF                                                       GBIBPGM 
02643                                                                   GBIBPGM 
02644      IF  (GIC-FAM-REL-LVL NOT   = LOW-VALUES) AND                 GBIBPGM 
02645          (GIC-FAM-REL-LVL NOT   = SPACES)                         GBIBPGM 
02646           MOVE +2            TO  IBFRXL                           GBIBPGM 
02647      END-IF                                                       GBIBPGM 
02648                                                                   GBIBPGM 
02649      IF  (GI-EFFECTIVE-DATE NOT =   LOW-VALUES) AND               GBIBPGM 
02650          (GI-EFFECTIVE-DATE NOT =   SPACES)                       GBIBPGM 
02651           MOVE +6            TO  IBEDTXL                          GBIBPGM 
02652      END-IF.                                                      GBIBPGM 
02653                                                                   GBIBPGM 
02654  9300-900-EXIT.                                                   GBIBPGM 
02655           EXIT.                                                   GBIBPGM 
02656                                                                   GBIBPGM 
02657                                                                   GBIBPGM 
02658  9400-000-RECEIVE-SCR  SECTION.                                   GBIBPGM 
02659                                                                   GBIBPGM 
02660 *        +----------------------------------------+               GBIBPGM 
02661 *        +  RECEIVE THE CURRENT SCREEN            +               GBIBPGM 
02662 *        +----------------------------------------+               GBIBPGM 
02663                                                                   GBIBPGM 
02664      EXEC CICS RECEIVE   MAP   ('GBIBI01')                        GBIBPGM 
02665                          MAPSET('GBIBSET')                        GBIBPGM 
02666                          INTO  (GBIBI01I)                         GBIBPGM 
02667                          END-EXEC.                                GBIBPGM 
02668                                                                   GBIBPGM 
02669                                                                   GBIBPGM 
02670  9400-900-EXIT.                                                   GBIBPGM 
02671           EXIT.                                                   GBIBPGM 
02672                                                                   GBIBPGM 
02673                                                                   GBIBPGM 
02674                                                                   GBIBPGM 
02675  9900-000-ERROR-MSG-THEN-ABEND  SECTION.                          GBIBPGM 
02676                                                                   GBIBPGM 
02677      MOVE -1  TO  IBGRPXL.                                        GBIBPGM 
02678      EXEC CICS SEND   MAP   ('GBIBI01')                           GBIBPGM 
02679                       MAPSET('GBIBSET')                           GBIBPGM 
02680                       ERASE                                       GBIBPGM 
02681                       FROM  (GBIBI01O)                            GBIBPGM 
02682                       CURSOR                                      GBIBPGM 
02683                       WAIT                                        GBIBPGM 
02684                       END-EXEC.                                   GBIBPGM 
02685                                                                   GBIBPGM 
02686      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GBIBPGM 
02687                                                                   GBIBPGM 
02688  9900-900-EXIT.                                                   GBIBPGM 
02689      EXIT.                                                        GBIBPGM 
