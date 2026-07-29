00001  ID DIVISION.                                                     08/20/03
00002  PROGRAM-ID.     GA1FPGM.                                         GA1FPGM 
00003 *** THIS IS A COBOL II PROGRAM                                       LV001
00004  AUTHOR.         S BUCH.                                          GA1FPGM 
00005  DATE-WRITTEN.   11/16/84.                                        GA1FPGM 
00006  DATE-COMPILED.                                                   GA1FPGM 
00007      SKIP3                                                        GA1FPGM 
00008 ******************************************************************GA1FPGM 
00009 *                         REVISIONS                               GA1FPGM 
00010 ******************************************************************GA1FPGM 
00011 *   DATE    BY   DESCRIPTION                                      GA1FPGM 
00012 * --------  ---  -------------------------------------------------GA1FPGM 
00013 * 01/16/86  ENW  CHANGED WS-CONTRACT-FIXED-PORTION FROM 483 TO 563GA1FPGM 
00014 *                        WS-CONTRACT-VARIABLE-LEN  FROM  12 TO  10GA1FPGM 
00015 *                        WS-CONTRACT-MAX-OCCURS    FROM 450 TO 520GA1FPGM 
00016 *                                                                 GA1FPGM 
00017 * 08/14/86  JLA  1. ADD PAGING SUPPORT FOR PF7,8,10 AND 11.       GA1FPGM 
00018 *   (D137)       2. ADD BENEFIT CODE SELECTION FIELD.             GA1FPGM 
00019 *                3. ADD LOCATION COUNTERS(I.E 1 TO 36             GA1FPGM 
00020 *                   OF 54 BENEFIT CODES DISPLAYED) TO             GA1FPGM 
00021 *                   SCREEN.                                       GA1FPGM 
00022 *                4. USE USER DEFINED LOGICAL MAP FOR              GA1FPGM 
00023 *                   SCREEN.  THIS REPLACES THE PARTIAL            GA1FPGM 
00024 *                   USE OF BMS MAP AND USER DEFINED.              GA1FPGM 
00025 *                                                                 GA1FPGM 
00026 * 01/29/87  JLA  CHANGES FOR SINGLE TABULAR SUPPORT THAT ARE      GA1FPGM 
00027 *  (D0120)        EXECUTED FROM TRANSACTION GTM1:                 GA1FPGM 
00028 *                 1. PF1/PF13 - CONSTRUCT COMMAREA AS IF GC4A     GA1FPGM 
00029 *                    HAD CALLED, XCTL TO ADD SCREEN PROGRAM.      GA1FPGM 
00030 *                 2. PF3/PF15 - CONSTRUCT COMMAREA AS IF GC4A     GA1FPGM 
00031 *                    HAD CALLED, XCTL TO GTM1PGM.                 GA1FPGM 
00032 *                                                                *GA1FPGM 
00033 * 08/17/87  FRY   CAPTURE OPERATOR-ID WHEN A 'C3', 'C5', OR 'G3' *GA1FPGM 
00034 *   (D116)        RECORD IS UPDATED.                             *GA1FPGM 
00035 *                                                                *GA1FPGM 
00036 *                                                                *GA1FPGM 
00037 *  11161  11/17/90  ENW   CHANGED PROGRAM TO BRING IN COPYBOOK   *GA1FPGM 
00038 *                         GCCDRLEN.                               GA1FPGM 
00039 *                         REMOVED HARD CODED LENGTHS.             GA1FPGM 
00040 *                         REMOVED PF12/24 HARDCOPY ROUTINES.      GA1FPGM 
00041 *                                                                *GA1FPGM 
00042 *  D12009 09/26/91  GDM   CONVERT TO COBOL II                    *GA1FPGM 
00043 *                                                                *GA1FPGM 
00044 *  D12009 08/28/91  TPM   INCREASED THE FAMILY RELATION  FIELD   *GA1FPGM 
00045 *                         FROM ONE POSITION TO TWO POSITIONS.    *GA1FPGM 
00046 *                                                                *GA1FPGM 
00047 *  14726/ 10/15/97  DAU   ADDED CODE TO SUPPORT THE YEAR 2000    *GA1FPGM 
00048 *  15057                  AND THE EXPANSION OF THE GROUP SPECIFIC*GA1FPGM 
00049 *                         AND CONTRACT KEY TO SUPPORT THE TEXAS  *GA1FPGM 
00050 *                         MERGER.                                *GA1FPGM 
00051 *                                                                *GA1FPGM 
00052 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GA1FPGM 
00053 *                                                                *GA1FPGM 
00054 ******************************************************************GA1FPGM 
00055 ******************************************************************GA1FPGM 
00056 ******************************************************************GA1FPGM 
00057 *   GA1FPGM     ALL LEVEL TABULAR PROVISION MAINTENANCE PROGRAM   GA1FPGM 
00058 *                 ANCILLARY RELATIONSHIP BENEFIT CODES - GA1F     GA1FPGM 
00059 *                                                                 GA1FPGM 
00060 *     THIS PROGRAM WILL PERFORM DELETE MAINTENANCE ON ALL ENTRIES GA1FPGM 
00061 *   CURRENTLY ON THE ALL LEVEL TABULAR RECORD.                    GA1FPGM 
00062 *                                                                 GA1FPGM 
00063 *     THE DELETE SCREEN WILL DISPLAY ALL ENTRIES CURRENTLY ON THE GA1FPGM 
00064 *   ALL LEVEL TABULAR RECORD.  THE OPERATOR WILL THEN DECIDE IF   GA1FPGM 
00065 *   ANY ENTRIES WILL BE DELETED.  THE SCREEN ENTRY WILL BE        GA1FPGM 
00066 *   VALIDATED AND A COPY OF THE ENTRIES FROM THE RECORD WILL BE   GA1FPGM 
00067 *   MADE.  ANY MATCHED ENTRIES WILL NOT BE MOVED BACK INTO THE    GA1FPGM 
00068 *   RECORD BEFORE UPDATING THE RECORD.                            GA1FPGM 
00069 *                                                                 GA1FPGM 
00070 *     TO EXECUTE THE ADD PORTION FOR THIS SET OF DATA (ID: #AAR)  GA1FPGM 
00071 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA1FPGM 
00072 *   XCTL TO TRANS GA2F OR PROGRAM GA2FPGM.  THIS PROGRAM WILL     GA1FPGM 
00073 *   VALIDATE ALL FIELDS AND THEN SEQUENCE ALL ENTRIES IN THE      GA1FPGM 
00074 *   TABLE.                                                        GA1FPGM 
00075 *                                                                 GA1FPGM 
00076 *   PF7/PF19  PAGE BACKWARD.                                      GA1FPGM 
00077 *   PF8/PF20  PAGE FORWARD.                                       GA1FPGM 
00078 *   PF10/PF22 PAGE TO BOTTOM.                                     GA1FPGM 
00079 *   PF11/PF23 PAGE TO TOP.                                        GA1FPGM 
00080 *                                                                 GA1FPGM 
00081 *                                                                 GA1FPGM 
00082 *   FUNC CODE: GA1F                                               GA1FPGM 
00083 *   MAPSET:    GA1FSETC  <<<< REDEFINED BY USER DEFINED MAP >>>>  GA1FPGM 
00084 *   FILES:     GCPSWORK                                           GA1FPGM 
00085 *                                                                 GA1FPGM 
00086 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00087 *                                                                 GA1FPGM 
00088 *    TAILORING INSTRUCTIONS:                                      GA1FPGM 
00089 *                                                                 GA1FPGM 
00090 *              EDIT THE FOLLOWING CHARACTER STRINGS               GA1FPGM 
00091 *                                                                 GA1FPGM 
00092 *              PROGRAM FUNCTION CODE          EX. /GC9I/GA1F/     GA1FPGM 
00093 *              SCREEN PAGE NUMBER                 /009I/001F/     GA1FPGM 
00094 *              ADD PROGRAM FUNCTION CODE          /GA9I/GA2F/     GA1FPGM 
00095 *              BENEFIT PROVISION TABULAR ID       /#PPF/#AAR/     GA1FPGM 
00096 *              RDW PREFIX FOR TABULAR RECORD      /GBB/GAE/       GA1FPGM 
00097 *                                                                 GA1FPGM 
00098 *     ALL AREAS BETWEEN LINES OF +++++++ MUST BE CHANGED TO       GA1FPGM 
00099 *     MATCH THE ACTUAL TABULAR RECORD FIELDS OR COUNT OF SCREEN   GA1FPGM 
00100 *     OCCURANCES.                                                 GA1FPGM 
00101 *                                                                 GA1FPGM 
00102 *     YOU CAN SCAN FOR /**+**/ TO FIND ALL AREAS IN THIS PROGRAM  GA1FPGM 
00103 *     THAT MUST BE CHANGED.                                       GA1FPGM 
00104 *                                                                 GA1FPGM 
00105 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00106      EJECT                                                        GA1FPGM 
00107  ENVIRONMENT DIVISION.                                            GA1FPGM 
00108      EJECT                                                        GA1FPGM 
00109  DATA DIVISION.                                                   GA1FPGM 
00110  WORKING-STORAGE SECTION.                                         GA1FPGM 
00111  01  WS-BEGIN                    PIC X(24)  VALUE                 GA1FPGM 
00112      '***GA1FPGM WS BEGINS***'.                                   GA1FPGM 
00113  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GA1FPGM 
00114                                                                   GA1FPGM 
00115  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GA1FPGM 
00116                                                                   GA1FPGM 
00117  01  WS-ONE-LOW                  PIC X(01) VALUE LOW-VALUES.      GA1FPGM 
00118                                                                   GA1FPGM 
00119  01  COMMAREA-POINTER-AREA.                                       GA1FPGM 
00120      05  COMMAREA-PNTR-COMP      PIC S9(8) COMP.                  GA1FPGM 
00121      05  COMMAREA-PNTR           REDEFINES                        GA1FPGM 
00122          COMMAREA-PNTR-COMP      USAGE IS POINTER.                GA1FPGM 
00123                                                                   GA1FPGM 
00124  01  WS-MISC.                                                     GA1FPGM 
00125      05  WS-SELECT-FROM          PIC S9(4) COMP VALUE +0.         GA1FPGM 
00126      05  WS-SELECT-TO            PIC S9(4) COMP VALUE +0.         GA1FPGM 
00127      05  WS-SELECT-OF            PIC S9(4) COMP VALUE +0.         GA1FPGM 
00128      05  WS-SELECT-FROM-MASK     PIC ZZ9.                         GA1FPGM 
00129      05  WS-SELECT-TO-MASK       PIC ZZ9.                         GA1FPGM 
00130      05  WS-SELECT-OF-MASK       PIC ZZ9.                         GA1FPGM 
00131      05  WS-GAE-INDEX            PIC S9(4) COMP VALUE +0.         GA1FPGM 
00132                                                                   GA1FPGM 
00133 ** MAP COBOL SCREEN DSECTS **                                     GA1FPGM 
00134  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA1FPGM 
00135      '***  I/O MAPAREA ***'.                                      GA1FPGM 
00136  COPY GA1FSETC.                                                   GA1FPGM 
00137 /*****************************************************************GA1FPGM 
00138 ******************************************************************GA1FPGM 
00139 ******************************************************************GA1FPGM 
00140 **                                                              **GA1FPGM 
00141 **    THIS IS A USER DEFINED LOGICAL MAP.  ANY CHANGES TO       **GA1FPGM 
00142 **     MAPSET GA1FSETC AFFECTING IT\
00143 **     FOR HERE.                                                **GA1FPGM 
00144 **                                            JLA 8/14/86       **GA1FPGM 
00145 **                                                              **GA1FPGM 
00146 ******************************************************************GA1FPGM 
00147 ******************************************************************GA1FPGM 
00148 ******************************************************************GA1FPGM 
00149                                                                   GA1FPGM 
00150  01  MAP-USER-DEFINED     REDEFINES   GA1FI01I.                   GA1FPGM 
00151                                                                   GA1FPGM 
00152      05  MAP-STORAGE-ACCOUNTING           PIC X(12).              GA1FPGM 
00153                                                                   GA1FPGM 
00154      05  MAP-FUNCTION-CODE-LEN            PIC S9(4) COMP SYNC.    GA1FPGM 
00155      05  MAP-FUNCTION-CODE-ATTR           PIC X.                  GA1FPGM 
00156      05  MAP-FUNCTION-CODE                PIC X(04).              GA1FPGM 
00157                                                                   GA1FPGM 
00158      05  MAP-TITLE-LINE-LEN               PIC S9(4) COMP SYNC.    GA1FPGM 
00159      05  MAP-TITLE-LINE-ATTR              PIC X.                  GA1FPGM 
00160      05  MAP-TITLE-LINE                   PIC X(47).              GA1FPGM 
00161                                                                   GA1FPGM 
00162      05  MAP-SCREEN-ID-LEN                PIC S9(4) COMP SYNC.    GA1FPGM 
00163      05  MAP-SCREEN-ID-ATTR               PIC X.                  GA1FPGM 
00164      05  MAP-SCREEN-ID                    PIC X(06).              GA1FPGM 
00165                                                                   GA1FPGM 
00166      05  MAP-ID-LINE-LEN                  PIC S9(4) COMP SYNC.    GA1FPGM 
00167      05  MAP-ID-LINE-ATTR                 PIC X.                  GA1FPGM 
00168      05  MAP-ID-LINE                      PIC X(79).              GA1FPGM 
00169      05  GROUP-SPECIFIC-ID-LINE REDEFINES MAP-ID-LINE.            GA1FPGM 
00170          10  GRP-SPEC-ID-HEADING              PIC X(20).          GA1FPGM 
00171          10  GRP-SPEC-GROUP-HEADING           PIC X(5).           GA1FPGM 
00172          10  GRP-SPEC-GROUP-NO                PIC X(6).           GA1FPGM 
00173          10  GRP-SPEC-SECTION-HEADING         PIC X(6).           GA1FPGM 
00174          10  GRP-SPEC-SECTION-NO              PIC X(4).           GA1FPGM 
00175          10  GRP-SPEC-FAM-REL-HEADING         PIC X(5).           GA1FPGM 
00176          10  GRP-SPEC-FAM-REL-LVL             PIC XX.             GA1FPGM 
00177          10  GRP-SPEC-EFF-DT-HEADING          PIC X(7).           GA1FPGM 
00178          10  GRP-SPEC-EFF-DATE                PIC X(6).           GA1FPGM 
00179          10  FILLER                           PIC X(18).          GA1FPGM 
00180      05  CONTRACT-ID-LINE  REDEFINES  MAP-ID-LINE.                GA1FPGM 
00181          10  CONTRACT-ID-HEADING              PIC X(14).          GA1FPGM 
00182          10  CONTRACT-GROUP-HEADING           PIC X(5).           GA1FPGM 
00183          10  CONTRACT-GROUP-NO                PIC X(6).           GA1FPGM 
00184          10  CONTRACT-SECTION-HEADING         PIC X(6).           GA1FPGM 
00185          10  CONTRACT-SECTION-NO              PIC X(4).           GA1FPGM 
00186          10  CONTRACT-LOB-HEADING             PIC X(6).           GA1FPGM 
00187          10  CONTRACT-LOB                     PIC X.              GA1FPGM 
00188          10  CONTRACT-PROV-CTL-HEADING        PIC X(6).           GA1FPGM 
00189          10  CONTRACT-PROV-CTL                PIC XX.             GA1FPGM 
00190          10  CONTRACT-FAM-REL-HEADING         PIC X(5).           GA1FPGM 
00191          10  CONTRACT-FAM-REL-LVL             PIC XX.             GA1FPGM 
00192          10  CONTRACT-EFF-DT-HEADING          PIC X(7).           GA1FPGM 
00193          10  CONTRACT-EFF-DATE                PIC X(6).           GA1FPGM 
00194          10  FILLER                           PIC X(09).          GA1FPGM 
00195      05  BENEFIT-PROVISION-ID-LINE  REDEFINES  MAP-ID-LINE.       GA1FPGM 
00196          10  BEN-PROV-GROUP-HEADING           PIC X(5).           GA1FPGM 
00197          10  BEN-PROV-GROUP-NO                PIC X(6).           GA1FPGM 
00198          10  BEN-PROV-SECTION-HEADING         PIC X(6).           GA1FPGM 
00199          10  BEN-PROV-SECTION-NO              PIC X(4).           GA1FPGM 
00200          10  BEN-PROV-LOB-HEADING             PIC X(6).           GA1FPGM 
00201          10  BEN-PROV-LOB                     PIC X.              GA1FPGM 
00202          10  BEN-PROV-PROV-CTL-HEADING        PIC X(6).           GA1FPGM 
00203          10  BEN-PROV-PROV-CTL                PIC XX.             GA1FPGM 
00204          10  BEN-PROV-FAM-REL-HEADING         PIC X(5).           GA1FPGM 
00205          10  BEN-PROV-FAM-REL-LVL             PIC XX.             GA1FPGM 
00206          10  BEN-PROV-EFF-DT-HEADING          PIC X(7).           GA1FPGM 
00207          10  BEN-PROV-EFF-DATE                PIC X(6).           GA1FPGM 
00208          10  BEN-PROV-ID-HEADING              PIC X(8).           GA1FPGM 
00209          10  BEN-PROV-ID-NO                   PIC X(6).           GA1FPGM 
00210          10  FILLER                           PIC X(09).          GA1FPGM 
00211                                                                   GA1FPGM 
00212      05  MAP-ALL-LEVEL-TAB-ID-LEN         PIC S9(4) COMP SYNC.    GA1FPGM 
00213      05  MAP-ALL-LEVEL-TAB-ID-ATTR        PIC X.                  GA1FPGM 
00214      05  MAP-ALL-LEVEL-TAB-ID             PIC X(06).              GA1FPGM 
00215                                                                   GA1FPGM 
00216      05  MAP-ALL-LEVEL-TAB-SLOT-LEN       PIC S9(4) COMP SYNC.    GA1FPGM 
00217      05  MAP-ALL-LEVEL-TAB-SLOT-ATTR      PIC X.                  GA1FPGM 
00218      05  MAP-ALL-LEVEL-TAB-SLOT           PIC X(07).              GA1FPGM 
00219                                                                   GA1FPGM 
00220      05  MAP-FROM-MENU-ID-LEN             PIC S9(4) COMP SYNC.    GA1FPGM 
00221      05  MAP-FROM-MENU-ID-ATTR            PIC X.                  GA1FPGM 
00222      05  MAP-FROM-MENU-ID                 PIC X(04).              GA1FPGM 
00223                                                                   GA1FPGM 
00224      05  MAP-SELECT-LEN                   PIC S9(4) COMP SYNC.    GA1FPGM 
00225      05  MAP-SELECT-ATTR                  PIC X.                  GA1FPGM 
00226      05  MAP-SELECT                       PIC X(06).              GA1FPGM 
00227                                                                   GA1FPGM 
00228      05  MAP-SELECT-FROM-LEN              PIC S9(4) COMP SYNC.    GA1FPGM 
00229      05  MAP-SELECT-FROM-ATTR             PIC X.                  GA1FPGM 
00230      05  MAP-SELECT-FROM                  PIC X(03).              GA1FPGM 
00231                                                                   GA1FPGM 
00232      05  MAP-SELECT-TO-LEN                PIC S9(4) COMP SYNC.    GA1FPGM 
00233      05  MAP-SELECT-TO-ATTR               PIC X.                  GA1FPGM 
00234      05  MAP-SELECT-TO                    PIC X(03).              GA1FPGM 
00235                                                                   GA1FPGM 
00236      05  MAP-SELECT-OF-LEN                PIC S9(4) COMP SYNC.    GA1FPGM 
00237      05  MAP-SELECT-OF-ATTR               PIC X.                  GA1FPGM 
00238      05  MAP-SELECT-OF                    PIC X(03).              GA1FPGM 
00239                                                                   GA1FPGM 
00240 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00241 **                                                                GA1FPGM 
00242 **  THIS AREA MUST BE CHANGED TO MATCH ONE ENTRY IN THE MAP. THE  GA1FPGM 
00243 **  OCCURS COUNT CHANGED TO MATCH THE MAP.                        GA1FPGM 
00244 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00245      05  MAP-BENEFIT-CODE-ROW  OCCURS 15 TIMES INDEXED BY         GA1FPGM 
00246          MAP-IDX1.                                                GA1FPGM 
00247        10  MAP-BENEFIT-CODE-COL  OCCURS 3 TIMES INDEXED BY        GA1FPGM 
00248            MAP-IDX2.                                              GA1FPGM 
00249          15  MAP-ACTION-CODE-LEN     PIC S9(4) COMP SYNC.         GA1FPGM 
00250          15  MAP-ACTION-CODE-ATTR    PIC X.                       GA1FPGM 
00251          15  MAP-ACTION-CODE         PIC X.                       GA1FPGM 
00252          15  MAP-BENEFIT-CODE-LEN    PIC S9(4) COMP SYNC.         GA1FPGM 
00253          15  MAP-BENEFIT-CODE-ATTR   PIC X.                       GA1FPGM 
00254          15  MAP-BENEFIT-CODE        PIC X(6).                    GA1FPGM 
00255                                                                   GA1FPGM 
00256      05  MAP-ERROR-MESSAGE-LEN            PIC S9(4) COMP SYNC.    GA1FPGM 
00257      05  MAP-ERROR-MESSAGE-ATTR           PIC X.                  GA1FPGM 
00258      05  MAP-ERROR-MESSAGE                PIC X(79).              GA1FPGM 
00259 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00260  01  WS-MAP-OCCURS-COUNTERS.                                      GA1FPGM 
00261 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00262 **  THIS AREA MUST BE CHANGED TO NUMBER OF OCCURS FROM MAP.       GA1FPGM 
00263 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00264      05  WS-MAP-ROW              PIC S9(3)  COMP-3  VALUE +15.    GA1FPGM 
00265      05  WS-MAP-COL              PIC S9(3)  COMP-3  VALUE +3.     GA1FPGM 
00266 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00267      EJECT                                                        GA1FPGM 
00268 ** ALTERNATIVE WORKFILE KEYS **                                   GA1FPGM 
00269  01  FILLER                      PIC X(32)  VALUE                 GA1FPGM 
00270      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA1FPGM 
00271  01  WS-ALT-WORKFILE-KEYS.                                        GA1FPGM 
00272  COPY GCWRKKEY.                                                   GA1FPGM 
00273                                                                   GA1FPGM 
00274 *** HARDCOPY WORK AREA **                                         GA1FPGM 
00275 * COMMENTED OUT 10/24/90.                                         GA1FPGM 
00276 * 01  WS-HARDCOPY-COMMAREA.                                       GA1FPGM 
00277 * COPY PRNCOBOL.                                                  GA1FPGM 
00278                                                                   GA1FPGM 
00279 ** WORKFIELDS, AND SWITCHES **                                    GA1FPGM 
00280  01  WS-WORK-FIELDS.                                              GA1FPGM 
00281      05  WS-HEX-00                     PIC X.                     GA1FPGM 
00282      05  WS-DELETE-COUNT               PIC 999  COMP-3.           GA1FPGM 
00283 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00284 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1FPGM 
00285 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00286      05  WS-SAVED-FIELDS.                                         GA1FPGM 
00287        10  WS-SAVED-BENEFIT-CODE       PIC X(6).                  GA1FPGM 
00288 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00289  01  WS-SWITCHES.                                                 GA1FPGM 
00290      05  WS-ERROR-SW                   PIC X.                     GA1FPGM 
00291                                                                   GA1FPGM 
00292 ** TITLE LINES **                                                 GA1FPGM 
00293  01  WS-TITLE-LINES.                                              GA1FPGM 
00294      05  GROUP-SPECIFIC-TITLE-LINE            PIC X(47)  VALUE    GA1FPGM 
00295          ' GROUP SPECIFIC ALL LEVEL TABULAR MAINTENANCE  '.       GA1FPGM 
00296      05  CONTRACT-TITLE-LINE                  PIC X(47)  VALUE    GA1FPGM 
00297          '    CONTRACT ALL LEVEL TABULAR MAINTENANCE     '.       GA1FPGM 
00298      05  BENEFIT-PROVISION-TITLE-LINE         PIC X(47)  VALUE    GA1FPGM 
00299          'BENEFIT PROVISION ALL LEVEL TABULAR MAINTENANCE'.       GA1FPGM 
00300                                                                   GA1FPGM 
00301      EJECT                                                        GA1FPGM 
00302 ** ATTRIBUTES **                                                  GA1FPGM 
00303  COPY DFHBMSCA.                                                   GA1FPGM 
00304      02  DFHBMABF                PIC X VALUE 'Z'.                 GA1FPGM 
00305      EJECT                                                        GA1FPGM 
00306 ** ATTENTION IDENTIFIERS **                                       GA1FPGM 
00307  COPY DFHAID.                                                     GA1FPGM 
00308      EJECT                                                        GA1FPGM 
00309 ** RECORD LENGTHS **                                              GA1FPGM 
00310  01  WS-RECORD-LENGTHS.                                           GA1FPGM 
00311     05 WS-IO-PARM-WRK-ALL-LVL-TAB-LEN PIC S9(4) COMP.             GA1FPGM 
00312     05 WS-XCTL-WRK-LEN                PIC S9(4) COMP.             GA1FPGM 
00313     05 WS-COPY-LENGTH                 PIC S9(4) COMP.             GA1FPGM 
00314     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA1FPGM 
00315 *   05 GC-GCIOPARM-LEN                PIC S9(5) COMP-3 VALUE +228.GA1FPGM 
00316 *   05 GC-WORKFILE-KEY-LEN            PIC S9(5) COMP-3 VALUE +64. GA1FPGM 
00317 *   05 GC-GCGRPSPC-FIXED-LEN          PIC S9(5) COMP-3 VALUE +410.GA1FPGM 
00318 *   05 GC-GCGRPSPC-VARY-LEN           PIC S9(5) COMP-3 VALUE +10. GA1FPGM 
00319 *   05 GC-GCGRPSPC-VARY-MAX-OCUR      PIC S9(5) COMP-3 VALUE +30. GA1FPGM 
00320 *   05 GC-GCCONTR-FIXED-LEN           PIC S9(5) COMP-3 VALUE +563.GA1FPGM 
00321 *   05 GC-GCCONTR-VARY-LEN            PIC S9(5) COMP-3 VALUE +10. GA1FPGM 
00322 *   05 GC-GCCONTR-VARY-MAX-OCUR       PIC S9(5) COMP-3 VALUE +520.GA1FPGM 
00323 *   05 GC-GCBENPRV-FIXED-LEN          PIC S9(5) COMP-3 VALUE +501.GA1FPGM 
00324 *   05 GC-GCBENPRV-VARY-LEN           PIC S9(5) COMP-3 VALUE +10. GA1FPGM 
00325 *   05 GC-BENPRV-VARY-MAX-OCUR        PIC S9(5) COMP-3 VALUE +15. GA1FPGM 
00326                                                                   GA1FPGM 
00327  01  WS-GCPS-LENGTHS.                                             GA1FPGM 
00328      COPY GCCDRLEN.                                               GA1FPGM 
00329                                                                   GA1FPGM 
00330  01  WS-END                      PIC X(16)  VALUE                 GA1FPGM 
00331      '*** W/S ENDS ***'.                                          GA1FPGM 
00332      EJECT                                                        GA1FPGM 
00333  LINKAGE SECTION.                                                 GA1FPGM 
00334  01  DFHCOMMAREA.                                                 GA1FPGM 
00335      COPY G2ALCKEC.                                               GA1FPGM 
00336 *    05  INCOMING-COMMAREA-PNTR-COMP   PIC S9(8)  COMP.           GA1FPGM 
00337 *    05  INCOMING-COMMAREA-PNTR  REDEFINES                        GA1FPGM 
00338 *        INCOMING-COMMAREA-PNTR-COMP   USAGE IS POINTER.          GA1FPGM 
00339 *                                                                 GA1FPGM 
00340 *01  BLL-CELLS.                                                   GA1FPGM 
00341 *    02  FILLER                  PIC S9(8)  COMP.                 GA1FPGM 
00342 *    02  COMMAREA-PNTR           PIC S9(8)  COMP.                 GA1FPGM 
00343 *    02  ALL-LEVEL-TAB-PNTR      PIC S9(8)  COMP.                 GA1FPGM 
00344 *    02  ALL-LEVEL-TAB-PNTR2     PIC S9(8)  COMP.                 GA1FPGM 
00345 *    02  COPY-AREA-PNTR          PIC S9(8)  COMP.                 GA1FPGM 
00346 *    02  GRP-SPEC-PNTR           PIC S9(8)  COMP.                 GA1FPGM 
00347 *    02  CONTRACT-PNTR           PIC S9(8)  COMP.                 GA1FPGM 
00348 *    02  CONTRACT-PNTR2          PIC S9(8)  COMP.                 GA1FPGM 
00349 *    02  BEN-PROV-PNTR           PIC S9(8)  COMP.                 GA1FPGM 
00350 *                                                                 GA1FPGM 
00351 *01  GCA-COMMAREA.                                                GA1FPGM 
00352 *COPY G2ALCKEC.                                                   GA1FPGM 
00353      EJECT                                                        GA1FPGM 
00354  01  IO-PARM-ALL-LVL-TAB-RECORD.                                  GA1FPGM 
00355  COPY GCIOPRM1.                                                   GA1FPGM 
00356      EJECT                                                        GA1FPGM 
00357  COPY GCWRKDCC.                                                   GA1FPGM 
00358      SKIP3                                                        GA1FPGM 
00359      SKIP3                                                        GA1FPGM 
00360      SKIP3                                                        GA1FPGM 
00361  COPY GCTAARC.                                                    GA1FPGM 
00362      EJECT                                                        GA1FPGM 
00363 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00364 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1FPGM 
00365 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00366  01  COPY-OF-TABLE-AREA.                                          GA1FPGM 
00367      05  COPY-OF-TABLE   OCCURS 660 TIMES   INDEXED BY  COPY-IDX. GA1FPGM 
00368        10  COPY-BENEFIT-CODE           PIC X(6).                  GA1FPGM 
00369 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00370      EJECT                                                        GA1FPGM 
00371  01  IO-PARM-GRP-SPEC-RECORD.                                     GA1FPGM 
00372  COPY GCIOPRM2.                                                   GA1FPGM 
00373      EJECT                                                        GA1FPGM 
00374  COPY GCWRKDC2.                                                   GA1FPGM 
00375      EJECT                                                        GA1FPGM 
00376  COPY GCGROUPC.                                                   GA1FPGM 
00377      EJECT                                                        GA1FPGM 
00378                                                                   GA1FPGM 
00379  01  IO-PARM-CONTRACT-RECORD.                                     GA1FPGM 
00380  COPY GCIOPRM3.                                                   GA1FPGM 
00381      EJECT                                                        GA1FPGM 
00382  COPY GCWRKDC3.                                                   GA1FPGM 
00383      EJECT                                                        GA1FPGM 
00384  COPY GCCONTRC.                                                   GA1FPGM 
00385      EJECT                                                        GA1FPGM 
00386                                                                   GA1FPGM 
00387  01  IO-PARM-BEN-PROV-RECORD.                                     GA1FPGM 
00388  COPY GCIOPRM4.                                                   GA1FPGM 
00389      EJECT                                                        GA1FPGM 
00390  COPY GCWRKDC4.                                                   GA1FPGM 
00391      EJECT                                                        GA1FPGM 
00392  COPY GCBENPVC.                                                   GA1FPGM 
00393      EJECT                                                        GA1FPGM 
00394  PROCEDURE DIVISION.                                              GA1FPGM 
00395                                                                   GA1FPGM 
00396 ******************************************************************GA1FPGM 
00397 **                H O U S E K E E P I N G                         GA1FPGM 
00398 **                                                                GA1FPGM 
00399 ** DO THE ONETIME LOGIC THAT IS DONE AT THE START OF THE PROGRAM. GA1FPGM 
00400 **                                                                GA1FPGM 
00401 ******************************************************************GA1FPGM 
00402  0000-HOUSEKEEPING SECTION.                                       GA1FPGM 
00403                                                                   GA1FPGM 
00404      MOVE  LOW-VALUES  TO  WS-HEX-00.                             GA1FPGM 
00405                                                                   GA1FPGM 
00406      IF EIBAID  =  DFHCLEAR                                       GA1FPGM 
00407          EXEC CICS SEND FROM(WS-ONE-LOW)                          GA1FPGM 
00408                         ERASE                                     GA1FPGM 
00409          END-EXEC                                                 GA1FPGM 
00410          EXEC CICS RETURN                                         GA1FPGM 
00411          END-EXEC.                                                GA1FPGM 
00412                                                                   GA1FPGM 
00413      EXEC CICS HANDLE CONDITION   MAPFAIL(6000-XCTL-TO-MAIN-MENU) GA1FPGM 
00414         NOSTG(9010-NO-STORAGE)                                    GA1FPGM 
00415         PGMIDERR(9020-PGM-ID-ERROR)   END-EXEC.                   GA1FPGM 
00416      EJECT                                                        GA1FPGM 
00417 ******************************************************************GA1FPGM 
00418 **                     M A I N L I N E                            GA1FPGM 
00419 **                                                                GA1FPGM 
00420 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA1FPGM 
00421 **  TAKEN BY THE OPERATOR.                                        GA1FPGM 
00422 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA1FPGM 
00423 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO DETERMINE  GA1FPGM 
00424 **     WHICH ENTRIES, IF ANY, THEY MIGHT WANT TO DELETE.          GA1FPGM 
00425 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA1FPGM 
00426 **     KEY PF12 OR PF24.                                          GA1FPGM 
00427 **  3. RECEIVE THE SCREEN.                                        GA1FPGM 
00428 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA1FPGM 
00429 **     MENU.                                                      GA1FPGM 
00430 **  5. IF THEY USED THE ENTER PF7/PF19, PF8/PF20, PF10/PF22,      GA1FPGM 
00431 **     PF11/PF23 KEY THEN PERFORM NORMAL DELETE LOGIC.            GA1FPGM 
00432 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA1FPGM 
00433 **     (RETURN) TO THE ADD PROGRAM (GA2FPGM).                     GA1FPGM 
00434 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA1FPGM 
00435 **     (RETURN) TO THE PREVIOUS MENU.                             GA1FPGM 
00436 **  8. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA1FPGM 
00437 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA1FPGM 
00438 **                                                                GA1FPGM 
00439 ******************************************************************GA1FPGM 
00440  1000-MAIN-LINE SECTION.                                          GA1FPGM 
00441                                                                   GA1FPGM 
00442      MOVE '1000'  TO  WS-PARA-ID.                                 GA1FPGM 
00443      IF EIBTRNID  NOT =  'GA1F'                                   GA1FPGM 
00444         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA1FPGM 
00445         GO TO 1099-RETURN.                                        GA1FPGM 
00446                                                                   GA1FPGM 
00447                                                                   GA1FPGM 
00448      EXEC CICS RECEIVE   MAP('GA1FI01') MAPSET('GA1FSET')         GA1FPGM 
00449         INTO(GA1FI01I) END-EXEC.                                  GA1FPGM 
00450                                                                   GA1FPGM 
00451      IF MAP-SCREEN-ID   NOT = '001F00'                            GA1FPGM 
00452         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA1FPGM 
00453                                                                   GA1FPGM 
00454      IF EIBAID  =  DFHENTER OR                                    GA1FPGM 
00455                    DFHPF7   OR DFHPF19 OR                         GA1FPGM 
00456                    DFHPF8   OR DFHPF20 OR                         GA1FPGM 
00457                    DFHPF10  OR DFHPF22 OR                         GA1FPGM 
00458                    DFHPF11  OR DFHPF23                            GA1FPGM 
00459         PERFORM 2000-DELETE-PROCESSING                            GA1FPGM 
00460         GO TO 1099-RETURN.                                        GA1FPGM 
00461                                                                   GA1FPGM 
00462      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA1FPGM 
00463         PERFORM 3000-XCTL-TO-ADD-SCREEN.                          GA1FPGM 
00464                                                                   GA1FPGM 
00465      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA1FPGM 
00466         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA1FPGM 
00467                                                                   GA1FPGM 
00468      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1FPGM 
00469      MOVE -1  TO  MAP-SELECT-LEN.                                 GA1FPGM 
00470      MOVE '*** INVALID REQUEST. THE PF KEY USED HAS NO MEANING TO GA1FPGM 
00471 -    'THIS PROGRAM ***'  TO  MAP-ERROR-MESSAGE.                   GA1FPGM 
00472      EXEC CICS SEND   MAP('GA1FI01') MAPSET('GA1FSET') DATAONLY   GA1FPGM 
00473         FROM(GA1FI01O) CURSOR END-EXEC.                           GA1FPGM 
00474                                                                   GA1FPGM 
00475  1099-RETURN.                                                     GA1FPGM 
00476 *    EXEC CICS RETURN  END-EXEC.                                  GA1FPGM 
00477      EXEC CICS RETURN TRANSID ('GA1F')                            GA1FPGM 
00478                COMMAREA (DFHCOMMAREA)                             GA1FPGM 
00479                END-EXEC.                                          GA1FPGM 
00480                                                                   GA1FPGM 
00481      GOBACK.                                                      GA1FPGM 
00482      EJECT                                                        GA1FPGM 
00483 ******************************************************************GA1FPGM 
00484 **              D E L E T E   P R O C E S S I N G                 GA1FPGM 
00485 **                                                                GA1FPGM 
00486 **  WE WILL PERFORM THE FOLLOWING OPERATIONS IN DELETE PROCESSING:GA1FPGM 
00487 **                                                                GA1FPGM 
00488 ** 1. VALIDATE THAT THE ACTION CODE IS EITHER BLANK, 'D', OR LOW- GA1FPGM 
00489 **    VALUES (IF THE OPERATOR KEYED ERASE EOF).                   GA1FPGM 
00490 **                                                                GA1FPGM 
00491 ** 2. READ THE TABULAR RECORD AND MAKE A COPY OF THE RECORD.      GA1FPGM 
00492 **    (WE WILL BE MOVING ENTRIES THAT AREN'T DELETED FROM THE COPYGA1FPGM 
00493 **    BACK INTO THE RECORD THAT WE READ.)                         GA1FPGM 
00494 **                                                                GA1FPGM 
00495 ** 3. FIND THE ENTRY IN THE COPY THAT CORRESPONDS TO THE ENTRY ON GA1FPGM 
00496 **    THE SCREEN.  IF THE SCREEN HAS BEEN POSITIONED PAST SOME    GA1FPGM 
00497 **    ENTRIES IN THE COPY THEY WILL BE MOVED BACK INTO THE RECORD.GA1FPGM 
00498 **                                                                GA1FPGM 
00499 ** 4. IF THE ENTRY ON THE SCREEN AND IN THE COPY MATCH BUT THE    GA1FPGM 
00500 **    ENTRY IS NOT MARKED FOR DELETION THEN SAVE THE ENTRY.       GA1FPGM 
00501 **                                                                GA1FPGM 
00502 ** 5. IF THE TWO ENTRIES MATCH AND IT IS MARKED FOR DELETION THEN GA1FPGM 
00503 **    POSITION THE INDEX FOR THE SCREEN AND FOR THE COPY PAST THISGA1FPGM 
00504 **    ENTRY.                                                      GA1FPGM 
00505 **                                                                GA1FPGM 
00506 ** 6. IF WE GET PAST THE LAST ENTRY ON THE SCREEN AND THERE ARE   GA1FPGM 
00507 **    MORE ENTRIES IN THE COPY THEN MOVE ALL OF THEM BACK INTO THEGA1FPGM 
00508 **    RECORD.                                                     GA1FPGM 
00509 **                                                                GA1FPGM 
00510 ** 7. FINALLY REWRITE THE RECORD BACK ONTO THE WORKFILE.  SAVE THEGA1FPGM 
00511 **    NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD THEGA1FPGM 
00512 **    DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.               GA1FPGM 
00513 **                                                                GA1FPGM 
00514 ** 8. IF NO ENTRIES WERE MARKED FOR DELETION THEN STEPS 2 THRU 7  GA1FPGM 
00515 **    ARE BYPASSED; WE READ THE ALL LEVEL TABULAR RECORD:         GA1FPGM 
00516 **     A. IF ENTER WAS KEYED - SAVE THE NEXT ENTRY TO BE DISPLAYEDGA1FPGM 
00517 **        PERFORM THE ROUTINE TO BUILD THE DISPLAY, AND SEND THE  GA1FPGM 
00518 **        SCREEN TO THE OPERATOR.                                 GA1FPGM 
00519 **     B. IF PF7/PF19  KEYED - COMPUTE THE FIRST ENTRY OF THE     GA1FPGM 
00520 **        PREVIOUS PAGE TO BE DISPLAYED, PERFORM THE ROUTINE TO   GA1FPGM 
00521 **        BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR. GA1FPGM 
00522 **     C. IF PF8/PF20  KEYED - COMPUTE THE FIRST ENTRY OF THE     GA1FPGM 
00523 **        NEXT PAGE TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD GA1FPGM 
00524 **        BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR. GA1FPGM 
00525 **     D. IF PF10/PF22  KEYED - COMPUTE THE FIRST ENTRY OF THE    GA1FPGM 
00526 **        LAST PAGE TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD GA1FPGM 
00527 **        BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR. GA1FPGM 
00528 **     E. IF PF11/PF23  KEYED - COMPUTE THE FIRST ENTRY OF THE    GA1FPGM 
00529 **        FIRST PAGE TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILDGA1FPGM 
00530 **        BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR. GA1FPGM 
00531 **                                                                GA1FPGM 
00532 ******************************************************************GA1FPGM 
00533  2000-DELETE-PROCESSING SECTION.                                  GA1FPGM 
00534                                                                   GA1FPGM 
00535      MOVE '2000'  TO  WS-PARA-ID.                                 GA1FPGM 
00536      MOVE 'N'  TO  WS-ERROR-SW.                                   GA1FPGM 
00537      MOVE ZERO  TO  WS-DELETE-COUNT.                              GA1FPGM 
00538      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1FPGM 
00539                                                                   GA1FPGM 
00540      MOVE '2010'  TO  WS-PARA-ID.                                 GA1FPGM 
00541  2010-VALIDATE-ACT-CODE.                                          GA1FPGM 
00542      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2) =  'D'               GA1FPGM 
00543         ADD 1  TO  WS-DELETE-COUNT.                               GA1FPGM 
00544      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2) =  'D' OR            GA1FPGM 
00545         =  SPACE OR  =  LOW-VALUES                                GA1FPGM 
00546         MOVE DFHBMUNF  TO                                         GA1FPGM 
00547            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1FPGM 
00548         MOVE DFHBMASF  TO                                         GA1FPGM 
00549 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00550 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1FPGM 
00551 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00552            MAP-BENEFIT-CODE-ATTR (MAP-IDX1, MAP-IDX2)             GA1FPGM 
00553      ELSE                                                         GA1FPGM 
00554         MOVE DFHBMUBF  TO                                         GA1FPGM 
00555            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1FPGM 
00556         MOVE DFHBMABF  TO                                         GA1FPGM 
00557            MAP-BENEFIT-CODE-ATTR (MAP-IDX1, MAP-IDX2)             GA1FPGM 
00558 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00559         IF WS-ERROR-SW  NOT =  'Y'                                GA1FPGM 
00560            MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2)  GA1FPGM 
00561            MOVE 'Y'  TO  WS-ERROR-SW.                             GA1FPGM 
00562                                                                   GA1FPGM 
00563      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1FPGM 
00564         SET MAP-IDX1  UP BY  1                                    GA1FPGM 
00565      ELSE                                                         GA1FPGM 
00566         IF MAP-IDX2  <  WS-MAP-COL                                GA1FPGM 
00567            SET MAP-IDX1  TO  1                                    GA1FPGM 
00568            SET MAP-IDX2  UP BY  1                                 GA1FPGM 
00569         ELSE                                                      GA1FPGM 
00570            GO TO 2020-DONE-VALIDATE-A-C.                          GA1FPGM 
00571 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00572 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1FPGM 
00573 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00574      IF MAP-BENEFIT-CODE (MAP-IDX1, MAP-IDX2)  NOT =  LOW-VALUES  GA1FPGM 
00575 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00576         GO TO 2010-VALIDATE-ACT-CODE.                             GA1FPGM 
00577                                                                   GA1FPGM 
00578  2020-DONE-VALIDATE-A-C.                                          GA1FPGM 
00579      MOVE '2020'  TO  WS-PARA-ID.                                 GA1FPGM 
00580      SET MAP-IDX1  TO  1.                                         GA1FPGM 
00581      IF WS-ERROR-SW  =  'Y'                                       GA1FPGM 
00582         MOVE '*** INVALID ACTION CODE FOUND ***'  TO              GA1FPGM 
00583            MAP-ERROR-MESSAGE                                      GA1FPGM 
00584         MOVE LOW-VALUES  TO  MAP-FUNCTION-CODE                    GA1FPGM 
00585                              MAP-TITLE-LINE                       GA1FPGM 
00586                              MAP-SCREEN-ID                        GA1FPGM 
00587                              MAP-ALL-LEVEL-TAB-ID                 GA1FPGM 
00588                              MAP-ALL-LEVEL-TAB-SLOT               GA1FPGM 
00589                              MAP-ID-LINE                          GA1FPGM 
00590                              MAP-FROM-MENU-ID                     GA1FPGM 
00591 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00592 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1FPGM 
00593 **  ADD ITS MAP FIELD NAME HERE.                                  GA1FPGM 
00594 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00595         MOVE '2100'  TO  WS-PARA-ID                               GA1FPGM 
00596         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA1FPGM 
00597            VARYING MAP-IDX2 FROM  1  BY  1                        GA1FPGM 
00598                             UNTIL MAP-IDX2  >  WS-MAP-COL         GA1FPGM 
00599              AFTER MAP-IDX1 FROM  1  BY  1                        GA1FPGM 
00600                             UNTIL MAP-IDX1  > WS-MAP-ROW          GA1FPGM 
00601         EXEC CICS SEND   MAP('GA1FI01') MAPSET('GA1FSET') DATAONLYGA1FPGM 
00602            FROM(GA1FI01O) CURSOR END-EXEC                         GA1FPGM 
00603         GO TO 2099-EXIT.                                          GA1FPGM 
00604                                                                   GA1FPGM 
00605      COMPUTE WS-IO-PARM-WRK-ALL-LVL-TAB-LEN =                     GA1FPGM 
00606              GC-GCIOPARM-LEN +                                    GA1FPGM 
00607              GC-WORKFILE-KEY-LEN +                                GA1FPGM 
00608              GC-GCTABULR-AAR-FIXED-LEN +                          GA1FPGM 
00609             (GC-GCTABULR-AAR-VARY-MAX-OCUR *                      GA1FPGM 
00610              GC-GCTABULR-AAR-VARY-LEN).                           GA1FPGM 
00611                                                                   GA1FPGM 
00612 ***  EXEC CICS GETMAIN  SET(ALL-LEVEL-TAB-PNTR) INITIMG(WS-HEX-00)GA1FPGM 
00613      EXEC CICS GETMAIN                                            GA1FPGM 
00614         SET(ADDRESS OF IO-PARM-ALL-LVL-TAB-RECORD)                GA1FPGM 
00615         INITIMG(WS-HEX-00)                                        GA1FPGM 
00616         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA1FPGM 
00617                                                                   GA1FPGM 
00618 ***  SERVICE RELOAD  IO-PARM-ALL-LVL-TAB-RECORD.                  GA1FPGM 
00619 ***  ADD ALL-LEVEL-TAB-PNTR,  4096  GIVING  ALL-LEVEL-TAB-PNTR2.  GA1FPGM 
00620                                                                   GA1FPGM 
00621      IF  MAP-FROM-MENU-ID  = 'GS3A'                               GA1FPGM 
00622         MOVE SPACES               TO  GCIO-WORKFILE-KEY           GA1FPGM 
00623         MOVE 'G'                  TO  GCIO-WRK-STATUS-CODE        GA1FPGM 
00624         MOVE 'G3'                 TO  GCIO-WRK-RECORD-TYPE        GA1FPGM 
00625         MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE          GA1FPGM 
00626         MOVE GCA-GROUP-NO-1-3     TO  GCIO-WRK-GROUP-NO-1-3       GA1FPGM 
00627         MOVE GRP-SPEC-GROUP-NO    TO  GCIO-WRK-GROUP-NO           GA1FPGM 
00628         MOVE GCA-SEC-NO-1         TO  GCIO-WRK-SEC-NO-1           GA1FPGM 
00629         MOVE GRP-SPEC-SECTION-NO  TO  GCIO-WRK-SECTION-NO         GA1FPGM 
00630         MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE           GA1FPGM 
00631         MOVE SPACES               TO  GCIO-WRK-LINE-OF-BUS,       GA1FPGM 
00632                                       GCIO-WRK-PROVIDER-CONTROL   GA1FPGM 
00633         MOVE GRP-SPEC-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA1FPGM 
00634         MOVE GCA-EFFDT-CEN        TO  GCIO-WRK-EFFDT-CEN          GA1FPGM 
00635         MOVE MAP-ALL-LEVEL-TAB-ID TO  GCIO-WRK-PROVISION-ID       GA1FPGM 
00636         MOVE MAP-ALL-LEVEL-TAB-SLOT TO GCIO-WRK-PROVISION-SLOT-NO GA1FPGM 
00637         MOVE SPACES               TO  GCIO-WRK-TAB-PROVISION-ID   GA1FPGM 
00638         MOVE ZEROES               TO  GCIO-WRK-TAB-PROV-SLOT-NO.  GA1FPGM 
00639                                                                   GA1FPGM 
00640      IF  MAP-FROM-MENU-ID  = 'GC4A'  OR  'GTM1'                   GA1FPGM 
00641         MOVE SPACES               TO  GCIO-WORKFILE-KEY           GA1FPGM 
00642         MOVE 'C'                  TO  GCIO-WRK-STATUS-CODE        GA1FPGM 
00643         MOVE 'C3'                 TO  GCIO-WRK-RECORD-TYPE        GA1FPGM 
00644         MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE          GA1FPGM 
00645         MOVE GCA-GROUP-NO-1-3     TO  GCIO-WRK-GROUP-NO-1-3       GA1FPGM 
00646         MOVE CONTRACT-GROUP-NO    TO  GCIO-WRK-GROUP-NO           GA1FPGM 
00647         MOVE GCA-SEC-NO-1         TO  GCIO-WRK-SEC-NO-1           GA1FPGM 
00648         MOVE CONTRACT-SECTION-NO  TO  GCIO-WRK-SECTION-NO         GA1FPGM 
00649         MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE           GA1FPGM 
00650         MOVE CONTRACT-LOB         TO  GCIO-WRK-LINE-OF-BUS        GA1FPGM 
00651         MOVE CONTRACT-PROV-CTL    TO  GCIO-WRK-PROVIDER-CONTROL   GA1FPGM 
00652         MOVE CONTRACT-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA1FPGM 
00653         MOVE GCA-EFFDT-CEN        TO  GCIO-WRK-EFFDT-CEN          GA1FPGM 
00654         MOVE MAP-ALL-LEVEL-TAB-ID TO  GCIO-WRK-PROVISION-ID       GA1FPGM 
00655         MOVE MAP-ALL-LEVEL-TAB-SLOT TO GCIO-WRK-PROVISION-SLOT-NO GA1FPGM 
00656         MOVE SPACES               TO  GCIO-WRK-TAB-PROVISION-ID   GA1FPGM 
00657         MOVE ZEROES               TO  GCIO-WRK-TAB-PROV-SLOT-NO.  GA1FPGM 
00658                                                                   GA1FPGM 
00659      IF  MAP-FROM-MENU-ID  = 'GC8A'                               GA1FPGM 
00660         MOVE SPACES               TO  GCIO-WORKFILE-KEY           GA1FPGM 
00661         MOVE 'C'                  TO  GCIO-WRK-STATUS-CODE        GA1FPGM 
00662         MOVE 'C5'                 TO  GCIO-WRK-RECORD-TYPE        GA1FPGM 
00663         MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE          GA1FPGM 
00664         MOVE GCA-GROUP-NO-1-3     TO  GCIO-WRK-GROUP-NO-1-3       GA1FPGM 
00665         MOVE BEN-PROV-GROUP-NO    TO  GCIO-WRK-GROUP-NO           GA1FPGM 
00666         MOVE GCA-SEC-NO-1         TO  GCIO-WRK-SEC-NO-1           GA1FPGM 
00667         MOVE BEN-PROV-SECTION-NO  TO  GCIO-WRK-SECTION-NO         GA1FPGM 
00668         MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE           GA1FPGM 
00669         MOVE BEN-PROV-LOB         TO  GCIO-WRK-LINE-OF-BUS        GA1FPGM 
00670         MOVE BEN-PROV-PROV-CTL    TO  GCIO-WRK-PROVIDER-CONTROL   GA1FPGM 
00671         MOVE BEN-PROV-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA1FPGM 
00672         MOVE GCA-EFFDT-CEN        TO  GCIO-WRK-EFFDT-CEN          GA1FPGM 
00673         MOVE BEN-PROV-ID-NO       TO  GCIO-WRK-PROVISION-ID       GA1FPGM 
00674         MOVE 9999999              TO  GCIO-WRK-PROVISION-SLOT-NO  GA1FPGM 
00675         MOVE MAP-ALL-LEVEL-TAB-ID TO GCIO-WRK-TAB-PROVISION-ID    GA1FPGM 
00676         MOVE MAP-ALL-LEVEL-TAB-SLOT TO GCIO-WRK-TAB-PROV-SLOT-NO. GA1FPGM 
00677                                                                   GA1FPGM 
00678      MOVE 'GCPSWORK'  TO  GCIO-FILE-DDNAME.                       GA1FPGM 
00679      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA1FPGM 
00680      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA1FPGM 
00681                                                                   GA1FPGM 
00682      IF WS-DELETE-COUNT  =  ZERO                                  GA1FPGM 
00683         GO TO 2080-READ-NEXT-SCREENS-FIELDS.                      GA1FPGM 
00684                                                                   GA1FPGM 
00685      IF  EIBAID  =  DFHPF7   OR DFHPF19 OR                        GA1FPGM 
00686                     DFHPF8   OR DFHPF20 OR                        GA1FPGM 
00687                     DFHPF10  OR DFHPF22 OR                        GA1FPGM 
00688                     DFHPF11  OR DFHPF23                           GA1FPGM 
00689      THEN                                                         GA1FPGM 
00690          MOVE '*** ACTION CODE ENTRY INVALID WHEN PAGING ***'     GA1FPGM 
00691            TO MAP-ERROR-MESSAGE                                   GA1FPGM 
00692          MOVE -1  TO  MAP-SELECT-LEN                              GA1FPGM 
00693          MOVE LOW-VALUES  TO  MAP-FUNCTION-CODE                   GA1FPGM 
00694                               MAP-TITLE-LINE                      GA1FPGM 
00695                               MAP-SCREEN-ID                       GA1FPGM 
00696                               MAP-ALL-LEVEL-TAB-ID                GA1FPGM 
00697                               MAP-ALL-LEVEL-TAB-SLOT              GA1FPGM 
00698                               MAP-ID-LINE                         GA1FPGM 
00699                               MAP-FROM-MENU-ID                    GA1FPGM 
00700 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00701 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1FPGM 
00702 **  ADD ITS MAP FIELD NAME HERE.                                  GA1FPGM 
00703 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00704          MOVE '2100'  TO  WS-PARA-ID                              GA1FPGM 
00705          PERFORM 2100-DONT-RETRANSMIT-FIELDS                      GA1FPGM 
00706             VARYING MAP-IDX2 FROM  1  BY  1                       GA1FPGM 
00707                              UNTIL MAP-IDX2  >  WS-MAP-COL        GA1FPGM 
00708               AFTER MAP-IDX1 FROM  1  BY  1                       GA1FPGM 
00709                              UNTIL MAP-IDX1  > WS-MAP-ROW         GA1FPGM 
00710          EXEC CICS SEND   MAP('GA1FI01')                          GA1FPGM 
00711                           MAPSET('GA1FSET')                       GA1FPGM 
00712                           DATAONLY                                GA1FPGM 
00713                           FROM(GA1FI01O)                          GA1FPGM 
00714                           CURSOR                                  GA1FPGM 
00715                           END-EXEC                                GA1FPGM 
00716          GO TO 2099-EXIT                                          GA1FPGM 
00717      ELSE                                                         GA1FPGM 
00718          NEXT SENTENCE.                                           GA1FPGM 
00719                                                                   GA1FPGM 
00720 ******************************************************************GA1FPGM 
00721 *      WE FOUND ENTRIES TO DELETE AND THERE WERE NO ERRORS.       GA1FPGM 
00722 *                                                                 GA1FPGM 
00723 ******************************************************************GA1FPGM 
00724                                                                   GA1FPGM 
00725      MOVE GC-GCTABULR-AAR-VARY-MAX-OCUR                           GA1FPGM 
00726      TO   GAE-ENTRY-COUNT.                                        GA1FPGM 
00727                                                                   GA1FPGM 
00728      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1FPGM 
00729                                                                   GA1FPGM 
00730      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1FPGM 
00731         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA1FPGM 
00732         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA1FPGM 
00733                                                                   GA1FPGM 
00734      IF  NOT GCIO-GOOD-RETURN                                     GA1FPGM 
00735         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA1FPGM 
00736 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA1FPGM 
00737         MOVE '1FF1'  TO  WS-ABEND-CODE                            GA1FPGM 
00738         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1FPGM 
00739                                                                   GA1FPGM 
00740      COMPUTE  WS-COPY-LENGTH  =                                   GA1FPGM 
00741            GAE-ENTRY-COUNT  *  GC-GCTABULR-AAR-VARY-LEN.          GA1FPGM 
00742                                                                   GA1FPGM 
00743 ***  EXEC CICS GETMAIN  SET(COPY-AREA-PNTR) LENGTH(WS-COPY-LENGTH)GA1FPGM 
00744      EXEC CICS GETMAIN                                            GA1FPGM 
00745         SET(ADDRESS OF COPY-OF-TABLE-AREA)                        GA1FPGM 
00746         LENGTH(WS-COPY-LENGTH)                                    GA1FPGM 
00747         INITIMG(WS-HEX-00) END-EXEC.                              GA1FPGM 
00748                                                                   GA1FPGM 
00749 ***  SERVICE RELOAD COPY-OF-TABLE-AREA.                           GA1FPGM 
00750                                                                   GA1FPGM 
00751      MOVE GAE-ENTRY-COUNT  TO  GAE-ENTRY-COUNT.                   GA1FPGM 
00752      SET COPY-IDX, GAE-INDEX  TO  1.                              GA1FPGM 
00753                                                                   GA1FPGM 
00754      MOVE '2030'  TO  WS-PARA-ID.                                 GA1FPGM 
00755  2030-MAKE-A-COPY-OF-RECORD.                                      GA1FPGM 
00756      IF GAE-INDEX  NOT >  GAE-ENTRY-COUNT                         GA1FPGM 
00757         MOVE GAE-ENTRY (GAE-INDEX)  TO  COPY-OF-TABLE (COPY-IDX)  GA1FPGM 
00758         SET COPY-IDX, GAE-INDEX  UP BY 1                          GA1FPGM 
00759         GO TO 2030-MAKE-A-COPY-OF-RECORD.                         GA1FPGM 
00760                                                                   GA1FPGM 
00761      SET MAP-IDX1, MAP-IDX2, COPY-IDX, GAE-INDEX  TO  1.          GA1FPGM 
00762      MOVE '2040'  TO  WS-PARA-ID.                                 GA1FPGM 
00763  2040-DELETE-MARKED-ENTRIES.                                      GA1FPGM 
00764 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00765 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1FPGM 
00766 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00767      IF MAP-BENEFIT-CODE (MAP-IDX1, MAP-IDX2)  =  LOW-VALUES      GA1FPGM 
00768         GO TO 2060-SAVE-REST-OF-COPY.                             GA1FPGM 
00769                                                                   GA1FPGM 
00770      IF MAP-BENEFIT-CODE (MAP-IDX1, MAP-IDX2)  >                  GA1FPGM 
00771         COPY-BENEFIT-CODE (COPY-IDX)                              GA1FPGM 
00772         GO TO 2050-SAVE-COPIED-ENTRY                              GA1FPGM 
00773      ELSE                                                         GA1FPGM 
00774         IF MAP-BENEFIT-CODE (MAP-IDX1, MAP-IDX2)  <               GA1FPGM 
00775            COPY-BENEFIT-CODE (COPY-IDX)                           GA1FPGM 
00776            MOVE '1FL1'  TO  WS-ABEND-CODE                         GA1FPGM 
00777            MOVE '*** PROGRAM ERROR FOUND IN PARA 2040, PLEASE INFOGA1FPGM 
00778 -    'RM SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE                 GA1FPGM 
00779            PERFORM 9999-ERROR-MSG-THEN-ABEND.                     GA1FPGM 
00780                                                                   GA1FPGM 
00781 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00782                                                                   GA1FPGM 
00783      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  NOT =  'D'          GA1FPGM 
00784         IF MAP-IDX1  <  WS-MAP-ROW                                GA1FPGM 
00785            SET MAP-IDX1  UP BY  1                                 GA1FPGM 
00786            GO TO 2050-SAVE-COPIED-ENTRY                           GA1FPGM 
00787         ELSE                                                      GA1FPGM 
00788            IF MAP-IDX2  <  WS-MAP-COL                             GA1FPGM 
00789               SET MAP-IDX1  TO  1                                 GA1FPGM 
00790               SET MAP-IDX2  UP BY  1                              GA1FPGM 
00791               GO TO 2050-SAVE-COPIED-ENTRY                        GA1FPGM 
00792            ELSE                                                   GA1FPGM 
00793               GO TO 2060-SAVE-REST-OF-COPY.                       GA1FPGM 
00794                                                                   GA1FPGM 
00795      SET COPY-IDX  UP BY  1.                                      GA1FPGM 
00796      IF COPY-IDX  NOT <  GAE-ENTRY-COUNT                          GA1FPGM 
00797         MOVE COPY-OF-TABLE (COPY-IDX)  TO  GAE-ENTRY (GAE-INDEX)  GA1FPGM 
00798         SET  GAE-ENTRY-COUNT  TO  GAE-INDEX                       GA1FPGM 
00799         MOVE GAE-ENTRY-COUNT  TO  GAE-ENTRY-COUNT                 GA1FPGM 
00800         GO TO 2070-UPDATE-MODIFIED-REC.                           GA1FPGM 
00801      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1FPGM 
00802         SET MAP-IDX1  UP BY  1                                    GA1FPGM 
00803         GO TO 2040-DELETE-MARKED-ENTRIES.                         GA1FPGM 
00804      IF MAP-IDX2  <  WS-MAP-COL                                   GA1FPGM 
00805         SET MAP-IDX1  TO  1                                       GA1FPGM 
00806         SET MAP-IDX2  UP BY  1                                    GA1FPGM 
00807         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1FPGM 
00808      ELSE                                                         GA1FPGM 
00809         GO TO 2060-SAVE-REST-OF-COPY.                             GA1FPGM 
00810                                                                   GA1FPGM 
00811  2050-SAVE-COPIED-ENTRY.                                          GA1FPGM 
00812      MOVE '2050'  TO  WS-PARA-ID.                                 GA1FPGM 
00813      MOVE COPY-OF-TABLE (COPY-IDX)  TO  GAE-ENTRY (GAE-INDEX).    GA1FPGM 
00814                                                                   GA1FPGM 
00815      SET GAE-INDEX  UP BY  1.                                     GA1FPGM 
00816      IF COPY-IDX  <  GAE-ENTRY-COUNT                              GA1FPGM 
00817         SET COPY-IDX  UP BY  1                                    GA1FPGM 
00818         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1FPGM 
00819      ELSE                                                         GA1FPGM 
00820 ***      SOMETHING'S WRONG WE SHOULDN'T BE IN THIS POSITION.  THE GA1FPGM 
00821 ***      MAP HAS MORE ENTRIES BUT WE HAVE JUST REACHED THE END OF GA1FPGM 
00822 ***      THE TABLE OF ENTRIES.                                    GA1FPGM 
00823         MOVE '1FL2'  TO  WS-ABEND-CODE                            GA1FPGM 
00824         MOVE '*** PROGRAM ERROR FOUND IN PARA 2050, PLEASE INFORM GA1FPGM 
00825 -    'SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE                    GA1FPGM 
00826         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1FPGM 
00827                                                                   GA1FPGM 
00828  2060-SAVE-REST-OF-COPY.                                          GA1FPGM 
00829      MOVE '2060'  TO  WS-PARA-ID.                                 GA1FPGM 
00830      MOVE COPY-OF-TABLE (COPY-IDX)  TO  GAE-ENTRY (GAE-INDEX).    GA1FPGM 
00831                                                                   GA1FPGM 
00832      SET GAE-INDEX  UP BY  1.                                     GA1FPGM 
00833      IF COPY-IDX  <  GAE-ENTRY-COUNT                              GA1FPGM 
00834         SET COPY-IDX  UP BY  1                                    GA1FPGM 
00835         GO TO 2060-SAVE-REST-OF-COPY.                             GA1FPGM 
00836                                                                   GA1FPGM 
00837      SET GAE-INDEX  DOWN BY  1.                                   GA1FPGM 
00838      SET GAE-ENTRY-COUNT  TO  GAE-INDEX.                          GA1FPGM 
00839      MOVE GAE-ENTRY-COUNT  TO  GAE-ENTRY-COUNT.                   GA1FPGM 
00840                                                                   GA1FPGM 
00841  2070-UPDATE-MODIFIED-REC.                                        GA1FPGM 
00842      MOVE '2070'  TO  WS-PARA-ID.                                 GA1FPGM 
00843                                                                   GA1FPGM 
00844 *-- SET INDICATOR TO CAPTURE OPERATOR-ID.                         GA1FPGM 
00845                                                                   GA1FPGM 
00846      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA1FPGM 
00847      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1FPGM 
00848                                                                   GA1FPGM 
00849      COMPUTE  GCIO-RECORD-LENGTH  =                               GA1FPGM 
00850               GC-WORKFILE-KEY-LEN        +                        GA1FPGM 
00851               GC-GCTABULR-AAR-FIXED-LEN  +                        GA1FPGM 
00852              (GAE-ENTRY-COUNT  *  GC-GCTABULR-AAR-VARY-LEN).      GA1FPGM 
00853                                                                   GA1FPGM 
00854      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA1FPGM 
00855               GC-GCIOPARM-LEN     +                               GA1FPGM 
00856               GCIO-RECORD-LENGTH.                                 GA1FPGM 
00857                                                                   GA1FPGM 
00858      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1FPGM 
00859         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA1FPGM 
00860         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA1FPGM 
00861                                                                   GA1FPGM 
00862      IF GCIO-GOOD-RETURN                                          GA1FPGM 
00863         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1FPGM 
00864      MOVE '*** ERROR REWRITING ALL LEVEL TABULAR RECORD.  PLEASE CGA1FPGM 
00865 -    'ONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE.            GA1FPGM 
00866      MOVE '1FF2'  TO  WS-ABEND-CODE.                              GA1FPGM 
00867      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1FPGM 
00868                                                                   GA1FPGM 
00869  2080-READ-NEXT-SCREENS-FIELDS.                                   GA1FPGM 
00870      MOVE  '2080'  TO  WS-PARA-ID.                                GA1FPGM 
00871                                                                   GA1FPGM 
00872      MOVE GC-GCTABULR-AAR-VARY-MAX-OCUR                           GA1FPGM 
00873      TO   GAE-ENTRY-COUNT.                                        GA1FPGM 
00874                                                                   GA1FPGM 
00875      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA1FPGM 
00876      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1FPGM 
00877         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA1FPGM 
00878         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA1FPGM 
00879                                                                   GA1FPGM 
00880      IF GCIO-GOOD-RETURN                                          GA1FPGM 
00881         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1FPGM 
00882      MOVE '1FF3'  TO  WS-ABEND-CODE.                              GA1FPGM 
00883      MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE CONGA1FPGM 
00884 -    'TACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE.              GA1FPGM 
00885      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1FPGM 
00886  2090-BUILD-NEXT-DISPLAY.                                         GA1FPGM 
00887      MOVE  '2090'  TO  WS-PARA-ID.                                GA1FPGM 
00888      SET MAP-IDX1  TO  WS-MAP-ROW.                                GA1FPGM 
00889      SET MAP-IDX2  TO  WS-MAP-COL.                                GA1FPGM 
00890      SET GAE-INDEX TO  1.                                         GA1FPGM 
00891 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00892 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1FPGM 
00893 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00894      IF MAP-BENEFIT-CODE (MAP-IDX1, MAP-IDX2)  =  LOW-VALUES      GA1FPGM 
00895         MOVE GAE-ENTRY (GAE-INDEX)  TO  WS-SAVED-FIELDS           GA1FPGM 
00896      ELSE                                                         GA1FPGM 
00897         MOVE MAP-BENEFIT-CODE (MAP-IDX1, MAP-IDX2) TO             GA1FPGM 
00898                                          WS-SAVED-BENEFIT-CODE.   GA1FPGM 
00899 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00900                                                                   GA1FPGM 
00901      IF  MAP-SELECT-LEN > 0        AND                            GA1FPGM 
00902          MAP-SELECT     > SPACES                                  GA1FPGM 
00903          MOVE MAP-SELECT TO WS-SAVED-BENEFIT-CODE.                GA1FPGM 
00904                                                                   GA1FPGM 
00905      IF  EIBAID  =  DFHPF7   OR DFHPF19 OR                        GA1FPGM 
00906                     DFHPF8   OR DFHPF20 OR                        GA1FPGM 
00907                     DFHPF10  OR DFHPF22 OR                        GA1FPGM 
00908                     DFHPF11  OR DFHPF23                           GA1FPGM 
00909      THEN                                                         GA1FPGM 
00910          MOVE SPACES TO MAP-SELECT                                GA1FPGM 
00911      ELSE                                                         GA1FPGM 
00912          GO TO 2090-FILL-THE-SCREEN.                              GA1FPGM 
00913                                                                   GA1FPGM 
00914      IF  EIBAID  =  DFHPF7   OR DFHPF19                           GA1FPGM 
00915          GO TO 2090-PAGE-BACKWARD.                                GA1FPGM 
00916      IF  EIBAID  =  DFHPF8   OR DFHPF20                           GA1FPGM 
00917          GO TO 2090-PAGE-FORWARD.                                 GA1FPGM 
00918      IF  EIBAID  =  DFHPF10  OR DFHPF22                           GA1FPGM 
00919          GO TO 2090-PAGE-TO-BOTTOM.                               GA1FPGM 
00920      IF  EIBAID  =  DFHPF11  OR DFHPF23                           GA1FPGM 
00921          GO TO 2090-PAGE-TO-TOP.                                  GA1FPGM 
00922                                                                   GA1FPGM 
00923  2090-PAGE-BACKWARD.                                              GA1FPGM 
00924                                                                   GA1FPGM 
00925      MOVE MAP-BENEFIT-CODE(1 1) TO WS-SAVED-BENEFIT-CODE.         GA1FPGM 
00926                                                                   GA1FPGM 
00927      SEARCH GAE-ENTRY                                             GA1FPGM 
00928          AT END                                                   GA1FPGM 
00929                MOVE GAE-BENEFIT-CODE(1) TO WS-SAVED-BENEFIT-CODE  GA1FPGM 
00930                GO TO 2090-FILL-THE-SCREEN                         GA1FPGM 
00931          WHEN                                                     GA1FPGM 
00932                WS-SAVED-BENEFIT-CODE = GAE-BENEFIT-CODE(GAE-INDEX)GA1FPGM 
00933                SET WS-GAE-INDEX TO GAE-INDEX.                     GA1FPGM 
00934                                                                   GA1FPGM 
00935      COMPUTE WS-GAE-INDEX = WS-GAE-INDEX                          GA1FPGM 
00936                           - (WS-MAP-ROW * WS-MAP-COL)             GA1FPGM 
00937                           + 1.                                    GA1FPGM 
00938                                                                   GA1FPGM 
00939      IF  WS-GAE-INDEX < +0                                        GA1FPGM 
00940      THEN                                                         GA1FPGM 
00941          MOVE GAE-BENEFIT-CODE(1) TO WS-SAVED-BENEFIT-CODE        GA1FPGM 
00942      ELSE                                                         GA1FPGM 
00943          SET  GAE-INDEX TO WS-GAE-INDEX                           GA1FPGM 
00944          MOVE GAE-BENEFIT-CODE(GAE-INDEX)                         GA1FPGM 
00945            TO WS-SAVED-BENEFIT-CODE.                              GA1FPGM 
00946                                                                   GA1FPGM 
00947      GO TO 2090-FILL-THE-SCREEN.                                  GA1FPGM 
00948                                                                   GA1FPGM 
00949  2090-PAGE-FORWARD.                                               GA1FPGM 
00950                                                                   GA1FPGM 
00951      IF  MAP-BENEFIT-CODE (MAP-IDX1, MAP-IDX2)  =  LOW-VALUES     GA1FPGM 
00952      THEN                                                         GA1FPGM 
00953          MOVE GAE-BENEFIT-CODE(GAE-INDEX)                         GA1FPGM 
00954            TO WS-SAVED-BENEFIT-CODE                               GA1FPGM 
00955      ELSE                                                         GA1FPGM 
00956          MOVE MAP-BENEFIT-CODE (MAP-IDX1, MAP-IDX2)               GA1FPGM 
00957            TO WS-SAVED-BENEFIT-CODE.                              GA1FPGM 
00958                                                                   GA1FPGM 
00959      GO TO 2090-FILL-THE-SCREEN.                                  GA1FPGM 
00960                                                                   GA1FPGM 
00961  2090-PAGE-TO-BOTTOM.                                             GA1FPGM 
00962                                                                   GA1FPGM 
00963                                                                   GA1FPGM 
00964      COMPUTE WS-GAE-INDEX = GAE-ENTRY-COUNT                       GA1FPGM 
00965                           - (WS-MAP-ROW * WS-MAP-COL).            GA1FPGM 
00966                                                                   GA1FPGM 
00967      IF  WS-GAE-INDEX < +0                                        GA1FPGM 
00968      THEN                                                         GA1FPGM 
00969          MOVE GAE-BENEFIT-CODE(1) TO WS-SAVED-BENEFIT-CODE        GA1FPGM 
00970      ELSE                                                         GA1FPGM 
00971          SET  GAE-INDEX TO WS-GAE-INDEX                           GA1FPGM 
00972          MOVE GAE-BENEFIT-CODE(GAE-INDEX)                         GA1FPGM 
00973                         TO WS-SAVED-BENEFIT-CODE.                 GA1FPGM 
00974                                                                   GA1FPGM 
00975      GO TO 2090-FILL-THE-SCREEN.                                  GA1FPGM 
00976                                                                   GA1FPGM 
00977  2090-PAGE-TO-TOP.                                                GA1FPGM 
00978                                                                   GA1FPGM 
00979      SET  GAE-INDEX TO 1.                                         GA1FPGM 
00980      MOVE GAE-BENEFIT-CODE(GAE-INDEX) TO WS-SAVED-BENEFIT-CODE.   GA1FPGM 
00981                                                                   GA1FPGM 
00982      GO TO 2090-FILL-THE-SCREEN.                                  GA1FPGM 
00983                                                                   GA1FPGM 
00984  2090-FILL-THE-SCREEN.                                            GA1FPGM 
00985                                                                   GA1FPGM 
00986      PERFORM 4500-FILL-THE-SCREEN.                                GA1FPGM 
00987      EXEC CICS SEND   MAP('GA1FI01') MAPSET('GA1FSET') ERASE      GA1FPGM 
00988         FROM(GA1FI01O) END-EXEC.                                  GA1FPGM 
00989                                                                   GA1FPGM 
00990  2099-EXIT.   EXIT.                                               GA1FPGM 
00991      EJECT                                                        GA1FPGM 
00992  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA1FPGM 
00993 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00994 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1FPGM 
00995 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
00996      MOVE LOW-VALUES  TO                                          GA1FPGM 
00997         MAP-ACTION-CODE  (MAP-IDX1, MAP-IDX2),                    GA1FPGM 
00998         MAP-BENEFIT-CODE (MAP-IDX1, MAP-IDX2).                    GA1FPGM 
00999 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
01000                                                                   GA1FPGM 
01001  2199-EXIT.   EXIT.                                               GA1FPGM 
01002      EJECT                                                        GA1FPGM 
01003 ******************************************************************GA1FPGM 
01004 **          X C T L   T O   A D D   S C R E E N                   GA1FPGM 
01005 **                                                                GA1FPGM 
01006 **  THE OPERATOR WANTS TO SWITCH MODES, FROM DELETING ENTRIES TO  GA1FPGM 
01007 ** ADDING ENTRIES.  WE READ THE ALL LEVEL TABULAR & PASS THE      GA1FPGM 
01008 ** ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL TABULAR  GA1FPGM 
01009 ** RECORD TO THE ADD PROGRAM.  (DEPENDING ON THE MENU THE PROGRAM GA1FPGM 
01010 ** ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE            GA1FPGM 
01011 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA1FPGM 
01012 ******************************************************************GA1FPGM 
01013  3000-XCTL-TO-ADD-SCREEN SECTION.                                 GA1FPGM 
01014      MOVE '3000'  TO  WS-PARA-ID.                                 GA1FPGM 
01015                                                                   GA1FPGM 
01016 ***  EXEC CICS GETMAIN  SET(COMMAREA-PNTR) INITIMG(WS-HEX-00)     GA1FPGM 
01017 *    EXEC CICS GETMAIN                                            GA1FPGM 
01018 *       SET(ADDRESS OF GCA-COMMAREA)                              GA1FPGM 
01019 *       INITIMG(WS-HEX-00)                                        GA1FPGM 
01020 *       LENGTH(WS-COMMUNICATION-KEY-LEN) END-EXEC.                GA1FPGM 
01021 ***  SERVICE RELOAD  GCA-COMMAREA.                                GA1FPGM 
01022                                                                   GA1FPGM 
01023 *    IF  MAP-FROM-MENU-ID  = 'GS3A'                               GA1FPGM 
01024 *       MOVE MAP-ID-LINE  TO GROUP-SPECIFIC-ID-LINE               GA1FPGM 
01025 *       MOVE GRP-SPEC-GROUP-NO  TO  GCA-GRP-NO                    GA1FPGM 
01026 *       MOVE GRP-SPEC-SECTION-NO  TO  GCA-SECTN-NO                GA1FPGM 
01027 *       MOVE GRP-SPEC-FAM-REL-LVL  TO  GCA-FAM-REL-LVL            GA1FPGM 
01028 *       MOVE GRP-SPEC-EFF-DATE  TO  GCA-EFFECTIVE-DATE            GA1FPGM 
01029 *       MOVE SPACES  TO  GCA-L-O-B,                               GA1FPGM 
01030 *                        GCA-PROV-CTL,                            GA1FPGM 
01031 *                        GCA-BEN-PROV-ID.                         GA1FPGM 
01032 *                                                                 GA1FPGM 
01033 *    IF  MAP-FROM-MENU-ID  = 'GC4A'  OR  'GTM1'                   GA1FPGM 
01034 *       MOVE MAP-ID-LINE  TO CONTRACT-ID-LINE                     GA1FPGM 
01035 *       MOVE CONTRACT-GROUP-NO  TO  GCA-GRP-NO                    GA1FPGM 
01036 *       MOVE CONTRACT-SECTION-NO  TO  GCA-SECTN-NO                GA1FPGM 
01037 *       MOVE CONTRACT-LOB  TO  GCA-L-O-B                          GA1FPGM 
01038 *       MOVE CONTRACT-PROV-CTL  TO  GCA-PROV-CTL                  GA1FPGM 
01039 *       MOVE CONTRACT-FAM-REL-LVL  TO  GCA-FAM-REL-LVL            GA1FPGM 
01040 *       MOVE CONTRACT-EFF-DATE  TO  GCA-EFFECTIVE-DATE            GA1FPGM 
01041 *       MOVE SPACES  TO  GCA-BEN-PROV-ID.                         GA1FPGM 
01042                                                                   GA1FPGM 
01043 *    IF  MAP-FROM-MENU-ID  = 'GC8A'                               GA1FPGM 
01044 *       MOVE MAP-ID-LINE  TO BENEFIT-PROVISION-ID-LINE            GA1FPGM 
01045 *       MOVE BEN-PROV-GROUP-NO  TO  GCA-GRP-NO                    GA1FPGM 
01046 *       MOVE BEN-PROV-SECTION-NO  TO  GCA-SECTN-NO                GA1FPGM 
01047 *       MOVE BEN-PROV-LOB  TO  GCA-L-O-B                          GA1FPGM 
01048 *       MOVE BEN-PROV-PROV-CTL  TO  GCA-PROV-CTL                  GA1FPGM 
01049 *       MOVE BEN-PROV-FAM-REL-LVL  TO  GCA-FAM-REL-LVL            GA1FPGM 
01050 *       MOVE BEN-PROV-EFF-DATE  TO  GCA-EFFECTIVE-DATE            GA1FPGM 
01051 *       MOVE BEN-PROV-ID-NO  TO  GCA-BEN-PROV-ID.                 GA1FPGM 
01052                                                                   GA1FPGM 
01053 *    MOVE MAP-ALL-LEVEL-TAB-ID  TO GCA-ALL-LEVEL-TAB-ID.          GA1FPGM 
01054 *    MOVE MAP-ALL-LEVEL-TAB-SLOT TO GCA-ALL-LEVEL-TAB-SLOT.       GA1FPGM 
01055 *    MOVE SPACES  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE,                GA1FPGM 
01056 *                     GCA-INTERNAL-TAB-ID,                        GA1FPGM 
01057 *                     GCA-INTERNAL-TAB-SLOT,                      GA1FPGM 
01058 *                     GCA-OCCURS-ENTRY-COUNTER,                   GA1FPGM 
01059 *                     GCA-ADD-DEL-IND.                            GA1FPGM 
01060 *    MOVE MAP-FROM-MENU-ID  TO GCA-FROM-MENU-ID.                  GA1FPGM 
01061 *    MOVE ZEROES  TO  GCA-EFF-DT.                                 GA1FPGM 
01062                                                                   GA1FPGM 
01063 *    SET  COMMAREA-PNTR   TO                                      GA1FPGM 
01064 *         ADDRESS  OF GCA-COMMAREA.                               GA1FPGM 
01065                                                                   GA1FPGM 
01066 *    EXEC CICS XCTL  PROGRAM('GA2FPGM') COMMAREA(COMMAREA-PNTR)   GA1FPGM 
01067 *       LENGTH(4) END-EXEC.                                       GA1FPGM 
01068      EXEC CICS XCTL  PROGRAM('GA2FPGM')                           GA1FPGM 
01069                      COMMAREA(DFHCOMMAREA)                        GA1FPGM 
01070                      LENGTH (LENGTH OF DFHCOMMAREA)               GA1FPGM 
01071      END-EXEC.                                                    GA1FPGM 
01072                                                                   GA1FPGM 
01073  3099-EXIT.   EXIT.                                               GA1FPGM 
01074      EJECT                                                        GA1FPGM 
01075 ***************************************************************** GA1FPGM 
01076 **          D I S P L A Y   F I R S T   S C R E E N               GA1FPGM 
01077 **                                                                GA1FPGM 
01078 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE MENU ORGA1FPGM 
01079 ** THE ADD PROGRAM, EITHER OF THOSE TWO PROGRAMS WILL READ THE    GA1FPGM 
01080 ** ALL LEVEL TABULAR RECORD & PASS US THE RECORD (PRECEEDED BY I/OGA1FPGM 
01081 ** PARMS AND WORKFILE KEY).  WE WILL THEN USE THAT RECORD TO BUILDGA1FPGM 
01082 ** THE SCREEN IMAGE.                                              GA1FPGM 
01083 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA1FPGM 
01084 ** (DEPENDING UPON THE TYPE OF SCREEN THE PROGRAM ORIGINATED FROM)GA1FPGM 
01085 ** SAVES THE FIRST ENTRY TO BE DISPLAYED, PERFORMS THE ROUTINE    GA1FPGM 
01086 ** WHICH USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO BE       GA1FPGM 
01087 ** DISPLAYED, THEN FILLS THE SCREEN WITH ALL SUCCEEDING ENTRIES,  GA1FPGM 
01088 ** AND FINALLY SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR   GA1FPGM 
01089 ** DETERMINATION OF APPROPRIATE ACTION.                           GA1FPGM 
01090 ******************************************************************GA1FPGM 
01091  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA1FPGM 
01092      MOVE '4000'  TO  WS-PARA-ID.                                 GA1FPGM 
01093                                                                   GA1FPGM 
01094      IF  EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                   GA1FPGM 
01095         MOVE '*** COMMONAREA LENGTH IS INVALID ***'               GA1FPGM 
01096            TO MAP-ERROR-MESSAGE                                   GA1FPGM 
01097         MOVE '1FC1'  TO  WS-ABEND-CODE                            GA1FPGM 
01098         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1FPGM 
01099                                                                   GA1FPGM 
01100 ***  MOVE INCOMING-COMMAREA-PNTR  TO  COMMAREA-PNTR.              GA1FPGM 
01101 ***  SERVICE RELOAD  GCA-COMMAREA.                                GA1FPGM 
01102 ***  MOVE GCA-RECORD-POINTER  TO  ALL-LEVEL-TAB-PNTR.             GA1FPGM 
01103 ***  SERVICE RELOAD  IO-PARM-ALL-LVL-TAB-RECORD.                  GA1FPGM 
01104 ***  ADD ALL-LEVEL-TAB-PNTR,  4096  GIVING  ALL-LEVEL-TAB-PNTR2.  GA1FPGM 
01105                                                                   GA1FPGM 
01106 *    SET ADDRESS OF GCA-COMMAREA                                  GA1FPGM 
01107 *    TO  INCOMING-COMMAREA-PNTR.                                  GA1FPGM 
01108                                                                   GA1FPGM 
01109      SET ADDRESS OF IO-PARM-ALL-LVL-TAB-RECORD                    GA1FPGM 
01110      TO  GCA-RECORD-POINTER.                                      GA1FPGM 
01111                                                                   GA1FPGM 
01112      MOVE GCA-ALL-LEVEL-TAB-ID  TO  MAP-ALL-LEVEL-TAB-ID.         GA1FPGM 
01113      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  MAP-ALL-LEVEL-TAB-SLOT.     GA1FPGM 
01114      MOVE GCA-FROM-MENU-ID  TO  MAP-FROM-MENU-ID.                 GA1FPGM 
01115 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
01116 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1FPGM 
01117 **  ITS MOVE TO THE MAP SHOULD BE HERE.                           GA1FPGM 
01118 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
01119                                                                   GA1FPGM 
01120      IF  GCA-FROM-MENU-ID  =  'GS3A'                              GA1FPGM 
01121         MOVE GROUP-SPECIFIC-TITLE-LINE  TO  MAP-TITLE-LINE        GA1FPGM 
01122         MOVE 'GROUP SPECIFIC ID = '  TO GRP-SPEC-ID-HEADING       GA1FPGM 
01123         MOVE 'GRP= '  TO  GRP-SPEC-GROUP-HEADING                  GA1FPGM 
01124         MOVE GCA-GRP-NO  TO  GRP-SPEC-GROUP-NO                    GA1FPGM 
01125         MOVE ' SEC= '  TO  GRP-SPEC-SECTION-HEADING               GA1FPGM 
01126         MOVE GCA-SECTN-NO  TO  GRP-SPEC-SECTION-NO                GA1FPGM 
01127         MOVE ' FR= '  TO  GRP-SPEC-FAM-REL-HEADING                GA1FPGM 
01128         MOVE GCA-FAM-REL-LVL  TO  GRP-SPEC-FAM-REL-LVL            GA1FPGM 
01129         MOVE ' EFDT= '  TO  GRP-SPEC-EFF-DT-HEADING               GA1FPGM 
01130         MOVE GCA-EFFECTIVE-DATE  TO  GRP-SPEC-EFF-DATE.           GA1FPGM 
01131                                                                   GA1FPGM 
01132      IF  GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                    GA1FPGM 
01133         MOVE CONTRACT-TITLE-LINE  TO  MAP-TITLE-LINE              GA1FPGM 
01134         MOVE 'CONTRACT ID = '  TO  CONTRACT-ID-HEADING            GA1FPGM 
01135         MOVE 'GRP= '  TO  CONTRACT-GROUP-HEADING                  GA1FPGM 
01136         MOVE GCA-GRP-NO  TO  CONTRACT-GROUP-NO                    GA1FPGM 
01137         MOVE ' SEC= '  TO  CONTRACT-SECTION-HEADING               GA1FPGM 
01138         MOVE GCA-SECTN-NO  TO  CONTRACT-SECTION-NO                GA1FPGM 
01139         MOVE ' LOB= '  TO  CONTRACT-LOB-HEADING                   GA1FPGM 
01140         MOVE GCA-L-O-B  TO  CONTRACT-LOB                          GA1FPGM 
01141         MOVE ' PRV= '  TO  CONTRACT-PROV-CTL-HEADING              GA1FPGM 
01142         MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL                  GA1FPGM 
01143         MOVE ' FR= '  TO  CONTRACT-FAM-REL-HEADING                GA1FPGM 
01144         MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL            GA1FPGM 
01145         MOVE ' EFDT= '  TO  CONTRACT-EFF-DT-HEADING               GA1FPGM 
01146         MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.           GA1FPGM 
01147                                                                   GA1FPGM 
01148      IF  GCA-FROM-MENU-ID  =  'GC8A'                              GA1FPGM 
01149         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  MAP-TITLE-LINE     GA1FPGM 
01150         MOVE 'GRP= '  TO  BEN-PROV-GROUP-HEADING                  GA1FPGM 
01151         MOVE GCA-GRP-NO  TO  BEN-PROV-GROUP-NO                    GA1FPGM 
01152         MOVE ' SEC= '  TO  BEN-PROV-SECTION-HEADING               GA1FPGM 
01153         MOVE GCA-SECTN-NO  TO  BEN-PROV-SECTION-NO                GA1FPGM 
01154         MOVE ' LOB= '  TO  BEN-PROV-LOB-HEADING                   GA1FPGM 
01155         MOVE GCA-L-O-B  TO  BEN-PROV-LOB                          GA1FPGM 
01156         MOVE ' PRV= '  TO  BEN-PROV-PROV-CTL-HEADING              GA1FPGM 
01157         MOVE GCA-PROV-CTL  TO  BEN-PROV-PROV-CTL                  GA1FPGM 
01158         MOVE ' FR= '  TO  BEN-PROV-FAM-REL-HEADING                GA1FPGM 
01159         MOVE GCA-FAM-REL-LVL  TO  BEN-PROV-FAM-REL-LVL            GA1FPGM 
01160         MOVE ' EFDT= '  TO  BEN-PROV-EFF-DT-HEADING               GA1FPGM 
01161         MOVE GCA-EFFECTIVE-DATE  TO  BEN-PROV-EFF-DATE            GA1FPGM 
01162         MOVE ' BPVID= '  TO  BEN-PROV-ID-HEADING                  GA1FPGM 
01163         MOVE GCA-BEN-PROV-ID  TO  BEN-PROV-ID-NO.                 GA1FPGM 
01164                                                                   GA1FPGM 
01165      SET GAE-INDEX  TO  1.                                        GA1FPGM 
01166      MOVE GAE-ENTRY (GAE-INDEX)  TO  WS-SAVED-FIELDS.             GA1FPGM 
01167                                                                   GA1FPGM 
01168      PERFORM 4500-FILL-THE-SCREEN.                                GA1FPGM 
01169      EXEC CICS SEND   MAP('GA1FI01') MAPSET('GA1FSET') ERASE      GA1FPGM 
01170         FROM(GA1FI01O) END-EXEC.                                  GA1FPGM 
01171                                                                   GA1FPGM 
01172  4099-EXIT.   EXIT.                                               GA1FPGM 
01173      EJECT                                                        GA1FPGM 
01174 ***************************************************************** GA1FPGM 
01175 **             F I L L   T H E   S C R E E N                      GA1FPGM 
01176 **                                                                GA1FPGM 
01177 **   THIS SECTION USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO GA1FPGM 
01178 ** BE DISPLAYED THEN MOVES ALL THE FOLLOWING ENTRIES THAT WILL FITGA1FPGM 
01179 ** ON THE SCREEN.  IF THE SCREEN HAS EXTRA ENTRIES THE ACTION CODEGA1FPGM 
01180 ** FOR THOSE ENTRIES WILL HAVE ITS ATTRIBUTE SET TO AUTO-SKIP SO  GA1FPGM 
01181 ** THE OPERATOR CANNOT ERRONEOUSLY MARK THIS ENTRY FOR DELETION.  GA1FPGM 
01182 ******************************************************************GA1FPGM 
01183  4500-FILL-THE-SCREEN SECTION.                                    GA1FPGM 
01184                                                                   GA1FPGM 
01185      MOVE '4500'  TO  WS-PARA-ID.                                 GA1FPGM 
01186      MOVE  GAE-ENTRY-COUNT  TO  GAE-ENTRY-COUNT.                  GA1FPGM 
01187      SET MAP-IDX1,  MAP-IDX2  TO  1.                              GA1FPGM 
01188                                                                   GA1FPGM 
01189      COMPUTE WS-SELECT-OF = GAE-ENTRY-COUNT - 1.                  GA1FPGM 
01190      MOVE WS-SELECT-OF      TO WS-SELECT-OF-MASK.                 GA1FPGM 
01191      MOVE WS-SELECT-OF-MASK TO MAP-SELECT-FROM                    GA1FPGM 
01192                                MAP-SELECT-TO                      GA1FPGM 
01193                                MAP-SELECT-OF.                     GA1FPGM 
01194                                                                   GA1FPGM 
01195      IF GAE-ENTRY-COUNT  NOT >  1                                 GA1FPGM 
01196         MOVE '4530'  TO  WS-PARA-ID                               GA1FPGM 
01197         GO TO 4530-FILL-REST-WITH-NULLS.                          GA1FPGM 
01198                                                                   GA1FPGM 
01199      SET GAE-INDEX  TO  1.                                        GA1FPGM 
01200      MOVE '4510'  TO  WS-PARA-ID.                                 GA1FPGM 
01201  4510-FIND-1ST-ENTRY-TO-DISPLAY.                                  GA1FPGM 
01202 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
01203 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1FPGM 
01204 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
01205      IF GAE-BENEFIT-CODE (GAE-INDEX)  <   WS-SAVED-BENEFIT-CODE   GA1FPGM 
01206 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
01207         SET GAE-INDEX  UP BY  1                                   GA1FPGM 
01208         IF  GAE-INDEX  <  GAE-ENTRY-COUNT                         GA1FPGM 
01209            GO TO 4510-FIND-1ST-ENTRY-TO-DISPLAY                   GA1FPGM 
01210         ELSE                                                      GA1FPGM 
01211            SET GAE-INDEX  TO  1.                                  GA1FPGM 
01212                                                                   GA1FPGM 
01213      SET  WS-SELECT-FROM       TO GAE-INDEX.                      GA1FPGM 
01214      MOVE WS-SELECT-FROM       TO WS-SELECT-FROM-MASK.            GA1FPGM 
01215      MOVE WS-SELECT-FROM-MASK TO MAP-SELECT-FROM.                 GA1FPGM 
01216                                                                   GA1FPGM 
01217      MOVE '4520'  TO  WS-PARA-ID.                                 GA1FPGM 
01218  4520-DISPLAY-ENTRIES-TO-DELETE.                                  GA1FPGM 
01219      MOVE DFHBMUNF  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1FPGM 
01220      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2).   GA1FPGM 
01221 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
01222 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1FPGM 
01223 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
01224      MOVE GAE-BENEFIT-CODE (GAE-INDEX)  TO                        GA1FPGM 
01225         MAP-BENEFIT-CODE (MAP-IDX1, MAP-IDX2).                    GA1FPGM 
01226 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
01227                                                                   GA1FPGM 
01228      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1FPGM 
01229         SET  MAP-IDX1  UP BY  1                                   GA1FPGM 
01230      ELSE                                                         GA1FPGM 
01231         IF MAP-IDX2  <  WS-MAP-COL                                GA1FPGM 
01232            SET  MAP-IDX1  TO  1                                   GA1FPGM 
01233            SET  MAP-IDX2  UP BY  1                                GA1FPGM 
01234         ELSE                                                      GA1FPGM 
01235            SET  WS-SELECT-TO         TO GAE-INDEX                 GA1FPGM 
01236            MOVE WS-SELECT-TO         TO WS-SELECT-TO-MASK         GA1FPGM 
01237            MOVE WS-SELECT-TO-MASK    TO MAP-SELECT-TO             GA1FPGM 
01238            GO TO 4540-DETERMINE-MSG-TO-DISPLAY.                   GA1FPGM 
01239                                                                   GA1FPGM 
01240      IF GAE-INDEX  <  (GAE-ENTRY-COUNT - 1 )                      GA1FPGM 
01241         SET  GAE-INDEX  UP BY  1                                  GA1FPGM 
01242         GO TO  4520-DISPLAY-ENTRIES-TO-DELETE.                    GA1FPGM 
01243                                                                   GA1FPGM 
01244      SET  WS-SELECT-TO         TO GAE-INDEX.                      GA1FPGM 
01245      MOVE WS-SELECT-TO         TO WS-SELECT-TO-MASK.              GA1FPGM 
01246      MOVE WS-SELECT-TO-MASK    TO MAP-SELECT-TO.                  GA1FPGM 
01247                                                                   GA1FPGM 
01248      MOVE '4530'  TO  WS-PARA-ID.                                 GA1FPGM 
01249  4530-FILL-REST-WITH-NULLS.                                       GA1FPGM 
01250      MOVE DFHBMASK  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1FPGM 
01251 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
01252 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1FPGM 
01253 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
01254      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),   GA1FPGM 
01255         MAP-BENEFIT-CODE (MAP-IDX1, MAP-IDX2).                    GA1FPGM 
01256 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1FPGM 
01257      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1FPGM 
01258         SET  MAP-IDX1  UP BY  1                                   GA1FPGM 
01259         GO TO 4530-FILL-REST-WITH-NULLS                           GA1FPGM 
01260      ELSE                                                         GA1FPGM 
01261         IF MAP-IDX2  <  WS-MAP-COL                                GA1FPGM 
01262            SET  MAP-IDX1  TO  1                                   GA1FPGM 
01263            SET  MAP-IDX2  UP BY  1                                GA1FPGM 
01264            GO TO 4530-FILL-REST-WITH-NULLS.                       GA1FPGM 
01265                                                                   GA1FPGM 
01266      MOVE '4540'  TO  WS-PARA-ID.                                 GA1FPGM 
01267  4540-DETERMINE-MSG-TO-DISPLAY.                                   GA1FPGM 
01268      IF GAE-ENTRY-COUNT  =  1                                     GA1FPGM 
01269         MOVE '*** NO ENTRIES TO DELETE ***'  TO  MAP-ERROR-MESSAGEGA1FPGM 
01270         GO TO 4599-EXIT.                                          GA1FPGM 
01271                                                                   GA1FPGM 
01272      IF MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)  =  DFHBMASK    GA1FPGM 
01273         MOVE '*** NO MORE ENTRIES TO DISPLAY ***'                 GA1FPGM 
01274           TO  MAP-ERROR-MESSAGE.                                  GA1FPGM 
01275                                                                   GA1FPGM 
01276  4599-EXIT.     EXIT.                                             GA1FPGM 
01277      EJECT                                                        GA1FPGM 
01278 ***************************************************************** GA1FPGM 
01279 **        X C T L   T O   P R E V I O U S   M E N U               GA1FPGM 
01280 **                                                                GA1FPGM 
01281 **  THE OPERATOR WANTS TO RETURN TO THE MENU THIS PROGRAM         GA1FPGM 
01282 ** ORIGINATED FROM.  WE READ THE ALL LEVEL TABULAR RECORD AND     GA1FPGM 
01283 ** PASS IT PRECEEDED BY THE WORKFILE KEY TO THE CORRECT           GA1FPGM 
01284 ** ORIGINATING PROGRAM (DETERMINED BY THE CODE IN THE 'FROM MENU  GA1FPGM 
01285 ** ID' FIELD).                                                    GA1FPGM 
01286 ******************************************************************GA1FPGM 
01287  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA1FPGM 
01288      MOVE '5000'  TO  WS-PARA-ID.                                 GA1FPGM 
01289                                                                   GA1FPGM 
01290      IF  MAP-FROM-MENU-ID  = 'GS3A'                               GA1FPGM 
01291         GO TO 5010-XCTL-TO-GRP-SPEC-MENU.                         GA1FPGM 
01292                                                                   GA1FPGM 
01293      IF  MAP-FROM-MENU-ID  = 'GC4A'                               GA1FPGM 
01294         GO TO 5020-XCTL-TO-CONTRACT-MENU.                         GA1FPGM 
01295                                                                   GA1FPGM 
01296      IF  MAP-FROM-MENU-ID  = 'GC8A'                               GA1FPGM 
01297         GO TO 5030-XCTL-TO-BEN-PROV-MENU.                         GA1FPGM 
01298                                                                   GA1FPGM 
01299      IF  MAP-FROM-MENU-ID  = 'GTM1'                               GA1FPGM 
01300         GO TO 5040-XCTL-TO-SINGLE-TAB-MENU.                       GA1FPGM 
01301                                                                   GA1FPGM 
01302  5010-XCTL-TO-GRP-SPEC-MENU.                                      GA1FPGM 
01303      MOVE '5010'  TO  WS-PARA-ID.                                 GA1FPGM 
01304                                                                   GA1FPGM 
01305      COMPUTE  WS-XCTL-WRK-LEN  = GC-GCIOPARM-LEN                  GA1FPGM 
01306                                + GC-WORKFILE-KEY-LEN              GA1FPGM 
01307                                + GC-GCGRPSPC-MAX-REC-LEN.         GA1FPGM 
01308                                                                   GA1FPGM 
01309                                                                   GA1FPGM 
01310 ***  EXEC CICS GETMAIN  SET(GRP-SPEC-PNTR) INITIMG(WS-HEX-00)     GA1FPGM 
01311      EXEC CICS GETMAIN                                            GA1FPGM 
01312         SET(ADDRESS OF IO-PARM-GRP-SPEC-RECORD)                   GA1FPGM 
01313         INITIMG(WS-HEX-00)                                        GA1FPGM 
01314         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1FPGM 
01315                                                                   GA1FPGM 
01316 ***  SERVICE RELOAD  IO-PARM-GRP-SPEC-RECORD.                     GA1FPGM 
01317                                                                   GA1FPGM 
01318      MOVE SPACES      TO  GCIO-WORKFILE-KEY.                      GA1FPGM 
01319                                                                   GA1FPGM 
01320      MOVE 'G'   TO  GCIO-WRK-STATUS-CODE.                         GA1FPGM 
01321      MOVE 'G2'  TO  GCIO-WRK-RECORD-TYPE.                         GA1FPGM 
01322      MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE.                    GA1FPGM 
01323      MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM.                    GA1FPGM 
01324      MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM.                GA1FPGM 
01325      MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE.                      GA1FPGM 
01326      MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                       GA1FPGM 
01327                       GCIO-WRK-PROVIDER-CONTROL.                  GA1FPGM 
01328      MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.        GA1FPGM 
01329      MOVE GCA-EFFDT-CEN   TO GCIO-WRK-EFFDT-CEN.                  GA1FPGM 
01330                                                                   GA1FPGM 
01331      MOVE 'GCPSWORK'  TO  GCIO2-FILE-DDNAME.                      GA1FPGM 
01332      MOVE SPACES  TO  GCIO-WRK-PROVISION-ID,                      GA1FPGM 
01333                       GCIO-WRK-TAB-PROVISION-ID.                  GA1FPGM 
01334      MOVE ZEROES  TO  GCIO-WRK-PROVISION-SLOT-NO,                 GA1FPGM 
01335                       GCIO-WRK-TAB-PROV-SLOT-NO.                  GA1FPGM 
01336      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA1FPGM 
01337                                                                   GA1FPGM 
01338      MOVE GC-GCGRPSPC-VARY-MAX-OCUR                               GA1FPGM 
01339      TO   GCG-COUNT-TAB-PROVN-POINTERS.                           GA1FPGM 
01340                                                                   GA1FPGM 
01341      MOVE  'RD '  TO  GCIO2-FILE-ACCESS-CODE.                     GA1FPGM 
01342                                                                   GA1FPGM 
01343      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1FPGM 
01344         COMMAREA(IO-PARM-GRP-SPEC-RECORD)                         GA1FPGM 
01345         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1FPGM 
01346                                                                   GA1FPGM 
01347      IF  NOT GCIO2-GOOD-RETURN                                    GA1FPGM 
01348         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA1FPGM 
01349 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA1FPGM 
01350         MOVE '1FF4'  TO  WS-ABEND-CODE                            GA1FPGM 
01351         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1FPGM 
01352                                                                   GA1FPGM 
01353      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1FPGM 
01354               GC-WORKFILE-KEY-LEN        +                        GA1FPGM 
01355               GC-GCGRPSPC-MAX-REC-LEN.                            GA1FPGM 
01356                                                                   GA1FPGM 
01357      EXEC CICS XCTL  PROGRAM('GS3APGM') COMMAREA(WORK-RECORD-2)   GA1FPGM 
01358         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1FPGM 
01359                                                                   GA1FPGM 
01360      GO  TO  5099-EXIT.                                           GA1FPGM 
01361                                                                   GA1FPGM 
01362  5020-XCTL-TO-CONTRACT-MENU.                                      GA1FPGM 
01363      MOVE '5020'  TO  WS-PARA-ID.                                 GA1FPGM 
01364                                                                   GA1FPGM 
01365      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1FPGM 
01366          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA1FPGM 
01367                 GC-GCCONTR-MAX-REC-LEN.                           GA1FPGM 
01368                                                                   GA1FPGM 
01369 ***  EXEC CICS GETMAIN  SET(CONTRACT-PNTR) INITIMG(WS-HEX-00)     GA1FPGM 
01370      EXEC CICS GETMAIN                                            GA1FPGM 
01371         SET(ADDRESS OF IO-PARM-CONTRACT-RECORD)                   GA1FPGM 
01372         INITIMG(WS-HEX-00)                                        GA1FPGM 
01373         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1FPGM 
01374 ***  SERVICE RELOAD  IO-PARM-CONTRACT-RECORD.                     GA1FPGM 
01375 ***  ADD  CONTRACT-PNTR,  4096  GIVING  CONTRACT-PNTR2.           GA1FPGM 
01376                                                                   GA1FPGM 
01377      MOVE SPACES      TO  GCIO-WORKFILE-KEY.                      GA1FPGM 
01378                                                                   GA1FPGM 
01379      MOVE 'C'   TO  GCIO-WRK-STATUS-CODE.                         GA1FPGM 
01380      MOVE 'C2'  TO  GCIO-WRK-RECORD-TYPE.                         GA1FPGM 
01381      MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE.                    GA1FPGM 
01382      MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM.                    GA1FPGM 
01383      MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM.                GA1FPGM 
01384      MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE.                      GA1FPGM 
01385      MOVE GCA-L-O-B TO  GCIO-WRK-LINE-OF-BUS.                     GA1FPGM 
01386      MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL.              GA1FPGM 
01387      MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.        GA1FPGM 
01388      MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                    GA1FPGM 
01389                                                                   GA1FPGM 
01390      MOVE 'GCPSWORK'  TO  GCIO3-FILE-DDNAME.                      GA1FPGM 
01391      MOVE SPACES  TO  GCIO-WRK-PROVISION-ID,                      GA1FPGM 
01392                       GCIO-WRK-TAB-PROVISION-ID.                  GA1FPGM 
01393      MOVE ZEROES  TO  GCIO-WRK-PROVISION-SLOT-NO,                 GA1FPGM 
01394                       GCIO-WRK-TAB-PROV-SLOT-NO.                  GA1FPGM 
01395      MOVE GCIO-WORKFILE-KEY  TO  GCIO3-FILE-KEY.                  GA1FPGM 
01396                                                                   GA1FPGM 
01397      MOVE GC-GCCONTR-VARY-MAX-OCUR                                GA1FPGM 
01398      TO   GCT-COUNT-BEN-PROVN-POINTERS.                           GA1FPGM 
01399                                                                   GA1FPGM 
01400      MOVE  'RD '  TO  GCIO3-FILE-ACCESS-CODE.                     GA1FPGM 
01401                                                                   GA1FPGM 
01402      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1FPGM 
01403         COMMAREA(IO-PARM-CONTRACT-RECORD)                         GA1FPGM 
01404         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1FPGM 
01405                                                                   GA1FPGM 
01406      IF  NOT GCIO3-GOOD-RETURN                                    GA1FPGM 
01407         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA1FPGM 
01408 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA1FPGM 
01409         MOVE '1FF5'  TO  WS-ABEND-CODE                            GA1FPGM 
01410         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1FPGM 
01411                                                                   GA1FPGM 
01412      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1FPGM 
01413          GC-WORKFILE-KEY-LEN        +                             GA1FPGM 
01414          GC-GCCONTR-MAX-REC-LEN.                                  GA1FPGM 
01415                                                                   GA1FPGM 
01416      EXEC CICS XCTL  PROGRAM('GC4APGM') COMMAREA(WORK-RECORD-3)   GA1FPGM 
01417         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1FPGM 
01418                                                                   GA1FPGM 
01419      GO  TO  5099-EXIT.                                           GA1FPGM 
01420                                                                   GA1FPGM 
01421  5030-XCTL-TO-BEN-PROV-MENU.                                      GA1FPGM 
01422      MOVE '5030'  TO  WS-PARA-ID.                                 GA1FPGM 
01423                                                                   GA1FPGM 
01424      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1FPGM 
01425          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA1FPGM 
01426                 GC-GCBENPRV-MAX-REC-LEN.                          GA1FPGM 
01427                                                                   GA1FPGM 
01428 ***  EXEC CICS GETMAIN  SET(BEN-PROV-PNTR) INITIMG(WS-HEX-00)     GA1FPGM 
01429      EXEC CICS GETMAIN                                            GA1FPGM 
01430         SET(ADDRESS OF IO-PARM-BEN-PROV-RECORD)                   GA1FPGM 
01431         INITIMG(WS-HEX-00)                                        GA1FPGM 
01432         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1FPGM 
01433                                                                   GA1FPGM 
01434 ***  SERVICE RELOAD  IO-PARM-BEN-PROV-RECORD.                     GA1FPGM 
01435                                                                   GA1FPGM 
01436      MOVE SPACES      TO  GCIO-WORKFILE-KEY.                      GA1FPGM 
01437                                                                   GA1FPGM 
01438      MOVE 'C'   TO  GCIO-WRK-STATUS-CODE.                         GA1FPGM 
01439      MOVE 'C4'  TO  GCIO-WRK-RECORD-TYPE.                         GA1FPGM 
01440      MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE.                    GA1FPGM 
01441      MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM.                    GA1FPGM 
01442      MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM.                GA1FPGM 
01443      MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE.                      GA1FPGM 
01444      MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS.                      GA1FPGM 
01445      MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL.              GA1FPGM 
01446      MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.        GA1FPGM 
01447      MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                    GA1FPGM 
01448      MOVE GCA-BEN-PROV-ID TO  GCIO-WRK-PROVISION-ID.              GA1FPGM 
01449                                                                   GA1FPGM 
01450      MOVE 'GCPSWORK'  TO  GCIO4-FILE-DDNAME.                      GA1FPGM 
01451      MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO.                GA1FPGM 
01452      MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID.                  GA1FPGM 
01453      MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                  GA1FPGM 
01454      MOVE GCIO-WORKFILE-KEY  TO  GCIO4-FILE-KEY.                  GA1FPGM 
01455                                                                   GA1FPGM 
01456      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               GA1FPGM 
01457      TO   GCP-COUNT-TAB-PROVN-POINTERS.                           GA1FPGM 
01458                                                                   GA1FPGM 
01459      MOVE  'RD '  TO  GCIO4-FILE-ACCESS-CODE.                     GA1FPGM 
01460                                                                   GA1FPGM 
01461      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1FPGM 
01462         COMMAREA(IO-PARM-BEN-PROV-RECORD)                         GA1FPGM 
01463         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1FPGM 
01464                                                                   GA1FPGM 
01465      IF  NOT GCIO4-GOOD-RETURN                                    GA1FPGM 
01466         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA1FPGM 
01467 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA1FPGM 
01468         MOVE '1FF6'  TO  WS-ABEND-CODE                            GA1FPGM 
01469         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1FPGM 
01470                                                                   GA1FPGM 
01471      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1FPGM 
01472          GC-WORKFILE-KEY-LEN        +                             GA1FPGM 
01473          GC-GCBENPRV-MAX-REC-LEN.                                 GA1FPGM 
01474                                                                   GA1FPGM 
01475      EXEC CICS XCTL  PROGRAM('GC8APGM') COMMAREA(WORK-RECORD-4)   GA1FPGM 
01476         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1FPGM 
01477                                                                   GA1FPGM 
01478      GO  TO  5099-EXIT.                                           GA1FPGM 
01479                                                                   GA1FPGM 
01480  5040-XCTL-TO-SINGLE-TAB-MENU.                                    GA1FPGM 
01481      MOVE '5040'  TO  WS-PARA-ID.                                 GA1FPGM 
01482                                                                   GA1FPGM 
01483      EXEC CICS XCTL                                               GA1FPGM 
01484                PROGRAM('GTM1PGM')                                 GA1FPGM 
01485                END-EXEC.                                          GA1FPGM 
01486                                                                   GA1FPGM 
01487                                                                   GA1FPGM 
01488                                                                   GA1FPGM 
01489      GO  TO  5099-EXIT.                                           GA1FPGM 
01490                                                                   GA1FPGM 
01491  5099-EXIT.                                                       GA1FPGM 
01492      EXIT.                                                        GA1FPGM 
01493      EJECT                                                        GA1FPGM 
01494 ***************************************************************** GA1FPGM 
01495 **           X C T L   T O   M A I N   M E N U                    GA1FPGM 
01496 **                                                                GA1FPGM 
01497 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA1FPGM 
01498 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA1FPGM 
01499 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA1FPGM 
01500 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOME OF AGA1FPGM 
01501 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA1FPGM 
01502 ** AND PROGRESS DOWN.                                             GA1FPGM 
01503 ******************************************************************GA1FPGM 
01504  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA1FPGM 
01505      MOVE '6000'  TO  WS-PARA-ID.                                 GA1FPGM 
01506      MOVE '1FP1'  TO  WS-ABEND-CODE.                              GA1FPGM 
01507                                                                   GA1FPGM 
01508      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA1FPGM 
01509                                                                   GA1FPGM 
01510  6099-EXIT.     EXIT.                                             GA1FPGM 
01511      EJECT                                                        GA1FPGM 
01512  9010-NO-STORAGE SECTION.                                         GA1FPGM 
01513                                                                   GA1FPGM 
01514      MOVE '1FS1'  TO  WS-ABEND-CODE.                              GA1FPGM 
01515      MOVE '*** CICS IS UNABLE TO FIND STORAGE REQUESTED BY THIS PGGA1FPGM 
01516 -    'M, NOTIFY SYSTEMS ***'  TO  MAP-ERROR-MESSAGE.              GA1FPGM 
01517                                                                   GA1FPGM 
01518      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1FPGM 
01519                                                                   GA1FPGM 
01520  9019-EXIT.     EXIT.                                             GA1FPGM 
01521      SKIP3                                                        GA1FPGM 
01522      SKIP3                                                        GA1FPGM 
01523 ******************************************************************GA1FPGM 
01524  9020-PGM-ID-ERROR SECTION.                                       GA1FPGM 
01525                                                                   GA1FPGM 
01526 **     THIS ERROR CAN BE INVOKED BY A NUMBER OF DIFFERENT REQUESTSGA1FPGM 
01527 **     THE PROGRAMMER SHOULD CHECK THE WS-PARA-ID FIELD IN THE    GA1FPGM 
01528 **     DUMP TO DETERMINE WHAT CODE CAUSED THIS ABEND.             GA1FPGM 
01529                                                                   GA1FPGM 
01530      MOVE '1FP2'  TO  WS-ABEND-CODE.                              GA1FPGM 
01531      MOVE '*** CICS CAN T ACCESS A PGM, TABLE, OR MAP FOR THIS PGMGA1FPGM 
01532 -    '. CALL SYSTEMS ***'  TO  MAP-ERROR-MESSAGE.                 GA1FPGM 
01533                                                                   GA1FPGM 
01534      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1FPGM 
01535                                                                   GA1FPGM 
01536  9029-EXIT.     EXIT.                                             GA1FPGM 
01537      SKIP3                                                        GA1FPGM 
01538      SKIP3                                                        GA1FPGM 
01539 ******************************************************************GA1FPGM 
01540  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA1FPGM 
01541                                                                   GA1FPGM 
01542      SET MAP-IDX1  TO  7.                                         GA1FPGM 
01543      SET MAP-IDX2  TO  1.                                         GA1FPGM 
01544      MOVE -1  TO  MAP-SELECT-LEN.                                 GA1FPGM 
01545                                                                   GA1FPGM 
01546      EXEC CICS SEND   MAP('GA1FI01') MAPSET('GA1FSET') ERASE      GA1FPGM 
01547         FROM(GA1FI01O) WAIT END-EXEC.                             GA1FPGM 
01548                                                                   GA1FPGM 
01549      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA1FPGM 
01550                                                                   GA1FPGM 
01551  9999-EXIT.     EXIT.                                             GA1FPGM 
